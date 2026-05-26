# =============================================================================
# pipeline.R  --  Pipeline de análisis FCS para Karion-Flow API
# =============================================================================
# Adaptado de CITOMETRIATODOMIX_FINAL.R
# Uso: source("pipeline.R"); analizar_fcs(paths, output_dir)

`%||%` <- function(a, b) if (is.null(a)) b else a

htmlspecialchars <- function(s) {
  s <- gsub("&", "&amp;", s)
  s <- gsub("<", "&lt;", s)
  s <- gsub(">", "&gt;", s)
  s <- gsub("\"", "&quot;", s)
  s <- gsub("'", "&#039;", s)
  s
}

library(flowCore)
library(PeacoQC)
library(FlowSOM)
library(ggplot2)
library(gridExtra)
library(grid)
library(MASS)
library(scatterplot3d)
library(plotly)
library(htmltools)
library(htmlwidgets)
library(base64enc)
library(jsonlite)

CANAL_FSC  <- "FSC-A"
CANAL_SSC  <- "SSC-A"
CD45_LABEL <- "CD45"
CANALES_EXCLUIR <- c("FSC-A","FSC-W","FSC-H","SSC-A","SSC-W","SSC-H","Time")
N_POB_CLSI <- 5
SEMILLA         <- 8471
UMBRAL_POS_CLSI <- 1.5
UMBRAL_QC_FLAG  <- 20
MAX_PLOT        <- 50000
UMBRAL_SATURACION <- 260000
RADIO_PEAK_GATE   <- 40000
MIN_EVENTOS_COV   <- 10

COLORES_CLSI <- c(
  "Blastos"      = "#E74C3C",
  "Linfocitos"   = "#27AE60",
  "Monocitos"    = "#E67E22",
  "Granulocitos" = "#2980B9",
  "Eosinofilos"  = "#8E44AD",
  "Otro"         = "#95A5A6"
)
ORDEN_POB <- c("Blastos","Linfocitos","Monocitos","Granulocitos","Eosinofilos","Otro")
PANELES_CONTROL <- c("U-A", "UA", "ISOTIPOS", "ISOTIPO", "UNSTAINED",
                     "COMP", "COMPENSATION", "BEAD", "BEADS")

extraer_info_archivo <- function(ruta) {
  nombre <- tools::file_path_sans_ext(basename(ruta))
  partes <- strsplit(nombre, "_")[[1]]
  if (length(partes) < 3)
    return(list(panel = nombre, clave_paciente = nombre, fecha = "", hora = ""))
  list(
    panel          = partes[1],
    clave_paciente = paste(partes[-c(1, length(partes))], collapse = "_"),
    fecha          = partes[length(partes) - 1],
    hora           = partes[length(partes)]
  )
}

detectar_marcadores <- function(ff) {
  params  <- parameters(ff)@data
  pnn     <- as.character(params$name)
  pns_raw <- as.character(params$desc)
  pns     <- ifelse(is.na(pns_raw) | pns_raw == "", pnn, pns_raw)
  excluir <- toupper(CANALES_EXCLUIR)
  mask    <- !toupper(pnn) %in% excluir & !grepl("^$", pns)
  todos_canales <- setNames(pns[mask], pnn[mask])
  cd45_canal    <- names(todos_canales)[toupper(todos_canales) == toupper(CD45_LABEL)]
  if (length(cd45_canal) == 0) {
    cd45_patron <- names(todos_canales)[grepl("CD45", todos_canales, ignore.case = TRUE)]
    cd45_canal  <- if (length(cd45_patron) > 0) cd45_patron[1] else character(0)
  }
  limpiar <- function(s) {
    s <- trimws(gsub("\\s*[-/].*", "", s))
    s
  }
  todos_limpios <- setNames(limpiar(todos_canales), names(todos_canales))
  otros_canales  <- todos_limpios[!names(todos_limpios) %in% cd45_canal]
  cd45_limpios   <- todos_limpios[cd45_canal]
  marcador_ultimo <- list()
  for (canal in names(todos_limpios)) {
    m <- todos_limpios[[canal]]
    marcador_ultimo[[m]] <- canal
  }
  otros_unicos <- unlist(marcador_ultimo)
  otros_unicos <- otros_unicos[!names(otros_unicos) %in% cd45_canal]
  list(
    cd45_canal    = if (length(cd45_canal) > 0) cd45_canal[1] else NA_character_,
    cd45_label    = if (length(cd45_limpios) > 0) cd45_limpios[1] else CD45_LABEL,
    todos_canales = todos_limpios,
    otros_canales = setNames(names(otros_unicos), unname(otros_unicos))
  )
}

aplicar_peacoqc <- function(ff) {
  channels <- colnames(exprs(ff))
  channels <- channels[!grepl("Time|FSC|SSC", channels, ignore.case = TRUE)]
  channels <- channels[channels %in% colnames(exprs(ff))]
  res <- tryCatch(
    PeacoQC(ff, channels = channels, output_directory = tempdir(),
            save_fcs = FALSE, display_plots = FALSE, report = FALSE),
    error = function(e) NULL
  )
  if (is.null(res)) {
    return(list(ff_limpio = ff, n_orig = nrow(ff), n_limpio = nrow(ff),
                pct_removido = 0, pct_mad = 0, pct_consec = 0,
                ContributionMad = NULL))
  }
  ff_limpio <- res$FinalFF
  n_orig    <- nrow(ff)
  n_limpio  <- nrow(ff_limpio)
  contrib_mad <- res$ContributionMad
  contrib <- if (!is.null(contrib_mad) && length(contrib_mad) > 0)
    sort(contrib_mad, decreasing = TRUE) else numeric(0)
  list(
    ff_limpio    = ff_limpio,
    n_orig       = n_orig,
    n_limpio     = n_limpio,
    pct_removido = round(100 * (1 - n_limpio / n_orig), 1),
    pct_mad      = round(100 * sum(res$GoodCells == FALSE & !is.na(res$GoodCells)) / n_orig, 1),
    pct_consec   = 0,
    ContributionMad = contrib
  )
}

transformar_fcs <- function(ff, canales_fluor) {
  canales_ok <- intersect(canales_fluor, colnames(exprs(ff)))
  if (length(canales_ok) == 0) return(ff)
  tryCatch({
    lg <- estimateLogicle(ff, channels = canales_ok)
    transform(ff, lg)
  }, error = function(e) ff)
}

gate_scatter <- function(ff) {
  mat <- as.data.frame(exprs(ff)[, c(CANAL_FSC, CANAL_SSC), drop = FALSE])
  no_sat <- mat[mat[[CANAL_FSC]] < UMBRAL_SATURACION & mat[[CANAL_SSC]] < UMBRAL_SATURACION, ]
  if (nrow(no_sat) < MIN_EVENTOS_COV) return(ff)
  dens <- tryCatch(kde2d(no_sat[[CANAL_FSC]], no_sat[[CANAL_SSC]], n = 50), error = function(e) NULL)
  if (is.null(dens)) return(ff)
  peak_x <- dens$x[which(dens$z == max(dens$z), arr.ind = TRUE)[1, 1]]
  peak_y <- dens$y[which(dens$z == max(dens$z), arr.ind = TRUE)[1, 2]]
  cerca  <- no_sat[abs(no_sat[[CANAL_FSC]] - peak_x) < RADIO_PEAK_GATE &
                     abs(no_sat[[CANAL_SSC]] - peak_y) < RADIO_PEAK_GATE, ]
  if (nrow(cerca) < MIN_EVENTOS_COV) return(ff)
  cov_mat <- tryCatch(cov(cerca), error = function(e) NULL)
  if (is.null(cov_mat)) return(ff)
  gate <- ellipsoidGate(.gate = cov_mat * 9,
                        mean = c(peak_x, peak_y),
                        filterId = "scatter")
  tryCatch(Subset(ff, gate), error = function(e) ff)
}

gate_singlets <- function(ff) {
  if (!all(c("FSC-A","FSC-H") %in% colnames(exprs(ff)))) return(ff)
  mat <- as.data.frame(exprs(ff)[, c("FSC-A","FSC-H"), drop = FALSE])
  if (nrow(mat) < MIN_EVENTOS_COV) return(ff)
  cov_mat <- tryCatch(cov(mat), error = function(e) NULL)
  if (is.null(cov_mat)) return(ff)
  gate <- ellipsoidGate(.gate = cov_mat * 4,
                        mean  = colMeans(mat),
                        filterId = "singlets")
  tryCatch(Subset(ff, gate), error = function(e) ff)
}

aplicar_compensacion <- function(ff) {
  spill <- keyword(ff, "$SPILLOVER")
  if (is.null(spill)) spill <- keyword(ff, "$SPILL")
  if (!is.null(spill)) {
    tryCatch({
      ff <- compensate(ff, compensation(spill))
    }, error = function(e) {})
  }
  ff
}

etiquetar_poblaciones_clsi <- function(centroides) {
  n   <- nrow(centroides)
  cd  <- centroides$cd45
  sc  <- centroides$ssc
  etq <- rep("Otro", n)
  usd <- logical(n)
  ig <- which.max(sc)
  etq[ig] <- "Granulocitos"; usd[ig] <- TRUE
  rem <- which(!usd)
  sb  <- rank(cd[rem]) + rank(sc[rem])
  ib  <- rem[which.min(sb)]
  etq[ib] <- "Blastos"; usd[ib] <- TRUE
  rem <- which(!usd)
  if (length(rem) >= 3) {
    ie <- rem[which.max(sc[rem])]
    etq[ie] <- "Eosinofilos"; usd[ie] <- TRUE
    rem <- which(!usd)
  }
  rem <- which(!usd)
  if (length(rem) >= 2) {
    sl <- rank(-cd[rem]) + rank(sc[rem])
    il <- rem[which.min(sl)]
    etq[il] <- "Linfocitos"; usd[il] <- TRUE
    rem <- which(!usd)
    if (length(rem) >= 1) etq[rem[1]] <- "Monocitos"
  } else if (length(rem) == 1) {
    etq[rem] <- "Linfocitos"
  }
  etq
}

clasificar_cd45ssc_clsi <- function(tubos_ff, canal_cd45) {
  mats <- lapply(tubos_ff, function(ff) {
    mat <- exprs(ff)[, c(canal_cd45, CANAL_SSC), drop = FALSE]
    colnames(mat) <- c("cd45", "ssc")
    as.data.frame(mat)
  })
  n_por_tubo <- sapply(mats, nrow)
  pool       <- do.call(rbind, mats)
  if (nrow(pool) < N_POB_CLSI * 10) {
    return(lapply(tubos_ff, function(ff) rep("Otro", nrow(ff))))
  }
  ff_pool <- flowFrame(as.matrix(pool))
  fsom    <- tryCatch(
    FlowSOM(ff_pool, colsToUse = c("cd45", "ssc"), nClus = N_POB_CLSI, seed = SEMILLA),
    error = function(e) NULL
  )
  if (is.null(fsom)) {
    return(lapply(tubos_ff, function(ff) rep("Otro", nrow(ff))))
  }
  meta_todos <- GetMetaclusters(fsom)
  centroides <- aggregate(pool, by = list(Cluster = meta_todos), FUN = mean)
  etiquetas  <- etiquetar_poblaciones_clsi(
    data.frame(cd45 = centroides$cd45, ssc = centroides$ssc)
  )
  mapa <- setNames(etiquetas, as.character(centroides$Cluster))
  pob_todos <- mapa[as.character(meta_todos)]
  indices_fin <- cumsum(n_por_tubo)
  indices_ini <- c(1, head(indices_fin, -1) + 1)
  lapply(seq_along(tubos_ff), function(i)
    pob_todos[indices_ini[i]:indices_fin[i]])
}

caracterizar_marcadores <- function(datos_df, poblaciones_vec, marcadores) {
  datos_df$Poblacion <- poblaciones_vec
  filas <- list()
  pobs  <- sort(unique(poblaciones_vec))
  n_total <- nrow(datos_df)
  for (pop in pobs) {
    sub <- datos_df[datos_df$Poblacion == pop, , drop = FALSE]
    n_pop <- nrow(sub)
    pct_tubo <- round(100 * n_pop / n_total, 1)
    for (m in marcadores) {
      vals <- sub[[m]]
      vals <- vals[!is.na(vals)]
      filas[[length(filas) + 1]] <- data.frame(
        Poblacion  = pop,
        Marcador   = m,
        N          = n_pop,
        Pct_tubo   = pct_tubo,
        Media_expr = if (length(vals) > 0) round(mean(vals), 3) else NA_real_,
        Pct_pos    = if (length(vals) > 0) round(100 * mean(vals > UMBRAL_POS_CLSI), 1) else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, filas)
}

pagina_portada_clsi <- function(id_paciente, tubos_info, resumen_pob) {
  grid.newpage()
  grid.rect(x = 0.5, y = 0.88, width = 1, height = 0.24,
            gp = gpar(fill = "#1A252F", col = NA))
  grid.text("LABORATORIO DE CITOMETRIA DE FLUJO",
            x = 0.5, y = 0.96,
            gp = gpar(fontsize = 13, col = "white", fontface = "bold"))
  grid.text("Inmunofenotipificacion - Estrategia CLSI H62",
            x = 0.5, y = 0.91,
            gp = gpar(fontsize = 10, col = "#AED6F1"))
  fecha_fmt <- tryCatch(format(as.Date(tubos_info[[1]]$fecha, "%Y%m%d"), "%d/%m/%Y"),
                        error = function(e) tubos_info[[1]]$fecha)
  grid.text(paste0("ID Paciente:  ", id_paciente),
            x = 0.08, y = 0.80, just = "left",
            gp = gpar(fontsize = 14, fontface = "bold"))
  grid.text(paste0("Fecha:        ", fecha_fmt),
            x = 0.08, y = 0.74, just = "left",
            gp = gpar(fontsize = 13))
  grid.text(paste0("Tubos analizados: ", length(tubos_info)),
            x = 0.08, y = 0.68, just = "left",
            gp = gpar(fontsize = 13))
  grid.lines(x = c(0.05, 0.95), y = c(0.63, 0.63),
             gp = gpar(col = "#2C3E50", lwd = 2))
  y_pos <- 0.60
  grid.text("TUBOS PROCESADOS:", x = 0.08, y = y_pos, just = "left",
            gp = gpar(fontsize = 10, fontface = "bold", col = "#2C3E50"))
  y_pos <- y_pos - 0.04
  max_lineas <- floor((y_pos - 0.28) / 0.05)
  for (i in seq_along(tubos_info)) {
    if (i > max_lineas) {
      grid.text(paste0("  ... y ", length(tubos_info) - max_lineas, " tubo(s) mas"),
                x = 0.10, y = y_pos, just = "left",
                gp = gpar(fontsize = 9, col = "#7F8C8D"))
      break
    }
    grid.text(paste0("  ", i, ".  ", tubos_info[[i]]$panel),
              x = 0.10, y = y_pos, just = "left",
              gp = gpar(fontsize = 9.5, fontfamily = "mono"))
    y_pos <- y_pos - 0.05
  }
  if (!is.null(resumen_pob) && nrow(resumen_pob) > 0) {
    grid.lines(x = c(0.05, 0.95), y = c(0.25, 0.25),
               gp = gpar(col = "#2C3E50", lwd = 1))
    grid.text("DISTRIBUCION CLSI H62 (global, todos los tubos):",
              x = 0.08, y = 0.22, just = "left",
              gp = gpar(fontsize = 10, fontface = "bold", col = "#2C3E50"))
    y_p <- 0.18
    for (i in seq_len(nrow(resumen_pob))) {
      pop   <- resumen_pob$Poblacion[i]
      color <- COLORES_CLSI[pop]
      if (is.na(color)) color <- "#95A5A6"
      grid.rect(x = 0.10, y = y_p + 0.005, width = 0.012, height = 0.018,
                gp = gpar(fill = color, col = NA))
      grid.text(sprintf("  %s:  %.1f%%  (%s eventos)",
                        pop, resumen_pob$Pct[i],
                        format(resumen_pob$N[i], big.mark = ",")),
                x = 0.115, y = y_p, just = "left",
                gp = gpar(fontsize = 9))
      y_p <- y_p - 0.04
      if (y_p < 0.04) break
    }
  }
  grid.text(paste0("Generado: ", format(Sys.time(), "%d/%m/%Y %H:%M"),
                   "  -  Pipeline: R / flowCore / PeacoQC / FlowSOM (CLSI H62)"),
            x = 0.5, y = 0.025,
            gp = gpar(fontsize = 8, col = "grey60"))
}

pagina_cd45ssc_global <- function(datos_fusion, nombre_paciente) {
  n_plot <- min(nrow(datos_fusion), MAX_PLOT)
  set.seed(SEMILLA)
  df_plot <- datos_fusion[sample(nrow(datos_fusion), n_plot), ]
  df_plot$ssc_log <- log10(pmax(df_plot$ssc, 1))
  df_plot$Poblacion <- factor(df_plot$Poblacion, levels = ORDEN_POB)
  p1 <- ggplot(df_plot, aes(x = cd45, y = ssc_log, color = Poblacion)) +
    geom_point(size = 0.25, alpha = 0.35) +
    scale_color_manual(values = COLORES_CLSI, drop = FALSE) +
    labs(title   = paste0("CD45/SSC - CLSI H62 - ", nombre_paciente),
         subtitle = paste0("Todos los tubos fundidos  |  N = ",
                           format(nrow(datos_fusion), big.mark = ","), " eventos"),
         x = paste0(CD45_LABEL, " (logicle)"),
         y = "SSC-A (log10)") +
    theme_bw(base_size = 10) +
    guides(color = guide_legend(override.aes = list(size = 3, alpha = 1),
                                title = "Poblacion CLSI H62")) +
    theme(legend.position = "right",
          plot.title    = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 8.5, color = "grey40"))
  tbl_pob <- as.data.frame(table(datos_fusion$Poblacion))
  colnames(tbl_pob) <- c("Poblacion", "Eventos")
  tbl_pob$Pct <- paste0(round(100 * tbl_pob$Eventos / sum(tbl_pob$Eventos), 1), "%")
  tbl_pob$Eventos <- format(tbl_pob$Eventos, big.mark = ",")
  tbl_pob <- tbl_pob[tbl_pob$Eventos != "0", ]
  tg <- tableGrob(tbl_pob, rows = NULL,
                  theme = ttheme_minimal(base_size = 9,
                                         core    = list(fg_params = list(fontsize = 9)),
                                         colhead = list(fg_params = list(fontsize = 9.5, fontface = "bold"))))
  p2 <- ggplot(df_plot, aes(x = cd45, fill = Poblacion)) +
    geom_density(alpha = 0.45, position = "stack") +
    scale_fill_manual(values = COLORES_CLSI, drop = FALSE) +
    labs(title = paste0("Densidad CD45 por poblacion"), x = paste0(CD45_LABEL, " (logicle)"),
         y = "Densidad") +
    theme_bw(base_size = 9) + theme(legend.position = "none")
  titulo <- textGrob(paste0("Estrategia CLSI H62 - Fusion CD45/SSC - ", nombre_paciente),
                     gp = gpar(fontsize = 12, fontface = "bold"))
  grid.arrange(p1, arrangeGrob(tg, p2, nrow = 2), ncol = 2,
               widths = c(2.2, 1), top = titulo)
}

pagina_heatmap_poblaciones <- function(expr_global, nombre_paciente) {
  if (is.null(expr_global) || nrow(expr_global) == 0) return(invisible(NULL))
  agr <- aggregate(
    x = expr_global$Pct_pos,
    by = list(Poblacion = expr_global$Poblacion, Marcador = expr_global$Marcador),
    FUN = function(x) round(mean(x, na.rm = TRUE), 1)
  )
  colnames(agr) <- c("Poblacion", "Marcador", "Pct_pos")
  ord_m <- names(sort(tapply(agr$Pct_pos, agr$Marcador, mean, na.rm = TRUE),
                      decreasing = TRUE))
  agr$Marcador <- factor(agr$Marcador, levels = ord_m)
  agr$Poblacion <- factor(agr$Poblacion, levels = rev(ORDEN_POB))
  p <- ggplot(agr, aes(x = Marcador, y = Poblacion, fill = Pct_pos)) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = paste0(Pct_pos, "%")), size = 3.2, fontface = "bold") +
    scale_fill_gradient2(low = "#1A5276", mid = "#F9E79F", high = "#C0392B",
                         midpoint = 40, name = "% Positivo") +
    labs(title = paste0("Heatmap Poblacion x Marcador  -  ", nombre_paciente),
         subtitle = paste0("% de celulas positivas por poblacion (umbral logicle > ", UMBRAL_POS_CLSI, ")"),
         x = "Marcador", y = "Poblacion CLSI H62") +
    theme_minimal(base_size = 10) +
    theme(axis.text.x  = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y  = element_text(size = 10, face = "bold"),
          panel.grid   = element_blank(),
          legend.position = "bottom",
          plot.title   = element_text(size = 12, face = "bold"),
          legend.key.width = unit(1.2, "cm"))
  grid.newpage()
  titulo <- textGrob(paste0("Perfil Antigenico - ", nombre_paciente),
                     gp = gpar(fontsize = 13, fontface = "bold"))
  grid.arrange(p, top = titulo)
}

pagina_distribucion_tubos <- function(comp_df, nombre_paciente) {
  if (is.null(comp_df) || nrow(comp_df) == 0) return(invisible(NULL))
  comp_df$Poblacion <- factor(comp_df$Poblacion, levels = ORDEN_POB)
  comp_df$Tubo_label <- paste0(comp_df$Panel, "\n", comp_df$Tubo)
  comp_df$Tubo_label <- factor(comp_df$Tubo_label, levels = unique(comp_df$Tubo_label))
  
  agregado <- aggregate(Pct ~ Poblacion, data = comp_df, FUN = mean)
  agregado$Pct <- round(agregado$Pct, 1)
  agregado <- agregado[order(match(agregado$Poblacion, ORDEN_POB)), ]
  
  p1 <- ggplot(comp_df, aes(x = Tubo_label, y = Pct, fill = Poblacion)) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_fill_manual(values = COLORES_CLSI, drop = FALSE) +
    labs(title = "Distribucion de Poblaciones por Tubo",
         subtitle = paste0(nombre_paciente, "  |  % del total de eventos por tubo"),
         x = "Tubo", y = "% de eventos") +
    theme_bw(base_size = 9) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
          legend.position = "right",
          plot.title = element_text(size = 11, face = "bold"))
  
  tg <- tableGrob(agregado, rows = NULL,
                  theme = ttheme_minimal(base_size = 10,
                                         core = list(fg_params = list(fontsize = 10)),
                                         colhead = list(fg_params = list(fontsize = 10, fontface = "bold"))))
  titulo <- textGrob(paste0("Distribucion de Poblaciones - ", nombre_paciente),
                     gp = gpar(fontsize = 12, fontface = "bold"))
  grid.arrange(p1, tg, ncol = 2, widths = c(2.5, 1), top = titulo)
}

pagina_aps_clsi <- function(expr_global, nombre_paciente) {
  if (is.null(expr_global) || nrow(expr_global) == 0) return(invisible(NULL))
  agr <- aggregate(
    x = expr_global$Pct_pos,
    by = list(Poblacion = expr_global$Poblacion, Marcador = expr_global$Marcador),
    FUN = function(x) round(mean(x, na.rm = TRUE), 1)
  )
  colnames(agr) <- c("Poblacion", "Marcador", "Pct_pos")
  agr$Poblacion <- factor(agr$Poblacion, levels = ORDEN_POB)
  ord_m <- names(sort(tapply(agr$Pct_pos, agr$Marcador, mean, na.rm = TRUE), decreasing = TRUE))
  agr$Marcador <- factor(agr$Marcador, levels = ord_m)
  
  p_aps <- ggplot(agr, aes(x = Marcador, y = Poblacion, size = Pct_pos, color = Pct_pos)) +
    geom_point(alpha = 0.85) +
    scale_size_continuous(range = c(2, 12), name = "% Pos") +
    scale_color_gradient(low = "#3498DB", high = "#C0392B", name = "% Pos") +
    labs(title = "APS - Advanced Parameter Scoring (Infinicyt-style)",
         subtitle = paste0(nombre_paciente, "  |  Tamano = % positivo"),
         x = "Marcador", y = "Poblacion CLSI H62") +
    theme_bw(base_size = 10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 10, face = "bold"),
          legend.position = "bottom",
          plot.title = element_text(size = 11, face = "bold"))
  
  radar_df <- agr
  radar_df$Pct_pos_escalado <- radar_df$Pct_pos / 100
  radar_df$id <- paste(radar_df$Poblacion, radar_df$Marcador, sep = "|")
  radar_wide <- tryCatch({
    reshape(radar_df[, c("Poblacion", "Marcador", "Pct_pos_escalado")],
            idvar = "Poblacion", timevar = "Marcador", direction = "wide")
  }, error = function(e) NULL)
  
  n_marc <- length(unique(agr$Marcador))
  n_pob  <- length(unique(agr$Poblacion))
  
  grid.newpage()
  if (n_marc >= 3 && n_pob >= 2 && !is.null(radar_wide)) {
    pushViewport(viewport(layout = grid.layout(1, 2)))
    pushViewport(viewport(layout.pos.col = 1))
    grid.draw(ggplotGrob(p_aps))
    upViewport()
    pushViewport(viewport(layout.pos.col = 2))
    cols_radar <- grep("^Pct_pos_escalado\\.", colnames(radar_wide), value = TRUE)
    radar_mat <- as.matrix(radar_wide[, cols_radar])
    rownames(radar_mat) <- radar_wide$Poblacion
    colnames(radar_mat) <- gsub("^Pct_pos_escalado\\.", "", colnames(radar_mat))
    radar_mat[is.na(radar_mat)] <- 0
    radar_mat <- rbind(radar_mat, max = rep(1, ncol(radar_mat)))
    
    n_col <- ncol(radar_mat)
    angles <- seq(0, 2 * pi - 2 * pi / n_col, length.out = n_col)
    pushViewport(viewport(x = 0.5, y = 0.5, width = 0.85, height = 0.85))
    grid.circle(x = 0.5, y = 0.5, r = 0.45, gp = gpar(col = "grey80", fill = NA, lwd = 0.5))
    for (r in seq(0.2, 0.4, by = 0.1)) {
      grid.circle(x = 0.5, y = 0.5, r = r, gp = gpar(col = "grey90", fill = NA, lwd = 0.3))
    }
    pobs_radar <- rownames(radar_mat)[-nrow(radar_mat)]
    for (i in seq_along(pobs_radar)) {
      vals <- radar_mat[i, ]
      scaled <- vals / 2
      x_pos <- 0.5 + sapply(seq_len(n_col), function(j) scaled[j] * cos(angles[j]))
      y_pos <- 0.5 + sapply(seq_len(n_col), function(j) scaled[j] * sin(angles[j]))
      grid.polygon(x = x_pos, y = y_pos,
                   gp = gpar(fill = COLORES_CLSI[pobs_radar[i]],
                             col = COLORES_CLSI[pobs_radar[i]],
                             alpha = 0.35, lwd = 1.5))
    }
    for (j in seq_len(n_col)) {
      grid.lines(x = c(0.5, 0.5 + 0.45 * cos(angles[j])),
                 y = c(0.5, 0.5 + 0.45 * sin(angles[j])),
                 gp = gpar(col = "grey80", lwd = 0.5))
      grid.text(colnames(radar_mat)[j],
                x = 0.5 + 0.52 * cos(angles[j]),
                y = 0.5 + 0.52 * sin(angles[j]),
                gp = gpar(fontsize = 7.5))
    }
    popViewport()
    grid.text("Firma Antigenica (Radar)",
              x = 0.5, y = 0.97, gp = gpar(fontsize = 10, fontface = "bold"))
    upViewport()
    popViewport()
  } else {
    pushViewport(viewport(width = 0.97, height = 0.94))
    grid.draw(ggplotGrob(p_aps))
    popViewport()
  }
  
  grid.newpage()
  pushViewport(viewport(width = 0.97, height = 0.94))
  lolli_agg <- aggregate(Pct_pos ~ Marcador, data = agr, FUN = mean)
  lolli_agg <- lolli_agg[order(lolli_agg$Pct_pos, decreasing = TRUE), ]
  lolli_agg$Marcador <- factor(lolli_agg$Marcador, levels = rev(lolli_agg$Marcador))
  lolli_agg$color <- ifelse(lolli_agg$Pct_pos >= 50, "#C0392B",
                            ifelse(lolli_agg$Pct_pos >= 20, "#E67E22", "#3498DB"))
  p_lolli <- ggplot(lolli_agg, aes(x = Pct_pos, y = Marcador)) +
    geom_segment(aes(x = 0, xend = Pct_pos, y = Marcador, yend = Marcador),
                 color = "grey50", linewidth = 0.7) +
    geom_point(aes(color = color), size = 3.5) +
    scale_color_identity() +
    labs(title = "Estado de Expresion Global (Lollipop)",
         subtitle = paste0(nombre_paciente, "  |  Promedio entre poblaciones"),
         x = "% Positivo promedio", y = "") +
    theme_bw(base_size = 10) +
    theme(axis.text.y = element_text(size = 9, face = "bold"),
          plot.title = element_text(size = 11, face = "bold"),
          panel.grid.major.y = element_blank()) +
    geom_vline(xintercept = 20, linetype = "dashed", color = "orange", alpha = 0.5) +
    geom_vline(xintercept = 50, linetype = "dashed", color = "red", alpha = 0.5) +
    annotate("text", x = 10, y = 0.6, label = "Bajo", size = 3, color = "orange") +
    annotate("text", x = 35, y = 0.6, label = "Moderado", size = 3, color = "orange") +
    annotate("text", x = 75, y = 0.6, label = "Alto", size = 3, color = "red")
  grid.draw(ggplotGrob(p_lolli))
  popViewport()
}

pagina_aps_2d_3d <- function(datos, nombre_paciente) {
  if (is.null(datos) || nrow(datos) < 50) return(NULL)
  cd45_col <- if ("cd45" %in% colnames(datos)) "cd45" else "CD45"
  if (!cd45_col %in% colnames(datos)) return(NULL)
  has_fsc <- "fsc" %in% colnames(datos) && !all(is.na(datos$fsc))
  has_ssc <- "ssc" %in% colnames(datos) || "SSC" %in% colnames(datos)
  if (!has_ssc) return(NULL)
  ssc_col <- if ("ssc" %in% colnames(datos)) "ssc" else "SSC"
  
  if (!"Poblacion" %in% colnames(datos)) {
    datos$Poblacion <- "Total"
  }
  datos$Poblacion <- factor(datos$Poblacion, levels = ORDEN_POB)
  
  n_plot <- min(nrow(datos), MAX_PLOT)
  set.seed(SEMILLA)
  df_plot <- datos[sample(nrow(datos), n_plot), ]
  if (ssc_col %in% colnames(df_plot)) {
    df_plot$ssc_log <- log10(pmax(df_plot[[ssc_col]], 1))
  } else {
    df_plot$ssc_log <- log10(pmax(df_plot[[ssc_col]], 1))
  }
  df_plot$cd45_val <- df_plot[[cd45_col]]
  
  p_dens <- ggplot(df_plot, aes(x = cd45_val, y = ssc_log)) +
    stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE) +
    scale_fill_viridis_c(option = "plasma", name = "Densidad") +
    labs(title = "Mapa de Densidad 2D - CD45/SSC",
         subtitle = paste0(nombre_paciente, "  |  estimacion kernel"),
         x = paste0(CD45_LABEL, " (logicle)"), y = "SSC-A (log10)") +
    theme_bw(base_size = 10) +
    theme(plot.title = element_text(size = 11, face = "bold"),
          legend.position = "right")
  
  p_scatter <- ggplot(df_plot, aes(x = cd45_val, y = ssc_log, color = Poblacion)) +
    geom_point(size = 0.3, alpha = 0.4) +
    scale_color_manual(values = COLORES_CLSI, drop = FALSE) +
    labs(title = "CD45/SSC por Poblacion", x = paste0(CD45_LABEL, " (logicle)"),
         y = "SSC-A (log10)") +
    theme_bw(base_size = 10) +
    theme(plot.title = element_text(size = 11, face = "bold"),
          legend.position = "right")
  
  paginas <- list()
  paginas[[1]] <- arrangeGrob(p_dens, p_scatter, ncol = 2,
                              top = textGrob(paste0("APS 2D - ", nombre_paciente),
                                             gp = gpar(fontsize = 12, fontface = "bold")))
  
  if (has_fsc) {
    df_plot$fsc_log <- log10(pmax(df_plot$fsc, 1))
    p_fsc <- ggplot(df_plot, aes(x = cd45_val, y = fsc_log, color = Poblacion)) +
      geom_point(size = 0.3, alpha = 0.4) +
      scale_color_manual(values = COLORES_CLSI, drop = FALSE) +
      labs(title = "CD45/FSC", x = paste0(CD45_LABEL, " (logicle)"), y = "FSC-A (log10)") +
      theme_bw(base_size = 10) + theme(legend.position = "none")
    p_ssc_fsc <- ggplot(df_plot, aes(x = ssc_log, y = fsc_log, color = Poblacion)) +
      geom_point(size = 0.3, alpha = 0.4) +
      scale_color_manual(values = COLORES_CLSI, drop = FALSE) +
      labs(title = "SSC/FSC", x = "SSC-A (log10)", y = "FSC-A (log10)") +
      theme_bw(base_size = 10) + theme(legend.position = "none")
    paginas[[2]] <- arrangeGrob(p_fsc, p_ssc_fsc, ncol = 2,
                                top = textGrob(paste0("Vistas 2D Adicionales - ", nombre_paciente),
                                               gp = gpar(fontsize = 12, fontface = "bold")))
  }
  paginas
}

pagina_seccion_infinicyt <- function(df_infinicyt, nombre_paciente) {
  if (is.null(df_infinicyt) || nrow(df_infinicyt) < 30) return(invisible(NULL))
  
  if (!"Poblacion" %in% colnames(df_infinicyt))
    df_infinicyt$Poblacion <- "Total"
  df_infinicyt$Poblacion <- factor(df_infinicyt$Poblacion, levels = ORDEN_POB)
  
  num_cols <- names(Filter(is.numeric, df_infinicyt))
  skip_cols <- c("Poblacion", "Tubo", "Panel")
  mark_cols <- setdiff(num_cols, skip_cols)
  mark_cols <- setdiff(mark_cols, c("cd45", "ssc", "fsc", "CD45", "SSC", "FSC"))
  
  mark_cols <- head(mark_cols, 20)
  if (length(mark_cols) < 2) return(invisible(NULL))
  
  n_plot_inf <- min(nrow(df_infinicyt), MAX_PLOT)
  set.seed(SEMILLA)
  df_inf_plot <- df_infinicyt[sample(nrow(df_infinicyt), n_plot_inf), ]
  
  grid.newpage()
  pushViewport(viewport(width = 0.97, height = 0.94))
  grid.text("Visualizaciones Avanzadas (Infinicyt-style)",
            x = 0.5, y = 0.98, gp = gpar(fontsize = 14, fontface = "bold"))
  grid.text(paste0(nombre_paciente, "  |  Datos de expresion por evento (muestra de ",
                   n_plot_inf, " eventos)"),
            x = 0.5, y = 0.955, gp = gpar(fontsize = 9, col = "grey40"))
  popViewport()
  
  n_pares <- min(3, length(mark_cols) %/% 2)
  for (ip in seq_len(n_pares)) {
    c1 <- mark_cols[ip * 2 - 1]
    c2 <- mark_cols[min(ip * 2, length(mark_cols))]
    grid.newpage()
    pushViewport(viewport(width = 0.97, height = 0.94))
    p_biv <- ggplot(df_inf_plot, aes(x = .data[[c1]], y = .data[[c2]], color = .data[["Poblacion"]])) +
      geom_point(size = 0.5, alpha = 0.5) +
      scale_color_manual(values = COLORES_CLSI, drop = FALSE) +
      labs(title = paste0(c1, " vs ", c2, "  (", nombre_paciente, ")"),
           x = paste0(c1, " (logicle)"), y = paste0(c2, " (logicle)")) +
      theme_bw(base_size = 10) +
      theme(plot.title = element_text(size = 11, face = "bold"),
            legend.position = "right")
    grid.draw(ggplotGrob(p_biv))
    popViewport()
  }
  
  if (length(mark_cols) >= 2) {
    exp_matrix <- as.matrix(df_inf_plot[, mark_cols])
    exp_mean <- colMeans(exp_matrix, na.rm = TRUE)
    exp_sd <- apply(exp_matrix, 2, sd, na.rm = TRUE)
    ord <- order(exp_mean, decreasing = TRUE)
    exp_df <- data.frame(
      Marcador = factor(mark_cols[ord], levels = mark_cols[ord]),
      Media    = exp_mean[ord],
      SD       = exp_sd[ord]
    )
    grid.newpage()
    pushViewport(viewport(width = 0.97, height = 0.94))
    p_bar <- ggplot(exp_df, aes(x = Marcador, y = Media)) +
      geom_bar(stat = "identity", fill = "#3498DB", alpha = 0.8) +
      geom_errorbar(aes(ymin = Media - SD, ymax = Media + SD), width = 0.2, color = "grey40") +
      labs(title = paste0("Perfil de Expresion Promedio - ", nombre_paciente),
           subtitle = paste0("Media +/- SD  |  ", n_plot_inf, " eventos"),
           x = "Marcador", y = "Media expresion (logicle)") +
      theme_bw(base_size = 10) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            plot.title = element_text(size = 11, face = "bold")) +
      geom_hline(yintercept = UMBRAL_POS_CLSI, linetype = "dashed", color = "red", alpha = 0.5) +
      annotate("text", x = 0.5, y = UMBRAL_POS_CLSI + 0.1,
               label = paste0("Umbral positividad (", UMBRAL_POS_CLSI, ")"),
               hjust = 0, size = 3, color = "red", alpha = 0.7)
    grid.draw(ggplotGrob(p_bar))
    popViewport()
  }
}

pagina_detalle_tubo <- function(mat_tubo, expr_tubo, nomb_tubo, canal_cd45) {
  if (is.null(mat_tubo) || nrow(mat_tubo) < 10) return(invisible(NULL))
  mat_tubo$ssc_log <- log10(pmax(mat_tubo$ssc, 1))
  mat_tubo$Poblacion <- factor(mat_tubo$Poblacion, levels = ORDEN_POB)
  n_plot <- min(nrow(mat_tubo), MAX_PLOT)
  set.seed(SEMILLA)
  df_plot <- mat_tubo[sample(nrow(mat_tubo), n_plot), ]
  
  p1 <- ggplot(df_plot, aes(x = cd45, y = ssc_log, color = Poblacion)) +
    geom_point(size = 0.4, alpha = 0.45) +
    scale_color_manual(values = COLORES_CLSI, drop = FALSE) +
    labs(title = paste0("CD45/SSC - ", nomb_tubo), x = "CD45 (logicle)", y = "SSC-A (log10)") +
    theme_bw(base_size = 9) +
    theme(legend.position = "none",
          plot.title = element_text(size = 10, face = "bold"))
  
  if (!is.null(expr_tubo) && nrow(expr_tubo) > 0) {
    expr_sub <- expr_tubo[, c("Poblacion", "Marcador", "Pct_pos", "Media_expr")]
    expr_sub$Poblacion <- factor(expr_sub$Poblacion, levels = ORDEN_POB)
    tg <- tableGrob(expr_sub, rows = NULL,
                    theme = ttheme_minimal(base_size = 7.5))
    grid.arrange(p1, tg, ncol = 2, widths = c(1.2, 2.5),
                 top = textGrob(paste0("Tubo: ", nomb_tubo),
                                gp = gpar(fontsize = 11, fontface = "bold")))
  } else {
    grid.arrange(p1,
                 top = textGrob(paste0("Tubo: ", nomb_tubo),
                                gp = gpar(fontsize = 11, fontface = "bold")))
  }
}

pagina_analisis_clsi <- function(expr_global, comp_global, nombre_paciente, qc_df) {
  grid.newpage()
  pushViewport(viewport(width = 0.94, height = 0.94))
  
  grid.rect(x = 0.5, y = 0.96, width = 1, height = 0.07,
            gp = gpar(fill = "#1A252F", col = NA))
  grid.text("Analisis Preliminar CLSI H62", x = 0.5, y = 0.965,
            gp = gpar(fontsize = 14, fontface = "bold", col = "white"))
  grid.text(paste0("QC Automatico + Alertas - ", nombre_paciente),
            x = 0.5, y = 0.94, gp = gpar(fontsize = 9, col = "grey40"))
  
  y <- 0.88
  grid.text("CONTROL DE CALIDAD (QC):", x = 0.05, y = y, just = "left",
            gp = gpar(fontsize = 11, fontface = "bold", col = "#2C3E50"))
  y <- y - 0.04
  
  if (!is.null(qc_df) && nrow(qc_df) > 0) {
    for (i in seq_len(nrow(qc_df))) {
      icono <- if (qc_df$QC_flag[i]) "!!" else "OK"
      color <- if (qc_df$QC_flag[i]) "#E74C3C" else "#27AE60"
      grid.text(sprintf("  %-25s QC removio %5.1f%%  %s",
                        qc_df$Panel[i], qc_df$QC_pct[i], icono),
                x = 0.06, y = y, just = "left",
                gp = gpar(fontsize = 8.5, col = color))
      y <- y - 0.03
    }
  } else {
    grid.text("  (sin datos de QC)", x = 0.06, y = y, just = "left",
              gp = gpar(fontsize = 9, col = "grey50"))
    y <- y - 0.03
  }
  
  y <- y - 0.02
  grid.text("ALERTAS AUTOMATICAS:", x = 0.05, y = y, just = "left",
            gp = gpar(fontsize = 11, fontface = "bold", col = "#2C3E50"))
  y <- y - 0.04
  
  alertas <- list()
  if (!is.null(comp_global) && nrow(comp_global) > 0) {
    blastos_row <- comp_global[comp_global$Poblacion == "Blastos", ]
    if (nrow(blastos_row) > 0 && blastos_row$Pct > 1)
      alertas <- c(alertas, list(data.frame(
        Tipo = "Blastos", Mensaje = sprintf(
          "Blastos > 1%% (%s%%): sugiere aspirado de medula osea",
          blastos_row$Pct), stringsAsFactors = FALSE)))
    if (nrow(blastos_row) > 0 && blastos_row$Pct > 20)
      alertas <- c(alertas, list(data.frame(
        Tipo = "Sospecha LMA", Mensaje = sprintf(
          "Blastos > 20%% (%s%%): criterio OMS 2022 para LMA",
          blastos_row$Pct), stringsAsFactors = FALSE)))
  }
  if (!is.null(qc_df) && any(qc_df$QC_flag))
    alertas <- c(alertas, list(data.frame(
      Tipo = "QC", Mensaje = "Alta remocion de eventos: verificar calidad de la muestra",
      stringsAsFactors = FALSE)))
  if (!is.null(expr_global) && nrow(expr_global) > 0) {
    cd45_exp <- expr_global[grepl("CD45", expr_global$Marcador, ignore.case = TRUE), ]
    if (nrow(cd45_exp) > 0) {
      min_cd45 <- min(cd45_exp$Media_expr, na.rm = TRUE)
      if (!is.na(min_cd45) && min_cd45 < 0.3)
        alertas <- c(alertas, list(data.frame(
          Tipo = "CD45", Mensaje = "Expresion CD45 baja: verificar tincion o compensacion",
          stringsAsFactors = FALSE)))
    }
  }
  
  if (length(alertas) > 0) {
    alertas_df <- do.call(rbind, alertas)
    for (i in seq_len(nrow(alertas_df))) {
      grid.text(paste0("  [", alertas_df$Tipo[i], "] ", alertas_df$Mensaje[i]),
                x = 0.06, y = y, just = "left",
                gp = gpar(fontsize = 8.5, fontface = "bold", col = "#E74C3C"))
      y <- y - 0.035
    }
  } else {
    grid.text("  Sin alertas significativas.", x = 0.06, y = y, just = "left",
              gp = gpar(fontsize = 9, col = "#27AE60"))
    y <- y - 0.03
  }
  
  y <- y - 0.02
  grid.text("NOTAS DE REVISION:", x = 0.05, y = y, just = "left",
            gp = gpar(fontsize = 11, fontface = "bold", col = "#2C3E50"))
  y <- y - 0.035
  
  notas <- c(
    "Este es un analisis PRELIMINAR automatizado basado en CLSI H62.",
    "Los resultados deben ser interpretados por un hematologo calificado.",
    "FlowSOM k=5 (Blastos, Linfocitos, Monocitos, Granulocitos, Eosinofilos).",
    "Umbral de positividad: logicle > 1.5 (orientativo).",
    "Se requiere correlacion clinica con morfologia, citogenetica y molecular."
  )
  for (n in notas) {
    grid.text(paste0("  * ", n), x = 0.06, y = y, just = "left",
              gp = gpar(fontsize = 8, col = "#5D6D7E"))
    y <- y - 0.025
  }
  popViewport()
}

pagina_prediagnostico_clsi <- function(expr_global, comp_global, nombre_paciente) {
  grid.newpage()
  pushViewport(viewport(width = 0.94, height = 0.94))
  
  grid.rect(x = 0.5, y = 0.96, width = 1, height = 0.07,
            gp = gpar(fill = "#1A252F", col = NA))
  grid.text("Analisis Preliminar - Hipotesis Diagnostica", x = 0.5, y = 0.965,
            gp = gpar(fontsize = 14, fontface = "bold", col = "white"))
  grid.text(paste0("Basado en CLSI H62 + OMS 2022 - ", nombre_paciente),
            x = 0.5, y = 0.94, gp = gpar(fontsize = 9, col = "grey40"))
  
  y <- 0.88
  grid.text("HALLAZGOS PRINCIPALES:", x = 0.05, y = y, just = "left",
            gp = gpar(fontsize = 11, fontface = "bold", col = "#2C3E50"))
  y <- y - 0.04
  
  hallazgos <- c("Distribucion de poblaciones dentro de parametros generales.")
  if (!is.null(comp_global) && nrow(comp_global) > 0) {
    blastos_row <- comp_global[comp_global$Poblacion == "Blastos", ]
    if (nrow(blastos_row) > 0 && blastos_row$Pct > 1)
      hallazgos <- c(hallazgos, "Poblacion blastica aumentada. Verificar aspirado de medula osea.")
    if (nrow(blastos_row) > 0 && blastos_row$Pct > 20)
      hallazgos <- c(hallazgos, "Blastos >20%: sugerente de LMA segun OMS 2022.")
  }
  for (h in hallazgos) {
    grid.text(paste0("  * ", h), x = 0.06, y = y, just = "left",
              gp = gpar(fontsize = 9, fontface = "bold"))
    y <- y - 0.03
  }
  
  y <- y - 0.02
  grid.text("PERFIL ANTIGENICO (basado en marcadores detectados):", x = 0.05, y = y,
            just = "left", gp = gpar(fontsize = 11, fontface = "bold", col = "#2C3E50"))
  y <- y - 0.04
  
  if (!is.null(expr_global) && nrow(expr_global) > 0) {
    markers_found <- unique(expr_global$Marcador)
    markers_str <- paste(markers_found, collapse = ", ")
    grid.text(paste0("  Marcadores analizados: ", markers_str),
              x = 0.06, y = y, just = "left",
              gp = gpar(fontsize = 8.5))
    y <- y - 0.03
  }
  
  y <- y - 0.02
  grid.text("SOPORTE OMS 2022:", x = 0.05, y = y, just = "left",
            gp = gpar(fontsize = 11, fontface = "bold", col = "#2C3E50"))
  y <- y - 0.04
  
  oms_notes <- c(
    "LMA con diferenciacion minima: CD34+, CD117+, MPO+ (por IHQ/citoquimica).",
    "LMA sin diferenciacion: CD34+, CD38+, HLA-DR+, sin marcadores de linea.",
    "LMA con diferenciacion monocitica: CD14+, CD64+, CD11b+, CD4+.",
    "LMA promielocitica (APL): CD33+, CD13+, HLA-DR-, CD34- (tipico).",
    "LMA megacarioblastica: CD41+, CD61+, CD36+.",
    "NOTA: La clasificacion definitiva requiere citogenetica y molecular."
  )
  for (o in oms_notes) {
    grid.text(paste0("  - ", o), x = 0.06, y = y, just = "left",
              gp = gpar(fontsize = 7.5, col = "#5D6D7E"))
    y <- y - 0.022
  }
  
  grid.text(paste0("DISCLAIMER: Este pre-diagnostico es automatico y PRELIMINAR. ",
                   "No reemplaza la evaluacion de un hematologo. ",
                   "Requiere confirmacion con citogenetica, FISH y biologia molecular."),
            x = 0.5, y = 0.02, gp = gpar(fontsize = 7.5, col = "#922B21", fontface = "bold"))
  popViewport()
}

pagina_aps_3d_html <- function(fusion_df, clave_paciente, output_dir) {
  if (is.null(fusion_df) || nrow(fusion_df) < 100) return(NULL)
  df <- fusion_df[fusion_df$Poblacion %in% ORDEN_POB, , drop = FALSE]
  if (nrow(df) < 100) return(NULL)
  MAX_3D <- 5000
  if (nrow(df) > MAX_3D) {
    set.seed(SEMILLA)
    df <- df[sample(nrow(df), MAX_3D), , drop = FALSE]
    rm(fusion_df); gc()
  }
  df$Poblacion <- factor(df$Poblacion, levels = ORDEN_POB)
  df_s <- df
  tiene_fsc <- "fsc" %in% colnames(df_s) && !all(is.na(df_s$fsc))
  if (tiene_fsc) {
    df_s$z <- log10(pmax(df_s$fsc, 1))
    eje_z <- "FSC-A (log10)"
    titulo_z <- "FSC-A (tamano celular, log10)"
    tooltip_z <- paste0("<br>FSC: ", round(df_s$fsc, 0))
  } else {
    dens <- tryCatch(
      MASS::kde2d(df_s$cd45, df_s$ssc, n = 30),
      error = function(e) NULL
    )
    if (!is.null(dens)) {
      idx_x <- findInterval(df_s$cd45, dens$x, all.inside = TRUE)
      idx_y <- findInterval(df_s$ssc, dens$y, all.inside = TRUE)
      df_s$z <- dens$z[cbind(idx_x, idx_y)]
    } else {
      df_s$z <- runif(nrow(df_s), -0.5, 0.5)
    }
    eje_z <- "Densidad"
    titulo_z <- "Densidad celular (KDE)"
    tooltip_z <- ""
  }
  n_total <- format(nrow(fusion_df), big.mark = ",")
  if (tiene_fsc) {
    subtitle_text <- paste0("CD45/SSC/FSC: 3 parametros reales  |  N = ", n_total,
                            " eventos  |  Estrategia CLSI H62")
  } else {
    subtitle_text <- paste0("Todos los tubos fundidos | N = ", n_total, " eventos")
  }
  pop_counts <- table(fusion_df$Poblacion[fusion_df$Poblacion %in% ORDEN_POB])
  pop_pct <- prop.table(pop_counts) * 100
  present_levels <- intersect(levels(df_s$Poblacion), names(pop_pct))
  pct_labels <- vapply(present_levels, function(lvl) {
    pct <- sprintf("%.1f", pop_pct[lvl])
    paste0(lvl, " (", pct, "%)")
  }, character(1))
  df_s$Poblacion <- factor(df_s$Poblacion, levels = present_levels, labels = unname(pct_labels))
  fig <- plot_ly(
    df_s,
    x = ~cd45,
    y = ~ssc,
    z = ~z,
    color = ~Poblacion,
    colors = setNames(COLORES_CLSI[names(COLORES_CLSI) %in% present_levels],
                      unname(pct_labels)),
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 3.5, opacity = 0.85),
    text = ~paste0("Poblacion: ", sub(" \\(.*", "", Poblacion),
                   "<br>CD45: ", round(cd45, 2),
                   "<br>SSC: ", round(ssc, 0),
                   tooltip_z),
    hoverinfo = "text"
  ) %>%
    layout(
      title = list(
        text = paste0("APS 3D Interactivo - ", clave_paciente,
                      "<br><sup>", subtitle_text, "</sup>"),
        font = list(size = 14)
      ),
      scene = list(
        xaxis = list(title = "CD45 (logicle)", titlefont = list(size = 14),
                     tickfont = list(size = 11), showspikes = TRUE, spikecolor = "#b0b0b0", spikethickness = 1),
        yaxis = list(title = "SSC-A (raw)", titlefont = list(size = 14),
                     tickfont = list(size = 11), showspikes = TRUE, spikecolor = "#b0b0b0", spikethickness = 1),
        zaxis = list(title = titulo_z, titlefont = list(size = 14),
                     tickfont = list(size = 11), showspikes = TRUE, spikecolor = "#b0b0b0", spikethickness = 1),
        camera = list(eye = list(x = 2.0, y = 1.6, z = 0.8)),
        aspectmode = "cube"
      ),
      margin = list(l = 0, r = 0, b = 0, t = 50),
      paper_bgcolor = "#ffffff",
      legend = list(title = list(text = "Poblacion CLSI H62"), font = list(size = 12),
                    x = 1.05, y = 1, itemsizing = "constant")
    ) %>%
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = c("lasso2d", "select2d"),
           displaylogo = FALSE)
  ruta_widget <- file.path(output_dir, "widgets", paste0(clave_paciente, "_APS_3D.html"))
  dir.create(dirname(ruta_widget), showWarnings = FALSE, recursive = TRUE)
  tryCatch({
    saveWidget(fig, ruta_widget, selfcontained = FALSE, libdir = paste0(ruta_widget, "_libs"))
    wc <- readLines(ruta_widget, warn = FALSE)
    wc <- gsub("</head>",
               '<script src=\"https://cdn.plot.ly/plotly-2.35.2.min.js\" charset=\"utf-8\"></script></head>',
               wc)
    writeLines(wc, ruta_widget)
  }, error = function(e) {})
  rm(df_s, fig); gc()
  ruta_widget
}

# ===========================================================================
# FUNCION PRINCIPAL: analizar_fcs
# ===========================================================================
# Recibe un vector de rutas a archivos FCS y un directorio de salida.
# Ejecuta el pipeline completo y retorna la ruta del reporte HTML.
#
# Parametros:
#   fcs_paths    - character vector con rutas absolutas a archivos .fcs
#   output_dir   - directorio donde escribir resultados
#   patient_id   - identificador del paciente (opcional, default = auto)
#   gating_yaml  - ruta a archivo YAML de gating template (opcional)
#
# Retorna:
#   list(ruta_html, ruta_pdf, pacientes, error)
# ===========================================================================
analizar_fcs <- function(fcs_paths, output_dir, patient_id = NULL,
                          gating_yaml = NULL) {
  if (length(fcs_paths) == 0) {
    return(list(ruta_html = NULL, ruta_pdf = NULL, pacientes = 0,
                error = "No se proporcionaron archivos FCS"))
  }
  
  archivos_fcs <- normalizePath(fcs_paths, mustWork = FALSE)
  archivos_fcs <- archivos_fcs[file.exists(archivos_fcs)]
  if (length(archivos_fcs) == 0) {
    return(list(ruta_html = NULL, ruta_pdf = NULL, pacientes = 0,
                error = "Ningun archivo FCS existe en las rutas proporcionadas"))
  }
  
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  infos <- lapply(archivos_fcs, extraer_info_archivo)
  nombres <- sapply(infos, `[[`, "panel")
  es_control <- toupper(nombres) %in% toupper(PANELES_CONTROL)
  
  archivos_analizar <- archivos_fcs[!es_control]
  n_total <- length(archivos_analizar)
  
  if (n_total == 0) {
    return(list(ruta_html = NULL, ruta_pdf = NULL, pacientes = 0,
                error = "Todos los archivos son tubos de control"))
  }
  
  if (is.null(patient_id) || nchar(patient_id) == 0) {
    patient_id <- paste0("Px_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }
  
  grupos_paciente <- setNames(list(archivos_analizar), patient_id)
  
  dir.create(file.path(output_dir, "reportes_clsi"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(output_dir, "tablas_clsi"), showWarnings = FALSE, recursive = TRUE)
  
  resumen_global_clsi <- list()
  
  for (clave_paciente in names(grupos_paciente)) {
    tubos_rutas <- grupos_paciente[[clave_paciente]]
    
    ruta_pdf <- file.path(output_dir, "pdf", paste0(clave_paciente, "_CLSI_H62.pdf"))
    dir.create(dirname(ruta_pdf), showWarnings = FALSE, recursive = TRUE)
    if (file.exists(ruta_pdf)) unlink(ruta_pdf)
    
    resultado_paciente <- tryCatch({
      tubos_ff_trans <- list()
      tubos_info     <- list()
      tubos_marcad   <- list()
      canal_cd45     <- NA
      qc_info_rows   <- list()
      
      for (ruta in tubos_rutas) {
        info_tubo  <- extraer_info_archivo(ruta)
        nomb_tubo  <- tools::file_path_sans_ext(basename(ruta))
        
        if (toupper(info_tubo$panel) %in% toupper(PANELES_CONTROL)) next
        
        resultado_tubo <- tryCatch({
          ff_raw      <- read.FCS(ruta, transformation = FALSE, truncate_max_range = FALSE)
          ff_raw      <- aplicar_compensacion(ff_raw)
          panel_info  <- detectar_marcadores(ff_raw)
          canales_fl  <- names(panel_info$todos_canales)
          
          if (is.na(panel_info$cd45_canal))
            stop("Canal CD45 no detectado en ", basename(ruta))
          if (is.na(canal_cd45)) canal_cd45 <- panel_info$cd45_canal
          
          qc_stats  <- aplicar_peacoqc(ff_raw)
          ff_trans  <- transformar_fcs(qc_stats$ff_limpio, canales_fl)
          ff_sc     <- gate_scatter(ff_trans)
          ff_gated  <- gate_singlets(ff_sc)
          
          marcadores_tubo <- unname(panel_info$otros_canales)
          tubos_ff_trans[[nomb_tubo]] <- ff_gated
          tubos_info[[nomb_tubo]]     <- info_tubo
          tubos_marcad[[nomb_tubo]]   <- marcadores_tubo
          qc_info_rows[[nomb_tubo]] <- data.frame(
            Panel = info_tubo$panel, QC_pct = qc_stats$pct_removido,
            QC_flag = qc_stats$pct_removido > UMBRAL_QC_FLAG, stringsAsFactors = FALSE)
          TRUE
        }, error = function(e) FALSE)
      }
      
      if (length(tubos_ff_trans) == 0)
        stop("Ningun tubo procesado correctamente para ", clave_paciente)
      
      if (is.na(canal_cd45)) stop("Canal CD45 no detectado en ningun tubo")
      pob_por_tubo <- clasificar_cd45ssc_clsi(tubos_ff_trans, canal_cd45)
      names(pob_por_tubo) <- names(tubos_ff_trans)
      
      datos_fusion    <- list()
      expr_por_tubo   <- list()
      comp_por_tubo   <- list()
      expr_global_all <- list()
      datos_infinicyt <- list()
      
      for (nomb_tubo in names(tubos_ff_trans)) {
        ff        <- tubos_ff_trans[[nomb_tubo]]
        pobs_vec  <- pob_por_tubo[[nomb_tubo]]
        info_tubo <- tubos_info[[nomb_tubo]]
        marcad    <- tubos_marcad[[nomb_tubo]]
        
        canales_extraer <- c(canal_cd45, CANAL_SSC)
        canales_expr <- colnames(exprs(ff))
        if (CANAL_FSC %in% canales_expr) canales_extraer <- c(canales_extraer, CANAL_FSC)
        mat_df <- as.data.frame(exprs(ff)[, canales_extraer, drop = FALSE])
        colnames(mat_df)[1:2] <- c("cd45", "ssc")
        if (length(canales_extraer) > 2) colnames(mat_df)[3] <- "fsc"
        mat_df$Poblacion <- pobs_vec
        mat_df$Tubo      <- nomb_tubo
        mat_df$Panel     <- info_tubo$panel
        datos_fusion[[nomb_tubo]] <- mat_df
        
        tbl_pob <- as.data.frame(table(pobs_vec))
        colnames(tbl_pob) <- c("Poblacion", "N")
        tbl_pob$Pct   <- round(100 * tbl_pob$N / sum(tbl_pob$N), 1)
        tbl_pob$Tubo  <- nomb_tubo
        tbl_pob$Panel <- info_tubo$panel
        comp_por_tubo[[nomb_tubo]] <- tbl_pob
        
        if (length(marcad) > 0) {
          datos_mdf <- as.data.frame(exprs(ff))
          panel_info_t <- detectar_marcadores(ff)
          canales_fl_t <- names(panel_info_t$otros_canales)
          nombres_mk   <- unname(panel_info_t$otros_canales)
          for (j in seq_along(canales_fl_t)) {
            if (canales_fl_t[j] %in% colnames(datos_mdf))
              colnames(datos_mdf)[colnames(datos_mdf) == canales_fl_t[j]] <- nombres_mk[j]
          }
          marcad_ok <- intersect(marcad, colnames(datos_mdf))
          if (length(marcad_ok) > 0) {
            expr_t <- caracterizar_marcadores(datos_mdf, pobs_vec, marcad_ok)
            expr_t$Tubo  <- nomb_tubo
            expr_t$Panel <- info_tubo$panel
            expr_por_tubo[[nomb_tubo]]  <- expr_t
            expr_global_all[[nomb_tubo]] <- expr_t
            inf_df <- datos_mdf[, marcad_ok, drop = FALSE]
            cd45_ch <- panel_info_t$cd45_canal
            if (!is.null(cd45_ch) && !is.na(cd45_ch) && cd45_ch %in% colnames(datos_mdf))
              inf_df$CD45 <- datos_mdf[[cd45_ch]]
            if (CANAL_SSC %in% colnames(datos_mdf))
              inf_df$SSC <- datos_mdf[[CANAL_SSC]]
            inf_df$Poblacion <- pobs_vec
            inf_df$Tubo      <- nomb_tubo
            inf_df$Panel     <- info_tubo$panel
            datos_infinicyt[[nomb_tubo]] <- inf_df
          }
        }
      }
      
      fusion_df    <- do.call(rbind, datos_fusion)
      expr_global  <- do.call(rbind, expr_global_all)
      comp_df      <- do.call(rbind, comp_por_tubo)
      df_infinicyt <- tryCatch({
        if (length(datos_infinicyt) > 0) {
          todas_cols <- unique(unlist(lapply(datos_infinicyt, colnames)))
          tubos_alin <- lapply(datos_infinicyt, function(df) {
            for (col in setdiff(todas_cols, colnames(df))) df[[col]] <- NA_real_
            df[, todas_cols, drop = FALSE]
          })
          do.call(rbind, tubos_alin)
        } else NULL
      }, error = function(e) NULL)
      qc_df <- do.call(rbind, qc_info_rows)
      
      comp_global <- aggregate(x = comp_df$N, by = list(Poblacion = comp_df$Poblacion), FUN = sum)
      colnames(comp_global)[2] <- "N"
      comp_global$Pct <- round(100 * comp_global$N / sum(comp_global$N), 1)
      comp_global <- comp_global[order(match(comp_global$Poblacion, ORDEN_POB)), ]
      
      write.csv(expr_global, file.path(output_dir, "tablas_clsi",
                                        paste0(clave_paciente, "_expr_clsi.csv")), row.names = FALSE)
      write.csv(comp_df, file.path(output_dir, "tablas_clsi",
                                    paste0(clave_paciente, "_composicion_clsi.csv")), row.names = FALSE)
      
      pdf(ruta_pdf, width = 14, height = 10)
      pagina_portada_clsi(clave_paciente, tubos_info, comp_global)
      pagina_cd45ssc_global(fusion_df, clave_paciente)
      if (!is.null(expr_global) && nrow(expr_global) > 0)
        pagina_heatmap_poblaciones(expr_global, clave_paciente)
      pagina_distribucion_tubos(comp_df, clave_paciente)
      if (!is.null(expr_global) && nrow(expr_global) > 0)
        pagina_aps_clsi(expr_global, clave_paciente)
      if (!is.null(fusion_df) && nrow(fusion_df) > 0) {
        aps_2d_3d_paginas <- pagina_aps_2d_3d(fusion_df, clave_paciente)
      } else if (!is.null(df_infinicyt) && nrow(df_infinicyt) > 0) {
        aps_2d_3d_paginas <- pagina_aps_2d_3d(df_infinicyt, clave_paciente)
      } else if (!is.null(expr_global) && nrow(expr_global) > 0) {
        aps_2d_3d_paginas <- pagina_aps_2d_3d(expr_global, clave_paciente)
      } else {
        aps_2d_3d_paginas <- NULL
      }
      if (!is.null(aps_2d_3d_paginas)) {
        for (pagina in aps_2d_3d_paginas) {
          grid.newpage()
          pushViewport(viewport(width = 0.97, height = 0.96))
          grid.draw(pagina)
          upViewport()
        }
      }
      if (!is.null(df_infinicyt) && nrow(df_infinicyt) > 0)
        pagina_seccion_infinicyt(df_infinicyt, clave_paciente)
      for (nomb_tubo in names(tubos_ff_trans)) {
        mat_tubo  <- datos_fusion[[nomb_tubo]]
        expr_tubo <- expr_por_tubo[[nomb_tubo]]
        pagina_detalle_tubo(mat_tubo, expr_tubo, nomb_tubo, canal_cd45)
      }
      pagina_analisis_clsi(expr_global, comp_global, clave_paciente, qc_df)
      pagina_prediagnostico_clsi(expr_global, comp_global, clave_paciente)
      dev.off()
      
      dir_salida <- output_dir
      ruta_html  <- file.path(dir_salida, paste0(clave_paciente, "_CLSI_H62.html"))
      ruta_png_base <- file.path(dir_salida, paste0(clave_paciente, "_pagina"))
      
      ruta_pdf_abs <- normalizePath(ruta_pdf)
      png_prefix <- basename(ruta_png_base)
      system2("pdftoppm", c("-png", "-r", "72", ruta_pdf_abs, ruta_png_base),
              stdout = FALSE, stderr = FALSE)
      Sys.sleep(0.5)
      all_png <- list.files(dir_salida, pattern = "\\.png$")
      png_files <- sort(file.path(dir_salida,
                                  grep(paste0("^", png_prefix, "-"), all_png, value = TRUE)))
      
      ruta_widget_rel <- ""
      if (!is.null(fusion_df) && nrow(fusion_df) > 0) {
        ruta_widget <- pagina_aps_3d_html(fusion_df, clave_paciente, output_dir)
        if (!is.null(ruta_widget) && nchar(ruta_widget) > 0) {
          ruta_widget_rel <- file.path("widgets", basename(ruta_widget))
        }
      }
      
      png_data_urls <- lapply(png_files, function(f) {
        paste0("data:image/png;base64,",
               base64enc::base64encode(readBin(f, raw(), file.info(f)$size)))
      })
      unlink(png_files)
      
      rm(fusion_df, comp_df, qc_df, datos_fusion, comp_por_tubo, qc_info_rows)
      rm(tubos_ff_trans, tubos_info, tubos_marcad, expr_por_tubo, expr_global_all, datos_infinicyt)
      gc()
      
      n_pags <- length(png_data_urls)
      etiquetas <- character(0)
      if (n_pags >= 1) etiquetas <- c(etiquetas, "Portada")
      if (n_pags >= 2) etiquetas <- c(etiquetas, "CD45 / SSC")
      if (n_pags >= 3) etiquetas <- c(etiquetas, "Heatmap")
      if (n_pags >= 4) etiquetas <- c(etiquetas, "Distribucion")
      if (n_pags >= 5) etiquetas <- c(etiquetas, "APS")
      resto <- n_pags - length(etiquetas)
      if (resto > 0) etiquetas <- c(etiquetas, paste0("Pagina ", seq(length(etiquetas) + 1, n_pags)))
      
      paginas_html <- paste(sapply(seq_len(n_pags), function(i) {
        lbl <- if (i <= length(etiquetas)) etiquetas[i] else sprintf("Pag %d", i)
        lbl_id <- gsub("[^A-Za-z0-9]", "_", tolower(lbl))
        sprintf(
          '<section class="pagina" id="%s"><div class="seccion-titulo">%s</div><img src="%s" alt="%s"></section>',
          lbl_id, lbl, png_data_urls[[i]], lbl)
      }), collapse = "\n    ")
      
      widgets_html <- ""
      if (nchar(ruta_widget_rel) > 0) {
        widget_id <- paste0("pag-3d-", gsub("[^A-Za-z0-9]", "_", clave_paciente))
        widgets_html <- paste0(
          '<section class="pagina-3d" id="', widget_id, '">',
          '<div class="seccion-titulo">APS 3D Interactivo - Paisaje de Densidad CD45/SSC</div>',
          '<div class="seccion-cuerpo" style="text-align:center;">',
          '<iframe src="', ruta_widget_rel, '" style="width:100%;height:600px;border:none;border-radius:4px;"',
          ' onerror="this.style.display=\'none\'"></iframe>',
          '<p style="font-size:11px;color:#888;margin-top:6px;">',
          'Widget 3D: <a href="', ruta_widget_rel, '" target="_blank">abrir en ventana aparte</a>',
          ' &nbsp;|&nbsp; Requiere internet para plotly.js</p>',
          '</div></section>'
        )
      }
      
      nav_items <- character(0)
      for (i in seq_len(n_pags)) {
        lbl <- if (i <= length(etiquetas)) etiquetas[i] else sprintf("Pag %d", i)
        lbl_id <- gsub("[^A-Za-z0-9]", "_", tolower(lbl))
        nav_items <- c(nav_items, sprintf('<a href="#%s">%s</a>', lbl_id, lbl))
      }
      if (nchar(ruta_widget_rel) > 0) {
        widget_id <- paste0("pag-3d-", gsub("[^A-Za-z0-9]", "_", clave_paciente))
        nav_items <- c(nav_items, sprintf('<a href="#%s">APS 3D</a>', widget_id))
      }
      
      fecha_str <- format(Sys.Date(), "%d/%m/%Y")
      
      html_body <- paste0(
        '<!DOCTYPE html><html lang="es"><head>',
        '<meta charset="UTF-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
        '<title>Reporte CLSI H62 - ', clave_paciente, '</title>',
        '<style>',
        '* { box-sizing: border-box; margin: 0; padding: 0; }',
        'body { font-family: "Segoe UI", Arial, sans-serif; background: #e9ecef; color: #212529; }',
        '.layout { display: flex; min-height: 100vh; }',
        '.sidebar { width: 190px; background: #1A252F; color: #ECF0F1; position: sticky; top: 0; height: 100vh; overflow-y: auto; padding: 14px 0; flex-shrink: 0; }',
        '.sidebar h2 { font-size: 10px; text-transform: uppercase; letter-spacing: 1px; padding: 0 13px 9px; border-bottom: 1px solid rgba(255,255,255,.2); margin-bottom: 5px; }',
        '.sidebar a { display: block; padding: 6px 13px; color: #BFC9CA; text-decoration: none; font-size: 11px; transition: background .12s; }',
        '.sidebar a:hover, .sidebar a.active { background: rgba(255,255,255,.13); color: #fff; }',
        '.main { flex: 1; padding: 22px 28px; max-width: 1200px; }',
        '.header { background: linear-gradient(135deg, #1A252F, #2C3E50); color: white; padding: 20px 24px; border-radius: 8px; margin-bottom: 20px; }',
        '.header h1 { font-size: 20px; margin: 0 0 3px; }',
        '.header p { font-size: 12px; opacity: 0.85; margin: 0; }',
        '.pagina, .pagina-3d, .seccion { background: white; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.12); margin-bottom: 20px; overflow: hidden; }',
        '.seccion-titulo { background: #F2F3F4; border-bottom: 2px solid #3498DB; padding: 8px 15px; font-size: 13px; font-weight: bold; color: #1A5276; }',
        '.seccion-cuerpo { padding: 13px 15px; }',
        '.pagina img { display: block; width: 100%; height: auto; }',
        '.pagina-3d .seccion-cuerpo { padding: 16px; }',
        '.footer { border-top: 1px solid #D5D8DC; margin-top: 26px; padding-top: 9px; font-size: 11px; color: #888; text-align: center; }',
        '@media (max-width: 768px) { .layout { flex-direction: column; } .sidebar { width: 100%; height: auto; position: static; display: flex; flex-wrap: wrap; padding: 8px; } .sidebar h2 { display: none; } .main { padding: 12px; } }',
        '</style>',
        '</head><body>',
        '<div class="layout">',
        '<nav class="sidebar"><h2>Secciones</h2>',
        paste(nav_items, collapse = ""),
        '</nav>',
        '<main class="main">',
        '<div class="header"><h1>Inmunofenotipificacion CLSI H62</h1><p>Analisis: <strong>', clave_paciente,
        '</strong> &nbsp;|&nbsp; ', fecha_str, ' &nbsp;|&nbsp; ', n_pags, ' paginas &nbsp;|&nbsp; Umbral logicle: ',
        UMBRAL_POS_CLSI, ' &nbsp;|&nbsp; Poblaciones: ', N_POB_CLSI, '</p></div>',
        paginas_html,
        widgets_html,
        '<div class="footer">Lab. Citometria de Flujo &nbsp;|&nbsp; CLSI H62 &nbsp;|&nbsp; FlowSOM k=', N_POB_CLSI,
        ' &nbsp;|&nbsp; <strong>ESTE REPORTE NO CONSTITUYE UN DIAGNOSTICO DEFINITIVO.</strong></div>',
        '</main></div>',
        '<script>',
        'var secs = document.querySelectorAll("section[id]");',
        'var lnks = document.querySelectorAll(".sidebar a");',
        'window.addEventListener("scroll",function(){',
        '  var pos = window.scrollY + 80;',
        '  for (var i=0;i<secs.length;i++){',
        '    var s = secs[i];',
        '    if (s.offsetTop <= pos && s.offsetTop + s.offsetHeight > pos){',
        '      for (var j=0;j<lnks.length;j++){lnks[j].classList.remove("active");}',
        '      var a = document.querySelector(".sidebar a[href=\'#"+s.id+"\']");',
        '      if (a) a.classList.add("active");',
        '    }',
        '  }',
        '},{passive:true});',
        'for (var i=0;i<lnks.length;i++){',
        '  lnks[i].addEventListener("click",function(e){',
        '    var href = this.getAttribute("href"); if (!href || href[0]!=="#") return;',
        '    var target = document.getElementById(href.substring(1));',
        '    if (target) target.scrollIntoView({behavior:"smooth"});',
        '  });',
        '}',
        '</script>',
        '</body></html>'
      )
      
      writeLines(html_body, ruta_html, useBytes = FALSE)
      
      list(expr = expr_global, comp = comp_global, ruta_html = ruta_html, ruta_pdf = ruta_pdf)
      
    }, error = function(e) {
      while (length(dev.list()) > 0) dev.off()
      list(error = conditionMessage(e))
    })
    
    if (!is.null(resultado_paciente) && is.null(resultado_paciente$error)) {
      resumen_global_clsi[[clave_paciente]] <- resultado_paciente
    }
  }
  
  if (length(resumen_global_clsi) > 0) {
    primer_resultado <- resumen_global_clsi[[1]]
    list(ruta_html = primer_resultado$ruta_html %||% NULL,
         ruta_pdf  = primer_resultado$ruta_pdf %||% NULL,
         pacientes = length(resumen_global_clsi),
         error = NULL)
  } else {
    list(ruta_html = NULL, ruta_pdf = NULL, pacientes = 0,
         error = resultado_paciente$error %||% "Error desconocido en el pipeline")
  }
}
