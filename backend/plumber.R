# =============================================================================
# plumber.R  --  API REST para Karion-Flow
# =============================================================================
# Ejecutar: R -e "library(plumber); pr('plumber.R') %>% pr_run(host='0.0.0.0', port=7860)"
# Endpoints:
#   POST /api/analizar   -> Recibe FCS, inicia analisis
#   GET  /api/estado/:id -> Estado del analisis
#   GET  /api/reporte/:id -> HTML del reporte
#   GET  /api/gates/:id   -> Datos de poblaciones detectadas
#   GET  /api/widget3d/:id -> Widget 3D interactivo
#   GET  /api/template    -> Template de gating actual
#   POST /api/template    -> Actualiza template de gating
#   GET  /api/health      -> Health check
# =============================================================================

library(plumber)
library(jsonlite)

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

get_creds <- function() {
  raw <- Sys.getenv("KARION_USERS", "")
  if (nzchar(raw)) {
    tryCatch(fromJSON(raw), error = function(e) list())
  } else {
    list(admin = "admin123", user = "user123")
  }
}

#' @filter auth
function(req, res) {
  if (req$REQUEST_METHOD == "OPTIONS") return(plumber::forward())
  if (grepl("^/api/health", req$PATH_INFO)) return(plumber::forward())

  ahdr <- req$HTTP_AUTHORIZATION %||% ""
  if (!grepl("^Basic ", ahdr)) {
    res$status <- 401
    res$setHeader("WWW-Authenticate", "Basic realm=\"Karion-Flow\"")
    return(list(error = "Autenticacion requerida"))
  }

  decoded <- rawToChar(base64dec(sub("^Basic ", "", ahdr)))
  parts <- strsplit(decoded, ":", fixed = TRUE)[[1]]
  user <- parts[1] %||% ""
  pass <- parts[2] %||% ""

  creds <- get_creds()
  if (is.null(creds[[user]]) || creds[[user]] != pass) {
    res$status <- 401
    res$setHeader("WWW-Authenticate", "Basic realm=\"Karion-Flow\"")
    return(list(error = "Credenciales invalidas"))
  }
  plumber::forward()
}

#' @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")

  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }

  plumber::forward()
}

ANALISIS_DIR <- file.path(tempdir(), "karion_analisis")
dir.create(ANALISIS_DIR, showWarnings = FALSE, recursive = TRUE)

GATING_TEMPLATE <- list(
  version = "1.0",
  poblaciones = c("Blastos", "Linfocitos", "Monocitos", "Granulocitos", "Eosinofilos"),
  umbral_positividad = 1.5,
  n_poblaciones = 5
)

#' Health check
#' @get /api/health
function() {
  list(
    status = "ok",
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    r_version = R.version.string
  )
}

#' Subir archivos FCS e iniciar analisis (base64 via JSON)
#' @parser json
#' @post /api/analizar
function(req, res) {
  tryCatch({
    body <- req$body
    if (is.null(body)) {
      res$status <- 400
      return(list(error = "Cuerpo de solicitud vacio. Enviar JSON: {files: [{name, data}]}"))
    }
    
    files_df <- body[["files"]]
    if (is.null(files_df) || nrow(files_df) == 0) {
      res$status <- 400
      return(list(error = "No se recibieron archivos en el campo 'files'"))
    }

    fcs_paths <- character()
    for (i in seq_len(nrow(files_df))) {
      fname <- files_df$name[i]
      fdata <- files_df$data[i]
      if (!is.null(fdata) && !is.na(fdata) && grepl("\\.fcs$", fname %||% "", ignore.case = TRUE)) {
        tmp <- tempfile(fileext = ".fcs")
        writeBin(base64enc::base64decode(fdata), tmp)
        fcs_paths <- c(fcs_paths, tmp)
      }
    }

    if (length(fcs_paths) == 0) {
      res$status <- 400
      return(list(error = "No se encontraron archivos .fcs validos en la subida"))
    }

    analysis_id <- paste0("KF-", format(Sys.time(), "%Y%m%d-%H%M%S"), "-",
                          substr(paste0(sample(c(0:9, letters, LETTERS), 6, replace = TRUE), collapse = ""), 1, 6))

    patient_id <- body$paciente %||% body$patient_id %||% ""
    
    output_dir <- file.path(ANALISIS_DIR, analysis_id)
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

    fcs_dest <- file.path(output_dir, "fcs")
    dir.create(fcs_dest, showWarnings = FALSE)
    for (f in fcs_paths) {
      file.copy(f, file.path(fcs_dest, basename(f)), overwrite = TRUE)
    }
    fcs_final <- list.files(fcs_dest, pattern = "\\.fcs$", full.names = TRUE, ignore.case = TRUE)

    write_json(list(fcs_paths = fcs_final, patient_id = patient_id),
               file.path(output_dir, "metadata.json"), auto_unbox = TRUE)

    estado_file <- file.path(output_dir, "estado.json")
    write_json(list(
      id = analysis_id, estado = "procesando",
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      archivos = length(fcs_final), paciente = patient_id
    ), estado_file, auto_unbox = TRUE)

    system2("Rscript", c("run_job.R", output_dir), wait = FALSE)

    list(id = analysis_id, estado = "procesando",
         archivos = length(fcs_final),
         mensaje = sprintf("Analisis iniciado: %d archivo(s)", length(fcs_final)))

  }, error = function(e) {
    res$status <- 500
    list(error = conditionMessage(e))
  })
}

#' Obtener estado de un analisis
#' @get /api/estado/<id>
function(id, res) {
  output_dir <- file.path(ANALISIS_DIR, id)
  estado_file <- file.path(output_dir, "estado.json")
  result_file <- file.path(output_dir, "resultado.json")

  if (!file.exists(output_dir)) {
    res$status <- 404
    return(list(error = "Analisis no encontrado", id = id))
  }
  
  if (file.exists(result_file)) {
    resultado <- fromJSON(result_file)
    resultado$id <- id
    return(resultado)
  }
  
  if (!file.exists(estado_file)) {
    res$status <- 500
    return(list(error = "Archivo de estado no encontrado", id = id))
  }

  fromJSON(estado_file)
}

#' Obtener reporte HTML
#' @get /api/reporte/<id>
#' @serializer contentType list(type="text/html")
function(id, res) {
  output_dir <- file.path(ANALISIS_DIR, id)
  result_file <- file.path(output_dir, "resultado.json")

  if (!file.exists(output_dir)) {
    res$status <- 404
    return(list(error = "Analisis no encontrado"))
  }
  if (!file.exists(result_file)) {
    res$status <- 400
    return(list(error = "Analisis no completado"))
  }

  resultado <- fromJSON(result_file)
  if (resultado$estado != "completado") {
    res$status <- 400
    return(list(error = "Analisis no completado", estado = resultado$estado))
  }

  ruta_html <- resultado$ruta_html
  if (is.null(ruta_html) || is.na(ruta_html) || nchar(ruta_html) == 0 || !file.exists(ruta_html)) {
    posibles <- list.files(output_dir, pattern = "\\.html$", full.names = TRUE)
    if (length(posibles) > 0) {
      ruta_html <- posibles[1]
    } else {
      res$status <- 500
      return(list(error = "Reporte HTML no encontrado en el analisis"))
    }
  }

  readBin(ruta_html, "raw", file.info(ruta_html)$size)
}

#' Obtener datos de poblaciones (gates)
#' @get /api/gates/<id>
function(id, res) {
  output_dir <- file.path(ANALISIS_DIR, id)
  if (!file.exists(output_dir)) {
    res$status <- 404
    return(list(error = "Analisis no encontrado"))
  }

  tablas_dir <- file.path(output_dir, "tablas_clsi")
  csv_files <- list.files(tablas_dir, pattern = "_composicion_clsi\\.csv$", full.names = TRUE)
  expr_files <- list.files(tablas_dir, pattern = "_expr_clsi\\.csv$", full.names = TRUE)

  list(
    id = id,
    composicion = if (length(csv_files) > 0) read.csv(csv_files[1], stringsAsFactors = FALSE) else list(),
    expresion = if (length(expr_files) > 0) read.csv(expr_files[1], stringsAsFactors = FALSE) else list()
  )
}

#' Obtener widget 3D
#' @get /api/widget3d/<id>
function(id, res) {
  output_dir <- file.path(ANALISIS_DIR, id)
  widget_dir <- file.path(output_dir, "widgets")
  widget_files <- list.files(widget_dir, pattern = "\\.html$", full.names = TRUE)

  if (length(widget_files) == 0) {
    res$status <- 404
    return(list(error = "Widget 3D no encontrado para este analisis"))
  }

  wf <- widget_files[1]
  res$setHeader("Content-Type", "text/html; charset=utf-8")
  readBin(wf, "raw", file.info(wf)$size)
}

#' Obtener template de gating actual
#' @get /api/template
function() {
  GATING_TEMPLATE
}

#' Actualizar template de gating
#' @post /api/template
function(req, res) {
  tryCatch({
    body <- req$body
    if (is.character(body)) {
      body <- fromJSON(body)
    }
    if (length(body) == 0) {
      res$status <- 400
      return(list(error = "Cuerpo de solicitud vacio"))
    }
    if (!is.null(body$umbral_positividad)) {
      GATING_TEMPLATE$umbral_positividad <<- as.numeric(body$umbral_positividad)
    }
    if (!is.null(body$n_poblaciones)) {
      GATING_TEMPLATE$n_poblaciones <<- as.integer(body$n_poblaciones)
    }
    list(status = "ok", mensaje = "Template actualizado", template = GATING_TEMPLATE)
  }, error = function(e) {
    res$status <- 400
    list(error = conditionMessage(e))
  })
}
