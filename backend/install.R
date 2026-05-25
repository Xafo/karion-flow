options(repos = structure(c(CRAN = "https://packagemanager.rstudio.com/all/__linux__/jammy/latest")))
Sys.setenv(MAKEFLAGS = "-j1")

cran <- c(
  "ggplot2", "gridExtra", "MASS", "scatterplot3d", "plotly",
  "htmltools", "htmlwidgets", "base64enc", "jsonlite", "plumber",
  "yaml", "mime", "igraph", "Rcpp", "RcppArmadillo"
)

bioc <- c("flowCore", "PeacoQC", "FlowSOM")

inst <- function(pkg, repo = getOption("repos")) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("  [SKIP]", pkg, "\n"); return(invisible(TRUE))
  }
  cat("  Instalando:", pkg, "\n")
  install.packages(pkg, repos = repo, quiet = TRUE)
}

cat("=== CRAN packages (via RSPM binary) ===\n")
for (p in cran) inst(p)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager", repos = getOption("repos"), quiet = TRUE)

cat("\n=== Bioconductor packages ===\n")
for (p in bioc) {
  if (requireNamespace(p, quietly = TRUE)) {
    cat("  [SKIP]", p, "\n")
  } else {
    cat("  Instalando:", p, "\n")
    BiocManager::install(p, update = FALSE, ask = FALSE, quiet = TRUE)
    if (!requireNamespace(p, quietly = TRUE)) {
      cat("  [FALLA GRAVE]", p, "- abortando\n")
      quit(save = "no", status = 1)
    }
  }
}

cat("\n=== Verificacion final ===\n")
fail <- c()
for (p in c(cran, bioc)) {
  ok <- requireNamespace(p, quietly = TRUE)
  cat(if (ok) "  [OK]" else "  [FALTA]", p, "\n")
  if (!ok) fail <- c(fail, p)
}
if (length(fail)) {
  cat("\nPaquetes faltantes:", paste(fail, collapse = ", "), "\n")
  quit(save = "no", status = 1)
}
cat("\nInstalacion completada exitosamente.\n")
