os <- system("cat /etc/os-release | grep ^VERSION_CODENAME", intern = TRUE)
os <- tolower(sub("VERSION_CODENAME=", "", os))
rspm <- paste0("https://packagemanager.rstudio.com/all/__linux__/", os, "/latest")
options(repos = structure(c(CRAN = rspm)))
Sys.setenv(MAKEFLAGS = "-j1")

cran <- c("plumber", "plotly", "scatterplot3d", "base64enc", "jsonlite", "yaml")

bioc <- c("flowCore", "PeacoQC", "FlowSOM")

inst <- function(pkg, repo = getOption("repos")) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("  [SKIP]", pkg, "\n"); return(invisible(TRUE))
  }
  cat("  Instalando:", pkg, "\n")
  install.packages(pkg, repos = repo, quiet = TRUE)
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("  [FALLO]", pkg, "- abortando\n")
    quit(save = "no", status = 1)
  }
}

cat("=== CRAN packages (binary via RSPM) ===\n")
for (p in cran) inst(p)

cat("\n=== Bioconductor packages ===\n")
for (p in bioc) {
  if (requireNamespace(p, quietly = TRUE)) {
    cat("  [SKIP]", p, "\n")
  } else {
    cat("  Instalando:", p, "\n")
    tryCatch(
      BiocManager::install(p, update = FALSE, ask = FALSE, quiet = TRUE),
      error = function(e) {
        cat("  [FALLO]", p, ":", conditionMessage(e), "\n")
        quit(save = "no", status = 1)
      }
    )
    if (!requireNamespace(p, quietly = TRUE)) {
      cat("  [FALLA GRAVE]", p, "- abortando\n")
      quit(save = "no", status = 1)
    }
  }
}

cat("\n=== Verificacion final ===\n")
all_pkgs <- c(cran, bioc, "ggplot2", "gridExtra", "MASS", "igraph", "htmltools", "htmlwidgets", "mime")
fail <- c()
for (p in all_pkgs) {
  ok <- requireNamespace(p, quietly = TRUE)
  cat(if (ok) "  [OK]" else "  [FALTA]", p, "\n")
  if (!ok) fail <- c(fail, p)
}
if (length(fail)) {
  cat("\nFaltan:", paste(fail, collapse = ", "), "\n")
  quit(save = "no", status = 1)
}
cat("\nInstalacion completada.\n")
