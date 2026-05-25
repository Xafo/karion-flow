os <- system("cat /etc/os-release | grep ^VERSION_CODENAME", intern = TRUE)
os <- tolower(sub("VERSION_CODENAME=", "", os))
cat("OS detectado:", os, "\n")

if (os == "noble") {
  rspm <- "https://packagemanager.rstudio.com/all/__linux__/noble/latest"
} else {
  rspm <- paste0("https://packagemanager.rstudio.com/all/__linux__/", os, "/latest")
}

options(repos = structure(c(CRAN = rspm)))
Sys.setenv(MAKEFLAGS = "-j1")

cran <- c(
  "ggplot2", "gridExtra", "MASS", "scatterplot3d", "plotly",
  "htmltools", "htmlwidgets", "base64enc", "jsonlite", "plumber",
  "yaml", "mime"
)

bioc <- c("flowCore", "PeacoQC", "FlowSOM")

inst <- function(pkg, repo = getOption("repos")) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("  [SKIP]", pkg, "\n"); return(invisible(TRUE))
  }
  cat("  Instalando:", pkg, "\n")
  result <- tryCatch(
    install.packages(pkg, repos = repo, quiet = TRUE),
    error = function(e) e
  )
  if (inherits(result, "error") || !requireNamespace(pkg, quietly = TRUE)) {
    cat("  [FALLO]", pkg, "- abortando\n")
    quit(save = "no", status = 1)
  }
}

cat("\n=== CRAN packages ===\n")
for (p in cran) inst(p)

if (!requireNamespace("BiocManager", quietly = TRUE))
  inst("BiocManager")

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
fail <- c()
for (p in c(cran, bioc)) {
  ok <- requireNamespace(p, quietly = TRUE)
  cat(if (ok) "  [OK]" else "  [FALTA]", p, "\n")
  if (!ok) fail <- c(fail, p)
}
if (length(fail)) {
  cat("\nFaltan:", paste(fail, collapse = ", "), "\n")
  quit(save = "no", status = 1)
}
cat("\nInstalacion completada.\n")
