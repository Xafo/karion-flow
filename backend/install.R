os <- system("cat /etc/os-release | grep ^VERSION_CODENAME", intern = TRUE)
os <- tolower(sub("VERSION_CODENAME=", "", os))
rspm <- paste0("https://packagemanager.rstudio.com/all/__linux__/", os, "/latest")
options(repos = structure(c(CRAN = rspm)))
Sys.setenv(MAKEFLAGS = "-j1")

pkgs <- c("plumber", "plotly", "scatterplot3d", "base64enc", "yaml")

cat("=== Instalando paquetes CRAN faltantes ===\n")
for (p in pkgs) {
  if (requireNamespace(p, quietly = TRUE)) {
    cat("  [SKIP]", p, "\n")
  } else {
    cat("  Instalando:", p, "\n")
    install.packages(p, quiet = TRUE)
  }
}

cat("\n=== Verificacion ===\n")
required <- c(pkgs, "flowCore", "FlowSOM", "PeacoQC", "ggplot2", "igraph",
              "Rcpp", "RcppArmadillo", "jsonlite", "htmltools", "htmlwidgets",
              "gridExtra", "MASS", "base64enc", "plumber", "yaml", "mime")
fail <- c()
installed_pkgs <- tolower(rownames(installed.packages()))
for (p in required) {
  ok <- tolower(p) %in% installed_pkgs
  cat(if (ok) "  [OK]" else "  [FALTA]", p, "\n")
  if (!ok) fail <- c(fail, p)
}
if (length(fail)) {
  cat("\nFaltan:", paste(fail, collapse = ", "), "\n")
  quit(save = "no", status = 1)
}
cat("\nOK - todos los paquetes instalados.\n")
