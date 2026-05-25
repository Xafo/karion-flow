os <- system("cat /etc/os-release | grep ^VERSION_CODENAME", intern = TRUE)
os <- tolower(sub("VERSION_CODENAME=", "", os))
rspm <- paste0("https://packagemanager.rstudio.com/all/__linux__/", os, "/latest")
options(repos = structure(c(CRAN = rspm)))
Sys.setenv(MAKEFLAGS = "-j1")

# All CRAN packages needed by FlowSOM/PeacoQC/flowCore transitively
# Installed as RSPM binary (seconds each, no OOM risk)
cran <- c(
  "plumber", "plotly", "scatterplot3d", "base64enc", "jsonlite", "yaml",
  "Rcpp", "matrixStats", "data.table", "foreach", "doParallel", "plyr",
  "abind", "rjson", "iterators", "colorspace", "clue", "png", "shape",
  "RcppArmadillo", "colorRamps", "Rtsne", "corrplot", "ggrepel", "ggsci",
  "cowplot", "ggsignif", "ggforce", "ggnewscale", "ggpubr", "rstatix",
  "polynom", "tweenr", "polyclip", "systemfonts",
  "igraph", "car", "carData", "lme4", "nloptr", "minqa", "RcppEigen",
  "pbkrtest", "quantreg", "SparseM", "MatrixModels",
  "forecast", "fracdiff", "lmtest", "timeDate", "urca", "zoo",
  "broom", "doBy", "numDeriv", "microbenchmark", "modelr",
  "Deriv", "Rdpack", "rbibutils", "backports", "Formula",
  "httpuv", "sodium", "swagger", "webutils", "openssl", "curl",
  "crayon", "later", "promises", "stringi", "stringr", "digest", "magrittr",
  "dplyr", "tidyr", "tibble", "pillar", "lazyeval", "crosstalk", "purrr",
  "fastmap", "knitr", "rmarkdown", "xfun", "bslib", "sass", "jquerylib",
  "fontawesome", "evaluate", "highr", "rappdirs", "cachem", "memoise",
  "XML", "R6", "httr", "generics", "tidyselect", "pkgconfig"
)

cat("=== Installing ALL CRAN deps as binary via RSPM ===\n")
for (p in cran) {
  p_lower <- tolower(p)
  if (requireNamespace(p_lower, quietly = TRUE) || requireNamespace(p, quietly = TRUE)) {
    cat("  [SKIP]", p, "\n")
    next
  }
  cat("  Instalando:", p, "\n")
  tryCatch(
    install.packages(p, quiet = TRUE),
    error = function(e) cat("  [WARN]", p, ":", conditionMessage(e), "\n")
  )
}

cat("\n=== Bioconductor packages ===\n")
bioc_targets <- c("flowCore", "PeacoQC", "FlowSOM")
for (pkg in bioc_targets) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("  [SKIP]", pkg, "\n")
    next
  }
  cat("  Instalando:", pkg, "\n")
  result <- tryCatch(
    BiocManager::install(pkg, update = FALSE, ask = FALSE, quiet = TRUE),
    error = function(e) e
  )
  if (inherits(result, "error") || !requireNamespace(pkg, quietly = TRUE)) {
    cat("  [FATAL]", pkg, "- fallo\n")
    quit(save = "no", status = 1)
  }
}

cat("\n=== Verificacion final ===\n")
all_pkgs <- unique(c(tolower(cran), bioc_targets))
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
