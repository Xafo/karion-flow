os <- system("cat /etc/os-release | grep ^VERSION_CODENAME", intern = TRUE)
os <- tolower(sub("VERSION_CODENAME=", "", os))
rspm <- paste0("https://packagemanager.rstudio.com/all/__linux__/", os, "/latest")
options(repos = structure(c(CRAN = rspm)))
Sys.setenv(MAKEFLAGS = "-j1")

installed_names <- function() tolower(rownames(installed.packages()))
is_installed <- function(pkg) tolower(pkg) %in% installed_names()

cat("=== Resolviendo dependencias CRAN de Bioconductor ===\n")

if (!is_installed("BiocManager"))
  install.packages("BiocManager", quiet = TRUE)

all_repos <- BiocManager::repositories()
options(repos = all_repos)

avail <- tryCatch(available.packages(), error = function(e) NULL)
if (is.null(avail)) {
  cat("No se pudieron consultar los repositorios. Abortando.\n")
  quit(save = "no", status = 1)
}

db <- as.data.frame(avail, stringsAsFactors = FALSE)

bioc_targets <- c("flowCore", "PeacoQC", "FlowSOM")

all_deps <- unique(unlist(
  tools::package_dependencies(bioc_targets, db = avail,
    which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE)
))
cat("Total de dependencias recursivas:", length(all_deps), "\n")

bioc_pkgs <- rownames(db[grepl("Bioconductor", db$Repository, ignore.case = TRUE), ])
base_pkgs <- rownames(installed.packages(priority = "base"))

cran_deps <- setdiff(all_deps, c(bioc_pkgs, base_pkgs, bioc_targets))
cat("Dependencias CRAN a instalar:", length(cran_deps), "\n")

for (p in cran_deps) {
  if (is_installed(p)) { cat("  [SKIP]", p, "\n"); next }
  cat("  Instalando:", p, "\n")
  tryCatch(install.packages(p, quiet = TRUE),
    error = function(e) cat("  [WARN]", conditionMessage(e), "\n"))
}

extra <- c("plumber", "plotly", "scatterplot3d", "base64enc", "yaml")
for (p in extra) {
  if (is_installed(p)) next
  cat("  Instalando extra:", p, "\n")
  install.packages(p, quiet = TRUE)
}

cat("\n=== Instalando paquetes Bioconductor ===\n")
for (p in bioc_targets) {
  if (is_installed(p)) {
    cat("  [SKIP]", p, "\n"); next
  }
  cat("  Instalando:", p, "\n")
  tryCatch(
    BiocManager::install(p, update = FALSE, ask = FALSE, quiet = TRUE),
    error = function(e) {
      cat("  [FATAL]", p, ":", conditionMessage(e), "\n")
      quit(save = "no", status = 1)
    }
  )
  if (!is_installed(p)) {
    cat("  [FATAL]", p, "- no se instalo\n")
    quit(save = "no", status = 1)
  }
}

cat("\n=== Verificacion final ===\n")
all_pkgs <- unique(c(bioc_targets, cran_deps, extra,
  "ggplot2", "gridExtra", "MASS", "htmltools", "htmlwidgets", "mime", "jsonlite"))
fail <- c()
for (p in all_pkgs) {
  ok <- is_installed(p)
  cat(if (ok) "  [OK]" else "  [FALTA]", p, "\n")
  if (!ok) fail <- c(fail, p)
}
if (length(fail)) {
  cat("\nFaltan:", paste(fail, collapse = ", "), "\n")
  quit(save = "no", status = 1)
}
cat("\nInstalacion completada exitosamente.\n")
