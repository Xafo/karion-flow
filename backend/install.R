# =============================================================================
# install.R  --  Instalacion de paquetes para Karion-Flow
# =============================================================================
# Ejecutar: Rscript install.R

packages <- c(
  "flowCore",
  "PeacoQC",
  "FlowSOM",
  "ggplot2",
  "gridExtra",
  "grid",
  "MASS",
  "scatterplot3d",
  "plotly",
  "htmltools",
  "htmlwidgets",
  "base64enc",
  "jsonlite",
  "plumber",
  "yaml",
  "mime"
)

# Bioconductor packages
bioc_packages <- c("flowCore", "PeacoQC", "FlowSOM")

# CRAN packages
cran_packages <- setdiff(packages, bioc_packages)

cat("Instalando paquetes CRAN...\n")
for (pkg in cran_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("  Instalando:", pkg, "\n")
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  } else {
    cat("  OK:", pkg, "\n")
  }
}

cat("\nInstalando paquetes Bioconductor...\n")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager", repos = "https://cloud.r-project.org", quiet = TRUE)

for (pkg in bioc_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("  Instalando:", pkg, "\n")
    BiocManager::install(pkg, update = FALSE, ask = FALSE, quiet = TRUE)
  } else {
    cat("  OK:", pkg, "\n")
  }
}

cat("\nVerificacion final:\n")
for (pkg in packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("  [OK]", pkg, "\n")
  } else {
    cat("  [FALTA]", pkg, "\n")
  }
}

cat("\nInstalacion completada.\n")
