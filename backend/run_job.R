library(jsonlite)
`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

job_dir <- commandArgs(trailingOnly = TRUE)[1]
if (is.null(job_dir) || is.na(job_dir) || !nzchar(job_dir)) {
  cat("Uso: Rscript run_job.R <job_dir>\n")
  quit(save = "no", status = 1)
}

result <- tryCatch({
  metadata <- fromJSON(file.path(job_dir, "metadata.json"))
  fcs_paths <- metadata$fcs_paths
  patient_id <- metadata$patient_id %||% NULL
  output_dir <- job_dir

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

  source("pipeline.R")

  resultado <- analizar_fcs(fcs_paths, output_dir, patient_id)
  list(estado = "completado",
       ruta_html = resultado$ruta_html %||% "",
       ruta_pdf = resultado$ruta_pdf %||% "",
       pacientes = resultado$pacientes %||% 0,
       error = resultado$error %||% "")
}, error = function(e) {
  list(estado = "error", error = conditionMessage(e))
})

write_json(result, file.path(job_dir, "resultado.json"), auto_unbox = TRUE)
cat("Job", result$estado, "\n")
if (result$estado == "error") cat("Error:", result$error, "\n")
