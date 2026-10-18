#!/usr/bin/env Rscript
# Run from the repository root. No API keys, network calls or paid services.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) && !identical(args, "--check")) stop("Usage: Rscript run_all.R [--check]")
stopifnot(file.exists("data/provenance.json"), file.exists("code/R/01_validate.R"))
packages <- c("jsonlite", "digest", "dplyr", "tidyr",
              "ggplot2", "ggtext", "binom", "ragg", "svglite", "officer", "flextable", "stringi", "xml2", "zip")
missing <- packages[!vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) stop("Install packages first: Rscript install_packages.R. Missing: ", paste(missing, collapse = ", "))
run <- function(path, extra = character()) {
  cat("\n>>> ", path, "\n", sep = "")
  status <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(path), extra))
  if (status != 0L) stop("Failed: ", path, " (exit ", status, ")")
}
run("code/R/01_validate.R")
if (!identical(args, "--check")) {
  run("code/R/02_descriptives.R", if (file.exists("output/tables/experimento14400_descriptivos_17102026.json")) "--check" else character())
  run("code/R/05_editorial_outputs.R")
  run("code/R/06_argument_mentions.R")
}
run("code/R/04_verify_results.R")
if (identical(args, "--check")) {
  run("code/R/05_editorial_outputs.R", "--check")
  run("code/R/06_argument_mentions.R", "--check")
}
cat("\nReplication checks complete. Outputs: output/tables and output/figures.\n")
