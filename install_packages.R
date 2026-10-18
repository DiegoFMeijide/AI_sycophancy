#!/usr/bin/env Rscript
# This is the only setup step that accesses the internet (CRAN).
# It does not contact OpenAI or create billable requests.
packages <- c("jsonlite", "digest", "dplyr", "tidyr",
              "ggplot2", "ggtext", "binom", "ragg", "svglite", "officer", "flextable", "stringi", "xml2", "zip")
missing <- packages[!vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
cat("Required packages are installed. See documents/software_versions.csv for tested versions.\n")
