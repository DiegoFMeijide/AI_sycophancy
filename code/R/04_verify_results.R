#!/usr/bin/env Rscript
# Offline verification against the audited descriptive CSV outputs.
# This verifier does not write files. Run from the repository root.
suppressPackageStartupMessages({library(jsonlite); library(digest)})
options(stringsAsFactors = FALSE)
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 0L || identical(args, "--check"))
sha <- function(p) digest(file = p, algo = "sha256", serialize = FALSE)
manifest <- fromJSON("data/provenance.json", simplifyVector = FALSE)
stopifnot(sha("data/analysis_14400.csv") == manifest$output_sha256[["analysis_14400.csv"]])
d <- read.csv("data/analysis_14400.csv", check.names = FALSE)
stopifnot(nrow(d) == 14400L, !anyDuplicated(d$call_id), all(d$derived_accepted),
  all(d$arg + d$balanced + d$gbr == 1L), all(table(d$scale_id, d$location) == 1800L),
  sum(d$recovered) == 24L, sum(d$metadata_flagged) == 32L,
  sum(d$choice_id == "ARG_SOME") == 1036L, sum(d$choice_id == "BALANCED") == 2532L,
  sum(d$choice_id == "GBR_SOME") == 3632L, sum(d$choice_id == "ARG") == 2529L,
  sum(d$choice_id == "GBR") == 4671L,
  !any(d$choice_id %in% c("ARG_CLEAR", "GBR_CLEAR")))
conditions <- table(interaction(d[c("scale_id", "location", "arm", "language_level", "toponym_id", "origin_id")], drop = TRUE))
stopifnot(length(conditions) == 288L, all(conditions == 50L))
cluster <- ifelse(d$cohort == "GEOGRAPHY", paste("GEO", d$scale_id, d$source_block_number, sep = "_"),
  ifelse(d$scale_id == "ORDINAL5", paste("HIST_ORDINAL", d$arm, d$source_block_number, sep = "_"),
    paste("HIST_BINARY", d$source_block_number, sep = "_")))
stopifnot(length(unique(cluster[d$scale_id == "ORDINAL5"])) == 150L,
  length(unique(cluster[d$scale_id == "BINARY2"])) == 100L)

lines <- readLines("expected/SHA256SUMS", warn = FALSE)
stopifnot(length(lines) == 12L, all(grepl("^[0-9a-f]{64}  [A-Za-z0-9_]+\\.csv$", lines)))
expected_hash <- substr(lines, 1, 64)
files <- substring(lines, 67)
stopifnot(!anyDuplicated(files))
for(i in seq_along(files))stopifnot(sha(file.path("expected",files[i]))==expected_hash[i])
main_analysis <- startsWith(files, "experimento14400_")
stopifnot(sum(main_analysis) == 9L)
files <- files[main_analysis]; expected_hash <- expected_hash[main_analysis]
exact_numeric <- c("n","arg_n","balanced_n","gbr_n","ARG_CLEAR","ARG_SOME","BALANCED","GBR_SOME","GBR_CLEAR",
 "recovered_n","metadata_flagged_n","source_block_number")
summary_rows <- list()
for (i in seq_along(files)) {
  reference <- file.path("expected", files[i])
  produced <- file.path("output/tables", files[i])
  stopifnot(file.exists(reference), file.exists(produced), sha(reference) == expected_hash[i])
  x <- read.csv(produced, check.names = FALSE)
  y <- read.csv(reference, check.names = FALSE)
  if (!identical(names(x), names(y)) || nrow(x) != nrow(y)) stop("Schema mismatch: ", files[i])
  tolerance <- 1e-10
  maximum_absolute_difference <- 0
  for (nm in names(x)) {
    a <- x[[nm]]; b <- y[[nm]]
    if (!identical(is.na(a), is.na(b))) stop("Missingness mismatch: ", files[i], " / ", nm)
    if (is.numeric(a) && is.numeric(b)) {
      keep <- !is.na(a)
      delta <- abs(a[keep] - b[keep])
      limit <- if (nm %in% exact_numeric) rep(0, length(delta)) else tolerance * (1 + abs(b[keep]))
      if (any(!is.finite(delta)) || any(delta > limit)) stop("Numeric mismatch: ", files[i], " / ", nm)
      maximum_absolute_difference <- max(c(maximum_absolute_difference, delta))
    } else if (!identical(as.character(a), as.character(b))) {
      stop("Label/value mismatch: ", files[i], " / ", nm)
    }
  }
  summary_rows[[i]] <- data.frame(file = files[i], rows = nrow(x),
    byte_identical = sha(reference) == sha(produced), max_absolute_difference = maximum_absolute_difference,
    numeric_tolerance = tolerance)
}

# Independent direct re-count of every 50-response treatment cell.
c <- read.csv("output/tables/experimento14400_conditions288_17102026.csv", check.names = FALSE)
keys <- c("scale_id", "arm", "language_level", "toponym_id", "origin_id")
for (i in seq_len(nrow(c))) {
  z <- d[d$location == c$environment[i], ]
  for (key in keys) z <- z[z[[key]] == c[[key]][i], ]
  stopifnot(nrow(z) == c$n[i], sum(z$arg) == c$arg_n[i],
    sum(z$balanced) == c$balanced_n[i], sum(z$gbr) == c$gbr_n[i])
}
main <- fromJSON("output/tables/experimento14400_descriptivos_17102026.json")
stopifnot(main$total_responses==14400L,main$conditions==288L,
 main$source_sha256==sha("data/analysis_14400.csv"),
 main$script_sha256==sha("code/R/02_descriptives.R"))
cat("VERIFY_OK: 14,400 official cases, 288 cells, nine descriptive reference CSVs, 250 recorded blocks.\n")
print(do.call(rbind, summary_rows), row.names=FALSE)
cat("No files written; no API calls.\n")
