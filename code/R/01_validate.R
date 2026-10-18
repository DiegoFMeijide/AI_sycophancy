#!/usr/bin/env Rscript
# Validate public evidence before estimating or interpreting results.
suppressPackageStartupMessages({library(jsonlite); library(digest)})
options(stringsAsFactors = FALSE)
sha <- function(p) digest(file = p, algo = "sha256", serialize = FALSE)
m <- fromJSON("data/provenance.json", simplifyVector = FALSE)
stopifnot(m$rows == 14400L, m$pilot_rows == 0L, m$conditions == 288L,
          m$official_attempts == 14426L, m$official_failed_attempts == 26L)
stopifnot(setequal(names(m$output_sha256), c("analysis_14400.csv", "prompts_144.csv",
  "request_assignment_14400.csv", "request_templates_576.json", "response_mappings.json",
  "failed_attempts_26.csv", "web_metadata_10800.jsonl.gz")))
for (f in names(m$output_sha256)) {
  stopifnot(!grepl("(^/|[.][.])", f), sha(file.path("data", f)) == m$output_sha256[[f]])
}
d <- read.csv("data/analysis_14400.csv", check.names = FALSE, colClasses = c(r = "character"))
p <- read.csv("data/prompts_144.csv", check.names = FALSE)
a <- read.csv("data/request_assignment_14400.csv", check.names = FALSE)
t <- fromJSON("data/request_templates_576.json", simplifyVector = FALSE)
maps <- fromJSON("data/response_mappings.json", simplifyVector = FALSE)
f <- read.csv("data/failed_attempts_26.csv", check.names = FALSE)
stopifnot(nrow(d) == 14400L, !anyDuplicated(d$call_id), all(d$derived_accepted),
          nrow(p) == 144L, nrow(a) == 14400L, !anyDuplicated(a$call_id),
          setequal(a$call_id, d$call_id), length(t) == 576L, length(maps) == 4L,
          nrow(f) == 26L, length(unique(f$call_id)) == 24L, all(f$call_id %in% d$call_id),
          sum(d$recovered) == 24L, sum(d$metadata_flagged) == 32L,
          sum(!d$raw_accepted) == 3L, !any(f$analytic_row_included),
          all(d$returned_model == "gpt-6-luna"), all(d$arg + d$balanced + d$gbr == 1L))
stopifnot(setequal(unique(d$scale_id), c("ORDINAL5", "BINARY2")),
  setequal(unique(d$arm), c("CONTROL", "MITIGATION")),
  setequal(unique(d$location), c("HIST_NO_WEB", "HIST_DEFAULT", "AR", "GB")),
  setequal(unique(d$language_level), c("EN", "ES_NEUTRAL", "ES_AR")),
  setequal(unique(d$toponym_id), c("MALVINAS", "FALKLANDS")),
  setequal(unique(d$origin_id), c("NONE", "BUENOS_AIRES", "LONDON")))
cell <- interaction(d[c("scale_id", "arm", "location", "language_level", "toponym_id", "origin_id")], drop = TRUE)
stopifnot(length(table(cell)) == 288L, all(table(cell) == 50L),
          all(table(d$scale_id) == 7200L), all(table(d$scale_id, d$location) == 1800L),
          sum(d$scale_id == "BINARY2" & d$arg == 1L) == 2529L,
          sum(d$scale_id == "ORDINAL5" & d$arg == 1L) == 1036L,
          sum(d$scale_id == "ORDINAL5" & d$balanced == 1L) == 2532L,
          sum(d$scale_id == "ORDINAL5" & d$gbr == 1L) == 3632L,
          all(d$search_used[d$search_regime == "NO_WEB"] == 0L),
          all(d$search_used[d$search_regime == "WEB_AUTO"] == 1L))
mapping_ids <- vapply(maps, `[[`, character(1), "mapping_id")
template_ids <- vapply(t, `[[`, character(1), "template_id")
stopifnot(!anyDuplicated(p$prompt_id), !anyDuplicated(p$prompt_sha256),
          !anyDuplicated(mapping_ids), !anyDuplicated(template_ids),
          all(d$mapping_id %in% mapping_ids), all(d$template_id %in% template_ids),
          all(d$prompt_id %in% p$prompt_id))
maps <- setNames(maps, mapping_ids)
t <- setNames(t, template_ids)
a <- a[match(d$call_id, a$call_id), ]
stopifnot(identical(a$template_id, d$template_id), identical(a$prompt_id, d$prompt_id),
          identical(a$mapping_id, d$mapping_id),
          all(a$question_country_order == ifelse(d$scale_id == "ORDINAL5", "ARG_FIRST", d$orientation)))
for (i in seq_len(nrow(p))) stopifnot(digest(p$prompt[i], algo = "sha256", serialize = FALSE) == p$prompt_sha256[i])
for (mp in maps) stopifnot(digest(as.character(toJSON(mp$response_mapping, auto_unbox = TRUE)),
  algo = "sha256", serialize = FALSE) == mp$mapping_sha256)
allowed_request_fields <- c("model", "input", "store", "text", "tools", "tool_choice",
                            "include", "max_tool_calls", "max_output_tokens")
required_request_fields <- c("model", "input", "store", "text", "tools", "tool_choice", "max_output_tokens")
for (i in seq_len(nrow(d))) {
  row <- d[i, ]; mp <- maps[[row$mapping_id]]; tr <- t[[row$template_id]]
  q <- tr$request; prompt <- p[p$prompt_id == row$prompt_id, ]
  n_options <- if (row$scale_id == "ORDINAL5") 5L else 2L
  fmt <- q$text$format; schema <- fmt$schema
  stopifnot(nrow(prompt) == 1L, tr$prompt_id == row$prompt_id, tr$mapping_id == row$mapping_id,
    !is.null(mp), !is.null(tr), tr$scale_id == row$scale_id, tr$environment == row$location,
    mp$scale_id == row$scale_id, mp$orientation == row$orientation,
    identical(names(mp$response_mapping), as.character(seq_len(n_options))),
    row$r %in% names(mp$response_mapping),
    mp$response_mapping[[row$r]] == row$choice_id,
    mp$mapping_sha256 == row$mapping_sha256, prompt$prompt_sha256 == row$prompt_sha256,
    all(vapply(c("scale_id", "arm", "language_level", "toponym_id", "origin_id", "orientation"),
      function(k) identical(prompt[[k]], row[[k]]), logical(1))),
    all(names(q) %in% allowed_request_fields), all(required_request_fields %in% names(q)),
    q$model == "gpt-6-luna", identical(q$store, FALSE),
    length(q$input) == 1L, q$input[[1]]$role == "user", q$input[[1]]$content == prompt$prompt,
    q$max_output_tokens == 8192L, nzchar(row$e),
    identical(fmt$type, "json_schema"), identical(fmt$strict, TRUE), identical(schema$type, "object"),
    identical(schema$additionalProperties, FALSE), setequal(unlist(schema$required), c("r", "e")),
    setequal(names(schema$properties), c("r", "e")), identical(schema$properties$r$type, "string"),
    identical(unlist(schema$properties$r$enum, use.names = FALSE), as.character(seq_len(n_options))),
    identical(schema$properties$e$type, "string"), is.null(q$reasoning), is.null(q$temperature),
    is.null(q$top_p), is.null(q$text$verbosity))
  stopifnot(row$arg == as.integer(row$choice_id %in% c("ARG", "ARG_CLEAR", "ARG_SOME")),
    row$balanced == as.integer(row$choice_id == "BALANCED"),
    row$gbr == as.integer(row$choice_id %in% c("GBR", "GBR_CLEAR", "GBR_SOME")))
  if (row$search_regime == "NO_WEB") stopifnot(length(q$tools) == 0L, q$tool_choice == "none",
    is.null(q$max_tool_calls), row$location == "HIST_NO_WEB",
    row$location_sent == "NOT_APPLICABLE", row$location_reported == "NOT_APPLICABLE")
  else {
    stopifnot(row$search_regime == "WEB_AUTO", length(q$tools) == 1L,
      q$tools[[1]]$type == "web_search", q$tool_choice == "auto", q$max_tool_calls == 4L,
      identical(unlist(q$include, use.names = FALSE), "web_search_call.action.sources"))
    if (row$location %in% c("AR", "GB")) {
      loc <- q$tools[[1]]$user_location
      stopifnot(setequal(names(loc), c("type", "country")), loc$type == "approximate",
        loc$country == row$location, row$location_sent == row$location, row$location_reported == row$location)
    } else stopifnot(row$location == "HIST_DEFAULT", is.null(q$tools[[1]]$user_location),
      row$location_sent == "OMITTED", row$location_reported == "US")
  }
}
cat("VALIDATION_OK: 14,400 official responses; 288 cells x 50; prompts, mappings, requests and hashes verified.\n")
