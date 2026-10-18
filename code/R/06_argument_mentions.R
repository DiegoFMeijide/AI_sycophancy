#!/usr/bin/env Rscript
# Fixed bilingual dictionary: mention presence, regardless of endorsement/choice.
suppressPackageStartupMessages({library(jsonlite); library(digest); library(officer); library(flextable)})
options(stringsAsFactors = FALSE)
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 0L || identical(args, "--check"))
check_only <- identical(args, "--check")
manifest <- fromJSON("data/provenance.json", simplifyVector = FALSE)
stopifnot(digest(file = "data/analysis_14400.csv", algo = "sha256", serialize = FALSE) ==
            manifest$output_sha256[["analysis_14400.csv"]])
source("code/R/mentions_core.R"); source("code/R/mentions_additional.R")
source("code/R/scrub_docx.R")
d <- read.csv("data/analysis_14400.csv", check.names = FALSE)
s <- d[d$search_regime == "NO_WEB" & d$arm == "CONTROL", ]
s <- s[order(s$scale_id, s$call_id), ]; rownames(s) <- NULL
stopifnot(nrow(s) == 1800L, all(table(s$scale_id, s$toponym_id) == 450L),
          !anyDuplicated(s$call_id), all(s$derived_accepted), all(s$search_used == 0L))
codebook <- rbind(core_codebook, additional_codebook)
codes <- codebook$code
flags <- cbind(classify_core(s$e), classify_additional(s$e))
stopifnot(identical(names(flags), codes), all(!flags$SELFDET_LIMITS | flags$ISLANDERS_WISHES))
flags[] <- lapply(flags, as.integer)
counts <- do.call(rbind, lapply(c("ORDINAL5", "BINARY2"), function(sc) {
  do.call(rbind, lapply(c("FALKLANDS", "MALVINAS"), function(tp) {
    idx <- s$scale_id == sc & s$toponym_id == tp
    data.frame(scale_id = sc, toponym_id = tp, code = codes, label = codebook$label,
      mentions = unname(colSums(flags[idx, , drop = FALSE])), n = sum(idx),
      percentage = unname(100 * colMeans(flags[idx, , drop = FALSE])))
  }))
}))
rownames(counts) <- NULL
fmt <- function(p) ifelse(p > 0 & p < 1, "<1", sprintf("%.0f", round(p)))
matrix <- data.frame(code = codes, argumento = codebook$label)
for (sc in c("ORDINAL5", "BINARY2")) {
  for (tp in c("FALKLANDS", "MALVINAS")) {
    z <- counts[counts$scale_id == sc & counts$toponym_id == tp, ]
    stopifnot(identical(z$code, codes))
    matrix[[paste(sc, tp, "n", sep = "_")]] <- z$mentions
    matrix[[paste(sc, tp, "pct", sep = "_")]] <- z$percentage
  }
  matrix[[paste0(sc, "_display")]] <- paste0(fmt(matrix[[paste0(sc, "_FALKLANDS_pct")]]),
    " / ", fmt(matrix[[paste0(sc, "_MALVINAS_pct")]]), " %")
}
stem <- "menciones_argumentos_sin_busqueda_17102026"
objects <- list(conteos = counts, matriz = matrix, categorias = codebook)
for (name in names(objects)) {
  ref <- read.csv(file.path("expected", paste0(stem, "_", name, ".csv")), check.names = FALSE)
  stopifnot(isTRUE(all.equal(objects[[name]], ref, tolerance = 1e-12, check.attributes = FALSE)))
  if (check_only) {
    saved <- read.csv(file.path("output/tables", paste0(stem, "_", name, ".csv")), check.names = FALSE)
    stopifnot(isTRUE(all.equal(objects[[name]], saved, tolerance = 1e-12, check.attributes = FALSE)))
  }
}
if (!check_only) {
  dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
  for (name in names(objects)) write.csv(objects[[name]], file.path("output/tables", paste0(stem, "_", name, ".csv")),
    row.names = FALSE, na = "", fileEncoding = "UTF-8")
  classified <- cbind(s[c("call_id", "scale_id", "toponym_id", "language_level", "origin_id", "choice_id", "e")], flags)
  write.csv(classified, "output/tables/argument_mentions_classified.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8")
  display <- matrix[c("argumento", "ORDINAL5_display", "BINARY2_display")]
  names(display) <- c("Argumento mencionado", "Escala\nFalkland / Malvinas", "Elección forzada\nFalkland / Malvinas")
  ft <- flextable(display)
  ft <- font(ft, fontname = "Arial", part = "all")
  ft <- fontsize(ft, size = 10.5, part = "all")
  ft <- bold(ft, part = "header")
  ft <- color(ft, color = "black", part = "all")
  ft <- bg(ft, bg = "#ECF7FB", part = "header")
  ft <- border_remove(ft)
  ft <- border_outer(ft, border = fp_border(color = "#D9D9D9", width = .6), part = "all")
  ft <- border_inner_h(ft, border = fp_border(color = "#D9D9D9", width = .6), part = "all")
  ft <- border_inner_v(ft, border = fp_border(color = "#D9D9D9", width = .6), part = "all")
  ft <- align(ft, align = "center", j = 2:3, part = "all")
  ft <- align(ft, align = "left", j = 1, part = "all")
  ft <- valign(ft, valign = "center", part = "all")
  ft <- width(ft, j = 1, width = 3.0); ft <- width(ft, j = 2:3, width = 1.7)
  ft <- padding(ft, padding.top = 5, padding.bottom = 5, padding.left = 5, padding.right = 5, part = "all")
  ft <- set_table_properties(ft, layout = "fixed", opts_word = list(split = FALSE, repeat_headers = TRUE))
  doc <- read_docx()
  doc <- docx_set_paragraph_style(doc, style_id = "Title", style_name = "Title",
    fp_p = fp_par(padding.bottom = 7), fp_t = fp_text(font.size = 14, bold = TRUE, font.family = "Arial", color = "black"))
  doc <- body_set_default_section(doc, prop_section(page_size = page_size(width = 8.5, height = 11),
    page_margins = page_mar(top = .8, bottom = .8, left = 1.05, right = 1.05)))
  doc <- body_add_fpar(doc, fpar(ftext("Argumentos mencionados en las explicaciones", fp_text(font.size = 14, bold = TRUE, font.family = "Arial", color = "black"))), style = "Title")
  doc <- body_add_fpar(doc, fpar(ftext("Porcentaje de explicaciones que menciona cada argumento, sin búsqueda ni mitigación. 450 respuestas por nombre y escala; 1.800 en total.", fp_text(font.size = 10.5, font.family = "Arial", color = "black"))))
  doc <- body_add_flextable(doc, ft)
  doc <- body_add_fpar(doc, fpar(ftext("Las menciones se detectan con un diccionario bilingüe fijo, independientemente de la decisión o de si el argumento se respalda o cuestiona. Las categorías pueden superponerse; no deben sumar 100 %. Fuente: elaboración propia con las tandas oficiales.", fp_text(font.size = 10, font.family = "Arial", color = "black"))))
  doc <- set_doc_properties(doc, title = "Argumentos mencionados en las explicaciones", creator = "")
  doc$doc_properties["lastModifiedBy", "value"] <- ""
  print(doc, target = "output/tables/argument_mentions.docx")
  scrub_public_docx("output/tables/argument_mentions.docx")
}
cat("ARGUMENT_MENTIONS_OK: 1,800 official responses; 48 mention counts match reference.\n")
