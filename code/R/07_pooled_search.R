#!/usr/bin/env Rscript
# Marginal percentages: assigned search availability, BINARY2 CONTROL.
# Run from the repository root. Existing outputs are verified; --check never writes.
packages <- c("ggplot2", "binom", "jsonlite", "digest", "ragg", "svglite")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install the required R packages: ", paste(missing, collapse = ", "))
suppressPackageStartupMessages({library(ggplot2); library(jsonlite); library(digest)})
options(stringsAsFactors = FALSE, scipen = 999)
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 0L || identical(args, "--check") || identical(args, "--overwrite"))
check_only <- identical(args, "--check")
overwrite <- identical(args, "--overwrite")
script <- "code/R/07_pooled_search.R"
stem <- "search_pooled"
out_dir <- "output/figures"
table_dir <- "output/tables"
source_csv <- "data/analysis_14400.csv"
provenance_file <- "data/provenance.json"
sha <- function(p) digest(file = p, algo = "sha256", serialize = FALSE)
provenance <- fromJSON(provenance_file, simplifyVector = FALSE)
expected_source_hash <- provenance$output_sha256[["analysis_14400.csv"]]
stopifnot(is.character(expected_source_hash), length(expected_source_hash) == 1L,
  sha(source_csv) == expected_source_hash)
input_files <- c(provenance_file, source_csv)
d <- read.csv(source_csv, check.names = FALSE)
required <- c("call_id", "scale_id", "arm", "location", "search_regime",
  "toponym_id", "origin_id", "language_level", "choice_id", "derived_accepted")
stopifnot(all(required %in% names(d)), nrow(d) == 14400L, !anyDuplicated(d$call_id),
  !anyNA(d[required]), all(d$derived_accepted))
s <- d[d$scale_id == "BINARY2" & d$arm == "CONTROL", ]
locations <- c("HIST_NO_WEB", "HIST_DEFAULT", "GB", "AR")
stopifnot(nrow(s) == 3600L, !anyDuplicated(s$call_id),
  all(table(s$location) == 900L), setequal(s$location, locations),
  all(s$choice_id %in% c("ARG", "GBR")),
  all(table(s$location, s$toponym_id, s$origin_id, s$language_level) == 50L))

# Agrupar observaciones, no porcentajes redondeados.
s$search_group <- s$search_regime
groups <- c("NO_WEB", "WEB_AUTO")
stopifnot(all(s$search_group %in% groups),
  sum(s$search_group == "NO_WEB") == 900L,
  sum(s$search_group == "WEB_AUTO") == 2700L,
  all(s$location[s$search_group == "NO_WEB"] == "HIST_NO_WEB"),
  setequal(s$location[s$search_group == "WEB_AUTO"], c("HIST_DEFAULT", "GB", "AR")),
  all(table(s$location[s$search_group == "WEB_AUTO"]) == 900L))
definitions <- list(
  toponym_id = c(FALKLANDS = "Falkland", MALVINAS = "Malvinas"),
  origin_id = c(LONDON = "Londres", NONE = "No lo dice", BUENOS_AIRES = "Buenos Aires"),
  language_level = c(EN = "Inglés", ES_NEUTRAL = "Español neutro", ES_AR = "Español con voseo"))
factor_labels <- c(toponym_id = "Nombre", origin_id = "Origen declarado", language_level = "Idioma")
arg_levels <- c(toponym_id = "MALVINAS", origin_id = "BUENOS_AIRES", language_level = "ES_AR")
rows <- list()
for (group in groups) for (f in names(definitions)) for (level in names(definitions[[f]])) {
  q <- s[s$search_group == group & s[[f]] == level, ]
  locations_in_group <- if (group == "NO_WEB") "HIST_NO_WEB" else c("HIST_DEFAULT", "GB", "AR")
  stopifnot(setequal(q$location, locations_in_group))
  if (group == "WEB_AUTO") stopifnot(length(unique(as.integer(table(q$location)))) == 1L)
  rows[[length(rows)+1L]] <- data.frame(search_group = group, factor = f, signal = factor_labels[[f]],
    level = level, label = definitions[[f]][[level]], denominator = nrow(q),
    argentina_n = sum(q$choice_id == "ARG"), argentina_pct = 100 * mean(q$choice_id == "ARG"),
    argentine_variant = level == arg_levels[[f]])
}
counts <- do.call(rbind, rows)
counts$rounded_pct <- floor(counts$argentina_pct + .5)
stopifnot(nrow(counts) == 16L,
  all(counts$denominator == ifelse(counts$search_group == "NO_WEB",
    ifelse(counts$factor == "toponym_id", 450, 300),
    ifelse(counts$factor == "toponym_id", 1350, 900))))
groups <- c("NO_WEB", "WEB_AUTO")
factors <- c("toponym_id", "origin_id", "language_level")
ci <- binom::binom.confint(counts$argentina_n, counts$denominator, conf.level = .95, methods = "asymptotic")
stopifnot(nrow(ci)==nrow(counts), all(ci$method=="asymptotic"),
  all(abs(100*ci$mean-counts$argentina_pct)<1e-12))
counts$lower <- 100 * ci$lower
counts$upper <- 100 * ci$upper
counts$search_group <- factor(counts$search_group, levels = groups)
headers <- list(); axis_rows <- list(); cursor <- 0
for (f in factors) {
  q <- counts[counts$factor == f & counts$search_group == "NO_WEB", ]
  headers[[length(headers) + 1L]] <- data.frame(y = -cursor, text = q$signal[1])
  cursor <- cursor + 1.05
  for (i in seq_len(nrow(q))) {
    counts$plot_y[counts$factor == f & counts$level == q$level[i]] <- -cursor
    axis_rows[[length(axis_rows) + 1L]] <- data.frame(y = -cursor, label = q$label[i])
    cursor <- cursor + 1.15
  }
  cursor <- cursor + .35
}
headers <- do.call(rbind, headers); axis_rows <- do.call(rbind, axis_rows)
fills <- c("FALSE" = "#969DA3", "TRUE" = "#6DBBDE")
make_plot <- function(titled) {
  p <- ggplot() +
    geom_rect(data = headers, aes(xmin = 0, xmax = 70, ymin = y - .38, ymax = y + .4),
      fill = "white", colour = NA) +
    geom_rect(data = counts, aes(xmin = 0, xmax = argentina_pct,
      ymin = plot_y - .31, ymax = plot_y + .31, fill = argentine_variant),
      colour = "#606C76", linewidth = .35) +
    geom_errorbar(data = counts, aes(y = plot_y, xmin = lower, xmax = upper),
      orientation = "y", width = .26, linewidth = .65, colour = "#34434C") +
    geom_text(data = counts, aes(x = upper + 1.4, y = plot_y,
      label = paste0(rounded_pct, "%")), size = 5.1, hjust = 0,
      fontface = "bold", family = "sans", colour = "black") +
    geom_text(data = headers, aes(x = 0, y = y, label = text), hjust = 0,
      fontface = "bold", family = "sans", size = 5.8, colour = "black") +
    facet_wrap(vars(search_group), nrow = 1, scales = "fixed",
      labeller = as_labeller(c(NO_WEB = "Sin búsqueda en internet", WEB_AUTO = "Con búsqueda en internet"))) +
    scale_fill_manual(values = fills, guide = "none") +
    scale_x_continuous(limits = c(0, 70), breaks = seq(0, 60, 20),
      labels = function(x) paste0(x, "%"), expand = expansion(mult = 0)) +
    scale_y_continuous(limits = c(min(counts$plot_y) - .75, .65),
      breaks = axis_rows$y, labels = axis_rows$label, expand = expansion(mult = 0)) +
    labs(x = "Respuestas que eligen Argentina", y = NULL,
      title = if (titled) "Elección de Argentina, con y sin búsqueda en internet" else NULL,
      subtitle = if (titled) paste("Elección forzada · Sin mitigación · 900 respuestas sin búsqueda en internet y 2.700 con búsqueda en internet",
        "Con búsqueda en internet: EE. UU. (por defecto), Reino Unido y Argentina, agrupados · IC del 95%", sep = "\n") else NULL,
      caption = NULL) +
    theme_minimal(base_size = 17, base_family = "sans") +
    theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#EBEEF0", linewidth = .35),
      panel.spacing.x = grid::unit(42, "pt"),
      axis.text.x = element_text(size = 17, colour = "black"),
      axis.text.y = element_text(size = 17, colour = "black", margin = margin(r = 12)),
      axis.title.x = element_text(size = 19, colour = "black", margin = margin(t = 15)),
      axis.line.x = element_line(colour = "#AFB8BF", linewidth = .4),
      strip.text = element_text(size = 21, face = "bold", colour = "black", margin = margin(b = 15)),
      strip.background = element_blank(), legend.position = "none",
      plot.title = element_text(size = 27, face = "bold", colour = "black", margin = margin(b = 10)),
      plot.subtitle = element_text(size = 17, colour = "black", margin = margin(b = 20)),
      plot.title.position = "plot", plot.margin = margin(22, 30, 20, 20),
      plot.background = element_rect(fill = "white", colour = NA))
  built <- ggplot_build(p)
  layout <- built$layout$layout
  bars <- built$data[[2]]
  stopifnot(nrow(layout) == 2L, all(layout$ROW == 1L),
    identical(as.character(layout$search_group), groups), nrow(bars) == 16L,
    all(bars$xmin == 0), all(table(bars$PANEL) == 8L),
    all(abs(bars$xmax - counts$argentina_pct) < 1e-12),
    all(bars$fill == unname(fills[as.character(counts$argentine_variant)])),
    all(built$data[[4]]$label == paste0(counts$rounded_pct, "%")),
    all(abs(built$data[[3]]$xmin - counts$lower) < 1e-12),
    all(abs(built$data[[3]]$xmax - counts$upper) < 1e-12),
    all(vapply(built$layout$panel_params, function(x) identical(x$x.range, c(0, 70)), logical(1))),
    identical(built$layout$panel_params[[1]]$y.range, built$layout$panel_params[[2]]$y.range),
    p$theme$legend.position == "none", is.null(p$labels$caption))
  p
}

variants <- c("con_titulos", "sin_titulos")
formats <- c("png", "pdf", "svg")
files <- unlist(lapply(variants, function(v) file.path(out_dir, v, formats, paste0(stem, ".", formats))))
csv <- file.path(table_dir, "editorial_search_pooled_counts.csv")
manifest_path <- file.path(table_dir, "search_pooled_manifest.json")
check_only <- check_only || (!overwrite && file.exists(manifest_path))
if (check_only) stopifnot(file.exists(manifest_path))
if (!check_only) {
  if (overwrite && file.exists(manifest_path)) {
    prior <- fromJSON(manifest_path, simplifyVector=FALSE)
    for (path in names(prior$input_sha256)) stopifnot(sha(path)==prior$input_sha256[[path]])
    for (path in names(prior$output_sha256)) {
      # Obsolete dotplot-only label/range tables are no longer produced.
      if (path %in% c("output/tables/editorial_search_pooled_labels.csv",
                      "output/tables/editorial_search_pooled_ranges.csv")) next
      stopifnot(sha(path)==prior$output_sha256[[path]])
    }
  }
  if (!overwrite) stopifnot(!any(file.exists(c(files,csv,manifest_path))))
  for (folder in unique(dirname(c(files,csv,manifest_path))))
    dir.create(folder,recursive=TRUE,showWarnings=FALSE)
}
for (variant in variants) {
  titled <- variant=="con_titulos"
  p <- make_plot(titled)
  if (!check_only) {
    dest <- function(format) file.path(out_dir,variant,format,paste0(stem,".",format))
    height <- if(titled) 8.4 else 7.2
    ggsave(dest("png"),p,width=17,height=height,units="in",dpi=300,device=ragg::agg_png,bg="white")
    ggsave(dest("pdf"),p,width=17,height=height,units="in",device=grDevices::pdf,useDingbats=FALSE,bg="white")
    ggsave(dest("svg"),p,width=17,height=height,units="in",device=svglite::svglite,bg="white")
  }
}
if (check_only) {
  m <- fromJSON(manifest_path,simplifyVector=FALSE)
  stopifnot(m$script_sha256==sha(script))
  for(path in names(m$input_sha256)) stopifnot(sha(path)==m$input_sha256[[path]])
  for(path in names(m$output_sha256)) stopifnot(sha(path)==m$output_sha256[[path]])
  expected <- counts
  expected$search_group <- as.character(expected$search_group)
  stopifnot(isTRUE(all.equal(read.csv(csv),expected,check.attributes=FALSE,tolerance=1e-12)))
  cat("POOLED_SEARCH_CHECK_OK: 900 sin búsqueda y 2700 con búsqueda en internet; 16 barras e IC95 de binom.\n")
} else {
  write.csv(counts,csv,row.names=FALSE,fileEncoding="UTF-8")
  outputs <- c(files,csv)
  m <- list(status="VERIFIED_OFFLINE_POOLED_SEARCH",script=script,script_sha256=sha(script),
    source=source_csv,source_sha256=expected_source_hash,
    input_sha256=as.list(setNames(vapply(input_files,sha,character(1)),input_files)),
    output_sha256=as.list(setNames(vapply(outputs,sha,character(1)),outputs)),
    filters=list(scale="BINARY2",arm="CONTROL"),selected_cases=3600,
    groups=list(NO_WEB=list(n=900,locations="HIST_NO_WEB"),
      WEB_AUTO=list(n=2700,locations=c("HIST_DEFAULT","GB","AR"),n_per_location=900)),
    denominators=list(NO_WEB=list(name=450,origin_or_language=300),
      WEB_AUTO=list(name=1350,origin_or_language=900)),
    aggregation="ARG count divided by all cases in each group and signal level, pooling all other conditions. Three web locations have equal sample size.",
    search_group_definition="Assigned tool availability (search_regime), not a filter on realized search_used.",
    comparison="Descriptive pooling of historical and geographic cohorts; not a pure causal effect of enabling search.",
    uncertainty=list(package="binom",function_name="binom::binom.confint",methods="asymptotic",
      confidence_level=.95,interpretation="Pointwise Wald intervals, not clipped; no simultaneous or block-cluster adjustment."),
    visual=list(kind="faceted horizontal bars",panels=2,bars=16,shared_axis=c(0,70),
      fill_colors=as.list(fills),width_inches=17,
      heights_inches=list(con_titulos=8.4,sin_titulos=7.2),
      color_meaning="Celeste para Malvinas, Buenos Aires y voseo; gris para las demás variantes.",
      footnotes=FALSE),
    packages=as.list(setNames(vapply(packages,function(p)as.character(packageVersion(p)),character(1)),packages)),
    network_used=FALSE)
  writeLines(toJSON(m,auto_unbox=TRUE,pretty=TRUE,digits=15),manifest_path)
  cat(paste(files,collapse="\n"),"\n")
}
print(counts[,c("search_group","factor","level","denominator","argentina_n","argentina_pct","lower","upper")],row.names=FALSE)
