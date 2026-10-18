#!/usr/bin/env Rscript
# Marginal percentages: assigned search availability, BINARY2 CONTROL.
# Run from the repository root. Existing outputs are verified; --check never writes.
packages <- c("ggplot2", "ggtext", "jsonlite", "digest", "ragg", "svglite")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install the required R packages: ", paste(missing, collapse = ", "))
suppressPackageStartupMessages({library(ggplot2); library(ggtext); library(jsonlite); library(digest)})
options(stringsAsFactors = FALSE, scipen = 999)
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 0L || identical(args, "--check"))
check_only <- identical(args, "--check")
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
range_rows <- list()
for (group in groups) for (f in names(definitions)) {
  q <- counts[counts$search_group == group & counts$factor == f, ]
  range_rows[[length(range_rows)+1L]] <- data.frame(search_group = group, factor = f,
    minimum_pct = min(q$argentina_pct), maximum_pct = max(q$argentina_pct),
    range_pp = diff(range(q$argentina_pct)))
}
ranges <- do.call(rbind, range_rows)
factors <- c("toponym_id", "origin_id", "language_level")
signal_labels <- c("Nombre", "Origen declarado", "Idioma")
centers <- setNames(c(7.5, 4.6, 1.7), factors)
offsets <- setNames(c(.55, -.55), groups)
counts$plot_y <- unname(centers[counts$factor] + offsets[counts$search_group])
ranges$plot_y <- unname(centers[ranges$factor] + offsets[ranges$search_group])
for (i in seq_len(nrow(ranges))) {
  x <- counts$argentina_pct[counts$search_group == ranges$search_group[i] & counts$factor == ranges$factor[i]]
  stopifnot(abs(min(x) - ranges$minimum_pct[i]) < 1e-10,
            abs(max(x) - ranges$maximum_pct[i]) < 1e-10)
}
# Se agrupan sólo textos, nunca puntos ni porcentajes. Cada nivel sigue visible.
label_rows <- list()
for (loc in groups) for (f in factors) {
  z <- counts[counts$search_group == loc & counts$factor == f, ]
  z <- z[order(z$argentina_pct), ]
  z$cluster <- cumsum(c(TRUE, diff(z$argentina_pct) >= 10))
  for (cl in unique(z$cluster)) {
    q <- z[z$cluster == cl, ]
    label_rows[[length(label_rows) + 1L]] <- data.frame(search_group = loc, factor = f,
      x = mean(range(q$argentina_pct)), y = q$plot_y[1] + .16,
      label = paste0(q$label, " · <b>", q$rounded_pct, "%</b>", collapse = "<br>"),
      n_points = nrow(q), levels = paste(q$level, collapse = "|"))
  }
}
labels <- do.call(rbind, label_rows)
stopifnot(sum(labels$n_points) == 16L)
counts$search_group <- factor(counts$search_group, levels = groups)
ranges$search_group <- factor(ranges$search_group, levels = groups)
fill_colors <- c("FALSE" = "#969DA3", "TRUE" = "#6DBBDE")
point_outline <- "#606C76"
line_color <- "#BFC5CA"
shapes <- c(NO_WEB = 21, WEB_AUTO = 22)
line_types <- c(NO_WEB = "solid", WEB_AUTO = "solid")
legend_labels <- c("Sin búsqueda", "Con búsqueda")
make_plot <- function(titled = TRUE) {
  p <- ggplot() +
    geom_hline(yintercept = c(6.05, 3.15), colour = "#E8EAEC", linewidth = .4) +
    geom_segment(data = ranges, aes(x = minimum_pct, xend = maximum_pct,
      y = plot_y, yend = plot_y, linetype = search_group), colour = line_color, linewidth = 1.1, lineend = "round") +
    geom_point(data = counts, aes(x = argentina_pct, y = plot_y, fill = argentine_variant, shape = search_group),
      colour = point_outline, size = 3.8, stroke = .65) +
    ggtext::geom_richtext(data = labels, aes(x = x, y = y, label = label),
      colour = "black", fill = NA, label.color = NA, size = 5.1, vjust = 0, hjust = .5,
      label.padding = grid::unit(rep(0, 4), "pt"), lineheight = 1.1, family = "sans") +
    scale_fill_manual(values = fill_colors, guide = "none") +
    scale_linetype_manual(values = line_types, breaks = groups, labels = legend_labels, name = NULL) +
    scale_shape_manual(values = shapes, breaks = groups, labels = legend_labels, name = NULL) +
    scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, 10),
      labels = function(x) paste0(x, "%"), expand = expansion(mult = 0)) +
    scale_y_continuous(limits = c(.5, 8.8), breaks = unname(centers), labels = signal_labels,
      expand = expansion(mult = 0)) +
    labs(title = "Elección de Argentina, con y sin búsqueda",
      subtitle = "Elección forzada · Sin mitigación · 900 respuestas sin búsqueda y 2.700 con búsqueda\nCon búsqueda: EE. UU. (por defecto), Reino Unido y Argentina, agrupados",
      x = "Respuestas que eligen Argentina", y = NULL, caption = NULL) +
    guides(linetype = guide_legend(nrow = 1, byrow = TRUE),
      shape = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(fill = "#6DBBDE"))) +
    theme_minimal(base_size = 17, base_family = "sans") +
    theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#EBEEF0", linewidth = .35),
      axis.text.x = element_text(size = 18, colour = "black"),
      axis.text.y = element_text(size = 21, face = "bold", colour = "black", margin = margin(r = 18)),
      axis.title.x = element_text(size = 19, colour = "black", margin = margin(t = 14)),
      axis.line.x = element_line(colour = "#AFB8BF", linewidth = .4),
      legend.position = "top", legend.justification = "left",
      legend.text = element_text(size = 19, colour = "black", lineheight = 1.1),
      legend.key.width = grid::unit(31, "pt"), legend.spacing.x = grid::unit(18, "pt"),
      legend.margin = margin(t = 6, b = 16),
      plot.title = element_text(size = 27, face = "bold", colour = "black", margin = margin(b = 10)),
      plot.subtitle = element_text(size = 16, colour = "black", lineheight = 1.2, margin = margin(b = 6)),
      plot.title.position = "plot", plot.margin = margin(22, 32, 20, 20),
      plot.background = element_rect(fill = "white", colour = NA))
  if (!titled) p <- p + labs(title = NULL, subtitle = NULL)
  built <- ggplot_build(p)
  stopifnot(nrow(built$layout$layout) == 1L, nrow(built$data[[2]]) == 6L,
    nrow(built$data[[3]]) == 16L,
    max(abs(built$data[[3]]$x - counts$argentina_pct)) < 1e-12,
    max(abs(built$data[[2]]$x - ranges$minimum_pct)) < 1e-12,
    max(abs(built$data[[2]]$xend - ranges$maximum_pct)) < 1e-12,
    all(built$data[[2]]$y == built$data[[2]]$yend),
    all(built$data[[3]]$colour == point_outline), length(unique(built$data[[3]]$shape)) == 2L,
    all(built$data[[3]]$fill == unname(fill_colors[as.character(counts$argentine_variant)])),
    all(built$data[[2]]$colour == line_color), all(built$data[[2]]$linetype == "solid"),
    identical(built$layout$panel_params[[1]]$x.range, c(0, 80)),
    length(built$layout$panel_params[[1]]$y$breaks) == 3L, is.null(p$labels$caption))
  p
}

variants <- c("con_titulos", "sin_titulos")
formats <- c("png", "pdf", "svg")
files <- unlist(lapply(variants, function(v) file.path(out_dir, v, formats, paste0(stem, ".", formats))))
csv_files <- setNames(file.path(table_dir, c("editorial_search_pooled_counts.csv",
  "editorial_search_pooled_ranges.csv", "editorial_search_pooled_labels.csv")),
  c("counts", "ranges", "labels"))
manifest_path <- file.path(table_dir, "search_pooled_manifest.json")
check_only <- check_only || file.exists(manifest_path)
if (check_only) stopifnot(file.exists(manifest_path))
if (!check_only) {
  stopifnot(!any(file.exists(c(files, csv_files, manifest_path))))
  for (folder in unique(dirname(c(files, csv_files, manifest_path))))
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
}
for (variant in variants) {
  titled <- variant == "con_titulos"
  p <- make_plot(titled)
  if (!check_only) {
    dest <- function(format) file.path(out_dir, variant, format, paste0(stem, ".", format))
    height <- if (titled) 9.6 else 8.6
    ggsave(dest("png"), p, width = 16, height = height, units = "in", dpi = 300, device = ragg::agg_png, bg = "white")
    ggsave(dest("pdf"), p, width = 16, height = height, units = "in", device = grDevices::pdf, useDingbats = FALSE, bg = "white")
    ggsave(dest("svg"), p, width = 16, height = height, units = "in", device = svglite::svglite, bg = "white")
  }
}
if (check_only) {
  m <- fromJSON(manifest_path, simplifyVector = FALSE)
  stopifnot(m$script_sha256 == sha(script))
  for (path in names(m$input_sha256)) stopifnot(sha(path) == m$input_sha256[[path]])
  for (path in names(m$output_sha256)) stopifnot(sha(path) == m$output_sha256[[path]])
  for (name in names(csv_files)) {
    expected <- as.data.frame(lapply(get(name), function(x) if (is.factor(x)) as.character(x) else x))
    stopifnot(isTRUE(all.equal(read.csv(csv_files[[name]]), expected, check.attributes = FALSE, tolerance = 1e-12)))
  }
  cat("POOLED_SEARCH_CHECK_OK: 900 sin búsqueda, 2700 con búsqueda; 16 porcentajes y seis líneas.\n")
} else {
  for (name in names(csv_files)) write.csv(get(name), csv_files[[name]], row.names = FALSE, fileEncoding = "UTF-8")
  m <- list(status = "VERIFIED_OFFLINE_POOLED_SEARCH", script = script, script_sha256 = sha(script),
    source = source_csv, source_sha256 = expected_source_hash,
    input_sha256 = as.list(setNames(vapply(input_files, sha, character(1)), input_files)),
    output_sha256 = as.list(setNames(vapply(c(files, csv_files), sha, character(1)), c(files, csv_files))),
    filters = list(scale = "BINARY2", arm = "CONTROL"), selected_cases = 3600,
    groups = list(NO_WEB = list(n = 900, locations = "HIST_NO_WEB"),
      WEB_AUTO = list(n = 2700, locations = c("HIST_DEFAULT", "GB", "AR"), n_per_location = 900)),
    denominators = list(NO_WEB = list(name = 450, origin_or_language = 300),
      WEB_AUTO = list(name = 1350, origin_or_language = 900)),
    aggregation = "ARG count divided by all cases in each group and signal level, pooling all other conditions. Three web locations have equal sample size.",
    search_group_definition = "Assigned tool availability (search_regime), not a filter on realized search_used.",
    comparison = "Descriptive pooling of historical and geographic cohorts; not a pure causal effect of enabling search.",
    visual = list(points = 16, lines = 6, shared_axis = c(0, 80), fill_colors = as.list(fill_colors),
      shapes = as.list(shapes), width_inches = 16, heights_inches = list(con_titulos = 9.6, sin_titulos = 8.6),
      line_meaning = "Minimum to maximum marginal percentage, not confidence intervals.",
      color_meaning = "Celeste para Malvinas, Buenos Aires y voseo; gris para las demás variantes.",
      labels = labels, footnotes = FALSE),
    packages = as.list(setNames(vapply(packages, function(p) as.character(packageVersion(p)), character(1)), packages)),
    network_used = FALSE)
  writeLines(toJSON(m, auto_unbox = TRUE, pretty = TRUE, digits = 15), manifest_path)
  cat(paste(files, collapse = "\n"), "\n")
}
print(counts[, c("search_group", "factor", "level", "denominator", "argentina_n", "argentina_pct")], row.names = FALSE)
