#!/usr/bin/env Rscript
# Editorial figures and table from the public, official-only data.
# Run from the package root: Rscript code/R/05_editorial_outputs.R
# Existing generated outputs are verified, not silently replaced. --overwrite
# regenerates only the output files owned by this script; --check never writes.
run_editorial_outputs <- function(data = NULL, root = ".", overwrite = FALSE, check = FALSE) {
  packages <- c("ggplot2", "ggtext", "binom", "jsonlite", "digest", "ragg", "svglite",
                "officer", "flextable", "xml2", "zip")
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install the required R packages first: ", paste(missing, collapse = ", "))
  suppressPackageStartupMessages({library(ggplot2); library(ggtext)})
  options(scipen = 999)
  source_csv <- file.path(root, "data/analysis_14400.csv")
  metadata_helper <- file.path(root, "code/R/scrub_docx.R")
  source(metadata_helper, local = TRUE)
  sha <- function(p) digest::digest(file = p, algo = "sha256", serialize = FALSE)
  d <- if (is.null(data)) read.csv(source_csv, check.names = FALSE, stringsAsFactors = FALSE) else data
  required <- c("call_id", "scale_id", "arm", "location", "language_level",
                "toponym_id", "origin_id", "choice_id", "search_regime")
  stopifnot(all(required %in% names(d)), nrow(d) == 14400L, !anyDuplicated(d$call_id),
            !anyNA(d[required]), setequal(d$scale_id, c("BINARY2", "ORDINAL5")))
  if ("derived_accepted" %in% names(d)) stopifnot(all(d$derived_accepted))
  definitions <- list(
    toponym_id = c(FALKLANDS = "Falkland", MALVINAS = "Malvinas"),
    origin_id = c(LONDON = "Londres", NONE = "No lo dice", BUENOS_AIRES = "Buenos Aires"),
    location = c(GB = "Reino Unido", HIST_NO_WEB = "Sin búsqueda", HIST_DEFAULT = "EE. UU. (sin ubicación)", AR = "Argentina"),
    language_level = c(EN = "Inglés", ES_NEUTRAL = "Español neutro", ES_AR = "Español con voseo"))
  factor_names <- c(toponym_id = "Nombre", origin_id = "Origen declarado",
                    location = "Ubicación del buscador en la API", language_level = "Idioma")
  arg_levels <- c(toponym_id = "MALVINAS", origin_id = "BUENOS_AIRES", location = "AR", language_level = "ES_AR")
  marginal_counts <- function(cases, group, group_levels, factors) {
    rows <- list()
    for (g in group_levels) for (f in factors) for (lev in names(definitions[[f]])) {
      z <- cases[cases[[group]] == g & cases[[f]] == lev, ]
      stopifnot(nrow(z) > 0, all(z$choice_id %in% c("ARG", "GBR")))
      row <- data.frame(group = g, factor = f, signal = factor_names[[f]], level = lev,
        label = definitions[[f]][[lev]], denominator = nrow(z), argentina_n = sum(z$choice_id == "ARG"),
        argentina_pct = 100 * mean(z$choice_id == "ARG"), argentine_variant = lev == arg_levels[[f]],
        stringsAsFactors = FALSE)
      names(row)[1] <- group
      rows[[length(rows) + 1L]] <- row
    }
    out <- do.call(rbind, rows)
    out$rounded_pct <- floor(out$argentina_pct + .5)
    rownames(out) <- NULL
    out
  }
  marginal_ranges <- function(counts, group, group_levels, factors) {
    rows <- list()
    for (g in group_levels) for (f in factors) {
      z <- counts[counts[[group]] == g & counts$factor == f, ]
      row <- data.frame(group = g, factor = f, signal = factor_names[[f]],
        minimum_pct = min(z$argentina_pct), maximum_pct = max(z$argentina_pct),
        range_pp = diff(range(z$argentina_pct)), stringsAsFactors = FALSE)
      names(row)[1] <- group
      rows[[length(rows) + 1L]] <- row
    }
    do.call(rbind, rows)
  }
  locations <- c("HIST_NO_WEB", "HIST_DEFAULT", "GB", "AR")
  arms <- c("CONTROL", "MITIGATION")
  binary <- d[d$scale_id == "BINARY2", ]
  control <- binary[binary$arm == "CONTROL", ]
  stopifnot(nrow(binary) == 7200L, nrow(control) == 3600L,
            all(table(binary$arm, binary$location) == 900L))
  env_factors <- c("toponym_id", "origin_id", "language_level")
  all_factors <- c("toponym_id", "origin_id", "location", "language_level")
  environment_counts <- marginal_counts(control, "location", locations, env_factors)
  environment_ranges <- marginal_ranges(environment_counts, "location", locations, env_factors)
  mitigation_counts <- marginal_counts(binary, "arm", arms, all_factors)
  mitigation_ranges <- marginal_ranges(mitigation_counts, "arm", arms, all_factors)
  stopifnot(nrow(environment_counts) == 32L, nrow(mitigation_counts) == 24L,
    all(environment_counts$denominator == ifelse(environment_counts$factor == "toponym_id", 450L, 300L)),
    all(mitigation_counts$denominator == c(toponym_id=1800L, origin_id=1200L, location=900L,
                                          language_level=1200L)[mitigation_counts$factor]))
  # Profile comparisons retain separate scales and mitigation arms.
  eligible <- d$origin_id == "NONE" & d$search_regime == "WEB_AUTO" & d$arm %in% arms
  arg_profile <- d$language_level == "ES_AR" & d$toponym_id == "MALVINAS" & d$location == "AR"
  gbr_profile <- d$language_level == "EN" & d$toponym_id == "FALKLANDS" & d$location == "GB"
  profiles_selection <- d[eligible & (arg_profile | gbr_profile), ]
  profiles_selection$profile_id <- profiles_selection$location
  stopifnot(nrow(profiles_selection) == 400L, !anyDuplicated(profiles_selection$call_id),
            all(table(profiles_selection$scale_id, profiles_selection$arm, profiles_selection$profile_id) == 50L))
  categories <- list(ORDINAL5 = c("ARG_CLEAR", "ARG_SOME", "BALANCED", "GBR_SOME", "GBR_CLEAR"),
                     BINARY2 = c("ARG", "GBR"))
  choice_labels <- c(ARG_CLEAR = "Argentina: claramente mejores", ARG_SOME = "Argentina: algo mejores",
    BALANCED = "Argumentos equilibrados", GBR_SOME = "Reino Unido: algo mejores",
    GBR_CLEAR = "Reino Unido: claramente mejores", ARG = "Argentina", GBR = "Reino Unido")
  profile_rows <- list()
  for (scale in names(categories)) for (arm in arms) for (profile in c("AR", "GB")) {
    z <- profiles_selection[profiles_selection$scale_id == scale & profiles_selection$arm == arm &
                            profiles_selection$profile_id == profile, ]
    stopifnot(nrow(z) == 50L, all(z$choice_id %in% categories[[scale]]))
    for (choice in categories[[scale]]) {
      n <- sum(z$choice_id == choice)
      ci <- binom::binom.confint(n, nrow(z), conf.level = .95, methods = "asymptotic")
      profile_rows[[length(profile_rows)+1L]] <- data.frame(scale_id = scale, arm = arm,
        profile_id = profile, choice_id = choice, choice_label = choice_labels[[choice]],
        count = n, denominator = nrow(z), percentage = 100*n/nrow(z), ci95_lower = 100*ci$lower,
        ci95_upper = 100*ci$upper, ci_method = "Normal (Wald), puntual y sin recorte")
    }
  }
  profile_counts <- do.call(rbind, profile_rows)
  stopifnot(nrow(profile_counts) == 28L,
    all(aggregate(percentage ~ scale_id + arm + profile_id, profile_counts, sum)$percentage == 100))
  # Origin-by-language table: binary, control, no web; both island names pooled.
  table_selection <- control[control$location == "HIST_NO_WEB", ]
  origins <- c("LONDON", "NONE", "BUENOS_AIRES")
  languages <- c("EN", "ES_NEUTRAL", "ES_AR")
  stopifnot(nrow(table_selection) == 900L, all(table_selection$search_regime == "NO_WEB"),
    all(table(table_selection$origin_id, table_selection$language_level) == 100L),
    all(table(table_selection$origin_id, table_selection$language_level, table_selection$toponym_id) == 50L))
  table_rows <- list()
  for (o in origins) for (l in languages) {
    z <- table_selection[table_selection$origin_id == o & table_selection$language_level == l, ]
    table_rows[[length(table_rows)+1L]] <- data.frame(origin_id=o, origin_label=definitions$origin_id[[o]],
      language_level=l, language_label=definitions$language_level[[l]], denominator=nrow(z),
      argentina_n=sum(z$choice_id=="ARG"), argentina_pct=100*mean(z$choice_id=="ARG"))
  }
  table_counts <- do.call(rbind, table_rows)
  table_matrix <- data.frame(origen_declarado=unname(definitions$origin_id[origins]),
    ingles_pct_argentina=table_counts$argentina_pct[table_counts$language_level=="EN"],
    espanol_neutro_pct_argentina=table_counts$argentina_pct[table_counts$language_level=="ES_NEUTRAL"],
    espanol_voseo_pct_argentina=table_counts$argentina_pct[table_counts$language_level=="ES_AR"])
  control_counts <- mitigation_counts[mitigation_counts$arm=="CONTROL", setdiff(names(mitigation_counts),"arm")]
  control_ranges <- mitigation_ranges[mitigation_ranges$arm=="CONTROL", setdiff(names(mitigation_ranges),"arm")]
  control_ranges <- control_ranges[order(-control_ranges$range_pp), ]; rownames(control_ranges) <- NULL
  control_ranges$rank <- seq_len(nrow(control_ranges))
  control_ranges$y <- rev(seq_len(nrow(control_ranges))) * 1.6
  control_counts$rank <- control_ranges$rank[match(control_counts$factor, control_ranges$factor)]
  control_counts$y <- control_ranges$y[match(control_counts$factor, control_ranges$factor)]
  ref <- profiles_selection[profiles_selection$scale_id=="BINARY2" & profiles_selection$arm=="CONTROL" &
                            profiles_selection$profile_id=="GB", ]
  reference <- data.frame(denominator=nrow(ref), argentina_n=sum(ref$choice_id=="ARG"),
                           argentina_pct=100*mean(ref$choice_id=="ARG"))
  environment_plot <- local({
    counts <- environment_counts; ranges <- environment_ranges
    factors <- c("toponym_id", "origin_id", "language_level")
    signal_labels <- c("Nombre", "Origen declarado", "Idioma")
    centers <- setNames(c(10.5, 6.2, 1.9), factors)
    offsets <- setNames(c(1.2, .4, -.4, -1.2), locations)
    counts$plot_y <- unname(centers[counts$factor] + offsets[counts$location])
    ranges$plot_y <- unname(centers[ranges$factor] + offsets[ranges$location])
    for (i in seq_len(nrow(ranges))) {
      x <- counts$argentina_pct[counts$location == ranges$location[i] & counts$factor == ranges$factor[i]]
      stopifnot(abs(min(x) - ranges$minimum_pct[i]) < 1e-10,
                abs(max(x) - ranges$maximum_pct[i]) < 1e-10)
    }
    # Se agrupan sólo textos, nunca puntos ni porcentajes. Cada nivel sigue visible.
    label_rows <- list()
    for (loc in locations) for (f in factors) {
      z <- counts[counts$location == loc & counts$factor == f, ]
      z <- z[order(z$argentina_pct), ]
      z$cluster <- cumsum(c(TRUE, diff(z$argentina_pct) >= 10))
      for (cl in unique(z$cluster)) {
        q <- z[z$cluster == cl, ]
        label_rows[[length(label_rows) + 1L]] <- data.frame(location = loc, factor = f,
          x = mean(range(q$argentina_pct)), y = q$plot_y[1] + .16,
          label = paste0(q$label, " · <b>", q$rounded_pct, "%</b>", collapse = "<br>"),
          n_points = nrow(q), levels = paste(q$level, collapse = "|"))
      }
    }
    labels <- do.call(rbind, label_rows)
    stopifnot(sum(labels$n_points) == 32L)
    counts$location <- factor(counts$location, levels = locations)
    ranges$location <- factor(ranges$location, levels = locations)
    fill_colors <- c("FALSE" = "#969DA3", "TRUE" = "#6DBBDE")
    point_outline <- "#606C76"
    line_color <- "#BFC5CA"
    shapes <- c(HIST_NO_WEB = 21, HIST_DEFAULT = 24, GB = 23, AR = 22)
    line_types <- c(HIST_NO_WEB = "solid", HIST_DEFAULT = "solid", GB = "solid", AR = "solid")
    legend_labels <- c("Sin búsqueda", "Buscador en EE. UU.\n(por defecto)", "Buscador en el\nReino Unido", "Buscador en\nArgentina")
    make_plot <- function(titled = TRUE) {
      p <- ggplot() +
        geom_hline(yintercept = c(8.35, 4.05), colour = "#E8EAEC", linewidth = .4) +
        geom_segment(data = ranges, aes(x = minimum_pct, xend = maximum_pct,
          y = plot_y, yend = plot_y, linetype = location), colour = line_color, linewidth = 1.1, lineend = "round") +
        geom_point(data = counts, aes(x = argentina_pct, y = plot_y, fill = argentine_variant, shape = location),
          colour = point_outline, size = 3.8, stroke = .65) +
        ggtext::geom_richtext(data = labels, aes(x = x, y = y, label = label),
          colour = "black", fill = NA, label.color = NA, size = 5.1, vjust = 0, hjust = .5,
          label.padding = grid::unit(rep(0, 4), "pt"), lineheight = 1.1, family = "sans") +
        scale_fill_manual(values = fill_colors, guide = "none") +
        scale_linetype_manual(values = line_types, breaks = locations, labels = legend_labels, name = NULL) +
        scale_shape_manual(values = shapes, breaks = locations, labels = legend_labels, name = NULL) +
        scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, 10),
          labels = function(x) paste0(x, "%"), expand = expansion(mult = 0)) +
        scale_y_continuous(limits = c(.2, 12.45), breaks = unname(centers), labels = signal_labels,
          expand = expansion(mult = 0)) +
        labs(title = "Las diferencias aparecen con y sin búsqueda en internet",
          subtitle = "Elección forzada · Sin mitigación · 900 respuestas por entorno\nCada línea conecta las variantes de una señal; las formas identifican el entorno de búsqueda",
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
      stopifnot(nrow(built$layout$layout) == 1L, nrow(built$data[[2]]) == 12L,
        nrow(built$data[[3]]) == 32L,
        max(abs(built$data[[3]]$x - counts$argentina_pct)) < 1e-12,
        max(abs(built$data[[2]]$x - ranges$minimum_pct)) < 1e-12,
        max(abs(built$data[[2]]$xend - ranges$maximum_pct)) < 1e-12,
        all(built$data[[2]]$y == built$data[[2]]$yend),
        all(built$data[[3]]$colour == point_outline), length(unique(built$data[[3]]$shape)) == 4L,
        all(built$data[[3]]$fill == unname(fill_colors[as.character(counts$argentine_variant)])),
        all(built$data[[2]]$colour == line_color), all(built$data[[2]]$linetype == "solid"),
        identical(built$layout$panel_params[[1]]$x.range, c(0, 80)),
        length(built$layout$panel_params[[1]]$y$breaks) == 3L, is.null(p$labels$caption))
      p
    }

    make_plot
  })

  mitigation_plot <- local({
    counts <- mitigation_counts; ranges <- mitigation_ranges
    factors <- c("toponym_id", "origin_id", "location", "language_level")
    signal_labels <- c("Nombre", "Origen declarado", "Ubicación del buscador\nen la API", "Idioma")
    centers <- setNames(c(10.4, 7.5, 4.6, 1.7), factors)
    offsets <- setNames(c(.55, -.55), arms)
    counts$plot_y <- unname(centers[counts$factor] + offsets[counts$arm])
    ranges$plot_y <- unname(centers[ranges$factor] + offsets[ranges$arm])
    for (i in seq_len(nrow(ranges))) {
      x <- counts$argentina_pct[counts$arm == ranges$arm[i] & counts$factor == ranges$factor[i]]
      stopifnot(abs(min(x) - ranges$minimum_pct[i]) < 1e-10,
                abs(max(x) - ranges$maximum_pct[i]) < 1e-10)
    }
    # Se agrupan sólo textos, nunca puntos ni porcentajes. Cada nivel sigue visible.
    label_rows <- list()
    for (arm in arms) for (f in factors) {
      z <- counts[counts$arm == arm & counts$factor == f, ]
      z <- z[order(z$argentina_pct), ]
      z$cluster <- cumsum(c(TRUE, diff(z$argentina_pct) >= 10))
      for (cl in unique(z$cluster)) {
        q <- z[z$cluster == cl, ]
        label_rows[[length(label_rows) + 1L]] <- data.frame(arm = arm, factor = f,
          x = mean(range(q$argentina_pct)), y = q$plot_y[1] + .16,
          label = paste0(q$label, " · <b>", q$rounded_pct, "%</b>", collapse = "<br>"),
          n_points = nrow(q), levels = paste(q$level, collapse = "|"))
      }
    }
    labels <- do.call(rbind, label_rows)
    stopifnot(sum(labels$n_points) == 24L)
    counts$arm <- factor(counts$arm, levels = arms)
    ranges$arm <- factor(ranges$arm, levels = arms)
    fill_colors <- c("FALSE" = "#969DA3", "TRUE" = "#6DBBDE")
    point_outline <- "#606C76"
    line_color <- "#BFC5CA"
    shapes <- c(CONTROL = 21, MITIGATION = 22)
    line_types <- c(CONTROL = "solid", MITIGATION = "solid")
    legend_labels <- c("Sin mitigación", "Con mitigación")
    make_plot <- function(titled = TRUE) {
      p <- ggplot() +
        geom_hline(yintercept = c(8.95, 6.35, 3.15), colour = "#E8EAEC", linewidth = .4) +
        geom_segment(data = ranges, aes(x = minimum_pct, xend = maximum_pct,
          y = plot_y, yend = plot_y, linetype = arm), colour = line_color, linewidth = 1.1, lineend = "round") +
        geom_point(data = counts, aes(x = argentina_pct, y = plot_y, fill = argentine_variant, shape = arm),
          colour = point_outline, size = 3.8, stroke = .65) +
        ggtext::geom_richtext(data = labels, aes(x = x, y = y, label = label),
          colour = "black", fill = NA, label.color = NA, size = 5.1, vjust = 0, hjust = .5,
          label.padding = grid::unit(rep(0, 4), "pt"), lineheight = 1.1, family = "sans") +
        scale_fill_manual(values = fill_colors, guide = "none") +
        scale_linetype_manual(values = line_types, breaks = arms, labels = legend_labels, name = NULL) +
        scale_shape_manual(values = shapes, breaks = arms, labels = legend_labels, name = NULL) +
        scale_x_continuous(limits = c(0, 65), breaks = seq(0, 60, 10),
          labels = function(x) paste0(x, "%"), expand = expansion(mult = 0)) +
        scale_y_continuous(limits = c(.5, 11.7), breaks = unname(centers), labels = signal_labels,
          expand = expansion(mult = 0)) +
        labs(title = "Elección de Argentina, con y sin mitigación",
          subtitle = "Elección forzada · 3.600 respuestas por brazo\nPorcentajes agrupando las demás condiciones de la consulta",
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
      stopifnot(nrow(built$layout$layout) == 1L, nrow(built$data[[2]]) == 8L,
        nrow(built$data[[3]]) == 24L,
        max(abs(built$data[[3]]$x - counts$argentina_pct)) < 1e-12,
        max(abs(built$data[[2]]$x - ranges$minimum_pct)) < 1e-12,
        max(abs(built$data[[2]]$xend - ranges$maximum_pct)) < 1e-12,
        all(built$data[[2]]$y == built$data[[2]]$yend),
        all(built$data[[3]]$colour == point_outline), length(unique(built$data[[3]]$shape)) == 2L,
        all(built$data[[3]]$fill == unname(fill_colors[as.character(counts$argentine_variant)])),
        all(built$data[[2]]$colour == line_color), all(built$data[[2]]$linetype == "solid"),
        identical(built$layout$panel_params[[1]]$x.range, c(0, 65)),
        length(built$layout$panel_params[[1]]$y$breaks) == 4L, is.null(p$labels$caption))
      p
    }

    make_plot
  })

  profile_plot <- local({
    counts <- profile_counts[profile_counts$arm=="CONTROL", ]
    categories <- list(ORDINAL5 = c("ARG_CLEAR", "ARG_SOME", "BALANCED", "GBR_SOME", "GBR_CLEAR"),
                       BINARY2 = c("ARG", "GBR"))
    labels <- c(ARG_CLEAR = "Argentina:\nclaramente mejores", ARG_SOME = "Argentina:\nalgo mejores",
                BALANCED = "Argumentos\nequilibrados", GBR_SOME = "Reino Unido:\nalgo mejores",
                GBR_CLEAR = "Reino Unido:\nclaramente mejores", ARG = "Argentina", GBR = "Reino Unido")
    profiles <- c(AR = "Perfil argentino", GB = "Perfil británico")
    profile_description <- c(AR = "Voseo + «Malvinas» + ubicación Argentina",
                             GB = "Inglés + «Falkland» + ubicación Reino Unido")
    profile_filename <- c(AR = "argentino", GB = "britanico")
    scale_filename <- c(ORDINAL5 = "cinco_opciones", BINARY2 = "binaria")
    scale_description <- c(ORDINAL5 = "Escala de cinco opciones", BINARY2 = "Elección forzada entre dos países")
    palette <- c(AR = "#6DBBDE", GB = "#969DA3")
    make_plot <- function(scale, with_title = TRUE) {
      z <- counts[counts$scale_id == scale, ]
      z$category <- factor(z$choice_id, levels = categories[[scale]])
      z$profile_id <- factor(z$profile_id, levels = c("AR", "GB"))
      z <- z[order(z$category, z$profile_id), ]
      stopifnot(nrow(z) == 2L * length(categories[[scale]]),
                all(tapply(z$count, z$profile_id, sum) == 50L))
      dodge <- position_dodge(width = .76)
      p <- ggplot(z, aes(category, percentage, fill = profile_id, group = profile_id)) +
        geom_hline(yintercept = 0, colour = "#78838B", linewidth = .35) +
        geom_col(position = dodge, width = .66, colour = "#24313A", linewidth = .3) +
        geom_errorbar(aes(ymin = ci95_lower, ymax = ci95_upper), position = dodge,
                      width = .15, colour = "#24313A", linewidth = .7) +
        geom_point(aes(shape = profile_id), position = dodge, size = 2.2,
                   colour = "#24313A", show.legend = FALSE) +
        geom_text(aes(y = ci95_upper + 3.2, label = sprintf("%.0f%%", percentage)),
                  position = dodge, size = 4.8, fontface = "bold", colour = "#000000") +
        scale_x_discrete(labels = labels, drop = FALSE) +
        scale_y_continuous(limits = c(-5, 110), breaks = seq(0, 100, 25),
                           labels = function(v) paste0(v, "%"), expand = expansion(mult = c(0, 0))) +
        scale_fill_manual(values = palette, labels = profiles, drop = FALSE) +
        scale_shape_manual(values = c(AR = 16, GB = 18)) +
        labs(title = if (scale == "ORDINAL5") "Comparación de perfiles: escala Likert"
                     else "Comparación de perfiles: elección binaria",
             subtitle = paste(
               "Perfil argentino: voseo + «Malvinas» + ubicación Argentina.",
               "Perfil británico: inglés + «Falkland» + ubicación Reino Unido.",
               "50 respuestas por perfil · Sin origen declarado ni mitigación", sep = "\n"),
             x = NULL, y = NULL, fill = NULL, caption = NULL) +
        theme_minimal(base_size = 14, base_family = "sans") +
        theme(text = element_text(colour = "#000000"), panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
              panel.grid.major.y = element_line(colour = "#E4E8EB", linewidth = .35),
              axis.text = element_text(colour = "#000000"),
              axis.text.x = element_text(size = 12.5, lineheight = 1.15),
              plot.title = element_text(size = 21, face = "bold", colour = "#000000", margin = margin(b = 9)),
              plot.subtitle = element_text(size = 12.5, colour = "#000000", lineheight = 1.25, margin = margin(b = 12)),
              plot.title.position = "plot",
              plot.margin = margin(20, 23, 18, 20), legend.position = "top",
              legend.text = element_text(size = 12.5, colour = "#000000"),
              plot.background = element_rect(fill = "white", colour = NA))
      if (!with_title) p <- p + labs(title = NULL, subtitle = NULL)
      stopifnot(is.null(p$labels$caption))
      if (!with_title) stopifnot(is.null(p$labels$title), is.null(p$labels$subtitle))
      built <- ggplot_build(p)
      stopifnot(nrow(built$layout$layout) == 1L, nrow(built$data[[2]]) == nrow(z),
                max(abs(built$data[[2]]$y - z$percentage)) < 1e-12,
                max(abs(built$data[[3]]$ymin - z$ci95_lower)) < 1e-12,
                max(abs(built$data[[3]]$ymax - z$ci95_upper)) < 1e-12)
      p
    }

    make_plot
  })

  profile_facets_plot <- local({
    counts <- profile_counts
    labels <- c(ARG_CLEAR="Argentina:\nclaramente\nmejores", ARG_SOME="Argentina:\nalgo mejores",
      BALANCED="Argumentos\nequilibrados", GBR_SOME="Reino Unido:\nalgo mejores",
      GBR_CLEAR="Reino Unido:\nclaramente\nmejores", ARG="Argentina", GBR="Reino Unido")
    make_plot <- function(scale, with_title = TRUE) {
      z <- counts[counts$scale_id == scale, ]
      z$category <- factor(z$choice_id, levels = categories[[scale]])
      z$profile_id <- factor(z$profile_id, levels = c("AR", "GB"))
      z$arm <- factor(z$arm, levels = c("CONTROL", "MITIGATION"))
      z <- z[order(z$arm, z$category, z$profile_id), ]
      dodge <- position_dodge(width = .76)
      p <- ggplot(z, aes(category, percentage, fill = profile_id, group = profile_id)) +
        geom_hline(yintercept = 0, colour = "#78838B", linewidth = .35) +
        geom_col(position = dodge, width = .66, colour = "#24313A", linewidth = .3) +
        geom_errorbar(aes(ymin = ci95_lower, ymax = ci95_upper), position = dodge,
                      width = .15, colour = "#24313A", linewidth = .7) +
        geom_point(aes(shape = profile_id), position = dodge, size = 2.2,
                   colour = "#24313A", show.legend = FALSE) +
        geom_text(aes(y = ci95_upper + 3.2, label = sprintf("%.0f%%", percentage)), position = dodge,
                  size = 4.8, fontface = "bold", colour = "#000000") +
        facet_wrap(vars(arm), ncol = 2, labeller = as_labeller(c(CONTROL = "Sin mitigación", MITIGATION = "Con mitigación"))) +
        scale_x_discrete(labels = labels, drop = FALSE) +
        scale_y_continuous(limits = c(-5, 110), breaks = seq(0, 100, 25),
                           labels = function(v) paste0(v, "%"), expand = expansion(mult = c(0, 0))) +
        scale_fill_manual(values = c(AR = "#6DBBDE", GB = "#969DA3"),
                          labels = c(AR = "Perfil argentino", GB = "Perfil británico"), drop = FALSE) +
        scale_shape_manual(values = c(AR = 16, GB = 18)) +
        labs(title = if (scale == "ORDINAL5") "Comparación de perfiles: escala Likert" else "Comparación de perfiles: elección binaria",
             subtitle = paste("Perfil argentino: voseo + «Malvinas» + ubicación Argentina.",
                              "Perfil británico: inglés + «Falkland» + ubicación Reino Unido.",
                              "50 respuestas por perfil en cada panel · Sin origen declarado", sep = "\n"),
             x = NULL, y = NULL, fill = NULL, caption = NULL) +
        theme_minimal(base_size = 14, base_family = "sans") +
        theme(text = element_text(colour = "#000000"), panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
              panel.grid.major.y = element_line(colour = "#E4E8EB", linewidth = .35),
              axis.text = element_text(colour = "#000000"),
              axis.text.x = element_text(size = 12.5, lineheight = 1.15),
              strip.text = element_text(size = 16, face = "bold", colour = "#000000", margin = margin(b = 15)),
              panel.spacing = grid::unit(24, "pt"),
              plot.title = element_text(size = 23, face = "bold", colour = "#000000", margin = margin(b = 9)),
              plot.subtitle = element_text(size = 13, colour = "#000000", lineheight = 1.25, margin = margin(b = 12)),
              plot.title.position = "plot", plot.margin = margin(20, 23, 18, 20),
              legend.position = "top", legend.text = element_text(size = 13, colour = "#000000"),
              plot.background = element_rect(fill = "white", colour = NA))
      if (!with_title) p <- p + labs(title = NULL, subtitle = NULL)
      stopifnot(is.null(p$labels$caption))
      if (!with_title) stopifnot(is.null(p$labels$title), is.null(p$labels$subtitle))
      built <- ggplot_build(p)
      stopifnot(nrow(built$layout$layout) == 2L, nrow(built$data[[2]]) == nrow(z),
                max(abs(built$data[[2]]$y - z$percentage)) < 1e-12,
                max(abs(built$data[[3]]$ymin - z$ci95_lower)) < 1e-12,
                max(abs(built$data[[3]]$ymax - z$ci95_upper)) < 1e-12,
                all(is.finite(built$data[[4]]$y)), all(is.finite(built$data[[5]]$y)))
      p
    }

    make_plot
  })

  control_plot <- local({
    counts <- control_counts; ranges <- control_ranges
    # Agrupar etiquetas cercanas, nunca puntos ni estimaciones. Umbral: 6 pp.
    # Mantener cada punto en su posición exacta y su porcentaje individual.
    blue_text <- "#147CA7"; neutral_text <- "#000000"
    label_rows <- list(); group_index <- 0L
    for (f in ranges$factor) {
      z <- counts[counts$factor == f, ]; z <- z[order(z$argentina_pct), ]
      z$label_group <- cumsum(c(TRUE, diff(z$argentina_pct) >= 6))
      for (group in unique(z$label_group)) {
        q <- z[z$label_group == group, ]; group_index <- group_index + 1L
        lines <- vapply(seq_len(nrow(q)), function(i) {
          separator <- if (nrow(q) == 1L) "<br>" else " · "
          paste0("<span style='color:", if (q$argentine_variant[i]) blue_text else neutral_text, "'>",
                 q$label[i], separator, "<b>", q$rounded_pct[i], "%</b></span>")
        }, character(1))
        label_rows[[group_index]] <- data.frame(factor = f, group = group_index,
          levels = paste(q$level, collapse = "|"), n_points = nrow(q),
          x = mean(range(q$argentina_pct)), y = q$y[1] + .21,
          label = paste(lines, collapse = "<br>"))
      }
    }
    labels <- do.call(rbind, label_rows)
    stopifnot(sum(labels$n_points) == 12L, nrow(labels) == 9L)
    make_plot <- function(with_title = TRUE) {
      p <- ggplot() +
        geom_segment(aes(x = reference$argentina_pct, xend = reference$argentina_pct, y = .95, yend = 7.78),
                     colour = "#BCC2C7", linewidth = .6, linetype = "dashed") +
        geom_segment(data = ranges, aes(x = minimum_pct, xend = maximum_pct, y = y, yend = y),
                     colour = "#BFC5CA", linewidth = 1.05, lineend = "round") +
        geom_point(data = counts, aes(x = argentina_pct, y = y, fill = argentine_variant),
                   shape = 21, size = 4.4, stroke = .65, colour = "#606C76", show.legend = FALSE) +
        ggtext::geom_richtext(data = labels, aes(x = x, y = y, label = label),
          fill = NA, label.color = NA, size = 4.7, vjust = 0, hjust = .5,
          label.padding = grid::unit(rep(0, 4), "pt"), lineheight = 1.15) +
        ggtext::geom_richtext(aes(x = reference$argentina_pct, y = 8.25,
          label = paste0("<b>Perfil británico: ", round(reference$argentina_pct), "%</b><br>",
                         "<span style='font-size:11pt'>Inglés · Falkland · Sin origen declarado<br>Buscador ubicado en Reino Unido</span>")),
          colour = "#000000", fill = NA, label.color = NA, size = 4.5, hjust = 0, vjust = .5,
          label.padding = grid::unit(rep(0, 4), "pt")) +
        scale_fill_manual(values = c("FALSE" = "#969DA3", "TRUE" = "#6DBBDE")) +
        scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), labels = function(x) paste0(x, "%"),
                           expand = expansion(mult = 0)) +
        scale_y_continuous(limits = c(.95, 8.65), breaks = ranges$y,
                           labels = sub("buscador en la API", "buscador\nen la API", ranges$signal, fixed = TRUE),
                           expand = expansion(mult = 0)) +
        labs(title = "Elección de Argentina según cada señal",
             subtitle = "Elección forzada · Sin mitigación · 3.600 respuestas\nPorcentajes agrupando todas las demás condiciones",
             x = "Respuestas que eligen Argentina", y = NULL, caption = NULL) +
        theme_minimal(base_size = 14, base_family = "sans") +
        theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line(colour = "#EBEEF0", linewidth = .35),
              axis.text.x = element_text(size = 13, colour = "#000000"),
              axis.text.y = element_text(size = 14, face = "bold", colour = "#000000", margin = margin(r = 18)),
              axis.title.x = element_text(size = 13, colour = "#000000", margin = margin(t = 14)),
              axis.line.x = element_line(colour = "#AFB8BF", linewidth = .4),
              plot.title = element_text(size = 23, face = "bold", colour = "#000000", margin = margin(b = 10)),
              plot.subtitle = element_text(size = 13, colour = "#000000", lineheight = 1.2, margin = margin(b = 10)),
              plot.title.position = "plot", plot.margin = margin(24, 30, 22, 20),
              plot.background = element_rect(fill = "white", colour = NA))
      if (!with_title) p <- p + labs(title = NULL, subtitle = NULL)
      built <- ggplot_build(p)
      stopifnot(nrow(built$data[[2]]) == 4L, nrow(built$data[[3]]) == 12L,
                max(abs(built$data[[3]]$x - counts$argentina_pct)) < 1e-12,
                built$data[[1]]$x == reference$argentina_pct,
                all(built$data[[2]]$xend >= built$data[[2]]$x), is.null(p$labels$caption))
      p
    }

    make_plot
  })
  out_dir <- file.path(root, "output/figures")
  table_dir <- file.path(root, "output/tables")
  csv_objects <- list(editorial_search_environment_counts=environment_counts,
    editorial_mitigation_counts=mitigation_counts, editorial_profile_counts=profile_counts,
    editorial_origin_language_counts=table_counts, editorial_origin_language_matrix=table_matrix,
    editorial_control_signals_counts=control_counts)
  csv_files <- file.path(table_dir, paste0(names(csv_objects), ".csv"))
  manifest_path <- file.path(table_dir, "editorial_outputs_manifest.json")
  if (check || (!overwrite && file.exists(manifest_path))) {
    stopifnot(file.exists(manifest_path))
    m <- jsonlite::fromJSON(manifest_path, simplifyVector=FALSE)
    stopifnot(m$source_sha256 == sha(source_csv),
      m$script_sha256 == sha(file.path(root, "code/R/05_editorial_outputs.R")),
      m$metadata_helper_sha256 == sha(metadata_helper))
    for (f in names(m$output_sha256)) stopifnot(sha(file.path(root, f)) == m$output_sha256[[f]])
    for (i in seq_along(csv_objects)) stopifnot(isTRUE(all.equal(read.csv(csv_files[i]),
      csv_objects[[i]], check.attributes=FALSE, tolerance=1e-10)))
    message("EDITORIAL_CHECK_OK: generated outputs intact; all tables reproduced from public data.")
    return(invisible(m))
  }
  for (folder in c(out_dir, table_dir)) dir.create(folder, recursive=TRUE, showWarnings=FALSE)
  owned <- character()
  save_family <- function(builder, stem, width, heights) {
    for (variant in c("con_titulos","sin_titulos")) {
      titled <- variant=="con_titulos"; p <- builder(titled)
      for (format in c("png","pdf","svg")) {
        path <- file.path(out_dir,variant,format,paste0(stem,".",format))
        if (file.exists(path) && !overwrite) stop("Output exists; use --check or --overwrite: ",path)
        dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
        if(format=="png") ggsave(path,p,width=width,height=heights[[variant]],units="in",
          dpi=300,device=ragg::agg_png,bg="white")
        if(format=="pdf") ggsave(path,p,width=width,height=heights[[variant]],units="in",
          device=grDevices::pdf,useDingbats=FALSE,bg="white")
        if(format=="svg") ggsave(path,p,width=width,height=heights[[variant]],units="in",
          device=svglite::svglite,bg="white")
        owned <<- c(owned,path)
      }
    }
  }
  save_family(environment_plot,"search_environments",16,c(con_titulos=13.6,sin_titulos=12.6))
  save_family(mitigation_plot,"mitigation_signals",16,c(con_titulos=12.2,sin_titulos=11.2))
  save_family(control_plot,"control_signals",14,c(con_titulos=9,sin_titulos=7.9))
  for (scale in c("ORDINAL5","BINARY2")) {
    suffix <- if(scale=="ORDINAL5") "five_options" else "binary"
    save_family(function(titled) profile_plot(scale,titled),paste0("profiles_",suffix),11.5,
      c(con_titulos=7.2,sin_titulos=5.7))
    save_family(function(titled) profile_facets_plot(scale,titled),paste0("profiles_mitigation_",suffix),16,
      c(con_titulos=7.6,sin_titulos=6.1))
  }
  for (i in seq_along(csv_objects)) {
    if (file.exists(csv_files[i]) && !overwrite) stop("Output exists: ",csv_files[i])
    write.csv(csv_objects[[i]],csv_files[i],row.names=FALSE,fileEncoding="UTF-8")
  }
  owned <- c(owned,csv_files)
  # Editable Word table. Values are calculated above, never copied from a report.
  word_path <- file.path(table_dir,"origin_language_table.docx")
  if(file.exists(word_path) && !overwrite) stop("Output exists: ",word_path)
  display <- table_matrix
  names(display) <- c("origin","english","neutral","voseo")
  for(j in 2:4) display[[j]] <- sprintf("%.0f%%",display[[j]])
  ft <- flextable::flextable(display)
  ft <- flextable::set_header_labels(ft,origin="Origen declarado",english="Inglés",
    neutral="Español neutro",voseo="Español con voseo")
  ft <- flextable::font(ft,fontname="Arial",part="all")
  ft <- flextable::fontsize(ft,size=11,part="all")
  ft <- flextable::color(ft,color="#000000",part="all")
  ft <- flextable::bg(ft,bg="#ECF7FB",part="header")
  ft <- flextable::bold(ft,bold=TRUE,part="header")
  ft <- flextable::align(ft,align="center",part="all")
  ft <- flextable::align(ft,j=1,align="left",part="all")
  ft <- flextable::valign(ft,valign="center",part="all")
  ft <- flextable::padding(ft,padding.top=5.5,padding.bottom=5.5,padding.left=7,
    padding.right=7,part="all")
  ft <- flextable::line_spacing(ft,space=1.05,part="all")
  ft <- flextable::width(ft,width=c(1.9,.9,1.65,1.95))
  ft <- flextable::border_remove(ft)
  border <- officer::fp_border(color="#D9D9D9",width=.5)
  ft <- flextable::border_outer(ft,border=border,part="all")
  ft <- flextable::border_inner_h(ft,border=border,part="all")
  ft <- flextable::border_inner_v(ft,border=border,part="all")
  ft <- flextable::set_table_properties(ft,layout="fixed",align="left",
    opts_word=list(split=FALSE,repeat_headers=TRUE))
  doc <- officer::read_docx()
  doc <- officer::docx_set_paragraph_style(doc,style_id="Title",style_name="Title",
    base_on="Normal",fp_p=officer::fp_par(padding.bottom=8,keep_with_next=TRUE),
    fp_t=officer::fp_text(font.family="Arial",font.size=14,bold=TRUE,color="#000000"))
  doc <- officer::body_set_default_section(doc,officer::prop_section(
    page_size=officer::page_size(width=8.5,height=11),
    page_margins=officer::page_mar(top=.85,bottom=.85,left=1,right=1)))
  doc <- officer::body_add_fpar(doc,officer::fpar(
    officer::ftext("Elección de Argentina por idioma y origen",
      officer::fp_text(font.family="Arial",font.size=14,bold=TRUE,color="#000000")),
    fp_p=officer::fp_par(padding.bottom=8,keep_with_next=TRUE)),style="Title")
  doc <- officer::body_add_fpar(doc,officer::fpar(
    officer::ftext(paste0("Porcentaje que elige Argentina. Sin búsqueda ni mitigación.\n",
      "100 respuestas por casillero, agrupando ambos nombres de las islas."),
      officer::fp_text(font.family="Arial",font.size=10,color="#000000")),
    fp_p=officer::fp_par(line_spacing=1.15,padding.bottom=12,keep_with_next=TRUE)))
  doc <- flextable::body_add_flextable(doc,ft)
  doc <- officer::set_doc_properties(doc,title="Elección de Argentina por idioma y origen",
    creator="",subject="Elección binaria sin búsqueda ni mitigación")
  doc$doc_properties["lastModifiedBy", "value"] <- ""
  print(doc,target=word_path)
  scrub_public_docx(word_path)
  owned <- c(owned,word_path)
  # root may be "." or an absolute directory; stored paths remain package-relative.
  rel <- substring(owned,nchar(root)+2L)
  m <- list(status="EDITORIAL_REPRODUCED",source="data/analysis_14400.csv",
    source_sha256=sha(source_csv),script="code/R/05_editorial_outputs.R",
    script_sha256=sha(file.path(root,"code/R/05_editorial_outputs.R")),
    metadata_helper="code/R/scrub_docx.R",metadata_helper_sha256=sha(metadata_helper),
    official_cases=14400L,pilot_cases=0L,
    scopes=list(search_environments=3600L,mitigation_signals=7200L,control_signals=3600L,
      profiles_control_per_scale=100L,profiles_mitigation_per_scale=200L,origin_language_table=900L),
    ci=list(function_used="binom::binom.confint(conf.level=0.95,methods='asymptotic')",
      meaning="Pointwise normal (Wald), not clipped to 0–100; complementary bars share observations"),
    dotplot_lines="Minimum to maximum marginal percentage within signal/group, not confidence intervals",
    output_sha256=as.list(setNames(vapply(owned,sha,character(1)),rel)),
    packages=as.list(setNames(vapply(packages,function(p)as.character(utils::packageVersion(p)),character(1)),packages)),
    network_used=FALSE)
  jsonlite::write_json(m,manifest_path,pretty=TRUE,auto_unbox=TRUE,digits=15)
  message("EDITORIAL_OK: 42 figure exports, 6 CSV tables and one editable Word table.")
  invisible(m)
}

if (sys.nframe()==0L) {
  cli <- commandArgs(trailingOnly=TRUE)
  stopifnot(all(cli %in% c("--overwrite","--check")))
  run_editorial_outputs(overwrite="--overwrite" %in% cli,check="--check" %in% cli)
}
