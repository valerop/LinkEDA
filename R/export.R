.rls_export_format_number <- function(x, digits = 3L) {
  if (!is.finite(x)) return("\u2014")
  sprintf(paste0("%.", digits, "f"), x)
}

.rls_export_format_percent <- function(x, digits = 1L) {
  if (!is.finite(x)) return("\u2014")
  sprintf(paste0("%.", digits, "f%%"), 100 * x)
}

.rls_export_format_p <- function(p) {
  if (!is.finite(p)) return("\u2014")
  if (p < .001) return("< .001")
  sub("^0", "", sprintf("%.3f", p))
}

.rls_export_stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p < .001) return("***")
  if (p < .01) return("**")
  if (p < .05) return("*")
  ""
}

.rls_export_coefficient_cell <- function(estimate, p) {
  if (!is.finite(estimate)) return("\u2014")
  paste0(.rls_export_format_number(estimate, 3L), .rls_export_stars(p))
}

.rls_export_fit_rows_from_summary <- function(summary) {
  rows <- data.frame(
    label = c("N", "R\u00b2", "Adjusted R\u00b2", "s", "df residual", "F", "p", "AIC", "BIC"),
    value = c(
      as.character(summary$n_used %||% NA_integer_),
      .rls_export_format_percent(summary$r_squared %||% NA_real_),
      .rls_export_format_percent(summary$adj_r_squared %||% NA_real_),
      .rls_export_format_number(summary$residual_se %||% NA_real_, 3L),
      as.character(summary$df_residual %||% NA_integer_),
      .rls_export_format_number(summary$global_f %||% NA_real_, 3L),
      .rls_export_format_p(summary$global_p %||% NA_real_),
      .rls_export_format_number(summary$aic %||% NA_real_, 1L),
      .rls_export_format_number(summary$bic %||% NA_real_, 1L)
    ),
    stringsAsFactors = FALSE
  )
  if (!is.null(summary$comparison_method)) {
    rows <- rbind(
      rows,
      data.frame(
        label = c("Test vs previous", "F vs previous", "df1 vs previous", "df2 vs previous", "p vs previous"),
        value = c(
          if (identical(summary$comparison_status %||% "", "not_applicable")) "\u2014" else summary$comparison_method,
          .rls_export_format_number(summary$comparison_f %||% NA_real_, 3L),
          .rls_export_format_number(summary$comparison_df1 %||% NA_real_, 0L),
          .rls_export_format_number(summary$comparison_df2 %||% NA_real_, 1L),
          .rls_export_format_p(summary$comparison_p %||% NA_real_)
        ),
        stringsAsFactors = FALSE
      )
    )
  }
  rows
}

.rls_regression_table_model <- function(label, coefficients, summary, coefficient_rows = NULL,
                                        response = NULL, fit_rows = NULL) {
  rows <- coefficient_rows
  if (is.null(rows) || !nrow(rows)) {
    rows <- data.frame(
      term = coefficients$term,
      display_label = coefficients$term,
      row_type = "coefficient",
      source_term = coefficients$term,
      estimate = coefficients$estimate,
      p_value = coefficients$p_value,
      stringsAsFactors = FALSE
    )
  }
  values <- stats::setNames(rep("\u2014", nrow(rows)), rows$term)
  for (i in seq_len(nrow(rows))) {
    if (rows$row_type[[i]] %in% c("factor_parent", "term_parent", "reference")) {
      values[[rows$term[[i]]]] <- "\u2014"
    } else {
      values[[rows$term[[i]]]] <- .rls_export_coefficient_cell(rows$estimate[[i]], rows$p_value[[i]])
    }
  }
  list(
    label = label,
    response = response,
    coefficients = values,
    fit_rows = fit_rows %||% .rls_export_fit_rows_from_summary(summary)
  )
}

.rls_regression_table <- function(response, models, terms, title = "Regression Table") {
  if (is.data.frame(terms)) {
    term_rows <- terms[!duplicated(terms$term), , drop = FALSE]
  } else {
    term_rows <- data.frame(term = unique(terms), display_label = unique(terms), stringsAsFactors = FALSE)
  }
  model_labels <- vapply(models, `[[`, character(1L), "label")
  model_responses <- vapply(models, function(model) as.character(model$response %||% response), character(1L))
  responses_differ <- length(unique(model_responses)) > 1L
  coef_table <- data.frame(`Variable / level` = term_rows$display_label, stringsAsFactors = FALSE, check.names = FALSE)
  names(coef_table)[[1L]] <- "Variable / level"
  if (responses_differ) {
    coef_table <- rbind(
      data.frame(`Variable / level` = "Response", stringsAsFactors = FALSE, check.names = FALSE),
      coef_table
    )
    names(coef_table)[[1L]] <- "Variable / level"
  }
  fit_labels <- models[[1L]]$fit_rows$label
  fit_table <- data.frame(Statistic = fit_labels, stringsAsFactors = FALSE, check.names = FALSE)
  for (model in models) {
    values <- unname(vapply(term_rows$term, function(term) {
      if (term %in% names(model$coefficients)) model$coefficients[[term]] else "\u2014"
    }, character(1L)))
    if (responses_differ) {
      values <- c(model$response %||% response, values)
    }
    coef_table[[model$label]] <- values
    fit_values <- stats::setNames(model$fit_rows$value, model$fit_rows$label)
    fit_table[[model$label]] <- unname(vapply(fit_labels, function(label) fit_values[[label]] %||% "\u2014", character(1L)))
  }
  structure(
    list(
      title = title,
      response = response,
      responses = model_responses,
      responses_differ = responses_differ,
      model_labels = model_labels,
      terms = term_rows,
      coefficients = coef_table,
      fit = fit_table,
      note = "* p < .05; ** p < .01; *** p < .001"
    ),
    class = "rlispstat_regression_table"
  )
}

.rls_regression_table_from_glm <- function(model) {
  record <- .rls_glm_model_record(model)
  if (is.null(record$fit)) {
    record <- .rls_glm_model_record(ls_glm_fit(model))
  }
  table_model <- .rls_regression_table_model(
    "Model", record$coefficients, record$summary,
    coefficient_rows = record$coefficient_rows,
    response = record$dependent
  )
  table <- .rls_regression_table(record$dependent, list(table_model), record$coefficient_rows, "General Linear Model")
  if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation")) {
    table$title <- "General Linear Model - Multiple Imputation"
    table$note <- paste(
      table$note,
      sprintf("Multiple imputation: m = %d; coefficients pooled with Rubin's rules. Global model test uses D1 pooled Wald; standardized beta and AIC/BIC are not pooled.",
              record$imputation_count %||% length(record$fits_by_imputation %||% list())),
      sep = "\n"
    )
  }
  table
}

.rls_regression_table_from_comparison <- function(comparison) {
  record <- .rls_regcmp_record(comparison)
  models <- lapply(record$models, function(model) {
    .rls_regression_table_model(
      model$label, model$coefficients, model$summary,
      coefficient_rows = model$coefficient_rows,
      response = model$response %||% record$response
    )
  })
  table <- .rls_regression_table(record$response, models, .rls_regcmp_display_rows(record), "Regression Model Comparison")
  if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation")) {
    table$title <- "Regression Model Comparison - Multiple Imputation"
    table$note <- paste(
      table$note,
      sprintf("Multiple imputation: m = %d; coefficients pooled with Rubin's rules. Sequential model tests use a pooled Wald/D1-style test; ordinary F statistics and p-values are not averaged.",
              record$imputation_count %||% length(record$models[[1L]]$fits_by_imputation %||% list())),
      sep = "\n"
    )
  }
  table
}

.rls_regression_table_from_generalized_glm <- function(model) {
  record <- .rls_generalized_glm_record(model)
  if (is.null(record$fit)) {
    record <- .rls_generalized_glm_record(ls_generalized_linear_model_fit(model))
  }
  table_model <- .rls_regression_table_model(
    "Model",
    record$coefficients,
    record$summary,
    coefficient_rows = record$coefficient_rows,
    response = record$response,
    fit_rows = .rls_generalized_glm_fit_rows(record$summary)
  )
  table <- .rls_regression_table(
    record$response,
    list(table_model),
    record$coefficient_rows,
    "Generalized Linear Model"
  )
  table$family <- record$family
  table$link <- record$link
  if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation")) {
    table$title <- "Generalized Linear Model - Multiple Imputation"
    table$note <- paste(
      table$note,
      sprintf("Multiple imputation: m = %d; link-scale coefficients pooled with Rubin's rules. Deviance/AIC/BIC/logLik are not pooled.",
              record$imputation_count %||% length(record$fits_by_imputation %||% list())),
      sep = "\n"
    )
  }
  table
}

.rls_render_table_text <- function(table) {
  header <- c(table$title, paste("Response:", if (isTRUE(table$responses_differ)) "see table" else table$response))
  if (!is.null(table$family)) {
    header <- c(header, paste("Family:", table$family), paste("Link:", table$link))
  }
  blocks <- list(
    c(header, ""),
    .rls_render_data_frame_text(table$coefficients),
    "",
    .rls_render_data_frame_text(table$fit),
    "",
    table$note
  )
  paste(unlist(blocks), collapse = "\n")
}

.rls_render_data_frame_text <- function(data) {
  chars <- lapply(data, as.character)
  widths <- pmax(nchar(names(chars), type = "width"), vapply(chars, function(x) max(nchar(x, type = "width")), integer(1L)))
  fmt_cell <- function(value, width, right = TRUE) {
    value <- as.character(value)
    pad <- max(0L, width - nchar(value, type = "width"))
    if (right) paste0(strrep(" ", pad), value) else paste0(value, strrep(" ", pad))
  }
  header <- paste(vapply(seq_along(chars), function(i) fmt_cell(names(chars)[[i]], widths[[i]], i > 1L), character(1L)), collapse = "  ")
  rows <- vapply(seq_len(nrow(data)), function(r) {
    paste(vapply(seq_along(chars), function(i) fmt_cell(chars[[i]][[r]], widths[[i]], i > 1L), character(1L)), collapse = "  ")
  }, character(1L))
  c(header, rows)
}

.rls_markdown_escape <- function(x) {
  gsub("\\|", "\\\\|", as.character(x))
}

.rls_render_data_frame_markdown <- function(data) {
  header <- paste0("| ", paste(.rls_markdown_escape(names(data)), collapse = " | "), " |")
  align <- paste0("| ", paste(c(":---", rep("---:", ncol(data) - 1L)), collapse = " | "), " |")
  rows <- vapply(seq_len(nrow(data)), function(i) {
    paste0("| ", paste(.rls_markdown_escape(unlist(data[i, ], use.names = FALSE)), collapse = " | "), " |")
  }, character(1L))
  c(header, align, rows)
}

.rls_render_table_markdown <- function(table) {
  header <- c(
    paste0("### ", table$title),
    "",
    paste0("Response: `", .rls_markdown_escape(if (isTRUE(table$responses_differ)) "see table" else table$response), "`")
  )
  if (!is.null(table$family)) {
    header <- c(header, paste0("Family: `", .rls_markdown_escape(table$family), "`"), paste0("Link: `", .rls_markdown_escape(table$link), "`"))
  }
  paste(c(
    header,
    "",
    .rls_render_data_frame_markdown(table$coefficients),
    "",
    .rls_render_data_frame_markdown(table$fit),
    "",
    table$note
  ), collapse = "\n")
}

.rls_copy_text <- function(text) {
  .rls_state$last_clipboard <- text
  copied <- FALSE
  if (.Platform$OS.type == "unix" && Sys.info()[["sysname"]] == "Darwin") {
    copied <- identical(tryCatch(system2("pbcopy", input = text), error = function(e) 1L), 0L)
  } else if (.Platform$OS.type == "windows" && exists("writeClipboard", envir = asNamespace("utils"), inherits = FALSE)) {
    utils::writeClipboard(text)
    copied <- TRUE
  }
  if (!copied) {
    message("Clipboard access is not available; returning the copied text invisibly.")
  }
  invisible(text)
}

.rls_export_table_pdf <- function(table, path, width = 8.5, height = 11) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lines <- strsplit(.rls_render_table_text(table), "\n", fixed = TRUE)[[1L]]
  line_height <- 0.032
  lines_per_page <- max(8L, floor(0.88 / line_height))
  if (isTRUE(capabilities("cairo"))) {
    grDevices::cairo_pdf(path, width = width, height = height, onefile = TRUE)
  } else {
    grDevices::pdf(path, width = width, height = height, onefile = TRUE)
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  for (page_start in seq(1L, length(lines), by = lines_per_page)) {
    graphics::plot.new()
    graphics::par(family = "Helvetica", mar = c(0, 0, 0, 0))
    page_lines <- lines[page_start:min(length(lines), page_start + lines_per_page - 1L)]
    y <- 0.96
    for (line in page_lines) {
      graphics::text(0.06, y, line, adj = c(0, 1), family = "mono", cex = 0.72)
      y <- y - line_height
    }
  }
  invisible(path)
}

#' Build a structured regression table
#'
#' @param model A GLM handle.
#' @return An `rlispstat_regression_table` object.
#' @export
ls_glm_regression_table <- function(model) {
  .rls_regression_table_from_glm(model)
}

#' @rdname ls_glm_regression_table
#' @export
ls_copy_glm_table <- function(model, format = c("text", "markdown")) {
  format <- match.arg(format)
  table <- .rls_regression_table_from_glm(model)
  text <- if (identical(format, "markdown")) .rls_render_table_markdown(table) else .rls_render_table_text(table)
  .rls_copy_text(text)
}

#' @rdname ls_glm_regression_table
#' @export
ls_export_glm_table <- function(model, path, format = c("pdf", "txt", "md")) {
  format <- match.arg(format)
  table <- .rls_regression_table_from_glm(model)
  switch(format,
    pdf = .rls_export_table_pdf(table, path),
    txt = writeLines(.rls_render_table_text(table), path, useBytes = TRUE),
    md = writeLines(.rls_render_table_markdown(table), path, useBytes = TRUE)
  )
  invisible(path)
}

#' @rdname ls_glm_regression_table
#' @export
ls_regression_comparison_table <- function(comparison) {
  .rls_regression_table_from_comparison(comparison)
}

#' @rdname ls_glm_regression_table
#' @export
ls_copy_regression_comparison_table <- function(comparison, format = c("text", "markdown")) {
  format <- match.arg(format)
  table <- .rls_regression_table_from_comparison(comparison)
  text <- if (identical(format, "markdown")) .rls_render_table_markdown(table) else .rls_render_table_text(table)
  .rls_copy_text(text)
}

#' @rdname ls_glm_regression_table
#' @export
ls_export_regression_comparison_table <- function(comparison, path, format = c("pdf", "txt", "md")) {
  format <- match.arg(format)
  table <- .rls_regression_table_from_comparison(comparison)
  switch(format,
    pdf = .rls_export_table_pdf(table, path),
    txt = writeLines(.rls_render_table_text(table), path, useBytes = TRUE),
    md = writeLines(.rls_render_table_markdown(table), path, useBytes = TRUE)
  )
  invisible(path)
}

#' @rdname ls_glm_regression_table
#' @export
ls_generalized_linear_model_table <- function(model) {
  .rls_regression_table_from_generalized_glm(model)
}

#' @rdname ls_glm_regression_table
#' @export
ls_copy_generalized_linear_model_table <- function(model, format = c("text", "markdown")) {
  format <- match.arg(format)
  table <- .rls_regression_table_from_generalized_glm(model)
  text <- if (identical(format, "markdown")) .rls_render_table_markdown(table) else .rls_render_table_text(table)
  .rls_copy_text(text)
}

#' @rdname ls_glm_regression_table
#' @export
ls_export_generalized_linear_model_table <- function(model, path, format = c("pdf", "txt", "md")) {
  format <- match.arg(format)
  table <- .rls_regression_table_from_generalized_glm(model)
  switch(format,
    pdf = .rls_export_table_pdf(table, path),
    txt = writeLines(.rls_render_table_text(table), path, useBytes = TRUE),
    md = writeLines(.rls_render_table_markdown(table), path, useBytes = TRUE)
  )
  invisible(path)
}

.rls_plot_themes <- c(
  "classic", "minimal", "bw", "gray",
  "cowplot", "ipsum", "theme_tq", "theme_modern",
  "tufte", "economist", "fivethirtyeight", "manet", "vista", "beige", "datadesk", "garish"
)

.rls_validate_plot_theme <- function(theme) {
  theme <- match.arg(theme, .rls_plot_themes)
  theme
}

.rls_sync_plot_theme_native <- function(theme) {
  if (!isTRUE(.rls_state$process_started)) {
    return(invisible(FALSE))
  }
  out <- try(.rls_send(c("PLOT_THEME", theme)), silent = TRUE)
  invisible(is.character(out) && startsWith(out, "OK"))
}

.rls_plot_theme_colors <- function(theme) {
  theme <- .rls_validate_plot_theme(theme)
  switch(theme,
    manet = list(background = "#FFFCA3", panel = "#FFFCA3", grid = "#D6D38A",
                 axis = "#111111", text = "#111111", muted = "#333333",
                 fill = "#B8B8B8", border = "#111111", accent = "#66FF00",
                 point = "#B8B8B8", bar = "#B8B8B8",
                 categorical = c("#B8B8B8", "#D8D8A8", "#888888")),
    vista = list(background = "#FFFFFF", panel = "#FFFFFF", grid = "#E5E5E5",
                 axis = "#111111", text = "#111111", muted = "#555555",
                 fill = "#A9B4E8", border = "#344BC5", accent = "#E3262E",
                 point = "#344BC5", bar = "#A9B4E8",
                 categorical = c("#A9B4E8", "#344BC5", "#1EAA4B")),
    beige = list(background = "#F6F4EA", panel = "#FFFDF6", grid = "#C7C5BB",
                 axis = "#171715", text = "#090908", muted = "#4A4944",
                 fill = "#DEDCD0", border = "#090908", accent = "#090908",
                 point = "#090908", bar = "#DEDCD0",
                 categorical = c("#090908", "#2E506E", "#9E251B", "#706B5D")),
    datadesk = list(background = "#FFFFFF", panel = "#FFFFFF", grid = "#FFFFFF",
                    axis = "#0D0D0D", text = "#050505", muted = "#333333",
                    fill = "#FFFFFF", border = "#050505", accent = "#050505",
                    point = "#050505", bar = "#FFFFFF",
                    categorical = c("#FFFFFF", "#BFBFBF", "#666666", "#111111")),
    garish = list(background = "#FF4FD8", panel = "#FFF36A", grid = "#6A00FF",
                  axis = "#24003D", text = "#180026", muted = "#55205F",
                  fill = "#00D8FF", border = "#24003D", accent = "#FF005C",
                  point = "#00D8FF", bar = "#00D8FF",
                  categorical = c("#00D8FF", "#FF005C", "#38E000", "#FF6B00", "#6A00FF")),
    minimal = list(background = "white", panel = "white", grid = "gray90",
                   axis = "gray15", text = "gray10", muted = "gray40",
                   fill = "gray94", border = "gray20", accent = "#0072B2"),
    bw = list(background = "white", panel = "white", grid = "gray82",
              axis = "black", text = "black", muted = "gray30",
              fill = "gray90", border = "black", accent = "black"),
    gray = list(background = "gray96", panel = "gray92", grid = "white",
                axis = "gray20", text = "gray10", muted = "gray35",
                fill = "gray82", border = "gray20", accent = "gray25"),
    cowplot = list(background = "white", panel = "white", grid = "gray88",
                   axis = "gray5", text = "gray5", muted = "gray30",
                   fill = "gray96", border = "gray5", accent = "gray5"),
    ipsum = list(background = "#FAFAF5", panel = "#FEFDF9", grid = "#D8D5CC",
                 axis = "#36342F", text = "#24221F", muted = "#69665C",
                 fill = "#E8E3D2", border = "#3D3A33", accent = "#00737F"),
    theme_tq = list(background = "#F4F6F9", panel = "white", grid = "#D0D6DD",
                    axis = "#17212B", text = "#0F171E", muted = "#545E68",
                    fill = "#C8DFEE", border = "#173854", accent = "#0073B2"),
    theme_modern = list(background = "#12141A", panel = "#1B1F26", grid = "#46515C",
                        axis = "#C8D4DE", text = "#ECF0F3", muted = "#A5B2BD",
                        fill = "#2B84B8", border = "#C2E0F0", accent = "#00B8D1"),
    tufte = list(background = "white", panel = "white", grid = "gray91",
                 axis = "gray25", text = "gray10", muted = "gray40",
                 fill = "#FFFFFF", border = "gray25", accent = "gray20"),
    economist = list(background = "#D5DCE2", panel = "#D5DCE2", grid = "white",
                     axis = "#293036", text = "#151A1D", muted = "#4D565C",
                     fill = "#B3C7D5", border = "#17242D", accent = "#E91E2F"),
    fivethirtyeight = list(background = "#F0F0ED", panel = "#F0F0ED", grid = "white",
                           axis = "#3A3D3E", text = "#1F2223", muted = "#65686A",
                           fill = "#C7CCCF", border = "#42484A", accent = "#007FBA"),
    classic = list(background = "#F6F7F8", panel = "#FDFDFC", grid = "#D9DEE0",
                   axis = "#30383D", text = "#171C20", muted = "#4D5459",
                   fill = "#DDE8EF", border = "#30383D", accent = "#0072B2")
  )
}

.rls_plot_record <- function(plot) {
  id <- .rls_plot_id(plot)
  if (!exists(id, envir = .rls_state$plots, inherits = FALSE)) {
    stop("No local plot metadata are available for this plot.", call. = FALSE)
  }
  get(id, envir = .rls_state$plots)
}

.rls_export_plot_native <- function(plot, path, format) {
  if (!isTRUE(.rls_state$process_started)) {
    return(FALSE)
  }
  out <- try(.rls_send(c("EXPORT_PLOT", .rls_plot_id(plot), normalizePath(path, mustWork = FALSE), toupper(format))), silent = TRUE)
  is.character(out) && startsWith(out, "OK")
}

.rls_copy_plot_native <- function(plot, format) {
  if (!isTRUE(.rls_state$process_started)) {
    return(FALSE)
  }
  out <- try(.rls_send(c("COPY_PLOT", .rls_plot_id(plot), toupper(format))), silent = TRUE)
  is.character(out) && startsWith(out, "OK")
}

.rls_plot_device <- function(path, format, width, height, dpi = 300) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  switch(format,
    pdf = grDevices::pdf(path, width = width, height = height, useDingbats = FALSE),
    png = grDevices::png(path, width = width, height = height, units = "in", res = dpi),
    svg = if (requireNamespace("svglite", quietly = TRUE)) {
      # svglite preserves labels as portable SVG text, unlike Cairo's glyph
      # outlines, while keeping all marks and clipping genuinely vectorial.
      svglite::svglite(path, width = width, height = height, bg = "transparent")
    } else {
      grDevices::svg(path, width = width, height = height)
    }
  )
}

.rls_plot_par <- function(theme) {
  colors <- .rls_plot_theme_colors(theme)
  graphics::par(
    bg = colors$background,
    fg = colors$axis,
    col.axis = colors$muted,
    col.lab = colors$text,
    col.main = colors$text,
    bty = "l",
    las = 1
  )
}

.rls_plot_panel <- function(theme) {
  colors <- .rls_plot_theme_colors(theme)
  usr <- graphics::par("usr")
  graphics::rect(usr[1], usr[3], usr[2], usr[4], col = colors$panel, border = NA)
  graphics::grid(col = colors$grid, lty = "solid")
  graphics::box(col = colors$axis)
}

.rls_export_axis_time <- function(values, side = 1L) {
  numeric_values <- suppressWarnings(as.double(values))
  numeric_values <- numeric_values[is.finite(numeric_values)]
  if (!length(numeric_values)) return(invisible(NULL))
  at <- pretty(range(numeric_values), n = 6L)
  if (inherits(values, "Date")) {
    labels <- format(as.Date(at, origin = "1970-01-01"))
  } else if (inherits(values, c("POSIXct", "POSIXlt"))) {
    labels <- format(as.POSIXct(at, origin = "1970-01-01", tz = attr(values, "tzone") %||% "UTC"))
  } else {
    # Years and other large integer-like time coordinates must remain readable,
    # never switching to scientific notation in the displayed/exported axis.
    labels <- format(at, scientific = FALSE, trim = TRUE, digits = 12)
  }
  graphics::axis(side, at = at, labels = labels)
  invisible(at)
}

.rls_export_draw_scatter_layers <- function(record, x, y, ok, colors) {
  if (isTRUE(record$lm) && sum(ok) >= 2L) {
    fit <- try(stats::lm(y[ok] ~ x[ok]), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      limits <- range(x[ok], finite = TRUE)
      coefficients <- stats::coef(fit)
      if (length(coefficients) >= 2L && all(is.finite(coefficients[1:2]))) {
        graphics::lines(limits, coefficients[[1L]] + coefficients[[2L]] * limits,
                        col = grDevices::adjustcolor(colors$axis, alpha.f = 0.9),
                        lwd = 2, lty = 2)
      }
    }
  }
  if (isTRUE(record$smooth) && sum(ok) >= 4L && length(unique(x[ok])) >= 3L) {
    frame <- data.frame(x = x[ok], y = y[ok])
    fit <- try(stats::loess(y ~ x, data = frame, span = record$smooth_span %||% 0.75,
                            na.action = stats::na.exclude, control = stats::loess.control(surface = "direct")),
               silent = TRUE)
    if (!inherits(fit, "try-error")) {
      at <- seq(min(frame$x), max(frame$x), length.out = 200L)
      estimate <- suppressWarnings(stats::predict(fit, newdata = data.frame(x = at)))
      usable <- is.finite(at) & is.finite(estimate)
      if (sum(usable) >= 2L) graphics::lines(at[usable], estimate[usable], col = colors$axis, lwd = 2.2)
    }
  }
  invisible(NULL)
}

.rls_export_scatter_fallback <- function(record, path, format, width, height, theme, dpi) {
  .rls_plot_device(path, format, width, height, dpi)
  on.exit(grDevices::dev.off(), add = TRUE)
  old <- .rls_plot_par(theme)
  on.exit(graphics::par(old), add = TRUE)
  colors <- .rls_plot_theme_colors(theme)
  if (is.data.frame(record$render_points) && nrow(record$render_points)) {
    x <- record$render_points$x
    y <- record$render_points$y
  } else {
    x <- record$data[[record$x]]
    y <- record$data[[record$y]]
  }
  ok <- is.finite(x) & is.finite(y)
  graphics::plot(x[ok], y[ok], type = "n", axes = FALSE,
                 xlab = record$x, ylab = record$y,
                 main = record$title %||% paste(record$y, "vs", record$x))
  .rls_plot_panel(theme)
  graphics::axis(1)
  graphics::axis(2)
  if (is.data.frame(record$line_points) && nrow(record$line_points)) {
    for (label in unique(record$line_points$label)) {
      line <- record$line_points[record$line_points$label == label, , drop = FALSE]
      line <- line[is.finite(line$x) & is.finite(line$y), , drop = FALSE]
      if (nrow(line) >= 2L) graphics::lines(line$x, line$y, col = colors$axis, lwd = 2)
    }
    if (identical(record$diagnostic_kind %||% "", "roc_curve")) {
      graphics::abline(0, 1, col = colors$muted, lty = 2)
    }
  }
  .rls_export_draw_scatter_layers(record, x, y, ok, colors)
  graphics::points(x[ok], y[ok], pch = if (identical(theme, "vista")) 1 else if (identical(theme, "datadesk")) 3 else 19,
                   cex = if (identical(theme, "datadesk")) 0.65 else 1,
                   col = grDevices::adjustcolor(colors$point %||% "black", alpha.f = 0.72))
  invisible(path)
}

.rls_export_precomputed_fallback <- function(record, path, format, width, height, theme, dpi) {
  points <- record$render_points
  lines <- record$line_points
  loadings <- record$loadings
  has_points <- is.data.frame(points) && nrow(points)
  has_lines <- is.data.frame(lines) && nrow(lines)
  if (!has_points && !has_lines) {
    stop("No native vector geometry is available for this plot.", call. = FALSE)
  }
  .rls_plot_device(path, format, width, height, dpi)
  on.exit(grDevices::dev.off(), add = TRUE)
  old <- .rls_plot_par(theme)
  on.exit(graphics::par(old), add = TRUE)
  colors <- .rls_plot_theme_colors(theme)
  x_values <- c(if (has_points) points$x, if (has_lines) lines$x)
  y_values <- c(if (has_points) points$y, if (has_lines) lines$y)
  finite <- is.finite(x_values) & is.finite(y_values)
  if (!any(finite)) stop("Native vector geometry contains no finite points.", call. = FALSE)
  xlim <- range(x_values[finite])
  ylim <- range(y_values[finite])
  if (!diff(xlim)) xlim <- xlim + c(-0.5, 0.5)
  if (!diff(ylim)) ylim <- ylim + c(-0.5, 0.5)
  pad <- function(limits, fraction = 0.06) limits + c(-1, 1) * diff(limits) * fraction
  xlim <- pad(xlim)
  ylim <- pad(ylim)
  graphics::plot(NA_real_, NA_real_, type = "n", axes = FALSE, xlim = xlim, ylim = ylim,
                 xlab = record$x %||% "", ylab = record$y %||% "",
                 main = record$title %||% "")
  .rls_plot_panel(theme)
  if (is.data.frame(record$x_ticks) && nrow(record$x_ticks)) {
    ticks <- record$x_ticks[is.finite(record$x_ticks$at), , drop = FALSE]
    graphics::axis(1, at = ticks$at, labels = ticks$label)
  } else {
    graphics::axis(1)
  }
  graphics::axis(2)
  kind <- record$type %||% ""
  if (identical(kind, "pca_scree") && has_points) {
    ordering <- order(points$x)
    graphics::lines(points$x[ordering], points$y[ordering], col = colors$axis, lwd = 2)
    parallel <- record$parallel_points
    if (is.data.frame(parallel) && nrow(parallel) >= 2L) {
      ordering <- order(parallel$x)
      graphics::lines(parallel$x[ordering], parallel$y[ordering],
                      col = colors$accent, lwd = 2, lty = 2)
      graphics::legend("topright", legend = c("Observed", "Parallel analysis"),
                       col = c(colors$axis, colors$accent), lty = c(1, 2),
                       pch = c(19, NA), bty = "n", cex = 0.8)
    }
  }
  if (has_lines) {
    labels <- unique(lines$label)
    palette <- rep(colors$categorical %||% colors$axis, length.out = length(labels))
    for (index in seq_along(labels)) {
      line <- lines[lines$label == labels[[index]], , drop = FALSE]
      line <- line[is.finite(line$x) & is.finite(line$y), , drop = FALSE]
      if (!nrow(line)) next
      ordering <- order(line$x)
      graphics::lines(line$x[ordering], line$y[ordering], col = palette[[index]], lwd = 2)
      graphics::points(line$x, line$y, col = palette[[index]], pch = 19, cex = 0.8)
    }
    if (length(labels) > 1L) {
      graphics::legend("topright", legend = labels, col = palette, lty = 1, pch = 19,
                       bty = "n", cex = 0.8)
    }
  }
  if (has_points) {
    graphics::points(points$x, points$y,
                     pch = if (identical(theme, "datadesk")) 3 else 19,
                     col = grDevices::adjustcolor(colors$point %||% colors$axis, alpha.f = 0.72))
  }
  if (identical(kind, "pca_biplot") && is.data.frame(loadings) && nrow(loadings)) {
    loadings <- loadings[is.finite(loadings$x) & is.finite(loadings$y), , drop = FALSE]
    if (nrow(loadings)) {
      score_x <- max(abs(xlim), na.rm = TRUE)
      score_y <- max(abs(ylim), na.rm = TRUE)
      loading_x <- max(abs(loadings$x), na.rm = TRUE)
      loading_y <- max(abs(loadings$y), na.rm = TRUE)
      factors <- c(if (loading_x > 0) score_x / loading_x,
                   if (loading_y > 0) score_y / loading_y)
      scale <- if (length(factors)) 0.78 * min(factors) else 1
      end_x <- loadings$x * scale
      end_y <- loadings$y * scale
      graphics::arrows(0, 0, end_x, end_y, length = 0.08, col = colors$accent, lwd = 1.5)
      graphics::text(end_x, end_y, labels = loadings$variable, pos = 3,
                     col = colors$text, cex = 0.75, xpd = NA)
    }
  }
  invisible(path)
}

.rls_export_time_series_fallback <- function(record, path, format, width, height, theme, dpi) {
  .rls_plot_device(path, format, width, height, dpi)
  on.exit(grDevices::dev.off(), add = TRUE)
  old <- .rls_plot_par(theme)
  on.exit(graphics::par(old), add = TRUE)
  colors <- .rls_plot_theme_colors(theme)
  time <- record$data[[record$x]]
  value <- suppressWarnings(as.double(record$data[[record$y]]))
  ok <- !is.na(time) & is.finite(value)
  graphics::plot(time[ok], value[ok], type = "n", axes = FALSE,
                 xlab = record$x, ylab = record$y,
                 main = record$title %||% paste(record$y, "over", record$x))
  .rls_plot_panel(theme)
  .rls_export_axis_time(time)
  graphics::axis(2)
  series <- record$series %||% ""
  if (nzchar(series) && series %in% names(record$data)) {
    groups <- factor(record$data[[series]][ok], exclude = NULL)
    palette <- colors$categorical %||% grDevices::hcl.colors(max(3L, nlevels(groups)), "Dark 3")
    for (index in seq_along(levels(groups))) {
      in_group <- groups == levels(groups)[[index]]
      order <- order(time[ok][in_group])
      graphics::lines(time[ok][in_group][order], value[ok][in_group][order],
                      col = palette[[1L + (index - 1L) %% length(palette)]], lwd = 1.4)
      graphics::points(time[ok][in_group], value[ok][in_group], pch = 19,
                       col = palette[[1L + (index - 1L) %% length(palette)]], cex = 0.7)
    }
    identification <- record$identification %||% "legend"
    if (identical(identification, "legend")) {
      location <- switch(record$legend_position %||% "top_right",
                         top_left = "topleft", bottom_right = "bottomright",
                         bottom_left = "bottomleft", "topright")
      graphics::legend(location, legend = levels(groups), col = rep(palette, length.out = nlevels(groups)),
                       lty = 1, pch = 19, bty = "n", cex = 0.8)
    } else if (identical(identification, "start_labels")) {
      for (index in seq_along(levels(groups))) {
        in_group <- groups == levels(groups)[[index]]
        ordering <- order(time[ok][in_group])
        if (!length(ordering)) next
        first <- ordering[[1L]]
        graphics::text(time[ok][in_group][first], value[ok][in_group][first],
                       labels = levels(groups)[[index]], pos = 2, xpd = NA, cex = 0.78,
                       col = palette[[1L + (index - 1L) %% length(palette)]])
      }
    }
  } else {
    order <- order(time[ok])
    graphics::lines(time[ok][order], value[ok][order], col = colors$axis, lwd = 1.4)
    graphics::points(time[ok], value[ok], pch = 19, col = colors$point %||% colors$axis, cex = 0.7)
  }
  invisible(path)
}

.rls_export_scatter_matrix_fallback <- function(record, path, format, width, height, theme, dpi) {
  variables <- record$variables %||% character()
  variables <- variables[variables %in% names(record$data)]
  variables <- variables[vapply(record$data[variables], is.numeric, logical(1L))]
  if (length(variables) < 2L) {
    stop("At least two numeric variables are required for scatterplot-matrix export.", call. = FALSE)
  }
  .rls_plot_device(path, format, width, height, dpi)
  on.exit(grDevices::dev.off(), add = TRUE)
  old <- .rls_plot_par(theme)
  on.exit(graphics::par(old), add = TRUE)
  colors <- .rls_plot_theme_colors(theme)
  point_character <- if (identical(theme, "datadesk")) 3 else 19
  graphics::pairs(
    record$data[variables],
    labels = variables,
    pch = point_character,
    col = grDevices::adjustcolor(colors$point %||% "black", alpha.f = 0.72),
    main = record$title %||% "Scatterplot matrix"
  )
  invisible(path)
}

.rls_export_trellis_scatterplot_fallback <- function(record, path, format, width, height, theme, dpi) {
  .rls_plot_device(path, format, width, height, dpi)
  on.exit(grDevices::dev.off(), add = TRUE)
  old <- .rls_plot_par(theme)
  on.exit(graphics::par(old), add = TRUE)
  colors <- .rls_plot_theme_colors(theme)
  plot_type <- record$plot_type %||% "scatter"
  conditions <- record$conditions %||% list(list(variable = record$condition, kind = "categorical"))
  conditions <- conditions[vapply(conditions, function(x) nzchar(x$variable %||% ""), logical(1L))]
  if (!length(conditions)) stop("A conditioning variable is required for trellis export.", call. = FALSE)
  condition_factors <- vector("list", length(conditions))
  condition_levels <- vector("list", length(conditions))
  for (index in seq_along(conditions)) {
    specification <- conditions[[index]]
    values <- record$data[[specification$variable]]
    if (identical(specification$kind %||% "categorical", "continuous")) {
      numeric_values <- as.double(values)
      bins <- max(2L, min(6L, as.integer(specification$bins %||% 4L)))
      if (identical(specification$method %||% "equal_width", "equal_count")) {
        breaks <- unique(stats::quantile(numeric_values, probs = seq(0, 1, length.out = bins + 1L),
                                         na.rm = TRUE, names = FALSE, type = 7))
      } else {
        limits <- range(numeric_values, finite = TRUE)
        breaks <- seq(limits[[1L]], limits[[2L]], length.out = bins + 1L)
      }
      if (length(breaks) < 2L) stop("Too few unique values for continuous conditioning.", call. = FALSE)
      condition_factors[[index]] <- cut(numeric_values, breaks = breaks, include.lowest = TRUE,
                                        right = TRUE, dig.lab = 7)
    } else {
      levels <- .rls_trellis_levels(values, which(!is.na(values)))
      condition_factors[[index]] <- factor(as.character(values), levels = levels)
    }
    condition_levels[[index]] <- levels(condition_factors[[index]])
  }
  complete_condition <- Reduce(`&`, lapply(condition_factors, function(x) !is.na(x)))
  panel_grid <- expand.grid(condition_levels, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  panel_keys <- apply(panel_grid, 1L, paste, collapse = "\037")
  panel_labels <- apply(panel_grid, 1L, function(values) paste(
    paste0(vapply(conditions, `[[`, character(1L), "variable"), " = ", values), collapse = " · "))
  row_keys <- do.call(paste, c(lapply(condition_factors, as.character), sep = "\037"))
  panel_count <- length(panel_keys)
  columns <- if (length(conditions) >= 2L) length(condition_levels[[1L]]) else
    .rls_trellis_columns(panel_count, width, height, record$layout %||% "automatic")
  rows <- ceiling(panel_count / columns)
  outer_left <- 0.075
  outer_right <- 0.018
  outer_bottom <- 0.085
  outer_top <- 0.105
  gap_x <- 0.018
  gap_y <- 0.025
  available_width <- 1 - outer_left - outer_right
  available_height <- 1 - outer_bottom - outer_top
  panel_width <- (available_width - gap_x * (columns - 1L)) / columns
  maximum_panel_height <- panel_width * width / (height * 1.05)
  panel_height <- min(
    (available_height - gap_y * (rows - 1L)) / rows,
    maximum_panel_height
  )
  grid_height <- panel_height * rows + gap_y * (rows - 1L)
  grid_bottom <- outer_bottom + (available_height - grid_height) / 2
  x_raw <- record$data[[record$x]]
  y_raw <- if (nzchar(record$y %||% "") && record$y %in% names(record$data)) record$data[[record$y]] else NULL
  x_values <- suppressWarnings(as.double(x_raw))
  y_values <- suppressWarnings(as.double(y_raw))
  if (plot_type %in% c("scatter", "time_series", "histogram")) {
    x_limits <- range(x_values[complete_condition], finite = TRUE)
  } else {
    x_limits <- c(0.5, length(unique(as.character(x_raw[!is.na(x_raw)]))) + 0.5)
  }
  if (plot_type %in% c("scatter", "time_series", "boxplot")) y_limits <- range(y_values[complete_condition], finite = TRUE) else y_limits <- c(0, 1)
  x_padding <- diff(x_limits) * 0.05
  y_padding <- diff(y_limits) * 0.05
  if (!is.finite(x_padding) || x_padding <= 0) x_padding <- 0.5
  if (!is.finite(y_padding) || y_padding <= 0) y_padding <- 0.5
  x_limits <- x_limits + c(-x_padding, x_padding)
  y_limits <- y_limits + c(-y_padding, y_padding)
  if (plot_type == "histogram") {
    histogram_breaks <- seq(x_limits[[1L]], x_limits[[2L]], length.out = (record$histogram_bins %||% 10L) + 1L)
    maxima <- vapply(panel_keys, function(key) {
      counts <- graphics::hist(x_values[row_keys == key & complete_condition], breaks = histogram_breaks, plot = FALSE)$counts
      if (identical(record$histogram_measure %||% "count", "percent")) counts <- 100 * counts / max(1, sum(counts))
      if (identical(record$histogram_measure %||% "count", "density")) counts <- counts / max(1, sum(counts)) / diff(histogram_breaks)
      max(counts, 0)
    }, numeric(1L))
    y_limits <- c(0, max(maxima, 1) * 1.05)
  } else if (plot_type == "bar") {
    categories <- unique(as.character(x_raw[!is.na(x_raw)]))
    maxima <- vapply(panel_keys, function(key) {
      counts <- table(factor(as.character(x_raw[row_keys == key & complete_condition]), levels = categories))
      if (identical(record$bar_measure %||% "count", "percent")) counts <- 100 * counts / max(1, sum(counts))
      max(counts, 0)
    }, numeric(1L))
    y_limits <- c(0, max(maxima, 1) * 1.05)
  }
  point_character <- if (identical(theme, "datadesk")) 3 else 19
  for (index in seq_along(panel_keys)) {
    in_panel <- row_keys == panel_keys[[index]] & complete_condition
    panel_row <- ceiling(index / columns)
    panel_column <- ((index - 1L) %% columns) + 1L
    first_in_row <- (panel_row - 1L) * columns + 1L
    count_in_row <- min(columns, panel_count - first_in_row + 1L)
    row_width <- panel_width * count_in_row + gap_x * (count_in_row - 1L)
    row_left <- outer_left + (available_width - row_width) / 2
    panel_left <- row_left + (panel_column - 1L) * (panel_width + gap_x)
    panel_bottom <- grid_bottom + (rows - panel_row) * (panel_height + gap_y)
    graphics::par(
      fig = c(panel_left, panel_left + panel_width,
              panel_bottom, panel_bottom + panel_height),
      mar = c(2.4, 2.8, 1.8, 0.5),
      oma = c(0, 0, 0, 0),
      new = index > 1L
    )
    if (plot_type %in% c("scatter", "time_series")) {
      panel_x_limits <- x_limits
      panel_y_limits <- y_limits
      if ((record$scale_mode %||% "common_xy") %in% c("free_x", "free_xy") && any(in_panel)) {
        panel_x_limits <- range(x_values[in_panel], finite = TRUE)
        padding <- diff(panel_x_limits) * 0.05
        if (!is.finite(padding) || padding <= 0) padding <- 0.5
        panel_x_limits <- panel_x_limits + c(-padding, padding)
      }
      if ((record$scale_mode %||% "common_xy") %in% c("free_y", "free_xy") && any(in_panel)) {
        panel_y_limits <- range(y_values[in_panel], finite = TRUE)
        padding <- diff(panel_y_limits) * 0.05
        if (!is.finite(padding) || padding <= 0) padding <- 0.5
        panel_y_limits <- panel_y_limits + c(-padding, padding)
      }
      graphics::plot(NA_real_, NA_real_, xlim = panel_x_limits, ylim = panel_y_limits, axes = FALSE,
                     xlab = "", ylab = "", main = panel_labels[[index]], type = "n")
      .rls_plot_panel(theme)
      if (plot_type == "time_series" && panel_row == rows) {
        .rls_export_axis_time(x_raw[in_panel])
      } else {
        graphics::axis(1, labels = panel_row == rows)
      }
      graphics::axis(2, labels = panel_column == 1L, las = 1)
      if (plot_type == "time_series") {
        grouping <- record$grouping %||% ""
        if (nzchar(grouping) && grouping %in% names(record$data)) {
          groups <- factor(record$data[[grouping]][in_panel], exclude = NULL)
          palette <- colors$categorical %||% grDevices::hcl.colors(max(3L, nlevels(groups)), "Dark 3")
          for (group_index in seq_along(levels(groups))) {
            in_group <- groups == levels(groups)[[group_index]]
            ordering <- order(x_values[in_panel][in_group])
            graphics::lines(x_values[in_panel][in_group][ordering], y_values[in_panel][in_group][ordering],
                            col = palette[[1L + (group_index - 1L) %% length(palette)]], lwd = 1.4)
          }
        } else {
          ordering <- order(x_values[in_panel])
          graphics::lines(x_values[in_panel][ordering], y_values[in_panel][ordering],
                          col = colors$axis, lwd = 1.4)
        }
      } else {
        .rls_export_draw_scatter_layers(record, x_values, y_values, in_panel, colors)
      }
      graphics::points(x_values[in_panel], y_values[in_panel], pch = point_character,
                       col = grDevices::adjustcolor(colors$point %||% "black", alpha.f = 0.78))
    } else if (plot_type == "boxplot") {
      graphics::boxplot(y_values[in_panel] ~ factor(x_raw[in_panel]), ylim = y_limits,
                        axes = FALSE, xlab = "", ylab = "", main = panel_labels[[index]],
                        col = colors$fill, border = colors$border)
      graphics::axis(1, labels = panel_row == rows); graphics::axis(2, labels = panel_column == 1L, las = 1)
    } else if (plot_type == "bar") {
      categories <- unique(as.character(x_raw[!is.na(x_raw)]))
      values <- table(factor(as.character(x_raw[in_panel]), levels = categories))
      if (identical(record$bar_measure %||% "count", "percent")) values <- 100 * values / max(1, sum(values))
      graphics::barplot(values, ylim = y_limits, axes = FALSE, main = panel_labels[[index]],
                        col = colors$bar %||% colors$fill, border = colors$axis)
      graphics::axis(1, at = seq_along(categories) - 0.5, labels = if (panel_row == rows) categories else FALSE)
      graphics::axis(2, labels = panel_column == 1L, las = 1)
    } else {
      histogram <- graphics::hist(x_values[in_panel], breaks = histogram_breaks, plot = FALSE)
      values <- histogram$counts
      if (identical(record$histogram_measure %||% "count", "percent")) values <- 100 * values / max(1, sum(values))
      if (identical(record$histogram_measure %||% "count", "density")) values <- values / max(1, sum(values)) / diff(histogram_breaks)
      graphics::plot(NA_real_, NA_real_, xlim = x_limits, ylim = y_limits, axes = FALSE,
                     xlab = "", ylab = "", main = panel_labels[[index]], type = "n")
      .rls_plot_panel(theme)
      graphics::rect(histogram_breaks[-length(histogram_breaks)], 0,
                     histogram_breaks[-1L], values, col = colors$fill, border = colors$border)
      graphics::axis(1, labels = panel_row == rows); graphics::axis(2, labels = panel_column == 1L, las = 1)
    }
    if (!any(in_panel)) graphics::text(mean(x_limits), mean(y_limits), "No observations", col = colors$axis)
  }
  graphics::par(fig = c(0, 1, 0, 1), mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0),
                new = TRUE, xpd = NA)
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
  graphics::text(
    0.5, 0.975,
    if (isTRUE(record$custom_title)) record$title else .rls_trellis_derived_title(record),
    font = 2, cex = 1.05
  )
  graphics::text(0.5, 0.025, record$x)
  y_label <- if (plot_type %in% c("scatter", "time_series", "boxplot")) record$y else if (plot_type == "bar") {
    if (identical(record$bar_measure %||% "count", "percent")) "Percent within panel" else "Count"
  } else if (identical(record$histogram_measure %||% "count", "density")) "Density" else if (
    identical(record$histogram_measure %||% "count", "percent")) "Percent" else "Count"
  graphics::text(0.018, 0.5, y_label, srt = 90)
  invisible(path)
}

.rls_export_boxplot_fallback <- function(record, path, format, width, height, theme, dpi) {
  .rls_plot_device(path, format, width, height, dpi)
  on.exit(grDevices::dev.off(), add = TRUE)
  old <- .rls_plot_par(theme)
  on.exit(graphics::par(old), add = TRUE)
  colors <- .rls_plot_theme_colors(theme)
  if (nzchar(record$x)) {
    graphics::boxplot(record$data[[record$y]] ~ record$data[[record$x]],
                      xlab = record$x, ylab = record$y, main = record$title %||% record$y,
                      col = colors$fill, border = colors$border)
  } else if (length(record$boxplot_variables %||% character()) > 1L || isTRUE(record$standardize)) {
    vars <- record$boxplot_variables
    if (!length(vars)) {
      vars <- record$y
    }
    values <- record$data[vars]
    if (isTRUE(record$standardize)) {
      values <- as.data.frame(scale(values))
      values[] <- lapply(values, function(x) {
        x[is.na(x)] <- 0
        x
      })
    }
    graphics::boxplot(values, ylab = if (isTRUE(record$standardize)) "Standardized value" else "Value",
                      main = record$title %||% "Parallel boxplots",
                      col = colors$fill, border = colors$border)
    if (isTRUE(record$connect_rows)) {
      mat <- as.matrix(values)
      ok <- stats::complete.cases(mat)
      if (any(ok)) {
        graphics::matlines(seq_along(vars), t(mat[ok, , drop = FALSE]), type = "l",
                           lty = 1, col = grDevices::adjustcolor(colors$axis, alpha.f = 0.25))
      }
    }
  } else {
    graphics::boxplot(record$data[[record$y]], ylab = record$y, main = record$title %||% record$y,
                      col = colors$fill, border = colors$border)
  }
  invisible(path)
}

.rls_export_histogram_fallback <- function(record, path, format, width, height, theme, dpi, breaks = NULL) {
  .rls_plot_device(path, format, width, height, dpi)
  on.exit(grDevices::dev.off(), add = TRUE)
  old <- .rls_plot_par(theme)
  on.exit(graphics::par(old), add = TRUE)
  colors <- .rls_plot_theme_colors(theme)
  values <- if (is.data.frame(record$render_points) && nrow(record$render_points)) {
    record$render_points$x
  } else {
    record$data[[record$x]]
  }
  values <- values[is.finite(values)]
  requested_breaks <- breaks %||% record$histogram_bins %||% "Sturges"
  histogram <- graphics::hist(values, breaks = requested_breaks, xlab = record$x,
                              main = record$title %||% paste("Histogram of", record$x),
                              col = colors$fill, border = colors$border)
  if (isTRUE(record$show_density) && identical(record$density_mode %||% "all", "all") && length(values) >= 2L) {
    bandwidth <- record$density_bw %||% 0
    density <- stats::density(values, bw = if (is.finite(bandwidth) && bandwidth > 0) bandwidth else "nrd0",
                              adjust = record$density_adjust %||% 1, na.rm = TRUE)
    bin_width <- if (length(histogram$breaks) >= 2L) stats::median(diff(histogram$breaks)) else 1
    graphics::lines(density$x, density$y * length(values) * bin_width, col = colors$axis, lwd = 2.2)
  }
  if (isTRUE(record$show_rug)) graphics::rug(values, col = colors$axis)
  invisible(path)
}

.rls_export_barplot_fallback <- function(record, path, format, width, height, theme, dpi) {
  .rls_plot_device(path, format, width, height, dpi)
  on.exit(grDevices::dev.off(), add = TRUE)
  old <- .rls_plot_par(theme)
  on.exit(graphics::par(old), add = TRUE)
  colors <- .rls_plot_theme_colors(theme)
  xvars <- record$x
  if (length(xvars) == 1L) {
    xvars <- trimws(strsplit(as.character(xvars), "\\s*\\+\\s*")[[1L]])
  }
  xvars <- xvars[nzchar(xvars)]
  if (!length(xvars)) {
    stop("No X variable metadata are available for barplot export.", call. = FALSE)
  }
  xdata <- lapply(xvars, function(name) record$data[[name]])
  xgroup <- if (length(xdata) == 1L) {
    factor(xdata[[1L]], exclude = NULL)
  } else {
    interaction(xdata, drop = TRUE, sep = " / ", lex.order = TRUE)
  }
  main <- record$title %||% paste("Bar chart of", paste(xvars, collapse = " + "))
  if (nzchar(record$y %||% "")) {
    y <- factor(record$data[[record$y]], exclude = NULL)
    tab <- table(xgroup, y, useNA = "ifany")
    fill <- colors$categorical %||% grDevices::gray.colors(max(2L, ncol(tab)), start = 0.82, end = 0.45)
    fill <- rep(fill, length.out = ncol(tab))
    graphics::barplot(t(tab), beside = FALSE, col = fill, border = colors$axis,
                      main = main, xlab = paste(xvars, collapse = " + "),
                      ylab = "Count", legend.text = colnames(tab),
                      args.legend = list(x = "topright", bty = "n", cex = 0.8))
  } else {
    tab <- table(xgroup, useNA = "ifany")
    graphics::barplot(tab, col = colors$bar %||% "gray82", border = colors$axis,
                      main = main, xlab = paste(xvars, collapse = " + "), ylab = "Count")
  }
  invisible(path)
}

#' Plot export theme
#'
#' Gets or sets the application-wide plot theme used by native plots and R
#' fallback plot exports.
#'
#' @param theme Optional theme. Supported values include `"classic"`,
#' `"minimal"`, `"bw"`, `"gray"`, `"cowplot"`, `"ipsum"`, `"theme_tq"`,
#' `"theme_modern"`, `"tufte"`, `"economist"`, `"fivethirtyeight"`,
#' `"manet"`, `"vista"`, `"beige"`, `"datadesk"`, and `"garish"`.
#' @return The current theme invisibly when setting, or visibly when querying.
#' @export
ls_plot_theme <- function(theme = NULL) {
  if (is.null(theme)) {
    return(.rls_state$plot_theme %||% "classic")
  }
  theme <- .rls_validate_plot_theme(theme)
  .rls_state$plot_theme <- theme
  .rls_sync_plot_theme_native(theme)
  invisible(theme)
}

#' Export or copy linked plot output
#'
#' @param plot An `rlispstat_plot` object or plot id.
#' @param path Output path.
#' @param width,height Output size in inches for R fallback export.
#' @param format For saving, one of `"pdf"`, `"png"`, or `"svg"`. For
#'   copying, `NULL` chooses EMF on Windows and PDF elsewhere; explicit
#'   `"pdf"`, `"svg"`, `"png"`, and (on Windows) `"emf"` are accepted.
#' @param theme Optional plot theme for this export.
#' @param dpi PNG resolution for R fallback export.
#' @return Invisibly returns `path` or copied status.
#' @export
ls_export_plot <- function(plot, path, width = NULL, height = NULL,
                           format = c("pdf", "png", "svg"),
                           theme = NULL, dpi = 300) {
  format <- match.arg(format)
  theme <- .rls_validate_plot_theme(theme %||% (.rls_state$plot_theme %||% "classic"))
  if (isTRUE(.rls_state$process_started)) {
    .rls_sync_plot_theme_native(theme)
  }
  if (format %in% c("pdf", "png") && .rls_export_plot_native(plot, path, format)) {
    return(invisible(path))
  }
  record <- .rls_plot_record(plot)
  width <- width %||% 7
  type <- record$type %||% "scatter"
  height <- height %||% if (identical(type, "trellis_scatterplot")) {
    max(3.2, width * 0.48)
  } else {
    5
  }
  if (identical(type, "scatter")) {
    return(.rls_export_scatter_fallback(record, path, format, width, height, theme, dpi))
  }
  if (type %in% c("pca_scree", "pca_biplot", "glm_interaction", "interaction_plot")) {
    return(.rls_export_precomputed_fallback(record, path, format, width, height, theme, dpi))
  }
  if (identical(type, "time_series")) {
    return(.rls_export_time_series_fallback(record, path, format, width, height, theme, dpi))
  }
  if (identical(type, "scatter_matrix")) {
    return(.rls_export_scatter_matrix_fallback(record, path, format, width, height, theme, dpi))
  }
  if (identical(type, "trellis_scatterplot")) {
    return(.rls_export_trellis_scatterplot_fallback(record, path, format, width, height, theme, dpi))
  }
  if (identical(type, "boxplot")) {
    return(.rls_export_boxplot_fallback(record, path, format, width, height, theme, dpi))
  }
  if (identical(type, "histogram")) {
    breaks <- if (inherits(plot, "rlispstat_plot")) plot$breaks else record$breaks
    return(.rls_export_histogram_fallback(record, path, format, width, height, theme, dpi, breaks = breaks))
  }
  if (identical(type, "barplot")) {
    return(.rls_export_barplot_fallback(record, path, format, width, height, theme, dpi))
  }
  stop(sprintf("No plot export fallback is available for plot type `%s`.", type), call. = FALSE)
}

#' @rdname ls_export_plot
#' @export
ls_copy_plot <- function(plot, format = NULL, theme = NULL) {
  if (is.null(format)) format <- if (.Platform$OS.type == "windows") "emf" else "pdf"
  format <- match.arg(format, c("pdf", "svg", "png", "emf"))
  if (identical(format, "emf") && .Platform$OS.type != "windows") {
    stop("Enhanced Metafile copy is available only on Windows.", call. = FALSE)
  }
  theme <- .rls_validate_plot_theme(theme %||% (.rls_state$plot_theme %||% "classic"))
  if (isTRUE(.rls_state$process_started)) {
    .rls_sync_plot_theme_native(theme)
  }
  if (.rls_copy_plot_native(plot, format)) {
    return(invisible(TRUE))
  }
  if (identical(format, "emf")) {
    stop("The Windows native backend could not place an Enhanced Metafile on the clipboard.", call. = FALSE)
  }
  path <- tempfile("rlispstat-plot-", fileext = paste0(".", format))
  ls_export_plot(plot, path, format = format, theme = theme)
  if (identical(format, "svg")) {
    .rls_copy_text(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  } else {
    .rls_copy_text(path)
  }
}

#' @rdname ls_export_plot
#' @export
ls_export_scatterplot <- function(plot, path, width = NULL, height = NULL,
                                  format = c("pdf", "png", "svg"),
                                  theme = NULL, dpi = 300) {
  ls_export_plot(plot, path, width = width, height = height, format = format,
                 theme = theme, dpi = dpi)
}

#' @rdname ls_export_plot
#' @export
ls_copy_scatterplot <- function(plot, format = NULL, theme = NULL) {
  ls_copy_plot(plot, format = format, theme = theme)
}

#' @rdname ls_export_plot
#' @export
ls_export_barplot <- function(plot, path, width = NULL, height = NULL,
                              format = c("pdf", "png", "svg"),
                              theme = NULL, dpi = 300) {
  ls_export_plot(plot, path, width = width, height = height, format = format,
                 theme = theme, dpi = dpi)
}

#' @rdname ls_export_plot
#' @export
ls_copy_barplot <- function(plot, format = NULL, theme = NULL) {
  ls_copy_plot(plot, format = format, theme = theme)
}

#' @rdname ls_export_plot
#' @export
ls_export_boxplot <- function(plot, path, width = NULL, height = NULL,
                              format = c("pdf", "png", "svg"),
                              theme = NULL, dpi = 300) {
  ls_export_plot(plot, path, width = width, height = height, format = format,
                 theme = theme, dpi = dpi)
}

#' @rdname ls_export_plot
#' @export
ls_copy_boxplot <- function(plot, format = NULL, theme = NULL) {
  ls_copy_plot(plot, format = format, theme = theme)
}

#' @rdname ls_export_plot
#' @export
ls_export_histogram <- function(plot, path, width = NULL, height = NULL,
                                format = c("pdf", "png", "svg"),
                                theme = NULL, dpi = 300) {
  ls_export_plot(plot, path, width = width, height = height, format = format,
                 theme = theme, dpi = dpi)
}

#' @rdname ls_export_plot
#' @export
ls_copy_histogram <- function(plot, format = NULL, theme = NULL) {
  ls_copy_plot(plot, format = format, theme = theme)
}

#' @rdname ls_export_plot
#' @export
ls_export_barchart <- ls_export_barplot

#' @rdname ls_export_plot
#' @export
ls_copy_barchart <- ls_copy_barplot
