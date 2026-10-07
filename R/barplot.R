#' Prepare linked bar chart data
#'
#' @param data A data frame.
#' @param x One or more X condition variables.
#' @param y Optional stacked segment variable.
#' @param group Optional linked brushing group.
#' @param mode Vertical scale.
#' @param bar_width Bar width mode.
#' @param include_missing Include missing X/Y values as `NA`.
#' @param sort_x,sort_y Sort X conditions or Y levels alphabetically.
#' @return A structured list used by the native backend.
.rls_prepare_barplot_data <- function(data, x, y = NULL, group = NULL,
                                      mode = c("conditional_percent", "count", "overall_percent"),
                                      bar_width = c("equal", "proportional_n", "proportional_percent"),
                                      include_missing = TRUE,
                                      sort_x = FALSE,
                                      sort_y = FALSE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!is.character(x) || !length(x) || anyNA(x) || any(!nzchar(x))) {
    stop("`x` must name one or more columns.", call. = FALSE)
  }
  x <- vapply(x, .rls_validate_protocol_name, character(1L), what = "x")
  missing_x <- setdiff(x, names(data))
  if (length(missing_x)) {
    stop(sprintf("Column `%s` was not found in `data`.", missing_x[[1L]]), call. = FALSE)
  }
  if (!is.null(y)) {
    y <- .rls_validate_protocol_name(y, "y")
    if (!y %in% names(data)) {
      stop(sprintf("Column `%s` was not found in `data`.", y), call. = FALSE)
    }
  }
  group <- .rls_validate_group(group, allow_null = TRUE)
  mode <- match.arg(mode)
  bar_width <- match.arg(bar_width)
  for (arg in c("include_missing", "sort_x", "sort_y")) {
    value <- get(arg)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop(sprintf("`%s` must be TRUE or FALSE.", arg), call. = FALSE)
    }
  }

  x_values <- lapply(x, function(name) as.character(data[[name]]))
  x_missing <- Reduce(`|`, lapply(x_values, is.na), init = rep(FALSE, nrow(data)))
  if (isTRUE(include_missing)) {
    x_values <- lapply(x_values, function(value) {
      value[is.na(value)] <- "NA"
      value
    })
  }

  y_values <- if (is.null(y)) {
    rep("All", nrow(data))
  } else {
    as.character(data[[y]])
  }
  y_missing <- is.na(y_values)
  if (isTRUE(include_missing)) {
    y_values[y_missing] <- "NA"
  }

  included <- rep(TRUE, nrow(data))
  if (!isTRUE(include_missing)) {
    included <- included & !x_missing & !y_missing
  }
  rows <- which(included)
  excluded_rows <- which(!included)
  if (!length(rows)) {
    stop("No rows are available for the bar chart after applying missing-value rules.", call. = FALSE)
  }

  x_by_row <- lapply(x_values, function(value) value[rows])
  x_condition <- if (length(x) == 1L) {
    x_by_row[[1L]]
  } else {
    pieces <- lapply(seq_along(x), function(i) paste0(x[[i]], "=", x_by_row[[i]]))
    do.call(paste, c(pieces, sep = "; "))
  }
  y_level <- y_values[rows]

  x_levels <- .rls_ordered_barplot_x_conditions(x_condition, x_by_row, sort_x = sort_x)
  y_levels <- unique(y_level)
  if (isTRUE(sort_y)) y_levels <- sort(y_levels)

  total_n <- length(rows)
  bar_rows <- split(rows, factor(x_condition, levels = x_levels), drop = TRUE)
  segment_rows <- split(rows, interaction(
    factor(x_condition, levels = x_levels),
    factor(y_level, levels = y_levels),
    sep = "\r", drop = TRUE
  ))

  bars <- lapply(x_levels, function(condition) {
    ids <- as.integer(bar_rows[[condition]] %||% integer())
    bar_n <- length(ids)
    bar_percent <- if (total_n > 0L) 100 * bar_n / total_n else 0
    width_value <- switch(bar_width,
      equal = 1,
      proportional_n = bar_n,
      proportional_percent = bar_percent
    )
    data.frame(
      x_condition = condition,
      bar_n = bar_n,
      bar_percent = bar_percent,
      bar_width_value = width_value,
      row_ids = I(list(ids)),
      stringsAsFactors = FALSE
    )
  })
  bars <- do.call(rbind, bars)

  segments <- list()
  for (condition in x_levels) {
    bar_n <- bars$bar_n[match(condition, bars$x_condition)]
    bar_percent <- bars$bar_percent[match(condition, bars$x_condition)]
    width_value <- bars$bar_width_value[match(condition, bars$x_condition)]
    for (level in y_levels) {
      key <- paste(condition, level, sep = "\r")
      ids <- as.integer(segment_rows[[key]] %||% integer())
      count <- length(ids)
      if (count == 0L) next
      conditional_percent <- if (bar_n > 0L) 100 * count / bar_n else 0
      overall_percent <- if (total_n > 0L) 100 * count / total_n else 0
      tooltip <- paste(
        paste0("X condition: ", condition),
        paste0(if (is.null(y)) "Y level" else y, ": ", level),
        paste0("n = ", count),
        paste0("bar denominator = ", bar_n),
        paste0("conditional percent = ", sprintf("%.1f%%", conditional_percent)),
        paste0("overall percent = ", sprintf("%.1f%%", overall_percent)),
        paste0("bar N = ", bar_n),
        paste0("bar percent of total = ", sprintf("%.1f%%", bar_percent)),
        paste0("bar width mode = ", bar_width),
        sep = "\n"
      )
      segments[[length(segments) + 1L]] <- data.frame(
        x_condition = condition,
        y_level = level,
        count = count,
        bar_n = bar_n,
        total_n = total_n,
        conditional_percent = conditional_percent,
        overall_percent = overall_percent,
        bar_percent = bar_percent,
        bar_width_value = width_value,
        row_ids = I(list(ids)),
        tooltip = tooltip,
        stringsAsFactors = FALSE
      )
    }
  }
  segments <- if (length(segments)) do.call(rbind, segments) else data.frame(
    x_condition = character(), y_level = character(), count = integer(),
    bar_n = integer(), total_n = integer(), conditional_percent = numeric(),
    overall_percent = numeric(), bar_percent = numeric(), bar_width_value = numeric(),
    row_ids = I(list()), tooltip = character(), stringsAsFactors = FALSE
  )

  bars$visual_width <- .rls_barplot_widths(bars$bar_width_value, bar_width)
  visual <- .rls_default_barplot_visual_state(y_levels)
  list(
    x = x,
    y = y %||% "",
    group = group,
    mode = mode,
    bar_width = bar_width,
    include_missing = isTRUE(include_missing),
    sort_x = isTRUE(sort_x),
    sort_y = isTRUE(sort_y),
    total_n = total_n,
    excluded_row_ids = as.integer(excluded_rows),
    y_level_colors = visual$y_level_colors,
    y_level_alpha = visual$y_level_alpha,
    y_level_patterns = visual$y_level_patterns,
    segment_color_overrides = visual$segment_color_overrides,
    segment_alpha_overrides = visual$segment_alpha_overrides,
    segment_pattern_overrides = visual$segment_pattern_overrides,
    default_segment_alpha = visual$default_segment_alpha,
    show_patterns = visual$show_patterns,
    segment_encoding_mode = visual$segment_encoding_mode,
    bars = bars,
    segments = segments
  )
}

.rls_ordered_barplot_x_conditions <- function(x_condition, x_by_row, sort_x = FALSE) {
  x_levels <- unique(x_condition)
  if (length(x_by_row) <= 1L) {
    if (isTRUE(sort_x)) x_levels <- sort(x_levels)
    return(x_levels)
  }

  first_index <- !duplicated(x_condition)
  condition_labels <- x_condition[first_index]
  condition_values <- lapply(x_by_row, function(value) value[first_index])
  level_orders <- lapply(x_by_row, function(value) {
    levels <- unique(value)
    if (isTRUE(sort_x)) sort(levels) else levels
  })
  sort_keys <- lapply(seq_along(condition_values), function(i) {
    match(condition_values[[i]], level_orders[[i]])
  })
  condition_labels[do.call(order, c(sort_keys, list(seq_along(condition_labels))))]
}

.rls_barplot_widths <- function(width_value, bar_width = c("equal", "proportional_n", "proportional_percent"),
                                available_width = 1) {
  bar_width <- match.arg(bar_width)
  n <- length(width_value)
  if (!n) return(numeric())
  if (identical(bar_width, "equal")) {
    return(rep(available_width / n, n))
  }
  total <- sum(width_value)
  if (!is.finite(total) || total <= 0) {
    return(rep(available_width / n, n))
  }
  available_width * width_value / total
}

.rls_default_barplot_visual_state <- function(y_levels,
                                              default_segment_alpha = 0.70,
                                              show_patterns = FALSE,
                                              segment_encoding_mode = "transparent_color_only") {
  palette <- c("white")
  patterns <- c("diagonal_slash", "dots", "crosshatch", "vertical", "horizontal", "diagonal_backslash")
  if (!length(y_levels)) {
    y_levels <- "All"
  }
  idx <- seq_along(y_levels)
  y_level_colors <- setNames(palette[((idx - 1L) %% length(palette)) + 1L], y_levels)
  y_level_alpha <- setNames(rep(default_segment_alpha, length(y_levels)), y_levels)
  y_level_patterns <- setNames(patterns[((idx - 1L) %% length(patterns)) + 1L], y_levels)
  list(
    y_level_colors = y_level_colors,
    y_level_alpha = y_level_alpha,
    y_level_patterns = y_level_patterns,
    segment_color_overrides = character(),
    segment_alpha_overrides = numeric(),
    segment_pattern_overrides = character(),
    default_segment_alpha = default_segment_alpha,
    show_patterns = isTRUE(show_patterns),
    segment_encoding_mode = segment_encoding_mode
  )
}

.rls_barplot_selection_summary <- function(prepared, selected_rows = integer()) {
  if (!is.list(prepared) || is.null(prepared$bars) || is.null(prepared$segments)) {
    stop("`prepared` must be a barplot data object.", call. = FALSE)
  }
  selected_rows <- unique(as.integer(selected_rows))
  selected_rows <- selected_rows[!is.na(selected_rows)]

  summarize_rows <- function(rows) {
    rows <- as.integer(rows)
    selected <- intersect(rows, selected_rows)
    list(
      selected_count = length(selected),
      total_count = length(rows),
      selected_fraction = if (length(rows)) length(selected) / length(rows) else 0,
      selected_row_ids = selected
    )
  }

  bars <- prepared$bars
  bar_summary <- lapply(seq_len(nrow(bars)), function(i) {
    summary <- summarize_rows(bars$row_ids[[i]])
    data.frame(
      x_condition = bars$x_condition[[i]],
      selected_count = summary$selected_count,
      total_count = summary$total_count,
      selected_fraction = summary$selected_fraction,
      selected_row_ids = I(list(summary$selected_row_ids)),
      stringsAsFactors = FALSE
    )
  })
  bar_summary <- if (length(bar_summary)) do.call(rbind, bar_summary) else data.frame(
    x_condition = character(), selected_count = integer(), total_count = integer(),
    selected_fraction = numeric(), selected_row_ids = I(list()), stringsAsFactors = FALSE
  )

  segments <- prepared$segments
  segment_summary <- lapply(seq_len(nrow(segments)), function(i) {
    summary <- summarize_rows(segments$row_ids[[i]])
    data.frame(
      x_condition = segments$x_condition[[i]],
      y_level = segments$y_level[[i]],
      selected_count = summary$selected_count,
      total_count = summary$total_count,
      selected_fraction = summary$selected_fraction,
      selected_row_ids = I(list(summary$selected_row_ids)),
      stringsAsFactors = FALSE
    )
  })
  segment_summary <- if (length(segment_summary)) do.call(rbind, segment_summary) else data.frame(
    x_condition = character(), y_level = character(), selected_count = integer(),
    total_count = integer(), selected_fraction = numeric(),
    selected_row_ids = I(list()), stringsAsFactors = FALSE
  )

  list(bars = bar_summary, segments = segment_summary)
}

.rls_barplot_clean_field <- function(x) {
  gsub("[\r\n\t|]", " ", x)
}

.rls_barplot_rows_field <- function(rows) {
  paste(as.integer(rows), collapse = ",")
}

.rls_barplot_payload_lines <- function(prepared) {
  lines <- c(
    as.character(length(prepared$excluded_row_ids)),
    as.character(prepared$excluded_row_ids),
    as.character(nrow(prepared$bars))
  )
  for (i in seq_len(nrow(prepared$bars))) {
    bar <- prepared$bars[i, , drop = FALSE]
    bar_rows <- bar$row_ids[[1L]]
    lines <- c(lines, paste(
      .rls_barplot_clean_field(bar$x_condition),
      as.integer(bar$bar_n),
      sprintf("%.17g", bar$bar_percent),
      sprintf("%.17g", bar$bar_width_value),
      .rls_barplot_rows_field(bar_rows),
      sep = "\t"
    ))
    seg <- prepared$segments[prepared$segments$x_condition == bar$x_condition, , drop = FALSE]
    lines <- c(lines, as.character(nrow(seg)))
    if (nrow(seg)) {
      for (j in seq_len(nrow(seg))) {
        rows <- seg$row_ids[[j]]
        lines <- c(lines, paste(
          .rls_barplot_clean_field(seg$y_level[[j]]),
          as.integer(seg$count[[j]]),
          as.integer(seg$bar_n[[j]]),
          as.integer(seg$total_n[[j]]),
          sprintf("%.17g", seg$conditional_percent[[j]]),
          sprintf("%.17g", seg$overall_percent[[j]]),
          sprintf("%.17g", seg$bar_percent[[j]]),
          sprintf("%.17g", seg$bar_width_value[[j]]),
          .rls_barplot_rows_field(rows),
          sep = "\t"
        ))
      }
    }
  }
  lines
}

#' Open a native linked bar chart
#'
#' @param data A data frame.
#' @param x One or more X condition variables.
#' @param y Optional stacked segment variable.
#' @param group Optional linked brushing group.
#' @param mode Vertical scale: conditional percent, raw count, or overall percent.
#' @param bar_width Equal width or proportional width.
#' @param include_missing Include missing X/Y values as `NA`.
#' @param sort_x,sort_y Sort X conditions or Y levels.
#' @param title Optional plot title.
#' @param linked Logical; if `TRUE`, use the supplied group for linked brushing.
#' @return An `rlispstat_plot` object with type `"barplot"`.
#' @export
ls_barplot <- function(data, x, y = NULL, group = NULL,
                       mode = c("conditional_percent", "count", "overall_percent"),
                       bar_width = c("equal", "proportional_n", "proportional_percent"),
                       include_missing = TRUE, sort_x = FALSE, sort_y = FALSE,
                       title = NULL, linked = TRUE) {
  prepared <- .rls_prepare_barplot_data(
    data = data, x = x, y = y, group = group, mode = mode, bar_width = bar_width,
    include_missing = include_missing, sort_x = sort_x, sort_y = sort_y
  )
  plot_id <- paste0("barplot_", format(Sys.time(), "%Y%m%d%H%M%OS3"), "_", sample.int(1e6, 1L))
  plot_group <- if (isTRUE(linked)) prepared$group %||% plot_id else plot_id
  if (is.null(title)) {
    title <- if (nzchar(prepared$y)) {
      paste("Bar chart of", paste(prepared$x, collapse = " + "), "by", prepared$y)
    } else {
      paste("Bar chart of", paste(prepared$x, collapse = " + "))
    }
  }
  title <- .rls_validate_protocol_name(title, "title")

  .rls_start_backend()
  .rls_send(c(
    "ADD_BARPLOT",
    plot_id,
    plot_group,
    paste(prepared$x, collapse = " + "),
    prepared$y,
    title,
    prepared$mode,
    prepared$bar_width,
    prepared$y,
    as.character(prepared$total_n),
    .rls_barplot_payload_lines(prepared),
    .rls_variable_payload(data),
    .rls_dataframe_payload(data)
  ))
  rows <- sort(unique(unlist(prepared$bars$row_ids, use.names = FALSE)))
  .rls_register_plot(
    plot_id, plot_group, paste(prepared$x, collapse = " + "), prepared$y,
    data, rows, type = "barplot", title = title,
    mode = prepared$mode, bar_width = prepared$bar_width,
    include_missing = prepared$include_missing, sort_x = prepared$sort_x,
    sort_y = prepared$sort_y, bars = prepared$bars, segments = prepared$segments,
    excluded_row_ids = prepared$excluded_row_ids
  )
  structure(
    list(id = plot_id, group = plot_group, x = prepared$x, y = prepared$y,
         title = title, type = "barplot", mode = prepared$mode,
         bar_width = prepared$bar_width),
    class = "rlispstat_plot"
  )
}

#' @rdname ls_barplot
#' @param name Optional plot title.
#' @export
ls_new_barplot <- function(data = NULL, x = NULL, y = NULL,
                           mode = c("conditional_percent", "count", "overall_percent"),
                           bar_width = c("equal", "proportional_n", "proportional_percent"),
                           include_missing = TRUE,
                           sort_x = FALSE,
                           sort_y = FALSE,
                           name = NULL) {
  group <- NULL
  record <- NULL
  if (is.null(data) || (is.character(data) && length(data) == 1L && data %in% ls(envir = .rls_state$datasets))) {
    record <- .rls_dataset_record(data)
    data <- record$data
    group <- record$group
  } else if (!is.data.frame(data)) {
    stop("`data` must be a data frame, a registered dataset name, or NULL for the active dataset.", call. = FALSE)
  }
  if (is.null(x)) {
    x <- names(data)[[1L]]
  }
  if (!is.null(record)) {
    .rls_mi_warn_current_version(record, "Bar chart")
    if (is.null(name) && identical(record$dataset_type %||% "data_frame", "multiple_imputation")) {
      name <- paste(
        if (is.null(y)) paste("Bar chart of", paste(x, collapse = " + "))
        else paste("Bar chart of", paste(x, collapse = " + "), "by", y),
        .rls_mi_title_suffix(record)
      )
    }
  }
  ls_barplot(
    data = data, x = x, y = y, group = group, mode = mode, bar_width = bar_width,
    include_missing = include_missing, sort_x = sort_x, sort_y = sort_y,
    title = name, linked = TRUE
  )
}
