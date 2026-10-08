.rls_trellis_levels <- function(value, rows) {
  observed <- value[rows]
  if (is.factor(value)) {
    return(levels(value)[levels(value) %in% as.character(observed)])
  }
  if (is.numeric(value)) {
    return(as.character(sort(unique(as.double(observed)))))
  }
  sort(unique(as.character(observed)), method = "radix")
}

.rls_order_trellis_levels <- function(levels, order = "defined") {
  if (identical(order, "defined")) return(levels)
  numeric_levels <- suppressWarnings(as.double(levels))
  index <- if (all(is.finite(numeric_levels))) {
    order(numeric_levels, method = "radix")
  } else {
    order(levels, method = "radix")
  }
  if (identical(order, "descending")) index <- rev(index)
  levels[index]
}

.rls_trellis_columns <- function(panel_count, width, height, mode = "automatic") {
  if (identical(mode, "one_row")) return(panel_count)
  if (identical(mode, "one_column")) return(1L)
  if (identical(mode, "grid")) return(as.integer(ceiling(sqrt(panel_count))))
  if (identical(mode, "two_columns")) return(min(2L, panel_count))
  if (identical(mode, "three_columns")) return(min(3L, panel_count))
  preferred <- if (panel_count <= 3L) panel_count else if (panel_count == 4L) 2L else
    if (panel_count <= 6L) 3L else if (panel_count <= 8L) 4L else
      if (panel_count == 9L) 3L else if (panel_count <= 12L) 4L else ceiling(sqrt(panel_count))
  available_width <- max(10, width * 72 - 70)
  available_height <- max(10, height * 72 - 82)
  if (panel_count == 3L) {
    plot_aspect <- available_width / available_height
    three_panel_width <- (available_width - 32) / 3
    if (plot_aspect >= 1.15 && three_panel_width >= 120) return(3L)
    if (plot_aspect <= 0.75) return(1L)
  }
  candidates <- seq_len(panel_count)
  scores <- vapply(candidates, function(columns) {
    rows <- ceiling(panel_count / columns)
    frame_width <- (available_width - 16 * (columns - 1)) / columns
    frame_height <- (available_height - 16 * (rows - 1)) / rows
    plot_height <- max(1, frame_height - 22)
    aspect <- max(0.05, frame_width / plot_height)
    empty <- rows * columns - panel_count
    score <- 2.2 * empty + 0.55 * abs(columns - preferred) +
      2.8 * abs(log(aspect / 1.25))
    if (frame_width < 170) score <- score + (170 - frame_width) * 0.10
    if (plot_height < 120) score <- score + (120 - plot_height) * 0.12
    score
  }, numeric(1L))
  candidates[[which.min(scores)]]
}

.rls_trellis_condition_names <- function(record) {
  conditions <- record$conditions %||% list(list(variable = record$condition))
  vapply(conditions, function(x) x$variable, character(1L))
}

.rls_trellis_derived_title <- function(record) {
  conditions <- .rls_trellis_condition_names(record)
  suffix <- if (!length(conditions)) "" else if (length(conditions) < 3L) {
    paste0(", conditioned by ", paste(conditions, collapse = " and "))
  } else {
    paste0(", conditioned by ", length(conditions), " variables")
  }
  switch(record$plot_type %||% "scatter",
    scatter = paste0(record$y, " by ", record$x, suffix),
    boxplot = paste0(record$y, " by ",
                     paste(record$boxplot_groups %||% record$x, collapse = " + "), suffix),
    bar = paste0(record$x, suffix),
    histogram = paste0("Distribution of ", record$x, suffix),
    paste0(record$y, " by ", record$x, suffix)
  )
}

.rls_validate_trellis_condition <- function(value, row_count = length(value)) {
  supported <- is.factor(value) || is.character(value) || is.logical(value) ||
    (is.numeric(value) && !inherits(value, c("Date", "POSIXt")))
  if (!supported) {
    stop("`by` must name a categorical or discrete numeric column.", call. = FALSE)
  }
  observed <- value[!is.na(value)]
  levels <- length(unique(observed))
  if (levels < 2L) stop("`by` must have at least two observed categories.", call. = FALSE)
  if (levels > 20L) stop("`by` has too many categories for a trellis plot (maximum 20).", call. = FALSE)
  if (is.numeric(value) && levels > max(3L, ceiling(row_count / 4))) {
    stop("A continuous `by` variable must be discrete and have relatively few categories.", call. = FALSE)
  }
  invisible(TRUE)
}

.rls_prepare_trellis_scatterplot_data <- function(data, x, y, by) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  x <- .rls_validate_protocol_name(x, "x")
  y <- .rls_validate_protocol_name(y, "y")
  by <- .rls_validate_protocol_name(by, "by")
  if (identical(x, y)) stop("`x` and `y` must name different variables.", call. = FALSE)
  missing_names <- setdiff(c(x, y, by), names(data))
  if (length(missing_names)) {
    stop(sprintf("Column `%s` was not found.", missing_names[[1L]]), call. = FALSE)
  }
  numeric_ok <- function(value) is.numeric(value) && !inherits(value, c("Date", "POSIXt"))
  if (!numeric_ok(data[[x]]) || !numeric_ok(data[[y]])) {
    stop("`x` and `y` must name numeric columns.", call. = FALSE)
  }
  .rls_validate_trellis_condition(data[[by]], nrow(data))
  x_values <- as.double(data[[x]])
  y_values <- as.double(data[[y]])
  rows <- which(is.finite(x_values) & is.finite(y_values) & !is.na(data[[by]]))
  if (!length(rows)) {
    stop("No complete finite X, Y, and conditioning cases are available.", call. = FALSE)
  }
  panel_values <- as.character(data[[by]][rows])
  panel_levels <- .rls_trellis_levels(data[[by]], rows)
  panel_indices <- match(panel_values, panel_levels) - 1L
  list(x = x, y = y, by = by, x_values = x_values, y_values = y_values,
       rows = rows, panel_levels = gsub("[\r\n\t|]+", " ", panel_levels),
       panel_indices = panel_indices)
}

#' Open an interactive trellis scatterplot
#'
#' Draws a two-dimensional scatterplot in one panel per level of a required
#' conditioning variable. All panels share X and Y scales and case selection.
#'
#' @param data A data frame.
#' @param x,y Names of different numeric columns.
#' @param by Name of a factor-like or discrete numeric conditioning column.
#' @param group Optional dataset/group id used for linked selection.
#' @param labels Optional case labels.
#' @param title Optional plot title.
#' @param layout Initial panel layout preference.
#' @param panel_order Initial visual order of conditioning levels.
#' @return An object of class `rlispstat_plot`.
#' @export
#' @examples
#' \dontrun{
#' ls_trellis_scatterplot(mtcars, "wt", "mpg", "cyl", group = "cars")
#' }
ls_trellis_scatterplot <- function(data, x, y, by, group = NULL,
                                   labels = NULL, title = NULL,
                                   layout = c("automatic", "one_row", "one_column",
                                              "grid", "two_columns", "three_columns"),
                                   panel_order = c("defined", "ascending", "descending")) {
  layout <- match.arg(layout)
  panel_order <- match.arg(panel_order)
  prepared <- .rls_prepare_trellis_scatterplot_data(data, x, y, by)
  if (!is.null(labels) && length(labels) != nrow(data)) {
    stop("`labels` must have one value per row in `data`.", call. = FALSE)
  }
  group <- .rls_validate_group(group, allow_null = TRUE)
  plot_id <- paste0("trellis_scatterplot_", format(Sys.time(), "%Y%m%d%H%M%OS3"),
                    "_", sample.int(1e6, 1L))
  plot_group <- group %||% plot_id
  custom_title <- !is.null(title)
  title <- title %||% paste0(prepared$y, " by ", prepared$x,
                             ", conditioned by ", prepared$by)
  title <- .rls_validate_protocol_name(title, "title")
  point_lines <- sprintf("%.17g %.17g %d %d",
                         prepared$x_values[prepared$rows],
                         prepared$y_values[prepared$rows],
                         prepared$rows, prepared$panel_indices)
  lines <- c(
    "ADD_PLOT", plot_id, plot_group, prepared$x, prepared$y, title,
    as.character(length(prepared$rows)), point_lines,
    "TRELLIS_SCATTERPLOT", prepared$by,
    as.character(length(prepared$panel_levels)), prepared$panel_levels,
    "TRELLIS_SCATTERPLOT_OPTIONS", layout, panel_order
  )
  if (.Platform$OS.type == "windows") {
    .rls_send_winui(lines)
  } else {
    .rls_start_backend()
    lines <- c(lines, .rls_variable_payload(data), .rls_dataframe_payload(data))
    .rls_send(lines)
  }
  .rls_register_plot(plot_id, plot_group, prepared$x, prepared$y, data,
                     prepared$rows, type = "trellis_scatterplot", title = title,
                     condition = prepared$by, layout = layout,
                     panel_order = panel_order, plot_type = "scatter",
                     conditions = list(list(variable = prepared$by,
                                            kind = "categorical", dimension = "columns")),
                     custom_title = custom_title,
                     scale_mode = "common_xy")
  structure(list(id = plot_id, group = plot_group, x = prepared$x, y = prepared$y,
                 condition = prepared$by, title = title), class = "rlispstat_plot")
}

#' Create a trellis scatterplot from a registered dataset
#'
#' @param group Registered dataset name. The active dataset is used when `NULL`.
#' @inheritParams ls_trellis_scatterplot
#' @param linked Whether selections should use the registered dataset group.
#' @return An object of class `rlispstat_plot`.
#' @export
ls_new_trellis_scatterplot <- function(group = NULL, x = NULL, y = NULL, by = NULL,
                                       labels = NULL, title = NULL, linked = TRUE,
                                       layout = c("automatic", "one_row", "one_column",
                                                  "grid", "two_columns", "three_columns"),
                                       panel_order = c("defined", "ascending", "descending")) {
  record <- .rls_dataset_record(group)
  numeric <- .rls_numeric_variable_names(record$data, record$variable_metadata)
  if (length(numeric) < 2L && (is.null(x) || is.null(y))) {
    stop("The dataset needs at least two numeric variables.", call. = FALSE)
  }
  x <- x %||% numeric[[1L]]
  y <- y %||% numeric[[2L]]
  if (is.null(by)) stop("`by` is required.", call. = FALSE)
  plot_group <- if (isTRUE(linked)) record$group else
    paste0(record$group, "_", format(Sys.time(), "%Y%m%d%H%M%OS3"))
  ls_trellis_scatterplot(record$data, x, y, by, plot_group, labels, title,
                         layout = layout, panel_order = panel_order)
}

.rls_set_trellis_variable <- function(plot, value, role) {
  id <- .rls_plot_id(plot)
  record <- .rls_plot_record(plot)
  if (!identical(record$type, "trellis_scatterplot")) {
    stop("`plot` is not a trellis scatterplot.", call. = FALSE)
  }
  candidate <- record
  candidate[[role]] <- value
  .rls_prepare_trellis_scatterplot_data(candidate$data, candidate$x,
                                        candidate$y, candidate$condition)
  command <- switch(role, x = "TRELLIS_SCATTERPLOT_SET_X",
                    y = "TRELLIS_SCATTERPLOT_SET_Y",
                    condition = "TRELLIS_SCATTERPLOT_SET_CONDITION")
  .rls_send(c(command, id, value))
  record[[role]] <- value
  if (identical(role, "condition")) {
    record$conditions[[1L]]$variable <- value
  }
  if (!isTRUE(record$custom_title)) record$title <- .rls_trellis_derived_title(record)
  assign(id, record, envir = .rls_state$plots)
  invisible(plot)
}

#' Change a trellis scatterplot variable
#'
#' @param plot A trellis scatterplot or plot id.
#' @param variable A column name.
#' @return Invisibly returns `plot`.
#' @export
ls_trellis_scatterplot_x <- function(plot, variable) {
  .rls_set_trellis_variable(plot, variable, "x")
}

#' @rdname ls_trellis_scatterplot_x
#' @export
ls_trellis_scatterplot_y <- function(plot, variable) {
  .rls_set_trellis_variable(plot, variable, "y")
}

#' @rdname ls_trellis_scatterplot_x
#' @export
ls_trellis_scatterplot_condition <- function(plot, variable) {
  .rls_set_trellis_variable(plot, variable, "condition")
}

#' Change trellis panel layout or order
#'
#' @param plot A trellis scatterplot or plot id.
#' @param layout One of `"automatic"`, `"one_row"`, `"one_column"`,
#'   `"grid"`, `"two_columns"`, or `"three_columns"`.
#' @param panel_order One of `"defined"`, `"ascending"`, or `"descending"`.
#' @return Invisibly returns `plot`.
#' @export
ls_trellis_scatterplot_display <- function(
    plot,
    layout = NULL,
    panel_order = NULL) {
  id <- .rls_plot_id(plot)
  record <- .rls_plot_record(plot)
  if (!identical(record$type, "trellis_scatterplot")) {
    stop("`plot` is not a trellis scatterplot.", call. = FALSE)
  }
  if (!is.null(layout)) {
    layout <- match.arg(layout, c("automatic", "one_row", "one_column",
                                  "grid", "two_columns", "three_columns"))
    .rls_send(c("TRELLIS_SCATTERPLOT_SET_LAYOUT", id, layout))
    record$layout <- layout
  }
  if (!is.null(panel_order)) {
    panel_order <- match.arg(panel_order, c("defined", "ascending", "descending"))
    .rls_send(c("TRELLIS_SCATTERPLOT_SET_ORDER", id, panel_order))
    record$panel_order <- panel_order
  }
  assign(id, record, envir = .rls_state$plots)
  invisible(plot)
}

#' Change a trellis plot type
#'
#' @param type One of `"scatter"`, `"boxplot"`, `"bar"`, or `"histogram"`.
#' @param x Optional compatible X variable to apply atomically with the new
#'   type. For boxplots this may be an ordered vector of categorical variables,
#'   from the outermost to the innermost grouping level.
#' @param y Optional numeric Y variable for scatterplots or boxplots.
#' @rdname ls_trellis_scatterplot_x
#' @export
ls_trellis_plot_type <- function(plot, type, x = NULL, y = NULL) {
  id <- .rls_plot_id(plot)
  record <- .rls_plot_record(plot)
  type <- match.arg(type, c("scatter", "boxplot", "bar", "histogram"))
  if (!is.null(y)) {
    y <- .rls_validate_protocol_name(y, "y")
    .rls_send(c("TRELLIS_SCATTERPLOT_SET_Y", id, y))
    record$y <- y
  }
  if (!is.null(x)) {
    if (!is.character(x) || !length(x) || anyNA(x) || anyDuplicated(x)) {
      stop("`x` must contain unique variable names.", call. = FALSE)
    }
    x <- vapply(x, function(value) .rls_validate_protocol_name(value, "x"), character(1L))
    if (!identical(type, "boxplot") && length(x) != 1L) {
      stop("Multiple `x` variables are only available for trellis boxplots.", call. = FALSE)
    }
    missing_x <- setdiff(x, names(record$data))
    if (length(missing_x)) stop(sprintf("Unknown X variable `%s`.", missing_x[[1L]]), call. = FALSE)
    .rls_send(c("TRELLIS_SCATTERPLOT_SET_TYPE_WITH_X", id, type, x[[1L]]))
    if (identical(type, "boxplot")) {
      .rls_send(c("TRELLIS_SCATTERPLOT_SET_BOXPLOT_GROUPS", id,
                  as.character(length(x)), x))
      record$boxplot_groups <- x
    } else {
      record$boxplot_groups <- NULL
    }
    record$x <- x[[1L]]
  } else {
    .rls_send(c("TRELLIS_SCATTERPLOT_SET_TYPE", id, type))
    if (identical(type, "boxplot") && is.null(record$boxplot_groups))
      record$boxplot_groups <- record$x
  }
  record$plot_type <- type
  if (!isTRUE(record$custom_title)) record$title <- .rls_trellis_derived_title(record)
  assign(id, record, envir = .rls_state$plots)
  invisible(plot)
}

#' Add, remove, or reorder trellis conditioning variables
#'
#' @param variable Dataset column name.
#' @param kind `"categorical"` or `"continuous"`.
#' @param method `"equal_width"` or `"equal_count"` for continuous conditioning.
#' @param bins Number of continuous intervals, from 2 through 6.
#' @param direction `"earlier"` or `"later"`.
#' @rdname ls_trellis_scatterplot_x
#' @export
ls_trellis_condition_add <- function(plot, variable,
                                     kind = c("categorical", "continuous"),
                                     method = c("equal_width", "equal_count"), bins = 4L) {
  id <- .rls_plot_id(plot)
  record <- .rls_plot_record(plot)
  kind <- match.arg(kind)
  method <- match.arg(method)
  variable <- .rls_validate_protocol_name(variable, "variable")
  if (!variable %in% names(record$data)) stop("Unknown conditioning variable.", call. = FALSE)
  if (variable %in% .rls_trellis_condition_names(record)) {
    stop("The variable is already used for conditioning.", call. = FALSE)
  }
  bins <- as.integer(bins)
  if (length(bins) != 1L || is.na(bins) || bins < 2L || bins > 6L) {
    stop("`bins` must be an integer from 2 through 6.", call. = FALSE)
  }
  command <- if (identical(kind, "categorical")) {
    "TRELLIS_SCATTERPLOT_ADD_CONDITION_CATEGORICAL"
  } else if (identical(method, "equal_count")) {
    "TRELLIS_SCATTERPLOT_ADD_CONDITION_EQUAL_COUNT"
  } else {
    "TRELLIS_SCATTERPLOT_ADD_CONDITION_EQUAL_WIDTH"
  }
  .rls_send(c(command, id, variable))
  if (identical(kind, "continuous") && bins != 4L) {
    .rls_send(c("TRELLIS_SCATTERPLOT_CONDITION_BINS", id, variable, as.character(bins)))
  }
  record$conditions <- c(record$conditions %||% list(list(variable = record$condition,
                                                           kind = "categorical", dimension = "columns")),
                         list(list(variable = variable, kind = kind,
                                   method = method, bins = bins,
                                   dimension = if (length(record$conditions %||% list()) < 2L) "rows" else "nested")))
  if (!isTRUE(record$custom_title)) record$title <- .rls_trellis_derived_title(record)
  assign(id, record, envir = .rls_state$plots)
  invisible(plot)
}

#' @rdname ls_trellis_scatterplot_x
#' @export
ls_trellis_condition_remove <- function(plot, variable) {
  id <- .rls_plot_id(plot)
  record <- .rls_plot_record(plot)
  conditions <- record$conditions %||% list(list(variable = record$condition, kind = "categorical"))
  keep <- vapply(conditions, function(x) !identical(x$variable, variable), logical(1L))
  if (all(keep)) stop("Conditioning variable not found.", call. = FALSE)
  if (sum(keep) < 1L) stop("A trellis plot must retain one conditioning variable.", call. = FALSE)
  .rls_send(c("TRELLIS_SCATTERPLOT_REMOVE_CONDITION", id, variable))
  record$conditions <- conditions[keep]
  record$condition <- record$conditions[[1L]]$variable
  if (!isTRUE(record$custom_title)) record$title <- .rls_trellis_derived_title(record)
  assign(id, record, envir = .rls_state$plots)
  invisible(plot)
}

#' @rdname ls_trellis_scatterplot_x
#' @export
ls_trellis_condition_move <- function(plot, variable, direction = c("earlier", "later")) {
  id <- .rls_plot_id(plot)
  record <- .rls_plot_record(plot)
  direction <- match.arg(direction)
  conditions <- record$conditions %||% list(list(variable = record$condition, kind = "categorical"))
  index <- match(variable, vapply(conditions, `[[`, character(1L), "variable"))
  if (is.na(index)) stop("Conditioning variable not found.", call. = FALSE)
  other <- index + if (identical(direction, "earlier")) -1L else 1L
  if (other < 1L || other > length(conditions)) return(invisible(plot))
  .rls_send(c(paste0("TRELLIS_SCATTERPLOT_MOVE_CONDITION_", toupper(direction)), id, variable))
  conditions[c(index, other)] <- conditions[c(other, index)]
  record$conditions <- conditions
  record$condition <- conditions[[1L]]$variable
  if (!isTRUE(record$custom_title)) record$title <- .rls_trellis_derived_title(record)
  assign(id, record, envir = .rls_state$plots)
  invisible(plot)
}

#' @param dimension One of `"rows"`, `"columns"`, or `"nested"`.
#' @rdname ls_trellis_scatterplot_x
#' @export
ls_trellis_condition_dimension <- function(plot, variable,
                                           dimension = c("rows", "columns", "nested")) {
  id <- .rls_plot_id(plot)
  record <- .rls_plot_record(plot)
  dimension <- match.arg(dimension)
  conditions <- record$conditions %||%
    list(list(variable = record$condition, kind = "categorical", dimension = "columns"))
  index <- match(variable, vapply(conditions, `[[`, character(1L), "variable"))
  if (is.na(index)) stop("Conditioning variable not found.", call. = FALSE)
  if (!identical(dimension, "nested")) {
    for (i in seq_along(conditions)) {
      if (i != index && identical(conditions[[i]]$dimension %||% "nested", dimension)) {
        conditions[[i]]$dimension <- "nested"
      }
    }
  }
  .rls_send(c(paste0("TRELLIS_SCATTERPLOT_DIMENSION_", toupper(dimension)), id, variable))
  conditions[[index]]$dimension <- dimension
  record$conditions <- conditions
  assign(id, record, envir = .rls_state$plots)
  invisible(plot)
}

#' @rdname ls_trellis_scatterplot_x
#' @export
ls_trellis_swap_dimensions <- function(plot) {
  id <- .rls_plot_id(plot)
  record <- .rls_plot_record(plot)
  .rls_send(c("TRELLIS_SCATTERPLOT_SWAP_DIMENSIONS", id))
  conditions <- record$conditions %||% list()
  for (i in seq_along(conditions)) {
    current <- conditions[[i]]$dimension %||% "nested"
    conditions[[i]]$dimension <- if (identical(current, "rows")) "columns" else
      if (identical(current, "columns")) "rows" else current
  }
  record$conditions <- conditions
  assign(id, record, envir = .rls_state$plots)
  invisible(plot)
}
