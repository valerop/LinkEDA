.rls_prepare_time_series_data <- function(data, time, value, series = NULL) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  time <- .rls_validate_protocol_name(time, "time")
  value <- .rls_validate_protocol_name(value, "value")
  if (!time %in% names(data)) stop(sprintf("Column `%s` was not found.", time), call. = FALSE)
  if (!value %in% names(data)) stop(sprintf("Column `%s` was not found.", value), call. = FALSE)
  if (!is.numeric(data[[value]])) stop("`value` must name a numeric column.", call. = FALSE)
  time_column <- data[[time]]
  time_type <- if (inherits(time_column, "Date")) "date" else
    if (inherits(time_column, c("POSIXct", "POSIXlt"))) "datetime" else
      if (is.numeric(time_column)) {
        observed <- time_column[is.finite(time_column)]
        if (length(observed) && all(observed >= 1000 & observed <= 3000) &&
            all(abs(observed - round(observed)) < sqrt(.Machine$double.eps))) "year" else "numeric"
      } else
        stop("`time` must name a numeric, Date, or POSIXt column.", call. = FALSE)
  time_values <- as.double(time_column)
  value_values <- as.double(data[[value]])
  rows <- which(is.finite(time_values) & is.finite(value_values))
  if (!length(rows)) stop("No complete time/value pairs are available.", call. = FALSE)
  series_name <- ""
  if (is.null(series)) {
    labels <- value
    point_series <- rep.int(0L, length(rows))
  } else {
    series <- .rls_validate_protocol_name(series, "series")
    if (!series %in% names(data)) stop(sprintf("Column `%s` was not found.", series), call. = FALSE)
    series_name <- series
    raw_groups <- as.character(data[[series]][rows])
    missing_group <- is.na(raw_groups) | !nzchar(raw_groups)
    observed <- raw_groups[!missing_group]
    labels <- if (is.factor(data[[series]])) {
      levels(data[[series]])[levels(data[[series]]) %in% observed]
    } else unique(observed)
    point_series <- match(raw_groups, labels) - 1L
    if (any(missing_group)) {
      missing_label <- if ("(missing)" %in% labels) "(missing values)" else "(missing)"
      labels <- c(labels, missing_label)
      point_series[missing_group] <- length(labels) - 1L
    }
    # Group identities are determined before preparing safe display labels.
    # Reserve existing suffixes too, so e.g. A|B, A B and A B #1 stay distinct.
    labels <- make.unique(gsub("[\r\n\t|]+", " ", labels), sep = " #")
  }
  list(time = time, value = value, time_type = time_type,
       time_values = time_values, value_values = value_values, rows = rows,
       series_name = series_name, labels = gsub("[\r\n\t|]+", " ", labels),
       point_series = point_series)
}

#' Open a native interactive time-series plot
#'
#' Draws observations in time order. When `series` is supplied, one line is
#' drawn for each distinct value while cases remain linked to other rlispstat
#' views through `group`.
#'
#' @param data A data frame.
#' @param time Name of a numeric, `Date`, or `POSIXt` time column.
#' @param value Name of a numeric value column.
#' @param series Optional column identifying separate series.
#' @param group Optional dataset/group id used for linked selection.
#' @param title Optional plot title.
#' @param identification How grouped series are identified: a movable legend,
#'   labels at the start of each series, or no identification.
#' @param legend_position Position of the legend inside the plotting area.
#' @return An object of class `rlispstat_plot`.
#' @export
#' @examples
#' \dontrun{
#' ls_time_series(airquality, time = "Day", value = "Temp", series = "Month")
#' }
ls_time_series <- function(data, time, value, series = NULL, group = NULL, title = NULL,
                           identification = c("legend", "start_labels", "none"),
                           legend_position = c("top_right", "top_left", "bottom_right", "bottom_left", "left", "right", "top", "bottom")) {
  identification <- match.arg(identification)
  legend_position <- match.arg(legend_position)
  prepared <- .rls_prepare_time_series_data(data, time, value, series)
  time <- prepared$time
  value <- prepared$value
  time_type <- prepared$time_type
  time_values <- prepared$time_values
  value_values <- prepared$value_values
  rows <- prepared$rows
  series_name <- prepared$series_name
  labels <- prepared$labels
  point_series <- prepared$point_series
  group <- .rls_validate_group(group, allow_null = TRUE)
  plot_id <- paste0("timeseries_", format(Sys.time(), "%Y%m%d%H%M%OS3"), "_", sample.int(1e6, 1L))
  plot_group <- group %||% plot_id
  if (is.null(title)) title <- paste(value, "over", time)
  title <- .rls_validate_protocol_name(title, "title")

  point_lines <- sprintf("%.17g %.17g %d %d",
                         time_values[rows], value_values[rows], rows, point_series)
  lines <- c(
    "ADD_PLOT", plot_id, plot_group, time, value, title,
    as.character(length(rows)), point_lines,
    "TIME_SERIES", time_type, series_name, as.character(length(labels)), labels,
    "TIME_SERIES_OPTIONS", identification, legend_position
  )
  if (.Platform$OS.type == "windows") {
    .rls_send_winui(lines)
  } else {
    .rls_start_backend()
    lines <- c(lines, .rls_variable_payload(data), .rls_dataframe_payload(data))
    .rls_send(lines)
  }
  .rls_register_plot(plot_id, plot_group, time, value, data, rows,
                     type = "time_series", title = title,
                     series = series_name, time_type = time_type,
                     identification = identification,
                     legend_position = legend_position)
  structure(
    list(id = plot_id, group = plot_group, x = time, y = value,
         series = series_name, title = title),
    class = "rlispstat_plot"
  )
}

#' Create a time-series plot from a registered dataset
#'
#' @param group Registered dataset name. The active dataset is used when `NULL`.
#' @inheritParams ls_time_series
#' @param linked Whether selections should use the registered dataset group.
#' @return An object of class `rlispstat_plot`.
#' @export
ls_new_time_series <- function(group = NULL, time = NULL, value = NULL,
                               series = NULL, title = NULL, linked = TRUE,
                               identification = c("legend", "start_labels", "none"),
                               legend_position = c("top_right", "top_left", "bottom_right", "bottom_left", "left", "right", "top", "bottom")) {
  record <- .rls_dataset_record(group)
  numeric <- .rls_numeric_variable_names(record$data, record$variable_metadata)
  if (length(numeric) < 2L && (is.null(time) || is.null(value))) {
    stop("The dataset needs at least two numeric variables unless `time` and `value` are supplied.", call. = FALSE)
  }
  time <- time %||% numeric[[1L]]
  value <- value %||% numeric[[min(2L, length(numeric))]]
  plot_group <- if (isTRUE(linked)) record$group else
    paste0(record$group, "_", format(Sys.time(), "%Y%m%d%H%M%OS3"))
  ls_time_series(record$data, time = time, value = value, series = series,
                 group = plot_group, title = title,
                 identification = identification,
                 legend_position = legend_position)
}

#' Change the grouping variable of a time-series plot
#'
#' @param plot A time-series plot or plot id.
#' @param series A column name, or `NULL` for a single series.
#' @return Invisibly returns `plot`.
#' @export
ls_time_series_group <- function(plot, series = NULL) {
  id <- .rls_plot_id(plot)
  record <- .rls_plot_record(plot)
  if (!identical(record$type, "time_series")) stop("`plot` is not a time-series plot.", call. = FALSE)
  if (is.null(series)) {
    wire <- "."
    record$series <- ""
  } else {
    series <- .rls_validate_protocol_name(series, "series")
    if (!series %in% names(record$data)) stop(sprintf("Column `%s` was not found.", series), call. = FALSE)
    wire <- series
    record$series <- series
  }
  .rls_send(c("TIME_SERIES_SET_GROUP", id, wire))
  assign(id, record, envir = .rls_state$plots)
  invisible(plot)
}

#' Change how grouped time-series are identified
#'
#' @param plot A time-series plot or plot id.
#' @param identification One of `"legend"`, `"start_labels"`, or `"none"`.
#' @param legend_position Optional legend position: `"top_right"`,
#'   `"top_left"`, `"bottom_right"`, `"bottom_left"`, `"left"`, `"right"`, `"top"`, or `"bottom"`.
#' @return Invisibly returns `plot`.
#' @export
ls_time_series_identification <- function(
    plot,
    identification = c("legend", "start_labels", "none"),
    legend_position = NULL) {
  id <- .rls_plot_id(plot)
  record <- .rls_plot_record(plot)
  if (!identical(record$type, "time_series")) stop("`plot` is not a time-series plot.", call. = FALSE)
  identification <- match.arg(identification)
  record$identification <- identification
  if (!is.null(legend_position)) {
    legend_position <- match.arg(
      legend_position,
      c("top_right", "top_left", "bottom_right", "bottom_left", "left", "right", "top", "bottom")
    )
    .rls_send(c("TIME_SERIES_SET_LEGEND_POSITION", id, legend_position))
    record$legend_position <- legend_position
  }
  .rls_send(c("TIME_SERIES_SET_IDENTIFICATION", id, identification))
  assign(id, record, envir = .rls_state$plots)
  invisible(plot)
}
