#' List active LinkEDA plots
#'
#' @return A data frame with plot id, group, variables, point count, and
#'   selection mode.
#' @export
ls_plots <- function() {
  if (!isTRUE(.rls_state$process_started)) {
    ids <- ls(envir = .rls_state$plots)
    records <- lapply(ids, function(id) get(id, envir = .rls_state$plots))
    return(data.frame(
      id = vapply(records, `[[`, character(1L), "id"),
      group = vapply(records, `[[`, character(1L), "group"),
      x = vapply(records, `[[`, character(1L), "x"),
      y = vapply(records, `[[`, character(1L), "y"),
      n = vapply(records, `[[`, integer(1L), "n"),
      selection_mode = "replace",
      mode = "select",
      stringsAsFactors = FALSE
    ))
  }

  records <- .rls_parse_records(.rls_send("LIST_PLOTS"))
  if (!length(records)) {
    return(data.frame(
      id = character(), group = character(), x = character(), y = character(),
      n = integer(), selection_mode = character(), mode = character(),
      stringsAsFactors = FALSE
    ))
  }

  parts <- strsplit(records, "|", fixed = TRUE)
  data.frame(
    id = vapply(parts, `[`, character(1L), 1L),
    group = vapply(parts, `[`, character(1L), 2L),
    x = vapply(parts, `[`, character(1L), 3L),
    y = vapply(parts, `[`, character(1L), 4L),
    n = as.integer(vapply(parts, `[`, character(1L), 5L)),
    selection_mode = vapply(parts, `[`, character(1L), 6L),
    mode = vapply(parts, `[`, character(1L), 7L),
    stringsAsFactors = FALSE
  )
}

#' Return metadata for an active plot
#'
#' @param plot An `rlispstat_plot` object or plot id.
#' @return A named list describing the plot.
#' @export
ls_plot_info <- function(plot) {
  plot_id <- .rls_plot_id(plot)
  reply <- .rls_send(c("PLOT_INFO", plot_id))
  records <- .rls_parse_records(reply)
  if (length(records) != 1L) {
    stop("Unexpected plot info reply from the LinkEDA backend.", call. = FALSE)
  }
  parts <- strsplit(records, "|", fixed = TRUE)[[1L]]
  list(
    id = parts[[1L]],
    group = parts[[2L]],
    x = parts[[3L]],
    y = parts[[4L]],
    n = as.integer(parts[[5L]]),
    selected_n = as.integer(parts[[6L]]),
    xlim = as.numeric(parts[7:8]),
    ylim = as.numeric(parts[9:10]),
    selection_mode = parts[[11L]],
    mode = parts[[12L]],
    title = if (length(parts) >= 13L) parts[[13L]] else ""
  )
}

#' Redraw an active plot
#'
#' @param plot An `rlispstat_plot` object or plot id.
#' @return Invisibly returns `TRUE`.
#' @export
ls_redraw <- function(plot) {
  .rls_send(c("REDRAW", .rls_plot_id(plot)))
  invisible(TRUE)
}

#' Close one active plot
#'
#' @param plot An `rlispstat_plot` object or plot id.
#' @return Invisibly returns `TRUE`.
#' @export
ls_close <- function(plot) {
  plot_id <- .rls_plot_id(plot)
  .rls_send(c("CLOSE_PLOT", plot_id))
  .rls_unregister_plot(plot_id)
  invisible(TRUE)
}

#' Set or return a plot's selection mode
#'
#' @param plot An `rlispstat_plot` object or plot id.
#' @param mode One of `"replace"`, `"add"`, `"subtract"`, or `"toggle"`.
#' @return The selected mode.
#' @export
ls_selection_mode <- function(plot, mode = c("replace", "add", "subtract", "toggle")) {
  mode <- match.arg(mode)
  .rls_send(c("SELECTION_MODE", .rls_plot_id(plot), mode))
  invisible(mode)
}

#' Get or set a plot interaction mode
#'
#' @param plot An `rlispstat_plot` object or plot id.
#' @return `ls_mode()` returns the current interaction mode. `ls_set_mode()`
#'   invisibly returns the mode.
#' @export
ls_mode <- function(plot) {
  reply <- .rls_send(c("MODE", .rls_plot_id(plot)))
  if (!startsWith(reply, "OK ")) {
    stop("Unexpected reply from the LinkEDA backend.", call. = FALSE)
  }
  sub("^OK ", "", reply)
}

#' @rdname ls_mode
#' @param mode One of `"none"`, `"select"`, `"brush"`, `"identify"`, `"pan"`,
#'   `"zoom"`, or `"label"`.
#' @export
ls_set_mode <- function(plot, mode = c("none", "select", "brush", "identify", "pan", "zoom", "label")) {
  mode <- match.arg(mode)
  .rls_send(c("MODE", .rls_plot_id(plot), mode))
  invisible(mode)
}

#' Get or set a plot selection operation
#'
#' @param plot An `rlispstat_plot` object or plot id.
#' @return `ls_selection_operation()` returns the current operation.
#'   `ls_set_selection_operation()` invisibly returns the operation.
#' @export
ls_selection_operation <- function(plot) {
  reply <- .rls_send(c("SELECTION_OPERATION", .rls_plot_id(plot)))
  if (!startsWith(reply, "OK ")) {
    stop("Unexpected reply from the LinkEDA backend.", call. = FALSE)
  }
  sub("^OK ", "", reply)
}

#' @rdname ls_selection_operation
#' @param operation One of `"replace"`, `"add"`, `"subtract"`, or `"toggle"`.
#' @export
ls_set_selection_operation <- function(plot, operation = c("replace", "add", "subtract", "toggle")) {
  operation <- match.arg(operation)
  .rls_send(c("SELECTION_OPERATION", .rls_plot_id(plot), operation))
  invisible(operation)
}

#' Show or hide the native LinkEDA tool panel
#'
#' @param show Logical. `TRUE` shows the floating panel; `FALSE` hides it.
#' @return Invisibly returns `TRUE`.
#' @export
ls_panel <- function(show = TRUE) {
  if (!is.logical(show) || length(show) != 1L || is.na(show)) {
    stop("`show` must be TRUE or FALSE.", call. = FALSE)
  }
  .rls_start_backend()
  .rls_send(c("PANEL", if (show) "show" else "hide"))
  invisible(TRUE)
}
