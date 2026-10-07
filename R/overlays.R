#' Add a linear regression overlay
#'
#' Adds a native overlay that recomputes from the current plot variables whenever
#' the plot redraws. Selected-data overlays use the current linked selection.
#' Color overlays fit one line for each explicitly assigned point color.
#'
#' @param plot An `rlispstat_plot` object or plot id.
#' @param data Which data to use: all visible points, selected points, both, or
#'   one line per explicitly assigned point color.
#' @return Invisibly returns `TRUE`.
#' @export
ls_lm_line <- function(plot, data = c("all", "selected", "both", "color")) {
  data <- match.arg(data)
  .rls_send(c("ADD_LM", .rls_plot_id(plot), data))
  invisible(TRUE)
}

#' List native overlays for a plot
#'
#' @param plot An `rlispstat_plot` object or plot id.
#' @return A data frame with overlay id, type, source, and visibility.
#' @export
ls_overlays <- function(plot) {
  records <- .rls_parse_records(.rls_send(c("OVERLAYS", .rls_plot_id(plot))))
  if (!length(records)) {
    return(data.frame(id = integer(), type = character(), source = character(), visible = logical()))
  }
  parts <- strsplit(records, "|", fixed = TRUE)
  data.frame(
    id = as.integer(vapply(parts, `[`, character(1L), 1L)),
    type = vapply(parts, `[`, character(1L), 2L),
    source = vapply(parts, `[`, character(1L), 3L),
    visible = vapply(parts, `[`, character(1L), 4L) == "TRUE",
    stringsAsFactors = FALSE
  )
}

#' Remove linear regression overlays
#'
#' @param plot An `rlispstat_plot` object or plot id.
#' @param data Currently accepted for API compatibility; all overlays are cleared
#'   in this first implementation.
#' @return Invisibly returns `TRUE`.
#' @export
ls_clear_overlays <- function(plot, data = c("all", "selected", "both", "color")) {
  match.arg(data)
  .rls_send(c("CLEAR_OVERLAYS", .rls_plot_id(plot)))
  invisible(TRUE)
}
