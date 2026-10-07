#' Open the native GLM workbench
#'
#' Opens a native macOS General Linear Model window associated with a scatterplot.
#' The first prototype fits a simple linear model for the plot's current
#' variables and can use either all plotted rows or the selected rows in the
#' linked group.
#'
#' @param plot Optional `rlispstat_plot` object or plot id. If omitted, the
#'   backend's active plot is used.
#' @return Invisibly returns `TRUE`.
#' @export
#' @examples
#' \dontrun{
#' p <- ls_scatter(mtcars, "wt", "mpg", group = "cars")
#' ls_glm(p)
#' }
ls_glm <- function(plot = NULL) {
  if (is.null(plot)) {
    .rls_start_backend()
    .rls_send(c("GLM", "active"))
  } else {
    .rls_send(c("GLM", .rls_plot_id(plot)))
  }
  invisible(TRUE)
}
