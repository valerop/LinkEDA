#' Open a native interactive scatterplot
#'
#' Opens a native macOS scatterplot window. Drag with the mouse to select points
#' using a rectangular brush. Plots that share the same `group` have linked
#' selections.
#'
#' Missing values in `x` or `y` are excluded from drawing, but selected points
#' still map back to their original row indices.
#'
#' @param data A data frame.
#' @param x,y Names of numeric columns to plot.
#' @param group Optional non-empty string used for linked brushing.
#' @param labels Optional labels for future identify support. Accepted now for
#'   API compatibility; native labels are currently used where the backend supports them.
#' @param title Optional plot title shown in the native window.
#' @return An object of class `rlispstat_plot` containing the plot id and group.
#' @export
#' @examples
#' \dontrun{
#' p1 <- ls_scatter(mtcars, x = "wt", y = "mpg", group = "cars")
#' p2 <- ls_scatter(mtcars, x = "hp", y = "mpg", group = "cars")
#' ls_selected("cars")
#' }
ls_scatter <- function(data, x, y, group = NULL, labels = NULL, title = NULL) {
  prepared <- .rls_prepare_scatter_data(data, x, y, group)
  plot_id <- paste0("plot_", format(Sys.time(), "%Y%m%d%H%M%OS3"), "_", sample.int(1e6, 1L))
  plot_group <- prepared$group %||% plot_id
  if (!is.null(labels) && length(labels) != nrow(data)) {
    stop("`labels` must have one value per row in `data`.", call. = FALSE)
  }
  if (is.null(title)) {
    title <- paste(prepared$y_name, "vs", prepared$x_name)
  }
  title <- .rls_validate_protocol_name(title, "title")

  n <- length(prepared$x)
  lines <- c(
    "ADD_PLOT",
    plot_id,
    plot_group,
    prepared$x_name,
    prepared$y_name,
    title,
    as.character(n)
  )
  if (n) {
    point_lines <- sprintf("%.17g %.17g %d", prepared$x, prepared$y, prepared$row)
    lines <- c(lines, point_lines)
  }
  if (.Platform$OS.type == "windows") {
    .rls_send_winui(lines)
  } else {
    .rls_start_backend()
    lines <- c(lines, .rls_variable_payload(data), .rls_dataframe_payload(data))
    .rls_send(lines)
  }
  .rls_register_plot(plot_id, plot_group, prepared$x_name, prepared$y_name, data, prepared$row,
                     type = "scatter", title = title)

  structure(
    list(id = plot_id, group = plot_group, x = prepared$x_name, y = prepared$y_name, title = title),
    class = "rlispstat_plot"
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' @export
print.rlispstat_plot <- function(x, ...) {
  cat("<LinkEDA_plot>\n")
  cat("  id:    ", x$id, "\n", sep = "")
  cat("  group: ", x$group, "\n", sep = "")
  cat("  x:     ", x$x, "\n", sep = "")
  cat("  y:     ", x$y, "\n", sep = "")
  invisible(x)
}
