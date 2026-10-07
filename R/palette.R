.rls_palette_colors <- c(
  orange = "#E69F00",
  blue = "#0072B2",
  green = "#009E73",
  vermillion = "#D55E00",
  purple = "#CC79A7",
  brown = "#A6761D",
  pink = "#D81B60",
  yellow = "#F0E442"
)

.rls_data_sheet_palette_colors <- c(
  orange = "#FBE6B3",
  blue = "#CFE8F6",
  green = "#CCEDE3",
  vermillion = "#F3D4C2",
  purple = "#F1DDEA",
  brown = "#D7C3A3",
  pink = "#F8BBD0",
  yellow = "#FCF7BD"
)

.rls_data_sheet_selected_palette_colors <- c(
  orange = "#E69F00",
  blue = "#0072B2",
  green = "#009E73",
  vermillion = "#D55E00",
  purple = "#CC79A7",
  brown = "#A6761D",
  pink = "#D81B60",
  yellow = "#F0E442"
)

.rls_data_sheet_selected_text_colors <- c(
  orange = "#000000",
  blue = "#FFFFFF",
  green = "#FFFFFF",
  vermillion = "#FFFFFF",
  purple = "#000000",
  brown = "#FFFFFF",
  pink = "#FFFFFF",
  yellow = "#000000"
)

.rls_palette_color_names <- names(.rls_palette_colors)

.rls_data_sheet_display_color <- function(color = NULL, selected = FALSE, has_explicit_color = !is.null(color) && nzchar(color)) {
  if (!isTRUE(has_explicit_color)) {
    return(list(background = if (isTRUE(selected)) "#E9E9E9" else "#FFFFFF", text = "#000000"))
  }
  color <- .rls_validate_palette_color(color)
  if (isTRUE(selected)) {
    list(background = unname(.rls_data_sheet_selected_palette_colors[color]),
         text = unname(.rls_data_sheet_selected_text_colors[color]))
  } else {
    list(background = unname(.rls_data_sheet_palette_colors[color]), text = "#000000")
  }
}

.rls_get_display_color <- .rls_data_sheet_display_color

.rls_palette_table <- function(print = TRUE) {
  tbl <- data.frame(
    color = .rls_palette_color_names,
    base_hex = unname(.rls_palette_colors),
    light_row_background_hex = unname(.rls_data_sheet_palette_colors),
    selected_row_background_hex = unname(.rls_data_sheet_selected_palette_colors),
    recommended_text_color = unname(.rls_data_sheet_selected_text_colors),
    stringsAsFactors = FALSE
  )
  if (isTRUE(print)) {
    print(tbl, row.names = FALSE)
  }
  invisible(tbl)
}

.rls_validate_palette_color <- function(color) {
  color <- .rls_validate_protocol_name(color, "color")
  if (!color %in% .rls_palette_color_names) {
    stop(sprintf("`color` must be one of: %s.", paste(.rls_palette_color_names, collapse = ", ")), call. = FALSE)
  }
  color
}

#' Show or hide the selected-point color palette
#'
#' @param show Logical. `TRUE` shows the floating palette; `FALSE` hides it.
#' @return Invisibly returns `TRUE`.
#' @export
ls_palette <- function(show = TRUE) {
  if (!is.logical(show) || length(show) != 1L || is.na(show)) {
    stop("`show` must be TRUE or FALSE.", call. = FALSE)
  }
  .rls_start_backend()
  .rls_send(c("PALETTE", if (show) "show" else "hide"))
  invisible(TRUE)
}

#' Set or get the selected-point color for a group
#'
#' If rows are currently selected, setting the selected color also applies the
#' color to those original row indices across linked plots. If no rows are
#' selected, it becomes the default color for future selected points.
#'
#' @param group A non-empty group name. If `NULL`, the only active group is used.
#' @param color One of `orange`, `blue`, `green`, `vermillion`, `purple`,
#'   `brown`, `pink`, or `yellow`. Use the Default/Reset color control to remove
#'   explicit row color and return to black points with white data-sheet rows.
#' @return The getter returns a color name. The setter invisibly returns `TRUE`.
#' @export
ls_set_selected_color <- function(group = NULL, color) {
  group <- .rls_resolve_group(group)
  color <- .rls_validate_palette_color(color)
  .rls_send(c("SET_SELECTED_COLOR", group, color))
  invisible(TRUE)
}

#' @rdname ls_set_selected_color
#' @export
ls_get_selected_color <- function(group = NULL) {
  group <- .rls_resolve_group(group)
  reply <- .rls_send(c("GET_SELECTED_COLOR", group))
  if (!startsWith(reply, "OK ")) {
    stop("Unexpected reply from the LinkEDA backend.", call. = FALSE)
  }
  sub("^OK ", "", reply)
}

#' Set point colors by original row index
#'
#' @param plot An `rlispstat_plot` object or plot id.
#' @param rows Original R row indices.
#' @param color One palette color name.
#' @return Invisibly returns `TRUE`.
#' @export
ls_set_point_color <- function(plot, rows, color) {
  plot_id <- .rls_plot_id(plot)
  if (!exists(plot_id, envir = .rls_state$plots, inherits = FALSE)) {
    stop("No local plot metadata are available for this plot.", call. = FALSE)
  }
  group <- get(plot_id, envir = .rls_state$plots)$group
  color <- .rls_validate_palette_color(color)
  if (!is.numeric(rows) && !is.integer(rows)) {
    stop("`rows` must be numeric or integer row indices.", call. = FALSE)
  }
  rows <- sort(unique(as.integer(rows)))
  rows <- rows[!is.na(rows) & rows > 0L]
  .rls_send(c("SET_POINT_COLOR", group, color, as.character(length(rows)), as.character(rows)))
  invisible(TRUE)
}
