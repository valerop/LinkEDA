#' Reset or tile native LinkEDA windows
#'
#' @return Invisibly returns `TRUE`.
#' @export
ls_reset_window_layout <- function() {
  .rls_start_backend()
  .rls_send("RESET_WINDOW_LAYOUT")
  invisible(TRUE)
}

#' @rdname ls_reset_window_layout
#' @export
ls_tile_windows <- function() {
  .rls_start_backend()
  .rls_send("TILE_WINDOWS")
  invisible(TRUE)
}
