#' Close all native LinkEDA windows
#'
#' Tells the native backend to close all windows and exit cleanly.
#'
#' @return Invisibly returns `TRUE`.
#' @export
ls_close_all <- function() {
  if (!isTRUE(.rls_state$process_started)) {
    return(invisible(TRUE))
  }
  try(.rls_send("CLOSE_ALL"), silent = TRUE)
  .rls_reset_backend_connection(clear_views = TRUE)
  invisible(TRUE)
}
