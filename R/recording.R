#' Control the LinkEDA reproducibility recorder
#'
#' These helpers mirror the native Record menu. A recording stores both an
#' internal LinkEDA command stream for replay and an R-script rendering for
#' later reproduction.
#'
#' @return Invisibly returns `TRUE`.
#' @export
ls_record_start <- function() {
  .rls_send("RECORD_START")
  invisible(TRUE)
}

#' @rdname ls_record_start
#' @export
ls_record_stop <- function() {
  .rls_send("RECORD_STOP")
  invisible(TRUE)
}

#' @rdname ls_record_start
#' @export
ls_record_show <- function() {
  .rls_send("RECORD_SHOW")
  invisible(TRUE)
}

#' @rdname ls_record_start
#' @export
ls_record_replay <- function() {
  .rls_send("RECORD_REPLAY")
  invisible(TRUE)
}

#' @rdname ls_record_start
#' @export
ls_record_clear <- function() {
  .rls_send("RECORD_CLEAR")
  invisible(TRUE)
}

#' @rdname ls_record_start
#' @export
ls_record_copy_r <- function() {
  .rls_send("RECORD_COPY_R")
  invisible(TRUE)
}

#' @rdname ls_record_start
#' @export
ls_record_save_r <- function() {
  .rls_send("RECORD_SAVE_R")
  invisible(TRUE)
}

#' @rdname ls_record_start
#' @export
ls_record_copy_internal <- function() {
  .rls_send("RECORD_COPY_INTERNAL")
  invisible(TRUE)
}
