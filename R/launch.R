#' Open LinkEDA
#'
#' Starts the native LinkEDA application directly from R. Data are optional:
#' when supplied, the data frame (including tibble and tribble objects) is
#' registered, made active, and opened in a linked data sheet. With no data the
#' application opens empty and data can be imported later from the File menu.
#'
#' @param data Optional data frame, tibble, tribble, or a single path to a
#'   supported data file.
#' @param name Optional name for `data` inside LinkEDA. By default the R object
#'   name is used when possible.
#' @return Invisibly returns the registered dataset name, or `NULL` when no data
#'   were supplied.
#' @export
LinkEDA <- function(data = NULL, name = NULL) {
  data_expression <- if (is.null(data)) "" else deparse(substitute(data), nlines = 1L)
  if (is.data.frame(data) && is.null(name) && nzchar(data_expression) &&
      grepl("^[.A-Za-z][.A-Za-z0-9_]*$", data_expression)) {
    name <- data_expression
  }
  .linkeda_launch(data = data, name = name, data_expression = data_expression)
}

#' @rdname LinkEDA
#' @export
rlispstat <- function(data = NULL, name = NULL) {
  .Deprecated("LinkEDA", package = "LinkEDA")
  data_expression <- if (is.null(data)) "" else deparse(substitute(data), nlines = 1L)
  .linkeda_launch(data = data, name = name, data_expression = data_expression)
}

.linkeda_launch <- function(data = NULL, name = NULL, data_expression = "") {
  is_file_request <- is.character(data) && length(data) == 1L &&
    !is.na(data) && nzchar(data)
  if (!is.null(data) && !is.data.frame(data) && !is_file_request) {
    stop("`data` must be a data frame, tibble, tribble, or a single data-file path.",
         call. = FALSE)
  }
  if (!is.null(name) &&
      (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name))) {
    stop("`name` must be a single non-empty string.", call. = FALSE)
  }

  .rls_start_backend()
  if (is.null(data)) {
    version <- as.character(utils::packageVersion("LinkEDA"))
    welcome <- c("WELCOME_LAUNCH", "from_existing_r", version, "none")
    opened <- tryCatch(.rls_send(welcome), error = identity)
    if (inherits(opened, "error")) {
      # Quit/Command-Q runs in AppKit. It can win the race after the startup
      # PING, leaving R with a pipe to an instance that is already closing.
      .rls_reset_backend_connection(clear_views = TRUE)
      .rls_start_backend()
      .rls_send(welcome)
    }
    return(invisible(NULL))
  }

  if (is_file_request) {
    path <- normalizePath(data, mustWork = FALSE)
    return(tryCatch(
      {
        dataset <- ls_import_data(path, name = name)
        try(.rls_send(c("WELCOME_RECENT_NOTE", path)), silent = TRUE)
        invisible(dataset)
      },
      error = function(error) {
        message <- gsub("[[:cntrl:]|]+", " ", conditionMessage(error))
        try(.rls_send(c("WELCOME_INITIAL_OPEN_ERROR", path, message)), silent = TRUE)
        stop(conditionMessage(error), call. = FALSE)
      }
    ))
  }

  if (is.null(name)) {
    name <- if (nzchar(data_expression) &&
                   grepl("^[.A-Za-z][.A-Za-z0-9_]*$", data_expression)) {
      data_expression
    } else {
      "data"
    }
  }
  registered <- .rls_register_dataset(
    name, data, source = "R launch", activate = TRUE, replace = TRUE
  )
  invisible(registered)
}
