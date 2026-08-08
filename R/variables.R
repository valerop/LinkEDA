#' List variables available to a plot
#'
#' @param plot An `rlispstat_plot` object or plot id.
#' @return A character vector of variable names.
#' @export
ls_variables <- function(plot) {
  plot_id <- .rls_plot_id(plot)
  if (exists(plot_id, envir = .rls_state$plots, inherits = FALSE)) {
    return(names(get(plot_id, envir = .rls_state$plots)$data))
  }
  character()
}

#' @rdname ls_variables
#' @export
ls_numeric_variables <- function(plot) {
  plot_id <- .rls_plot_id(plot)
  if (exists(plot_id, envir = .rls_state$plots, inherits = FALSE)) {
    return(.rls_numeric_variable_names(get(plot_id, envir = .rls_state$plots)$data))
  }
  .rls_parse_records(.rls_send(c("VARIABLES", plot_id)))
}

#' Get or set scatterplot axis variables
#'
#' @param plot An `rlispstat_plot` object or plot id.
#' @param var Name of a numeric variable available in the plot data.
#' @return Getter functions return a variable name. Setter functions invisibly
#'   return the plot object or id.
#' @export
ls_get_xvar <- function(plot) {
  reply <- .rls_send(c("GET_XVAR", .rls_plot_id(plot)))
  if (!startsWith(reply, "OK ")) {
    stop("Unexpected reply from the LinkEDA backend.", call. = FALSE)
  }
  sub("^OK ", "", reply)
}

#' @rdname ls_get_xvar
#' @export
ls_get_yvar <- function(plot) {
  reply <- .rls_send(c("GET_YVAR", .rls_plot_id(plot)))
  if (!startsWith(reply, "OK ")) {
    stop("Unexpected reply from the LinkEDA backend.", call. = FALSE)
  }
  sub("^OK ", "", reply)
}

#' @rdname ls_get_xvar
#' @export
ls_set_xvar <- function(plot, var) {
  .rls_set_axis_var(plot, var, axis = "x")
}

#' @rdname ls_get_xvar
#' @export
ls_set_yvar <- function(plot, var) {
  .rls_set_axis_var(plot, var, axis = "y")
}

.rls_set_axis_var <- function(plot, var, axis) {
  plot_id <- .rls_plot_id(plot)
  var <- .rls_validate_protocol_name(var, "var")
  if (!exists(plot_id, envir = .rls_state$plots, inherits = FALSE)) {
    stop("No local plot metadata are available for this plot.", call. = FALSE)
  }
  record <- get(plot_id, envir = .rls_state$plots)
  if (!var %in% names(record$data)) {
    stop(sprintf("Column `%s` was not found in the plot data.", var), call. = FALSE)
  }
  if (!is.numeric(record$data[[var]])) {
    stop(sprintf("Column `%s` must be numeric.", var), call. = FALSE)
  }

  .rls_send(c(if (axis == "x") "SET_XVAR" else "SET_YVAR", plot_id, var))
  record[[axis]] <- var
  ok <- stats::complete.cases(record$data[, c(record$x, record$y), drop = FALSE])
  record$rows <- which(ok)
  record$n <- length(record$rows)
  assign(plot_id, record, envir = .rls_state$plots)

  if (inherits(plot, "rlispstat_plot")) {
    plot[[axis]] <- var
    return(invisible(plot))
  }
  invisible(plot_id)
}
