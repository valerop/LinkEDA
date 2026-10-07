.rls_multivariate_native_send <- function(lines) {
  if (.Platform$OS.type == "windows") return(.rls_send_winui(lines))
  .rls_start_backend()
  .rls_send(lines)
}

.rls_multivariate_register_native <- function(record) {
  if (.Platform$OS.type == "windows") {
    .rls_register_native_dataset_if_needed(record, sender = .rls_send_winui)
  } else {
    .rls_start_backend()
    .rls_register_native_dataset_if_needed(record)
  }
}

.rls_multivariate_variables <- function(record, variables) {
  numeric <- .rls_numeric_variable_names(record$data)
  variables <- variables %||% head(numeric, 4L)
  if (!is.character(variables) || length(variables) < 2L || anyNA(variables)) {
    stop("Choose at least two numeric variables.", call. = FALSE)
  }
  variables <- unique(vapply(variables, .rls_validate_protocol_name, character(1L), what = "variable"))
  missing <- setdiff(variables, names(record$data))
  if (length(missing)) stop(sprintf("Column `%s` was not found in the dataset.", missing[[1L]]), call. = FALSE)
  non_numeric <- variables[!vapply(record$data[variables], is.numeric, logical(1L))]
  if (length(non_numeric)) stop(sprintf("Column `%s` is not numeric.", non_numeric[[1L]]), call. = FALSE)
  variables
}

#' Open a linked scatterplot matrix
#'
#' @param group Registered dataset or dataset group name.
#' @param variables Two or more numeric variables. Defaults to the first four.
#' @return A native plot handle.
#' @export
ls_scatterplot_matrix <- function(group = NULL, variables = NULL) {
  record <- .rls_dataset_record(group)
  variables <- .rls_multivariate_variables(record, variables)
  .rls_multivariate_register_native(record)
  reply <- .rls_multivariate_native_send(c(
    "PLOT_NEW_LINKED_SCATTER_MATRIX", record$group, variables
  ))
  plot_id <- .rls_parse_records(reply)[[1L]]
  structure(list(id = plot_id, group = record$group, variables = variables,
                 type = "scatter_matrix"), class = "rlispstat_plot")
}

#' Open linked parallel coordinates
#'
#' @param group Registered dataset or dataset group name.
#' @param variables Two or more numeric variables. Defaults to the first four.
#' @param standardize Logical; standardize each variable before drawing.
#' @param connect Logical; connect values belonging to the same observation.
#' @return A native plot handle.
#' @export
ls_parallel_coordinates <- function(group = NULL, variables = NULL,
                                    standardize = TRUE, connect = TRUE) {
  record <- .rls_dataset_record(group)
  variables <- .rls_multivariate_variables(record, variables)
  for (value in c(standardize, connect)) {
    if (!is.logical(value) || length(value) != 1L || is.na(value))
      stop("`standardize` and `connect` must be TRUE or FALSE.", call. = FALSE)
  }
  .rls_multivariate_register_native(record)
  reply <- .rls_multivariate_native_send(c(
    "PLOT_NEW_PARALLEL_COORDINATES", record$group,
    if (standardize) "TRUE" else "FALSE", if (connect) "TRUE" else "FALSE",
    variables
  ))
  plot_id <- .rls_parse_records(reply)[[1L]]
  structure(list(id = plot_id, group = record$group, variables = variables,
                 type = "parallel_coordinates"), class = "rlispstat_plot")
}
