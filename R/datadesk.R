#' Open the native variable palette for a group
#'
#' Opens a DataDesk-style native variable browser for the given linked group.
#'
#' @param group Linked group name. If omitted, the only active group is used.
#' @return Invisibly returns `TRUE`.
#' @export
ls_variables_window <- function(group = NULL) {
  group <- .rls_resolve_group(group)
  .rls_send(c("VARIABLES_WINDOW", group))
  invisible(TRUE)
}

#' Return variable metadata and roles for a group
#'
#' @param group Linked group name. If omitted, the only active group is used.
#' @return A data frame with variable name, type, and current role.
#' @export
ls_variable_info <- function(group = NULL) {
  group <- .rls_resolve_group(group)
  records <- .rls_parse_records(.rls_send(c("VARIABLE_INFO", group)))
  if (!length(records)) {
    return(data.frame(
      variable = character(), type = character(), role = character(),
      stringsAsFactors = FALSE
    ))
  }
  parts <- strsplit(records, "|", fixed = TRUE)
  data.frame(
    variable = vapply(parts, `[`, character(1L), 1L),
    type = vapply(parts, `[`, character(1L), 2L),
    role = vapply(parts, function(x) if (length(x) >= 3L) x[[3L]] else "", character(1L)),
    stringsAsFactors = FALSE
  )
}

#' Set a variable role in the group model
#'
#' @param group Linked group name. If omitted, the only active group is used.
#' @param variable Variable name.
#' @param role One of `"dependent"` or `"predictor"`.
#' @return Invisibly returns `TRUE`.
#' @export
ls_set_variable_role <- function(group = NULL, variable, role = c("dependent", "predictor")) {
  group <- .rls_resolve_group(group)
  variable <- .rls_validate_protocol_name(variable, "variable")
  role <- match.arg(role)
  if (role == "dependent") {
    .rls_send(c("MODEL_SET_Y", group, variable))
  } else {
    .rls_send(c("MODEL_ADD_TERM", group, variable))
  }
  invisible(TRUE)
}

#' Create or retrieve the group model object
#'
#' @param group Linked group name. If omitted, the only active group is used.
#' @return An `rlispstat_model` handle.
#' @export
ls_model <- function(group = NULL) {
  group <- .rls_resolve_group(group)
  .rls_send(c("MODEL_INFO", group))
  structure(list(id = paste0("model:", group), group = group), class = "rlispstat_model")
}

#' @export
print.rlispstat_model <- function(x, ...) {
  info <- try(ls_model_info(x), silent = TRUE)
  cat("<LinkEDA_model>\n")
  cat("  group: ", x$group, "\n", sep = "")
  if (!inherits(info, "try-error")) {
    cat("  y:     ", info$dependent, "\n", sep = "")
    cat("  terms: ", if (length(info$terms)) paste(info$terms, collapse = " + ") else "(none)", "\n", sep = "")
    cat("  scope: ", info$scope, "\n", sep = "")
  }
  invisible(x)
}

.rls_model_group <- function(model) {
  if (inherits(model, "rlispstat_model")) {
    return(model$group)
  }
  .rls_validate_group(model, allow_null = FALSE)
}

#' Return model metadata
#'
#' @param model An `rlispstat_model` object or group name.
#' @return A list with group, dependent variable, scope, and terms.
#' @export
ls_model_info <- function(model) {
  group <- .rls_model_group(model)
  records <- .rls_parse_records(.rls_send(c("MODEL_INFO", group)))
  if (length(records) != 1L) {
    stop("Unexpected model info reply from the LinkEDA backend.", call. = FALSE)
  }
  parts <- strsplit(records[[1L]], "|", fixed = TRUE)[[1L]]
  list(
    group = parts[[1L]],
    dependent = if (length(parts) >= 2L) parts[[2L]] else "",
    scope = if (length(parts) >= 3L) parts[[3L]] else "all",
    terms = if (length(parts) > 3L) parts[4:length(parts)] else character()
  )
}

#' Update the live group model
#'
#' @param model An `rlispstat_model` object or group name.
#' @param variable Variable name.
#' @return Invisibly returns `TRUE`.
#' @export
ls_model_set_y <- function(model, variable) {
  group <- .rls_model_group(model)
  variable <- .rls_validate_protocol_name(variable, "variable")
  .rls_send(c("MODEL_SET_Y", group, variable))
  invisible(TRUE)
}

#' @rdname ls_model_set_y
#' @export
ls_model_add_term <- function(model, variable) {
  group <- .rls_model_group(model)
  variable <- .rls_validate_protocol_name(variable, "variable")
  .rls_send(c("MODEL_ADD_TERM", group, variable))
  invisible(TRUE)
}

#' Set model scope
#'
#' @param model An `rlispstat_model` object or group name.
#' @param scope One of `"all"`, `"selected"`, or `"unselected"`.
#' @return Invisibly returns the selected scope.
#' @export
ls_model_scope <- function(model, scope = c("all", "selected", "unselected")) {
  group <- .rls_model_group(model)
  scope <- match.arg(scope)
  # Legacy API now explicitly changes the dataset's global scope.
  if(scope=="all") ls_use_all_observations(group) else if(scope=="selected")
    ls_use_selected_as_analysis_scope(group) else
    .rls_send(c("SET_ANALYSIS_SCOPE_UNSELECTED",group))
  invisible(scope)
}

#' @rdname ls_model_set_y
#' @export
ls_model_remove_term <- function(model, variable) {
  group <- .rls_model_group(model)
  variable <- .rls_validate_protocol_name(variable, "variable")
  .rls_send(c("MODEL_REMOVE_TERM", group, variable))
  invisible(TRUE)
}

#' Fit or open the live model window
#'
#' @param model An `rlispstat_model` object or group name.
#' @return Invisibly returns `TRUE`.
#' @export
ls_model_fit <- function(model) {
  group <- .rls_model_group(model)
  .rls_send(c("MODEL_OPEN", group))
  invisible(TRUE)
}

#' @rdname ls_model_fit
#' @export
ls_model_open_diagnostics <- function(model) {
  group <- .rls_model_group(model)
  records <- .rls_parse_records(.rls_send(c("MODEL_OPEN_DIAGNOSTICS", group)))
  invisible(if (length(records)) records[[1L]] else TRUE)
}

#' @rdname ls_model_fit
#' @export
ls_model_open_residuals_fitted <- function(model) {
  group <- .rls_model_group(model)
  records <- .rls_parse_records(.rls_send(c("MODEL_OPEN_RESIDUALS_FITTED", group)))
  invisible(if (length(records)) records[[1L]] else TRUE)
}

#' @rdname ls_model_fit
#' @export
ls_model_open_observed_fitted <- function(model) {
  group <- .rls_model_group(model)
  records <- .rls_parse_records(.rls_send(c("MODEL_OPEN_OBSERVED_FITTED", group)))
  invisible(if (length(records)) records[[1L]] else TRUE)
}

#' Open the native DataDesk-style workbench for a data frame
#'
#' Convenience launcher for integrations such as jamovi. It opens a native
#' scatterplot, the variable palette, and the live model window for the supplied
#' data frame.
#'
#' @param data A data frame.
#' @param group Linked group name.
#' @param y Optional numeric dependent variable name.
#' @param terms Optional predictor variable names. Numeric terms are added to
#'   the first prototype model; nonnumeric terms are kept in the data/variable
#'   palette but not added to the native numeric model yet.
#' @param labels Optional label variable name or vector of labels.
#' @param color Optional color/grouping variable name. Stored for future
#'   DataDesk-style color mapping; not applied by the current native backend.
#' @param openDiagnostics Logical. If `TRUE`, opens residuals vs fitted.
#' @param scope Model scope: `"all"`, `"selected"`, or `"unselected"`.
#' @return Invisibly returns a list with the plot and model handles.
#' @export
ls_workbench <- function(data,
                         group = "data",
                         y = NULL,
                         terms = NULL,
                         labels = NULL,
                         color = NULL,
                         openDiagnostics = FALSE,
                         scope = c("all", "selected", "unselected")) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  group <- .rls_validate_group(group, allow_null = FALSE)
  .rls_register_dataset(group, data, source = "workbench", activate = TRUE)
  scope <- match.arg(scope)

  numeric_vars <- .rls_numeric_variable_names(data)
  if (!length(numeric_vars)) {
    stop("`data` must contain at least one numeric variable.", call. = FALSE)
  }

  if (is.null(y)) {
    y <- numeric_vars[[1L]]
  }
  y <- .rls_validate_protocol_name(y, "y")
  if (!y %in% names(data)) {
    stop(sprintf("Column `%s` was not found in `data`.", y), call. = FALSE)
  }
  if (!is.numeric(data[[y]])) {
    stop("`y` must name a numeric column for the first native model prototype.", call. = FALSE)
  }

  terms <- as.character(terms %||% setdiff(numeric_vars, y)[1L])
  terms <- terms[!is.na(terms) & nzchar(terms)]
  for (term in terms) {
    .rls_validate_protocol_name(term, "term")
    if (!term %in% names(data)) {
      stop(sprintf("Column `%s` was not found in `data`.", term), call. = FALSE)
    }
  }
  numeric_terms <- terms[vapply(terms, function(term) is.numeric(data[[term]]), logical(1L))]
  x <- if (length(numeric_terms)) numeric_terms[[1L]] else y

  if (is.character(labels) && length(labels) == 1L && labels %in% names(data)) {
    labels <- as.character(data[[labels]])
  }
  if (!is.null(labels) && length(labels) != nrow(data)) {
    stop("`labels` must be a column name or one label per row in `data`.", call. = FALSE)
  }

  if (!is.null(color)) {
    color <- .rls_validate_protocol_name(color, "color")
    if (!color %in% names(data)) {
      stop(sprintf("Column `%s` was not found in `data`.", color), call. = FALSE)
    }
  }

  plot <- ls_scatter(data, x = x, y = y, group = group, labels = labels,
                     title = paste(y, "vs", x))
  ls_variables_window(group)

  model <- ls_model(group)
  ls_model_set_y(model, y)
  for (term in numeric_terms) {
    if (!identical(term, y)) {
      ls_model_add_term(model, term)
    }
  }
  ls_model_scope(model, scope)
  ls_model_fit(model)

  if (isTRUE(openDiagnostics)) {
    ls_model_open_residuals_fitted(model)
  }

  invisible(list(plot = plot, model = model, group = group, color = color))
}
