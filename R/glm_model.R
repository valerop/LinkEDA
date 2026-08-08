.rls_glm_model_id <- function(group) {
  paste0("glm:", group)
}

.rls_glm_model_record <- function(model) {
  id <- if (inherits(model, "rlispstat_glm")) model$id else .rls_glm_model_id(.rls_validate_group(model, allow_null = FALSE))
  if (!exists(id, envir = .rls_state$glm_models, inherits = FALSE)) {
    stop("Unknown LinkEDA GLM model.", call. = FALSE)
  }
  get(id, envir = .rls_state$glm_models)
}

.rls_assign_glm_model <- function(record) {
  assign(record$id, record, envir = .rls_state$glm_models)
  structure(list(id = record$id, group = record$group), class = "rlispstat_glm")
}

.rls_glm_mark_model_changed <- function(record, status = "Model changed; refit required.") {
  record$model_version <- record$model_version + 1L
  record$is_stale <- TRUE
  record$fit <- NULL
  record$diagnostics <- data.frame()
  record$diagnostics_version <- record$diagnostics_version + 1L
  record$status <- status
  .rls_spreadplot_emit(
    "GLM_SPEC_CHANGED",
    group = record$group,
    sender_id = record$id,
    model_id = record$id,
    payload = list(model_version = record$model_version)
  )
  record
}

.rls_glm_notify_diagnostic_plots <- function(record) {
  ids <- ls(.rls_state$glm_diagnostic_plots, all.names = TRUE)
  for (id in ids) {
    plot <- get(id, envir = .rls_state$glm_diagnostic_plots)
    if (identical(plot$model_id, record$id)) {
      plot$displayed_fit_version <- record$fit_version
      plot$displayed_diagnostics_version <- record$diagnostics_version
      plot$is_stale <- FALSE
      plot$data <- record$diagnostics
      assign(id, plot, envir = .rls_state$glm_diagnostic_plots)
    }
  }
  .rls_spreadplot_emit(
    "GLM_DIAGNOSTICS_UPDATED",
    group = record$group,
    sender_id = record$id,
    model_id = record$id,
    rows = record$rows_used,
    payload = list(fit_version = record$fit_version, diagnostics_version = record$diagnostics_version)
  )
  invisible(TRUE)
}

.rls_validate_glm_variable <- function(record, variable, role) {
  variable <- .rls_validate_protocol_name(variable, role)
  if (!variable %in% names(record$data)) {
    stop(sprintf("Column `%s` was not found in the dataset.", variable), call. = FALSE)
  }
  if (!is.numeric(record$data[[variable]])) {
    stop(sprintf("`%s` must be numeric for this GLM phase.", variable), call. = FALSE)
  }
  variable
}

.rls_glm_formula_object <- function(record) {
  .rls_model_formula_object(record$dependent, record$predictors)
}

.rls_glm_data_for_fit <- function(record) {
  .rls_model_data_for_term_types(record$data, record$term_types %||% list(), response = record$dependent)
}

.rls_glm_complete_data <- function(record) {
  .rls_model_complete_data(
    record$data,
    .rls_glm_formula_object(record),
    record$scope %||% "all",
    record$selected_rows %||% integer()
  )
}

.rls_compute_glm_diagnostics <- function(record, fit, complete) {
  fitted <- stats::fitted(fit)
  residuals <- stats::residuals(fit)
  sigma <- summary(fit)$sigma
  leverage <- stats::hatvalues(fit)
  std_resid <- residuals / (sigma * sqrt(pmax(1 - leverage, .Machine$double.eps)))
  stud_resid <- tryCatch(stats::rstudent(fit), error = function(e) rep(NA_real_, length(residuals)))
  cooks <- tryCatch(stats::cooks.distance(fit), error = function(e) rep(NA_real_, length(residuals)))
  response <- all.vars(stats::formula(fit))[[1L]]
  data.frame(
    row_id = complete$rows,
    observed = complete$data[[response]],
    fitted = unname(fitted),
    residual = unname(residuals),
    standardized_residual = unname(std_resid),
    studentized_residual = unname(stud_resid),
    leverage = unname(leverage),
    cooks_distance = unname(cooks),
    sqrt_abs_standardized_residual = sqrt(abs(unname(std_resid))),
    stringsAsFactors = FALSE
  )
}

.rls_glm_extract_fit <- function(record, fit, complete) {
  summary_fit <- summary(fit)
  coef_matrix <- as.data.frame(unclass(summary_fit$coefficients), stringsAsFactors = FALSE)
  names(coef_matrix) <- c("estimate", "std_error", "t_value", "p_value")
  coef_matrix$term <- rownames(summary_fit$coefficients)
  coef_matrix$partial_r2 <- NA_real_
  df_resid <- stats::df.residual(fit)
  non_intercept <- coef_matrix$term != "(Intercept)"
  coef_matrix$partial_r2[non_intercept] <-
    coef_matrix$t_value[non_intercept]^2 / (coef_matrix$t_value[non_intercept]^2 + df_resid)
  coef_matrix <- coef_matrix[, c("term", "estimate", "std_error", "t_value", "p_value", "partial_r2")]
  rownames(coef_matrix) <- NULL

  fstat <- summary_fit$fstatistic
  if (is.null(fstat)) {
    global_f <- NA_real_
    df_model <- 0
    global_p <- NA_real_
  } else {
    global_f <- unname(fstat[["value"]])
    df_model <- unname(fstat[["numdf"]])
    global_p <- stats::pf(global_f, df_model, unname(fstat[["dendf"]]), lower.tail = FALSE)
  }
  residuals <- stats::residuals(fit)
  observed <- stats::model.response(stats::model.frame(fit))
  ss_residual <- sum(residuals^2)
  ss_total <- sum((observed - mean(observed))^2)
  ss_regression <- ss_total - ss_residual
  ms_regression <- if (df_model > 0) ss_regression / df_model else NA_real_
  ms_residual <- if (df_resid > 0) ss_residual / df_resid else NA_real_
  list(
    fit = fit,
    coefficients = coef_matrix,
    coefficient_rows = .rls_model_coefficient_display_rows(
      fit,
      transform(coef_matrix, statistic = t_value),
      record$data,
      record$predictors,
      statistic_name = "t"
    ),
    summary = list(
      n_used = length(complete$rows),
      n_excluded = complete$excluded,
      r_squared = unname(summary_fit$r.squared),
      adj_r_squared = unname(summary_fit$adj.r.squared),
      global_f = global_f,
      df_model = df_model,
      df_residual = df_resid,
      global_p = global_p,
      ss_regression = ss_regression,
      ss_residual = ss_residual,
      ms_regression = ms_regression,
      ms_residual = ms_residual,
      rmse = sqrt(mean(residuals^2)),
      residual_se = unname(summary_fit$sigma),
      aic = stats::AIC(fit),
      bic = stats::BIC(fit)
    ),
    rows_used = complete$rows,
    rows_excluded = setdiff(seq_len(nrow(record$data)), complete$rows),
    diagnostics = .rls_compute_glm_diagnostics(record, fit, complete),
    status = sprintf("Model fitted successfully; %d rows used, %d excluded.", length(complete$rows), complete$excluded)
  )
}

#' Create or open a native General Linear Model window
#'
#' @param group Dataset/group name. If `NULL`, the active dataset is used.
#' @return An `rlispstat_glm` model handle.
#' @export
ls_new_glm <- function(group = NULL) {
  record <- .rls_dataset_record(group)
  numeric <- .rls_numeric_variable_names(record$data, record$variable_metadata)
  model <- list(
    id = .rls_glm_model_id(record$group),
    group = record$group,
    data = record$data,
    dataset_type = record$dataset_type %||% "data_frame",
    analysis_backend = .rls_analysis_backend(record, "linear_model"),
    imputation_id = record$imputation_id %||% NULL,
    imputation_count = record$imputation_count %||% NULL,
    dependent = if (length(numeric)) numeric[[1L]] else "",
    predictors = character(),
    fit = NULL,
    coefficients = data.frame(),
    summary = list(),
    rows_used = integer(),
    rows_excluded = integer(),
    diagnostics = data.frame(),
    diagnostics_by_imputation = NULL,
    fits_by_imputation = NULL,
    multiple_imputation = NULL,
    coefficient_rows = data.frame(),
    term_types = list(),
    selected_rows = integer(),
    model_version = 0L,
    fit_version = 0L,
    diagnostics_version = 0L,
    is_stale = TRUE,
    status = "Add at least one independent variable."
  )
  .rls_spreadplot_register(
    model$id,
    model$group,
    c("GLM_REFIT_REQUESTED", "ROW_SELECTION_CHANGED", "ROW_COLORS_CHANGED")
  )
  .rls_assign_glm_model(model)
}

#' @rdname ls_new_glm
#' @export
ls_glm_window <- function(group = NULL) {
  dataset <- .rls_dataset_record(group)
  model_id <- .rls_glm_model_id(dataset$group)
  model <- if (exists(model_id, envir = .rls_state$glm_models, inherits = FALSE)) {
    structure(list(id = model_id, group = dataset$group), class = "rlispstat_glm")
  } else {
    ls_new_glm(dataset$group)
  }
  record <- .rls_dataset_record(model$group)
  if (identical(.rls_analysis_backend(record, "linear_model"), "multiple_imputation")) {
    model_record <- .rls_glm_model_record(model)
    if (length(model_record$predictors) && is.null(model_record$fit)) {
      model <- ls_glm_fit(model)
      model_record <- .rls_glm_model_record(model)
    }
    if (is.null(model_record$fit)) {
      warning("Add at least one independent variable before opening the pooled MI General Linear Model table.", call. = FALSE)
      return(model)
    }
    .rls_glm_sync_native_pooled(model_record)
    return(model)
  }
  numeric <- .rls_numeric_variable_names(record$data, record$variable_metadata)
  .rls_start_backend()
  .rls_send(c(
    "REGISTER_DATASET",
    record$group,
    .rls_variable_payload(record$data, record$variable_metadata),
    .rls_dataframe_payload(record$data, record$variable_metadata, dataset_record = record)
  ))
  opened <- try(.rls_send(c("MODEL_OPEN", record$group)), silent = TRUE)
  if (inherits(opened, "try-error") && length(numeric) >= 2L) {
    plot <- ls_new_scatterplot(record$group, x = numeric[[2L]], y = numeric[[1L]])
    try(.rls_send(c("MODEL_SET_Y", record$group, numeric[[1L]])), silent = TRUE)
    try(.rls_send(c("MODEL_OPEN", plot$group)), silent = TRUE)
  }
  model
}

#' Configure and fit a General Linear Model
#'
#' @param model An `rlispstat_glm` object or dataset/group name.
#' @param variable Variable name.
#' @return See individual functions.
#' @export
ls_glm_set_dependent <- function(model, variable) {
  record <- .rls_glm_model_record(model)
  variable <- .rls_model_validate_response(record$data, variable, "variable")
  old <- record$dependent
  record$dependent <- variable
  record$predictors <- setdiff(record$predictors, variable)
  record <- .rls_glm_mark_model_changed(record)
  .rls_spreadplot_emit(
    "GLM_DEPENDENT_CHANGED",
    group = record$group,
    sender_id = record$id,
    model_id = record$id,
    payload = list(old = old, new = variable, model_version = record$model_version)
  )
  invisible(.rls_assign_glm_model(record))
}

#' @rdname ls_glm_set_dependent
#' @export
ls_glm_get_dependent <- function(model) {
  .rls_glm_model_record(model)$dependent
}

#' @rdname ls_glm_set_dependent
#' @export
ls_glm_add_predictor <- function(model, variable) {
  record <- .rls_glm_model_record(model)
  variable <- .rls_model_validate_term(record$data, variable, response = record$dependent, what = "variable")
  if (identical(.rls_model_clean_factor_call(variable), record$dependent)) {
    stop("The dependent variable cannot also be an independent variable.", call. = FALSE)
  }
  if (!variable %in% record$predictors) {
    record$predictors <- c(record$predictors, variable)
    record <- .rls_glm_mark_model_changed(record)
    .rls_spreadplot_emit(
      "GLM_PREDICTOR_ADDED",
      group = record$group,
      sender_id = record$id,
      model_id = record$id,
      payload = list(variable = variable, model_version = record$model_version)
    )
  } else {
    record$status <- "Model unchanged."
  }
  invisible(.rls_assign_glm_model(record))
}

#' @rdname ls_glm_set_dependent
#' @export
ls_glm_add_interaction <- function(model, var1, var2) {
  record <- .rls_glm_model_record(model)
  var1 <- .rls_validate_glm_variable(record, var1, "var1")
  var2 <- .rls_validate_glm_variable(record, var2, "var2")
  term <- paste(var1, var2, sep = ":")
  predictors <- .rls_model_add_hierarchical_term(record$predictors, record$data, term, response = record$dependent)
  if (!identical(predictors, record$predictors)) {
    record$predictors <- predictors
    record <- .rls_glm_mark_model_changed(record)
  } else {
    record$status <- "Model unchanged."
  }
  invisible(.rls_assign_glm_model(record))
}

#' @rdname ls_glm_set_dependent
#' @export
ls_glm_add_polynomial <- function(model, variable, degree = 2) {
  record <- .rls_glm_model_record(model)
  variable <- .rls_validate_glm_variable(record, variable, "variable")
  if (!is.numeric(degree) || length(degree) != 1L || is.na(degree) || degree < 2) {
    stop("`degree` must be a single number >= 2.", call. = FALSE)
  }
  term <- sprintf("I(%s^%d)", variable, as.integer(degree))
  if (!term %in% record$predictors) {
    record$predictors <- c(record$predictors, term)
    record <- .rls_glm_mark_model_changed(record)
  } else {
    record$status <- "Model unchanged."
  }
  invisible(.rls_assign_glm_model(record))
}

#' @rdname ls_glm_set_dependent
#' @export
ls_glm_remove_predictor <- function(model, variable) {
  record <- .rls_glm_model_record(model)
  variable <- .rls_validate_protocol_name(variable, "variable")
  old <- record$predictors
  record$predictors <- .rls_model_remove_hierarchical_term(record$predictors, variable)
  if (!identical(old, record$predictors)) {
    record <- .rls_glm_mark_model_changed(record)
    .rls_spreadplot_emit(
      "GLM_PREDICTOR_REMOVED",
      group = record$group,
      sender_id = record$id,
      model_id = record$id,
      payload = list(variable = variable, model_version = record$model_version)
    )
  } else {
    record$status <- "Model unchanged."
  }
  invisible(.rls_assign_glm_model(record))
}

#' @rdname ls_glm_set_dependent
#' @export
ls_glm_predictors <- function(model) {
  .rls_glm_model_record(model)$predictors
}

#' @rdname ls_glm_set_dependent
#' @export
ls_glm_terms <- function(model) {
  .rls_glm_model_record(model)$predictors
}

#' @rdname ls_glm_set_dependent
#' @export
ls_glm_formula <- function(model) {
  record <- .rls_glm_model_record(model)
  .rls_glm_formula_object(record)
}

#' @rdname ls_glm_set_dependent
#' @export
ls_glm_fit <- function(model) {
  record <- .rls_glm_model_record(model)
  if (!length(record$predictors)) {
    stop("Add at least one independent variable before fitting.", call. = FALSE)
  }
  dataset <- .rls_dataset_record(record$group)
  record$analysis_backend <- .rls_analysis_backend(dataset, "linear_model")
  .rls_spreadplot_emit("GLM_REFIT_REQUESTED", group = record$group, sender_id = record$id, model_id = record$id)
  if (identical(record$analysis_backend, "multiple_imputation")) {
    extracted <- .rls_mi_fit_linear_model_record(record)
  } else {
    fit_record <- record
    fit_record$data <- .rls_glm_data_for_fit(record)
    complete <- .rls_glm_complete_data(fit_record)
    if (nrow(complete$data) <= length(record$predictors) + 1L) {
      stop("Not enough complete cases to fit the model.", call. = FALSE)
    }
    fit <- stats::lm(.rls_glm_formula_object(fit_record), data = complete$data)
    extracted <- .rls_glm_extract_fit(fit_record, fit, complete)
  }
  record[names(extracted)] <- extracted
  record$fit_version <- record$fit_version + 1L
  record$diagnostics_version <- record$diagnostics_version + 1L
  record$is_stale <- FALSE
  handle <- .rls_assign_glm_model(record)
  .rls_spreadplot_emit(
    "GLM_REFIT_COMPLETED",
    group = record$group,
    sender_id = record$id,
    model_id = record$id,
    rows = record$rows_used,
    payload = list(fit_version = record$fit_version, diagnostics_version = record$diagnostics_version)
  )
  .rls_glm_notify_diagnostic_plots(record)
  if (identical(record$analysis_backend, "multiple_imputation") && isTRUE(.rls_state$process_started)) {
    try(.rls_glm_sync_native_pooled(record), silent = TRUE)
  }
  handle
}

#' @rdname ls_glm_set_dependent
#' @export
ls_glm_coefficients <- function(model) {
  record <- .rls_glm_model_record(model)
  if (is.null(record$fit)) record <- .rls_glm_model_record(ls_glm_fit(model))
  record$coefficients
}

#' @rdname ls_glm_set_dependent
#' @export
ls_glm_coefficient_rows <- function(model) {
  record <- .rls_glm_model_record(model)
  if (is.null(record$fit)) record <- .rls_glm_model_record(ls_glm_fit(model))
  record$coefficient_rows
}

#' @rdname ls_glm_set_dependent
#' @export
ls_glm_fit_summary <- function(model) {
  record <- .rls_glm_model_record(model)
  if (is.null(record$fit)) record <- .rls_glm_model_record(ls_glm_fit(model))
  record$summary
}

#' @rdname ls_glm_set_dependent
#' @export
ls_glm_partial_r2 <- function(model) {
  coefs <- ls_glm_coefficients(model)
  stats::setNames(coefs$partial_r2, coefs$term)
}

#' @rdname ls_glm_set_dependent
#' @export
ls_glm_rows_used <- function(model) {
  record <- .rls_glm_model_record(model)
  if (is.null(record$fit)) record <- .rls_glm_model_record(ls_glm_fit(model))
  record$rows_used
}

#' @rdname ls_glm_set_dependent
#' @export
ls_glm_rows_excluded <- function(model) {
  record <- .rls_glm_model_record(model)
  if (is.null(record$fit)) record <- .rls_glm_model_record(ls_glm_fit(model))
  record$rows_excluded
}

#' @rdname ls_glm_set_dependent
#' @export
ls_glm_diagnostics <- function(model) {
  record <- .rls_glm_model_record(model)
  if (is.null(record$fit)) record <- .rls_glm_model_record(ls_glm_fit(model))
  record$diagnostics
}

#' @rdname ls_glm_set_dependent
#' @export
ls_glm_refresh_diagnostics <- function(model) {
  record <- .rls_glm_model_record(ls_glm_fit(model))
  .rls_glm_notify_diagnostic_plots(record)
  invisible(.rls_assign_glm_model(record))
}

#' @rdname ls_glm_set_dependent
#' @export
ls_glm_open_diagnostic <- function(model, type = c("residuals_vs_fitted", "observed_vs_fitted",
                                                   "residuals_fitted", "observed_fitted"),
                                   native = isTRUE(.rls_state$process_started)) {
  type <- match.arg(type)
  native_type <- switch(
    type,
    residuals_vs_fitted = "residuals_fitted",
    observed_vs_fitted = "observed_fitted",
    residuals_fitted = "residuals_fitted",
    observed_fitted = "observed_fitted"
  )
  record <- .rls_glm_model_record(model)
  if (is.null(record$fit)) record <- .rls_glm_model_record(ls_glm_fit(model))
  id <- paste(record$id, type, length(ls(.rls_state$glm_diagnostic_plots)) + 1L, sep = ":")
  native_plot_id <- NULL
  if (isTRUE(native)) {
    native_plot_id <- .rls_open_native_glm_diagnostic(record, native_type)
  }
  plot <- list(
    id = id,
    model_id = record$id,
    type = type,
    native_plot_id = native_plot_id,
    displayed_fit_version = record$fit_version,
    displayed_diagnostics_version = record$diagnostics_version,
    is_stale = FALSE,
    data = record$diagnostics
  )
  .rls_spreadplot_register(
    id,
    record$group,
    c("GLM_REFIT_COMPLETED", "GLM_DIAGNOSTICS_UPDATED", "ROW_SELECTION_CHANGED", "ROW_COLORS_CHANGED")
  )
  assign(id, plot, envir = .rls_state$glm_diagnostic_plots)
  .rls_spreadplot_emit(
    "DIAGNOSTIC_PLOT_OPENED",
    group = record$group,
    sender_id = id,
    model_id = record$id,
    payload = list(type = type, fit_version = record$fit_version)
  )
  structure(plot, class = "rlispstat_glm_diagnostic")
}

.rls_open_native_glm_diagnostic <- function(record, native_type) {
  if (!native_type %in% c("residuals_fitted", "observed_fitted")) {
    stop(sprintf("Diagnostic `%s` is not available for the native linear model.", native_type), call. = FALSE)
  }
  if (!length(record$predictors)) {
    stop("Add at least one independent variable before opening diagnostics.", call. = FALSE)
  }
  .rls_start_backend()
  dataset <- .rls_dataset_record(record$group)
  .rls_send(c(
    "REGISTER_DATASET",
    record$group,
    .rls_variable_payload(record$data, dataset$variable_metadata),
    .rls_dataframe_payload(record$data, dataset$variable_metadata, dataset_record = dataset)
  ))
  plots <- try(ls_plots(), silent = TRUE)
  group_has_plot <- !inherits(plots, "try-error") && nrow(plots) > 0L && any(plots$group == record$group)
  if (!group_has_plot) {
    ls_new_scatterplot(record$group, x = record$predictors[[1L]], y = record$dependent,
                       title = paste("GLM seed:", record$group))
  }
  .rls_send(c("MODEL_SET_Y", record$group, record$dependent))
  info <- try(ls_model_info(record$group), silent = TRUE)
  if (!inherits(info, "try-error")) {
    for (term in setdiff(info$terms, record$predictors)) {
      .rls_send(c("MODEL_REMOVE_TERM", record$group, term))
    }
  }
  for (term in record$predictors) {
    .rls_send(c("MODEL_ADD_TERM", record$group, term))
  }
  reply <- .rls_send(c("MODEL_OPEN_DIAGNOSTIC", record$group, native_type))
  records <- .rls_parse_records(reply)
  if (!length(records) || !nzchar(records[[1L]])) {
    stop("The native backend did not return a diagnostic plot id.", call. = FALSE)
  }
  records[[1L]]
}

#' @rdname ls_glm_set_dependent
#' @export
ls_glm_diagnostic_state <- function(plot) {
  if (!inherits(plot, "rlispstat_glm_diagnostic")) {
    stop("`plot` must be a LinkEDA GLM diagnostic handle.", call. = FALSE)
  }
  get(plot$id, envir = .rls_state$glm_diagnostic_plots)
}
