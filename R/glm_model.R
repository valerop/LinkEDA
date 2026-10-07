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
  .rls_glm_mark_diagnostic_plots_stale(record$id)
  .rls_spreadplot_emit(
    "GLM_SPEC_CHANGED",
    group = record$group,
    sender_id = record$id,
    model_id = record$id,
    payload = list(model_version = record$model_version)
  )
  record
}

.rls_glm_mark_diagnostic_plots_stale <- function(model_id) {
  ids <- ls(.rls_state$glm_diagnostic_plots, all.names = TRUE)
  for (id in ids) {
    plot <- get(id, envir = .rls_state$glm_diagnostic_plots)
    if (!identical(plot$model_id, model_id)) next
    plot$is_stale <- TRUE
    plot$data <- data.frame()
    assign(id, plot, envir = .rls_state$glm_diagnostic_plots)
  }
  invisible(TRUE)
}

.rls_glm_refresh_diagnostic_plots <- function(record) {
  ids <- ls(.rls_state$glm_diagnostic_plots, all.names = TRUE)
  for (id in ids) {
    plot <- get(id, envir = .rls_state$glm_diagnostic_plots)
    if (identical(plot$model_id, record$id)) {
      plot$displayed_fit_version <- record$fit_version
      plot$displayed_diagnostics_version <- record$diagnostics_version
      plot$is_stale <- FALSE
      plot$data <- if (isTRUE(plot$type %in% c(
          "observed_predicted_score_distribution",
          "observed_predicted_distribution", "boundary_zero_fit"))) {
        record$summary$observed_predicted_distribution %||% data.frame()
      } else record$diagnostics
      assign(id, plot, envir = .rls_state$glm_diagnostic_plots)
    }
  }
  invisible(TRUE)
}

.rls_glm_notify_diagnostic_plots <- function(record) {
  .rls_glm_refresh_diagnostic_plots(record)
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

.rls_validate_glm_variable <- function(record, variable, role, numeric_only = TRUE) {
  variable <- .rls_validate_protocol_name(variable, role)
  if (!variable %in% names(record$data)) {
    stop(sprintf("Column `%s` was not found in the dataset.", variable), call. = FALSE)
  }
  if (numeric_only && !is.numeric(record$data[[variable]])) {
    stop(sprintf("`%s` must be numeric for this GLM phase.", variable), call. = FALSE)
  }
  variable
}

.rls_glm_formula_object <- function(record) {
  .rls_model_formula_object(record$dependent, record$predictors)
}

.rls_glm_data_for_fit <- function(record) {
  data <- .rls_model_data_for_term_types(
    record$data, record$term_types %||% list(), response = record$dependent
  )
  .rls_glm_apply_factor_references(data, record$factor_reference_levels %||% list())
}

.rls_glm_apply_factor_references <- function(data, references = list()) {
  references <- references %||% list()
  if (!length(references)) return(data)
  for (variable in intersect(names(references), names(data))) {
    reference <- as.character(references[[variable]])[[1L]]
    values <- data[[variable]]
    if (!is.factor(values) || is.ordered(values)) {
      values <- .rls_model_factor_interpretation(values)
    }
    if (nzchar(reference) && reference %in% levels(values)) {
      data[[variable]] <- stats::relevel(values, ref = reference)
    }
  }
  data
}

.rls_glm_complete_data <- function(record) {
  .rls_model_complete_data(
    record$data,
    .rls_glm_formula_object(record),
    record$scope %||% "all",
    record$selected_rows %||% integer()
  )
}

.rls_glm_center_complete_data <- function(complete, centered_predictors = character()) {
  centered_predictors <- unique(as.character(centered_predictors %||% character()))
  centered_predictors <- intersect(centered_predictors, names(complete$data))
  centers <- numeric()
  for (variable in centered_predictors) {
    values <- complete$data[[variable]]
    if (!is.numeric(values)) next
    center <- mean(values, na.rm = TRUE)
    if (!is.finite(center)) next
    complete$data[[variable]] <- values - center
    centers[[variable]] <- center
  }
  complete$predictor_centers <- centers
  complete
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
  df_resid <- stats::df.residual(fit)
  non_intercept <- coef_matrix$term != "(Intercept)"
  coef_matrix$partial_r <- NA_real_
  coef_matrix$partial_r[non_intercept] <- .rls_model_partial_r(
    coef_matrix$t_value[non_intercept], df_resid
  )
  # Standardize only an ordinary numeric main effect, using exactly the
  # observations in this fitted model. MI coefficient rows deliberately stay
  # unavailable: a pooled standardized effect needs its own estimand.
  coef_matrix$standardized_beta <- NA_real_
  model_data <- stats::model.frame(fit)
  outcome_sd <- stats::sd(stats::model.response(model_data))
  term_delta_r2 <- .rls_model_term_delta_r2(fit)
  coef_matrix$delta_r2 <- NA_real_
  mm <- stats::model.matrix(fit)
  assignment <- attr(mm, "assign")
  term_labels <- attr(stats::terms(fit), "term.labels")
  for (term_index in seq_along(term_labels)) {
    term_label <- term_labels[[term_index]]
    columns <- colnames(mm)[assignment == term_index]
    if (length(columns) == 1L &&
        length(.rls_model_interaction_parts(term_label)) < 2L &&
        !identical(.rls_model_semantic_term_type(record$data, term_label, fit$xlevels), "factor")) {
      row <- match(columns[[1L]], coef_matrix$term)
      if (!is.na(row)) coef_matrix$delta_r2[[row]] <- term_delta_r2[[term_label]] %||% NA_real_
      if (!is.na(row) && term_label %in% names(model_data) &&
          is.numeric(model_data[[term_label]]) && is.finite(outcome_sd) &&
          outcome_sd > 0) {
        predictor_sd <- stats::sd(model_data[[term_label]])
        if (is.finite(predictor_sd) && predictor_sd > 0) {
          coef_matrix$standardized_beta[[row]] <-
            coef_matrix$estimate[[row]] * predictor_sd / outcome_sd
        }
      }
    }
  }
  coef_matrix <- coef_matrix[, c("term", "estimate", "std_error", "t_value", "p_value", "partial_r", "standardized_beta", "delta_r2")]
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
  term_tests <- .rls_model_term_omnibus_tests(
    fit, term_labels, test = "wald_f"
  )
  parent_test_terms <- .rls_model_parent_test_terms(fit, record$data)
  parent_term_tests <- term_tests[vapply(term_tests$term, function(term) {
    any(vapply(parent_test_terms, .rls_model_terms_equivalent, logical(1L),
               b = term))
  }, logical(1L)), , drop = FALSE]
  coefficient_rows <- .rls_model_coefficient_display_rows(
    fit,
    transform(coef_matrix, statistic = t_value),
    record$data,
    record$predictors,
    statistic_name = "t",
    term_delta_r2 = term_delta_r2
  )
  coefficient_rows <- .rls_model_apply_parent_term_tests(
    coefficient_rows, parent_term_tests
  )
  list(
    fit = fit,
    coefficients = coef_matrix,
    coefficient_rows = coefficient_rows,
    parent_term_tests = parent_term_tests,
    term_tests = .rls_as_global_term_tests(term_tests),
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
    predictor_centers = complete$predictor_centers %||% numeric(),
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
    centered_predictors = character(),
    factor_reference_levels = list(),
    selected_rows = integer(),
    model_version = 0L,
    fit_version = 0L,
    diagnostics_version = 0L,
    # Models created from R do not publish every intermediate fit until a
    # native window has actually been opened.  Native-originated fit requests
    # temporarily disable this flag and publish one compact MODEL_UPDATE after
    # the calculation, avoiding a redundant dataset registration and
    # MODEL_OPEN_POOLED round trip.
    native_sync_enabled = FALSE,
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
    model_record$native_sync_enabled <- TRUE
    model <- .rls_assign_glm_model(model_record)
    .rls_glm_sync_native_pooled(model_record)
    return(model)
  }
  numeric <- .rls_numeric_variable_names(record$data, record$variable_metadata)
  .rls_start_backend()
  .rls_register_native_dataset_if_needed(record, visible = TRUE)
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
  # A response cannot remain anywhere in the model's right-hand side.  Remove
  # both the direct term and every interaction containing it, then prune all
  # model-local interpretation metadata for the removed variable.
  record$predictors <- .rls_model_remove_hierarchical_term(record$predictors, variable)
  remove_named_term_state <- function(values) {
    if (is.null(values) || !length(values) || is.null(names(values))) return(values)
    keep <- !vapply(names(values), function(term) {
      variable %in% .rls_model_interaction_parts(term)
    }, logical(1L))
    values[keep]
  }
  record$term_types <- remove_named_term_state(record$term_types)
  record$factor_reference_levels <- remove_named_term_state(record$factor_reference_levels)
  record$centered_predictors <- .rls_model_remove_hierarchical_term(
    record$centered_predictors %||% character(), variable
  )
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
  var1 <- .rls_validate_glm_variable(record, var1, "var1", numeric_only = FALSE)
  var2 <- .rls_validate_glm_variable(record, var2, "var2", numeric_only = FALSE)
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
  if (!is.numeric(degree) || length(degree) != 1L || !is.finite(degree) || degree != trunc(degree) || degree < 2 || degree > 99) {
    stop("`degree` must be a whole number from 2 to 99.", call. = FALSE)
  }
  term <- .rls_model_polynomial_expr(variable, degree)
  predictors <- .rls_model_add_hierarchical_term(record$predictors, record$data, term, record$dependent)
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
  record <- .rls_apply_scope_to_model_request(record, dataset)
  record$analysis_backend <- .rls_analysis_backend(dataset, "linear_model")
  executed_r_code <- .rls_linear_executed_r_code(
    record, identical(record$analysis_backend, "multiple_imputation")
  )
  .rls_spreadplot_emit("GLM_REFIT_REQUESTED", group = record$group, sender_id = record$id, model_id = record$id)
  if (identical(record$analysis_backend, "multiple_imputation")) {
    extracted <- .rls_mi_fit_linear_model_record(record)
  } else {
    fit_record <- record
    fit_record$data <- .rls_glm_data_for_fit(record)
    complete <- .rls_glm_complete_data(fit_record)
    complete <- .rls_glm_center_complete_data(complete, record$centered_predictors)
    if (nrow(complete$data) <= length(record$predictors) + 1L) {
      stop("Not enough complete cases to fit the model.", call. = FALSE)
    }
    fit <- stats::lm(
      .rls_glm_formula_object(fit_record), data = complete$data,
      na.action = stats::na.fail
    )
    extracted <- .rls_glm_extract_fit(fit_record, fit, complete)
  }
  record[names(extracted)] <- extracted
  verification <- .rls_linear_verification_r_code(
    record, identical(record$analysis_backend, "multiple_imputation")
  )
  record <- .rls_attach_analysis_provenance(
    record,
    executed_r_code,
    title = if (identical(record$analysis_backend, "multiple_imputation")) {
      "Linear Model \u2014 Multiple Imputation"
    } else "Linear Model",
    output_code = .rls_linear_output_r_code(
      identical(record$analysis_backend, "multiple_imputation")
    ),
    verification_code = list(model = verification$code),
    verification_variables = verification$variables,
    verification_warnings = verification$warnings
  )
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
  if (isTRUE(record$native_sync_enabled) &&
      identical(record$analysis_backend, "multiple_imputation") &&
      isTRUE(.rls_state$process_started)) {
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
ls_glm_partial_r <- function(model) {
  coefs <- ls_glm_coefficients(model)
  stats::setNames(coefs$partial_r, coefs$term)
}

#' @rdname ls_glm_set_dependent
#' @export
ls_glm_delta_r2 <- function(model) {
  rows <- ls_glm_coefficient_rows(model)
  values <- rows$delta_r2
  names(values) <- rows$term
  values[is.finite(values)]
}

#' @rdname ls_glm_set_dependent
#' @export
ls_glm_partial_r2 <- function(model) {
  partial <- ls_glm_partial_r(model)
  partial^2
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
ls_glm_open_diagnostic <- function(model, type = c("observed_vs_fitted", "residuals_vs_fitted",
                                                   "residual_histogram", "normal_qq",
                                                   "scale_location", "residuals_leverage",
                                                   "cooks_distance", "observed_fitted",
                                                   "residuals_fitted",
                                                   "observed_predicted_score_distribution"),
                                   native = isTRUE(.rls_state$process_started)) {
  type <- match.arg(type)
  native_type <- switch(
    type,
    residuals_vs_fitted = "residuals_fitted",
    observed_vs_fitted = "observed_fitted",
    residual_histogram = "residual_histogram",
    normal_qq = "normal_qq",
    scale_location = "scale_location",
    residuals_leverage = "residuals_leverage",
    cooks_distance = "cooks_distance",
    residuals_fitted = "residuals_fitted",
    observed_fitted = "observed_fitted",
    observed_predicted_score_distribution =
      "observed_predicted_score_distribution"
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
    data = if (identical(type, "observed_predicted_score_distribution"))
      record$summary$observed_predicted_distribution %||% data.frame()
    else record$diagnostics
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
  if (!native_type %in% c("observed_fitted", "residuals_fitted",
                          "residual_histogram", "normal_qq", "scale_location",
                          "residuals_leverage", "cooks_distance",
                          "observed_predicted_score_distribution")) {
    stop(sprintf("Diagnostic `%s` is not available for the native linear model.", native_type), call. = FALSE)
  }
  if (!length(record$predictors)) {
    stop("Add at least one independent variable before opening diagnostics.", call. = FALSE)
  }
  .rls_start_backend()
  dataset <- .rls_dataset_record(record$group)
  .rls_register_native_dataset_if_needed(dataset, record$data, visible = TRUE)
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
