.rls_count_trial_specification <- function(data, trials) {
  if (is.character(trials) && length(trials) == 1L && nzchar(trials)) {
    if (!trials %in% names(data)) {
      stop(sprintf("Trials column `%s` was not found in the dataset.", trials),
           call. = FALSE)
    }
    if (!is.numeric(data[[trials]])) {
      stop("Trials must be a numeric variable or a fixed positive integer.",
           call. = FALSE)
    }
    return(list(variable = trials, constant = NA_real_))
  }
  value <- suppressWarnings(as.numeric(trials))
  if (length(value) != 1L || !is.finite(value) || value <= 0 ||
      abs(value - round(value)) > sqrt(.Machine$double.eps)) {
    stop("A fixed Trials value must be a positive integer.", call. = FALSE)
  }
  list(variable = "", constant = unname(value))
}

.rls_count_grouped_response <- function(record, data) {
  successes <- data[[record$response]]
  if (!is.numeric(successes)) {
    stop("Successes must be a numeric variable containing integer counts.",
         call. = FALSE)
  }
  trials <- if (nzchar(record$trials_variable %||% "")) {
    data[[record$trials_variable]]
  } else {
    rep(record$trials_constant, nrow(data))
  }
  missing <- is.na(successes) | is.na(trials)
  observed_successes <- successes[!missing]
  observed_trials <- trials[!missing]
  tolerance <- sqrt(.Machine$double.eps)
  if (any(!is.finite(observed_trials)) || any(observed_trials <= 0)) {
    stop("Trials must be positive.", call. = FALSE)
  }
  if (any(abs(observed_trials - round(observed_trials)) > tolerance)) {
    stop("Trials must contain integer counts.", call. = FALSE)
  }
  if (any(!is.finite(observed_successes)) || any(observed_successes < 0) ||
      any(observed_successes > observed_trials)) {
    stop("Binomial successes must be between 0 and the number of trials.",
         call. = FALSE)
  }
  if (any(abs(observed_successes - round(observed_successes)) > tolerance)) {
    stop("Binomial successes must contain integer counts.", call. = FALSE)
  }
  list(
    successes = as.numeric(successes),
    trials = as.numeric(trials),
    failures = as.numeric(trials - successes),
    proportion = as.numeric(successes / trials),
    missing = missing
  )
}

#' Count regression
#'
#' Fits Poisson, quasi-Poisson, negative-binomial, grouped-binomial,
#' beta-binomial, ceiling-hurdle beta-binomial, or perfect-score logistic count
#' models. `exposure`, when supplied to an unbounded count
#' model, is represented in the model as
#' `offset(log(exposure))`; the source data are never modified.
#'
#' @param data Registered dataset name, a data frame, or `NULL` for the active dataset.
#' @param response Non-negative integer response variable.
#' @param terms Predictor terms.
#' @param distribution One of `"poisson"`, `"quasipoisson"`,
#'   `"negative_binomial"`, `"binomial_trials"`, `"beta_binomial"`,
#'   `"hurdle_beta_binomial_ceiling"`, or `"perfect_score"`.
#' @param trials A fixed positive integer number of trials (or a numeric trials
#'   column) for bounded-count distributions.
#' @param exposure Optional positive numeric exposure variable.
#' @param scope One of `"all"`, `"selected"`, or `"unselected"`.
#' @param name Optional model id or dataset name for data-frame input.
#' @param native Logical; open/update the native result window.
#' @param term_types Optional named model-local predictor type overrides.
#' @param centered_predictors Optional model-local centered numeric predictors.
#' @param factor_reference_levels Optional named model-local reference categories.
#' @param offset Optional numeric column added to the linear predictor with a
#'   fixed coefficient of one. Unlike `exposure`, it is not log-transformed.
#' @param ... Arguments forwarded to the shared GLM constructor, including
#'   `diagnostic_seed` and explicit theoretical `response_bounds = c(lower, upper)`.
#'   NB diagnostics report probability above a known upper bound; the fitted
#'   distribution is not truncated. An explicit `theoretical_range` attribute on
#'   the response is also recognized. Observed maxima are never used as bounds.
#' @return An `rlispstat_count_regression` handle.
#' @export
ls_new_count_regression <- function(data = NULL, response = NULL, terms = NULL,
                                    distribution = c(
                                      "poisson", "quasipoisson", "negative_binomial",
                                      "binomial_trials", "beta_binomial",
                                      "hurdle_beta_binomial_ceiling", "perfect_score"),
                                    exposure = NULL, trials = NULL, scope = "all", name = NULL,
                                    native = TRUE, term_types = NULL,
                                    centered_predictors = NULL,
                                    factor_reference_levels = NULL, offset = NULL, ...) {
  distribution <- match.arg(distribution)
  ls_new_generalized_linear_model(
    data = data, response = response, terms = terms,
    family = if (identical(distribution, "quasipoisson")) "quasipoisson" else
      if (.rls_count_distribution_uses_trials(distribution)) "binomial" else "poisson",
    link = if (.rls_count_distribution_uses_trials(distribution)) "logit" else "log",
    scope = scope, name = name, native = native,
    term_types = term_types, centered_predictors = centered_predictors,
    factor_reference_levels = factor_reference_levels,
    .count_regression = TRUE, .count_distribution = distribution,
    .exposure = exposure, .trials = trials, offset = offset,
    .model_type = "count", ...
  )
}

#' @rdname ls_new_count_regression
#' @export
ls_count_regression_fit <- function(model) ls_generalized_linear_model_fit(model)

#' @rdname ls_new_count_regression
#' @export
ls_count_regression_coefficients <- function(model) ls_generalized_linear_model_coefficients(model)

#' @rdname ls_new_count_regression
#' @export
ls_count_regression_coefficient_rows <- function(model) ls_generalized_linear_model_coefficient_rows(model)

#' @rdname ls_new_count_regression
#' @export
ls_count_regression_fit_summary <- function(model) ls_generalized_linear_model_fit_summary(model)

#' @rdname ls_new_count_regression
#' @export
ls_count_regression_diagnostics <- function(model) ls_generalized_linear_model_diagnostics(model)

#' @rdname ls_new_count_regression
#' @export
ls_count_regression_open_diagnostic <- function(model, type, native = isTRUE(.rls_state$process_started)) {
  ls_generalized_linear_model_open_diagnostic(model, type = type, native = native)
}

#' @rdname ls_new_count_regression
#' @export
ls_count_regression_state <- function(model) .rls_generalized_glm_record(model)

#' @rdname ls_new_count_regression
#' @export
ls_count_regression_term_tests <- function(model) {
  ls_generalized_linear_model_term_tests(model)
}

#' @rdname ls_new_count_regression
#' @export
ls_count_regression_set_distribution <- function(model,
                                                 distribution = c(
                                                   "poisson", "quasipoisson", "negative_binomial",
                                                   "binomial_trials", "beta_binomial",
                                                   "hurdle_beta_binomial_ceiling", "perfect_score"),
                                                 trials = NULL) {
  distribution <- match.arg(distribution)
  record <- .rls_generalized_glm_record(model)
  if (!isTRUE(record$count_regression)) stop("The model is not a Count Model.", call. = FALSE)
  record$count_distribution <- distribution
  bounded <- .rls_count_distribution_uses_trials(distribution)
  record$family <- if (identical(distribution, "quasipoisson")) "quasipoisson" else
    if (bounded) "binomial" else "poisson"
  record$link <- if (bounded) "logit" else "log"
  if (bounded) {
    trial_spec <- .rls_count_trial_specification(
      record$data, trials %||%
        if (nzchar(record$trials_variable %||% "")) record$trials_variable else record$trials_constant
    )
    record$trials_variable <- trial_spec$variable
    record$trials_constant <- trial_spec$constant
    record$exposure <- ""
  }
  record$family_object <- .rls_glm_make_family(record$family, record$link)
  record$model_version <- record$model_version + 1L
  record$fit <- record$fitted_glm <- NULL
  handle <- .rls_assign_generalized_glm(record)
  if (length(record$terms) || isTRUE(record$allow_intercept_only)) ls_count_regression_fit(handle) else invisible(handle)
}

#' @rdname ls_new_count_regression
#' @export
ls_count_regression_set_trials <- function(model, trials) {
  record <- .rls_generalized_glm_record(model)
  if (!isTRUE(record$count_regression)) stop("The model is not a Count Model.", call. = FALSE)
  if (!.rls_count_distribution_uses_trials(record$count_distribution)) {
    stop("Trials are available only for bounded-count distributions.", call. = FALSE)
  }
  specification <- .rls_count_trial_specification(record$data, trials)
  record$trials_variable <- specification$variable
  record$trials_constant <- specification$constant
  record$model_version <- record$model_version + 1L
  record$fit <- record$fitted_glm <- NULL
  handle <- .rls_assign_generalized_glm(record)
  if (length(record$terms) || isTRUE(record$allow_intercept_only)) ls_count_regression_fit(handle) else invisible(handle)
}

#' @rdname ls_new_count_regression
#' @export
ls_count_regression_set_exposure <- function(model, exposure = NULL) {
  record <- .rls_generalized_glm_record(model)
  if (!isTRUE(record$count_regression)) stop("The model is not a Count Model.", call. = FALSE)
  exposure <- as.character(exposure %||% "")[[1L]]
  .rls_count_validate_exposure(record$data, exposure)
  record$exposure <- exposure
  record$model_version <- record$model_version + 1L
  record$fit <- record$fitted_glm <- NULL
  handle <- .rls_assign_generalized_glm(record)
  if (length(record$terms) || isTRUE(record$allow_intercept_only)) ls_count_regression_fit(handle) else invisible(handle)
}

#' @rdname ls_new_count_regression
#' @export
ls_compare_count_regression_models <- function(...,
                                               native = isTRUE(.rls_state$process_started),
                                               .native_id = NULL,
                                               .native_labels = NULL,
                                               .native_generation = 0L,
                                               .native_specification_revisions = integer(),
                                               .native_specification_fingerprints = character()) {
  models <- list(...)
  if (length(models) == 1L && is.list(models[[1L]]) &&
      !inherits(models[[1L]], "rlispstat_generalized_linear_model")) models <- models[[1L]]
  do.call(ls_compare_generalized_linear_models, c(models, list(
    native = native,
    .native_id = .native_id,
    .native_labels = .native_labels,
    .native_generation = .native_generation,
    .native_specification_revisions = .native_specification_revisions,
    .native_specification_fingerprints = .native_specification_fingerprints,
    .count_comparison = TRUE
  )))
}
