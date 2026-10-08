.rls_binary_effective_levels <- function(response) {
  observed <- response[!is.na(response)]
  if (is.factor(observed)) {
    levels(droplevels(observed))
  } else if (is.numeric(observed)) {
    as.character(sort(unique(observed)))
  } else {
    levels(factor(as.character(observed)))
  }
}

.rls_binary_resolve_coding <- function(response, event = NULL, reference = NULL) {
  labels <- .rls_binary_effective_levels(response)
  if (length(labels) != 2L) {
    stop("Binary Model requires exactly two observed response values in the selected scope.", call. = FALSE)
  }
  if (!is.null(event)) event <- as.character(event)[[1L]]
  if (!is.null(reference)) reference <- as.character(reference)[[1L]]
  if (!is.null(event) && (is.na(event) || !event %in% labels))
    stop("The event must be an observed response value.", call. = FALSE)
  if (!is.null(reference) && (is.na(reference) || !reference %in% labels))
    stop("The reference must be an observed response value.", call. = FALSE)
  event <- event %||% if (!is.null(reference)) setdiff(labels, reference)[[1L]] else labels[[2L]]
  reference <- reference %||% setdiff(labels, event)[[1L]]
  if (!event %in% labels) stop(sprintf("Event `%s` is not an observed response value.", event), call. = FALSE)
  if (!reference %in% labels) stop(sprintf("Reference `%s` is not an observed response value.", reference), call. = FALSE)
  if (identical(event, reference) || !setequal(c(event, reference), labels)) {
    stop("Event and reference must be the two distinct observed response values.", call. = FALSE)
  }
  original <- as.character(response)
  list(
    event = event,
    reference = reference,
    event_count = sum(original == event, na.rm = TRUE),
    reference_count = sum(original == reference, na.rm = TRUE),
    labels = labels,
    binary = as.integer(original == event),
    original = original
  )
}

.rls_binary_auc <- function(observed, probability) {
  keep <- is.finite(observed) & is.finite(probability)
  observed <- observed[keep]
  probability <- probability[keep]
  n1 <- sum(observed == 1)
  n0 <- sum(observed == 0)
  if (!n1 || !n0) return(NA_real_)
  ranks <- rank(probability, ties.method = "average")
  (sum(ranks[observed == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

.rls_binary_term_tests <- function(fit) {
  .rls_model_global_term_tests(
    fit,
    attr(stats::terms(fit), "term.labels") %||% character(),
    likelihood_available = TRUE
  )
}

.rls_binary_augment_coefficient_rows <- function(rows, link) {
  if (!is.data.frame(rows) || !nrow(rows)) return(rows)
  critical <- stats::qnorm(0.975)
  rows$ci_lower <- rows$estimate - critical * rows$std_error
  rows$ci_upper <- rows$estimate + critical * rows$std_error
  # Keep the legacy odds-ratio fields genuinely odds-ratio specific. The
  # shared generalized fields carry the risk ratio for a log-binomial fit.
  if (identical(link, "logit")) {
    rows$odds_ratio <- exp(rows$estimate)
    rows$odds_ratio_lower <- exp(rows$ci_lower)
    rows$odds_ratio_upper <- exp(rows$ci_upper)
  } else {
    rows$odds_ratio <- NA_real_
    rows$odds_ratio_lower <- NA_real_
    rows$odds_ratio_upper <- NA_real_
  }
  rows
}

.rls_binary_prepare_generalized_fit <- function(record, fit_record, complete) {
  coding <- .rls_binary_resolve_coding(
    complete$data[[record$response]], record$event %||% NULL, record$reference %||% NULL
  )
  temporary_response <- ".rls_binary_response"
  while (temporary_response %in% names(complete$data)) temporary_response <- paste0(temporary_response, "_")
  complete$data[[temporary_response]] <- coding$binary
  fit_record$response <- temporary_response
  fit_record$data <- complete$data
  list(fit_record = fit_record, complete = complete, coding = coding)
}

.rls_binary_validate_log_fit <- function(fit, link, captured_warnings = character(),
                                         prefix = "The log-binomial model") {
  if (!identical(link, "log")) return(invisible(TRUE))
  problems <- character()
  if (!isTRUE(fit$converged)) problems <- c(problems, "stats::glm() did not converge")
  if (isTRUE(fit$boundary)) problems <- c(problems, "stats::glm() reported a boundary fit")
  probabilities <- suppressWarnings(as.numeric(stats::fitted(fit)))
  if (any(!is.finite(probabilities)) ||
      any(probabilities < -sqrt(.Machine$double.eps) |
          probabilities > 1 + sqrt(.Machine$double.eps))) {
    problems <- c(problems, "fitted probabilities are outside the admissible [0, 1] range")
  }
  if (length(problems)) {
    warning_text <- if (length(captured_warnings)) {
      paste0(" R warning(s): ", paste(unique(captured_warnings), collapse = "; "), ".")
    } else ""
    stop(paste0(
      prefix, " did not produce a valid fit: ",
      paste(unique(problems), collapse = "; "), ".", warning_text,
      " LinkEDA has not substituted a different model or link."
    ), call. = FALSE)
  }
  invisible(TRUE)
}

.rls_binary_finalize_generalized_fit <- function(record, fit_record, fit, complete,
                                                 coding, extracted,
                                                 captured_warnings = character()) {
  .rls_binary_validate_log_fit(fit, record$link, captured_warnings)
  extracted$coefficient_rows <- .rls_binary_augment_coefficient_rows(extracted$coefficient_rows, record$link)
  extracted$coefficients <- .rls_binary_augment_coefficient_rows(extracted$coefficients, record$link)
  diagnostics <- extracted$diagnostics
  diagnostics$observed_binary <- diagnostics$observed
  diagnostics$observed_label <- coding$original
  diagnostics$observed_original <- coding$original
  extracted$diagnostics <- diagnostics

  n <- length(coding$binary)
  log_lik <- extracted$summary$log_lik
  null_log_lik <- tryCatch(as.numeric(stats::logLik(stats::update(fit, . ~ 1))), error = function(e) NA_real_)
  global_lr <- unname(fit$null.deviance - fit$deviance)
  global_df <- unname(fit$df.null - fit$df.residual)
  cox_snell <- if (is.finite(log_lik) && is.finite(null_log_lik)) 1 - exp((2 / n) * (null_log_lik - log_lik)) else NA_real_
  max_cox_snell <- if (is.finite(null_log_lik)) 1 - exp(2 * null_log_lik / n) else NA_real_
  probability <- unname(stats::fitted(fit))
  # Logistic calibration always uses predicted log odds, whatever link fitted
  # the original model. Boundary probabilities have no finite log odds.
  calibration <- c(NA_real_, NA_real_)
  if (all(is.finite(probability) & probability > 0 & probability < 1)) {
    predicted_log_odds <- stats::qlogis(probability)
    calibration <- tryCatch(
      suppressWarnings(stats::coef(stats::glm(coding$binary ~ predicted_log_odds,
        family = stats::binomial(link = "logit")))),
      error = function(e) c(NA_real_, NA_real_))
  }
  separation_hint <- any(abs(stats::coef(fit)) > 25, na.rm = TRUE) ||
    any(probability < 1e-08 | probability > 1 - 1e-08, na.rm = TRUE)
  coefficient_se <- tryCatch(sqrt(diag(stats::vcov(fit))), error = function(e) rep(NA_real_, length(stats::coef(fit))))
  unstable <- which(!is.finite(stats::coef(fit)) | !is.finite(coefficient_se) | coefficient_se > 100)
  if (!isTRUE(fit$converged)) captured_warnings <- unique(c(captured_warnings, "stats::glm() did not converge."))
  if (separation_hint) captured_warnings <- unique(c(captured_warnings, "Possible complete or quasi-complete separation; inspect coefficients and fitted probabilities."))
  if (length(unstable)) captured_warnings <- unique(c(captured_warnings,
    sprintf("Very unstable coefficient estimate(s): %s. Coefficients, standard errors, and Wald intervals may be unreliable.",
      paste(names(stats::coef(fit))[unstable], collapse = ", "))))
  extracted$summary <- c(extracted$summary, list(
    df_model = global_df,
    global_lr = global_lr,
    global_p = if (is.finite(global_lr) && global_df > 0L) stats::pchisq(global_lr, global_df, lower.tail = FALSE) else NA_real_,
    mcfadden_r2 = if (is.finite(log_lik) && is.finite(null_log_lik) && null_log_lik != 0) 1 - log_lik / null_log_lik else NA_real_,
    cox_snell_r2 = cox_snell,
    nagelkerke_r2 = if (is.finite(cox_snell) && is.finite(max_cox_snell) && max_cox_snell > 0) cox_snell / max_cox_snell else NA_real_,
    auc = .rls_binary_auc(coding$binary, probability),
    calibration_intercept = unname(calibration[[1L]] %||% NA_real_),
    calibration_slope = unname(calibration[[2L]] %||% NA_real_),
    calibration_method = "Apparent logistic calibration: outcome ~ logit(predicted probability), on fitting cases",
    converged = isTRUE(fit$converged),
    iterations = as.integer(fit$iter %||% NA_integer_),
    boundary = isTRUE(fit$boundary),
    rank = as.integer(fit$rank %||% NA_integer_),
    parameter_count = length(stats::coef(fit)),
    rank_deficient = is.finite(fit$rank %||% NA_real_) && fit$rank < length(stats::coef(fit)),
    event = coding$event,
    reference = coding$reference,
    event_count = coding$event_count,
    reference_count = coding$reference_count,
    warnings = captured_warnings
  ))
  # The shared generalized extractor calculates the complete-term tests.
  # Keep this compatibility path for records produced by older callers.
  if (!is.data.frame(extracted$term_tests) || !nrow(extracted$term_tests)) {
    extracted$term_tests <- .rls_binary_term_tests(fit)
  }
  extracted$event <- coding$event
  extracted$reference <- coding$reference
  extracted$warnings <- captured_warnings
  extracted$status <- sprintf(
    "Binary regression fitted in R using stats::glm() with binomial(link = '%s'); %d rows used, %d excluded.",
    record$link, length(complete$rows), complete$excluded
  )
  extracted
}

#' Fit a binary regression model in R
#'
#' Fits `stats::glm(..., family = binomial(link = ...))` after explicitly coding
#' the chosen event as 1 and the reference as 0. The source response is never
#' modified.
#'
#' @param data Registered dataset name, data frame, or `NULL` for the active dataset.
#' @param response Binary response variable.
#' @param terms Predictor terms.
#' @param link One of the supported binomial links: `"logit"`, `"log"`,
#'   `"probit"`, or `"cloglog"`.
#' @param event,reference Original response values defining the event and reference.
#' @param scope One of `"all"`, `"selected"`, or `"unselected"`.
#' @param name Optional model identifier.
#' @param native Open/synchronize the native result window.
#' @param offset Optional numeric column added to the link-scale predictor with
#'   a fixed coefficient of one.
#' @param term_types Optional named predictor type overrides.
#' @param model A fitted Binary Regression model handle.
#' @return An `rlispstat_binary_regression` model handle.
#' @export
ls_new_binary_regression <- function(data = NULL, response = NULL, terms = NULL,
                                     link = c("logit", "log", "probit", "cloglog"), event = NULL,
                                     reference = NULL, scope = "all", name = NULL,
                                     native = TRUE, term_types = NULL,
                                     centered_predictors = NULL,
                                     factor_reference_levels = NULL,
                                     .selected_rows = NULL,
                                     .native_generation = 0L,
                                     .comparison_rows = NULL, offset = NULL) {
  link <- match.arg(link)
  handle <- ls_new_generalized_linear_model(
    data = data, response = response, terms = NULL, family = "binomial", link = link,
    offset = offset,
    scope = scope, name = name, native = FALSE, term_types = term_types,
    centered_predictors = centered_predictors,
    factor_reference_levels = factor_reference_levels,
    .model_type = "binary",
    .selected_rows = .selected_rows,
    .native_generation = .native_generation,
    .comparison_rows = .comparison_rows
  )
  record <- .rls_generalized_glm_record(handle)
  terms <- unlist(lapply(terms %||% character(), function(term) {
    term <- .rls_model_validate_term(record$data, term, response = record$response, what = "term")
    .rls_model_hierarchical_terms(record$data, term, response = record$response)
  }), use.names = FALSE)
  record$terms <- if (length(terms)) unique(as.character(terms)) else character()
  record$term_types <- .rls_generalized_glm_normalize_term_types(term_types, record$terms)
  # The shared constructor initially receives no terms so that this frontend
  # can enable its valid intercept-only state.  Once the Binary Regression
  # terms have been resolved, retain only transformations that refer to the
  # now-effective model terms.  These remain model-local; the data set is not
  # altered.
  term_variables <- if (length(record$terms)) {
    setdiff(all.vars(.rls_model_formula_object(record$response, record$terms)), record$response)
  } else {
    character()
  }
  record$centered_predictors <- intersect(
    unique(as.character(centered_predictors %||% character())), term_variables
  )
  references <- factor_reference_levels %||% list()
  if (!is.list(references)) references <- as.list(references)
  if (is.null(names(references))) references <- list()
  record$factor_reference_levels <- references[
    intersect(names(references), term_variables)
  ]
  record$binary_regression <- TRUE
  record$allow_intercept_only <- TRUE
  record$event <- if (is.null(event)) NULL else as.character(event)[[1L]]
  record$reference <- if (is.null(reference)) NULL else as.character(reference)[[1L]]
  record$status <- "Not fitted."
  if (isTRUE(native)) .rls_start_backend()
  record$native_sync_enabled <- isTRUE(native)
  handle <- .rls_assign_generalized_glm(record)
  class(handle) <- c("rlispstat_binary_regression", class(handle))
  handle <- ls_binary_regression_fit(handle)
  record <- .rls_generalized_glm_record(handle)
  record$native_sync_enabled <- isTRUE(native)
  handle <- .rls_assign_generalized_glm(record)
  invisible(structure(handle, class = c("rlispstat_binary_regression", "rlispstat_generalized_linear_model")))
}

#' @rdname ls_new_binary_regression
#' @export
ls_binary_regression_fit <- function(model) {
  record <- .rls_generalized_glm_record(model)
  if (!isTRUE(record$binary_regression)) stop("The model is not a Binary Model.", call. = FALSE)
  record$allow_intercept_only <- TRUE
  handle <- .rls_assign_generalized_glm(record)
  handle <- ls_generalized_linear_model_fit(handle)
  invisible(structure(handle, class = c("rlispstat_binary_regression", "rlispstat_generalized_linear_model")))
}

#' @rdname ls_new_binary_regression
#' @export
ls_binary_regression_state <- function(model) .rls_generalized_glm_record(model)

#' @rdname ls_new_binary_regression
#' @export
ls_binary_regression_coefficients <- function(model) {
  record <- .rls_generalized_glm_record(model)
  if (is.null(record$fit)) record <- .rls_generalized_glm_record(ls_binary_regression_fit(model))
  record$coefficient_rows
}

#' @rdname ls_new_binary_regression
#' @export
ls_binary_regression_fit_summary <- function(model) {
  record <- .rls_generalized_glm_record(model)
  if (is.null(record$fit)) record <- .rls_generalized_glm_record(ls_binary_regression_fit(model))
  record$summary
}

#' @rdname ls_new_binary_regression
#' @export
ls_binary_regression_diagnostics <- function(model) {
  record <- .rls_generalized_glm_record(model)
  if (is.null(record$fit)) record <- .rls_generalized_glm_record(ls_binary_regression_fit(model))
  record$diagnostics
}

#' Open a linked Binary Regression diagnostic plot
#'
#' @param model A fitted Binary Regression model.
#' @param type Diagnostic plot type.
#' @param native Open the native LinkEDA plot window when available.
#' @return A linked diagnostic handle.
#' @export
ls_binary_regression_open_diagnostic <- function(
    model,
    type = c("observed_vs_fitted", "residuals_vs_fitted", "residual_histogram",
             "normal_qq", "scale_location", "residuals_leverage",
             "cooks_distance", "roc_curve", "calibration_plot"),
    native = isTRUE(.rls_state$process_started)) {
  ls_generalized_linear_model_open_diagnostic(model, match.arg(type), native = native)
}

#' @rdname ls_new_binary_regression
#' @export
ls_binary_regression_term_tests <- function(model) {
  ls_generalized_linear_model_term_tests(model)
}

#' Pairwise comparisons for a categorical term in a binary regression
#'
#' @param model Binary regression model.
#' @param term Simple categorical term.
#' @param scale For logit, `"odds_ratio"`, `"link"`, or `"probability"`; for
#'   log, `"risk_ratio"`, `"link"`, or `"probability"`; for the other links,
#'   `"probability"` or `"link"`.
#' @param adjust Multiplicity adjustment passed to `emmeans`.
#' @return A data frame calculated by `emmeans` in R.
#' @export
ls_binary_regression_pairwise <- function(model, term, scale = NULL, adjust = "tukey") {
  record <- .rls_generalized_glm_record(model)
  if (is.null(record$fit)) record <- .rls_generalized_glm_record(ls_binary_regression_fit(model))
  if (is.null(scale)) {
    scale <- if (identical(record$link, "logit")) "odds_ratio" else
      if (identical(record$link, "log")) "risk_ratio" else "probability"
  }
  allowed <- if (identical(record$link, "logit")) {
    c("odds_ratio", "link", "probability")
  } else if (identical(record$link, "log")) {
    c("risk_ratio", "link", "probability")
  } else c("probability", "link")
  scale <- match.arg(scale, allowed)
  result <- .rls_regression_pairwise_record(record, term, scale, adjust)
  # Preserve the established public aliases while adding the standardized
  # pooled columns shared by every regression family.
  if (identical(scale, "odds_ratio")) {
    result$lower.OR <- result$odds_ratio_conf_low
    result$upper.OR <- result$odds_ratio_conf_high
  }
  result
}

#' Compare stored Binary Regression models
#'
#' Models are never silently refitted. A likelihood-ratio test is calculated
#' by R only for adjacent models with identical data/response coding, rows,
#' family and link, and hierarchically nested term sets. Models with different
#' links or analysis rows retain descriptive AIC/BIC/AUC comparison but no LRT.
#'
#' @param ... Binary Regression model handles, or one list of handles.
#' @param native Logical; open the native Windows comparison window.
#' @return A comparison object with `models` and adjacent `tests` data frames.
#' @export
ls_compare_binary_regression_models <- function(...,
                                                native = isTRUE(.rls_state$process_started),
                                                .native_id = NULL,
                                                .native_labels = NULL,
                                                .native_generation = 0L,
                                                .native_specification_revisions = integer(),
                                                .native_specification_fingerprints = character()) {
  models <- list(...)
  if (length(models) == 1L && is.list(models[[1L]]) && !inherits(models[[1L]], "rlispstat_generalized_linear_model")) {
    models <- models[[1L]]
  }
  if (length(models) < 1L) stop("Choose at least one stored Binary Model.", call. = FALSE)
  do.call(ls_compare_generalized_linear_models, c(models, list(
    native = native,
    .native_id = .native_id,
    .native_labels = .native_labels,
    .native_generation = .native_generation,
    .native_specification_revisions = .native_specification_revisions,
    .native_specification_fingerprints = .native_specification_fingerprints,
    .binary_comparison = TRUE
  )))
}
