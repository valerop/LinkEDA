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
    stop("Binary Regression requires exactly two observed response values in the selected scope.", call. = FALSE)
  }
  event <- as.character(event %||% labels[[2L]])[[1L]]
  reference <- as.character(reference %||% setdiff(labels, event)[[1L]])[[1L]]
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
  table <- tryCatch(stats::drop1(fit, test = "Chisq"), error = function(e) NULL)
  if (is.null(table) || nrow(table) < 2L) return(data.frame())
  table <- as.data.frame(table, stringsAsFactors = FALSE)
  term <- rownames(table)
  keep <- term != "<none>"
  statistic_name <- intersect(c("LRT", "Deviance"), names(table))
  p_name <- grep("Pr\\(", names(table), value = TRUE)
  data.frame(
    term = term[keep],
    df = as.integer(table[keep, "Df"]),
    statistic = if (length(statistic_name)) as.numeric(table[keep, statistic_name[[1L]]]) else NA_real_,
    p_value = if (length(p_name)) as.numeric(table[keep, p_name[[1L]]]) else NA_real_,
    stringsAsFactors = FALSE
  )
}

.rls_binary_augment_coefficient_rows <- function(rows, link) {
  if (!is.data.frame(rows) || !nrow(rows)) return(rows)
  critical <- stats::qnorm(0.975)
  rows$ci_lower <- rows$estimate - critical * rows$std_error
  rows$ci_upper <- rows$estimate + critical * rows$std_error
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

.rls_binary_fit_record <- function(record) {
  fit_record <- record
  fit_record$data <- .rls_generalized_glm_data_for_fit(record)
  complete <- .rls_generalized_glm_complete_data(fit_record)
  if (nrow(complete$data) <= length(record$terms)) {
    stop("Not enough complete cases to fit the binary regression model.", call. = FALSE)
  }
  coding <- .rls_binary_resolve_coding(
    complete$data[[record$response]], record$event %||% NULL, record$reference %||% NULL
  )
  temporary_response <- ".rls_binary_response"
  while (temporary_response %in% names(complete$data)) temporary_response <- paste0(temporary_response, "_")
  complete$data[[temporary_response]] <- coding$binary
  fit_record$response <- temporary_response
  fit_record$data <- complete$data
  family_object <- stats::binomial(link = record$link)
  captured_warnings <- character()
  fit <- withCallingHandlers(
    stats::glm(.rls_generalized_glm_formula_object(fit_record), data = complete$data, family = family_object),
    warning = function(w) {
      captured_warnings <<- unique(c(captured_warnings, conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
  )
  extracted <- .rls_generalized_glm_extract_fit(fit_record, fit, complete)
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
  eta <- unname(stats::predict(fit, type = "link"))
  probability <- unname(stats::fitted(fit))
  calibration <- tryCatch(
    suppressWarnings(stats::coef(stats::glm(coding$binary ~ eta, family = stats::binomial()))),
    error = function(e) c(NA_real_, NA_real_)
  )
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
  extracted$term_tests <- .rls_binary_term_tests(fit)
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
#' @param link Either `"logit"` or `"probit"`.
#' @param event,reference Original response values defining the event and reference.
#' @param scope One of `"all"`, `"selected"`, or `"unselected"`.
#' @param name Optional model identifier.
#' @param native Open/synchronize the native result window.
#' @param term_types Optional named predictor type overrides.
#' @param model A fitted Binary Regression model handle.
#' @return An `rlispstat_binary_regression` model handle.
#' @export
ls_new_binary_regression <- function(data = NULL, response = NULL, terms = NULL,
                                     link = c("logit", "probit"), event = NULL,
                                     reference = NULL, scope = "all", name = NULL,
                                     native = TRUE, term_types = NULL,
                                     .selected_rows = NULL) {
  link <- match.arg(link)
  handle <- ls_new_generalized_linear_model(
    data = data, response = response, terms = NULL, family = "binomial", link = link,
    scope = scope, name = name, native = FALSE, term_types = term_types,
    .selected_rows = .selected_rows
  )
  record <- .rls_generalized_glm_record(handle)
  terms <- unlist(lapply(terms %||% character(), function(term) {
    term <- .rls_model_validate_term(record$data, term, response = record$response, what = "term")
    .rls_model_hierarchical_terms(record$data, term, response = record$response)
  }), use.names = FALSE)
  record$terms <- if (length(terms)) unique(as.character(terms)) else character()
  record$term_types <- .rls_generalized_glm_normalize_term_types(term_types, record$terms)
  record$binary_regression <- TRUE
  record$event <- if (is.null(event)) NULL else as.character(event)[[1L]]
  record$reference <- if (is.null(reference)) NULL else as.character(reference)[[1L]]
  record$status <- "Not fitted."
  handle <- .rls_assign_generalized_glm(record)
  class(handle) <- c("rlispstat_binary_regression", class(handle))
  handle <- ls_binary_regression_fit(handle)
  record <- .rls_generalized_glm_record(handle)
  if (isTRUE(native)) .rls_generalized_glm_sync_native(record)
  invisible(structure(handle, class = c("rlispstat_binary_regression", "rlispstat_generalized_linear_model")))
}

#' @rdname ls_new_binary_regression
#' @export
ls_binary_regression_fit <- function(model) {
  record <- .rls_generalized_glm_record(model)
  if (!isTRUE(record$binary_regression)) stop("The model is not a Binary Regression model.", call. = FALSE)
  extracted <- .rls_binary_fit_record(record)
  record[names(extracted)] <- extracted
  record$fit_statistics <- extracted$summary
  record$diagnostic_data <- extracted$diagnostics
  record$fit_version <- record$fit_version + 1L
  record$diagnostics_version <- record$diagnostics_version + 1L
  handle <- .rls_assign_generalized_glm(record)
  if (isTRUE(.rls_state$process_started)) try(.rls_generalized_glm_sync_native(record), silent = TRUE)
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

#' @rdname ls_new_binary_regression
#' @export
ls_binary_regression_term_tests <- function(model) {
  record <- .rls_generalized_glm_record(model)
  if (is.null(record$fit)) record <- .rls_generalized_glm_record(ls_binary_regression_fit(model))
  record$term_tests %||% data.frame()
}

#' Pairwise comparisons for a categorical term in a binary regression
#'
#' @param model Binary regression model.
#' @param term Simple categorical term.
#' @param scale For logit, `"odds_ratio"`, `"link"`, or `"probability"`; for
#'   probit, `"probability"` or `"link"`.
#' @param adjust Multiplicity adjustment passed to `emmeans`.
#' @return A data frame calculated by `emmeans` in R.
#' @export
ls_binary_regression_pairwise <- function(model, term, scale = NULL, adjust = "tukey") {
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop("Pairwise comparisons require the suggested R package `emmeans`.", call. = FALSE)
  }
  record <- .rls_generalized_glm_record(model)
  if (is.null(record$fit)) record <- .rls_generalized_glm_record(ls_binary_regression_fit(model))
  term <- .rls_validate_protocol_name(term, "term")
  if (!term %in% record$terms || grepl(":", term, fixed = TRUE)) {
    stop("Pairwise comparisons are available only for simple terms in the model.", call. = FALSE)
  }
  if (!is.factor(record$fit$model[[term]])) stop("Pairwise comparisons require a categorical term.", call. = FALSE)
  if (is.null(scale)) scale <- if (identical(record$link, "logit")) "odds_ratio" else "probability"
  allowed <- if (identical(record$link, "logit")) c("odds_ratio", "link", "probability") else c("probability", "link")
  scale <- match.arg(scale, allowed)
  grid <- emmeans::emmeans(record$fit, specs = term)
  if (identical(scale, "probability")) grid <- emmeans::regrid(grid, transform = "response")
  result <- as.data.frame(summary(emmeans::contrast(grid, method = "pairwise", adjust = adjust), infer = c(TRUE, TRUE)))
  result$term <- term
  result$scale <- scale
  if (identical(scale, "odds_ratio")) {
    result$odds_ratio <- exp(result$estimate)
    if ("lower.CL" %in% names(result)) result$lower.OR <- exp(result$lower.CL)
    if ("upper.CL" %in% names(result)) result$upper.OR <- exp(result$upper.CL)
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
#' @return A comparison object with `models` and adjacent `tests` data frames.
#' @export
ls_compare_binary_regression_models <- function(...) {
  models <- list(...)
  if (length(models) == 1L && is.list(models[[1L]]) && !inherits(models[[1L]], "rlispstat_generalized_linear_model")) {
    models <- models[[1L]]
  }
  if (length(models) < 2L) stop("Choose at least two stored Binary Regression models.", call. = FALSE)
  records <- lapply(models, .rls_generalized_glm_record)
  if (!all(vapply(records, function(x) isTRUE(x$binary_regression) && !is.null(x$fit), logical(1L)))) {
    stop("All models must be fitted Binary Regression models.", call. = FALSE)
  }
  base <- records[[1L]]
  same_identity <- vapply(records, function(x) {
    identical(x$group, base$group) && identical(x$response, base$response) &&
      identical(x$event, base$event) && identical(x$reference, base$reference)
  }, logical(1L))
  if (!all(same_identity)) {
    stop("Models must use the same dataset, response, and event/reference coding.", call. = FALSE)
  }
  summaries <- lapply(records, function(x) x$summary %||% list())
  model_table <- data.frame(
    model = vapply(records, function(x) x$id, character(1L)),
    link = vapply(records, function(x) x$link, character(1L)),
    n = vapply(summaries, function(x) as.integer(x$n_used %||% NA_integer_), integer(1L)),
    logLik = vapply(summaries, function(x) as.numeric(x$log_lik %||% NA_real_), numeric(1L)),
    AIC = vapply(summaries, function(x) as.numeric(x$aic %||% NA_real_), numeric(1L)),
    BIC = vapply(summaries, function(x) as.numeric(x$bic %||% NA_real_), numeric(1L)),
    AUC = vapply(summaries, function(x) as.numeric(x$auc %||% NA_real_), numeric(1L)),
    terms = vapply(records, function(x) paste(x$terms, collapse = " + "), character(1L)),
    stringsAsFactors = FALSE
  )
  tests <- vector("list", length(records) - 1L)
  for (i in seq_len(length(records) - 1L)) {
    left <- records[[i]]; right <- records[[i + 1L]]
    same_link <- identical(left$link, right$link) && identical(left$family, right$family)
    same_rows <- identical(sort(left$rows_used), sort(right$rows_used))
    nested <- all(left$terms %in% right$terms) || all(right$terms %in% left$terms)
    row <- data.frame(
      reduced = NA_character_, full = NA_character_, link = if (same_link) left$link else NA_character_,
      df = NA_integer_, statistic = NA_real_, p_value = NA_real_, available = FALSE,
      reason = if (!same_rows) "Different analysis rows: descriptive comparison only; AIC/BIC may not be directly comparable."
        else if (!same_link) "Different links: descriptive comparison only."
        else if (!nested) "Models are not nested." else "",
      stringsAsFactors = FALSE
    )
    if (same_rows && same_link && nested && !setequal(left$terms, right$terms)) {
      reduced <- if (length(left$terms) < length(right$terms)) left else right
      full <- if (length(left$terms) < length(right$terms)) right else left
      tab <- tryCatch(as.data.frame(stats::anova(reduced$fit, full$fit, test = "Chisq")), error = function(e) NULL)
      if (!is.null(tab) && nrow(tab) >= 2L) {
        p_name <- grep("Pr\\(", names(tab), value = TRUE)
        row$reduced <- reduced$id; row$full <- full$id
        row$df <- as.integer(abs(stats::df.residual(reduced$fit) - stats::df.residual(full$fit)))
        row$statistic <- as.numeric(tab$Deviance[[2L]])
        row$p_value <- if (length(p_name)) as.numeric(tab[[p_name[[1L]]]][[2L]]) else NA_real_
        row$available <- is.finite(row$statistic) && is.finite(row$p_value)
        row$reason <- if (row$available) "R: stats::anova(..., test = 'Chisq')" else "R did not return an estimable LRT."
      }
    }
    tests[[i]] <- row
  }
  structure(list(
    response = base$response, event = base$event, reference = base$reference,
    common_rows = all(vapply(records, function(x) identical(sort(x$rows_used), sort(base$rows_used)), logical(1L))),
    rows_used = base$rows_used, models = model_table, tests = do.call(rbind, tests)
  ), class = "rlispstat_binary_regression_comparison")
}
