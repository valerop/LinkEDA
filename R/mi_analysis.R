.rls_analysis_backend <- function(dataset, analysis_type = "analysis") {
  record <- if (is.list(dataset) && !is.null(dataset$data)) dataset else .rls_dataset_record(dataset)
  if (identical(record$dataset_type %||% "data_frame", "multiple_imputation")) {
    "multiple_imputation"
  } else {
    "ordinary"
  }
}

.rls_mi_debug_enabled <- function() {
  isTRUE(getOption("LinkEDA.mi_debug", FALSE)) ||
    identical(tolower(Sys.getenv("LINKEDA_MI_DEBUG", unset = "")), "true") ||
    identical(Sys.getenv("LINKEDA_MI_DEBUG", unset = ""), "1")
}

.rls_mi_trace <- function(stage, ..., .elapsed = NULL) {
  if (!.rls_mi_debug_enabled()) return(invisible(FALSE))
  detail <- paste(..., collapse = " ")
  timing <- if (is.null(.elapsed)) "" else sprintf(" [%.3f s]", as.numeric(.elapsed))
  message(sprintf("[LinkEDA MI] %s%s%s", stage,
                  if (nzchar(detail)) paste0(": ", detail) else "", timing))
  invisible(TRUE)
}

.rls_mi_elapsed_text <- function(seconds) {
  seconds <- max(0, as.numeric(seconds))
  sprintf("%02d:%02d", as.integer(seconds) %/% 60L, as.integer(seconds) %% 60L)
}

.rls_mi_progress_event <- function(task_id, group, phase, status = "running",
                                   completed = 0L, total = 0L, running = 0L,
                                   workers = 1L, imputation = 0L,
                                   fit_seconds = NA_real_, elapsed = 0,
                                   warnings = 0L, failures = 0L,
                                   cancel_path = "", message = "") {
  event <- list(
    task_id = as.character(task_id %||% "generalized-model")[[1L]],
    group = as.character(group %||% "")[[1L]], phase = as.character(phase)[[1L]],
    status = as.character(status)[[1L]], completed = as.integer(completed),
    total = as.integer(total), running = as.integer(running), workers = as.integer(workers),
    imputation = as.integer(imputation), fit_seconds = as.numeric(fit_seconds),
    elapsed = as.numeric(elapsed), warnings = as.integer(warnings),
    failures = as.integer(failures), cancel_path = as.character(cancel_path)[[1L]],
    message = as.character(message %||% "")[[1L]]
  )
  callback <- getOption("LinkEDA.mi_progress_callback", NULL)
  if (is.function(callback)) try(callback(event), silent = TRUE)
  if (isTRUE(.rls_state$process_started)) {
    number <- function(x) if (is.finite(x)) format(x, digits = 17L, trim = TRUE) else "NA"
    try(.rls_send(c("GGLM_PROGRESS", event$task_id, event$group, event$phase,
      event$status, as.character(event$completed), as.character(event$total),
      as.character(event$running), as.character(event$workers),
      as.character(event$imputation), number(event$fit_seconds), number(event$elapsed),
      as.character(event$warnings), as.character(event$failures), event$cancel_path,
      event$message), expect_reply = TRUE), silent = TRUE)
  }
  invisible(event)
}

.rls_mi_cancelled <- function(cancel_path) {
  nzchar(cancel_path %||% "") && file.exists(cancel_path)
}

.rls_mi_cancel_condition <- function(completed, total, elapsed) {
  structure(list(message = sprintf(
    "Generalized-model task cancelled after %d of %d imputations (%s).",
    completed, total, .rls_mi_elapsed_text(elapsed)), call = NULL,
    completed = completed, total = total, elapsed = elapsed),
    class = c("linkeda_mi_cancelled", "error", "condition"))
}

#' Inspect the analysis backend selected for a dataset
#'
#' @param dataset Registered dataset name, or `NULL` for the active dataset.
#' @param analysis_type Optional analysis label.
#' @return A list describing the selected analysis backend.
#' @export
ls_analysis_backend <- function(dataset = NULL, analysis_type = "analysis") {
  record <- .rls_dataset_record(dataset)
  backend <- .rls_analysis_backend(record, analysis_type)
  list(
    dataset_id = record$dataset_id %||% record$group,
    dataset_type = record$dataset_type %||% "data_frame",
    analysis_type = analysis_type,
    backend = backend,
    m = if (identical(backend, "multiple_imputation")) {
      record$imputation_count %||% length(record$completed_datasets %||% list())
    } else {
      NA_integer_
    }
  )
}

.rls_mi_is_dataset <- function(record) {
  identical(record$dataset_type %||% "data_frame", "multiple_imputation")
}

.rls_mi_completed_datasets <- function(record) {
  if (!.rls_mi_is_dataset(record)) {
    stop("Dataset is not a multiple-imputation dataset.", call. = FALSE)
  }
  completed <- record$completed_datasets %||% list()
  if (!length(completed)) {
    stop("This multiple-imputation dataset does not contain completed datasets.", call. = FALSE)
  }
  if (!all(vapply(completed, is.data.frame, logical(1L)))) {
    stop("Completed imputations must all be data frames.", call. = FALSE)
  }
  row_counts <- vapply(completed, nrow, integer(1L))
  if (length(unique(row_counts)) != 1L) {
    stop("Completed imputations must preserve the same original rows.", call. = FALSE)
  }
  completed
}

.rls_mi_original_row_ids <- function(record) {
  ids <- record$original_row_ids %||% seq_len(nrow(record$data))
  as.integer(ids)
}

.rls_mi_imputed_model_variables <- function(record, response, terms,
                                            additional_variables = character(),
                                            original_rows = NULL) {
  if (!.rls_mi_is_dataset(record)) return(character())
  formula <- .rls_model_formula_object(response, terms %||% character())
  variables <- unique(c(all.vars(formula), as.character(additional_variables %||% character())))
  variables <- variables[nzchar(variables)]
  mask <- record$missing_cell_mask %||% list()
  if (!length(mask) || !length(variables)) return(character())

  original_ids <- .rls_mi_original_row_ids(record)
  positions <- if (is.null(original_rows)) {
    seq_along(original_ids)
  } else {
    match(as.integer(original_rows), original_ids, nomatch = 0L)
  }
  positions <- unique(positions[positions > 0L])
  if (!length(positions)) return(character())

  variables[vapply(variables, function(variable) {
    column_mask <- mask[[variable]]
    if (is.null(column_mask) || !length(column_mask)) return(FALSE)
    valid <- positions[positions <= length(column_mask)]
    length(valid) > 0L && any(as.logical(column_mask[valid]), na.rm = TRUE)
  }, logical(1L))]
}

.rls_mi_dataset_for <- function(record, data) {
  record$data <- data
  record$data_frame <- data
  record
}

.rls_mi_require_mice <- function(context = "multiple-imputation analysis") {
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop(
      sprintf("%s requires the R package `mice`. Install `mice` in the LinkEDA runtime library and try again.", context),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.rls_mi_empty_scalar_pool <- function(m = 0L, reason = NULL) {
  list(
    Qbar = NA_real_, Ubar = NA_real_, B = NA_real_, T = NA_real_,
    SE = NA_real_, df = NA_real_, statistic = NA_real_, p = NA_real_,
    CI_low = NA_real_, CI_high = NA_real_, RIV = NA_real_, FMI = NA_real_,
    m = as.integer(m), valid = FALSE, reason = reason,
    pooling_method = "mice::pool.scalar", mice_result = NULL
  )
}

.rls_mi_pool_scalar <- function(estimates, variances, df_complete = NA_real_, conf_level = 0.95) {
  .rls_mi_require_mice("Scalar multiple-imputation pooling")
  q <- as.numeric(estimates)
  u <- as.numeric(variances)
  if (length(q) != length(u)) {
    stop("Scalar MI estimates and variances must have the same length.", call. = FALSE)
  }
  if (!length(q)) return(.rls_mi_empty_scalar_pool())
  if (length(q) < 2L) {
    stop("Scalar MI pooling requires at least two imputations.", call. = FALSE)
  }
  invalid <- which(!is.finite(q) | !is.finite(u) | u < 0)
  if (length(invalid)) {
    stop(
      sprintf(
        "Scalar MI pooling requires a finite estimate and non-negative variance from every imputation; invalid imputation(s): %s.",
        paste(invalid, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L || !is.finite(conf_level) ||
      conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a single number between 0 and 1.", call. = FALSE)
  }
  n <- if (is.finite(df_complete) && df_complete > 0) df_complete + 1 else Inf
  pooled <- mice::pool.scalar(q, u, n = n, k = 1, rule = "rubin1987")
  se <- sqrt(pooled$t)
  statistic <- if (is.finite(se) && se > 0) pooled$qbar / se else NA_real_
  p <- if (is.finite(statistic) && !is.na(pooled$df) && pooled$df > 0) {
    2 * stats::pt(abs(statistic), df = pooled$df, lower.tail = FALSE)
  } else {
    NA_real_
  }
  alpha <- 1 - conf_level
  critical <- if (!is.na(pooled$df) && pooled$df > 0) {
    stats::qt(1 - alpha / 2, df = pooled$df)
  } else {
    NA_real_
  }
  ci <- if (is.finite(critical) && is.finite(se)) {
    pooled$qbar + c(-1, 1) * critical * se
  } else {
    c(NA_real_, NA_real_)
  }
  list(
    Qbar = unname(pooled$qbar),
    Ubar = unname(pooled$ubar),
    B = unname(pooled$b),
    T = unname(pooled$t),
    SE = unname(se),
    df = unname(pooled$df),
    statistic = unname(statistic),
    p = unname(p),
    CI_low = unname(ci[[1L]]),
    CI_high = unname(ci[[2L]]),
    RIV = unname(pooled$r),
    FMI = unname(pooled$fmi),
    m = as.integer(pooled$m),
    valid = TRUE,
    reason = NULL,
    pooling_method = "mice::pool.scalar",
    mice_result = pooled
  )
}

.rls_mi_fit_kind <- function(fit) {
  if (inherits(fit, "negbin")) return("negbin")
  if (inherits(fit, "glm")) return("glm")
  if (inherits(fit, "lm")) return("lm")
  class(fit)[[1L]] %||% "unknown"
}

.rls_mi_fit_has_ordinary_likelihood <- function(fit) {
  if (is.null(fit)) return(FALSE)
  if (inherits(fit, c("betareg", "gamlss"))) return(TRUE)
  if (inherits(fit, "negbin")) return(TRUE)
  if (inherits(fit, "glm")) {
    family_name <- tolower(as.character(fit$family$family %||% "")[[1L]])
    return(!startsWith(family_name, "quasi"))
  }
  inherits(fit, "lm")
}

.rls_mi_validate_model_fits <- function(fits, context = "MI model pooling") {
  .rls_mi_require_mice(context)
  if (!is.list(fits) || length(fits) < 2L) {
    stop(sprintf("%s requires at least two fitted imputations.", context), call. = FALSE)
  }
  failed <- which(vapply(fits, is.null, logical(1L)))
  if (length(failed)) {
    stop(
      sprintf("%s requires a valid fit from every imputation; fit(s) %s failed.", context, paste(failed, collapse = ", ")),
      call. = FALSE
    )
  }
  kinds <- vapply(fits, .rls_mi_fit_kind, character(1L))
  if (length(unique(kinds)) != 1L || !kinds[[1L]] %in% c("lm", "glm", "negbin")) {
    stop(sprintf("%s requires compatible `lm`, `glm`, or `negbin` fits of one model class.", context), call. = FALSE)
  }
  if (kinds[[1L]] %in% c("glm", "negbin")) {
    unconverged <- which(!vapply(fits, function(fit) isTRUE(fit$converged), logical(1L)))
    if (length(unconverged)) {
      stop(sprintf("%s cannot pool non-converged GLM fit(s): %s.", context, paste(unconverged, collapse = ", ")), call. = FALSE)
    }
    families <- if (identical(kinds[[1L]], "negbin")) {
      vapply(fits, function(fit) paste("negative_binomial", fit$family$link, sep = "/"), character(1L))
    } else {
      vapply(fits, function(fit) paste(fit$family$family, fit$family$link, sep = "/"), character(1L))
    }
    if (length(unique(families)) != 1L) {
      stop(sprintf("%s found inconsistent GLM families or links across imputations.", context), call. = FALSE)
    }
  }
  coefficient_names <- lapply(fits, function(fit) names(stats::coef(fit)))
  if (!all(vapply(coefficient_names[-1L], identical, logical(1L), coefficient_names[[1L]]))) {
    stop(sprintf("%s found different coefficient sets across imputations; categories, reference categories, or aliases are incompatible.", context), call. = FALSE)
  }
  aliased <- which(vapply(fits, function(fit) any(!is.finite(stats::coef(fit))), logical(1L)))
  if (length(aliased)) {
    stop(sprintf("%s cannot pool aliased or non-finite coefficients in imputation(s): %s.", context, paste(aliased, collapse = ", ")), call. = FALSE)
  }
  formula_text <- vapply(fits, function(fit) paste(deparse(stats::formula(fit), width.cutoff = 500L), collapse = " "), character(1L))
  if (length(unique(formula_text)) != 1L) {
    stop(sprintf("%s found different model formulas across imputations.", context), call. = FALSE)
  }
  matrices <- lapply(fits, function(fit) tryCatch(stats::model.matrix(fit), error = function(e) NULL))
  if (any(vapply(matrices, is.null, logical(1L)))) {
    stop(sprintf("%s could not recover every model matrix.", context), call. = FALSE)
  }
  matrix_signatures <- lapply(matrices, function(mm) list(names = colnames(mm), assign = attr(mm, "assign")))
  if (!all(vapply(matrix_signatures[-1L], identical, logical(1L), matrix_signatures[[1L]]))) {
    stop(sprintf("%s found incompatible model-matrix columns across imputations.", context), call. = FALSE)
  }
  xlevels <- lapply(fits, function(fit) fit$xlevels %||% list())
  contrasts <- lapply(fits, function(fit) fit$contrasts %||% list())
  if (!all(vapply(xlevels[-1L], identical, logical(1L), xlevels[[1L]])) ||
      !all(vapply(contrasts[-1L], identical, logical(1L), contrasts[[1L]]))) {
    stop(sprintf("%s found inconsistent categories, reference categories, or contrasts across imputations.", context), call. = FALSE)
  }
  invalid_vcov <- which(vapply(fits, function(fit) {
    vc <- tryCatch(stats::vcov(fit), error = function(e) NULL)
    is.null(vc) || !identical(rownames(vc), coefficient_names[[1L]]) ||
      !identical(colnames(vc), coefficient_names[[1L]]) || any(!is.finite(vc))
  }, logical(1L)))
  if (length(invalid_vcov)) {
    stop(sprintf("%s found singular or non-finite covariance matrices in imputation(s): %s.", context, paste(invalid_vcov, collapse = ", ")), call. = FALSE)
  }
  fits
}

.rls_mi_as_mira <- function(fits, context = "MI model pooling") {
  fits <- .rls_mi_validate_model_fits(fits, context)
  mice::as.mira(fits)
}

.rls_mi_linear_partial_r <- function(fits, terms) {
  fits <- Filter(Negate(is.null), fits)
  unavailable <- stats::setNames(rep(NA_real_, length(terms)), terms)
  # The OLS t-to-r identity uses the residual df of each completed-data fit.
  # Rubin/Barnard-Rubin df describe inference and cannot replace those df.
  if (!length(fits) || !all(vapply(fits, function(fit)
      inherits(fit, "lm") && !inherits(fit, "glm") && is.null(fit$weights), logical(1L))))
    return(unavailable)
  by_imputation <- lapply(fits, function(fit) {
    table <- stats::coef(summary(fit))
    values <- unavailable
    common <- intersect(setdiff(terms, "(Intercept)"), rownames(table))
    values[common] <- .rls_model_partial_r(table[common, "t value"], stats::df.residual(fit))
    values
  })
  stats::setNames(vapply(terms, function(term) {
    values <- vapply(by_imputation, `[[`, numeric(1L), term)
    if (any(!is.finite(values)) || any(abs(values) >= 1)) return(NA_real_)
    tanh(mean(atanh(values)))
  }, numeric(1L)), terms)
}

.rls_mi_pool_coefficients <- function(fits, statistic_name = "t", conf_level = 0.95) {
  mira <- .rls_mi_as_mira(fits, "MI coefficient pooling with mice::pool")
  pooled <- tryCatch(
    mice::pool(mira),
    error = function(e) stop(sprintf("mice::pool could not combine the MI model coefficients: %s", conditionMessage(e)), call. = FALSE)
  )
  pooled_data <- pooled$pooled
  summary_data <- summary(pooled, conf.int = TRUE, conf.level = conf_level)
  pooled_terms <- as.character(pooled_data$term)
  summary_terms <- as.character(summary_data$term)
  order <- match(pooled_terms, summary_terms)
  if (anyNA(order) || anyDuplicated(pooled_terms) || anyDuplicated(summary_terms)) {
    stop("mice::pool returned an incompatible coefficient summary.", call. = FALSE)
  }
  summary_data <- summary_data[order, , drop = FALSE]
  ci_low_name <- if ("conf.low" %in% names(summary_data)) "conf.low" else grep("^2\\.5", names(summary_data), value = TRUE)[[1L]]
  ci_high_name <- if ("conf.high" %in% names(summary_data)) "conf.high" else grep("^97\\.5", names(summary_data), value = TRUE)[[1L]]
  out <- data.frame(
    term = pooled_terms,
    estimate = unname(summary_data$estimate),
    std_error = unname(summary_data$std.error),
    statistic = unname(summary_data$statistic),
    t_value = unname(summary_data$statistic),
    p_value = unname(summary_data$p.value),
    df = unname(summary_data$df),
    ci_lower = unname(summary_data[[ci_low_name]]),
    ci_upper = unname(summary_data[[ci_high_name]]),
    CI_low = unname(summary_data[[ci_low_name]]),
    CI_high = unname(summary_data[[ci_high_name]]),
    within_imputation_variance = unname(pooled_data$ubar),
    between_imputation_variance = unname(pooled_data$b),
    total_variance = unname(pooled_data$t),
    relative_increase_variance = unname(pooled_data$riv),
    fraction_missing_information = unname(pooled_data$fmi),
    partial_r = unname(.rls_mi_linear_partial_r(mira$analyses, pooled_terms)),
    delta_r2 = NA_real_,
    statistic_name = statistic_name,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  attr(out, "mice_pool") <- pooled
  attr(out, "mira") <- mira
  attr(out, "pooling_method") <- "mice::pool (Rubin's rules)"
  out
}

.rls_mi_validate_generalized_mean_fits <- function(fits, context = "MI generalized-model pooling") {
  .rls_mi_require_mice(context)
  if (!is.list(fits) || length(fits) < 2L) {
    stop(sprintf("%s requires at least two fitted imputations.", context), call. = FALSE)
  }
  failed <- which(vapply(fits, is.null, logical(1L)))
  if (length(failed)) {
    stop(sprintf(
      "%s requires a valid fit from every imputation; fit(s) %s failed.",
      context, paste(failed, collapse = ", ")
    ), call. = FALSE)
  }
  kinds <- vapply(fits, function(fit) {
    if (inherits(fit, "betareg")) "betareg"
    else if (inherits(fit, "gamlss")) "gamlss"
    else if (inherits(fit, "glmmTMB")) "glmmTMB"
    else .rls_mi_fit_kind(fit)
  }, character(1L))
  if (length(unique(kinds)) != 1L) {
    stop(sprintf("%s found incompatible model classes across imputations.", context), call. = FALSE)
  }
  unconverged <- which(!vapply(fits, function(fit) {
    if (inherits(fit, "glmmTMB")) {
      isTRUE(fit$sdr$pdHess) && identical(as.integer(fit$fit$convergence %||% 1L), 0L)
    } else {
      isTRUE(fit$converged)
    }
  }, logical(1L)))
  if (length(unconverged)) {
    stop(sprintf(
      "%s cannot pool non-converged fit(s): %s.", context,
      paste(unconverged, collapse = ", ")
    ), call. = FALSE)
  }
  coefficients <- lapply(fits, .rls_glm_mean_coefficients)
  coefficient_names <- lapply(coefficients, names)
  if (!all(vapply(coefficient_names[-1L], identical, logical(1L), coefficient_names[[1L]]))) {
    stop(sprintf(
      "%s found different mean-submodel coefficient sets across imputations; categories, reference categories, or aliases are incompatible.",
      context
    ), call. = FALSE)
  }
  invalid_coefficients <- which(vapply(coefficients, function(value) any(!is.finite(value)), logical(1L)))
  if (length(invalid_coefficients)) {
    stop(sprintf(
      "%s cannot pool aliased or non-finite mean coefficients in imputation(s): %s.",
      context, paste(invalid_coefficients, collapse = ", ")
    ), call. = FALSE)
  }
  formulas <- vapply(fits, function(fit) {
    paste(deparse(stats::formula(fit), width.cutoff = 500L), collapse = " ")
  }, character(1L))
  if (length(unique(formulas)) != 1L) {
    stop(sprintf("%s found different model formulas across imputations.", context), call. = FALSE)
  }
  matrices <- lapply(fits, function(fit) tryCatch(stats::model.matrix(fit), error = function(e) NULL))
  if (any(vapply(matrices, is.null, logical(1L)))) {
    stop(sprintf("%s could not recover every mean-submodel matrix.", context), call. = FALSE)
  }
  signatures <- lapply(matrices, function(matrix) {
    list(names = colnames(matrix), assign = attr(matrix, "assign"))
  })
  if (!all(vapply(signatures[-1L], identical, logical(1L), signatures[[1L]]))) {
    stop(sprintf("%s found incompatible mean-submodel columns across imputations.", context), call. = FALSE)
  }
  covariances <- lapply(fits, function(fit) tryCatch(.rls_glm_mean_vcov(fit), error = function(e) NULL))
  invalid_covariances <- which(vapply(covariances, function(covariance) {
    is.null(covariance) ||
      !identical(rownames(covariance), coefficient_names[[1L]]) ||
      !identical(colnames(covariance), coefficient_names[[1L]]) ||
      any(!is.finite(covariance))
  }, logical(1L)))
  if (length(invalid_covariances)) {
    stop(sprintf(
      "%s found singular or non-finite mean-submodel covariance matrices in imputation(s): %s.",
      context, paste(invalid_covariances, collapse = ", ")
    ), call. = FALSE)
  }
  list(fits = fits, coefficients = coefficients, covariances = covariances)
}

.rls_mi_pool_generalized_mean_coefficients <- function(fits, statistic_name = "t",
                                                        conf_level = 0.95) {
  if (!inherits(fits[[1L]], c("betareg", "gamlss", "glmmTMB"))) {
    return(.rls_mi_pool_coefficients(fits, statistic_name, conf_level))
  }
  validated <- .rls_mi_validate_generalized_mean_fits(
    fits, "MI bounded-response mean-coefficient pooling"
  )
  terms <- names(validated$coefficients[[1L]])
  df_complete <- suppressWarnings(min(vapply(fits, stats::df.residual, numeric(1L)), na.rm = TRUE))
  if (!is.finite(df_complete)) df_complete <- NA_real_
  pools <- lapply(seq_along(terms), function(index) {
    .rls_mi_pool_scalar(
      vapply(validated$coefficients, `[[`, numeric(1L), index),
      vapply(validated$covariances, function(value) value[index, index], numeric(1L)),
      df_complete = df_complete, conf_level = conf_level
    )
  })
  out <- data.frame(
    term = terms,
    estimate = vapply(pools, `[[`, numeric(1L), "Qbar"),
    std_error = vapply(pools, `[[`, numeric(1L), "SE"),
    statistic = vapply(pools, `[[`, numeric(1L), "statistic"),
    t_value = vapply(pools, `[[`, numeric(1L), "statistic"),
    p_value = vapply(pools, `[[`, numeric(1L), "p"),
    df = vapply(pools, `[[`, numeric(1L), "df"),
    ci_lower = vapply(pools, `[[`, numeric(1L), "CI_low"),
    ci_upper = vapply(pools, `[[`, numeric(1L), "CI_high"),
    CI_low = vapply(pools, `[[`, numeric(1L), "CI_low"),
    CI_high = vapply(pools, `[[`, numeric(1L), "CI_high"),
    within_imputation_variance = vapply(pools, `[[`, numeric(1L), "Ubar"),
    between_imputation_variance = vapply(pools, `[[`, numeric(1L), "B"),
    total_variance = vapply(pools, `[[`, numeric(1L), "T"),
    relative_increase_variance = vapply(pools, `[[`, numeric(1L), "RIV"),
    fraction_missing_information = vapply(pools, `[[`, numeric(1L), "FMI"),
    partial_r = NA_real_,
    delta_r2 = NA_real_, statistic_name = statistic_name,
    stringsAsFactors = FALSE, check.names = FALSE
  )
  attr(out, "scalar_pools") <- pools
  attr(out, "pooling_method") <- "mice::pool.scalar (Rubin's rules; mean submodel)"
  out
}

.rls_mi_validate_hurdle_component_fits <- function(fits, component, context) {
  fits <- Filter(Negate(is.null), fits)
  if (!length(fits)) stop(sprintf("%s received no fitted models.", context), call. = FALSE)
  coefficients <- lapply(fits, .rls_hurdle_component_coefficients, component = component)
  coefficient_names <- lapply(coefficients, names)
  if (!all(vapply(coefficient_names[-1L], identical, logical(1L), coefficient_names[[1L]]))) {
    stop(sprintf("%s found incompatible coefficient sets across imputations.", context), call. = FALSE)
  }
  covariances <- lapply(fits, function(fit) {
    tryCatch(.rls_hurdle_component_vcov(fit, component), error = function(e) NULL)
  })
  invalid <- which(vapply(covariances, function(value) {
    is.null(value) || any(!is.finite(value)) ||
      !identical(rownames(value), coefficient_names[[1L]])
  }, logical(1L)))
  if (length(invalid)) {
    stop(sprintf(
      "%s found singular or non-finite covariance matrices in imputation(s): %s.",
      context, paste(invalid, collapse = ", ")
    ), call. = FALSE)
  }
  list(fits = fits, coefficients = coefficients, covariances = covariances)
}

.rls_mi_pool_hurdle_component_coefficients <- function(
    fits, component, statistic_name = "t", conf_level = .95) {
  validated <- .rls_mi_validate_hurdle_component_fits(
    fits, component, paste("MI", component, "component pooling")
  )
  terms <- names(validated$coefficients[[1L]])
  df_complete <- suppressWarnings(min(vapply(
    validated$fits, stats::df.residual, numeric(1L)
  ), na.rm = TRUE))
  if (!is.finite(df_complete)) df_complete <- NA_real_
  pools <- lapply(seq_along(terms), function(index) {
    .rls_mi_pool_scalar(
      vapply(validated$coefficients, `[[`, numeric(1L), index),
      vapply(validated$covariances, function(value) value[index, index], numeric(1L)),
      df_complete = df_complete, conf_level = conf_level
    )
  })
  out <- data.frame(
    term = terms,
    estimate = vapply(pools, `[[`, numeric(1L), "Qbar"),
    std_error = vapply(pools, `[[`, numeric(1L), "SE"),
    statistic = vapply(pools, `[[`, numeric(1L), "statistic"),
    t_value = vapply(pools, `[[`, numeric(1L), "statistic"),
    p_value = vapply(pools, `[[`, numeric(1L), "p"),
    df = vapply(pools, `[[`, numeric(1L), "df"),
    ci_lower = vapply(pools, `[[`, numeric(1L), "CI_low"),
    ci_upper = vapply(pools, `[[`, numeric(1L), "CI_high"),
    partial_r = NA_real_, delta_r2 = NA_real_,
    within_imputation_variance = vapply(pools, `[[`, numeric(1L), "Ubar"),
    between_imputation_variance = vapply(pools, `[[`, numeric(1L), "B"),
    total_variance = vapply(pools, `[[`, numeric(1L), "T"),
    relative_increase_variance = vapply(pools, `[[`, numeric(1L), "RIV"),
    fraction_missing_information = vapply(pools, `[[`, numeric(1L), "FMI"),
    statistic_name = statistic_name,
    stringsAsFactors = FALSE, check.names = FALSE
  )
  attr(out, "scalar_pools") <- pools
  attr(out, "pooling_method") <- paste(
    "mice::pool.scalar (Rubin's rules;", component, "component)"
  )
  out
}

.rls_mi_hurdle_component_term_tests <- function(fits, component, source_terms = NULL) {
  validated <- .rls_mi_validate_hurdle_component_fits(
    fits, component, paste("MI", component, "term tests")
  )
  labels <- attr(stats::terms(validated$fits[[1L]]), "term.labels") %||% character()
  if (!is.null(source_terms)) {
    labels <- labels[vapply(labels, function(label) {
      any(vapply(source_terms, .rls_model_terms_equivalent, logical(1L), b = label))
    }, logical(1L))]
  }
  if (!length(labels)) return(.rls_empty_global_term_tests())
  tests <- lapply(labels, function(term) {
    coefficient_names <- .rls_model_term_coefficient_names(validated$fits[[1L]], term)
    indices <- match(coefficient_names, names(validated$coefficients[[1L]]))
    statistic <- p_value <- df1 <- NA_real_
    if (length(indices) && !anyNA(indices)) {
      estimates <- do.call(rbind, lapply(validated$coefficients, `[`, indices))
      within <- Reduce(`+`, lapply(validated$covariances, function(value) {
        value[indices, indices, drop = FALSE]
      })) / length(validated$fits)
      between <- stats::cov(estimates)
      if (length(indices) == 1L) between <- matrix(between, 1L, 1L)
      total <- within + (1 + 1 / length(validated$fits)) * between
      pooled <- colMeans(estimates)
      solution <- tryCatch(qr.solve(total, pooled), error = function(e) NULL)
      if (!is.null(solution)) {
        statistic <- unname(sum(pooled * solution))
        df1 <- length(indices)
        p_value <- stats::pchisq(statistic, df = df1, lower.tail = FALSE)
      }
    }
    data.frame(
      term = term, coefficient_names = paste(coefficient_names, collapse = "\u001f"),
      statistic_name = "Rubin Wald chi-square", statistic = statistic,
      df1 = df1, df2 = NA_real_, p_value = p_value,
      method = paste("Rubin pooled joint Wald chi-square for the", component, "component"),
      stringsAsFactors = FALSE
    )
  })
  .rls_as_global_term_tests(do.call(rbind, tests))
}

.rls_mi_generalized_term_omnibus_tests <- function(
    fits, source_terms = NULL, test = c("wald_chisq", "wald_f")) {
  test <- match.arg(test)
  if (identical(test, "wald_f")) {
    return(.rls_as_global_term_tests(
      .rls_mi_term_omnibus_tests(fits, source_terms)
    ))
  }
  validated <- .rls_mi_validate_generalized_mean_fits(
    fits, "MI generalized-model term-specific Wald tests"
  )
  labels <- attr(stats::terms(fits[[1L]]), "term.labels")
  if (!is.null(source_terms)) {
    labels <- labels[vapply(labels, function(label) {
      any(vapply(source_terms, function(term) .rls_model_terms_equivalent(label, term), logical(1L)))
    }, logical(1L))]
  }
  empty <- data.frame(
    term = character(), coefficient_names = character(), statistic_name = character(),
    statistic = numeric(), df1 = numeric(), df2 = numeric(), p_value = numeric(),
    method = character(), stringsAsFactors = FALSE
  )
  if (!length(labels)) return(empty)
  rows <- lapply(labels, function(term) {
    coefficient_names <- .rls_model_term_coefficient_names(fits[[1L]], term)
    indices <- match(coefficient_names, names(validated$coefficients[[1L]]))
    estimable <- length(indices) > 0L && !anyNA(indices)
    statistic <- p_value <- df1 <- NA_real_
    if (estimable) {
      estimates <- do.call(rbind, lapply(validated$coefficients, `[`, indices))
      within <- Reduce(`+`, lapply(validated$covariances, function(value) {
        value[indices, indices, drop = FALSE]
      })) / length(fits)
      between <- stats::cov(estimates)
      if (length(indices) == 1L) between <- matrix(between, 1L, 1L)
      total <- within + (1 + 1 / length(fits)) * between
      pooled <- colMeans(estimates)
      solution <- if (all(is.finite(total)) && all(is.finite(pooled))) {
        tryCatch(qr.solve(total, pooled), error = function(e) NULL)
      } else NULL
      if (!is.null(solution)) {
        statistic <- unname(sum(pooled * solution))
        df1 <- length(indices)
        p_value <- stats::pchisq(statistic, df = df1, lower.tail = FALSE)
      }
    }
    data.frame(
      term = term,
      coefficient_names = paste(coefficient_names, collapse = "\u001f"),
      statistic_name = "Rubin Wald chi-square", statistic = statistic,
      df1 = df1, df2 = NA_real_, p_value = p_value,
      method = "Rubin pooled joint Wald chi-square from the fitted mean coefficients",
      stringsAsFactors = FALSE
    )
  })
  .rls_as_global_term_tests(do.call(rbind, rows))
}

.rls_mi_pool_r_squared <- function(fits, adjusted = FALSE) {
  mira <- .rls_mi_as_mira(fits, "MI R-squared pooling with mice::pool.r.squared")
  if (!identical(.rls_mi_fit_kind(fits[[1L]]), "lm")) {
    stop("mice::pool.r.squared is available only for `lm` fits.", call. = FALSE)
  }
  pooled <- tryCatch(
    mice::pool.r.squared(mira, adjusted = adjusted),
    error = function(e) stop(sprintf("mice::pool.r.squared failed: %s", conditionMessage(e)), call. = FALSE)
  )
  list(
    estimate = unname(pooled[1L, "est"]),
    conf_low = unname(pooled[1L, "lo 95"]),
    conf_high = unname(pooled[1L, "hi 95"]),
    fraction_missing_information = unname(pooled[1L, "fmi"]),
    adjusted = isTRUE(adjusted),
    pooling_method = "mice::pool.r.squared",
    mice_result = pooled
  )
}

.rls_mi_term_coefficient_names <- function(fit, source_terms) {
  .rls_model_term_coefficient_names(fit, source_terms)
}

coef.rls_mi_term_restriction <- function(object, ...) {
  object$restricted_coefficients
}

vcov.rls_mi_term_restriction <- function(object, ...) {
  object$restricted_covariance
}

.rls_mi_term_restriction <- function(fit, remove_term) {
  target <- .rls_model_term_coefficient_names(fit, remove_term)
  coefficients <- stats::coef(fit)
  covariance <- stats::vcov(fit)
  if (!length(target) || !all(target %in% names(coefficients))) {
    stop(sprintf("Could not identify coefficient columns for model term `%s`.", remove_term), call. = FALSE)
  }
  retained <- setdiff(names(coefficients), target)
  restricted <- fit
  restricted$restricted_coefficients <- coefficients[retained]
  restricted$restricted_covariance <- covariance[retained, retained, drop = FALSE]
  class(restricted) <- unique(c("rls_mi_term_restriction", class(fit)))
  restricted
}

.rls_mi_term_omnibus_tests <- function(fits, source_terms = NULL) {
  fits <- .rls_mi_validate_model_fits(fits, "MI term-specific omnibus tests")
  labels <- attr(stats::terms(fits[[1L]]), "term.labels")
  empty <- data.frame(
    term = character(), coefficient_names = character(), statistic_name = character(),
    statistic = numeric(), df1 = numeric(), df2 = numeric(), p_value = numeric(),
    method = character(), stringsAsFactors = FALSE
  )
  if (!is.null(source_terms)) {
    labels <- labels[vapply(labels, function(label) {
      any(vapply(source_terms, function(term) {
        .rls_model_terms_equivalent(label, term)
      }, logical(1L)))
    }, logical(1L))]
  }
  if (!length(labels)) return(empty)

  pooled_results <- vector("list", length(labels))
  rows <- lapply(seq_along(labels), function(index) {
    term <- labels[[index]]
    coefficient_names <- .rls_model_term_coefficient_names(fits[[1L]], term)
    pool <- tryCatch({
      restricted <- lapply(fits, .rls_mi_term_restriction, remove_term = term)
      .rls_mi_pool_d1(
        fits, restricted, term_names = term,
        allow_d3_fallback = FALSE
      )
    }, error = function(e) e)
    pooled_results[[index]] <<- pool
    ok <- !inherits(pool, "error") && isTRUE(pool$ok)
    data.frame(
      term = term,
      coefficient_names = paste(coefficient_names, collapse = "\u001f"),
      statistic_name = "D1",
      statistic = if (ok) pool$F else NA_real_,
      df1 = if (ok) pool$df1 else NA_real_,
      df2 = if (ok) pool$df2 else NA_real_,
      p_value = if (ok) pool$p else NA_real_,
      method = if (ok) pool$method else "mice::D1 unavailable",
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  attr(out, "pooled_results") <- pooled_results
  .rls_as_global_term_tests(out)
}

.rls_mi_reduced_fits <- function(fits, remove_terms = NULL, intercept_only = FALSE) {
  fits <- .rls_mi_validate_model_fits(fits, "MI reduced-model construction")
  lapply(seq_along(fits), function(i) {
    fit <- fits[[i]]
    formula <- stats::formula(fit)
    labels <- attr(stats::terms(fit), "term.labels")
    if (isTRUE(intercept_only)) {
      kept <- character()
    } else {
      remove_terms <- as.character(remove_terms %||% character())
      remove <- vapply(labels, function(label) {
        any(vapply(remove_terms, function(term) .rls_model_terms_equivalent(label, term), logical(1L)))
      }, logical(1L))
      if (length(remove_terms) && !any(remove)) {
        stop(
          sprintf("Could not map term(s) `%s` to the fitted model when constructing a D1 reduced model.", paste(remove_terms, collapse = ", ")),
          call. = FALSE
        )
      }
      kept <- labels[!remove]
    }
    response <- paste(deparse(formula[[2L]], width.cutoff = 500L), collapse = " ")
    model_data <- stats::model.frame(fit)
    fitted_offset <- stats::model.offset(model_data)
    if (!is.null(fitted_offset)) {
      # A terms object omits offsets from term.labels. Reuse the exact total
      # offset that R fitted, including multiple formula or call offsets.
      offset_name <- ".linkeda_d1_offset"
      while (offset_name %in% names(model_data))
        offset_name <- paste0(offset_name, "_")
      model_data[[offset_name]] <- as.numeric(fitted_offset)
      kept <- c(kept, sprintf("offset(%s)", offset_name))
    }
    reduced_formula <- stats::reformulate(
      if (length(kept)) kept else "1",
      response = response,
      intercept = as.logical(attr(stats::terms(fit), "intercept"))
    )
    environment(reduced_formula) <- environment(formula)
    replay <- if (is.null(fitted_offset)) fit else .rls_mi_d3_replay_fit(fit)
    if (!is.null(fitted_offset)) replay$call$offset <- NULL
    reduced <- tryCatch(
      stats::update(replay, formula = reduced_formula, data = model_data, na.action = stats::na.fail),
      error = function(first_error) {
        if (!is.null(fitted_offset)) stop(sprintf(
          "Could not fit D1 reduced model with its original offset for imputation %d: %s",
          i, conditionMessage(first_error)), call. = FALSE)
        tryCatch(
          stats::update(fit, formula = reduced_formula, na.action = stats::na.fail),
          error = function(second_error) {
            stop(
              sprintf("Could not fit D1 reduced model for imputation %d: %s", i, conditionMessage(first_error)),
              call. = FALSE
            )
          }
        )
      }
    )
    if (!is.null(fitted_offset) &&
        !isTRUE(all.equal(unname(stats::model.offset(stats::model.frame(reduced))),
                          unname(fitted_offset), check.attributes = FALSE)))
      stop(sprintf("D1 reduced model changed the fitted offset in imputation %d.", i),
           call. = FALSE)
    reduced
  })
}

.rls_mi_validate_d1_models <- function(full_fits, reduced_fits) {
  full_fits <- .rls_mi_validate_model_fits(full_fits, "mice::D1 full models")
  reduced_fits <- .rls_mi_validate_model_fits(reduced_fits, "mice::D1 reduced models")
  if (length(full_fits) != length(reduced_fits)) {
    stop("mice::D1 requires one full and one reduced fit for every imputation.", call. = FALSE)
  }
  for (i in seq_along(full_fits)) {
    full_frame <- stats::model.frame(full_fits[[i]])
    reduced_frame <- stats::model.frame(reduced_fits[[i]])
    full_response <- stats::model.response(full_frame)
    reduced_response <- stats::model.response(reduced_frame)
    if (nrow(full_frame) != nrow(reduced_frame) ||
        !identical(rownames(full_frame), rownames(reduced_frame)) ||
        !isTRUE(all.equal(unname(full_response), unname(reduced_response), check.attributes = FALSE))) {
      stop(
        sprintf("mice::D1 full and reduced models use different observations in imputation %d.", i),
        call. = FALSE
      )
    }
    full_names <- names(stats::coef(full_fits[[i]]))
    reduced_names <- names(stats::coef(reduced_fits[[i]]))
    if (!identical(stats::model.weights(full_frame), stats::model.weights(reduced_frame)) ||
        !identical(stats::model.offset(full_frame), stats::model.offset(reduced_frame))) {
      stop(sprintf("mice::D1 requires the same weights and offset in imputation %d.", i), call. = FALSE)
    }
    if (inherits(full_fits[[i]], "lm") && inherits(reduced_fits[[i]], "lm")) {
      full_family <- stats::family(full_fits[[i]])
      reduced_family <- stats::family(reduced_fits[[i]])
      # MASS::glm.nb embeds its fitted theta in family$family. A changed
      # nuisance estimate does not change the negative-binomial distribution.
      full_negbin <- inherits(full_fits[[i]], "negbin")
      reduced_negbin <- inherits(reduced_fits[[i]], "negbin")
      same_distribution <- (full_negbin && reduced_negbin) ||
        (!full_negbin && !reduced_negbin && identical(full_family$family, reduced_family$family))
      if (!same_distribution || !identical(full_family$link, reduced_family$link))
        stop(sprintf("mice::D1 requires the same distribution and link in imputation %d.", i), call. = FALSE)
      full_matrix <- stats::model.matrix(full_fits[[i]])
      reduced_matrix <- stats::model.matrix(reduced_fits[[i]])
      common <- intersect(full_names, reduced_names)
      if (!all(common %in% colnames(full_matrix)) || !all(common %in% colnames(reduced_matrix)) ||
          !isTRUE(all.equal(unname(full_matrix[, common, drop = FALSE]),
                            unname(reduced_matrix[, common, drop = FALSE]), tolerance = 1e-10)))
        stop(sprintf("mice::D1 models have different coding or predictor values in imputation %d; use a common model parameterization.", i), call. = FALSE)
    }
    # mice::D1 requires a consistent order for the shared coefficients.
    if (!identical(full_names[full_names %in% reduced_names], reduced_names))
      stop(sprintf("mice::D1 requires the same shared coefficient order in imputation %d.", i), call. = FALSE)
    if (!all(reduced_names %in% full_names) || length(reduced_names) >= length(full_names)) {
      stop(
        sprintf("mice::D1 models are not a proper nested full/reduced pair in imputation %d.", i),
        call. = FALSE
      )
    }
  }
  list(full = full_fits, reduced = reduced_fits)
}

coef.rls_mi_mean_submodel <- function(object, ...) {
  object$mean_submodel_coefficients
}

vcov.rls_mi_mean_submodel <- function(object, ...) {
  object$mean_submodel_covariance
}

.rls_mi_mean_submodel <- function(fit) {
  coefficients <- .rls_glm_mean_coefficients(fit)
  covariance <- .rls_glm_mean_vcov(fit)
  if (!length(coefficients) ||
      !identical(rownames(covariance), names(coefficients)) ||
      !identical(colnames(covariance), names(coefficients)) ||
      any(!is.finite(coefficients)) || any(!is.finite(covariance))) {
    stop("The bounded-response mean submodel has incompatible coefficients or covariance.",
         call. = FALSE)
  }
  fit$mean_submodel_coefficients <- coefficients
  fit$mean_submodel_covariance <- covariance
  class(fit) <- unique(c("rls_mi_mean_submodel", class(fit)))
  fit
}

.rls_mi_validate_bounded_d1_models <- function(full_fits, reduced_fits) {
  .rls_mi_require_mice("MI bounded-response D1 comparison")
  if (!requireNamespace("mitml", quietly = TRUE)) {
    stop("MI bounded-response D1 comparison requires the R package `mitml`.",
         call. = FALSE)
  }
  if (!is.list(full_fits) || !is.list(reduced_fits) ||
      length(full_fits) < 2L || length(full_fits) != length(reduced_fits)) {
    stop("MI bounded-response D1 comparison requires matching fits from at least two imputations.",
         call. = FALSE)
  }
  all_fits <- c(full_fits, reduced_fits)
  kinds <- vapply(all_fits, function(fit) {
    if (inherits(fit, "betareg")) "betareg"
    else if (inherits(fit, "gamlss")) "gamlss"
    else "unsupported"
  }, character(1L))
  if (length(unique(kinds)) != 1L || identical(kinds[[1L]], "unsupported")) {
    stop("MI bounded-response D1 comparison requires one compatible R model class in every imputation.",
         call. = FALSE)
  }
  for (i in seq_along(full_fits)) {
    full_names <- names(.rls_glm_mean_coefficients(full_fits[[i]]))
    reduced_names <- names(.rls_glm_mean_coefficients(reduced_fits[[i]]))
    if (!all(reduced_names %in% full_names) || length(reduced_names) >= length(full_names)) {
      stop(sprintf(
        "MI bounded-response D1 models are not a proper nested mean-model pair in imputation %d.", i
      ), call. = FALSE)
    }
  }
  list(
    full = lapply(full_fits, .rls_mi_mean_submodel),
    reduced = lapply(reduced_fits, .rls_mi_mean_submodel)
  )
}

.rls_mi_d3_replay_fit <- function(fit) {
  # mice::D3 refits each model with a fixed offset. A call that still names a
  # temporary family, data, formula, weight or subset object can fail after
  # the original fit's evaluation frame has gone away. Freeze the exact fitted
  # frame and specification in a copy; never modify the accepted fit.
  replay <- fit
  frame <- stats::model.frame(fit)
  replay$call$formula <- stats::formula(fit)
  replay$call$data <- frame
  replay$call$subset <- NULL
  replay$call$na.action <- NULL
  if (inherits(fit, "glm") && !inherits(fit, "negbin")) {
    replay$call$family <- stats::family(fit)
  }
  if ("weights" %in% names(replay$call)) {
    replay$call$weights <- stats::model.weights(frame)
  }
  if ("offset" %in% names(replay$call)) {
    replay$call$offset <- stats::model.offset(frame)
  }
  if (inherits(fit, "negbin") && "init.theta" %in% names(replay$call)) {
    replay$call$init.theta <- fit$theta
  }
  replay
}

.rls_mi_pool_d1 <- function(full_fits, reduced_fits, term_names = NULL,
                            allow_d3_fallback = TRUE) {
  bounded_mean_submodel <- all(vapply(
    c(full_fits, reduced_fits), inherits, logical(1L), c("betareg", "gamlss")
  ))
  if (bounded_mean_submodel) {
    fits <- .rls_mi_validate_bounded_d1_models(full_fits, reduced_fits)
    full_mira <- fits$full
    reduced_mira <- fits$reduced
    tested <- tryCatch(
      mitml::testModels(
        full_mira, reduced_mira, method = "D1"
      ),
      error = function(e) stop(sprintf(
        "mitml::testModels(method = 'D1') could not compare the MI bounded-response mean models: %s",
        conditionMessage(e)
      ), call. = FALSE)
    )
    d1 <- list(
      result = tested$test, m = tested$m, dfcom = tested$df.com,
      method = "D1", mitml_result = tested
    )
  } else {
    fits <- .rls_mi_validate_d1_models(full_fits, reduced_fits)
    full_mira <- mice::as.mira(fits$full)
    reduced_mira <- mice::as.mira(fits$reduced)
    d1 <- tryCatch(
      mice::D1(full_mira, reduced_mira),
      error = function(e) stop(sprintf("mice::D1 could not compare the MI models: %s", conditionMessage(e)), call. = FALSE)
    )
  }
  result <- if (is.matrix(d1$result) || is.data.frame(d1$result)) {
    d1$result[1L, , drop = TRUE]
  } else {
    d1$result
  }
  required <- c("F.value", "df1", "df2", "P(>F)", "RIV")
  if (length(result) == length(required) && is.null(names(result))) names(result) <- required
  if (!all(required %in% names(result))) {
    stop("mice::D1 returned an unexpected structured result.", call. = FALSE)
  }
  f_value <- unname(result[["F.value"]])
  df1 <- unname(result[["df1"]])
  df2 <- unname(result[["df2"]])
  p_value <- unname(result[["P(>F)"]])
  riv <- unname(result[["RIV"]])
  zero_riv_limit <- FALSE
  # mice::D1/mitml may return NaN for df2 and p at the exact zero-RIV
  # boundary even though it supplies a finite D1 F statistic and complete-data
  # df.  This is only a boundary adaptation of the structured mice result; no
  # coefficient or covariance pooling is reimplemented here.
  if ((!is.finite(df2) || !is.finite(p_value)) && is.finite(f_value) &&
      is.finite(df1) && is.finite(riv) && abs(riv) <= sqrt(.Machine$double.eps) &&
      is.finite(d1$dfcom) && d1$dfcom > 0) {
    df2 <- unname(d1$dfcom)
    p_value <- stats::pf(f_value, df1 = df1, df2 = df2, lower.tail = FALSE)
    zero_riv_limit <- TRUE
  }
  if ((!is.finite(df2) || !is.finite(p_value)) && !zero_riv_limit) {
    d3_allowed <- !bounded_mean_submodel && isTRUE(allow_d3_fallback) &&
      all(vapply(c(fits$full, fits$reduced),
                 .rls_mi_fit_has_ordinary_likelihood, logical(1L)))
    d3 <- if (d3_allowed) {
      tryCatch(mice::D3(
        mice::as.mira(lapply(fits$full, .rls_mi_d3_replay_fit)),
        mice::as.mira(lapply(fits$reduced, .rls_mi_d3_replay_fit))
      ), error = function(e) e)
    } else {
      structure(list(message = if (bounded_mean_submodel) {
        paste(
          "D3 was not attempted for a bounded-response mean-submodel comparison;",
          "the valid pooled test is mitml::testModels(method = 'D1')."
        )
      } else if (!isTRUE(allow_d3_fallback)) {
        "mice::D3 was not attempted because this is an exact coefficient-restriction D1/Wald test."
      } else {
        paste(
          "mice::D3 was not attempted because at least one model has no ordinary likelihood;",
          "D3 is unavailable for quasi-likelihood models."
        )
      }), class = c("rls_mi_d3_unavailable", "condition"))
    }
    if (d3_allowed && !inherits(d3, "error")) {
      d3_result <- if (is.matrix(d3$result) || is.data.frame(d3$result)) {
        d3$result[1L, , drop = TRUE]
      } else {
        d3$result
      }
      if (length(d3_result) == length(required) && is.null(names(d3_result))) names(d3_result) <- required
      if (all(required %in% names(d3_result)) &&
          is.finite(d3_result[["F.value"]]) && is.finite(d3_result[["P(>F)"]])) {
        return(list(
          ok = TRUE,
          method = "mice::D3 (fallback because mice::D1 was undefined)",
          df1 = unname(d3_result[["df1"]]),
          df2 = unname(d3_result[["df2"]]),
          F = unname(d3_result[["F.value"]]),
          p = unname(d3_result[["P(>F)"]]),
          m = as.integer(d3$m),
          term_names = as.character(term_names %||% character()),
          RIV = unname(d3_result[["RIV"]]),
          FMI = NA_real_, Ubar = NA_real_, B = NA_real_, T = NA_real_,
          dfcom = unname(d3$dfcom),
          zero_riv_limit = FALSE,
          fallback_reason = "mice::D1 returned undefined denominator degrees of freedom or p-value.",
          fallback_from = "mice::D1",
          d3_available = TRUE,
          d3_attempted = TRUE,
          mice_result = d3,
          mice_d1_result = d1,
          full_mira = full_mira,
          reduced_mira = reduced_mira
        ))
      }
    }
    d3_failure_reason <- if (!d3_allowed) {
      conditionMessage(d3)
    } else if (inherits(d3, "error")) {
      sprintf("mice::D3 failed: %s", conditionMessage(d3))
    } else {
      "mice::D3 did not return a finite structured result."
    }
  } else {
    d3_allowed <- !bounded_mean_submodel && isTRUE(allow_d3_fallback) &&
      all(vapply(c(fits$full, fits$reduced),
                 .rls_mi_fit_has_ordinary_likelihood, logical(1L)))
    d3 <- NULL
    d3_failure_reason <- NULL
  }
  list(
    ok = is.finite(f_value) && is.finite(p_value),
    method = if (bounded_mean_submodel) {
      "mitml::testModels D1 (bounded-response mean submodel)"
    } else "mice::D1",
    df1 = df1,
    df2 = df2,
    F = f_value,
    p = p_value,
    m = as.integer(d1$m),
    term_names = as.character(term_names %||% character()),
    RIV = riv,
    FMI = NA_real_,
    Ubar = NA_real_,
    B = NA_real_,
    T = NA_real_,
    dfcom = unname(d1$dfcom),
    zero_riv_limit = zero_riv_limit,
    fallback_reason = d3_failure_reason,
    fallback_from = NULL,
    d3_available = d3_allowed,
    d3_attempted = !is.null(d3) && d3_allowed,
    mice_result = d1,
    full_mira = full_mira,
    reduced_mira = reduced_mira
  )
}

.rls_mi_common_model_rows <- function(dataset, model_specs, selected_rows = integer()) {
  completed <- .rls_mi_completed_datasets(dataset)
  original_ids <- .rls_mi_original_row_ids(dataset)
  if (!length(completed) || !length(model_specs)) return(integer())
  selected_rows <- unique(as.integer(selected_rows %||% integer()))
  selected_positions <- match(selected_rows, original_ids)
  selected_positions <- selected_positions[is.finite(selected_positions)]
  row_sets <- unlist(lapply(model_specs, function(spec) {
    formula <- .rls_generalized_glm_formula_object(list(
      response = spec$response,
      terms = spec$terms %||% character(),
      count_regression = isTRUE(spec$count_regression),
      exposure = spec$exposure %||% "",
      offset_variable = spec$offset %||% ""
    ))
    lapply(completed, function(data) {
      data <- .rls_model_data_for_term_types(
        data, spec$term_types %||% list(), response = spec$response
      )
      data <- .rls_glm_apply_factor_references(
        data, spec$factor_reference_levels %||% list()
      )
      complete <- .rls_model_complete_data(
        data, formula, spec$scope %||% "all", selected_positions
      )
      original_ids[complete$rows]
    })
  }), recursive = FALSE)
  if (!length(row_sets)) return(integer())
  sort(Reduce(intersect, row_sets))
}

.rls_mi_fit_data_by_imputation <- function(record, formula, scope = "all") {
  dataset <- .rls_dataset_record(record$group)
  completed <- .rls_mi_completed_datasets(dataset)
  original_ids <- .rls_mi_original_row_ids(dataset)
  selected_positions <- match(record$selected_rows %||% integer(), original_ids)
  selected_positions <- selected_positions[is.finite(selected_positions)]
  lapply(seq_along(completed), function(i) {
    data <- completed[[i]]
    if (length(record$term_types %||% list())) {
      response <- record$dependent %||% record$response %||% NULL
      data <- .rls_model_data_for_term_types(data, record$term_types, response = response)
    }
    data <- .rls_glm_apply_factor_references(
      data, record$factor_reference_levels %||% list()
    )
    complete <- .rls_model_complete_data(data, formula, scope, selected_positions)
    complete$source_rows <- complete$rows
    complete$original_rows <- original_ids[complete$rows]
    comparison_rows <- record$comparison_rows
    if (!is.null(comparison_rows)) {
      comparison_rows <- as.integer(comparison_rows)
      keep <- complete$original_rows %in% comparison_rows
      complete$data <- complete$data[keep, , drop = FALSE]
      complete$rows <- complete$rows[keep]
      complete$source_rows <- complete$source_rows[keep]
      complete$original_rows <- complete$original_rows[keep]
      complete$excluded <- nrow(data) - nrow(complete$data)
    }
    complete <- .rls_glm_center_complete_data(complete, record$centered_predictors)
    complete$rows <- complete$original_rows
    complete$imputation <- i
    complete
  })
}

.rls_mi_result_metadata <- function(dataset, analysis_type, specification,
                                    fits_by_imputation = list(),
                                    estimates_by_imputation = NULL,
                                    pooled_result = NULL,
                                    pooling_method = "mice::pool.scalar (Rubin's rules)",
                                    warnings = character(),
                                    complete_data_result_by_imputation = NULL,
                                    diagnostic_summary_by_imputation = NULL) {
  variances <- if (is.data.frame(pooled_result)) {
    list(
      within_imputation_variance = pooled_result$within_imputation_variance %||% NA_real_,
      between_imputation_variance = pooled_result$between_imputation_variance %||% NA_real_,
      total_variance = pooled_result$total_variance %||% NA_real_,
      fraction_missing_information = pooled_result$fraction_missing_information %||% NA_real_,
      relative_increase_variance = pooled_result$relative_increase_variance %||% NA_real_,
      degrees_of_freedom = pooled_result$df %||% NA_real_
    )
  } else if (is.list(pooled_result)) {
    list(
      within_imputation_variance = pooled_result$Ubar %||% NA_real_,
      between_imputation_variance = pooled_result$B %||% NA_real_,
      total_variance = pooled_result$T %||% NA_real_,
      fraction_missing_information = pooled_result$FMI %||% NA_real_,
      relative_increase_variance = pooled_result$RIV %||% NA_real_,
      degrees_of_freedom = pooled_result$df %||% NA_real_
    )
  } else {
    list(
      within_imputation_variance = NA_real_,
      between_imputation_variance = NA_real_,
      total_variance = NA_real_,
      fraction_missing_information = NA_real_,
      relative_increase_variance = NA_real_,
      degrees_of_freedom = NA_real_
    )
  }
  c(list(
    dataset_id = dataset$dataset_id %||% dataset$group,
    imputation_id = dataset$imputation_id %||% NA_character_,
    m = dataset$imputation_count %||% length(dataset$completed_datasets %||% list()),
    analysis_type = analysis_type,
    analysis_specification = specification,
    fits_by_imputation = fits_by_imputation,
    estimates_by_imputation = estimates_by_imputation,
    pooled_result = pooled_result,
    pooling_method = pooling_method,
    warnings = warnings,
    result_version = 1L,
    created_at = Sys.time(),
    complete_data_result_by_imputation = complete_data_result_by_imputation,
    diagnostic_summary_by_imputation = diagnostic_summary_by_imputation
  ), variances)
}

.rls_mi_format_mean <- function(pool, sds) {
  if (!isTRUE(pool$valid)) return("\u2014")
  sd_text <- if (length(sds) && any(is.finite(sds))) .rls_export_format_number(mean(sds, na.rm = TRUE), 1L) else "\u2014"
  paste0(
    .rls_export_format_number(pool$Qbar, 1L),
    " (SD ", sd_text,
    "; SE ", .rls_export_format_number(pool$SE, 1L), ")"
  )
}

.rls_mi_format_count_percent <- function(counts, percentages) {
  if (!length(counts)) return("\u2014")
  count_text <- .rls_export_format_number(mean(counts, na.rm = TRUE), 1L)
  pct_text <- .rls_export_format_percent(mean(percentages, na.rm = TRUE))
  paste0(count_text, " (", pct_text, ")")
}

.rls_mi_group_levels <- function(dataset, completed, group, variable_types = NULL) {
  if (is.null(group)) return(character())
  levels <- unique(unlist(lapply(completed, function(data) {
    gf <- .rls_table1_group_factor(.rls_mi_dataset_for(dataset, data), group, variable_types)
    levels(droplevels(gf))
  }), use.names = FALSE))
  levels[!is.na(levels) & nzchar(levels)]
}

.rls_mi_group_slices <- function(dataset, data, group, levels, variable_types = NULL) {
  if (is.null(group)) {
    return(list(Overall = seq_len(nrow(data))))
  }
  gf <- .rls_table1_group_factor(.rls_mi_dataset_for(dataset, data), group, variable_types)
  gf <- factor(as.character(gf), levels = levels)
  stats::setNames(c(list(seq_len(nrow(data))), lapply(levels, function(level) {
    which(!is.na(gf) & gf == level)
  })), .rls_table1_group_column_names(levels))
}

.rls_mi_table1_numeric_group_test <- function(dataset, variable, group, levels, variable_types = NULL) {
  if (is.null(group)) return(NULL)
  completed <- .rls_mi_completed_datasets(dataset)
  original_ids <- .rls_mi_original_row_ids(dataset)
  estimates <- variances <- numeric()
  rows_used <- integer()
  rows_excluded <- integer()
  fits <- list()
  for (data in completed) {
    gf <- .rls_table1_group_factor(.rls_mi_dataset_for(dataset, data), group, variable_types)
    df <- data.frame(
      y = suppressWarnings(as.double(.rls_table1_raw(data[[variable]]))),
      group = factor(as.character(gf), levels = levels)
    )
    ok <- stats::complete.cases(df)
    rows_used <- union(rows_used, original_ids[which(ok)])
    rows_excluded <- union(rows_excluded, original_ids[which(!ok)])
    if (sum(ok) < 3L || length(unique(df$group[ok])) < 2L) {
      estimates <- c(estimates, NA_real_)
      variances <- c(variances, NA_real_)
      fits[length(fits) + 1L] <- list(NULL)
      next
    }
    fit <- stats::lm(y ~ group, data = df[ok, , drop = FALSE])
    fits[[length(fits) + 1L]] <- fit
    if (length(levels) == 2L) {
      coef_names <- setdiff(names(stats::coef(fit)), "(Intercept)")
      coef_name <- if (length(coef_names)) coef_names[[1L]] else NA_character_
      if (is.na(coef_name)) {
        estimates <- c(estimates, NA_real_)
        variances <- c(variances, NA_real_)
      } else {
        estimates <- c(estimates, unname(stats::coef(fit)[[coef_name]]))
        variances <- c(variances, unname(stats::vcov(fit)[coef_name, coef_name]))
      }
    }
  }
  if (length(levels) > 2L) {
    valid_fit_indices <- which(!vapply(fits, is.null, logical(1L)))
    if (!length(valid_fit_indices)) {
      return(list(
        p = NA_real_,
        test = "MI pooled omnibus (mice::D1)",
        statistic = NA_real_,
        parameter = NA_real_,
        rows_used_original_ids = rows_used,
        rows_excluded_original_ids = rows_excluded,
        status = "insufficient_data",
        detail = "No fitted imputations were available for the pooled omnibus group comparison.",
        pooled_result = NULL,
        fits_by_imputation = fits
      ))
    }
    coef_names <- setdiff(names(stats::coef(fits[[valid_fit_indices[[1L]]]])), "(Intercept)")
    if (!length(coef_names)) {
      return(list(
        p = NA_real_,
        test = "MI pooled omnibus (mice::D1)",
        statistic = NA_real_,
        parameter = NA_real_,
        rows_used_original_ids = rows_used,
        rows_excluded_original_ids = rows_excluded,
        status = "insufficient_data",
        detail = "No non-intercept coefficients were available for the pooled omnibus group comparison.",
        pooled_result = NULL,
        fits_by_imputation = fits
      ))
    }
    reduced_fits <- .rls_mi_reduced_fits(fits, intercept_only = TRUE)
    pool <- .rls_mi_pool_d1(fits, reduced_fits, term_names = group)
    return(list(
      p = pool$p,
      test = sprintf("MI pooled omnibus (%s)", pool$method),
      statistic = pool$F,
      parameter = pool$df1,
      rows_used_original_ids = rows_used,
      rows_excluded_original_ids = rows_excluded,
      status = if (isTRUE(pool$ok)) "ok" else "insufficient_data",
      detail = sprintf(
        "Pooled omnibus group comparison using lm(y ~ group) in each imputation; F = %s; df1 = %s; df2 = %s.",
        .rls_export_format_number(pool$F, 3L),
        .rls_export_format_number(pool$df1, 0L),
        .rls_export_format_number(pool$df2, 1L)
      ),
      pooled_result = pool,
      fits_by_imputation = fits
    ))
  }
  valid_fits <- fits[!vapply(fits, is.null, logical(1L))]
  df_complete <- if (length(valid_fits)) min(vapply(valid_fits, stats::df.residual, numeric(1L))) else NA_real_
  pool <- .rls_mi_pool_scalar(estimates, variances, df_complete = df_complete)
  list(
    p = pool$p,
    test = "MI pooled t",
    statistic = pool$statistic,
    parameter = pool$df,
    rows_used_original_ids = rows_used,
    rows_excluded_original_ids = rows_excluded,
    status = if (isTRUE(pool$valid)) "ok" else "insufficient_data",
    detail = sprintf(
      "Pooled group comparison (%s - %s) using lm(y ~ group) in each imputation; b = %s; SE = %s; df = %s.",
      levels[[2L]], levels[[1L]],
      .rls_export_format_number(pool$Qbar, 3L),
      .rls_export_format_number(pool$SE, 3L),
      .rls_export_format_number(pool$df, 1L)
    ),
    pooled_result = pool,
    estimates_by_imputation = estimates,
    variances_by_imputation = variances
  )
}

.rls_mi_table1_unsupported_test <- function(dataset, type) {
  list(
    p = NA_real_,
    test = switch(type,
      categorical = "MI categorical test unavailable",
      ordinal = "MI ordinal test unavailable",
      "MI test unavailable"
    ),
    statistic = NA_real_,
    parameter = NA_real_,
    rows_used_original_ids = .rls_mi_original_row_ids(dataset),
    rows_excluded_original_ids = integer(),
    status = "unsupported",
    detail = "Descriptive proportions are combined across imputations; inferential pooling for this variable type is not yet implemented."
  )
}

.rls_table1_build_mi <- function(dataset, variables, group, variable_types, include_missing,
                                 show_p, show_test, show_n, ordinal_as,
                                 numeric_stats, categorical_stats, ordinal_stats,
                                 name = NULL) {
  completed <- .rls_mi_completed_datasets(dataset)
  group_levels <- .rls_mi_group_levels(dataset, completed, group, variable_types)
  column_names <- .rls_table1_group_column_names(group_levels)
  original_ids <- .rls_mi_original_row_ids(dataset)
  original <- dataset$original_data %||% dataset$data
  display <- data.frame(Variable = character(), stringsAsFactors = FALSE)
  for (column in column_names) display[[column]] <- character()
  if (!is.null(group) && isTRUE(show_p)) display[["p"]] <- character()
  if (!is.null(group) && isTRUE(show_test)) display[["Test"]] <- character()
  rows <- list()
  missingness <- list()
  rows_used_by_variable <- list()
  rows_used_by_test <- list()
  test_results <- list()
  estimates_by_imputation <- list()

  add_row <- function(label, values, p = "", test = "", row_type = "summary",
                      variable = "", level = "", detail = "", statistics = NULL,
                      raw_statistics = NULL) {
    row <- as.list(c(Variable = label, values))
    if (!is.null(group) && isTRUE(show_p)) row[["p"]] <- p
    if (!is.null(group) && isTRUE(show_test)) row[["Test"]] <- test
    new_row <- as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
    display <<- rbind(display, new_row[names(display)])
    rows[[length(rows) + 1L]] <<- list(
      row_index = nrow(display), variable = variable, level = level,
      row_type = row_type, values = values, p = p, test = test, detail = detail,
      statistics = statistics, raw_statistics = raw_statistics
    )
  }

  if (isTRUE(show_n)) {
    counts <- lapply(completed, function(data) {
      slices <- .rls_mi_group_slices(dataset, data, group, group_levels, variable_types)
      vapply(slices[column_names], length, integer(1L))
    })
    count_matrix <- do.call(rbind, counts)
    n_values <- vapply(seq_along(column_names), function(i) {
      values <- count_matrix[, i]
      if (length(unique(values)) == 1L) as.character(values[[1L]]) else .rls_export_format_number(mean(values), 1L)
    }, character(1L))
    names(n_values) <- column_names
    add_row("N", n_values, row_type = "n", detail = "Number of rows; varying group counts are averaged across imputations.")
  }

  for (variable in variables) {
    type <- .rls_table1_infer_type(dataset, variable, variable_types, ordinal_as)
    if (identical(type, "unsupported")) {
      stop(sprintf("Variable `%s` is unsupported for Table 1.", variable), call. = FALSE)
    }
    original_raw <- .rls_table1_raw(original[[variable]])
    rows_used_by_variable[[variable]] <- original_ids
    missingness[[variable]] <- list(
      total_n = length(original_raw),
      non_missing_n = sum(!is.na(original_raw)),
      missing_n = sum(is.na(original_raw)),
      missing_percent = mean(is.na(original_raw)),
      by_group = stats::setNames(rep(NA_integer_, length(column_names)), column_names)
    )

    test <- if (identical(type, "numeric")) {
      .rls_mi_table1_numeric_group_test(dataset, variable, group, group_levels, variable_types)
    } else if (!is.null(group)) {
      .rls_mi_table1_unsupported_test(dataset, type)
    } else {
      NULL
    }
    if (!is.null(test)) rows_used_by_test[[variable]] <- test$rows_used_original_ids
    test_results[[variable]] <- test
    p_text <- if (!is.null(test)) .rls_export_format_p(test$p) else ""
    test_text <- if (!is.null(test)) test$test else ""

    if (identical(type, "numeric")) {
      value_pools <- list()
      values <- vapply(column_names, function(column) {
        means <- vars <- sds <- sample_sizes <- numeric()
        medians <- q1s <- q3s <- numeric()
        for (data in completed) {
          slices <- .rls_mi_group_slices(dataset, data, group, group_levels, variable_types)
          idx <- slices[[column]]
          x <- .rls_table1_numeric(data[[variable]][idx])
          sample_sizes <- c(sample_sizes, length(x))
          means <- c(means, if (length(x)) mean(x) else NA_real_)
          vars <- c(vars, if (length(x) > 1L) stats::var(x) / length(x) else NA_real_)
          sds <- c(sds, if (length(x) > 1L) stats::sd(x) else NA_real_)
          if (length(x)) {
            q <- stats::quantile(x, probs = c(.25, .5, .75), na.rm = TRUE, names = FALSE, type = 7)
            q1s <- c(q1s, q[[1L]])
            medians <- c(medians, q[[2L]])
            q3s <- c(q3s, q[[3L]])
          } else {
            q1s <- c(q1s, NA_real_)
            medians <- c(medians, NA_real_)
            q3s <- c(q3s, NA_real_)
          }
        }
        valid_df <- sample_sizes[is.finite(sample_sizes) & sample_sizes > 1L] - 1
        pool <- .rls_mi_pool_scalar(
          means, vars,
          df_complete = if (length(valid_df)) min(valid_df) else NA_real_
        )
        value_pools[[column]] <<- list(pool = pool, means = means, variances = vars, sds = sds,
                                       sample_sizes = sample_sizes, medians = medians, q1 = q1s, q3 = q3s)
        .rls_mi_format_mean(pool, sds)
      }, character(1L))
      statistics <- lapply(column_names, function(column) {
        entry <- value_pools[[column]]
        pool <- entry$pool
        list(
          mean = .rls_export_format_number(pool$Qbar, 1L),
          sd = if (length(entry$sds) && any(is.finite(entry$sds)))
            .rls_export_format_number(mean(entry$sds, na.rm = TRUE), 1L) else "\u2014",
          se = .rls_export_format_number(pool$SE, 1L),
          ci95 = paste0("[", .rls_export_format_number(pool$CI_low, 1L), ", ",
                        .rls_export_format_number(pool$CI_high, 1L), "]"),
          median = .rls_export_format_number(mean(entry$medians, na.rm = TRUE), 1L),
          q1 = .rls_export_format_number(mean(entry$q1, na.rm = TRUE), 1L),
          q3 = .rls_export_format_number(mean(entry$q3, na.rm = TRUE), 1L)
        )
      })
      names(statistics) <- column_names
      raw_statistics <- lapply(column_names, function(column) {
        entry <- value_pools[[column]]; pool <- entry$pool
        list(
          mean = pool$Qbar,
          sd = if (any(is.finite(entry$sds))) mean(entry$sds, na.rm = TRUE) else NA_real_,
          se = pool$SE, ci_lower = pool$CI_low, ci_upper = pool$CI_high,
          median = mean(entry$medians, na.rm = TRUE),
          q1 = mean(entry$q1, na.rm = TRUE), q3 = mean(entry$q3, na.rm = TRUE)
        )
      })
      names(raw_statistics) <- column_names
      add_row(variable, values, p_text, test_text, "numeric_mean_sd", variable,
              detail = paste(variable, "pooled mean using mice::pool.scalar; SD is averaged descriptively across imputations."),
              statistics = statistics, raw_statistics = raw_statistics)
      estimates_by_imputation[[variable]] <- value_pools
      if ("median_iqr" %in% numeric_stats) {
        values <- vapply(column_names, function(column) {
          entry <- value_pools[[column]]
          paste0(
            .rls_export_format_number(mean(entry$medians, na.rm = TRUE), 1L), " [",
            .rls_export_format_number(mean(entry$q1, na.rm = TRUE), 1L), ", ",
            .rls_export_format_number(mean(entry$q3, na.rm = TRUE), 1L), "]"
          )
        }, character(1L))
        add_row("  Median [Q1, Q3]", values, row_type = "numeric_median_iqr", variable = variable,
                detail = "Median and quartiles are summarized across imputations; Rubin's rules are not applied.",
                statistics = statistics, raw_statistics = raw_statistics)
      }
    } else {
      add_row(variable, stats::setNames(rep("", length(column_names)), column_names),
              p_text, test_text, if (identical(type, "ordinal")) "ordinal_parent" else "categorical_parent", variable,
              detail = "Descriptive proportions are averaged across imputations.")
      levels <- unique(unlist(lapply(completed, function(data) {
        .rls_table1_levels(data[[variable]], type)
      }), use.names = FALSE))
      levels <- levels[!is.na(levels) & nzchar(levels)]
      for (level in levels) {
        per_level <- list()
        values <- vapply(column_names, function(column) {
          counts <- pcts <- numeric()
          for (data in completed) {
            slices <- .rls_mi_group_slices(dataset, data, group, group_levels, variable_types)
            xf <- .rls_table1_display_factor(data[[variable]], type)
            idx <- slices[[column]]
            non_missing <- !is.na(xf[idx])
            denom <- sum(non_missing)
            count <- sum(non_missing & as.character(xf[idx]) == level)
            counts <- c(counts, count)
            pcts <- c(pcts, if (denom > 0) count / denom else NA_real_)
          }
          per_level[[column]] <<- list(counts = counts, percentages = pcts)
          .rls_mi_format_count_percent(counts, pcts)
        }, character(1L))
        estimates_by_imputation[[paste(variable, level, sep = "=")]] <- per_level
        raw_statistics <- lapply(column_names, function(column) {
          entry <- per_level[[column]]
          list(
            n = mean(entry$counts, na.rm = TRUE),
            percent = mean(entry$percentages, na.rm = TRUE)
          )
        })
        names(raw_statistics) <- column_names
        add_row(paste0("  ", level), values, row_type = paste0(type, "_level"),
                variable = variable, level = level,
                detail = paste(variable, "=", level, "mean n (%) across imputations"),
                raw_statistics = raw_statistics)
      }
      if (identical(type, "ordinal") && "median_iqr" %in% ordinal_stats) {
        values <- vapply(column_names, function(column) {
          medians <- q1s <- q3s <- numeric()
          for (data in completed) {
            slices <- .rls_mi_group_slices(dataset, data, group, group_levels, variable_types)
            scores <- .rls_table1_ordinal_scores(data[[variable]])
            x <- scores[slices[[column]]]
            x <- x[!is.na(x)]
            if (length(x)) {
              q <- stats::quantile(x, probs = c(.25, .5, .75), na.rm = TRUE, names = FALSE, type = 7)
              q1s <- c(q1s, q[[1L]])
              medians <- c(medians, q[[2L]])
              q3s <- c(q3s, q[[3L]])
            }
          }
          paste0(
            .rls_export_format_number(mean(medians, na.rm = TRUE), 1L), " [",
            .rls_export_format_number(mean(q1s, na.rm = TRUE), 1L), ", ",
            .rls_export_format_number(mean(q3s, na.rm = TRUE), 1L), "]"
          )
        }, character(1L))
        add_row("  Median [Q1, Q3]", values, row_type = "ordinal_median_iqr", variable = variable,
                detail = "Ordinal median and quartiles are summarized across imputations.")
      }
    }
    if (isTRUE(include_missing)) {
      values <- stats::setNames(rep("", length(column_names)), column_names)
      values[["Overall"]] <- sprintf(
        "%d (%s)",
        sum(is.na(original_raw)),
        .rls_export_format_percent(mean(is.na(original_raw)))
      )
      add_row("  Missing in original", values, row_type = "missing", variable = variable,
              detail = paste(variable, "missing values in the source data before imputation"),
              raw_statistics = stats::setNames(c(
                list(list(n = sum(is.na(original_raw)), percent = mean(is.na(original_raw)))),
                rep(list(list()), length(column_names) - 1L)
              ), column_names))
    }
  }

  note <- c(
    sprintf("Multiple imputation analysis: m = %d imputations.", length(completed)),
    "Pooling method: mice::pool.scalar for numeric means and two-group numeric comparisons; mice::D1 for numeric comparisons with more than two groups, with mice::D3 used only if D1 is undefined.",
    "Descriptive SDs, medians, quartiles, counts, and percentages are summarized across imputations and are labelled as descriptive.",
    "Categorical and ordinal inferential tests under MI are not shown unless a validated pooling method is implemented."
  )
  title <- if (is.null(group)) {
    "Table 1. Descriptive statistics - Multiple Imputation"
  } else {
    paste("Table 1. Descriptive statistics by", group, "- Multiple Imputation")
  }
  metadata <- .rls_mi_result_metadata(
    dataset,
    "table1",
    list(variables = variables, group = group, ordinal_as = ordinal_as),
    estimates_by_imputation = estimates_by_imputation,
    pooled_result = test_results,
    pooling_method = "mice::pool.scalar for numeric estimates; mice::D1 (mice::D3 fallback if undefined) for omnibus tests; descriptive averaging for proportions/quantiles",
    warnings = note[-1L],
    complete_data_result_by_imputation = lapply(seq_along(completed), function(i) {
      list(imputation = i, n_rows = nrow(completed[[i]]))
    })
  )
  structure(list(
    id = .rls_table1_id(dataset$group, name),
    table1_id = NULL,
    title = title,
    dataset_id = dataset$group,
    dataset_name = dataset$name %||% dataset$group,
    group = dataset$group,
    data = dataset$data,
    variables = variables,
    group_variable = group,
    group_levels = group_levels,
    group_column_names = column_names,
    group_value_labels = if(is.null(group)) NULL else .rls_table1_value_labels(completed[[1L]][[group]]),
    variable_types = stats::setNames(vapply(variables, function(variable) .rls_table1_infer_type(dataset, variable, variable_types, ordinal_as), character(1L)), variables),
    summary_statistics = rows,
    test_results = test_results,
    missingness = missingness,
    rows_used_by_variable = rows_used_by_variable,
    rows_used_by_test = rows_used_by_test,
    display_options = list(include_missing = include_missing, show_p = show_p, show_test = show_test,
                           show_n = show_n, ordinal_as = ordinal_as, numeric_stats = numeric_stats,
                           categorical_stats = categorical_stats, ordinal_stats = ordinal_stats),
    display_table = display,
    footnotes = note,
    imputation_note = note[[1L]],
    analysis_backend = "multiple_imputation",
    multiple_imputation = metadata,
    result_version = 1L,
    created_at = Sys.time()
  ), class = "rlispstat_table1_record")
}

.rls_mi_pool_correlation <- function(r, n) {
  r <- as.numeric(r)
  n <- as.numeric(n)
  if (length(r) != length(n)) return(list(
    pool = .rls_mi_empty_scalar_pool(length(r), "Each imputation requires one correlation and one sample size."),
    z = rep(NA_real_, length(r)), variances = rep(NA_real_, length(r))
  ))
  valid <- is.finite(r) & abs(r) < 1 & is.finite(n) & n > 3
  if (!any(valid)) {
    return(list(
      pool = .rls_mi_empty_scalar_pool(length(r), "Fisher-z pooling requires n > 3 and a finite correlation strictly between -1 and 1 in every imputation."),
      z = rep(NA_real_, length(r)), variances = rep(NA_real_, length(r))
    ))
  }
  z <- rep(NA_real_, length(r))
  z[valid] <- atanh(r[valid])
  variances <- rep(NA_real_, length(r))
  variances[valid] <- 1 / (n[valid] - 3)
  if (!all(valid)) {
    return(list(
      pool = .rls_mi_empty_scalar_pool(
        length(r),
        sprintf("Fisher-z pooling requires finite |r| < 1 and n > 3 in every imputation; invalid imputation(s): %s.", paste(which(!valid), collapse = ", "))
      ),
      z = z,
      variances = variances
    ))
  }
  if (length(r) < 2L) return(list(pool=.rls_mi_empty_scalar_pool(length(r), "Pooling requires at least two imputations."), z=z, variances=variances))
  list(pool = .rls_mi_pool_scalar(z, variances), z = z, variances = variances)
}

.rls_mi_correlation_compute <- function(record) {
  dataset <- .rls_dataset_record(record$group)
  if(!is.null(record$data_scope)) dataset <- .rls_dataset_subset_original_rows(dataset,record$data_scope$rows)
  completed <- .rls_mi_completed_datasets(dataset)
  original_ids <- .rls_mi_original_row_ids(dataset)
  variables <- record$variables
  cells <- list()
  for (y in variables) {
    for (x in variables) {
      if (identical(x, y)) {
        rows_by_imp <- lapply(completed, function(data) {
          if (identical(record$missing_mode, "listwise")) {
            which(stats::complete.cases(data[, variables, drop = FALSE]))
          } else {
            which(!is.na(data[[x]]))
          }
        })
        n_by_imp <- vapply(rows_by_imp, length, integer(1L))
        cells[[length(cells) + 1L]] <- list(
          x_variable = x, y_variable = y, r = NA_real_, p = NA_real_,
          n = as.integer(round(mean(n_by_imp))), status = "diagonal", pooling_reason = "",
          rows_used = original_ids[sort(unique(unlist(rows_by_imp)))],
          r_by_imputation = rep(NA_real_, length(completed)),
          z_by_imputation = rep(NA_real_, length(completed)),
          pooled_z = NA_real_, pooled_SE = NA_real_, pooled_df = NA_real_,
          pooled_CI_low = NA_real_, pooled_CI_high = NA_real_,
          within_imputation_variance = NA_real_,
          between_imputation_variance = NA_real_,
          total_variance = NA_real_,
          fraction_missing_information = NA_real_,
          relative_increase_variance = NA_real_
        )
        next
      }
      r_by_imp <- n_by_imp <- numeric()
      rows_by_imp <- list()
      statuses <- character()
      for (data in completed) {
        rows <- if (identical(record$missing_mode, "listwise")) {
          which(stats::complete.cases(data[, variables, drop = FALSE]))
        } else {
          which(stats::complete.cases(data[, c(x, y), drop = FALSE]))
        }
        rows_by_imp[[length(rows_by_imp) + 1L]] <- rows
        n_by_imp <- c(n_by_imp, length(rows))
        if (length(rows) < 4L) {
          r_by_imp <- c(r_by_imp, NA_real_)
          statuses <- c(statuses, "insufficient_n")
          next
        }
        xv <- as.double(data[[x]][rows])
        yv <- as.double(data[[y]][rows])
        if (any(!is.finite(xv)) || any(!is.finite(yv))) {
          r_by_imp <- c(r_by_imp, NA_real_); statuses <- c(statuses, "nonfinite_values"); next
        }
        if (stats::sd(xv) == 0 || stats::sd(yv) == 0) {
          r_by_imp <- c(r_by_imp, NA_real_)
          statuses <- c(statuses, "zero_variance")
          next
        }
        r_by_imp <- c(r_by_imp, unname(stats::cor.test(xv, yv, method = "pearson")$estimate))
        statuses <- c(statuses, "valid")
      }
      pooled <- .rls_mi_pool_correlation(r_by_imp, n_by_imp)
      pool <- pooled$pool
      r <- if (isTRUE(pool$valid)) tanh(pool$Qbar) else NA_real_
      ci <- if (isTRUE(pool$valid)) tanh(c(pool$CI_low, pool$CI_high)) else c(NA_real_, NA_real_)
      cells[[length(cells) + 1L]] <- list(
        x_variable = x, y_variable = y, r = r, p = pool$p,
        n = as.integer(round(mean(n_by_imp, na.rm = TRUE))),
        status = if (isTRUE(pool$valid)) "valid" else "pooling_unavailable",
        pooling_reason = pool$reason %||% "",
        rows_used = original_ids[sort(unique(unlist(rows_by_imp)))],
        r_by_imputation = r_by_imp,
        z_by_imputation = pooled$z,
        pooled_z = pool$Qbar,
        pooled_SE = pool$SE,
        pooled_df = pool$df,
        pooled_CI_low = ci[[1L]],
        pooled_CI_high = ci[[2L]],
        within_imputation_variance = pool$Ubar,
        between_imputation_variance = pool$B,
        total_variance = pool$T,
        fraction_missing_information = pool$FMI,
        relative_increase_variance = pool$RIV
      )
    }
  }
  data.frame(
    x_variable = vapply(cells, `[[`, character(1L), "x_variable"),
    y_variable = vapply(cells, `[[`, character(1L), "y_variable"),
    r = vapply(cells, `[[`, numeric(1L), "r"),
    p = vapply(cells, `[[`, numeric(1L), "p"),
    n = vapply(cells, `[[`, integer(1L), "n"),
    missing_mode = record$missing_mode,
    status = vapply(cells, `[[`, character(1L), "status"),
    pooling_reason = vapply(cells, `[[`, character(1L), "pooling_reason"),
    rows_used_original_ids = I(lapply(cells, `[[`, "rows_used")),
    r_by_imputation = I(lapply(cells, `[[`, "r_by_imputation")),
    z_by_imputation = I(lapply(cells, `[[`, "z_by_imputation")),
    pooled_z = vapply(cells, `[[`, numeric(1L), "pooled_z"),
    pooled_SE = vapply(cells, `[[`, numeric(1L), "pooled_SE"),
    pooled_df = vapply(cells, `[[`, numeric(1L), "pooled_df"),
    pooled_CI_low = vapply(cells, `[[`, numeric(1L), "pooled_CI_low"),
    pooled_CI_high = vapply(cells, `[[`, numeric(1L), "pooled_CI_high"),
    within_imputation_variance = vapply(cells, `[[`, numeric(1L), "within_imputation_variance"),
    between_imputation_variance = vapply(cells, `[[`, numeric(1L), "between_imputation_variance"),
    total_variance = vapply(cells, `[[`, numeric(1L), "total_variance"),
    fraction_missing_information = vapply(cells, `[[`, numeric(1L), "fraction_missing_information"),
    relative_increase_variance = vapply(cells, `[[`, numeric(1L), "relative_increase_variance"),
    pooling_scale = "Fisher z",
    stringsAsFactors = FALSE
  )
}

.rls_mi_fit_linear_model_record <- function(record) {
  dataset <- .rls_dataset_record(record$group)
  formula <- .rls_model_formula_object(record$dependent, record$predictors)
  complete_by_imp <- .rls_mi_fit_data_by_imputation(record, formula, record$scope %||% "all")
  fits <- lapply(complete_by_imp, function(complete) {
    if (nrow(complete$data) <= length(record$predictors) + 1L) return(NULL)
    stats::lm(formula, data = complete$data, na.action = stats::na.fail)
  })
  if (!any(!vapply(fits, is.null, logical(1L)))) {
    stop("Not enough complete cases in the imputations to fit the model.", call. = FALSE)
  }
  reference_fit <- fits[[which(!vapply(fits, is.null, logical(1L)))[[1L]]]]
  coefs <- .rls_mi_pool_coefficients(fits, statistic_name = "t")
  term_labels <- attr(stats::terms(reference_fit), "term.labels")
  deltas_by_imputation <- lapply(fits, function(fit) {
    if (is.null(fit)) return(setNames(rep(NA_real_, length(term_labels)), term_labels))
    .rls_model_term_delta_r2(fit)
  })
  term_delta_r2 <- stats::setNames(vapply(term_labels, function(term) {
    values <- vapply(deltas_by_imputation, function(delta) {
      if (term %in% names(delta)) unname(delta[[term]]) else NA_real_
    }, numeric(1L))
    if (any(is.finite(values))) mean(values[is.finite(values)]) else NA_real_
  }, numeric(1L)), term_labels)
  coefficient_rows <- .rls_model_coefficient_display_rows(
    reference_fit,
    transform(coefs, statistic = t_value),
    dataset$data,
    record$predictors,
    statistic_name = "t",
    term_delta_r2 = term_delta_r2
  )
  term_tests <- .rls_mi_term_omnibus_tests(fits, term_labels)
  parent_test_terms <- .rls_model_parent_test_terms(reference_fit, dataset$data)
  parent_term_tests <- term_tests[vapply(term_tests$term, function(term) {
    any(vapply(parent_test_terms, .rls_model_terms_equivalent, logical(1L),
               b = term))
  }, logical(1L)), , drop = FALSE]
  coefficient_rows <- .rls_model_apply_parent_term_tests(
    coefficient_rows, parent_term_tests
  )
  summaries <- lapply(fits, function(fit) if (is.null(fit)) NULL else summary(fit))
  valid_summaries <- summaries[!vapply(summaries, is.null, logical(1L))]
  valid_fits <- fits[!vapply(fits, is.null, logical(1L))]
  residual_se <- vapply(valid_summaries, `[[`, numeric(1L), "sigma")
  r_squared <- vapply(valid_summaries, `[[`, numeric(1L), "r.squared")
  adj_r_squared <- vapply(valid_summaries, `[[`, numeric(1L), "adj.r.squared")
  pooled_r_squared <- .rls_mi_pool_r_squared(fits, adjusted = FALSE)
  pooled_adj_r_squared <- .rls_mi_pool_r_squared(fits, adjusted = TRUE)
  omnibus <- if (length(record$predictors)) {
    reduced_fits <- .rls_mi_reduced_fits(fits, intercept_only = TRUE)
    .rls_mi_pool_d1(fits, reduced_fits, term_names = record$predictors)
  } else {
    list(ok = FALSE, df1 = NA_real_, F = NA_real_, p = NA_real_)
  }
  global_test_method <- omnibus$method %||% "not applicable"
  rows_used <- Reduce(intersect, lapply(complete_by_imp, `[[`, "original_rows"))
  rows_excluded <- setdiff(.rls_mi_original_row_ids(dataset), rows_used)
  diagnostics_by_imp <- lapply(seq_along(fits), function(i) {
    fit <- fits[[i]]
    if (is.null(fit)) return(data.frame())
    .rls_compute_glm_diagnostics(record, fit, complete_by_imp[[i]])
  })
  diagnostics <- diagnostics_by_imp[[which(lengths(diagnostics_by_imp) > 0L)[[1L]]]]
  diagnostics$imputation_display <- "first fitted imputation"
  list(
    fit = reference_fit,
    fitted_lm = reference_fit,
    fits_by_imputation = fits,
    coefficients = coefs[, c("term", "estimate", "std_error", "t_value", "p_value", "ci_lower", "ci_upper",
                             "partial_r", "delta_r2", "df",
                             "within_imputation_variance", "between_imputation_variance", "total_variance",
                             "relative_increase_variance", "fraction_missing_information"), drop = FALSE],
    coefficient_rows = coefficient_rows,
    parent_term_tests = parent_term_tests,
    term_tests = term_tests,
    summary = list(
      n_used = as.integer(round(mean(vapply(complete_by_imp, function(x) nrow(x$data), numeric(1L))))),
      n_excluded = length(rows_excluded),
      r_squared = pooled_r_squared$estimate,
      adj_r_squared = pooled_adj_r_squared$estimate,
      r_squared_conf_int = c(pooled_r_squared$conf_low, pooled_r_squared$conf_high),
      adj_r_squared_conf_int = c(pooled_adj_r_squared$conf_low, pooled_adj_r_squared$conf_high),
      r_squared_fmi = pooled_r_squared$fraction_missing_information,
      adj_r_squared_fmi = pooled_adj_r_squared$fraction_missing_information,
      r_squared_descriptive_mean = mean(r_squared, na.rm = TRUE),
      adj_r_squared_descriptive_mean = mean(adj_r_squared, na.rm = TRUE),
      global_f = omnibus$F %||% NA_real_,
      df_model = if (is.finite(omnibus$df1 %||% NA_real_)) as.integer(omnibus$df1) else length(record$predictors),
      df_residual = min(vapply(valid_fits, stats::df.residual, numeric(1L)), na.rm = TRUE),
      global_p = omnibus$p %||% NA_real_,
      global_test_method = global_test_method,
      rmse = mean(residual_se, na.rm = TRUE),
      residual_se = mean(residual_se, na.rm = TRUE),
      aic = NA_real_,
      bic = NA_real_,
      fit_information_method = sprintf("R2 and adjusted R2 use mice::pool.r.squared; residual SE and delta R2 are descriptive means across imputations; Partial r combines the completed-data partial correlations on the Fisher z scale, using each fit residual df; the global model test uses %s; AIC/BIC are not pooled.", global_test_method)
    ),
    rows_used = rows_used,
    rows_excluded = rows_excluded,
    predictor_centers = complete_by_imp[[which(!vapply(fits, is.null, logical(1L)))[[1L]]]]$predictor_centers %||% numeric(),
    diagnostics = diagnostics,
    diagnostics_by_imputation = diagnostics_by_imp,
    multiple_imputation = .rls_mi_result_metadata(
      dataset,
      "linear_model",
      list(response = record$dependent, terms = record$predictors, formula = deparse(formula)),
      fits_by_imputation = fits,
      estimates_by_imputation = lapply(fits, function(fit) if (is.null(fit)) NULL else stats::coef(fit)),
      pooled_result = coefs,
      pooling_method = sprintf("mice::pool (coefficients); mice::pool.r.squared; %s", global_test_method),
      warnings = c("Standardized coefficients are not pooled for multiple-imputation models.",
                   "R2 and adjusted R2 use mice::pool.r.squared; their arithmetic means remain only as explicitly descriptive metadata.",
                   "Residual SE and delta R2 remain descriptive means; coefficient significance tests use mice::pool.",
                   "Partial r is pooled on the Fisher z scale from completed-data correlations; it is not calculated from pooled t or MI-adjusted df.",
                   sprintf("Global model tests use %s; AIC/BIC are not pooled.", global_test_method)),
      complete_data_result_by_imputation = complete_by_imp,
      diagnostic_summary_by_imputation = diagnostics_by_imp
    ),
    status = sprintf(
      "Multiple imputation linear model fitted across %d imputations using mice pooling.",
      length(fits)
    )
  )
}

.rls_mi_extract_ceiling_hurdle <- function(
    record, dataset, fits, fit_contexts, complete_by_imp, fit_warnings = list()) {
  valid_indices <- which(!vapply(fits, is.null, logical(1L)))
  valid_fits <- fits[valid_indices]
  reference_fit <- valid_fits[[1L]]
  no_perfect_by_imputation <- vapply(valid_fits, function(fit) {
    identical(fit$linkeda_perfect_score_boundary %||% "", "no_perfect_scores")
  }, logical(1L))
  if (any(no_perfect_by_imputation) && !all(no_perfect_by_imputation)) {
    stop(paste(
      "The perfect-score component is estimable in only some imputations.",
      "LinkEDA will not pool a mixture of estimable and boundary fits."
    ), call. = FALSE)
  }
  no_perfect_scores <- all(no_perfect_by_imputation)
  components <- c("perfect_score", "below_ceiling")
  labels <- c(
    perfect_score = "Perfect score \u2014 logistic component",
    below_ceiling = "Score below ceiling \u2014 truncated beta-binomial component"
  )
  component_validation <- lapply(components, function(component) {
    lapply(seq_along(fits), function(index) {
      .rls_validate_hurdle_component(fits[[index]], component, index)
    })
  })
  names(component_validation) <- components
  component_successes <- vapply(component_validation, function(results) {
    sum(vapply(results, `[[`, logical(1L), "ok"))
  }, integer(1L))
  if (any(component_successes != length(fits))) {
    joint_nonconvergence <- which(vapply(fits, function(fit) {
      !is.null(fit) && inherits(fit, "gamlss") && !isTRUE(fit$converged)
    }, logical(1L)))
    joint_description <- if (length(joint_nonconvergence)) {
      sprintf(
        "The joint ZABB fit did not converge in imputation(s) %s after the extended R optimisation.",
        paste(joint_nonconvergence, collapse = ", ")
      )
    } else character()
    component_descriptions <- vapply(components, function(component) {
      failures <- component_validation[[component]][!vapply(
        component_validation[[component]], `[[`, logical(1L), "ok"
      )]
      failures <- failures[!vapply(failures, function(failure) {
        failure$imputation %in% joint_nonconvergence
      }, logical(1L))]
      label <- if (component == "perfect_score") "Perfect-score component" else
        "Truncated beta-binomial component"
      if (!length(failures)) return("")
      details <- vapply(failures, function(failure) {
        reason <- sub("^[^:]+ failed: ", "", failure$status)
        sprintf("%d (%s)", failure$imputation, reason)
      }, character(1L))
      sprintf("%s failed in imputation(s) %s.", label, paste(details, collapse = ", "))
    }, character(1L))
    descriptions <- c(joint_description, component_descriptions)
    descriptions <- unique(descriptions[nzchar(descriptions)])
    stop(paste(descriptions, collapse = " "), call. = FALSE)
  }
  coefficient_tables <- lapply(components, function(component) {
    table <- if (identical(component, "perfect_score") && no_perfect_scores) {
      .rls_empty_hurdle_coefficient_table(multiple_imputation = TRUE)
    } else .rls_mi_pool_hurdle_component_coefficients(fits, component)
    table$component <- rep(component, nrow(table))
    .rls_glm_augment_exponentiated(table, record)
  })
  names(coefficient_tables) <- components
  term_tests <- lapply(components, function(component) {
    tests <- if (identical(component, "perfect_score") && no_perfect_scores) {
      .rls_empty_global_term_tests()
    } else .rls_mi_hurdle_component_term_tests(fits, component, record$terms)
    tests$component <- rep(component, nrow(tests))
    tests
  })
  names(term_tests) <- components
  row_blocks <- lapply(components, function(component) {
    if (!nrow(coefficient_tables[[component]])) {
      section_label <- paste0(
        labels[[component]], " (not estimable: no perfect scores)"
      )
      section <- .rls_glm_augment_exponentiated(
        .rls_hurdle_section_row(section_label, component), record
      )
      return(section)
    }
    rows <- .rls_model_coefficient_display_rows(
      reference_fit, coefficient_tables[[component]], dataset$data, record$terms,
      statistic_name = "t"
    )
    rows <- .rls_model_apply_parent_term_tests(rows, term_tests[[component]])
    rows <- .rls_glm_augment_exponentiated(rows, record)
    rows$component <- component
    section <- .rls_glm_augment_exponentiated(
      .rls_hurdle_section_row(labels[[component]], component), record
    )
    rbind(section, rows)
  })
  coefficient_rows <- do.call(rbind, row_blocks)
  coefficients <- do.call(rbind, coefficient_tables)
  rownames(coefficient_rows) <- rownames(coefficients) <- NULL
  distribution_by_imp <- vector("list", length(valid_indices))
  diagnostic_failures <- character()
  diagnostics_by_imp <- lapply(seq_along(valid_indices), function(position) {
    index <- valid_indices[[position]]
    diagnostics <- tryCatch(
      .rls_generalized_glm_diagnostics(
        record, fits[[index]], fit_contexts[[index]]$complete,
        imputation_index = index
      ),
      error = function(error) {
        diagnostic_failures <<- c(diagnostic_failures, sprintf(
          paste(
            "Imputation %d diagnostic could not be computed.",
            "The fitted model remains valid. %s"
          ), index, conditionMessage(error)
        ))
        data.frame()
      }
    )
    if (!nrow(diagnostics)) return(diagnostics)
    distribution_input <- diagnostics
    distribution_input$.mu_failure <- as.numeric(
      fits[[index]]$linkeda_component_mu_fv %||% fits[[index]]$mu.fv
    )
    distribution_input$.sigma <- as.numeric(
      fits[[index]]$linkeda_component_sigma_fv %||% fits[[index]]$sigma.fv
    )
    distribution_result <- tryCatch(
      .rls_hurdle_distribution_summary(
        record, fit_contexts[[index]]$complete, distribution_input
      ),
      error = function(error) {
        diagnostic_failures <<- c(diagnostic_failures, sprintf(
          paste(
            "Imputation %d diagnostic distribution could not be computed.",
            "The fitted model remains valid. %s"
          ), index, conditionMessage(error)
        ))
        list(observed_predicted_distribution = data.frame())
      }
    )
    distribution_by_imp[[position]] <<-
      distribution_result$observed_predicted_distribution
    diagnostics
  })
  diagnostics <- Filter(function(value) is.data.frame(value) && nrow(value),
                        diagnostics_by_imp)
  diagnostics <- if (length(diagnostics)) diagnostics[[1L]] else data.frame()
  if (nrow(diagnostics)) diagnostics$imputation_display <- "first fitted imputation"
  grouped_by_imp <- lapply(valid_indices, function(index) {
    .rls_count_grouped_response(record, fit_contexts[[index]]$complete$data)
  })
  perfect_counts <- vapply(grouped_by_imp, function(grouped) {
    sum(grouped$successes == grouped$trials)
  }, numeric(1L))
  non_perfect_counts <- vapply(grouped_by_imp, function(grouped) {
    sum(grouped$successes < grouped$trials)
  }, numeric(1L))
  perfect_proportions <- perfect_counts / (perfect_counts + non_perfect_counts)
  predicted_perfect <- vapply(valid_fits, function(fit) {
    mean(as.numeric(
      fit$linkeda_component_nu_fv %||% fit$nu.fv
    ), na.rm = TRUE)
  }, numeric(1L))
  predicted_perfect_counts <- vapply(valid_fits, function(fit) {
    sum(as.numeric(
      fit$linkeda_component_nu_fv %||% fit$nu.fv
    ), na.rm = TRUE)
  }, numeric(1L))
  dispersion_values <- vapply(valid_fits, .rls_glm_fit_dispersion, numeric(1L))
  observed_predicted_distribution <- data.frame()
  usable_distributions <- Filter(
    function(value) is.data.frame(value) && nrow(value), distribution_by_imp
  )
  if (length(usable_distributions) && all(vapply(
      usable_distributions[-1L], function(value) {
        identical(value$score, usable_distributions[[1L]]$score)
      }, logical(1L)))) {
    observed_predicted_distribution <- usable_distributions[[1L]]
    for (column in c(
      "observed_frequency", "predicted_frequency", "observed", "predicted"
    )) {
      observed_predicted_distribution[[column]] <- rowMeans(vapply(
        usable_distributions, `[[`, numeric(nrow(observed_predicted_distribution)),
        column
      ))
    }
  }
  residual_deviance <- vapply(valid_fits, .rls_glm_fit_residual_deviance, numeric(1L))
  rows_used <- Reduce(intersect, lapply(complete_by_imp, `[[`, "original_rows"))
  rows_excluded <- setdiff(.rls_mi_original_row_ids(dataset), rows_used)
  captured_fit_warnings <- c(unlist(Map(function(imputation, warnings) {
    if (!length(warnings)) return(character())
    paste0("Imputation ", imputation, ": ", warnings)
  }, seq_along(fit_warnings), fit_warnings), use.names = FALSE),
  diagnostic_failures)
  all_tests <- do.call(rbind, term_tests)
  rownames(all_tests) <- NULL
  .rls_assert_hurdle_one_df_consistency(coefficient_rows, all_tests)
  converged <- all(vapply(valid_fits, function(fit) isTRUE(fit$converged), logical(1L)))
  fit_method <- paste(
    if (any(vapply(valid_fits, function(fit) {
      isTRUE(fit$linkeda_hurdle_component_fallback)
    }, logical(1L)))) {
      paste(
        "When the joint numerical Hessian was singular, the same exact hurdle",
        "likelihood was fitted in its two independent R components."
      )
    } else "The joint ZABB fits supplied both likelihood components.",
    "Both components use Rubin's rules separately; beta-binomial dispersion is",
    "summarized descriptively across imputation-specific fits; likelihood",
    "statistics are not pooled across imputations."
  )
  result <- list(
    fit = reference_fit, fitted_glm = reference_fit,
    fits_by_imputation = fits, coefficients = coefficients,
    coefficient_rows = coefficient_rows,
    component_coefficients = coefficient_tables,
    component_term_tests = term_tests,
    parent_term_tests = all_tests, term_tests = all_tests,
    summary = list(
      n_used = as.integer(round(mean(vapply(
        complete_by_imp, function(value) nrow(value$data), numeric(1L)
      )))),
      n_excluded = length(rows_excluded), null_deviance = NA_real_,
      residual_deviance = mean(residual_deviance, na.rm = TRUE),
      df_residual = min(vapply(valid_fits, stats::df.residual, numeric(1L))),
      aic = NA_real_, bic = NA_real_, log_lik = NA_real_,
      dispersion = mean(dispersion_values, na.rm = TRUE),
      beta_binomial_dispersion = mean(dispersion_values, na.rm = TRUE),
      beta_binomial_dispersion_by_imputation = dispersion_values,
      family = record$family, link = record$link,
      count_distribution = record$count_distribution,
      trials = if (nzchar(record$trials_variable %||% "")) NA_real_
        else record$trials_constant,
      fitter = if (any(vapply(valid_fits, function(fit) {
        isTRUE(fit$linkeda_hurdle_component_fallback)
      }, logical(1L)))) {
        paste(
          "Exact factorized hurdle likelihood: stats::glm plus",
          "gamlss::gamlss with gamlss.dist::ZABB"
        )
      } else "gamlss::gamlss with gamlss.dist::ZABB",
      statistic_name = "t", likelihood_available = FALSE,
      likelihood_ratio_available = FALSE, pooled_likelihood_available = FALSE,
      perfect_scores = mean(perfect_counts),
      perfect_scores_by_imputation = perfect_counts,
      non_perfect_scores = mean(non_perfect_counts),
      non_perfect_scores_by_imputation = non_perfect_counts,
      perfect_score_proportion = mean(perfect_proportions),
      perfect_score_proportion_by_imputation = perfect_proportions,
      predicted_perfect_score_proportion = mean(predicted_perfect),
      predicted_perfect_score_proportion_by_imputation = predicted_perfect,
      predicted_perfect_score_count = mean(predicted_perfect_counts),
      predicted_perfect_score_count_by_imputation = predicted_perfect_counts,
      observed_ceiling_count_constant = length(unique(perfect_counts)) == 1L,
      perfect_score_component_status = if (no_perfect_scores) paste(
        "Perfect-score logistic component not estimated because no perfect",
        "scores were observed in any imputation; its probability is fixed at zero."
      ) else sprintf(
        "Perfect-score logistic component fitted successfully in all %d imputations.",
        length(valid_fits)
      ),
      below_ceiling_component_status = sprintf(
        "Truncated beta-binomial component below the ceiling fitted successfully in all %d imputations.",
        length(valid_fits)
      ),
      hurdle_successful_imputations = length(valid_fits),
      hurdle_attempted_imputations = length(fits),
      observed_predicted_distribution = observed_predicted_distribution,
      observed_predicted_distribution_by_imputation = distribution_by_imp,
      diagnostic_capabilities = .rls_discrete_diagnostic_capabilities(record),
      converged = converged, iterations = {
        values <- vapply(valid_fits, .rls_glm_fit_iterations,
                         integer(1L), USE.NAMES = FALSE)
        values <- values[is.finite(values)]
        if (length(values)) max(values) else NA_integer_
      },
      boundary = FALSE, rank = .rls_glm_fit_rank(reference_fit),
      parameter_count = .rls_glm_fit_parameter_count(reference_fit),
      rank_deficient = FALSE, warnings = captured_fit_warnings,
      fit_information_method = fit_method
    ),
    rows_used = rows_used, rows_excluded = rows_excluded,
    diagnostics = diagnostics, diagnostic_data = diagnostics,
    diagnostics_by_imputation = diagnostics_by_imp,
    warnings = captured_fit_warnings,
    multiple_imputation = .rls_mi_result_metadata(
      dataset, "ceiling_hurdle_beta_binomial",
      list(response = record$response, terms = record$terms,
           distribution = record$count_distribution,
           trials_variable = record$trials_variable,
           trials_constant = record$trials_constant),
      fits_by_imputation = fits,
      estimates_by_imputation = lapply(fits, function(fit) {
        if (is.null(fit)) return(NULL)
        list(
          perfect_score = if (no_perfect_scores) numeric() else
            .rls_hurdle_component_coefficients(fit, "perfect_score"),
          below_ceiling = .rls_hurdle_component_coefficients(fit, "below_ceiling")
        )
      }),
      pooled_result = coefficient_tables,
      pooling_method = "Rubin scalar pooling separately for hurdle and conditional coefficients",
      warnings = c(captured_fit_warnings, fit_method),
      complete_data_result_by_imputation = complete_by_imp,
      diagnostic_summary_by_imputation = diagnostics_by_imp
    ),
    status = if (no_perfect_scores) sprintf(
      paste(
        "Upper-truncated beta-binomial fitted across %d imputations; no",
        "perfect scores were observed, so perfect-score probability is fixed at zero."
      ), length(valid_fits)
    ) else sprintf(
      "Ceiling-hurdle beta-binomial fitted across %d imputations; both components were pooled separately.",
      length(valid_fits)
    )
  )
  diagnostic_warnings <- .rls_discrete_diagnostic_warnings(
    result$summary, result$summary$n_used
  )
  if (length(diagnostic_warnings)) {
    result$warnings <- unique(c(result$warnings, diagnostic_warnings))
    result$summary$warnings <- unique(c(result$summary$warnings, diagnostic_warnings))
    result$multiple_imputation$warnings <- unique(c(
      result$multiple_imputation$warnings, diagnostic_warnings
    ))
  }
  result
}

.rls_mi_fit_generalized_model_record_impl <- function(record) {
  operation_started <- proc.time()[["elapsed"]]
  dataset <- .rls_dataset_record(record$group)
  task_id <- record$id %||% paste0("generalized-", record$group, "-", record$response)
  cancel_path <- tempfile("linkeda-generalized-cancel-", fileext = ".signal")
  on.exit(unlink(cancel_path), add = TRUE)
  progress <- function(phase, status = "running", completed = 0L, total = 0L,
                       running = 0L, workers = 1L, imputation = 0L,
                       fit_seconds = NA_real_, warnings = 0L, failures = 0L,
                       message = "") {
    .rls_mi_progress_event(task_id, record$group, phase, status, completed,
      total, running, workers, imputation, fit_seconds,
      proc.time()[["elapsed"]] - operation_started, warnings, failures,
      cancel_path, message)
  }
  .rls_mi_trace(
    "backend selected", "multiple_imputation",
    sprintf("dataset=%s", record$group),
    sprintf("m=%d", length(dataset$completed_datasets %||% list())),
    sprintf("model=%s", if (isTRUE(record$count_regression)) "count" else if (isTRUE(record$binary_regression)) "binary" else "generalized")
  )
  total_imputations <- length(dataset$completed_datasets %||% list())
  progress("preparing", total = total_imputations,
    message = sprintf("Preparing %d completed datasets\u2026", total_imputations))
  preparation_started <- proc.time()[["elapsed"]]
  formula <- .rls_generalized_glm_formula_object(record)
  complete_by_imp <- .rls_mi_fit_data_by_imputation(record, formula, record$scope %||% "all")
  family_object <- .rls_glm_make_family(record$family, record$link)
  if (isTRUE(record$count_regression) &&
      identical(record$count_distribution, "negative_binomial") &&
      !requireNamespace("MASS", quietly = TRUE)) {
    .rls_require_optional_packages("MASS", "Negative-binomial Count Model")
  }
  if (isTRUE(record$count_regression) &&
      identical(record$count_distribution, "beta_binomial") &&
      !requireNamespace("glmmTMB", quietly = TRUE)) {
    .rls_require_optional_packages("glmmTMB", "Beta-binomial Count Model")
  }
  fit_contexts <- lapply(complete_by_imp, function(complete) {
    fit_record <- record
    fit_record$family_object <- family_object
    coding <- NULL
    if (isTRUE(record$binary_regression)) {
      prepared <- .rls_binary_prepare_generalized_fit(record, fit_record, complete)
      fit_record <- prepared$fit_record
      complete <- prepared$complete
      coding <- prepared$coding
    }
    if (identical(record$model_type %||% "", "positive_continuous")) {
      .rls_generalized_validate_response(record, complete$data[[record$response]])
    } else if (identical(record$model_type %||% "", "proportion")) {
      domain <- if (identical(record$family, "beta"))
        "open_unit_interval" else "open_closed_unit_interval"
      .rls_validate_response_domain(
        complete$data[[record$response]], domain, "Proportion Model"
      )
    }
    complete <- .rls_glm_transform_bounded_response(fit_record, complete)
    list(
      complete = complete,
      coding = coding,
      formula = .rls_generalized_glm_formula_object(fit_record),
      fit_record = fit_record
    )
  })
  if (isTRUE(record$binary_regression)) {
    coding_ids <- vapply(fit_contexts, function(context) {
      paste(context$coding$event, context$coding$reference, sep = "\r")
    }, character(1L))
    if (length(unique(coding_ids)) != 1L) {
      stop("Binary event/reference coding is inconsistent across imputations.", call. = FALSE)
    }
  }
  preparation_seconds <- proc.time()[["elapsed"]] - preparation_started
  worker_count <- .rls_mi_worker_count(length(fit_contexts))
  progress("workers", total = length(fit_contexts), workers = worker_count,
    message = if (worker_count > 1L) sprintf("Attempting to start %d R worker processes\u2026", worker_count)
      else "Using sequential execution.")
  tasks <- lapply(seq_along(fit_contexts), function(i) list(index = i,
    complete = fit_contexts[[i]]$complete,
    specification = .rls_mi_fit_specification(fit_contexts[[i]]$fit_record)))
  fit_warning_count <- fit_failure_count <- 0L
  fit_progress <- function(result = NULL, completed, total, running, workers,
                           elapsed, message = "") {
    if (!is.null(result)) {
      diagnostic_issue <- nzchar(result$diagnostic_error %||% "") ||
        nzchar(result$distribution_error %||% "")
      fit_warning_count <<- fit_warning_count + length(result$warnings) +
        as.integer(nzchar(result$diagnostic_error %||% "")) +
        as.integer(nzchar(result$distribution_error %||% ""))
      fit_failure_count <<- fit_failure_count + as.integer(is.null(result$fit))
      detail <- if (is.null(result$fit)) sprintf(
        "[%d/%d] FAILED \u2014 imputation %d \u2014 %.1f s: %s",
        completed, total, result$index, result$seconds, result$error)
      else sprintf(paste0("[%d/%d] completed%s \u2014 imputation %d \u2014 ",
                          "fit %.1f s; diagnostics %.1f s"),
        completed, total,
        if (length(result$warnings) || diagnostic_issue) " with warning" else "",
        result$index, result$fit_seconds, result$diagnostic_seconds)
      warning_detail <- c(result$warnings, result$diagnostic_error,
        result$distribution_error)
      warning_detail <- warning_detail[nzchar(warning_detail)]
      if (length(warning_detail) && !is.null(result$fit))
        detail <- paste0(detail, " \u2014 ", substr(warning_detail[[1L]], 1L, 160L))
      .rls_mi_trace(
        sprintf("imputation %d/%d", result$index, total),
        if (is.null(result$fit)) "failed" else "fitted",
        .elapsed = result$fit_seconds %||% result$seconds
      )
    } else detail <- message
    progress("fitting", completed = completed, total = total, running = running,
      workers = workers, imputation = result$index %||% 0L,
      fit_seconds = result$fit_seconds %||% NA_real_, warnings = fit_warning_count,
      failures = fit_failure_count, message = detail)
  }
  execution <- tryCatch(
    .rls_mi_fit_imputations(tasks, .rls_mi_fit_task, workers = worker_count,
      progress = fit_progress, cancel_path = cancel_path),
    linkeda_mi_cancelled = function(condition) {
      progress("cancelled", "cancelled", completed = condition$completed,
        total = condition$total, workers = worker_count,
        message = conditionMessage(condition))
      stop(condition)
    }
  )
  fit_results <- execution$results
  fits <- lapply(fit_results, `[[`, "fit")
  fit_errors <- vapply(fit_results, `[[`, character(1L), "error")
  fit_warnings <- lapply(fit_results, `[[`, "warnings")
  technical_warnings <- unique(unlist(lapply(fit_results, `[[`,
    "technical_warnings"), use.names = FALSE))
  fit_diagnostics <- data.frame(
    imputation = seq_along(fit_results),
    seconds = vapply(fit_results, `[[`, numeric(1L), "fit_seconds"),
    diagnostic_seconds = vapply(fit_results, `[[`, numeric(1L),
      "diagnostic_seconds"),
    worker_total_seconds = vapply(fit_results, `[[`, numeric(1L), "seconds"),
    success = !vapply(fits, is.null, logical(1L)),
    warning = vapply(fit_results, function(value) paste(c(value$warnings,
      if (nzchar(value$diagnostic_error %||% "")) paste0(
        "Diagnostic: ", value$diagnostic_error) else character(),
      if (nzchar(value$distribution_error %||% "")) paste0(
        "Distribution diagnostic: ", value$distribution_error)
      else character()), collapse = "; "), character(1L)),
    error = fit_errors,
    convergence = vapply(fit_results, `[[`, logical(1L), "convergence"),
    stringsAsFactors = FALSE
  )
  progress("collecting", completed = length(fits), total = length(fits),
    workers = execution$workers, warnings = fit_warning_count,
    failures = fit_failure_count, message = "Fit results collected in imputation order.")
  if (!any(!vapply(fits, is.null, logical(1L)))) {
    details <- .rls_mi_condition_summary(fit_errors)
    if (length(details)) {
      stop(sprintf(
        "The generalized linear model could not be fitted in any imputation: %s",
        paste(details, collapse = "; ")
      ), call. = FALSE)
    }
    stop("Not enough complete cases in the imputations to fit the generalized linear model.", call. = FALSE)
  }
  failed_imputations <- which(vapply(fits, is.null, logical(1L)))
  if (length(failed_imputations)) {
    missing_details <- fit_errors
    blank_failures <- failed_imputations[
      !nzchar(missing_details[failed_imputations])
    ]
    missing_details[blank_failures] <- "insufficient complete observations"
    details <- .rls_mi_condition_summary(missing_details, seq_along(fits))
    if (.rls_count_is_ceiling_hurdle(record)) {
      stop(sprintf(
        paste(
          "The hurdle beta-binomial model was not pooled.",
          "A required fit failed: %s."
        ), paste(details, collapse = "; ")
      ), call. = FALSE)
    }
    stop(sprintf(
      "The model was not pooled because one or more imputation-specific fits failed: %s.",
      paste(details, collapse = "; ")
    ), call. = FALSE)
  }
  reference_fit <- fits[[which(!vapply(fits, is.null, logical(1L)))[[1L]]]]
  if (.rls_count_is_ceiling_hurdle(record)) {
    progress("pooling", completed = length(fits), total = length(fits),
      workers = execution$workers,
      message = "Pooling hurdle components with Rubin's rules\u2026")
    result <- .rls_mi_extract_ceiling_hurdle(
      record, dataset, fits, fit_contexts, complete_by_imp, fit_warnings
    )
    total_seconds <- proc.time()[["elapsed"]] - operation_started
    result$execution_diagnostics <- list(
      workers = execution$workers, preparation_seconds = preparation_seconds,
      worker_pids = execution$worker_pids,
      technical_warnings = technical_warnings,
      worker_startup_seconds = execution$startup_seconds,
      model_fitting_seconds = execution$fitting_seconds,
      worker_fit_and_diagnostics_seconds = execution$fitting_seconds,
      imputation_fit_work_seconds = sum(fit_diagnostics$seconds),
      diagnostic_work_seconds = sum(fit_diagnostics$diagnostic_seconds),
      result_collection_seconds = execution$collection_seconds,
      total_seconds = total_seconds, fits = fit_diagnostics
    )
    progress("delivering", completed = length(fits), total = length(fits),
      workers = execution$workers, warnings = fit_warning_count,
      failures = fit_failure_count,
      message = "Calculations finished; preparing results for the model window\u2026")
    return(result)
  }
  pooling_started <- proc.time()[["elapsed"]]
  pooling_label <- if (inherits(reference_fit, c("betareg", "gamlss", "glmmTMB"))) {
    "Rubin scalar pooling of mean-submodel coefficients"
  } else "mice::pool"
  .rls_mi_trace("pooling", "start", pooling_label)
  progress("pooling", completed = length(fits), total = length(fits),
    workers = execution$workers, message = "Pooling coefficients with Rubin's rules\u2026")
  coefs <- .rls_mi_pool_generalized_mean_coefficients(fits, statistic_name = "t")
  pooling_seconds <- proc.time()[["elapsed"]] - pooling_started
  .rls_mi_trace("pooling", "complete", pooling_label,
                .elapsed = proc.time()[["elapsed"]] - pooling_started)
  semantic_started <- proc.time()[["elapsed"]]
  .rls_mi_trace("semantic result", "start")
  progress("formatting", completed = length(fits), total = length(fits),
    workers = execution$workers, message = "Formatting coefficient results\u2026")
  coef_display <- coefs[, c("term", "estimate", "std_error", "statistic", "p_value", "ci_lower", "ci_upper",
                            "partial_r", "delta_r2", "df",
                            "within_imputation_variance", "between_imputation_variance", "total_variance",
                            "relative_increase_variance", "fraction_missing_information"), drop = FALSE]
  coef_display <- .rls_glm_augment_exponentiated(coef_display, record)
  coefficient_rows <- .rls_model_coefficient_display_rows(
    reference_fit,
    coef_display,
    dataset$data,
    record$terms,
    statistic_name = "t"
  )
  coefficient_rows <- .rls_glm_augment_exponentiated(coefficient_rows, record)
  ordinary_linear <- inherits(reference_fit, "lm") && !inherits(reference_fit, "glm")
  global_tests_started <- proc.time()[["elapsed"]]
  progress("global_tests", completed = length(fits), total = length(fits),
    workers = execution$workers, message = "Computing global Wald tests\u2026")
  global_term_tests <- .rls_mi_generalized_term_omnibus_tests(
    fits,
    attr(stats::terms(reference_fit), "term.labels") %||% character(),
    # Pooled GLM/quasi-GLM terms use a transparent Rubin covariance Wald
    # statistic.  The ordinary lognormal lm remains on the linear-model F path.
    test = if (ordinary_linear) "wald_f" else "wald_chisq"
  )
  global_tests_seconds <- proc.time()[["elapsed"]] - global_tests_started
  parent_test_terms <- .rls_model_parent_test_terms(reference_fit, dataset$data)
  parent_term_tests <- global_term_tests[vapply(global_term_tests$term, function(term) {
    any(vapply(parent_test_terms, .rls_model_terms_equivalent, logical(1L),
               b = term))
  }, logical(1L)), , drop = FALSE]
  coefficient_rows <- .rls_model_apply_parent_term_tests(
    coefficient_rows, parent_term_tests
  )
  valid_fits <- fits[!vapply(fits, is.null, logical(1L))]
  converged <- all(vapply(valid_fits, function(fit) {
    if (inherits(fit, "glmmTMB")) {
      isTRUE(fit$sdr$pdHess) && identical(as.integer(fit$fit$convergence %||% 1L), 0L)
    } else if (inherits(fit, "lm") && !inherits(fit, "glm")) TRUE else isTRUE(fit$converged)
  }, logical(1L)))
  iterations <- suppressWarnings(max(vapply(
    valid_fits, .rls_glm_fit_iterations, integer(1L)
  ), na.rm = TRUE))
  if (!is.finite(iterations)) iterations <- NA_integer_
  boundary <- any(vapply(valid_fits, function(fit) isTRUE(fit$boundary), logical(1L)))
  ranks <- vapply(valid_fits, .rls_glm_fit_rank, integer(1L))
  mean_parameter_counts <- vapply(
    valid_fits, function(fit) length(.rls_glm_mean_coefficients(fit)), integer(1L)
  )
  rows_used <- Reduce(intersect, lapply(complete_by_imp, `[[`, "original_rows"))
  rows_excluded <- setdiff(.rls_mi_original_row_ids(dataset), rows_used)
  diagnostics_by_imp <- lapply(seq_along(fits), function(i) {
    if (is.null(fits[[i]])) return(data.frame())
    diagnostics <- fit_results[[i]]$diagnostics %||% data.frame()
    coding <- fit_contexts[[i]]$coding
    if (!is.null(coding) && nrow(diagnostics)) {
      diagnostics$observed_binary <- diagnostics$observed
      diagnostics$observed_label <- coding$original
      diagnostics$observed_original <- coding$original
    }
    diagnostics
  })
  first_diagnostic <- which(lengths(diagnostics_by_imp) > 0L)
  diagnostics <- if (length(first_diagnostic))
    diagnostics_by_imp[[first_diagnostic[[1L]]]] else data.frame()
  if (nrow(diagnostics))
    diagnostics$imputation_display <- "first fitted imputation"
  bounded_distributions_by_imputation <- lapply(seq_along(fits), function(i) {
    fit_results[[i]]$distribution %||% data.frame()
  })
  usable_bounded_distributions <- Filter(
    function(value) is.data.frame(value) && nrow(value),
    bounded_distributions_by_imputation
  )
  observed_predicted_distribution <- data.frame()
  if (length(usable_bounded_distributions) && all(vapply(
      usable_bounded_distributions[-1L], function(value) {
        identical(value$score, usable_bounded_distributions[[1L]]$score)
      }, logical(1L)))) {
    observed_predicted_distribution <- usable_bounded_distributions[[1L]]
    for (column in c(
      "observed_frequency", "predicted_frequency", "observed", "predicted"
    )) {
      observed_predicted_distribution[[column]] <- rowMeans(vapply(
        usable_bounded_distributions, `[[`,
        numeric(nrow(observed_predicted_distribution)), column
      ))
    }
  }
  ordinary_likelihood <- all(vapply(valid_fits, .rls_mi_fit_has_ordinary_likelihood, logical(1L)))
  fitter <- if (identical(record$family, "beta")) {
    "betareg::betareg"
  } else if (identical(record$family, "beta_one_inflated")) {
    "gamlss::gamlss with gamlss.dist::BEOI"
  } else if (isTRUE(record$count_regression) &&
                identical(record$count_distribution, "negative_binomial")) {
    "MASS::glm.nb"
  } else if (isTRUE(record$count_regression) &&
                identical(record$count_distribution, "beta_binomial")) {
    "glmmTMB::glmmTMB"
  } else if (identical(record$family, "lognormal")) {
    "stats::lm"
  } else {
    "stats::glm"
  }
  fit_classes <- unique(vapply(valid_fits, function(fit) paste(class(fit), collapse = "/"), character(1L)))
  theta_values <- vapply(valid_fits, function(fit) as.numeric(fit$theta %||% NA_real_), numeric(1L))
  beta_binomial_precision_values <- if (identical(record$count_distribution %||% "", "beta_binomial")) {
    vapply(valid_fits, .rls_glm_fit_dispersion, numeric(1L))
  } else numeric()
  binomial_overdispersion_values <- if (identical(record$count_distribution %||% "", "binomial_trials")) {
    valid_indices <- which(!vapply(fits, is.null, logical(1L)))
    vapply(seq_along(valid_fits), function(position) {
      .rls_glm_family_diagnostics(record, valid_fits[[position]],
        fit_contexts[[valid_indices[[position]]]]$complete)$pearson_dispersion_ratio %||% NA_real_
    }, numeric(1L))
  } else numeric()
  inflation_values <- vapply(valid_fits, function(fit) {
    value <- if (inherits(fit, "gamlss")) {
      mean(as.numeric(
        fit$linkeda_component_nu_fv %||% fit$nu.fv %||% NA_real_
      ), na.rm = TRUE)
    } else NA_real_
    if (is.finite(value)) value else NA_real_
  }, numeric(1L))
  binary_coding <- if (isTRUE(record$binary_regression)) fit_contexts[[1L]]$coding else NULL
  captured_fit_warnings <- unlist(Map(function(imputation, warnings) {
    if (!length(warnings)) return(character())
    paste0("Imputation ", imputation, ": ", warnings)
  }, seq_along(fit_warnings), fit_warnings), use.names = FALSE)
  diagnostic_failures <- unlist(lapply(seq_along(fit_results), function(i) {
    result <- fit_results[[i]]
    c(
      if (nzchar(result$diagnostic_error %||% "")) sprintf(
        "Imputation %d diagnostic could not be computed; the fitted model remains valid: %s",
        i, result$diagnostic_error
      ) else character(),
      if (nzchar(result$distribution_error %||% "")) sprintf(
        "Imputation %d diagnostic distribution could not be computed; the fitted model remains valid: %s",
        i, result$distribution_error
      ) else character()
    )
  }), use.names = FALSE)
  captured_fit_warnings <- unique(c(captured_fit_warnings, diagnostic_failures))
  ceiling_counts <- if (isTRUE(record$count_regression) &&
      .rls_count_distribution_uses_trials(record$count_distribution)) {
    grouped <- lapply(fit_contexts, function(context) {
      .rls_count_grouped_response(record, context$complete$data)
    })
    perfect <- vapply(grouped, function(value) {
      sum(value$successes == value$trials)
    }, numeric(1L))
    non_perfect <- vapply(grouped, function(value) {
      sum(value$successes < value$trials)
    }, numeric(1L))
    summary <- list(
      trials = if (nzchar(record$trials_variable %||% "")) NA_real_
        else record$trials_constant,
      perfect_scores = mean(perfect), perfect_scores_by_imputation = perfect,
      non_perfect_scores = mean(non_perfect),
      non_perfect_scores_by_imputation = non_perfect,
      perfect_score_proportion = mean(perfect / (perfect + non_perfect)),
      perfect_score_proportion_by_imputation = perfect / (perfect + non_perfect),
      predicted_perfect_score_proportion = {
        values <- vapply(diagnostics_by_imp, function(value) {
          if (is.data.frame(value) && nrow(value) &&
              "perfect_score_probability" %in% names(value)) {
            mean(value$perfect_score_probability, na.rm = TRUE)
          } else NA_real_
        }, numeric(1L))
        if (any(is.finite(values))) mean(values[is.finite(values)]) else NA_real_
      },
      observed_predicted_distribution = observed_predicted_distribution,
      observed_predicted_distribution_by_imputation =
        bounded_distributions_by_imputation
    )
    if (nrow(observed_predicted_distribution)) {
      summary$floor_scores <- observed_predicted_distribution$observed_frequency[[1L]]
      summary$floor_score_proportion <- observed_predicted_distribution$observed[[1L]]
      summary$predicted_floor_count <-
        observed_predicted_distribution$predicted_frequency[[1L]]
      summary$predicted_floor_proportion <-
        observed_predicted_distribution$predicted[[1L]]
      last <- nrow(observed_predicted_distribution)
      summary$predicted_perfect_score_count <-
        observed_predicted_distribution$predicted_frequency[[last]]
      summary$predicted_perfect_score_proportion <-
        observed_predicted_distribution$predicted[[last]]
    }
    summary
  } else list()
  discrete_summary <- list(
    diagnostic_capabilities = .rls_discrete_diagnostic_capabilities(record),
    observed_predicted_distribution = observed_predicted_distribution,
    observed_predicted_distribution_by_imputation =
      bounded_distributions_by_imputation
  )
  if (nrow(observed_predicted_distribution)) {
    discrete_summary$floor_scores <-
      observed_predicted_distribution$observed_frequency[[1L]]
    discrete_summary$floor_score_proportion <-
      observed_predicted_distribution$observed[[1L]]
    discrete_summary$predicted_floor_count <-
      observed_predicted_distribution$predicted_frequency[[1L]]
    discrete_summary$predicted_floor_proportion <-
      observed_predicted_distribution$predicted[[1L]]
  }
  for (name in names(ceiling_counts)) discrete_summary[[name]] <- ceiling_counts[[name]]
  result <- list(
    fit = reference_fit,
    fitted_glm = reference_fit,
    fits_by_imputation = fits,
    coefficients = coef_display,
    coefficient_rows = coefficient_rows,
    parent_term_tests = parent_term_tests,
    term_tests = global_term_tests,
    summary = c(list(
      n_used = as.integer(round(mean(vapply(complete_by_imp, function(x) nrow(x$data), numeric(1L))))),
      n_excluded = length(rows_excluded),
      null_deviance = {
        values <- vapply(valid_fits, function(fit) as.numeric(fit$null.deviance %||% NA_real_), numeric(1L))
        if (any(is.finite(values))) mean(values[is.finite(values)]) else NA_real_
      },
      residual_deviance = {
        values <- vapply(valid_fits, .rls_glm_fit_residual_deviance, numeric(1L))
        if (any(is.finite(values))) mean(values[is.finite(values)]) else NA_real_
      },
      df_residual = min(vapply(valid_fits, stats::df.residual, numeric(1L)), na.rm = TRUE),
      aic = NA_real_,
      bic = NA_real_,
      dispersion = if (identical(record$count_distribution %||% "", "negative_binomial")) {
        NA_real_
      } else {
        values <- vapply(valid_fits, .rls_glm_fit_dispersion, numeric(1L))
        if (any(is.finite(values))) mean(values[is.finite(values)]) else NA_real_
      },
      log_lik = NA_real_,
      family = record$family,
      link = record$link,
      response_bounds = record$response_bounds,
      response_transformation = record$response_transformation,
      inflation_probability = if (any(is.finite(inflation_values))) {
        mean(inflation_values[is.finite(inflation_values)])
      } else NA_real_,
      count_distribution = record$count_distribution %||% "poisson",
      fitter = fitter,
      fit_class = fit_classes,
      ordinary_likelihood_by_imputation = ordinary_likelihood,
      likelihood_available = FALSE,
      likelihood_ratio_available = FALSE,
      pooled_likelihood_available = FALSE,
      theta = NA_real_,
      theta_descriptive_mean = if (any(is.finite(theta_values))) mean(theta_values[is.finite(theta_values)]) else NA_real_,
      theta_descriptive_min = if (any(is.finite(theta_values))) min(theta_values[is.finite(theta_values)]) else NA_real_,
      theta_descriptive_max = if (any(is.finite(theta_values))) max(theta_values[is.finite(theta_values)]) else NA_real_,
      theta_by_imputation = theta_values,
      beta_binomial_precision = NA_real_,
      beta_binomial_precision_by_imputation = beta_binomial_precision_values,
      binomial_overdispersion_ratio = if (any(is.finite(binomial_overdispersion_values)))
        mean(binomial_overdispersion_values[is.finite(binomial_overdispersion_values)]) else NA_real_,
      binomial_overdispersion_by_imputation = binomial_overdispersion_values,
      family_diagnostics = .rls_glm_family_diagnostics_mi(
        record, valid_fits,
        lapply(which(!vapply(fits, is.null, logical(1L))), function(index) fit_contexts[[index]]$complete)
      ),
      multiple_imputation = TRUE,
      event = binary_coding$event %||% record$event %||% NULL,
      reference = binary_coding$reference %||% record$reference %||% NULL,
      converged = converged,
      iterations = as.integer(iterations),
      boundary = boundary,
      rank = .rls_glm_fit_rank(reference_fit),
      parameter_count = .rls_glm_fit_parameter_count(reference_fit),
      rank_deficient = any(is.finite(ranks) & ranks < mean_parameter_counts),
      warnings = captured_fit_warnings,
      fit_information_method = if (identical(record$family, "lognormal")) {
        paste(
          "Regression coefficients use Rubin's rules on the log-response scale;",
          "diagnostic residuals are on the log-response scale; response-scale arithmetic means",
          "use the imputation-specific sigma-squared correction."
        )
      } else if (identical(record$count_distribution %||% "", "negative_binomial")) {
        paste(
          "Deviance is a descriptive mean across imputations; negative-binomial theta",
          "is summarized across imputation-specific fits and is not Rubin-pooled;",
          "AIC/BIC/logLik and likelihood-ratio statistics are not pooled."
        )
      } else if (identical(record$count_distribution %||% "", "beta_binomial")) {
        paste(
          "Mean-submodel coefficients use Rubin covariance pooling; beta-binomial precision",
          "is summarized across imputation-specific fits and is not Rubin-pooled;",
          "AIC/BIC/logLik and likelihood-ratio statistics are not pooled."
        )
      } else {
        paste(
          "Deviance and dispersion are descriptive means across imputations;",
          "AIC/BIC/logLik and likelihood-ratio statistics are not pooled."
        )
      }
    ), discrete_summary),
    rows_used = rows_used,
    rows_excluded = rows_excluded,
    diagnostics = diagnostics,
    diagnostic_data = diagnostics,
    diagnostics_by_imputation = diagnostics_by_imp,
    execution_diagnostics = list(
      workers = execution$workers,
      worker_pids = execution$worker_pids,
      technical_warnings = technical_warnings,
      preparation_seconds = preparation_seconds,
      worker_startup_seconds = execution$startup_seconds,
      model_fitting_seconds = execution$fitting_seconds,
      worker_fit_and_diagnostics_seconds = execution$fitting_seconds,
      imputation_fit_work_seconds = sum(fit_diagnostics$seconds),
      diagnostic_work_seconds = sum(fit_diagnostics$diagnostic_seconds),
      result_collection_seconds = execution$collection_seconds,
      pooling_seconds = pooling_seconds,
      global_tests_seconds = global_tests_seconds,
      fits = fit_diagnostics
    ),
    warnings = captured_fit_warnings,
    multiple_imputation = .rls_mi_result_metadata(
      dataset,
      "generalized_linear_model",
      list(response = record$response, terms = record$terms, family = record$family,
           link = record$link, response_bounds = record$response_bounds,
           formula = deparse(formula)),
      fits_by_imputation = fits,
      estimates_by_imputation = lapply(fits, function(fit) if (is.null(fit)) NULL else .rls_glm_mean_coefficients(fit)),
      pooled_result = coefs,
      pooling_method = attr(coefs, "pooling_method") %||% "mice::pool (Rubin's rules on link-scale coefficients)",
      warnings = c(
        captured_fit_warnings,
        if (identical(record$count_distribution %||% "", "negative_binomial"))
          "Deviance is summarized descriptively across imputations; log-likelihood, AIC, BIC, and likelihood-ratio statistics are not pooled for MI negative-binomial models."
        else
          "Deviance and dispersion are descriptive by-imputation summaries; log-likelihood, AIC, BIC, and likelihood-ratio statistics are not pooled for MI GLMs.",
        if (identical(record$count_distribution %||% "", "negative_binomial"))
          "Negative-binomial theta is estimated separately in every imputation and is not pooled." else character(),
        if (inherits(reference_fit, "betareg"))
          "Beta precision is summarized descriptively across imputations; mean-submodel coefficients use Rubin's rules." else character(),
        if (inherits(reference_fit, "gamlss"))
          "One-inflation probability and beta dispersion are summarized descriptively across imputations; mean-submodel coefficients use Rubin's rules." else character()
      ),
      complete_data_result_by_imputation = complete_by_imp,
      diagnostic_summary_by_imputation = diagnostics_by_imp
    ),
    status = sprintf(
      "Multiple imputation generalized linear model fitted across %d imputations using %s.",
      length(fits), pooling_label
    )
  )
  diagnostic_warnings <- .rls_discrete_diagnostic_warnings(
    result$summary, result$summary$n_used
  )
  if (length(diagnostic_warnings)) {
    result$warnings <- unique(c(result$warnings, diagnostic_warnings))
    result$summary$warnings <- unique(c(
      result$summary$warnings %||% character(), diagnostic_warnings
    ))
    result$status <- paste(result$status, paste(diagnostic_warnings, collapse = " "))
  }
  .rls_mi_trace("semantic result", "complete",
                .elapsed = proc.time()[["elapsed"]] - semantic_started)
  formatting_seconds <- proc.time()[["elapsed"]] - semantic_started
  total_seconds <- proc.time()[["elapsed"]] - operation_started
  result$execution_diagnostics$formatting_seconds <- formatting_seconds
  result$execution_diagnostics$total_seconds <- total_seconds
  progress("delivering", completed = length(fits), total = length(fits),
    workers = execution$workers, warnings = fit_warning_count,
    failures = fit_failure_count,
    message = "Calculations finished; preparing results for the model window\u2026")
  .rls_mi_trace("generalized MI operation", "complete",
                .elapsed = proc.time()[["elapsed"]] - operation_started)
  result
}

.rls_mi_fit_generalized_model_record <- function(record) {
  started <- proc.time()[["elapsed"]]
  tryCatch(.rls_mi_fit_generalized_model_record_impl(record),
    error = function(error) {
      if (!inherits(error, "linkeda_mi_cancelled")) {
        .rls_mi_progress_event(
          record$id %||% paste0("generalized-", record$group, "-", record$response),
          record$group, "failed", status = "failed",
          elapsed = proc.time()[["elapsed"]] - started,
          message = conditionMessage(error)
        )
      }
      stop(error)
    })
}
