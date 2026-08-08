.rls_native_wire_value <- function(value) {
  value <- as.character(value %||% "")
  value[is.na(value)] <- ""
  gsub("[\r\n\t]+", " ", value)
}

.rls_native_wire_number <- function(value) {
  numeric_value <- suppressWarnings(as.numeric(value))
  if (!length(numeric_value) || is.na(numeric_value[[1L]]) || is.nan(numeric_value[[1L]])) return("NA")
  if (is.infinite(numeric_value[[1L]])) return(if (numeric_value[[1L]] > 0) "Inf" else "-Inf")
  sprintf("%.17g", numeric_value[[1L]])
}

.rls_native_wire_integer <- function(value) {
  numeric_value <- suppressWarnings(as.numeric(value))
  if (!length(numeric_value) || !is.finite(numeric_value[[1L]])) return("NA")
  as.character(as.integer(round(numeric_value[[1L]])))
}

.rls_native_wire_bool <- function(value) {
  if (isTRUE(value)) "TRUE" else "FALSE"
}

.rls_native_int_vector_payload <- function(value) {
  rows <- suppressWarnings(as.integer(value %||% integer()))
  rows <- rows[!is.na(rows)]
  c(as.character(length(rows)), as.character(rows))
}

.rls_native_row_value <- function(row, name, default = "") {
  if (!is.data.frame(row) || !name %in% names(row)) return(default)
  value <- row[[name]]
  if (!length(value) || is.na(value[[1L]])) return(default)
  value[[1L]]
}

.rls_native_linear_coefficient_rows <- function(record) {
  rows <- record$coefficient_rows %||% data.frame()
  if (is.data.frame(rows) && nrow(rows)) return(rows)

  coefs <- record$coefficients %||% data.frame()
  if (!is.data.frame(coefs) || !nrow(coefs)) return(data.frame())
  statistic <- if ("t_value" %in% names(coefs)) coefs$t_value else coefs$statistic %||% NA_real_
  data.frame(
    row_type = "coefficient",
    term = coefs$term,
    display_label = coefs$term,
    source_term = coefs$term,
    term_type = ifelse(coefs$term == "(Intercept)", "intercept", "numeric"),
    level = NA_character_,
    reference_level = NA_character_,
    estimate = coefs$estimate %||% NA_real_,
    std_error = coefs$std_error %||% NA_real_,
    statistic_name = "t",
    statistic = statistic,
    p_value = coefs$p_value %||% NA_real_,
    partial_r2 = coefs$partial_r2 %||% NA_real_,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.rls_native_linear_coefficient_payload <- function(record) {
  rows <- .rls_native_linear_coefficient_rows(record)
  payload <- as.character(if (is.data.frame(rows)) nrow(rows) else 0L)
  if (!is.data.frame(rows) || !nrow(rows)) return(payload)

  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    payload <- c(
      payload,
      .rls_native_wire_value(.rls_native_row_value(row, "term")),
      .rls_native_wire_value(.rls_native_row_value(row, "source_term")),
      .rls_native_wire_value(.rls_native_row_value(row, "term_type", "numeric")),
      .rls_native_wire_value(.rls_native_row_value(row, "row_type", "coefficient")),
      .rls_native_wire_value(.rls_native_row_value(row, "display_label")),
      .rls_native_wire_value(.rls_native_row_value(row, "level")),
      .rls_native_wire_value(.rls_native_row_value(row, "reference_level")),
      .rls_native_wire_number(.rls_native_row_value(row, "estimate", NA_real_)),
      .rls_native_wire_number(.rls_native_row_value(row, "standardized_beta", NA_real_)),
      .rls_native_wire_number(.rls_native_row_value(row, "std_error", NA_real_)),
      .rls_native_wire_number(.rls_native_row_value(row, "statistic", NA_real_)),
      .rls_native_wire_number(.rls_native_row_value(row, "p_value", NA_real_)),
      .rls_native_wire_number(.rls_native_row_value(row, "partial_r2", NA_real_))
    )
  }
  payload
}

.rls_native_linear_fit_payload <- function(record) {
  summary <- record$summary %||% list()
  rows <- .rls_native_linear_coefficient_rows(record)
  fit_ok <- !is.null(record$fit) || (is.data.frame(rows) && nrow(rows) > 0L)
  note <- summary$fit_information_method %||% record$status %||% ""
  c(
    .rls_native_wire_bool(fit_ok),
    .rls_native_wire_integer(summary$n_used %||% length(record$rows_used %||% integer())),
    .rls_native_wire_integer(summary$n_excluded %||% length(record$rows_excluded %||% integer())),
    .rls_native_wire_integer(summary$df_model %||% length(record$predictors %||% record$terms %||% character())),
    .rls_native_wire_integer(summary$df_residual %||% NA_real_),
    .rls_native_wire_number(summary$r_squared %||% NA_real_),
    .rls_native_wire_number(summary$adj_r_squared %||% NA_real_),
    .rls_native_wire_number(summary$global_f %||% NA_real_),
    .rls_native_wire_number(summary$global_p %||% NA_real_),
    .rls_native_wire_number(summary$ss_regression %||% NA_real_),
    .rls_native_wire_number(summary$ss_residual %||% NA_real_),
    .rls_native_wire_number(summary$ms_regression %||% NA_real_),
    .rls_native_wire_number(summary$ms_residual %||% NA_real_),
    .rls_native_wire_number(summary$rmse %||% summary$residual_se %||% NA_real_),
    .rls_native_wire_number(summary$residual_se %||% summary$rmse %||% NA_real_),
    .rls_native_wire_number(summary$aic %||% NA_real_),
    .rls_native_wire_number(summary$bic %||% NA_real_),
    .rls_native_wire_value(note),
    .rls_native_int_vector_payload(record$rows_used %||% integer()),
    .rls_native_int_vector_payload(record$rows_excluded %||% integer()),
    .rls_native_linear_coefficient_payload(record)
  )
}

.rls_glm_pooled_native_payload <- function(record) {
  imputation_count <- as.integer(record$imputation_count %||% record$multiple_imputation$m %||% 0L)
  note <- sprintf(
    "Pooled multiple-imputation linear model; m = %d; coefficients use Rubin's rules. Global model test uses D1 pooled Wald; AIC/BIC are not pooled.",
    imputation_count
  )
  c(
    "MODEL_OPEN_POOLED",
    .rls_native_wire_value(record$group),
    .rls_native_wire_value("General Linear Model - Multiple Imputation"),
    .rls_native_wire_value(record$dependent),
    .rls_native_wire_value(record$scope %||% "all"),
    as.character(imputation_count),
    .rls_native_wire_value(note),
    as.character(length(record$predictors %||% character())),
    .rls_native_wire_value(record$predictors %||% character()),
    .rls_native_linear_fit_payload(record)
  )
}

.rls_glm_sync_native_pooled <- function(record) {
  .rls_start_backend()
  dataset <- .rls_dataset_record(record$group)
  try(.rls_send(c(
    "REGISTER_DATASET_SILENT",
    record$group,
    .rls_variable_payload(record$data, dataset$variable_metadata),
    .rls_dataframe_payload(record$data, dataset$variable_metadata, dataset_record = dataset)
  )), silent = TRUE)
  try(.rls_send(.rls_glm_pooled_native_payload(record)), silent = TRUE)
}

.rls_native_linear_diagnostics_payload <- function(record) {
  diagnostics <- record$diagnostics %||% data.frame()
  payload <- as.character(if (is.data.frame(diagnostics)) nrow(diagnostics) else 0L)
  if (!is.data.frame(diagnostics) || !nrow(diagnostics)) return(payload)
  for (i in seq_len(nrow(diagnostics))) {
    row <- diagnostics[i, , drop = FALSE]
    payload <- c(
      payload,
      .rls_native_wire_integer(.rls_native_row_value(row, "row_id", NA_real_)),
      .rls_native_wire_number(.rls_native_row_value(row, "observed", NA_real_)),
      .rls_native_wire_number(.rls_native_row_value(row, "fitted", NA_real_)),
      .rls_native_wire_number(.rls_native_row_value(row, "residual", NA_real_)),
      .rls_native_wire_number(.rls_native_row_value(row, "standardized_residual", NA_real_)),
      .rls_native_wire_number(.rls_native_row_value(row, "studentized_residual", NA_real_)),
      .rls_native_wire_number(.rls_native_row_value(row, "leverage", NA_real_)),
      .rls_native_wire_number(.rls_native_row_value(row, "cooks_distance", NA_real_)),
      .rls_native_wire_number(.rls_native_row_value(row, "sqrt_abs_standardized_residual", NA_real_))
    )
  }
  payload
}

.rls_native_linear_design_payload <- function(record) {
  fit <- record$fit
  if (is.null(fit)) return("0")
  beta <- tryCatch(stats::coef(fit), error = function(e) numeric())
  covariance <- tryCatch(stats::vcov(fit), error = function(e) matrix(numeric(), 0L, 0L))
  labels <- tryCatch(colnames(stats::model.matrix(fit)), error = function(e) names(beta))
  n <- length(beta)
  if (!n || !is.matrix(covariance) || nrow(covariance) != n || ncol(covariance) != n) {
    return("0")
  }
  beta_payload <- vapply(beta, .rls_native_wire_number, character(1L))
  payload <- c(as.character(n), .rls_native_wire_value(labels), beta_payload)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      payload <- c(payload, .rls_native_wire_number(covariance[i, j]))
    }
  }
  payload
}

.rls_glm_native_update_payload <- function(record) {
  c(
    "MODEL_UPDATE",
    .rls_native_wire_value(record$group),
    .rls_native_wire_value(record$dependent),
    .rls_native_wire_value(record$scope %||% "all"),
    as.character(length(record$predictors %||% character())),
    .rls_native_wire_value(record$predictors %||% character()),
    .rls_native_linear_fit_payload(record),
    .rls_native_linear_diagnostics_payload(record),
    .rls_native_linear_design_payload(record)
  )
}

.rls_glm_sync_native_update <- function(record) {
  if (!isTRUE(.rls_state$process_started)) return(FALSE)
  result <- try(.rls_send(.rls_glm_native_update_payload(record)), silent = TRUE)
  is.character(result) && length(result) && startsWith(result[[1L]], "OK")
}

.rls_glm_sync_native_error <- function(group, message) {
  if (!isTRUE(.rls_state$process_started)) return(FALSE)
  result <- try(.rls_send(c(
    "MODEL_UPDATE_ERROR",
    .rls_native_wire_value(group),
    .rls_native_wire_value(message)
  )), silent = TRUE)
  is.character(result) && length(result) && startsWith(result[[1L]], "OK")
}

.rls_native_compact_factor_coefficient_rows <- function(rows, max_factor_levels = 40L) {
  if (!is.data.frame(rows) || !nrow(rows)) return(rows)
  needed <- c("row_type", "source_term", "term_type", "term", "display_label")
  if (!all(needed %in% names(rows))) return(rows)
  level_row <- (rows$row_type %in% c("reference", "factor_level")) &
    (as.character(rows$term_type %||% "") == "factor")
  if (!any(level_row)) return(rows)

  counts <- table(rows$source_term[level_row])
  compact_sources <- names(counts[counts > max_factor_levels])
  if (!length(compact_sources)) return(rows)

  kept <- stats::setNames(rep.int(0L, length(compact_sources)), compact_sources)
  omitted_added <- stats::setNames(rep.int(FALSE, length(compact_sources)), compact_sources)
  out <- vector("list", 0L)
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    source <- as.character(row$source_term[[1L]] %||% "")
    is_compact_level <- source %in% compact_sources &&
      row$row_type[[1L]] %in% c("reference", "factor_level") &&
      identical(as.character(row$term_type[[1L]]), "factor")
    if (is_compact_level) {
      if (kept[[source]] < max_factor_levels) {
        kept[[source]] <- kept[[source]] + 1L
        out[[length(out) + 1L]] <- row
      } else if (!isTRUE(omitted_added[[source]])) {
        omitted <- row
        omitted$row_type <- "term_parent"
        omitted$term <- paste0(source, "::__omitted_factor_levels__")
        omitted$display_label <- sprintf("  ... %d more levels", as.integer(counts[[source]]) - max_factor_levels)
        for (column in intersect(c("level", "reference_level"), names(omitted))) {
          omitted[[column]] <- NA_character_
        }
        for (column in intersect(c("estimate", "std_error", "statistic", "p_value", "partial_r2"), names(omitted))) {
          omitted[[column]] <- NA_real_
        }
        out[[length(out) + 1L]] <- omitted
        omitted_added[[source]] <- TRUE
      }
    } else {
      out[[length(out) + 1L]] <- row
    }
  }
  do.call(rbind, out)
}

.rls_native_generalized_coefficient_payload <- function(record) {
  rows <- record$coefficient_rows %||% data.frame()
  if (!is.data.frame(rows) || !nrow(rows)) {
    coefs <- record$coefficients %||% data.frame()
    if (is.data.frame(coefs) && nrow(coefs)) {
      rows <- data.frame(
        row_type = "coefficient",
        term = coefs$term,
        display_label = coefs$term,
        source_term = coefs$term,
        term_type = ifelse(coefs$term == "(Intercept)", "intercept", "numeric"),
        level = NA_character_,
        reference_level = NA_character_,
        estimate = coefs$estimate %||% NA_real_,
        std_error = coefs$std_error %||% NA_real_,
        statistic_name = coefs$statistic_name %||% "t",
        statistic = coefs$statistic %||% coefs$t_value %||% NA_real_,
        p_value = coefs$p_value %||% NA_real_,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }
  rows <- .rls_native_compact_factor_coefficient_rows(rows)

  payload <- as.character(if (is.data.frame(rows)) nrow(rows) else 0L)
  if (!is.data.frame(rows) || !nrow(rows)) return(payload)
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    payload <- c(
      payload,
      .rls_native_wire_value(.rls_native_row_value(row, "term")),
      .rls_native_wire_value(.rls_native_row_value(row, "source_term")),
      .rls_native_wire_value(.rls_native_row_value(row, "term_type", "numeric")),
      .rls_native_wire_value(.rls_native_row_value(row, "row_type", "coefficient")),
      .rls_native_wire_value(.rls_native_row_value(row, "display_label")),
      .rls_native_wire_value(.rls_native_row_value(row, "level")),
      .rls_native_wire_value(.rls_native_row_value(row, "reference_level")),
      .rls_native_wire_number(.rls_native_row_value(row, "estimate", NA_real_)),
      .rls_native_wire_number(.rls_native_row_value(row, "std_error", NA_real_)),
      .rls_native_wire_value(.rls_native_row_value(row, "statistic_name", "t")),
      .rls_native_wire_number(.rls_native_row_value(row, "statistic", NA_real_)),
      .rls_native_wire_number(.rls_native_row_value(row, "p_value", NA_real_))
    )
  }
  payload
}

.rls_native_generalized_state_payload <- function(record) {
  summary <- record$summary %||% record$fit_statistics %||% list()
  fit_ok <- !is.null(record$fit) || !is.null(record$fitted_glm)
  status <- record$status %||% summary$fit_information_method %||% ""
  payload <- c(
    .rls_native_wire_bool(fit_ok),
    .rls_native_wire_integer(summary$n_used %||% length(record$rows_used %||% integer())),
    .rls_native_wire_integer(summary$n_excluded %||% length(record$rows_excluded %||% integer())),
    .rls_native_wire_number(summary$null_deviance %||% NA_real_),
    .rls_native_wire_number(summary$residual_deviance %||% NA_real_),
    .rls_native_wire_integer(summary$df_residual %||% NA_real_),
    .rls_native_wire_number(summary$aic %||% NA_real_),
    .rls_native_wire_number(summary$bic %||% NA_real_),
    .rls_native_wire_number(summary$dispersion %||% NA_real_),
    .rls_native_wire_number(summary$log_lik %||% NA_real_),
    .rls_native_wire_value(if (isTRUE(record$binary_regression)) "z" else "t"),
    .rls_native_wire_value(status),
    .rls_native_int_vector_payload(record$rows_used %||% integer()),
    .rls_native_int_vector_payload(record$rows_excluded %||% integer()),
    .rls_native_generalized_coefficient_payload(record)
  )
  if (isTRUE(record$binary_regression)) {
    summary <- record$summary %||% list()
    warnings <- as.character(summary$warnings %||% record$warnings %||% character())
    rows <- record$coefficient_rows %||% data.frame()
    tests <- record$term_tests %||% data.frame()
    diagnostics <- record$diagnostics %||% data.frame()
    binary <- c(
      "BINARY_V2",
      .rls_native_wire_value(summary$event %||% record$event %||% ""),
      .rls_native_wire_value(summary$reference %||% record$reference %||% ""),
      .rls_native_wire_integer(summary$event_count %||% NA_integer_),
      .rls_native_wire_integer(summary$reference_count %||% NA_integer_),
      .rls_native_wire_integer(summary$df_model %||% NA_integer_),
      .rls_native_wire_number(summary$global_lr %||% NA_real_),
      .rls_native_wire_number(summary$global_p %||% NA_real_),
      .rls_native_wire_number(summary$mcfadden_r2 %||% NA_real_),
      .rls_native_wire_number(summary$cox_snell_r2 %||% NA_real_),
      .rls_native_wire_number(summary$nagelkerke_r2 %||% NA_real_),
      .rls_native_wire_number(summary$auc %||% NA_real_),
      .rls_native_wire_number(summary$calibration_intercept %||% NA_real_),
      .rls_native_wire_number(summary$calibration_slope %||% NA_real_),
      .rls_native_wire_bool(summary$converged %||% FALSE),
      .rls_native_wire_integer(summary$iterations %||% NA_integer_),
      .rls_native_wire_bool(summary$boundary %||% FALSE),
      .rls_native_wire_integer(summary$rank %||% NA_integer_),
      .rls_native_wire_integer(summary$parameter_count %||% NA_integer_),
      .rls_native_wire_bool(summary$rank_deficient %||% FALSE),
      as.character(length(warnings)),
      .rls_native_wire_value(warnings),
      as.character(if (is.data.frame(rows)) nrow(rows) else 0L)
    )
    if (is.data.frame(rows) && nrow(rows)) {
      for (i in seq_len(nrow(rows))) {
        row <- rows[i, , drop = FALSE]
        binary <- c(binary,
          .rls_native_wire_number(.rls_native_row_value(row, "ci_lower", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "ci_upper", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "odds_ratio", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "odds_ratio_lower", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "odds_ratio_upper", NA_real_))
        )
      }
    }
    binary <- c(binary, as.character(if (is.data.frame(tests)) nrow(tests) else 0L))
    if (is.data.frame(tests) && nrow(tests)) {
      for (i in seq_len(nrow(tests))) binary <- c(binary,
        .rls_native_wire_value(tests$term[[i]]),
        .rls_native_wire_integer(tests$df[[i]]),
        .rls_native_wire_number(tests$statistic[[i]]),
        .rls_native_wire_number(tests$p_value[[i]])
      )
    }
    binary <- c(binary, as.character(if (is.data.frame(diagnostics)) nrow(diagnostics) else 0L))
    if (is.data.frame(diagnostics) && nrow(diagnostics)) {
      for (i in seq_len(nrow(diagnostics))) {
        row <- diagnostics[i, , drop = FALSE]
        binary <- c(binary,
          .rls_native_wire_integer(.rls_native_row_value(row, "row_id", NA_integer_)),
          .rls_native_wire_value(.rls_native_row_value(row, "observed_label", "")),
          .rls_native_wire_number(.rls_native_row_value(row, "observed_binary", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "fitted", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "linear_predictor", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "deviance_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "pearson_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "working_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "leverage", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "cooks_distance", NA_real_))
        )
      }
    }
    payload <- c(payload, binary)
  }
  payload
}

.rls_generalized_glm_pooled_native_payload <- function(record) {
  imputation_count <- as.integer(record$imputation_count %||% record$multiple_imputation$m %||% 0L)
  note <- sprintf(
    "Pooled multiple-imputation generalized linear model; m = %d; link-scale coefficients use Rubin's rules. Deviance/AIC/BIC are descriptive or unavailable.",
    imputation_count
  )
  c(
    "GENERALIZED_GLM_OPEN_POOLED",
    .rls_native_wire_value(record$id),
    .rls_native_wire_value(record$group),
    .rls_native_wire_value("Generalized Linear Model - Multiple Imputation"),
    .rls_native_wire_value(record$response),
    .rls_native_wire_value(record$family),
    .rls_native_wire_value(record$link),
    .rls_native_wire_value(record$scope %||% "all"),
    as.character(imputation_count),
    .rls_native_wire_value(note),
    as.character(length(record$terms %||% character())),
    .rls_native_wire_value(record$terms %||% character()),
    .rls_native_generalized_state_payload(record)
  )
}

.rls_generalized_glm_sync_native_pooled <- function(record) {
  .rls_start_backend()
  dataset <- .rls_dataset_record(record$group)
  try(.rls_send(c(
    "REGISTER_DATASET_SILENT",
    record$group,
    .rls_variable_payload(record$data, dataset$variable_metadata),
    .rls_dataframe_payload(record$data, dataset$variable_metadata, dataset_record = dataset)
  )), silent = TRUE)
  try(.rls_send(.rls_generalized_glm_pooled_native_payload(record)), silent = TRUE)
}

.rls_regcmp_pooled_native_payload <- function(record) {
  imputation_count <- as.integer(record$imputation_count %||% record$multiple_imputation$m %||% 0L)
  title <- "Regression Model Comparison - Multiple Imputation"
  note <- sprintf(
    "Pooled multiple-imputation regression comparison; m = %d; coefficients use Rubin's rules and sequential model tests use D1 pooled Wald tests.",
    imputation_count
  )
  display_rows <- tryCatch(.rls_regcmp_display_rows(record), error = function(e) data.frame(term = record$term_rows))
  term_rows <- unique(as.character(display_rows$term %||% record$term_rows %||% "(Intercept)"))
  payload <- c(
    "REGCMP_OPEN_POOLED",
    .rls_native_wire_value(record$id),
    .rls_native_wire_value(record$group),
    .rls_native_wire_value(record$response),
    .rls_native_wire_value(record$scope %||% "all"),
    as.character(imputation_count),
    .rls_native_wire_value(title),
    .rls_native_wire_value(note),
    as.character(length(term_rows)),
    .rls_native_wire_value(term_rows),
    as.character(length(record$models %||% list()))
  )
  for (i in seq_along(record$models)) {
    model <- record$models[[i]]
    test <- model$comparison_vs_previous %||% list()
    comparison_ok <- identical(test$status %||% "", "ok") && is.finite(test$F %||% NA_real_) && is.finite(test$p %||% NA_real_)
    payload <- c(
      payload,
      .rls_native_wire_value(model$id),
      .rls_native_wire_value(model$label),
      .rls_native_wire_value(model$response %||% record$response),
      as.character(length(model$terms %||% character())),
      .rls_native_wire_value(model$terms %||% character()),
      .rls_native_linear_fit_payload(model),
      .rls_native_wire_bool(comparison_ok),
      .rls_native_wire_integer(test$df1 %||% NA_real_),
      .rls_native_wire_number(test$df2 %||% NA_real_),
      .rls_native_wire_number(NA_real_),
      .rls_native_wire_number(test$F %||% NA_real_),
      .rls_native_wire_number(test$p %||% NA_real_)
    )
  }
  payload
}

.rls_regcmp_sync_native_pooled <- function(record) {
  .rls_start_backend()
  dataset <- .rls_dataset_record(record$group)
  try(.rls_send(c(
    "REGISTER_DATASET_SILENT",
    record$group,
    .rls_variable_payload(record$data, dataset$variable_metadata),
    .rls_dataframe_payload(record$data, dataset$variable_metadata, dataset_record = dataset)
  )), silent = TRUE)
  try(.rls_send(.rls_regcmp_pooled_native_payload(record)), silent = TRUE)
}

.rls_regcmp_native_update_payload <- function(record) {
  display_rows <- tryCatch(.rls_regcmp_display_rows(record), error = function(e) data.frame(term = record$term_rows))
  term_rows <- unique(as.character(display_rows$term %||% record$term_rows %||% "(Intercept)"))
  payload <- c(
    "REGCMP_UPDATE",
    .rls_native_wire_value(record$id),
    .rls_native_wire_value(record$group),
    .rls_native_wire_value(record$response),
    .rls_native_wire_value(record$scope %||% "all"),
    .rls_native_wire_bool(record$auto_refit),
    as.character(length(term_rows)),
    .rls_native_wire_value(term_rows),
    as.character(length(record$term_types %||% list()))
  )
  if (length(record$term_types %||% list())) {
    for (term in names(record$term_types)) {
      payload <- c(payload, .rls_native_wire_value(term), .rls_native_wire_value(record$term_types[[term]]))
    }
  }
  payload <- c(payload, as.character(length(record$models %||% list())))
  for (model in record$models) {
    test <- model$comparison_vs_previous %||% list()
    comparison_ok <- identical(test$status %||% "", "ok") && is.finite(test$F %||% NA_real_) && is.finite(test$p %||% NA_real_)
    payload <- c(
      payload,
      .rls_native_wire_value(model$id),
      .rls_native_wire_value(model$label),
      .rls_native_wire_value(model$response %||% record$response),
      as.character(length(model$terms %||% character())),
      .rls_native_wire_value(model$terms %||% character()),
      .rls_native_linear_fit_payload(model),
      .rls_native_linear_diagnostics_payload(model),
      .rls_native_linear_design_payload(model),
      .rls_native_wire_bool(comparison_ok),
      .rls_native_wire_integer(test$df1 %||% NA_real_),
      .rls_native_wire_number(test$df2 %||% NA_real_),
      .rls_native_wire_number(test$delta %||% NA_real_),
      .rls_native_wire_number(test$F %||% NA_real_),
      .rls_native_wire_number(test$p %||% NA_real_)
    )
  }
  payload
}

.rls_regcmp_sync_native_update <- function(record) {
  if (!isTRUE(.rls_state$process_started)) return(FALSE)
  result <- try(.rls_send(.rls_regcmp_native_update_payload(record)), silent = TRUE)
  is.character(result) && length(result) && startsWith(result[[1L]], "OK")
}

.rls_regcmp_sync_native_error <- function(id, message) {
  if (!isTRUE(.rls_state$process_started)) return(FALSE)
  result <- try(.rls_send(c(
    "REGCMP_UPDATE_ERROR",
    .rls_native_wire_value(id),
    .rls_native_wire_value(message)
  )), silent = TRUE)
  is.character(result) && length(result) && startsWith(result[[1L]], "OK")
}
