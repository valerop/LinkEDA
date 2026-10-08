.rls_native_wire_value <- function(value) {
  value <- as.character(value %||% "")
  value[is.na(value)] <- ""

  # The Windows R session can start without a working UTF-8 locale (notably
  # when C.UTF-8 is requested but unavailable).  In that state character-mode
  # regular expressions may transcode an otherwise valid UTF-8 label through
  # the active code page and introduce an embedded NUL before it reaches the
  # native process.  The wire protocol is already UTF-8 and line based, so
  # validate/convert each value explicitly and strip line controls by bytes.
  valid_utf8 <- !is.na(suppressWarnings(
    iconv(value, from = "UTF-8", to = "UTF-8", sub = NA_character_)
  ))
  if (any(valid_utf8)) {
    value[valid_utf8] <- iconv(
      value[valid_utf8], from = "UTF-8", to = "UTF-8", sub = "?"
    )
  }
  if (any(!valid_utf8)) {
    value[!valid_utf8] <- iconv(
      value[!valid_utf8], from = "", to = "UTF-8", sub = "?"
    )
  }
  value[is.na(value)] <- ""
  Encoding(value) <- "UTF-8"
  gsub("[\r\n\t]+", " ", value, useBytes = TRUE)
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
    partial_r = coefs$partial_r %||% NA_real_,
    delta_r2 = coefs$delta_r2 %||% NA_real_,
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
      .rls_native_wire_number(.rls_native_row_value(row, "partial_r", NA_real_)),
      .rls_native_wire_number(.rls_native_row_value(row, "delta_r2", NA_real_))
    )
  }
  payload
}

.rls_native_linear_fit_payload <- function(record) {
  summary <- record$summary %||% list()
  rows <- .rls_native_linear_coefficient_rows(record)
  fit_ok <- !is.null(record$fit) || (is.data.frame(rows) && nrow(rows) > 0L)
  imputation_count <- as.integer(
    record$imputation_count %||% record$multiple_imputation$m %||%
      length(record$fits_by_imputation %||% list())
  )
  note <- if (isTRUE(imputation_count > 1L)) {
    paste0(
      "Multiple imputation: R-squared is pooled; s and delta R-squared are descriptive; ",
      "partial r uses Fisher-z pooling. Global test: ",
      summary$global_test_method %||% "mice::D1",
      ". AIC/BIC are unavailable."
    )
  } else {
    summary$fit_information_method %||% record$status %||% ""
  }
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
    "Pooled multiple-imputation linear model; m = %d; coefficients use mice::pool, R-squared uses mice::pool.r.squared, and the global model test uses %s; AIC/BIC are not pooled.",
    imputation_count,
    record$summary$global_test_method %||% "mice::D1"
  )
  c(
    "MODEL_OPEN_POOLED",
    .rls_native_wire_value(record$group),
    .rls_native_wire_value("Linear Model \u2014 Multiple Imputation"),
    .rls_native_wire_value(record$dependent),
    .rls_native_wire_value(record$scope %||% "all"),
    as.character(imputation_count),
    .rls_native_wire_value(paste(.rls_mi_status_note(record),note)),
    as.character(length(record$predictors %||% character())),
    .rls_native_wire_value(record$predictors %||% character()),
    .rls_native_linear_fit_payload(record),
    .rls_native_linear_diagnostics_payload(record),
    .rls_native_linear_design_payload(record),
    .rls_native_linear_mi_diagnostics_payload(record),
    "LINEAR_SCOPE_ROWS_V1",
    .rls_native_int_vector_payload(record$selected_rows %||% integer()),
    .rls_analysis_provenance_payload(record)
  )
}

.rls_glm_sync_native_pooled <- function(record) {
  .rls_start_backend()
  dataset <- .rls_dataset_record(record$group)
  .rls_register_native_dataset_if_needed(dataset, record$data)
  try(.rls_send(.rls_glm_pooled_native_payload(record)), silent = TRUE)
}

.rls_native_qq_by_row <- function(values) {
  values <- suppressWarnings(as.numeric(values))
  result <- rep(NA_real_, length(values))
  finite <- which(is.finite(values))
  if (length(finite) < 2L) return(result)
  sorted <- finite[order(values[finite], finite)]
  result[sorted] <- stats::qnorm((seq_along(sorted) - 0.5) / length(sorted))
  result
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
  qq_columns <- c("residual", "standardized_residual", "studentized_residual")
  qq <- lapply(qq_columns, function(column) {
    .rls_native_qq_by_row(if (column %in% names(diagnostics))
      diagnostics[[column]] else rep(NA_real_, nrow(diagnostics)))
  })
  payload <- c(payload, "LINEAR_QQ_V1")
  for (i in seq_len(nrow(diagnostics))) {
    payload <- c(payload, paste(vapply(qq, function(values)
      .rls_native_wire_number(values[[i]]), character(1L)), collapse = "\x1f"))
  }
  payload
}

.rls_native_generalized_qq_payload <- function(record) {
  columns <- c("dunn_smyth_residual", "raw_residual", "deviance_residual",
    "pearson_residual", "working_residual", "standardized_residual",
    "studentized_residual")
  encode <- function(diagnostics) {
    if (!is.data.frame(diagnostics) || !nrow(diagnostics)) return("0")
    count <- nrow(diagnostics)
    qq <- lapply(columns, function(column) {
      .rls_native_qq_by_row(if (column %in% names(diagnostics))
        diagnostics[[column]] else rep(NA_real_, count))
    })
    c(as.character(count), vapply(seq_len(count), function(i)
      paste(vapply(qq, function(values)
        .rls_native_wire_number(values[[i]]), character(1L)), collapse = "\x1f"),
      character(1L)))
  }
  by_imputation <- record$diagnostics_by_imputation %||% list()
  c("GENERALIZED_QQ_V1", encode(record$diagnostics %||% data.frame()),
    as.character(length(by_imputation)),
    unlist(lapply(by_imputation, encode), use.names = FALSE))
}

.rls_native_linear_mi_diagnostics_payload <- function(record) {
  diagnostics_by_imp <- record$diagnostics_by_imputation
  if (!is.list(diagnostics_by_imp) || !length(diagnostics_by_imp)) return(character())
  payload <- c("LINEAR_MI_DIAGNOSTICS_V1", as.character(length(diagnostics_by_imp)))
  for (diagnostics in diagnostics_by_imp) {
    payload <- c(payload, .rls_native_linear_diagnostics_payload(list(
      diagnostics = if (is.data.frame(diagnostics)) diagnostics else data.frame()
    )))
  }
  payload
}

.rls_native_linear_design_payload <- function(record) {
  fit <- record$fit
  if (is.null(fit)) return(c("0", "FACTOR_CODINGS", "0", "PREDICTOR_CENTERS", "0"))
  beta <- tryCatch(stats::coef(fit), error = function(e) numeric())
  covariance <- tryCatch(stats::vcov(fit), error = function(e) matrix(numeric(), 0L, 0L))
  labels <- tryCatch(colnames(stats::model.matrix(fit)), error = function(e) names(beta))
  n <- length(beta)
  if (!n || !is.matrix(covariance) || nrow(covariance) != n || ncol(covariance) != n) {
    return(c("0", "FACTOR_CODINGS", "0", "PREDICTOR_CENTERS", "0"))
  }
  beta_payload <- vapply(beta, .rls_native_wire_number, character(1L))
  payload <- c(as.character(n), .rls_native_wire_value(labels), beta_payload)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      payload <- c(payload, .rls_native_wire_number(covariance[i, j]))
    }
  }
  factor_codings <- .rls_model_factor_codings(fit)
  payload <- c(payload, "FACTOR_CODINGS", as.character(length(factor_codings)))
  for (coding in factor_codings) {
    matrix_values <- as.vector(t(coding$coding))
    payload <- c(
      payload,
      .rls_native_wire_value(coding$variable),
      .rls_native_wire_value(if (is.na(coding$reference)) "" else coding$reference),
      as.character(length(coding$levels)),
      as.character(ncol(coding$coding)),
      .rls_native_wire_value(coding$levels),
      .rls_native_wire_value(coding$contrast_labels),
      vapply(matrix_values, .rls_native_wire_number, character(1L))
    )
  }
  centers <- record$predictor_centers %||% numeric()
  centers <- centers[is.finite(suppressWarnings(as.numeric(centers)))]
  payload <- c(payload, "PREDICTOR_CENTERS", as.character(length(centers)))
  if (length(centers)) {
    for (variable in names(centers)) {
      payload <- c(payload, .rls_native_wire_value(variable),
                   .rls_native_wire_number(centers[[variable]]))
    }
  }
  payload
}

.rls_glm_native_update_payload <- function(record, request_identity = "", model_id = record$group) {
  payload <- c(
    "MODEL_UPDATE",
    .rls_native_wire_value(record$group),
    .rls_native_wire_value(record$dependent),
    .rls_native_wire_value(record$scope %||% "all"),
    as.character(length(record$predictors %||% character())),
    .rls_native_wire_value(record$predictors %||% character()),
    .rls_native_linear_fit_payload(record),
    .rls_native_linear_diagnostics_payload(record),
    .rls_native_linear_design_payload(record),
    .rls_native_linear_mi_diagnostics_payload(record)
  )
  if (length(request_identity) && !is.na(request_identity[[1L]]) &&
      nzchar(request_identity[[1L]])) {
    payload <- c(payload, "LINEAR_RESULT_V1",
                 .rls_native_wire_value(request_identity[[1L]]))
  }
  if (!identical(model_id, record$group)) {
    payload <- c(payload, "LINEAR_MODEL_ID_V1", .rls_native_wire_value(model_id))
  }
  payload <- c(payload, .rls_analysis_provenance_payload(record))
  payload
}

.rls_glm_sync_native_update <- function(record, request_identity = "", model_id = record$group) {
  if (!isTRUE(.rls_state$process_started)) return(FALSE)
  result <- try(.rls_send(.rls_glm_native_update_payload(
    record, request_identity, model_id)), silent = TRUE)
  is.character(result) && length(result) && startsWith(result[[1L]], "OK")
}

.rls_glm_sync_native_error <- function(group, message, request_identity = "", model_id = group) {
  if (!isTRUE(.rls_state$process_started)) return(FALSE)
  payload <- c(
    "MODEL_UPDATE_ERROR",
    .rls_native_wire_value(group),
    .rls_native_wire_value(message)
  )
  if (length(request_identity) && !is.na(request_identity[[1L]]) &&
      nzchar(request_identity[[1L]])) {
    payload <- c(payload, "LINEAR_RESULT_V1",
                 .rls_native_wire_value(request_identity[[1L]]))
  }
  if (!identical(model_id, group)) {
    payload <- c(payload, "LINEAR_MODEL_ID_V1", .rls_native_wire_value(model_id))
  }
  result <- try(.rls_send(payload), silent = TRUE)
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
        omitted$display_label <- sprintf("  ... %d more categories", as.integer(counts[[source]]) - max_factor_levels)
        for (column in intersect(c("level", "reference_level"), names(omitted))) {
          omitted[[column]] <- NA_character_
        }
        for (column in intersect(c("estimate", "std_error", "statistic", "p_value", "ci_lower", "ci_upper", "partial_r", "delta_r2"), names(omitted))) {
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

.rls_native_generalized_coefficient_rows <- function(record) {
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
        ci_lower = coefs$ci_lower %||% NA_real_,
        ci_upper = coefs$ci_upper %||% NA_real_,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }
  .rls_native_compact_factor_coefficient_rows(rows)
}

.rls_native_generalized_coefficient_payload <- function(record) {
  rows <- .rls_native_generalized_coefficient_rows(record)

  payload <- c("GENERALIZED_ROWS_V2", as.character(if (is.data.frame(rows)) nrow(rows) else 0L))
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
      .rls_native_wire_number(.rls_native_row_value(row, "p_value", NA_real_)),
      .rls_native_wire_value(.rls_native_row_value(row, "component", ""))
    )
  }
  payload
}

.rls_native_generalized_mi_diagnostics_payload <- function(record) {
  diagnostics_by_imp <- record$diagnostics_by_imputation
  if (!is.list(diagnostics_by_imp) || !length(diagnostics_by_imp)) return(character())
  number_column <- function(diagnostics, name, count) {
    values <- if (name %in% names(diagnostics))
      suppressWarnings(as.numeric(diagnostics[[name]])) else rep(NA_real_, count)
    out <- rep("NA", count)
    finite <- is.finite(values)
    out[finite] <- sprintf("%.17g", values[finite])
    out[is.infinite(values) & values > 0] <- "Inf"
    out[is.infinite(values) & values < 0] <- "-Inf"
    out
  }
  integer_column <- function(diagnostics, name, count) {
    values <- if (name %in% names(diagnostics))
      suppressWarnings(as.numeric(diagnostics[[name]])) else rep(NA_real_, count)
    out <- rep("NA", count)
    finite <- is.finite(values)
    out[finite] <- as.character(as.integer(round(values[finite])))
    out
  }
  payload_parts <- vector("list", length(diagnostics_by_imp) + 1L)
  payload_parts[[1L]] <- c("GENERALIZED_MI_DIAGNOSTICS_V4",
                          as.character(length(diagnostics_by_imp)))
  for (imp in seq_along(diagnostics_by_imp)) {
    diagnostics <- diagnostics_by_imp[[imp]]
    count <- if (is.data.frame(diagnostics)) nrow(diagnostics) else 0L
    if (!count) {
      payload_parts[[imp + 1L]] <- "0"
      next
    }
    labels <- if ("observed_label" %in% names(diagnostics))
      .rls_native_wire_value(diagnostics$observed_label) else rep("", count)
    labels <- gsub("\x1f", " ", labels, fixed = TRUE)
    columns <- c(list(integer_column(diagnostics, "row_id", count), labels),
      lapply(c("observed", "observed_binary", "fitted", "linear_predictor",
        "dunn_smyth_residual", "raw_residual", "deviance_residual",
        "pearson_residual", "working_residual", "standardized_residual",
        "studentized_residual", "leverage", "cooks_distance"),
        function(name) number_column(diagnostics, name, count)))
    payload_parts[[imp + 1L]] <- c(as.character(count),
      do.call(paste, c(columns, sep = "\x1f")))
  }
  unlist(payload_parts, use.names = FALSE)
}

.rls_native_generalized_state_payload <- function(record) {
  summary <- record$summary %||% record$fit_statistics %||% list()
  fit_ok <- !is.null(record$fit) || !is.null(record$fitted_glm)
  status <- record$status %||% summary$fit_information_method %||% ""
  beta_binomial_precision <- summary$beta_binomial_precision %||% NA_real_
  if (!length(beta_binomial_precision) ||
      !is.finite(beta_binomial_precision[[1L]])) {
    beta_binomial_precision <- summary$dispersion %||% NA_real_
  }
  payload <- c(
    .rls_native_wire_bool(fit_ok),
    .rls_native_wire_integer(summary$n_used %||% length(record$rows_used %||% integer())),
    .rls_native_wire_integer(summary$n_excluded %||% length(record$rows_excluded %||% integer())),
    .rls_native_wire_number(summary$null_deviance %||% NA_real_),
    .rls_native_wire_number(summary$residual_deviance %||% NA_real_),
    .rls_native_wire_integer(summary$df_residual %||% NA_real_),
    .rls_native_wire_number(summary$aic %||% NA_real_),
    .rls_native_wire_number(summary$bic %||% NA_real_),
    .rls_native_wire_number(
      if (identical(record$count_distribution %||% "", "binomial_trials"))
        summary$binomial_overdispersion_ratio %||% NA_real_
      else if (identical(record$count_distribution %||% "", "beta_binomial"))
        beta_binomial_precision
      else summary$dispersion %||% NA_real_
    ),
    .rls_native_wire_number(summary$log_lik %||% NA_real_),
    .rls_native_wire_value(summary$statistic_name %||% if (isTRUE(record$binary_regression)) "z" else "t"),
    .rls_native_wire_value(status),
    .rls_native_int_vector_payload(record$rows_used %||% integer()),
    .rls_native_int_vector_payload(record$rows_excluded %||% integer()),
    .rls_native_generalized_coefficient_payload(record)
  )
  rows <- .rls_native_generalized_coefficient_rows(record)
  payload <- c(
    payload,
    "GENERALIZED_CI_V1",
    as.character(if (is.data.frame(rows)) nrow(rows) else 0L)
  )
  if (is.data.frame(rows) && nrow(rows)) {
    for (i in seq_len(nrow(rows))) {
      row <- rows[i, , drop = FALSE]
      payload <- c(
        payload,
        .rls_native_wire_number(.rls_native_row_value(row, "ci_lower", NA_real_)),
        .rls_native_wire_number(.rls_native_row_value(row, "ci_upper", NA_real_))
      )
    }
  }
  payload <- c(
    payload,
    "GENERALIZED_EXPONENTIATED_V1",
    as.character(if (is.data.frame(rows)) nrow(rows) else 0L)
  )
  if (is.data.frame(rows) && nrow(rows)) {
    for (i in seq_len(nrow(rows))) {
      row <- rows[i, , drop = FALSE]
      payload <- c(
        payload,
        .rls_native_wire_number(.rls_native_row_value(row, "exponentiated_estimate", NA_real_)),
        .rls_native_wire_number(.rls_native_row_value(row, "exponentiated_lower", NA_real_)),
        .rls_native_wire_number(.rls_native_row_value(row, "exponentiated_upper", NA_real_))
      )
    }
  }
  warnings <- as.character(summary$warnings %||% record$warnings %||% character())
  converged <- summary$converged
  if (!length(converged) || is.na(converged[[1L]])) converged <- fit_ok
  payload <- c(
    payload,
    "GENERALIZED_META_V1",
    .rls_native_wire_bool(converged),
    .rls_native_wire_integer(summary$iterations %||% NA_integer_),
    .rls_native_wire_bool(summary$boundary %||% FALSE),
    .rls_native_wire_integer(summary$rank %||% NA_integer_),
    .rls_native_wire_integer(summary$parameter_count %||% NA_integer_),
    .rls_native_wire_bool(summary$rank_deficient %||% FALSE),
    as.character(length(warnings)),
    .rls_native_wire_value(warnings)
  )
  if (isTRUE(record$count_regression)) {
    rows <- .rls_native_generalized_coefficient_rows(record)
    diagnostics <- record$diagnostics %||% data.frame()
    count <- c(
      "COUNT_V7",
      .rls_native_wire_value(record$count_distribution %||% "poisson"),
      .rls_native_wire_value(record$exposure %||% ""),
      .rls_native_wire_value(record$trials_variable %||% ""),
      .rls_native_wire_number(record$trials_constant %||% NA_real_),
      .rls_native_wire_bool(summary$likelihood_available %||% FALSE),
      .rls_native_wire_bool(summary$likelihood_ratio_available %||% FALSE),
      .rls_native_wire_number(summary$theta %||% NA_real_),
      .rls_native_wire_number(summary$theta_descriptive_mean %||% NA_real_),
      .rls_native_wire_number(summary$theta_descriptive_min %||% NA_real_),
      .rls_native_wire_number(summary$theta_descriptive_max %||% NA_real_),
      .rls_native_wire_number(summary$perfect_scores %||% NA_real_),
      .rls_native_wire_number(summary$non_perfect_scores %||% NA_real_),
      .rls_native_wire_number(summary$perfect_score_proportion %||% NA_real_),
      .rls_native_wire_number(summary$predicted_perfect_score_proportion %||% NA_real_),
      .rls_native_wire_number(summary$predicted_perfect_score_count %||% NA_real_),
      .rls_native_wire_bool(summary$observed_ceiling_count_constant %||% TRUE),
      .rls_native_wire_value(summary$perfect_score_component_status %||% ""),
      .rls_native_wire_value(summary$below_ceiling_component_status %||% ""),
      .rls_native_wire_integer(summary$hurdle_successful_imputations %||% NA_integer_),
      .rls_native_wire_integer(summary$hurdle_attempted_imputations %||% NA_integer_),
      .rls_native_wire_number(
        summary$beta_binomial_dispersion %||%
          if (identical(record$count_distribution %||% "", "hurdle_beta_binomial_ceiling"))
            summary$dispersion %||% NA_real_ else NA_real_
      ),
      .rls_native_wire_number({
        values <- summary$beta_binomial_dispersion_by_imputation %||% numeric()
        values <- values[is.finite(values)]
        if (length(values)) min(values) else NA_real_
      }),
      .rls_native_wire_number({
        values <- summary$beta_binomial_dispersion_by_imputation %||% numeric()
        values <- values[is.finite(values)]
        if (length(values)) max(values) else NA_real_
      }),
      as.character(if (is.data.frame(rows)) nrow(rows) else 0L)
    )
    if (is.data.frame(rows) && nrow(rows)) {
      for (i in seq_len(nrow(rows))) {
        row <- rows[i, , drop = FALSE]
        count <- c(count,
          .rls_native_wire_number(.rls_native_row_value(row, "exponentiated_estimate", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "exponentiated_lower", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "exponentiated_upper", NA_real_))
        )
      }
    }
    count <- c(count, as.character(if (is.data.frame(diagnostics)) nrow(diagnostics) else 0L))
    if (is.data.frame(diagnostics) && nrow(diagnostics)) {
      for (i in seq_len(nrow(diagnostics))) {
        row <- diagnostics[i, , drop = FALSE]
        count <- c(count,
          .rls_native_wire_integer(.rls_native_row_value(row, "row_id", NA_integer_)),
          .rls_native_wire_number(.rls_native_row_value(row, "observed", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(
            row,
            if (.rls_count_distribution_uses_trials(record$count_distribution))
              "fitted_response_scale" else "fitted",
            NA_real_
          )),
          .rls_native_wire_number(.rls_native_row_value(row, "linear_predictor", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "dunn_smyth_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "raw_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "deviance_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "pearson_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "working_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "standardized_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "studentized_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "leverage", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "cooks_distance", NA_real_))
        )
      }
    }
    payload <- c(payload, count)
  }
  if (isTRUE(record$binary_regression)) {
    summary <- record$summary %||% list()
    warnings <- as.character(summary$warnings %||% record$warnings %||% character())
    rows <- .rls_native_generalized_coefficient_rows(record)
    tests <- record$term_tests %||% data.frame()
    diagnostics <- record$diagnostics %||% data.frame()
    binary <- c(
      "BINARY_V4",
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
        .rls_native_wire_integer(tests$df1[[i]] %||% tests$term_test_df[[i]]),
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
          .rls_native_wire_number(.rls_native_row_value(row, "dunn_smyth_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "raw_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "deviance_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "pearson_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "working_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "standardized_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "studentized_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "leverage", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "cooks_distance", NA_real_))
        )
      }
    }
    payload <- c(payload, binary)
  }
  if (!isTRUE(record$count_regression) && !isTRUE(record$binary_regression)) {
    diagnostics <- record$diagnostics %||% data.frame()
    ordinary_diagnostics <- c(
      "GENERALIZED_DIAGNOSTICS_V2",
      as.character(if (is.data.frame(diagnostics)) nrow(diagnostics) else 0L)
    )
    if (is.data.frame(diagnostics) && nrow(diagnostics)) {
      for (i in seq_len(nrow(diagnostics))) {
        row <- diagnostics[i, , drop = FALSE]
        ordinary_diagnostics <- c(ordinary_diagnostics,
          .rls_native_wire_integer(.rls_native_row_value(row, "row_id", NA_integer_)),
          .rls_native_wire_number(.rls_native_row_value(row, "observed", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "fitted", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "linear_predictor", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "dunn_smyth_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "raw_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "deviance_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "pearson_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "working_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "standardized_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "studentized_residual", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "leverage", NA_real_)),
          .rls_native_wire_number(.rls_native_row_value(row, "cooks_distance", NA_real_))
        )
      }
    }
    payload <- c(payload, ordinary_diagnostics)
  }
  c(payload, .rls_native_global_term_tests_payload(record),
    .rls_native_generalized_mi_diagnostics_payload(record),
    .rls_native_bounded_count_distribution_payload(record),
    .rls_native_diagnostic_notes_payload(record),
    "GENERALIZED_FAMILY_DIAGNOSTICS_V1",
    as.character(length(summary$family_diagnostics %||% list())),
    unlist(lapply(names(summary$family_diagnostics %||% list()), function(key) c(
      .rls_native_wire_value(key),
      .rls_native_wire_number(summary$family_diagnostics[[key]])
    )), use.names = FALSE),
    .rls_native_generalized_qq_payload(record),
    "MODEL_TYPE_V1",
    .rls_native_wire_value(record$model_type %||% .rls_model_type_for_family(
      record$family, record$binary_regression, record$count_regression)))
}

.rls_native_bounded_count_distribution_payload <- function(record) {
  capabilities <- .rls_discrete_diagnostic_capabilities(record)
  if (!isTRUE(capabilities$full_probability_distribution)) {
    return(character())
  }
  encode <- function(value) {
    if (!is.data.frame(value) || !nrow(value) ||
        !all(c("score", "observed_frequency", "predicted_frequency") %in%
             names(value))) {
      return("0")
    }
    result <- as.character(nrow(value))
    for (i in seq_len(nrow(value))) {
      result <- c(
        result,
        .rls_native_wire_number(value$score[[i]]),
        .rls_native_wire_number(value$observed_frequency[[i]]),
        .rls_native_wire_number(value$predicted_frequency[[i]])
      )
    }
    result
  }
  distributions <- record$summary$observed_predicted_distribution_by_imputation %||%
    list()
  payload <- c(
    "DISCRETE_RESPONSE_DISTRIBUTION_V1",
    encode(record$summary$observed_predicted_distribution %||% data.frame()),
    as.character(length(distributions))
  )
  for (value in distributions) payload <- c(payload, encode(value))
  payload
}

.rls_native_global_term_tests_payload <- function(record) {
  tests <- .rls_as_global_term_tests(record$term_tests %||% data.frame())
  payload <- c("GLOBAL_TERM_TESTS_V2", as.character(nrow(tests)))
  if (!nrow(tests)) return(payload)
  for (i in seq_len(nrow(tests))) {
    payload <- c(
      payload,
      .rls_native_wire_value(tests$term[[i]]),
      .rls_native_wire_value(tests$coefficient_names[[i]]),
      .rls_native_wire_value(tests$term_test_method[[i]]),
      .rls_native_wire_number(tests$term_test_statistic[[i]]),
      .rls_native_wire_number(tests$term_test_df[[i]]),
      .rls_native_wire_number(tests$term_test_df2[[i]]),
      .rls_native_wire_number(tests$term_test_p[[i]]),
      .rls_native_wire_value(if ("component" %in% names(tests)) tests$component[[i]] else "")
    )
  }
  payload
}

.rls_generalized_glm_pooled_note <- function(record, imputation_count) {
  distribution <- record$count_distribution %||% ""
  if (identical(distribution, "hurdle_beta_binomial_ceiling")) {
    opening <- sprintf(paste(
      "Multiple imputation: m = %d; logistic and truncated beta-binomial",
      "coefficients/tests are pooled separately."
    ), imputation_count)
    detail <- "Dispersion and ceiling summaries are descriptive across imputations."
  } else {
    opening <- sprintf(
      "Multiple imputation: m = %d; link-scale coefficients use mice::pool (Rubin's rules).",
      imputation_count
    )
    detail <- if (identical(distribution, "negative_binomial")) {
      "Deviance and theta are descriptive means across imputations; theta is not Rubin-pooled."
    } else if (identical(distribution, "beta_binomial")) {
      "Beta-binomial precision and variance diagnostics are descriptive means across imputations."
    } else {
      "Deviance and estimated dispersion, when available, are descriptive across imputations."
    }
  }
  missing_note <- if (nrow(.rls_mi_missing_information(record)))
    "Missing-information diagnostics are available in the Multiple imputation options."
  else character()
  paste(c(opening, missing_note, detail), collapse = " ")
}

.rls_generalized_glm_pooled_native_payload <- function(record) {
  imputation_count <- as.integer(record$imputation_count %||% record$multiple_imputation$m %||% 0L)
  title <- .rls_model_window_title(
    record$model_type %||% .rls_model_type_for_family(
      record$family, record$binary_regression, record$count_regression
    ), TRUE
  )
  term_types <- record$term_types %||% list()
  type_payload <- as.character(length(term_types))
  if (length(term_types)) {
    for (term in names(term_types)) {
      type_payload <- c(type_payload,
        .rls_native_wire_value(term),
        .rls_native_wire_value(term_types[[term]]))
    }
  }
  centered <- unique(as.character(record$centered_predictors %||% character()))
  references <- record$factor_reference_levels %||% list()
  reference_payload <- as.character(length(references))
  if (length(references)) {
    for (term in names(references)) {
      reference_payload <- c(reference_payload,
        .rls_native_wire_value(term),
        .rls_native_wire_value(references[[term]]))
    }
  }
  note <- .rls_generalized_glm_pooled_note(record, imputation_count)
  data_scope <- record$data_scope %||% list()
  scope_kind <- as.character(data_scope$kind %||%
    if (identical(record$scope, "all")) "all" else "explicit")[[1L]]
  scope_rows <- as.integer(data_scope$rows %||% if (identical(record$scope, "selected")) {
    record$selected_rows %||% integer()
  } else if (identical(record$scope, "unselected")) {
    setdiff(seq_len(nrow(record$data)), record$selected_rows %||% integer())
  } else integer())
  scope_rows <- unique(scope_rows[
    is.finite(scope_rows) & scope_rows >= 1L & scope_rows <= nrow(record$data)
  ])
  scope_source_kind <- as.character(data_scope$source_kind %||%
    if (identical(scope_kind, "all")) "all_data" else "other_explicit_subset")[[1L]]
  scope_description <- as.character(data_scope$description %||%
    if (identical(scope_kind, "all")) "All observations" else "Explicit subset")[[1L]]
  scope_payload <- c(
    "ANALYSIS_SCOPE_V1",
    .rls_native_wire_value(scope_kind),
    .rls_native_wire_value(scope_source_kind),
    .rls_native_wire_value(scope_description),
    as.character(nrow(record$data)),
    as.character(length(scope_rows)),
    as.character(scope_rows)
  )
  bounds <- suppressWarnings(as.numeric(record$response_bounds %||% numeric()))
  bounds_payload <- if (length(bounds) == 2L && all(is.finite(bounds)) && bounds[[1L]] < bounds[[2L]]) {
    c(
      "BOUNDED_RESPONSE_V1",
      .rls_native_wire_number(bounds[[1L]]),
      .rls_native_wire_number(bounds[[2L]])
    )
  } else {
    character()
  }
  c(
    "GENERALIZED_GLM_OPEN_POOLED",
    .rls_native_wire_value(record$id),
    .rls_native_wire_value(record$group),
    .rls_native_wire_value(title),
    .rls_native_wire_value(record$response),
    .rls_native_wire_value(record$family),
    .rls_native_wire_value(record$link),
    .rls_native_wire_value(record$scope %||% "all"),
    as.character(imputation_count),
    .rls_native_wire_value(note),
    as.character(length(record$terms %||% character())),
    .rls_native_wire_value(record$terms %||% character()),
    "MODEL_SPEC_V1",
    type_payload,
    as.character(length(centered)),
    .rls_native_wire_value(centered),
    reference_payload,
    scope_payload,
    bounds_payload,
    "OFFSET_SPEC_V1", .rls_native_wire_value(record$offset_variable %||% ""),
    .rls_native_generalized_state_payload(record),
    "GGLM_RESULT_V1",
    .rls_native_wire_integer(record$native_generation %||% 0L),
    .rls_analysis_provenance_payload(record)
  )
}

.rls_generalized_glm_sync_native_pooled <- function(record) {
  started <- proc.time()[["elapsed"]]
  .rls_start_backend()
  backend_ready <- proc.time()[["elapsed"]]
  dataset <- .rls_dataset_record(record$group)
  dataset_current <- .rls_native_dataset_matches(dataset, record$data)
  dataset_payload <- if (!dataset_current) c(
    "REGISTER_DATASET_SILENT", record$group,
    .rls_variable_payload(record$data, dataset$variable_metadata),
    .rls_dataframe_payload(record$data, dataset$variable_metadata, dataset_record = dataset)
  ) else NULL
  dataset_prepared <- proc.time()[["elapsed"]]
  if (!dataset_current) .rls_send(dataset_payload)
  dataset_received <- proc.time()[["elapsed"]]
  model_payload <- .rls_generalized_glm_pooled_native_payload(record)
  model_prepared <- proc.time()[["elapsed"]]
  .rls_send(model_payload)
  model_received <- proc.time()[["elapsed"]]
  invisible(list(
    total_seconds = unname(model_received - started),
    backend_startup_seconds = unname(backend_ready - started),
    dataset_preparation_seconds = unname(dataset_prepared - backend_ready),
    dataset_delivery_seconds = unname(dataset_received - dataset_prepared),
    model_preparation_seconds = unname(model_prepared - dataset_received),
    model_delivery_seconds = unname(model_received - model_prepared)
  ))
}

.rls_regcmp_pooled_native_payload <- function(record) {
  if (is.null(record$analysis_provenance)) {
    record <- .rls_attach_analysis_provenance(
      record, .rls_regression_comparison_executed_r_code(record),
      "Compare Linear Models",
      list(comparison = "comparison_table <- comparisons",
           models = "model_summaries <- lapply(models, summary)")
    )
  }
  imputation_count <- as.integer(record$imputation_count %||% record$multiple_imputation$m %||% 0L)
  title <- "Compare Linear Models \u2014 Multiple Imputation"
  note <- if (isTRUE(record$mi_pooling_required)) {
    sprintf(
      "Pooled multiple-imputation General Linear Model comparison; m = %d; coefficients use mice::pool (Rubin's rules) and sequential nested-model tests use %s.",
      imputation_count,
      record$model_comparison_tests$method %||% "mice::D1"
    )
  } else {
    sprintf(
      "Multiple-imputation dataset (m = %d), but no response or predictor value used by these models was imputed; Rubin's rules were not required or applied, and ordinary nested F tests are identical across imputations.",
      imputation_count
    )
  }
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
      as.character(length(model$term_types %||% record$term_types %||% list()))
    )
    model_term_types <- model$term_types %||% record$term_types %||% list()
    if (length(model_term_types)) {
      for (term in names(model_term_types)) {
        payload <- c(payload, .rls_native_wire_value(term), .rls_native_wire_value(model_term_types[[term]]))
      }
    }
    payload <- c(
      payload,
      as.character(length(model$centered_predictors %||% character())),
      .rls_native_wire_value(model$centered_predictors %||% character()),
      .rls_native_linear_fit_payload(model),
      .rls_native_linear_diagnostics_payload(model),
      .rls_native_linear_design_payload(model),
      .rls_native_linear_mi_diagnostics_payload(model),
      .rls_native_wire_bool(comparison_ok),
      .rls_native_wire_integer(test$df1 %||% NA_real_),
      .rls_native_wire_number(test$df2 %||% NA_real_),
      .rls_native_wire_number(NA_real_),
      .rls_native_wire_number(test$F %||% NA_real_),
      .rls_native_wire_number(test$p %||% NA_real_)
    )
  }
  c(payload, .rls_analysis_provenance_payload(record))
}

.rls_regcmp_sync_native_pooled <- function(record) {
  .rls_start_backend()
  dataset <- .rls_dataset_record(record$group)
  .rls_register_native_dataset_if_needed(dataset, record$data)
  try(.rls_send(.rls_regcmp_pooled_native_payload(record)), silent = TRUE)
}

.rls_regcmp_native_update_payload <- function(record) {
  if (is.null(record$analysis_provenance)) {
    record <- .rls_attach_analysis_provenance(
      record, .rls_regression_comparison_executed_r_code(record),
      "Compare Linear Models",
      list(comparison = "comparison_table <- comparisons",
           models = "model_summaries <- lapply(models, summary)")
    )
  }
  display_rows <- tryCatch(.rls_regcmp_display_rows(record), error = function(e) data.frame(term = record$term_rows))
  term_rows <- unique(as.character(display_rows$term %||% record$term_rows %||% "(Intercept)"))
  payload <- c(
    "REGCMP_UPDATE",
    .rls_native_wire_value(record$id),
    .rls_native_wire_integer(record$request_generation %||% 0L),
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
      as.character(length(model$term_types %||% record$term_types %||% list()))
    )
    model_term_types <- model$term_types %||% record$term_types %||% list()
    if (length(model_term_types)) {
      for (term in names(model_term_types)) {
        payload <- c(payload, .rls_native_wire_value(term), .rls_native_wire_value(model_term_types[[term]]))
      }
    }
    payload <- c(
      payload,
      as.character(length(model$centered_predictors %||% character())),
      .rls_native_wire_value(model$centered_predictors %||% character()),
      .rls_native_linear_fit_payload(model),
      .rls_native_linear_diagnostics_payload(model),
      .rls_native_linear_design_payload(model),
      .rls_native_linear_mi_diagnostics_payload(model),
      .rls_native_wire_bool(comparison_ok),
      .rls_native_wire_integer(test$df1 %||% NA_real_),
      .rls_native_wire_number(test$df2 %||% NA_real_),
      .rls_native_wire_number(test$delta %||% NA_real_),
      .rls_native_wire_number(test$F %||% NA_real_),
      .rls_native_wire_number(test$p %||% NA_real_)
    )
  }
  c(payload, .rls_analysis_provenance_payload(record))
}

.rls_regcmp_sync_native_update <- function(record) {
  if (!isTRUE(.rls_state$process_started)) return(FALSE)
  .rls_send_native_analysis_result(
    .rls_regcmp_native_update_payload(record),
    "regression-comparison result"
  )
  TRUE
}

.rls_regcmp_sync_native_error <- function(id, request_generation, message) {
  if (!isTRUE(.rls_state$process_started)) return(FALSE)
  .rls_send_native_analysis_result(c(
    "REGCMP_UPDATE_ERROR",
    .rls_native_wire_value(id),
    .rls_native_wire_integer(request_generation %||% 0L),
    .rls_native_wire_value(message)
  ), "regression-comparison error")
  TRUE
}

# Dataset-cell transport is lossless for protocol separators. Display formatting
# belongs to the receiving view, never to the category identity.
.rls_encode_data_value <- function(value) {
  value <- as.character(value)
  value[is.na(value)] <- ""
  for (pair in list(c("%", "%25"), c("|", "%7C"), c("\r", "%0D"),
                    c("\n", "%0A"), c("\t", "%09")))
    value <- gsub(pair[[1L]], pair[[2L]], value, fixed=TRUE, useBytes=TRUE)
  .rls_native_wire_value(value)
}
