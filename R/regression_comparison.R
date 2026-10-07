.rls_regcmp_record <- function(comparison) {
  id <- if (inherits(comparison, "rlispstat_regression_comparison")) comparison$id else comparison
  id <- .rls_validate_protocol_name(id, "comparison")
  if (!exists(id, envir = .rls_state$regression_comparisons, inherits = FALSE)) {
    stop("Unknown General Linear Model comparison.", call. = FALSE)
  }
  get(id, envir = .rls_state$regression_comparisons)
}

.rls_assign_regcmp <- function(record) {
  if (length(record$models %||% list()) &&
      all(vapply(record$models, function(model) !is.null(model$fit), logical(1L)))) {
    verification <- .rls_regression_comparison_verification_r_code(record)
    record <- .rls_attach_analysis_provenance(
      record,
      .rls_regression_comparison_executed_r_code(record),
      title = "General Linear Model Comparison",
      output_code = list(
        comparison = "comparison_table <- comparisons",
        models = "model_summaries <- lapply(models, summary)"
      ),
      verification_code = list(comparison = verification$code),
      verification_variables = verification$variables,
      verification_warnings = verification$warnings
    )
  }
  assign(record$id, record, envir = .rls_state$regression_comparisons)
  structure(list(id = record$id, group = record$group), class = "rlispstat_regression_comparison")
}

.rls_regcmp_default_labels <- function(models) {
  labels <- names(models)
  if (is.null(labels)) {
    labels <- rep("", length(models))
  }
  labels[is.na(labels)] <- ""
  next_model <- 1L
  for (i in seq_along(labels)) {
    if (!nzchar(labels[[i]])) {
      repeat {
        candidate <- paste("Model", next_model)
        next_model <- next_model + 1L
        if (!candidate %in% labels) break
      }
      labels[[i]] <- candidate
    }
  }
  labels
}

.rls_regcmp_next_model_id <- function(record) {
  prefix <- paste0(record$id, ":model:")
  ids <- vapply(record$models, `[[`, character(1L), "id")
  serials <- suppressWarnings(as.integer(ifelse(
    startsWith(ids, prefix),
    substring(ids, nchar(prefix) + 1L),
    NA_character_
  )))
  serials <- serials[!is.na(serials)]
  paste0(prefix, if (length(serials)) max(serials) + 1L else 1L)
}

.rls_regcmp_next_model_label <- function(record) {
  labels <- vapply(record$models, `[[`, character(1L), "label")
  serials <- suppressWarnings(as.integer(sub("^Model ", "", labels[grepl("^Model [0-9]+$", labels)])))
  next_index <- if (length(serials)) max(serials) + 1L else 1L
  repeat {
    candidate <- paste("Model", next_index)
    if (!candidate %in% labels) return(candidate)
    next_index <- next_index + 1L
  }
}

.rls_regcmp_model_index <- function(record, model) {
  if (is.numeric(model) && length(model) == 1L && !is.na(model)) {
    idx <- as.integer(model)
    if (idx >= 1L && idx <= length(record$models)) return(idx)
  }
  if (is.character(model) && length(model) == 1L && !is.na(model)) {
    labels <- vapply(record$models, `[[`, character(1L), "label")
    ids <- vapply(record$models, `[[`, character(1L), "id")
    hit <- which(labels == model | ids == model)
    if (length(hit)) return(hit[[1L]])
  }
  stop("`model` must identify an existing comparison model.", call. = FALSE)
}

.rls_regcmp_validate_response <- function(record, response) {
  .rls_model_validate_response(record$data, response, "response")
}

.rls_regcmp_validate_term <- function(record, term, response = record$response) {
  term <- .rls_model_validate_term(record$data, term, response = response, what = "term")
  if (identical(.rls_model_clean_factor_call(term), response)) {
    stop("The response variable cannot also be a predictor.", call. = FALSE)
  }
  term
}

.rls_regcmp_refresh_mi_pooling_semantics <- function(record) {
  if (!identical(record$analysis_backend %||% "ordinary", "multiple_imputation")) {
    record$mi_pooling_required <- FALSE
    record$mi_imputed_input_variables <- character()
    return(record)
  }
  dataset <- .rls_dataset_record(record$group)
  rows <- record$comparison_rows %||% .rls_mi_original_row_ids(dataset)
  imputed <- unique(unlist(lapply(record$models, function(model) {
    response <- if (isTRUE(record$uses_shared_response)) record$response else
      model$response %||% record$response
    .rls_mi_imputed_model_variables(
      dataset,
      response,
      model$terms %||% character(),
      original_rows = rows
    )
  }), use.names = FALSE))
  record$mi_imputed_input_variables <- imputed
  record$mi_pooling_required <- length(imputed) > 0L
  record
}

.rls_regcmp_fit_model <- function(record, index) {
  record <- .rls_regcmp_refresh_mi_pooling_semantics(record)
  model <- record$models[[index]]
  response <- if (isTRUE(record$uses_shared_response)) record$response else model$response %||% record$response
  glm_record <- list(
    id = model$id,
    group = record$group,
    data = record$data,
    dependent = response,
    predictors = model$terms,
    scope = record$scope,
    selected_rows = record$selected_rows %||% integer(),
    comparison_rows = record$comparison_rows %||% integer(),
    term_types = model$term_types %||% record$term_types %||% list(),
    centered_predictors = model$centered_predictors %||% character(),
    factor_reference_levels = model$factor_reference_levels %||% list(),
    analysis_backend = record$analysis_backend %||% "ordinary"
  )
  if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation") &&
      isTRUE(record$mi_pooling_required)) {
    extracted <- .rls_mi_fit_linear_model_record(glm_record)
  } else {
    fit_record <- glm_record
    fit_record$data <- if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation")) {
      .rls_mi_completed_datasets(.rls_dataset_record(record$group))[[1L]]
    } else {
      .rls_glm_data_for_fit(glm_record)
    }
    fit_record$data <- .rls_model_data_for_term_types(
      fit_record$data, glm_record$term_types, response = response
    )
    fit_record$data <- .rls_glm_apply_factor_references(
      fit_record$data, glm_record$factor_reference_levels
    )
    complete <- .rls_glm_complete_data(fit_record)
    complete <- .rls_glm_center_complete_data(complete, glm_record$centered_predictors)
    fit <- stats::lm(.rls_glm_formula_object(fit_record), data = complete$data)
    extracted <- .rls_glm_extract_fit(fit_record, fit, complete)
    if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation")) {
      extracted$status <- paste(
        "Multiple-imputation dataset, but every response and predictor value used by",
        "this comparison is observed; Rubin pooling is not required."
      )
    }
  }
  model$response <- response
  model$fit <- extracted$fit
  model$fitted_lm <- extracted$fit
  model$fits_by_imputation <- extracted$fits_by_imputation %||% NULL
  model$coefficients <- extracted$coefficients
  model$coefficient_rows <- extracted$coefficient_rows
  model$summary <- extracted$summary
  model$rows_used <- extracted$rows_used
  model$rows_excluded <- extracted$rows_excluded
  model$diagnostics <- extracted$diagnostics
  model$diagnostics_by_imputation <- extracted$diagnostics_by_imputation %||% NULL
  model$multiple_imputation <- extracted$multiple_imputation %||% NULL
  model$fit_version <- model$fit_version + 1L
  model$is_stale <- FALSE
  model$status <- extracted$status
  record$models[[index]] <- model
  record
}

.rls_regcmp_ordinary_nested_test <- function(previous, current) {
  unavailable <- function(status, detail) list(method = "Nested F test", status = status,
    detail = detail, df1 = NA_real_, df2 = NA_real_, delta = NA_real_, F = NA_real_, p = NA_real_)
  if (is.null(previous$fit) || is.null(current$fit))
    return(unavailable("not_fitted", "Fit both models before comparing them."))
  left <- stats::model.frame(previous$fit)
  right <- stats::model.frame(current$fit)
  if (!identical(rownames(left), rownames(right)) ||
      (!is.null(previous$rows_used) && !is.null(current$rows_used) &&
       !identical(previous$rows_used, current$rows_used)))
    return(unavailable("different_cases", "The models use different observations. Refit them on the same cases before comparing them."))
  if (!identical(stats::formula(previous$fit)[[2L]], stats::formula(current$fit)[[2L]]) ||
      !isTRUE(all.equal(stats::model.response(left), stats::model.response(right), tolerance = 0)) ||
      !identical(stats::model.weights(left), stats::model.weights(right)) ||
      !identical(stats::model.offset(left), stats::model.offset(right)))
    return(unavailable("incompatible_models", "Nested F tests require the same response, weights and offset."))
  x1 <- stats::model.matrix(previous$fit)
  x2 <- stats::model.matrix(current$fit)
  rank1 <- qr(x1)$rank; rank2 <- qr(x2)$rank
  joint_rank <- qr(cbind(if (rank1 > rank2) x1 else x2, if (rank1 > rank2) x2 else x1))$rank
  if (joint_rank != max(rank1, rank2))
    return(unavailable("not_nested", "The model matrices are not nested; no nested F-test p-value is reported."))
  if (rank1 == rank2)
    return(unavailable("unchanged", "The models span the same fitted space; there are no additional degrees of freedom to test."))
  test <- tryCatch(stats::anova(previous$fit, current$fit), error = function(e) NULL)
  if (is.null(test) || nrow(test) < 2L)
    return(unavailable("unavailable", "R could not calculate this nested F test."))
  stat <- as.numeric(test$F[[2L]]); p <- as.numeric(test$`Pr(>F)`[[2L]])
  list(method = "Nested F test", status = if (is.finite(stat) && is.finite(p)) "ok" else "unavailable",
    df1 = abs(as.numeric(test$Df[[2L]])),
    df2 = as.numeric(stats::df.residual(if (rank1 > rank2) previous$fit else current$fit)),
    delta = as.numeric(test$`Sum of Sq`[[2L]]), F = stat, p = p)
}

.rls_regcmp_update_ordinary_nested_tests <- function(record) {
  record <- .rls_regcmp_refresh_mi_pooling_semantics(record)
  if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation") &&
      isTRUE(record$mi_pooling_required)) {
    return(record)
  }
  tests <- vector("list", length(record$models))
  for (i in seq_along(record$models)) {
    tests[[i]] <- if (i == 1L) {
      list(method = "Nested F test", status = "not_applicable", df1 = NA_real_, df2 = NA_real_, delta = NA_real_, F = NA_real_, p = NA_real_)
    } else {
      .rls_regcmp_ordinary_nested_test(record$models[[i - 1L]], record$models[[i]])
    }
    if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation")) {
      tests[[i]]$method <- "Nested F test (Rubin pooling not required)"
      tests[[i]]$detail <- paste(
        "No response or predictor value used by the compared models was imputed;",
        "the ordinary nested F test is identical across completed datasets."
      )
    }
    record$models[[i]]$comparison_vs_previous <- tests[[i]]
    record$models[[i]]$summary$comparison_method <- tests[[i]]$method
    record$models[[i]]$summary$comparison_status <- tests[[i]]$status
    record$models[[i]]$summary$comparison_detail <- tests[[i]]$detail %||% ""
    record$models[[i]]$summary$comparison_df1 <- tests[[i]]$df1
    record$models[[i]]$summary$comparison_df2 <- tests[[i]]$df2
    record$models[[i]]$summary$comparison_delta <- tests[[i]]$delta
    record$models[[i]]$summary$comparison_f <- tests[[i]]$F
    record$models[[i]]$summary$comparison_p <- tests[[i]]$p
  }
  record$model_comparison_tests <- list(
    method = if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation"))
      "Nested F test (Rubin pooling not required)" else "Nested F test",
    status = "ok",
    comparisons = tests,
    detail = if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation"))
      "All model inputs are fully observed in the analyzed rows; Rubin's rules were not applied."
    else "Ordinary nested F tests."
  )
  record
}

.rls_regcmp_format_p <- function(p) {
  if (!is.finite(p)) return("\u2014")
  if (p < .001) return("< .001")
  sub("^0", "", sprintf("%.3f", p))
}

.rls_regcmp_format_percent <- function(x, digits = 1L) {
  if (!is.finite(x)) return("\u2014")
  sprintf(paste0("%.", digits, "f%%"), 100 * x)
}

.rls_regcmp_format_number <- function(x, digits = 3L) {
  if (!is.finite(x)) return("\u2014")
  sprintf(paste0("%.", digits, "f"), x)
}

.rls_regcmp_fit_rows <- function(record, model) {
  index <- .rls_regcmp_model_index(record, model)
  summary <- record$models[[index]]$summary
  labels <- c("N", "R\u00b2", "Adjusted R\u00b2", "s", "df residual", "F", "p", "AIC", "BIC")
  values <- c(
    as.character(summary$n_used %||% NA_integer_),
    .rls_regcmp_format_percent(summary$r_squared %||% NA_real_),
    .rls_regcmp_format_percent(summary$adj_r_squared %||% NA_real_),
    .rls_regcmp_format_number(summary$residual_se %||% NA_real_, 3L),
    as.character(summary$df_residual %||% NA_integer_),
    .rls_regcmp_format_number(summary$global_f %||% NA_real_, 3L),
    .rls_regcmp_format_p(summary$global_p %||% NA_real_),
    .rls_regcmp_format_number(summary$aic %||% NA_real_, 1L),
    .rls_regcmp_format_number(summary$bic %||% NA_real_, 1L)
  )
  values[is.na(values) | !nzchar(values)] <- "\u2014"
  data.frame(label = labels, value = values, stringsAsFactors = FALSE)
}

.rls_regcmp_coefficient_detail_text <- function(record, model, term) {
  index <- .rls_regcmp_model_index(record, model)
  model_record <- record$models[[index]]
  display_row <- model_record$coefficient_rows[model_record$coefficient_rows$term == term, , drop = FALSE]
  if (nrow(display_row) && display_row$row_type[[1L]] %in% c("factor_parent", "term_parent", "reference", "factor_level")) {
    return(.rls_model_display_detail_text(display_row, display_row$statistic_name[[1L]]))
  }
  row <- model_record$coefficients[model_record$coefficients$term == term, , drop = FALSE]
  if (!nrow(row)) return("\u2014")
  response <- model_record$response %||% record$response
  beta <- if (identical(term, "(Intercept)") || !term %in% names(model_record$fit$model)) "\u2014" else .rls_regcmp_format_number(row$estimate[[1L]] * stats::sd(model_record$fit$model[[term]], na.rm = TRUE) / stats::sd(model_record$fit$model[[response]], na.rm = TRUE), 3L)
  paste0(
    "b = ", .rls_regcmp_format_number(row$estimate[[1L]], 2L), "; ",
    "\u03b2 = ", beta, "; ",
    "SE = ", .rls_regcmp_format_number(row$std_error[[1L]], 2L), "; ",
    "t = ", .rls_regcmp_format_number(row$t_value[[1L]], 2L), "; ",
    "p", if (startsWith(.rls_regcmp_format_p(row$p_value[[1L]]), "<")) " " else " = ",
    .rls_regcmp_format_p(row$p_value[[1L]]), "; ",
    "Partial r = ", .rls_regcmp_format_number(row$partial_r[[1L]], 3L), "; ",
    "\u0394R\u00b2 = ", .rls_regcmp_format_number(row$delta_r2[[1L]], 3L)
  )
}

.rls_regcmp_notify_diagnostic_plots <- function(record, model_ids) {
  ids <- ls(.rls_state$glm_diagnostic_plots, all.names = TRUE)
  for (id in ids) {
    plot <- get(id, envir = .rls_state$glm_diagnostic_plots)
    if (!identical(plot$comparison_id, record$id) || !plot$model_id %in% model_ids) {
      next
    }
    model_index <- match(plot$model_id, vapply(record$models, `[[`, character(1L), "id"))
    if (is.na(model_index)) next
    model <- record$models[[model_index]]
    plot$displayed_fit_version <- model$fit_version
    plot$displayed_diagnostics_version <- model$fit_version
    plot$is_stale <- FALSE
    plot$data <- model$diagnostics
    assign(id, plot, envir = .rls_state$glm_diagnostic_plots)
  }
  invisible(TRUE)
}

.rls_regcmp_fit_models <- function(record, indices = seq_along(record$models), force = FALSE) {
  previous_scope <- record$data_scope
  record <- .rls_apply_scope_to_model_request(record, .rls_dataset_record(record$group))
  if (!identical(previous_scope$rows,record$data_scope$rows)) {
    indices <- seq_along(record$models); force <- TRUE
  }
  if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation")) {
    dataset <- .rls_dataset_record(record$group)
    specs <- lapply(record$models, function(model) list(
      response = if (isTRUE(record$uses_shared_response)) record$response else
        model$response %||% record$response,
      terms = model$terms %||% character(),
      term_types = model$term_types %||% record$term_types %||% list(),
      centered_predictors = model$centered_predictors %||% character(),
      factor_reference_levels = model$factor_reference_levels %||% list(),
      scope = record$scope %||% "all",
      count_regression = FALSE,
      exposure = ""
    ))
    common_rows <- .rls_mi_common_model_rows(
      dataset, specs, record$selected_rows %||% integer()
    )
    if (!identical(record$comparison_rows, common_rows)) {
      record$comparison_rows <- common_rows
      indices <- seq_along(record$models)
      force <- TRUE
    }
    record <- .rls_regcmp_refresh_mi_pooling_semantics(record)
  }
  for (i in indices) {
    if (isTRUE(force) || isTRUE(record$models[[i]]$is_stale) || is.null(record$models[[i]]$fit)) {
      record <- .rls_regcmp_fit_model(record, i)
    }
  }
  record
}

.rls_regcmp_mi_nested_test <- function(previous, current) {
  unavailable <- function(status, detail) list(method = "mice::D1", status = status,
    detail = detail, df1 = NA_real_, df2 = NA_real_, F = NA_real_, p = NA_real_)
  left <- previous$fits_by_imputation %||% list()
  right <- current$fits_by_imputation %||% list()
  if (length(left) < 2L || length(left) != length(right) ||
      any(vapply(c(left, right), is.null, logical(1L))))
    return(unavailable("insufficient_data", "Both models require a fit for every imputation."))
  sizes <- vapply(left, function(fit) length(stats::coef(fit)), integer(1L)) -
    vapply(right, function(fit) length(stats::coef(fit)), integer(1L))
  if (all(sizes == 0L)) {
    identical_spaces <- vapply(seq_along(left), function(i) {
      .rls_regcmp_ordinary_nested_test(list(fit = left[[i]]), list(fit = right[[i]]))$status == "unchanged"
    }, logical(1L))
    return(if (all(identical_spaces)) unavailable("unchanged", "Both models span the same fitted space in every imputation.") else
      unavailable("incompatible_models", "The models differ without forming a proper nested comparison."))
  }
  if (!all(sizes > 0L) && !all(sizes < 0L))
    return(unavailable("incompatible_models", "Nesting direction must be the same in every imputation."))
  removing <- all(sizes > 0L)
  full <- if (removing) previous else current
  reduced <- if (removing) current else previous
  changed_terms <- setdiff(full$terms, reduced$terms)
  pool <- tryCatch(.rls_mi_pool_d1(full$fits_by_imputation, reduced$fits_by_imputation,
    term_names = changed_terms), error = function(e) e)
  if (inherits(pool, "error"))
    return(unavailable("incompatible_models", conditionMessage(pool)))
  list(method = pool$method, status = if (isTRUE(pool$ok)) "ok" else "insufficient_data",
    detail = sprintf("MI nested-model test for terms %s relative to the previous model: %s. Method: %s.",
      if (removing) "removed" else "added", paste(changed_terms, collapse = ", "), pool$method),
    terms = changed_terms, direction = if (removing) "removed" else "added",
    coefficient_names = setdiff(names(stats::coef(full$fits_by_imputation[[1L]])),
                                names(stats::coef(reduced$fits_by_imputation[[1L]]))),
    df1 = pool$df1, df2 = pool$df2, F = pool$F, p = pool$p, pooled_result = pool)
}

.rls_regcmp_update_mi_nested_tests <- function(record) {
  record <- .rls_regcmp_refresh_mi_pooling_semantics(record)
  if (!identical(record$analysis_backend %||% "ordinary", "multiple_imputation") ||
      !isTRUE(record$mi_pooling_required)) {
    return(record)
  }
  tests <- vector("list", length(record$models))
  for (i in seq_along(record$models)) {
    if (i == 1L) {
      tests[[i]] <- list(
        method = "mice::D1",
        status = "not_applicable",
        detail = "First model has no previous model for comparison.",
        df1 = NA_real_, df2 = NA_real_, F = NA_real_, p = NA_real_
      )
    } else {
      tests[[i]] <- .rls_regcmp_mi_nested_test(record$models[[i - 1L]], record$models[[i]])
    }
    record$models[[i]]$comparison_vs_previous <- tests[[i]]
    record$models[[i]]$summary$comparison_method <- tests[[i]]$method
    record$models[[i]]$summary$comparison_status <- tests[[i]]$status
    record$models[[i]]$summary$comparison_df1 <- tests[[i]]$df1
    record$models[[i]]$summary$comparison_df2 <- tests[[i]]$df2
    record$models[[i]]$summary$comparison_f <- tests[[i]]$F
    record$models[[i]]$summary$comparison_p <- tests[[i]]$p
  }
  comparison_methods <- unique(vapply(tests[-1L], function(test) test$method %||% "mice::D1", character(1L)))
  record$model_comparison_tests <- list(
    method = if (length(comparison_methods)) paste(comparison_methods, collapse = "; ") else "mice::D1",
    status = "ok",
    comparisons = tests,
    detail = "Each MI comparison passes the larger model and its nested smaller model to mice::D1, including comparisons that remove terms. If D1 has undefined denominator degrees of freedom (notably for very small m), LinkEDA uses mice::D3 as an explicit nested-model fallback. D2 is not used because it pools test statistics rather than performing the intended full/reduced model comparison. No statistics or p-values are averaged."
  )
  record
}

.rls_regcmp_stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p < .001) return("***")
  if (p < .01) return("**")
  if (p < .05) return("*")
  ""
}

.rls_regcmp_model_includes_source <- function(model, source) {
  identical(source, "(Intercept)") ||
    .rls_model_term_list_contains(model$terms, source) ||
    .rls_model_term_list_contains(vapply(model$terms, .rls_model_term_display_name, character(1L)), source)
}

.rls_regcmp_display_cell <- function(record, model_index, term) {
  model <- record$models[[model_index]]
  display_row <- model$coefficient_rows[model$coefficient_rows$term == term, , drop = FALSE]
  if (nrow(display_row)) {
    if (display_row$row_type[[1L]] == "reference") {
      return("\u2014")
    }
    if (display_row$row_type[[1L]] %in% c("factor_parent", "term_parent")) {
      return("")
    }
    if (!.rls_regcmp_model_includes_source(model, display_row$source_term[[1L]])) {
      return("\u2014")
    }
    return(paste0(sprintf("%.3f", display_row$estimate[[1L]]), .rls_regcmp_stars(display_row$p_value[[1L]])))
  }
  if (!identical(term, "(Intercept)") && !term %in% model$terms) {
    return("\u2014")
  }
  coef <- model$coefficients
  row <- coef[coef$term == term, , drop = FALSE]
  if (!nrow(row)) return("")
  paste0(sprintf("%.3f", row$estimate[[1L]]), .rls_regcmp_stars(row$p_value[[1L]]))
}

.rls_regcmp_display_rows <- function(record) {
  rows <- list()
  add_row <- function(row) {
    if (!length(rows) || !row$term[[1L]] %in% vapply(rows, `[[`, character(1L), "term")) {
      rows[[length(rows) + 1L]] <<- row
    }
  }
  add_row(list(term = "(Intercept)", display_label = "(Intercept)", row_type = "coefficient", source_term = "(Intercept)"))
  for (source in setdiff(record$term_rows, "(Intercept)")) {
    level_parent <- .rls_model_term_without_level_suffixes(source)
    if (!identical(level_parent, source) &&
        !any(vapply(record$models, .rls_regcmp_model_includes_source, logical(1L), source = level_parent))) {
      next
    }
    visible_source <- .rls_model_term_display_name(source)
    relevant_models <- Filter(function(model) {
      .rls_regcmp_model_includes_source(model, source)
    }, record$models)
    source_rows <- if (length(relevant_models)) {
      do.call(rbind, lapply(relevant_models, function(model) {
        model$coefficient_rows[model$coefficient_rows$source_term %in% c(source, visible_source), , drop = FALSE]
      }))
    } else {
      data.frame()
    }
    factor_rows <- if (!is.null(source_rows) && nrow(source_rows)) {
      source_rows[source_rows$row_type %in%
                    c("factor_parent", "reference", "factor_level"), , drop = FALSE]
    } else data.frame()
    if (nrow(factor_rows)) {
      # A comparison row may be numeric in one model and categorical in
      # another.  Use the fitted factor rows to define the shared row tree;
      # each model still supplies its own value for the parent row.
      parent <- factor_rows[factor_rows$row_type == "factor_parent", , drop = FALSE]
      if (nrow(parent)) add_row(as.list(parent[1L, , drop = FALSE]))
      for (i in which(factor_rows$row_type != "factor_parent")) {
        add_row(as.list(factor_rows[i, , drop = FALSE]))
      }
    } else {
      add_row(list(term = source, display_label = visible_source,
                   row_type = "coefficient", source_term = visible_source))
    }
  }
  data.frame(
    term = vapply(rows, `[[`, character(1L), "term"),
    display_label = vapply(rows, `[[`, character(1L), "display_label"),
    row_type = vapply(rows, `[[`, character(1L), "row_type"),
    source_term = vapply(rows, `[[`, character(1L), "source_term"),
    stringsAsFactors = FALSE
  )
}

.rls_regcmp_model_specs <- function(record, response, models) {
  labels <- .rls_regcmp_default_labels(models)
  lapply(seq_along(models), function(i) {
    spec <- models[[i]]
    if (is.list(spec) && (!is.null(spec$response) || !is.null(spec$terms))) {
      model_response <- as.character(spec$response %||% "")[[1L]]
      if (!nzchar(model_response)) model_response <- response
      if (is.null(model_response) || !nzchar(as.character(model_response)[[1L]])) {
        stop("Each model must provide `response` when no shared `response` is supplied.", call. = FALSE)
      }
      terms <- spec$terms %||% character()
    } else {
      model_response <- response
      if (is.null(model_response)) {
        stop("`response` is required for old-style predictor-vector models.", call. = FALSE)
      }
      terms <- spec
    }
    model_response <- .rls_regcmp_validate_response(record, model_response)
    terms <- unlist(lapply(terms %||% character(), function(term) {
      term <- .rls_regcmp_validate_term(record, term, response = model_response)
      .rls_model_hierarchical_terms(record$data, term, response = model_response)
    }), use.names = FALSE)
    terms <- if (length(terms)) unique(as.character(terms)) else character()
    centered <- if (is.list(spec)) spec$centered_predictors %||% character() else character()
    centered <- unique(as.character(centered[nzchar(centered)]))
    term_types <- if (is.list(spec)) spec$term_types %||% list() else list()
    if (!is.list(term_types)) term_types <- as.list(term_types)
    term_variables <- if (length(terms)) all.vars(stats::reformulate(terms)) else character()
    term_types <- term_types[names(term_types) %in% term_variables]
    term_types <- lapply(term_types, function(type) {
      type <- as.character(type)[[1L]]
      if (type %in% c("numeric", "factor")) type else "numeric"
    })
    factor_references <- if (is.list(spec)) spec$factor_reference_levels %||% list() else list()
    if (!is.list(factor_references)) factor_references <- as.list(factor_references)
    factor_references <- factor_references[names(factor_references) %in% term_variables]
    factor_references <- lapply(factor_references, function(level) as.character(level)[[1L]])
    list(label = labels[[i]], response = model_response, terms = terms,
         term_types = term_types,
         centered_predictors = intersect(centered, term_variables),
         factor_reference_levels = factor_references)
  })
}

.rls_regcmp_sync_native_open <- function(record) {
  .rls_start_backend()
  dataset <- .rls_dataset_record(record$group)
  .rls_register_native_dataset_if_needed(dataset, record$data, visible = TRUE)
  payload <- c(
    "REGCMP_OPEN2", record$id, record$group, record$response, record$scope,
    if (isTRUE(record$auto_refit)) "TRUE" else "FALSE",
    as.character(length(record$models))
  )
  for (model in record$models) {
    payload <- c(payload, model$label, model$response %||% record$response, as.character(length(model$terms)), model$terms)
  }
  .rls_parse_records(.rls_send(payload))[[1L]]
}

#' Create a native General Linear Model comparison window
#'
#' @param data Registered dataset name, a data frame, or `NULL` for the active dataset.
#' @param response Numeric response variable.
#' @param models Named list of predictor vectors.
#' @param scope One of `"all"`, `"selected"`, or `"unselected"`.
#' @param name Optional comparison id or dataset name for data-frame input.
#' @return A `rlispstat_regression_comparison` handle.
#' @export
ls_new_regression_comparison <- function(data = NULL, response = NULL, models = NULL,
                                         scope = "all", name = NULL, native = TRUE) {
  scope <- match.arg(scope, c("all", "selected", "unselected"))
  if (is.data.frame(data)) {
    group <- .rls_register_dataset(name %||% "regression_comparison", data, activate = TRUE)
    record <- .rls_dataset_record(group)
  } else {
    record <- .rls_dataset_record(data)
  }
  numeric <- .rls_numeric_variable_names(record$data, record$variable_metadata)
  if (!length(numeric)) {
    stop("The dataset must contain numeric variables.", call. = FALSE)
  }
  if (is.null(models)) {
    models <- list(character())
  }
  if (!is.list(models)) {
    stop("`models` must be a named list of predictor vectors.", call. = FALSE)
  }
  shared_response <- !is.null(response)
  if (!is.null(response)) {
    response <- .rls_regcmp_validate_response(list(data = record$data), response)
  }
  model_specs <- .rls_regcmp_model_specs(list(data = record$data, response = response), response, models)
  response <- response %||% model_specs[[1L]]$response
  id <- .rls_validate_protocol_name(name %||% paste0("regcmp_", record$group, "_", format(Sys.time(), "%Y%m%d%H%M%OS3")), "name")
  comparison <- list(
    id = id,
    group = record$group,
    data = record$data,
    dataset_type = record$dataset_type %||% "data_frame",
    analysis_backend = .rls_analysis_backend(record, "regression_comparison"),
    imputation_id = record$imputation_id %||% NULL,
    imputation_count = record$imputation_count %||% NULL,
    response = response,
    uses_shared_response = shared_response,
    scope = scope,
    auto_refit = TRUE,
    term_rows = "(Intercept)",
    term_types = list(),
    selected_rows = integer(),
    models = vector("list", length(models)),
    active_model = 1L
  )
  for (i in seq_along(model_specs)) {
    terms <- model_specs[[i]]$terms
    comparison$term_rows <- unique(c(comparison$term_rows, terms))
    comparison$models[[i]] <- list(
      id = paste0(comparison$id, ":model:", i),
      label = model_specs[[i]]$label,
      response = model_specs[[i]]$response,
      terms = terms,
      term_types = model_specs[[i]]$term_types %||% list(),
      centered_predictors = model_specs[[i]]$centered_predictors %||% character(),
      factor_reference_levels = model_specs[[i]]$factor_reference_levels %||% list(),
      fit = NULL,
      fitted_lm = NULL,
      coefficients = data.frame(),
      coefficient_rows = data.frame(),
      summary = list(),
      rows_used = integer(),
      rows_excluded = integer(),
      diagnostics = data.frame(),
      diagnostics_by_imputation = NULL,
      fits_by_imputation = NULL,
      multiple_imputation = NULL,
      model_version = 0L,
      fit_version = 0L,
      is_stale = TRUE,
      status = "Not fitted."
    )
  }
  comparison <- .rls_regcmp_fit_models(comparison, force = TRUE)
  comparison <- .rls_regcmp_update_mi_nested_tests(comparison)
  comparison <- .rls_regcmp_update_ordinary_nested_tests(comparison)
  .rls_assign_regcmp(comparison)
  if (isTRUE(native) && identical(comparison$analysis_backend, "multiple_imputation")) {
    .rls_regcmp_sync_native_pooled(comparison)
  } else if (isTRUE(native)) {
    .rls_regcmp_sync_native_open(comparison)
  }
  invisible(.rls_assign_regcmp(comparison))
}

#' @rdname ls_new_regression_comparison
#' @export
ls_regression_comparison_add_model <- function(comparison, terms = NULL, label = NULL, response = NULL) {
  record <- .rls_regcmp_record(comparison)
  response <- .rls_regcmp_validate_response(record, response %||% record$models[[record$active_model]]$response %||% record$response)
  terms <- unlist(lapply(terms %||% record$models[[record$active_model]]$terms, function(term) {
    term <- .rls_regcmp_validate_term(record, term, response = response)
    .rls_model_hierarchical_terms(record$data, term, response = response)
  }), use.names = FALSE)
  terms <- if (length(terms)) unique(as.character(terms)) else character()
  label <- label %||% .rls_regcmp_next_model_label(record)
  model <- list(
    id = .rls_regcmp_next_model_id(record),
    label = label,
    response = response,
    terms = terms,
    fit = NULL,
    fitted_lm = NULL,
    coefficients = data.frame(),
    coefficient_rows = data.frame(),
    summary = list(),
    rows_used = integer(),
    rows_excluded = integer(),
    diagnostics = data.frame(),
    diagnostics_by_imputation = NULL,
    fits_by_imputation = NULL,
    multiple_imputation = NULL,
    model_version = 0L,
    fit_version = 0L,
    is_stale = TRUE,
    status = "Not fitted."
  )
  record$models[[length(record$models) + 1L]] <- model
  record$term_rows <- unique(c(record$term_rows, terms))
  record$active_model <- length(record$models)
  record <- .rls_regcmp_fit_model(record, record$active_model)
  record <- .rls_regcmp_update_mi_nested_tests(record)
  record <- .rls_regcmp_update_ordinary_nested_tests(record)
  .rls_assign_regcmp(record)
  if (isTRUE(.rls_state$process_started)) {
    if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation")) {
      try(.rls_regcmp_sync_native_pooled(record), silent = TRUE)
    } else {
      try(.rls_send(c("REGCMP_ADD_MODEL", record$id, label, response, as.character(length(terms)), terms)), silent = TRUE)
    }
  }
  invisible(.rls_assign_regcmp(record))
}

#' @rdname ls_new_regression_comparison
#' @export
ls_regression_comparison_add_term <- function(comparison, term) {
  record <- .rls_regcmp_record(comparison)
  index <- record$active_model
  term <- .rls_regcmp_validate_term(record, term, response = record$models[[index]]$response %||% record$response)
  terms <- .rls_model_hierarchical_terms(record$data, term, response = record$models[[index]]$response %||% record$response)
  old_versions <- vapply(record$models, `[[`, integer(1L), "fit_version")
  record$models[[index]]$terms <- unique(c(record$models[[index]]$terms, terms))
  record$term_rows <- unique(c(record$term_rows, terms))
  record$models[[index]]$model_version <- record$models[[index]]$model_version + 1L
  record$models[[index]]$is_stale <- TRUE
  record <- .rls_regcmp_fit_model(record, index)
  record <- .rls_regcmp_update_mi_nested_tests(record)
  record <- .rls_regcmp_update_ordinary_nested_tests(record)
  attr(record, "previous_fit_versions") <- old_versions
  .rls_regcmp_notify_diagnostic_plots(record, record$models[[index]]$id)
  .rls_assign_regcmp(record)
  if (isTRUE(.rls_state$process_started)) {
    if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation")) {
      try(.rls_regcmp_sync_native_pooled(record), silent = TRUE)
    } else {
      try(.rls_send(c("REGCMP_SET_TERM", record$id, as.character(index), term, "TRUE")), silent = TRUE)
    }
  }
  invisible(.rls_assign_regcmp(record))
}

#' @rdname ls_new_regression_comparison
#' @export
ls_regression_comparison_set_term <- function(comparison, model, term, included = TRUE) {
  record <- .rls_regcmp_record(comparison)
  index <- .rls_regcmp_model_index(record, model)
  term <- .rls_regcmp_validate_term(record, term, response = record$models[[index]]$response %||% record$response)
  old_versions <- vapply(record$models, `[[`, integer(1L), "fit_version")
  if (isTRUE(included)) {
    terms <- .rls_model_hierarchical_terms(record$data, term, response = record$models[[index]]$response %||% record$response)
    record$models[[index]]$terms <- unique(c(record$models[[index]]$terms, terms))
    record$term_rows <- unique(c(record$term_rows, terms))
  } else {
    record$models[[index]]$terms <- .rls_model_remove_hierarchical_term(record$models[[index]]$terms, term)
  }
  record$models[[index]]$model_version <- record$models[[index]]$model_version + 1L
  record$models[[index]]$is_stale <- TRUE
  record <- .rls_regcmp_fit_model(record, index)
  record <- .rls_regcmp_update_mi_nested_tests(record)
  record <- .rls_regcmp_update_ordinary_nested_tests(record)
  attr(record, "previous_fit_versions") <- old_versions
  .rls_regcmp_notify_diagnostic_plots(record, record$models[[index]]$id)
  .rls_assign_regcmp(record)
  if (isTRUE(.rls_state$process_started)) {
    if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation")) {
      try(.rls_regcmp_sync_native_pooled(record), silent = TRUE)
    } else {
      try(.rls_send(c("REGCMP_SET_TERM", record$id, as.character(index), term, if (isTRUE(included)) "TRUE" else "FALSE")), silent = TRUE)
    }
  }
  invisible(.rls_assign_regcmp(record))
}

#' @rdname ls_new_regression_comparison
#' @export
ls_regression_comparison_open_diagnostic <- function(comparison, model, type = c("observed_vs_fitted", "residuals_vs_fitted"),
                                                     native = isTRUE(.rls_state$process_started)) {
  type <- match.arg(type)
  record <- .rls_regcmp_record(comparison)
  index <- .rls_regcmp_model_index(record, model)
  native_type <- if (identical(type, "observed_vs_fitted")) "observed_fitted" else "residuals_fitted"
  native_id <- NULL
  if (isTRUE(native)) {
    .rls_start_backend()
    native_id <- .rls_parse_records(.rls_send(c("REGCMP_OPEN_DIAGNOSTIC", record$id, as.character(index), native_type)))[[1L]]
  }
  model_record <- record$models[[index]]
  id <- paste(model_record$id, type, length(ls(.rls_state$glm_diagnostic_plots)) + 1L, sep = ":")
  plot <- list(
    id = id,
    comparison_id = record$id,
    model_id = model_record$id,
    native_plot_id = native_id,
    type = type,
    displayed_fit_version = model_record$fit_version,
    displayed_diagnostics_version = model_record$fit_version,
    is_stale = FALSE,
    data = model_record$diagnostics
  )
  assign(id, plot, envir = .rls_state$glm_diagnostic_plots)
  structure(plot, class = "rlispstat_glm_diagnostic")
}

#' @rdname ls_new_regression_comparison
#' @export
ls_regression_comparison_state <- function(comparison) {
  .rls_regcmp_record(comparison)
}
