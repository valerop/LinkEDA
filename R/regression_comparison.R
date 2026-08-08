.rls_regcmp_record <- function(comparison) {
  id <- if (inherits(comparison, "rlispstat_regression_comparison")) comparison$id else comparison
  id <- .rls_validate_protocol_name(id, "comparison")
  if (!exists(id, envir = .rls_state$regression_comparisons, inherits = FALSE)) {
    stop("Unknown regression comparison.", call. = FALSE)
  }
  get(id, envir = .rls_state$regression_comparisons)
}

.rls_assign_regcmp <- function(record) {
  assign(record$id, record, envir = .rls_state$regression_comparisons)
  structure(list(id = record$id, group = record$group), class = "rlispstat_regression_comparison")
}

.rls_regcmp_default_labels <- function(models) {
  labels <- names(models)
  if (is.null(labels)) {
    labels <- rep("", length(models))
  }
  labels[is.na(labels)] <- ""
  next_untitled <- 1L
  for (i in seq_along(labels)) {
    if (!nzchar(labels[[i]])) {
      labels[[i]] <- paste("Untitled", next_untitled)
      next_untitled <- next_untitled + 1L
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

.rls_regcmp_next_untitled_label <- function(record) {
  labels <- vapply(record$models, `[[`, character(1L), "label")
  serials <- suppressWarnings(as.integer(sub("^Untitled ", "", labels[grepl("^Untitled [0-9]+$", labels)])))
  next_index <- if (length(serials)) max(serials) + 1L else 1L
  repeat {
    candidate <- paste("Untitled", next_index)
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

.rls_regcmp_fit_model <- function(record, index) {
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
    term_types = record$term_types %||% list(),
    analysis_backend = record$analysis_backend %||% "ordinary"
  )
  if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation")) {
    extracted <- .rls_mi_fit_linear_model_record(glm_record)
  } else {
    fit_record <- glm_record
    fit_record$data <- .rls_glm_data_for_fit(glm_record)
    complete <- .rls_glm_complete_data(fit_record)
    fit <- stats::lm(.rls_glm_formula_object(fit_record), data = complete$data)
    extracted <- .rls_glm_extract_fit(fit_record, fit, complete)
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
  if (is.null(previous$fit) || is.null(current$fit)) {
    return(list(method = "Nested F test", status = "not_fitted", df1 = NA_real_, df2 = NA_real_, delta = NA_real_, F = NA_real_, p = NA_real_))
  }
  test <- tryCatch(stats::anova(previous$fit, current$fit), error = function(e) NULL)
  if (is.null(test) || nrow(test) < 2L) {
    return(list(method = "Nested F test", status = "unavailable", df1 = NA_real_, df2 = NA_real_, delta = NA_real_, F = NA_real_, p = NA_real_))
  }
  delta_df <- suppressWarnings(as.numeric(test$Df[[2L]]))
  delta_ss <- suppressWarnings(as.numeric(test$`Sum of Sq`[[2L]]))
  stat <- suppressWarnings(as.numeric(test$F[[2L]]))
  p <- suppressWarnings(as.numeric(test$`Pr(>F)`[[2L]]))
  df2 <- suppressWarnings(as.numeric(stats::df.residual(current$fit)))
  list(
    method = "Nested F test",
    status = if (is.finite(stat) && is.finite(p)) "ok" else "unavailable",
    df1 = delta_df,
    df2 = df2,
    delta = delta_ss,
    F = stat,
    p = p
  )
}

.rls_regcmp_update_ordinary_nested_tests <- function(record) {
  if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation")) {
    return(record)
  }
  tests <- vector("list", length(record$models))
  for (i in seq_along(record$models)) {
    tests[[i]] <- if (i == 1L) {
      list(method = "Nested F test", status = "not_applicable", df1 = NA_real_, df2 = NA_real_, delta = NA_real_, F = NA_real_, p = NA_real_)
    } else {
      .rls_regcmp_ordinary_nested_test(record$models[[i - 1L]], record$models[[i]])
    }
    record$models[[i]]$comparison_vs_previous <- tests[[i]]
    record$models[[i]]$summary$comparison_method <- tests[[i]]$method
    record$models[[i]]$summary$comparison_status <- tests[[i]]$status
    record$models[[i]]$summary$comparison_df1 <- tests[[i]]$df1
    record$models[[i]]$summary$comparison_df2 <- tests[[i]]$df2
    record$models[[i]]$summary$comparison_delta <- tests[[i]]$delta
    record$models[[i]]$summary$comparison_f <- tests[[i]]$F
    record$models[[i]]$summary$comparison_p <- tests[[i]]$p
  }
  record$model_comparison_tests <- list(
    method = "Nested F test",
    status = "ok",
    comparisons = tests
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
    "Partial R\u00b2 = ", .rls_regcmp_format_percent(row$partial_r2[[1L]])
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
    plot$data <- model$diagnostics
    assign(id, plot, envir = .rls_state$glm_diagnostic_plots)
  }
  invisible(TRUE)
}

.rls_regcmp_fit_models <- function(record, indices = seq_along(record$models), force = FALSE) {
  for (i in indices) {
    if (isTRUE(force) || isTRUE(record$models[[i]]$is_stale) || is.null(record$models[[i]]$fit)) {
      record <- .rls_regcmp_fit_model(record, i)
    }
  }
  record
}

.rls_regcmp_mi_nested_test <- function(previous, current) {
  added_terms <- setdiff(current$terms, previous$terms)
  if (!length(added_terms)) {
    return(list(
      method = "D1 pooled Wald",
      status = "unchanged",
      detail = "No terms were added relative to the previous model.",
      df1 = NA_real_, df2 = NA_real_, F = NA_real_, p = NA_real_
    ))
  }
  fits <- current$fits_by_imputation %||% list()
  ref_indices <- which(!vapply(fits, is.null, logical(1L)))
  if (!length(ref_indices)) {
    return(list(
      method = "D1 pooled Wald",
      status = "insufficient_data",
      detail = "The current model has no fitted imputations.",
      df1 = NA_real_, df2 = NA_real_, F = NA_real_, p = NA_real_
    ))
  }
  ref_index <- ref_indices[[1L]]
  coefficient_names <- .rls_mi_term_coefficient_names(fits[[ref_index]], added_terms)
  if (!length(coefficient_names)) {
    return(list(
      method = "D1 pooled Wald",
      status = "unsupported",
      detail = "Could not identify model-matrix columns for the terms added relative to the previous model.",
      terms = added_terms,
      df1 = NA_real_, df2 = NA_real_, F = NA_real_, p = NA_real_
    ))
  }
  pool <- .rls_mi_pool_wald_for_fits(fits, coefficient_names, term_names = added_terms)
  list(
    method = pool$method,
    status = if (isTRUE(pool$ok)) "ok" else "insufficient_data",
    detail = sprintf(
      "MI nested-model test vs previous model for added terms: %s. Method: %s; coefficients: %s.",
      paste(added_terms, collapse = ", "),
      pool$method,
      paste(coefficient_names, collapse = ", ")
    ),
    terms = added_terms,
    coefficient_names = coefficient_names,
    df1 = pool$df1,
    df2 = pool$df2,
    F = pool$F,
    p = pool$p,
    pooled_result = pool
  )
}

.rls_regcmp_update_mi_nested_tests <- function(record) {
  if (!identical(record$analysis_backend %||% "ordinary", "multiple_imputation")) {
    return(record)
  }
  tests <- vector("list", length(record$models))
  for (i in seq_along(record$models)) {
    if (i == 1L) {
      tests[[i]] <- list(
        method = "D1 pooled Wald",
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
  record$model_comparison_tests <- list(
    method = "D1 pooled Wald",
    status = "ok",
    comparisons = tests,
    detail = "Each MI comparison tests the current model against the immediately previous model using a pooled Wald/D1-style test. No F statistics or p-values are averaged."
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
    if (!identical(level_parent, source) && identical(record$term_types[[level_parent]] %||% NA_character_, "numeric")) {
      next
    }
    visible_source <- .rls_model_term_display_name(source)
    source_type <- record$term_types[[source]] %||% NA_character_
    source_rows <- if (identical(source_type, "numeric")) {
      data.frame()
    } else {
      do.call(rbind, lapply(record$models, function(model) {
        model$coefficient_rows[model$coefficient_rows$source_term %in% c(source, visible_source), , drop = FALSE]
      }))
    }
    if (!is.null(source_rows) && nrow(source_rows)) {
      for (i in seq_len(nrow(source_rows))) {
        add_row(as.list(source_rows[i, , drop = FALSE]))
      }
    } else {
      add_row(list(term = source, display_label = source, row_type = "coefficient", source_term = source))
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
      model_response <- spec$response %||% response
      if (is.null(model_response)) {
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
    list(label = labels[[i]], response = model_response, terms = terms)
  })
}

.rls_regcmp_sync_native_open <- function(record) {
  .rls_start_backend()
  dataset <- .rls_dataset_record(record$group)
  .rls_send(c(
    "REGISTER_DATASET",
    record$group,
    .rls_variable_payload(record$data, dataset$variable_metadata),
    .rls_dataframe_payload(record$data, dataset$variable_metadata, dataset_record = dataset)
  ))
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

#' Create a native regression model comparison window
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
  label <- label %||% .rls_regcmp_next_untitled_label(record)
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
