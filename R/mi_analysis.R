.rls_analysis_backend <- function(dataset, analysis_type = "analysis") {
  record <- if (is.list(dataset) && !is.null(dataset$data)) dataset else .rls_dataset_record(dataset)
  if (identical(record$dataset_type %||% "data_frame", "multiple_imputation")) {
    "multiple_imputation"
  } else {
    "ordinary"
  }
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

.rls_mi_dataset_for <- function(record, data) {
  record$data <- data
  record$data_frame <- data
  record
}

.rls_mi_pool_scalar <- function(estimates, variances, df_complete = NA_real_, conf_level = 0.95) {
  q <- as.numeric(estimates)
  u <- as.numeric(variances)
  valid <- is.finite(q) & is.finite(u) & u >= 0
  q <- q[valid]
  u <- u[valid]
  m <- length(q)
  if (!m) {
    return(list(
      Qbar = NA_real_, Ubar = NA_real_, B = NA_real_, T = NA_real_,
      SE = NA_real_, df = NA_real_, statistic = NA_real_, p = NA_real_,
      CI_low = NA_real_, CI_high = NA_real_, RIV = NA_real_, FMI = NA_real_,
      m = 0L, valid = FALSE
    ))
  }
  qbar <- mean(q)
  ubar <- mean(u)
  b <- if (m > 1L) stats::var(q) else 0
  total <- ubar + (1 + 1 / m) * b
  se <- sqrt(total)
  riv <- if (is.finite(ubar) && ubar > 0) {
    ((1 + 1 / m) * b) / ubar
  } else if (b > 0) {
    Inf
  } else {
    0
  }
  df_old <- if (m > 1L && is.finite(riv) && riv > 0) {
    (m - 1) * (1 + 1 / riv)^2
  } else {
    Inf
  }
  df <- df_old
  if (is.finite(df_complete) && df_complete > 0 && is.finite(riv)) {
    lambda <- ((1 + 1 / m) * b) / total
    df_obs <- ((df_complete + 1) / (df_complete + 3)) * df_complete * (1 - lambda)
    if (is.finite(df_obs) && df_obs > 0 && is.finite(df_old)) {
      df <- 1 / (1 / df_old + 1 / df_obs)
    }
  }
  statistic <- if (is.finite(se) && se > 0) qbar / se else NA_real_
  p <- if (is.finite(statistic)) {
    if (is.finite(df)) 2 * stats::pt(abs(statistic), df = df, lower.tail = FALSE) else 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
  } else {
    NA_real_
  }
  alpha <- 1 - conf_level
  crit <- if (is.finite(df)) stats::qt(1 - alpha / 2, df = df) else stats::qnorm(1 - alpha / 2)
  ci <- if (is.finite(se)) qbar + c(-1, 1) * crit * se else c(NA_real_, NA_real_)
  lambda <- if (is.finite(total) && total > 0) ((1 + 1 / m) * b) / total else NA_real_
  fmi <- if (is.finite(riv) && is.finite(df)) {
    (riv + 2 / (df + 3)) / (riv + 1)
  } else {
    lambda
  }
  list(
    Qbar = qbar,
    Ubar = ubar,
    B = b,
    T = total,
    SE = se,
    df = df,
    statistic = statistic,
    p = p,
    CI_low = ci[[1L]],
    CI_high = ci[[2L]],
    RIV = riv,
    FMI = fmi,
    m = m,
    valid = TRUE
  )
}

.rls_mi_pool_coefficients <- function(fits, statistic_name = "t") {
  fits <- fits[!vapply(fits, is.null, logical(1L))]
  if (!length(fits)) {
    return(data.frame())
  }
  terms <- unique(unlist(lapply(fits, function(fit) names(stats::coef(fit))), use.names = FALSE))
  rows <- lapply(terms, function(term) {
    estimates <- vapply(fits, function(fit) {
      coef <- stats::coef(fit)
      if (term %in% names(coef)) unname(coef[[term]]) else NA_real_
    }, numeric(1L))
    variances <- vapply(fits, function(fit) {
      vc <- tryCatch(stats::vcov(fit), error = function(e) NULL)
      if (!is.null(vc) && term %in% rownames(vc) && term %in% colnames(vc)) {
        return(unname(vc[term, term]))
      }
      sm <- tryCatch(summary(fit)$coefficients, error = function(e) NULL)
      if (!is.null(sm) && term %in% rownames(sm) && ncol(sm) >= 2L) {
        return(unname(sm[term, 2L]^2))
      }
      NA_real_
    }, numeric(1L))
    df_complete <- suppressWarnings(min(vapply(fits, stats::df.residual, numeric(1L)), na.rm = TRUE))
    pool <- .rls_mi_pool_scalar(estimates, variances, df_complete = df_complete)
    data.frame(
      term = term,
      estimate = pool$Qbar,
      std_error = pool$SE,
      statistic = pool$statistic,
      t_value = pool$statistic,
      p_value = pool$p,
      df = pool$df,
      within_imputation_variance = pool$Ubar,
      between_imputation_variance = pool$B,
      total_variance = pool$T,
      relative_increase_variance = pool$RIV,
      fraction_missing_information = pool$FMI,
      partial_r2 = if (is.finite(pool$statistic) && is.finite(pool$df)) {
        pool$statistic^2 / (pool$statistic^2 + pool$df)
      } else {
        NA_real_
      },
      statistic_name = statistic_name,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.rls_mi_solve <- function(matrix, rhs = NULL) {
  out <- tryCatch({
    if (is.null(rhs)) {
      solve(matrix)
    } else {
      solve(matrix, rhs)
    }
  }, error = function(e) {
    if (!requireNamespace("MASS", quietly = TRUE)) {
      return(NULL)
    }
    inv <- MASS::ginv(matrix)
    if (is.null(rhs)) inv else inv %*% rhs
  })
  out
}

.rls_mi_pool_wald <- function(estimates, variances, term_names = NULL, conf_level = 0.95) {
  q_list <- lapply(estimates, as.numeric)
  u_list <- variances
  valid <- vapply(seq_along(q_list), function(i) {
    q <- q_list[[i]]
    u <- u_list[[i]]
    length(q) > 0L && all(is.finite(q)) && is.matrix(u) &&
      all(dim(u) == length(q)) && all(is.finite(u))
  }, logical(1L))
  q_list <- q_list[valid]
  u_list <- u_list[valid]
  m <- length(q_list)
  if (!m) {
    return(list(
      ok = FALSE, method = "D1 pooled Wald", df1 = NA_real_, df2 = NA_real_,
      F = NA_real_, p = NA_real_, m = 0L, term_names = term_names %||% character(),
      Qbar = numeric(), Ubar = matrix(numeric(), 0, 0), B = matrix(numeric(), 0, 0),
      T = matrix(numeric(), 0, 0), RIV = NA_real_, FMI = NA_real_
    ))
  }
  q <- length(q_list[[1L]])
  q_mat <- do.call(rbind, q_list)
  qbar <- colMeans(q_mat)
  ubar <- Reduce(`+`, u_list) / m
  b <- if (m > 1L) stats::cov(q_mat) else matrix(0, q, q)
  if (q == 1L) b <- matrix(as.numeric(b), 1L, 1L)
  total <- ubar + (1 + 1 / m) * b
  inv_total_q <- .rls_mi_solve(total, qbar)
  if (is.null(inv_total_q)) {
    return(list(
      ok = FALSE, method = "D1 pooled Wald", df1 = q, df2 = NA_real_,
      F = NA_real_, p = NA_real_, m = m, term_names = term_names %||% character(),
      Qbar = qbar, Ubar = ubar, B = b, T = total, RIV = NA_real_, FMI = NA_real_
    ))
  }
  f_value <- as.numeric(t(qbar) %*% inv_total_q / q)
  inv_ubar <- .rls_mi_solve(ubar)
  riv <- if (!is.null(inv_ubar) && q > 0L) {
    as.numeric((1 + 1 / m) * sum(diag(inv_ubar %*% b)) / q)
  } else {
    NA_real_
  }
  if (!is.finite(riv) || riv < 0) riv <- NA_real_
  df2 <- if (m > 1L && is.finite(riv) && riv > 0) {
    (m - 1) * (1 + 1 / riv)^2
  } else {
    Inf
  }
  p <- if (is.finite(f_value)) {
    if (is.finite(df2)) {
      stats::pf(f_value, df1 = q, df2 = df2, lower.tail = FALSE)
    } else {
      stats::pchisq(q * f_value, df = q, lower.tail = FALSE)
    }
  } else {
    NA_real_
  }
  lambda <- if (all(is.finite(total)) && sum(abs(total)) > 0) {
    as.numeric((1 + 1 / m) * sum(diag(.rls_mi_solve(total) %*% b)) / q)
  } else {
    NA_real_
  }
  list(
    ok = TRUE,
    method = "D1 pooled Wald",
    df1 = q,
    df2 = df2,
    F = f_value,
    p = p,
    m = m,
    term_names = term_names %||% character(),
    Qbar = qbar,
    Ubar = ubar,
    B = b,
    T = total,
    RIV = riv,
    FMI = lambda
  )
}

.rls_mi_term_coefficient_names <- function(fit, source_terms) {
  if (is.null(fit) || !length(source_terms)) return(character())
  mm <- tryCatch(stats::model.matrix(fit), error = function(e) NULL)
  if (is.null(mm)) return(character())
  assign <- attr(mm, "assign")
  mm_names <- colnames(mm)
  labels <- attr(stats::terms(fit), "term.labels")
  wanted <- unique(c(source_terms, vapply(source_terms, .rls_model_term_display_name, character(1L))))
  label_hits <- which(labels %in% wanted | vapply(labels, .rls_model_term_display_name, character(1L)) %in% wanted)
  setdiff(mm_names[assign %in% label_hits], "(Intercept)")
}

.rls_mi_pool_wald_for_fits <- function(fits, coefficient_names, term_names = coefficient_names) {
  estimates <- lapply(fits, function(fit) {
    if (is.null(fit)) return(numeric())
    coef <- stats::coef(fit)
    if (!all(coefficient_names %in% names(coef))) return(numeric())
    unname(coef[coefficient_names])
  })
  variances <- lapply(fits, function(fit) {
    if (is.null(fit)) return(matrix(numeric(), 0, 0))
    vc <- tryCatch(stats::vcov(fit), error = function(e) NULL)
    if (is.null(vc) || !all(coefficient_names %in% rownames(vc)) || !all(coefficient_names %in% colnames(vc))) {
      return(matrix(numeric(), 0, 0))
    }
    as.matrix(vc[coefficient_names, coefficient_names, drop = FALSE])
  })
  .rls_mi_pool_wald(estimates, variances, term_names = term_names)
}

.rls_mi_fit_data_by_imputation <- function(record, formula, scope = "all") {
  dataset <- .rls_dataset_record(record$group)
  completed <- .rls_mi_completed_datasets(dataset)
  original_ids <- .rls_mi_original_row_ids(dataset)
  lapply(seq_along(completed), function(i) {
    data <- completed[[i]]
    if (length(record$term_types %||% list())) {
      response <- record$dependent %||% record$response %||% NULL
      data <- .rls_model_data_for_term_types(data, record$term_types, response = response)
    }
    complete <- .rls_model_complete_data(data, formula, scope)
    complete$source_rows <- complete$rows
    complete$original_rows <- original_ids[complete$rows]
    complete$rows <- complete$original_rows
    complete$imputation <- i
    complete
  })
}

.rls_mi_result_metadata <- function(dataset, analysis_type, specification,
                                    fits_by_imputation = list(),
                                    estimates_by_imputation = NULL,
                                    pooled_result = NULL,
                                    pooling_method = "Rubin's rules",
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
  c(list(Overall = seq_len(nrow(data))), stats::setNames(lapply(levels, function(level) {
    which(!is.na(gf) & gf == level)
  }), levels))
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
      fits[[length(fits) + 1L]] <- NULL
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
        test = "MI pooled omnibus (D1)",
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
        test = "MI pooled omnibus (D1)",
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
    pool <- .rls_mi_pool_wald_for_fits(fits, coef_names, term_names = group)
    return(list(
      p = pool$p,
      test = "MI pooled omnibus (D1)",
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
  pool <- .rls_mi_pool_scalar(estimates, variances)
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
  column_names <- if (is.null(group)) "Overall" else c("Overall", group_levels)
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
                      variable = "", level = "", detail = "") {
    row <- as.list(c(Variable = label, values))
    if (!is.null(group) && isTRUE(show_p)) row[["p"]] <- p
    if (!is.null(group) && isTRUE(show_test)) row[["Test"]] <- test
    new_row <- as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
    display <<- rbind(display, new_row[names(display)])
    rows[[length(rows) + 1L]] <<- list(
      row_index = nrow(display), variable = variable, level = level,
      row_type = row_type, values = values, p = p, test = test, detail = detail
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
    p_text <- if (!is.null(test) && isTRUE(show_p)) .rls_export_format_p(test$p) else ""
    test_text <- if (!is.null(test) && isTRUE(show_test)) test$test else ""

    if (identical(type, "numeric")) {
      value_pools <- list()
      values <- vapply(column_names, function(column) {
        means <- vars <- sds <- numeric()
        medians <- q1s <- q3s <- numeric()
        for (data in completed) {
          slices <- .rls_mi_group_slices(dataset, data, group, group_levels, variable_types)
          idx <- slices[[column]]
          x <- .rls_table1_numeric(data[[variable]][idx])
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
        pool <- .rls_mi_pool_scalar(means, vars)
        value_pools[[column]] <<- list(pool = pool, means = means, variances = vars, sds = sds,
                                       medians = medians, q1 = q1s, q3 = q3s)
        .rls_mi_format_mean(pool, sds)
      }, character(1L))
      add_row(variable, values, p_text, test_text, "numeric_mean_sd", variable,
              detail = paste(variable, "pooled mean using Rubin's rules; SD is averaged descriptively across imputations."))
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
                detail = "Median and quartiles are summarized across imputations; Rubin's rules are not applied.")
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
        add_row(paste0("  ", level), values, row_type = paste0(type, "_level"),
                variable = variable, level = level,
                detail = paste(variable, "=", level, "mean n (%) across imputations"))
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
              detail = paste(variable, "missing values in the source data before imputation"))
    }
  }

  note <- c(
    sprintf("Multiple imputation analysis: m = %d imputations.", length(completed)),
    "Pooling method: Rubin's rules for numeric means and two-group numeric comparisons; D1-style pooled Wald tests for numeric comparisons with more than two groups.",
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
    pooling_method = "Rubin's rules for numeric estimates; descriptive averaging for proportions/quantiles",
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
  valid <- is.finite(r) & is.finite(n) & n > 3
  if (!any(valid)) {
    return(list(pool = .rls_mi_pool_scalar(numeric(), numeric()), z = rep(NA_real_, length(r)), variances = rep(NA_real_, length(r))))
  }
  z <- rep(NA_real_, length(r))
  z[valid] <- atanh(pmax(pmin(r[valid], 0.999999), -0.999999))
  variances <- rep(NA_real_, length(r))
  variances[valid] <- 1 / (n[valid] - 3)
  list(pool = .rls_mi_pool_scalar(z, variances), z = z, variances = variances)
}

.rls_mi_correlation_compute <- function(record) {
  dataset <- .rls_dataset_record(record$group)
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
          n = as.integer(round(mean(n_by_imp))), status = "diagonal",
          rows_used = original_ids[rows_by_imp[[1L]]],
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
        if (stats::sd(xv) == 0 || stats::sd(yv) == 0) {
          r_by_imp <- c(r_by_imp, NA_real_)
          statuses <- c(statuses, "zero_variance")
          next
        }
        r_by_imp <- c(r_by_imp, unname(stats::cor(xv, yv, method = "pearson")))
        statuses <- c(statuses, "valid")
      }
      pooled <- .rls_mi_pool_correlation(r_by_imp, n_by_imp)
      pool <- pooled$pool
      r <- if (isTRUE(pool$valid)) tanh(pool$Qbar) else NA_real_
      ci <- if (isTRUE(pool$valid)) tanh(c(pool$CI_low, pool$CI_high)) else c(NA_real_, NA_real_)
      cells[[length(cells) + 1L]] <- list(
        x_variable = x, y_variable = y, r = r, p = pool$p,
        n = as.integer(round(mean(n_by_imp, na.rm = TRUE))),
        status = if (isTRUE(pool$valid)) "valid" else paste(unique(statuses), collapse = ","),
        rows_used = original_ids[rows_by_imp[[1L]]],
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
    stats::lm(formula, data = complete$data)
  })
  if (!any(!vapply(fits, is.null, logical(1L)))) {
    stop("Not enough complete cases in the imputations to fit the model.", call. = FALSE)
  }
  reference_fit <- fits[[which(!vapply(fits, is.null, logical(1L)))[[1L]]]]
  coefs <- .rls_mi_pool_coefficients(fits, statistic_name = "t")
  coefficient_rows <- .rls_model_coefficient_display_rows(
    reference_fit,
    transform(coefs, statistic = t_value),
    dataset$data,
    record$predictors,
    statistic_name = "t"
  )
  summaries <- lapply(fits, function(fit) if (is.null(fit)) NULL else summary(fit))
  valid_summaries <- summaries[!vapply(summaries, is.null, logical(1L))]
  valid_fits <- fits[!vapply(fits, is.null, logical(1L))]
  residual_se <- vapply(valid_summaries, `[[`, numeric(1L), "sigma")
  r_squared <- vapply(valid_summaries, `[[`, numeric(1L), "r.squared")
  adj_r_squared <- vapply(valid_summaries, `[[`, numeric(1L), "adj.r.squared")
  omnibus_coefficients <- .rls_mi_term_coefficient_names(reference_fit, record$predictors)
  omnibus <- if (length(omnibus_coefficients)) {
    .rls_mi_pool_wald_for_fits(fits, omnibus_coefficients, term_names = record$predictors)
  } else {
    list(ok = FALSE, df1 = NA_real_, F = NA_real_, p = NA_real_)
  }
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
    coefficients = coefs[, c("term", "estimate", "std_error", "t_value", "p_value", "partial_r2", "df",
                             "within_imputation_variance", "between_imputation_variance", "total_variance",
                             "relative_increase_variance", "fraction_missing_information"), drop = FALSE],
    coefficient_rows = coefficient_rows,
    summary = list(
      n_used = as.integer(round(mean(vapply(complete_by_imp, function(x) nrow(x$data), numeric(1L))))),
      n_excluded = length(rows_excluded),
      r_squared = mean(r_squared, na.rm = TRUE),
      adj_r_squared = mean(adj_r_squared, na.rm = TRUE),
      global_f = omnibus$F %||% NA_real_,
      df_model = if (is.finite(omnibus$df1 %||% NA_real_)) as.integer(omnibus$df1) else length(record$predictors),
      df_residual = min(vapply(valid_fits, stats::df.residual, numeric(1L)), na.rm = TRUE),
      global_p = omnibus$p %||% NA_real_,
      rmse = mean(residual_se, na.rm = TRUE),
      residual_se = mean(residual_se, na.rm = TRUE),
      aic = NA_real_,
      bic = NA_real_,
      fit_information_method = "R2 and residual SE are descriptive means across imputations; global model test uses a D1 pooled Wald test; AIC/BIC are not pooled."
    ),
    rows_used = rows_used,
    rows_excluded = rows_excluded,
    diagnostics = diagnostics,
    diagnostics_by_imputation = diagnostics_by_imp,
    multiple_imputation = .rls_mi_result_metadata(
      dataset,
      "linear_model",
      list(response = record$dependent, terms = record$predictors, formula = deparse(formula)),
      fits_by_imputation = fits,
      estimates_by_imputation = lapply(fits, function(fit) if (is.null(fit)) NULL else stats::coef(fit)),
      pooled_result = coefs,
      pooling_method = "Rubin's rules",
      warnings = c("Standardized coefficients are not pooled for multiple-imputation models.",
                   "Global model tests and information criteria are not pooled in this first MI-aware version."),
      complete_data_result_by_imputation = complete_by_imp,
      diagnostic_summary_by_imputation = diagnostics_by_imp
    ),
    status = sprintf(
      "Multiple imputation linear model fitted across %d imputations using Rubin's rules.",
      length(fits)
    )
  )
}

.rls_mi_fit_generalized_model_record <- function(record) {
  dataset <- .rls_dataset_record(record$group)
  formula <- .rls_generalized_glm_formula_object(record)
  complete_by_imp <- .rls_mi_fit_data_by_imputation(record, formula, record$scope %||% "all")
  family_object <- .rls_glm_make_family(record$family, record$link)
  fits <- lapply(complete_by_imp, function(complete) {
    if (nrow(complete$data) <= length(record$terms) + 1L) return(NULL)
    tryCatch(stats::glm(formula, data = complete$data, family = family_object), error = function(e) NULL)
  })
  if (!any(!vapply(fits, is.null, logical(1L)))) {
    stop("Not enough complete cases in the imputations to fit the generalized linear model.", call. = FALSE)
  }
  reference_fit <- fits[[which(!vapply(fits, is.null, logical(1L)))[[1L]]]]
  coefs <- .rls_mi_pool_coefficients(fits, statistic_name = "t")
  coef_display <- coefs[, c("term", "estimate", "std_error", "statistic", "p_value", "partial_r2", "df",
                            "within_imputation_variance", "between_imputation_variance", "total_variance",
                            "relative_increase_variance", "fraction_missing_information"), drop = FALSE]
  if (identical(record$family, "binomial") && identical(record$link, "logit")) {
    coef_display$odds_ratio <- exp(coef_display$estimate)
  }
  if (record$family %in% c("poisson", "quasipoisson") && identical(record$link, "log")) {
    coef_display$rate_ratio <- exp(coef_display$estimate)
  }
  coefficient_rows <- .rls_model_coefficient_display_rows(
    reference_fit,
    coef_display,
    dataset$data,
    record$terms,
    statistic_name = "t"
  )
  summaries <- lapply(fits, function(fit) if (is.null(fit)) NULL else summary(fit))
  valid_fits <- fits[!vapply(fits, is.null, logical(1L))]
  rows_used <- Reduce(intersect, lapply(complete_by_imp, `[[`, "original_rows"))
  rows_excluded <- setdiff(.rls_mi_original_row_ids(dataset), rows_used)
  diagnostics_by_imp <- lapply(seq_along(fits), function(i) {
    fit <- fits[[i]]
    if (is.null(fit)) return(data.frame())
    .rls_generalized_glm_diagnostics(record, fit, complete_by_imp[[i]])
  })
  diagnostics <- diagnostics_by_imp[[which(lengths(diagnostics_by_imp) > 0L)[[1L]]]]
  diagnostics$imputation_display <- "first fitted imputation"
  list(
    fit = reference_fit,
    fitted_glm = reference_fit,
    fits_by_imputation = fits,
    coefficients = coef_display,
    coefficient_rows = coefficient_rows,
    summary = list(
      n_used = as.integer(round(mean(vapply(complete_by_imp, function(x) nrow(x$data), numeric(1L))))),
      n_excluded = length(rows_excluded),
      null_deviance = mean(vapply(valid_fits, `[[`, numeric(1L), "null.deviance"), na.rm = TRUE),
      residual_deviance = mean(vapply(valid_fits, `[[`, numeric(1L), "deviance"), na.rm = TRUE),
      df_residual = min(vapply(valid_fits, stats::df.residual, numeric(1L)), na.rm = TRUE),
      aic = NA_real_,
      bic = NA_real_,
      dispersion = mean(vapply(summaries[!vapply(summaries, is.null, logical(1L))], `[[`, numeric(1L), "dispersion"), na.rm = TRUE),
      log_lik = NA_real_,
      family = record$family,
      link = record$link,
      fit_information_method = "Deviance and dispersion are descriptive means across imputations; AIC/BIC/logLik are not pooled."
    ),
    rows_used = rows_used,
    rows_excluded = rows_excluded,
    diagnostics = diagnostics,
    diagnostic_data = diagnostics,
    diagnostics_by_imputation = diagnostics_by_imp,
    multiple_imputation = .rls_mi_result_metadata(
      dataset,
      "generalized_linear_model",
      list(response = record$response, terms = record$terms, family = record$family, link = record$link, formula = deparse(formula)),
      fits_by_imputation = fits,
      estimates_by_imputation = lapply(fits, function(fit) if (is.null(fit)) NULL else stats::coef(fit)),
      pooled_result = coefs,
      pooling_method = "Rubin's rules on link-scale coefficients",
      warnings = c("Deviance, log-likelihood, AIC, and BIC are not pooled for MI GLMs in this first version."),
      complete_data_result_by_imputation = complete_by_imp,
      diagnostic_summary_by_imputation = diagnostics_by_imp
    ),
    status = sprintf(
      "Multiple imputation generalized linear model fitted across %d imputations using Rubin's rules.",
      length(fits)
    )
  )
}
