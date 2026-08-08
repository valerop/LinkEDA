.rls_compare_means_state <- new.env(parent = emptyenv())

.rls_compare_means_id <- function() {
  paste0("cm_", format(Sys.time(), "%Y%m%d%H%M%OS3"), "_", sample.int(1e6, 1L))
}

.rls_compare_means_record <- function(id) {
  id <- .rls_validate_protocol_name(id, "compare_means_result")
  if (!exists(id, envir = .rls_compare_means_state, inherits = FALSE)) {
    stop("Unknown compare-means result.", call. = FALSE)
  }
  get(id, envir = .rls_compare_means_state)
}

.rls_compare_means_assign <- function(record) {
  assign(record$result_id, record, envir = .rls_compare_means_state)
  invisible(record$result_id)
}

.rls_compare_means_adjust_results <- function(results, method = c("holm", "bonferroni", "none")) {
  method <- match.arg(method)
  p_values <- vapply(results, function(result) {
    value <- result$test_results$p_value %||% NA_real_
    if (length(value) == 1L && is.finite(value)) value else NA_real_
  }, numeric(1L))
  valid <- which(is.finite(p_values))
  adjusted <- rep(NA_real_, length(results))
  if (length(valid)) {
    adjusted[valid] <- if (identical(method, "none")) p_values[valid] else
      stats::p.adjust(p_values[valid], method = method)
  }
  for (i in seq_along(results)) {
    results[[i]]$test_results$p_adjusted <- adjusted[[i]]
    results[[i]]$test_results$p_adjustment <- method
    results[[i]]$test_results$adjustment_family_size <- length(valid)
  }
  results
}

.rls_compare_means_failed_result <- function(dataset, analysis_type, specification,
                                              message, original_ids) {
  groups <- specification$group_levels %||% character()
  descriptives <- switch(analysis_type,
    one_sample_t_test = list(n = NA_real_, mean = NA_real_, sd = NA_real_, se = NA_real_),
    independent_samples_t_test = list(
      group = groups, n = rep(NA_real_, length(groups)), mean = rep(NA_real_, length(groups)),
      sd = rep(NA_real_, length(groups)), se = rep(NA_real_, length(groups)),
      rows_original_ids = rep(list(integer()), length(groups))
    ),
    paired_samples_t_test = list(
      n = c(NA_real_, NA_real_), mean = c(NA_real_, NA_real_), sd = c(NA_real_, NA_real_),
      differences = list(n = NA_real_, mean = NA_real_, sd = NA_real_, se = NA_real_)
    ),
    one_way_anova = list(
      group = groups, n = rep(NA_real_, length(groups)), mean = rep(NA_real_, length(groups)),
      sd = rep(NA_real_, length(groups)), se = rep(NA_real_, length(groups)),
      rows_original_ids = rep(list(integer()), length(groups))
    ),
    list()
  )
  effect_label <- switch(analysis_type,
    one_sample_t_test = "Cohen's d",
    independent_samples_t_test = "Hedges' g",
    paired_samples_t_test = "Cohen's dz",
    one_way_anova = "Omega squared",
    "Effect size"
  )
  list(
    result_id = .rls_compare_means_id(),
    dataset_id = dataset$dataset_id %||% dataset$group,
    analysis_type = analysis_type,
    analysis_backend = "ordinary",
    specification = specification,
    descriptives = descriptives,
    test_results = list(
      method = specification$method %||% "",
      statistic = NA_real_, parameter = c(NA_real_, NA_real_), p_value = NA_real_,
      conf_int = c(NA_real_, NA_real_), conf_level = specification$conf_level %||% 0.95,
      alternative = specification$alternative %||% "two.sided",
      mean_diff = NA_real_, mean_diff_se = NA_real_
    ),
    effect_sizes = list(effect_size_label = effect_label),
    post_hoc = NULL,
    rows_used_original_ids = original_ids,
    rows_excluded_original_ids = integer(),
    warnings = as.character(message),
    multiple_imputation = NULL,
    result_version = 1L,
    created_at = Sys.time()
  )
}

.rls_compare_means_require_any_estimable <- function(results) {
  estimable <- vapply(results, function(result) {
    p <- result$test_results$p_value %||% NA_real_
    length(p) == 1L && is.finite(p)
  }, logical(1L))
  if (!any(estimable)) {
    stop(results[[1L]]$warnings[[1L]] %||% "No requested comparison was estimable.", call. = FALSE)
  }
  results
}

.rls_compare_means_complete_data <- function(data, variables, scope = "all", selected_rows = integer()) {
  vars <- unique(as.character(variables))
  missing_data <- data[, vars, drop = FALSE]
  ok <- stats::complete.cases(missing_data)
  if (identical(scope, "selected")) {
    keep <- seq_len(nrow(data)) %in% selected_rows
    ok <- ok & keep
  } else if (identical(scope, "unselected")) {
    keep <- !seq_len(nrow(data)) %in% selected_rows
    ok <- ok & keep
  }
  list(data = data[ok, , drop = FALSE], rows = which(ok), excluded = sum(!ok), scope = scope)
}

.rls_compare_means_validate_responses <- function(data, responses) {
  if (!length(responses)) stop("At least one response variable is required.", call. = FALSE)
  for (i in seq_along(responses)) {
    responses[[i]] <- .rls_validate_protocol_name(responses[[i]], "response")
    if (!responses[[i]] %in% names(data)) {
      stop(sprintf("Response variable `%s` was not found.", responses[[i]]), call. = FALSE)
    }
    if (!is.numeric(data[[responses[[i]]]])) {
      stop(sprintf("Response variable `%s` must be numeric.", responses[[i]]), call. = FALSE)
    }
  }
  unique(responses)
}

.rls_compare_means_validate_group <- function(data, group) {
  group <- .rls_validate_protocol_name(group, "group")
  if (!group %in% names(data)) {
    stop(sprintf("Grouping variable `%s` was not found.", group), call. = FALSE)
  }
  group
}

.rls_compare_means_group_factor <- function(data, group, group_order = NULL) {
  x <- data[[group]]
  f <- if (is.factor(x)) droplevels(x) else factor(x)
  if (length(group_order)) {
    observed <- levels(f)
    requested <- as.character(group_order)
    if (!setequal(observed, requested)) {
      stop("Group order must contain every effective group exactly once.", call. = FALSE)
    }
    f <- factor(as.character(f), levels = requested)
  }
  if (nlevels(f) < 2L) {
    stop(sprintf("Grouping variable `%s` must have at least two levels.", group), call. = FALSE)
  }
  f
}

.rls_compare_means_validate_options <- function(conf_level, numeric_value = NULL) {
  conf_level <- suppressWarnings(as.numeric(conf_level))
  if (length(conf_level) != 1L || !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("Confidence level must be a finite number between 0 and 1.", call. = FALSE)
  }
  if (!is.null(numeric_value)) {
    numeric_value <- suppressWarnings(as.numeric(numeric_value))
    if (length(numeric_value) != 1L || !is.finite(numeric_value)) {
      stop("Test value must be numeric and finite.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

.rls_signed_rank_biserial <- function(differences) {
  differences <- differences[is.finite(differences) & differences != 0]
  if (!length(differences)) return(NA_real_)
  ranks <- rank(abs(differences), ties.method = "average")
  positive <- sum(ranks[differences > 0])
  negative <- sum(ranks[differences < 0])
  denominator <- positive + negative
  if (denominator > 0) (positive - negative) / denominator else NA_real_
}

.rls_wilcox_with_interval <- function(x, y = NULL, mu = 0, paired = FALSE,
                                      alternative = "two.sided", conf_level = 0.95) {
  arguments <- list(x = x, mu = mu, paired = paired, alternative = alternative,
                    conf.int = TRUE, conf.level = conf_level, exact = FALSE)
  if (!is.null(y)) arguments$y <- y
  tryCatch(
    do.call(stats::wilcox.test, arguments),
    error = function(e) {
      arguments$conf.int <- FALSE
      result <- do.call(stats::wilcox.test, arguments)
      result$conf.int <- c(NA_real_, NA_real_)
      result$estimate <- NA_real_
      result
    }
  )
}

.rls_one_sample_t_test_ordinary <- function(data, response, complete, mu, alternative, conf_level, original_ids) {
  y <- complete$data[[response]]
  n <- length(y)
  if (n < 2L) stop("The selected variable has fewer than two valid observations.", call. = FALSE)
  mn <- mean(y)
  s <- stats::sd(y)
  if (!is.finite(s) || s <= 0) stop("The variance is zero, so the t statistic cannot be calculated.", call. = FALSE)
  se <- s / sqrt(n)
  diff <- mn - mu
  test <- stats::t.test(y, mu = mu, alternative = alternative, conf.level = conf_level)
  t_stat <- unname(test$statistic)
  df <- unname(test$parameter)
  p <- test$p.value
  ci <- unname(test$conf.int) - mu
  t_crit <- stats::qt(1 - (1 - conf_level) / 2, df = df)
  d <- diff / s
  rows_used <- original_ids[complete$rows]
  rows_excluded <- setdiff(original_ids, rows_used)
  list(
    result_id = .rls_compare_means_id(),
    dataset_id = data$dataset_id %||% data$group,
    analysis_type = "one_sample_t_test",
    analysis_backend = "ordinary",
    specification = list(
      response = response,
      null_mu = mu,
      alternative = alternative,
      conf_level = conf_level,
      scope = complete$scope %||% "all"
    ),
    descriptives = list(
      n = n,
      mean = mn,
      sd = s,
      se = se
    ),
    test_results = list(
      method = "One-sample t-test",
      statistic = t_stat,
      parameter = df,
      p_value = p,
      conf_int = ci,
      conf_level = conf_level,
      alternative = alternative,
      null_value = mu,
      mean_diff = diff,
      mean_diff_se = se
    ),
    effect_sizes = list(
      cohens_d = d,
      effect_size_label = "Cohen's d",
      effect_size_ci = d + c(-1, 1) * t_crit * sqrt(1 / n + d^2 / (2 * n))
    ),
    post_hoc = NULL,
    rows_used_original_ids = rows_used,
    rows_excluded_original_ids = rows_excluded,
    multiple_imputation = NULL,
    result_version = 1L,
    created_at = Sys.time()
  )
}

.rls_one_sample_wilcoxon_ordinary <- function(data, response, complete, mu, alternative,
                                               conf_level, original_ids) {
  y <- complete$data[[response]]
  n <- length(y)
  if (n < 2L) stop("The selected variable has fewer than two valid observations.", call. = FALSE)
  test <- .rls_wilcox_with_interval(y, mu = mu, alternative = alternative,
                                    conf_level = conf_level)
  estimate <- unname(test$estimate %||% NA_real_)
  shift <- if (length(estimate) && is.finite(estimate[[1L]])) estimate[[1L]] - mu else NA_real_
  interval <- unname(test$conf.int %||% c(NA_real_, NA_real_)) - mu
  rows_used <- original_ids[complete$rows]
  rows_excluded <- setdiff(original_ids, rows_used)
  list(
    result_id = .rls_compare_means_id(),
    dataset_id = data$dataset_id %||% data$group,
    analysis_type = "one_sample_t_test",
    analysis_backend = "ordinary",
    specification = list(response = response, null_mu = mu, alternative = alternative,
                         conf_level = conf_level, scope = complete$scope %||% "all",
                         method = "wilcoxon"),
    descriptives = list(n = n, mean = mean(y), sd = stats::sd(y),
                        se = stats::sd(y) / sqrt(n)),
    test_results = list(
      method = "Wilcoxon signed-rank test", statistic = unname(test$statistic),
      parameter = NA_real_, p_value = test$p.value, conf_int = interval,
      conf_level = conf_level, alternative = alternative, null_value = mu,
      mean_diff = shift, mean_diff_se = NA_real_
    ),
    effect_sizes = list(rank_biserial = .rls_signed_rank_biserial(y - mu),
                        effect_size_label = "Rank-biserial r"),
    post_hoc = NULL, rows_used_original_ids = rows_used,
    rows_excluded_original_ids = rows_excluded, multiple_imputation = NULL,
    result_version = 1L, created_at = Sys.time()
  )
}

.rls_one_sample_t_test_mi <- function(record, dataset, response, mu, alternative, conf_level, scope, original_ids) {
  completed <- .rls_mi_completed_datasets(dataset)
  estimates <- numeric()
  variances <- numeric()
  rows_used <- integer()
  rows_excluded <- integer()
  for (data_imp in completed) {
    complete <- .rls_compare_means_complete_data(data_imp, response, scope, record$selected_rows %||% integer())
    y <- complete$data[[response]]
    n <- length(y)
    if (n < 2L) {
      estimates <- c(estimates, NA_real_)
      variances <- c(variances, NA_real_)
      next
    }
    mn <- mean(y)
    s <- stats::sd(y)
    diff <- mn - mu
    estimates <- c(estimates, diff)
    variances <- c(variances, s^2 / n)
    rows_used <- union(rows_used, original_ids[complete$rows])
    rows_excluded <- union(rows_excluded, setdiff(original_ids, original_ids[complete$rows]))
  }
  pool <- .rls_mi_pool_scalar(estimates, variances, conf_level = conf_level)
  df_complete <- min(vapply(completed, function(d) {
    cc <- .rls_compare_means_complete_data(d, response, scope, record$selected_rows %||% integer())
    length(cc$rows) - 1L
  }, numeric(1L)), na.rm = TRUE)
  if (is.finite(df_complete) && df_complete > 0) {
    pool <- .rls_mi_pool_scalar(estimates, variances, df_complete = df_complete, conf_level = conf_level)
  }
  if (identical(alternative, "greater")) {
    p <- stats::pt(pool$statistic, df = pool$df, lower.tail = FALSE)
    crit <- stats::qt(conf_level, df = pool$df)
    ci <- c(pool$Qbar - crit * pool$SE, Inf)
  } else if (identical(alternative, "less")) {
    p <- stats::pt(pool$statistic, df = pool$df, lower.tail = TRUE)
    crit <- stats::qt(conf_level, df = pool$df)
    ci <- c(-Inf, pool$Qbar + crit * pool$SE)
  } else {
    p <- pool$p
    ci <- c(pool$CI_low, pool$CI_high)
  }
  mi_meta <- .rls_mi_result_metadata(
    dataset, "one_sample_t_test",
    list(response = response, null_mu = mu, alternative = alternative, conf_level = conf_level),
    estimates_by_imputation = estimates,
    pooled_result = pool
  )
  list(
    result_id = .rls_compare_means_id(),
    dataset_id = dataset$dataset_id %||% dataset$group,
    analysis_type = "one_sample_t_test",
    analysis_backend = "multiple_imputation",
    specification = list(
      response = response,
      null_mu = mu,
      alternative = alternative,
      conf_level = conf_level,
      scope = scope
    ),
    descriptives = list(
      pooled_mean = pool$Qbar + mu,
      pooled_se = pool$SE,
      n_imputations = pool$m,
      note = "Descriptive mean is reconstructed from pooled difference; SD is not reliably pooled."
    ),
    test_results = list(
      method = "One-sample t-test (MI pooled)",
      statistic = pool$statistic,
      parameter = pool$df,
      p_value = p,
      conf_int = ci,
      conf_level = conf_level,
      alternative = alternative,
      null_value = mu,
      mean_diff = pool$Qbar,
      mean_diff_se = pool$SE,
      pooling_method = "Rubin's rules"
    ),
    effect_sizes = list(
      cohens_d = NA_real_,
      effect_size_label = "Cohen's d",
      note = "Effect size is not pooled for multiple-imputation analyses in this version."
    ),
    post_hoc = NULL,
    rows_used_original_ids = rows_used,
    rows_excluded_original_ids = rows_excluded,
    multiple_imputation = mi_meta,
    result_version = 1L,
    created_at = Sys.time()
  )
}

.rls_independent_samples_t_test_ordinary <- function(data, response, group_factor, complete, alternative, conf_level, var_equal, original_ids, group_name = "group") {
  gf <- droplevels(group_factor[complete$rows])
  levs <- levels(gf)
  y <- complete$data[[response]]
  g1 <- y[gf == levs[[1L]]]
  g2 <- y[gf == levs[[2L]]]
  n1 <- length(g1); n2 <- length(g2)
  if (n1 < 2L || n2 < 2L) stop("Each effective group must contain at least two valid observations.", call. = FALSE)
  m1 <- mean(g1); m2 <- mean(g2)
  s1 <- stats::sd(g1); s2 <- stats::sd(g2)
  if (!is.finite(s1) || !is.finite(s2) || s1 <= 0 || s2 <= 0) {
    stop("The variance is zero in at least one group, so the t statistic cannot be calculated.", call. = FALSE)
  }
  diff <- m1 - m2
  test <- stats::t.test(g1, g2, alternative = alternative, var.equal = isTRUE(var_equal), conf.level = conf_level)
  t_stat <- unname(test$statistic)
  se <- if (isTRUE(var_equal)) {
    pooled <- ((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2)
    sqrt(pooled * (1 / n1 + 1 / n2))
  } else sqrt(s1^2 / n1 + s2^2 / n2)
  df <- unname(test$parameter)
  method <- if (isTRUE(var_equal)) "Student's t-test (equal variance)" else "Welch t-test"
  p <- test$p.value
  ci <- unname(test$conf.int)
  sp <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
  d <- diff / sp
  correction <- 1 - 3 / (4 * (n1 + n2) - 9)
  g <- correction * d
  d_se <- sqrt((n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2)))
  rows_used <- original_ids[complete$rows]
  rows_excluded <- setdiff(original_ids, rows_used)
  rows_by_group <- lapply(levs, function(level) original_ids[complete$rows[gf == level]])
  list(
    result_id = .rls_compare_means_id(),
    dataset_id = data$dataset_id %||% data$group,
    analysis_type = "independent_samples_t_test",
    analysis_backend = "ordinary",
    specification = list(
      response = response,
      group = group_name,
      group_levels = levs,
      alternative = alternative,
      conf_level = conf_level,
      var_equal = isTRUE(var_equal),
      scope = complete$scope %||% "all"
    ),
    descriptives = list(
      group = levs,
      n = c(n1, n2),
      mean = c(m1, m2),
      sd = c(s1, s2),
      se = c(s1 / sqrt(n1), s2 / sqrt(n2)),
      rows_original_ids = rows_by_group
    ),
    test_results = list(
      method = method,
      statistic = t_stat,
      parameter = df,
      p_value = p,
      conf_int = ci,
      conf_level = conf_level,
      alternative = alternative,
      mean_diff = diff,
      mean_diff_se = se,
      group_reference = levs[[1L]],
      group_comparison = levs[[2L]],
      direction_note = sprintf("Mean difference: %s - %s", levs[[1L]], levs[[2L]])
    ),
    effect_sizes = list(
      cohens_d = d,
      cohens_d_se = d_se,
      hedges_g = g,
      effect_size_label = "Hedges' g"
    ),
    post_hoc = NULL,
    rows_used_original_ids = rows_used,
    rows_excluded_original_ids = rows_excluded,
    multiple_imputation = NULL,
    result_version = 1L,
    created_at = Sys.time()
  )
}

.rls_independent_samples_mann_whitney_ordinary <- function(data, response, group_factor,
                                                            complete, alternative, conf_level,
                                                            original_ids, group_name = "group") {
  gf <- droplevels(group_factor[complete$rows])
  levs <- levels(gf)
  y <- complete$data[[response]]
  g1 <- y[gf == levs[[1L]]]
  g2 <- y[gf == levs[[2L]]]
  n1 <- length(g1); n2 <- length(g2)
  if (n1 < 1L || n2 < 1L) stop("Each effective group must contain at least one valid observation.", call. = FALSE)
  test <- .rls_wilcox_with_interval(g1, g2, alternative = alternative,
                                    conf_level = conf_level)
  u <- unname(test$statistic)
  estimate <- unname(test$estimate %||% NA_real_)
  shift <- if (length(estimate) && is.finite(estimate[[1L]])) estimate[[1L]] else NA_real_
  interval <- unname(test$conf.int %||% c(NA_real_, NA_real_))
  rows_used <- original_ids[complete$rows]
  rows_excluded <- setdiff(original_ids, rows_used)
  rows_by_group <- lapply(levs, function(level) original_ids[complete$rows[gf == level]])
  list(
    result_id = .rls_compare_means_id(),
    dataset_id = data$dataset_id %||% data$group,
    analysis_type = "independent_samples_t_test", analysis_backend = "ordinary",
    specification = list(response = response, group = group_name, group_levels = levs,
                         alternative = alternative, conf_level = conf_level,
                         var_equal = FALSE, method = "mann_whitney",
                         scope = complete$scope %||% "all"),
    descriptives = list(
      group = levs, n = c(n1, n2), mean = c(mean(g1), mean(g2)),
      sd = c(stats::sd(g1), stats::sd(g2)),
      se = c(stats::sd(g1) / sqrt(n1), stats::sd(g2) / sqrt(n2)),
      rows_original_ids = rows_by_group
    ),
    test_results = list(
      method = "Mann–Whitney U test", statistic = u, parameter = NA_real_,
      p_value = test$p.value, conf_int = interval, conf_level = conf_level,
      alternative = alternative, mean_diff = shift, mean_diff_se = NA_real_,
      group_reference = levs[[1L]], group_comparison = levs[[2L]],
      direction_note = sprintf("Location shift: %s - %s", levs[[1L]], levs[[2L]])
    ),
    effect_sizes = list(rank_biserial = 2 * u / (n1 * n2) - 1,
                        effect_size_label = "Rank-biserial r"),
    post_hoc = NULL, rows_used_original_ids = rows_used,
    rows_excluded_original_ids = rows_excluded, multiple_imputation = NULL,
    result_version = 1L, created_at = Sys.time()
  )
}

.rls_independent_samples_t_test_mi <- function(record, dataset, response, group, alternative, conf_level, var_equal, scope, original_ids) {
  completed <- .rls_mi_completed_datasets(dataset)
  fits <- list()
  estimates <- numeric()
  variances <- numeric()
  rows_used <- integer()
  rows_excluded <- integer()
  group_name <- group
  for (data_imp in completed) {
    complete <- .rls_compare_means_complete_data(data_imp, c(response, group), scope, record$selected_rows %||% integer())
    gf <- .rls_compare_means_group_factor(complete$data, group)
    levs <- levels(gf)
    df_imp <- data.frame(y = complete$data[[response]], g = gf)
    ok <- stats::complete.cases(df_imp)
    if (sum(ok) < 4L || length(unique(df_imp$g[ok])) < 2L) {
      estimates <- c(estimates, NA_real_)
      variances <- c(variances, NA_real_)
      fits[[length(fits) + 1L]] <- NULL
      next
    }
    fit <- stats::lm(y ~ g, data = df_imp[ok, , drop = FALSE])
    fits[[length(fits) + 1L]] <- fit
    coef_names <- setdiff(names(stats::coef(fit)), "(Intercept)")
    if (length(coef_names)) {
      estimates <- c(estimates, unname(stats::coef(fit)[[coef_names[[1L]]]]))
      variances <- c(variances, unname(stats::vcov(fit)[coef_names[[1L]], coef_names[[1L]]]))
    }
    rows_used <- union(rows_used, original_ids[complete$rows[ok]])
    rows_excluded <- union(rows_excluded, setdiff(original_ids, original_ids[complete$rows[ok]]))
  }
  if (length(estimates) < 1L || all(is.na(estimates))) {
    stop("No valid fits across imputations for the independent-samples t-test.", call. = FALSE)
  }
  pool <- .rls_mi_pool_scalar(estimates, variances, conf_level = conf_level)
  valid_fits <- fits[!vapply(fits, is.null, logical(1L))]
  if (length(valid_fits)) {
    df_complete <- min(vapply(valid_fits, stats::df.residual, numeric(1L)), na.rm = TRUE)
    if (is.finite(df_complete) && df_complete > 0) {
      pool <- .rls_mi_pool_scalar(estimates, variances, df_complete = df_complete, conf_level = conf_level)
    }
  }
  if (identical(alternative, "greater")) {
    p <- stats::pt(pool$statistic, df = pool$df, lower.tail = FALSE)
    crit <- stats::qt(conf_level, df = pool$df)
    ci <- c(pool$Qbar - crit * pool$SE, Inf)
  } else if (identical(alternative, "less")) {
    p <- stats::pt(pool$statistic, df = pool$df, lower.tail = TRUE)
    crit <- stats::qt(conf_level, df = pool$df)
    ci <- c(-Inf, pool$Qbar + crit * pool$SE)
  } else {
    p <- pool$p
    ci <- c(pool$CI_low, pool$CI_high)
  }
  mi_meta <- .rls_mi_result_metadata(
    dataset, "independent_samples_t_test",
    list(response = response, group = group, alternative = alternative, conf_level = conf_level),
    fits_by_imputation = fits,
    estimates_by_imputation = estimates,
    pooled_result = pool
  )
  list(
    result_id = .rls_compare_means_id(),
    dataset_id = dataset$dataset_id %||% dataset$group,
    analysis_type = "independent_samples_t_test",
    analysis_backend = "multiple_imputation",
    specification = list(
      response = response,
      group = group,
      alternative = alternative,
      conf_level = conf_level,
      scope = scope
    ),
    descriptives = list(
      note = "Descriptive statistics are not pooled in this version; see per-imputation results.",
      n_imputations = pool$m
    ),
    test_results = list(
      method = "Independent-samples t-test (MI pooled)",
      statistic = pool$statistic,
      parameter = pool$df,
      p_value = p,
      conf_int = ci,
      conf_level = conf_level,
      alternative = alternative,
      mean_diff = pool$Qbar,
      mean_diff_se = pool$SE,
      pooling_method = "Rubin's rules",
      direction_note = "Pooled mean difference (group coefficient)"
    ),
    effect_sizes = list(
      cohens_d = NA_real_,
      note = "Effect size is not pooled for MI analyses in this version."
    ),
    post_hoc = NULL,
    rows_used_original_ids = rows_used,
    rows_excluded_original_ids = rows_excluded,
    multiple_imputation = mi_meta,
    result_version = 1L,
    created_at = Sys.time()
  )
}

.rls_paired_samples_t_test_ordinary <- function(data, response1, response2, complete, alternative, conf_level, original_ids) {
  d <- complete$data[[response1]] - complete$data[[response2]]
  n <- length(d)
  if (n < 2L) stop("The selected pair has fewer than two complete observations.", call. = FALSE)
  mn <- mean(d)
  s <- stats::sd(d)
  if (!is.finite(s) || s <= 0) stop("The variance of the paired differences is zero, so the t statistic cannot be calculated.", call. = FALSE)
  se <- s / sqrt(n)
  test <- stats::t.test(complete$data[[response1]], complete$data[[response2]], paired = TRUE,
                        alternative = alternative, conf.level = conf_level)
  t_stat <- unname(test$statistic)
  df <- unname(test$parameter)
  p <- test$p.value
  ci <- unname(test$conf.int)
  t_crit <- stats::qt(1 - (1 - conf_level) / 2, df = df)
  dz <- mn / s
  rows_used <- original_ids[complete$rows]
  rows_excluded <- setdiff(original_ids, rows_used)
  y1 <- complete$data[[response1]]
  y2 <- complete$data[[response2]]
  list(
    result_id = .rls_compare_means_id(),
    dataset_id = data$dataset_id %||% data$group,
    analysis_type = "paired_samples_t_test",
    analysis_backend = "ordinary",
    specification = list(
      response1 = response1,
      response2 = response2,
      difference = paste(response1, "-", response2),
      alternative = alternative,
      conf_level = conf_level,
      scope = complete$scope %||% "all"
    ),
    descriptives = list(
      variable = c(response1, response2),
      n = c(length(y1), length(y2)),
      mean = c(mean(y1), mean(y2)),
      sd = c(stats::sd(y1), stats::sd(y2)),
      differences = list(
        n = n,
        mean = mn,
        sd = s,
        se = se
      )
    ),
    test_results = list(
      method = "Paired t-test",
      statistic = t_stat,
      parameter = df,
      p_value = p,
      conf_int = ci,
      conf_level = conf_level,
      alternative = alternative,
      mean_diff = mn,
      mean_diff_se = se,
      direction_note = sprintf("Mean difference: %s - %s", response1, response2)
    ),
    effect_sizes = list(
      cohens_dz = dz,
      effect_size_label = "Cohen's dz",
      effect_size_ci = dz + c(-1, 1) * t_crit * sqrt(1 / n + dz^2 / (2 * n))
    ),
    post_hoc = NULL,
    rows_used_original_ids = rows_used,
    rows_excluded_original_ids = rows_excluded,
    multiple_imputation = NULL,
    result_version = 1L,
    created_at = Sys.time()
  )
}

.rls_paired_samples_wilcoxon_ordinary <- function(data, response1, response2, complete,
                                                   alternative, conf_level, original_ids) {
  y1 <- complete$data[[response1]]
  y2 <- complete$data[[response2]]
  d <- y1 - y2
  n <- length(d)
  if (n < 2L) stop("The selected pair has fewer than two complete observations.", call. = FALSE)
  test <- .rls_wilcox_with_interval(y1, y2, paired = TRUE,
                                    alternative = alternative, conf_level = conf_level)
  estimate <- unname(test$estimate %||% NA_real_)
  shift <- if (length(estimate) && is.finite(estimate[[1L]])) estimate[[1L]] else NA_real_
  interval <- unname(test$conf.int %||% c(NA_real_, NA_real_))
  rows_used <- original_ids[complete$rows]
  rows_excluded <- setdiff(original_ids, rows_used)
  list(
    result_id = .rls_compare_means_id(), dataset_id = data$dataset_id %||% data$group,
    analysis_type = "paired_samples_t_test", analysis_backend = "ordinary",
    specification = list(response1 = response1, response2 = response2,
                         difference = paste(response1, "-", response2),
                         alternative = alternative, conf_level = conf_level,
                         method = "wilcoxon", scope = complete$scope %||% "all"),
    descriptives = list(
      variable = c(response1, response2), n = c(n, n),
      mean = c(mean(y1), mean(y2)), sd = c(stats::sd(y1), stats::sd(y2)),
      differences = list(n = n, mean = mean(d), sd = stats::sd(d),
                         se = stats::sd(d) / sqrt(n))
    ),
    test_results = list(
      method = "Paired Wilcoxon signed-rank test", statistic = unname(test$statistic),
      parameter = NA_real_, p_value = test$p.value, conf_int = interval,
      conf_level = conf_level, alternative = alternative, mean_diff = shift,
      mean_diff_se = NA_real_,
      direction_note = sprintf("Location shift: %s - %s", response1, response2)
    ),
    effect_sizes = list(rank_biserial = .rls_signed_rank_biserial(d),
                        effect_size_label = "Rank-biserial r"),
    post_hoc = NULL, rows_used_original_ids = rows_used,
    rows_excluded_original_ids = rows_excluded, multiple_imputation = NULL,
    result_version = 1L, created_at = Sys.time()
  )
}

.rls_paired_samples_t_test_mi <- function(record, dataset, response1, response2, alternative, conf_level, scope, original_ids) {
  completed <- .rls_mi_completed_datasets(dataset)
  estimates <- numeric()
  variances <- numeric()
  rows_used <- integer()
  rows_excluded <- integer()
  for (data_imp in completed) {
    complete <- .rls_compare_means_complete_data(data_imp, c(response1, response2), scope, record$selected_rows %||% integer())
    d <- complete$data[[response1]] - complete$data[[response2]]
    n <- length(d)
    if (n < 2L) {
      estimates <- c(estimates, NA_real_)
      variances <- c(variances, NA_real_)
      next
    }
    mn <- mean(d)
    s <- stats::sd(d)
    estimates <- c(estimates, mn)
    variances <- c(variances, s^2 / n)
    rows_used <- union(rows_used, original_ids[complete$rows])
    rows_excluded <- union(rows_excluded, setdiff(original_ids, original_ids[complete$rows]))
  }
  if (length(estimates) < 1L || all(is.na(estimates))) {
    stop("No valid paired differences across imputations.", call. = FALSE)
  }
  df_complete <- min(vapply(completed, function(d) {
    cc <- .rls_compare_means_complete_data(d, c(response1, response2), scope, record$selected_rows %||% integer())
    length(cc$rows) - 1L
  }, numeric(1L)), na.rm = TRUE)
  pool <- .rls_mi_pool_scalar(estimates, variances, df_complete = df_complete, conf_level = conf_level)
  if (identical(alternative, "greater")) {
    p <- stats::pt(pool$statistic, df = pool$df, lower.tail = FALSE)
    crit <- stats::qt(conf_level, df = pool$df)
    ci <- c(pool$Qbar - crit * pool$SE, Inf)
  } else if (identical(alternative, "less")) {
    p <- stats::pt(pool$statistic, df = pool$df, lower.tail = TRUE)
    crit <- stats::qt(conf_level, df = pool$df)
    ci <- c(-Inf, pool$Qbar + crit * pool$SE)
  } else {
    p <- pool$p
    ci <- c(pool$CI_low, pool$CI_high)
  }
  mi_meta <- .rls_mi_result_metadata(
    dataset, "paired_samples_t_test",
    list(response1 = response1, response2 = response2, alternative = alternative, conf_level = conf_level),
    estimates_by_imputation = estimates,
    pooled_result = pool
  )
  list(
    result_id = .rls_compare_means_id(),
    dataset_id = dataset$dataset_id %||% dataset$group,
    analysis_type = "paired_samples_t_test",
    analysis_backend = "multiple_imputation",
    specification = list(
      response1 = response1,
      response2 = response2,
      difference = paste(response1, "-", response2),
      alternative = alternative,
      conf_level = conf_level,
      scope = scope
    ),
    descriptives = list(
      note = "Descriptive statistics are not pooled per-variable in this version.",
      pooled_mean_diff = pool$Qbar,
      pooled_se_diff = pool$SE,
      n_imputations = pool$m
    ),
    test_results = list(
      method = "Paired t-test (MI pooled)",
      statistic = pool$statistic,
      parameter = pool$df,
      p_value = p,
      conf_int = ci,
      conf_level = conf_level,
      alternative = alternative,
      mean_diff = pool$Qbar,
      mean_diff_se = pool$SE,
      pooling_method = "Rubin's rules",
      direction_note = sprintf("Pooled mean difference: %s - %s", response1, response2)
    ),
    effect_sizes = list(
      cohens_dz = NA_real_,
      note = "Effect size is not pooled for MI analyses in this version."
    ),
    post_hoc = NULL,
    rows_used_original_ids = rows_used,
    rows_excluded_original_ids = rows_excluded,
    multiple_imputation = mi_meta,
    result_version = 1L,
    created_at = Sys.time()
  )
}

.rls_one_way_anova_ordinary <- function(data, response, group_factor, complete, alternative, conf_level, original_ids, group_name = "group", method = "welch") {
  gf <- droplevels(group_factor[complete$rows])
  levs <- levels(gf)
  y <- complete$data[[response]]
  k <- nlevels(gf)
  n_total <- length(y)
  group_stats <- lapply(levs, function(lv) {
    vals <- y[gf == lv]
    c(n = length(vals), mean = mean(vals), sd = stats::sd(vals), se = stats::sd(vals) / sqrt(length(vals)))
  })
  group_stats <- do.call(rbind, group_stats)
  rownames(group_stats) <- levs
  if (k < 2L) stop("One-way ANOVA requires at least two effective groups.", call. = FALSE)
  if (!identical(method, "kruskal_wallis") &&
      (any(group_stats[, "n"] < 2L) || any(!is.finite(group_stats[, "sd"])) || any(group_stats[, "sd"] <= 0))) {
    stop("Each group must contain at least two valid observations with non-zero variance.", call. = FALSE)
  }
  fit <- if (identical(method, "kruskal_wallis")) stats::kruskal.test(y, gf) else
    stats::oneway.test(y ~ gf, var.equal = identical(method, "classical"))
  f_stat <- unname(fit$statistic)
  df1 <- unname(fit$parameter[[1L]])
  df2 <- if (identical(method, "kruskal_wallis")) NA_real_ else unname(fit$parameter[[2L]])
  p <- fit$p.value
  grand_mean <- mean(y)
  ss_between <- sum(group_stats[, "n"] * (group_stats[, "mean"] - grand_mean)^2)
  ss_within <- sum(vapply(levs, function(level) sum((y[gf == level] - mean(y[gf == level]))^2), numeric(1L)))
  ms_within <- ss_within / (n_total - k)
  omega_squared <- if (identical(method, "kruskal_wallis")) NA_real_ else
    max(0, (ss_between - (k - 1) * ms_within) / (ss_between + ss_within + ms_within))
  epsilon_squared <- if (identical(method, "kruskal_wallis") && n_total > k)
    max(0, (f_stat - k + 1) / (n_total - k)) else NA_real_
  rows_used <- original_ids[complete$rows]
  rows_excluded <- setdiff(original_ids, rows_used)
  rows_by_group <- lapply(levs, function(level) original_ids[complete$rows[gf == level]])
  list(
    result_id = .rls_compare_means_id(),
    dataset_id = data$dataset_id %||% data$group,
    analysis_type = "one_way_anova",
    analysis_backend = "ordinary",
    specification = list(
      response = response,
      group = group_name,
      group_levels = levs,
      method = method,
      scope = complete$scope %||% "all"
    ),
    descriptives = list(
      group = levs,
      n = group_stats[, "n"],
      mean = group_stats[, "mean"],
      sd = group_stats[, "sd"],
      se = group_stats[, "se"],
      rows_original_ids = rows_by_group
    ),
    test_results = list(
      method = if (identical(method, "kruskal_wallis")) "Kruskal–Wallis test" else
        if (identical(method, "classical")) "Classical one-way ANOVA" else "Welch one-way ANOVA",
      statistic = f_stat,
      parameter = c(df1 = df1, df2 = df2),
      p_value = p,
      conf_level = conf_level
    ),
    effect_sizes = if (identical(method, "kruskal_wallis"))
      list(epsilon_squared = epsilon_squared, effect_size_label = "Epsilon squared") else
      list(omega_squared = omega_squared, effect_size_label = "Omega squared"),
    post_hoc = NULL,
    rows_used_original_ids = rows_used,
    rows_excluded_original_ids = rows_excluded,
    multiple_imputation = NULL,
    result_version = 1L,
    created_at = Sys.time()
  )
}

.rls_one_way_anova_mi <- function(record, dataset, response, group, alternative, conf_level, scope, original_ids) {
  completed <- .rls_mi_completed_datasets(dataset)
  fits <- list()
  rows_used <- integer()
  rows_excluded <- integer()
  group_name <- group
  for (data_imp in completed) {
    complete <- .rls_compare_means_complete_data(data_imp, c(response, group), scope, record$selected_rows %||% integer())
    gf <- .rls_compare_means_group_factor(complete$data, group)
    levs <- levels(gf)
    df_imp <- data.frame(y = complete$data[[response]], g = gf)
    ok <- stats::complete.cases(df_imp)
    if (sum(ok) < (length(levs) + 1L) || length(unique(df_imp$g[ok])) < 2L) {
      fits[[length(fits) + 1L]] <- NULL
      next
    }
    fit <- stats::lm(y ~ g, data = df_imp[ok, , drop = FALSE])
    fits[[length(fits) + 1L]] <- fit
    rows_used <- union(rows_used, original_ids[complete$rows[ok]])
    rows_excluded <- union(rows_excluded, setdiff(original_ids, original_ids[complete$rows[ok]]))
  }
  valid_fit_indices <- which(!vapply(fits, is.null, logical(1L)))
  if (!length(valid_fit_indices)) {
    stop("No valid fits across imputations for the one-way ANOVA.", call. = FALSE)
  }
  coef_names <- setdiff(names(stats::coef(fits[[valid_fit_indices[[1L]]]])), "(Intercept)")
  if (!length(coef_names)) {
    stop("No non-intercept coefficients available for pooled ANOVA.", call. = FALSE)
  }
  pool <- .rls_mi_pool_wald_for_fits(fits, coef_names, term_names = group)
  if (!isTRUE(pool$ok)) {
    stop("Pooled Wald test for ANOVA failed.", call. = FALSE)
  }
  mi_meta <- .rls_mi_result_metadata(
    dataset, "one_way_anova",
    list(response = response, group = group),
    fits_by_imputation = fits,
    pooled_result = pool,
    pooling_method = "D1 pooled Wald"
  )
  list(
    result_id = .rls_compare_means_id(),
    dataset_id = dataset$dataset_id %||% dataset$group,
    analysis_type = "one_way_anova",
    analysis_backend = "multiple_imputation",
    specification = list(
      response = response,
      group = group,
      scope = scope
    ),
    descriptives = list(
      note = "Descriptive statistics are not pooled in this version.",
      n_imputations = pool$m
    ),
    test_results = list(
      method = "Welch one-way ANOVA (MI pooled, D1 Wald)",
      statistic = pool$F,
      parameter = c(df1 = pool$df1, df2 = pool$df2),
      p_value = pool$p,
      conf_level = conf_level
    ),
    effect_sizes = NULL,
    post_hoc = NULL,
    rows_used_original_ids = rows_used,
    rows_excluded_original_ids = rows_excluded,
    multiple_imputation = mi_meta,
    result_version = 1L,
    created_at = Sys.time()
  )
}

.rls_compare_means_native_payload <- function(result) {
  analysis_type <- result$analysis_type
  title <- switch(analysis_type,
    one_sample_t_test = "One-Sample t Test",
    independent_samples_t_test = "Independent-Samples t Test",
    paired_samples_t_test = "Paired-Samples t Test",
    one_way_anova = "One-Way ANOVA",
    "Compare Means"
  )
  is_mi <- identical(result$analysis_backend, "multiple_imputation")
  if (is_mi) {
    mi_m <- result$multiple_imputation$m %||% NA_integer_
    mi_note <- sprintf("Multiple imputation: m = %d; pooled using Rubin's rules.", mi_m)
    payload_prefix <- "COMPARE_MEANS_OPEN_POOLED"
  } else {
    mi_note <- ""
    payload_prefix <- "COMPARE_MEANS_OPEN"
  }
  spec <- result$specification
  if (analysis_type == "one_sample_t_test") {
    spec_lines <- c(
      "Response:", spec$response,
      "Null hypothesis: \u03bc =", as.character(spec$null_mu %||% 0),
      "Alternative:", spec$alternative %||% "two.sided",
      "Confidence level:", .rls_export_format_percent(spec$conf_level %||% 0.95)
    )
  } else if (analysis_type == "independent_samples_t_test") {
    spec_lines <- c(
      "Response:", spec$response,
      "Grouping variable:", spec$group,
      "Groups:", paste(spec$group_levels, collapse = " vs "),
      "Alternative:", spec$alternative %||% "two.sided",
      "Confidence level:", .rls_export_format_percent(spec$conf_level %||% 0.95),
      if (isTRUE(spec$var_equal)) "Equal variance assumed" else "Equal variance not assumed (Welch)"
    )
  } else if (analysis_type == "paired_samples_t_test") {
    spec_lines <- c(
      "Variable 1:", spec$response1,
      "Variable 2:", spec$response2,
      "Difference:", spec$difference,
      "Alternative:", spec$alternative %||% "two.sided",
      "Confidence level:", .rls_export_format_percent(spec$conf_level %||% 0.95)
    )
  } else {
    spec_lines <- c(
      "Response:", spec$response,
      "Factor:", spec$group,
      "Groups:", paste(spec$group_levels, collapse = ", "),
      "Confidence level:", .rls_export_format_percent(spec$conf_level %||% 0.95)
    )
  }
  payload <- c(
    payload_prefix,
    .rls_native_wire_value(result$result_id),
    .rls_native_wire_value(result$dataset_id),
    .rls_native_wire_value(analysis_type),
    .rls_native_wire_value(title),
    .rls_native_wire_value(mi_note),
    .rls_native_wire_number(result$test_results$statistic %||% NA_real_),
    .rls_native_wire_number(as.numeric(result$test_results$parameter[[1L]] %||% NA_real_)),
    if (length(result$test_results$parameter) > 1L)
      .rls_native_wire_number(as.numeric(result$test_results$parameter[[2L]] %||% NA_real_))
    else "",
    .rls_native_wire_number(result$test_results$p_value %||% NA_real_),
    .rls_native_wire_number(result$test_results$mean_diff %||% NA_real_),
    .rls_native_wire_number(result$test_results$mean_diff_se %||% NA_real_),
    .rls_native_wire_value(result$test_results$method %||% ""),
    .rls_native_wire_value(result$test_results$direction_note %||% ""),
    "COMPARE_MEANS_V2",
    .rls_native_wire_number((result$test_results$conf_int %||% c(NA_real_, NA_real_))[[1L]]),
    .rls_native_wire_number((result$test_results$conf_int %||% c(NA_real_, NA_real_))[[2L]]),
    as.character(length(spec_lines)),
    .rls_native_wire_value(spec_lines)
  )
  desc <- result$descriptives
  if (is_mi && analysis_type == "one_sample_t_test") {
    desc_lines <- c(
      "N imputations", as.character(desc$n_imputations %||% ""),
      "Mean", .rls_native_wire_number(desc$pooled_mean %||% NA_real_),
      "SD", "NA",
      "SE", .rls_native_wire_number(desc$pooled_se %||% NA_real_),
      "Note", desc$note %||% ""
    )
  } else if (is_mi && analysis_type == "paired_samples_t_test") {
    desc_lines <- c(
      "N imputations", as.character(desc$n_imputations %||% ""),
      "Mean difference", .rls_native_wire_number(desc$pooled_mean_diff %||% NA_real_),
      "SD of differences", "NA",
      "SE of differences", .rls_native_wire_number(desc$pooled_se_diff %||% NA_real_),
      "Note", desc$note %||% ""
    )
  } else if (is_mi) {
    desc_lines <- c(
      "N imputations", as.character(desc$n_imputations %||% ""),
      "Note", desc$note %||% ""
    )
  } else if (analysis_type == "one_sample_t_test") {
    desc_lines <- c(
      "N", as.character(desc$n %||% ""),
      "Mean", .rls_native_wire_number(desc$mean %||% NA_real_),
      "SD", .rls_native_wire_number(desc$sd %||% NA_real_),
      "SE", .rls_native_wire_number(desc$se %||% NA_real_)
    )
  } else if (analysis_type == "independent_samples_t_test") {
    dg <- desc$group %||% character()
    desc_lines <- as.character(rbind(
      c("Group", dg),
      c("N", as.character(desc$n %||% "")),
      c("Mean", vapply(desc$mean %||% NA_real_, .rls_native_wire_number, character(1L))),
      c("SD", vapply(desc$sd %||% NA_real_, .rls_native_wire_number, character(1L))),
      c("SE", vapply(desc$se %||% NA_real_, .rls_native_wire_number, character(1L)))
    ))
  } else if (analysis_type == "paired_samples_t_test") {
    dsc <- desc$differences %||% list()
    desc_lines <- c(
      "Pairs analyzed", as.character(dsc$n %||% ""),
      "Mean difference", .rls_native_wire_number(dsc$mean %||% NA_real_),
      "SD of differences", .rls_native_wire_number(dsc$sd %||% NA_real_),
      "SE of differences", .rls_native_wire_number(dsc$se %||% NA_real_),
      "Variable 1 mean", .rls_native_wire_number(desc$mean[[1L]] %||% NA_real_),
      "Variable 2 mean", .rls_native_wire_number(desc$mean[[2L]] %||% NA_real_)
    )
  } else {
    dg <- desc$group %||% character()
    mn <- vapply(desc$mean %||% NA_real_, .rls_native_wire_number, character(1L))
    sds <- vapply(desc$sd %||% NA_real_, .rls_native_wire_number, character(1L))
    ses <- vapply(desc$se %||% NA_real_, .rls_native_wire_number, character(1L))
    desc_lines <- as.character(rbind(
      c("Group", dg),
      c("N", as.character(desc$n %||% "")),
      c("Mean", mn),
      c("SD", sds),
      c("SE", ses)
    ))
  }
  payload <- c(payload, as.character(length(desc_lines)), .rls_native_wire_value(desc_lines))
  es <- result$effect_sizes
  if (!is.null(es)) {
    es_label <- es$effect_size_label %||% "Effect size"
    es_value <- es$cohens_d %||% es$cohens_dz %||% NA_real_
    es_lines <- c(
      es_label, .rls_native_wire_number(es_value)
    )
    es_ci <- es$effect_size_ci
    if (length(es_ci) == 2L && all(is.finite(es_ci))) {
      es_lines <- c(es_lines,
        "CI lower", .rls_native_wire_number(es_ci[[1L]]),
        "CI upper", .rls_native_wire_number(es_ci[[2L]])
      )
    }
    es_se <- es$cohens_d_se
    if (is.finite(es_se %||% NA_real_)) {
      es_lines <- c(es_lines, "SE", .rls_native_wire_number(es_se))
    }
  } else {
    es_lines <- character()
  }
  payload <- c(payload, .rls_native_wire_integer(length(es_lines) / 2L), .rls_native_wire_value(es_lines))
  ph <- result$post_hoc
  if (!is.null(ph) && length(ph)) {
    payload <- c(payload, "1", .rls_native_wire_value("Post-hoc comparisons not yet implemented in this wire block."))
  } else {
    payload <- c(payload, "0")
  }
  payload <- c(payload,
    .rls_native_int_vector_payload(result$rows_used_original_ids %||% integer()),
    .rls_native_int_vector_payload(result$rows_excluded_original_ids %||% integer()),
    if (is_mi) .rls_native_wire_value(sprintf("MI result: m = %d; %s", mi_m, result$test_results$pooling_method %||% "Rubin's rules")) else "",
    as.character(result$result_version %||% 1L)
  )
  payload
}

.rls_compare_means_sync_native <- function(result) {
  if (!isTRUE(.rls_state$process_started)) return(invisible(FALSE))
  try(.rls_send(.rls_compare_means_native_payload(result)), silent = TRUE)
  invisible(TRUE)
}

.rls_compare_means_batch_native_payload <- function(results, run_id = "") {
  if (!length(results)) stop("No compare-means results were produced.", call. = FALSE)
  first <- results[[1L]]
  type <- first$analysis_type
  spec <- first$specification
  title <- switch(type,
    one_sample_t_test = "One-Sample t Test",
    independent_samples_t_test = "Independent-Samples t Test",
    paired_samples_t_test = "Paired-Samples t Test",
    one_way_anova = "One-Way ANOVA",
    "Compare Means"
  )
  method <- if (type == "independent_samples_t_test") {
    if (identical(spec$method, "mann_whitney")) "Mann–Whitney U test" else
      if (isTRUE(spec$var_equal)) "Student - equal variances" else "Welch"
  } else if (type == "one_way_anova") {
    first$test_results$method %||% "Welch one-way ANOVA"
  } else first$test_results$method %||% ""
  group_levels <- spec$group_levels %||% character()
  first_group <- if (length(group_levels) >= 1L) group_levels[[1L]] else ""
  second_group <- if (length(group_levels) >= 2L) group_levels[[2L]] else ""
  adjustment <- first$test_results$p_adjustment %||% "holm"
  payload <- c(
    "COMPARE_MEANS_BATCH_OPEN",
    "COMPARE_MEANS_BATCH_V2",
    .rls_native_wire_value(if (nzchar(run_id)) run_id else .rls_compare_means_id()),
    .rls_native_wire_value(first$dataset_id),
    .rls_native_wire_value(type),
    .rls_native_wire_value(title),
    .rls_native_wire_value(spec$group %||% ""),
    .rls_native_wire_value(method),
    .rls_native_wire_value(spec$alternative %||% "two.sided"),
    .rls_native_wire_number(spec$conf_level %||% first$test_results$conf_level %||% 0.95),
    .rls_native_wire_number(spec$null_mu %||% 0),
    .rls_native_wire_value(first_group),
    .rls_native_wire_value(second_group),
    .rls_native_wire_value(adjustment),
    as.character(length(results))
  )
  at <- function(x, i = 1L, default = NA_real_) {
    if (length(x) >= i && !is.null(x[[i]])) x[[i]] else default
  }
  for (result in results) {
    desc <- result$descriptives %||% list()
    test <- result$test_results %||% list()
    effect <- result$effect_sizes %||% list()
    rspec <- result$specification %||% list()
    response1 <- rspec$response %||% rspec$response1 %||% ""
    response2 <- rspec$response2 %||% ""
    paired_desc <- desc$differences %||% list()
    n1 <- if (type == "paired_samples_t_test") paired_desc$n %||% NA_real_ else if (type %in% c("independent_samples_t_test", "one_way_anova")) sum(desc$n %||% NA_real_) else desc$n %||% NA_real_
    mean1 <- if (type %in% c("paired_samples_t_test", "independent_samples_t_test")) at(desc$mean, 1L) else if (type == "one_sample_t_test") desc$mean %||% NA_real_ else NA_real_
    sd1 <- if (type %in% c("paired_samples_t_test", "independent_samples_t_test")) at(desc$sd, 1L) else if (type == "one_sample_t_test") desc$sd %||% NA_real_ else NA_real_
    se1 <- if (type == "independent_samples_t_test") at(desc$se, 1L) else if (type == "one_sample_t_test") desc$se %||% NA_real_ else NA_real_
    mean2 <- if (type %in% c("paired_samples_t_test", "independent_samples_t_test")) at(desc$mean, 2L) else NA_real_
    sd2 <- if (type %in% c("paired_samples_t_test", "independent_samples_t_test")) at(desc$sd, 2L) else NA_real_
    se2 <- if (type == "independent_samples_t_test") at(desc$se, 2L) else NA_real_
    n2 <- if (type == "independent_samples_t_test") at(desc$n, 2L) else if (type == "paired_samples_t_test") paired_desc$n %||% NA_real_ else NA_real_
    effect_value <- effect$hedges_g %||% effect$cohens_dz %||% effect$cohens_d %||%
      effect$rank_biserial %||% effect$omega_squared %||% effect$epsilon_squared %||% NA_real_
    parameter <- test$parameter %||% c(NA_real_)
    interval <- test$conf_int %||% c(NA_real_, NA_real_)
    parameter <- c(parameter, rep(NA_real_, max(0L, 2L - length(parameter))))
    interval <- c(interval, rep(NA_real_, max(0L, 2L - length(interval))))
    values <- c(
      n1, mean1, sd1, se1, n2, mean2, sd2, se2,
      test$mean_diff %||% paired_desc$mean %||% NA_real_,
      paired_desc$sd %||% NA_real_, test$mean_diff_se %||% paired_desc$se %||% NA_real_,
      at(interval, 1L), at(interval, 2L),
      test$statistic %||% NA_real_, at(parameter, 1L), at(parameter, 2L),
      test$p_value %||% NA_real_, effect_value,
      test$p_adjusted %||% NA_real_, test$adjustment_family_size %||% 0
    )
    payload <- c(payload,
      .rls_native_wire_value(response1), .rls_native_wire_value(response2),
      .rls_native_wire_value(test$method %||% method),
      .rls_native_wire_value(effect$effect_size_label %||% ""),
      vapply(values, .rls_native_wire_number, character(1L))
    )
    groups <- desc$group %||% character()
    payload <- c(payload, as.character(length(groups)))
    for (g in seq_along(groups)) {
      row_lists <- desc$rows_original_ids %||% list()
      group_rows <- if (length(row_lists) >= g) row_lists[[g]] else integer()
      payload <- c(payload,
        .rls_native_wire_value(groups[[g]]),
        .rls_native_wire_number(at(desc$n, g)),
        .rls_native_wire_number(at(desc$mean, g)),
        .rls_native_wire_number(at(desc$sd, g)),
        .rls_native_wire_number(at(desc$se, g)),
        .rls_native_int_vector_payload(group_rows)
      )
    }
    excluded_n <- length(result$rows_excluded_original_ids %||% integer())
    warnings <- result$warnings %||% character()
    if (excluded_n > 0L) warnings <- c(warnings, sprintf("%d observations with missing values or outside the selected scope were excluded for %s.", excluded_n, response1))
    payload <- c(payload,
      .rls_native_int_vector_payload(result$rows_used_original_ids %||% integer()),
      .rls_native_int_vector_payload(result$rows_excluded_original_ids %||% integer()),
      as.character(length(warnings)), .rls_native_wire_value(warnings)
    )
  }
  payload
}

.rls_compare_means_sync_native_batch <- function(results) {
  if (!isTRUE(.rls_state$process_started)) return(invisible(FALSE))
  run_id <- .rls_state$compare_means_run_id %||% ""
  try(.rls_send(.rls_compare_means_batch_native_payload(results, run_id)), silent = TRUE)
  invisible(TRUE)
}

#' One-sample t-test
#'
#' @param data Registered dataset name, a data frame, or `NULL` for active dataset.
#' @param response Response variable name(s). Multiple responses produce separate analyses.
#' @param mu Null hypothesis mean (default 0).
#' @param alternative Direction of the alternative hypothesis: `"two.sided"`, `"greater"`, or `"less"`.
#' @param conf_level Confidence level for the interval (default 0.95).
#' @param scope One of `"all"`, `"selected"`, or `"unselected"`.
#' @param p_adjust Multiplicity adjustment for the displayed inferential family:
#'   `"holm"` (default), `"bonferroni"`, or `"none"`.
#' @param .selected_rows Internal explicit original-row IDs supplied by the native analysis-scope bridge.
#' @return A compare-means result id (character).
#' @rdname compare-means
#' @export
ls_new_one_sample_t_test <- function(data = NULL, response = NULL, mu = 0,
                                     alternative = c("two.sided", "less", "greater"),
                                     conf_level = 0.95, scope = "all",
                                     p_adjust = c("holm", "bonferroni", "none"),
                                     method = c("student", "wilcoxon"), .selected_rows = NULL) {
  alternative <- match.arg(alternative)
  p_adjust <- match.arg(p_adjust)
  method <- match.arg(method)
  .rls_compare_means_validate_options(conf_level, mu)
  scope <- match.arg(scope, c("all", "selected", "unselected"))
  dataset <- .rls_dataset_record(data)
  original_ids <- dataset$original_row_ids %||% seq_len(nrow(dataset$data))
  selected_rows <- if (!is.null(.selected_rows)) as.integer(.selected_rows) else
    if (scope != "all") ls_selected(dataset$group) else integer()
  responses <- .rls_compare_means_validate_responses(dataset$data, response %||% names(dataset$data)[vapply(dataset$data, is.numeric, logical(1L))])
  results <- lapply(responses, function(r) {
    method_label <- if (identical(method, "wilcoxon")) "Wilcoxon signed-rank test" else "One-sample t-test"
    spec <- list(response = r, null_mu = mu, alternative = alternative,
                 conf_level = conf_level, scope = scope, method = method_label)
    tryCatch({
      backend <- .rls_analysis_backend(dataset, "one_sample_t_test")
      if (identical(backend, "multiple_imputation")) {
        if (identical(method, "wilcoxon")) stop("Wilcoxon tests are not available for multiple-imputation datasets.", call. = FALSE)
        .rls_one_sample_t_test_mi(list(selected_rows = selected_rows), dataset, r, mu, alternative, conf_level, scope, original_ids)
      } else {
        complete <- .rls_compare_means_complete_data(dataset$data, r, scope, selected_rows)
        if (length(complete$rows) < 2L) {
          stop(sprintf("Not enough complete cases for variable `%s`.", r), call. = FALSE)
        }
        if (identical(method, "wilcoxon"))
          .rls_one_sample_wilcoxon_ordinary(dataset, r, complete, mu, alternative, conf_level, original_ids) else
          .rls_one_sample_t_test_ordinary(dataset, r, complete, mu, alternative, conf_level, original_ids)
      }
    }, error = function(e) .rls_compare_means_failed_result(
      dataset, "one_sample_t_test", spec, conditionMessage(e), original_ids))
  })
  results <- .rls_compare_means_require_any_estimable(results)
  results <- .rls_compare_means_adjust_results(results, p_adjust)
  for (result in results) {
    .rls_compare_means_assign(result)
  }
  .rls_compare_means_sync_native_batch(results)
  invisible(vapply(results, `[[`, character(1L), "result_id"))
}

#' Independent-samples t-test
#'
#' @param data Registered dataset name, a data frame, or `NULL` for active dataset.
#' @param response Response variable name(s). Multiple responses produce separate analyses.
#' @param group Two-level grouping variable.
#' @param alternative Direction of the alternative hypothesis.
#' @param conf_level Confidence level for the interval.
#' @param var_equal Logical. If `TRUE` use the pooled-variance Student t-test; default `FALSE` uses Welch.
#' @param scope One of `"all"`, `"selected"`, or `"unselected"`.
#' @param group_order Optional character vector defining the displayed and tested group order.
#' @param p_adjust Multiplicity adjustment for the displayed inferential family:
#'   `"holm"` (default), `"bonferroni"`, or `"none"`.
#' @param .selected_rows Internal explicit original-row IDs supplied by the native analysis-scope bridge.
#' @return A compare-means result id (character).
#' @rdname compare-means
#' @export
ls_new_independent_samples_t_test <- function(data = NULL, response = NULL, group = NULL,
                                              alternative = c("two.sided", "less", "greater"),
                                              conf_level = 0.95, var_equal = FALSE, scope = "all",
                                              group_order = NULL,
                                              p_adjust = c("holm", "bonferroni", "none"),
                                              method = NULL, .selected_rows = NULL) {
  alternative <- match.arg(alternative)
  p_adjust <- match.arg(p_adjust)
  method <- if (is.null(method)) {
    if (isTRUE(var_equal)) "student" else "welch"
  } else match.arg(method, c("welch", "student", "mann_whitney"))
  var_equal <- identical(method, "student")
  .rls_compare_means_validate_options(conf_level)
  scope <- match.arg(scope, c("all", "selected", "unselected"))
  dataset <- .rls_dataset_record(data)
  original_ids <- dataset$original_row_ids %||% seq_len(nrow(dataset$data))
  if (is.null(group)) stop("A grouping variable is required.", call. = FALSE)
  group <- .rls_compare_means_validate_group(dataset$data, group)
  selected_rows <- if (!is.null(.selected_rows)) as.integer(.selected_rows) else
    if (scope != "all") ls_selected(dataset$group) else integer()
  responses <- .rls_compare_means_validate_responses(dataset$data, response %||% names(dataset$data)[vapply(dataset$data, is.numeric, logical(1L))])
  group_factor <- .rls_compare_means_group_factor(dataset$data, group, group_order)
  levs <- levels(group_factor)
  if (length(levs) != 2L) {
    stop(sprintf("Grouping variable `%s` must have exactly two levels, but has %d.", group, length(levs)), call. = FALSE)
  }
  results <- lapply(responses, function(r) {
    method_label <- if (identical(method, "mann_whitney")) "Mann–Whitney U test" else
      if (identical(method, "student")) "Student's t-test (equal variance)" else "Welch t-test"
    spec <- list(response = r, group = group, group_levels = levs,
                 alternative = alternative, conf_level = conf_level,
                 var_equal = isTRUE(var_equal), scope = scope,
                 method = method_label)
    tryCatch({
      backend <- .rls_analysis_backend(dataset, "independent_samples_t_test")
      if (identical(backend, "multiple_imputation")) {
        if (identical(method, "mann_whitney")) stop("Mann–Whitney tests are not available for multiple-imputation datasets.", call. = FALSE)
        .rls_independent_samples_t_test_mi(list(selected_rows = selected_rows), dataset, r, group, alternative, conf_level, var_equal, scope, original_ids)
      } else {
        complete <- .rls_compare_means_complete_data(dataset$data, c(r, group), scope, selected_rows)
        if (length(unique(complete$data[[group]])) < 2L) {
          stop(sprintf("Not enough complete cases across both groups for `%s`.", r), call. = FALSE)
        }
        if (identical(method, "mann_whitney"))
          .rls_independent_samples_mann_whitney_ordinary(dataset, r, group_factor, complete, alternative, conf_level, original_ids, group_name = group) else
          .rls_independent_samples_t_test_ordinary(dataset, r, group_factor, complete, alternative, conf_level, var_equal, original_ids, group_name = group)
      }
    }, error = function(e) .rls_compare_means_failed_result(
      dataset, "independent_samples_t_test", spec, conditionMessage(e), original_ids))
  })
  results <- .rls_compare_means_require_any_estimable(results)
  results <- .rls_compare_means_adjust_results(results, p_adjust)
  for (result in results) {
    .rls_compare_means_assign(result)
  }
  .rls_compare_means_sync_native_batch(results)
  invisible(vapply(results, `[[`, character(1L), "result_id"))
}

#' Paired-samples t-test
#'
#' @param data Registered dataset name, a data frame, or `NULL` for active dataset.
#' @param pairs A list of character vectors of length 2, e.g. `list(c("pre","post"))`.
#' @param alternative Direction of the alternative hypothesis.
#' @param conf_level Confidence level for the interval.
#' @param scope One of `"all"`, `"selected"`, or `"unselected"`.
#' @param p_adjust Multiplicity adjustment for the displayed inferential family:
#'   `"holm"` (default), `"bonferroni"`, or `"none"`.
#' @param .selected_rows Internal explicit original-row IDs supplied by the native analysis-scope bridge.
#' @return A compare-means result id (character).
#' @rdname compare-means
#' @export
ls_new_paired_samples_t_test <- function(data = NULL, pairs = NULL,
                                         alternative = c("two.sided", "less", "greater"),
                                         conf_level = 0.95, scope = "all",
                                         p_adjust = c("holm", "bonferroni", "none"),
                                         method = c("student", "wilcoxon"), .selected_rows = NULL) {
  alternative <- match.arg(alternative)
  p_adjust <- match.arg(p_adjust)
  method <- match.arg(method)
  .rls_compare_means_validate_options(conf_level)
  scope <- match.arg(scope, c("all", "selected", "unselected"))
  dataset <- .rls_dataset_record(data)
  original_ids <- dataset$original_row_ids %||% seq_len(nrow(dataset$data))
  if (is.null(pairs) || !is.list(pairs)) stop("`pairs` must be a list of variable-name pairs.", call. = FALSE)
  selected_rows <- if (!is.null(.selected_rows)) as.integer(.selected_rows) else
    if (scope != "all") ls_selected(dataset$group) else integer()
  results <- lapply(pairs, function(pair) {
    r1 <- if (length(pair) >= 1L) as.character(pair[[1L]])[[1L]] else ""
    r2 <- if (length(pair) >= 2L) as.character(pair[[2L]])[[1L]] else ""
    method_label <- if (identical(method, "wilcoxon")) "Paired Wilcoxon signed-rank test" else "Paired t-test"
    spec <- list(response1 = r1, response2 = r2, difference = paste(r1, "-", r2),
                 alternative = alternative, conf_level = conf_level, scope = scope,
                 method = method_label)
    tryCatch({
      r1 <- .rls_validate_protocol_name(r1, "response")
      r2 <- .rls_validate_protocol_name(r2, "response")
      if (identical(r1, r2)) stop("Select two different numeric variables for each pair.", call. = FALSE)
      if (!r1 %in% names(dataset$data)) stop(sprintf("Variable `%s` not found.", r1), call. = FALSE)
      if (!r2 %in% names(dataset$data)) stop(sprintf("Variable `%s` not found.", r2), call. = FALSE)
      if (!is.numeric(dataset$data[[r1]])) stop(sprintf("Variable `%s` must be numeric.", r1), call. = FALSE)
      if (!is.numeric(dataset$data[[r2]])) stop(sprintf("Variable `%s` must be numeric.", r2), call. = FALSE)
      backend <- .rls_analysis_backend(dataset, "paired_samples_t_test")
      if (identical(backend, "multiple_imputation")) {
        if (identical(method, "wilcoxon")) stop("Paired Wilcoxon tests are not available for multiple-imputation datasets.", call. = FALSE)
        .rls_paired_samples_t_test_mi(list(selected_rows = selected_rows), dataset, r1, r2, alternative, conf_level, scope, original_ids)
      } else {
        complete <- .rls_compare_means_complete_data(dataset$data, c(r1, r2), scope, selected_rows)
        if (length(complete$rows) < 2L) {
          stop(sprintf("Not enough complete pairs for `%s` and `%s`.", r1, r2), call. = FALSE)
        }
        if (identical(method, "wilcoxon"))
          .rls_paired_samples_wilcoxon_ordinary(dataset, r1, r2, complete, alternative, conf_level, original_ids) else
          .rls_paired_samples_t_test_ordinary(dataset, r1, r2, complete, alternative, conf_level, original_ids)
      }
    }, error = function(e) .rls_compare_means_failed_result(
      dataset, "paired_samples_t_test", spec, conditionMessage(e), original_ids))
  })
  results <- .rls_compare_means_require_any_estimable(results)
  results <- .rls_compare_means_adjust_results(results, p_adjust)
  for (result in results) {
    .rls_compare_means_assign(result)
  }
  .rls_compare_means_sync_native_batch(results)
  invisible(vapply(results, `[[`, character(1L), "result_id"))
}

#' One-way ANOVA
#'
#' @param data Registered dataset name, a data frame, or `NULL` for active dataset.
#' @param response Response variable name(s).
#' @param group Grouping variable (categorical or ordinal).
#' @param conf_level Confidence level.
#' @param scope One of `"all"`, `"selected"`, or `"unselected"`.
#' @param method `"welch"` (default), `"classical"`, or `"kruskal_wallis"`.
#' @param group_order Optional character vector defining the group order.
#' @param p_adjust Multiplicity adjustment for the displayed inferential family:
#'   `"holm"` (default), `"bonferroni"`, or `"none"`.
#' @param .selected_rows Internal explicit original-row IDs supplied by the native analysis-scope bridge.
#' @return A compare-means result id (character).
#' @rdname compare-means
#' @export
ls_new_one_way_anova <- function(data = NULL, response = NULL, group = NULL,
                                 conf_level = 0.95, scope = "all",
                                 method = c("welch", "classical", "kruskal_wallis"), group_order = NULL,
                                 p_adjust = c("holm", "bonferroni", "none"), .selected_rows = NULL) {
  method <- match.arg(method)
  p_adjust <- match.arg(p_adjust)
  .rls_compare_means_validate_options(conf_level)
  scope <- match.arg(scope, c("all", "selected", "unselected"))
  dataset <- .rls_dataset_record(data)
  original_ids <- dataset$original_row_ids %||% seq_len(nrow(dataset$data))
  if (is.null(group)) stop("A grouping variable is required.", call. = FALSE)
  group <- .rls_compare_means_validate_group(dataset$data, group)
  selected_rows <- if (!is.null(.selected_rows)) as.integer(.selected_rows) else
    if (scope != "all") ls_selected(dataset$group) else integer()
  responses <- .rls_compare_means_validate_responses(dataset$data, response %||% names(dataset$data)[vapply(dataset$data, is.numeric, logical(1L))])
  group_factor <- .rls_compare_means_group_factor(dataset$data, group, group_order)
  results <- lapply(responses, function(r) {
    method_label <- if (identical(method, "kruskal_wallis")) "Kruskal–Wallis test" else
      if (identical(method, "classical")) "Classical one-way ANOVA" else "Welch one-way ANOVA"
    spec <- list(response = r, group = group, group_levels = levels(group_factor),
                 alternative = "two.sided", conf_level = conf_level, scope = scope,
                 method = method_label)
    tryCatch({
      backend <- .rls_analysis_backend(dataset, "one_way_anova")
      if (identical(backend, "multiple_imputation")) {
        if (identical(method, "kruskal_wallis")) stop("Kruskal–Wallis tests are not available for multiple-imputation datasets.", call. = FALSE)
        .rls_one_way_anova_mi(list(selected_rows = selected_rows), dataset, r, group, "two.sided", conf_level, scope, original_ids)
      } else {
        complete <- .rls_compare_means_complete_data(dataset$data, c(r, group), scope, selected_rows)
        .rls_one_way_anova_ordinary(dataset, r, group_factor, complete, "two.sided", conf_level, original_ids, group_name = group, method = method)
      }
    }, error = function(e) .rls_compare_means_failed_result(
      dataset, "one_way_anova", spec, conditionMessage(e), original_ids))
  })
  results <- .rls_compare_means_require_any_estimable(results)
  results <- .rls_compare_means_adjust_results(results, p_adjust)
  for (result in results) {
    .rls_compare_means_assign(result)
  }
  .rls_compare_means_sync_native_batch(results)
  invisible(vapply(results, `[[`, character(1L), "result_id"))
}
