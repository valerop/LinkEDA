.rls_compare_means_state <- new.env(parent = emptyenv())

.rls_compare_means_id <- function() {
  # Identifiers must not advance R's statistical random-number stream.
  .rls_state$compare_means_sequence <- (.rls_state$compare_means_sequence %||% 0) + 1
  paste0("cm_", format(Sys.time(), "%Y%m%d%H%M%OS3"), "_", .rls_state$compare_means_sequence)
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

.rls_compare_means_complete_data <- function(data, variables, scope = "all",
                                              selected_rows = integer(),
                                              original_ids = NULL) {
  vars <- unique(as.character(variables))
  missing_data <- data[, vars, drop = FALSE]
  ok <- stats::complete.cases(missing_data)
  row_ids <- as.integer(original_ids %||% seq_len(nrow(data)))
  if (length(row_ids) != nrow(data)) {
    stop("The subject-id map does not match the analysis data.", call. = FALSE)
  }
  if (identical(scope, "selected")) {
    keep <- row_ids %in% selected_rows
    ok <- ok & keep
  } else if (identical(scope, "unselected")) {
    keep <- !row_ids %in% selected_rows
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

.rls_compare_means_record_data <- function(record) {
  if (is.data.frame(record)) record else record$data
}

.rls_compare_means_record_metadata <- function(record) {
  if (is.data.frame(record)) NULL else record$variable_metadata %||% record$metadata
}

.rls_compare_means_semantic_type <- function(record, variable) {
  data <- .rls_compare_means_record_data(record)
  .rls_metadata_type(
    .rls_compare_means_record_metadata(record), variable,
    .rls_variable_type(data[[variable]])
  )
}

.rls_compare_means_rank_values <- function(record, variable, values) {
  type <- .rls_compare_means_semantic_type(record, variable)
  if (!type %in% c("numeric", "ordered")) {
    stop(sprintf(paste0("Response variable `%s` must be Numeric or Ordinal for a rank test. ",
      "Nominal categories have no defined order; set an explicit Ordinal order first."),
      variable), call. = FALSE)
  }
  if (identical(type, "ordered")) {
    ordered_levels <- .rls_metadata_levels(.rls_compare_means_record_metadata(record), variable)
    if (!length(ordered_levels) && is.ordered(values)) ordered_levels <- levels(values)
    if (!length(ordered_levels))
      stop(sprintf("Ordinal variable `%s` has no defined category order.", variable), call. = FALSE)
    encoded <- match(as.character(values), ordered_levels)
    if (any(!is.na(values) & is.na(encoded)))
      stop(sprintf("Variable `%s` contains categories outside its defined order.", variable), call. = FALSE)
    return(list(values = encoded, levels = ordered_levels))
  }
  if (!is.numeric(values))
    stop(sprintf("Response variable `%s` must contain numeric values.", variable), call. = FALSE)
  list(values = values, levels = character())
}

.rls_compare_means_validate_group <- function(record, group) {
  data <- .rls_compare_means_record_data(record)
  group <- .rls_validate_protocol_name(group, "group")
  if (!group %in% names(data)) {
    stop(sprintf("Grouping variable `%s` was not found.", group), call. = FALSE)
  }
  type <- .rls_compare_means_semantic_type(record, group)
  if (identical(type, "character")) {
    stop(sprintf(
      "Grouping variable `%s` is Text. Change its type to Categorical or Ordinal before using it as a categorical grouping variable.",
      group
    ), call. = FALSE)
  }
  if (!type %in% c("factor", "ordered", "logical", "numeric")) {
    stop(sprintf("Grouping variable `%s` is not a supported grouping type.", group), call. = FALSE)
  }
  group
}

.rls_compare_means_group_factor <- function(record, group, group_order = NULL) {
  data <- .rls_compare_means_record_data(record)
  x <- data[[group]]
  defined_levels <- .rls_metadata_levels(
    .rls_compare_means_record_metadata(record), group
  )
  f <- if (is.factor(x)) {
    droplevels(x)
  } else if (length(defined_levels)) {
    droplevels(factor(as.character(x), levels = defined_levels))
  } else {
    factor(x)
  }
  if (length(group_order)) {
    observed <- levels(f)
    requested <- as.character(group_order)
    requested <- requested[!is.na(requested) & nzchar(requested)]
    if (anyDuplicated(requested) ||
        !(setequal(observed, requested) ||
          (length(requested) == 2L && all(requested %in% observed)))) {
      stop(paste0(
        "Group order must contain either every effective group exactly once ",
        "or two distinct observed groups for a two-sample comparison."
      ), call. = FALSE)
    }
    f <- factor(as.character(f), levels = requested)
  }
  if (nlevels(f) < 2L) {
    stop(sprintf("Grouping variable `%s` must have at least two categories.", group), call. = FALSE)
  }
  f
}

.rls_compare_means_scoped_group_factor <- function(dataset, group, scope, selected_rows,
                                                     original_ids, group_order = NULL) {
  complete <- .rls_compare_means_complete_data(dataset$data, group, scope, selected_rows, original_ids)
  if (!nrow(complete$data)) stop("Not enough complete cases across both groups in the analysis scope.", call. = FALSE)
  scoped <- dataset
  scoped$data <- complete$data
  effective <- .rls_compare_means_group_factor(scoped, group, group_order)
  factor(as.character(dataset$data[[group]]), levels = levels(effective))
}

# Native analysis windows retain a user-selected group order so that changing
# options does not unexpectedly reverse a contrast.  When the grouping
# variable itself changes, however, an older window can briefly submit the
# levels of the previous variable.  Reconcile that transport state against the
# current data before invoking the strict public analysis API.  A valid order
# is preserved verbatim; a stale or duplicate order is replaced by the factor's
# canonical effective levels.
.rls_compare_means_native_group_order <- function(record, group, group_order = NULL) {
  data <- .rls_compare_means_record_data(record)
  if (is.null(group) || length(group) != 1L || is.na(group) || !nzchar(group) ||
      !group %in% names(data)) {
    return(character())
  }
  observed <- levels(.rls_compare_means_group_factor(record, group))
  requested <- as.character(group_order %||% character())
  requested <- requested[!is.na(requested) & nzchar(requested)]
  if (!anyDuplicated(requested) &&
      ((length(requested) == length(observed) && setequal(requested, observed)) ||
       (length(requested) == 2L && all(requested %in% observed)))) {
    return(requested)
  }
  observed
}

.rls_compare_means_restrict_complete_groups <- function(complete, group_factor) {
  keep <- !is.na(group_factor[complete$rows])
  complete$rows <- complete$rows[keep]
  complete$data <- complete$data[keep, , drop = FALSE]
  complete$excluded <- length(group_factor) - length(complete$rows)
  complete
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
  effect <- effectsize::cohens_d(y, mu = mu, ci = conf_level,
                                alternative = alternative, verbose = FALSE)
  d <- effect$Cohens_d[[1L]]
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
      effect_size_ci = c(effect$CI_low[[1L]], effect$CI_high[[1L]]),
      method = "effectsize::cohens_d (noncentral-t interval)"
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
  raw_y <- complete$data[[response]]
  ranked <- .rls_compare_means_rank_values(data, response, raw_y)
  y <- ranked$values
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
                         method = "wilcoxon",
                         ordinal_levels = ranked$levels),
    descriptives = list(n = n, mean = stats::median(y), sd = stats::IQR(y),
                        se = NA_real_),
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

.rls_compare_means_binary_levels <- function(data, response, values) {
  categories <- .rls_metadata_levels(data$variable_metadata %||% data$metadata, response)
  if (length(categories) != 2L) categories <- levels(factor(data$data[[response]]))
  if (length(categories) != 2L) categories <- levels(droplevels(factor(values)))
  categories
}

.rls_one_sample_binomial_ordinary <- function(data, response, complete, probability,
                                               alternative, conf_level, original_ids) {
  # Preserve a known event even when the selected scope has no successes or
  # no failures. Do not invent a second category for an ambiguous one-level input.
  categories <- .rls_compare_means_binary_levels(data, response, complete$data[[response]])
  y <- factor(as.character(complete$data[[response]]), levels = categories)
  if (length(categories) != 2L || anyNA(y)) {
    stop(sprintf("Variable `%s` must have exactly two defined categories for a binomial test.",
                 response), call. = FALSE)
  }
  if (!is.finite(probability) || probability < 0 || probability > 1) {
    stop("The binomial test proportion must be between 0 and 1.", call. = FALSE)
  }
  event <- levels(y)[[2L]]
  n <- length(y)
  successes <- sum(as.character(y) == event)
  observed <- successes / n
  test <- stats::binom.test(successes, n, p = probability,
                            alternative = alternative, conf.level = conf_level)
  rows_used <- original_ids[complete$rows]
  rows_excluded <- setdiff(original_ids, rows_used)
  list(
    result_id = .rls_compare_means_id(),
    dataset_id = data$dataset_id %||% data$group,
    analysis_type = "one_sample_t_test",
    analysis_backend = "ordinary",
    specification = list(
      response = response, null_mu = probability, alternative = alternative,
      conf_level = conf_level, scope = complete$scope %||% "all",
      method = "binomial", test_family = "binomial", response_type = "categorical",
      event_level = event
    ),
    descriptives = list(
      n = n, mean = observed, sd = NA_real_,
      se = sqrt(observed * (1 - observed) / n)
    ),
    test_results = list(
      method = "Exact binomial test", statistic = successes, parameter = n,
      p_value = test$p.value, conf_int = unname(test$conf.int),
      conf_level = conf_level, alternative = alternative,
      null_value = probability, mean_diff = observed - probability,
      mean_diff_se = sqrt(observed * (1 - observed) / n),
      successes = successes, trials = n, observed_proportion = observed
    ),
    effect_sizes = list(effect_size_label = ""), post_hoc = NULL,
    rows_used_original_ids = rows_used,
    rows_excluded_original_ids = rows_excluded,
    multiple_imputation = NULL, result_version = 1L, created_at = Sys.time()
  )
}

.rls_one_sample_t_test_mi <- function(record, dataset, response, mu, alternative, conf_level, scope, original_ids) {
  completed <- .rls_mi_completed_datasets(dataset)
  estimates <- numeric()
  variances <- numeric()
  sample_sizes <- integer()
  rows_used <- integer()
  rows_excluded <- integer()
  for (data_imp in completed) {
    complete <- .rls_compare_means_complete_data(
      data_imp, response, scope, record$selected_rows %||% integer(), original_ids)
    y <- complete$data[[response]]
    n <- length(y)
    sample_sizes <- c(sample_sizes, n)
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
    cc <- .rls_compare_means_complete_data(
      d, response, scope, record$selected_rows %||% integer(), original_ids)
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
  valid_sizes <- sample_sizes[is.finite(estimates) & is.finite(variances)]
  pooled_n <- if (length(valid_sizes) && length(unique(valid_sizes)) == 1L) {
    valid_sizes[[1L]]
  } else {
    NA_integer_
  }
  pooled_mean <- pool$Qbar + mu
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
      # Keep the canonical descriptive names used by the batch/table bridge,
      # while retaining the explicit pooled aliases for older consumers.
      n = pooled_n,
      mean = pooled_mean,
      sd = NA_real_,
      se = pool$SE,
      pooled_mean = pooled_mean,
      pooled_se = pool$SE,
      n_imputations = pool$m,
      n_min = if (length(valid_sizes)) min(valid_sizes) else NA_integer_,
      n_max = if (length(valid_sizes)) max(valid_sizes) else NA_integer_,
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
      pooling_method = "mice::pool.scalar (Rubin's rules)"
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
  diff <- m1 - m2
  test <- stats::t.test(g1, g2, alternative = alternative, var.equal = isTRUE(var_equal), conf.level = conf_level)
  t_stat <- unname(test$statistic)
  se <- unname(test$stderr)
  df <- unname(test$parameter)
  method <- if (isTRUE(var_equal)) "Student's t-test (equal variance)" else "Welch t-test"
  p <- test$p.value
  ci <- unname(test$conf.int)
  effect_d <- effectsize::cohens_d(g1, g2, pooled_sd = TRUE, ci = NULL, verbose = FALSE)
  effect_g <- effectsize::hedges_g(g1, g2, pooled_sd = TRUE, ci = conf_level,
                                  alternative = alternative, verbose = FALSE)
  d <- effect_d$Cohens_d[[1L]]
  g <- effect_g$Hedges_g[[1L]]
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
      group_reference = levs[[2L]],
      group_comparison = levs[[1L]],
      direction_note = sprintf("Mean difference: %s - %s", levs[[1L]], levs[[2L]])
    ),
    effect_sizes = list(
      cohens_d = d,
      cohens_d_se = NA_real_,
      hedges_g = g,
      effect_size_ci = c(effect_g$CI_low[[1L]], effect_g$CI_high[[1L]]),
      effect_size_label = "Hedges' g",
      method = "effectsize::hedges_g (exact correction, pooled SD)",
      note = "Hedges' g uses pooled SD and the exact small-sample correction, including when the test is Welch's."
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
  raw_y <- complete$data[[response]]
  ranked <- .rls_compare_means_rank_values(data, response, raw_y)
  y <- ranked$values
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
                         ordinal_levels = ranked$levels,
                         scope = complete$scope %||% "all"),
    descriptives = list(
      group = levs, n = c(n1, n2),
      # The batch bridge uses the four compact descriptive slots for the
      # quantities that are meaningful for a rank comparison: median and IQR.
      mean = c(stats::median(g1), stats::median(g2)),
      sd = c(stats::IQR(g1), stats::IQR(g2)), se = c(NA_real_, NA_real_),
      rows_original_ids = rows_by_group
    ),
    test_results = list(
      method = "Mann-Whitney U test", statistic = u, parameter = NA_real_,
      p_value = test$p.value, conf_int = interval, conf_level = conf_level,
      alternative = alternative, mean_diff = shift, mean_diff_se = NA_real_,
      group_reference = levs[[2L]], group_comparison = levs[[1L]],
      direction_note = sprintf("Location shift: %s - %s", levs[[1L]], levs[[2L]])
    ),
    effect_sizes = list(rank_biserial = 2 * u / (n1 * n2) - 1,
                        effect_size_label = "Rank-biserial r"),
    post_hoc = NULL, rows_used_original_ids = rows_used,
    rows_excluded_original_ids = rows_excluded, multiple_imputation = NULL,
    result_version = 1L, created_at = Sys.time()
  )
}

.rls_independent_samples_proportion_ordinary <- function(data, response, group_factor,
                                                          complete, alternative, conf_level,
                                                          original_ids,
                                                          group_name = "group") {
  gf <- droplevels(group_factor[complete$rows])
  levs <- levels(gf)
  response_factor <- droplevels(factor(complete$data[[response]]))
  if (nlevels(response_factor) != 2L) {
    stop(sprintf("Variable `%s` must have exactly two observed categories for a two-sample proportion test.",
                 response), call. = FALSE)
  }
  event <- levels(response_factor)[[2L]]
  n <- vapply(levs, function(level) sum(gf == level), integer(1L))
  successes <- vapply(levs, function(level) {
    sum(gf == level & as.character(response_factor) == event)
  }, integer(1L))
  if (any(n < 1L)) stop("Both effective groups must contain valid observations.", call. = FALSE)
  proportions <- successes / n
  captured_warnings <- character()
  test <- withCallingHandlers(
    stats::prop.test(successes, n, alternative = alternative,
                     conf.level = conf_level, correct = FALSE),
    warning = function(w) {
      captured_warnings <<- unique(c(captured_warnings, conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
  )
  difference <- proportions[[1L]] - proportions[[2L]]
  pooled <- sum(successes) / sum(n)
  difference_se <- sqrt(pooled * (1 - pooled) * sum(1 / n))
  cohens_h <- 2 * asin(sqrt(proportions[[1L]])) -
    2 * asin(sqrt(proportions[[2L]]))
  binary_sd <- vapply(seq_along(n), function(i) {
    values <- c(rep(1, successes[[i]]), rep(0, n[[i]] - successes[[i]]))
    if (length(values) > 1L) stats::sd(values) else NA_real_
  }, numeric(1L))
  rows_used <- original_ids[complete$rows]
  rows_excluded <- setdiff(original_ids, rows_used)
  rows_by_group <- lapply(levs, function(level) original_ids[complete$rows[gf == level]])
  list(
    result_id = .rls_compare_means_id(),
    dataset_id = data$dataset_id %||% data$group,
    analysis_type = "independent_samples_t_test", analysis_backend = "ordinary",
    specification = list(
      response = response, group = group_name, group_levels = levs,
      alternative = alternative, conf_level = conf_level, var_equal = FALSE,
      method = "proportion", test_family = "proportion",
      response_type = "categorical", event_level = event,
      scope = complete$scope %||% "all"
    ),
    descriptives = list(
      group = levs, n = n, mean = proportions, sd = binary_sd,
      se = sqrt(proportions * (1 - proportions) / n),
      successes = successes, event_level = event,
      rows_original_ids = rows_by_group
    ),
    test_results = list(
      method = "Two-sample proportion test", statistic = unname(test$statistic),
      parameter = unname(test$parameter), p_value = test$p.value,
      conf_int = unname(test$conf.int), conf_level = conf_level,
      alternative = alternative, null_value = 0,
      mean_diff = difference, mean_diff_se = difference_se,
      successes = successes, trials = n, observed_proportions = proportions,
      group_reference = levs[[2L]], group_comparison = levs[[1L]],
      direction_note = sprintf("Proportion difference for %s: %s - %s",
                               event, levs[[1L]], levs[[2L]])
    ),
    effect_sizes = list(cohens_h = cohens_h, effect_size_label = "Cohen's h"),
    post_hoc = NULL, rows_used_original_ids = rows_used,
    rows_excluded_original_ids = rows_excluded,
    warnings = captured_warnings, multiple_imputation = NULL,
    result_version = 1L, created_at = Sys.time()
  )
}

.rls_independent_samples_t_test_mi <- function(record, dataset, response, group,
                                                group_levels, alternative,
                                                conf_level, var_equal, scope,
                                                original_ids) {
  completed <- .rls_mi_completed_datasets(dataset)
  levs <- as.character(group_levels)
  if (length(levs) != 2L || anyDuplicated(levs)) {
    stop("The independent-samples test requires exactly two ordered groups.", call. = FALSE)
  }

  m_total <- length(completed)
  fits <- vector("list", m_total)
  estimates <- rep(NA_real_, m_total)
  variances <- rep(NA_real_, m_total)
  complete_dfs <- rep(NA_real_, m_total)
  group_n <- matrix(NA_real_, nrow = m_total, ncol = 2L,
                    dimnames = list(NULL, levs))
  group_means <- group_variances <- group_mean_variances <- group_n
  rows_used <- integer()
  rows_by_group <- setNames(vector("list", 2L), levs)

  for (i in seq_along(completed)) {
    data_imp <- completed[[i]]
    complete <- .rls_compare_means_complete_data(
      data_imp, c(response, group), scope,
      record$selected_rows %||% integer(), original_ids
    )
    y <- complete$data[[response]]
    gf <- factor(as.character(complete$data[[group]]), levels = levs)
    ok <- is.finite(y) & !is.na(gf)
    if (sum(ok) < 4L || length(unique(gf[ok])) < 2L) next

    samples <- lapply(levs, function(level) y[ok & gf == level])
    n <- vapply(samples, length, integer(1L))
    if (any(n < 2L)) next
    means <- vapply(samples, mean, numeric(1L))
    sample_variances <- vapply(samples, stats::var, numeric(1L))
    test <- tryCatch(stats::t.test(samples[[1L]], samples[[2L]],
      var.equal = isTRUE(var_equal)), error = function(e) NULL)
    if (is.null(test)) next

    # The visible contract is always first displayed group minus second
    # displayed group.  Do not take the factor coefficient directly: treatment
    # coding represents second minus first and silently reversed the result.
    estimates[[i]] <- means[[1L]] - means[[2L]]
    variances[[i]] <- unname(test$stderr)^2
    complete_dfs[[i]] <- unname(test$parameter)

    group_n[i, ] <- n
    group_means[i, ] <- means
    group_variances[i, ] <- sample_variances
    group_mean_variances[i, ] <- sample_variances / n
    df_imp <- data.frame(y = y[ok], g = droplevels(gf[ok]))
    fits[[i]] <- stats::lm(y ~ g, data = df_imp)

    used_ids <- original_ids[complete$rows[ok]]
    rows_used <- union(rows_used, used_ids)
    for (j in seq_along(levs)) {
      rows_by_group[[j]] <- union(
        rows_by_group[[j]],
        original_ids[complete$rows[ok & gf == levs[[j]]]]
      )
    }
  }
  if (all(is.na(estimates))) {
    stop("No valid fits across imputations for the independent-samples t-test.", call. = FALSE)
  }
  valid_complete_dfs <- complete_dfs[is.finite(complete_dfs) & complete_dfs > 0]
  df_complete <- if (length(valid_complete_dfs)) min(valid_complete_dfs) else NA_real_
  pool <- .rls_mi_pool_scalar(
    estimates, variances, df_complete = df_complete,
    conf_level = conf_level
  )
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
    list(response = response, group = group, group_levels = levs,
         alternative = alternative, conf_level = conf_level,
         var_equal = isTRUE(var_equal)),
    fits_by_imputation = fits,
    estimates_by_imputation = estimates,
    pooled_result = pool
  )

  descriptive_pools <- lapply(seq_along(levs), function(j) {
    group_df <- group_n[, j] - 1
    valid_df <- group_df[is.finite(group_df) & group_df > 0]
    .rls_mi_pool_scalar(
      group_means[, j], group_mean_variances[, j],
      df_complete = if (length(valid_df)) min(valid_df) else NA_real_,
      conf_level = conf_level
    )
  })
  mi_meta$group_mean_pools <- stats::setNames(descriptive_pools, levs)
  descriptive_n <- vapply(seq_along(levs), function(j) {
    values <- group_n[, j]
    values <- values[is.finite(values)]
    # Group membership itself can be imputed, so the effective group count may
    # vary slightly between completed datasets.  The native table displays an
    # integer N; pass the mean effective count (it is rounded only for display)
    # and retain the exact range below for programmatic consumers.
    if (length(values)) mean(values) else NA_real_
  }, numeric(1L))
  descriptive_sd <- vapply(seq_along(levs), function(j) {
    values <- group_variances[, j]
    values <- values[is.finite(values) & values >= 0]
    if (length(values)) sqrt(mean(values)) else NA_real_
  }, numeric(1L))
  display_complete <- .rls_compare_means_complete_data(
    dataset$data, c(response, group), scope,
    record$selected_rows %||% integer(), original_ids
  )
  display_group <- factor(as.character(display_complete$data[[group]]), levels = levs)
  display_y <- display_complete$data[[response]]
  display_ok <- is.finite(display_y) & !is.na(display_group)
  rows_by_group <- lapply(levs, function(level) {
    original_ids[display_complete$rows[display_ok & display_group == level]]
  })
  rows_excluded <- setdiff(original_ids, rows_used)
  list(
    result_id = .rls_compare_means_id(),
    dataset_id = dataset$dataset_id %||% dataset$group,
    analysis_type = "independent_samples_t_test",
    analysis_backend = "multiple_imputation",
    specification = list(
      response = response,
      group = group,
      group_levels = levs,
      alternative = alternative,
      conf_level = conf_level,
      var_equal = isTRUE(var_equal),
      scope = scope
    ),
    descriptives = list(
      group = levs,
      n = descriptive_n,
      mean = vapply(descriptive_pools, `[[`, numeric(1L), "Qbar"),
      sd = descriptive_sd,
      se = vapply(descriptive_pools, `[[`, numeric(1L), "SE"),
      rows_original_ids = unname(rows_by_group),
      n_imputations = pool$m,
      n_min = apply(group_n, 2L, function(x) if (any(is.finite(x))) min(x, na.rm = TRUE) else NA_real_),
      n_max = apply(group_n, 2L, function(x) if (any(is.finite(x))) max(x, na.rm = TRUE) else NA_real_),
      note = paste0(
        "Group means and SEs are pooled using mice::pool.scalar; ",
        "SD is the root mean within-imputation variance."
      )
    ),
    test_results = list(
      method = if (isTRUE(var_equal))
        "Student's t-test (MI pooled)" else "Welch t-test (MI pooled)",
      statistic = pool$statistic,
      parameter = pool$df,
      p_value = p,
      conf_int = ci,
      conf_level = conf_level,
      alternative = alternative,
      mean_diff = pool$Qbar,
      mean_diff_se = pool$SE,
      pooling_method = "mice::pool.scalar (Rubin's rules)",
      group_reference = levs[[2L]],
      group_comparison = levs[[1L]],
      direction_note = sprintf("Pooled mean difference: %s - %s",
                               levs[[1L]], levs[[2L]])
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
  effect <- effectsize::cohens_d(complete$data[[response1]], complete$data[[response2]],
    paired = TRUE, ci = conf_level, alternative = alternative, verbose = FALSE)
  dz <- effect$Cohens_d[[1L]]
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
      method = "student",
      test_family = "student",
      response_type = "numeric",
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
      effect_size_ci = c(effect$CI_low[[1L]], effect$CI_high[[1L]]),
      method = "effectsize::cohens_d (paired differences, noncentral-t interval)"
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
  raw_y1 <- complete$data[[response1]]
  raw_y2 <- complete$data[[response2]]
  first <- .rls_compare_means_rank_values(data, response1, raw_y1)
  second <- .rls_compare_means_rank_values(data, response2, raw_y2)
  ordinal <- length(first$levels) > 0L || length(second$levels) > 0L
  if (ordinal && !identical(first$levels, second$levels)) {
    stop(paste("Paired Wilcoxon requires a common scale: both variables must be numeric,",
      "or ordinal with identical category levels and order. Set a common category order",
      "or explicitly recode both variables to a common numeric scale first."), call. = FALSE)
  }
  ordinal_levels <- first$levels
  y1 <- first$values
  y2 <- second$values
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
                         method = "wilcoxon", test_family = "wilcoxon",
                         response_type = "ordinal", scope = complete$scope %||% "all",
                         ordinal_levels = ordinal_levels),
    descriptives = list(
      variable = c(response1, response2), n = c(n, n),
      mean = c(stats::median(y1), stats::median(y2)),
      sd = c(stats::IQR(y1), stats::IQR(y2)),
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

.rls_paired_samples_mcnemar_ordinary <- function(data, response1, response2, complete,
                                                  alternative, conf_level, original_ids) {
  first_levels <- .rls_compare_means_binary_levels(data, response1, complete$data[[response1]])
  second_levels <- .rls_compare_means_binary_levels(data, response2, complete$data[[response2]])
  first <- factor(as.character(complete$data[[response1]]), levels = first_levels)
  second <- factor(as.character(complete$data[[response2]]), levels = second_levels)
  if (length(first_levels) != 2L || length(second_levels) != 2L) {
    stop("Both variables in a McNemar pair must have exactly two defined categories.",
         call. = FALSE)
  }
  if (!setequal(first_levels, second_levels)) {
    stop(sprintf("Binary variables `%s` and `%s` must use the same two categories for a McNemar test.",
                 response1, response2), call. = FALSE)
  }
  if (anyNA(first) || anyNA(second)) stop("Observed values do not match the defined McNemar categories.", call. = FALSE)
  level_order <- first_levels
  first <- factor(first, levels = level_order)
  second <- factor(second, levels = level_order)
  event <- level_order[[2L]]
  first_event <- as.character(first) == event
  second_event <- as.character(second) == event
  n <- length(first_event)
  if (n < 2L) stop("The selected binary pair has fewer than two complete observations.", call. = FALSE)
  first_only <- sum(first_event & !second_event)
  second_only <- sum(!first_event & second_event)
  discordant <- first_only + second_only
  if (discordant < 1L) {
    stop("The paired binary variables contain no discordant observations, so McNemar's test cannot be calculated.",
         call. = FALSE)
  }
  exact <- stats::binom.test(first_only, discordant, p = 0.5,
                             alternative = alternative, conf.level = conf_level)
  first_proportion <- mean(first_event)
  second_proportion <- mean(second_event)
  difference <- first_proportion - second_proportion
  paired_difference <- as.numeric(first_event) - as.numeric(second_event)
  difference_sd <- stats::sd(paired_difference)
  difference_se <- difference_sd / sqrt(n)
  alpha <- 1 - conf_level
  z <- stats::qnorm(1 - if (identical(alternative, "two.sided")) alpha / 2 else alpha)
  interval <- if (identical(alternative, "greater")) {
    c(max(-1, difference - z * difference_se), 1)
  } else if (identical(alternative, "less")) {
    c(-1, min(1, difference + z * difference_se))
  } else {
    pmax(-1, pmin(1, difference + c(-1, 1) * z * difference_se))
  }
  matched_odds_ratio <- exp(stats::qlogis(exact$estimate))
  matched_odds_ratio <- unname(matched_odds_ratio)
  rows_used <- original_ids[complete$rows]
  rows_excluded <- setdiff(original_ids, rows_used)
  list(
    result_id = .rls_compare_means_id(),
    dataset_id = data$dataset_id %||% data$group,
    analysis_type = "paired_samples_t_test", analysis_backend = "ordinary",
    specification = list(
      response1 = response1, response2 = response2,
      difference = paste(response1, "-", response2),
      alternative = alternative, conf_level = conf_level,
      method = "mcnemar", test_family = "mcnemar",
      response_type = "categorical", event_level = event,
      scope = complete$scope %||% "all"
    ),
    descriptives = list(
      variable = c(response1, response2), n = c(n, n),
      mean = c(first_proportion, second_proportion),
      sd = c(stats::sd(as.numeric(first_event)), stats::sd(as.numeric(second_event))),
      differences = list(n = n, mean = difference, sd = difference_sd,
                         se = difference_se),
      event_level = event, first_only = first_only, second_only = second_only
    ),
    test_results = list(
      method = "Exact McNemar test", statistic = first_only,
      parameter = discordant, p_value = exact$p.value, conf_int = interval,
      conf_level = conf_level, alternative = alternative,
      mean_diff = difference, mean_diff_se = difference_se,
      first_only = first_only, second_only = second_only,
      direction_note = sprintf("Paired event-proportion difference: %s - %s", response1, response2)
    ),
    effect_sizes = list(matched_odds_ratio = matched_odds_ratio,
                        effect_size_label = "Matched odds ratio"),
    post_hoc = NULL, rows_used_original_ids = rows_used,
    rows_excluded_original_ids = rows_excluded, multiple_imputation = NULL,
    result_version = 1L, created_at = Sys.time()
  )
}

.rls_paired_samples_t_test_mi <- function(record, dataset, response1, response2, alternative, conf_level, scope, original_ids) {
  completed <- .rls_mi_completed_datasets(dataset)
  estimates <- numeric()
  variances <- numeric()
  sample_sizes <- numeric()
  first_means <- numeric()
  first_mean_variances <- numeric()
  first_variances <- numeric()
  second_means <- numeric()
  second_mean_variances <- numeric()
  second_variances <- numeric()
  difference_variances <- numeric()
  rows_used <- integer()
  rows_excluded <- integer()
  for (data_imp in completed) {
    complete <- .rls_compare_means_complete_data(
      data_imp, c(response1, response2), scope,
      record$selected_rows %||% integer(), original_ids)
    first <- complete$data[[response1]]
    second <- complete$data[[response2]]
    d <- first - second
    n <- length(d)
    sample_sizes <- c(sample_sizes, n)
    if (n < 2L) {
      estimates <- c(estimates, NA_real_)
      variances <- c(variances, NA_real_)
      first_means <- c(first_means, NA_real_)
      first_mean_variances <- c(first_mean_variances, NA_real_)
      first_variances <- c(first_variances, NA_real_)
      second_means <- c(second_means, NA_real_)
      second_mean_variances <- c(second_mean_variances, NA_real_)
      second_variances <- c(second_variances, NA_real_)
      difference_variances <- c(difference_variances, NA_real_)
      next
    }
    mn <- mean(d)
    s <- stats::sd(d)
    first_variance <- stats::var(first)
    second_variance <- stats::var(second)
    estimates <- c(estimates, mn)
    variances <- c(variances, s^2 / n)
    first_means <- c(first_means, mean(first))
    first_mean_variances <- c(first_mean_variances, first_variance / n)
    first_variances <- c(first_variances, first_variance)
    second_means <- c(second_means, mean(second))
    second_mean_variances <- c(second_mean_variances, second_variance / n)
    second_variances <- c(second_variances, second_variance)
    difference_variances <- c(difference_variances, s^2)
    rows_used <- union(rows_used, original_ids[complete$rows])
    rows_excluded <- union(rows_excluded, setdiff(original_ids, original_ids[complete$rows]))
  }
  if (length(estimates) < 1L || all(is.na(estimates))) {
    stop("No valid paired differences across imputations.", call. = FALSE)
  }
  df_complete <- min(vapply(completed, function(d) {
    cc <- .rls_compare_means_complete_data(
      d, c(response1, response2), scope,
      record$selected_rows %||% integer(), original_ids)
    length(cc$rows) - 1L
  }, numeric(1L)), na.rm = TRUE)
  pool <- .rls_mi_pool_scalar(estimates, variances, df_complete = df_complete, conf_level = conf_level)
  first_pool <- .rls_mi_pool_scalar(
    first_means, first_mean_variances,
    df_complete = df_complete, conf_level = conf_level
  )
  second_pool <- .rls_mi_pool_scalar(
    second_means, second_mean_variances,
    df_complete = df_complete, conf_level = conf_level
  )
  valid_sizes <- sample_sizes[is.finite(estimates) & is.finite(variances)]
  descriptive_n <- if (length(valid_sizes)) mean(valid_sizes) else NA_real_
  root_mean_variance <- function(x) {
    x <- x[is.finite(x) & x >= 0]
    if (length(x)) sqrt(mean(x)) else NA_real_
  }
  first_sd <- root_mean_variance(first_variances)
  second_sd <- root_mean_variance(second_variances)
  difference_sd <- root_mean_variance(difference_variances)
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
  mi_meta$group_mean_pools <- stats::setNames(list(first_pool, second_pool), c(response1, response2))
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
      variable = c(response1, response2),
      n = c(descriptive_n, descriptive_n),
      mean = c(first_pool$Qbar, second_pool$Qbar),
      sd = c(first_sd, second_sd),
      se = c(first_pool$SE, second_pool$SE),
      differences = list(
        n = descriptive_n,
        mean = pool$Qbar,
        sd = difference_sd,
        se = pool$SE
      ),
      pooled_mean_diff = pool$Qbar,
      pooled_se_diff = pool$SE,
      n_imputations = pool$m,
      n_min = if (length(valid_sizes)) min(valid_sizes) else NA_real_,
      n_max = if (length(valid_sizes)) max(valid_sizes) else NA_real_,
      note = paste0(
        "Variable means and SEs are pooled using mice::pool.scalar; ",
        "SDs are the root mean within-imputation variances."
      )
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
      pooling_method = "mice::pool.scalar (Rubin's rules)",
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
  raw_y <- complete$data[[response]]
  ordinal_levels <- if (is.ordered(raw_y)) levels(raw_y) else character()
  y <- if (length(ordinal_levels)) match(as.character(raw_y), ordinal_levels) else raw_y
  k <- nlevels(gf)
  n_total <- length(y)
  group_stats <- lapply(levs, function(lv) {
    vals <- y[gf == lv]
    c(n = length(vals), mean = mean(vals), sd = stats::sd(vals), se = stats::sd(vals) / sqrt(length(vals)))
  })
  group_stats <- do.call(rbind, group_stats)
  rownames(group_stats) <- levs
  if (k < 2L) stop("One-way ANOVA requires at least two effective groups.", call. = FALSE)
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
  epsilon_squared <- if (identical(method, "kruskal_wallis"))
    effectsize::rank_epsilon_squared(y, gf, ci = NULL, verbose = FALSE)$rank_epsilon_squared[[1L]] else NA_real_
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
      ordinal_levels = ordinal_levels,
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
      method = if (identical(method, "kruskal_wallis")) "Kruskal-Wallis test" else
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

.rls_one_way_welch_mi <- function(record, dataset, response, group, conf_level, scope, original_ids) {
  completed <- .rls_mi_completed_datasets(dataset)
  if (length(completed) < 2L) stop("Welch MI pooling requires at least two imputations.", call. = FALSE)
  fits <- vector("list", length(completed))
  complete_results <- vector("list", length(completed))
  rows_used <- rows_excluded <- integer()
  group_levels <- record$group_order
  for (i in seq_along(completed)) {
    complete <- .rls_compare_means_complete_data(completed[[i]], c(response, group), scope,
      record$selected_rows %||% integer(), original_ids)
    gf <- factor(as.character(complete$data[[group]]), levels = group_levels)
    valid <- !is.na(gf)
    current <- data.frame(y = complete$data[[response]][valid], g = gf[valid])
    if (any(table(current$g) < 2L)) stop(sprintf(
      "Welch ANOVA requires at least two observations in every group in imputation %d.", i), call. = FALSE)
    fit <- stats::oneway.test(y ~ g, data = current, var.equal = FALSE)
    if (!is.finite(fit$statistic) || !is.finite(fit$p.value)) stop(sprintf(
      "Welch ANOVA is not estimable in imputation %d (check within-group variances).", i), call. = FALSE)
    fits[[i]] <- fit
    used <- original_ids[complete$rows[valid]]
    complete_results[[i]] <- list(original_rows = used)
    rows_used <- union(rows_used, used)
    rows_excluded <- union(rows_excluded, setdiff(original_ids, used))
  }
  numerator_df <- vapply(fits, function(fit) unname(fit$parameter[[1L]]), numeric(1L))
  if (length(unique(numerator_df)) != 1L) stop("Welch MI pooling requires the same groups in every imputation.", call. = FALSE)
  combined <- miceadds::micombine.F(vapply(fits, function(fit) unname(fit$statistic), numeric(1L)),
    df1 = numerator_df[[1L]], display = FALSE, version = 1)
  if (!is.finite(combined[["D"]]) || !is.finite(combined[["p"]]))
    stop("miceadds::micombine.F could not combine the Welch tests.", call. = FALSE)
  method <- "miceadds::micombine.F (D2 chi-square approximation)"
  pool <- list(ok = TRUE, F = unname(combined[["D"]]), p = unname(combined[["p"]]),
    df1 = unname(combined[["df"]]), df2 = unname(combined[["df2"]]), m = length(fits),
    method = method, RIV = NA_real_, package_result = combined)
  note <- paste("Welch tests are calculated separately in every imputation.",
    "D2 pooling uses a chi-square approximation; finite Welch denominator degrees of freedom are not used in pooling.")
  specification <- list(response = response, group = group, group_levels = group_levels,
    method = "welch", conf_level = conf_level, scope = scope)
  list(result_id = .rls_compare_means_id(), dataset_id = dataset$dataset_id %||% dataset$group,
    analysis_type = "one_way_anova", analysis_backend = "multiple_imputation",
    specification = specification,
    descriptives = list(note = "Descriptive statistics are not pooled in this version.", n_imputations = length(fits)),
    test_results = list(method = "Welch one-way ANOVA (MI pooled, D2 approximation)",
      statistic = pool$F, parameter = c(df1 = pool$df1, df2 = pool$df2), p_value = pool$p,
      conf_level = conf_level, pooling_method = method),
    effect_sizes = NULL, post_hoc = NULL, warnings = note,
    rows_used_original_ids = rows_used, rows_excluded_original_ids = rows_excluded,
    multiple_imputation = .rls_mi_result_metadata(dataset, "one_way_anova", specification,
      fits_by_imputation = fits, pooled_result = pool, pooling_method = method, warnings = note,
      complete_data_result_by_imputation = complete_results),
    result_version = 1L, created_at = Sys.time())
}

.rls_one_way_anova_mi <- function(record, dataset, response, group, alternative, conf_level, scope, original_ids) {
  if (identical(record$method, "welch")) return(.rls_one_way_welch_mi(
    record, dataset, response, group, conf_level, scope, original_ids))
  completed <- .rls_mi_completed_datasets(dataset)
  fits <- vector("list", length(completed))
  rows_used <- integer()
  rows_excluded <- integer()
  group_name <- group
  for (imputation in seq_along(completed)) {
    data_imp <- completed[[imputation]]
    complete <- .rls_compare_means_complete_data(
      data_imp, c(response, group), scope,
      record$selected_rows %||% integer(), original_ids)
    complete_record <- list(
      data = complete$data,
      variable_metadata = dataset$variable_metadata %||% dataset$metadata
    )
    gf <- if (length(record$group_order))
      factor(as.character(complete$data[[group]]), levels = record$group_order) else
      .rls_compare_means_group_factor(complete_record, group)
    levs <- levels(gf)
    df_imp <- data.frame(y = complete$data[[response]], g = gf)
    ok <- stats::complete.cases(df_imp)
    if (sum(ok) < (length(levs) + 1L) || length(unique(df_imp$g[ok])) < 2L) {
      next
    }
    fit <- stats::lm(y ~ g, data = df_imp[ok, , drop = FALSE])
    fits[[imputation]] <- fit
    rows_used <- union(rows_used, original_ids[complete$rows[ok]])
    rows_excluded <- union(rows_excluded, setdiff(original_ids, original_ids[complete$rows[ok]]))
  }
  valid_fit_indices <- which(!vapply(fits, is.null, logical(1L)))
  if (length(valid_fit_indices) != length(completed)) {
    stop(sprintf("One-way ANOVA requires a valid fit in every imputation; invalid imputation(s): %s.",
      paste(setdiff(seq_along(completed), valid_fit_indices), collapse = ", ")), call. = FALSE)
  }
  coef_names <- setdiff(names(stats::coef(fits[[valid_fit_indices[[1L]]]])), "(Intercept)")
  if (!length(coef_names)) {
    stop("No non-intercept coefficients available for pooled ANOVA.", call. = FALSE)
  }
  reduced_fits <- .rls_mi_reduced_fits(fits, intercept_only = TRUE)
  pool <- .rls_mi_pool_d1(fits, reduced_fits, term_names = group)
  if (!isTRUE(pool$ok)) {
    stop("mice::D1 test for ANOVA failed.", call. = FALSE)
  }
  mi_meta <- .rls_mi_result_metadata(
    dataset, "one_way_anova",
    list(response = response, group = group),
    fits_by_imputation = fits,
    pooled_result = pool,
    pooling_method = pool$method
  )
  list(
    result_id = .rls_compare_means_id(),
    dataset_id = dataset$dataset_id %||% dataset$group,
    analysis_type = "one_way_anova",
    analysis_backend = "multiple_imputation",
    specification = list(
      response = response,
      group = group,
      group_levels = record$group_order %||% levels(stats::model.frame(fits[[1L]])$g),
      method = "classical", conf_level = conf_level, scope = scope
    ),
    descriptives = list(
      note = "Descriptive statistics are not pooled in this version.",
      n_imputations = pool$m
    ),
    test_results = list(
      method = sprintf("Classical one-way ANOVA (MI pooled, %s)", pool$method),
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
    one_sample_t_test = "One-Sample Tests",
    independent_samples_t_test = "Two-Sample Tests",
    paired_samples_t_test = "Paired-Samples Tests",
    one_way_anova = "One-Way ANOVA",
    "Compare Means"
  )
  is_mi <- identical(result$analysis_backend, "multiple_imputation")
  if (is_mi) {
    mi_m <- result$multiple_imputation$m %||% NA_integer_
    mi_note <- sprintf(
      "Multiple imputation: m = %d; method = %s.",
      mi_m,
      result$test_results$pooling_method %||% result$multiple_imputation$pooling_method %||% "mice"
    )
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
    es_value <- es$hedges_g %||% es$cohens_dz %||% es$cohens_d %||%
      es$cohens_h %||% es$matched_odds_ratio %||% es$rank_biserial %||%
      es$omega_squared %||% es$epsilon_squared %||% NA_real_
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
    if (is_mi) .rls_native_wire_value(sprintf("MI result: m = %d; %s", mi_m, result$test_results$pooling_method %||% "mice")) else "",
    as.character(result$result_version %||% 1L)
  )
  payload
}

.rls_compare_means_sync_native <- function(result) {
  if (!isTRUE(.rls_state$process_started)) return(invisible(FALSE))
  .rls_send_native_analysis_result(
    .rls_compare_means_native_payload(result),
    "compare-means result"
  )
  invisible(TRUE)
}

.rls_send_native_analysis_result <- function(payload, description, timeout = 5) {
  # Preserve the historical call shape for ordinary result delivery because
  # tests and downstream integrations may replace `.rls_send()` with a simple
  # one-argument transport.  Only modal package-installation results need the
  # extended timeout.
  send <- function() tryCatch(
    if (identical(timeout, 5)) .rls_send(payload)
    else .rls_send(payload, timeout = timeout),
    error = identity
  )
  outcome <- send()
  if (inherits(outcome, "error") && identical(.rls_state$backend_kind, "winui")) {
    # A restarted WinUI process invalidates the long-lived polling socket, but
    # not necessarily the control port. Drop that stale socket and retry the
    # result once on a fresh connection before reporting a real failure.
    .rls_close_winui_task_poll_connection()
    outcome <- send()
  }
  if (inherits(outcome, "error")) {
    stop(
      sprintf("Could not deliver the %s to LinkEDA: %s",
              description, conditionMessage(outcome)),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.rls_compare_means_batch_native_payload <- function(results, run_id = "") {
  if (!length(results)) stop("No compare-means results were produced.", call. = FALSE)
  first <- results[[1L]]
  type <- first$analysis_type
  spec <- first$specification
  mixed_one_sample <- identical(type, "one_sample_t_test") &&
    isTRUE(.rls_state$compare_means_mixed_one_sample)
  mixed_independent <- identical(type, "independent_samples_t_test") &&
    isTRUE(.rls_state$compare_means_mixed_independent)
  mixed_paired <- identical(type, "paired_samples_t_test") &&
    isTRUE(.rls_state$compare_means_mixed_paired)
  mixed_report <- mixed_one_sample || mixed_independent || mixed_paired
  is_mi <- any(vapply(results, function(result) {
    identical(result$analysis_backend %||% "ordinary", "multiple_imputation")
  }, logical(1L)))
  extended_result <- mixed_report || is_mi
  mi_counts <- suppressWarnings(vapply(results, function(result) {
    as.numeric(result$multiple_imputation$m %||%
      result$descriptives$n_imputations %||% NA_real_)
  }, numeric(1L)))
  mi_counts <- mi_counts[is.finite(mi_counts) & mi_counts > 0]
  mi_m <- if (length(mi_counts)) as.integer(max(mi_counts)) else 0L
  pooling_methods <- unique(vapply(results, function(result) {
    as.character(result$test_results$pooling_method %||%
      result$multiple_imputation$pooling_method %||% "")
  }, character(1L)))
  pooling_methods <- pooling_methods[nzchar(pooling_methods)]
  pooling_method <- if (length(pooling_methods)) pooling_methods[[1L]] else "mice"
  title <- switch(type,
    one_sample_t_test = "One-Sample Tests",
    independent_samples_t_test = "Two-Sample Tests",
    paired_samples_t_test = "Paired-Samples Tests",
    one_way_anova = "One-Way ANOVA",
    "Compare Means"
  )
  method <- if (type == "independent_samples_t_test") {
    if (mixed_independent) {
      continuous_method <- .rls_state$compare_means_batch_method %||% "welch"
      if (identical(continuous_method, "mann_whitney"))
        "Mann-Whitney for numeric/ordinal; proportions for binary" else
      if (identical(continuous_method, "student"))
        "Automatic by variable type; continuous: Student" else
        "Automatic by variable type; continuous: Welch"
    } else if (identical(spec$method, "mann_whitney")) "Mann-Whitney U test" else
      if (isTRUE(spec$var_equal)) "Student - equal variances" else "Welch"
  } else if (type == "paired_samples_t_test" && mixed_paired) {
    continuous_method <- .rls_state$compare_means_batch_method %||% "student"
    if (identical(continuous_method, "wilcoxon"))
      "Automatic by pair type; numeric/ordinal: Wilcoxon" else
      "Automatic by pair type; continuous: paired t"
  } else if (type == "one_way_anova") {
    first$test_results$method %||% "Welch one-way ANOVA"
  } else first$test_results$method %||% ""
  group_levels <- spec$group_levels %||% character()
  first_group <- if (length(group_levels) >= 1L) group_levels[[1L]] else ""
  second_group <- if (length(group_levels) >= 2L) group_levels[[2L]] else ""
  adjustment <- first$test_results$p_adjustment %||% "holm"
  output_id <- if (nzchar(run_id)) run_id else .rls_compare_means_id()
  payload <- c(
    "COMPARE_MEANS_BATCH_OPEN",
    if (is_mi) "COMPARE_MEANS_BATCH_V4" else
      if (mixed_report) "COMPARE_MEANS_BATCH_V3" else "COMPARE_MEANS_BATCH_V2",
    .rls_native_wire_value(output_id),
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
    .rls_native_wire_value(adjustment)
  )
  if (is_mi) {
    payload <- c(payload,
      "multiple_imputation",
      as.character(mi_m),
      .rls_native_wire_value(pooling_method)
    )
  }
  payload <- c(payload, as.character(length(results)))
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
    effect_value <- effect$hedges_g %||% effect$cohens_dz %||% effect$cohens_d %||% effect$cohens_h %||%
      effect$matched_odds_ratio %||%
      effect$rank_biserial %||% effect$omega_squared %||% effect$epsilon_squared %||% NA_real_
    # The existing batch wire format uses slots 10 and 11 for McNemar's b and
    # c counts. Keep that transport contract without mislabelling them as the
    # SD and SE in the R result or the individual report.
    mcnemar <- identical(rspec$test_family %||% rspec$method, "mcnemar")
    paired_sd_wire <- if (mcnemar) test$first_only %||% desc$first_only else
      paired_desc$sd %||% NA_real_
    paired_se_wire <- if (mcnemar) test$second_only %||% desc$second_only else
      test$mean_diff_se %||% paired_desc$se %||% NA_real_
    parameter <- test$parameter %||% c(NA_real_)
    interval <- test$conf_int %||% c(NA_real_, NA_real_)
    parameter <- c(parameter, rep(NA_real_, max(0L, 2L - length(parameter))))
    interval <- c(interval, rep(NA_real_, max(0L, 2L - length(interval))))
    values <- c(
      n1, mean1, sd1, se1, n2, mean2, sd2, se2,
      test$mean_diff %||% paired_desc$mean %||% NA_real_,
      paired_sd_wire, paired_se_wire,
      at(interval, 1L), at(interval, 2L),
      test$statistic %||% NA_real_, at(parameter, 1L), at(parameter, 2L),
      test$p_value %||% NA_real_, effect_value,
      test$p_adjusted %||% NA_real_, test$adjustment_family_size %||% 0
    )
    payload <- c(payload,
      .rls_native_wire_value(response1), .rls_native_wire_value(response2),
      .rls_native_wire_value(test$method %||% method),
      .rls_native_wire_value(effect$effect_size_label %||% "")
    )
    if (extended_result) {
      test_family <- rspec$test_family %||%
        if (identical(rspec$method, "wilcoxon") || grepl("Wilcoxon", test$method %||% "")) "wilcoxon" else
        if (identical(rspec$method, "binomial") || grepl("[Bb]inomial", test$method %||% "")) "binomial" else
        if (identical(rspec$method, "mann_whitney") || grepl("Mann", test$method %||% "")) "mann_whitney" else
        if (identical(rspec$method, "proportion") || grepl("[Pp]roportion", test$method %||% "")) "proportion" else
        if (identical(rspec$method, "mcnemar") || grepl("McNemar", test$method %||% "")) "mcnemar" else
        "student"
      response_type <- rspec$response_type %||%
        if (test_family %in% c("binomial", "proportion", "mcnemar")) "categorical" else
        if (test_family %in% c("wilcoxon", "mann_whitney")) "ordinal" else "numeric"
      payload <- c(payload,
        .rls_native_wire_value(test_family),
        .rls_native_wire_value(response_type),
        .rls_native_wire_value(rspec$event_level %||% ""),
        .rls_native_wire_number(rspec$null_mu %||% test$null_value %||% 0)
      )
    }
    payload <- c(payload, vapply(values, .rls_native_wire_number, character(1L)))
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
    if (length(result$specification$ordinal_levels)) warnings <- c(warnings,
      paste0("Ordinal scores (category order): ", paste(sprintf("%s = %d",
        result$specification$ordinal_levels, seq_along(result$specification$ordinal_levels)),
        collapse = "; "), ".", if (identical(result$specification$method, "wilcoxon"))
          " Signed-rank differences use these equally spaced scores." else
          if (identical(result$analysis_type, "one_way_anova"))
            " M and SD summarize category scores; Kruskal-Wallis compares their ranks." else ""))
    if (nzchar(result$effect_sizes$note %||% "")) warnings <- c(warnings, result$effect_sizes$note)
    if (excluded_n > 0L) warnings <- c(warnings, sprintf("%d observations with missing values or outside the selected scope were excluded for %s.", excluded_n, response1))
    payload <- c(payload,
      .rls_native_int_vector_payload(result$rows_used_original_ids %||% integer()),
      .rls_native_int_vector_payload(result$rows_excluded_original_ids %||% integer()),
      as.character(length(warnings)), .rls_native_wire_value(warnings)
    )
  }
  verification <- .rls_compare_means_verification_r_code(results)
  provenance_record <- first
  provenance_record$id <- output_id
  provenance_record$group <- first$dataset_id
  recorded_code <- paste0(
    "# Compare-means statistics were calculated in R before the native table was emitted.\n",
    "compare_means_result <- ",
    paste(utils::capture.output(dput(first$test_results %||% list())), collapse = "\n")
  )
  if (exists(first$dataset_id, envir = .rls_state$datasets, inherits = FALSE)) {
    provenance_record <- .rls_attach_analysis_provenance(
      provenance_record,
      code = recorded_code,
      title = title,
      output_code = list(table = "reference_results"),
      verification_code = list(table = verification$code),
      verification_variables = verification$variables,
      verification_warnings = verification$warnings
    )
  } else {
    # Payload unit tests and external integrations may serialize an already
    # computed result after its source registration has gone away. Preserve a
    # self-contained recipe trailer without inventing a data object.
    provenance_record$analysis_provenance <- list(
      schema = "LinkEDAAnalysisProvenance/v2",
      analysis_id = output_id,
      title = title,
      dataset_id = first$dataset_id,
      dataset_version = 1L,
      object_type = if (is_mi) "multiple_imputation" else "data.frame",
      imputation_count = mi_m,
      code_origin = "reconstructed",
      executed_r_code = recorded_code,
      output_r_code = list(table = "reference_results"),
      verification_r_code = list(table = verification$code),
      verification_variables = verification$variables,
      verification_warnings = verification$warnings
    )
  }
  provenance_record$analysis_provenance$code_origin <- "reconstructed"
  diagnostic_rows <- diagnostic_display <- list()
  offset <- 0L
  for (result in results) {
    value <- .rls_mi_missing_information(result)
    if (!nrow(value)) next
    display <- .rls_mi_diagnostic_display_rows(result,value)
    if (is.null(display)) display <- data.frame(value_row=seq_len(nrow(value))-1L,
      row_type="coefficient",label=value$Variable,variable=value$Variable)
    display$value_row[display$value_row>=0L] <- display$value_row[display$value_row>=0L]+offset
    diagnostic_rows[[length(diagnostic_rows)+1L]] <- value
    diagnostic_display[[length(diagnostic_display)+1L]] <- display
    offset <- offset+nrow(value)
  }
  if(length(diagnostic_rows)) {
    provenance_record$analysis_provenance$missing_information <- do.call(rbind,diagnostic_rows)
    provenance_record$analysis_provenance$missing_information_display_rows <- do.call(rbind,diagnostic_display)
  }
  payload <- c(payload, .rls_analysis_provenance_payload(provenance_record))
  payload
}

.rls_compare_means_sync_native_batch <- function(results) {
  if (!isTRUE(.rls_state$process_started)) return(invisible(FALSE))
  run_id <- .rls_state$compare_means_run_id %||% ""
  .rls_send_native_analysis_result(
    .rls_compare_means_batch_native_payload(results, run_id),
    "compare-means table"
  )
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
                                     method = c("student", "wilcoxon", "binomial"), .selected_rows = NULL) {
  if (missing(method)) method <- "student"
  alternative <- match.arg(alternative)
  p_adjust <- match.arg(p_adjust)
  .rls_compare_means_validate_options(conf_level)
  scope <- match.arg(scope, c("all", "selected", "unselected"))
  dataset <- .rls_dataset_record(data)
  original_ids <- dataset$original_row_ids %||% seq_len(nrow(dataset$data))
  scope_snapshot <- .rls_capture_analysis_scope(dataset, scope, .selected_rows)
  scope <- scope_snapshot$fit_scope
  selected_rows <- if(scope=="all") integer() else scope_snapshot$rows
  responses <- unique(as.character(response %||%
    names(dataset$data)[vapply(dataset$data, is.numeric, logical(1L))]))
  if (!length(responses)) stop("At least one response variable is required.", call. = FALSE)
  for (r in responses) {
    .rls_validate_protocol_name(r, "response")
    if (!r %in% names(dataset$data))
      stop(sprintf("Response variable `%s` was not found.", r), call. = FALSE)
  }
  expand_by_response <- function(values, default, allowed = NULL) {
    if (!length(values)) values <- default
    if (!is.null(names(values)) && all(responses %in% names(values))) {
      values <- values[responses]
    } else if (length(values) == 1L) {
      values <- rep(values, length(responses))
    } else if (length(values) != length(responses)) {
      stop("One value is required for each response variable.", call. = FALSE)
    }
    names(values) <- responses
    if (!is.null(allowed) && any(!values %in% allowed))
      stop("Unsupported one-sample test method.", call. = FALSE)
    values
  }
  methods <- expand_by_response(as.character(method), "student",
                                c("student", "wilcoxon", "binomial"))
  null_values <- suppressWarnings(as.numeric(expand_by_response(mu, 0)))
  names(null_values) <- responses
  if (any(!is.finite(null_values))) stop("Test values must be numeric and finite.", call. = FALSE)
  results <- lapply(responses, function(r) {
    response_method <- methods[[r]]
    null_value <- null_values[[r]]
    if (identical(response_method, "student") && !is.numeric(dataset$data[[r]]))
      stop(sprintf("Response variable `%s` must be numeric for this test.", r), call. = FALSE)
    spec <- list(response = r, null_mu = null_value, alternative = alternative,
                 conf_level = conf_level, scope = scope, method = response_method,
                 test_family = response_method,
                 response_type = if (identical(response_method, "binomial")) "categorical" else
                   if (identical(response_method, "wilcoxon")) "ordinal" else "numeric")
    result <- tryCatch({
      backend <- .rls_analysis_backend(dataset, "one_sample_t_test")
      if (identical(backend, "multiple_imputation")) {
        if (!identical(response_method, "student")) stop("Only one-sample t tests are available for multiple-imputation datasets.", call. = FALSE)
        .rls_one_sample_t_test_mi(list(selected_rows = selected_rows), dataset, r, null_value, alternative, conf_level, scope, original_ids)
      } else {
        complete <- .rls_compare_means_complete_data(
          dataset$data, r, scope, selected_rows, original_ids)
        minimum_n <- if (identical(response_method, "binomial")) 1L else 2L
        if (length(complete$rows) < minimum_n) {
          stop(sprintf("Not enough complete cases for variable `%s`.", r), call. = FALSE)
        }
        if (identical(response_method, "wilcoxon"))
          .rls_one_sample_wilcoxon_ordinary(dataset, r, complete, null_value, alternative, conf_level, original_ids) else
        if (identical(response_method, "binomial"))
          .rls_one_sample_binomial_ordinary(dataset, r, complete, null_value, alternative, conf_level, original_ids) else
          .rls_one_sample_t_test_ordinary(dataset, r, complete, null_value, alternative, conf_level, original_ids)
      }
    }, error = function(e) .rls_compare_means_failed_result(
      dataset, "one_sample_t_test", spec, conditionMessage(e), original_ids))
    result$specification$method <- response_method
    result$specification$test_family <- response_method
    result$specification$response_type <- spec$response_type
    result
  })
  results <- lapply(results, function(result) { result$data_scope <- scope_snapshot; result })
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
  .rls_compare_means_validate_options(conf_level)
  scope <- match.arg(scope, c("all", "selected", "unselected"))
  dataset <- .rls_dataset_record(data)
  original_ids <- dataset$original_row_ids %||% seq_len(nrow(dataset$data))
  if (is.null(group)) stop("A grouping variable is required.", call. = FALSE)
  group <- .rls_compare_means_validate_group(dataset, group)
  scope_snapshot <- .rls_capture_analysis_scope(dataset, scope, .selected_rows)
  scope <- scope_snapshot$fit_scope
  selected_rows <- if(scope=="all") integer() else scope_snapshot$rows
  responses <- unique(as.character(response %||%
    names(dataset$data)[vapply(dataset$data, is.numeric, logical(1L))]))
  if (!length(responses)) stop("At least one response variable is required.", call. = FALSE)
  for (r in responses) {
    .rls_validate_protocol_name(r, "response")
    if (!r %in% names(dataset$data))
      stop(sprintf("Response variable `%s` was not found.", r), call. = FALSE)
  }
  group_factor <- .rls_compare_means_scoped_group_factor(dataset, group, scope,
    selected_rows, original_ids, group_order)
  levs <- levels(group_factor)
  if (length(levs) != 2L) {
    stop(sprintf("Grouping variable `%s` must have exactly two categories, but has %d.", group, length(levs)), call. = FALSE)
  }
  effective_response <- function(r) {
    complete <- .rls_compare_means_complete_data(dataset$data, c(r, group), scope, selected_rows, original_ids)
    complete <- .rls_compare_means_restrict_complete_groups(complete, group_factor)
    complete$data[[r]]
  }
  default_method <- if (isTRUE(var_equal)) "student" else "welch"
  if (is.null(method)) {
    methods <- vapply(responses, function(r) {
      type <- .rls_compare_means_semantic_type(dataset, r)
      if (identical(type, "ordered")) "mann_whitney" else
        if (type %in% c("factor", "logical") &&
            nlevels(droplevels(factor(effective_response(r)))) == 2L) "proportion" else default_method
    }, character(1L))
  } else {
    methods <- as.character(method)
    if (!is.null(names(methods)) && all(responses %in% names(methods))) {
      methods <- methods[responses]
    } else if (length(methods) == 1L) {
      methods <- rep(methods, length(responses))
    } else if (length(methods) != length(responses)) {
      stop("One method is required for each response variable.", call. = FALSE)
    }
    names(methods) <- responses
  }
  if (any(!methods %in% c("welch", "student", "mann_whitney", "proportion")))
    stop("Unsupported independent-samples test method.", call. = FALSE)
  results <- lapply(responses, function(r) {
    response_method <- methods[[r]]
    response_type <- if (identical(response_method, "proportion")) "categorical" else
      if (identical(response_method, "mann_whitney")) "ordinal" else "numeric"
    event_levels <- if (identical(response_method, "proportion"))
      levels(droplevels(factor(effective_response(r)))) else character()
    event_level <- if (length(event_levels) == 2L) event_levels[[2L]] else ""
    spec <- list(response = r, group = group, group_levels = levs,
                 alternative = alternative, conf_level = conf_level,
                 var_equal = identical(response_method, "student"), scope = scope,
                 method = response_method, test_family = response_method,
                 response_type = response_type, event_level = event_level)
    result <- tryCatch({
      backend <- .rls_analysis_backend(dataset, "independent_samples_t_test")
      if (identical(backend, "multiple_imputation")) {
        if (!is.numeric(dataset$data[[r]]) ||
            .rls_compare_means_semantic_type(dataset,r) %in% c("factor","ordered","logical"))
          stop(sprintf("%s: mean comparisons require a numeric variable.",r),call.=FALSE)
        if (!response_method %in% c("welch", "student"))
          stop("Only independent-samples t tests are available for multiple-imputation datasets.", call. = FALSE)
        .rls_independent_samples_t_test_mi(list(selected_rows = selected_rows), dataset, r, group,
                                           levs, alternative, conf_level,
                                           identical(response_method, "student"), scope, original_ids)
      } else {
        complete <- .rls_compare_means_complete_data(
          dataset$data, c(r, group), scope, selected_rows, original_ids)
        complete <- .rls_compare_means_restrict_complete_groups(
          complete, group_factor)
        if (length(unique(complete$data[[group]])) < 2L) {
          stop(sprintf("Not enough complete cases across both groups for `%s`.", r), call. = FALSE)
        }
        if (identical(response_method, "mann_whitney"))
          .rls_independent_samples_mann_whitney_ordinary(dataset, r, group_factor, complete, alternative, conf_level, original_ids, group_name = group) else
        if (identical(response_method, "proportion"))
          .rls_independent_samples_proportion_ordinary(dataset, r, group_factor, complete, alternative, conf_level, original_ids, group_name = group) else {
          if (!is.numeric(dataset$data[[r]]))
            stop(sprintf("Response variable `%s` must be numeric for a t test.", r), call. = FALSE)
          .rls_independent_samples_t_test_ordinary(dataset, r, group_factor, complete,
            alternative, conf_level, identical(response_method, "student"), original_ids,
            group_name = group)
        }
      }
    }, error = function(e) .rls_compare_means_failed_result(
      dataset, "independent_samples_t_test", spec, conditionMessage(e), original_ids))
    result$specification$method <- response_method
    result$specification$test_family <- response_method
    result$specification$response_type <- response_type
    result$specification$event_level <- result$specification$event_level %||% event_level
    result
  })
  results <- lapply(results, function(result) { result$data_scope <- scope_snapshot; result })
  results <- .rls_compare_means_require_any_estimable(results)
  results <- .rls_compare_means_adjust_results(results, p_adjust)
  for (result in results) {
    .rls_compare_means_assign(result)
  }
  .rls_compare_means_sync_native_batch(results)
  invisible(vapply(results, `[[`, character(1L), "result_id"))
}

#' Paired-samples tests
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
                                         method = NULL, .selected_rows = NULL) {
  alternative <- match.arg(alternative)
  p_adjust <- match.arg(p_adjust)
  .rls_compare_means_validate_options(conf_level)
  scope <- match.arg(scope, c("all", "selected", "unselected"))
  dataset <- .rls_dataset_record(data)
  original_ids <- dataset$original_row_ids %||% seq_len(nrow(dataset$data))
  scope_snapshot <- .rls_capture_analysis_scope(dataset, scope, .selected_rows)
  scope <- scope_snapshot$fit_scope
  selected_rows <- if(scope=="all") integer() else scope_snapshot$rows
  if (is.null(pairs) || !is.list(pairs)) stop("`pairs` must be a list of variable-name pairs.", call. = FALSE)
  pair_names <- vapply(pairs, function(pair) {
    r1 <- if (length(pair) >= 1L) as.character(pair[[1L]])[[1L]] else ""
    r2 <- if (length(pair) >= 2L) as.character(pair[[2L]])[[1L]] else ""
    paste(r1, r2, sep = "\u001f")
  }, character(1L))
  if (is.null(method)) {
    methods <- vapply(pairs, function(pair) {
      r1 <- if (length(pair) >= 1L) as.character(pair[[1L]])[[1L]] else ""
      r2 <- if (length(pair) >= 2L) as.character(pair[[2L]])[[1L]] else ""
      if (!r1 %in% names(dataset$data) || !r2 %in% names(dataset$data)) return("student")
      complete <- .rls_compare_means_complete_data(dataset$data, c(r1, r2),
        scope, selected_rows, original_ids)
      first <- complete$data[[r1]]; second <- complete$data[[r2]]
      first_binary <- (is.factor(first) || is.character(first) || is.logical(first) ||
        .rls_compare_means_semantic_type(dataset, r1) %in% c("factor", "ordered")) &&
        length(.rls_compare_means_binary_levels(dataset, r1, first)) == 2L
      second_binary <- (is.factor(second) || is.character(second) || is.logical(second) ||
        .rls_compare_means_semantic_type(dataset, r2) %in% c("factor", "ordered")) &&
        length(.rls_compare_means_binary_levels(dataset, r2, second)) == 2L
      if (first_binary && second_binary) "mcnemar" else
        if (is.ordered(first) || is.ordered(second)) "wilcoxon" else "student"
    }, character(1L))
  } else {
    methods <- as.character(method)
    if (!is.null(names(methods)) && all(pair_names %in% names(methods))) {
      methods <- methods[pair_names]
    } else if (length(methods) == 1L) {
      methods <- rep(methods, length(pairs))
    } else if (length(methods) != length(pairs)) {
      stop("One method is required for each pair.", call. = FALSE)
    }
  }
  if (any(!methods %in% c("student", "wilcoxon", "mcnemar")))
    stop("Unsupported paired-samples test method.", call. = FALSE)
  results <- lapply(seq_along(pairs), function(pair_index) {
    pair <- pairs[[pair_index]]
    r1 <- if (length(pair) >= 1L) as.character(pair[[1L]])[[1L]] else ""
    r2 <- if (length(pair) >= 2L) as.character(pair[[2L]])[[1L]] else ""
    pair_method <- methods[[pair_index]]
    response_type <- if (identical(pair_method, "mcnemar")) "categorical" else
      if (identical(pair_method, "wilcoxon")) "ordinal" else "numeric"
    method_label <- if (identical(pair_method, "wilcoxon")) "Paired Wilcoxon signed-rank test" else
      if (identical(pair_method, "mcnemar")) "Exact McNemar test" else "Paired t-test"
    spec <- list(response1 = r1, response2 = r2, difference = paste(r1, "-", r2),
                 alternative = alternative, conf_level = conf_level, scope = scope,
                 method = pair_method, test_family = pair_method,
                 response_type = response_type)
    result <- tryCatch({
      r1 <- .rls_validate_protocol_name(r1, "response")
      r2 <- .rls_validate_protocol_name(r2, "response")
      if (identical(r1, r2)) stop("Select two different variables for each pair.", call. = FALSE)
      if (!r1 %in% names(dataset$data)) stop(sprintf("Variable `%s` not found.", r1), call. = FALSE)
      if (!r2 %in% names(dataset$data)) stop(sprintf("Variable `%s` not found.", r2), call. = FALSE)
      if (identical(pair_method, "student") &&
          (!is.numeric(dataset$data[[r1]]) || !is.numeric(dataset$data[[r2]])))
        stop("Both variables must be numeric for a paired t test.", call. = FALSE)
      if (identical(pair_method, "wilcoxon") &&
          ((!is.numeric(dataset$data[[r1]]) && !is.factor(dataset$data[[r1]])) ||
           (!is.numeric(dataset$data[[r2]]) && !is.factor(dataset$data[[r2]]))))
        stop("Both variables must be numeric or ordinal for a paired Wilcoxon test.", call. = FALSE)
      backend <- .rls_analysis_backend(dataset, "paired_samples_t_test")
      if (identical(backend, "multiple_imputation")) {
        if (!identical(pair_method, "student")) stop("Only paired t tests are available for multiple-imputation datasets.", call. = FALSE)
        .rls_paired_samples_t_test_mi(list(selected_rows = selected_rows), dataset, r1, r2, alternative, conf_level, scope, original_ids)
      } else {
        complete <- .rls_compare_means_complete_data(
          dataset$data, c(r1, r2), scope, selected_rows, original_ids)
        if (length(complete$rows) < 2L) {
          stop(sprintf("Not enough complete pairs for `%s` and `%s`.", r1, r2), call. = FALSE)
        }
        if (identical(pair_method, "wilcoxon"))
          .rls_paired_samples_wilcoxon_ordinary(dataset, r1, r2, complete, alternative, conf_level, original_ids) else
        if (identical(pair_method, "mcnemar"))
          .rls_paired_samples_mcnemar_ordinary(dataset, r1, r2, complete, alternative, conf_level, original_ids) else
          .rls_paired_samples_t_test_ordinary(dataset, r1, r2, complete, alternative, conf_level, original_ids)
      }
    }, error = function(e) .rls_compare_means_failed_result(
      dataset, "paired_samples_t_test", spec, conditionMessage(e), original_ids))
    result$specification$method <- pair_method
    result$specification$test_family <- pair_method
    result$specification$response_type <- response_type
    if (identical(pair_method, "mcnemar") && is.null(result$specification$event_level)) {
      event_levels <- if (r1 %in% names(dataset$data)) levels(droplevels(factor(dataset$data[[r1]]))) else character()
      result$specification$event_level <- if (length(event_levels) == 2L) event_levels[[2L]] else ""
    }
    result
  })
  results <- lapply(results, function(result) { result$data_scope <- scope_snapshot; result })
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
  group <- .rls_compare_means_validate_group(dataset, group)
  scope_snapshot <- .rls_capture_analysis_scope(dataset, scope, .selected_rows)
  scope <- scope_snapshot$fit_scope
  selected_rows <- if(scope=="all") integer() else scope_snapshot$rows
  responses <- unique(as.character(response %||%
    names(dataset$data)[vapply(dataset$data, is.numeric, logical(1L))]))
  if (!length(responses)) stop("At least one response variable is required.", call. = FALSE)
  for (r in responses) {
    .rls_validate_protocol_name(r, "response")
    if (!r %in% names(dataset$data))
      stop(sprintf("Response variable `%s` was not found.", r), call. = FALSE)
  }
  group_factor <- .rls_compare_means_scoped_group_factor(dataset, group, scope,
    selected_rows, original_ids, group_order)
  results <- lapply(responses, function(r) {
    method_label <- if (identical(method, "kruskal_wallis")) "Kruskal-Wallis test" else
      if (identical(method, "classical")) "Classical one-way ANOVA" else "Welch one-way ANOVA"
    spec <- list(response = r, group = group, group_levels = levels(group_factor),
                 alternative = "two.sided", conf_level = conf_level, scope = scope,
                 method = method_label)
    tryCatch({
      response_type <- .rls_compare_means_semantic_type(dataset, r)
      rank_ordinal <- identical(method, "kruskal_wallis") && is.ordered(dataset$data[[r]])
      if (!rank_ordinal && (!is.numeric(dataset$data[[r]]) ||
          response_type %in% c("factor","ordered","logical")))
        stop(sprintf("%s: ANOVA requires a numeric dependent variable; Kruskal-Wallis also supports ordinal variables.", r), call. = FALSE)
      backend <- .rls_analysis_backend(dataset, "one_way_anova")
      if (identical(backend, "multiple_imputation")) {
        if (identical(method, "kruskal_wallis")) stop("Kruskal-Wallis tests are not available for multiple-imputation datasets.", call. = FALSE)
        .rls_one_way_anova_mi(list(selected_rows = selected_rows, group_order = levels(group_factor), method = method),
          dataset, r, group, "two.sided", conf_level, scope, original_ids)
      } else {
        complete <- .rls_compare_means_complete_data(
          dataset$data, c(r, group), scope, selected_rows, original_ids)
        complete <- .rls_compare_means_restrict_complete_groups(complete, group_factor)
        .rls_one_way_anova_ordinary(dataset, r, group_factor, complete, "two.sided", conf_level, original_ids, group_name = group, method = method)
      }
    }, error = function(e) .rls_compare_means_failed_result(
      dataset, "one_way_anova", spec, conditionMessage(e), original_ids))
  })
  results <- lapply(results, function(result) { result$data_scope <- scope_snapshot; result })
  results <- .rls_compare_means_require_any_estimable(results)
  results <- .rls_compare_means_adjust_results(results, p_adjust)
  for (result in results) {
    .rls_compare_means_assign(result)
  }
  .rls_compare_means_sync_native_batch(results)
  invisible(vapply(results, `[[`, character(1L), "result_id"))
}
