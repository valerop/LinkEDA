.lognormal_audit_data <- function() {
  set.seed(61)
  d <- data.frame(g = factor(rep(c("A", "B"), each = 30)))
  d$y <- exp(1 + .6 * (d$g == "B") + rnorm(60, sd = 1.4))
  d
}

.lognormal_audit_recipe <- function(code, data) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(data, path)
  env <- new.env(parent = globalenv())
  env$verification_data_path <- path
  invisible(capture.output(eval(parse(text = code), env)))
  env$reference
}

test_that("lognormal arithmetic means are exact, including large residual variance", {
  skip_if_not_installed("emmeans")
  d <- .lognormal_audit_data()
  model <- ls_new_positive_continuous_model(d, "y", "g", distribution = "lognormal", native = FALSE)
  result <- ls_generalized_linear_model_interaction(model, "g")
  fit <- lm(log(y) ~ g, d)
  eta <- predict(fit, data.frame(g = levels(d$g)))
  exact <- vapply(eta, function(z) integrate(function(y)
    y * dlnorm(y, z, sigma(fit)), 0, Inf)$value, numeric(1))
  expect_equal(result$plot_data$pooled_estimate, unname(exact), tolerance = 1e-7)
  grid <- emmeans::emmeans(fit, "g", offset = sigma(fit)^2 / 2)
  ref <- as.data.frame(summary(grid, type = "response", infer = c(TRUE, FALSE)))
  expect_equal(result$plot_data$pooled_std_error, ref$SE)
  expect_equal(result$plot_data$pooled_conf_low, ref$lower.CL)
  expect_equal(result$plot_data$pooled_conf_high, ref$upper.CL)
  expect_match(result$interpretation, "residual variance as fixed", fixed = TRUE)
  expect_false(is.null(result$analysis_provenance))
  expect_true("ANALYSIS_PROVENANCE_V2" %in% LinkEDA:::.rls_interaction_native_plot_payload(result))
  skip_if_not_installed("gtsummary")
  recipe <- result$analysis_provenance$verification_r_code$effect
  expect_false(grepl("LinkEDA:::", recipe, fixed = TRUE))
  reproduced <- .lognormal_audit_recipe(recipe, d)
  expect_equal(reproduced$response, result$plot_data$pooled_estimate)
  expect_equal(reproduced$SE, result$plot_data$pooled_std_error)
})

test_that("lognormal pairwise respects response, link, ratio, adjustment and confidence", {
  skip_if_not_installed("emmeans")
  d <- .lognormal_audit_data()
  d$g <- factor(rep(c("A", "B", "C"), each = 20))
  model <- ls_new_positive_continuous_model(d, "y", "g", distribution = "lognormal", native = FALSE)
  fit <- lm(log(y) ~ g, d)
  for (scale in c("response", "link", "multiplicative_ratio")) {
    for (adjust in c("none", "tukey", "holm")) {
      result <- ls_generalized_linear_model_pairwise(model, "g", scale, adjust, .90)
      grid <- emmeans::emmeans(fit, "g")
      if (scale == "response") grid <- emmeans::regrid(grid, transform = "response")
      ref <- as.data.frame(summary(emmeans::contrast(grid, "pairwise", adjust = adjust),
        infer = c(TRUE, TRUE), type = "link", level = .90))
      expect_equal(result$pooled_estimate, ref$estimate)
      expect_equal(result$pooled_std_error, ref$SE)
      expect_equal(result$pooled_p_value, ref$p.value)
      expect_equal(result$pooled_conf_low, ref$lower.CL)
      expect_equal(result$pooled_conf_high, ref$upper.CL)
      if (scale == "multiplicative_ratio") expect_equal(result$ratio, exp(ref$estimate))
    }
  }
  result <- ls_generalized_linear_model_pairwise(model, "g")
  expect_match(result$pooling_method[1], "conditional medians", fixed = TRUE)
  expect_identical(result$response_measure[[1L]], "conditional median")
  report <- LinkEDA:::.rls_pairwise_scale_table_lines(result)
  expect_true(any(grepl("Median first (response)", report, fixed = TRUE)))
  expect_true(any(grepl("Median difference", report, fixed = TRUE)))
  expect_false(any(grepl("Mean first (response)", report, fixed = TRUE)))
  expect_true(any(grepl("Conditional medians: response scale",
    LinkEDA:::.rls_pairwise_scale_notes(result), fixed = TRUE)))
  skip_if_not_installed("gtsummary")
  recipe <- attr(result, "analysis_provenance")$verification_r_code$pairwise
  expect_false(grepl("LinkEDA:::", recipe, fixed = TRUE))
  reproduced <- .lognormal_audit_recipe(recipe, d)
  expect_equal(reproduced$estimate, result$pooled_estimate)
  expect_equal(reproduced$p.value, result$pooled_p_value)
})

test_that("MI lognormal effects transform each imputation and pairwise uses emmeans mira", {
  skip_if_not_installed("emmeans"); skip_if_not_installed("mice")
  d <- .lognormal_audit_data()
  completed <- lapply(c(.7, 1, 1.3), function(s) transform(d, y = y^s))
  original <- d; original$y[c(1, 35)] <- NA_real_
  id <- ls_register_dataset("lognormal_rank_audit_mi", completed[[1]])
  on.exit(ls_unregister_dataset(id))
  record <- LinkEDA:::.rls_dataset_record(id)
  record$dataset_type <- "multiple_imputation"
  record$imputation_id <- "lognormal_rank_audit_mi_imp"
  record$source_dataset_id <- "lognormal_rank_audit_mi_source"
  record$original_data <- original; record$completed_datasets <- completed
  record$missing_cell_mask <- as.data.frame(lapply(original, is.na))
  record$imputed_cell_map <- list(); record$active_imputation_version <- 1L
  record$imputation_display_mode <- "version"; record$imputation_count <- 3L
  record$original_row_ids <- seq_len(nrow(d))
  LinkEDA:::.rls_set_dataset_record(record)
  model <- ls_new_positive_continuous_model(id, "y", "g", distribution = "lognormal", native = FALSE)
  fits <- lapply(completed, function(data) lm(log(y) ~ g, data))
  result <- ls_generalized_linear_model_interaction(model, "g")
  means <- lapply(fits, function(fit) as.data.frame(summary(
    emmeans::emmeans(fit, "g", offset = sigma(fit)^2 / 2), type = "response")))
  pools <- lapply(1:2, function(i) mice::pool.scalar(
    vapply(means, function(x) x$response[i], numeric(1)),
    vapply(means, function(x) x$SE[i]^2, numeric(1)), n = 59, k = 1))
  expect_equal(result$plot_data$pooled_estimate, vapply(pools, `[[`, numeric(1), "qbar"))
  expect_equal(result$plot_data$pooled_std_error, sqrt(vapply(pools, `[[`, numeric(1), "t")))
  expect_equal(result$plot_data$pooled_df, vapply(pools, `[[`, numeric(1), "df"))
  pair <- ls_generalized_linear_model_pairwise(model, "g", adjust = "none")
  grid <- stats::update(emmeans::emmeans(mice::as.mira(fits), "g", data = completed[[1]]), tran = "log")
  ref <- as.data.frame(summary(emmeans::contrast(emmeans::regrid(grid, transform = "response"),
    "pairwise", adjust = "none"), infer = TRUE))
  expect_equal(pair$pooled_estimate, ref$estimate)
  expect_equal(pair$pooled_std_error, ref$SE)
  expect_equal(pair$pooled_p_value, ref$p.value)
  expect_true(nrow(attr(pair, "missing_information")) > 0)
  skip_if_not_installed("gtsummary")
  long <- do.call(rbind, lapply(seq_along(completed), function(i)
    cbind(.imp = i, .id = seq_len(nrow(d)), completed[[i]])))
  reproduced <- .lognormal_audit_recipe(result$analysis_provenance$verification_r_code$effect, long)
  expect_equal(reproduced$response, result$plot_data$pooled_estimate)
  expect_equal(reproduced$SE, result$plot_data$pooled_std_error)
  expect_equal(reproduced$df, result$plot_data$pooled_df)
  pairwise_check <- .lognormal_audit_recipe(result$analysis_provenance$verification_r_code$table, long)
  expect_equal(pairwise_check$mean_first, result$comparisons$mean_first)
  expect_equal(pairwise_check$mean_second, result$comparisons$mean_second)
  expect_equal(pairwise_check$estimate, result$comparisons$link_estimate)
  expect_equal(pairwise_check$SE, result$comparisons$link_std_error)

  reproduced_pair <- .lognormal_audit_recipe(attr(pair, "analysis_provenance")$verification_r_code$pairwise, long)
  expect_equal(reproduced_pair$estimate, pair$pooled_estimate)
  expect_equal(reproduced_pair$p.value, pair$pooled_p_value)
})

test_that("rank tests reject nominal categories regardless of factor level order", {
  d <- data.frame(y = factor(c(rep("red", 8), rep("green", 4), rep("blue", 8))),
    g = factor(rep(c("A", "B"), each = 10)))
  for (lev in list(c("red", "green", "blue"), c("green", "red", "blue"))) {
    d$y <- factor(d$y, levels = lev); d$z <- rev(d$y)
    dataset <- ls_register_dataset(paste0("rank_audit_nominal_", lev[1]), d)
    on.exit(ls_unregister_dataset(dataset), add = TRUE)
    expect_error(ls_new_one_sample_t_test(dataset, "y", method = "wilcoxon"), "no defined order")
    expect_error(ls_new_independent_samples_t_test(dataset, "y", "g", method = "mann_whitney"), "no defined order")
    expect_error(ls_new_paired_samples_t_test(dataset, list(c("y", "z")), method = "wilcoxon"), "no defined order")
  }
  # Semantic Categorical must also be rejected when physically stored numeric.
  record <- list(data = data.frame(y = 1:6), variable_metadata = NULL)
  record$variable_metadata <- LinkEDA:::.rls_dataset_record(ls_register_dataset("rank_audit_semantic", record$data))$variable_metadata
  record$variable_metadata$current_analysis_type[record$variable_metadata$variable_name == "y"] <- "factor"
  on.exit(ls_unregister_dataset("rank_audit_semantic"))
  expect_error(LinkEDA:::.rls_compare_means_rank_values(record, "y", 1:6), "no defined order")
})

test_that("valid numeric and explicitly ordered rank tests still match stats wilcox.test", {
  for (ordinal in c(FALSE, TRUE)) {
    values <- c(1, 1, 2, 3, 2, 3, 4, 5, 4, 5, 5, 4)
    d <- data.frame(y = if (ordinal) ordered(values, levels = 1:5) else values,
      g = factor(rep(c("A", "B"), each = 6)))
    dataset <- ls_register_dataset(paste0("rank_audit_valid_", ordinal), d)
    on.exit(ls_unregister_dataset(dataset), add = TRUE)
    id <- ls_new_independent_samples_t_test(dataset, "y", "g", method = "mann_whitney")
    actual <- LinkEDA:::.rls_compare_means_record(id[[1]])
    ref <- suppressWarnings(wilcox.test(values[1:6], values[7:12], exact = FALSE, conf.int = TRUE))
    expect_equal(actual$test_results$p_value, ref$p.value)
    id <- ls_new_one_sample_t_test(dataset, "y", mu = 2, method = "wilcoxon")
    actual <- LinkEDA:::.rls_compare_means_record(id[[1]])
    ref <- suppressWarnings(wilcox.test(values, mu = 2, exact = FALSE, conf.int = TRUE))
    expect_equal(actual$test_results$p_value, ref$p.value)
    expect_equal(actual$specification$ordinal_levels, if (ordinal) as.character(1:5) else character())
  }
})


test_that("rank batches retain valid responses and verification uses explicit ordinal order", {
  d <- data.frame(
    rating = ordered(rep(c("high", "low", "middle", "low"), 5),
      levels = c("low", "middle", "high")),
    nominal = factor(rep(c("a", "c", "b", "a"), 5)),
    g = factor(rep(c("A", "B"), each = 10)))
  dataset <- ls_register_dataset("rank_batch_audit", d)
  on.exit(ls_unregister_dataset(dataset))
  ids <- ls_new_independent_samples_t_test(dataset, c("rating", "nominal"), "g", method = "mann_whitney")
  results <- lapply(ids, LinkEDA:::.rls_compare_means_record)
  expect_equal(results[[1]]$specification$ordinal_levels, c("low", "middle", "high"))
  expect_true(is.finite(results[[1]]$test_results$p_value))
  expect_true(is.na(results[[2]]$test_results$p_value))
  code <- LinkEDA:::.rls_compare_means_verification_r_code(results[1])$code
  expect_match(code, '"low", "middle", "high"', fixed = TRUE)
  expect_match(code, "stats::wilcox.test", fixed = TRUE)
})
