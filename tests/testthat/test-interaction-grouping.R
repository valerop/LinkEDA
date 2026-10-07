test_that("interaction tables can repeatedly exchange comparison and grouping factors", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("mice")
  set.seed(716)
  data <- expand.grid(country = factor(c("Italy", "Cyprus", "Finland")),
                      group = factor(c("Control", "Treatment")), replicate = 1:25)
  completed <- lapply(1:3, function(i) transform(data,
    y = 10 + 2 * (group == "Treatment") + as.integer(country) +
      (group == "Treatment") * (country == "Italy") + rnorm(nrow(data), sd = 2)))
  for (mi in c(FALSE, TRUE)) {
    id <- ls_register_dataset(paste0("grouping-test-", mi), completed[[1]])
    if (mi) {
      dataset <- LinkEDA:::.rls_dataset_record(id)
      dataset$dataset_type <- "multiple_imputation"
      dataset$imputation_id <- "grouping-imputations"
      dataset$original_data <- completed[[1]]
      dataset$completed_datasets <- completed
      dataset$imputation_count <- length(completed)
      dataset$original_row_ids <- seq_len(nrow(data))
      dataset$active_imputation_version <- 1L
      dataset$imputation_display_mode <- "version"
      LinkEDA:::.rls_set_dataset_record(dataset)
    }
    model <- ls_new_glm(id)
    ls_glm_set_dependent(model, "y")
    for (term in c("country", "group", "country:group")) ls_glm_add_predictor(model, term)
    ls_glm_fit(model)
    state <- LinkEDA:::.rls_glm_model_record(model)
    fits <- LinkEDA:::.rls_regression_post_estimation_fits(state)
    target <- if (mi) mice::as.mira(fits) else fits[[1]]
    for (focal in c("country", "group", "country")) {
      by <- setdiff(c("country", "group"), focal)
      actual <- ls_glm_interaction(model, "country:group", focal = focal)
      expect_identical(actual$focal, focal)
      expect_identical(actual$conditioning, by)
      grid <- emmeans::emmeans(target, specs = focal, by = by)
      reference <- as.data.frame(summary(grid))
      expect_equal(actual$estimates$pooled_estimate, reference$emmean, tolerance = 1e-10)
      pairs <- as.data.frame(summary(emmeans::contrast(grid, "pairwise", adjust = "tukey"), infer = TRUE))
      expect_equal(actual$comparisons$pooled_estimate, pairs$estimate, tolerance = 1e-10)
      expect_equal(actual$comparisons$pooled_p_value, pairs$p.value, tolerance = 1e-10)
      expect_true(all(actual$interaction_contrasts$contrast_factor == focal))
      expect_true(all(actual$interaction_contrasts$comparison_factor == by))
      recipe <- paste0("/tmp/linkeda-grouping-", by, ".R")
      if (file.exists(recipe)) {
        path <- tempfile(fileext = ".rds")
        saveRDS(fits, path)
        Sys.setenv(LINKEDA_GROUPING_FITS = path)
        env <- new.env(parent = globalenv())
        capture.output(sys.source(recipe, env))
        checks <- if (mi) do.call(rbind, env$interaction_checks) else env$interaction_tables[[1]]
        # emmeans lists the conditioning factor first in its second contrast table.
        expect_equal(sort(checks$estimate), sort(actual$interaction_contrasts$pooled_estimate), tolerance = 1e-10)
        expect_equal(sort(checks$SE), sort(actual$interaction_contrasts$pooled_std_error), tolerance = 1e-10)
        expect_equal(sort(if (mi) checks$p else checks$p.value),
                     sort(actual$interaction_contrasts$pooled_p_value), tolerance = 1e-10)
        unlink(path)
      }
    }
    ls_unregister_dataset(id)
  }
})

test_that("three-factor interactions change grouping without changing the fitted estimand", {
  skip_if_not_installed("emmeans")
  set.seed(1307)
  data <- expand.grid(country = factor(c("Italy", "Cyprus")),
                      group = factor(c("Control", "Treatment")),
                      period = factor(c("Before", "After")), replicate = 1:12)
  data$y <- 4 + as.integer(data$country) + 2 * (data$group == "Treatment") +
    (data$period == "After") +
    (data$country == "Italy") * (data$group == "Treatment") *
    (data$period == "After") + rnorm(nrow(data))
  id <- ls_register_dataset("three-factor-grouping-test", data)
  on.exit(ls_unregister_dataset(id), add = TRUE)
  model <- ls_new_glm(id)
  ls_glm_set_dependent(model, "y")
  for (term in c("country", "group", "period", "country:group",
                 "country:period", "group:period", "country:group:period")) {
    ls_glm_add_predictor(model, term)
  }
  ls_glm_fit(model)
  state <- LinkEDA:::.rls_glm_model_record(model)
  fit <- LinkEDA:::.rls_regression_post_estimation_fits(state)[[1L]]
  for (group_by in c("period", "group", "period")) {
    other <- setdiff(c("group", "period"), group_by)
    result <- ls_glm_interaction(model, "country:group:period",
                                 focal = "country", group_by = group_by)
    expect_identical(result$focal, "country")
    expect_identical(result$moderator, group_by)
    expect_identical(result$conditioning, c(group_by, other))
    report <- LinkEDA:::.rls_interaction_native_report(result)
    expect_true(any(startsWith(report, paste0(
      paste(c("country", group_by, other, "Estimate"), collapse = "\t"), "\t"))))
    grid <- emmeans::emmeans(fit, specs = "country", by = c(group_by, other))
    expect_equal(result$estimates$pooled_estimate,
                 as.data.frame(summary(grid))$emmean, tolerance = 1e-10)
    expect_true(all(result$interaction_contrasts$comparison_factor == group_by))
    expect_identical(attr(result$interaction_contrasts,
                          "interaction_contrast")$conditioning_factors, other)
    recipe <- result$analysis_provenance$verification_r_code$table
    expect_true(is.character(recipe) && length(recipe) == 1L)
    expect_true(grepl(group_by, recipe, fixed = TRUE))
  }
  expect_error(ls_glm_interaction(model, "country:group:period",
                                  focal = "country", group_by = "country"),
               "different")
})

test_that("three-factor grouping also follows all completed imputations", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("mice")
  set.seed(1308)
  data <- expand.grid(country = factor(c("Italy", "Cyprus")),
                      group = factor(c("Control", "Treatment")),
                      period = factor(c("Before", "After")), replicate = 1:12)
  data$y <- 3 + as.integer(data$country) + (data$group == "Treatment") +
    (data$period == "After") + rnorm(nrow(data))
  completed <- list(data, transform(data, y = y + rnorm(nrow(data), sd = .2)))
  id <- ls_register_dataset("three-factor-mi-grouping-test", completed[[1L]])
  on.exit(ls_unregister_dataset(id), add = TRUE)
  dataset <- LinkEDA:::.rls_dataset_record(id)
  dataset$dataset_type <- "multiple_imputation"
  dataset$imputation_id <- "three-factor-grouping-imputations"
  dataset$original_data <- completed[[1L]]
  dataset$completed_datasets <- completed
  dataset$imputation_count <- length(completed)
  dataset$original_row_ids <- seq_len(nrow(data))
  dataset$active_imputation_version <- 1L
  dataset$imputation_display_mode <- "version"
  LinkEDA:::.rls_set_dataset_record(dataset)
  model <- ls_new_glm(id)
  ls_glm_set_dependent(model, "y")
  for (term in c("country", "group", "period", "country:group",
                 "country:period", "group:period", "country:group:period")) {
    ls_glm_add_predictor(model, term)
  }
  ls_glm_fit(model)
  result <- ls_glm_interaction(model, "country:group:period",
                               focal = "country", group_by = "period")
  expect_identical(result$conditioning, c("period", "group"))
  fits <- LinkEDA:::.rls_regression_post_estimation_fits(
    LinkEDA:::.rls_glm_model_record(model))
  reference <- emmeans::emmeans(mice::as.mira(fits), specs = "country",
                               by = c("period", "group"))
  expect_equal(result$estimates$pooled_estimate,
               as.data.frame(summary(reference))$emmean, tolerance = 1e-10)
})
