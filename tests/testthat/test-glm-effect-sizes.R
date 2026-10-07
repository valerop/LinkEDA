test_that("ordinary linear-model effect sizes separate partial r from delta R-squared", {
  set.seed(1042)
  n <- 120L
  x_positive <- stats::rnorm(n)
  x_negative <- stats::rnorm(n)
  y <- 1.5 + 0.8 * x_positive - 0.55 * x_negative + stats::rnorm(n, sd = 0.7)
  raw_zero <- rep(c(-1, 1), length.out = n)
  x_zero <- stats::residuals(stats::lm(raw_zero ~ x_positive + x_negative + y))
  data <- data.frame(y, x_positive, x_negative, x_zero)

  id <- ls_register_dataset("glm_effect_sizes_numeric", data)
  on.exit(ls_unregister_dataset(id), add = TRUE)
  model <- ls_new_glm(id)
  ls_glm_set_dependent(model, "y")
  for (term in c("x_positive", "x_negative", "x_zero")) {
    ls_glm_add_predictor(model, term)
  }
  model <- ls_glm_fit(model)
  record <- LinkEDA:::.rls_glm_model_record(model)
  coefs <- ls_glm_coefficients(model)
  rows <- ls_glm_coefficient_rows(model)

  intercept <- rows[rows$term == "(Intercept)", , drop = FALSE]
  expect_true(is.na(intercept$partial_r))
  expect_true(is.na(intercept$delta_r2))

  ordinary_fit <- record$fit
  expected_partial <- with(
    coefs,
    t_value / sqrt(t_value^2 + stats::df.residual(ordinary_fit))
  )
  names(expected_partial) <- coefs$term
  expect_equal(coefs$partial_r[coefs$term != "(Intercept)"],
               unname(expected_partial[coefs$term != "(Intercept)"]), tolerance = 1e-12)
  expect_gt(expected_partial[["x_positive"]], 0)
  expect_lt(expected_partial[["x_negative"]], 0)

  full_r2 <- summary(ordinary_fit)$r.squared
  for (term in c("x_positive", "x_negative", "x_zero")) {
    reduced_terms <- setdiff(c("x_positive", "x_negative", "x_zero"), term)
    reduced <- stats::lm(stats::reformulate(reduced_terms, response = "y"), data = data)
    expected_delta <- full_r2 - summary(reduced)$r.squared
    displayed <- rows$delta_r2[rows$source_term == term & rows$row_type == "coefficient"]
    expect_equal(displayed, max(0, expected_delta), tolerance = 1e-10)
  }
  expect_lt(abs(rows$delta_r2[rows$source_term == "x_zero"]), 1e-10)
})

test_that("factor and interaction effect sizes are placed on their statistical level", {
  set.seed(2091)
  n <- 180L
  x <- stats::rnorm(n)
  group <- factor(rep(c("A", "B", "C"), each = n / 3L), levels = c("A", "B", "C"))
  y <- 2 + 0.6 * x + ifelse(group == "B", 0.7, ifelse(group == "C", -0.4, 0)) +
    ifelse(group == "B", 0.35 * x, ifelse(group == "C", -0.2 * x, 0)) +
    stats::rnorm(n, sd = 0.8)
  data <- data.frame(y, x, group)

  id <- ls_register_dataset("glm_effect_sizes_factor", data)
  on.exit(ls_unregister_dataset(id), add = TRUE)
  model <- ls_new_glm(id)
  ls_glm_set_dependent(model, "y")
  ls_glm_add_predictor(model, "x")
  ls_glm_add_predictor(model, "group")
  ls_glm_add_predictor(model, "x:group")
  model <- ls_glm_fit(model)
  record <- LinkEDA:::.rls_glm_model_record(model)
  rows <- ls_glm_coefficient_rows(model)

  factor_parent <- rows[rows$source_term == "group" & rows$row_type == "factor_parent", ]
  factor_reference <- rows[rows$source_term == "group" & rows$row_type == "reference", ]
  factor_contrasts <- rows[rows$source_term == "group" & rows$row_type == "factor_level", ]
  interaction_parent <- rows[rows$source_term == "x:group" & rows$row_type == "term_parent", ]
  interaction_contrasts <- rows[rows$source_term == "x:group" & rows$row_type == "coefficient", ]

  expect_length(factor_parent$delta_r2, 1L)
  expect_true(is.finite(factor_parent$delta_r2))
  expect_true(is.na(factor_parent$partial_r))
  expect_true(all(is.na(factor_reference$partial_r)))
  expect_true(all(is.na(factor_reference$delta_r2)))
  expect_true(all(is.finite(factor_contrasts$partial_r)))
  expect_true(all(is.na(factor_contrasts$delta_r2)))
  expect_true(is.finite(interaction_parent$delta_r2))
  expect_true(is.na(interaction_parent$partial_r))
  expect_true(all(is.finite(interaction_contrasts$partial_r)))
  expect_true(all(is.na(interaction_contrasts$delta_r2)))

  full_r2 <- summary(record$fit)$r.squared
  reduced_factor <- stats::lm(y ~ x + x:group, data = data)
  reduced_interaction <- stats::lm(y ~ x + group, data = data)
  expect_equal(factor_parent$delta_r2,
               full_r2 - summary(reduced_factor)$r.squared, tolerance = 1e-10)
  expect_equal(interaction_parent$delta_r2,
               full_r2 - summary(reduced_interaction)$r.squared, tolerance = 1e-10)
})

test_that("intercept-only models do not invent coefficient effect sizes", {
  data <- data.frame(y = c(1.2, 2.0, 2.8, 4.1, 5.0))
  fit <- stats::lm(y ~ 1, data = data)
  extracted <- LinkEDA:::.rls_glm_extract_fit(
    list(data = data, predictors = character()),
    fit,
    list(data = data, rows = seq_len(nrow(data)), excluded = 0L)
  )
  expect_equal(nrow(extracted$coefficient_rows), 1L)
  expect_true(is.na(extracted$coefficient_rows$partial_r))
  expect_true(is.na(extracted$coefficient_rows$delta_r2))
  expect_length(LinkEDA:::.rls_model_term_delta_r2(fit), 0L)
})

test_that("centering a numeric GLM predictor changes only the model origin", {
  data <- data.frame(
    y = c(3, 5.2, 6.8, 9.4, 10.7),
    x = c(1, 2, 3, 4, 5)
  )
  id <- ls_register_dataset("glm_centering_numeric", data)
  on.exit(ls_unregister_dataset(id), add = TRUE)

  model <- ls_new_glm(id)
  ls_glm_set_dependent(model, "y")
  ls_glm_add_predictor(model, "x")
  model <- ls_glm_fit(model)
  raw <- LinkEDA:::.rls_glm_model_record(model)

  centered_record <- raw
  centered_record$centered_predictors <- "x"
  centered_model <- LinkEDA:::.rls_assign_glm_model(centered_record)
  centered_model <- ls_glm_fit(centered_model)
  centered <- LinkEDA:::.rls_glm_model_record(centered_model)

  expect_equal(unname(stats::coef(raw$fit)[["x"]]),
               unname(stats::coef(centered$fit)[["x"]]), tolerance = 1e-12)
  expect_false(isTRUE(all.equal(unname(stats::coef(raw$fit)[["(Intercept)"]]), mean(data$y))))
  expect_equal(unname(stats::coef(centered$fit)[["(Intercept)"]]), mean(data$y), tolerance = 1e-12)
  expect_equal(unname(stats::fitted(raw$fit)), unname(stats::fitted(centered$fit)), tolerance = 1e-12)
  expect_equal(centered$centered_predictors, "x")
  expect_equal(unname(centered$predictor_centers[["x"]]), mean(data$x), tolerance = 1e-12)
})

test_that("GLM centering uses fitted complete cases and is reversible", {
  data <- data.frame(
    y = c(2, 5, 203, 10, 14.2),
    x = c(1, 2, 100, 4, 5),
    z = c(0, 1, NA, 2, 4)
  )
  id <- ls_register_dataset("glm_centering_complete_cases", data)
  on.exit(ls_unregister_dataset(id), add = TRUE)
  model <- ls_new_glm(id)
  ls_glm_set_dependent(model, "y")
  ls_glm_add_predictor(model, "x")
  ls_glm_add_predictor(model, "z")
  raw <- LinkEDA:::.rls_glm_model_record(ls_glm_fit(model))

  centered_spec <- raw
  centered_spec$centered_predictors <- "x"
  centered <- LinkEDA:::.rls_glm_model_record(
    ls_glm_fit(LinkEDA:::.rls_assign_glm_model(centered_spec))
  )
  expect_equal(unname(centered$predictor_centers[["x"]]), 3, tolerance = 1e-12)
  expect_identical(centered$rows_used, c(1L, 2L, 4L, 5L))
  expect_equal(unname(stats::fitted(raw$fit)), unname(stats::fitted(centered$fit)), tolerance = 1e-10)
  expect_equal(unname(stats::residuals(raw$fit)), unname(stats::residuals(centered$fit)), tolerance = 1e-10)
  expect_equal(summary(raw$fit)$r.squared, summary(centered$fit)$r.squared, tolerance = 1e-12)

  restored_spec <- centered
  restored_spec$centered_predictors <- character()
  restored <- LinkEDA:::.rls_glm_model_record(
    ls_glm_fit(LinkEDA:::.rls_assign_glm_model(restored_spec))
  )
  expect_length(restored$predictor_centers, 0L)
  expect_equal(unname(stats::coef(restored$fit)), unname(stats::coef(raw$fit)), tolerance = 1e-10)
})

test_that("GLM factor references reparameterize main effects and interactions", {
  group <- factor(rep(c("A", "B", "C"), each = 4L), levels = c("A", "B", "C"))
  x <- rep(1:4, 3L)
  y <- 10 + 2 * x + ifelse(group == "B", 4, ifelse(group == "C", -3, 0)) +
    ifelse(group == "B", x, ifelse(group == "C", -0.5 * x, 0)) +
    rep(0.01 * c(1, -1, -1, 1), 3L)
  data <- data.frame(y, x, group)
  id <- ls_register_dataset("glm_factor_reference_interaction", data)
  on.exit(ls_unregister_dataset(id), add = TRUE)
  model <- ls_new_glm(id)
  ls_glm_set_dependent(model, "y")
  for (term in c("x", "group", "x:group")) ls_glm_add_predictor(model, term)
  raw <- LinkEDA:::.rls_glm_model_record(ls_glm_fit(model))

  releveled_spec <- raw
  releveled_spec$factor_reference_levels <- list(group = "B")
  releveled <- LinkEDA:::.rls_glm_model_record(
    ls_glm_fit(LinkEDA:::.rls_assign_glm_model(releveled_spec))
  )
  expect_identical(levels(stats::model.frame(releveled$fit)$group)[[1L]], "B")
  expect_equal(unname(stats::fitted(raw$fit)), unname(stats::fitted(releveled$fit)), tolerance = 1e-9)
  expect_equal(unname(stats::residuals(raw$fit)), unname(stats::residuals(releveled$fit)), tolerance = 1e-9)
  expect_equal(summary(raw$fit)$r.squared, summary(releveled$fit)$r.squared, tolerance = 1e-12)
  expect_equal(unname(summary(raw$fit)$fstatistic),
               unname(summary(releveled$fit)$fstatistic), tolerance = 1e-10)
  reference_row <- releveled$coefficient_rows[
    releveled$coefficient_rows$source_term == "group" &
      releveled$coefficient_rows$row_type == "reference", , drop = FALSE
  ]
  interaction_rows <- releveled$coefficient_rows[
    releveled$coefficient_rows$source_term == "x:group" &
      releveled$coefficient_rows$row_type == "coefficient", , drop = FALSE
  ]
  expect_identical(reference_row$level, "B")
  expect_true(all(interaction_rows$reference_level == "B"))
  expect_true(all(grepl("vs B", interaction_rows$display_label, fixed = TRUE)))
})
