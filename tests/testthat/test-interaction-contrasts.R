.interaction_contrast_data <- function(levels_a = c("a1", "a2"),
                                       levels_b = c("b1", "b2"),
                                       levels_c = NULL,
                                       interaction = 0, replicates = 12L) {
  cells <- expand.grid(
    replicate = seq_len(replicates),
    alpha_code = levels_a,
    beta_code = levels_b,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  if (!is.null(levels_c)) {
    cells <- merge(
      cells,
      data.frame(condition_code = levels_c, stringsAsFactors = FALSE),
      all = TRUE
    )
  }
  cells$alpha_code <- factor(cells$alpha_code, levels = levels_a)
  cells$beta_code <- factor(cells$beta_code, levels = levels_b)
  if (!is.null(levels_c)) {
    cells$condition_code <- factor(cells$condition_code, levels = levels_c)
  }
  residual_pattern <- rep(c(-1.5, -.5, .5, 1.5), length.out = nrow(cells))
  a_index <- match(cells$alpha_code, levels_a) - 1
  b_index <- match(cells$beta_code, levels_b) - 1
  c_index <- if (is.null(levels_c)) 0 else match(cells$condition_code, levels_c) - 1
  cells$outcome <- 10 + .7 * a_index - .4 * b_index + .2 * c_index +
    interaction * a_index * b_index + residual_pattern
  cells
}

.interaction_contrast_record <- function(data, formula, term) {
  list(
    fit = stats::lm(formula, data = data),
    predictors = unique(c(all.vars(stats::delete.response(stats::terms(formula))), term))
  )
}

.direct_emmeans_difference_of_differences <- function(fit, factor_a, a1, a2,
                                                       factor_b, b1, b2) {
  grid <- emmeans::emmeans(
    fit, specs = stats::as.formula(paste("~", factor_a, "*", factor_b))
  )
  tab <- as.data.frame(grid)
  coefficient <- numeric(nrow(tab))
  coefficient[tab[[factor_a]] == a1 & tab[[factor_b]] == b1] <- 1
  coefficient[tab[[factor_a]] == a2 & tab[[factor_b]] == b1] <- -1
  coefficient[tab[[factor_a]] == a1 & tab[[factor_b]] == b2] <- -1
  coefficient[tab[[factor_a]] == a2 & tab[[factor_b]] == b2] <- 1
  list(
    estimate = unname(sum(coefficient * tab$emmean)),
    variance = unname(drop(crossprod(coefficient, stats::vcov(grid) %*% coefficient)))
  )
}

test_that("2 by 2 interaction contrasts equal R's exact full-covariance calculation", {
  skip_if_not_installed("emmeans")

  constant <- .interaction_contrast_data(interaction = 0)
  constant_record <- .interaction_contrast_record(
    constant, outcome ~ alpha_code * beta_code, "alpha_code:beta_code"
  )
  constant_result <- LinkEDA:::.rls_regression_interaction_contrasts_record(
    constant_record, "alpha_code:beta_code", scale = "link"
  )
  expect_equal(constant_result$pooled_estimate, 0, tolerance = 1e-12)

  known <- .interaction_contrast_data(interaction = 2.75)
  known_record <- .interaction_contrast_record(
    known, outcome ~ alpha_code * beta_code, "alpha_code:beta_code"
  )
  actual <- LinkEDA:::.rls_regression_interaction_contrasts_record(
    known_record, "alpha_code:beta_code", scale = "link"
  )
  direct <- .direct_emmeans_difference_of_differences(
    known_record$fit, "alpha_code", "a1", "a2", "beta_code", "b1", "b2"
  )
  expect_equal(actual$pooled_estimate, 2.75, tolerance = 1e-12)
  expect_equal(actual$pooled_estimate, direct$estimate, tolerance = 1e-12)
  expect_equal(actual$pooled_std_error^2, direct$variance, tolerance = 1e-12)
})

test_that("visible level orientation controls sign without changing evidence", {
  skip_if_not_installed("emmeans")
  data <- .interaction_contrast_data(interaction = 1.4)
  record <- .interaction_contrast_record(
    data, outcome ~ alpha_code * beta_code, "alpha_code:beta_code"
  )
  original <- LinkEDA:::.rls_regression_interaction_contrasts_record(
    record, "alpha_code:beta_code", scale = "link"
  )
  reverse_a <- LinkEDA:::.rls_regression_interaction_contrasts_record(
    record, "alpha_code:beta_code",
    level_order = list(alpha_code = c("a2", "a1")), scale = "link"
  )
  reverse_both <- LinkEDA:::.rls_regression_interaction_contrasts_record(
    record, "alpha_code:beta_code",
    level_order = list(alpha_code = c("a2", "a1"), beta_code = c("b2", "b1")),
    scale = "link"
  )
  expect_equal(reverse_a$pooled_estimate, -original$pooled_estimate, tolerance = 1e-12)
  expect_equal(reverse_a$pooled_p_value, original$pooled_p_value, tolerance = 1e-12)
  expect_equal(reverse_both$pooled_estimate, original$pooled_estimate, tolerance = 1e-12)
  expect_equal(reverse_both$pooled_p_value, original$pooled_p_value, tolerance = 1e-12)
})

test_that("multi-level and conditioned contrasts are complete and semantically unique", {
  skip_if_not_installed("emmeans")
  data <- .interaction_contrast_data(
    levels_a = c("first - value", "second/value", "third value"),
    levels_b = c("before", "during", "after"),
    levels_c = c("north", "south"), interaction = .8
  )
  record <- .interaction_contrast_record(
    data,
    outcome ~ alpha_code * beta_code * condition_code,
    "alpha_code:beta_code:condition_code"
  )
  result <- LinkEDA:::.rls_regression_interaction_contrasts_record(
    record, "alpha_code:beta_code:condition_code",
    contrast_factor = "alpha_code", comparison_factor = "beta_code",
    conditioning_factors = "condition_code", scale = "link"
  )
  expect_equal(nrow(result), choose(3, 2) * choose(3, 2) * 2)
  expect_equal(length(unique(result$semantic_key)), nrow(result))
  expect_setequal(unique(as.character(result$condition_code)), c("north", "south"))
  expect_true(all(c("first - value", "second/value", "third value") %in%
                    c(result$A_first, result$A_second)))

  reference_result <- LinkEDA:::.rls_regression_interaction_contrasts_record(
    record, "alpha_code:beta_code:condition_code",
    contrast_method = "reference", comparison_method = "consecutive",
    contrast_reference = "second/value", scale = "link"
  )
  expect_equal(nrow(reference_result), 2 * 2 * 2)
  expect_true(all(reference_result$A_second == "second/value"))
})

test_that("MI interaction contrast pools the complete scalar with mice", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("mice")
  base <- .interaction_contrast_data(interaction = 1.15, replicates = 15L)
  fits <- lapply(seq_len(5L), function(imputation) {
    completed <- base
    completed$outcome <- completed$outcome +
      sin(seq_len(nrow(completed)) * (imputation + 1L)) * .03
    stats::lm(outcome ~ alpha_code * beta_code, data = completed)
  })
  record <- list(
    fits_by_imputation = fits,
    predictors = c("alpha_code", "beta_code", "alpha_code:beta_code")
  )
  actual <- LinkEDA:::.rls_regression_interaction_contrasts_record(
    record, "alpha_code:beta_code", scale = "link"
  )
  direct <- lapply(fits, .direct_emmeans_difference_of_differences,
                   factor_a = "alpha_code", a1 = "a1", a2 = "a2",
                   factor_b = "beta_code", b1 = "b1", b2 = "b2")
  direct_pool <- mice::pool.scalar(
    Q = vapply(direct, `[[`, numeric(1L), "estimate"),
    U = vapply(direct, `[[`, numeric(1L), "variance"),
    n = stats::df.residual(fits[[1L]]) + 1,
    k = 1, rule = "rubin1987"
  )
  expect_equal(actual$pooled_estimate, unname(direct_pool$qbar), tolerance = 1e-12)
  expect_equal(actual$pooled_std_error, sqrt(unname(direct_pool$t)), tolerance = 1e-12)
  expect_equal(actual$pooled_df, unname(direct_pool$df), tolerance = 1e-10)
  expect_match(actual$pooling_method, "mice::pool.scalar", fixed = TRUE)
  expect_equal(length(attr(actual, "interaction_contrast")$per_imputation), 5L)
})

test_that("existing EMMs and simple contrasts retain direct emmeans results", {
  skip_if_not_installed("emmeans")
  data <- .interaction_contrast_data(interaction = .9)
  record <- .interaction_contrast_record(
    data, outcome ~ alpha_code * beta_code, "alpha_code:beta_code"
  )
  interaction <- LinkEDA:::.rls_regression_interaction_record(
    record, "alpha_code:beta_code"
  )
  direct_emm <- as.data.frame(summary(
    emmeans::emmeans(record$fit, specs = "alpha_code", by = "beta_code"),
    infer = c(TRUE, TRUE), null = 0
  ))
  expect_equal(interaction$estimates$pooled_estimate, direct_emm$emmean,
               tolerance = 1e-12)
  expect_true(inherits(interaction$interaction_contrasts,
                       "linkeda_interaction_contrasts"))
  expect_equal(interaction$interaction_contrasts$pooled_estimate, .9,
               tolerance = 1e-12)

  report <- LinkEDA:::.rls_interaction_native_report(interaction)
  expect_equal(sum(startsWith(report, "Pooling:")), 0L)
  expect_true(any(startsWith(report, "Interaction contrast pooling:")))
  contrast_header <- match("Interaction contrasts", report) + 1L
  expect_match(report[[contrast_header]], "\\tdf\\t", perl = TRUE)
})
