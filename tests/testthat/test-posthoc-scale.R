test_that("post-hoc interaction contrasts distinguish response, logit, and odds-ratio scales", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("emmeans")
  set.seed(20260926)
  data <- expand.grid(a = factor(c("A", "B")), b = factor(c("X", "Y")),
                      i = seq_len(50L))
  data$y <- stats::rbinom(nrow(data), 1L, stats::plogis(
    -.4 + .7 * (data$a == "B") + .4 * (data$b == "Y") +
      .8 * (data$a == "B") * (data$b == "Y")))
  fit <- suppressWarnings(glmmTMB::glmmTMB(
    y ~ a * b, data = data, family = stats::binomial()))
  record <- list(fit = fit, fits_by_imputation = list(fit),
                 predictors = c("a", "b", "a:b"), family = "binomial", link = "logit")

  result_response <- LinkEDA:::.rls_regression_interaction_contrasts_record(
    record, "a:b", scale = "response")
  result_link <- LinkEDA:::.rls_regression_interaction_contrasts_record(
    record, "a:b", scale = "link")
  reference <- function(scale) {
    grid <- emmeans::emmeans(fit, "a", by = "b")
    if (scale == "response") grid <- emmeans::regrid(grid, transform = "response")
    first <- emmeans::contrast(grid, "pairwise", adjust = "none")
    as.data.frame(summary(emmeans::contrast(
      first, "pairwise", by = "contrast", adjust = "none")))
  }
  expect_equal(result_response$pooled_estimate, reference("response")$estimate,
               tolerance = 1e-10)
  expect_equal(result_link$pooled_estimate, reference("link")$estimate,
               tolerance = 1e-10)
  expect_identical(result_response$effect_scale[[1L]],
                   "difference of differences (response scale)")
  expect_identical(result_link$effect_link[[1L]], "logit")
  expect_equal(result_link$ratio_of_odds_ratios,
               exp(result_link$pooled_estimate), tolerance = 1e-12)

  effect <- list(interaction = "a:b", interaction_type = "factor_factor",
                 focal = "a", moderator = "b", conditioning = "b",
                 all_factor = TRUE, estimates = data.frame(), comparisons = data.frame(),
                 interaction_contrasts = result_link, imputation_count = 1L,
                 pooling_method = "emmeans", confidence_level = .95,
                 comparison_adjustment = "none", interpretation = "")
  class(effect) <- "linkeda_regression_interaction"
  lines <- LinkEDA:::.rls_interaction_native_report(effect)
  expect_true(any(grepl("Log ratio of odds ratios (logit)", lines, fixed = TRUE)))
  expect_true(any(grepl("Ratio of odds ratios", lines, fixed = TRUE)))
  expect_true("Exponentiated interaction contrasts" %in% lines)
})

test_that("beta-regression pairwise link contrasts use logits rather than response means", {
  skip_if_not_installed("betareg")
  skip_if_not_installed("emmeans")
  set.seed(20260927)
  data <- data.frame(group = factor(rep(c("Control", "Treatment"), each = 80L)))
  data$y <- stats::rbeta(nrow(data),
    shape1 = ifelse(data$group == "Control", 2, 5),
    shape2 = ifelse(data$group == "Control", 5, 2))
  fit <- betareg::betareg(y ~ group, data = data)
  record <- list(fit = fit, fits_by_imputation = list(fit),
                 predictors = "group", family = "beta", link = "logit")
  link <- LinkEDA:::.rls_regression_pairwise_record(record, "group", scale = "link")
  response <- LinkEDA:::.rls_regression_pairwise_record(record, "group", scale = "response")
  grid <- emmeans::emmeans(fit, "group", mode = "link")
  reference_link <- as.data.frame(summary(
    emmeans::contrast(grid, "pairwise"), infer = c(TRUE, TRUE)))
  reference_response <- as.data.frame(summary(emmeans::contrast(
    emmeans::regrid(grid, transform = "response"), "pairwise"),
    infer = c(TRUE, TRUE)))
  expect_equal(link$pooled_estimate, reference_link$estimate, tolerance = 1e-10)
  expect_equal(response$pooled_estimate, reference_response$estimate,
               tolerance = 1e-10)
  expect_identical(link$link_name[[1L]], "logit")
  expect_identical(link$effect_label[[1L]], "Ratio of mean odds")
  expect_equal(link$transformed_effect, exp(link$link_estimate), tolerance = 1e-10)
  expect_true(any(grepl("mean odds are mu / (1 - mu)",
    LinkEDA:::.rls_pairwise_scale_notes(link), fixed = TRUE)))
  expect_false(isTRUE(all.equal(link$pooled_estimate, response$pooled_estimate)))
})
