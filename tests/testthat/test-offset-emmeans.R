test_that("count-model marginal means use unit exposure and reproduce emmeans", {
  skip_if_not_installed("emmeans")
  set.seed(20260924)
  data <- data.frame(
    g = factor(rep(c("A", "B"), each = 48L)),
    exposure = rep(c(1, 2, 4, 8), 24L)
  )
  data$y <- stats::rpois(nrow(data), exp(.3 + .5 * (data$g == "B")) * data$exposure)
  fit <- stats::glm(y ~ g + offset(log(exposure)), data = data,
                    family = stats::poisson())
  record <- list(fit = fit, fits_by_imputation = list(fit), predictors = "g",
                 family = "poisson", link = "log")

  result <- LinkEDA:::.rls_regression_pairwise_record(record, "g", scale = "response")
  expect_match(attr(result, "scale_note"), "offset at zero", fixed = TRUE)
  reference <- as.data.frame(summary(emmeans::emmeans(fit, "g", offset = 0),
                                     type = "response"))
  default <- as.data.frame(summary(emmeans::emmeans(fit, "g"), type = "response"))
  expect_equal(result$mean_first[[1L]], reference$rate[[1L]], tolerance = 1e-12)
  expect_equal(result$mean_second[[1L]], reference$rate[[2L]], tolerance = 1e-12)
  expect_equal(result$response_difference[[1L]],
               reference$rate[[1L]] - reference$rate[[2L]], tolerance = 1e-12)
  expect_gt(default$rate[[1L]], result$mean_first[[1L]])

  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(data, path)
  record$analysis_provenance <- list(verification_r_code = list(model = paste(
    "d <- readRDS(verification_data_path)",
    "fit <- stats::glm(y ~ g + offset(log(exposure)), data=d, family=stats::poisson())",
    sep = "\n"
  )))
  result <- LinkEDA:::.rls_regression_pairwise_record(record, "g", scale = "response")
  code <- attr(result, "analysis_provenance")$verification_r_code$pairwise_scales
  expect_match(code, "offset=0", fixed = TRUE)
  env <- new.env(parent = globalenv())
  env$verification_data_path <- path
  invisible(capture.output(eval(parse(text = code), envir = env)))
  expect_equal(env$reference$mean_first, result$mean_first, tolerance = 1e-12)
  expect_equal(env$reference$mean_second, result$mean_second, tolerance = 1e-12)
})

test_that("binary effect interpretation retains fitted offsets", {
  set.seed(20260925)
  data <- data.frame(g = factor(rep(c("A", "B"), each = 60L)),
                     known = rep(seq(-1, 1, length.out = 30L), 4L))
  data$y <- stats::rbinom(nrow(data), 1L,
    stats::plogis(-.5 + .6 * (data$g == "B") + data$known))
  fit <- stats::glm(y ~ g + offset(known), data = data,
                    family = stats::binomial())
  scenario <- list(g = factor("A", levels = levels(data$g)))
  actual <- LinkEDA:::.rls_binary_scenario_estimate(fit, scenario, "average_sample")
  reference_data <- data
  reference_data$g <- factor("A", levels = levels(data$g))
  expect_equal(actual$estimate,
    mean(stats::predict(fit, newdata = reference_data, type = "response")),
    tolerance = 1e-12)
  expect_gt(abs(actual$estimate - stats::plogis(stats::coef(fit)[[1L]])), .001)

  typical <- LinkEDA:::.rls_binary_scenario_estimate(fit, scenario, "reference_profile")
  reference_profile <- data.frame(g = factor("A", levels = levels(data$g)),
                                  known = mean(data$known))
  expect_equal(typical$estimate,
    unname(stats::predict(fit, newdata = reference_profile, type = "response")),
    tolerance = 1e-12)

  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(data, path)
  record <- list(fit = fit, fits_by_imputation = list(fit), predictors = "g",
    family = "binomial", link = "logit", binary_regression = TRUE, event = "1",
    analysis_provenance = list(verification_r_code = list(model = paste(
      "d <- readRDS(verification_data_path)",
      "fit <- stats::glm(y ~ g + offset(known), data=d, family=stats::binomial())",
      sep = "\n"))))
  effect <- LinkEDA:::.rls_binary_effect_record(record, list(fit), "g", "g",
    character(), .95, 25L, "predicted_probability", "average_sample", "probability")
  expect_match(effect$interpretation, "Each observation retains its fitted offset", fixed = TRUE)
  code <- effect$analysis_provenance$verification_r_code$effect
  env <- new.env(parent = globalenv())
  env$verification_data_path <- path
  invisible(capture.output(eval(parse(text = code), envir = env)))
  expect_equal(env$reference$estimated_probability, effect$estimates$pooled_estimate,
    tolerance = 1e-12)

  completed <- lapply(c(-.1, 0, .1), function(shift) {
    current <- data
    current$known <- current$known + shift
    current
  })
  fits <- lapply(completed, function(current) stats::glm(
    y ~ g + offset(known), data = current, family = stats::binomial()
  ))
  pooled <- LinkEDA:::.rls_binary_effect_record(record, fits, "g", "g",
    character(), .95, 25L, "predicted_probability", "average_sample", "probability")
  expected <- vapply(seq_along(fits), function(i) {
    current <- completed[[i]]
    current$g <- factor("A", levels = levels(data$g))
    mean(stats::predict(fits[[i]], newdata = current, type = "response"))
  }, numeric(1L))
  expect_equal(pooled$estimates$pooled_estimate[[1L]], mean(expected), tolerance = 1e-12)
})
