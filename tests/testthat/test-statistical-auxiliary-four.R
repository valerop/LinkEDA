test_that("ordinary standardized beta uses exact fitted rows from R", {
  set.seed(913)
  data <- data.frame(
    y = 2 + 1.8 * seq_len(35) + stats::rnorm(35),
    x = seq_len(35),
    group = factor(rep(c("a", "b"), length.out = 35))
  )
  data$y[1:3] <- NA_real_
  fit <- stats::lm(y ~ x + group, data = data)
  used <- as.integer(rownames(stats::model.frame(fit)))
  result <- LinkEDA:::.rls_glm_extract_fit(
    list(data = data, predictors = c("x", "group")), fit,
    list(data = data[used, , drop = FALSE], rows = used, excluded = 3L))
  expected <- unname(stats::coef(fit)[["x"]]) *
    stats::sd(data$x[used]) / stats::sd(data$y[used])
  coefficient <- result$coefficient_rows
  expect_equal(coefficient$standardized_beta[
    which(coefficient$coefficient_name == "x")], expected)
  expect_true(all(is.na(coefficient$standardized_beta[
    which(coefficient$source_term == "group")])))
  expect_true(is.na(coefficient$standardized_beta[
    which(coefficient$source_term == "(Intercept)")]))
  pooled_without_defined_beta <- result$coefficients
  pooled_without_defined_beta$standardized_beta <- NULL
  pooled_rows <- LinkEDA:::.rls_model_coefficient_display_rows(
    fit, transform(pooled_without_defined_beta, statistic = t_value),
    data, c("x", "group"), statistic_name = "t")
  expect_true(all(is.na(pooled_rows$standardized_beta)))
})

test_that("generalized coefficient intervals use the reference t or z distribution", {
  set.seed(915)
  data <- data.frame(x = seq(-1, 1, length.out = 28))
  data$positive <- stats::rgamma(
    nrow(data), shape = 2.5,
    scale = exp(0.3 + 0.5 * data$x) / 2.5)
  data$count <- stats::rpois(nrow(data), exp(0.4 + 0.7 * data$x))
  for (distribution in c("Gamma", "quasipoisson", "poisson")) {
    if (identical(distribution, "Gamma")) {
      model <- ls_new_positive_continuous_model(
        data, "positive", "x", distribution = distribution, native = FALSE)
      reference <- stats::glm(positive ~ x, data = data,
                              family = stats::Gamma(link = "log"))
    } else {
      model <- ls_new_count_regression(
        data, "count", "x", distribution = distribution, native = FALSE)
      reference <- stats::glm(count ~ x, data = data,
                              family = if (identical(distribution, "poisson"))
                                stats::poisson() else stats::quasipoisson())
    }
    rows <- ls_generalized_linear_model_state(model)$coefficient_rows
    row <- rows[rows$coefficient_name == "x", , drop = FALSE]
    expect_equal(nrow(row), 1L)
    reference_row <- summary(reference)$coefficients["x", ]
    critical <- if (identical(distribution, "poisson"))
      stats::qnorm(0.975) else stats::qt(0.975, stats::df.residual(reference))
    expect_equal(row$ci_lower, unname(reference_row[[1L]] - critical * reference_row[[2L]]))
    expect_equal(row$ci_upper, unname(reference_row[[1L]] + critical * reference_row[[2L]]))
  }
})

test_that("normal Q-Q transport uses R quantiles for each residual definition", {
  values <- c(NA_real_, 3, -1, 2, Inf)
  actual <- LinkEDA:::.rls_native_qq_by_row(values)
  expect_equal(actual[c(3, 4, 2)], stats::qnorm(c(1, 3, 5) / 6))
  expect_true(all(is.na(actual[c(1, 5)])))
  diagnostics <- data.frame(
    row_id = seq_len(3), observed = 1:3, fitted = 1:3,
    residual = c(3, -1, 2),
    standardized_residual = c(1, 2, 3),
    studentized_residual = c(3, 2, 1))
  linear <- LinkEDA:::.rls_native_linear_diagnostics_payload(
    list(diagnostics = diagnostics))
  expect_true("LINEAR_QQ_V1" %in% linear)
  marker <- match("LINEAR_QQ_V1", linear)
  expect_length(linear[(marker + 1L):length(linear)], 3L)
  generalized <- LinkEDA:::.rls_native_generalized_qq_payload(list(
    diagnostics = transform(diagnostics,
      deviance_residual = residual),
    diagnostics_by_imputation = list(
      transform(diagnostics, deviance_residual = residual))))
  expect_identical(generalized[[1L]], "GENERALIZED_QQ_V1")
  expect_identical(generalized[[2L]], "3")
  expect_identical(generalized[[6L]], "1")
})

test_that("D3 replay removes stale model-call family references", {
  skip_if_not_installed("mice")
  set.seed(914)
  data <- lapply(seq_len(3), function(i) data.frame(
    y = stats::rpois(60, lambda = rep(c(2.5, 4), each = 30)),
    x = rep(c(0, 1), each = 30)))
  full <- lapply(data, function(d)
    stats::glm(y ~ x, data = d, family = stats::poisson()))
  reduced <- lapply(data, function(d)
    stats::glm(y ~ 1, data = d, family = stats::poisson()))
  reference <- mice::D3(mice::as.mira(full), mice::as.mira(reduced))$result
  stale <- full
  for (i in seq_along(stale)) stale[[i]]$call$family <- quote(transient_family)
  expect_error(mice::D3(mice::as.mira(stale), mice::as.mira(reduced)),
               "transient_family")
  repaired <- lapply(stale, LinkEDA:::.rls_mi_d3_replay_fit)
  actual <- mice::D3(mice::as.mira(repaired), mice::as.mira(reduced))$result
  expect_equal(actual, reference)
})
