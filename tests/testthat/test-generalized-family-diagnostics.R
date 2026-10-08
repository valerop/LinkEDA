test_that("beta-binomial diagnostics use actual trial counts and transform before MI averaging", {
  one <- LinkEDA:::.rls_beta_binomial_variance_diagnostics(4, c(24, 23))
  two <- LinkEDA:::.rls_beta_binomial_variance_diagnostics(9, c(24, 23))
  expect_equal(one$beta_binomial_rho, 0.2)
  expect_equal(one$beta_binomial_variance_inflation, mean(c(5.6, 5.4)))
  expect_equal(one$beta_binomial_trials_mean, 23.5)
  expect_true(one$beta_binomial_trials_vary)
  expect_equal(mean(c(one$beta_binomial_rho, two$beta_binomial_rho)), 0.15)
  expect_false(isTRUE(all.equal(0.15, 1/(1+mean(c(4, 9))))))
  expect_equal(mean(c(one$beta_binomial_variance_inflation,
                      two$beta_binomial_variance_inflation)),
               mean(c(1+(24-1)*.15, 1+(23-1)*.15)))
  expect_equal(LinkEDA:::.rls_beta_binomial_variance_diagnostics(4, 24)$beta_binomial_variance_inflation, 5.6)
  expect_equal(LinkEDA:::.rls_beta_binomial_variance_diagnostics(4, 23)$beta_binomial_variance_inflation, 5.4)
})

test_that("Poisson and grouped binomial Pearson ratios use R residuals and residual df", {
  data <- data.frame(y = c(0, 1, 3, 2, 5, 1, 4, 3, 6, 2, 5, 4),
                     x = seq(-1, 1, length.out = 12))
  poisson <- stats::glm(y ~ x, data, family = stats::poisson())
  record <- list(family = "poisson", count_regression = TRUE,
                 count_distribution = "poisson")
  value <- LinkEDA:::.rls_glm_family_diagnostics(record, poisson, list(data = data))
  expect_equal(value$pearson_dispersion_ratio,
               sum(stats::residuals(poisson, type = "pearson")^2) /
                 stats::df.residual(poisson))
  expect_equal(stats::df.residual(poisson), nrow(data) - length(stats::coef(poisson)))
  shifted <- data; shifted$y[1] <- 3
  second <- stats::glm(y ~ x, shifted, family = stats::poisson())
  mi <- LinkEDA:::.rls_glm_family_diagnostics_mi(record,
    list(poisson, second), list(list(data = data), list(data = shifted)))
  expect_equal(mi$pearson_dispersion_ratio, mean(c(
    value$pearson_dispersion_ratio,
    LinkEDA:::.rls_glm_family_diagnostics(record, second, list(data = shifted))$pearson_dispersion_ratio)))

  grouped <- data.frame(y = c(2, 5, 7, 9, 4, 6, 11, 8, 10, 3, 9, 12),
                        trials = rep(20, 12), x = data$x)
  grouped_fit <- stats::glm(cbind(y, trials-y) ~ x, grouped,
                            family = stats::binomial())
  grouped_record <- list(family = "binomial", count_regression = TRUE,
    count_distribution = "binomial_trials", response = "y",
    trials_variable = "trials")
  grouped_value <- LinkEDA:::.rls_glm_family_diagnostics(
    grouped_record, grouped_fit, list(data = grouped))
  expect_equal(grouped_value$pearson_dispersion_ratio,
    sum(stats::residuals(grouped_fit, type = "pearson")^2) /
      stats::df.residual(grouped_fit))
  bernoulli <- grouped; bernoulli$trials <- 1
  bernoulli$y <- as.integer(grouped$y > 6)
  bernoulli_fit <- stats::glm(cbind(y, trials-y) ~ x, bernoulli,
                              family = stats::binomial())
  expect_null(LinkEDA:::.rls_glm_family_diagnostics(grouped_record,
    bernoulli_fit, list(data = bernoulli))$pearson_dispersion_ratio)
})

test_that("NB2 and Gamma transformations follow their R family parameterizations", {
  expect_equal(MASS::negative.binomial(4)$variance(8), 8 + 8^2/4)
  expect_equal(stats::Gamma()$variance(8), 8^2)
  first <- LinkEDA:::.rls_negative_binomial_variance_diagnostics(4, c(2, 6))
  second <- LinkEDA:::.rls_negative_binomial_variance_diagnostics(2, c(3, 9))
  expect_equal(first$negative_binomial_variance_inflation, 2)
  expect_equal(second$negative_binomial_variance_inflation, 4)
  expect_equal(first$negative_binomial_mean_fitted_count, 4)
  expect_false(isTRUE(all.equal(first$negative_binomial_variance_inflation,
    LinkEDA:::.rls_negative_binomial_variance_diagnostics(4, c(8, 8))$negative_binomial_variance_inflation)))
  expect_equal(mean(c(first$negative_binomial_variance_inflation,
                      second$negative_binomial_variance_inflation)), 3)

  set.seed(4701)
  data <- data.frame(x = seq(-1, 1, length.out = 100))
  data$y <- stats::rgamma(100, shape = 5, scale = exp(.5+.3*data$x)/5)
  fit <- stats::glm(y ~ x, data, family = stats::Gamma(link = "log"))
  changed <- data; changed$y[1:10] <- changed$y[1:10] * 1.3
  fit2 <- stats::glm(y ~ x, changed, family = stats::Gamma(link = "log"))
  record <- list(family = "Gamma", count_regression = FALSE)
  diagnostic <- LinkEDA:::.rls_glm_family_diagnostics(record, fit, list(data = data))
  expect_equal(diagnostic$gamma_cv, sqrt(summary(fit)$dispersion))
  mi <- LinkEDA:::.rls_glm_family_diagnostics_mi(record,
    list(fit, fit2), list(list(data = data), list(data = changed)))
  expect_equal(mi$gamma_cv, mean(sqrt(c(summary(fit)$dispersion,
                                          summary(fit2)$dispersion))))
  expect_false(isTRUE(all.equal(mi$gamma_cv,
    sqrt(mean(c(summary(fit)$dispersion, summary(fit2)$dispersion))))))
  expect_equal(stats::coef(fit), stats::coef(stats::glm(y ~ x, data,
    family = stats::Gamma(link = "log"))))
})

test_that("beta-binomial fitted precision is glmmTMB sigma and MI diagnostics transform each fit", {
  skip_if_not_installed("glmmTMB")
  set.seed(4702)
  data <- data.frame(x = rep(c(-1, 1), each = 80), trials = rep(c(23, 24), 80))
  probability <- stats::rbeta(nrow(data), 2, 3)
  data$y <- stats::rbinom(nrow(data), data$trials, probability)
  changed <- data; changed$y[seq(1, nrow(data), by = 9)] <-
    pmin(changed$trials[seq(1, nrow(data), by = 9)],
         changed$y[seq(1, nrow(data), by = 9)] + 2)
  fit <- suppressWarnings(glmmTMB::glmmTMB(cbind(y, trials-y) ~ x, data,
                                              family = glmmTMB::betabinomial()))
  fit2 <- suppressWarnings(glmmTMB::glmmTMB(cbind(y, trials-y) ~ x, changed,
                                               family = glmmTMB::betabinomial()))
  record <- list(family = "binomial", count_regression = TRUE,
    count_distribution = "beta_binomial", response = "y",
    trials_variable = "trials")
  one <- LinkEDA:::.rls_glm_family_diagnostics(record, fit, list(data = data))
  expect_equal(one$beta_binomial_rho, 1/(1+stats::sigma(fit)))
  expect_equal(one$beta_binomial_variance_inflation,
    mean(1+(data$trials-1)/(1+stats::sigma(fit))))
  mi <- LinkEDA:::.rls_glm_family_diagnostics_mi(record,
    list(fit, fit2), list(list(data = data), list(data = changed)))
  expect_equal(mi$beta_binomial_rho,
    mean(1/(1+c(stats::sigma(fit), stats::sigma(fit2)))))
  expect_equal(mi$beta_binomial_variance_inflation,
    mean(c(mean(1+(data$trials-1)/(1+stats::sigma(fit))),
           mean(1+(changed$trials-1)/(1+stats::sigma(fit2))))))
})

test_that("public R verification recipes expose the same family diagnostics", {
  base <- list(response = "y", terms = "x", predictors = "x",
    term_types = list(x = "numeric"), factor_reference_levels = list(),
    centered_predictors = character(), response_bounds = NULL,
    binary_regression = FALSE, count_regression = TRUE, exposure = "",
    family = "poisson", link = "log", count_distribution = "poisson")
  recipe <- function(record, mi = FALSE)
    LinkEDA:::.rls_generalized_verification_r_code(record,
      multiple_imputation = mi)$code
  poisson <- recipe(base)
  poisson_mi <- recipe(base, TRUE)
  expect_match(poisson, "stats::residuals(reference_fit, type = 'pearson')", fixed = TRUE)
  expect_match(poisson_mi, "pearson_dispersion_by_imputation", fixed = TRUE)
  nb <- recipe(modifyList(base, list(count_distribution = "negative_binomial")), TRUE)
  expect_match(nb, "mean(stats::fitted(model)) / model$theta", fixed = TRUE)
  beta <- recipe(modifyList(base, list(family = "binomial", link = "logit",
    count_distribution = "beta_binomial", trials_constant = 23,
    trials_variable = "")), TRUE)
  expect_match(beta, "rho_by_imputation <- 1 / (1 + precision_values)", fixed = TRUE)
  expect_match(beta, "trial_counts <- lapply", fixed = TRUE)
  grouped <- recipe(modifyList(base, list(family = "binomial", link = "logit",
    count_distribution = "binomial_trials", trials_constant = 24,
    trials_variable = "")), TRUE)
  expect_match(grouped, "if (!any(trials > 1) || df <= 0)", fixed = TRUE)
  gamma <- recipe(modifyList(base, list(family = "Gamma", link = "log",
    count_regression = FALSE, count_distribution = NULL)), TRUE)
  expect_match(gamma, "sqrt(summary(model)$dispersion)", fixed = TRUE)
  for (code in list(poisson, poisson_mi, nb, beta, grouped, gamma)) {
    expect_silent(parse(text = code))
    expect_false(grepl("LinkEDA:::|.rls_", code))
  }
})
