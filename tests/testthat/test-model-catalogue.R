test_that("response-specific model catalogue excludes incompatible distributions", {
  catalogue <- LinkEDA:::.rls_model_type_catalogue

  expect_identical(catalogue$count$distributions,
                   c("poisson", "negative_binomial", "binomial_trials",
                     "beta_binomial", "hurdle_beta_binomial_ceiling",
                     "perfect_score"))
  expect_false("quasipoisson" %in% catalogue$count$distributions)
  expect_identical(catalogue$count$inference_options, "quasipoisson")
  expect_identical(catalogue$binary$distributions, "binomial")
  expect_length(catalogue$binary$inference_options, 0L)
  expect_identical(catalogue$positive_continuous$distributions,
                   c("gaussian_log", "lognormal", "Gamma", "inverse.gaussian"))
  expect_identical(catalogue$proportion$distributions,
                   c("beta", "beta_one_inflated"))
  expect_identical(
    names(catalogue)[seq_len(5L)],
    c("linear", "binary", "count", "positive_continuous", "proportion")
  )
  expect_false(any(grepl("loglinear", c(
    names(catalogue), vapply(catalogue, `[[`, character(1L), "label")
  ), ignore.case = TRUE)))

  forbidden_count <- c("gaussian", "Gamma", "inverse.gaussian",
                       "beta", "beta_one_inflated", "gamma_distance")
  expect_false(any(forbidden_count %in% catalogue$count$distributions))
})

test_that("each distribution owns its links, backend, and likelihood capabilities", {
  spec <- LinkEDA:::.rls_model_distribution_catalogue
  expect_identical(spec$binomial$links, c("logit", "log", "probit", "cloglog"))
  expect_identical(spec$gaussian_log$backend, "stats::glm")
  expect_identical(spec$gaussian_log$links, "log")
  expect_identical(spec$lognormal$backend, "stats::lm")
  expect_identical(spec$lognormal$domain, "strictly_positive")
  expect_identical(spec$poisson$links, "log")
  expect_identical(spec$negative_binomial$backend, "MASS::glm.nb")
  expect_identical(spec$binomial_trials$backend, "stats::glm")
  expect_identical(spec$binomial_trials$links, "logit")
  expect_identical(spec$beta_binomial$backend, "glmmTMB::glmmTMB")
  expect_identical(spec$beta_binomial$links, "logit")
  expect_identical(spec$hurdle_beta_binomial_ceiling$backend,
                   "gamlss::gamlss + gamlss.dist::ZABB")
  expect_identical(spec$hurdle_beta_binomial_ceiling$links, "logit")
  expect_identical(spec$perfect_score$backend, "stats::glm")
  expect_identical(spec$perfect_score$links, "logit")
  expect_identical(spec$Gamma$links, c("log", "inverse", "identity"))
  expect_identical(spec$inverse.gaussian$default_link, "log")
  expect_identical(spec$beta$backend, "betareg::betareg")
  expect_false(spec$quasipoisson$distribution)
  expect_false(spec$quasipoisson$likelihood)
  expect_false(spec$quasipoisson$aic)
  expect_false(spec$quasipoisson$bic)
  expect_true(all(vapply(spec, `[[`, logical(1L), "supports_multiple_imputation")))
  expect_true(all(vapply(spec, `[[`, logical(1L), "supports_emmeans")))
  expect_true(all(vapply(spec, `[[`, logical(1L), "supports_contrasts")))
  expect_true(all(vapply(spec, `[[`, logical(1L), "supports_predictions")))

  expect_error(
    LinkEDA:::.rls_validate_model_distribution("count", "Gamma"),
    "not available for Count Model"
  )
})

test_that("Gaussian-log and lognormal models use their distinct R formulations", {
  set.seed(741)
  x <- seq(-1, 1, length.out = 80)
  group <- factor(rep(c("A", "B"), each = 40), levels = c("A", "B"))
  gaussian_y <- pmax(.05, exp(.8 + .25 * x + .2 * (group == "B")) +
                       stats::rnorm(80, sd = .08))
  lognormal_y <- exp(.7 + .3 * x + .25 * (group == "B") +
                       stats::rnorm(80, sd = .18))
  data <- data.frame(gaussian_y, lognormal_y, x, group)

  gaussian <- ls_new_positive_continuous_model(
    data, "gaussian_y", c("x", "group"), distribution = "gaussian_log",
    native = FALSE, name = "gaussian_log_public"
  )
  gaussian_state <- ls_generalized_linear_model_state(gaussian)
  expect_s3_class(gaussian_state$fit, "glm")
  expect_identical(gaussian_state$fit$family$family, "gaussian")
  expect_identical(gaussian_state$fit$family$link, "log")
  expect_equal(
    gaussian_state$coefficients$exponentiated_estimate,
    exp(gaussian_state$coefficients$estimate)
  )
  expect_identical(
    unique(gaussian_state$coefficients$exponentiated_label), "Mean ratio"
  )

  lognormal <- ls_new_positive_continuous_model(
    data, "lognormal_y", c("x", "group"), distribution = "lognormal",
    native = FALSE, name = "lognormal_public"
  )
  lognormal_state <- ls_generalized_linear_model_state(lognormal)
  expect_s3_class(lognormal_state$fit, "lm")
  expect_false(inherits(lognormal_state$fit, "glm"))
  expect_match(deparse(stats::formula(lognormal_state$fit)), "log\\(")
  expect_identical(all.vars(stats::formula(lognormal_state$fit))[[1L]], "lognormal_y")
  expect_identical(lognormal_state$response_transformation, "log(Y)")
  expect_equal(
    lognormal_state$diagnostics$observed,
    log(data$lognormal_y), tolerance = 1e-10
  )
  expect_identical(
    unique(lognormal_state$coefficients$exponentiated_label),
    "Multiplicative ratio"
  )
})

test_that("Gaussian-log does not pre-reject non-positive observations", {
  record <- list(family = "gaussian_log", count_regression = FALSE)
  expect_invisible(LinkEDA:::.rls_generalized_validate_response(
    record, c(-2, 0, 1, 4)
  ))
  record$family <- "lognormal"
  expect_error(
    LinkEDA:::.rls_generalized_validate_response(record, c(0, 1, 4)),
    "greater than zero"
  )
})

test_that("lognormal effect plots display exact arithmetic means", {
  skip_if_not_installed("emmeans")
  set.seed(742)
  data <- data.frame(
    y = exp(rep(c(.5, .9), each = 50) + stats::rnorm(100, sd = .25)),
    group = factor(rep(c("A", "B"), each = 50), levels = c("A", "B"))
  )
  model <- ls_new_positive_continuous_model(
    data, "y", "group", distribution = "lognormal", native = FALSE,
    name = "lognormal_effect"
  )
  state <- ls_generalized_linear_model_state(model)
  result <- ls_generalized_linear_model_interaction(model, "group")
  direct_grid <- stats::update(
    emmeans::emmeans(state$fit, specs = "group", offset = stats::sigma(state$fit)^2 / 2),
    tran = "log"
  )
  direct <- summary(
    direct_grid,
    type = "response", bias.adjust = FALSE
  )
  expect_identical(result$plot_data$response_scale, rep("arithmetic_mean", 2L))
  expect_equal(result$plot_data$pooled_estimate, direct$response, tolerance = 1e-8)
})

test_that("positive and proportion models validate the effective scope", {
  positive <- data.frame(y = c(1, 2, 1.5, 3, 2.5, -1), x = 0:5)
  model <- ls_new_positive_continuous_model(
    positive, "y", "x", scope = "selected", .selected_rows = 1:5,
    native = FALSE, name = "positive_scoped"
  )
  state <- ls_generalized_linear_model_state(model)
  expect_s3_class(model, "rlispstat_positive_continuous_model")
  expect_identical(state$model_type, "positive_continuous")
  expect_s3_class(state$fit, "glm")
  expect_identical(state$fit$family$family, "Gamma")
  expect_identical(state$link, "log")
  expect_error(
    ls_new_positive_continuous_model(
      positive, "y", "x", native = FALSE, name = "positive_invalid"
    ),
    "greater than zero"
  )

  invalid_beta <- data.frame(y = c(.2, .8, 1), x = 1:3)
  expect_error(
    ls_new_proportion_model(
      invalid_beta, "y", "x", distribution = "beta", native = FALSE,
      name = "beta_endpoint"
    ),
    "strictly between 0 and 1"
  )
  expect_error(
    ls_new_proportion_model(
      transform(invalid_beta, y = c(.2, .8, 1.1)), "y", "x",
      distribution = "beta_one_inflated", native = FALSE,
      name = "one_inflated_outside"
    ),
    "no greater than 1"
  )
})

test_that("positive-continuous comparisons use the shared generalized backend", {
  data <- data.frame(y = exp(seq(-1, 1, length.out = 40)), x = seq_len(40),
                     z = rep(c(0, 1), 20))
  group <- LinkEDA:::.rls_register_dataset(
    "positive_compare_data", data, activate = TRUE
  )
  reduced <- ls_new_positive_continuous_model(
    group, "y", "x", distribution = "Gamma", link = "log",
    native = FALSE, name = "positive_compare_reduced"
  )
  full <- ls_new_positive_continuous_model(
    group, "y", c("x", "z"), distribution = "Gamma", link = "log",
    native = FALSE, name = "positive_compare_full"
  )
  comparison <- ls_compare_generalized_linear_models(
    reduced, full, native = FALSE, .model_type = "positive_continuous"
  )
  expect_s3_class(comparison, "rlispstat_generalized_linear_model_comparison")
  expect_identical(comparison$response, "y")
  expect_true(comparison$common_rows)
  expect_identical(comparison$models$family, c("Gamma", "Gamma"))
  provenance <- comparison$analysis_provenance
  expect_match(provenance$executed_r_code, "stats::anova", fixed = TRUE)
})

test_that("Gaussian-log and lognormal nested comparisons use R F tests", {
  set.seed(743)
  data <- data.frame(
    y_gaussian = exp(.4 + .15 * seq(-1, 1, length.out = 80)) +
      stats::rnorm(80, sd = .04),
    y_lognormal = exp(.6 + .2 * seq(-1, 1, length.out = 80) +
      stats::rnorm(80, sd = .12)),
    x = seq(-1, 1, length.out = 80),
    group = factor(rep(c("A", "B"), each = 40))
  )
  id <- LinkEDA:::.rls_register_dataset(
    "new_positive_model_comparisons", data, activate = TRUE
  )
  on.exit(ls_unregister_dataset(id), add = TRUE)

  for (specification in list(
    list(response = "y_gaussian", distribution = "gaussian_log"),
    list(response = "y_lognormal", distribution = "lognormal")
  )) {
    reduced <- ls_new_positive_continuous_model(
      id, specification$response, "x",
      distribution = specification$distribution, native = FALSE,
      name = paste0(specification$distribution, "_reduced")
    )
    full <- ls_new_positive_continuous_model(
      id, specification$response, c("x", "group"),
      distribution = specification$distribution, native = FALSE,
      name = paste0(specification$distribution, "_full")
    )
    comparison <- ls_compare_generalized_linear_models(
      reduced, full, native = FALSE, .model_type = "positive_continuous"
    )
    expect_true(comparison$tests$available[[1L]])
    expect_identical(comparison$tests$statistic_label[[1L]], "F")
    expect_identical(comparison$tests$method[[1L]], "Nested-model F test")
    expect_match(comparison$analysis_provenance$executed_r_code,
                 'test = "F"', fixed = TRUE)
  }
})

test_that("proportion comparisons reject different response bounds", {
  skip_if_not_installed("betareg")
  data <- data.frame(y = seq(.1, .9, length.out = 30), x = seq_len(30),
                     z = rep(c(0, 1), 15))
  group <- LinkEDA:::.rls_register_dataset(
    "proportion_compare_data", data, activate = TRUE
  )
  reduced <- ls_new_generalized_linear_model(
    group, "y", "x", family = "beta", link = "logit",
    response_bounds = c(0, 1), .model_type = "proportion",
    native = FALSE, name = "proportion_compare_reduced"
  )
  full <- ls_new_generalized_linear_model(
    group, "y", c("x", "z"), family = "beta", link = "logit",
    response_bounds = c(0, 2), .model_type = "proportion",
    native = FALSE, name = "proportion_compare_full"
  )
  comparison <- ls_compare_generalized_linear_models(
    reduced, full, native = FALSE, .model_type = "proportion"
  )
  expect_false(comparison$tests$available[[1L]])
  expect_match(comparison$tests$reason[[1L]], "Different response bounds")
})

test_that("beta and one-inflated-beta comparisons use their fitted R likelihoods", {
  skip_if_not_installed("betareg")
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  set.seed(504)
  n <- 100L
  data <- data.frame(x = stats::rnorm(n), z = rep(c(0, 1), n / 2L))
  data$y <- stats::plogis(
    -0.2 + 0.5 * data$x + 0.25 * data$z + stats::rnorm(n, sd = 0.5)
  )
  group <- LinkEDA:::.rls_register_dataset(
    "bounded_likelihood_compare_data", data, activate = TRUE
  )
  on.exit(ls_unregister_dataset(group), add = TRUE)

  compare_distribution <- function(distribution, prefix, dataset_group = group) {
    reduced <- ls_new_proportion_model(
      dataset_group, "y", "x", distribution = distribution, native = FALSE,
      name = paste0(prefix, "_reduced")
    )
    full <- ls_new_proportion_model(
      dataset_group, "y", c("x", "z"), distribution = distribution, native = FALSE,
      name = paste0(prefix, "_full")
    )
    ls_compare_generalized_linear_models(
      reduced, full, native = FALSE, .model_type = "proportion"
    )
  }

  beta <- compare_distribution("beta", "beta_likelihood_compare")
  expect_true(beta$tests$available[[1L]])
  expect_match(
    beta$analysis_provenance$executed_r_code,
    ".rls_generalized_likelihood_ratio", fixed = TRUE
  )

  data$y[seq_len(20L)] <- 1
  inflated_group <- LinkEDA:::.rls_register_dataset(
    "inflated_likelihood_compare_data", data, activate = TRUE
  )
  on.exit(ls_unregister_dataset(inflated_group), add = TRUE)
  inflated <- compare_distribution(
    "beta_one_inflated", "inflated_likelihood_compare", inflated_group
  )
  expect_true(inflated$tests$available[[1L]])
  expect_gt(inflated$tests$df[[1L]], 0)
})

test_that("executed provenance names the real generalized backend", {
  base <- list(
    response = "y", terms = "x", family = "poisson", link = "log",
    count_regression = TRUE, count_distribution = "negative_binomial",
    exposure = "", response_bounds = NULL, scope = "all",
    term_types = list(), factor_reference_levels = list(),
    centered_predictors = character(), data_scope = list(kind = "all")
  )
  nb_code <- LinkEDA:::.rls_generalized_executed_r_code(base, FALSE)
  expect_match(nb_code, "MASS::glm.nb", fixed = TRUE)
  expect_false(grepl("stats::glm(model_formula", nb_code, fixed = TRUE))

  base$count_distribution <- "poisson"
  poisson_code <- LinkEDA:::.rls_generalized_executed_r_code(base, FALSE)
  expect_match(poisson_code, "stats::glm", fixed = TRUE)
  expect_match(poisson_code, "stats::poisson", fixed = TRUE)
})

test_that("macOS and Windows consume the shared model catalogue", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(root, "src", "platform", "macos",
                                   "linkeda_macos_app.mm"), warn = FALSE), collapse = "\n")
  windows <- paste(readLines(file.path(root, "src", "platform", "windows", "winui",
                                       "LinkEDA", "WorkflowWindows.cpp"),
                             warn = FALSE), collapse = "\n")
  expect_match(mac, "DistributionSpecificationsForModel", fixed = TRUE)
  expect_match(mac, "LinksForModelDistribution", fixed = TRUE)
  expect_match(windows, "DistributionSpecificationsForModel", fixed = TRUE)
  expect_match(windows, "LinksForModelDistribution", fixed = TRUE)
})
