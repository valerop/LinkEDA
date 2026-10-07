test_that("Poisson Count Regression matches stats::glm and reports rate ratios", {
  data <- data.frame(
    events = c(0, 1, 1, 2, 3, 4, 2, 5, 7, 6, 8, 9),
    treatment = factor(rep(c("control", "treated"), each = 6)),
    time = seq_len(12),
    person_time = c(1, 1.5, 2, 2.5, 3, 4, 1, 1.5, 2, 2.5, 3, 4)
  )

  model <- ls_new_count_regression(
    data, response = "events", terms = c("treatment", "time"),
    distribution = "poisson", exposure = "person_time", native = FALSE,
    name = "count_poisson", term_types = list(treatment = "factor")
  )
  state <- ls_count_regression_state(model)
  direct <- stats::glm(
    events ~ treatment + time + offset(log(person_time)),
    data = data, family = stats::poisson("log")
  )
  rows <- ls_count_regression_coefficient_rows(model)

  expect_s3_class(model, "rlispstat_count_regression")
  expect_equal(stats::coef(state$fit), stats::coef(direct), tolerance = 1e-10)
  expect_equal(stats::fitted(state$fit), stats::fitted(direct), tolerance = 1e-10)
  expect_identical(state$link, "log")
  expect_identical(state$exposure, "person_time")
  expect_true(isTRUE(state$summary$likelihood_available))
  coefficient <- rows$row_type == "coefficient"
  expect_equal(rows$rate_ratio[coefficient], exp(rows$estimate[coefficient]), tolerance = 1e-12)
  expect_equal(rows$rate_ratio_lower[coefficient], exp(rows$ci_lower[coefficient]), tolerance = 1e-12)
  expect_equal(rows$rate_ratio_upper[coefficient], exp(rows$ci_upper[coefficient]), tolerance = 1e-12)
  expect_true(all(c("observed", "fitted", "deviance_residual", "pearson_residual",
                    "leverage", "cooks_distance") %in% names(ls_count_regression_diagnostics(model))))
})

test_that("discrete count diagnostics use deterministic R distribution residuals", {
  data <- data.frame(
    events = c(0L, 1L, 0L, 2L, 3L, 1L, 5L, 4L, 7L, 2L, 8L, 6L),
    x = seq(-1, 1, length.out = 12L)
  )
  model <- ls_new_count_regression(
    data, "events", "x", distribution = "poisson", native = FALSE,
    name = "poisson_distribution_diagnostics"
  )
  state <- ls_count_regression_state(model)
  diagnostics <- state$diagnostics
  mu <- as.numeric(stats::fitted(state$fit))

  expect_equal(diagnostics$raw_residual, data$events - mu, tolerance = 1e-12)
  expect_equal(
    diagnostics$pearson_residual,
    (data$events - mu) / sqrt(mu), tolerance = 1e-12
  )
  expect_true(all(is.finite(diagnostics$dunn_smyth_residual)))
  pit <- stats::pnorm(diagnostics$dunn_smyth_residual)
  expect_true(all(pit >= stats::ppois(data$events - 1L, mu)))
  expect_true(all(pit <= stats::ppois(data$events, mu)))
  expect_equal(diagnostics$residual, diagnostics$dunn_smyth_residual)
  before <- diagnostics$dunn_smyth_residual
  ls_count_regression_fit(model)
  expect_equal(
    ls_count_regression_state(model)$diagnostics$dunn_smyth_residual,
    before, tolerance = 0
  )

  distribution <- state$summary$observed_predicted_distribution
  expect_equal(sum(distribution$observed_frequency), nrow(data))
  expect_equal(sum(distribution$predicted_frequency), nrow(data), tolerance = 1e-8)
  expect_identical(
    state$summary$diagnostic_capabilities$default_residual, "dunn_smyth"
  )
})

test_that("Count Regression validates response and exposure in the fitted scope", {
  invalid_response <- data.frame(events = c(0, 1.5, 2), x = 1:3)
  expect_error(
    ls_new_count_regression(invalid_response, "events", "x", native = FALSE,
                            name = "count_invalid_response"),
    "non-negative integer"
  )

  invalid_exposure <- data.frame(events = 0:3, x = 1:4, exposure = c(1, 2, 0, 4))
  expect_error(
    ls_new_count_regression(invalid_exposure, "events", "x", exposure = "exposure",
                            native = FALSE, name = "count_invalid_exposure"),
    "greater than zero"
  )

  scoped <- data.frame(events = c(0, 1, 2, 1.5), x = 1:4, exposure = c(1, 2, 3, 0))
  model <- ls_new_count_regression(
    scoped, "events", "x", exposure = "exposure", scope = "selected",
    .selected_rows = 1:3, native = FALSE, name = "count_scoped_validation"
  )
  expect_equal(ls_count_regression_fit_summary(model)$n_used, 3L)
})

test_that("quasi-Poisson and negative-binomial Count Regression expose model semantics", {
  data <- data.frame(
    events = c(0, 1, 3, 2, 7, 4, 12, 5, 16, 8, 21, 10, 28, 12, 35, 15),
    x = rep(0:7, 2),
    group = factor(rep(c("a", "b"), each = 8))
  )

  quasi <- ls_new_count_regression(
    data, "events", c("x", "group"), distribution = "quasipoisson",
    native = FALSE, name = "count_quasi", term_types = list(group = "factor")
  )
  quasi_state <- ls_count_regression_state(quasi)
  expect_identical(quasi_state$count_distribution, "quasipoisson")
  expect_false(isTRUE(quasi_state$summary$likelihood_available))
  expect_true(is.na(quasi_state$summary$aic))
  expect_true(is.na(quasi_state$summary$bic))
  expect_true(is.na(quasi_state$summary$log_lik))
  expect_true(is.finite(quasi_state$summary$dispersion))

  skip_if_not_installed("MASS")
  negative_binomial <- ls_new_count_regression(
    data, "events", c("x", "group"), distribution = "negative_binomial",
    native = FALSE, name = "count_negative_binomial", term_types = list(group = "factor")
  )
  nb_state <- ls_count_regression_state(negative_binomial)
  direct <- MASS::glm.nb(events ~ x + group, data = data, link = "log")
  expect_equal(stats::coef(nb_state$fit), stats::coef(direct), tolerance = 1e-8)
  expect_equal(nb_state$summary$theta, direct$theta, tolerance = 1e-8)
  expect_equal(nb_state$summary$theta_descriptive_mean, direct$theta, tolerance = 1e-8)
  expect_equal(nb_state$summary$theta_descriptive_min, direct$theta, tolerance = 1e-8)
  expect_equal(nb_state$summary$theta_descriptive_max, direct$theta, tolerance = 1e-8)
  expect_true(is.na(nb_state$summary$dispersion))
  expect_true(isTRUE(nb_state$summary$likelihood_available))
  fit_rows <- LinkEDA:::.rls_generalized_glm_fit_rows(nb_state$summary)
  expect_true("Theta" %in% fit_rows$label)
  expect_false("Dispersion" %in% fit_rows$label)
})

test_that("Count Regression transformations are model-local and include interactions", {
  data <- data.frame(
    events = c(0, 1, 1, 2, 2, 4, 3, 5, 6, 8, 9, 12),
    x = seq_len(12),
    group = factor(rep(c("a", "b"), each = 6))
  )
  raw <- ls_new_count_regression(
    data, "events", "x:group", native = FALSE, name = "count_raw",
    term_types = list(group = "factor")
  )
  centered <- ls_new_count_regression(
    data, "events", "x:group", native = FALSE, name = "count_centered",
    term_types = list(group = "factor"), centered_predictors = "x"
  )
  raw_state <- ls_count_regression_state(raw)
  centered_state <- ls_count_regression_state(centered)

  expect_equal(stats::fitted(raw_state$fit), stats::fitted(centered_state$fit), tolerance = 1e-8)
  expect_identical(raw_state$centered_predictors, character())
  expect_identical(centered_state$centered_predictors, "x")
  expect_true(any(grepl("x:group", centered_state$terms, fixed = TRUE)))
})

test_that("Poisson Count Regression comparison reports a nested likelihood-ratio test", {
  data <- data.frame(
    events = c(0, 1, 1, 2, 3, 5, 2, 4, 7, 8, 10, 13, 1, 2, 4, 7),
    x = seq_len(16),
    planet = factor(rep(c("Aurelia", "Borealis"), each = 8)),
    person_time = rep(c(1, 2, 1.5, 3), 4)
  )
  reduced <- ls_new_count_regression(
    data, "events", "x", distribution = "poisson", exposure = "person_time",
    native = FALSE, name = "count_compare_poisson_reduced"
  )
  group <- ls_count_regression_state(reduced)$group
  full <- ls_new_count_regression(
    group, "events", c("x", "planet"), distribution = "poisson",
    exposure = "person_time", native = FALSE, name = "count_compare_poisson_full",
    term_types = list(planet = "factor")
  )

  comparison <- ls_compare_count_regression_models(reduced, full, native = FALSE)
  reduced_fit <- ls_count_regression_state(reduced)$fit
  full_fit <- ls_count_regression_state(full)$fit
  expected_statistic <- 2 * (as.numeric(stats::logLik(full_fit)) -
    as.numeric(stats::logLik(reduced_fit)))

  expect_s3_class(comparison, "rlispstat_count_regression_comparison")
  expect_true(comparison$common_rows)
  expect_true(comparison$tests$available[[1L]])
  expect_equal(comparison$tests$statistic[[1L]], expected_statistic, tolerance = 1e-10)
  expect_equal(comparison$tests$df[[1L]], 1L)
  expect_true(all(comparison$models$has_likelihood))
  expect_true(all(comparison$models$has_aic))
  expect_identical(comparison$models$exposure, c("person_time", "person_time"))
})

test_that("Count Regression comparison respects distribution capabilities", {
  data <- data.frame(
    events = c(0, 1, 3, 2, 7, 4, 12, 5, 16, 8, 21, 10, 28, 12, 35, 15),
    x = rep(0:7, 2),
    planet = factor(rep(c("Aurelia", "Borealis"), each = 8))
  )
  quasi_reduced <- ls_new_count_regression(
    data, "events", "x", distribution = "quasipoisson", native = FALSE,
    name = "count_compare_quasi_reduced"
  )
  group <- ls_count_regression_state(quasi_reduced)$group
  quasi_full <- ls_new_count_regression(
    group, "events", c("x", "planet"), distribution = "quasipoisson",
    native = FALSE, name = "count_compare_quasi_full",
    term_types = list(planet = "factor")
  )
  quasi_comparison <- ls_compare_count_regression_models(
    quasi_reduced, quasi_full, native = FALSE
  )

  expect_false(any(quasi_comparison$models$has_likelihood))
  expect_false(any(quasi_comparison$models$has_aic))
  expect_true(all(is.na(quasi_comparison$models$logLik)))
  expect_true(all(is.na(quasi_comparison$models$AIC)))
  expect_false(quasi_comparison$tests$available[[1L]])
  expect_match(quasi_comparison$tests$reason[[1L]], "unavailable for this distribution")

  poisson <- ls_new_count_regression(
    group, "events", "x", distribution = "poisson", native = FALSE,
    name = "count_compare_cross_distribution"
  )
  cross_distribution <- ls_compare_count_regression_models(
    poisson, quasi_full, native = FALSE
  )
  expect_false(cross_distribution$tests$available[[1L]])
  expect_match(cross_distribution$tests$reason[[1L]], "Different count distributions")
})

test_that("negative-binomial Count Regression comparison uses fitted likelihoods", {
  skip_if_not_installed("MASS")
  data <- data.frame(
    events = c(0, 1, 4, 2, 8, 3, 14, 6, 20, 9, 30, 13, 45, 18, 60, 24),
    x = rep(0:7, 2),
    planet = factor(rep(c("Aurelia", "Borealis"), each = 8))
  )
  reduced <- ls_new_count_regression(
    data, "events", "x", distribution = "negative_binomial", native = FALSE,
    name = "count_compare_nb_reduced"
  )
  group <- ls_count_regression_state(reduced)$group
  full <- ls_new_count_regression(
    group, "events", c("x", "planet"), distribution = "negative_binomial",
    native = FALSE, name = "count_compare_nb_full",
    term_types = list(planet = "factor")
  )
  comparison <- ls_compare_count_regression_models(reduced, full, native = FALSE)

  expect_true(comparison$tests$available[[1L]])
  expect_true(is.finite(comparison$tests$statistic[[1L]]))
  expect_true(is.finite(comparison$tests$p_value[[1L]]))
  expect_true(all(is.finite(comparison$models$theta)))
})

test_that("Count Regression comparison rejects incompatible exposure and rows", {
  data <- data.frame(
    events = c(0, 1, 1, 2, 3, 5, 2, 4, 7, 8, 10, 13),
    x = seq_len(12),
    z = c(NA, seq_len(11)),
    exposure_a = rep(c(1, 2), 6),
    exposure_b = rep(c(2, 3), 6)
  )
  exposure_a <- ls_new_count_regression(
    data, "events", "x", exposure = "exposure_a", native = FALSE,
    name = "count_compare_exposure_a"
  )
  group <- ls_count_regression_state(exposure_a)$group
  exposure_b <- ls_new_count_regression(
    group, "events", c("x", "z"), exposure = "exposure_b", native = FALSE,
    name = "count_compare_exposure_b"
  )
  exposure_comparison <- ls_compare_count_regression_models(
    exposure_a, exposure_b, native = FALSE
  )
  expect_false(exposure_comparison$tests$available[[1L]])
  expect_match(exposure_comparison$tests$reason[[1L]], "Different analysis rows")

  same_rows_different_exposure <- ls_new_count_regression(
    group, "events", "x", exposure = "exposure_b", native = FALSE,
    name = "count_compare_exposure_same_rows"
  )
  exposure_only <- ls_compare_count_regression_models(
    exposure_a, same_rows_different_exposure, native = FALSE
  )
  expect_false(exposure_only$tests$available[[1L]])
  expect_match(exposure_only$tests$reason[[1L]], "Different exposure specifications")
})

test_that("bounded binomial counts use the grouped-binomial R formulation", {
  set.seed(1904)
  data <- data.frame(
    successes = c(2, 4, 3, 6, 5, 7, 4, 8, 6, 9, 5, 7),
    x = seq(-1, 1, length.out = 12),
    group = factor(rep(c("Control", "Treatment"), each = 6))
  )
  model <- ls_new_count_regression(
    data, "successes", c("x", "group"),
    distribution = "binomial_trials", trials = 10,
    native = FALSE, name = "bounded_binomial_count",
    term_types = list(group = "factor")
  )
  state <- ls_count_regression_state(model)
  direct <- stats::glm(
    cbind(successes, 10 - successes) ~ x + group,
    data = data, family = stats::binomial(link = "logit")
  )

  expect_s3_class(state$fit, "glm")
  expect_identical(state$count_distribution, "binomial_trials")
  expect_equal(stats::coef(state$fit), stats::coef(direct), tolerance = 1e-10)
  expect_equal(state$trials_constant, 10)
  expect_identical(state$trials_variable, "")
  expect_equal(unname(state$diagnostics$fitted_probability), unname(stats::fitted(direct)),
               tolerance = 1e-10)
  expect_equal(unname(state$diagnostics$fitted_response_scale),
               unname(10 * stats::fitted(direct)), tolerance = 1e-10)
  expect_equal(unname(state$diagnostics$fitted),
               unname(10 * stats::fitted(direct)), tolerance = 1e-10)
  distribution <- state$summary$observed_predicted_distribution
  expect_equal(distribution$score, 0:10)
  expect_equal(sum(distribution$observed_frequency), nrow(data))
  expect_equal(sum(distribution$predicted_frequency), nrow(data), tolerance = 1e-8)
  coefficients <- state$coefficient_rows$row_type == "coefficient"
  expect_equal(
    state$coefficient_rows$exponentiated_estimate[coefficients],
    exp(state$coefficient_rows$estimate[coefficients]), tolerance = 1e-12
  )
  expect_identical(
    unique(state$coefficient_rows$exponentiated_label[coefficients]),
    "Odds ratio"
  )
  expect_true(is.finite(state$summary$binomial_overdispersion_ratio))
})

test_that("bounded observed-vs-fitted diagnostics use expected counts", {
  data <- data.frame(
    successes = c(rep(21L, 4L), rep(22L, 6L)),
    trials = rep(24L, 10L)
  )
  model <- ls_new_count_regression(
    data, "successes", NULL, distribution = "binomial_trials",
    trials = "trials", native = FALSE, name = "bounded_diagnostic_scale",
    .allow_intercept_only = TRUE
  )
  state <- ls_count_regression_state(model)
  expect_equal(unique(state$diagnostics$fitted_probability), .9,
               tolerance = 1e-10)
  expect_equal(unique(state$diagnostics$fitted), 21.6, tolerance = 1e-10)
  expect_equal(state$diagnostics$observed, data$successes)
  distribution <- state$summary$observed_predicted_distribution
  expect_equal(distribution$score, 0:24)
  expect_equal(sum(distribution$observed_frequency), 10)
  expect_equal(sum(distribution$predicted_frequency), 10, tolerance = 1e-8)
  expected_variance <- 24 * .9 * .1
  expect_equal(
    state$diagnostics$pearson_residual,
    (data$successes - 21.6) / sqrt(expected_variance), tolerance = 1e-10
  )
  expect_true(all(is.finite(state$diagnostics$dunn_smyth_residual)))
  expect_equal(state$diagnostics$residual,
               state$diagnostics$dunn_smyth_residual)
})

test_that("bounded count validation enforces integer successes and trials", {
  base <- data.frame(successes = c(0, 2, 5, NA), x = 1:4)
  expect_silent(ls_new_count_regression(
    base, "successes", "x", distribution = "binomial_trials", trials = 5,
    native = FALSE, name = "bounded_missing_allowed"
  ))
  expect_error(ls_new_count_regression(
    transform(base, successes = c(0, 2.5, 5, NA)), "successes", "x",
    distribution = "binomial_trials", trials = 5,
    native = FALSE, name = "bounded_noninteger_success"
  ), "integer counts")
  expect_error(ls_new_count_regression(
    transform(base, successes = c(0, 2, 6, NA)), "successes", "x",
    distribution = "binomial_trials", trials = 5,
    native = FALSE, name = "bounded_too_many_successes"
  ), "between 0 and the number of trials")
  expect_error(ls_new_count_regression(
    transform(base, successes = c(0, -1, 5, NA)), "successes", "x",
    distribution = "binomial_trials", trials = 5,
    native = FALSE, name = "bounded_negative_success"
  ), "between 0 and the number of trials")
  expect_error(ls_new_count_regression(
    base, "successes", "x", distribution = "binomial_trials", trials = 2.5,
    native = FALSE, name = "bounded_noninteger_trials"
  ), "positive integer")
  expect_error(ls_new_count_regression(
    base, "successes", "x", distribution = "binomial_trials", trials = 0,
    native = FALSE, name = "bounded_zero_trials"
  ), "positive integer")

  score_0_24 <- data.frame(successes = 0:24, x = seq(-1, 1, length.out = 25))
  valid <- ls_new_count_regression(
    score_0_24, "successes", "x", distribution = "binomial_trials",
    trials = 24, native = FALSE, name = "bounded_valid_0_24"
  )
  expect_equal(ls_count_regression_fit_summary(valid)$n_used, 25L)
})

test_that("bounded count comparisons require the same Trials specification", {
  data <- data.frame(successes = c(1, 2, 2, 3, 4, 5, 3, 4), x = 1:8)
  reduced <- ls_new_count_regression(
    data, "successes", NULL, distribution = "binomial_trials", trials = 6,
    native = FALSE, name = "bounded_compare_six", .allow_intercept_only = TRUE
  )
  group <- ls_count_regression_state(reduced)$group
  different_trials <- ls_new_count_regression(
    group, "successes", "x", distribution = "binomial_trials", trials = 7,
    native = FALSE, name = "bounded_compare_seven"
  )
  comparison <- ls_compare_count_regression_models(
    reduced, different_trials, native = FALSE
  )
  expect_false(comparison$tests$available[[1L]])
  expect_match(comparison$tests$reason[[1L]], "Different Trials specifications")
  expect_identical(comparison$models$trials_constant, c(6, 7))
})

test_that("bounded count effect plots default to expected counts", {
  skip_if_not_installed("emmeans")
  data <- data.frame(
    successes = c(1, 2, 2, 4, 3, 5, 4, 6, 5, 7, 6, 8),
    group = factor(rep(c("Control", "Treatment"), each = 6))
  )
  model <- ls_new_count_regression(
    data, "successes", "group", distribution = "binomial_trials", trials = 10,
    native = FALSE, name = "bounded_effect_count",
    term_types = list(group = "factor")
  )
  count_effect <- ls_count_regression_interaction(model, "group")
  probability_effect <- ls_count_regression_interaction(
    model, "group", quantity = "predicted_probability"
  )
  expect_true(isTRUE(count_effect$bounded_count))
  expect_identical(count_effect$quantity, "expected_count")
  expect_equal(
    count_effect$plot_data$pooled_estimate,
    10 * probability_effect$plot_data$pooled_estimate,
    tolerance = 1e-8
  )
  expect_equal(
    count_effect$plot_data$pooled_conf_low,
    10 * probability_effect$plot_data$pooled_conf_low,
    tolerance = 1e-8
  )
})

test_that("bounded count interactions use the grouped response and remain interpretable", {
  skip_if_not_installed("emmeans")
  data <- expand.grid(
    group = factor(c("Control", "Treatment")),
    moment = factor(c("Before", "After")),
    replicate = seq_len(8L)
  )
  probabilities <- with(data,
    stats::plogis(-.4 + .3 * (group == "Treatment") +
      .2 * (moment == "After") + .35 * (group == "Treatment") * (moment == "After")))
  set.seed(2401)
  data$successes <- stats::rbinom(nrow(data), size = 24L, prob = probabilities)
  model <- ls_new_count_regression(
    data, "successes", c("group", "moment", "group:moment"),
    distribution = "binomial_trials", trials = 24L,
    term_types = list(group = "factor", moment = "factor"),
    native = FALSE, name = "bounded_interaction"
  )
  state <- ls_count_regression_state(model)
  expect_match(deparse(stats::formula(state$fit)), "cbind\\(successes, 24 - successes\\)")
  interaction <- ls_count_regression_interaction(model, "group:moment")
  expect_identical(interaction$quantity, "expected_count")
  expect_true(all(interaction$plot_data$pooled_estimate >= 0))
  expect_true(all(interaction$plot_data$pooled_estimate <= 24))
  expect_true(all(c("group", "moment") %in% names(interaction$plot_data)))
})

test_that("bounded-count MI effect plots match the fitted centred R models", {
  skip_if_not_installed("mice")
  skip_if_not_installed("emmeans")
  set.seed(2402)
  base <- expand.grid(
    group = factor(c("Control", "Treatment")),
    gender = factor(c("Male", "Female")),
    x = seq(4, 22, length.out = 10L),
    replicate = seq_len(3L)
  )
  eta <- with(base,
    -1.8 + .12 * x + .55 * (group == "Treatment") +
      .3 * (gender == "Female") - .07 * x * (group == "Treatment") +
      .025 * x * (gender == "Female") - .2 *
      (group == "Treatment") * (gender == "Female") + .018 * x *
      (group == "Treatment") * (gender == "Female"))
  base$successes <- stats::rbinom(nrow(base), 24L, stats::plogis(eta))
  completed <- lapply(seq_len(3L), function(imputation) {
    data <- base
    data$x <- data$x + (imputation - 2L) * .03
    data
  })
  group_id <- ls_register_dataset("bounded_effect_mi", completed[[1L]])
  on.exit(ls_unregister_dataset(group_id), add = TRUE)
  dataset <- LinkEDA:::.rls_dataset_record(group_id)
  dataset$dataset_type <- "multiple_imputation"
  dataset$imputation_id <- "bounded_effect_mi_imp"
  dataset$imputation_count <- length(completed)
  dataset$completed_datasets <- completed
  dataset$original_data <- completed[[1L]]
  dataset$original_row_ids <- seq_len(nrow(base))
  dataset$missing_cell_mask <- as.data.frame(
    lapply(completed[[1L]], is.na), stringsAsFactors = FALSE
  )
  LinkEDA:::.rls_set_dataset_record(dataset)

  terms <- c("x", "group", "gender", "x:group", "x:gender",
             "group:gender", "x:group:gender")
  model <- ls_new_count_regression(
    group_id, "successes", terms, distribution = "binomial_trials",
    trials = 24L, centered_predictors = "x",
    term_types = list(group = "factor", gender = "factor"),
    native = FALSE, name = "bounded_effect_mi_model"
  )
  state <- ls_count_regression_state(model)
  expect_true(all(vapply(state$fits_by_imputation, function(fit) {
    abs(mean(stats::model.frame(fit)$x)) < 1e-10
  }, logical(1L))))

  group_effect <- ls_count_regression_interaction(model, "group")
  direct_group <- as.data.frame(summary(
    emmeans::emmeans(mice::as.mira(state$fits_by_imputation), specs = "group"),
    infer = c(TRUE, FALSE), type = "response"
  ))
  expect_equal(group_effect$plot_data$pooled_estimate,
               24 * direct_group$prob, tolerance = 1e-9)

  interaction <- ls_count_regression_interaction(model, "x:group")
  displayed_x <- unique(interaction$plot_data$x)
  direct_interaction <- as.data.frame(summary(
    emmeans::emmeans(
      mice::as.mira(state$fits_by_imputation), specs = "x", by = "group",
      at = list(x = displayed_x)
    ), infer = c(TRUE, FALSE), type = "response"
  ))
  expect_equal(interaction$plot_data$pooled_estimate,
               24 * direct_interaction$prob, tolerance = 1e-9)
})

test_that("beta-binomial count models use glmmTMB when available", {
  skip_if_not_installed("glmmTMB")
  data <- data.frame(
    successes = c(0, 1, 3, 2, 6, 5, 8, 7, 9, 4, 6, 2),
    x = seq(-1, 1, length.out = 12)
  )
  model <- ls_new_count_regression(
    data, "successes", "x", distribution = "beta_binomial", trials = 10,
    native = FALSE, name = "bounded_beta_binomial"
  )
  state <- ls_count_regression_state(model)
  expect_s3_class(state$fit, "glmmTMB")
  expect_identical(state$summary$fitter, "glmmTMB::glmmTMB")
  expect_true(is.finite(state$summary$beta_binomial_precision))
  expect_true(all(is.finite(state$diagnostics$dunn_smyth_residual)))
  expect_true(all(is.finite(state$diagnostics$pearson_residual)))
  expect_true(all(is.na(state$diagnostics$deviance_residual)))
  expect_equal(state$diagnostics$residual,
               state$diagnostics$dunn_smyth_residual)
  expect_equal(
    sum(state$summary$observed_predicted_distribution$predicted_frequency),
    nrow(data), tolerance = 1e-8
  )
})

test_that("native count-model fit errors preserve the trials specification", {
  group <- "count_error_trials_identity"
  ls_register_dataset(group, data.frame(successes = c(1, 2, 3), x = 1:3))
  on.exit(ls_unregister_dataset(group), add = TRUE)

  sent <- NULL
  testthat::local_mocked_bindings(
    ls_new_count_regression = function(...) stop("forced fit failure", call. = FALSE),
    .rls_send = function(lines, ...) {
      sent <<- lines
      "OK"
    },
    .package = "LinkEDA"
  )

  expect_message(
    LinkEDA:::.rls_handle_generalized_glm_needed(
      id = "count_error_trials_model",
      group = group,
      response = "successes",
      family = "binomial",
      link = "logit",
      scope = "all",
      terms = "x",
      count_regression = TRUE,
      count_distribution = "beta_binomial",
      trials_constant = 12,
      generation = 9L
    ),
    "generalized GLM task failed"
  )

  marker <- match("COUNT_V7", sent)
  expect_true(is.finite(marker))
  expect_identical(sent[[marker + 1L]], "beta_binomial")
  expect_identical(sent[[marker + 2L]], "")
  expect_identical(sent[[marker + 3L]], "")
  expect_identical(sent[[marker + 4L]], "12")
  result_marker <- match("GGLM_RESULT_V1", sent)
  expect_true(is.finite(result_marker))
  expect_identical(sent[[result_marker + 1L]], "9")
})

test_that("beta-binomial descriptive precision survives MI presentation fallbacks", {
  summary <- list(
    count_distribution = "beta_binomial",
    n_used = 40L,
    df_residual = 36L,
    aic = NA_real_, bic = NA_real_, log_lik = NA_real_,
    beta_binomial_precision = NA_real_,
    beta_binomial_precision_by_imputation = c(3.2, 3.6, 3.4),
    dispersion = 3.4
  )
  rows <- LinkEDA:::.rls_generalized_glm_fit_rows(summary)
  expect_true("Beta-binomial precision (phi) — mean across imputations (not Rubin-pooled)" %in% rows$label)
  expect_true("Beta-binomial precision (phi) range across imputations" %in% rows$label)
  expect_identical(
    rows$value[rows$label == "Beta-binomial precision (phi) — mean across imputations (not Rubin-pooled)"],
    "3.400"
  )

  payload <- LinkEDA:::.rls_native_generalized_state_payload(list(
    count_regression = TRUE, count_distribution = "beta_binomial",
    trials_variable = "", trials_constant = 24,
    fit = structure(list(), class = "linkeda_test_fit"), summary = summary,
    coefficient_rows = data.frame(), rows_used = seq_len(40L),
    rows_excluded = integer(), diagnostics = data.frame()
  ))
  expect_equal(as.numeric(payload[[9L]]), 3.4)
})

test_that("multiply-imputed beta-binomial models fit and summarize every imputation", {
  skip_if_not_installed("mice")
  skip_if_not_installed("glmmTMB")
  completed <- lapply(seq_len(3L), function(imputation) {
    data.frame(
      successes = c(0, 1, 3, 2, 6, 5, 8, 7, 9, 4, 6, 2),
      x = seq(-1, 1, length.out = 12) + (imputation - 2L) * .01
    )
  })
  group <- ls_register_dataset("bounded_beta_binomial_mi", completed[[1L]])
  on.exit(ls_unregister_dataset(group), add = TRUE)
  dataset <- LinkEDA:::.rls_dataset_record(group)
  dataset$dataset_type <- "multiple_imputation"
  dataset$imputation_id <- "bounded_beta_binomial_mi_imp"
  dataset$imputation_count <- length(completed)
  dataset$completed_datasets <- completed
  dataset$original_data <- completed[[1L]]
  dataset$original_row_ids <- seq_len(nrow(completed[[1L]]))
  dataset$missing_cell_mask <- as.data.frame(
    lapply(completed[[1L]], is.na), stringsAsFactors = FALSE
  )
  LinkEDA:::.rls_set_dataset_record(dataset)

  model <- ls_new_count_regression(
    group, "successes", "x", distribution = "beta_binomial", trials = 10,
    native = FALSE, name = "bounded_beta_binomial_mi_model"
  )
  state <- ls_count_regression_state(model)
  expect_length(state$fits_by_imputation, 3L)
  expect_true(all(vapply(state$fits_by_imputation, inherits, logical(1L), "glmmTMB")))
  precision <- vapply(state$fits_by_imputation, stats::sigma, numeric(1L))
  expect_equal(state$summary$beta_binomial_precision_by_imputation, precision)
  expect_true(is.na(state$summary$beta_binomial_precision))
  expect_false(state$summary$pooled_likelihood_available)
  expect_match(state$summary$fit_information_method, "not Rubin-pooled", fixed = TRUE)

  expected_count <- ls_count_regression_interaction(
    model, "x", quantity = "expected_count"
  )
  predicted_probability <- ls_count_regression_interaction(
    model, "x", quantity = "predicted_probability"
  )
  expect_true(isTRUE(predicted_probability$bounded_count))
  expect_identical(predicted_probability$quantity, "predicted_probability")
  expect_equal(
    expected_count$plot_data$pooled_estimate,
    10 * predicted_probability$plot_data$pooled_estimate,
    tolerance = 1e-8
  )
  expect_true(all(predicted_probability$plot_data$pooled_estimate >= 0))
  expect_true(all(predicted_probability$plot_data$pooled_estimate <= 1))
})

test_that("ceiling-hurdle beta-binomial uses exact ZABB components and effects", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  set.seed(2501)
  n <- 600L
  data <- data.frame(
    x = stats::rnorm(n),
    group = factor(sample(c("A", "B"), n, replace = TRUE))
  )
  failures <- gamlss.dist::rZABB(
    n,
    mu = stats::plogis(-.25 + .3 * data$x), sigma = .16,
    nu = stats::plogis(-1 + .45 * data$x + .2 * (data$group == "B")),
    bd = 12L
  )
  data$score <- 12L - failures
  model <- ls_new_count_regression(
    data, "score", c("x", "group", "x:group"),
    distribution = "hurdle_beta_binomial_ceiling", trials = 12L,
    native = FALSE, name = "ceiling_hurdle_exact"
  )
  state <- ls_count_regression_state(model)
  expect_s3_class(state$fit, "gamlss")
  expect_equal(state$fit$control$n.cyc, 200)
  expect_identical(state$summary$fitter,
                   "gamlss::gamlss with gamlss.dist::ZABB")
  expect_setequal(unique(state$coefficient_rows$component),
                  c("perfect_score", "below_ceiling"))
  expect_equal(state$summary$perfect_scores, sum(data$score == 12L))
  expect_equal(state$summary$non_perfect_scores, sum(data$score < 12L))
  expect_true(is.finite(state$summary$predicted_perfect_score_proportion))
  expect_equal(
    state$summary$predicted_perfect_score_count,
    sum(state$diagnostics$perfect_score_probability), tolerance = 1e-8
  )
  expect_true(is.finite(state$summary$beta_binomial_dispersion))
  expect_true(grepl("fitted successfully", state$summary$perfect_score_component_status))
  expect_true(grepl("fitted successfully", state$summary$below_ceiling_component_status))
  expect_equal(state$summary$hurdle_successful_imputations, 1L)
  expect_equal(state$summary$hurdle_attempted_imputations, 1L)
  expect_true(isTRUE(state$summary$observed_ceiling_count_constant))
  expect_false(identical(
    state$component_term_tests$perfect_score$statistic,
    state$component_term_tests$below_ceiling$statistic
  ))
  deliberately_different <- state$diagnostics
  deliberately_different$perfect_score_probability <- rep(.1, nrow(data))
  deliberately_different$.mu_failure <- as.numeric(state$fit$mu.fv)
  deliberately_different$.sigma <- as.numeric(state$fit$sigma.fv)
  ceiling_check <- LinkEDA:::.rls_hurdle_distribution_summary(
    LinkEDA:::.rls_generalized_glm_record(model), list(data = data),
    deliberately_different
  )
  expect_equal(ceiling_check$predicted_perfect_score_count, .1 * nrow(data))
  expect_false(isTRUE(all.equal(
    ceiling_check$predicted_perfect_score_count, ceiling_check$perfect_scores
  )))
  expect_false(any(grepl("quasi-Poisson", state$status, ignore.case = TRUE)))

  record_for_components <- LinkEDA:::.rls_generalized_glm_record(model)
  complete_for_components <- LinkEDA:::.rls_generalized_glm_complete_data(
    record_for_components
  )
  separate_fit <- LinkEDA:::.rls_hurdle_attach_separate_component_fits(
    record_for_components, state$fit, complete_for_components$data,
    LinkEDA:::.rls_generalized_glm_formula_object(record_for_components),
    gamlss::gamlss.control(n.cyc = 200L, c.crit = 0.001, trace = FALSE)
  )
  expect_s3_class(separate_fit$linkeda_perfect_score_fit, "glm")
  expect_s3_class(separate_fit$linkeda_below_ceiling_fit, "gamlss")
  expect_equal(separate_fit$linkeda_perfect_score_fit$control$maxit, 200L)
  expect_true(LinkEDA:::.rls_validate_hurdle_component(
    separate_fit, "perfect_score", 1L
  )$ok)
  expect_true(LinkEDA:::.rls_validate_hurdle_component(
    separate_fit, "below_ceiling", 1L
  )$ok)
  expect_true(all(is.finite(LinkEDA:::.rls_hurdle_component_vcov(
    separate_fit, "perfect_score"
  ))))
  expect_true(all(is.finite(LinkEDA:::.rls_hurdle_component_vcov(
    separate_fit, "below_ceiling"
  ))))
  separate_effect <- LinkEDA:::.rls_ceiling_hurdle_effect_record(
    record_for_components, list(separate_fit), term = "x:group",
    parts = c("x", "group"), focal = "x", confidence_level = .95,
    grid_points = 5L, quantity = "overall_expected_score"
  )
  expect_equal(nrow(separate_effect$plot_data), 10L)
  expect_true(all(is.finite(separate_effect$plot_data$pooled_estimate)))
  expect_true(all(is.finite(separate_effect$plot_data$pooled_conf_low)))
  expect_true(all(is.finite(separate_effect$plot_data$pooled_conf_high)))
  factorized_only_fit <- LinkEDA:::.rls_hurdle_attach_separate_component_fits(
    record_for_components, NULL, complete_for_components$data,
    LinkEDA:::.rls_generalized_glm_formula_object(record_for_components),
    gamlss::gamlss.control(n.cyc = 200L, c.crit = 0.001, trace = FALSE)
  )
  expect_s3_class(factorized_only_fit, "gamlss")
  expect_true(isTRUE(factorized_only_fit$linkeda_hurdle_component_fallback))
  expect_true(LinkEDA:::.rls_validate_hurdle_component(
    factorized_only_fit, "perfect_score", 1L
  )$ok)
  expect_true(LinkEDA:::.rls_validate_hurdle_component(
    factorized_only_fit, "below_ceiling", 1L
  )$ok)

  broken <- state$fit
  broken$nu.coefficients[[1L]] <- NA_real_
  expect_false(LinkEDA:::.rls_validate_hurdle_component(
    broken, "perfect_score", 7L
  )$ok)
  expect_match(
    LinkEDA:::.rls_validate_hurdle_component(broken, "perfect_score", 7L)$status,
    "Perfect-score logistic component failed"
  )
  broken_below <- state$fit
  broken_below$mu.coefficients[[1L]] <- NA_real_
  expect_false(LinkEDA:::.rls_validate_hurdle_component(
    broken_below, "below_ceiling", 8L
  )$ok)
  expect_match(
    LinkEDA:::.rls_validate_hurdle_component(
      broken_below, "below_ceiling", 8L
    )$status,
    "Truncated beta-binomial component below the ceiling failed"
  )
  withr::local_options(linkeda.development_checks = TRUE)
  expect_silent(LinkEDA:::.rls_assert_hurdle_one_df_consistency(
    state$coefficient_rows, state$term_tests
  ))

  effects <- lapply(c(
    "overall_expected_score", "perfect_score_probability",
    "conditional_expected_score"
  ), function(quantity) ls_count_regression_interaction(
    model, "x:group", quantity = quantity, grid_points = 5L
  ))
  expect_true(all(vapply(effects, function(effect) {
    nrow(effect$plot_data) == 10L &&
      all(is.finite(effect$plot_data$pooled_estimate))
  }, logical(1L))))
  expect_true(all(effects[[2L]]$plot_data$pooled_estimate >= 0 &
                  effects[[2L]]$plot_data$pooled_estimate <= 1))
  expect_true(is.data.frame(state$summary$observed_predicted_distribution))
  expect_equal(
    sum(state$summary$observed_predicted_distribution$predicted_frequency),
    nrow(data), tolerance = 1e-6
  )
  expect_equal(
    sum(state$summary$observed_predicted_distribution$observed), 1,
    tolerance = 1e-10
  )
  expect_true(all(c("conditional_residual", "joint_model_residual") %in%
                  names(state$diagnostics)))
  expect_equal(state$diagnostics$residual,
               state$diagnostics$dunn_smyth_residual)
  expect_true(all(is.finite(state$diagnostics$dunn_smyth_residual)))

  record <- LinkEDA:::.rls_generalized_glm_record(model)
  complete <- LinkEDA:::.rls_generalized_glm_complete_data(record)
  testthat::local_mocked_bindings(
    .rls_generalized_glm_diagnostics = function(...) {
      stop("deliberate diagnostic failure", call. = FALSE)
    },
    .package = "LinkEDA"
  )
  diagnostic_failure <- LinkEDA:::.rls_extract_ceiling_hurdle_fit(
    record, state$fit, complete
  )
  expect_match(diagnostic_failure$status, "fitted successfully", fixed = TRUE)
  expect_true(nrow(diagnostic_failure$coefficient_rows) > 0L)
  expect_equal(nrow(diagnostic_failure$diagnostics), 0L)
  expect_match(
    diagnostic_failure$warnings,
    "diagnostic could not be computed.*fitted model remains valid"
  )
})

test_that("ceiling-hurdle beta-binomial fits and pools every imputation separately", {
  skip_if_not_installed("mice")
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  set.seed(2503)
  base <- data.frame(
    x = rep(seq(-1.5, 1.5, length.out = 60L), 3L),
    group = factor(rep(c("Control", "Treatment"), length.out = 180L)),
    trials = rep(c(10L, 12L, 14L), each = 60L)
  )
  completed <- lapply(seq_len(3L), function(imputation) {
    data <- base
    data$x <- data$x + (imputation - 2L) * .02
    failures <- vapply(seq_len(nrow(data)), function(index) {
      gamlss.dist::rZABB(
        1L, mu = stats::plogis(
          -.2 + .25 * data$x[[index]] + .08 * (data$group[[index]] == "Treatment")
        ), sigma = .18,
        nu = stats::plogis(
          -.8 + .3 * data$x[[index]] + .12 * (data$group[[index]] == "Treatment")
        ),
        bd = data$trials[[index]]
      )
    }, numeric(1L))
    data$score <- data$trials - failures
    data
  })
  group <- ls_register_dataset("ceiling_hurdle_mi", completed[[1L]])
  on.exit(ls_unregister_dataset(group), add = TRUE)
  dataset <- LinkEDA:::.rls_dataset_record(group)
  dataset$dataset_type <- "multiple_imputation"
  dataset$imputation_id <- "ceiling_hurdle_mi_imp"
  dataset$imputation_count <- length(completed)
  dataset$completed_datasets <- completed
  dataset$original_data <- completed[[1L]]
  dataset$original_row_ids <- seq_len(nrow(completed[[1L]]))
  dataset$missing_cell_mask <- as.data.frame(
    lapply(completed[[1L]], is.na), stringsAsFactors = FALSE
  )
  LinkEDA:::.rls_set_dataset_record(dataset)

  model <- ls_new_count_regression(
    group, "score", c("x", "group", "x:group"),
    distribution = "hurdle_beta_binomial_ceiling",
    trials = "trials", native = FALSE, name = "ceiling_hurdle_mi_model"
  )
  state <- ls_count_regression_state(model)
  expect_length(state$fits_by_imputation, 3L)
  expect_true(all(vapply(state$fits_by_imputation, inherits, logical(1L), "gamlss")))
  expect_true(all(vapply(
    state$fits_by_imputation,
    function(fit) identical(as.integer(fit$control$n.cyc), 200L),
    logical(1L)
  )))
  expect_setequal(unique(state$coefficient_rows$component),
                  c("perfect_score", "below_ceiling"))
  expect_false(state$summary$likelihood_available)
  expect_length(state$summary$beta_binomial_dispersion_by_imputation, 3L)
  expect_equal(
    state$summary$beta_binomial_dispersion,
    mean(state$summary$beta_binomial_dispersion_by_imputation), tolerance = 1e-10
  )
  expect_equal(state$summary$hurdle_successful_imputations, 3L)
  expect_equal(state$summary$hurdle_attempted_imputations, 3L)
  expect_equal(
    state$summary$predicted_perfect_score_count,
    mean(state$summary$predicted_perfect_score_count_by_imputation), tolerance = 1e-10
  )
  expect_true(is.logical(state$summary$observed_ceiling_count_constant))
  expect_true(is.data.frame(state$summary$observed_predicted_distribution))
  expect_length(state$summary$observed_predicted_distribution_by_imputation, 3L)
  expect_length(state$diagnostics_by_imputation, 3L)
  expect_true(all(vapply(
    state$diagnostics_by_imputation,
    function(rows) {
      all(is.finite(rows$dunn_smyth_residual)) &&
        all(is.na(rows$deviance_residual)) &&
        isTRUE(all.equal(
          rows$raw_residual, rows$observed - rows$fitted,
          tolerance = 1e-10
        ))
    }, logical(1L)
  )))
  expect_identical(
    state$summary$diagnostic_capabilities$default_residual,
    "dunn_smyth"
  )
  expect_true(all(vapply(
    state$summary$observed_predicted_distribution_by_imputation,
    function(value) abs(sum(value$predicted_frequency) - nrow(base)) < 1e-5,
    logical(1L)
  )))
  effect <- ls_count_regression_interaction(
    model, "x", quantity = "overall_expected_score", grid_points = 5L
  )
  expect_equal(nrow(effect$plot_data), 5L)
  expect_true(all(is.finite(effect$plot_data$pooled_estimate)))
  expect_true(all(effect$plot_data$pooled_estimate >= 0))
  expect_true(all(effect$plot_data$pooled_estimate <= max(base$trials)))

  report <- LinkEDA:::.rls_interaction_native_report(effect)
  payload <- LinkEDA:::.rls_interaction_native_plot_payload(effect)
  expect_match(paste(report, collapse = "\n"),
               "Overall expected score", fixed = TRUE)
  expect_match(paste(report, collapse = "\n"),
               "both fitted ZABB components", fixed = TRUE)
  expect_false(any(grepl("emmeans", report, fixed = TRUE)))
  expect_true(any(startsWith(report, "Interaction term:")))
  expect_false(any(startsWith(report, "Effect term:")))
  expect_true(any(startsWith(payload, "BOUNDED\toverall_expected_score")))
  expect_equal(sum(startsWith(payload, "ROW\t")), 5L)

  interaction <- ls_count_regression_interaction(
    model, "x:group", quantity = "overall_expected_score", grid_points = 5L
  )
  expect_identical(interaction$interaction_type, "numeric_factor")
  expect_equal(nrow(interaction$plot_data), 10L)
  expect_true(all(is.finite(interaction$plot_data$pooled_estimate)))
  interaction_report <- LinkEDA:::.rls_interaction_native_report(interaction)
  interaction_payload <- LinkEDA:::.rls_interaction_native_plot_payload(interaction)
  expect_match(paste(interaction_report, collapse = "\n"),
               "x:group", fixed = TRUE)
  expect_equal(sum(startsWith(interaction_payload, "ROW\t")), 10L)
  expect_error(ls_count_regression_interaction(
    model, "x:group", quantity = "overall_expected_score",
    focal = "x", group_by = "group"
  ), "grouping factor is unavailable for ceiling-hurdle effects")
})

test_that("ceiling hurdle remains interpretable when no perfect scores occur", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  set.seed(2510)
  data <- data.frame(
    x = stats::rnorm(360L),
    group = factor(rep(c("Control", "Treatment"), 180L))
  )
  # Every response is below the configured, theoretically possible ceiling.
  failures <- sample(1:8, nrow(data), replace = TRUE)
  data$score <- 12L - failures
  model <- ls_new_count_regression(
    data, "score", c("x", "group", "x:group"),
    distribution = "hurdle_beta_binomial_ceiling", trials = 12L,
    native = FALSE, name = "ceiling_hurdle_without_perfect_scores"
  )
  state <- ls_count_regression_state(model)
  expect_equal(state$summary$perfect_scores, 0)
  expect_equal(state$summary$predicted_perfect_score_count, 0)
  expect_match(
    state$summary$perfect_score_component_status,
    "not estimated because no perfect scores", fixed = TRUE
  )
  perfect_rows <- state$coefficient_rows$component == "perfect_score"
  expect_equal(sum(perfect_rows), 1L)
  expect_match(
    state$coefficient_rows$display_label[perfect_rows],
    "not estimable: no perfect scores", fixed = TRUE
  )
  effect <- ls_count_regression_interaction(
    model, "x:group", quantity = "overall_expected_score", grid_points = 5L
  )
  expect_equal(nrow(effect$plot_data), 10L)
  expect_true(all(is.finite(effect$plot_data$pooled_estimate)))
  perfect_effect <- ls_count_regression_interaction(
    model, "group", quantity = "perfect_score_probability"
  )
  expect_true(all(perfect_effect$plot_data$pooled_estimate == 0))
  expect_true(all(perfect_effect$plot_data$pooled_conf_low == 0))
  expect_true(all(perfect_effect$plot_data$pooled_conf_high == 0))
})

test_that("ceiling-hurdle conditional expectation uses the exact BB identity", {
  skip_if_not_installed("gamlss.dist")
  set.seed(25031)
  trials <- sample(c(8L, 12L, 24L), 20L, replace = TRUE)
  eta_mu <- stats::qlogis(stats::runif(20L, .08, .92))
  eta_sigma <- log(stats::runif(20L, .04, .45))
  direct <- LinkEDA:::.rls_hurdle_conditional_score(
    trials, eta_mu, eta_sigma
  )
  enumerated <- vapply(seq_along(trials), function(index) {
    failures <- seq_len(trials[[index]])
    mu <- stats::plogis(eta_mu[[index]])
    sigma <- exp(eta_sigma[[index]])
    probability_zero <- gamlss.dist::dBB(
      0, mu = mu, sigma = sigma, bd = trials[[index]]
    )
    trials[[index]] - sum(failures * gamlss.dist::dBB(
      failures, mu = mu, sigma = sigma, bd = trials[[index]]
    )) / (1 - probability_zero)
  }, numeric(1L))
  expect_equal(direct, enumerated, tolerance = 1e-10)
})

test_that("perfect-score convenience model is ordinary R logistic regression", {
  set.seed(2502)
  data <- data.frame(
    score = sample(5:10, 300L, replace = TRUE),
    x = stats::rnorm(300L)
  )
  model <- ls_new_count_regression(
    data, "score", "x", distribution = "perfect_score", trials = 10L,
    native = FALSE, name = "perfect_score_logistic"
  )
  state <- ls_count_regression_state(model)
  direct <- stats::glm(I(score == 10L) ~ x, data = data,
                       family = stats::binomial(link = "logit"))
  expect_s3_class(state$fit, "glm")
  expect_equal(stats::coef(state$fit), stats::coef(direct), tolerance = 1e-10)
  expect_false(any(grepl("quasi-Poisson", state$status, ignore.case = TRUE)))
})

test_that("beta-binomial and ceiling-hurdle models compare by AIC without a false LR test", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  set.seed(2504)
  data <- data.frame(x = stats::rnorm(300L))
  failures <- gamlss.dist::rZABB(
    nrow(data), mu = stats::plogis(-.15 + .2 * data$x), sigma = .2,
    nu = stats::plogis(-1 + .25 * data$x), bd = 10L
  )
  data$score <- 10L - failures
  beta <- ls_new_count_regression(
    data, "score", "x", distribution = "beta_binomial", trials = 10L,
    native = FALSE, name = "ceiling_compare_beta"
  )
  group <- ls_count_regression_state(beta)$group
  hurdle <- ls_new_count_regression(
    group, "score", "x", distribution = "hurdle_beta_binomial_ceiling",
    trials = 10L, native = FALSE, name = "ceiling_compare_hurdle"
  )
  comparison <- ls_compare_count_regression_models(beta, hurdle, native = FALSE)
  expect_true(all(is.finite(comparison$models$AIC)))
  expect_false(comparison$tests$available[[1L]])
  expect_match(comparison$tests$reason[[1L]], "compare their AIC values",
               fixed = TRUE)
  expect_true(is.na(comparison$tests$p_value[[1L]]))
})

test_that("ordinary bounded models warn when they underpredict ceiling mass", {
  data <- data.frame(
    score = c(rep(10L, 30L), sample(2:9, 70L, replace = TRUE)),
    x = stats::rnorm(100L)
  )
  model <- ls_new_count_regression(
    data, "score", "x", distribution = "binomial_trials", trials = 10L,
    native = FALSE, name = "bounded_ceiling_warning"
  )
  state <- ls_count_regression_state(model)
  expect_true(any(grepl("underpredicts", state$warnings, ignore.case = TRUE)))
  expect_match(state$status, "ceiling-hurdle", fixed = TRUE)
})

test_that("Poisson diagnostics warn non-blockingly about excess zero mass", {
  data <- data.frame(
    events = c(rep(0L, 95L), rep(20L, 5L)),
    x = rep(c(-1, 1), 50L)
  )
  model <- ls_new_count_regression(
    data, "events", "x", distribution = "poisson", native = FALSE,
    name = "poisson_zero_mass_warning"
  )
  state <- ls_count_regression_state(model)
  expect_true(any(grepl("underpredicts zero counts", state$warnings,
                        fixed = TRUE)))
  expect_match(state$status, "zero-inflated alternative", fixed = TRUE)
})
