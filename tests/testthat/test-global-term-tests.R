test_that("binary factors and interactions use joint Wald tests", {
  data <- transform(
    mtcars,
    outcome = factor(am, levels = c(0, 1), labels = c("automatic", "manual")),
    country = factor(cyl),
    group = factor(vs)
  )
  factor_model <- ls_new_binary_regression(
    data, "outcome", c("mpg", "country"), event = "manual", native = FALSE,
    name = "global_term_binary_factor", term_types = list(country = "factor")
  )
  state <- ls_binary_regression_state(factor_model)
  test <- state$term_tests[state$term_tests$term == "country", , drop = FALSE]
  coefficient_names <- strsplit(test$coefficient_names, "\u001f", fixed = TRUE)[[1L]]
  beta <- stats::coef(state$fit)[coefficient_names]
  covariance <- stats::vcov(state$fit)[coefficient_names, coefficient_names, drop = FALSE]

  expect_equal(nrow(test), 1L)
  expect_identical(test$term_test_method, "Wald chi-square")
  expect_equal(test$term_test_statistic,
               as.numeric(crossprod(beta, qr.solve(covariance, beta))), tolerance = 1e-10)
  expect_equal(test$term_test_df, 2)
  country_parent <- state$coefficient_rows[
    state$coefficient_rows$source_term == "country" &
      state$coefficient_rows$row_type == "factor_parent", , drop = FALSE
  ]
  expect_equal(country_parent$statistic, test$term_test_statistic,
               tolerance = 1e-10)
  expect_equal(country_parent$p_value, test$term_test_p,
               tolerance = 1e-10)
  mpg_test <- state$term_tests[state$term_tests$term == "mpg", , drop = FALSE]
  mpg_row <- state$coefficients[state$coefficients$term == "mpg", , drop = FALSE]
  expect_equal(mpg_test$term_test_statistic, mpg_row$statistic^2,
               tolerance = 1e-10)
  expect_equal(mpg_test$term_test_p, mpg_row$p_value, tolerance = 1e-10)
  expect_true(sum(state$coefficient_rows$source_term == "country") >= 3L)
  exported_table <- ls_generalized_linear_model_table(factor_model)
  expect_identical(names(exported_table$global_term_tests),
                   c("Term", "Wald χ²", "df", "df2", "p"))
  expect_equal(nrow(exported_table$global_term_tests), 2L)
  expect_false(any(c("Wald χ²", "df", "df2", "Term p") %in%
                     names(exported_table$coefficients)))
  expect_match(exported_table$note,
               "Global tests shown on parent term rows use Wald χ².", fixed = TRUE)
  expect_false(grepl("#### Term tests",
                     LinkEDA:::.rls_render_table_markdown(exported_table), fixed = TRUE))

  interaction_model <- ls_new_binary_regression(
    data, "outcome", "country:group", event = "manual", native = FALSE,
    name = "global_term_binary_interaction",
    term_types = list(country = "factor", group = "factor")
  )
  interaction_state <- ls_binary_regression_state(interaction_model)
  interaction <- interaction_state$term_tests[
    interaction_state$term_tests$term == "country:group", , drop = FALSE
  ]
  expect_equal(nrow(interaction), 1L)
  expect_identical(interaction$term_test_method, "Wald chi-square")
  expect_false(grepl("LR", interaction$term_test_method, fixed = TRUE))
  interaction_parent <- interaction_state$coefficient_rows[
    interaction_state$coefficient_rows$source_term == "country:group" &
      interaction_state$coefficient_rows$row_type == "term_parent", , drop = FALSE
  ]
  expect_equal(interaction_parent$statistic, interaction$term_test_statistic,
               tolerance = 1e-10)
  expect_equal(interaction_parent$p_value, interaction$term_test_p,
               tolerance = 1e-10)
  hierarchy_note <- LinkEDA:::.rls_global_term_test_hierarchy_note(
    interaction_state$term_tests
  )
  expect_match(hierarchy_note, "conditionally under the current model parameterization", fixed = TRUE)
  interaction_table <- ls_generalized_linear_model_table(interaction_model)
  expect_match(interaction_table$note,
               "conditionally under the current model parameterization", fixed = TRUE)
  expect_match(LinkEDA:::.rls_render_table_markdown(interaction_table),
               "simple effects or the effect plot", fixed = TRUE)
})

test_that("term-test presentation identifies one shared method only once", {
  tests <- data.frame(
    term = c("x", "group"),
    coefficient_names = c("x", "groupb"),
    term_test_method = c("Wald chi-square", "Wald chi-square"),
    term_test_statistic = c(4.25, 6.75),
    term_test_df = c(1, 1),
    term_test_df2 = c(NA, NA),
    term_test_p = c(.039, .009),
    stringsAsFactors = FALSE
  )
  exported <- LinkEDA:::.rls_export_global_term_tests(tests)
  expect_identical(names(exported), c("Term", "Wald χ²", "df", "df2", "p"))
  expect_false("Test" %in% names(exported))

  tests$term_test_method[[2L]] <- "F"
  mixed <- LinkEDA:::.rls_export_global_term_tests(tests)
  expect_identical(names(mixed), c("Term", "Term test", "df", "df2", "p"))
  expect_match(mixed[["Term test"]][[1L]], "Wald χ²: 4.250", fixed = TRUE)
  expect_match(mixed[["Term test"]][[2L]], "F: 6.750", fixed = TRUE)
})

test_that("Poisson, Gaussian-log, and Gamma term tests are joint Wald tests", {
  count_data <- data.frame(
    events = c(0, 1, 1, 2, 3, 4, 2, 5, 7, 6, 8, 9),
    treatment = factor(rep(c("control", "treated"), each = 6)),
    time = seq_len(12),
    exposure = rep(c(1, 1.5, 2), 4)
  )
  poisson <- ls_new_count_regression(
    count_data, "events", c("treatment", "time"), exposure = "exposure",
    native = FALSE, name = "global_term_poisson",
    term_types = list(treatment = "factor")
  )
  poisson_state <- ls_count_regression_state(poisson)
  poisson_test <- poisson_state$term_tests[
    poisson_state$term_tests$term == "treatment", , drop = FALSE
  ]
  expect_identical(poisson_test$term_test_method, "Wald chi-square")

  gaussian_data <- transform(mtcars, positive = mpg + 1)
  gaussian <- ls_new_positive_continuous_model(
    gaussian_data, "positive", c("wt", "cyl"), distribution = "gaussian_log",
    native = FALSE, name = "global_term_gaussian_log"
  )
  gaussian_state <- ls_generalized_linear_model_state(gaussian)
  expect_true(nrow(gaussian_state$term_tests) == 2L)
  expect_true(all(gaussian_state$term_tests$term_test_method == "Wald chi-square"))
  gaussian_test <- gaussian_state$term_tests[
    gaussian_state$term_tests$term == "wt", , drop = FALSE
  ]
  gaussian_z <- gaussian_state$coefficients$statistic[
    gaussian_state$coefficients$term == "wt"
  ]
  expect_equal(gaussian_test$term_test_statistic, unname(gaussian_z^2),
               tolerance = 1e-10)

  gamma <- ls_new_positive_continuous_model(
    gaussian_data, "positive", c("wt", "cyl"), distribution = "Gamma",
    link = "log", native = FALSE, name = "global_term_gamma"
  )
  gamma_state <- ls_generalized_linear_model_state(gamma)
  gamma_test <- gamma_state$term_tests[
    gamma_state$term_tests$term == "wt", , drop = FALSE
  ]
  expect_identical(gamma_test$term_test_method, "Wald chi-square")
})

test_that("negative-binomial terms use Wald tests and exclude theta", {
  skip_if_not_installed("MASS")
  data <- data.frame(
    events = c(0, 1, 4, 2, 8, 3, 14, 6, 20, 9, 30, 13, 45, 18, 60, 24),
    x = rep(0:7, 2),
    group = factor(rep(c("a", "b"), each = 8))
  )
  model <- ls_new_count_regression(
    data, "events", c("x", "group"), distribution = "negative_binomial",
    native = FALSE, name = "global_term_negative_binomial",
    term_types = list(group = "factor")
  )
  state <- ls_count_regression_state(model)
  test <- state$term_tests[state$term_tests$term == "group", , drop = FALSE]
  expect_identical(test$term_test_method, "Wald chi-square")
  expect_false(any(grepl("theta|dispersion", state$term_tests$term,
                         ignore.case = TRUE)))
})

test_that("one-inflated beta terms use mean-submodel Wald tests", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  set.seed(20260913)
  data <- data.frame(
    proportion = c(rep(1, 18), stats::plogis(stats::rnorm(102))),
    x = stats::rnorm(120),
    group = factor(rep(c("a", "b", "c"), each = 40))
  )
  model <- ls_new_proportion_model(
    data, "proportion", c("x", "group"),
    distribution = "beta_one_inflated", native = FALSE,
    name = "global_term_one_inflated_beta",
    term_types = list(group = "factor")
  )
  state <- ls_generalized_linear_model_state(model)
  test <- state$term_tests[state$term_tests$term == "group", , drop = FALSE]
  expect_identical(test$term_test_method, "Wald chi-square")
  expect_equal(test$term_test_df, 2)
  expect_true(is.finite(test$term_test_statistic))
})

test_that("quasi-Poisson uses a joint Wald F test rather than LR", {
  data <- data.frame(
    events = c(0, 1, 3, 2, 7, 4, 12, 5, 16, 8, 21, 10, 28, 12, 35, 15),
    x = rep(0:7, 2),
    group = factor(rep(c("a", "b"), each = 8))
  )
  model <- ls_new_count_regression(
    data, "events", c("x", "group"), distribution = "quasipoisson",
    native = FALSE, name = "global_term_quasipoisson",
    term_types = list(group = "factor")
  )
  tests <- ls_count_regression_state(model)$term_tests

  expect_true(all(tests$term_test_method == "F"))
  expect_true(all(is.finite(tests$term_test_df2)))
  expect_false(any(grepl("LR", tests$term_test_method, fixed = TRUE)))
  expect_true(all(grepl("quasi-likelihood", tests$method, fixed = TRUE)))
})

test_that("MI generalized complete-term tests pool fitted covariances", {
  skip_if_not_installed("mice")
  set.seed(20260912)
  data_sets <- lapply(seq_len(3L), function(index) {
    data.frame(
      y = exp(1 + stats::rnorm(90, sd = 0.25)),
      x = stats::rnorm(90) + index / 10,
      group = factor(rep(c("a", "b", "c"), each = 30))
    )
  })
  fits <- lapply(data_sets, function(data) {
    stats::glm(y ~ x + group, data = data, family = stats::gaussian("log"))
  })
  tests <- LinkEDA:::.rls_mi_generalized_term_omnibus_tests(fits)
  group_test <- tests[tests$term == "group", , drop = FALSE]

  expect_equal(nrow(group_test), 1L)
  expect_identical(group_test$term_test_method, "Rubin Wald chi-square")
  expect_equal(group_test$term_test_df, 2)
  expect_true(is.finite(group_test$term_test_statistic))
  expect_true(is.na(group_test$term_test_df2))
  expect_true(is.finite(group_test$term_test_p))
  expect_true(all(c(
    "term_test_method", "term_test_statistic", "term_test_df",
    "term_test_df2", "term_test_p"
  ) %in% names(tests)))
  expect_false(any(grepl("LR|D1", group_test$term_test_method)))
})

test_that("MI one-inflated beta terms use all mean-model covariance matrices", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  set.seed(20260914)
  data_sets <- lapply(seq_len(3L), function(index) {
    x <- stats::rnorm(300) + index / 20
    group <- factor(rep(c("a", "b", "c"), 100))
    eta <- -0.3 + 0.2 * x + c(a = 0, b = 0.35, c = -0.25)[group]
    at_one <- stats::rbinom(300, 1, 0.12)
    y <- ifelse(
      at_one == 1, 1,
      stats::rbeta(300, stats::plogis(eta) * 8,
                   (1 - stats::plogis(eta)) * 8)
    )
    data.frame(y = y, x = x, group = group)
  })
  fits <- lapply(data_sets, function(current_data) {
    gamlss::gamlss(
      y ~ x + group, data = current_data,
      family = gamlss.dist::BEOI(mu.link = "logit", nu.link = "logit"),
      trace = FALSE
    )
  })
  tests <- LinkEDA:::.rls_mi_generalized_term_omnibus_tests(fits)
  group_test <- tests[tests$term == "group", , drop = FALSE]

  expect_equal(nrow(group_test), 1L)
  expect_identical(group_test$term_test_method, "Rubin Wald chi-square")
  expect_equal(group_test$term_test_df, 2)
  expect_true(is.finite(group_test$term_test_statistic))
  expect_true(is.finite(group_test$term_test_p))
  expect_match(group_test$method, "fitted mean coefficients", fixed = TRUE)
})

test_that("Linear and Lognormal outputs receive conventional F term tests", {
  dataset <- ls_register_dataset("global_term_linear_excluded", mtcars)
  linear <- ls_new_glm(dataset)
  linear <- ls_glm_set_dependent(linear, "mpg")
  linear <- ls_glm_add_predictor(linear, "wt")
  linear <- ls_glm_add_predictor(linear, "cyl")
  linear <- ls_glm_fit(linear)
  linear_record <- LinkEDA:::.rls_glm_model_record(linear)
  expect_true(all(linear_record$term_tests$term_test_method == "F"))
  expect_true(nrow(LinkEDA:::.rls_regression_table_from_glm(linear)$global_term_tests) == 2L)

  lognormal_data <- transform(mtcars, positive = mpg + 1)
  lognormal <- ls_new_positive_continuous_model(
    lognormal_data, "positive", c("wt", "cyl"), distribution = "lognormal",
    native = FALSE, name = "global_term_lognormal_excluded"
  )
  expect_true(all(ls_generalized_linear_model_state(lognormal)$term_tests$term_test_method == "F"))
  expect_equal(nrow(ls_generalized_linear_model_table(lognormal)$global_term_tests), 2L)
})

test_that("verification recipes reproduce model-aware Wald and F term tests", {
  base <- list(
    response = "outcome", terms = c("group", "moment", "group:moment"),
    predictors = c("group", "moment", "group:moment"),
    term_types = list(group = "factor", moment = "factor"),
    factor_reference_levels = list(group = "Control", moment = "Before"),
    centered_predictors = character(), response_bounds = NULL,
    binary_regression = FALSE, count_regression = FALSE, exposure = "",
    event = NULL, reference = NULL
  )
  gaussian_log <- modifyList(base, list(family = "gaussian_log", link = "log"))
  lognormal <- modifyList(base, list(family = "lognormal", link = "identity"))
  quasi <- modifyList(base, list(
    family = "quasipoisson", link = "log", count_regression = TRUE,
    count_distribution = "quasipoisson"
  ))
  nb <- modifyList(base, list(
    family = "poisson", link = "log", count_regression = TRUE,
    count_distribution = "negative_binomial"
  ))
  beta_mi <- modifyList(base, list(
    family = "beta", link = "logit", response_bounds = c(0, 1)
  ))

  gaussian_code <- LinkEDA:::.rls_generalized_verification_r_code(
    gaussian_log, FALSE
  )$code
  lognormal_code <- LinkEDA:::.rls_generalized_verification_r_code(
    lognormal, FALSE
  )$code
  quasi_code <- LinkEDA:::.rls_generalized_verification_r_code(quasi, FALSE)$code
  nb_mi_code <- LinkEDA:::.rls_generalized_verification_r_code(nb, TRUE)$code
  beta_mi_code <- LinkEDA:::.rls_generalized_verification_r_code(
    beta_mi, TRUE
  )$code

  expect_match(gaussian_code, 'method = "Wald chi-square"', fixed = TRUE)
  expect_match(lognormal_code, 'method = "F"', fixed = TRUE)
  expect_match(quasi_code, 'method = "F"', fixed = TRUE)
  expect_false(grepl('method = "LR chi-square"', quasi_code, fixed = TRUE))
  expect_match(nb_mi_code, "Rubin pooled Wald chi-square", fixed = TRUE)
  expect_match(nb_mi_code, "MASS::glm.nb(model_formula", fixed = TRUE)
  expect_false(grepl("LinkEDA:::", nb_mi_code, fixed = TRUE))
  expect_match(beta_mi_code, "Rubin pooled Wald chi-square", fixed = TRUE)
  expect_false(grepl("mice::D1", beta_mi_code, fixed = TRUE))
  for (code in list(
    gaussian_code, lognormal_code, quasi_code, nb_mi_code, beta_mi_code
  )) {
    expect_false(grepl("stats::drop.terms", code, fixed = TRUE))
    expect_false(grepl('method = "LR chi-square"', code, fixed = TRUE))
    expect_silent(parse(text = code))
  }
})
