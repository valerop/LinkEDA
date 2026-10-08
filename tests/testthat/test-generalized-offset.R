test_that("a numeric offset reaches the R fit on the link scale", {
  data <- data.frame(
    y = c(2, 3, 4, 4, 7, 9, 11, 14, 15, 18, 20, 23),
    x = seq(-1, 1, length.out = 12),
    known = seq(-0.35, 0.4, length.out = 12),
    exposure = seq(1, 2, length.out = 12)
  )
  gaussian <- ls_new_generalized_linear_model(
    data, "y", "x", family = "gaussian", link = "identity",
    offset = "known", native = FALSE, name = "offset_gaussian"
  )
  gaussian_state <- LinkEDA:::.rls_generalized_glm_record(gaussian)
  gaussian_direct <- stats::glm(y ~ x + offset(known), data = data,
                                family = stats::gaussian("identity"))
  expect_equal(stats::coef(gaussian_state$fit), stats::coef(gaussian_direct),
               tolerance = 1e-10)
  expect_equal(stats::fitted(gaussian_state$fit), stats::fitted(gaussian_direct),
               tolerance = 1e-10)
  expect_identical(gaussian_state$offset_variable, "known")
  expect_match(paste(deparse(stats::formula(gaussian_state$fit)), collapse = " "),
               "offset")

  count <- ls_new_count_regression(
    data, "y", "x", distribution = "poisson", exposure = "exposure",
    offset = "known", native = FALSE, name = "offset_poisson"
  )
  count_state <- ls_count_regression_state(count)
  count_direct <- stats::glm(
    y ~ x + offset(log(exposure)) + offset(known), data = data,
    family = stats::poisson("log")
  )
  expect_equal(stats::coef(count_state$fit), stats::coef(count_direct),
               tolerance = 1e-10)
  expect_equal(stats::fitted(count_state$fit), stats::fitted(count_direct),
               tolerance = 1e-10)
})

test_that("binary offsets are applied by R and invalid offsets are rejected", {
  data <- data.frame(
    event = factor(c(0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1)),
    x = seq(-1, 1, length.out = 12),
    known = seq(-0.4, 0.3, length.out = 12),
    label = letters[1:12]
  )
  model <- ls_new_binary_regression(
    data, "event", "x", event = "1", offset = "known",
    native = FALSE, name = "offset_binary"
  )
  state <- ls_binary_regression_state(model)
  direct <- stats::glm(I(event == "1") ~ x + offset(known), data = data,
                       family = stats::binomial("logit"))
  expect_equal(stats::coef(state$fit), stats::coef(direct), tolerance = 1e-10)
  expect_error(ls_new_binary_regression(data, "event", "x", offset = "label",
                                        native = FALSE), "numeric column")
  expect_error(ls_new_binary_regression(data, "event", "x", offset = "event",
                                        native = FALSE), "distinct from the response")
})

test_that("link-scale offsets reject non-finite values in the fitted scope", {
  data <- data.frame(
    y = c(1L, 2L, 3L, 4L, 5L, 6L),
    x = seq_len(6),
    known = c(-0.2, 0, 0.1, 0.2, Inf, NA_real_)
  )
  expect_error(
    ls_new_count_regression(
      data, "y", "x", offset = "known", native = FALSE,
      name = "offset_nonfinite_all"
    ),
    "must be finite"
  )
  scoped <- ls_new_count_regression(
    data, "y", "x", offset = "known", scope = "selected",
    .selected_rows = 1:4, native = FALSE,
    name = "offset_nonfinite_outside_scope"
  )
  expect_true(ls_count_regression_state(scoped)$fit$converged)
  expect_error(
    ls_new_count_regression(
      transform(data, known = NA_real_), "y", "x", offset = "known",
      native = FALSE, name = "offset_all_missing"
    ),
    "no observed values"
  )
})

test_that("one count variable cannot be both exposure and link-scale offset", {
  data <- data.frame(
    y = c(1L, 2L, 3L, 4L, 5L, 6L),
    x = seq_len(6),
    person_time = c(1, 2, 1.5, 3, 2.5, 4)
  )
  expect_error(
    ls_new_count_regression(
      data, "y", "x", exposure = "person_time",
      offset = "person_time", native = FALSE
    ),
    "cannot be both Exposure and Link-scale offset"
  )
  model <- ls_new_count_regression(
    data, "y", "x", offset = "person_time", native = FALSE,
    name = "offset_then_exposure"
  )
  expect_error(
    ls_count_regression_set_exposure(model, "person_time"),
    "cannot be both Exposure and Link-scale offset"
  )
})

test_that("beta-binomial offsets are passed to glmmTMB", {
  suppressWarnings(skip_if_not_installed("glmmTMB"))
  set.seed(14)
  n <- 200L
  data <- data.frame(x = stats::rnorm(n), known = stats::runif(n, -0.3, 0.3),
                     trials = rep(12L, n))
  mean_probability <- stats::plogis(-0.2 + 0.5 * data$x + data$known)
  probability <- stats::rbeta(n, 4 * mean_probability,
                              4 * (1 - mean_probability))
  data$success <- stats::rbinom(
    n, data$trials, probability
  )
  model <- suppressWarnings(ls_new_count_regression(
    data, "success", "x", distribution = "beta_binomial",
    trials = "trials", offset = "known", native = FALSE,
    name = "offset_beta_binomial"
  ))
  direct <- suppressWarnings(glmmTMB::glmmTMB(
    cbind(success, trials - success) ~ x + offset(known), data = data,
    family = glmmTMB::betabinomial(link = "logit")
  ))
  expect_true(isTRUE(ls_count_regression_state(model)$fit$sdr$pdHess))
  expect_equal(unname(glmmTMB::fixef(ls_count_regression_state(model)$fit)$cond),
               unname(glmmTMB::fixef(direct)$cond), tolerance = 1e-8)
})

test_that("multiple-imputation workers retain the offset specification", {
  skip_if_not_installed("mice")
  data <- data.frame(
    y = c(2, 3, 4, 4, 7, 9, 11, 14, 15, 18, 20, 23),
    x = seq(-1, 1, length.out = 12),
    known = seq(-0.35, 0.4, length.out = 12)
  )
  group <- ls_register_dataset("offset_mi_data", data)
  on.exit(ls_unregister_dataset(group), add = TRUE)
  dataset <- LinkEDA:::.rls_dataset_record(group)
  dataset$dataset_type <- "multiple_imputation"
  dataset$imputation_id <- "offset_mi_data:completed"
  dataset$imputation_count <- 2L
  dataset$completed_datasets <- list(data, transform(data, y = y + c(0, 1)))
  dataset$original_data <- data
  dataset$original_row_ids <- seq_len(nrow(data))
  dataset$missing_cell_mask <- as.data.frame(lapply(data, is.na))
  LinkEDA:::.rls_set_dataset_record(dataset)
  previous <- getOption("LinkEDA.mi_parallel")
  on.exit(options(LinkEDA.mi_parallel = previous), add = TRUE)
  options(LinkEDA.mi_parallel = FALSE)
  model <- ls_new_generalized_linear_model(
    group, "y", "x", family = "gaussian", link = "identity",
    offset = "known", native = FALSE, name = "offset_mi_fit"
  )
  state <- LinkEDA:::.rls_generalized_glm_record(model)
  expect_length(state$fits_by_imputation, 2L)
  for (i in seq_along(state$fits_by_imputation)) {
    direct <- stats::glm(y ~ x + offset(known),
                         data = dataset$completed_datasets[[i]],
                         family = stats::gaussian("identity"))
    expect_equal(stats::coef(state$fits_by_imputation[[i]]),
                 stats::coef(direct), tolerance = 1e-10)
  }
})

test_that("model comparisons require the same offset and retain it in verification code", {
  data <- data.frame(
    y = c(1, 2, 1, 4, 3, 5, 2, 6, 4, 7, 5, 9),
    x = seq(-1, 1, length.out = 12),
    z = rep(c(-1, 1), 6),
    known = seq(-0.2, 0.3, length.out = 12)
  )
  group <- ls_register_dataset("offset_comparison_data", data)
  on.exit(ls_unregister_dataset(group), add = TRUE)
  reduced <- ls_new_count_regression(
    group, "y", "x", offset = "known", native = FALSE,
    name = "offset_compare_reduced"
  )
  full <- ls_new_count_regression(
    group, "y", c("x", "z"), offset = "known", native = FALSE,
    name = "offset_compare_full"
  )
  comparison <- ls_compare_count_regression_models(reduced, full, native = FALSE)
  expect_true(comparison$tests$available[[1L]])
  expect_equal(comparison$models$offset, rep("known", 2L))
  code <- comparison$analysis_provenance$verification_r_code$comparison
  expect_match(code, "offset\\(known\\)")

  without_offset <- ls_new_count_regression(
    group, "y", c("x", "z"), native = FALSE,
    name = "offset_compare_mismatch"
  )
  expect_error(
    ls_compare_count_regression_models(reduced, without_offset, native = FALSE),
    "different fitted responses, weights, or offsets"
  )
})
