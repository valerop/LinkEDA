.runtime_register_mi <- function(name, completed) {
  original <- completed[[1L]]
  original$x[seq(7L, nrow(original), by = 29L)] <- NA_real_
  id <- ls_register_dataset(name, completed[[1L]])
  record <- LinkEDA:::.rls_dataset_record(id)
  record$dataset_type <- "multiple_imputation"
  record$imputation_id <- paste0(name, "_imp")
  record$imputation_count <- length(completed)
  record$completed_datasets <- completed
  record$original_data <- original
  record$original_row_ids <- seq_len(nrow(original))
  record$missing_cell_mask <- as.data.frame(lapply(original, is.na),
    stringsAsFactors = FALSE)
  LinkEDA:::.rls_set_dataset_record(record)
  id
}

.runtime_expect_numeric_equal <- function(first, second, tolerance = 1e-10) {
  common <- intersect(names(first), names(second))
  common <- common[vapply(first[common], is.numeric, logical(1L)) &
    vapply(second[common], is.numeric, logical(1L))]
  for (column in common) {
    expect_equal(first[[column]], second[[column]], tolerance = tolerance,
      info = column)
  }
}

.runtime_expect_state_equal <- function(sequential, parallel) {
  expect_identical(length(sequential$fits_by_imputation),
    length(parallel$fits_by_imputation))
  for (i in seq_along(sequential$fits_by_imputation)) {
    first <- sequential$fits_by_imputation[[i]]
    second <- parallel$fits_by_imputation[[i]]
    expect_identical(class(first), class(second), info = paste("fit", i))
    expect_equal(LinkEDA:::.rls_glm_mean_coefficients(first),
      LinkEDA:::.rls_glm_mean_coefficients(second), tolerance = 1e-12,
      info = paste("coefficients", i))
    expect_equal(LinkEDA:::.rls_glm_mean_vcov(first),
      LinkEDA:::.rls_glm_mean_vcov(second), tolerance = 1e-12,
      info = paste("covariance", i))
  }
  .runtime_expect_numeric_equal(sequential$coefficient_rows,
    parallel$coefficient_rows)
  .runtime_expect_numeric_equal(sequential$parent_term_tests,
    parallel$parent_term_tests)
  expect_equal(sequential$execution_diagnostics$workers, 1L)
  expect_equal(parallel$execution_diagnostics$workers, 2L)
  expect_identical(parallel$execution_diagnostics$fits$imputation,
    seq_along(parallel$fits_by_imputation))
  expect_true(all(parallel$execution_diagnostics$fits$success))
}

.runtime_expect_original_diagnostics <- function(model, state) {
  record <- LinkEDA:::.rls_generalized_glm_record(model)
  formula <- LinkEDA:::.rls_generalized_glm_formula_object(record)
  completed <- LinkEDA:::.rls_mi_fit_data_by_imputation(
    record, formula, if (is.null(record$scope)) "all" else record$scope)
  for (i in seq_along(state$fits_by_imputation)) {
    fit_record <- record
    complete <- completed[[i]]
    if (isTRUE(record$binary_regression)) {
      prepared <- LinkEDA:::.rls_binary_prepare_generalized_fit(
        record, fit_record, complete)
      fit_record <- prepared$fit_record
      complete <- prepared$complete
    }
    complete <- LinkEDA:::.rls_glm_transform_bounded_response(
      fit_record, complete)
    expected <- LinkEDA:::.rls_generalized_glm_diagnostics(
      fit_record, state$fits_by_imputation[[i]], complete,
      imputation_index = i)
    actual <- state$diagnostics_by_imputation[[i]]
    if (isTRUE(record$binary_regression)) {
      actual <- actual[, setdiff(names(actual), c("observed_binary",
        "observed_label", "observed_original")), drop = FALSE]
    }
    # The model record increments its version after fitting. The cache key
    # records the fit-time version, so a fresh diagnostic call has a new key.
    expect_equal(actual[, setdiff(names(actual), "diagnostic_cache_key"),
      drop = FALSE], expected[, setdiff(names(expected),
      "diagnostic_cache_key"), drop = FALSE], tolerance = 1e-12,
      info = paste("diagnostics from original R function, imputation", i))
    expected_distribution <- LinkEDA:::.rls_discrete_distribution_summary(
      fit_record, state$fits_by_imputation[[i]], complete,
      expected)$observed_predicted_distribution
    expect_equal(state$summary$observed_predicted_distribution_by_imputation[[i]],
      expected_distribution, tolerance = 1e-12,
      info = paste("R distribution summary, imputation", i))
  }
}

test_that("MI generalized models are numerically identical with one and two workers", {
  skip_if_not_installed("mice")
  skip_if_not_installed("broom")
  skip_if_not_installed("MASS")
  suppressWarnings(skip_if_not_installed("glmmTMB"))
  set.seed(20260921)
  n <- 180L
  x <- seq(-1.8, 1.8, length.out = n)
  g <- factor(rep(c("A", "B", "C"), length.out = n),
    levels = c("A", "B", "C"))
  g_effect <- c(A = 0, B = .35, C = -.25)[as.character(g)]
  eta <- .4 + .55 * x + g_effect +
    x * c(A = 0, B = .18, C = -.12)[as.character(g)]
  gaussian <- 3 + eta + stats::rnorm(n, sd = .65)
  binary <- stats::rbinom(n, 1L, stats::plogis(-.4 + eta / 1.6))
  count <- stats::rnbinom(n, mu = exp(.55 + eta / 3), size = 2.4)
  trials <- rep(12L, n)
  beta_mean <- stats::plogis(-.7 + eta / 2)
  latent_probability <- stats::rbeta(n, 5 * beta_mean, 5 * (1 - beta_mean))
  successes <- stats::rbinom(n, trials, latent_probability)
  completed <- lapply(seq_len(3L), function(i) data.frame(
    gaussian = gaussian + stats::rnorm(n, sd = .012 * i),
    binary = binary, count = count, successes = successes, trials = trials,
    x = x + stats::rnorm(n, sd = .008 * i), g = g
  ))
  id <- .runtime_register_mi("mi_parallel_equivalence", completed)
  on.exit(ls_unregister_dataset(id), add = TRUE)
  terms <- c("x", "g", "x:g")
  cases <- list(
    gaussian = function(name) ls_new_generalized_linear_model(
      id, "gaussian", terms, family = "gaussian", link = "identity",
      native = FALSE, name = name),
    binary = function(name) ls_new_binary_regression(
      id, "binary", terms, link = "logit", event = "1", reference = "0",
      native = FALSE, name = name),
    negative_binomial = function(name) ls_new_count_regression(
      id, "count", terms, distribution = "negative_binomial",
      native = FALSE, name = name),
    beta_binomial = function(name) ls_new_count_regression(
      id, "successes", terms, distribution = "beta_binomial", trials = "trials",
      native = FALSE, name = name)
  )
  states <- list()
  for (label in names(cases)) {
    sequential_model <- withr::with_options(list(LinkEDA.mi_workers = 1L),
      cases[[label]](paste0("mi_runtime_", label, "_sequential")))
    parallel_model <- withr::with_options(list(LinkEDA.mi_workers = 2L),
      cases[[label]](paste0("mi_runtime_", label, "_parallel")))
    state_function <- if (label == "binary") ls_binary_regression_state else if (
      label %in% c("negative_binomial", "beta_binomial")) {
      ls_count_regression_state
    } else ls_generalized_linear_model_state
    sequential_state <- state_function(sequential_model)
    parallel_state <- state_function(parallel_model)
    .runtime_expect_state_equal(sequential_state, parallel_state)
    .runtime_expect_original_diagnostics(parallel_model, parallel_state)
    states[[label]] <- list(
      sequential_model = sequential_model, parallel_model = parallel_model,
      sequential = sequential_state, parallel = parallel_state
    )
  }

  skip_if_not_installed("emmeans")
  post_events <- list()
  withr::local_options(list(LinkEDA.mi_progress_callback = function(event)
    post_events[[length(post_events) + 1L]] <<- event))
  for (label in names(states)) {
    item <- states[[label]]
    pairwise_function <- if (label == "binary") {
      ls_binary_regression_pairwise
    } else if (label %in% c("negative_binomial", "beta_binomial")) {
      ls_count_regression_pairwise
    } else ls_generalized_linear_model_pairwise
    interaction_function <- if (label == "binary") {
      ls_binary_regression_interaction
    } else if (label %in% c("negative_binomial", "beta_binomial")) {
      ls_count_regression_interaction
    } else ls_generalized_linear_model_interaction
    first_pairwise <- pairwise_function(item$sequential_model, "g")
    second_pairwise <- pairwise_function(item$parallel_model, "g")
    .runtime_expect_numeric_equal(first_pairwise, second_pairwise)
    pairwise_timing <- attr(second_pairwise, "execution_diagnostics")
    expect_identical(pairwise_timing$operation, "pairwise_comparisons")
    expect_equal(pairwise_timing$imputations, 3L)
    expect_true(all(unlist(pairwise_timing[grepl("seconds$", names(pairwise_timing))]) >= 0))
    first_interaction <- if (label == "beta_binomial") {
      interaction_function(item$sequential_model, "x:g",
        quantity = "predicted_probability")
    } else interaction_function(item$sequential_model, "x:g")
    second_interaction <- if (label == "beta_binomial") {
      interaction_function(item$parallel_model, "x:g",
        quantity = "predicted_probability")
    } else interaction_function(item$parallel_model, "x:g")
    .runtime_expect_numeric_equal(first_interaction$estimates,
      second_interaction$estimates)
    .runtime_expect_numeric_equal(first_interaction$plot_data,
      second_interaction$plot_data)
    expect_identical(second_interaction$execution_diagnostics$operation,
      "interaction_interpretation")
    expect_equal(second_interaction$execution_diagnostics$imputations, 3L)
    expect_true(all(unlist(second_interaction$execution_diagnostics[
      grepl("seconds$", names(second_interaction$execution_diagnostics))
    ]) >= 0))
  }
  pairwise_phases <- vapply(Filter(function(event) grepl("-pairwise$",
    event$task_id), post_events), `[[`, character(1L), "phase")
  interaction_phases <- vapply(Filter(function(event) grepl("-interaction$",
    event$task_id), post_events), `[[`, character(1L), "phase")
  expect_true(all(c("preparing", "emmeans", "pairwise", "formatting",
    "completed") %in% pairwise_phases))
  expect_true(all(c("preparing", "emmeans", "formatting",
    "completed") %in% interaction_phases))
})
