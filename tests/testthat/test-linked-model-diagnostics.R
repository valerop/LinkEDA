test_that("generalized diagnostic handles follow their exact refitted model", {
  set.seed(20260928)
  data <- data.frame(x = seq(-1, 1, length.out = 80L))
  data$y <- stats::rpois(nrow(data), exp(1 + 0.7 * data$x))
  group <- paste0("linked_diagnostics_", Sys.getpid())
  ls_register_dataset(group, data)
  model <- ls_new_generalized_linear_model(
    group, "y", "x", family = "poisson", link = "log", native = FALSE)
  kinds <- c("residual_histogram", "residuals_vs_fitted", "normal_qq")
  handles <- lapply(kinds, function(kind)
    ls_generalized_linear_model_open_diagnostic(model, kind, native = FALSE))
  snapshot <- function(handle) get(handle$id, envir = LinkEDA:::.rls_state$glm_diagnostic_plots)
  before <- lapply(handles, snapshot)
  expect_true(all(vapply(before, function(plot) identical(plot$model_id, model$id), logical(1L))))
  expect_true(all(vapply(before, function(plot) !plot$is_stale, logical(1L))))

  model <- ls_generalized_linear_model_set_family(model, "gaussian", link = "identity")
  after <- lapply(handles, snapshot)
  current <- ls_generalized_linear_model_state(model)
  for (i in seq_along(handles)) {
    expect_identical(after[[i]]$displayed_fit_version, current$fit_version)
    expect_identical(after[[i]]$displayed_diagnostics_version,
                     current$diagnostics_version)
    expect_identical(after[[i]]$data, current$diagnostics)
    expect_false(after[[i]]$is_stale)
    expect_false(isTRUE(all.equal(before[[i]]$data$residual,
                                  after[[i]]$data$residual)))
  }
})

test_that("special diagnostic handles retain the distribution payload on refresh", {
  state <- LinkEDA:::.rls_state
  id <- paste0("diagnostic_payload_", Sys.getpid())
  handle_id <- paste0(id, "_plot")
  distribution <- data.frame(observed = c(2, 3), predicted = c(2.2, 2.8))
  assign(handle_id, list(id = handle_id, model_id = id,
    type = "observed_predicted_distribution", is_stale = TRUE,
    data = data.frame()), envir = state$glm_diagnostic_plots)
  on.exit(rm(list = handle_id, envir = state$glm_diagnostic_plots), add = TRUE)
  LinkEDA:::.rls_glm_refresh_diagnostic_plots(list(
    id = id, fit_version = 2L, diagnostics_version = 3L,
    diagnostics = data.frame(row_id = 1:2, residual = c(0.1, -0.1)),
    summary = list(observed_predicted_distribution = distribution)))
  plot <- get(handle_id, envir = state$glm_diagnostic_plots)
  expect_identical(plot$data, distribution)
  expect_false(plot$is_stale)
  LinkEDA:::.rls_glm_mark_diagnostic_plots_stale(id)
  stale <- get(handle_id, envir = state$glm_diagnostic_plots)
  expect_true(stale$is_stale)
  expect_equal(nrow(stale$data), 0L)
})

test_that("linear comparison diagnostics follow their own model column", {
  group <- paste0("linked_linear_comparison_", Sys.getpid())
  ls_register_dataset(group, datasets::mtcars)
  comparison <- ls_new_regression_comparison(
    data = group, response = "mpg",
    models = list("wt", c("wt", "hp")),
    native = FALSE)
  first <- ls_regression_comparison_open_diagnostic(
    comparison, 1L, "residuals_vs_fitted", native = FALSE)
  second <- ls_regression_comparison_open_diagnostic(
    comparison, 2L, "residuals_vs_fitted", native = FALSE)
  snapshot <- function(handle)
    get(handle$id, envir = LinkEDA:::.rls_state$glm_diagnostic_plots)
  original_first <- snapshot(first)
  original_second <- snapshot(second)

  comparison <- ls_regression_comparison_set_term(
    comparison, 1L, "hp", included = TRUE)
  updated_first <- snapshot(first)
  updated_second <- snapshot(second)
  expect_gt(updated_first$displayed_fit_version,
            original_first$displayed_fit_version)
  expect_identical(updated_first$displayed_diagnostics_version,
                   updated_first$displayed_fit_version)
  expect_false(updated_first$is_stale)
  expect_false(isTRUE(all.equal(original_first$data$residual,
                                updated_first$data$residual)))
  expect_identical(updated_second$data, original_second$data)
  expect_identical(updated_second$displayed_fit_version,
                   original_second$displayed_fit_version)
})
