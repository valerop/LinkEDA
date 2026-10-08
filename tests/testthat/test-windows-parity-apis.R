test_that("generalized model comparison uses R likelihood-ratio tests", {
  base <- ls_new_generalized_linear_model(
    mtcars, response = "mpg", terms = "wt", family = "gaussian",
    link = "identity", name = "generalized_comparison_data", native = FALSE
  )
  group <- ls_generalized_linear_model_state(base)$group
  full <- ls_new_generalized_linear_model(
    group, response = "mpg", terms = c("wt", "hp"), family = "gaussian",
    link = "identity", name = "generalized_comparison_full", native = FALSE
  )
  comparison <- ls_compare_generalized_linear_models(base, full, native = FALSE)
  expect_true(comparison$common_rows)
  expect_true(comparison$tests$available[[1L]])
  expect_match(comparison$tests$reason[[1L]], "stats::anova")
  expect_true(all(is.finite(comparison$models$AIC)))
})

test_that("generalized model comparison supports an intercept-only first model", {
  intercept_only <- ls_new_generalized_linear_model(
    mtcars, response = "mpg", terms = character(), family = "gaussian",
    link = "identity", name = "generalized_comparison_intercept", native = FALSE,
    .allow_intercept_only = TRUE
  )

  comparison <- ls_compare_generalized_linear_models(
    intercept_only, native = FALSE
  )

  expect_equal(nrow(comparison$models), 1L)
  expect_identical(comparison$models$terms[[1L]], "")
  expect_equal(nrow(comparison$tests), 0L)
  expect_true(is.finite(comparison$models$AIC[[1L]]))
})

test_that("internal generalized comparison models do not publish standalone GLM windows", {
  state <- LinkEDA:::.rls_state
  previous_started <- state$process_started
  on.exit({ state$process_started <- previous_started }, add = TRUE)
  state$process_started <- TRUE

  standalone_syncs <- 0L
  pooled_syncs <- 0L
  testthat::local_mocked_bindings(
    ls_analysis_scope = function(group = NULL) {
      record <- LinkEDA:::.rls_dataset_record(group)
      list(dataset_id = group, kind = "all", source = "all_data",
           description = "All observations", rows = seq_len(nrow(record$data)),
           n = nrow(record$data), total_n = nrow(record$data))
    },
    .rls_generalized_glm_sync_native = function(record) {
      standalone_syncs <<- standalone_syncs + 1L
      invisible(TRUE)
    },
    .rls_generalized_glm_sync_native_pooled = function(record) {
      pooled_syncs <<- pooled_syncs + 1L
      invisible(TRUE)
    },
    .package = "LinkEDA"
  )

  first <- ls_new_generalized_linear_model(
    mtcars, response = "mpg", terms = character(), family = "gaussian",
    link = "identity", name = "comparison_native_owner_first", native = FALSE,
    .allow_intercept_only = TRUE
  )
  group <- ls_generalized_linear_model_state(first)$group
  second <- ls_new_generalized_linear_model(
    group, response = "mpg", terms = c("wt", "hp"), family = "gaussian",
    link = "identity", name = "comparison_native_owner_second", native = FALSE
  )

  expect_false(LinkEDA:::.rls_generalized_glm_record(first)$native_sync_enabled)
  expect_false(LinkEDA:::.rls_generalized_glm_record(second)$native_sync_enabled)
  expect_silent(ls_generalized_linear_model_fit(first))
  expect_silent(ls_generalized_linear_model_fit(second))
  expect_silent(ls_compare_generalized_linear_models(first, second, native = FALSE))
  expect_identical(standalone_syncs, 0L)
  expect_identical(pooled_syncs, 0L)
})

test_that("multivariate plot APIs validate explicit variable sets", {
  group <- LinkEDA:::.rls_register_dataset("multivariate_validation", mtcars)
  record <- LinkEDA:::.rls_dataset_record(group)
  expect_identical(
    LinkEDA:::.rls_multivariate_variables(record, c("mpg", "wt", "hp")),
    c("mpg", "wt", "hp")
  )
  expect_error(LinkEDA:::.rls_multivariate_variables(record, "mpg"), "at least two")
  factor_record <- record
  factor_record$data$cyl <- factor(factor_record$data$cyl)
  expect_error(
    LinkEDA:::.rls_multivariate_variables(factor_record, c("mpg", "cyl")),
    "not numeric"
  )
})

test_that("model trellis validates its response before opening native UI", {
  group <- LinkEDA:::.rls_register_dataset(
    "trellis_validation", transform(mtcars, cyl = factor(cyl))
  )
  expect_error(
    ls_new_model_trellis(group, response = "missing", columns = "cyl"),
    "was not found"
  )
  expect_error(
    ls_new_model_trellis(group, response = "mpg", columns = "cyl", rows = "cyl"),
    "must be different"
  )
})
