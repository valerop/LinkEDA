mixed_school_data <- function() {
  set.seed(123)
  school <- data.frame(
    school = factor(rep(1:4, each = 60)),
    classroom = factor(rep(1:12, each = 20)),
    condition = factor(rep(c("Control", "Treatment"), 120)),
    pretest = rnorm(240)
  )
  u <- rnorm(12, 0, 4)
  school$score <- 50 +
    3 * (school$condition == "Treatment") +
    2 * school$pretest +
    u[school$classroom] +
    rnorm(240, 0, 5)
  school$passed <- rbinom(
    240,
    1,
    plogis(-0.5 + 0.8 * (school$condition == "Treatment") +
             0.5 * school$pretest + u[school$classroom] / 8)
  )
  school
}

test_that("mixed model API is exported", {
  exports <- getNamespaceExports("LinkEDA")
  expect_true("ls_new_linear_mixed_model" %in% exports)
  expect_true("ls_new_generalized_mixed_model" %in% exports)
  expect_true("ls_mixed_model_state" %in% exports)
  expect_true("ls_mixed_model_table" %in% exports)
  expect_true("ls_copy_mixed_model_table" %in% exports)
  expect_true("ls_export_mixed_model_table" %in% exports)
  expect_true("ls_mixed_model_refit" %in% exports)
  expect_true("ls_mixed_model_add_fixed_effect" %in% exports)
  expect_true("ls_mixed_model_remove_fixed_effect" %in% exports)
  expect_true("ls_mixed_model_add_random_effect" %in% exports)
  expect_true("ls_mixed_model_remove_random_effect" %in% exports)
  expect_true("ls_mixed_model_add_random_slope" %in% exports)
  expect_true("ls_mixed_model_remove_random_slope" %in% exports)
  expect_true("ls_mixed_model_diagnostics" %in% exports)
  expect_true("ls_mixed_model_open_diagnostic" %in% exports)
})

test_that("mixed model formula is generated from structured random effects", {
  skip_if_not_installed("lme4")
  school <- mixed_school_data()
  ls_register_dataset("mixed_formula_school", school)
  ls_set_active_dataset("mixed_formula_school")

  record <- LinkEDA:::.rls_new_mixed_model_record(
    data = NULL,
    response = "score",
    fixed = c("condition", "pretest"),
    random = list(list(group = "classroom", terms = c("1", "pretest"))),
    model_type = "linear_mixed_model",
    method = "REML"
  )
  expect_equal(record$formula, "`score` ~ `condition` + `pretest` + (1 + `pretest` | `classroom`)")
  expect_equal(record$random_effects[[1]]$group, "classroom")
  expect_equal(record$random_effects[[1]]$terms, c("1", "pretest"))

  crossed <- LinkEDA:::.rls_new_mixed_model_record(
    data = "mixed_formula_school",
    response = "score",
    fixed = "pretest",
    random = list(list(group = "classroom", terms = "1"), list(group = "school", terms = "1")),
    model_type = "linear_mixed_model",
    method = "ML"
  )
  expect_equal(crossed$formula, "`score` ~ `pretest` + (1 | `classroom`) + (1 | `school`)")
  expect_length(crossed$random_effects, 2L)
})

test_that("linear mixed model fits with lme4 and stores structured tables", {
  skip_if_not_installed("lme4")
  school <- mixed_school_data()
  school$score[4] <- NA
  ls_register_dataset("mixed_lmm_school", school)
  ls_set_active_dataset("mixed_lmm_school")

  model <- ls_new_linear_mixed_model(
    response = "score",
    fixed = c("condition", "pretest"),
    random = list(list(group = "classroom", terms = "1")),
    method = "REML",
    native = FALSE
  )
  state <- ls_mixed_model_state(model)
  table <- ls_mixed_model_table(model)

  expect_s3_class(model, "rlispstat_linear_mixed_model")
  expect_true(inherits(state$model_object, "lmerMod"))
  expect_equal(state$model_type, "linear_mixed_model")
  expect_equal(state$method, "REML")
  expect_true(nrow(state$fixed_effect_table) >= 5L)
  expect_true(any(state$fixed_effect_table$row_type == "factor_parent"))
  expect_true(any(state$fixed_effect_table$row_type == "reference"))
  expect_true(any(state$fixed_effect_table$display_label == "  Treatment"))
  expect_true(nrow(state$random_effect_table) > 0L)
  expect_true("classroom" %in% state$random_effect_table$group)
  expect_true(all(c("n_used", "n_excluded", "aic", "bic", "sigma") %in% names(state$fit_statistics)))
  expect_equal(state$fit_statistics$n_excluded, 1L)
  expect_false(4L %in% state$rows_used_original_ids)
  expect_true(4L %in% state$rows_excluded_original_ids)
  expect_s3_class(table, "rlispstat_mixed_model_table")
  expect_true("Fixed effects" %in% strsplit(LinkEDA:::.rls_render_mixed_model_text(table), "\n", fixed = TRUE)[[1L]])
  native_text <- LinkEDA:::.rls_render_mixed_model_native_text(table)
  expect_match(native_text, "Variable\tType\tb\tSE", fixed = TRUE)
  expect_match(native_text, "Statistic\tValue", fixed = TRUE)
})

test_that("generalized mixed model fits binomial/logit and stores diagnostics", {
  skip_if_not_installed("lme4")
  school <- mixed_school_data()
  school$passed[5] <- NA
  ls_register_dataset("mixed_glmm_school", school)

  model <- ls_new_generalized_mixed_model(
    data = "mixed_glmm_school",
    response = "passed",
    fixed = c("condition", "pretest"),
    random = list(list(group = "classroom", terms = "1")),
    family = "binomial",
    link = "logit",
    native = FALSE
  )
  state <- ls_mixed_model_state(model)
  table <- ls_mixed_model_table(model)

  expect_s3_class(model, "rlispstat_generalized_mixed_model")
  expect_true(inherits(state$model_object, "glmerMod"))
  expect_equal(state$model_type, "generalized_linear_mixed_model")
  expect_equal(state$family, "binomial")
  expect_equal(state$link, "logit")
  expect_true(nrow(state$fixed_effect_table) >= 5L)
  expect_true(nrow(state$random_effect_table) > 0L)
  expect_true(all(c("n_used", "n_excluded", "aic", "bic", "deviance") %in% names(state$fit_statistics)))
  expect_equal(state$fit_statistics$n_excluded, 1L)
  expect_false(5L %in% state$rows_used_original_ids)
  expect_true(all(c("row_id", "observed", "fitted", "residual", "pearson_residual", "deviance_residual") %in% names(state$diagnostic_data)))
  expect_equal(state$fit_version, 1L)
  expect_equal(table$family, "binomial")
  expect_equal(table$link, "logit")
})

test_that("mixed model table copy and export use structured result objects", {
  skip_if_not_installed("lme4")
  school <- mixed_school_data()
  ls_register_dataset("mixed_export_school", school)
  model <- ls_new_linear_mixed_model(
    data = "mixed_export_school",
    response = "score",
    fixed = c("condition", "pretest"),
    random = list(list(group = "classroom", terms = "1")),
    native = FALSE
  )

  copied <- ls_copy_mixed_model_table(model, format = "markdown")
  expect_match(copied, "Linear Mixed Model", fixed = TRUE)
  expect_match(copied, "Random effects", fixed = TRUE)

  out <- tempfile(fileext = ".txt")
  ls_export_mixed_model_table(model, out, format = "txt")
  expect_true(file.exists(out))
  exported <- paste(readLines(out), collapse = "\n")
  expect_match(exported, "Fixed effects", fixed = TRUE)
  expect_match(exported, "Model fit", fixed = TRUE)
})

test_that("mixed model editing functions refit and update formula/version", {
  skip_if_not_installed("lme4")
  school <- mixed_school_data()
  ls_register_dataset("mixed_edit_school", school)
  model <- ls_new_linear_mixed_model(
    data = "mixed_edit_school",
    response = "score",
    fixed = "pretest",
    random = list(list(group = "classroom", terms = "1")),
    native = FALSE
  )
  version1 <- ls_mixed_model_state(model)$fit_version

  model <- ls_mixed_model_add_fixed_effect(model, "condition", native = FALSE)
  state <- ls_mixed_model_state(model)
  expect_true("condition" %in% state$fixed_effects)
  expect_gt(state$fit_version, version1)
  expect_match(state$formula, "`condition`", fixed = TRUE)

  model <- ls_mixed_model_add_random_slope(model, "classroom", "pretest", native = FALSE)
  state <- ls_mixed_model_state(model)
  expect_equal(state$random_effects[[1]]$terms, c("1", "pretest"))
  expect_match(state$formula, "1 + `pretest` | `classroom`", fixed = TRUE)

  model <- ls_mixed_model_remove_random_slope(model, "classroom", "pretest", native = FALSE)
  expect_equal(ls_mixed_model_state(model)$random_effects[[1]]$terms, "1")

  model <- ls_mixed_model_remove_fixed_effect(model, "condition", native = FALSE)
  expect_false("condition" %in% ls_mixed_model_state(model)$fixed_effects)

  model <- ls_mixed_model_set_method(model, "ML", native = FALSE)
  expect_equal(ls_mixed_model_state(model)$method, "ML")
})

test_that("mixed model random effects can be added and removed safely", {
  skip_if_not_installed("lme4")
  school <- mixed_school_data()
  ls_register_dataset("mixed_random_school", school)
  model <- ls_new_linear_mixed_model(
    data = "mixed_random_school",
    response = "score",
    fixed = "pretest",
    random = list(list(group = "classroom", terms = "1")),
    native = FALSE
  )

  model <- ls_mixed_model_add_random_effect(model, "school", terms = "1", native = FALSE)
  state <- ls_mixed_model_state(model)
  expect_equal(vapply(state$random_effects, `[[`, character(1L), "group"), c("classroom", "school"))
  expect_match(state$formula, "(1 | `school`)", fixed = TRUE)

  model <- ls_mixed_model_remove_random_effect(model, "school", native = FALSE)
  expect_equal(vapply(ls_mixed_model_state(model)$random_effects, `[[`, character(1L), "group"), "classroom")
  expect_error(ls_mixed_model_remove_random_effect(model, "classroom", native = FALSE), "at least one random effect")
})

test_that("generalized mixed model family and diagnostics update", {
  skip_if_not_installed("lme4")
  school <- mixed_school_data()
  ls_register_dataset("mixed_diag_school", school)
  model <- ls_new_generalized_mixed_model(
    data = "mixed_diag_school",
    response = "passed",
    fixed = c("condition", "pretest"),
    random = list(list(group = "classroom", terms = "1")),
    family = "binomial",
    native = FALSE
  )

  model <- ls_mixed_model_set_family(model, "binomial", "probit", native = FALSE)
  expect_equal(ls_mixed_model_state(model)$link, "probit")

  diag <- ls_mixed_model_diagnostics(model, "pearson_residuals")
  expect_true(all(c("fitted", "pearson_residual", "row_id") %in% names(diag)))
  expect_true(nrow(diag) > 0L)

  opened <- ls_mixed_model_open_diagnostic(model, "observed_vs_fitted", native = FALSE)
  expect_s3_class(opened, "rlispstat_mixed_model_diagnostic")
  expect_equal(opened$displayed_fit_version, ls_mixed_model_state(model)$fit_version)
  expect_true(all(c("fitted", "observed", "row_id") %in% names(opened$data)))
})
