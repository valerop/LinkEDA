test_that("linear-model scope uses selected row identities", {
  data <- data.frame(
    y = seq_len(8),
    x = seq_len(8),
    z = c(1, 2, NA, 4, 5, 6, 7, 8)
  )
  formula <- stats::reformulate(c("x", "z"), response = "y")

  selected <- LinkEDA:::.rls_model_complete_data(
    data, formula, scope = "selected", selected = c(2L, 3L, 6L)
  )
  unselected <- LinkEDA:::.rls_model_complete_data(
    data, formula, scope = "unselected", selected = c(2L, 3L, 6L)
  )

  expect_identical(selected$rows, c(2L, 6L))
  expect_identical(unselected$rows, c(1L, 4L, 5L, 7L, 8L))
  expect_equal(nrow(selected$data), 2L)
  expect_equal(nrow(unselected$data), 5L)
})

test_that("MI linear-model scope is applied independently to every imputation", {
  completed <- list(
    data.frame(y = 1:8, x = 11:18),
    data.frame(y = 1:8, x = 21:28)
  )
  id <- ls_register_dataset("mi_linear_scope", completed[[1L]])
  dataset <- LinkEDA:::.rls_dataset_record(id)
  dataset$dataset_type <- "multiple_imputation"
  dataset$completed_datasets <- completed
  dataset$imputation_count <- 2L
  dataset$original_row_ids <- 101:108
  LinkEDA:::.rls_set_dataset_record(dataset)

  record <- list(
    group = id,
    dependent = "y",
    response = "y",
    selected_rows = c(102L, 105L, 108L),
    term_types = list(),
    factor_reference_levels = list(),
    centered_predictors = character()
  )
  formula <- stats::reformulate("x", response = "y")

  selected <- LinkEDA:::.rls_mi_fit_data_by_imputation(
    record, formula, scope = "selected"
  )
  unselected <- LinkEDA:::.rls_mi_fit_data_by_imputation(
    record, formula, scope = "unselected"
  )

  expect_true(all(vapply(selected, function(x) nrow(x$data), integer(1L)) == 3L))
  expect_true(all(vapply(unselected, function(x) nrow(x$data), integer(1L)) == 5L))
  expect_true(all(vapply(selected, function(x) {
    identical(x$original_rows, c(102L, 105L, 108L))
  }, logical(1L))))
  expect_true(all(vapply(unselected, function(x) {
    identical(x$original_rows, c(101L, 103L, 104L, 106L, 107L))
  }, logical(1L))))
})

test_that("MI scope keeps exact 315/100/215 row identities", {
  completed <- list(
    data.frame(y = seq_len(315), x = seq_len(315)),
    data.frame(y = seq_len(315) + 0.25, x = seq_len(315))
  )
  id <- ls_register_dataset("mi_linear_scope_315", completed[[1L]])
  dataset <- LinkEDA:::.rls_dataset_record(id)
  dataset$dataset_type <- "multiple_imputation"
  dataset$completed_datasets <- completed
  dataset$imputation_count <- 2L
  dataset$original_row_ids <- seq_len(315)
  LinkEDA:::.rls_set_dataset_record(dataset)

  record <- list(
    group = id,
    dependent = "y",
    response = "y",
    selected_rows = seq_len(100),
    term_types = list(),
    factor_reference_levels = list(),
    centered_predictors = character()
  )
  formula <- stats::reformulate("x", response = "y")
  all_rows <- LinkEDA:::.rls_mi_fit_data_by_imputation(record, formula, "all")
  selected <- LinkEDA:::.rls_mi_fit_data_by_imputation(record, formula, "selected")
  unselected <- LinkEDA:::.rls_mi_fit_data_by_imputation(record, formula, "unselected")

  expect_true(all(vapply(all_rows, function(x) nrow(x$data), integer(1L)) == 315L))
  expect_true(all(vapply(selected, function(x) nrow(x$data), integer(1L)) == 100L))
  expect_true(all(vapply(unselected, function(x) nrow(x$data), integer(1L)) == 215L))
  expect_true(all(vapply(selected, function(x) {
    identical(x$original_rows, seq_len(100))
  }, logical(1L))))
  expect_true(all(vapply(unselected, function(x) {
    identical(x$original_rows, 101:315)
  }, logical(1L))))
})

test_that("a scoped factor without variability produces an actionable message", {
  data <- data.frame(
    y = seq_len(8),
    coastal = factor(c("yes", "yes", "no", "no", "no", "no", "no", "no")),
    x = seq_len(8)
  )
  formula <- stats::reformulate(c("coastal", "x"), response = "y")

  expect_error(
    LinkEDA:::.rls_model_complete_data(
      data, formula, scope = "unselected", selected = 3:8
    ),
    "Cannot fit the model for Unselected rows because categorical predictor `coastal` has no variability in this subset: it has only one observed category \\(`yes`\\)"
  )

  complete <- LinkEDA:::.rls_model_complete_data(
    data, formula, scope = "all", selected = integer()
  )
  expect_identical(complete$rows, seq_len(8))
})

test_that("blank factor labels are missing rather than anonymous model levels", {
  data <- data.frame(
    y = seq_len(8),
    gender = factor(
      c("Male", "", "Female", "   ", "Other/Prefer not", NA,
        "Male", "Female"),
      levels = c("Male", "", "Female", "   ", "Other/Prefer not")
    )
  )
  typed <- LinkEDA:::.rls_model_data_for_term_types(
    data, list(gender = "factor"), response = "y"
  )
  expect_identical(levels(typed$gender), c("Male", "Female", "Other/Prefer not"))
  expect_identical(which(is.na(typed$gender)), c(2L, 4L, 6L))

  complete <- LinkEDA:::.rls_model_complete_data(
    typed, y ~ gender, scope = "all", selected = integer()
  )
  fit <- stats::lm(y ~ gender, data = complete$data)
  expect_identical(complete$rows, c(1L, 3L, 5L, 7L, 8L))
  expect_identical(
    fit$xlevels$gender,
    c("Male", "Female", "Other/Prefer not")
  )
  expect_false(any(!nzchar(trimws(fit$xlevels$gender))))
  expect_false(any(names(stats::coef(fit)) %in% c("gender", "gender   ")))

  semantic <- LinkEDA:::.rls_glm_extract_fit(
    list(data = typed, predictors = "gender"), fit, complete
  )$coefficient_rows
  factor_rows <- semantic[semantic$source_term == "gender", , drop = FALSE]
  expect_identical(
    trimws(factor_rows$display_label),
    c("gender", "Male", "Female", "Other/Prefer not")
  )
  expect_false(any(!nzchar(trimws(factor_rows$display_label))))
})

test_that("MI regression normalizes blank factor labels in every imputation", {
  completed <- lapply(seq_len(3L), function(i) {
    data.frame(
      y = seq_len(9) + i / 10,
      gender = factor(
        c("Male", "", "Female", "Other/Prefer not", "Male", "   ",
          "Female", "Other/Prefer not", NA),
        levels = c("Male", "", "Female", "   ", "Other/Prefer not")
      )
    )
  })
  id <- ls_register_dataset("mi_blank_factor_labels", completed[[1L]])
  dataset <- LinkEDA:::.rls_dataset_record(id)
  dataset$dataset_type <- "multiple_imputation"
  dataset$completed_datasets <- completed
  dataset$imputation_count <- length(completed)
  dataset$original_row_ids <- seq_len(nrow(completed[[1L]]))
  LinkEDA:::.rls_set_dataset_record(dataset)

  record <- list(
    group = id,
    dependent = "y",
    response = "y",
    selected_rows = integer(),
    term_types = list(gender = "factor"),
    factor_reference_levels = list(gender = "Male"),
    centered_predictors = character()
  )
  prepared <- LinkEDA:::.rls_mi_fit_data_by_imputation(record, y ~ gender, "all")
  expect_true(all(vapply(prepared, function(x) {
    identical(levels(x$data$gender), c("Male", "Female", "Other/Prefer not"))
  }, logical(1L))))
  expect_true(all(vapply(prepared, function(x) {
    identical(x$original_rows, c(1L, 3L, 4L, 5L, 7L, 8L))
  }, logical(1L))))

  fits <- lapply(prepared, function(x) stats::lm(y ~ gender, data = x$data))
  expect_true(all(vapply(fits, function(fit) {
    identical(
      names(stats::coef(fit)),
      c("(Intercept)", "genderFemale", "genderOther/Prefer not")
    )
  }, logical(1L))))
})
