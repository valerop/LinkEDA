test_that("principal components analysis computes eigenvalues, loadings, and scores", {
  data <- mtcars[, c("mpg", "disp", "hp", "wt")]
  model <- ls_new_dimensionality(data, variables = names(data), method = "pca",
                                 n_components = 2, native = FALSE)

  eigenvalues <- ls_dimensionality_eigenvalues(model)
  loadings <- ls_dimensionality_loadings(model)
  scores <- ls_dimensionality_scores(model)

  expect_equal(nrow(eigenvalues), 4)
  expect_equal(names(eigenvalues), c("component", "eigenvalue", "parallel_eigenvalue", "variance", "cumulative"))
  expect_true(all(eigenvalues$eigenvalue >= -1e-8))
  expect_true(all(is.finite(eigenvalues$parallel_eigenvalue)))
  expect_equal(nrow(loadings), 4)
  expect_true(all(c("variable", "PC1", "PC2", "communality", "uniqueness") %in% names(loadings)))
  expect_equal(nrow(scores), nrow(data))
  expect_equal(ncol(scores), 2)
})

test_that("factor analysis returns retained factor loadings", {
  data <- mtcars[, c("mpg", "disp", "hp", "wt", "qsec")]
  model <- ls_new_dimensionality(data, variables = names(data), method = "factor",
                                 n_components = 2, native = FALSE)
  state <- ls_dimensionality_state(model)

  expect_equal(state$method, "factor")
  expect_equal(nrow(state$loadings), 5)
  expect_true(any(grepl("^(F|PC)1$", names(state$loadings))))
  expect_true(all(is.finite(state$loadings$communality)))
})

test_that("dimensionality validates numeric variables and add/remove workflow", {
  data <- data.frame(
    x = 1:8,
    y = c(2, 1, 4, 3, 6, 5, 8, 7),
    z = c(5, 4, 3, 2, 1, 2, 3, 4),
    g = factor(rep(c("a", "b"), 4))
  )

  expect_error(
    ls_new_dimensionality(data, variables = c("x", "g"), native = FALSE),
    "must be numeric"
  )

  model <- ls_new_dimensionality(data, variables = c("x", "y"), native = FALSE)
  model <- ls_dimensionality_add_variable(model, "z")
  expect_equal(ls_dimensionality_state(model)$variables, c("x", "y", "z"))

  model <- ls_dimensionality_remove_variable(model, "y")
  expect_equal(ls_dimensionality_state(model)$variables, c("x", "z"))
  expect_error(ls_dimensionality_remove_variable(model, "x"), "At least two variables")
})

test_that("rotations and parallel-analysis references are retained", {
  data <- mtcars[, c("mpg", "disp", "hp", "wt", "qsec")]
  unrotated <- ls_new_dimensionality(data, variables = names(data), method = "pca",
                                     n_components = 3, rotation = "none",
                                     parallel_iterations = 10, native = FALSE)
  rotated <- ls_new_dimensionality(data, variables = names(data), method = "pca",
                                   n_components = 3, rotation = "varimax",
                                   parallel_iterations = 10, native = FALSE)

  unrotated_state <- ls_dimensionality_state(unrotated)
  rotated_state <- ls_dimensionality_state(rotated)

  expect_equal(rotated_state$rotation, "varimax")
  expect_true(all(is.finite(rotated_state$eigenvalues$parallel_eigenvalue)))
  expect_false(isTRUE(all.equal(
    unrotated_state$loadings[, c("PC1", "PC2", "PC3")],
    rotated_state$loadings[, c("PC1", "PC2", "PC3")]
  )))
})

test_that("dimensionality scope uses selected and unselected original rows", {
  data <- mtcars[seq_len(12), c("mpg", "disp", "hp", "wt")]
  selected <- c(1L, 3L, 5L, 7L)

  selected_model <- ls_new_dimensionality(
    data, variables = names(data), method = "pca", n_components = 2,
    scope = "selected", selected_rows = selected, native = FALSE
  )
  selected_state <- ls_dimensionality_state(selected_model)

  expect_equal(selected_state$scope, "selected")
  expect_equal(selected_state$rows_used_original_ids, selected)
  expect_equal(nrow(selected_state$scores), length(selected))

  unselected_model <- ls_new_dimensionality(
    data, variables = names(data), method = "pca", n_components = 2,
    scope = "unselected", selected_rows = selected, native = FALSE
  )
  unselected_state <- ls_dimensionality_state(unselected_model)

  expect_equal(unselected_state$scope, "unselected")
  expect_equal(unselected_state$rows_used_original_ids, setdiff(seq_len(nrow(data)), selected))
  expect_equal(nrow(unselected_state$scores), nrow(data) - length(selected))
})

test_that("factor/component scores can be saved to the registered dataset", {
  dataset_id <- ls_register_dataset("dimensionality_scores_test", mtcars[, c("mpg", "disp", "hp", "wt")])
  model <- ls_new_dimensionality(dataset_id, variables = c("mpg", "disp", "hp", "wt"),
                                 n_components = 2, native = FALSE)

  names_added <- ls_dimensionality_save_scores(model, prefix = "Dim", components = 1:2)
  record <- getFromNamespace(".rls_dataset_record", "LinkEDA")(dataset_id)

  expect_equal(length(names_added), 2)
  expect_true(all(names_added %in% names(record$data)))
  expect_true(all(vapply(record$data[names_added], is.numeric, logical(1L))))
  expect_true(all(is.finite(record$data[[names_added[[1L]]]])))
})
