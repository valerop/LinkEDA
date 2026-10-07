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

  state <- ls_dimensionality_state(model)
  recipe <- state$analysis_provenance$verification_r_code$table
  expect_match(recipe, "stats::prcomp", fixed = TRUE)
  expect_match(recipe, "knitr::kable", fixed = TRUE)
  expect_false(grepl("LinkEDA:::", recipe, fixed = TRUE))
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

test_that("dimensionality validates item types and add/remove workflow", {
  data <- data.frame(
    x = 1:8,
    y = c(2, 1, 4, 3, 6, 5, 8, 7),
    z = c(5, 4, 3, 2, 1, 2, 3, 4),
    g = factor(rep(c("a", "b", "c", "a"), 2))
  )

  expect_error(
    ls_new_dimensionality(data, variables = c("x", "g"), native = FALSE),
    "Explicitly mark it as Ordinal"
  )

  model <- ls_new_dimensionality(data, variables = c("x", "y"), native = FALSE)
  model <- ls_dimensionality_add_variable(model, "z")
  expect_equal(ls_dimensionality_state(model)$variables, c("x", "y", "z"))

  model <- ls_dimensionality_remove_variable(model, "y")
  expect_equal(ls_dimensionality_state(model)$variables, c("x", "z"))
  model <- ls_dimensionality_remove_variable(model, "x")
  expect_equal(ls_dimensionality_state(model)$variables, "z")
  expect_equal(ls_dimensionality_state(model)$status, "Choose at least two numeric, ordinal, or binary variables.")
  expect_equal(nrow(ls_dimensionality_loadings(model)), 0L)
  model <- ls_dimensionality_remove_variable(model, "z")
  expect_length(ls_dimensionality_state(model)$variables, 0L)
  expect_equal(nrow(ls_dimensionality_eigenvalues(model)), 0L)
})

test_that("dimensionality starts blank and preserves partial manual selections", {
  data <- mtcars[, c("mpg", "disp", "hp")]
  model <- ls_new_dimensionality(data, native = FALSE)
  state <- ls_dimensionality_state(model)

  expect_length(state$variables, 0L)
  expect_equal(state$status, "Choose at least two numeric, ordinal, or binary variables.")
  expect_equal(nrow(state$loadings), 0L)

  model <- ls_dimensionality_add_variable(model, "mpg")
  expect_equal(ls_dimensionality_state(model)$variables, "mpg")
  expect_equal(nrow(ls_dimensionality_loadings(model)), 0L)

  model <- ls_dimensionality_add_variable(model, "disp")
  expect_equal(ls_dimensionality_state(model)$variables, c("mpg", "disp"))
  expect_gt(nrow(ls_dimensionality_loadings(model)), 0L)
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

  expect_equal(unselected_state$scope, "selected") # execution uses explicit included rows
  expect_equal(unselected_state$data_scope$rows, setdiff(seq_len(nrow(data)), selected))
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
  expect_identical(record$data_version, 2L)
  score_step <- tail(record$data_provenance$history, 1L)[[1L]]
  expect_identical(score_step$input_columns, c("mpg", "disp", "hp", "wt"))
  expect_identical(score_step$output_columns, names_added)
  expect_identical(length(score_step$stable_row_ids), nrow(mtcars))
  expect_match(score_step$r_code, "LinkEDA::ls_dimensionality_save_scores", fixed = TRUE)

  column_count <- ncol(record$data)
  names_reused <- ls_dimensionality_save_scores(model, prefix = "Dim", components = 1:2)
  record <- getFromNamespace(".rls_dataset_record", "LinkEDA")(dataset_id)
  expect_identical(names_reused, names_added)
  expect_equal(ncol(record$data), column_count)

  record$derived_score_columns <- list()
  getFromNamespace(".rls_set_dataset_record", "LinkEDA")(record)
  # A matching name alone cannot establish ownership after cache loss.
  expect_identical(
    ls_dimensionality_save_scores(model, prefix = "Dim", components = 1:2),
    paste0(names_added, "_1")
  )
  after <- getFromNamespace(".rls_dataset_record", "LinkEDA")(dataset_id)$data
  expect_equal(ncol(after), column_count + length(names_added))
  expect_identical(after[names_added], record$data[names_added])
})

test_that("MI dimensionality scores are saved per imputation without duplicate names", {
  set.seed(9321)
  base <- mtcars[, c("mpg", "disp", "hp", "wt")]
  completed <- list(base, transform(base, mpg = mpg + seq_len(nrow(base)) / 10))
  dataset_id <- ls_register_dataset("mi_dimensionality_scores_test", base)
  record <- getFromNamespace(".rls_dataset_record", "LinkEDA")(dataset_id)
  marker <- structure(list(call = quote(mice::mice(base))), class = "mids_marker")
  record$dataset_type <- "multiple_imputation"
  record$original_data <- base
  record$completed_datasets <- completed
  record$imputation_count <- 2L
  record$active_imputation_version <- 1L
  record$missing_cell_mask <- lapply(base, is.na)
  record$mids_object <- marker
  # RDS/mids imports retain the source variable names as provenance metadata.
  # Newly computed scores must be appended without requiring that extra column
  # to exist in the generated metadata row.
  record$metadata$original_name <- record$metadata$name
  record$variable_metadata$original_name <- record$variable_metadata$name
  # Keep the hand-built MI source snapshot consistent with the fixture.
  LinkEDA:::.rls_store_data_version(record)
  LinkEDA:::.rls_set_dataset_record(record)

  model <- ls_new_dimensionality(
    dataset_id, variables = names(base), method = "pca",
    n_components = 2L, parallel = FALSE, native = FALSE
  )
  model_state <- ls_dimensionality_state(model)
  expect_identical(model_state$analysis_backend, "multiple_imputation")
  expect_identical(model_state$imputation_count, 2L)
  expect_identical(model_state$active_imputation_version, 1L)
  mi_recipe <- model_state$analysis_provenance$verification_r_code$table
  expect_match(mi_recipe, "displayed_imputation <- 1L", fixed = TRUE)
  expect_match(mi_recipe, "completed_sets", fixed = TRUE)
  expect_true(any(grepl(
    "not Rubin-pooled",
    model_state$analysis_provenance$verification_warnings,
    fixed = TRUE
  )))
  names_added <- ls_dimensionality_save_scores(model, components = 1:2)
  saved <- getFromNamespace(".rls_dataset_record", "LinkEDA")(dataset_id)

  expect_identical(names_added, c("PC1_score", "PC2_score"))
  expect_true(all(vapply(saved$completed_datasets, function(data) {
    all(names_added %in% names(data)) && all(is.finite(data[[names_added[[1L]]]]))
  }, logical(1L))))
  expect_false(isTRUE(all.equal(
    saved$completed_datasets[[1L]][[names_added[[1L]]]],
    saved$completed_datasets[[2L]][[names_added[[1L]]]]
  )))
  expect_true(all(is.na(saved$original_data[[names_added[[1L]]]])))
  expect_identical(saved$mids_object, marker)
  expect_true("original_name" %in% names(saved$variable_metadata))
  expect_equal(nrow(saved$variable_metadata), ncol(saved$data))
  expect_identical(saved$data_version, 2L)
  expect_identical(tail(saved$data_provenance$history, 1L)[[1L]]$output_columns,
                   names_added)

  before <- ncol(saved$data)
  expect_identical(
    ls_dimensionality_save_scores(model, components = 1:2), names_added
  )
  saved_again <- getFromNamespace(".rls_dataset_record", "LinkEDA")(dataset_id)
  expect_equal(ncol(saved_again$data), before)
})

test_that("Descriptives and Scale Analysis share typed correlations and extraction", {
  skip_if_not_installed("psych")
  set.seed(921)
  latent <- rnorm(500)
  data <- data.frame(
    numeric_item = latent + rnorm(500),
    ordinal_item = ordered(cut(latent + rnorm(500), breaks = c(-Inf, -.5, .5, Inf), labels = c("Low", "Medium", "High"))),
    binary_item = factor(ifelse(latent + rnorm(500) > 0, "Yes", "No")),
    second_numeric = latent + rnorm(500))
  data$ordinal_item[1:10] <- NA
  data$numeric_item[11:20] <- NA
  for (missing in c("listwise", "pairwise")) {
    set.seed(522)
    computed <- LinkEDA:::.rls_dimension_compute(data, names(data), "factor", 1,
      missing = missing, rotation = "varimax", extraction = "ml", parallel_iterations = 3)
    specs <- LinkEDA:::.rls_scale_item_specifications(data, names(data))
    prepared <- LinkEDA:::.rls_scale_prepare_items(data, specs)
    correlations <- LinkEDA:::.rls_scale_correlations_one(prepared, specs, missing = missing)
    set.seed(522)
    scale_fit <- LinkEDA:::.rls_scale_dimensionality_one(prepared, specs, correlations,
      list(enabled = TRUE, factors = 1, method = "factor", scale = TRUE,
        missing = missing, rotation = "varimax", extraction = "ml", parallel_iterations = 3))
    expect_identical(computed$correlation_method, "mixed")
    expect_equal(computed$loadings$F1, scale_fit$loadings[[2]])
    expect_equal(computed$loadings$communality, scale_fit$loadings$h2)
    expect_equal(computed$scores$F1, scale_fit$scores[[2]])
    expect_equal(computed$score_rows_original_ids, 21:500)
    expect_equal(computed$rows_used_original_ids, if (missing == "listwise") 21:500 else 1:500)
    direct_r <- psych::mixedCor(as.data.frame(lapply(prepared, as.numeric)),
      c = c(1, 4), p = c(2, 3), use = if (missing == "listwise") "complete" else "pairwise", smooth = FALSE)$rho
    # Listwise is enforced before mixedCor, as in Scale Analysis.
    if (missing == "listwise") direct_r <- psych::mixedCor(
      as.data.frame(lapply(prepared[stats::complete.cases(prepared), ], as.numeric)),
      c = c(1, 4), p = c(2, 3), use = "complete", smooth = FALSE)$rho
    expect_equal(unname(correlations$matrix), unname(direct_r))
    direct_fit <- psych::fa(correlations$matrix, nfactors = 1, fm = "ml", rotate = "varimax")
    expect_equal(computed$loadings$F1, as.numeric(direct_fit$loadings))
  }
  model <- ls_new_dimensionality(data, variables = names(data), method = "factor",
    n_components = 1, missing = "pairwise", parallel_iterations = 3, native = FALSE)
  state <- ls_dimensionality_state(model)
  recipe <- state$analysis_provenance$verification_r_code$table
  expect_match(recipe, "psych::mixedCor", fixed = TRUE)
  expect_match(recipe, "psych::fa(", fixed = TRUE)
  expect_false(grepl("LinkEDA:::", recipe, fixed = TRUE))
  path <- tempfile(fileext = ".rds"); saveRDS(data, path)
  environment <- new.env(parent = globalenv()); environment$verification_data_path <- path
  capture.output(eval(parse(text = recipe), envir = environment))
  expect_equal(as.numeric(environment$reference_result$loadings[[2]]), state$loadings$F1)
  if (identical(Sys.getenv("LINKEDA_DIMENSION_FIXTURES"), "1")) {
    dataset <- LinkEDA:::.rls_dataset_record(state$group)
    writeLines(c("REGISTER_DATASET", state$group,
      LinkEDA:::.rls_variable_payload(dataset$data, dataset$variable_metadata),
      LinkEDA:::.rls_dataframe_payload(dataset$data, dataset$variable_metadata, dataset_record = dataset)),
      "/tmp/linkeda-dimension-typed-dataset.payload")
    writeLines(LinkEDA:::.rls_dimension_native_update_payload(state), "/tmp/linkeda-dimension-typed-result.payload")
  }
  expect_match(state$calculation_method, "mixed; pairwise", fixed = TRUE)
  expect_true("CALCULATION_V1" %in% LinkEDA:::.rls_dimension_native_update_payload(state))
  saved <- ls_dimensionality_save_scores(model, prefix = "typed_F")
  saved_values <- LinkEDA:::.rls_dataset_record(state$group)$data[[saved[[1L]]]]
  expect_true(all(is.na(saved_values[1:20])))
  expect_equal(saved_values[21:500], state$scores$F1)
})

test_that("binary and ordinal analysis keeps category order and source case IDs", {
  skip_if_not_installed("psych")
  set.seed(57)
  z <- rnorm(350)
  data <- data.frame(a = ordered(ifelse(z + rnorm(350) > 0, "Yes", "No"), levels = c("Yes", "No")),
    b = ordered(cut(z + rnorm(350), c(-Inf, -.8, .8, Inf))),
    c = factor(ifelse(z + rnorm(350) > .1, "High", "Low")))
  result <- LinkEDA:::.rls_dimension_compute(data, names(data), "factor", 1,
    scope = "selected", selected_rows = 51:350, parallel_iterations = 3)
  expect_identical(result$correlation_method, "polychoric")
  expect_equal(result$rows_used_original_ids, 51:350)
  expect_true(all(is.finite(result$loadings$F1)))
  expect_true(all(is.finite(result$scores$F1)))
  specs <- LinkEDA:::.rls_scale_item_specifications(data, names(data))
  prepared <- LinkEDA:::.rls_scale_prepare_items(data, specs)
  expect_identical(levels(prepared$a), c("Yes", "No"))
})
