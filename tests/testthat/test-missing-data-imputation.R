test_that("missing-data imputation API exists and reports missing mice guidance", {
  expect_true(exists("ls_new_missing_data_imputation"))
  expect_true(exists("ls_run_imputation"))
  expect_true(exists("ls_completed_dataset"))
  expect_true(exists("ls_set_imputed_dataset_version"))
  expect_true(exists("ls_imputation_summary"))
  expect_equal(
    LinkEDA:::.rls_mice_missing_message,
    "Multiple imputation requires the mice package. Install it with install.packages(\"mice\") and try again."
  )
})

test_that("new imputation uses active dataset and builds methods and predictor matrix", {
  data <- data.frame(
    id = 1:8,
    income = c(10, 12, NA, 15, 16, NA, 20, 22),
    stress = c(1, 2, 2, NA, 3, 3, 4, 4),
    sex = factor(c("f", "m", "f", "m", NA, "m", "f", "m")),
    education = factor(c("low", "mid", "high", NA, "mid", "high", "low", "mid")),
    ordered_rating = ordered(c("low", "mid", NA, "high", "mid", "high", "low", "mid"),
                             levels = c("low", "mid", "high"))
  )
  name <- ls_register_dataset("mi_defaults", data)
  on.exit(ls_unregister_dataset(name), add = TRUE)

  imp <- ls_new_missing_data_imputation(m = 3, maxit = 1, seed = 42)
  record <- LinkEDA:::.rls_imputation_record(imp)

  expect_equal(record$source_dataset_id, name)
  expect_false("id" %in% record$impute_variables)
  expect_true(all(c("income", "stress", "sex", "education", "ordered_rating") %in% record$impute_variables))
  expect_equal(record$method_vector[["income"]], "pmm")
  expect_equal(record$method_vector[["stress"]], "pmm")
  expect_equal(record$method_vector[["sex"]], "logreg")
  expect_equal(record$method_vector[["education"]], "polyreg")
  expect_equal(record$method_vector[["ordered_rating"]], "polr")
  expect_true(all(diag(record$predictor_matrix) == 0))
  expect_true(all(record$predictor_matrix[setdiff(rownames(record$predictor_matrix), record$impute_variables), ] == 0))
  expect_false(any(record$predictor_matrix[, "id"] != 0))
})

test_that("case-label text columns are not default imputation predictors", {
  data <- data.frame(
    y = c(1, 2, NA, 4, 5, NA, 7, 8),
    x = c(4, 5, 6, 6, 8, 9, 9, 11),
    etiqueta = paste0("Caso ", seq_len(8)),
    stringsAsFactors = FALSE
  )
  name <- ls_register_dataset("mi_label_column", data)
  on.exit(ls_unregister_dataset(name), add = TRUE)

  imp <- ls_new_missing_data_imputation(data = name, impute = "y", m = 2, maxit = 1)
  record <- LinkEDA:::.rls_imputation_record(imp)

  expect_false("etiqueta" %in% record$predictor_variables)
  expect_true(record$predictor_matrix["y", "x"] == 1)
  expect_true(record$predictor_matrix["y", "etiqueta"] == 0)
})

test_that("running mice works when seed is omitted", {
  skip_if_not_installed("mice")
  data <- data.frame(
    y = c(1, NA, 3, 4, NA, 6, 7, 8),
    x = c(2, 3, 4, 5, 6, 7, 8, 9)
  )
  name <- ls_register_dataset("mi_no_seed", data)
  on.exit(ls_unregister_dataset(name), add = TRUE)

  imp <- ls_new_missing_data_imputation(
    data = name,
    impute = "y",
    predictors = "x",
    m = 2,
    maxit = 1
  )

  expect_no_error(ls_run_imputation(imp, open_data = FALSE, make_active = FALSE))
})

test_that("running mice handles haven labelled SPSS-style columns", {
  skip_if_not_installed("mice")
  skip_if_not_installed("haven")
  data <- data.frame(
    y = haven::labelled(c(1, NA, 2, 2, NA, 1, 2, 1), labels = c(Low = 1, High = 2)),
    x = c(2, 3, 4, 5, 6, 7, 8, 9)
  )
  name <- ls_register_dataset("mi_haven_labelled", data)
  on.exit(ls_unregister_dataset(name), add = TRUE)

  imp <- ls_new_missing_data_imputation(
    data = name,
    impute = "y",
    predictors = "x",
    m = 2,
    maxit = 1
  )

  expect_no_error(ls_run_imputation(imp, open_data = FALSE, make_active = FALSE))
})

test_that("running mice stores mids, completed datasets, row ids, and imputed cell masks", {
  skip_if_not_installed("mice")
  data <- mtcars[1:12, c("mpg", "hp", "wt", "qsec", "cyl")]
  data$mpg[c(1, 3)] <- NA
  data$hp[c(2, 4)] <- NA
  data$cyl <- factor(data$cyl)
  name <- ls_register_dataset("mi_run", data)
  on.exit(ls_unregister_dataset(name), add = TRUE)

  imp <- ls_new_missing_data_imputation(
    data = name,
    impute = c("mpg", "hp"),
    predictors = c("wt", "qsec", "cyl", "mpg", "hp"),
    m = 2,
    maxit = 1,
    seed = 123
  )
  imp <- ls_run_imputation(imp, open_data = FALSE, make_active = TRUE)
  record <- LinkEDA:::.rls_imputation_record(imp)

  expect_s3_class(record$mids_object, "mids")
  expect_length(record$completed_datasets, 2)
  expect_equal(record$original_row_ids, seq_len(nrow(data)))
  expect_true(record$missing_cell_mask$mpg[[1L]])
  expect_true(record$missing_cell_mask$hp[[2L]])
  expect_false(record$missing_cell_mask$wt[[1L]])
  expect_true("mpg" %in% names(record$imputed_cell_map))
  expect_true(record$dataset_id %in% ls_datasets()$dataset_id)

  dataset_record <- LinkEDA:::.rls_dataset_record(record$dataset_id)
  expect_equal(dataset_record$dataset_type, "multiple_imputation")
  expect_equal(dataset_record$original_row_ids, seq_len(nrow(data)))
  expect_equal(dataset_record$source_dataset_id, name)
  expect_equal(nrow(ls_completed_dataset(imp, 1)), nrow(data))
  expect_equal(nrow(ls_completed_dataset(imp, 2)), nrow(data))
  expect_true(is.na(LinkEDA:::.rls_dataset_record(name)$data$mpg[[1L]]))
})

test_that("imputed data sheet state switches version, original, and combined display", {
  skip_if_not_installed("mice")
  data <- data.frame(
    y = c(1, NA, 3, 4, NA, 6),
    x = c(2, 3, 4, 5, 6, 7),
    g = factor(c("a", "a", "b", "b", "a", "b"))
  )
  name <- ls_register_dataset("mi_display", data)
  on.exit(ls_unregister_dataset(name), add = TRUE)

  imp <- ls_new_missing_data_imputation(
    data = name,
    impute = "y",
    predictors = c("x", "g"),
    m = 2,
    maxit = 1,
    seed = 321
  )
  imp <- ls_run_imputation(imp, open_data = FALSE, make_active = FALSE)
  record <- LinkEDA:::.rls_imputation_record(imp)
  dataset_record <- LinkEDA:::.rls_dataset_record(record$dataset_id)

  expect_equal(LinkEDA:::.rls_imputed_cell_display(dataset_record, 1, "y"), "1")
  expect_true(grepl("^[0-9.]+$", LinkEDA:::.rls_imputed_cell_display(dataset_record, 2, "y")))

  ls_set_imputed_dataset_version(imp, "all")
  record <- LinkEDA:::.rls_imputation_record(imp)
  dataset_record <- LinkEDA:::.rls_dataset_record(record$dataset_id)
  expect_equal(dataset_record$imputation_display_mode, "all")
  expect_match(LinkEDA:::.rls_imputed_cell_display(dataset_record, 2, "y"), " \\| ")
  expect_equal(LinkEDA:::.rls_imputed_cell_display(dataset_record, 1, "y"), "1")

  ls_set_imputed_dataset_version(imp, "original")
  record <- LinkEDA:::.rls_imputation_record(imp)
  dataset_record <- LinkEDA:::.rls_dataset_record(record$dataset_id)
  expect_equal(LinkEDA:::.rls_imputed_cell_display(dataset_record, 2, "y"), "NA")

  ls_set_imputed_dataset_version(imp, 2)
  record <- LinkEDA:::.rls_imputation_record(imp)
  expect_equal(record$active_version, 2L)
  expect_equal(record$display_mode, "version")
  cm <- expect_no_warning(
    ls_new_correlation_matrix(data = record$dataset_id, variables = c("x", "y"), native = FALSE)
  )
  cm_state <- ls_correlation_matrix_state(cm)
  xy <- which(cm_state$results$x_variable == "x" & cm_state$results$y_variable == "y")[[1L]]
  expect_equal(cm_state$analysis_backend, "multiple_imputation")
  expect_length(cm_state$results$r_by_imputation[[xy]], 2L)
})

.make_test_mi_dataset <- function(name, original, completed) {
  id <- ls_register_dataset(name, completed[[1L]])
  record <- LinkEDA:::.rls_dataset_record(id)
  record$dataset_type <- "multiple_imputation"
  record$imputation_id <- paste0(name, "_imp")
  record$source_dataset_id <- paste0(name, "_source")
  record$original_data <- original
  record$completed_datasets <- completed
  record$missing_cell_mask <- as.data.frame(lapply(original, is.na), stringsAsFactors = FALSE)
  record$imputed_cell_map <- list()
  record$active_imputation_version <- 1L
  record$imputation_display_mode <- "version"
  record$imputation_count <- length(completed)
  record$original_row_ids <- seq_len(nrow(original))
  LinkEDA:::.rls_set_dataset_record(record)
  id
}

test_that("analysis dispatcher detects multiple-imputation datasets", {
  original <- data.frame(y = c(1, NA, 3, 4), x = c(1, 2, 3, 4))
  completed <- list(
    data.frame(y = c(1, 2, 3, 4), x = c(1, 2, 3, 4)),
    data.frame(y = c(1, 2.5, 3, 4), x = c(1, 2, 3, 4))
  )
  id <- .make_test_mi_dataset("mi_backend", original, completed)
  expect_equal(ls_analysis_backend(id, "table1")$backend, "multiple_imputation")

  ordinary <- ls_register_dataset("ordinary_backend", completed[[1L]])
  expect_equal(ls_analysis_backend(ordinary, "table1")$backend, "ordinary")
})

test_that("MI Table 1 pools numeric summaries and avoids unsupported averaged p-values", {
  original <- data.frame(
    y = c(2.1, NA, 3.8, 4.2, 5.1, NA, 6.0, 6.4),
    x = c(1, 2, 3, 4, 5, 6, 7, 8),
    g = factor(rep(c("a", "b"), each = 4)),
    cat = factor(c("no", "no", "yes", NA, "yes", "yes", "no", "no"))
  )
  completed <- list(
    transform(original, y = c(2.1, 2.7, 3.8, 4.2, 5.1, 5.7, 6.0, 6.4),
              cat = factor(c("no", "no", "yes", "yes", "yes", "yes", "no", "no"))),
    transform(original, y = c(2.1, 3.0, 3.8, 4.2, 5.1, 5.9, 6.0, 6.4),
              cat = factor(c("no", "no", "yes", "no", "yes", "yes", "no", "no")))
  )
  id <- .make_test_mi_dataset("mi_table1", original, completed)
  table <- ls_new_table1(id, variables = c("y", "cat"), group = "g")
  record <- LinkEDA:::.rls_table1_record(table)
  display <- ls_table1_table(table)

  expect_equal(record$analysis_backend, "multiple_imputation")
  expect_equal(record$multiple_imputation$m, 2L)
  expect_match(display$Overall[display$Variable == "y"][[1L]], "SE")
  expect_equal(display$Test[display$Variable == "y"][[1L]], "MI pooled t")
  expect_equal(display$Test[display$Variable == "cat"][[1L]], "MI categorical test unavailable")
  expect_true(any(display$Variable == "  Missing in original"))
  expect_true(is.finite(record$test_results$y$p))
})

test_that("MI Table 1 uses a pooled omnibus test for numeric groups with more than two levels", {
  original <- data.frame(
    y = c(2.1, NA, 2.8, 3.2, 5.1, 5.4, NA, 6.0, 8.1, 8.5, 8.9, NA),
    g = factor(rep(c("a", "b", "c"), each = 4))
  )
  completed <- list(
    transform(original, y = c(2.1, 2.5, 2.8, 3.2, 5.1, 5.4, 5.7, 6.0, 8.1, 8.5, 8.9, 9.2)),
    transform(original, y = c(2.1, 2.7, 2.8, 3.2, 5.1, 5.4, 5.9, 6.0, 8.1, 8.5, 8.9, 9.4))
  )
  id <- .make_test_mi_dataset("mi_table1_omnibus", original, completed)
  table <- ls_new_table1(id, variables = "y", group = "g")
  record <- LinkEDA:::.rls_table1_record(table)

  expect_equal(record$test_results$y$test, "MI pooled omnibus (D1)")
  expect_equal(record$test_results$y$status, "ok")
  expect_true(is.finite(record$test_results$y$statistic))
  expect_true(is.finite(record$test_results$y$p))
  expect_match(record$test_results$y$detail, "lm\\(y ~ group\\)")
})

test_that("MI correlations pool on Fisher z instead of averaging raw r", {
  original <- data.frame(
    x = c(1, 2, 3, 4, 5, NA, 7, 8),
    y = c(2, 3, 5, 6, NA, 8, 9, 12)
  )
  completed <- list(
    data.frame(x = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(2, 3, 5, 6, 7, 8, 9, 12)),
    data.frame(x = c(1, 2, 3, 4, 5, 6.5, 7, 8), y = c(2.2, 2.8, 5.4, 5.8, 7.6, 8.2, 8.7, 11.5))
  )
  id <- .make_test_mi_dataset("mi_corr", original, completed)
  cm <- ls_new_correlation_matrix(id, variables = c("x", "y"), native = FALSE)
  cells <- ls_correlation_matrix_cells(cm)
  xy <- cells[cells$x_variable == "x" & cells$y_variable == "y", ]
  r_by_imp <- unlist(xy$r_by_imputation[[1L]])
  expected <- tanh(mean(atanh(r_by_imp)))

  expect_equal(ls_correlation_matrix_state(cm)$analysis_backend, "multiple_imputation")
  expect_equal(xy$r, expected, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(xy$r, mean(r_by_imp), tolerance = 1e-12)))
  expect_equal(xy$pooling_scale, "Fisher z")
  expect_true(is.finite(xy$pooled_SE))

  payload <- LinkEDA:::.rls_correlation_native_payload(ls_correlation_matrix_state(cm))
  expect_equal(payload[[1L]], "CORR_OPEN_STRUCTURED")
  expect_true("Pearson Correlation Matrix - Multiple Imputation" %in% payload)
  expect_true(any(grepl("Fisher z", payload, fixed = TRUE)))
  expect_true(any(grepl("FMI", payload, fixed = TRUE)))
})

test_that("MI linear and generalized models fit every imputation and store Rubin components", {
  original <- data.frame(
    y = c(2.1, NA, 4.9, 6.2, 8.1, NA, 11.8, 13.0),
    x = c(0.5, 1.0, 1.8, 2.5, 3.1, 3.8, 4.5, 5.2),
    z = c(0, 1, 0, 1, 0, 1, 0, 1)
  )
  completed <- list(
    transform(original, y = c(2.1, 3.2, 4.9, 6.2, 8.1, 9.7, 11.8, 13.0)),
    transform(original, y = c(2.1, 3.5, 4.9, 6.2, 8.1, 10.2, 11.8, 13.0))
  )
  id <- .make_test_mi_dataset("mi_models", original, completed)

  m <- ls_new_glm(id)
  ls_glm_set_dependent(m, "y")
  ls_glm_add_predictor(m, "x")
  m <- ls_glm_fit(m)
  state <- LinkEDA:::.rls_glm_model_record(m)
  coefs <- ls_glm_coefficients(m)
  xrow <- coefs[coefs$term == "x", ]
  expect_equal(state$analysis_backend, "multiple_imputation")
  expect_length(state$fits_by_imputation, 2L)
  expect_true(is.finite(xrow$between_imputation_variance))
  expect_true(is.finite(xrow$total_variance))
  expect_true(is.finite(xrow$fraction_missing_information))
  expect_true(is.finite(state$summary$global_f))
  expect_true(is.finite(state$summary$global_p))
  expect_match(state$summary$fit_information_method, "D1 pooled Wald")
  linear_payload <- LinkEDA:::.rls_glm_pooled_native_payload(state)
  expect_equal(linear_payload[[1L]], "MODEL_OPEN_POOLED")
  expect_true("General Linear Model - Multiple Imputation" %in% linear_payload)
  expect_true(any(grepl("Rubin", linear_payload, fixed = TRUE)))
  expect_true(any(grepl("D1 pooled Wald", linear_payload, fixed = TRUE)))

  original_factor <- data.frame(
    y = c(2.1, NA, 4.9, 6.2, 8.1, NA, 11.8, 13.0),
    g = factor(c("A", "B", "A", "B", "A", "B", "A", "B"))
  )
  completed_factor <- list(
    transform(original_factor, y = c(2.1, 3.2, 4.9, 6.2, 8.1, 9.7, 11.8, 13.0)),
    transform(original_factor, y = c(2.1, 3.5, 4.9, 6.2, 8.1, 10.2, 11.8, 13.0))
  )
  factor_id <- .make_test_mi_dataset("mi_models_factor", original_factor, completed_factor)
  factor_model <- ls_new_glm(factor_id)
  ls_glm_set_dependent(factor_model, "y")
  ls_glm_add_predictor(factor_model, "g")
  factor_model <- ls_glm_fit(factor_model)
  factor_state <- LinkEDA:::.rls_glm_model_record(factor_model)
  factor_payload <- LinkEDA:::.rls_glm_pooled_native_payload(factor_state)
  expect_equal(factor_state$analysis_backend, "multiple_imputation")
  expect_true(any(grepl("Rubin", factor_payload, fixed = TRUE)))
  expect_true(any(grepl("g", factor_payload, fixed = TRUE)))

  g <- ls_new_generalized_linear_model(
    data = id,
    response = "z",
    terms = "x",
    family = "binomial",
    native = FALSE
  )
  gstate <- ls_generalized_linear_model_state(g)
  expect_equal(gstate$analysis_backend, "multiple_imputation")
  expect_length(gstate$fits_by_imputation, 2L)
  expect_true("odds_ratio" %in% names(ls_generalized_linear_model_coefficients(g)))
  generalized_payload <- LinkEDA:::.rls_generalized_glm_pooled_native_payload(gstate)
  expect_equal(generalized_payload[[1L]], "GENERALIZED_GLM_OPEN_POOLED")
  expect_true("Generalized Linear Model - Multiple Imputation" %in% generalized_payload)
  expect_true(any(grepl("link-scale coefficients", generalized_payload, fixed = TRUE)))
})

test_that("MI regression comparison pools coefficients and uses sequential nested tests", {
  original <- data.frame(
    y = c(2.1, NA, 4.9, 6.2, 8.1, NA, 11.8, 13.0),
    x = c(0.5, 1.0, 1.8, 2.5, 3.1, 3.8, 4.5, 5.2),
    w = c(1, 0, 1, 0, 1, 0, 1, 0)
  )
  completed <- list(
    transform(original, y = c(2.1, 3.2, 4.9, 6.2, 8.1, 9.7, 11.8, 13.0)),
    transform(original, y = c(2.1, 3.5, 4.9, 6.2, 8.1, 10.2, 11.8, 13.0))
  )
  id <- .make_test_mi_dataset("mi_regcmp", original, completed)
  cmp <- ls_new_regression_comparison(
    id,
    response = "y",
    models = list(Base = "x", Full = c("x", "w")),
    native = FALSE
  )
  state <- ls_regression_comparison_state(cmp)
  expect_equal(state$analysis_backend, "multiple_imputation")
  expect_true(all(vapply(state$models, function(model) length(model$fits_by_imputation) == 2L, logical(1L))))
  expect_equal(state$model_comparison_tests$status, "ok")
  expect_equal(state$model_comparison_tests$comparisons[[1L]]$status, "not_applicable")
  expect_equal(state$model_comparison_tests$comparisons[[2L]]$status, "ok")
  expect_match(state$model_comparison_tests$comparisons[[2L]]$method, "D1 pooled Wald")
  expect_true(is.finite(state$model_comparison_tests$comparisons[[2L]]$F))
  expect_true(is.finite(state$model_comparison_tests$comparisons[[2L]]$p))
  expect_equal(
    state$models[[2L]]$summary$comparison_p,
    state$model_comparison_tests$comparisons[[2L]]$p
  )
  expect_match(state$model_comparison_tests$detail, "No F statistics or p-values are averaged")
  payload <- LinkEDA:::.rls_regcmp_pooled_native_payload(state)
  expect_equal(payload[[1L]], "REGCMP_OPEN_POOLED")
  expect_true("Regression Model Comparison - Multiple Imputation" %in% payload)
  expect_true(any(grepl("D1 pooled Wald", payload, fixed = TRUE)))
})

test_that("multiple-imputation dataframe payload sends imputed cells sparsely", {
  current <- data.frame(
    y = c(1, 11, 3, 21),
    x = c(2, 3, 4, 5),
    z = c("a", "b", "c", "d"),
    stringsAsFactors = FALSE
  )
  original <- current
  original$y[c(2, 4)] <- NA
  completed <- list(
    current,
    transform(current, y = c(1, 12, 3, 22))
  )
  mask <- as.data.frame(lapply(original, is.na), stringsAsFactors = FALSE)
  record <- list(
    dataset_type = "multiple_imputation",
    imputation_id = "mi_sparse",
    source_dataset_id = "source",
    completed_datasets = completed,
    original_data = original,
    missing_cell_mask = mask,
    active_imputation_version = 1L,
    imputation_display_mode = "version"
  )

  payload <- LinkEDA:::.rls_dataframe_payload(current, dataset_record = record)
  marker <- match("IMPUTATION_SPARSE", payload)
  expect_false(is.na(marker))
  expect_equal(payload[[marker + 7L]], "1")

  imputation_section <- payload[(marker + 8L):length(payload)]
  expect_equal(imputation_section[[1L]], "y")
  expect_equal(imputation_section[[2L]], "2")
  expect_equal(imputation_section[3:4], c("2", "4"))
  expect_false(any(imputation_section == "x"))
  expect_false(any(imputation_section == "z"))
})

test_that("imputation summary exposes methods, missing counts, and pooling hooks", {
  data <- data.frame(y = c(1, NA, 3), x = c(2, 3, 4))
  name <- ls_register_dataset("mi_summary", data)
  on.exit(ls_unregister_dataset(name), add = TRUE)
  imp <- ls_new_missing_data_imputation(data = name, impute = "y", predictors = "x", m = 2, maxit = 1)
  summary <- ls_imputation_summary(imp)
  record <- LinkEDA:::.rls_imputation_record(imp)

  expect_s3_class(summary, "rlispstat_imputation_summary")
  expect_equal(summary$missing$missing, 1L)
  expect_equal(summary$missing$method, "pmm")
  expect_true(record$pooling$rubin_rules_ready)
  expect_true("missing_cell_mask" %in% names(record$plot_metadata))
})
