test_that("dimensionality requests select an imputation independently of the data sheet", {
  base <- mtcars[, c("mpg", "disp", "hp", "wt")]
  completed <- list(base, transform(base, mpg = mpg + seq_len(nrow(base)) / 2))
  id <- ls_register_dataset("dimension-selector", base)
  dataset <- LinkEDA:::.rls_dataset_record(id)
  dataset$dataset_type <- "multiple_imputation"
  dataset$completed_datasets <- completed
  dataset$original_data <- base
  dataset$imputation_count <- 2L
  dataset$active_imputation_version <- 1L
  LinkEDA:::.rls_set_dataset_record(dataset)
  for (method in c("pca", "factor")) for (imp in c(2L, 1L)) {
    name <- paste0("dimension-selector-", method)
    task <- c("PCAFA_FACTOR_NEEDED", name, id, method, "listwise", "none", "selected",
              "TRUE", "1", "4", names(base), "24", as.character(1:24), "IMPUTATION_V1", as.character(imp))
    LinkEDA:::.rls_handle_dimensionality_needed(task)
    record <- LinkEDA:::.rls_dimension_record(name)
    expect_identical(record$active_imputation_version, imp)
    expect_equal(record$data, completed[[imp]])
    expect_identical(record$rows_used_original_ids, 1:24)
    expected <- if (method == "pca") stats::prcomp(completed[[imp]][1:24, ], scale. = TRUE)$sdev^2 else
      psych::fa(cor(completed[[imp]][1:24, ]), nfactors = 1, fm = "minres", rotate = "none")$values
    expect_equal(record$eigenvalues$eigenvalue, unname(expected), tolerance = 1e-8)
    recipe <- record$analysis_provenance$verification_r_code$table
    expect_match(recipe, paste0("displayed_imputation <- ", imp, "L"), fixed = TRUE)
    wire <- LinkEDA:::.rls_dimension_native_update_payload(record)
    marker <- match("IMPUTATION_V1", wire)
    expect_identical(wire[marker + 1:2], c(as.character(imp), "2"))
    expect_identical(LinkEDA:::.rls_dataset_record(id)$active_imputation_version, 1L)
    if (identical(Sys.getenv("LINKEDA_DIMENSION_FIXTURES"), "1"))
      writeLines(wire, paste0("/tmp/linkeda-dimension-", method, "-", imp, ".payload"))
  }
  if (identical(Sys.getenv("LINKEDA_DIMENSION_FIXTURES"), "1"))
    writeLines(c("REGISTER_DATASET", id,
      LinkEDA:::.rls_variable_payload(base, dataset$variable_metadata),
      LinkEDA:::.rls_dataframe_payload(base, dataset$variable_metadata, dataset_record = dataset)),
      "/tmp/linkeda-dimension-dataset.payload")
})
