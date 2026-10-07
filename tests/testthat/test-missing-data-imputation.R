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

test_that("existing mice objects import as pooled LinkEDA datasets", {
  skip_if_not_installed("mice")
  source <- mice::nhanes
  names(source) <- c("Age Years", "Body Mass", "Hypertension", "Cholesterol")
  missing_row <- which(is.na(source[["Body Mass"]]))[[1L]]
  mids <- mice::mice(source, m = 2, maxit = 2, seed = 2026, printFlag = FALSE)
  expected_completed <- mice::complete(mids, action = "all")
  path <- tempfile(fileext = ".rds")
  saveRDS(mids, path)

  imported <- ls_import_rds(path, name = "external mice", make_active = FALSE)
  on.exit(ls_unregister_dataset(imported), add = TRUE)
  dataset <- LinkEDA:::.rls_dataset_record(imported)
  imputation <- LinkEDA:::.rls_imputation_record(dataset$imputation_id)
  on.exit(rm(list = imputation$id, envir = LinkEDA:::.rls_state$missing_imputations),
          add = TRUE)

  expect_equal(dataset$dataset_type, "multiple_imputation")
  expect_true(dataset$imported_mids)
  expect_s3_class(dataset$mids_object, "mids")
  expect_equal(dataset$imputation_count, 2L)
  expect_length(dataset$completed_datasets, 2L)
  expect_identical(dataset$original_row_ids, seq_len(nrow(source)))
  expect_identical(dataset$original_row_names, row.names(source))
  expect_identical(
    names(dataset$data),
    c("age_years", "body_mass", "hypertension", "cholesterol")
  )
  expect_true(is.na(dataset$original_data$body_mass[[missing_row]]))
  expect_true(dataset$missing_cell_mask$body_mass[[missing_row]])
  expect_false(anyNA(dataset$completed_datasets[[1L]]$body_mass))
  names(expected_completed) <- NULL
  expect_true(all(vapply(seq_along(expected_completed), function(version) {
    expected <- expected_completed[[version]]
    names(expected) <- names(dataset$completed_datasets[[version]])
    observed <- dataset$completed_datasets[[version]]
    attr(observed, "linkeda_row_ids") <- NULL
    identical(observed, expected)
  }, logical(1L))))
  expect_true(all(vapply(dataset$completed_datasets, function(data) {
    identical(attr(data, "linkeda_row_ids"), dataset$stable_row_ids)
  }, logical(1L))))
  expect_identical(names(imputation$method_vector), names(dataset$data))
  expect_identical(colnames(imputation$predictor_matrix), names(dataset$data))
  expect_equal(ls_analysis_backend(imported, "correlation")$backend,
               "multiple_imputation")

  correlation <- ls_new_correlation_matrix(
    imported, variables = c("body_mass", "cholesterol"), native = FALSE
  )
  state <- ls_correlation_matrix_state(correlation)
  expect_equal(state$analysis_backend, "multiple_imputation")
  expect_equal(state$multiple_imputation$m, 2L)
})

test_that("variable type changes update every imputation and rebuild a usable mids object", {
  skip_if_not_installed("mice")
  source <- data.frame(
    y = c(1, NA, 3, 4, NA, 6, 7, 8),
    group = factor(rep(c("Control", "Treatment"), 4),
                   levels = c("Control", "Treatment")),
    z = seq_len(8)
  )
  mids <- mice::mice(source, m = 2, maxit = 1, seed = 902, printFlag = FALSE)
  imported <- LinkEDA:::.rls_register_mids_dataset(
    mids, name = "mi type propagation", make_active = FALSE
  )
  on.exit(ls_unregister_dataset(imported), add = TRUE)

  expect_warning(ls_set_variable_type(imported, "group", "numeric"),
                 "Control = 0; Treatment = 1")
  record <- LinkEDA:::.rls_dataset_record(imported)
  expect_true(is.numeric(record$original_data$group))
  expect_true(all(vapply(record$completed_datasets,
                         function(frame) is.numeric(frame$group), logical(1L))))
  expect_true(all(vapply(record$completed_datasets, function(frame) {
    setequal(unique(frame$group), c(0, 1))
  }, logical(1L))))
  expect_s3_class(record$mids_object, "mids")
  expect_true(is.numeric(record$mids_object$data$group))
  expect_true(is.data.frame(attr(record$mids_object$data, "linkeda_variable_metadata")))
  expect_true(isTRUE(attr(record$mids_object, "linkeda_type_change_rebuilt")))
  expect_length(mice::complete(record$mids_object, action = "all"), 2L)
  fits <- with(record$mids_object, stats::lm(y ~ group + z))
  expect_s3_class(mice::pool(fits), "mipo")

  ls_set_variable_type(imported, "group", "factor")
  restored <- LinkEDA:::.rls_dataset_record(imported)
  expect_true(is.factor(restored$original_data$group))
  expect_true(all(vapply(restored$completed_datasets, function(frame) {
    is.factor(frame$group) && identical(levels(frame$group), c("Control", "Treatment"))
  }, logical(1L))))
})

test_that("shared R-object classification is explicit and completed lists are not misclassified", {
  skip_if_not_installed("mice")
  source <- mice::nhanes
  mids <- mice::mice(source, m = 2, maxit = 1, seed = 73, printFlag = FALSE)
  completed <- mice::complete(mids, action = "all")
  expect_identical(LinkEDA:::.rls_classify_r_import_object(mids), "mids")
  expect_identical(LinkEDA:::.rls_classify_r_import_object(source), "data_frame")
  expect_identical(
    LinkEDA:::.rls_classify_r_import_object(completed),
    "completed_dataset_list"
  )
  expect_identical(
    LinkEDA:::.rls_classify_r_import_object(list()),
    "completed_dataset_list"
  )
  expect_identical(LinkEDA:::.rls_classify_r_import_object(1:3), "unsupported")

  path <- tempfile(fileext = ".rds")
  saveRDS(completed, path)
  expect_error(
    ls_import_rds(path, make_active = FALSE),
    "Multiple-imputation data require a `mids` object"
  )
})

test_that("completed datasets are validated before MI registration", {
  original <- data.frame(
    x = c(1, NA, 3),
    g = factor(c("A", "B", "A"), levels = c("A", "B")),
    row.names = c("case-a", "case-b", "case-c")
  )
  good <- list(transform(original, x = c(1, 2, 3)))
  expect_invisible(LinkEDA:::.rls_mi_validate_completed_datasets(
    good, original, row.names(original)
  ))
  expect_error(
    LinkEDA:::.rls_mi_validate_completed_datasets(
      list(good[[1L]][-1L, ]), original, row.names(original)
    ),
    "source row count"
  )
  reordered <- good[[1L]][c(2, 1, 3), ]
  expect_error(
    LinkEDA:::.rls_mi_validate_completed_datasets(
      list(reordered), original, row.names(original)
    ),
    "source row identity"
  )
  changed_levels <- good[[1L]]
  changed_levels$g <- factor(changed_levels$g, levels = c("B", "A"))
  expect_error(
    LinkEDA:::.rls_mi_validate_completed_datasets(
      list(changed_levels), original, row.names(original)
    ),
    "incompatible type or categories"
  )
})

test_that("mids import preserves semantic types, factor order, levels, and labels", {
  skip_if_not_installed("mice")
  source <- data.frame(
    score = c(1, 2, NA, 4, 5, 6, NA, 8, 9, 10, 11, 12),
    g = factor(rep(c("A", "B", "C"), 4), levels = c("A", "B", "C")),
    rating = ordered(rep(c("low", "mid", "high"), 4),
                     levels = c("low", "mid", "high"))
  )
  attr(source$score, "label") <- "Imported score"
  mids <- suppressWarnings(mice::mice(
    source, m = 2, maxit = 1, seed = 17, printFlag = FALSE
  ))
  path <- tempfile(fileext = ".rds")
  saveRDS(mids, path)
  imported <- ls_import_rds(path, name = "mi semantic metadata", make_active = FALSE)
  record <- LinkEDA:::.rls_dataset_record(imported)
  on.exit({
    if (exists(record$imputation_id,
               envir = LinkEDA:::.rls_state$missing_imputations,
               inherits = FALSE)) {
      rm(list = record$imputation_id,
         envir = LinkEDA:::.rls_state$missing_imputations)
    }
    if (imported %in% ls_datasets()$name) ls_unregister_dataset(imported)
  }, add = TRUE)

  types <- setNames(record$variable_metadata$current_analysis_type,
                    record$variable_metadata$variable_name)
  expect_identical(types[["score"]], "numeric")
  expect_identical(types[["g"]], "factor")
  expect_identical(types[["rating"]], "ordered")
  expect_identical(levels(record$original_data$g), c("A", "B", "C"))
  expect_identical(levels(record$original_data$rating), c("low", "mid", "high"))
  expect_true(is.ordered(record$original_data$rating))
  expect_true(all(vapply(record$completed_datasets, function(data) {
    identical(levels(data$g), c("A", "B", "C")) &&
      identical(levels(data$rating), c("low", "mid", "high")) &&
      is.ordered(data$rating)
  }, logical(1L))))
  expect_identical(
    record$variable_metadata$label[
      record$variable_metadata$variable_name == "score"
    ][[1L]],
    "Imported score"
  )
})

test_that("the shared staged importer preserves a mids object through preview and commit", {
  skip_if_not_installed("mice")
  source <- mice::nhanes
  mids <- mice::mice(source, m = 2, maxit = 1, seed = 91, printFlag = FALSE)
  path <- tempfile(fileext = ".rds")
  saveRDS(mids, path)
  source_path <- file.path(tempdir(), "mi_staged_reference.rds")
  replies <- list()
  testthat::local_mocked_bindings(
    .rls_send = function(lines, expect_reply = TRUE) {
      replies[[length(replies) + 1L]] <<- lines
      invisible("OK")
    },
    .package = "LinkEDA"
  )

  LinkEDA:::.rls_handle_import_data_needed(path, source_path, FALSE)
  expect_equal(replies[[1L]][[1L]], "IMPORT_DATA_PREVIEW")
  staged <- replies[[1L]][[2L]]
  expect_setequal(tail(replies[[1L]], 4L), names(source))
  # Commit must activate the previewed mids dataset, not read the RDS and
  # reconstruct every completed imputation again.
  unlink(path)
  LinkEDA:::.rls_handle_import_data_commit_needed(c(
    "IMPORT_DATA_COMMIT_NEEDED", staged, "0", "4", names(source)
  ))
  record <- LinkEDA:::.rls_dataset_record(staged)
  on.exit({
    if (exists(record$imputation_id,
               envir = LinkEDA:::.rls_state$missing_imputations,
               inherits = FALSE)) {
      rm(list = record$imputation_id,
         envir = LinkEDA:::.rls_state$missing_imputations)
    }
    if (staged %in% ls_datasets()$name) ls_unregister_dataset(staged)
  }, add = TRUE)
  expect_identical(record$dataset_type, "multiple_imputation")
  expect_identical(ls_active_dataset(), staged)
  expect_identical(record$imputation_count, 2L)
  expect_length(record$completed_datasets, 2L)
  provenance_code <- record$data_provenance$origin_code
  expect_match(provenance_code, "Reconstructed from mids metadata", fixed = TRUE)
  expect_false(grepl("readRDS\\(|load\\(", provenance_code))
  expect_false(grepl(normalizePath(path, mustWork = FALSE), provenance_code,
                     fixed = TRUE))
  expect_equal(replies[[length(replies)]][1:2], c("IMPORT_DATA_RESULT", "ok"))
})

test_that("mice imports support direct objects and RData files", {
  skip_if_not_installed("mice")
  source <- mice::nhanes
  mids <- mice::mice(source, m = 2, maxit = 2, seed = 42, printFlag = FALSE)
  rdata <- tempfile(fileext = ".RData")
  ordinary <- data.frame(a = 1:3)
  save(mids, ordinary, file = rdata)

  direct <- ls_import_mice(mids, name = "direct mids", make_active = FALSE)
  from_file <- ls_import_rdata(rdata, name = "rdata mids", make_active = FALSE)
  on.exit(ls_unregister_dataset(direct), add = TRUE)
  on.exit(ls_unregister_dataset(from_file), add = TRUE)
  direct_record <- LinkEDA:::.rls_dataset_record(direct)
  file_record <- LinkEDA:::.rls_dataset_record(from_file)
  on.exit(rm(list = direct_record$imputation_id,
             envir = LinkEDA:::.rls_state$missing_imputations), add = TRUE)
  on.exit(rm(list = file_record$imputation_id,
             envir = LinkEDA:::.rls_state$missing_imputations), add = TRUE)

  expect_equal(direct_record$dataset_type, "multiple_imputation")
  expect_equal(file_record$dataset_type, "multiple_imputation")
  expect_match(file_record$source, "mice mids R data file")
  expect_equal(file_record$path, normalizePath(rdata, mustWork = FALSE))
})

test_that("the external MI reference fixture preserves data and matches mice numerically", {
  reference_path <- Sys.getenv("LINKEDA_MI_REFERENCE_RDS", unset = "")
  skip_if(!nzchar(reference_path) || !file.exists(reference_path),
          "Set LINKEDA_MI_REFERENCE_RDS to run the external parity fixture")
  skip_if_not_installed("mice")
  skip_if_not_installed("broom")
  skip_if_not_installed("MASS")

  mids <- readRDS(reference_path)
  expect_s3_class(mids, "mids")
  expected_completed <- mice::complete(mids, action = "all")
  imported <- ls_import_rds(
    reference_path, name = "mi external parity", make_active = FALSE
  )
  record <- LinkEDA:::.rls_dataset_record(imported)
  on.exit({
    if (exists(record$imputation_id,
               envir = LinkEDA:::.rls_state$missing_imputations,
               inherits = FALSE)) {
      rm(list = record$imputation_id,
         envir = LinkEDA:::.rls_state$missing_imputations)
    }
    if (imported %in% ls_datasets()$name) ls_unregister_dataset(imported)
  }, add = TRUE)

  expect_identical(record$dataset_type, "multiple_imputation")
  expect_identical(record$imputation_count, 5L)
  expect_identical(record$original_data, mids$data)
  expect_identical(record$original_row_ids, seq_len(nrow(mids$data)))
  expect_identical(record$original_row_names, row.names(mids$data))
  expect_true(all(vapply(seq_along(expected_completed), function(version) {
    identical(record$completed_datasets[[version]], expected_completed[[version]])
  }, logical(1L))))
  g_row <- match("g", record$variable_metadata$variable_name)
  expect_identical(record$variable_metadata$current_analysis_type[[g_row]], "factor")
  expect_identical(record$variable_metadata$factor_levels[[g_row]], c("A", "B", "C"))
  expect_identical(ls_analysis_backend(imported, "binary_regression")$backend,
                   "multiple_imputation")

  binary <- ls_new_binary_regression(
    imported, response = "y_bin", terms = c("x", "z", "g"),
    event = "1", reference = "0", link = "logit", native = FALSE,
    factor_reference_levels = c(g = "A")
  )
  binary_state <- ls_binary_regression_state(binary)
  expect_identical(binary_state$coefficients$term,
                   c("(Intercept)", "x", "z", "gB", "gC"))
  expect_equal(binary_state$coefficients$estimate,
               c(0.09676639, 0.91807121, -0.71718427, 0.19347669, -0.82158596),
               tolerance = 1e-7)
  expect_equal(binary_state$coefficients$std_error,
               c(0.2049626, 0.1574646, 0.1717188, 0.2946631, 0.3012714),
               tolerance = 1e-6)

  reverse <- ls_new_binary_regression(
    imported, response = "y_bin", terms = c("x", "z", "g"),
    event = "0", reference = "1", link = "logit", native = FALSE,
    factor_reference_levels = c(g = "A")
  )
  reverse_state <- ls_binary_regression_state(reverse)
  expect_equal(reverse_state$coefficients$estimate,
               -binary_state$coefficients$estimate, tolerance = 1e-12)
  expect_equal(reverse_state$coefficients$std_error,
               binary_state$coefficients$std_error, tolerance = 1e-12)

  # Reproduce the macOS comparison case exactly, including the numeric-by-
  # factor interaction.  Every event transition must fit the same five
  # completed datasets; event 0 is the exact complement of event 1.
  event_sequence <- c("1", "0", "1", "0")
  interaction_models <- lapply(event_sequence, function(event) {
    ls_new_binary_regression(
      imported, response = "y_bin", terms = c("x", "z", "g", "x:g"),
      event = event, reference = if (event == "1") "0" else "1",
      link = "logit", native = FALSE,
      term_types = list(x = "numeric", z = "numeric", g = "factor"),
      factor_reference_levels = c(g = "A")
    )
  })
  interaction_states <- lapply(interaction_models, ls_binary_regression_state)
  expected_terms <- c("(Intercept)", "x", "z", "gB", "gC", "x:gB", "x:gC")
  for (i in seq_along(interaction_states)) {
    state <- interaction_states[[i]]
    expect_identical(state$analysis_backend, "multiple_imputation")
    expect_identical(state$multiple_imputation$m, 5L)
    expect_length(state$fits_by_imputation, 5L)
    expect_identical(state$coefficients$term, expected_terms)
    expect_identical(state$summary$parameter_count, 7L)
    expect_identical(state$summary$df_residual, 393)
    expect_true(all(vapply(seq_along(state$fits_by_imputation), function(version) {
      fitted_response <- as.integer(stats::model.response(
        stats::model.frame(state$fits_by_imputation[[version]])))
      expected_response <- as.integer(
        as.character(record$completed_datasets[[version]]$y_bin) == event_sequence[[i]]
      )
      identical(fitted_response, expected_response)
    }, logical(1L))))
  }
  for (i in 2:length(interaction_states)) {
    same_event <- identical(event_sequence[[i]], event_sequence[[i - 1L]])
    multiplier <- if (same_event) 1 else -1
    expect_equal(
      interaction_states[[i]]$coefficients$estimate,
      multiplier * interaction_states[[i - 1L]]$coefficients$estimate,
      tolerance = 1e-12
    )
    expect_equal(
      interaction_states[[i]]$coefficients$std_error,
      interaction_states[[i - 1L]]$coefficients$std_error,
      tolerance = 1e-12
    )
  }

  # Exact Binary Model Comparison parity case: the two factor contrasts and
  # the two numeric-by-factor interaction contrasts are six independent model
  # matrix columns in every completed dataset.  Parameter count and residual
  # df come from those fitted matrices, not from the presentation hierarchy.
  exact_binary_interaction <- ls_binary_regression_state(
    ls_new_binary_regression(
      imported, response = "y_bin", terms = c("x", "g", "x:g"),
      event = "1", reference = "0", link = "logit", native = FALSE,
      term_types = list(x = "numeric", g = "factor"),
      factor_reference_levels = c(g = "A")
    )
  )
  expect_identical(
    exact_binary_interaction$coefficients$term,
    c("(Intercept)", "x", "gB", "gC", "x:gB", "x:gC")
  )
  expect_identical(exact_binary_interaction$summary$parameter_count, 6L)
  expect_identical(exact_binary_interaction$summary$df_residual, 394)
  expect_true(all(vapply(exact_binary_interaction$fits_by_imputation, function(fit) {
    identical(
      colnames(stats::model.matrix(fit)),
      c("(Intercept)", "x", "gB", "gC", "x:gB", "x:gC")
    ) &&
      identical(names(stats::coef(fit)),
                c("(Intercept)", "x", "gB", "gC", "x:gB", "x:gC")) &&
      identical(fit$rank, 6L) &&
      identical(stats::df.residual(fit), 394L)
  }, logical(1L))))
  expect_identical(
    exact_binary_interaction$coefficient_rows$row_type[
      exact_binary_interaction$coefficient_rows$source_term == "g"
    ],
    c("factor_parent", "reference", "factor_level", "factor_level")
  )
  expect_identical(
    exact_binary_interaction$coefficient_rows$row_type[
      exact_binary_interaction$coefficient_rows$source_term == "x:g"
    ],
    c("term_parent", "coefficient", "coefficient")
  )

  exact_binary_reference_b <- ls_binary_regression_state(
    ls_new_binary_regression(
      imported, response = "y_bin", terms = c("x", "g", "x:g"),
      event = "1", reference = "0", link = "logit", native = FALSE,
      term_types = list(x = "numeric", g = "factor"),
      factor_reference_levels = c(g = "B")
    )
  )
  expect_identical(
    exact_binary_reference_b$coefficients$term,
    c("(Intercept)", "x", "gA", "gC", "x:gA", "x:gC")
  )
  expect_true(all(vapply(exact_binary_reference_b$fits_by_imputation, function(fit) {
    identical(fit$rank, 6L) && identical(stats::df.residual(fit), 394L)
  }, logical(1L))))
  expect_equal(
    Map(stats::fitted, exact_binary_interaction$fits_by_imputation),
    Map(stats::fitted, exact_binary_reference_b$fits_by_imputation),
    tolerance = 1e-8
  )

  # Exercise the exact native-comparison request path used after changing the
  # event popup.  It must retrieve the canonical MI DatasetRecord by id rather
  # than fitting the compact preview or manufacturing a one-imputation copy.
  captured <- NULL
  sent <- list()
  testthat::local_mocked_bindings(
    .rls_send = function(lines, expect_reply = TRUE) {
      sent[[length(sent) + 1L]] <<- lines
      captured <<- lines
      "OK"
    },
    .package = "LinkEDA"
  )
  native_state <- LinkEDA:::.rls_state
  previous_started <- native_state$process_started
  on.exit({ native_state$process_started <- previous_started }, add = TRUE)
  native_state$process_started <- TRUE
  source_dataset_id <- if (is.null(record$source_dataset_id)) "" else
    as.character(record$source_dataset_id)
  request <- c(
    "GCOMP_NEEDED", "mi_event_comparison", imported,
    "y_bin", "binomial", "logit", "all", "BINARY", "0", "1",
    "3", "x", "numeric", "z", "numeric", "g", "factor",
    "1", "mi_event_model_1", "Model 1",
    "4", "x", "z", "g", "x:g", "0",
    "GCOMP_SPEC_V5", "73", "TRUE", "FALSE",
    "multiple_imputation", as.character(record$imputation_id),
    source_dataset_id, "5", "1",
    "mi_event_model_1", "y_bin", "binomial", "logit", "all",
    "FALSE", "poisson", "", "29", "mi-event-fingerprint-29",
    "4", "x", "z", "g", "x:g",
    "3", "x", "numeric", "z", "numeric", "g", "factor",
    "0", "1", "g", "A"
  )
  expect_true(isTRUE(LinkEDA:::.rls_handle_generalized_comparison_needed(request)))
  expect_identical(
    sent[[1L]],
    c("GCOMP_TASK_RECEIVED", "mi_event_comparison", "73")
  )
  comparison_record <- LinkEDA:::.rls_generalized_glm_record("mi_event_model_1")
  expect_identical(comparison_record$event, "0")
  expect_identical(comparison_record$reference, "1")
  expect_identical(comparison_record$analysis_backend, "multiple_imputation")
  expect_identical(comparison_record$multiple_imputation$m, 5L)
  expect_length(comparison_record$fits_by_imputation, 5L)
  expect_identical(comparison_record$coefficients$term, expected_terms)
  expect_equal(
    comparison_record$coefficients$estimate,
    interaction_states[[2L]]$coefficients$estimate,
    tolerance = 1e-12
  )
  expect_equal(
    comparison_record$coefficients$std_error,
    interaction_states[[2L]]$coefficients$std_error,
    tolerance = 1e-12
  )
  expect_true("GENERALIZED_COMPARISON_MODELS_V5" %in% captured)
  expect_true("mi-event-fingerprint-29" %in% captured)
  expect_true(all(c("factor_parent", "reference", "factor_level",
                    "term_parent", "coefficient") %in% captured))
  expect_true(all(c("x", "z", "g", "x:g") %in% captured))
  expect_true("393" %in% captured)
  expect_true("7" %in% captured)

  # A terminal error for the current V5 generation must be sent back to the
  # comparison window.  Otherwise native remains in its pending state and the
  # whole coefficient column appears to hang indefinitely.
  invalid_identity_request <- request
  invalid_identity_request[
    match(as.character(record$imputation_id), invalid_identity_request)
  ] <- paste0(record$imputation_id, "-obsolete")
  expect_warning(
    expect_false(isTRUE(LinkEDA:::.rls_handle_generalized_comparison_needed(
      invalid_identity_request
    ))),
    "different dataset/imputation revision"
  )
  expect_identical(captured[[1L]], "GENERALIZED_COMPARISON_UPDATE_ERROR")
  expect_identical(captured[[2L]], "mi_event_comparison")
  expect_identical(captured[[3L]], "73")
  expect_match(captured[[4L]], "different dataset/imputation revision")

  # A native comparison owns a separate R model record.  Opening it from an
  # already-fitted single Binary Regression, and then changing the comparison
  # interpretation, must not rewrite the source record or any of its pooled
  # fits/semantic rows.
  source_independence <- ls_new_binary_regression(
    imported, response = "y_bin", terms = c("x", "g"),
    event = "1", reference = "0", link = "logit", native = FALSE,
    name = "mi_binary_source_independence",
    term_types = list(x = "numeric", g = "factor"),
    factor_reference_levels = c(g = "A")
  )
  source_before <- LinkEDA:::.rls_generalized_glm_record(
    source_independence$id
  )
  expect_identical(source_before$coefficients$term,
                   c("(Intercept)", "x", "gB", "gC"))
  expect_identical(source_before$summary$parameter_count, 4L)
  expect_identical(source_before$summary$df_residual, 396)
  source_snapshot <- serialize(source_before, NULL)
  independence_request <- c(
    "GCOMP_NEEDED", "mi_binary_independence", imported,
    "y_bin", "binomial", "logit", "all", "BINARY", "1", "0",
    "2", "x", "numeric", "g", "factor",
    "1", "mi_binary_independence:model:1", "Model 1",
    "2", "x", "g", "0",
    "GCOMP_SPEC_V5", "81", "TRUE", "FALSE",
    "multiple_imputation", as.character(record$imputation_id),
    source_dataset_id, "5", "1",
    "mi_binary_independence:model:1", "y_bin", "binomial", "logit", "all",
    "FALSE", "poisson", "", "41", "mi-binary-factor-fingerprint-41",
    "2", "x", "g",
    "2", "x", "numeric", "g", "factor",
    "0", "1", "g", "A"
  )
  expect_true(isTRUE(LinkEDA:::.rls_handle_generalized_comparison_needed(
    independence_request
  )))
  comparison_factor <- LinkEDA:::.rls_generalized_glm_record(
    "mi_binary_independence:model:1"
  )
  expect_identical(comparison_factor$coefficients$term,
                   c("(Intercept)", "x", "gB", "gC"))
  expect_identical(comparison_factor$summary$parameter_count, 4L)
  expect_identical(comparison_factor$summary$df_residual, 396)
  expect_true(all(vapply(comparison_factor$fits_by_imputation, function(fit) {
    identical(colnames(stats::model.matrix(fit)),
              c("(Intercept)", "x", "gB", "gC")) &&
      identical(stats::df.residual(fit), 396L)
  }, logical(1L))))
  expect_identical(
    serialize(LinkEDA:::.rls_generalized_glm_record(source_independence$id), NULL),
    source_snapshot
  )

  # Refit the comparison alone with a deliberately different, numeric g.
  # The result may change in that independent comparison session, but the
  # source model must remain byte-for-byte invariant.
  independence_numeric <- c(
    "GCOMP_NEEDED", "mi_binary_independence", imported,
    "y_bin", "binomial", "logit", "all", "BINARY", "1", "0",
    "2", "x", "numeric", "g", "numeric",
    "1", "mi_binary_independence:model:1", "Model 1",
    "2", "x", "g", "0",
    "GCOMP_SPEC_V5", "82", "TRUE", "FALSE",
    "multiple_imputation", as.character(record$imputation_id),
    source_dataset_id, "5", "1",
    "mi_binary_independence:model:1", "y_bin", "binomial", "logit", "all",
    "FALSE", "poisson", "", "42", "mi-binary-numeric-fingerprint-42",
    "2", "x", "g",
    "2", "x", "numeric", "g", "numeric",
    "0", "0"
  )
  expect_true(isTRUE(LinkEDA:::.rls_handle_generalized_comparison_needed(
    independence_numeric
  )))
  comparison_numeric <- LinkEDA:::.rls_generalized_glm_record(
    "mi_binary_independence:model:1"
  )
  expect_identical(comparison_numeric$coefficients$term,
                   c("(Intercept)", "x", "g"))
  expect_identical(comparison_numeric$summary$parameter_count, 3L)
  expect_identical(comparison_numeric$summary$df_residual, 397)
  expect_identical(
    serialize(LinkEDA:::.rls_generalized_glm_record(source_independence$id), NULL),
    source_snapshot
  )

  count_distributions <- c("poisson", "quasipoisson", "negative_binomial")
  count_states <- setNames(lapply(count_distributions, function(distribution) {
    model <- ls_new_count_regression(
      imported, response = "y_count", terms = c("x", "z", "g"),
      distribution = distribution, native = FALSE,
      term_types = list(x = "numeric", z = "numeric", g = "factor"),
      factor_reference_levels = c(g = "A")
    )
    ls_count_regression_state(model)
  }), count_distributions)
  for (distribution in names(count_states)) {
    state <- count_states[[distribution]]
    expect_identical(state$term_types$g, "factor", info = distribution)
    expect_identical(state$summary$n_used, 400L, info = distribution)
    expect_identical(state$summary$df_residual, 395, info = distribution)
    expect_identical(state$summary$parameter_count, 5L, info = distribution)
    expect_identical(
      state$coefficients$term,
      c("(Intercept)", "x", "z", "gB", "gC"),
      info = distribution
    )
    expect_identical(
      state$coefficient_rows$row_type[state$coefficient_rows$source_term == "g"],
      c("factor_parent", "reference", "factor_level", "factor_level"),
      info = distribution
    )
    expect_identical(
      state$coefficient_rows$level[state$coefficient_rows$source_term == "g"],
      c(NA_character_, "A", "B", "C"),
      info = distribution
    )
    expect_identical(
      state$coefficient_rows$reference_level[state$coefficient_rows$source_term == "g"],
      rep("A", 4L),
      info = distribution
    )
    for (i in seq_along(state$fits_by_imputation)) {
      fit <- state$fits_by_imputation[[i]]
      frame_g <- stats::model.frame(fit)$g
      expect_true(inherits(frame_g, "factor"), info = paste(distribution, i))
      expect_identical(levels(frame_g), c("A", "B", "C"),
                       info = paste(distribution, i))
      expect_identical(rownames(stats::contrasts(frame_g)), c("A", "B", "C"),
                       info = paste(distribution, i))
      expect_identical(
        paste(deparse(stats::formula(fit)), collapse = ""),
        "y_count ~ x + z + g",
        info = paste(distribution, i)
      )
      expect_identical(
        colnames(stats::model.matrix(fit)),
        c("(Intercept)", "x", "z", "gB", "gC"),
        info = paste(distribution, i)
      )
      expect_identical(
        names(stats::coef(fit)),
        c("(Intercept)", "x", "z", "gB", "gC"),
        info = paste(distribution, i)
      )
      expect_identical(stats::df.residual(fit), 395L,
                       info = paste(distribution, i))
    }
    expect_true(all(vapply(
      state$multiple_imputation$complete_data_result_by_imputation,
      function(complete) {
        inherits(complete$data$g, "factor") &&
          identical(levels(complete$data$g), c("A", "B", "C"))
      },
      logical(1L)
    )), info = distribution)

    reference_b <- ls_count_regression_state(ls_new_count_regression(
      imported, response = "y_count", terms = c("x", "z", "g"),
      distribution = distribution, native = FALSE,
      term_types = list(x = "numeric", z = "numeric", g = "factor"),
      factor_reference_levels = c(g = "B")
    ))
    expect_identical(
      reference_b$coefficients$term,
      c("(Intercept)", "x", "z", "gA", "gC"),
      info = distribution
    )
    expect_identical(
      reference_b$coefficient_rows$row_type[
        reference_b$coefficient_rows$source_term == "g"
      ],
      c("factor_parent", "reference", "factor_level", "factor_level"),
      info = distribution
    )
    expect_identical(
      reference_b$coefficient_rows$level[
        reference_b$coefficient_rows$source_term == "g"
      ],
      c(NA_character_, "B", "A", "C"),
      info = distribution
    )
    expect_true(all(vapply(reference_b$fits_by_imputation, function(fit) {
      identical(levels(stats::model.frame(fit)$g), c("B", "A", "C")) &&
        identical(stats::df.residual(fit), 395L)
    }, logical(1L))), info = distribution)
    expect_equal(
      Map(stats::fitted, state$fits_by_imputation),
      Map(stats::fitted, reference_b$fits_by_imputation),
      tolerance = 1e-8,
      info = distribution
    )
  }

  # Exact Count screenshot: the numeric-by-numeric interaction has a semantic
  # parent and a coefficient row with the same R name (`x:z`).  The fit itself
  # has rank six and df 394; comparison rendering must preserve both rows.
  exact_count_interaction <- ls_count_regression_state(
    ls_new_count_regression(
      imported, response = "y_count", terms = c("x", "z", "g", "x:z"),
      distribution = "poisson", native = FALSE,
      term_types = list(x = "numeric", z = "numeric", g = "factor"),
      factor_reference_levels = c(g = "A")
    )
  )
  expect_identical(
    exact_count_interaction$coefficients$term,
    c("(Intercept)", "x", "z", "gB", "gC", "x:z")
  )
  expect_identical(exact_count_interaction$summary$parameter_count, 6L)
  expect_identical(exact_count_interaction$summary$df_residual, 394)
  expect_true(all(vapply(exact_count_interaction$fits_by_imputation, function(fit) {
    identical(
      colnames(stats::model.matrix(fit)),
      c("(Intercept)", "x", "z", "gB", "gC", "x:z")
    ) &&
      identical(fit$rank, 6L) &&
      identical(stats::df.residual(fit), 394L)
  }, logical(1L))))
  expect_identical(
    exact_count_interaction$coefficient_rows$row_type[
      exact_count_interaction$coefficient_rows$source_term == "x:z"
    ],
    c("term_parent", "coefficient")
  )
  expect_identical(
    exact_count_interaction$coefficient_rows$display_label[
      exact_count_interaction$coefficient_rows$source_term == "x:z"
    ],
    c("x:z", "  x:z")
  )

  # Exact macOS Count Comparison lifecycle after removing z.  The V5 request
  # carries the canonical MI set identity and must fit all five completed
  # datasets, producing the same model as the single Count Regression path.
  reduced_count <- ls_count_regression_state(ls_new_count_regression(
    imported, response = "y_count", terms = c("x", "g"),
    distribution = "poisson", native = FALSE,
    term_types = list(x = "numeric", g = "factor"),
    factor_reference_levels = c(g = "A")
  ))
  captured <- NULL
  source_dataset_id <- if (is.null(record$source_dataset_id)) "" else
    as.character(record$source_dataset_id)
  count_request <- c(
    "GCOMP_NEEDED", "mi_count_lifecycle", imported,
    "y_count", "poisson", "log", "all", "COUNT", "", "",
    "2", "x", "numeric", "g", "factor",
    "1", "mi_count_lifecycle:model:1", "Model 1",
    "2", "x", "g", "0",
    "GCOMP_SPEC_V5", "91", "TRUE", "TRUE",
    "multiple_imputation", as.character(record$imputation_id),
    source_dataset_id, "5", "1",
    "mi_count_lifecycle:model:1", "y_count", "poisson", "log", "all",
    "TRUE", "poisson", "", "31", "mi-count-reduced-fingerprint-31",
    "2", "x", "g",
    "2", "x", "numeric", "g", "factor",
    "0", "1", "g", "A"
  )
  expect_true(isTRUE(LinkEDA:::.rls_handle_generalized_comparison_needed(
    count_request
  )))
  comparison_count <- LinkEDA:::.rls_generalized_glm_record(
    "mi_count_lifecycle:model:1"
  )
  expect_identical(comparison_count$analysis_backend, "multiple_imputation")
  expect_identical(comparison_count$multiple_imputation$m, 5L)
  expect_length(comparison_count$fits_by_imputation, 5L)
  expect_identical(comparison_count$coefficients$term,
                   c("(Intercept)", "x", "gB", "gC"))
  expect_identical(comparison_count$summary$parameter_count, 4L)
  expect_identical(comparison_count$summary$df_residual, 396)
  expect_equal(comparison_count$summary$residual_deviance,
               1484.757, tolerance = 1e-3)
  expect_equal(comparison_count$coefficients$estimate,
               reduced_count$coefficients$estimate, tolerance = 1e-12)
  expect_equal(comparison_count$coefficients$std_error,
               reduced_count$coefficients$std_error, tolerance = 1e-12)
  expect_true(all(vapply(comparison_count$fits_by_imputation, function(fit) {
    identical(colnames(stats::model.matrix(fit)),
              c("(Intercept)", "x", "gB", "gC")) &&
      identical(stats::df.residual(fit), 396L)
  }, logical(1L))))
  expect_true("GENERALIZED_COMPARISON_MODELS_V5" %in% captured)
  expect_true("mi-count-reduced-fingerprint-31" %in% captured)

  count <- ls_new_count_regression(
    imported, response = "y_count", terms = c("x", "z", "g"),
    distribution = "negative_binomial", native = FALSE,
    term_types = list(x = "numeric", z = "numeric", g = "factor"),
    factor_reference_levels = c(g = "A")
  )
  count_state <- count_states$negative_binomial
  expect_identical(count_state$summary$fitter, "MASS::glm.nb")
  expect_equal(count_state$coefficients$estimate,
               c(0.71002647, 0.39406536, -0.30071987, 0.55649921, 0.05315062),
               tolerance = 1e-7)
  expect_equal(count_state$coefficients$std_error,
               c(0.10929114, 0.06300348, 0.06074921, 0.14652552, 0.15024881),
               tolerance = 1e-7)

  reduced <- ls_new_count_regression(
    imported, response = "y_count", terms = c("x", "z"),
    distribution = "negative_binomial", native = FALSE
  )
  comparison <- ls_compare_count_regression_models(reduced, count, native = FALSE)
  expect_identical(comparison$tests$method[[1L]], "mice::D1")
  expect_true(comparison$tests$available[[1L]])
  expect_equal(comparison$tests$statistic[[1L]], 8.80657, tolerance = 1e-5)
  expect_equal(comparison$tests$df1[[1L]], 2, tolerance = 1e-12)
  expect_equal(comparison$tests$df2[[1L]], 267.7497, tolerance = 1e-4)
  expect_lt(abs(comparison$tests$p_value[[1L]] - 0.00019765), 1e-8)
})

test_that("Windows multiple-imputation task preserves the dialog specification", {
  created <- NULL
  run <- NULL
  sent <- list()
  testthat::local_mocked_bindings(
    ls_new_missing_data_imputation = function(data, impute, predictors, m, maxit,
                                               seed, method, name) {
      created <<- list(data = data, impute = impute, predictors = predictors,
                       m = m, maxit = maxit, seed = seed, method = method,
                       name = name)
      structure(list(id = "imp-id"), class = "rlispstat_imputation")
    },
    ls_run_imputation = function(imputation, open_data, make_active) {
      run <<- list(imputation = imputation, open_data = open_data,
                  make_active = make_active)
      structure(list(id = "imp-id", dataset_id = "cars imputed",
                     group = "cars imputed"), class = "rlispstat_imputation")
    },
    .rls_send = function(lines, ...) {
      sent[[length(sent) + 1L]] <<- lines
      "OK"
    },
    .package = "LinkEDA"
  )
  parts <- c("MULTIPLE_IMPUTATION_NEEDED", "cars", "5", "7", "42", "TRUE",
             "2", "mpg", "hp", "3", "mpg", "cyl", "hp",
             "2", "mpg", "pmm", "hp", "norm")
  expect_true(LinkEDA:::.rls_handle_multiple_imputation_needed(parts))
  expect_equal(created$data, "cars")
  expect_equal(created$impute, c("mpg", "hp"))
  expect_equal(created$predictors, c("mpg", "cyl", "hp"))
  expect_equal(created$method, c(mpg = "pmm", hp = "norm"))
  expect_equal(created$m, 5L)
  expect_equal(created$maxit, 7L)
  expect_equal(created$seed, 42L)
  expect_true(run$open_data)
  expect_true(run$make_active)
  expect_equal(sent[[1L]][1:3],
               c("MULTIPLE_IMPUTATION_RESULT", "ok", "cars imputed"))
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
  # This test helper promotes an existing ordinary dataset to MI in place.
  # Preserve the promoted immutable version, as the production MI importer does.
  LinkEDA:::.rls_store_data_version(record)
  id
}

test_that("MI regression families preserve ordered-factor level order and interactions", {
  skip_if_not_installed("mice")
  skip_if_not_installed("broom")

  n <- 90L
  set.seed(20260828)
  x <- seq(-1.25, 1.25, length.out = n)
  group <- factor(rep(c("A", "B", "C"), length.out = n),
                  levels = c("A", "B", "C"))
  rating <- ordered(rep(c("low", "mid", "high"), each = n / 3L),
                    levels = c("low", "mid", "high"))
  rating_effect <- c(low = -.3, mid = .2, high = .55)[as.character(rating)]
  group_effect <- c(A = 0, B = .25, C = -.15)[as.character(group)]
  interaction_effect <- x * c(low = 0, mid = .18, high = -.12)[as.character(rating)]
  linear <- 2 + .7 * x + rating_effect + group_effect + interaction_effect +
    stats::rnorm(n, sd = .25)
  binary <- factor(
    stats::rbinom(n, 1, stats::plogis(-.15 + .5 * x + rating_effect +
                                      interaction_effect)),
    levels = c(0, 1)
  )
  count <- stats::rpois(n, exp(.75 + .2 * x + rating_effect + interaction_effect))
  completed <- lapply(seq_len(3L), function(i) {
    data.frame(
      linear = linear + (i - 2L) * .01,
      binary = binary,
      count = count,
      x = x + (i - 2L) * .003,
      group = group,
      rating = rating
    )
  })
  original <- completed[[1L]]
  original$x[c(8L, 47L)] <- NA_real_
  id <- .make_test_mi_dataset("mi_ordered_regression_matrix", original, completed)
  on.exit(ls_unregister_dataset(id), add = TRUE)

  term_types <- list(x = "numeric", group = "factor", rating = "factor")
  references <- list(group = "A", rating = "mid")
  expected_columns <- c(
    "(Intercept)", "x", "groupB", "groupC", "ratinglow", "ratinghigh",
    "x:ratinglow", "x:ratinghigh"
  )
  assert_fits <- function(fits, label) {
    expect_length(fits, 3L)
    expect_true(all(vapply(fits, function(fit) {
      model_rating <- stats::model.frame(fit)$rating
      is.factor(model_rating) &&
        identical(levels(model_rating), c("mid", "low", "high")) &&
        identical(colnames(stats::model.matrix(fit)), expected_columns) &&
        identical(names(stats::coef(fit)), expected_columns)
    }, logical(1L))), info = label)
  }

  linear_comparison <- ls_new_regression_comparison(
    id, response = "linear",
    models = list(Full = list(
      terms = c("x", "group", "rating", "x:rating"),
      term_types = term_types,
      factor_reference_levels = references
    )),
    native = FALSE
  )
  linear_state <- ls_regression_comparison_state(linear_comparison)
  assert_fits(linear_state$models[[1L]]$fits_by_imputation, "linear")

  generalized <- ls_new_generalized_linear_model(
    id, response = "linear", terms = c("x", "group", "rating", "x:rating"),
    family = "gaussian", link = "identity", native = FALSE,
    term_types = term_types, factor_reference_levels = references
  )
  assert_fits(ls_generalized_linear_model_state(generalized)$fits_by_imputation,
              "generalized")

  binary_model <- ls_new_binary_regression(
    id, response = "binary", terms = c("x", "group", "rating", "x:rating"),
    event = "1", reference = "0", link = "logit", native = FALSE,
    term_types = term_types, factor_reference_levels = references
  )
  assert_fits(ls_binary_regression_state(binary_model)$fits_by_imputation,
              "binary")

  count_model <- ls_new_count_regression(
    id, response = "count", terms = c("x", "group", "rating", "x:rating"),
    distribution = "poisson", native = FALSE,
    term_types = term_types, factor_reference_levels = references
  )
  assert_fits(ls_count_regression_state(count_model)$fits_by_imputation,
              "count")
})

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

  expect_equal(record$test_results$y$test, "MI pooled omnibus (mice::D1)")
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
  direct_linear <- summary(mice::pool(mice::as.mira(state$fits_by_imputation)), conf.int = TRUE)
  direct_x <- direct_linear[as.character(direct_linear$term) == "x", , drop = FALSE]
  expect_equal(xrow$estimate, direct_x$estimate, tolerance = 1e-12)
  expect_equal(xrow$std_error, direct_x$std.error, tolerance = 1e-12)
  expect_equal(xrow$t_value, direct_x$statistic, tolerance = 1e-12)
  expect_equal(xrow$df, direct_x$df, tolerance = 1e-12)
  expect_equal(xrow$p_value, direct_x$p.value, tolerance = 1e-12)
  expect_equal(
    state$summary$r_squared,
    mice::pool.r.squared(mice::as.mira(state$fits_by_imputation))[1L, "est"],
    tolerance = 1e-12
  )
  expect_equal(
    state$summary$adj_r_squared,
    mice::pool.r.squared(mice::as.mira(state$fits_by_imputation), adjusted = TRUE)[1L, "est"],
    tolerance = 1e-12
  )
  expect_match(state$summary$fit_information_method, "mice::D1", fixed = TRUE)
  expect_match(state$summary$fit_information_method, "delta R2 are descriptive means")
  expect_equal(
    xrow$partial_r,
    tanh(mean(vapply(state$fits_by_imputation, function(fit) {
      t <- coef(summary(fit))["x", "t value"]
      atanh(t / sqrt(t^2 + df.residual(fit)))
    }, numeric(1L)))),
    tolerance = 1e-12
  )
  expected_delta <- mean(vapply(
    state$fits_by_imputation,
    function(fit) LinkEDA:::.rls_model_term_delta_r2(fit)[["x"]],
    numeric(1L)
  ))
  x_display <- state$coefficient_rows[
    state$coefficient_rows$source_term == "x" &
      state$coefficient_rows$row_type == "coefficient",
  ]
  expect_equal(x_display$delta_r2, expected_delta, tolerance = 1e-12)
  intercept_display <- state$coefficient_rows[state$coefficient_rows$term == "(Intercept)", ]
  expect_true(is.na(intercept_display$partial_r))
  expect_true(is.na(intercept_display$delta_r2))
  linear_payload <- LinkEDA:::.rls_glm_pooled_native_payload(state)
  expect_equal(linear_payload[[1L]], "MODEL_OPEN_POOLED")
  expect_true("Linear Model — Multiple Imputation" %in% linear_payload)
  expect_true(any(grepl("mice::pool", linear_payload, fixed = TRUE)))
  expect_true(any(grepl("mice::D1", linear_payload, fixed = TRUE)))
  linear_fit_payload <- LinkEDA:::.rls_native_linear_fit_payload(state)
  visible_fit_note <- linear_fit_payload[[18L]]
  expect_match(visible_fit_note, "Multiple imputation:", fixed = TRUE)
  expect_match(visible_fit_note, "Fisher-z pooling", fixed = TRUE)
  expect_match(visible_fit_note, "mice::D1", fixed = TRUE)
  expect_lte(nchar(visible_fit_note), 180L)
  expect_false(grepl("completed-data partial correlations", visible_fit_note,
                     fixed = TRUE))
  expect_length(state$diagnostics_by_imputation, 2L)
  expect_true(all(vapply(
    state$diagnostics_by_imputation,
    function(rows) is.data.frame(rows) && nrow(rows) > 0L,
    logical(1L)
  )))
  expect_true("LINEAR_MI_DIAGNOSTICS_V1" %in% linear_payload)
  linear_diagnostic_marker <- match("LINEAR_MI_DIAGNOSTICS_V1", linear_payload)
  expect_identical(linear_payload[[linear_diagnostic_marker + 1L]], "2")
  expect_true("LINEAR_SCOPE_ROWS_V1" %in% linear_payload)
  linear_scope_marker <- match("LINEAR_SCOPE_ROWS_V1", linear_payload)
  expect_identical(linear_payload[[linear_scope_marker + 1L]], "0")
  request_identity <- "y|selected|x|types|x=numeric|selection|2|5"
  update_payload <- LinkEDA:::.rls_glm_native_update_payload(
    state, request_identity)
  linear_result_marker <- match("LINEAR_RESULT_V1", update_payload)
  expect_false(is.na(linear_result_marker))
  expect_identical(unname(update_payload[linear_result_marker + 0:1]),
                   c("LINEAR_RESULT_V1", request_identity))
  expect_false("LINEAR_RESULT_V1" %in%
                 LinkEDA:::.rls_glm_native_update_payload(state))

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
  expect_true(any(grepl("mice::pool", factor_payload, fixed = TRUE)))
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
  expect_equal(gstate$coefficients$odds_ratio, exp(gstate$coefficients$estimate), tolerance = 1e-12)
  direct_generalized <- summary(mice::pool(mice::as.mira(gstate$fits_by_imputation)), conf.int = TRUE)
  expect_equal(gstate$coefficients$estimate, direct_generalized$estimate, tolerance = 1e-12)
  expect_equal(gstate$coefficients$std_error, direct_generalized$std.error, tolerance = 1e-12)
  expect_equal(gstate$coefficients$df, direct_generalized$df, tolerance = 1e-12)
  generalized_payload <- LinkEDA:::.rls_generalized_glm_pooled_native_payload(gstate)
  expect_equal(generalized_payload[[1L]], "GENERALIZED_GLM_OPEN_POOLED")
  generalized_result_marker <- match("GGLM_RESULT_V1", generalized_payload)
  expect_false(is.na(generalized_result_marker))
  expect_equal(generalized_payload[generalized_result_marker + 0:1],
               c("GGLM_RESULT_V1", "0"))
  expect_true("Generalized Linear Model — Multiple Imputation" %in% generalized_payload)
  expect_true(any(grepl("link-scale coefficients", generalized_payload, fixed = TRUE)))
  expect_length(gstate$diagnostics_by_imputation, 2L)
  expect_true(all(vapply(
    gstate$diagnostics_by_imputation,
    function(rows) is.data.frame(rows) && nrow(rows) > 0L,
    logical(1L)
  )))
  expect_true("GENERALIZED_MI_DIAGNOSTICS_V4" %in% generalized_payload)
  generalized_diagnostic_marker <- match(
    "GENERALIZED_MI_DIAGNOSTICS_V4", generalized_payload
  )
  expect_identical(generalized_payload[[generalized_diagnostic_marker + 1L]], "2")
})

test_that("MI linear models center predictors within every imputation", {
  original <- data.frame(
    y = c(3, NA, 7, 9, NA),
    x = c(1, 2, 3, 4, 5)
  )
  completed <- list(
    transform(original, y = c(3, 5.2, 7, 9.3, 10.8)),
    transform(original, y = c(3, 5.6, 7, 9.3, 10.4))
  )
  id <- .make_test_mi_dataset("mi_models_centered", original, completed)

  model <- ls_new_glm(id)
  ls_glm_set_dependent(model, "y")
  ls_glm_add_predictor(model, "x")
  model <- ls_glm_fit(model)
  raw <- LinkEDA:::.rls_glm_model_record(model)

  centered_record <- raw
  centered_record$centered_predictors <- "x"
  centered_model <- LinkEDA:::.rls_assign_glm_model(centered_record)
  centered_model <- ls_glm_fit(centered_model)
  centered <- LinkEDA:::.rls_glm_model_record(centered_model)

  expect_equal(centered$analysis_backend, "multiple_imputation")
  expect_equal(centered$centered_predictors, "x")
  expect_equal(
    vapply(raw$fits_by_imputation, function(fit) unname(stats::coef(fit)[["x"]]), numeric(1L)),
    vapply(centered$fits_by_imputation, function(fit) unname(stats::coef(fit)[["x"]]), numeric(1L)),
    tolerance = 1e-12
  )
  expect_equal(
    Map(function(raw_fit, centered_fit) unname(stats::fitted(raw_fit) - stats::fitted(centered_fit)),
        raw$fits_by_imputation, centered$fits_by_imputation),
    list(rep(0, nrow(original)), rep(0, nrow(original))),
    tolerance = 1e-12
  )
  expect_equal(
    vapply(centered$fits_by_imputation,
           function(fit) unname(stats::coef(fit)[["(Intercept)"]]), numeric(1L)),
    vapply(completed, function(data) mean(data$y), numeric(1L)),
    tolerance = 1e-12
  )
})

test_that("native-originated MI linear refits do not reopen and resend the pooled window", {
  skip_if_not_installed("mice")
  original <- data.frame(
    y = c(1, NA, 3, 4, 5, 6),
    x = c(0, 1, 2, 3, 4, 5)
  )
  completed <- list(
    transform(original, y = c(1, 2.1, 3, 4, 5, 6)),
    transform(original, y = c(1, 1.9, 3, 4, 5, 6))
  )
  id <- .make_test_mi_dataset("mi_linear_native_sync", original, completed)
  on.exit(ls_unregister_dataset(id), add = TRUE)

  model <- ls_new_glm(id)
  ls_glm_set_dependent(model, "y")
  ls_glm_add_predictor(model, "x")

  state <- get(".rls_state", envir = asNamespace("LinkEDA"))
  process_started <- state$process_started
  on.exit(state$process_started <- process_started, add = TRUE)
  state$process_started <- TRUE
  pooled_syncs <- 0L
  testthat::local_mocked_bindings(
    ls_analysis_scope = function(group) list(dataset_id=group,kind="all",description="All observations",rows=1:6,n=6L,total_n=6L),
    .rls_glm_sync_native_pooled = function(record) {
      pooled_syncs <<- pooled_syncs + 1L
      TRUE
    },
    .package = "LinkEDA"
  )

  record <- LinkEDA:::.rls_glm_model_record(model)
  expect_false(record$native_sync_enabled)
  model <- ls_glm_fit(model)
  expect_equal(pooled_syncs, 0L)

  record <- LinkEDA:::.rls_glm_model_record(model)
  record$native_sync_enabled <- TRUE
  model <- LinkEDA:::.rls_assign_glm_model(record)
  model <- ls_glm_fit(model)
  expect_equal(pooled_syncs, 1L)
})

test_that("native multiple-imputation synchronization recreates the complete R registry", {
  group <- "native_imputation_sync"
  folder <- tempfile("rlispstat-sync-")
  dir.create(folder)
  payload <- file.path(folder, "dataset.txt")
  writeLines(c(
    "DATASET", group, "3", "2",
    "y", "numeric", "1", "2", "3",
    "g", "factor", "A", "B", "A",
    "IMPUTATION_SPARSE", "multiple_imputation", "native_sync_mi", "source_data",
    "2", "1", "version", "2",
    "y", "1", "2", "NA", "2.1", "1.9",
    "g", "1", "2", "NA", "B", "A"
  ), payload, useBytes = TRUE)
  on.exit({
    if (group %in% ls_datasets()$name) ls_unregister_dataset(group)
    unlink(folder, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  replies <- list()
  testthat::local_mocked_bindings(
    .rls_send = function(lines, expect_reply = TRUE) {
      replies[[length(replies) + 1L]] <<- lines
      "OK"
    },
    .package = "LinkEDA"
  )
  expect_true(LinkEDA:::.rls_handle_r_dataset_sync_needed(c(
    "R_DATASET_SYNC_NEEDED", "sync-request", group, payload
  )))

  record <- LinkEDA:::.rls_dataset_record(group)
  expect_equal(record$dataset_type, "multiple_imputation")
  expect_equal(record$imputation_count, 2L)
  expect_equal(record$imputation_id, "native_sync_mi")
  expect_true(is.na(record$original_data$y[[2L]]))
  expect_equal(vapply(record$completed_datasets, function(data) data$y[[2L]], numeric(1L)),
               c(2.1, 1.9))
  expect_true(record$missing_cell_mask$y[[2L]])
  expect_true(is.na(record$original_data$g[[2L]]))
  expect_identical(
    vapply(record$completed_datasets, function(data) as.character(data$g[[2L]]), character(1L)),
    c("B", "A")
  )
  expect_true(record$missing_cell_mask$g[[2L]])
  expect_true(all(vapply(record$completed_datasets, function(data) is.factor(data$g), logical(1L))))
  expect_true(all(vapply(record$completed_datasets, function(data) identical(levels(data$g), c("A", "B")), logical(1L))))
  expect_equal(levels(record$data$g), c("A", "B"))
  expect_identical(replies[[1L]][[1L]], "R_DATASET_SYNC_RESULT")
})

test_that("MI binary and count regressions cover every available family and link through mice", {
  skip_if_not_installed("mice")
  skip_if_not_installed("broom")
  skip_if_not_installed("MASS")
  n <- 120L
  x <- seq(-1.5, 1.5, length.out = n)
  z <- rep(c(0, 1), length.out = n)
  set.seed(20260827)
  binary_base <- ifelse(
    stats::rbinom(n, 1, stats::plogis(-.2 + .5 * x + .2 * z)) == 1L,
    "event", "reference"
  )
  count_base <- stats::rnbinom(n, mu = exp(.8 + .3 * x + .2 * z), size = 3)
  bounded_base <- stats::rbinom(
    n, size = 10L, prob = stats::plogis(-.25 + .45 * x + .15 * z)
  )
  completed <- lapply(seq_len(5L), function(i) {
    binary <- binary_base
    binary_index <- seq(i + 5L, n, by = 31L)
    binary[binary_index] <- ifelse(binary[binary_index] == "event", "reference", "event")
    count <- count_base
    count_index <- seq(i + 4L, n, by = 29L)
    count[count_index] <- count[count_index] + 1L
    bounded <- bounded_base
    bounded_index <- seq(i + 3L, n, by = 37L)
    bounded[bounded_index] <- pmin(10L, bounded[bounded_index] + 1L)
    data.frame(
      binary = factor(binary, levels = c("reference", "event")),
      count, bounded, x, z
    )
  })
  original <- completed[[1L]]
  original$x[c(9L, 47L)] <- NA_real_
  id <- .make_test_mi_dataset("mi_all_regression_families", original, completed)

  for (link in c("logit", "probit", "cloglog")) {
    base <- ls_new_binary_regression(
      id, response = "binary", terms = "x", link = link,
      event = "event", reference = "reference", native = FALSE
    )
    full <- ls_new_binary_regression(
      id, response = "binary", terms = c("x", "z"), link = link,
      event = "event", reference = "reference", native = FALSE
    )
    state <- ls_binary_regression_state(full)
    expect_equal(state$analysis_backend, "multiple_imputation", info = link)
    expect_true(all(vapply(state$fits_by_imputation, inherits, logical(1L), "glm")), info = link)
    expect_true(all(vapply(state$fits_by_imputation, function(fit) {
      identical(fit$family$family, "binomial") && identical(fit$family$link, link)
    }, logical(1L))), info = link)
    expect_true(all(vapply(seq_along(state$fits_by_imputation), function(i) {
      identical(
        as.integer(stats::model.response(stats::model.frame(state$fits_by_imputation[[i]]))),
        as.integer(completed[[i]]$binary == "event")
      )
    }, logical(1L))), info = link)
    direct <- summary(mice::pool(mice::as.mira(state$fits_by_imputation)), conf.int = TRUE)
    order <- match(state$coefficients$term, as.character(direct$term))
    expect_equal(state$coefficients$estimate, direct$estimate[order], tolerance = 1e-11, info = link)
    expect_equal(state$coefficients$std_error, direct$std.error[order], tolerance = 1e-11, info = link)
    expect_true(all(is.na(unlist(state$summary[c("aic", "bic", "log_lik")]))), info = link)
    binary_payload <- LinkEDA:::.rls_generalized_glm_pooled_native_payload(state)
    expect_true("Binary Model — Multiple Imputation" %in% binary_payload, info = link)
    expect_equal(length(state$diagnostics_by_imputation), 5L, info = link)
    expect_true(all(vapply(seq_along(state$diagnostics_by_imputation), function(i) {
      diagnostics <- state$diagnostics_by_imputation[[i]]
      fit <- state$fits_by_imputation[[i]]
      all(is.finite(diagnostics$dunn_smyth_residual)) &&
        isTRUE(all.equal(
          diagnostics$observed,
          as.numeric(stats::model.response(stats::model.frame(fit))),
          tolerance = 1e-12
        )) &&
        isTRUE(all.equal(
          diagnostics$raw_residual,
          diagnostics$observed - diagnostics$fitted,
          tolerance = 1e-12
        ))
    }, logical(1L))), info = link)
    expect_identical(
      state$summary$diagnostic_capabilities$default_residual,
      "dunn_smyth", info = link
    )
    expect_true("GENERALIZED_MI_DIAGNOSTICS_V4" %in% binary_payload, info = link)
    comparison <- ls_compare_binary_regression_models(base, full, native = FALSE)
    expect_equal(comparison$analysis_backend, "multiple_imputation", info = link)
    expect_match(comparison$tests$method[[1L]], "mice::D1", fixed = TRUE, info = link)
    expect_true(comparison$tests$available[[1L]], info = link)
    expect_match(comparison$tests$reason[[1L]], comparison$tests$method[[1L]], fixed = TRUE, info = link)
  }

  count_cases <- list(
    poisson = list(class = "glm", fitter = "stats::glm", likelihood = TRUE),
    quasipoisson = list(class = "glm", fitter = "stats::glm", likelihood = FALSE),
    negative_binomial = list(class = "negbin", fitter = "MASS::glm.nb", likelihood = TRUE)
  )
  for (distribution in names(count_cases)) {
    case <- count_cases[[distribution]]
    base <- ls_new_count_regression(
      id, response = "count", terms = "x", distribution = distribution, native = FALSE
    )
    full <- ls_new_count_regression(
      id, response = "count", terms = c("x", "z"), distribution = distribution, native = FALSE
    )
    state <- ls_count_regression_state(full)
    expect_equal(state$analysis_backend, "multiple_imputation", info = distribution)
    expect_true(all(vapply(state$fits_by_imputation, inherits, logical(1L), case$class)), info = distribution)
    expect_identical(state$summary$fitter, case$fitter, info = distribution)
    expect_identical(state$summary$ordinary_likelihood_by_imputation, case$likelihood, info = distribution)
    expect_false(state$summary$likelihood_available, info = distribution)
    expect_false(state$summary$pooled_likelihood_available, info = distribution)
    expect_true(all(is.na(unlist(state$summary[c("aic", "bic", "log_lik")]))), info = distribution)
    direct <- summary(mice::pool(mice::as.mira(state$fits_by_imputation)), conf.int = TRUE)
    order <- match(state$coefficients$term, as.character(direct$term))
    expect_equal(state$coefficients$estimate, direct$estimate[order], tolerance = 1e-10, info = distribution)
    expect_equal(state$coefficients$std_error, direct$std.error[order], tolerance = 1e-10, info = distribution)
    count_payload <- LinkEDA:::.rls_generalized_glm_pooled_native_payload(state)
    expect_true("Count Model — Multiple Imputation" %in% count_payload, info = distribution)
    expect_equal(length(state$diagnostics_by_imputation), 5L, info = distribution)
    expect_true(all(vapply(seq_along(state$diagnostics_by_imputation), function(i) {
      diagnostics <- state$diagnostics_by_imputation[[i]]
      fit <- state$fits_by_imputation[[i]]
      isTRUE(all.equal(
        diagnostics$observed,
        as.numeric(stats::model.response(stats::model.frame(fit))),
        tolerance = 1e-12
      )) && isTRUE(all.equal(
        diagnostics$raw_residual,
        diagnostics$observed - diagnostics$fitted,
        tolerance = 1e-12
      ))
    }, logical(1L))), info = distribution)
    if (identical(distribution, "quasipoisson")) {
      expect_true(all(vapply(
        state$diagnostics_by_imputation,
        function(rows) all(is.na(rows$dunn_smyth_residual)), logical(1L)
      )), info = distribution)
      expect_identical(
        state$summary$diagnostic_capabilities$default_residual,
        "pearson", info = distribution
      )
    } else {
      expect_true(all(vapply(
        state$diagnostics_by_imputation,
        function(rows) all(is.finite(rows$dunn_smyth_residual)), logical(1L)
      )), info = distribution)
      expect_identical(
        state$summary$diagnostic_capabilities$default_residual,
        "dunn_smyth", info = distribution
      )
      expect_true(all(vapply(
        state$summary$observed_predicted_distribution_by_imputation,
        function(value) abs(sum(value$predicted_frequency) - nrow(original)) < 1e-7,
        logical(1L)
      )), info = distribution)
    }
    expect_true("GENERALIZED_MI_DIAGNOSTICS_V4" %in% count_payload, info = distribution)
    comparison <- ls_compare_count_regression_models(base, full, native = FALSE)
    expect_equal(comparison$analysis_backend, "multiple_imputation", info = distribution)
    expect_match(comparison$tests$method[[1L]], "mice::D1", fixed = TRUE, info = distribution)
    expect_true(comparison$tests$available[[1L]], info = distribution)
    expect_false(any(comparison$models$has_likelihood), info = distribution)
    if (identical(distribution, "quasipoisson")) {
      expect_false(grepl("D3", comparison$tests$method[[1L]], fixed = TRUE), info = distribution)
    }
  }

  bounded_base_model <- ls_new_count_regression(
    id, response = "bounded", terms = "x",
    distribution = "binomial_trials", trials = 10L, native = FALSE
  )
  bounded_full_model <- ls_new_count_regression(
    id, response = "bounded", terms = c("x", "z"),
    distribution = "binomial_trials", trials = 10L, native = FALSE
  )
  bounded_state <- ls_count_regression_state(bounded_full_model)
  expect_identical(bounded_state$analysis_backend, "multiple_imputation")
  expect_length(bounded_state$fits_by_imputation, 5L)
  expect_true(all(vapply(
    bounded_state$fits_by_imputation, inherits, logical(1L), "glm"
  )))
  expect_true(all(vapply(seq_along(bounded_state$fits_by_imputation), function(i) {
    isTRUE(all.equal(
      unname(stats::model.response(stats::model.frame(
        bounded_state$fits_by_imputation[[i]]
      ))),
      unname(cbind(completed[[i]]$bounded, 10L - completed[[i]]$bounded)),
      check.attributes = FALSE
    ))
  }, logical(1L))))
  bounded_direct <- summary(mice::pool(
    mice::as.mira(bounded_state$fits_by_imputation)
  ), conf.int = TRUE)
  bounded_order <- match(
    bounded_state$coefficients$term, as.character(bounded_direct$term)
  )
  expect_equal(
    bounded_state$coefficients$estimate,
    bounded_direct$estimate[bounded_order], tolerance = 1e-10
  )
  expect_equal(
    bounded_state$coefficients$std_error,
    bounded_direct$std.error[bounded_order], tolerance = 1e-10
  )
  expect_length(bounded_state$summary$binomial_overdispersion_by_imputation, 5L)
  expect_length(bounded_state$diagnostics_by_imputation, 5L)
  expect_true(all(vapply(
    bounded_state$diagnostics_by_imputation,
    function(rows) {
      all(is.finite(rows$dunn_smyth_residual)) &&
        isTRUE(all.equal(
          rows$raw_residual, rows$observed - rows$fitted,
          tolerance = 1e-12
        ))
    }, logical(1L)
  )))
  expect_identical(
    bounded_state$summary$diagnostic_capabilities$default_residual,
    "dunn_smyth"
  )
  expect_true(all(vapply(
    bounded_state$summary$observed_predicted_distribution_by_imputation,
    function(value) abs(sum(value$predicted_frequency) - nrow(original)) < 1e-7,
    logical(1L)
  )))
  expect_true(all(is.na(unlist(
    bounded_state$summary[c("aic", "bic", "log_lik")]
  ))))
  bounded_comparison <- ls_compare_count_regression_models(
    bounded_base_model, bounded_full_model, native = FALSE
  )
  expect_true(bounded_comparison$tests$available[[1L]])
  expect_match(bounded_comparison$tests$method[[1L]], "mice::D1", fixed = TRUE)
  captured <- NULL
  testthat::local_mocked_bindings(
    .rls_send = function(lines, expect_reply = TRUE) {
      captured <<- lines
      "OK"
    },
    .rls_send_winui = function(lines, expect_reply = TRUE) {
      captured <<- lines
      "OK"
    },
    .package = "LinkEDA"
  )
  native_state <- LinkEDA:::.rls_state
  previous_started <- native_state$process_started
  on.exit(native_state$process_started <- previous_started, add = TRUE)
  native_state$process_started <- TRUE
  visible <- ls_compare_count_regression_models(
    base, full, native = TRUE, .native_id = "mi_count_comparison_protocol"
  )
  expect_equal(visible$analysis_backend, "multiple_imputation")
  expect_true("GENERALIZED_COMPARISON_MODELS_V4" %in% captured)
  expect_true(visible$tests$method[[1L]] %in% captured)
})

test_that("MI model comparisons use completed scalar data and common original row identities", {
  skip_if_not_installed("mice")
  skip_if_not_installed("broom")
  n <- 40L
  original <- data.frame(
    y_bin = factor(rep(c("0", "1"), length.out = n), levels = c("0", "1")),
    x = seq(-1, 1, length.out = n),
    g = factor(rep(c("A", "B"), length.out = n))
  )
  original$x[c(5L, 19L)] <- NA_real_
  completed <- lapply(seq_len(3L), function(i) {
    data <- original
    data$x[c(5L, 19L)] <- c(-.35, .42) + i * .01
    data$y_bin[[11L]] <- NA
    data
  })
  id <- .make_test_mi_dataset("mi_comparison_completed_source", original, completed)
  dataset <- LinkEDA:::.rls_dataset_record(id)

  # The displayed compact preview is deliberately invalid as a numeric model
  # column.  Statistical fitting must still consume completed_datasets.
  dataset$data$x <- rep("-0.2 | 0.1 | 0.4", n)
  LinkEDA:::.rls_set_dataset_record(dataset)
  specs <- list(
    list(response = "y_bin", terms = "x", term_types = list(x = "numeric"),
         factor_reference_levels = list(), scope = "all", count_regression = FALSE,
         exposure = ""),
    list(response = "y_bin", terms = c("x", "g"),
         term_types = list(x = "numeric", g = "factor"),
         factor_reference_levels = list(g = "A"), scope = "all",
         count_regression = FALSE, exposure = "")
  )
  common <- LinkEDA:::.rls_mi_common_model_rows(dataset, specs)
  expect_identical(common, setdiff(seq_len(n), 11L))
  expect_true(all(c(5L, 19L) %in% common))

  base <- ls_new_binary_regression(
    id, "y_bin", "x", event = "1", reference = "0", native = FALSE,
    term_types = list(x = "numeric"), .comparison_rows = common
  )
  full <- ls_new_binary_regression(
    id, "y_bin", c("x", "g"), event = "1", reference = "0", native = FALSE,
    term_types = list(x = "numeric", g = "factor"),
    factor_reference_levels = list(g = "A"), .comparison_rows = common
  )
  comparison <- ls_compare_binary_regression_models(base, full, native = FALSE)
  expect_true(comparison$common_rows)
  expect_length(comparison$rows_used_by_imputation, 3L)
  expect_true(all(vapply(
    comparison$rows_used_by_imputation, identical, logical(1L), common
  )))
  expect_true(comparison$tests$available[[1L]])
  expect_match(comparison$tests$method[[1L]], "mice::D1", fixed = TRUE)
})

test_that("MI Count Regression completes the 400-row five-imputation native task", {
  skip_if_not_installed("mice")
  skip_if_not_installed("broom")
  skip_if_not_installed("MASS")
  n <- 400L
  set.seed(20260828)
  x <- stats::rnorm(n)
  z <- rep(c(0, 1), length.out = n)
  g <- factor(rep(c("A", "B", "C"), length.out = n),
              levels = c("A", "B", "C"))
  g_effect <- c(A = 0, B = .35, C = -.2)[as.character(g)]
  y_count <- stats::rpois(n, lambda = exp(.7 + .25 * x - .15 * z + g_effect))
  completed <- lapply(seq_len(5L), function(i) {
    data.frame(
      y_count = y_count,
      x = x + (i - 3L) * .005,
      z = z,
      g = g
    )
  })
  original <- completed[[1L]]
  original$x[seq(17L, n, by = 53L)] <- NA_real_
  id <- .make_test_mi_dataset("mi_count_native_roundtrip", original, completed)
  dataset <- LinkEDA:::.rls_dataset_record(id)
  # The compact all-imputation preview is deliberately unusable as numeric
  # fitting data. Count Regression must take every fit from completed_datasets.
  dataset$data$x <- rep("-0.2 | 0.1 | 0.4", n)
  dataset$data$z <- rep("0 | 1 | 0", n)
  LinkEDA:::.rls_set_dataset_record(dataset)

  elapsed <- numeric()
  states <- list()
  trace <- testthat::capture_messages({
    old_debug <- getOption("LinkEDA.mi_debug")
    options(LinkEDA.mi_debug = TRUE)
    on.exit(options(LinkEDA.mi_debug = old_debug), add = TRUE)
    for (distribution in c("poisson", "quasipoisson", "negative_binomial")) {
      timing <- system.time({
        model <- ls_new_count_regression(
          id, response = "y_count", terms = c("x", "z", "g"),
          distribution = distribution, native = FALSE,
          term_types = list(x = "numeric", z = "numeric", g = "factor"),
          factor_reference_levels = list(g = "A")
        )
        states[[distribution]] <- ls_count_regression_state(model)
      })
      elapsed[[distribution]] <- unname(timing[["elapsed"]])
    }
  })
  expect_true(any(grepl("backend selected: multiple_imputation", trace, fixed = TRUE)))
  expect_true(any(grepl("imputation 5/5: fitted", trace, fixed = TRUE)))
  expect_true(any(grepl("pooling: complete mice::pool", trace, fixed = TRUE)))
  expect_true(any(grepl("semantic result: complete", trace, fixed = TRUE)))
  expect_true(all(elapsed < 10), info = paste(names(elapsed), elapsed, collapse = "; "))
  for (distribution in names(states)) {
    state <- states[[distribution]]
    expect_identical(state$analysis_backend, "multiple_imputation", info = distribution)
    expect_length(state$fits_by_imputation, 5L)
    expect_true(all(vapply(state$fits_by_imputation, function(fit) !is.null(fit), logical(1L))),
                info = distribution)
    expect_identical(state$summary$df_residual, 395, info = distribution)
    expect_identical(state$summary$parameter_count, 5L, info = distribution)
    expect_identical(state$coefficients$term,
                     c("(Intercept)", "x", "z", "gB", "gC"),
                     info = distribution)
    expect_true(all(vapply(state$fits_by_imputation, function(fit) {
      inherits(stats::model.frame(fit)$g, "factor") &&
        identical(levels(stats::model.frame(fit)$g), c("A", "B", "C")) &&
        identical(colnames(stats::model.matrix(fit)),
                  c("(Intercept)", "x", "z", "gB", "gC")) &&
        identical(names(stats::coef(fit)),
                  c("(Intercept)", "x", "z", "gB", "gC")) &&
        identical(stats::df.residual(fit), 395L)
    }, logical(1L))), info = distribution)
    expect_identical(
      state$coefficient_rows$row_type[state$coefficient_rows$source_term == "g"],
      c("factor_parent", "reference", "factor_level", "factor_level"),
      info = distribution
    )
    expect_match(state$multiple_imputation$pooling_method, "mice::pool", fixed = TRUE,
                 info = distribution)
  }

  nb_state <- states$negative_binomial
  theta_values <- vapply(
    nb_state$fits_by_imputation,
    function(fit) fit$theta,
    numeric(1L)
  )
  expect_equal(nb_state$summary$theta_by_imputation, theta_values)
  expect_equal(nb_state$summary$theta_descriptive_mean, mean(theta_values))
  expect_equal(nb_state$summary$theta_descriptive_min, min(theta_values))
  expect_equal(nb_state$summary$theta_descriptive_max, max(theta_values))
  expect_true(is.na(nb_state$summary$theta))
  expect_true(is.na(nb_state$summary$dispersion))
  expect_match(nb_state$summary$fit_information_method, "not Rubin-pooled", fixed = TRUE)
  expect_false(any(grepl("quasi-Poisson", nb_state$multiple_imputation$warnings,
                         fixed = TRUE)))
  nb_fit_rows <- LinkEDA:::.rls_generalized_glm_fit_rows(nb_state$summary)
  expect_true("Theta — mean across imputations (not Rubin-pooled)" %in% nb_fit_rows$label)
  expect_true("Theta range across imputations" %in% nb_fit_rows$label)
  expect_false("Dispersion" %in% nb_fit_rows$label)

  nb_payload <- LinkEDA:::.rls_generalized_glm_pooled_native_payload(nb_state)
  count_marker <- match(TRUE, nb_payload %in% c("COUNT_V6", "COUNT_V7"))
  expect_false(is.na(count_marker))
  expect_equal(as.numeric(nb_payload[[count_marker + 8L]]), mean(theta_values))
  expect_equal(as.numeric(nb_payload[[count_marker + 9L]]), min(theta_values))
  expect_equal(as.numeric(nb_payload[[count_marker + 10L]]), max(theta_values))

  native_state <- states$poisson
  native_state$id <- "mi_count_native_task"
  native_state$native_generation <- 41L
  pooled_payload <- LinkEDA:::.rls_generalized_glm_pooled_native_payload(native_state)
  expect_identical(pooled_payload[[1L]], "GENERALIZED_GLM_OPEN_POOLED")
  expect_true("Count Model — Multiple Imputation" %in% pooled_payload)
  expect_true("ANALYSIS_SCOPE_V1" %in% pooled_payload)
  expect_true("GENERALIZED_MI_DIAGNOSTICS_V4" %in% pooled_payload)
  result_marker <- match("GGLM_RESULT_V1", pooled_payload)
  expect_false(is.na(result_marker))
  expect_identical(pooled_payload[result_marker + 0:1],
                   c("GGLM_RESULT_V1", "41"))
})

test_that("MI regression comparison pools coefficients and uses sequential nested tests", {
  original <- data.frame(
    y = c(2.1, NA, 4.9, 6.2, 8.1, NA, 11.8, 13.0),
    x = c(0.5, 1.0, 1.8, 2.5, 3.1, 3.8, 4.5, 5.2),
    w = c(1, 0, 1, 0, 1, 0, 1, 0),
    z = c(0.2, 1.4, 0.8, 2.1, 1.2, 2.8, 2.0, 3.3)
  )
  completed <- list(
    transform(original, y = c(2.1, 3.2, 4.9, 6.2, 8.1, 9.7, 11.8, 13.0)),
    transform(original, y = c(2.1, 3.5, 4.9, 6.2, 8.1, 10.2, 11.8, 13.0))
  )
  id <- .make_test_mi_dataset("mi_regcmp", original, completed)
  cmp <- ls_new_regression_comparison(
    id,
    response = "y",
    models = list(Base = "x", Full = c("x", "w", "z")),
    native = FALSE
  )
  state <- ls_regression_comparison_state(cmp)
  expect_equal(state$analysis_backend, "multiple_imputation")
  expect_equal(state$models[[2L]]$terms, c("x", "w", "z"))
  expect_true(all(c("x", "w", "z") %in% state$models[[2L]]$coefficients$term))
  expect_true(all(is.finite(state$models[[2L]]$coefficients$estimate)))
  expect_gt(state$models[[2L]]$summary$n_used, 0L)
  expect_true(all(vapply(state$models, function(model) length(model$fits_by_imputation) == 2L, logical(1L))))
  expect_true(all(vapply(
    state$models,
    function(model) length(model$diagnostics_by_imputation) == 2L,
    logical(1L)
  )))
  expect_equal(state$model_comparison_tests$status, "ok")
  expect_equal(state$model_comparison_tests$comparisons[[1L]]$status, "not_applicable")
  expect_equal(state$model_comparison_tests$comparisons[[2L]]$status, "ok")
  expect_match(state$model_comparison_tests$comparisons[[2L]]$method, "mice::D[13]")
  expect_true(is.finite(state$model_comparison_tests$comparisons[[2L]]$F))
  expect_true(is.finite(state$model_comparison_tests$comparisons[[2L]]$p))
  expect_equal(
    state$models[[2L]]$summary$comparison_p,
    state$model_comparison_tests$comparisons[[2L]]$p
  )

  # A model fitted in the comparison must be numerically identical to the
  # same specification fitted in the standalone General Linear Model window.
  standalone <- ls_new_glm(id)
  ls_glm_set_dependent(standalone, "y")
  ls_glm_add_predictor(standalone, "x")
  standalone <- ls_glm_fit(standalone)
  standalone_coefficients <- ls_glm_coefficients(standalone)
  comparison_coefficients <- state$models[[1L]]$coefficients
  common_terms <- intersect(
    standalone_coefficients$term,
    comparison_coefficients$term
  )
  standalone_coefficients <- standalone_coefficients[
    match(common_terms, standalone_coefficients$term),
    c("term", "estimate", "std_error", "p_value")
  ]
  comparison_coefficients <- comparison_coefficients[
    match(common_terms, comparison_coefficients$term),
    c("term", "estimate", "std_error", "p_value")
  ]
  expect_equal(
    standalone_coefficients,
    comparison_coefficients,
    tolerance = 1e-12
  )
  expect_match(state$model_comparison_tests$detail, "No statistics or p-values are averaged")
  state$request_generation <- 9L
  update_payload <- LinkEDA:::.rls_regcmp_native_update_payload(state)
  expect_equal(unname(update_payload[seq_len(4L)]), c("REGCMP_UPDATE", state$id, "9", state$group))
  expect_true("TRUE" %in% update_payload)
  expect_true("LINEAR_MI_DIAGNOSTICS_V1" %in% update_payload)
  payload <- LinkEDA:::.rls_regcmp_pooled_native_payload(state)
  expect_equal(payload[[1L]], "REGCMP_OPEN_POOLED")
  expect_true("Compare Linear Models — Multiple Imputation" %in% payload)
  expect_true("LINEAR_MI_DIAGNOSTICS_V1" %in% payload)
  expect_true(any(grepl(state$model_comparison_tests$comparisons[[2L]]$method, payload, fixed = TRUE)))
})

test_that("native MI regression-comparison task completes its first fit", {
  skip_if_not_installed("mice")
  skip_if_not_installed("broom")

  original <- data.frame(
    y = c(2.1, NA, 4.8, 6.0, 7.9, NA, 11.7, 12.9),
    x = c(0.5, 1.0, 1.8, 2.5, 3.1, 3.8, 4.5, 5.2)
  )
  completed <- list(
    transform(original, y = c(2.1, 3.1, 4.8, 6.0, 7.9, 9.6, 11.7, 12.9)),
    transform(original, y = c(2.1, 3.4, 4.8, 6.0, 7.9, 10.0, 11.7, 12.9))
  )
  dataset_id <- .make_test_mi_dataset(
    "mi_regcmp_native_first_fit", original, completed
  )
  on.exit(ls_unregister_dataset(dataset_id), add = TRUE)
  dataset <- LinkEDA:::.rls_dataset_record(dataset_id)
  comparison_id <- "mi_regcmp_native_first_fit_task"
  on.exit(rm(
    list = comparison_id,
    envir = LinkEDA:::.rls_state$regression_comparisons
  ), add = TRUE)

  parts <- c(
    "REGCMP_NEEDED", comparison_id, "1", dataset_id, "y", "all", "TRUE",
    "2", "(Intercept)", "x",
    "1", "x", "numeric",
    "1",
    paste0(comparison_id, ":model:1"), "Model 1", "y",
    "1", "x",
    "1", "x", "numeric",
    "0", # centered predictors
    "0", # factor reference levels
    "0", # selected rows
    "REGCMP_DATASET_V1", dataset$dataset_type, dataset$imputation_id,
    dataset$source_dataset_id, as.character(dataset$imputation_count)
  )

  expect_no_error(LinkEDA:::.rls_handle_regcmp_needed(parts))
  expect_true(exists(
    comparison_id,
    envir = LinkEDA:::.rls_state$regression_comparisons,
    inherits = FALSE
  ))
  state <- get(
    comparison_id,
    envir = LinkEDA:::.rls_state$regression_comparisons,
    inherits = FALSE
  )
  expect_identical(state$request_generation, 1L)
  expect_false(is.null(state$models[[1L]]$fit))
  expect_gt(state$models[[1L]]$summary$n_used, 0L)
  expect_false(state$models[[1L]]$is_stale)
})

test_that("MI regression comparison skips Rubin pooling when all model inputs are observed", {
  original <- data.frame(
    y = c(2.1, 3.0, 4.9, 6.2, 8.1, 9.6, 11.8, 13.0),
    x = c(0.5, 1.0, 1.8, 2.5, 3.1, 3.8, 4.5, 5.2),
    w = c(1, 0, 1, 0, 1, 0, 1, 0),
    irrelevant = c(NA, 2, 3, 4, 5, 6, 7, 8)
  )
  completed <- list(
    transform(original, irrelevant = c(1.1, 2, 3, 4, 5, 6, 7, 8)),
    transform(original, irrelevant = c(1.9, 2, 3, 4, 5, 6, 7, 8))
  )
  id <- .make_test_mi_dataset("mi_regcmp_fully_observed_inputs", original, completed)
  on.exit(ls_unregister_dataset(id), add = TRUE)
  cmp <- ls_new_regression_comparison(
    id,
    response = "y",
    models = list(Base = "x", Full = c("x", "w")),
    native = FALSE
  )
  state <- ls_regression_comparison_state(cmp)
  reference <- stats::lm(y ~ x + w, data = completed[[1L]])

  expect_false(state$mi_pooling_required)
  expect_length(state$mi_imputed_input_variables, 0L)
  expect_true(all(vapply(state$models, function(model) {
    is.null(model$fits_by_imputation)
  }, logical(1L))))
  expect_equal(
    setNames(state$models[[2L]]$coefficients$estimate,
             state$models[[2L]]$coefficients$term),
    stats::coef(reference),
    tolerance = 1e-10
  )
  expect_match(state$model_comparison_tests$method, "Rubin pooling not required", fixed = TRUE)
  expect_match(state$model_comparison_tests$detail, "Rubin's rules were not applied", fixed = TRUE)
  payload <- LinkEDA:::.rls_regcmp_pooled_native_payload(state)
  expect_true(any(grepl("Rubin's rules were not required or applied", payload, fixed = TRUE)))
  expect_false(any(grepl("coefficients use mice::pool", payload, fixed = TRUE)))
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
  expect_equal(imputation_section[5:10], c("NA", "NA", "11", "21", "12", "22"))
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

test_that("new log-link and lognormal models fit and pool every imputation in R", {
  skip_if_not_installed("mice")
  skip_if_not_installed("broom")
  n <- 200L
  group <- factor(rep(c("A", "B"), each = n / 2L), levels = c("A", "B"))
  x <- rep(seq(-1, 1, length.out = n / 2L), 2L)
  binary <- factor(
    c(rep(c("no", "yes"), c(80, 20)), rep(c("no", "yes"), c(60, 40))),
    levels = c("no", "yes")
  )
  base <- data.frame(
    gaussian_y = exp(.7 + .2 * x + .15 * (group == "B")),
    lognormal_y = exp(.5 + .3 * x + .2 * (group == "B")),
    binary = binary, x = x, group = group
  )
  completed <- list(
    transform(base,
      gaussian_y = gaussian_y * exp(.01 * sin(seq_len(n))),
      lognormal_y = lognormal_y * exp(.02 * cos(seq_len(n)))
    ),
    transform(base,
      gaussian_y = gaussian_y * exp(-.01 * cos(seq_len(n))),
      lognormal_y = lognormal_y * exp(-.02 * sin(seq_len(n)))
    )
  )
  original <- base
  original$gaussian_y[c(5L, 105L)] <- NA_real_
  original$lognormal_y[c(6L, 106L)] <- NA_real_
  original$binary[c(7L, 107L)] <- NA
  id <- .make_test_mi_dataset("mi_log_model_extensions", original, completed)
  on.exit(ls_unregister_dataset(id), add = TRUE)

  gaussian <- ls_new_positive_continuous_model(
    id, "gaussian_y", c("x", "group", "x:group"),
    distribution = "gaussian_log", native = FALSE,
    name = "mi_gaussian_log"
  )
  gaussian_state <- ls_generalized_linear_model_state(gaussian)
  expect_length(gaussian_state$fits_by_imputation, 2L)
  expect_true(all(vapply(gaussian_state$fits_by_imputation, function(fit) {
    inherits(fit, "glm") && identical(fit$family$family, "gaussian") &&
      identical(fit$family$link, "log")
  }, logical(1L))))
  expect_identical(unique(gaussian_state$coefficients$exponentiated_label),
                   "Mean ratio")

  lognormal <- ls_new_positive_continuous_model(
    id, "lognormal_y", c("x", "group", "x:group"),
    distribution = "lognormal", native = FALSE,
    name = "mi_lognormal"
  )
  lognormal_state <- ls_generalized_linear_model_state(lognormal)
  expect_length(lognormal_state$fits_by_imputation, 2L)
  expect_true(all(vapply(lognormal_state$fits_by_imputation, function(fit) {
    inherits(fit, "lm") && !inherits(fit, "glm") &&
      grepl("log\\(", deparse(stats::formula(fit))[[1L]])
  }, logical(1L))))
  expect_identical(unique(lognormal_state$coefficients$exponentiated_label),
                   "Multiplicative ratio")
  expect_match(lognormal_state$summary$fit_information_method,
               "arithmetic means", fixed = TRUE)

  binomial <- suppressWarnings(ls_new_binary_regression(
    id, "binary", "group", event = "yes", link = "log",
    native = FALSE, name = "mi_log_binomial"
  ))
  binomial_state <- ls_binary_regression_state(binomial)
  expect_length(binomial_state$fits_by_imputation, 2L)
  expect_true(all(vapply(binomial_state$fits_by_imputation, function(fit) {
    inherits(fit, "glm") && identical(fit$family$family, "binomial") &&
      identical(fit$family$link, "log")
  }, logical(1L))))
  expect_identical(unique(binomial_state$coefficients$exponentiated_label),
                   "Risk ratio")

  if (requireNamespace("emmeans", quietly = TRUE)) {
    gaussian_effect <- ls_generalized_linear_model_interaction(gaussian, "group")
    expect_true(all(is.finite(gaussian_effect$plot_data$pooled_estimate)))
    expect_true(all(gaussian_effect$plot_data$pooled_estimate > 0))

    lognormal_effect <- ls_generalized_linear_model_interaction(lognormal, "group")
    expect_identical(unique(lognormal_effect$plot_data$response_scale),
                     "arithmetic_mean")
    expect_identical(
      unique(lognormal_effect$plot_data$back_transformation),
      "exp(eta + sigma^2 / 2) within each imputation"
    )

    binomial_effect <- ls_binary_regression_interaction(binomial, "group")
    expect_identical(unique(binomial_effect$plot_data$quantity),
                     "predicted_probability")
    expect_true(all(binomial_effect$plot_data$pooled_estimate >= 0 &
                    binomial_effect$plot_data$pooled_estimate <= 1))
  }
})
