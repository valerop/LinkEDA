library(LinkEDA)

test_that("the first native generalized, binary, and count fits publish once", {
  id <- ls_register_dataset("initial_native_generalized_data", mtcars)
  on.exit(ls_unregister_dataset(id), add = TRUE)
  state <- LinkEDA:::.rls_state
  previous_started <- state$process_started
  on.exit(state$process_started <- previous_started, add = TRUE)
  state$process_started <- TRUE
  publications <- character()
  testthat::local_mocked_bindings(
    .rls_start_backend = function() invisible(NULL),
    ls_analysis_scope = function(group) {
      rows <- seq_len(nrow(LinkEDA:::.rls_dataset_record(group)$data))
      list(dataset_id = group, kind = "all", source = "all_data",
           description = "All observations", rows = rows,
           n = length(rows), total_n = length(rows))
    },
    .rls_generalized_glm_sync_native = function(record) {
      publications <<- c(publications, record$id)
      invisible(TRUE)
    },
    .package = "LinkEDA"
  )
  generalized <- ls_new_generalized_linear_model(
    id, response = "mpg", terms = "wt", family = "gaussian",
    link = "identity", name = "initial_generalized_sync", native = TRUE
  )
  binary <- ls_new_binary_regression(
    id, response = "am", terms = "wt", name = "initial_binary_sync",
    native = TRUE
  )
  count <- ls_new_count_regression(
    id, response = "cyl", terms = "wt", distribution = "poisson",
    name = "initial_count_sync", native = TRUE
  )
  expect_identical(publications, c(generalized$id, binary$id, count$id))
})

test_that("the first native MI fit emits completed timing and publishes once", {
  skip_if_not_installed("mice")
  skip_if_not_installed("broom")
  source <- data.frame(x = seq_len(12L), y = 3 + seq_len(12L) / 2)
  original <- source
  original$y[[2L]] <- NA_real_
  completed <- list(source, transform(source, y = replace(y, 2L, 4.2)))
  id <- ls_register_dataset("initial_mi_generalized_sync", source)
  on.exit(ls_unregister_dataset(id), add = TRUE)
  dataset <- LinkEDA:::.rls_dataset_record(id)
  dataset$dataset_type <- "multiple_imputation"
  dataset$imputation_id <- paste0(id, "_imp")
  dataset$imputation_count <- length(completed)
  dataset$completed_datasets <- completed
  dataset$original_data <- original
  dataset$original_row_ids <- seq_len(nrow(original))
  dataset$missing_cell_mask <- as.data.frame(lapply(original, is.na))
  LinkEDA:::.rls_set_dataset_record(dataset)

  state <- LinkEDA:::.rls_state
  previous_started <- state$process_started
  previous_parallel <- getOption("LinkEDA.mi_parallel")
  on.exit({
    state$process_started <- previous_started
    options(LinkEDA.mi_parallel = previous_parallel)
  }, add = TRUE)
  state$process_started <- TRUE
  options(LinkEDA.mi_parallel = FALSE)
  publications <- 0L
  phases <- character()
  messages <- character()
  testthat::local_mocked_bindings(
    .rls_start_backend = function() invisible(NULL),
    ls_analysis_scope = function(group) {
      rows <- seq_len(nrow(LinkEDA:::.rls_dataset_record(group)$data))
      list(dataset_id = group, kind = "all", source = "all_data",
           description = "All observations", rows = rows,
           n = length(rows), total_n = length(rows))
    },
    .rls_mi_progress_event = function(task_id, group, phase, status = "running", ..., message = "") {
      phases <<- c(phases, phase)
      messages <<- c(messages, message)
      invisible(NULL)
    },
    .rls_generalized_glm_sync_native_pooled = function(record) {
      publications <<- publications + 1L
      list(backend_startup_seconds = 0, dataset_preparation_seconds = 0,
           dataset_delivery_seconds = 0, model_preparation_seconds = 0,
           model_delivery_seconds = 0)
    },
    .package = "LinkEDA"
  )
  ls_new_generalized_linear_model(
    id, response = "y", terms = "x", family = "gaussian",
    link = "identity", name = "initial_mi_native_sync", native = TRUE
  )
  expect_identical(publications, 1L)
  expect_true("transferring" %in% phases)
  expect_true("completed" %in% phases)
  expect_match(messages[[which(phases == "completed")[[1L]]]],
               "R fit .* report .* data sync .* result send/draw")
})
