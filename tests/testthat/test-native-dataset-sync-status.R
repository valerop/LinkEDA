library(LinkEDA)

test_that("unchanged native datasets are not transferred again for pooled models", {
  id <- ls_register_dataset("native_dataset_sync_status", data.frame(x = 1:3))
  on.exit(ls_unregister_dataset(id), add = TRUE)
  dataset <- LinkEDA:::.rls_dataset_record(id)
  sent <- character()
  testthat::local_mocked_bindings(
    .rls_start_backend = function() invisible(TRUE),
    .rls_send = function(lines, ...) {
      sent <<- c(sent, lines[[1L]])
      if (identical(lines[[1L]], "DATASET_SYNC_STATUS")) {
        return(paste("OK", "present", "1", "3", "1", "data_frame", "", "0",
                     "1", "version", "", LinkEDA:::.rls_state$sync_session_token,
                     "END", sep = "\t"))
      }
      "OK"
    },
    .rls_generalized_glm_pooled_native_payload = function(record) "MODEL_RESULT",
    .rls_variable_payload = function(...) stop("unchanged dataset was serialized"),
    .rls_dataframe_payload = function(...) stop("unchanged dataset was serialized"),
    .package = "LinkEDA"
  )
  LinkEDA:::.rls_generalized_glm_sync_native_pooled(
    list(group = id, data = dataset$data))
  expect_identical(sent, c("DATASET_SYNC_STATUS", "MODEL_RESULT"))
})

test_that("a changed or absent native dataset is transferred before the model", {
  id <- ls_register_dataset("native_dataset_sync_changed", data.frame(x = 1:3))
  on.exit(ls_unregister_dataset(id), add = TRUE)
  dataset <- LinkEDA:::.rls_dataset_record(id)
  sent <- character()
  testthat::local_mocked_bindings(
    .rls_start_backend = function() invisible(TRUE),
    .rls_send = function(lines, ...) {
      sent <<- c(sent, lines[[1L]])
      if (identical(lines[[1L]], "DATASET_SYNC_STATUS")) return("OK\tmissing")
      "OK"
    },
    .rls_generalized_glm_pooled_native_payload = function(record) "MODEL_RESULT",
    .rls_variable_payload = function(...) "VARS",
    .rls_dataframe_payload = function(...) "DATAFRAME",
    .package = "LinkEDA"
  )
  LinkEDA:::.rls_generalized_glm_sync_native_pooled(
    list(group = id, data = dataset$data))
  expect_identical(sent, c("DATASET_SYNC_STATUS", "REGISTER_DATASET_SILENT", "MODEL_RESULT"))
})

test_that("a dataset from a previous R session cannot be reused", {
  id <- ls_register_dataset("native_dataset_sync_old_session", data.frame(x = 1:3))
  on.exit(ls_unregister_dataset(id), add = TRUE)
  dataset <- LinkEDA:::.rls_dataset_record(id)
  testthat::local_mocked_bindings(
    .rls_send = function(...) paste("OK", "present", "1", "3", "1",
                                   "data_frame", "", "0", "1", "version", "",
                                   "another-R-session", "END", sep = "\t"),
    .package = "LinkEDA"
  )
  expect_false(LinkEDA:::.rls_native_dataset_matches(dataset, dataset$data))
})

test_that("shared model registration reuses a current dataset and still opens its sheet", {
  id <- ls_register_dataset("shared_native_dataset_sync", data.frame(x = 1:3))
  on.exit(ls_unregister_dataset(id), add = TRUE)
  dataset <- LinkEDA:::.rls_dataset_record(id)
  sent <- character()
  sender <- function(lines) {
    sent <<- c(sent, lines[[1L]])
    if (identical(lines[[1L]], "DATASET_SYNC_STATUS")) {
      return(paste("OK", "present", "1", "3", "1", "data_frame", "", "0",
                   "1", "version", "", LinkEDA:::.rls_state$sync_session_token,
                   "END", sep = "\t"))
    }
    "OK"
  }
  testthat::local_mocked_bindings(
    .rls_variable_payload = function(...) stop("unchanged dataset was serialized"),
    .rls_dataframe_payload = function(...) stop("unchanged dataset was serialized"),
    .package = "LinkEDA"
  )
  expect_false(LinkEDA:::.rls_register_native_dataset_if_needed(
    dataset, sender = sender))
  expect_identical(sent, "DATASET_SYNC_STATUS")
  sent <- character()
  expect_false(LinkEDA:::.rls_register_native_dataset_if_needed(
    dataset, visible = TRUE, sender = sender))
  expect_identical(sent, c("DATASET_SYNC_STATUS", "DATA_OPEN_DATA_SHEET"))
})

test_that("shared registration refuses to overwrite a newer native edit", {
  id <- ls_register_dataset("shared_native_dataset_stale", data.frame(x = 1:3))
  on.exit(ls_unregister_dataset(id), add = TRUE)
  dataset <- LinkEDA:::.rls_dataset_record(id)
  sent <- character()
  sender <- function(lines) {
    sent <<- c(sent, lines[[1L]])
    if (identical(lines[[1L]], "DATASET_SYNC_STATUS")) {
      return(paste("OK", "present", "2", "3", "1", "data_frame", "", "0",
                   "1", "version", "", LinkEDA:::.rls_state$sync_session_token,
                   "END", sep = "\t"))
    }
    "OK"
  }
  expect_error(LinkEDA:::.rls_register_native_dataset_if_needed(
    dataset, sender = sender), "data sheet changed")
  expect_identical(sent, "DATASET_SYNC_STATUS")
})

test_that("every macOS cell-edit route applies the complete linked-analysis refresh", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")

  expect_match(mac, "static void ApplyDatasetValueChangeEffectsOnMac", fixed = TRUE)
  expect_match(mac,
    "ApplyDatasetValueChangeEffectsOnMac(\n        group, effects, plotsToRedraw, syncQueued)",
    fixed = TRUE
  )
  expect_match(mac,
    "ApplyDatasetValueChangeEffectsOnMac(\n                        event.group, event.valueChangeEffects)",
    fixed = TRUE
  )

  helper_start <- regexpr(
    "static void ApplyDatasetValueChangeEffectsOnMac(", mac, fixed = TRUE
  )[[1]]
  expect_gt(helper_start, 0L)
  helper_tail <- substr(mac, helper_start, nchar(mac))
  helper_end <- regexpr(
    "static bool SetDataFrameCellValueForGroup", helper_tail, fixed = TRUE
  )[[1]]
  expect_gt(helper_end, 0L)
  helper <- substr(helper_tail, 1L, helper_end - 1L)

  for (effect in c(
    "correlationIds", "dendrogramIds", "dimensionalityIds",
    "scaleAnalysisIds", "generalizedGlmIdsToRefit", "mixedModelIds",
    "regressionComparisonIds", "generalizedComparisonIds", "modelTrellisIds"
  )) {
    expect_match(helper, paste0("effects.", effect), fixed = TRUE)
  }
  expect_match(helper, "state->second.autoRefit", fixed = TRUE)
  expect_match(helper, "state->autoFit", fixed = TRUE)
  expect_match(helper, "RefitOpenTable1WindowsOnMain(group, false)", fixed = TRUE)
  expect_match(helper,
    "RefreshMeanComparisonSelectionScopesOnMain(group, true)", fixed = TRUE
  )
  expect_match(helper, "ShowScaleAnalysisOnMain(id)", fixed = TRUE)
  expect_match(helper, "RefreshGLMDiagnosticPlotsSynchronouslyOnMain", fixed = TRUE)

  expect_match(mac,
    "return pending.state.id == request.id;",
    fixed = TRUE
  )
})

test_that("global scope changes reach native tables and mixed models", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")

  start <- regexpr("static void RefitOpenAnalysesForScopeOnMain(", mac, fixed = TRUE)[[1]]
  expect_gt(start, 0L)
  tail <- substr(mac, start, nchar(mac))
  finish <- regexpr("static NSDictionary *ModelTrellisTextAttributes", tail, fixed = TRUE)[[1]]
  expect_gt(finish, 0L)
  helper <- substr(tail, 1L, finish - 1L)

  expect_match(helper, "application.nativeMixedModels()", fixed = TRUE)
  expect_match(helper, "QueueMixedModelMainRTask(state)", fixed = TRUE)
  expect_match(helper, "RefitOpenTable1WindowsOnMain(group, true)", fixed = TRUE)
  expect_match(helper,
    "RefreshMeanComparisonSelectionScopesOnMain(group, !selectionChanged)",
    fixed = TRUE
  )
})

test_that("Windows applies the same mutation effects and auto-refit policy", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(windows, "if ((valueChanged || renamed) && rSyncQueued)", fixed = TRUE)
  expect_match(windows, "effects.generalizedGlmIdsToRefit", fixed = TRUE)
  expect_match(windows, "if (found->second.autoFit) RefitScaleAnalysis(id)", fixed = TRUE)
  expect_match(windows, "if (found->second.autoRefit) QueueRegressionComparison", fixed = TRUE)
  expect_match(windows, "if (found->second.autoRefit) QueueGeneralizedComparison", fixed = TRUE)
  expect_match(windows, "return task.state.id == id;", fixed = TRUE)
})
