test_that("WinUI returns TCP replies from the socket apartment", {
  root <- linkeda_source_test_root()
  path <- file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")
  start <- regexpr("fire_and_forget ReplyToClient", source, fixed = TRUE)[[1L]]
  expect_gt(start, 0L)
  implementation <- substr(source, start, nchar(source))
  finish <- regexpr(
    "\n    void ComputeScatterRangesIncludingAllImputations",
    implementation,
    fixed = TRUE
  )[[1L]]
  expect_gt(finish, 0L)
  implementation <- substr(implementation, 1L, finish)

  position <- function(text) regexpr(text, implementation, fixed = TRUE)[[1L]]
  writer <- position("Windows::Storage::Streams::DataWriter writer")
  capture <- position("winrt::apartment_context connectionApartment;")
  foreground <- position("co_await wil::resume_foreground(dispatcher,")
  dispatch <- position("reply = app->DispatchRequest(commandLines);")
  restore <- position("co_await connectionApartment;")
  write <- position("writer.WriteString")

  expect_gt(writer, 0L)
  expect_gt(capture, writer)
  expect_gt(foreground, capture)
  expect_gt(dispatch, foreground)
  expect_gt(restore, dispatch)
  expect_gt(write, restore)
  expect_false(grepl("co_await winrt::resume_background();", implementation,
                     fixed = TRUE))
  expect_false(grepl("writer.DetachStream();", implementation, fixed = TRUE))
})

test_that("WinUI accepts the generalized-comparison task acknowledgement", {
  root <- linkeda_source_test_root()
  source <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(source, 'if (command == "GCOMP_TASK_RECEIVED")', fixed = TRUE)
  expect_match(
    source,
    'return "ERR malformed generalized-comparison task acknowledgement";',
    fixed = TRUE
  )
  expect_match(
    source,
    'return "ERR invalid generalized-comparison task acknowledgement";',
    fixed = TRUE
  )
  expect_match(source, 'return "OK\\t" + lines[1];', fixed = TRUE)
})

test_that("WinUI routes imputation diagnostic page changes through the application dispatcher", {
  root <- linkeda_source_test_root()
  source <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE), collapse = "\n")

  start <- regexpr(
    "void App::HandleAnalysisViewCommand(std::vector<std::string> const& command)",
    source, fixed = TRUE
  )[[1L]]
  expect_gt(start, 0L)
  handler <- substr(source, start, start + 1800L)
  expect_match(handler, 'if (name == "DATA_IMPUTATION_DIAGNOSTICS")', fixed = TRUE)
  expect_match(handler, "DispatchApplicationCommand(command)", fixed = TRUE)
  expect_match(handler, "Imputation diagnostics command failed", fixed = TRUE)
})

test_that("WinUI boxes routed-event delegates instead of querying IInspectable", {
  root <- linkeda_source_test_root()
  source <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "DataSheetView.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(source, "box_value(activateDatasetHandler)", fixed = TRUE)
  expect_false(grepl(
    "activateDatasetHandler.as<Windows::Foundation::IInspectable>()",
    source, fixed = TRUE
  ))
})

test_that("linked bar selection does not focus an inactive data sheet", {
  root <- linkeda_source_test_root()
  source <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "DataSheetView.cpp"
  ), warn = FALSE), collapse = "\n")

  refresh <- regexpr("void DataSheetView::RefreshSelection(", source, fixed = TRUE)[[1L]]
  sync <- regexpr("void DataSheetView::SynchronizeListSelection()", source,
                  fixed = TRUE)[[1L]]
  styles <- regexpr("void DataSheetView::ApplyRowStyles()", source, fixed = TRUE)[[1L]]
  expect_gt(refresh, 0L)
  expect_gt(sync, refresh)
  expect_gt(styles, sync)
  refresh_body <- substr(source, refresh, sync - 1L)
  sync_body <- substr(source, sync, styles - 1L)

  expect_match(refresh_body, "if (windowActive_) SynchronizeListSelection();",
               fixed = TRUE)
  expect_match(refresh_body, "else listSelectionDirty_ = true;", fixed = TRUE)
  expect_match(refresh_body, "ApplyRowStyles(changed);", fixed = TRUE)
  expect_false(grepl("SelectedItems()", refresh_body, fixed = TRUE))
  expect_match(sync_body, "rowsView_.SelectedItems()", fixed = TRUE)
  expect_match(source,
               "if (active && listSelectionDirty_ && !suppressSelection_)",
               fixed = TRUE)
})

test_that("WinUI data sheets size ordinary columns from their variable names", {
  root <- linkeda_source_test_root()
  source <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "DataSheetView.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(
    source,
    "24.0 + static_cast<double>(widest) * 7.2, 92.0, 360.0",
    fixed = TRUE
  )
  width_loop <- regexpr(
    "for (std::size_t columnIndex = 0;",
    source,
    fixed = TRUE
  )[[1L]]
  imputation_check <- regexpr(
    "if (::rlispstat::core::DataFrameShowsAllImputations(dataframe))",
    source,
    fixed = TRUE
  )[[1L]]
  expect_gt(width_loop, 0L)
  expect_gt(imputation_check, width_loop)
  expect_false(grepl("Â·", source, fixed = TRUE))
  expect_match(source, '" \\xC2\\xB7 "', fixed = TRUE)
})

test_that("WinUI paired tests keep distinct continuous variables available", {
  root <- linkeda_source_test_root()
  source <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE), collapse = "\n")

  start <- regexpr(
    "std::vector<std::string> MeanComparisonResponseVariables(",
    source,
    fixed = TRUE
  )[[1L]]
  expect_gt(start, 0L)
  implementation <- substr(source, start, start + 1800L)
  expect_match(
    implementation,
    "DataColumnLooksLikeAnalysisId(column)",
    fixed = TRUE
  )
  expect_false(grepl(
    "DataColumnLooksLikeId(column)",
    implementation,
    fixed = TRUE
  ))
})

test_that("WinUI presents MI fit methods as compact notes", {
  root <- linkeda_source_test_root()
  source <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(source, 'value.rfind("Multiple imputation:", 0) == 0',
               fixed = TRUE)
  expect_match(
    source,
    'value.rfind("R2 and adjusted R2 use mice::pool.r.squared", 0) == 0',
    fixed = TRUE
  )
  expect_match(source, "CompactMultipleImputationFitMethod()", fixed = TRUE)
  expect_match(source, 'multipleImputationNote?"Note: "', fixed = TRUE)
  expect_match(source, "warning_.MaxWidth(kModelResultWindowWidth-48.0)",
               fixed = TRUE)
})

test_that("WinUI silently retries a missing R dataset registration", {
  root <- linkeda_source_test_root()
  source <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE), collapse = "\n")

  start <- regexpr('if (command == "MODEL_UPDATE_ERROR"', source,
                   fixed = TRUE)[[1L]]
  expect_gt(start, 0L)
  handler <- substr(source, start, start + 6500L)
  pending <- regexpr("datasetRecoveryPending_.insert(modelId)", handler,
                     fixed = TRUE)[[1L]]
  dispatch <- regexpr("commandDispatcher_->dispatch(lines)", handler,
                      fixed = TRUE)[[1L]]
  restore <- regexpr("fits[modelId] = std::move(*retainedFit)", handler,
                     fixed = TRUE)[[1L]]
  retry <- regexpr("QueueLinearModelFit(modelId)", handler,
                   fixed = TRUE)[[1L]]
  expect_gt(pending, 0L)
  expect_gt(dispatch, pending)
  expect_gt(restore, dispatch)
  expect_gt(retry, restore)
  expect_match(source,
    "if (datasetRecoveryPending_.find(group) != datasetRecoveryPending_.end())",
    fixed = TRUE)
})

test_that("WinUI hides Welcome through its HWND", {
  root <- linkeda_source_test_root()
  source <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WelcomeWindow.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(source, "ShowWindow(WindowHandle(window_), SW_HIDE)", fixed = TRUE)
  expect_false(grepl("window_.AppWindow().Hide()", source, fixed = TRUE))
})

test_that("WinUI scale-analysis menus keep macOS variable information and annotations", {
  root <- linkeda_source_test_root()
  workflow <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE), collapse = "\n")
  app <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(workflow, 'information.Text(L"Variable information")', fixed = TRUE)
  expect_match(workflow, '"SCALE_VARIABLE_INFO", state_.group, current.variable', fixed = TRUE)
  expect_match(workflow, "!header && column == 0 && rowMenu", fixed = TRUE)
  expect_false(grepl("AttachDirectMenu(grid, menus[index])", workflow, fixed = TRUE))
  expect_gte(length(gregexpr(
    "AppendTableAnnotationContextItems", workflow, fixed = TRUE
  )[[1L]]), 8L)
  expect_match(app, 'if (command == "SCALE_VARIABLE_INFO")', fixed = TRUE)
  expect_match(app, "VariableInformationText(*dataframe, lines[2])", fixed = TRUE)
  expect_match(app, 'if (name == "SCALE_VARIABLE_INFO")', fixed = TRUE)
  expect_match(
    app,
    'if (name == "SCALE_VARIABLE_INFO")\n        {\n            const std::string reply = DispatchApplicationCommand(command);',
    fixed = TRUE
  )

  application_dispatch_start <- regexpr(
    "std::string App::DispatchApplicationCommand(std::vector<std::string> const& command)",
    app, fixed = TRUE
  )[[1L]]
  request_dispatch_start <- regexpr(
    "std::string App::DispatchRequest(std::vector<std::string> const& lines)",
    app, fixed = TRUE
  )[[1L]]
  expect_gt(application_dispatch_start, 0L)
  expect_gt(request_dispatch_start, application_dispatch_start)
  application_dispatch <- substr(
    app, application_dispatch_start, request_dispatch_start - 1L
  )
  expect_match(
    application_dispatch,
    'if (command[0] == "SCALE_VARIABLE_INFO")',
    fixed = TRUE
  )
  expect_match(
    application_dispatch,
    "VariableInformationText(",
    fixed = TRUE
  )
  expect_match(
    application_dispatch,
    "*dataframe, command[2]",
    fixed = TRUE
  )
})

test_that("WinUI linear models use the contextual menu without a duplicate Model button", {
  root <- linkeda_source_test_root()
  workflow <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE), collapse = "\n")
  header <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.h"
  ), warn = FALSE), collapse = "\n")

  expect_false(grepl('Content(box_value(L"Model ▾"))', workflow, fixed = TRUE))
  expect_false(grepl("modelActionsButton_", workflow, fixed = TRUE))
  expect_false(grepl("modelActionsButton_", header, fixed = TRUE))
  expect_match(workflow, "AttachWindowContextFlyout(window_, menu);", fixed = TRUE)
})

test_that("WinUI result annotations precede the final Export menu", {
  root <- linkeda_source_test_root()
  branding <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WindowBranding.cpp"
  ), warn = FALSE), collapse = "\n")

  start <- regexpr(
    "void AppendTableAnnotationContextItems(", branding, fixed = TRUE
  )[[1L]]
  expect_gt(start, 0L)
  implementation <- substr(branding, start, start + 6500L)
  expect_match(implementation, 'item.Text() == L"Export"', fixed = TRUE)
  expect_match(
    implementation, "menu.Items().InsertAt(exportIndex, annotate);", fixed = TRUE
  )
  expect_match(
    implementation,
    "menu.Items().InsertAt(exportIndex + 1, MenuFlyoutSeparator());",
    fixed = TRUE
  )
})

test_that("WinUI Scale Analysis dimensionality menus match the macOS controls", {
  root <- linkeda_source_test_root()
  workflow <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE), collapse = "\n")
  app <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE), collapse = "\n")

  for (label in c(
    'method.Text(L"Method")', 'missing.Text(L"Missing data")',
    'factors.Text(L"Number of factors")', 'extraction.Text(L"Extraction")',
    'rotation.Text(L"Rotation")', 'standardize.Text(L"Standardize variables")',
    'saveScores.Text(L"Save factor/component scores...")',
    'explain.Text(L"Explain statistic")'
  )) expect_match(workflow, label, fixed = TRUE)
  expect_match(workflow, '{L"Quartimax", "quartimax"}', fixed = TRUE)
  expect_match(
    workflow, 'commandCallback_({"SCALE_SAVE_DIMENSION_SCORES"', fixed = TRUE
  )
  expect_match(
    app, 'if (command[0] == "SCALE_SAVE_DIMENSION_SCORES")', fixed = TRUE
  )
  expect_match(
    app, "pendingMainRTasks_.dimensionalityScoreSaveTasks", fixed = TRUE
  )
  expect_match(
    app, 'if (command == "DIMENSIONALITY_SAVE_SCORES_RESULT")', fixed = TRUE
  )
})

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
