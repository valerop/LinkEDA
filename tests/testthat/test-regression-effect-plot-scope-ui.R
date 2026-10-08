test_that("Windows effect plots display the rows used by their R fit", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  app <- paste(readLines(file.path(root, "src", "platform", "windows", "winui",
                                   "LinkEDA", "App.xaml.cpp"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  runner <- paste(readLines(file.path(root, "src", "core", "dataset_model.cpp"),
                            warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  view <- paste(readLines(file.path(root, "src", "platform", "windows", "winui",
                                    "LinkEDA", "ScatterPlotView.cpp"),
                          warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  plot_function <- regmatches(app, regexpr(
    "bool App::OpenPooledRegressionInteractionPlot\\([\\s\\S]*?bool App::OpenPooledRegressionPartialPlot",
    app, perl = TRUE))
  expect_length(plot_function, 1L)
  expect_match(plot_function,
               "SetComputedPlotScope(*pointer, computedScope, scopeCaptured, *dataframe)",
               fixed = TRUE)
  expect_match(plot_function, "if (existing)", fixed = TRUE)
  expect_match(plot_function, "view->second->Show(*existing)", fixed = TRUE)
  expect_false(grepl("activeAnalysisScope", plot_function, fixed = TRUE))

  calls <- regmatches(app, gregexpr("OpenPooledRegressionInteractionPlot\\(",
                                    app, perl = TRUE))[[1L]]
  expect_length(calls, 6L) # Definition and five Windows entry points.
  expect_match(app, "post.lines, specification.dataScope,", fixed = TRUE)
  expect_match(app, "post.lines,computedScope,scopeCaptured,message", fixed = TRUE)
  expect_match(app, "post.lines, computedScope, scopeCaptured, message", fixed = TRUE)
  expect_match(app, "computedScope = modelCopy.dataScope", fixed = TRUE)
  expect_match(app, "SetComputedPlotScope(*model, modelState->second.dataScope,",
               fixed = TRUE)
  expect_match(app, "SetComputedPlotScope(*model, found->second.dataScope,",
               fixed = TRUE)
  expect_match(app, "SetComputedPlotScope(*model, state.dataScope,",
               fixed = TRUE)
  expect_match(app, "SetComputedPlotScope(*pointer, computedScope, scopeCaptured, *dataframe)",
               fixed = TRUE)
  expect_match(app, "plot.dataScopeCaptured = true", fixed = TRUE)
  partial_function <- regmatches(app, regexpr(
    "bool App::OpenPooledRegressionPartialPlot\\([\\s\\S]*?void App::ShowDataSheet",
    app, perl = TRUE))
  expect_length(partial_function, 1L)
  expect_match(partial_function,
               "SetComputedPlotScope(*pointer, computedScope, scopeCaptured, *dataframe)",
               fixed = TRUE)
  partial_calls <- regmatches(app, gregexpr("OpenPooledRegressionPartialPlot\\(",
                                            app, perl = TRUE))[[1L]]
  expect_length(partial_calls, 5L) # Definition and four model paths.
  expect_match(runner, "model_record$scope <- scope", fixed = TRUE)
  expect_match(runner, "model_record$selected_rows <-", fixed = TRUE)
  expect_match(view, "AnalysisScopeWindowSummary(\n", fixed = TRUE)
})
