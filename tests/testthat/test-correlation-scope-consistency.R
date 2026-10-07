test_that("correlation scope controls are consistent across native platforms", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(
    file.path(root, "src", "platform", "macos", "linkeda_macos_app.mm"),
    warn = FALSE
  ), collapse = "\n")
  windows_view <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE), collapse = "\n")
  windows_app <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(mac, "GlobalAnalysisScopeSummary", fixed=TRUE)
  expect_match(mac, "setEnabled:NO", fixed=TRUE)
  expect_match(windows_view, "combo.IsHitTestVisible(false)", fixed=TRUE)
  expect_match(windows_view, "combo.IsTabStop(false)", fixed=TRUE)
  expect_match(windows_app, 'name == "CORR_SET_SCOPE"', fixed=TRUE)
  expect_match(mac, "captureAnalysisScope(state.group, state.dataScope", fixed=TRUE)
  expect_match(windows_app, "captureAnalysisScope(state.group, state.dataScope", fixed=TRUE)
})
