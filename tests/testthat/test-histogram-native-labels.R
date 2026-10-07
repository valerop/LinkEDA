test_that("native histograms display and keep the active variable name", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "ScatterPlotView.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(mac, "ToNSString(self.model->xLabel)", fixed = TRUE)
  expect_match(mac, "NSMaxY(pr) + 30.0", fixed = TRUE)
  expect_match(mac, "previousHistogramDefaultTitle", fixed = TRUE)
  expect_match(mac, "model->title = HistogramDefaultTitle(model->xLabel)", fixed = TRUE)
  expect_match(mac, "histogramShowTickMarks", fixed = TRUE)
  expect_match(mac, "histogramShowTickLabels", fixed = TRUE)
  expect_match(mac, "menuState.tickMarks", fixed = TRUE)
  expect_match(mac, "menuState.tickLabels", fixed = TRUE)
  expect_match(mac, "- (NSRect)variableLabelRect", fixed = TRUE)
  expect_match(mac, "NSPointInRect(initialPoint, [self histogramAxisRect])", fixed = TRUE)
  expect_match(mac, "- (NSMenu *)variableMenu", fixed = TRUE)
  expect_match(mac, 'initWithTitle:@"Change variable"', fixed = TRUE)
  expect_match(mac, '"CHANGE_X_VARIABLE|" + variable.name', fixed = TRUE)

  expect_match(windows, "xAxisLabel_ = Label(to_hstring(currentModel_->xLabel)",
               fixed = TRUE)
  expect_match(windows, "Show tick marks", fixed = TRUE)
  expect_match(windows, "Show tick labels", fixed = TRUE)
  expect_match(windows, "currentModel_->histogramShowTickMarks", fixed = TRUE)
  expect_match(windows, "currentModel_->histogramShowTickLabels", fixed = TRUE)
})
