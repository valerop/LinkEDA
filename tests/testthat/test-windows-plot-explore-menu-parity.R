test_that("Windows plot exploration menu matches the macOS structure", {
  root <- normalizePath(testthat::test_path("..", ".."),
                        winslash = "/", mustWork = TRUE)
  windows_path <- file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "ScatterPlotView.cpp"
  )
  mac_path <- file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  )
  skip_if_not(
    all(file.exists(c(windows_path, mac_path))),
    "Native source checkout not available"
  )
  windows <- paste(readLines(windows_path, warn = FALSE, encoding = "UTF-8"),
                   collapse = "\n")
  app <- paste(readLines(file.path(root, "src", "platform", "windows",
                                   "winui", "LinkEDA", "App.xaml.cpp"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  mac <- paste(readLines(mac_path, warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")

  expect_match(windows, 'selection.Text(L"Explore plot")', fixed = TRUE)
  expect_match(windows, 'appendInteractionMode(L"Select cases", "select")', fixed = TRUE)
  expect_match(windows, 'appendInteractionMode(L"Brush cases", "brush")', fixed = TRUE)
  expect_match(windows, 'appendInteractionMode(L"Pan", "pan")', fixed = TRUE)
  expect_match(windows, 'appendInteractionMode(L"Zoom", "zoom")', fixed = TRUE)
  expect_match(windows, 'commandCallback_({ "INTERACTION_MODE", plotId_, mode })', fixed = TRUE)
  expect_match(windows, "ScatterplotSelectionModeMenuOptions()", fixed = TRUE)
  expect_match(windows, "EffectiveSelectionModeName(", fixed = TRUE)
  expect_match(windows, "SelectionModeFromString(", fixed = TRUE)
  expect_match(windows, 'commandCallback_({ "SELECTION_MODE", plotId_, value })', fixed = TRUE)
  expect_match(windows, "brushSelectionBase_, rows, selectionMode_", fixed = TRUE)
  expect_match(windows,
               "selectionCallback_(group_, next, ::rlispstat::core::SelectionMode::Replace)",
               fixed = TRUE)
  expect_match(windows, 'selectAll.Text(L"Select all visible cases")', fixed = TRUE)
  expect_match(windows, 'clear.Text(L"Clear selection")', fixed = TRUE)
  expect_match(windows, 'invert.Text(L"Invert selection")', fixed = TRUE)
  expect_match(windows,
               'exclude.Text(L"Exclude selected cases from all analyses")',
               fixed = TRUE)
  expect_match(windows, "PlotSupportsCaseExclusion(*currentModel_)", fixed = TRUE)
  expect_match(windows, 'DispatchPlotCommand({ "PLOT_EXCLUDE_SELECTED", plotId_ })',
               fixed = TRUE)
  expect_match(windows,
               'restore.Text(L"Include all cases again")', fixed = TRUE)
  expect_match(windows, 'DispatchPlotCommand({ "PLOT_RESTORE_SCOPE", plotId_ })',
               fixed = TRUE)
  expect_match(app, 'name == "PLOT_EXCLUDE_SELECTED"', fixed = TRUE)
  expect_match(app, '"EXCLUDE_SELECTED_CASES"', fixed = TRUE)
  expect_match(app, "PlotSupportsCaseExclusion(*found->second)", fixed = TRUE)
  expect_match(app, 'name == "PLOT_RESTORE_SCOPE"', fixed = TRUE)
  expect_match(app, 'RefreshOpenPlotsForScope(event.datasetId, false)',
               fixed = TRUE)
  expect_match(app, 'ReconcileAnalysisScope(', fixed = TRUE)
  expect_match(windows, 'resetZoom.Text(L"Reset zoom")', fixed = TRUE)
  expect_match(windows, 'currentModel_->interactionMode == "pan"', fixed = TRUE)
  expect_match(windows, 'currentModel_->interactionMode == "zoom"', fixed = TRUE)
  expect_match(windows, "Color selected points", fixed = TRUE)
  expect_match(windows, "DataColumnCommandOptions(group_)", fixed = TRUE)
  expect_match(windows, "DATA_ADD_SELECTION_COLUMN", fixed = TRUE)
  expect_match(windows, "DATA_ADD_POINT_COLOR_COLUMN", fixed = TRUE)

  expect_match(mac, 'initWithTitle:@"Explore plot"', fixed = TRUE)
  expect_match(mac, '@[@"Select cases", @"select"]', fixed = TRUE)
  expect_match(mac, '@[@"Brush cases", @"brush"]', fixed = TRUE)
  expect_match(mac, '@[@"Pan", @"pan"]', fixed = TRUE)
  expect_match(mac, '@[@"Zoom", @"zoom"]', fixed = TRUE)
  expect_match(mac, 'add(@"Reset zoom", @"reset", false)', fixed = TRUE)
  expect_match(mac, '"EXCLUDE_SELECTED_CASES|" + model->group', fixed = TRUE)
  expect_match(mac, "PlotSupportsCaseExclusion(*model)", fixed = TRUE)
  expect_match(mac, '"INCLUDE_SELECTED_CASES|" + model->group', fixed = TRUE)
  expect_match(mac, '"INCLUDE_ALL_CASES|" + model->group', fixed = TRUE)
  expect_match(mac, "AddDataColumnSubmenuItem()", fixed = TRUE)
})
