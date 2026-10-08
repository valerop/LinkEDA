test_that("Windows application menus expose the shared macOS command families", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  windows_menu <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "ApplicationMenu.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  windows_app <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  for (title in c("file", "edit", "data", "plot", "analyze"))
    expect_match(windows_menu, paste0("menuTitles.", title), fixed = TRUE)

  expect_match(windows_menu, "ActiveDatasetCommandOptions(datasetGroups)", fixed = TRUE)
  expect_match(windows_menu, 'L"Show Active Dataset"', fixed = TRUE)
  expect_match(windows_menu, 'L"Open Data Sheet"', fixed = TRUE)
  expect_match(windows_menu, "DataVariableTypeCommandOptions()[0].title", fixed = TRUE)
  expect_match(windows_menu, "DataVariableInformationCommandOption().title", fixed = TRUE)
  expect_match(windows_menu, "lastPlotToken_ = token", fixed = TRUE)
  expect_match(windows_menu, "const auto* activePlot = Context(lastPlotToken_)", fixed = TRUE)
  expect_match(windows_menu, '{"DATA_SET_ACTIVE_DATASET", value}', fixed = TRUE)
  expect_match(windows_menu, '{"DATA_SHOW_ACTIVE_DATASET"}', fixed = TRUE)
  expect_match(windows_app, '{"SET_VARIABLE_TYPE", plot.group, plot.xLabel, type}', fixed = TRUE)
  expect_match(windows_app, "VariableInformationText(", fixed = TRUE)

  expect_match(windows_menu, '{ApplicationCommandId::Copy, L"Copy"', fixed = TRUE)
  expect_match(windows_menu, '{"SELECTED", context->group}', fixed = TRUE)
  expect_match(windows_menu, 'L"Show Snapshot Album"', fixed = TRUE)

  help <- regexpr('{L"Help", {', windows_menu, fixed = TRUE)[[1L]]
  expect_gt(help, 0L)
  help_block <- substr(windows_menu, help, help + 350L)
  expect_lt(regexpr('L"Documentation"', help_block, fixed = TRUE)[[1L]],
            regexpr('L"Welcome to LinkEDA"', help_block, fixed = TRUE)[[1L]])

  window_start <- regexpr('NSMenu *windowMenu =', mac, fixed = TRUE)[[1L]]
  help_start <- regexpr('NSMenuItem *helpItem =', mac, fixed = TRUE)[[1L]]
  expect_gt(window_start, 0L)
  expect_gt(help_start, window_start)
  expect_false(grepl("Welcome to LinkEDA",
                     substr(mac, window_start, help_start - 1L), fixed = TRUE))
})

test_that("Table 1 keeps its complete contextual controls and Export last", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  variable_start <- regexpr(
    "Controls::MenuFlyout AnalysisOutputView::CreateTable1VariableMenu(",
    windows, fixed = TRUE
  )[[1L]]
  expect_gt(variable_start, 0L)
  variable_tail <- substr(windows, variable_start, nchar(windows))
  variable_end <- regexpr(
    "Controls::MenuFlyout AnalysisOutputView::CreateNestedRowVariableMenu(",
    variable_tail, fixed = TRUE
  )[[1L]]
  expect_gt(variable_end, 0L)
  menu <- substr(variable_tail, 1L, variable_end - 1L)

  expect_match(menu, "CreateGroupingMenu()", fixed = TRUE)
  expect_match(menu, "CreateAddVariableMenu()", fixed = TRUE)
  expect_match(menu, "CreateTable1StatisticsMenu()", fixed = TRUE)
  expect_match(menu, "Table1ReplaceVariableTitle(variable)", fixed = TRUE)
  expect_match(menu, "Table1RemoveVariableTitle(variable)", fixed = TRUE)
  expect_match(menu, 'types.Text(L"Type of variable")', fixed = TRUE)
  expect_match(menu, "Table1PlotMenuOptions(", fixed = TRUE)
  expect_match(menu, "AppendTableAnnotationContextItems(window_, menu)", fixed = TRUE)
  expect_match(menu, "exporter_->CreateMenu", fixed = TRUE)
  expect_lt(regexpr("CreateGroupingMenu()", menu, fixed = TRUE)[[1L]],
            regexpr("CreateAddVariableMenu()", menu, fixed = TRUE)[[1L]])
  expect_lt(regexpr("AppendTableAnnotationContextItems", menu, fixed = TRUE)[[1L]],
            regexpr("exporter_->CreateMenu", menu, fixed = TRUE)[[1L]])
})

test_that("Windows selection refresh preserves live menu controls", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  menu <- paste(readLines(file.path(root, "src", "platform", "windows",
    "winui", "LinkEDA", "ApplicationMenu.cpp"), warn = FALSE),
    collapse = "\n")
  sheet <- paste(readLines(file.path(root, "src", "platform", "windows",
    "winui", "LinkEDA", "DataSheetView.cpp"), warn = FALSE),
    collapse = "\n")

  update <- regexpr("SameMenuStructure(lastSnapshot_, snapshot)", menu,
    fixed = TRUE)[[1L]]
  replace <- regexpr("menuBar_.Items().Clear()", menu, fixed = TRUE)[[1L]]
  expect_gt(update, 0L)
  expect_gt(replace, update)
  expect_match(menu, "item.IsEnabled(state.enabled)", fixed = TRUE)
  expect_match(menu, "toggle.IsChecked(state.checked)", fixed = TRUE)
  expect_match(sheet, "rowsView_.ContextRequested(", fixed = TRUE)
  expect_false(grepl("rowsView_.ContextFlyout(BuildRowsContextMenu(",
    sheet, fixed = TRUE))
})

test_that("Windows menus track the first flyout lifetime before rebuilding", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  menu_cpp <- paste(readLines(file.path(root, "src", "platform", "windows",
    "winui", "LinkEDA", "ApplicationMenu.cpp"), warn = FALSE),
    collapse = "\n")
  menu_h <- paste(readLines(file.path(root, "src", "platform", "windows",
    "winui", "LinkEDA", "ApplicationMenu.h"), warn = FALSE),
    collapse = "\n")

  expect_match(menu_cpp, "item.LayoutUpdated(", fixed = TRUE)
  expect_match(menu_cpp, "TryConfigureMenuBarItem(state)", fixed = TRUE)
  expect_match(menu_cpp, "UIElement::PointerPressedEvent()", fixed = TRUE)
  expect_match(menu_cpp, "item.LayoutUpdated(state->layoutUpdatedToken)",
    fixed = TRUE)
  expect_match(menu_cpp,
    "openMenuCount_ > 0 || menuDismissPending_ || rebuildQueued_",
    fixed = TRUE)
  expect_match(menu_cpp, "host->menuDismissPending_ = false", fixed = TRUE)
  expect_match(menu_h, "bool menuDismissPending_ = false", fixed = TRUE)
})
