test_that("file import variable choosers support searchable persistent selection", {
  root <- linkeda_source_test_root()
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  windows_header <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.h"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  windows_app <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(windows_header, "std::set<std::string> selectedVariables_;", fixed = TRUE)
  expect_match(windows, "void DatasetVariableDialog::RebuildMultipleVariableList()",
               fixed = TRUE)
  expect_match(windows, "variableSearch_.PlaceholderText(L\"Search variables\");",
               fixed = TRUE)
  expect_match(windows, "SyncMultipleVariableSelection();", fixed = TRUE)
  expect_match(windows, "variables selected", fixed = TRUE)
  expect_gte(lengths(regmatches(windows_app, gregexpr(
    "DatasetVariableDialog::Create(", windows_app, fixed = TRUE
  ))), 3L)
  expect_match(windows_app, "ShowImportColumnChooser(lines[1], lines[2]",
               fixed = TRUE)
  expect_match(windows_app, "ShowNativeImportColumnChooser(std::move(dataframe)",
               fixed = TRUE)

  expect_match(mac, "NativeImportColumnSelectionTarget", fixed = TRUE)
  expect_match(mac, "initWithTable:table searchField:searchField", fixed = TRUE)
  expect_match(mac, "setPlaceholderString:@\"Search variables\"", fixed = TRUE)
  expect_match(mac, "std::set<std::string> _selectedVariables;", fixed = TRUE)
  expect_match(mac, "task.selectedVariables = [selectionTarget selectedVariables];",
               fixed = TRUE)
  expect_match(mac, "NSCaseInsensitiveSearch", fixed = TRUE)
})
