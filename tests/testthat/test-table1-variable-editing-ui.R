test_that("ordinary Table 1 exposes direct variable editing on both native platforms", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(mac, '_state.tableType == "table1" ||', fixed = TRUE)
  expect_match(mac, "- (NSMenu *)addVariableMenu", fixed = TRUE)
  expect_match(mac, "action:@selector(addVariableFromMenu:)", fixed = TRUE)
  expect_match(mac, "- (NSMenu *)replacementMenuForRow:(NSInteger)row", fixed = TRUE)
  expect_match(mac, "NSMenu *menu = [_controller replacementMenuForRow:row]", fixed = TRUE)
  expect_match(
    mac,
    "rlispstat::core::Table1DisplayLabelText(displayRow) == displayRow.variable",
    fixed = TRUE
  )

  expect_match(windows, 'const bool ordinaryTable1 = state_.tableType == "table1"',
               fixed = TRUE)
  expect_match(windows, "CreateTable1AddVariableFlyout", fixed = TRUE)
  expect_match(windows, "CreateTable1ReplacementMenu", fixed = TRUE)
  expect_match(windows, 'Dispatch("TABLE1_ADD_VARIABLE", variable)', fixed = TRUE)
  expect_match(windows, 'Dispatch("TABLE1_REPLACE_VARIABLE",', fixed = TRUE)
  expect_match(windows, "AttachPrimaryAndContextMenus(cell,", fixed = TRUE)
  expect_match(windows, "CreateTable1ReplacementMenu(row.variable), context", fixed = TRUE)

  expect_match(
    mac,
    "std::find(_state.variables.begin(), _state.variables.end(), column.name)",
    fixed = TRUE
  )
  expect_match(
    windows,
    "std::find(state_.variables.begin(), state_.variables.end(), candidate)",
    fixed = TRUE
  )
})

test_that("the shared table export corrections advance the package version", {
  root <- linkeda_source_test_root()
  description <- read.dcf(file.path(root, "DESCRIPTION"))
  expect_gte(package_version(unname(description[1, "Version"])),
             package_version("0.0.59"))
})

test_that("Table 1 uses the standard inline control and semantic APA exporter", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  dispatcher <- paste(readLines(file.path(
    root, "src", "core", "command_dispatcher.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(mac, '[_addVariableButton setTitle:@"+ Add variable…"]', fixed = TRUE)
  expect_match(mac, "[_addVariableButton setBezelStyle:NSBezelStyleInline]", fixed = TRUE)
  expect_match(mac, "[_addVariableButton setBordered:NO]", fixed = TRUE)
  expect_match(mac, "auto table = [self clipboardTable]", fixed = TRUE)
  expect_match(mac, "options.apa7 = true", fixed = TRUE)
  expect_match(mac, "PublicationTablesPDFData({table}, options)", fixed = TRUE)
  expect_match(
    mac,
    "table.title = rlispstat::core::PublicationTitleWithoutTableLabel(_state.title);",
    fixed = TRUE
  )
  expect_match(
    dispatcher,
    "table.title = PublicationTitleWithoutTableLabel(state.title);",
    fixed = TRUE
  )
})
