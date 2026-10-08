test_that("secondary click exposes the shared searchable add selector on both platforms", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(windows, "Controls::Flyout CreateSearchableAddSelector(", fixed = TRUE)
  expect_match(windows, "target.ContextFlyout(selector);", fixed = TRUE)
  expect_match(windows, "ListViewSelectionMode::Extended", fixed = TRUE)
  expect_match(windows, "Shift/Ctrl+click to select", fixed = TRUE)
  expect_match(windows, "AttachPrimaryMenu(addTerm", fixed = TRUE)
  expect_gte(lengths(regmatches(windows, gregexpr(
    "AttachSearchableSelectorOnSecondaryClick(", windows, fixed = TRUE
  ))), 8L)

  for (command in c(
    "TABLE1_ADD_VARIABLE", "MEAN_ADD_DEPENDENT", "PCAFA_SET_VARIABLES",
    "DENDRO_SET_VARIABLES", "GGLM_ADD_TERM", "GCOMP_ADD_TERM_MODEL"
  )) expect_match(windows, command, fixed = TRUE)

  expect_match(mac, "@interface LinkEDASearchableAddController", fixed = TRUE)
  expect_match(mac, "@interface LinkEDASecondaryActionButton", fixed = TRUE)
  expect_match(mac, "@property(nonatomic, assign) NSWindow *ownerWindow;", fixed = TRUE)
  expect_false(grepl("__weak", mac, fixed = TRUE))
  expect_match(mac, "self.tableView.allowsMultipleSelection = YES;", fixed = TRUE)
  expect_match(mac, "- (void)rightMouseDown:(NSEvent *)event", fixed = TRUE)
  expect_gte(lengths(regmatches(mac, gregexpr(
    "ShowSearchableAddSelector(", mac, fixed = TRUE
  ))), 9L)
  expect_match(mac, "[super mouseDown:event];", fixed = TRUE)
})

test_that("searchable add selectors submit batches to list based analyses", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(windows,
    'std::vector<std::string> command{ "PCAFA_SET_VARIABLES"', fixed = TRUE)
  expect_match(windows,
    'std::vector<std::string> command{"DENDRO_SET_VARIABLES"', fixed = TRUE)
  expect_match(mac, "for (NSString *label in names)", fixed = TRUE)
  expect_match(mac, "RefitCorrelationMatrixState(*current);", fixed = TRUE)
  expect_match(mac, "if (changed) [self queueRefit];", fixed = TRUE)
})

test_that("searchable term rows expose the existing model-term submenus", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(windows,
    "std::function<Controls::MenuFlyout(std::string const&)> contextMenu", fixed = TRUE)
  expect_match(windows, "item.ContextFlyout(menu);", fixed = TRUE)
  expect_match(windows, "CreateSearchableTermContextMenu(", fixed = TRUE)
  expect_match(windows, 'modelTerms.Text(L"Model terms");', fixed = TRUE)
  expect_match(windows, "TermsIncludingSearchBase(", fixed = TRUE)
  expect_match(windows, "BuildInteractionMenuSubItem(", fixed = TRUE)
  expect_false(grepl(
    "AppendPolynomialSearchChoices(choices", windows, fixed = TRUE
  ))

  expect_match(mac,
    "typedef NSMenu * (^LinkEDASearchContextMenuHandler)(NSString *value);",
    fixed = TRUE)
  expect_match(mac, "self.tableView.menu = self.contextMenu;", fixed = TRUE)
  expect_match(mac, "- (void)menuNeedsUpdate:(NSMenu *)menu", fixed = TRUE)
  expect_match(mac, "SearchableModelTermContextMenu(", fixed = TRUE)
  expect_match(mac, "SearchableComparisonTermContextMenu(", fixed = TRUE)
  expect_match(mac, 'initWithTitle:@"Model terms"', fixed = TRUE)
  expect_match(mac, "AddModelTermMenuItems(termsMenu", fixed = TRUE)
  expect_match(mac, "AddComparisonModelTermMenuItems(termsMenu", fixed = TRUE)
})

test_that("anchored polynomial terms use one degree prompt on both platforms", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(windows, "void PromptPolynomialDegree(", fixed = TRUE)
  expect_match(windows, 'item.Text(L"Add polynomial term…");', fixed = TRUE)
  expect_match(windows, "Available degrees:", fixed = TRUE)
  expect_match(windows, "Enter one of the available integer degrees.", fixed = TRUE)
  expect_match(windows, "args.Cancel(true);", fixed = TRUE)

  expect_match(mac, "@interface LinkEDAPolynomialDegreePromptTarget", fixed = TRUE)
  expect_match(mac, 'initWithTitle:@"Add polynomial term…"', fixed = TRUE)
  expect_match(mac, "- (void)promptForDegree:(id)sender", fixed = TRUE)
  expect_match(mac, "Enter one of the available integer degrees:", fixed = TRUE)
  expect_match(mac, "[target performSelector:action withObject:selection];", fixed = TRUE)
})
