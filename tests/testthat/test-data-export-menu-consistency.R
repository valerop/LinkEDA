test_that("data export is wired to the shared writer on both native platforms", {
  root <- linkeda_source_test_root()
  command_model <- paste(readLines(file.path(
    root, "src", "core", "command_model.cpp"
  ), warn = FALSE), collapse = "\n")
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  windows_menu <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "ApplicationMenu.cpp"
  ), warn = FALSE), collapse = "\n")
  windows_app <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(command_model,
               '{"Export Data...", "export_data", "FILE_EXPORT_DATA"',
               fixed = TRUE)
  expect_match(mac, "ShowExportDataChooserOnMain", fixed = TRUE)
  expect_match(mac, "WriteDataExportCSV(csv, exportFrame)", fixed = TRUE)
  expect_match(mac, "NativeDataExportFileFilters", fixed = TRUE)
  expect_match(mac, "NativeDataExportRScript", fixed = TRUE)
  expect_match(mac, "[panel setAccessoryView:accessory]", fixed = TRUE)
  expect_false(grepl("setAccessoryViewDisclosed:", mac, fixed = TRUE))
  expect_match(windows_menu, 'L"Export Data..."', fixed = TRUE)
  expect_match(windows_menu, '"FILE_EXPORT_DATA"', fixed = TRUE)
  expect_match(windows_app, "ChooseExportDataFile", fixed = TRUE)
  expect_match(windows_app, "NativeDataExportFileFilters", fixed = TRUE)
  expect_match(windows_app, "NativeDataExportRScript", fixed = TRUE)
})

test_that("result windows expose their associated data sheet on both platforms", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WindowBranding.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(windows, "SetWindowDataSheetGroup", fixed = TRUE)
  expect_match(windows, "Show data sheet", fixed = TRUE)
  expect_match(mac, "SetWindowDataSheetGroupOnMac", fixed = TRUE)
  expect_match(mac, "Show associated data sheet", fixed = TRUE)
  expect_match(mac, "NSTitlebarAccessoryViewController", fixed = TRUE)
  expect_match(mac, "ShowDataSheetOnMain", fixed = TRUE)
  expect_match(
    mac,
    "SetWindowDataSheetGroupOnMac(window, dataSheetGroup)",
    fixed = TRUE
  )
  expect_match(
    mac,
    "SetWindowDataSheetGroupOnMac(window, dataSheetGroup);\n    NSView *content",
    fixed = TRUE
  )
  expect_match(mac,
    'EnableNotesForWindow(_window, _tableId, @"this statistical table", _state.datasetId)',
    fixed = TRUE
  )
})

test_that("the common macOS dataset refresh includes correlation windows", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")

  refresh_start <- regexpr(
    "static void RefreshDataDeskWindowsOnMain(const std::string &group)\n{",
    mac,
    fixed = TRUE
  )[[1]]
  expect_gt(refresh_start, 0L)
  refresh_tail <- substr(mac, refresh_start, nchar(mac))
  refresh_end <- regexpr(
    "static void CopySelectedRowsToClipboard",
    refresh_tail,
    fixed = TRUE
  )[[1]]
  expect_gt(refresh_end, 0L)
  refresh <- substr(refresh_tail, 1L, refresh_end - 1L)

  expect_match(refresh, "g_correlationMatrixControllers", fixed = TRUE)
  expect_match(refresh, "stateIt->second.group == groupCopy", fixed = TRUE)
  expect_match(refresh, "[controller refresh];", fixed = TRUE)
})

test_that("macOS Import Data uses a reliable modal open panel", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")

  chooser_start <- regexpr("static void ShowImportChooserOnMain()\n{", mac, fixed = TRUE)[[1]]
  expect_gt(chooser_start, 0L)
  chooser_tail <- substr(mac, chooser_start, nchar(mac))
  chooser_end <- regexpr("static void ShowExportDataChooserOnMain()", chooser_tail,
                         fixed = TRUE)[[1]]
  expect_gt(chooser_end, 0L)
  chooser <- substr(chooser_tail, 1L, chooser_end - 1L)

  expect_match(chooser, "NSOpenPanel *panel = [NSOpenPanel openPanel]", fixed = TRUE)
  expect_match(chooser, "const NSModalResponse result = [panel runModal]", fixed = TRUE)
  expect_false(grepl("beginWithCompletionHandler", chooser, fixed = TRUE))
  expect_match(chooser, "OpenWelcomeDataFileAtURLOnMain", fixed = TRUE)

  expect_match(
    mac,
    'if (!lines.empty() && lines[0] == "FILE_IMPORT_DATA")',
    fixed = TRUE
  )
  expect_match(mac, "ShowImportChooserOnMain();", fixed = TRUE)
})

test_that("R code export actions reach the central dispatcher on both platforms", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE), collapse = "\n")

  # AppKit menu items enter DispatchCommand even for non-plot result windows.
  # Both code actions must be forwarded before its plot-only fallback.
  expect_match(mac, "case CommandAction::ShowRCode:", fixed = TRUE)
  expect_match(mac, "case CommandAction::ShowRPublicationCode:", fixed = TRUE)
  expect_match(mac, "MacCommandDispatcher().dispatch(lines)", fixed = TRUE)
  expect_match(mac, "ShowWorkbenchMessageOnMain", fixed = TRUE)

  # The WinUI analysis dispatcher already has a final central-dispatch path.
  expect_match(windows, "const auto reply = commandDispatcher_->dispatch(command);",
               fixed = TRUE)
  expect_match(windows, 'command[0] == "SHOW_R_CODE"', fixed = TRUE)
  expect_match(windows, "ShowAppMessage", fixed = TRUE)
})

test_that("verification data export binds the final path on both platforms", {
  root <- linkeda_source_test_root()
  core <- paste(readLines(file.path(
    root, "src", "core", "provenance_model.cpp"
  ), warn = FALSE), collapse = "\n")
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(core, "verification_data_path <- NULL", fixed = TRUE)
  expect_match(core, "ProvenanceRStringLiteral(absolutePath)", fixed = TRUE)
  expect_false(grepl("Run this script from the directory", core, fixed = TRUE))

  expect_match(mac, "BuildQuartoVerificationDocument", fixed = TRUE)
  expect_match(mac, "BindAnalysisVerificationDataPath", fixed = TRUE)
  expect_match(mac, "[dataDestination fileSystemRepresentation]", fixed = TRUE)
  expect_match(mac, 'setTitle:@"Save Quarto + Data…"', fixed = TRUE)
  expect_match(mac, 'setTitle:@"Create R verification folder"', fixed = TRUE)
  expect_match(mac, 'stringByAppendingString:@"_data.rds"', fixed = TRUE)
  expect_false(grepl('setTitle:@"Save R verification script"', mac, fixed = TRUE))
  expect_false(grepl('setTitle:@"Copy"', mac, fixed = TRUE))

  expect_match(windows, "BuildQuartoVerificationDocument", fixed = TRUE)
  expect_match(windows, "BindAnalysisVerificationDataPath", fixed = TRUE)
  expect_match(windows, "dataPath.wstring()", fixed = TRUE)
  expect_match(windows, 'SecondaryButtonText(L"Save Quarto + Data...")', fixed = TRUE)
  expect_match(windows, 'L"_data.rds"', fixed = TRUE)
  expect_false(grepl('L"R script (*.R)"', windows, fixed = TRUE))
  expect_false(grepl('PrimaryButtonText(L"Copy")', windows, fixed = TRUE))
})

test_that("result-table export menus expose the shared R Code category", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE), collapse = "\n")
  windows_header <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.h"
  ), warn = FALSE), collapse = "\n")

  # AppKit table families share one complete Export constructor, while custom
  # menus append the same R Code category to their existing visual exporters.
  expect_match(mac, "static void AddStandardResultTableExportMenu", fixed = TRUE)
  expect_match(mac,
    "AddRCodeExportItems(exportMenu, \"output\", _analysisId, true)",
    fixed = TRUE)
  expect_match(mac,
    "AddRCodeExportItems(exportMenu, \"output\", _state.id, true)",
    fixed = TRUE)
  expect_match(mac,
    "AddRCodeExportItems(ex, \"output\", _modelTrellisId, true)",
    fixed = TRUE)
  expect_match(mac, "@\"interaction-report\"", fixed = TRUE)

  # WinUI uses the same nested category for whole tables and the contextual
  # menus attached to table rows, variables, and derived scale results.
  expect_match(windows, "void AppendRCodeOnlyExportMenu", fixed = TRUE)
  expect_gte(length(gregexpr(
    "AppendRCodeExportItems\\(", windows, perl = TRUE)[[1]]), 10L)
  expect_gte(length(gregexpr(
    "AppendRCodeOnlyExportMenu\\(", windows, perl = TRUE)[[1]]), 8L)
  for (payload in c(
    "InteractionReportExportPayload", "CorrelationExportPayload",
    "DimensionalityExportPayload", "ScaleAnalysisExportPayload"
  )) {
    expect_match(windows, payload, fixed = TRUE)
  }
  expect_match(windows, "exporter_->CreateMenu", fixed = TRUE)
  expect_match(windows_header, "void SetCommandCallback(Command callback);",
               fixed = TRUE)
})

test_that("Windows table exports never write empty CSV or Markdown payloads", {
  root <- linkeda_source_test_root()
  service <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "TableExportService.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(service, "payload.csvText.empty() ? PayloadCsv(payload)", fixed = TRUE)
  expect_match(service, "payload.markdownText.empty() ? PayloadMarkdown(payload)", fixed = TRUE)
})

test_that("custom macOS result menus organize their final export items", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")

  # These menus append analysis-specific commands after the common visual and
  # R-code items, so their final operation must be the common organizer.
  expect_match(mac,
    "OrganizeExportMenu(ex);\n    [m addItem:[self root:@\"Export\" submenu:ex]]",
    fixed = TRUE
  )
  expect_match(mac,
    "[exportMenu addItem:copy];\n    OrganizeExportMenu(exportMenu);\n    [exportRoot setSubmenu:exportMenu]",
    fixed = TRUE
  )
  expect_match(mac,
    "AddRCodeExportItems(exportMenu, \"output\", self.model->id, false);\n        OrganizeExportMenu(exportMenu);",
    fixed = TRUE
  )
  expect_match(mac,
    "[menu setSubmenu:copyMenu forItem:copyRoot];\n\n        NSMenuItem *exportRoot",
    fixed = TRUE
  )
  expect_match(mac, "[exportMenu addItem:copyRoot];", fixed = TRUE)
})

test_that("Windows export menus use the same Copy Save R Code hierarchy", {
  root <- linkeda_source_test_root()
  service <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "TableExportService.cpp"
  ), warn = FALSE), collapse = "\n")
  plots <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "ScatterPlotView.cpp"
  ), warn = FALSE), collapse = "\n")
  workflows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE), collapse = "\n")

  for (source in list(service, plots, workflows)) {
    expect_match(source, 'copyMenu.Text(L"Copy")', fixed = TRUE)
    expect_match(source, 'saveMenu.Text(L"Save")', fixed = TRUE)
  }
  expect_match(service,
    'action.command.rfind("COPY_", 0) == 0', fixed = TRUE)
  expect_match(plots,
    'AppendPlotRCodeExportItems(exportMenu, commandCallback_, currentModel_)',
    fixed = TRUE
  )
  expect_match(workflows,
    'AppendRCodeExportItems(exportMenu,commandCallback_,state_.id,false)',
    fixed = TRUE
  )
  expect_match(workflows,
    'copyMenu.Items().Append(copy); exportMenu.Items().Append(copyMenu);',
    fixed = TRUE
  )
})

test_that("scatterplot export menus expose registered R code on both platforms", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "ScatterPlotView.cpp"
  ), warn = FALSE), collapse = "\n")

  ordinary_menu_start <- regexpr(
    'if (self.model->kind == "scatter_matrix")', mac, fixed = TRUE
  )[[1]]
  expect_gt(ordinary_menu_start, 0L)
  ordinary_menu_tail <- substr(mac, ordinary_menu_start, nchar(mac))
  ordinary_menu_end <- regexpr(
    "- (void)keyDown:(NSEvent *)event", ordinary_menu_tail, fixed = TRUE
  )[[1]]
  expect_gt(ordinary_menu_end, 0L)
  ordinary_menu <- substr(ordinary_menu_tail, 1L, ordinary_menu_end - 1L)

  expect_match(
    ordinary_menu,
    'AddRCodeExportItems(exportMenu, "output", self.model->id',
    fixed = TRUE
  )
  expect_match(windows, "AppendPlotRCodeExportItems(exportMenu", fixed = TRUE)
})

test_that("multiple-imputation plots disclose and can change the displayed data", {
  root <- linkeda_source_test_root()
  core <- paste(readLines(file.path(
    root, "src", "core", "dataset_model.cpp"
  ), warn = FALSE), collapse = "\n")
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "ScatterPlotView.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(core, "PlotImputationDisplayStatus", fixed = TRUE)
  expect_match(core, "PooledEffectPlotImputationStatus", fixed = TRUE)
  expect_match(core,
    "this descriptive plot shows imputation", fixed = TRUE)
  expect_match(mac, "ImputationDisplaySubmenuItemForPlot", fixed = TRUE)
  expect_gte(length(gregexpr(
    "DrawPlotImputationDisplayStatus\\(", mac, perl = TRUE)[[1]]), 6L)
  expect_match(windows, "PlotImputationDisplayStatus", fixed = TRUE)
  expect_match(mac, 'model->kind == "glm_interaction"', fixed = TRUE)
  expect_match(windows, 'currentModel_->kind == "glm_interaction"', fixed = TRUE)
  expect_match(mac, "PooledEffectPlotImputationStatus", fixed = TRUE)
  expect_match(windows, "PooledEffectPlotImputationStatus", fixed = TRUE)
  expect_match(windows, '"SET_IMPUTATION_DISPLAY"', fixed = TRUE)
})

test_that("trellis export menus expose their ggplot2 verification recipe", {
  root <- linkeda_source_test_root()
  core <- paste(readLines(file.path(
    root, "src", "core", "command_dispatcher.cpp"
  ), warn = FALSE), collapse = "\n")
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(core, 'plot.kind == "trellis_scatterplot"', fixed = TRUE)
  expect_match(core, "ggplot2::facet_wrap", fixed = TRUE)

  trellis_starts <- gregexpr(
    "- (NSMenu *)trellisScatterplotMenuForEvent:", mac, fixed = TRUE
  )[[1]]
  trellis_start <- max(trellis_starts)
  expect_gt(trellis_start, 0L)
  trellis_tail <- substr(mac, trellis_start, nchar(mac))
  trellis_end <- regexpr(
    "- (void)runTrellisPanelAnalysis:", trellis_tail, fixed = TRUE
  )[[1]]
  expect_gt(trellis_end, 0L)
  trellis_menu <- substr(trellis_tail, 1L, trellis_end - 1L)
  expect_match(
    trellis_menu,
    'AddRCodeExportItems(exportMenu, "output", self.model->id, false)',
    fixed = TRUE
  )
  expect_match(
    windows,
    "RefreshBasicPlotCodeReference(\n                commandDispatcher_->applicationState(), model)",
    fixed = TRUE
  )
})

test_that("Windows comparison and model-trellis tables use the complete exporter", {
  root <- linkeda_source_test_root()
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE), collapse = "\n")
  header <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.h"
  ), warn = FALSE), collapse = "\n")

  for (symbol in c(
    "GeneralizedComparisonExportPayload",
    "ModelTrellisExportPayload"
  )) expect_match(windows, symbol, fixed = TRUE)

  expect_match(windows,
    "return GeneralizedComparisonExportPayload(state_);", fixed = TRUE)
  expect_match(windows,
    "return ModelTrellisExportPayload(state_);", fixed = TRUE)
  expect_gte(length(gregexpr(
    "exporter_=TableExportService::Create\\(window_,root\\)",
    windows, perl = TRUE)[[1]]), 3L)
  expect_gte(length(gregexpr(
    "std::shared_ptr<TableExportService> exporter_", header, fixed = TRUE)[[1]]),
    11L)
})

test_that("descriptive and test outputs register working R-code recipes", {
  root <- linkeda_source_test_root()
  core <- paste(readLines(file.path(
    root, "src", "core", "command_dispatcher.cpp"
  ), warn = FALSE), collapse = "\n")
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(core, "psych::corr.test", fixed = TRUE)
  # These recipes now live with the shared R analysis implementations.
  cluster_r <- paste(readLines(file.path(root, "R", "quick_cluster.R")), collapse = "\n")
  contingency_r <- paste(readLines(file.path(root, "R", "provenance.R")), collapse = "\n")
  expect_match(cluster_r, "stats::hclust", fixed = TRUE)
  expect_match(contingency_r, "base::table", fixed = TRUE)
  expect_match(core, "MeanComparisonCodeReference(state)", fixed = TRUE)
  expect_match(mac,
    'AddRCodeExportItems(exportMenu, "output", state->id, false)',
    fixed = TRUE)
  expect_match(windows,
    "AppendRCodeExportItems(exportMenu,commandCallback_,state_.id,false)",
    fixed = TRUE)
})
