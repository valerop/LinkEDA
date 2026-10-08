test_that("regression diagnostic menus use one cross-platform catalogue", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  mac_path <- file.path(root, "src", "platform", "macos", "linkeda_macos_app.mm")
  windows_path <- file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  )
  mac <- paste(readLines(mac_path, warn = FALSE), collapse = "\n")
  windows <- paste(readLines(windows_path, warn = FALSE), collapse = "\n")

  expect_match(mac, "static NSMenuItem *ModelDiagnosticPlotsMenuItem", fixed = TRUE)
  expect_match(mac, "ModelDiagnosticPlotOptions(includeExtended)", fixed = TRUE)
  expect_match(windows, "ModelDiagnosticPlotOptions(true)", fixed = TRUE)

  # These old labels identified independently assembled, reduced menus.  All
  # regression surfaces now use DefaultModelContextMenuTitles().openDiagnostics
  # and the shared seven-item catalogue instead.
  expect_false(grepl('initWithTitle:@"Diagnostics"', mac, fixed = TRUE))
  expect_false(grepl('initWithTitle:@"Diagnostic plots"', mac, fixed = TRUE))
  expect_false(grepl('Text(L"Diagnostics")', windows, fixed = TRUE))
  expect_false(grepl('Text(L"Diagnostic plots")', windows, fixed = TRUE))

  # A platform caller may not opt back into the historical two-item menu.
  expect_false(grepl("ModelDiagnosticPlotOptions(false)", mac, fixed = TRUE))
  expect_false(grepl("ModelDiagnosticPlotOptions(false)", windows, fixed = TRUE))
})

test_that("native diagnostic histograms are populated as histograms", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  mac <- paste(readLines(
    file.path(root, "src", "platform", "macos", "linkeda_macos_app.mm"),
    warn = FALSE, encoding = "UTF-8"
  ), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE), collapse = "\n")

  # Both native frontends must feed their histogram renderer through binned
  # histogram cases; scatter points with y = 0 produce an empty canvas.
  for (source in list(mac, windows)) {
    expect_match(source, 'data.kind == "histogram"', fixed = TRUE)
    expect_match(source, "histogramPoints.push_back", fixed = TRUE)
    expect_match(source, "RebinHistogramByRule", fixed = TRUE)
  }
  expect_match(mac, "static bool ApplyDiagnosticPlotData", fixed = TRUE)
  expect_gte(lengths(regmatches(
    mac, gregexpr("ApplyDiagnosticPlotData\\(", mac)
  )), 4L)
})

test_that("regression predictor menu terminology is consistent", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  paths <- c(
    file.path(root, "src", "platform", "macos", "linkeda_macos_app.mm"),
    file.path(root, "src", "platform", "windows", "winui", "LinkEDA",
              "WorkflowWindows.cpp")
  )
  source <- paste(unlist(lapply(paths, readLines, warn = FALSE)), collapse = "\n")

  expect_false(grepl("Reference level", source, fixed = TRUE))
  expect_false(grepl("Treat as factor", source, fixed = TRUE))
  expect_false(grepl("\\bCategoric\\b", source, perl = TRUE))
  expect_false(grepl("Center variable", source, fixed = TRUE))
  expect_false(grepl("Comparaciones por pares", source, fixed = TRUE))
  for (label in c("Analyze predictor", "Model terms", "Edit predictor",
                  "Reference category", "Center predictor", "Pairwise comparisons",
                  "Treat predictor as categorical")) {
    expect_match(source, label, fixed = TRUE)
  }
})

test_that("Windows single-model predictor menus share the macOS hierarchy", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  # Ordinary and generalized single-model views both group contextual actions
  # into the same three sections as their macOS counterparts.
  expect_gte(lengths(regmatches(
    windows,
    gregexpr('modelTerms.Text(L"Model terms")', windows, fixed = TRUE)
  )), 4L)
  expect_gte(lengths(regmatches(
    windows,
    gregexpr('edit.Text(L"Edit predictor")', windows, fixed = TRUE)
  )), 4L)
  expect_match(
    windows,
    "if(modelTerms.Items().Size()>0)menu.Items().Append(modelTerms);",
    fixed = TRUE
  )
  expect_match(
    windows,
    "if(edit.Items().Size()>0)menu.Items().Append(edit);",
    fixed = TRUE
  )
})

test_that("macOS regression context clicks do not run primary row actions", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(
    file.path(root, "src", "platform", "macos", "linkeda_macos_app.mm"),
    warn = FALSE
  ), collapse = "\n")

  expect_match(mac, "static bool PopContextMenuForMouseDownIfNeeded", fixed = TRUE)
  guards <- gregexpr(
    "if (PopContextMenuForMouseDownIfNeeded(self, event)) return;",
    mac, fixed = TRUE
  )[[1]]
  expect_gte(sum(guards > 0), 4L)
  direct_right_clicks <- gregexpr(
    "- \\(void\\)rightMouseDown:\\(NSEvent \\*\\)event[\\s\\S]{0,800}PopContextMenuForMouseDownIfNeeded\\(self, event\\);",
    mac, perl = TRUE
  )[[1]]
  expect_gte(sum(direct_right_clicks > 0), 4L)
  expect_match(mac, 'initWithTitle:@"Linear Model"', fixed = TRUE)
  expect_match(mac, "[menu addItem:[self regressionExportMenuItem]]", fixed = TRUE)

  # Both the generalized and ordinary single-model coefficient views must
  # consume Control-click before invoking their primary cell action. The two
  # comparison views use the same guard as well.
  expect_gte(lengths(regmatches(mac, gregexpr(
    "PopContextMenuForMouseDownIfNeeded\\(self, event\\)\\) return;[\\s\\S]{0,1800}coefficientCellClickedAtRow",
    mac, perl = TRUE
  ))), 2L)

  # A primary click on an interaction row opens the same action menu instead
  # of synchronously launching both the interpretation and the plot.
  expect_match(
    mac,
    "NSMenu *menu = [self contextMenuForCoefficientRow:row column:column];",
    fixed = TRUE
  )
  expect_false(grepl(
    "openInteractionReportForTerm:term\\];[[:space:]]*\\[self openInteractionPlotForTerm:term",
    mac, perl = TRUE
  ))
})

test_that("macOS effect plots for interactions always use R without blocking AppKit", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  mac <- paste(readLines(
    file.path(root, "src", "platform", "macos", "linkeda_macos_app.mm"),
    warn = FALSE
  ), collapse = "\n")
  windows <- paste(readLines(
    file.path(root, "src", "platform", "windows", "winui", "LinkEDA", "App.xaml.cpp"),
    warn = FALSE
  ), collapse = "\n")

  native_fast_paths <- gregexpr(
    "!multipleImputation && SplitInteractionTerm(term).size() == 2",
    mac, fixed = TRUE
  )[[1]]
  expect_equal(sum(native_fast_paths > 0), 0L)
  expect_false(grepl("PopulateGLMInteractionPlotModel(", mac, fixed = TRUE))
  expect_false(grepl("PopulateGLMInteractionPlotModel(", windows, fixed = TRUE))
  expect_match(mac, "static std::string ApplyRBackedInteractionPlotResult", fixed = TRUE)
  expect_match(
    windows,
    "services.ui.openLinearInteractionPlot[\\s\\S]{0,3000}RunRRegressionPostEstimation",
    perl = TRUE
  )
  expect_match(
    mac,
    "Calculating the effect plot in R[\\s\\S]{0,700}std::thread worker",
    perl = TRUE
  )
  expect_match(mac, "g_pendingInteractionPlotJobs.insert(jobKey)", fixed = TRUE)

  # R-backed interpretations must be asynchronous for the same reason. A
  # response change invalidates the old revision instead of presenting it in
  # the new model window.
  expect_match(
    mac,
    "Calculating the interaction interpretation in R[\\s\\S]{0,700}std::thread worker",
    perl = TRUE
  )
  expect_match(mac, "g_pendingInteractionReportJobs.insert(jobKey)", fixed = TRUE)
  expect_match(
    mac,
    "The model changed while the interaction was being interpreted",
    fixed = TRUE
  )
})

test_that("closed macOS effect plots cannot absorb a later result invisibly", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(
    file.path(root, "src", "platform", "macos", "linkeda_macos_app.mm"),
    warn = FALSE
  ), collapse = "\n")

  # Closing the native window unregisters both its view and portable plot.
  expect_match(mac, "NSWindowWillCloseNotification", fixed = TRUE)
  expect_match(mac, "closedPlotView", fixed = TRUE)
  expect_match(mac, "DetachPlotModelUnlocked(plotId)", fixed = TRUE)

  # A cached derived plot may be refreshed/reused only while it still owns a
  # visible native window. This covers old orphaned state as well as the close
  # notification race.
  open_view_guards <- gregexpr(
    "view.model == (candidate|plot) && window && \\[window isVisible\\]",
    mac, perl = TRUE
  )[[1]]
  expect_gte(sum(open_view_guards > 0), 2L)
})

test_that("effect plots expose the same axis and interval defaults on both platforms", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  core <- paste(readLines(file.path(root, "src", "core", "plot_geometry.h"),
                          warn = FALSE), collapse = "\n")
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "ScatterPlotView.cpp"
  ), warn = FALSE), collapse = "\n")
  windows_app <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(core, "bool regressionConnectEstimates = true;", fixed = TRUE)
  expect_match(core, "bool regressionConfidenceIntervalsVisible = false;", fixed = TRUE)
  for (source in list(mac, windows, windows_app)) {
    expect_match(source, "PLOT_REGRESSION_SET_EFFECT_X", fixed = TRUE)
  }
  expect_match(mac, "Horizontal-axis variable", fixed = TRUE)
  expect_match(windows, "Horizontal-axis variable", fixed = TRUE)
  expect_match(windows, "legendSample.PointerPressed", fixed = TRUE)
  expect_match(core, "interactionXTickRows", fixed = TRUE)
  expect_match(mac, "PlotEffectCategoryRows", fixed = TRUE)
  expect_match(windows, "PlotEffectCategoryRows", fixed = TRUE)

  # Effect plots are not arbitrary scatterplots: Y remains the fitted
  # response, and the only X choices come from the displayed interaction.
  expect_match(
    mac,
    'if (self.model->kind != "glm_interaction" && !self.model->isGLMDiagnostic)',
    fixed = TRUE
  )
  expect_equal(
    lengths(regmatches(
      mac,
      gregexpr(
        'if (self.model->kind != "glm_interaction" && !self.model->isGLMDiagnostic)',
        mac,
        fixed = TRUE
      )
    )),
    1L,
    info = paste(
      "The effect-plot guard must wrap the ordinary scatterplot Variables",
      "submenu, rather than an unrelated plot-specific menu."
    )
  )
  expect_match(
    mac,
    paste0(
      "menuState.variables.openVariablesWindow\\]\\];\\s*",
      "(?s:.*?)",
      'if \\(self\\.model->kind != "glm_interaction" && !self\\.model->isGLMDiagnostic\\) \\{\\s*',
      "\\[menu setSubmenu:variablesMenu forItem:variables\\];\\s*",
      "\\[menu addItem:variables\\];"
    ),
    perl = TRUE
  )
  expect_match(
    mac,
    'if (model->kind == "glm_interaction")',
    fixed = TRUE
  )
  expect_match(
    mac,
    "The response axis of an effect plot is fixed by its fitted model.",
    fixed = TRUE
  )
  for (source in list(mac, windows)) {
    expect_match(
      source,
      'kind == "glm_interaction"',
      fixed = TRUE
    )
    expect_match(
      source,
      "SplitInteractionTerm",
      fixed = TRUE
    )
  }
  expect_match(
    mac,
    "if (!isX) return;",
    fixed = TRUE
  )
  expect_match(
    windows,
    "if (!xAxis) return;",
    fixed = TRUE
  )

  # Applying a recalculated X axis must collect native views under the global
  # mutex and force drawing only after releasing it. drawRect: itself consults
  # linked selection under the same mutex.
  expect_match(
    mac,
    "Never[\\s\\S]{0,220}std::vector<ScatterView [*]> views;[\\s\\S]{0,350}for [(]ScatterView [*]view : views[)]",
    perl = TRUE
  )
})

test_that("comparison interactions report when the selected model omits the term", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  core <- paste(readLines(
    file.path(root, "src", "core", "glm_model.cpp"), warn = FALSE
  ), collapse = "\n")
  mac <- paste(readLines(
    file.path(root, "src", "platform", "macos", "linkeda_macos_app.mm"),
    warn = FALSE
  ), collapse = "\n")
  windows_app <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE), collapse = "\n")
  windows_view <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(core, "is not included in the active model", fixed = TRUE)
  expect_gte(lengths(regmatches(mac, gregexpr(
    "ComparisonInteractionNotIncludedMessage(term, model.label)",
    mac, fixed = TRUE
  ))), 4L)
  expect_match(windows_app,
               "regressionComparisonViews_.find(state.id)", fixed = TRUE)
  expect_match(windows_app,
               "generalizedComparisonViews_.find(state.id)", fixed = TRUE)
  expect_gte(lengths(regmatches(windows_view, gregexpr(
    "ShowWorkflowMessage(window_,L\"Interaction unavailable\",message)",
    windows_view, fixed = TRUE
  ))), 2L)
})

test_that("visual regression-table copies do not masquerade as text", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  mac <- paste(readLines(
    file.path(root, "src", "platform", "macos", "linkeda_macos_app.mm"),
    warn = FALSE
  ), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "TableExportService.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_false(grepl(
    "[pasteboard setString:_tabDelimited forType:NSPasteboardTypeString]",
    mac,
    fixed = TRUE
  ))
  expect_match(mac, "[self setWantsLayer:NO]", fixed = TRUE)
  expect_false(grepl(
    "package\\.SetBitmap[\\s\\S]{0,500}package\\.SetText",
    windows,
    perl = TRUE
  ))
  expect_match(windows, "package.SetBitmap", fixed = TRUE)
  expect_match(
    windows,
    "package.SetText(to_hstring(TableText(payload, false, false)))",
    fixed = TRUE
  )
})

test_that("native regression tables preserve omnibus p values on parent rows", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  core <- paste(readLines(
    file.path(root, "src", "core", "glm_model.cpp"), warn = FALSE
  ), collapse = "\n")
  mac <- paste(readLines(
    file.path(root, "src", "platform", "macos", "linkeda_macos_app.mm"),
    warn = FALSE
  ), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  # The shared linear-table formatter owns the factor-parent p cell.  Both
  # native frontends delegate their linear rows to it.
  expect_match(core, "if (parentRow) return std::isfinite(row.pValue)", fixed = TRUE)
  expect_match(mac, "GLMRegressionTableCell(\n            coef", fixed = TRUE)
  expect_match(windows, "GLMRegressionTableCellText(displayCells[column])", fixed = TRUE)

  # Generalized, binary, and count tables have platform-specific renderers;
  # only reference rows may suppress p.  Factor/term parents retain the
  # omnibus p delivered by the shared semantic result.
  expect_match(mac, "if (referenceRow) return @\"\\U00002014\";", fixed = TRUE)
  expect_match(mac, "return ToNSString(FormatPValue(coef.pValue));", fixed = TRUE)
  expect_match(windows, "reference ? std::string(\"\\xE2\\x80\\x94\") : FormatPValue(source.pValue)", fixed = TRUE)
  expect_match(windows, "reference?dash:FormatPValue(row.pValue)", fixed = TRUE)
})

test_that("all native regression surfaces expose marginal effect plots and confidence controls", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  mac <- paste(readLines(
    file.path(root, "src", "platform", "macos", "linkeda_macos_app.mm"),
    warn = FALSE, encoding = "UTF-8"
  ), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  windows_plot <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "ScatterPlotView.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_gte(lengths(regmatches(mac, gregexpr("Effect plot…", mac, fixed = TRUE))), 4L)
  expect_gte(lengths(regmatches(windows, gregexpr("Effect plot…", windows, fixed = TRUE))), 4L)
  expect_match(mac, "PLOT_REGRESSION_TOGGLE_CONFIDENCE_INTERVALS", fixed = TRUE)
  expect_match(mac, "PLOT_REGRESSION_TOGGLE_CONFIDENCE_LEVEL", fixed = TRUE)
  expect_match(windows_plot, "PLOT_REGRESSION_TOGGLE_CONFIDENCE_INTERVALS", fixed = TRUE)
  expect_match(windows_plot, "PLOT_REGRESSION_TOGGLE_CONFIDENCE_LEVEL", fixed = TRUE)
})

test_that("Count Model add-term hit testing uses its displayed layout", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  mac <- paste(readLines(
    file.path(root, "src", "platform", "macos", "linkeda_macos_app.mm"),
    warn = FALSE
  ), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE), collapse = "\n")
  windows_app <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE), collapse = "\n")

  # Drawing, hit testing, contextual menus, and popup positioning must all
  # receive the same Count Model layout flag. The former six-argument calls
  # interpreted a seven-column count table as a binary table and displaced
  # the clickable Add term row.
  calls <- regmatches(mac, gregexpr(
    "GeneralizedGLMReportLayout::Compute\\([\\s\\S]*?\\);",
    mac, perl = TRUE
  ))[[1L]]
  expect_gt(length(calls), 5L)
  expect_true(all(grepl("countRegression", calls, fixed = TRUE) |
                  grepl("countModel", calls, fixed = TRUE)))
  expect_match(mac, "else if (coefficientColumns == 7)", fixed = TRUE)
  expect_match(mac, "widths = countModel", fixed = TRUE)
  expect_match(mac, "GeneralizedExponentiatedEffectLabel", fixed = TRUE)

  # Multiple-imputation results retain their editable model specification on
  # Windows just as on macOS; precomputed non-MI tables remain read-only.
  expect_match(
    windows,
    "(!state_.precomputed || state_.multipleImputation)",
    fixed = TRUE
  )
  expect_match(
    windows_app,
    "if (state.precomputed && !state.multipleImputation) return;",
    fixed = TRUE
  )
  expect_match(windows, "GeneralizedExponentiatedEffectLabel", fixed = TRUE)
})

test_that("Count Model tables expose the complete export menu", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  mac <- paste(readLines(
    file.path(root, "src", "platform", "macos", "linkeda_macos_app.mm"),
    warn = FALSE
  ), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE), collapse = "\n")

  # AppKit previously exposed this complete menu only for binary regression.
  # All generalized-model variants now use the same entry point; binary-only
  # CSV and APA extras remain conditional within that menu.
  expect_match(mac, "- (NSMenuItem *)modelExportMenuItem", fixed = TRUE)
  expect_false(grepl("binaryExportMenuItem", mac, fixed = TRUE))
  expect_match(mac, "state->countRegression[\\s\\S]*?\\? \"count-model\"", perl = TRUE)
  expect_match(mac, "AddRCodeExportItems(exportMenu, \"output\", state->id, true)", fixed = TRUE)
  expect_gte(length(gregexpr(
    "[menu addItem:[self modelExportMenuItem]]", mac, fixed = TRUE
  )[[1]]), 2L)

  # WinUI already exports every GeneralizedModelView payload, including count
  # models and their rate-ratio columns, through the shared table exporter.
  expect_match(windows, "GeneralizedModelExportPayload(state_)", fixed = TRUE)
  expect_match(windows, "GeneralizedModelHasExponentiatedEffect(state)", fixed = TRUE)
  expect_match(windows, "GeneralizedExponentiatedEffectLabel(state)", fixed = TRUE)
})

test_that("negative-binomial theta is distinct from quasi-Poisson dispersion", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  mac <- paste(readLines(
    file.path(root, "src", "platform", "macos", "linkeda_macos_app.mm"),
    warn = FALSE
  ), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(mac, "CountModelParameterSummary(*state)", fixed = TRUE)
  expect_match(windows, "CountModelParameterSummary(state)", fixed = TRUE)
  core <- paste(readLines(file.path(root, "src", "core", "glm_model.cpp")), collapse = "\n")
  expect_match(core, "Theta", fixed = TRUE)
  expect_match(core, "not Rubin-pooled", fixed = TRUE)
  expect_match(windows, "CountDistribution::QuasiPoisson", fixed = TRUE)
})
