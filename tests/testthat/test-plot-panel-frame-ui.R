test_that("panel frames use the axis style consistently across native renderers", {
  root <- linkeda_source_test_root()
  mac_path <- file.path(root, "src", "platform", "macos", "linkeda_macos_app.mm")
  windows_path <- file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "ScatterPlotView.cpp"
  )
  svg_path <- file.path(root, "src", "core", "svg_writer.cpp")

  mac <- paste(readLines(mac_path, warn = FALSE), collapse = "\n")
  windows <- paste(readLines(windows_path, warn = FALSE), collapse = "\n")
  svg <- paste(readLines(svg_path, warn = FALSE), collapse = "\n")

  # AppKit must inset and draw the final frame after graph geometry, otherwise
  # the right/bottom half-strokes can be clipped or covered.
  expect_gte(
    lengths(regmatches(mac, gregexpr(
      "bezierPathWithRect:\\s*NSInsetRect\\((pr|panelPlotRect), 0\\.5, 0\\.5\\)",
      mac, perl = TRUE
    ))),
    5L
  )
  expect_false(grepl(
    "\\[theme\\.majorGrid setStroke\\];\\s*NSFrameRect\\(pr\\)",
    mac, perl = TRUE
  ))

  # A requested panel border is an axis, not a grid line.
  expect_false(grepl(
    "theme\\.showPanelBorder \\? Brush\\(theme\\.majorGrid\\)",
    windows, perl = TRUE
  ))
  expect_match(
    windows,
    "auto frame = Rectangle(rect, TransparentBrush(), Brush(theme.axis), 1.4);",
    fixed = TRUE
  )
  expect_gte(
    lengths(regmatches(windows, gregexpr(
      "AppendPanelFrame\\(plotCanvas_", windows, perl = TRUE
    ))),
    7L
  )
  expect_gte(
    lengths(regmatches(windows, gregexpr(
      "if \\(!theme\\.showPanelBorder\\)", windows, perl = TRUE
    ))),
    5L
  )
  expect_match(
    svg,
    'theme.showPanelBorder ? SvgColor(theme.axis) : "none"',
    fixed = TRUE
  )
})

test_that("plot themes are local in context menus and global in the menu bar", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(root, "src", "platform", "macos",
                                   "linkeda_macos_app.mm"), warn = FALSE), collapse = "\n")
  windows <- paste(readLines(file.path(root, "src", "platform", "windows",
                                       "winui", "LinkEDA", "ApplicationMenu.cpp"),
                             warn = FALSE), collapse = "\n")

  expect_match(mac, "AddPlotThemeSubmenu(viewMenu);", fixed = TRUE)
  expect_match(mac, "AddPlotThemeSubmenu(viewMenu, self);", fixed = TRUE)
  expect_match(mac, "changeLocalPlotTheme:", fixed = TRUE)
  expect_match(mac, "g_plotThemeRevision.fetch_add(1);", fixed = TRUE)
  expect_match(mac, 'owner ? ToNSString(titles.theme) : @"Global theme"', fixed = TRUE)
  expect_match(windows, 'L"Global theme"', fixed = TRUE)
})

test_that("straight fitted-line menus use one user-facing name", {
  root <- linkeda_source_test_root()
  paths <- c(
    file.path(root, "src", "core", "command_model.cpp"),
    file.path(root, "src", "core", "scatterplot_model.cpp"),
    file.path(root, "src", "platform", "macos", "linkeda_macos_app.mm"),
    file.path(root, "src", "platform", "windows", "winui", "LinkEDA",
              "ScatterPlotView.cpp")
  )
  source <- paste(unlist(lapply(paths, readLines, warn = FALSE)), collapse = "\n")
  expect_match(source, "Regression lines", fixed = TRUE)
  expect_match(source, "Remove all regression lines", fixed = TRUE)
})

test_that("interaction legend category names remain legible after selection", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "ScatterPlotView.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(
    windows,
    "legendLabel.Opacity(1.0);",
    fixed = TRUE
  )
  legend <- substr(windows,
    regexpr("auto legendLabel = Label(", windows, fixed = TRUE)[[1]],
    nchar(windows))
  attach <- regexpr("AttachBoxplotSelection(legendLabel,", legend,
                    fixed = TRUE)[[1]]
  layering <- regexpr("Controls::Canvas::SetZIndex(legendLabel, 50);", legend,
                      fixed = TRUE)[[1]]
  expect_gt(attach, 0L)
  expect_gt(layering, attach)
  expect_false(grepl(
    "anySelectedSeries ? Brush(color, alpha) : textBrush",
    windows, fixed = TRUE
  ))
  expect_match(
    mac,
    "NSForegroundColorAttributeName: theme.text",
    fixed = TRUE
  )
})

test_that("histogram bins keep black outlines above colour and selection layers", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "ScatterPlotView.cpp"
  ), warn = FALSE), collapse = "\n")
  svg <- paste(readLines(file.path(
    root, "src", "core", "svg_writer.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(mac, "[[NSColor blackColor] setStroke];", fixed = TRUE)
  expect_match(mac, "NSInsetRect(rect, 0.5, 0.5)", fixed = TRUE)
  expect_gte(
    lengths(regmatches(windows, gregexpr(
      "AppendHistogramBinOutline\\(plotCanvas_, bar\\.rect\\)",
      windows, perl = TRUE
    ))),
    2L
  )
  expect_match(
    windows,
    "auto outline = Rectangle(rect, TransparentBrush(), Brush(0, 0, 0), 1.0);",
    fixed = TRUE
  )
  expect_match(svg, 'SvgColor(PlotThemeWhite(0.0))', fixed = TRUE)
  expect_match(svg, "for (const HistogramBarSegment &segment : bar.colorSegments)",
               fixed = TRUE)
  expect_match(svg, "PlotLightColorForNameOrHex(segment.colorName)", fixed = TRUE)
  expect_match(windows, "LightPaletteColor(segment.colorName)", fixed = TRUE)
  expect_match(
    mac,
    "BarplotDataSheetToneColor(\n                segment.colorName, false",
    fixed = TRUE
  )
  expect_false(grepl(
    "NSBezierPath *outline = [NSBezierPath bezierPathWithRect:NSInsetRect(segmentRect",
    mac,
    fixed = TRUE
  ))
})

test_that("manual case colours feed histogram and bar compositions", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "ScatterPlotView.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(windows, "input.rowColors = pointColors_;", fixed = TRUE)
  expect_match(
    windows,
    "layer.localRect, layer.rows, selectedRows_, colors,",
    fixed = TRUE
  )
  expect_match(windows, "return theme.geomFill;", fixed = TRUE)
  expect_match(windows, "return theme.selectedMarkFill;", fixed = TRUE)
  expect_match(windows, "neutral ? 0.30 : 0.96", fixed = TRUE)
  expect_match(mac, "renderInput.rowColors = rowColors;", fixed = TRUE)
  expect_match(
    mac,
    "rows,\n            selection,\n            rowColors,",
    fixed = TRUE
  )
  expect_match(
    mac,
    "NSColor *base = selected ? theme.selectedMarkFill : theme.geomFill;",
    fixed = TRUE
  )
})

test_that("bar charts use the macOS reference presentation without patterns", {
  root <- linkeda_source_test_root()
  core <- paste(readLines(file.path(
    root, "src", "core", "barplot_model.cpp"
  ), warn = FALSE), collapse = "\n")
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "ScatterPlotView.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(
    core,
    "bool BarplotEncodingShowsPatterns(const PlotModel &model)\n{\n    (void)model;\n    return false;",
    fixed = TRUE
  )
  expect_false(grepl('segmentPatterns.Text(L"Segment pattern")', windows,
                     fixed = TRUE))
  expect_false(grepl("if (display.showSegmentEncodingMenu)", windows,
                     fixed = TRUE))
  expect_false(grepl("[menu addItem:levelPatternItem]", mac, fixed = TRUE))
  expect_false(grepl("if (displayMenuState.showSegmentEncodingMenu)", mac,
                     fixed = TRUE))
})

test_that("Windows bar hits use the general bar-chart context menu", {
  root <- linkeda_source_test_root()
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "ScatterPlotView.cpp"
  ), warn = FALSE), collapse = "\n")
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")

  expect_false(grepl("CreateBarplotSegmentMenu", windows, fixed = TRUE))
  expect_false(grepl("hit.ContextFlyout", windows, fixed = TRUE))
  expect_match(
    windows,
    "right-click bubbles to the general\n            // bar-chart context menu",
    fixed = TRUE
  )
  expect_match(windows, "BuildBarplotXMenuState(", fixed = TRUE)
  expect_match(windows, "BuildBarplotDisplayMenuState(", fixed = TRUE)
  expect_match(windows, "AttachWindowContextFlyout(window_, menu, false);",
               fixed = TRUE)
  expect_match(mac, "if (kShowExperimentalFeatures) {", fixed = TRUE)
  expect_match(mac, "// Bar Chart submenu", fixed = TRUE)
})

test_that("interaction plots redraw series and never use descriptions as labels", {
  root <- linkeda_source_test_root()
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "ScatterPlotView.cpp"
  ), warn = FALSE), collapse = "\n")
  glm <- paste(readLines(file.path(
    root, "src", "core", "glm_model.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(
    windows,
    "if (!currentModel_->interactionPlotLines.empty()) return false;",
    fixed = TRUE
  )
  label_function <- sub(
    ".*void ApplyRegressionInteractionVariableLabels", "",
    glm
  )
  label_function <- sub(
    "std::string RegressionEffectPlotTitle.*", "",
    label_function
  )
  expect_match(label_function, "column.displayName", fixed = TRUE)
  expect_false(grepl("column.description", label_function, fixed = TRUE))
})
