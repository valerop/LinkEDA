test_that("mean-comparison editing controls are consistent across native platforms", {
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

  for (source in list(mac, windows_view)) {
    expect_match(source, "+ Add variable", fixed = TRUE)
    expect_match(source, "+ Add pair", fixed = TRUE)
    expect_match(source, "Direction of difference", fixed = TRUE)
    expect_false(grepl('"Reference group"', source, fixed = TRUE))
    expect_match(source, "MeanComparisonIndependentGroupOrderForReference", fixed = TRUE)
  }

  expect_false(grepl("for(auto const& warning:state_.warnings)",
                     windows_view, fixed = TRUE))
  expect_match(windows_view, 'menu.Items().Append(descriptives);', fixed = TRUE)
  expect_match(windows_view, 'menu.Items().Append(openDescriptives);', fixed = TRUE)
  expect_false(grepl('display.Items().Append(openDescriptives);',
                     windows_view, fixed = TRUE))

  render_start <- regexpr("void MeanComparisonView::Render()", windows_view,
                          fixed = TRUE)[[1]]
  render_end <- regexpr("void MeanComparisonView::Activate()", windows_view,
                        fixed = TRUE)[[1]]
  expect_gt(render_start, 0L)
  expect_gt(render_end, render_start)
  render <- substr(windows_view, render_start, render_end - 1L)
  positions <- vapply(c(
    "content_.Children().Append(grid);",
    "content_.Children().Append(addVariable);",
    "content_.Children().Append(message);"
  ), function(text) regexpr(text, render, fixed = TRUE)[[1]], integer(1))
  expect_true(all(positions > 0L) && all(diff(positions) > 0L))
  expect_equal(lengths(regmatches(render, gregexpr("messages.push_back(text);",
                                               render, fixed = TRUE))), 2L)
  mean_comparison_view <- substr(windows_view,
    regexpr("MeanComparisonView::Create(bool descriptivesOnly)", windows_view,
            fixed = TRUE)[[1]], render_end)
  expect_match(mean_comparison_view, 'state_.analysisType == "one_way_anova"',
               fixed = TRUE)
  menu_start <- regexpr("void MeanComparisonView::ConfigureContextMenu()",
                        windows_view, fixed = TRUE)[[1]]
  controls_start <- regexpr("void MeanComparisonView::RebuildControls()",
                            windows_view, fixed = TRUE)[[1]]
  expect_gt(menu_start, 0L)
  expect_gt(controls_start, menu_start)
  menu_source <- substr(windows_view, menu_start, controls_start - 1L)
  expect_false(grepl("MEAN_SET_GROUP", menu_source, fixed = TRUE))
  expect_match(menu_source, "Direction of difference", fixed = TRUE)
  controls_source <- substr(windows_view, controls_start, render_start - 1L)
  expect_match(controls_source, "MEAN_SET_GROUP", fixed = TRUE)

  expect_match(mac, "MeanComparisonIndependentGroupOrderForReference", fixed = TRUE)
  expect_match(windows_app, "MEAN_SET_REFERENCE_GROUP", fixed = TRUE)
  expect_match(windows_app, "MeanComparisonIndependentGroupOrderForReference", fixed = TRUE)

  obsolete_controls <- c(
    "MEAN_SET_FIRST_GROUP", "MEAN_SET_SECOND_GROUP", "MEAN_SWAP_GROUPS",
    "_firstGroupPopup", "_secondGroupPopup", "_swapGroupsButton"
  )
  combined <- paste(mac, windows_view, windows_app, collapse = "\n")
  for (control in obsolete_controls) {
    expect_false(grepl(control, combined, fixed = TRUE), info = control)
  }
})
