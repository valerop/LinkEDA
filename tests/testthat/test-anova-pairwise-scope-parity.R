test_that("ANOVA pairwise results follow the ANOVA's fitted rows on both platforms", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(root, "src/platform/macos/linkeda_macos_app.mm"),
                         warn = FALSE), collapse = "\n")
  windows_view <- paste(readLines(file.path(root,
    "src/platform/windows/winui/rlispstatWinUI/WorkflowWindows.cpp"),
    warn = FALSE), collapse = "\n")
  windows_app <- paste(readLines(file.path(root,
    "src/platform/windows/winui/rlispstatWinUI/App.xaml.cpp"),
    warn = FALSE), collapse = "\n")

  expect_match(mac, 'link.sourceModelKind = "mean_comparison"', fixed = TRUE)
  expect_match(mac, 'fit.rowsUsed = row->originalRowIndices', fixed = TRUE)
  expect_match(mac, 'RefreshPairwiseDerivedOutputsForModelNow(copy.id + "\\x1f" + response)',
               fixed = TRUE)
  expect_match(mac, 'MarkPairwiseDerivedResultsPendingOnMain(', fixed = TRUE)
  expect_match(mac, 'if (!_plotLink.factor.empty() && _plotLink.factor != result.term) return;',
               fixed = TRUE)
  expect_match(windows_view, 'send("MEAN_PAIRWISE", response)', fixed = TRUE)
  expect_match(windows_app, 'fit.rowsUsed = rowsUsed;', fixed = TRUE)
  expect_match(windows_app, 'previous comparison results remain visible', fixed = TRUE)
  expect_match(windows_app, 'if (!row.variable.empty() && outputStates_.count(outputId))',
               fixed = TRUE)
  expect_match(windows_app, 'current->second != generation', fixed = TRUE)
})
