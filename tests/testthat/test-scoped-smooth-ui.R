test_that("live scatter and diagnostic curves refit from the displayed cases", {
  root <- linkeda_source_test_root()
  windows <- paste(readLines(file.path(root, "src", "platform", "windows",
                                      "winui", "LinkEDA", "App.xaml.cpp"),
                             warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  macos <- paste(readLines(file.path(root, "src", "platform", "macos",
                                    "linkeda_macos_app.mm"),
                           warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  windows_task <- regmatches(windows, regexpr(
    "void App::QueueSmoothRecompute\\([\\s\\S]*?void App::QueueExistingSmoothRecompute",
    windows, perl = TRUE))
  expect_length(windows_task, 1L)
  expect_match(windows_task, "task.useVisibleRows = true", fixed = TRUE)
  expect_match(windows_task, "task.visibleRows.push_back(point.row)", fixed = TRUE)

  windows_diagnostic <- regmatches(windows, regexpr(
    "void App::RefreshDiagnosticPlots\\([\\s\\S]*?void App::RefreshSelectionVisuals",
    windows, perl = TRUE))
  expect_length(windows_diagnostic, 1L)
  expect_match(windows_diagnostic,
               "if (!ApplyDiagnosticPlotData(*model, data)) continue;\n            //",
               fixed = TRUE)
  expect_match(windows_diagnostic, "QueueExistingSmoothRecompute(*model)",
               fixed = TRUE)

  expect_match(macos, "task.useVisibleRows = true", fixed = TRUE)
  macos_diagnostic <- regmatches(macos, regexpr(
    "static void RefreshGLMDiagnosticPlotsNow\\([\\s\\S]*?static void RefreshGLMDiagnosticPlotsOnMain",
    macos, perl = TRUE))
  expect_length(macos_diagnostic, 1L)
  expect_match(macos_diagnostic,
               "RequestSmoothRecomputeForPlotUnlocked(diag)", fixed = TRUE)
})
