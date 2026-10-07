test_that("Windows linear-model Auto-refit matches macOS", {
  root <- linkeda_source_test_root()
  path <- function(...) file.path(root, "src", ...)
  source_text <- function(file) paste(readLines(file, warn = FALSE,
                                               encoding = "UTF-8"), collapse = "\n")
  mac <- source_text(path("platform", "macos", "linkeda_macos_app.mm"))
  ui <- source_text(path("platform", "windows", "winui", "LinkEDA",
                         "WorkflowWindows.cpp"))
  app <- source_text(path("platform", "windows", "winui", "LinkEDA",
                          "App.xaml.cpp"))
  core <- source_text(path("core", "command_dispatcher.cpp"))

  controls <- regmatches(ui, regexpr(
    "void LinearModelView::RebuildControls\\(\\)[\\s\\S]*?void LinearModelView::Show",
    ui, perl = TRUE))
  expect_length(controls, 1L)
  expect_match(mac, "[_autoRefitButton setTitle:ToNSString(rlispstat::core::GLMAutoRefitButtonTitle())]",
               fixed = TRUE)
  expect_match(controls, "GLMAutoRefitButtonTitle()", fixed = TRUE)
  expect_match(controls, "autoRefit.IsChecked(model_.autoRefit)", fixed = TRUE)
  expect_match(controls,
               "autoRefit.IsEnabled(!model_.precomputed || model_.multipleImputation)",
               fixed = TRUE)
  expect_match(controls, 'commandCallback_({"MODEL_TOGGLE_AUTO",', fixed = TRUE)
  expect_match(ui, 'send("MODEL_REFIT","")', fixed = TRUE)

  expect_match(app, "if (!state.autoRefit && !force)", fixed = TRUE)
  expect_match(app, "QueueLinearModelFit(group, true);", fixed = TRUE)
  expect_match(app, 'if (name == "MODEL_TOGGLE_AUTO"', fixed = TRUE)
  expect_match(app, "state.rFitPending = false;", fixed = TRUE)
  expect_match(core, "if (!state.autoRefit && !state.rFitPending) return", fixed = TRUE)
})
