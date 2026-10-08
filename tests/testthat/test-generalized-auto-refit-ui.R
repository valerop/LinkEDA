test_that("generalized-model Auto-refit matches the macOS interaction", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  windows_ui <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  windows_app <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  windows_header <- regmatches(windows_ui, regexpr(
    "void GeneralizedModelView::RebuildControls\\(\\)[\\s\\S]*?void GeneralizedModelView::ConfigureContextMenu",
    windows_ui, perl = TRUE
  ))
  expect_length(windows_header, 1L)

  expect_match(mac, "[_autoRefitButton setButtonType:NSButtonTypeSwitch]",
               fixed = TRUE)
  expect_match(mac, "if (state->autoRefit) [self markChangedAndRefresh]",
               fixed = TRUE)

  # Windows presents the same persistent switch in the controls row. A manual
  # refit is offered only while Auto-refit is disabled, not as a redundant
  # permanent command or a misleading button beside the residual selector.
  expect_match(windows_header, "auto autoRefit=Controls::CheckBox()", fixed = TRUE)
  expect_match(windows_header, "GLMAutoRefitButtonTitle()", fixed = TRUE)
  expect_match(windows_header, "second.Children().Append(autoRefit)", fixed = TRUE)
  expect_false(grepl('refit.Content(box_value(L"Refit"))', windows_header,
                     fixed = TRUE))
  expect_match(windows_ui, 'refit.Text(L"Refit model")', fixed = TRUE)
  expect_match(windows_ui, 'send("GGLM_REFIT", "")', fixed = TRUE)
  expect_match(windows_ui, "if (!state_.autoRefit)", fixed = TRUE)

  auto_position <- regexpr("second.Children().Append(autoRefit)", windows_header,
                           fixed = TRUE)[[1]]
  residual_position <- regexpr('second.Children().Append(label(L"Residuals:"))',
                               windows_header, fixed = TRUE)[[1]]
  expect_gt(auto_position, 0L)
  expect_gt(residual_position, auto_position)

  # Re-enabling is an explicit retry on macOS, not merely a stale-state check.
  expect_match(windows_app, "forceRefit = state.autoRefit;", fixed = TRUE)
  expect_false(grepl(
    "forceRefit = state.autoRefit &&\n                    (!state.ok || state.fitVersion < state.modelVersion);",
    windows_app, fixed = TRUE
  ))
})
