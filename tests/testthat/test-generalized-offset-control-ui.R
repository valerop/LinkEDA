test_that("generalized models expose offset as a first-class control", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(mac, '_offsetLabel = GLMLabel(@"Offset"', fixed = TRUE)
  expect_match(mac, "[_offsetPopup setAction:@selector(offsetChanged:)]", fixed = TRUE)
  expect_match(mac, "[_offsetPopup setHidden:!supportsOffset]", fixed = TRUE)
  expect_match(mac, "state->offsetVariable = value", fixed = TRUE)
  expect_match(mac, "[_statusField setLineBreakMode:NSLineBreakByWordWrapping]", fixed = TRUE)
  expect_match(mac, "[_statusField setMaximumNumberOfLines:2]", fixed = TRUE)

  expect_match(windows, 'second.Children().Append(label(L"Offset:"))', fixed = TRUE)
  expect_match(windows, 'send("GGLM_SET_OFFSET"', fixed = TRUE)
  expect_match(windows, "Numeric variable added to the linear predictor", fixed = TRUE)
})
