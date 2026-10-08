test_that("generalized models expose offset as a first-class control", {
  root <- linkeda_source_test_root()
  windows_file <- c(
    file.path(root, "src", "platform", "windows", "winui", "rlispstatWinUI",
              "WorkflowWindows.cpp"),
    file.path(root, "src", "platform", "windows", "winui", "LinkEDA",
              "WorkflowWindows.cpp")
  )
  windows_file <- windows_file[file.exists(windows_file)][1L]
  skip_if(is.na(windows_file), "Windows UI sources are absent from this checkout")
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")
  windows <- paste(readLines(windows_file, warn = FALSE), collapse = "\n")

  expect_match(mac, '_offsetLabel = GLMLabel(@"Link offset"', fixed = TRUE)
  expect_match(mac, "[_offsetPopup setAction:@selector(offsetChanged:)]", fixed = TRUE)
  expect_match(mac, "[_offsetPopup setHidden:!supportsOffset]", fixed = TRUE)
  expect_match(mac, "state->offsetVariable = value", fixed = TRUE)
  expect_match(mac, "[_statusField setLineBreakMode:NSLineBreakByWordWrapping]", fixed = TRUE)
  expect_match(mac, "[_statusField setMaximumNumberOfLines:2]", fixed = TRUE)

  expect_match(windows, 'second.Children().Append(label(L"Link offset:"))', fixed = TRUE)
  expect_match(windows, 'send("GGLM_SET_OFFSET"', fixed = TRUE)
  expect_match(windows, "values are used as stored on the link scale", fixed = TRUE)
})
