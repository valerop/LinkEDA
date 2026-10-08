test_that("scatter brushing coalesces linked redraws until mouse-up", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")

  sampled <- regexpr(
    "if (_brushStepChanged) {", mac, fixed = TRUE
  )[[1L]]
  after_sampled <- substr(mac, sampled, nchar(mac))
  ended_relative <- regexpr(
    "- (void)endBrushGesture\n{", after_sampled, fixed = TRUE
  )[[1L]]
  ended <- sampled + ended_relative - 1L
  expect_gt(sampled, 0L)
  expect_gt(ended, sampled)
  section <- substr(mac, sampled, ended - 1L)
  expect_match(section,
    "RedrawGroupViewsSynchronouslyOnMain(self.model->group, false);",
    fixed = TRUE)
  expect_match(section, "[self displayIfNeeded];", fixed = TRUE)
  expect_false(grepl("refreshLinkedSelectionForGroup", section, fixed = TRUE))

  end_section <- substr(mac, ended, min(nchar(mac), ended + 2600L))
  expect_match(end_section,
    "PublishSpreadPlotMessage(\"ROW_SELECTION_CHANGED\"",
    fixed = TRUE)
  expect_match(end_section,
    "RedrawGroupViewsSynchronouslyOnMain(self.model->group, true);",
    fixed = TRUE)
})
