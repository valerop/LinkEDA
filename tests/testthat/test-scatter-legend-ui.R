test_that("scatter legends use top-origin placement and a heading above their categories", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "ScatterPlotView.cpp"
  ), warn = FALSE), collapse = "\n")

  expect_match(mac, "return NSMakeRect(x, top, width, height);", fixed = TRUE)
  expect_match(mac, "NSMinY(frame) + 5.0", fixed = TRUE)
  expect_match(mac, "NSMinY(frame) + 35.0 + 22.0", fixed = TRUE)
  expect_match(mac, "model->colorByLegendY = top /", fixed = TRUE)
  expect_false(grepl(
    "NSHeight(bounds) - top - height", mac, fixed = TRUE
  ))

  header <- regexpr("content.Children().Append(header);", windows, fixed = TRUE)[1L]
  rows <- regexpr(
    "for (auto const& [level, colorName] : currentModel_->colorByLegendItems)",
    windows, fixed = TRUE
  )[1L]
  expect_gt(header, 0L)
  expect_gt(rows, header)
})
