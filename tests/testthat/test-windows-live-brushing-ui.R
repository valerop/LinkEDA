test_that("Windows brushing publishes selection while the pointer moves", {
  root <- normalizePath(testthat::test_path("..", ".."),
                        winslash = "/", mustWork = TRUE)
  source_path <- file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "ScatterPlotView.cpp"
  )
  header_path <- file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "ScatterPlotView.h"
  )
  skip_if_not(
    all(file.exists(c(source_path, header_path))),
    "Native source checkout not available"
  )
  source <- paste(readLines(source_path, warn = FALSE, encoding = "UTF-8"),
                  collapse = "\n")
  header <- paste(readLines(header_path, warn = FALSE, encoding = "UTF-8"),
                  collapse = "\n")

  update <- regmatches(source, regexpr(
    "void ScatterPlotView::UpdateSelection[\\s\\S]*?void ScatterPlotView::CompleteSelection",
    source, perl = TRUE
  ))
  expect_length(update, 1L)
  expect_match(update, "PublishBrushSelection(x, y)", fixed = TRUE)
  expect_match(update, "RowsForFixedBrush", fixed = TRUE)
  expect_match(update, "ApplySelectionOperation", fixed = TRUE)
  expect_match(update, "brushSelectionBase_", fixed = TRUE)
  expect_match(update, "brushPublishedSelection_", fixed = TRUE)
  expect_match(
    update,
    "selectionCallback_(group_, next, ::rlispstat::core::SelectionMode::Replace)",
    fixed = TRUE
  )
  expect_match(source, "pointsCanvas_.CapturePointer(event.Pointer())", fixed = TRUE)
  expect_match(source, "pointsCanvas_.ReleasePointerCapture(event.Pointer())", fixed = TRUE)
  expect_match(header, "std::set<int> brushSelectionBase_", fixed = TRUE)
  expect_match(header, "bool brushSelectionPublished_ = false", fixed = TRUE)
})
