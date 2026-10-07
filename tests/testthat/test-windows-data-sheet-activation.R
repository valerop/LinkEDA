test_that("opening a Windows data sheet does not nest dataset commands", {
  root <- linkeda_source_test_root()
  sheet <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "DataSheetView.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(sheet, "void DataSheetView::QueueActiveDataset()", fixed = TRUE)
  expect_match(sheet, "window_.DispatcherQueue().TryEnqueue([weak]()",
               fixed = TRUE)
  expect_match(sheet, 'view->commandCallback_({"SET_ACTIVE_DATASET", view->group_});',
               fixed = TRUE)
  expect_equal(length(regmatches(sheet,
    gregexpr('commandCallback_\\(\\{"SET_ACTIVE_DATASET"', sheet))[[1]]), 1L)
  expect_true(length(regmatches(sheet,
    gregexpr("QueueActiveDataset();", sheet, fixed = TRUE))[[1]]) >= 3L)
})
