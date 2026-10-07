test_that("Scale Analysis picker keeps native range selection and adds a batch", {
  root <- linkeda_source_test_root()
  view <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  start <- regexpr("void ScaleAnalysisView::Initialize()", view,
                   fixed = TRUE)[[1]]
  end <- regexpr("void ScaleAnalysisView::ConfigureContextMenu()", view,
                 fixed = TRUE)[[1]]
  expect_gt(start, 0L)
  expect_gt(end, start)
  picker <- substr(view, start, end - 1L)

  expect_match(picker,
               "addItemList_.SelectionMode(Controls::ListViewSelectionMode::Extended);",
               fixed = TRUE)
  expect_false(grepl("addItemList_.ItemClick", picker, fixed = TRUE))
  expect_match(picker, 'addSelectedItems_.Content(box_value(L"Add selected items"));',
               fixed = TRUE)
  expect_match(picker, "addItemList_.SelectAll();", fixed = TRUE)
  expect_match(picker, "for (auto const& item : addItemList_.SelectedItems())",
               fixed = TRUE)
  expect_match(picker, "AddQuickItems(items);", fixed = TRUE)

  batch <- substr(picker,
    regexpr("void ScaleAnalysisView::AddQuickItems(", picker, fixed = TRUE)[[1]],
    nchar(picker))
  expect_equal(lengths(regmatches(batch, gregexpr("Submit(std::move(candidate));",
                                               batch, fixed = TRUE))), 1L)
})

test_that("Scale Analysis item rows align and derived windows use plain status", {
  root <- linkeda_source_test_root()
  view <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  start <- regexpr("void ScaleAnalysisView::Initialize()", view,
                   fixed = TRUE)[[1]]
  scale <- substr(view, start, nchar(view))

  expect_false(grepl("rowIndex % 2 == 1", scale, fixed = TRUE))
  expect_false(grepl("Analysis information...", scale, fixed = TRUE))
  expect_false(grepl("ShowAnalysisInformation", scale, fixed = TRUE))
  expect_false(grepl("exact specification fingerprint", view, fixed = TRUE))
  expect_match(scale, 'derived->status.Text(L"");', fixed = TRUE)
  expect_match(scale, 'if (s.status != "value") appendNote(s.status);',
               fixed = TRUE)
  expect_match(view, 'if (s.status != "value" && !s.status.empty())',
               fixed = TRUE)
})
