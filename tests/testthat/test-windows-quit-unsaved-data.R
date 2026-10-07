test_that("Windows asks to save only after data changes", {
  root <- linkeda_source_test_root()
  app <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(app, "RecordInitialDataBaseline(group);", fixed = TRUE)
  expect_match(app, "EncodeLinkEDADataChangePayload(", fixed = TRUE)
  quit <- substr(app, regexpr("void App::QuitApplication()", app, fixed = TRUE)[[1]],
                 nchar(app))
  quit <- substr(quit, 1L,
                 regexpr("void App::InitializeDispatcher()", quit,
                         fixed = TRUE)[[1]] - 1L)
  expect_match(quit, "baseline->second != currentData", fixed = TRUE)
  expect_match(quit, "if (!changed) continue;", fixed = TRUE)
  expect_false(grepl("if (!neverSaved && !changed) continue;", quit, fixed = TRUE))
})
