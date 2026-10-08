test_that("conditioning menus flatten redundant singleton levels", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "ScatterPlotView.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(windows, "AppendMenuGroupWithoutRedundantSingleLevel", fixed = TRUE)
  expect_match(windows, "if (count == 1)", fixed = TRUE)
  expect_match(windows, "group.Items().RemoveAt(0)", fixed = TRUE)
  expect_match(
    windows,
    "AppendMenuGroupWithoutRedundantSingleLevel(menu, conditioningMenu)",
    fixed = TRUE
  )
  expect_gte(lengths(regmatches(
    windows,
    gregexpr(
      "AppendMenuGroupWithoutRedundantSingleLevel(addCondition,",
      windows, fixed = TRUE
    )
  )), 2L)

  expect_match(mac, "AddMenuGroupWithoutRedundantSingleLevel", fixed = TRUE)
  expect_match(mac, "if (count == 1)", fixed = TRUE)
  expect_match(mac, "if ([conditions numberOfItems] == 0) return addRoot;",
               fixed = TRUE)
})
