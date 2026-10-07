test_that("table insertion actions use the shared LinkEDA inline style", {
  root <- linkeda_source_test_root()
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(windows, "Controls::Button LinkEDAInlineActionButton", fixed = TRUE)
  for (style in c(
    "button.Foreground(Brush(105, 105, 105))",
    "button.HorizontalContentAlignment(HorizontalAlignment::Left)",
    "button.Background(TransparentBrush())",
    "button.BorderBrush(TransparentBrush())",
    "button.BorderThickness(Thickness{ 0, 0, 0, 0 })",
    "button.Padding(Thickness{ 8, 4, 8, 4 })"
  )) expect_match(windows, style, fixed = TRUE)

  expect_match(
    windows,
    'addModelVariable_=LinkEDAInlineActionButton(L"+ Add variable")',
    fixed = TRUE
  )
  expect_match(
    windows,
    'paired ? L"+ Add pair" : L"+ Add variable"',
    fixed = TRUE
  )
  expect_gte(lengths(regmatches(
    windows,
    gregexpr('LinkEDAInlineActionButton(L"+ Add term")', windows, fixed = TRUE)
  )), 2L)

  # Do not regress to a default framed WinUI button for the table affordances.
  expect_false(grepl(
    'Controls::Button();addModelVariable_.Content(box_value(L"+ Add variable"))',
    windows, fixed = TRUE
  ))
})

test_that("model inline term menus refresh with response and term changes", {
  root <- linkeda_source_test_root()
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(
    windows,
    "termRowsInitialized_ = false;",
    fixed = TRUE
  )
  expect_match(
    windows,
    "if (controlsChanged || !reportInitialized_ || scopeNoticeChanged",
    fixed = TRUE
  )
  expect_match(
    windows,
    "coefficientRowsInitialized_ = false;",
    fixed = TRUE
  )
})
