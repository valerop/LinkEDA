test_that("Welcome exposes one compact statistical-example chooser", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WelcomeWindow.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  app <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  core <- paste(readLines(file.path(root, "src", "core", "welcome_model.cpp"),
                          warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(mac, "Open a statistical example…", fixed = TRUE)
  expect_match(mac, "NSTableViewDataSource", fixed = TRUE)
  expect_match(mac, "DefaultWelcomeExampleItems()", fixed = TRUE)
  expect_false(grepl("WelcomeActionButton\\(ToNSString\\(firstExample", mac))

  expect_match(windows, "Open a statistical example…", fixed = TRUE)
  expect_match(app, "Choose a statistical example", fixed = TRUE)
  expect_match(app, "DefaultWelcomeExampleItems()", fixed = TRUE)
  expect_match(core, '"Alien", "Alien"', fixed = TRUE)
  expect_false(grepl('ActionButton\\(L".*mtcars', windows))
  expect_false(grepl('ActionButton\\(L".*Alien', windows))
})

test_that("Windows Welcome keeps the full identity tagline visible", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  windows_lines <- readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WelcomeWindow.cpp"
  ), warn = FALSE, encoding = "UTF-8")
  windows <- paste(windows_lines, collapse = "\n")

  expect_match(windows, 'Label\\(L"Interactive exploratory data\\\\nanalysis"')
  width_line <- grep(
    "identityColumn\\.Width\\(GridLengthHelper::FromPixels",
    windows_lines,
    value = TRUE
  )
  expect_length(width_line, 1L)
  width <- sub(
    ".*identityColumn\\.Width\\(GridLengthHelper::FromPixels\\(([0-9]+)\\)\\).*",
    "\\1",
    width_line
  )
  expect_true(as.numeric(width) >= 390)
})

test_that("Windows statistical examples open larger and on the welcome monitor", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WelcomeWindow.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(windows, 'requestId_ == "welcome-examples" ? 720 : 470',
               fixed = TRUE)
  expect_match(windows, 'requestId_ == "welcome-examples" ? 540 : 365',
               fixed = TRUE)
  show <- substr(windows,
    regexpr("void RDataFrameChooserWindow::Show()", windows, fixed = TRUE)[[1]],
    regexpr("void RDataFrameChooserWindow::Complete(", windows,
            fixed = TRUE)[[1]] - 1L)
  expect_match(show, 'CenterOnOwnerMonitor(window_, owner_);', fixed = TRUE)
  expect_match(windows,
               'MonitorFromWindow(reference, MONITOR_DEFAULTTONEAREST)',
               fixed = TRUE)
  expect_match(windows, 'const auto& work = monitor.rcWork;', fixed = TRUE)
})

test_that("Windows examples use one-line labels with description tooltips", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  app <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  chooser <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WelcomeWindow.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(app,
               'item.displayName + "  —  " + item.category });', fixed = TRUE)
  expect_false(grepl('"\\n" + item.description', app, fixed = TRUE))
  expect_match(chooser, 'if (requestId_ == "welcome-examples")', fixed = TRUE)
  expect_match(chooser, 'label.TextWrapping(TextWrapping::NoWrap);',
               fixed = TRUE)
  expect_match(chooser, 'label.TextTrimming(TextTrimming::CharacterEllipsis);',
               fixed = TRUE)
  expect_match(chooser, 'to_hstring(example->description)', fixed = TRUE)
})

test_that("Trellis time-series creation separates series from panel conditioning", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(mac, 'GLMLabel(@"Series"', fixed = TRUE)
  expect_match(mac, "seriesVariables", fixed = TRUE)
  expect_match(mac, "trellisSpecification.groupingVariableId = seriesVariable",
               fixed = TRUE)
  expect_match(mac, "Condition on creates panels; Series draws separate trajectories.",
               fixed = TRUE)
})
