test_that("count exposure is a real log-offset specification on both native platforms", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- test_path("..", "..")
  windows_path <- file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  )
  windows_app_path <- file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  )
  macos_path <- file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  )
  model_path <- file.path(root, "R", "generalized_linear_model.R")
  skip_if_not(
    all(file.exists(c(windows_path, windows_app_path, macos_path, model_path))),
    "Native source checkout not available"
  )
  windows <- readLines(windows_path, warn = FALSE)
  windows_app <- readLines(windows_app_path, warn = FALSE)
  macos <- readLines(macos_path, warn = FALSE)
  model <- readLines(model_path, warn = FALSE)

  expect_true(any(grepl('send\\("GGLM_SET_EXPOSURE"', windows)))
  expect_true(any(grepl('name == "GGLM_SET_EXPOSURE"', windows_app, fixed = TRUE)))
  expect_true(any(grepl('offset\\(log\\(`%s`\\)\\)', model)))
  expect_true(any(grepl("countComparisonExposureSelected:", macos, fixed = TRUE)))
  expect_true(any(grepl("Exposure (log offset)", macos, fixed = TRUE)))
  expect_true(any(grepl("SetGeneralizedComparisonExposure", macos, fixed = TRUE)))
})

test_that("beta-binomial exposes and persists a trials value on Windows", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- test_path("..", "..")
  windows_path <- file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  )
  windows_app_path <- file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  )
  skip_if_not(
    all(file.exists(c(windows_path, windows_app_path))),
    "Native source checkout not available"
  )
  windows <- readLines(windows_path, warn = FALSE)
  windows_app <- readLines(windows_app_path, warn = FALSE)

  expect_true(any(grepl("CountDistributionUsesTrials(state_.countDistribution)",
                        windows, fixed = TRUE)))
  expect_true(any(grepl('send\\("GGLM_SET_TRIALS"', windows)))
  expect_true(any(grepl("state.trialsConstant", windows, fixed = TRUE)))
  expect_true(any(grepl('name == "GGLM_SET_TRIALS"', windows_app, fixed = TRUE)))
})
