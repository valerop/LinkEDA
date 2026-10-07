test_that("semantic variable type labels use the shared categorical vocabulary", {
  root <- linkeda_source_test_root()
  shared <- paste(readLines(file.path(root, "src", "core", "command_model.cpp"),
                            warn = FALSE), collapse = "\n")
  dataset <- paste(readLines(file.path(root, "src", "core", "dataset_model.cpp"),
                             warn = FALSE), collapse = "\n")

  expect_match(shared, '"Categorical", "factor"', fixed = TRUE)
  expect_match(shared, '"Numeric", "numeric"', fixed = TRUE)
  expect_match(shared, '"Ordinal", "ordered"', fixed = TRUE)
  expect_match(shared, "Treat predictor as categorical", fixed = TRUE)
  expect_match(shared, "Treat predictor as continuous", fixed = TRUE)
  expect_match(dataset, 'return "Categorical"', fixed = TRUE)
  expect_match(dataset, 'return "Ordinal"', fixed = TRUE)
  expect_match(dataset, 'return "Categorical (binary)"', fixed = TRUE)
  expect_false(grepl('{"Logical", "logical"', shared, fixed = TRUE))
  expect_false(grepl("\\bCategoric\\b", paste(shared, dataset), perl = TRUE))
})

test_that("macOS and Windows expose matching categorical model terminology", {
  root <- linkeda_source_test_root()
  paths <- c(
    file.path(root, "src", "platform", "macos", "linkeda_macos_app.mm"),
    file.path(root, "src", "platform", "windows", "winui", "LinkEDA",
              "WorkflowWindows.cpp"),
    file.path(root, "src", "platform", "windows", "winui", "LinkEDA",
              "ScatterPlotView.cpp"),
    file.path(root, "src", "platform", "windows", "winui", "LinkEDA",
              "App.xaml.cpp")
  )
  platform_files <- vapply(paths, function(path) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
  }, character(1L))
  source <- c(macOS = platform_files[[1L]],
              Windows = paste(platform_files[-1L], collapse = "\n"))

  for (platform_source in source) {
    expect_match(platform_source, "Categorical", fixed = TRUE)
    expect_match(platform_source, "Reference category", fixed = TRUE)
    expect_match(platform_source, "Event category", fixed = TRUE)
    expect_match(platform_source, "Treat predictor as categorical", fixed = TRUE)
    expect_match(platform_source, "Treat predictor as continuous", fixed = TRUE)
    expect_false(grepl("\\bCategoric\\b", platform_source, perl = TRUE))
    expect_false(grepl("Treat as factor", platform_source, fixed = TRUE))
    expect_false(grepl("Reference level", platform_source, fixed = TRUE))
  }

  expect_match(source[["macOS"]], 'type == "ordinal" ? "ordered"', fixed = TRUE)
  expect_match(source[["Windows"]], 'type == "ordinal" ? "ordered"', fixed = TRUE)
})

test_that("R verification code keeps real factor operations", {
  root <- linkeda_source_test_root()
  provenance <- paste(readLines(file.path(root, "R", "provenance.R"),
                                warn = FALSE), collapse = "\n")

  expect_match(provenance, "factor(data[[variable]])", fixed = TRUE)
  expect_match(provenance, "is.factor", fixed = TRUE)
  expect_false(grepl("\\bcategorical\\s*\\(", provenance, perl = TRUE))
  expect_false(grepl("as.numeric\\s*\\(\\s*factor", provenance, perl = TRUE))
})
