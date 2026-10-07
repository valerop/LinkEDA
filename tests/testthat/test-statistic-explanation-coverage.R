test_that("Explain statistic covers non-regression analyses on both native platforms", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(
    file.path(root, "src", "platform", "macos", "linkeda_macos_app.mm"),
    warn = FALSE, encoding = "UTF-8"
  ), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "WorkflowWindows.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  for (selector in c(
    "explainTableStatistic:",
    "explainMeanComparisonStatistic:",
    "explainCorrelationStatistic:",
    "explainDimensionalityStatistic:",
    "explainScaleStatistic:",
    "explainPairwiseStatistic:"
  )) {
    expect_match(mac, selector, fixed = TRUE)
  }

  for (surface in c(
    '"correlation"',
    '"dimensionality"',
    '"scale"',
    "InteractionReportStatisticContext",
    "columnsAreStatistics",
    "MeanComparisonCellFormat::Text"
  )) {
    expect_match(windows, surface, fixed = TRUE)
  }

  # Both front ends must delegate wording to the portable semantic engine.
  expect_match(mac, "ExplainModelStatistic(context)", fixed = TRUE)
  expect_match(windows, "ExplainModelStatistic(context)", fixed = TRUE)
})

test_that("the shared explanation catalogue distinguishes statistical semantics", {
  root <- linkeda_source_test_root()
  core <- paste(readLines(
    file.path(root, "src", "core", "glm_model.cpp"),
    warn = FALSE, encoding = "UTF-8"
  ), collapse = "\n")

  for (semantic in c(
    "observed count and percent",
    "reports its pooling method as",
    "Hodges–Lehmann location shift",
    "Rank-biserial correlation",
    "parallel-analysis eigenvalue",
    "Multiplicity note"
  )) {
    expect_match(core, semantic, fixed = TRUE)
  }
  expect_false(grepl(
    'original + " is a statistic reported for this fitted model',
    core, fixed = TRUE
  ))
})
