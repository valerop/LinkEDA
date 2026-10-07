test_that("Windows rich table clipboard output is compact and fits Word", {
  source_path <- testthat::test_path(
    "..", "..", "src", "platform", "windows", "winui",
    "LinkEDA", "TableExportService.cpp"
  )
  skip_if_not(file.exists(source_path), "Native source checkout not available")
  source <- paste(
    readLines(source_path, warn = FALSE),
    collapse = "\n"
  )

  expect_match(source, '<table width=\\"100%\\"', fixed = TRUE)
  expect_match(source, "table-layout:fixed", fixed = TRUE)
  expect_match(source, "max-width:6.25in", fixed = TRUE)
  expect_match(source, "font-size:9pt", fixed = TRUE)
  expect_match(source, "padding:2.5pt 4pt", fixed = TRUE)
  expect_match(source, "word-wrap:break-word", fixed = TRUE)
  expect_false(grepl("min-width:520px", source, fixed = TRUE))

  # The two editable copies are intentionally different: the Office command
  # produces APA output, while formatted text preserves the LinkEDA style.
  expect_match(
    source,
    'if (command == "COPY_RICH") ExportApaAsync(payload, true)',
    fixed = TRUE
  )
  expect_match(
    source,
    'command == "COPY_FORMATTED_TEXT") CopyFormatted(payload)',
    fixed = TRUE
  )
  expect_match(source, "Copy APA 7 table...", fixed = TRUE)
  expect_match(source, "Copy formatted table", fixed = TRUE)
  expect_match(source, "ApaPayload", fixed = TRUE)
  expect_match(source, "IsStandardizedBetaColumn", fixed = TRUE)
  expect_match(source, "not export a misleading column", fixed = TRUE)
  expect_match(source, 'value == "type"', fixed = TRUE)
  expect_match(source, "Times New Roman", fixed = TRUE)
  expect_match(source, 'dialog.Title(box_value(L"APA 7 table details"))', fixed = TRUE)
  expect_match(source, 'numberLabel.Text(L"Table number")', fixed = TRUE)
  expect_match(source, 'titleLabel.Text(L"Table title")', fixed = TRUE)
  expect_match(source, 'layoutLabel.Text(L"Dimensionality tables")', fixed = TRUE)
  expect_match(source, "One combined table in one PDF", fixed = TRUE)
  expect_match(source, "Two tables in two PDF files", fixed = TRUE)
  expect_match(source, "source.separatePublicationTables", fixed = TRUE)
  expect_match(source, "FollowingTableNumber", fixed = TRUE)
  expect_match(source, "ColumnBodyIsRightAligned", fixed = TRUE)
  expect_match(source, 'centered ? "middle"', fixed = TRUE)
  expect_match(source, 'style += "text-align:center;"', fixed = TRUE)

  workflow_path <- testthat::test_path(
    "..", "..", "src", "platform", "windows", "winui",
    "LinkEDA", "WorkflowWindows.cpp"
  )
  workflow <- paste(readLines(workflow_path, warn = FALSE), collapse = "\n")
  expect_match(
    workflow,
    "kind == ScaleAnalysisChildKind::InterItemCorrelations ||",
    fixed = TRUE
  )
  expect_match(
    workflow,
    "static_cast<int>(ScaleAnalysisChildKind::InterItemCorrelations)",
    fixed = TRUE
  )
})

test_that("macOS offers the same separate dimensionality PDF layout", {
  source_path <- testthat::test_path(
    "..", "..", "src", "platform", "macos", "linkeda_macos_app.mm"
  )
  skip_if_not(file.exists(source_path), "Native source checkout not available")
  source <- paste(readLines(source_path, warn = FALSE), collapse = "\n")

  expect_match(source, "setSeparateReportTables", fixed = TRUE)
  expect_match(source, "One combined table in one PDF", fixed = TRUE)
  expect_match(source, "Two tables in two PDF files", fixed = TRUE)
  expect_match(source, "allowSeparateFiles:YES", fixed = TRUE)
  expect_match(
    source,
    "ScaleAnalysisDimensionalityPresentation::SeparateTables",
    fixed = TRUE
  )
  expect_match(source, "PublicationTablesPDFData({firstTable}", fixed = TRUE)
  expect_match(source, "PublicationTablesPDFData({secondTable}", fixed = TRUE)
})
