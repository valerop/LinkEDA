test_that("statistical-table PDF export uses a compiled tinytable document", {
  skip_if_not_installed("tinytable")

  table <- structure(list(
    title = "Model summary",
    response = "outcome",
    responses_differ = FALSE,
    family = "binomial",
    link = "logit",
    coefficients = data.frame(
      `Variable / category` = c("(Intercept)", "groupTreatment"),
      Model = c("-0.240", "0.615"),
      check.names = FALSE
    ),
    fit = data.frame(
      Statistic = c("N", "Residual deviance"),
      Model = c("120", "141.830"),
      check.names = FALSE
    ),
    note = "Odds ratios are exp(b).\n* p < .05"
  ), class = "rlispstat_regression_table")

  path <- tempfile(fileext = ".pdf")
  expect_identical(LinkEDA:::.rls_export_table_pdf(table, path), path)
  expect_true(file.exists(path))
  expect_gt(file.info(path)$size, 1000)
  expect_identical(rawToChar(readBin(path, "raw", n = 4L)), "%PDF")
})

test_that("statistical-table PDF export rejects incompatible sections", {
  skip_if_not_installed("tinytable")

  table <- list(
    title = "Broken table", response = "y", responses_differ = FALSE,
    coefficients = data.frame(Term = "x", Estimate = "1.0"),
    fit = data.frame(Statistic = "N", Value = "10", Extra = "x"),
    note = ""
  )
  expect_error(
    LinkEDA:::.rls_export_table_pdf(table, tempfile(fileext = ".pdf")),
    "incompatible columns",
    fixed = TRUE
  )
})
