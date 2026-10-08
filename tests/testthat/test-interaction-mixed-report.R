test_that("ordinary mixed three-way interactions can compare and group categorical terms", {
  skip_if_not_installed("emmeans")
  data <- utils::read.csv(
    system.file("examples", "Alien.csv", package = "LinkEDA"),
    stringsAsFactors = FALSE
  )
  data$planet <- factor(data$planet)
  data$happy <- factor(data$happy)
  fit <- stats::lm(size ~ happy * planet * eggs, data = data)
  record <- list(
    terms = c("happy", "planet", "eggs", "happy:planet",
              "happy:eggs", "planet:eggs", "happy:planet:eggs"),
    fit = fit
  )

  slopes <- LinkEDA:::.rls_regression_interaction_record(
    record, "happy:planet:eggs", focal = "eggs", group_by = "planet"
  )
  expect_identical(slopes$conditioning, c("planet", "happy"))
  expect_identical(slopes$interaction_type, "three_way_with_numeric")
  means <- LinkEDA:::.rls_regression_interaction_record(
    record, "happy:planet:eggs", focal = "happy", group_by = "planet"
  )
  expect_identical(means$conditioning, c("planet", "eggs"))
  expect_true(nrow(means$estimates) > 0L)

  report <- LinkEDA:::.rls_interaction_native_report(slopes)
  expect_false(any(grepl(
    "Imputations:|Pooling:|every fitted imputation", report
  )))

  mixed_fit <- stats::lm(size ~ blue_eyes * planet * happy, data = data)
  mixed_record <- list(
    terms = c("blue_eyes", "planet", "happy", "blue_eyes:planet",
              "blue_eyes:happy", "planet:happy", "blue_eyes:planet:happy"),
    fit = mixed_fit
  )
  categorical_focal <- LinkEDA:::.rls_regression_interaction_record(
    mixed_record, "blue_eyes:planet:happy", focal = "planet", group_by = "happy"
  )
  means_report <- LinkEDA:::.rls_interaction_native_report(categorical_focal)
  expect_true("Estimated marginal means" %in% means_report)
  expect_false("Simple slopes" %in% means_report)
  expect_true("EMM reference test: None" %in% means_report)
  expect_true(any(grepl("^Numeric conditioning values for blue_eyes .*1\\.78, 5\\.28, 8\\.79\\.$",
                        means_report)))
  expect_true(any(grepl("^planet\\thappy\\tblue_eyes\\tEstimate\\tSE\\tLower 95%\\tUpper 95%$",
                        means_report)))
  expect_true("Pairwise comparisons" %in% means_report)
  expect_false(any(grepl("1.77986451153533", means_report, fixed = TRUE)))
})
