test_that("Table 1 builds grouped numeric, categorical, and ordinal summaries", {
  data <- data.frame(
    id = seq_len(8),
    group = factor(rep(c("control", "treatment"), each = 4)),
    age = c(51, 55, 54, NA, 61, 63, 66, 62),
    sex = factor(c("female", "male", "female", "female", "male", "male", "female", NA)),
    response = ordered(c("low", "medium", "high", "medium", "medium", "high", "high", "low"),
                       levels = c("low", "medium", "high"))
  )
  ls_register_dataset("table1_grouped", data)

  table <- ls_new_table1(
    "table1_grouped",
    variables = c("age", "sex", "response"),
    group = "group",
    variable_types = c(response = "ordinal")
  )
  display <- ls_table1_table(table)
  record <- LinkEDA:::`.rls_table1_record`(table)

  expect_s3_class(table, "rlispstat_table1")
  expect_true(all(c("Variable", "Overall", "control", "treatment", "p", "Test") %in% names(display)))
  expect_true(any(display$Variable == "age"))
  expect_true(any(display$Variable == "sex"))
  expect_true(any(display$Variable == "response"))
  expect_true(any(display$Variable == "  Missing"))
  expect_equal(record$variable_types[c("age", "sex", "response")],
               c(age = "numeric", sex = "categorical", response = "ordinal"))
  expect_match(display$Test[display$Variable == "age"][[1L]], "Welch")
  expect_match(display$Test[display$Variable == "response"][[1L]], "Wilcoxon|Kruskal")
  expect_true(length(record$rows_used_by_variable$age) < nrow(data))
  expect_equal(record$missingness$age$missing_n, 1L)
  expect_equal(record$missingness$sex$missing_n, 1L)
})

test_that("Table 1 auto-selection skips ID-like variables and omits test columns ungrouped", {
  data <- data.frame(
    id = 1:6,
    subject_label = sprintf("S%02d", 1:6),
    score = c(10, 11, 13, 15, 16, 18),
    arm = factor(c("a", "a", "b", "b", "b", "a")),
    stringsAsFactors = FALSE
  )
  ls_register_dataset("table1_auto", data)

  table <- ls_new_table1("table1_auto")
  display <- ls_table1_table(table)
  record <- LinkEDA:::`.rls_table1_record`(table)

  expect_false("p" %in% names(display))
  expect_false("Test" %in% names(display))
  expect_false("id" %in% record$variables)
  expect_false("subject_label" %in% record$variables)
  expect_true(all(c("score", "arm") %in% record$variables))
  expect_equal(display$Variable[[1L]], "N")
  expect_equal(display$Overall[[1L]], "6")
})

test_that("Table 1 preserves value labels and exports text, markdown, and csv", {
  labelled <- c(1, 2, 1, 3, NA, 2)
  attr(labelled, "labels") <- c(No = 1, Maybe = 2, Yes = 3)
  data <- data.frame(
    group = factor(c("a", "a", "b", "b", "b", "a")),
    attitude = labelled,
    score = c(1.1, 1.5, 2.2, 2.6, 2.7, NA)
  )
  ls_register_dataset("table1_labels", data)

  table <- ls_new_table1("table1_labels", variables = c("attitude", "score"), group = "group")
  display <- ls_table1_table(table)
  record <- LinkEDA:::`.rls_table1_record`(table)

  expect_equal(record$variable_types[["attitude"]], "categorical")
  expect_true(any(display$Variable == "  No"))
  expect_true(any(display$Variable == "  Maybe"))
  expect_true(any(display$Variable == "  Yes"))

  text <- ls_copy_table1(table, "text")
  markdown <- ls_copy_table1(table, "markdown")
  expect_match(text, "Table 1. Descriptive statistics by group", fixed = TRUE)
  expect_match(markdown, "| Variable |", fixed = TRUE)

  csv_path <- tempfile(fileext = ".csv")
  txt_path <- tempfile(fileext = ".txt")
  md_path <- tempfile(fileext = ".md")
  expect_equal(ls_export_table1(table, csv_path, "csv"), csv_path)
  expect_equal(ls_export_table1(table, txt_path, "txt"), txt_path)
  expect_equal(ls_export_table1(table, md_path, "md"), md_path)
  expect_true(file.exists(csv_path))
  expect_true(file.exists(txt_path))
  expect_true(file.exists(md_path))
  exported <- utils::read.csv(csv_path, check.names = FALSE)
  expect_equal(names(exported), names(display))
})

test_that("Table 1 treats mtcars low-cardinality coded integers as categorical or ordinal", {
  ls_register_dataset("table1_mtcars", mtcars)

  table <- ls_new_table1("table1_mtcars", variables = c("mpg", "cyl", "vs", "am", "gear", "carb"))
  display <- ls_table1_table(table)
  record <- LinkEDA:::`.rls_table1_record`(table)

  expect_equal(record$variable_types[["mpg"]], "numeric")
  expect_equal(record$variable_types[["vs"]], "categorical")
  expect_equal(record$variable_types[["am"]], "categorical")
  expect_equal(record$variable_types[["cyl"]], "ordinal")
  expect_equal(record$variable_types[["gear"]], "ordinal")
  expect_equal(record$variable_types[["carb"]], "ordinal")

  for (variable in c("cyl", "vs", "am", "gear", "carb")) {
    parent <- display[display$Variable == variable, , drop = FALSE]
    expect_equal(parent$Overall[[1L]], "")
    expect_true(any(startsWith(display$Variable, "  ")))
  }

  table_num <- ls_new_table1(
    "table1_mtcars",
    variables = "cyl",
    variable_types = c(cyl = "numeric")
  )
  display_num <- ls_table1_table(table_num)
  expect_match(display_num$Overall[display_num$Variable == "cyl"][[1L]], "\\(")
})

test_that("Table 1 native payload is structured, not a preformatted text block", {
  ls_register_dataset("table1_payload", mtcars)
  table <- ls_new_table1("table1_payload", variables = c("mpg", "am"), group = "vs")
  record <- LinkEDA:::`.rls_table1_record`(table)
  payload <- LinkEDA:::`.rls_table1_native_payload`(record)

  expect_equal(payload[[1L]], "TABLE1_OPEN_STRUCTURED")
  expect_true("numeric_mean_sd" %in% payload)
  expect_true("categorical_parent" %in% payload)
  expect_true("Welch t" %in% payload)
  expect_false(any(grepl("^Table 1\\.", payload[-4L])))
})

test_that("Table 1 grouped tests use real p-values and appropriate test families", {
  data <- data.frame(
    g2 = factor(rep(c("a", "b"), each = 8)),
    y = c(1.1, 1.2, 1.5, 1.4, 1.8, 1.6, 1.7, 1.9,
          3.2, 3.4, 3.3, 3.8, 3.7, 3.6, 3.9, 4.0),
    cat = factor(c(rep("no", 7), "yes", "no", rep("yes", 7))),
    ord = ordered(c(1, 1, 2, 2, 2, 3, 3, 3,
                    2, 3, 3, 4, 4, 4, 5, 5),
                  levels = 1:5)
  )
  data3 <- data.frame(
    g = factor(rep(c("a", "b", "c"), each = 6)),
    y = c(1, 1.2, 1.1, 1.3, 1.4, 1.2,
          2.5, 2.7, 2.9, 2.6, 2.8, 3.0,
          4.1, 4.4, 4.5, 4.2, 4.6, 4.7),
    ord = ordered(c(1, 1, 2, 2, 2, 3,
                    2, 3, 3, 4, 4, 4,
                    3, 4, 4, 5, 5, 5),
                  levels = 1:5)
  )
  ls_register_dataset("table1_tests2", data)
  ls_register_dataset("table1_tests3", data3)

  t2 <- ls_new_table1("table1_tests2", variables = c("y", "cat", "ord"), group = "g2")
  d2 <- ls_table1_table(t2)
  expect_equal(d2$Test[d2$Variable == "y"][[1L]], "Welch t")
  expect_true(is.finite(LinkEDA:::`.rls_table1_record`(t2)$test_results$y$p))
  expect_true(d2$Test[d2$Variable == "cat"][[1L]] %in% c("Fisher exact", "\u03c7\u00b2", "\u03c7\u00b2 simulated"))
  expect_equal(d2$Test[d2$Variable == "ord"][[1L]], "Wilcoxon rank-sum")

  t3 <- ls_new_table1("table1_tests3", variables = c("y", "ord"), group = "g")
  d3 <- ls_table1_table(t3)
  expect_equal(d3$Test[d3$Variable == "y"][[1L]], "Welch ANOVA")
  expect_equal(d3$Test[d3$Variable == "ord"][[1L]], "Kruskal-Wallis")
})
