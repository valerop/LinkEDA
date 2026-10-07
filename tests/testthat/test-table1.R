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
  age_row <- Filter(
    function(row) identical(row$variable, "age") &&
      identical(row$row_type, "numeric_mean_sd"),
    record$summary_statistics
  )[[1L]]
  expect_setequal(
    names(age_row$statistics$Overall),
    c("mean", "sd", "se", "ci95", "median", "q1", "q3")
  )
  expect_match(age_row$statistics$Overall$ci95, "^\\[")
})

test_that("Table 1 without an explicit specification opens as an empty table", {
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
  expect_identical(record$variables, character())
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

test_that("Table 1 consumes canonical semantic types instead of guessing from cardinality", {
  ls_register_dataset("table1_mtcars", mtcars)

  table <- ls_new_table1("table1_mtcars", variables = c("mpg", "cyl", "vs", "am", "gear", "carb"))
  display <- ls_table1_table(table)
  record <- LinkEDA:::`.rls_table1_record`(table)

  expect_equal(record$variable_types[["mpg"]], "numeric")
  expect_equal(record$variable_types[c("mpg", "cyl", "vs", "am", "gear", "carb")],
               setNames(rep("numeric", 6), c("mpg", "cyl", "vs", "am", "gear", "carb")))

  for (variable in c("mpg", "cyl", "vs", "am", "gear", "carb")) {
    parent <- display[display$Variable == variable, , drop = FALSE]
    expect_match(parent$Overall[[1L]], "\\(")
  }

  table_typed <- ls_new_table1(
    "table1_mtcars",
    variables = c("cyl", "am"),
    variable_types = c(cyl = "ordinal", am = "categorical")
  )
  typed_record <- LinkEDA:::`.rls_table1_record`(table_typed)
  expect_equal(typed_record$variable_types[c("cyl", "am")],
               c(cyl = "ordinal", am = "categorical"))
  expect_true(any(typed_record$display_table$Variable == "  4"))
  expect_true(any(typed_record$display_table$Variable == "  0"))
})

test_that("Table 1 native task respects canonical numeric metadata", {
  ls_register_dataset("table1_canonical_mtcars", mtcars)
  LinkEDA:::.rls_handle_table1_needed(c(
    "TABLE1_NEEDED", "table1_canonical_numeric", "table1_canonical_mtcars", "",
    "TRUE", "TRUE", "TRUE", "TRUE", "ordinal", "all", "All observations",
    "1", "cyl", "TYPES", "1", "cyl", "numeric", "0"
  ))

  result <- LinkEDA:::.rls_table1_record("table1_canonical_numeric")
  display <- result$display_table
  expect_equal(result$variable_types[["cyl"]], "numeric")
  expect_match(display$Overall[display$Variable == "cyl"][[1L]], "\\(")
})

test_that("Table 1 native payload is structured, not a preformatted text block", {
  typed_mtcars <- transform(mtcars, am = factor(am), vs = factor(vs))
  ls_register_dataset("table1_payload", typed_mtcars)
  table <- ls_new_table1("table1_payload", variables = c("mpg", "am"), group = "vs")
  record <- LinkEDA:::`.rls_table1_record`(table)
  payload <- LinkEDA:::`.rls_table1_native_payload`(record)

  expect_equal(payload[[1L]], "TABLE1_OPEN_STRUCTURED")
  expect_true("numeric_mean_sd" %in% payload)
  expect_true("categorical_parent" %in% payload)
  expect_true("Welch t" %in% payload)
  expect_true("TABLE1_DISPLAY_V1" %in% payload)
  expect_true("TABLE1_RAW_V1" %in% payload)
  expect_true("ANALYSIS_PROVENANCE_V2" %in% payload)
  expect_true(all(c("mean", "sd", "se", "ci95", "median", "q1", "q3") %in% payload))
  provenance_marker <- match("ANALYSIS_PROVENANCE_V2", payload)
  structured_payload <- payload[seq_len(provenance_marker - 1L)]
  expect_false(any(grepl("^Table 1\\.", structured_payload[-4L])))
})

test_that("Table 1 emits every categorical level in canonical factor order", {
  data <- data.frame(
    happy = factor(rep(c("no", "yes"), each = 5), levels = c("no", "yes", "unused")),
    binary = factor(rep(c("second", "first"), 5), levels = c("first", "second")),
    planet = factor(
      c("Aurelia", "Cygnus", "Borealis", "Aurelia", "Cygnus",
        "Borealis", "Aurelia", "Cygnus", "Borealis", "Aurelia"),
      levels = c("Cygnus", "Aurelia", "Borealis")
    ),
    colour = factor(
      c("red", "blue", "green", "amber", "red",
        "blue", "green", "amber", "red", "blue"),
      levels = c("green", "red", "blue", "amber", "violet")
    )
  )
  ls_register_dataset("table1_all_factor_levels", data)

  level_rows <- function(record, variable) {
    rows <- Filter(
      function(row) identical(row$variable, variable) &&
        identical(row$row_type, "categorical_level"),
      record$summary_statistics
    )
    vapply(rows, `[[`, character(1L), "level")
  }

  grouped <- ls_new_table1(
    "table1_all_factor_levels",
    variables = c("binary", "planet", "colour"),
    group = "happy"
  )
  grouped_record <- LinkEDA:::`.rls_table1_record`(grouped)
  expect_equal(names(grouped_record$summary_statistics[[1L]]$values),
               c("Overall", "no", "yes"))
  expect_equal(level_rows(grouped_record, "binary"), c("first", "second"))
  expect_equal(level_rows(grouped_record, "planet"), c("Cygnus", "Aurelia", "Borealis"))
  expect_equal(level_rows(grouped_record, "colour"),
               c("green", "red", "blue", "amber", "violet"))
  expect_true(all(vapply(
    Filter(function(row) identical(row$row_type, "categorical_level"),
           grouped_record$summary_statistics),
    function(row) length(row$values) == 3L,
    logical(1L)
  )))

  ungrouped <- ls_new_table1(
    "table1_all_factor_levels",
    variables = c("binary", "planet", "colour")
  )
  ungrouped_record <- LinkEDA:::`.rls_table1_record`(ungrouped)
  expect_equal(level_rows(ungrouped_record, "binary"), level_rows(grouped_record, "binary"))
  expect_equal(level_rows(ungrouped_record, "planet"), level_rows(grouped_record, "planet"))
  expect_equal(level_rows(ungrouped_record, "colour"), level_rows(grouped_record, "colour"))

  payload <- LinkEDA:::`.rls_table1_native_payload`(grouped_record)
  expect_equal(sum(payload == "categorical_level"), 10L)
  expect_true(all(c("Aurelia", "Borealis", "Cygnus", "violet") %in% payload))
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

test_that("Table 1 selected scope preserves subject identity across imputations", {
  completed <- lapply(seq_len(3L), function(i) {
    data.frame(
      y = c(1, 2, 100 + i, 10, 20, -100 - i),
      g = factor(rep(c("Control", "Treatment"), each = 3L),
                 levels = c("Control", "Treatment"))
    )
  })
  original <- completed[[1L]]
  original$y[c(3L, 6L)] <- NA_real_
  ls_register_dataset("table1_mi_scope", completed[[1L]])
  dataset <- LinkEDA:::.rls_dataset_record("table1_mi_scope")
  dataset$dataset_type <- "multiple_imputation"
  dataset$completed_datasets <- completed
  dataset$imputation_count <- length(completed)
  dataset$original_data <- original
  dataset$missing_cell_mask <- as.data.frame(lapply(
    original, is.na
  ), stringsAsFactors = FALSE)
  dataset$original_row_ids <- c(10L, 20L, 30L, 40L, 50L, 60L)
  LinkEDA:::.rls_set_dataset_record(dataset)

  table <- ls_new_table1(
    "table1_mi_scope", variables = "y", group = "g",
    .selected_rows = c(10L, 20L, 40L, 50L),
    .scope_description = "Selected rows"
  )
  record <- LinkEDA:::.rls_table1_record(table)
  pooled <- record$multiple_imputation$estimates_by_imputation$y
  per_imputation <- record$multiple_imputation$complete_data_result_by_imputation

  expect_identical(record$data_scope$kind, "explicit")
  expect_identical(record$data_scope$rows, c(10L, 20L, 40L, 50L))
  expect_equal(record$data_scope$n, 4L)
  expect_equal(record$data_scope$total_n, 6L)
  expect_equal(pooled$Overall$sample_sizes, rep(4, 3))
  expect_equal(pooled$Control$sample_sizes, rep(2, 3))
  expect_equal(pooled$Treatment$sample_sizes, rep(2, 3))
  expect_equal(pooled$Overall$means, rep(8.25, 3), tolerance = 1e-12)
  expect_equal(vapply(per_imputation, `[[`, integer(1L), "n_rows"), rep(4L, 3))
  expect_true(any(grepl("Selected rows.*N = 4 of 6",record$footnotes)))
  y_row <- Filter(
    function(row) identical(row$variable, "y") &&
      identical(row$row_type, "numeric_mean_sd"),
    record$summary_statistics
  )[[1L]]
  expect_setequal(
    names(y_row$statistics$Overall),
    c("mean", "sd", "se", "ci95", "median", "q1", "q3")
  )
  expect_equal(
    y_row$statistics$Overall$mean,
    LinkEDA:::`.rls_export_format_number`(pooled$Overall$pool$Qbar, 1L)
  )
  expect_true("TABLE1_DISPLAY_V1" %in%
                LinkEDA:::`.rls_table1_native_payload`(record))

  diagnostics <- ls_missing_information_diagnostics(table)
  hierarchy <- record$analysis_provenance$missing_information_display_rows
  expect_identical(hierarchy$label, c("y", "Overall", "Control", "Treatment",
                                    "Difference (Treatment − Control)"))
  expect_identical(hierarchy$row_type, c("term_parent", rep("summary_mean",3), "group_comparison"))
  expect_equal(hierarchy$value_row, -1:3)
  expect_true(all(is.na(diagnostics$p[1:3])))
  expect_equal(diagnostics$p[4], record$test_results$y$p)
  expect_equal(diagnostics$Estimate[4], record$test_results$y$pooled_result$Qbar)
  expect_equal(diagnostics$FMI[4], record$test_results$y$pooled_result$FMI)
  expect_equal(diagnostics$`MCSE (estimate)`[4], sqrt(diagnostics$B[4]/3))
  # Verify the contrast against public pooling from the same scoped rows.
  fits <- lapply(completed, function(data) stats::lm(y~g, data[c(1,2,4,5),]))
  reference <- summary(mice::pool(mice::as.mira(fits)))
  expect_equal(diagnostics$p[4], reference$p.value[2], tolerance=1e-10)
  expect_equal(diagnostics$Estimate[4], reference$estimate[2], tolerance=1e-10)
  expect_true("MI_DIAGNOSTIC_ROWS_V1" %in% LinkEDA:::.rls_table1_native_payload(record))
  recipe <- record$analysis_provenance$verification_r_code$table
  expect_match(recipe, "reference_group_diagnostics", fixed=TRUE)
  expect_false(grepl("LinkEDA:::", recipe, fixed=TRUE))

  expect_error(
    ls_new_table1(
      "table1_mi_scope", variables = "y", group = "g",
      .selected_rows = integer(), .scope_description = "Selected rows"
    ),
    "No rows are selected"
  )
})

test_that("Table 1 diagnostics distinguish omnibus tests and ungrouped means", {
  skip_if_not_installed("mice")
  completed <- lapply(seq_len(3), function(i) data.frame(
    y=c(1,3,2,4,6,7,5,8,6)+i*c(.2,0,.1,0,.2,0,.1,0,.2),
    g=factor(rep(c("A","B","C"),each=3))))
  id <- "table1_mi_diagnostic_groups"
  ls_register_dataset(id, completed[[1L]])
  on.exit(ls_unregister_dataset(id),add=TRUE)
  dataset <- LinkEDA:::.rls_dataset_record(id)
  dataset$dataset_type <- "multiple_imputation"
  dataset$completed_datasets <- completed
  dataset$imputation_count <- 3L
  dataset$original_data <- completed[[1L]]
  dataset$original_row_ids <- seq_len(9)
  LinkEDA:::.rls_set_dataset_record(dataset)
  grouped <- ls_new_table1(id,variables="y",group="g")
  record <- LinkEDA:::.rls_table1_record(grouped)
  diagnostics <- ls_missing_information_diagnostics(grouped)
  expect_equal(nrow(diagnostics),5)
  expect_true(all(is.na(diagnostics$p[1:4])))
  expect_equal(diagnostics$p[5],record$test_results$y$p)
  expect_true(is.na(diagnostics$FMI[5]))
  expect_true(is.na(diagnostics$Estimate[5]))
  expect_equal(tail(record$analysis_provenance$missing_information_display_rows$label,1),
               "Group comparison (omnibus)")
  ungrouped <- ls_new_table1(id,variables="y")
  diagnostics <- ls_missing_information_diagnostics(ungrouped)
  expect_equal(nrow(diagnostics),1)
  expect_true(is.na(diagnostics$p[1]))
})
