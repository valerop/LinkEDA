.mi_contingency_fixture <- function(name = "mi_cross_test", completed = NULL) {
  if (is.null(completed)) {
    completed <- list(
      data.frame(row = factor(c("A", "A", "B", "B"), levels = c("A", "B", "unused")),
                 column = factor(c("X", "Y", "X", "Y"))),
      data.frame(row = factor(c("A", "B", "B", "B"), levels = c("A", "B", "unused")),
                 column = factor(c("X", "X", "Y", "Y")))
    )
  }
  id <- ls_register_dataset(name, completed[[1L]])
  record <- LinkEDA:::.rls_dataset_record(id)
  record$dataset_type <- "multiple_imputation"
  record$completed_datasets <- completed
  record$original_data <- completed[[1L]]
  record$original_data$row[2] <- NA
  record$imputation_count <- length(completed)
  record$original_row_ids <- seq_len(nrow(record$data))
  record$missing_cell_mask <- as.data.frame(lapply(record$original_data, is.na))
  record$data_version <- 7L
  LinkEDA:::.rls_set_dataset_record(record)
  id
}

.mi_contingency_verify <- function(result, group, rows = NULL) {
  data <- LinkEDA:::.rls_dataset_record(group)$completed_datasets
  if (!is.null(rows)) data <- lapply(data, function(d) d[rows, , drop = FALSE])
  long <- do.call(rbind, lapply(seq_along(data), function(i) {
    d <- data[[i]]; d$.imp <- i; d$.id <- seq_len(nrow(d)); d
  }))
  path <- tempfile(fileext = ".rds"); on.exit(unlink(path))
  saveRDS(long, path)
  env <- new.env(parent = baseenv()); env$verification_data_path <- path
  code <- result$analysis_provenance$verification_r_code$table
  expect_false(grepl("LinkEDA|\\.rls_", code))
  invisible(eval(parse(text = code), env))
  expect_equal(env$reference_mean_counts, result$mean_counts)
  expect_equal(env$reference_mean_row_percentages, result$mean_row_percentages)
}

test_that("MI cross-tabs average completed counts and within-imputation row percentages", {
  id <- .mi_contingency_fixture()
  result <- LinkEDA:::.rls_mi_contingency_record(id, "row", "column", "cross_1")
  expect_equal(unname(result$mean_counts["A", ]), c(1, .5, 1.5))
  expect_equal(unname(result$mean_row_percentages["A", ]), c(.75, .25, 1))
  expect_equal(unname(result$mean_counts["Total", ]), c(2, 2, 4))
  expect_true(all(is.na(result$mean_row_percentages["unused", ])))
  expect_identical(result$table_type, "mi_contingency")
  expect_identical(result$analysis_provenance$dataset_version, 7L)
  expect_identical(result$data_scope$kind, "all")
  expect_true(grepl("1.0 (75.0%)", result$summary_statistics[[1]]$values[[1]], fixed = TRUE))
  expect_true(any(grepl("not a Rubin-pooled", result$footnotes, fixed = TRUE)))
  .mi_contingency_verify(result, id)
  payload <- LinkEDA:::.rls_table1_native_payload(result)
  expect_true("TABLE_KIND_V1" %in% payload)
  expect_true("ANALYSIS_PROVENANCE_V2" %in% payload)
  expect_true("mi_contingency" %in% payload)
  expect_false(result$display_options$show_p)
})

test_that("MI cross-tabs preserve selected original rows and reject an empty scope", {
  id <- .mi_contingency_fixture("mi_cross_subset")
  result <- LinkEDA:::.rls_mi_contingency_record(id, "row", "column", "cross_subset",
                                                selected_rows = c(2L, 4L))
  expect_identical(result$data_scope$kind, "explicit")
  expect_identical(result$data_scope$rows, c(2L, 4L))
  expect_equal(result$data_scope$total_n, 4L)
  expect_equal(result$mean_counts["Total", "Total"], 2)
  .mi_contingency_verify(result, id, c(2L, 4L))
  expect_error(LinkEDA:::.rls_mi_contingency_record(id, "row", "column",
                selected_rows = integer()), "No rows")
  expect_error(LinkEDA:::.rls_mi_contingency_record(id, "row", "row"), "distinct")
  expect_error(LinkEDA:::.rls_mi_contingency_record(id, "missing", "column"), "distinct")
})

test_that("MI cross-tabs support editing cell contents, row variables and column split", {
  id <- .mi_contingency_fixture("mi_cross_editor")
  for(mode in c("count_percent","count","percent")) {
    result <- LinkEDA:::.rls_mi_contingency_record(id,"row","column",id="editable_cross",display_mode=mode)
    expect_identical(result$id,"editable_cross")
    expect_identical(result$contingency_display_mode,mode)
    expect_identical(result$summary_statistics[[1]]$stub_values,"A")
    expect_true("MI_CONTINGENCY_LAYOUT_V1" %in% LinkEDA:::.rls_table1_native_payload(result))
    expect_equal(result$mean_counts["Total","Total"],4)
    if(mode=="count") expect_identical(unname(result$summary_statistics[[1]]$values[1]),"1.0")
    if(mode=="percent") expect_identical(unname(result$summary_statistics[[1]]$values[1]),"75.0%")
    .mi_contingency_verify(result,id)
  }
  unsplit <- LinkEDA:::.rls_mi_contingency_record(id,"row","",id="editable_cross")
  expect_identical(unsplit$id,"editable_cross")
  expect_identical(unsplit$group_variable,"")
  expect_equal(ncol(unsplit$mean_counts),1)
  expect_equal(unname(unsplit$mean_counts["A",]),1.5)
  .mi_contingency_verify(unsplit,id)
  nested <- LinkEDA:::.rls_mi_contingency_record(id,c("row","column"),"",id="editable_cross")
  expect_identical(nested$summary_statistics[[1]]$stub_values,c("A","X"))
  expect_equal(length(nested$row_variables),2)
  .mi_contingency_verify(nested,id)
})

test_that("nested MI cross-tabs retain absent levels and handle missing observations", {
  completed <- list(
    data.frame(row = c("A", "A", "B", "B"), second = c("one", "two", "one", "two"),
               column = factor(c("X", "Y", NA, "Y"), levels = c("X", "Y", "Z"))),
    data.frame(row = c("A", "B", "B", "B"), second = c("one", "two", "one", "two"),
               column = factor(c("X", "Z", "Y", "Y"), levels = c("X", "Y", "Z"))))
  id <- .mi_contingency_fixture("mi_cross_nested", completed)
  result <- LinkEDA:::.rls_mi_contingency_record(id, c("row", "second"), "column")
  expect_equal(dim(result$mean_counts), c(5L, 4L))
  expect_equal(result$mean_counts["Total", "Total"], 3.5)
  expect_equal(result$mean_counts["A / two", "Total"], .5)
  expect_equal(result$mean_row_percentages["A / two", "Y"], 1)
  .mi_contingency_verify(result, id)
})

test_that("MI cross-tabs retain protocol-reserved category names and labelled data", {
  completed <- list(
    data.frame(row = c("a", "b", "a"), column = c("Variable", "p", "Test")),
    data.frame(row = c("a", "b", "b"), column = c("p", "Test", "Total")))
  for (i in seq_along(completed)) {
    completed[[i]]$row <- structure(ifelse(completed[[i]]$row == "a", 1, 2),
                                   labels = c(First = 1, Second = 2))
  }
  id <- .mi_contingency_fixture("mi_cross_names", completed)
  result <- LinkEDA:::.rls_mi_contingency_record(id, "row", "column")
  expect_equal(ncol(result$display_table), 6L)
  expect_true(all(c("First", "Second") %in% rownames(result$mean_counts)))
  expect_false(any(names(result$display_table)[-1L] %in% c("Variable", "p", "Test")))
  .mi_contingency_verify(result, id)
})
