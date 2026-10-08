library(LinkEDA)

test_that("a native point-colour column reaches every imputation before refitting", {
  group <- "native_point_color_mi_sync"
  if (group %in% ls_datasets()$name) ls_unregister_dataset(group)
  on.exit(if (group %in% ls_datasets()$name) ls_unregister_dataset(group), add = TRUE)
  ls_register_dataset(group, data.frame(y = c(1, 2, 3)))
  exchange <- tempfile("rlispstat-sync-")
  dir.create(exchange)
  on.exit(unlink(exchange, recursive = TRUE, force = TRUE), add = TRUE)
  payload <- file.path(exchange, "dataset.txt")
  writeLines(c(
    "DATASET", group, "3", "2", "DATACELLS_PERCENT_V1",
    "y", "numeric", "1", "2", "3",
    "point_color_from_plot", "factor", "green", "yellow", "green",
    "IMPUTATION_SPARSE", "multiple_imputation", "point_color_mi", "source",
    "2", "1", "version", "0", "DATA_VERSION_V1", "2"
  ), payload, useBytes = TRUE)
  testthat::local_mocked_bindings(.rls_send = function(...) "OK", .package = "LinkEDA")

  expect_true(LinkEDA:::.rls_handle_r_dataset_sync_needed(c(
    "R_DATASET_SYNC_NEEDED", "point-color-sync", group, payload
  )))
  record <- LinkEDA:::.rls_dataset_record(group)
  expect_identical(record$data_version, 2L)
  expect_identical(record$imputation_count, 2L)
  for (data in record$completed_datasets) {
    expect_identical(as.character(data$point_color_from_plot),
                     c("green", "yellow", "green"))
  }
})
