test_that("data export supports delimited, Excel, statistical-package, and R formats", {
  data <- data.frame(
    score = c(1.5, NA, 3.25),
    group = factor(c("Control", "Treatment", "Control")),
    ordered = ordered(c("low", "high", "medium"), levels = c("low", "medium", "high")),
    check.names = FALSE
  )

  csv <- tempfile(fileext = ".csv")
  tsv <- tempfile(fileext = ".tsv")
  rds <- tempfile(fileext = ".rds")
  rdata <- tempfile(fileext = ".RData")
  expect_equal(LinkEDA:::.rls_export_data_frame(data, csv), normalizePath(csv))
  expect_equal(LinkEDA:::.rls_export_data_frame(data, tsv), normalizePath(tsv))
  expect_equal(readRDS(LinkEDA:::.rls_export_data_frame(data, rds)), data)
  LinkEDA:::.rls_export_data_frame(data, rdata)
  workspace <- new.env(parent = emptyenv())
  expect_equal(load(rdata, envir = workspace), "LinkEDA_data")
  expect_equal(workspace$LinkEDA_data, data)
  expect_equal(names(utils::read.csv(csv, check.names = FALSE)), names(data))
  expect_equal(names(utils::read.delim(tsv, check.names = FALSE)), names(data))

  skip_if_not_installed("writexl")
  xlsx <- tempfile(fileext = ".xlsx")
  LinkEDA:::.rls_export_data_frame(data, xlsx)
  expect_gt(file.info(xlsx)$size, 0)

  skip_if_not_installed("haven")
  paths <- c(
    sav = tempfile(fileext = ".sav"),
    zsav = tempfile(fileext = ".zsav"),
    dta = tempfile(fileext = ".dta"),
    xpt = tempfile(fileext = ".xpt")
  )
  for (format in names(paths)) {
    LinkEDA:::.rls_export_data_frame(data, paths[[format]], format)
    expect_gt(file.info(paths[[format]])$size, 0)
  }
  expect_equal(levels(haven::as_factor(haven::read_sav(paths[["sav"]])$group)),
               levels(data$group))
})

test_that("public data export uses the registered dataset and rejects read-only SAS format", {
  name <- ls_register_dataset("export formats", data.frame(x = 1:3, g = c("a", "b", "a")))
  on.exit(ls_unregister_dataset(name), add = TRUE)
  path <- tempfile(fileext = ".csv")
  expect_equal(ls_export_data(name, path), normalizePath(path))
  expect_equal(utils::read.csv(path, check.names = FALSE)$x, 1:3)
  expect_error(
    ls_export_data(name, tempfile(fileext = ".sas7bdat")),
    "Unsupported export format"
  )
})

test_that("multiple-imputation tabular export uses mice long form with original data", {
  skip_if_not_installed("mice")
  source <- data.frame(
    x = c(1, NA, 3, 4, 5, NA, 7, 8, 9, 10),
    y = c(2, 4, 1, 8, 6, 3, 7, 9, 5, 10),
    z = c(3, 8, 4, 9, 2, 7, 1, 6, 10, 5)
  )
  mids <- suppressWarnings(mice::mice(source, m = 2L, maxit = 1L,
                                     printFlag = FALSE, seed = 91L))
  name <- LinkEDA:::.rls_register_mids_dataset(mids, name = "export mi", make_active = FALSE)
  on.exit(ls_unregister_dataset(name), add = TRUE)
  path <- tempfile(fileext = ".csv")
  ls_export_data(name, path)
  exported <- utils::read.csv(path, check.names = FALSE)
  expect_equal(sort(unique(exported$.imp)), 0:2)
  expect_equal(nrow(exported), nrow(source) * 3L)
  expect_equal(exported$x[exported$.imp == 0L], source$x)
})

test_that("multiple-imputation RDS export preserves an importable current mids object", {
  skip_if_not_installed("mice")
  source <- data.frame(
    x = c(1, NA, 3, 4, 5, NA, 7, 8, 9, 10),
    y = c(2, 4, 1, 8, 6, 3, 7, 9, 5, 10),
    z = c(3, 8, 4, 9, 2, 7, 1, 6, 10, 5)
  )
  mids <- suppressWarnings(mice::mice(source, m = 2L, maxit = 1L,
                                     printFlag = FALSE, seed = 92L))
  name <- LinkEDA:::.rls_register_mids_dataset(
    mids, name = "export current mi", make_active = FALSE
  )
  on.exit(ls_unregister_dataset(name), add = TRUE)

  record <- LinkEDA:::.rls_dataset_record(name)
  record$original_data$y[[1L]] <- 99
  record$data$y[[1L]] <- 99
  record$completed_datasets <- lapply(record$completed_datasets, function(data) {
    data$y[[1L]] <- 99
    data
  })
  LinkEDA:::.rls_set_dataset_record(record)

  path <- tempfile(fileext = ".rds")
  ls_export_data(name, path)
  exported <- readRDS(path)
  expect_s3_class(exported, "mids")
  expect_equal(exported$m, 2L)
  expect_equal(mice::complete(exported, 1L)$y[[1L]], 99)

  imported_name <- ls_import_rds(path, make_active = FALSE)
  on.exit(ls_unregister_dataset(imported_name), add = TRUE)
  imported <- LinkEDA:::.rls_dataset_record(imported_name)
  expect_identical(imported$dataset_type, "multiple_imputation")
  expect_equal(imported$imputation_count, 2L)
  expect_equal(imported$completed_datasets[[2L]]$y[[1L]], 99)
})

test_that("older long-form RDS exports open as ordinary data with an explanation", {
  skip_if_not_installed("mice")
  source <- data.frame(
    x = c(1, NA, 3, 4, 5, NA, 7, 8, 9, 10),
    y = c(8, 2, 9, 1, 6, 4, 10, 3, 7, 5),
    z = c(2, 7, 4, 9, 1, 8, 5, 10, 3, 6)
  )
  mids <- suppressWarnings(mice::mice(source, m = 2L, maxit = 1L,
                                     printFlag = FALSE, seed = 93L))
  name <- LinkEDA:::.rls_register_mids_dataset(
    mids, name = "legacy long mi", make_active = FALSE
  )
  on.exit(ls_unregister_dataset(name), add = TRUE)
  legacy <- LinkEDA:::.rls_export_dataset_data(LinkEDA:::.rls_dataset_record(name))
  path <- tempfile(fileext = ".rds")
  saveRDS(legacy, path)

  imported_name <- ls_import_rds(path, make_active = FALSE)
  on.exit(ls_unregister_dataset(imported_name), add = TRUE)
  imported <- LinkEDA:::.rls_dataset_record(imported_name)
  expect_false(identical(imported$dataset_type, "multiple_imputation"))
  expect_match(imported$import_notice, "Stacked multiple imputations detected")
})
