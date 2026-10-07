test_that("transport never shortens distinct long categories or text", {
  prefix <- paste(rep("\u00e1|%\t", 50), collapse = "")
  values <- paste0(prefix, c("first", "second"))
  data <- data.frame(category = factor(values, levels = rev(values)), text = values)
  payload <- LinkEDA:::.rls_dataframe_payload(data, max_cell_chars = 120L)
  encoded <- LinkEDA:::.rls_encode_data_value(values)
  expect_true(all(encoded %in% payload))
  driver <- Sys.getenv("LINKEDA_DATA_ROUNDTRIP_TEST")
  if (!nzchar(driver)) return(invisible(NULL))
  input <- tempfile(); output <- tempfile()
  writeLines(payload, input, useBytes = TRUE)
  expect_identical(system2(driver, c(input, output)), 0L)
  restored <- LinkEDA:::.rls_read_native_data_payload(output)
  expect_identical(restored$columns$category, values)
  expect_identical(restored$columns$text, values)
  expect_identical(restored$factor_levels$category, rev(values))
})

test_that("existing correlations accept a newly created numeric column", {
  id <- LinkEDA::ls_register_dataset("correlation_new_column_regression", mtcars[c("mpg", "hp", "wt")])
  handle <- LinkEDA::ls_new_correlation_matrix(id, c("mpg", "hp"), native = FALSE)
  dataset <- LinkEDA:::.rls_dataset_record(id)
  dataset$data$new_column <- dataset$data$wt^2
  dataset$data_frame <- dataset$data
  dataset$metadata <- dataset$variable_metadata <- LinkEDA:::.rls_refresh_metadata_row(
    dataset$variable_metadata, dataset$data, "new_column", "numeric")
  dataset <- LinkEDA:::.rls_advance_data_version(dataset, "New numeric column")
  LinkEDA:::.rls_set_dataset_record(dataset)
  LinkEDA::ls_correlation_matrix_add_variable(handle, "new_column")
  state <- LinkEDA::ls_correlation_matrix_state(handle)
  expect_identical(state$variables, c("mpg", "hp", "new_column"))
  expect_identical(state$data$new_column, dataset$data$new_column)
  expect_equal(state$data_scope$dataset_version, dataset$data_version)
  for (i in seq_len(nrow(state$results))) {
    cell <- state$results[i, ]
    if (identical(cell$x_variable, cell$y_variable)) next
    expected <- stats::cor.test(state$data[[cell$x_variable]], state$data[[cell$y_variable]])
    expect_equal(cell$r, unname(expected$estimate))
    expect_equal(cell$p, expected$p.value)
  }
  expect_error(LinkEDA::ls_correlation_matrix_add_variable(handle, "absent"), "not found")
})

test_that("SVG time series keep missing groups and safe distinct labels", {
  groups <- c("A|B", "A B", "(missing)", NA_character_)
  data <- data.frame(time = as.Date("2026-01-01") + rep(0:1, each = 4),
                     value = seq_len(8), group = rep(groups, 2))
  record <- list(type = "time_series", x = "time", y = "value", series = "group",
                 data = data, title = "Series", identification = "legend")
  calls <- list(); labels <- NULL
  original_lines <- graphics::lines.default
  original_legend <- graphics::legend
  testthat::local_mocked_bindings(
    lines.default = function(x, y, ...) {
      calls[[length(calls) + 1L]] <<- list(x = x, y = y)
      original_lines(x, y, ...)
    },
    legend = function(x, legend, ...) {
      labels <<- legend
      original_legend(x, legend = legend, ...)
    }, .package = "graphics")
  path <- tempfile(fileext = ".svg")
  LinkEDA:::.rls_export_time_series_fallback(record, path, "svg", 7, 5, "classic", 144)
  expect_length(calls, 4L)
  expect_identical(vapply(calls, function(x) length(x$y), integer(1L)), rep(2L, 4))
  for (i in seq_along(calls)) expect_equal(calls[[i]]$y, c(i, i + 4L))
  expect_length(unique(labels), 4L)
  expect_true("(missing values)" %in% labels)
  expect_match(paste(readLines(path, warn = FALSE), collapse = "\n"), "<svg", fixed = TRUE)
})

test_that("fallback export preserves the user's graphics device and settings", {
  folder <- tempfile(); dir.create(folder); withr::local_dir(folder)
  grDevices::pdf(file = NULL)
  user_device <- grDevices::dev.cur()
  on.exit(if (user_device %in% grDevices::dev.list()) grDevices::dev.off(user_device), add = TRUE)
  graphics::par(mar = c(2, 3, 4, 5))
  before <- graphics::par(no.readonly = TRUE)
  record <- list(type = "time_series", data = data.frame(x = 1:3, y = c(2, 5, 3)),
                 x = "x", y = "y", series = "", title = "Export")
  LinkEDA:::.rls_export_time_series_fallback(record, "ok.svg", "svg", 7, 5, "classic", 144)
  expect_identical(grDevices::dev.cur(), user_device)
  expect_equal(graphics::par(no.readonly = TRUE), before)
  record$series <- "absent"
  expect_error(LinkEDA:::.rls_export_time_series_fallback(record, "error.svg", "svg", 7, 5, "classic", 144))
  expect_identical(grDevices::dev.cur(), user_device)
  expect_equal(graphics::par(no.readonly = TRUE), before)
  expect_false(file.exists("Rplots.pdf"))
})


test_that("long imputed categories survive sparse native transport", {
  driver <- Sys.getenv("LINKEDA_DATA_ROUNDTRIP_TEST")
  skip_if(!nzchar(driver), "Native round-trip driver not provided")
  labels <- paste0(paste(rep("category", 25), collapse = ""), c(" A", " B"))
  original <- data.frame(group = factor(c(labels, NA), levels = labels))
  one <- two <- original
  one$group[3] <- labels[1]; two$group[3] <- labels[2]
  record <- list(dataset_type = "multiple_imputation", data = original,
    original_data = original, completed_datasets = list(one, two),
    missing_cell_mask = lapply(original, is.na), active_imputation_version = 1L,
    imputation_display_mode = "original", group = "long-mi", dataset_id = "long-mi",
    data_version = 1L, original_row_ids = 1:3)
  input <- tempfile(); output <- tempfile()
  writeLines(LinkEDA:::.rls_dataframe_payload(original, dataset_record = record), input)
  expect_identical(system2(driver, c(input, output)), 0L)
  restored <- LinkEDA:::.rls_read_native_data_payload(output)
  expect_identical(restored$columns$group, c(labels, "NA"))
  expect_identical(restored$factor_levels$group, labels)
  expect_identical(restored$imputation$sparse[[1]]$versions, as.list(labels))
})
