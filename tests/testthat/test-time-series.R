test_that("time-series preparation creates one indexed line per series", {
  data <- data.frame(
    time = c(3, 1, 2, 1),
    value = c(30, 10, 22, 20),
    series = factor(c("A", "A", "B", "B"), levels = c("B", "A"))
  )
  prepared <- LinkEDA:::.rls_prepare_time_series_data(data, "time", "value", "series")
  expect_equal(prepared$time_type, "numeric")
  expect_equal(prepared$labels, c("B", "A"))
  expect_equal(prepared$point_series, c(1L, 1L, 0L, 0L))
  expect_equal(prepared$rows, 1:4)
})

test_that("time-series preparation accepts Date and excludes incomplete pairs", {
  data <- data.frame(
    day = as.Date("2024-01-01") + 0:2,
    value = c(1, NA, 3)
  )
  prepared <- LinkEDA:::.rls_prepare_time_series_data(data, "day", "value")
  expect_equal(prepared$time_type, "date")
  expect_equal(prepared$rows, c(1L, 3L))
  expect_equal(prepared$labels, "value")
})

test_that("numeric calendar years use readable temporal labels", {
  data <- data.frame(year = 2019:2023, value = seq_len(5))
  prepared <- LinkEDA:::.rls_prepare_time_series_data(data, "year", "value")
  expect_equal(prepared$time_type, "year")

  ordinary <- transform(data, year = seq_len(5))
  expect_equal(
    LinkEDA:::.rls_prepare_time_series_data(ordinary, "year", "value")$time_type,
    "numeric"
  )
})

test_that("time-series preparation validates variable roles", {
  data <- data.frame(time = letters[1:3], value = 1:3)
  expect_error(
    LinkEDA:::.rls_prepare_time_series_data(data, "time", "value"),
    "numeric, Date, or POSIXt"
  )
  expect_error(
    LinkEDA:::.rls_prepare_time_series_data(transform(data, time = 1:3), "time", "missing"),
    "was not found"
  )
})

test_that("time-series display options have stable public choices", {
  expect_identical(
    match.arg("start_labels", c("legend", "start_labels", "none")),
    "start_labels"
  )
  expect_identical(
    match.arg("bottom_left", c("top_right", "top_left", "bottom_right", "bottom_left")),
    "bottom_left"
  )
})
