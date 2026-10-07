test_that("standardized box exports retain missingness and observed sample sizes", {
  testthat::local_mocked_bindings(.rls_start_backend = function(...) NULL,
    .rls_send = function(...) "OK", .package = "LinkEDA")
  captured <- NULL; original <- graphics::boxplot.default
  testthat::local_mocked_bindings(boxplot.default = function(x, ...) {
    out <- original(x, ...); captured <<- list(input = x, result = out); out
  }, .package = "graphics")
  data <- data.frame(a = c(1, 2, 8, NA, NA, NA),
                     constant = c(2, NA, 2, NA, 2, NA), empty = rep(NA_real_, 6))
  p <- ls_boxplot(data, y = "a", standardize = TRUE)
  ls_export_plot(p, tempfile(fileext = ".svg"), format = "svg")
  expect_equal(captured$result$n, 3)
  expect_equal(captured$result$stats[3, 1], stats::median(base::scale(data$a), na.rm = TRUE))
  record <- LinkEDA:::.rls_plot_record(p)
  record$boxplot_variables <- names(data)
  record$connect_rows <- TRUE
  LinkEDA:::.rls_export_boxplot_fallback(record, tempfile(fileext = ".svg"), "svg", 7, 5, "classic", 144)
  expect_equal(captured$result$n, c(3, 3, 0))
  expect_identical(is.na(captured$input), is.na(data))
  expect_equal(captured$input$constant[!is.na(data$constant)], rep(0, 3))
})

test_that("SVG histograms use the same interval membership as the linked histogram", {
  testthat::local_mocked_bindings(.rls_start_backend = function(...) NULL,
    .rls_send = function(...) "OK", .package = "LinkEDA")
  captured <- NULL; original <- graphics::hist.default
  testthat::local_mocked_bindings(hist.default = function(x, ...) {
    out <- original(x, ...); captured <<- out; out
  }, .package = "graphics")
  data <- data.frame(x = c(0, 1, 1, 1, 2, 3, 4, NA, Inf))
  for (breaks in list(0:4, c(0, 1, 2, 4))) {
    p <- ls_histogram(data, "x", breaks = breaks)
    prepared <- LinkEDA:::.rls_prepare_histogram_data(data, "x", breaks = breaks)
    ls_export_plot(p, tempfile(fileext = ".svg"), format = "svg")
    expect_identical(captured$breaks, as.double(breaks))
    expect_identical(captured$counts, tabulate(prepared$bin, nbins = length(breaks) - 1L))
  }
  record <- LinkEDA:::.rls_plot_record(p); record$histogram_bins <- 3L
  LinkEDA:::.rls_export_histogram_fallback(record, tempfile(fileext = ".svg"), "svg", 7, 5, "classic", 144)
  expect_equal(captured$breaks, seq(0, 4, length.out = 4))
})

test_that("SVG bars respect measure, missingness, order and proportional widths", {
  testthat::local_mocked_bindings(.rls_start_backend = function(...) NULL,
    .rls_send = function(...) "OK", .package = "LinkEDA")
  captured <- NULL; original <- graphics::barplot.default
  testthat::local_mocked_bindings(barplot.default = function(height, ...) {
    captured <<- c(list(height = height), list(...)); original(height, ...)
  }, .package = "graphics")
  data <- data.frame(x = c("B", "A", "A", "A", NA), y = c("yes", "yes", "yes", "no", "yes"))
  for (mode in c("count", "conditional_percent", "overall_percent")) {
    for (missing in c(FALSE, TRUE)) {
      p <- ls_barplot(data, "x", "y", mode = mode, include_missing = missing,
                       bar_width = "proportional_n", sort_x = TRUE, sort_y = TRUE)
      ls_export_plot(p, tempfile(fileext = ".svg"), format = "svg")
      expected <- table(factor(data$y), factor(data$x), useNA = if (missing) "ifany" else "no")
      if (mode == "conditional_percent") expected <- 100 * prop.table(expected, 2)
      if (mode == "overall_percent") expected <- 100 * prop.table(expected)
      expect_equal(unname(captured$height), unname(as.matrix(expected)), ignore_attr = TRUE)
      expect_equal(captured$width, if (missing) c(3, 1, 1)/5 else c(3, 1)/4)
      expect_equal(sum(captured$height), if (mode == "count") if (missing) 5 else 4 else
        if (mode == "overall_percent") 100 else if (missing) 300 else 200)
    }
  }
  p <- ls_barplot(data, c("x", "y"), mode = "count", include_missing = FALSE)
  ls_export_plot(p, tempfile(fileext = ".svg"), format = "svg")
  expect_equal(sum(captured$height), 4)
  expect_equal(ncol(captured$height), 3)
})

test_that("native SVG requests refresh options and exact histogram cuts", {
  id <- "export-current-distribution"
  data <- data.frame(x = c(0, 1, 1, 1, 2, 3, 4), g = c("B", "A", "A", "A", "B", "B", NA))
  LinkEDA:::.rls_register_dataset(id, data, activate = FALSE)
  replies <- list()
  testthat::local_mocked_bindings(.rls_send = function(x) {replies[[length(replies)+1L]] <<- x; "OK"},
                                 .package = "LinkEDA")
  send <- function(plot, kind, x, y, options) {
    path <- tempfile(fileext = ".svg")
    LinkEDA:::.rls_handle_plot_export_needed(c("PLOT_EXPORT_NEEDED", "request", plot, path,
      "svg", "save", "7", "5", id, kind, x, y, "Current options",
      as.character(length(options)/2), options))
    expect_identical(tail(replies, 1)[[1]][3], "ok")
    expect_true(file.exists(path))
    LinkEDA:::.rls_plot_record(plot)
  }
  state <- send("native-bar-current", "barplot", "g", "", c("bar_mode", "overall_percent",
    "bar_width", "proportional_n", "include_missing", "FALSE", "sort_x", "TRUE"))
  expect_identical(state$mode, "overall_percent")
  expect_identical(state$bar_width, "proportional_n")
  expect_false(state$include_missing)
  expect_true(state$sort_x)
  state <- send("native-bar-current", "barplot", "g", "", c("bar_mode", "count", "bar_width", "equal"))
  expect_identical(state$mode, "count")
  state <- send("native-hist-current", "histogram", "x", "", c("histogram_bins", "4",
    as.vector(rbind(rep("histogram_break", 5), as.character(0:4)))))
  expect_equal(state$breaks, 0:4)
  state <- send("native-hist-current", "histogram", "x", "", c("histogram_bins", "2",
    "histogram_break", "0", "histogram_break", "2", "histogram_break", "4"))
  expect_equal(state$breaks, c(0, 2, 4))
})
