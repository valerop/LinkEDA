test_that("barplot preparation supports all width modes", {
  d <- data.frame(x = c("a", "a", "b", "b", "b"), y = c("u", "v", "u", "u", "v"))

  equal <- LinkEDA:::.rls_prepare_barplot_data(d, "x", "y", mode = "conditional_percent", bar_width = "equal")
  prop_n <- LinkEDA:::.rls_prepare_barplot_data(d, "x", "y", mode = "conditional_percent", bar_width = "proportional_n")
  prop_pct <- LinkEDA:::.rls_prepare_barplot_data(d, "x", "y", mode = "conditional_percent", bar_width = "proportional_percent")

  expect_equal(equal$bars$bar_width_value, c(1, 1))
  expect_equal(prop_n$bars$bar_width_value, c(2, 3))
  expect_equal(prop_pct$bars$bar_width_value, c(40, 60))
  expect_equal(sum(prop_n$bars$visual_width), 1)
  expect_equal(sum(prop_pct$bars$visual_width), 1)
})

test_that("barplot modes store counts and percentages", {
  d <- data.frame(x = c("a", "a", "b", "b", "b"), y = c("u", "v", "u", "u", "v"))
  out <- LinkEDA:::.rls_prepare_barplot_data(d, "x", "y", mode = "conditional_percent", bar_width = "proportional_n")

  expect_equal(out$total_n, 5L)
  expect_equal(out$bars$bar_n, c(2L, 3L))
  expect_equal(out$bars$bar_percent, c(40, 60))

  by_bar <- split(out$segments$conditional_percent, out$segments$x_condition)
  expect_true(all(abs(vapply(by_bar, sum, numeric(1)) - 100) < 1e-8))

  b_u <- out$segments[out$segments$x_condition == "b" & out$segments$y_level == "u", ]
  expect_equal(b_u$count, 2L)
  expect_equal(b_u$bar_n, 3L)
  expect_equal(b_u$total_n, 5L)
  expect_equal(b_u$conditional_percent, 100 * 2 / 3)
  expect_equal(b_u$overall_percent, 40)
})

test_that("barplot preparation stores structured segment visual defaults", {
  d <- data.frame(x = c("a", "a", "b"), y = c("Automatic", "Manual", "Manual"))
  out <- LinkEDA:::.rls_prepare_barplot_data(d, "x", "y", mode = "conditional_percent", bar_width = "equal")

  expect_true(all(c("Automatic", "Manual") %in% names(out$y_level_colors)))
  expect_equal(out$y_level_colors[["Automatic"]], "white")
  expect_equal(out$y_level_colors[["Manual"]], "white")
  expect_equal(unname(out$y_level_alpha), c(0.7, 0.7))
  expect_equal(out$y_level_patterns[["Automatic"]], "diagonal_slash")
  expect_equal(out$y_level_patterns[["Manual"]], "dots")

  expect_length(out$segment_color_overrides, 0L)
  expect_length(out$segment_alpha_overrides, 0L)
  expect_length(out$segment_pattern_overrides, 0L)
  expect_equal(out$default_segment_alpha, 0.7)
  expect_true(out$show_patterns)
  expect_equal(out$segment_encoding_mode, "transparent_color_pattern")
})

test_that("barplot structured tooltips contain denominator and percent details", {
  d <- data.frame(x = c("a", "a", "b"), y = c("u", "v", "u"))
  out <- LinkEDA:::.rls_prepare_barplot_data(d, "x", "y", mode = "conditional_percent", bar_width = "proportional_percent")

  tooltip <- out$segments$tooltip[[1L]]
  expect_match(tooltip, "n = ")
  expect_match(tooltip, "bar denominator = ")
  expect_match(tooltip, "conditional percent = ")
  expect_match(tooltip, "overall percent = ")
  expect_match(tooltip, "bar N = ")
  expect_match(tooltip, "bar percent of total = ")
  expect_match(tooltip, "bar width mode = proportional_percent")
})

test_that("barplot missing-value handling includes and excludes rows correctly", {
  d <- data.frame(x = c("a", NA, "b", "b"), y = c("u", "v", NA, "u"))

  included <- LinkEDA:::.rls_prepare_barplot_data(d, "x", "y", include_missing = TRUE)
  expect_equal(included$total_n, 4L)
  expect_true("NA" %in% included$bars$x_condition)
  expect_equal(included$excluded_row_ids, integer())

  excluded <- LinkEDA:::.rls_prepare_barplot_data(d, "x", "y", include_missing = FALSE)
  expect_equal(excluded$total_n, 2L)
  expect_equal(excluded$excluded_row_ids, c(2L, 3L))
  expect_false("NA" %in% excluded$bars$x_condition)
})

test_that("barplot selection summary uses original row ids for bars and segments", {
  d <- data.frame(
    x = c("a", "a", "a", "b", "b"),
    y = c("u", "u", "v", "u", "v")
  )
  out <- LinkEDA:::.rls_prepare_barplot_data(
    d, "x", "y", mode = "conditional_percent", bar_width = "proportional_n"
  )

  selection <- LinkEDA:::.rls_barplot_selection_summary(out, selected_rows = c(2, 3, 5, 99))
  a_bar <- selection$bars[selection$bars$x_condition == "a", ]
  expect_equal(a_bar$selected_count, 2L)
  expect_equal(a_bar$total_count, 3L)
  expect_equal(a_bar$selected_fraction, 2 / 3)
  expect_equal(a_bar$selected_row_ids[[1L]], c(2L, 3L))

  a_u <- selection$segments[selection$segments$x_condition == "a" & selection$segments$y_level == "u", ]
  expect_equal(a_u$selected_count, 1L)
  expect_equal(a_u$total_count, 2L)
  expect_equal(a_u$selected_fraction, 1 / 2)
  expect_equal(a_u$selected_row_ids[[1L]], 2L)

  a_v <- selection$segments[selection$segments$x_condition == "a" & selection$segments$y_level == "v", ]
  expect_equal(a_v$selected_count, 1L)
  expect_equal(a_v$total_count, 1L)
  expect_equal(a_v$selected_fraction, 1)
  expect_equal(a_v$selected_row_ids[[1L]], 3L)

  b_u <- selection$segments[selection$segments$x_condition == "b" & selection$segments$y_level == "u", ]
  expect_equal(b_u$selected_count, 0L)
  expect_equal(b_u$selected_fraction, 0)
  expect_equal(b_u$selected_row_ids[[1L]], integer())
})

test_that("barplot selection summary is independent of bar width and percentages", {
  d <- data.frame(
    x = c("a", "a", "b", "b", "b"),
    y = c("u", "v", "u", "u", "v")
  )
  out <- LinkEDA:::.rls_prepare_barplot_data(
    d, "x", "y", mode = "conditional_percent", bar_width = "proportional_percent"
  )
  before_widths <- out$bars$visual_width
  before_percentages <- out$segments$conditional_percent

  selection <- LinkEDA:::.rls_barplot_selection_summary(out, selected_rows = c(1, 4))

  expect_equal(out$bars$visual_width, before_widths)
  expect_equal(out$segments$conditional_percent, before_percentages)
  expect_equal(selection$bars$selected_count, c(1L, 1L))
  expect_equal(selection$bars$total_count, out$bars$bar_n)
  expect_equal(selection$segments$selected_fraction[selection$segments$y_level == "u"], c(1, 1 / 2))
})

test_that("barplot selection summary handles empty selections", {
  d <- data.frame(x = c("a", "a", "b"), y = c("u", "v", "u"))
  out <- LinkEDA:::.rls_prepare_barplot_data(d, "x", "y")

  selection <- LinkEDA:::.rls_barplot_selection_summary(out)

  expect_equal(selection$bars$selected_count, c(0L, 0L))
  expect_equal(selection$bars$selected_fraction, c(0, 0))
  expect_true(all(selection$segments$selected_count == 0L))
  expect_true(all(selection$segments$selected_fraction == 0))
})

test_that("barplot supports multiple X variables with proportional widths", {
  d <- data.frame(
    cyl = c("4", "4", "6", "6", "6"),
    gear = c("3", "4", "3", "3", "5"),
    am = c("Auto", "Manual", "Auto", "Manual", "Manual")
  )
  out <- LinkEDA:::.rls_prepare_barplot_data(
    d, x = c("cyl", "gear"), y = "am",
    mode = "conditional_percent", bar_width = "proportional_n"
  )

  expect_true(any(grepl("cyl=4; gear=3", out$bars$x_condition, fixed = TRUE)))
  expect_equal(sum(out$bars$bar_n), out$total_n)
  expect_equal(sum(out$bars$visual_width), 1)
})

test_that("barplot orders multiple X variables as nested subdivisions", {
  d <- data.frame(
    first = c("A", "B", "A", "B", "A", "B"),
    second = c("y", "x", "x", "y", "z", "z"),
    third = c("m", "m", "n", "n", "m", "n")
  )

  two_x <- LinkEDA:::.rls_prepare_barplot_data(d, x = c("first", "second"))
  expect_equal(two_x$bars$x_condition, c(
    "first=A; second=y",
    "first=A; second=x",
    "first=A; second=z",
    "first=B; second=y",
    "first=B; second=x",
    "first=B; second=z"
  ))

  sorted <- LinkEDA:::.rls_prepare_barplot_data(d, x = c("first", "second"), sort_x = TRUE)
  expect_equal(sorted$bars$x_condition, c(
    "first=A; second=x",
    "first=A; second=y",
    "first=A; second=z",
    "first=B; second=x",
    "first=B; second=y",
    "first=B; second=z"
  ))

  three_x <- LinkEDA:::.rls_prepare_barplot_data(d, x = c("first", "second", "third"))
  expect_true(all(grepl("^first=A", three_x$bars$x_condition[1:3])))
  expect_true(all(grepl("^first=B", three_x$bars$x_condition[4:6])))
  expect_equal(three_x$bars$x_condition[[1L]], "first=A; second=y; third=m")
  expect_equal(three_x$bars$x_condition[[4L]], "first=B; second=y; third=n")
})

test_that("barplot repeats added variable level sequence within each outer category", {
  d <- data.frame(
    group = c("A", "A", "B", "B"),
    am = c(1, 0, 0, 1)
  )

  out <- LinkEDA:::.rls_prepare_barplot_data(d, x = c("group", "am"))

  expect_equal(out$bars$x_condition, c(
    "group=A; am=1",
    "group=A; am=0",
    "group=B; am=1",
    "group=B; am=0"
  ))
})

test_that("barplot validates API arguments", {
  expect_error(LinkEDA:::.rls_prepare_barplot_data(1, "x"), "data.frame")
  expect_error(LinkEDA:::.rls_prepare_barplot_data(data.frame(x = 1), "missing"), "not found")
  expect_error(LinkEDA:::.rls_prepare_barplot_data(data.frame(x = 1), "x", mode = "bad"), "should be one of")
  expect_error(LinkEDA:::.rls_prepare_barplot_data(data.frame(x = 1), "x", include_missing = NA), "TRUE or FALSE")
})
