test_that("scatterplot smooth curves continue to use R's LOESS implementation", {
  curve <- LinkEDA:::.rls_compute_smooth(mtcars$wt, mtcars$mpg, ".", 0.75, 200L)

  expect_true(curve$ok)
  expect_length(curve$x, 200L)
  expect_length(curve$y, 200L)
  expect_true(all(is.finite(curve$x)))
  expect_true(all(is.finite(curve$y)))
  expect_length(curve$lower, 200L)
  expect_length(curve$upper, 200L)
  expect_true(all(curve$lower <= curve$y))
  expect_true(all(curve$upper >= curve$y))
  expect_true(all(diff(curve$x) >= 0))
})

test_that("straight fitted lines and confidence intervals are calculated in R", {
  curve <- LinkEDA:::.rls_compute_smooth(
    mtcars$wt, mtcars$mpg, ".", 0.75, 80L,
    confidence_level = 0.95, fit_method = "lm"
  )

  expect_true(curve$ok)
  expect_length(curve$x, 80L)
  expect_length(curve$lower, 80L)
  expect_length(curve$upper, 80L)
  expect_true(all(curve$lower <= curve$y))
  expect_true(all(curve$upper >= curve$y))
})

test_that("two distinct points permit an R regression line without inventing an interval", {
  x <- c(1, 2)
  y <- c(3, 5)
  curve <- LinkEDA:::.rls_compute_smooth(
    x, y, ".", 0.75, 5L, fit_method = "lm"
  )
  reference <- stats::predict(stats::lm(y ~ x),
                              newdata = data.frame(x = curve$x))

  expect_true(curve$ok)
  expect_equal(curve$y, as.double(reference))
  expect_length(curve$lower, 0L)
  expect_length(curve$upper, 0L)

  loess_curve <- LinkEDA:::.rls_compute_smooth(x, y, ".", 0.75, 5L)
  expect_false(loess_curve$ok)
})

test_that("two-point regression groups survive MI and trellis colour filtering", {
  point_curves <- LinkEDA:::.rls_smooth_curves_for_point_set(
    rows = 1:4, x = c(1, 2, 3, 4), y = c(3, 5, 2, 6),
    scope = "color", span = 0.75,
    row_colors = c(`1` = "blue", `2` = "blue", `3` = "red", `4` = "red"),
    imputation = 1L, fit_method = "lm"
  )
  expect_length(point_curves, 2L)
  expect_true(all(vapply(point_curves, function(curve) curve$ok, logical(1))))
  expect_true(all(vapply(point_curves, function(curve) length(curve$lower) == 0L,
                         logical(1))))

  group <- ls_register_dataset("smooth_two_point_trellis", mtcars)
  on.exit(ls_unregister_dataset(group), add = TRUE)
  sent <- list()
  local_mocked_bindings(.rls_send = function(lines, expect_reply = TRUE) {
    sent[[length(sent) + 1L]] <<- lines
    "OK"
  }, .package = "LinkEDA")
  LinkEDA:::.rls_handle_smooth_needed(
    "native_two_point_scatter", group, "color", "wt", "mpg",
    row_colors = c(`1` = "blue", `2` = "blue", `3` = "red", `4` = "red"),
    fit_method = "lm"
  )
  expect_identical(sent[[1L]][1:6],
                   c("ADD_SMOOTH", "native_two_point_scatter", "color",
                     "FIT_CURVE_V2", "lm", "3"))
  LinkEDA:::.rls_handle_trellis_smooth_needed(
    "native_two_point_panel", group, "panel_1", "overall", "wt", "mpg",
    panel_rows = 1:4, panel_colors = c("blue", "blue", "red", "red"),
    fit_method = "lm"
  )
  expect_identical(sent[[2L]][1:7],
                   c("ADD_TRELLIS_SMOOTH", "native_two_point_panel",
                     "panel_1", "overall", "FIT_CURVE_V2", "lm", "2"))
})

test_that("scatter-matrix cell payload fits the exact plotted points in R", {
  group <- ls_register_dataset("matrix_explicit_fit", mtcars)
  on.exit(ls_unregister_dataset(group), add = TRUE)
  sent <- list()
  local_mocked_bindings(.rls_send = function(lines, expect_reply = TRUE) {
    sent[[length(sent) + 1L]] <<- lines
    "OK"
  }, .package = "LinkEDA")

  payload <- c(
    "TRELLIS_SMOOTH_NEEDED", "matrix_plot", group, "matrix:2:0:1",
    "overall", "wt", "mpg", "0.75", "2", "1", "2",
    "0", "0", "0", "FIT_CURVE_V2", "lm", "0.95",
    "EXPLICIT_POINTS_V1", "2", "1", "1", "3", "2", "2", "5"
  )
  LinkEDA:::.rls_process_control_lines(paste(payload, collapse = "\t"))
  expect_length(sent, 1L)
  expect_identical(sent[[1L]][1:7],
                   c("ADD_TRELLIS_SMOOTH", "matrix_plot", "matrix:2:0:1",
                     "overall", "FIT_CURVE_V2", "lm", "1"))
  fitted <- scan(text = sent[[1L]][12L], quiet = TRUE)
  expect_equal(c(fitted[1L], fitted[length(fitted)]), c(3, 5))
  expect_identical(sent[[1L]][13L], "")
  expect_identical(sent[[1L]][14L], "")
})

test_that("scatterplot smooth reports insufficient data without inventing a curve", {
  curve <- LinkEDA:::.rls_compute_smooth(c(1, 1), c(2, 3), ".", 0.75, 200L)

  expect_false(curve$ok)
  expect_length(curve$x, 0L)
  expect_match(curve$message, "too few valid points")
})

test_that("native-created plots resolve smoothing data from their dataset group", {
  group <- ls_register_dataset("smooth_native_group", mtcars)
  on.exit(ls_unregister_dataset(group), add = TRUE)

  record <- LinkEDA:::.rls_smooth_data_record("plot_not_registered_in_r", group)
  expect_identical(record$group, group)
  expect_equal(unclass(record$data), unclass(mtcars), ignore_attr = TRUE)
  expect_length(attr(record$data, "linkeda_row_ids"), nrow(mtcars))
})

test_that("sparse point colours keep unassigned rows in the default smooth group", {
  colours <- c(`2` = "#0078D4", `5` = "#E83E8C")

  expect_identical(
    LinkEDA:::.rls_colors_for_rows(1:6, colours),
    c("black", "#0078D4", "black", "black", "#E83E8C", "black")
  )
  expect_identical(
    LinkEDA:::.rls_colors_for_rows(1:3, character()),
    rep("black", 3L)
  )
})

test_that("native smoothing task consumes its local selection snapshot", {
  group <- ls_register_dataset("smooth_selected_snapshot", mtcars)
  on.exit(ls_unregister_dataset(group), add = TRUE)

  sent <- list()
  local_mocked_bindings(.rls_send = function(lines, expect_reply = TRUE) {
    sent[[length(sent) + 1L]] <<- lines
    "OK"
  }, .package = "LinkEDA")

  LinkEDA:::.rls_handle_smooth_needed(
    "native_selected", group, "selected", "wt", "mpg", 0.75,
    selected_rows = 1:12, row_colors = character()
  )
  expect_identical(sent[[1L]][1:6],
                   c("ADD_SMOOTH", "native_selected", "selected",
                     "FIT_CURVE_V2", "loess", "1"))
})

test_that("native smooth refits use only the cases displayed by a scoped plot", {
  group <- ls_register_dataset("smooth_scoped_plot", data.frame(
    x = 1:5, y = c(1, 2, 3, 4, 100)))
  on.exit(ls_unregister_dataset(group), add = TRUE)

  fitted_rows <- list()
  sent <- list()
  local_mocked_bindings(
    .rls_compute_smooth = function(x, y, group_id, ...) {
      fitted_rows[[length(fitted_rows) + 1L]] <<- list(x = x, y = y)
      list(ok = TRUE, group = group_id, x = x, y = y,
           lower = numeric(), upper = numeric())
    },
    .rls_send = function(lines, expect_reply = TRUE) {
      sent[[length(sent) + 1L]] <<- lines
      "OK"
    }, .package = "LinkEDA"
  )

  LinkEDA:::.rls_process_control_lines(
    "SMOOTH_NEEDED|scoped_plot|smooth_scoped_plot|overall|x|y|0.75|0|0|VISIBLE_ROWS_V1|3|1|2|3|FIT_CURVE_V2|lm|0.95"
  )
  expect_equal(fitted_rows[[1L]]$x, 1:3)
  expect_equal(fitted_rows[[1L]]$y, 1:3)
  expect_identical(sent[[1L]][1:6],
                   c("ADD_SMOOTH", "scoped_plot", "overall", "FIT_CURVE_V2", "lm", "1"))

  fitted_rows <- list()
  LinkEDA:::.rls_handle_smooth_needed(
    "scoped_plot", group, "selected", "x", "y", 0.75,
    selected_rows = 1:5, visible_rows = 1:3
  )
  expect_equal(fitted_rows[[1L]]$x, 1:3)

  fitted_rows <- list()
  LinkEDA:::.rls_handle_smooth_needed(
    "scoped_plot", group, "color", "x", "y", 0.75,
    row_colors = c(`4` = "red", `5` = "red"), visible_rows = 1:3
  )
  expect_length(fitted_rows, 1L)
  expect_equal(fitted_rows[[1L]]$x, 1:3)

  fitted_rows <- list()
  LinkEDA:::.rls_handle_smooth_needed(
    "scoped_plot", group, "overall", "x", "y", 0.75,
    imputation_point_sets = list(list(imputation = 1L,
      rows = 1:5, x = 1:5, y = c(1, 2, 3, 4, 100))),
    visible_rows = 1:3
  )
  expect_equal(fitted_rows[[1L]]$x, 1:3)
})

test_that("native smoothing task consumes its local colour snapshot", {
  group <- ls_register_dataset("smooth_colour_snapshot", mtcars)
  on.exit(ls_unregister_dataset(group), add = TRUE)

  sent <- list()
  local_mocked_bindings(.rls_send = function(lines, expect_reply = TRUE) {
    sent[[length(sent) + 1L]] <<- lines
    "OK"
  }, .package = "LinkEDA")

  colours <- setNames(rep(c("blue", "red"), each = 16), as.character(1:32))
  LinkEDA:::.rls_handle_smooth_needed(
    "native_colour", group, "color", "wt", "mpg", 0.75,
    selected_rows = integer(), row_colors = colours
  )
  expect_identical(sent[[1L]][1:6],
                   c("ADD_SMOOTH", "native_colour", "color",
                     "FIT_CURVE_V2", "loess", "2"))
})

test_that("colour smoothing accepts the sparse colour map used by linked brushing", {
  group <- ls_register_dataset("smooth_sparse_colour_snapshot", mtcars)
  on.exit(ls_unregister_dataset(group), add = TRUE)

  sent <- list()
  local_mocked_bindings(.rls_send = function(lines, expect_reply = TRUE) {
    sent[[length(sent) + 1L]] <<- lines
    "OK"
  }, .package = "LinkEDA")

  colours <- setNames(rep("#0078D4", 8L), as.character(1:8))
  LinkEDA:::.rls_handle_smooth_needed(
    "native_sparse_colour", group, "color", "wt", "mpg", 0.75,
    selected_rows = integer(), row_colors = colours
  )
  expect_identical(sent[[1L]][1:6],
                   c("ADD_SMOOTH", "native_sparse_colour", "color",
                     "FIT_CURVE_V2", "loess", "2"))
})

test_that("multiple-imputation smoothing fits one R LOESS curve per completed dataset", {
  group <- ls_register_dataset("smooth_imputation_bundle", mtcars)
  on.exit(ls_unregister_dataset(group), add = TRUE)

  sent <- list()
  local_mocked_bindings(.rls_send = function(lines, expect_reply = TRUE) {
    sent[[length(sent) + 1L]] <<- lines
    "OK"
  }, .package = "LinkEDA")

  point_sets <- list(
    list(imputation = 1L, rows = 1:6, x = 1:6, y = c(1, 2, 2, 4, 5, 7)),
    list(imputation = 2L, rows = 1:6, x = 1:6, y = c(2, 2, 3, 5, 6, 8))
  )
  LinkEDA:::.rls_handle_smooth_needed(
    "native_mi", group, "overall", "wt", "mpg", 0.75,
    selected_rows = integer(), row_colors = character(),
    imputation_point_sets = point_sets
  )

  expect_identical(sent[[1L]][1:6],
                   c("ADD_SMOOTH", "native_mi", "overall",
                     "FIT_CURVE_V2", "loess", "2"))
  expect_match(sent[[1L]][7L], "mi:1$", fixed = FALSE)
  expect_match(sent[[1L]][14L], "mi:2$", fixed = FALSE)
})

test_that("native MI diagnostic smooth payload is parsed into one curve per imputation", {
  group <- ls_register_dataset("smooth_imputation_protocol", mtcars)
  on.exit(ls_unregister_dataset(group), add = TRUE)

  sent <- list()
  local_mocked_bindings(.rls_send = function(lines, expect_reply = TRUE) {
    sent[[length(sent) + 1L]] <<- lines
    "OK"
  }, .package = "LinkEDA")

  encode_set <- function(imputation, x, y) {
    rows <- seq_along(x)
    c(as.character(imputation), as.character(length(rows)),
      as.vector(rbind(as.character(rows),
                      sprintf("%.17g", x),
                      sprintf("%.17g", y))))
  }
  payload <- c(
    "SMOOTH_NEEDED", "native_mi_protocol", group, "overall",
    "fitted", "observed", "0.75", "0", "0",
    "MI_POINT_SETS_V1", "2",
    encode_set(1L, 1:6, c(1, 2, 2, 4, 5, 7)),
    encode_set(2L, 1:6, c(2, 2, 3, 5, 6, 8))
  )
  LinkEDA:::.rls_process_control_lines(paste(payload, collapse = "|"))

  expect_length(sent, 1L)
  expect_identical(sent[[1L]][1:6],
                   c("ADD_SMOOTH", "native_mi_protocol", "overall",
                     "FIT_CURVE_V2", "loess", "2"))
  expect_match(sent[[1L]][7L], "mi:1$")
  expect_match(sent[[1L]][14L], "mi:2$")
})

test_that("a displayed diagnostic imputation fits its straight line from supplied coordinates", {
  group <- ls_register_dataset("smooth_diagnostic_single", mtcars)
  on.exit(ls_unregister_dataset(group), add = TRUE)

  sent <- list()
  local_mocked_bindings(.rls_send = function(lines, expect_reply = TRUE) {
    sent[[length(sent) + 1L]] <<- lines
    "OK"
  }, .package = "LinkEDA")

  x <- c(10, 20, 30, 40, 50)
  y <- c(-2, -1, 0.5, 1, 3)
  rows <- seq_along(x)
  encoded <- c(
    "SMOOTH_NEEDED", "native_diagnostic_single", group, "overall",
    "fitted", "residual", "0.75", "0", "0",
    "MI_POINT_SETS_V1", "1", "7", as.character(length(rows)),
    as.vector(rbind(as.character(rows), sprintf("%.17g", x), sprintf("%.17g", y))),
    "FIT_CURVE_V2", "lm", "0.95"
  )
  LinkEDA:::.rls_process_control_lines(paste(encoded, collapse = "|"))

  expect_length(sent, 1L)
  expect_identical(sent[[1L]][1:6],
                   c("ADD_SMOOTH", "native_diagnostic_single", "overall",
                     "FIT_CURVE_V2", "lm", "1"))
  expect_match(sent[[1L]][7L], "mi:7$")
})
