test_that("scatterplot smooth curves continue to use R's LOESS implementation", {
  curve <- rlispstat:::.rls_compute_smooth(mtcars$wt, mtcars$mpg, ".", 0.75, 200L)

  expect_true(curve$ok)
  expect_length(curve$x, 200L)
  expect_length(curve$y, 200L)
  expect_true(all(is.finite(curve$x)))
  expect_true(all(is.finite(curve$y)))
  expect_true(all(diff(curve$x) >= 0))
})

test_that("scatterplot smooth reports insufficient data without inventing a curve", {
  curve <- rlispstat:::.rls_compute_smooth(c(1, 1), c(2, 3), ".", 0.75, 200L)

  expect_false(curve$ok)
  expect_length(curve$x, 0L)
  expect_match(curve$message, "too few valid points")
})

test_that("native-created plots resolve smoothing data from their dataset group", {
  group <- ls_register_dataset("smooth_native_group", mtcars)
  on.exit(ls_unregister_dataset(group), add = TRUE)

  record <- rlispstat:::.rls_smooth_data_record("plot_not_registered_in_r", group)
  expect_identical(record$group, group)
  expect_equal(record$data, mtcars)
})
