test_that("window layout API is exported", {
  expect_true("ls_reset_window_layout" %in% getNamespaceExports("LinkEDA"))
  expect_true("ls_tile_windows" %in% getNamespaceExports("LinkEDA"))
})
