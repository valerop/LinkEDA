test_that("overlap display choices preserve coordinates and control exported color and area", {
  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  colors <- LinkEDA:::.rls_plot_theme_colors("theme_modern")
  shaded <- LinkEDA:::.rls_export_scatter_point_color(list(shade_overlap = TRUE), colors)
  solid <- LinkEDA:::.rls_export_scatter_point_color(list(shade_overlap = FALSE), colors)
  expect_equal(unname(grDevices::col2rgb(shaded, alpha = TRUE)[4L, ]), 89)
  expect_equal(unname(grDevices::col2rgb(solid, alpha = TRUE)[4L, ]), 255)
  x <- c(1, 2, 2, 2, 2, 3); y <- c(1, 1, 1, 1, 1, 1)
  expect_equal(LinkEDA:::.rls_export_scatter_point_size(list(size_by_overlap = FALSE), x, y), rep(1, 6))
  expect_equal(LinkEDA:::.rls_export_scatter_point_size(list(size_by_overlap = TRUE), x, y)^2, c(1, 4, 4, 4, 4, 1))
  for (shade in c(FALSE, TRUE)) for (size in c(FALSE, TRUE)) {
    record <- list(data = data.frame(x, y), x = "x", y = "y", title = "Overlap",
                   shade_overlap = shade, size_by_overlap = size)
    path <- tempfile(fileext = ".svg")
    LinkEDA:::.rls_export_scatter_fallback(record, path, "svg", 6, 4, "theme_modern", 144)
    expect_gt(file.info(path)$size, 1000)
    expect_match(paste(readLines(path, warn = FALSE), collapse = ""), "<svg")
    unlink(path)
  }
})
