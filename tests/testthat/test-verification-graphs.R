test_that("public ggplot2 checks recalculate scatter, histogram, and boxplot quantities", {
  skip_if_not_installed("ggplot2")
  data <- data.frame(
    x = c(0.1, 0.8, 1.2, 1.9, 2.7, 3.0),
    y = c(4, 7, 3, 8, 5, 12),
    group = factor(c("a", "a", "b", "b", "b", "a"))
  )

  scatter <- ggplot2::ggplot(
    data, ggplot2::aes(x = x, y = y, colour = group)
  ) + ggplot2::geom_point()
  scatter_values <- ggplot2::ggplot_build(scatter)$data[[1L]]
  expect_equal(sort(scatter_values$x), sort(data$x), tolerance = 1e-12)
  expect_equal(sort(scatter_values$y), sort(data$y), tolerance = 1e-12)

  breaks <- c(0, 1, 2, 3)
  bin <- cut(data$x, breaks = breaks, include.lowest = TRUE, right = FALSE)
  bin[data$x == max(breaks)] <- tail(levels(bin), 1L)
  reference_counts <- as.numeric(table(bin, useNA = "no"))
  expect_equal(reference_counts, c(2, 2, 2))
  expect_false(isTRUE(all.equal(reference_counts, c(2, 2, 3))))

  boxes <- ggplot2::ggplot(data, ggplot2::aes(x = group, y = y)) +
    ggplot2::geom_boxplot(coef = 1.5)
  box_values <- ggplot2::ggplot_build(boxes)$data[[1L]]
  for (index in seq_along(levels(data$group))) {
    expected <- grDevices::boxplot.stats(
      data$y[data$group == levels(data$group)[[index]]], coef = 1.5
    )$stats
    actual <- unlist(box_values[index, c("ymin", "lower", "middle", "upper", "ymax")],
                     use.names = FALSE)
    expect_equal(actual, expected, tolerance = 1e-12)
  }
})

test_that("categorical effect intervals are individual and dodged with their points", {
  skip_if_not_installed("ggplot2")
  estimates <- data.frame(
    category = factor(rep(c("1", "2"), each = 2L), levels = c("1", "2")),
    series = factor(rep(c("A", "B"), 2L)),
    estimate = c(2.0, 2.8, 3.2, 4.1),
    lower = c(1.5, 2.2, 2.6, 3.4),
    upper = c(2.6, 3.5, 3.9, 4.9)
  )
  dodge <- ggplot2::position_dodge(width = .35)
  plot <- ggplot2::ggplot(
    estimates,
    ggplot2::aes(x = category, y = estimate, colour = series, group = series)
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lower, ymax = upper), width = .10, position = dodge
    ) +
    ggplot2::geom_point(position = dodge)
  layers <- ggplot2::ggplot_build(plot)$data
  expect_equal(layers[[1L]]$ymin, estimates$lower, tolerance = 1e-12)
  expect_equal(layers[[1L]]$ymax, estimates$upper, tolerance = 1e-12)
  expect_equal(layers[[1L]]$x, layers[[2L]]$x, tolerance = 1e-12)
  expect_false(any(vapply(layers, function(layer) "ribbon" %in% names(layer), logical(1L))))
})

test_that("continuous effect intervals remain confidence bands", {
  skip_if_not_installed("ggplot2")
  estimates <- data.frame(
    x = c(0, 1, 2), estimate = c(2, 3, 5),
    lower = c(1.5, 2.4, 4.1), upper = c(2.5, 3.7, 5.8)
  )
  plot <- ggplot2::ggplot(
    estimates, ggplot2::aes(x = x, y = estimate)
  ) + ggplot2::geom_ribbon(
    ggplot2::aes(ymin = lower, ymax = upper), alpha = .16
  )
  layer <- ggplot2::ggplot_build(plot)$data[[1L]]
  expect_equal(layer$ymin, estimates$lower, tolerance = 1e-12)
  expect_equal(layer$ymax, estimates$upper, tolerance = 1e-12)
})

test_that("bar reference quantities use the selected denominator", {
  data <- data.frame(
    x = factor(c("A", "A", "A", "B", "B"), levels = c("A", "B")),
    series = factor(c("yes", "yes", "no", "yes", "no"))
  )
  counts <- as.data.frame(table(data$x, data$series), stringsAsFactors = FALSE)
  names(counts) <- c("x", "series", "count")

  counts$overall <- 100 * counts$count / sum(counts$count)
  counts$conditional <- 100 * counts$count /
    ave(counts$count, counts$x, FUN = sum)

  expect_equal(sum(counts$overall), 100, tolerance = 1e-12)
  expect_equal(
    as.numeric(tapply(counts$conditional, counts$x, sum)),
    c(100, 100), tolerance = 1e-12
  )
  expect_equal(
    counts$conditional[counts$x == "A" & counts$series == "yes"],
    200 / 3, tolerance = 1e-12
  )
})

test_that("ordinary MI graph labels identify the displayed completed dataset", {
  record <- list(
    dataset_type = "multiple_imputation",
    imputation_display_mode = "all",
    active_imputation_version = 3L,
    imputation_count = 20L
  )
  expect_identical(
    LinkEDA:::.rls_mi_title_suffix(record), " [imputation 3 of 20]"
  )
  record$imputation_display_mode <- "original"
  expect_identical(
    LinkEDA:::.rls_mi_title_suffix(record), " [original incomplete data]"
  )
})

test_that("ordinary MI graph warnings distinguish all, one, and original displays", {
  record <- list(
    dataset_type = "multiple_imputation",
    imputation_display_mode = "all",
    active_imputation_version = 3L,
    imputation_count = 20L
  )
  expect_warning(
    LinkEDA:::.rls_mi_warn_current_version(record, "Histogram"),
    "displaying all imputations; this ordinary command is not a combined MI analysis",
    fixed = TRUE
  )
  record$imputation_display_mode <- "version"
  expect_warning(
    LinkEDA:::.rls_mi_warn_current_version(record, "Histogram"),
    "using imputation 3 of 20",
    fixed = TRUE
  )
  record$imputation_display_mode <- "original"
  expect_warning(
    LinkEDA:::.rls_mi_warn_current_version(record, "Histogram"),
    "using the original incomplete data",
    fixed = TRUE
  )
})
