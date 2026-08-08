test_that("trellis scatterplot preparation uses X, Y, By, and complete cases", {
  data <- data.frame(
    x = c(2, 3, 4, NA, 6),
    y = c(20, 18, Inf, 14, 10),
    panel = factor(c("6", "4", "8", "6", "8"), levels = c("4", "6", "8"))
  )
  prepared <- rlispstat:::.rls_prepare_trellis_scatterplot_data(data, "x", "y", "panel")
  expect_identical(prepared$rows, c(1L, 2L, 5L))
  expect_identical(prepared$panel_levels, c("4", "6", "8"))
  expect_identical(prepared$panel_indices, c(1L, 0L, 2L))
  expect_error(
    rlispstat:::.rls_prepare_trellis_scatterplot_data(data, "x", "x", "panel"),
    "different"
  )
})

test_that("trellis conditioning accepts discrete numeric values in numeric order", {
  data <- data.frame(x = 1:8, y = 8:1, by = c(8, 4, 6, 8, 4, 6, 8, 4))
  prepared <- rlispstat:::.rls_prepare_trellis_scatterplot_data(data, "x", "y", "by")
  expect_identical(prepared$panel_levels, c("4", "6", "8"))
  expect_error(
    rlispstat:::.rls_prepare_trellis_scatterplot_data(
      data.frame(x = 1:100, y = 1:100, by = 1:100), "x", "y", "by"
    ),
    "too many|discrete"
  )
})

test_that("trellis layout choices and panel ordering are deterministic", {
  expect_identical(rlispstat:::.rls_trellis_columns(3L, 14, 7, "automatic"), 3L)
  expect_identical(rlispstat:::.rls_trellis_columns(3L, 5, 10, "automatic"), 1L)
  expect_identical(rlispstat:::.rls_trellis_columns(5L, 14, 8, "automatic"), 3L)
  expect_identical(rlispstat:::.rls_trellis_columns(5L, 8, 8, "one_row"), 5L)
  expect_identical(rlispstat:::.rls_trellis_columns(5L, 8, 8, "one_column"), 1L)
  expect_identical(rlispstat:::.rls_trellis_columns(5L, 8, 8, "grid"), 3L)
  expect_identical(rlispstat:::.rls_trellis_columns(5L, 8, 8, "two_columns"), 2L)
  expect_identical(
    rlispstat:::.rls_order_trellis_levels(c("10", "2", "8"), "ascending"),
    c("2", "8", "10")
  )
  expect_identical(
    rlispstat:::.rls_order_trellis_levels(c("10", "2", "8"), "descending"),
    c("10", "8", "2")
  )
})

test_that("trellis preparation drops missing and unused factor levels stably", {
  data <- data.frame(
    x = 1:6,
    y = 6:1,
    panel = factor(c("a long label", "b", NA, "a long label", "b", "b"),
                   levels = c("unused", "a long label", "b"))
  )
  prepared <- rlispstat:::.rls_prepare_trellis_scatterplot_data(data, "x", "y", "panel")
  expect_identical(prepared$panel_levels, c("a long label", "b"))
  expect_false(3L %in% prepared$rows)
})

test_that("trellis scatterplot fallback export creates a two-dimensional file", {
  record <- list(
    data = data.frame(x = c(1, 2, 3, 4), y = c(8, 6, 4, 2),
                      panel = factor(c("a", "a", "b", "b"))),
    x = "x", y = "y", condition = "panel",
    title = "y by x, conditioned by panel",
    layout = "automatic", panel_order = "defined",
    type = "trellis_scatterplot"
  )
  path <- tempfile(fileext = ".svg")
  rlispstat:::.rls_export_trellis_scatterplot_fallback(record, path, "svg", 7, 5, "classic", 96)
  expect_true(file.exists(path))
  expect_gt(file.info(path)$size, 0)
})

test_that("trellis metadata follows the current specification without stale axis names", {
  record <- list(
    x = "mpg", y = "wt", condition = "cyl", plot_type = "scatter",
    conditions = list(list(variable = "cyl", kind = "categorical")),
    custom_title = FALSE
  )
  expect_identical(
    rlispstat:::.rls_trellis_derived_title(record),
    "wt by mpg, conditioned by cyl"
  )
  record$x <- "qsec"
  expect_identical(
    rlispstat:::.rls_trellis_derived_title(record),
    "wt by qsec, conditioned by cyl"
  )
  expect_false(grepl("mpg", rlispstat:::.rls_trellis_derived_title(record), fixed = TRUE))
  record$x <- "mpg"
  record$conditions <- c(record$conditions, list(list(variable = "am", kind = "categorical")))
  expect_identical(
    rlispstat:::.rls_trellis_derived_title(record),
    "wt by mpg, conditioned by cyl and am"
  )
})

test_that("fallback export supports multiple conditions and all trellis panel types", {
  data <- mtcars
  data$cyl <- factor(data$cyl)
  data$am <- factor(data$am)
  base <- list(
    data = data, x = "mpg", y = "wt", condition = "cyl",
    conditions = list(list(variable = "cyl", kind = "categorical"),
                      list(variable = "am", kind = "categorical")),
    layout = "automatic", panel_order = "defined", custom_title = FALSE,
    plot_type = "scatter", title = NULL
  )
  records <- list(
    scatter = base,
    histogram = within(base, { plot_type <- "histogram"; histogram_bins <- 5L }),
    boxplot = within(base, { plot_type <- "boxplot"; x <- "am"; y <- "mpg" }),
    bar = within(base, { plot_type <- "bar"; x <- "cyl"; y <- "" })
  )
  for (name in names(records)) {
    path <- tempfile(fileext = ".svg")
    rlispstat:::.rls_export_trellis_scatterplot_fallback(
      records[[name]], path, "svg", 8, 5, "classic", 96
    )
    expect_true(file.exists(path), info = name)
    expect_true(file.info(path)$size > 1000, info = name)
  }
})

test_that("panel analysis fits its linear model in R on panel rows", {
  source_group <- ls_register_dataset("trellis_panel_source", mtcars)
  before <- ls_datasets()$name
  on.exit({
    for (group in setdiff(ls_datasets()$name, before)) ls_unregister_dataset(group)
    ls_unregister_dataset(source_group)
  }, add = TRUE)
  sent <- list()
  testthat::local_mocked_bindings(
    .rls_send = function(lines, expect_reply = TRUE) {
      sent[[length(sent) + 1L]] <<- lines
      "OK"
    },
    .package = "rlispstat"
  )
  rows <- which(mtcars$cyl == 4)
  parts <- c(
    "TRELLIS_PANEL_ANALYSIS_NEEDED", "request1", "plot1", source_group,
    "panel4", "cyl = 4", "scatter", "wt", "mpg", "",
    as.character(length(rows)), as.character(rows)
  )

  rlispstat:::.rls_handle_trellis_panel_analysis_needed(parts)

  derived <- setdiff(ls_datasets()$name, before)
  expect_length(derived, 1L)
  model_id <- rlispstat:::.rls_glm_model_id(derived)
  model <- get(model_id, envir = rlispstat:::.rls_state$glm_models)
  expect_s3_class(model$fit, "lm")
  expect_equal(model$rows_used, seq_along(rows))
  expect_true(any(vapply(sent, function(command) identical(command[[1L]], "MODEL_OPEN"), logical(1L))))
})
