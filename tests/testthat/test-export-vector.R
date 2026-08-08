test_that("main-R SVG export task creates a genuine vector document", {
  id <- paste0("svg_task_", sample.int(1e6, 1L))
  data <- data.frame(x = 1:4, y = c(2, 5, 3, 8))
  rlispstat:::.rls_register_plot(
    id, "svg_task_group", "x", "y", data, seq_len(nrow(data)),
    type = "scatter", title = "A & B − β"
  )
  path <- tempfile(fileext = ".svg")
  replies <- list()
  testthat::local_mocked_bindings(
    .rls_send = function(lines) {
      replies[[length(replies) + 1L]] <<- lines
      "OK"
    },
    .package = "rlispstat"
  )

  rlispstat:::.rls_handle_plot_export_needed(c(
    "PLOT_EXPORT_NEEDED", "request-1", id, path, "svg", "save", "7", "5"
  ))

  expect_true(file.exists(path))
  svg <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(svg, "<svg", fixed = TRUE)
  expect_match(svg, "viewBox=", fixed = TRUE)
  expect_false(grepl("data:image/png", svg, fixed = TRUE))
  expect_true(grepl("<(path|circle|use|line|rect)", svg, perl = TRUE))
  expect_length(replies, 1L)
  expect_identical(replies[[1L]][1:3], c("PLOT_EXPORT_RESULT", "request-1", "ok"))
})

test_that("main-R SVG export task reports invalid dimensions safely", {
  replies <- list()
  testthat::local_mocked_bindings(
    .rls_send = function(lines) {
      replies[[length(replies) + 1L]] <<- lines
      "OK"
    },
    .package = "rlispstat"
  )
  rlispstat:::.rls_handle_plot_export_needed(c(
    "PLOT_EXPORT_NEEDED", "request-2", "missing", tempfile(fileext = ".svg"),
    "svg", "save", "0", "5"
  ))
  expect_identical(replies[[1L]][3L], "error")
  expect_match(replies[[1L]][6L], "dimensions", ignore.case = TRUE)
})

test_that("native-only plot metadata reconstructs matrix and parallel SVG exports", {
  data <- data.frame(a = 1:5, b = c(2, 5, 3, 8, 4), c = c(9, 7, 8, 3, 2))
  group <- paste0("svg_metadata_", sample.int(1e6, 1L))
  rlispstat:::.rls_register_dataset(group, data, activate = FALSE)
  replies <- list()
  testthat::local_mocked_bindings(
    .rls_send = function(lines) { replies[[length(replies) + 1L]] <<- lines; "OK" },
    .package = "rlispstat"
  )

  matrix_path <- tempfile(fileext = ".svg")
  rlispstat:::.rls_handle_plot_export_needed(c(
    "PLOT_EXPORT_NEEDED", "matrix-request", "native-matrix", matrix_path,
    "svg", "save", "7", "7", group, "scatter_matrix", "", "",
    "Matrix − β", "3", "variable", "a", "variable", "b", "variable", "c"
  ))
  expect_true(file.exists(matrix_path))
  expect_false(any(grepl("data:image/png", readLines(matrix_path, warn = FALSE), fixed = TRUE)))

  box_path <- tempfile(fileext = ".svg")
  rlispstat:::.rls_handle_plot_export_needed(c(
    "PLOT_EXPORT_NEEDED", "box-request", "native-parallel", box_path,
    "svg", "save", "7", "5", group, "boxplot", "", "a", "Parallel coordinates",
    "5", "variable", "a", "variable", "b", "variable", "c",
    "standardize", "TRUE", "connect_rows", "TRUE"
  ))
  expect_true(file.exists(box_path))
  expect_identical(replies[[length(replies)]][3L], "ok")
})

test_that("native component geometry remains vectorial and keeps categorical ticks", {
  data <- data.frame(seed = 1:4)
  group <- paste0("svg_component_", sample.int(1e6, 1L))
  rlispstat:::.rls_register_dataset(group, data, activate = FALSE)
  replies <- list()
  testthat::local_mocked_bindings(
    .rls_send = function(lines) { replies[[length(replies) + 1L]] <<- lines; "OK" },
    .package = "rlispstat"
  )
  path <- tempfile(fileext = ".svg")
  separator <- "\037"
  rlispstat:::.rls_handle_plot_export_needed(c(
    "PLOT_EXPORT_NEEDED", "scree-request", "native-scree", path,
    "svg", "save", "7", "5", group, "pca_scree", "Component", "Eigenvalue",
    "Parallel analysis − β", "6",
    "point", paste(c(1, 2.8, 1), collapse = separator),
    "point", paste(c(2, 1.4, 2), collapse = separator),
    "point", paste(c(3, 0.7, 3), collapse = separator),
    "parallel_point", paste(c(1, 1.6), collapse = separator),
    "parallel_point", paste(c(3, 0.6), collapse = separator),
    "x_tick", paste(c(1, "PC one"), collapse = separator)
  ))
  expect_true(file.exists(path))
  record <- get("native-scree", envir = rlispstat:::.rls_state$plots)
  expect_equal(nrow(record$render_points), 3L)
  expect_equal(nrow(record$parallel_points), 2L)
  expect_identical(record$x_ticks$label, "PC one")
  svg <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(svg, "Parallel analysis", fixed = TRUE)
  expect_match(svg, "PC one", fixed = TRUE)
  expect_false(grepl("data:image/png", svg, fixed = TRUE))
  expect_identical(replies[[length(replies)]][3L], "ok")
})

test_that("trellis time-series SVG uses panels and ordinary numeric date labels", {
  id <- paste0("trellis_time_", sample.int(1e6, 1L))
  data <- data.frame(
    year = rep(2018:2022, 2), value = c(2, 3, 5, 4, 7, 8, 7, 9, 10, 12),
    region = rep(c("North", "South"), each = 5)
  )
  rlispstat:::.rls_register_plot(
    id, id, "year", "value", data, seq_len(nrow(data)),
    type = "trellis_scatterplot", title = "Series by region",
    condition = "region", conditions = list(list(variable = "region", kind = "categorical")),
    plot_type = "time_series", layout = "one_row", scale_mode = "common_xy",
    custom_title = TRUE
  )
  path <- tempfile(fileext = ".svg")
  rlispstat::ls_export_plot(id, path, format = "svg", width = 8, height = 4)
  svg <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(svg, "North", fixed = TRUE)
  expect_match(svg, "South", fixed = TRUE)
  expect_false(grepl("2e\\+03", svg, ignore.case = TRUE))
  expect_false(grepl("data:image/png", svg, fixed = TRUE))
})

test_that("current analysis layers and the documented selection policy reach SVG", {
  id <- paste0("svg_layers_", sample.int(1e6, 1L))
  data <- data.frame(x = 1:12, y = c(2, 2.5, 4, 4.2, 5.8, 7, 7.2, 8.7, 9, 11, 11.4, 13))
  rlispstat:::.rls_register_plot(id, id, "x", "y", data, seq_len(nrow(data)),
                                type = "scatter", title = "Layers")
  path <- tempfile(fileext = ".svg")
  replies <- list()
  testthat::local_mocked_bindings(
    .rls_send = function(lines) { replies[[length(replies) + 1L]] <<- lines; "OK" },
    .package = "rlispstat"
  )
  rlispstat:::.rls_handle_plot_export_needed(c(
    "PLOT_EXPORT_NEEDED", "layer-request", id, path, "svg", "save", "7", "5",
    id, "scatter", "x", "y", "Layers", "4",
    "lm", "TRUE", "smooth", "TRUE", "smooth_span", "0.6", "selection", "excluded"
  ))
  record <- get(id, envir = rlispstat:::.rls_state$plots)
  expect_true(record$lm)
  expect_true(record$smooth)
  expect_equal(record$smooth_span, 0.6)
  expect_identical(record$selection_export_policy, "excluded")
  svg <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(svg, "<(path|polyline)", perl = TRUE)
  expect_false(grepl("data:image/png", svg, fixed = TRUE))
  if (requireNamespace("xml2", quietly = TRUE)) {
    expect_s3_class(xml2::read_xml(path), "xml_document")
  }
})

test_that("histogram density stays vectorial and PDF/PNG remain available", {
  id <- paste0("export_formats_", sample.int(1e6, 1L))
  data <- data.frame(value = seq(-2, 2, length.out = 80)^3 + rep(c(-0.1, 0.1), 40))
  rlispstat:::.rls_register_plot(
    id, id, "value", "", data, seq_len(nrow(data)), type = "histogram",
    title = "Density", histogram_bins = 12L, show_density = TRUE,
    density_mode = "all", density_adjust = 1
  )
  svg_path <- tempfile(fileext = ".svg")
  pdf_path <- tempfile(fileext = ".pdf")
  png_path <- tempfile(fileext = ".png")
  old_started <- rlispstat:::.rls_state$process_started
  on.exit(assign("process_started", old_started, envir = rlispstat:::.rls_state), add = TRUE)
  assign("process_started", FALSE, envir = rlispstat:::.rls_state)
  rlispstat::ls_export_plot(id, svg_path, format = "svg")
  rlispstat::ls_export_plot(id, pdf_path, format = "pdf")
  rlispstat::ls_export_plot(id, png_path, format = "png")
  svg <- paste(readLines(svg_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(svg, "<(path|polyline)", perl = TRUE)
  expect_identical(readChar(pdf_path, nchars = 5L, useBytes = TRUE), "%PDF-")
  expect_identical(readBin(png_path, "raw", n = 8L),
                   as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))
})

test_that("visual copy defaults are platform-aware", {
  seen <- NULL
  testthat::local_mocked_bindings(
    .rls_copy_plot_native = function(plot, format) { seen <<- format; TRUE },
    .package = "rlispstat"
  )
  rlispstat::ls_copy_plot("plot-id")
  expect_identical(seen, if (.Platform$OS.type == "windows") "emf" else "pdf")
  if (.Platform$OS.type != "windows") {
    expect_error(rlispstat::ls_copy_plot("plot-id", "emf"), "only on Windows")
  }
})
