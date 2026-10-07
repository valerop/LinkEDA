test_that("Windows time-series lines and legend use the active input geometry", {
  root <- linkeda_source_test_root()
  source <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "ScatterPlotView.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  render_start <- regexpr("void ScatterPlotView::RenderTimeSeries", source, fixed = TRUE)
  expect_gt(render_start, 0L)
  render_tail <- substr(source, render_start, nchar(source))
  render_end <- regexpr("void ScatterPlotView::RenderTrellis", render_tail, fixed = TRUE)
  expect_gt(render_end, 0L)
  render <- substr(render_tail, 1L, render_end - 1L)

  expect_match(render, "renderedViewport_ = viewport", fixed = TRUE)
  expect_match(render, "renderedPlotRect_ = plotRect", fixed = TRUE)
  expect_match(
    render,
    "pointsCanvas_.Children().Append(dragSurface)",
    fixed = TRUE
  )
  expect_false(grepl(
    "plotCanvas_.Children().Append(dragSurface)",
    render,
    fixed = TRUE
  ))
  expect_match(render, "PlotSeriesLegendRows", fixed = TRUE)
  expect_match(render, "selectionCallback_", fixed = TRUE)
})

test_that("Welcome to LinkEDA is exposed from Help rather than File", {
  root <- linkeda_source_test_root()
  source <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "ApplicationMenu.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  welcome <- '{ApplicationCommandId::ShowWelcome, L"Welcome to LinkEDA", true, {}}'
  expect_length(gregexpr(welcome, source, fixed = TRUE)[[1L]], 1L)
  help <- regexpr('{L"Help", {', source, fixed = TRUE)
  expect_gt(help, 0L)
  help_block <- substr(source, help, help + 400L)
  expect_match(help_block, welcome, fixed = TRUE)
})
