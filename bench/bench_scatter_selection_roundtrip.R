# End-to-end R/native selection benchmark for an ordinary scatterplot.
# Run with: Rscript --vanilla bench/bench_scatter_selection_roundtrip.R
# This measures command/selection round trips; mouse-drag frame timing still
# requires an interactive native session.
suppressPackageStartupMessages(library(LinkEDA))

sizes <- c(10000L, 100000L)
iterations <- 5L
set.seed(20260927)
results <- lapply(sizes, function(n) {
  data <- data.frame(x = rnorm(n), y = rnorm(n))
  group <- paste0("bench-selection-", n)
  on.exit(ls_close_all(), add = TRUE)
  open_seconds <- unname(system.time(ls_scatter(data, "x", "y", group = group))[["elapsed"]])
  sample_size <- min(1000L, n)
  updates <- numeric(iterations)
  for (i in seq_len(iterations)) {
    rows <- sort(sample.int(n, sample_size))
    updates[[i]] <- unname(system.time(ls_set_selected(group, rows))[["elapsed"]])
    stopifnot(identical(ls_selected(group), rows))
  }
  clear_seconds <- unname(system.time(ls_clear_selection(group))[["elapsed"]])
  stopifnot(length(ls_selected(group)) == 0L)
  ls_close_all()
  data.frame(cases = n, selected = sample_size,
             open_seconds = open_seconds,
             selection_median_seconds = median(updates),
             selection_max_seconds = max(updates),
             clear_seconds = clear_seconds)
})
print(do.call(rbind, results), row.names = FALSE)
