#' Prepare linked histogram data
#'
#' @param data A data frame.
#' @param x Numeric histogram variable.
#' @param group Linked brushing group.
#' @param bins Number of bins.
#' @param binwidth Bin width.
#' @param breaks Explicit break vector.
#' @param rule Automatic binning rule.
#' @return A list used by the native backend.
.rls_prepare_histogram_data <- function(data, x, group = NULL, bins = NULL,
                                        binwidth = NULL, breaks = NULL,
                                        rule = c("sturges", "fd", "scott", "sqrt")) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  x <- .rls_validate_protocol_name(x, "x")
  if (!x %in% names(data)) {
    stop(sprintf("Column `%s` was not found in `data`.", x), call. = FALSE)
  }
  if (!is.numeric(data[[x]])) {
    stop("`x` must name a numeric column.", call. = FALSE)
  }
  group <- .rls_validate_group(group, allow_null = TRUE)
  rule <- match.arg(rule)

  ok <- is.finite(data[[x]]) & !is.na(data[[x]])
  values <- unname(as.double(data[[x]][ok]))
  rows <- which(ok)
  if (!length(values)) {
    stop("`x` has no finite values to plot.", call. = FALSE)
  }

  breaks <- .rls_histogram_breaks_for_values(values, bins = bins, binwidth = binwidth,
                                             breaks = breaks, rule = rule)
  bin <- findInterval(values, breaks, rightmost.closed = TRUE, all.inside = TRUE)
  bin <- pmax(1L, pmin(length(breaks) - 1L, bin))

  list(
    x = values,
    row = rows,
    bin = bin,
    breaks = breaks,
    x_name = x,
    group = group,
    n_total = nrow(data),
    rule = rule
  )
}

.rls_histogram_breaks_for_values <- function(values, bins = NULL, binwidth = NULL,
                                             breaks = NULL,
                                             rule = c("sturges", "fd", "scott", "sqrt")) {
  rule <- match.arg(rule)
  if (!is.null(breaks)) {
    if (!is.numeric(breaks) || length(breaks) < 2L || anyNA(breaks) || any(!is.finite(breaks))) {
      stop("`breaks` must be a finite numeric vector with at least two values.", call. = FALSE)
    }
    breaks <- sort(unique(as.double(breaks)))
    if (length(breaks) < 2L) {
      stop("`breaks` must contain at least two distinct values.", call. = FALSE)
    }
    return(breaks)
  }
  rng <- range(values, finite = TRUE)
  if (rng[[1L]] == rng[[2L]]) {
    rng <- rng + c(-0.5, 0.5)
  }
  width <- diff(rng)
  if (!is.null(binwidth)) {
    if (!is.numeric(binwidth) || length(binwidth) != 1L || is.na(binwidth) || binwidth <= 0) {
      stop("`binwidth` must be a single positive number.", call. = FALSE)
    }
    bins <- max(1L, ceiling(width / binwidth))
  } else if (is.null(bins)) {
    n <- length(values)
    bins <- switch(rule,
      sturges = ceiling(log2(n) + 1),
      sqrt = ceiling(sqrt(n)),
      fd = {
        h <- 2 * stats::IQR(values, na.rm = TRUE) / n^(1 / 3)
        if (is.finite(h) && h > 0) ceiling(width / h) else ceiling(log2(n) + 1)
      },
      scott = {
        h <- 3.5 * stats::sd(values, na.rm = TRUE) / n^(1 / 3)
        if (is.finite(h) && h > 0) ceiling(width / h) else ceiling(log2(n) + 1)
      }
    )
  }
  if (!is.numeric(bins) || length(bins) != 1L || is.na(bins) || bins < 1) {
    stop("`bins` must be a single positive number.", call. = FALSE)
  }
  bins <- max(1L, as.integer(round(bins)))
  seq(rng[[1L]], rng[[2L]], length.out = bins + 1L)
}

.rls_histogram_density_modes <- c("none", "all", "selected", "colors", "selected_and_colors")

.rls_validate_histogram_density_mode <- function(mode) {
  mode <- match.arg(mode, .rls_histogram_density_modes)
  mode
}

#' Open a native linked histogram
#'
#' @param data A data frame.
#' @param x Numeric histogram variable.
#' @param group Optional linked brushing group.
#' @param bins Number of bins.
#' @param binwidth Bin width.
#' @param breaks Explicit break vector.
#' @param title Optional plot title.
#' @param linked Logical; if `TRUE`, use the supplied group for linked brushing.
#' @param show_counts,show_points Initial display options.
#' @return An `rlispstat_plot` object with type `"histogram"`.
#' @export
ls_histogram <- function(data, x, group = NULL, bins = NULL, binwidth = NULL,
                         breaks = NULL, title = NULL, linked = TRUE,
                         show_counts = TRUE, show_points = FALSE) {
  prepared <- .rls_prepare_histogram_data(data, x, group, bins = bins,
                                          binwidth = binwidth, breaks = breaks)
  plot_id <- paste0("histogram_", format(Sys.time(), "%Y%m%d%H%M%OS3"), "_", sample.int(1e6, 1L))
  plot_group <- if (isTRUE(linked)) prepared$group %||% plot_id else plot_id
  if (is.null(title)) {
    title <- paste("Histogram of", prepared$x_name)
  }
  title <- .rls_validate_protocol_name(title, "title")

  .rls_start_backend()
  n <- length(prepared$x)
  bin_count <- length(prepared$breaks) - 1L
  lines <- c(
    "ADD_HISTOGRAM",
    plot_id,
    plot_group,
    prepared$x_name,
    title,
    if (isTRUE(show_counts)) "TRUE" else "FALSE",
    if (isTRUE(show_points)) "TRUE" else "FALSE",
    as.character(bin_count),
    as.character(n)
  )
  for (i in seq_len(bin_count)) {
    lines <- c(lines, sprintf("%.17g\t%.17g", prepared$breaks[[i]], prepared$breaks[[i + 1L]]))
  }
  if (n) {
    lines <- c(lines, sprintf("%.17g\t%d\t%d", prepared$x, prepared$row, prepared$bin))
  }
  lines <- c(lines, .rls_variable_payload(data), .rls_dataframe_payload(data))
  .rls_send(lines)
  .rls_register_plot(plot_id, plot_group, prepared$x_name, "", data, prepared$row,
                     type = "histogram", title = title,
                     breaks = prepared$breaks,
                     show_counts = isTRUE(show_counts),
                     show_points = isTRUE(show_points))

  structure(
    list(id = plot_id, group = plot_group, x = prepared$x_name, y = "",
         title = title, type = "histogram", breaks = prepared$breaks),
    class = "rlispstat_plot"
  )
}

#' @rdname ls_histogram
#' @export
ls_new_histogram <- function(group = NULL, x = NULL, bins = NULL,
                             binwidth = NULL, breaks = NULL, title = NULL,
                             linked = TRUE, show_counts = TRUE,
                             show_points = FALSE) {
  record <- .rls_dataset_record(group)
  data <- record$data
  numeric <- .rls_numeric_variable_names(data)
  if (!length(numeric) && is.null(x)) {
    stop("The dataset must contain at least one numeric variable.", call. = FALSE)
  }
  x <- x %||% numeric[[1L]]
  if (is.null(title) && identical(record$dataset_type %||% "data_frame", "multiple_imputation")) {
    title <- paste("Histogram of", x, .rls_mi_title_suffix(record))
  }
  .rls_mi_warn_current_version(record, "Histogram")
  ls_histogram(data, x = x, group = record$group, bins = bins, binwidth = binwidth,
               breaks = breaks, title = title, linked = linked,
               show_counts = show_counts, show_points = show_points)
}

#' Configure a histogram bin count
#'
#' @param plot An `rlispstat_plot` object or plot id.
#' @param bins Optional new bin count. Getter support is native-backend dependent.
#' @return Invisibly returns `TRUE` for setters.
#' @export
ls_histogram_bins <- function(plot, bins = NULL) {
  if (is.null(bins)) {
    stop("Histogram bin details are only available for registered histogram objects.", call. = FALSE)
  }
  .rls_send(c("HIST_SET_BINS", .rls_plot_id(plot), as.character(as.integer(bins))))
  invisible(TRUE)
}

#' @rdname ls_histogram_bins
#' @export
ls_histogram_rule <- function(plot, rule = c("sturges", "fd", "scott", "sqrt")) {
  rule <- match.arg(rule)
  .rls_send(c("HIST_SET_BINNING_RULE", .rls_plot_id(plot), rule))
  invisible(TRUE)
}

#' @rdname ls_histogram_bins
#' @export
ls_histogram_breaks <- function(plot) {
  reply <- .rls_send(c("HIST_BREAKS", .rls_plot_id(plot)))
  parts <- strsplit(sub("^OK\\s*", "", reply), "\t", fixed = FALSE)[[1L]]
  as.numeric(parts[nzchar(parts)])
}

#' Configure histogram density curves
#'
#' @param plot An `rlispstat_plot` object or plot id.
#' @param show Logical. Show or hide density curves.
#' @param mode Density display mode.
#' @param bw Numeric bandwidth. Use `NULL` for automatic bandwidth.
#' @param adjust Positive bandwidth adjustment multiplier.
#' @return Setters invisibly return `TRUE`; `ls_histogram_density_info()` returns
#'   a named list.
#' @export
ls_histogram_show_density <- function(plot, show = TRUE) {
  if (!is.logical(show) || length(show) != 1L || is.na(show)) {
    stop("`show` must be TRUE or FALSE.", call. = FALSE)
  }
  .rls_send(c("HIST_SHOW_DENSITY", .rls_plot_id(plot), if (show) "TRUE" else "FALSE"))
  invisible(TRUE)
}

#' @rdname ls_histogram_show_density
#' @export
ls_histogram_density_mode <- function(plot, mode = c("none", "all", "selected", "colors", "selected_and_colors")) {
  mode <- .rls_validate_histogram_density_mode(mode)
  .rls_send(c("HIST_SET_DENSITY_MODE", .rls_plot_id(plot), mode))
  invisible(TRUE)
}

#' @rdname ls_histogram_show_density
#' @export
ls_histogram_density_bw <- function(plot, bw = NULL) {
  if (is.null(bw)) {
    bw <- 0
  }
  if (!is.numeric(bw) || length(bw) != 1L || is.na(bw) || bw < 0) {
    stop("`bw` must be NULL or a single non-negative number.", call. = FALSE)
  }
  .rls_send(c("HIST_SET_DENSITY_BW", .rls_plot_id(plot), sprintf("%.17g", bw)))
  invisible(TRUE)
}

#' @rdname ls_histogram_show_density
#' @export
ls_histogram_density_adjust <- function(plot, adjust = 1) {
  if (!is.numeric(adjust) || length(adjust) != 1L || is.na(adjust) || adjust <= 0) {
    stop("`adjust` must be a single positive number.", call. = FALSE)
  }
  .rls_send(c("HIST_SET_DENSITY_ADJUST", .rls_plot_id(plot), sprintf("%.17g", adjust)))
  invisible(TRUE)
}

#' @rdname ls_histogram_show_density
#' @export
ls_histogram_density_info <- function(plot) {
  reply <- .rls_send(c("HIST_DENSITY_INFO", .rls_plot_id(plot)))
  payload <- sub("^OK\\s*", "", reply)
  parts <- strsplit(payload, "\\|")[[1L]]
  list(
    show = identical(parts[[1L]], "TRUE"),
    mode = parts[[2L]],
    bw = as.numeric(parts[[3L]]),
    adjust = as.numeric(parts[[4L]])
  )
}
