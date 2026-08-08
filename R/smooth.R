#' Add a smooth curve (LOESS) overlay to a scatterplot
#'
#' Computes a LOESS smooth curve via `stats::loess()` and sends the result to
#' the native scatterplot window. The curve appears or disappears when toggled
#' from the plot's context menu.
#'
#' @param plot An `rlispstat_plot` object or plot id.
#' @param scope Which data to use: `"overall"` (all visible points),
#'   `"selected"` (only selected points), or `"color"` (one curve per point
#'   color group).
#' @param span Smoothing parameter passed to `stats::loess()`. Default 0.75.
#' @param evaluation_points Number of points along the x-axis at which to
#'   evaluate the smooth. Default 200.
#' @return Invisibly returns `TRUE`.
#' @export
#' @examples
#' \dontrun{
#' p <- ls_scatter(mtcars, "wt", "mpg", group = "cars")
#' ls_add_smooth(p, "overall")
#' ls_add_smooth(p, "selected")
#' }
ls_add_smooth <- function(plot, scope = c("overall", "selected", "color"),
                           span = 0.75, evaluation_points = 200) {
  scope <- match.arg(scope)
  plot_id <- .rls_plot_id(plot)
  plot_rec <- .rls_get_plot(plot_id)
  if (is.null(plot_rec)) {
    stop("Plot not found.", call. = FALSE)
  }
  data <- plot_rec$data
  x_col <- plot_rec$x
  y_col <- plot_rec$y
  rows <- plot_rec$rows
  if (!length(rows)) {
    stop("The scatterplot has no visible points.", call. = FALSE)
  }

  x <- as.double(data[[x_col]][rows])
  y <- as.double(data[[y_col]][rows])

  if (scope == "selected") {
    selected_rows <- ls_selected(plot_rec$group)
    idx <- which(rows %in% selected_rows)
    if (!length(idx)) {
      message("No points selected for smooth curve.")
      .rls_send(c("ADD_SMOOTH", plot_id, scope, "0"))
      return(invisible(TRUE))
    }
    curves <- list(.rls_compute_smooth(x[idx], y[idx], ".", span, evaluation_points))
  } else if (scope == "color") {
    reply <- .rls_send(c("POINT_COLORS", plot_id))
    if (identical(reply, "OK")) {
      message("No assigned point colors for smooth curves.")
      .rls_send(c("ADD_SMOOTH", plot_id, scope, "0"))
      return(invisible(TRUE))
    }
    records <- .rls_parse_records(reply)
    row_to_color <- list()
    for (rec in records) {
      parts <- strsplit(rec, "|", fixed = TRUE)[[1L]]
      if (length(parts) >= 2L) {
        row_to_color[[parts[1L]]] <- parts[2L]
      }
    }
    color_groups <- split(seq_along(rows), vapply(seq_along(rows), function(i) {
      row_to_color[[as.character(rows[i])]] %||% "black"
    }, character(1L)))
    curves <- list()
    for (color_name in names(color_groups)) {
      idx <- color_groups[[color_name]]
      if (length(idx) < 3L) next
      cv <- .rls_compute_smooth(x[idx], y[idx], color_name, span, evaluation_points)
      if (cv$ok) curves <- c(curves, list(cv))
    }
    if (!length(curves)) {
      message("Insufficient data for any color group smooth curve.")
      .rls_send(c("ADD_SMOOTH", plot_id, scope, "0"))
      return(invisible(TRUE))
    }
  } else {
    curves <- list(.rls_compute_smooth(x, y, ".", span, evaluation_points))
  }

  .rls_send_smooth(plot_id, scope, curves)
  invisible(TRUE)
}

.rls_compute_smooth <- function(x, y, group_id, span, evaluation_points) {
  ok <- FALSE
  x_out <- numeric()
  y_out <- numeric()
  message <- ""

  x <- as.double(x)
  y <- as.double(y)
  finite <- is.finite(x) & is.finite(y)
  x <- x[finite]
  y <- y[finite]

  if (length(x) < 3L || length(unique(x)) < 2L) {
    return(list(ok = FALSE, group = group_id,
                x = x_out, y = y_out, message = "too few valid points"))
  }

  result <- tryCatch(
    {
      x_order <- order(x)
      xs <- x[x_order]
      ys <- y[x_order]
      lo <- stats::loess(ys ~ xs, span = span, degree = 1L,
                         na.action = stats::na.exclude,
                         control = stats::loess.control(surface = "direct"))
      x_grid <- seq(min(xs, na.rm = TRUE), max(xs, na.rm = TRUE),
                    length.out = evaluation_points)
      pred <- stats::predict(lo, newdata = data.frame(xs = x_grid))
      ok <- all(is.finite(pred))
      list(ok = ok, group = group_id,
           x = x_grid, y = as.double(pred),
           message = if (ok) "" else "non-finite predictions")
    },
    error = function(e) {
      list(ok = FALSE, group = group_id,
           x = numeric(), y = numeric(),
           message = conditionMessage(e))
    },
    warning = function(w) {
      x_order <- order(x)
      xs <- x[x_order]
      ys <- y[x_order]
      lo <- suppressWarnings(stats::loess(ys ~ xs, span = span, degree = 1L,
                                          na.action = stats::na.exclude,
                                          control = stats::loess.control(surface = "direct")))
      x_grid <- seq(min(xs, na.rm = TRUE), max(xs, na.rm = TRUE),
                    length.out = evaluation_points)
      pred <- tryCatch(stats::predict(lo, newdata = data.frame(xs = x_grid)),
                       error = function(e2) rep(NA_real_, length(x_grid)))
      ok <- all(is.finite(pred))
      list(ok = ok, group = group_id,
           x = x_grid, y = as.double(pred),
           message = conditionMessage(w))
    }
  )

  result
}

.rls_send_smooth <- function(plot_id, scope, curves) {
  lines <- c("ADD_SMOOTH", plot_id, scope, as.character(length(curves)))
  for (cv in curves) {
    n <- length(cv$x)
    lines <- c(lines, cv$group, if (isTRUE(cv$ok)) "1" else "0", as.character(n))
    if (n > 0) {
      lines <- c(
        lines,
        paste(sprintf("%.17g", cv$x), collapse = " "),
        paste(sprintf("%.17g", cv$y), collapse = " ")
      )
    } else {
      lines <- c(lines, "", "")
    }
  }
  .rls_send(lines)
}

.rls_get_plot <- function(plot_id) {
  if (exists(plot_id, envir = .rls_state$plots, inherits = FALSE)) {
    get(plot_id, envir = .rls_state$plots)
  } else {
    NULL
  }
}
