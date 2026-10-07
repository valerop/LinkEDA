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
  if (identical(plot_rec$type, "trellis_scatterplot")) {
    # Trellis smooths are fitted independently inside every panel.  The
    # native application owns the current panel definitions (including any
    # interactive conditioning changes), so ask it to issue one R task per
    # panel instead of fitting one curve across the complete dataset here.
    .rls_send(c("REQUEST_SMOOTH", plot_id, scope))
    return(invisible(TRUE))
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
    reply <- .rls_send(c("POINT_COLORS", plot_rec$group))
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
    color_groups <- split(seq_along(rows), .rls_colors_for_rows(rows, row_to_color))
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

# Resolve the sparse row -> colour maps used by linked brushing without using
# `[[` on a missing name.  The native application only transmits explicitly
# assigned colours, so every absent row must remain in the default group.
.rls_colors_for_rows <- function(rows, row_colors, default = "black") {
  resolved <- rep.int(as.character(default)[1L], length(rows))
  if (!length(rows) || !length(row_colors)) return(resolved)

  color_names <- names(row_colors)
  if (is.null(color_names)) return(resolved)

  matches <- match(as.character(rows), color_names)
  assigned <- which(!is.na(matches))
  if (!length(assigned)) return(resolved)

  values <- as.character(row_colors[matches[assigned]])
  valid <- !is.na(values) & nzchar(values)
  resolved[assigned[valid]] <- values[valid]
  resolved
}

.rls_compute_smooth <- function(x, y, group_id, span, evaluation_points,
                                confidence_level = 0.95,
                                fit_method = c("loess", "lm")) {
  fit_method <- match.arg(fit_method)
  ok <- FALSE
  x_out <- numeric()
  y_out <- numeric()
  message <- ""

  x <- as.double(x)
  y <- as.double(y)
  finite <- is.finite(x) & is.finite(y)
  x <- x[finite]
  y <- y[finite]

  minimum_points <- if (identical(fit_method, "lm")) 2L else 3L
  if (length(x) < minimum_points || length(unique(x)) < 2L) {
    return(list(ok = FALSE, group = group_id,
                x = x_out, y = y_out, lower = numeric(), upper = numeric(),
                message = "too few valid points"))
  }

  result <- tryCatch(
    {
      x_order <- order(x)
      xs <- x[x_order]
      ys <- y[x_order]
      x_grid <- seq(min(xs, na.rm = TRUE), max(xs, na.rm = TRUE),
                    length.out = evaluation_points)
      if (identical(fit_method, "lm")) {
        fit <- stats::lm(ys ~ xs)
        if (stats::df.residual(fit) > 0L) {
          pred <- stats::predict(
            fit, newdata = data.frame(xs = x_grid),
            interval = "confidence", level = confidence_level
          )
          estimate <- as.double(pred[, "fit"])
          lower <- as.double(pred[, "lwr"])
          upper <- as.double(pred[, "upr"])
        } else {
          estimate <- as.double(stats::predict(fit, newdata = data.frame(xs = x_grid)))
          lower <- upper <- numeric()
        }
      } else {
        fit <- stats::loess(ys ~ xs, span = span, degree = 1L,
                            na.action = stats::na.exclude,
                            control = stats::loess.control(surface = "direct"))
        pred <- stats::predict(fit, newdata = data.frame(xs = x_grid), se = TRUE)
        estimate <- as.double(pred$fit)
        critical <- stats::qt((1 + confidence_level) / 2, df = pred$df)
        lower <- estimate - critical * as.double(pred$se.fit)
        upper <- estimate + critical * as.double(pred$se.fit)
      }
      ok <- all(is.finite(estimate))
      list(ok = ok, group = group_id,
           x = x_grid, y = estimate, lower = lower, upper = upper,
           message = if (ok) "" else "non-finite predictions")
    },
    error = function(e) {
      list(ok = FALSE, group = group_id,
           x = numeric(), y = numeric(), lower = numeric(), upper = numeric(),
           message = conditionMessage(e))
    },
    warning = function(w) {
      x_order <- order(x)
      xs <- x[x_order]
      ys <- y[x_order]
      x_grid <- seq(min(xs, na.rm = TRUE), max(xs, na.rm = TRUE),
                    length.out = evaluation_points)
      predicted <- tryCatch({
        if (identical(fit_method, "lm")) {
          fit <- stats::lm(ys ~ xs)
          if (stats::df.residual(fit) > 0L) {
            pred <- stats::predict(fit, newdata = data.frame(xs = x_grid),
                                   interval = "confidence", level = confidence_level)
            list(y = as.double(pred[, "fit"]), lower = as.double(pred[, "lwr"]),
                 upper = as.double(pred[, "upr"]))
          } else {
            list(y = as.double(stats::predict(fit, newdata = data.frame(xs = x_grid))),
                 lower = numeric(), upper = numeric())
          }
        } else {
          fit <- suppressWarnings(stats::loess(
            ys ~ xs, span = span, degree = 1L, na.action = stats::na.exclude,
            control = stats::loess.control(surface = "direct")
          ))
          pred <- stats::predict(fit, newdata = data.frame(xs = x_grid), se = TRUE)
          critical <- stats::qt((1 + confidence_level) / 2, df = pred$df)
          list(y = as.double(pred$fit),
               lower = as.double(pred$fit - critical * pred$se.fit),
               upper = as.double(pred$fit + critical * pred$se.fit))
        }
      }, error = function(e2) list(
        y = rep(NA_real_, length(x_grid)), lower = numeric(), upper = numeric()
      ))
      ok <- all(is.finite(predicted$y))
      list(ok = ok, group = group_id,
           x = x_grid, y = predicted$y,
           lower = predicted$lower, upper = predicted$upper,
           message = conditionMessage(w))
    }
  )

  result
}

.rls_send_smooth <- function(plot_id, scope, curves, fit_method = "loess") {
  lines <- c("ADD_SMOOTH", plot_id, scope, "FIT_CURVE_V2", fit_method,
             as.character(length(curves)))
  for (cv in curves) {
    n <- length(cv$x)
    lines <- c(lines, cv$group, if (isTRUE(cv$ok)) "1" else "0", as.character(n))
    if (n > 0) {
      lines <- c(
        lines,
        paste(sprintf("%.17g", cv$x), collapse = " "),
        paste(sprintf("%.17g", cv$y), collapse = " "),
        paste(sprintf("%.17g", cv$lower %||% numeric()), collapse = " "),
        paste(sprintf("%.17g", cv$upper %||% numeric()), collapse = " ")
      )
    } else {
      lines <- c(lines, "", "", "", "")
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
