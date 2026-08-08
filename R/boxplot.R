#' Prepare linked boxplot data
#'
#' @param data A data frame.
#' @param y Numeric response variable.
#' @param x Optional grouping variable.
#' @param group Linked brushing group.
#' @return A list used by the native backend.
.rls_prepare_boxplot_data <- function(data, y, x = NULL, group = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  y <- .rls_validate_protocol_name(y, "y")
  if (!y %in% names(data)) {
    stop(sprintf("Column `%s` was not found in `data`.", y), call. = FALSE)
  }
  if (!is.numeric(data[[y]])) {
    stop("`y` must name a numeric column.", call. = FALSE)
  }
  if (!is.null(x)) {
    x <- .rls_validate_protocol_name(x, "x")
    if (!x %in% names(data)) {
      stop(sprintf("Column `%s` was not found in `data`.", x), call. = FALSE)
    }
  }
  group <- .rls_validate_group(group, allow_null = TRUE)
  ok <- !is.na(data[[y]]) & is.finite(data[[y]])
  rows <- which(ok)
  groups <- if (is.null(x)) {
    rep("All", length(rows))
  } else {
    values <- as.character(data[[x]][ok])
    values[is.na(values)] <- "NA"
    values
  }
  list(
    y = unname(as.double(data[[y]][ok])),
    category = groups,
    row = rows,
    y_name = y,
    x_name = x %||% "",
    group = group,
    n_total = nrow(data)
  )
}

.rls_quantile_type7 <- function(x, p) {
  stats::quantile(x, probs = p, names = FALSE, type = 7, na.rm = TRUE)
}

#' Compute Tukey boxplot summaries
#'
#' @param data A data frame.
#' @param y Numeric response variable.
#' @param x Optional grouping variable.
#' @return A data frame of summaries.
.rls_boxplot_summaries <- function(data, y, x = NULL) {
  prepared <- .rls_prepare_boxplot_data(data, y, x, group = "boxplot_summary")
  if (!length(prepared$y)) {
    return(data.frame(
      category = character(), n = integer(), q1 = numeric(), median = numeric(),
      q3 = numeric(), iqr = numeric(), lower = numeric(), upper = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  split_y <- split(prepared$y, prepared$category)
  do.call(rbind, lapply(names(split_y), function(category) {
    values <- sort(split_y[[category]])
    q1 <- .rls_quantile_type7(values, 0.25)
    med <- .rls_quantile_type7(values, 0.50)
    q3 <- .rls_quantile_type7(values, 0.75)
    iqr <- q3 - q1
    lower_limit <- q1 - 1.5 * iqr
    upper_limit <- q3 + 1.5 * iqr
    data.frame(
      category = category,
      n = length(values),
      q1 = q1,
      median = med,
      q3 = q3,
      iqr = iqr,
      lower = min(values[values >= lower_limit]),
      upper = max(values[values <= upper_limit]),
      stringsAsFactors = FALSE
    )
  }))
}

#' Open a native linked boxplot
#'
#' @param data A data frame.
#' @param y Numeric response variable.
#' @param x Optional grouping variable.
#' @param group Optional linked brushing group.
#' @param labels Optional label variable name or vector.
#' @param title Optional plot title.
#' @param linked Logical; if `TRUE`, use the supplied group for linked brushing.
#' @param show_points,show_box,show_whiskers Initial visibility options.
#' @param show_violin Initial visibility for the native violin density layer.
#' @param connect_rows Logical; for ungrouped boxplots with multiple variables,
#'   draw lines connecting matching rows.
#' @param standardize Logical; for ungrouped boxplots, display each variable as
#'   z-scores before computing boxes and connection lines.
#' @return An `rlispstat_plot` object with type `"boxplot"`.
#' @export
ls_boxplot <- function(data, y, x = NULL, group = NULL, labels = NULL,
                       title = NULL, linked = TRUE, show_points = TRUE,
                       show_box = TRUE, show_whiskers = TRUE,
                       show_violin = FALSE, connect_rows = FALSE,
                       standardize = FALSE) {
  prepared <- .rls_prepare_boxplot_data(data, y, x, group)
  if (isTRUE(connect_rows) && nzchar(prepared$x_name)) {
    stop("`connect_rows` is only available for ungrouped boxplots.", call. = FALSE)
  }
  if (isTRUE(standardize) && nzchar(prepared$x_name)) {
    stop("`standardize` is only available for ungrouped boxplots.", call. = FALSE)
  }
  plot_id <- paste0("boxplot_", format(Sys.time(), "%Y%m%d%H%M%OS3"), "_", sample.int(1e6, 1L))
  plot_group <- if (isTRUE(linked)) prepared$group %||% plot_id else plot_id
  if (is.character(labels) && length(labels) == 1L && labels %in% names(data)) {
    labels <- as.character(data[[labels]])
  }
  if (!is.null(labels) && length(labels) != nrow(data)) {
    stop("`labels` must be a column name or one label per row in `data`.", call. = FALSE)
  }
  if (is.null(title)) {
    title <- if (nzchar(prepared$x_name)) {
      paste(prepared$y_name, "by", prepared$x_name)
    } else {
      prepared$y_name
    }
  }
  title <- .rls_validate_protocol_name(title, "title")

  .rls_start_backend()
  n <- length(prepared$y)
  lines <- c(
    "ADD_BOXPLOT",
    plot_id,
    plot_group,
    prepared$x_name,
    prepared$y_name,
    title,
    if (isTRUE(show_points)) "TRUE" else "FALSE",
    if (isTRUE(show_box)) "TRUE" else "FALSE",
    if (isTRUE(show_whiskers)) "TRUE" else "FALSE",
    as.character(n)
  )
  if (n) {
    categories <- gsub("[\r\n\t|]", " ", prepared$category)
    lines <- c(lines, sprintf("%.17g\t%s\t%d", prepared$y, categories, prepared$row))
  }
  lines <- c(lines, .rls_variable_payload(data), .rls_dataframe_payload(data))
  .rls_send(lines)
  if (isTRUE(standardize)) {
    .rls_send(c("BOXPLOT_OPTION", plot_id, "standardize_variables", "TRUE"))
  }
  if (isTRUE(connect_rows)) {
    .rls_send(c("BOXPLOT_OPTION", plot_id, "connect_rows", "TRUE"))
  }
  if (isTRUE(show_violin)) {
    .rls_send(c("BOXPLOT_OPTION", plot_id, "violin", "TRUE"))
  }
  .rls_register_plot(plot_id, plot_group, prepared$x_name, prepared$y_name, data, prepared$row,
                     type = "boxplot", title = title,
                     show_points = isTRUE(show_points),
                     show_box = isTRUE(show_box),
                     show_whiskers = isTRUE(show_whiskers),
                     show_violin = isTRUE(show_violin),
                     connect_rows = isTRUE(connect_rows),
                     standardize = isTRUE(standardize),
                     boxplot_variables = if (nzchar(prepared$x_name)) character() else prepared$y_name)

  structure(
    list(id = plot_id, group = plot_group, x = prepared$x_name, y = prepared$y_name,
         title = title, type = "boxplot"),
    class = "rlispstat_plot"
  )
}

#' @rdname ls_boxplot
#' @export
ls_new_boxplot <- function(group = NULL, y = NULL, x = NULL, labels = NULL,
                           title = NULL, linked = TRUE, show_points = TRUE,
                           show_box = TRUE, show_whiskers = TRUE,
                           show_violin = FALSE, connect_rows = FALSE,
                           standardize = FALSE) {
  record <- .rls_dataset_record(group)
  data <- record$data
  numeric <- .rls_numeric_variable_names(data)
  if (!length(numeric) && is.null(y)) {
    stop("The dataset must contain at least one numeric variable.", call. = FALSE)
  }
  y <- y %||% numeric[[1L]]
  if (is.null(title) && identical(record$dataset_type %||% "data_frame", "multiple_imputation")) {
    title <- if (is.null(x)) {
      paste(y, .rls_mi_title_suffix(record))
    } else {
      paste(y, "by", x, .rls_mi_title_suffix(record))
    }
  }
  ls_boxplot(data, y = y, x = x, group = record$group, labels = labels,
             title = title, linked = linked, show_points = show_points,
             show_box = show_box, show_whiskers = show_whiskers,
             show_violin = show_violin, connect_rows = connect_rows,
             standardize = standardize)
}

.rls_boxplot_option <- function(plot, option, show) {
  .rls_send(c("BOXPLOT_OPTION", .rls_plot_id(plot), option, if (isTRUE(show)) "TRUE" else "FALSE"))
  invisible(TRUE)
}

#' Set boxplot display options
#'
#' @param plot An `rlispstat_plot` object or plot id.
#' @param show Logical.
#' @return Invisibly returns `TRUE`.
#' @export
ls_boxplot_show_points <- function(plot, show = TRUE) {
  .rls_boxplot_option(plot, "points", show)
}

#' @rdname ls_boxplot_show_points
#' @export
ls_boxplot_show_box <- function(plot, show = TRUE) {
  .rls_boxplot_option(plot, "box", show)
}

#' @rdname ls_boxplot_show_points
#' @export
ls_boxplot_show_whiskers <- function(plot, show = TRUE) {
  .rls_boxplot_option(plot, "whiskers", show)
}

#' @rdname ls_boxplot_show_points
#' @export
ls_boxplot_show_violin <- function(plot, show = TRUE) {
  .rls_boxplot_option(plot, "violin", show)
  id <- .rls_plot_id(plot)
  if (exists(id, envir = .rls_state$plots, inherits = FALSE)) {
    record <- get(id, envir = .rls_state$plots)
    record$show_violin <- isTRUE(show)
    assign(id, record, envir = .rls_state$plots)
  }
  invisible(TRUE)
}

#' @rdname ls_boxplot_show_points
#' @export
ls_boxplot_connect_rows <- function(plot, show = TRUE) {
  .rls_boxplot_option(plot, "connect_rows", show)
  id <- .rls_plot_id(plot)
  if (exists(id, envir = .rls_state$plots, inherits = FALSE)) {
    record <- get(id, envir = .rls_state$plots)
    record$connect_rows <- isTRUE(show)
    assign(id, record, envir = .rls_state$plots)
  }
  invisible(TRUE)
}

#' @rdname ls_boxplot_show_points
#' @export
ls_boxplot_standardize_variables <- function(plot, show = TRUE) {
  .rls_boxplot_option(plot, "standardize_variables", show)
  id <- .rls_plot_id(plot)
  if (exists(id, envir = .rls_state$plots, inherits = FALSE)) {
    record <- get(id, envir = .rls_state$plots)
    record$standardize <- isTRUE(show)
    assign(id, record, envir = .rls_state$plots)
  }
  invisible(TRUE)
}

.rls_boxplot_display_values <- function(record) {
  vars <- record$boxplot_variables %||% character()
  if (!length(vars)) {
    vars <- record$y
  }
  values <- unlist(record$data[vars], use.names = FALSE)
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  if (isTRUE(record$standardize)) {
    scaled <- lapply(vars, function(var) {
      x <- as.numeric(record$data[[var]])
      x <- x[is.finite(x)]
      s <- stats::sd(x)
      if (!length(x)) {
        numeric()
      } else if (!is.finite(s) || s == 0) {
        rep(0, length(x))
      } else {
        (x - mean(x)) / s
      }
    })
    values <- unlist(scaled, use.names = FALSE)
  }
  values[is.finite(values)]
}

#' @rdname ls_boxplot_show_points
#' @param threshold Numeric threshold. For \code{alternative = "two.sided"}, use
#'   either two thresholds or one absolute threshold around zero.
#' @param alternative One of \code{"less"}, \code{"greater"}, or
#'   \code{"two.sided"}.
#' @param pvalue Optional tail probability used to compute threshold quantiles
#'   from the displayed boxplot values.
#' @export
ls_boxplot_split_violin <- function(plot, threshold = NULL,
                                    alternative = c("less", "greater", "two.sided"),
                                    pvalue = NULL, show = TRUE) {
  alternative <- match.arg(alternative)
  id <- .rls_plot_id(plot)
  record <- .rls_plot_record(plot)
  if (!identical(record$type, "boxplot")) {
    stop("`plot` must be a boxplot.", call. = FALSE)
  }
  if (is.null(threshold) && !is.null(pvalue)) {
    if (!is.numeric(pvalue) || length(pvalue) != 1L || !is.finite(pvalue) ||
        pvalue <= 0 || pvalue >= 1) {
      stop("`pvalue` must be a number between 0 and 1.", call. = FALSE)
    }
    values <- .rls_boxplot_display_values(record)
    if (!length(values)) {
      stop("No finite boxplot values are available to compute thresholds.", call. = FALSE)
    }
    threshold <- switch(alternative,
      less = unname(stats::quantile(values, pvalue, names = FALSE, type = 7)),
      greater = unname(stats::quantile(values, 1 - pvalue, names = FALSE, type = 7)),
      two.sided = unname(stats::quantile(values, c(pvalue / 2, 1 - pvalue / 2), names = FALSE, type = 7))
    )
  }
  if (is.null(threshold)) {
    threshold <- 0
  }
  if (!is.numeric(threshold) || !all(is.finite(threshold))) {
    stop("`threshold` must contain finite numeric values.", call. = FALSE)
  }
  if (alternative == "two.sided") {
    if (length(threshold) == 1L) {
      lower <- -abs(threshold[[1L]])
      upper <- abs(threshold[[1L]])
    } else {
      lower <- min(threshold)
      upper <- max(threshold)
    }
  } else {
    lower <- threshold[[1L]]
    upper <- lower
  }
  .rls_send(c(
    "BOXPLOT_SPLIT_VIOLIN",
    id,
    if (isTRUE(show)) "TRUE" else "FALSE",
    alternative,
    sprintf("%.17g", lower),
    sprintf("%.17g", upper)
  ))
  if (exists(id, envir = .rls_state$plots, inherits = FALSE)) {
    record$show_violin <- isTRUE(show) || isTRUE(record$show_violin)
    record$split_violin <- isTRUE(show)
    record$split_alternative <- alternative
    record$split_lower <- lower
    record$split_upper <- upper
    assign(id, record, envir = .rls_state$plots)
  }
  invisible(TRUE)
}

#' @rdname ls_boxplot_show_points
#' @param h0 Null value. For a one-variable boxplot this is \eqn{\mu_0}; for a
#'   two-group boxplot this is the null difference between group means.
#' @param draws Number of simulated null draws.
#' @export
ls_boxplot_h0_simulation <- function(plot, h0 = 0,
                                     alternative = c("two.sided", "less", "greater"),
                                     draws = 1000, show = TRUE) {
  alternative <- match.arg(alternative)
  id <- .rls_plot_id(plot)
  record <- .rls_plot_record(plot)
  if (!identical(record$type, "boxplot")) {
    stop("`plot` must be a boxplot.", call. = FALSE)
  }
  if (!is.numeric(h0) || length(h0) != 1L || !is.finite(h0)) {
    stop("`h0` must be a single finite number.", call. = FALSE)
  }
  if (!is.numeric(draws) || length(draws) != 1L || !is.finite(draws)) {
    stop("`draws` must be a single finite number.", call. = FALSE)
  }
  draws <- as.integer(max(100, min(200000, round(draws))))
  .rls_send(c(
    "BOXPLOT_H0_SIMULATION",
    id,
    if (isTRUE(show)) "TRUE" else "FALSE",
    sprintf("%.17g", h0),
    alternative,
    as.character(draws)
  ))
  if (exists(id, envir = .rls_state$plots, inherits = FALSE)) {
    record$h0_simulation <- isTRUE(show)
    record$h0 <- h0
    record$h0_alternative <- alternative
    record$h0_draws <- draws
    assign(id, record, envir = .rls_state$plots)
  }
  invisible(TRUE)
}

#' @rdname ls_boxplot_show_points
#' @export
ls_boxplot_clear_h0_simulation <- function(plot) {
  ls_boxplot_h0_simulation(plot, show = FALSE)
}

.rls_boxplot_variable_record <- function(plot, variable) {
  record <- .rls_plot_record(plot)
  if (!identical(record$type, "boxplot")) {
    stop("`plot` must be a boxplot.", call. = FALSE)
  }
  if (nzchar(record$x)) {
    stop("Variables can only be added to ungrouped boxplots.", call. = FALSE)
  }
  variable <- .rls_validate_protocol_name(variable, "variable")
  if (!variable %in% names(record$data)) {
    stop(sprintf("Column `%s` was not found in the plot data.", variable), call. = FALSE)
  }
  if (!is.numeric(record$data[[variable]])) {
    stop("`variable` must name a numeric column.", call. = FALSE)
  }
  list(record = record, variable = variable)
}

#' Add or remove variables in a parallel boxplot
#'
#' @param plot An `rlispstat_plot` object or plot id.
#' @param variable Numeric variable name.
#' @return Invisibly returns `TRUE`.
#' @export
ls_boxplot_add_variable <- function(plot, variable) {
  info <- .rls_boxplot_variable_record(plot, variable)
  .rls_send(c("BOXPLOT_ADD_VARIABLE", info$record$id, info$variable))
  vars <- unique(c(info$record$boxplot_variables %||% info$record$y, info$variable))
  info$record$boxplot_variables <- vars
  info$record$y <- if (length(vars) == 1L) vars[[1L]] else "Value"
  assign(info$record$id, info$record, envir = .rls_state$plots)
  invisible(TRUE)
}

#' @rdname ls_boxplot_add_variable
#' @export
ls_boxplot_remove_variable <- function(plot, variable) {
  info <- .rls_boxplot_variable_record(plot, variable)
  vars <- info$record$boxplot_variables %||% info$record$y
  if (length(vars) <= 1L) {
    stop("A boxplot must keep at least one variable.", call. = FALSE)
  }
  if (!info$variable %in% vars) {
    stop(sprintf("Variable `%s` is not in this boxplot.", info$variable), call. = FALSE)
  }
  .rls_send(c("BOXPLOT_REMOVE_VARIABLE", info$record$id, info$variable))
  vars <- setdiff(vars, info$variable)
  info$record$boxplot_variables <- vars
  info$record$y <- if (length(vars) == 1L) vars[[1L]] else "Value"
  assign(info$record$id, info$record, envir = .rls_state$plots)
  invisible(TRUE)
}

#' @rdname ls_boxplot_show_points
#' @export
ls_boxplot_options <- function(plot) {
  reply <- .rls_send(c("BOXPLOT_OPTIONS", .rls_plot_id(plot)))
  records <- .rls_parse_records(reply)
  if (length(records) != 1L) {
    stop("Unexpected boxplot options reply from the LinkEDA backend.", call. = FALSE)
  }
  parts <- strsplit(records[[1L]], "|", fixed = TRUE)[[1L]]
  list(
    show_points = identical(parts[[1L]], "TRUE"),
    show_box = identical(parts[[2L]], "TRUE"),
    show_whiskers = identical(parts[[3L]], "TRUE"),
    connect_rows = length(parts) >= 4L && identical(parts[[4L]], "TRUE"),
    standardize = length(parts) >= 5L && identical(parts[[5L]], "TRUE"),
    show_violin = length(parts) >= 6L && identical(parts[[6L]], "TRUE"),
    split_violin = length(parts) >= 7L && identical(parts[[7L]], "TRUE"),
    split_alternative = if (length(parts) >= 8L) parts[[8L]] else NA_character_,
    split_lower = if (length(parts) >= 9L) suppressWarnings(as.numeric(parts[[9L]])) else NA_real_,
    split_upper = if (length(parts) >= 10L) suppressWarnings(as.numeric(parts[[10L]])) else NA_real_,
    h0_simulation = length(parts) >= 11L && identical(parts[[11L]], "TRUE"),
    h0_alternative = if (length(parts) >= 12L) parts[[12L]] else NA_character_,
    h0 = if (length(parts) >= 13L) suppressWarnings(as.numeric(parts[[13L]])) else NA_real_,
    h0_draws = if (length(parts) >= 14L) suppressWarnings(as.integer(parts[[14L]])) else NA_integer_,
    h0_p = if (length(parts) >= 15L) parts[[15L]] else NA_character_,
    h0_effect = if (length(parts) >= 16L) suppressWarnings(as.numeric(parts[[16L]])) else NA_real_
  )
}
