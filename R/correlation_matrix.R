.rls_correlation_id <- function(group, name = NULL) {
  .rls_validate_protocol_name(name %||% paste0("corr_", group, "_", format(Sys.time(), "%Y%m%d%H%M%OS3")), "name")
}

.rls_correlation_record <- function(matrix) {
  id <- if (inherits(matrix, "rlispstat_correlation_matrix")) matrix$id else matrix
  id <- .rls_validate_protocol_name(id, "matrix")
  if (!exists(id, envir = .rls_state$correlation_matrices, inherits = FALSE)) {
    stop("Unknown correlation matrix.", call. = FALSE)
  }
  get(id, envir = .rls_state$correlation_matrices)
}

.rls_assign_correlation <- function(record) {
  assign(record$id, record, envir = .rls_state$correlation_matrices)
  structure(list(id = record$id, group = record$group), class = "rlispstat_correlation_matrix")
}

.rls_correlation_validate_method <- function(method) {
  match.arg(method, "pearson")
}

.rls_correlation_validate_variables <- function(data, variables, metadata = NULL) {
  numeric <- .rls_numeric_variable_names(data, metadata)
  if (is.null(variables)) {
    return(utils::head(numeric, 4L))
  }
  variables <- unique(vapply(variables, .rls_validate_protocol_name, character(1L), what = "variable"))
  missing <- setdiff(variables, names(data))
  if (length(missing)) {
    stop(sprintf("Column `%s` was not found in the dataset.", missing[[1L]]), call. = FALSE)
  }
  non_numeric <- variables[!variables %in% numeric]
  if (length(non_numeric)) {
    stop(sprintf("Column `%s` must be numeric for Pearson correlations.", non_numeric[[1L]]), call. = FALSE)
  }
  variables
}

.rls_correlation_cell <- function(data, variables, x_name, y_name, missing_mode) {
  if (identical(x_name, y_name)) {
    rows <- if (identical(missing_mode, "listwise")) {
      which(stats::complete.cases(data[, variables, drop = FALSE]))
    } else {
      which(!is.na(data[[x_name]]))
    }
    return(list(
      x_variable = x_name, y_variable = y_name, r = NA_real_, p = NA_real_,
      n = length(rows), rows_used = rows, status = "diagonal"
    ))
  }

  rows <- if (identical(missing_mode, "listwise")) {
    which(stats::complete.cases(data[, variables, drop = FALSE]))
  } else {
    which(stats::complete.cases(data[, c(x_name, y_name), drop = FALSE]))
  }
  n <- length(rows)
  if (n < 3L) {
    return(list(
      x_variable = x_name, y_variable = y_name, r = NA_real_, p = NA_real_,
      n = n, rows_used = rows, status = "insufficient_n"
    ))
  }
  x <- as.double(data[[x_name]][rows])
  y <- as.double(data[[y_name]][rows])
  if (stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(list(
      x_variable = x_name, y_variable = y_name, r = NA_real_, p = NA_real_,
      n = n, rows_used = rows, status = "zero_variance"
    ))
  }
  r <- stats::cor(x, y, method = "pearson")
  p <- if (is.finite(r) && abs(r) < 1) {
    t_value <- r * sqrt((n - 2) / (1 - r * r))
    2 * stats::pt(-abs(t_value), df = n - 2)
  } else if (is.finite(r)) {
    0
  } else {
    NA_real_
  }
  list(
    x_variable = x_name, y_variable = y_name, r = unname(r), p = unname(p),
    n = n, rows_used = rows, status = if (is.finite(r)) "valid" else "invalid"
  )
}

.rls_correlation_compute <- function(data, variables, method, missing_mode) {
  method <- .rls_correlation_validate_method(method)
  if (!identical(method, "pearson")) {
    stop("Only Pearson correlations are currently supported.", call. = FALSE)
  }
  cells <- vector("list", length(variables) * length(variables))
  k <- 1L
  for (y in variables) {
    for (x in variables) {
      cells[[k]] <- .rls_correlation_cell(data, variables, x, y, missing_mode)
      k <- k + 1L
    }
  }
  data.frame(
    x_variable = vapply(cells, `[[`, character(1L), "x_variable"),
    y_variable = vapply(cells, `[[`, character(1L), "y_variable"),
    r = vapply(cells, `[[`, numeric(1L), "r"),
    p = vapply(cells, `[[`, numeric(1L), "p"),
    n = vapply(cells, `[[`, integer(1L), "n"),
    missing_mode = missing_mode,
    status = vapply(cells, `[[`, character(1L), "status"),
    rows_used_original_ids = I(lapply(cells, `[[`, "rows_used")),
    stringsAsFactors = FALSE
  )
}

.rls_correlation_refit <- function(record) {
  if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation")) {
    record$results <- .rls_mi_correlation_compute(record)
    dataset <- .rls_dataset_record(record$group)
    record$multiple_imputation <- .rls_mi_result_metadata(
      dataset,
      "correlation_matrix",
      list(variables = record$variables, method = record$method, missing_mode = record$missing_mode),
      estimates_by_imputation = record$results$r_by_imputation,
      pooled_result = record$results,
      pooling_method = "Fisher z transformation plus Rubin's rules",
      warnings = character()
    )
  } else {
    record$results <- .rls_correlation_compute(record$data, record$variables, record$method, record$missing_mode)
  }
  record$model_version <- record$model_version + 1L
  record
}

.rls_correlation_wire_value <- function(value) {
  value <- as.character(value %||% "")
  value[is.na(value)] <- ""
  gsub("[\r\n\t]+", " ", value)
}

.rls_correlation_wire_number <- function(value) {
  numeric_value <- suppressWarnings(as.numeric(value))
  if (!length(numeric_value) || !is.finite(numeric_value[[1L]])) return("NA")
  sprintf("%.17g", numeric_value[[1L]])
}

.rls_correlation_cell_detail <- function(row, imputation_count, pooling_method) {
  if (identical(row$status[[1L]], "diagonal")) {
    return(sprintf("Multiple imputation matrix; m = %d; diagonal cell.", imputation_count))
  }
  ci_low <- .rls_export_format_number(row$pooled_CI_low[[1L]], 3L)
  ci_high <- .rls_export_format_number(row$pooled_CI_high[[1L]], 3L)
  se <- .rls_export_format_number(row$pooled_SE[[1L]], 3L)
  df <- .rls_export_format_number(row$pooled_df[[1L]], 1L)
  fmi <- .rls_export_format_percent(row$fraction_missing_information[[1L]], 1L)
  paste0(
    "m = ", imputation_count,
    "; pooling = ", pooling_method,
    "; 95% CI [", ci_low, ", ", ci_high, "]",
    "; SE(z) = ", se,
    "; df = ", df,
    "; FMI = ", fmi,
    ". Scatterplots opened from this cell display the active imputation."
  )
}

.rls_correlation_native_payload <- function(record) {
  if (!identical(record$analysis_backend %||% "ordinary", "multiple_imputation")) {
    return(c(
      "CORR_OPEN", record$id, record$group, record$method, record$missing_mode,
      if (isTRUE(record$show_significance_stars)) "TRUE" else "FALSE",
      if (isTRUE(record$show_p_value)) "TRUE" else "FALSE",
      if (isTRUE(record$show_n)) "TRUE" else "FALSE",
      as.character(length(record$variables)), record$variables
    ))
  }

  cells <- record$results
  imputation_count <- as.integer(record$imputation_count %||% record$multiple_imputation$m %||% 0L)
  pooling_method <- record$multiple_imputation$pooling_method %||% "Fisher z transformation plus Rubin's rules"
  note <- sprintf(
    "Multiple imputation correlation matrix: m = %d; Pearson r pooled on Fisher z scale with Rubin's rules.",
    imputation_count
  )
  lines <- c(
    "CORR_OPEN_STRUCTURED",
    .rls_correlation_wire_value(record$id),
    .rls_correlation_wire_value(record$group),
    .rls_correlation_wire_value("Pearson Correlation Matrix - Multiple Imputation"),
    .rls_correlation_wire_value(record$method),
    .rls_correlation_wire_value(record$missing_mode),
    if (isTRUE(record$show_significance_stars)) "TRUE" else "FALSE",
    if (isTRUE(record$show_p_value)) "TRUE" else "FALSE",
    if (isTRUE(record$show_n)) "TRUE" else "FALSE",
    as.character(imputation_count),
    .rls_correlation_wire_value(pooling_method),
    .rls_correlation_wire_value(note),
    as.character(length(record$variables)),
    .rls_correlation_wire_value(record$variables),
    as.character(nrow(cells))
  )
  for (i in seq_len(nrow(cells))) {
    row <- cells[i, , drop = FALSE]
    rows_used <- row$rows_used_original_ids[[1L]] %||% integer()
    rows_used <- suppressWarnings(as.integer(rows_used))
    rows_used <- rows_used[!is.na(rows_used)]
    n_value <- suppressWarnings(as.integer(row$n[[1L]]))
    if (!length(n_value) || is.na(n_value)) n_value <- 0L
    lines <- c(
      lines,
      .rls_correlation_wire_value(row$x_variable[[1L]]),
      .rls_correlation_wire_value(row$y_variable[[1L]]),
      .rls_correlation_wire_number(row$r[[1L]]),
      .rls_correlation_wire_number(row$p[[1L]]),
      as.character(n_value),
      .rls_correlation_wire_value(row$status[[1L]]),
      .rls_correlation_wire_value(.rls_correlation_cell_detail(row, imputation_count, pooling_method)),
      as.character(length(rows_used)),
      as.character(rows_used)
    )
  }
  lines
}

.rls_correlation_sync_native_open <- function(record) {
  .rls_start_backend()
  dataset <- .rls_dataset_record(record$group)
  try(.rls_send(c(
    "REGISTER_DATASET",
    record$group,
    .rls_variable_payload(record$data, dataset$variable_metadata),
    .rls_dataframe_payload(record$data, dataset$variable_metadata, dataset_record = dataset)
  )), silent = TRUE)
  try(.rls_send(.rls_correlation_native_payload(record)), silent = TRUE)
}

#' Create a native Pearson correlation matrix window
#'
#' @param data Registered dataset name, a data frame, or `NULL` for active dataset.
#' @param variables Numeric variables to include. If `NULL`, the first numeric
#'   variables are used and the native window can add more interactively.
#' @param method Correlation method. Currently only `"pearson"` is supported.
#' @param missing Missing-data mode, `"pairwise"` or `"listwise"`.
#' @param show_p Legacy alias for significance stars.
#' @param show_n Logical display option for sample sizes.
#' @param show_p_value Logical. Show exact p-values in cells.
#' @param show_significance_stars Logical. Show significance stars in cells.
#' @param name Optional correlation matrix id or dataset name for data-frame input.
#' @param native Logical. Open the native window when possible.
#' @return A `rlispstat_correlation_matrix` handle.
#' @export
ls_new_correlation_matrix <- function(data = NULL, variables = NULL, method = "pearson",
                                      missing = c("pairwise", "listwise"), show_p = NULL,
                                      show_n = FALSE, show_p_value = FALSE,
                                      show_significance_stars = TRUE, name = NULL, native = TRUE) {
  method <- .rls_correlation_validate_method(method)
  missing <- match.arg(missing)
  if (is.data.frame(data)) {
    group <- .rls_register_dataset(name %||% "correlation_matrix", data, activate = TRUE)
    dataset <- .rls_dataset_record(group)
    id_name <- NULL
  } else {
    dataset <- .rls_dataset_record(data)
    id_name <- name
  }
  variables <- .rls_correlation_validate_variables(dataset$data, variables, dataset$variable_metadata)
  show_significance_stars <- if (is.null(show_p)) isTRUE(show_significance_stars) else isTRUE(show_p)
  backend <- .rls_analysis_backend(dataset, "correlation_matrix")
  record <- list(
    id = .rls_correlation_id(dataset$group, id_name),
    correlation_id = NULL,
    group = dataset$group,
    dataset_id = dataset$group,
    data = dataset$data,
    variables = variables,
    method = method,
    missing_mode = missing,
    missing = missing,
    show_p = show_significance_stars,
    show_significance_stars = show_significance_stars,
    show_p_value = isTRUE(show_p_value),
    show_n = isTRUE(show_n),
    dataset_type = dataset$dataset_type %||% "data_frame",
    analysis_backend = backend,
    imputation_id = dataset$imputation_id %||% NULL,
    imputation_count = dataset$imputation_count %||% NULL,
    results = data.frame(),
    layout = list(),
    selected_cell = NULL,
    model_version = 0L
  )
  record$correlation_id <- record$id
  record <- .rls_correlation_refit(record)
  handle <- .rls_assign_correlation(record)
  if (isTRUE(native)) {
    .rls_correlation_sync_native_open(record)
  }
  invisible(handle)
}

#' @rdname ls_new_correlation_matrix
#' @export
ls_correlation_matrix_state <- function(matrix) {
  .rls_correlation_record(matrix)
}

#' @rdname ls_new_correlation_matrix
#' @export
ls_correlation_matrix_cells <- function(matrix) {
  .rls_correlation_record(matrix)$results
}

#' @rdname ls_new_correlation_matrix
#' @export
ls_correlation_matrix_add_variable <- function(matrix, variable) {
  record <- .rls_correlation_record(matrix)
  dataset <- .rls_dataset_record(record$group)
  variable <- .rls_correlation_validate_variables(record$data, variable, dataset$variable_metadata)
  record$variables <- unique(c(record$variables, variable))
  record <- .rls_correlation_refit(record)
  handle <- .rls_assign_correlation(record)
  if (isTRUE(.rls_state$process_started)) {
    if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation")) {
      .rls_correlation_sync_native_open(record)
    } else {
      try(.rls_send(c("CORR_SET_VARIABLES", record$id, as.character(length(record$variables)), record$variables)), silent = TRUE)
    }
  }
  invisible(handle)
}

#' @rdname ls_new_correlation_matrix
#' @export
ls_correlation_matrix_remove_variable <- function(matrix, variable) {
  record <- .rls_correlation_record(matrix)
  variable <- .rls_validate_protocol_name(variable, "variable")
  record$variables <- setdiff(record$variables, variable)
  record <- .rls_correlation_refit(record)
  handle <- .rls_assign_correlation(record)
  if (isTRUE(.rls_state$process_started)) {
    if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation")) {
      .rls_correlation_sync_native_open(record)
    } else {
      try(.rls_send(c("CORR_SET_VARIABLES", record$id, as.character(length(record$variables)), record$variables)), silent = TRUE)
    }
  }
  invisible(handle)
}

.rls_correlation_format_r <- function(r) {
  if (!is.finite(r)) return("\u2014")
  out <- sprintf("%.3f", r)
  out <- sub("^0", "", out)
  sub("^-0", "-", out)
}

.rls_correlation_cell_label <- function(cell, show_significance_stars = TRUE, show_p_value = FALSE) {
  if (identical(cell$status, "diagonal")) return("—")
  if (!is.finite(cell$r)) return("—")
  parts <- .rls_correlation_format_r(cell$r)
  if (isTRUE(show_significance_stars)) {
    parts <- c(parts, .rls_export_stars(cell$p))
  }
  if (isTRUE(show_p_value) && is.finite(cell$p)) {
    parts <- c(parts, paste0("p=", format.pval(cell$p, digits = 3, eps = 0.001)))
  }
  paste(parts, collapse = "\n")
}

.rls_correlation_table <- function(matrix) {
  record <- .rls_correlation_record(matrix)
  vars <- record$variables
  table <- data.frame(Variable = vars, stringsAsFactors = FALSE, check.names = FALSE)
  for (x in vars) {
    table[[x]] <- vapply(vars, function(y) {
      row <- record$results[record$results$x_variable == x & record$results$y_variable == y, , drop = FALSE]
      if (!nrow(row)) return("\u2014")
      .rls_correlation_cell_label(row[1L, ], record$show_p)
    }, character(1L))
  }
  structure(
    list(
      title = if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation")) {
        "Pearson Correlation Matrix - Multiple Imputation"
      } else {
        "Pearson Correlation Matrix"
      },
      dataset = record$group,
      variables = vars,
      method = record$method,
      missing_mode = record$missing_mode,
      show_p = record$show_significance_stars,
      show_significance_stars = record$show_significance_stars,
      show_p_value = record$show_p_value,
      show_n = record$show_n,
      matrix = table,
      note = paste0(
        if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation")) {
          sprintf(
            "Pooled Pearson correlations from %d imputations. Pooling scale: Fisher z; uncertainty pooled with Rubin's rules. ",
            record$imputation_count %||% length(.rls_dataset_record(record$group)$completed_datasets %||% list())
          )
        } else {
          "Pearson correlations. "
        },
        if (identical(record$missing_mode, "pairwise")) "Pairwise complete observations." else "Listwise complete observations.",
        if (isTRUE(record$show_significance_stars)) "\n* p < .05; ** p < .01; *** p < .001" else "",
        if (isTRUE(record$show_p_value)) "\nExact p-values shown." else ""
      )
    ),
    class = "rlispstat_correlation_table"
  )
}

.rls_render_correlation_table_text <- function(table) {
  paste(c(
    table$title,
    paste("Dataset:", table$dataset),
    paste("Missing data:", table$missing_mode),
    "",
    .rls_render_data_frame_text(table$matrix),
    "",
    table$note
  ), collapse = "\n")
}

.rls_render_correlation_table_markdown <- function(table) {
  paste(c(
    paste0("### ", table$title),
    "",
    paste0("Dataset: `", .rls_markdown_escape(table$dataset), "`"),
    paste0("Missing data: `", .rls_markdown_escape(table$missing_mode), "`"),
    "",
    .rls_render_data_frame_markdown(table$matrix),
    "",
    table$note
  ), collapse = "\n")
}

#' @rdname ls_new_correlation_matrix
#' @export
ls_correlation_matrix_table <- function(matrix) {
  .rls_correlation_table(matrix)
}

#' @rdname ls_new_correlation_matrix
#' @export
ls_copy_correlation_matrix <- function(matrix, format = c("text", "markdown")) {
  format <- match.arg(format)
  table <- .rls_correlation_table(matrix)
  text <- if (identical(format, "markdown")) {
    .rls_render_correlation_table_markdown(table)
  } else {
    .rls_render_correlation_table_text(table)
  }
  .rls_copy_text(text)
}

#' @rdname ls_new_correlation_matrix
#' @export
ls_export_correlation_matrix <- function(matrix, path, format = c("pdf", "txt", "md")) {
  format <- match.arg(format)
  table <- .rls_correlation_table(matrix)
  switch(format,
    pdf = {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      if (isTRUE(capabilities("cairo"))) {
        grDevices::cairo_pdf(path, width = 8.5, height = 11, onefile = TRUE)
      } else {
        grDevices::pdf(path, width = 8.5, height = 11, onefile = TRUE)
      }
      on.exit(grDevices::dev.off(), add = TRUE)
      graphics::plot.new()
      graphics::par(family = "Helvetica", mar = c(0, 0, 0, 0))
      lines <- strsplit(.rls_render_correlation_table_text(table), "\n", fixed = TRUE)[[1L]]
      y <- 0.96
      for (line in lines) {
        graphics::text(0.06, y, line, adj = c(0, 1), family = "mono", cex = 0.72)
        y <- y - 0.032
        if (y < 0.04) break
      }
    },
    txt = writeLines(.rls_render_correlation_table_text(table), path, useBytes = TRUE),
    md = writeLines(.rls_render_correlation_table_markdown(table), path, useBytes = TRUE)
  )
  invisible(path)
}
