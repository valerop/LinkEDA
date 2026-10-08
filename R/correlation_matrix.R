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

.rls_correlation_sample_size_matrix <- function(data, use = "pairwise") {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  count <- ncol(data)
  result <- matrix(0, nrow = count, ncol = count,
                   dimnames = list(names(data), names(data)))
  if (!count) return(result)
  if (identical(use, "complete") || identical(use, "complete.obs") ||
      identical(use, "listwise")) {
    result[] <- sum(stats::complete.cases(data))
    return(result)
  }
  for (row in seq_len(count)) for (column in seq_len(count)) {
    result[row, column] <- sum(stats::complete.cases(
      data[, c(row, column), drop = FALSE]
    ))
  }
  result
}

# Shared correlation engine used by both the standalone Correlation Matrix and
# Scale Analysis.  The caller is responsible for preparing the variables (for
# example, preserving ordered item levels); this function owns the statistical
# choice and always returns the same correlation-result contract.  Keeping the
# estimator independent from either window lets the common native matrix render
# Pearson, polychoric, and mixed correlations without duplicating presentation
# code.
.rls_correlation_psych_matrix <- function(data, variable_types = NULL,
                                          method = c("auto", "pearson", "polychoric", "mixed"),
                                          use = "pairwise", smooth = TRUE) {
  method <- match.arg(tolower(method), c("auto", "pearson", "polychoric", "mixed"))
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (use %in% c("complete", "complete.obs"))
    data <- data[stats::complete.cases(data), , drop = FALSE]
  if (is.null(variable_types)) variable_types <- rep("numeric", ncol(data))
  variable_types <- tolower(as.character(variable_types))
  if (length(variable_types) != ncol(data)) {
    stop("Correlation variable types must match the supplied columns.", call. = FALSE)
  }
  variable_types[variable_types %in% c("continuous", "scale")] <- "numeric"
  variable_types[variable_types %in% c("ordered")] <- "ordinal"
  if (any(!variable_types %in% c("numeric", "ordinal"))) {
    stop("Correlation variables must be numeric or ordinal.", call. = FALSE)
  }
  if (method == "auto") {
    method <- if (all(variable_types == "numeric")) "pearson"
      else if (all(variable_types == "ordinal")) "polychoric" else "mixed"
  }
  if (ncol(data) < 2L) {
    empty <- matrix(numeric(), 0L, 0L)
    return(list(matrix = empty, p_values = empty, sample_sizes = empty, method = method,
                package = "psych", function_name = NA_character_))
  }
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("Correlation analysis requires the R package `psych`.", call. = FALSE)
  }
  if (identical(method, "pearson")) {
    fit <- suppressWarnings(psych::corr.test(
      data, use = use, method = "pearson", adjust = "none", ci = FALSE
    ))
    return(list(matrix = unclass(fit$r), p_values = unclass(fit$p),
                sample_sizes = .rls_correlation_sample_size_matrix(data, use),
                method = method, package = "psych",
                function_name = "psych::corr.test"))
  }
  if (identical(method, "polychoric")) {
    fit <- suppressMessages(suppressWarnings(psych::polychoric(data, progress = FALSE, smooth = smooth)))
    correlations <- unclass(fit$rho)
    return(list(matrix = correlations,
                p_values = matrix(NA_real_, nrow(correlations), ncol(correlations),
                                  dimnames = dimnames(correlations)),
                sample_sizes = .rls_correlation_sample_size_matrix(data, use),
                method = method, package = "psych",
                function_name = "psych::polychoric"))
  }
  ordinal <- which(variable_types == "ordinal")
  continuous <- which(variable_types == "numeric")
  fit <- suppressMessages(suppressWarnings(psych::mixedCor(
    data = data, c = continuous, p = ordinal, use = use, smooth = smooth
  )))
  correlations <- unclass(fit$rho)
  list(matrix = correlations,
       p_values = matrix(NA_real_, nrow(correlations), ncol(correlations),
                         dimnames = dimnames(correlations)),
       sample_sizes = .rls_correlation_sample_size_matrix(data, use),
       method = method, package = "psych",
       function_name = "psych::mixedCor")
}

.rls_correlation_validate_variables <- function(data, variables, metadata = NULL) {
  numeric <- .rls_numeric_variable_names(data, metadata)
  if (is.null(variables)) {
    return(character())
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

.rls_correlation_empty_results <- function() {
  data.frame(
    x_variable = character(), y_variable = character(),
    r = numeric(), p = numeric(), n = integer(),
    missing_mode = character(), status = character(),
    rows_used_original_ids = I(list()),
    stringsAsFactors = FALSE
  )
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
  if (any(!is.finite(x)) || any(!is.finite(y))) {
    return(list(x_variable=x_name, y_variable=y_name, r=NA_real_, p=NA_real_,
      n=n, rows_used=rows, status="nonfinite_values"))
  }
  if (stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(list(
      x_variable = x_name, y_variable = y_name, r = NA_real_, p = NA_real_,
      n = n, rows_used = rows, status = "zero_variance"
    ))
  }
  test <- stats::cor.test(x, y, method = "pearson")
  r <- unname(test$estimate)
  p <- test$p.value
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
  if (length(variables) < 2L) return(.rls_correlation_empty_results())
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
  dataset <- .rls_dataset_record(record$group)
  if (!isTRUE(record$native_scope_snapshot))
    record <- .rls_apply_scope_to_model_request(record, dataset)
  scoped <- .rls_dataset_subset_original_rows(dataset, record$data_scope$rows)
  record$data <- scoped$data
  record$original_row_ids <- scoped$original_row_ids
  if (length(record$variables) < 2L) {
    record$results <- .rls_correlation_empty_results()
    record$multiple_imputation <- NULL
    record$model_version <- record$model_version + 1L
    return(.rls_correlation_attach_provenance(record))
  }
  if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation")) {
    record$results <- .rls_mi_correlation_compute(record)
    dataset <- .rls_dataset_record(record$group)
    record$multiple_imputation <- .rls_mi_result_metadata(
      dataset,
      "correlation_matrix",
      list(variables = record$variables, method = record$method, missing_mode = record$missing_mode),
      estimates_by_imputation = record$results$r_by_imputation,
      pooled_result = record$results,
      pooling_method = "Fisher z + mice::pool.scalar",
      warnings = character()
    )
  } else {
    record$results <- .rls_correlation_compute(record$data, record$variables, record$method, record$missing_mode)
    record$results$rows_used_original_ids <- I(lapply(record$results$rows_used_original_ids,
      function(rows) record$original_row_ids[rows]))
  }
  record$model_version <- record$model_version + 1L
  .rls_correlation_attach_provenance(record)
}

.rls_correlation_wire_value <- function(value) {
  .rls_native_wire_value(value)
}

.rls_correlation_wire_number <- function(value) {
  numeric_value <- suppressWarnings(as.numeric(value))
  if (!length(numeric_value) || !is.finite(numeric_value[[1L]])) return("NA")
  sprintf("%.17g", numeric_value[[1L]])
}

.rls_correlation_cell_detail <- function(row, imputation_count, pooling_method) {
  if (!imputation_count) return(switch(row$status[[1L]],
    valid="Pearson correlation and two-sided p-value calculated with stats::cor.test in R.",
    diagonal="Diagonal cell; N counts non-missing observations.",
    insufficient_n="Correlation unavailable: fewer than three complete pairs.",
    zero_variance="Correlation unavailable: a variable is constant in the included cases.",
    nonfinite_values="Correlation unavailable: the included values contain infinity.",
    "Correlation unavailable for these variables and cases."))
  if (!row$status[[1L]] %in% c("valid", "diagonal"))
    return(paste("Pooling unavailable:", row$pooling_reason[[1L]]))
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
    ". N is the rounded mean pair count across imputations. Linked rows are the union of included cases. Scatterplots display the active imputation."
  )
}

.rls_correlation_native_payload <- function(record) {
  cells <- record$results
  mi <- identical(record$analysis_backend %||% "ordinary", "multiple_imputation")
  imputation_count <- if (mi) as.integer(record$imputation_count %||% 0L) else 0L
  pooling_method <- if (mi) "Fisher z + mice::pool.scalar" else ""
  note <- if (mi) sprintf("Multiple imputation: m = %d; Fisher-z pooling in R.", imputation_count) else
    "Pearson correlations calculated in R."
  title <- if (mi) "Pearson Correlation Matrix - Multiple Imputation" else "Pearson Correlation Matrix"
  lines <- c(
    "CORR_OPEN_STRUCTURED",
    .rls_correlation_wire_value(record$id),
    .rls_correlation_wire_value(record$group),
    .rls_correlation_wire_value(title),
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
  snapshot <- record$data_scope
  lines <- c(lines,"CORR_SCOPE_V1",snapshot$kind,snapshot$description,
    as.character(snapshot$total_n),as.character(length(snapshot$rows)),as.character(snapshot$rows))
  lines <- c(lines, .rls_analysis_provenance_payload(record))
  lines
}

.rls_correlation_sync_native_open <- function(record) {
  .rls_start_backend()
  dataset <- .rls_dataset_record(record$group)
  try(.rls_register_native_dataset_if_needed(dataset, visible = TRUE), silent = TRUE)
  try(.rls_send(.rls_correlation_native_payload(record)), silent = TRUE)
}

#' Create a native Pearson correlation matrix window
#'
#' @param data Registered dataset name, a data frame, or `NULL` for active dataset.
#' @param variables Numeric variables to include. If `NULL`, the matrix opens
#'   with an empty variable list and waits for an explicit selection.
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
  variable <- .rls_correlation_validate_variables(dataset$data, variable, dataset$variable_metadata)
  record$variables <- unique(c(record$variables, variable))
  record <- .rls_correlation_refit(record)
  handle <- .rls_assign_correlation(record)
  if (isTRUE(.rls_state$process_started)) {
    .rls_correlation_sync_native_open(record)
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
    .rls_correlation_sync_native_open(record)
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
  if (identical(cell$status, "diagonal")) return("\u2014")
  if (!is.finite(cell$r)) return("\u2014")
  parts <- .rls_correlation_format_r(cell$r)
  if (isTRUE(show_significance_stars)) {
    parts <- paste0(parts, .rls_export_stars(cell$p))
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
      cell <- row[1L, ]
      label <- .rls_correlation_cell_label(
        cell, record$show_significance_stars %||% record$show_p, record$show_p_value
      )
      if (isTRUE(record$show_n) && !identical(cell$status, "diagonal") && is.finite(cell$n)) {
        label <- paste(label, paste0("N = ", cell$n), sep = "\n")
      }
      label
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
            "Pooled Pearson correlations from %d imputations. Pooling scale: Fisher z; scalar uncertainty pooled with mice::pool.scalar. ",
            record$imputation_count %||% length(.rls_dataset_record(record$group)$completed_datasets %||% list())
          )
        } else {
          "Pearson correlations. "
        },
        if (identical(record$missing_mode, "pairwise")) "Pairwise complete observations." else "Listwise complete observations.",
        if (isTRUE(record$show_significance_stars)) "\n* p < .05; ** p < .01; *** p < .001" else "",
        if (isTRUE(record$show_p_value)) "\nExact p-values shown." else "",
        if (isTRUE(record$show_n)) {
          if (identical(record$analysis_backend %||% "ordinary", "multiple_imputation")) {
            "\nN is the rounded mean of pair counts across imputations."
          } else "\nN is the number of complete observations for each pair."
        } else ""
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

# A queued request carries an immutable row scope and dataset version.
.rls_handle_correlation_needed <- function(parts) {
  if (length(parts) < 10L) return(invisible(NULL))
  id <- parts[2L]; revision <- parts[4L]; version <- parts[5L]
  record <- NULL
  result <- tryCatch({
    dataset <- .rls_dataset_record(parts[3L])
    if (!identical(as.character(dataset$data_version %||% 1L), version))
      stop("The data changed before correlations could run. Recalculate the matrix.", call.=FALSE)
    count <- as.integer(parts[9L])
    if (is.na(count) || count < 2L || length(parts) < 10L + count)
      stop("Invalid correlation variables.", call.=FALSE)
    variables <- parts[seq.int(10L, 9L + count)]
    cursor <- 10L + count; nr <- as.integer(parts[cursor])
    if (is.na(nr) || nr < 0L || length(parts) != cursor + nr)
      stop("Invalid correlation scope.", call.=FALSE)
    rows <- if (nr) as.integer(parts[seq.int(cursor+1L,cursor+nr)]) else integer()
    if (anyNA(rows) || any(rows < 1L)) stop("Invalid row identities.", call.=FALSE)
    scope <- .rls_capture_analysis_scope(dataset, if(parts[6L]=="all") "all" else "selected", rows)
    record <- list(id=id, correlation_id=id, group=dataset$group, dataset_id=dataset$group,
      variables=.rls_correlation_validate_variables(dataset$data,variables,dataset$variable_metadata),
      method=.rls_correlation_validate_method(parts[7L]),
      missing_mode=match.arg(parts[8L], c("pairwise","listwise")),
      analysis_backend=.rls_analysis_backend(dataset,"correlation_matrix"),
      imputation_count=dataset$imputation_count, dataset_type=dataset$dataset_type,
      data_scope=scope, native_scope_snapshot=TRUE, model_version=0L,
      show_significance_stars=TRUE, show_p_value=FALSE, show_n=FALSE)
    record <- .rls_correlation_refit(record)
    .rls_assign_correlation(record)
    c("CORR_UPDATE",id,revision,version,"ok",.rls_correlation_native_payload(record)[-1L])
  }, error=function(e) c("CORR_UPDATE",id,revision,version,"error",.rls_native_wire_value(conditionMessage(e))))
  .rls_send(as.character(result))
  invisible(record)
}

.rls_correlation_attach_provenance <- function(record) {
  record$rows_used_original_ids <- sort(unique(as.integer(unlist(record$results$rows_used_original_ids))))
  record$rows_excluded_original_ids <- setdiff(record$data_scope$rows, record$rows_used_original_ids)
  recipe <- .rls_correlation_verification_code(record)
  .rls_attach_analysis_provenance(record, recipe,
    title="Pearson Correlation Matrix", output_code=list(table="reference_correlations"),
    verification_code=list(table=recipe),
    verification_variables=record$variables)
}
