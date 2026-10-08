#' Open a linked contingency table
#'
#' Creates the native LinkEDA contingency-table output using the application's
#' current analysis scope. Selection highlighting remains independent of that
#' scope, as in the macOS application.
#'
#' @param group Registered dataset or dataset group name.
#' @param row Row variable name.
#' @param column Column variable name.
#' @return Invisibly returns `TRUE` after the native table is opened.
#' @export
ls_new_contingency_table <- function(group = NULL, row, column) {
  record <- .rls_dataset_record(group)
  row <- .rls_validate_protocol_name(row, "row")
  column <- .rls_validate_protocol_name(column, "column")
  if (!row %in% names(record$data)) {
    stop(sprintf("Column `%s` was not found in the dataset.", row), call. = FALSE)
  }
  if (!column %in% names(record$data)) {
    stop(sprintf("Column `%s` was not found in the dataset.", column), call. = FALSE)
  }
  if (identical(row, column)) {
    stop("`row` and `column` must name different variables.", call. = FALSE)
  }
  .rls_send(c("ANALYZE_CONTINGENCY_TABLE", record$group, row, column))
  invisible(TRUE)
}

# Descriptive cross-tabulations: calculate each completed table before averaging.
.rls_contingency_build <- function(dataset, row_variables, column_variable, id,
                                     display_mode="count_percent") {
  display_mode <- match.arg(display_mode,c("count_percent","count","percent"))
  if (!nzchar(column_variable %||% "")) column_variable <- NULL
  variables <- c(row_variables, column_variable)
  if (!length(row_variables) || anyDuplicated(variables) ||
      any(!variables %in% names(dataset$data)))
    stop("Choose distinct row and column variables from the dataset.", call. = FALSE)
  mi <- identical(dataset$dataset_type, "multiple_imputation")
  completed <- if (mi) .rls_mi_completed_datasets(dataset) else list(dataset$data)
  factors <- lapply(completed, function(data) {
    stats::setNames(lapply(variables, function(v) {
      .rls_table1_display_factor(data[[v]], "categorical")
    }), variables)
  })
  category_levels <- stats::setNames(lapply(variables, function(v) {
    unique(unlist(lapply(factors, function(data) levels(data[[v]])), use.names = FALSE))
  }), variables)
  if (any(lengths(category_levels) == 0L))
    stop("No non-missing categories are available in the analysis scope.", call. = FALSE)
  cells <- prod(lengths(category_levels))
  if (!is.finite(cells) || cells > 100000L)
    stop("The contingency table has too many categories. Choose categorical variables with fewer levels.", call. = FALSE)
  row_keys <- expand.grid(category_levels[row_variables], KEEP.OUT.ATTRS = FALSE,
                          stringsAsFactors = FALSE)
  row_labels <- apply(row_keys, 1L, paste, collapse = " / ")
  column_labels <- if (is.null(column_variable)) character() else category_levels[[column_variable]]
  counts <- lapply(factors, function(data) {
    for (v in variables) data[[v]] <- factor(data[[v]], levels = category_levels[[v]])
    data <- as.data.frame(data, check.names = FALSE)
    data <- data[stats::complete.cases(data), , drop = FALSE]
    tab <- do.call(base::table, c(unname(data), list(useNA = "no")))
    matrix(as.numeric(tab), nrow = nrow(row_keys),
           dimnames = list(row_labels, if(length(column_labels)) column_labels else "Total"))
  })
  if (!any(vapply(counts, sum, numeric(1L)) > 0))
    stop(if(mi) "No complete observations are available for these variables in any imputation." else "No complete observations are available for these variables in the analysis scope.", call. = FALSE)
  # Include margins in each imputation, so their percentages use its own N.
  with_margins <- lapply(counts, function(tab) {
    if (!is.null(column_variable)) tab <- cbind(tab, rowSums(tab))
    rbind(tab, colSums(tab))
  })
  percentages <- lapply(with_margins, function(tab) {
    proportions <- if (is.null(column_variable)) tab / tab[nrow(tab), ncol(tab)] else
      tab / tab[, ncol(tab)]
    proportions[!is.finite(proportions)] <- NA_real_
    proportions
  })
  mean_counts <- Reduce(`+`, with_margins) / length(completed)
  available <- Reduce(`+`, lapply(percentages, function(p) !is.na(p)))
  mean_percentages <- Reduce(`+`, lapply(percentages, function(p) {
    p[is.na(p)] <- 0; p
  })) / available
  mean_percentages[available == 0] <- NA_real_
  columns <- make.unique(c("Variable", "p", "Test", column_labels, "Total"))[-seq_len(3L)]
  if (!mi) {
    order_rows <- do.call(order, lapply(seq_along(row_variables), function(j)
      match(row_keys[[j]], category_levels[[row_variables[[j]]]])))
    row_keys <- row_keys[order_rows, , drop = FALSE]
    row_labels <- row_labels[order_rows]
    mean_counts <- mean_counts[c(order_rows, nrow(mean_counts)), , drop = FALSE]
    mean_percentages <- mean_percentages[c(order_rows, nrow(mean_percentages)), , drop = FALSE]
    available <- available[c(order_rows, nrow(available)), , drop = FALSE]
  }
  labels <- c(row_labels, "Total")
  dimnames(mean_counts) <- dimnames(mean_percentages) <- list(labels, columns)
  display <- data.frame(Variable = labels, check.names = FALSE)
  for (column in columns) display[[column]] <- character(length(labels))
  rows <- lapply(seq_along(labels), function(i) {
    values <- stats::setNames(vapply(seq_along(columns), function(j) {
      count <- formatC(mean_counts[i, j], format = "f", digits = if(mi) 1L else 0L)
      percent <- if (is.na(mean_percentages[i, j])) "\u2014" else
        paste0(formatC(100 * mean_percentages[i, j], format = "f", digits = 1L), "%")
      switch(display_mode,count=count,percent=percent,paste0(count, " (", percent, ")"))
    }, character(1L)), columns)
    raw <- stats::setNames(lapply(seq_along(columns), function(j) {
      list(n = mean_counts[i, j], percent = mean_percentages[i, j])
    }), columns)
    list(row_index = i, row_type = if (i == length(labels)) "nested_total" else if(mi) "mi_contingency_level" else "nested_leaf_level", variable = "",
         level = labels[[i]], values = values, p = "", test = "",
         stub_values = if(i==length(labels)) c("Total",rep("",length(row_variables)-1L)) else
           as.character(unlist(row_keys[i,,drop=FALSE],use.names=FALSE)),
         detail = sprintf("Percentages available in %d of %d imputations.",
                          available[i, ncol(available)], length(completed)),
         raw_statistics = raw)
  })
  if (!mi) {
    original_ids <- dataset$original_row_ids %||% seq_len(nrow(dataset$data))
    category_data <- factors[[1L]]
    complete <- stats::complete.cases(as.data.frame(category_data))
    for (i in seq_along(rows)) {
      selected <- complete
      if (i <= nrow(row_keys)) for (v in row_variables)
        selected <- selected & !is.na(category_data[[v]]) & as.character(category_data[[v]]) == row_keys[[v]][i]
      rows[[i]]$row_ids <- as.integer(original_ids[which(selected)])
      rows[[i]]$cell_ids <- if (is.null(column_variable)) list(rows[[i]]$row_ids) else
        c(lapply(column_labels, function(level) as.integer(original_ids[which(selected &
          !is.na(category_data[[column_variable]]) & as.character(category_data[[column_variable]]) == level)])),
          list(rows[[i]]$row_ids))
      rows[[i]]$row_key <- if (i <= nrow(row_keys)) as.character(unlist(row_keys[i,],use.names=FALSE)) else character()
      rows[[i]]$detail <- if (is.null(column_variable)) "Percent of all complete observations in the analysis scope." else
        "Row percentages; the total row uses all complete observations in the analysis scope."
    }
  }
  for (i in seq_along(rows)) for (column in columns)
    display[i, column] <- rows[[i]]$values[[column]]
  percentage_label <- if (is.null(column_variable)) "percentage of complete observations" else "row percentage"
  notes <- c(
    sprintf("Multiple imputation: m = %d. Cells show %s.", length(completed),
      switch(display_mode,count="mean count",percent=paste("mean", percentage_label),
        paste0("mean count (mean ", percentage_label, ")"))),
    "Counts and percentages are calculated separately in each imputation, then averaged; fractional counts are expected. This is a descriptive summary, not a Rubin-pooled inferential test.",
    if (is.null(column_variable))
      "Percentages use all complete observations in each imputation as the denominator. Empty imputations are omitted from percentages; undefined percentages are shown as a dash." else
      "Each imputation excludes observations missing any table variable. Percentages omit imputations with an empty row; an undefined percentage is shown as a dash. Totals use each imputation's complete observations."
  )
  if (!mi) notes <- c(
    sprintf("N = %d complete observations; %d excluded within the analysis scope.", sum(complete), sum(!complete)),
    if (is.null(column_variable)) "Percentages use all complete observations in the analysis scope." else
      "Percentages use each row total; the Total row uses all complete observations in the analysis scope.",
    "Calculated in R with base::table. Observations missing any table variable are excluded. Undefined percentages are shown as a dash."
  )
  structure(list(id = id, dataset_id = dataset$group, group = dataset$group,
    title = paste("Contingency Table \u2014", paste(row_variables, collapse = " \u00D7 "),
                  if(!is.null(column_variable)) paste("by", column_variable), if(mi) "\u2014 Multiple Imputation"),
    table_type = if(mi) "mi_contingency" else "nested_contingency", variables = row_variables,
    group_variable = column_variable %||% "", variable_types = list(), data = dataset$data,
    contingency_display_mode=display_mode,
    row_variables = row_variables, category_levels = category_levels,
    category_value_labels = stats::setNames(lapply(variables, function(v) {
      .rls_table1_value_labels(completed[[1L]][[v]])
    }), variables),
    summary_statistics = rows, display_table = display, footnotes = notes,
    display_options = list(show_p = FALSE, show_test = FALSE),
    counts_by_imputation = counts, mean_counts = mean_counts,
    mean_row_percentages = mean_percentages,
    analysis_backend = if(mi) "multiple_imputation" else "ordinary", imputation_count = if(mi) length(completed) else 0L
  ), class = "rlispstat_table1_record")
}

.rls_mi_contingency_record <- function(group, row_variables, column_variable,
                                       id = NULL, selected_rows = NULL, display_mode="count_percent") {
  dataset <- .rls_dataset_record(group)
  captured_scope <- .rls_capture_analysis_scope(dataset,if(is.null(selected_rows)) "all" else "selected",selected_rows)
  selected_rows <- if(captured_scope$kind=="all") NULL else captured_scope$rows
  .rls_store_data_version(dataset)
  total_n <- nrow(dataset$data)
  if (!is.null(selected_rows)) {
    dataset <- .rls_dataset_subset_original_rows(dataset, selected_rows)
    if (!nrow(dataset$data)) stop("No rows are selected.", call. = FALSE)
  }
  id <- id %||% .rls_table1_id(dataset$group, NULL)
  # Store the exact invocation; the public verification recipe below recalculates
  # the quantities independently with base R from the exported, scoped data.
  code <- paste0("contingency_result <- LinkEDA:::.rls_mi_contingency_build(dataset, ",
    .rls_r_character_vector(row_variables), ", ", .rls_r_string_literal(column_variable),
    ", ", .rls_r_string_literal(id), ", display_mode=", .rls_r_string_literal(display_mode), ")")
  env <- new.env(parent = environment())
  env$dataset <- dataset
  eval(parse(text = code), envir = env)
  record <- env$contingency_result
  record$data_scope <- captured_scope
  verification <- .rls_mi_contingency_verification_r_code(record)
  record <- .rls_attach_analysis_provenance(record, code, record$title,
    output_code = list(table = "contingency_result$display_table"),
    verification_code = list(table = verification),
    verification_variables = c(row_variables, if(nzchar(column_variable %||% "")) column_variable, ".imp", ".id"))
  assign(record$id, record, envir = .rls_state$table1_tables)
  record
}

# Preserve existing MI callers while sharing the R cross-tabulation implementation.
.rls_mi_contingency_build <- function(...) .rls_contingency_build(...)

.rls_nested_contingency_record <- function(group, variables, column_variable, id,
                                          selected_rows=NULL, display_mode="count_percent",
                                          scope_description="All observations") {
  dataset <- .rls_dataset_record(group)
  .rls_store_data_version(dataset)
  scope <- .rls_capture_analysis_scope(dataset, if(is.null(selected_rows)) "all" else "selected",
    if(is.null(selected_rows)) integer() else selected_rows)
  scope$description <- scope_description
  if (!is.null(selected_rows)) dataset <- .rls_dataset_subset_original_rows(dataset, selected_rows)
  code <- paste0("contingency_result <- LinkEDA:::.rls_contingency_build(dataset, ",
    .rls_r_character_vector(variables), ", ", .rls_r_string_literal(column_variable), ", ",
    .rls_r_string_literal(id), ", display_mode=", .rls_r_string_literal(display_mode), ")")
  env <- new.env(parent=environment()); env$dataset <- dataset
  eval(parse(text=code),env); record <- env$contingency_result; record$data_scope <- scope
  recipe <- .rls_mi_contingency_verification_r_code(record)
  record <- .rls_attach_analysis_provenance(record,code,record$title,
    output_code=list(table="contingency_result$display_table"), verification_code=list(table=recipe),
    verification_variables=c(variables,if(nzchar(column_variable)) column_variable,
      if(record$analysis_backend=="multiple_imputation") c(".imp",".id")))
  assign(id,record,envir=.rls_state$table1_tables)
  structure(list(id=id,dataset_id=group,group=group),class="rlispstat_table1")
}
