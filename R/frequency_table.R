# Frequency summaries use the same versioned R transport and provenance as Table 1.
.rls_frequency_build <- function(dataset, variable, id) {
  if (length(variable) != 1L || !variable %in% names(dataset$data))
    stop("Choose one variable for the frequency table.", call. = FALSE)
  mi <- identical(dataset$dataset_type, "multiple_imputation")
  completed <- if (mi) .rls_mi_completed_datasets(dataset) else list(dataset$data)
  factors <- lapply(completed, function(data) .rls_table1_display_factor(data[[variable]], "categorical"))
  category_levels <- unique(unlist(lapply(factors, levels), use.names = FALSE))
  summaries <- lapply(factors, function(x) {
    counts <- base::table(factor(x, levels = category_levels), useNA = "no")
    valid <- sum(counts)
    list(counts = c(valid, as.numeric(counts), sum(is.na(x))),
      percentages = c(if(valid > 0) 1 else NA_real_, as.numeric(base::prop.table(counts)),
        if(length(x)) mean(is.na(x)) else NA_real_))
  })
  mean_counts <- rowMeans(do.call(cbind, lapply(summaries, `[[`, "counts")))
  mean_percentages <- rowMeans(do.call(cbind, lapply(summaries, `[[`, "percentages")), na.rm = TRUE)
  mean_percentages[!is.finite(mean_percentages)] <- NA_real_
  labels <- c("Valid N", category_levels, "Missing")
  rows <- lapply(seq_along(labels), function(i) {
    category <- i > 1L && i < length(labels)
    list(row_index = i, row_type = if(category) "frequency_level" else if(i==1L) "n" else "missing",
      variable = variable, level = if(category) category_levels[[i-1L]] else "",
      values = list(N = .rls_export_format_number(mean_counts[[i]], if(mi) 1L else 0L),
        Percent = .rls_export_format_percent(mean_percentages[[i]])),
      raw_statistics = list(N = list(value = mean_counts[[i]]), Percent = list(value = 100 * mean_percentages[[i]])),
      p = "", test = "", detail = "")
  })
  display <- data.frame(Variable=labels, N=vapply(rows,function(r) r$values$N,character(1)),
    Percent=vapply(rows,function(r) r$values$Percent,character(1)),check.names=FALSE)
  structure(list(id=id,dataset_id=dataset$group,group=dataset$group,
    title=paste0("Frequency table: ",variable,if(mi) " \u2014 Multiple Imputation" else ""),
    variables=variable,group_variable="",variable_types=stats::setNames(list("categorical"),variable),
    table_type="frequency",row_variables=variable,category_levels=category_levels,
    value_labels=.rls_table1_value_labels(completed[[1L]][[variable]]),
    summary_statistics=rows,display_table=display,mean_counts=mean_counts,mean_percentages=mean_percentages,
    analysis_backend=if(mi) "multiple_imputation" else "ordinary",imputation_count=if(mi) length(completed) else 0L,
    display_options=list(show_p=FALSE,show_test=FALSE),
    footnotes=c("Category percentages use valid cases; the Missing percentage uses all cases in the analysis scope.",
      if(mi) sprintf("Multiple imputation: m = %d. Counts and percentages are calculated in each completion and averaged. Percentages omit completions without a defined denominator; these are descriptive averages, not Rubin-pooled estimates.",length(completed)))),
    class="rlispstat_table1_record")
}

.rls_frequency_record <- function(group, variable, id, selected_rows=NULL, scope_description="All observations") {
  dataset <- .rls_dataset_record(group)
  .rls_store_data_version(dataset)
  scope <- .rls_capture_analysis_scope(dataset,if(is.null(selected_rows)) "all" else "selected",
    if(is.null(selected_rows)) integer() else selected_rows)
  scope$description <- scope_description
  if(!is.null(selected_rows)) {
    dataset <- .rls_dataset_subset_original_rows(dataset,selected_rows)
    if(!nrow(dataset$data)) stop("No rows are selected.",call.=FALSE)
  }
  code <- paste0("frequency_result <- LinkEDA:::.rls_frequency_build(dataset, ",
    .rls_r_string_literal(variable),", ",.rls_r_string_literal(id),")")
  env <- new.env(parent=environment());env$dataset <- dataset
  eval(parse(text=code),env);record <- env$frequency_result;record$data_scope <- scope
  recipe <- .rls_frequency_verification_r_code(record)
  record <- .rls_attach_analysis_provenance(record,code,record$title,
    output_code=list(table="frequency_result$display_table"), verification_code=list(table=recipe),
    verification_variables=c(variable,if(record$analysis_backend=="multiple_imputation") c(".imp",".id")))
  assign(id,record,envir=.rls_state$table1_tables)
  structure(list(id=id,dataset_id=group,group=group),class="rlispstat_table1")
}
