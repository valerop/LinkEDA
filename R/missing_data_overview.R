# Missingness analyses share ordinary Table 1, binary GLM and data-version engines.
# The retained original frame, never reconstructed from completed values, is authoritative.
.rls_missing_data_source <- function(dataset=NULL, source=c("original", "active"), rows=NULL) {
  record <- .rls_dataset_record(dataset)
  scope_snapshot <- .rls_capture_analysis_scope(record, if(is.null(rows)) "all" else "selected", rows)
  rows <- if(scope_snapshot$kind=="all") NULL else scope_snapshot$rows
  source <- match.arg(source)
  mi <- .rls_mi_is_dataset(record)
  data <- if (mi && source == "original") record$original_data else record$data
  if (!is.data.frame(data)) stop("Original incomplete data are unavailable.", call.=FALSE)
  ids <- record$original_row_ids %||% seq_len(nrow(data))
  keep <- if (is.null(rows)) seq_len(nrow(data)) else which(ids %in% rows)
  if (!length(keep)) stop("The analysis scope contains no observations.", call.=FALSE)
  list(record=record, data=data[keep,,drop=FALSE], positions=keep,
       source=if(mi && source=="original") "original" else "active",
       version=record$data_version %||% 1L,
       scope=scope_snapshot)
}

.rls_missing_data_capabilities <- function(dataset=NULL) {
  record <- .rls_dataset_record(dataset)
  original <- if (.rls_mi_is_dataset(record)) record$original_data else record$data
  missing <- is.data.frame(original) && anyNA(original)
  list(overview=is.data.frame(original), models=!.rls_mi_is_dataset(record) && missing && any(vapply(original,function(x) anyNA(x) && any(!is.na(x)),logical(1L))),
       imputation=anyNA(record$data) && !.rls_mi_is_dataset(record),
       diagnostics=.rls_mi_is_dataset(record) && inherits(record$mids_object,"mids") &&
         length(record$completed_datasets)>0L && is.data.frame(record$original_data))
}

# Canonical bit signatures determine stable labels independently of row/scope order.
# Counts determine display order only: complete first, then frequency, then amount.
.rls_missing_patterns <- function(data, variables) {
  variables <- unique(as.character(variables))
  if (!length(variables) || !all(variables %in% names(data)))
    stop("Select at least one existing pattern variable.",call.=FALSE)
  mask <- is.na(data[variables])
  keys <- apply(mask,1L,function(x) paste(as.integer(x),collapse=""))
  signatures <- sort(unique(keys))
  labels <- paste0("P",seq_along(signatures))
  index <- match(keys,signatures)
  n <- tabulate(index,nbins=length(labels))
  bits <- lapply(strsplit(signatures,"",fixed=TRUE), function(x) x=="1")
  amount <- vapply(bits,sum,integer(1L))
  summary <- data.frame(Pattern=labels,N=n,`%`=100*n/nrow(data),
    `Missing variables`=vapply(bits,function(x) if(any(x)) paste(variables[x],collapse=", ") else "none",character(1L)),
    `Number of missing cells`=n*amount,check.names=FALSE)
  summary <- summary[order(amount!=0,-n,amount,signatures),,drop=FALSE]
  pattern <- factor(labels[index],levels=summary$Pattern)
  attr(pattern,"pattern_labels") <- setNames(summary$`Missing variables`,summary$Pattern)
  list(pattern=pattern,summary=summary,n_missing=rowSums(mask),pct_missing=100*rowMeans(mask),mask=mask,variables=variables)
}

.rls_little_mcar <- function(data, metadata=NULL) {
  note <- "A significant result provides evidence against MCAR. A non-significant result does not prove MCAR. This test does not establish MAR; MAR cannot be verified solely from observed data."
  unavailable <- function(reason) list(result=NULL,note=paste(reason,note))
  if (!anyNA(data)) return(unavailable("Little's MCAR test: no missing values in the selected variables."))
  continuous <- vapply(names(data),function(v) is.numeric(data[[v]]) &&
    identical(.rls_metadata_type(metadata,v,.rls_variable_type(data[[v]])),"numeric"),logical(1L))
  if (!all(continuous)) return(unavailable("Little's MCAR test was not run: select only continuous Numeric variables; categorical, ordinal and text variables are not converted to numeric codes."))
  if (ncol(data)<2L || ncol(data)>50L || any(vapply(data,function(x) sum(!is.na(x))<2L || !is.finite(stats::var(x,na.rm=TRUE)) || stats::var(x,na.rm=TRUE)<=0,logical(1L))))
    return(unavailable("Little's MCAR test was not run: requires 2\u201350 nonconstant numeric variables with at least two observed values each."))
  if (any(vapply(data,function(x) any(!is.finite(x) & !is.na(x)),logical(1L))))
    return(unavailable("Little's MCAR test was not run: infinite values are present."))
  if (!requireNamespace("naniar",quietly=TRUE)) return(unavailable("Install R package 'naniar' to run Little's MCAR test."))
  tryCatch({
    result <- naniar::mcar_test(data)
    if (!all(is.finite(unlist(result))) || result$df<=0) return(unavailable("Little's MCAR test is undefined for these data."))
    list(result=result,note=paste("Little's MCAR test: naniar::mcar_test, using a multivariate-normal model for continuous data.",note))
  },error=function(e) unavailable(paste("Little's MCAR test could not be estimated:",conditionMessage(e))))
}

.rls_missing_snapshot <- function(input, data, purpose, reuse = NULL, update_code = "") {
  snapshot <- if (!is.null(reuse)) get0(reuse, envir=.rls_state$datasets, inherits=FALSE) else NULL
  if (!is.null(snapshot) && !identical(snapshot$missingness_source$dataset, input$record$group))
    stop("The missingness snapshot belongs to a different dataset.", call.=FALSE)
  existing <- !is.null(snapshot)
  changed <- !existing || !identical(as.list(snapshot$data), as.list(data)) ||
    !identical(snapshot$missingness_source$version, input$version)
  if (!existing) {
    name <- .rls_unique_dataset_name(paste(input$record$group,purpose))
    id <- .rls_register_dataset(name,data,source=paste("Missingness analysis of",input$record$group,
      "version",input$version,"\u2014",input$source,"data"),activate=FALSE)
    snapshot <- .rls_dataset_record(id)
  } else if (changed) {
    snapshot$data <- snapshot$data_frame <- data
    snapshot$n_rows <- nrow(data); snapshot$n_columns <- ncol(data)
    snapshot <- .rls_advance_data_version(snapshot, "Update missingness pattern specification",
      code=update_code, origin=if(nzchar(update_code)) "recorded" else "unavailable")
  }
  snapshot$stable_row_ids <- (input$record$stable_row_ids %||% paste0(input$record$group,":row:",seq_len(nrow(input$record$data))))[input$positions]
  # Keep semantic types (including numeric-backed categorical columns).
  inherited <- input$record$variable_metadata
  if(is.data.frame(inherited)) {
    for(v in names(data))
      snapshot$variable_metadata <- .rls_refresh_metadata_row(snapshot$variable_metadata,data,v,
        .rls_metadata_type(inherited,v,.rls_variable_type(data[[v]])))
    snapshot$metadata <- snapshot$variable_metadata
  }
  snapshot$missingness_source <- list(dataset=input$record$group,version=input$version,source=input$source,scope=input$scope)
  # One prepared file per actual snapshot version is shared by its report pages.
  # Previous versions remain immutable for previously returned/exported results.
  attr(snapshot$data, "linkeda_row_ids") <- snapshot$stable_row_ids
  snapshot$data_frame <- snapshot$data
  source <- snapshot$missingness_prepared_path %||% ""
  if (changed || !file.exists(source)) {
    source <- tempfile("linkeda-missingness-", fileext=".rds")
    saveRDS(snapshot$data, source)
  }
  snapshot$missingness_prepared_path <- source
  .rls_set_dataset_record(snapshot); .rls_store_data_version(snapshot)
  # This prepared frame belongs to the analysis. Keep it available for linked
  # tables, refits and exports without opening an unsolicited data sheet.
  if (isTRUE(.rls_state$process_started)) {
    active <- .rls_state$active_dataset
    .rls_send(c("REGISTER_DATASET_SILENT", snapshot$group,
      .rls_variable_payload(snapshot$data, snapshot$variable_metadata),
      .rls_dataframe_payload(snapshot$data, snapshot$variable_metadata, dataset_record=snapshot)))
    if (length(active) && nzchar(active)) .rls_send(c("SET_ACTIVE_DATASET", active))
  }
  snapshot
}

.rls_missing_pattern_recipe <- function(variables) paste(
  "analysis_data <- readRDS(verification_data_path)",
  paste0("pattern_variables <- ",.rls_r_character_vector(variables)),
  "mask <- is.na(analysis_data[pattern_variables])",
  "signature <- apply(mask, 1, function(x) paste(as.integer(x), collapse=''))",
  "signatures <- sort(unique(signature))",
  "pattern <- factor(paste0('P', match(signature, signatures)), levels=paste0('P', seq_along(signatures)))",
  "counts <- as.data.frame(table(pattern))",
  "counts$percent <- 100 * counts$Freq / nrow(analysis_data)",
  "counts$missing_variables <- vapply(strsplit(signatures, ''), function(x) paste(pattern_variables[x=='1'], collapse=', '), character(1))",
  "counts$missing_cells <- counts$Freq * vapply(strsplit(signatures, ''), function(x) sum(x=='1'), integer(1))",
  "print(counts)",sep="\n")

.rls_missing_summary_table <- function(snapshot, frame, notes, recipe,
    title="Missingness patterns", id_prefix="missing_overview:", table_type="missing_data_overview",
    output="counts", implementation="# Missingness summaries computed in R with is.na(), table() and rowSums().") {
  display <- as.data.frame(lapply(frame,function(x) if(is.numeric(x))
    vapply(x,function(v) format(round(v,3),trim=TRUE),character(1L)) else as.character(x)),check.names=FALSE)
  names(display)[1L] <- "Variable"
  rows <- lapply(seq_len(nrow(display)),function(i) list(row_index=i,row_type="text",variable=display$Variable[i],level="",
    values=as.list(display[i,-1,drop=FALSE]),p=if("p" %in% names(display)) display$p[i] else "",test="",detail=""))
  record <- list(id=paste0(id_prefix,snapshot$group),dataset_id=snapshot$group,group=snapshot$group,
    title=title,variables=names(snapshot$data),variable_types=character(),
    table_type=table_type,display_table=display,summary_statistics=rows,data=snapshot$data,
    footnotes=notes,display_options=list(show_p="p" %in% names(display),show_test=FALSE),
    data_scope=list(kind="all",description="Missingness analysis snapshot",total_n=nrow(snapshot$data),rows=integer()))
  record <- .rls_attach_analysis_provenance(record,implementation,
    record$title,output_code=list(table=output),verification_code=list(table=recipe),verification_variables=names(snapshot$data))
  source <- snapshot$missingness_prepared_path %||% ""
  if (!file.exists(source)) {
    source <- tempfile("linkeda-missingness-",fileext=".rds"); saveRDS(snapshot$data,source)
  }
  record$analysis_provenance$prepared_data_path <- source
  .rls_assign_table1(record)
  record
}

.rls_missing_little_table <- function(snapshot, little, variables, source_note) {
  recipe <- paste("analysis_data <- readRDS(verification_data_path)",
    paste0("test_data <- analysis_data[", .rls_r_character_vector(variables), "]"), sep="\n")
  if (!is.null(little$result)) {
    frame <- data.frame(Test="Little's MCAR test",
      statistic=sprintf("%.3f", little$result$statistic),
      df=as.character(little$result$df),
      p=.rls_export_format_p(little$result$p.value), check.names=FALSE)
    names(frame)[[2L]] <- "\u03C7\u00B2"
    recipe <- paste(recipe, "little_test <- naniar::mcar_test(test_data)", "print(little_test)", sep="\n")
  } else {
    frame <- data.frame(Test="Little's MCAR test", Status="Not calculated", check.names=FALSE)
    recipe <- paste(recipe, paste0("# ", little$note), "str(test_data)", sep="\n")
  }
  .rls_missing_summary_table(snapshot, frame,
    c(source_note, paste("Variables:", paste(variables, collapse=", ")), little$note), recipe,
    title="Little's MCAR test", id_prefix="missing_little:", table_type="missing_data_test",
    output=if(is.null(little$result)) "test_data" else "little_test",
    implementation=if(is.null(little$result)) "# Little's MCAR test was not calculated; see eligibility notes."
      else "little_test <- naniar::mcar_test(test_data)")
}

# Build and update the interactive overview from its immutable source and scope.
.rls_missing_build_overview <- function(input, variables, id=NULL, little_open=FALSE,
    descriptors=character(), native=FALSE) {
  variables <- unique(variables)
  patterns <- .rls_missing_patterns(input$data, variables)
  pattern_name <- make.unique(c(names(input$data), "missing_pattern"), sep="_")[[ncol(input$data)+1L]]
  prepared <- input$data; prepared[[pattern_name]] <- patterns$pattern
  previous <- if (!is.null(id)) get0(id, envir=.rls_state$table1_tables, inherits=FALSE) else NULL
  update_code <- paste(sub("analysis_data <- readRDS(verification_data_path)",
    "analysis_data <- data", .rls_missing_pattern_recipe(variables), fixed=TRUE),
    paste0("pattern <- factor(as.character(pattern), levels=",
      .rls_r_character_vector(levels(patterns$pattern)), ")"),
    paste0("attr(pattern, 'pattern_labels') <- stats::setNames(",
      .rls_r_character_vector(unname(attr(patterns$pattern, "pattern_labels"))), ", levels(pattern))"),
    paste0("data[[", .rls_r_character_vector(pattern_name), "]] <- pattern"), sep="\n")
  snapshot <- .rls_missing_snapshot(input, prepared, "missingness overview", reuse=previous$group,
    update_code=update_code)
  source_note <- paste("Source:", input$record$group, "\u2014", input$source, "data; version", input$version,
    "\u2014", input$scope$description, sprintf("(scope N = %d of %d)", input$scope$n, input$scope$total_n))
  labels <- as.character(patterns$summary$Pattern)
  display <- data.frame(Variable=c(variables, "Observations", "% of observations"), check.names=FALSE)
  pattern_rows <- setNames(lapply(labels, function(label) which(patterns$pattern==label)), labels)
  for (label in labels) {
    pos <- pattern_rows[[label]][1L]
    display[[label]] <- c(ifelse(is.na(input$data[pos,variables,drop=FALSE]), "\u2014", "\u2022"),
      as.character(length(pattern_rows[[label]])), sprintf("%.1f", 100*length(pattern_rows[[label]])/nrow(input$data)))
  }
  recipe <- paste(.rls_missing_pattern_recipe(variables),
    "missingness_matrix <- matrix(unlist(lapply(signatures, function(key) ifelse(strsplit(key, '')[[1]]=='1', '\u2014', '\u2022')), use.names=FALSE),",
    "  nrow=length(pattern_variables), ncol=length(signatures),",
    "  dimnames=list(pattern_variables, paste0('P', seq_along(signatures))))",
    "print(missingness_matrix)", sep="\n")
  summary <- .rls_missing_summary_table(snapshot, display,
    c(source_note, "\u2022 Observed   \u2014 Missing. Click a pattern to select its observations; right-click for descriptives.",
      "Add or remove variables here. Patterns, descriptives and an open Little test update together."), recipe,
    output="missingness_matrix")
  if (!is.null(id)) { rm(list=summary$id, envir=.rls_state$table1_tables); summary$id <- id }
  summary$variables <- variables
  summary$missingness_layout <- list(source_dataset=input$record$group,
    available_variables=setdiff(names(input$data), variables),
    pattern_rows=lapply(pattern_rows, function(rows) as.integer((input$record$original_row_ids %||% seq_len(nrow(input$record$data)))[input$positions[rows]])))
  for (i in seq_along(summary$summary_statistics)) {
    summary$summary_statistics[[i]]$row_type <- if(i<=length(variables)) "text" else "nested_total"
    summary$summary_statistics[[i]]$variable <- if(i<=length(variables)) variables[i] else ""
  }
  little <- NULL; little_table <- NULL
  if (little_open) {
    continuous <- variables[vapply(variables, function(v) is.numeric(input$data[[v]]) &&
      identical(.rls_metadata_type(input$record$variable_metadata,v,.rls_variable_type(input$data[[v]])),"numeric"), logical(1))]
    little <- if(length(continuous)<2L) list(result=NULL, note="Little's MCAR test requires at least two continuous Numeric variables in the patterns table.")
      else .rls_little_mcar(input$data[continuous], input$record$variable_metadata)
    excluded <- setdiff(variables,continuous)
    if(length(excluded)) little$note <- paste(little$note, "Excluded non-continuous variables:", paste(excluded,collapse=", "))
    little_table <- .rls_missing_little_table(snapshot,little,continuous,source_note)
    rm(list=little_table$id,envir=.rls_state$table1_tables)
    little_table$id <- paste0(summary$id,":little")
    .rls_assign_table1(little_table)
    if(native) .rls_table1_sync_native(little_table)
  }
  descriptive <- NULL
  for (pattern in descriptors) {
    rows <- if(pattern=="all") seq_len(nrow(prepared)) else pattern_rows[[pattern]]
    if(is.null(rows)) rows <- integer()
    if(!length(rows)) {
      tab <- .rls_missing_summary_table(snapshot,
        data.frame(Variable=pattern,Status="This pattern is no longer present."),
        source_note,recipe,title=paste("Descriptives \u2014",pattern),table_type="missing_data_test")
      rm(list=tab$id,envir=.rls_state$table1_tables)
      tab$id <- paste0(summary$id,":descriptives:",pattern)
      .rls_assign_table1(tab)
      if(native) .rls_table1_sync_native(tab)
      next
    }
    types <- setNames(vapply(variables,function(v) .rls_table1_infer_type(input$record,v),character(1)),variables)
    # Text is described categorically; no numeric conversion or inference is applied.
    types[types=="unsupported"] <- "categorical"
    descriptive <- ls_new_table1(snapshot$group,variables,group=if(pattern=="all") pattern_name else NULL,
      variable_types=types, show_p=FALSE, show_test=FALSE, native=FALSE,
      name=paste0(summary$id,":descriptives:",pattern), .selected_rows=rows,
      .scope_description=if(pattern=="all") "All missingness patterns" else paste("Missingness pattern",pattern))
    tab <- .rls_table1_record(descriptive)
    tab$table_type <- "missingness_descriptives"
    tab$title <- if(pattern=="all") "Descriptives by missingness pattern" else paste("Descriptives \u2014",pattern)
    tab$footnotes <- c(source_note,tab$footnotes)
    .rls_assign_table1(tab)
    if(native) .rls_table1_sync_native(tab)
  }
  summary$missingness_context <- list(input=input,variables=variables,little_open=little_open,descriptors=descriptors)
  .rls_assign_table1(summary)
  if(native) .rls_table1_sync_native(summary)
  invisible(structure(list(input=input,patterns=patterns,little=little,little_table=little_table,
    summary=summary,descriptive=descriptive),class="linkeda_missingness_overview"))
}

#' Examine missingness patterns with one shared set of variables
#' @export
ls_missing_data_overview <- function(dataset=NULL, pattern_variables=NULL, descriptive_variables=NULL,
    source=c("original","active"), .selected_rows=NULL, save=character(), native=isTRUE(.rls_state$process_started)) {
  input <- .rls_missing_data_source(dataset,source,.selected_rows)
  # Keep the old argument for API compatibility; descriptors now always match patterns.
  result <- .rls_missing_build_overview(input,pattern_variables %||% names(input$data),native=native)
  if(length(save)) ls_save_missing_data(result,save)
  invisible(result)
}

.rls_missing_pattern_action <- function(id, action, value="", native=TRUE) {
  summary <- .rls_table1_record(id)
  context <- summary$missingness_context
  if(is.null(context)) stop("Reopen Missing Data Overview to edit this report.",call.=FALSE)
  variables <- context$variables
  if(action=="add") variables <- unique(c(variables,value))
  else if(action=="remove") variables <- setdiff(variables,value)
  else if(action=="little") context$little_open <- TRUE
  else if(action=="descriptives") context$descriptors <- unique(c(context$descriptors,if(nzchar(value)) value else "all"))
  else if(action=="save") {
    result <- structure(list(input=context$input,patterns=.rls_missing_patterns(context$input$data,variables)),class="linkeda_missingness_overview")
    saved <- ls_save_missing_data(result,value)
    context$input$version <- .rls_dataset_record(context$input$record$group)$data_version
    summary$missingness_context <- context
    .rls_assign_table1(summary)
    if(native) .rls_table1_sync_native(summary)
    return(invisible(saved))
  } else stop("Unknown missingness action.",call.=FALSE)
  if(!length(variables)) stop("Keep at least one variable in the patterns table.",call.=FALSE)
  .rls_missing_build_overview(context$input,variables,id,context$little_open,context$descriptors,native)
}

#' Save missingness results as ordinary linked data columns
#' @export
ls_save_missing_data <- function(result, what=c("pattern","n_missing","pct_missing","indicators")) {
  input <- result$input
  target <- .rls_dataset_record(input$record$group)
  if(!identical(as.integer(target$data_version %||% 1L),as.integer(input$version)))
    stop("The dataset changed after this analysis. Recalculate before saving.",call.=FALSE)
  values <- list(); labels <- list()
  if(inherits(result,"linkeda_missingness_overview")) {
    what <- match.arg(what,c("pattern","n_missing","pct_missing","indicators"),several.ok=TRUE)
    p <- result$patterns
    if("pattern" %in% what) { values$missing_pattern <- p$pattern; labels$missing_pattern <- paste(names(attr(p$pattern,"pattern_labels")),attr(p$pattern,"pattern_labels"),sep=" = ",collapse="; ") }
    if("n_missing" %in% what) values$n_missing <- p$n_missing
    if("pct_missing" %in% what) values$pct_missing <- p$pct_missing
    if("indicators" %in% what) for(v in p$variables) values[[paste0(v,"_missing")]] <- is.na(input$data[[v]])
    code <- sub("analysis_data <- readRDS(verification_data_path)","analysis_data <- data",.rls_missing_pattern_recipe(p$variables),fixed=TRUE)
    inputs <- p$variables
  } else {
    what <- match.arg(what,c("indicator","probability"),several.ok=TRUE)
    if("indicator" %in% what) values[[paste0(result$target,"_missing")]] <- is.na(input$data[[result$target]])
    if("probability" %in% what) values[[paste0(result$target,"_missing_probability")]] <- result$probability
    code <- sub("analysis_data <- readRDS(verification_data_path)","analysis_data <- data",result$recipe,fixed=TRUE); inputs <- c(result$target,result$predictors)
  }
  occupied <- unique(c(names(target$data),if(inherits(target$mids_object,"mids"))names(target$mids_object$data)))
  new_names <- tail(make.unique(c(occupied,names(values)),sep="_"),length(values))
  added <- data.frame(row.names=seq_len(nrow(target$data)))
  for(i in seq_along(values)) {
    v <- values[[i]]; full <- v[rep(NA_integer_,nrow(target$data))]; full[input$positions] <- v
    if(!is.null(attr(v,"pattern_labels"))) attr(full,"pattern_labels") <- attr(v,"pattern_labels")
    added[[new_names[i]]] <- full
  }
  append <- function(data) { for(v in new_names) data[[v]] <- added[[v]]; data }
  target$data <- append(target$data); target$data_frame <- target$data
  if(.rls_mi_is_dataset(target)) {
    target$original_data <- append(target$original_data)
    target$completed_datasets <- lapply(target$completed_datasets,append)
    if(inherits(target$mids_object,"mids")) target$mids_object <- mice::cbind(target$mids_object,added)
    if(is.data.frame(target$import_name_map)) {
      mapping <- target$import_name_map[rep(NA_integer_,length(new_names)),,drop=FALSE]
      mapping$original_name <- mapping$variable_name <- new_names
      target$import_name_map <- rbind(target$import_name_map,mapping)
    }
    target$missing_cell_mask <- as.data.frame(lapply(target$original_data,is.na),check.names=FALSE)
  }
  metadata <- target$variable_metadata
  for(i in seq_along(new_names)) {
    v <- new_names[i]; type <- if(is.numeric(added[[v]])) "numeric" else "factor"
    metadata <- .rls_refresh_metadata_row(metadata,target$data,v,type)
    if(length(labels[[names(values)[i]]]) && "description" %in% names(metadata))
      metadata$description[metadata$name==v] <- labels[[names(values)[i]]]
  }
  target$metadata <- target$variable_metadata <- metadata
  target$n_columns <- ncol(target$data); target$modified <- TRUE
  recipes <- if(inherits(result,"linkeda_missingness_overview")) {
    vapply(names(values),function(v) {
      if(v=="missing_pattern") "pattern" else if(v=="n_missing") "rowSums(mask)" else if(v=="pct_missing") "100 * rowMeans(mask)" else
        paste0("is.na(analysis_data[[",.rls_r_string_literal(sub("_missing$","",v)),"]])")
    },character(1L))
  } else vapply(names(values),function(v) if(endsWith(v,"_probability"))
    "stats::predict(fit, newdata=analysis_data, type='response', na.action=stats::na.pass)" else
    paste0("is.na(analysis_data[[",.rls_r_string_literal(result$target),"]])"),character(1L))
  code <- sub("analysis_data <- data",paste0("analysis_data <- data[c(",paste(input$positions,collapse=","),"), , drop=FALSE]"),code,fixed=TRUE)
  code <- paste(code,paste(vapply(seq_along(new_names),function(i)
    paste0("saved <- ",recipes[i],"; data[[",.rls_r_string_literal(new_names[i]),"]] <- saved[rep(NA_integer_, nrow(data))]; data[[",
      .rls_r_string_literal(new_names[i]),"]][c(",paste(input$positions,collapse=","),")] <- saved"),character(1L)),collapse="\n"),sep="\n")
  target <- .rls_advance_data_version(target,"Save missingness information",code=code,
    input_columns=inputs,output_columns=new_names,stable_row_ids=target$stable_row_ids[input$positions])
  .rls_set_dataset_record(target)
  if(.rls_mi_is_dataset(target) && nzchar(target$imputation_id %||% "") &&
      exists(target$imputation_id,envir=.rls_state$missing_imputations,inherits=FALSE)) {
    imputation <- .rls_imputation_record(target$imputation_id)
    for(field in c("original_data","completed_datasets","mids_object","missing_cell_mask","variable_metadata","import_name_map"))
      imputation[[field]] <- target[[field]]
    imputation$imputed_cell_map <- .rls_mi_build_cell_map(target$original_data,target$completed_datasets,target$missing_cell_mask)
    .rls_set_imputation_record(imputation)
  }
  .rls_notify_dataset_changed(target)
  invisible(new_names)
}

#' Fit a binary logistic model of a variable's missingness
#' @export
ls_missingness_model <- function(dataset=NULL,target,predictors,source=c("original","active"),
    .selected_rows=NULL,save=character(),native=isTRUE(.rls_state$process_started)) {
  if(.rls_mi_is_dataset(.rls_dataset_record(dataset)))
    stop("Missingness models are disabled for imputed datasets. Open the original incomplete dataset to model missingness.",call.=FALSE)
  input <- .rls_missing_data_source(dataset,source,.selected_rows)
  if(!target %in% names(input$data) || !all(predictors %in% names(input$data))) stop("Unknown variable.",call.=FALSE)
  if(target %in% predictors) stop("The target itself cannot predict its own missingness.",call.=FALSE)
  indicator <- is.na(input$data[[target]])
  if(length(unique(indicator))!=2L) stop("The target must have both missing and observed values in this scope.",call.=FALSE)
  response <- make.unique(c(names(input$data),paste0(target,"_missing")),sep="_")[[ncol(input$data)+1L]]
  data <- input$data; data[[response]] <- factor(ifelse(indicator,"Missing","Observed"),levels=c("Observed","Missing"))
  snapshot <- .rls_missing_snapshot(input,data,paste("missingness of",target))
  model <- ls_new_binary_regression(snapshot$group,response,terms=predictors,event="Missing",reference="Observed",native=FALSE)
  record <- .rls_generalized_glm_record(model)
  probability <- as.numeric(stats::predict(record$fit,newdata=data,type="response",na.action=stats::na.pass))
  recipe <- paste("analysis_data <- readRDS(verification_data_path)",
    paste0("analysis_data[[",.rls_r_string_literal(response),"]] <- as.integer(is.na(analysis_data[[",.rls_r_string_literal(target),"]]))"),
    paste0("fit <- stats::glm(",paste(deparse(.rls_model_formula_object(response,predictors)),collapse=" "),", data=analysis_data, family=stats::binomial('logit'))"),
    "summary(fit)","exp(cbind(OR=coef(fit), confint.default(fit)))",
    "stats::drop1(fit, test='Chisq')",sep="\n")
  record$missingness_analysis <- list(target=target,source=input$source,source_dataset=input$record$group,
    source_version=input$version,scope=input$scope)
  record <- .rls_missingness_attach_model_provenance(record)
  record$native_sync_enabled <- isTRUE(native)
  .rls_assign_generalized_glm(record)
  if(isTRUE(native)) .rls_generalized_glm_sync_native(record)
  result <- structure(list(input=input,model=model,target=target,predictors=predictors,probability=probability,recipe=recipe),class="linkeda_missingness_model")
  if(length(save)) ls_save_missing_data(result,save)
  invisible(result)
}

.rls_handle_missing_data_workflow <- function(kind,group,target,source,variables,descriptions,save,version,scope,rows) {
  current <- .rls_dataset_record(group)
  if(as.character(current$data_version %||% 1L)!=version) stop("The dataset changed while this dialog was open. Open it again.",call.=FALSE)
  split <- function(x) if(nzchar(x)) strsplit(x,"\x1f",fixed=TRUE)[[1L]] else character()
  selected <- if(scope=="all") current$original_row_ids %||% seq_len(nrow(current$data)) else as.integer(rows)
  if(kind=="missing_data_overview") ls_missing_data_overview(group,variables,split(descriptions),source,
    .selected_rows=selected,save=split(save),native=TRUE)
  else ls_missingness_model(group,target,variables,source,.selected_rows=selected,save=split(save),native=TRUE)
}

.rls_missingness_attach_model_provenance <- function(record) {
  if(is.null(record$missingness_analysis)) return(record)
  target <- record$missingness_analysis$target; response <- record$response
  data <- record$data; predictors <- unique(all.vars(.rls_model_formula_object(response,record$terms)))
  indicator_code <- paste0("analysis_data[[",.rls_r_string_literal(response),"]] <- factor(ifelse(is.na(analysis_data[[",.rls_r_string_literal(target),"]]), 'Missing', 'Observed'), levels=c('Observed','Missing'))")
  record$analysis_provenance$verification_r_code <- lapply(record$analysis_provenance$verification_r_code,function(x) {
    lines <- strsplit(x,"\n",fixed=TRUE)[[1L]]
    at <- grep("analysis_data <- base::readRDS|analysis_data <- readRDS",lines)
    if(length(at)) lines <- append(lines,indicator_code,after=at[[1L]])
    paste(c(lines,"# Models associations with missingness; this does not establish MAR."),collapse="\n")
  })
  source_path <- tempfile("linkeda-missingness-model-",fileext=".rds");saveRDS(data,source_path)
  record$analysis_provenance$prepared_data_path <- source_path
  record$analysis_provenance$verification_variables <- unique(c(target,response,predictors))
  record$status <- paste("Missingness model:",target,"(Missing versus Observed).",
    "Associations do not establish MAR. Predictors with missing values are excluded.",record$status)
  record
}
