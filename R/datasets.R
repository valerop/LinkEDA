#' Register, list, and activate LinkEDA datasets
#'
#' The dataset registry is R-side state used by application-style workbench
#' commands. Native windows receive data through plot commands, while R keeps
#' the original data frame and row identity.
#'
#' @param name Dataset name and default linked brushing group id.
#' @param data A data frame.
#' @param envir Environment scanned for data frames.
#' @return See individual functions.
#' @export
ls_datasets <- function() {
  names <- ls(envir = .rls_state$datasets, all.names = FALSE)
  if (!length(names)) {
    return(data.frame(
      name = character(), group = character(), rows = integer(),
      variables = integer(), source = character(), active = logical(),
      stringsAsFactors = FALSE
    ))
  }
  records <- lapply(names, function(name) get(name, envir = .rls_state$datasets))
  data.frame(
    dataset_id = vapply(records, `[[`, character(1L), "dataset_id"),
    name = vapply(records, `[[`, character(1L), "name"),
    group = vapply(records, `[[`, character(1L), "group"),
    rows = vapply(records, function(x) nrow(x$data), integer(1L)),
    variables = vapply(records, function(x) ncol(x$data), integer(1L)),
    n_rows = vapply(records, function(x) nrow(x$data), integer(1L)),
    n_columns = vapply(records, function(x) ncol(x$data), integer(1L)),
    source = vapply(records, `[[`, character(1L), "source"),
    path = vapply(records, function(x) as.character(x$path %||% NA_character_), character(1L)),
    active = vapply(records, function(x) identical(x$name, .rls_state$active_dataset), logical(1L)),
    stringsAsFactors = FALSE
  )
}

#' @rdname ls_datasets
#' @export
ls_register_dataset <- function(name, data) {
  if (is.data.frame(name) && is.character(data) && length(data) == 1L) {
    tmp <- name
    name <- data
    data <- tmp
  }
  .rls_register_dataset(name, data, source = "R environment", activate = TRUE)
}

.rls_safe_dataset_name <- function(name) {
  name <- tools::file_path_sans_ext(basename(name))
  name <- gsub("[\r\n\t|]+", " ", name)
  name <- gsub("[^[:alnum:]_. -]+", " ", name)
  name <- gsub("[ ]+", " ", name)
  name <- trimws(name)
  if (!nzchar(name)) {
    name <- "dataset"
  }
  name
}

.rls_unique_dataset_name <- function(name) {
  base <- .rls_safe_dataset_name(name)
  candidate <- base
  i <- 2L
  while (exists(candidate, envir = .rls_state$datasets, inherits = FALSE)) {
    candidate <- paste(base, i)
    i <- i + 1L
  }
  candidate
}

.rls_variable_label <- function(x) {
  label <- attr(x, "label", exact = TRUE)
  if (is.null(label) || !length(label) || is.na(label[[1L]])) {
    return(NA_character_)
  }
  as.character(label[[1L]])
}

.rls_variable_decimals <- function(x) {
  format <- attr(x, "format.spss", exact = TRUE)
  if (!is.null(format) && length(format) && !is.na(format[[1L]])) {
    hit <- regexec("\\.([0-9]+)", as.character(format[[1L]]))
    parts <- regmatches(as.character(format[[1L]]), hit)[[1L]]
    if (length(parts) >= 2L) {
      return(as.integer(parts[[2L]]))
    }
  }
  if (is.integer(x) || is.logical(x)) {
    return(0L)
  }
  NA_integer_
}

.rls_variable_value_labels <- function(x) {
  labels <- attr(x, "labels", exact = TRUE)
  if (is.null(labels)) {
    return(NULL)
  }
  labels
}

.rls_variable_metadata <- function(data) {
  data.frame(
    variable_name = names(data),
    name = names(data),
    display_name = names(data),
    original_class = vapply(data, function(x) paste(class(x), collapse = ", "), character(1L)),
    current_analysis_type = vapply(data, .rls_variable_type, character(1L)),
    type = vapply(data, .rls_variable_type, character(1L)),
    class = vapply(data, function(x) paste(class(x), collapse = ", "), character(1L)),
    is_numeric = vapply(data, is.numeric, logical(1L)),
    is_factor = vapply(data, is.factor, logical(1L)),
    is_character = vapply(data, is.character, logical(1L)),
    is_logical = vapply(data, is.logical, logical(1L)),
    label = vapply(data, .rls_variable_label, character(1L)),
    labels = vapply(data, .rls_variable_label, character(1L)),
    description = vapply(data, .rls_variable_label, character(1L)),
    decimals = vapply(data, .rls_variable_decimals, integer(1L)),
    missing_count = vapply(data, function(x) sum(is.na(x)), integer(1L)),
    value_labels = I(lapply(data, .rls_variable_value_labels)),
    stringsAsFactors = FALSE
  )
}

.rls_metadata_type <- function(metadata, variable, default = NULL) {
  if (is.null(metadata) || !nrow(metadata)) {
    return(default)
  }
  name_col <- if ("variable_name" %in% names(metadata)) "variable_name" else "name"
  row <- metadata[[name_col]] == variable
  if (!any(row)) {
    return(default)
  }
  type_col <- if ("current_analysis_type" %in% names(metadata)) "current_analysis_type" else "type"
  as.character(metadata[[type_col]][which(row)[[1L]]])
}

.rls_refresh_metadata_row <- function(metadata, data, variable, type = NULL) {
  if (is.null(metadata) || !nrow(metadata)) {
    metadata <- .rls_variable_metadata(data)
  }
  if (!"variable_name" %in% names(metadata)) metadata$variable_name <- metadata$name
  if (!"display_name" %in% names(metadata)) metadata$display_name <- metadata$name
  if (!"original_class" %in% names(metadata)) metadata$original_class <- metadata$class
  if (!"current_analysis_type" %in% names(metadata)) metadata$current_analysis_type <- metadata$type
  if (!"labels" %in% names(metadata)) metadata$labels <- metadata$label %||% NA_character_
  if (!"description" %in% names(metadata)) metadata$description <- metadata$label %||% NA_character_
  if (!"decimals" %in% names(metadata)) metadata$decimals <- NA_integer_
  if (!"value_labels" %in% names(metadata)) metadata$value_labels <- I(vector("list", nrow(metadata)))
  name_col <- if ("variable_name" %in% names(metadata)) "variable_name" else "name"
  row <- match(variable, metadata[[name_col]])
  if (is.na(row)) {
    metadata <- rbind(metadata, .rls_variable_metadata(data[variable]))
    row <- nrow(metadata)
  }
  current_type <- type %||% .rls_variable_type(data[[variable]])
  metadata$variable_name[[row]] <- variable
  metadata$name[[row]] <- variable
  metadata$display_name[[row]] <- metadata$display_name[[row]] %||% variable
  if (is.null(metadata$display_name[[row]]) || is.na(metadata$display_name[[row]]) || !nzchar(metadata$display_name[[row]])) {
    metadata$display_name[[row]] <- variable
  }
  if (!"original_class" %in% names(metadata) || is.na(metadata$original_class[[row]]) || !nzchar(metadata$original_class[[row]])) {
    metadata$original_class[[row]] <- paste(class(data[[variable]]), collapse = ", ")
  }
  metadata$current_analysis_type[[row]] <- current_type
  metadata$type[[row]] <- current_type
  metadata$class[[row]] <- paste(class(data[[variable]]), collapse = ", ")
  metadata$is_numeric[[row]] <- identical(current_type, "numeric")
  metadata$is_factor[[row]] <- identical(current_type, "factor")
  metadata$is_character[[row]] <- identical(current_type, "character")
  metadata$is_logical[[row]] <- identical(current_type, "logical")
  metadata$label[[row]] <- .rls_variable_label(data[[variable]])
  metadata$labels[[row]] <- .rls_variable_label(data[[variable]])
  if (is.na(metadata$description[[row]]) || !nzchar(metadata$description[[row]] %||% "")) {
    metadata$description[[row]] <- .rls_variable_label(data[[variable]])
  }
  if (is.na(metadata$decimals[[row]])) {
    metadata$decimals[[row]] <- .rls_variable_decimals(data[[variable]])
  }
  metadata$missing_count[[row]] <- sum(is.na(data[[variable]]))
  metadata$value_labels[row] <- list(.rls_variable_value_labels(data[[variable]]))
  metadata
}

.rls_register_dataset <- function(name, data, source = "R environment", path = NA_character_,
                                  activate = TRUE, replace = FALSE) {
  name <- .rls_safe_dataset_name(name)
  if (!isTRUE(replace)) {
    name <- .rls_unique_dataset_name(name)
  }
  name <- .rls_validate_group(name, allow_null = FALSE)
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  names(data) <- make.unique(names(data))
  metadata <- .rls_variable_metadata(data)
  assign(
    name,
    list(
      dataset_id = name,
      name = name,
      dataset_name = name,
	      group = name,
	      original_data = data,
	      data_frame = data,
	      data = data,
      n_rows = nrow(data),
      n_columns = ncol(data),
      original_row_ids = seq_len(nrow(data)),
      selection_state = integer(0),
      row_color_state = character(0),
      metadata = metadata,
      variable_metadata = metadata,
      source = source,
      path = path,
      row_colors = character(0),
      modified = FALSE
    ),
    envir = .rls_state$datasets
  )
  if (isTRUE(activate)) {
    .rls_state$active_dataset <- name
    .rls_notify_active_dataset(name)
  }
  invisible(name)
}

.rls_check_import_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("`path` must be a single non-empty file path.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop(sprintf("Cannot import data because file does not exist: %s", path), call. = FALSE)
  }
  if (file.access(path, 4) != 0L) {
    stop(sprintf("Cannot import data because file is not readable: %s", path), call. = FALSE)
  }
  normalizePath(path, mustWork = TRUE)
}

.rls_validate_imported_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("The selected file was read successfully, but it did not contain a data frame.", call. = FALSE)
  }
  if (!nrow(data) || !ncol(data)) {
    stop("The selected file was read successfully, but it contained an empty data frame.", call. = FALSE)
  }
  data
}

.rls_register_imported_dataset <- function(data, path, name, source, make_active = TRUE) {
  data <- .rls_validate_imported_data(data)
  name <- name %||% tools::file_path_sans_ext(basename(path))
  .rls_register_dataset(
    name,
    data,
    source = source,
    path = normalizePath(path, mustWork = FALSE),
    activate = isTRUE(make_active)
  )
}

.rls_notify_active_dataset <- function(group) {
  .rls_spreadplot_emit(
    "ACTIVE_DATASET_CHANGED",
    group = group,
    sender_id = "datasets",
    dataset_id = group,
    payload = list(active_dataset = group)
  )
  if (isTRUE(.rls_state$process_started)) {
    record <- .rls_dataset_record(group)
    try(.rls_send(c(
      "REGISTER_DATASET",
      record$group,
      .rls_variable_payload(record$data, record$variable_metadata),
      .rls_dataframe_payload(record$data, record$variable_metadata, dataset_record = record)
    )), silent = TRUE)
    try(.rls_send(c("SET_ACTIVE_DATASET", group)), silent = TRUE)
    try(.rls_send(c("DATA_OPEN_DATA_SHEET", group)), silent = TRUE)
  }
}

#' @rdname ls_datasets
#' @export
ls_unregister_dataset <- function(name) {
  name <- .rls_validate_group(name, allow_null = FALSE)
  if (exists(name, envir = .rls_state$datasets, inherits = FALSE)) {
    rm(list = name, envir = .rls_state$datasets)
  }
  if (identical(.rls_state$active_dataset, name)) {
    remaining <- ls(envir = .rls_state$datasets, all.names = FALSE)
    .rls_state$active_dataset <- if (length(remaining)) remaining[[1L]] else NULL
    if (!is.null(.rls_state$active_dataset)) {
      .rls_notify_active_dataset(.rls_state$active_dataset)
    }
  }
  invisible(TRUE)
}

#' @rdname ls_datasets
#' @export
ls_set_active_dataset <- function(name) {
  name <- .rls_validate_group(name, allow_null = FALSE)
  if (!exists(name, envir = .rls_state$datasets, inherits = FALSE)) {
    stop(sprintf("Dataset `%s` is not registered.", name), call. = FALSE)
  }
  .rls_state$active_dataset <- name
  .rls_notify_active_dataset(name)
  invisible(name)
}

#' @rdname ls_datasets
#' @export
ls_active_dataset <- function() {
  .rls_state$active_dataset
}

#' @rdname ls_datasets
#' @export
ls_get_active_dataset <- function() {
  .rls_dataset_record()$data
}

#' @rdname ls_datasets
#' @export
ls_refresh_r_dataframes <- function(envir = .GlobalEnv) {
  if (!is.environment(envir)) {
    stop("`envir` must be an environment.", call. = FALSE)
  }
  names <- ls(envir = envir, all.names = FALSE)
  data_names <- names[vapply(names, function(name) is.data.frame(get(name, envir = envir)), logical(1L))]
  for (name in data_names) {
    .rls_register_dataset(name, get(name, envir = envir), source = "R environment", activate = FALSE, replace = TRUE)
  }
  invisible(ls_datasets())
}

.rls_dataset_record <- function(group = NULL) {
  group <- group %||% .rls_state$active_dataset
  if (is.null(group)) {
    stop("No active dataset. Import or register a dataset first.", call. = FALSE)
  }
  group <- .rls_validate_group(group, allow_null = FALSE)
  if (!exists(group, envir = .rls_state$datasets, inherits = FALSE)) {
    stop(sprintf("Dataset `%s` is not registered.", group), call. = FALSE)
  }
  get(group, envir = .rls_state$datasets)
}

.rls_safe_as_numeric <- function(x, variable) {
  if (is.numeric(x)) {
    return(x)
  }
  original_na <- is.na(x)
  converted <- suppressWarnings(as.numeric(as.character(x)))
  introduced_na <- is.na(converted) & !original_na
  if (any(introduced_na)) {
    stop(sprintf(
      "Variable `%s` cannot be treated as numeric because some values cannot be converted.",
      variable
    ), call. = FALSE)
  }
  converted
}

.rls_safe_as_logical <- function(x, variable) {
  if (is.logical(x)) {
    return(x)
  }
  text <- trimws(tolower(as.character(x)))
  out <- rep(NA, length(text))
  out[text %in% c("true", "t", "yes", "y", "1")] <- TRUE
  out[text %in% c("false", "f", "no", "n", "0")] <- FALSE
  introduced_na <- is.na(out) & !is.na(x) & nzchar(text)
  if (any(introduced_na)) {
    stop(sprintf(
      "Variable `%s` cannot be treated as logical because some values are not true/false or 0/1.",
      variable
    ), call. = FALSE)
  }
  out
}

.rls_set_dataset_record <- function(record) {
  assign(record$group, record, envir = .rls_state$datasets)
  invisible(record)
}

.rls_notify_dataset_changed <- function(record, variable = NULL) {
  .rls_spreadplot_emit(
    "DATASET_CHANGED",
    group = record$group,
    sender_id = "datasets",
    dataset_id = record$group,
    payload = list(variable = variable)
  )
  if (isTRUE(.rls_state$process_started)) {
    try(.rls_send(c(
      "REGISTER_DATASET",
      record$group,
      .rls_variable_payload(record$data, record$variable_metadata),
      .rls_dataframe_payload(record$data, record$variable_metadata, dataset_record = record)
    )), silent = TRUE)
    if (!is.null(variable)) {
      type <- .rls_metadata_type(record$variable_metadata, variable, .rls_variable_type(record$data[[variable]]))
      try(.rls_send(c("SET_VARIABLE_TYPE", record$group, variable, type)), silent = TRUE)
    }
  }
  invisible(record)
}

.rls_update_analysis_records_for_variable_type <- function(record, variable) {
  for (id in ls(.rls_state$glm_models, all.names = TRUE)) {
    model <- get(id, envir = .rls_state$glm_models)
    if (!identical(model$group, record$group)) next
    model$data <- record$data
    model$model_version <- model$model_version + 1L
    model$fit <- NULL
    model$is_stale <- TRUE
    model$status <- sprintf("Variable `%s` type changed; refit the model.", variable)
    assign(id, model, envir = .rls_state$glm_models)
    if (length(model$predictors)) {
      try(ls_glm_fit(structure(list(id = id, group = model$group), class = "rlispstat_glm")), silent = TRUE)
    }
  }
  for (id in ls(.rls_state$generalized_glm_models, all.names = TRUE)) {
    model <- get(id, envir = .rls_state$generalized_glm_models)
    if (!identical(model$group, record$group)) next
    model$data <- record$data
    model$model_version <- model$model_version + 1L
    model$fit <- NULL
    model$fitted_glm <- NULL
    model$diagnostics <- data.frame()
    model$diagnostic_data <- data.frame()
    model$status <- sprintf("Variable `%s` type changed; refit the model.", variable)
    assign(id, model, envir = .rls_state$generalized_glm_models)
    if (length(model$terms)) {
      try(ls_generalized_linear_model_fit(structure(list(id = id, group = model$group), class = "rlispstat_generalized_linear_model")), silent = TRUE)
    }
  }
  for (id in ls(.rls_state$correlation_matrices, all.names = TRUE)) {
    matrix <- get(id, envir = .rls_state$correlation_matrices)
    if (!identical(matrix$group, record$group)) next
    matrix$data <- record$data
    numeric_vars <- .rls_numeric_variable_names(record$data, record$variable_metadata)
    matrix$variables <- intersect(matrix$variables, numeric_vars)
    matrix <- .rls_correlation_refit(matrix)
    assign(id, matrix, envir = .rls_state$correlation_matrices)
  }
  for (id in ls(.rls_state$dendrograms, all.names = TRUE)) {
    dendrogram <- get(id, envir = .rls_state$dendrograms)
    if (!identical(dendrogram$group, record$group)) next
    dendrogram$data <- record$data
    numeric_vars <- .rls_numeric_variable_names(record$data, record$variable_metadata)
    dendrogram$variables <- intersect(dendrogram$variables, numeric_vars)
    if (length(dendrogram$variables) >= 1L) {
      dendrogram <- get(".rls_dendrogram_refit", mode = "function")(dendrogram)
    } else {
      dendrogram$hclust <- NULL
      dendrogram$merge <- matrix(integer(), nrow = 0L, ncol = 2L)
      dendrogram$height <- numeric()
      dendrogram$order <- integer()
      dendrogram$labels <- character()
      dendrogram$distance_matrix <- matrix(numeric(), nrow = 0L, ncol = 0L)
      dendrogram$case_rows <- integer()
      dendrogram$model_version <- dendrogram$model_version + 1L
    }
    assign(id, dendrogram, envir = .rls_state$dendrograms)
  }
  for (id in ls(.rls_state$regression_comparisons, all.names = TRUE)) {
    comparison <- get(id, envir = .rls_state$regression_comparisons)
    if (!identical(comparison$group, record$group)) next
    comparison$data <- record$data
    comparison$model_version <- comparison$model_version + 1L
    comparison$status <- sprintf("Variable `%s` type changed; refit comparison models.", variable)
    assign(id, comparison, envir = .rls_state$regression_comparisons)
  }
  invisible(TRUE)
}

#' Inspect or change the modelling type of registered variables
#'
#' @param data Registered dataset name, a data frame already registered by name,
#'   or `NULL` for the active dataset.
#' @param variable Variable name. For `ls_variable_metadata()`, `NULL` returns
#'   all variables.
#' @param type One of `"numeric"`, `"factor"`, `"character"`, or `"logical"`.
#' @return `ls_variable_metadata()` returns a metadata data frame.
#'   `ls_set_variable_type()` invisibly returns the updated metadata.
#' @export
ls_variable_metadata <- function(data = NULL, variable = NULL) {
  record <- .rls_dataset_record(data)
  metadata <- record$variable_metadata %||% record$metadata %||% .rls_variable_metadata(record$data)
  if (!is.null(variable)) {
    variable <- .rls_validate_protocol_name(variable, "variable")
    name_col <- if ("variable_name" %in% names(metadata)) "variable_name" else "name"
    metadata <- metadata[metadata[[name_col]] == variable, , drop = FALSE]
    if (!nrow(metadata)) {
      stop(sprintf("Column `%s` was not found in the dataset.", variable), call. = FALSE)
    }
  }
  metadata
}

#' @rdname ls_variable_metadata
#' @export
ls_set_variable_type <- function(data = NULL, variable, type = c("numeric", "factor", "character", "logical")) {
  type <- match.arg(type)
  record <- .rls_dataset_record(data)
  variable <- .rls_validate_protocol_name(variable, "variable")
  if (!variable %in% names(record$data)) {
    stop(sprintf("Column `%s` was not found in the dataset.", variable), call. = FALSE)
  }
  current <- record$data[[variable]]
  converted <- switch(
    type,
    numeric = .rls_safe_as_numeric(current, variable),
    factor = as.factor(current),
    character = as.character(current),
    logical = .rls_safe_as_logical(current, variable)
  )
  attrs <- attributes(current)
  keep_attrs <- intersect(names(attrs), c("label", "labels", "format.spss", "display_width"))
  for (attr_name in keep_attrs) {
    attr(converted, attr_name) <- attrs[[attr_name]]
  }
  record$data[[variable]] <- converted
  record$data_frame <- record$data
  record$metadata <- .rls_refresh_metadata_row(record$metadata, record$data, variable, type)
  record$variable_metadata <- .rls_refresh_metadata_row(record$variable_metadata, record$data, variable, type)
  record$modified <- TRUE
  .rls_set_dataset_record(record)
  .rls_update_analysis_records_for_variable_type(record, variable)
  .rls_notify_dataset_changed(record, variable)
  invisible(ls_variable_metadata(record$group, variable))
}

#' Create a scatterplot from a registered dataset
#'
#' @param group Dataset/group name. If `NULL`, the active dataset is used.
#' @param x,y Numeric variable names. Missing values are omitted from drawing
#'   while original row ids are preserved.
#' @param labels Optional label variable name or vector.
#' @param title Optional plot title.
#' @param linked Logical; currently `TRUE` uses the dataset group for linked
#'   brushing. `FALSE` creates a private plot group.
#' @return An `rlispstat_plot` object.
#' @export
ls_new_scatterplot <- function(group = NULL, x = NULL, y = NULL, labels = NULL,
                               title = NULL, linked = TRUE) {
  record <- .rls_dataset_record(group)
  data <- record$data
  numeric <- .rls_numeric_variable_names(data, record$variable_metadata)
  if (length(numeric) < 2L && (is.null(x) || is.null(y))) {
    stop("The dataset must contain at least two numeric variables unless both `x` and `y` are supplied.", call. = FALSE)
  }
  x <- x %||% numeric[[1L]]
  y <- y %||% numeric[[min(2L, length(numeric))]]
  if (is.character(labels) && length(labels) == 1L && labels %in% names(data)) {
    labels <- as.character(data[[labels]])
  }
  if (is.null(title) && identical(record$dataset_type %||% "data_frame", "multiple_imputation")) {
    title <- paste(y, "vs", x, .rls_mi_title_suffix(record))
  }
  plot_group <- if (isTRUE(linked)) record$group else paste0(record$group, "_", format(Sys.time(), "%Y%m%d%H%M%OS3"))
  ls_scatter(data, x = x, y = y, group = plot_group, labels = labels, title = title)
}

#' Import external data into the LinkEDA registry
#'
#' @param path File path. If omitted interactively, a native chooser is used by R.
#' @param name Optional dataset name.
#' @param sheet Excel sheet name or index. If omitted, the first sheet is used.
#' @param make_active Logical. If `TRUE`, the imported dataset becomes active.
#' @param ... Additional arguments passed to `utils::read.csv()`.
#' @return Invisibly returns the dataset name.
#' @export
ls_import_data <- function(path = NULL, name = NULL, sheet = NULL, make_active = TRUE) {
  if (is.null(path)) {
    if (!interactive()) {
      stop("`path` is required in non-interactive sessions.", call. = FALSE)
    }
    path <- .rls_choose_data_file()
    if (is.null(path) || !nzchar(path)) {
      message("Import cancelled.")
      return(invisible(NULL))
    }
  }
  path <- .rls_check_import_path(path)
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("sav", "zsav")) {
    return(ls_import_spss(path, name = name, make_active = make_active))
  }
  if (ext == "dta") {
    return(ls_import_stata(path, name = name, make_active = make_active))
  }
  if (ext %in% c("sas7bdat", "xpt")) {
    return(ls_import_sas(path, name = name, make_active = make_active))
  }
  if (ext %in% c("xls", "xlsx")) {
    return(ls_import_excel(path, sheet = sheet, name = name, make_active = make_active))
  }
  if (ext == "csv") {
    return(ls_import_csv(path, name = name, make_active = make_active))
  }
  if (ext %in% c("tsv", "txt")) {
    return(ls_import_tsv(path, name = name, make_active = make_active))
  }
  if (ext == "rds") {
    return(ls_import_rds(path, name = name, make_active = make_active))
  }
  if (ext %in% c("rda", "rdata")) {
    return(ls_import_rdata(path, name = name, make_active = make_active))
  }
  stop("Unsupported data file. Use .csv, .txt, .tsv, .sav, .zsav, .dta, .sas7bdat, .xpt, .xlsx, .xls, .rds, .rda, or .RData.", call. = FALSE)
}

.rls_choose_data_file <- function() {
  if (.Platform$OS.type == "unix" && Sys.info()[["sysname"]] == "Darwin") {
    script <- paste(
      'set f to choose file with prompt "Import data into LinkEDA" of type {"csv", "txt", "tsv", "sav", "zsav", "dta", "sas7bdat", "xpt", "xlsx", "xls", "rds", "rda", "RData"}',
      "POSIX path of f",
      sep = "\n"
    )
    path <- tryCatch(system2("osascript", c("-e", script), stdout = TRUE, stderr = TRUE), error = function(e) character())
    if (length(path) && file.exists(path[[1L]])) {
      return(path[[1L]])
    }
    return(NULL)
  }
  file.choose()
}

#' @rdname ls_import_data
#' @export
ls_import_spss <- function(path, name = NULL, make_active = TRUE) {
  path <- .rls_check_import_path(path)
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Cannot import SPSS file because package 'haven' is not installed. Install it with install.packages(\"haven\") and try again.", call. = FALSE)
  }
  data <- as.data.frame(haven::read_sav(path), stringsAsFactors = FALSE)
  .rls_register_imported_dataset(data, path, name, "SPSS file", make_active = make_active)
}

#' @rdname ls_import_data
#' @export
ls_import_stata <- function(path, name = NULL, make_active = TRUE) {
  path <- .rls_check_import_path(path)
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Cannot import Stata file because package 'haven' is not installed. Install it with install.packages(\"haven\") and try again.", call. = FALSE)
  }
  data <- as.data.frame(haven::read_dta(path), stringsAsFactors = FALSE)
  .rls_register_imported_dataset(data, path, name, "Stata file", make_active = make_active)
}

#' @rdname ls_import_data
#' @export
ls_import_sas <- function(path, name = NULL, make_active = TRUE) {
  path <- .rls_check_import_path(path)
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Cannot import SAS file because package 'haven' is not installed. Install it with install.packages(\"haven\") and try again.", call. = FALSE)
  }
  ext <- tolower(tools::file_ext(path))
  reader <- if (identical(ext, "xpt")) haven::read_xpt else haven::read_sas
  data <- as.data.frame(reader(path), stringsAsFactors = FALSE)
  .rls_register_imported_dataset(data, path, name, if (identical(ext, "xpt")) "SAS transport file" else "SAS file", make_active = make_active)
}

#' @rdname ls_import_data
#' @export
ls_import_excel <- function(path, sheet = NULL, name = NULL, make_active = TRUE) {
  path <- .rls_check_import_path(path)
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Cannot import Excel file because package 'readxl' is not installed. Install it with install.packages(\"readxl\") and try again.", call. = FALSE)
  }
  if (is.null(sheet)) {
    sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) {
      stop(sprintf("Cannot read Excel sheets: %s", conditionMessage(e)), call. = FALSE)
    })
    if (!length(sheets)) {
      stop("Cannot import Excel file because no readable sheets were found.", call. = FALSE)
    }
    sheet <- sheets[[1L]]
  }
  data <- tryCatch(
    as.data.frame(readxl::read_excel(path, sheet = sheet, .name_repair = "unique"), stringsAsFactors = FALSE),
    error = function(e) stop(sprintf("Cannot import Excel sheet `%s`: %s", as.character(sheet), conditionMessage(e)), call. = FALSE)
  )
  out <- .rls_register_imported_dataset(data, path, name, sprintf("Excel file: %s", as.character(sheet)), make_active = make_active)
  message(sprintf("Imported Excel sheet `%s`.", as.character(sheet)))
  out
}

#' @rdname ls_import_data
#' @export
ls_import_csv <- function(path, name = NULL, make_active = TRUE, ...) {
  path <- .rls_check_import_path(path)
  data <- tryCatch(
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE, ...),
    error = function(e) stop(sprintf("Cannot import CSV file: %s", conditionMessage(e)), call. = FALSE)
  )
  names(data) <- make.unique(names(data))
  .rls_register_imported_dataset(data, path, name, "CSV file", make_active = make_active)
}

#' @rdname ls_import_data
#' @export
ls_import_tsv <- function(path, name = NULL, make_active = TRUE, ...) {
  path <- .rls_check_import_path(path)
  data <- tryCatch(
    utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE, ...),
    error = function(e) stop(sprintf("Cannot import delimited text file: %s", conditionMessage(e)), call. = FALSE)
  )
  names(data) <- make.unique(names(data))
  .rls_register_imported_dataset(data, path, name, "Delimited text file", make_active = make_active)
}

#' @rdname ls_import_data
#' @export
ls_import_rds <- function(path, name = NULL, make_active = TRUE) {
  path <- .rls_check_import_path(path)
  data <- tryCatch(
    readRDS(path),
    error = function(e) stop(sprintf("Cannot import RDS file: %s", conditionMessage(e)), call. = FALSE)
  )
  .rls_register_imported_dataset(data, path, name, "RDS file", make_active = make_active)
}

#' @rdname ls_import_data
#' @export
ls_import_rdata <- function(path, name = NULL, make_active = TRUE) {
  path <- .rls_check_import_path(path)
  env <- new.env(parent = emptyenv())
  loaded <- tryCatch(
    load(path, envir = env),
    error = function(e) stop(sprintf("Cannot import R data file: %s", conditionMessage(e)), call. = FALSE)
  )
  data_names <- loaded[vapply(loaded, function(object_name) is.data.frame(get(object_name, envir = env)), logical(1L))]
  if (!length(data_names)) {
    stop("The selected R data file did not contain a data frame.", call. = FALSE)
  }
  sizes <- vapply(data_names, function(object_name) {
    data <- get(object_name, envir = env)
    nrow(data) * max(1L, ncol(data))
  }, numeric(1L))
  selected <- data_names[[which.max(sizes)]]
  data <- get(selected, envir = env)
  .rls_register_imported_dataset(data, path, name %||% selected, sprintf("R data file: %s", selected), make_active = make_active)
}

#' Open or return the active data sheet
#'
#' Opens the native data sheet for the registered data frame and returns the data
#' frame invisibly.
#'
#' @param group Dataset/group name. If omitted, the active dataset is used.
#' @return Invisibly returns the data frame.
#' @export
ls_data_sheet <- function(group = NULL) {
  record <- .rls_dataset_record(group)
  .rls_start_backend()
  .rls_send(c(
    "REGISTER_DATASET",
    record$group,
    .rls_variable_payload(record$data, record$variable_metadata),
    .rls_dataframe_payload(record$data, record$variable_metadata, dataset_record = record)
  ))
  if (isTRUE(.rls_state$process_started)) {
    try(.rls_send(c("DATA_OPEN_DATA_SHEET", record$group)), silent = TRUE)
  }
  invisible(record$data)
}

#' @rdname ls_data_sheet
#' @export
ls_open_data_sheet <- function(group = NULL) {
  ls_data_sheet(group)
}

#' @rdname ls_data_sheet
#' @export
ls_close_data_sheet <- function(group = NULL) {
  record <- .rls_dataset_record(group)
  if (isTRUE(.rls_state$process_started)) {
    try(.rls_send(c("DATA_CLOSE_DATA_SHEET", record$group)), silent = TRUE)
  }
  invisible(TRUE)
}

#' @rdname ls_data_sheet
#' @export
ls_show_data_sheet <- function(show = TRUE) {
  if (isTRUE(show)) {
    ls_data_sheet()
  } else {
    ls_close_data_sheet()
  }
}

#' @rdname ls_data_sheet
#' @export
ls_data_sheet_active_dataset <- function() {
  ls_active_dataset()
}

.rls_resolve_color_group <- function(group = NULL) {
  .rls_dataset_record(group)$group
}

#' Set row colors for linked workbench rows
#'
#' @param group Dataset/group name. If `NULL`, the active dataset is used.
#' @param rows Original row indices.
#' @param color One of the palette color names.
#' @return Invisibly returns `TRUE`.
#' @export
ls_set_row_color <- function(group = NULL, rows, color) {
  group <- .rls_resolve_color_group(group)
  color <- .rls_validate_palette_color(color)
  rows <- as.integer(rows)
  rows <- rows[!is.na(rows) & rows > 0L]
  record <- .rls_dataset_record(group)
  row_colors <- record$row_colors
  row_colors[as.character(rows)] <- color
  record$row_colors <- row_colors
  assign(group, record, envir = .rls_state$datasets)
  if (isTRUE(.rls_state$process_started)) {
    .rls_send(c("SET_POINT_COLOR", group, color, as.character(length(rows)), as.character(rows)))
  }
  invisible(TRUE)
}

#' @rdname ls_set_row_color
#' @export
ls_get_row_colors <- function(group = NULL) {
  record <- .rls_dataset_record(group)
  record$row_colors
}

#' @rdname ls_set_row_color
#' @export
ls_clear_row_color <- function(group = NULL, rows = NULL) {
  group <- .rls_resolve_color_group(group)
  record <- .rls_dataset_record(group)
  if (is.null(rows)) {
    record$row_colors <- character(0)
    if (isTRUE(.rls_state$process_started)) {
      .rls_send(c("CLEAR_ROW_COLORS", group))
    }
  } else {
    rows <- as.character(as.integer(rows))
    record$row_colors <- record$row_colors[!names(record$row_colors) %in% rows]
    if (isTRUE(.rls_state$process_started)) {
      .rls_send(c("CLEAR_ROW_COLORS", group, as.character(length(rows)), rows))
    }
  }
  assign(group, record, envir = .rls_state$datasets)
  invisible(TRUE)
}

#' @rdname ls_set_row_color
#' @export
ls_color_selected_rows <- function(group = NULL, color) {
  group <- .rls_resolve_color_group(group)
  rows <- ls_selected(group)
  if (!length(rows)) {
    stop("No rows are selected.", call. = FALSE)
  }
  ls_set_row_color(group, rows, color)
}
