.rls_mice_missing_message <- paste(
  "Multiple imputation requires the mice package.",
  "Install it with install.packages(\"mice\") and try again."
)

.rls_require_mice <- function() {
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop(.rls_mice_missing_message, call. = FALSE)
  }
  invisible(TRUE)
}

.rls_imputation_id <- function(name) {
  base <- .rls_safe_dataset_name(name %||% "imputation")
  base <- gsub("[ ]+", "_", base)
  candidate <- paste0("mi_", base)
  i <- 2L
  while (exists(candidate, envir = .rls_state$missing_imputations, inherits = FALSE)) {
    candidate <- paste0("mi_", base, "_", i)
    i <- i + 1L
  }
  candidate
}

.rls_imputation_record <- function(imputation) {
  id <- if (inherits(imputation, "rlispstat_imputation")) {
    imputation$id
  } else if (is.character(imputation) && length(imputation) == 1L) {
    imputation
  } else {
    stop("`imputation` must be a LinkEDA imputation object or imputation id.", call. = FALSE)
  }
  if (!exists(id, envir = .rls_state$missing_imputations, inherits = FALSE)) {
    stop(sprintf("Imputation `%s` is not registered.", id), call. = FALSE)
  }
  get(id, envir = .rls_state$missing_imputations)
}

.rls_set_imputation_record <- function(record) {
  assign(record$id, record, envir = .rls_state$missing_imputations)
  structure(list(id = record$id, dataset_id = record$dataset_id, group = record$group),
            class = "rlispstat_imputation")
}

.rls_mi_data_record <- function(data, name = NULL) {
  if (is.null(data)) {
    return(.rls_dataset_record())
  }
  if (is.character(data) && length(data) == 1L && exists(data, envir = .rls_state$datasets, inherits = FALSE)) {
    return(.rls_dataset_record(data))
  }
  if (is.data.frame(data)) {
    dataset_name <- name %||% "imputation data"
    dataset_id <- .rls_register_dataset(dataset_name, data, source = "multiple imputation source", activate = FALSE)
    return(.rls_dataset_record(dataset_id))
  }
  stop("`data` must be NULL, a registered dataset name, or a data.frame.", call. = FALSE)
}

.rls_mi_variable_names <- function(record) {
  names(record$data)
}

.rls_mi_is_id_like <- function(values, name) {
  lname <- tolower(name)
  if (grepl("(^id$|_id$|^id_|identifier|subject|case|etiqueta|label)", lname)) {
    return(TRUE)
  }
  observed <- values[!is.na(values)]
  if (!length(observed)) {
    return(FALSE)
  }
  if (is.numeric(observed) || is.integer(observed)) {
    sorted <- sort(unique(observed))
    return(length(sorted) == length(observed) &&
             all(abs(sorted - round(sorted)) < .Machine$double.eps^0.5) &&
             all(diff(sorted) == 1))
  }
  length(unique(observed)) == length(observed) && length(observed) >= 0.9 * length(values)
}

.rls_mi_type <- function(record, variable) {
  .rls_metadata_type(record$variable_metadata, variable, .rls_variable_type(record$data[[variable]]))
}

.rls_mi_supported_predictor <- function(record, variable) {
  type <- .rls_mi_type(record, variable)
  type %in% c("numeric", "factor", "logical", "character")
}

.rls_mi_default_method <- function(record, variable) {
  x <- record$data[[variable]]
  type <- .rls_mi_type(record, variable)
  observed <- x[!is.na(x)]
  if (!length(observed)) {
    return(NA_character_)
  }
  if (identical(type, "numeric")) {
    return("pmm")
  }
  if (identical(type, "logical")) {
    return("logreg")
  }
  if (identical(type, "character")) {
    observed <- factor(observed)
    nlevels <- nlevels(observed)
    return(if (nlevels <= 2L) "logreg" else "polyreg")
  }
  if (identical(type, "factor")) {
    nlevels <- nlevels(droplevels(x))
    if (nlevels <= 2L) {
      "logreg"
    } else if (is.ordered(x)) {
      "polr"
    } else {
      "polyreg"
    }
  } else {
    NA_character_
  }
}

.rls_mi_allowed_methods <- function(record, variable) {
  type <- .rls_mi_type(record, variable)
  x <- record$data[[variable]]
  if (identical(type, "numeric")) {
    return(c("pmm", "norm", "cart", "rf"))
  }
  if (identical(type, "logical")) {
    return(c("logreg", "cart", "rf"))
  }
  if (identical(type, "character")) {
    return(c("logreg", "polyreg", "cart", "rf"))
  }
  if (identical(type, "factor")) {
    if (is.ordered(x) && nlevels(droplevels(x)) > 2L) {
      return(c("polr", "cart", "rf"))
    }
    if (nlevels(droplevels(x)) <= 2L) {
      return(c("logreg", "cart", "rf"))
    }
    return(c("polyreg", "cart", "rf"))
  }
  character()
}

.rls_mi_validate_variables <- function(record, variables, what) {
  variables <- unique(as.character(variables %||% character()))
  variables <- variables[nzchar(variables)]
  missing <- setdiff(variables, .rls_mi_variable_names(record))
  if (length(missing)) {
    stop(sprintf("%s variable `%s` was not found in the dataset.", what, missing[[1L]]), call. = FALSE)
  }
  variables
}

.rls_mi_default_impute <- function(record) {
  vars <- names(record$data)[vapply(record$data, function(x) any(is.na(x)), logical(1L))]
  vars <- vars[vapply(vars, function(variable) {
    x <- record$data[[variable]]
    .rls_mi_supported_predictor(record, variable) &&
      !all(is.na(x)) &&
      !.rls_mi_is_id_like(x, variable) &&
      !is.na(.rls_mi_default_method(record, variable))
  }, logical(1L))]
  vars
}

.rls_mi_default_predictors <- function(record, impute) {
  vars <- names(record$data)[vapply(names(record$data), function(variable) {
    x <- record$data[[variable]]
    .rls_mi_supported_predictor(record, variable) &&
      !all(is.na(x)) &&
      !.rls_mi_is_id_like(x, variable)
  }, logical(1L))]
  unique(c(setdiff(vars, character()), impute))
}

.rls_mi_method_vector <- function(record, impute, method = NULL) {
  methods <- rep("", ncol(record$data))
  names(methods) <- names(record$data)
  defaults <- vapply(impute, function(variable) .rls_mi_default_method(record, variable), character(1L))
  if (anyNA(defaults)) {
    bad <- impute[is.na(defaults)][[1L]]
    stop(sprintf("Variable `%s` has an unsupported type for imputation.", bad), call. = FALSE)
  }
  methods[impute] <- defaults
  if (!is.null(method)) {
    if (is.character(method) && is.null(names(method)) && length(method) == 1L) {
      methods[impute] <- method
    } else {
      if (is.null(names(method)) || any(!nzchar(names(method)))) {
        stop("`method` must be a named character vector, or a single method for all imputed variables.", call. = FALSE)
      }
      unknown <- setdiff(names(method), impute)
      if (length(unknown)) {
        stop(sprintf("Method was supplied for non-imputed variable `%s`.", unknown[[1L]]), call. = FALSE)
      }
      methods[names(method)] <- unname(method)
    }
  }
  for (variable in impute) {
    allowed <- .rls_mi_allowed_methods(record, variable)
    if (!methods[[variable]] %in% allowed) {
      stop(sprintf(
        "Method `%s` is not valid for variable `%s`.",
        methods[[variable]], variable
      ), call. = FALSE)
    }
  }
  methods
}

.rls_mi_predictor_matrix <- function(record, impute, predictors, predictor_matrix = NULL) {
  p <- ncol(record$data)
  variable_names <- names(record$data)
  if (!is.null(predictor_matrix)) {
    matrix <- as.matrix(predictor_matrix)
    if (!identical(dim(matrix), c(p, p))) {
      stop("`predictor_matrix` must have one row and one column for each dataset variable.", call. = FALSE)
    }
    if (is.null(rownames(matrix))) rownames(matrix) <- variable_names
    if (is.null(colnames(matrix))) colnames(matrix) <- variable_names
    matrix <- matrix[variable_names, variable_names, drop = FALSE]
    storage.mode(matrix) <- "numeric"
  } else {
    matrix <- matrix(0, nrow = p, ncol = p, dimnames = list(variable_names, variable_names))
    matrix[impute, predictors] <- 1
  }
  diag(matrix) <- 0
  matrix[setdiff(variable_names, impute), ] <- 0
  matrix[, setdiff(variable_names, predictors)] <- 0
  matrix
}

.rls_mi_prepare_data_for_mice <- function(record, variables) {
  data <- record$original_data %||% record$data
  data[] <- lapply(data, function(x) {
    if (!is.null(attr(x, "labels", exact = TRUE))) {
      raw <- unclass(x)
      attributes(raw) <- NULL
      return(raw)
    }
    x
  })
  for (variable in variables) {
    type <- .rls_mi_type(record, variable)
    if (identical(type, "logical")) {
      data[[variable]] <- factor(data[[variable]], levels = c(FALSE, TRUE))
    } else if (identical(type, "character")) {
      data[[variable]] <- factor(data[[variable]])
    }
  }
  data
}

.rls_mi_missing_mask <- function(data, impute_variables = names(data)) {
  mask <- as.data.frame(lapply(data, function(x) rep(FALSE, length(x))), stringsAsFactors = FALSE)
  for (name in intersect(names(data), impute_variables)) {
    mask[[name]] <- is.na(data[[name]])
  }
  mask
}

.rls_mi_completed_name <- function(source_name, name = NULL) {
  .rls_unique_dataset_name(name %||% paste(source_name, "imputed"))
}

.rls_register_imputed_dataset <- function(record, make_active = TRUE) {
  dataset_name <- .rls_mi_completed_name(record$source_dataset_id, record$name)
  data <- record$completed_datasets[[record$active_version]]
  dataset_id <- .rls_register_dataset(
    dataset_name,
    data,
    source = sprintf("Multiple imputation from %s", record$source_dataset_id),
    activate = FALSE
  )
  dataset_record <- .rls_dataset_record(dataset_id)
  dataset_record$dataset_type <- "multiple_imputation"
  dataset_record$imputation_id <- record$id
  dataset_record$source_dataset_id <- record$source_dataset_id
  dataset_record$original_data <- record$original_data
  dataset_record$completed_datasets <- record$completed_datasets
  dataset_record$mids_object <- record$mids_object
  dataset_record$missing_cell_mask <- record$missing_cell_mask
  dataset_record$imputed_cell_map <- record$imputed_cell_map
  dataset_record$active_imputation_version <- record$active_version
  dataset_record$imputation_display_mode <- record$display_mode
  dataset_record$imputation_count <- record$m
  dataset_record$original_row_ids <- record$original_row_ids
  dataset_record$variable_metadata <- record$variable_metadata
  dataset_record$metadata <- record$variable_metadata
  .rls_set_dataset_record(dataset_record)
  record$dataset_id <- dataset_id
  record$group <- dataset_id
  if (isTRUE(make_active)) {
    .rls_state$active_dataset <- dataset_id
    .rls_notify_active_dataset(dataset_id)
  }
  record
}

.rls_sync_imputed_dataset_record <- function(record, open_sheet = FALSE) {
  if (is.null(record$dataset_id) || !exists(record$dataset_id, envir = .rls_state$datasets, inherits = FALSE)) {
    return(invisible(record))
  }
  dataset_record <- .rls_dataset_record(record$dataset_id)
  if (identical(record$display_mode, "original")) {
    dataset_record$data <- record$original_data
  } else {
    dataset_record$data <- record$completed_datasets[[record$active_version]]
  }
  dataset_record$data_frame <- dataset_record$data
  dataset_record$dataset_type <- "multiple_imputation"
  dataset_record$completed_datasets <- record$completed_datasets
  dataset_record$mids_object <- record$mids_object
  dataset_record$missing_cell_mask <- record$missing_cell_mask
  dataset_record$imputed_cell_map <- record$imputed_cell_map
  dataset_record$active_imputation_version <- record$active_version
  dataset_record$imputation_display_mode <- record$display_mode
  dataset_record$imputation_count <- record$m
  dataset_record$original_row_ids <- record$original_row_ids
  .rls_set_dataset_record(dataset_record)
  if (isTRUE(.rls_state$process_started)) {
    try(.rls_send(c(
      "REGISTER_DATASET",
      dataset_record$group,
      .rls_variable_payload(dataset_record$data, dataset_record$variable_metadata),
      .rls_dataframe_payload(dataset_record$data, dataset_record$variable_metadata, dataset_record = dataset_record)
    )), silent = TRUE)
    if (isTRUE(open_sheet)) {
      try(.rls_send(c("DATA_OPEN_DATA_SHEET", dataset_record$group)), silent = TRUE)
    }
  }
  invisible(record)
}

.rls_mi_build_cell_map <- function(original_data, completed_datasets, mask) {
  rows <- seq_len(nrow(original_data))
  out <- list()
  for (variable in names(original_data)) {
    missing_rows <- rows[mask[[variable]]]
    if (!length(missing_rows)) next
    values <- lapply(completed_datasets, function(data) data[[variable]][missing_rows])
    out[[variable]] <- data.frame(
      row_id = rep(missing_rows, times = length(completed_datasets)),
      variable = variable,
      imputation = rep(seq_along(completed_datasets), each = length(missing_rows)),
      value = unlist(values, use.names = FALSE),
      stringsAsFactors = FALSE
    )
  }
  out
}

#' Multiple imputation for missing data
#'
#' Creates and runs a multiple-imputation object using `mice`.
#'
#' @param data Registered dataset name, data frame, or `NULL` for the active dataset.
#' @param impute Variables to impute. Defaults to variables with missing values.
#' @param predictors Variables used as predictors. Defaults to suitable non-ID variables.
#' @param m Number of imputations.
#' @param maxit Number of MICE iterations.
#' @param seed Optional random seed.
#' @param method Optional named method vector, or one method for all imputed variables.
#' @param predictor_matrix Optional complete `mice` predictor matrix.
#' @param name Optional name for the imputed dataset.
#' @return An `rlispstat_imputation` handle.
#' @export
ls_new_missing_data_imputation <- function(data = NULL,
                                           impute = NULL,
                                           predictors = NULL,
                                           m = 5,
                                           maxit = 5,
                                           seed = NULL,
                                           method = NULL,
                                           predictor_matrix = NULL,
                                           name = NULL) {
  record <- .rls_mi_data_record(data, name = name)
  if (!is.numeric(m) || length(m) != 1L || is.na(m) || m < 1L) {
    stop("`m` must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(maxit) || length(maxit) != 1L || is.na(maxit) || maxit < 0L) {
    stop("`maxit` must be a non-negative integer.", call. = FALSE)
  }
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1L || is.na(seed))) {
    stop("`seed` must be NULL or a single numeric value.", call. = FALSE)
  }
  impute <- if (is.null(impute)) .rls_mi_default_impute(record) else .rls_mi_validate_variables(record, impute, "Imputation")
  if (!length(impute)) {
    stop("No variables with missing values were selected for imputation.", call. = FALSE)
  }
  for (variable in impute) {
    x <- record$data[[variable]]
    if (!any(is.na(x))) {
      stop(sprintf("Variable `%s` has no missing values to impute.", variable), call. = FALSE)
    }
    if (all(is.na(x))) {
      stop(sprintf("Variable `%s` has all values missing and cannot be imputed safely.", variable), call. = FALSE)
    }
    if (.rls_mi_is_id_like(x, variable) && is.null(method)) {
      stop(sprintf("Variable `%s` appears to be an identifier and will not be imputed by default.", variable), call. = FALSE)
    }
  }
  predictors <- if (is.null(predictors)) .rls_mi_default_predictors(record, impute) else .rls_mi_validate_variables(record, predictors, "Predictor")
  predictors <- predictors[vapply(predictors, function(variable) .rls_mi_supported_predictor(record, variable), logical(1L))]
  if (!length(predictors)) {
    stop("No valid predictor variables were selected.", call. = FALSE)
  }
  methods <- .rls_mi_method_vector(record, impute, method)
  matrix <- .rls_mi_predictor_matrix(record, impute, predictors, predictor_matrix)
  id <- .rls_imputation_id(name %||% record$name)
  missing_mask <- .rls_mi_missing_mask(record$data, impute)
  imputation_record <- list(
    id = id,
    dataset_id = NULL,
    group = NULL,
    source_dataset_id = record$group,
    name = name,
    original_data = record$data,
    original_row_ids = record$original_row_ids %||% seq_len(nrow(record$data)),
    variable_metadata = record$variable_metadata,
    impute_variables = impute,
    predictor_variables = predictors,
    methods = methods[impute],
    method_vector = methods,
    predictor_matrix = matrix,
    m = as.integer(round(m)),
    maxit = as.integer(round(maxit)),
    seed = if (is.null(seed)) NULL else as.integer(seed),
    mids_object = NULL,
    completed_datasets = list(),
    missing_cell_mask = missing_mask,
    imputed_cell_map = list(),
    active_version = 1L,
    display_mode = "version",
    created_at = Sys.time(),
    fit_version = 0L,
    warnings = character(),
    errors = character(),
    pooling = list(engine = "mice", with = "mice::with", pool = "mice::pool", rubin_rules_ready = TRUE),
    plot_metadata = list(active_version = 1L, missing_cell_mask = missing_mask)
  )
  .rls_set_imputation_record(imputation_record)
}

#' @rdname ls_new_missing_data_imputation
#' @param imputation An `rlispstat_imputation` object or imputation id.
#' @param ... Additional arguments passed to `mice::mice()`.
#' @export
ls_run_imputation <- function(imputation, ..., open_data = TRUE, make_active = TRUE) {
  .rls_require_mice()
  record <- .rls_imputation_record(imputation)
  variables_for_mice <- unique(c(record$impute_variables, record$predictor_variables))
  data_for_mice <- .rls_mi_prepare_data_for_mice(record, variables_for_mice)
  warnings <- character()
  mice_args <- list(
    data = data_for_mice,
    m = record$m,
    maxit = record$maxit,
    method = record$method_vector,
    predictorMatrix = record$predictor_matrix,
    printFlag = FALSE,
    ...
  )
  if (!is.null(record$seed)) {
    mice_args$seed <- record$seed
  }
  mids <- tryCatch(
    withCallingHandlers(
      do.call(mice::mice, mice_args),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      record$errors <- c(record$errors, conditionMessage(e))
      assign(record$id, record, envir = .rls_state$missing_imputations)
      stop(sprintf("mice::mice() failed: %s", conditionMessage(e)), call. = FALSE)
    }
  )
  completed <- tryCatch(
    lapply(seq_len(record$m), function(i) {
      data <- as.data.frame(mice::complete(mids, i), stringsAsFactors = FALSE)
      data[names(record$original_data)]
    }),
    error = function(e) stop(sprintf("Completed datasets could not be created: %s", conditionMessage(e)), call. = FALSE)
  )
  record$mids_object <- mids
  record$completed_datasets <- completed
  record$warnings <- warnings
  record$errors <- character()
  record$fit_version <- record$fit_version + 1L
  record$imputed_cell_map <- .rls_mi_build_cell_map(record$original_data, completed, record$missing_cell_mask)
  record$plot_metadata$active_version <- record$active_version
  record <- .rls_register_imputed_dataset(record, make_active = make_active)
  .rls_set_imputation_record(record)
  .rls_sync_imputed_dataset_record(record, open_sheet = isTRUE(open_data))
  invisible(structure(list(id = record$id, dataset_id = record$dataset_id, group = record$group),
                      class = "rlispstat_imputation"))
}

#' @rdname ls_new_missing_data_imputation
#' @param version Imputation version. Use `"all"` for combined display or
#'   `"original"` for the incomplete data in `ls_set_imputed_dataset_version()`.
#' @export
ls_completed_dataset <- function(imputation, version = 1) {
  record <- .rls_imputation_record(imputation)
  if (!length(record$completed_datasets)) {
    stop("This imputation has not been run yet.", call. = FALSE)
  }
  version <- as.integer(version)
  if (is.na(version) || version < 1L || version > length(record$completed_datasets)) {
    stop(sprintf("`version` must be between 1 and %d.", length(record$completed_datasets)), call. = FALSE)
  }
  record$completed_datasets[[version]]
}

#' @rdname ls_new_missing_data_imputation
#' @export
ls_set_imputed_dataset_version <- function(imputation, version = 1) {
  record <- .rls_imputation_record(imputation)
  if (!length(record$completed_datasets)) {
    stop("This imputation has not been run yet.", call. = FALSE)
  }
  if (is.character(version) && length(version) == 1L && version %in% c("all", "original")) {
    record$display_mode <- version
  } else {
    version <- as.integer(version)
    if (is.na(version) || version < 1L || version > length(record$completed_datasets)) {
      stop(sprintf("`version` must be between 1 and %d, or one of 'all'/'original'.", length(record$completed_datasets)), call. = FALSE)
    }
    record$active_version <- version
    record$display_mode <- "version"
  }
  record$plot_metadata$active_version <- record$active_version
  .rls_set_imputation_record(record)
  .rls_sync_imputed_dataset_record(record, open_sheet = TRUE)
  invisible(structure(list(id = record$id, dataset_id = record$dataset_id, group = record$group),
                      class = "rlispstat_imputation"))
}

#' @rdname ls_new_missing_data_imputation
#' @export
ls_imputation_summary <- function(imputation) {
  record <- .rls_imputation_record(imputation)
  missing <- vapply(record$impute_variables, function(variable) {
    sum(record$missing_cell_mask[[variable]])
  }, integer(1L))
  table <- data.frame(
    variable = record$impute_variables,
    missing = unname(missing),
    method = unname(record$methods[record$impute_variables]),
    stringsAsFactors = FALSE
  )
  structure(
    list(
      imputation_id = record$id,
      dataset = record$source_dataset_id,
      imputed_dataset = record$dataset_id %||% NA_character_,
      rows = nrow(record$original_data),
      variables = ncol(record$original_data),
      m = record$m,
      maxit = record$maxit,
      seed = record$seed,
      imputed_variables = record$impute_variables,
      predictor_variables = record$predictor_variables,
      methods = record$methods,
      missing = table,
      predictor_matrix = record$predictor_matrix,
      warnings = record$warnings,
      errors = record$errors,
      fit_version = record$fit_version
    ),
    class = "rlispstat_imputation_summary"
  )
}

print.rlispstat_imputation_summary <- function(x, ...) {
  cat("Multiple imputation summary\n\n")
  cat("Dataset:", x$dataset, "\n")
  cat("Imputations:", x$m, "\n")
  cat("Iterations:", x$maxit, "\n\n")
  print(x$missing, row.names = FALSE)
  if (length(x$warnings)) {
    cat("\nWarnings:\n")
    cat(paste0("- ", x$warnings, collapse = "\n"), "\n")
  }
  invisible(x)
}

.rls_imputed_data_sheet_state <- function(record) {
  if (!identical(record$dataset_type %||% "data_frame", "multiple_imputation")) {
    stop("Dataset is not a multiple-imputation dataset.", call. = FALSE)
  }
  list(
    dataset_id = record$dataset_id,
    imputation_id = record$imputation_id,
    active_version = record$active_imputation_version,
    display_mode = record$imputation_display_mode,
    imputation_count = record$imputation_count,
    missing_cell_mask = record$missing_cell_mask,
    imputed_cell_map = record$imputed_cell_map
  )
}

.rls_imputed_cell_display <- function(record, row, variable) {
  if (!identical(record$dataset_type %||% "data_frame", "multiple_imputation")) {
    return(as.character(record$data[[variable]][[row]]))
  }
  was_missing <- isTRUE(record$missing_cell_mask[[variable]][[row]])
  if (!was_missing) {
    return(as.character(record$original_data[[variable]][[row]]))
  }
  if (identical(record$imputation_display_mode, "all")) {
    values <- vapply(record$completed_datasets, function(data) as.character(data[[variable]][[row]]), character(1L))
    return(paste(values, collapse = " | "))
  }
  if (identical(record$imputation_display_mode, "original")) {
    return("NA")
  }
  as.character(record$data[[variable]][[row]])
}

.rls_mi_title_suffix <- function(record) {
  if (!identical(record$dataset_type %||% "data_frame", "multiple_imputation")) {
    return("")
  }
  if (identical(record$imputation_display_mode, "all")) {
    return(sprintf(" [all %d imputations]", record$imputation_count))
  }
  if (identical(record$imputation_display_mode, "original")) {
    return(" [original incomplete data]")
  }
  sprintf(" [imputation %d of %d]", record$active_imputation_version, record$imputation_count)
}

.rls_mi_warn_current_version <- function(record, analysis = "analysis") {
  if (!identical(record$dataset_type %||% "data_frame", "multiple_imputation")) {
    return(invisible(FALSE))
  }
  warning(sprintf(
    "%s is using imputation %d of %d for this command. Use the multiple-imputation aware analysis entry point when you need pooled results.",
    analysis,
    record$active_imputation_version,
    record$imputation_count
  ), call. = FALSE)
  invisible(TRUE)
}
