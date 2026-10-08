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
  name_expression <- substitute(name)
  data_expression <- substitute(data)
  if (is.data.frame(name) && is.character(data) && length(data) == 1L) {
    tmp <- name
    name <- data
    data <- tmp
    data_expression <- name_expression
  }
  expression_text <- paste(deparse(data_expression, width.cutoff = 500L), collapse = "\n")
  .rls_register_dataset(
    name, data, source = "R environment", activate = TRUE,
    provenance_origin = "recorded",
    provenance_code = paste0("data <- ", expression_text)
  )
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

.rls_import_name_is_identifier <- function(name) {
  normalized_name <- tolower(trimws(as.character(name %||% "")))
  grepl(
    "(^|[._ -])(id|identifier|index|row|case|record|serial|number|no)([._ -]|$)",
    normalized_name
  )
}

.rls_imported_variable_type <- function(x, name = "") {
  type <- .rls_variable_type(x)
  if (identical(type, "character")) {
    # Infer semantic categories only at file import; retain the original R
    # character vector. Explicit type changes and R registrations are untouched.
    observed <- as.character(x[!is.na(x)])
    observed <- observed[nzchar(trimws(observed))]
    categories <- unique(observed)
    if (.rls_import_name_is_identifier(name) || length(observed) < 4L ||
        length(categories) > min(20L, floor(length(observed) / 2L)) ||
        any(nchar(categories) > 80L | grepl("[\r\n]", categories)) ||
        grepl("(^|[._ -])(name|nombre|comment|comments|comentario|comentarios|note|notes|nota|notas|description|descripcion|address|direccion|email)([._ -]|$)",
              tolower(name))) return("character")
    # A mostly numeric column contaminated by a few textual values needs
    # review, not an automatic categorical interpretation (see import warnings).
    parsed <- suppressWarnings(as.double(sub(",", ".", trimws(observed), fixed = TRUE)))
    numeric <- is.finite(parsed)
    if (any(!numeric) && mean(numeric) >= 0.9) return("character")
    return("factor")
  }
  if (!identical(type, "numeric")) return(type)

  # Value labels are explicit categorical evidence in SPSS/Stata/SAS files,
  # even when the storage vector is numeric.
  if (length(.rls_variable_value_labels(x))) return("factor")

  "numeric"
}

.rls_import_numeric_type_warnings <- function(data, metadata = NULL) {
  if (!is.data.frame(data) || !ncol(data)) return(character())
  warnings <- character()
  for (name in names(data)) {
    type <- .rls_metadata_type(
      metadata, name,
      default = .rls_imported_variable_type(data[[name]], name)
    )
    if (!type %in% c("factor", "ordered", "character")) next

    values <- as.character(data[[name]])
    observed <- !is.na(values) & nzchar(trimws(values))
    if (sum(observed) < 4L) next
    candidates <- trimws(values[observed])
    comma_count <- nchar(candidates) - nchar(gsub(",", "", candidates, fixed = TRUE))
    decimal_comma <- comma_count == 1L & !grepl(".", candidates, fixed = TRUE)
    candidates[decimal_comma] <- sub(",", ".", candidates[decimal_comma], fixed = TRUE)
    parsed <- suppressWarnings(as.double(candidates))
    numeric <- is.finite(parsed)
    numeric_count <- sum(numeric)
    incompatible <- length(candidates) - numeric_count
    if (numeric_count < 4L || incompatible < 1L || numeric_count / length(candidates) < 0.90) next

    examples <- unique(trimws(values[observed])[!numeric])
    examples <- examples[seq_len(min(3L, length(examples)))]
    warning <- sprintf(
      "`%s` looks numeric, but %d of %d non-missing %s not numeric%s. It was imported as %s; review these values before changing it to Numeric.",
      name, incompatible, length(candidates),
      if (incompatible == 1L) "value is" else "values are",
      if (length(examples)) sprintf(" (%s)", paste(sprintf("`%s`", examples), collapse = ", ")) else "",
      switch(type, factor = "Categorical", ordered = "Ordinal", character = "Text")
    )
    warnings <- c(warnings, warning)
  }
  warnings
}

.rls_semantic_variable_levels <- function(x, type = .rls_variable_type(x)) {
  if (!type %in% c("factor", "ordered", "logical")) return(character())
  if (is.factor(x)) {
    declared <- as.character(levels(x))
    # Empty strings are missing values in LinkEDA's data/model semantics, not
    # selectable factor levels.  Keeping them here would expose a blank
    # reference option even though the fitting path correctly omits the rows.
    return(declared[!is.na(declared) & nzchar(trimws(declared))])
  }
  value_labels <- .rls_variable_value_labels(x)
  if (length(value_labels)) {
    labels <- names(value_labels)
    if (is.null(labels)) labels <- rep("", length(value_labels))
    raw <- unclass(value_labels)
    attributes(raw) <- NULL
    labels[is.na(labels) | !nzchar(labels)] <- as.character(raw)[is.na(labels) | !nzchar(labels)]
    return(unique(as.character(labels)))
  }
  if (is.logical(x)) return(c("FALSE", "TRUE"))
  observed <- as.character(x[!is.na(x)])
  observed <- observed[nzchar(trimws(observed))]
  unique(observed)
}

.rls_variable_metadata <- function(data, infer_imported_types = FALSE) {
  analysis_types <- vapply(names(data), function(name) {
    x <- data[[name]]
    type <- if (length(.rls_variable_value_labels(x))) {
      "factor"
    } else if (isTRUE(infer_imported_types)) {
      .rls_imported_variable_type(x, name)
    } else {
      .rls_variable_type(x)
    }
    # `logical` is an R storage class, not a fifth statistical type.
    # At import it becomes a binary categorical variable while retaining
    # its real storage class in `storage_class` below.
    if (identical(type, "logical")) "factor" else type
  }, character(1L))
  category_levels <- Map(.rls_semantic_variable_levels, data, unname(analysis_types))
  data.frame(
    variable_name = names(data),
    name = names(data),
    display_name = names(data),
    original_class = vapply(data, function(x) paste(class(x), collapse = ", "), character(1L)),
    current_analysis_type = unname(analysis_types),
    semantic_type = unname(analysis_types),
    type = unname(analysis_types),
    class = vapply(data, function(x) paste(class(x), collapse = ", "), character(1L)),
    storage_class = vapply(data, function(x) paste(class(x), collapse = ", "), character(1L)),
    is_numeric = unname(analysis_types == "numeric"),
    is_factor = unname(analysis_types %in% c("factor", "ordered")),
    is_character = unname(analysis_types == "character"),
    # Kept for compatibility with callers that inspect the technical R
    # representation.  Logical is storage, not a statistical type.
    is_logical = vapply(data, is.logical, logical(1L)),
    label = vapply(data, .rls_variable_label, character(1L)),
    labels = vapply(data, .rls_variable_label, character(1L)),
    description = vapply(data, .rls_variable_label, character(1L)),
    decimals = vapply(data, .rls_variable_decimals, integer(1L)),
    missing_count = vapply(data, function(x) sum(is.na(x)), integer(1L)),
    value_labels = I(lapply(data, .rls_variable_value_labels)),
    factor_levels = I(category_levels),
    category_order = I(category_levels),
    is_binary = lengths(category_levels) == 2L & analysis_types %in% c("factor", "ordered"),
    numeric_mapping = I(lapply(names(data), function(name) numeric())),
    type_change_provenance = I(lapply(names(data), function(name) list())),
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

.rls_metadata_levels <- function(metadata, variable, default = character()) {
  if (is.null(metadata) || !nrow(metadata) || !"factor_levels" %in% names(metadata)) {
    return(default)
  }
  name_col <- if ("variable_name" %in% names(metadata)) "variable_name" else "name"
  row <- match(variable, metadata[[name_col]])
  if (is.na(row)) return(default)
  levels <- metadata$factor_levels[[row]]
  if (is.null(levels)) default else as.character(levels)
}

.rls_metadata_list_value <- function(metadata, variable, column, default = NULL) {
  if (is.null(metadata) || !nrow(metadata) || !column %in% names(metadata)) return(default)
  name_col <- if ("variable_name" %in% names(metadata)) "variable_name" else "name"
  row <- match(variable, metadata[[name_col]])
  if (is.na(row)) return(default)
  value <- metadata[[column]][[row]]
  if (is.null(value)) default else value
}

.rls_refresh_metadata_row <- function(metadata, data, variable, type = NULL) {
  if (is.null(metadata) || !nrow(metadata)) {
    metadata <- .rls_variable_metadata(data)
  }
  if (!"variable_name" %in% names(metadata)) metadata$variable_name <- metadata$name
  if (!"display_name" %in% names(metadata)) metadata$display_name <- metadata$name
  if (!"original_class" %in% names(metadata)) metadata$original_class <- metadata$class
  if (!"current_analysis_type" %in% names(metadata)) metadata$current_analysis_type <- metadata$type
  if (!"semantic_type" %in% names(metadata)) metadata$semantic_type <- metadata$current_analysis_type
  if (!"storage_class" %in% names(metadata)) metadata$storage_class <- metadata$class
  if (!"labels" %in% names(metadata)) metadata$labels <- metadata$label %||% NA_character_
  if (!"description" %in% names(metadata)) metadata$description <- metadata$label %||% NA_character_
  if (!"decimals" %in% names(metadata)) metadata$decimals <- NA_integer_
  if (!"value_labels" %in% names(metadata)) metadata$value_labels <- I(vector("list", nrow(metadata)))
  if (!"factor_levels" %in% names(metadata)) metadata$factor_levels <- I(vector("list", nrow(metadata)))
  if (!"category_order" %in% names(metadata)) metadata$category_order <- metadata$factor_levels
  if (!"is_binary" %in% names(metadata)) metadata$is_binary <- FALSE
  if (!"numeric_mapping" %in% names(metadata)) metadata$numeric_mapping <- I(vector("list", nrow(metadata)))
  if (!"type_change_provenance" %in% names(metadata)) metadata$type_change_provenance <- I(vector("list", nrow(metadata)))
  name_col <- if ("variable_name" %in% names(metadata)) "variable_name" else "name"
  row <- match(variable, metadata[[name_col]])
  if (is.na(row)) {
    new_metadata <- .rls_variable_metadata(data[variable])
    # Imported datasets can carry provenance columns that are intentionally not
    # part of the metadata generated for a newly created variable (for example,
    # `original_name`).  Align both frames before appending so derived columns
    # such as factor scores do not fail with base rbind's column-count error.
    for (column in setdiff(names(metadata), names(new_metadata))) {
      new_metadata[[column]] <- metadata[[column]][rep(NA_integer_, nrow(new_metadata))]
    }
    for (column in setdiff(names(new_metadata), names(metadata))) {
      metadata[[column]] <- new_metadata[[column]][rep(NA_integer_, nrow(metadata))]
    }
    new_metadata <- new_metadata[names(metadata)]
    metadata <- rbind(metadata, new_metadata)
    row <- nrow(metadata)
  }
  current_type <- type %||% .rls_variable_type(data[[variable]])
  if (identical(current_type, "logical")) current_type <- "factor"
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
  metadata$semantic_type[[row]] <- current_type
  metadata$type[[row]] <- current_type
  metadata$class[[row]] <- paste(class(data[[variable]]), collapse = ", ")
  metadata$storage_class[[row]] <- paste(class(data[[variable]]), collapse = ", ")
  metadata$is_numeric[[row]] <- identical(current_type, "numeric")
  metadata$is_factor[[row]] <- current_type %in% c("factor", "ordered")
  metadata$is_character[[row]] <- identical(current_type, "character")
  metadata$is_logical[[row]] <- is.logical(data[[variable]])
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
  metadata$factor_levels[row] <- list(.rls_semantic_variable_levels(data[[variable]], current_type))
  metadata$category_order[row] <- metadata$factor_levels[row]
  metadata$is_binary[[row]] <- current_type %in% c("factor", "ordered") &&
    length(metadata$factor_levels[[row]]) == 2L
  metadata
}

.rls_register_dataset <- function(name, data, source = "R environment", path = NA_character_,
                                  activate = TRUE, replace = FALSE,
                                  infer_imported_types = FALSE,
                                  provenance_origin = NULL,
                                  provenance_code = "") {
  name <- .rls_safe_dataset_name(name)
  if (!isTRUE(replace)) {
    name <- .rls_unique_dataset_name(name)
  }
  name <- .rls_validate_group(name, allow_null = FALSE)
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  names(data) <- make.unique(names(data))
  previous <- if (exists(name, envir = .rls_state$datasets, inherits = FALSE))
    get(name, envir = .rls_state$datasets, inherits = FALSE) else NULL
  stable_row_ids <- if (!is.null(previous) &&
                        length(previous$stable_row_ids %||% character()) == nrow(data))
    as.character(previous$stable_row_ids) else paste0(name, ":row:", seq_len(nrow(data)))
  attr(data, "linkeda_row_ids") <- stable_row_ids
  metadata <- .rls_variable_metadata(data, infer_imported_types = infer_imported_types)
  provenance_origin <- provenance_origin %||% "unavailable"
  if (!provenance_origin %in% c("recorded", "reconstructed", "unavailable")) {
    stop("Invalid data-provenance origin.", call. = FALSE)
  }
  record <- list(
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
      stable_row_ids = stable_row_ids,
      data_version = if (is.null(previous)) 1L else as.integer(previous$data_version %||% 1L) + 1L,
      data_provenance = list(
        schema = "LinkEDADataProvenance/v1",
        origin = provenance_origin,
        origin_code = as.character(provenance_code %||% ""),
        origin_description = source,
        history = list()
      ),
      selection_state = integer(0),
      row_color_state = character(0),
      metadata = metadata,
      variable_metadata = metadata,
      source = source,
      path = path,
      row_colors = character(0),
      modified = FALSE
  )
  assign(name, record, envir = .rls_state$datasets)
  .rls_store_data_version(record)
  if (isTRUE(activate)) {
    .rls_state$active_dataset <- name
    .rls_notify_active_dataset(name)
  }
  invisible(name)
}

.rls_data_version_key <- function(dataset_id, version) {
  paste0(dataset_id, "@", as.integer(version))
}

.rls_store_data_version <- function(record) {
  version <- as.integer(record$data_version %||% 1L)
  key <- .rls_data_version_key(record$dataset_id %||% record$group, version)
  stable_row_ids <- as.character(
    record$stable_row_ids %||% paste0(record$group, ":row:", record$original_row_ids)
  )
  is_multiple_imputation <- identical(
    record$dataset_type %||% "data_frame", "multiple_imputation"
  ) && length(record$completed_datasets %||% list()) > 0L
  snapshot <- if (is_multiple_imputation) {
    completed <- lapply(record$completed_datasets, function(data) {
      data <- as.data.frame(data, stringsAsFactors = FALSE)
      attr(data, "linkeda_row_ids") <- stable_row_ids
      data
    })
    structure(list(
      data = record$original_data %||% record$data,
      completed_datasets = completed,
      mids_object = record$mids_object %||% NULL
    ), class = c("linkeda_multiple_imputation_version", "list"))
  } else {
    record$data
  }
  attr(snapshot, "linkeda_dataset_id") <- record$dataset_id %||% record$group
  attr(snapshot, "linkeda_dataset_version") <- version
  attr(snapshot, "linkeda_row_ids") <- stable_row_ids
  attr(snapshot, "linkeda_provenance") <- record$data_provenance %||% NULL
  attr(snapshot, "linkeda_variable_metadata") <- record$variable_metadata %||%
    record$metadata %||% NULL
  assign(key, snapshot, envir = .rls_state$data_versions)
  invisible(key)
}

.rls_advance_data_version <- function(record, operation, code = "",
                                      origin = c("recorded", "reconstructed", "unavailable"),
                                      columns = character(),
                                      input_columns = columns,
                                      output_columns = columns,
                                      stable_row_ids = character()) {
  origin <- match.arg(origin)
  previous_key <- .rls_data_version_key(record$dataset_id %||% record$group,
                                        record$data_version %||% 1L)
  record$data_version <- as.integer(record$data_version %||% 1L) + 1L
  provenance <- record$data_provenance %||% list(
    schema = "LinkEDADataProvenance/v1", origin = "unavailable",
    origin_code = "", origin_description = record$source %||% "", history = list())
  provenance$history[[length(provenance$history) + 1L]] <- list(
    id = paste0(record$group, ":transformation:", length(provenance$history) + 1L),
    label = operation,
    origin = origin,
    r_code = code,
    parent_version_keys = previous_key,
    input_columns = as.character(input_columns),
    output_columns = as.character(output_columns),
    stable_row_ids = as.character(stable_row_ids)
  )
  record$data_provenance <- provenance
  attr(record$data, "linkeda_row_ids") <- record$stable_row_ids
  .rls_store_data_version(record)
  record
}

#' Retrieve an immutable LinkEDA data version
#'
#' @param data Registered dataset name.
#' @param version Positive data-version number. The current version is used
#'   when omitted.
#' @return A preserved data frame (or mids-compatible object where available).
#' @export
ls_get_data_version <- function(data = NULL, version = NULL) {
  record <- .rls_dataset_record(data)
  version <- as.integer(version %||% record$data_version %||% 1L)
  if (length(version) != 1L || is.na(version) || version < 1L)
    stop("`version` must be one positive integer.", call. = FALSE)
  key <- .rls_data_version_key(record$dataset_id %||% record$group, version)
  if (!exists(key, envir = .rls_state$data_versions, inherits = FALSE))
    stop(sprintf("Data version `%s` is not available.", key), call. = FALSE)
  get(key, envir = .rls_state$data_versions, inherits = FALSE)
}

#' Stable LinkEDA row identities
#'
#' @param data A LinkEDA data-version object or registered dataset name.
#' @return Character vector of immutable row identities.
#' @export
ls_row_ids <- function(data = NULL) {
  object <- if (is.character(data) || is.null(data)) ls_get_data_version(data) else data
  ids <- attr(object, "linkeda_row_ids", exact = TRUE)
  row_count <- if (inherits(object, "linkeda_multiple_imputation_version")) {
    nrow(object$data)
  } else nrow(object)
  if (is.null(ids) || length(ids) != row_count)
    stop("This object does not contain valid LinkEDA row identities.", call. = FALSE)
  as.character(ids)
}

# Base subsetting drops attributes on plain numeric vectors, including imported
# value labels. Keep column-level semantics without restoring row-sized names,
# dimensions or time-series indices from the unsliced column.
.rls_subset_data_rows <- function(data, rows) {
  result <- data[rows, , drop = FALSE]
  semantic_attributes <- c("labels", "label", "na_values", "na_range",
                           "format.spss", "format.stata", "format.sas", "units")
  for (j in seq_along(data)) for (name in semantic_attributes) {
    value <- attr(data[[j]], name, exact = TRUE)
    if (!is.null(value)) attr(result[[j]], name) <- value
  }
  result
}

#' Select a data-version object by stable row identity
#'
#' @param data A LinkEDA data-version object.
#' @param row_ids Stable identities returned by [ls_row_ids()].
#' @return The same data type restricted in the requested identity order.
#' @export
ls_select_rows_by_id <- function(data, row_ids) {
  ids <- ls_row_ids(data)
  requested <- unique(as.character(row_ids))
  positions <- match(requested, ids)
  missing <- requested[is.na(positions)]
  if (length(missing)) warning(sprintf(
    "%d requested LinkEDA row %s no longer available.", length(missing),
    if (length(missing) == 1L) "identity is" else "identities are"), call. = FALSE)
  positions <- positions[!is.na(positions)]
  if (inherits(data, "linkeda_multiple_imputation_version")) {
    result <- data
    result$data <- .rls_subset_data_rows(result$data, positions)
    result$completed_datasets <- lapply(result$completed_datasets, function(value) {
      value <- .rls_subset_data_rows(value, positions)
      attr(value, "linkeda_row_ids") <- ids[positions]
      value
    })
    # A row-subsetted object is intentionally represented by the exact
    # completed datasets.  Retaining the original mids object here would make
    # a later mice::complete() silently reintroduce rows outside the scope.
    result$mids_object <- NULL
  } else {
    result <- .rls_subset_data_rows(data, positions)
  }
  attr(result, "linkeda_row_ids") <- ids[positions]
  attr(result, "linkeda_dataset_id") <- attr(data, "linkeda_dataset_id", exact = TRUE)
  attr(result, "linkeda_dataset_version") <- attr(data, "linkeda_dataset_version", exact = TRUE)
  attr(result, "linkeda_provenance") <- attr(data, "linkeda_provenance", exact = TRUE)
  attr(result, "linkeda_variable_metadata") <- attr(
    data, "linkeda_variable_metadata", exact = TRUE
  )
  result
}

#' Completed data sets from an immutable LinkEDA data version
#'
#' @param data An object returned by [ls_get_data_version()].
#' @return A list of data frames. Ordinary data versions return a one-element
#'   list; multiple-imputation versions return every preserved completion.
#' @export
ls_complete_data_version <- function(data) {
  if (inherits(data, "linkeda_multiple_imputation_version")) {
    completed <- data$completed_datasets %||% list()
    if (!length(completed) || !all(vapply(completed, is.data.frame, logical(1L)))) {
      stop("This multiple-imputation version has no preserved completed datasets.",
           call. = FALSE)
    }
    return(completed)
  }
  if (!is.data.frame(data)) {
    stop("`data` must be an immutable LinkEDA data version.", call. = FALSE)
  }
  list(data)
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

.rls_clean_imported_data <- function(data) {
  data <- .rls_validate_imported_data(data)
  original_names <- names(data)
  cleaned <- suppressWarnings(
    janitor::clean_names(data, case = "snake", allow_dupes = FALSE)
  )
  name_map <- data.frame(
    original_name = original_names,
    variable_name = names(cleaned),
    stringsAsFactors = FALSE
  )
  selected <- .rls_state$import_selected_columns %||% character()
  if (length(selected)) {
    missing <- setdiff(selected, names(cleaned))
    if (length(missing)) {
      stop(sprintf("Selected variables were not found after cleaning names: %s",
                   paste(missing, collapse = ", ")), call. = FALSE)
    }
    cleaned <- cleaned[selected]
    name_map <- name_map[match(selected, name_map$variable_name), , drop = FALSE]
  }
  list(data = cleaned, name_map = name_map)
}

.rls_apply_import_name_map <- function(data, name_map) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (ncol(data) != nrow(name_map)) {
    stop("The completed imputation does not contain the same variables as the original data.",
         call. = FALSE)
  }
  names(data) <- name_map$variable_name
  data
}

.rls_store_import_name_map <- function(dataset_id, name_map) {
  record <- .rls_dataset_record(dataset_id)
  metadata <- record$variable_metadata
  metadata$original_name <- name_map$original_name[
    match(metadata$variable_name, name_map$variable_name)
  ]
  record$variable_metadata <- metadata
  record$metadata <- metadata
  record$import_name_map <- name_map
  .rls_set_dataset_record(record)
  invisible(record)
}

.rls_import_reconstruction_code <- function(path, source) {
  path_code <- encodeString(normalizePath(path, mustWork = FALSE), quote = '"')
  extension <- tolower(tools::file_ext(path))
  source <- as.character(source %||% "")[[1L]]
  reader <- switch(
    extension,
    csv = sprintf(
      "utils::read.csv(%s, check.names = FALSE, stringsAsFactors = FALSE, na.strings = c(\"NA\", \"\"))",
      path_code
    ),
    tsv = , txt = sprintf(
      "utils::read.delim(%s, check.names = FALSE, stringsAsFactors = FALSE, na.strings = c(\"NA\", \"\"))",
      path_code
    ),
    sav = , zsav = sprintf("haven::read_sav(%s)", path_code),
    dta = sprintf("haven::read_dta(%s)", path_code),
    sas7bdat = sprintf("haven::read_sas(%s)", path_code),
    xpt = sprintf("haven::read_xpt(%s)", path_code),
    xls = , xlsx = {
      sheet <- sub("^Excel file:[[:space:]]*", "", source)
      sheet_argument <- if (!identical(sheet, source) && nzchar(sheet))
        paste0(", sheet = ", encodeString(sheet, quote = '"')) else ""
      sprintf("readxl::read_excel(%s%s, .name_repair = \"minimal\")",
              path_code, sheet_argument)
    },
    rds = sprintf("readRDS(%s)", path_code),
    rda = , rdata = {
      object_name <- sub("^R data file:[[:space:]]*", "", source)
      object_code <- if (!identical(object_name, source) && nzchar(object_name))
        encodeString(object_name, quote = '"') else "loaded_names[[1L]]"
      paste0(
        "local({ import_environment <- new.env(parent = emptyenv()); ",
        "loaded_names <- load(", path_code, ", envir = import_environment); ",
        "get(", object_code, ", envir = import_environment) })"
      )
    },
    sprintf("stop(\"No reconstructed reader is available for %s\")", extension)
  )
  paste(
    "# Reconstructed from the preserved import metadata.",
    "# This may not be identical to the original call.",
    paste0("imported_data <- ", reader),
    "data <- janitor::clean_names(as.data.frame(imported_data, stringsAsFactors = FALSE), case = \"snake\", allow_dupes = FALSE)",
    sep = "\n"
  )
}

.rls_register_imported_dataset <- function(data, path, name, source, make_active = TRUE) {
  import_notice <- .rls_mi_stacked_import_notice(data)
  if (nzchar(import_notice)) message(import_notice)
  cleaned <- .rls_clean_imported_data(data)
  data <- cleaned$data
  name <- name %||% tools::file_path_sans_ext(basename(path))
  dataset_id <- .rls_register_dataset(
    name,
    data,
    source = source,
    path = normalizePath(path, mustWork = FALSE),
    activate = isTRUE(make_active),
    infer_imported_types = TRUE,
    provenance_origin = "reconstructed",
    provenance_code = .rls_import_reconstruction_code(path, source)
  )
  .rls_store_import_name_map(dataset_id, cleaned$name_map)
  record <- .rls_dataset_record(dataset_id)
  record$import_warnings <- .rls_import_numeric_type_warnings(
    record$data, record$variable_metadata
  )
  record$import_notice <- import_notice
  .rls_set_dataset_record(record)
  invisible(dataset_id)
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

.rls_data_export_format <- function(path, format = NULL) {
  aliases <- c(
    csv = "csv", tsv = "tsv", txt = "tsv", xlsx = "xlsx",
    sav = "sav", zsav = "zsav", dta = "dta", xpt = "xpt",
    rds = "rds", rda = "rdata", rdata = "rdata"
  )
  requested <- if (is.null(format)) tools::file_ext(path) else format
  requested <- tolower(trimws(as.character(requested %||% "")))
  if (length(requested) != 1L || is.na(requested) || !nzchar(requested) ||
      !requested %in% names(aliases)) {
    stop(
      "Unsupported export format. Use .csv, .tsv, .xlsx, .sav, .zsav, .dta, .xpt, .rds, or .RData.",
      call. = FALSE
    )
  }
  unname(aliases[[requested]])
}

.rls_unique_statistical_names <- function(names, max_length = 32L) {
  names <- iconv(as.character(names), from = "", to = "ASCII//TRANSLIT", sub = "_")
  names[is.na(names)] <- "variable"
  names <- gsub("[^A-Za-z0-9_]", "_", names)
  names <- gsub("_+", "_", names)
  names[!grepl("^[A-Za-z_]", names)] <- paste0("v_", names[!grepl("^[A-Za-z_]", names)])
  names[!nzchar(names)] <- "variable"
  out <- character(length(names))
  used <- character()
  for (i in seq_along(names)) {
    base <- substr(names[[i]], 1L, max_length)
    candidate <- base
    suffix <- 2L
    while (candidate %in% used) {
      marker <- paste0("_", suffix)
      candidate <- paste0(substr(base, 1L, max(1L, max_length - nchar(marker))), marker)
      suffix <- suffix + 1L
    }
    out[[i]] <- candidate
    used <- c(used, candidate)
  }
  out
}

.rls_prepare_haven_export <- function(data, max_name_length = 32L) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  original_names <- names(data)
  names(data) <- .rls_unique_statistical_names(original_names, max_name_length)
  for (i in seq_along(data)) {
    x <- data[[i]]
    variable_label <- attr(x, "label", exact = TRUE)
    if (is.factor(x)) {
      level_names <- levels(x)
      codes <- as.integer(x)
      x <- haven::labelled(codes, labels = stats::setNames(seq_along(level_names), level_names))
    } else if (is.logical(x)) {
      x <- haven::labelled(as.integer(x), labels = c(No = 0L, Yes = 1L))
    } else if (is.list(x) && !inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
      x <- vapply(x, function(value) paste(as.character(value), collapse = " | "), character(1L))
    }
    if ((is.null(variable_label) || !length(variable_label) || is.na(variable_label[[1L]]) ||
         !nzchar(as.character(variable_label[[1L]]))) &&
        !identical(names(data)[[i]], original_names[[i]])) {
      variable_label <- original_names[[i]]
    }
    if (!is.null(variable_label) && length(variable_label) && !is.na(variable_label[[1L]]) &&
        nzchar(as.character(variable_label[[1L]]))) {
      attr(x, "label") <- substr(as.character(variable_label[[1L]]), 1L, 255L)
    }
    data[[i]] <- x
  }
  data
}

.rls_export_data_frame <- function(data, path, format = NULL) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("`path` must be a single non-empty file path.", call. = FALSE)
  }
  format <- .rls_data_export_format(path, format)
  path <- path.expand(path)
  parent <- dirname(path)
  if (!dir.exists(parent)) stop(sprintf("Export folder does not exist: %s", parent), call. = FALSE)

  if (identical(format, "csv")) {
    utils::write.csv(data, path, row.names = FALSE, na = "NA")
  } else if (identical(format, "tsv")) {
    utils::write.table(data, path, sep = "\t", row.names = FALSE, quote = TRUE, na = "NA")
  } else if (identical(format, "xlsx")) {
    if (!requireNamespace("writexl", quietly = TRUE)) {
      stop("Excel export requires package 'writexl'. Install it with install.packages(\"writexl\").", call. = FALSE)
    }
    writexl::write_xlsx(list(Data = data), path)
  } else if (format %in% c("sav", "zsav", "dta", "xpt")) {
    if (!requireNamespace("haven", quietly = TRUE)) {
      stop("Statistical-package export requires package 'haven'. Install it with install.packages(\"haven\").", call. = FALSE)
    }
    max_length <- if (format %in% c("dta", "xpt")) 32L else 64L
    prepared <- .rls_prepare_haven_export(data, max_name_length = max_length)
    if (identical(format, "sav")) {
      haven::write_sav(prepared, path, compress = "byte")
    } else if (identical(format, "zsav")) {
      haven::write_sav(prepared, path, compress = "zsav")
    } else if (identical(format, "dta")) {
      haven::write_dta(prepared, path)
    } else {
      haven::write_xpt(prepared, path)
    }
  } else if (identical(format, "rds")) {
    saveRDS(data, path)
  } else if (identical(format, "rdata")) {
    LinkEDA_data <- data
    save(LinkEDA_data, file = path)
  }
  invisible(normalizePath(path, mustWork = TRUE))
}

.rls_export_r_object <- function(object, path, format) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("`path` must be a single non-empty file path.", call. = FALSE)
  }
  format <- .rls_data_export_format(path, format)
  if (!format %in% c("rds", "rdata")) {
    stop("R objects can only be exported as RDS or RData.", call. = FALSE)
  }
  path <- path.expand(path)
  parent <- dirname(path)
  if (!dir.exists(parent)) stop(sprintf("Export folder does not exist: %s", parent), call. = FALSE)
  if (identical(format, "rds")) {
    saveRDS(object, path)
  } else {
    LinkEDA_data <- object
    save(LinkEDA_data, file = path)
  }
  invisible(normalizePath(path, mustWork = TRUE))
}

.rls_export_dataset_data <- function(record) {
  if (!identical(record$dataset_type %||% "data_frame", "multiple_imputation")) {
    return(as.data.frame(record$data, stringsAsFactors = FALSE, check.names = FALSE))
  }
  completed <- .rls_mi_completed_datasets(record)
  original <- as.data.frame(record$original_data %||% record$data,
                            stringsAsFactors = FALSE, check.names = FALSE)
  row_ids <- as.integer(record$original_row_ids %||% seq_len(nrow(original)))
  add_identity <- function(data, imputation) {
    data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
    data.frame(.imp = imputation, .id = row_ids, data,
               check.names = FALSE, stringsAsFactors = FALSE)
  }
  frames <- c(list(add_identity(original, 0L)),
              lapply(seq_along(completed), function(i) add_identity(completed[[i]], i)))
  do.call(rbind, frames)
}

.rls_export_dataset_mids <- function(record) {
  if (!identical(record$dataset_type %||% "data_frame", "multiple_imputation")) {
    stop("Dataset is not a multiple-imputation dataset.", call. = FALSE)
  }
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("RDS/RData multiple-imputation export requires package 'mice'.", call. = FALSE)
  }
  long <- .rls_export_dataset_data(record)
  # Keep chain statistics and the original imputation specification when the
  # retained mids still describes every current representation exactly.
  retained <- record$mids_object
  same_values <- function(x,y) isTRUE(all.equal(as.data.frame(x),as.data.frame(y),check.attributes=FALSE)) &&
    identical(names(x),names(y))
  unchanged <- inherits(retained,"mids") &&
    same_values(retained$data, record$original_data) &&
    retained$m == length(record$completed_datasets) &&
    all(vapply(seq_len(retained$m),function(i)
      same_values(mice::complete(retained,i),record$completed_datasets[[i]]),logical(1L)))
  rebuilt <- if(unchanged) retained else tryCatch(
    mice::as.mids(long, .imp = ".imp", .id = ".id"),
    error = function(error) stop(sprintf(
      "The current multiple-imputation data could not be reconstructed as a mice `mids` object: %s",
      conditionMessage(error)
    ), call. = FALSE)
  )
  original <- as.data.frame(record$original_data %||% record$data,
                            stringsAsFactors = FALSE, check.names = FALSE)
  metadata <- record$variable_metadata %||% record$metadata %||% NULL
  attr(original, "linkeda_variable_metadata") <- metadata
  rebuilt$data <- original
  attr(rebuilt, "linkeda_variable_metadata") <- metadata
  attr(rebuilt, "linkeda_exported_dataset_type") <- "multiple_imputation"
  rebuilt
}

#' Export a LinkEDA dataset
#'
#' Exports an ordinary registered dataset or a multiple-imputation dataset.
#' RDS and RData exports preserve multiple-imputation datasets as a
#' `mice::mids` object. Tabular formats write them in long form with `.imp` and
#' `.id` columns, including `.imp = 0` for the original incomplete data.
#'
#' @param dataset Registered dataset name, or `NULL` for the active dataset.
#' @param path Destination file. Its extension determines the format unless
#'   `format` is supplied.
#' @param format Optional format: `csv`, `tsv`, `xlsx`, `sav`, `zsav`, `dta`,
#'   `xpt`, `rds`, or `rdata`.
#' @return Invisibly returns the normalized destination path.
#' @export
ls_export_data <- function(dataset = NULL, path, format = NULL) {
  record <- .rls_dataset_record(dataset)
  resolved_format <- .rls_data_export_format(path, format)
  if (identical(record$dataset_type %||% "data_frame", "multiple_imputation") &&
      resolved_format %in% c("rds", "rdata")) {
    return(.rls_export_r_object(
      .rls_export_dataset_mids(record), path, resolved_format
    ))
  }
  .rls_export_data_frame(.rls_export_dataset_data(record), path, resolved_format)
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
    global <- identical(envir, .GlobalEnv)
    .rls_register_dataset(
      name, get(name, envir = envir), source = "R environment",
      activate = FALSE, replace = TRUE,
      provenance_origin = if (global) "recorded" else "unavailable",
      provenance_code = if (global) sprintf(
        "data <- get(%s, envir = .GlobalEnv)", encodeString(name, quote = '"')
      ) else ""
    )
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
  text <- trimws(as.character(x))
  original_missing <- is.na(x) | !nzchar(text)
  comma_count <- nchar(text) - nchar(gsub(",", "", text, fixed = TRUE))
  decimal_comma <- !original_missing & comma_count == 1L &
    !grepl(".", text, fixed = TRUE)
  text[decimal_comma] <- sub(",", ".", text[decimal_comma], fixed = TRUE)
  converted <- suppressWarnings(as.double(text))
  converted[original_missing] <- NA_real_
  incompatible <- !original_missing & !is.finite(converted)
  observed_count <- sum(!original_missing)
  numeric_count <- sum(!original_missing & is.finite(converted))
  mostly_numeric <- observed_count >= 4L && numeric_count >= 4L &&
    numeric_count / observed_count >= 0.90
  if (any(incompatible) && !mostly_numeric) {
    stop(sprintf(
      "Variable `%s` cannot be treated as numeric because some values cannot be converted.",
      variable
    ), call. = FALSE)
  }
  if (any(incompatible)) {
    converted[incompatible] <- NA_real_
    warning(sprintf(
      "Variable `%s` is now treated as numeric; %d non-numeric %s set to missing.",
      variable, sum(incompatible),
      if (sum(incompatible) == 1L) "value was" else "values were"
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

.rls_type_levels_across_record <- function(record, variable, previous_levels = character()) {
  frames <- c(
    list(record$data),
    if (is.data.frame(record$original_data)) list(record$original_data) else list(),
    record$completed_datasets %||% list()
  )
  observed <- unlist(lapply(frames, function(frame) {
    if (!is.data.frame(frame) || !variable %in% names(frame)) return(character())
    values <- as.character(frame[[variable]])
    unique(values[!is.na(values) & nzchar(trimws(values))])
  }), use.names = FALSE)
  unique(c(as.character(previous_levels), observed))
}

.rls_validate_numeric_mapping <- function(mapping, levels, variable) {
  if (is.null(mapping)) return(NULL)
  if (is.list(mapping) && !is.atomic(mapping)) mapping <- unlist(mapping, use.names = TRUE)
  if (!is.numeric(mapping) || is.null(names(mapping)) || any(!nzchar(names(mapping)))) {
    stop("`mapping` must be a named numeric vector such as c(Control = 0, Treatment = 1).",
         call. = FALSE)
  }
  mapping_names <- names(mapping)
  mapping <- as.double(mapping)
  names(mapping) <- mapping_names
  if (any(!is.finite(mapping)) || anyDuplicated(names(mapping))) {
    stop("`mapping` must contain one finite, uniquely named score per category.", call. = FALSE)
  }
  missing <- setdiff(levels, names(mapping))
  extra <- setdiff(names(mapping), levels)
  if (length(missing) || length(extra)) {
    stop(sprintf(
      "The numeric mapping for `%s` must name every category exactly once (missing: %s; extra: %s).",
      variable,
      if (length(missing)) paste(missing, collapse = ", ") else "none",
      if (length(extra)) paste(extra, collapse = ", ") else "none"
    ), call. = FALSE)
  }
  mapping[levels]
}

.rls_numeric_conversion_plan <- function(x, variable, semantic_type, levels,
                                         mapping = NULL, invert_binary = FALSE,
                                         ordinal_scores = NULL) {
  if (is.numeric(x) && identical(semantic_type, "numeric")) {
    return(list(mapping = NULL, warning = NULL, method = "already numeric"))
  }
  if (!semantic_type %in% c("factor", "ordered")) {
    return(list(mapping = NULL, warning = NULL, method = "parse labels"))
  }
  parsed <- suppressWarnings(as.double(sub(",", ".", trimws(levels), fixed = TRUE)))
  if (length(levels) && all(is.finite(parsed))) {
    map <- stats::setNames(parsed, levels)
    return(list(mapping = map, warning = NULL, method = "numeric category labels"))
  }
  if (!is.null(ordinal_scores)) mapping <- ordinal_scores
  mapping <- .rls_validate_numeric_mapping(mapping, levels, variable)
  if (!is.null(mapping)) {
    return(list(mapping = mapping, warning = NULL, method = "explicit mapping"))
  }
  if (identical(semantic_type, "ordered")) {
    map <- stats::setNames(seq_along(levels), levels)
    return(list(
      mapping = map,
      warning = "This conversion treats consecutive ordinal categories as equally spaced.",
      method = "consecutive ordinal scores"
    ))
  }
  if (length(levels) == 2L) {
    scores <- if (isTRUE(invert_binary)) c(1, 0) else c(0, 1)
    return(list(
      mapping = stats::setNames(scores, levels),
      warning = sprintf("Numeric mapping: %s.", paste0(levels, " = ", scores, collapse = "; ")),
      method = "binary category mapping"
    ))
  }
  stop(sprintf(
    "Categorical variable `%s` has %d non-numeric categories. Supply an explicit named numeric `mapping` before converting it to Numeric.",
    variable, length(levels)
  ), call. = FALSE)
}

.rls_apply_numeric_plan <- function(x, variable, plan) {
  if (is.null(plan$mapping)) return(.rls_safe_as_numeric(x, variable))
  text <- as.character(x)
  missing <- is.na(x) | !nzchar(trimws(text))
  unknown <- !missing & !text %in% names(plan$mapping)
  if (any(unknown)) {
    stop(sprintf("Variable `%s` contains categories absent from the numeric mapping: %s.",
                 variable, paste(unique(text[unknown]), collapse = ", ")), call. = FALSE)
  }
  out <- unname(plan$mapping[text])
  out[missing] <- NA_real_
  as.double(out)
}

# Synchronize derived columns without fitting an imputation model. The completed
# values come from the R analysis, and as.mids only reconstructs their storage.
.rls_sync_derived_mi_columns <- function(record, columns) {
  if (!.rls_mi_is_dataset(record)) return(record)
  if (is.data.frame(record$import_name_map)) {
    for (name in setdiff(columns, record$import_name_map$variable_name)) {
      mapping <- record$import_name_map[NA_integer_, , drop = FALSE]
      mapping$original_name <- mapping$variable_name <- name
      record$import_name_map <- rbind(record$import_name_map, mapping)
    }
  }
  retained <- record$mids_object
  if (inherits(retained, "mids")) {
    frames <- c(list(record$original_data), record$completed_datasets)
    long <- do.call(rbind, lapply(seq_along(frames), function(i)
      data.frame(.imp = i - 1L, .id = rownames(record$original_data),
                 frames[[i]], check.names = FALSE)))
    rebuilt <- mice::as.mids(long)
    # Let mice extend its own bookkeeping, keeping the original methods,
    # predictor matrix, blocks and chain history intact. Then transfer only
    # the stored derived values reconstructed by as.mids (no re-imputation).
    new_columns <- setdiff(columns, names(retained$data))
    if (length(new_columns)) retained <- mice::cbind(
      retained, record$original_data[new_columns])
    for (name in columns) {
      retained$data[[name]] <- record$original_data[[name]]
      retained$imp[[name]] <- rebuilt$imp[[name]]
      retained$where[, name] <- rebuilt$where[, name]
      retained$nmis[name] <- rebuilt$nmis[name]
    }
    attr(retained, "linkeda_derived_columns") <- unique(c(
      attr(retained, "linkeda_derived_columns"), columns))
    record$mids_object <- retained
  }
  record
}

.rls_sync_derived_imputation_record <- function(record) {
  if (.rls_mi_is_dataset(record) && nzchar(record$imputation_id %||% "") &&
      exists(record$imputation_id, envir = .rls_state$missing_imputations, inherits = FALSE)) {
    imputation <- .rls_imputation_record(record$imputation_id)
    for (field in c("original_data", "completed_datasets", "mids_object", "missing_cell_mask", "variable_metadata", "import_name_map"))
      imputation[[field]] <- record[[field]]
    imputation$imputed_cell_map <- .rls_mi_build_cell_map(
      record$original_data, record$completed_datasets, record$missing_cell_mask)
    .rls_set_imputation_record(imputation)
  }
  invisible(record)
}

.rls_rebuild_mids_after_type_change <- function(record) {
  if (!identical(record$dataset_type %||% "data_frame", "multiple_imputation") ||
      !length(record$completed_datasets %||% list()) ||
      !requireNamespace("mice", quietly = TRUE)) return(record)
  original <- as.data.frame(record$original_data, check.names = FALSE)
  completed <- lapply(record$completed_datasets, as.data.frame, check.names = FALSE)
  ids <- record$original_row_names %||% row.names(original)
  make_long <- function(frame, imp) {
    data.frame(.imp = imp, .id = ids, frame, check.names = FALSE)
  }
  long <- do.call(rbind, c(
    list(make_long(original, 0L)),
    Map(make_long, completed, seq_along(completed))
  ))
  rebuilt <- tryCatch(mice::as.mids(long), error = function(e) NULL)
  if (!is.null(rebuilt)) {
    # `as.mids()` reconstructs the imputation bookkeeping, but it is not
    # responsible for LinkEDA's semantic metadata. Reattach both the exact
    # converted .imp = 0 data and its metadata so public mice operations and
    # later LinkEDA analyses observe the same variable definition.
    attr(original, "linkeda_variable_metadata") <- record$variable_metadata %||% record$metadata
    rebuilt$data <- original
    attr(rebuilt, "linkeda_variable_metadata") <- record$variable_metadata %||% record$metadata
    attr(rebuilt, "linkeda_type_change_rebuilt") <- TRUE
    record$mids_object <- rebuilt
  }
  record
}

.rls_set_dataset_record <- function(record) {
  if (is.null(record$stable_row_ids) || length(record$stable_row_ids) != nrow(record$data)) {
    original <- record$original_row_ids %||% seq_len(nrow(record$data))
    record$stable_row_ids <- paste0(record$group, ":row:", original)
  }
  attr(record$data, "linkeda_row_ids") <- record$stable_row_ids
  assign(record$group, record, envir = .rls_state$datasets)
  invisible(record)
}

.rls_dataset_subset_original_rows <- function(record, original_rows) {
  requested <- unique(suppressWarnings(as.integer(original_rows)))
  requested <- requested[is.finite(requested) & requested > 0L]
  original_ids <- as.integer(record$original_row_ids %||% seq_len(nrow(record$data)))
  if (length(original_ids) != nrow(record$data) || anyDuplicated(original_ids)) {
    stop("Dataset row identity is inconsistent with its analysis data.", call. = FALSE)
  }
  keep <- original_ids %in% requested

  subset_frame <- function(value, label) {
    if (is.null(value)) return(NULL)
    if (!is.data.frame(value) || nrow(value) != length(original_ids)) {
      stop(sprintf("Dataset `%s` does not preserve the original subject rows.", label),
           call. = FALSE)
    }
    .rls_subset_data_rows(value, keep)
  }

  record$data <- subset_frame(record$data, "data")
  if (!is.null(record$data_frame)) {
    record$data_frame <- subset_frame(record$data_frame, "data_frame")
  }
  if (!is.null(record$original_data)) {
    record$original_data <- subset_frame(record$original_data, "original_data")
  }
  if (!is.null(record$missing_cell_mask)) {
    # Importers retain this mask as a data frame, matrix or named column list.
    mask <- record$missing_cell_mask
    if (!is.data.frame(mask) && (is.matrix(mask) || is.list(mask)))
      mask <- as.data.frame(mask, check.names=FALSE)
    record$missing_cell_mask <- subset_frame(mask, "missing_cell_mask")
  }
  if (length(record$completed_datasets %||% list())) {
    record$completed_datasets <- lapply(
      seq_along(record$completed_datasets),
      function(i) subset_frame(record$completed_datasets[[i]], paste0("imputation ", i))
    )
  }
  if (length(record$row_color_state) == length(original_ids)) {
    record$row_color_state <- record$row_color_state[keep]
  }
  if (length(record$row_colors) == length(original_ids)) {
    record$row_colors <- record$row_colors[keep]
  }
  record$original_row_ids <- original_ids[keep]
  stable_ids <- as.character(record$stable_row_ids %||% paste0(record$group, ":row:", original_ids))
  record$stable_row_ids <- stable_ids[keep]
  attr(record$data, "linkeda_row_ids") <- record$stable_row_ids
  record$selection_state <- integer()
  record$n_rows <- nrow(record$data)
  record
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
#' @param type One of `"numeric"`, `"factor"`, `"ordered"`, or `"character"`.
#'   These correspond to Numeric, Categorical, Ordinal, and Text. The legacy
#'   value `"logical"` remains accepted for storage compatibility but is shown
#'   as a binary Categorical variable rather than as a statistical type.
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
ls_set_variable_type <- function(data = NULL, variable,
                                 type = c("numeric", "factor", "ordered", "character"),
                                 mapping = NULL, invert_binary = FALSE,
                                 category_order = NULL, ordinal_scores = NULL,
                                 breaks = NULL, labels = NULL) {
  requested_type <- tolower(as.character(type)[[1L]])
  if (identical(requested_type, "logical")) {
    warning("`logical` is an R storage class, not a LinkEDA statistical type; using binary Categorical.",
            call. = FALSE)
    requested_type <- "factor"
  }
  type <- match.arg(requested_type, c("numeric", "factor", "ordered", "character"))
  record <- .rls_dataset_record(data)
  variable <- .rls_validate_protocol_name(variable, "variable")
  if (!variable %in% names(record$data)) {
    stop(sprintf("Column `%s` was not found in the dataset.", variable), call. = FALSE)
  }
  current <- record$data[[variable]]
  source_metadata <- record$variable_metadata %||% record$metadata
  previous_type <- .rls_metadata_type(
    source_metadata, variable,
    .rls_variable_type(current)
  )
  previous_levels <- .rls_metadata_levels(
    source_metadata, variable,
    if (is.factor(current)) levels(current) else character()
  )
  saved_mapping <- .rls_metadata_list_value(
    source_metadata, variable, "numeric_mapping", numeric()
  )
  saved_category_order <- .rls_metadata_list_value(
    source_metadata, variable, "category_order", character()
  )
  restore_categories <- previous_type == "numeric" && type %in% c("factor", "ordered") &&
    length(saved_mapping) && length(saved_category_order)
  if (restore_categories) previous_levels <- as.character(saved_category_order)
  all_levels <- .rls_type_levels_across_record(record, variable, previous_levels)
  if (restore_categories) all_levels <- as.character(saved_category_order)
  if (!is.null(category_order)) {
    category_order <- as.character(category_order)
    if (anyDuplicated(category_order) || !setequal(category_order, all_levels)) {
      stop("`category_order` must contain every category exactly once.", call. = FALSE)
    }
    all_levels <- category_order
  }
  factor_levels_for <- function(x, is_ordered = FALSE) {
    defined <- all_levels
    values <- x
    if (restore_categories) {
      numeric_values <- suppressWarnings(as.double(as.character(x)))
      labels_by_score <- stats::setNames(names(saved_mapping),
                                         format(as.double(saved_mapping), digits = 17,
                                                scientific = FALSE, trim = TRUE))
      keys <- format(numeric_values, digits = 17, scientific = FALSE, trim = TRUE)
      values <- unname(labels_by_score[keys])
      values[is.na(x)] <- NA_character_
      unresolved <- !is.na(x) & is.na(values)
      if (any(unresolved)) {
        stop(sprintf("Variable `%s` contains numeric values absent from its saved category mapping.",
                     variable), call. = FALSE)
      }
    }
    if (isTRUE(is_ordered)) ordered(values, levels = defined) else factor(values, levels = defined)
  }
  numeric_plan <- if (identical(type, "numeric")) {
    .rls_numeric_conversion_plan(
      current, variable, previous_type, all_levels,
      mapping = mapping, invert_binary = invert_binary,
      ordinal_scores = ordinal_scores
    )
  } else NULL
  convert <- function(x) {
    switch(
      type,
      numeric = .rls_apply_numeric_plan(x, variable, numeric_plan),
      factor = factor_levels_for(x),
      ordered = {
        if (!is.null(breaks)) {
          if (!is.numeric(x)) stop("Numeric cut points require a Numeric source variable.", call. = FALSE)
          breaks_checked <- as.double(breaks)
          if (length(breaks_checked) < 2L || any(is.na(breaks_checked)) ||
              is.unsorted(breaks_checked, strictly = TRUE)) {
            stop("`breaks` must contain strictly increasing cut points.", call. = FALSE)
          }
          interval_count <- length(breaks_checked) - 1L
          if (!is.null(labels) && length(labels) != interval_count) {
            stop("`labels` must contain one label per ordinal interval.", call. = FALSE)
          }
          cut(x, breaks = breaks_checked, labels = labels, ordered_result = TRUE,
              include.lowest = TRUE)
        } else factor_levels_for(x, is_ordered = TRUE)
      },
      character = as.character(x)
    )
  }
  converted <- convert(current)
  attrs <- attributes(current)
  keep_attrs <- intersect(names(attrs), c("label", "labels", "format.spss", "display_width"))
  for (attr_name in keep_attrs) {
    attr(converted, attr_name) <- attrs[[attr_name]]
  }
  apply_to_frame <- function(frame) {
    if (!is.data.frame(frame) || !variable %in% names(frame)) return(frame)
    # The active vector above is the one user-facing conversion. Other frames
    # (original data and MI completions) must receive the same transformation
    # without repeating an identical warning once per stored representation.
    frame[[variable]] <- suppressWarnings(convert(frame[[variable]]))
    frame
  }
  record$data[[variable]] <- converted
  record$data_frame <- apply_to_frame(record$data_frame %||% record$data)
  if (is.data.frame(record$original_data)) {
    record$original_data <- apply_to_frame(record$original_data)
  }
  if (length(record$completed_datasets %||% list())) {
    record$completed_datasets <- lapply(record$completed_datasets, apply_to_frame)
    active <- as.integer(record$active_imputation_version %||% record$active_version %||% 1L)
    active <- max(1L, min(length(record$completed_datasets), active))
    if (!identical(record$imputation_display_mode %||% record$display_mode, "original")) {
      record$data <- record$completed_datasets[[active]]
      record$data_frame <- record$data
    }
  }
  record$metadata <- .rls_refresh_metadata_row(record$metadata, record$data, variable, type)
  record$variable_metadata <- .rls_refresh_metadata_row(record$variable_metadata, record$data, variable, type)
  # A temporary Categorical -> Text change changes analysis eligibility immediately,
  # but must not discard the user's explicit category order.  Keep the levels
  # as dormant metadata while Text; the semantic type still prevents every
  # categorical consumer from using them until the user changes the type back.
  if (identical(type, "character") && length(previous_levels)) {
    preserve_levels <- function(metadata) {
      if (is.null(metadata) || !nrow(metadata) || !"factor_levels" %in% names(metadata)) {
        return(metadata)
      }
      name_col <- if ("variable_name" %in% names(metadata)) "variable_name" else "name"
      row <- match(variable, metadata[[name_col]])
      if (!is.na(row)) metadata$factor_levels[row] <- list(as.character(previous_levels))
      metadata
    }
    record$metadata <- preserve_levels(record$metadata)
    record$variable_metadata <- preserve_levels(record$variable_metadata)
  }
  update_conversion_metadata <- function(metadata) {
    if (is.null(metadata) || !nrow(metadata)) return(metadata)
    name_col <- if ("variable_name" %in% names(metadata)) "variable_name" else "name"
    row <- match(variable, metadata[[name_col]])
    if (is.na(row)) return(metadata)
    if (!"numeric_mapping" %in% names(metadata)) metadata$numeric_mapping <- I(vector("list", nrow(metadata)))
    if (!"category_order" %in% names(metadata)) metadata$category_order <- I(vector("list", nrow(metadata)))
    if (!"type_change_provenance" %in% names(metadata)) metadata$type_change_provenance <- I(vector("list", nrow(metadata)))
    next_mapping <- if (!is.null(numeric_plan)) numeric_plan$mapping %||% numeric() else
      if (restore_categories) saved_mapping else numeric()
    current_levels <- .rls_metadata_levels(metadata, variable, character())
    metadata$numeric_mapping[row] <- list(next_mapping)
    metadata$category_order[row] <- list(if (type %in% c("factor", "ordered")) current_levels else previous_levels)
    history <- metadata$type_change_provenance[[row]] %||% list()
    history[[length(history) + 1L]] <- list(
      from = previous_type, to = type, categories = current_levels,
      mapping = if (is.null(numeric_plan)) NULL else numeric_plan$mapping,
      method = if (is.null(numeric_plan)) NULL else numeric_plan$method,
      breaks = if (is.null(breaks)) NULL else as.double(breaks),
      labels = if (is.null(labels)) NULL else as.character(labels),
      missing_values = "preserved unless a documented Text-to-Numeric parse fails",
      timestamp = format(Sys.time(), tz = "UTC", usetz = TRUE)
    )
    metadata$type_change_provenance[row] <- list(history)
    metadata
  }
  record$metadata <- update_conversion_metadata(record$metadata)
  record$variable_metadata <- update_conversion_metadata(record$variable_metadata)
  record <- .rls_rebuild_mids_after_type_change(record)
  record$modified <- TRUE
  mapping_code <- if (!is.null(numeric_plan) && length(numeric_plan$mapping)) {
    entries <- paste(sprintf("%s = %s",
                             encodeString(names(numeric_plan$mapping), quote = '"'),
                             format(unname(numeric_plan$mapping), scientific = FALSE,
                                    trim = TRUE)), collapse = ", ")
    sprintf("numeric_mapping <- c(%s)\ndata[[%s]] <- unname(numeric_mapping[as.character(data[[%s]])])",
            entries, encodeString(variable, quote = '"'), encodeString(variable, quote = '"'))
  } else if (identical(type, "ordered") && !is.null(breaks)) {
    break_literals <- vapply(as.double(breaks), function(value) {
      if (is.infinite(value)) {
        if (value < 0) "-Inf" else "Inf"
      } else format(value, scientific = FALSE, trim = TRUE, digits = 17)
    }, character(1L))
    label_code <- if (is.null(labels)) {
      "NULL"
    } else {
      sprintf("c(%s)", paste(encodeString(as.character(labels), quote = '"'), collapse = ", "))
    }
    sprintf(
      "data[[%s]] <- cut(data[[%s]], breaks = c(%s), labels = %s, ordered_result = TRUE, include.lowest = TRUE)",
      encodeString(variable, quote = '"'), encodeString(variable, quote = '"'),
      paste(break_literals, collapse = ", "), label_code
    )
  } else if (identical(type, "ordered")) {
    ordered_levels <- .rls_metadata_levels(record$variable_metadata, variable, all_levels)
    sprintf("data[[%s]] <- ordered(data[[%s]], levels = c(%s))",
            encodeString(variable, quote = '"'), encodeString(variable, quote = '"'),
            paste(encodeString(ordered_levels, quote = '"'), collapse = ", "))
  } else {
    sprintf("data[[%s]] <- as.%s(data[[%s]])",
            encodeString(variable, quote = '"'),
            if (identical(type, "numeric")) "numeric" else if (identical(type, "character")) "character" else "factor",
            encodeString(variable, quote = '"'))
  }
  if (identical(record$dataset_type %||% "data_frame", "multiple_imputation")) {
    mapping_code <- paste(
      sprintf("# Previous statistical type: %s; new statistical type: %s.",
              previous_type, type),
      "# Apply the same conversion to .imp = 0 and every completed imputation; preserve .id.",
      mapping_code,
      "# Rebuild the mids bookkeeping with mice::as.mids() and reattach LinkEDA variable metadata.",
      sep = "\n"
    )
  }
  record <- .rls_advance_data_version(
    record,
    sprintf("Change column `%s` analysis type to %s", variable, type),
    code = mapping_code,
    columns = variable
  )
  .rls_set_dataset_record(record)
  .rls_update_analysis_records_for_variable_type(record, variable)
  .rls_notify_dataset_changed(record, variable)
  if (previous_type == "numeric" && type %in% c("factor", "ordered") &&
      is.null(breaks) && length(all_levels) > 20L) {
    warning(sprintf(
      "Variable `%s` now has %d categories. Consider cut points or an explicit category mapping for a lower-cardinality variable.",
      variable, length(all_levels)
    ), call. = FALSE)
  }
  if (!is.null(numeric_plan$warning %||% NULL)) warning(numeric_plan$warning, call. = FALSE)
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
  .rls_mi_warn_current_version(record, "Scatterplot")
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
    as.data.frame(readxl::read_excel(path, sheet = sheet, .name_repair = "minimal"), stringsAsFactors = FALSE),
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
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                    na.strings = c("NA", ""), ...),
    error = function(e) stop(sprintf("Cannot import CSV file: %s", conditionMessage(e)), call. = FALSE)
  )
  .rls_register_imported_dataset(data, path, name, "CSV file", make_active = make_active)
}

#' @rdname ls_import_data
#' @export
ls_import_tsv <- function(path, name = NULL, make_active = TRUE, ...) {
  path <- .rls_check_import_path(path)
  data <- tryCatch(
    utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE,
                      na.strings = c("NA", ""), ...),
    error = function(e) stop(sprintf("Cannot import delimited text file: %s", conditionMessage(e)), call. = FALSE)
  )
  .rls_register_imported_dataset(data, path, name, "Delimited text file", make_active = make_active)
}

.rls_classify_r_import_object <- function(object) {
  if (inherits(object, "mids")) return("mids")
  if (inherits(object, c("mira", "mipo"))) {
    return("mice_result_without_imputations")
  }
  if (inherits(object, "mild") ||
      (is.list(object) && !is.data.frame(object) &&
       (!length(object) || all(vapply(object, is.data.frame, logical(1L)))))) {
    return("completed_dataset_list")
  }
  if (.rls_is_mice_long_data(object)) return("mice_long_data")
  if (is.data.frame(object)) return("data_frame")
  "unsupported"
}

.rls_is_mice_long_data <- function(object) {
  if (!is.data.frame(object) || !".imp" %in% names(object)) return(FALSE)
  imp <- suppressWarnings(as.integer(as.character(object$.imp)))
  any(is.finite(imp) & imp > 0) && (".id" %in% names(object) || length(unique(imp[is.finite(imp)])) > 1L)
}

.rls_mi_import_requirement <- function() paste(
  "Multiple-imputation data require a `mids` object.",
  "This file contains completed imputed datasets, but it does not preserve all information about the original missing data and imputation process required by LinkEDA for complete multiple-imputation diagnostics and reproducible MI analyses.",
  "Please import the original `mice::mids` object instead.",
  'In R, save the object returned by mice() directly: saveRDS(imp, "my_imputation.rds")',
  sep = "\n\n")

.rls_mi_stacked_import_notice <- function(object) {
  if (!.rls_is_mice_long_data(object)) return("")
  paste("Stacked multiple imputations detected.",
    "LinkEDA requires the original mice::mids object for its multiple-imputation workflow.",
    "This dataset is opened as ordinary data; multiple-imputation pooling and imputation diagnostics are disabled.")
}

.rls_register_r_import_object <- function(object, path, name = NULL,
                                          make_active = TRUE,
                                          source = "RDS file") {
  kind <- .rls_classify_r_import_object(object)
  if (identical(kind, "mids")) {
    return(.rls_register_mids_dataset(
      object, name = name %||% tools::file_path_sans_ext(basename(path)),
      source = if (identical(source, "RDS file")) "mice mids RDS file" else source,
      path = path, make_active = make_active
    ))
  }
  if (identical(kind, "mice_long_data")) kind <- "data_frame"
  if (identical(kind, "mice_result_without_imputations")) {
    stop(
      "The RDS file contains a fitted or pooled mice result, but not the completed imputations. Import or save the original `mids` object instead.",
      call. = FALSE
    )
  }
  if (identical(kind, "completed_dataset_list")) {
    stop(.rls_mi_import_requirement(), call. = FALSE)
  }
  if (!identical(kind, "data_frame")) {
    stop(
      "The selected file was read successfully, but it did not contain a data frame or a mice `mids` object.",
      call. = FALSE
    )
  }
  .rls_register_imported_dataset(
    object, path, name, source, make_active = make_active
  )
}

#' @rdname ls_import_data
#' @export
ls_import_rds <- function(path, name = NULL, make_active = TRUE) {
  path <- .rls_check_import_path(path)
  data <- tryCatch(
    readRDS(path),
    error = function(e) stop(sprintf("Cannot import RDS file: %s", conditionMessage(e)), call. = FALSE)
  )
  .rls_register_r_import_object(
    data, path, name, make_active = make_active, source = "RDS file"
  )
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
  mids_names <- loaded[vapply(loaded, function(object_name) {
    inherits(get(object_name, envir = env), "mids")
  }, logical(1L))]
  if (length(mids_names)) {
    sizes <- vapply(mids_names, function(object_name) {
      object <- get(object_name, envir = env)
      as.numeric(object$m %||% 1L) * nrow(object$data) * max(1L, ncol(object$data))
    }, numeric(1L))
    selected <- mids_names[[which.max(sizes)]]
    return(.rls_register_mids_dataset(
      get(selected, envir = env), name = name %||% selected,
      source = sprintf("mice mids R data file: %s", selected),
      path = path, make_active = make_active
    ))
  }
  data_names <- loaded[vapply(loaded, function(object_name) is.data.frame(get(object_name, envir = env)), logical(1L))]
  if (!length(data_names)) {
    if (any(vapply(loaded, function(key) identical(.rls_classify_r_import_object(get(key, envir = env)), "completed_dataset_list"), logical(1L))))
      stop(.rls_mi_import_requirement(), call. = FALSE)
    stop("The selected R data file did not contain a data frame or a mice `mids` object.", call. = FALSE)
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
