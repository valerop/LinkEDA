.rls_dendrogram_id <- function(group, name = NULL) {
  .rls_validate_protocol_name(name %||% paste0("dendro_", group, "_", format(Sys.time(), "%Y%m%d%H%M%OS3")), "name")
}

.rls_dendrogram_record <- function(dendrogram) {
  id <- if (inherits(dendrogram, "rlispstat_dendrogram")) dendrogram$id else dendrogram
  id <- .rls_validate_protocol_name(id, "dendrogram")
  if (!exists(id, envir = .rls_state$dendrograms, inherits = FALSE)) {
    stop("Unknown dendrogram object.", call. = FALSE)
  }
  get(id, envir = .rls_state$dendrograms)
}

.rls_assign_dendrogram <- function(record) {
  assign(record$id, record, envir = .rls_state$dendrograms)
  structure(list(id = record$id, group = record$group), class = "rlispstat_dendrogram")
}

.rls_dendrogram_validate_distance <- function(distance) {
  if (identical(distance, "correlation")) {
    stop("Correlation distance is not supported by Quick Cluster. Choose euclidean explicitly.", call. = FALSE)
  }
  match.arg(distance, c("euclidean", "manhattan", "maximum", "canberra"))
}

.rls_dendrogram_validate_linkage <- function(linkage) {
  match.arg(linkage, c("average", "complete", "single"))
}

.rls_dendrogram_validate_variables <- function(data, variables, metadata = NULL, require_at_least = 1L) {
  numeric_vars <- .rls_numeric_variable_names(data, metadata)
  if (length(numeric_vars) < require_at_least) {
    stop(sprintf("Quick cluster requires at least %d numeric variable%s.",
                 require_at_least,
                 if (require_at_least == 1L) "" else "s"), call. = FALSE)
  }
  if (is.null(variables)) {
    return(character())
  }
  variables <- unique(vapply(variables, .rls_validate_protocol_name, character(1L), what = "variable"))
  missing <- setdiff(variables, names(data))
  if (length(missing)) {
    stop(sprintf("Column `%s` was not found in the dataset.", missing[[1L]]), call. = FALSE)
  }
  non_numeric <- variables[!variables %in% numeric_vars]
  if (length(non_numeric)) {
    stop(sprintf("Column `%s` must be numeric for quick cluster.", non_numeric[[1L]]), call. = FALSE)
  }
  if (length(variables) < require_at_least) {
    stop(sprintf("Quick cluster requires at least %d numeric variable%s.",
                 require_at_least,
                 if (require_at_least == 1L) "" else "s"), call. = FALSE)
  }
  variables
}

.rls_dendrogram_compute <- function(data, variables, distance, linkage, missing_mode,
                                    retain_matrix = FALSE) {
  distance <- .rls_dendrogram_validate_distance(distance)
  linkage <- .rls_dendrogram_validate_linkage(linkage)
  missing_mode <- match.arg(missing_mode, c("pairwise", "listwise"))

  matrix_data <- as.matrix(data[, variables, drop = FALSE])
  storage.mode(matrix_data) <- "double"
  row_ids <- seq_len(nrow(matrix_data))
  if (any(is.infinite(matrix_data)))
    stop("Quick Cluster requires finite values or NA; remove infinite values first.", call. = FALSE)


  if (identical(missing_mode, "listwise")) {
    keep <- stats::complete.cases(matrix_data)
  } else {
    keep <- rowSums(is.finite(matrix_data)) > 0L
  }
  matrix_data <- matrix_data[keep, , drop = FALSE]
  row_ids <- row_ids[keep]

  if (!nrow(matrix_data)) {
    return(list(
      hclust = NULL,
      merge = matrix(integer(), nrow = 0L, ncol = 2L),
      height = numeric(),
      order = integer(),
      labels = character(),
      distance_matrix = matrix(numeric(), nrow = 0L, ncol = 0L),
      case_rows = integer()
    ))
  }

  if (nrow(matrix_data) < 2L)
    stop("Quick Cluster requires at least two usable cases in the analysis scope.", call. = FALSE)

  # Use the public R implementation for both standardisation and distances.
  # Do not replace undefined distances or unstandardisable columns with numbers.
  z <- base::scale(matrix_data)
  rownames(z) <- as.character(row_ids)
  spread <- attr(z, "scaled:scale")
  invalid <- !is.finite(spread) | spread <= 0
  if (any(invalid)) stop(sprintf(
    "Cannot standardize variables with no variation or insufficient observations: %s.",
    paste(variables[invalid], collapse = ", ")), call. = FALSE)
  distances <- stats::dist(z, method = distance)
  if (any(!is.finite(distances))) stop(
    "Some cases have no jointly observed variables; their distances are undefined. Choose listwise missing-data handling or change the variables/scope.",
    call. = FALSE)
  hc <- stats::hclust(distances, method = linkage)
  # The tree is sufficient for rendering and linked cases. Do not retain a
  # second, dense N-by-N copy of the temporary stats::dist object.
  distance_matrix <- if (isTRUE(retain_matrix)) as.matrix(distances) else
    matrix(numeric(), nrow = 0L, ncol = 0L)
  n <- nrow(z)

  list(
    hclust = hc,
    merge = if (is.null(hc)) matrix(integer(), nrow = 0L, ncol = 2L) else hc$merge,
    height = if (is.null(hc)) numeric() else hc$height,
    order = if (is.null(hc)) 1L else hc$order,
    labels = if (is.null(hc)) as.character(row_ids) else hc$labels,
    distance_matrix = distance_matrix,
    case_rows = as.integer(row_ids)
  )
}

.rls_dendrogram_refit <- function(record) {
  dataset <- .rls_dataset_record(record$group)
  if (!isTRUE(record$native_scope_snapshot))
    record <- .rls_apply_scope_to_model_request(record, dataset)
  if (.rls_mi_is_dataset(dataset)) {
    completed <- .rls_mi_completed_datasets(dataset)
    active <- if (isTRUE(record$native_scope_snapshot)) record$active_imputation_version else
      dataset$active_imputation_version %||% 1L
    if (!is.finite(active) || active < 1L || active > length(completed))
      stop("Invalid imputation number for Quick Cluster.", call. = FALSE)
    dataset$data <- completed[[active]]
    record$active_imputation_version <- active
  }
  scoped <- .rls_dataset_subset_original_rows(dataset,record$data_scope$rows)
  record$data <- scoped$data
  record$original_row_ids <- scoped$original_row_ids
  if (!length(record$variables)) {
    record$hclust <- NULL
    record$merge <- matrix(integer(), nrow = 0L, ncol = 2L)
    record$height <- numeric()
    record$order <- integer()
    record$labels <- character()
    record$distance_matrix <- matrix(numeric(), nrow = 0L, ncol = 0L)
    record$case_rows <- integer()
    record$model_version <- record$model_version + 1L
    return(record)
  }
  fit <- .rls_dendrogram_compute(record$data, record$variables, record$distance, record$linkage, record$missing_mode)
  record$hclust <- fit$hclust
  record$merge <- fit$merge
  record$height <- fit$height
  record$order <- fit$order
  record$labels <- fit$labels
  record$distance_matrix <- fit$distance_matrix
  record$case_rows <- record$original_row_ids[fit$case_rows]
  record$labels <- as.character(record$case_rows)
  if (!is.null(record$hclust)) record$hclust$labels <- record$labels
  record$rows_used_original_ids <- record$case_rows
  record$rows_excluded_original_ids <- setdiff(record$data_scope$rows, record$case_rows)
  recipe <- .rls_dendrogram_verification_r_code(record, .rls_mi_is_dataset(dataset))
  record <- .rls_attach_analysis_provenance(record,
    paste0("z <- base::scale(analysis_data)\ndistances <- stats::dist(z, method = ",
           .rls_r_string_literal(record$distance),
           ")\ncluster <- stats::hclust(distances, method = linkage)"),
    title = "Quick Cluster", verification_code = list(plot = recipe),
    verification_variables = record$variables,
    verification_warnings = if (.rls_mi_is_dataset(dataset))
      "Quick Cluster uses one completed imputation; cluster trees are not Rubin-pooled." else character())
  record$model_version <- record$model_version + 1L
  record
}

.rls_dendrogram_sync_native_open <- function(record) {
  .rls_start_backend()
  dataset <- .rls_dataset_record(record$group)
  try(.rls_register_native_dataset_if_needed(dataset, visible = TRUE), silent = TRUE)
  try(.rls_send(c(
    "DENDRO_OPEN", record$id, record$group, record$distance, record$linkage, record$missing_mode,
    as.character(length(record$variables)), record$variables
  )), silent = TRUE)
}

#' Quick cluster cases using selected numeric variables
#'
#' @param data Registered dataset name, a data frame, or `NULL` for active dataset.
#' @param variables Numeric variables to include. If `NULL`, the analysis opens
#'   with an empty variable list and waits for an explicit selection.
#' @param distance Distance metric: `"euclidean"`, `"manhattan"`,
#'   `"maximum"`, or `"canberra"`. `"correlation"` is rejected.
#' @param linkage Hierarchical linkage method (`"average"`, `"complete"`,
#'   or `"single"`).
#' @param missing Missing-data mode (`"pairwise"` or `"listwise"`).
#' @param name Optional dendrogram id or dataset name for data-frame input.
#' @param native Logical. Open the native dendrogram window when possible.
#' @return A `rlispstat_dendrogram` handle.
#' @export
ls_new_quick_cluster <- function(data = NULL, variables = NULL,
                                 distance = "euclidean",
                                 linkage = c("average", "complete", "single"),
                                 missing = c("pairwise", "listwise"),
                                 name = NULL,
                                 native = TRUE) {
  distance <- .rls_dendrogram_validate_distance(distance)
  linkage <- .rls_dendrogram_validate_linkage(match.arg(linkage))
  missing <- match.arg(missing)

  if (is.data.frame(data)) {
    group <- .rls_register_dataset(name %||% "quick_cluster", data, activate = TRUE)
    dataset <- .rls_dataset_record(group)
    id_name <- NULL
  } else {
    dataset <- .rls_dataset_record(data)
    id_name <- name
  }
  .rls_mi_warn_current_version(dataset, "Quick Cluster")

  variables <- .rls_dendrogram_validate_variables(
    dataset$data, variables, dataset$variable_metadata, require_at_least = 0L)
  record <- list(
    id = .rls_dendrogram_id(dataset$group, id_name),
    group = dataset$group,
    dataset_id = dataset$group,
    data = dataset$data,
    variables = variables,
    distance = distance,
    linkage = linkage,
    missing_mode = missing,
    missing = missing,
    hclust = NULL,
    merge = matrix(integer(), nrow = 0L, ncol = 2L),
    height = numeric(),
    order = integer(),
    labels = character(),
    distance_matrix = matrix(numeric(), nrow = 0L, ncol = 0L),
    case_rows = integer(),
    model_version = 0L
  )
  record <- .rls_dendrogram_refit(record)
  handle <- .rls_assign_dendrogram(record)
  if (isTRUE(native)) {
    .rls_dendrogram_sync_native_open(record)
  }
  invisible(handle)
}

#' @rdname ls_new_quick_cluster
#' @export
ls_dendrogram_state <- function(dendrogram) {
  .rls_dendrogram_record(dendrogram)
}

#' @rdname ls_new_quick_cluster
#' @export
ls_dendrogram_hclust <- function(dendrogram) {
  .rls_dendrogram_record(dendrogram)$hclust
}

#' @rdname ls_new_quick_cluster
#' @export
ls_dendrogram_add_variable <- function(dendrogram, variable) {
  record <- .rls_dendrogram_record(dendrogram)
  dataset <- .rls_dataset_record(record$group)
  source_data <- if (.rls_mi_is_dataset(dataset)) {
    completed <- .rls_mi_completed_datasets(dataset)
    active <- if (isTRUE(record$native_scope_snapshot)) record$active_imputation_version else
      dataset$active_imputation_version %||% 1L
    if (length(active) != 1L || !is.finite(active) || active < 1L || active > length(completed))
      stop("Invalid imputation number for Quick Cluster.", call. = FALSE)
    completed[[active]]
  } else dataset$data
  variable <- .rls_dendrogram_validate_variables(source_data, variable, dataset$variable_metadata, require_at_least = 1L)
  record$variables <- unique(c(record$variables, variable))
  record <- .rls_dendrogram_refit(record)
  handle <- .rls_assign_dendrogram(record)
  if (isTRUE(.rls_state$process_started)) {
    try(.rls_send(c("DENDRO_SET_VARIABLES", record$id, as.character(length(record$variables)), record$variables)), silent = TRUE)
  }
  invisible(handle)
}

#' @rdname ls_new_quick_cluster
#' @export
ls_dendrogram_remove_variable <- function(dendrogram, variable) {
  record <- .rls_dendrogram_record(dendrogram)
  variable <- .rls_validate_protocol_name(variable, "variable")
  record$variables <- setdiff(record$variables, variable)
  record <- .rls_dendrogram_refit(record)
  handle <- .rls_assign_dendrogram(record)
  if (isTRUE(.rls_state$process_started)) {
    try(.rls_send(c("DENDRO_SET_VARIABLES", record$id, as.character(length(record$variables)), record$variables)), silent = TRUE)
  }
  invisible(handle)
}

# Native requests carry an immutable scope, source version and imputation.
# A terminal error is returned too, so the window never keeps an obsolete tree.
.rls_handle_dendrogram_needed <- function(parts) {
  if (length(parts) < 12L) return(invisible(NULL))
  id <- parts[2L]
  revision <- parts[4L]
  source_version <- parts[5L]
  result <- c("DENDRO_UPDATE", id, revision, source_version)
  record <- NULL
  tryCatch({
    group <- parts[3L]
    dataset <- .rls_dataset_record(group)
    if (!identical(as.character(dataset$data_version %||% 1L), source_version))
      stop("The data changed before clustering could run. Recalculate the dendrogram.", call. = FALSE)
    count <- as.integer(parts[11L])
    if (is.na(count) || count < 1L || length(parts) < 12L + count)
      stop("Invalid clustering variable request.", call. = FALSE)
    variables <- parts[seq.int(12L, 11L + count)]
    cursor <- 12L + count
    row_count <- as.integer(parts[cursor])
    if (is.na(row_count) || row_count < 0L || length(parts) != cursor + row_count)
      stop("Invalid clustering scope request.", call. = FALSE)
    rows <- if (row_count) as.integer(parts[seq.int(cursor + 1L, cursor + row_count)]) else integer()
    if (anyNA(rows) || any(rows < 1L)) stop("Invalid clustering row identity.", call. = FALSE)
    scope <- .rls_capture_analysis_scope(dataset, if (parts[7L] == "all") "all" else "selected", rows)
    # Do not consult mutable native scope while processing a queued snapshot.
    record <- list(id = id, group = group, dataset_id = group,
      variables = .rls_dendrogram_validate_variables(dataset$data, variables, dataset$variable_metadata),
      distance = parts[8L], linkage = parts[9L], missing_mode = parts[10L],
      active_imputation_version = as.integer(parts[6L]),
      data_scope = scope, native_scope_snapshot = TRUE, model_version = 0L,
      request_revision = revision, source_data_version = source_version)
    record <- .rls_dendrogram_refit(record)
    .rls_assign_dendrogram(record)
    status <- if (length(record$case_rows))
      "Calculated in R: base::scale, stats::dist and stats::hclust." else
      "No usable cases in the requested analysis scope."
    if (.rls_mi_is_dataset(dataset)) status <- paste(status,
      sprintf("Imputation %d of %d (not pooled).", record$active_imputation_version, dataset$imputation_count))
    result <- c(result, "ok", status,
      length(record$case_rows), record$case_rows,
      length(record$order), record$order - 1L, nrow(record$merge))
    n <- length(record$case_rows)
    for (i in seq_len(nrow(record$merge))) {
      children <- record$merge[i, ]
      children <- ifelse(children < 0L, -children - 1L, n + children - 1L)
      result <- c(result, children, format(record$height[i], digits = 17, scientific = TRUE))
    }
    result <- c(result, .rls_analysis_provenance_payload(record))
  }, error = function(e) {
    result <<- c("DENDRO_UPDATE", id, revision, source_version, "error",
      .rls_native_wire_value(conditionMessage(e)), "0", "0", "0")
  })
  .rls_send(as.character(result))
  invisible(record)
}

.rls_handle_dendrogram_distance_matrix_needed <- function(parts) {
  if (length(parts) < 5L) return(invisible(FALSE))
  id <- parts[[2L]]
  group <- parts[[3L]]
  data_version <- parts[[4L]]
  revision <- parts[[5L]]
  tryCatch({
    dataset <- .rls_dataset_record(group)
    record <- .rls_dendrogram_record(id)
    if (!identical(as.character(dataset$data_version %||% 1L), data_version) ||
        !identical(as.character(record$request_revision), revision) ||
        !identical(record$group, group))
      stop("The dendrogram has changed. Reopen its distance matrix after the calculation finishes.", call. = FALSE)
    if (length(record$case_rows) < 2L)
      stop("Calculate a dendrogram with at least two cases first.", call. = FALSE)
    fit <- .rls_dendrogram_compute(record$data, record$variables,
                                   record$distance, record$linkage,
                                   record$missing_mode, retain_matrix = TRUE)
    if (!identical(as.integer(record$case_rows),
                   as.integer(record$original_row_ids[fit$case_rows])))
      stop("The dendrogram cases changed. Recalculate the tree before opening its distance matrix.", call. = FALSE)
    labels <- paste0("case_", record$case_rows)
    matrix_data <- fit$distance_matrix
    colnames(matrix_data) <- labels
    rownames(matrix_data) <- labels
    result <- data.frame(case = labels, matrix_data, check.names = FALSE)
    name <- .rls_register_dataset(
      paste0("distance_", id), result, source = "Quick Cluster distance matrix",
      activate = FALSE, provenance_origin = "reconstructed",
      provenance_code = paste0(
        "# Distances among the scoped cases used by Quick Cluster.\n",
        "# The clustering verification recipe supplies analysis_data.\n",
        "z <- base::scale(analysis_data[, ", .rls_r_character_vector(record$variables),
        ", drop = FALSE])\n",
        "as.matrix(stats::dist(z, method = ",
        .rls_r_string_literal(record$distance), "))"
      ))
    output <- .rls_dataset_record(name)
    .rls_register_native_dataset_if_needed(output, visible = FALSE)
    .rls_send(c("DATA_OPEN_DATA_SHEET", name))
    invisible(TRUE)
  }, error = function(error) {
    .rls_send(c("WORKBENCH_MESSAGE", "Distance matrix",
                .rls_native_wire_value(conditionMessage(error))))
    invisible(FALSE)
  })
}
