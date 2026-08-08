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
  distance <- match.arg(distance, c("euclidean", "correlation"))
  if (identical(distance, "correlation")) "euclidean" else distance
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
    return(numeric_vars)
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

.rls_dendrogram_compute <- function(data, variables, distance, linkage, missing_mode) {
  distance <- .rls_dendrogram_validate_distance(distance)
  linkage <- .rls_dendrogram_validate_linkage(linkage)
  missing_mode <- match.arg(missing_mode, c("pairwise", "listwise"))

  matrix_data <- as.matrix(data[, variables, drop = FALSE])
  storage.mode(matrix_data) <- "double"
  row_ids <- seq_len(nrow(matrix_data))

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

  z <- matrix(NA_real_, nrow = nrow(matrix_data), ncol = ncol(matrix_data),
              dimnames = list(as.character(row_ids), variables))
  for (j in seq_len(ncol(matrix_data))) {
    col <- matrix_data[, j]
    finite <- is.finite(col)
    if (!any(finite)) next
    mu <- mean(col[finite])
    sigma <- stats::sd(col[finite])
    if (is.finite(sigma) && sigma > 1e-12) {
      z[finite, j] <- (col[finite] - mu) / sigma
    } else {
      z[finite, j] <- 0
    }
  }

  n <- nrow(z)
  distance_matrix <- matrix(0, nrow = n, ncol = n,
                            dimnames = list(as.character(row_ids), as.character(row_ids)))
  if (n > 1L) {
    for (i in seq_len(n - 1L)) {
      for (j in (i + 1L):n) {
        both <- is.finite(z[i, ]) & is.finite(z[j, ])
        d <- if (any(both)) {
          sqrt(mean((z[i, both] - z[j, both])^2))
        } else {
          1
        }
        distance_matrix[i, j] <- d
        distance_matrix[j, i] <- d
      }
    }
  }

  hc <- if (n >= 2L) stats::hclust(stats::as.dist(distance_matrix), method = linkage) else NULL
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
  fit <- .rls_dendrogram_compute(record$data, record$variables, record$distance, record$linkage, record$missing_mode)
  record$hclust <- fit$hclust
  record$merge <- fit$merge
  record$height <- fit$height
  record$order <- fit$order
  record$labels <- fit$labels
  record$distance_matrix <- fit$distance_matrix
  record$case_rows <- fit$case_rows
  record$model_version <- record$model_version + 1L
  record
}

.rls_dendrogram_sync_native_open <- function(record) {
  .rls_start_backend()
  dataset <- .rls_dataset_record(record$group)
  try(.rls_send(c(
    "REGISTER_DATASET",
    record$group,
    .rls_variable_payload(record$data, dataset$variable_metadata),
    .rls_dataframe_payload(record$data, dataset$variable_metadata)
  )), silent = TRUE)
  try(.rls_send(c(
    "DENDRO_OPEN", record$id, record$group, record$distance, record$linkage, record$missing_mode,
    as.character(length(record$variables)), record$variables
  )), silent = TRUE)
}

#' Quick cluster cases using selected numeric variables
#'
#' @param data Registered dataset name, a data frame, or `NULL` for active dataset.
#' @param variables Numeric variables to include. If `NULL`, all numeric
#'   variables are used.
#' @param distance Distance metric. Currently only `"euclidean"` is supported
#'   (`"correlation"` is accepted as a legacy alias).
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

  variables <- .rls_dendrogram_validate_variables(dataset$data, variables, dataset$variable_metadata)
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
  variable <- .rls_dendrogram_validate_variables(record$data, variable, dataset$variable_metadata, require_at_least = 1L)
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
  if (!length(record$variables)) {
    stop("Quick cluster requires at least one numeric variable.", call. = FALSE)
  }
  record <- .rls_dendrogram_refit(record)
  handle <- .rls_assign_dendrogram(record)
  if (isTRUE(.rls_state$process_started)) {
    try(.rls_send(c("DENDRO_SET_VARIABLES", record$id, as.character(length(record$variables)), record$variables)), silent = TRUE)
  }
  invisible(handle)
}
