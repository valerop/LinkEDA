.rls_dimension_id <- function(group, name = NULL) {
  .rls_validate_protocol_name(name %||% paste0("dim_", group, "_", format(Sys.time(), "%Y%m%d%H%M%OS3")), "name")
}

.rls_dimension_record <- function(model) {
  id <- if (inherits(model, "rlispstat_dimensionality")) model$id else model
  id <- .rls_validate_protocol_name(id, "model")
  if (!exists(id, envir = .rls_state$dimensionality_models, inherits = FALSE)) {
    stop("Unknown principal components/factor analysis model.", call. = FALSE)
  }
  get(id, envir = .rls_state$dimensionality_models)
}

.rls_assign_dimension <- function(record) {
  assign(record$id, record, envir = .rls_state$dimensionality_models)
  structure(list(id = record$id, group = record$group), class = "rlispstat_dimensionality")
}

.rls_dimension_validate_variables <- function(data, variables, metadata = NULL, min_variables = 2L) {
  numeric <- .rls_numeric_variable_names(data, metadata)
  if (is.null(variables)) {
    variables <- utils::head(numeric, max(min_variables, min(6L, length(numeric))))
  }
  variables <- unique(vapply(variables, .rls_validate_protocol_name, character(1L), what = "variable"))
  missing <- setdiff(variables, names(data))
  if (length(missing)) {
    stop(sprintf("Column `%s` was not found in the dataset.", missing[[1L]]), call. = FALSE)
  }
  non_numeric <- variables[!variables %in% numeric]
  if (length(non_numeric)) {
    stop(sprintf("Column `%s` must be numeric for principal components/factor analysis.", non_numeric[[1L]]), call. = FALSE)
  }
  if (length(variables) < min_variables) {
    stop(sprintf("At least %d numeric variables are required.", min_variables), call. = FALSE)
  }
  variables
}

.rls_dimension_complete_matrix <- function(data, variables, missing = c("listwise", "pairwise"),
                                           scope = c("all", "selected", "unselected"),
                                           selected_rows = integer()) {
  missing <- match.arg(missing)
  scope <- match.arg(scope)
  x <- data[, variables, drop = FALSE]
  rows <- stats::complete.cases(x)
  if (!identical(scope, "all")) {
    selected_rows <- unique(as.integer(selected_rows %||% integer()))
    selected_rows <- selected_rows[!is.na(selected_rows) & selected_rows >= 1L & selected_rows <= nrow(data)]
    selected <- rep(FALSE, nrow(data))
    selected[selected_rows] <- TRUE
    if (identical(scope, "selected")) {
      rows <- rows & selected
    } else {
      rows <- rows & !selected
    }
  }
  list(
    x = as.matrix(x[rows, , drop = FALSE]),
    rows_used = which(rows),
    rows_excluded = which(!rows)
  )
}

.rls_dimension_parallel <- function(n, p, iterations = 100L, seed = 271828L) {
  iterations <- max(1L, as.integer(iterations %||% 100L))
  if (n < 2L || p < 1L) {
    return(rep(NA_real_, p))
  }
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    NULL
  }
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    } else {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  values <- matrix(NA_real_, nrow = iterations, ncol = p)
  for (i in seq_len(iterations)) {
    simulated <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)
    simulated <- scale(simulated, center = TRUE, scale = TRUE)
    ev <- eigen(stats::cor(simulated), symmetric = TRUE, only.values = TRUE)$values
    values[i, seq_along(ev)] <- ev
  }
  as.numeric(apply(values, 2L, stats::quantile, probs = 0.95, na.rm = TRUE, names = FALSE))
}

.rls_dimension_orthomax <- function(loadings, scores, rotation = c("none", "varimax", "quartimax"),
                                    max_iter = 60L, tolerance = 1e-6) {
  rotation <- match.arg(rotation)
  loadings <- as.matrix(loadings)
  scores <- as.matrix(scores)
  if (identical(rotation, "none") || ncol(loadings) < 2L) {
    return(list(loadings = loadings, scores = scores))
  }
  gamma <- switch(rotation, varimax = 1, quartimax = 0)
  p <- nrow(loadings)
  q <- ncol(loadings)
  for (iter in seq_len(max_iter)) {
    max_change <- 0
    for (i in seq_len(q - 1L)) {
      for (j in seq.int(i + 1L, q)) {
        x <- loadings[, i]
        y <- loadings[, j]
        u <- x^2 - y^2
        v <- 2 * x * y
        a <- sum(u)
        b <- sum(v)
        c_term <- sum(u^2 - v^2) - gamma / p * (a^2 - b^2)
        d_term <- 2 * sum(u * v) - 2 * gamma / p * a * b
        phi <- 0.25 * atan2(d_term, c_term)
        if (!is.finite(phi) || abs(phi) < tolerance) {
          next
        }
        co <- cos(phi)
        si <- sin(phi)
        loadings[, i] <- co * x + si * y
        loadings[, j] <- -si * x + co * y
        sx <- scores[, i]
        sy <- scores[, j]
        scores[, i] <- co * sx + si * sy
        scores[, j] <- -si * sx + co * sy
        max_change <- max(max_change, abs(phi))
      }
    }
    if (max_change < tolerance) {
      break
    }
  }
  list(loadings = loadings, scores = scores)
}

.rls_dimension_pca <- function(x, variables, n_components = NULL, scale = TRUE,
                               rotation = c("none", "varimax", "quartimax")) {
  rotation <- match.arg(rotation)
  p <- length(variables)
  n_components <- min(p, max(1L, as.integer(n_components %||% min(2L, p))))
  fit <- stats::prcomp(x, center = TRUE, scale. = isTRUE(scale))
  eigenvalues <- fit$sdev^2
  total <- sum(eigenvalues)
  variance <- if (total > 0) eigenvalues / total else rep(NA_real_, length(eigenvalues))
  loadings <- sweep(fit$rotation[, seq_len(n_components), drop = FALSE], 2L,
                    fit$sdev[seq_len(n_components)], `*`)
  colnames(loadings) <- paste0("PC", seq_len(n_components))
  scores <- fit$x[, seq_len(n_components), drop = FALSE]
  colnames(scores) <- paste0("PC", seq_len(n_components))
  rotated <- .rls_dimension_orthomax(loadings, scores, rotation)
  loadings <- rotated$loadings
  scores <- rotated$scores
  colnames(loadings) <- colnames(scores) <- paste0("PC", seq_len(n_components))
  list(
    method = "pca",
    eigenvalues = eigenvalues,
    variance = variance,
    cumulative = cumsum(variance),
    loadings = loadings,
    scores = scores,
    communalities = rowSums(loadings^2),
    uniquenesses = pmax(0, 1 - rowSums(loadings^2))
  )
}

.rls_dimension_max_factor_count <- function(p) {
  p <- as.integer(p)
  if (is.na(p) || p < 3L) return(1L)
  max_factors <- 1L
  for (m in seq_len(p - 1L)) {
    df <- ((p - m)^2 - p - m) / 2
    if (is.finite(df) && df >= 0) {
      max_factors <- m
    }
  }
  max_factors
}

.rls_dimension_factor <- function(x, variables, n_components = NULL, scale = TRUE,
                                  rotation = c("none", "varimax", "quartimax")) {
  rotation <- match.arg(rotation)
  p <- length(variables)
  max_factors <- .rls_dimension_max_factor_count(p)
  n_components <- min(max_factors, max(1L, as.integer(n_components %||% min(2L, max_factors))))
  if (n_components < 1L) {
    stop("At least one factor is required.", call. = FALSE)
  }
  x_fit <- if (isTRUE(scale)) scale(x) else x
  fit <- try(stats::factanal(x_fit, factors = n_components, scores = "regression", rotation = "none"), silent = TRUE)
  if (inherits(fit, "try-error")) {
    stop(sprintf("Could not fit factor analysis: %s", conditionMessage(attr(fit, "condition"))), call. = FALSE)
  }
  loadings <- unclass(fit$loadings)[, seq_len(n_components), drop = FALSE]
  colnames(loadings) <- paste0("F", seq_len(n_components))
  scores <- fit$scores
  colnames(scores) <- paste0("F", seq_len(n_components))
  rotated <- .rls_dimension_orthomax(loadings, scores, rotation)
  loadings <- rotated$loadings
  scores <- rotated$scores
  colnames(loadings) <- colnames(scores) <- paste0("F", seq_len(n_components))
  ss <- colSums(loadings^2)
  variance <- ss / p
  list(
    method = "factor",
    eigenvalues = eigen(stats::cor(x_fit), symmetric = TRUE, only.values = TRUE)$values,
    variance = variance,
    cumulative = cumsum(variance),
    loadings = loadings,
    scores = scores,
    communalities = rowSums(loadings^2),
    uniquenesses = as.numeric(fit$uniquenesses)
  )
}

.rls_dimension_compute <- function(data, variables, method = c("pca", "factor"),
                                   n_components = NULL, scale = TRUE,
                                   missing = c("listwise", "pairwise"),
                                   rotation = c("none", "varimax", "quartimax"),
                                   scope = c("all", "selected", "unselected"),
                                   selected_rows = integer(),
                                   parallel = TRUE, parallel_iterations = 100L) {
  method <- match.arg(method)
  missing <- match.arg(missing)
  rotation <- match.arg(rotation)
  scope <- match.arg(scope)
  complete <- .rls_dimension_complete_matrix(data, variables, missing, scope, selected_rows)
  x <- complete$x
  if (nrow(x) < 2L) {
    stop("At least two complete rows are required.", call. = FALSE)
  }
  zero_var <- vapply(seq_len(ncol(x)), function(j) stats::sd(x[, j]) == 0, logical(1L))
  if (any(zero_var)) {
    stop(sprintf("Column `%s` has zero variance.", variables[which(zero_var)[[1L]]]), call. = FALSE)
  }
  result <- if (identical(method, "factor")) {
    .rls_dimension_factor(x, variables, n_components, scale, rotation)
  } else {
    .rls_dimension_pca(x, variables, n_components, scale, rotation)
  }
  components <- seq_along(result$eigenvalues)
  prefix <- if (identical(method, "factor")) "F" else "PC"
  parallel_values <- if (isTRUE(parallel)) {
    .rls_dimension_parallel(nrow(x), length(variables), parallel_iterations)
  } else {
    rep(NA_real_, length(result$eigenvalues))
  }
  eigen_table <- data.frame(
    component = paste0(prefix, components),
    eigenvalue = as.numeric(result$eigenvalues),
    parallel_eigenvalue = as.numeric(parallel_values[components]),
    variance = as.numeric(result$variance[components]),
    cumulative = as.numeric(result$cumulative[components]),
    stringsAsFactors = FALSE
  )
  loadings <- as.data.frame(result$loadings, stringsAsFactors = FALSE)
  loadings <- cbind(
    variable = variables,
    loadings,
    communality = as.numeric(result$communalities),
    uniqueness = as.numeric(result$uniquenesses),
    stringsAsFactors = FALSE
  )
  list(
    method = method,
    rotation = rotation,
    eigenvalues = eigen_table,
    loadings = loadings,
    scores = as.data.frame(result$scores),
    rows_used_original_ids = complete$rows_used,
    rows_excluded_original_ids = complete$rows_excluded
  )
}

.rls_dimension_refit <- function(record) {
  computed <- .rls_dimension_compute(
    record$data, record$variables, method = record$method,
    n_components = record$n_components, scale = record$scale,
    missing = record$missing_mode, rotation = record$rotation %||% "none",
    scope = record$scope %||% "all",
    selected_rows = record$selected_rows %||% integer(),
    parallel = record$parallel %||% TRUE,
    parallel_iterations = record$parallel_iterations %||% 100L
  )
  record$eigenvalues <- computed$eigenvalues
  record$loadings <- computed$loadings
  record$scores <- computed$scores
  record$rotation <- computed$rotation
  record$n_components <- max(1L, min(record$n_components, ncol(record$loadings) - 3L))
  record$rows_used_original_ids <- computed$rows_used_original_ids
  record$rows_excluded_original_ids <- computed$rows_excluded_original_ids
  record$model_version <- record$model_version + 1L
  record
}

.rls_dimension_sync_native_open <- function(record) {
  .rls_start_backend()
  dataset <- .rls_dataset_record(record$group)
  try(.rls_send(c(
    "REGISTER_DATASET",
    record$group,
    .rls_variable_payload(record$data, dataset$variable_metadata),
    .rls_dataframe_payload(record$data, dataset$variable_metadata, dataset_record = dataset)
  )), silent = TRUE)
  try(.rls_send(c(
    "PCAFA_OPEN", record$id, record$group, record$method, record$missing_mode,
    if (isTRUE(record$scale)) "TRUE" else "FALSE",
    as.character(record$n_components),
    record$rotation %||% "none",
    record$scope %||% "all",
    as.character(length(record$variables)), record$variables
  )), silent = TRUE)
}

.rls_dimension_native_update_payload <- function(record) {
  eigen <- record$eigenvalues %||% data.frame()
  loadings <- record$loadings %||% data.frame()
  scores <- record$scores %||% data.frame()
  component_count <- max(1L, as.integer(record$n_components %||% 1L))
  component_names <- if (is.data.frame(loadings)) {
    setdiff(names(loadings), c("variable", "communality", "uniqueness"))
  } else {
    character()
  }
  score_names <- if (is.data.frame(scores)) names(scores) else character()

  payload <- c(
    "PCAFA_UPDATE",
    .rls_native_wire_value(record$id),
    .rls_native_wire_value(record$group),
    .rls_native_wire_value(record$method),
    .rls_native_wire_value(record$missing_mode),
    .rls_native_wire_value(record$rotation %||% "none"),
    .rls_native_wire_value(record$scope %||% "all"),
    .rls_native_wire_bool(record$scale),
    .rls_native_wire_integer(component_count),
    .rls_native_wire_value(sprintf(
      "%s: %d variables, %d complete rows%s",
      if (identical(record$method, "factor")) "Factor analysis" else "Principal components",
      length(record$variables %||% character()),
      length(record$rows_used_original_ids %||% integer()),
      if (!identical(record$rotation %||% "none", "none")) paste0(", ", record$rotation, " rotation") else ""
    )),
    as.character(length(record$variables %||% character())),
    .rls_native_wire_value(record$variables %||% character()),
    .rls_native_int_vector_payload(record$rows_used_original_ids %||% integer()),
    .rls_native_int_vector_payload(record$rows_excluded_original_ids %||% integer())
  )

  component_rows <- if (is.data.frame(eigen)) nrow(eigen) else 0L
  payload <- c(payload, as.character(component_rows))
  if (component_rows) {
    for (i in seq_len(component_rows)) {
      payload <- c(
        payload,
        as.character(i),
        .rls_native_wire_number(eigen$eigenvalue[[i]]),
        .rls_native_wire_number(eigen$parallel_eigenvalue[[i]]),
        .rls_native_wire_number(eigen$variance[[i]]),
        .rls_native_wire_number(eigen$cumulative[[i]])
      )
    }
  }

  loading_rows <- if (is.data.frame(loadings)) nrow(loadings) else 0L
  payload <- c(payload, as.character(loading_rows))
  if (loading_rows) {
    for (i in seq_len(loading_rows)) {
      values <- unname(vapply(component_names, function(name) {
        .rls_native_wire_number(loadings[[name]][[i]])
      }, character(1L)))
      payload <- c(
        payload,
        .rls_native_wire_value(loadings$variable[[i]]),
        .rls_native_wire_number(loadings$communality[[i]]),
        .rls_native_wire_number(loadings$uniqueness[[i]]),
        as.character(length(values)),
        values
      )
    }
  }

  score_rows <- if (is.data.frame(scores)) nrow(scores) else 0L
  payload <- c(payload, as.character(score_rows))
  if (score_rows) {
    rows_used <- as.integer(record$rows_used_original_ids %||% seq_len(score_rows))
    for (i in seq_len(score_rows)) {
      values <- unname(vapply(score_names, function(name) {
        .rls_native_wire_number(scores[[name]][[i]])
      }, character(1L)))
      payload <- c(payload, as.character(rows_used[[i]]), as.character(length(values)), values)
    }
  }
  payload
}

.rls_dimension_sync_native_update <- function(record) {
  if (!isTRUE(.rls_state$process_started)) return(FALSE)
  result <- try(.rls_send(.rls_dimension_native_update_payload(record)), silent = TRUE)
  is.character(result) && length(result) && startsWith(result[[1L]], "OK")
}

#' Create a principal components / factor analysis window
#'
#' @param data Registered dataset name, a data frame, or `NULL` for active dataset.
#' @param variables Numeric variables to include. If `NULL`, the first numeric
#'   variables are selected and the native window can add or remove variables.
#' @param method `"pca"` for principal components or `"factor"` for factor analysis.
#' @param n_components Number of components/factors retained in the loading table.
#' @param scale Logical. Use standardized variables.
#' @param missing Missing-data mode. Currently the analysis uses listwise complete rows.
#' @param rotation Orthogonal rotation for the retained loadings: `"none"`,
#'   `"varimax"`, or `"quartimax"`.
#' @param scope Rows used by the analysis: `"all"`, `"selected"`, or
#'   `"unselected"`.
#' @param selected_rows Original one-based row ids used when `scope` is
#'   `"selected"` or `"unselected"`. The native window updates this from the
#'   linked selection state.
#' @param parallel Logical. Include a 95th percentile parallel-analysis
#'   reference in the scree table/plot.
#' @param parallel_iterations Number of simulated datasets for parallel analysis.
#' @param name Optional model id.
#' @param native Logical. Open the native window when possible.
#' @return A `rlispstat_dimensionality` handle.
#' @export
ls_new_dimensionality <- function(data = NULL, variables = NULL,
                                  method = c("pca", "factor"),
                                  n_components = NULL, scale = TRUE,
                                  missing = c("listwise", "pairwise"),
                                  rotation = c("none", "varimax", "quartimax"),
                                  scope = c("all", "selected", "unselected"),
                                  selected_rows = NULL,
                                  parallel = TRUE,
                                  parallel_iterations = 100L,
                                  name = NULL, native = TRUE) {
  method <- match.arg(method)
  missing <- match.arg(missing)
  rotation <- match.arg(rotation)
  scope <- match.arg(scope)
  if (is.data.frame(data)) {
    group <- .rls_register_dataset(name %||% "dimensionality", data, activate = TRUE)
    dataset <- .rls_dataset_record(group)
    id_name <- NULL
  } else {
    dataset <- .rls_dataset_record(data)
    id_name <- name
  }
  .rls_mi_warn_current_version(dataset, "Principal Components / Factor Analysis")
  variables <- .rls_dimension_validate_variables(dataset$data, variables, dataset$variable_metadata)
  n_components <- min(length(variables), max(1L, as.integer(n_components %||% min(2L, length(variables)))))
  selected_rows <- unique(as.integer(selected_rows %||% integer()))
  selected_rows <- selected_rows[!is.na(selected_rows) & selected_rows >= 1L]
  record <- list(
    id = .rls_dimension_id(dataset$group, id_name),
    group = dataset$group,
    dataset_id = dataset$group,
    data = dataset$data,
    variables = variables,
    method = method,
    n_components = n_components,
    scale = isTRUE(scale),
    missing_mode = missing,
    rotation = rotation,
    scope = scope,
    selected_rows = selected_rows,
    parallel = isTRUE(parallel),
    parallel_iterations = max(1L, as.integer(parallel_iterations %||% 100L)),
    eigenvalues = data.frame(),
    loadings = data.frame(),
    scores = data.frame(),
    rows_used_original_ids = integer(),
    rows_excluded_original_ids = integer(),
    model_version = 0L
  )
  record <- .rls_dimension_refit(record)
  handle <- .rls_assign_dimension(record)
  if (isTRUE(native)) {
    .rls_dimension_sync_native_open(record)
  }
  invisible(handle)
}

#' @rdname ls_new_dimensionality
#' @export
ls_dimensionality_state <- function(model) {
  .rls_dimension_record(model)
}

#' @rdname ls_new_dimensionality
#' @export
ls_dimensionality_eigenvalues <- function(model) {
  .rls_dimension_record(model)$eigenvalues
}

#' @rdname ls_new_dimensionality
#' @export
ls_dimensionality_loadings <- function(model) {
  .rls_dimension_record(model)$loadings
}

#' @rdname ls_new_dimensionality
#' @export
ls_dimensionality_scores <- function(model) {
  .rls_dimension_record(model)$scores
}

#' @rdname ls_new_dimensionality
#' @export
ls_dimensionality_save_scores <- function(model, prefix = NULL, components = NULL, dataset = NULL) {
  record <- .rls_dimension_record(model)
  target <- .rls_dataset_record(dataset %||% record$group)
  if (nrow(target$data) < max(record$rows_used_original_ids, 0L)) {
    stop("The target dataset does not contain the original analysis rows.", call. = FALSE)
  }
  scores <- record$scores
  if (!nrow(scores) || !ncol(scores)) {
    stop("No component/factor scores are available to save.", call. = FALSE)
  }
  components <- components %||% seq_len(ncol(scores))
  if (is.character(components)) {
    components <- match(components, names(scores))
  }
  components <- unique(as.integer(components))
  if (anyNA(components) || any(components < 1L | components > ncol(scores))) {
    stop("`components` must identify available score columns.", call. = FALSE)
  }
  prefix <- prefix %||% if (identical(record$method, "factor")) "F" else "PC"
  prefix <- .rls_validate_protocol_name(prefix, "prefix")
  new_names <- character(length(components))
  data <- target$data
  for (i in seq_along(components)) {
    component <- components[[i]]
    base <- paste0(prefix, component, "_score")
    name <- make.unique(c(names(data), base), sep = "_")[[ncol(data) + 1L]]
    values <- rep(NA_real_, nrow(data))
    values[record$rows_used_original_ids] <- scores[[component]]
    data[[name]] <- values
    new_names[[i]] <- name
  }
  target$data <- data
  target$data_frame <- data
  target$n_rows <- nrow(data)
  target$n_columns <- ncol(data)
  metadata <- target$variable_metadata %||% target$metadata %||% .rls_variable_metadata(data)
  for (name in new_names) {
    metadata <- .rls_refresh_metadata_row(metadata, data, name, "numeric")
  }
  target$metadata <- metadata
  target$variable_metadata <- metadata
  target$modified <- TRUE
  .rls_set_dataset_record(target)
  .rls_notify_dataset_changed(target, variable = NULL)
  record$data <- data
  .rls_assign_dimension(record)
  invisible(new_names)
}

#' @rdname ls_new_dimensionality
#' @export
ls_dimensionality_add_variable <- function(model, variable) {
  record <- .rls_dimension_record(model)
  dataset <- .rls_dataset_record(record$group)
  variable <- .rls_dimension_validate_variables(record$data, variable, dataset$variable_metadata, min_variables = 1L)
  record$variables <- unique(c(record$variables, variable))
  record$n_components <- min(record$n_components, length(record$variables))
  record <- .rls_dimension_refit(record)
  handle <- .rls_assign_dimension(record)
  if (isTRUE(.rls_state$process_started)) {
    try(.rls_send(c("PCAFA_SET_VARIABLES", record$id, as.character(length(record$variables)), record$variables)), silent = TRUE)
  }
  invisible(handle)
}

#' @rdname ls_new_dimensionality
#' @export
ls_dimensionality_remove_variable <- function(model, variable) {
  record <- .rls_dimension_record(model)
  variable <- .rls_validate_protocol_name(variable, "variable")
  record$variables <- setdiff(record$variables, variable)
  if (length(record$variables) < 2L) {
    stop("At least two variables must remain in the analysis.", call. = FALSE)
  }
  record$n_components <- min(record$n_components, length(record$variables))
  record <- .rls_dimension_refit(record)
  handle <- .rls_assign_dimension(record)
  if (isTRUE(.rls_state$process_started)) {
    try(.rls_send(c("PCAFA_SET_VARIABLES", record$id, as.character(length(record$variables)), record$variables)), silent = TRUE)
  }
  invisible(handle)
}
