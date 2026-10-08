.rls_scale_id <- function(group, name = NULL) {
  .rls_validate_protocol_name(
    name %||% paste0("scale_", group, "_", format(Sys.time(), "%Y%m%d%H%M%OS3")),
    "name"
  )
}

.rls_scale_record <- function(analysis) {
  id <- if (inherits(analysis, "rlispstat_scale_analysis")) analysis$id else analysis
  id <- .rls_validate_protocol_name(id, "analysis")
  if (!exists(id, envir = .rls_state$scale_analyses, inherits = FALSE)) {
    stop("Unknown scale analysis.", call. = FALSE)
  }
  get(id, envir = .rls_state$scale_analyses, inherits = FALSE)
}

.rls_assign_scale <- function(record) {
  assign(record$id, record, envir = .rls_state$scale_analyses)
  structure(list(id = record$id, group = record$group), class = "rlispstat_scale_analysis")
}

.rls_scale_require_psych <- function(context = "Scale Analysis") {
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop(sprintf(
      "%s requires the R package `psych`. Install `psych` in the LinkEDA runtime library and try again.",
      context
    ), call. = FALSE)
  }
  invisible(TRUE)
}

.rls_scale_specification_data <- function(dataset) {
  if (.rls_mi_is_dataset(dataset)) {
    # dataset$data is the compact worksheet preview for an MI dataset.  Cells
    # whose values vary by imputation can therefore contain display strings
    # such as "No | Yes"; those strings are not statistical data and must not
    # be used to validate item types or derive factor levels.  Every completed
    # dataset has the canonical classes/levels used by the actual fits.
    completed <- .rls_mi_completed_datasets(dataset)
    reference <- completed[[1L]]
    sets <- c(list(dataset$original_data), completed)
    for (name in names(reference)) {
      columns <- lapply(sets, function(data) data[[name]])
      values <- unlist(lapply(columns, function(x) if (is.factor(x)) levels(x) else x[!is.na(x)]), use.names = FALSE)
      levels <- if (is.factor(reference[[name]])) unique(as.character(values)) else sort(unique(values), na.last = TRUE)
      if (!identical(as.character(levels), .rls_scale_item_levels(reference[[name]])))
        attr(reference[[name]], "linkeda_scale_levels") <- as.character(levels)
    }
    return(reference)
  }
  dataset$data
}

.rls_scale_item_levels <- function(x) {
  attr(x, "linkeda_scale_levels", exact = TRUE) %||%
    if (is.factor(x)) levels(x) else as.character(sort(unique(x[!is.na(x)]), na.last = TRUE))
}

.rls_scale_item_specifications <- function(data, items, item_types = NULL,
                                           reverse_items = character(),
                                           scoring_ranges = list(), metadata = NULL) {
  items <- unique(vapply(items %||% character(), .rls_validate_protocol_name,
                         character(1L), what = "item"))
  missing <- setdiff(items, names(data))
  if (length(missing)) {
    stop(sprintf("Column `%s` was not found in the dataset.", missing[[1L]]), call. = FALSE)
  }
  item_types <- item_types %||% list()
  if (is.atomic(item_types) && !is.list(item_types)) item_types <- as.list(item_types)
  if (is.null(names(item_types)) && length(item_types)) names(item_types) <- items[seq_along(item_types)]
  reverse_items <- unique(as.character(reverse_items %||% character()))
  unknown_reverse <- setdiff(reverse_items, items)
  if (length(unknown_reverse)) {
    stop(sprintf("Reverse-scored item `%s` is not in the scale.", unknown_reverse[[1L]]), call. = FALSE)
  }

  lapply(items, function(item) {
    x <- data[[item]]
    declared <- tolower(as.character(item_types[[item]] %||% "auto"))
    if (declared == "auto") {
      declared <- if (is.ordered(x)) "ordinal" else if (is.numeric(x)) "numeric"
        else if (length(unique(x[!is.na(x)])) == 2L) "ordinal" else "unordered"
    }
    if (!declared %in% c("numeric", "continuous", "ordinal", "unordered")) {
      stop(sprintf("Unsupported scale item type `%s` for `%s`.", declared, item), call. = FALSE)
    }
    if (declared == "continuous") declared <- "numeric"
    if (declared == "unordered") {
      stop(sprintf(
        "Column `%s` is Categorical. Explicitly mark it as Ordinal before using it as a scale item.",
        item
      ), call. = FALSE)
    }
    numeric_factor_encoding <- NULL
    if (identical(declared, "numeric") && !is.numeric(x)) {
      converted <- suppressWarnings(as.numeric(as.character(x)))
      if (any(is.na(converted) & !is.na(x))) {
        # Imported dichotomous questionnaire items are commonly stored by
        # mice as factors, characters, or logicals such as No/Yes even when
        # the worksheet analysis type is numeric.  Use the two *observed*
        # categories: imported factors can retain unused labels, and requiring
        # exactly two declared levels would reject an otherwise valid binary
        # item.  Factors with three or more observed unordered levels remain
        # an error and are never silently promoted to ordinal items.
        observed <- unique(as.character(x[!is.na(x)]))
        if (length(observed) == 2L) {
          encoding_levels <- if (is.factor(x)) {
            c(intersect(levels(x), observed), setdiff(observed, levels(x)))
          } else {
            sort(observed, na.last = TRUE)
          }
          numeric_factor_encoding <- list(
            method = "binary_zero_one",
            levels = encoding_levels
          )
        } else {
          stop(sprintf("Column `%s` cannot be treated as a numeric scale item.", item), call. = FALSE)
        }
      }
    }
    range <- scoring_ranges[[item]] %||% NULL
    if (!is.null(range)) {
      range <- as.numeric(range)
      if (length(range) != 2L || any(!is.finite(range)) || range[[1L]] >= range[[2L]]) {
        stop(sprintf("The scoring range for `%s` must contain a finite minimum and maximum.", item),
             call. = FALSE)
      }
    }
    if (item %in% reverse_items && identical(declared, "numeric") && is.null(range)) {
      stop(sprintf(
        "Reverse scoring continuous item `%s` requires an explicit or metadata-defined theoretical range; observed minima/maxima are never used.",
        item
      ), call. = FALSE)
    }
    list(name = item, type = declared, direction = if (item %in% reverse_items) "reversed" else "forward",
         scoring_range = range, numeric_factor_encoding = numeric_factor_encoding,
         ordinal_levels = if (declared == "ordinal") .rls_scale_item_levels(x) else NULL)
  })
}

.rls_scale_prepare_items <- function(data, item_specs) {
  out <- data.frame(row.names = seq_len(nrow(data)))
  for (spec in item_specs) {
    x <- data[[spec$name]]
    if (identical(spec$type, "ordinal")) {
      # Specifications created before this field existed retain a legacy fallback.
      levels_now <- spec$ordinal_levels %||% .rls_scale_item_levels(x)
      unknown <- !is.na(x) & !as.character(x) %in% levels_now
      if (any(unknown)) stop(sprintf(
        "Column `%s` contains a category outside the frozen ordinal coding. Refit the scale items.",
        spec$name), call. = FALSE)
      x <- ordered(x, levels = levels_now)
      if (identical(spec$direction, "reversed")) {
        original <- levels(x)
        x <- ordered(x, levels = rev(original))
      }
      out[[spec$name]] <- x
    } else {
      encoding <- spec$numeric_factor_encoding %||% NULL
      if (is.list(encoding) && identical(encoding$method, "binary_zero_one")) {
        original_missing <- is.na(x)
        x <- match(as.character(x), encoding$levels) - 1L
        if (any(is.na(x) & !original_missing)) {
          stop(sprintf(
            "Column `%s` contains a category not present in the binary scale-item specification.",
            spec$name
          ), call. = FALSE)
        }
        x[original_missing] <- NA_real_
        x <- as.numeric(x)
      } else {
        x <- if (is.numeric(x)) as.numeric(x) else suppressWarnings(as.numeric(as.character(x)))
      }
      if (identical(spec$direction, "reversed")) {
        range <- spec$scoring_range
        x <- range[[1L]] + range[[2L]] - x
      }
      out[[spec$name]] <- x
    }
  }
  out
}

.rls_scale_numeric_matrix <- function(prepared, item_specs) {
  out <- prepared
  for (spec in item_specs) {
    if (identical(spec$type, "ordinal")) out[[spec$name]] <- as.numeric(out[[spec$name]])
  }
  as.data.frame(out, stringsAsFactors = FALSE)
}

.rls_scale_correlation_method <- function(item_specs, method = "auto") {
  method <- match.arg(tolower(method), c("auto", "pearson", "polychoric", "mixed"))
  types <- vapply(item_specs, `[[`, character(1L), "type")
  if (method != "auto") return(method)
  if (all(types == "numeric")) "pearson" else if (all(types == "ordinal")) "polychoric" else "mixed"
}

.rls_scale_correlations_one <- function(prepared, item_specs, method = "auto",
                                        missing = "pairwise") {
  numeric_data <- .rls_scale_numeric_matrix(prepared, item_specs)
  missing <- match.arg(tolower(missing), c("listwise", "pairwise"))
  .rls_correlation_psych_matrix(
    numeric_data,
    variable_types = vapply(item_specs, `[[`, character(1L), "type"),
    method = method,
    use = if (identical(missing, "listwise")) "complete" else "pairwise",
    smooth = FALSE
  )
}

.rls_scale_alpha_one <- function(prepared, item_specs) {
  .rls_scale_require_psych("Scale reliability")
  numeric_data <- .rls_scale_numeric_matrix(prepared, item_specs)
  if (ncol(numeric_data) < 2L) stop("Scale reliability requires at least two items.", call. = FALSE)
  fit <- suppressMessages(suppressWarnings(psych::alpha(
    numeric_data, check.keys = FALSE, warnings = FALSE, delete = TRUE, use = "pairwise"
  )))
  item_names <- names(numeric_data)
  rows <- data.frame(
    item = item_names,
    type = vapply(item_specs, `[[`, character(1L), "type"),
    direction = vapply(item_specs, `[[`, character(1L), "direction"),
    n = as.numeric(fit$item.stats[item_names, "n"]),
    mean = as.numeric(fit$item.stats[item_names, "mean"]),
    sd = as.numeric(fit$item.stats[item_names, "sd"]),
    item_rest_r = as.numeric(fit$item.stats[item_names, "r.drop"]),
    alpha_if_deleted = as.numeric(fit$alpha.drop[item_names, "raw_alpha"]),
    stringsAsFactors = FALSE
  )
  rows$missing_percent <- 100 * colMeans(is.na(numeric_data))
  list(
    raw = fit,
    summary = list(
      alpha = unname(fit$total[["raw_alpha"]]),
      standardized_alpha = unname(fit$total[["std.alpha"]]),
      mean_inter_item_r = unname(fit$total[["average_r"]]),
      scale_mean = unname(fit$total[["mean"]]),
      scale_sd = unname(fit$total[["sd"]])
    ),
    items = rows,
    package = "psych",
    function_name = "psych::alpha"
  )
}

.rls_scale_omega_one <- function(prepared, item_specs, enabled = FALSE) {
  if (!isTRUE(enabled)) {
    return(list(value = NA_real_, status = "not_requested", package = "psych",
                function_name = "psych::omega"))
  }
  .rls_scale_require_psych("Omega reliability")
  numeric_data <- .rls_scale_numeric_matrix(prepared, item_specs)
  if (ncol(numeric_data) < 3L) {
    return(list(value = NA_real_, status = "unavailable_fewer_than_three_items",
                package = "psych", function_name = "psych::omega"))
  }
  value <- tryCatch({
    suppressMessages(suppressWarnings(invisible(capture.output(
      result <- psych::omega(numeric_data, plot = FALSE, warnings = FALSE, flip = FALSE)
    ))))
    result
  }, error = identity)
  if (inherits(value, "error")) {
    return(list(value = NA_real_, status = paste0("unavailable: ", conditionMessage(value)),
                package = "psych", function_name = "psych::omega"))
  }
  list(value = unname(value$omega.tot %||% NA_real_), status = "value", raw = value,
       package = "psych", function_name = "psych::omega")
}

.rls_scale_scores_one <- function(prepared, item_specs, score = "mean", minimum_valid_items = NULL) {
  score <- match.arg(score, c("mean", "sum"))
  numeric_data <- .rls_scale_numeric_matrix(prepared, item_specs)
  minimum_valid_items <- as.integer(minimum_valid_items %||% ncol(numeric_data))
  minimum_valid_items <- max(1L, min(ncol(numeric_data), minimum_valid_items))
  valid <- rowSums(!is.na(numeric_data))
  values <- if (identical(score, "sum")) rowSums(numeric_data, na.rm = TRUE) else rowMeans(numeric_data, na.rm = TRUE)
  values[valid < minimum_valid_items] <- NA_real_
  data.frame(row = seq_len(nrow(numeric_data)), score = values, valid_items = valid,
             stringsAsFactors = FALSE)
}

.rls_scale_dimensionality_one <- function(prepared, item_specs, correlation, options,
                                          correlation_method = "auto") {
  if (!isTRUE(options$enabled)) {
    return(list(status = "not_requested", loadings = data.frame(), parallel = list()))
  }
  numeric_data <- .rls_scale_numeric_matrix(prepared, item_specs)
  missing <- match.arg(tolower(options$missing %||% "pairwise"),
                       c("listwise", "pairwise"))
  dimension_correlation <- if (identical(missing, "pairwise")) correlation else {
    .rls_scale_correlations_one(
      prepared, item_specs, correlation_method, missing
    )
  }
  r <- dimension_correlation$matrix
  requested_scale <- isTRUE(options$scale %||% TRUE)
  covariance_available <- identical(dimension_correlation$method, "pearson") &&
    all(vapply(item_specs, `[[`, character(1L), "type") == "numeric")
  effective_scale <- requested_scale || !covariance_available
  if (!effective_scale) {
    r <- stats::cov(
      numeric_data,
      use = if (identical(missing, "listwise")) "complete.obs" else "pairwise.complete.obs"
    )
  }
  complete_rows <- stats::complete.cases(numeric_data)
  n_obs <- if (identical(missing, "listwise")) sum(complete_rows) else {
    sizes <- dimension_correlation$sample_sizes %||% matrix(nrow(numeric_data), ncol(r), ncol(r))
    finite <- sizes[upper.tri(sizes) & is.finite(sizes)]
    if (length(finite)) min(finite) else sum(complete_rows)
  }
  method <- match.arg(tolower(options$method %||% "factor"), c("factor", "pca"))
  engine <- if (identical(method, "pca")) {
    .rls_dimension_pca_from_correlation
  } else {
    .rls_dimension_factor_from_correlation
  }
  result <- engine(
    r, n_obs,
    n_components = options$factors,
    extraction = options$extraction,
    rotation = options$rotation,
    parallel_iterations = options$parallel_iterations,
    score_data = numeric_data,
    scale = effective_scale, missing = missing, run_parallel = options[["parallel", exact = TRUE]] %||% TRUE
  )
  result$method <- method
  result$missing <- missing
  result$scale <- effective_scale
  result$report_summary <- paste0(sum(complete_rows), " complete item rows; ",
    sum(!complete_rows), " incomplete rows. Matrix N", if (missing == "pairwise") " (minimum pairwise)" else "", " = ", n_obs, ".")
  result$report_method <- paste0(
    if (method == "pca") "psych::principal" else paste0("psych::fa (", options$extraction, ")"),
    "; ", options$rotation, "; ", dimension_correlation$method, "; ", missing,
    if (effective_scale) "; correlation matrix." else "; covariance matrix.")
  result$report_parallel <- if (identical(options[["parallel", exact = TRUE]], FALSE)) "Parallel analysis disabled." else if (inherits(result$parallel, "error"))
    paste("Parallel analysis unavailable:", conditionMessage(result$parallel)) else
    if (!length(result$parallel$reference)) "Parallel analysis unavailable." else
    paste0("Parallel analysis: psych::fa.parallel; ", options$parallel_iterations,
      " replicates; 95th percentile; ", result$parallel$reference_method, ".")
  result
}

.rls_scale_fit_one <- function(data, item_specs, options) {
  prepared <- .rls_scale_prepare_items(data, item_specs)
  alpha <- .rls_scale_alpha_one(prepared, item_specs)
  correlation <- .rls_scale_correlations_one(prepared, item_specs, options$correlation_method)
  omega <- .rls_scale_omega_one(prepared, item_specs, options$omega)
  scores <- .rls_scale_scores_one(prepared, item_specs, options$score, options$minimum_valid_items)
  dimensionality <- .rls_scale_dimensionality_one(
    prepared, item_specs, correlation, options$dimensionality,
    options$correlation_method
  )
  list(alpha = alpha, correlation = correlation, omega = omega, scores = scores,
       dimensionality = dimensionality, prepared = prepared)
}

.rls_scale_descriptive_distribution <- function(values) {
  values <- as.numeric(values)
  finite <- values[is.finite(values)]
  if (!length(finite)) return(list(mean = NA_real_, median = NA_real_, min = NA_real_, max = NA_real_))
  list(mean = mean(finite), median = stats::median(finite), min = min(finite), max = max(finite))
}

.rls_scale_score_summary <- function(scores, method, status = "value",
                                     valid_n = NULL,
                                     valid_n_by_imputation = NULL) {
  values <- as.numeric(scores$score %||% numeric())
  finite <- values[is.finite(values)]
  if (is.null(valid_n)) valid_n <- length(finite)
  list(
    method = method,
    status = status,
    # For MI analyses this is the number of original cases represented in
    # each completed dataset, never the length of the stacked m * N score
    # vector.  The latter is retained separately as score_estimates.
    valid_n = valid_n,
    valid_n_by_imputation = valid_n_by_imputation,
    score_estimates = length(finite),
    mean = if (length(finite)) mean(finite) else NA_real_,
    sd = if (length(finite) > 1L) stats::sd(finite) else NA_real_,
    min = if (length(finite)) min(finite) else NA_real_,
    max = if (length(finite)) max(finite) else NA_real_
  )
}

.rls_scale_histogram_plot_series <- function(values, kind, name, breaks = NULL) {
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  if (!length(values)) return(NULL)
  if (is.null(breaks)) {
    limits <- range(values)
    if (limits[[1L]] == limits[[2L]]) limits <- limits + c(-0.5, 0.5)
    breaks <- pretty(limits, n = min(10L, max(3L, ceiling(sqrt(length(values))))))
  }
  if (length(unique(breaks)) < 2L) return(NULL)
  histogram <- graphics::hist(values, breaks = breaks, plot = FALSE,
                              include.lowest = TRUE, right = TRUE)
  total <- sum(histogram$counts)
  list(
    kind = kind,
    name = name,
    labels = if (identical(kind, "item_distribution")) paste0(
      c("[", rep("(", length(histogram$counts) - 1L)),
      format(head(histogram$breaks, -1L), digits = 5L, trim = TRUE), ", ",
      format(tail(histogram$breaks, -1L), digits = 5L, trim = TRUE), "]")
    else format(histogram$mids, digits = 5L, trim = TRUE),
    values = if (total > 0L) histogram$counts / total else histogram$counts
  )
}

.rls_scale_item_distribution_series <- function(fits, item_specs) {
  series <- list()
  for (index in seq_along(item_specs)) {
    spec <- item_specs[[index]]
    values_by_fit <- lapply(fits, function(fit) fit$prepared[[index]])
    if (identical(spec$type, "ordinal")) {
      labels <- unique(unlist(lapply(values_by_fit, levels), use.names = FALSE))
      if (!length(labels)) next
      proportions <- vapply(values_by_fit, function(values) {
        counts <- table(factor(values, levels = labels), useNA = "no")
        if (sum(counts)) as.numeric(counts) / sum(counts) else rep(NA_real_, length(labels))
      }, numeric(length(labels)))
      values <- if (is.null(dim(proportions))) proportions else rowMeans(proportions, na.rm = TRUE)
      series[[length(series) + 1L]] <- list(
        kind = "item_distribution", name = spec$name,
        labels = labels, values = values
      )
    } else {
      all_values <- unlist(values_by_fit, use.names = FALSE)
      finite <- as.numeric(all_values[is.finite(all_values)])
      if (!length(finite)) next
      limits <- range(finite)
      if (limits[[1L]] == limits[[2L]]) limits <- limits + c(-0.5, 0.5)
      breaks <- pretty(limits, n = min(10L, max(3L, ceiling(sqrt(length(finite))))))
      per_fit <- lapply(seq_along(values_by_fit), function(imputation) {
        .rls_scale_histogram_plot_series(
          values_by_fit[[imputation]], "item_distribution", spec$name, breaks
        )
      })
      per_fit <- Filter(Negate(is.null), per_fit)
      if (!length(per_fit)) next
      matrix_values <- do.call(cbind, lapply(per_fit, `[[`, "values"))
      series[[length(series) + 1L]] <- list(
        kind = "item_distribution", name = spec$name,
        labels = per_fit[[1L]]$labels,
        values = if (is.null(dim(matrix_values))) matrix_values else rowMeans(matrix_values, na.rm = TRUE)
      )
    }
  }
  series
}

.rls_scale_score_distribution_series <- function(fits, method) {
  all_values <- unlist(lapply(fits, function(fit) fit$scores$score), use.names = FALSE)
  finite <- as.numeric(all_values[is.finite(all_values)])
  if (!length(finite)) return(list())
  limits <- range(finite)
  if (limits[[1L]] == limits[[2L]]) limits <- limits + c(-0.5, 0.5)
  breaks <- pretty(limits, n = min(10L, max(3L, ceiling(sqrt(length(finite))))))
  output <- lapply(seq_along(fits), function(index) {
    .rls_scale_histogram_plot_series(
      fits[[index]]$scores$score, "score_distribution",
      if (length(fits) == 1L) paste0(tools::toTitleCase(method), " score")
      else paste0("Imputation ", index), breaks
    )
  })
  Filter(Negate(is.null), output)
}

.rls_scale_scree_series <- function(dimensionalities, multiple_imputation = FALSE) {
  valid_indices <- which(vapply(dimensionalities, function(value) {
    parallel <- value$parallel %||% NULL
    is.list(parallel) && length(parallel$fa.values %||% numeric()) > 0L
  }, logical(1L)))
  valid <- dimensionalities[valid_indices]
  if (!length(valid)) return(list())
  observed_by_imputation <- lapply(valid, function(value) as.numeric(value$parallel$fa.values))
  reference_by_imputation <- lapply(valid, function(value) as.numeric(value$parallel$reference))
  observed <- observed_by_imputation
  reference <- reference_by_imputation
  length_used <- min(vapply(observed, length, integer(1L)))
  observed <- vapply(observed, function(value) value[seq_len(length_used)], numeric(length_used))
  observed <- if (is.null(dim(observed))) observed else rowMeans(observed, na.rm = TRUE)
  reference <- Filter(function(value) length(value) >= length_used, reference)
  reference <- if (length(reference)) {
    values <- vapply(reference, function(value) value[seq_len(length_used)], numeric(length_used))
    if (is.null(dim(values))) values else rowMeans(values, na.rm = TRUE)
  } else rep(NA_real_, length_used)
  suffix <- if (!multiple_imputation) "" else if (length(valid_indices) < length(dimensionalities))
    sprintf(" mean across available imputations (descriptive; %d of %d)", length(valid_indices), length(dimensionalities))
    else " mean across imputations (descriptive)"
  result <- list(
    list(kind = "scree_observed", name = paste0("Observed", suffix),
         labels = as.character(seq_len(length_used)), values = observed),
    list(kind = "scree_reference", name = paste0("Parallel reference", suffix),
         labels = as.character(seq_len(length_used)), values = reference)
  )
  if (isTRUE(multiple_imputation)) {
    for (index in seq_along(observed_by_imputation)) {
      result[[length(result) + 1L]] <- list(
        kind = "scree_observed_imputation", name = paste0("Imputation ", valid_indices[[index]]),
        labels = as.character(seq_len(length_used)),
        values = observed_by_imputation[[index]][seq_len(length_used)]
      )
      if (length(reference_by_imputation[[index]]) >= length_used) {
        result[[length(result) + 1L]] <- list(
          kind = "scree_reference_imputation", name = paste0("Imputation ", valid_indices[[index]]),
          labels = as.character(seq_len(length_used)),
          values = reference_by_imputation[[index]][seq_len(length_used)]
        )
      }
    }
  }
  result
}

.rls_scale_dimensionality_series <- function(dimensionalities) {
  output <- list()
  for (imputation in seq_along(dimensionalities)) {
    value <- dimensionalities[[imputation]] %||% list()
    metadata <- c(value$report_summary %||% "", value$report_method %||% "",
                  value$report_parallel %||% "", value$status %||% "")
    output[[length(output) + 1L]] <- list(kind = "dimensionality_metadata",
      name = as.character(imputation), labels = metadata, values = rep(0, length(metadata)))
    loadings <- value$loadings %||% data.frame()
    if (is.data.frame(loadings) && nrow(loadings)) {
      factor_names <- setdiff(names(loadings), c("item", "h2", "u2"))
      for (factor in factor_names) {
        output[[length(output) + 1L]] <- list(
          kind = "factor_loading_imputation",
          name = paste0(imputation, "|", factor),
          labels = as.character(loadings$item),
          values = as.numeric(loadings[[factor]])
        )
      }
      output[[length(output) + 1L]] <- list(
        kind = "factor_communality_imputation", name = as.character(imputation),
        labels = as.character(loadings$item), values = as.numeric(loadings$h2)
      )
      output[[length(output) + 1L]] <- list(
        kind = "factor_uniqueness_imputation", name = as.character(imputation),
        labels = as.character(loadings$item), values = as.numeric(loadings$u2)
      )
    }
    # PCA proportions refer to unrotated component eigenvalues. EFA scree
    # eigenvalues are not converted into misleading percentages in native UI.
    for (field in c("component_variance", "component_cumulative")) {
      values <- value[[field]] %||% numeric()
      if (length(values)) output[[length(output) + 1L]] <- list(
        kind = paste0(field, "_imputation"), name = as.character(imputation),
        labels = as.character(seq_along(values)), values = values)
    }
    scores <- value$scores %||% data.frame()
    if (is.data.frame(scores) && nrow(scores) && "row" %in% names(scores)) {
      for (factor in setdiff(names(scores), "row")) {
        output[[length(output) + 1L]] <- list(
          kind = "factor_score_imputation",
          name = paste0(imputation, "|", factor),
          labels = as.character(scores$row),
          values = as.numeric(scores[[factor]])
        )
      }
    }
  }
  output
}

.rls_scale_pool_scalar_safe <- function(estimates, variances) {
  estimates <- as.numeric(estimates)
  variances <- as.numeric(variances)
  if (length(estimates) < 2L || any(!is.finite(estimates)) || any(!is.finite(variances)) || any(variances < 0)) {
    return(.rls_mi_empty_scalar_pool(length(estimates), "one or more imputations were not estimable"))
  }
  .rls_mi_pool_scalar(estimates, variances)
}

.rls_scale_pool_fisher <- function(values, sample_sizes) {
  pool <- .rls_mi_pool_correlation(values, sample_sizes)$pool
  pool$r <- if (isTRUE(pool$valid)) tanh(pool$Qbar) else NA_real_
  pool
}

# A loading table for one completed dataset does not guarantee that every
# imputation has a proper factor solution and finite case-level scores.
.rls_scale_score_availability <- function(dimensions) {
  total <- length(dimensions)
  usable <- vapply(dimensions, function(value) {
    scores <- value$scores %||% data.frame()
    is.data.frame(scores) && nrow(scores) > 0L && ncol(scores) > 1L &&
      all(vapply(scores[-1L], function(column) all(is.finite(column)), logical(1L)))
  }, logical(1L))
  failed <- which(!usable)
  first_reason <- if (length(failed)) {
    value <- dimensions[[failed[[1L]]]]
    reason <- value$score_status %||% ""
    if (!nzchar(reason)) reason <- value$status %||% ""
    if (!nzchar(reason) || identical(reason, "value"))
      reason <- "No valid factor/component scores were returned."
    reason
  } else ""
  list(available = sum(usable), total = total,
       failed = failed, first_reason = first_reason)
}

.rls_scale_fit_mi <- function(dataset, item_specs, options) {
  .rls_mi_require_mice("Scale Analysis")
  completed <- .rls_mi_completed_datasets(dataset)
  fits <- lapply(completed, .rls_scale_fit_one, item_specs = item_specs, options = options)
  item_names <- vapply(item_specs, `[[`, character(1L), "name")
  original <- dataset$original_data %||% dataset$data
  original_missing <- vapply(item_names, function(item) 100 * mean(is.na(original[[item]])), numeric(1L))

  item_rows <- lapply(seq_along(item_names), function(j) {
    means <- vapply(fits, function(fit) fit$alpha$items$mean[[j]], numeric(1L))
    ns <- vapply(fits, function(fit) fit$alpha$items$n[[j]], numeric(1L))
    sds <- vapply(fits, function(fit) fit$alpha$items$sd[[j]], numeric(1L))
    mean_pool <- .rls_scale_pool_scalar_safe(means, (sds^2) / pmax(ns, 1))
    rests <- vapply(fits, function(fit) fit$alpha$items$item_rest_r[[j]], numeric(1L))
    rest_pool <- .rls_scale_pool_fisher(rests, ns)
    deleted <- vapply(fits, function(fit) fit$alpha$items$alpha_if_deleted[[j]], numeric(1L))
    data.frame(
      item = item_names[[j]], type = item_specs[[j]]$type, direction = item_specs[[j]]$direction,
      n = round(mean(ns)), mean = mean_pool$Qbar, mean_se = mean_pool$SE,
      sd = mean(sds, na.rm = TRUE), missing_percent = original_missing[[j]],
      item_rest_r = rest_pool$r, item_rest_se_z = rest_pool$SE,
      item_rest_reason = if (isTRUE(rest_pool$valid)) "" else rest_pool$reason,
      alpha_if_deleted = mean(deleted, na.rm = TRUE),
      alpha_if_deleted_min = min(deleted, na.rm = TRUE),
      alpha_if_deleted_max = max(deleted, na.rm = TRUE), stringsAsFactors = FALSE
    )
  })
  item_rows <- do.call(rbind, item_rows)

  alphas <- vapply(fits, function(x) x$alpha$summary$alpha, numeric(1L))
  standardized <- vapply(fits, function(x) x$alpha$summary$standardized_alpha, numeric(1L))
  average_r <- vapply(fits, function(x) x$alpha$summary$mean_inter_item_r, numeric(1L))
  scale_means <- vapply(fits, function(x) x$alpha$summary$scale_mean, numeric(1L))
  scale_sds <- vapply(fits, function(x) x$alpha$summary$scale_sd, numeric(1L))
  omega <- vapply(fits, function(x) x$omega$value, numeric(1L))
  scores <- lapply(seq_along(fits), function(i) transform(fits[[i]]$scores, imputation = i))
  correlations <- lapply(fits, function(x) x$correlation$matrix)
  correlation_sample_sizes <- lapply(fits, function(x) x$correlation$sample_sizes)
  correlation_method <- fits[[1L]]$correlation$method
  pooled_correlation <- NULL
  pooled_p_values <- NULL
  pooled_sample_sizes <- NULL
  correlation_reasons <- matrix("", length(item_names), length(item_names),
                                dimnames = list(item_names, item_names))
  correlation_status <- "descriptive_by_imputation"
  if (length(correlation_sample_sizes)) {
    sample_size_array <- simplify2array(correlation_sample_sizes)
    if (length(dim(sample_size_array)) == 3L) {
      # The native shared correlation table has an integer N field.  MI
      # completed datasets normally share the same usable rows; if a valid-N
      # count differs across imputations, expose the nearest descriptive mean
      # rather than silently truncating it in the wire parser.
      pooled_sample_sizes <- round(apply(sample_size_array, c(1L, 2L), mean, na.rm = TRUE))
      dimnames(pooled_sample_sizes) <- dimnames(correlation_sample_sizes[[1L]])
    }
  }
  if (identical(correlation_method, "pearson")) {
    p <- length(item_names)
    pooled_correlation <- diag(1, p)
    dimnames(pooled_correlation) <- list(item_names, item_names)
    pooled_p_values <- matrix(NA_real_, p, p, dimnames = list(item_names, item_names))
    for (i in seq_len(p - 1L)) for (j in seq.int(i + 1L, p)) {
      r <- vapply(correlations, function(x) x[i, j], numeric(1L))
      n <- vapply(correlation_sample_sizes, function(x) x[i, j], numeric(1L))
      pooled <- .rls_scale_pool_fisher(r, n)
      pooled_correlation[i, j] <- pooled_correlation[j, i] <- pooled$r
      pooled_p_values[i, j] <- pooled_p_values[j, i] <- pooled$p
      if (!isTRUE(pooled$valid)) {
        correlation_reasons[i, j] <- correlation_reasons[j, i] <- pooled$reason
      }
    }
    correlation_status <- "formal_fisher_z_plus_mice_pool_scalar"
  } else if (length(correlations)) {
    # Polychoric and mixed correlations have no generally accepted Rubin
    # pooling rule.  A labelled element-wise descriptive mean is nevertheless
    # useful for the correlation window and heatmap; the original matrices are
    # retained so no inferential pooling is implied or lost.
    correlation_array <- simplify2array(correlations)
    if (length(dim(correlation_array)) == 3L) {
      pooled_correlation <- apply(correlation_array, c(1L, 2L), mean, na.rm = TRUE)
      dimnames(pooled_correlation) <- dimnames(correlations[[1L]])
      correlation_status <- "descriptive_mean_across_imputations_not_pooled"
    }
    if (is.matrix(pooled_correlation)) {
      pooled_p_values <- matrix(NA_real_, nrow(pooled_correlation), ncol(pooled_correlation),
                                dimnames = dimnames(pooled_correlation))
    }
  }

  dimensionality <- list(status = "unavailable_under_mi_unaligned_loadings")
  if (isTRUE(options$dimensionality$enabled)) {
    suggestions <- vapply(fits, function(x) x$dimensionality$suggested_factors %||% NA_integer_, integer(1L))
    dimensionality <- list(
      status = "descriptive_by_imputation_loadings_not_pooled",
      suggested_factor_count = as.list(table(suggestions[is.finite(suggestions)])),
      results_by_imputation = lapply(fits, `[[`, "dimensionality"),
      score_availability = .rls_scale_score_availability(
        lapply(fits, `[[`, "dimensionality"))
    )
  }

  unavailable_rest <- sum(nzchar(item_rows$item_rest_reason))
  unavailable_pairs <- sum(nzchar(correlation_reasons[upper.tri(correlation_reasons)]))
  pooling_note <- if (unavailable_rest + unavailable_pairs > 0L) {
    sprintf(paste0("Fisher-z pooling unavailable for %d item-rest correlation(s) and %d item pair(s). ",
                   "Every imputation requires n > 3 and a finite correlation strictly between -1 and 1; ",
                   "at least two imputations are required. Unavailable results are shown as dashes."),
            unavailable_rest, unavailable_pairs)
  } else ""
  score_rows <- do.call(rbind, scores)
  valid_score_counts <- vapply(fits, function(fit) {
    sum(is.finite(as.numeric(fit$scores$score %||% numeric())))
  }, integer(1L))
  valid_case_display <- if (length(unique(valid_score_counts)) == 1L) {
    as.character(valid_score_counts[[1L]])
  } else {
    paste0(min(valid_score_counts), "\u2013", max(valid_score_counts))
  }
  valid_case_description <- if (length(unique(valid_score_counts)) == 1L) {
    paste0(valid_case_display, " valid cases in each imputation")
  } else {
    paste0(valid_case_display, " valid cases per imputation")
  }
  plot_series <- c(
    .rls_scale_item_distribution_series(fits, item_specs),
    .rls_scale_score_distribution_series(fits, options$score),
    .rls_scale_scree_series(lapply(fits, `[[`, "dimensionality"), TRUE),
    .rls_scale_dimensionality_series(lapply(fits, `[[`, "dimensionality"))
  )
  if (isTRUE(options$dimensionality$enabled)) {
    suggestions <- vapply(fits, function(x) x$dimensionality$suggested_factors %||% NA_integer_, integer(1L))
    suggestion_table <- table(suggestions[is.finite(suggestions)])
    if (length(suggestion_table)) {
      plot_series[[length(plot_series) + 1L]] <- list(
        kind = "factor_stability", name = "Suggested factors across imputations",
        labels = names(suggestion_table), values = as.numeric(suggestion_table)
      )
    }
  }
  list(
    backend = "multiple_imputation", m = length(fits),
    pooling_note = pooling_note,
    summary = list(
      items = length(item_names), N = nrow(dataset$data), n_used = nrow(dataset$data),
      alpha = .rls_scale_descriptive_distribution(alphas),
      standardized_alpha = .rls_scale_descriptive_distribution(standardized),
      mean_inter_item_r = .rls_scale_descriptive_distribution(average_r),
      scale_mean = .rls_scale_descriptive_distribution(scale_means),
      scale_sd = .rls_scale_descriptive_distribution(scale_sds),
      omega_total = .rls_scale_descriptive_distribution(omega),
      reliability_status = "descriptive_by_imputation_not_Rubin_pooled",
      original_missing_percent = mean(original_missing)
    ),
    items = item_rows,
    correlations = list(method = correlation_method, status = correlation_status,
                        matrix = pooled_correlation, p_values = pooled_p_values,
                        sample_sizes = pooled_sample_sizes,
                        unavailability_reasons = correlation_reasons,
                        matrices_by_imputation = correlations),
    dimensionality = dimensionality,
    scores = score_rows,
    score_summary = .rls_scale_score_summary(
      score_rows, options$score,
      paste0(
        "Descriptive across ", length(fits), " imputations; ",
        valid_case_description, "; ", sum(valid_score_counts),
        " total score estimates; not Rubin-pooled."
      ),
      valid_n = valid_case_display,
      valid_n_by_imputation = valid_score_counts
    ),
    plot_series = plot_series,
    fits_by_imputation = fits,
    reliability_by_imputation = data.frame(
      imputation = seq_along(fits), alpha = alphas,
      standardized_alpha = standardized, omega_total = omega,
      stringsAsFactors = FALSE
    ),
    provenance = list(
      item_means = "mice::pool.scalar (Rubin's rules)",
      pearson_correlations = if (identical(correlation_method, "pearson"))
        "Fisher z + mice::pool.scalar (Rubin's rules)" else "descriptive by imputation",
      alpha = "psych::alpha per imputation; descriptive distribution, not Rubin-pooled",
      omega = "psych::omega per imputation; descriptive distribution, not Rubin-pooled",
      dimensionality = if (identical(options$dimensionality$method, "pca"))
        "psych::principal/psych::fa.parallel per imputation; loadings not pooled"
      else "psych::fa/psych::fa.parallel per imputation; loadings not pooled"
    )
  )
}

.rls_scale_fit <- function(dataset, item_specs, options) {
  snapshot <- dataset$analysis_scope_snapshot %||% .rls_capture_analysis_scope(dataset)
  scoped <- .rls_dataset_subset_original_rows(dataset,snapshot$rows)
  result <- .rls_scale_fit_scoped(scoped,item_specs,options)
  result$data_scope <- snapshot
  # Transport immutable coding with the computed result for shared R exports.
  result$plot_series <- c(result$plot_series, lapply(Filter(function(spec)
    identical(spec$type, "ordinal"), item_specs), function(spec) list(
      kind = "ordinal_item_levels", name = spec$name,
      labels = spec$ordinal_levels %||% character(),
      values = seq_along(spec$ordinal_levels %||% character()))))
  ids <- scoped$original_row_ids
  map_scores <- function(scores) {
    if(is.data.frame(scores) && "row" %in% names(scores)) scores$row <- ids[scores$row]
    scores
  }
  result$scores <- map_scores(result$scores)
  # Plot payloads are assembled while row numbers are local to the scope.
  # Link biplot points to the same original cases as the saved R scores.
  result$plot_series <- lapply(result$plot_series, function(series) {
    if (identical(series$kind, "factor_score_imputation"))
      series$labels <- as.character(ids[as.integer(series$labels)])
    series
  })
  if(!is.null(result$dimensionality$scores)) result$dimensionality$scores <- map_scores(result$dimensionality$scores)
  if(length(result$dimensionality$results_by_imputation)) result$dimensionality$results_by_imputation <- lapply(
    result$dimensionality$results_by_imputation,function(value) { value$scores <- map_scores(value$scores);value })
  result
}

.rls_scale_fit_scoped <- function(dataset, item_specs, options) {
  if (length(item_specs) < 2L) {
    return(list(backend = .rls_analysis_backend(dataset, "scale_analysis"),
                status = "choose_at_least_two_items", summary = list(), items = data.frame(),
                correlations = list(), dimensionality = list(), scores = data.frame(),
                provenance = list()))
  }
  if (.rls_mi_is_dataset(dataset)) return(.rls_scale_fit_mi(dataset, item_specs, options))
  fit <- .rls_scale_fit_one(dataset$data, item_specs, options)
  list(
    backend = "ordinary", status = "value",
    summary = c(list(items = length(item_specs), N = nrow(dataset$data),
                     n_used = sum(stats::complete.cases(fit$prepared)),
                     original_missing_percent = mean(is.na(fit$prepared)) * 100,
                     omega_total = fit$omega$value, omega_status = fit$omega$status),
                fit$alpha$summary),
    items = fit$alpha$items,
    correlations = fit$correlation,
    dimensionality = fit$dimensionality,
    scores = fit$scores,
    score_summary = .rls_scale_score_summary(fit$scores, options$score),
    plot_series = c(
      .rls_scale_item_distribution_series(list(fit), item_specs),
      .rls_scale_score_distribution_series(list(fit), options$score),
      .rls_scale_scree_series(list(fit$dimensionality), FALSE),
      .rls_scale_dimensionality_series(list(fit$dimensionality))
    ),
    reliability_by_imputation = data.frame(),
    provenance = list(
      reliability = "psych::alpha",
      omega = "psych::omega",
      correlations = fit$correlation$function_name,
      dimensionality = if (identical(options$dimensionality$method, "pca"))
        "psych::principal" else "psych::fa",
      parallel_analysis = "psych::fa.parallel"
    )
  )
}

.rls_scale_default_options <- function(correlation_method = "auto", score = "mean",
                                       minimum_valid_items = NULL, omega = FALSE,
                                       dimensionality = FALSE, factors = NULL,
                                       extraction = "minres", rotation = "oblimin",
                                       parallel_iterations = 20L,
                                       dimensionality_method = "factor",
                                       dimensionality_missing = "pairwise",
                                       dimensionality_scale = TRUE) {
  list(
    correlation_method = match.arg(tolower(correlation_method), c("auto", "pearson", "polychoric", "mixed")),
    score = match.arg(score, c("mean", "sum")), minimum_valid_items = minimum_valid_items,
    omega = isTRUE(omega), dimensionality = list(
      enabled = isTRUE(dimensionality), factors = factors,
      method = match.arg(tolower(dimensionality_method), c("factor", "pca")),
      missing = match.arg(tolower(dimensionality_missing), c("listwise", "pairwise")),
      scale = isTRUE(dimensionality_scale),
      extraction = match.arg(extraction, c("minres", "ml", "pa")),
      rotation = match.arg(rotation, c("oblimin", "varimax", "quartimax", "promax", "none")),
      parallel_iterations = max(1L, as.integer(parallel_iterations))
    )
  )
}

.rls_scale_number_token <- function(value) {
  if (!length(value) || !is.finite(value[[1L]])) return("")
  format(as.numeric(value[[1L]]), digits = 17L, scientific = FALSE, trim = TRUE)
}

.rls_scale_specification_fingerprint <- function(dataset_id, item_specs, options) {
  dimensionality <- options$dimensionality %||% list()
  minimum <- as.integer(options$minimum_valid_items %||% 0L)
  factors <- as.integer(dimensionality$factors %||% 0L)
  base <- paste(
    dataset_id,
    tolower(options$correlation_method %||% "auto"),
    tolower(options$score %||% "mean"),
    minimum,
    as.integer(isTRUE(options$omega)),
    as.integer(isTRUE(dimensionality$enabled)),
    tolower(dimensionality$method %||% "factor"),
    tolower(dimensionality$missing %||% "pairwise"),
    as.integer(isTRUE(dimensionality$scale %||% TRUE)),
    factors,
    tolower(dimensionality$extraction %||% "minres"),
    tolower(dimensionality$rotation %||% "oblimin"),
    as.integer(dimensionality$parallel_iterations %||% 20L),
    sep = "|"
  )
  item_tokens <- vapply(item_specs, function(spec) {
    range <- spec$scoring_range %||% NULL
    token <- paste(
      spec$name, spec$type,
      as.integer(identical(spec$direction, "reversed")),
      as.integer(!is.null(range)), sep = ":"
    )
    if (!is.null(range)) {
      token <- paste0(token, ":", .rls_scale_number_token(range[[1L]]),
                      ":", .rls_scale_number_token(range[[2L]]))
    }
    token
  }, character(1L))
  paste0(base, paste0("||", item_tokens, collapse = ""))
}

.rls_scale_attach_identity <- function(result, revision, fingerprint) {
  result$revision <- as.integer(revision)
  result$fingerprint <- as.character(fingerprint)
  result
}

#' Analyze a psychometric scale
#'
#' Reliability, item analysis, correlations, optional omega and optional
#' dimensionality are delegated to `psych`. Multiple-imputation scalar
#' estimates are pooled with `mice::pool.scalar`; reliability coefficients and
#' dimensionality remain explicitly descriptive by imputation.
#'
#' @param data A registered LinkEDA dataset name or a data frame.
#' @param items Scale item names. A fresh session remains empty when omitted.
#' @param item_types Named item types (`"numeric"` or `"ordinal"`).
#' @param reverse_items Items scored in the reverse direction.
#' @param scoring_ranges Named theoretical numeric ranges. Observed ranges are
#'   never used for reverse scoring.
#' @param correlation_method `"auto"`, `"pearson"`, `"polychoric"`, or `"mixed"`.
#' @param score `"mean"` or `"sum"`.
#' @param minimum_valid_items Minimum non-missing items required for a score.
#' @param omega Compute omega through `psych::omega`.
#' @param dimensionality Compute EFA and parallel analysis through `psych`.
#' @param factors Optional retained factor count.
#' @param dimensionality_method `"factor"` for exploratory factor analysis or
#'   `"pca"` for principal components.
#' @param dimensionality_missing `"pairwise"` or `"listwise"` handling for the
#'   dimensionality correlation/covariance matrix.
#' @param dimensionality_scale Logical. Analyze standardized item correlations;
#'   when `FALSE`, an unstandardized covariance matrix is available for numeric
#'   items with Pearson correlations.
#' @param extraction Factor extraction method.
#' @param rotation Factor rotation.
#' @param parallel_iterations Parallel-analysis replications.
#' @param name Optional analysis id.
#' @param native Open the native result session when available.
#' @return A `rlispstat_scale_analysis` handle.
#' @export
ls_new_scale_analysis <- function(data = NULL, items = NULL, item_types = NULL,
                                  reverse_items = character(), scoring_ranges = list(),
                                  correlation_method = "auto", score = "mean",
                                  minimum_valid_items = NULL, omega = FALSE,
                                  dimensionality = FALSE, factors = NULL,
                                  dimensionality_method = "factor",
                                  dimensionality_missing = "pairwise",
                                  dimensionality_scale = TRUE,
                                  extraction = "minres", rotation = "oblimin",
                                  parallel_iterations = 20L, name = NULL, native = TRUE) {
  if (is.data.frame(data)) {
    group <- .rls_register_dataset(name %||% "scale_analysis", data, activate = TRUE)
    dataset <- .rls_dataset_record(group)
    id_name <- NULL
  } else {
    dataset <- .rls_dataset_record(data)
    id_name <- name
  }
  specs <- .rls_scale_item_specifications(
    .rls_scale_specification_data(dataset), items, item_types, reverse_items,
    scoring_ranges, dataset$variable_metadata
  )
  options <- .rls_scale_default_options(
    correlation_method, score, minimum_valid_items, omega, dimensionality,
    factors, extraction, rotation, parallel_iterations,
    dimensionality_method, dimensionality_missing, dimensionality_scale
  )
  revision <- 1L
  fingerprint <- .rls_scale_specification_fingerprint(
    dataset$dataset_id %||% dataset$group, specs, options
  )
  result <- .rls_scale_attach_identity(
    .rls_scale_fit(dataset, specs, options), revision, fingerprint
  )
  record <- list(
    id = .rls_scale_id(dataset$group, id_name), group = dataset$group,
    dataset_id = dataset$dataset_id %||% dataset$group,
    item_specifications = specs, options = options,
    specification_fingerprint = fingerprint,
    result = result, model_version = revision,
    native_requested = isTRUE(native)
  )
  handle <- .rls_assign_scale(record)
  if (isTRUE(native) && isTRUE(.rls_state$process_started)) {
    try(.rls_scale_sync_native(record, open = TRUE), silent = TRUE)
  }
  invisible(handle)
}

#' @rdname ls_new_scale_analysis
#' @export
ls_scale_analysis_state <- function(analysis) .rls_scale_record(analysis)

#' @rdname ls_new_scale_analysis
#' @export
ls_scale_analysis_summary <- function(analysis) .rls_scale_record(analysis)$result$summary

#' @rdname ls_new_scale_analysis
#' @export
ls_scale_analysis_items <- function(analysis) .rls_scale_record(analysis)$result$items

#' @rdname ls_new_scale_analysis
#' @export
ls_scale_analysis_correlations <- function(analysis) .rls_scale_record(analysis)$result$correlations

#' @rdname ls_new_scale_analysis
#' @export
ls_scale_analysis_dimensionality <- function(analysis) .rls_scale_record(analysis)$result$dimensionality

#' @rdname ls_new_scale_analysis
#' @export
ls_scale_analysis_scores <- function(analysis) .rls_scale_record(analysis)$result$scores

#' @rdname ls_new_scale_analysis
#' @param components Component/factor numbers (or names) to save. By default,
#'   all retained dimensions are saved.
#' @param prefix Optional prefix for the new score-column names.
#' @param dataset Optional registered target dataset. It must be the dataset
#'   used by the Scale Analysis.
#' @export
ls_scale_analysis_save_scores <- function(analysis, components = NULL,
                                          prefix = NULL, dataset = NULL) {
  record <- .rls_scale_record(analysis)
  target <- .rls_dataset_record(dataset %||% record$group)
  if (!identical(target$group, record$group))
    stop("Save scores to the dataset used by this Scale Analysis.", call. = FALSE)
  dimensionality <- record$result$dimensionality %||% list()
  dimensions <- if (.rls_mi_is_dataset(target)) {
    dimensionality$results_by_imputation %||% list()
  } else list(dimensionality)
  availability <- .rls_scale_score_availability(dimensions)
  if (length(availability$failed)) {
    if (availability$total > 1L) {
      indices <- paste(head(availability$failed, 12L), collapse = ", ")
      if (length(availability$failed) > 12L) indices <- paste0(indices, ", ...")
      stop(sprintf(paste0(
        "Cannot save scores across %d imputations: only %d have valid scores. ",
        "Unavailable imputations: %s. First failure (Imputation %d): %s ",
        "The visible loading table belongs to one imputation; no score columns were added."
      ), availability$total, availability$available, indices,
      availability$failed[[1L]], availability$first_reason), call. = FALSE)
    }
    stop(availability$first_reason, call. = FALSE)
  }
  raw_scores <- lapply(dimensions, function(value) value$scores %||% data.frame())
  row_sets <- lapply(raw_scores, function(value) {
    as.integer(value$row %||% integer())
  })
  score_sets <- lapply(raw_scores, function(value) {
    value[, setdiff(names(value), "row"), drop = FALSE]
  })
  method <- record$options$dimensionality$method %||% "factor"
  if (is.character(components) && length(score_sets)) {
    components <- match(components, names(score_sets[[1L]]))
  }
  if (!is.null(prefix)) prefix <- .rls_validate_protocol_name(prefix, "prefix")
  new_names <- .rls_save_dimension_score_sets(
    target, score_sets, row_sets, method, components,
    paste("scale_analysis", record$id, method, prefix %||% "", sep = ":"),
    prefix,
    provenance_code = paste0(
      "LinkEDA::ls_scale_analysis_save_scores(analysis = ",
      .rls_r_string_literal(record$id), ", components = ",
      if (is.null(components)) "NULL" else
        .rls_r_character_vector(as.character(components)), ", prefix = ",
      if (is.null(prefix)) "NULL" else .rls_r_string_literal(prefix),
      ", dataset = ", .rls_r_string_literal(target$group), ")"
    ),
    input_columns = vapply(record$item_specifications, `[[`, character(1L), "name"),
    source_scope = record$result$data_scope
  )
  invisible(new_names)
}

#' @rdname ls_new_scale_analysis
#' @export
ls_scale_analysis_set_items <- function(analysis, items, item_types = NULL,
                                        reverse_items = character(), scoring_ranges = list()) {
  record <- .rls_scale_record(analysis)
  dataset <- .rls_dataset_record(record$group)
  record$item_specifications <- .rls_scale_item_specifications(
    .rls_scale_specification_data(dataset), items, item_types, reverse_items,
    scoring_ranges, dataset$variable_metadata
  )
  record$model_version <- record$model_version + 1L
  record$specification_fingerprint <- .rls_scale_specification_fingerprint(
    record$dataset_id, record$item_specifications, record$options
  )
  record$result <- .rls_scale_attach_identity(
    .rls_scale_fit(dataset, record$item_specifications, record$options),
    record$model_version, record$specification_fingerprint
  )
  handle <- .rls_assign_scale(record)
  if (isTRUE(record$native_requested) && isTRUE(.rls_state$process_started)) {
    try(.rls_scale_sync_native(record, open = FALSE), silent = TRUE)
  }
  invisible(handle)
}

#' @rdname ls_new_scale_analysis
#' @export
ls_scale_analysis_set_options <- function(
    analysis, correlation_method = NULL, score = NULL,
    minimum_valid_items = NULL, omega = NULL, dimensionality = NULL,
    factors = NULL, dimensionality_method = NULL,
    dimensionality_missing = NULL, dimensionality_scale = NULL,
    extraction = NULL, rotation = NULL,
    parallel_iterations = NULL) {
  record <- .rls_scale_record(analysis)
  current <- record$options
  dim_current <- current$dimensionality %||% list()
  record$options <- .rls_scale_default_options(
    correlation_method = correlation_method %||% current$correlation_method,
    score = score %||% current$score,
    minimum_valid_items = minimum_valid_items %||% current$minimum_valid_items,
    omega = omega %||% current$omega,
    dimensionality = dimensionality %||% dim_current$enabled,
    factors = factors %||% dim_current$factors,
    extraction = extraction %||% dim_current$extraction,
    rotation = rotation %||% dim_current$rotation,
    parallel_iterations = parallel_iterations %||% dim_current$parallel_iterations,
    dimensionality_method = dimensionality_method %||% dim_current$method %||% "factor",
    dimensionality_missing = dimensionality_missing %||% dim_current$missing %||% "pairwise",
    dimensionality_scale = dimensionality_scale %||% dim_current$scale %||% TRUE
  )
  record$model_version <- record$model_version + 1L
  record$specification_fingerprint <- .rls_scale_specification_fingerprint(
    record$dataset_id, record$item_specifications, record$options
  )
  dataset <- .rls_dataset_record(record$group)
  record$result <- .rls_scale_attach_identity(
    .rls_scale_fit(dataset, record$item_specifications, record$options),
    record$model_version, record$specification_fingerprint
  )
  handle <- .rls_assign_scale(record)
  if (isTRUE(record$native_requested) && isTRUE(.rls_state$process_started)) {
    try(.rls_scale_sync_native(record, open = FALSE), silent = TRUE)
  }
  invisible(handle)
}

.rls_handle_scale_analysis_needed <- function(parts) {
  if (length(parts) < 20L) return(invisible(FALSE))
  cursor <- 2L
  take <- function() {
    if (cursor > length(parts)) stop("The Scale Analysis request is malformed.", call. = FALSE)
    value <- parts[[cursor]]
    cursor <<- cursor + 1L
    value
  }
  id <- .rls_validate_protocol_name(take(), "analysis")
  group <- .rls_validate_protocol_name(take(), "dataset")
  revision <- suppressWarnings(as.integer(take()))
  fingerprint <- take()
  correlation_method <- take()
  score <- take()
  minimum_valid_items <- suppressWarnings(as.integer(take()))
  omega <- identical(take(), "TRUE")
  dimensionality <- identical(take(), "TRUE")
  dimensionality_method <- take()
  dimensionality_missing <- take()
  dimensionality_scale <- identical(take(), "TRUE")
  factors <- suppressWarnings(as.integer(take()))
  extraction <- take()
  rotation <- take()
  parallel_iterations <- suppressWarnings(as.integer(take()))
  scope <- take()
  scope_row_count <- suppressWarnings(as.integer(take()))
  if (!scope %in% c("all", "selected") || !is.finite(scope_row_count) ||
      scope_row_count < 0L || length(parts) < cursor + scope_row_count) {
    stop("The Scale Analysis request has an invalid analysis scope.", call. = FALSE)
  }
  scope_rows <- if (scope_row_count > 0L) {
    rows <- suppressWarnings(as.integer(parts[cursor:(cursor + scope_row_count - 1L)]))
    cursor <- cursor + scope_row_count
    unique(rows[is.finite(rows) & rows > 0L])
  } else integer()
  item_count <- suppressWarnings(as.integer(take()))
  if (!is.finite(revision) || revision < 0L || !nzchar(fingerprint) ||
      !is.finite(item_count) || item_count < 0L ||
      length(parts) < cursor + 6L * item_count - 1L) {
    stop("The Scale Analysis request has an invalid revision or item payload.", call. = FALSE)
  }
  item_names <- character(item_count)
  item_types <- setNames(vector("list", item_count), character(item_count))
  reverse_items <- character()
  scoring_ranges <- list()
  for (index in seq_len(item_count)) {
    variable <- take()
    type <- take()
    reversed <- identical(take(), "TRUE")
    has_range <- identical(take(), "TRUE")
    minimum <- take()
    maximum <- take()
    item_names[[index]] <- variable
    names(item_types)[[index]] <- variable
    item_types[[index]] <- type
    if (reversed) reverse_items <- c(reverse_items, variable)
    if (has_range) {
      range <- suppressWarnings(as.numeric(c(minimum, maximum)))
      if (length(range) != 2L || any(!is.finite(range))) {
        stop(sprintf("The scoring range for `%s` is invalid.", variable), call. = FALSE)
      }
      scoring_ranges[[variable]] <- range
    }
  }
  dataset <- .rls_dataset_record(group)
  dataset$analysis_scope_snapshot <- .rls_capture_analysis_scope(dataset,scope,scope_rows)
  # Freeze item coding from the full dataset; .rls_scale_fit applies the scope.
  options <- .rls_scale_default_options(
    correlation_method, score,
    if (minimum_valid_items > 0L) minimum_valid_items else NULL,
    omega, dimensionality,
    if (factors > 0L) factors else NULL,
    extraction, rotation, parallel_iterations,
    dimensionality_method, dimensionality_missing, dimensionality_scale
  )
  specs <- tryCatch(.rls_scale_item_specifications(
    .rls_scale_specification_data(dataset), item_names, item_types,
    reverse_items, scoring_ranges,
    dataset$variable_metadata
  ), error = identity)
  if (inherits(specs, "error")) {
    specification_error <- conditionMessage(specs)
    specs <- lapply(seq_along(item_names), function(index) list(
      name = item_names[[index]], type = as.character(item_types[[index]]),
      direction = if (item_names[[index]] %in% reverse_items) "reversed" else "forward",
      scoring_range = scoring_ranges[[item_names[[index]]]] %||% NULL
    ))
    result <- list(
      backend = .rls_analysis_backend(dataset, "scale_analysis"),
      status = paste0("Scale Analysis error: ", specification_error),
      summary = list(), items = data.frame(), correlations = list(),
      dimensionality = list(), scores = data.frame(), score_summary = list(),
      reliability_by_imputation = data.frame(), provenance = list()
    )
  } else {
    result <- tryCatch(
      .rls_scale_fit(dataset, specs, options),
      error = function(error) list(
        backend = .rls_analysis_backend(dataset, "scale_analysis"),
        status = paste0("Scale Analysis error: ", conditionMessage(error)),
        summary = list(), items = data.frame(), correlations = list(),
        dimensionality = list(), scores = data.frame(), score_summary = list(),
        reliability_by_imputation = data.frame(), provenance = list()
      )
    )
  }
  result <- .rls_scale_attach_identity(result, revision, fingerprint)
  record <- list(
    id = id, group = group, dataset_id = dataset$dataset_id %||% group,
    scope = scope, selected_rows = scope_rows,
    item_specifications = specs, options = options,
    specification_fingerprint = fingerprint,
    result = result, model_version = revision, native_requested = TRUE
  )
  .rls_assign_scale(record)
  .rls_scale_sync_native(record, open = FALSE)
  invisible(TRUE)
}

.rls_scale_wire <- function(value) .rls_native_wire_value(value)

.rls_scale_sync_native <- function(record, open = FALSE) {
  result <- record$result
  summary <- result$summary %||% list()
  status <- result$status %||% "value"
  summary_text <- if (!identical(status, "value")) {
    status
  } else if (identical(result$backend, "multiple_imputation")) {
    sprintf("m = %d; alpha by imputation mean %.4f (range %.4f to %.4f); not Rubin-pooled",
            result$m, summary$alpha$mean, summary$alpha$min, summary$alpha$max)
  } else {
    sprintf("alpha = %.4f; standardized alpha = %.4f; mean inter-item r = %.4f",
            summary$alpha, summary$standardized_alpha, summary$mean_inter_item_r)
  }
  lines <- c(
    if (open) "SCALE_ANALYSIS_OPEN" else "SCALE_ANALYSIS_UPDATE",
    .rls_scale_wire(record$id), .rls_scale_wire(record$group),
    .rls_scale_wire(result$backend), as.character(result$m %||% 1L),
    as.character(result$revision %||% record$model_version),
    .rls_scale_wire(result$fingerprint %||% record$specification_fingerprint),
    .rls_scale_wire(summary_text),
    as.character(length(record$item_specifications))
  )
  for (spec in record$item_specifications) {
    row <- if (is.data.frame(result$items) && "item" %in% names(result$items)) {
      result$items[result$items$item == spec$name, , drop = FALSE]
    } else data.frame()
    value <- function(name) if (nrow(row) && name %in% names(row)) row[[name]][[1L]] else NA_real_
    range <- spec$scoring_range %||% NULL
    lines <- c(lines, .rls_scale_wire(spec$name), .rls_scale_wire(spec$type),
               .rls_scale_wire(spec$direction), if (is.null(range)) "FALSE" else "TRUE",
               if (is.null(range)) "" else .rls_scale_wire(.rls_scale_number_token(range[[1L]])),
               if (is.null(range)) "" else .rls_scale_wire(.rls_scale_number_token(range[[2L]])),
               .rls_scale_wire(.rls_export_format_number(value("mean"), 3L)),
               .rls_scale_wire(.rls_export_format_number(value("sd"), 3L)),
               .rls_scale_wire(.rls_export_format_number(value("missing_percent"), 1L)),
               .rls_scale_wire(.rls_export_format_number(value("item_rest_r"), 3L)),
               .rls_scale_wire(.rls_export_format_number(value("alpha_if_deleted"), 3L)))
  }
  provenance <- paste(unlist(result$provenance %||% list()), collapse = "; ")
  if (nzchar(result$pooling_note %||% "")) provenance <- paste(provenance, result$pooling_note)
  lines <- c(lines, .rls_scale_wire(provenance))

  format_value <- function(value, digits = 3L) {
    if (!length(value) || !is.finite(as.numeric(value[[1L]]))) return("")
    .rls_export_format_number(as.numeric(value[[1L]]), digits)
  }
  format_distribution <- function(value, digits = 3L) {
    if (!is.list(value)) return(format_value(value, digits))
    if (!is.finite(value$mean %||% NA_real_)) return("")
    paste0(format_value(value$mean, digits), " (",
           format_value(value$min, digits), "\u2013", format_value(value$max, digits), ")")
  }
  is_mi <- identical(result$backend, "multiple_imputation")
  summary_fields <- c(
    as.character(summary$items %||% length(record$item_specifications)),
    as.character(summary$n_used %||% summary$N %||% ""),
    format_value(summary$original_missing_percent, 1L),
    if (is_mi) format_distribution(summary$scale_mean) else format_value(summary$scale_mean),
    if (is_mi) format_distribution(summary$scale_sd) else format_value(summary$scale_sd),
    if (is_mi) format_distribution(summary$mean_inter_item_r) else format_value(summary$mean_inter_item_r),
    if (is_mi) format_distribution(summary$alpha) else format_value(summary$alpha),
    if (is_mi) format_distribution(summary$standardized_alpha) else format_value(summary$standardized_alpha),
    if (is_mi) format_distribution(summary$omega_total) else format_value(summary$omega_total),
    as.character(summary$reliability_status %||% if (is_mi)
      "descriptive_by_imputation_not_Rubin_pooled" else "ordinary_data")
  )
  lines <- c(lines, "SCALE_DETAILS_V1", vapply(summary_fields, .rls_scale_wire, character(1L)))

  reliability <- result$reliability_by_imputation %||% data.frame()
  lines <- c(lines, as.character(nrow(reliability)))
  if (nrow(reliability)) for (index in seq_len(nrow(reliability))) {
    lines <- c(lines,
      as.character(reliability$imputation[[index]]),
      .rls_scale_wire(format_value(reliability$alpha[[index]])),
      .rls_scale_wire(format_value(reliability$standardized_alpha[[index]])),
      .rls_scale_wire(format_value(reliability$omega_total[[index]])))
  }

  correlations <- result$correlations %||% list()
  correlation_matrix <- correlations$matrix %||% NULL
  correlation_variables <- if (is.matrix(correlation_matrix)) {
    colnames(correlation_matrix) %||% vapply(record$item_specifications, `[[`, character(1L), "name")
  } else character()
  lines <- c(lines,
    .rls_scale_wire(correlations$method %||% ""),
    .rls_scale_wire(correlations$status %||% if (is.matrix(correlation_matrix)) "value" else "unavailable"),
    as.character(length(correlation_variables)),
    vapply(correlation_variables, .rls_scale_wire, character(1L)))
  if (length(correlation_variables)) {
    correlation_values <- as.vector(t(correlation_matrix))
    lines <- c(lines, vapply(correlation_values, format_value, character(1L)))
    correlation_p_values <- correlations$p_values
    if (!is.matrix(correlation_p_values) ||
        !identical(dim(correlation_p_values), dim(correlation_matrix))) {
      correlation_p_values <- matrix(NA_real_, nrow(correlation_matrix), ncol(correlation_matrix))
    }
    correlation_sample_sizes <- correlations$sample_sizes
    if (!is.matrix(correlation_sample_sizes) ||
        !identical(dim(correlation_sample_sizes), dim(correlation_matrix))) {
      correlation_sample_sizes <- matrix(NA_real_, nrow(correlation_matrix), ncol(correlation_matrix))
    }
    lines <- c(lines, "SCALE_CORRELATION_DETAILS_V1",
      vapply(as.vector(t(correlation_p_values)), .rls_scale_number_token, character(1L)),
      vapply(as.vector(t(correlation_sample_sizes)), .rls_scale_number_token, character(1L)))
  }

  dimensionality <- result$dimensionality %||% list()
  displayed_dimensionality <- dimensionality
  displayed_imputation <- NA_integer_
  if (is_mi && length(dimensionality$results_by_imputation %||% list())) {
    candidates <- dimensionality$results_by_imputation
    valid <- which(vapply(candidates, function(value) {
      is.data.frame(value$loadings %||% NULL) && nrow(value$loadings) > 0L
    }, logical(1L)))
    if (length(valid)) {
      displayed_imputation <- valid[[1L]]
      displayed_dimensionality <- candidates[[displayed_imputation]]
    }
  }
  loading_table <- displayed_dimensionality$loadings %||% data.frame()
  factor_names <- if (is.data.frame(loading_table)) {
    setdiff(names(loading_table), c("item", "h2", "u2"))
  } else character()
  dimension_noun <- if (identical(record$options$dimensionality$method, "pca"))
    "component" else "factor"
  suggested <- if (is_mi && length(dimensionality$suggested_factor_count %||% list())) {
    counts <- unlist(dimensionality$suggested_factor_count)
    paste(paste0(names(counts), " ", dimension_noun,
                 ifelse(names(counts) == "1", "", "s"),
                 " in ", counts, " imputation", ifelse(counts == 1, "", "s")),
          collapse = "; ")
  } else as.character(dimensionality$suggested_factors %||% "")
  dimensionality_status <- dimensionality$status %||% "not_requested"
  if (identical(dimensionality_status, "descriptive_by_imputation_loadings_not_pooled")) {
    dimensionality_status <- paste(
      paste0("Parallel analysis and ",
             if (identical(dimension_noun, "component")) "PCA" else "EFA",
             " were calculated separately in every imputation using psych."),
      if (is.finite(displayed_imputation))
        paste0("The table shows Imputation ", displayed_imputation,
               "; ", dimension_noun,
               " counts are descriptive and loading matrices are not pooled.")
      else paste0(tools::toTitleCase(dimension_noun),
                  " counts are descriptive; rotated loading matrices are not pooled.")
    )
    availability <- dimensionality$score_availability %||% list()
    if (length(availability$total) && availability$total > 0L) {
      dimensionality_status <- paste0(
        "Loadings are shown for one imputation at a time, not pooled. ",
        tools::toTitleCase(dimension_noun), " scores available in ",
        availability$available, " of ", availability$total, " imputations. ",
        if (availability$available < availability$total)
          paste0("Saving requires all imputations; first unavailable: Imputation ",
                 availability$failed[[1L]], ".")
        else "Scores can be saved for every imputation."
      )
    }
  } else if (identical(dimensionality_status, "unavailable_under_mi_unaligned_loadings")) {
    dimensionality_status <- paste(
      "Pooled EFA loadings are unavailable for multiple-imputation data because",
      "no validated factor-alignment and pooling method is being applied."
    )
  }
  lines <- c(lines,
    .rls_scale_wire(dimensionality_status),
    .rls_scale_wire(suggested),
    as.character(displayed_dimensionality$factors %||% dimensionality$factors %||% 0L),
    as.character(if (is.data.frame(loading_table)) nrow(loading_table) else 0L),
    as.character(length(factor_names)),
    vapply(factor_names, .rls_scale_wire, character(1L)))
  if (is.data.frame(loading_table) && nrow(loading_table)) {
    for (index in seq_len(nrow(loading_table))) {
      lines <- c(lines, .rls_scale_wire(as.character(loading_table$item[[index]])))
      for (factor in factor_names) {
        lines <- c(lines, .rls_scale_wire(format_value(loading_table[[factor]][[index]])))
      }
      lines <- c(lines,
        .rls_scale_wire(format_value(loading_table$h2[[index]])),
        .rls_scale_wire(format_value(loading_table$u2[[index]])))
    }
  }

  score_summary <- result$score_summary %||% list()
  lines <- c(lines,
    .rls_scale_wire(score_summary$method %||% record$options$score %||% "mean"),
    .rls_scale_wire(score_summary$status %||% "unavailable"),
    as.character(score_summary$valid_n %||% ""),
    .rls_scale_wire(format_value(score_summary$mean)),
    .rls_scale_wire(format_value(score_summary$sd)),
    .rls_scale_wire(format_value(score_summary$min)),
    .rls_scale_wire(format_value(score_summary$max)))
  plot_series <- result$plot_series %||% list()
  # Factor scores can contribute hundreds of thousands of protocol fields for
  # an MI analysis.  Appending two fields at a time with `lines <- c(lines, ...)`
  # repeatedly copies the whole accumulated vector, making serialization
  # quadratic and capable of monopolising the main R session for minutes.
  # Build each series once and concatenate the chunks once instead.
  plot_chunks <- lapply(plot_series, function(series) {
    labels <- as.character(series$labels %||% character())
    values <- as.numeric(series$values %||% numeric())
    count <- min(length(labels), length(values))
    chunk <- c(
      .rls_scale_wire(series$kind %||% ""),
      .rls_scale_wire(series$name %||% ""),
      as.character(count))
    if (count) {
      indices <- seq_len(count)
      encoded_labels <- .rls_scale_wire(labels[indices])
      encoded_values <- vapply(values[indices], format_value, character(1L), digits = 6L)
      chunk <- c(chunk, as.vector(rbind(encoded_labels, encoded_values)))
    }
    chunk
  })
  lines <- c(lines, "SCALE_PLOTS_V1", as.character(length(plot_series)),
             unlist(plot_chunks, use.names = FALSE))
  # Explicit immutable options accompany results opened from R. Never infer
  # requested settings from rounded statistics or a resolved estimator label.
  o <- record$options; d <- o$dimensionality
  lines <- c(lines, "SCALE_SPEC_V1", o$correlation_method %||% "auto", o$score %||% "mean",
    as.character(o$minimum_valid_items %||% 0L), if (isTRUE(o$omega)) "TRUE" else "FALSE",
    if (isTRUE(d$enabled)) "TRUE" else "FALSE", d$method %||% "factor",
    d$missing %||% "pairwise", if (isTRUE(d$scale)) "TRUE" else "FALSE",
    as.character(d$factors %||% 0L), d$extraction %||% "minres", d$rotation %||% "oblimin",
    as.character(d$parallel_iterations %||% 20L))
  .rls_send(lines)
  invisible(TRUE)
}

#' Save a scale sum or mean to its source dataset
#'
#' Uses the items, direction, missing-item rule and frozen scope of the analysis.
#' Each imputation is scored separately. Rows outside the scope receive NA.
#' Existing columns are never overwritten. The returned name identifies the new
#' column; the dataset's transformation history retains executable base-R code.
#' @param analysis Scale Analysis handle or identifier.
#' @param method Either "sum" or "mean".
#' @param name Optional new column name; a unique suffix is added if needed.
#' @export
ls_scale_analysis_save_total <- function(analysis, method = c("sum", "mean"), name = NULL) {
  method <- match.arg(method)
  record <- .rls_scale_record(analysis)
  target <- .rls_dataset_record(record$group)
  scope <- record$result$data_scope
  if (is.null(scope)) stop("Refit the scale before saving scores.", call. = FALSE)
  specs <- record$item_specifications
  if (!length(specs)) stop("Add items before saving scores.", call. = FALSE)
  inputs <- vapply(specs, `[[`, character(1L), "name")
  frozen <- ls_get_data_version(record$group, scope$dataset_version)
  mi <- .rls_mi_is_dataset(target)
  old_sets <- if (mi) frozen$completed_datasets else list(frozen)
  sets <- if (mi) .rls_mi_completed_datasets(target) else list(target$data)
  # Added output columns are harmless; edited input values or row identities are not.
  same_inputs <- function(a, b) all(vapply(inputs, function(v) identical(a[[v]], b[[v]]), logical(1L)))
  if (!identical(as.character(attr(frozen, "linkeda_row_ids")), as.character(target$stable_row_ids)) ||
      length(old_sets) != length(sets) || !all(mapply(same_inputs, old_sets, sets)))
    stop("The scale input data changed. Refit before saving scores.", call. = FALSE)
  positions <- match(scope$stable_row_ids, target$stable_row_ids)
  if (anyNA(positions)) stop("The scale rows no longer match the dataset.", call. = FALSE)
  original <- if (mi) target$original_data else target$data
  if (mi && !same_inputs(frozen$data, original))
    stop("The original scale input data changed. Refit before saving scores.", call. = FALSE)
  name <- .rls_validate_protocol_name(name %||% paste0("scale_", method), "name")
  occupied <- unique(c(names(target$data), if (inherits(target$mids_object, "mids")) names(target$mids_object$data)))
  name <- tail(make.unique(c(occupied, name), sep = "_"), 1L)
  minimum <- as.integer(record$options$minimum_valid_items %||% length(specs))
  minimum <- max(1L, min(length(specs), minimum))
  score_frame <- function(data) {
    prepared <- .rls_scale_prepare_items(data[positions, , drop = FALSE], specs)
    score <- .rls_scale_scores_one(prepared, specs, method, minimum)$score
    values <- rep(NA_real_, nrow(data)); values[positions] <- score
    data[[name]] <- values
    data
  }
  # Build the verification recipe from the exact coding used above, not a later UI state.
  reference <- .rls_scale_prepare_items(original[positions, , drop = FALSE], specs)
  code <- c(paste0("# Scale ", method, "; rows and item coding frozen at calculation."),
    paste0("score_rows <- ", paste(deparse(as.integer(positions)), collapse = "\n")),
    paste0("x <- data[score_rows, ", .rls_r_character_vector(inputs), ", drop = FALSE]"))
  for (spec in specs) {
    v <- .rls_r_string_literal(spec$name)
    if (spec$type == "ordinal") {
      code <- c(code, paste0("x[[", v, "]] <- match(as.character(x[[", v, "]]), ",
        .rls_r_character_vector(levels(reference[[spec$name]])), ")"))
    } else {
      encoding <- spec$numeric_factor_encoding
      if (is.list(encoding) && identical(encoding$method, "binary_zero_one"))
        code <- c(code, paste0("x[[", v, "]] <- match(as.character(x[[", v, "]]), ",
          .rls_r_character_vector(encoding$levels), ") - 1L"))
      else code <- c(code, paste0("x[[", v, "]] <- as.numeric(as.character(x[[", v, "]]))"))
      if (identical(spec$direction, "reversed"))
        code <- c(code, paste0("x[[", v, "]] <- sum(",
          paste(deparse(spec$scoring_range), collapse = ""), ") - x[[", v, "]]"))
    }
  }
  code <- paste(c(code,
    paste0("score <- ", if (method == "sum") "rowSums" else "rowMeans", "(x, na.rm = TRUE)"),
    paste0("score[rowSums(!is.na(x)) < ", minimum, "L] <- NA_real_"),
    paste0("data[[", .rls_r_string_literal(name), "]] <- NA_real_"),
    paste0("data[[", .rls_r_string_literal(name), "]][score_rows] <- score")), collapse = "\n")
  if (mi) {
    target$original_data <- score_frame(original)
    target$completed_datasets <- lapply(sets, score_frame)
    active <- max(1L, min(length(sets), as.integer(target$active_imputation_version %||% 1L)))
    target$data <- target$completed_datasets[[active]]
    target$missing_cell_mask <- as.data.frame(lapply(target$original_data, is.na), check.names = FALSE)
    target <- .rls_sync_derived_mi_columns(target, name)
  } else target$data <- score_frame(target$data)
  target$data_frame <- target$data
  target$n_columns <- ncol(target$data); target$modified <- TRUE
  target$metadata <- target$variable_metadata <- .rls_refresh_metadata_row(
    target$variable_metadata, target$data, name, "numeric")
  target <- .rls_advance_data_version(target, paste("Save scale", method, "score:", name),
    code = code, input_columns = inputs, output_columns = name,
    stable_row_ids = target$stable_row_ids[positions])
  .rls_set_dataset_record(target)
  .rls_sync_derived_imputation_record(target)
  .rls_notify_dataset_changed(target)
  invisible(name)
}
