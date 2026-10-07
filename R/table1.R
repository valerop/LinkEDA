.rls_table1_id <- function(group, name = NULL) {
  .rls_validate_protocol_name(name %||% paste0("table1_", group, "_", format(Sys.time(), "%Y%m%d%H%M%OS3")), "name")
}

.rls_table1_record <- function(table) {
  id <- if (inherits(table, "rlispstat_table1")) table$id else table
  id <- .rls_validate_protocol_name(id, "table1")
  if (!exists(id, envir = .rls_state$table1_tables, inherits = FALSE)) {
    stop("Unknown Table 1.", call. = FALSE)
  }
  get(id, envir = .rls_state$table1_tables)
}

.rls_assign_table1 <- function(record) {
  assign(record$id, record, envir = .rls_state$table1_tables)
  structure(list(id = record$id, dataset_id = record$dataset_id, group = record$group),
            class = "rlispstat_table1")
}

.rls_table1_executed_r_code <- function(variables, group, variable_types,
                                        include_missing, show_p, show_test,
                                        show_n, ordinal_as, numeric_stats,
                                        categorical_stats, ordinal_stats,
                                        multiple_imputation = FALSE,
                                        name = NULL) {
  function_source <- function(fun) paste(
    deparse(fun, width.cutoff = 500L, control = "keepInteger"),
    collapse = "\n"
  )
  qualify_internal_orchestration <- function(code) {
    code <- gsub("%||%", "%linkeda_or%", code, fixed = TRUE)
    gsub("(?<![[:alnum:]_:])\\.rls_", "LinkEDA:::.rls_", code,
         perl = TRUE)
  }
  engine <- function_source(if (isTRUE(multiple_imputation)) {
    .rls_table1_build_mi
  } else {
    .rls_table1_build
  })
  helpers <- character()
  if (isTRUE(multiple_imputation)) {
    # These local definitions make the substantive MI operations visible and
    # are the exact functions invoked by the emitted Table 1 engine.
    pool_scalar <- function_source(.rls_mi_pool_scalar)
    pool_d1 <- function_source(.rls_mi_pool_d1)
    group_test <- function_source(.rls_mi_table1_numeric_group_test)
    group_test <- gsub(".rls_mi_pool_scalar(", "table1_pool_scalar(",
                       group_test, fixed = TRUE)
    group_test <- gsub(".rls_mi_pool_d1(", "table1_pool_d1(",
                       group_test, fixed = TRUE)
    engine <- gsub(".rls_mi_pool_scalar(", "table1_pool_scalar(",
                   engine, fixed = TRUE)
    engine <- gsub(".rls_mi_table1_numeric_group_test(",
                   "table1_numeric_group_test(", engine, fixed = TRUE)
    pool_scalar <- qualify_internal_orchestration(pool_scalar)
    pool_d1 <- qualify_internal_orchestration(pool_d1)
    group_test <- qualify_internal_orchestration(group_test)
    engine <- qualify_internal_orchestration(engine)
    helpers <- c(
      paste0("table1_pool_scalar <- ", pool_scalar),
      paste0("table1_pool_d1 <- ", pool_d1),
      paste0("table1_numeric_group_test <- ", group_test)
    )
  } else {
    table_test <- function_source(.rls_table1_test)
    engine <- gsub(".rls_table1_test(", "table1_group_test(",
                   engine, fixed = TRUE)
    table_test <- qualify_internal_orchestration(table_test)
    engine <- qualify_internal_orchestration(engine)
    helpers <- paste0("table1_group_test <- ", table_test)
  }
  paste(c(
    "# Executed inside LinkEDA's R session using the public data-version APIs.",
    "# `source_data` and the scoped object below are created in sections 1-2.",
    "# LinkEDA-internal calls below are limited to type/format orchestration;",
    "# the substantive statistical operations are defined explicitly here.",
    "`%linkeda_or%` <- function(x, y) if (is.null(x)) y else x",
    paste0("variables <- ", .rls_r_character_vector(variables)),
    paste0("group_variable <- ", if (is.null(group)) "NULL" else .rls_r_string_literal(group)),
    paste0("variable_types <- ", .rls_r_named_character_vector(variable_types)),
    paste0("include_missing <- ", if (isTRUE(include_missing)) "TRUE" else "FALSE"),
    paste0("show_p <- ", if (isTRUE(show_p)) "TRUE" else "FALSE"),
    paste0("show_test <- ", if (isTRUE(show_test)) "TRUE" else "FALSE"),
    paste0("show_n <- ", if (isTRUE(show_n)) "TRUE" else "FALSE"),
    paste0("ordinal_as <- ", .rls_r_string_literal(ordinal_as)),
    paste0("numeric_stats <- ", .rls_r_character_vector(numeric_stats)),
    paste0("categorical_stats <- ", .rls_r_character_vector(categorical_stats)),
    paste0("ordinal_stats <- ", .rls_r_character_vector(ordinal_stats)),
    if (isTRUE(multiple_imputation)) c(
      "completed_sets <- LinkEDA::ls_complete_data_version(scoped_source_data)",
      "table1_dataset <- list(",
      "  dataset_type = \"multiple_imputation\",",
      "  data = scoped_source_data$data, original_data = scoped_source_data$data,",
      "  completed_datasets = completed_sets,",
      "  original_row_ids = match(LinkEDA::ls_row_ids(scoped_source_data), LinkEDA::ls_row_ids(source_data)),",
      "  variable_metadata = attr(scoped_source_data, \"linkeda_variable_metadata\", exact = TRUE),",
      "  dataset_id = attr(source_data, \"linkeda_dataset_id\", exact = TRUE),",
      "  group = attr(source_data, \"linkeda_dataset_id\", exact = TRUE),",
      "  imputation_count = length(completed_sets)",
      ")"
    ) else c(
      "table1_dataset <- list(",
      "  dataset_type = \"data_frame\", data = analysis_data,",
      "  original_row_ids = match(LinkEDA::ls_row_ids(analysis_data), LinkEDA::ls_row_ids(source_data)),",
      "  variable_metadata = attr(analysis_data, \"linkeda_variable_metadata\", exact = TRUE),",
      "  dataset_id = attr(source_data, \"linkeda_dataset_id\", exact = TRUE),",
      "  group = attr(source_data, \"linkeda_dataset_id\", exact = TRUE)",
      ")"
    ),
    helpers,
    paste0("table1_engine <- ", engine),
    "table1_result <- table1_engine(",
    "  dataset = table1_dataset, variables = variables,",
    "  group = group_variable, variable_types = variable_types,",
    "  include_missing = include_missing, show_p = show_p,",
    "  show_test = show_test, show_n = show_n, ordinal_as = ordinal_as,",
    "  numeric_stats = numeric_stats, categorical_stats = categorical_stats,",
    "  ordinal_stats = ordinal_stats,",
    paste0("  name = ", if (is.null(name)) "NULL" else .rls_r_string_literal(name)),
    ")",
    "table_values <- table1_result$display_table"
  ), collapse = "\n")
}

.rls_execute_table1_r_code <- function(code, dataset) {
  execution_environment <- new.env(parent = environment(.rls_execute_table1_r_code))
  source_data <- ls_get_data_version(
    dataset$group, version = dataset$data_version %||% 1L
  )
  scoped_data <- ls_select_rows_by_id(source_data, dataset$stable_row_ids)
  execution_environment$source_data <- source_data
  if (inherits(source_data, "linkeda_multiple_imputation_version")) {
    execution_environment$scoped_source_data <- scoped_data
  } else {
    execution_environment$analysis_data <- scoped_data
  }
  expressions <- parse(text = code, keep.source = FALSE)
  for (expression in expressions) eval(expression, envir = execution_environment)
  result <- execution_environment$table1_result
  if (!inherits(result, "rlispstat_table1_record")) {
    stop("The recorded Table 1 R code did not produce a structured result.",
         call. = FALSE)
  }
  result$statistical_r_code_executed <- enc2utf8(code)
  result
}

.rls_table1_raw <- function(x) {
  if (!is.null(attr(x, "labels", exact = TRUE))) {
    raw <- unclass(x)
    attributes(raw) <- NULL
    return(raw)
  }
  x
}

.rls_table1_value_labels <- function(x) {
  labels <- attr(x, "labels", exact = TRUE)
  if (is.null(labels) || !length(labels)) return(NULL)
  values <- unclass(labels)
  attributes(values) <- NULL
  names <- names(labels)
  if (is.null(names)) names <- as.character(values)
  if (length(names) < length(values)) names <- c(names, rep("", length(values) - length(names)))
  bad_names <- is.na(names) | !nzchar(names)
  names[bad_names] <- as.character(values[bad_names])
  stats::setNames(values, names)
}

.rls_table1_is_id_like <- function(x, name) {
  lname <- tolower(name)
  if (grepl("(^id$|_id$|^id_|identifier|subject|case|etiqueta|label)", lname)) return(TRUE)
  raw <- .rls_table1_raw(x)
  observed <- raw[!is.na(raw)]
  if (!length(observed)) return(FALSE)
  if (is.numeric(observed) || is.integer(observed)) {
    sorted <- sort(unique(observed))
    return(length(sorted) == length(observed) &&
             all(abs(sorted - round(sorted)) < .Machine$double.eps^0.5) &&
             all(diff(sorted) == 1))
  }
  length(unique(observed)) == length(observed) && length(observed) >= 0.9 * length(raw)
}

.rls_table1_type_override <- function(variable_types, variable) {
  if (is.null(variable_types)) return(NULL)
  if (is.list(variable_types) || is.character(variable_types)) {
    if (is.null(names(variable_types))) return(NULL)
    pos <- match(variable, names(variable_types))
    if (is.na(pos)) return(NULL)
    value <- variable_types[[pos]]
    if (is.null(value) || !length(value) || is.na(value[[1L]])) return(NULL)
    return(as.character(value[[1L]]))
  }
  NULL
}

.rls_table1_infer_type <- function(record, variable, variable_types = NULL,
                                   ordinal_as = c("ordinal", "categorical")) {
  override <- .rls_table1_type_override(variable_types, variable)
  if (!is.null(override)) {
    override <- match.arg(override, c("numeric", "categorical", "ordinal", "unsupported", "text"))
    if (identical(override, "text")) override <- "unsupported"
    return(override)
  }
  ordinal_as <- match.arg(ordinal_as)
  x <- record$data[[variable]]
  metadata_type <- .rls_metadata_type(record$variable_metadata, variable, .rls_variable_type(x))
  if (identical(metadata_type, "ordered")) {
    return(if (identical(ordinal_as, "ordinal")) "ordinal" else "categorical")
  }
  if (metadata_type %in% c("factor", "logical")) return("categorical")
  if (identical(metadata_type, "character")) return("unsupported")
  if (identical(metadata_type, "numeric")) {
    if (.rls_table1_is_id_like(x, variable)) return("unsupported")
    return("numeric")
  }
  "unsupported"
}

.rls_table1_validate_variables <- function(record, variables, variable_types = NULL, ordinal_as = "ordinal") {
  if (is.null(variables)) {
    return(character())
  }
  variables <- unique(vapply(variables, .rls_validate_protocol_name, character(1L), what = "variable"))
  missing <- setdiff(variables, names(record$data))
  if (length(missing)) stop(sprintf("Summary variable `%s` was not found.", missing[[1L]]), call. = FALSE)
  variables
}

.rls_table1_group_factor <- function(record, group, variable_types = NULL) {
  if (is.null(group)) return(NULL)
  group <- .rls_validate_protocol_name(group, "group")
  if (!group %in% names(record$data)) {
    stop(sprintf("The grouping variable `%s` was not found.", group), call. = FALSE)
  }
  type <- .rls_table1_infer_type(record, group, variable_types, ordinal_as = "categorical")
  x <- record$data[[group]]
  raw <- .rls_table1_raw(x)
  labels <- .rls_table1_value_labels(x)
  unique_n <- length(unique(raw[!is.na(raw)]))
  if (identical(type, "numeric") && unique_n > 10L) {
    stop(sprintf(
      "The grouping variable `%s` is numeric with many unique values. Treat it as categorical/ordinal or choose another grouping variable.",
      group
    ), call. = FALSE)
  }
  if (identical(type, "unsupported")) {
    stop(sprintf("The grouping variable `%s` is not supported for Table 1.", group), call. = FALSE)
  }
  defined_levels <- .rls_metadata_levels(record$variable_metadata, group)
  if (!is.null(labels)) {
    values <- unname(labels)
    level_names <- names(labels)
    out <- rep(NA_character_, length(raw))
    for (i in seq_along(values)) out[!is.na(raw) & raw == values[[i]]] <- level_names[[i]]
    extra <- sort(unique(as.character(raw[!is.na(raw) & is.na(out)])))
    out[!is.na(raw) & is.na(out)] <- as.character(raw[!is.na(raw) & is.na(out)])
    levels <- c(level_names, extra)
    return(factor(out, levels = unique(levels)))
  }
  if (is.factor(x)) return(droplevels(x))
  if (length(defined_levels)) {
    return(droplevels(factor(as.character(raw), levels = defined_levels)))
  }
  factor(raw)
}

.rls_table1_levels <- function(x, type, defined_levels = character()) {
  raw <- .rls_table1_raw(x)
  labels <- .rls_table1_value_labels(x)
  if (!is.null(labels)) return(names(labels))
  if (length(defined_levels)) return(as.character(defined_levels))
  if (is.factor(x)) return(levels(x))
  vals <- raw[!is.na(raw)]
  if (!length(vals)) return(character())
  if (identical(type, "ordinal") && is.numeric(vals)) return(as.character(sort(unique(vals))))
  sort(unique(as.character(vals)))
}

.rls_table1_display_factor <- function(x, type, defined_levels = character()) {
  raw <- .rls_table1_raw(x)
  labels <- .rls_table1_value_labels(x)
  if (!is.null(labels)) {
    values <- unname(labels)
    level_names <- names(labels)
    out <- rep(NA_character_, length(raw))
    for (i in seq_along(values)) out[!is.na(raw) & raw == values[[i]]] <- level_names[[i]]
    out[!is.na(raw) & is.na(out)] <- as.character(raw[!is.na(raw) & is.na(out)])
    return(factor(out, levels = unique(c(level_names, sort(unique(out[!is.na(out)]))))))
  }
  if (is.factor(x)) return(x)
  factor(as.character(raw), levels = .rls_table1_levels(x, type, defined_levels))
}

.rls_table1_numeric <- function(x) {
  raw <- suppressWarnings(as.double(.rls_table1_raw(x)))
  raw[is.finite(raw)]
}

.rls_table1_ordinal_scores <- function(x) {
  raw <- .rls_table1_raw(x)
  labels <- .rls_table1_value_labels(x)
  if (is.ordered(x)) return(as.numeric(x))
  if (!is.null(labels)) {
    out <- match(raw, unname(labels))
    return(as.numeric(out))
  }
  if (is.factor(x)) return(as.numeric(x))
  if (is.numeric(raw) || is.integer(raw)) return(as.numeric(raw))
  factor <- factor(raw, levels = .rls_table1_levels(x, "ordinal"), ordered = TRUE)
  as.numeric(factor)
}

.rls_table1_format_mean_sd <- function(x) {
  if (!length(x)) return("\u2014")
  paste0(.rls_export_format_number(mean(x), 1L), " (", .rls_export_format_number(stats::sd(x), 1L), ")")
}

.rls_table1_format_median_iqr <- function(x) {
  if (!length(x)) return("\u2014")
  q <- stats::quantile(x, probs = c(.25, .5, .75), na.rm = TRUE, names = FALSE, type = 7)
  paste0(.rls_export_format_number(q[[2L]], 1L), " [",
         .rls_export_format_number(q[[1L]], 1L), ", ",
         .rls_export_format_number(q[[3L]], 1L), "]")
}

.rls_table1_numeric_components <- function(x) {
  if (!length(x)) return(list())
  n <- length(x)
  value_mean <- mean(x)
  value_sd <- if (n > 1L) stats::sd(x) else NA_real_
  value_se <- if (n > 1L) value_sd / sqrt(n) else NA_real_
  critical <- if (n > 1L) stats::qt(.975, df = n - 1L) else NA_real_
  q <- stats::quantile(x, probs = c(.25, .5, .75), na.rm = TRUE,
                       names = FALSE, type = 7)
  list(
    mean = .rls_export_format_number(value_mean, 1L),
    sd = .rls_export_format_number(value_sd, 1L),
    se = .rls_export_format_number(value_se, 1L),
    ci95 = if (is.finite(critical) && is.finite(value_se)) paste0(
      "[", .rls_export_format_number(value_mean - critical * value_se, 1L),
      ", ", .rls_export_format_number(value_mean + critical * value_se, 1L), "]"
    ) else "\u2014",
    median = .rls_export_format_number(q[[2L]], 1L),
    q1 = .rls_export_format_number(q[[1L]], 1L),
    q3 = .rls_export_format_number(q[[3L]], 1L)
  )
}

.rls_table1_numeric_raw_components <- function(x) {
  if (!length(x)) return(list())
  n <- length(x)
  value_mean <- mean(x)
  value_sd <- if (n > 1L) stats::sd(x) else NA_real_
  value_se <- if (n > 1L) value_sd / sqrt(n) else NA_real_
  critical <- if (n > 1L) stats::qt(.975, df = n - 1L) else NA_real_
  q <- stats::quantile(x, probs = c(.25, .5, .75), na.rm = TRUE,
                       names = FALSE, type = 7)
  list(
    mean = value_mean, sd = value_sd, se = value_se,
    ci_lower = if (is.finite(critical) && is.finite(value_se)) value_mean - critical * value_se else NA_real_,
    ci_upper = if (is.finite(critical) && is.finite(value_se)) value_mean + critical * value_se else NA_real_,
    median = q[[2L]], q1 = q[[1L]], q3 = q[[3L]]
  )
}

.rls_table1_count_percent <- function(x, level) {
  non_missing <- !is.na(x)
  denominator <- sum(non_missing)
  n <- sum(non_missing & as.character(x) == level)
  pct <- if (denominator > 0) n / denominator else NA_real_
  sprintf("%d (%s)", n, .rls_export_format_percent(pct))
}

.rls_table1_group_column_names <- function(group_levels) {
  # Keep statistical column identifiers disjoint from the stub, tests and total.
  reserved <- c("Variable", "p", "Test", "Overall")
  labels <- as.character(group_levels)
  labels[labels %in% reserved] <- paste0(labels[labels %in% reserved], " (group)")
  make.unique(c(reserved, labels))[-seq_len(3L)]
}

.rls_table1_group_slices <- function(n, group_factor = NULL) {
  if (is.null(group_factor)) return(list(Overall = seq_len(n)))
  levels <- levels(droplevels(group_factor))
  stats::setNames(c(list(seq_len(n)), lapply(levels, function(level)
    which(!is.na(group_factor) & group_factor == level))), .rls_table1_group_column_names(levels))
}

.rls_table1_test <- function(data, variable, type, group_factor, original_row_ids,
                             simulation_seed = 104729L, simulation_replicates = 2000L) {
  if (is.null(group_factor)) return(NULL)
  x <- data[[variable]]
  complete <- !is.na(.rls_table1_raw(x)) & !is.na(group_factor)
  rows <- which(complete)
  used_ids <- original_row_ids[rows]
  excluded_ids <- original_row_ids[setdiff(seq_along(group_factor), rows)]
  gf <- droplevels(group_factor[rows])
  if (length(levels(gf)) < 2L || length(rows) < 3L) {
    return(list(p = NA_real_, test = "\u2014", statistic = NA_real_, parameter = NA_real_,
                rows_used_original_ids = used_ids, rows_excluded_original_ids = excluded_ids,
                status = "insufficient_data", detail = "Too few complete observations or group categories."))
  }
  result <- tryCatch({
    if (identical(type, "numeric")) {
      y <- suppressWarnings(as.double(.rls_table1_raw(x)[rows]))
      if (length(levels(gf)) == 2L) {
        fit <- stats::t.test(y ~ gf, var.equal = FALSE)
        list(p = unname(fit$p.value), test = "Welch t", statistic = unname(fit$statistic),
             parameter = unname(fit$parameter), detail = sprintf("t = %.3f; df = %.1f", fit$statistic, fit$parameter))
      } else {
        fit <- stats::oneway.test(y ~ gf, var.equal = FALSE)
        list(p = unname(fit$p.value), test = "Welch ANOVA", statistic = unname(fit$statistic),
             parameter = unname(fit$parameter[[1L]]), detail = sprintf("F = %.3f; df1 = %.0f; df2 = %.1f", fit$statistic, fit$parameter[[1L]], fit$parameter[[2L]]))
      }
    } else if (identical(type, "ordinal")) {
      score <- .rls_table1_ordinal_scores(x)[rows]
      if (length(levels(gf)) == 2L) {
        fit <- stats::wilcox.test(score ~ gf, exact = FALSE)
        list(p = unname(fit$p.value), test = "Wilcoxon rank-sum", statistic = unname(fit$statistic),
             parameter = NA_real_, detail = sprintf("W = %.3f", fit$statistic))
      } else {
        fit <- stats::kruskal.test(score ~ gf)
        list(p = unname(fit$p.value), test = "Kruskal-Wallis", statistic = unname(fit$statistic),
             parameter = unname(fit$parameter), detail = sprintf("chi-square = %.3f; df = %.0f", fit$statistic, fit$parameter))
      }
    } else {
      xf <- droplevels(.rls_table1_display_factor(x, "categorical")[rows])
      tab <- table(xf, gf)
      if (any(dim(tab) < 2L)) stop("Too few categories for association test.")
      chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
      sparse <- any(chi$expected < 5)
      if (all(dim(tab) == c(2L, 2L)) && sparse) {
        fit <- stats::fisher.test(tab)
        list(p = unname(fit$p.value), test = "Fisher exact", statistic = NA_real_,
             parameter = NA_real_, detail = "Fisher exact test")
      } else if (sparse && prod(dim(tab)) > 16L) {
        fit <- withr::with_seed(simulation_seed,
          suppressWarnings(stats::chisq.test(tab, simulate.p.value = TRUE, B = simulation_replicates)),
          .rng_kind = "Mersenne-Twister", .rng_normal_kind = "Inversion", .rng_sample_kind = "Rejection")
        list(p = unname(fit$p.value), test = "\u03c7\u00b2 simulated", statistic = unname(fit$statistic),
             parameter = NA_real_, simulation_seed = simulation_seed, simulation_replicates = simulation_replicates,
             detail = sprintf("chi-square = %.3f; simulated p-value (%d replicates; seed %d)", fit$statistic, simulation_replicates, simulation_seed))
      } else if (sparse) {
        fit <- tryCatch(stats::fisher.test(tab), error = function(e) NULL)
        if (!is.null(fit)) {
          list(p = unname(fit$p.value), test = "Fisher exact", statistic = NA_real_,
               parameter = NA_real_, detail = "Fisher exact test")
        } else {
          fit <- withr::with_seed(simulation_seed,
          suppressWarnings(stats::chisq.test(tab, simulate.p.value = TRUE, B = simulation_replicates)),
          .rng_kind = "Mersenne-Twister", .rng_normal_kind = "Inversion", .rng_sample_kind = "Rejection")
          list(p = unname(fit$p.value), test = "\u03c7\u00b2 simulated", statistic = unname(fit$statistic),
               parameter = NA_real_, simulation_seed = simulation_seed, simulation_replicates = simulation_replicates,
             detail = sprintf("chi-square = %.3f; simulated p-value (%d replicates; seed %d)", fit$statistic, simulation_replicates, simulation_seed))
        }
      } else {
        list(p = unname(chi$p.value), test = "\u03c7\u00b2", statistic = unname(chi$statistic),
             parameter = unname(chi$parameter), detail = sprintf("chi-square = %.3f; df = %.0f", chi$statistic, chi$parameter))
      }
    }
  }, error = function(e) {
    list(p = NA_real_, test = "\u2014", statistic = NA_real_, parameter = NA_real_,
         detail = conditionMessage(e), status = "error")
  })
  result$rows_used_original_ids <- used_ids
  result$rows_excluded_original_ids <- excluded_ids
  result$status <- result$status %||% "ok"
  result
}

.rls_table1_build <- function(dataset, variables, group, variable_types, include_missing,
                              show_p, show_test, show_n, ordinal_as,
                              numeric_stats, categorical_stats, ordinal_stats,
                              name = NULL) {
  data <- dataset$data
  group_factor <- .rls_table1_group_factor(dataset, group, variable_types)
  slices <- .rls_table1_group_slices(nrow(data), group_factor)
  column_names <- names(slices)
  original_ids <- dataset$original_row_ids %||% seq_len(nrow(data))
  display <- data.frame(Variable = character(), stringsAsFactors = FALSE)
  for (column in column_names) display[[column]] <- character()
  if (!is.null(group_factor) && isTRUE(show_p)) display[["p"]] <- character()
  if (!is.null(group_factor) && isTRUE(show_test)) display[["Test"]] <- character()
  rows <- list()
  missingness <- list()
  rows_used_by_variable <- list()
  rows_used_by_test <- list()
  test_results <- list()
  add_row <- function(label, values, p = "", test = "", row_type = "summary", variable = "", level = "", detail = "",
                      statistics = NULL, raw_statistics = NULL) {
    row <- as.list(c(Variable = label, values))
    if (!is.null(group_factor) && isTRUE(show_p)) row[["p"]] <- p
    if (!is.null(group_factor) && isTRUE(show_test)) row[["Test"]] <- test
    new_row <- as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
    display <<- rbind(display, new_row[names(display)])
    rows[[length(rows) + 1L]] <<- list(
      row_index = nrow(display), variable = variable, level = level,
      row_type = row_type, values = values, p = p, test = test, detail = detail,
      statistics = statistics, raw_statistics = raw_statistics
    )
  }
  if (isTRUE(show_n)) {
    n_values <- vapply(slices, function(idx) as.character(length(idx)), character(1L))
    add_row("N", n_values, row_type = "n", detail = "Number of rows in each column.")
  }
  for (variable in variables) {
    type <- .rls_table1_infer_type(dataset, variable, variable_types, ordinal_as)
    if (identical(type, "unsupported")) {
      stop(sprintf("Variable `%s` is unsupported for Table 1.", variable), call. = FALSE)
    }
    x <- data[[variable]]
    raw <- .rls_table1_raw(x)
    complete_var <- which(!is.na(raw))
    rows_used_by_variable[[variable]] <- original_ids[complete_var]
    missingness[[variable]] <- list(
      total_n = length(raw),
      non_missing_n = length(complete_var),
      missing_n = sum(is.na(raw)),
      missing_percent = mean(is.na(raw)),
      by_group = vapply(slices, function(idx) sum(is.na(raw[idx])), integer(1L))
    )
    test <- if(length(complete_var) && (show_p || show_test))
      .rls_table1_test(data, variable, type, group_factor, original_ids) else NULL
    if (!is.null(test)) rows_used_by_test[[variable]] <- test$rows_used_original_ids
    test_results[[variable]] <- test
    p_text <- if (!is.null(test)) .rls_export_format_p(test$p) else ""
    test_text <- if (!is.null(test)) test$test else ""
    if (identical(type, "numeric")) {
      numeric_values <- lapply(slices, function(idx) .rls_table1_numeric(x[idx]))
      statistics <- lapply(numeric_values, .rls_table1_numeric_components)
      raw_statistics <- lapply(numeric_values, .rls_table1_numeric_raw_components)
      values <- vapply(numeric_values, .rls_table1_format_mean_sd, character(1L))
      add_row(variable, values, p_text, test_text, "numeric_mean_sd", variable,
              detail = paste(variable, "mean/SD; p uses", test_text), statistics = statistics,
              raw_statistics = raw_statistics)
      if ("median_iqr" %in% numeric_stats) {
        values <- vapply(numeric_values, .rls_table1_format_median_iqr, character(1L))
        add_row("  Median [Q1, Q3]", values, row_type = "numeric_median_iqr", variable = variable,
                statistics = statistics, raw_statistics = raw_statistics)
      }
    } else {
      defined_levels <- .rls_metadata_levels(dataset$variable_metadata, variable)
      xf <- .rls_table1_display_factor(x, type, defined_levels)
      add_row(variable, stats::setNames(rep("", length(column_names)), column_names),
              p_text, test_text, if (identical(type, "ordinal")) "ordinal_parent" else "categorical_parent", variable)
      for (level in .rls_table1_levels(x, type, defined_levels)) {
        category_statistics <- lapply(slices, function(idx) {
          observed <- xf[idx]
          denominator <- sum(!is.na(observed))
          count <- sum(observed == level, na.rm = TRUE)
          list(n = as.integer(count), percent = if (denominator > 0L) count / denominator else NA_real_)
        })
        values <- vapply(slices, function(idx) .rls_table1_count_percent(xf[idx], level), character(1L))
        add_row(paste0("  ", level), values, row_type = paste0(type, "_level"),
                variable = variable, level = level,
                detail = paste(variable, "=", level, "n (%)"),
                statistics = category_statistics, raw_statistics = category_statistics)
      }
      if (identical(type, "ordinal") && "median_iqr" %in% ordinal_stats) {
        scores <- .rls_table1_ordinal_scores(x)
        values <- vapply(slices, function(idx) .rls_table1_format_median_iqr(scores[idx][!is.na(scores[idx])]), character(1L))
        statistics <- lapply(slices, function(idx) .rls_table1_numeric_components(scores[idx]))
        raw_statistics <- lapply(slices, function(idx) .rls_table1_numeric_raw_components(scores[idx]))
        add_row("  Median [Q1, Q3]", values, row_type = "ordinal_median_iqr", variable = variable,
                statistics = statistics, raw_statistics = raw_statistics)
      }
    }
    if (isTRUE(include_missing)) {
      missing_statistics <- lapply(slices, function(idx) {
        n <- sum(is.na(raw[idx])); denominator <- length(idx)
        list(n = as.integer(n), percent = if (denominator > 0L) n / denominator else NA_real_)
      })
      values <- vapply(slices, function(idx) {
        n <- sum(is.na(raw[idx]))
        denom <- length(idx)
        sprintf("%d (%s)", n, .rls_export_format_percent(if (denom > 0) n / denom else NA_real_))
      }, character(1L))
      add_row("  Missing", values, row_type = "missing", variable = variable,
              detail = paste(variable, "missing values"), statistics = missing_statistics,
              raw_statistics = missing_statistics)
    }
  }
  note <- c(
    "Numeric variables are shown as mean (SD) and median [Q1, Q3].",
    if(any(vapply(variables,function(v) all(is.na(data[[v]])),logical(1))))
      "Some variables have no observed values in this pattern or subset; their descriptive estimates are unavailable.",
    "Categorical variables are shown as n (%). Percentages exclude missing values.",
    "Ordinal variables are shown as ordered n (%) and median [Q1, Q3]."
  )
  if (!is.null(group_factor)) {
    note <- c(note,
              "Numeric group comparisons use Welch t-tests or Welch one-way ANOVA.",
              "Ordinal group comparisons use Wilcoxon rank-sum or Kruskal-Wallis tests.",
              "Categorical group comparisons use chi-square or Fisher exact tests when appropriate.")
    failed_tests <- names(Filter(function(test) !is.null(test) && identical(test$status, "error"),
                                 test_results))
    if (length(failed_tests)) {
      note <- c(note, vapply(failed_tests, function(variable) {
        sprintf("%s: comparison not calculated (%s).", variable,
                test_results[[variable]]$detail %||% "insufficient data")
      }, character(1L)))
    }
  }
  mi_note <- NULL
  title <- if (is.null(group)) "Table 1. Descriptive statistics" else paste("Table 1. Descriptive statistics by", group)
  structure(list(
    id = .rls_table1_id(dataset$group, name),
    table1_id = NULL,
    title = title,
    dataset_id = dataset$group,
    dataset_name = dataset$name %||% dataset$group,
    group = dataset$group,
    data = data,
    variables = variables,
    group_variable = group,
    variable_types = stats::setNames(vapply(variables, function(variable) .rls_table1_infer_type(dataset, variable, variable_types, ordinal_as), character(1L)), variables),
    summary_statistics = rows,
    test_results = test_results,
    missingness = missingness,
    rows_used_by_variable = rows_used_by_variable,
    rows_used_by_test = rows_used_by_test,
    display_options = list(include_missing = include_missing, show_p = show_p, show_test = show_test,
                           show_n = show_n, ordinal_as = ordinal_as, numeric_stats = numeric_stats,
                           categorical_stats = categorical_stats, ordinal_stats = ordinal_stats),
    display_table = display,
    footnotes = note,
    imputation_note = mi_note,
    analysis_backend = "ordinary",
    result_version = 1L,
    created_at = Sys.time()
  ), class = "rlispstat_table1_record")
}

.rls_render_table1_text <- function(record) {
  paste(c(record$title %||% if (is.null(record$group_variable)) "Table 1. Descriptive statistics" else paste("Table 1. Descriptive statistics by", record$group_variable),
          "",
          .rls_render_data_frame_text(record$display_table),
          "",
          record$footnotes), collapse = "\n")
}

.rls_render_table1_markdown <- function(record) {
  paste(c(paste0("### ", record$title %||% if (is.null(record$group_variable)) "Table 1. Descriptive statistics" else paste("Table 1. Descriptive statistics by", record$group_variable)),
          "",
          .rls_render_data_frame_markdown(record$display_table),
          "",
          record$footnotes), collapse = "\n")
}

.rls_table1_csv <- function(record) {
  record$display_table
}

.rls_table1_wire_value <- function(value) {
  .rls_native_wire_value(value)
}

.rls_table1_native_payload <- function(record) {
  display <- record$display_table
  statistic_columns <- setdiff(names(display), c("Variable", "p", "Test"))
  rows <- record$summary_statistics
  variable_types <- record$variable_types
  lines <- c(
    "TABLE1_OPEN_STRUCTURED",
    .rls_table1_wire_value(record$id),
    .rls_table1_wire_value(record$dataset_id),
    if (!is.null(record$native_request_revision)) c("REQUEST_V1",
      as.character(record$native_request_revision), as.character(record$native_data_version)),
    .rls_table1_wire_value(record$title %||% "Table 1"),
    .rls_table1_wire_value(record$group_variable %||% ""),
    as.character(length(record$variables)),
    .rls_table1_wire_value(record$variables),
    as.character(length(variable_types))
  )
  for (name in names(variable_types)) {
    lines <- c(lines, .rls_table1_wire_value(name), .rls_table1_wire_value(variable_types[[name]]))
  }
  lines <- c(lines, as.character(length(statistic_columns)), .rls_table1_wire_value(statistic_columns))
  lines <- c(lines, as.character(length(rows)))
  for (row in rows) {
    values <- row$values
    values <- unname(vapply(statistic_columns, function(column) values[[column]] %||% "", character(1L)))
    lines <- c(
      lines,
      .rls_table1_wire_value(row$row_index),
      .rls_table1_wire_value(row$row_type),
      .rls_table1_wire_value(row$variable),
      .rls_table1_wire_value(row$level),
      .rls_table1_wire_value(display$Variable[[row$row_index]] %||% ""),
      .rls_table1_wire_value(row$p),
      .rls_table1_wire_value(row$test),
      .rls_table1_wire_value(row$detail),
      as.character(length(values)),
      .rls_table1_wire_value(values)
    )
  }
  lines <- c(lines, as.character(length(record$footnotes)), .rls_table1_wire_value(record$footnotes))
  data_scope <- record$data_scope %||% list(kind = "all", description = "All observations",
                                             total_n = nrow(record$data), rows = integer())
  scope_rows <- as.integer(data_scope$rows %||% integer())
  lines <- c(lines,
             .rls_table1_wire_value(data_scope$kind %||% "all"),
             .rls_table1_wire_value(data_scope$description %||% "All observations"),
             as.character(as.integer(data_scope$total_n %||% nrow(record$data))),
             as.character(length(scope_rows)), as.character(scope_rows),
             if (isTRUE(record$display_options$show_p)) "TRUE" else "FALSE",
             if (isTRUE(record$display_options$show_test)) "TRUE" else "FALSE")
  # Versioned semantic suffix. Older native clients safely ignore it. Newer
  # clients retain the independently formatted components so display-only
  # choices never trigger a new analysis or MI pooling pass.
  lines <- c(lines, "TABLE1_DISPLAY_V1", as.character(length(rows)))
  for (row in rows) {
    statistics <- row$statistics %||% list()
    lines <- c(lines, as.character(row$row_index), as.character(length(statistic_columns)))
    for (column in statistic_columns) {
      components <- statistics[[column]] %||% list()
      lines <- c(lines, as.character(length(components)))
      if (length(components)) {
        for (key in names(components)) {
          lines <- c(lines, .rls_table1_wire_value(key),
                     .rls_table1_wire_value(components[[key]] %||% ""))
        }
      }
    }
  }
  lines <- c(lines, "TABLE1_RAW_V1", as.character(length(rows)))
  for (row in rows) {
    raw_statistics <- row$raw_statistics %||% list()
    lines <- c(lines, as.character(row$row_index), as.character(length(statistic_columns)))
    for (column in statistic_columns) {
      components <- raw_statistics[[column]] %||% list()
      lines <- c(lines, as.character(length(components)))
      if (length(components)) for (key in names(components)) {
        lines <- c(lines, .rls_table1_wire_value(key),
                   .rls_native_wire_number(components[[key]]))
      }
    }
  }
  if ((record$table_type %||% "") %in% c("nested_contingency", "mi_contingency", "mi_diagnostics", "missing_data_overview", "missing_data_test", "missingness_descriptives", "frequency"))
    lines <- c(lines, "TABLE_KIND_V1", record$table_type)
  if ((record$table_type %||% "") %in% c("mi_contingency", "nested_contingency")) {
    lines <- c(lines,"MI_CONTINGENCY_LAYOUT_V1",record$contingency_display_mode %||% "count_percent",
      as.character(length(record$row_variables)),.rls_native_wire_value(record$row_variables),
      as.character(length(record$summary_statistics)))
    for(row in record$summary_statistics) lines <- c(lines,
      as.character(length(row$stub_values)),.rls_native_wire_value(row$stub_values))
  }
  if (identical(record$table_type, "nested_contingency")) {
    lines <- c(lines, "CONTINGENCY_ROWS_V1", as.character(length(rows)))
    for (row in rows) {
      lines <- c(lines, as.character(length(row$row_key)), .rls_native_wire_value(row$row_key),
        as.character(length(row$row_ids)), as.character(row$row_ids), as.character(length(row$cell_ids)))
      for (ids in row$cell_ids) lines <- c(lines, as.character(length(ids)), as.character(ids))
    }
  }
  if (!is.null(record$missingness_layout)) {
    layout <- record$missingness_layout
    lines <- c(lines,"MISSINGNESS_LAYOUT_V1",layout$source_dataset,
      as.character(length(layout$available_variables)),layout$available_variables,
      as.character(length(layout$pattern_rows)))
    for (pattern in names(layout$pattern_rows)) lines <- c(lines,pattern,
      as.character(length(layout$pattern_rows[[pattern]])),as.character(layout$pattern_rows[[pattern]]))
  }
  c(lines, .rls_analysis_provenance_payload(record))
}

.rls_table1_sync_native <- function(record) {
  if (!isTRUE(.rls_state$process_started)) return(FALSE)
  result <- try(.rls_send(.rls_table1_native_payload(record)), silent = TRUE)
  if (inherits(result, "try-error")) {
    lines <- strsplit(.rls_render_table1_text(record), "\n", fixed = TRUE)[[1L]]
    result <- try(.rls_send(c("TABLE1_OPEN", record$id, record$dataset_id, as.character(length(lines)), lines)), silent = TRUE)
  }
  is.character(result) && length(result) && startsWith(result[[1L]], "OK")
}

#' Create a native Table 1 / descriptive summary table
#'
#' @param data Registered dataset name, a data frame, or `NULL` for active dataset.
#' @param variables Variables to summarize. If `NULL`, supported non-ID variables are selected.
#' @param group Optional grouping variable.
#' @param variable_types Optional named vector/list overriding variable types.
#' @param include_missing Logical. Show missing counts.
#' @param show_p Logical. Show p-values when grouped.
#' @param show_test Logical. Show test labels when grouped.
#' @param show_n Logical. Show N row.
#' @param ordinal_as How Ordinal variables are summarized.
#' @param numeric_stats Numeric summaries.
#' @param categorical_stats Categorical summaries.
#' @param ordinal_stats Ordinal summaries.
#' @param name Optional Table 1 id.
#' @param native Logical; show the result in the native LinkEDA window.
#' @param .selected_rows Internal explicit original-row IDs supplied by the native analysis-scope bridge.
#' @param .scope_description Internal human-readable description for the captured analysis scope.
#' @return A `rlispstat_table1` handle.
#' @export
ls_new_table1 <- function(data = NULL,
                          variables = NULL,
                          group = NULL,
                          variable_types = NULL,
                          include_missing = TRUE,
                          show_p = TRUE,
                          show_test = TRUE,
                          show_n = TRUE,
                          ordinal_as = c("ordinal", "categorical"),
                          numeric_stats = c("mean_sd", "median_iqr"),
                          categorical_stats = c("n_percent"),
                          ordinal_stats = c("n_percent", "median_iqr"),
                          name = NULL, native = isTRUE(.rls_state$process_started),
                          .selected_rows = NULL,
                          .scope_description = NULL) {
  ordinal_as <- match.arg(ordinal_as)
  numeric_stats <- match.arg(numeric_stats, c("mean_sd", "median_iqr"), several.ok = TRUE)
  categorical_stats <- match.arg(categorical_stats, c("n_percent"), several.ok = TRUE)
  ordinal_stats <- match.arg(ordinal_stats, c("n_percent", "median_iqr"), several.ok = TRUE)
  if (inherits(data, "linkeda_multiple_imputation_version")) {
    dataset_id <- .rls_register_dataset(
      name %||% "table1_imputed_data", data$data,
      source = "Immutable LinkEDA multiple-imputation version", activate = TRUE
    )
    dataset <- .rls_dataset_record(dataset_id)
    dataset$dataset_type <- "multiple_imputation"
    dataset$completed_datasets <- data$completed_datasets
    dataset$imputation_count <- length(data$completed_datasets)
    dataset$mids_object <- data$mids_object %||% NULL
    dataset$original_row_ids <- seq_len(nrow(dataset$data))
    dataset$data_version <- as.integer(attr(data, "linkeda_data_version") %||% 1L)
    assign(dataset$group, dataset, envir = .rls_state$datasets)
  } else if (is.data.frame(data)) {
    dataset_id <- .rls_register_dataset(name %||% "table1_data", data, source = "Table 1 data", activate = TRUE)
    dataset <- .rls_dataset_record(dataset_id)
  } else {
    dataset <- .rls_dataset_record(data)
  }
  # Execute against the same immutable snapshot that the provenance wrapper
  # later retrieves. This also repairs legacy/test records whose MI fields
  # were populated after their initial ordinary snapshot was registered.
  .rls_store_data_version(dataset)
  scope_snapshot <- .rls_capture_analysis_scope(dataset,
    if(is.null(.selected_rows)) "all" else "selected",
    if(is.null(.selected_rows) && !is.null(.scope_description)) integer() else .selected_rows)
  .selected_rows <- if(scope_snapshot$kind=="all") NULL else scope_snapshot$rows
  .scope_description <- .scope_description %||% scope_snapshot$description
  scope_rows <- NULL
  dataset_total_n <- nrow(dataset$data)
  if (!is.null(.selected_rows)) {
    dataset <- .rls_dataset_subset_original_rows(dataset, .selected_rows)
    scope_rows <- dataset$original_row_ids
    if (!nrow(dataset$data)) {
      stop("No rows are selected. Select one or more rows, or use All data.",
           call. = FALSE)
    }
  }
  variables <- .rls_table1_validate_variables(dataset, variables, variable_types, ordinal_as)
  if (!is.null(group)) {
    group <- .rls_validate_protocol_name(group, "group")
    variables <- setdiff(variables, group)
  }
  executed_r_code <- .rls_table1_executed_r_code(
    variables, group, variable_types, include_missing, show_p, show_test,
    show_n, ordinal_as, numeric_stats, categorical_stats, ordinal_stats,
    multiple_imputation = identical(
      .rls_analysis_backend(dataset, "table1"), "multiple_imputation"
    ), name = name
  )
  record <- .rls_execute_table1_r_code(executed_r_code, dataset)
  record$data_scope <- list(
    kind = if (is.null(scope_rows)) "all" else "explicit",
    description = if (is.null(scope_rows)) "All observations" else
      (.scope_description %||% "Explicit subset"),
    n = if (is.null(scope_rows)) dataset_total_n else length(scope_rows),
    total_n = dataset_total_n,
    rows = if (is.null(scope_rows)) integer() else as.integer(scope_rows)
  )
  if (!is.null(scope_rows)) {
    record$footnotes <- c(record$footnotes, sprintf(
      "Data scope: %s \u00B7 N = %d of %d observations.",
      record$data_scope$description, record$data_scope$n, record$data_scope$total_n))
  }
  record$table1_id <- record$id
  verification <- if (identical(record$analysis_backend, "multiple_imputation")) {
    .rls_table1_mi_verification_r_code(record)
  } else {
    .rls_table1_verification_r_code(record)
  }
  record <- .rls_attach_analysis_provenance(
    record, executed_r_code, record$title %||% "Table 1",
    output_code = list(table = "table_values <- table1_result$display_table"),
    verification_code = list(table = verification$code),
    verification_variables = verification$variables,
    verification_warnings = verification$warnings
  )
  assign(record$id, record, envir = .rls_state$table1_tables)
  handle <- structure(list(id = record$id, dataset_id = record$dataset_id, group = record$group),
                      class = "rlispstat_table1")
  if (isTRUE(native)) .rls_table1_sync_native(record)
  handle
}

#' @rdname ls_new_table1
#' @export
ls_table1_table <- function(table) {
  .rls_table1_record(table)$display_table
}

#' @rdname ls_new_table1
#' @export
ls_copy_table1 <- function(table, format = c("text", "markdown")) {
  format <- match.arg(format)
  record <- .rls_table1_record(table)
  text <- if (identical(format, "markdown")) .rls_render_table1_markdown(record) else .rls_render_table1_text(record)
  .rls_copy_text(text)
}

#' @rdname ls_new_table1
#' @export
ls_export_table1 <- function(table, path, format = c("pdf", "txt", "md", "csv")) {
  format <- match.arg(format)
  record <- .rls_table1_record(table)
  switch(format,
    pdf = .rls_export_table_pdf(list(
      title = record$title %||% "Table 1",
      responses_differ = FALSE,
      response = "",
      coefficients = record$display_table,
      fit = data.frame(Note = record$footnotes, stringsAsFactors = FALSE),
      note = ""
    ), path),
    txt = writeLines(.rls_render_table1_text(record), path, useBytes = TRUE),
    md = writeLines(.rls_render_table1_markdown(record), path, useBytes = TRUE),
    csv = utils::write.csv(.rls_table1_csv(record), path, row.names = FALSE, na = "")
  )
  invisible(path)
}

#' @export
print.rlispstat_table1 <- function(x, ...) {
  cat(.rls_render_table1_text(.rls_table1_record(x)), "\n")
  invisible(x)
}
