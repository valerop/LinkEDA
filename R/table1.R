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
  labels <- .rls_table1_value_labels(x)
  if (.rls_table1_is_id_like(x, variable)) return("unsupported")
  if (identical(metadata_type, "ordinal")) return("ordinal")
  if (is.ordered(x)) return(if (identical(ordinal_as, "ordinal")) "ordinal" else "categorical")
  if (!is.null(labels)) return("categorical")
  if (is.factor(x) || identical(metadata_type, "factor")) return("categorical")
  if (is.logical(x) || identical(metadata_type, "logical")) return("categorical")
  if (is.character(x) || identical(metadata_type, "character")) {
    observed <- x[!is.na(x)]
    unique_n <- length(unique(observed))
    if (unique_n <= max(12L, floor(length(observed) / 3))) return("categorical")
    return("unsupported")
  }
  if (is.numeric(x) || is.integer(x) || identical(metadata_type, "numeric")) {
    observed <- .rls_table1_raw(x)
    observed <- observed[!is.na(observed)]
    if (!length(observed)) return("numeric")
    unique_n <- length(unique(observed))
    integer_like <- all(abs(observed - round(observed)) < .Machine$double.eps^0.5)
    if (unique_n <= 2L && integer_like) {
      return("categorical")
    }
    low_cardinality_limit <- min(7L, max(3L, floor(length(observed) / 4L)))
    if (unique_n <= low_cardinality_limit && integer_like) {
      return("ordinal")
    }
    return("numeric")
  }
  "unsupported"
}

.rls_table1_validate_variables <- function(record, variables, variable_types = NULL, ordinal_as = "ordinal") {
  if (is.null(variables)) {
    variables <- names(record$data)[!vapply(names(record$data), function(variable) {
      identical(.rls_table1_infer_type(record, variable, variable_types, ordinal_as), "unsupported")
    }, logical(1L))]
    return(utils::head(variables, 8L))
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
  if (!is.null(labels)) {
    values <- unname(labels)
    level_names <- names(labels)
    out <- rep(NA_character_, length(raw))
    for (i in seq_along(values)) out[!is.na(raw) & raw == values[[i]]] <- level_names[[i]]
    extra <- sort(unique(as.character(raw[!is.na(raw) & is.na(out)])))
    levels <- c(level_names, extra)
    return(factor(out, levels = unique(levels)))
  }
  if (is.factor(x)) return(droplevels(x))
  factor(raw)
}

.rls_table1_levels <- function(x, type) {
  raw <- .rls_table1_raw(x)
  labels <- .rls_table1_value_labels(x)
  if (!is.null(labels)) return(names(labels))
  if (is.factor(x)) return(levels(x))
  vals <- raw[!is.na(raw)]
  if (!length(vals)) return(character())
  if (identical(type, "ordinal") && is.numeric(vals)) return(as.character(sort(unique(vals))))
  sort(unique(as.character(vals)))
}

.rls_table1_display_factor <- function(x, type) {
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
  factor(as.character(raw), levels = .rls_table1_levels(x, type))
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

.rls_table1_count_percent <- function(x, level) {
  non_missing <- !is.na(x)
  denominator <- sum(non_missing)
  n <- sum(non_missing & as.character(x) == level)
  pct <- if (denominator > 0) n / denominator else NA_real_
  sprintf("%d (%s)", n, .rls_export_format_percent(pct))
}

.rls_table1_group_slices <- function(n, group_factor = NULL) {
  if (is.null(group_factor)) return(list(Overall = seq_len(n)))
  levels <- levels(droplevels(group_factor))
  c(list(Overall = seq_len(n)), stats::setNames(lapply(levels, function(level) which(!is.na(group_factor) & group_factor == level)), levels))
}

.rls_table1_test <- function(data, variable, type, group_factor, original_row_ids) {
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
                status = "insufficient_data", detail = "Too few complete observations or group levels."))
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
      if (any(dim(tab) < 2L)) stop("Too few levels for association test.")
      chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
      sparse <- any(chi$expected < 5)
      if (all(dim(tab) == c(2L, 2L)) && sparse) {
        fit <- stats::fisher.test(tab)
        list(p = unname(fit$p.value), test = "Fisher exact", statistic = NA_real_,
             parameter = NA_real_, detail = "Fisher exact test")
      } else if (sparse && prod(dim(tab)) > 16L) {
        fit <- suppressWarnings(stats::chisq.test(tab, simulate.p.value = TRUE, B = 2000))
        list(p = unname(fit$p.value), test = "\u03c7\u00b2 simulated", statistic = unname(fit$statistic),
             parameter = NA_real_, detail = sprintf("chi-square = %.3f; simulated p-value", fit$statistic))
      } else if (sparse) {
        fit <- tryCatch(stats::fisher.test(tab), error = function(e) NULL)
        if (!is.null(fit)) {
          list(p = unname(fit$p.value), test = "Fisher exact", statistic = NA_real_,
               parameter = NA_real_, detail = "Fisher exact test")
        } else {
          fit <- suppressWarnings(stats::chisq.test(tab, simulate.p.value = TRUE, B = 2000))
          list(p = unname(fit$p.value), test = "\u03c7\u00b2 simulated", statistic = unname(fit$statistic),
               parameter = NA_real_, detail = sprintf("chi-square = %.3f; simulated p-value", fit$statistic))
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
  add_row <- function(label, values, p = "", test = "", row_type = "summary", variable = "", level = "", detail = "") {
    row <- as.list(c(Variable = label, values))
    if (!is.null(group_factor) && isTRUE(show_p)) row[["p"]] <- p
    if (!is.null(group_factor) && isTRUE(show_test)) row[["Test"]] <- test
    new_row <- as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
    display <<- rbind(display, new_row[names(display)])
    rows[[length(rows) + 1L]] <<- list(
      row_index = nrow(display), variable = variable, level = level,
      row_type = row_type, values = values, p = p, test = test, detail = detail
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
    if (all(is.na(raw))) {
      stop(sprintf("Variable `%s` has all values missing.", variable), call. = FALSE)
    }
    complete_var <- which(!is.na(raw))
    rows_used_by_variable[[variable]] <- original_ids[complete_var]
    missingness[[variable]] <- list(
      total_n = length(raw),
      non_missing_n = length(complete_var),
      missing_n = sum(is.na(raw)),
      missing_percent = mean(is.na(raw)),
      by_group = vapply(slices, function(idx) sum(is.na(raw[idx])), integer(1L))
    )
    test <- .rls_table1_test(data, variable, type, group_factor, original_ids)
    if (!is.null(test)) rows_used_by_test[[variable]] <- test$rows_used_original_ids
    test_results[[variable]] <- test
    p_text <- if (!is.null(test) && isTRUE(show_p)) .rls_export_format_p(test$p) else ""
    test_text <- if (!is.null(test) && isTRUE(show_test)) test$test else ""
    if (identical(type, "numeric")) {
      values <- vapply(slices, function(idx) .rls_table1_format_mean_sd(.rls_table1_numeric(x[idx])), character(1L))
      add_row(variable, values, p_text, test_text, "numeric_mean_sd", variable,
              detail = paste(variable, "mean/SD; p uses", test_text))
      if ("median_iqr" %in% numeric_stats) {
        values <- vapply(slices, function(idx) .rls_table1_format_median_iqr(.rls_table1_numeric(x[idx])), character(1L))
        add_row("  Median [Q1, Q3]", values, row_type = "numeric_median_iqr", variable = variable)
      }
    } else {
      xf <- .rls_table1_display_factor(x, type)
      add_row(variable, stats::setNames(rep("", length(column_names)), column_names),
              p_text, test_text, if (identical(type, "ordinal")) "ordinal_parent" else "categorical_parent", variable)
      for (level in .rls_table1_levels(x, type)) {
        values <- vapply(slices, function(idx) .rls_table1_count_percent(xf[idx], level), character(1L))
        add_row(paste0("  ", level), values, row_type = paste0(type, "_level"),
                variable = variable, level = level,
                detail = paste(variable, "=", level, "n (%)"))
      }
      if (identical(type, "ordinal") && "median_iqr" %in% ordinal_stats) {
        scores <- .rls_table1_ordinal_scores(x)
        values <- vapply(slices, function(idx) .rls_table1_format_median_iqr(scores[idx][!is.na(scores[idx])]), character(1L))
        add_row("  Median [Q1, Q3]", values, row_type = "ordinal_median_iqr", variable = variable)
      }
    }
    if (isTRUE(include_missing)) {
      values <- vapply(slices, function(idx) {
        n <- sum(is.na(raw[idx]))
        denom <- length(idx)
        sprintf("%d (%s)", n, .rls_export_format_percent(if (denom > 0) n / denom else NA_real_))
      }, character(1L))
      add_row("  Missing", values, row_type = "missing", variable = variable,
              detail = paste(variable, "missing values"))
    }
  }
  note <- c(
    "Numeric variables are shown as mean (SD) and median [Q1, Q3].",
    "Categorical variables are shown as n (%). Percentages exclude missing values.",
    "Ordinal variables are shown as ordered n (%) and median [Q1, Q3]."
  )
  if (!is.null(group_factor)) {
    note <- c(note,
              "Numeric group comparisons use Welch t-tests or Welch one-way ANOVA.",
              "Ordinal group comparisons use Wilcoxon rank-sum or Kruskal-Wallis tests.",
              "Categorical group comparisons use chi-square or Fisher exact tests when appropriate.")
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
  value <- as.character(value %||% "")
  value[is.na(value)] <- ""
  gsub("[\r\n\t]+", " ", value)
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
             as.character(length(scope_rows)), as.character(scope_rows))
  lines
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
#' @param ordinal_as How ordered factors are summarized.
#' @param numeric_stats Numeric summaries.
#' @param categorical_stats Categorical summaries.
#' @param ordinal_stats Ordinal summaries.
#' @param name Optional Table 1 id.
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
                          name = NULL, .selected_rows = NULL,
                          .scope_description = NULL) {
  ordinal_as <- match.arg(ordinal_as)
  numeric_stats <- match.arg(numeric_stats, c("mean_sd", "median_iqr"), several.ok = TRUE)
  categorical_stats <- match.arg(categorical_stats, c("n_percent"), several.ok = TRUE)
  ordinal_stats <- match.arg(ordinal_stats, c("n_percent", "median_iqr"), several.ok = TRUE)
  if (is.data.frame(data)) {
    dataset_id <- .rls_register_dataset(name %||% "table1_data", data, source = "Table 1 data", activate = TRUE)
    dataset <- .rls_dataset_record(dataset_id)
  } else {
    dataset <- .rls_dataset_record(data)
  }
  scope_rows <- NULL
  dataset_total_n <- nrow(dataset$data)
  if (!is.null(.selected_rows)) {
    requested <- unique(as.integer(.selected_rows))
    requested <- requested[is.finite(requested) & requested > 0L]
    original_ids <- dataset$original_row_ids %||% seq_len(nrow(dataset$data))
    keep <- original_ids %in% requested
    dataset$data <- dataset$data[keep, , drop = FALSE]
    dataset$original_row_ids <- original_ids[keep]
    scope_rows <- dataset$original_row_ids
  }
  variables <- .rls_table1_validate_variables(dataset, variables, variable_types, ordinal_as)
  if (!length(variables)) stop("No variables were selected for Table 1.", call. = FALSE)
  if (!is.null(group)) {
    group <- .rls_validate_protocol_name(group, "group")
    variables <- setdiff(variables, group)
  }
  if (!length(variables)) stop("No variables were selected for Table 1.", call. = FALSE)
  record <- if (identical(.rls_analysis_backend(dataset, "table1"), "multiple_imputation")) {
    .rls_table1_build_mi(dataset, variables, group, variable_types, isTRUE(include_missing),
                         isTRUE(show_p), isTRUE(show_test), isTRUE(show_n), ordinal_as,
                         numeric_stats, categorical_stats, ordinal_stats, name)
  } else {
    .rls_table1_build(dataset, variables, group, variable_types, isTRUE(include_missing),
                      isTRUE(show_p), isTRUE(show_test), isTRUE(show_n), ordinal_as,
                      numeric_stats, categorical_stats, ordinal_stats, name)
  }
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
      "Data scope: %s · N = %d of %d observations.",
      record$data_scope$description, record$data_scope$n, record$data_scope$total_n))
  }
  record$table1_id <- record$id
  assign(record$id, record, envir = .rls_state$table1_tables)
  handle <- structure(list(id = record$id, dataset_id = record$dataset_id, group = record$group),
                      class = "rlispstat_table1")
  .rls_table1_sync_native(record)
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
