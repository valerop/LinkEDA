.rls_model_trellis_term_tests <- function(fit) {
  tab <- tryCatch(suppressWarnings(stats::anova(fit)), error = function(e) NULL)
  if (is.null(tab) || !nrow(tab)) return(data.frame())
  labels <- rownames(tab)
  keep <- !labels %in% c("<none>", "Residuals")
  if (!any(keep)) return(data.frame())
  tab <- tab[keep, , drop = FALSE]
  labels <- labels[keep]
  f_column <- intersect(c("F value", "F"), names(tab))
  p_column <- grep("^Pr\\(", names(tab), value = TRUE)
  data.frame(
    term_id = labels,
    label = vapply(labels, .rls_model_term_display_name, character(1L)),
    statistic = if (length(f_column)) as.numeric(tab[[f_column[[1L]]]]) else NA_real_,
    df1 = as.numeric(tab[["Df"]] %||% NA_real_),
    df2 = rep.int(stats::df.residual(fit), length(labels)),
    p_value = if (length(p_column)) as.numeric(tab[[p_column[[1L]]]]) else NA_real_,
    p_adjusted = NA_real_,
    p_holm = NA_real_,
    p_bonferroni = NA_real_,
    estimable = if (length(f_column)) is.finite(as.numeric(tab[[f_column[[1L]]]])) else FALSE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.rls_model_trellis_coefficient_results <- function(record, confidence_level) {
  rows <- .rls_native_linear_coefficient_rows(record)
  if (!is.data.frame(rows) || !nrow(rows)) return(data.frame())
  intervals <- tryCatch(suppressWarnings(stats::confint(record$fit, level = confidence_level)), error = function(e) NULL)
  coefficient_names <- tryCatch(names(stats::coef(record$fit)), error = function(e) character())
  result <- rows
  result$coefficient_id <- ifelse(!is.na(result$coefficient_name) & nzchar(result$coefficient_name),
                                  as.character(result$coefficient_name), as.character(result$term))
  result$ci_lower <- NA_real_
  result$ci_upper <- NA_real_
  result$p_adjusted <- NA_real_
  result$p_holm <- NA_real_
  result$p_bonferroni <- NA_real_
  result$estimable <- FALSE
  for (i in seq_len(nrow(result))) {
    candidate <- as.character(result$coefficient_id[[i]])
    if (!is.null(intervals) && candidate %in% rownames(intervals)) {
      result$ci_lower[[i]] <- intervals[candidate, 1L]
      result$ci_upper[[i]] <- intervals[candidate, 2L]
    }
    result$estimable[[i]] <- is.finite(suppressWarnings(as.numeric(result$estimate[[i]]))) &&
      is.finite(suppressWarnings(as.numeric(result$std_error[[i]]))) &&
      is.finite(suppressWarnings(as.numeric(result$p_value[[i]])))
  }
  result
}

.rls_model_trellis_fit_panel <- function(data, response, terms, term_types, rows,
                                         confidence_level = 0.95) {
  rows <- unique(as.integer(rows))
  rows <- rows[is.finite(rows) & rows >= 1L & rows <= nrow(data)]
  if (!length(rows)) return(list(
    has_observations = FALSE, estimable = FALSE, n_complete = 0L, warnings = "No observations",
    record = NULL, coefficient_results = data.frame(), term_tests = data.frame(),
    adjusted_global_p = NA_real_
  ))

  typed_data <- .rls_model_data_for_term_types(data, term_types %||% list(), response = response)
  panel_data <- typed_data[rows, , drop = FALSE]
  formula <- .rls_model_formula_object(response, terms)
  complete <- .rls_model_complete_data(panel_data, formula, "all", integer())
  complete$rows <- rows[complete$rows]
  warnings <- character()
  if (!nrow(complete$data)) return(list(
    has_observations = TRUE, estimable = FALSE, n_complete = 0L,
    warnings = "No complete observations for the effective panel formula.",
    record = NULL, coefficient_results = data.frame(), term_tests = data.frame(),
    adjusted_global_p = NA_real_
  ))

  fit_record <- list(
    data = typed_data,
    dependent = response,
    predictors = terms,
    term_types = term_types,
    scope = "all",
    selected_rows = integer()
  )
  fit <- tryCatch(
    withCallingHandlers(
      stats::lm(formula, data = complete$data, singular.ok = TRUE, na.action = stats::na.omit),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  if (inherits(fit, "error")) return(list(
    has_observations = TRUE, estimable = FALSE, n_complete = nrow(complete$data),
    warnings = unique(c(warnings, conditionMessage(fit))), record = NULL,
    coefficient_results = data.frame(), term_tests = data.frame(), adjusted_global_p = NA_real_
  ))

  extracted <- .rls_glm_extract_fit(fit_record, fit, complete)
  record <- c(fit_record, extracted)
  aliased <- names(stats::coef(fit))[is.na(stats::coef(fit))]
  if (length(aliased)) warnings <- c(warnings, paste("Non-estimable coefficients:", paste(aliased, collapse = ", ")))
  if (stats::df.residual(fit) <= 0L) warnings <- c(warnings, "Zero residual degrees of freedom.")
  if (!is.finite(summary(fit)$sigma) || identical(unname(summary(fit)$sigma), 0)) {
    warnings <- c(warnings, "Residual variance is zero or unavailable.")
  }
  if (qr(stats::model.matrix(fit))$rank < ncol(stats::model.matrix(fit))) {
    warnings <- c(warnings, "The panel model matrix is rank deficient.")
  }
  record$status <- paste(unique(warnings), collapse = "; ")
  coefficient_results <- .rls_model_trellis_coefficient_results(record, confidence_level)
  term_tests <- .rls_model_trellis_term_tests(fit)
  estimable <- stats::df.residual(fit) > 0L && any(coefficient_results$estimable)
  list(
    has_observations = TRUE,
    estimable = estimable,
    n_complete = nrow(complete$data),
    warnings = unique(warnings),
    record = record,
    coefficient_results = coefficient_results,
    term_tests = term_tests,
    adjusted_global_p = NA_real_
  )
}

.rls_model_trellis_adjust <- function(results, method = "holm") {
  method <- match.arg(method, c("none", "holm", "bonferroni"))
  coefficient_ids <- unique(unlist(lapply(results, function(x) {
    if (is.data.frame(x$coefficient_results)) as.character(x$coefficient_results$coefficient_id) else character()
  }), use.names = FALSE))
  for (id in coefficient_ids) {
    locations <- lapply(seq_along(results), function(i) {
      rows <- results[[i]]$coefficient_results
      if (!is.data.frame(rows)) return(NULL)
      hit <- which(rows$coefficient_id == id & rows$estimable & is.finite(rows$p_value))
      if (length(hit)) c(i, hit[[1L]]) else NULL
    })
    locations <- Filter(Negate(is.null), locations)
    if (length(locations)) {
      p <- vapply(locations, function(x) results[[x[[1L]]]]$coefficient_results$p_value[[x[[2L]]]], numeric(1L))
      adjusted <- stats::p.adjust(p, method = method)
      holm <- stats::p.adjust(p, method = "holm")
      bonferroni <- stats::p.adjust(p, method = "bonferroni")
      for (j in seq_along(locations)) {
        where <- locations[[j]]
        results[[where[[1L]]]]$coefficient_results$p_adjusted[[where[[2L]]]] <- adjusted[[j]]
        results[[where[[1L]]]]$coefficient_results$p_holm[[where[[2L]]]] <- holm[[j]]
        results[[where[[1L]]]]$coefficient_results$p_bonferroni[[where[[2L]]]] <- bonferroni[[j]]
      }
    }
  }
  term_ids <- unique(unlist(lapply(results, function(x) {
    if (is.data.frame(x$term_tests)) as.character(x$term_tests$term_id) else character()
  }), use.names = FALSE))
  for (id in term_ids) {
    locations <- lapply(seq_along(results), function(i) {
      rows <- results[[i]]$term_tests
      if (!is.data.frame(rows)) return(NULL)
      hit <- which(rows$term_id == id & rows$estimable & is.finite(rows$p_value))
      if (length(hit)) c(i, hit[[1L]]) else NULL
    })
    locations <- Filter(Negate(is.null), locations)
    if (length(locations)) {
      p <- vapply(locations, function(x) results[[x[[1L]]]]$term_tests$p_value[[x[[2L]]]], numeric(1L))
      adjusted <- stats::p.adjust(p, method = method)
      holm <- stats::p.adjust(p, method = "holm")
      bonferroni <- stats::p.adjust(p, method = "bonferroni")
      for (j in seq_along(locations)) {
        where <- locations[[j]]
        results[[where[[1L]]]]$term_tests$p_adjusted[[where[[2L]]]] <- adjusted[[j]]
        results[[where[[1L]]]]$term_tests$p_holm[[where[[2L]]]] <- holm[[j]]
        results[[where[[1L]]]]$term_tests$p_bonferroni[[where[[2L]]]] <- bonferroni[[j]]
      }
    }
  }
  locations <- which(vapply(results, function(x) {
    !is.null(x$record) && is.finite(x$record$summary$global_p %||% NA_real_)
  }, logical(1L)))
  if (length(locations)) {
    p <- vapply(results[locations], function(x) x$record$summary$global_p, numeric(1L))
    adjusted <- stats::p.adjust(p, method = method)
    holm <- stats::p.adjust(p, method = "holm")
    bonferroni <- stats::p.adjust(p, method = "bonferroni")
    for (j in seq_along(locations)) {
      results[[locations[[j]]]]$adjusted_global_p <- adjusted[[j]]
      results[[locations[[j]]]]$holm_global_p <- holm[[j]]
      results[[locations[[j]]]]$bonferroni_global_p <- bonferroni[[j]]
    }
  }
  results
}

.rls_model_trellis_fit <- function(data, response, effective_terms, term_types,
                                   panels, confidence_level = 0.95,
                                   p_adjustment = "holm") {
  results <- lapply(panels, function(panel) {
    result <- .rls_model_trellis_fit_panel(
      data, response, effective_terms, term_types, panel$rows, confidence_level
    )
    c(panel, result)
  })
  .rls_model_trellis_adjust(results, p_adjustment)
}

.rls_model_trellis_native_payload <- function(id, generation, results) {
  payload <- c("MODEL_TRELLIS_UPDATE", .rls_native_wire_value(id), as.character(as.integer(generation)), as.character(length(results)))
  for (result in results) {
    payload <- c(payload,
      .rls_native_wire_value(result$panel_id),
      .rls_native_wire_value(result$row_level_id),
      .rls_native_wire_value(result$row_level_label),
      .rls_native_wire_value(result$column_level_id),
      .rls_native_wire_value(result$column_level_label),
      .rls_native_wire_bool(result$has_observations),
      .rls_native_wire_bool(result$estimable),
      as.character(length(result$warnings)),
      .rls_native_wire_value(result$warnings)
    )
    empty_record <- list(
      fit = NULL, coefficient_rows = data.frame(), summary = list(n_used = result$n_complete %||% 0L, n_excluded = length(result$rows) - (result$n_complete %||% 0L)),
      rows_used = integer(), rows_excluded = integer(), status = paste(result$warnings, collapse = "; ")
    )
    payload <- c(payload, .rls_native_linear_fit_payload(result$record %||% empty_record))
    rows <- result$coefficient_results
    payload <- c(payload, as.character(if (is.data.frame(rows)) nrow(rows) else 0L))
    if (is.data.frame(rows) && nrow(rows)) for (i in seq_len(nrow(rows))) payload <- c(payload,
      .rls_native_wire_value(rows$coefficient_id[[i]]),
      .rls_native_wire_value(rows$source_term[[i]]),
      .rls_native_wire_value(rows$display_label[[i]]),
      .rls_native_wire_value(rows$row_type[[i]]),
      .rls_native_wire_value(rows$term_type[[i]]),
      .rls_native_wire_value(rows$level[[i]]),
      .rls_native_wire_value(rows$reference_level[[i]]),
      .rls_native_wire_number(rows$estimate[[i]]),
      .rls_native_wire_number(rows$std_error[[i]]),
      .rls_native_wire_number(rows$statistic[[i]]),
      .rls_native_wire_number(rows$p_value[[i]]),
      .rls_native_wire_number(rows$p_adjusted[[i]]),
      .rls_native_wire_number(rows$p_holm[[i]]),
      .rls_native_wire_number(rows$p_bonferroni[[i]]),
      .rls_native_wire_number(rows$ci_lower[[i]]),
      .rls_native_wire_number(rows$ci_upper[[i]]),
      .rls_native_wire_bool(rows$estimable[[i]])
    )
    tests <- result$term_tests
    payload <- c(payload, as.character(if (is.data.frame(tests)) nrow(tests) else 0L))
    if (is.data.frame(tests) && nrow(tests)) for (i in seq_len(nrow(tests))) payload <- c(payload,
      .rls_native_wire_value(tests$term_id[[i]]),
      .rls_native_wire_value(tests$label[[i]]),
      .rls_native_wire_number(tests$statistic[[i]]),
      .rls_native_wire_number(tests$df1[[i]]),
      .rls_native_wire_number(tests$df2[[i]]),
      .rls_native_wire_number(tests$p_value[[i]]),
      .rls_native_wire_number(tests$p_adjusted[[i]]),
      .rls_native_wire_number(tests$p_holm[[i]]),
      .rls_native_wire_number(tests$p_bonferroni[[i]]),
      .rls_native_wire_bool(tests$estimable[[i]])
    )
    payload <- c(payload, .rls_native_wire_number(result$adjusted_global_p),
                 .rls_native_wire_number(result$holm_global_p),
                 .rls_native_wire_number(result$bonferroni_global_p))
  }
  payload
}

.rls_handle_model_trellis_needed <- function(parts) {
  cursor <- 2L
  take <- function() { value <- parts[[cursor]]; cursor <<- cursor + 1L; value }
  take_vector <- function() {
    count <- suppressWarnings(as.integer(take()))
    if (!is.finite(count) || count <= 0L) return(character())
    value <- parts[seq.int(cursor, cursor + count - 1L)]
    cursor <<- cursor + count
    value
  }
  tryCatch({
    id <- take(); generation <- as.integer(take()); group <- take(); response <- take()
    scope <- take(); confidence <- as.numeric(take()); adjustment <- take()
    base_terms <- take_vector(); effective_terms <- take_vector(); omitted_terms <- take_vector()
    type_count <- as.integer(take()); term_types <- list()
    if (is.finite(type_count) && type_count > 0L) for (i in seq_len(type_count)) term_types[[take()]] <- take()
    panel_count <- as.integer(take()); panels <- vector("list", panel_count)
    for (i in seq_len(panel_count)) {
      panel_id <- take(); row_level_id <- take(); row_level_label <- take()
      column_level_id <- take(); column_level_label <- take()
      rows <- suppressWarnings(as.integer(take_vector()))
      panels[[i]] <- list(panel_id = panel_id, row_level_id = row_level_id,
                          row_level_label = row_level_label, column_level_id = column_level_id,
                          column_level_label = column_level_label, rows = rows)
    }
    dataset <- .rls_dataset_record(group)
    results <- .rls_model_trellis_fit(dataset$data, response, effective_terms, term_types,
                                      panels, confidence, adjustment)
    .rls_send(.rls_model_trellis_native_payload(id, generation, results))
  }, error = function(e) {
    id <- if (exists("id", inherits = FALSE)) id else ""
    generation <- if (exists("generation", inherits = FALSE)) generation else 0L
    try(.rls_send(c("MODEL_TRELLIS_UPDATE_ERROR", .rls_native_wire_value(id),
                    as.character(generation), .rls_native_wire_value(conditionMessage(e)))), silent = TRUE)
  })
  invisible(NULL)
}

#' Open a linear model trellis
#'
#' Fits the same linear-model specification independently in the panels formed
#' by one or two categorical conditioning variables. Panel calculations are
#' performed in R; Windows only presents the shared model-trellis state.
#'
#' @param group Registered dataset or dataset group name.
#' @param response Numeric response variable.
#' @param terms Optional model terms used in every panel.
#' @param columns Categorical conditioning variable displayed in columns.
#' @param rows Optional categorical conditioning variable displayed in rows.
#' @param p_adjustment Multiplicity adjustment across panels.
#' @return A native model-trellis handle.
#' @export
ls_new_model_trellis <- function(group = NULL, response, terms = character(),
                                 columns, rows = NULL,
                                 p_adjustment = c("holm", "bonferroni", "none")) {
  record <- .rls_dataset_record(group)
  response <- .rls_validate_protocol_name(response, "response")
  columns <- .rls_validate_protocol_name(columns, "columns")
  if (!is.null(rows)) rows <- .rls_validate_protocol_name(rows, "rows")
  p_adjustment <- match.arg(p_adjustment)
  required <- c(response, columns, rows %||% character())
  missing <- setdiff(required, names(record$data))
  if (length(missing)) stop(sprintf("Column `%s` was not found in the dataset.", missing[[1L]]), call. = FALSE)
  if (!is.numeric(record$data[[response]])) stop("`response` must name a numeric variable.", call. = FALSE)
  if (!is.null(rows) && identical(rows, columns)) stop("`rows` and `columns` must be different.", call. = FALSE)
  terms <- as.character(terms %||% character())
  if (anyNA(terms)) stop("`terms` must not contain missing values.", call. = FALSE)
  terms <- unique(vapply(terms, function(term)
    .rls_model_validate_term(record$data, term, response = response, what = "term"), character(1L)))
  if (.Platform$OS.type == "windows") {
    .rls_register_native_dataset_if_needed(record, sender = .rls_send_winui)
  } else {
    .rls_start_backend()
    .rls_register_native_dataset_if_needed(record)
  }
  reply <- .rls_send(c("ANALYZE_LINEAR_MODEL_TRELLIS", record$group, response,
    rows %||% "", columns, p_adjustment, as.character(length(terms)), terms))
  id <- .rls_parse_records(reply)[[1L]]
  structure(list(id = id, group = record$group, response = response,
                 terms = terms, rows = rows, columns = columns,
                 p_adjustment = p_adjustment),
            class = "rlispstat_model_trellis")
}
