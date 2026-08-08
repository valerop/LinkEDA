.rls_em_dash <- "\u2014"

.rls_model_clean_factor_call <- function(term) {
  term <- as.character(term)
  match <- regexec("^factor\\(([^()]+)\\)$", term)
  hit <- regmatches(term, match)[[1L]]
  if (length(hit) == 2L) {
    trimws(hit[[2L]])
  } else {
    term
  }
}

.rls_model_term_display_name <- function(term) {
  .rls_model_clean_factor_call(term)
}

.rls_model_interaction_parts <- function(term) {
  parts <- trimws(strsplit(as.character(term), ":", fixed = TRUE)[[1L]])
  parts <- parts[nzchar(parts)]
  unique(vapply(parts, .rls_model_clean_factor_call, character(1L)))
}

.rls_model_term_without_level_suffixes <- function(term) {
  parts <- trimws(strsplit(as.character(term), ":", fixed = TRUE)[[1L]])
  parts <- parts[nzchar(parts)]
  parts <- sub("=.*$", "", parts)
  parts <- trimws(parts)
  paste(parts[nzchar(parts)], collapse = ":")
}

.rls_model_terms_equivalent <- function(a, b) {
  left <- .rls_model_interaction_parts(a)
  right <- .rls_model_interaction_parts(b)
  if (length(left) >= 2L || length(right) >= 2L) {
    return(length(left) == length(right) && identical(sort(left), sort(right)))
  }
  identical(.rls_model_clean_factor_call(a), .rls_model_clean_factor_call(b))
}

.rls_model_term_list_contains <- function(terms, term) {
  any(vapply(terms, .rls_model_terms_equivalent, logical(1L), b = term))
}

.rls_model_hierarchical_terms <- function(data, term, response = NULL, what = "term") {
  term <- .rls_model_validate_term(data, term, response = response, what = what)
  parts <- .rls_model_interaction_parts(term)
  if (length(parts) < 2L) {
    return(term)
  }
  for (part in parts) {
    .rls_model_validate_term(data, part, response = response, what = what)
  }
  out <- character()
  for (order in seq_along(parts)) {
    combos <- utils::combn(parts, order, simplify = FALSE)
    out <- c(out, vapply(combos, paste, character(1L), collapse = ":"))
  }
  unique(out)
}

.rls_model_add_hierarchical_term <- function(terms, data, term, response = NULL, what = "term") {
  unique(c(terms, .rls_model_hierarchical_terms(data, term, response = response, what = what)))
}

.rls_model_remove_hierarchical_term <- function(terms, term) {
  removed <- .rls_model_interaction_parts(term)
  if (!length(removed)) {
    return(terms)
  }
  keep <- vapply(terms, function(candidate) {
    candidate_parts <- .rls_model_interaction_parts(candidate)
    if (length(candidate_parts) < length(removed)) {
      return(TRUE)
    }
    !all(removed %in% candidate_parts)
  }, logical(1L))
  terms[keep]
}

.rls_model_validate_response <- function(data, response, what = "response") {
  response <- .rls_validate_protocol_name(response, what)
  if (!response %in% names(data)) {
    stop(sprintf("Column `%s` was not found in the dataset.", response), call. = FALSE)
  }
  if (!is.numeric(data[[response]])) {
    stop(sprintf("`%s` must be numeric.", response), call. = FALSE)
  }
  response
}

.rls_model_validate_term <- function(data, term, response = NULL, what = "term") {
  term <- .rls_validate_protocol_name(term, what)
  variable <- .rls_model_clean_factor_call(term)
  if (!variable %in% names(data) && length(all.vars(stats::as.formula(paste("~", term)))) == 1L) {
    stop(sprintf("Column `%s` was not found in the dataset.", variable), call. = FALSE)
  }
  if (!is.null(response) && identical(variable, response)) {
    stop("The dependent variable cannot also be a predictor.", call. = FALSE)
  }
  term
}

.rls_model_term_type <- function(data, term) {
  variable <- .rls_model_clean_factor_call(term)
  if (!identical(variable, term)) {
    return("factor")
  }
  if (variable %in% names(data)) {
    x <- data[[variable]]
    if (is.factor(x) || is.character(x) || is.logical(x)) {
      return("factor")
    }
    if (is.numeric(x)) {
      return("numeric")
    }
  }
  "other"
}

.rls_model_xlevel_names <- function(xlevels) {
  if (!length(xlevels)) return(character())
  unique(c(names(xlevels), vapply(names(xlevels), .rls_model_clean_factor_call, character(1L))))
}

.rls_model_component_type <- function(data, component, xlevels = list()) {
  variable <- .rls_model_clean_factor_call(component)
  if (variable %in% .rls_model_xlevel_names(xlevels)) {
    return("factor")
  }
  type <- .rls_model_term_type(data, variable)
  if (identical(type, "factor")) "factor" else "numeric"
}

.rls_model_interaction_type <- function(component_types) {
  component_types <- ifelse(component_types == "factor", "factor", "numeric")
  if (length(component_types) > 2L) {
    return("higher_order_interaction")
  }
  factor_count <- sum(component_types == "factor")
  if (factor_count == 2L) {
    return("factor_factor_interaction")
  }
  if (factor_count == 1L) {
    return("numeric_factor_interaction")
  }
  "numeric_numeric_interaction"
}

.rls_model_semantic_term_type <- function(data, term, xlevels = list()) {
  parts <- .rls_model_interaction_parts(term)
  if (length(parts) >= 2L) {
    component_types <- vapply(parts, .rls_model_component_type, character(1L), data = data, xlevels = xlevels)
    return(.rls_model_interaction_type(component_types))
  }
  display_term <- .rls_model_term_display_name(term)
  if (term %in% .rls_model_xlevel_names(xlevels) || display_term %in% .rls_model_xlevel_names(xlevels)) {
    return("factor")
  }
  .rls_model_term_type(data, term)
}

.rls_model_data_for_term_types <- function(data, term_types = list(), response = NULL) {
  if (!length(term_types)) return(data)
  for (term in names(term_types)) {
    type <- term_types[[term]]
    variables <- .rls_model_interaction_parts(term)
    if (!length(variables)) variables <- .rls_model_clean_factor_call(term)
    for (variable in variables) {
      if (!variable %in% names(data) || identical(variable, response)) next
      if (identical(type, "factor")) {
        data[[variable]] <- factor(data[[variable]])
      } else if (identical(type, "numeric")) {
        data[[variable]] <- suppressWarnings(as.numeric(data[[variable]]))
      }
    }
  }
  data
}

.rls_model_formula_object <- function(response, terms) {
  if (!nzchar(response)) {
    stop("Choose a dependent variable before fitting the model.", call. = FALSE)
  }
  predictor_variables <- all.vars(stats::reformulate(terms))
  if (response %in% predictor_variables) {
    stop("The dependent variable cannot also be a predictor.", call. = FALSE)
  }
  stats::reformulate(terms, response = response)
}

.rls_model_complete_data <- function(data, formula, scope = "all", selected = integer()) {
  vars <- unique(all.vars(formula))
  model_data <- data[, vars, drop = FALSE]
  ok <- stats::complete.cases(model_data)
  if (identical(scope, "selected")) {
    keep <- seq_len(nrow(data)) %in% selected
    ok <- ok & keep
  } else if (identical(scope, "unselected")) {
    keep <- !seq_len(nrow(data)) %in% selected
    ok <- ok & keep
  }
  list(data = data[ok, , drop = FALSE], rows = which(ok), excluded = sum(!ok))
}

.rls_model_coefficient_row <- function(row_type, term, display_label, source_term = term,
                                       term_type = "numeric", level = NA_character_,
                                       reference_level = NA_character_,
                                       coefficient_name = NA_character_,
                                       coefficient = NULL, statistic_name = "t") {
  value <- function(name) {
    if (is.null(coefficient) || !name %in% names(coefficient)) NA_real_ else coefficient[[name]]
  }
  data.frame(
    row_type = row_type,
    term = term,
    display_label = display_label,
    source_term = source_term,
    term_type = term_type,
    level = level,
    reference_level = reference_level,
    coefficient_name = coefficient_name,
    estimate = value("estimate"),
    std_error = value("std_error"),
    statistic_name = statistic_name,
    statistic = value("statistic"),
    p_value = value("p_value"),
    partial_r2 = value("partial_r2"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.rls_model_coefficient_display_rows <- function(fit, coefficients, data, requested_terms = NULL,
                                                statistic_name = "t") {
  if (!nrow(coefficients)) {
    return(data.frame())
  }
  coefs <- coefficients
  coef_by_name <- split(coefs, coefs$term)
  coefficient_for <- function(name) {
    row <- coef_by_name[[name]]
    if (is.null(row) || !nrow(row)) NULL else row[1L, , drop = FALSE]
  }

  out <- list()
  append_row <- function(row) {
    out[[length(out) + 1L]] <<- row
  }

  intercept <- coefficient_for("(Intercept)")
  append_row(.rls_model_coefficient_row(
    "coefficient", "(Intercept)", "(Intercept)", "(Intercept)", "intercept",
    coefficient_name = "(Intercept)", coefficient = intercept, statistic_name = statistic_name
  ))

  term_labels <- attr(stats::terms(fit), "term.labels")
  mm <- tryCatch(stats::model.matrix(fit), error = function(e) NULL)
  assign <- if (!is.null(mm)) attr(mm, "assign") else integer()
  mm_names <- if (!is.null(mm)) colnames(mm) else character()
  xlevels <- tryCatch(fit$xlevels, error = function(e) NULL) %||% list()
  if (!length(xlevels)) {
    xlevels <- tryCatch(.getXlevels(stats::terms(fit), stats::model.frame(fit)), error = function(e) list())
  }

  for (i in seq_along(term_labels)) {
    term_label <- term_labels[[i]]
    display_term <- .rls_model_term_display_name(term_label)
    term_type <- .rls_model_semantic_term_type(data, term_label, xlevels)
    term_columns <- mm_names[assign == i]
    term_columns <- setdiff(term_columns, "(Intercept)")

    if (length(.rls_model_interaction_parts(term_label)) >= 2L && length(term_columns)) {
      append_row(.rls_model_coefficient_row(
        "term_parent", display_term, display_term, display_term, term_type,
        statistic_name = statistic_name
      ))
      for (coef_name in term_columns) {
        append_row(.rls_model_coefficient_row(
          "coefficient", coef_name, paste0("  ", coef_name), display_term, term_type,
          coefficient_name = coef_name, coefficient = coefficient_for(coef_name),
          statistic_name = statistic_name
        ))
      }
      next
    }

    if (identical(term_type, "factor") && term_label %in% names(xlevels)) {
      levels <- as.character(xlevels[[term_label]])
      if (length(levels) >= 2L && length(term_columns) == length(levels) - 1L) {
        reference <- levels[[1L]]
        append_row(.rls_model_coefficient_row(
          "factor_parent", display_term, display_term, display_term, "factor",
          reference_level = reference, statistic_name = statistic_name
        ))
        append_row(.rls_model_coefficient_row(
          "reference", paste(display_term, reference, sep = "="), paste0("  ", reference),
          display_term, "factor", level = reference, reference_level = reference,
          statistic_name = statistic_name
        ))
        for (level_index in seq_along(levels[-1L])) {
          level <- levels[-1L][[level_index]]
          coef_name <- term_columns[[level_index]]
          append_row(.rls_model_coefficient_row(
            "factor_level", paste(display_term, level, sep = "="), paste0("  ", level),
            display_term, "factor", level = level, reference_level = reference,
            coefficient_name = coef_name, coefficient = coefficient_for(coef_name),
            statistic_name = statistic_name
          ))
        }
        next
      }
    }

    if (length(term_columns) > 1L) {
      append_row(.rls_model_coefficient_row(
        "term_parent", display_term, display_term, display_term, term_type,
        statistic_name = statistic_name
      ))
      for (coef_name in term_columns) {
        append_row(.rls_model_coefficient_row(
          "coefficient", coef_name, paste0("  ", coef_name), display_term, term_type,
          coefficient_name = coef_name, coefficient = coefficient_for(coef_name),
          statistic_name = statistic_name
        ))
      }
    } else {
      coef_name <- if (length(term_columns)) term_columns[[1L]] else term_label
      append_row(.rls_model_coefficient_row(
        "coefficient", display_term, display_term, display_term, term_type,
        coefficient_name = coef_name, coefficient = coefficient_for(coef_name),
        statistic_name = statistic_name
      ))
    }
  }

  rows <- do.call(rbind, out)
  rownames(rows) <- NULL
  rows
}

.rls_model_display_detail_text <- function(row, statistic_label = row$statistic_name[[1L]]) {
  if (!nrow(row)) return(.rls_em_dash)
  if (identical(row$row_type[[1L]], "reference")) {
    return(sprintf(
      "%s: %s is the reference category; b = %s; SE = %s; %s = %s; p = %s",
      row$source_term[[1L]], row$level[[1L]], .rls_em_dash, .rls_em_dash,
      statistic_label, .rls_em_dash, .rls_em_dash
    ))
  }
  label <- if (identical(row$row_type[[1L]], "factor_level")) {
    sprintf("%s: %s vs %s", row$source_term[[1L]], row$level[[1L]], row$reference_level[[1L]])
  } else {
    row$display_label[[1L]]
  }
  paste0(
    label, "; ",
    "b = ", .rls_regcmp_format_number(row$estimate[[1L]], 2L), "; ",
    "SE = ", .rls_regcmp_format_number(row$std_error[[1L]], 2L), "; ",
    statistic_label, " = ", .rls_regcmp_format_number(row$statistic[[1L]], 2L), "; ",
    "p", if (startsWith(.rls_regcmp_format_p(row$p_value[[1L]]), "<")) " " else " = ",
    .rls_regcmp_format_p(row$p_value[[1L]]), "; ",
    "Partial R\u00b2 = ", .rls_regcmp_format_percent(row$partial_r2[[1L]])
  )
}
