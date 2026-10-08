.rls_model_polynomial_term <- function(term) {
  expr <- tryCatch(str2lang(term), error = function(e) NULL)
  if (!is.call(expr) || !identical(expr[[1L]], as.name("I")) || length(expr) != 2L) return(NULL)
  power <- expr[[2L]]
  if (!is.call(power) || !identical(power[[1L]], as.name("^")) || length(power) != 3L ||
      !is.symbol(power[[2L]]) || !is.numeric(power[[3L]])) return(NULL)
  degree <- power[[3L]]
  if (length(degree) != 1L || !is.finite(degree) || degree != trunc(degree) || degree < 2 || degree > 99) return(NULL)
  list(variable = as.character(power[[2L]]), degree = as.integer(degree))
}

.rls_model_polynomial_expr <- function(variable, degree) {
  paste0("I(", deparse1(as.name(variable), backtick = TRUE), "^", degree, ")")
}

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
  polynomial <- .rls_model_polynomial_term(term)
  if (!is.null(polynomial)) {
    return(c(polynomial$variable, vapply(seq.int(2L, polynomial$degree), function(degree)
      .rls_model_polynomial_expr(polynomial$variable, degree), character(1L))))
  }
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
  for (candidate in .rls_model_hierarchical_terms(data, term, response = response, what = what)) {
    if (!.rls_model_term_list_contains(terms, candidate)) terms <- c(terms, candidate)
  }
  terms
}

.rls_model_remove_hierarchical_term <- function(terms, term) {
  removed <- .rls_model_interaction_parts(term)
  if (!length(removed)) {
    return(terms)
  }
  keep <- vapply(terms, function(candidate) {
    candidate_parts <- .rls_model_interaction_parts(candidate)
    if (length(removed) == 1L && is.null(.rls_model_polynomial_term(term))) {
      base_parts <- vapply(candidate_parts, function(part) {
        polynomial <- .rls_model_polynomial_term(part)
        if (is.null(polynomial)) part else polynomial$variable
      }, character(1L))
      if (removed %in% base_parts) return(FALSE)
    }
    if (length(candidate_parts) < length(removed)) {
      return(TRUE)
    }
    !all(removed %in% candidate_parts)
  }, logical(1L))
  terms[keep]
}

.rls_model_partial_r <- function(statistic, df) {
  statistic <- suppressWarnings(as.numeric(statistic))
  df <- suppressWarnings(as.numeric(df))
  denominator <- statistic^2 + df
  ifelse(is.finite(statistic) & is.finite(df) & df > 0 & denominator > 0,
         statistic / sqrt(denominator), NA_real_)
}

.rls_model_term_coefficient_names <- function(fit, source_terms) {
  if (is.null(fit) || !length(source_terms)) return(character())
  mm <- tryCatch(stats::model.matrix(fit), error = function(e) NULL)
  if (is.null(mm)) return(character())
  assignment <- attr(mm, "assign")
  labels <- attr(stats::terms(fit), "term.labels")
  wanted <- unique(as.character(source_terms))
  label_hits <- which(vapply(labels, function(label) {
    any(vapply(wanted, function(term) {
      .rls_model_terms_equivalent(label, term)
    }, logical(1L)))
  }, logical(1L)))
  setdiff(colnames(mm)[assignment %in% label_hits], "(Intercept)")
}

.rls_model_parent_test_terms <- function(fit, data) {
  labels <- attr(stats::terms(fit), "term.labels")
  xlevels <- tryCatch(fit$xlevels, error = function(e) list()) %||% list()
  labels[vapply(labels, function(term) {
    identical(.rls_model_semantic_term_type(data, term, xlevels), "factor") ||
      length(.rls_model_interaction_parts(term)) >= 2L ||
      length(.rls_model_term_coefficient_names(fit, term)) > 1L
  }, logical(1L))]
}

.rls_model_term_omnibus_tests <- function(fit, source_terms = NULL,
                                           test = c("auto", "wald_chisq", "wald_f")) {
  test <- match.arg(test)
  labels <- attr(stats::terms(fit), "term.labels")
  empty <- data.frame(
    term = character(), coefficient_names = character(), statistic_name = character(),
    statistic = numeric(), df1 = numeric(), df2 = numeric(), p_value = numeric(),
    method = character(), stringsAsFactors = FALSE
  )
  if (!is.null(source_terms)) {
    labels <- labels[vapply(labels, function(label) {
      any(vapply(source_terms, function(term) {
        .rls_model_terms_equivalent(label, term)
      }, logical(1L)))
    }, logical(1L))]
  }
  if (!length(labels)) return(empty)

  estimates <- tryCatch({
    if (inherits(fit, c("betareg", "gamlss", "glmmTMB")) &&
        exists(".rls_glm_mean_coefficients", mode = "function")) {
      .rls_glm_mean_coefficients(fit)
    } else {
      stats::coef(fit)
    }
  }, error = function(e) numeric())
  covariance <- tryCatch({
    if (inherits(fit, c("betareg", "gamlss", "glmmTMB")) &&
        exists(".rls_glm_mean_vcov", mode = "function")) {
      .rls_glm_mean_vcov(fit)
    } else {
      stats::vcov(fit)
    }
  }, error = function(e) NULL)
  coefficient_table <- tryCatch(summary(fit)$coefficients, error = function(e) NULL)
  statistic_column <- if (!is.null(coefficient_table) && ncol(coefficient_table) >= 3L) {
    colnames(coefficient_table)[[3L]]
  } else {
    ""
  }
  uses_f <- if (identical(test, "wald_f")) {
    TRUE
  } else if (identical(test, "wald_chisq")) {
    FALSE
  } else {
    inherits(fit, "lm") && !inherits(fit, "glm") ||
      grepl("^t", statistic_column, ignore.case = TRUE)
  }
  denominator_df <- suppressWarnings(as.numeric(stats::df.residual(fit)))

  rows <- lapply(labels, function(term) {
    coefficient_names <- .rls_model_term_coefficient_names(fit, term)
    estimable <- length(coefficient_names) > 0L &&
      all(coefficient_names %in% names(estimates)) &&
      !is.null(covariance) &&
      all(coefficient_names %in% rownames(covariance)) &&
      all(coefficient_names %in% colnames(covariance))
    statistic <- df1 <- p_value <- NA_real_
    if (estimable) {
      beta <- unname(estimates[coefficient_names])
      variance <- covariance[coefficient_names, coefficient_names, drop = FALSE]
      solution <- if (all(is.finite(beta)) && all(is.finite(variance))) {
        tryCatch(qr.solve(variance, beta), error = function(e) NULL)
      } else {
        NULL
      }
      if (!is.null(solution)) {
        wald <- unname(sum(beta * solution))
        df1 <- length(beta)
        statistic <- if (uses_f) wald / df1 else wald
        p_value <- if (uses_f && is.finite(denominator_df) && denominator_df > 0) {
          stats::pf(statistic, df1 = df1, df2 = denominator_df, lower.tail = FALSE)
        } else if (!uses_f) {
          stats::pchisq(statistic, df = df1, lower.tail = FALSE)
        } else {
          NA_real_
        }
      }
    }
    data.frame(
      term = term,
      coefficient_names = paste(coefficient_names, collapse = "\u001f"),
      statistic_name = if (uses_f) "F" else "Wald chi-square",
      statistic = statistic,
      df1 = df1,
      df2 = if (uses_f) denominator_df else NA_real_,
      p_value = p_value,
      method = if (uses_f) "conditional joint Wald F" else "conditional joint Wald chi-square",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

.rls_empty_global_term_tests <- function() {
  data.frame(
    term = character(), coefficient_names = character(),
    term_test_method = character(), term_test_statistic = numeric(),
    term_test_df = numeric(), term_test_df2 = numeric(), term_test_p = numeric(),
    statistic_name = character(), statistic = numeric(),
    df = numeric(), df1 = numeric(), df2 = numeric(), p_value = numeric(),
    method = character(), component = character(), stringsAsFactors = FALSE
  )
}

.rls_as_global_term_tests <- function(tests) {
  if (!is.data.frame(tests) || !nrow(tests)) return(.rls_empty_global_term_tests())
  value <- function(primary, fallback, default) {
    if (primary %in% names(tests)) tests[[primary]]
    else if (fallback %in% names(tests)) tests[[fallback]]
    else rep(default, nrow(tests))
  }
  method <- as.character(value("term_test_method", "statistic_name", ""))
  statistic <- as.numeric(value("term_test_statistic", "statistic", NA_real_))
  df1 <- as.numeric(value("term_test_df", "df1", NA_real_))
  df2 <- as.numeric(value("term_test_df2", "df2", NA_real_))
  p <- as.numeric(value("term_test_p", "p_value", NA_real_))
  data.frame(
    term = as.character(value("term", "term", "")),
    coefficient_names = as.character(value("coefficient_names", "coefficient_names", "")),
    term_test_method = method,
    term_test_statistic = statistic,
    term_test_df = df1,
    term_test_df2 = df2,
    term_test_p = p,
    statistic_name = method,
    statistic = statistic,
    df = df1,
    df1 = df1,
    df2 = df2,
    p_value = p,
    method = as.character(value("method", "method", method)),
    component = as.character(value("component", "component", "")),
    stringsAsFactors = FALSE
  )
}

.rls_global_term_test_method_label <- function(method) {
  labels <- c(
    "LR chi-square" = "LR \u03C7\u00B2",
    "Wald chi-square" = "Wald \u03C7\u00B2",
    "Rubin Wald chi-square" = "Pooled Wald \u03C7\u00B2",
    "D1/Wald F" = "Pooled F",
    "mice::D1" = "Pooled F",
    "F" = "F"
  )
  method <- as.character(method %||% "")
  if (!nzchar(method)) return("\u2014")
  index <- match(method, names(labels))
  if (is.na(index)) method else unname(labels[[index]])
}

.rls_global_term_test_hierarchy_note <- function(tests) {
  tests <- .rls_as_global_term_tests(tests %||% data.frame())
  if (!nrow(tests)) return("")
  term_parts <- lapply(tests$term, .rls_model_interaction_parts)
  for (index in seq_along(term_parts)) {
    parts <- term_parts[[index]]
    if (!length(parts)) next
    contained <- vapply(term_parts, function(candidate) {
      length(candidate) > length(parts) && all(parts %in% candidate)
    }, logical(1L))
    if (any(contained)) {
      return(paste(
        "Lower-order terms contained in higher-order interactions are tested",
        "conditionally under the current model parameterization; interpret",
        "them together with simple effects or the effect plot."
      ))
    }
  }
  ""
}

.rls_model_formula_without_term <- function(fit, remove_term) {
  terms_object <- stats::terms(fit)
  labels <- attr(terms_object, "term.labels") %||% character()
  index <- which(vapply(labels, .rls_model_terms_equivalent, logical(1L),
                        b = remove_term))
  if (length(index) != 1L) {
    stop(sprintf("Could not identify the complete model term `%s`.", remove_term),
         call. = FALSE)
  }
  formula <- if (length(labels) == 1L) {
    # drop.terms() delegates to reformulate(character(0)) on R 4.4, which is
    # an error.  Removing the last predictor means the ordinary intercept-only
    # model and must remain available for LR/D1 comparisons.
    stats::update.formula(stats::formula(fit), . ~ 1)
  } else {
    stats::formula(stats::drop.terms(
      terms_object, dropx = index, keep.response = TRUE
    ))
  }
  environment(formula) <- environment(stats::formula(fit))
  formula
}

.rls_model_refit_without_term <- function(fit, remove_term, model_data = NULL) {
  reduced_formula <- .rls_model_formula_without_term(fit, remove_term)
  if (is.null(model_data)) {
    model_data <- stats::model.frame(fit)
  }

  # model.frame() stores an offset such as offset(log(exposure)) under its
  # evaluated expression, while the formula still refers to `exposure`.
  # LinkEDA's count models use precisely this log-exposure convention, so
  # reconstruct that analysis-ready column from the fitted offset if needed.
  missing_variables <- setdiff(all.vars(reduced_formula), names(model_data))
  fitted_offset <- stats::model.offset(model_data)
  if (length(missing_variables) == 1L && !is.null(fitted_offset)) {
    model_data[[missing_variables[[1L]]]] <- exp(as.numeric(fitted_offset))
  }

  weights <- stats::model.weights(model_data)
  if (inherits(fit, "negbin")) {
    if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Negative-binomial term tests require the recommended MASS package.",
         call. = FALSE)
    }
    arguments <- list(
      formula = reduced_formula, data = model_data,
      link = fit$family$link, na.action = stats::na.fail
    )
    if (!is.null(weights)) arguments$weights <- weights
    # This is a new glm.nb() fit, not a coefficient deletion from the full
    # model: theta is therefore estimated afresh for every reduced model.
    return(do.call(MASS::glm.nb, arguments))
  }
  # A gamlss fit also inherits from glm and lm, so it must be dispatched
  # before the ordinary glm branch.
  if (inherits(fit, "gamlss")) {
    if (!requireNamespace("gamlss", quietly = TRUE) ||
        !requireNamespace("gamlss.dist", quietly = TRUE)) {
      stop("One-inflated beta term tests require the optional gamlss packages.",
           call. = FALSE)
    }
    if (!identical(as.character(fit$family[[1L]]), "BEOI")) {
      stop("This GAMLSS family does not yet have a verified reduced-model refit.",
           call. = FALSE)
    }
    # The fitted call is normalised by .rls_glm_fit_engine() so that its family
    # is the self-contained expression BEOI(mu.link = ..., nu.link = ...).
    # update() therefore preserves the fitted links and all nuisance-parameter
    # formulas while genuinely refitting the reduced mean model.
    return(stats::update(
      fit, formula = reduced_formula, data = model_data, evaluate = TRUE
    ))
  }
  if (inherits(fit, "glm")) {
    arguments <- list(
      formula = reduced_formula, data = model_data,
      family = fit$family, na.action = stats::na.fail
    )
    if (!is.null(weights)) arguments$weights <- weights
    return(do.call(stats::glm, arguments))
  }
  if (inherits(fit, "betareg")) {
    if (!requireNamespace("betareg", quietly = TRUE)) {
      stop("Beta term tests require the optional betareg package.", call. = FALSE)
    }
    mean_link <- tryCatch(fit$link$mean, error = function(e) NULL) %||% "logit"
    return(betareg::betareg(reduced_formula, data = model_data, link = mean_link))
  }
  # Other likelihood model classes retain a self-contained family expression
  # in their call.  Supplying the immutable fitted model frame prevents a
  # change in scope or missing-data handling during the comparison.
  data <- model_data
  stats::update(fit, formula = reduced_formula, data = data, evaluate = TRUE)
}

.rls_model_likelihood_term_tests <- function(fit, source_terms = NULL,
                                             model_data = NULL) {
  labels <- attr(stats::terms(fit), "term.labels") %||% character()
  if (!is.null(source_terms)) {
    labels <- labels[vapply(labels, function(label) {
      any(vapply(source_terms, .rls_model_terms_equivalent, logical(1L),
                 b = label))
    }, logical(1L))]
  }
  if (!length(labels)) return(.rls_empty_global_term_tests())
  full_log_lik <- tryCatch(stats::logLik(fit), error = function(e) NULL)
  full_n <- tryCatch(stats::nobs(fit), error = function(e) NA_real_)
  if (is.null(full_log_lik) || !is.finite(as.numeric(full_log_lik))) {
    return(.rls_empty_global_term_tests())
  }
  rows <- lapply(labels, function(term) {
    reduced <- tryCatch(
      .rls_model_refit_without_term(fit, term, model_data = model_data),
      error = function(e) e
    )
    reduced_log_lik <- if (!inherits(reduced, "error")) {
      tryCatch(stats::logLik(reduced), error = function(e) NULL)
    } else NULL
    same_rows <- !inherits(reduced, "error") &&
      isTRUE(all.equal(as.numeric(stats::nobs(reduced)), as.numeric(full_n)))
    statistic <- df <- p <- NA_real_
    status <- if (inherits(reduced, "error")) conditionMessage(reduced) else
      if (!same_rows) "Full and reduced models used different observations." else
      if (is.null(reduced_log_lik) || !is.finite(as.numeric(reduced_log_lik)))
        "The reduced model did not provide a finite log likelihood." else ""
    if (!nzchar(status)) {
      statistic <- 2 * (as.numeric(full_log_lik) - as.numeric(reduced_log_lik))
      df <- attr(full_log_lik, "df") - attr(reduced_log_lik, "df")
      tolerance <- sqrt(.Machine$double.eps) * max(1, abs(as.numeric(full_log_lik)))
      if (is.finite(statistic) && statistic < 0 && statistic > -tolerance) statistic <- 0
      if (!is.finite(statistic) || statistic < 0 || !is.finite(df) || df <= 0) {
        status <- "The fitted models did not produce a valid nested likelihood comparison."
        statistic <- df <- NA_real_
      } else {
        p <- stats::pchisq(statistic, df = df, lower.tail = FALSE)
      }
    }
    data.frame(
      term = term,
      coefficient_names = paste(.rls_model_term_coefficient_names(fit, term),
                                collapse = "\u001f"),
      term_test_method = "LR chi-square",
      term_test_statistic = statistic,
      term_test_df = df,
      term_test_df2 = NA_real_,
      term_test_p = p,
      statistic_name = "LR chi-square",
      statistic = statistic,
      df = df,
      df1 = df,
      df2 = NA_real_,
      p_value = p,
      method = if (nzchar(status)) paste("Likelihood-ratio test unavailable:", status)
        else "full-versus-reduced likelihood-ratio test",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

.rls_model_global_term_tests <- function(fit, source_terms = NULL,
                                         likelihood_available = TRUE,
                                         model_data = NULL) {
  # Within-model term inference is based on the fitted coefficient vector and
  # its covariance matrix.  Likelihood-ratio tests are reserved for the
  # explicit model-comparison workflow.
  ordinary_linear <- inherits(fit, "lm") && !inherits(fit, "glm")
  uses_f <- ordinary_linear || !isTRUE(likelihood_available)
  tests <- .rls_model_term_omnibus_tests(
    fit, source_terms, test = if (uses_f) "wald_f" else "wald_chisq"
  )
  if (!is.data.frame(tests) || !nrow(tests)) {
    return(.rls_empty_global_term_tests())
  }
  tests$statistic_name <- if (uses_f) "F" else "Wald chi-square"
  tests$method <- if (ordinary_linear) {
    "partial F test from the fitted linear model"
  } else if (!isTRUE(likelihood_available)) {
    "joint Wald F test (quasi-likelihood)"
  } else {
    "joint Wald chi-square test from the fitted coefficient covariance"
  }
  .rls_as_global_term_tests(tests)
}

.rls_model_apply_parent_term_tests <- function(rows, term_tests) {
  if (!is.data.frame(rows) || !nrow(rows) ||
      !is.data.frame(term_tests) || !nrow(term_tests)) return(rows)
  parent_indices <- which(rows$row_type %in% c("factor_parent", "term_parent"))
  for (row_index in parent_indices) {
    source <- rows$source_term[[row_index]]
    test_index <- which(vapply(term_tests$term, function(term) {
      .rls_model_terms_equivalent(term, source)
    }, logical(1L)))
    if (length(test_index)) {
      test_index <- test_index[[1L]]
      if ("statistic" %in% names(rows)) {
        rows$statistic[[row_index]] <- term_tests$statistic[[test_index]]
      }
      if ("p_value" %in% names(rows)) {
        rows$p_value[[row_index]] <- term_tests$p_value[[test_index]]
      }
      # Keep the model's ordinary statistic-column label (t or z).  The
      # parent-row value is an omnibus statistic whose exact method remains
      # available in term_tests and is explained by the table note/tooltip.
    }
  }
  rows
}

.rls_model_term_delta_r2 <- function(fit) {
  mm <- tryCatch(stats::model.matrix(fit), error = function(e) NULL)
  mf <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  if (is.null(mm) || is.null(mf) || !nrow(mm)) return(setNames(numeric(), character()))
  assignment <- attr(mm, "assign")
  labels <- attr(stats::terms(fit), "term.labels")
  response <- tryCatch(stats::model.response(mf), error = function(e) NULL)
  full_r2 <- tryCatch(unname(summary(fit)$r.squared), error = function(e) NA_real_)
  if (is.null(response) || !length(labels) || length(full_r2) != 1L || !is.finite(full_r2)) {
    return(setNames(rep(NA_real_, length(labels)), labels))
  }
  total_ss <- sum((response - mean(response))^2)
  if (!is.finite(total_ss) || total_ss <= 0) {
    return(setNames(rep(NA_real_, length(labels)), labels))
  }
  values <- vapply(seq_along(labels), function(index) {
    keep <- assignment != index
    reduced <- tryCatch(stats::lm.fit(mm[, keep, drop = FALSE], response), error = function(e) NULL)
    if (is.null(reduced)) return(NA_real_)
    reduced_r2 <- 1 - sum(reduced$residuals^2) / total_ss
    delta <- full_r2 - reduced_r2
    tolerance <- 1e-10 * max(1, abs(full_r2))
    if (!is.finite(delta) || delta < -tolerance) return(NA_real_)
    if (delta < 0) 0 else delta
  }, numeric(1L))
  stats::setNames(values, labels)
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
  polynomial <- .rls_model_polynomial_term(term)
  if (!is.null(polynomial)) {
    variable <- polynomial$variable
    if (!variable %in% names(data)) stop(sprintf("Column `%s` was not found in the dataset.", variable), call. = FALSE)
    if (identical(variable, response)) stop("The dependent variable cannot also be a predictor.", call. = FALSE)
    if (!is.numeric(data[[variable]]) || is.factor(data[[variable]]))
      stop("Polynomial terms require a numeric predictor.", call. = FALSE)
    observed <- data[[variable]][is.finite(data[[variable]])]
    if (length(unique(observed)) <= polynomial$degree) {
      stop(sprintf(
        "Polynomial degree %d for `%s` cannot be estimated from only %d distinct finite values; choose a lower degree or a continuous predictor.",
        polynomial$degree, variable, length(unique(observed))
      ), call. = FALSE)
    }
    return(.rls_model_polynomial_expr(variable, polynomial$degree))
  }
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
  if (!is.null(.rls_model_polynomial_term(term))) return("numeric")
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

.rls_model_numeric_interpretation <- function(x) {
  if (is.numeric(x) || is.logical(x)) {
    return(as.numeric(x))
  }

  labels <- if (is.factor(x)) as.character(x) else x
  # Empty text is a missing value, not a numeric category.  This matters in
  # particular for imported labelled/factor data, where an empty field may
  # otherwise become the first factor code and enter the design matrix as a
  # perfectly valid number.
  blank <- !is.na(labels) & !nzchar(trimws(as.character(labels)))
  labels[blank] <- NA_character_
  parsed <- suppressWarnings(as.numeric(labels))
  present <- !is.na(labels)

  # A factor may simply be a numeric variable imported with the wrong type.
  # Recover its displayed numeric values instead of its internal factor codes.
  if (!any(present & is.na(parsed))) {
    return(parsed)
  }

  # Binary categories have a meaningful deterministic 0/1 conversion.  Do
  # not extend that convenience to a nominal variable with three or more
  # categories: internal factor codes would invent an unsupported metric.
  declared_levels <- if (is.factor(x)) as.character(levels(x)) else
    unique(as.character(labels[present]))
  declared_levels <- declared_levels[
    !is.na(declared_levels) & nzchar(trimws(declared_levels))
  ]
  if (length(declared_levels) == 2L) {
    binary_mapping <- stats::setNames(c(0, 1), declared_levels)
    return(unname(binary_mapping[as.character(labels)]))
  }
  stop(
    "A non-binary Categorical variable cannot be treated as Numeric without an explicit global numeric mapping.",
    call. = FALSE
  )
}

.rls_model_factor_interpretation <- function(x) {
  value_labels <- .rls_variable_value_labels(x)
  if (length(value_labels)) {
    raw <- unclass(x)
    attributes(raw) <- NULL
    labels <- as.character(raw)
    labelled_values <- unclass(value_labels)
    attributes(labelled_values) <- NULL
    declared_levels <- names(value_labels)
    if (is.null(declared_levels)) declared_levels <- rep("", length(value_labels))
    missing_names <- is.na(declared_levels) | !nzchar(declared_levels)
    declared_levels[missing_names] <- as.character(labelled_values)[missing_names]
    for (index in seq_along(labelled_values)) {
      matches <- !is.na(raw) & !is.na(labelled_values[[index]]) &
        raw == labelled_values[[index]]
      labels[!is.na(matches) & matches] <- declared_levels[[index]]
    }
  } else {
    labels <- as.character(x)
    declared_levels <- if (is.factor(x)) as.character(levels(x)) else character()
  }
  # R treats "" and whitespace as ordinary factor levels.  Imported missing
  # values encoded that way would consequently produce an anonymous
  # coefficient row.  Model semantics instead treat them like NA so that
  # complete-case handling excludes them (or, for MI, uses the completed value
  # when one is actually present).
  blank <- !is.na(labels) & !nzchar(trimws(labels))
  labels[blank] <- NA_character_

  declared_levels <- declared_levels[
    !is.na(declared_levels) & nzchar(trimws(declared_levels))
  ]
  observed_levels <- if (is.factor(x)) {
    unique(labels[!is.na(labels)])
  } else {
    # Match base R's stable treatment coding for a numeric/character column
    # that the model interprets as categorical.  Using order of appearance
    # makes the reference depend on row order (for example mtcars$cyl would
    # start at 6 rather than 4) and can silently change coefficients after a
    # scope or selection edit.
    levels(factor(labels))
  }

  # Regression term semantics offer model-local categorical treatment.
  # Preserve a factor/ordered variable's declared order, but remove the ordered
  # class so treatment contrasts and an explicit reference remain available.
  factor(labels, levels = unique(c(declared_levels, observed_levels)), ordered = FALSE)
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
        data[[variable]] <- .rls_model_factor_interpretation(data[[variable]])
      } else if (identical(type, "numeric")) {
        data[[variable]] <- .rls_model_numeric_interpretation(data[[variable]])
      }
    }
  }
  data
}

.rls_model_formula_object <- function(response, terms) {
  if (!nzchar(response)) {
    stop("Choose a dependent variable before fitting the model.", call. = FALSE)
  }
  terms <- as.character(terms %||% character())
  formula_terms <- if (length(terms)) terms else "1"
  predictor_variables <- all.vars(stats::reformulate(formula_terms))
  if (response %in% predictor_variables) {
    stop("The dependent variable cannot also be a predictor.", call. = FALSE)
  }
  stats::reformulate(formula_terms, response = as.name(response))
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
  scoped_data <- data[ok, , drop = FALSE]
  if (nrow(scoped_data)) {
    predictor_variables <- unique(all.vars(stats::delete.response(stats::terms(formula))))
    predictor_variables <- intersect(predictor_variables, names(scoped_data))
    factor_variables <- predictor_variables[vapply(
      scoped_data[predictor_variables],
      function(values) is.factor(values) || is.ordered(values),
      logical(1L)
    )]
    observed_levels <- lapply(factor_variables, function(variable) {
      unique(as.character(scoped_data[[variable]][!is.na(scoped_data[[variable]])]))
    })
    names(observed_levels) <- factor_variables
    without_variability <- names(observed_levels)[lengths(observed_levels) < 2L]
    if (length(without_variability)) {
      details <- vapply(without_variability, function(variable) {
        levels <- observed_levels[[variable]]
        observed <- if (length(levels)) sprintf("`%s`", levels[[1L]]) else "no observed category"
        sprintf(
          "categorical predictor `%s` has no variability in this subset: it has only one observed category (%s)",
          variable, observed
        )
      }, character(1L))
      scope_label <- switch(
        scope,
        selected = "Selected rows",
        unselected = "Unselected rows",
        "All data"
      )
      stop(sprintf(
        "Cannot fit the model for %s because %s. Select more rows, choose All data, or remove the categorical predictor from the model.",
        scope_label, paste(details, collapse = "; ")
      ), call. = FALSE)
    }
  }
  list(data = scoped_data, rows = which(ok), excluded = sum(!ok))
}

.rls_model_factor_codings <- function(fit) {
  xlevels <- tryCatch(fit$xlevels, error = function(e) NULL) %||% list()
  if (!length(xlevels)) {
    xlevels <- tryCatch(
      .getXlevels(stats::terms(fit), stats::model.frame(fit)),
      error = function(e) list()
    )
  }
  fit_contrasts <- tryCatch(fit$contrasts, error = function(e) NULL) %||% list()
  out <- list()
  for (variable in names(xlevels)) {
    levels <- as.character(xlevels[[variable]])
    if (!length(levels)) next
    specification <- fit_contrasts[[variable]]
    coding <- tryCatch({
      if (is.matrix(specification)) {
        specification
      } else if (is.character(specification) && length(specification) == 1L) {
        contrast_function <- get(specification, mode = "function")
        contrast_function(length(levels), contrasts = TRUE, sparse = FALSE)
      } else {
        stats::contrasts(factor(levels, levels = levels), contrasts = TRUE)
      }
    }, error = function(e) NULL)
    if (is.null(coding)) next
    coding <- as.matrix(coding)
    if (nrow(coding) != length(levels)) next
    rownames(coding) <- levels
    if (is.null(colnames(coding))) {
      colnames(coding) <- paste0("C", seq_len(ncol(coding)))
    }
    zero_rows <- which(apply(coding, 1L, function(values) {
      all(is.finite(values)) && all(abs(values) < sqrt(.Machine$double.eps))
    }))
    reference <- if (length(zero_rows) == 1L) levels[[zero_rows]] else NA_character_
    coefficient_levels <- rep.int(NA_character_, ncol(coding))
    for (column in seq_len(ncol(coding))) {
      hits <- which(abs(coding[, column] - 1) < sqrt(.Machine$double.eps))
      if (length(hits) == 1L &&
          all(abs(coding[-hits, column]) < sqrt(.Machine$double.eps))) {
        coefficient_levels[[column]] <- levels[[hits]]
      }
    }
    out[[variable]] <- list(
      variable = variable,
      levels = levels,
      reference = reference,
      contrast_labels = colnames(coding),
      coefficient_levels = coefficient_levels,
      coding = coding
    )
  }
  out
}

.rls_model_factor_coding_for <- function(codings, term) {
  if (!length(codings)) return(NULL)
  candidates <- unique(c(term, .rls_model_term_display_name(term),
                         .rls_model_clean_factor_call(term)))
  for (candidate in candidates) {
    if (!is.null(codings[[candidate]])) return(codings[[candidate]])
  }
  NULL
}

.rls_model_coefficient_row <- function(row_type, term, display_label, source_term = term,
                                       term_type = "numeric", level = NA_character_,
                                       reference_level = NA_character_,
                                       coefficient_name = NA_character_,
                                       coefficient = NULL, statistic_name = "t",
                                       delta_r2 = NA_real_) {
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
    ci_lower = value("ci_lower"),
    ci_upper = value("ci_upper"),
    exponentiated_estimate = value("exponentiated_estimate"),
    exponentiated_lower = value("exponentiated_lower"),
    exponentiated_upper = value("exponentiated_upper"),
    partial_r = value("partial_r"),
    standardized_beta = value("standardized_beta"),
    delta_r2 = delta_r2,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.rls_model_coefficient_display_rows <- function(fit, coefficients, data, requested_terms = NULL,
                                                statistic_name = "t", term_delta_r2 = NULL) {
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
  if (is.null(term_delta_r2)) term_delta_r2 <- .rls_model_term_delta_r2(fit)
  delta_for <- function(term_label) {
    candidates <- unique(c(term_label, .rls_model_term_display_name(term_label)))
    for (candidate in candidates) {
      if (candidate %in% names(term_delta_r2)) return(unname(term_delta_r2[[candidate]]))
    }
    NA_real_
  }
  factor_codings <- .rls_model_factor_codings(fit)

  for (i in seq_along(term_labels)) {
    term_label <- term_labels[[i]]
    requested <- requested_terms[vapply(requested_terms, .rls_model_terms_equivalent,
                                        logical(1L), b = term_label)]
    display_term <- .rls_model_term_display_name(if (length(requested)) requested[[1L]] else term_label)
    term_type <- .rls_model_semantic_term_type(data, term_label, xlevels)
    term_columns <- mm_names[assign == i]
    term_columns <- setdiff(term_columns, "(Intercept)")

    if (length(.rls_model_interaction_parts(term_label)) >= 2L && length(term_columns)) {
      # R may canonicalize the fitted term according to main-effect order.
      # Keep that order for model-matrix lookup, and the requested order for labels.
      parts <- .rls_model_interaction_parts(term_label)
      display_parts <- .rls_model_interaction_parts(display_term)
      display_indices <- match(display_parts, parts)
      interaction_label <- paste(
        vapply(display_parts, .rls_model_term_display_name, character(1L)),
        collapse = " \u00d7 "
      )
      append_row(.rls_model_coefficient_row(
        "term_parent", display_term, interaction_label, display_term, term_type,
        statistic_name = statistic_name, delta_r2 = delta_for(term_label)
      ))
      part_codings <- lapply(parts, function(part) {
        .rls_model_factor_coding_for(factor_codings, part)
      })
      factor_indices <- which(!vapply(part_codings, is.null, logical(1L)))
      numeric_indices <- setdiff(seq_along(parts), factor_indices)
      factor_display_order <- match(display_indices[display_indices %in% factor_indices], factor_indices)
      coding <- if (length(factor_indices) == 1L) part_codings[[factor_indices]] else NULL
      semantic_numeric_factor <- length(factor_indices) == 1L &&
        length(numeric_indices) == 1L && !is.null(coding) &&
        length(term_columns) == length(coding$coefficient_levels) &&
        !is.na(coding$reference) &&
        all(!is.na(coding$coefficient_levels))
      semantic_factor_interaction <- length(factor_indices) >= 2L &&
        all(vapply(part_codings[factor_indices], function(part_coding) {
          !is.null(part_coding) &&
            length(part_coding$coefficient_levels) > 0L &&
            all(!is.na(part_coding$coefficient_levels)) &&
            !is.na(part_coding$reference)
        }, logical(1L)))
      factor_combinations <- NULL
      if (semantic_factor_interaction) {
        factor_combinations <- expand.grid(
          lapply(part_codings[factor_indices], `[[`, "coefficient_levels"),
          KEEP.OUT.ATTRS = FALSE,
          stringsAsFactors = FALSE
        )
        if (nrow(factor_combinations) != length(term_columns)) {
          factor_combinations <- NULL
        }
      }
      for (column_index in seq_along(term_columns)) {
        coef_name <- term_columns[[column_index]]
        combination_levels <- if (!is.null(factor_combinations)) {
          as.character(factor_combinations[column_index, , drop = TRUE])
        } else character()
        combination_references <- if (!is.null(factor_combinations)) {
          vapply(part_codings[factor_indices], `[[`, character(1L), "reference")
        } else character()
        level <- if (semantic_numeric_factor) {
          coding$coefficient_levels[[column_index]]
        } else if (length(combination_levels)) {
          paste(combination_levels[factor_display_order], collapse = " \u00d7 ")
        } else NA_character_
        reference <- if (semantic_numeric_factor) {
          coding$reference
        } else if (length(combination_references)) {
          paste(combination_references[factor_display_order], collapse = " \u00d7 ")
        } else NA_character_
        child_label <- if (semantic_numeric_factor) {
          paste0(
            "  ", paste(vapply(display_indices, function(index) {
              name <- .rls_model_term_display_name(parts[[index]])
              if (index %in% factor_indices) paste0(name, " = ", level) else name
            }, character(1L)), collapse = " \u00d7 "),
            " (vs ", reference, ")"
          )
        } else if (!is.null(factor_combinations)) {
          all_factor <- length(factor_indices) == length(parts)
          components <- if (all_factor) combination_levels[factor_display_order] else {
            vapply(display_indices, function(part_index) {
              factor_position <- match(part_index, factor_indices)
              if (!is.na(factor_position)) {
                paste0(.rls_model_term_display_name(parts[[part_index]]),
                       " = ", combination_levels[[factor_position]])
              } else {
                .rls_model_term_display_name(parts[[part_index]])
              }
            }, character(1L))
          }
          paste0(
            "  ", paste(components, collapse = " \u00d7 "),
            " (vs ", paste(combination_references[factor_display_order], collapse = " \u00d7 "), ")"
          )
        } else {
          paste0("  ", coef_name)
        }
        append_row(.rls_model_coefficient_row(
          "coefficient", coef_name, child_label, display_term, term_type,
          level = level, reference_level = reference,
          coefficient_name = coef_name, coefficient = coefficient_for(coef_name),
          statistic_name = statistic_name
        ))
      }
      next
    }

    if (identical(term_type, "factor")) {
      coding <- .rls_model_factor_coding_for(factor_codings, term_label)
      if (!is.null(coding)) {
        levels <- coding$levels
      } else {
        levels <- character()
      }
      if (length(levels) >= 2L && length(term_columns) == length(levels) - 1L &&
          !is.na(coding$reference) && all(!is.na(coding$coefficient_levels))) {
        reference <- coding$reference
        append_row(.rls_model_coefficient_row(
          "factor_parent", display_term, display_term, display_term, "factor",
          reference_level = reference, statistic_name = statistic_name,
          delta_r2 = delta_for(term_label)
        ))
        append_row(.rls_model_coefficient_row(
          "reference", paste(display_term, reference, sep = "="), paste0("  ", reference),
          display_term, "factor", level = reference, reference_level = reference,
          statistic_name = statistic_name
        ))
        for (level_index in seq_along(term_columns)) {
          level <- coding$coefficient_levels[[level_index]]
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
        statistic_name = statistic_name, delta_r2 = delta_for(term_label)
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
        statistic_name = statistic_name, delta_r2 = delta_for(term_label)
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
    "Partial r = ", .rls_regcmp_format_number(row$partial_r[[1L]], 3L), "; ",
    "\u0394R\u00b2 = ", .rls_regcmp_format_number(row$delta_r2[[1L]], 3L)
  )
}
