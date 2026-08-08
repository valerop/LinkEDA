.rls_mixed_models <- new.env(parent = emptyenv())

.rls_mixed_model_id <- function(group, prefix, name = NULL) {
  .rls_validate_protocol_name(
    name %||% paste0(prefix, ":", group, ":", format(Sys.time(), "%Y%m%d%H%M%OS3")),
    "name"
  )
}

.rls_assign_mixed_model <- function(record, class) {
  assign(record$id, record, envir = .rls_mixed_models)
  structure(list(id = record$id, group = record$group), class = class)
}

.rls_mixed_model_record <- function(model) {
  id <- if (inherits(model, c("rlispstat_linear_mixed_model", "rlispstat_generalized_mixed_model"))) model$id else model
  id <- .rls_validate_protocol_name(id, "model")
  if (!exists(id, envir = .rls_mixed_models, inherits = FALSE)) {
    stop("Unknown mixed model.", call. = FALSE)
  }
  get(id, envir = .rls_mixed_models)
}

.rls_require_lme4 <- function() {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Mixed models require the lme4 package.\nInstall it with install.packages(\"lme4\") and try again.", call. = FALSE)
  }
  invisible(TRUE)
}

.rls_mixed_bt <- function(x) {
  paste0("`", gsub("`", "``", x, fixed = TRUE), "`")
}

.rls_mixed_term_expr <- function(term) {
  term <- trimws(as.character(term))
  if (grepl(":", term, fixed = TRUE)) {
    return(paste(vapply(strsplit(term, ":", fixed = TRUE)[[1L]], .rls_mixed_term_expr, character(1L)), collapse = ":"))
  }
  if (identical(term, "1") || identical(term, "0")) return(term)
  if (grepl("^factor\\(([^()]+)\\)$", term)) {
    variable <- .rls_model_clean_factor_call(term)
    return(paste0("factor(", .rls_mixed_bt(variable), ")"))
  }
  .rls_mixed_bt(term)
}

.rls_mixed_validate_fixed <- function(data, fixed, response) {
  fixed <- unique(vapply(fixed %||% character(), .rls_model_validate_term, character(1L),
                         data = data, response = response, what = "fixed"))
  fixed
}

.rls_mixed_validate_group <- function(data, group) {
  group <- .rls_validate_protocol_name(group, "group variable")
  if (!group %in% names(data)) {
    stop(sprintf("Column `%s` was not found in the dataset.", group), call. = FALSE)
  }
  x <- data[[group]]
  observed <- x[!is.na(x)]
  n_levels <- length(unique(observed))
  if (n_levels < 2L) {
    stop(sprintf("Grouping variable `%s` has fewer than 2 observed levels.", group), call. = FALSE)
  }
  if (is.numeric(x) && n_levels > max(20L, ceiling(length(observed) / 4))) {
    stop(sprintf("Variable '%s' is numeric with many unique values. Treat it as categorical or choose another grouping variable.", group), call. = FALSE)
  }
  group
}

.rls_mixed_validate_random_term <- function(data, term, response) {
  term <- trimws(.rls_validate_protocol_name(term, "random term"))
  if (term %in% c("1", "0")) return(term)
  .rls_model_validate_term(data, term, response = response, what = "random term")
}

.rls_mixed_normalize_random <- function(data, random, response) {
  if (is.null(random) || !length(random)) {
    stop("Mixed models require at least one random effect.", call. = FALSE)
  }
  if (is.character(random)) {
    random <- lapply(random, function(group) list(group = group, terms = "1"))
  }
  if (!is.list(random)) {
    stop("`random` must be a list of random-effect specifications.", call. = FALSE)
  }
  out <- lapply(random, function(spec) {
    if (is.character(spec) && length(spec) == 1L) {
      spec <- list(group = spec, terms = "1")
    }
    if (!is.list(spec) || is.null(spec$group)) {
      stop("Each random-effect specification must contain a `group` variable.", call. = FALSE)
    }
    group <- .rls_mixed_validate_group(data, spec$group)
    terms <- spec$terms %||% "1"
    terms <- unique(vapply(terms, .rls_mixed_validate_random_term, character(1L), data = data, response = response))
    covariance <- spec$covariance_structure %||% spec$covariance %||% "|"
    covariance <- match.arg(covariance, c("|", "||"))
    list(group = group, terms = terms, covariance_structure = covariance)
  })
  names(out) <- NULL
  out
}

.rls_mixed_random_formula_parts <- function(random) {
  vapply(random, function(spec) {
    terms <- paste(vapply(spec$terms, .rls_mixed_term_expr, character(1L)), collapse = " + ")
    paste0("(", terms, " ", spec$covariance_structure, " ", .rls_mixed_bt(spec$group), ")")
  }, character(1L))
}

.rls_mixed_formula_string <- function(response, fixed, random) {
  rhs <- c(vapply(fixed, .rls_mixed_term_expr, character(1L)), .rls_mixed_random_formula_parts(random))
  if (!length(rhs)) rhs <- "1"
  paste(.rls_mixed_bt(response), "~", paste(rhs, collapse = " + "))
}

.rls_mixed_formula_object <- function(record) {
  stats::as.formula(record$formula)
}

.rls_mixed_complete_data <- function(record) {
  .rls_model_complete_data(record$data, .rls_mixed_formula_object(record),
                           record$scope %||% "all", record$selected_rows %||% integer())
}

.rls_mixed_prepare_data <- function(data, random) {
  for (spec in random) {
    data[[spec$group]] <- as.factor(data[[spec$group]])
  }
  data
}

.rls_mixed_group_summary <- function(data, random) {
  groups <- unique(vapply(random, `[[`, character(1L), "group"))
  data.frame(
    group = groups,
    levels = vapply(groups, function(group) length(unique(data[[group]][!is.na(data[[group]])])), integer(1L)),
    stringsAsFactors = FALSE
  )
}

.rls_mixed_group_warnings <- function(data, random) {
  warnings <- character()
  for (spec in random) {
    levels <- length(unique(data[[spec$group]][!is.na(data[[spec$group]])]))
    if (levels < 2L) {
      stop(sprintf(
        "Grouping variable '%s' has only one observed level after missing-value filtering.",
        spec$group
      ), call. = FALSE)
    }
    if (levels < 3L) {
      warnings <- c(warnings, sprintf(
        "Grouping variable '%s' has fewer than 3 levels. Random-effect estimates may be unstable.",
        spec$group
      ))
    }
  }
  unique(warnings)
}

.rls_mixed_random_effect_table <- function(fit) {
  vc <- as.data.frame(lme4::VarCorr(fit), stringsAsFactors = FALSE)
  if (!nrow(vc)) {
    return(data.frame(group = character(), effect = character(), variance = numeric(), sd = numeric(), corr = numeric(), stringsAsFactors = FALSE))
  }
  effect <- ifelse(is.na(vc$var2), ifelse(is.na(vc$var1), .rls_em_dash, vc$var1), paste(vc$var1, vc$var2, sep = ":"))
  effect[effect == "(Intercept)"] <- "Intercept"
  data.frame(
    group = vc$grp,
    effect = effect,
    variance = ifelse(is.na(vc$var2), vc$vcov, NA_real_),
    sd = ifelse(is.na(vc$var2), vc$sdcor, NA_real_),
    corr = ifelse(is.na(vc$var2), NA_real_, vc$sdcor),
    stringsAsFactors = FALSE
  )
}

.rls_mixed_fixed_table <- function(fit, data, fixed, statistic_name, lmm_p_values) {
  coef_matrix <- as.data.frame(unclass(summary(fit)$coefficients), stringsAsFactors = FALSE)
  if (!nrow(coef_matrix)) return(data.frame())
  names_lower <- tolower(names(coef_matrix))
  estimate <- coef_matrix[[grep("^estimate$", names_lower)[[1L]]]]
  std_error <- coef_matrix[[grep("std.*error", names_lower)[[1L]]]]
  stat_col <- grep("(^t value$|^z value$|t value|z value)", names_lower)
  statistic <- if (length(stat_col)) coef_matrix[[stat_col[[1L]]]] else NA_real_
  df_col <- grep("^df$|denom", names_lower)
  p_col <- grep("pr\\(>|p.value|p value", names_lower)
  p_value <- if (length(p_col)) coef_matrix[[p_col[[1L]]]] else rep(NA_real_, nrow(coef_matrix))
  if (!isTRUE(lmm_p_values)) p_value[] <- NA_real_
  df <- if (length(df_col)) coef_matrix[[df_col[[1L]]]] else rep(NA_real_, nrow(coef_matrix))
  coefficients <- data.frame(
    term = rownames(coef_matrix),
    estimate = unname(estimate),
    std_error = unname(std_error),
    statistic = unname(statistic),
    df = unname(df),
    p_value = unname(p_value),
    partial_r2 = NA_real_,
    stringsAsFactors = FALSE
  )
  rows <- .rls_model_coefficient_display_rows(
    fit, coefficients, data, requested_terms = fixed, statistic_name = statistic_name
  )
  rows$df <- NA_real_
  for (i in seq_len(nrow(rows))) {
    coef_name <- rows$coefficient_name[[i]]
    hit <- match(coef_name, coefficients$term)
    if (!is.na(hit)) rows$df[[i]] <- coefficients$df[[hit]]
  }
  rows
}

.rls_mixed_diagnostics <- function(record, fit, complete, generalized = FALSE) {
  response <- all.vars(stats::formula(fit))[[1L]]
  fitted <- unname(stats::fitted(fit))
  data.frame(
    row_id = complete$rows,
    original_row_id = record$original_row_ids[complete$rows],
    observed = complete$data[[response]],
    fitted = fitted,
    residual = unname(stats::residuals(fit)),
    pearson_residual = if (isTRUE(generalized)) unname(stats::residuals(fit, type = "pearson")) else NA_real_,
    deviance_residual = if (isTRUE(generalized)) unname(stats::residuals(fit, type = "deviance")) else NA_real_,
    stringsAsFactors = FALSE
  )
}

.rls_mixed_convergence <- function(fit) {
  opt <- fit@optinfo
  messages <- c(opt$conv$lme4$messages, opt$warnings)
  messages <- unique(as.character(messages[!is.na(messages) & nzchar(messages)]))
  list(ok = !length(messages), messages = messages)
}

.rls_fit_linear_mixed_model_record <- function(record) {
  .rls_require_lme4()
  complete <- .rls_mixed_complete_data(record)
  if (nrow(complete$data) <= length(record$fixed_effects) + 1L) {
    stop("Not enough complete cases to fit the linear mixed model.", call. = FALSE)
  }
  fit_data <- .rls_mixed_prepare_data(complete$data, record$random_effects)
  warnings <- .rls_mixed_group_warnings(fit_data, record$random_effects)
  use_lmer_test <- requireNamespace("lmerTest", quietly = TRUE)
  fit <- withCallingHandlers(
    tryCatch({
      lme4::lmer(.rls_mixed_formula_object(record), data = fit_data, REML = identical(record$method, "REML"))
    }, error = function(e) stop(sprintf("Linear mixed model could not be fitted: %s", conditionMessage(e)), call. = FALSE)),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  if (!use_lmer_test) {
    warnings <- c(warnings, "Approximate p-values for linear mixed models require lmerTest.")
  }
  singular <- isTRUE(lme4::isSingular(fit, tol = 1e-4))
  if (singular) warnings <- c(warnings, "Warning: singular fit. Some random-effect variances are estimated near zero.")
  conv <- .rls_mixed_convergence(fit)
  if (!conv$ok) warnings <- c(warnings, "Warning: model failed to converge. Review optimizer diagnostics.", conv$messages)
  summary_fit <- summary(fit)
  fixed_fit <- if (use_lmer_test) {
    tryCatch(lmerTest::as_lmerModLmerTest(fit), error = function(e) fit)
  } else {
    fit
  }
  record$model_object <- fit
  record$fixed_effect_table <- .rls_mixed_fixed_table(fixed_fit, fit_data, record$fixed_effects, "t", use_lmer_test)
  record$random_effect_table <- .rls_mixed_random_effect_table(fit)
  record$group_summary <- .rls_mixed_group_summary(fit_data, record$random_effects)
  record$fit_statistics <- list(
    n_used = length(complete$rows),
    n_excluded = complete$excluded,
    method = record$method,
    reml_criterion = if (identical(record$method, "REML")) unname(lme4::REMLcrit(fit)) else NA_real_,
    log_lik = as.numeric(stats::logLik(fit)),
    aic = stats::AIC(fit),
    bic = stats::BIC(fit),
    sigma = unname(summary_fit$sigma),
    df_residual = stats::df.residual(fit)
  )
  record$diagnostic_data <- .rls_mixed_diagnostics(record, fit, complete, generalized = FALSE)
  record$rows_used_original_ids <- record$original_row_ids[complete$rows]
  record$rows_excluded_original_ids <- record$original_row_ids[setdiff(seq_len(nrow(record$data)), complete$rows)]
  record$warnings <- unique(warnings)
  record$convergence_status <- if (conv$ok) "converged" else "failed"
  record$singular_fit <- singular
  record$fit_version <- record$fit_version + 1L
  record$status <- sprintf("Linear mixed model fitted on %d rows.", length(complete$rows))
  record
}

.rls_fit_generalized_mixed_model_record <- function(record) {
  .rls_require_lme4()
  complete <- .rls_mixed_complete_data(record)
  if (nrow(complete$data) <= length(record$fixed_effects) + 1L) {
    stop("Not enough complete cases to fit the generalized mixed model.", call. = FALSE)
  }
  fit_data <- .rls_mixed_prepare_data(complete$data, record$random_effects)
  warnings <- .rls_mixed_group_warnings(fit_data, record$random_effects)
  family_object <- .rls_glm_make_family(record$family, record$link)
  record$link <- family_object$link
  fit <- withCallingHandlers(
    tryCatch(
      lme4::glmer(.rls_mixed_formula_object(record), data = fit_data, family = family_object),
      error = function(e) stop(sprintf("Generalized mixed model could not be fitted: %s", conditionMessage(e)), call. = FALSE)
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  singular <- isTRUE(lme4::isSingular(fit, tol = 1e-4))
  if (singular) warnings <- c(warnings, "Warning: singular fit. Some random-effect variances are estimated near zero.")
  conv <- .rls_mixed_convergence(fit)
  if (!conv$ok) warnings <- c(warnings, "Warning: generalized mixed model failed to converge. Review random-effects structure or optimizer settings.", conv$messages)
  record$model_object <- fit
  record$fixed_effect_table <- .rls_mixed_fixed_table(fit, fit_data, record$fixed_effects, "z", TRUE)
  record$random_effect_table <- .rls_mixed_random_effect_table(fit)
  record$group_summary <- .rls_mixed_group_summary(fit_data, record$random_effects)
  record$fit_statistics <- list(
    n_used = length(complete$rows),
    n_excluded = complete$excluded,
    family = record$family,
    link = record$link,
    log_lik = as.numeric(stats::logLik(fit)),
    aic = stats::AIC(fit),
    bic = stats::BIC(fit),
    deviance = stats::deviance(fit),
    df_residual = stats::df.residual(fit)
  )
  record$diagnostic_data <- .rls_mixed_diagnostics(record, fit, complete, generalized = TRUE)
  record$rows_used_original_ids <- record$original_row_ids[complete$rows]
  record$rows_excluded_original_ids <- record$original_row_ids[setdiff(seq_len(nrow(record$data)), complete$rows)]
  record$warnings <- unique(warnings)
  record$convergence_status <- if (conv$ok) "converged" else "failed"
  record$singular_fit <- singular
  record$fit_version <- record$fit_version + 1L
  record$status <- sprintf("Generalized mixed model fitted on %d rows.", length(complete$rows))
  record
}

.rls_new_mixed_model_record <- function(data, response, fixed, random, model_type, method = NULL,
                                        family = NULL, link = NULL, scope = "all", name = NULL,
                                        .selected_rows = NULL) {
  scope <- match.arg(scope, c("all", "selected", "unselected"))
  if (is.data.frame(data)) {
    group <- .rls_register_dataset(name %||% model_type, data, activate = TRUE)
    dataset <- .rls_dataset_record(group)
  } else {
    dataset <- .rls_dataset_record(data)
  }
  response <- response %||% names(dataset$data)[[1L]]
  response <- .rls_model_validate_response(dataset$data, response, "response")
  fixed <- .rls_mixed_validate_fixed(dataset$data, fixed, response)
  random <- .rls_mixed_normalize_random(dataset$data, random, response)
  formula <- .rls_mixed_formula_string(response, fixed, random)
  id <- .rls_mixed_model_id(dataset$group, if (identical(model_type, "linear_mixed_model")) "lmm" else "glmm", name)
  list(
    id = id,
    model_id = id,
    model_type = model_type,
    dataset_id = dataset$group,
    dataset_name = dataset$group,
    group = dataset$group,
    data = dataset$data,
    original_row_ids = dataset$original_row_ids %||% seq_len(nrow(dataset$data)),
    response = response,
    fixed_effects = fixed,
    random_effects = random,
    formula = formula,
    family = family,
    link = link,
    method = method,
    scope = scope,
    selected_rows = if (!is.null(.selected_rows)) as.integer(.selected_rows) else
      if (scope != "all") ls_selected(dataset$group) else integer(),
    model_object = NULL,
    fixed_effect_table = data.frame(),
    random_effect_table = data.frame(),
    group_summary = data.frame(),
    fit_statistics = list(),
    diagnostic_data = data.frame(),
    rows_used_original_ids = integer(),
    rows_excluded_original_ids = integer(),
    warnings = character(),
    convergence_status = "not fitted",
    singular_fit = NA,
    fit_version = 0L,
    created_at = Sys.time(),
    status = "Not fitted."
  )
}

.rls_mixed_value <- function(x, digits = 3L) {
  .rls_export_format_number(x %||% NA_real_, digits)
}

.rls_mixed_p_value <- function(x) {
  .rls_export_format_p(x %||% NA_real_)
}

.rls_mixed_title <- function(record) {
  if (identical(record$model_type, "generalized_linear_mixed_model")) {
    "Generalized Linear Mixed Model"
  } else {
    "Linear Mixed Model"
  }
}

.rls_mixed_fixed_display <- function(record) {
  rows <- record$fixed_effect_table
  stat_name <- if (identical(record$model_type, "generalized_linear_mixed_model")) "z" else "t"
  if (!nrow(rows)) {
    out <- data.frame(Variable = character(), Type = character(), b = character(), SE = character(),
                      stringsAsFactors = FALSE, check.names = FALSE)
    out[[stat_name]] <- character()
    if (!identical(record$model_type, "generalized_linear_mixed_model")) out[["df"]] <- character()
    out[["p"]] <- character()
    return(out)
  }
  type_for <- function(row_type, term_type) {
    if (identical(row_type, "factor_parent")) return("Factor")
    if (identical(row_type, "term_parent")) return("Term")
    if (identical(row_type, "coefficient") && identical(term_type, "numeric")) return("Numeric")
    if (identical(row_type, "coefficient") && identical(term_type, "intercept")) return(.rls_em_dash)
    if (identical(row_type, "coefficient") && identical(term_type, "factor")) return("Factor")
    ""
  }
  value_or_dash <- function(row, name, digits = 3L) {
    if (row$row_type %in% c("factor_parent", "term_parent", "reference")) return(.rls_em_dash)
    .rls_mixed_value(row[[name]], digits)
  }
  out <- data.frame(
    Variable = rows$display_label,
    Type = mapply(type_for, rows$row_type, rows$term_type, USE.NAMES = FALSE),
    b = vapply(seq_len(nrow(rows)), function(i) value_or_dash(rows[i, , drop = FALSE], "estimate", 4L), character(1L)),
    SE = vapply(seq_len(nrow(rows)), function(i) value_or_dash(rows[i, , drop = FALSE], "std_error", 4L), character(1L)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out[[stat_name]] <- vapply(seq_len(nrow(rows)), function(i) value_or_dash(rows[i, , drop = FALSE], "statistic", 3L), character(1L))
  if (!identical(record$model_type, "generalized_linear_mixed_model")) {
    out[["df"]] <- vapply(seq_len(nrow(rows)), function(i) value_or_dash(rows[i, , drop = FALSE], "df", 1L), character(1L))
  }
  out[["p"]] <- vapply(seq_len(nrow(rows)), function(i) {
    if (rows$row_type[[i]] %in% c("factor_parent", "term_parent", "reference")) return(.rls_em_dash)
    .rls_mixed_p_value(rows$p_value[[i]])
  }, character(1L))
  out
}

.rls_mixed_random_display <- function(record) {
  rows <- record$random_effect_table
  if (!nrow(rows)) {
    return(data.frame(Group = character(), Effect = character(), Variance = character(),
                      SD = character(), Corr = character(), stringsAsFactors = FALSE,
                      check.names = FALSE))
  }
  data.frame(
    Group = rows$group,
    Effect = rows$effect,
    Variance = vapply(rows$variance, .rls_mixed_value, character(1L), digits = 4L),
    SD = vapply(rows$sd, .rls_mixed_value, character(1L), digits = 4L),
    Corr = vapply(rows$corr, .rls_mixed_value, character(1L), digits = 3L),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.rls_mixed_group_display <- function(record) {
  groups <- record$group_summary
  if (!nrow(groups)) {
    return(data.frame(Group = character(), Levels = character(), stringsAsFactors = FALSE,
                      check.names = FALSE))
  }
  data.frame(
    Group = groups$group,
    Levels = as.character(groups$levels),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.rls_mixed_group_fit_value <- function(record) {
  if (!nrow(record$group_summary)) return(.rls_em_dash)
  paste0(record$group_summary$group, ": ", record$group_summary$levels, " levels", collapse = "; ")
}

.rls_mixed_fit_display <- function(record) {
  stats <- record$fit_statistics
  if (identical(record$model_type, "generalized_linear_mixed_model")) {
    labels <- c("N", "Groups", "Family", "Link", "logLik", "AIC", "BIC",
                "Deviance", "df residual", "Rows excluded", "Convergence")
    values <- c(
      as.character(stats$n_used %||% NA_integer_),
      .rls_mixed_group_fit_value(record),
      stats$family %||% record$family %||% .rls_em_dash,
      stats$link %||% record$link %||% .rls_em_dash,
      .rls_mixed_value(stats$log_lik %||% NA_real_, 3L),
      .rls_mixed_value(stats$aic %||% NA_real_, 1L),
      .rls_mixed_value(stats$bic %||% NA_real_, 1L),
      .rls_mixed_value(stats$deviance %||% NA_real_, 3L),
      as.character(stats$df_residual %||% NA_integer_),
      as.character(stats$n_excluded %||% 0L),
      record$convergence_status %||% .rls_em_dash
    )
  } else {
    labels <- c("N", "Groups", "Estimation", "REML criterion", "logLik", "AIC", "BIC",
                "Sigma", "df residual", "Rows excluded", "Convergence")
    values <- c(
      as.character(stats$n_used %||% NA_integer_),
      .rls_mixed_group_fit_value(record),
      stats$method %||% record$method %||% .rls_em_dash,
      .rls_mixed_value(stats$reml_criterion %||% NA_real_, 3L),
      .rls_mixed_value(stats$log_lik %||% NA_real_, 3L),
      .rls_mixed_value(stats$aic %||% NA_real_, 1L),
      .rls_mixed_value(stats$bic %||% NA_real_, 1L),
      .rls_mixed_value(stats$sigma %||% NA_real_, 3L),
      as.character(stats$df_residual %||% NA_integer_),
      as.character(stats$n_excluded %||% 0L),
      record$convergence_status %||% .rls_em_dash
    )
  }
  data.frame(Statistic = labels, Value = values, stringsAsFactors = FALSE, check.names = FALSE)
}

.rls_mixed_model_table <- function(record) {
  structure(
    list(
      title = .rls_mixed_title(record),
      model_id = record$model_id,
      model_type = record$model_type,
      dataset = record$dataset_name,
      response = record$response,
      formula = record$formula,
      family = record$family,
      link = record$link,
      method = record$method,
      scope = record$scope,
      fixed = .rls_mixed_fixed_display(record),
      random = .rls_mixed_random_display(record),
      groups = .rls_mixed_group_display(record),
      fit = .rls_mixed_fit_display(record),
      warnings = record$warnings,
      rows_used_original_ids = record$rows_used_original_ids,
      rows_excluded_original_ids = record$rows_excluded_original_ids,
      fit_version = record$fit_version,
      created_at = record$created_at
    ),
    class = "rlispstat_mixed_model_table"
  )
}

.rls_render_mixed_native_data_frame <- function(data) {
  header <- paste(names(data), collapse = "\t")
  if (!nrow(data)) return(header)
  rows <- vapply(seq_len(nrow(data)), function(row) {
    paste(vapply(data, function(column) as.character(column[[row]]), character(1L)),
          collapse = "\t")
  }, character(1L))
  c(header, rows)
}

.rls_render_mixed_model_native_text <- function(table) {
  header <- c(
    table$title,
    paste("Dataset:", table$dataset),
    paste("Response:", table$response),
    paste("Formula:", table$formula),
    paste("Scope:", table$scope)
  )
  if (!is.null(table$method)) header <- c(header, paste("Estimation:", table$method))
  if (!is.null(table$family)) {
    header <- c(header, paste("Family:", table$family), paste("Link:", table$link))
  }
  blocks <- list(
    c(header, ""),
    "Fixed effects",
    .rls_render_mixed_native_data_frame(table$fixed),
    "",
    "Random effects",
    .rls_render_mixed_native_data_frame(table$random),
    "",
    "Groups",
    .rls_render_mixed_native_data_frame(table$groups),
    "",
    "Model fit",
    .rls_render_mixed_native_data_frame(table$fit)
  )
  if (length(table$warnings)) {
    blocks <- c(blocks, list("", "Warnings", paste("- ", table$warnings)))
  }
  paste(unlist(blocks), collapse = "\n")
}

.rls_render_mixed_model_text <- function(table) {
  header <- c(
    table$title,
    paste("Dataset:", table$dataset),
    paste("Response:", table$response),
    paste("Formula:", table$formula),
    paste("Scope:", table$scope)
  )
  if (!is.null(table$method)) header <- c(header, paste("Estimation:", table$method))
  if (!is.null(table$family)) header <- c(header, paste("Family:", table$family), paste("Link:", table$link))
  blocks <- list(
    c(header, ""),
    "Fixed effects",
    .rls_render_data_frame_text(table$fixed),
    "",
    "Random effects",
    .rls_render_data_frame_text(table$random),
    "",
    "Groups",
    .rls_render_data_frame_text(table$groups),
    "",
    "Model fit",
    .rls_render_data_frame_text(table$fit)
  )
  if (length(table$warnings)) {
    blocks <- c(blocks, list("", "Warnings", paste("- ", table$warnings)))
  }
  paste(unlist(blocks), collapse = "\n")
}

.rls_render_mixed_model_markdown <- function(table) {
  header <- c(
    paste0("### ", table$title),
    "",
    paste0("Dataset: `", .rls_markdown_escape(table$dataset), "`"),
    paste0("Response: `", .rls_markdown_escape(table$response), "`"),
    paste0("Formula: `", .rls_markdown_escape(table$formula), "`"),
    paste0("Scope: `", .rls_markdown_escape(table$scope), "`")
  )
  if (!is.null(table$method)) header <- c(header, paste0("Estimation: `", .rls_markdown_escape(table$method), "`"))
  if (!is.null(table$family)) {
    header <- c(header, paste0("Family: `", .rls_markdown_escape(table$family), "`"),
                paste0("Link: `", .rls_markdown_escape(table$link), "`"))
  }
  out <- c(
    header,
    "",
    "#### Fixed effects",
    "",
    .rls_render_data_frame_markdown(table$fixed),
    "",
    "#### Random effects",
    "",
    .rls_render_data_frame_markdown(table$random),
    "",
    "#### Groups",
    "",
    .rls_render_data_frame_markdown(table$groups),
    "",
    "#### Model fit",
    "",
    .rls_render_data_frame_markdown(table$fit)
  )
  if (length(table$warnings)) {
    out <- c(out, "", "#### Warnings", "", paste0("- ", table$warnings))
  }
  paste(out, collapse = "\n")
}

.rls_export_mixed_model_pdf <- function(table, path, width = 8.5, height = 11) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lines <- strsplit(.rls_render_mixed_model_text(table), "\n", fixed = TRUE)[[1L]]
  line_height <- 0.032
  lines_per_page <- max(8L, floor(0.88 / line_height))
  if (isTRUE(capabilities("cairo"))) {
    grDevices::cairo_pdf(path, width = width, height = height, onefile = TRUE)
  } else {
    grDevices::pdf(path, width = width, height = height, onefile = TRUE)
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  for (page_start in seq(1L, length(lines), by = lines_per_page)) {
    graphics::plot.new()
    graphics::par(family = "Helvetica", mar = c(0, 0, 0, 0))
    page_lines <- lines[page_start:min(length(lines), page_start + lines_per_page - 1L)]
    y <- 0.96
    for (line in page_lines) {
      graphics::text(0.06, y, line, adj = c(0, 1), family = "mono", cex = 0.72)
      y <- y - line_height
    }
  }
  invisible(path)
}

.rls_mixed_sync_native <- function(record) {
  .rls_start_backend()
  dataset <- .rls_dataset_record(record$group)
  try(.rls_send(c(
    "REGISTER_DATASET",
    record$group,
    .rls_variable_payload(record$data, dataset$variable_metadata),
    .rls_dataframe_payload(record$data, dataset$variable_metadata, dataset_record = dataset)
  )), silent = TRUE)
  text <- .rls_render_mixed_model_native_text(.rls_mixed_model_table(record))
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  random_payload <- unlist(lapply(record$random_effects, function(spec) {
    terms <- spec$terms %||% "1"
    c(spec$group, spec$covariance_structure %||% "|", as.character(length(terms)), terms)
  }), use.names = FALSE)
  structured <- c(
    "MIXED_MODEL_OPEN_STRUCTURED",
    record$id,
    record$group,
    record$model_type,
    record$response,
    record$method %||% "",
    record$family %||% "",
    record$link %||% "",
    as.character(length(record$fixed_effects)),
    record$fixed_effects,
    as.character(length(record$random_effects)),
    random_payload,
    as.character(length(lines)),
    lines
  )
  ok <- try(.rls_send(structured), silent = TRUE)
  if (inherits(ok, "try-error") || !is.character(ok) || !startsWith(ok, "OK")) {
    try(.rls_send(c("MIXED_MODEL_OPEN_TEXT", record$id, record$group, record$model_type, as.character(length(lines)), lines)), silent = TRUE)
  }
}

.rls_mixed_refit_record <- function(record) {
  record$formula <- .rls_mixed_formula_string(record$response, record$fixed_effects, record$random_effects)
  if (identical(record$model_type, "generalized_linear_mixed_model")) {
    .rls_fit_generalized_mixed_model_record(record)
  } else {
    .rls_fit_linear_mixed_model_record(record)
  }
}

.rls_mixed_update_handle <- function(record, refit = TRUE, native = isTRUE(.rls_state$process_started)) {
  if (isTRUE(refit)) {
    record <- .rls_mixed_refit_record(record)
  } else {
    record$formula <- .rls_mixed_formula_string(record$response, record$fixed_effects, record$random_effects)
    record$status <- "Model changed; refit required."
  }
  class <- if (identical(record$model_type, "generalized_linear_mixed_model")) {
    "rlispstat_generalized_mixed_model"
  } else {
    "rlispstat_linear_mixed_model"
  }
  handle <- .rls_assign_mixed_model(record, class)
  if (isTRUE(native) && isTRUE(refit)) .rls_mixed_sync_native(record)
  handle
}

.rls_mixed_validate_response_for_type <- function(record, response) {
  if (identical(record$model_type, "generalized_linear_mixed_model")) {
    response <- .rls_validate_protocol_name(response, "response")
    if (!response %in% names(record$data)) {
      stop(sprintf("Column `%s` was not found in the dataset.", response), call. = FALSE)
    }
    response
  } else {
    .rls_model_validate_response(record$data, response, "response")
  }
}

.rls_mixed_diagnostic_types <- c(
  "residuals_vs_fitted", "observed_vs_fitted", "histogram_residuals",
  "normal_qq", "scale_location", "pearson_residuals", "deviance_residuals"
)

.rls_mixed_diagnostic_data <- function(record, type) {
  type <- match.arg(type, .rls_mixed_diagnostic_types)
  data <- record$diagnostic_data
  if (!nrow(data)) {
    stop("The mixed model has no diagnostic data. Refit the model first.", call. = FALSE)
  }
  residual <- switch(
    type,
    pearson_residuals = data$pearson_residual,
    deviance_residuals = data$deviance_residual,
    data$residual
  )
  if (type %in% c("pearson_residuals", "deviance_residuals") && all(is.na(residual))) {
    stop(sprintf("Diagnostic `%s` is not available for this mixed model.", type), call. = FALSE)
  }
  if (identical(type, "residuals_vs_fitted")) {
    out <- data.frame(fitted = data$fitted, residual = residual, row_id = data$original_row_id)
  } else if (identical(type, "observed_vs_fitted")) {
    out <- data.frame(fitted = data$fitted, observed = data$observed, row_id = data$original_row_id)
  } else if (identical(type, "histogram_residuals")) {
    out <- data.frame(residual = residual, row_id = data$original_row_id)
  } else if (identical(type, "normal_qq")) {
    ok <- is.finite(residual)
    sorted <- sort(residual[ok])
    n <- length(sorted)
    out <- data.frame(
      theoretical = if (n) stats::qnorm((seq_len(n) - 0.5) / n) else numeric(),
      residual = sorted,
      row_id = data$original_row_id[ok][order(residual[ok])]
    )
  } else if (identical(type, "scale_location")) {
    out <- data.frame(fitted = data$fitted, sqrt_abs_residual = sqrt(abs(residual)), row_id = data$original_row_id)
  } else if (identical(type, "pearson_residuals")) {
    out <- data.frame(fitted = data$fitted, pearson_residual = residual, row_id = data$original_row_id)
  } else {
    out <- data.frame(fitted = data$fitted, deviance_residual = residual, row_id = data$original_row_id)
  }
  out[stats::complete.cases(out), , drop = FALSE]
}

#' Create a linear mixed model using lme4::lmer()
#'
#' @export
ls_new_linear_mixed_model <- function(data = NULL, response = NULL, fixed = NULL,
                                      random = NULL, method = c("REML", "ML"),
                                      scope = "all", name = NULL, native = TRUE,
                                      .selected_rows = NULL) {
  method <- match.arg(method)
  record <- .rls_new_mixed_model_record(
    data = data, response = response, fixed = fixed, random = random,
    model_type = "linear_mixed_model", method = method, scope = scope, name = name,
    .selected_rows = .selected_rows
  )
  record <- .rls_fit_linear_mixed_model_record(record)
  handle <- .rls_assign_mixed_model(record, "rlispstat_linear_mixed_model")
  if (isTRUE(native)) .rls_mixed_sync_native(record)
  invisible(handle)
}

#' Create a generalized linear mixed model using lme4::glmer()
#'
#' @export
ls_new_generalized_mixed_model <- function(data = NULL, response = NULL, fixed = NULL,
                                           random = NULL, family = "binomial",
                                           link = NULL, scope = "all", name = NULL,
                                           native = TRUE, .selected_rows = NULL) {
  family_object <- .rls_glm_make_family(family, link)
  record <- .rls_new_mixed_model_record(
    data = data, response = response, fixed = fixed, random = random,
    model_type = "generalized_linear_mixed_model", method = "ML",
    family = family, link = family_object$link, scope = scope, name = name,
    .selected_rows = .selected_rows
  )
  record <- .rls_fit_generalized_mixed_model_record(record)
  handle <- .rls_assign_mixed_model(record, "rlispstat_generalized_mixed_model")
  if (isTRUE(native)) .rls_mixed_sync_native(record)
  invisible(handle)
}

#' @rdname ls_new_linear_mixed_model
#' @export
ls_mixed_model_refit <- function(model, native = isTRUE(.rls_state$process_started)) {
  record <- .rls_mixed_model_record(model)
  invisible(.rls_mixed_update_handle(record, refit = TRUE, native = native))
}

#' @rdname ls_new_linear_mixed_model
#' @export
ls_mixed_model_set_response <- function(model, response, refit = TRUE,
                                        native = isTRUE(.rls_state$process_started)) {
  record <- .rls_mixed_model_record(model)
  response <- .rls_mixed_validate_response_for_type(record, response)
  record$response <- response
  record$fixed_effects <- record$fixed_effects[
    vapply(record$fixed_effects, function(term) !identical(.rls_model_clean_factor_call(term), response), logical(1L))
  ]
  for (i in seq_along(record$random_effects)) {
    record$random_effects[[i]]$terms <- record$random_effects[[i]]$terms[
      vapply(record$random_effects[[i]]$terms, function(term) !identical(.rls_model_clean_factor_call(term), response), logical(1L))
    ]
    if (!length(record$random_effects[[i]]$terms)) record$random_effects[[i]]$terms <- "1"
  }
  invisible(.rls_mixed_update_handle(record, refit = refit, native = native))
}

#' @rdname ls_new_linear_mixed_model
#' @export
ls_mixed_model_add_fixed_effect <- function(model, variable, refit = TRUE,
                                            native = isTRUE(.rls_state$process_started)) {
  record <- .rls_mixed_model_record(model)
  variable <- .rls_model_validate_term(record$data, variable, response = record$response, what = "fixed effect")
  if (!variable %in% record$fixed_effects) {
    record$fixed_effects <- c(record$fixed_effects, variable)
  }
  invisible(.rls_mixed_update_handle(record, refit = refit, native = native))
}

#' @rdname ls_new_linear_mixed_model
#' @export
ls_mixed_model_remove_fixed_effect <- function(model, variable, refit = TRUE,
                                               native = isTRUE(.rls_state$process_started)) {
  record <- .rls_mixed_model_record(model)
  variable <- .rls_validate_protocol_name(variable, "fixed effect")
  record$fixed_effects <- record$fixed_effects[
    vapply(record$fixed_effects, function(term) {
      !identical(term, variable) && !identical(.rls_model_clean_factor_call(term), variable)
    }, logical(1L))
  ]
  invisible(.rls_mixed_update_handle(record, refit = refit, native = native))
}

#' @rdname ls_new_linear_mixed_model
#' @export
ls_mixed_model_add_random_effect <- function(model, group, terms = "1",
                                             covariance_structure = c("|", "||"),
                                             refit = TRUE,
                                             native = isTRUE(.rls_state$process_started)) {
  covariance_structure <- match.arg(covariance_structure)
  record <- .rls_mixed_model_record(model)
  spec <- .rls_mixed_normalize_random(
    record$data,
    list(list(group = group, terms = terms, covariance_structure = covariance_structure)),
    record$response
  )[[1L]]
  existing <- match(spec$group, vapply(record$random_effects, `[[`, character(1L), "group"))
  if (is.na(existing)) {
    record$random_effects <- c(record$random_effects, list(spec))
  } else {
    merged_terms <- unique(c(record$random_effects[[existing]]$terms, spec$terms))
    record$random_effects[[existing]]$terms <- merged_terms
    record$random_effects[[existing]]$covariance_structure <- covariance_structure
  }
  invisible(.rls_mixed_update_handle(record, refit = refit, native = native))
}

#' @rdname ls_new_linear_mixed_model
#' @export
ls_mixed_model_remove_random_effect <- function(model, group, refit = TRUE,
                                                native = isTRUE(.rls_state$process_started)) {
  record <- .rls_mixed_model_record(model)
  group <- .rls_validate_protocol_name(group, "group variable")
  keep <- vapply(record$random_effects, function(spec) !identical(spec$group, group), logical(1L))
  if (!any(keep)) {
    stop("Mixed models require at least one random effect.", call. = FALSE)
  }
  record$random_effects <- record$random_effects[keep]
  invisible(.rls_mixed_update_handle(record, refit = refit, native = native))
}

#' @rdname ls_new_linear_mixed_model
#' @export
ls_mixed_model_add_random_slope <- function(model, group, variable, refit = TRUE,
                                            native = isTRUE(.rls_state$process_started)) {
  record <- .rls_mixed_model_record(model)
  group <- .rls_mixed_validate_group(record$data, group)
  variable <- .rls_mixed_validate_random_term(record$data, variable, record$response)
  index <- match(group, vapply(record$random_effects, `[[`, character(1L), "group"))
  if (is.na(index)) {
    record$random_effects <- c(record$random_effects, list(list(group = group, terms = c("1", variable), covariance_structure = "|")))
  } else if (!variable %in% record$random_effects[[index]]$terms) {
    record$random_effects[[index]]$terms <- unique(c(record$random_effects[[index]]$terms, variable))
  }
  invisible(.rls_mixed_update_handle(record, refit = refit, native = native))
}

#' @rdname ls_new_linear_mixed_model
#' @export
ls_mixed_model_remove_random_slope <- function(model, group, variable, refit = TRUE,
                                               native = isTRUE(.rls_state$process_started)) {
  record <- .rls_mixed_model_record(model)
  group <- .rls_validate_protocol_name(group, "group variable")
  variable <- .rls_validate_protocol_name(variable, "random slope")
  index <- match(group, vapply(record$random_effects, `[[`, character(1L), "group"))
  if (!is.na(index)) {
    record$random_effects[[index]]$terms <- setdiff(record$random_effects[[index]]$terms, variable)
    if (!length(record$random_effects[[index]]$terms)) record$random_effects[[index]]$terms <- "1"
  }
  invisible(.rls_mixed_update_handle(record, refit = refit, native = native))
}

#' @rdname ls_new_linear_mixed_model
#' @export
ls_mixed_model_scope <- function(model, scope = c("all", "selected", "unselected"),
                                 refit = TRUE, native = isTRUE(.rls_state$process_started)) {
  scope <- match.arg(scope)
  record <- .rls_mixed_model_record(model)
  record$scope <- scope
  invisible(.rls_mixed_update_handle(record, refit = refit, native = native))
}

#' @rdname ls_new_linear_mixed_model
#' @export
ls_mixed_model_set_method <- function(model, method = c("REML", "ML"), refit = TRUE,
                                      native = isTRUE(.rls_state$process_started)) {
  method <- match.arg(method)
  record <- .rls_mixed_model_record(model)
  if (!identical(record$model_type, "linear_mixed_model")) {
    stop("Only linear mixed models have REML/ML method selection.", call. = FALSE)
  }
  record$method <- method
  invisible(.rls_mixed_update_handle(record, refit = refit, native = native))
}

#' @rdname ls_new_linear_mixed_model
#' @export
ls_mixed_model_set_family <- function(model, family = "binomial", link = NULL,
                                      refit = TRUE, native = isTRUE(.rls_state$process_started)) {
  record <- .rls_mixed_model_record(model)
  if (!identical(record$model_type, "generalized_linear_mixed_model")) {
    stop("Only generalized linear mixed models have family/link selection.", call. = FALSE)
  }
  family_object <- .rls_glm_make_family(family, link)
  record$family <- family
  record$link <- family_object$link
  invisible(.rls_mixed_update_handle(record, refit = refit, native = native))
}

#' @rdname ls_new_linear_mixed_model
#' @export
ls_mixed_model_state <- function(model) {
  .rls_mixed_model_record(model)
}

#' @rdname ls_new_linear_mixed_model
#' @export
ls_mixed_model_table <- function(model) {
  .rls_mixed_model_table(.rls_mixed_model_record(model))
}

#' @rdname ls_new_linear_mixed_model
#' @export
ls_mixed_model_diagnostics <- function(model, type = NULL) {
  record <- .rls_mixed_model_record(model)
  if (is.null(record$model_object)) record <- .rls_mixed_model_record(ls_mixed_model_refit(model, native = FALSE))
  if (is.null(type)) {
    return(record$diagnostic_data)
  }
  .rls_mixed_diagnostic_data(record, match.arg(type, .rls_mixed_diagnostic_types))
}

.rls_mixed_diagnostic_plot_data <- function(record, type) {
  values <- .rls_mixed_diagnostic_data(record, type)
  out <- data.frame(.row = seq_len(nrow(record$data)))
  add_column <- function(name) out[[name]] <<- rep(NA_real_, nrow(record$data))
  for (name in setdiff(names(values), "row_id")) add_column(name)
  rows <- match(values$row_id, record$original_row_ids)
  rows <- rows[!is.na(rows)]
  if (length(rows)) {
    value_rows <- values[match(record$original_row_ids[rows], values$row_id), , drop = FALSE]
    for (name in setdiff(names(values), "row_id")) {
      out[[name]][rows] <- value_rows[[name]]
    }
  }
  out$.row <- NULL
  out
}

#' @rdname ls_new_linear_mixed_model
#' @export
ls_mixed_model_open_diagnostic <- function(model,
                                           type = c("residuals_vs_fitted", "observed_vs_fitted",
                                                    "histogram_residuals", "normal_qq",
                                                    "scale_location", "pearson_residuals",
                                                    "deviance_residuals"),
                                           native = TRUE) {
  type <- match.arg(type)
  record <- .rls_mixed_model_record(model)
  if (is.null(record$model_object)) record <- .rls_mixed_model_record(ls_mixed_model_refit(model, native = FALSE))
  id <- paste(record$id, type, record$fit_version, sep = ":")
  data <- .rls_mixed_diagnostic_data(record, type)
  native_plot <- NULL
  if (isTRUE(native)) {
    plot_data <- .rls_mixed_diagnostic_plot_data(record, type)
    native_plot <- switch(
      type,
      residuals_vs_fitted = ls_scatter(plot_data, "fitted", "residual", group = record$group,
                                       title = paste("Residuals vs fitted", record$dataset_name)),
      observed_vs_fitted = ls_scatter(plot_data, "fitted", "observed", group = record$group,
                                      title = paste("Observed vs fitted", record$dataset_name)),
      histogram_residuals = ls_histogram(plot_data, "residual", group = record$group,
                                         title = paste("Histogram of residuals", record$dataset_name)),
      normal_qq = ls_scatter(plot_data, "theoretical", "residual", group = record$group,
                             title = paste("Normal Q-Q", record$dataset_name)),
      scale_location = ls_scatter(plot_data, "fitted", "sqrt_abs_residual", group = record$group,
                                  title = paste("Scale-location", record$dataset_name)),
      pearson_residuals = ls_scatter(plot_data, "fitted", "pearson_residual", group = record$group,
                                     title = paste("Pearson residuals", record$dataset_name)),
      deviance_residuals = ls_scatter(plot_data, "fitted", "deviance_residual", group = record$group,
                                      title = paste("Deviance residuals", record$dataset_name))
    )
  }
  structure(
    list(
      id = id,
      model_id = record$id,
      type = type,
      native_plot = native_plot,
      displayed_fit_version = record$fit_version,
      data = data
    ),
    class = "rlispstat_mixed_model_diagnostic"
  )
}

#' @rdname ls_new_linear_mixed_model
#' @export
ls_copy_mixed_model_table <- function(model, format = c("text", "markdown")) {
  format <- match.arg(format)
  table <- ls_mixed_model_table(model)
  text <- if (identical(format, "markdown")) .rls_render_mixed_model_markdown(table) else .rls_render_mixed_model_text(table)
  .rls_copy_text(text)
}

#' @rdname ls_new_linear_mixed_model
#' @export
ls_export_mixed_model_table <- function(model, path, format = c("pdf", "txt", "md")) {
  format <- match.arg(format)
  table <- ls_mixed_model_table(model)
  switch(format,
    pdf = .rls_export_mixed_model_pdf(table, path),
    txt = writeLines(.rls_render_mixed_model_text(table), path, useBytes = TRUE),
    md = writeLines(.rls_render_mixed_model_markdown(table), path, useBytes = TRUE)
  )
  invisible(path)
}
