.rls_r_string_literal <- function(value) {
  encodeString(enc2utf8(as.character(value %||% "")[[1L]]), quote = '"')
}

.rls_r_character_vector <- function(values) {
  values <- as.character(values %||% character())
  if (!length(values)) return("character()")
  paste0("c(", paste(vapply(values, .rls_r_string_literal, character(1L)),
                       collapse = ", "), ")")
}

.rls_r_named_character_vector <- function(values) {
  if (is.null(values) || !length(values) || is.null(names(values))) return("character()")
  pieces <- vapply(seq_along(values), function(i) {
    paste0(.rls_r_string_literal(names(values)[[i]]), " = ",
           .rls_r_string_literal(as.character(values[[i]])[[1L]]))
  }, character(1L))
  paste0("c(", paste(pieces, collapse = ", "), ")")
}

.rls_r_formula_text <- function(formula) {
  paste(deparse(formula, width.cutoff = 500L), collapse = " ")
}

.rls_readable_r_formula_text <- function(formula, requested_terms = NULL) {
  terms_object <- stats::terms(formula)
  labels <- attr(terms_object, "term.labels") %||% character()
  if (!length(labels)) return(.rls_r_formula_text(formula))
  labels <- vapply(labels, function(label) {
    requested <- requested_terms[vapply(requested_terms, .rls_model_terms_equivalent,
                                        logical(1L), b = label)]
    if (length(requested)) requested[[1L]] else label
  }, character(1L), USE.NAMES = FALSE)

  consumed <- rep(FALSE, length(labels))
  compact_at <- vector("list", length(labels))
  for (i in seq_along(labels)) {
    interaction <- strsplit(labels[[i]], ":", fixed = TRUE)[[1L]]
    if (length(interaction) == 2L && all(interaction %in% labels)) {
      members <- match(interaction, labels)
      group <- sort(unique(c(i, members)))
      if (!any(consumed[group])) {
        compact_at[[min(group)]] <- paste(interaction, collapse = " * ")
        consumed[group] <- TRUE
      }
    }
  }
  readable <- unlist(lapply(seq_along(labels), function(i) {
    if (length(compact_at[[i]])) return(compact_at[[i]])
    if (!consumed[[i]]) labels[[i]] else character()
  }), use.names = FALSE)

  response <- paste(deparse(formula[[2L]], width.cutoff = 500L), collapse = " ")
  paste(response, "~", paste(readable, collapse = " + "))
}

.rls_model_setup_r_code <- function(record, formula) {
  lines <- c(
    paste0("model_formula <- ", .rls_r_formula_text(formula)),
    paste0("term_types <- ",
           .rls_r_named_character_vector(record$term_types %||% list())),
    paste0("factor_reference_levels <- ",
           .rls_r_named_character_vector(record$factor_reference_levels %||% list())),
    paste0("centered_predictors <- ",
           .rls_r_character_vector(record$centered_predictors %||% character()))
  )
  paste(lines, collapse = "\n")
}

.rls_model_preparation_r_code <- function(input_name = "analysis_data",
                                          output_name = "analysis_data") {
  paste(c(
    "prepare_linkeda_model_data <- function(current_data) {",
    "  numeric_interpretation <- function(x) {",
    "    if (is.numeric(x) || is.logical(x)) return(as.numeric(x))",
    "    labels <- if (is.factor(x)) as.character(x) else x",
    "    blank <- !is.na(labels) & !nzchar(trimws(as.character(labels)))",
    "    labels[blank] <- NA_character_",
    "    parsed <- suppressWarnings(as.numeric(labels))",
    "    present <- !is.na(labels)",
    "    if (!any(present & is.na(parsed))) return(parsed)",
    "    declared <- if (is.factor(x)) as.character(levels(x)) else unique(as.character(labels[present]))",
    "    declared <- declared[!is.na(declared) & nzchar(trimws(declared))]",
    "    if (length(declared) == 2L) {",
    "      binary_mapping <- stats::setNames(c(0, 1), declared)",
    "      return(unname(binary_mapping[as.character(labels)]))",
    "    }",
    "    stop(\"A non-binary Categorical variable requires an explicit global numeric mapping.\", call. = FALSE)",
    "  }",
    "  factor_interpretation <- function(x) {",
    "    labels <- as.character(x)",
    "    blank <- !is.na(labels) & !nzchar(trimws(labels))",
    "    labels[blank] <- NA_character_",
    "    declared <- if (is.factor(x)) as.character(levels(x)) else character()",
    "    declared <- declared[!is.na(declared) & nzchar(trimws(declared))]",
    "    observed <- unique(labels[!is.na(labels)])",
    "    factor(labels, levels = unique(c(declared, observed)), ordered = FALSE)",
    "  }",
    "  for (term in names(term_types)) {",
    "    variables <- all.vars(stats::reformulate(term))",
    "    for (variable in intersect(variables, names(current_data))) {",
    "      if (identical(variable, all.vars(model_formula)[[1L]])) next",
    "      if (identical(unname(term_types[[term]]), \"factor\"))",
    "        current_data[[variable]] <- factor_interpretation(current_data[[variable]])",
    "      if (identical(unname(term_types[[term]]), \"numeric\"))",
    "        current_data[[variable]] <- numeric_interpretation(current_data[[variable]])",
    "    }",
    "  }",
    "  for (variable in intersect(names(factor_reference_levels), names(current_data))) {",
    "    values <- current_data[[variable]]",
    "    if (!is.factor(values) || is.ordered(values)) values <- factor_interpretation(values)",
    "    reference <- unname(factor_reference_levels[[variable]])",
    "    if (nzchar(reference) && reference %in% levels(values))",
    "      current_data[[variable]] <- stats::relevel(values, ref = reference)",
    "  }",
    "  required <- unique(all.vars(model_formula))",
    "  current_data <- current_data[stats::complete.cases(current_data[, required, drop = FALSE]), , drop = FALSE]",
    "  for (variable in intersect(centered_predictors, names(current_data))) {",
    "    if (is.numeric(current_data[[variable]]))",
    "      current_data[[variable]] <- current_data[[variable]] - mean(current_data[[variable]])",
    "  }",
    "  current_data",
    "}",
    paste0(output_name, " <- prepare_linkeda_model_data(", input_name, ")")
  ), collapse = "\n")
}

.rls_verification_mi_partial_r_code <- function(fits_name) {
  c(
    "# Partial correlations use each completed-data fit's residual df, not pooled df.",
    paste0("partial_terms <- setdiff(names(stats::coef(", fits_name, "[[1L]])), \"(Intercept)\")"),
    "pooled_partial_correlations <- data.frame(",
    "  term = partial_terms,",
    "  partial_r = vapply(partial_terms, function(term) {",
    paste0("    r <- vapply(", fits_name, ", function(fit) {"),
    "      coefficients <- stats::coef(summary(fit))",
    "      if (!term %in% rownames(coefficients)) return(NA_real_)",
    "      t <- coefficients[term, \"t value\"]",
    "      df <- stats::df.residual(fit)",
    "      if (!is.finite(df) || df <= 0) return(NA_real_)",
    "      t / sqrt(t^2 + df)",
    "    }, numeric(1L))",
    "    if (any(!is.finite(r)) || any(abs(r) >= 1)) return(NA_real_)",
    "    tanh(mean(atanh(r)))",
    "  }, numeric(1L)), row.names = NULL)",
    "print(pooled_partial_correlations)"
  )
}

.rls_linear_verification_r_code <- function(record,
                                            multiple_imputation = FALSE) {
  formula <- .rls_model_formula_object(record$dependent, record$predictors)
  variables <- unique(all.vars(formula))
  term_types <- record$term_types %||% list()
  factor_variables <- unique(unlist(lapply(
    names(term_types)[unlist(term_types, use.names = FALSE) == "factor"],
    function(term) all.vars(stats::reformulate(term))
  ), use.names = FALSE))
  factor_variables <- intersect(factor_variables, variables)
  references <- record$factor_reference_levels %||% list()
  centered <- intersect(record$centered_predictors %||% character(), variables)
  preparation <- c(
    paste0("model_formula <- ", .rls_r_formula_text(formula)),
    paste0("factor_variables <- ", .rls_r_character_vector(factor_variables)),
    paste0("factor_reference_levels <- ",
           .rls_r_named_character_vector(references)),
    paste0("centered_predictors <- ", .rls_r_character_vector(centered)),
    "prepare_reference_data <- function(data) {",
    "  for (variable in intersect(factor_variables, names(data)))",
    "    data[[variable]] <- factor(data[[variable]])",
    "  for (variable in intersect(names(factor_reference_levels), names(data))) {",
    "    reference <- unname(factor_reference_levels[[variable]])",
    "    if (reference %in% levels(data[[variable]]))",
    "      data[[variable]] <- stats::relevel(data[[variable]], ref = reference)",
    "  }",
    "  required <- all.vars(model_formula)",
    "  data <- data[stats::complete.cases(data[, required, drop = FALSE]), , drop = FALSE]",
    "  for (variable in intersect(centered_predictors, names(data)))",
    "    data[[variable]] <- data[[variable]] - mean(data[[variable]])",
    "  data",
    "}"
  )
  if (isTRUE(multiple_imputation)) {
    code <- c(
      "if (!requireNamespace(\"mice\", quietly = TRUE))",
      "  stop(\"Install package 'mice' to run this check.\")",
      "if (!requireNamespace(\"gtsummary\", quietly = TRUE))",
      "  stop(\"Install package 'gtsummary' to present this check.\")",
      "if (!requireNamespace(\"tibble\", quietly = TRUE))",
      "  stop(\"Install package 'tibble' to present model-fit statistics.\")",
      "mi_long <- base::readRDS(verification_data_path)",
      "stopifnot(all(c(\".imp\", \".id\") %in% names(mi_long)))",
      "completed_sets <- split(mi_long[mi_long$.imp > 0L, , drop = FALSE],",
      "                        mi_long$.imp[mi_long$.imp > 0L])",
      "completed_sets <- lapply(completed_sets, function(data) {",
      "  data$.imp <- NULL; data$.id <- NULL; rownames(data) <- NULL; data",
      "})",
      preparation,
      "reference_fits <- lapply(completed_sets, function(current_data) {",
      "  analysis_data <- prepare_reference_data(current_data)",
      "  do.call(stats::lm, list(formula = model_formula, data = analysis_data,",
      "                          na.action = stats::na.fail))",
      "})",
      "reference_mira <- mice::as.mira(reference_fits)",
      "reference_model <- mice::pool(reference_mira)",
      .rls_verification_mi_partial_r_code("reference_fits"),
      "# Rubin diagnostics: FMI, RIV, within/between and total variance.",
      "print(reference_model$pooled)",
      .rls_verification_mi_d1_term_code(
        "reference_fits", "completed_sets",
        "stats::lm(model_formula, data = current_data, na.action = stats::na.fail)"
      ),
      "pooled_r2 <- mice::pool.r.squared(reference_mira, adjusted = FALSE)",
      "pooled_adjusted_r2 <- mice::pool.r.squared(reference_mira, adjusted = TRUE)",
      "intercept_mira <- mice::as.mira(lapply(reference_fits, function(fit)",
      "  stats::update(fit, . ~ 1)))",
      paste0("global_method <- ", .rls_r_string_literal(record$summary$global_test_method %||% "mice::D1")),
      "global_test <- NULL; global_result <- NULL",
      "if (length(stats::coef(reference_fits[[1L]])) > 1L) {",
      "  global_test <- if (grepl(\"mice::D3\", global_method, fixed = TRUE)) mice::D3(reference_mira, intercept_mira) else mice::D1(reference_mira, intercept_mira)",
      "  global_result <- if (is.matrix(global_test$result) || is.data.frame(global_test$result)) global_test$result[1L, ] else global_test$result",
      "  keys <- c(\"F.value\", \"df1\", \"df2\", \"P(>F)\", \"RIV\")",
      "  if (length(global_result) == length(keys) && is.null(names(global_result))) names(global_result) <- keys",
      "  stopifnot(all(keys %in% names(global_result)))",
      "  # Match the recorded D1 complete-data limit when between-imputation variation is zero.",
      "  if (grepl(\"mice::D1\", global_method, fixed = TRUE) &&",
      "      (!is.finite(global_result[[\"df2\"]]) || !is.finite(global_result[[\"P(>F)\"]])) &&",
      "      is.finite(global_result[[\"RIV\"]]) && abs(global_result[[\"RIV\"]]) <= sqrt(.Machine$double.eps) &&",
      "      is.finite(global_test$dfcom) && global_test$dfcom > 0) {",
      "    global_result[[\"df2\"]] <- global_test$dfcom",
      "    global_result[[\"P(>F)\"]] <- stats::pf(global_result[[\"F.value\"]], global_result[[\"df1\"]], global_test$dfcom, lower.tail = FALSE)",
      "  }",
      "}",
      "reference_fit_metrics <- tibble::tibble(",
      "  nobs = as.integer(round(mean(vapply(reference_fits, stats::nobs, numeric(1L))))),",
      "  r.squared = unname(pooled_r2[1L, \"est\"]),",
      "  adj.r.squared = unname(pooled_adjusted_r2[1L, \"est\"]),",
      "  sigma = mean(vapply(reference_fits, function(fit) summary(fit)$sigma, numeric(1L))),",
      "  statistic = if (is.null(global_result)) NA_real_ else unname(global_result[[\"F.value\"]]),",
      "  df = if (is.null(global_result)) NA_real_ else unname(global_result[[\"df1\"]]),",
      "  df.denominator = if (is.null(global_result)) NA_real_ else unname(global_result[[\"df2\"]]),",
      "  df.residual = min(vapply(reference_fits, stats::df.residual, numeric(1L))),",
      "  p.value = if (is.null(global_result)) NA_real_ else unname(global_result[[\"P(>F)\"]])",
      ")",
      "reference_table <- gtsummary::tbl_regression(",
      "  reference_mira, intercept = TRUE, conf.int = TRUE,",
      "  add_estimate_to_reference_rows = TRUE",
      ")",
      "reference_table <- gtsummary::modify_column_unhide(",
      "  reference_table, columns = c(std.error, statistic)",
      ")",
      "reference_table <- gtsummary::modify_header(",
      "  reference_table, std.error = \"**SE**\", statistic = \"**t**\"",
      ")",
      "reference_table <- gtsummary::add_glance_table(",
      "  reference_table,",
      "  include = c(nobs, r.squared, adj.r.squared, sigma, statistic, df, df.denominator, df.residual, p.value),",
      "  label = list(nobs = \"N\", r.squared = \"R-squared\",",
      "               adj.r.squared = \"Adjusted R-squared\", sigma = \"Residual s\",",
      "               statistic = \"Global D1/D3 F\", df = \"df model\",",
      "               df.denominator = \"df test denominator\", df.residual = \"df residual\", p.value = \"Global p\"),",
      "  glance_fun = function(x) reference_fit_metrics",
      ")",
      "reference_table <- gtsummary::modify_caption(",
      "  reference_table, \"**Linear model with Rubin-pooled estimates**\"",
      ")",
      "reference_table <- gtsummary::modify_table_body(reference_table, function(data) {",
      "  is_reference <- !is.na(data$reference_row) & data$reference_row",
      "  data$label[is_reference] <- paste0(data$label[is_reference], \" (reference)\")",
      "  data",
      "})",
      "if (interactive()) print(reference_table) else reference_table"
    )
  } else {
    code <- c(
      "if (!requireNamespace(\"gtsummary\", quietly = TRUE))",
      "  stop(\"Install package 'gtsummary' to present this check.\")",
      "analysis_data <- base::readRDS(verification_data_path)",
      preparation,
      "analysis_data <- prepare_reference_data(analysis_data)",
      "reference_model <- stats::lm(model_formula, data = analysis_data,",
      "                             na.action = stats::na.fail)",
      .rls_verification_wald_f_term_code("reference_model"),
      "reference_table <- gtsummary::tbl_regression(",
      "  reference_model, intercept = TRUE, conf.int = TRUE,",
      "  add_estimate_to_reference_rows = TRUE",
      ")",
      "reference_table <- gtsummary::modify_column_unhide(",
      "  reference_table, columns = c(std.error, statistic)",
      ")",
      "reference_table <- gtsummary::modify_header(",
      "  reference_table, std.error = \"**SE**\", statistic = \"**t**\"",
      ")",
      "reference_table <- gtsummary::add_glance_table(",
      "  reference_table,",
      "  include = c(nobs, r.squared, adj.r.squared, sigma, statistic, df, df.residual, p.value, AIC, BIC),",
      "  label = list(nobs = \"N\", r.squared = \"R-squared\",",
      "               adj.r.squared = \"Adjusted R-squared\", sigma = \"Residual s\",",
      "               statistic = \"Global F\", df = \"df model\",",
      "               df.residual = \"df residual\", p.value = \"Global p\",",
      "               AIC = \"AIC\", BIC = \"BIC\")",
      ")",
      "reference_table <- gtsummary::modify_caption(reference_table, \"**Linear model**\")",
      "reference_table <- gtsummary::modify_table_body(reference_table, function(data) {",
      "  is_reference <- !is.na(data$reference_row) & data$reference_row",
      "  data$label[is_reference] <- paste0(data$label[is_reference], \" (reference)\")",
      "  data",
      "})",
      "if (interactive()) print(reference_table) else reference_table"
    )
  }
  list(
    code = paste(c(
      "# This check uses the prepared data exported by LinkEDA.",
      "# It does not independently verify how derived columns or selections were created.",
      code
    ), collapse = "\n"),
    variables = variables,
    warnings = if (isTRUE(multiple_imputation)) c(
      "Coefficients, R-squared values, and the global test are pooled with mice using Rubin's rules.",
      "Residual s is the descriptive mean across imputations; AIC and BIC are not pooled."
    ) else character()
  )
}

.rls_regression_comparison_verification_r_code <- function(record) {
  models <- record$models %||% list()
  if (!length(models)) {
    return(list(code = "", variables = character(), warnings = character()))
  }
  formulas <- lapply(models, function(model) {
    response <- if (isTRUE(record$uses_shared_response)) record$response else
      model$response %||% record$response
    .rls_model_formula_object(response, model$terms %||% character())
  })
  labels <- vapply(seq_along(models), function(i) {
    as.character(models[[i]]$label %||% paste("Model", i))[[1L]]
  }, character(1L))
  factor_variables <- lapply(seq_along(models), function(i) {
    model <- models[[i]]
    types <- model$term_types %||% record$term_types %||% list()
    factor_terms <- names(types)[vapply(types, function(type) {
      as.character(type)[[1L]] %in% c("factor", "ordered")
    }, logical(1L))]
    variables <- unique(unlist(lapply(factor_terms, function(term) {
      all.vars(stats::reformulate(term))
    }), use.names = FALSE))
    intersect(variables, all.vars(formulas[[i]]))
  })
  references <- lapply(models, function(model) {
    model$factor_reference_levels %||% list()
  })
  centered <- lapply(seq_along(models), function(i) {
    intersect(models[[i]]$centered_predictors %||% character(),
              all.vars(formulas[[i]]))
  })
  model_setup <- c(
    paste0("model_labels <- ", .rls_r_character_vector(labels)),
    paste0("model_formulas <- list(", paste(vapply(
      formulas, .rls_r_formula_text, character(1L)), collapse = ", "), ")"),
    paste0("model_factor_variables <- list(", paste(vapply(
      factor_variables, .rls_r_character_vector, character(1L)), collapse = ", "), ")"),
    paste0("model_factor_reference_levels <- list(", paste(vapply(
      references, .rls_r_named_character_vector, character(1L)), collapse = ", "), ")"),
    paste0("model_centered_predictors <- list(", paste(vapply(
      centered, .rls_r_character_vector, character(1L)), collapse = ", "), ")"),
    "prepare_model_data <- function(data, model_index) {",
    "  formula <- model_formulas[[model_index]]",
    "  factors <- model_factor_variables[[model_index]]",
    "  references <- model_factor_reference_levels[[model_index]]",
    "  centered <- model_centered_predictors[[model_index]]",
    "  for (variable in intersect(factors, names(data)))",
    "    data[[variable]] <- factor(data[[variable]])",
    "  for (variable in intersect(names(references), names(data))) {",
    "    reference <- unname(references[[variable]])",
    "    if (reference %in% levels(data[[variable]]))",
    "      data[[variable]] <- stats::relevel(data[[variable]], ref = reference)",
    "  }",
    "  required <- all.vars(formula)",
    "  data <- data[stats::complete.cases(data[, required, drop = FALSE]), , drop = FALSE]",
    "  for (variable in intersect(centered, names(data)))",
    "    data[[variable]] <- data[[variable]] - mean(data[[variable]])",
    "  data",
    "}",
    "safe_nested_anova <- function(previous, current) {",
    "  left <- stats::model.frame(previous); right <- stats::model.frame(current)",
    "  if (!identical(rownames(left), rownames(right)) ||",
    "      !identical(stats::formula(previous)[[2L]], stats::formula(current)[[2L]]) ||",
    "      !isTRUE(all.equal(stats::model.response(left), stats::model.response(right), tolerance = 0)) ||",
    "      !identical(stats::model.weights(left), stats::model.weights(right)) ||",
    "      !identical(stats::model.offset(left), stats::model.offset(right))) return(NULL)",
    "  x1 <- stats::model.matrix(previous); x2 <- stats::model.matrix(current)",
    "  r1 <- qr(x1)$rank; r2 <- qr(x2)$rank",
    "  joint <- qr(cbind(if (r1 > r2) x1 else x2, if (r1 > r2) x2 else x1))$rank",
    "  if (joint != max(r1, r2) || r1 == r2) return(NULL)",
    "  stats::anova(previous, current)",
    "}",
    "decorate_model_table <- function(table, metrics) {",
    "  table <- gtsummary::modify_column_unhide(table, columns = c(std.error, statistic))",
    "  table <- gtsummary::modify_header(table, std.error = \"**SE**\", statistic = \"**t**\")",
    "  table <- gtsummary::add_glance_table(",
    "    table, include = everything(),",
    "    label = list(nobs = \"N\", r.squared = \"R-squared\",",
    "      adj.r.squared = \"Adjusted R-squared\", sigma = \"Residual s\",",
    "      df.residual = \"df residual\", statistic = \"Global F / D1\",",
    "      df = \"df model\", p.value = \"Global p\", AIC = \"AIC\", BIC = \"BIC\",",
    "      delta.r.squared = \"Delta R-squared vs previous\",",
    "      delta.adj.r.squared = \"Delta adjusted R-squared vs previous\",",
    "      delta.sigma = \"Delta s vs previous\", delta.df = \"Delta df vs previous\",",
    "      comparison.statistic = \"F / D1 vs previous\",",
    "      comparison.p.value = \"p vs previous\"),",
    "    glance_fun = function(x) metrics",
    "  )",
    "  gtsummary::modify_table_body(table, function(data) {",
    "    is_reference <- !is.na(data$reference_row) & data$reference_row",
    "    data$label[is_reference] <- paste0(data$label[is_reference], \" (reference)\")",
    "    data",
    "  })",
    "}",
    "merge_model_tables <- function(model_tables) {",
    "  comparison_table <- gtsummary::tbl_merge(",
    "    tbls = model_tables, tab_spanner = paste0(\"**\", model_labels, \"**\"), quiet = TRUE",
    "  )",
    "  gtsummary::modify_table_body(comparison_table, function(data) {",
    "    data[order(data$row_type == \"glance_statistic\"), , drop = FALSE]",
    "  })",
    "}"
  )
  shared_requirements <- c(
    "if (!requireNamespace(\"gtsummary\", quietly = TRUE))",
    "  stop(\"Install package 'gtsummary' to present this comparison.\")",
    "if (!requireNamespace(\"tibble\", quietly = TRUE))",
    "  stop(\"Install package 'tibble' to present model-fit statistics.\")"
  )
  imputed_dataset <- identical(record$analysis_backend %||% "ordinary", "multiple_imputation")
  multiple_imputation <- imputed_dataset && !identical(record$mi_pooling_required, FALSE)
  if (multiple_imputation) {
    code <- c(
      shared_requirements,
      "if (!requireNamespace(\"mice\", quietly = TRUE))",
      "  stop(\"Install package 'mice' to pool this comparison.\")",
      "mi_long <- base::readRDS(verification_data_path)",
      "stopifnot(all(c(\".imp\", \".id\") %in% names(mi_long)))",
      "completed_sets <- split(mi_long[mi_long$.imp > 0L, , drop = FALSE],",
      "                        mi_long$.imp[mi_long$.imp > 0L])",
      "completed_sets <- lapply(completed_sets, function(data) {",
      "  data$.imp <- NULL; data$.id <- NULL; rownames(data) <- NULL; data",
      "})",
      model_setup,
      "model_fits <- lapply(seq_along(model_formulas), function(i) {",
      "  lapply(completed_sets, function(current_data) {",
      "    analysis_data <- prepare_model_data(current_data, i)",
      "    do.call(stats::lm, list(formula = model_formulas[[i]], data = analysis_data,",
      "                            na.action = stats::na.fail))",
      "  })",
      "})",
      "model_miras <- lapply(model_fits, mice::as.mira)",
      "reference_missing_information <- lapply(model_miras, function(model) {",
      "  pooled <- mice::pool(model)$pooled",
      "  pooled$mcse <- with(pooled, sqrt(b/m))",
      "  pooled$mcse_percent_se <- with(pooled, ifelse(t>0,100*mcse/sqrt(t),NA_real_))",
      "  pooled",
      "})",
      "names(reference_missing_information) <- model_labels",
      "print(reference_missing_information)",
      "pooled_test <- function(full, reduced, method) {",
      "  unavailable <- c(statistic = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_)",
      "  if (!grepl(\"mice::D1|mice::D3\", method)) return(unavailable)",
      "  full_fits <- mice::getfit(full); reduced_fits <- mice::getfit(reduced)",
      "  if (length(full_fits) != length(reduced_fits)) return(unavailable)",
      "  valid <- vapply(seq_along(full_fits), function(i) {",
      "    f <- full_fits[[i]]; r <- reduced_fits[[i]]",
      "    common <- names(stats::coef(r)); all_names <- names(stats::coef(f))",
      "    if (length(common) >= length(all_names) || !all(common %in% all_names)) return(FALSE)",
      "    if (!identical(all_names[all_names %in% common], common)) return(FALSE)",
      "    if (is.null(safe_nested_anova(r, f))) return(FALSE)",
      "    isTRUE(all.equal(unname(stats::model.matrix(f)[, common, drop = FALSE]),",
      "                     unname(stats::model.matrix(r)[, common, drop = FALSE]), tolerance = 1e-10))",
      "  }, logical(1L))",
      "  if (!all(valid)) return(unavailable)",
      "  test <- if (grepl(\"mice::D3\", method, fixed = TRUE)) mice::D3(full, reduced) else mice::D1(full, reduced)",
      "  result <- if (is.matrix(test$result) || is.data.frame(test$result)) test$result[1L, ] else test$result",
      "  keys <- c(\"F.value\", \"df1\", \"df2\", \"P(>F)\", \"RIV\")",
      "  if (length(result) == length(keys) && is.null(names(result))) names(result) <- keys",
      "  stopifnot(all(keys %in% names(result)))",
      "  # At zero between-imputation variation use the complete-data limit of D1.",
      "  if (grepl(\"mice::D1\", method, fixed = TRUE) &&",
      "      (!is.finite(result[[\"df2\"]]) || !is.finite(result[[\"P(>F)\"]])) &&",
      "      is.finite(result[[\"RIV\"]]) && abs(result[[\"RIV\"]]) <= sqrt(.Machine$double.eps) &&",
      "      is.finite(test$dfcom) && test$dfcom > 0) {",
      "    result[[\"df2\"]] <- test$dfcom",
      "    result[[\"P(>F)\"]] <- stats::pf(result[[\"F.value\"]], result[[\"df1\"]], test$dfcom, lower.tail = FALSE)",
      "  }",
      "  c(statistic = unname(result[[\"F.value\"]]), df1 = unname(result[[\"df1\"]]),",
      "    df2 = unname(result[[\"df2\"]]), p = unname(result[[\"P(>F)\"]]))",
      "}",
      paste0("global_methods <- ", .rls_r_character_vector(vapply(models,
        function(model) model$summary$global_test_method %||% "mice::D1", character(1L)))),
      paste0("comparison_methods <- ", .rls_r_character_vector(vapply(models,
        function(model) model$comparison_vs_previous$method %||% "mice::D1", character(1L)))),
      "model_metrics <- lapply(seq_along(model_miras), function(i) {",
      "  fits <- model_fits[[i]]; mira <- model_miras[[i]]",
      "  r2 <- unname(mice::pool.r.squared(mira, adjusted = FALSE)[1L, \"est\"])",
      "  adj_r2 <- unname(mice::pool.r.squared(mira, adjusted = TRUE)[1L, \"est\"])",
      "  sigma <- mean(vapply(fits, function(fit) summary(fit)$sigma, numeric(1L)))",
      "  intercept_mira <- mice::as.mira(lapply(fits, function(fit) stats::update(fit, . ~ 1)))",
      "  global <- pooled_test(mira, intercept_mira, global_methods[[i]])",
      "  previous <- c(statistic = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_)",
      "  if (i > 1L) {",
      "    larger_current <- length(stats::coef(fits[[1L]])) > length(stats::coef(model_fits[[i - 1L]][[1L]]))",
      "    previous <- if (larger_current) pooled_test(mira, model_miras[[i - 1L]], comparison_methods[[i]]) else",
      "      pooled_test(model_miras[[i - 1L]], mira, comparison_methods[[i]])",
      "  }",
      "  previous_r2 <- if (i > 1L) unname(mice::pool.r.squared(model_miras[[i - 1L]], adjusted = FALSE)[1L, \"est\"]) else NA_real_",
      "  previous_adj_r2 <- if (i > 1L) unname(mice::pool.r.squared(model_miras[[i - 1L]], adjusted = TRUE)[1L, \"est\"]) else NA_real_",
      "  previous_sigma <- if (i > 1L) mean(vapply(model_fits[[i - 1L]], function(fit) summary(fit)$sigma, numeric(1L))) else NA_real_",
      "  tibble::tibble(nobs = as.integer(round(mean(vapply(fits, stats::nobs, numeric(1L))))),",
      "    r.squared = r2, adj.r.squared = adj_r2, sigma = sigma,",
      "    df.residual = min(vapply(fits, stats::df.residual, numeric(1L))),",
      "    statistic = global[[\"statistic\"]], df = global[[\"df1\"]], p.value = global[[\"p\"]],",
      "    delta.r.squared = r2 - previous_r2,",
      "    delta.adj.r.squared = adj_r2 - previous_adj_r2,",
      "    delta.sigma = sigma - previous_sigma, delta.df = previous[[\"df1\"]],",
      "    comparison.statistic = previous[[\"statistic\"]],",
      "    comparison.p.value = previous[[\"p\"]])",
      "})",
      "model_tables <- lapply(seq_along(model_miras), function(i) {",
      "  table <- gtsummary::tbl_regression(model_miras[[i]], intercept = TRUE, conf.int = TRUE,",
      "    add_estimate_to_reference_rows = TRUE)",
      "  decorate_model_table(table, model_metrics[[i]])",
      "})",
      "comparison_table <- merge_model_tables(model_tables)",
      "if (interactive()) print(comparison_table) else comparison_table"
    )
    warnings <- c(
      "Coefficients, R-squared values, and D1/D3 model tests are pooled with mice.",
      "Residual s and its change are descriptive means across imputations; AIC and BIC are not pooled."
    )
  } else {
    code <- c(
      shared_requirements,
      "analysis_data <- base::readRDS(verification_data_path)",
      if (imputed_dataset) c(
        "# No model inputs were imputed in this scope; reproduce the ordinary analysis.",
        "analysis_data <- analysis_data[analysis_data$.imp == min(analysis_data$.imp[analysis_data$.imp > 0L]), , drop = FALSE]",
        "rownames(analysis_data) <- as.character(analysis_data$.id)",
        "analysis_data$.imp <- NULL; analysis_data$.id <- NULL"
      ) else character(),
      model_setup,
      "model_fits <- lapply(seq_along(model_formulas), function(i)",
      "  stats::lm(model_formulas[[i]], data = prepare_model_data(analysis_data, i),",
      "            na.action = stats::na.fail))",
      "model_metrics <- lapply(seq_along(model_fits), function(i) {",
      "  fit <- model_fits[[i]]; summary_fit <- summary(fit)",
      "  global <- unname(summary_fit$fstatistic)",
      "  global_p <- if (length(global) == 3L) stats::pf(global[[1L]], global[[2L]], global[[3L]], lower.tail = FALSE) else NA_real_",
      "  nested <- if (i > 1L) tryCatch(safe_nested_anova(model_fits[[i - 1L]], fit), error = function(e) NULL) else NULL",
      "  previous <- if (i > 1L) summary(model_fits[[i - 1L]]) else NULL",
      "  tibble::tibble(nobs = stats::nobs(fit), r.squared = summary_fit$r.squared,",
      "    adj.r.squared = summary_fit$adj.r.squared, sigma = summary_fit$sigma,",
      "    df.residual = stats::df.residual(fit),",
      "    statistic = if (length(global)) global[[1L]] else NA_real_,",
      "    df = if (length(global) >= 2L) global[[2L]] else NA_real_, p.value = global_p,",
      "    AIC = stats::AIC(fit), BIC = stats::BIC(fit),",
      "    delta.r.squared = if (is.null(previous)) NA_real_ else summary_fit$r.squared - previous$r.squared,",
      "    delta.adj.r.squared = if (is.null(previous)) NA_real_ else summary_fit$adj.r.squared - previous$adj.r.squared,",
      "    delta.sigma = if (is.null(previous)) NA_real_ else summary_fit$sigma - previous$sigma,",
      "    delta.df = if (is.null(nested) || nrow(nested) < 2L) NA_real_ else abs(nested$Df[[2L]]),",
      "    comparison.statistic = if (is.null(nested) || nrow(nested) < 2L) NA_real_ else nested$F[[2L]],",
      "    comparison.p.value = if (is.null(nested) || nrow(nested) < 2L) NA_real_ else nested$`Pr(>F)`[[2L]])",
      "})",
      "model_tables <- lapply(seq_along(model_fits), function(i) {",
      "  table <- gtsummary::tbl_regression(model_fits[[i]], intercept = TRUE, conf.int = TRUE,",
      "    add_estimate_to_reference_rows = TRUE)",
      "  decorate_model_table(table, model_metrics[[i]])",
      "})",
      "comparison_table <- merge_model_tables(model_tables)",
      "if (interactive()) print(comparison_table) else comparison_table"
    )
    warnings <- character()
  }
  list(
    code = paste(c(
      "# This check refits every compared model from the prepared data exported by LinkEDA.",
      "# Model columns remain in their LinkEDA order; fit and change statistics follow the coefficients.",
      code
    ), collapse = "\n"),
    variables = unique(unlist(lapply(formulas, all.vars), use.names = FALSE)),
    warnings = warnings
  )
}

.rls_linear_executed_r_code <- function(record, multiple_imputation = FALSE) {
  formula <- .rls_model_formula_object(record$dependent, record$predictors)
  setup <- .rls_model_setup_r_code(record, formula)
  if (isTRUE(multiple_imputation)) {
    global_test <- if (length(record$predictors %||% character())) c(
      "reduced_fits <- LinkEDA:::.rls_mi_reduced_fits(fits, intercept_only = TRUE)",
      paste0("pooled_global_test <- LinkEDA:::.rls_mi_pool_d1(fits, reduced_fits, term_names = ",
             .rls_r_character_vector(record$predictors), ")")
    ) else "pooled_global_test <- NULL"
    return(paste(c(
      setup,
      "completed_sets <- LinkEDA::ls_complete_data_version(scoped_source_data)",
      .rls_model_preparation_r_code("completed_sets[[1L]]", "analysis_data"),
      "imputed_analysis_data <- lapply(completed_sets, prepare_linkeda_model_data)",
      "fits <- lapply(imputed_analysis_data, function(current_data) {",
      "  stats::lm(model_formula, data = current_data, na.action = stats::na.fail)",
      "})",
      "pooled_coefficients <- mice::pool(mice::as.mira(fits))",
      "pooled_r_squared <- mice::pool.r.squared(mice::as.mira(fits), adjusted = FALSE)",
      "pooled_adjusted_r_squared <- mice::pool.r.squared(mice::as.mira(fits), adjusted = TRUE)",
      global_test
    ), collapse = "\n"))
  }
  paste(
    setup,
    .rls_model_preparation_r_code("analysis_data", "analysis_data"),
    "model <- stats::lm(model_formula, data = analysis_data, na.action = stats::na.fail)",
    sep = "\n"
  )
}

.rls_generalized_fitter_r_code <- function(record) {
  formula <- .rls_generalized_glm_formula_object(record)
  family <- as.character(record$family %||% "gaussian")[[1L]]
  link <- as.character(record$link %||% "identity")[[1L]]
  stats_family <- if (identical(family, "gamma_distance")) "Gamma"
    else if (identical(family, "gaussian_log")) "gaussian" else family
  fit <- if (identical(family, "beta")) {
    paste0("betareg::betareg(model_formula, data = current_data, link = ",
           .rls_r_string_literal(link), ")")
  } else if (identical(family, "beta_one_inflated")) {
    paste0("gamlss::gamlss(model_formula, data = current_data, family = ",
           "gamlss.dist::BEOI(mu.link = ", .rls_r_string_literal(link),
           ", nu.link = \"logit\"), trace = FALSE)")
  } else if (isTRUE(record$count_regression) &&
             identical(record$count_distribution %||% "", "negative_binomial")) {
    "MASS::glm.nb(model_formula, data = current_data, link = \"log\")"
  } else if (isTRUE(record$count_regression) &&
             identical(record$count_distribution %||% "",
                       "hurdle_beta_binomial_ceiling")) {
    paste0(
      "gamlss::gamlss(model_formula, nu.formula = stats::reformulate(",
      "attr(stats::terms(model_formula), \"term.labels\")), data = current_data, ",
      "family = gamlss.dist::ZABB(mu.link = \"logit\", sigma.link = \"log\", ",
      "nu.link = \"logit\"), trace = FALSE)"
    )
  } else if (isTRUE(record$count_regression) &&
             identical(record$count_distribution %||% "", "beta_binomial")) {
    paste0(
      "glmmTMB::glmmTMB(model_formula, data = current_data, family = ",
      "glmmTMB::betabinomial(link = \"logit\"))"
    )
  } else if (identical(family, "lognormal")) {
    "stats::lm(model_formula, data = current_data, na.action = stats::na.fail)"
  } else {
    paste0(
      "stats::glm(model_formula, data = current_data, family = ",
      "stats::", stats_family, "(link = ", .rls_r_string_literal(link), "))"
    )
  }
  list(formula = formula, fit = fit)
}

.rls_generalized_executed_r_code <- function(record, multiple_imputation = FALSE) {
  fitter <- .rls_generalized_fitter_r_code(record)
  setup <- .rls_model_setup_r_code(record, fitter$formula)
  bounded <- if (length(record$response_bounds %||% numeric()) == 2L) {
    paste0("response_bounds <- c(",
           format(record$response_bounds[[1L]], digits = 17L, scientific = FALSE), ", ",
           format(record$response_bounds[[2L]], digits = 17L, scientific = FALSE), ")")
  } else "response_bounds <- NULL"
  transformation <- if (record$family %in% c("beta", "beta_one_inflated")) {
    paste0(
      "current_data[[", .rls_r_string_literal(record$response), "]] <- ",
      "(current_data[[", .rls_r_string_literal(record$response), "]] - response_bounds[[1L]]) / ",
      "(response_bounds[[2L]] - response_bounds[[1L]])"
    )
  } else if (identical(record$family, "gamma_distance")) {
    paste0(
      "current_data[[", .rls_r_string_literal(record$response), "]] <- ",
      "response_bounds[[2L]] - current_data[[", .rls_r_string_literal(record$response), "]]"
    )
  } else character()
  exposure <- if (nzchar(record$exposure %||% "")) {
    paste0("# Exposure offset: log(", .rls_r_string_literal(record$exposure), ")")
  } else character()
  binary_transformation <- if (isTRUE(record$binary_regression)) {
    c(
      paste0("binary_event <- ", .rls_r_string_literal(record$event %||% "1")),
      paste0("binary_reference <- ", .rls_r_string_literal(record$reference %||% "0")),
      paste0(
        "current_data[[", .rls_r_string_literal(record$response), "]] <- as.integer(",
        "as.character(current_data[[", .rls_r_string_literal(record$response), "]]) == binary_event)"
      )
    )
  } else character()
  if (isTRUE(multiple_imputation)) {
    return(paste(c(
      setup, bounded, exposure,
      "completed_sets <- LinkEDA::ls_complete_data_version(scoped_source_data)",
      .rls_model_preparation_r_code("completed_sets[[1L]]", "analysis_data"),
      "imputed_analysis_data <- lapply(completed_sets, prepare_linkeda_model_data)",
      "fits <- lapply(imputed_analysis_data, function(current_data) {",
      if (length(binary_transformation)) paste0("  ", binary_transformation) else character(),
      if (length(transformation)) paste0("  ", transformation) else character(),
      paste0("  ", fitter$fit),
      "})",
      "pooled_coefficients <- LinkEDA:::.rls_mi_pool_generalized_mean_coefficients(fits, statistic_name = \"t\")",
      "pooled_term_tests <- LinkEDA:::.rls_mi_generalized_term_omnibus_tests(fits)",
      "# Ordinary GLM coefficients are pooled through mice::pool; bounded-response mean submodels use mice::pool.scalar."
    ), collapse = "\n"))
  }
  paste(c(setup, bounded, exposure,
          .rls_model_preparation_r_code("analysis_data", "analysis_data"),
          if (length(binary_transformation)) {
            gsub("current_data", "analysis_data", binary_transformation, fixed = TRUE)
          } else character(),
          if (length(transformation)) gsub("current_data", "analysis_data", transformation, fixed = TRUE) else character(),
          paste0("model <- ", gsub("current_data", "analysis_data", fitter$fit,
                                    fixed = TRUE))),
        collapse = "\n")
}

.rls_verification_lr_term_code <- function(fit_name, data_name,
                                           fitter_expression) {
  reduced_fit <- gsub("model_formula", "reduced_formula", fitter_expression,
                      fixed = TRUE)
  reduced_fit <- gsub("current_data", data_name, reduced_fit, fixed = TRUE)
  c(
    "model_terms <- attr(stats::terms(model_formula), \"term.labels\")",
    "global_term_tests <- do.call(rbind, lapply(model_terms, function(term) {",
    "  term_index <- match(term, attr(stats::terms(model_formula), \"term.labels\"))",
    "  reduced_formula <- if (length(model_terms) == 1L)",
    "    stats::update.formula(model_formula, . ~ 1) else",
    "    stats::formula(stats::drop.terms(",
    "      stats::terms(model_formula), dropx = term_index, keep.response = TRUE",
    "    ))",
    paste0("  reduced_model <- ", reduced_fit),
    paste0("  full_log_likelihood <- stats::logLik(", fit_name, ")"),
    "  reduced_log_likelihood <- stats::logLik(reduced_model)",
    paste0("  stopifnot(stats::nobs(", fit_name, ") == stats::nobs(reduced_model))"),
    "  statistic <- 2 * as.numeric(full_log_likelihood - reduced_log_likelihood)",
    "  degrees_freedom <- attr(full_log_likelihood, \"df\") -",
    "    attr(reduced_log_likelihood, \"df\")",
    "  data.frame(",
    "    term = term, method = \"LR chi-square\", statistic = statistic,",
    "    df = degrees_freedom, df2 = NA_real_,",
    "    p = stats::pchisq(statistic, df = degrees_freedom, lower.tail = FALSE)",
    "  )",
    "}))",
    "global_term_tests"
  )
}

.rls_verification_mi_d1_term_code <- function(fits_name, data_sets_name,
                                               fitter_expression,
                                               pre_fit_lines = character()) {
  reduced_fit <- gsub("model_formula", "reduced_formula", fitter_expression,
                      fixed = TRUE)
  c(
    "model_terms <- attr(stats::terms(model_formula), \"term.labels\")",
    "global_term_tests <- do.call(rbind, lapply(model_terms, function(term) {",
    "  term_index <- match(term, attr(stats::terms(model_formula), \"term.labels\"))",
    "  reduced_formula <- if (length(model_terms) == 1L)",
    "    stats::update.formula(model_formula, . ~ 1) else",
    "    stats::formula(stats::drop.terms(",
    "      stats::terms(model_formula), dropx = term_index, keep.response = TRUE",
    "    ))",
    paste0("  reduced_fits <- lapply(", data_sets_name, ", function(current_data) {"),
    if (length(pre_fit_lines)) paste0("    ", pre_fit_lines) else character(),
    paste0("    ", reduced_fit),
    "  })",
    paste0("  pooled_test <- mice::D1(mice::as.mira(", fits_name,
           "), mice::as.mira(reduced_fits))"),
    "  result <- pooled_test$result",
    "  if (is.matrix(result) || is.data.frame(result))",
    "    result <- result[1L, , drop = TRUE]",
    "  if (is.null(names(result)))",
    "    names(result) <- c(\"F.value\", \"df1\", \"df2\", \"P(>F)\", \"RIV\")",
    "  data.frame(",
    "    term = term, method = \"D1\", statistic = unname(result[[\"F.value\"]]),",
    "    df = unname(result[[\"df1\"]]), df2 = unname(result[[\"df2\"]]),",
    "    p = unname(result[[\"P(>F)\"]])",
    "  )",
    "}))",
    "global_term_tests"
  )
}

.rls_verification_joint_wald_term_code <- function(fit_name,
                                                    statistic = c("chisq", "F")) {
  statistic <- match.arg(statistic)
  uses_f <- identical(statistic, "F")
  c(
    paste0("design <- stats::model.matrix(", fit_name, ")"),
    paste0("coefficient_estimates <- stats::coef(", fit_name, ")"),
    paste0("coefficient_covariance <- stats::vcov(", fit_name, ")"),
    paste0("model_terms <- attr(stats::terms(", fit_name, "), \"term.labels\")"),
    "term_assignment <- attr(design, \"assign\")",
    "term_tests <- do.call(rbind, lapply(seq_along(model_terms), function(index) {",
    "  coefficient_names <- colnames(design)[term_assignment == index]",
    "  estimates <- coefficient_estimates[coefficient_names]",
    "  covariance <- coefficient_covariance[coefficient_names, coefficient_names, drop = FALSE]",
    "  degrees_freedom <- length(estimates)",
    if (uses_f) c(
      "  statistic <- as.numeric(crossprod(estimates, qr.solve(covariance, estimates))) /",
      "    degrees_freedom",
      paste0("  denominator_df <- stats::df.residual(", fit_name, ")")
    ) else c(
      "  statistic <- as.numeric(crossprod(estimates, qr.solve(covariance, estimates)))",
      "  denominator_df <- NA_real_"
    ),
    "  data.frame(",
    paste0("    term = model_terms[[index]], method = ",
           if (uses_f) "\"F\"" else "\"Wald chi-square\"", ", statistic = statistic,"),
    "    df = degrees_freedom, df2 = denominator_df,",
    if (uses_f) {
      "    p = stats::pf(statistic, degrees_freedom, denominator_df, lower.tail = FALSE)"
    } else {
      "    p = stats::pchisq(statistic, degrees_freedom, lower.tail = FALSE)"
    },
    "  )",
    "}))",
    "term_tests"
  )
}

.rls_verification_wald_f_term_code <- function(fit_name) {
  .rls_verification_joint_wald_term_code(fit_name, statistic = "F")
}

.rls_verification_wald_chisq_term_code <- function(fit_name) {
  .rls_verification_joint_wald_term_code(fit_name, statistic = "chisq")
}

.rls_verification_mi_pooled_wald_term_code <- function(fits_name,
                                                       plain_glm = FALSE) {
  c(
    if (isTRUE(plain_glm)) c(
      "mean_coefficients <- stats::coef",
      "mean_covariance <- stats::vcov"
    ) else c(
      "mean_coefficients <- function(model) {",
      "  if (inherits(model, \"glmmTMB\")) return(glmmTMB::fixef(model)$cond)",
      "  if (inherits(model, \"betareg\")) stats::coef(model, model = \"mean\")",
      "  else if (inherits(model, \"gamlss\")) stats::coef(model, what = \"mu\")",
      "  else stats::coef(model)",
      "}",
      "mean_covariance <- function(model) {",
      "  if (inherits(model, \"glmmTMB\")) return(stats::vcov(model)$cond)",
      "  if (inherits(model, \"betareg\")) return(stats::vcov(model, model = \"mean\"))",
      "  coefficient_count <- length(mean_coefficients(model))",
      "  stats::vcov(model)[seq_len(coefficient_count), seq_len(coefficient_count), drop = FALSE]",
      "}"
    ),
    paste0("design <- stats::model.matrix(", fits_name, "[[1L]])"),
    paste0("model_terms <- attr(stats::terms(", fits_name, "[[1L]]), \"term.labels\")"),
    "term_assignment <- attr(design, \"assign\")",
    "term_tests <- do.call(rbind, lapply(seq_along(model_terms), function(index) {",
    "  coefficient_names <- colnames(design)[term_assignment == index]",
    paste0("  estimates <- do.call(rbind, lapply(", fits_name,
           ", function(model) mean_coefficients(model)[coefficient_names]))"),
    paste0("  within <- Reduce(`+`, lapply(", fits_name,
           ", function(model) mean_covariance(model)[coefficient_names, coefficient_names, drop = FALSE])) / length(",
           fits_name, ")"),
    "  between <- stats::cov(estimates)",
    "  if (length(coefficient_names) == 1L) between <- matrix(between, 1L, 1L)",
    paste0("  total <- within + (1 + 1 / length(", fits_name, ")) * between"),
    "  pooled <- colMeans(estimates)",
    "  statistic <- as.numeric(crossprod(pooled, qr.solve(total, pooled)))",
    "  degrees_freedom <- length(pooled)",
    "  data.frame(",
    "    term = model_terms[[index]], method = \"Rubin pooled Wald chi-square\",",
    "    statistic = statistic, df = degrees_freedom, df2 = NA_real_,",
    "    p = stats::pchisq(statistic, degrees_freedom, lower.tail = FALSE)",
    "  )",
    "}))",
    "term_tests"
  )
}

.rls_binary_calibration_verification_code <- function(fit_name) {
  c(
    "# Apparent calibration on fitting cases; this is not external validation.",
    paste0("predicted_probability <- stats::fitted(", fit_name, ")"),
    paste0("observed_binary <- stats::model.response(stats::model.frame(", fit_name, "))"),
    "reference_calibration <- c(Intercept = NA_real_, Slope = NA_real_)",
    "if (all(is.finite(predicted_probability) & predicted_probability > 0 & predicted_probability < 1)) {",
    "  predicted_log_odds <- stats::qlogis(predicted_probability)",
    "  reference_calibration <- stats::coef(stats::glm(observed_binary ~ predicted_log_odds,",
    "    family = stats::binomial(link = \"logit\")))",
    "}",
    "reference_calibration"
  )
}

.rls_generalized_verification_r_code <- function(record,
                                                 multiple_imputation = FALSE) {
  fitter <- .rls_generalized_fitter_r_code(record)
  variables <- unique(all.vars(fitter$formula))
  family <- as.character(record$family %||% "gaussian")[[1L]]
  negative_binomial <- isTRUE(record$count_regression) &&
    identical(record$count_distribution %||% "", "negative_binomial")
  bounded_count <- isTRUE(record$count_regression) &&
    .rls_count_distribution_uses_trials(record$count_distribution)
  quasi_likelihood <- identical(record$count_distribution %||% "", "quasipoisson") ||
    family %in% c("quasipoisson", "quasibinomial")

  concise_extension <- identical(family, "gaussian_log") ||
    identical(family, "lognormal") ||
    (isTRUE(record$binary_regression) && identical(record$link, "log"))
  if (concise_extension) {
    formula_line <- paste0(
      "model_formula <- ", .rls_readable_r_formula_text(fitter$formula, record$terms %||% record$predictors)
    )
    binary_prepare <- if (isTRUE(record$binary_regression)) {
      paste0(
        "data[[", .rls_r_string_literal(record$response), "]] <- as.integer(",
        "as.character(data[[", .rls_r_string_literal(record$response), "]]) == ",
        .rls_r_string_literal(record$event %||% "1"), ")"
      )
    } else character()
    fit_expression <- gsub("current_data", "data", fitter$fit, fixed = TRUE)
    verification_table_code <- function(model_name) {
      if (identical(family, "lognormal")) {
        return(c(
          paste0("verification_table <- gtsummary::tbl_regression(", model_name, ","),
          "  intercept = TRUE, conf.int = TRUE, exponentiate = FALSE,",
          "  add_estimate_to_reference_rows = TRUE)",
          "verification_table <- gtsummary::modify_table_body(verification_table, function(data) {",
          "  columns <- intersect(c(\"estimate\", \"conf.low\", \"conf.high\"), names(data))",
          "  data[columns] <- lapply(data[columns], function(value) exp(value))",
          "  data",
          "})",
          "verification_table <- gtsummary::modify_header(",
          "  verification_table, estimate = \"**Multiplicative ratio**\")",
          "verification_table"
        ))
      }
      c(
        paste0("gtsummary::tbl_regression(", model_name, ", intercept = TRUE, conf.int = TRUE,"),
        "  exponentiate = TRUE, add_estimate_to_reference_rows = TRUE)"
      )
    }
    checks <- c(
      "if (!requireNamespace(\"gtsummary\", quietly = TRUE))",
      "  stop(\"Install package 'gtsummary' to present this check.\")",
      if (isTRUE(multiple_imputation)) c(
        "if (!requireNamespace(\"mice\", quietly = TRUE))",
        "  stop(\"Install package 'mice' to pool this check.\")"
      ) else character()
    )
    if (isTRUE(multiple_imputation)) {
      code <- c(
        checks,
        "mi_long <- base::readRDS(verification_data_path)",
        "stopifnot(all(c(\".imp\", \".id\") %in% names(mi_long)))",
        "completed_sets <- split(mi_long[mi_long$.imp > 0L, , drop = FALSE],",
        "                        mi_long$.imp[mi_long$.imp > 0L])",
        "completed_sets <- lapply(completed_sets, function(data) {",
        "  data$.imp <- NULL; data$.id <- NULL; rownames(data) <- NULL; data",
        "})",
        formula_line,
        "fits <- lapply(completed_sets, function(data) {",
        if (length(binary_prepare)) paste0("  ", binary_prepare) else character(),
        paste0("  ", fit_expression),
        "})",
        if (identical(family, "lognormal")) c(
          "# Raw diagnostics are residuals of log(Y), not residuals in original response units.",
          "raw_residuals <- lapply(fits, function(fit) stats::residuals(fit, type = \"response\"))",
          "response_predictions <- lapply(fits, function(fit) {",
          "  eta <- stats::predict(fit)",
          "  sigma_squared <- stats::sigma(fit)^2",
          "  data.frame(median = exp(eta), mean = exp(eta + sigma_squared / 2))",
          "})"
        ) else character(),
        "mira_model <- mice::as.mira(fits)",
        "pooled_model <- mice::pool(mira_model)",
        "summary(pooled_model, conf.int = TRUE)",
        if (identical(family, "lognormal")) {
          .rls_verification_mi_d1_term_code(
            "fits", "completed_sets", fitter$fit
          )
        } else {
          .rls_verification_mi_pooled_wald_term_code("fits")
        },
        verification_table_code("mira_model")
      )
      warnings <- if (identical(family, "lognormal")) {
        "Coefficients are Rubin-pooled on the log-response scale; arithmetic-mean predictions require the imputation-specific residual-variance correction."
      } else "Coefficients are pooled with mice using Rubin's rules."
    } else {
      code <- c(
        checks,
        "analysis_data <- base::readRDS(verification_data_path)",
        formula_line,
        "data <- analysis_data",
        binary_prepare,
        paste0("fit <- ", fit_expression),
        if (identical(family, "lognormal")) c(
          "# Raw diagnostics use the log-response scale, matching the fitted lm.",
          "raw_residual <- stats::residuals(fit, type = \"response\")",
          "eta <- stats::predict(fit)",
          "sigma_squared <- stats::sigma(fit)^2",
          "response_predictions <- data.frame(",
          "  median = exp(eta),",
          "  mean = exp(eta + sigma_squared / 2)",
          ")"
        ) else character(),
        "summary(fit)",
        if (identical(family, "lognormal")) c(
          "# Likelihood of the original response, including the log-transformation Jacobian.",
          "response_log_likelihood <- stats::logLik(fit) -",
          "  sum(stats::model.response(stats::model.frame(fit)))",
          "fit_statistics <- data.frame(logLik = as.numeric(response_log_likelihood),",
          "  AIC = stats::AIC(response_log_likelihood), BIC = stats::BIC(response_log_likelihood))",
          "fit_statistics"
        ) else character(),
        if (identical(family, "lognormal")) {
          .rls_verification_wald_f_term_code("fit")
        } else {
          .rls_verification_wald_chisq_term_code("fit")
        },
        verification_table_code("fit"),
        if (isTRUE(record$binary_regression)) .rls_binary_calibration_verification_code("fit") else character()
      )
      warnings <- if (identical(family, "lognormal")) {
        "The model is fitted on log(response); exp(fitted values) are conditional medians, while arithmetic means use exp(eta + sigma^2 / 2)."
      } else character()
    }
    return(list(
      code = paste(c(
        "# This check refits the requested model from LinkEDA's analysis-ready export.",
        code
      ), collapse = "\n"),
      variables = variables,
      warnings = warnings
    ))
  }

  ceiling_hurdle <- isTRUE(record$count_regression) &&
    identical(record$count_distribution %||% "",
              "hurdle_beta_binomial_ceiling")
  if (ceiling_hurdle) {
    formula_line <- paste0(
      "model_formula <- ", .rls_readable_r_formula_text(fitter$formula, record$terms %||% record$predictors)
    )
    checks <- c(
      "if (!requireNamespace(\"gamlss\", quietly = TRUE) ||",
      "    !requireNamespace(\"gamlss.dist\", quietly = TRUE))",
      "  stop(\"Install packages 'gamlss' and 'gamlss.dist' to run this check.\")",
      if (isTRUE(multiple_imputation)) c(
        "if (!requireNamespace(\"mice\", quietly = TRUE))",
        "  stop(\"Install package 'mice' to pool this check.\")"
      ) else character()
    )
    fit_lines <- c(
      "hurdle_formula <- stats::reformulate(",
      "  attr(stats::terms(model_formula), \"term.labels\")",
      ")",
      "fit_hurdle_model <- function(data) {",
      "  gamlss::gamlss(",
      "    model_formula, nu.formula = hurdle_formula, data = data,",
      "    family = gamlss.dist::ZABB(",
      "      mu.link = \"logit\", sigma.link = \"log\", nu.link = \"logit\"",
      "    ), trace = FALSE",
      "  )",
      "}",
      "component_coefficients <- function(model, component) {",
      "  if (identical(component, \"perfect_score\"))",
      "    stats::coef(model, what = \"nu\")",
      "  else -stats::coef(model, what = \"mu\")",
      "}",
      "component_covariance <- function(model, component) {",
      "  covariance_model <- model",
      "  covariance_model$call$data <- NULL",
      "  full <- as.matrix(stats::vcov(covariance_model))",
      "  n_mu <- length(stats::coef(model, what = \"mu\"))",
      "  n_sigma <- length(stats::coef(model, what = \"sigma\"))",
      "  n_nu <- length(stats::coef(model, what = \"nu\"))",
      "  index <- if (identical(component, \"perfect_score\"))",
      "    n_mu + n_sigma + seq_len(n_nu) else seq_len(n_mu)",
      "  full[index, index, drop = FALSE]",
      "}"
    )
    single_component_lines <- c(
      "component_table <- function(model, component) {",
      "  estimate <- component_coefficients(model, component)",
      "  standard_error <- sqrt(diag(component_covariance(model, component)))",
      "  statistic <- estimate / standard_error",
      "  data.frame(",
      "    term = names(estimate), estimate = unname(estimate),",
      "    std.error = unname(standard_error), statistic = unname(statistic),",
      "    p.value = 2 * stats::pnorm(abs(statistic), lower.tail = FALSE),",
      "    conf.low = unname(estimate - stats::qnorm(.975) * standard_error),",
      "    conf.high = unname(estimate + stats::qnorm(.975) * standard_error),",
      "    row.names = NULL",
      "  )",
      "}"
    )
    if (isTRUE(multiple_imputation)) {
      code <- c(
        checks,
        "mi_long <- base::readRDS(verification_data_path)",
        "stopifnot(all(c(\".imp\", \".id\") %in% names(mi_long)))",
        "completed_sets <- split(",
        "  mi_long[mi_long$.imp > 0L, , drop = FALSE],",
        "  mi_long$.imp[mi_long$.imp > 0L]",
        ")",
        "completed_sets <- lapply(completed_sets, function(data) {",
        "  data$.imp <- NULL; data$.id <- NULL; rownames(data) <- NULL; data",
        "})",
        formula_line, fit_lines,
        "fits <- lapply(completed_sets, fit_hurdle_model)",
        "pool_component <- function(component) {",
        "  terms <- names(component_coefficients(fits[[1L]], component))",
        "  do.call(rbind, lapply(seq_along(terms), function(index) {",
        "    estimates <- vapply(fits, function(model)",
        "      unname(component_coefficients(model, component)[[index]]), numeric(1L))",
        "    variances <- vapply(fits, function(model)",
        "      unname(component_covariance(model, component)[index, index]), numeric(1L))",
        "    pooled <- mice::pool.scalar(estimates, variances)",
        "    pooled$mcse <- sqrt(pooled$b/pooled$m)",
        "    pooled$mcse_percent_se <- 100*pooled$mcse/sqrt(pooled$t)",
        "    print(pooled[c('m','qbar','ubar','b','t','r','df','fmi','mcse','mcse_percent_se')])",
        "    standard_error <- sqrt(pooled$t)",
        "    statistic <- pooled$qbar / standard_error",
        "    critical <- stats::qt(.975, df = pooled$df)",
        "    data.frame(",
        "      term = terms[[index]], estimate = pooled$qbar,",
        "      std.error = standard_error, statistic = statistic, df = pooled$df,",
        "      p.value = 2 * stats::pt(abs(statistic), pooled$df, lower.tail = FALSE),",
        "      conf.low = pooled$qbar - critical * standard_error,",
        "      conf.high = pooled$qbar + critical * standard_error, row.names = NULL",
        "    )",
        "  }))",
        "}",
        "perfect_score_component <- pool_component(\"perfect_score\")",
        "below_ceiling_component <- pool_component(\"below_ceiling\")",
        "dispersion_values <- vapply(fits, function(model) mean(model$sigma.fv), numeric(1L))",
        "dispersion_summary <- c(mean = mean(dispersion_values),",
        "                        min = min(dispersion_values),",
        "                        max = max(dispersion_values))",
        "perfect_score_component",
        "below_ceiling_component",
        "dispersion_summary"
      )
      warnings <- c(
        "The logistic and below-ceiling components are pooled separately with Rubin's rules.",
        "Beta-binomial dispersion is summarized across fitted imputations and is not Rubin-pooled."
      )
    } else {
      code <- c(
        checks,
        "data <- base::readRDS(verification_data_path)",
        formula_line, fit_lines, single_component_lines,
        "fit <- fit_hurdle_model(data)",
        "perfect_score_component <- component_table(fit, \"perfect_score\")",
        "below_ceiling_component <- component_table(fit, \"below_ceiling\")",
        "beta_binomial_dispersion <- mean(fit$sigma.fv)",
        "perfect_score_component",
        "below_ceiling_component",
        "beta_binomial_dispersion"
      )
      warnings <- character()
    }
    return(list(
      code = paste(c(
        "# Exact ceiling-hurdle beta-binomial check using public GAMLSS packages.",
        "# ZABB is fitted to failures: zero failures is a perfect score;",
        "# positive failures follow the beta-binomial distribution truncated above zero.",
        code
      ), collapse = "\n"),
      variables = variables,
      warnings = warnings
    ))
  }

  perfect_score <- isTRUE(record$count_regression) &&
    identical(record$count_distribution %||% "", "perfect_score")
  if (perfect_score) {
    formula_line <- paste0(
      "model_formula <- ", .rls_readable_r_formula_text(fitter$formula, record$terms %||% record$predictors)
    )
    checks <- c(
      "if (!requireNamespace(\"gtsummary\", quietly = TRUE))",
      "  stop(\"Install package 'gtsummary' to present this check.\")",
      if (isTRUE(multiple_imputation)) c(
        "if (!requireNamespace(\"mice\", quietly = TRUE))",
        "  stop(\"Install package 'mice' to pool this check.\")"
      ) else character()
    )
    if (isTRUE(multiple_imputation)) {
      code <- c(
        checks,
        "mi_long <- base::readRDS(verification_data_path)",
        "stopifnot(all(c(\".imp\", \".id\") %in% names(mi_long)))",
        "completed_sets <- split(mi_long[mi_long$.imp > 0L, , drop = FALSE],",
        "                        mi_long$.imp[mi_long$.imp > 0L])",
        "completed_sets <- lapply(completed_sets, function(data) {",
        "  data$.imp <- NULL; data$.id <- NULL; rownames(data) <- NULL; data",
        "})",
        formula_line,
        "fits <- lapply(completed_sets, function(data)",
        "  stats::glm(model_formula, data = data, family = stats::binomial(link = \"logit\")))",
        "mira_model <- mice::as.mira(fits)",
        "pooled_model <- mice::pool(mira_model)",
        "summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)",
        .rls_verification_mi_pooled_wald_term_code("fits", plain_glm = TRUE),
        "gtsummary::tbl_regression(mira_model, intercept = TRUE, conf.int = TRUE,",
        "  exponentiate = TRUE, add_estimate_to_reference_rows = TRUE)"
      )
      warnings <- "Perfect-score logistic coefficients are pooled with mice using Rubin's rules."
    } else {
      code <- c(
        checks,
        "data <- base::readRDS(verification_data_path)",
        formula_line,
        "fit <- stats::glm(model_formula, data = data, family = stats::binomial(link = \"logit\"))",
        "summary(fit)",
        .rls_verification_wald_chisq_term_code("fit"),
        "gtsummary::tbl_regression(fit, intercept = TRUE, conf.int = TRUE,",
        "  exponentiate = TRUE, add_estimate_to_reference_rows = TRUE)"
      )
      warnings <- character()
    }
    return(list(
      code = paste(c(
        "# This check models whether each bounded score attained its own Trials value.",
        code
      ), collapse = "\n"),
      variables = variables,
      warnings = warnings
    ))
  }

  if (negative_binomial) {
    package_checks <- c(
      "if (!requireNamespace(\"MASS\", quietly = TRUE))",
      "  stop(\"Install package 'MASS' to run this check.\")",
      "if (!requireNamespace(\"gtsummary\", quietly = TRUE))",
      "  stop(\"Install package 'gtsummary' to present this check.\")",
      if (isTRUE(multiple_imputation)) c(
        "if (!requireNamespace(\"mice\", quietly = TRUE))",
        "  stop(\"Install package 'mice' to pool this check.\")"
      ) else character()
    )
    formula_line <- paste0(
      "model_formula <- ", .rls_readable_r_formula_text(fitter$formula, record$terms %||% record$predictors)
    )
    table_code <- function(model_name) c(
      "verification_table <- gtsummary::tbl_regression(",
      paste0("  ", model_name, ", intercept = TRUE, conf.int = TRUE, exponentiate = TRUE,"),
      "  add_estimate_to_reference_rows = TRUE",
      ")",
      "verification_table <- gtsummary::modify_table_body(verification_table, function(data) {",
      "  is_reference <- !is.na(data$reference_row) & data$reference_row",
      "  data$label[is_reference] <- paste0(data$label[is_reference], \" (reference)\")",
      "  data",
      "})",
      "if (interactive()) print(verification_table) else verification_table"
    )
    if (isTRUE(multiple_imputation)) {
      code <- c(
        package_checks,
        "mi_long <- base::readRDS(verification_data_path)",
        "stopifnot(all(c(\".imp\", \".id\") %in% names(mi_long)))",
        "completed_sets <- split(",
        "  mi_long[mi_long$.imp > 0L, , drop = FALSE],",
        "  mi_long$.imp[mi_long$.imp > 0L]",
        ")",
        "completed_sets <- lapply(completed_sets, function(data) {",
        "  data$.imp <- NULL",
        "  data$.id <- NULL",
        "  rownames(data) <- NULL",
        "  data",
        "})",
        formula_line,
        "fits <- lapply(completed_sets, function(data) {",
        "  MASS::glm.nb(model_formula, data = data, link = \"log\")",
        "})",
        "mira_model <- mice::as.mira(fits)",
        "pooled_model <- mice::pool(mira_model)",
        "pooled_coefficients <- summary(pooled_model, conf.int = TRUE)",
        "pooled_coefficients",
        "theta_values <- vapply(fits, function(model) model$theta, numeric(1L))",
        "theta_summary <- c(",
        "  mean = mean(theta_values),",
        "  min = min(theta_values),",
        "  max = max(theta_values)",
        ")",
        "theta_summary",
        "nb_variance_inflation <- vapply(fits, function(model)",
        "  1 + mean(stats::fitted(model)) / model$theta, numeric(1L))",
        "mean(nb_variance_inflation) # Descriptive mean, not Rubin-pooled",
        .rls_verification_mi_pooled_wald_term_code("fits"),
        table_code("mira_model")
      )
      warnings <- c(
        "Coefficients are pooled with mice using Rubin's rules; gtsummary presents the pooled model.",
        "Negative-binomial theta is summarized across imputation-specific fits and is not Rubin-pooled."
      )
    } else {
      code <- c(
        package_checks,
        "model_data <- base::readRDS(verification_data_path)",
        formula_line,
        "fit <- MASS::glm.nb(model_formula, data = model_data, link = \"log\")",
        "theta <- fit$theta",
        "theta",
        "1 + mean(stats::fitted(fit)) / theta # Variance ratio at mean fitted count",
        .rls_verification_wald_chisq_term_code("fit"),
        table_code("fit")
      )
      warnings <- character()
    }
    return(list(
      code = paste(c(
        "# This check uses the analysis-ready data exported by LinkEDA.",
        "# It refits the public-package model; theta follows Var(Y) = mu + mu^2 / theta.",
        code
      ), collapse = "\n"),
      variables = variables,
      warnings = warnings
    ))
  }

  if (bounded_count) {
    beta_binomial <- identical(record$count_distribution, "beta_binomial")
    package_checks <- c(
      if (beta_binomial) c(
        "if (!requireNamespace(\"glmmTMB\", quietly = TRUE))",
        "  stop(\"Install package 'glmmTMB' to run this check.\")"
      ) else character(),
      "if (!requireNamespace(\"gtsummary\", quietly = TRUE))",
      "  stop(\"Install package 'gtsummary' to present this check.\")",
      if (isTRUE(multiple_imputation)) c(
        "if (!requireNamespace(\"mice\", quietly = TRUE))",
        "  stop(\"Install package 'mice' to pool this check.\")"
      ) else character()
    )
    formula_line <- paste0(
      "model_formula <- ", .rls_readable_r_formula_text(fitter$formula, record$terms %||% record$predictors)
    )
    fit_expression <- if (beta_binomial) {
      paste(
        "glmmTMB::glmmTMB(model_formula, data = data,",
        "family = glmmTMB::betabinomial(link = \"logit\"))"
      )
    } else {
      "stats::glm(model_formula, data = data, family = stats::binomial(link = \"logit\"))"
    }
    predictions <- c(
      "predicted_probability <- stats::predict(fit, type = \"response\")",
      paste0("trials <- ", if (nzchar(record$trials_variable %||% "")) {
        paste0("data[[", .rls_r_string_literal(record$trials_variable), "]]")
      } else format(record$trials_constant, digits = 17L, scientific = FALSE, trim = TRUE)),
      "predicted_values <- data.frame(",
      "  probability = predicted_probability,",
      "  expected_count = predicted_probability * trials",
      ")"
    )
    table_code <- c(
      "verification_table <- gtsummary::tbl_regression(",
      "  verification_model, intercept = TRUE, conf.int = TRUE, exponentiate = TRUE,",
      "  add_estimate_to_reference_rows = TRUE",
      ")",
      "verification_table"
    )
    if (isTRUE(multiple_imputation)) {
      pooled_coefficient_code <- if (beta_binomial) c(
        "coefficient_names <- names(glmmTMB::fixef(fits[[1L]])$cond)",
        "pooled_coefficients <- do.call(rbind, lapply(coefficient_names, function(term) {",
        "  estimates <- vapply(fits, function(model)",
        "    unname(glmmTMB::fixef(model)$cond[[term]]), numeric(1L))",
        "  variances <- vapply(fits, function(model)",
        "    unname(stats::vcov(model)$cond[term, term]), numeric(1L))",
        "  pooled <- mice::pool.scalar(estimates, variances)",
        "  pooled$mcse <- sqrt(pooled$b/pooled$m)",
        "  pooled$mcse_percent_se <- 100*pooled$mcse/sqrt(pooled$t)",
        "  print(pooled[c('m','qbar','ubar','b','t','r','df','fmi','mcse','mcse_percent_se')])",
        "  standard_error <- sqrt(pooled$t)",
        "  statistic <- pooled$qbar / standard_error",
        "  critical_value <- stats::qt(.975, df = pooled$df)",
        "  data.frame(",
        "    term = term, estimate = pooled$qbar, std.error = standard_error,",
        "    statistic = statistic, df = pooled$df,",
        "    p.value = 2 * stats::pt(abs(statistic), df = pooled$df, lower.tail = FALSE),",
        "    conf.low = pooled$qbar - critical_value * standard_error,",
        "    conf.high = pooled$qbar + critical_value * standard_error,",
        "    row.names = NULL",
        "  )",
        "}))",
        "pooled_coefficients"
      ) else c(
        "mira_model <- mice::as.mira(fits)",
        "pooled_model <- mice::pool(mira_model)",
        "summary(pooled_model, conf.int = TRUE)"
      )
      code <- c(
        package_checks,
        "mi_long <- base::readRDS(verification_data_path)",
        "stopifnot(all(c(\".imp\", \".id\") %in% names(mi_long)))",
        "completed_sets <- split(mi_long[mi_long$.imp > 0L, , drop = FALSE],",
        "                        mi_long$.imp[mi_long$.imp > 0L])",
        "completed_sets <- lapply(completed_sets, function(data) {",
        "  data$.imp <- NULL; data$.id <- NULL; rownames(data) <- NULL; data",
        "})",
        formula_line,
        "fits <- lapply(completed_sets, function(data) {",
        paste0("  ", fit_expression),
        "})",
        pooled_coefficient_code,
        "fit <- fits[[1L]]",
        "data <- completed_sets[[1L]]",
        predictions,
        if (beta_binomial) c(
          "precision_values <- vapply(fits, stats::sigma, numeric(1L))",
          "c(mean = mean(precision_values), min = min(precision_values), max = max(precision_values))",
          "rho_by_imputation <- 1 / (1 + precision_values)",
          "mean(rho_by_imputation) # Descriptive mean, not Rubin-pooled",
          paste0("trial_counts <- lapply(completed_sets, function(data) ",
            if (nzchar(record$trials_variable %||% ""))
              paste0("data[[", .rls_r_string_literal(record$trials_variable), "]]")
            else format(record$trials_constant, digits = 17L, scientific = FALSE, trim = TRUE), ")"),
          "variance_ratio_by_imputation <- vapply(seq_along(fits), function(i)",
          "  mean(1 + (trial_counts[[i]] - 1) * rho_by_imputation[[i]]), numeric(1L))",
          "mean(variance_ratio_by_imputation) # Mean of observation-specific ratios"
        ) else c(
          "pearson_ratio_by_imputation <- vapply(seq_along(fits), function(i) {",
          paste0("  trials <- ", if (nzchar(record$trials_variable %||% ""))
            paste0("completed_sets[[i]][[", .rls_r_string_literal(record$trials_variable), "]]")
          else format(record$trials_constant, digits = 17L, scientific = FALSE, trim = TRUE)),
          "  df <- stats::df.residual(fits[[i]])",
          "  if (!any(trials > 1) || df <= 0) return(NA_real_)",
          "  sum(stats::residuals(fits[[i]], type = 'pearson')^2) / df",
          "}, numeric(1L))",
          "if (all(is.finite(pearson_ratio_by_imputation))) mean(pearson_ratio_by_imputation)"
        ),
        .rls_verification_mi_pooled_wald_term_code("fits"),
        if (!beta_binomial) c("verification_model <- mira_model", table_code)
        else character()
      )
      warnings <- c(
        if (beta_binomial) {
          "Mean-submodel coefficients use coefficient-wise Rubin pooling with mice::pool.scalar; predictions shown in this recipe use the first completed set for inspection."
        } else {
          "Coefficients and tests use multiple-imputation pooling; predictions shown in this recipe use the first completed set for inspection."
        },
        if (beta_binomial) "Beta-binomial precision is summarized across imputed-model estimates and is not Rubin-pooled." else character()
      )
    } else {
      code <- c(
        package_checks,
        "data <- base::readRDS(verification_data_path)",
        formula_line,
        paste0("fit <- ", fit_expression),
        predictions,
        if (beta_binomial) c(
          "beta_binomial_precision <- stats::sigma(fit)",
          "rho <- 1 / (1 + beta_binomial_precision)",
          "mean(1 + (trials - 1) * rho) # Variance ratio vs binomial"
        ) else c(
          "if (any(trials > 1) && stats::df.residual(fit) > 0)",
          "  sum(stats::residuals(fit, type = 'pearson')^2) / stats::df.residual(fit)"
        ),
        .rls_verification_wald_chisq_term_code("fit"),
        "verification_model <- fit",
        table_code
      )
      warnings <- character()
    }
    return(list(
      code = paste(c(
        "# This check refits the bounded-count model from LinkEDA's analysis-ready export.",
        "# The cbind(successes, trials - successes) response is explicit in model_formula.",
        code
      ), collapse = "\n"),
      variables = variables,
      warnings = warnings
    ))
  }

  setup <- .rls_model_setup_r_code(record, fitter$formula)
  package_checks <- c(
    "if (!requireNamespace(\"gtsummary\", quietly = TRUE))",
    "  stop(\"Install package 'gtsummary' to present this check.\")",
    if (identical(family, "beta")) c(
      "if (!requireNamespace(\"betareg\", quietly = TRUE))",
      "  stop(\"Install package 'betareg' to run this check.\")"
    ) else character(),
    if (identical(family, "beta_one_inflated")) c(
      "if (!requireNamespace(\"gamlss\", quietly = TRUE) ||",
      "    !requireNamespace(\"gamlss.dist\", quietly = TRUE))",
      "  stop(\"Install packages 'gamlss' and 'gamlss.dist' to run this check.\")"
    ) else character(),
    if (isTRUE(multiple_imputation)) c(
      "if (!requireNamespace(\"mice\", quietly = TRUE))",
      "  stop(\"Install package 'mice' to pool this check.\")"
    ) else character()
  )
  bounds <- if (length(record$response_bounds %||% numeric()) == 2L) {
    paste0("response_bounds <- c(",
           format(record$response_bounds[[1L]], digits = 17L, scientific = FALSE), ", ",
           format(record$response_bounds[[2L]], digits = 17L, scientific = FALSE), ")")
  } else "response_bounds <- NULL"
  transformation <- if (family %in% c("beta", "beta_one_inflated")) {
    paste0(
      "current_data[[", .rls_r_string_literal(record$response), "]] <- ",
      "(current_data[[", .rls_r_string_literal(record$response), "]] - response_bounds[[1L]]) / ",
      "(response_bounds[[2L]] - response_bounds[[1L]])"
    )
  } else if (identical(family, "gamma_distance")) {
    paste0(
      "current_data[[", .rls_r_string_literal(record$response), "]] <- ",
      "response_bounds[[2L]] - current_data[[",
      .rls_r_string_literal(record$response), "]]"
    )
  } else character()
  binary_transformation <- if (isTRUE(record$binary_regression)) {
    paste0(
      "current_data[[", .rls_r_string_literal(record$response), "]] <- as.integer(",
      "as.character(current_data[[", .rls_r_string_literal(record$response), "]]) == ",
      .rls_r_string_literal(record$event %||% "1"), ")"
    )
  } else character()
  fit_block <- c(
    binary_transformation,
    transformation,
    paste0("reference_fit <- ", fitter$fit)
  )
  exponentiate <- (isTRUE(record$binary_regression) &&
                   identical(record$link %||% "logit", "logit")) ||
    isTRUE(record$count_regression)
  table_block <- c(
    "reference_table <- gtsummary::tbl_regression(",
    paste0("  reference_model, intercept = TRUE, conf.int = TRUE, exponentiate = ",
           if (exponentiate) "TRUE," else "FALSE,"),
    "  add_estimate_to_reference_rows = TRUE",
    ")",
    "reference_table <- gtsummary::modify_table_body(reference_table, function(data) {",
    "  is_reference <- !is.na(data$reference_row) & data$reference_row",
    "  data$label[is_reference] <- paste0(data$label[is_reference], \" (reference)\")",
    "  data",
    "})",
    "if (interactive()) print(reference_table) else reference_table"
  )
  family_diagnostic_mi <- if (identical(family, "poisson") &&
      !quasi_likelihood) c(
    "pearson_dispersion_by_imputation <- vapply(reference_fits, function(model) {",
    "  df <- stats::df.residual(model)",
    "  if (df <= 0) return(NA_real_)",
    "  sum(stats::residuals(model, type = 'pearson')^2) / df",
    "}, numeric(1L))",
    "if (all(is.finite(pearson_dispersion_by_imputation))) mean(pearson_dispersion_by_imputation)"
  ) else if (family %in% c("Gamma", "gamma_distance")) c(
    "gamma_cv_by_imputation <- vapply(reference_fits, function(model)",
    "  sqrt(summary(model)$dispersion), numeric(1L))",
    "mean(gamma_cv_by_imputation) # Transform each fit before averaging"
  ) else character()
  family_diagnostic_single <- if (identical(family, "poisson") &&
      !quasi_likelihood) c(
    "df <- stats::df.residual(reference_fit)",
    "if (df > 0) sum(stats::residuals(reference_fit, type = 'pearson')^2) / df"
  ) else if (family %in% c("Gamma", "gamma_distance")) c(
    "sqrt(summary(reference_fit)$dispersion) # Conditional Gamma CV"
  ) else character()
  if (isTRUE(multiple_imputation)) {
    code <- c(
      package_checks,
      "mi_long <- base::readRDS(verification_data_path)",
      "stopifnot(all(c(\".imp\", \".id\") %in% names(mi_long)))",
      "completed_sets <- split(mi_long[mi_long$.imp > 0L, , drop = FALSE],",
      "                        mi_long$.imp[mi_long$.imp > 0L])",
      "completed_sets <- lapply(completed_sets, function(data) {",
      "  data$.imp <- NULL; data$.id <- NULL; rownames(data) <- NULL; data",
      "})",
      setup, bounds,
      .rls_model_preparation_r_code("completed_sets[[1L]]", "analysis_data"),
      "prepared_sets <- lapply(completed_sets, prepare_linkeda_model_data)",
      "reference_fits <- lapply(prepared_sets, function(current_data) {",
      paste0("  ", fit_block),
      "  reference_fit",
      "})",
      family_diagnostic_mi,
      "reference_mira <- mice::as.mira(reference_fits)",
      "reference_model <- reference_mira",
      "reference_pooled_model <- mice::pool(reference_mira)",
      .rls_verification_mi_pooled_wald_term_code("reference_fits"),
      table_block
    )
  } else {
    code <- c(
      package_checks,
      "analysis_data <- base::readRDS(verification_data_path)",
      setup, bounds,
      .rls_model_preparation_r_code("analysis_data", "analysis_data"),
      "current_data <- analysis_data",
      fit_block,
      family_diagnostic_single,
      "reference_model <- reference_fit",
      if (quasi_likelihood) {
        .rls_verification_wald_f_term_code("reference_fit")
      } else {
        .rls_verification_wald_chisq_term_code("reference_fit")
      },
      table_block,
      if (isTRUE(record$binary_regression)) .rls_binary_calibration_verification_code("reference_fit") else character()
    )
  }
  warnings <- if (isTRUE(multiple_imputation)) {
    "Coefficients are pooled with mice using Rubin's rules; gtsummary presents the pooled model."
  } else character()
  list(
    code = paste(c(
      "# This check uses the prepared data exported by LinkEDA.",
      "# It recalculates the model with public package functions and presents a semantic regression table.",
      code
    ), collapse = "\n"),
    variables = variables,
    warnings = warnings
  )
}

.rls_linear_output_r_code <- function(multiple_imputation = FALSE) {
  if (isTRUE(multiple_imputation)) {
    return(list(
      model = paste(
        "model_summary <- list(",
        "  coefficients = summary(pooled_coefficients, conf.int = TRUE),",
        "  r_squared = pooled_r_squared,",
        "  adjusted_r_squared = pooled_adjusted_r_squared,",
        "  global_test = pooled_global_test",
        ")",
        sep = "\n"
      ),
      coefficients = "coefficient_table <- summary(pooled_coefficients, conf.int = TRUE)",
      model_fit = paste(
        "model_fit <- list(r_squared = pooled_r_squared,",
        "                  adjusted_r_squared = pooled_adjusted_r_squared,",
        "                  global_test = pooled_global_test)",
        sep = "\n"
      )
    ))
  }
  list(
    model = "model_summary <- summary(model)\ncoefficient_table <- stats::coef(model_summary)",
    coefficients = "coefficient_table <- stats::coef(summary(model))",
    model_fit = "model_summary <- summary(model)"
  )
}

.rls_generalized_output_r_code <- function(multiple_imputation = FALSE) {
  if (isTRUE(multiple_imputation)) {
    return(list(
      model = "model_summary <- summary(pooled_coefficients, conf.int = TRUE)",
      coefficients = "coefficient_table <- summary(pooled_coefficients, conf.int = TRUE)",
      model_fit = paste(
        "model_fit <- list(",
        "  pooled_coefficients = pooled_coefficients,",
        "  fits_by_imputation = fits",
        ")",
        sep = "\n"
      )
    ))
  }
  list(
    model = "model_summary <- summary(model)\ncoefficient_table <- stats::coef(model_summary)",
    coefficients = "coefficient_table <- stats::coef(summary(model))",
    model_fit = "model_summary <- summary(model)"
  )
}

.rls_regression_comparison_executed_r_code <- function(record) {
  multiple_imputation <- identical(
    record$analysis_backend %||% "ordinary", "multiple_imputation"
  ) && isTRUE(record$mi_pooling_required)
  model_names <- make.unique(vapply(seq_along(record$models), function(index) {
    make.names(record$models[[index]]$label %||% paste("Model", index))
  }, character(1L)))
  model_lines <- unlist(lapply(seq_along(record$models), function(index) {
    model <- record$models[[index]]
    response <- model$response %||% record$response
    formula <- .rls_model_formula_object(response, model$terms %||% character())
    label <- model_names[[index]]
    specification <- list(
      term_types = model$term_types %||% record$term_types %||% list(),
      factor_reference_levels = model$factor_reference_levels %||% list(),
      centered_predictors = model$centered_predictors %||% character()
    )
    setup <- .rls_model_setup_r_code(specification, formula)
    if (multiple_imputation) {
      c(
        paste0("# ", model$label %||% paste("Model", index)),
        paste0(label, "_formula <- ", .rls_r_formula_text(formula)),
        gsub("model_formula", paste0(label, "_formula"), setup, fixed = TRUE),
        gsub("model_formula", paste0(label, "_formula"),
            .rls_model_preparation_r_code("completed_sets[[1L]]", paste0(label, "_data")),
            fixed = TRUE),
        paste0(label, "_fits <- lapply(imputed_analysis_data, function(current_data) {"),
        "  current_data <- prepare_linkeda_model_data(current_data)",
        paste0("  stats::lm(", label,
               "_formula, data = current_data, na.action = stats::na.fail)"),
        "})",
        paste0(label, "_pooled <- mice::pool(mice::as.mira(", label, "_fits))")
      )
    } else {
      c(
        paste0("# ", model$label %||% paste("Model", index)),
        paste0(label, "_formula <- ", .rls_r_formula_text(formula)),
        gsub("model_formula", paste0(label, "_formula"), setup, fixed = TRUE),
        gsub("model_formula", paste0(label, "_formula"),
            .rls_model_preparation_r_code("analysis_data", paste0(label, "_data")),
            fixed = TRUE),
        paste0(label, " <- stats::lm(", label,
               "_formula, data = ", label, "_data, na.action = stats::na.fail)")
      )
    }
  }), use.names = FALSE)
  comparison_object_names <- if (length(record$models) < 2L) character() else
    paste0("comparison_", seq_len(length(record$models) - 1L), "_",
           seq.int(2L, length(record$models)))
  comparison_lines <- if (!length(comparison_object_names)) character() else
    if (multiple_imputation) {
      c(
        "# Nested comparisons test added or removed terms against the preceding model.",
        "# LinkEDA applies its R-side mice::D1 implementation; mice::D3 is used only when",
        "# D1 has undefined denominator degrees of freedom. Statistics are never averaged.",
        unlist(lapply(2:length(model_names), function(index) paste0(
          comparison_object_names[[index - 1L]], " <- LinkEDA:::.rls_regcmp_mi_nested_test(",
          "list(terms = ", .rls_r_character_vector(record$models[[index - 1L]]$terms),
          ", fits_by_imputation = ", model_names[[index - 1L]], "_fits), ",
          "list(terms = ", .rls_r_character_vector(record$models[[index]]$terms),
          ", fits_by_imputation = ", model_names[[index]], "_fits))"
        )), use.names = FALSE)
      )
    } else {
      unlist(lapply(2:length(model_names), function(index) paste0(
        comparison_object_names[[index - 1L]], " <- LinkEDA:::.rls_regcmp_ordinary_nested_test(",
        "list(fit = ", model_names[[index - 1L]], "), list(fit = ", model_names[[index]], "))"
      )), use.names = FALSE)
    }
  paste(c(
    if (multiple_imputation)
      c(
        "completed_sets <- LinkEDA::ls_complete_data_version(scoped_source_data)",
        "imputed_analysis_data <- completed_sets"
      )
    else character(),
    model_lines,
    paste0("models <- list(", paste(
      if (multiple_imputation) paste0(model_names, "_pooled") else model_names,
      collapse = ", "), ")"),
    comparison_lines,
    paste0("comparisons <- list(", paste(comparison_object_names, collapse = ", "), ")")
  ), collapse = "\n")
}

# Freeze each model's actual input independently; comparisons may have different
# scopes or missing-case exclusions. This uses the shared prepared-RDS export.
.rls_generalized_comparison_inputs <- function(records, versions) {
  if (anyNA(versions)) stop("Refit models without a recorded data version before exporting a comparison.", call. = FALSE)
  source <- ls_get_data_version(records[[1L]]$group, versions[[1L]])
  completed <- ls_complete_data_version(source)
  stable_ids <- ls_row_ids(source)
  scopes <- lapply(records, function(record) {
    scope <- record$data_scope %||% record$analysis_provenance$analysis_scope
    if (is.null(scope)) stop("Refit models without a recorded analysis scope before comparing them.", call. = FALSE)
    scope
  })
  scope <- scopes[[1L]]
  requested <- unique(unlist(lapply(scopes, `[[`, "rows"), use.names = FALSE))
  if (!all(vapply(scopes, function(x) identical(x$rows, scope$rows), logical(1L)))) {
    scope$rows <- sort(as.integer(requested))
    scope$kind <- "explicit"; scope$source <- scope$source_kind <- "other_explicit_subset"
    scope$description <- "Union of fitted model scopes; each model retains its own cases"
    scope$fit_scope <- "selected"
  }
  scope$n <- length(scope$rows)
  scope$total_n <- length(stable_ids)
  scope$dataset_version <- versions[[1L]]
  identities <- unlist(unname(lapply(scopes, function(x) stats::setNames(x$stable_row_ids, x$rows))))
  scope$stable_row_ids <- unname(identities[as.character(scope$rows)])
  positions <- match(scope$stable_row_ids, stable_ids)
  if (anyNA(positions)) stop("Fitted scope rows are missing from the recorded data version.", call. = FALSE)
  variables <- unique(unlist(lapply(records, function(record) all.vars(.rls_generalized_glm_formula_object(record)))))
  prepared <- lapply(completed, function(data) {
    data <- .rls_subset_data_rows(data, positions)[, variables, drop = FALSE]
    attr(data, "linkeda_row_ids") <- scope$stable_row_ids
    data
  })
  model_rows <- lapply(records, function(record) {
    rows <- if (identical(record$analysis_backend, "multiple_imputation")) {
      lapply(record$multiple_imputation$complete_data_result_by_imputation,
        function(x) as.integer(x$original_rows %||% x$rows))
    } else list(as.integer(record$rows_used))
    if (length(rows) != length(completed)) stop("The fitted imputation inputs are incomplete; refit the models.", call. = FALSE)
    lapply(rows, function(ids) {
      positions <- match(ids, scope$rows)
      if (anyNA(positions)) stop("Fitted rows are missing from the recorded scope.", call. = FALSE)
      positions
    })
  })
  data <- if (identical(records[[1L]]$analysis_backend, "multiple_imputation")) {
    do.call(rbind, lapply(seq_along(prepared), function(i)
      data.frame(.imp=i, .id=scope$rows, prepared[[i]], check.names=FALSE)))
  } else prepared[[1L]]
  list(scope = scope, data = data, model_rows = model_rows,
       imputation_count = if (identical(records[[1L]]$analysis_backend, "multiple_imputation")) length(completed) else 0L,
       used = sort(unique(as.integer(unlist(lapply(records, `[[`, "rows_used"))))))
}

.rls_generalized_comparison_verification_r_code <- function(records, tests, model_rows) {
  mi <- identical(records[[1L]]$analysis_backend, "multiple_imputation")
  blocks <- vapply(seq_along(records), function(i) {
    # The shared ordinary refit block contains only public package calls and
    # model preparation. Reuse it for each frozen completion, without pooling
    # or embedding previously computed statistical results.
    refit <- .rls_generalized_executed_r_code(records[[i]], FALSE)
    paste0("model_fits[[", i, "]] <- lapply(model_data[[", i, "]], function(analysis_data) {\n",
      refit, "\n  model\n})")
  }, character(1L))
  comparisons <- vapply(seq_len(nrow(tests)), function(i) {
    test <- tests[i, ]
    name <- paste0("reference_comparisons[[", i, "]]")
    # Invalid comparisons must not turn into tests in the verification export.
    if (!isTRUE(test$available)) return(paste0(name, " <- list(available = FALSE, reason = ",
      .rls_r_string_literal(test$reason), ")"))
    ids <- vapply(records, `[[`, character(1L), "id")
    full <- match(test$full, ids); reduced <- match(test$reduced, ids)
    prefix <- c(paste0("full <- model_fits[[", full, "]]"),
                paste0("reduced <- model_fits[[", reduced, "]]"))
    calculation <- if (mi) {
      if (grepl("mitml::", test$method, fixed = TRUE)) {
        c("# D1 on the added mean coefficients via public mitml constraints.",
          "mean_coef <- function(fit) if (inherits(fit, 'betareg')) stats::coef(fit, model='mean') else stats::coef(fit, what='mu')",
          "mean_vcov <- function(fit) {",
          "  if (inherits(fit, 'betareg')) return(stats::vcov(fit, model='mean'))",
          "  index <- seq_along(mean_coef(fit)); fit$call$data <- NULL",
          "  as.matrix(stats::vcov(fit))[index,index,drop=FALSE]",
          "}",
          "added <- setdiff(names(mean_coef(full[[1L]])), names(mean_coef(reduced[[1L]])))",
          "qhat <- lapply(full, function(fit) mean_coef(fit)[added])",
          "uhat <- lapply(full, function(fit) mean_vcov(fit)[added,added,drop=FALSE])",
          "# Safe temporary parameter names affect names only, not estimates.",
          "parameter_names <- paste0('b',seq_along(added))",
          "qhat <- lapply(qhat, stats::setNames, parameter_names)",
          "uhat <- lapply(uhat, function(x) {dimnames(x) <- list(parameter_names,parameter_names);x})",
          "pooled <- mitml::testConstraints(qhat=qhat, uhat=uhat, constraints=parameter_names, method='D1')",
          "value <- pooled$test[1L, ]",
          paste0(name, " <- c(statistic=unname(value[1]),df1=unname(value[2]),df2=unname(value[3]),p=unname(value[4]))"))
      } else {
        c(paste0("pooled <- mice::", if(grepl("mice::D3",test$method,fixed=TRUE)) "D3" else "D1",
            "(mice::as.mira(full), mice::as.mira(reduced))"),
          "value <- if (is.matrix(pooled$result) || is.data.frame(pooled$result)) pooled$result[1L, ] else pooled$result",
          "if (is.null(names(value))) names(value) <- c('F.value','df1','df2','P(>F)','RIV')",
          if (!grepl("mice::D3",test$method,fixed=TRUE)) c(
            "# Preserve the complete-data limit used at zero between-imputation variation.",
            "if ((!is.finite(value[['df2']]) || !is.finite(value[['P(>F)']])) &&",
            "    is.finite(value[['RIV']]) && abs(value[['RIV']]) <= sqrt(.Machine$double.eps) &&",
            "    is.finite(pooled$dfcom) && pooled$dfcom > 0) {",
            "  value[['df2']] <- pooled$dfcom",
            "  value[['P(>F)']] <- stats::pf(value[['F.value']],value[['df1']],pooled$dfcom,lower.tail=FALSE)",
            "}") else character(),
          paste0(name, " <- c(statistic=unname(value[['F.value']]),df1=unname(value[['df1']]),df2=unname(value[['df2']]),p=unname(value[['P(>F)']]))"))
      }
    } else if (identical(test$statistic_label, "F")) {
      c("tab <- stats::anova(reduced[[1L]],full[[1L]],test='F')",
        paste0(name," <- c(statistic=tab$F[2],df1=abs(tab$Df[2]),df2=stats::df.residual(full[[1L]]),p=tab[['Pr(>F)']][2])"))
    } else if (isTRUE(records[[full]]$count_regression) || records[[full]]$family %in% c("beta","beta_one_inflated")) {
      c("ll_full <- stats::logLik(full[[1L]]); ll_reduced <- stats::logLik(reduced[[1L]])",
        "statistic <- 2 * as.numeric(ll_full - ll_reduced)",
        "df <- attr(ll_full,'df') - attr(ll_reduced,'df')",
        paste0(name," <- c(statistic=statistic,df1=df,p=stats::pchisq(max(0,statistic),df,lower.tail=FALSE))"))
    } else {
      c("tab <- stats::anova(reduced[[1L]],full[[1L]],test='Chisq')",
        "# R uses the larger model's dispersion to scale the deviance reduction.",
        "statistic <- tab$Deviance[2] / summary(full[[1L]])$dispersion",
        paste0(name," <- c(statistic=statistic,df1=abs(tab$Df[2]),p=tab[['Pr(>Chi)']][2])"))
    }
    paste(c(prefix,calculation),collapse="\n")
  }, character(1L))
  list(code = paste(c(
    "# Refit each model from its own frozen analysis cases, with public R packages.",
    "# Rows below are positions within the frozen exported scope, before model-specific exclusions.",
    "analysis_data <- base::readRDS(verification_data_path)",
    if (mi) c(
      "stopifnot(all(c('.imp','.id') %in% names(analysis_data)))",
      "completed_sets <- split(analysis_data[analysis_data$.imp > 0L, , drop=FALSE], analysis_data$.imp[analysis_data$.imp > 0L])",
      "completed_sets <- lapply(completed_sets, function(data) {data$.imp <- NULL; data$.id <- NULL; data})"
    ) else "completed_sets <- list(analysis_data)",
    paste0("model_rows <- ", paste(deparse(unname(model_rows),width.cutoff=500L),collapse="\n")),
    "model_data <- lapply(model_rows, function(rows) {",
    "  stopifnot(length(rows) == length(completed_sets))",
    "  Map(function(data, index) {",
    "    stopifnot(all(index >= 1L & index <= nrow(data)))",
    "    data[index, , drop=FALSE]",
    "  }, completed_sets, rows)",
    "})",
    "model_fits <- vector('list',length(model_data))", blocks,
    paste0("reference_comparisons <- vector('list',",nrow(tests),"L)"),comparisons,
    "print(reference_comparisons)"),collapse="\n"),
    variables=unique(unlist(lapply(records,function(x)all.vars(.rls_generalized_glm_formula_object(x))))),
    warnings="Prepared data preserve each model's fitted cases and imputations; unavailable comparisons remain explicitly unavailable.")
}

.rls_generalized_comparison_executed_r_code <- function(records) {
  labels <- make.unique(vapply(seq_along(records), function(index) {
    make.names(records[[index]]$label %||% paste("Model", index))
  }, character(1L)))
  model_blocks <- unlist(lapply(seq_along(records), function(index) {
    record <- records[[index]]
    code <- record$analysis_provenance$executed_r_code %||%
      .rls_generalized_executed_r_code(
        record,
        identical(record$analysis_backend %||% "ordinary", "multiple_imputation")
      )
    result_expression <- if (identical(
      record$analysis_backend %||% "ordinary", "multiple_imputation"
    )) {
      "list(fits = fits, pooled = pooled_coefficients)"
    } else "model"
    c(
      paste0("# ", record$label %||% paste("Model", index)),
      paste0(labels[[index]], " <- local({"),
      paste0("  ", strsplit(code, "\n", fixed = TRUE)[[1L]]),
      paste0("  ", result_expression),
      "})"
    )
  }), use.names = FALSE)
  multiple_imputation <- all(vapply(records, function(record) identical(
    record$analysis_backend %||% "ordinary", "multiple_imputation"
  ), logical(1L)))
  comparisons <- if (length(records) < 2L) character() else
    unlist(lapply(2:length(records), function(index) {
      left <- labels[[index - 1L]]
      right <- labels[[index]]
      if (multiple_imputation) {
        paste0(
          "comparison_", index - 1L, "_", index,
          " <- LinkEDA:::.rls_mi_pool_d1(", right, "$fits, ",
          left, "$fits, term_names = ",
          .rls_r_character_vector(setdiff(
            records[[index]]$terms %||% character(),
            records[[index - 1L]]$terms %||% character()
          )), ")"
        )
      } else if (isTRUE(records[[index]]$count_regression) ||
                 records[[index]]$family %in% c("beta", "beta_one_inflated")) {
        paste0(
          "comparison_", index - 1L, "_", index,
          " <- LinkEDA:::.rls_generalized_likelihood_ratio(", left,
          ", ", right, ")"
        )
      } else if (records[[index]]$family %in% c("gaussian_log", "lognormal")) {
        paste0(
          "comparison_", index - 1L, "_", index,
          " <- stats::anova(", left, ", ", right, ", test = \"F\")"
        )
      } else {
        paste0(
          "comparison_", index - 1L, "_", index,
          " <- stats::anova(", left, ", ", right, ", test = \"Chisq\")"
        )
      }
    }), use.names = FALSE)
  paste(c(
    model_blocks,
    paste0("models <- list(", paste(labels, collapse = ", "), ")"),
    comparisons,
    paste0("comparisons <- list(", paste(
      if (length(records) < 2L) character() else
        paste0("comparison_", seq_len(length(records) - 1L), "_",
               seq.int(2L, length(records))),
      collapse = ", "), ")")
  ), collapse = "\n")
}

.rls_table1_reference_test_code <- function(record) {
  group <- record$group_variable %||% ""
  if (!nzchar(group) || !(isTRUE(record$display_options$show_p) || isTRUE(record$display_options$show_test)))
    return("reference_tests <- list()")
  variables <- as.character(record$variables %||% character())
  types <- record$variable_types %||% character()
  blocks <- vapply(variables, function(variable) {
    type <- unname(types[[variable]] %||% "numeric")
    saved <- record$test_results[[variable]] %||% list()
    expression <- if (identical(type, "numeric")) {
      "if (nlevels(g) == 2L) stats::t.test(x ~ g, var.equal = FALSE) else stats::oneway.test(x ~ g, var.equal = FALSE)"
    } else if (identical(type, "ordinal")) {
      "if (nlevels(g) == 2L) stats::wilcox.test(as.numeric(x) ~ g, exact = FALSE) else stats::kruskal.test(as.numeric(x) ~ g)"
    } else {
      test <- if (!is.null(saved$simulation_seed)) paste0(
        "withr::with_seed(", as.integer(saved$simulation_seed), "L, stats::chisq.test(tab, simulate.p.value = TRUE, B = ",
        as.integer(saved$simulation_replicates), "L), .rng_kind = 'Mersenne-Twister', .rng_normal_kind = 'Inversion', .rng_sample_kind = 'Rejection')")
      else if (identical(saved$test, "Fisher exact")) "stats::fisher.test(tab)"
      else "stats::chisq.test(tab, correct = FALSE)"
      paste0("{ tab <- table(droplevels(factor(x)), g); ", test, " }")
    }
    paste0("  ", .rls_r_string_literal(variable), " = local({\n",
      "    x <- analysis_data[[", .rls_r_string_literal(variable), "]]\n",
      "    g <- analysis_data[[", .rls_r_string_literal(group), "]]\n",
      "    keep <- stats::complete.cases(x, g); x <- x[keep]; g <- droplevels(factor(g[keep]))\n",
      "    tryCatch(", expression, ", error = function(e) list(unavailable = conditionMessage(e)))\n  })")
  }, character(1L))
  paste0("reference_tests <- list(\n", paste(blocks, collapse = ",\n"), "\n)")
}

.rls_table1_verification_r_code <- function(record) {
  variables <- as.character(record$variables %||% character())
  group <- record$group_variable %||% ""
  include <- unique(c(variables, if (nzchar(group)) group else character()))
  continuous <- names(record$variable_types)[
    unlist(record$variable_types, use.names = FALSE) == "numeric"
  ]
  ordinal <- names(record$variable_types)[
    unlist(record$variable_types, use.names = FALSE) == "ordinal"
  ]
  categorical <- names(record$variable_types)[
    unlist(record$variable_types, use.names = FALSE) == "categorical"
  ]
  numeric_stats <- record$display_options$numeric_stats %||% "mean_sd"
  continuous_statistics <- c(
    if ("mean_sd" %in% numeric_stats) c("{mean}", "{sd}") else character(),
    if ("median_iqr" %in% numeric_stats) c("{median}", "{p25}", "{p75}") else character()
  )
  if (!length(continuous_statistics)) continuous_statistics <- c("{mean}", "{sd}")
  type_lines <- c(
    if (length(continuous))
      "    tidyselect::all_of(numeric_variables) ~ \"continuous2\"" else character(),
    if (length(c(categorical, ordinal)))
      "    tidyselect::all_of(categorical_variables) ~ \"categorical\"" else character()
  )
  header <- c(
    "# This check uses the prepared data exported by LinkEDA.",
    "# It does not independently verify how derived columns or selections were created.",
    "if (!requireNamespace(\"gtsummary\", quietly = TRUE))",
    "  stop(\"Install package 'gtsummary' to run this check.\")",
    "analysis_data <- base::readRDS(verification_data_path)",
    paste0("table_variables <- ", .rls_r_character_vector(variables)),
    paste0("numeric_variables <- ", .rls_r_character_vector(continuous)),
    paste0("categorical_variables <- ",
           .rls_r_character_vector(c(categorical, ordinal)))
  )
  summary_lines <- c(
    "reference_table <- gtsummary::tbl_summary(",
    "  data = analysis_data,",
    if (nzchar(group)) paste0("  by = ", .rls_r_string_literal(group), ",") else character(),
    "  include = tidyselect::all_of(table_variables),",
    "  type = list(",
    paste(type_lines, collapse = ",\n"),
    "  ),",
    "  statistic = list(",
    paste0("    gtsummary::all_continuous() ~ ",
           .rls_r_character_vector(continuous_statistics), ","),
    "    gtsummary::all_categorical() ~ \"{n} ({p}%)\"",
    "  ),",
    paste0("  missing = ", if (isTRUE(record$display_options$include_missing)) "\"ifany\"" else "\"no\"", ","),
    "  percent = \"column\"",
    ")",
    if (isTRUE(record$display_options$show_n)) {
      if (nzchar(group)) {
        paste0("reference_group_n <- c(Overall = nrow(analysis_data), table(droplevels(factor(analysis_data[[",
               .rls_r_string_literal(group), "]])), useNA = \"no\"))")
      } else "reference_group_n <- c(Overall = nrow(analysis_data))"
    } else character(),
    if (isTRUE(record$display_options$show_n)) paste0("names(reference_group_n) <- ",
      .rls_r_character_vector(setdiff(names(record$display_table), c("Variable", "p", "Test"))))
  )
  ordinal_lines <- if (length(ordinal) &&
                        "median_iqr" %in% (record$display_options$ordinal_stats %||% character())) c(
    "# LinkEDA also treats these ordered factors as ordinal scores for medians/tests:",
    paste0("ordinal_variables <- ", .rls_r_character_vector(ordinal)),
    "reference_ordinal_quantiles <- lapply(ordinal_variables, function(variable) {",
    "  stats::quantile(as.numeric(analysis_data[[variable]]),",
    "                  probs = c(.25, .5, .75), na.rm = TRUE, type = 7)",
    "})",
    "names(reference_ordinal_quantiles) <- ordinal_variables"
  ) else character()
  code <- c(header, summary_lines, ordinal_lines,
            .rls_table1_reference_test_code(record),
            "if (interactive()) print(reference_table) else reference_table",
            if (isTRUE(record$display_options$show_n)) "reference_group_n" else character(),
            "reference_tests")
  warnings <- character()
  if (length(continuous) && "median_iqr" %in% (record$display_options$numeric_stats %||% character()))
    warnings <- c(warnings, "gtsummary displays the same recalculated quantities in a different layout from LinkEDA.")
  list(code = paste(code, collapse = "\n"), variables = include,
       warnings = unique(warnings))
}

.rls_table1_mi_verification_r_code <- function(record) {
  variables <- as.character(record$variables %||% character())
  group <- record$group_variable %||% ""
  include <- unique(c(variables, if (nzchar(group)) group else character(), ".imp", ".id"))
  numeric_variables <- names(record$variable_types)[
    unlist(record$variable_types, use.names = FALSE) == "numeric"
  ]
  ordinal_variables <- names(record$variable_types)[
    unlist(record$variable_types, use.names = FALSE) == "ordinal"
  ]
  categorical_variables <- names(record$variable_types)[
    unlist(record$variable_types, use.names = FALSE) %in% c("categorical", "ordinal")
  ]
  numeric_stats <- record$display_options$numeric_stats %||% "mean_sd"
  continuous_statistics <- c(
    if ("mean_sd" %in% numeric_stats) c("{mean}", "{sd}") else character(),
    if ("median_iqr" %in% numeric_stats) c("{median}", "{p25}", "{p75}") else character()
  )
  if (!length(continuous_statistics)) continuous_statistics <- c("{mean}", "{sd}")
  type_lines <- c(
    if (length(numeric_variables))
      "      tidyselect::all_of(numeric_variables) ~ \"continuous2\"" else character(),
    if (length(categorical_variables))
      "      tidyselect::all_of(categorical_variables) ~ \"categorical\"" else character()
  )
  group_levels <- record$group_levels %||% if(nzchar(group))
    levels(droplevels(.rls_table1_display_factor(record$data[[group]], "categorical"))) else character()
  group_columns <- record$group_column_names %||% .rls_table1_group_column_names(group_levels)
  group_labels <- record$group_value_labels %||% if(nzchar(group)) .rls_table1_value_labels(record$data[[group]]) else NULL
  test_methods <- stats::setNames(vapply(numeric_variables, function(v)
    record$test_results[[v]]$pooled_result$method %||% "mice::D1", character(1L)), numeric_variables)
  code <- c(
    "# This check uses the prepared imputations exported by LinkEDA; it does not rerun imputation.",
    "if (!requireNamespace(\"mice\", quietly = TRUE))",
    "  stop(\"Install package 'mice' to run this check.\")",
    "if (!requireNamespace(\"gtsummary\", quietly = TRUE))",
    "  stop(\"Install package 'gtsummary' to run this check.\")",
    "mi_long <- base::readRDS(verification_data_path)",
    "stopifnot(all(c(\".imp\", \".id\") %in% names(mi_long)))",
    "original_data <- mi_long[mi_long$.imp == 0L, , drop = FALSE]",
    "completed_sets <- split(mi_long[mi_long$.imp > 0L, , drop = FALSE],",
    "                        mi_long$.imp[mi_long$.imp > 0L])",
    "completed_sets <- lapply(completed_sets, function(data) {",
    "  data$.imp <- NULL; data$.id <- NULL; rownames(data) <- NULL; data",
    "})",
    paste0("table_variables <- ", .rls_r_character_vector(variables)),
    paste0("numeric_variables <- ", .rls_r_character_vector(numeric_variables)),
    paste0("categorical_variables <- ", .rls_r_character_vector(categorical_variables)),
    paste0("ordinal_variables <- ", .rls_r_character_vector(ordinal_variables)),
    if (nzchar(group)) c(
      paste0("group_variable <- ", .rls_r_string_literal(group)),
      paste0("group_levels <- ", .rls_r_character_vector(group_levels)),
      paste0("summary_groups <- ", .rls_r_character_vector(group_columns)),
      paste0("group_value_labels <- ", paste(deparse(group_labels), collapse="\n")),
      "group_labels_by_column <- stats::setNames(c(NA_character_, group_levels), summary_groups)",
      "as_group <- function(x) {",
      "  if (is.factor(x)) return(factor(as.character(x), levels=group_levels))",
      "  raw <- unclass(x); attributes(raw) <- NULL; values <- as.character(raw)",
      "  if (!is.null(group_value_labels)) {",
      "    matched <- match(raw, unname(group_value_labels)); found <- !is.na(matched)",
      "    values[found] <- names(group_value_labels)[matched[found]]",
      "  }",
      "  factor(values, levels=group_levels)",
      "}",
      "completed_sets <- lapply(completed_sets, function(data) {data[[group_variable]] <- as_group(data[[group_variable]]); data})",
      "rows_for_group <- function(data, group_level) {",
      "  if (identical(group_level, summary_groups[[1L]])) return(rep(TRUE, nrow(data)))",
      "  !is.na(data[[group_variable]]) & as.character(data[[group_variable]]) == group_labels_by_column[[group_level]]",
      "}"
    ) else c(
      "summary_groups <- \"Overall\"",
      "rows_for_group <- function(data, group_level) rep(TRUE, nrow(data))"
    ),
    "reference_tables_by_imputation <- lapply(completed_sets, function(data) {",
    "  gtsummary::tbl_summary(",
    "    data = data,",
    if (nzchar(group)) paste0("    by = ", .rls_r_string_literal(group), ",") else character(),
    "    include = tidyselect::all_of(table_variables),",
    "    type = list(",
    paste(type_lines, collapse = ",\n"),
    "    ),",
    "    statistic = list(",
    paste0("      gtsummary::all_continuous() ~ ",
           .rls_r_character_vector(continuous_statistics), ","),
    "      gtsummary::all_categorical() ~ \"{n} ({p}%)\"",
    paste0("    ), missing = ",
           if (isTRUE(record$display_options$include_missing)) "\"ifany\"" else "\"no\"",
           ", percent = \"column\""),
    "  )",
    "})",
    "pool_numeric_mean <- function(variable, group_level = \"Overall\") {",
    "  estimates <- variances <- sample_sizes <- numeric(length(completed_sets))",
    "  for (i in seq_along(completed_sets)) {",
    "    data <- completed_sets[[i]]",
    "    values <- data[[variable]][rows_for_group(data, group_level)]",
    "    values <- values[!is.na(values)]",
    "    sample_sizes[[i]] <- length(values)",
    "    estimates[[i]] <- mean(values)",
    "    variances[[i]] <- stats::var(values) / length(values)",
    "  }",
    "  pooled <- mice::pool.scalar(estimates, variances, n = min(sample_sizes), k = 1L)",
    "  pooled$mcse <- sqrt(pooled$b/pooled$m)",
    "  pooled$mcse_percent_se <- if (pooled$t>0) 100*pooled$mcse/sqrt(pooled$t) else NA_real_",
    "  pooled",
    "}",
    "reference_pooled_means <- stats::setNames(lapply(numeric_variables, function(variable)",
    "  stats::setNames(lapply(summary_groups, function(group_level)",
    "    pool_numeric_mean(variable, group_level)), summary_groups)), numeric_variables)",
    "reference_descriptive_components <- lapply(numeric_variables, function(variable) {",
    "  stats::setNames(lapply(summary_groups, function(group_level) {",
    "    values <- lapply(completed_sets, function(data) {",
    "      x <- data[[variable]][rows_for_group(data, group_level)]; x[!is.na(x)]",
    "    })",
    "    list(sd = mean(vapply(values, stats::sd, numeric(1L)), na.rm = TRUE),",
    "         median = mean(vapply(values, stats::median, numeric(1L)), na.rm = TRUE),",
    "         quartiles = Reduce(`+`, lapply(values, stats::quantile,",
    "           probs = c(.25, .75), na.rm = TRUE, type = 7)) / length(values))",
    "  }), summary_groups)",
    "})",
    "names(reference_descriptive_components) <- numeric_variables",
    "reference_categorical_summaries <- stats::setNames(lapply(categorical_variables, function(variable) {",
    "  declared <- if (is.factor(completed_sets[[1L]][[variable]]))",
    "    levels(completed_sets[[1L]][[variable]]) else character()",
    "  observed <- unique(unlist(lapply(completed_sets, function(data)",
    "    as.character(data[[variable]][!is.na(data[[variable]])]))))",
    "  variable_levels <- unique(c(declared, observed))",
    "  stats::setNames(lapply(variable_levels, function(level) {",
    "    stats::setNames(lapply(summary_groups, function(group_level) {",
    "      pieces <- lapply(completed_sets, function(data) {",
    "        x <- data[[variable]][rows_for_group(data, group_level)]",
    "        denominator <- sum(!is.na(x)); count <- sum(as.character(x) == level, na.rm = TRUE)",
    "        c(n = count, percent = if (denominator) count / denominator else NA_real_)",
    "      })",
    "      matrix <- do.call(rbind, pieces)",
    "      c(n = mean(matrix[, \"n\"], na.rm = TRUE),",
    "        percent = mean(matrix[, \"percent\"], na.rm = TRUE))",
    "    }), summary_groups)",
    "  }), variable_levels)",
    "}), categorical_variables)",
    "ordinal_scores <- function(x) if (is.factor(x)) as.numeric(x) else as.numeric(x)",
    "reference_ordinal_quantiles <- stats::setNames(lapply(ordinal_variables, function(variable) {",
    "  stats::setNames(lapply(summary_groups, function(group_level) {",
    "    values <- lapply(completed_sets, function(data) {",
    "      x <- ordinal_scores(data[[variable]])[rows_for_group(data, group_level)]",
    "      x <- x[!is.na(x)]; stats::quantile(x, c(.25, .5, .75), type = 7)",
    "    })",
    "    Reduce(`+`, values) / length(values)",
    "  }), summary_groups)",
    "}), ordinal_variables)",
    "reference_missing_in_original <- stats::setNames(lapply(table_variables, function(variable) {",
    "  x <- original_data[[variable]]; c(n = sum(is.na(x)), percent = mean(is.na(x)))",
    "}), table_variables)"
  )
  if (nzchar(group) && length(numeric_variables)) {
    code <- c(code,
      paste0("test_methods <- ", paste(deparse(test_methods), collapse="\n")),
      "reference_numeric_tests <- lapply(numeric_variables, function(variable) {",
      "  full_fits <- lapply(completed_sets, function(data)",
      "    stats::lm(stats::reformulate(group_variable, response = variable), data = data))",
      "  reduced_fits <- lapply(full_fits, function(fit)",
      "    stats::lm(stats::reformulate(\"1\", response = variable), data = stats::model.frame(fit)))",
      "  full <- mice::as.mira(full_fits); reduced <- mice::as.mira(reduced_fits)",
      "  if (length(group_levels) == 2L) return(summary(mice::pool(full)))",
      "  method <- test_methods[[variable]]",
      "  test <- if (grepl('mice::D3', method, fixed=TRUE)) mice::D3(full, reduced) else mice::D1(full, reduced)",
      "  result <- if (is.matrix(test$result) || is.data.frame(test$result)) test$result[1L, ] else test$result",
      "  keys <- c('F.value', 'df1', 'df2', 'P(>F)', 'RIV')",
      "  if (is.null(names(result))) names(result) <- keys",
      "  if (grepl('mice::D1', method, fixed=TRUE) &&",
      "      (!is.finite(result[['df2']]) || !is.finite(result[['P(>F)']])) &&",
      "      is.finite(result[['RIV']]) && abs(result[['RIV']]) <= sqrt(.Machine$double.eps) &&",
      "      is.finite(test$dfcom) && test$dfcom > 0) {",
      "    result[['df2']] <- test$dfcom",
      "    result[['P(>F)']] <- stats::pf(result[['F.value']], result[['df1']], test$dfcom, lower.tail=FALSE)",
      "  }",
      "  test$result <- matrix(result, nrow=1L, dimnames=list(NULL, names(result)))",
      "  test",
      "})",
      "names(reference_numeric_tests) <- numeric_variables",
      "# Scalar missing-information diagnostics for the two-group difference only.",
      "reference_group_diagnostics <- stats::setNames(lapply(numeric_variables, function(variable) {",
      "  fits <- lapply(completed_sets, function(data)",
      "    stats::lm(stats::reformulate(group_variable, response = variable), data = data))",
      "  if (length(stats::coef(fits[[1L]])) != 2L) return(NULL)",
      "  pooled <- mice::pool(mice::as.mira(fits))$pooled",
      "  pooled <- pooled[pooled$term != '(Intercept)', , drop = FALSE]",
      "  pooled$mcse <- with(pooled, sqrt(b/m))",
      "  pooled$mcse_percent_se <- with(pooled, ifelse(t>0, 100*mcse/sqrt(t), NA_real_))",
      "  pooled",
      "}), numeric_variables)")
  }
  code <- c(code,
    "if (interactive()) print(reference_tables_by_imputation[[1L]]) else reference_tables_by_imputation[[1L]]",
    "reference_pooled_means",
    "reference_descriptive_components",
    "reference_categorical_summaries",
    "reference_ordinal_quantiles",
    "reference_missing_in_original",
    if (nzchar(group) && length(numeric_variables))
      c("reference_numeric_tests", "reference_group_diagnostics") else character())
  list(
    code = paste(code, collapse = "\n"),
    variables = include,
    warnings = c(
      "Categorical and ordinal inference is not pooled because LinkEDA does not report it for this Table 1.",
      "SDs and quantiles are descriptive averages across imputations, not Rubin-pooled inferential quantities."
    )
  )
}

.rls_dimensionality_verification_r_code <- function(record,
                                                     multiple_imputation = FALSE) {
  variables <- unique(as.character(record$variables %||% character()))
  method <- match.arg(as.character(record$method %||% "pca"), c("pca", "factor"))
  rotation <- match.arg(as.character(record$rotation %||% "none"),
                        c("none", "varimax", "quartimax", "oblimin", "promax"))
  extraction <- match.arg(as.character(record$extraction %||% "minres"),
                          c("minres", "ml", "pa"))
  components <- max(1L, as.integer(record$n_components %||% 1L))
  standardize <- isTRUE(record$scale)
  parallel_enabled <- isTRUE(record$parallel %||% TRUE)
  parallel_iterations <- max(1L, as.integer(record$parallel_iterations %||% 100L))
  displayed_imputation <- max(1L, as.integer(record$active_imputation_version %||% 1L))
  imputation_count <- max(1L, as.integer(record$imputation_count %||% 1L))
  setup <- c(
    paste0("analysis_variables <- ", .rls_r_character_vector(variables)),
    paste0("analysis_method <- ", .rls_r_string_literal(method)),
    paste0("retained_dimensions <- ", components, "L"),
    paste0("standardize_variables <- ", if (standardize) "TRUE" else "FALSE"),
    paste0("rotation_method <- ", .rls_r_string_literal(rotation)),
    paste0("extraction_method <- ", .rls_r_string_literal(extraction)),
    paste0("parallel_enabled <- ", if (parallel_enabled) "TRUE" else "FALSE"),
    paste0("parallel_iterations <- ", parallel_iterations, "L"),
    "parallel_seed <- 271828L",
    "prepare_dimension_data <- function(data) {",
    "  x <- data[, analysis_variables, drop = FALSE]",
    "  x <- x[stats::complete.cases(x), , drop = FALSE]",
    "  if (nrow(x) < 2L) stop(\"At least two complete rows are required.\")",
    "  x",
    "}",
    "rotate_dimension_loadings <- function(loadings) {",
    "  if (rotation_method == \"none\" || ncol(loadings) < 2L) return(loadings)",
    "  if (rotation_method == \"varimax\")",
    "    return(as.matrix(stats::varimax(loadings, normalize = FALSE)$loadings))",
    "  if (!requireNamespace(\"GPArotation\", quietly = TRUE))",
    "    stop(\"Install package 'GPArotation' to check quartimax rotation.\")",
    "  as.matrix(GPArotation::quartimax(loadings)$loadings)",
    "}",
    "fit_dimension <- function(data) {",
    "  x <- prepare_dimension_data(data)",
    "  dimension_count <- min(retained_dimensions, ncol(x))",
    "  if (analysis_method == \"pca\") {",
    "    fit <- stats::prcomp(x, center = TRUE, scale. = standardize_variables)",
    "    eigenvalues <- fit$sdev^2",
    "    variance <- eigenvalues / sum(eigenvalues)",
    "    loadings <- sweep(fit$rotation[, seq_len(dimension_count), drop = FALSE],",
    "                      2L, fit$sdev[seq_len(dimension_count)], `*`)",
    "    prefix <- \"PC\"",
    "  } else {",
    "    matrix_fit <- if (standardize_variables) stats::cor(x) else stats::cov(x)",
    "    fit <- psych::fa(matrix_fit, nfactors = dimension_count, n.obs = nrow(x),",
    "                     fm = extraction_method, rotate = rotation_method,",
    "                     covar = !standardize_variables)",
    "    eigenvalues <- fit$values",
    "    loadings <- unclass(fit$loadings)[, seq_len(dimension_count), drop = FALSE]",
    "    variance <- colSums(loadings^2) / ncol(x)",
    "    prefix <- \"F\"",
    "  }",
    "  if (analysis_method == \"pca\") loadings <- rotate_dimension_loadings(loadings)",
    "  colnames(loadings) <- paste0(prefix, seq_len(ncol(loadings)))",
    "  old_seed <- if (exists(\".Random.seed\", envir = .GlobalEnv, inherits = FALSE)) .Random.seed else NULL",
    "  on.exit(if (is.null(old_seed)) {",
    "    if (exists(\".Random.seed\", envir = .GlobalEnv, inherits = FALSE)) rm(.Random.seed, envir = .GlobalEnv)",
    "  } else assign(\".Random.seed\", old_seed, envir = .GlobalEnv), add = TRUE)",
    "  parallel_reference <- rep(NA_real_, length(eigenvalues))",
    "  if (parallel_enabled) {",
    "    set.seed(parallel_seed)",
    "    parallel_values <- replicate(parallel_iterations, {",
    "      simulated <- scale(matrix(stats::rnorm(nrow(x) * ncol(x)), nrow = nrow(x)))",
    "      eigen(stats::cor(simulated), symmetric = TRUE, only.values = TRUE)$values",
    "    })",
    "    parallel_reference <- apply(parallel_values, 1L, stats::quantile,",
    "                                probs = .95, names = FALSE)",
    "  }",
    "  variance_full <- rep(NA_real_, length(eigenvalues))",
    "  variance_full[seq_along(variance)] <- variance",
    "  component_table <- data.frame(",
    "    Dimension = paste0(prefix, seq_along(eigenvalues)),",
    "    Eigenvalue = eigenvalues, Parallel = parallel_reference,",
    "    Variance = variance_full, Cumulative = cumsum(variance_full),",
    "    check.names = FALSE)",
    "  h2 <- rowSums(loadings^2)",
    "  loading_table <- data.frame(Variable = rownames(loadings), loadings,",
    "                              h2 = h2, u2 = pmax(0, 1 - h2),",
    "                              check.names = FALSE)",
    "  list(components = component_table, loadings = loading_table)",
    "}"
  )
  if (method == "pca" && !isTRUE(record$typed_engine)) {
    setup <- c(setup[seq_len(9L)],
      paste0("missing_method <- ", .rls_r_string_literal(record$missing_mode %||% "listwise")),
      "prepare_dimension_data <- function(data) {\n  x <- data[, analysis_variables, drop = FALSE]\n  keep <- if (missing_method == \"listwise\") stats::complete.cases(x) else rowSums(!is.na(x)) > 0L\n  x[keep, , drop = FALSE]\n}\nfit_dimension <- function(data) {\n  x <- prepare_dimension_data(data)\n  if (nrow(x) < 2L) stop(\"At least two usable rows are required.\")\n  use <- if (missing_method == \"pairwise\") \"pairwise.complete.obs\" else \"everything\"\n  covariance <- stats::cov(x, use = use)\n  if (missing_method == \"pairwise\") {\n    matrix <- if (standardize_variables) stats::cor(x, use = use) else covariance\n    fit <- stats::princomp(covmat = list(cov = matrix, n.obs = nrow(x)))\n    vectors <- unclass(fit$loadings)\n  } else {\n    fit <- stats::prcomp(x, center = TRUE, scale. = standardize_variables)\n    vectors <- fit$rotation\n  }\n  eigenvalues <- fit$sdev^2\n  rank <- sum(fit$sdev > max(fit$sdev) * sqrt(.Machine$double.eps))\n  dimension_count <- min(retained_dimensions, rank)\n  if (dimension_count < 1L) stop(\"No nonzero principal components are available.\")\n  loadings <- sweep(vectors[, seq_len(dimension_count), drop = FALSE],\n                    2L, fit$sdev[seq_len(dimension_count)], `*`)\n  if (dimension_count > 1L && rotation_method == \"varimax\")\n    loadings <- as.matrix(stats::varimax(loadings, normalize = FALSE)$loadings)\n  if (dimension_count > 1L && rotation_method == \"quartimax\")\n    loadings <- as.matrix(GPArotation::quartimax(loadings)$loadings)\n  loadings <- unclass(loadings)\n  colnames(loadings) <- paste0(\"PC\", seq_len(ncol(loadings)))\n  parallel_reference <- rep(NA_real_, length(eigenvalues))\n  if (parallel_enabled) {\n    old_seed <- if (exists(\".Random.seed\", envir = .GlobalEnv, inherits = FALSE)) .Random.seed else NULL\n    old_options <- options(mc.cores = 1L)\n    on.exit({\n      options(old_options)\n      if (is.null(old_seed)) {\n        if (exists(\".Random.seed\", envir = .GlobalEnv, inherits = FALSE)) rm(.Random.seed, envir = .GlobalEnv)\n      } else assign(\".Random.seed\", old_seed, envir = .GlobalEnv)\n    }, add = TRUE)\n    set.seed(parallel_seed)\n    capture.output(reference <- psych::fa.parallel(x, fa = \"pc\", n.iter = parallel_iterations,\n      cor = if (standardize_variables) \"cor\" else \"cov\", sim = FALSE, SMC = TRUE,\n      use = if (missing_method == \"pairwise\") \"pairwise\" else \"complete\", quant = .95, plot = FALSE))\n    parallel_reference <- apply(reference$values[, seq_len(ncol(x)), drop = FALSE],\n                                2L, stats::quantile, probs = .95, names = FALSE)[seq_along(eigenvalues)]\n  }\n  variance <- eigenvalues / sum(eigenvalues)\n  component_table <- data.frame(Dimension = paste0(\"PC\", seq_along(eigenvalues)),\n    Eigenvalue = eigenvalues, Parallel = parallel_reference, Variance = variance, Cumulative = cumsum(variance))\n  h2 <- rowSums(loadings^2)\n  baseline <- if (standardize_variables) rep(1, ncol(x)) else diag(covariance)\n  loading_table <- data.frame(Variable = rownames(loadings), loadings, h2 = h2,\n    u2 = pmax(0, baseline - h2), check.names = FALSE)\n  list(components = component_table, loadings = loading_table)\n}")
  }
  if (isTRUE(record$typed_engine)) {
    specs <- record$item_specifications %||% .rls_scale_item_specifications(record$data, variables)
    types <- setNames(vapply(specs, `[[`, character(1L), "type"), variables)
    levels <- lapply(specs, function(spec) spec$ordinal_levels %||%
      .rls_scale_item_levels(record$data[[spec$name]]))
    names(levels) <- variables
    typed_recipe <- "parallel_reference <- function(r, n, method, extraction, iterations,\n                               data = NULL, covariance = FALSE, missing = \"pairwise\") {\n  old_seed <- if (exists(\".Random.seed\", envir = .GlobalEnv, inherits = FALSE))\n    get(\".Random.seed\", envir = .GlobalEnv) else NULL\n  old_options <- options(mc.cores = 1L)\n  on.exit({\n    options(old_options)\n    if (is.null(old_seed)) {\n      if (exists(\".Random.seed\", envir = .GlobalEnv, inherits = FALSE))\n        rm(\".Random.seed\", envir = .GlobalEnv)\n    } else assign(\".Random.seed\", old_seed, envir = .GlobalEnv)\n  }, add = TRUE)\n  set.seed(271828L)\n  raw_covariance <- covariance && method == \"pca\"\n  if (raw_covariance) {\n    if (is.null(data)) stop(\"Covariance PCA parallel analysis requires the source observations.\")\n    data <- as.data.frame(data)\n    data <- data[if (missing == \"listwise\") stats::complete.cases(data) else\n      rowSums(!is.na(data)) > 0L, , drop = FALSE]\n    utils::capture.output(result <- psych::fa.parallel(data, fa = \"pc\", cor = \"cov\",\n      sim = FALSE, SMC = TRUE, use = if (missing == \"listwise\") \"complete\" else \"pairwise\",\n      n.iter = iterations, quant = .95, plot = FALSE))\n  } else {\n    # Factor parallel analysis uses reduced correlations, including for covariance fits.\n    utils::capture.output(result <- psych::fa.parallel(stats::cov2cor(r), n.obs = n,\n      fm = extraction, fa = if (method == \"pca\") \"pc\" else \"fa\",\n      SMC = method == \"pca\", n.iter = iterations, quant = .95, plot = FALSE))\n  }\n  columns <- seq_len(ncol(r)) + if (method == \"pca\") 0L else ncol(r)\n  result$reference <- as.numeric(apply(result$values[, columns, drop = FALSE],\n    2L, stats::quantile, probs = .95, names = FALSE))\n  result$reference_method <- if (raw_covariance) \"column resampling (covariance units)\" else\n    if (method == \"factor\") \"normal simulations (reduced correlations)\" else \"normal simulations (correlations)\"\n  result\n}\n\n# Use the same explicit item encoding and correlation basis as Scale Analysis.\nitem_types <- TYPES\nitem_levels <- LEVELS\nprepare_dimension_data <- function(data) {\n  x <- data[, analysis_variables, drop = FALSE]\n  for (name in names(x)) {\n    if (item_types[[name]] == \"ordinal\") {\n      x[[name]] <- as.integer(factor(as.character(x[[name]]), levels = item_levels[[name]], ordered = TRUE))\n    }\n  }\n  x\n}\nfit_dimension <- function(data) {\n  x <- prepare_dimension_data(data)\n  matrix_data <- if (MISSING == \"listwise\") x[stats::complete.cases(x), , drop = FALSE] else x\n  use <- if (MISSING == \"listwise\") \"complete\" else \"pairwise\"\n  ordinal <- which(item_types == \"ordinal\")\n  continuous <- which(item_types == \"numeric\")\n  r <- if (!length(ordinal)) psych::corr.test(matrix_data, use = use, ci = FALSE)$r else if (!length(continuous)) {\n    psych::polychoric(matrix_data, smooth = FALSE, progress = FALSE)$rho\n  } else psych::mixedCor(matrix_data, c = continuous, p = ordinal, use = use, smooth = FALSE)$rho\n  covariance <- !standardize_variables && !length(ordinal)\n  if (covariance) r <- stats::cov(matrix_data, use = if (use == \"complete\") \"complete.obs\" else \"pairwise.complete.obs\")\n  sizes <- crossprod(!is.na(matrix_data))\n  n <- min(sizes[upper.tri(sizes)])\n  if (any(!is.finite(r)) || any(diag(r) <= 0) || !isTRUE(isSymmetric(unname(r))))\n    stop(\"Invalid correlation/covariance matrix.\")\n  check <- stats::cov2cor(r)\n  ev <- eigen(check, symmetric = TRUE, only.values = TRUE)$values\n  if (min(ev) < -max(abs(ev)) * sqrt(.Machine$double.eps))\n    stop(\"Correlation matrix is not positive semidefinite; no matrix correction was applied.\")\n  if (analysis_method == \"factor\" && (min(ev) <= 0 || rcond(check) <= sqrt(.Machine$double.eps)))\n    stop(\"Correlation matrix is not positive definite or is numerically singular.\")\n  fit <- if (analysis_method == \"factor\") psych::fa(r, nfactors = retained_dimensions, n.obs = n,\n    fm = \"ml\", rotate = rotation_method, covar = covariance) else psych::principal(r,\n    nfactors = retained_dimensions, n.obs = n, rotate = rotation_method, covar = covariance, scores = FALSE)\n  if (parallel_enabled) {\n    reference <- parallel_reference(r, n, analysis_method, \"ml\", parallel_iterations,\n      data = matrix_data, covariance = covariance, missing = MISSING)\n    values <- if (analysis_method == \"factor\") reference$fa.values else reference$pc.values\n    parallel_values <- reference$reference\n  } else {\n    values <- if (analysis_method == \"factor\")\n      psych::fa(stats::cov2cor(r), nfactors = 1L, rotate = \"none\", fm = \"ml\", warnings = FALSE)$values\n      else eigen(r, symmetric = TRUE, only.values = TRUE)$values\n    parallel_values <- rep(NA_real_, length(values))\n  }\n  loadings <- as.data.frame(unclass(fit$loadings))\n  loading_table <- data.frame(Variable = rownames(loadings), loadings, h2 = fit$communality, u2 = fit$uniquenesses)\n  component_table <- data.frame(Dimension = seq_along(values), Eigenvalue = values, Parallel = parallel_values)\n  list(components = component_table, loadings = loading_table)\n}"
    typed_recipe <- gsub('fm = "ml"', "fm = extraction_method", typed_recipe, fixed = TRUE)
    typed_recipe <- gsub('analysis_method, "ml",',
                         "analysis_method, extraction_method,", typed_recipe, fixed = TRUE)
    typed_recipe <- gsub("TYPES", paste(capture.output(dput(types)), collapse = "\n"), typed_recipe, fixed = TRUE)
    typed_recipe <- gsub("LEVELS", paste(capture.output(dput(levels)), collapse = "\n"), typed_recipe, fixed = TRUE)
    typed_recipe <- gsub("MISSING", .rls_r_string_literal(record$missing_mode), typed_recipe, fixed = TRUE)
    setup <- c(setup[seq_len(9L)], typed_recipe)
  }
  data_code <- if (isTRUE(multiple_imputation)) c(
    "mi_long <- base::readRDS(verification_data_path)",
    "stopifnot(all(c(\".imp\", \".id\") %in% names(mi_long)))",
    "completed_sets <- split(mi_long[mi_long$.imp > 0L, , drop = FALSE],",
    "                        mi_long$.imp[mi_long$.imp > 0L])",
    "completed_sets <- lapply(completed_sets, function(data) {",
    "  data$.imp <- NULL; data$.id <- NULL; rownames(data) <- NULL; data",
    "})",
    paste0("displayed_imputation <- ", displayed_imputation, "L"),
    "analysis_data <- completed_sets[[as.character(displayed_imputation)]]",
    "if (is.null(analysis_data)) stop(\"The displayed imputation is not present in the exported data.\")"
  ) else "analysis_data <- base::readRDS(verification_data_path)"
  presentation <- c(
    "reference_result <- fit_dimension(analysis_data)",
    "if (!requireNamespace(\"knitr\", quietly = TRUE))",
    "  stop(\"Install package 'knitr' to present these tables.\")",
    "knitr::kable(reference_result$components, digits = 3,",
    "             caption = \"Components and parallel-analysis reference\")",
    "knitr::kable(reference_result$loadings, digits = 3,",
    "             caption = \"Rotated loadings, communalities, and uniquenesses\")"
  )
  warnings <- c(
    "The verification recipe uses public R functions with the recorded missing-data and parallel-analysis settings.",
    if (identical(record$missing_mode %||% "listwise", "pairwise"))
      "Pairwise PCA fits the available-pair matrix; individual scores require complete rows."
    else character(),
    if (rotation != "none")
      "Rotated component or factor signs can be reversed without changing the statistical solution."
    else character(),
    if (isTRUE(multiple_imputation)) sprintf(
      "The table shows completed imputation %d of %d. Eigenvalues and loading matrices are not Rubin-pooled.",
      displayed_imputation, imputation_count
    ) else character()
  )
  if (isTRUE(record$typed_engine)) warnings <- c(
    "Correlations and extraction use the same public psych functions as Scale Analysis. The reference is the 95th percentile of library simulations with seed 271828; covariance PCA uses column resampling.",
    if (isTRUE(multiple_imputation)) sprintf("Showing imputation %d of %d; not pooled.", displayed_imputation, imputation_count))
  list(
    code = paste(c(
      "# Recalculate the dimensionality analysis from LinkEDA's prepared data.",
      "# Eigenvalues and loading matrices do not have a direct Rubin-rules pooling step.",
      setup, data_code, presentation
    ), collapse = "\n"),
    variables = variables,
    warnings = warnings
  )
}

.rls_compare_means_verification_r_code <- function(results) {
  stopifnot(length(results) > 0L)
  results <- Filter(function(result) {
    failed <- length(result$warnings %||% character()) > 0L &&
      length(result$test_results$p_value) == 1L &&
      !is.finite(result$test_results$p_value)
    !failed
  }, results)
  stopifnot(length(results) > 0L)
  first <- results[[1L]]
  multiple_imputation <- any(vapply(results, function(result) identical(
    result$analysis_backend %||% "ordinary", "multiple_imputation"
  ), logical(1L)))
  specification_code <- vapply(results, function(result) {
    specification <- result$specification %||% list()
    family <- specification$test_family %||% specification$method %||% "student"
    method_text <- paste(
      as.character(specification$method %||% result$test_results$method %||% family),
      collapse = " "
    )
    if (identical(result$analysis_type, "one_way_anova")) {
      family <- if (grepl("Kruskal", method_text, ignore.case = TRUE)) "kruskal_wallis" else
        if (grepl("Classical", method_text, ignore.case = TRUE)) "classical" else "welch"
    }
    paste0(
      "  list(label = ", .rls_r_string_literal(
        specification$response %||%
          paste(specification$response1 %||% "", specification$response2 %||% "", sep = " - ")
      ),
      ", kind = ", .rls_r_string_literal(result$analysis_type %||% ""),
      ", family = ", .rls_r_string_literal(family),
      ", response1 = ", .rls_r_string_literal(
        specification$response %||% specification$response1 %||% ""
      ),
      ", response2 = ", .rls_r_string_literal(specification$response2 %||% ""),
      ", group = ", .rls_r_string_literal(specification$group %||% ""),
      ", group_levels = ", .rls_r_character_vector(specification$group_levels %||% character()),
      ", pooling_method = ", .rls_r_string_literal(result$multiple_imputation$pooling_method %||% ""),
      ", ordinal_levels = ", .rls_r_character_vector(specification$ordinal_levels %||% character()),
      ", event = ", .rls_r_string_literal(specification$event_level %||% ""),
      ", null = ", format(as.numeric(
        specification$null_mu %||% result$test_results$null_value %||% 0
      ), digits = 17L, scientific = FALSE),
      ", alternative = ", .rls_r_string_literal(specification$alternative %||% "two.sided"),
      ", conf_level = ", format(as.numeric(
        specification$conf_level %||% result$test_results$conf_level %||% .95
      ), digits = 17L, scientific = FALSE),
      ", var_equal = ", if (isTRUE(specification$var_equal)) "TRUE" else "FALSE", ")"
    )
  }, character(1L))
  variables <- unique(unlist(lapply(results, function(result) {
    specification <- result$specification %||% list()
    c(specification$response, specification$response1,
      specification$response2, specification$group)
  }), use.names = FALSE))
  variables <- variables[!is.na(variables) & nzchar(variables)]
  adjustment <- first$test_results$p_adjustment %||% "none"
  setup <- c(
    paste0("test_specifications <- list(\n", paste(specification_code, collapse = ",\n"), "\n)"),
    paste0("p_adjustment <- ", .rls_r_string_literal(adjustment)),
    "prepare_test_data <- function(data, specification) {",
    "  required <- unique(c(specification$response1, specification$response2, specification$group))",
    "  required <- required[nzchar(required)]",
    "  data <- data[stats::complete.cases(data[, required, drop = FALSE]), , drop = FALSE]",
    "  if (length(specification$group_levels)) {",
    "    data <- data[as.character(data[[specification$group]]) %in% specification$group_levels, , drop = FALSE]",
    "    data[[specification$group]] <- factor(as.character(data[[specification$group]]),",
    "                                           levels = specification$group_levels)",
    "  }",
    "  if (length(specification$ordinal_levels)) {",
    "    # Explicit equally spaced category scores, frozen with this result.",
    "    for (variable in c(specification$response1, specification$response2)) {",
    "      if (nzchar(variable)) data[[variable]] <- match(as.character(data[[variable]]), specification$ordinal_levels)",
    "    }",
    "  }",
    "  data",
    "}",
    "run_ordinary_test <- function(data, specification) {",
    "  data <- prepare_test_data(data, specification)",
    "  y <- data[[specification$response1]]",
    "  if (specification$kind == \"one_sample_t_test\") {",
    "    if (specification$family == \"wilcoxon\")",
    "      return(stats::wilcox.test(y, mu = specification$null, exact = FALSE, conf.int = TRUE,",
    "                                alternative = specification$alternative, conf.level = specification$conf_level))",
    "    if (specification$family == \"binomial\") {",
    "      event <- if (nzchar(specification$event)) specification$event else tail(levels(factor(y)), 1L)",
    "      return(stats::binom.test(sum(as.character(y) == event), length(y), p = specification$null,",
    "                               alternative = specification$alternative, conf.level = specification$conf_level))",
    "    }",
    "    return(stats::t.test(y, mu = specification$null, alternative = specification$alternative,",
    "                         conf.level = specification$conf_level))",
    "  }",
    "  if (specification$kind == \"independent_samples_t_test\") {",
    "    g <- data[[specification$group]]",
    "    if (specification$family == \"mann_whitney\")",
    "      return(stats::wilcox.test(y ~ g, exact = FALSE, conf.int = TRUE,",
    "                                alternative = specification$alternative, conf.level = specification$conf_level))",
    "    if (specification$family == \"proportion\") {",
    "      event <- if (nzchar(specification$event)) specification$event else tail(levels(factor(y)), 1L)",
    "      trials <- as.integer(table(g)); successes <- as.integer(table(g[as.character(y) == event]))",
    "      successes <- stats::setNames(rep(0L, length(trials)), names(table(g))) |>",
    "        base::replace(match(names(table(g[as.character(y) == event])), names(table(g))),",
    "                      as.integer(table(g[as.character(y) == event])))",
    "      return(stats::prop.test(successes, trials, correct = FALSE,",
    "                              alternative = specification$alternative, conf.level = specification$conf_level))",
    "    }",
    "    return(stats::t.test(y ~ g, var.equal = specification$var_equal,",
    "                         alternative = specification$alternative, conf.level = specification$conf_level))",
    "  }",
    "  if (specification$kind == \"paired_samples_t_test\") {",
    "    y2 <- data[[specification$response2]]",
    "    if (specification$family == \"wilcoxon\")",
    "      return(stats::wilcox.test(y, y2, paired = TRUE, exact = FALSE, conf.int = TRUE,",
    "                                alternative = specification$alternative, conf.level = specification$conf_level))",
    "    if (specification$family == \"mcnemar\") {",
    "      event <- if (nzchar(specification$event)) specification$event else tail(levels(factor(y)), 1L)",
    "      first_only <- sum(as.character(y) == event & as.character(y2) != event)",
    "      discordant <- first_only + sum(as.character(y) != event & as.character(y2) == event)",
    "      return(stats::binom.test(first_only, discordant, p = .5,",
    "                               alternative = specification$alternative, conf.level = specification$conf_level))",
    "    }",
    "    return(stats::t.test(y, y2, paired = TRUE, alternative = specification$alternative,",
    "                         conf.level = specification$conf_level))",
    "  }",
    "  g <- data[[specification$group]]",
    "  if (specification$family == \"kruskal_wallis\") return(stats::kruskal.test(y, g))",
    "  stats::oneway.test(y ~ g, var.equal = specification$family == \"classical\")",
    "}"
  )
  ordinary <- c(
    "analysis_data <- base::readRDS(verification_data_path)",
    "reference_results <- lapply(test_specifications, function(specification)",
    "  run_ordinary_test(analysis_data, specification))",
    "names(reference_results) <- vapply(test_specifications, `[[`, character(1L), \"label\")",
    "raw_p <- vapply(reference_results, function(result) unname(result$p.value), numeric(1L))",
    "adjusted_p <- if (p_adjustment == \"none\") raw_p else stats::p.adjust(raw_p, method = p_adjustment)",
    "print(data.frame(Result = names(reference_results), p = raw_p, p_adjusted = adjusted_p,",
    "                 row.names = NULL, check.names = FALSE))",
    "invisible(lapply(reference_results, print))",
    "reference_effects <- lapply(test_specifications, function(specification) {",
    "  data <- prepare_test_data(analysis_data, specification)",
    "  y <- data[[specification$response1]]",
    "  if (specification$family == \"kruskal_wallis\")",
    "    return(effectsize::rank_epsilon_squared(y, data[[specification$group]], ci = NULL))",
    "  if (specification$family == \"mcnemar\") {",
    "    test <- run_ordinary_test(data, specification)",
    "    return(data.frame(Matched_odds_ratio = unname(exp(stats::qlogis(test$estimate)))))",
    "  }",
    "  if (specification$family %in% c(\"wilcoxon\", \"mann_whitney\", \"binomial\", \"proportion\")) return(NULL)",
    "  if (specification$kind == \"one_sample_t_test\")",
    "    return(effectsize::cohens_d(y, mu = specification$null, ci = specification$conf_level,",
    "                               alternative = specification$alternative, verbose = FALSE))",
    "  if (specification$kind == \"paired_samples_t_test\")",
    "    return(effectsize::cohens_d(y, data[[specification$response2]], paired = TRUE,",
    "                               ci = specification$conf_level, alternative = specification$alternative, verbose = FALSE))",
    "  if (specification$kind == \"independent_samples_t_test\") {",
    "    g <- data[[specification$group]]",
    "    return(effectsize::hedges_g(y[g == specification$group_levels[1L]], y[g == specification$group_levels[2L]],",
    "                               pooled_sd = TRUE, ci = specification$conf_level,",
    "                               alternative = specification$alternative, verbose = FALSE))",
    "  }",
    "  NULL",
    "})",
    "names(reference_effects) <- names(reference_results)",
    "invisible(lapply(reference_effects, print))"
  )
  imputed <- c(
    "if (!requireNamespace(\"mice\", quietly = TRUE))",
    "  stop(\"Install package 'mice' to pool this check.\")",
    "mi_long <- base::readRDS(verification_data_path)",
    "stopifnot(all(c(\".imp\", \".id\") %in% names(mi_long)))",
    "completed_sets <- split(mi_long[mi_long$.imp > 0L, , drop = FALSE],",
    "                        mi_long$.imp[mi_long$.imp > 0L])",
    "completed_sets <- lapply(completed_sets, function(data) {",
    "  data$.imp <- NULL; data$.id <- NULL; rownames(data) <- NULL; data",
    "})",
    "run_imputed_test <- function(specification) {",
    "  prepared <- lapply(completed_sets, prepare_test_data, specification = specification)",
    "  if (specification$kind == \"one_way_anova\") {",
    "    if (specification$family == \"welch\") {",
    "      if (!requireNamespace(\"miceadds\", quietly = TRUE)) stop(\"Install package 'miceadds' to pool Welch tests.\")",
    "      tests <- lapply(prepared, function(data) stats::oneway.test(",
    "        stats::reformulate(specification$group, response = specification$response1),",
    "        data = data, var.equal = FALSE))",
    "      numerator_df <- vapply(tests, function(test) unname(test$parameter[[1L]]), numeric(1L))",
    "      Fvalues <- vapply(tests, function(test) unname(test$statistic), numeric(1L))",
    "      stopifnot(length(tests) >= 2L, length(unique(numerator_df)) == 1L, all(is.finite(Fvalues)))",
    "      # D2 chi-square approximation; finite Welch denominator df are not used in pooling.",
    "      combined <- miceadds::micombine.F(Fvalues, df1 = numerator_df[[1L]], display = FALSE, version = 1)",
    "      return(list(statistic = unname(combined[[\"D\"]]), p.value = unname(combined[[\"p\"]]),",
    "        parameter = c(df1 = unname(combined[[\"df\"]]), df2 = unname(combined[[\"df2\"]])),",
    "        method = \"Welch ANOVA; miceadds::micombine.F (D2 approximation)\", by_imputation = tests))",
    "    }",
    "    full <- mice::as.mira(lapply(prepared, function(data)",
    "      stats::lm(stats::reformulate(specification$group, response = specification$response1), data = data)))",
    "    reduced <- mice::as.mira(lapply(prepared, function(data)",
    "      stats::lm(stats::reformulate(\"1\", response = specification$response1), data = data)))",
    "    test <- if (grepl(\"mice::D3\", specification$pooling_method, fixed = TRUE)) mice::D3(full, reduced) else mice::D1(full, reduced)",
    "    values <- if (is.matrix(test$result) || is.data.frame(test$result)) test$result[1L, ] else test$result",
    "    keys <- c(\"F.value\", \"df1\", \"df2\", \"P(>F)\", \"RIV\")",
    "    if (length(values) == length(keys) && is.null(names(values))) names(values) <- keys",
    "    stopifnot(all(keys %in% names(values)))",
    "    # The recorded D1 uses its complete-data limit when between-imputation variation is zero.",
    "    if (grepl(\"mice::D1\", specification$pooling_method, fixed = TRUE) &&",
    "        (!is.finite(values[[\"df2\"]]) || !is.finite(values[[\"P(>F)\"]])) &&",
    "        is.finite(values[[\"RIV\"]]) && abs(values[[\"RIV\"]]) <= sqrt(.Machine$double.eps) &&",
    "        is.finite(test$dfcom) && test$dfcom > 0) {",
    "      values[[\"df2\"]] <- test$dfcom",
    "      values[[\"P(>F)\"]] <- stats::pf(values[[\"F.value\"]], values[[\"df1\"]], test$dfcom, lower.tail = FALSE)",
    "      if (is.matrix(test$result) || is.data.frame(test$result)) test$result[1L, names(values)] <- values else test$result <- values",
    "    }",
    "    return(test)",
    "  }",
    "  by_imputation <- lapply(prepared, function(data) {",
    "    if (specification$kind == \"paired_samples_t_test\") {",
    "      values <- data[[specification$response1]] - data[[specification$response2]]",
    "      return(c(estimate = mean(values), variance = stats::var(values) / length(values),",
    "                 complete_df = length(values) - 1L))",
    "    }",
    "    if (specification$kind == \"independent_samples_t_test\") {",
    "      groups <- factor(as.character(data[[specification$group]]),",
    "                       levels = specification$group_levels)",
    "      samples <- lapply(levels(groups), function(level)",
    "        data[[specification$response1]][groups == level])",
    "      n <- vapply(samples, length, integer(1L))",
    "      means <- vapply(samples, mean, numeric(1L))",
    "      variances <- vapply(samples, stats::var, numeric(1L))",
    "      if (specification$var_equal) {",
    "        pooled_variance <- sum((n - 1L) * variances) / (sum(n) - 2L)",
    "        sampling_variance <- pooled_variance * sum(1 / n)",
    "        complete_df <- sum(n) - 2L",
    "      } else {",
    "        components <- variances / n",
    "        sampling_variance <- sum(components)",
    "        complete_df <- sampling_variance^2 / sum(components^2 / (n - 1L))",
    "      }",
    "      return(c(estimate = means[[1L]] - means[[2L]],",
    "                 variance = sampling_variance, complete_df = complete_df))",
    "    }",
    "    values <- data[[specification$response1]] - specification$null",
    "    c(estimate = mean(values), variance = stats::var(values) / length(values),",
    "      complete_df = length(values) - 1L)",
    "  })",
    "  scalar <- do.call(rbind, by_imputation)",
    "  stopifnot(all(is.finite(scalar[, c(\"estimate\", \"variance\")])))",
    "  complete_df <- min(scalar[, \"complete_df\"])",
    "  pooled <- mice::pool.scalar(scalar[, \"estimate\"], scalar[, \"variance\"],",
    "                              n = complete_df + 1, k = 1, rule = \"rubin1987\")",
    "  pooled$mcse <- sqrt(pooled$b/pooled$m)",
    "  pooled$mcse_percent_se <- if (pooled$t>0) 100*pooled$mcse/sqrt(pooled$t) else NA_real_",
    "  print(pooled[c('m','qbar','ubar','b','t','r','df','fmi','mcse','mcse_percent_se')])",
    "  standard_error <- sqrt(pooled$t)",
    "  statistic <- pooled$qbar / standard_error",
    "  if (specification$alternative == \"greater\") {",
    "    p_value <- stats::pt(statistic, pooled$df, lower.tail = FALSE)",
    "    interval <- c(pooled$qbar - stats::qt(specification$conf_level, pooled$df) * standard_error, Inf)",
    "  } else if (specification$alternative == \"less\") {",
    "    p_value <- stats::pt(statistic, pooled$df, lower.tail = TRUE)",
    "    interval <- c(-Inf, pooled$qbar + stats::qt(specification$conf_level, pooled$df) * standard_error)",
    "  } else {",
    "    p_value <- 2 * stats::pt(abs(statistic), pooled$df, lower.tail = FALSE)",
    "    interval <- pooled$qbar + c(-1, 1) * stats::qt((1 + specification$conf_level) / 2, pooled$df) * standard_error",
    "  }",
    "  data.frame(estimate = pooled$qbar, std.error = standard_error, statistic = statistic,",
    "             df = pooled$df, p.value = p_value, conf.low = interval[[1L]],",
    "             conf.high = interval[[2L]])",
    "}",
    "reference_results <- lapply(test_specifications, run_imputed_test)",
    "names(reference_results) <- vapply(test_specifications, `[[`, character(1L), \"label\")",
    "raw_p <- vapply(reference_results, function(result) {",
    "  if (!is.null(result$p.value)) return(unname(result$p.value))",
    "  values <- if (is.matrix(result$result) || is.data.frame(result$result)) result$result[1L, ] else result$result",
    "  if (is.null(names(values))) return(unname(values[[4L]]))",
    "  unname(values[[\"P(>F)\"]])",
    "}, numeric(1L))",
    "adjusted_p <- rep(NA_real_, length(raw_p)); names(adjusted_p) <- names(reference_results)",
    "valid <- is.finite(raw_p)",
    "adjusted_p[valid] <- if (p_adjustment == \"none\") raw_p[valid] else stats::p.adjust(raw_p[valid], method = p_adjustment)",
    "print(data.frame(Result = names(reference_results), p = raw_p, p_adjusted = adjusted_p, row.names = NULL))",
    "invisible(lapply(reference_results, print))"
  )
  imputed <- c(imputed,
    "run_imputed_group_diagnostics <- function(specification) {",
    "  if (!specification$kind %in% c(\"independent_samples_t_test\", \"paired_samples_t_test\")) return(NULL)",
    "  paired <- specification$kind == \"paired_samples_t_test\"",
    "  labels <- if (paired) c(specification$response1, specification$response2) else specification$group_levels",
    "  prepared <- lapply(completed_sets, prepare_test_data, specification = specification)",
    "  samples <- lapply(prepared, function(data) lapply(labels, function(group)",
    "    if (paired) data[[group]] else data[[specification$response1]][as.character(data[[specification$group]]) == group]))",
    "  valid <- vapply(samples, function(groups) all(vapply(groups, function(x)",
    "    length(x) >= 2L && all(is.finite(x)) && is.finite(stats::var(x)), logical(1L))), logical(1L))",
    "  samples <- samples[valid]",
    "  if (length(samples) < 2L) return(NULL)",
    "  do.call(rbind,lapply(seq_along(labels), function(j) {",
    "    groups <- lapply(samples, `[[`, j)",
    "    means <- vapply(groups, mean, numeric(1L))",
    "    variances <- vapply(groups, function(x) stats::var(x)/length(x), numeric(1L))",
    "    pool <- mice::pool.scalar(means,variances,n=min(lengths(groups)),k=1,rule=\"rubin1987\")",
    "    se <- sqrt(pool$t)",
    "    ci <- if (!is.na(pool$df) && pool$df > 0)",
    "      pool$qbar+c(-1,1)*stats::qt((1+specification$conf_level)/2,pool$df)*se else c(NA_real_,NA_real_)",
    "    data.frame(Group=labels[j],Estimate=pool$qbar,SE=se,df=pool$df,",
    "      CI_low=ci[1],CI_high=ci[2],FMI=pool$fmi,RIV=pool$r,Ubar=pool$ubar,B=pool$b,T=pool$t,m=pool$m,",
    "      lambda=(1+1/pool$m)*pool$b/pool$t,relative_efficiency=1/(1+pool$fmi/pool$m),",
    "      MCSE=sqrt(pool$b/pool$m),MCSE_percent_SE=100*sqrt(pool$b/pool$m)/se)",
    "  }))",
    "}",
    "reference_group_diagnostics <- lapply(test_specifications,run_imputed_group_diagnostics)",
    "names(reference_group_diagnostics) <- vapply(test_specifications, `[[`, character(1L), \"label\")",
    "invisible(lapply(reference_group_diagnostics,print))"
  )

  list(
    code = paste(c(
      "# Recalculate the tests from LinkEDA's prepared data with public R functions.",
      setup, if (multiple_imputation) imputed else ordinary
    ), collapse = "\n"),
    variables = variables,
    warnings = if (multiple_imputation) {
      "Multiple-imputation t tests use mice::pool.scalar. Welch ANOVA uses miceadds::micombine.F (D2 chi-square approximation, without finite Welch denominator df); classical ANOVA uses the recorded mice::D1 or mice::D3 method. Nonparametric MI tests are not offered by LinkEDA."
    } else character()
  )
}

.rls_attach_analysis_provenance <- function(record, code, title = "",
                                            output_code = list(),
                                            verification_code = list(),
                                            verification_variables = character(),
                                            verification_warnings = character(),
                                            dataset_version = NULL) {
  if (!is.character(code) || length(code) != 1L || is.na(code) || !nzchar(trimws(code))) {
    stop("Recorded analysis provenance requires non-empty executed R code.", call. = FALSE)
  }
  if (length(output_code)) {
    if (is.null(names(output_code)) || any(!nzchar(names(output_code))) ||
        any(!vapply(output_code, function(value) {
          is.character(value) && length(value) == 1L && !is.na(value) && nzchar(trimws(value))
        }, logical(1L)))) {
      stop("Every recorded output requires a named, non-empty R-code block.", call. = FALSE)
    }
  }
  if (length(verification_code)) {
    if (is.null(names(verification_code)) || any(!nzchar(names(verification_code))) ||
        any(!vapply(verification_code, function(value) {
          is.character(value) && length(value) == 1L && !is.na(value) && nzchar(trimws(value))
        }, logical(1L)))) {
      stop("Every verification output requires a named, non-empty R-code recipe.",
           call. = FALSE)
    }
  }
  dataset <- .rls_dataset_record(record$group %||% record$dataset_id)
  record$analysis_provenance <- list(
    schema = "LinkEDAAnalysisProvenance/v2",
    analysis_id = record$id %||% paste0("analysis:", dataset$group),
    title = title,
    dataset_id = dataset$dataset_id %||% dataset$group,
    dataset_version = as.integer(dataset_version %||% dataset$data_version %||% 1L),
    object_type = dataset$dataset_type %||% "data.frame",
    imputation_count = as.integer(dataset$imputation_count %||% 0L),
    code_origin = "recorded",
    executed_r_code = enc2utf8(code),
    output_r_code = output_code,
    verification_r_code = lapply(verification_code, enc2utf8),
    verification_variables = unique(as.character(verification_variables)),
    verification_warnings = unique(as.character(verification_warnings))
  )
  # Preserve requested rows independently of the model's complete-case sample.
  snapshot <- record$data_scope
  if (is.null(snapshot)) snapshot <- .rls_capture_analysis_scope(dataset,
    record$scope %||% "all", record$selected_rows %||% integer())
  if(is.null(snapshot$n)) {
    ids <- dataset$original_row_ids %||% seq_len(nrow(dataset$data))
    snapshot$rows <- if(snapshot$kind=="all") ids else snapshot$rows %||% integer()
    snapshot$n <- length(snapshot$rows)
  }
  used <- unique(as.integer(record$rows_used %||% record$rows_used_original_ids %||% integer()))
  if (length(used)) {
    record$effective_sample <- list(scope_n=snapshot$n, analyzed_n=length(used),
      excluded_n=length(setdiff(snapshot$rows,used)), rows=used,
      excluded_rows=setdiff(snapshot$rows,used))
  }
  if(!is.null(record$effective_sample)) {
    sample <- record$effective_sample
    sample_note <- sprintf("Analysis scope N: %d; analyzed N: %d; analysis-specific exclusions: %d.",
      sample$scope_n,sample$analyzed_n,sample$excluded_n)
    record$status <- paste(record$status %||% "",sample_note)
  }
  record$data_scope <- snapshot
  record$analysis_provenance$analysis_scope <- snapshot
  record$analysis_provenance$effective_sample <- record$effective_sample
  scope_note <- sprintf("# Analysis scope when computed: %s; N = %d of %d.\n# Model-specific exclusions are applied after this scope.",
    snapshot$description, snapshot$n, snapshot$total_n)
  record$analysis_provenance$verification_r_code <- lapply(
    record$analysis_provenance$verification_r_code, function(recipe) paste(scope_note,recipe,sep="\n"))
  record$analysis_provenance$missing_information <- .rls_mi_missing_information(record)
  record$analysis_provenance$missing_information_display_rows <- .rls_mi_diagnostic_display_rows(
    record, record$analysis_provenance$missing_information)
  if(is.list(record$multiple_imputation) && nrow(record$analysis_provenance$missing_information) &&
     !is.null(record$footnotes))record$footnotes <- unique(c(record$footnotes,.rls_mi_status_note(record)))
  if(nrow(record$analysis_provenance$missing_information)) {
    # These are the public pooled objects created by the verification recipes;
    # all values are freshly computed from exported, scoped imputations.
    record$analysis_provenance$verification_r_code <- lapply(
      record$analysis_provenance$verification_r_code,function(recipe)paste(recipe,
        "# Inspect missing-information diagnostics on the pooling scale.",
        "for (name in c('reference_model','reference_pooled_model','pooled_model')) {",
        "  if (exists(name, inherits=FALSE) && inherits(get(name), 'mipo')) {",
        "    missing_information <- get(name)$pooled",
        "    missing_information$mcse <- with(missing_information, sqrt(b/m))",
        "    missing_information$mcse_percent_se <- with(missing_information, ifelse(t>0, 100*mcse/sqrt(t), NA_real_))",
        "    print(missing_information)",
        "  }",
        "}",sep="\n"))
  }

  record
}

.rls_hex_encode_utf8 <- function(value) {
  value <- enc2utf8(as.character(value %||% "")[[1L]])
  paste(sprintf("%02x", as.integer(charToRaw(value))), collapse = "")
}

.rls_data_provenance_payload <- function(record, data = NULL) {
  if (is.null(record)) return(character())
  provenance <- record$data_provenance %||% list()
  source_data <- data %||% record$data %||% record$original_data
  if (is.null(source_data) && length(record$completed_datasets %||% list()))
    source_data <- record$completed_datasets[[record$active_imputation_version %||% 1L]]
  row_count <- if (is.data.frame(source_data)) nrow(source_data) else 0L
  dataset_id <- record$group %||% record$dataset_id %||%
    record$source_dataset_id %||% "dataset"
  stable_ids <- as.character(record$stable_row_ids %||%
    paste0(dataset_id, ":row:", seq_len(row_count)))
  history <- provenance$history %||% list()
  payload <- c(
    "DATAPROVENANCE_V1",
    as.character(as.integer(record$data_version %||% 1L)),
    .rls_native_wire_value(provenance$origin %||% "unavailable"),
    .rls_hex_encode_utf8(provenance$origin_code %||% ""),
    .rls_native_wire_value(provenance$origin_description %||% record$source %||% ""),
    as.character(length(stable_ids)),
    .rls_native_wire_value(stable_ids),
    as.character(length(history))
  )
  for (step in history) {
    parents <- as.character(step$parent_version_keys %||% character())
    inputs <- as.character(step$input_columns %||% character())
    outputs <- as.character(step$output_columns %||% character())
    rows <- as.character(step$stable_row_ids %||% character())
    payload <- c(
      payload,
      .rls_native_wire_value(step$id %||% ""),
      .rls_native_wire_value(step$label %||% ""),
      .rls_native_wire_value(step$origin %||% "unavailable"),
      .rls_hex_encode_utf8(step$r_code %||% ""),
      as.character(length(parents)), .rls_native_wire_value(parents),
      as.character(length(inputs)), .rls_native_wire_value(inputs),
      as.character(length(outputs)), .rls_native_wire_value(outputs),
      as.character(length(rows)), .rls_native_wire_value(rows)
    )
  }
  payload
}

.rls_analysis_provenance_payload <- function(record) {
  provenance <- record$analysis_provenance
  if (is.null(provenance) || !nzchar(provenance$executed_r_code %||% "")) return(character())
  outputs <- provenance$output_r_code %||% list()
  output_names <- names(outputs)
  if (is.null(output_names)) output_names <- character()
  verification <- provenance$verification_r_code %||% list()
  verification_names <- names(verification)
  if (is.null(verification_names)) verification_names <- character()
  variables <- unique(as.character(provenance$verification_variables %||% character()))
  warnings <- unique(as.character(provenance$verification_warnings %||% character()))
  payload <- c(
    "ANALYSIS_PROVENANCE_V2",
    .rls_native_wire_value(provenance$analysis_id %||% record$id %||% ""),
    .rls_native_wire_value(provenance$title %||% ""),
    .rls_native_wire_value(provenance$dataset_id %||% record$group %||% ""),
    as.character(as.integer(provenance$dataset_version %||% 1L)),
    .rls_native_wire_value(provenance$object_type %||% "data.frame"),
    as.character(as.integer(provenance$imputation_count %||% 0L)),
    .rls_native_wire_value(provenance$code_origin %||% "recorded"),
    .rls_hex_encode_utf8(provenance$executed_r_code),
    as.character(length(output_names))
  )
  for (name in output_names) {
    payload <- c(payload, .rls_native_wire_value(name),
                 .rls_hex_encode_utf8(outputs[[name]]))
  }
  payload <- c(payload, as.character(length(variables)),
               .rls_native_wire_value(variables),
               as.character(length(verification_names)))
  for (name in verification_names) {
    payload <- c(payload, .rls_native_wire_value(name),
                 .rls_hex_encode_utf8(verification[[name]]))
  }
  payload <- c(payload, as.character(length(warnings)),
               .rls_native_wire_value(warnings))
  diagnostics <- provenance$missing_information %||% data.frame()
  source_path <- provenance$prepared_data_path %||% ""
  if (nrow(diagnostics) || nzchar(source_path)) {
    payload <- c(payload, "MI_DIAGNOSTICS_V1", .rls_native_wire_value(source_path),
      as.character(ncol(diagnostics)), .rls_native_wire_value(names(diagnostics)), as.character(nrow(diagnostics)))
    for (i in seq_len(nrow(diagnostics))) payload <- c(payload,
      .rls_native_wire_value(vapply(diagnostics, function(column) {
        value <- column[[i]]
        if (is.numeric(value)) {
          if(is.na(value)) "\u2014" else sprintf("%.17g",value)
        } else as.character(value)
      }, character(1L))))
    display_rows <- provenance$missing_information_display_rows
    if (is.data.frame(display_rows) && nrow(display_rows)) {
      payload <- c(payload, "MI_DIAGNOSTIC_ROWS_V1", as.character(nrow(display_rows)))
      for (i in seq_len(nrow(display_rows))) payload <- c(payload,
        .rls_native_wire_value(as.character(unlist(display_rows[i,],use.names=FALSE))))
    }
  }

  if (isTRUE(provenance$frozen_scope)) {
    scope <- provenance$analysis_scope
    effective <- provenance$effective_sample
    row_ids <- scope$stable_row_ids
    used <- row_ids[match(effective$rows, scope$rows)]
    excluded <- setdiff(row_ids, used)
    payload <- c(payload, "FROZEN_ANALYSIS_SCOPE_V1",
      .rls_native_wire_value(c(scope$kind, scope$source_kind, scope$description)),
      as.character(scope$total_n), as.character(length(row_ids)), .rls_native_wire_value(row_ids),
      as.character(length(used)), .rls_native_wire_value(used),
      as.character(length(excluded)), .rls_native_wire_value(excluded))
  }
  payload
}

# Verification stays in the shared export layer and uses only public R APIs.
.rls_partial_plot_verification_r_code <- function(record, term, residual_type) {
  multiple <- length(.rls_regression_post_estimation_fits(record)) > 1L
  fit_code <- if (!is.null(record$family)) {
    .rls_generalized_executed_r_code(record, multiple_imputation = FALSE)
  } else .rls_linear_executed_r_code(record, multiple_imputation = FALSE)
  source <- paste(c(
    "# Refit with public package functions using the same prepared data and model specification.",
    "partial_input <- base::readRDS(verification_data_path)",
    if (multiple) c(
      'stopifnot(all(c(".imp", ".id") %in% names(partial_input)))',
      "partial_sources <- split(partial_input[partial_input$.imp > 0L, , drop = FALSE],",
      "                         partial_input$.imp[partial_input$.imp > 0L])",
      "partial_sources <- lapply(partial_sources, function(d) {",
      "  rownames(d) <- as.character(d$.id); d$.imp <- NULL; d$.id <- NULL; d",
      "})"
    ) else "partial_sources <- list(partial_input)",
    "partial_fits <- lapply(partial_sources, function(analysis_data) {",
    fit_code,
    "  model",
    "})"
  ), collapse = "\n")
  distribution <- .rls_discrete_diagnostic_distribution(record)
  paste(c(source,
    "# Component-plus-residual plot; contributions are centered on the fitted sample.",
    "# Each imputation is shown separately; these diagnostics are not pooled.",
    if (residual_type == "dunn_smyth") {
      "# Dunn-Smyth verification uses the captured per-imputation seed and original row IDs."
    },
    if (identical(record$family, "beta_one_inflated")) {
      "# GAMLSS supplies quantile residuals; randomization at the boundary can differ on refitting."
    },
    if (.rls_count_is_ceiling_hurdle(record)) {
      "# Contribution is the below-ceiling mean submodel in the success direction; verification fits the joint ZABB likelihood."
    },
    paste0("partial_term <- ", .rls_r_string_literal(term)),
    paste0("partial_residual_type <- ", .rls_r_string_literal(residual_type)),
    paste0("partial_distribution <- ", .rls_r_string_literal(distribution)),
    "partial_data <- do.call(rbind, lapply(seq_along(partial_fits), function(i) {",
    "  f <- partial_fits[[i]]",
    "  X <- stats::model.matrix(f)",
    '  labels <- attr(stats::terms(f), "term.labels")',
    '  j <- match(gsub("`", "", partial_term), gsub("`", "", labels))',
    '  columns <- which(attr(X, "assign") == j)',
    '  b <- if (inherits(f, "glmmTMB")) glmmTMB::fixef(f)$cond else',
    '    if (inherits(f, "betareg")) stats::coef(f, model = "mean") else',
    '    if (inherits(f, "gamlss")) stats::coef(f, what = "mu") else stats::coef(f)',
    '  if (partial_distribution == "hurdle_beta_binomial_ceiling") b <- -b',
    "  b <- b[colnames(X)[columns]]; b[is.na(b)] <- 0",
    "  block <- X[, columns, drop = FALSE]",
    '  if (attr(stats::terms(f), "intercept") == 1L) block <- sweep(block, 2L, colMeans(block), "-")',
    "  contribution <- as.numeric(block %*% b)",
    if (nzchar(distribution)) c(
      '  y <- if (inherits(f, "gamlss")) f$y else stats::model.response(stats::model.frame(f))',
      "  trials <- if (is.matrix(y)) rowSums(y) else rep(1, length(y))",
      "  observed <- if (is.matrix(y)) y[, 1L] else as.numeric(y)",
      "  mu <- as.numeric(stats::fitted(f))",
      if (residual_type == "dunn_smyth" && nzchar(.rls_dunn_smyth_verification_code(record))) c(
        "  diagnostic_imputation <- i",
        .rls_dunn_smyth_verification_code(record)
      ),
      '  if (partial_distribution %in% c("binomial", "binomial_trials", "perfect_score", "quasibinomial")) {',
      '    if (partial_distribution == "perfect_score") trials[] <- 1',
      "    expected <- trials * mu; variance <- trials * mu * (1 - mu)",
      '    if (partial_distribution == "quasibinomial") variance <- variance * summary(f)$dispersion',
      "    lower <- stats::pbinom(observed - 1, trials, mu); upper <- stats::pbinom(observed, trials, mu)",
      '  } else if (partial_distribution == "beta_binomial") {',
      "    precision <- as.numeric(stats::sigma(f))",
      "    expected <- trials * mu",
      "    variance <- trials * mu * (1 - mu) * (trials + precision) / (1 + precision)",
      "    lower <- gamlss.dist::pBB(observed - 1, mu = mu, sigma = 1 / precision, bd = trials)",
      "    upper <- gamlss.dist::pBB(observed, mu = mu, sigma = 1 / precision, bd = trials)",
      '  } else if (partial_distribution == "hurdle_beta_binomial_ceiling") {',
      "    # The model is fitted to failures; use its public zero-adjusted distribution.",
      paste0("    trials <- ", if(nzchar(record$trials_variable %||% "")) {
        paste0("partial_sources[[i]][rownames(X), ", .rls_r_string_literal(record$trials_variable), "]")
      } else paste0("rep(", record$trials_constant, ", nrow(X))")),
      "    observed <- trials - observed",
      "    lower <- 1 - gamlss.dist::pZABB(trials - observed, mu=f$mu.fv, sigma=f$sigma.fv, nu=f$nu.fv, bd=trials)",
      "    upper <- 1 - gamlss.dist::pZABB(trials - observed - 1, mu=f$mu.fv, sigma=f$sigma.fv, nu=f$nu.fv, bd=trials)",
      "    moments <- vapply(seq_along(observed), function(k) {",
      "      support <- 0:trials[k]",
      "      p <- gamlss.dist::dZABB(trials[k] - support, mu=f$mu.fv[k], sigma=f$sigma.fv[k], nu=f$nu.fv[k], bd=trials[k])",
      "      mean <- sum(support * p); c(mean, sum((support - mean)^2 * p))",
      "    }, numeric(2L))",
      "    expected <- moments[1L, ]; variance <- moments[2L, ]",
      '  } else if (partial_distribution == "negative_binomial") {',
      "    expected <- mu; variance <- mu + mu^2 / f$theta",
      "    lower <- stats::pnbinom(observed - 1, mu=mu, size=f$theta)",
      "    upper <- stats::pnbinom(observed, mu=mu, size=f$theta)",
      "  } else {",
      "    expected <- mu; variance <- mu",
      '    if (partial_distribution == "quasipoisson") variance <- variance * summary(f)$dispersion',
      "    lower <- stats::ppois(observed - 1, mu); upper <- stats::ppois(observed, mu)",
      "  }",
      "  residual <- switch(partial_residual_type,",
      "    raw = observed - expected, response = observed - expected,",
      "    pearson = (observed - expected) / sqrt(variance),",
      if (nzchar(.rls_dunn_smyth_verification_code(record)))
        "    dunn_smyth = if (inherits(f, 'glm')) diagnostic_statmod_residual(f) else if (partial_distribution == 'beta_binomial') diagnostic_beta_binomial_residual(observed,trials,mu,rep(precision,length.out=length(observed)),lower,upper) else diagnostic_cdf_residual(lower,upper)," else
        "    dunn_smyth = stop('This legacy fit has no captured residual seed. Refit before exporting Dunn-Smyth diagnostics.'),",
      '    stats::residuals(f, type = partial_residual_type))'
    ) else c(
      '  residual <- switch(partial_residual_type, raw = stats::residuals(f, type="response"),',
      '    standardized = if (inherits(f, "glm")) stats::rstandard(f, type="deviance") else stats::rstandard(f),',
      '    studentized = stats::rstudent(f),',
      '    if (inherits(f, "gamlss")) stats::residuals(f) else stats::residuals(f, type=partial_residual_type))'
    ),
    '  if (inherits(f, "lm") && !inherits(f, "glm") && partial_residual_type %in% c("standardized", "studentized")) {',
    "    # Both coordinates share the same row-specific residual scale.",
    "    influence <- stats::lm.influence(f, do.coef = FALSE)",
    '    sigma <- if (partial_residual_type == "studentized") influence$sigma else sqrt(stats::deviance(f) / stats::df.residual(f))',
    "    weights <- stats::weights(f)",
    "    if (is.null(weights)) weights <- rep(1, length(influence$hat))",
    "    scale <- sigma * sqrt(1 - influence$hat) / sqrt(weights)",
    "    contribution <- contribution / scale",
    "  }",
    "  data.frame(imputation = i, contribution, partial_residual = contribution + as.numeric(residual))",
    "}))",
    "partial_plot <- ggplot2::ggplot(partial_data, ggplot2::aes(contribution, partial_residual)) +",
    "  ggplot2::geom_point() + ggplot2::facet_wrap(~ imputation) + ggplot2::theme_minimal()",
    "print(partial_plot)"
  ), collapse = "\n")
}

.rls_mi_contingency_verification_r_code <- function(record) {
  mi <- identical(record$analysis_backend, "multiple_imputation")
  paste(c(
    if(mi) "# Descriptive averages across imputations; no inferential pooling." else "# Counts and percentages for the captured analysis scope.",
    "source_data <- base::readRDS(verification_data_path)",
    paste0("row_variables <- ", .rls_r_character_vector(record$row_variables)),
    paste0("column_variable <- ", .rls_r_string_literal(record$group_variable)),
    "variables <- c(row_variables, if(nzchar(column_variable)) column_variable)",
    if(mi) "completed <- split(source_data[source_data$.imp > 0L, , drop = FALSE], source_data$.imp[source_data$.imp > 0L])" else "completed <- list(source_data)",
    "# Match the category labels and order used when the table was calculated.",
    paste0("category_levels <- stats::setNames(list(", paste(vapply(
      record$category_levels, .rls_r_character_vector, character(1L)), collapse = ", "),
      "), variables)"),
    paste0("value_labels <- ", paste(deparse(record$category_value_labels), collapse = "\n")),
    "as_category <- function(x, levels, saved_labels) {",
    "  if (is.factor(x)) return(factor(as.character(x), levels = levels))",
    "  labels <- attr(x, \"labels\", exact = TRUE)",
    "  if (is.null(labels)) labels <- saved_labels",
    "  if (!is.null(labels)) {",
    "    raw <- unclass(x); attributes(raw) <- NULL",
    "    values <- as.character(raw); matched <- match(raw, unname(labels))",
    "    values[!is.na(matched)] <- names(labels)[matched[!is.na(matched)]]",
    "    x <- values",
    "  }",
    "  factor(x, levels = levels)",
    "}",
    "reference_tables <- lapply(completed, function(data) {",
    "  data <- data[, variables, drop = FALSE]",
    "  for (v in variables) data[[v]] <- as_category(data[[v]], category_levels[[v]], value_labels[[v]])",
    "  data <- data[stats::complete.cases(data), , drop = FALSE]",
    "  counts <- do.call(base::table, c(unname(data), list(useNA = \"no\")))",
    "  counts <- matrix(as.numeric(counts), nrow = prod(lengths(category_levels[row_variables])))",
    "  if(nzchar(column_variable)) counts <- cbind(counts, rowSums(counts))",
    "  rbind(counts, colSums(counts))",
    "})",
    "reference_mean_counts <- Reduce(`+`, reference_tables) / length(reference_tables)",
    "reference_percentages <- lapply(reference_tables, function(counts) {",
    "  result <- if(nzchar(column_variable)) counts / counts[, ncol(counts)] else counts / counts[nrow(counts), ncol(counts)]",
    "  result[!is.finite(result)] <- NA_real_; result",
    "})",
    "available <- Reduce(`+`, lapply(reference_percentages, function(p) !is.na(p)))",
    "reference_mean_row_percentages <- Reduce(`+`, lapply(reference_percentages, function(p) {",
    "  p[is.na(p)] <- 0; p",
    "})) / available",
    "reference_mean_row_percentages[available == 0] <- NA_real_",
    "row_keys <- expand.grid(category_levels[row_variables], KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)",
    if(!mi) c(
      "row_order <- do.call(order, lapply(seq_along(row_variables), function(j) match(row_keys[[j]], category_levels[[row_variables[[j]]]])))",
      "row_keys <- row_keys[row_order, , drop=FALSE]",
      "reference_mean_counts <- reference_mean_counts[c(row_order, nrow(reference_mean_counts)), , drop=FALSE]",
      "reference_mean_row_percentages <- reference_mean_row_percentages[c(row_order, nrow(reference_mean_row_percentages)), , drop=FALSE]"),
    "table_names <- list(c(unname(apply(row_keys, 1L, paste, collapse = \" / \")), \"Total\"),",
    "                    make.unique(c(\"Variable\", \"p\", \"Test\", if(nzchar(column_variable)) category_levels[[column_variable]], \"Total\"))[-seq_len(3L)])",
    "dimnames(reference_mean_counts) <- dimnames(reference_mean_row_percentages) <- table_names",
    "reference_counts <- reference_mean_counts",
    "reference_row_percentages <- reference_mean_row_percentages",
    "categories <- rbind(row_keys, stats::setNames(as.list(c('Total', rep('', length(row_variables)-1L))), row_variables))",
    paste0("display_mode <- ", .rls_r_string_literal(record$contingency_display_mode %||% "count_percent")),
    paste0("counts <- matrix(formatC(reference_counts, format='f', digits=", if(mi) "1" else "0", "), nrow=nrow(reference_counts))"),
    "percent <- matrix(ifelse(is.finite(reference_row_percentages), paste0(formatC(100*reference_row_percentages, format='f', digits=1), '%'), '\\u2014'), nrow=nrow(reference_counts))",
    "cells <- if(display_mode=='count') counts else if(display_mode=='percent') percent else matrix(paste0(counts, ' (', percent, ')'), nrow=nrow(counts))",
    "colnames(cells) <- colnames(reference_counts)",
    "reference_table <- data.frame(categories, cells, check.names=FALSE)",
    paste0("tinytable::tt(reference_table, caption=", .rls_r_string_literal(record$title), ", notes=", .rls_r_character_vector(record$footnotes), ")")
  ), collapse = "\n")
}

.rls_dendrogram_verification_r_code <- function(record, multiple_imputation = FALSE) {
  paste(c(
    "# Recalculate Quick Cluster with public R functions.",
    "analysis_data <- base::readRDS(verification_data_path)",
    if (multiple_imputation) c(
      sprintf("analysis_data <- analysis_data[analysis_data$.imp == %dL, , drop = FALSE]", record$active_imputation_version),
      "# One completed imputation; no pooling of cluster trees."),
    paste0("variables <- ", .rls_r_character_vector(record$variables)),
    "x <- as.matrix(analysis_data[, variables, drop = FALSE])",
    "stopifnot(!any(is.infinite(x)))",
    if (record$missing_mode == "listwise")
      "x <- x[stats::complete.cases(x), , drop = FALSE]" else
      "x <- x[rowSums(is.finite(x)) > 0L, , drop = FALSE]",
    "stopifnot(nrow(x) >= 2L)",
    "z <- base::scale(x)",
    "spread <- attr(z, 'scaled:scale')",
    "stopifnot(all(is.finite(spread) & spread > 0))",
    paste0("distances <- stats::dist(z, method = ",
           .rls_r_string_literal(record$distance), ")"),
    "# With missing values, dist scales by the number of available coordinates.",
    "# Cases without any jointly observed coordinate have undefined distances.",
    "stopifnot(all(is.finite(distances)))",
    paste0("reference_cluster <- stats::hclust(distances, method = ", .rls_r_string_literal(record$linkage), ")"),
    "graphics::plot(reference_cluster, main = 'Quick Cluster', xlab = '', sub = '')",
    "reference_cluster"
  ), collapse = "\n")
}


.rls_correlation_verification_code <- function(record) {
  mi <- identical(record$analysis_backend %||% "ordinary", "multiple_imputation")
  paste(c(
    "source_data <- readRDS(verification_data_path)",
    paste0("variables <- ", .rls_r_character_vector(record$variables)),
    paste0("listwise <- ", if(record$missing_mode=="listwise") "TRUE" else "FALSE"),
    "cell <- function(data, x, y) {",
    "  used <- stats::complete.cases(data[, if(listwise) variables else c(x,y), drop=FALSE])",
    "  a <- data[[x]][used]; b <- data[[y]][used]; n <- length(a)",
    "  if(x==y || n<3L || any(!is.finite(a)) || any(!is.finite(b)) || stats::sd(a)==0 || stats::sd(b)==0)",
    "    return(c(r=NA_real_, p=NA_real_, n=n))",
    "  fit <- stats::cor.test(a,b,method='pearson')",
    "  c(r=unname(fit$estimate),p=fit$p.value,n=n)",
    "}",
    if(mi) c(
      "completed <- split(source_data[source_data$.imp>0,,drop=FALSE], source_data$.imp[source_data$.imp>0])",
      "pool_cell <- function(x,y) {",
      "  values <- vapply(completed,function(data) cell(data,x,y),numeric(3L))",
      "  r <- values['r',]; n <- values['n',]",
      "  if(length(r)<2L || any(!is.finite(r)) || any(abs(r)>=1) || any(n<=3))",
      "    return(list(r=NA_real_,p=NA_real_,reason='Fisher-z pooling unavailable for one or more imputations'))",
      "  pooled <- mice::pool.scalar(atanh(r),1/(n-3),n=Inf,k=1,rule='rubin1987')",
      "  se <- sqrt(pooled$t); p <- 2*stats::pt(-abs(pooled$qbar/se),df=pooled$df)",
      "  ci <- tanh(pooled$qbar+c(-1,1)*stats::qt(.975,df=pooled$df)*se)",
      "  list(r=tanh(pooled$qbar),p=p,n=round(mean(n)),ci=ci,SE=se,df=pooled$df,diagnostics=pooled)",
      "}",
      "reference_correlations <- lapply(variables,function(y) lapply(variables,function(x) pool_cell(x,y)))"
    ) else "reference_correlations <- lapply(variables,function(y) lapply(variables,function(x) cell(source_data,x,y)))",
    "reference_correlations"), collapse="\n")
}


.rls_frequency_verification_r_code <- function(record) {
  mi <- identical(record$analysis_backend,"multiple_imputation")
  paste(c(
    "source_data <- base::readRDS(verification_data_path)",
    if(mi) "completed <- split(source_data[source_data$.imp > 0L, , drop=FALSE], source_data$.imp[source_data$.imp > 0L])" else "completed <- list(source_data)",
    paste0("variable <- ",.rls_r_string_literal(record$variables[[1L]])),
    paste0("category_levels <- ",.rls_r_character_vector(record$category_levels)),
    paste0("saved_labels <- ",paste(deparse(record$value_labels),collapse="\n")),
    "frequency_by_imputation <- lapply(completed, function(data) {",
    "  x <- data[[variable]]",
    "  if (!is.factor(x) && !is.null(saved_labels)) {",
    "    raw <- unclass(x); attributes(raw) <- NULL; x <- as.character(raw)",
    "    matched <- match(raw, unname(saved_labels)); found <- !is.na(matched)",
    "    x[found] <- names(saved_labels)[matched[found]]",
    "  }",
    "  x <- factor(x, levels=category_levels)",
    "  counts <- base::table(x, useNA='no'); valid <- sum(counts)",
    "  list(counts=c(valid, as.numeric(counts), sum(is.na(x))),",
    "    percentages=c(if(valid>0) 1 else NA_real_, as.numeric(base::prop.table(counts)), if(length(x)) mean(is.na(x)) else NA_real_))",
    "})",
    "reference_counts <- rowMeans(do.call(cbind, lapply(frequency_by_imputation, `[[`, 'counts')))",
    "reference_percentages <- rowMeans(do.call(cbind, lapply(frequency_by_imputation, `[[`, 'percentages')), na.rm=TRUE)",
    "reference_percentages[!is.finite(reference_percentages)] <- NA_real_",
    "data.frame(Category=c('Valid N',category_levels,'Missing'), N=reference_counts, Percent=100*reference_percentages)"
  ),collapse="\n")
}


# Post-estimation recipes inherit the fitted model's immutable prepared data and
# scope. These use public emmeans/mice calls; no LinkEDA engine is exported.
.rls_lognormal_recipe_base <- function(record) {
  provenance <- record$analysis_provenance
  base <- provenance$verification_r_code$model %||% provenance$verification_r_code$table
  if (is.null(base)) base <- provenance$verification_r_code[[1L]]
  c(base, if (length(.rls_regression_post_estimation_fits(record)) == 1L) "fits <- list(fit)")
}

.rls_lognormal_pairwise_verification_r_code <- function(record, term, scale, adjust, confidence_level) {
  paste(c(
    .rls_lognormal_recipe_base(record),
    "target <- if (length(fits) == 1L) fits[[1L]] else mice::as.mira(fits)",
    paste0("grid <- emmeans::emmeans(target, specs = ", .rls_r_string_literal(term),
      ", data = stats::model.frame(fits[[1L]]))"),
    "grid <- stats::update(grid, tran = 'log')",
    if (scale %in% c("response", "probability")) c(
      "# Differences of conditional medians, not arithmetic means.",
      "# MI combines the log-scale reference grid before back-transformation.",
      "grid <- emmeans::regrid(grid, transform = 'response')"),
    paste0("contrast <- emmeans::contrast(grid, 'pairwise', adjust = ", .rls_r_string_literal(adjust), ")"),
    paste0("reference <- as.data.frame(summary(contrast, infer = c(TRUE, TRUE), level = ",
      format(confidence_level, digits = 17), ", type = 'link'))"),
    if (scale %in% c("odds_ratio", "risk_ratio", "mean_ratio", "multiplicative_ratio")) c(
      "reference$ratio <- exp(reference$estimate)",
      "reference$ratio_conf_low <- exp(reference$lower.CL)",
      "reference$ratio_conf_high <- exp(reference$upper.CL)"),
    "reference"
  ), collapse = "\n")
}

.rls_lognormal_effect_provenance <- function(record, focal, conditioning, at, confidence_level, means_table = TRUE) {
  provenance <- record$analysis_provenance
  if (is.null(provenance)) return(NULL)
  at_code <- paste(capture.output(dput(at)), collapse = "\n")
  recipe <- paste(c(
    .rls_lognormal_recipe_base(record),
    paste0("effect_at <- ", at_code),
    "# Exact fitted lognormal means; residual variance is treated as fixed for intervals.",
    "per_imputation <- lapply(fits, function(fit) {",
    paste0("  grid <- emmeans::emmeans(fit, specs = ", .rls_r_string_literal(focal),
      ", by = ", if (length(conditioning)) .rls_r_character_vector(conditioning) else "NULL", ","),
    "    at = effect_at, data = stats::model.frame(fit), offset = stats::sigma(fit)^2 / 2)",
    "  grid <- stats::update(grid, tran = 'log')",
    paste0("  as.data.frame(summary(grid, type = 'response', bias.adjust = FALSE, infer = c(TRUE, FALSE), level = ",
      format(confidence_level, digits = 17), "))"),
    "})",
    "reference <- per_imputation[[1L]]",
    "if (length(fits) > 1L) {",
    "  for (i in seq_len(nrow(reference))) {",
    "    q <- vapply(per_imputation, function(x) x$response[i], numeric(1))",
    "    u <- vapply(per_imputation, function(x) x$SE[i]^2, numeric(1))",
    "    dfcom <- min(vapply(per_imputation, function(x) x$df[i], numeric(1)))",
    "    pooled <- mice::pool.scalar(q, u, n = dfcom + 1, k = 1, rule = 'rubin1987')",
    "    reference$response[i] <- pooled$qbar; reference$SE[i] <- sqrt(pooled$t)",
    "    reference$df[i] <- pooled$df",
    paste0("    interval <- pooled$qbar + stats::qt(c(", format((1-confidence_level)/2, digits=17), ", ",
      format((1+confidence_level)/2, digits=17), "), pooled$df) * sqrt(pooled$t)"),
    "    reference$lower.CL[i] <- interval[1]; reference$upper.CL[i] <- interval[2]",
    "  }",
    "}",
    "reference"
  ), collapse = "\n")
  provenance$verification_r_code <- list(effect = recipe, plot = recipe)
  provenance$output_r_code <- list(effect = "reference", plot = "reference")
  if (isTRUE(means_table)) {
    provenance$verification_r_code$table <- paste(
      "# Verifies the arithmetic-mean section. Log-scale contrasts are a separate quantity.",
      recipe, sep = "\n")
    provenance$output_r_code$table <- "reference"
  }
  # The new rows describe means, not model coefficients.
  provenance$missing_information <- data.frame()
  provenance$missing_information_display_rows <- NULL
  provenance
}
