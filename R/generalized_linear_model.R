.rls_generalized_glm_id <- function(group, name = NULL) {
  .rls_validate_protocol_name(name %||% paste0("gglm:", group, ":", format(Sys.time(), "%Y%m%d%H%M%OS3")), "name")
}

.rls_generalized_glm_record <- function(model) {
  id <- if (inherits(model, "rlispstat_generalized_linear_model")) model$id else model
  id <- .rls_validate_protocol_name(id, "model")
  if (!exists(id, envir = .rls_state$generalized_glm_models, inherits = FALSE)) {
    stop("Unknown generalized linear model.", call. = FALSE)
  }
  get(id, envir = .rls_state$generalized_glm_models)
}

.rls_assign_generalized_glm <- function(record) {
  assign(record$id, record, envir = .rls_state$generalized_glm_models)
  structure(list(id = record$id, group = record$group), class = "rlispstat_generalized_linear_model")
}

.rls_glm_supported_families <- c(
  "gaussian", "binomial", "poisson", "Gamma", "inverse.gaussian",
  "quasibinomial", "quasipoisson"
)

.rls_glm_family_links <- list(
  gaussian = c("identity", "log", "inverse"),
  binomial = c("logit", "probit", "cloglog", "cauchit", "log"),
  poisson = c("log", "identity", "sqrt"),
  Gamma = c("inverse", "identity", "log"),
  inverse.gaussian = c("1/mu^2", "inverse", "identity", "log"),
  quasibinomial = c("logit", "probit", "cloglog", "cauchit", "log"),
  quasipoisson = c("log", "identity", "sqrt")
)

.rls_glm_default_link <- function(family) {
  switch(family,
    gaussian = "identity",
    binomial = "logit",
    poisson = "log",
    Gamma = "inverse",
    inverse.gaussian = "1/mu^2",
    quasibinomial = "logit",
    quasipoisson = "log"
  )
}

.rls_glm_make_family <- function(family, link = NULL) {
  family <- match.arg(family, .rls_glm_supported_families)
  link <- link %||% .rls_glm_default_link(family)
  allowed <- .rls_glm_family_links[[family]]
  if (!link %in% allowed) {
    stop(sprintf(
      "Invalid link `%s` for family `%s`. Valid links are: %s.",
      link, family, paste(allowed, collapse = ", ")
    ), call. = FALSE)
  }
  constructor <- get(family, envir = asNamespace("stats"))
  tryCatch(
    constructor(link = link),
    error = function(e) stop(sprintf("Invalid family/link combination: %s", conditionMessage(e)), call. = FALSE)
  )
}

.rls_generalized_glm_formula_object <- function(record) {
  .rls_model_formula_object(record$response, record$terms)
}

.rls_generalized_glm_data_for_fit <- function(record) {
  .rls_model_data_for_term_types(record$data, record$term_types %||% list(), response = record$response)
}

.rls_generalized_glm_complete_data <- function(record) {
  .rls_model_complete_data(record$data, .rls_generalized_glm_formula_object(record),
                           record$scope %||% "all", record$selected_rows %||% integer())
}

.rls_generalized_glm_diagnostics <- function(record, fit, complete) {
  response <- all.vars(stats::formula(fit))[[1L]]
  leverage <- tryCatch(stats::hatvalues(fit), error = function(e) rep(NA_real_, length(stats::fitted(fit))))
  cooks <- tryCatch(stats::cooks.distance(fit), error = function(e) rep(NA_real_, length(stats::fitted(fit))))
  data.frame(
    row_id = complete$rows,
    original_row_id = complete$rows,
    observed = complete$data[[response]],
    fitted = unname(stats::fitted(fit)),
    fitted_response_scale = unname(stats::fitted(fit)),
    linear_predictor = unname(stats::predict(fit, type = "link")),
    residual = unname(stats::residuals(fit, type = "deviance")),
    deviance_residual = unname(stats::residuals(fit, type = "deviance")),
    pearson_residual = unname(stats::residuals(fit, type = "pearson")),
    working_residual = tryCatch(unname(stats::residuals(fit, type = "working")), error = function(e) NA_real_),
    leverage = unname(leverage),
    cooks_distance = unname(cooks),
    stringsAsFactors = FALSE
  )
}

.rls_generalized_glm_extract_fit <- function(record, fit, complete) {
  summary_fit <- summary(fit)
  coef_matrix <- as.data.frame(unclass(summary_fit$coefficients), stringsAsFactors = FALSE)
  if (ncol(coef_matrix) < 4L) {
    stop("Could not extract generalized linear model coefficients.", call. = FALSE)
  }
  statistic_label <- if (grepl("^z", names(coef_matrix)[[3L]], ignore.case = TRUE)) "z" else "t"
  names(coef_matrix)[1:4] <- c("estimate", "std_error", "statistic", "p_value")
  coef_matrix$term <- rownames(summary_fit$coefficients)
  coef_matrix$partial_r2 <- NA_real_
  coef_matrix <- coef_matrix[, c("term", "estimate", "std_error", "statistic", "p_value", "partial_r2")]
  rownames(coef_matrix) <- NULL
  aic <- tryCatch(stats::AIC(fit), error = function(e) NA_real_)
  bic <- tryCatch(stats::BIC(fit), error = function(e) NA_real_)
  log_lik <- tryCatch(as.numeric(stats::logLik(fit)), error = function(e) NA_real_)
  list(
    fit = fit,
    fitted_glm = fit,
    coefficients = coef_matrix,
    coefficient_rows = .rls_model_coefficient_display_rows(
      fit, coef_matrix, record$data, record$terms, statistic_name = statistic_label
    ),
    summary = list(
      n_used = length(complete$rows),
      n_excluded = complete$excluded,
      null_deviance = unname(fit$null.deviance),
      residual_deviance = unname(fit$deviance),
      df_residual = stats::df.residual(fit),
      aic = aic,
      bic = bic,
      dispersion = unname(summary_fit$dispersion),
      log_lik = log_lik,
      family = record$family,
      link = record$link
    ),
    rows_used = complete$rows,
    rows_excluded = setdiff(seq_len(nrow(record$data)), complete$rows),
    diagnostics = .rls_generalized_glm_diagnostics(record, fit, complete),
    status = sprintf("GLM fitted successfully; %d rows used, %d excluded.", length(complete$rows), complete$excluded)
  )
}

.rls_generalized_glm_fit_rows <- function(summary) {
  data.frame(
    label = c("N", "Null deviance", "Residual deviance", "df residual", "AIC", "BIC", "Dispersion", "logLik"),
    value = c(
      as.character(summary$n_used %||% NA_integer_),
      .rls_export_format_number(summary$null_deviance %||% NA_real_, 3L),
      .rls_export_format_number(summary$residual_deviance %||% NA_real_, 3L),
      as.character(summary$df_residual %||% NA_integer_),
      .rls_export_format_number(summary$aic %||% NA_real_, 1L),
      .rls_export_format_number(summary$bic %||% NA_real_, 1L),
      .rls_export_format_number(summary$dispersion %||% NA_real_, 3L),
      .rls_export_format_number(summary$log_lik %||% NA_real_, 3L)
    ),
    stringsAsFactors = FALSE
  )
}

.rls_generalized_glm_normalize_term_types <- function(term_types, terms) {
  if (is.null(term_types) || !length(term_types)) return(list())
  if (!is.list(term_types)) term_types <- as.list(term_types)
  term_names <- names(term_types)
  if (is.null(term_names)) return(list())
  out <- list()
  for (i in seq_along(term_types)) {
    term <- term_names[[i]]
    type <- as.character(term_types[[i]])[[1L]]
    if (!nzchar(term) || !term %in% terms || !type %in% c("numeric", "factor")) next
    out[[term]] <- type
  }
  out
}

.rls_generalized_glm_native_payload <- function(record) {
  term_types <- record$term_types %||% list()
  type_payload <- as.character(length(term_types))
  if (length(term_types)) {
    for (term in names(term_types)) {
      type_payload <- c(type_payload, .rls_native_wire_value(term), .rls_native_wire_value(term_types[[term]]))
    }
  }
  data_scope <- record$data_scope %||% list()
  scope_kind <- as.character(data_scope$kind %||% if (identical(record$scope, "all")) "all" else "explicit")[[1L]]
  scope_rows <- as.integer(data_scope$rows %||% if (identical(record$scope, "selected")) {
    record$selected_rows %||% integer()
  } else if (identical(record$scope, "unselected")) {
    setdiff(seq_len(nrow(record$data)), record$selected_rows %||% integer())
  } else {
    integer()
  })
  scope_rows <- unique(scope_rows[is.finite(scope_rows) & scope_rows >= 1L & scope_rows <= nrow(record$data)])
  scope_source_kind <- as.character(data_scope$source_kind %||%
    if (identical(scope_kind, "all")) "all_data" else "other_explicit_subset")[[1L]]
  scope_description <- as.character(data_scope$description %||%
    if (identical(scope_kind, "all")) "All observations" else "Explicit subset")[[1L]]
  scope_payload <- c(
    "ANALYSIS_SCOPE_V1",
    .rls_native_wire_value(scope_kind),
    .rls_native_wire_value(scope_source_kind),
    .rls_native_wire_value(scope_description),
    as.character(nrow(record$data)),
    as.character(length(scope_rows)),
    as.character(scope_rows)
  )
  c(
    "GENERALIZED_GLM_OPEN_STRUCTURED",
    .rls_native_wire_value(record$id),
    .rls_native_wire_value(record$group),
    .rls_native_wire_value(if (isTRUE(record$binary_regression)) "Binary Regression" else "Generalized Linear Model"),
    .rls_native_wire_value(record$response),
    .rls_native_wire_value(record$family),
    .rls_native_wire_value(record$link),
    .rls_native_wire_value(record$scope %||% "all"),
    .rls_native_wire_value(record$status %||% ""),
    as.character(length(record$terms %||% character())),
    .rls_native_wire_value(record$terms %||% character()),
    type_payload,
    scope_payload,
    .rls_native_generalized_state_payload(record)
  )
}

.rls_generalized_glm_sync_native <- function(record) {
  .rls_start_backend()
  dataset <- .rls_dataset_record(record$group)
  try(.rls_send(c(
    "REGISTER_DATASET_SILENT",
    record$group,
    .rls_variable_payload(record$data, dataset$variable_metadata),
    .rls_dataframe_payload(record$data, dataset$variable_metadata, dataset_record = dataset)
  )), silent = TRUE)
  try(.rls_send(.rls_generalized_glm_native_payload(record)), silent = TRUE)
}

.rls_generalized_glm_sync_native_error <- function(id, group, response, family, link, scope, terms,
                                                   term_types = list(), message,
                                                   binary = FALSE, event = "", reference = "") {
  dataset <- tryCatch(.rls_dataset_record(group), error = function(e) NULL)
  record <- list(
    id = id,
    glm_model_id = id,
    group = group,
    dataset_id = group,
    data = if (is.null(dataset)) data.frame() else dataset$data,
    response = response,
    response_variable = response,
    terms = terms %||% character(),
    term_types = .rls_generalized_glm_normalize_term_types(term_types, terms %||% character()),
    family = family,
    link = link,
    binary_regression = isTRUE(binary),
    event = event,
    reference = reference,
    scope = scope %||% "all",
    fitted_glm = NULL,
    fit = NULL,
    coefficients = data.frame(),
    coefficient_rows = data.frame(),
    fit_statistics = list(),
    summary = list(),
    diagnostic_data = data.frame(),
    diagnostics = data.frame(),
    diagnostics_by_imputation = NULL,
    fits_by_imputation = NULL,
    multiple_imputation = NULL,
    rows_used = integer(),
    rows_excluded = integer(),
    status = message
  )
  try(.rls_send(.rls_generalized_glm_native_payload(record)), silent = TRUE)
}

#' Create a generalized linear model using stats::glm()
#'
#' @param data Registered dataset name, a data frame, or `NULL` for active dataset.
#' @param response Response variable.
#' @param terms Predictor terms.
#' @param family GLM family.
#' @param link Link function. If `NULL`, the family default is used.
#' @param scope One of `"all"`, `"selected"`, or `"unselected"`.
#' @param name Optional model id or dataset name for data-frame input.
#' @param native Logical. Reserved for native window integration.
#' @param term_types Optional named vector/list with per-term `"numeric"` or `"factor"` overrides.
#' @param .selected_rows Internal explicit original-row IDs supplied by the native analysis-scope bridge.
#' @param .scope_description Internal immutable analysis-scope description for a native result window.
#' @param .scope_source_kind Internal analysis-scope source identifier for a native result window.
#' @return An `rlispstat_generalized_linear_model` handle.
#' @export
ls_new_generalized_linear_model <- function(data = NULL, response = NULL, terms = NULL,
                                            family = "binomial", link = NULL,
                                            scope = "all", name = NULL, native = TRUE,
                                            term_types = NULL, .selected_rows = NULL,
                                            .scope_description = NULL,
                                            .scope_source_kind = NULL) {
  scope <- match.arg(scope, c("all", "selected", "unselected"))
  family_object <- .rls_glm_make_family(family, link)
  link <- family_object$link
  if (is.data.frame(data)) {
    group <- .rls_register_dataset(name %||% "generalized_linear_model", data, activate = TRUE)
    dataset <- .rls_dataset_record(group)
  } else {
    dataset <- .rls_dataset_record(data)
  }
  response <- response %||% names(dataset$data)[[1L]]
  response <- .rls_validate_protocol_name(response, "response")
  if (!response %in% names(dataset$data)) {
    stop(sprintf("Column `%s` was not found in the dataset.", response), call. = FALSE)
  }
  terms <- unlist(lapply(terms %||% character(), function(term) {
    term <- .rls_model_validate_term(dataset$data, term, response = response, what = "term")
    .rls_model_hierarchical_terms(dataset$data, term, response = response)
  }), use.names = FALSE)
  terms <- if (length(terms)) unique(as.character(terms)) else character()
  term_types <- .rls_generalized_glm_normalize_term_types(term_types, terms)
  model_id <- .rls_generalized_glm_id(dataset$group, name)
  record <- list(
    id = model_id,
    glm_model_id = model_id,
    group = dataset$group,
    dataset_id = dataset$group,
    data = dataset$data,
    dataset_type = dataset$dataset_type %||% "data_frame",
    analysis_backend = .rls_analysis_backend(dataset, "generalized_linear_model"),
    imputation_id = dataset$imputation_id %||% NULL,
    imputation_count = dataset$imputation_count %||% NULL,
    response = response,
    response_variable = response,
    terms = terms,
    term_types = term_types,
    family = family,
    link = link,
    family_object = family_object,
    scope = scope,
    selected_rows = if (!is.null(.selected_rows)) as.integer(.selected_rows) else
      if (scope != "all") ls_selected(dataset$group) else integer(),
    data_scope = list(
      kind = if (identical(scope, "all")) "all" else "explicit",
      rows = if (identical(scope, "all")) integer() else
        if (identical(scope, "unselected")) {
          setdiff(seq_len(nrow(dataset$data)), if (!is.null(.selected_rows)) as.integer(.selected_rows) else ls_selected(dataset$group))
        } else if (!is.null(.selected_rows)) as.integer(.selected_rows) else ls_selected(dataset$group),
      source_kind = .scope_source_kind %||% if (identical(scope, "all")) "all_data" else "other_explicit_subset",
      description = .scope_description %||% if (identical(scope, "all")) "All observations" else "Explicit subset"
    ),
    fitted_glm = NULL,
    fit = NULL,
    coefficients = data.frame(),
    coefficient_rows = data.frame(),
    fit_statistics = list(),
    summary = list(),
    diagnostic_data = data.frame(),
    diagnostics = data.frame(),
    diagnostics_by_imputation = NULL,
    fits_by_imputation = NULL,
    multiple_imputation = NULL,
    model_version = 0L,
    fit_version = 0L,
    diagnostics_version = 0L,
    rows_used = integer(),
    rows_excluded = integer(),
    status = if (length(terms)) "Not fitted." else "Add at least one predictor."
  )
  handle <- .rls_assign_generalized_glm(record)
  if (length(terms)) {
    handle <- ls_generalized_linear_model_fit(handle)
  }
  record <- .rls_generalized_glm_record(handle)
  if (isTRUE(native) && identical(record$analysis_backend, "multiple_imputation")) {
    if (is.null(record$fit)) {
      warning("Add at least one predictor before opening the pooled MI Generalized Linear Model table.", call. = FALSE)
    } else {
      .rls_generalized_glm_sync_native_pooled(record)
    }
  } else if (isTRUE(native)) {
    .rls_start_backend()
    try(.rls_send(c(
      "REGISTER_DATASET_SILENT",
      dataset$group,
      .rls_variable_payload(dataset$data, dataset$variable_metadata),
      .rls_dataframe_payload(dataset$data, dataset$variable_metadata, dataset_record = dataset)
    )), silent = TRUE)
    if (is.null(record$fit)) {
      try(.rls_send(c("GENERALIZED_GLM_OPEN", dataset$group, response, family, link, as.character(length(terms)), terms)), silent = TRUE)
    } else {
      .rls_generalized_glm_sync_native(record)
    }
  }
  invisible(handle)
}

#' @rdname ls_new_generalized_linear_model
#' @export
ls_generalized_linear_model_fit <- function(model) {
  record <- .rls_generalized_glm_record(model)
  if (!length(record$terms)) {
    stop("Add at least one predictor before fitting.", call. = FALSE)
  }
  record$family_object <- .rls_glm_make_family(record$family, record$link)
  dataset <- .rls_dataset_record(record$group)
  record$analysis_backend <- .rls_analysis_backend(dataset, "generalized_linear_model")
  if (identical(record$analysis_backend, "multiple_imputation")) {
    extracted <- .rls_mi_fit_generalized_model_record(record)
  } else {
    fit_record <- record
    fit_record$data <- .rls_generalized_glm_data_for_fit(record)
    complete <- .rls_generalized_glm_complete_data(fit_record)
    if (nrow(complete$data) <= length(record$terms)) {
      stop("Not enough complete cases to fit the generalized linear model.", call. = FALSE)
    }
    fit <- tryCatch(
      stats::glm(.rls_generalized_glm_formula_object(fit_record), data = complete$data, family = record$family_object),
      error = function(e) stop(sprintf("Could not fit generalized linear model: %s", conditionMessage(e)), call. = FALSE)
    )
    extracted <- .rls_generalized_glm_extract_fit(fit_record, fit, complete)
  }
  record[names(extracted)] <- extracted
  record$fit_statistics <- extracted$summary
  record$diagnostic_data <- extracted$diagnostics
  record$fit_version <- record$fit_version + 1L
  record$diagnostics_version <- record$diagnostics_version + 1L
  handle <- .rls_assign_generalized_glm(record)
  if (identical(record$analysis_backend, "multiple_imputation") && isTRUE(.rls_state$process_started)) {
    try(.rls_generalized_glm_sync_native_pooled(record), silent = TRUE)
  } else if (isTRUE(.rls_state$process_started)) {
    try(.rls_generalized_glm_sync_native(record), silent = TRUE)
  }
  handle
}

#' @rdname ls_new_generalized_linear_model
#' @export
ls_generalized_linear_model_set_family <- function(model, family, link = NULL) {
  record <- .rls_generalized_glm_record(model)
  family_object <- .rls_glm_make_family(family, link)
  record$family <- family
  record$link <- family_object$link
  record$family_object <- family_object
  record$model_version <- record$model_version + 1L
  record$fit <- NULL
  record$fitted_glm <- NULL
  .rls_assign_generalized_glm(record)
  if (length(record$terms)) ls_generalized_linear_model_fit(model) else invisible(model)
}

#' @rdname ls_new_generalized_linear_model
#' @export
ls_generalized_linear_model_set_link <- function(model, link) {
  record <- .rls_generalized_glm_record(model)
  ls_generalized_linear_model_set_family(model, record$family, link)
}

#' @rdname ls_new_generalized_linear_model
#' @export
ls_generalized_linear_model_coefficients <- function(model) {
  record <- .rls_generalized_glm_record(model)
  if (is.null(record$fit)) record <- .rls_generalized_glm_record(ls_generalized_linear_model_fit(model))
  record$coefficients
}

#' @rdname ls_new_generalized_linear_model
#' @export
ls_generalized_linear_model_coefficient_rows <- function(model) {
  record <- .rls_generalized_glm_record(model)
  if (is.null(record$fit)) record <- .rls_generalized_glm_record(ls_generalized_linear_model_fit(model))
  record$coefficient_rows
}

#' @rdname ls_new_generalized_linear_model
#' @export
ls_generalized_linear_model_fit_summary <- function(model) {
  record <- .rls_generalized_glm_record(model)
  if (is.null(record$fit)) record <- .rls_generalized_glm_record(ls_generalized_linear_model_fit(model))
  record$summary
}

#' @rdname ls_new_generalized_linear_model
#' @export
ls_generalized_linear_model_diagnostics <- function(model) {
  record <- .rls_generalized_glm_record(model)
  if (is.null(record$fit)) record <- .rls_generalized_glm_record(ls_generalized_linear_model_fit(model))
  record$diagnostics
}

#' @rdname ls_new_generalized_linear_model
#' @export
ls_generalized_linear_model_open_diagnostic <- function(model, type = c("observed_vs_fitted", "residuals_vs_fitted", "residual_histogram"),
                                                        native = FALSE) {
  type <- match.arg(type)
  record <- .rls_generalized_glm_record(model)
  if (is.null(record$fit)) record <- .rls_generalized_glm_record(ls_generalized_linear_model_fit(model))
  id <- paste(record$id, type, length(ls(.rls_state$glm_diagnostic_plots)) + 1L, sep = ":")
  plot <- list(
    id = id,
    model_id = record$id,
    type = type,
    native_plot_id = NULL,
    displayed_fit_version = record$fit_version,
    displayed_diagnostics_version = record$diagnostics_version,
    is_stale = FALSE,
    data = record$diagnostics
  )
  assign(id, plot, envir = .rls_state$glm_diagnostic_plots)
  structure(plot, class = "rlispstat_glm_diagnostic")
}

#' @rdname ls_new_generalized_linear_model
#' @export
ls_generalized_linear_model_state <- function(model) {
  .rls_generalized_glm_record(model)
}
