.rls_model_distribution_catalogue <- list(
  gaussian = list(
    id = "gaussian", label = "Gaussian", domain = "continuous",
    links = "identity", default_link = "identity", backend = "stats::lm",
    distribution = TRUE, likelihood = TRUE, aic = TRUE, bic = TRUE,
    deviance = TRUE, dispersion_parameter = FALSE
  ),
  gaussian_log = list(
    id = "gaussian_log", label = "Gaussian (log link)", domain = "continuous",
    links = "log", default_link = "log", backend = "stats::glm",
    response_transformation = "none", coefficient_interpretation = "mean_ratio",
    response_prediction = "mean", diagnostic_type = "gaussian_glm",
    distribution = TRUE, likelihood = TRUE, aic = TRUE, bic = TRUE,
    deviance = TRUE, dispersion_parameter = TRUE
  ),
  lognormal = list(
    id = "lognormal", label = "Lognormal", domain = "strictly_positive",
    links = "identity", default_link = "identity", backend = "stats::lm",
    response_transformation = "log", coefficient_interpretation = "multiplicative_ratio",
    response_prediction = "arithmetic_mean", diagnostic_type = "log_response_lm",
    distribution = TRUE, likelihood = TRUE, aic = TRUE, bic = TRUE,
    deviance = TRUE, dispersion_parameter = TRUE
  ),
  binomial = list(
    id = "binomial", label = "Binomial", domain = "binary",
    links = c("logit", "log", "probit", "cloglog"), default_link = "logit",
    backend = "stats::glm", distribution = TRUE, likelihood = TRUE,
    aic = TRUE, bic = TRUE, deviance = TRUE, dispersion_parameter = FALSE
  ),
  poisson = list(
    id = "poisson", label = "Poisson", domain = "non_negative_integer",
    links = "log", default_link = "log", backend = "stats::glm",
    distribution = TRUE, likelihood = TRUE, aic = TRUE, bic = TRUE,
    deviance = TRUE, dispersion_parameter = FALSE
  ),
  negative_binomial = list(
    id = "negative_binomial", label = "Negative binomial",
    domain = "non_negative_integer", links = "log", default_link = "log",
    backend = "MASS::glm.nb", distribution = TRUE, likelihood = TRUE,
    aic = TRUE, bic = TRUE, deviance = TRUE, dispersion_parameter = TRUE
  ),
  Gamma = list(
    id = "Gamma", label = "Gamma", domain = "strictly_positive",
    links = c("log", "inverse", "identity"), default_link = "log",
    backend = "stats::glm", distribution = TRUE, likelihood = TRUE,
    aic = TRUE, bic = TRUE, deviance = TRUE, dispersion_parameter = TRUE
  ),
  inverse.gaussian = list(
    id = "inverse.gaussian", label = "Inverse Gaussian",
    domain = "strictly_positive",
    links = c("log", "inverse", "identity", "1/mu^2"), default_link = "log",
    backend = "stats::glm", distribution = TRUE, likelihood = TRUE,
    aic = TRUE, bic = TRUE, deviance = TRUE, dispersion_parameter = TRUE
  ),
  beta = list(
    id = "beta", label = "Beta", domain = "open_unit_interval",
    links = "logit", default_link = "logit", backend = "betareg::betareg",
    distribution = TRUE, likelihood = TRUE, aic = TRUE, bic = TRUE,
    deviance = TRUE, dispersion_parameter = TRUE
  ),
  binomial_trials = list(
    id = "binomial_trials", label = "Binomial (successes/trials)",
    domain = "non_negative_integer", links = "logit", default_link = "logit",
    backend = "stats::glm", distribution = TRUE, likelihood = TRUE,
    aic = TRUE, bic = TRUE, deviance = TRUE, dispersion_parameter = FALSE,
    bounded_count = TRUE
  ),
  beta_binomial = list(
    id = "beta_binomial", label = "Beta-Binomial",
    domain = "non_negative_integer", links = "logit", default_link = "logit",
    backend = "glmmTMB::glmmTMB", distribution = TRUE, likelihood = TRUE,
    aic = TRUE, bic = TRUE, deviance = TRUE, dispersion_parameter = TRUE,
    bounded_count = TRUE
  ),
  hurdle_beta_binomial_ceiling = list(
    id = "hurdle_beta_binomial_ceiling",
    label = "Hurdle beta-binomial (ceiling)",
    domain = "non_negative_integer", links = "logit", default_link = "logit",
    backend = "gamlss::gamlss + gamlss.dist::ZABB",
    distribution = TRUE, likelihood = TRUE, aic = TRUE, bic = TRUE,
    deviance = TRUE, dispersion_parameter = TRUE, bounded_count = TRUE,
    two_component = TRUE
  ),
  perfect_score = list(
    id = "perfect_score", label = "Perfect score (logistic)",
    domain = "non_negative_integer", links = "logit", default_link = "logit",
    backend = "stats::glm", distribution = TRUE, likelihood = TRUE,
    aic = TRUE, bic = TRUE, deviance = TRUE, dispersion_parameter = FALSE,
    bounded_count = TRUE
  ),
  beta_one_inflated = list(
    id = "beta_one_inflated", label = "One-inflated beta",
    domain = "open_closed_unit_interval", links = "logit", default_link = "logit",
    backend = "gamlss::gamlss", distribution = TRUE, likelihood = TRUE,
    aic = TRUE, bic = TRUE, deviance = TRUE, dispersion_parameter = TRUE
  ),
  quasipoisson = list(
    id = "quasipoisson", label = "Quasi-Poisson",
    domain = "non_negative_integer", links = "log", default_link = "log",
    backend = "stats::glm", distribution = FALSE, likelihood = FALSE,
    aic = FALSE, bic = FALSE, deviance = TRUE, dispersion_parameter = TRUE
  ),
  quasibinomial = list(
    id = "quasibinomial", label = "Quasi-binomial", domain = "binary",
    links = c("logit", "probit", "cloglog"), default_link = "logit",
    backend = "stats::glm", distribution = FALSE, likelihood = FALSE,
    aic = FALSE, bic = FALSE, deviance = TRUE, dispersion_parameter = TRUE
  ),
  gamma_distance = list(
    id = "gamma_distance", label = "Gamma distance to maximum", domain = "legacy",
    links = "log", default_link = "log", backend = "stats::glm",
    distribution = TRUE, likelihood = TRUE, aic = TRUE, bic = TRUE,
    deviance = TRUE, dispersion_parameter = TRUE
  )
)

# Capabilities are part of each statistical specification, not UI policy.
# Keeping them on the registered distribution makes downstream output and
# post-estimation code able to distinguish genuine support from a merely
# available R constructor.
.rls_model_distribution_catalogue <- lapply(
  .rls_model_distribution_catalogue,
  function(specification) {
    specification$supports_multiple_imputation <- TRUE
    specification$supports_emmeans <- TRUE
    specification$supports_contrasts <- TRUE
    specification$supports_predictions <- TRUE
    specification
  }
)

.rls_model_type_catalogue <- list(
  linear = list(
    id = "linear", label = "Linear Model", domain = "continuous",
    distributions = "gaussian", inference_options = character(),
    model_types = "standard"
  ),
  binary = list(
    id = "binary", label = "Binary Model", domain = "binary",
    distributions = "binomial", inference_options = character(),
    model_types = "standard"
  ),
  count = list(
    id = "count", label = "Count Model", domain = "non_negative_integer",
    distributions = c(
      "poisson", "negative_binomial", "binomial_trials", "beta_binomial",
      "hurdle_beta_binomial_ceiling", "perfect_score"
    ),
    inference_options = "quasipoisson",
    model_types = "standard"
  ),
  positive_continuous = list(
    id = "positive_continuous", label = "Positive Continuous Model",
    domain = "continuous",
    distributions = c("gaussian_log", "lognormal", "Gamma", "inverse.gaussian"),
    inference_options = character(), model_types = "standard"
  ),
  proportion = list(
    id = "proportion", label = "Proportion Model",
    domain = "open_closed_unit_interval",
    distributions = c("beta", "beta_one_inflated"),
    inference_options = character(), model_types = "standard"
  ),
  legacy_generalized = list(
    id = "legacy_generalized", label = "Generalized Linear Model", domain = "any",
    distributions = c("gaussian", "binomial", "poisson", "Gamma",
                      "inverse.gaussian", "beta", "beta_one_inflated",
                      "gamma_distance"),
    inference_options = c("quasibinomial", "quasipoisson"),
    model_types = "standard"
  )
)

.rls_model_type_spec <- function(model_type) {
  model_type <- as.character(model_type %||% "legacy_generalized")[[1L]]
  specification <- .rls_model_type_catalogue[[model_type]]
  if (is.null(specification)) {
    stop(sprintf("Unknown LinkEDA model type `%s`.", model_type), call. = FALSE)
  }
  specification
}

.rls_model_window_title <- function(model_type, multiple_imputation = FALSE) {
  title <- .rls_model_type_spec(model_type)$label
  if (isTRUE(multiple_imputation)) paste0(title, " \u2014 Multiple Imputation") else title
}

.rls_model_comparison_title <- function(model_type, multiple_imputation = FALSE) {
  label <- .rls_model_type_spec(model_type)$label
  title <- paste("Compare", sub(" Model$", " Models", label))
  if (isTRUE(multiple_imputation)) paste0(title, " \u2014 Multiple Imputation") else title
}

.rls_model_distribution_spec <- function(distribution) {
  distribution <- as.character(distribution)[[1L]]
  specification <- .rls_model_distribution_catalogue[[distribution]]
  if (is.null(specification)) {
    stop(sprintf("Unknown LinkEDA distribution `%s`.", distribution), call. = FALSE)
  }
  specification
}

.rls_model_type_for_family <- function(family, binary = FALSE, count = FALSE) {
  if (isTRUE(binary)) return("binary")
  if (isTRUE(count)) return("count")
  if (family %in% c("gaussian_log", "lognormal", "Gamma", "inverse.gaussian")) return("positive_continuous")
  if (family %in% c("beta", "beta_one_inflated")) return("proportion")
  if (identical(family, "gaussian")) return("linear")
  if (identical(family, "binomial")) return("binary")
  if (family %in% c(
      "poisson", "quasipoisson", "negative_binomial", "binomial_trials",
      "beta_binomial", "hurdle_beta_binomial_ceiling", "perfect_score")) return("count")
  "legacy_generalized"
}

.rls_validate_model_distribution <- function(model_type, distribution,
                                             allow_inference_option = TRUE) {
  model <- .rls_model_type_spec(model_type)
  allowed <- model$distributions
  if (isTRUE(allow_inference_option)) allowed <- c(allowed, model$inference_options)
  if (!distribution %in% allowed) {
    stop(sprintf(
      "Distribution `%s` is not available for %s. Available distributions: %s.",
      distribution, model$label,
      paste(vapply(model$distributions, function(id) {
        .rls_model_distribution_catalogue[[id]]$label
      }, character(1L)), collapse = ", ")
    ), call. = FALSE)
  }
  .rls_model_distribution_spec(distribution)
}

.rls_validate_response_domain <- function(values, domain, model_label) {
  if (!is.numeric(values)) {
    stop(sprintf("%s requires a numeric response.", model_label), call. = FALSE)
  }
  observed <- values[!is.na(values)]
  if (!length(observed)) {
    stop("The response has no observed values in the analysis scope.", call. = FALSE)
  }
  if (any(!is.finite(observed))) {
    stop("The response must contain finite values in the analysis scope.", call. = FALSE)
  }
  valid <- switch(domain,
    continuous = rep(TRUE, length(observed)),
    non_negative_integer = observed >= 0 &
      abs(observed - round(observed)) <= sqrt(.Machine$double.eps),
    strictly_positive = observed > 0,
    open_unit_interval = observed > 0 & observed < 1,
    open_closed_unit_interval = observed > 0 & observed <= 1,
    rep(TRUE, length(observed))
  )
  if (!all(valid)) {
    requirement <- switch(domain,
      non_negative_integer = "finite, non-negative integers",
      strictly_positive = "values greater than zero",
      open_unit_interval = "values strictly between 0 and 1",
      open_closed_unit_interval = "values greater than 0 and no greater than 1",
      "compatible values"
    )
    stop(sprintf("%s requires %s in the analysis scope.", model_label, requirement),
         call. = FALSE)
  }
  invisible(TRUE)
}
