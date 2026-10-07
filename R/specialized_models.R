#' Positive continuous model
#'
#' Fits Gaussian-log, lognormal, Gamma, or inverse-Gaussian regression. The
#' individual distribution specification determines the response domain.
#'
#' @param data Registered dataset name, data frame, or `NULL` for the active dataset.
#' @param response Numeric response. Lognormal, Gamma, and inverse-Gaussian
#'   models require strictly positive values; Gaussian-log does not.
#' @param terms Predictor terms.
#' @param distribution One of `"gaussian_log"`, `"lognormal"`, `"Gamma"`, or
#'   `"inverse.gaussian"`.
#' @param link A link supported by the selected distribution.
#' @param scope One of `"all"`, `"selected"`, or `"unselected"`.
#' @param name Optional model identifier.
#' @param native Open/synchronize the native model window.
#' @param term_types,centered_predictors,factor_reference_levels Model-local options.
#' @param ... Internal native scope arguments.
#' @return A `rlispstat_positive_continuous_model` handle.
#' @export
ls_new_positive_continuous_model <- function(
    data = NULL, response = NULL, terms = NULL,
    distribution = c("Gamma", "inverse.gaussian", "gaussian_log", "lognormal"), link = NULL,
    scope = "all", name = NULL, native = TRUE, term_types = NULL,
    centered_predictors = NULL, factor_reference_levels = NULL, ...) {
  distribution <- match.arg(distribution)
  ls_new_generalized_linear_model(
    data = data, response = response, terms = terms,
    family = distribution, link = link, scope = scope, name = name,
    native = native, term_types = term_types,
    centered_predictors = centered_predictors,
    factor_reference_levels = factor_reference_levels,
    .model_type = "positive_continuous", ...
  )
}

#' @rdname ls_new_positive_continuous_model
#' @export
ls_positive_continuous_model_set_distribution <- function(model, distribution,
                                                          link = NULL) {
  distribution <- match.arg(distribution, c("gaussian_log", "lognormal", "Gamma", "inverse.gaussian"))
  ls_generalized_linear_model_set_family(model, distribution, link)
}

#' @rdname ls_new_positive_continuous_model
#' @export
ls_positive_continuous_model_term_tests <- function(model) {
  ls_generalized_linear_model_term_tests(model)
}

#' Proportion model
#'
#' Fits beta or one-inflated beta models to responses on the 0--1 scale.
#' Standard beta requires `0 < y < 1`; one-inflated beta permits `y = 1` but
#' still requires `y > 0`.
#'
#' @inheritParams ls_new_positive_continuous_model
#' @param distribution Either `"beta"` or `"beta_one_inflated"`.
#' @return A `rlispstat_proportion_model` handle.
#' @export
ls_new_proportion_model <- function(
    data = NULL, response = NULL, terms = NULL,
    distribution = c("beta", "beta_one_inflated"), link = NULL,
    scope = "all", name = NULL, native = TRUE, term_types = NULL,
    centered_predictors = NULL, factor_reference_levels = NULL, ...) {
  distribution <- match.arg(distribution)
  ls_new_generalized_linear_model(
    data = data, response = response, terms = terms,
    family = distribution, link = link, response_bounds = c(0, 1),
    scope = scope, name = name, native = native, term_types = term_types,
    centered_predictors = centered_predictors,
    factor_reference_levels = factor_reference_levels,
    .model_type = "proportion", ...
  )
}

#' @rdname ls_new_proportion_model
#' @export
ls_proportion_model_set_distribution <- function(model, distribution,
                                                 link = NULL) {
  distribution <- match.arg(distribution, c("beta", "beta_one_inflated"))
  ls_generalized_linear_model_set_family(model, distribution, link)
}


#' @rdname ls_new_proportion_model
#' @export
ls_proportion_model_term_tests <- function(model) {
  ls_generalized_linear_model_term_tests(model)
}
