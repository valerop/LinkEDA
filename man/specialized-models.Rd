\name{ls_new_positive_continuous_model}
\alias{ls_new_positive_continuous_model}
\alias{ls_positive_continuous_model_set_distribution}
\alias{ls_positive_continuous_model_term_tests}
\alias{ls_new_proportion_model}
\alias{ls_proportion_model_set_distribution}
\alias{ls_proportion_model_term_tests}
\title{Response-specific generalized models}
\usage{
ls_new_positive_continuous_model(
  data = NULL, response = NULL, terms = NULL,
  distribution = c("Gamma", "inverse.gaussian", "gaussian_log", "lognormal"), link = NULL,
  scope = "all", name = NULL, native = TRUE, term_types = NULL,
  centered_predictors = NULL, factor_reference_levels = NULL, ...
)

ls_positive_continuous_model_set_distribution(model, distribution, link = NULL)

ls_positive_continuous_model_term_tests(model)

ls_new_proportion_model(
  data = NULL, response = NULL, terms = NULL,
  distribution = c("beta", "beta_one_inflated"), link = NULL,
  scope = "all", name = NULL, native = TRUE, term_types = NULL,
  centered_predictors = NULL, factor_reference_levels = NULL, ...
)

ls_proportion_model_set_distribution(model, distribution, link = NULL)

ls_proportion_model_term_tests(model)
}
\arguments{
  \item{data}{A registered dataset name, data frame, or \code{NULL}.}
  \item{response}{The numeric response variable.}
  \item{terms}{Predictor terms.}
  \item{distribution}{A distribution supported by the response-specific model.}
  \item{link}{A link supported by the selected distribution.}
  \item{scope}{One of \code{"all"}, \code{"selected"}, or \code{"unselected"}.}
  \item{name}{Optional model identifier.}
  \item{native}{Whether to synchronize a native result window.}
  \item{term_types}{Optional model-local predictor type overrides.}
  \item{centered_predictors}{Optional predictors to center for this model.}
  \item{factor_reference_levels}{Optional model-local reference categories for categorical predictors.}
  \item{model}{An existing response-specific model handle.}
  \item{...}{Internal native analysis-scope arguments.}
}
\value{An invisible LinkEDA model handle.}
\description{
Fits Gaussian-log, lognormal, Gamma, or inverse-Gaussian models through the
Positive Continuous workflow, or beta and one-inflated beta models through
the Proportion workflow. Gaussian-log models do not impose observed-response
positivity; lognormal, Gamma, and inverse-Gaussian models do. LinkEDA validates
the selected model's response domain over the effective analysis scope.
}
