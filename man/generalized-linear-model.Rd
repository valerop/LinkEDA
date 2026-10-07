\name{ls_new_generalized_linear_model}
\alias{ls_new_generalized_linear_model}
\alias{ls_generalized_linear_model_fit}
\alias{ls_generalized_linear_model_set_family}
\alias{ls_generalized_linear_model_set_link}
\alias{ls_generalized_linear_model_set_response_bounds}
\alias{ls_generalized_linear_model_coefficients}
\alias{ls_generalized_linear_model_coefficient_rows}
\alias{ls_generalized_linear_model_fit_summary}
\alias{ls_generalized_linear_model_diagnostics}
\alias{ls_generalized_linear_model_open_diagnostic}
\alias{ls_generalized_linear_model_state}
\alias{ls_generalized_linear_model_term_tests}
\title{Generalized linear models}
\usage{
ls_new_generalized_linear_model(data = NULL, response = NULL, terms = NULL,
  family = "binomial", link = NULL, scope = "all", name = NULL, native = TRUE,
  term_types = NULL, centered_predictors = NULL,
  factor_reference_levels = NULL, response_bounds = NULL,
  .selected_rows = NULL, .scope_description = NULL,
  .scope_source_kind = NULL, .allow_intercept_only = FALSE,
  .count_regression = FALSE, .count_distribution = "poisson",
  .exposure = NULL, .trials = NULL, .model_type = "legacy_generalized",
  .native_generation = 0L, .comparison_rows = NULL,
  diagnostic_seed = 104729L, offset = NULL)
ls_generalized_linear_model_fit(model)
ls_generalized_linear_model_set_family(model, family, link = NULL)
ls_generalized_linear_model_set_link(model, link)
ls_generalized_linear_model_set_response_bounds(model, lower, upper)
ls_generalized_linear_model_coefficients(model)
ls_generalized_linear_model_coefficient_rows(model)
ls_generalized_linear_model_fit_summary(model)
ls_generalized_linear_model_diagnostics(model)
ls_generalized_linear_model_open_diagnostic(model,
  type = c("observed_vs_fitted", "residuals_vs_fitted", "residual_histogram",
    "normal_qq", "scale_location", "residuals_leverage", "cooks_distance",
    "roc_curve", "calibration_plot", "observed_predicted_distribution",
    "boundary_zero_fit"),
  native = isTRUE(.rls_state$process_started))
ls_generalized_linear_model_state(model)
ls_generalized_linear_model_term_tests(model)
}
\arguments{
\item{diagnostic_seed}{Non-negative integer seed for reproducible randomized quantile diagnostics; does not change the fit or imputations.}
\item{data}{Registered dataset name, a data frame, or \code{NULL} for the active dataset.}
\item{response}{Response variable.}
\item{terms}{Predictor terms. Terms such as \code{factor(cyl)} display hierarchically.}
\item{family}{One of \code{gaussian}, \code{gaussian_log}, \code{lognormal},
\code{binomial}, \code{poisson}, \code{Gamma},
\code{inverse.gaussian}, \code{quasibinomial}, \code{quasipoisson}, \code{beta},
\code{beta_one_inflated}, or \code{gamma_distance}.}
\item{link}{Optional link function. If omitted, the family default is used.}
\item{offset}{Optional numeric column added to the linear predictor with a fixed coefficient of one.}
\item{response_bounds}{Numeric \code{c(lower, upper)} bounds, required by the
bounded-response families.}
\item{lower, upper}{Finite response bounds with \code{lower < upper}.}
\item{scope}{One of \code{"all"}, \code{"selected"}, or \code{"unselected"}.}
\item{name}{Optional model id or dataset name for data-frame input.}
\item{native}{Logical. Native interactive family-window integration is reserved for the workbench window.}
\item{term_types}{Optional named predictor type overrides.}
\item{centered_predictors}{Optional model-local numeric predictors to center.}
\item{factor_reference_levels}{Optional named factor reference categories.}
\item{.selected_rows}{Internal explicit original-row IDs supplied by the native
analysis-scope bridge.}

\item{.scope_description}{Internal immutable analysis-scope description supplied
by the native result-window bridge.}

\item{.scope_source_kind}{Internal analysis-scope source identifier supplied by
the native result-window bridge.}
\item{.allow_intercept_only, .count_regression, .count_distribution,
.exposure, .trials, .model_type, .native_generation, .comparison_rows}{Internal
constructor and native-window state used by the specialized model wrappers.}
\item{model}{A \code{rlispstat_generalized_linear_model} handle.}
\item{type}{Diagnostic type.}
}
\description{
Fits generalized linear models with \code{stats::glm()}, lognormal models with
\code{stats::lm(log(response) ~ ...)}, beta models with
\code{betareg}, and one-inflated beta models with \code{gamlss}. It stores structured
coefficients, categorical parent/category display rows, fit statistics, and diagnostic
data keyed by original row ids. Family/link combinations are validated by R's
family constructors before fitting.
}
\value{
A \code{rlispstat_generalized_linear_model} handle or the requested model
component.
}
\examples{
ls_register_dataset("cars", mtcars)
g <- ls_new_generalized_linear_model(
  data = "cars",
  response = "am",
  terms = c("wt", "hp", "factor(cyl)"),
  family = "binomial",
  link = "logit",
  native = FALSE
)
}
