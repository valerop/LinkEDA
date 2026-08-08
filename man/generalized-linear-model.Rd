\name{ls_new_generalized_linear_model}
\alias{ls_new_generalized_linear_model}
\alias{ls_generalized_linear_model_fit}
\alias{ls_generalized_linear_model_set_family}
\alias{ls_generalized_linear_model_set_link}
\alias{ls_generalized_linear_model_coefficients}
\alias{ls_generalized_linear_model_coefficient_rows}
\alias{ls_generalized_linear_model_fit_summary}
\alias{ls_generalized_linear_model_diagnostics}
\alias{ls_generalized_linear_model_open_diagnostic}
\alias{ls_generalized_linear_model_state}
\title{Generalized linear models}
\usage{
ls_new_generalized_linear_model(data = NULL, response = NULL, terms = NULL,
  family = "binomial", link = NULL, scope = "all", name = NULL, native = TRUE,
  term_types = NULL, .selected_rows = NULL, .scope_description = NULL,
  .scope_source_kind = NULL)
ls_generalized_linear_model_fit(model)
ls_generalized_linear_model_set_family(model, family, link = NULL)
ls_generalized_linear_model_set_link(model, link)
ls_generalized_linear_model_coefficients(model)
ls_generalized_linear_model_coefficient_rows(model)
ls_generalized_linear_model_fit_summary(model)
ls_generalized_linear_model_diagnostics(model)
ls_generalized_linear_model_open_diagnostic(model,
  type = c("observed_vs_fitted", "residuals_vs_fitted", "residual_histogram"),
  native = FALSE)
ls_generalized_linear_model_state(model)
}
\arguments{
\item{data}{Registered dataset name, a data frame, or \code{NULL} for the active dataset.}
\item{response}{Response variable.}
\item{terms}{Predictor terms. Terms such as \code{factor(cyl)} display hierarchically.}
\item{family}{One of \code{gaussian}, \code{binomial}, \code{poisson}, \code{Gamma},
\code{inverse.gaussian}, \code{quasibinomial}, or \code{quasipoisson}.}
\item{link}{Optional link function. If omitted, the family default is used.}
\item{scope}{One of \code{"all"}, \code{"selected"}, or \code{"unselected"}.}
\item{name}{Optional model id or dataset name for data-frame input.}
\item{native}{Logical. Native interactive family-window integration is reserved for the workbench window.}
\item{term_types}{Optional named predictor type overrides.}
\item{.selected_rows}{Internal explicit original-row IDs supplied by the native
analysis-scope bridge.}

\item{.scope_description}{Internal immutable analysis-scope description supplied
by the native result-window bridge.}

\item{.scope_source_kind}{Internal analysis-scope source identifier supplied by
the native result-window bridge.}
\item{model}{A \code{rlispstat_generalized_linear_model} handle.}
\item{type}{Diagnostic type.}
}
\description{
Fits generalized linear models with \code{stats::glm()} and stores structured
coefficients, factor parent/level display rows, fit statistics, and diagnostic
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
