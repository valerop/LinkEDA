\name{ls_new_regression_comparison}
\alias{ls_new_regression_comparison}
\alias{ls_regression_comparison_add_model}
\alias{ls_regression_comparison_add_term}
\alias{ls_regression_comparison_set_term}
\alias{ls_regression_comparison_open_diagnostic}
\alias{ls_regression_comparison_state}
\title{Native regression model comparison}
\usage{
ls_new_regression_comparison(data = NULL, response = NULL, models = NULL,
  scope = "all", name = NULL, native = TRUE)
ls_regression_comparison_add_model(comparison, terms = NULL, label = NULL,
  response = NULL)
ls_regression_comparison_add_term(comparison, term)
ls_regression_comparison_set_term(comparison, model, term, included = TRUE)
ls_regression_comparison_open_diagnostic(comparison, model,
  type = c("observed_vs_fitted", "residuals_vs_fitted"),
  native = isTRUE(.rls_state$process_started))
ls_regression_comparison_state(comparison)
}
\arguments{
\item{data}{Registered dataset name, a data frame, or \code{NULL} for the active dataset.}
\item{response}{Optional shared numeric response variable. For
\code{ls_regression_comparison_add_model()}, this may override the response
for the new model column.}
\item{models}{Named list of predictor vectors, or a list of model specs with
\code{response} and \code{terms} entries.}
\item{scope}{One of \code{"all"}, \code{"selected"}, or \code{"unselected"}.}
\item{name}{Optional comparison id or dataset name for data-frame input.}
\item{native}{Whether to open/synchronize the native comparison window.}
\item{comparison}{A regression-comparison handle.}
\item{terms}{Predictor vector for a model column.}
\item{label}{Optional model-column label.}
\item{term}{Predictor term to add or toggle.}
\item{model}{Model index, label, or model id.}
\item{included}{Whether the term is included in the selected model.}
\item{type}{Diagnostic plot type.}
}
\description{
Creates a native, side-by-side ordinary linear regression model comparison
window. Rows are aligned terms, columns are alternative models, coefficient
cells show unstandardized coefficients with significance stars, and global fit
statistics are shown below the coefficient block.
The old shared-response syntax is still supported. Model specs may also carry
their own \code{response}, in which case each model column is fitted and
diagnosed against its own dependent variable.
}
\value{
A \code{rlispstat_regression_comparison} handle, invisibly for mutating
functions. \code{ls_regression_comparison_state()} returns the structured
R-side comparison state.
}
\examples{
ls_register_dataset("cars", mtcars)
cmp <- ls_new_regression_comparison(
  data = "cars",
  response = "mpg",
  models = list(Base = "wt", Extended = c("wt", "hp"), Full = c("wt", "hp", "am")),
  native = FALSE
)

cmp2 <- ls_new_regression_comparison(
  data = "cars",
  models = list(
    Speed = list(response = "qsec", terms = c("wt", "hp")),
    Economy = list(response = "mpg", terms = c("wt", "hp", "am"))
  ),
  native = FALSE
)
}
