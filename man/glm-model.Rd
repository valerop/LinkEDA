\name{ls_new_glm}
\alias{ls_new_glm}
\alias{ls_glm_window}
\alias{ls_glm_set_dependent}
\alias{ls_glm_get_dependent}
\alias{ls_glm_add_predictor}
\alias{ls_glm_add_interaction}
\alias{ls_glm_add_polynomial}
\alias{ls_glm_remove_predictor}
\alias{ls_glm_predictors}
\alias{ls_glm_terms}
\alias{ls_glm_formula}
\alias{ls_glm_fit}
\alias{ls_glm_coefficients}
\alias{ls_glm_coefficient_rows}
\alias{ls_glm_fit_summary}
\alias{ls_glm_diagnostics}
\alias{ls_glm_refresh_diagnostics}
\alias{ls_glm_open_diagnostic}
\alias{ls_glm_diagnostic_state}
\alias{ls_glm_partial_r2}
\alias{ls_glm_rows_used}
\alias{ls_glm_rows_excluded}
\title{General Linear Model workbench}
\usage{
ls_new_glm(group = NULL)

ls_glm_window(group = NULL)

ls_glm_set_dependent(model, variable)

ls_glm_get_dependent(model)

ls_glm_add_predictor(model, variable)

ls_glm_add_interaction(model, var1, var2)

ls_glm_add_polynomial(model, variable, degree = 2)

ls_glm_remove_predictor(model, variable)

ls_glm_predictors(model)

ls_glm_terms(model)

ls_glm_formula(model)

ls_glm_fit(model)

ls_glm_coefficients(model)

ls_glm_coefficient_rows(model)

ls_glm_fit_summary(model)

ls_glm_diagnostics(model)

ls_glm_refresh_diagnostics(model)

ls_glm_open_diagnostic(model, type = c("residuals_vs_fitted", "observed_vs_fitted",
  "residuals_fitted", "observed_fitted"), native = isTRUE(.rls_state$process_started))

ls_glm_diagnostic_state(plot)

ls_glm_partial_r2(model)

ls_glm_rows_used(model)

ls_glm_rows_excluded(model)
}
\arguments{
\item{group}{Registered dataset/group name. If \code{NULL}, the active dataset is used.}
\item{model}{An \code{rlispstat_glm} object or dataset/group name.}
\item{variable}{A variable name. Predictors may also use terms such as \code{factor(cyl)}.}
\item{var1, var2}{Numeric variable names used to define an interaction term.}
\item{degree}{Polynomial degree.}
\item{type}{Diagnostic plot type.}
\item{native}{Logical. If the native backend is active, open a visible native diagnostic plot.}
\item{plot}{An \code{rlispstat_glm_diagnostic} handle.}
}
\value{
\code{ls_new_glm()} and \code{ls_glm_window()} return an
\code{rlispstat_glm} handle. Other functions return model components or
invisibly return an updated handle.
}
\description{
Create, configure, fit, and inspect ordinary \code{lm()}-style General Linear
Models for registered datasets. The graphical window is native and linked to
the active LinkEDA workbench group; the R API stores structured model results
including coefficients, fit statistics, p values, and partial R-squared.
Factor predictors are exposed in display rows as parent variables with
indented level rows, including the reference category.
}
