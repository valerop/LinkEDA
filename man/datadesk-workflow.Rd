\name{ls_variables_window}
\alias{ls_variables_window}
\alias{ls_variable_info}
\alias{ls_set_variable_role}
\alias{ls_model}
\alias{ls_model_info}
\alias{ls_model_set_y}
\alias{ls_model_add_term}
\alias{ls_model_scope}
\alias{ls_model_fit}
\alias{ls_model_open_diagnostics}
\alias{ls_model_open_residuals_fitted}
\alias{ls_model_open_observed_fitted}
\alias{ls_model_remove_term}
\alias{ls_workbench}
\title{DataDesk-style variable and model workflow}
\usage{
ls_variables_window(group = NULL)
ls_variable_info(group = NULL)
ls_set_variable_role(group = NULL, variable, role = c("dependent", "predictor"))
ls_model(group = NULL)
ls_model_info(model)
ls_model_set_y(model, variable)
ls_model_add_term(model, variable)
ls_model_remove_term(model, variable)
ls_model_scope(model, scope = c("all", "selected", "unselected", "compare_selected_all"))
ls_model_fit(model)
ls_model_open_diagnostics(model)
ls_model_open_residuals_fitted(model)
ls_model_open_observed_fitted(model)
ls_workbench(data, group = "data", y = NULL, terms = NULL, labels = NULL,
  color = NULL, openDiagnostics = FALSE,
  scope = c("all", "selected", "unselected", "compare_selected_all"))
}
\arguments{
\item{group}{Linked group name. If omitted, the only active group is used.}
\item{variable}{Variable name.}
\item{role}{Variable role to assign.}
\item{model}{An \code{rlispstat_model} object or group name.}
\item{scope}{Model scope.}
\item{data}{A data frame.}
\item{y}{Optional numeric dependent variable name.}
\item{terms}{Optional predictor variable names.}
\item{labels}{Optional label variable name or one label per row.}
\item{color}{Optional color/grouping variable name.}
\item{openDiagnostics}{Logical. If \code{TRUE}, opens residual diagnostics.}
}
\value{
Window-opening and mutation functions invisibly return \code{TRUE}. Query
functions return data frames or lists.
}
\description{
These functions expose the first DataDesk-style workflow layer in
\pkg{LinkEDA}: a native variable palette and a live group-level model object.
The variable palette supports contextual assignment of dependent variables and
predictors. The model window reads the same group model state.
}
\examples{
\dontrun{
p <- ls_scatter(mtcars, "wt", "mpg", group = "cars")
ls_variables_window("cars")
m <- ls_model("cars")
ls_model_set_y(m, "mpg")
ls_model_add_term(m, "wt")
ls_model_add_term(m, "hp")
ls_model_open_residuals_fitted(m)
ls_model_scope(m, "selected")
ls_model_fit(m)
ls_workbench(mtcars, group = "cars", y = "mpg", terms = c("wt", "hp"))
}
}
