\name{ls_new_dimensionality}
\alias{ls_new_dimensionality}
\alias{ls_dimensionality_state}
\alias{ls_dimensionality_eigenvalues}
\alias{ls_dimensionality_loadings}
\alias{ls_dimensionality_scores}
\alias{ls_dimensionality_save_scores}
\alias{ls_dimensionality_add_variable}
\alias{ls_dimensionality_remove_variable}
\title{Principal components and factor analysis}
\usage{
ls_new_dimensionality(data = NULL, variables = NULL,
  method = c("pca", "factor"), n_components = NULL,
  scale = TRUE, missing = c("listwise", "pairwise"),
  rotation = c("none", "varimax", "quartimax"),
  scope = c("all", "selected", "unselected"),
  selected_rows = NULL,
  parallel = TRUE, parallel_iterations = 100L,
  name = NULL, native = TRUE)

ls_dimensionality_state(model)

ls_dimensionality_eigenvalues(model)

ls_dimensionality_loadings(model)

ls_dimensionality_scores(model)

ls_dimensionality_save_scores(model, prefix = NULL, components = NULL,
  dataset = NULL)

ls_dimensionality_add_variable(model, variable)

ls_dimensionality_remove_variable(model, variable)
}
\arguments{
\item{data}{Registered dataset name, data frame, or \code{NULL} for the active dataset.}
\item{variables}{Numeric variables to include.}
\item{method}{\code{"pca"} for principal components or \code{"factor"} for factor analysis.}
\item{n_components}{Number of retained components/factors in the loading table.}
\item{scale}{Whether variables are standardized before analysis.}
\item{missing}{Missing-data mode. The native decomposition uses complete rows.}
\item{rotation}{Orthogonal rotation for retained loadings: \code{"none"}, \code{"varimax"}, or \code{"quartimax"}.}
\item{scope}{Rows used by the analysis: \code{"all"}, \code{"selected"}, or \code{"unselected"}.}
\item{selected_rows}{Original one-based row ids used when \code{scope} is \code{"selected"} or \code{"unselected"}.}
\item{parallel}{Whether to include a 95th percentile parallel-analysis reference.}
\item{parallel_iterations}{Number of simulated datasets for parallel analysis.}
\item{name}{Optional model id.}
\item{native}{Whether to open the native window.}
\item{model}{A dimensionality model handle or id.}
\item{prefix}{Prefix for saved score columns.}
\item{components}{Component/factor score columns to save.}
\item{dataset}{Target registered dataset. Defaults to the model dataset.}
\item{variable}{Variable to add or remove.}
}
\value{
A \code{rlispstat_dimensionality} handle, state list, or a result table.
}
\description{
Creates a native principal components/factor analysis module with compact
statistical output, scree plots, and biplots.
}
