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
  rotation = c("none", "varimax", "quartimax", "oblimin", "promax"),
  extraction = c("minres", "ml", "pa"),
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
\item{variables}{Numeric, ordered, or binary variables to include. Unordered variables with more than two observed categories must first be given an explicit order.}
\item{method}{\code{"pca"} for principal components or \code{"factor"} for factor analysis.}
\item{n_components}{Number of retained components/factors in the loading table.}
\item{scale}{Whether variables are standardized before analysis.}
\item{missing}{Missing-data mode. Factor analysis and categorical PCA use listwise or pairwise correlations as selected; scores require complete cases. Numeric PCA uses the selected listwise or pairwise matrix; scores are returned only for complete cases.}
\item{rotation}{Rotation for retained loadings: \code{"none"}, the orthogonal rotations \code{"varimax"} and \code{"quartimax"}, or the oblique rotations \code{"oblimin"} and \code{"promax"}.}
\item{extraction}{Factor-extraction method: minimum residual (\code{"minres"}), maximum likelihood (\code{"ml"}), or principal axis (\code{"pa"}). This argument is used for factor analysis and retained for a later switch from PCA to factor analysis.}
\item{scope}{Rows used by the analysis: \code{"all"}, \code{"selected"}, or \code{"unselected"}.}
\item{selected_rows}{Original one-based row ids used when \code{scope} is \code{"selected"} or \code{"unselected"}.}
\item{parallel}{Whether to calculate and include a 95th percentile parallel-analysis reference. When false, no parallel-analysis simulations are run.}
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
statistical output, scree plots, and biplots. Factor analysis and categorical PCA share the Scale Analysis correlation engine: Pearson, polychoric, or mixed correlations from the R package \code{psych}, selected from the item types.
}

\details{
Saving scores preserves unrelated existing columns with the same name by allocating
a unique suffix. Columns previously saved by the same analysis are updated only
when their ownership is known. Changes to source variables or row identities
require refitting before scores can be saved. These checks include all completed
datasets when using multiple imputation. Adding or removing analysis variables
refits against the current source data while retaining the displayed imputation.
Ordinal category coding is fixed before applying the analysis scope.
}
