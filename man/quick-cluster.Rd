\name{ls_new_quick_cluster}
\alias{ls_new_quick_cluster}
\alias{ls_dendrogram_state}
\alias{ls_dendrogram_hclust}
\alias{ls_dendrogram_add_variable}
\alias{ls_dendrogram_remove_variable}
\title{Quick cluster cases using selected numeric variables}
\usage{
ls_new_quick_cluster(data = NULL, variables = NULL,
  distance = "euclidean", linkage = c("average", "complete", "single"),
  missing = c("pairwise", "listwise"), name = NULL, native = TRUE)

ls_dendrogram_state(dendrogram)

ls_dendrogram_hclust(dendrogram)

ls_dendrogram_add_variable(dendrogram, variable)

ls_dendrogram_remove_variable(dendrogram, variable)
}
\arguments{
\item{data}{Registered dataset name, a data frame, or \code{NULL} for the active dataset.}
\item{variables}{Numeric variables to include. If \code{NULL}, all numeric variables are used.}
\item{distance}{Distance metric. Currently only \code{"euclidean"} is supported (\code{"correlation"} is accepted as a legacy alias).}
\item{linkage}{Hierarchical linkage method: \code{"average"}, \code{"complete"}, or \code{"single"}.}
\item{missing}{Missing-data mode: \code{"pairwise"} or \code{"listwise"}.}
\item{name}{Optional dendrogram id or dataset name for data-frame input.}
\item{native}{Logical. Open the native dendrogram window when possible.}
\item{dendrogram}{A dendrogram handle returned by \code{ls_new_quick_cluster()}.}
\item{variable}{A numeric variable name.}
}
\value{
A dendrogram handle, state list, \code{hclust} object, or updated handle depending on the function.
}
\description{
Builds a quick hierarchical clustering over cases (rows) using selected numeric
variables and stores the result as a dendrogram object. The native window
renders a dendrogram and lets you switch linkage, missing-data handling, and the
included variables interactively.
}
\examples{
\dontrun{
ls_register_dataset("cars", mtcars)
den <- ls_new_quick_cluster("cars", variables = c("mpg", "wt", "hp"))
ls_dendrogram_hclust(den)
}
}
