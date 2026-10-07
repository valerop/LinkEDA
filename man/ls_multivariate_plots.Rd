\name{ls_scatterplot_matrix}
\alias{ls_scatterplot_matrix}
\alias{ls_parallel_coordinates}
\title{Open linked multivariate plots}
\usage{
ls_scatterplot_matrix(group = NULL, variables = NULL)

ls_parallel_coordinates(group = NULL, variables = NULL,
  standardize = TRUE, connect = TRUE)
}
\arguments{
\item{group}{Registered dataset or dataset group name.}
\item{variables}{Two or more numeric variables; defaults to the first four.}
\item{standardize}{Logical; standardize variables in parallel coordinates.}
\item{connect}{Logical; connect values belonging to the same observation.}
}
\value{A native LinkEDA plot handle.}
\description{Opens a linked scatterplot matrix or parallel-coordinates plot.}
