\name{ls_new_scatterplot}
\alias{ls_new_scatterplot}
\title{Create a scatterplot from a registered dataset}
\usage{
ls_new_scatterplot(group = NULL, x = NULL, y = NULL, labels = NULL,
  title = NULL, linked = TRUE)
}
\arguments{
\item{group}{Dataset/group name. If omitted, the active dataset is used.}
\item{x, y}{Numeric variable names.}
\item{labels}{Optional label variable name or vector.}
\item{title}{Optional plot title.}
\item{linked}{Logical; if \code{TRUE}, use the dataset group for linked brushing.}
}
\description{
Creates a native scatterplot from the active or named registered dataset while
preserving original row ids and linked brushing group identity.
}
\value{
An object of class \code{rlispstat_plot}.
}
