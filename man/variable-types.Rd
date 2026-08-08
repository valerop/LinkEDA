\name{ls_variable_metadata}
\alias{ls_variable_metadata}
\alias{ls_set_variable_type}
\title{Inspect or change variable analysis types}
\usage{
ls_variable_metadata(data = NULL, variable = NULL)

ls_set_variable_type(
  data = NULL,
  variable,
  type = c("numeric", "factor", "character", "logical")
)
}
\arguments{
\item{data}{Registered dataset name, or \code{NULL} for the active dataset.}
\item{variable}{Variable name. For \code{ls_variable_metadata()}, \code{NULL} returns all variables.}
\item{type}{Analysis type to apply.}
}
\value{
\code{ls_variable_metadata()} returns a metadata data frame. \code{ls_set_variable_type()} invisibly returns the updated variable metadata.
}
\description{
These helpers maintain LinkEDA's explicit analysis-type layer. A variable can be treated as numeric, factor, character, or logical for modelling and numeric-only analyses without relying only on its current R class.
}
\details{
Changing a type updates the registered dataset metadata, synchronizes the native backend when it is running, and marks affected models or numeric-only analyses for refresh. Numeric conversion is checked and fails with an explicit error if non-missing values cannot be converted.
}
