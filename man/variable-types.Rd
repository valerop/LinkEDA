\name{ls_variable_metadata}
\alias{ls_variable_metadata}
\alias{ls_set_variable_type}
\title{Inspect or change variable analysis types}
\usage{
ls_variable_metadata(data = NULL, variable = NULL)

ls_set_variable_type(
  data = NULL,
  variable,
  type = c("numeric", "factor", "ordered", "character"),
  mapping = NULL,
  invert_binary = FALSE,
  category_order = NULL,
  ordinal_scores = NULL,
  breaks = NULL,
  labels = NULL
)
}
\arguments{
\item{data}{Registered dataset name, or \code{NULL} for the active dataset.}
\item{variable}{Variable name. For \code{ls_variable_metadata()}, \code{NULL} returns all variables.}
\item{type}{Semantic analysis type to apply: Numeric, Categorical, Ordinal, or Text. The legacy value \code{logical} remains accepted for storage compatibility and is treated as binary Categorical.}
\item{mapping}{Optional named numeric vector assigning a score to every category. Required when a non-binary nominal Categorical variable has non-numeric labels and is converted to Numeric.}
\item{invert_binary}{Logical; invert the default 0/1 assignment when converting a binary Categorical variable to Numeric.}
\item{category_order}{Optional character vector containing every category exactly once in the desired order.}
\item{ordinal_scores}{Optional named numeric scores used when converting an Ordinal variable to Numeric.}
\item{breaks}{Optional strictly increasing cut points for converting a Numeric variable to Ordinal. Infinite end points are allowed.}
\item{labels}{Optional category labels, one per interval defined by \code{breaks}.}
}
\value{
\code{ls_variable_metadata()} returns a metadata data frame. \code{ls_set_variable_type()} invisibly returns the updated variable metadata.
}
\description{
These helpers maintain LinkEDA's explicit semantic-type layer. The statistical types are \code{numeric}, \code{factor}, \code{ordered}, and \code{character}, displayed as Numeric, Categorical, Ordinal, and Text. R logical storage is represented statistically as binary Categorical; \code{logical} is accepted only as a deprecated compatibility input and is converted to Categorical.
}
\details{
Changing a type updates the registered dataset metadata, synchronizes the native backend when it is running, and marks affected analyses for refresh. Categorical-to-Numeric conversion uses numeric labels or an explicit visible mapping, never the internal integer codes of an R factor. Binary Categorical variables use a reported 0/1 mapping which can be inverted. Ordinal variables may use saved scores, consecutive scores, or cut points. The same conversion is applied to every completed dataset in a multiple-imputation object.
}
