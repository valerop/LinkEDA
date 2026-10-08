\name{ls_new_model_trellis}
\alias{ls_new_model_trellis}
\title{Open a linear model trellis}
\usage{
ls_new_model_trellis(group = NULL, response, terms = character(),
  columns, rows = NULL,
  p_adjustment = c("holm", "bonferroni", "none"))
}
\arguments{
\item{group}{Registered dataset or dataset group name.}
\item{response}{Numeric response variable.}
\item{terms}{Optional model terms used in every panel.}
\item{columns}{Categorical conditioning variable displayed in columns.}
\item{rows}{Optional categorical conditioning variable displayed in rows.}
\item{p_adjustment}{Multiplicity adjustment across panels.}
}
\value{A native model-trellis handle.}
\description{Fits a common linear-model specification independently in categorical panels and displays the R results in a native window.}
