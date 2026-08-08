\name{ls_set_selected}
\alias{ls_set_selected}
\alias{ls_invert_selection}
\alias{ls_select_all}
\alias{ls_select_where}
\title{Set and transform linked selections}
\usage{
ls_set_selected(group = NULL, rows)
ls_invert_selection(group = NULL)
ls_select_all(group = NULL)
ls_select_where(group = NULL, expr)
}
\arguments{
\item{group}{A non-empty group name. If \code{NULL}, the only active group is used.}
\item{rows}{Integer row indices in the original R data frame.}
\item{expr}{An expression evaluated in the group's data frame.}
}
\value{
Invisibly returns \code{TRUE}.
}
\description{
Set, invert, select all, or select by expression using original R row indices.
Selections are immediately reflected in linked native plots.
}
