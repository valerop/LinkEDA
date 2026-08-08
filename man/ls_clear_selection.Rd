\name{ls_clear_selection}
\alias{ls_clear_selection}
\title{Clear selected points}
\usage{
ls_clear_selection(group = NULL)
}
\arguments{
\item{group}{A non-empty group name. If \code{NULL}, the only active group is used.}
}
\value{
Invisibly returns \code{TRUE}.
}
\description{
Clears the current selection in a linked brushing group and redraws any matching
native plots.
}
