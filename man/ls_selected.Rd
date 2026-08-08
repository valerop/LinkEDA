\name{ls_selected}
\alias{ls_selected}
\title{Return selected row indices}
\usage{
ls_selected(group = NULL)
}
\arguments{
\item{group}{A non-empty group name. If \code{NULL}, the only active group is used.}
}
\value{
An integer vector of selected row indices.
}
\description{
Queries the native backend for the selected original row indices in a linked
brushing group.
}
