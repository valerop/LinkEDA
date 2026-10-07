\name{LinkEDA}
\alias{LinkEDA}
\alias{rlispstat}
\title{Open LinkEDA}
\usage{
LinkEDA(data = NULL, name = NULL)

rlispstat(data = NULL, name = NULL)
}
\arguments{
\item{data}{Optional data frame, tibble, tribble, or a single path to a supported data file.}
\item{name}{Optional name for the data inside LinkEDA.}
}
\value{
Invisibly returns the registered dataset name, or \code{NULL} when no data
were supplied.
}
\description{
Starts the native LinkEDA application directly from R. When data are
supplied they are registered, made active, and opened in a linked data sheet.
Without data the native Welcome Window opens and offers the connected R
session, file import, recent files, and example datasets.

\code{rlispstat()} is retained as a deprecated compatibility alias for
\code{LinkEDA()}.
}
