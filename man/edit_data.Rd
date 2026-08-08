\name{edit_data}
\alias{edit_data}
\alias{choose_data}
\title{Edit R data in LinkEDA and return it to the calling session}
\usage{
edit_data(x, name = NULL, cancel = c("original", "null"))

choose_data(envir = .GlobalEnv, cancel = c("original", "null"))
}
\arguments{
  \item{x}{A data frame, tibble, or tribble.}
  \item{name}{Optional dataset name shown in LinkEDA.}
  \item{cancel}{Value returned when editing is cancelled: the original object
  or \code{NULL}.}
  \item{envir}{Environment from which data frames are offered.}
}
\value{The edited data frame, selected rows, the original object, or
\code{NULL}, according to the chosen action.}
\description{
\code{edit_data()} opens a linked editable data sheet and waits for
\emph{Return Data to R}. \code{choose_data()} first displays the data frames
owned by the calling R environment. Neither function overwrites an R object
automatically.
}
