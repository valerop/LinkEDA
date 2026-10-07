\name{ls_lm_line}
\alias{ls_lm_line}
\alias{ls_overlays}
\alias{ls_clear_overlays}
\title{Native regression overlays}
\usage{
ls_lm_line(plot, data = c("all", "selected", "both", "color"))
ls_overlays(plot)
ls_clear_overlays(plot, data = c("all", "selected", "both", "color"))
}
\arguments{
\item{plot}{An \code{rlispstat_plot} object or plot id.}
\item{data}{Which data to use: all visible points, selected points, both, or
one line per explicitly assigned point color.}
}
\value{
\code{ls_overlays()} returns a data frame. Other functions invisibly return
\code{TRUE}.
}
\description{
Add, list, and clear native linear regression overlays. Selected-data overlays
recompute from the current linked selection whenever the plot redraws. Color
overlays fit one line for each explicitly assigned point color.
}
