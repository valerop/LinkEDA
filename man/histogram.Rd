\name{ls_histogram}
\alias{ls_histogram}
\alias{ls_new_histogram}
\alias{ls_histogram_bins}
\alias{ls_histogram_rule}
\alias{ls_histogram_breaks}
\alias{ls_histogram_show_density}
\alias{ls_histogram_density_mode}
\alias{ls_histogram_density_bw}
\alias{ls_histogram_density_adjust}
\alias{ls_histogram_density_info}
\title{Native linked histograms}
\usage{
ls_histogram(data, x, group = NULL, bins = NULL, binwidth = NULL,
  breaks = NULL, title = NULL, linked = TRUE, show_counts = TRUE,
  show_points = FALSE)

ls_new_histogram(group = NULL, x = NULL, bins = NULL, binwidth = NULL,
  breaks = NULL, title = NULL, linked = TRUE, show_counts = TRUE,
  show_points = FALSE)

ls_histogram_bins(plot, bins = NULL)

ls_histogram_rule(plot, rule = c("sturges", "fd", "scott", "sqrt"))

ls_histogram_breaks(plot)

ls_histogram_show_density(plot, show = TRUE)

ls_histogram_density_mode(plot, mode = c("none", "all", "selected",
  "colors", "selected_and_colors"))

ls_histogram_density_bw(plot, bw = NULL)

ls_histogram_density_adjust(plot, adjust = 1)

ls_histogram_density_info(plot)
}
\arguments{
\item{data}{A data frame.}
\item{x}{Name of a numeric column to plot.}
\item{group}{Optional linked brushing group or registered dataset group.}
\item{bins}{Optional number of bins.}
\item{binwidth}{Optional bin width.}
\item{breaks}{Optional explicit break vector.}
\item{title}{Optional plot title.}
\item{linked}{Logical; if \code{TRUE}, share selection with the supplied group.}
\item{show_counts}{Logical; show count labels on bars.}
\item{show_points}{Logical; show the rug/point display when supported.}
\item{plot}{An \code{rlispstat_plot} object or plot id.}
\item{rule}{Automatic binning rule.}
\item{show}{Logical. Show or hide density curves.}
\item{mode}{Density display mode.}
\item{bw}{Density bandwidth. Use \code{NULL} for automatic bandwidth.}
\item{adjust}{Positive density bandwidth adjustment multiplier.}
}
\value{
\code{ls_histogram()} and \code{ls_new_histogram()} return an
\code{rlispstat_plot} object. Setter functions invisibly return \code{TRUE}.
\code{ls_histogram_breaks()} returns the native histogram breaks.
\code{ls_histogram_density_info()} returns a named list of density settings.
}
\description{
Open and configure a native linked histogram. Selecting a bin selects the
original rows represented by that bin, and linked plots and data sheets react to
the same row-level selection and color state. Density curves can be drawn for
all rows, selected rows, explicit row-color groups, or selected rows together
with row-color groups.
}
