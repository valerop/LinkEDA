\name{ls_palette}
\alias{ls_palette}
\alias{ls_set_selected_color}
\alias{ls_get_selected_color}
\alias{ls_set_point_color}
\title{Selected-point color palette}
\usage{
ls_palette(show = TRUE)
ls_set_selected_color(group = NULL, color)
ls_get_selected_color(group = NULL)
ls_set_point_color(plot, rows, color)
}
\arguments{
\item{show}{Logical. Show or hide the floating palette.}
\item{group}{A non-empty group name. If \code{NULL}, the only active group is used.}
\item{color}{One of \code{orange}, \code{blue}, \code{green},
\code{vermillion}, \code{purple}, \code{brown}, \code{pink}, or
\code{yellow}. Use the
Default/Reset color control to remove explicit row color and return to black
points with white data-sheet rows.}
\item{plot}{An \code{rlispstat_plot} object or plot id.}
\item{rows}{Original R row indices.}
}
\value{
Getter functions return a color name. Other functions invisibly return
\code{TRUE}.
}
\description{
Show a native floating palette and recolor selected or specified original rows
across linked plots.
}
