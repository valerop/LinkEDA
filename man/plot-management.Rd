\name{ls_plots}
\alias{ls_plots}
\alias{ls_plot_info}
\alias{ls_redraw}
\alias{ls_close}
\alias{ls_selection_mode}
\alias{ls_mode}
\alias{ls_set_mode}
\alias{ls_selection_operation}
\alias{ls_set_selection_operation}
\alias{ls_panel}
\title{Manage live LinkEDA plots}
\usage{
ls_plots()
ls_plot_info(plot)
ls_redraw(plot)
ls_close(plot)
ls_selection_mode(plot, mode = c("replace", "add", "subtract", "toggle"))
ls_mode(plot)
ls_set_mode(plot, mode = c("none", "select", "brush", "identify", "pan", "zoom", "label"))
ls_selection_operation(plot)
ls_set_selection_operation(plot, operation = c("replace", "add", "subtract", "toggle"))
ls_panel(show = TRUE)
}
\arguments{
\item{plot}{An \code{rlispstat_plot} object or plot id.}
\item{mode}{Selection mode: replace, add, subtract, or toggle.}
\item{operation}{Selection operation: replace, add, subtract, or toggle.}
\item{show}{Logical. Show or hide the floating native tool panel.}
}
\value{
\code{ls_plots()} returns a data frame. \code{ls_plot_info()} returns a list.
Other functions invisibly return \code{TRUE} or the selected mode.
}
\description{
List, inspect, redraw, close, and configure live native scatterplot objects.
}
