\name{ls_variables}
\alias{ls_variables}
\alias{ls_numeric_variables}
\alias{ls_get_xvar}
\alias{ls_get_yvar}
\alias{ls_set_xvar}
\alias{ls_set_yvar}
\title{Inspect and change scatterplot axis variables}
\usage{
ls_variables(plot)
ls_numeric_variables(plot)
ls_get_xvar(plot)
ls_get_yvar(plot)
ls_set_xvar(plot, var)
ls_set_yvar(plot, var)
}
\arguments{
\item{plot}{An \code{rlispstat_plot} object or plot id.}
\item{var}{Name of a numeric variable available in the plot data.}
}
\value{
Variable-list and getter functions return character vectors. Setter functions
invisibly return the plot object or id.
}
\description{
List variables available to a live plot and change the X or Y axis variable.
Changing variables recomputes visible points and preserves original row indices
and linked group selection state.
}
