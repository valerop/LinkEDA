\name{ls_boxplot_show_points}
\alias{ls_boxplot_show_points}
\alias{ls_boxplot_show_box}
\alias{ls_boxplot_show_whiskers}
\alias{ls_boxplot_show_violin}
\alias{ls_boxplot_connect_rows}
\alias{ls_boxplot_h0_simulation}
\alias{ls_boxplot_clear_h0_simulation}
\alias{ls_boxplot_standardize_variables}
\alias{ls_boxplot_split_violin}
\alias{ls_boxplot_options}
\alias{ls_boxplot_add_variable}
\alias{ls_boxplot_remove_variable}
\title{Set boxplot display options}
\usage{
ls_boxplot_show_points(plot, show = TRUE)
ls_boxplot_show_box(plot, show = TRUE)
ls_boxplot_show_whiskers(plot, show = TRUE)
ls_boxplot_show_violin(plot, show = TRUE)
ls_boxplot_connect_rows(plot, show = TRUE)
ls_boxplot_h0_simulation(plot, h0 = 0,
  alternative = c("two.sided", "less", "greater"), draws = 1000,
  show = TRUE)
ls_boxplot_clear_h0_simulation(plot)
ls_boxplot_standardize_variables(plot, show = TRUE)
ls_boxplot_split_violin(plot, threshold = NULL,
  alternative = c("less", "greater", "two.sided"), pvalue = NULL,
  show = TRUE)
ls_boxplot_add_variable(plot, variable)
ls_boxplot_remove_variable(plot, variable)
ls_boxplot_options(plot)
}
\arguments{
\item{plot}{An \code{rlispstat_plot} object or plot id.}
\item{show}{Logical.}
\item{variable}{Numeric variable name.}
\item{threshold}{Numeric split threshold.}
\item{alternative}{One of \code{"less"}, \code{"greater"}, or \code{"two.sided"}.}
\item{pvalue}{Optional tail probability used to compute threshold quantiles.}
\item{h0}{Null value. For one variable, \eqn{\mu_0}; for two groups, the null mean difference.}
\item{draws}{Number of simulated null draws.}
}
\description{
Show or hide points, boxes, whiskers, violin density layers, split-violin tail highlighting, H0 simulation with p/effect display, matching-row connection lines, and per-variable standardization for a native linked boxplot. Ungrouped boxplots can also add or remove numeric variables to create parallel boxplots.
}
\value{\code{ls_boxplot_options()} returns a list; setters invisibly return \code{TRUE}.}
