\name{ls_boxplot}
\alias{ls_boxplot}
\alias{ls_new_boxplot}
\title{Open a native linked boxplot}
\usage{
ls_boxplot(data, y, x = NULL, group = NULL, labels = NULL, title = NULL,
  linked = TRUE, show_points = TRUE, show_box = TRUE,
  show_whiskers = TRUE, show_violin = FALSE, connect_rows = FALSE,
  standardize = FALSE)
ls_new_boxplot(group = NULL, y = NULL, x = NULL, labels = NULL,
  title = NULL, linked = TRUE, show_points = TRUE, show_box = TRUE,
  show_whiskers = TRUE, show_violin = FALSE, connect_rows = FALSE,
  standardize = FALSE)
}
\arguments{
\item{data}{A data frame.}
\item{y}{Numeric response variable.}
\item{x}{Optional grouping variable.}
\item{group}{Dataset/group name. If omitted in \code{ls_new_boxplot()}, the active dataset is used.}
\item{labels}{Optional label variable name or vector.}
\item{title}{Optional plot title.}
\item{linked}{Logical; if \code{TRUE}, link by dataset/group id.}
\item{show_points, show_box, show_whiskers}{Initial display options.}
\item{show_violin}{Initial visibility for the native violin density layer.}
\item{connect_rows}{Logical; for ungrouped boxplots with multiple variables, draw lines connecting matching rows.}
\item{standardize}{Logical; for ungrouped boxplots, display each variable as z-scores.}
}
\description{
Creates a native linked boxplot. Points preserve original row ids and share
selection state with other plots in the same group. Ungrouped boxplots start as
a one-variable boxplot and can be extended with parallel variables.
}
\value{An object of class \code{rlispstat_plot}.}
