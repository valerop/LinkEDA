\name{ls_trellis_scatterplot}
\alias{ls_trellis_scatterplot}
\alias{ls_new_trellis_scatterplot}
\alias{ls_trellis_scatterplot_x}
\alias{ls_trellis_scatterplot_y}
\alias{ls_trellis_scatterplot_condition}
\alias{ls_trellis_scatterplot_display}
\alias{ls_trellis_plot_type}
\alias{ls_trellis_condition_add}
\alias{ls_trellis_condition_remove}
\alias{ls_trellis_condition_move}
\alias{ls_trellis_condition_dimension}
\alias{ls_trellis_swap_dimensions}
\title{Interactive trellis plots}
\usage{
ls_trellis_scatterplot(data, x, y, by, group = NULL,
  labels = NULL, title = NULL,
  layout = c("automatic", "one_row", "one_column", "grid", "two_columns", "three_columns"),
  panel_order = c("defined", "ascending", "descending"))

ls_new_trellis_scatterplot(group = NULL, x = NULL, y = NULL, by = NULL,
  labels = NULL, title = NULL, linked = TRUE,
  layout = c("automatic", "one_row", "one_column", "grid", "two_columns", "three_columns"),
  panel_order = c("defined", "ascending", "descending"))

ls_trellis_scatterplot_x(plot, variable)

ls_trellis_scatterplot_y(plot, variable)

ls_trellis_scatterplot_condition(plot, variable)

ls_trellis_scatterplot_display(plot, layout = NULL, panel_order = NULL)

ls_trellis_plot_type(plot, type, x = NULL, y = NULL)

ls_trellis_condition_add(plot, variable,
  kind = c("categorical", "continuous"),
  method = c("equal_width", "equal_count"), bins = 4L)

ls_trellis_condition_remove(plot, variable)

ls_trellis_condition_move(plot, variable, direction = c("earlier", "later"))

ls_trellis_condition_dimension(plot, variable,
  dimension = c("rows", "columns", "nested"))

ls_trellis_swap_dimensions(plot)
}
\arguments{
  \item{data}{A data frame.}
  \item{x, y}{Variable names. Scatterplots require two different numeric variables;
  boxplots and bar charts may use a categorical X variable.}
  \item{by}{A required factor-like or discrete numeric conditioning column.}
  \item{group}{Optional linked-selection group or registered dataset.}
  \item{labels}{Optional vector with one case label per row.}
  \item{title}{Optional plot title.}
  \item{layout}{Initial or replacement panel layout preference.}
  \item{panel_order}{Visual ordering of conditioning levels.}
  \item{linked}{Whether to share the registered dataset selection group.}
  \item{plot}{A trellis scatterplot object or plot id.}
  \item{variable}{A replacement column name.}
  \item{type}{Panel renderer: \code{scatter}, \code{boxplot}, \code{bar}, or \code{histogram}.}
  \item{kind}{Whether a conditioning variable is categorical or continuous.}
  \item{method}{Equal-width or equal-count binning for a continuous conditioning variable.}
  \item{bins}{Number of continuous intervals, from 2 through 6.}
  \item{direction}{Direction in which to move a conditioning variable.}
  \item{dimension}{Panel dimension: rows, columns, or nested strips.}
}
\description{
Creates linked conditioned scatterplots and supports changing the panel
renderer to boxplots, bar charts, or histograms. Multiple categorical or
binned continuous conditioning variables share one layout and retain original
case identifiers.
}
\value{A \code{rlispstat_plot}; setters return it invisibly.}
