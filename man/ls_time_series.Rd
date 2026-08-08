\name{ls_time_series}
\alias{ls_time_series}
\alias{ls_new_time_series}
\alias{ls_time_series_group}
\alias{ls_time_series_identification}
\title{Native interactive time-series plots}
\usage{
ls_time_series(data, time, value, series = NULL, group = NULL, title = NULL,
  identification = c("legend", "start_labels", "none"),
  legend_position = c("top_right", "top_left", "bottom_right", "bottom_left"))

ls_new_time_series(group = NULL, time = NULL, value = NULL,
  series = NULL, title = NULL, linked = TRUE,
  identification = c("legend", "start_labels", "none"),
  legend_position = c("top_right", "top_left", "bottom_right", "bottom_left"))

ls_time_series_group(plot, series = NULL)

ls_time_series_identification(plot,
  identification = c("legend", "start_labels", "none"),
  legend_position = NULL)
}
\arguments{
  \item{data}{A data frame.}
  \item{time}{A numeric, Date, or POSIXt time column name.}
  \item{value}{A numeric value column name.}
  \item{series}{Optional column identifying separate lines.}
  \item{group}{Dataset/group id for linked selection.}
  \item{title}{Optional plot title.}
  \item{identification}{How grouped series are identified: with a legend,
  labels at the start of each series, or no identification.}
  \item{legend_position}{Position of the legend inside the plotting area.}
  \item{linked}{Whether to link selections to the registered dataset.}
  \item{plot}{A time-series plot or plot id.}
}
\value{A native \code{rlispstat_plot}; the grouping setter returns it invisibly.}
\description{
Draws values in time order. When a series variable is supplied, one line is
drawn for each distinct value while observations remain linked to other plots.
}
