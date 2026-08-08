\name{ls_set_row_color}
\alias{ls_set_row_color}
\alias{ls_get_row_colors}
\alias{ls_clear_row_color}
\alias{ls_color_selected_rows}
\title{Set row colors for linked workbench rows}
\usage{
ls_set_row_color(group = NULL, rows, color)
ls_get_row_colors(group = NULL)
ls_clear_row_color(group = NULL, rows = NULL)
ls_color_selected_rows(group = NULL, color)
}
\arguments{
\item{group}{Dataset/group name. If omitted, the active dataset is used.}
\item{rows}{Original row indices.}
\item{color}{One of the palette color names.}
}
\description{
Stores row colors as group-level row state and redraws linked plots and data
sheets.
}
\value{\code{ls_get_row_colors()} returns a named character vector. Other functions invisibly return \code{TRUE}.}
