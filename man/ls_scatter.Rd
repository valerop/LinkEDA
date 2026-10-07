\name{ls_scatter}
\alias{ls_scatter}
\title{Open a native interactive scatterplot}
\usage{
ls_scatter(data, x, y, group = NULL, labels = NULL, title = NULL)
}
\arguments{
\item{data}{A data frame.}
\item{x, y}{Names of numeric columns to plot.}
\item{group}{Optional non-empty string used for linked brushing.}
\item{labels}{Optional labels for future identify support.}
\item{title}{Optional plot title shown in the native window.}
}
\value{
An object of class \code{rlispstat_plot} containing the plot id and group.
}
\description{
Opens a native macOS scatterplot window. Drag with the mouse to select points
using a rectangular brush. Plots that share the same \code{group} have linked
selections.
}
\details{
Missing values in \code{x} or \code{y} are excluded from drawing, but selected
points still map back to their original row indices.
}
\examples{
\dontrun{
p1 <- ls_scatter(mtcars, x = "wt", y = "mpg", group = "cars")
p2 <- ls_scatter(mtcars, x = "hp", y = "mpg", group = "cars")
ls_selected("cars")
}
}
