\name{ls_glm}
\alias{ls_glm}
\title{Open the native GLM workbench}
\usage{
ls_glm(plot = NULL)
}
\arguments{
\item{plot}{Optional \code{rlispstat_plot} object or plot id. If omitted, the
backend's active plot is used.}
}
\value{
Invisibly returns \code{TRUE}.
}
\description{
Opens a native macOS General Linear Model window associated with a scatterplot.
The first prototype fits a simple linear model for the plot's current variables
and can use either all plotted rows or the selected rows in the linked group.
}
\examples{
\dontrun{
p <- ls_scatter(mtcars, "wt", "mpg", group = "cars")
ls_glm(p)
}
}
