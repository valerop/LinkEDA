\name{ls_new_correlation_matrix}
\alias{ls_new_correlation_matrix}
\alias{ls_correlation_matrix_state}
\alias{ls_correlation_matrix_cells}
\alias{ls_correlation_matrix_add_variable}
\alias{ls_correlation_matrix_remove_variable}
\alias{ls_correlation_matrix_table}
\alias{ls_copy_correlation_matrix}
\alias{ls_export_correlation_matrix}
\title{Create a Pearson correlation matrix}
\usage{
ls_new_correlation_matrix(data = NULL, variables = NULL, method = "pearson",
  missing = c("pairwise", "listwise"), show_p = NULL, show_n = FALSE,
  show_p_value = FALSE, show_significance_stars = TRUE, name = NULL,
  native = TRUE)

ls_correlation_matrix_state(matrix)

ls_correlation_matrix_cells(matrix)

ls_correlation_matrix_add_variable(matrix, variable)

ls_correlation_matrix_remove_variable(matrix, variable)

ls_correlation_matrix_table(matrix)

ls_copy_correlation_matrix(matrix, format = c("text", "markdown"))

ls_export_correlation_matrix(matrix, path, format = c("pdf", "txt", "md"))
}
\arguments{
\item{data}{Registered dataset name, a data frame, or \code{NULL} for the active dataset.}
\item{variables}{Numeric variables to include.}
\item{method}{Correlation method. Currently only \code{"pearson"} is supported.}
\item{missing}{Missing-data mode: \code{"pairwise"} or \code{"listwise"}.}
\item{show_p}{Legacy alias for significance stars.}
\item{show_n}{Logical display option for sample sizes.}
\item{show_p_value}{Logical. Show exact p-values in cells.}
\item{show_significance_stars}{Logical. Show significance stars in cells.}
\item{name}{Optional correlation matrix id or dataset name for data-frame input.}
\item{native}{Logical. Open the native window when possible.}
\item{matrix}{A correlation matrix handle.}
\item{variable}{A numeric variable name.}
\item{format}{Copy/export format.}
\item{path}{Output path.}
}
\value{
A correlation matrix handle, state, cell data, copied text, or output path depending on the function.
}
\description{
Creates a Pearson correlation matrix for a registered \pkg{LinkEDA} dataset.
The native window can add or remove numeric variables interactively and opens
linked scatterplots from correlation cells.
}
\examples{
\dontrun{
ls_register_dataset("cars", mtcars)
cm <- ls_new_correlation_matrix("cars", variables = c("mpg", "wt", "hp"))
ls_correlation_matrix_cells(cm)
}
}
