\name{ls_new_table1}
\alias{ls_new_table1}
\alias{ls_table1_table}
\alias{ls_copy_table1}
\alias{ls_export_table1}
\title{Create a Table 1 descriptive summary}
\usage{
ls_new_table1(data = NULL, variables = NULL, group = NULL,
  variable_types = NULL, include_missing = TRUE, show_p = TRUE,
  show_test = TRUE, show_n = TRUE, ordinal_as = c("ordinal", "categorical"),
  numeric_stats = c("mean_sd", "median_iqr"),
  categorical_stats = c("n_percent"),
  ordinal_stats = c("n_percent", "median_iqr"), name = NULL,
  .selected_rows = NULL, .scope_description = NULL)

ls_table1_table(table)

ls_copy_table1(table, format = c("text", "markdown"))

ls_export_table1(table, path, format = c("pdf", "txt", "md", "csv"))
}
\arguments{
\item{data}{Registered dataset name, a data frame, or \code{NULL} for the active dataset.}
\item{variables}{Variables to summarize. If \code{NULL}, supported non-ID variables are selected.}
\item{group}{Optional grouping variable.}
\item{variable_types}{Optional named vector or list overriding variable types: \code{"numeric"}, \code{"categorical"}, or \code{"ordinal"}.}
\item{include_missing}{Logical. Show missing counts and percentages.}
\item{show_p}{Logical. Show p-values for grouped tables.}
\item{show_test}{Logical. Show the test used for grouped comparisons.}
\item{show_n}{Logical. Show the top N row.}
\item{ordinal_as}{How ordered factors are treated by default.}
\item{numeric_stats}{Numeric summaries to include.}
\item{categorical_stats}{Categorical summaries to include.}
\item{ordinal_stats}{Ordinal summaries to include.}
\item{name}{Optional Table 1 id.}
\item{.selected_rows}{Internal explicit original-row IDs supplied by the native
analysis-scope bridge.}
\item{.scope_description}{Internal human-readable description for the captured
analysis scope.}
\item{table}{A Table 1 handle returned by \code{ls_new_table1}.}
\item{format}{Copy/export format.}
\item{path}{Output path.}
}
\value{
A Table 1 handle, display table, copied text, or output path depending on the function.
}
\description{
Creates a native Table 1 / descriptive summary table for a registered
\pkg{LinkEDA} dataset. Numeric variables are summarized with mean (SD) and
median [Q1, Q3], categorical variables with n (%), and ordinal variables with
ordered n (%) plus optional median [Q1, Q3]. Grouped tables include appropriate
Welch, rank-based, chi-square, Fisher, or simulated chi-square tests.
}
\examples{
\dontrun{
ls_register_dataset("cars", mtcars)
t1 <- ls_new_table1("cars", variables = c("mpg", "cyl", "am"), group = "am")
ls_table1_table(t1)
}
}
