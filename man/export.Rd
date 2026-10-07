\name{ls_glm_regression_table}
\alias{ls_glm_regression_table}
\alias{ls_copy_glm_table}
\alias{ls_export_glm_table}
\alias{ls_regression_comparison_table}
\alias{ls_copy_regression_comparison_table}
\alias{ls_export_regression_comparison_table}
\alias{ls_generalized_linear_model_table}
\alias{ls_copy_generalized_linear_model_table}
\alias{ls_export_generalized_linear_model_table}
\alias{ls_export_boxplot}
\alias{ls_copy_boxplot}
\alias{ls_export_histogram}
\alias{ls_copy_histogram}
\title{Copy and export regression tables and linked plots}
\usage{
ls_glm_regression_table(model)
ls_copy_glm_table(model, format = c("text", "markdown"))
ls_export_glm_table(model, path, format = c("pdf", "txt", "md"))
ls_regression_comparison_table(comparison)
ls_copy_regression_comparison_table(comparison, format = c("text", "markdown"))
ls_export_regression_comparison_table(comparison, path, format = c("pdf", "txt", "md"))
ls_generalized_linear_model_table(model)
ls_copy_generalized_linear_model_table(model, format = c("text", "markdown"))
ls_export_generalized_linear_model_table(model, path, format = c("pdf", "txt", "md"))
ls_export_boxplot(plot, path, width = NULL, height = NULL,
  format = c("pdf", "png", "svg"), theme = NULL, dpi = 300)
ls_copy_boxplot(plot, format = NULL, theme = NULL)
ls_export_histogram(plot, path, width = NULL, height = NULL,
  format = c("pdf", "png", "svg"), theme = NULL, dpi = 300)
ls_copy_histogram(plot, format = NULL, theme = NULL)
}
\arguments{
\item{model}{An \code{rlispstat_glm} or \code{rlispstat_generalized_linear_model} model handle.}
\item{comparison}{An \code{rlispstat_regression_comparison} handle.}
\item{plot}{An \code{rlispstat_plot} object or plot id.}
\item{path}{Output file path.}
\item{format}{Output or clipboard format.}
\item{width, height}{Optional R fallback plot size in inches.}
\item{theme}{Optional plot theme used by the R fallback renderer.}
\item{dpi}{PNG resolution in dots per inch.}
}
\description{
Builds structured regression tables for single linear models, model-comparison
windows, and generalized linear models, renders them as plain text, Markdown,
or PDF, and exports linked boxplots/histograms. Categorical predictors are rendered
as parent/level rows when structured coefficient rows are available. Plot
export uses the native view when available and falls back to a base R rendering
otherwise.
}
\value{
Table builders return an \code{rlispstat_regression_table}. Copy functions
invisibly return the copied text or temporary path. Export functions invisibly
return \code{path}.
}
