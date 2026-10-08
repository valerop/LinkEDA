\name{ls_binary_regression_open_diagnostic}
\alias{ls_binary_regression_open_diagnostic}
\title{Open a linked Binary Regression diagnostic plot}
\usage{
ls_binary_regression_open_diagnostic(model,
  type = c("observed_vs_fitted", "residuals_vs_fitted", "residual_histogram",
    "normal_qq", "scale_location", "residuals_leverage", "cooks_distance",
    "roc_curve", "calibration_plot"),
  native = isTRUE(.rls_state$process_started))
}
\arguments{
\item{model}{A fitted Binary Regression model.}
\item{type}{Diagnostic plot type.}
\item{native}{Open the native LinkEDA plot window when available.}
}
\value{A linked diagnostic handle.}
\description{Opens a diagnostic plot linked to the observations used by a Binary Regression model.}
