\name{ls_missing_data_overview}
\alias{ls_missing_data_overview}
\alias{ls_missingness_model}
\alias{ls_save_missing_data}
\title{Explore and model missingness using original or active data}
\usage{
ls_missing_data_overview(dataset = NULL, pattern_variables = NULL,
  descriptive_variables = NULL, source = c("original", "active"),
  .selected_rows = NULL, save = character(),
  native = isTRUE(.rls_state$process_started))
ls_missingness_model(dataset = NULL, target, predictors,
  source = c("original", "active"), .selected_rows = NULL,
  save = character(), native = isTRUE(.rls_state$process_started))
ls_save_missing_data(result,
  what = c("pattern", "n_missing", "pct_missing", "indicators"))
}
\arguments{
\item{dataset}{Registered dataset name; NULL uses the active dataset.}
\item{pattern_variables}{Variables defining missingness patterns. Defaults to all columns.}
\item{descriptive_variables}{Retained for compatibility and ignored. All pattern variables are described.}
\item{source}{For the overview, original pre-imputation data (default) or active data. Missingness models are available only for ordinary datasets: open the original incomplete dataset before fitting a missingness model. Ordinary datasets always use their active data.}
\item{.selected_rows}{Optional original row IDs. An empty integer vector is an empty scope, not all rows.}
\item{save}{Optional outputs to save immediately, using the names accepted by \code{what}.}
\item{native}{Whether to display native result windows.}
\item{target}{Variable whose missingness is the binary outcome.}
\item{predictors}{Observed predictors passed to the existing binary regression engine.}
\item{result}{An overview or missingness-model result.}
\item{what}{Overview: pattern, n_missing, pct_missing, indicators. Model: indicator, probability.}
}
\description{Missingness summaries are calculated in R; Table 1 and logistic models reuse the existing engines. Imported mice mids objects retain their original incomplete data.}
\details{The overview displays variables in rows and patterns in columns: a bullet denotes an observed value and a dash denotes a missing value. Its controls add or remove variables, select the original cases belonging to a pattern, open a Table 1 for one pattern or all patterns, and save derived columns. Descriptive tables and an open Little test update after variable changes. Saving remains available programmatically through the save functions.

Little's MCAR test is requested from the overview and uses only the continuous Numeric variables currently included in that table. It uses \code{naniar::mcar_test} for suitable continuous numeric variables under a multivariate-normal model. Categorical and ordinal variables are not silently converted. A significant result argues against MCAR; it does not establish MAR. A non-significant result does not prove MCAR. Logistic models exclude rows with missing predictors; missingness probabilities are NA for those rows. Saved columns use unique names, preserve scope and data version, and are identical in every imputation. Rows outside a selected scope receive NA.}
\value{Overview/model functions invisibly return their result and immutable source information. The save function invisibly returns created column names.}
