\name{ls_scale_analysis_save_total}
\alias{ls_scale_analysis_save_total}
\title{Save a scale sum or mean to its source dataset}
\usage{ls_scale_analysis_save_total(analysis, method = c("sum", "mean"), name = NULL)}
\arguments{
\item{analysis}{A Scale Analysis handle or identifier.}
\item{method}{Sum or mean of the scored items.}
\item{name}{Optional column name. Existing names receive a unique suffix.}}
\description{Uses the analysis's item coding, reverse scoring, minimum valid item rule and frozen analysis scope. All calculations use R. Rows outside the scope receive missing values.}
\details{Each completed imputation is scored separately; scores are not pooled. Ordinal category order is frozen before case selection and shared across imputations, including reverse scoring. Categories absent from a subset do not renumber the remaining categories. Input changes require refitting. The new column is added to the original dataset and every completed representation. Transformation history contains an executable base-R verification recipe. Factor and component scores are saved separately with \code{ls_scale_analysis_save_scores}.}
\value{The new column name, invisibly.}
