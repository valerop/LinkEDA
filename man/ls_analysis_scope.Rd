\name{ls_analysis_scope}
\alias{ls_analysis_scope}
\alias{ls_set_analysis_scope}
\alias{ls_use_selected_as_analysis_scope}
\alias{ls_save_analysis_scope}
\alias{ls_saved_selections}
\alias{ls_use_saved_analysis_scope}
\alias{ls_add_saved_analysis_scope}
\alias{ls_use_all_observations}
\title{Inspect or change the persistent analysis scope}
\usage{
ls_analysis_scope(group = NULL)

ls_use_selected_as_analysis_scope(group = NULL, name = NULL)

ls_save_analysis_scope(group = NULL, name)

ls_saved_selections(group = NULL)

ls_use_saved_analysis_scope(group = NULL, name)

ls_add_saved_analysis_scope(group = NULL, name, result_name)

ls_use_all_observations(group = NULL)

ls_set_analysis_scope(group = NULL, rows, description = "Explicit rows from R")
}
\arguments{
\item{group}{Registered dataset/group. If \code{NULL}, the active group is used.}
  \item{name}{Name used when saving or applying an immutable selection scope.}
\item{result_name}{Name for a new saved selection formed by adding a saved selection to the active scope.}
\item{rows}{Original 1-based data-frame row IDs.}
\item{description}{Short description stored with the snapshot.}
}
\value{\code{ls_analysis_scope()} returns scope metadata and row IDs. \code{ls_saved_selections()} returns a data frame with names, sizes, and row-ID list columns. The setters invisibly return \code{TRUE}.}
\description{Controls the global rows used by analyses. Current-selection and unselected scopes follow linked brushing; named saved scopes and explicitly supplied rows are fixed snapshots. Every fit captures the effective rows and data version when it runs, so changing the global scope does not alter an existing result.}
