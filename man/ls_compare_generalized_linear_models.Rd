\name{ls_compare_generalized_linear_models}
\alias{ls_compare_generalized_linear_models}
\title{Compare fitted generalized linear models}
\usage{
ls_compare_generalized_linear_models(...,
  native = isTRUE(.rls_state$process_started), .native_id = NULL,
  .native_labels = NULL, .native_generation = 0L,
  .native_specification_revisions = integer(),
  .native_specification_fingerprints = character(),
  .binary_comparison = FALSE, .count_comparison = FALSE,
  .model_type = NULL)
}
\arguments{
\item{...}{Fitted generalized-linear-model handles, or one list of handles.}
\item{native}{Logical; open the native Windows comparison window.}
\item{.native_id, .native_labels, .native_generation,
.native_specification_revisions, .native_specification_fingerprints,
.binary_comparison, .count_comparison, .model_type}{Internal immutable native
window identity and specialized-comparison metadata.}
}
\value{A structured model-comparison result.}
\description{Compares fit statistics and obtains likelihood-ratio tests from R for compatible adjacent nested models.}
