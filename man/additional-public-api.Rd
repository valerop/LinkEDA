\name{additional-public-api}
\alias{additional-public-api}
\alias{ls_add_smooth}
\alias{ls_analysis_backend}
\alias{ls_barplot}
\alias{ls_binary_regression_interaction_contrasts}
\alias{ls_complete_data_version}
\alias{ls_completed_dataset}
\alias{ls_copy_barchart}
\alias{ls_copy_barplot}
\alias{ls_copy_mixed_model_table}
\alias{ls_copy_plot}
\alias{ls_copy_scatterplot}
\alias{ls_count_regression_interaction_contrasts}
\alias{ls_export_barchart}
\alias{ls_export_barplot}
\alias{ls_export_data}
\alias{ls_export_mixed_model_table}
\alias{ls_export_plot}
\alias{ls_export_scatterplot}
\alias{ls_generalized_linear_model_interaction_contrasts}
\alias{ls_get_data_version}
\alias{ls_glm_delta_r2}
\alias{ls_glm_interaction_contrasts}
\alias{ls_glm_partial_r}
\alias{ls_imputation_summary}
\alias{ls_mixed_model_add_fixed_effect}
\alias{ls_mixed_model_add_random_effect}
\alias{ls_mixed_model_add_random_slope}
\alias{ls_mixed_model_diagnostics}
\alias{ls_mixed_model_open_diagnostic}
\alias{ls_mixed_model_refit}
\alias{ls_mixed_model_remove_fixed_effect}
\alias{ls_mixed_model_remove_random_effect}
\alias{ls_mixed_model_remove_random_slope}
\alias{ls_mixed_model_scope}
\alias{ls_mixed_model_set_family}
\alias{ls_mixed_model_set_method}
\alias{ls_mixed_model_set_response}
\alias{ls_mixed_model_state}
\alias{ls_mixed_model_table}
\alias{ls_new_barplot}
\alias{ls_new_generalized_mixed_model}
\alias{ls_new_linear_mixed_model}
\alias{ls_new_missing_data_imputation}
\alias{ls_new_scale_analysis}
\alias{ls_plot_theme}
\alias{ls_record_clear}
\alias{ls_record_copy_internal}
\alias{ls_record_copy_r}
\alias{ls_record_replay}
\alias{ls_record_save_r}
\alias{ls_record_show}
\alias{ls_record_start}
\alias{ls_record_stop}
\alias{ls_row_ids}
\alias{ls_run_imputation}
\alias{ls_scale_analysis_correlations}
\alias{ls_scale_analysis_dimensionality}
\alias{ls_scale_analysis_items}
\alias{ls_scale_analysis_save_scores}
\alias{ls_scale_analysis_scores}
\alias{ls_scale_analysis_set_items}
\alias{ls_scale_analysis_set_options}
\alias{ls_scale_analysis_state}
\alias{ls_scale_analysis_summary}
\alias{ls_select_rows_by_id}
\alias{ls_set_imputed_dataset_version}
\title{Additional LinkEDA Workflow Functions}
\description{
These functions provide the programmatic counterparts of LinkEDA's native
menus for linked plots, dataset versions, model diagnostics, multiple
imputation, mixed models, scale analysis, and reproducibility recording.
}
\details{
Plot copy and export functions preserve the plot's current visual state.
Dataset-version functions expose the immutable version used by an analysis and
allow row identifiers to be mapped back to the data sheet. Model and scale
functions return the same R-calculated results shown in their native windows.
Recording functions capture user actions as reproducible LinkEDA commands.

For argument definitions and return structures, inspect the corresponding
function with \code{args()} and use the domain help pages linked below. The
native interface remains the recommended entry point for interactive work.
}
\seealso{
\code{\link{ls_new_scatterplot}}, \code{\link{ls_new_binary_regression}},
\code{\link{ls_new_count_regression}}, \code{\link{ls_imputation_diagnostics}},
\code{\link{ls_scale_analysis_save_total}}, and
\code{\link{ls_record_start}}.
}
