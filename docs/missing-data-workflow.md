# Missing Data workflow

## Implementation

- Menu order: Missing Data Overview, Missingness Models, Multiple Imputation, Imputation Diagnostics.
- All summaries and statistical analyses run in R. Native code only checks dataset capabilities, captures the specification/scope, transports it and renders the results.
- `R/missing_data_overview.R` is the shared orchestration layer. It calls existing Table 1 and binary regression code, the ordinary version/mutation/synchronization APIs, and the shared verification/publication architecture.
- `src/core/missing_data_model.h` shares capability checks and the workflow specification between macOS and Windows. Both use the existing main-R task transport, including the captured dataset version and explicit scope (an empty selection never becomes all rows).
- UI files: macOS `linkeda_macos_app.mm`; Windows `ApplicationMenu.{h,cpp}`, `App.xaml.{h,cpp}`, `WorkflowWindows.{h,cpp}`. `R/state.R` dispatches the new workflow kinds. `command_model.cpp` routes the menu commands; `command_dispatcher.cpp` accepts the overview table type.
- `R/table1.R` transports the overview through the existing structured-table protocol. `R/generalized_linear_model.R` retains missingness provenance when the binary model is refitted. `NAMESPACE`, `DESCRIPTION`, and `man/missing-data-overview.Rd` expose and document the R APIs.

## MI sources and availability

Existing `ls_import_mice()` and RDS/RData import recognize actual objects inheriting `mids`. The existing representation retains original_data, completed_datasets, m, methods, predictor matrix, iterations, chain history, source/name mapping, and the original mids. Nothing checks whether LinkEDA created the object.

Diagnostics are enabled only for a recognized MI dataset with retained process metadata. Plain completed dataframes are ordinary datasets and do not acquire MI capabilities. Legacy completed-only/stacked data without retained process metadata cannot provide process diagnostics.

Overview is available for nonempty datasets, including a meaningful no-missing-values state. Missingness models require a target with both missing and observed values. Multiple Imputation requires missing active values in an ordinary dataset. Existing MI datasets are not silently re-imputed.

## Overview

The default source for MI is the retained original incomplete data. The active completed dataset is an explicit alternative. Ordinary datasets use their active data.

Pattern-defining variables and descriptive variables are independent selections. Canonical missingness signatures receive P1, P2, ... labels; ordering of observations does not change those labels. Display order puts complete cases first, then frequency and missingness. Changing the defining variables, source, or analysis population defines a new pattern classification.

The pattern table reports N, percentage, missing variable names and **total missing cells across observations** within each pattern. Table 1 below it is built by the existing ordinary Table 1 engine. No estimates are produced for a group-variable combination with no observed data. Its existing comparisons, factor hierarchy, references, formatting and export remain available.

The two semantic tables are placed in one scrolling native report. Each table retains its own context menu and R/publication export. Exporting a table exports that section, not a combined two-section publication document.

An immutable registered analysis snapshot holds the chosen source and rows, plus the derived grouping variable. The source dataset/version/scope are retained and reported. The ordinary Table 1 can be edited using its existing controls. To change the pattern definition or source, open Missing Data Overview again.

## Little's MCAR test

Uses the established public `naniar::mcar_test()` implementation (maximum-likelihood estimation under a multivariate-normal model). No native or custom replacement statistic is implemented.

Only semantically Numeric variables with numeric storage are accepted. Factors, ordinal variables and text are rejected explicitly, even when stored as numeric codes. At least two nonconstant variables with enough observed values are required. Infinite values, nonfinite/undefined results, or failed estimation yield an informative unavailable state. A conservative 50-variable limit avoids exceeding the package's binary-pattern encoding precision. Package absence is explained rather than substituted with another algorithm.

A significant result gives evidence against MCAR. A nonsignificant result does not prove MCAR. Rejecting MCAR does not establish MAR; MAR cannot be verified from observed data alone.

Reference: https://naniar.njtierney.com/reference/mcar_test.html

## Saved columns

The optional Save to data area stores patterns, number missing, percentage missing and selected per-variable indicators. Patterns are categorical; their descriptions preserve P-label meanings. Logical indicators use LinkEDA's binary Categorical semantics and logical R storage. Counts/percentages/probabilities are Numeric.

Existing columns are never overwritten: names are made unique. Original row positions are preserved; rows outside a restricted scope receive NA. Saving rejects stale source versions. The standard dataset-version and notification path updates sheets and analyses.

For MI, derived columns are added identically to original data and every completed dataset. Public `mice::cbind()` extends the retained mids without discarding its chain history. The imputation registry is synchronized so changing the active imputation cannot discard the columns. RDS export/reimport retains the resulting mids metadata.

## Missingness models

One target per model: Missing vs Observed, with Missing as the event. The existing binary logistic model engine supplies coefficients, odds ratios, intervals, coefficient and global factor tests, references, N, diagnostics and the usual editable model controls.

Rows with missing predictors are excluded from fitting; corresponding predicted probabilities are unavailable. These models describe associations with observed predictors, not a test or proof of MAR. Saving the indicator and predicted probability uses the same versioned-column mechanism as Overview.

## R code / publication

Verification recipes are captured when results are computed, from frozen data and scope, using public base/stats, naniar, gtsummary and other existing model-engine packages. Model verification explicitly reconstructs the target missingness indicator from its original variable, including after refitting. Pattern verification recalculates signatures/counts. Derived-column history records public R expressions and exact row positions. The existing unbound-path/export-bundle workflow is retained.

Table 1 and binary outputs retain their standard semantic publication specifications and real LaTeX/tinytable PDF paths. There is no new HTML-to-PDF or screenshot-PDF pipeline.

## Validation

- `tests/testthat/test-missing-data-overview.R`: pattern counts, independent descriptor selection, row-order stability, saved values/types/labels, public transformation recipe execution, stale versions, complete data, empty/partial scopes, direct naniar parity, categorical exclusion, stats::glm coefficient/probability parity, factor tests, executable public model verification and refitting, external mids, preserved chains, switching imputations, RDS roundtrip and plain completed dataframes.
- Existing regression files: MI process retention, MI diagnostics, binary regression and Table 1.
- `tests/core/missing_data_model_test.cpp`: ordinary/complete/MI capability states, sparse original missingness, absent process metadata and explicit empty scope transport.
- `tests/native/missing_data_fixtures.R` + `missing_data_report_smoke.mm`: actual R structured payloads, native parsing/provenance, combined report, section layout, values and context menus.

Windows code follows the same shared workflow, but a Windows build/runtime is not available on this Mac. No claim of Windows runtime validation is made. Recognized process diagnostics currently use mice mids as the reference format; other R MI classes do not gain diagnostics unless supported by the existing importer with sufficient original/process metadata.
