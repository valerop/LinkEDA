\name{ls_imputation_diagnostics}
\alias{ls_imputation_diagnostics}
\alias{ls_missing_information_diagnostics}
\title{Multiple-imputation diagnostics}
\usage{
ls_imputation_diagnostics(dataset = NULL,
  section = c("summary", "variables", "patterns", "model", "events",
    "chain_mean", "chain_variance", "convergence", "distributions"),
  variable = NULL, max_patterns = 20L, sort_missing = TRUE, native = TRUE, imputation_start = 1L)
ls_missing_information_diagnostics(result)
}
\arguments{
\item{dataset}{Registered multiple-imputation dataset, or NULL for the active dataset.}
\item{section}{The diagnostic table or plot to display.}
\item{variable}{Variable for a chain or observed-versus-imputed comparison.}
\item{max_patterns}{Maximum number of missingness patterns; Inf shows all.}
\item{sort_missing}{Sort variables by decreasing original missing percentage.}
\item{imputation_start}{First imputation in a numeric distribution view. At most five imputations and the observed distribution are shown.}
\item{native}{Display the requested native table or plot.}
\item{result}{A fitted model handle or analytical result record.}
}
\description{
Imputation diagnostics describe original missingness and the process retained
in a genuine \code{mice::mids} object. Missing-information diagnostics describe
the uncertainty of pooled estimates, on the scale used for pooling.
}
\details{
Numeric distribution plots show at most 512 ECDF steps per curve. All finite values enter \code{stats::ecdf}; large displays use type-1 quantile locations and are labelled as simplified. Use the native Imputations shown menu to change the visible set. Exported verification code draws the exact ECDF for that same set.

Completed lists and stacked data cannot reconstruct the original imputation
process. Import the original mids with \code{ls_import_mice} or save it with
\code{saveRDS} and use \code{ls_import_rds}. Stacked data open as ordinary data.

The imputation dialog retains method and predictor controls. Its general-purpose
default uses the selected predictors across the imputation models. This may
be unsuitable for a study, make convergence difficult or use unnecessary
computing resources. Separate predictor sets, interactions, blocks and other
advanced specifications should be prepared in mice in R.

Missingness always refers to originally missing cells. Distribution comparisons
use observed cells separately from originally missing cells, with a separate
series for each imputation. Existing chain means and variances are displayed;
missing chain statistics are explicitly reported as unavailable. These checks
do not establish that imputations are correct.

The convergence summary uses \code{coda::autocorr.diag} (lag 1, averaged
over chains) and \code{coda::gelman.diag} for chain means and SDs. PSRF is
the classical Gelman-Rubin diagnostic, not rank-normalized split R-hat.
All retained iterations are used, with no automatic burn-in. At least four
iterations and two nonconstant finite chains are required; short chains
give unstable diagnostics. Inspect the trace plots as well. Categorical
chain summaries use category codes and can hide category-specific problems.

FMI is the fraction of uncertainty associated with missing information, not
the fraction of missing cells. RIV is the relative increase in variance;
Ubar, B and T are within-imputation, between-imputation and total variance.
Relative efficiency is the approximation 1 / (1 + FMI / m). No qualitative
thresholds are applied. The usual result tables remain unchanged; an optional
expanded table provides the diagnostic quantities where retained by pooling.

\code{MCSE (estimate)} is the approximate Monte Carlo standard error of
the pooled point estimate, \eqn{\sqrt{B/m}}, assuming independent, converged
imputations. \code{MCSE / SE (\%)} expresses it as a percentage of the pooled
standard error. It does not estimate the simulation error of the standard
error, confidence limits or p-value. Increasing m generally reduces MCSE;
four times as many imputations approximately halves it when B is similar.
With small m, MCSE itself is uncertain, and B = 0 does not establish stability.
High relative efficiency does not guarantee stable standard errors or p-values.
All quantities, including MCSE, remain on the pooling scale (e.g. Fisher z).
}
\value{
\code{ls_imputation_diagnostics} invisibly returns the retained inputs, table
and plotted data. \code{ls_missing_information_diagnostics} returns a data frame
of estimates and diagnostic quantities, or an empty data frame when unavailable.
}
\seealso{\code{mice::mice}, \code{mice::pool}, \code{mice::pool.scalar},
\code{coda::gelman.diag}, \code{coda::autocorr.diag}}
