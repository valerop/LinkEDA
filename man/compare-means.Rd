\name{compare-means}
\alias{ls_new_one_sample_t_test}
\alias{ls_new_independent_samples_t_test}
\alias{ls_new_paired_samples_t_test}
\alias{ls_new_one_way_anova}
\title{Tests of Location and Means}
\description{
Runs parametric or rank-based one-sample, independent-samples, paired-samples,
and one-way tests. All inferential calculations are performed by R.
}
\usage{
ls_new_one_sample_t_test(data = NULL, response = NULL, mu = 0,
  alternative = c("two.sided", "less", "greater"), conf_level = 0.95,
  scope = "all", p_adjust = c("holm", "bonferroni", "none"),
  method = c("student", "wilcoxon", "binomial"), .selected_rows = NULL)

ls_new_independent_samples_t_test(data = NULL, response = NULL, group = NULL,
  alternative = c("two.sided", "less", "greater"), conf_level = 0.95,
  var_equal = FALSE, scope = "all", group_order = NULL,
  p_adjust = c("holm", "bonferroni", "none"), method = NULL,
  .selected_rows = NULL)

ls_new_paired_samples_t_test(data = NULL, pairs = NULL,
  alternative = c("two.sided", "less", "greater"), conf_level = 0.95,
  scope = "all", p_adjust = c("holm", "bonferroni", "none"),
  method = NULL, .selected_rows = NULL)

ls_new_one_way_anova(data = NULL, response = NULL, group = NULL,
  conf_level = 0.95, scope = "all",
  method = c("welch", "classical", "kruskal_wallis"),
  group_order = NULL, p_adjust = c("holm", "bonferroni", "none"),
  .selected_rows = NULL)
}
\arguments{
  \item{data}{Registered dataset name, a data frame, or \code{NULL} for the active dataset.}
  \item{response}{One or more numeric response-variable names.}
  \item{mu}{Null-hypothesis mean for the one-sample test.}
  \item{alternative}{Alternative hypothesis: \code{"two.sided"}, \code{"less"}, or \code{"greater"}.}
  \item{conf_level}{Confidence level, as a number between zero and one.}
  \item{scope}{Rows to analyze: \code{"all"}, \code{"selected"}, or \code{"unselected"}.}
  \item{group}{Grouping-variable name. Independent tests require exactly two observed levels.}
  \item{var_equal}{If \code{TRUE}, uses Student's pooled-variance t test; otherwise uses Welch's test.}
  \item{group_order}{Optional character vector defining group order and contrast direction.}
  \item{pairs}{List of two-element character vectors defining paired responses.}
  \item{method}{Test method. Rank-based alternatives are \code{"wilcoxon"} for
  one-sample and paired tests, \code{"mann_whitney"} for two independent
  groups, and \code{"kruskal_wallis"} for one-way tests.}
  \item{p_adjust}{Multiplicity correction across the valid inferential rows shown:
  \code{"holm"} (the default), \code{"bonferroni"}, or \code{"none"}.}
  \item{.selected_rows}{Internal immutable original-row identifiers supplied by
  the native analysis-scope bridge.}
}
\details{
For multiply imputed data, Welch ANOVA calls \code{stats::oneway.test(var.equal = FALSE)}
in every imputation and combines F statistics with \code{miceadds::micombine.F(version = 1)}.
This is a D2 chi-square approximation: finite denominator degrees of freedom from
individual Welch tests are not used in pooling. Every imputation must yield a valid test
with the same groups. Classical ANOVA retains its \code{mice::D1} pooling path.
Exact McNemar tests preserve defined binary categories when one measurement has
no observed events or no observed non-events, provided discordant pairs exist.

The ordinary-data implementations call \code{stats::t.test()},
\code{stats::wilcox.test()}, \code{stats::oneway.test()}, or
\code{stats::kruskal.test()}. Missing observations are removed separately for
each response or pair, so every result reports its own analysis-specific sample
size and original row identifiers. Adjusted p values are calculated from the
full-precision numeric p values with \code{stats::p.adjust()}; non-estimable
rows remain in the result but are excluded from the adjustment family.
}
\value{
Invisibly returns a character vector of result identifiers, one per response or
pair. When the native backend is running, all results from a call are displayed
in one structured result window.
}
\seealso{\code{stats::t.test}, \code{stats::wilcox.test},
\code{stats::oneway.test}, \code{stats::kruskal.test}}
