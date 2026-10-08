\name{ls_glm_interaction}
\alias{ls_glm_interaction}
\alias{ls_generalized_linear_model_interaction}
\alias{ls_binary_regression_interaction}
\alias{ls_count_regression_interaction}
\title{Interpret a fitted regression interaction}
\usage{
ls_glm_interaction(model, term, confidence_level = 0.95,
  adjust = "tukey", grid_points = 25L, emm_test_reference = NULL,
  focal = NULL, quantity = "predicted_probability",
  adjustment = "average_sample", presentation = "percentage",
  group_by = NULL)

ls_generalized_linear_model_interaction(model, term,
  confidence_level = 0.95, adjust = "tukey", grid_points = 25L,
  emm_test_reference = NULL, focal = NULL,
  quantity = "predicted_probability", adjustment = "average_sample",
  presentation = "percentage", group_by = NULL)

ls_binary_regression_interaction(model, term,
  confidence_level = 0.95, adjust = "tukey", grid_points = 25L,
  emm_test_reference = NULL, focal = NULL,
  quantity = "predicted_probability", adjustment = "average_sample",
  presentation = "percentage", group_by = NULL)

ls_count_regression_interaction(model, term,
  confidence_level = 0.95, adjust = "tukey", grid_points = 25L,
  emm_test_reference = NULL, focal = NULL,
  quantity = c("expected_count", "predicted_probability",
    "overall_expected_score", "perfect_score_probability",
    "conditional_expected_score"), group_by = NULL)
}
\arguments{
\item{model}{A fitted LinkEDA regression model.}
\item{term}{An included main effect or two- or three-way interaction.}
\item{confidence_level}{Confidence level for the estimated effects.}
\item{adjust}{Multiplicity adjustment for pairwise comparisons.}
\item{grid_points}{Number of displayed focal values for numeric effects.}
\item{emm_test_reference}{Optional null value for tests of individual estimated marginal means.}
\item{focal}{Factor or variable compared on the horizontal axis.}
\item{quantity}{Response quantity used for binary and bounded-count effects.}
\item{adjustment}{Binary-model adjustment strategy.}
\item{presentation}{Binary-probability display scale.}
\item{group_by}{Optional categorical factor used for the first grouping within
an all-factor interaction. It must differ from \code{focal}. In a three-factor
interaction the remaining factor defines the strata.}
}
\description{
Calculates estimated effects and comparisons in R from the fitted model. For
all-factor interactions of up to three factors, \code{focal} chooses the
comparison factor and \code{group_by} chooses which other factor groups those
comparisons. The third factor, when present, defines strata.
}
\value{A \code{linkeda_regression_interaction} result containing estimates,
comparisons, interaction contrasts and plot data.}
