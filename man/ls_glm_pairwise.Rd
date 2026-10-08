\name{ls_glm_pairwise}
\alias{ls_glm_pairwise}
\alias{ls_generalized_linear_model_pairwise}
\alias{ls_count_regression_pairwise}
\title{Pairwise comparisons for regression models}
\usage{
ls_glm_pairwise(model, term, adjust = "tukey", confidence_level = .95)
ls_generalized_linear_model_pairwise(model, term, scale = "response",
  adjust = "tukey", confidence_level = .95)
ls_count_regression_pairwise(model, term, scale = "response",
  adjust = "tukey", confidence_level = .95)
}
\arguments{
\item{model}{A fitted LinkEDA regression model.}
\item{term}{Included categorical or ordered-factor main effect.}
\item{adjust}{Multiplicity adjustment passed to \code{emmeans}.}
\item{confidence_level}{Confidence level.}
\item{scale}{Result scale, normally \code{"response"} or \code{"link"}.
\code{"probability"} is available for binomial outcomes;
\code{"odds_ratio"} requires a logit link and \code{"risk_ratio"} a log link.
\code{"mean_ratio"} requires a non-binomial log link, and
\code{"multiplicative_ratio"} applies to lognormal models.
For lognormal models, response-scale comparisons are differences of conditional
medians (back-transformed log means), not arithmetic means. With multiple
imputations, emmeans combines the log-scale reference grid before transforming
it. Link-scale comparisons are differences of log means.
For beta regression with a logit link, link-scale comparisons use logits of
the fitted response means; their exponential compares mean odds
\code{mu / (1 - mu)}, not binary-event odds.}
}
\value{A data frame of pairwise comparisons calculated by emmeans in R.}
\description{Uses the fitted R model and, for multiple imputation,
the official emmeans support for mice::mira.}
