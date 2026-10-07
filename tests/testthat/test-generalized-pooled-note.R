test_that("pooled generalized model note states the pooling method once", {
  record <- list(imputation_count = 50L,
                 multiple_imputation = list(m = 50L),
                 response = "y", count_distribution = "poisson")
  note <- LinkEDA:::.rls_generalized_glm_pooled_note(record, 50L)
  expect_length(regmatches(note, gregexpr("m = 50", note, fixed = TRUE))[[1L]], 1L)
  expect_match(note, "link-scale coefficients use mice::pool (Rubin's rules)",
               fixed = TRUE)
  expect_false(grepl("Pooled multiple-imputation model", note, fixed = TRUE))
  expect_false(grepl("AIC/BIC/logLik", note, fixed = TRUE))
  expect_match(note, "Deviance", fixed = TRUE)

  record$count_distribution <- "negative_binomial"
  negative_binomial <- LinkEDA:::.rls_generalized_glm_pooled_note(record, 50L)
  expect_match(negative_binomial, "theta is not Rubin-pooled", fixed = TRUE)
  expect_length(regmatches(negative_binomial,
                           gregexpr("m = 50", negative_binomial, fixed = TRUE))[[1L]], 1L)

  record$count_distribution <- "hurdle_beta_binomial_ceiling"
  hurdle <- LinkEDA:::.rls_generalized_glm_pooled_note(record, 50L)
  expect_match(hurdle, "pooled separately", fixed = TRUE)
  expect_length(regmatches(hurdle, gregexpr("m = 50", hurdle, fixed = TRUE))[[1L]], 1L)

  attr(record, "missing_information") <- data.frame(Variable = "y")
  expect_match(LinkEDA:::.rls_generalized_glm_pooled_note(record, 50L),
               "Missing-information diagnostics are available", fixed = TRUE)
})
