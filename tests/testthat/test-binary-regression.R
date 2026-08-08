test_that("binary response coding is explicit and matches stats::glm", {
  data <- transform(
    mtcars,
    outcome = factor(am, levels = c(0, 1), labels = c("automatic", "manual")),
    cyl = factor(cyl)
  )
  model <- ls_new_binary_regression(
    data, "outcome", c("wt", "cyl"), event = "manual", native = FALSE,
    name = "binary_explicit", term_types = list(cyl = "factor")
  )
  state <- ls_binary_regression_state(model)
  direct <- stats::glm(I(outcome == "manual") ~ wt + cyl, data = data, family = stats::binomial("logit"))

  expect_equal(stats::coef(state$fit), stats::coef(direct), tolerance = 1e-10)
  expect_identical(state$event, "manual")
  expect_identical(state$reference, "automatic")
  expect_match(state$status, "stats::glm")
  expect_true(isTRUE(state$summary$converged))
  expect_true(all(c("global_lr", "global_p", "mcfadden_r2", "cox_snell_r2",
                    "nagelkerke_r2", "auc") %in% names(state$summary)))
})

test_that("binary interaction terms are hierarchical and fitted by stats::glm", {
  data <- transform(mtcars, outcome = factor(vs))
  model <- ls_new_binary_regression(
    data, "outcome", "mpg:wt", event = "1", native = FALSE,
    name = "binary_interaction"
  )
  state <- ls_binary_regression_state(model)
  direct <- stats::glm(
    I(outcome == "1") ~ mpg + wt + mpg:wt,
    data = data,
    family = stats::binomial("logit")
  )

  expect_identical(state$terms, c("mpg", "wt", "mpg:wt"))
  expect_equal(stats::coef(state$fit), stats::coef(direct), tolerance = 1e-10)
  expect_true(any(state$term_tests$term == "mpg:wt"))
})

test_that("default factor event follows the effective R factor-level order", {
  data <- transform(
    mtcars,
    outcome = factor(ifelse(am == 1, "manual", "automatic"),
                     levels = c("manual", "automatic", "unused"))
  )
  model <- ls_new_binary_regression(data, "outcome", "wt", native = FALSE,
                                    name = "binary_factor_order")
  state <- ls_binary_regression_state(model)
  direct <- stats::glm(I(outcome == "automatic") ~ wt, data = data,
                       family = stats::binomial("logit"))

  expect_identical(state$event, "automatic")
  expect_identical(state$reference, "manual")
  expect_equal(stats::coef(state$fit), stats::coef(direct), tolerance = 1e-10)
})

test_that("event inversion reverses coefficients without changing source response", {
  data <- transform(mtcars, outcome = ifelse(am == 1, "yes", "no"))
  original <- data$outcome
  yes <- ls_new_binary_regression(data, "outcome", "wt", event = "yes", native = FALSE, name = "binary_yes")
  no <- ls_new_binary_regression(data, "outcome", "wt", event = "no", native = FALSE, name = "binary_no")

  expect_equal(stats::coef(ls_binary_regression_state(yes)$fit),
               -stats::coef(ls_binary_regression_state(no)$fit), tolerance = 1e-10)
  expect_identical(data$outcome, original)
})

test_that("probit never reports odds ratios", {
  data <- transform(mtcars, outcome = factor(am), cyl = factor(cyl))
  model <- ls_new_binary_regression(
    data, "outcome", c("wt", "cyl"), link = "probit", native = FALSE,
    name = "binary_probit", term_types = list(cyl = "factor")
  )
  coefficients <- ls_binary_regression_coefficients(model)
  expect_true(all(is.na(coefficients$odds_ratio)))
  expect_true(all(is.finite(coefficients$ci_lower[coefficients$row_type == "coefficient"])))
  pairwise <- ls_binary_regression_pairwise(model, "cyl")
  expect_true(all(pairwise$scale == "probability"))
  expect_false("odds_ratio" %in% names(pairwise))
})

test_that("mtcars probit preserves full intervals, LR terms, and instability metadata", {
  data <- transform(mtcars, vs = factor(vs), cyl = factor(cyl))
  model <- ls_new_binary_regression(
    data, "vs", c("mpg", "cyl"), event = "0", reference = "1",
    link = "probit", native = FALSE, name = "binary_mtcars_probit",
    term_types = list(cyl = "factor")
  )
  state <- ls_binary_regression_state(model)
  rows <- ls_binary_regression_coefficients(model)
  cyl_rows <- rows[rows$source_term == "cyl", , drop = FALSE]

  expect_identical(state$event, "0")
  expect_identical(state$reference, "1")
  expect_equal(state$summary$n_used, 32L)
  expect_equal(state$summary$event_count, 18L)
  expect_equal(state$summary$reference_count, 14L)
  expect_identical(unique(rows$statistic_name), "z")
  expect_true(all(is.finite(cyl_rows$ci_lower[cyl_rows$row_type == "coefficient"])))
  expect_true(all(is.finite(cyl_rows$ci_upper[cyl_rows$row_type == "coefficient"])))
  expect_true(any(cyl_rows$row_type == "reference" & cyl_rows$level == "4"))
  expect_true(all(c("6", "8") %in% cyl_rows$level))
  expect_true(any(state$term_tests$term == "cyl"))
  expect_true(any(grepl("unstable|separation", state$warnings, ignore.case = TRUE)))
  expect_true(all(c("iterations", "boundary", "rank", "parameter_count", "rank_deficient") %in%
                    names(state$summary)))
  expect_true(all(is.na(rows$odds_ratio)))
  payload <- rlispstat:::.rls_native_generalized_state_payload(state)
  expect_identical(payload[[11L]], "z")
  expect_true("BINARY_V2" %in% payload)
  expect_identical(rlispstat:::.rls_native_wire_number(Inf), "Inf")
  expect_identical(rlispstat:::.rls_native_wire_number(-Inf), "-Inf")
})

test_that("binary diagnostics preserve original row identity and labels", {
  data <- transform(mtcars, outcome = ifelse(am == 1, "event", "reference"))
  data$outcome[c(2, 7)] <- NA_character_
  model <- ls_new_binary_regression(data, "outcome", "wt", native = FALSE, name = "binary_diag")
  diagnostics <- ls_binary_regression_diagnostics(model)
  state <- ls_binary_regression_state(model)

  expect_identical(diagnostics$row_id, state$rows_used)
  expect_true(all(c("observed_label", "observed_binary", "fitted_response_scale",
                    "linear_predictor", "deviance_residual", "pearson_residual",
                    "leverage", "cooks_distance") %in% names(diagnostics)))
  expect_setequal(unique(diagnostics$observed_label), c("event", "reference"))
  expect_true(all(diagnostics$fitted >= 0 & diagnostics$fitted <= 1))
})

test_that("invalid responses and absent scoped categories fail clearly", {
  data <- mtcars
  data$three <- rep(c("a", "b", "c"), length.out = nrow(data))
  expect_error(
    ls_new_binary_regression(data, "three", "wt", native = FALSE, name = "binary_invalid"),
    "exactly two observed"
  )
})

test_that("intercept-only binary models are fitted in R", {
  data <- transform(mtcars, outcome = factor(am))
  model <- ls_new_binary_regression(data, "outcome", terms = NULL, native = FALSE,
                                    name = "binary_intercept_only")
  state <- ls_binary_regression_state(model)
  expect_true(inherits(state$fit, "glm"))
  expect_identical(names(stats::coef(state$fit)), "(Intercept)")
  expect_equal(unname(stats::fitted(state$fit)[[1L]]), mean(data$am), tolerance = 1e-10)
})

test_that("stored binary model comparison uses R LRT and blocks different links", {
  data <- transform(mtcars, outcome = factor(am), cyl = factor(cyl))
  small <- ls_new_binary_regression(data, "outcome", "wt", native = FALSE, name = "binary_comparison_data")
  group <- ls_binary_regression_state(small)$group
  full <- ls_new_binary_regression(group, "outcome", c("wt", "cyl"), native = FALSE,
                                   name = "binary_full", term_types = list(cyl = "factor"))
  comparison <- ls_compare_binary_regression_models(small, full)
  expect_true(comparison$tests$available[[1L]])
  expect_match(comparison$tests$reason[[1L]], "stats::anova")

  probit <- ls_new_binary_regression(group, "outcome", c("wt", "cyl"), link = "probit",
                                     native = FALSE, name = "binary_other_link",
                                     term_types = list(cyl = "factor"))
  descriptive <- ls_compare_binary_regression_models(small, probit)
  expect_false(descriptive$tests$available[[1L]])
  expect_match(descriptive$tests$reason[[1L]], "Different links")
})

test_that("stored models with different analysis rows remain descriptive", {
  data <- transform(mtcars, outcome = factor(am), incomplete = disp)
  data$incomplete[c(1, 4)] <- NA_real_
  small <- ls_new_binary_regression(data, "outcome", "wt", native = FALSE,
                                    name = "binary_rows_base")
  group <- ls_binary_regression_state(small)$group
  incomplete <- ls_new_binary_regression(group, "outcome", c("wt", "incomplete"),
                                         native = FALSE, name = "binary_rows_incomplete")
  comparison <- ls_compare_binary_regression_models(small, incomplete)

  expect_false(comparison$common_rows)
  expect_false(comparison$tests$available[[1L]])
  expect_match(comparison$tests$reason[[1L]], "Different analysis rows")
  expect_true(all(is.finite(comparison$models$AIC)))
})
