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

test_that("native binary comparison returns the exact per-model specification identity", {
  native_state <- LinkEDA:::.rls_state
  previous_runtime <- list(
    process_started = native_state$process_started,
    backend_kind = native_state$backend_kind
  )
  on.exit({
    native_state$process_started <- previous_runtime$process_started
    native_state$backend_kind <- previous_runtime$backend_kind
  }, add = TRUE)
  native_state$process_started <- FALSE
  native_state$backend_kind <- NULL
  # This is a wire-format unit test.  Keep it on the mocked native transport
  # instead of allowing a locally installed WinUI app to be launched.
  data <- transform(mtcars, outcome = factor(vs), cyl = factor(cyl))
  model <- ls_new_binary_regression(
    data, "outcome", c("mpg", "cyl"), event = "0", native = FALSE,
    name = "binary_identity_protocol", term_types = list(cyl = "factor"),
    factor_reference_levels = list(cyl = "8")
  )
  transmissions <- list()
  native_state$process_started <- TRUE
  native_state$backend_kind <- "native"
  testthat::local_mocked_bindings(
    .rls_send = function(lines, expect_reply = TRUE) {
      transmissions[[length(transmissions) + 1L]] <<- lines
      "OK"
    },
    .rls_send_winui = function(lines) {
      transmissions[[length(transmissions) + 1L]] <<- lines
      "OK"
    },
    ls_analysis_scope = function(group) {
      rows <- seq_len(nrow(LinkEDA:::.rls_dataset_record(group)$data))
      list(dataset_id = group, kind = "all", source = "all_data",
           description = "All observations", rows = rows,
           n = length(rows), total_n = length(rows))
    },
    .package = "LinkEDA"
  )
  invisible(ls_compare_binary_regression_models(
    model, native = TRUE, .native_id = "binary_identity_comparison",
    .native_generation = 41L,
    .native_specification_revisions = 17L,
    .native_specification_fingerprints = "immutable-fingerprint-17"
  ))
  captured <- unlist(transmissions, use.names = FALSE)
  identity <- match("GENERALIZED_COMPARISON_MODELS_V7", captured)
  expect_true(is.finite(identity))
  expect_identical(captured[[identity + 1L]], "41")
  expect_true("17" %in% captured[(identity + 1L):length(captured)])
  expect_true("immutable-fingerprint-17" %in% captured)
  expect_true("mpg" %in% captured)
  expect_true("cyl" %in% captured)
  expect_false("hp" %in% captured)

  group <- LinkEDA:::.rls_register_dataset(
    "binary_identity_roundtrip", data
  )
  request <- c(
    "GCOMP_NEEDED", "binary_identity_roundtrip_comparison", group,
    "outcome", "binomial", "logit", "all", "BINARY", "0", "1",
    "2", "mpg", "numeric", "cyl", "factor",
    "1", "binary_identity_roundtrip:model:1", "Model 1",
    "2", "mpg", "cyl", "0",
    "GCOMP_SPEC_V4", "41", "TRUE", "FALSE", "1",
    "binary_identity_roundtrip:model:1", "outcome", "binomial", "logit", "all",
    "FALSE", "poisson", "", "17", "immutable-fingerprint-17",
    "2", "mpg", "cyl",
    "2", "mpg", "numeric", "cyl", "factor",
    "0", "1", "cyl", "8"
  )
  transmissions <- list()
  native_state$process_started <- TRUE
  native_state$backend_kind <- "native"
  expect_true(isTRUE(LinkEDA:::.rls_handle_generalized_comparison_needed(request)))
  captured <- unlist(transmissions, use.names = FALSE)
  expect_true("GENERALIZED_COMPARISON_MODELS_V7" %in% captured,
              info = paste(head(captured, 30L), collapse = " | "))
  expect_true("immutable-fingerprint-17" %in% captured)
  expect_true("17" %in% captured)
  expect_true("cyl" %in% captured)
  expect_false("hp" %in% captured)
})

test_that("native comparison round trip preserves the exact two-factor probit design", {
  native_state <- LinkEDA:::.rls_state
  previous_runtime <- list(
    process_started = native_state$process_started,
    backend_kind = native_state$backend_kind
  )
  on.exit({
    native_state$process_started <- previous_runtime$process_started
    native_state$backend_kind <- previous_runtime$backend_kind
  }, add = TRUE)
  native_state$process_started <- FALSE
  native_state$backend_kind <- NULL
  group <- LinkEDA:::.rls_register_dataset("binary_two_factor_wire", mtcars)
  transmissions <- list()
  native_state$process_started <- TRUE
  native_state$backend_kind <- "native"
  testthat::local_mocked_bindings(
    .rls_send = function(lines, expect_reply = TRUE) {
      transmissions[[length(transmissions) + 1L]] <<- lines
      "OK"
    },
    .rls_send_winui = function(lines) {
      transmissions[[length(transmissions) + 1L]] <<- lines
      "OK"
    },
    ls_analysis_scope = function(group) {
      rows <- seq_len(nrow(LinkEDA:::.rls_dataset_record(group)$data))
      list(dataset_id = group, kind = "all", source = "all_data",
           description = "All observations", rows = rows,
           n = length(rows), total_n = length(rows))
    },
    .package = "LinkEDA"
  )
  request <- c(
    "GCOMP_NEEDED", "binary_two_factor_comparison", group,
    "am", "binomial", "probit", "all", "BINARY", "1", "0",
    "3", "cyl", "factor", "mpg", "numeric", "gear", "factor",
    "1", "binary_two_factor_model_1", "Model 1",
    "3", "cyl", "mpg", "gear", "0",
    "GCOMP_SPEC_V4", "52", "TRUE", "FALSE", "1",
    "binary_two_factor_model_1", "am", "binomial", "probit", "all",
    "FALSE", "poisson", "", "19", "two-factor-fingerprint-19",
    "3", "cyl", "mpg", "gear",
    "3", "cyl", "factor", "mpg", "numeric", "gear", "factor",
    "0", "1", "cyl", "4"
  )

  native_state$process_started <- TRUE
  native_state$backend_kind <- "native"
  expect_true(isTRUE(suppressWarnings(
    LinkEDA:::.rls_handle_generalized_comparison_needed(request)
  )))
  captured <- unlist(transmissions, use.names = FALSE)
  record <- LinkEDA:::.rls_generalized_glm_record("binary_two_factor_model_1")
  expect_identical(record$terms, c("cyl", "mpg", "gear"))
  expect_identical(record$fit_specification_witness$coefficient_columns,
                   list(cyl = c("cyl6", "cyl8"), mpg = "mpg",
                        gear = c("gear4", "gear5")))
  expect_equal(record$summary$parameter_count, 6L)
  expect_equal(record$summary$df_residual, 26L)
  expect_true("GENERALIZED_COMPARISON_MODELS_V7" %in% captured,
              info = paste(head(captured, 30L), collapse = " | "))
  expect_true("two-factor-fingerprint-19" %in% captured)
  expect_true(all(c("cyl=4", "cyl=6", "cyl=8",
                    "gear=3", "gear=4", "gear=5") %in% captured))

  wrong <- list(
    response = "am", family = "binomial", link = "probit", scope = "all",
    terms = c("cyl", "mpg"),
    term_types = list(cyl = "factor", mpg = "numeric"),
    centered_predictors = character(), factor_reference_levels = list(cyl = "4")
  )
  expect_error(
    LinkEDA:::.rls_assert_generalized_fit_matches_specification(
      record, wrong, event = "1", reference = "0"
    ),
    "terms changed"
  )
  mixed_statistics <- record
  mixed_statistics$summary$parameter_count <- 4L
  mixed_statistics$summary$rank <- 4L
  mixed_statistics$summary$df_residual <- 28L
  expect_error(
    LinkEDA:::.rls_assert_generalized_fit_matches_specification(
      mixed_statistics,
      list(
        response = "am", family = "binomial", link = "probit", scope = "all",
        terms = c("cyl", "mpg", "gear"),
        term_types = list(cyl = "factor", mpg = "numeric", gear = "factor"),
        centered_predictors = character(), factor_reference_levels = list(cyl = "4")
      ),
      event = "1", reference = "0"
    ),
    "parameter count differs from the fitted design"
  )
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
  payload <- LinkEDA:::.rls_native_generalized_state_payload(state)
  expect_identical(payload[[11L]], "z")
  expect_true("BINARY_V4" %in% payload)
  expect_identical(LinkEDA:::.rls_native_wire_number(Inf), "Inf")
  expect_identical(LinkEDA:::.rls_native_wire_number(-Inf), "-Inf")
})

test_that("binary diagnostics preserve original row identity and labels", {
  data <- transform(mtcars, outcome = ifelse(am == 1, "event", "reference"))
  data$outcome[c(2, 7)] <- NA_character_
  model <- ls_new_binary_regression(data, "outcome", "wt", native = FALSE, name = "binary_diag")
  diagnostics <- ls_binary_regression_diagnostics(model)
  state <- ls_binary_regression_state(model)

  expect_identical(diagnostics$row_id, state$rows_used)
  expect_true(all(c("observed_label", "observed_binary", "fitted_response_scale",
                    "linear_predictor", "dunn_smyth_residual", "raw_residual",
                    "deviance_residual", "pearson_residual",
                    "leverage", "cooks_distance") %in% names(diagnostics)))
  expect_setequal(unique(diagnostics$observed_label), c("event", "reference"))
  expect_true(all(diagnostics$fitted >= 0 & diagnostics$fitted <= 1))
  expect_true(all(is.finite(diagnostics$dunn_smyth_residual)))
  expect_equal(diagnostics$residual, diagnostics$dunn_smyth_residual)
  expect_equal(diagnostics$raw_residual,
               diagnostics$observed_binary - diagnostics$fitted,
               tolerance = 1e-12)
  distribution <- state$summary$observed_predicted_distribution
  expect_equal(distribution$score, 0:1)
  expect_equal(sum(distribution$observed_frequency), state$summary$n_used)
  expect_equal(sum(distribution$predicted_frequency), state$summary$n_used,
               tolerance = 1e-10)
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

test_that("Binary Regression and the shared binomial GLM agree for every supported binary link", {
  data <- transform(mtcars, outcome = factor(am), cyl = factor(cyl))
  for (link in c("logit", "probit", "cloglog")) {
    binary <- ls_new_binary_regression(
      data, "outcome", c("wt", "cyl"), event = "1", link = link,
      native = FALSE, name = paste0("binary_shared_", link),
      term_types = list(cyl = "factor")
    )
    generalized <- ls_new_generalized_linear_model(
      data, "outcome", c("wt", "cyl"), family = "binomial", link = link,
      native = FALSE, name = paste0("generalized_shared_", link),
      term_types = list(cyl = "factor")
    )
    binary_state <- ls_binary_regression_state(binary)
    generalized_state <- ls_generalized_linear_model_state(generalized)

    expect_equal(stats::coef(binary_state$fit), stats::coef(generalized_state$fit), tolerance = 1e-10)
    expect_equal(stats::fitted(binary_state$fit), stats::fitted(generalized_state$fit), tolerance = 1e-10)
    expect_equal(binary_state$summary$n_used, generalized_state$summary$n_used)
    expect_equal(binary_state$summary$null_deviance, generalized_state$summary$null_deviance, tolerance = 1e-10)
    expect_equal(binary_state$summary$residual_deviance, generalized_state$summary$residual_deviance, tolerance = 1e-10)
    expect_equal(binary_state$summary$aic, generalized_state$summary$aic, tolerance = 1e-10)
    if (link == "logit") {
      expect_true(any(is.finite(ls_binary_regression_coefficients(binary)$odds_ratio)))
    } else {
      expect_true(all(is.na(ls_binary_regression_coefficients(binary)$odds_ratio)))
    }
  }
})

test_that("binary centering is model-local and preserves fitted probabilities", {
  data <- transform(mtcars, outcome = factor(am), cyl = factor(cyl))
  raw <- ls_new_binary_regression(
    data, "outcome", c("wt", "cyl"), event = "1", native = FALSE,
    name = "binary_raw_centering", term_types = list(cyl = "factor")
  )
  centered <- ls_new_binary_regression(
    data, "outcome", c("wt", "cyl"), event = "1", native = FALSE,
    name = "binary_centered", term_types = list(cyl = "factor"),
    centered_predictors = "wt"
  )
  raw_state <- ls_binary_regression_state(raw)
  centered_state <- ls_binary_regression_state(centered)

  expect_equal(stats::fitted(raw_state$fit), stats::fitted(centered_state$fit), tolerance = 1e-10)
  expect_equal(unname(stats::coef(raw_state$fit)["wt"]),
               unname(stats::coef(centered_state$fit)["wt"]), tolerance = 1e-10)
  expect_false(isTRUE(all.equal(unname(stats::coef(raw_state$fit)["(Intercept)"]),
                                unname(stats::coef(centered_state$fit)["(Intercept)"]))))
  expect_identical(centered_state$centered_predictors, "wt")
  expect_identical(raw_state$centered_predictors, character())
})

test_that("binary factor references and transformed interaction terms use the shared specification", {
  data <- transform(mtcars, outcome = factor(am), planet = factor(cyl))
  reference_a <- ls_new_binary_regression(
    data, "outcome", "wt:planet", event = "1", native = FALSE,
    name = "binary_reference_a", term_types = list(planet = "factor"),
    factor_reference_levels = list(planet = "4")
  )
  reference_b <- ls_new_binary_regression(
    data, "outcome", "wt:planet", event = "1", native = FALSE,
    name = "binary_reference_b", term_types = list(planet = "factor"),
    factor_reference_levels = list(planet = "6")
  )
  centered <- ls_new_binary_regression(
    data, "outcome", "wt:planet", event = "1", native = FALSE,
    name = "binary_reference_centered", term_types = list(planet = "factor"),
    factor_reference_levels = list(planet = "6"), centered_predictors = "wt"
  )
  a <- ls_binary_regression_state(reference_a)
  b <- ls_binary_regression_state(reference_b)
  c <- ls_binary_regression_state(centered)

  expect_identical(b$factor_reference_levels$planet, "6")
  expect_identical(c$centered_predictors, "wt")
  expect_equal(stats::fitted(a$fit), stats::fitted(b$fit), tolerance = 1e-10)
  expect_equal(stats::fitted(b$fit), stats::fitted(c$fit), tolerance = 1e-10)
  expect_equal(a$summary$residual_deviance, b$summary$residual_deviance, tolerance = 1e-10)
})

test_that("binary numeric-as-factor is a model-local term interpretation", {
  data <- transform(mtcars, outcome = factor(am))
  numeric_model <- ls_new_binary_regression(
    data, "outcome", "gear", event = "1", native = FALSE,
    name = "binary_gear_numeric"
  )
  factor_model <- ls_new_binary_regression(
    data, "outcome", "gear", event = "1", native = FALSE,
    name = "binary_gear_factor", term_types = list(gear = "factor")
  )
  numeric_state <- ls_binary_regression_state(numeric_model)
  factor_state <- ls_binary_regression_state(factor_model)

  expect_identical(factor_state$term_types$gear, "factor")
  expect_gt(length(stats::coef(factor_state$fit)), length(stats::coef(numeric_state$fit)))
  expect_identical(data$gear, mtcars$gear)
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

test_that("binary comparison accepts one intercept-only model", {
  data <- transform(mtcars, outcome = factor(am))
  intercept <- ls_new_binary_regression(
    data, "outcome", terms = NULL, event = "1", native = FALSE,
    name = "binary_comparison_intercept"
  )
  comparison <- ls_compare_binary_regression_models(intercept)

  expect_s3_class(comparison, "rlispstat_binary_regression_comparison")
  expect_equal(nrow(comparison$models), 1L)
  expect_equal(nrow(comparison$tests), 0L)
  expect_identical(comparison$event, "1")
  expect_identical(comparison$reference, "0")
})

test_that("binary comparison requires common response coding and nested matrices", {
  data <- transform(mtcars, outcome = factor(am))
  by_weight <- ls_new_binary_regression(
    data, "outcome", "wt", event = "1", native = FALSE,
    name = "binary_non_nested_weight"
  )
  group <- ls_binary_regression_state(by_weight)$group
  by_power <- ls_new_binary_regression(
    group, "outcome", "hp", event = "1", native = FALSE,
    name = "binary_non_nested_power"
  )
  non_nested <- ls_compare_binary_regression_models(by_weight, by_power)
  expect_false(non_nested$tests$available[[1L]])
  expect_match(non_nested$tests$reason[[1L]], "not nested")

  reverse_event <- ls_new_binary_regression(
    group, "outcome", "wt", event = "0", native = FALSE,
    name = "binary_reverse_event"
  )
  expect_error(
    ls_compare_binary_regression_models(by_weight, reverse_event),
    "event/reference coding"
  )
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

test_that("mtcars am changes from a numeric slope to factor coding", {
  source <- mtcars
  numeric_model <- suppressWarnings(ls_new_binary_regression(
    source, "vs", c("mpg", "hp", "am"), event = "1", native = FALSE,
    name = "binary_mtcars_am_numeric", term_types = list(am = "numeric")
  ))
  factor_model <- suppressWarnings(ls_new_binary_regression(
    source, "vs", c("mpg", "hp", "am"), event = "1", native = FALSE,
    name = "binary_mtcars_am_factor", term_types = list(am = "factor")
  ))
  numeric_state <- ls_binary_regression_state(numeric_model)
  factor_state <- ls_binary_regression_state(factor_model)
  numeric_columns <- colnames(stats::model.matrix(numeric_state$fit))
  factor_columns <- colnames(stats::model.matrix(factor_state$fit))
  am_rows <- factor_state$coefficient_rows[
    factor_state$coefficient_rows$source_term == "am", , drop = FALSE
  ]

  expect_true("am" %in% numeric_columns)
  expect_false("am" %in% factor_columns)
  expect_true("am1" %in% factor_columns)
  expect_true(is.factor(factor_state$fit$model$am))
  expect_true(all(c("factor_parent", "reference", "factor_level") %in% am_rows$row_type))
  expect_identical(source$am, mtcars$am)
})

test_that("mtcars cyl factor coding represents every observed level", {
  factor_model <- suppressWarnings(ls_new_binary_regression(
    mtcars, "vs", c("mpg", "hp", "cyl"), event = "1", native = FALSE,
    name = "binary_mtcars_cyl_factor", term_types = list(cyl = "factor")
  ))
  state <- ls_binary_regression_state(factor_model)
  columns <- colnames(stats::model.matrix(state$fit))
  cyl_rows <- state$coefficient_rows[
    state$coefficient_rows$source_term == "cyl", , drop = FALSE
  ]

  expect_false("cyl" %in% columns)
  expect_true(all(c("cyl6", "cyl8") %in% columns))
  expect_true(is.factor(state$fit$model$cyl))
  expect_identical(cyl_rows$level[cyl_rows$row_type == "reference"], "4")
  expect_true(all(c("6", "8") %in% cyl_rows$level))
})

test_that("mtcars changed cyl reference keeps mpg and the complete fitted specification", {
  model <- suppressWarnings(ls_new_binary_regression(
    mtcars, "vs", c("mpg", "cyl"), event = "1", reference = "0",
    native = FALSE, name = "binary_mtcars_reference_six",
    term_types = list(cyl = "factor"),
    factor_reference_levels = list(cyl = "6")
  ))
  state <- ls_binary_regression_state(model)
  direct_data <- transform(mtcars, cyl = stats::relevel(factor(cyl), ref = "6"))
  direct <- stats::glm(vs ~ mpg + cyl, direct_data, family = stats::binomial("logit"))
  cyl_rows <- state$coefficient_rows[
    state$coefficient_rows$source_term == "cyl", , drop = FALSE
  ]

  expect_identical(state$terms, c("mpg", "cyl"))
  expect_identical(state$term_types$cyl, "factor")
  expect_identical(state$factor_reference_levels$cyl, "6")
  expect_identical(cyl_rows$level[cyl_rows$row_type == "reference"], "6")
  expect_equal(stats::coef(state$fit), stats::coef(direct), tolerance = 1e-10)
  expect_equal(state$summary$parameter_count, 4L)
  expect_equal(state$summary$df_residual, 28L)
  expect_equal(state$summary$residual_deviance,
               unname(stats::deviance(direct)), tolerance = 1e-10)
})

test_that("mtcars am and cyl remain factors simultaneously", {
  source <- mtcars
  model <- suppressWarnings(ls_new_binary_regression(
    source, "vs", c("mpg", "hp", "am", "cyl"), event = "1", native = FALSE,
    name = "binary_mtcars_two_factors",
    term_types = list(am = "factor", cyl = "factor")
  ))
  state <- ls_binary_regression_state(model)
  columns <- colnames(stats::model.matrix(state$fit))

  expect_identical(state$term_types[c("am", "cyl")],
                   list(am = "factor", cyl = "factor"))
  expect_true(all(c("am1", "cyl6", "cyl8") %in% columns))
  expect_false(any(c("am", "cyl") %in% columns))
  expect_true(all(vapply(state$fit$model[c("am", "cyl")], is.factor, logical(1L))))
  expect_true(all(c("am", "cyl") %in%
                    state$coefficient_rows$source_term[
                      state$coefficient_rows$row_type == "factor_parent"
                    ]))
  expect_identical(source[c("am", "cyl")], mtcars[c("am", "cyl")])
})

test_that("mtcars comparison specification with cyl and gear factors matches the exact R fit", {
  fit_linkeda <- function(link = "probit", terms = c("cyl", "mpg", "gear"),
                          types = list(cyl = "factor", mpg = "numeric", gear = "factor"),
                          references = list(cyl = "4"), name = NULL) {
    suppressWarnings(ls_new_binary_regression(
      mtcars, "am", terms, event = "1", reference = "0", link = link,
      native = FALSE,
      name = if (is.null(name)) paste0("binary_exact_two_factors_", link) else name,
      term_types = types, factor_reference_levels = references
    ))
  }

  model <- fit_linkeda()
  state <- ls_binary_regression_state(model)
  direct_data <- mtcars
  direct_data$cyl <- stats::relevel(factor(direct_data$cyl), ref = "4")
  direct_data$gear <- factor(direct_data$gear)
  direct <- suppressWarnings(stats::glm(
    am ~ cyl + mpg + gear, data = direct_data,
    family = stats::binomial(link = "probit")
  ))
  actual_summary <- unclass(summary(state$fit)$coefficients)
  direct_summary <- unclass(summary(direct)$coefficients)
  hierarchy <- state$coefficient_rows[, c(
    "term", "source_term", "term_type", "row_type", "level", "reference_level"
  )]
  witness <- LinkEDA:::.rls_assert_generalized_fit_matches_specification(
    state,
    list(
      response = "am", family = "binomial", link = "probit", scope = "all",
      terms = c("cyl", "mpg", "gear"),
      term_types = list(cyl = "factor", mpg = "numeric", gear = "factor"),
      centered_predictors = character(), factor_reference_levels = list(cyl = "4")
    ),
    event = "1", reference = "0"
  )

  expect_identical(state$terms, c("cyl", "mpg", "gear"))
  expect_identical(state$term_types,
                   list(cyl = "factor", mpg = "numeric", gear = "factor"))
  expect_identical(colnames(stats::model.matrix(state$fit)),
                   c("(Intercept)", "cyl6", "cyl8", "mpg", "gear4", "gear5"))
  expect_identical(witness$coefficient_columns,
                   list(cyl = c("cyl6", "cyl8"), mpg = "mpg",
                        gear = c("gear4", "gear5")))
  expect_equal(state$summary$parameter_count, 6L)
  expect_equal(state$summary$rank, 6L)
  expect_equal(state$summary$df_residual, 26L)
  expect_identical(rownames(actual_summary), rownames(direct_summary))
  expect_equal(actual_summary[, c("Estimate", "Std. Error", "z value", "Pr(>|z|)")],
               direct_summary[, c("Estimate", "Std. Error", "z value", "Pr(>|z|)")],
               tolerance = 1e-10)
  expect_equal(state$summary$residual_deviance, stats::deviance(direct), tolerance = 1e-10)
  expect_equal(stats::fitted(state$fit), stats::fitted(direct), tolerance = 1e-10)
  expect_identical(hierarchy$level[
    hierarchy$source_term == "cyl" & hierarchy$row_type == "reference"
  ], "4")
  expect_true(all(c("6", "8") %in% hierarchy$level[hierarchy$source_term == "cyl"]))
  expect_length(hierarchy$level[
    hierarchy$source_term == "gear" & hierarchy$row_type == "reference"
  ], 1L)
  expect_equal(sum(hierarchy$source_term == "gear" &
                     hierarchy$row_type == "factor_level"), 2L)

  # Exercise the same construction after sequential additions, removals,
  # factor/numeric/factor transitions, reference changes and every native
  # binary link. Each accepted state is checked against its own R design.
  sequential <- list(
    list(terms = c("mpg", "cyl"),
         types = list(mpg = "numeric", cyl = "factor"), refs = list(cyl = "4")),
    list(terms = c("mpg", "cyl", "gear"),
         types = list(mpg = "numeric", cyl = "factor", gear = "factor"), refs = list(cyl = "4")),
    list(terms = c("mpg", "cyl"),
         types = list(mpg = "numeric", cyl = "factor"), refs = list(cyl = "4")),
    list(terms = c("mpg", "gear"),
         types = list(mpg = "numeric", gear = "factor"), refs = list()),
    list(terms = c("mpg", "cyl", "gear"),
         types = list(mpg = "numeric", cyl = "factor", gear = "factor"), refs = list(cyl = "4")),
    list(terms = c("mpg", "cyl", "gear"),
         types = list(mpg = "numeric", cyl = "numeric", gear = "factor"), refs = list()),
    list(terms = c("mpg", "cyl", "gear"),
         types = list(mpg = "numeric", cyl = "factor", gear = "factor"), refs = list(cyl = "8"))
  )
  for (index in seq_along(sequential)) {
    specification <- sequential[[index]]
    current <- ls_binary_regression_state(fit_linkeda(
      terms = specification$terms, types = specification$types,
      references = specification$refs, name = paste0("binary_sequential_", index)
    ))
    expect_identical(current$terms, specification$terms)
    expect_identical(current$term_types, specification$types)
    expect_silent(LinkEDA:::.rls_assert_generalized_fit_matches_specification(
      current,
      list(response = "am", family = "binomial", link = "probit", scope = "all",
           terms = specification$terms, term_types = specification$types,
           centered_predictors = character(),
           factor_reference_levels = specification$refs),
      event = "1", reference = "0"
    ))
  }
  for (link in c("logit", "probit", "cloglog")) {
    linked <- ls_binary_regression_state(fit_linkeda(
      link = link, name = paste0("binary_exact_two_factors_link_", link)
    ))
    expect_equal(linked$summary$parameter_count, 6L)
    expect_equal(linked$summary$df_residual, 26L)
    expect_identical(colnames(stats::model.matrix(linked$fit)),
                     c("(Intercept)", "cyl6", "cyl8", "mpg", "gear4", "gear5"))
  }
})

test_that("binary factor reference changes parameterization but not the fit", {
  fit_with <- function(name, references) suppressWarnings(ls_new_binary_regression(
    mtcars, "vs", c("mpg", "am", "cyl"), event = "1", native = FALSE,
    name = name, term_types = list(am = "factor", cyl = "factor"),
    factor_reference_levels = references
  ))
  first <- ls_binary_regression_state(fit_with(
    "binary_mtcars_reference_first", list(am = "0", cyl = "4")
  ))
  changed <- ls_binary_regression_state(fit_with(
    "binary_mtcars_reference_changed", list(am = "1", cyl = "8")
  ))
  reference_rows <- changed$coefficient_rows[
    changed$coefficient_rows$row_type == "reference", , drop = FALSE
  ]

  expect_false(identical(names(stats::coef(first$fit)), names(stats::coef(changed$fit))))
  expect_equal(stats::fitted(first$fit), stats::fitted(changed$fit), tolerance = 1e-10)
  expect_equal(first$summary$residual_deviance,
               changed$summary$residual_deviance, tolerance = 1e-10)
  expect_equal(first$summary$log_lik, changed$summary$log_lik, tolerance = 1e-10)
  expect_identical(reference_rows$level[reference_rows$source_term == "am"], "1")
  expect_identical(reference_rows$level[reference_rows$source_term == "cyl"], "8")
})

test_that("binary factor by factor interaction uses categorical design columns", {
  model <- suppressWarnings(ls_new_binary_regression(
    mtcars, "vs", c("mpg", "am:cyl"), event = "1", native = FALSE,
    name = "binary_mtcars_factor_interaction",
    term_types = list(am = "factor", cyl = "factor"),
    factor_reference_levels = list(am = "0", cyl = "4")
  ))
  state <- ls_binary_regression_state(model)
  columns <- colnames(stats::model.matrix(state$fit))
  interaction_rows <- state$coefficient_rows[
    state$coefficient_rows$source_term == "am:cyl", , drop = FALSE
  ]

  expect_true(all(c("am", "cyl", "am:cyl") %in% state$terms))
  expect_true(all(c("am1:cyl6", "am1:cyl8") %in% columns))
  expect_true(any(interaction_rows$row_type == "term_parent"))
  expect_true(all(interaction_rows$term_type == "factor_factor_interaction"))
})

test_that("binary predictor interpretation round trips numeric factor numeric", {
  source <- mtcars
  model <- suppressWarnings(ls_new_binary_regression(
    source, "vs", c("mpg", "am"), event = "1", native = FALSE,
    name = "binary_mtcars_type_roundtrip", term_types = list(am = "numeric")
  ))
  numeric_state <- ls_binary_regression_state(model)
  numeric_columns <- colnames(stats::model.matrix(numeric_state$fit))

  factor_record <- numeric_state
  factor_record$term_types <- list(am = "factor")
  model <- LinkEDA:::.rls_assign_generalized_glm(factor_record)
  model <- suppressWarnings(ls_binary_regression_fit(model))
  factor_state <- ls_binary_regression_state(model)

  numeric_record <- factor_state
  numeric_record$term_types <- list(am = "numeric")
  numeric_record$factor_reference_levels <- list()
  model <- LinkEDA:::.rls_assign_generalized_glm(numeric_record)
  model <- suppressWarnings(ls_binary_regression_fit(model))
  restored_state <- ls_binary_regression_state(model)

  expect_false(identical(colnames(stats::model.matrix(factor_state$fit)), numeric_columns))
  expect_identical(colnames(stats::model.matrix(restored_state$fit)), numeric_columns)
  expect_true(is.numeric(restored_state$fit$model$am))
  expect_false(any(restored_state$coefficient_rows$row_type %in%
                     c("factor_parent", "reference", "factor_level")))
  expect_identical(source$am, mtcars$am)
})

test_that("native binary tasks forward the shared model specification", {
  received <- NULL
  testthat::local_mocked_bindings(
    ls_new_binary_regression = function(...) {
      received <<- list(...)
      invisible(NULL)
    },
    .package = "LinkEDA"
  )

  LinkEDA:::.rls_handle_generalized_glm_needed(
    id = "binary_native_spec", group = "mtcars", response = "vs",
    family = "binomial", link = "logit", scope = "all",
    terms = c("mpg", "am", "cyl"),
    term_types = list(am = "factor", cyl = "factor"),
    centered_predictors = "mpg",
    factor_reference_levels = list(am = "1", cyl = "6"),
    binary = TRUE, event = "1", reference = "0"
  )

  expect_identical(received$term_types, list(am = "factor", cyl = "factor"))
  expect_identical(received$centered_predictors, "mpg")
  expect_identical(received$factor_reference_levels, list(am = "1", cyl = "6"))
})

test_that("log-binomial uses R's log link and reports risk ratios", {
  response <- c(rep("no", 80), rep("yes", 20),
                rep("no", 60), rep("yes", 40))
  data <- data.frame(
    response = factor(response, levels = c("no", "yes")),
    exposed = factor(rep(c("no", "yes"), each = 100),
                     levels = c("no", "yes"))
  )
  model <- suppressWarnings(ls_new_binary_regression(
    data, "response", "exposed", event = "yes", link = "log",
    native = FALSE, name = "binary_log_public"
  ))
  state <- ls_binary_regression_state(model)
  direct <- stats::glm(
    I(response == "yes") ~ exposed,
    data = data, family = stats::binomial(link = "log")
  )

  expect_s3_class(state$fit, "glm")
  expect_identical(state$fit$family$family, "binomial")
  expect_identical(state$fit$family$link, "log")
  expect_equal(stats::coef(state$fit), stats::coef(direct), tolerance = 1e-8)
  expect_identical(unique(state$coefficients$exponentiated_label), "Risk ratio")
  expect_equal(state$coefficients$exponentiated_estimate,
               exp(state$coefficients$estimate), tolerance = 1e-12)
  expect_true(all(is.na(state$coefficients$odds_ratio)))
  expect_true(all(stats::fitted(state$fit) >= 0 & stats::fitted(state$fit) <= 1))

  if (requireNamespace("emmeans", quietly = TRUE)) {
    pairwise <- ls_binary_regression_pairwise(model, "exposed")
    expect_identical(unique(pairwise$scale), "risk_ratio")
    expect_equal(pairwise$risk_ratio, exp(pairwise$pooled_estimate),
                 tolerance = 1e-12)
    effect <- ls_binary_regression_interaction(model, "exposed")
    expect_identical(unique(effect$plot_data$quantity), "predicted_probability")
    expect_true(all(effect$plot_data$pooled_estimate >= 0 &
                    effect$plot_data$pooled_estimate <= 1))
  }

  broken <- state$fit
  broken$converged <- FALSE
  expect_error(
    LinkEDA:::.rls_binary_validate_log_fit(broken, "log"),
    "has not substituted a different model or link"
  )
})
