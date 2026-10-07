test_that("beta regression rescales a bounded response and reports original-scale fits", {
  skip_if_not_installed("betareg")
  set.seed(421)
  data <- data.frame(x = stats::rnorm(90))
  data$score <- 100 * stats::plogis(-0.4 + 0.8 * data$x + stats::rnorm(90, sd = 0.45))

  model <- ls_new_generalized_linear_model(
    data, response = "score", terms = "x", family = "beta",
    response_bounds = c(0, 100), native = FALSE
  )
  state <- ls_generalized_linear_model_state(model)

  expect_s3_class(state$fit, "betareg")
  expect_identical(state$family, "beta")
  expect_identical(state$link, "logit")
  expect_equal(state$response_bounds, c(0, 100))
  expect_true(state$summary$converged)
  expect_gt(state$coefficients$estimate[state$coefficients$term == "x"], 0)
  expect_equal(state$diagnostics$observed, data$score)
  expect_true(all(state$diagnostics$fitted_response_scale > 0))
  expect_true(all(state$diagnostics$fitted_response_scale < 100))
})

test_that("beta regression displays imported value labels instead of storage codes", {
  skip_if_not_installed("betareg")
  labelled <- function(values, labels) {
    structure(as.double(values), labels = labels,
              class = c("haven_labelled", "vctrs_vctr", "double"))
  }
  data <- expand.grid(gender = 1:2, country = 1:3, replicate = seq_len(15L))
  data$gender <- labelled(data$gender, c(Female = 1, Male = 2))
  data$country <- labelled(data$country, c(Finland = 1, Greece = 2, Italy = 3))
  set.seed(4211)
  data$score <- stats::plogis(stats::rnorm(nrow(data)))

  model <- ls_new_generalized_linear_model(
    data, "score", c("gender", "country", "gender:country"),
    family = "beta", response_bounds = c(0, 1), native = FALSE,
    term_types = list(gender = "factor", country = "factor",
                      `gender:country` = "factor")
  )
  rows <- ls_generalized_linear_model_coefficient_rows(model)
  expect_true(all(c("  Female", "  Male", "  Finland", "  Greece", "  Italy") %in%
                    rows$display_label))
  interaction <- rows[
    rows$source_term == "gender:country" & rows$row_type == "coefficient",
    "display_label", drop = TRUE
  ]
  expect_true(any(grepl("Male × Greece", interaction, fixed = TRUE)))
  expect_false(any(grepl("2 × 2", interaction, fixed = TRUE)))
})

test_that("one-inflated beta models exact upper-bound observations", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  set.seed(422)
  data <- data.frame(x = stats::rnorm(100))
  data$score <- 100 * stats::plogis(-0.2 + 0.5 * data$x + stats::rnorm(100, sd = 0.6))
  data$score[seq_len(20)] <- 100

  model <- ls_new_generalized_linear_model(
    data, response = "score", terms = "x", family = "beta_one_inflated",
    response_bounds = c(0, 100), native = FALSE
  )
  state <- ls_generalized_linear_model_state(model)

  expect_s3_class(state$fit, "gamlss")
  expect_true(state$summary$converged)
  expect_equal(state$summary$inflation_probability, 0.2, tolerance = 0.02)
  expect_true(all(state$diagnostics$fitted_response_scale > 0))
  expect_true(all(state$diagnostics$fitted_response_scale <= 100))
})

test_that("Gamma distance regression fits upper minus response", {
  set.seed(423)
  data <- data.frame(x = stats::rnorm(100))
  distance <- stats::rgamma(100, shape = 7, rate = exp(1.7 - 0.35 * data$x))
  data$score <- 100 - distance

  model <- ls_new_generalized_linear_model(
    data, response = "score", terms = "x", family = "gamma_distance",
    response_bounds = c(0, 100), native = FALSE
  )
  state <- ls_generalized_linear_model_state(model)

  expect_s3_class(state$fit, "glm")
  expect_identical(state$fit$family$family, "gamma_distance")
  expect_identical(state$link, "log")
  expect_equal(state$diagnostics$observed, data$score)
  expect_true(all(state$diagnostics$fitted_response_scale < 100))
})

test_that("bounded families reject incompatible bounds and endpoints", {
  data <- data.frame(score = c(0, 25, 50, 75, 100), x = seq_len(5))
  expect_error(
    ls_new_generalized_linear_model(
      data, "score", "x", family = "beta", native = FALSE
    ),
    "response_bounds"
  )
  expect_error(
    ls_new_generalized_linear_model(
      data, "score", "x", family = "beta", response_bounds = c(0, 100), native = FALSE
    ),
    "strictly between"
  )
  expect_error(
    ls_new_generalized_linear_model(
      data, "score", "x", family = "beta_one_inflated",
      response_bounds = c(0, 100), native = FALSE
    ),
    "not at the lower bound"
  )
  expect_error(
    ls_new_generalized_linear_model(
      data, "score", "x", family = "gamma_distance",
      response_bounds = c(0, 100), native = FALSE
    ),
    "Y < upper"
  )
})

test_that("response bounds can be configured before switching families", {
  skip_if_not_installed("betareg")
  data <- data.frame(score = c(15, 25, 40, 55, 70, 85), x = seq_len(6))
  model <- ls_new_generalized_linear_model(
    data, "score", "x", family = "gaussian", native = FALSE
  )
  model <- ls_generalized_linear_model_set_response_bounds(model, 0, 100)
  model <- ls_generalized_linear_model_set_family(model, "beta")
  state <- ls_generalized_linear_model_state(model)
  expect_equal(state$response_bounds, c(0, 100))
  expect_s3_class(state$fit, "betareg")
})

.make_bounded_mi_dataset <- function(name, completed) {
  id <- ls_register_dataset(name, completed[[1L]])
  record <- LinkEDA:::.rls_dataset_record(id)
  original <- completed[[1L]]
  original$x[seq(7L, nrow(original), by = 19L)] <- NA_real_
  record$dataset_type <- "multiple_imputation"
  record$imputation_id <- paste0(name, "_imp")
  record$source_dataset_id <- paste0(name, "_source")
  record$original_data <- original
  record$completed_datasets <- completed
  record$missing_cell_mask <- as.data.frame(lapply(original, is.na), stringsAsFactors = FALSE)
  record$imputation_count <- length(completed)
  record$original_row_ids <- seq_len(nrow(original))
  LinkEDA:::.rls_set_dataset_record(record)
  LinkEDA:::.rls_store_data_version(record)
  id
}

test_that("bounded-response models pool every multiple imputation", {
  skip_if_not_installed("mice")
  skip_if_not_installed("broom")
  skip_if_not_installed("betareg")
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  set.seed(424)
  n <- 120L
  x <- stats::rnorm(n)
  group <- factor(rep(c("Control", "Control", "Treatment", "Treatment"), length.out = n))
  country <- factor(rep(c("Cyprus", "Italy", "Cyprus", "Italy"), length.out = n))
  beta_score <- 100 * stats::plogis(-0.25 + 0.7 * x + stats::rnorm(n, sd = 0.45))
  beta_completed <- lapply(seq_len(3L), function(version) {
    data.frame(
      score = pmin(99.9, pmax(0.1, beta_score + (version - 2L) * 0.04)),
      x = x + stats::rnorm(n, sd = 0.01),
      group = group,
      country = country
    )
  })
  beta_id <- .make_bounded_mi_dataset("bounded_beta_mi", beta_completed)
  on.exit(ls_unregister_dataset(beta_id), add = TRUE)
  beta <- ls_generalized_linear_model_state(ls_new_generalized_linear_model(
    beta_id, "score", "x", family = "beta", response_bounds = c(0, 100),
    native = FALSE
  ))
  expect_identical(beta$analysis_backend, "multiple_imputation")
  expect_length(beta$fits_by_imputation, 3L)
  expect_true(all(vapply(beta$fits_by_imputation, inherits, logical(1L), "betareg")))
  expect_match(beta$multiple_imputation$pooling_method, "pool.scalar", fixed = TRUE)
  expect_true(all(is.finite(beta$coefficients$total_variance)))
  expect_equal(beta$diagnostics$observed, beta_completed[[1L]]$score)
  expect_true(all(beta$diagnostics$fitted_response_scale > 0 &
                    beta$diagnostics$fitted_response_scale < 100))
  beta_proportion_completed <- lapply(beta_completed, function(data) {
    transform(data, score = score / 100)
  })
  beta_proportion_id <- .make_bounded_mi_dataset(
    "bounded_beta_proportion_mi", beta_proportion_completed
  )
  on.exit(ls_unregister_dataset(beta_proportion_id), add = TRUE)
  beta_reduced <- ls_new_proportion_model(
    beta_proportion_id, "score", "x", distribution = "beta", native = FALSE,
    name = "bounded_beta_mi_reduced"
  )
  beta_full <- ls_new_proportion_model(
    beta_proportion_id, "score", c("x", "group"), distribution = "beta",
    native = FALSE,
    name = "bounded_beta_mi_full"
  )
  beta_comparison <- ls_compare_generalized_linear_models(
    beta_reduced, beta_full, native = FALSE, .model_type = "proportion"
  )
  expect_true(beta_comparison$tests$available[[1L]],
              info = beta_comparison$tests$reason[[1L]])
  expect_match(beta_comparison$tests$method[[1L]], "D1", fixed = TRUE)
  beta$native_generation <- 17L
  beta_payload <- LinkEDA:::.rls_generalized_glm_pooled_native_payload(beta)
  bounds_marker <- match("BOUNDED_RESPONSE_V1", beta_payload)
  expect_true(is.finite(bounds_marker))
  expect_equal(beta_payload[bounds_marker + 1:2], c("0", "100"))
  result_marker <- match("GGLM_RESULT_V1", beta_payload)
  expect_true(is.finite(result_marker))
  expect_identical(beta_payload[result_marker + c(0L, 1L)], c("GGLM_RESULT_V1", "17"))
  expect_true("ANALYSIS_PROVENANCE_V2" %in% beta_payload)

  beta_interaction_model <- ls_new_generalized_linear_model(
    beta_id, "score", c("group", "country", "group:country"),
    family = "beta", response_bounds = c(0, 100), native = FALSE
  )
  beta_interaction <- ls_generalized_linear_model_interaction(
    beta_interaction_model, "group:country"
  )
  expect_identical(beta_interaction$imputation_count, 3L)
  expect_equal(nrow(beta_interaction$estimates), 4L)
  expect_equal(nrow(beta_interaction$comparisons), 2L)
  expect_equal(nrow(beta_interaction$interaction_contrasts), 1L)
  expect_true(all(is.finite(beta_interaction$estimates$pooled_estimate)))

  inflated_score <- beta_score
  inflated_score[seq_len(24L)] <- 100
  inflated_completed <- lapply(seq_len(3L), function(version) {
    data.frame(
      score = ifelse(
        inflated_score == 100, 100,
        pmin(99.9, pmax(0.1, inflated_score + (version - 2L) * 0.03))
      ),
      x = x + stats::rnorm(n, sd = 0.01),
      z = rep(c(0, 1), length.out = n)
    )
  })
  inflated_id <- .make_bounded_mi_dataset("bounded_inflated_mi", inflated_completed)
  on.exit(ls_unregister_dataset(inflated_id), add = TRUE)
  inflated <- ls_generalized_linear_model_state(ls_new_generalized_linear_model(
    inflated_id, "score", "x", family = "beta_one_inflated",
    response_bounds = c(0, 100), native = FALSE
  ))
  expect_identical(inflated$analysis_backend, "multiple_imputation")
  expect_true(all(vapply(inflated$fits_by_imputation, inherits, logical(1L), "gamlss")))
  expect_match(inflated$multiple_imputation$pooling_method, "pool.scalar", fixed = TRUE)
  expect_equal(inflated$summary$inflation_probability, 0.2, tolerance = 0.025)
  expect_true(all(inflated$diagnostics$fitted_response_scale <= 100))
  inflated_proportion_completed <- lapply(inflated_completed, function(data) {
    transform(data, score = score / 100)
  })
  inflated_proportion_id <- .make_bounded_mi_dataset(
    "bounded_inflated_proportion_mi", inflated_proportion_completed
  )
  on.exit(ls_unregister_dataset(inflated_proportion_id), add = TRUE)
  inflated_reduced <- ls_new_proportion_model(
    inflated_proportion_id, "score", "x",
    distribution = "beta_one_inflated", native = FALSE,
    name = "bounded_inflated_mi_reduced"
  )
  inflated_full <- ls_new_proportion_model(
    inflated_proportion_id, "score", c("x", "z"),
    distribution = "beta_one_inflated", native = FALSE,
    name = "bounded_inflated_mi_full"
  )
  inflated_comparison <- ls_compare_generalized_linear_models(
    inflated_reduced, inflated_full, native = FALSE, .model_type = "proportion"
  )
  expect_true(inflated_comparison$tests$available[[1L]],
              info = inflated_comparison$tests$reason[[1L]])
  expect_match(inflated_comparison$tests$method[[1L]], "D1", fixed = TRUE)

  distance <- stats::rgamma(n, shape = 7, rate = exp(1.8 - 0.3 * x))
  distance_completed <- lapply(seq_len(3L), function(version) {
    data.frame(
      score = 100 - pmax(0.01, distance + (version - 2L) * 0.02),
      x = x + stats::rnorm(n, sd = 0.01)
    )
  })
  distance_id <- .make_bounded_mi_dataset("bounded_distance_mi", distance_completed)
  on.exit(ls_unregister_dataset(distance_id), add = TRUE)
  gamma_distance <- ls_generalized_linear_model_state(ls_new_generalized_linear_model(
    distance_id, "score", "x", family = "gamma_distance",
    response_bounds = c(0, 100), native = FALSE
  ))
  expect_identical(gamma_distance$analysis_backend, "multiple_imputation")
  expect_true(all(vapply(gamma_distance$fits_by_imputation, inherits, logical(1L), "glm")))
  expect_match(gamma_distance$multiple_imputation$pooling_method, "mice::pool", fixed = TRUE)
  expect_equal(gamma_distance$diagnostics$observed, distance_completed[[1L]]$score)
  expect_true(all(gamma_distance$diagnostics$fitted_response_scale < 100))
})
