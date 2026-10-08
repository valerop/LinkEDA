.partial_test_data <- function() {
  set.seed(916)
  n <- 100L
  x <- rnorm(n)
  g <- factor(rep(c("A", "B"), length.out = n))
  trials <- rep(c(10L, 15L), length.out = n)
  y <- rbinom(n, trials, rbeta(n, 3, 2))
  data.frame(y, x, g, trials)
}

.partial_check <- function(state, term, residual_type) {
  result <- LinkEDA:::.rls_regression_partial_plot_record(state, term, residual_type)
  diagnostics <- state$diagnostics
  expect_equal(result$rows$row_id, diagnostics$row_id)
  expect_equal(result$rows$partial_residual - result$rows$contribution,
               diagnostics[[paste0(residual_type, "_residual")]], tolerance = 1e-10)
  expect_true(all(is.finite(result$rows$contribution)))
  expect_equal(mean(result$rows$contribution), 0, tolerance = 1e-10)
  expect_true(residual_type %in% result$available_residual_types)
  result
}

.partial_replay <- function(result, data) {
  path <- tempfile(fileext = ".rds")
  saveRDS(data, path)
  on.exit(unlink(path), add = TRUE)
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  code <- result$analysis_provenance$verification_r_code$partial
  expect_false(grepl("LinkEDA:::", code, fixed = TRUE))
  replay <- new.env(parent = globalenv())
  replay$verification_data_path <- path
  invisible(capture.output(eval(parse(text = code), replay)))
  replay$partial_data
}

test_that("partial plots use fitted residuals for every count distribution", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("gamlss")
  data <- .partial_test_data()
  data$x[c(3, 18)] <- NA_real_
  for (distribution in c("poisson", "quasipoisson", "negative_binomial",
                         "binomial_trials", "beta_binomial",
                         "hurdle_beta_binomial_ceiling", "perfect_score")) {
    model <- ls_new_count_regression(data, "y", c("x", "g"),
      distribution = distribution,
      trials = if (distribution %in% c("poisson", "quasipoisson", "negative_binomial")) NULL else "trials",
      native = FALSE, name = paste0("partial_count_", distribution))
    state <- ls_count_regression_state(model)
    for (residual in LinkEDA:::.rls_discrete_diagnostic_capabilities(state)$residuals) {
      for (term in c("x", "g")) .partial_check(state, term, residual)
    }
    result <- LinkEDA:::.rls_regression_partial_plot_record(state, "g", "pearson")
    replay <- .partial_replay(result, data)
    expect_equal(replay$contribution, result$rows$contribution, tolerance = 1e-5)
    expect_equal(replay$partial_residual, result$rows$partial_residual, tolerance = 1e-4)
    if (distribution == "beta_binomial") {
      expect_error(LinkEDA:::.rls_regression_partial_plot_record(state, "x", "working"),
                   "not available")
      expect_error(LinkEDA:::.rls_regression_partial_plot_record(state, "x", "deviance"),
                   "not available")
      dunn <- LinkEDA:::.rls_regression_partial_plot_record(state, "x", "dunn_smyth")
      check <- .partial_replay(dunn, data)
      expect_true(all(is.finite(check$partial_residual)))
      expect_match(dunn$analysis_provenance$verification_r_code$partial,
                   "diagnostic_beta_binomial_residual", fixed = TRUE)
      expect_match(dunn$analysis_provenance$verification_r_code$partial,
                   "diagnostic_row_ids", fixed = TRUE)
    }
  }
})

test_that("beta and inflated-beta partial plots use their mean submodel", {
  skip_if_not_installed("betareg")
  skip_if_not_installed("gamlss")
  data <- .partial_test_data()
  data$y <- (data$y + .5) / (data$trials + 1)
  for (distribution in c("beta", "beta_one_inflated")) {
    if (distribution == "beta_one_inflated") data$y[1:10] <- 1
    invisible(capture.output(model <- ls_new_proportion_model(
      data, "y", c("x", "g"), distribution = distribution,
      native = FALSE, name = paste0("partial_proportion_", distribution))))
    state <- ls_generalized_linear_model_state(model)
    for (term in c("x", "g")) {
      result <- .partial_check(state, term, "deviance")
      replay <- .partial_replay(result, data)
      expect_equal(replay$contribution, result$rows$contribution, tolerance = 1e-6)
      # GAMLSS randomizes quantile residuals at the inflated boundary.
      if (distribution == "beta") {
        expect_equal(replay$partial_residual, result$rows$partial_residual, tolerance = 1e-6)
      }
    }
  }
})

test_that("MI beta-binomial partial plots preserve imputations and original cases", {
  skip_if_not_installed("mice")
  skip_if_not_installed("glmmTMB")
  data <- .partial_test_data()
  completed <- lapply(1:3, function(i) {d <- data; d$x[2:6] <- d$x[2:6] + i / 8; d})
  id <- ls_register_dataset("partial_beta_binomial_mi", data)
  on.exit(ls_unregister_dataset(id), add = TRUE)
  dataset <- LinkEDA:::.rls_dataset_record(id)
  dataset$dataset_type <- "multiple_imputation"
  dataset$imputation_id <- "partial_bb_imp"
  dataset$imputation_count <- length(completed)
  dataset$completed_datasets <- completed
  dataset$original_data <- data
  dataset$original_data$x[2:6] <- NA_real_
  dataset$original_row_ids <- seq_len(nrow(data))
  dataset$missing_cell_mask <- as.data.frame(lapply(dataset$original_data, is.na))
  LinkEDA:::.rls_set_dataset_record(dataset)
  selected <- c(2:6, 11:85)
  model <- ls_new_count_regression(id, "y", c("x", "g"), distribution = "beta_binomial",
    trials = "trials", scope = "selected", .selected_rows = selected,
    native = FALSE, name = "partial_bb_mi_fit")
  state <- ls_count_regression_state(model)
  result <- LinkEDA:::.rls_regression_partial_plot_record(state, "x", "pearson")
  expect_identical(result$imputation_count, 3L)
  for (i in 1:3) {
    points <- result$rows[result$rows$imputation == i, ]
    expect_equal(points$row_id, selected)
    expect_equal(points$partial_residual - points$contribution,
                 state$diagnostics_by_imputation[[i]]$pearson_residual, tolerance = 1e-10)
  }
  exported <- do.call(rbind, lapply(1:3, function(i) {
    d <- completed[[i]][selected, ]; d$.imp <- i; d$.id <- selected; d
  }))
  replay <- .partial_replay(result, exported)
  expect_equal(replay$contribution, result$rows$contribution, tolerance = 1e-5)
  expect_equal(replay$partial_residual, result$rows$partial_residual, tolerance = 1e-4)
  payload <- LinkEDA:::.rls_partial_native_plot_payload(result)
  expect_true(any(grepl("^RESIDUALS\\tdunn_smyth", payload)))
  expect_true("ANALYSIS_PROVENANCE_V2" %in% payload)
})

test_that("binary and positive-continuous engines open partial plots", {
  # Balanced cells give all links a valid starting fit, including log-binomial.
  data <- data.frame(x = rep(c(-1, 0, 1), 40),
    g = factor(rep(c("A", "B"), each = 3, length.out = 120)),
    binary = factor(rep(c(rep("yes", 6), rep("no", 18)), 5)))
  for (link in c("logit", "log", "probit", "cloglog")) {
    model <- ls_new_binary_regression(data, "binary", c("x", "g"), link = link,
      event = "yes", native = FALSE, name = paste0("partial_binary_", link))
    state <- ls_generalized_linear_model_state(model)
    result <- .partial_check(state, "x", "dunn_smyth")
    result <- .partial_check(state, "g", "pearson")
    replay <- .partial_replay(result, data)
    expect_equal(replay$partial_residual, result$rows$partial_residual, tolerance = 1e-5)
  }
  data <- .partial_test_data()
  data$positive <- 1 + data$y / data$trials
  for (distribution in c("Gamma", "inverse.gaussian", "gaussian_log", "lognormal")) {
    model <- ls_new_positive_continuous_model(data, "positive", c("x", "g"),
      distribution = distribution, native = FALSE,
      name = paste0("partial_positive_", distribution))
    state <- ls_generalized_linear_model_state(model)
    result <- .partial_check(state, "x", "deviance")
    replay <- .partial_replay(result, data)
    expect_equal(replay$partial_residual, result$rows$partial_residual, tolerance = 1e-6)
  }
})


test_that("linear partial plots retain raw choices and use a common scale", {
  d <- .partial_test_data()
  d$y <- 20 + 7 * d$x + 2 * (d$g == "B") + sin(seq_len(nrow(d))) * 4
  d$x[c(3, 18)] <- NA_real_
  records <- lapply(c(1, 100), function(multiplier) {
    data <- d; data$y <- data$y * multiplier
    id <- ls_register_dataset(paste0("partial_linear_units_", multiplier), data)
    model <- ls_new_glm(id)
    ls_glm_set_dependent(model, "y")
    ls_glm_add_predictor(model, "x"); ls_glm_add_predictor(model, "g")
    ls_glm_fit(model)
    LinkEDA:::.rls_glm_model_record(model)
  })
  on.exit(for (multiplier in c(1, 100)) ls_unregister_dataset(paste0("partial_linear_units_", multiplier)), add = TRUE)
  reference <- lm(y ~ x + g, data = d)
  used <- as.integer(rownames(model.frame(reference)))
  for (term in c("x", "g")) {
    block <- predict(reference, type = "terms")[, term]
    for (type in c("raw", "standardized", "studentized", "raw")) {
      result <- LinkEDA:::.rls_regression_partial_plot_record(records[[1]], term, type)
      expect_setequal(result$available_residual_types, c("raw", "standardized", "studentized"))
      expect_setequal(result$available_terms, c("x", "g"))
      expect_equal(result$rows$row_id, used)
      residual <- switch(type, raw = residuals(reference),
        standardized = rstandard(reference), studentized = rstudent(reference))
      scale <- switch(type, raw = rep(1, length(residual)),
        standardized = sigma(reference) * sqrt(1 - hatvalues(reference)),
        studentized = lm.influence(reference)$sigma * sqrt(1 - hatvalues(reference)))
      expect_equal(result$rows$contribution, unname(block / scale), tolerance = 1e-10)
      expect_equal(result$rows$partial_residual, unname(block / scale + residual), tolerance = 1e-10)
      expect_equal(result$contribution_scale, if(type == "raw") "" else type)
      replay <- .partial_replay(result, d)
      expect_equal(replay$contribution, result$rows$contribution, tolerance = 1e-10)
      expect_equal(replay$partial_residual, result$rows$partial_residual, tolerance = 1e-10)
      if(type != "raw") {
        converted <- LinkEDA:::.rls_regression_partial_plot_record(records[[2]], term, type)
        expect_equal(converted$rows$contribution, result$rows$contribution, tolerance = 1e-10)
        expect_equal(converted$rows$partial_residual, result$rows$partial_residual, tolerance = 1e-10)
      }
    }
  }
})


test_that("MI linear partial scaling and public recipes use each fitted imputation", {
  skip_if_not_installed("mice")
  data <- .partial_test_data()
  completed <- lapply(1:3, function(i) { d <- data; d$x[2:6] <- d$x[2:6] + i / 8; d })
  id <- ls_register_dataset("partial_linear_mi", data)
  on.exit(ls_unregister_dataset(id), add = TRUE)
  dataset <- LinkEDA:::.rls_dataset_record(id)
  dataset$dataset_type <- "multiple_imputation"
  dataset$imputation_id <- "partial_linear_imp"
  dataset$imputation_count <- 3L; dataset$completed_datasets <- completed
  dataset$original_data <- data; dataset$original_data$x[2:6] <- NA_real_
  dataset$original_row_ids <- seq_len(nrow(data))
  dataset$missing_cell_mask <- as.data.frame(lapply(dataset$original_data, is.na))
  LinkEDA:::.rls_set_dataset_record(dataset)
  model <- ls_new_glm(id); ls_glm_set_dependent(model, "y")
  ls_glm_add_predictor(model, "x"); ls_glm_add_predictor(model, "g"); ls_glm_fit(model)
  state <- LinkEDA:::.rls_glm_model_record(model)
  exported <- do.call(rbind, lapply(1:3, function(i) {
    d <- completed[[i]]; d$.imp <- i; d$.id <- seq_len(nrow(d)); d
  }))
  for (type in c("raw", "standardized", "studentized")) {
    result <- LinkEDA:::.rls_regression_partial_plot_record(state, "x", type)
    expect_identical(result$imputation_count, 3L)
    expect_setequal(result$available_residual_types, c("raw", "standardized", "studentized"))
    for (i in 1:3) {
      fit <- lm(y ~ x + g, data = completed[[i]])
      scale <- switch(type, raw = rep(1, nrow(data)),
        standardized = sigma(fit) * sqrt(1 - hatvalues(fit)),
        studentized = lm.influence(fit)$sigma * sqrt(1 - hatvalues(fit)))
      points <- result$rows[result$rows$imputation == i, ]
      expect_equal(points$row_id, seq_len(nrow(data)))
      expect_equal(points$contribution, unname(predict(fit, type="terms")[, "x"] / scale), tolerance=1e-10)
    }
    replay <- .partial_replay(result, exported)
    expect_equal(replay$contribution, result$rows$contribution, tolerance=1e-10)
    expect_equal(replay$partial_residual, result$rows$partial_residual, tolerance=1e-10)
  }
})
