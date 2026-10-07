test_that("binary effects are R-computed probabilities from the complete fitted model", {
  set.seed(20260907)
  n <- 240L
  data <- data.frame(
    event = 0L,
    treatment = factor(rep(c("Control", "Treatment"), each = n / 2L),
                       levels = c("Control", "Treatment")),
    sex = factor(rep(c("Female", "Male"), length.out = n)),
    age = stats::rnorm(n, 42, 11)
  )
  eta <- with(data,
    -1.1 + .9 * (treatment == "Treatment") - .5 * (sex == "Male") +
      .025 * age + .45 * (treatment == "Treatment") * (sex == "Male")
  )
  data$event <- stats::rbinom(n, 1L, stats::plogis(eta))

  for (link in c("logit", "probit", "cloglog")) {
    fit <- stats::glm(
      event ~ treatment * sex + age, data = data,
      family = stats::binomial(link = link)
    )
    record <- list(
      predictors = c("treatment", "sex", "age", "treatment:sex"),
      fit = fit, binary_regression = TRUE, event = "1"
    )
    result <- LinkEDA:::.rls_regression_interaction_record(
      record, "treatment:sex", confidence_level = .90,
      quantity = "predicted_probability", adjustment = "average_sample"
    )
    assigned <- transform(
      data,
      treatment = factor("Treatment", levels = levels(treatment)),
      sex = factor("Male", levels = levels(sex))
    )
    direct <- mean(stats::predict(fit, newdata = assigned, type = "response"))
    cell <- with(result$estimates, treatment == "Treatment" & sex == "Male")

    expect_equal(result$estimates$pooled_estimate[cell], direct, tolerance = 1e-11)
    expect_false(isTRUE(all.equal(
      result$estimates$pooled_estimate[cell],
      fit$family$linkinv(stats::coef(fit)[["treatmentTreatment"]])
    )))
    expect_true(all(result$estimates$pooled_conf_low >= 0))
    expect_true(all(result$estimates$pooled_conf_high <= 1))
    expect_equal(result$confidence_level, .90)
  }
})

test_that("binary probability differences retain orientation and within-model covariance", {
  set.seed(20260908)
  n <- 180L
  data <- data.frame(
    event = stats::rbinom(n, 1L, .5),
    treatment = factor(rep(c("Control", "Treatment"), each = n / 2L),
                       levels = c("Control", "Treatment")),
    age = stats::rnorm(n, 35, 9)
  )
  fit <- stats::glm(event ~ treatment * age, data = data, family = stats::binomial())
  record <- list(
    predictors = c("treatment", "age", "treatment:age"), fit = fit,
    binary_regression = TRUE, event = "1"
  )
  probabilities <- LinkEDA:::.rls_regression_interaction_record(
    record, "treatment", quantity = "predicted_probability"
  )
  differences <- LinkEDA:::.rls_regression_interaction_record(
    record, "treatment", quantity = "probability_difference"
  )

  expect_identical(as.character(differences$comparisons$contrast),
                   "Treatment − Control")
  expect_equal(
    differences$comparisons$pooled_estimate,
    diff(probabilities$estimates$pooled_estimate), tolerance = 1e-11
  )
  left <- LinkEDA:::.rls_binary_scenario_estimate(
    fit, list(treatment = "Treatment"), "average_sample"
  )
  right <- LinkEDA:::.rls_binary_scenario_estimate(
    fit, list(treatment = "Control"), "average_sample"
  )
  gradient <- left$gradient - right$gradient
  covariance_variance <- drop(crossprod(gradient, stats::vcov(fit) %*% gradient))
  expect_equal(differences$comparisons$pooled_std_error,
               sqrt(covariance_variance), tolerance = 1e-11)

  percent_wire <- LinkEDA:::.rls_interaction_native_plot_payload(probabilities)
  proportion <- LinkEDA:::.rls_regression_interaction_record(
    record, "treatment", quantity = "predicted_probability",
    presentation = "probability"
  )
  proportion_wire <- LinkEDA:::.rls_interaction_native_plot_payload(proportion)
  percent_y <- as.numeric(strsplit(percent_wire[startsWith(percent_wire, "ROW\t")][[1L]], "\t")[[1L]][[4L]])
  proportion_y <- as.numeric(strsplit(proportion_wire[startsWith(proportion_wire, "ROW\t")][[1L]], "\t")[[1L]][[4L]])
  expect_equal(percent_y, 100 * proportion_y, tolerance = 1e-12)
})

test_that("binary probability effects pool every imputation in R", {
  skip_if_not_installed("mice")
  set.seed(20260909)
  n <- 150L
  data <- data.frame(
    event = stats::rbinom(n, 1L, .45),
    treatment = factor(rep(c("Control", "Treatment"), length.out = n),
                       levels = c("Control", "Treatment")),
    age = stats::rnorm(n, 40, 10)
  )
  fits <- lapply(seq_len(3L), function(i) stats::glm(
    event ~ treatment + age,
    data = transform(data, age = age + stats::rnorm(n, 0, .15 * i)),
    family = stats::binomial()
  ))
  result <- LinkEDA:::.rls_regression_interaction_record(
    list(
      predictors = c("treatment", "age"), fits_by_imputation = fits,
      binary_regression = TRUE, event = "1"
    ),
    "treatment", quantity = "probability_difference"
  )

  expect_equal(result$imputation_count, 3L)
  expect_match(result$pooling_method, "mice::pool.scalar", fixed = TRUE)
  expect_true(all(is.finite(result$comparisons$pooled_df)))
  expect_false(isTRUE(all.equal(
    result$comparisons$pooled_estimate,
    LinkEDA:::.rls_regression_interaction_record(
      list(
        predictors = c("treatment", "age"), fit = fits[[1L]],
        binary_regression = TRUE, event = "1"
      ),
      "treatment", quantity = "probability_difference"
    )$comparisons$pooled_estimate
  )))
})

test_that("binary interpretation explains non-estimable sparse interactions", {
  data <- mtcars
  data$am <- factor(data$am)
  data$cyl <- factor(data$cyl)
  data$gear <- factor(data$gear)
  model <- ls_new_binary_regression(
    data, response = "am",
    terms = c("mpg", "cyl", "gear", "gear:mpg", "gear:cyl",
              "mpg:cyl", "gear:mpg:cyl"),
    event = "1", reference = "0", native = FALSE
  )
  expect_error(
    ls_binary_regression_interaction(model, "gear:mpg:cyl"),
    "coefficients are not estimable.*Remove unsupported interaction terms"
  )

  simpler <- ls_new_binary_regression(
    data, response = "am", terms = c("mpg", "cyl", "mpg:cyl"),
    event = "1", reference = "0", native = FALSE
  )
  effect <- ls_binary_regression_interaction(simpler, "mpg:cyl")
  expect_true(all(is.finite(effect$estimates$pooled_estimate)))
  expect_true(all(is.finite(effect$comparisons$pooled_std_error)))
})

test_that("binary probability tables use observed count values and concise numeric labels", {
  data <- utils::read.csv(
    system.file("examples", "Alien.csv", package = "LinkEDA"),
    stringsAsFactors = FALSE
  )
  data$planet <- factor(data$planet)
  data$happy <- factor(data$happy)
  fit <- stats::glm(
    happy ~ humans_eaten * planet * size,
    data = data, family = stats::binomial()
  )
  result <- LinkEDA:::.rls_regression_interaction_record(
    list(
      fit = fit, binary_regression = TRUE, event = "yes",
      predictors = c("humans_eaten", "planet", "size", "humans_eaten:planet",
                     "humans_eaten:size", "planet:size", "humans_eaten:planet:size")
    ),
    "humans_eaten:planet:size"
  )
  expect_equal(sort(unique(result$estimates$humans_eaten)), 1:19)
  expect_length(unique(result$estimates$size), 3L)

  lines <- LinkEDA:::.rls_interaction_native_report(result)
  expect_true(any(grepl(
    "^Numeric conditioning values for size: 56\\.47, 65\\.23, 73\\.98\\.$",
    lines
  )))
  expect_true(any(grepl("^1\\tAurelia\\t[0-9]+\\.[0-9]+\\t", lines)))
  expect_false(any(grepl("^1\\.75\\t", lines)))
  expect_false(any(grepl("56.472181", lines, fixed = TRUE)))
})
