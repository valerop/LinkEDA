omnibus_coefficient_frame <- function(fit) {
  table <- as.data.frame(unclass(summary(fit)$coefficients), stringsAsFactors = FALSE)
  names(table)[1:4] <- c("estimate", "std_error", "statistic", "p_value")
  table$term <- rownames(summary(fit)$coefficients)
  rownames(table) <- NULL
  table
}

omnibus_display_rows <- function(fit, data, terms, tests = NULL) {
  rows <- LinkEDA:::.rls_model_coefficient_display_rows(
    fit, omnibus_coefficient_frame(fit), data, terms,
    statistic_name = if (inherits(fit, "lm") && !inherits(fit, "glm")) "t" else "z"
  )
  if (is.null(tests)) {
    tests <- LinkEDA:::.rls_model_term_omnibus_tests(fit, terms)
  }
  LinkEDA:::.rls_model_apply_parent_term_tests(rows, tests)
}

omnibus_test_for <- function(tests, term) {
  tests[tests$term == term, , drop = FALSE]
}

omnibus_mice_result <- function(result) {
  expected_names <- c("F.value", "df1", "df2", "P(>F)", "RIV")
  value <- if (is.matrix(result) || is.data.frame(result)) {
    result[1L, , drop = TRUE]
  } else {
    result
  }
  if (length(value) == length(expected_names) && is.null(names(value))) {
    names(value) <- expected_names
  }
  value
}

test_that("A: a three-level factor parent shows the exact conditional joint test", {
  set.seed(3101)
  n <- 150L
  factor3 <- factor(rep(c("A", "B", "C"), length.out = n), levels = c("A", "B", "C"))
  numeric <- stats::rnorm(n)
  y <- 2 + .7 * numeric + ifelse(factor3 == "B", .8, ifelse(factor3 == "C", -1.1, 0)) +
    stats::rnorm(n, sd = .8)
  data <- data.frame(y, factor3, numeric)
  fit <- stats::lm(y ~ factor3 + numeric, data)

  tests <- LinkEDA:::.rls_model_term_omnibus_tests(fit, "factor3")
  factor_test <- omnibus_test_for(tests, "factor3")
  direct <- stats::drop1(fit, test = "F")
  expect_equal(factor_test$df1, 2)
  expect_equal(factor_test$p_value, direct["factor3", "Pr(>F)"], tolerance = 1e-12)
  expect_identical(
    strsplit(factor_test$coefficient_names, "\u001f", fixed = TRUE)[[1L]],
    c("factor3B", "factor3C")
  )

  rows <- omnibus_display_rows(fit, data, c("factor3", "numeric"), tests)
  parent <- rows[rows$source_term == "factor3" & rows$row_type == "factor_parent", ]
  children <- rows[rows$source_term == "factor3" & rows$row_type == "factor_level", ]
  expect_equal(parent$p_value, direct["factor3", "Pr(>F)"], tolerance = 1e-12)
  expect_equal(
    children$p_value,
    unname(summary(fit)$coefficients[c("factor3B", "factor3C"), "Pr(>|t|)"]),
    tolerance = 1e-12
  )
})

test_that("B: a two-level factor parent p agrees with its sole coefficient", {
  set.seed(3102)
  n <- 96L
  factor2 <- factor(rep(c("control", "treated"), length.out = n),
                    levels = c("control", "treated"))
  numeric <- stats::rnorm(n)
  y <- 1 + .5 * numeric + .9 * (factor2 == "treated") + stats::rnorm(n)
  data <- data.frame(y, factor2, numeric)
  fit <- stats::lm(y ~ factor2 + numeric, data)
  tests <- LinkEDA:::.rls_model_term_omnibus_tests(fit, "factor2")
  rows <- omnibus_display_rows(fit, data, c("factor2", "numeric"), tests)
  parent_p <- rows$p_value[rows$source_term == "factor2" & rows$row_type == "factor_parent"]
  coefficient_p <- summary(fit)$coefficients["factor2treated", "Pr(>|t|)"]
  expect_equal(parent_p, coefficient_p, tolerance = 1e-12)
})

test_that("C: MI factor omnibus p is the mice D1 result", {
  skip_if_not_installed("mice")
  set.seed(3103)
  n <- 105L
  factor3 <- factor(rep(c("A", "B", "C"), length.out = n), levels = c("A", "B", "C"))
  numeric <- stats::rnorm(n)
  y <- 1.5 + .6 * numeric + ifelse(factor3 == "B", .7, ifelse(factor3 == "C", -1, 0)) +
    stats::rnorm(n, sd = .9)
  incomplete <- data.frame(y, factor3, numeric)
  incomplete$y[seq(4L, n, by = 17L)] <- NA_real_
  incomplete$numeric[seq(8L, n, by = 19L)] <- NA_real_
  mids <- mice::mice(incomplete, m = 5L, maxit = 2L, printFlag = FALSE, seed = 3103)
  completed <- mice::complete(mids, action = "all")
  full <- lapply(completed, function(data) stats::lm(y ~ factor3 + numeric, data))
  reduced <- lapply(completed, function(data) stats::lm(y ~ numeric, data))

  actual <- omnibus_test_for(
    LinkEDA:::.rls_mi_term_omnibus_tests(full, "factor3"), "factor3"
  )
  direct <- mice::D1(mice::as.mira(full), mice::as.mira(reduced))
  expected <- omnibus_mice_result(direct$result)
  expect_identical(actual$method, "mice::D1")
  expect_equal(actual$statistic, expected[["F.value"]], tolerance = 1e-10)
  expect_equal(actual$df1, expected[["df1"]], tolerance = 1e-10)
  expect_equal(actual$df2, expected[["df2"]], tolerance = 1e-10)
  expect_equal(actual$p_value, expected[["P(>F)"]], tolerance = 1e-10)

  pooled <- LinkEDA:::.rls_mi_pool_coefficients(full, statistic_name = "t")
  rows <- LinkEDA:::.rls_model_coefficient_display_rows(
    full[[1L]], transform(pooled, statistic = t_value), completed[[1L]],
    c("factor3", "numeric"), statistic_name = "t"
  )
  rows <- LinkEDA:::.rls_model_apply_parent_term_tests(
    rows, LinkEDA:::.rls_mi_term_omnibus_tests(full, "factor3")
  )
  expect_equal(
    rows$p_value[rows$source_term == "factor3" & rows$row_type == "factor_parent"],
    expected[["P(>F)"]], tolerance = 1e-10
  )
})

test_that("D: two factors receive separate exact omnibus tests", {
  set.seed(3104)
  n <- 180L
  factorA <- factor(rep(c("A1", "A2", "A3"), length.out = n))
  factorB <- factor(rep(rep(c("B1", "B2", "B3", "B4"), each = 3L), length.out = n))
  numeric <- stats::rnorm(n)
  y <- 3 + .4 * numeric + as.numeric(factorA) * .35 - as.numeric(factorB) * .2 +
    stats::rnorm(n, sd = .75)
  data <- data.frame(y, factorA, factorB, numeric)
  fit <- stats::lm(y ~ factorA + factorB + numeric, data)
  tests <- LinkEDA:::.rls_model_term_omnibus_tests(fit, c("factorA", "factorB"))
  direct <- stats::drop1(fit, test = "F")
  expect_equal(omnibus_test_for(tests, "factorA")$p_value,
               direct["factorA", "Pr(>F)"], tolerance = 1e-12)
  expect_equal(omnibus_test_for(tests, "factorB")$p_value,
               direct["factorB", "Pr(>F)"], tolerance = 1e-12)
  expect_equal(omnibus_test_for(tests, "factorA")$df1, 2)
  expect_equal(omnibus_test_for(tests, "factorB")$df1, 3)
})

test_that("E: exact assign mapping keeps interactions separate from main effects", {
  set.seed(3105)
  n <- 192L
  factorA <- factor(rep(c("A1", "A2", "A3"), length.out = n))
  factorB <- factor(rep(rep(c("B1", "B2"), each = 3L), length.out = n))
  numeric <- stats::rnorm(n)
  y <- 1 + .5 * numeric + .3 * as.numeric(factorA) - .4 * as.numeric(factorB) +
    .25 * (as.numeric(factorA) - 1) * (as.numeric(factorB) - 1) + stats::rnorm(n)
  data <- data.frame(y, factorA, factorB, numeric)
  fit <- stats::lm(y ~ numeric + factorA * factorB, data)
  tests <- LinkEDA:::.rls_model_term_omnibus_tests(
    fit, c("numeric", "factorA", "factorB", "factorA:factorB")
  )
  mm <- stats::model.matrix(fit)
  labels <- attr(stats::terms(fit), "term.labels")
  assignment <- attr(mm, "assign")
  for (term in c("factorA", "factorB", "factorA:factorB")) {
    expected_names <- colnames(mm)[assignment == match(term, labels)]
    actual_names <- strsplit(
      omnibus_test_for(tests, term)$coefficient_names, "\u001f", fixed = TRUE
    )[[1L]]
    expect_identical(actual_names, expected_names)
  }
  main_a <- strsplit(omnibus_test_for(tests, "factorA")$coefficient_names,
                     "\u001f", fixed = TRUE)[[1L]]
  interaction <- strsplit(omnibus_test_for(tests, "factorA:factorB")$coefficient_names,
                          "\u001f", fixed = TRUE)[[1L]]
  expect_length(intersect(main_a, interaction), 0L)

  completed <- lapply(seq_len(5L), function(i) {
    transform(data, y = y + sin(seq_len(n) * i / 13) * .08)
  })
  fits <- lapply(completed, function(data) stats::lm(y ~ numeric + factorA * factorB, data))
  mi_tests <- LinkEDA:::.rls_mi_term_omnibus_tests(
    fits, c("factorA", "factorB", "factorA:factorB")
  )
  expect_true(all(is.finite(mi_tests$p_value)))
  expect_true(all(mi_tests$method == "mice::D1"))
  expect_equal(omnibus_test_for(mi_tests, "factorA")$df1, 2)
  expect_equal(omnibus_test_for(mi_tests, "factorB")$df1, 1)
  expect_equal(omnibus_test_for(mi_tests, "factorA:factorB")$df1, 2)
})

test_that("factor by factor interaction coefficients have readable combination labels", {
  data <- expand.grid(
    group = factor(c("Control", "Treatment"), levels = c("Control", "Treatment")),
    country = factor(c("Finland", "Greece", "Italy"),
                     levels = c("Finland", "Greece", "Italy")),
    replicate = seq_len(12L)
  )
  set.seed(3110)
  data$response <- stats::rnorm(nrow(data))
  data$binary <- stats::rbinom(nrow(data), size = 1L, prob = 0.5)
  fits <- list(
    linear = stats::lm(response ~ group * country, data = data),
    generalized = stats::glm(
      binary ~ group * country, data = data, family = stats::binomial()
    )
  )

  for (fit in fits) {
    rows <- LinkEDA:::.rls_model_coefficient_display_rows(
      fit, omnibus_coefficient_frame(fit), data,
      c("group", "country", "group:country")
    )
    interaction <- rows[
      rows$source_term == "group:country" & rows$row_type == "coefficient",
      , drop = FALSE
    ]
    parent <- rows[
      rows$source_term == "group:country" & rows$row_type == "term_parent",
      , drop = FALSE
    ]

    expect_equal(parent$display_label, "group \u00d7 country")
    expect_equal(
      interaction$display_label,
      c("  Treatment \u00d7 Greece (vs Control \u00d7 Finland)",
        "  Treatment \u00d7 Italy (vs Control \u00d7 Finland)")
    )
    expect_false(any(grepl("groupTreatment|countryGreece|countryItaly",
                           interaction$display_label)))
  }
})

test_that("labelled categorical predictors retain category labels in every regression model", {
  labelled <- function(values, labels) {
    structure(as.double(values), labels = labels,
              class = c("haven_labelled", "vctrs_vctr", "double"))
  }
  data <- expand.grid(gender = 1:2, country = 1:3, replicate = seq_len(12L))
  data$gender <- labelled(data$gender, c(Female = 1, Male = 2))
  data$country <- labelled(data$country, c(Finland = 1, Greece = 2, Italy = 3))
  set.seed(3111)
  data$response <- stats::rnorm(nrow(data))
  data$binary <- stats::rbinom(nrow(data), 1L, .5)

  typed <- LinkEDA:::.rls_model_data_for_term_types(
    data, list(gender = "factor", country = "factor",
               `gender:country` = "factor"), response = "response"
  )
  expect_identical(levels(typed$gender), c("Female", "Male"))
  expect_identical(levels(typed$country), c("Finland", "Greece", "Italy"))

  fits <- list(
    stats::lm(response ~ gender * country, data = typed),
    stats::glm(binary ~ gender * country, data = typed,
               family = stats::binomial())
  )
  for (fit in fits) {
    coefficients <- omnibus_coefficient_frame(fit)
    rows <- LinkEDA:::.rls_model_coefficient_display_rows(
      fit, coefficients, typed, c("gender", "country", "gender:country")
    )
    expect_true(all(c("  Female", "  Male", "  Finland", "  Greece", "  Italy") %in%
                    rows$display_label))
    interaction <- rows[
      rows$source_term == "gender:country" & rows$row_type == "coefficient",
      "display_label", drop = TRUE
    ]
    expect_true(any(grepl("Male × Greece", interaction, fixed = TRUE)))
    expect_false(any(grepl("2 × 2", interaction, fixed = TRUE)))
  }
})

test_that("F: changing a factor reference changes contrasts but not its omnibus p", {
  set.seed(3106)
  n <- 144L
  factor3 <- factor(rep(c("A", "B", "C"), length.out = n), levels = c("A", "B", "C"))
  numeric <- stats::rnorm(n)
  y <- 2 + .8 * numeric + ifelse(factor3 == "B", .6, ifelse(factor3 == "C", -1.2, 0)) +
    stats::rnorm(n, sd = .7)
  data_a <- data.frame(y, factor3, numeric)
  data_b <- transform(data_a, factor3 = stats::relevel(factor3, ref = "B"))
  fit_a <- stats::lm(y ~ factor3 + numeric, data_a)
  fit_b <- stats::lm(y ~ factor3 + numeric, data_b)
  test_a <- omnibus_test_for(LinkEDA:::.rls_model_term_omnibus_tests(fit_a, "factor3"), "factor3")
  test_b <- omnibus_test_for(LinkEDA:::.rls_model_term_omnibus_tests(fit_b, "factor3"), "factor3")
  expect_equal(test_a$p_value, test_b$p_value, tolerance = 1e-12)
  expect_false(identical(names(stats::coef(fit_a)), names(stats::coef(fit_b))))
  expect_false(isTRUE(all.equal(unname(stats::coef(fit_a)), unname(stats::coef(fit_b)))))
})

test_that("ordinary generalized families use the same parent-row contract", {
  set.seed(3107)
  n <- 150L
  factor3 <- factor(rep(c("A", "B", "C"), length.out = n), levels = c("A", "B", "C"))
  numeric <- stats::rnorm(n)
  y_bin <- rep(c(0L, 1L, 0L, 1L, 1L), length.out = n)
  y_count <- as.integer(1L + (seq_len(n) * 7L) %% 11L)
  data <- data.frame(y_bin, y_count, factor3, numeric)
  fits <- list(
    stats::glm(y_bin ~ factor3 + numeric, data, family = stats::binomial()),
    stats::glm(y_count ~ factor3 + numeric, data, family = stats::poisson()),
    stats::glm(y_count ~ factor3 + numeric, data, family = stats::quasipoisson())
  )
  for (fit in fits) {
    tests <- LinkEDA:::.rls_model_term_omnibus_tests(fit, "factor3")
    rows <- omnibus_display_rows(fit, data, c("factor3", "numeric"), tests)
    parent <- rows[rows$source_term == "factor3" & rows$row_type == "factor_parent", ]
    expect_true(is.finite(parent$p_value))
    expect_equal(omnibus_test_for(tests, "factor3")$df1, 2)
  }
})

test_that("MI parent omnibus p reaches every regression-table family", {
  skip_if_not_installed("mice")
  skip_if_not_installed("MASS")
  set.seed(3108)
  n <- 120L
  factor3 <- factor(rep(c("A", "B", "C"), length.out = n), levels = c("A", "B", "C"))
  numeric <- stats::rnorm(n)
  completed <- lapply(seq_len(5L), function(i) {
    eta <- .25 + .55 * numeric + c(A = 0, B = .45, C = -.4)[as.character(factor3)]
    data.frame(
      y_linear = 2 + eta + stats::rnorm(n, sd = .5 + i / 100),
      y_binary = stats::rbinom(n, 1L, stats::plogis(eta)),
      y_count = stats::rpois(n, exp(.7 + eta / 3)),
      numeric = numeric + stats::rnorm(n, sd = .01 * i),
      factor3 = factor3
    )
  })
  original <- completed[[1L]]
  original$numeric[seq(9L, n, by = 23L)] <- NA_real_
  id <- LinkEDA::ls_register_dataset("omnibus_mi_all_families", completed[[1L]])
  on.exit(LinkEDA::ls_unregister_dataset(id), add = TRUE)
  dataset <- LinkEDA:::.rls_dataset_record(id)
  dataset$dataset_type <- "multiple_imputation"
  dataset$original_data <- original
  dataset$completed_datasets <- completed
  dataset$missing_cell_mask <- as.data.frame(lapply(original, is.na), stringsAsFactors = FALSE)
  dataset$imputation_count <- length(completed)
  dataset$original_row_ids <- seq_len(n)
  LinkEDA:::.rls_set_dataset_record(dataset)

  linear <- LinkEDA::ls_new_glm(id)
  linear <- LinkEDA::ls_glm_set_dependent(linear, "y_linear")
  linear <- LinkEDA::ls_glm_add_predictor(linear, "numeric")
  linear <- LinkEDA::ls_glm_add_predictor(linear, "factor3")
  linear <- LinkEDA::ls_glm_fit(linear)
  models <- c(
    list(linear = LinkEDA:::.rls_glm_model_record(linear)),
    list(generalized = LinkEDA::ls_generalized_linear_model_state(LinkEDA::ls_new_generalized_linear_model(
      id, response = "y_linear", terms = c("numeric", "factor3"),
      family = "gaussian", link = "identity", native = FALSE
    ))),
    setNames(lapply(c("logit", "probit", "cloglog"), function(link) {
      LinkEDA::ls_binary_regression_state(LinkEDA::ls_new_binary_regression(
        id, response = "y_binary", terms = c("numeric", "factor3"),
        link = link, event = "1", reference = "0", native = FALSE
      ))
    }), paste0("binary_", c("logit", "probit", "cloglog"))),
    setNames(lapply(c("poisson", "quasipoisson", "negative_binomial"), function(distribution) {
      LinkEDA::ls_count_regression_state(suppressWarnings(LinkEDA::ls_new_count_regression(
        id, response = "y_count", terms = c("numeric", "factor3"),
        distribution = distribution, native = FALSE
      )))
    }), paste0("count_", c("poisson", "quasipoisson", "negative_binomial")))
  )

  for (name in names(models)) {
    state <- models[[name]]
    parent <- state$coefficient_rows[
      state$coefficient_rows$source_term == "factor3" &
        state$coefficient_rows$row_type == "factor_parent", , drop = FALSE
    ]
    test <- omnibus_test_for(state$parent_term_tests, "factor3")
    expect_equal(nrow(parent), 1L, info = name)
    expect_true(is.finite(parent$p_value), info = name)
    expect_equal(parent$p_value, test$p_value, tolerance = 1e-12, info = name)
    expected_method <- if (identical(name, "linear")) {
      "mice::D1"
    } else {
      "Rubin pooled joint Wald chi-square from the fitted mean coefficients"
    }
    expect_identical(test$method, expected_method, info = name)
    expect_equal(test$df1, 2, info = name)
  }

  # The semantic p must survive the native wire protocol used by both AppKit
  # and WinUI; otherwise the correct R result would still render as blank.
  linear_parent <- which(models$linear$coefficient_rows$row_type == "factor_parent")
  linear_payload <- LinkEDA:::.rls_native_linear_coefficient_payload(models$linear)
  linear_p_position <- 1L + (linear_parent - 1L) * 14L + 12L
  expect_equal(
    as.numeric(linear_payload[[linear_p_position]]),
    models$linear$coefficient_rows$p_value[[linear_parent]], tolerance = 1e-12
  )

  generalized_parent <- which(
    models$generalized$coefficient_rows$row_type == "factor_parent"
  )
  generalized_payload <- LinkEDA:::.rls_native_generalized_coefficient_payload(
    models$generalized
  )
  row_marker_offset <- if (identical(generalized_payload[[1L]], "GENERALIZED_ROWS_V2")) 2L else 1L
  row_width <- if (identical(generalized_payload[[1L]], "GENERALIZED_ROWS_V2")) 13L else 12L
  generalized_p_position <- row_marker_offset + (generalized_parent - 1L) * row_width + 12L
  expect_equal(
    as.numeric(generalized_payload[[generalized_p_position]]),
    models$generalized$coefficient_rows$p_value[[generalized_parent]],
    tolerance = 1e-12
  )
})
