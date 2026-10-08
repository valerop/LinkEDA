.make_postestimation_mi_dataset <- function(name, original, completed) {
  id <- ls_register_dataset(name, completed[[1L]])
  record <- LinkEDA:::.rls_dataset_record(id)
  record$dataset_type <- "multiple_imputation"
  record$imputation_id <- paste0(name, "_imp")
  record$source_dataset_id <- paste0(name, "_source")
  record$original_data <- original
  record$completed_datasets <- completed
  record$missing_cell_mask <- as.data.frame(lapply(original, is.na), stringsAsFactors = FALSE)
  record$imputed_cell_map <- list()
  record$active_imputation_version <- 1L
  record$imputation_display_mode <- "version"
  record$imputation_count <- length(completed)
  record$original_row_ids <- seq_len(nrow(original))
  LinkEDA:::.rls_set_dataset_record(record)
  id
}

.expect_pairwise_matches_mira <- function(actual, fits, term) {
  direct <- emmeans::emmeans(mice::as.mira(fits), specs = term)
  direct <- emmeans::regrid(direct, transform = "response")
  direct <- as.data.frame(summary(
    emmeans::contrast(direct, method = "pairwise", adjust = "tukey"),
    infer = c(TRUE, TRUE)
  ))
  expect_equal(actual$pooled_estimate, direct$estimate, tolerance = 1e-10)
  expect_equal(actual$pooled_std_error, direct$SE, tolerance = 1e-10)
  expect_equal(actual$pooled_p_value, direct$p.value, tolerance = 1e-10)
  expect_true(all(actual$imputation_count == length(fits)))
  expect_true(all(grepl("mice::mira", actual$pooling_method, fixed = TRUE)))
}

.expect_interaction_matches_mira <- function(actual, fits, numeric_term, factor_term) {
  direct <- as.data.frame(summary(
    emmeans::emtrends(mice::as.mira(fits), specs = factor_term, var = numeric_term),
    infer = c(TRUE, TRUE)
  ))
  trend_name <- paste0(numeric_term, ".trend")
  expect_identical(actual$interaction_type, "numeric_factor")
  expect_equal(actual$estimates$pooled_estimate, direct[[trend_name]], tolerance = 1e-10)
  expect_equal(actual$estimates$pooled_std_error, direct$SE, tolerance = 1e-10)
  expect_equal(actual$imputation_count, length(fits))
  expect_true(actual$multiple_imputation)
  expect_true(nrow(actual$plot_data) >= 3L * 3L)
  expect_true(all(actual$plot_data$imputation_count == length(fits)))
  expect_match(actual$interpretation, "Pooled simple slopes")
}

.expect_three_way_interaction_matches_mira <- function(actual, fits, numeric_term,
                                                       first_factor, second_factor) {
  direct <- as.data.frame(summary(
    emmeans::emtrends(
      mice::as.mira(fits), specs = first_factor, by = second_factor,
      var = numeric_term
    ),
    infer = c(TRUE, TRUE)
  ))
  trend_name <- paste0(numeric_term, ".trend")
  expect_identical(actual$interaction_type, "three_way_with_numeric")
  expect_identical(actual$conditioning, c(first_factor, second_factor))
  expect_equal(actual$estimates$pooled_estimate, direct[[trend_name]], tolerance = 1e-10)
  expect_equal(actual$estimates$pooled_std_error, direct$SE, tolerance = 1e-10)
  expect_equal(actual$imputation_count, length(fits))
  expect_true(actual$multiple_imputation)
  expect_true(all(c(numeric_term, first_factor, second_factor) %in%
                    names(actual$plot_data)))
  expect_true(nrow(actual$plot_data) >= 25L * 3L * 3L)
  expect_match(actual$interpretation, "every displayed combination")
}

test_that("MI pairwise comparisons and interactions use every fit for all regression families", {
  skip_if_not_installed("mice")
  skip_if_not_installed("broom")
  skip_if_not_installed("emmeans")
  skip_if_not_installed("MASS")

  set.seed(20260829)
  n <- 180L
  x <- seq(-1.4, 1.4, length.out = n)
  z <- sin(seq_len(n) / 8)
  g <- factor(rep(c("A", "B", "C"), length.out = n), levels = c("A", "B", "C"))
  rating <- ordered(rep(c("low", "mid", "high"), each = 10L, length.out = n),
                    levels = c("low", "mid", "high"))
  g_effect <- c(A = 0, B = .45, C = -.3)[as.character(g)]
  interaction_effect <- x * c(A = 0, B = .35, C = -.2)[as.character(g)]
  linear_mean <- 2 + .8 * x - .25 * z + g_effect + interaction_effect
  binary_probability <- stats::plogis(-.15 + .65 * x - .2 * z + g_effect + interaction_effect)
  count_mean <- exp(.8 + .22 * x - .1 * z + .45 * g_effect + .15 * interaction_effect)
  y_linear <- linear_mean + stats::rnorm(n, sd = .45)
  y_binary <- stats::rbinom(n, 1, binary_probability)
  y_count <- stats::rnbinom(n, mu = count_mean, size = 2.5)
  completed <- lapply(seq_len(3L), function(i) {
    data.frame(
      y_linear = y_linear + stats::rnorm(n, sd = .025 * i),
      y_binary = y_binary,
      y_count = y_count,
      x = x + stats::rnorm(n, sd = .01 * i),
      z = z + stats::rnorm(n, sd = .008 * i),
      g = g,
      rating = rating
    )
  })
  original <- completed[[1L]]
  original$x[c(11L, 73L, 149L)] <- NA_real_
  original$g[c(31L, 122L)] <- NA
  id <- .make_postestimation_mi_dataset("mi_regression_post_estimation", original, completed)
  on.exit(ls_unregister_dataset(id), add = TRUE)

  linear <- ls_new_glm(id)
  linear <- ls_glm_set_dependent(linear, "y_linear")
  regression_terms <- c(
    "x", "g", "rating", "x:g", "x:rating", "g:rating", "x:g:rating"
  )
  for (term in regression_terms) linear <- ls_glm_add_predictor(linear, term)
  linear <- ls_glm_fit(linear)

  generalized <- ls_new_generalized_linear_model(
    id, response = "y_linear", terms = regression_terms,
    family = "gaussian", link = "identity", native = FALSE
  )
  binary_models <- lapply(c("logit", "probit", "cloglog"), function(link) {
    ls_new_binary_regression(
      id, response = "y_binary", terms = regression_terms,
      event = "1", reference = "0", link = link, native = FALSE
    )
  })
  count_models <- lapply(c("poisson", "quasipoisson", "negative_binomial"), function(distribution) {
    ls_new_count_regression(
      id, response = "y_count", terms = regression_terms,
      distribution = distribution, native = FALSE
    )
  })

  cases <- c(
    list(
      linear = list(
        state = LinkEDA:::.rls_glm_model_record(linear),
        pairwise = function() ls_glm_pairwise(linear, "g"),
        interaction = function() ls_glm_interaction(linear, "x:g"),
        interaction_three = function() ls_glm_interaction(linear, "x:g:rating"),
        interaction_three_grouped = function() ls_glm_interaction(
          linear, "x:g:rating", focal = "g", group_by = "rating")
      ),
      generalized = list(
        state = ls_generalized_linear_model_state(generalized),
        pairwise = function() ls_generalized_linear_model_pairwise(generalized, "g"),
        interaction = function() ls_generalized_linear_model_interaction(generalized, "x:g"),
        interaction_three = function() ls_generalized_linear_model_interaction(
          generalized, "x:g:rating"
        ),
        interaction_three_grouped = function() ls_generalized_linear_model_interaction(
          generalized, "x:g:rating", focal = "g", group_by = "rating")
      )
    ),
    setNames(lapply(seq_along(binary_models), function(i) list(
      state = ls_binary_regression_state(binary_models[[i]]),
      pairwise = local({ model <- binary_models[[i]]; function() ls_binary_regression_pairwise(model, "g", scale = "probability") }),
      interaction = local({ model <- binary_models[[i]]; function() ls_binary_regression_interaction(model, "x:g") }),
      interaction_three = local({ model <- binary_models[[i]]; function() ls_binary_regression_interaction(model, "x:g:rating") }),
      interaction_three_grouped = local({ model <- binary_models[[i]]; function() ls_binary_regression_interaction(
        model, "x:g:rating", focal = "g", group_by = "rating") })
    )), paste0("binary_", c("logit", "probit", "cloglog"))),
    setNames(lapply(seq_along(count_models), function(i) list(
      state = ls_count_regression_state(count_models[[i]]),
      pairwise = local({ model <- count_models[[i]]; function() ls_count_regression_pairwise(model, "g") }),
      interaction = local({ model <- count_models[[i]]; function() ls_count_regression_interaction(model, "x:g") }),
      interaction_three = local({ model <- count_models[[i]]; function() ls_count_regression_interaction(model, "x:g:rating") }),
      interaction_three_grouped = local({ model <- count_models[[i]]; function() ls_count_regression_interaction(
        model, "x:g:rating", focal = "g", group_by = "rating") })
    )), paste0("count_", c("poisson", "quasipoisson", "negative_binomial")))
  )

  for (label in names(cases)) {
    case <- cases[[label]]
    expect_length(case$state$fits_by_imputation, 3L)
    .expect_pairwise_matches_mira(case$pairwise(), case$state$fits_by_imputation, "g")
    grouped <- case$interaction_three_grouped()
    expect_identical(grouped$focal, "g")
    expect_identical(grouped$conditioning, c("rating", "x"))
    expect_equal(grouped$imputation_count, 3L)
    expect_true(nrow(grouped$estimates) > 0L)
    grouped_report <- LinkEDA:::.rls_interaction_native_report(grouped)
    expect_false("Simple slopes" %in% grouped_report)
    if (!startsWith(label, "binary_"))
      expect_true("Estimated marginal means" %in% grouped_report)
    if (startsWith(label, "binary_")) {
      binary_effect <- case$interaction()
      expect_true(binary_effect$binary_probability)
      expect_identical(binary_effect$interaction_type, "numeric_factor")
      expect_equal(binary_effect$imputation_count, 3L)
      expect_true(all(binary_effect$estimates$pooled_estimate >= 0 &
                        binary_effect$estimates$pooled_estimate <= 1))
      expect_true(all(binary_effect$estimates$imputation_count == 3L))
      expect_match(binary_effect$pooling_method, "mice::pool.scalar", fixed = TRUE)
      three_way <- case$interaction_three()
      expect_true(three_way$binary_probability)
      expect_identical(three_way$interaction_type, "three_way_with_numeric")
      expect_identical(three_way$conditioning, c("g", "rating"))
      expect_equal(three_way$imputation_count, 3L)
      three_way_wire <- LinkEDA:::.rls_interaction_native_plot_payload(three_way)
      expect_true(any(startsWith(three_way_wire, "BINARY\t")))
      expect_true(any(grepl("g = A · rating = low", three_way_wire, fixed = TRUE)))
      next
    }
    .expect_interaction_matches_mira(
      case$interaction(), case$state$fits_by_imputation, "x", "g"
    )
    three_way <- case$interaction_three()
    .expect_three_way_interaction_matches_mira(
      three_way, case$state$fits_by_imputation, "x", "g", "rating"
    )
    three_way_report <- LinkEDA:::.rls_interaction_native_report(three_way)
    expect_true(
      "g\trating\tEstimate\tSE\tStatistic\tp\tLower 95%\tUpper 95%" %in%
        three_way_report
    )
    expect_true(
      "rating\tComparison\tEstimate\tSE\tStatistic\tAdjusted p\tLower 95%\tUpper 95%" %in%
        three_way_report
    )
    plot_wire <- LinkEDA:::.rls_interaction_native_plot_payload(case$interaction())
    expect_identical(plot_wire[[1L]], "OK")
    expect_true(any(startsWith(plot_wire, "META\t")))
    expect_true(sum(startsWith(plot_wire, "ROW\t")) >= 9L)
    three_way_wire <- LinkEDA:::.rls_interaction_native_plot_payload(three_way)
    expect_true(any(grepl("g = A · rating = low", three_way_wire, fixed = TRUE)))
    expect_true(sum(startsWith(three_way_wire, "ROW\t")) >= 25L * 3L * 3L)
  }
})

test_that("all-factor three-way interactions return conditional EMMs and plots", {
  skip_if_not_installed("mice")
  skip_if_not_installed("broom")
  skip_if_not_installed("emmeans")

  set.seed(20260903)
  cells <- expand.grid(
    moment = factor(c("Moment 1", "Moment 2")),
    group = factor(c("Control", "Treatment")),
    country = factor(c("Cyprus", "Finland", "Greece", "Italy")),
    replicate = seq_len(12L)
  )
  cells$y <- with(cells,
    10 + 1.2 * (moment == "Moment 2") + .7 * (group == "Treatment") +
      c(Cyprus = 0, Finland = .4, Greece = -.3, Italy = .8)[country] +
      .9 * (moment == "Moment 2") * (group == "Treatment") *
        (country %in% c("Greece", "Italy")) + stats::rnorm(nrow(cells), sd = .7)
  )
  fits <- lapply(seq_len(3L), function(i) {
    data <- cells
    data$y <- data$y + stats::rnorm(nrow(data), sd = .02 * i)
    stats::lm(y ~ moment * group * country, data = data)
  })
  record <- list(
    predictors = c(
      "moment", "group", "country", "moment:group", "moment:country",
      "group:country", "moment:group:country"
    ),
    fits_by_imputation = fits
  )

  result <- LinkEDA:::.rls_regression_interaction_record(
    record, "moment:group:country"
  )
  expect_identical(result$interaction_type, "factor_factor_factor")
  expect_identical(result$focal, "moment")
  expect_identical(result$conditioning, c("group", "country"))
  expect_equal(nrow(result$estimates), 2L * 2L * 4L)
  expect_equal(nrow(result$comparisons), 2L * 4L)
  expect_equal(nrow(result$plot_data), 2L * 2L * 4L)
  expect_true(all(c("moment", "group", "country") %in% names(result$plot_data)))

  report <- LinkEDA:::.rls_interaction_native_report(result)
  expect_true(any(grepl("Estimated marginal means", report, fixed = TRUE)))
  expect_true(any(grepl("Pairwise comparisons", report, fixed = TRUE)))
  expect_true(any(grepl("Cyprus", report, fixed = TRUE)))
  wire <- LinkEDA:::.rls_interaction_native_plot_payload(result)
  expect_equal(sum(startsWith(wire, "TICK\t")), 2L)
  expect_equal(sum(startsWith(wire, "ROW\t")), 2L * 2L * 4L)
  expect_true(any(grepl("group = Control · country = Cyprus", wire, fixed = TRUE)))

  ordinary_record <- list(
    predictors = record$predictors,
    fit = fits[[1L]]
  )
  ordinary <- LinkEDA:::.rls_regression_interaction_record(
    ordinary_record, "moment:group:country"
  )
  expect_false(ordinary$multiple_imputation)
  expect_identical(ordinary$interaction_type, "factor_factor_factor")
  expect_identical(ordinary$conditioning, c("group", "country"))
  expect_equal(nrow(ordinary$estimates), 2L * 2L * 4L)
  expect_equal(nrow(ordinary$comparisons), 2L * 4L)
  expect_equal(nrow(ordinary$plot_data), 2L * 2L * 4L)
})

test_that("selected-scope effect plots exclude factor levels outside the fitted rows", {
  skip_if_not_installed("mice")
  skip_if_not_installed("broom")
  skip_if_not_installed("emmeans")

  set.seed(20260905)
  n <- 96L
  gender <- factor(
    rep(c("Female", "Male", "Other/Prefer not to say", "NA"), each = n / 4L),
    levels = c("Female", "Male", "Other/Prefer not to say", "NA")
  )
  age <- rep(seq(18, 65, length.out = n / 4L), 4L)
  selected <- which(gender %in% c("Female", "Male"))
  completed <- lapply(seq_len(3L), function(i) {
    data.frame(
      outcome = 4 + .08 * age + .6 * (gender == "Male") +
        .025 * age * (gender == "Male") + stats::rnorm(n, sd = .2),
      age = age + stats::rnorm(n, sd = .01 * i),
      gender = gender
    )
  })
  id <- .make_postestimation_mi_dataset(
    "mi_selected_interaction_levels", completed[[1L]], completed
  )
  on.exit(ls_unregister_dataset(id), add = TRUE)

  model <- ls_new_glm(id)
  model <- ls_glm_set_dependent(model, "outcome")
  for (term in c("age", "gender", "age:gender")) {
    model <- ls_glm_add_predictor(model, term)
  }
  record <- LinkEDA:::.rls_glm_model_record(model)
  record$scope <- "selected"
  record$selected_rows <- selected
  model <- LinkEDA:::.rls_assign_glm_model(record)
  model <- ls_glm_fit(model)

  fitted_record <- LinkEDA:::.rls_glm_model_record(model)
  expect_identical(levels(stats::model.frame(fitted_record$fit)$gender),
                   c("Female", "Male"))
  interaction <- ls_glm_interaction(model, "age:gender", grid_points = 5L)
  expect_setequal(as.character(interaction$plot_data$gender), c("Female", "Male"))
  expect_equal(nrow(interaction$plot_data), 10L)
  wire <- LinkEDA:::.rls_interaction_native_plot_payload(interaction)
  expect_false(any(grepl("Other/Prefer not to say|gender = NA", wire)))

  swapped <- ls_glm_interaction(
    model, "age:gender", focal = "gender", grid_points = 5L
  )
  expect_identical(swapped$focal, "gender")
  expect_identical(swapped$conditioning, "age")
  expect_setequal(as.character(swapped$plot_data$gender), c("Female", "Male"))
  expect_equal(length(unique(swapped$plot_data$age)), 3L)
  swapped_wire <- LinkEDA:::.rls_interaction_native_plot_payload(swapped)
  expect_equal(sum(startsWith(swapped_wire, "TICK\t")), 2L)
  numeric_series_rows <- swapped_wire[startsWith(swapped_wire, "ROW\t")]
  expect_true(any(grepl("ROW\tage = ", numeric_series_rows, fixed = TRUE)))
  numeric_series_labels <- vapply(
    strsplit(numeric_series_rows, "\t", fixed = TRUE), `[[`, character(1L), 2L
  )
  expect_false(any(grepl("[0-9]\\.[0-9]{7,}", numeric_series_labels)))
  expect_error(
    ls_glm_interaction(model, "age:gender", focal = "outcome"),
    "not part of this effect"
  )
})

test_that("continuous effect legend labels are concise without merging series", {
  ordinary <- c(12.345678901, 15.23456789, 18.1012345)
  expect_identical(
    LinkEDA:::.rls_effect_numeric_legend_labels(ordinary),
    c("12.35", "15.23", "18.1")
  )

  close <- c(0.123441, 0.123449, 0.123441)
  labels <- LinkEDA:::.rls_effect_numeric_legend_labels(close)
  expect_equal(length(unique(labels)), 2L)
  expect_identical(labels[[1L]], labels[[3L]])

  table <- data.frame(
    age = ordinary,
    group = factor(c("A", "B", "A")),
    check.names = FALSE
  )
  expect_identical(
    LinkEDA:::.rls_effect_conditioning_labels(table, c("age", "group")),
    c("age = 12.35 · group = A", "age = 15.23 · group = B",
      "age = 18.1 · group = A")
  )
  expect_equal(table$age, ordinary)
})

test_that("partial diagnostic plots use the selected term and R residual definitions", {
  set.seed(20260901)
  n <- 80L
  x <- seq(-1.5, 1.5, length.out = n)
  z <- sin(seq_len(n) / 7)
  g <- factor(rep(c("A", "B"), length.out = n))
  y <- 1.2 + .8 * x - .45 * z + .3 * (g == "B") + stats::rnorm(n, sd = .25)
  fits <- list(
    stats::lm(y ~ x + z + g),
    stats::lm((y + stats::rnorm(n, sd = .02)) ~ x + z + g)
  )
  record <- list(terms = c("x", "z", "g"), fits_by_imputation = fits)

  x_plot <- LinkEDA:::.rls_regression_partial_plot_record(record, "x", "raw")
  z_plot <- LinkEDA:::.rls_regression_partial_plot_record(record, "z", "raw")
  expect_identical(x_plot$term, "x")
  expect_identical(z_plot$term, "z")
  expect_equal(x_plot$imputation_count, 2L)
  expect_false(isTRUE(all.equal(x_plot$rows$contribution, z_plot$rows$contribution)))
  expect_equal(
    x_plot$rows$partial_residual[x_plot$rows$imputation == 1L],
    as.numeric(stats::predict(fits[[1L]], type = "terms")[, "x"]) +
      unname(stats::residuals(fits[[1L]], type = "response")),
    tolerance = 1e-12
  )

  standardized <- LinkEDA:::.rls_regression_partial_plot_record(
    record, "x", "standardized"
  )
  studentized <- LinkEDA:::.rls_regression_partial_plot_record(
    record, "x", "studentized"
  )
  x_contribution <- as.numeric(stats::predict(fits[[1L]], type = "terms")[, "x"])
  residual <- unname(stats::residuals(fits[[1L]]))
  standardized_scale <- unname(stats::rstandard(fits[[1L]]))/residual
  studentized_scale <- unname(stats::rstudent(fits[[1L]]))/residual
  expect_equal(
    standardized$rows$partial_residual[standardized$rows$imputation == 1L],
    (x_contribution + residual)*standardized_scale, tolerance = 1e-12
  )
  expect_equal(
    studentized$rows$partial_residual[studentized$rows$imputation == 1L],
    (x_contribution + residual)*studentized_scale, tolerance = 1e-12
  )

  glm_fit <- stats::glm(I(y > stats::median(y)) ~ x + z + g,
                        family = stats::binomial())
  glm_record <- list(terms = c("x", "z", "g"), fits_by_imputation = list(glm_fit))
  glm_standardized <- LinkEDA:::.rls_regression_partial_plot_record(
    glm_record, "x", "standardized"
  )
  glm_contribution <- as.numeric(stats::predict(glm_fit, type = "terms")[, "x"])
  expect_equal(
    glm_standardized$rows$partial_residual,
    glm_contribution + unname(stats::rstandard(glm_fit, type = "deviance")),
    tolerance = 1e-12
  )
})

test_that("ordered factors and every interaction shape retain MI semantics", {
  skip_if_not_installed("mice")
  skip_if_not_installed("broom")
  skip_if_not_installed("emmeans")

  set.seed(77)
  n <- 120L
  x <- seq(-1, 1, length.out = n)
  z <- cos(seq_len(n) / 9)
  g <- factor(sample(c("A", "B", "C"), n, replace = TRUE), levels = c("A", "B", "C"))
  rating <- ordered(sample(c("low", "mid", "high"), n, replace = TRUE),
                    levels = c("low", "mid", "high"))
  y <- 2 + .7 * x - .25 * z + c(A = 0, B = .4, C = -.2)[g] +
    c(low = -.2, mid = 0, high = .3)[rating] + .3 * x * z + stats::rnorm(n, sd = .3)
  completed <- lapply(1:3, function(i) data.frame(
    y = y + stats::rnorm(n, sd = .01 * i), x = x + i * .002,
    z = z - i * .001, g = g, rating = rating
  ))
  original <- completed[[1L]]
  original$rating[c(18L, 81L)] <- NA
  id <- .make_postestimation_mi_dataset("mi_interaction_shapes", original, completed)
  on.exit(ls_unregister_dataset(id), add = TRUE)

  model <- ls_new_generalized_linear_model(
    id, response = "y",
    terms = c("x", "z", "g", "rating", "x:z", "x:g", "g:rating"),
    family = "gaussian", link = "identity", native = FALSE,
    term_types = list(x = "numeric", z = "numeric", g = "factor", rating = "factor")
  )
  state <- ls_generalized_linear_model_state(model)
  ordered_pairwise <- ls_generalized_linear_model_pairwise(model, "rating")
  expect_equal(ordered_pairwise$imputation_count, rep(3L, nrow(ordered_pairwise)))
  expect_identical(ls_generalized_linear_model_interaction(model, "x:z")$interaction_type,
                   "numeric_numeric")
  expect_identical(ls_generalized_linear_model_interaction(model, "x:g")$interaction_type,
                   "numeric_factor")
  factor_factor <- ls_generalized_linear_model_interaction(model, "g:rating")
  expect_identical(factor_factor$interaction_type, "factor_factor")
  expect_true(nrow(factor_factor$plot_data) == 9L)
  expect_true(all(vapply(state$fits_by_imputation, function(fit) {
    is.factor(stats::model.frame(fit)$rating) &&
      identical(levels(stats::model.frame(fit)$rating), c("low", "mid", "high"))
  }, logical(1L))))
  factor_wire <- LinkEDA:::.rls_interaction_native_plot_payload(factor_factor)
  expect_equal(sum(startsWith(factor_wire, "TICK\t")), 3L)
  expect_equal(sum(startsWith(factor_wire, "ROW\t")), 9L)
})

test_that("EMM rows omit zero tests by default and support explicit reference tests", {
  skip_if_not_installed("mice")
  skip_if_not_installed("broom")
  skip_if_not_installed("emmeans")

  set.seed(20260901)
  n <- 150L
  country <- factor(rep(c("Greece", "Italy", "Finland"), length.out = n))
  group <- factor(rep(c("Control", "Treatment"), each = 3L, length.out = n))
  age <- 12 + seq_len(n) / n * 6
  y <- 1.5 + c(Greece = 1.2, Italy = -.4, Finland = .3)[country] +
    .5 * (group == "Treatment") +
    c(Greece = .7, Italy = -.2, Finland = .1)[country] * (group == "Treatment") +
    .08 * age + stats::rnorm(n, sd = .35)
  completed <- lapply(seq_len(3L), function(i) data.frame(
    y = y + stats::rnorm(n, sd = .01 * i), country = country,
    group = group, age = age + stats::rnorm(n, sd = .005 * i)
  ))
  fits <- lapply(completed, function(data) stats::lm(y ~ country * group + age, data))
  record <- list(
    terms = c("country", "group", "age", "country:group"),
    fits_by_imputation = fits
  )

  default <- LinkEDA:::.rls_regression_interaction_record(record, "country:group")
  zero <- LinkEDA:::.rls_regression_interaction_record(
    record, "country:group", emm_test_reference = 0
  )
  custom <- LinkEDA:::.rls_regression_interaction_record(
    record, "country:group", emm_test_reference = 10
  )
  direct_grid <- emmeans::emmeans(
    mice::as.mira(fits), specs = stats::as.formula("~ country | group")
  )
  direct_zero <- as.data.frame(summary(
    direct_grid, infer = c(TRUE, TRUE), null = 0
  ))
  direct_custom <- as.data.frame(summary(
    direct_grid, infer = c(TRUE, TRUE), null = 10
  ))
  direct_pairwise <- as.data.frame(summary(
    emmeans::contrast(direct_grid, method = "pairwise", adjust = "tukey"),
    infer = c(TRUE, TRUE)
  ))

  expect_identical(default$interaction_type, "factor_factor")
  expect_false(default$emm_reference_test_enabled)
  expect_null(default$emm_test_reference)
  expect_true(all(is.na(default$estimates$pooled_statistic)))
  expect_true(all(is.na(default$estimates$pooled_p_value)))
  expect_equal(default$estimates$pooled_estimate,
               zero$estimates$pooled_estimate, tolerance = 1e-12)
  expect_equal(default$estimates$pooled_conf_low,
               zero$estimates$pooled_conf_low, tolerance = 1e-12)
  expect_equal(default$estimates$pooled_conf_high,
               zero$estimates$pooled_conf_high, tolerance = 1e-12)
  expect_equal(zero$estimates$pooled_statistic,
               direct_zero$t.ratio, tolerance = 1e-10)
  expect_equal(zero$estimates$pooled_p_value,
               direct_zero$p.value, tolerance = 1e-10)
  expect_equal(custom$estimates$pooled_statistic,
               direct_custom$t.ratio, tolerance = 1e-10)
  expect_equal(custom$estimates$pooled_p_value,
               direct_custom$p.value, tolerance = 1e-10)
  expect_equal(default$comparisons$pooled_estimate,
               direct_pairwise$estimate, tolerance = 1e-10)
  expect_equal(default$comparisons$pooled_std_error,
               direct_pairwise$SE, tolerance = 1e-10)
  expect_equal(default$comparisons$pooled_statistic,
               direct_pairwise$t.ratio, tolerance = 1e-10)
  expect_equal(default$comparisons$pooled_p_value,
               direct_pairwise$p.value, tolerance = 1e-10)
  expect_equal(default$comparisons$pooled_conf_low,
               direct_pairwise$lower.CL, tolerance = 1e-10)
  expect_equal(default$comparisons$pooled_conf_high,
               direct_pairwise$upper.CL, tolerance = 1e-10)
  expect_identical(custom$emm_test_reference, 10)
  expect_identical(custom$imputation_count, 3L)
  expect_true(grepl("mice::mira", custom$pooling_method, fixed = TRUE))

  default_text <- LinkEDA:::.rls_interaction_native_report(default)
  zero_text <- LinkEDA:::.rls_interaction_native_report(zero)
  default_emm_header <- match("Estimated marginal means", default_text) + 1L
  default_comparison_header <- match("Pairwise comparisons", default_text) + 1L
  expect_true("EMM reference test: None" %in% default_text)
  expect_true("Multiple comparisons: emmeans with tukey adjustment" %in%
                default_text)
  expect_identical(default_text[[default_emm_header]],
                   "country\tgroup\tEstimate\tSE\tLower 95%\tUpper 95%")
  expect_identical(default_text[[default_comparison_header]],
                   paste("group", "Comparison", "Mean first (response)",
                         "Mean second (response)", "Response difference", "SE",
                         "df", "Statistic", "Adjusted p", "Lower 95% response",
                         "Upper 95% response", sep = "\t"))
  emm_rows <- default_text[seq.int(default_emm_header + 1L,
                                   default_emm_header + nrow(default$estimates))]
  comparison_rows <- default_text[seq.int(
    default_comparison_header + 1L,
    default_comparison_header + nrow(default$comparisons)
  )]
  expect_true(all(vapply(strsplit(emm_rows, "\t", fixed = TRUE), length,
                         integer(1L)) == 6L))
  expect_true(all(vapply(strsplit(comparison_rows, "\t", fixed = TRUE), length,
                         integer(1L)) == 11L))
  expect_setequal(vapply(strsplit(comparison_rows, "\t", fixed = TRUE), `[[`,
                           character(1L), 1L), levels(group))
  expect_true(any(grepl(
    as.character(default$comparisons$contrast[[1L]]), comparison_rows,
    fixed = TRUE
  )))
  for (i in seq_len(nrow(default$comparisons))) {
    condition <- as.character(default$comparisons$group[[i]])
    contrast <- strsplit(
      as.character(default$comparisons$contrast[[i]]), " - ", fixed = TRUE
    )[[1L]]
    expect_length(contrast, 2L)
    left <- default$estimates$pooled_estimate[
      default$estimates$group == condition &
        default$estimates$country == contrast[[1L]]
    ]
    right <- default$estimates$pooled_estimate[
      default$estimates$group == condition &
        default$estimates$country == contrast[[2L]]
    ]
    expect_equal(default$comparisons$pooled_estimate[[i]], left - right,
                 tolerance = 1e-10)
  }
  expect_true(any(grepl(
    "Estimated marginal means for country at each category of group", default_text,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "the interaction itself is tested by its interaction term", default_text,
    fixed = TRUE
  )))
  expect_true("EMM reference test: 0.0000" %in% zero_text)
  expect_true(paste("country", "group", "Estimate", "SE", "Statistic", "p",
                    "Lower 95%", "Upper 95%", sep = "\t") %in% zero_text)
  expect_true(paste("group", "Comparison", "Mean first (response)",
                    "Mean second (response)", "Response difference", "SE", "df",
                    "Statistic", "Adjusted p", "Lower 95% response",
                    "Upper 95% response", sep = "\t") %in%
                zero_text)

  reverse_fits <- lapply(completed, function(data) {
    stats::lm(y ~ group * country + age, data)
  })
  reverse <- LinkEDA:::.rls_regression_interaction_record(
    list(terms = c("group", "country", "age", "group:country"),
         fits_by_imputation = reverse_fits),
    "group:country"
  )
  reverse_text <- LinkEDA:::.rls_interaction_native_report(reverse)
  reverse_emm_header <- match("Estimated marginal means", reverse_text) + 1L
  reverse_comparison_header <- match("Pairwise comparisons", reverse_text) + 1L
  expect_identical(reverse_text[[reverse_emm_header]],
                   "group\tcountry\tEstimate\tSE\tLower 95%\tUpper 95%")
  expect_identical(reverse_text[[reverse_comparison_header]],
                   paste("country", "Comparison", "Mean first (response)",
                         "Mean second (response)", "Response difference", "SE",
                         "df", "Statistic", "Adjusted p", "Lower 95% response",
                         "Upper 95% response", sep = "\t"))
  expect_true(any(grepl(
    "Estimated marginal means for group at each category of country", reverse_text,
    fixed = TRUE
  )))

  unadjusted <- LinkEDA:::.rls_regression_interaction_record(
    record, "country:group", adjust = "none"
  )
  unadjusted_text <- LinkEDA:::.rls_interaction_native_report(unadjusted)
  unadjusted_header <- match("Pairwise comparisons", unadjusted_text) + 1L
  expect_true("Multiple comparisons: emmeans, unadjusted" %in% unadjusted_text)
  expect_identical(
    unadjusted_text[[unadjusted_header]],
    paste("group", "Comparison", "Mean first (response)",
          "Mean second (response)", "Response difference", "SE", "df",
          "Statistic", "p", "Lower 95% response", "Upper 95% response",
          sep = "\t")
  )

  ordinary <- LinkEDA:::.rls_regression_interaction_record(
    list(terms = record$terms, fit = fits[[1L]]), "country:group"
  )
  expect_true(all(is.na(ordinary$estimates$pooled_p_value)))
  ordinary_zero <- LinkEDA:::.rls_regression_interaction_record(
    list(terms = record$terms, fit = fits[[1L]]), "country:group",
    emm_test_reference = 0
  )
  expect_true(all(is.finite(ordinary_zero$estimates$pooled_p_value)))

  positive_age_fit <- stats::lm(age ~ country * group)
  positive_age <- LinkEDA:::.rls_regression_interaction_record(
    list(terms = c("country", "group", "country:group"), fit = positive_age_fit),
    "country:group"
  )
  expect_true(all(positive_age$estimates$pooled_estimate > 0))
  expect_true(all(is.na(positive_age$estimates$pooled_p_value)))
})

test_that("pairwise contrasts and simple slopes retain their inferential tests", {
  skip_if_not_installed("mice")
  skip_if_not_installed("broom")
  skip_if_not_installed("emmeans")

  set.seed(922)
  n <- 120L
  x <- seq(-1, 1, length.out = n)
  g <- factor(rep(c("A", "B", "C"), length.out = n))
  y <- 2 + .6 * x + c(A = 0, B = .4, C = -.25)[g] +
    x * c(A = 0, B = .3, C = -.15)[g] + stats::rnorm(n, sd = .3)
  fits <- lapply(1:3, function(i) {
    completed <- data.frame(
      y = y + stats::rnorm(n, sd = .01 * i), x = x, g = g
    )
    stats::lm(y ~ x * g, data = completed)
  })
  record <- list(terms = c("x", "g", "x:g"), fits_by_imputation = fits)

  slopes <- LinkEDA:::.rls_regression_interaction_record(record, "x:g")
  pairwise <- LinkEDA:::.rls_regression_pairwise_record(record, "g")
  expect_true(all(is.finite(slopes$estimates$pooled_statistic)))
  expect_true(all(is.finite(slopes$estimates$pooled_p_value)))
  expect_true(all(is.finite(pairwise$pooled_statistic)))
  expect_true(all(is.finite(pairwise$pooled_p_value)))
  slope_text <- LinkEDA:::.rls_interaction_native_report(slopes)
  expect_true("Effect\tEstimate\tSE\tStatistic\tp\tLower 95%\tUpper 95%" %in%
                slope_text)
})

test_that("every generalized family and link uses the shared MI post-estimation route", {
  skip_if_not_installed("mice")
  skip_if_not_installed("broom")
  skip_if_not_installed("emmeans")

  set.seed(20260830)
  n <- 240L
  x <- rep(seq(-.45, .45, length.out = 80L), 3L)
  g <- factor(rep(c("A", "B", "C"), each = 80L), levels = c("A", "B", "C"))
  group_effect <- c(A = 0, B = .08, C = -.06)[as.character(g)]
  eta <- .12 * x + group_effect + .05 * x * (g == "B") - .04 * x * (g == "C")
  binary_probability <- stats::plogis(eta)
  positive_mean <- exp(1.6 + eta)
  base <- data.frame(
    y_gaussian = 5 + eta + stats::rnorm(n, sd = .12),
    y_binary = stats::rbinom(n, 1L, binary_probability),
    y_poisson = stats::rpois(n, positive_mean),
    y_gamma = stats::rgamma(n, shape = 25, scale = positive_mean / 25),
    y_inverse_gaussian = pmax(.05, positive_mean + stats::rnorm(n, sd = .12)),
    x = x,
    g = g
  )
  completed <- lapply(seq_len(3L), function(i) {
    value <- base
    value$x <- value$x + stats::rnorm(n, sd = .001 * i)
    value
  })
  original <- completed[[1L]]
  original$x[c(17L, 89L, 201L)] <- NA_real_
  original$g[c(55L, 166L)] <- NA
  id <- .make_postestimation_mi_dataset("mi_all_generalized_links", original, completed)
  on.exit(ls_unregister_dataset(id), add = TRUE)

  cases <- list(
    gaussian = list(response = "y_gaussian", links = c("identity", "log", "inverse")),
    binomial = list(response = "y_binary", links = c("logit", "probit", "cloglog", "cauchit", "log")),
    poisson = list(response = "y_poisson", links = c("log", "identity", "sqrt")),
    Gamma = list(response = "y_gamma", links = c("inverse", "identity", "log")),
    inverse.gaussian = list(response = "y_inverse_gaussian", links = c("1/mu^2", "inverse", "identity", "log")),
    quasibinomial = list(response = "y_binary", links = c("logit", "probit", "cloglog", "cauchit", "log")),
    quasipoisson = list(response = "y_poisson", links = c("log", "identity", "sqrt"))
  )

  for (family in names(cases)) {
    for (link in cases[[family]]$links) {
      model <- ls_new_generalized_linear_model(
        id, response = cases[[family]]$response,
        terms = c("x", "g", "x:g"), family = family, link = link,
        native = FALSE, term_types = list(x = "numeric", g = "factor")
      )
      state <- ls_generalized_linear_model_state(model)
      expect_length(state$fits_by_imputation, 3L)
      pairwise <- ls_generalized_linear_model_pairwise(model, "g")
      expect_true(nrow(pairwise) == 3L)
      expect_true(all(pairwise$imputation_count == 3L))
      interaction <- ls_generalized_linear_model_interaction(model, "x:g", grid_points = 5L)
      expect_identical(interaction$interaction_type, "numeric_factor")
      expect_identical(interaction$imputation_count, 3L)
      expect_true(nrow(interaction$plot_data) == 15L)
    }
  }
})

test_that("simple-effect plots use the pooled emmeans prediction route", {
  skip_if_not_installed("mice")
  skip_if_not_installed("emmeans")

  set.seed(20260905)
  data <- data.frame(
    y = stats::rnorm(80),
    x = seq(-2, 2, length.out = 80),
    g = factor(rep(c("A", "B"), each = 40))
  )
  fits <- lapply(seq_len(3L), function(i) {
    completed <- data
    completed$y <- completed$y + stats::rnorm(nrow(data), sd = .01 * i)
    stats::lm(y ~ x + g, data = completed)
  })
  record <- list(terms = c("x", "g"), fits_by_imputation = fits)

  numeric_effect <- LinkEDA:::.rls_regression_interaction_record(
    record, "x", grid_points = 7L
  )
  expect_identical(numeric_effect$interaction_type, "simple_numeric")
  expect_identical(numeric_effect$imputation_count, 3L)
  expect_equal(nrow(numeric_effect$plot_data), 7L)
  expect_true(all(is.finite(numeric_effect$plot_data$pooled_conf_low)))

  factor_effect <- LinkEDA:::.rls_regression_interaction_record(record, "g")
  expect_identical(factor_effect$interaction_type, "simple_factor")
  expect_equal(nrow(factor_effect$plot_data), 2L)
  payload <- LinkEDA:::.rls_interaction_native_plot_payload(factor_effect)
  expect_true(any(grepl("^CONFIDENCE\\t", payload)))
  expect_equal(sum(grepl("^ROW\\t", payload)), 2L)

  ordinary_record <- list(
    terms = c("x", "g", "x:g"),
    fits_by_imputation = list(stats::lm(y ~ x * g, data = data))
  )
  ordinary_effect <- LinkEDA:::.rls_regression_interaction_record(
    ordinary_record, "x", grid_points = 7L
  )
  ordinary_interaction <- LinkEDA:::.rls_regression_interaction_record(
    ordinary_record, "x:g", grid_points = 7L
  )
  expect_identical(ordinary_effect$imputation_count, 1L)
  expect_false(ordinary_effect$multiple_imputation)
  expect_true(all(is.finite(ordinary_effect$plot_data$pooled_conf_low)))
  expect_identical(ordinary_interaction$imputation_count, 1L)
  expect_false(ordinary_interaction$multiple_imputation)
  expect_identical(ordinary_interaction$interaction_type, "numeric_factor")
  expect_true(all(is.finite(ordinary_interaction$plot_data$pooled_conf_high)))
})
