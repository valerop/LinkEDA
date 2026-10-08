mi_mice_result_vector <- function(result) {
  names_expected <- c("F.value", "df1", "df2", "P(>F)", "RIV")
  value <- if (is.matrix(result) || is.data.frame(result)) result[1L, , drop = TRUE] else result
  if (length(value) == length(names_expected) && is.null(names(value))) names(value) <- names_expected
  value
}

expect_linkeda_pool_matches_mice <- function(fits, tolerance = 1e-11) {
  linkeda <- LinkEDA:::.rls_mi_pool_coefficients(fits)
  mice_pool <- mice::pool(mice::as.mira(fits))
  mice_summary <- summary(mice_pool, conf.int = TRUE)
  order <- match(linkeda$term, as.character(mice_summary$term))
  mice_summary <- mice_summary[order, , drop = FALSE]
  mice_rows <- mice_pool$pooled[match(linkeda$term, as.character(mice_pool$pooled$term)), , drop = FALSE]

  expect_equal(linkeda$estimate, mice_summary$estimate, tolerance = tolerance)
  expect_equal(linkeda$std_error, mice_summary$std.error, tolerance = tolerance)
  expect_equal(linkeda$statistic, mice_summary$statistic, tolerance = tolerance)
  expect_equal(linkeda$df, mice_summary$df, tolerance = tolerance)
  expect_equal(linkeda$p_value, mice_summary$p.value, tolerance = tolerance)
  expect_equal(linkeda$ci_lower, mice_summary$conf.low, tolerance = tolerance)
  expect_equal(linkeda$ci_upper, mice_summary$conf.high, tolerance = tolerance)
  expect_equal(linkeda$within_imputation_variance, mice_rows$ubar, tolerance = tolerance)
  expect_equal(linkeda$between_imputation_variance, mice_rows$b, tolerance = tolerance)
  expect_equal(linkeda$total_variance, mice_rows$t, tolerance = tolerance)
  expect_equal(linkeda$relative_increase_variance, mice_rows$riv, tolerance = tolerance)
  expect_equal(linkeda$fraction_missing_information, mice_rows$fmi, tolerance = tolerance)
  expect_identical(attr(linkeda, "pooling_method"), "mice::pool (Rubin's rules)")
  invisible(linkeda)
}

test_that("scalar MI pooling is a thin adapter over mice::pool.scalar", {
  cases <- list(
    list(q = c(1.1, 1.8), u = c(0.20, 0.35), df = 18),
    list(q = rep(2.5, 20), u = rep(0.16, 20), df = 40),
    list(q = c(-5, 5), u = c(0.01, 0.01), df = 10)
  )
  for (case in cases) {
    actual <- LinkEDA:::.rls_mi_pool_scalar(case$q, case$u, df_complete = case$df)
    direct <- mice::pool.scalar(case$q, case$u, n = case$df + 1, k = 1)
    expect_equal(actual$Qbar, direct$qbar, tolerance = 1e-13)
    expect_equal(actual$Ubar, direct$ubar, tolerance = 1e-13)
    expect_equal(actual$B, direct$b, tolerance = 1e-13)
    expect_equal(actual$T, direct$t, tolerance = 1e-13)
    expect_equal(actual$SE, sqrt(direct$t), tolerance = 1e-13)
    expect_equal(actual$df, direct$df, tolerance = 1e-13)
    expect_equal(actual$RIV, direct$r, tolerance = 1e-13)
    expect_equal(actual$FMI, direct$fmi, tolerance = 1e-13)
    expect_identical(actual$pooling_method, "mice::pool.scalar")
  }
  expect_equal(LinkEDA:::.rls_mi_pool_scalar(rep(1, 2), rep(0.25, 2), 28)$B, 0)
  expect_gt(LinkEDA:::.rls_mi_pool_scalar(c(-5, 5), c(.01, .01), 10)$FMI, .99)
  expect_error(
    LinkEDA:::.rls_mi_pool_scalar(c(1, NA), c(.2, .2)),
    "every imputation"
  )
  expect_error(LinkEDA:::.rls_mi_pool_scalar(1, .2), "at least two")
})

test_that("lm coefficients, pooled R squared, factors, and interactions agree with mice", {
  n <- 72L
  x <- seq(-2, 2, length.out = n)
  z <- cos(seq_len(n) / 5)
  g <- factor(rep(c("control", "middle", "high"), length.out = n),
              levels = c("control", "middle", "high"))
  base_y <- 4 + 1.4 * x - .6 * z + ifelse(g == "middle", 1.2, ifelse(g == "high", 2.5, 0)) +
    ifelse(g == "high", .8 * x, 0)
  completed <- lapply(seq_len(5L), function(i) {
    data.frame(y = base_y + sin(seq_len(n) * (i + 1) / 9) * (.12 + i / 100), x = x, z = z, g = g)
  })
  fits <- lapply(completed, function(data) stats::lm(y ~ x * g + z, data = data))
  expect_linkeda_pool_matches_mice(fits)

  linkeda_r2 <- LinkEDA:::.rls_mi_pool_r_squared(fits, adjusted = FALSE)
  mice_r2 <- mice::pool.r.squared(mice::as.mira(fits), adjusted = FALSE)
  expect_equal(
    unname(unlist(linkeda_r2[c("estimate", "conf_low", "conf_high", "fraction_missing_information")])),
    unname(mice_r2[1L, ]),
    tolerance = 1e-12
  )
  linkeda_adj <- LinkEDA:::.rls_mi_pool_r_squared(fits, adjusted = TRUE)
  mice_adj <- mice::pool.r.squared(mice::as.mira(fits), adjusted = TRUE)
  expect_equal(
    unname(unlist(linkeda_adj[c("estimate", "conf_low", "conf_high", "fraction_missing_information")])),
    unname(mice_adj[1L, ]),
    tolerance = 1e-12
  )

  reduced <- LinkEDA:::.rls_mi_reduced_fits(fits, remove_terms = "x:g")
  linkeda_test <- LinkEDA:::.rls_mi_pool_d1(fits, reduced, term_names = "x:g")
  mice_test <- mice::D1(mice::as.mira(fits), mice::as.mira(reduced))
  expected <- mi_mice_result_vector(mice_test$result)
  expect_identical(linkeda_test$method, "mice::D1")
  expect_equal(linkeda_test$F, expected[["F.value"]], tolerance = 1e-12)
  expect_equal(linkeda_test$df1, expected[["df1"]], tolerance = 1e-12)
  expect_equal(linkeda_test$df2, expected[["df2"]], tolerance = 1e-12)
  expect_equal(linkeda_test$p, expected[["P(>F)"]], tolerance = 1e-12)
})

test_that("Gaussian, binomial, and Poisson GLM coefficients agree with mice", {
  n <- 90L
  x <- seq(-1.8, 1.8, length.out = n)
  z <- sin(seq_len(n) / 7)
  g <- factor(rep(c("A", "B", "C"), length.out = n), levels = c("A", "B", "C"))
  gaussian_base <- 2 + .7 * x - .4 * z + ifelse(g == "B", .5, ifelse(g == "C", -.6, 0))
  binomial_base <- rep(c(0, 1, 0, 1, 1, 0), length.out = n)
  poisson_base <- as.integer(1 + (seq_len(n) * 7L) %% 9L)
  completed <- lapply(seq_len(5L), function(i) {
    binomial <- binomial_base
    binomial[seq(i + 3L, n, by = 23L)] <- 1L - binomial[seq(i + 3L, n, by = 23L)]
    poisson <- poisson_base
    poisson[seq(i + 2L, n, by = 19L)] <- poisson[seq(i + 2L, n, by = 19L)] + 1L
    data.frame(
      gaussian = gaussian_base + cos(seq_len(n) * i / 11) * .1,
      binomial = binomial,
      poisson = poisson,
      x = x, z = z, g = g
    )
  })

  gaussian_fits <- lapply(completed, function(data) stats::glm(gaussian ~ x + z + g, data, family = stats::gaussian()))
  binomial_fits <- lapply(completed, function(data) stats::glm(binomial ~ x + z + g, data, family = stats::binomial()))
  poisson_fits <- lapply(completed, function(data) stats::glm(poisson ~ x + z + g, data, family = stats::poisson()))
  expect_linkeda_pool_matches_mice(gaussian_fits)
  binomial_pool <- expect_linkeda_pool_matches_mice(binomial_fits)
  poisson_pool <- expect_linkeda_pool_matches_mice(poisson_fits)
  expect_equal(exp(binomial_pool$estimate), exp(summary(mice::pool(mice::as.mira(binomial_fits)))$estimate))
  expect_equal(exp(poisson_pool$estimate), exp(summary(mice::pool(mice::as.mira(poisson_fits)))$estimate))
})

test_that("D1 zero-RIV boundary and small-m D3 fallback remain mice-led", {
  group <- factor(rep(c("a", "b", "c"), each = 4L))
  completed <- list(
    data.frame(y = c(2.1, 2.5, 2.8, 3.2, 5.1, 5.4, 5.7, 6, 8.1, 8.5, 8.9, 9.2), group),
    data.frame(y = c(2.1, 2.7, 2.8, 3.2, 5.1, 5.4, 5.9, 6, 8.1, 8.5, 8.9, 9.4), group)
  )
  full <- lapply(completed, function(data) stats::lm(y ~ group, data))
  reduced <- lapply(completed, function(data) stats::lm(y ~ 1, data))
  boundary <- LinkEDA:::.rls_mi_pool_d1(full, reduced, "group")
  direct <- mice::D1(mice::as.mira(full), mice::as.mira(reduced))
  direct_values <- mi_mice_result_vector(direct$result)
  expect_true(boundary$zero_riv_limit)
  expect_equal(boundary$F, direct_values[["F.value"]], tolerance = 1e-12)
  expect_equal(boundary$df2, direct$dfcom)
  expect_equal(boundary$p, stats::pf(boundary$F, boundary$df1, boundary$df2, lower.tail = FALSE))

  d <- data.frame(
    y = c(2.1, 3.2, 4.9, 6.2, 8.1, 9.7, 11.8, 13),
    x = c(.5, 1, 1.8, 2.5, 3.1, 3.8, 4.5, 5.2),
    w = c(1, 0, 1, 0, 1, 0, 1, 0),
    z = c(.2, 1.4, .8, 2.1, 1.2, 2.8, 2, 3.3)
  )
  d2 <- transform(d, y = y + c(0, .3, 0, 0, 0, .5, 0, 0))
  full <- list(stats::lm(y ~ x + w + z, d), stats::lm(y ~ x + w + z, d2))
  reduced <- list(stats::lm(y ~ x, d), stats::lm(y ~ x, d2))
  fallback <- LinkEDA:::.rls_mi_pool_d1(full, reduced, c("w", "z"))
  direct_d3 <- mice::D3(mice::as.mira(full), mice::as.mira(reduced))
  expected_d3 <- mi_mice_result_vector(direct_d3$result)
  expect_match(fallback$method, "mice::D3", fixed = TRUE)
  expect_equal(fallback$F, expected_d3[["F.value"]], tolerance = 1e-12)
  expect_equal(fallback$df2, expected_d3[["df2"]], tolerance = 1e-12)
  expect_equal(fallback$p, expected_d3[["P(>F)"]], tolerance = 1e-12)
  expect_identical(fallback$fallback_from, "mice::D1")
  expect_true(fallback$d3_attempted)
  expect_true(fallback$d3_available)
})

test_that("D3 is never attempted for quasi-likelihood MI models", {
  d1 <- data.frame(
    y = c(2, 3, 5, 6, 8, 10, 12, 13),
    x = c(.5, 1, 1.8, 2.5, 3.1, 3.8, 4.5, 5.2),
    w = c(1, 0, 1, 0, 1, 0, 1, 0),
    z = c(.2, 1.4, .8, 2.1, 1.2, 2.8, 2, 3.3)
  )
  d2 <- transform(d1, y = y + c(0, 1, 0, 0, 0, 1, 0, 0))
  full <- list(
    stats::glm(y ~ x + w + z, d1, family = stats::quasipoisson()),
    stats::glm(y ~ x + w + z, d2, family = stats::quasipoisson())
  )
  reduced <- list(
    stats::glm(y ~ x, d1, family = stats::quasipoisson()),
    stats::glm(y ~ x, d2, family = stats::quasipoisson())
  )
  result <- LinkEDA:::.rls_mi_pool_d1(full, reduced, c("w", "z"))

  expect_identical(result$method, "mice::D1")
  expect_false(result$ok)
  expect_false(result$d3_available)
  expect_false(result$d3_attempted)
  expect_match(result$fallback_reason, "D3 was not attempted", fixed = TRUE)
  expect_match(result$fallback_reason, "quasi-likelihood", fixed = TRUE)
})

test_that("negative-binomial D3 implementation failures remain explicit and are not reimplemented", {
  skip_if_not_installed("MASS")
  d1 <- data.frame(
    y = c(2, 3, 5, 6, 8, 10, 12, 13),
    x = c(.5, 1, 1.8, 2.5, 3.1, 3.8, 4.5, 5.2),
    w = c(1, 0, 1, 0, 1, 0, 1, 0),
    z = c(.2, 1.4, .8, 2.1, 1.2, 2.8, 2, 3.3)
  )
  d2 <- transform(d1, y = y + c(0, 1, 0, 0, 0, 1, 0, 0))
  full <- suppressWarnings(list(
    MASS::glm.nb(y ~ x + w + z, d1),
    MASS::glm.nb(y ~ x + w + z, d2)
  ))
  reduced <- suppressWarnings(list(
    MASS::glm.nb(y ~ x, d1),
    MASS::glm.nb(y ~ x, d2)
  ))
  result <- LinkEDA:::.rls_mi_pool_d1(full, reduced, c("w", "z"))

  expect_identical(result$method, "mice::D1")
  expect_false(result$ok)
  expect_true(result$d3_available)
  expect_true(result$d3_attempted)
  expect_match(result$fallback_reason, "mice::D3 failed", fixed = TRUE)
})

test_that("mice coefficient pooling has direct broom support for all LinkEDA MI regression classes", {
  skip_if_not_installed("broom")
  skip_if_not_installed("MASS")
  n <- 120L
  x <- seq(-1.5, 1.5, length.out = n)
  z <- rep(c(0, 1), length.out = n)
  set.seed(20260827)
  binary_base <- stats::rbinom(n, 1, stats::plogis(-.2 + .5 * x + .2 * z))
  count_base <- stats::rnbinom(n, mu = exp(.8 + .3 * x + .2 * z), size = 3)
  binary_data <- lapply(seq_len(5L), function(i) {
    y <- binary_base
    index <- seq(i + 5L, n, by = 31L)
    y[index] <- 1L - y[index]
    data.frame(y, x, z)
  })
  count_data <- lapply(seq_len(5L), function(i) {
    y <- count_base
    index <- seq(i + 4L, n, by = 29L)
    y[index] <- y[index] + 1L
    data.frame(y, x, z)
  })
  cases <- list(
    binomial_logit = list(
      data = binary_data,
      fit = function(data) stats::glm(y ~ x + z, data, family = stats::binomial("logit")),
      class = "glm", tidy = "glm", glance = "glm", d3 = TRUE
    ),
    binomial_probit = list(
      data = binary_data,
      fit = function(data) stats::glm(y ~ x + z, data, family = stats::binomial("probit")),
      class = "glm", tidy = "glm", glance = "glm", d3 = TRUE
    ),
    binomial_cloglog = list(
      data = binary_data,
      fit = function(data) stats::glm(y ~ x + z, data, family = stats::binomial("cloglog")),
      class = "glm", tidy = "glm", glance = "glm", d3 = TRUE
    ),
    poisson = list(
      data = count_data,
      fit = function(data) stats::glm(y ~ x + z, data, family = stats::poisson("log")),
      class = "glm", tidy = "glm", glance = "glm", d3 = TRUE
    ),
    quasipoisson = list(
      data = count_data,
      fit = function(data) stats::glm(y ~ x + z, data, family = stats::quasipoisson("log")),
      class = "glm", tidy = "glm", glance = "glm", d3 = FALSE
    ),
    negative_binomial = list(
      data = count_data,
      fit = function(data) MASS::glm.nb(y ~ x + z, data, link = "log"),
      class = "negbin", tidy = "negbin", glance = "negbin", d3 = TRUE
    )
  )

  for (case_name in names(cases)) {
    case <- cases[[case_name]]
    fits <- lapply(case$data, case$fit)
    reduced <- lapply(case$data, function(data) stats::update(case$fit(data), . ~ . - z))
    expect_true(inherits(fits[[1L]], case$class), info = case_name)
    expect_false(is.null(utils::getS3method(
      "tidy", case$tidy, optional = TRUE, envir = asNamespace("broom")
    )), info = case_name)
    expect_false(is.null(utils::getS3method(
      "glance", case$glance, optional = TRUE, envir = asNamespace("broom")
    )), info = case_name)
    expect_linkeda_pool_matches_mice(fits)
    pooled_test <- LinkEDA:::.rls_mi_pool_d1(fits, reduced, "z")
    expect_match(pooled_test$method, "mice::D1", fixed = TRUE, info = case_name)
    expect_identical(pooled_test$d3_available, case$d3, info = case_name)
  }
})

test_that("incompatible or failed imputation fits are never silently pooled", {
  d <- data.frame(
    y = c(1.2, 2.1, 2.9, 4.2, 5.1, 5.8, 7.2, 8.1, 8.8),
    x = seq_len(9),
    g = factor(rep(c("A", "B", "C"), 3), levels = c("A", "B", "C"))
  )
  good <- stats::lm(y ~ x + g, d)
  expect_error(LinkEDA:::.rls_mi_pool_coefficients(list(good, NULL)), "fit.*2 failed")

  absent_level <- stats::lm(y ~ x + g, droplevels(d[d$g != "C", ]))
  expect_error(
    LinkEDA:::.rls_mi_pool_coefficients(list(good, absent_level)),
    "different coefficient sets|factor levels"
  )

  changed_reference_data <- transform(d, g = stats::relevel(g, ref = "B"))
  changed_reference <- stats::lm(y ~ x + g, changed_reference_data)
  expect_error(
    LinkEDA:::.rls_mi_pool_coefficients(list(good, changed_reference)),
    "different coefficient sets|reference"
  )

  singular_data <- transform(d, duplicate_x = x)
  singular <- stats::lm(y ~ x + duplicate_x, singular_data)
  expect_error(
    LinkEDA:::.rls_mi_pool_coefficients(list(singular, singular)),
    "aliased|non-finite"
  )
  expect_false(exists(".rls_mi_pool_wald", envir = asNamespace("LinkEDA"), inherits = FALSE))
  expect_false(exists(".rls_mi_pool_wald_for_fits", envir = asNamespace("LinkEDA"), inherits = FALSE))
  expect_false(exists(".rls_mi_solve", envir = asNamespace("LinkEDA"), inherits = FALSE))
})
