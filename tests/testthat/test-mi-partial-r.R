mi_partial_fits <- function() {
  set.seed(914)
  d <- data.frame(x = rnorm(240), z = rnorm(240),
                  group = factor(rep(c("Control", "Treatment"), 120)))
  lapply(c(0.10, 0.35, 0.60, 0.85, 1.10), function(slope) {
    d$y <- slope * d$x + 0.3 * d$z + 0.2 * (d$group == "Treatment") +
      0.1 * d$x * (d$group == "Treatment") + rnorm(nrow(d), sd = 2)
    lm(y ~ x * group + z, data = d)
  })
}

residualized_partial_r <- function(fit, term) {
  design <- model.matrix(fit)
  nuisance <- design[, colnames(design) != term, drop = FALSE]
  cor(lm.fit(nuisance, model.response(model.frame(fit)))$residuals,
      lm.fit(nuisance, design[, term])$residuals)
}

test_that("MI partial correlations agree with independent residualization", {
  skip_if_not_installed("mice")
  fits <- mi_partial_fits()
  actual <- LinkEDA:::.rls_mi_pool_coefficients(fits)
  terms <- setdiff(actual$term, "(Intercept)")
  expected <- vapply(terms, function(term)
    tanh(mean(atanh(vapply(fits, residualized_partial_r, numeric(1), term = term)))),
    numeric(1))
  expect_equal(actual$partial_r[match(terms, actual$term)], unname(expected), tolerance = 1e-12)
  expect_true(is.na(actual$partial_r[actual$term == "(Intercept)"]))
  direct <- summary(mice::pool(mice::as.mira(fits)), conf.int = TRUE)
  direct <- direct[match(actual$term, direct$term), ]
  expect_equal(actual$estimate, direct$estimate)
  expect_equal(actual$std_error, direct$std.error)
  expect_equal(actual$t_value, direct$statistic)
  expect_equal(actual$df, direct$df)
  expect_equal(actual$p_value, direct$p.value)
  expect_equal(actual$ci_lower, direct$conf.low)
  expect_equal(actual$ci_upper, direct$conf.high)
  old_r <- actual$t_value / sqrt(actual$t_value^2 + actual$df)
  expect_gt(max(abs(old_r[-1] - actual$partial_r[-1])), 0.05)
})

test_that("identical imputations retain the completed-data partial correlation", {
  skip_if_not_installed("mice")
  fit <- mi_partial_fits()[[3]]
  actual <- LinkEDA:::.rls_mi_pool_coefficients(rep(list(fit), 5))
  expect_equal(actual$partial_r[actual$term == "x"], residualized_partial_r(fit, "x"), tolerance = 1e-12)
  expect_lt(actual$df[actual$term == "x"], df.residual(fit))
})

test_that("non-OLS and unidentifiable coefficients do not get a pseudo correlation", {
  fits <- mi_partial_fits()
  logistic <- lapply(fits, function(fit) glm(I(y > 0) ~ x + z, data = model.frame(fit), family = binomial()))
  expect_true(all(is.na(LinkEDA:::.rls_mi_linear_partial_r(logistic, c("(Intercept)", "x", "z")))))
  aliased <- lapply(fits, function(fit) lm(y ~ x + I(2 * x), data = model.frame(fit)))
  actual <- LinkEDA:::.rls_mi_linear_partial_r(aliased, c("x", "I(2 * x)", "absent"))
  expect_true(is.finite(actual[["x"]]))
  expect_true(all(is.na(actual[c("I(2 * x)", "absent")])))
})

test_that("public verification recipe independently reproduces the partial correlations", {
  fits <- mi_partial_fits()
  code <- LinkEDA:::.rls_verification_mi_partial_r_code("reference_fits")
  expect_false(any(grepl("LinkEDA:::|\\.rls_", code)))
  verification <- new.env(parent = globalenv())
  verification$reference_fits <- fits
  capture.output(eval(parse(text = code), envir = verification))
  result <- verification$pooled_partial_correlations
  expected <- LinkEDA:::.rls_mi_linear_partial_r(fits, result$term)
  expect_equal(result$partial_r, unname(expected), tolerance = 1e-12)
})
