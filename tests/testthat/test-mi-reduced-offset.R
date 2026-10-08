test_that("MI reduced models retain every fitted offset when terms are removed", {
  skip_if_not_installed("mice")
  set.seed(20260924)
  n <- 120L
  source <- data.frame(
    x = stats::rnorm(n), z = stats::rnorm(n),
    exposure = stats::runif(n, 0.7, 2.2),
    known = stats::runif(n, -0.3, 0.3)
  )
  source$y <- stats::rpois(
    n, source$exposure * exp(0.2 + 0.4 * source$x - 0.25 * source$z + source$known)
  )
  completed <- lapply(seq_len(3L), function(i) {
    transform(source, x = x + stats::rnorm(n, sd = 0.01 * i))
  })
  full <- lapply(completed, function(data) stats::glm(
    y ~ x + z + offset(log(exposure)), offset = known,
    family = stats::poisson(), data = data
  ))
  intercept <- LinkEDA:::.rls_mi_reduced_fits(full, intercept_only = TRUE)
  without_z <- LinkEDA:::.rls_mi_reduced_fits(full, remove_terms = "z")
  for (i in seq_along(completed)) {
    data <- completed[[i]]
    direct_intercept <- stats::glm(
      y ~ offset(log(exposure)) + offset(known),
      family = stats::poisson(), data = data
    )
    direct_without_z <- stats::glm(
      y ~ x + offset(log(exposure)) + offset(known),
      family = stats::poisson(), data = data
    )
    expect_equal(unname(stats::coef(intercept[[i]])),
                 unname(stats::coef(direct_intercept)), tolerance = 1e-10)
    expect_equal(unname(stats::coef(without_z[[i]])),
                 unname(stats::coef(direct_without_z)), tolerance = 1e-10)
    expect_equal(unname(stats::model.offset(stats::model.frame(intercept[[i]]))),
                 unname(stats::model.offset(stats::model.frame(full[[i]]))),
                 tolerance = 1e-12)
  }
  pooled <- LinkEDA:::.rls_mi_pool_d1(full, intercept)
  expect_true(is.finite(pooled$p))
})
