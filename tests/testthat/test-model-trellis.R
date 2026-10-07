test_that("Model Trellis fits each mtcars panel with the ordinary R lm", {
  panels <- list()
  for (am in c(0, 1)) for (cyl in c(4, 6, 8)) {
    panels[[length(panels) + 1L]] <- list(
      panel_id = paste(am, cyl, sep = ":"),
      row_level_id = as.character(am), row_level_label = as.character(am),
      column_level_id = as.character(cyl), column_level_label = as.character(cyl),
      rows = which(mtcars$am == am & mtcars$cyl == cyl)
    )
  }
  result <- LinkEDA:::.rls_model_trellis_fit(
    mtcars, "wt", "mpg", list(mpg = "numeric"), panels, 0.95, "holm"
  )
  expect_length(result, 6L)
  for (i in seq_along(result)) {
    panel <- result[[i]]
    if (!length(panel$rows)) {
      expect_false(panel$has_observations)
      next
    }
    direct <- stats::lm(wt ~ mpg, data = mtcars[panel$rows, , drop = FALSE])
    expect_equal(panel$record$summary$n_used, stats::nobs(direct))
    expect_equal(panel$record$rows_used, panel$rows)
    if (stats::df.residual(direct) > 0L) {
      row <- panel$coefficient_results[panel$coefficient_results$coefficient_id == "mpg", ]
      direct_row <- summary(direct)$coefficients["mpg", ]
      expect_equal(row$estimate, unname(direct_row[["Estimate"]]), tolerance = 1e-12)
      expect_equal(row$std_error, unname(direct_row[["Std. Error"]]), tolerance = 1e-12)
      expect_equal(row$statistic, unname(direct_row[["t value"]]), tolerance = 1e-12)
      expect_equal(row$p_value, unname(direct_row[["Pr(>|t|)"]]), tolerance = 1e-12)
      expect_equal(c(row$ci_lower, row$ci_upper), unname(stats::confint(direct)["mpg", ]), tolerance = 1e-12)
      expect_equal(panel$record$summary$r_squared, summary(direct)$r.squared, tolerance = 1e-12)
      expect_equal(panel$record$summary$adj_r_squared, summary(direct)$adj.r.squared, tolerance = 1e-12)
      expect_equal(panel$record$summary$residual_se, summary(direct)$sigma, tolerance = 1e-12)
    }
  }
})

test_that("A newly created Model Trellis supports the intercept-only starting model", {
  panels <- lapply(split(seq_len(nrow(mtcars)), mtcars$am), function(rows) list(
    panel_id = paste0("am", mtcars$am[rows[[1L]]]),
    row_level_id = "", row_level_label = "",
    column_level_id = as.character(mtcars$am[rows[[1L]]]),
    column_level_label = as.character(mtcars$am[rows[[1L]]]), rows = rows
  ))
  result <- LinkEDA:::.rls_model_trellis_fit(
    mtcars, "wt", character(), list(), panels, 0.95, "holm"
  )
  expect_length(result, 2L)
  for (panel in result) {
    direct <- stats::lm(wt ~ 1, data = mtcars[panel$rows, , drop = FALSE])
    intercept <- panel$coefficient_results[
      panel$coefficient_results$coefficient_id == "(Intercept)", ]
    expect_equal(intercept$estimate, unname(stats::coef(direct)[[1L]]), tolerance = 1e-12)
    expect_equal(panel$record$summary$n_used, stats::nobs(direct))
  }
})

test_that("Model Trellis p adjustments are R p.adjust families by coefficient id", {
  panels <- lapply(split(seq_len(nrow(mtcars)), mtcars$cyl), function(rows) list(
    panel_id = paste0("cyl", mtcars$cyl[rows[[1L]]]), row_level_id = "",
    row_level_label = "", column_level_id = as.character(mtcars$cyl[rows[[1L]]]),
    column_level_label = as.character(mtcars$cyl[rows[[1L]]]), rows = rows
  ))
  result <- LinkEDA:::.rls_model_trellis_fit(
    mtcars, "wt", "mpg", list(mpg = "numeric"), panels, 0.95, "holm"
  )
  rows <- lapply(result, function(x) x$coefficient_results[x$coefficient_results$coefficient_id == "mpg", ])
  raw <- vapply(rows, `[[`, numeric(1L), "p_value")
  expect_equal(vapply(rows, `[[`, numeric(1L), "p_holm"), stats::p.adjust(raw, "holm"))
  expect_equal(vapply(rows, `[[`, numeric(1L), "p_bonferroni"), stats::p.adjust(raw, "bonferroni"))
})

test_that("Model Trellis retains global factor levels and term tests", {
  panels <- lapply(split(seq_len(nrow(mtcars)), mtcars$am), function(rows) list(
    panel_id = paste0("am", mtcars$am[rows[[1L]]]), row_level_id = as.character(mtcars$am[rows[[1L]]]),
    row_level_label = as.character(mtcars$am[rows[[1L]]]), column_level_id = "",
    column_level_label = "", rows = rows
  ))
  result <- LinkEDA:::.rls_model_trellis_fit(
    mtcars, "wt", c("mpg", "gear"), list(mpg = "numeric", gear = "factor"), panels, 0.95, "holm"
  )
  expect_true(all(vapply(result, function(x) any(x$term_tests$term_id == "gear"), logical(1L))))
  references <- lapply(result, function(x) x$coefficient_results$reference_level[x$coefficient_results$source_term == "gear"])
  expect_true(all(vapply(references, function(x) length(unique(x[nzchar(x)])) <= 1L, logical(1L))))
})
