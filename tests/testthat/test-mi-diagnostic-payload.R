test_that("MI diagnostic payload preserves row order and missing values", {
  diagnostics <- data.frame(
    row_id = c(2L, 5L), observed_label = c("case 2", "case\n5"),
    observed = c(0, NA_real_), observed_binary = c(0, 1),
    fitted = c(.25, .75), linear_predictor = c(-1, 1),
    dunn_smyth_residual = c(-.5, .5), raw_residual = c(-.25, .25),
    deviance_residual = c(-.2, .2), pearson_residual = c(-.3, .3),
    working_residual = c(-.4, .4), standardized_residual = c(-.6, .6),
    studentized_residual = c(-.7, .7), leverage = c(.01, .02),
    cooks_distance = c(NA_real_, .03)
  )
  record <- list(diagnostics_by_imputation = list(diagnostics, data.frame()))
  payload <- LinkEDA:::.rls_native_generalized_mi_diagnostics_payload(record)
  number_fields <- c("observed", "observed_binary", "fitted",
    "linear_predictor", "dunn_smyth_residual", "raw_residual",
    "deviance_residual", "pearson_residual", "working_residual",
    "standardized_residual", "studentized_residual", "leverage",
    "cooks_distance")
  expected_rows <- vapply(seq_len(nrow(diagnostics)), function(i) {
    row <- diagnostics[i, , drop = FALSE]
    paste(c(LinkEDA:::.rls_native_wire_integer(row$row_id),
      LinkEDA:::.rls_native_wire_value(row$observed_label),
      vapply(number_fields, function(name)
        LinkEDA:::.rls_native_wire_number(row[[name]]), character(1L))),
      collapse = "\x1f")
  }, character(1L), USE.NAMES = FALSE)
  expect_identical(unname(payload),
    c("GENERALIZED_MI_DIAGNOSTICS_V4", "2", "2", expected_rows, "0"))
})

test_that("large MI diagnostic payload keeps every imputation and row", {
  sample <- data.frame(row_id = seq_len(835L), fitted = rep(.5, 835L))
  record <- list(diagnostics_by_imputation = rep(list(sample), 50L))
  payload <- LinkEDA:::.rls_native_generalized_mi_diagnostics_payload(record)
  expect_length(payload, 2L + 50L * (1L + 835L))
  expect_identical(payload[[1L]], "GENERALIZED_MI_DIAGNOSTICS_V4")
  expect_identical(payload[[2L]], "50")
})
