test_that("all structured table payloads share UTF-8-safe wire values", {
  input <- c("Espa\u00f1a", "Mann\u2013Whitney", "first\tsecond")
  expected <- c("Espa\u00f1a", "Mann\u2013Whitney", "first second")

  helpers <- list(
    native = LinkEDA:::.rls_native_wire_value,
    correlation = LinkEDA:::.rls_correlation_wire_value,
    table1 = LinkEDA:::.rls_table1_wire_value
  )
  for (helper in helpers) {
    actual <- helper(input)
    expect_identical(actual, expected)
    expect_true(all(!is.na(iconv(actual, from = "UTF-8", to = "UTF-8"))))
  }
})

test_that("data-sheet payloads preserve Unicode and never contain NUL bytes", {
  payload <- LinkEDA:::.rls_dataframe_payload(data.frame(
    label = c("Espa\u00f1a", "Mann\u2013Whitney", "line\tbreak"),
    stringsAsFactors = FALSE
  ))

  expect_true(all(!is.na(iconv(payload, from = "UTF-8", to = "UTF-8"))))
  connection <- rawConnection(raw(), "wb")
  on.exit(close(connection), add = TRUE)
  expect_silent(writeLines(payload, connection, useBytes = TRUE))
  expect_false(any(rawConnectionValue(connection) == as.raw(0L)))
})

test_that("native linear-design payload carries fitted centering constants", {
  data <- data.frame(y = c(3.1, 4.8, 7.1, 9), x = c(1, 2, 3, 4))
  fit <- stats::lm(y ~ x, data = data)
  payload <- LinkEDA:::.rls_native_linear_design_payload(list(
    fit = fit,
    predictor_centers = c(x = 2.5)
  ))
  marker <- match("PREDICTOR_CENTERS", payload)
  expect_false(is.na(marker))
  expect_identical(payload[[marker + 1L]], "1")
  expect_identical(payload[[marker + 2L]], "x")
  expect_equal(as.numeric(payload[[marker + 3L]]), 2.5)
})
