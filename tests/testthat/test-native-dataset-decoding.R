test_that("native decoding skips opaque imputation data and unescaped fields", {
  opaque <- strrep("Ab9+/=", 650000L)
  lines <- c("DATASET", "study%20data", "2", "1", "DATACELLS_PERCENT_V1",
             "x", "character", "50%25%09caf%C3%A9", "plain",
             "IMPUTATION_PROCESS_V1", opaque)
  decode <- utils::URLdecode
  decoded_fields <- character()
  testthat::local_mocked_bindings(URLdecode = function(x) {
    decoded_fields <<- c(decoded_fields, x)
    stopifnot(all(grepl("%", x, fixed = TRUE)))
    decode(x)
  }, .package = "utils")
  actual <- LinkEDA:::.rls_decode_native_dataset_lines(lines)
  expect_identical(actual[2], "study data")
  expect_identical(actual[7], "50%\tcafé")
  expect_identical(tail(actual, 1), opaque)
  expect_identical(decoded_fields, c("study%20data", "50%25%09caf%C3%A9"))
  expect_identical(LinkEDA:::.rls_decode_native_dataset_lines(lines[-5]), lines[-5])
})
