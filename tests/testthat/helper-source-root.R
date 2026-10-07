linkeda_source_test_root <- function() {
  test_dir <- testthat::test_path()
  candidates <- normalizePath(c(
    file.path(test_dir, "..", ".."),
    file.path(test_dir, "..", "..", "LinkEDA"),
    file.path(test_dir, "..", "..", "00_pkg_src", "LinkEDA")
  ), mustWork = FALSE)
  roots <- candidates[
    file.exists(file.path(candidates, "DESCRIPTION")) &
      dir.exists(file.path(candidates, "src", "core"))
  ]
  if (!length(roots)) {
    testthat::skip("The LinkEDA source tree is unavailable in this test environment.")
  }
  roots[[1L]]
}
