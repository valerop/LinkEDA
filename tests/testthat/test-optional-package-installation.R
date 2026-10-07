test_that("missing optional packages carry a structured native-install request", {
  testthat::local_mocked_bindings(
    .rls_activate_optional_library = function(...) invisible("unused"),
    .rls_namespace_available = function(package) FALSE,
    .package = "LinkEDA"
  )

  condition <- tryCatch(
    LinkEDA:::.rls_require_optional_packages(
      c("betareg", "glmmTMB"), "Bounded-response model"
    ),
    error = identity
  )
  expect_s3_class(condition, "linkeda_missing_packages")
  expect_identical(condition$packages, c("betareg", "glmmTMB"))
  expect_identical(condition$context, "Bounded-response model")
  expect_match(conditionMessage(condition), "private user library", fixed = TRUE)
})
test_that("LinkEDA installs only allow-listed packages in its private library", {
  library <- file.path(tempdir(), paste0("linkeda-library-", Sys.getpid()))
  withr::local_options(list(LinkEDA.optional_library = library))
  withr::local_libpaths(.libPaths())
  installed <- FALSE
  captured <- NULL
  testthat::local_mocked_bindings(
    .rls_namespace_available = function(package) installed,
    .rls_install_packages_impl = function(packages, lib, repos) {
      captured <<- list(packages = packages, lib = lib, repos = repos)
      installed <<- TRUE
      invisible(NULL)
    },
    .package = "LinkEDA"
  )

  result <- LinkEDA:::.rls_install_optional_packages(c("betareg", "betareg"))
  expect_true(dir.exists(result))
  expect_identical(captured$packages, "betareg")
  expect_identical(normalizePath(captured$lib, winslash = "/"),
                   normalizePath(library, winslash = "/"))
  expect_error(
    LinkEDA:::.rls_install_optional_packages("not-a-LinkEDA-package"),
    "refused an unknown optional package request",
    fixed = TRUE
  )
  expect_identical(
    LinkEDA:::.rls_optional_package_type(),
    if (.Platform$OS.type == "windows") "binary" else getOption("pkgType")
  )
})

test_that("native package prompts are shared by Windows and macOS", {
  root <- linkeda_source_test_root()
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  state <- paste(readLines(file.path(root, "R", "state.R"),
                           warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  for (source in list(windows, mac)) {
    expect_match(source, '"R_PACKAGES_REQUIRED"', fixed = TRUE)
    expect_match(source, '"R_PACKAGE_INSTALL_RESULT"', fixed = TRUE)
    expect_match(source, 'task.kind = "install_r_packages"', fixed = TRUE)
    expect_match(source, '"OK install"', fixed = TRUE)
    expect_match(source, '"OK cancel"', fixed = TRUE)
  }
  expect_match(state, 'identical(kind, "install_r_packages")', fixed = TRUE)
  expect_match(state, '"R_PACKAGE_INSTALL_RESULT"', fixed = TRUE)
  expect_match(state, '.rls_send <- function(lines, expect_reply = TRUE, timeout = 5)',
               fixed = TRUE)
  optional_packages <- paste(readLines(file.path(root, "R", "optional_packages.R"),
                                       warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(optional_packages, '.rls_send(payload, timeout = 300)', fixed = TRUE)
  expect_match(optional_packages, 'if (.Platform$OS.type == "windows") "binary"',
               fixed = TRUE)
  compare_means <- paste(readLines(file.path(root, "R", "compare_means.R"),
                                  warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(compare_means,
               '.rls_send_native_analysis_result <- function(payload, description, timeout = 5)',
               fixed = TRUE)
  expect_match(state, '"optional R package installation result", timeout = 300)',
               fixed = TRUE)
})
