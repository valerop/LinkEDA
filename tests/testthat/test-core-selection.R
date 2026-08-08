core_test_root <- function() {
  test_dir <- testthat::test_path()
  candidates <- normalizePath(c(
    file.path(test_dir, "..", ".."),
    file.path(test_dir, "..", "..", "rlispstat"),
    file.path(test_dir, "..", "..", "LinkEDA"),
    file.path(test_dir, "..", "..", "00_pkg_src", "rlispstat"),
    file.path(test_dir, "..", "..", "00_pkg_src", "LinkEDA")
  ), mustWork = FALSE)
  roots <- candidates[
    file.exists(file.path(candidates, "DESCRIPTION")) &
      dir.exists(file.path(candidates, "src", "core"))
  ]
  if (!length(roots)) stop("Cannot locate the rlispstat source tree for core tests.")
  roots[[1L]]
}

core_test_compiler <- function() {
  r_bin <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
  compiler <- tryCatch(
    system2(r_bin, c("CMD", "config", "CXX17"), stdout = TRUE, stderr = TRUE),
    error = function(e) character()
  )
  trimws(compiler[1])
}

core_sources <- function(root) {
  list.files(file.path(root, "src", "core"), pattern = "[.]cpp$", full.names = TRUE)
}

.core_test_build_cache <- new.env(parent = emptyenv())

core_test_library <- function(root, compiler) {
  key <- normalizePath(root, winslash = "/", mustWork = TRUE)
  if (exists(key, envir = .core_test_build_cache, inherits = FALSE)) {
    library <- get(key, envir = .core_test_build_cache, inherits = FALSE)
    if (file.exists(library)) return(library)
  }
  build_dir <- tempfile("rlispstat-core-library-")
  dir.create(build_dir, recursive = TRUE)
  compile_cmd <- paste(
    "cd", shQuote(build_dir), "&&", compiler, "-std=c++17",
    "-I", shQuote(file.path(root, "src")), "-c",
    paste(shQuote(core_sources(root)), collapse = " ")
  )
  if (system(compile_cmd) != 0L) stop("Failed to compile the portable core test library.")
  objects <- list.files(build_dir, pattern = "[.]o$", full.names = TRUE)
  if (!length(objects)) stop("The portable core test library produced no object files.")
  library <- file.path(build_dir, "librlispstat-core.a")
  archiver <- Sys.which("ar")
  if (!nzchar(archiver) || system(paste(shQuote(archiver), "rcs", shQuote(library),
                                           paste(shQuote(objects), collapse = " "))) != 0L) {
    stop("Failed to archive the portable core test library.")
  }
  assign(key, library, envir = .core_test_build_cache)
  library
}

run_core_cpp_test <- function(test_cpp, exe_prefix) {
  compiler <- core_test_compiler()
  skip_if(!nzchar(compiler), "No C++17 compiler is configured for this R installation.")

  root <- core_test_root()
  test_file <- file.path(root, "tests", "core", test_cpp)
  exe <- tempfile(exe_prefix)
  if (.Platform$OS.type == "windows") exe <- paste0(exe, ".exe")

  library <- core_test_library(root, compiler)

  cmd <- paste(
    compiler,
    "-std=c++17",
    "-I", shQuote(file.path(root, "src")),
    shQuote(test_file),
    shQuote(library),
    "-o", shQuote(exe)
  )
  build_status <- system(cmd)
  expect_identical(build_status, 0L)

  run_status <- system(shQuote(exe))
  expect_identical(run_status, 0L)
}

core_cpp_tests <- data.frame(
  label = c(
    "portable analysis-scope core",
    "portable selection core",
    "portable backend runtime",
    "portable plot geometry core",
    "portable command parsing core",
    "portable correlation model core",
    "portable dimensionality model core",
    "portable dendrogram model core",
    "portable recording core",
    "portable scatterplot hit testing",
    "portable scatter matrix hit testing",
    "portable barplot geometry",
    "portable boxplot geometry",
    "portable histogram geometry",
    "portable model terms core",
    "portable statistical formatting core",
    "portable export capability core",
    "portable SVG writer core",
    "portable Snapshot Album core",
    "portable generalized linear model metadata core",
    "portable statistical distributions core",
    "portable dataset state",
    "portable Table 1 core",
    "portable mean-comparison result core",
    "portable dataset protocol parsing",
    "portable main R task queue core",
    "portable time-series plot core",
    "portable Model Trellis core",
    "portable trellis scatterplot core",
    "portable session controller and command dispatcher",
    "portable Welcome Window and launch decision"
  ),
  test_cpp = c(
    "analysis_scope_test.cpp",
    "selection_model_test.cpp",
    "backend_runtime_test.cpp",
    "plot_geometry_test.cpp",
    "command_model_test.cpp",
    "correlation_model_test.cpp",
    "dimensionality_model_test.cpp",
    "dendrogram_model_test.cpp",
    "recording_model_test.cpp",
    "scatterplot_model_test.cpp",
    "scatter_matrix_model_test.cpp",
    "barplot_model_test.cpp",
    "boxplot_model_test.cpp",
    "histogram_model_test.cpp",
    "model_terms_test.cpp",
    "format_model_test.cpp",
    "export_model_test.cpp",
    "svg_writer_test.cpp",
    "snapshot_album_model_test.cpp",
    "glm_model_test.cpp",
    "statistics_model_test.cpp",
    "dataset_model_test.cpp",
    "table1_model_test.cpp",
    "mean_comparison_model_test.cpp",
    "dataset_protocol_test.cpp",
    "main_r_task_model_test.cpp",
    "time_series_model_test.cpp",
    "model_trellis_model_test.cpp",
    "trellis_scatterplot_model_test.cpp",
    "session_controller_test.cpp",
    "welcome_model_test.cpp"
  ),
  prefix = c(
    "rlispstat-analysis-scope-core",
    "rlispstat-selection-core",
    "rlispstat-backend-runtime-core",
    "rlispstat-plot-geometry-core",
    "rlispstat-command-core",
    "rlispstat-correlation-core",
    "rlispstat-dimensionality-core",
    "rlispstat-dendrogram-core",
    "rlispstat-recording-core",
    "rlispstat-scatterplot-core",
    "rlispstat-scatter-matrix-core",
    "rlispstat-barplot-core",
    "rlispstat-boxplot-core",
    "rlispstat-histogram-core",
    "rlispstat-model-terms-core",
    "rlispstat-format-core",
    "rlispstat-export-core",
    "rlispstat-svg-writer-core",
    "rlispstat-snapshot-album-core",
    "rlispstat-glm-core",
    "rlispstat-statistics-core",
    "rlispstat-dataset-core",
    "rlispstat-table1-core",
    "rlispstat-mean-comparison-core",
    "rlispstat-dataset-protocol-core",
    "rlispstat-main-r-task-core",
    "rlispstat-time-series-core",
    "rlispstat-model-trellis-core",
    "rlispstat-trellis-scatterplot-core",
    "rlispstat-session-controller-core",
    "rlispstat-welcome-core"
  ),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(core_cpp_tests))) {
  local({
    case <- core_cpp_tests[i, ]
    test_that(paste(case$label, "compiles and passes without AppKit"), {
      run_core_cpp_test(case$test_cpp, case$prefix)
    })
  })
}

test_that("Windows EMF renderer and clipboard integration are genuinely vectorial", {
  skip_if(.Platform$OS.type != "windows", "Windows GDI and clipboard are required.")
  compiler <- core_test_compiler()
  skip_if(!nzchar(compiler), "No C++17 compiler is configured for this R installation.")
  root <- core_test_root()
  library <- core_test_library(root, compiler)
  exe <- paste0(tempfile("rlispstat-windows-emf-"), ".exe")
  command <- paste(
    compiler, "-std=c++17", "-I", shQuote(file.path(root, "src")),
    shQuote(file.path(root, "tests", "core", "windows_emf_export_test.cpp")),
    shQuote(file.path(root, "src", "platform", "windows", "windows_emf_export.cpp")),
    shQuote(library), "-lgdi32 -luser32 -o", shQuote(exe)
  )
  expect_identical(system(command), 0L)
  expect_identical(system(shQuote(exe)), 0L)
})
