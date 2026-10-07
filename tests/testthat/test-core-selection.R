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
  if (.Platform$OS.type == "windows") {
    configured <- Sys.getenv("CXX17", unset = "")
    on_path <- unname(Sys.which(c("g++", "clang++")))
    rtools_roots <- unique(c(
      Sys.getenv("RTOOLS44_HOME", unset = ""),
      Sys.getenv("RTOOLS_HOME", unset = ""),
      "C:/rtools44"
    ))
    rtools_roots <- rtools_roots[nzchar(rtools_roots)]
    rtools <- file.path(
      rtools_roots,
      "x86_64-w64-mingw32.static.posix", "bin", "g++.exe"
    )
    candidates <- c(configured, on_path[nzchar(on_path)], rtools)
    candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
    return(if (length(candidates)) normalizePath(candidates[[1L]], winslash = "/") else "")
  }
  r_bin <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
  compiler <- tryCatch(
    system2(r_bin, c("CMD", "config", "CXX17"), stdout = TRUE, stderr = TRUE),
    error = function(e) character()
  )
  compiler <- trimws(compiler[1])
  # Recent R toolchains may report a compiler command with architecture flags
  # (for example, "clang++ -arch arm64").  The test builder quotes the
  # executable path separately, so resolve only the command token here.
  executable <- strsplit(compiler, "[[:space:]]+")[[1L]][1L]
  resolved <- unname(Sys.which(executable))
  if (nzchar(resolved)) normalizePath(resolved) else executable
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
  sources <- core_sources(root)
  objects <- file.path(build_dir, paste0(tools::file_path_sans_ext(basename(sources)), ".o"))
  for (i in seq_along(sources)) {
    compile_cmd <- paste(
      shQuote(compiler), "-std=c++17",
      "-I", shQuote(file.path(root, "src")), "-c",
      shQuote(sources[[i]]), "-o", shQuote(objects[[i]])
    )
    if (system(compile_cmd) != 0L) stop("Failed to compile the portable core test library.")
  }
  if (!length(objects)) stop("The portable core test library produced no object files.")
  library <- file.path(build_dir, "librlispstat-core.a")
  adjacent_archiver <- file.path(dirname(compiler), if (.Platform$OS.type == "windows") "ar.exe" else "ar")
  archiver <- if (file.exists(adjacent_archiver)) adjacent_archiver else Sys.which("ar")
  if (!nzchar(archiver) || system(paste(shQuote(archiver), "rcs", shQuote(library),
                                           paste(shQuote(objects), collapse = " "))) != 0L) {
    stop("Failed to archive the portable core test library.")
  }
  assign(key, library, envir = .core_test_build_cache)
  library
}

run_core_cpp_test <- function(test_cpp, exe_prefix, run_args = character()) {
  compiler <- core_test_compiler()
  skip_if(!nzchar(compiler), "No C++17 compiler is configured for this R installation.")

  root <- core_test_root()
  test_file <- file.path(root, "tests", "core", test_cpp)
  exe <- tempfile(exe_prefix)
  if (.Platform$OS.type == "windows") exe <- paste0(exe, ".exe")

  library <- core_test_library(root, compiler)

  cmd <- paste(
    shQuote(compiler),
    "-std=c++17",
    "-I", shQuote(file.path(root, "src")),
    shQuote(test_file),
    shQuote(library),
    "-o", shQuote(exe)
  )
  build_status <- system(cmd)
  expect_identical(build_status, 0L)

  run_command <- paste(c(shQuote(exe), shQuote(run_args)), collapse = " ")
  run_status <- system(run_command)
  expect_identical(run_status, 0L)
}

core_cpp_tests <- data.frame(
  label = c(
    "portable shared analysis initialization",
    "portable analysis-scope core",
    "portable selection core",
    "portable backend runtime",
    "portable plot geometry core",
    "portable dataset close and plot coordination core",
    "portable command parsing core",
    "portable correlation model core",
    "portable dimensionality model core",
    "portable scale analysis semantic core",
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
    "portable R provenance and publication core",
    "portable SVG writer core",
    "portable Snapshot Album core",
    "portable generalized linear model metadata core",
    "portable statistical distributions core",
    "portable dataset state",
    "portable global color state",
    "portable LinkEDA data-document persistence",
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
    "analysis_initialization_test.cpp",
    "analysis_scope_test.cpp",
    "selection_model_test.cpp",
    "backend_runtime_test.cpp",
    "plot_geometry_test.cpp",
    "plot_coordinator_test.cpp",
    "command_model_test.cpp",
    "correlation_model_test.cpp",
    "dimensionality_model_test.cpp",
    "scale_analysis_model_test.cpp",
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
    "provenance_model_test.cpp",
    "svg_writer_test.cpp",
    "snapshot_album_model_test.cpp",
    "glm_model_test.cpp",
    "statistics_model_test.cpp",
    "dataset_model_test.cpp",
    "color_state_test.cpp",
    "linkeda_document_test.cpp",
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
    "rlispstat-analysis-initialization-core",
    "rlispstat-analysis-scope-core",
    "rlispstat-selection-core",
    "rlispstat-backend-runtime-core",
    "rlispstat-plot-geometry-core",
    "rlispstat-plot-coordinator-core",
    "rlispstat-command-core",
    "rlispstat-correlation-core",
    "rlispstat-dimensionality-core",
    "rlispstat-scale-analysis-core",
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
    "rlispstat-provenance-core",
    "rlispstat-svg-writer-core",
    "rlispstat-snapshot-album-core",
    "rlispstat-glm-core",
    "rlispstat-statistics-core",
    "rlispstat-dataset-core",
    "rlispstat-color-state-core",
    "rlispstat-linkeda-document-core",
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
      run_args <- character()
      if (identical(case$test_cpp, "table1_model_test.cpp")) {
        root <- core_test_root()
        fixture_dir <- tempfile("linkeda-contingency-fixtures-")
        dir.create(fixture_dir)
        on.exit(unlink(fixture_dir, recursive = TRUE), add = TRUE)
        generator <- file.path(root, "tests", "scripts",
                               "generate-contingency-fixtures.R")
        rscript <- file.path(R.home("bin"), "Rscript")
        status <- system(paste(shQuote(rscript), shQuote(generator),
                               shQuote(fixture_dir)))
        expect_identical(status, 0L)
        run_args <- fixture_dir
      }
      run_core_cpp_test(case$test_cpp, case$prefix, run_args)
    })
  })
}

test_that("generalized auxiliary columns invalidate dependent model windows", {
  run_core_cpp_test("model_auxiliary_dependency_test.cpp",
                    "rlispstat-model-auxiliary-dependencies")
})

test_that("linear model windows keep independent state and R replies", {
  run_core_cpp_test("multiple_linear_models_test.cpp",
                    "rlispstat-multiple-linear-models")
})

test_that("scatterplot-matrix R verification recipe uses per-cell lm", {
  run_core_cpp_test("scatter_matrix_r_provenance_test.cpp",
                    "rlispstat-scatter-matrix-r-provenance")
})

test_that("Windows EMF renderer and clipboard integration are genuinely vectorial", {
  skip_if(.Platform$OS.type != "windows", "Windows GDI and clipboard are required.")
  compiler <- core_test_compiler()
  skip_if(!nzchar(compiler), "No C++17 compiler is configured for this R installation.")
  root <- core_test_root()
  library <- core_test_library(root, compiler)
  exe <- paste0(tempfile("rlispstat-windows-emf-"), ".exe")
  command <- paste(
    shQuote(compiler), "-std=c++17", "-I", shQuote(file.path(root, "src")),
    shQuote(file.path(root, "tests", "core", "windows_emf_export_test.cpp")),
    shQuote(file.path(root, "src", "platform", "windows", "windows_emf_export.cpp")),
    shQuote(library), "-lgdi32 -luser32 -o", shQuote(exe)
  )
  expect_identical(system(command), 0L)
  expect_identical(system(shQuote(exe)), 0L)
})
