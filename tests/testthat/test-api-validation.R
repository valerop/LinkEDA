test_that("scatter data preparation validates data frames and columns", {
  expect_error(LinkEDA:::`.rls_prepare_scatter_data`(1, "x", "y"), "data.frame")
  expect_error(LinkEDA:::`.rls_prepare_scatter_data`(mtcars, "nope", "mpg"), "not found")
  expect_error(LinkEDA:::`.rls_prepare_scatter_data`(data.frame(x = letters[1:3], y = 1:3), "x", "y"), "numeric")
})

test_that("scatter data preparation preserves original rows after missing values", {
  d <- data.frame(x = c(1, NA, 3, 4), y = c(1, 2, NA, 4))
  out <- LinkEDA:::`.rls_prepare_scatter_data`(d, "x", "y", group = "g")
  expect_equal(out$row, c(1L, 4L))
  expect_equal(out$x, c(1, 4))
  expect_equal(out$y, c(1, 4))
  expect_equal(out$group, "g")
})

test_that("group validation rejects invalid names", {
  expect_error(LinkEDA:::`.rls_validate_group`("", allow_null = FALSE), "non-empty")
  expect_error(LinkEDA:::`.rls_validate_group`(NA_character_, allow_null = FALSE), "non-empty")
  expect_error(LinkEDA:::`.rls_validate_group`("bad\ngroup", allow_null = FALSE), "control")
  expect_error(LinkEDA:::`.rls_validate_group`("bad|group", allow_null = FALSE), "control")
})

test_that("scatter accepts UI metadata but validates its shape", {
  expect_error(
    ls_scatter(mtcars, "wt", "mpg", labels = rownames(mtcars)[1:2]),
    "one value per row"
  )
  expect_error(
    ls_scatter(mtcars, "wt", "mpg", title = "bad|title"),
    "control"
  )
})

test_that("selection helpers report missing backend clearly", {
  expect_error(ls_selected("missing"), "No active LinkEDA backend|Could not connect")
  expect_error(ls_clear_selection("missing"), "No active LinkEDA backend|Could not connect")
  expect_silent(ls_close_all())
})

test_that("a stale native connection is reset before relaunching", {
  state <- LinkEDA:::.rls_state
  saved <- list(
    process_started = state$process_started,
    control_path = state$control_path,
    control_port = state$control_port,
    notify_path = state$notify_path,
    backend = state$backend,
    plots = as.list(state$plots, all.names = TRUE),
    groups = as.list(state$groups, all.names = TRUE)
  )
  restore_env <- function(env, values) {
    existing <- ls(env, all.names = TRUE)
    if (length(existing)) rm(list = existing, envir = env)
    if (length(values)) list2env(values, envir = env)
  }
  on.exit({
    state$process_started <- saved$process_started
    state$control_path <- saved$control_path
    state$control_port <- saved$control_port
    state$notify_path <- saved$notify_path
    state$backend <- saved$backend
    restore_env(state$plots, saved$plots)
    restore_env(state$groups, saved$groups)
  }, add = TRUE)

  state$process_started <- TRUE
  state$control_path <- tempfile("rlispstat-stale-")
  assign("stale_plot", list(), envir = state$plots)
  assign("stale_group", list(), envir = state$groups)

  withr::local_options(list(LinkEDA.launch = FALSE))
  expect_error(LinkEDA:::.rls_start_backend(), "launching is disabled")
  expect_false(state$process_started)
  expect_null(state$control_path)
  expect_false(exists("stale_plot", envir = state$plots, inherits = FALSE))
  expect_false(exists("stale_group", envir = state$groups, inherits = FALSE))
})

test_that("missing native control path reports a connection failure cleanly", {
  state <- LinkEDA:::.rls_state
  saved <- list(started = state$process_started, path = state$control_path)
  on.exit({
    state$process_started <- saved$started
    state$control_path <- saved$path
  }, add = TRUE)
  state$process_started <- TRUE
  state$control_path <- NULL

  expect_error(LinkEDA:::.rls_send("PING"),
               "Could not connect to the LinkEDA native backend", fixed = TRUE)
  expect_false(state$process_started)
  expect_null(state$control_path)
})

test_that("a welcome launch retries when Quit wins after the startup probe", {
  starts <- 0L
  welcomes <- 0L
  resets <- 0L
  testthat::local_mocked_bindings(
    .rls_start_backend = function() { starts <<- starts + 1L; invisible(TRUE) },
    .rls_send = function(lines, ...) {
      if (identical(lines[[1L]], "WELCOME_LAUNCH")) {
        welcomes <<- welcomes + 1L
        if (welcomes == 1L) stop("LinkEDA is closing", call. = FALSE)
      }
      "OK"
    },
    .rls_reset_backend_connection = function(...) {
      resets <<- resets + 1L
      invisible(TRUE)
    },
    .package = "LinkEDA"
  )
  expect_silent(LinkEDA())
  expect_identical(c(starts, welcomes, resets), c(2L, 2L, 1L))
})

test_that("a stale macOS FIFO fails promptly without a reader", {
  skip_if(.Platform$OS.type == "windows")
  path <- tempfile("linkeda-stale-fifo-")
  expect_identical(system2("mkfifo", path), 0L)
  on.exit(unlink(path), add = TRUE)
  state <- LinkEDA:::.rls_state
  old <- list(started = state$process_started, path = state$control_path)
  on.exit({
    state$process_started <- old$started
    state$control_path <- old$path
  }, add = TRUE)
  state$process_started <- TRUE
  state$control_path <- path
  elapsed <- system.time(expect_error(
    LinkEDA:::.rls_send(c("WELCOME_LAUNCH", "from_existing_r", "0.0.1", "none")),
    "no longer accepting commands"
  ))[["elapsed"]]
  expect_lt(elapsed, 3)
})

test_that("Windows startup prefers WinUI before consulting the console fallback", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  skip_if_not(.Platform$OS.type == "windows")
  state <- LinkEDA:::.rls_state
  saved <- list(
    process_started = state$process_started,
    control_port = state$control_port,
    backend_kind = state$backend_kind
  )
  on.exit({
    state$process_started <- saved$process_started
    state$control_port <- saved$control_port
    state$backend_kind <- saved$backend_kind
  }, add = TRUE)
  state$process_started <- FALSE
  state$control_port <- NULL
  state$backend_kind <- NULL

  calls <- character()
  testthat::local_mocked_bindings(
    .rls_send_winui = function(lines) {
      calls <<- c(calls, lines[[1L]])
      state$process_started <- TRUE
      state$control_port <- 39072L
      state$backend_kind <- "winui"
      "OK"
    },
    .rls_backend_path = function() stop("console fallback must not be queried"),
    .rls_start_backend_task_poll = function() invisible(TRUE),
    .package = "LinkEDA"
  )
  withr::local_options(list(LinkEDA.launch = TRUE, LinkEDA.use_winui = TRUE))

  expect_true(LinkEDA:::.rls_start_backend())
  expect_identical(state$backend_kind, "winui")
  expect_identical(calls, c("PING", "LIST_PLOTS"))
})

test_that("Windows startup rejects an obsolete WinUI protocol clearly", {
  skip_if_not(.Platform$OS.type == "windows")
  state <- LinkEDA:::.rls_state
  saved_started <- state$process_started
  on.exit({ state$process_started <- saved_started }, add = TRUE)
  state$process_started <- FALSE

  testthat::local_mocked_bindings(
    .rls_send_winui = function(lines) {
      if (identical(lines[[1L]], "PING")) return("OK")
      stop("unknown command", call. = FALSE)
    },
    .rls_backend_path = function() stop("console backend missing", call. = FALSE),
    .rls_reset_backend_connection = function(clear_views = TRUE) {
      state$process_started <- FALSE
      invisible(TRUE)
    },
    .package = "LinkEDA"
  )
  withr::local_options(list(LinkEDA.launch = TRUE, LinkEDA.use_winui = TRUE))

  expect_error(LinkEDA:::.rls_start_backend(), "WinUI reported: unknown command", fixed = TRUE)
  expect_false(state$process_started)
})

test_that("plot registry helpers track local object metadata", {
  plot_id <- "plot_test_registry"
  LinkEDA:::`.rls_register_plot`(plot_id, "g_registry", "x", "y", mtcars, seq_len(nrow(mtcars)))
  on.exit(LinkEDA:::`.rls_unregister_plot`(plot_id), add = TRUE)

  plots <- ls_plots()
  expect_true(plot_id %in% plots$id)
  expect_equal(LinkEDA:::.rls_resolve_group(NULL), "g_registry")
  expect_equal(LinkEDA:::.rls_plot_id(structure(list(id = plot_id), class = "rlispstat_plot")), plot_id)
})

test_that("numeric variable discovery uses plot data and excludes non-numeric columns", {
  plot_id <- "plot_test_vars"
  d <- data.frame(x = c(1, 2, NA), y = c(4, NA, 6), z = letters[1:3])
  LinkEDA:::`.rls_register_plot`(plot_id, "g_vars", "x", "y", d, c(1L))
  on.exit(LinkEDA:::`.rls_unregister_plot`(plot_id), add = TRUE)

  expect_equal(ls_variables(plot_id), c("x", "y", "z"))
  expect_equal(ls_numeric_variables(plot_id), c("x", "y"))
  payload <- LinkEDA:::.rls_variable_payload(d)
  expect_true("VARS" %in% payload)
  expect_true("NA" %in% payload)
})

test_that("DataDesk helpers classify variable types", {
  d <- data.frame(
    x = 1:3,
    f = factor(c("a", "b", "a")),
    o = ordered(c("low", "high", "low"), levels = c("low", "high")),
    c = letters[1:3],
    l = c(TRUE, FALSE, TRUE),
    date = as.Date("2026-01-01") + 0:2
  )
  expect_equal(vapply(d, LinkEDA:::.rls_variable_type, character(1L)),
               c(x = "numeric", f = "factor", o = "ordered", c = "character",
                 l = "logical", date = "datetime"))
  payload <- LinkEDA:::.rls_variable_payload(d)
  expect_true("VARMETA" %in% payload)
  expect_true("factor" %in% payload)
})

test_that("file-import inference recognizes coded categories conservatively", {
  d <- data.frame(
    id = seq_len(32),
    cyl = rep(c(4, 6, 8), length.out = 32),
    am = rep(c(0, 1), 16),
    score = seq(10.25, 18, length.out = 32),
    rounded_measure = rep(1:12, length.out = 32),
    planet = rep(c("Aurelia", "Borealis", "Cygnus"), length.out = 32),
    happy = rep(c("no", "yes"), 16),
    notes = sprintf("case-specific note %02d", seq_len(32)),
    stringsAsFactors = FALSE
  )
  metadata <- LinkEDA:::.rls_variable_metadata(d, infer_imported_types = TRUE)
  types <- setNames(metadata$current_analysis_type, metadata$variable_name)
  expect_equal(types[c("id", "cyl", "am", "score", "rounded_measure",
                       "planet", "happy", "notes")],
               c(id = "numeric", cyl = "numeric", am = "numeric",
                 score = "numeric", rounded_measure = "numeric",
                 planet = "factor", happy = "factor", notes = "character"))

  expect_equal(LinkEDA:::.rls_metadata_levels(metadata, "planet"), c("Aurelia", "Borealis", "Cygnus"))
  payload <- LinkEDA:::.rls_dataframe_payload(d, metadata = metadata)
  expect_true("DATLEVELS" %in% payload)

  labelled <- structure(c(1, 2, 1, 2), labels = c(Control = 1, Treatment = 2))
  expect_equal(LinkEDA:::.rls_imported_variable_type(labelled, "arm"), "factor")

  imported_age <- rep(as.character(12:19), length.out = 270)
  imported_age[c(11, 71, 141, 211)] <- c("11-vuotta", "11.V", "14 vuotta", "18 vuotias")
  imported_age[[31]] <- "17,5"
  imported_age[[61]] <- "15,"
  age_data <- data.frame(age = imported_age, stringsAsFactors = FALSE)
  age_metadata <- LinkEDA:::.rls_variable_metadata(age_data, infer_imported_types = TRUE)
  expect_equal(age_metadata$current_analysis_type, "character")
  warnings <- LinkEDA:::.rls_import_numeric_type_warnings(age_data, age_metadata)
  expect_length(warnings, 1L)
  expect_match(warnings, "`age` looks numeric, but 4 of 270 non-missing values are not numeric", fixed = TRUE)
  expect_match(warnings, "imported as Text", fixed = TRUE)
})

test_that("native cell edits synchronize the registered R dataset", {
  group <- ls_register_dataset(
    "native_sync_data", data.frame(score = c(1, 2), category = c(4, 6))
  )
  on.exit(ls_unregister_dataset(group), add = TRUE)
  exchange <- tempfile("rlispstat-sync-")
  dir.create(exchange)
  payload <- file.path(exchange, "dataset.txt")
  writeLines(c(
    "DATASET", group, "2", "2",
    "score", "numeric", "9", "2",
    "category", "factor", "4", "8"
  ), payload, useBytes = TRUE)
  testthat::local_mocked_bindings(
    .rls_send = function(lines) "OK",
    .package = "LinkEDA"
  )

  expect_true(LinkEDA:::.rls_handle_r_dataset_sync_needed(c(
    "R_DATASET_SYNC_NEEDED", "request-1", group, payload
  )))
  record <- LinkEDA:::.rls_dataset_record(group)
  expect_equal(record$data$score, c(9, 2))
  expect_equal(as.character(record$data$category), c("4", "8"))
  expect_equal(record$variable_metadata$current_analysis_type,
               c("numeric", "factor"))
  expect_true(record$modified)
  expect_false(dir.exists(exchange))
})

test_that("native dataset synchronization restores a registry lost after R restart", {
  group <- "native_recovery_data"
  if (group %in% ls_datasets()$name) ls_unregister_dataset(group)
  on.exit(ls_unregister_dataset(group), add = TRUE)
  exchange <- tempfile("rlispstat-sync-")
  dir.create(exchange)
  payload <- file.path(exchange, "dataset.txt")
  writeLines(c(
    "DATASET", group, "3", "2",
    "score", "numeric", "1", "2", "3",
    "arm", "factor", "control", "treatment", "control"
  ), payload, useBytes = TRUE)
  testthat::local_mocked_bindings(
    .rls_send = function(lines) "OK",
    .package = "LinkEDA"
  )

  expect_false(group %in% ls_datasets()$name)
  expect_true(LinkEDA:::.rls_handle_r_dataset_sync_needed(c(
    "R_DATASET_SYNC_NEEDED", "request-recovery", group, payload
  )))
  record <- LinkEDA:::.rls_dataset_record(group)
  expect_equal(record$data$score, c(1, 2, 3))
  expect_equal(as.character(record$data$arm),
               c("control", "treatment", "control"))
  expect_equal(record$variable_metadata$current_analysis_type,
               c("numeric", "factor"))
  expect_false(dir.exists(exchange))
})

test_that("native cell synchronization carries the edited data version to R", {
  group <- "native_cell_version_sync"
  if (group %in% ls_datasets()$name) ls_unregister_dataset(group)
  ls_register_dataset(group, data.frame(score = c(1, 2)))
  on.exit(ls_unregister_dataset(group), add = TRUE)
  exchange <- tempfile("rlispstat-sync-")
  dir.create(exchange)
  payload <- file.path(exchange, "dataset.txt")
  writeLines(c("DATASET", group, "2", "1", "score", "numeric", "1", "3",
               "DATA_VERSION_V1", "2"), payload, useBytes = TRUE)
  testthat::local_mocked_bindings(
    .rls_send = function(lines) "OK",
    .package = "LinkEDA"
  )
  expect_true(LinkEDA:::.rls_handle_r_dataset_sync_needed(c(
    "R_DATASET_SYNC_NEEDED", "request-edit", group, payload
  )))
  record <- LinkEDA:::.rls_dataset_record(group)
  expect_equal(record$data$score, c(1, 3))
  expect_identical(record$data_version, 2L)
})

test_that("a native derived category is available to R analyses after synchronization", {
  group <- "native_derived_category_sync"
  if (group %in% ls_datasets()$name) ls_unregister_dataset(group)
  ls_register_dataset(group, data.frame(score = c(1, 2, 3)))
  on.exit(ls_unregister_dataset(group), add = TRUE)
  exchange <- tempfile("rlispstat-sync-")
  dir.create(exchange)
  payload <- file.path(exchange, "dataset.txt")
  writeLines(c("DATASET", group, "3", "2", "DATACELLS_PERCENT_V1",
               "score", "numeric", "1", "2", "3",
               "point_color_from_plot", "factor", "green", "yellow", "green",
               "DATA_VERSION_V1", "2"), payload, useBytes = TRUE)
  testthat::local_mocked_bindings(.rls_send = function(lines) "OK", .package = "LinkEDA")
  expect_true(LinkEDA:::.rls_handle_r_dataset_sync_needed(c(
    "R_DATASET_SYNC_NEEDED", "request-derived", group, payload
  )))
  record <- LinkEDA:::.rls_dataset_record(group)
  expect_identical(record$data_version, 2L)
  expect_identical(as.character(record$data$point_color_from_plot),
                   c("green", "yellow", "green"))
  expect_true("point_color_from_plot" %in% names(record$data))
})

test_that("an older native sync payload cannot roll back the R dataset", {
  group <- "native_cell_stale_sync"
  if (group %in% ls_datasets()$name) ls_unregister_dataset(group)
  ls_register_dataset(group, data.frame(score = c(1, 2)))
  on.exit(ls_unregister_dataset(group), add = TRUE)
  record <- LinkEDA:::.rls_dataset_record(group)
  record$data$score[[2L]] <- 4
  record <- LinkEDA:::.rls_advance_data_version(
    record, "Test edit", origin = "unavailable", columns = "score")
  LinkEDA:::.rls_set_dataset_record(record)
  exchange <- tempfile("rlispstat-sync-")
  dir.create(exchange)
  payload <- file.path(exchange, "dataset.txt")
  writeLines(c("DATASET", group, "2", "1", "score", "numeric", "1", "3",
               "DATA_VERSION_V1", "1"), payload, useBytes = TRUE)
  testthat::local_mocked_bindings(.rls_send = function(lines) "OK", .package = "LinkEDA")
  expect_false(LinkEDA:::.rls_handle_r_dataset_sync_needed(c(
    "R_DATASET_SYNC_NEEDED", "request-stale", group, payload
  )))
  current <- LinkEDA:::.rls_dataset_record(group)
  expect_equal(current$data$score, c(1, 4))
  expect_identical(current$data_version, 2L)
})

test_that("dataset registration accepts the data-first comparison example", {
  ls_register_dataset(mtcars, "cars")
  datasets <- ls_datasets()
  expect_true("cars" %in% datasets$name)
  expect_true("ls_new_regression_comparison" %in% getNamespaceExports("LinkEDA"))

  cmp <- ls_new_regression_comparison(
    data = "cars",
    response = "mpg",
    models = list(
      Base = c("wt"),
      Extended = c("wt", "hp"),
      Full = c("wt", "hp", "am")
    ),
    native = FALSE
  )
  state <- ls_regression_comparison_state(cmp)

  expect_s3_class(cmp, "rlispstat_regression_comparison")
  expect_true(nzchar(cmp$id))
  expect_equal(state$group, "cars")
  expect_equal(state$response, "mpg")
  expect_length(state$models, 3L)
  expect_equal(vapply(state$models, `[[`, character(1), "label"), c("Base", "Extended", "Full"))
})

test_that("model handle validation is local and explicit", {
  m <- structure(list(id = "model:g", group = "g"), class = "rlispstat_model")
  expect_equal(LinkEDA:::.rls_model_group(m), "g")
  expect_error(LinkEDA:::.rls_model_group(NA_character_), "non-empty")
  expect_error(ls_model_scope(m, "visible"), "should be one of")
  expect_true("ls_model_open_residuals_fitted" %in% getNamespaceExports("LinkEDA"))
  expect_true("ls_model_open_observed_fitted" %in% getNamespaceExports("LinkEDA"))
  expect_true("ls_model_remove_term" %in% getNamespaceExports("LinkEDA"))
  expect_true("ls_workbench" %in% getNamespaceExports("LinkEDA"))
  expect_error(ls_workbench(data.frame(x = letters[1:3]), group = "g"), "numeric variable")
  expect_error(ls_workbench(mtcars, group = "g", y = "am", labels = letters[1:2]), "one label per row")
})

test_that("protocol names reject separators used by backend commands", {
  expect_error(LinkEDA:::.rls_validate_protocol_name("bad|name", "var"), "control")
  expect_error(LinkEDA:::.rls_validate_protocol_name("bad\tname", "var"), "control")
})

test_that("overlay API validates data source arguments", {
  expect_error(ls_lm_line("plot_missing", data = "bad"), "should be one of")
  expect_error(ls_clear_overlays("plot_missing", data = "bad"), "should be one of")
  expect_error(ls_lm_line("plot_missing", data = "color"), "No active LinkEDA backend|Could not connect")
})

test_that("histogram preparation preserves row identity and bins", {
  d <- data.frame(x = c(1, 2, NA, 4, 5, Inf), z = letters[1:6])
  prepared <- LinkEDA:::.rls_prepare_histogram_data(d, "x", group = "hist", bins = 2)
  expect_equal(prepared$row, c(1L, 2L, 4L, 5L))
  expect_equal(length(prepared$breaks), 3L)
  expect_true(all(prepared$bin %in% 1:2))
  expect_equal(prepared$group, "hist")
  expect_error(LinkEDA:::.rls_prepare_histogram_data(d, "z"), "numeric")
  expect_error(LinkEDA:::.rls_histogram_breaks_for_values(1:3, breaks = c(1, 1)), "distinct")
})

test_that("histogram density API validates modes and numeric options", {
  expect_equal(LinkEDA:::.rls_validate_histogram_density_mode("selected_and_colors"), "selected_and_colors")
  expect_error(LinkEDA:::.rls_validate_histogram_density_mode("by_cluster"), "should be one of")
  expect_error(ls_histogram_show_density("plot_missing", NA), "TRUE or FALSE")
  expect_error(ls_histogram_density_bw("plot_missing", -1), "non-negative")
  expect_error(ls_histogram_density_adjust("plot_missing", 0), "positive")
})

test_that("GLM API validates plot handles through the backend boundary", {
  expect_error(ls_glm(list(id = "bad")), "LinkEDA plot object|plot id|No active LinkEDA backend")
})

test_that("R-side GLM model fits lm-style multiple regression", {
  d <- mtcars
  d$hp[1] <- NA
  ls_register_dataset("glmtest", d)
  m <- ls_new_glm("glmtest")
  expect_s3_class(m, "rlispstat_glm")
  ls_glm_set_dependent(m, "mpg")
  expect_equal(ls_glm_get_dependent(m), "mpg")
  ls_glm_add_predictor(m, "wt")
  ls_glm_add_predictor(m, "hp")
  ls_glm_add_predictor(m, "hp")
  expect_equal(ls_glm_predictors(m), c("wt", "hp"))
  expect_error(ls_glm_add_predictor(m, "mpg"), "dependent variable")
  expect_equal(deparse(ls_glm_formula(m)), "mpg ~ wt + hp")

  m <- ls_glm_fit(m)
  coefs <- ls_glm_coefficients(m)
  fit_summary <- ls_glm_fit_summary(m)
  partial <- ls_glm_partial_r(m)
  delta <- ls_glm_delta_r2(m)
  expect_true(all(c("term", "estimate", "std_error", "t_value", "p_value", "partial_r", "delta_r2") %in% names(coefs)))
  expect_true(all(c("(Intercept)", "wt", "hp") %in% coefs$term))
  expect_equal(fit_summary$n_excluded, 1L)
  expect_true(is.finite(fit_summary$r_squared))
  expect_true(is.finite(fit_summary$adj_r_squared))
  expect_true(is.finite(fit_summary$global_f))
  expect_true(is.finite(fit_summary$global_p))
  expect_true(is.na(partial[["(Intercept)"]]))
  expect_true(is.finite(partial[["wt"]]))
  expect_true(is.finite(delta[["wt"]]))
  expect_equal(ls_glm_rows_excluded(m), 1L)
  expect_false(1L %in% ls_glm_rows_used(m))

  ls_glm_remove_predictor(m, "hp")
  expect_equal(ls_glm_predictors(m), "wt")

  ls_glm_add_interaction(m, "wt", "qsec")
  ls_glm_add_polynomial(m, "wt", 2)
  expect_true(all(c("wt", "qsec", "wt:qsec", "I(wt^2)") %in% ls_glm_terms(m)))
  expect_match(deparse(ls_glm_formula(m)), "wt:qsec", fixed = TRUE)

  ls_glm_remove_predictor(m, "qsec")
  expect_true("wt" %in% ls_glm_terms(m))
  expect_false("qsec" %in% ls_glm_terms(m))
  expect_false("wt:qsec" %in% ls_glm_terms(m))
})

test_that("model term removal drops dependent higher-order interactions", {
  terms <- c("wt", "qsec", "hp", "wt:qsec", "hp:qsec", "wt:hp:qsec")
  expect_equal(
    LinkEDA:::.rls_model_remove_hierarchical_term(terms, "wt"),
    c("qsec", "hp", "hp:qsec")
  )
  expect_equal(
    LinkEDA:::.rls_model_remove_hierarchical_term(terms, "wt:qsec"),
    c("wt", "qsec", "hp", "hp:qsec")
  )
})

test_that("GLM country interaction keeps fitted treatment identities", {
  d <- expand.grid(
    baseline = 0:3,
    study_group = c("control", "treatment"),
    country = c("Greece", "Finland"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  # Preserve the reported failure condition: Greece occurs first in the data,
  # while factor() and the fitted treatment contrast use Finland as reference.
  d <- d[order(match(d$country, c("Greece", "Finland"))), , drop = FALSE]
  d$response <- 2 + 0.3547 * d$baseline +
    ifelse(d$country == "Greece", -0.3930 - 0.2011 * d$baseline, 0) +
    ifelse(d$study_group == "treatment", 0.25, 0)
  typed <- LinkEDA:::.rls_model_data_for_term_types(
    d, list(country = "factor", study_group = "factor"), response = "response"
  )
  fit <- stats::lm(
    response ~ baseline + study_group + country + baseline:country,
    data = typed
  )
  coding <- LinkEDA:::.rls_model_factor_codings(fit)$country
  expect_equal(coding$levels, c("Finland", "Greece"))
  expect_equal(coding$reference, "Finland")
  expect_equal(unname(coding$coding[, 1L]), c(0, 1))

  coefficient_matrix <- summary(fit)$coefficients
  coefficients <- data.frame(
    term = rownames(coefficient_matrix),
    estimate = coefficient_matrix[, "Estimate"],
    std_error = coefficient_matrix[, "Std. Error"],
    statistic = coefficient_matrix[, "t value"],
    p_value = coefficient_matrix[, "Pr(>|t|)"],
    partial_r = LinkEDA:::.rls_model_partial_r(
      coefficient_matrix[, "t value"], stats::df.residual(fit)
    ),
    delta_r2 = NA_real_,
    check.names = FALSE
  )
  rows <- LinkEDA:::.rls_model_coefficient_display_rows(fit, coefficients, typed)
  reference <- rows[rows$source_term == "country" & rows$row_type == "reference", ]
  expect_equal(reference$level, "Finland")
  interaction <- rows[
    rows$source_term == "baseline:country" & rows$row_type == "coefficient",
  ]
  expect_equal(interaction$level, "Greece")
  expect_equal(interaction$reference_level, "Finland")
  expect_match(interaction$display_label, "baseline × country = Greece \\(vs Finland\\)")
  expect_equal(unname(stats::coef(fit)[["baseline"]]), 0.3547, tolerance = 1e-9)
  expect_equal(
    unname(stats::coef(fit)[["baseline"]] + stats::coef(fit)[["baseline:countryGreece"]]),
    0.1536,
    tolerance = 1e-9
  )
})

test_that("native GLM interaction report computes simple slopes", {
  backend_ready <- tryCatch({
    if (.Platform$OS.type == "windows") {
      isTRUE(LinkEDA:::.rls_start_backend())
    } else {
      backend <- LinkEDA:::.rls_backend_path()
      nzchar(backend) && file.exists(backend) && isTRUE(LinkEDA:::.rls_start_backend())
    }
  }, error = function(e) FALSE)
  skip_if_not(backend_ready)
  d <- data.frame(
    y = c(12, 14, 16, 18, 25, 30, 35, 40),
    x = rep(1:4, 2),
    g = factor(rep(c("A", "B"), each = 4))
  )
  group <- ls_register_dataset("glm_native_interaction_report", d)
  on.exit(ls_close_all(), add = TRUE)
  record <- LinkEDA:::.rls_dataset_record(group)
  LinkEDA:::.rls_send(c(
    "REGISTER_DATASET_SILENT",
    record$group,
    LinkEDA:::.rls_variable_payload(record$data, record$variable_metadata),
    LinkEDA:::.rls_dataframe_payload(record$data, record$variable_metadata, dataset_record = record)
  ))
  expect_equal(LinkEDA:::.rls_send(c("MODEL_SET_Y", record$group, "y")), "OK")
  expect_equal(LinkEDA:::.rls_send(c("MODEL_ADD_TERM", record$group, "x")), "OK")
  expect_equal(LinkEDA:::.rls_send(c("MODEL_ADD_TERM", record$group, "g")), "OK")
  expect_equal(LinkEDA:::.rls_send(c("MODEL_ADD_TERM", record$group, "x:g")), "OK")

  expect_error(
    LinkEDA:::.rls_send(c("MODEL_INTERACTION_REPORT", record$group, "x:g")),
    "requires a completed R fit", fixed = TRUE)
  model <- ls_new_glm(record$group)
  ls_glm_set_dependent(model, "y")
  ls_glm_add_predictor(model, "x")
  ls_glm_add_predictor(model, "g")
  ls_glm_add_predictor(model, "x:g")
  model <- ls_glm_fit(model)
  expect_true(LinkEDA:::.rls_glm_sync_native_update(
    LinkEDA:::.rls_glm_model_record(model)))
  reply <- LinkEDA:::.rls_send(c("MODEL_INTERACTION_REPORT", record$group, "x:g"))
  report <- utils::URLdecode(sub("^OK\t", "", reply))
  expect_match(report, "Type: continuous by categorical", fixed = TRUE)
  expect_match(report, "Simple slopes", fixed = TRUE)
  expect_match(report, "Pairwise slope comparisons", fixed = TRUE)
  expect_match(report, "Predicted means", fixed = TRUE)
  expect_match(report, "Slope of x when g = A\t2.0000", fixed = TRUE)
  expect_match(report, "Slope of x when g = B\t5.0000", fixed = TRUE)
  expect_match(report, "A - B slope difference\t-3.0000", fixed = TRUE)

  plot_reply <- LinkEDA:::.rls_send(c("MODEL_OPEN_INTERACTION_PLOT", record$group, "x:g"))
  expect_match(plot_reply, "^OK\tpooled_interaction_glm_native_interaction_report_")
})

test_that("GLM diagnostics are versioned live model data keyed by original rows", {
  d <- mtcars
  d$hp[1] <- NA
  ls_register_dataset("glmdiag", d)
  m <- ls_new_glm("glmdiag")
  ls_glm_set_dependent(m, "mpg")
  ls_glm_add_predictor(m, "wt")
  m <- ls_glm_fit(m)

  record1 <- LinkEDA:::.rls_glm_model_record(m)
  diag1 <- ls_glm_diagnostics(m)
  p <- ls_glm_open_diagnostic(m, "residuals_vs_fitted", native = FALSE)
  state1 <- ls_glm_diagnostic_state(p)

  expect_true(all(c("row_id", "fitted", "observed", "residual",
                    "standardized_residual", "leverage", "cooks_distance") %in% names(diag1)))
  expect_equal(diag1$row_id, ls_glm_rows_used(m))
  expect_true(1L %in% diag1$row_id)
  expect_equal(ls_glm_rows_excluded(m), integer())
  expect_equal(state1$displayed_fit_version, record1$fit_version)
  expect_equal(state1$displayed_diagnostics_version, record1$diagnostics_version)
  expect_equal(state1$model_id, m$id)
  expect_null(state1$native_plot_id)

  old_fitted <- diag1$fitted
  old_rows <- diag1$row_id
  fit_version_before_color <- record1$fit_version
  LinkEDA:::.rls_glm_notify_diagnostic_plots(record1)
  expect_equal(LinkEDA:::.rls_glm_model_record(m)$fit_version, fit_version_before_color)

  ls_glm_add_predictor(m, "hp")
  changed <- LinkEDA:::.rls_glm_model_record(m)
  expect_true(changed$model_version > record1$model_version)
  expect_true(changed$is_stale)

  m <- ls_glm_fit(m)
  record2 <- LinkEDA:::.rls_glm_model_record(m)
  diag2 <- ls_glm_diagnostics(m)
  state2 <- ls_glm_diagnostic_state(p)

  expect_true(record2$fit_version > record1$fit_version)
  expect_true(record2$diagnostics_version > record1$diagnostics_version)
  expect_false(1L %in% diag2$row_id)
  expect_equal(ls_glm_rows_excluded(m), 1L)
  expect_false(identical(diag2$row_id, old_rows))
  expect_false(isTRUE(all.equal(diag2$fitted, old_fitted[-1])))
  expect_equal(state2$displayed_fit_version, record2$fit_version)
  expect_equal(state2$displayed_diagnostics_version, record2$diagnostics_version)
  expect_equal(state2$data$row_id, diag2$row_id)

  ls_glm_remove_predictor(m, "hp")
  removed <- LinkEDA:::.rls_glm_model_record(m)
  expect_true(removed$model_version > record2$model_version)
  expect_true(removed$is_stale)

  m <- ls_glm_fit(m)
  record3 <- LinkEDA:::.rls_glm_model_record(m)
  diag3 <- ls_glm_diagnostics(m)
  state3 <- ls_glm_diagnostic_state(p)

  expect_true(record3$fit_version > record2$fit_version)
  expect_true(record3$diagnostics_version > record2$diagnostics_version)
  expect_equal(ls_glm_predictors(m), "wt")
  expect_equal(state3$model_id, m$id)
  expect_equal(state3$displayed_fit_version, record3$fit_version)
  expect_equal(state3$displayed_diagnostics_version, record3$diagnostics_version)
  expect_equal(state3$data$row_id, diag3$row_id)
  expect_false(isTRUE(all.equal(state3$data$fitted, state2$data$fitted)))
})

test_that("linear model factor predictors render parent and level rows", {
  d <- mtcars
  ls_register_dataset("glmfactorrows", d)
  m <- ls_new_glm("glmfactorrows")
  ls_glm_set_dependent(m, "mpg")
  ls_glm_add_predictor(m, "wt")
  ls_glm_add_predictor(m, "factor(cyl)")
  m <- ls_glm_fit(m)
  rows <- ls_glm_coefficient_rows(m)

  expect_true("cyl" %in% rows$term)
  expect_equal(rows$row_type[rows$term == "cyl"], "factor_parent")
  expect_true(all(c("cyl=4", "cyl=6", "cyl=8") %in% rows$term))
  expect_equal(rows$row_type[rows$term == "cyl=4"], "reference")
  expect_true(is.na(rows$estimate[rows$term == "cyl=4"]))
  raw <- ls_glm_coefficients(m)
  expect_equal(
    rows$estimate[rows$term == "cyl=6"],
    raw$estimate[raw$term == "factor(cyl)6"],
    tolerance = 1e-8
  )
  table <- ls_glm_regression_table(m)
  text <- LinkEDA:::.rls_render_table_text(table)
  expect_match(text, "\ncyl\\s+\u2014")
  expect_match(text, "\n  4\\s+\u2014")
  expect_match(text, "\n  6\\s+-")
  expect_false(grepl("^factor\\(cyl\\)6", text))
})

test_that("linear model interactions are semantically typed and grouped", {
  d <- mtcars
  d$gear <- factor(d$gear)
  d$am <- factor(d$am)
  ls_register_dataset("glminteractionrows", d)
  m <- ls_new_glm("glminteractionrows")
  ls_glm_set_dependent(m, "mpg")
  ls_glm_add_predictor(m, "gear")
  ls_glm_add_predictor(m, "am")
  ls_glm_add_predictor(m, "gear:am")
  m <- ls_glm_fit(m)
  rows <- ls_glm_coefficient_rows(m)

  parent <- rows[rows$term == "gear:am", , drop = FALSE]
  expect_equal(parent$row_type, "term_parent")
  expect_equal(parent$term_type, "factor_factor_interaction")
  children <- rows[rows$source_term == "gear:am" & rows$row_type == "coefficient", , drop = FALSE]
  expect_gt(nrow(children), 0)
  expect_true(all(children$term_type == "factor_factor_interaction"))
  expect_true(all(children$term %in% colnames(stats::model.matrix(LinkEDA:::.rls_glm_model_record(m)$fit))))
})

test_that("changing the linear response removes it from predictors and interactions", {
  cities <- data.frame(
    monthly_rent_eur = c(900, 1200, 1600, 2100, 2500, 3100),
    apartment_price_eur_m2 = c(2400, 3300, 5200, 7200, 8600, 11000),
    region = factor(c("Europe", "Europe", "Europe", "Asia", "Asia", "America"))
  )
  ls_register_dataset("glm_response_cascade", cities)
  m <- ls_new_glm("glm_response_cascade")
  ls_glm_set_dependent(m, "monthly_rent_eur")
  ls_glm_add_predictor(m, "apartment_price_eur_m2")
  ls_glm_add_predictor(m, "region")
  ls_glm_add_predictor(m, "apartment_price_eur_m2:region")

  record <- LinkEDA:::.rls_glm_model_record(m)
  record$term_types <- list(apartment_price_eur_m2 = "numeric", region = "factor")
  record$centered_predictors <- "apartment_price_eur_m2"
  record$factor_reference_levels <- list(region = "Europe")
  LinkEDA:::.rls_assign_glm_model(record)

  ls_glm_set_dependent(m, "apartment_price_eur_m2")
  changed <- LinkEDA:::.rls_glm_model_record(m)
  expect_equal(changed$dependent, "apartment_price_eur_m2")
  expect_equal(changed$predictors, "region")
  expect_false(any(vapply(changed$predictors, function(term) {
    "apartment_price_eur_m2" %in% LinkEDA:::.rls_model_interaction_parts(term)
  }, logical(1L))))
  expect_false("apartment_price_eur_m2" %in% names(changed$term_types))
  expect_false("apartment_price_eur_m2" %in% changed$centered_predictors)
  expect_equal(changed$factor_reference_levels$region, "Europe")
})

test_that("SpreadPlot manager routes GLM and row-state messages", {
  d <- mtcars
  d$hp[1] <- NA
  ls_register_dataset("glmmanager", d)
  m <- ls_new_glm("glmmanager")
  ls_glm_set_dependent(m, "mpg")
  ls_glm_add_predictor(m, "wt")
  m <- ls_glm_fit(m)
  p <- ls_glm_open_diagnostic(m, "residuals_fitted")
  LinkEDA:::.rls_spreadplot_clear_messages()

  record1 <- LinkEDA:::.rls_glm_model_record(m)
  diag1 <- ls_glm_diagnostics(m)
  ls_glm_add_predictor(m, "hp")
  m <- ls_glm_fit(m)
  record2 <- LinkEDA:::.rls_glm_model_record(m)
  diag2 <- ls_glm_diagnostics(m)
  state2 <- ls_glm_diagnostic_state(p)
  messages <- LinkEDA:::.rls_spreadplot_messages()
  types <- vapply(messages, `[[`, character(1), "type")

  expect_true("GLM_PREDICTOR_ADDED" %in% types)
  expect_true("GLM_REFIT_COMPLETED" %in% types)
  expect_true("GLM_DIAGNOSTICS_UPDATED" %in% types)
  expect_true(record2$fit_version > record1$fit_version)
  expect_equal(state2$displayed_fit_version, record2$fit_version)
  expect_false(isTRUE(all.equal(diag2$fitted, diag1$fitted[-1])))
  expect_true(all(diag2$row_id %in% seq_len(nrow(d))))

  fit_version_before_color <- record2$fit_version
  LinkEDA:::.rls_spreadplot_emit("ROW_COLORS_CHANGED", "glmmanager", sender_id = "mock", model_id = m$id, rows = c(2L, 3L))
  expect_equal(LinkEDA:::.rls_glm_model_record(m)$fit_version, fit_version_before_color)
  listener <- LinkEDA:::.rls_spreadplot_listener(p$id)
  received_types <- vapply(listener$received, `[[`, character(1), "type")
  expect_true("ROW_COLORS_CHANGED" %in% received_types)

  LinkEDA:::.rls_spreadplot_emit("ROW_SELECTION_CHANGED", "glmmanager", sender_id = "mock", model_id = m$id, rows = c(4L, 5L))
  expect_equal(LinkEDA:::.rls_glm_model_record(m)$fit_version, fit_version_before_color)
  listener <- LinkEDA:::.rls_spreadplot_listener(p$id)
  received_types <- vapply(listener$received, `[[`, character(1), "type")
  expect_true("ROW_SELECTION_CHANGED" %in% received_types)
})

test_that("palette API validates color names and row indices", {
  expect_error(LinkEDA:::.rls_validate_palette_color("skyblue"), "must be one of")
  expect_error(LinkEDA:::.rls_validate_palette_color("black"), "must be one of")
  expect_error(LinkEDA:::.rls_validate_palette_color("amber"), "must be one of")
  expect_equal(LinkEDA:::.rls_validate_palette_color("pink"), "pink")
  expect_equal(LinkEDA:::.rls_validate_palette_color("vermillion"), "vermillion")
  expect_equal(
    LinkEDA:::.rls_palette_colors,
    c(
      orange = "#E69F00",
      blue = "#0072B2",
      green = "#009E73",
      vermillion = "#D55E00",
      purple = "#CC79A7",
      brown = "#A6761D",
      pink = "#D81B60",
      yellow = "#F0E442"
    )
  )
  expect_false(exists(".rls_selected_palette_colors", envir = asNamespace("LinkEDA"), inherits = FALSE))
  expect_equal(
    LinkEDA:::.rls_data_sheet_palette_colors,
    c(
      orange = "#FBE6B3",
      blue = "#CFE8F6",
      green = "#CCEDE3",
      vermillion = "#F3D4C2",
      purple = "#F1DDEA",
      brown = "#D7C3A3",
      pink = "#F8BBD0",
      yellow = "#FCF7BD"
    )
  )
  expect_equal(
    LinkEDA:::.rls_data_sheet_selected_palette_colors,
    c(
      orange = "#E69F00",
      blue = "#0072B2",
      green = "#009E73",
      vermillion = "#D55E00",
      purple = "#CC79A7",
      brown = "#A6761D",
      pink = "#D81B60",
      yellow = "#F0E442"
    )
  )
  expect_error(ls_set_point_color("missing", c(1, 2), "skyblue"), "must be one of|metadata")
})

test_that("palette names avoid overlapping/conflicting color labels", {
  palette_names <- names(LinkEDA:::.rls_palette_colors)
  expect_length(palette_names, 8L)
  expect_equal(anyDuplicated(palette_names), 0L)
  expect_false(any(palette_names %in% c("black", "red", "cyan", "gray", "grey", "darkcyan", "dodgerblue", "skyblue", "amber")))
  expect_equal(sum(grepl("blue", palette_names, ignore.case = TRUE)), 1L)
  expect_false(any(grepl("gray|grey", palette_names, ignore.case = TRUE)))
})

test_that("data sheet display colors are derived and readable", {
  palette_names <- names(LinkEDA:::.rls_palette_colors)
  expect_equal(names(LinkEDA:::.rls_data_sheet_palette_colors), palette_names)
  expect_equal(names(LinkEDA:::.rls_data_sheet_selected_palette_colors), palette_names)
  expect_equal(names(LinkEDA:::.rls_data_sheet_selected_text_colors), palette_names)

  table <- LinkEDA:::.rls_palette_table(print = FALSE)
  expect_equal(nrow(table), 8L)
  expect_equal(table$color, palette_names)
  expect_equal(table$base_hex, unname(LinkEDA:::.rls_palette_colors))
  expect_equal(table$light_row_background_hex, unname(LinkEDA:::.rls_data_sheet_palette_colors))
  expect_equal(table$selected_row_background_hex, unname(LinkEDA:::.rls_data_sheet_selected_palette_colors))

  expect_equal(LinkEDA:::.rls_data_sheet_display_color(NULL, selected = FALSE)$background, "#FFFFFF")
  expect_equal(LinkEDA:::.rls_data_sheet_display_color("", selected = FALSE)$background, "#FFFFFF")
  expect_equal(LinkEDA:::.rls_get_display_color("black", selected = FALSE, has_explicit_color = FALSE)$background, "#FFFFFF")
  expect_equal(LinkEDA:::.rls_get_display_color("black", selected = TRUE, has_explicit_color = FALSE)$background, "#E9E9E9")
  expect_false(grepl("B2|CFE8F6|0072B2", LinkEDA:::.rls_get_display_color("black", selected = FALSE, has_explicit_color = FALSE)$background))
  expect_error(LinkEDA:::.rls_data_sheet_display_color("black", selected = FALSE, has_explicit_color = TRUE), "must be one of")
  expect_equal(LinkEDA:::.rls_data_sheet_display_color("blue", selected = FALSE, has_explicit_color = TRUE)$background, "#CFE8F6")
  expect_equal(LinkEDA:::.rls_data_sheet_display_color("blue", selected = TRUE, has_explicit_color = TRUE)$background, "#0072B2")

  distance_from_white <- function(hex) {
    rgb <- grDevices::col2rgb(hex)
    sqrt(colSums((255 - rgb)^2))
  }
  expect_true(all(distance_from_white(LinkEDA:::.rls_data_sheet_selected_palette_colors) >
                    distance_from_white(LinkEDA:::.rls_data_sheet_palette_colors)))
})

test_that("ambiguous implicit groups are rejected", {
  LinkEDA:::`.rls_register_plot`("plot_test_g1", "g1", "x", "y", mtcars, seq_len(nrow(mtcars)))
  LinkEDA:::`.rls_register_plot`("plot_test_g2", "g2", "x", "y", mtcars, seq_len(nrow(mtcars)))
  on.exit(LinkEDA:::`.rls_unregister_plot`("plot_test_g1"), add = TRUE)
  on.exit(LinkEDA:::`.rls_unregister_plot`("plot_test_g2"), add = TRUE)

  expect_error(LinkEDA:::.rls_resolve_group(NULL), "more than one active group")
})

test_that("regression comparison state aligns terms and fits model columns", {
  data <- mtcars
  data$hp[1] <- NA
  ls_register_dataset("regcmpcars", data)
  cmp <- ls_new_regression_comparison(
    data = "regcmpcars",
    response = "mpg",
    models = list(
      Base = "wt",
      Extended = c("wt", "hp"),
      Full = c("wt", "hp", "am")
    ),
    native = FALSE
  )
  state <- ls_regression_comparison_state(cmp)

  expect_length(state$models, 3L)
  expect_equal(state$term_rows, c("(Intercept)", "wt", "hp", "am"))
  expect_equal(vapply(state$models, `[[`, character(1), "label"), c("Base", "Extended", "Full"))
  expect_equal(vapply(state$models, `[[`, character(1), "id"),
               paste0(state$id, ":model:", 1:3))
  expect_true(all(vapply(state$models, function(x) x$fit_version, integer(1)) == 1L))
  expect_true(all(vapply(state$models, function(x) inherits(x$fitted_lm, "lm"), logical(1))))
  expect_equal(LinkEDA:::.rls_regcmp_display_cell(state, 1L, "hp"), "\u2014")
  expect_match(LinkEDA:::.rls_regcmp_display_cell(state, 2L, "wt"), "\\*")
  expect_false(identical(state$models[[1]]$coefficients$estimate,
                         state$models[[2]]$coefficients$estimate))
  expect_true(is.numeric(state$models[[1]]$summary$r_squared))
  expect_match(sprintf("%.1f%%", 100 * state$models[[1]]$summary$r_squared), "%")
  expect_false(1L %in% state$models[[2]]$rows_used)
  expect_true(1L %in% state$models[[2]]$rows_excluded)
})

test_that("regression comparison diagnostics are model-specific", {
  ls_register_dataset("regcmpdiagmodels", mtcars)
  cmp <- ls_new_regression_comparison(
    data = "regcmpdiagmodels",
    response = "mpg",
    models = list(Base = "wt", Extended = c("wt", "hp"), Full = c("wt", "hp", "am")),
    native = FALSE
  )
  state <- ls_regression_comparison_state(cmp)
  model_ids <- vapply(state$models, `[[`, character(1), "id")

  expect_equal(anyDuplicated(model_ids), 0L)
  expect_true(all(vapply(state$models, function(x) inherits(x$fitted_lm, "lm"), logical(1))))
  expect_false(isTRUE(all.equal(state$models[[1]]$diagnostics$fitted, state$models[[2]]$diagnostics$fitted)))
  expect_false(isTRUE(all.equal(state$models[[2]]$diagnostics$residual, state$models[[3]]$diagnostics$residual)))

  d1 <- ls_regression_comparison_open_diagnostic(cmp, "Base", native = FALSE)
  d2 <- ls_regression_comparison_open_diagnostic(cmp, "Extended", native = FALSE)
  d3 <- ls_regression_comparison_open_diagnostic(cmp, "Full", native = FALSE)
  s1 <- ls_glm_diagnostic_state(d1)
  s2 <- ls_glm_diagnostic_state(d2)
  s3 <- ls_glm_diagnostic_state(d3)

  expect_equal(s1$model_id, model_ids[[1]])
  expect_equal(s2$model_id, model_ids[[2]])
  expect_equal(s3$model_id, model_ids[[3]])
  expect_false(isTRUE(all.equal(s1$data$fitted, s2$data$fitted)))
  expect_false(isTRUE(all.equal(s2$data$residual, s3$data$residual)))

  v1 <- s1$displayed_fit_version
  v2 <- s2$displayed_fit_version
  v3 <- s3$displayed_fit_version
  ls_regression_comparison_set_term(cmp, "Extended", "hp", included = FALSE)
  s1_after <- ls_glm_diagnostic_state(d1)
  s2_after <- ls_glm_diagnostic_state(d2)
  s3_after <- ls_glm_diagnostic_state(d3)

  expect_equal(s1_after$displayed_fit_version, v1)
  expect_true(s2_after$displayed_fit_version > v2)
  expect_equal(s3_after$displayed_fit_version, v3)
  expect_equal(s2_after$model_id, model_ids[[2]])
  expect_equal(s2_after$data$fitted, s1_after$data$fitted, tolerance = 1e-12)
  expect_equal(s2_after$data$residual, s1_after$data$residual, tolerance = 1e-12)

  # A later change to Base refreshes only the diagnostic bound to Base.  The
  # Extended window remains attached to its stable model id and latest fit.
  ls_regression_comparison_set_term(cmp, "Base", "hp", included = TRUE)
  s1_changed <- ls_glm_diagnostic_state(d1)
  s2_unchanged <- ls_glm_diagnostic_state(d2)
  s3_unchanged <- ls_glm_diagnostic_state(d3)
  expect_true(s1_changed$displayed_fit_version > s1_after$displayed_fit_version)
  expect_equal(s2_unchanged$displayed_fit_version, s2_after$displayed_fit_version)
  expect_equal(s3_unchanged$displayed_fit_version, s3_after$displayed_fit_version)
  expect_equal(s2_unchanged$model_id, model_ids[[2]])
})

test_that("regression comparison supports model-specific responses", {
  ls_register_dataset("regcmpresponses", mtcars)
  cmp <- ls_new_regression_comparison(
    data = "regcmpresponses",
    models = list(
      Speed = list(response = "qsec", terms = c("wt", "hp")),
      Economy = list(response = "mpg", terms = c("wt", "hp", "factor(cyl)")),
      Power = list(response = "hp", terms = c("wt", "disp"))
    ),
    native = FALSE
  )
  state <- ls_regression_comparison_state(cmp)
  expect_equal(vapply(state$models, `[[`, character(1), "response"), c("qsec", "mpg", "hp"))
  expect_false(isTRUE(all.equal(state$models[[1]]$diagnostics$observed, state$models[[2]]$diagnostics$observed)))
  expect_true("cyl=4" %in% LinkEDA:::.rls_regcmp_display_rows(state)$term)
  table <- ls_regression_comparison_table(cmp)
  text <- LinkEDA:::.rls_render_table_text(table)
  expect_match(text, "Response\\s+qsec\\s+mpg\\s+hp")
  expect_match(text, "\n  4\\s+\u2014\\s+\u2014\\s+\u2014")
  expect_true(isTRUE(table$responses_differ))

  d1 <- ls_regression_comparison_open_diagnostic(cmp, "Speed", native = FALSE)
  d2 <- ls_regression_comparison_open_diagnostic(cmp, "Power", native = FALSE)
  expect_equal(ls_glm_diagnostic_state(d1)$data$observed, state$models[[1]]$diagnostics$observed)
  expect_equal(ls_glm_diagnostic_state(d2)$data$observed, state$models[[3]]$diagnostics$observed)
})

test_that("regression comparison fit rows include global p and superscript R labels", {
  ls_register_dataset("regcmpfitrows", mtcars)
  cmp <- ls_new_regression_comparison(
    data = "regcmpfitrows",
    response = "mpg",
    models = list(Base = "wt", Null = character()),
    native = FALSE
  )
  state <- ls_regression_comparison_state(cmp)
  rows <- LinkEDA:::.rls_regcmp_fit_rows(state, "Base")
  fstat <- summary(state$models[[1]]$fitted_lm)$fstatistic
  expected_p <- stats::pf(unname(fstat[["value"]]), unname(fstat[["numdf"]]),
                          unname(fstat[["dendf"]]), lower.tail = FALSE)

  expect_true("p" %in% rows$label)
  expect_equal(rows$value[rows$label == "p"], LinkEDA:::.rls_regcmp_format_p(expected_p))
  expect_true(all(c("R\u00b2", "Adjusted R\u00b2") %in% rows$label))
  expect_false(any(grepl("R2|R\\^2", rows$label)))

  null_rows <- LinkEDA:::.rls_regcmp_fit_rows(state, "Null")
  expect_equal(null_rows$value[null_rows$label == "p"], "\u2014")
})

test_that("regression comparison coefficient details distinguish partial r and delta R-squared", {
  ls_register_dataset("regcmpdetails", mtcars)
  cmp <- ls_new_regression_comparison(
    data = "regcmpdetails",
    response = "mpg",
    models = list(Extended = c("wt", "hp")),
    native = FALSE
  )
  state <- ls_regression_comparison_state(cmp)
  detail <- LinkEDA:::.rls_regcmp_coefficient_detail_text(state, "Extended", "wt")
  intercept <- LinkEDA:::.rls_regcmp_coefficient_detail_text(state, "Extended", "(Intercept)")

  expect_match(detail, "; ", fixed = TRUE)
  expect_match(detail, "Partial r", fixed = TRUE)
  expect_match(detail, "\u0394R\u00b2", fixed = TRUE)
  expect_match(detail, "\u03b2 = ")
  expect_false(grepl("R2|R\\^2|NA", detail))
  expect_match(intercept, "\u03b2 = \u2014", fixed = TRUE)
})

test_that("regression comparison unnamed models use numbered Model labels", {
  ls_register_dataset("regcmpuntitled", mtcars)
  cmp <- ls_new_regression_comparison(
    data = "regcmpuntitled",
    response = "mpg",
    models = list(c("wt"), c("wt", "hp"), Named = c("wt", "am")),
    native = FALSE
  )
  state <- ls_regression_comparison_state(cmp)
  expect_equal(vapply(state$models, `[[`, character(1), "label"),
               c("Model 1", "Model 2", "Named"))

  ls_regression_comparison_add_model(cmp, terms = "qsec")
  state2 <- ls_regression_comparison_state(cmp)
  expect_true("Model 3" %in% vapply(state2$models, `[[`, character(1), "label"))
  expect_equal(anyDuplicated(vapply(state2$models, `[[`, character(1), "id")), 0L)
})

test_that("regression comparison accepts and fits interaction terms", {
  ls_register_dataset("regcmpinteractions", mtcars)
  cmp <- ls_new_regression_comparison(
    data = "regcmpinteractions",
    response = "mpg",
    models = list(
      Main = c("wt", "hp"),
      Interaction = c("wt", "hp", "wt:hp")
    ),
    native = FALSE
  )
  state <- ls_regression_comparison_state(cmp)

  expect_true("wt:hp" %in% state$term_rows)
  expect_true("wt:hp" %in% state$models[[2L]]$terms)
  expect_true("wt:hp" %in% state$models[[2L]]$coefficients$term)
  expect_false("wt:hp" %in% state$models[[1L]]$terms)

  ls_regression_comparison_add_term(cmp, "drat:qsec")
  state <- ls_regression_comparison_state(cmp)
  expect_true("drat:qsec" %in% state$term_rows)
  ls_regression_comparison_set_term(cmp, "Main", "drat:qsec", included = TRUE)
  state <- ls_regression_comparison_state(cmp)
  expect_true(all(c("drat", "qsec", "drat:qsec") %in% state$models[[1L]]$terms))

  ls_regression_comparison_set_term(cmp, "Main", "wt:hp:qsec", included = TRUE)
  state <- ls_regression_comparison_state(cmp)
  expect_true(all(c("wt", "hp", "qsec", "wt:hp", "wt:qsec", "hp:qsec", "wt:hp:qsec") %in%
                    state$models[[1L]]$terms))

  ls_regression_comparison_set_term(cmp, "Main", "wt:hp", included = FALSE)
  state <- ls_regression_comparison_state(cmp)
  expect_true(all(c("wt", "hp", "qsec") %in% state$models[[1L]]$terms))
  expect_false(any(c("wt:hp", "wt:hp:qsec") %in% state$models[[1L]]$terms))
})

test_that("regression comparison keeps factor display rows grouped after type changes", {
  ls_register_dataset("regcmpfactortype", mtcars)
  cmp <- ls_new_regression_comparison(
    data = "regcmpfactortype",
    response = "wt",
    models = list(c("mpg", "cyl", "hp")),
    native = FALSE
  )
  state <- ls_regression_comparison_state(cmp)
  state$models[[1L]]$term_types <- list(hp = "factor")
  state <- LinkEDA:::.rls_regcmp_fit_model(state, 1L)
  state$term_rows <- unique(c("(Intercept)", state$models[[1L]]$terms))
  rows <- LinkEDA:::.rls_regcmp_display_rows(state)
  hp_index <- match("hp", rows$term)

  expect_true(is.finite(hp_index))
  expect_equal(rows$row_type[[hp_index]], "factor_parent")
  expect_true(all(startsWith(rows$term[seq.int(hp_index + 1L, hp_index + 3L)], "hp=")))
  expect_false(any(rows$term[seq.int(hp_index + 1L, hp_index + 3L)] %in% c("mpg", "cyl")))
  expect_equal(LinkEDA:::.rls_model_term_without_level_suffixes("cyl=6:am"), "cyl:am")
})

test_that("regression comparison model columns inherit a non-empty shared response", {
  ls_register_dataset("regcmp_shared_response", mtcars)
  on.exit(ls_unregister_dataset("regcmp_shared_response"), add = TRUE)

  cmp <- ls_new_regression_comparison(
    data = "regcmp_shared_response",
    response = "mpg",
    models = list(
      "Untitled 1" = list(response = "", terms = c("wt", "hp"))
    ),
    native = FALSE
  )
  state <- ls_regression_comparison_state(cmp)

  expect_identical(state$models[[1L]]$response, "mpg")
  expect_s3_class(state$models[[1L]]$fitted_lm, "lm")
  expect_s3_class(state$models[[1L]]$fit, "lm")
  expect_equal(state$models[[1L]]$summary$n_used, nrow(mtcars))
})

test_that("regression comparison refits the Alien predictor sequence", {
  alien <- data.frame(
    humans_eaten = rep(1:20, 3),
    planet = factor(rep(c("Aurelia", "Borealis", "Cygnus"), each = 20)),
    blue_eyes = rep(c(0, 1, 2, 3, 4), 12),
    eggs = rep(c(0, 1, 3, 2, 5, 4), 10)
  )
  ls_register_dataset("regcmp_alien_sequence", alien)
  on.exit(ls_unregister_dataset("regcmp_alien_sequence"), add = TRUE)

  cmp <- ls_new_regression_comparison(
    data = "regcmp_alien_sequence",
    response = "humans_eaten",
    models = list("Untitled 1" = character()),
    native = FALSE
  )
  versions <- integer()
  for (term in c("planet", "blue_eyes", "eggs")) {
    ls_regression_comparison_set_term(cmp, "Untitled 1", term, included = TRUE)
    state <- ls_regression_comparison_state(cmp)
    versions <- c(versions, state$models[[1L]]$fit_version)
    expect_s3_class(state$models[[1L]]$fitted_lm, "lm")
    expect_s3_class(state$models[[1L]]$fit, "lm")
    expect_equal(state$models[[1L]]$summary$n_used, nrow(alien))
    expect_true(term %in% state$models[[1L]]$terms)
  }

  expect_true(all(diff(versions) > 0L))
  final <- ls_regression_comparison_state(cmp)$models[[1L]]
  expect_true(all(c("planet", "blue_eyes", "eggs") %in% final$terms))
  expect_true(any(final$coefficient_rows$source_term == "planet"))
  expect_true(all(is.finite(c(final$summary$r_squared,
                              final$summary$adj_r_squared,
                              final$summary$residual_se))))
})

test_that("regression comparison predictor edits are model-specific and survive refits", {
  data <- data.frame(
    y = c(7, 10, 13, 16, 10, 15, 20, 25, 8, 12, 16, 20),
    x = rep(1:4, 3),
    planet = factor(rep(c("A", "B", "C"), each = 4))
  )
  ls_register_dataset("regcmp_model_edits", data)
  on.exit(ls_unregister_dataset("regcmp_model_edits"), add = TRUE)

  comparison <- ls_new_regression_comparison(
    data = "regcmp_model_edits",
    response = "y",
    models = list(Raw = "x", Edited = "x"),
    native = FALSE
  )
  state <- ls_regression_comparison_state(comparison)
  raw_fit <- state$models[[1L]]$fitted_lm
  raw_intercept <- unname(stats::coef(raw_fit)[["(Intercept)"]])
  raw_slope <- unname(stats::coef(raw_fit)[["x"]])

  state$models[[2L]]$centered_predictors <- "x"
  state$models[[2L]]$is_stale <- TRUE
  state <- LinkEDA:::.rls_regcmp_fit_model(state, 2L)
  centered_fit <- state$models[[2L]]$fitted_lm
  expect_identical(state$models[[1L]]$centered_predictors, character())
  expect_equal(state$models[[2L]]$centered_predictors, "x")
  expect_equal(stats::fitted(centered_fit), stats::fitted(raw_fit), tolerance = 1e-10)
  expect_equal(unname(stats::coef(centered_fit)[["x"]]), raw_slope, tolerance = 1e-10)
  expect_false(isTRUE(all.equal(
    unname(stats::coef(centered_fit)[["(Intercept)"]]), raw_intercept
  )))

  state$models[[2L]]$centered_predictors <- character()
  state$models[[2L]]$is_stale <- TRUE
  state <- LinkEDA:::.rls_regcmp_fit_model(state, 2L)
  expect_equal(stats::coef(state$models[[2L]]$fitted_lm), stats::coef(raw_fit), tolerance = 1e-10)

  state$models[[2L]]$term_types <- list(x = "factor")
  state$models[[2L]]$is_stale <- TRUE
  state <- LinkEDA:::.rls_regcmp_fit_model(state, 2L)
  expect_identical(state$models[[1L]]$term_types, list())
  expect_identical(state$models[[2L]]$term_types, list(x = "factor"))
  expect_true(is.numeric(state$models[[1L]]$fitted_lm$model$x))
  expect_true(is.factor(state$models[[2L]]$fitted_lm$model$x))
  expect_equal(length(grep("^x", names(stats::coef(state$models[[1L]]$fitted_lm)))), 1L)
  expect_equal(length(grep("^x", names(stats::coef(state$models[[2L]]$fitted_lm)))), 3L)
  expect_true(all(c("x", "x=1", "x=2", "x=3", "x=4") %in%
                    state$models[[2L]]$coefficient_rows$term))

  factor_fitted <- stats::fitted(state$models[[2L]]$fitted_lm)
  state$models[[2L]]$factor_reference_levels <- list(x = "3")
  state$models[[2L]]$is_stale <- TRUE
  state <- LinkEDA:::.rls_regcmp_fit_model(state, 2L)
  expect_equal(stats::fitted(state$models[[2L]]$fitted_lm), factor_fitted,
               tolerance = 1e-10)
  expect_identical(levels(state$models[[2L]]$fitted_lm$model$x)[[1L]], "3")
  expect_false(any(grepl("^x3$", names(stats::coef(state$models[[2L]]$fitted_lm)))))

  state$models[[2L]]$term_types <- list(x = "numeric")
  state$models[[2L]]$factor_reference_levels <- list()
  state$models[[2L]]$is_stale <- TRUE
  state <- LinkEDA:::.rls_regcmp_fit_model(state, 2L)
  expect_true(is.numeric(state$models[[2L]]$fitted_lm$model$x))
  expect_equal(stats::coef(state$models[[2L]]$fitted_lm), stats::coef(raw_fit), tolerance = 1e-10)
})

test_that("regression comparison rebuilds interactions after centering and type edits", {
  data <- data.frame(
    y = c(7, 10, 13, 16, 10, 15, 20, 25, 8, 12, 16, 20),
    x = rep(1:4, 3),
    planet = factor(rep(c("A", "B", "C"), each = 4))
  )
  ls_register_dataset("regcmp_interaction_edits", data)
  on.exit(ls_unregister_dataset("regcmp_interaction_edits"), add = TRUE)
  comparison <- ls_new_regression_comparison(
    data = "regcmp_interaction_edits",
    response = "y",
    models = list(Raw = c("x", "planet", "x:planet"),
                  Edited = c("x", "planet", "x:planet")),
    native = FALSE
  )
  state <- ls_regression_comparison_state(comparison)
  raw_fit <- state$models[[1L]]$fitted_lm

  state$models[[2L]]$centered_predictors <- "x"
  state$models[[2L]]$is_stale <- TRUE
  state <- LinkEDA:::.rls_regcmp_fit_model(state, 2L)
  expect_equal(stats::fitted(state$models[[2L]]$fitted_lm),
               stats::fitted(raw_fit), tolerance = 1e-9)
  expect_true(any(grepl("x:planet", names(stats::coef(state$models[[2L]]$fitted_lm)), fixed = TRUE)))

  state$models[[2L]]$centered_predictors <- character()
  state$models[[2L]]$term_types <- list(x = "factor")
  state$models[[2L]]$is_stale <- TRUE
  state <- LinkEDA:::.rls_regcmp_fit_model(state, 2L)
  expect_true(is.factor(state$models[[2L]]$fitted_lm$model$x))
  expect_true(any(grepl("x2:planet", names(stats::coef(state$models[[2L]]$fitted_lm)), fixed = TRUE)))
  expect_true(is.numeric(state$models[[1L]]$fitted_lm$model$x))
  expect_identical(state$models[[1L]]$term_types, list())
})

test_that("regression comparison model specifications preserve factor references", {
  data <- data.frame(
    y = c(7, 10, 13, 16, 10, 15, 20, 25, 8, 12, 16, 20),
    x = rep(1:4, 3)
  )
  ls_register_dataset("regcmp_factor_reference_spec", data)
  on.exit(ls_unregister_dataset("regcmp_factor_reference_spec"), add = TRUE)

  comparison <- ls_new_regression_comparison(
    data = "regcmp_factor_reference_spec",
    response = "y",
    models = list(Factor = list(
      terms = "x",
      term_types = list(x = "factor"),
      factor_reference_levels = list(x = "3")
    )),
    native = FALSE
  )
  model <- ls_regression_comparison_state(comparison)$models[[1L]]
  expect_identical(model$factor_reference_levels, list(x = "3"))
  expect_identical(levels(model$fitted_lm$model$x)[[1L]], "3")
  expect_false(any(grepl("^x3$", names(stats::coef(model$fitted_lm)))))
})

test_that("generalized linear model fits, validates links, renders factors, and exports", {
  ls_register_dataset("gglmcars", mtcars)
  g <- ls_new_generalized_linear_model(
    data = "gglmcars",
    response = "am",
    terms = c("wt", "hp", "factor(cyl)"),
    family = "binomial",
    link = "logit",
    native = FALSE
  )
  expect_s3_class(g, "rlispstat_generalized_linear_model")
  rows <- ls_generalized_linear_model_coefficient_rows(g)
  expect_true(all(c("cyl", "cyl=4", "cyl=6", "cyl=8") %in% rows$term))
  expect_equal(rows$row_type[rows$term == "cyl=4"], "reference")
  expect_equal(unique(rows$statistic_name[rows$row_type == "factor_level"]), "z")
  coefficient_rows <- rows[rows$row_type %in% c("coefficient", "factor_level"), , drop = FALSE]
  coefficient_rows <- coefficient_rows[is.finite(coefficient_rows$std_error), , drop = FALSE]
  expect_true(nrow(coefficient_rows) > 0L)
  expect_true(all(is.finite(coefficient_rows$ci_lower)))
  expect_true(all(is.finite(coefficient_rows$ci_upper)))
  expect_equal(
    coefficient_rows$ci_lower,
    coefficient_rows$estimate - stats::qnorm(0.975) * coefficient_rows$std_error,
    tolerance = 1e-10
  )
  expect_equal(
    coefficient_rows$ci_upper,
    coefficient_rows$estimate + stats::qnorm(0.975) * coefficient_rows$std_error,
    tolerance = 1e-10
  )
  summary <- ls_generalized_linear_model_fit_summary(g)
  expect_equal(summary$family, "binomial")
  expect_equal(summary$link, "logit")
  expect_true(is.finite(summary$residual_deviance))
  expect_true(summary$converged)
  expect_gte(summary$iterations, 1L)
  expect_gte(summary$rank, 1L)
  expect_gte(summary$parameter_count, summary$rank)

  diag <- ls_generalized_linear_model_diagnostics(g)
  expect_true(all(c("original_row_id", "fitted_response_scale", "linear_predictor",
                    "deviance_residual", "pearson_residual") %in% names(diag)))
  p <- ls_generalized_linear_model_open_diagnostic(g, "observed_vs_fitted")
  expect_equal(ls_glm_diagnostic_state(p)$model_id, g$id)

  before <- ls_generalized_linear_model_state(g)$fit_version
  g <- suppressWarnings(ls_generalized_linear_model_set_link(g, "probit"))
  expect_true(ls_generalized_linear_model_state(g)$fit_version > before)
  expect_equal(ls_generalized_linear_model_state(g)$link, "probit")

  gp <- ls_new_generalized_linear_model(
    data = data.frame(y = c(1, 2, 3, 4), x = c(0, 1, 2, 3)),
    response = "y",
    terms = "x",
    family = "poisson",
    link = "log",
    native = FALSE
  )
  expect_equal(ls_generalized_linear_model_fit_summary(gp)$family, "poisson")
  poisson_rows <- ls_generalized_linear_model_coefficient_rows(gp)
  expect_true(all(is.finite(poisson_rows$ci_lower[poisson_rows$row_type == "coefficient"])))
  poisson_payload <- LinkEDA:::.rls_native_generalized_state_payload(
    LinkEDA:::.rls_generalized_glm_record(gp)
  )
  ci_marker <- match("GENERALIZED_CI_V1", poisson_payload)
  expect_true(is.finite(ci_marker))
  expect_identical(poisson_payload[[ci_marker + 1L]], as.character(nrow(poisson_rows)))
  meta_marker <- match("GENERALIZED_META_V1", poisson_payload)
  expect_true(is.finite(meta_marker))
  expect_identical(poisson_payload[[meta_marker + 1L]], "TRUE")
  expect_error(
    ls_new_generalized_linear_model(data = "gglmcars", response = "am", terms = "wt",
                                    family = "poisson", link = "logit", native = FALSE),
    "Link `logit` is not available for Poisson"
  )
  text <- LinkEDA:::.rls_render_table_text(ls_generalized_linear_model_table(g))
  expect_match(text, "Generalized Linear Model")
  expect_match(text, "Residual deviance")
  expect_false(grepl("R\\u00b2|Adjusted R", text))
  pdf <- tempfile(fileext = ".pdf")
  ls_export_generalized_linear_model_table(g, pdf)
  expect_true(file.exists(pdf))
  expect_gt(file.info(pdf)$size, 0)
})

test_that("convergence metadata is present for every supported GLM family", {
  family_cases <- list(
    gaussian = list(data = mtcars, response = "mpg", link = "identity"),
    binomial = list(data = mtcars, response = "am", link = "logit"),
    poisson = list(data = warpbreaks, response = "breaks", link = "log"),
    Gamma = list(data = mtcars, response = "mpg", link = "inverse"),
    inverse.gaussian = list(data = mtcars, response = "mpg", link = "1/mu^2"),
    quasibinomial = list(data = mtcars, response = "am", link = "logit"),
    quasipoisson = list(data = warpbreaks, response = "breaks", link = "log")
  )
  for (family_name in names(family_cases)) {
    case <- family_cases[[family_name]]
    predictor <- if (identical(case$response, "breaks")) "tension" else "wt"
    model <- ls_new_generalized_linear_model(
      data = case$data,
      response = case$response,
      terms = predictor,
      family = family_name,
      link = case$link,
      native = FALSE
    )
    fit_summary <- ls_generalized_linear_model_fit_summary(model)
    expect_true(fit_summary$converged, info = family_name)
    expect_true(fit_summary$iterations >= 1L, info = family_name)
    payload <- LinkEDA:::.rls_native_generalized_state_payload(
      LinkEDA:::.rls_generalized_glm_record(model)
    )
    marker <- match("GENERALIZED_META_V1", payload)
    expect_true(is.finite(marker), info = family_name)
    expect_identical(payload[[marker + 1L]], "TRUE", info = family_name)
  }
})

test_that("model numeric interpretation uses 0/1 only for binary labels", {
  typed <- LinkEDA:::.rls_model_data_for_term_types(
    data.frame(
      y = seq_len(6L),
      happy = rep(c("no", "yes"), 3L),
      numeric_factor = factor(rep(c("10", "20", "30"), 2L))
    ),
    list(happy = "numeric", numeric_factor = "numeric"),
    response = "y"
  )
  expect_equal(typed$happy, rep(c(0, 1), 3L))
  expect_equal(typed$numeric_factor, rep(c(10, 20, 30), 2L))

  source <- data.frame(
    y = c(2, 4, 3, 7, 5, 10),
    happy = rep(c("no", "yes"), 3L),
    stringsAsFactors = FALSE
  )
  model <- ls_new_generalized_linear_model(
    data = source,
    response = "y",
    terms = "happy",
    family = "gaussian",
    link = "identity",
    term_types = list(happy = "numeric"),
    native = FALSE
  )
  record <- LinkEDA:::.rls_generalized_glm_record(model)
  expect_equal(record$summary$n_used, nrow(source))
  expect_equal(record$fit$model$happy, rep(c(0, 1), 3L))
  expect_true(is.finite(unname(stats::coef(record$fit)[["happy"]])))
  expect_identical(source$happy, rep(c("no", "yes"), 3L))

  expect_error(
    LinkEDA:::.rls_model_data_for_term_types(
      data.frame(y = 1:3, nominal = c("Low", "Middle", "High")),
      list(nominal = "numeric"), response = "y"
    ),
    "explicit global numeric mapping"
  )
})

test_that("model factor interpretation preserves ordered levels and permits references", {
  source <- data.frame(
    y = seq_len(9L),
    rating = ordered(rep(c("low", "mid", "high"), 3L),
                     levels = c("low", "mid", "high"))
  )
  typed <- LinkEDA:::.rls_model_data_for_term_types(
    source, list(rating = "factor"), response = "y"
  )
  expect_true(is.factor(typed$rating))
  expect_false(is.ordered(typed$rating))
  expect_identical(levels(typed$rating), c("low", "mid", "high"))
  expect_true(is.ordered(source$rating))

  referenced <- LinkEDA:::.rls_glm_apply_factor_references(
    typed, list(rating = "mid")
  )
  expect_identical(levels(referenced$rating), c("mid", "low", "high"))
  expect_identical(
    colnames(stats::model.matrix(y ~ rating, data = referenced)),
    c("(Intercept)", "ratinglow", "ratinghigh")
  )
})

test_that("generalized linear model accepts interaction terms", {
  ls_register_dataset("gglminteractions", mtcars)
  g <- ls_new_generalized_linear_model(
    data = "gglminteractions",
    response = "mpg",
    terms = "wt:hp:qsec",
    family = "gaussian",
    native = FALSE
  )
  state <- ls_generalized_linear_model_state(g)

  expect_true(all(c("wt", "hp", "qsec", "wt:hp", "wt:qsec", "hp:qsec", "wt:hp:qsec") %in% state$terms))
  expect_true("wt:hp:qsec" %in% state$coefficients$term)
  expect_true(is.finite(state$summary$residual_deviance))
})

test_that("generalized linear model interactions are semantically typed and grouped", {
  d <- mtcars
  d$gear <- factor(d$gear)
  d$am <- factor(d$am)
  g <- ls_new_generalized_linear_model(
    data = d,
    response = "cyl",
    terms = c("gear", "am", "gear:am"),
    family = "poisson",
    link = "log",
    native = FALSE
  )
  rows <- ls_generalized_linear_model_coefficient_rows(g)

  parent <- rows[rows$term == "gear:am", , drop = FALSE]
  expect_equal(parent$row_type, "term_parent")
  expect_equal(parent$term_type, "factor_factor_interaction")
  children <- rows[rows$source_term == "gear:am" & rows$row_type == "coefficient", , drop = FALSE]
  expect_gt(nrow(children), 0)
  expect_true(all(children$term_type == "factor_factor_interaction"))
  expect_true(all(children$term %in% colnames(stats::model.matrix(LinkEDA:::.rls_generalized_glm_record(g)$fit))))
})

test_that("generalized model specifications preserve family semantics across transformations", {
  set.seed(314159)
  n <- 120L
  x <- rep(seq(-2.5, 2.5, length.out = n / 3L), 3L)
  group_code <- rep(c(1, 2, 3), each = n / 3L)
  group_shift <- c(`1` = -0.4, `2` = 0.7, `3` = 0.1)[as.character(group_code)]
  gaussian_data <- data.frame(
    y = 10 + 1.25 * x + group_shift +
      c(`1` = 0.2, `2` = -0.15, `3` = 0.35)[as.character(group_code)] * x +
      stats::rnorm(n, sd = 0.35),
    x = x,
    group_code = group_code
  )
  ls_register_dataset("gglm_shared_spec_gaussian", gaussian_data)
  on.exit(ls_unregister_dataset("gglm_shared_spec_gaussian"), add = TRUE)

  make_gaussian <- function(centered = character(), reference = "1") {
    ls_new_generalized_linear_model(
      data = "gglm_shared_spec_gaussian",
      response = "y",
      terms = c("x", "group_code", "x:group_code"),
      family = "gaussian",
      link = "identity",
      term_types = list(group_code = "factor"),
      centered_predictors = centered,
      factor_reference_levels = list(group_code = reference),
      native = FALSE
    )
  }

  raw <- LinkEDA:::.rls_generalized_glm_record(make_gaussian())
  centered <- LinkEDA:::.rls_generalized_glm_record(make_gaussian("x"))
  rereferenced <- LinkEDA:::.rls_generalized_glm_record(make_gaussian(reference = "2"))

  expect_identical(centered$centered_predictors, "x")
  expect_identical(centered$term_types, list(group_code = "factor"))
  expect_identical(rereferenced$factor_reference_levels, list(group_code = "2"))
  expect_identical(levels(rereferenced$fit$model$group_code)[[1L]], "2")
  expect_false(any(grepl("^group_code2$", names(stats::coef(rereferenced$fit)))))

  for (changed in list(centered, rereferenced)) {
    expect_equal(stats::predict(changed$fit, type = "response"),
                 stats::predict(raw$fit, type = "response"), tolerance = 1e-9)
    expect_equal(stats::predict(changed$fit, type = "link"),
                 stats::predict(raw$fit, type = "link"), tolerance = 1e-9)
    expect_equal(stats::deviance(changed$fit), stats::deviance(raw$fit), tolerance = 1e-9)
    expect_equal(as.numeric(stats::logLik(changed$fit)),
                 as.numeric(stats::logLik(raw$fit)), tolerance = 1e-9)
    expect_equal(stats::AIC(changed$fit), stats::AIC(raw$fit), tolerance = 1e-9)
  }
  expect_identical(LinkEDA:::.rls_dataset_record("gglm_shared_spec_gaussian")$data$x,
                   gaussian_data$x)
  expect_true(is.numeric(LinkEDA:::.rls_dataset_record("gglm_shared_spec_gaussian")$data$group_code))

  changed_family <- ls_generalized_linear_model_set_family(make_gaussian("x", "2"), "Gamma", "log")
  changed_state <- ls_generalized_linear_model_state(changed_family)
  expect_identical(changed_state$centered_predictors, "x")
  expect_identical(changed_state$factor_reference_levels, list(group_code = "2"))
  expect_identical(changed_state$term_types, list(group_code = "factor"))
  expect_identical(changed_state$family, "Gamma")
  expect_identical(changed_state$link, "log")
})

test_that("binomial and Poisson generalized fits are invariant to shared reparameterization", {
  set.seed(271828)
  n <- 180L
  x <- rep(seq(-2, 2, length.out = n / 3L), 3L)
  group <- factor(rep(c("A", "B", "C"), each = n / 3L), levels = c("A", "B", "C"))
  group_lp <- c(A = -0.35, B = 0.55, C = 0.1)[as.character(group)]
  interaction_lp <- c(A = 0.15, B = -0.2, C = 0.3)[as.character(group)] * x
  binomial_data <- data.frame(
    y = stats::rbinom(n, 1, stats::plogis(-0.2 + 0.65 * x + group_lp + interaction_lp)),
    x = x,
    group = group
  )
  poisson_data <- data.frame(
    y = stats::rpois(n, exp(1.1 + 0.22 * x + group_lp / 2 + interaction_lp / 3)),
    x = x,
    group = group
  )

  compare_parameterizations <- function(data, family, link, prefix) {
    ls_register_dataset(prefix, data)
    on.exit(ls_unregister_dataset(prefix), add = TRUE)
    make_model <- function(centered = character(), reference = "A") {
      LinkEDA:::.rls_generalized_glm_record(ls_new_generalized_linear_model(
        data = prefix,
        response = "y",
        terms = c("x", "group", "x:group"),
        family = family,
        link = link,
        centered_predictors = centered,
        factor_reference_levels = list(group = reference),
        native = FALSE
      ))
    }
    raw <- make_model()
    centered <- make_model("x")
    rereferenced <- make_model(reference = "B")
    expect_identical(raw$summary$family, family)
    expect_identical(raw$summary$link, link)
    expect_identical(levels(rereferenced$fit$model$group)[[1L]], "B")
    for (changed in list(centered, rereferenced)) {
      expect_equal(stats::predict(changed$fit, type = "response"),
                   stats::predict(raw$fit, type = "response"), tolerance = 1e-8)
      expect_equal(stats::predict(changed$fit, type = "link"),
                   stats::predict(raw$fit, type = "link"), tolerance = 1e-8)
      expect_equal(stats::deviance(changed$fit), stats::deviance(raw$fit), tolerance = 1e-8)
      expect_equal(as.numeric(stats::logLik(changed$fit)),
                   as.numeric(stats::logLik(raw$fit)), tolerance = 1e-8)
      expect_equal(stats::AIC(changed$fit), stats::AIC(raw$fit), tolerance = 1e-8)
    }
    expect_identical(LinkEDA:::.rls_dataset_record(prefix)$data$x, data$x)
  }

  compare_parameterizations(binomial_data, "binomial", "logit", "gglm_shared_spec_binomial")
  compare_parameterizations(poisson_data, "poisson", "log", "gglm_shared_spec_poisson")
})

test_that("native generalized payload carries the shared model specification", {
  model <- ls_new_generalized_linear_model(
    data = data.frame(y = c(2, 4, 5, 7, 9, 12), x = 1:6, group = c(1, 1, 2, 2, 3, 3)),
    response = "y",
    terms = c("x", "group", "x:group"),
    family = "gaussian",
    term_types = list(group = "factor"),
    centered_predictors = "x",
    factor_reference_levels = list(group = "2"),
    native = FALSE,
    .native_generation = 23L
  )
  payload <- LinkEDA:::.rls_generalized_glm_native_payload(
    LinkEDA:::.rls_generalized_glm_record(model)
  )
  marker <- match("MODEL_SPEC_V1", payload)
  expect_true(is.finite(marker))
  expect_identical(payload[[marker + 1L]], "1")
  expect_identical(payload[[marker + 2L]], "x")
  expect_identical(payload[[marker + 3L]], "1")
  expect_identical(payload[[marker + 4L]], "group")
  expect_identical(payload[[marker + 5L]], "2")
  result_marker <- match("GGLM_RESULT_V1", payload)
  expect_true(is.finite(result_marker))
  expect_identical(payload[[result_marker + 1L]], "23")
  expect_true(match("ANALYSIS_PROVENANCE_V2", payload) > result_marker)
})

test_that("native generalized linear model payload compacts high-cardinality factors", {
  set.seed(1)
  d <- data.frame(
    y = stats::rbinom(250, 1, 0.5),
    x = seq_len(250)
  )
  g <- suppressWarnings(ls_new_generalized_linear_model(
    data = d,
    response = "y",
    terms = "x",
    family = "binomial",
    link = "logit",
    native = FALSE,
    term_types = c(x = "factor")
  ))

  record <- LinkEDA:::.rls_generalized_glm_record(g)
  expect_gt(nrow(record$coefficient_rows), 200)
  compact <- LinkEDA:::.rls_native_compact_factor_coefficient_rows(record$coefficient_rows)
  expect_lt(nrow(compact), 60)
  expect_true(any(grepl("more categories", compact$display_label, fixed = TRUE)))

  payload <- LinkEDA:::.rls_native_generalized_state_payload(record)
  # Provenance grows independently of the compacted statistical rows.
  # Confirm that only the compact factor rows enter the result transport.
  expect_true(any(grepl("more categories", payload, fixed = TRUE)))
  expect_false(any(grepl("x = 250", payload, fixed = TRUE)))
})

test_that("native generalized linear model tasks report R fit errors", {
  ls_register_dataset("gglm_fit_error", data.frame(y = c(-1, 0, 1), x = c(1, 2, 3)))
  expect_message(
    LinkEDA:::.rls_handle_generalized_glm_needed(
      id = "gglm_fit_error_model",
      group = "gglm_fit_error",
      response = "y",
      family = "poisson",
      link = "log",
      scope = "all",
      terms = "x"
    ),
    "generalized GLM task failed"
  )
})

test_that("generalized GLM native requests preserve the legacy prefix and parse model extensions", {
  legacy <- paste(
    c("GGLM_NEEDED", "g1", "cars", "cyl", "poisson", "log", "selected",
      "1", "mpg", "1", "mpg", "numeric", "GENERALIZED", "", "", "2", "1", "60"),
    collapse = "\t"
  )
  parsed_legacy <- LinkEDA:::.rls_parse_generalized_glm_needed(
    strsplit(legacy, "\t", fixed = TRUE)[[1L]]
  )
  expect_identical(parsed_legacy$terms, "mpg")
  expect_identical(parsed_legacy$term_types, list(mpg = "numeric"))
  expect_identical(parsed_legacy$selected_rows, c(1L, 60L))
  expect_identical(parsed_legacy$centered_predictors, character())
  expect_identical(parsed_legacy$factor_reference_levels, list())
  expect_identical(parsed_legacy$generation, 0L)

  extended <- paste0(
    legacy,
    "\tMODEL_SPEC_V1\t1\tmpg\t1\tgear\t4\tGGLM_REQUEST_V1\t17"
  )
  parsed_extended <- LinkEDA:::.rls_parse_generalized_glm_needed(
    strsplit(extended, "\t", fixed = TRUE)[[1L]]
  )
  expect_identical(parsed_extended$selected_rows, c(1L, 60L))
  expect_identical(parsed_extended$centered_predictors, "mpg")
  expect_identical(parsed_extended$factor_reference_levels, list(gear = "4"))
  expect_identical(parsed_extended$generation, 17L)

  bounded <- sub(
    "GGLM_REQUEST_V1",
    "BOUNDED_RESPONSE_V1\t0\t100\tGGLM_REQUEST_V1",
    extended,
    fixed = TRUE
  )
  parsed_bounded <- LinkEDA:::.rls_parse_generalized_glm_needed(
    strsplit(bounded, "\t", fixed = TRUE)[[1L]]
  )
  expect_equal(parsed_bounded$response_bounds, c(0, 100))
  expect_identical(parsed_bounded$generation, 17L)
})

test_that("Pearson correlation matrices compute pairwise/listwise cells and export", {
  d <- data.frame(
    x = c(1, 2, 3, 4, NA),
    y = c(2, 4, 6, 8, 10),
    z = c(1, NA, 2, 3, 4),
    label = letters[1:5]
  )
  ls_register_dataset("corrdata", d)
  cm_empty <- ls_new_correlation_matrix(data = "corrdata", native = FALSE)
  empty_state <- ls_correlation_matrix_state(cm_empty)
  expect_identical(empty_state$variables, character())
  expect_equal(nrow(empty_state$results), 0L)
  cm <- ls_new_correlation_matrix(
    data = "corrdata",
    variables = c("x", "y", "z"),
    missing = "pairwise",
    native = FALSE
  )
  expect_s3_class(cm, "rlispstat_correlation_matrix")
  state <- ls_correlation_matrix_state(cm)
  expect_equal(state$group, "corrdata")
  expect_equal(state$variables, c("x", "y", "z"))
  expect_false(state$show_n)
  expect_false(state$show_p_value)
  expect_true(state$show_significance_stars)

  cm_custom <- ls_new_correlation_matrix(
    data = "corrdata",
    variables = c("x", "y"),
    show_p_value = TRUE,
    show_significance_stars = FALSE,
    native = FALSE
  )
  custom_state <- ls_correlation_matrix_state(cm_custom)
  expect_true(custom_state$show_p_value)
  expect_false(custom_state$show_significance_stars)

  cells <- ls_correlation_matrix_cells(cm)
  xy <- cells[cells$x_variable == "x" & cells$y_variable == "y", ]
  xz <- cells[cells$x_variable == "x" & cells$y_variable == "z", ]
  expect_equal(xy$n, 4L)
  expect_equal(xy$rows_used_original_ids[[1L]], 1:4)
  expect_equal(xy$r, stats::cor(d$x, d$y, use = "pairwise.complete.obs"), tolerance = 1e-12)
  expect_equal(xz$n, 3L)
  expect_equal(xz$rows_used_original_ids[[1L]], c(1L, 3L, 4L))

  cm_listwise <- ls_new_correlation_matrix(
    data = "corrdata",
    variables = c("x", "y", "z"),
    missing = "listwise",
    native = FALSE
  )
  listwise_cells <- ls_correlation_matrix_cells(cm_listwise)
  expect_true(all(listwise_cells$n[listwise_cells$status != "diagonal"] == 3L))
  expect_error(
    ls_new_correlation_matrix(data = "corrdata", variables = c("x", "label"), native = FALSE),
    "must be numeric"
  )

  cm2 <- ls_new_correlation_matrix(data = "corrdata", variables = c("x", "y"), native = FALSE)
  cm2 <- ls_correlation_matrix_add_variable(cm2, "z")
  expect_equal(ls_correlation_matrix_state(cm2)$variables, c("x", "y", "z"))
  cm2 <- ls_correlation_matrix_remove_variable(cm2, "y")
  expect_equal(ls_correlation_matrix_state(cm2)$variables, c("x", "z"))

  table <- ls_correlation_matrix_table(cm)
  expect_equal(table$title, "Pearson Correlation Matrix")
  text <- ls_copy_correlation_matrix(cm)
  expect_match(text, "Pearson correlations")
  pdf <- tempfile(fileext = ".pdf")
  ls_export_correlation_matrix(cm, pdf)
  expect_true(file.exists(pdf))
  expect_gt(file.info(pdf)$size, 0)
})

test_that("quick cluster builds a dendrogram object and supports variable updates", {
  d <- data.frame(
    x = c(1, 2, 3, 4, 5, 6),
    y = c(1, 2, 3, 4, 5, 6),
    z = c(6, 5, 4, 3, 2, 1),
    label = letters[1:6]
  )
  ls_register_dataset("clusterdata", d)

  den_empty <- ls_new_quick_cluster(data = "clusterdata", native = FALSE)
  empty_state <- ls_dendrogram_state(den_empty)
  expect_identical(empty_state$variables, character())
  expect_null(empty_state$hclust)

  den <- ls_new_quick_cluster(
    data = "clusterdata",
    variables = c("x", "y", "z"),
    linkage = "complete",
    native = FALSE
  )
  expect_s3_class(den, "rlispstat_dendrogram")
  state <- ls_dendrogram_state(den)
  expect_equal(state$group, "clusterdata")
  expect_equal(state$distance, "euclidean")
  expect_equal(state$linkage, "complete")
  expect_equal(state$variables, c("x", "y", "z"))
  expect_true(inherits(ls_dendrogram_hclust(den), "hclust"))
  expect_equal(state$case_rows, 1:6)
  expect_equal(nrow(state$merge), length(state$case_rows) - 1L)
  expect_equal(ls_dendrogram_hclust(den)$labels, as.character(1:6))

  den2 <- ls_new_quick_cluster(data = "clusterdata", variables = c("x", "y"), native = FALSE)
  den2 <- ls_dendrogram_add_variable(den2, "z")
  expect_equal(ls_dendrogram_state(den2)$variables, c("x", "y", "z"))
  den2 <- ls_dendrogram_remove_variable(den2, "y")
  expect_equal(ls_dendrogram_state(den2)$variables, c("x", "z"))
  den2 <- ls_dendrogram_remove_variable(den2, "x")
  expect_equal(ls_dendrogram_state(den2)$variables, "z")

  expect_error(
    ls_new_quick_cluster(data = "clusterdata", variables = c("x", "label"), native = FALSE),
    "must be numeric"
  )
  den2 <- ls_dendrogram_remove_variable(den2, "z")
  expect_identical(ls_dendrogram_state(den2)$variables, character())
  expect_null(ls_dendrogram_state(den2)$hclust)
})

test_that("regression comparison refits only affected model for term changes", {
  ls_register_dataset("regcmpterm", mtcars)
  cmp <- ls_new_regression_comparison(
    data = "regcmpterm",
    response = "mpg",
    models = list(Base = "wt", Extended = c("wt", "hp"), Full = c("wt", "hp", "am")),
    native = FALSE
  )
  before <- ls_regression_comparison_state(cmp)
  before_versions <- vapply(before$models, `[[`, integer(1), "fit_version")
  ls_regression_comparison_set_term(cmp, 2L, "qsec", included = TRUE)
  after <- ls_regression_comparison_state(cmp)
  after_versions <- vapply(after$models, `[[`, integer(1), "fit_version")

  expect_equal(after_versions[2], before_versions[2] + 1L)
  expect_equal(after_versions[-2], before_versions[-2])
  expect_true("qsec" %in% after$term_rows)
  expect_true("qsec" %in% after$models[[2]]$terms)
})

test_that("regression comparison response and scope changes refit all models", {
  ls_register_dataset("regcmpresponse", mtcars)
  cmp <- ls_new_regression_comparison(
    data = "regcmpresponse",
    response = "mpg",
    models = list(Base = "wt", Extended = c("wt", "hp")),
    native = FALSE
  )
  state <- ls_regression_comparison_state(cmp)
  state$response <- "qsec"
  for (i in seq_along(state$models)) {
    state$models[[i]]$is_stale <- TRUE
    state$models[[i]]$model_version <- state$models[[i]]$model_version + 1L
  }
  assign(state$id, state, envir = LinkEDA:::.rls_state$regression_comparisons)
  refit <- LinkEDA:::.rls_regcmp_fit_models(state, force = TRUE)

  expect_true(all(vapply(refit$models, `[[`, integer(1), "fit_version") == 2L))
  expect_equal(refit$response, "qsec")
  refit$scope <- "selected"
  refit$selected_rows <- seq_len(10L)
  for (i in seq_along(refit$models)) refit$models[[i]]$is_stale <- TRUE
  refit2 <- LinkEDA:::.rls_regcmp_fit_models(refit, force = TRUE)
  expect_true(all(vapply(refit2$models, `[[`, integer(1), "fit_version") == 3L))
})

test_that("regression table exports use superscript R labels and p row", {
  ls_register_dataset("export_glm", mtcars)
  on.exit(ls_unregister_dataset("export_glm"), add = TRUE)
  m <- ls_new_glm("export_glm")
  ls_glm_add_predictor(m, "wt")
  m <- ls_glm_fit(m)
  table <- ls_glm_regression_table(m)
  text <- LinkEDA:::.rls_render_table_text(table)
  markdown <- LinkEDA:::.rls_render_table_markdown(table)
  expect_match(text, "R\u00b2")
  expect_match(markdown, "Adjusted R\u00b2")
  expect_false(grepl("R2|R\\^2", text))
  expect_match(text, "\np\\s+< .001")
  pdf <- tempfile(fileext = ".pdf")
  ls_export_glm_table(m, pdf)
  expect_true(file.exists(pdf))
  expect_gt(file.info(pdf)$size, 0)
})

test_that("comparison table export contains model columns and exact p row", {
  ls_register_dataset("export_cmp", mtcars)
  on.exit(ls_unregister_dataset("export_cmp"), add = TRUE)
  cmp <- ls_new_regression_comparison("export_cmp", response = "mpg",
                                      models = list(Base = "wt", Extended = c("wt", "hp")),
                                      native = FALSE)
  table <- ls_regression_comparison_table(cmp)
  text <- LinkEDA:::.rls_render_table_text(table)
  expect_match(text, "Base")
  expect_match(text, "Extended")
  expect_match(text, "\np\\s+< .001\\s+< .001")
  expect_false(grepl("R2|R\\^2|p model", text))
  md <- tempfile(fileext = ".md")
  ls_export_regression_comparison_table(cmp, md, format = "md")
  expect_true(file.exists(md))
  expect_match(paste(readLines(md, encoding = "UTF-8"), collapse = "\n"), "R\u00b2")
})

test_that("linked plot export creates non-empty academic graphics without native backend", {
  old_theme <- ls_plot_theme()
  on.exit(ls_plot_theme(old_theme), add = TRUE)
  ls_plot_theme("bw")
  expect_equal(ls_plot_theme(), "bw")

  data <- data.frame(y = c(1, 2, 3, 4), g = c("a", "a", "b", "b"), x = c(1, 1, 2, 3))
  box_id <- "box_export_test"
  hist_id <- "hist_export_test"
  scatter_id <- "scatter_export_test"
  bar_id <- "bar_export_test"
  LinkEDA:::.rls_register_plot(box_id, "export_plot_group", "g", "y", data, seq_len(nrow(data)),
                                 type = "boxplot", title = "Box export")
  LinkEDA:::.rls_register_plot(hist_id, "export_hist_group", "x", "", data, seq_len(nrow(data)),
                                 type = "histogram", title = "Histogram export",
                                 breaks = c(1, 2, 3))
  LinkEDA:::.rls_register_plot(scatter_id, "export_scatter_group", "x", "y", data, seq_len(nrow(data)),
                                 type = "scatter", title = "Scatter export")
  LinkEDA:::.rls_register_plot(bar_id, "export_bar_group", "g", "", data, seq_len(nrow(data)),
                                 type = "barplot", title = "Bar export")
  on.exit(LinkEDA:::.rls_unregister_plot(box_id), add = TRUE)
  on.exit(LinkEDA:::.rls_unregister_plot(hist_id), add = TRUE)
  on.exit(LinkEDA:::.rls_unregister_plot(scatter_id), add = TRUE)
  on.exit(LinkEDA:::.rls_unregister_plot(bar_id), add = TRUE)
  box <- structure(list(id = box_id, group = "export_plot_group", x = "g", y = "y",
                        title = "Box export", type = "boxplot"), class = "rlispstat_plot")
  hist <- structure(list(id = hist_id, group = "export_hist_group", x = "x", y = "",
                         title = "Histogram export", type = "histogram", breaks = c(1, 2, 3)),
                    class = "rlispstat_plot")
  scatter <- structure(list(id = scatter_id, group = "export_scatter_group", x = "x", y = "y",
                            title = "Scatter export", type = "scatter"), class = "rlispstat_plot")
  bar <- structure(list(id = bar_id, group = "export_bar_group", x = "g", y = "",
                        title = "Bar export", type = "barplot"), class = "rlispstat_plot")
  box_pdf <- tempfile(fileext = ".pdf")
  hist_png <- tempfile(fileext = ".png")
  scatter_svg <- tempfile(fileext = ".svg")
  bar_pdf <- tempfile(fileext = ".pdf")
  ls_export_boxplot(box, box_pdf)
  ls_export_histogram(hist, hist_png, format = "png")
  ls_export_plot(scatter, scatter_svg, format = "svg", theme = "minimal")
  ls_export_barplot(bar, bar_pdf)
  expect_true(file.exists(box_pdf))
  expect_true(file.exists(hist_png))
  expect_true(file.exists(scatter_svg))
  expect_true(file.exists(bar_pdf))
  expect_gt(file.info(box_pdf)$size, 0)
  expect_gt(file.info(hist_png)$size, 0)
  expect_gt(file.info(scatter_svg)$size, 0)
  expect_gt(file.info(bar_pdf)$size, 0)
})

test_that("coordinated native themes use the existing plot-theme registry and R export path", {
  expect_identical(LinkEDA:::.rls_plot_themes[[1L]], "publication")
  expect_equal(LinkEDA:::.rls_plot_theme_colors("publication")$point, "#0072B2")
  expect_true(all(c("manet", "vista", "beige", "datadesk", "garish") %in% LinkEDA:::.rls_plot_themes))
  expect_equal(LinkEDA:::.rls_plot_theme_colors("manet")$background, "#FFFCA3")
  expect_equal(LinkEDA:::.rls_plot_theme_colors("manet")$accent, "#66FF00")
  expect_equal(LinkEDA:::.rls_plot_theme_colors("vista")$point, "#344BC5")
  expect_equal(LinkEDA:::.rls_plot_theme_colors("vista")$accent, "#E3262E")
  expect_equal(LinkEDA:::.rls_plot_theme_colors("beige")$panel, "#FFFDF6")
  expect_equal(LinkEDA:::.rls_plot_theme_colors("datadesk")$point, "#050505")
  expect_equal(LinkEDA:::.rls_plot_theme_colors("datadesk")$panel, "#FFFFFF")
  expect_equal(LinkEDA:::.rls_plot_theme_colors("garish")$background, "#FF4FD8")
  expect_equal(LinkEDA:::.rls_plot_theme_colors("garish")$point, "#00D8FF")
  expect_equal(LinkEDA:::.rls_plot_theme_colors("garish")$accent, "#FF005C")

  old_theme <- ls_plot_theme()
  on.exit(ls_plot_theme(old_theme), add = TRUE)
  data <- data.frame(x = 1:5, y = c(2, 1, 4, 3, 5))
  id <- "theme_export_test"
  LinkEDA:::.rls_register_plot(id, "theme_export_group", "x", "y", data, seq_len(nrow(data)),
                                 type = "scatter", title = "Theme export")
  on.exit(LinkEDA:::.rls_unregister_plot(id), add = TRUE)
  plot <- structure(list(id = id, group = "theme_export_group", x = "x", y = "y",
                         title = "Theme export", type = "scatter"), class = "rlispstat_plot")
  exported_svg <- character()
  for (theme in c("manet", "vista", "beige", "datadesk", "garish")) {
    ls_plot_theme(theme)
    expect_equal(ls_plot_theme(), theme)
    path <- tempfile(fileext = ".svg")
    ls_export_plot(plot, path, format = "svg")
    expect_true(file.exists(path))
    expect_gt(file.info(path)$size, 0)
    svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
    rgb <- grDevices::col2rgb(LinkEDA:::.rls_plot_theme_colors(theme)$background)[, 1L] / 255 * 100
    components <- sub("\\.?0+$", "", sprintf("%.6f", rgb))
    expected_fill <- paste0("rgb(", paste0(components, "%", collapse = ", "), ")")
    # Base SVG serializes colours as rgb percentages, while svglite (preferred
    # for selectable text) writes the equivalent hexadecimal CSS colour.
    expected_hex <- tolower(LinkEDA:::.rls_plot_theme_colors(theme)$background)
    expect_true(grepl(expected_fill, svg, fixed = TRUE) ||
                grepl(expected_hex, tolower(svg), fixed = TRUE))
    exported_svg[[theme]] <- svg
  }
  expect_false(identical(exported_svg[["manet"]], exported_svg[["vista"]]))
  expect_false(identical(exported_svg[["vista"]], exported_svg[["beige"]]))
  expect_false(identical(exported_svg[["vista"]], exported_svg[["datadesk"]]))
  expect_false(identical(exported_svg[["datadesk"]], exported_svg[["garish"]]))
})

test_that("API validation leaves no native backend attached to later tests", {
  expect_silent(ls_close_all())
  LinkEDA:::.rls_reset_backend_connection(clear_views = TRUE)
  expect_false(LinkEDA:::.rls_state$process_started)
  expect_null(LinkEDA:::.rls_state$backend_kind)
})
