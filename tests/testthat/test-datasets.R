test_that("dataset registry tracks active datasets", {
  ls_register_dataset("cars_registry", mtcars)
  on.exit(ls_unregister_dataset("cars_registry"), add = TRUE)

  expect_equal(ls_active_dataset(), "cars_registry")
  datasets <- ls_datasets()
  expect_true("cars_registry" %in% datasets$name)
  expect_true(datasets$active[datasets$name == "cars_registry"])
})

test_that("LinkEDA launcher accepts optional data-frame-like input", {
  expect_identical(formals(LinkEDA), formals(rlispstat))
  expect_error(LinkEDA(data = 1:3), "data frame, tibble, tribble")
  expect_true("data" %in% names(formals(rlispstat)))
  expect_null(formals(rlispstat)$data)
  expect_error(rlispstat(data = 1:3), "data frame, tibble, tribble")
  expect_error(rlispstat(data = mtcars, name = ""), "single non-empty string")
})

test_that("LinkEDA preserves a supplied R object name", {
  registered_name <- NULL
  testthat::local_mocked_bindings(
    .rls_start_backend = function() invisible(TRUE),
    .rls_register_dataset = function(group, data, ...) {
      registered_name <<- group
      group
    },
    .package = "LinkEDA"
  )

  expect_identical(LinkEDA(mtcars), "mtcars")
  expect_identical(registered_name, "mtcars")
})

test_that("calling the empty launcher requests the Welcome Window", {
  commands <- list()
  testthat::local_mocked_bindings(
    .rls_start_backend = function() invisible(TRUE),
    .rls_send = function(lines, expect_reply = TRUE) {
      commands[[length(commands) + 1L]] <<- lines
      "OK"
    },
    .package = "LinkEDA"
  )

  expect_null(rlispstat())
  expect_equal(commands[[1L]][[1L]], "WELCOME_LAUNCH")
  expect_equal(commands[[1L]][[2L]], "from_existing_r")
  expect_equal(commands[[1L]][[4L]], "none")
})

test_that("LinkEDA opens a supplied data-file path directly", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("x,y", "1,2"), path)
  imported <- NULL
  commands <- list()
  testthat::local_mocked_bindings(
    .rls_start_backend = function() invisible(TRUE),
    ls_import_data = function(path, name = NULL, ...) {
      imported <<- path
      "direct_file"
    },
    .rls_send = function(lines, expect_reply = TRUE) {
      commands[[length(commands) + 1L]] <<- lines
      "OK"
    },
    .package = "LinkEDA"
  )
  expect_identical(LinkEDA(path), "direct_file")
  expect_equal(imported, normalizePath(path))
  expect_equal(commands, list(c("WELCOME_RECENT_NOTE", normalizePath(path))))
})

test_that("Welcome data-frame browsing reports dimensions and class from the calling R session", {
  object_name <- "linkeda_welcome_browse_fixture"
  assign(object_name, structure(data.frame(x = 1:2), class = c("tbl_df", "tbl", "data.frame")),
         envir = .GlobalEnv)
  on.exit(rm(list = object_name, envir = .GlobalEnv), add = TRUE)
  commands <- list()
  testthat::local_mocked_bindings(
    .rls_send = function(lines, expect_reply = TRUE) {
      commands[[length(commands) + 1L]] <<- lines
      "OK"
    },
    .package = "LinkEDA"
  )
  expect_true(LinkEDA:::.rls_handle_r_data_browse_needed(c("R_DATA_BROWSE_NEEDED", "request-1")))
  payload <- commands[[1L]]
  index <- match(object_name, payload)
  expect_equal(payload[[1L]], "OPEN_R_DATA_CHOOSER_V2")
  expect_equal(payload[index + 1:3], c("2", "1", "tbl_df, tbl, data.frame"))
})

test_that("selected analysis scopes require and transmit a stable user name", {
  sent <- NULL
  testthat::local_mocked_bindings(
    .rls_resolve_group = function(group) "cars",
    .rls_send = function(lines, expect_reply = TRUE) {
      sent <<- lines
      "OK"
    },
    .package = "LinkEDA"
  )
  expect_error(ls_use_selected_as_analysis_scope("cars"), "name")
  expect_error(ls_use_selected_as_analysis_scope("cars", "   "), "name")
  expect_error(ls_use_selected_as_analysis_scope("cars", "bad|name"), "cannot contain")
  expect_error(ls_use_selected_as_analysis_scope("cars", "bad\nname"), "cannot contain")
  expect_true(ls_use_selected_as_analysis_scope("cars", "High mileage cars"))
  expect_equal(sent, c("SET_ANALYSIS_SCOPE_FROM_SELECTION", "cars",
                       "Selection: High mileage cars", "R"))
})

test_that("saved analysis scopes can be listed, reused, and added", {
  sent <- list()
  testthat::local_mocked_bindings(
    .rls_resolve_group = function(group) "cars",
    .rls_send = function(lines, expect_reply = TRUE) {
      sent[[length(sent) + 1L]] <<- lines
      if (lines[[1L]] == "GET_SAVED_ANALYSIS_SCOPES") {
        return("OK\t2|High mileage cars|2|1|3|Odd cars|3|1|3|5")
      }
      "OK"
    },
    .package = "LinkEDA"
  )
  saved <- ls_saved_selections("cars")
  expect_equal(saved$name, c("High mileage cars", "Odd cars"))
  expect_equal(saved$n, c(2L, 3L))
  expect_equal(saved$rows[[1L]], c(1L, 3L))
  expect_true(ls_use_saved_analysis_scope("cars", "Odd cars"))
  expect_true(ls_add_saved_analysis_scope(
    "cars", "High mileage cars", "Mileage or odd"))
  expect_equal(sent[[2L]], c("USE_SAVED_ANALYSIS_SCOPE", "cars", "Odd cars"))
  expect_equal(sent[[3L]], c(
    "ADD_SAVED_ANALYSIS_SCOPE", "cars", "High mileage cars", "Mileage or odd"))
})

test_that("the Alien Welcome example is loaded in the connected R session", {
  registered <- NULL
  replies <- list()
  testthat::local_mocked_bindings(
    .rls_register_dataset = function(name, data, ...) {
      registered <<- list(name = name, data = data)
      name
    },
    .rls_send = function(lines, expect_reply = TRUE) {
      replies[[length(replies) + 1L]] <<- lines
      "OK"
    },
    .package = "LinkEDA"
  )
  expect_true(LinkEDA:::.rls_handle_welcome_action_needed(
    c("WELCOME_ACTION_NEEDED", "example-1", "example", "Alien")))
  expect_equal(registered$name, "Alien")
  expect_s3_class(registered$data, "data.frame")
  expect_true(all(c("humans_eaten", "size", "eggs", "happy") %in%
                  names(registered$data)))
  expect_equal(replies[[1L]][1:4],
               c("WELCOME_ACTION_RESULT", "example-1", "ok", "example"))
})

test_that("active dataset can be changed explicitly", {
  ls_register_dataset("cars_a", mtcars)
  ls_register_dataset("cars_b", mtcars[1:5, ])
  on.exit(ls_unregister_dataset("cars_a"), add = TRUE)
  on.exit(ls_unregister_dataset("cars_b"), add = TRUE)

  ls_set_active_dataset("cars_a")
  expect_equal(ls_active_dataset(), "cars_a")
  expect_error(ls_set_active_dataset("missing_dataset"), "not registered")
})

test_that("refreshing R data frames registers data frames without duplicating names", {
  env <- new.env(parent = emptyenv())
  env$d <- data.frame(x = 1:3, y = 4:6)
  env$not_data <- 1:3
  on.exit(ls_unregister_dataset("d"), add = TRUE)

  out <- ls_refresh_r_dataframes(env)
  expect_true("d" %in% out$name)
  out2 <- ls_refresh_r_dataframes(env)
  expect_equal(sum(out2$name == "d"), 1L)
})

test_that("manual dataset registration avoids duplicate names", {
  n1 <- ls_register_dataset("dup_data", mtcars)
  n2 <- ls_register_dataset("dup_data", mtcars[1:3, ])
  on.exit(ls_unregister_dataset(n1), add = TRUE)
  on.exit(ls_unregister_dataset(n2), add = TRUE)
  expect_equal(n1, "dup_data")
  expect_equal(n2, "dup_data 2")
})

test_that("CSV import registers and activates a dataset with row state", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("x,y", "1,2", "3,4"), path)
  name <- ls_import_csv(path, name = "csv_data")
  on.exit(ls_unregister_dataset(name), add = TRUE)
  expect_equal(ls_active_dataset(), name)
  expect_true(name %in% ls_datasets()$name)
  record <- LinkEDA:::.rls_dataset_record(name)
  expect_equal(record$original_row_ids, 1:2)
  expect_length(record$selection_state, 0L)
  expect_length(record$row_colors, 0L)
  expect_equal(record$variable_metadata$missing_count, c(0L, 0L))
})

test_that("make_active = FALSE preserves the previous active dataset", {
  base <- ls_register_dataset("active_base", data.frame(x = 1:3, y = 4:6))
  path <- tempfile(fileext = ".csv")
  writeLines(c("x,y", "10,20", "30,40"), path)
  imported <- ls_import_csv(path, name = "inactive_import", make_active = FALSE)
  on.exit(ls_unregister_dataset(base), add = TRUE)
  on.exit(ls_unregister_dataset(imported), add = TRUE)

  expect_equal(ls_active_dataset(), base)
  expect_true(imported %in% ls_datasets()$name)
  expect_equal(nrow(ls_get_active_dataset()), 3L)
})

test_that("TSV, TXT, RDS, and RData imports register data frames", {
  tsv <- tempfile(fileext = ".tsv")
  txt <- tempfile(fileext = ".txt")
  rds <- tempfile(fileext = ".rds")
  rdata <- tempfile(fileext = ".RData")
  writeLines(c("x\ty", "1\t2", "3\tNA"), tsv)
  writeLines(c("a\tb", "4\t5", "6\t7"), txt)
  saveRDS(data.frame(z = c(1, NA), g = c("a", "b")), rds)
  rdata_small <- data.frame(a = 1)
  rdata_big <- data.frame(x = 1:3, y = c("a", "b", "c"))
  save(rdata_small, rdata_big, file = rdata)
  n1 <- ls_import_data(tsv, name = "tsv data")
  n2 <- ls_import_data(txt, name = "txt data")
  n3 <- ls_import_rds(rds, name = "rds data")
  n4 <- ls_import_rdata(rdata, name = "rdata data")
  on.exit(ls_unregister_dataset(n1), add = TRUE)
  on.exit(ls_unregister_dataset(n2), add = TRUE)
  on.exit(ls_unregister_dataset(n3), add = TRUE)
  on.exit(ls_unregister_dataset(n4), add = TRUE)
  expect_true(all(c(n1, n2, n3, n4) %in% ls_datasets()$name))
  expect_equal(LinkEDA:::.rls_dataset_record(n1)$variable_metadata$missing_count[[2L]], 1L)
  expect_equal(nrow(LinkEDA:::.rls_dataset_record(n4)$data), 3L)
})

test_that("Stata and SAS transport imports register data frames when haven is available", {
  skip_if_not_installed("haven")
  dta <- tempfile(fileext = ".dta")
  xpt <- tempfile(fileext = ".xpt")
  data <- data.frame(x = 1:2, y = c("a", "b"))
  haven::write_dta(data, dta)
  haven::write_xpt(data, xpt)
  n1 <- ls_import_data(dta, name = "stata data")
  n2 <- ls_import_data(xpt, name = "sas transport data")
  on.exit(ls_unregister_dataset(n1), add = TRUE)
  on.exit(ls_unregister_dataset(n2), add = TRUE)
  expect_true(all(c(n1, n2) %in% ls_datasets()$name))
  expect_equal(nrow(LinkEDA:::.rls_dataset_record(n1)$data), 2L)
  expect_equal(nrow(LinkEDA:::.rls_dataset_record(n2)$data), 2L)
})

test_that("SPSS SAV import uses haven and preserves labelled metadata", {
  skip_if_not_installed("haven")
  sav <- tempfile(fileext = ".sav")
  labelled <- haven::labelled(c(1, 2, 1), labels = c(No = 1, Yes = 2))
  attr(labelled, "label") <- "Choice"
  haven::write_sav(data.frame(choice = labelled, score = c(2.5, 3.0, 4.5)), sav)
  imported <- ls_import_data(sav, name = "spss data")
  on.exit(ls_unregister_dataset(imported), add = TRUE)

  record <- LinkEDA:::.rls_dataset_record(imported)
  expect_equal(nrow(record$data), 3L)
  expect_equal(record$variable_metadata$label[record$variable_metadata$name == "choice"], "Choice")
  expect_equal(unname(record$variable_metadata$value_labels[[1L]]), c(1, 2))
})

test_that("staged cloud SPSS imports keep their original identity and clean up", {
  skip_if_not_installed("haven")
  platform_tmp <- Sys.getenv("TMPDIR", unset = dirname(tempdir()))
  staging <- tempfile("rlispstat-import-", tmpdir = platform_tmp)
  dir.create(staging)
  staged_sav <- file.path(staging, "worksat_47719193.sav")
  haven::write_sav(data.frame(score = c(2.5, 3.0), group = c(1, 2)), staged_sav)
  original <- file.path(tempdir(), "original", "worksat_47719193.sav")
  replies <- list()
  testthat::local_mocked_bindings(
    .rls_send = function(lines, expect_reply = TRUE) {
      replies[[length(replies) + 1L]] <<- lines
      invisible("OK")
    },
    .package = "LinkEDA"
  )

  LinkEDA:::.rls_handle_import_data_needed(staged_sav, original, TRUE)
  on.exit(if ("worksat_47719193" %in% ls_datasets()$name) {
    ls_unregister_dataset("worksat_47719193")
  }, add = TRUE)

  expect_false(dir.exists(staging))
  expect_equal(LinkEDA:::.rls_dataset_record("worksat_47719193")$path,
               normalizePath(original, mustWork = FALSE))
  expect_equal(replies[[1L]][1:2], c("IMPORT_DATA_RESULT", "ok"))
})

test_that("import errors are explicit", {
  missing <- tempfile(fileext = ".csv")
  expect_error(ls_import_data(missing), "does not exist")
  unsupported <- tempfile(fileext = ".json")
  writeLines("{}", unsupported)
  expect_error(ls_import_data(unsupported), "Unsupported data file")
  not_df <- tempfile(fileext = ".rds")
  saveRDS(1:3, not_df)
  expect_error(ls_import_rds(not_df), "did not contain a data frame")
  empty_rdata <- tempfile(fileext = ".RData")
  not_a_data_frame <- 1:3
  save(not_a_data_frame, file = empty_rdata)
  expect_error(ls_import_rdata(empty_rdata), "did not contain a data frame")
})

test_that("row color registry stores and clears original row colors", {
  name <- ls_register_dataset("row_color_data", mtcars)
  on.exit(ls_unregister_dataset(name), add = TRUE)
  ls_set_row_color(name, c(1, 3), "purple")
  colors <- ls_get_row_colors(name)
  expect_equal(unname(colors[c("1", "3")]), c("purple", "purple"))
  ls_clear_row_color(name, rows = 1)
  expect_false("1" %in% names(ls_get_row_colors(name)))
  ls_clear_row_color(name)
  expect_length(ls_get_row_colors(name), 0L)
})

test_that("variable analysis types can be changed safely", {
  data <- data.frame(
    x = c(1, 2, 3, NA),
    y = c(4, 5, 6, 7),
    safe_text = c("1", "2", NA, "4"),
    bad_text = c("1", "oops", "3", "4"),
    stringsAsFactors = FALSE
  )
  name <- ls_register_dataset("type_change_data", data)
  on.exit(ls_unregister_dataset(name), add = TRUE)

  metadata <- ls_variable_metadata(name)
  expect_true(all(c(
    "variable_name", "display_name", "original_class", "current_analysis_type",
    "is_numeric", "is_factor", "labels", "value_labels", "missing_count"
  ) %in% names(metadata)))

  ls_set_variable_type(name, "x", "factor")
  x_meta <- ls_variable_metadata(name, "x")
  expect_equal(x_meta$current_analysis_type, "factor")
  expect_true(x_meta$is_factor)
  expect_false("x" %in% LinkEDA:::.rls_numeric_variable_names(
    LinkEDA:::.rls_dataset_record(name)$data,
    LinkEDA:::.rls_dataset_record(name)$variable_metadata
  ))

  ls_set_variable_type(name, "safe_text", "numeric")
  safe_meta <- ls_variable_metadata(name, "safe_text")
  expect_equal(safe_meta$current_analysis_type, "numeric")
  expect_true(safe_meta$is_numeric)
  expect_type(LinkEDA:::.rls_dataset_record(name)$data$safe_text, "double")

  expect_error(
    ls_set_variable_type(name, "bad_text", "numeric"),
    "cannot be treated as numeric"
  )
})

test_that("variable type changes update numeric-only correlation state", {
  name <- ls_register_dataset("type_corr_data", data.frame(x = 1:5, y = 2:6, z = letters[1:5]))
  on.exit(ls_unregister_dataset(name), add = TRUE)

  cm <- ls_new_correlation_matrix(data = name, variables = c("x", "y"), native = FALSE)
  expect_equal(ls_correlation_matrix_state(cm)$variables, c("x", "y"))
  ls_set_variable_type(name, "y", "factor")
  expect_equal(ls_correlation_matrix_state(cm)$variables, "x")
})

test_that("visible GLM, diagnostic, variable type, and correlation paths avoid generic placeholders", {
  test_dir <- testthat::test_path()
  candidates <- normalizePath(c(
    getwd(), file.path(getwd(), ".."), file.path(getwd(), "..", ".."),
    file.path(getwd(), "rlispstat"),
    file.path(test_dir, "..", ".."),
    file.path(test_dir, "..", "..", "rlispstat"),
    file.path(test_dir, "..", "..", "LinkEDA"),
    file.path(test_dir, "..", "..", "00_pkg_src", "rlispstat"),
    file.path(test_dir, "..", "..", "00_pkg_src", "LinkEDA")
  ), mustWork = FALSE)
  roots <- candidates[
    file.exists(file.path(candidates, "DESCRIPTION")) &
      file.exists(file.path(candidates, "src", "native", "backend_macos.mm"))
  ]
  expect_true(length(roots) > 0L)
  root <- roots[[1L]]
  files <- c(
    file.path(root, "src/native/backend_macos.mm"),
    list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)
  )
  text <- paste(unlist(lapply(files, readLines, warn = FALSE)), collapse = "\n")
  expect_false(grepl(
    "Generalized Linear Model not implemented|Diagnostics not implemented|Variable type conversion not implemented|Correlation matrix not implemented|not implemented yet|planned for|show a placeholder|placeholder window|placeholder message",
    text,
    ignore.case = TRUE
  ))
})

test_that("new scatterplot defaults to the active dataset and preserves row ids", {
  data <- data.frame(x = c(1, NA, 3), y = c(4, 5, 6), z = c(7, 8, 9))
  ls_register_dataset("linked_rows", data)
  on.exit(ls_unregister_dataset("linked_rows"), add = TRUE)
  old_launch <- getOption("LinkEDA.launch")
  options(LinkEDA.launch = FALSE)
  on.exit(options(LinkEDA.launch = old_launch), add = TRUE)

  expect_error(ls_new_scatterplot(x = "x", y = "y"), "launching is disabled")
  prepared <- LinkEDA:::`.rls_prepare_scatter_data`(data, "x", "y", "linked_rows")
  expect_equal(prepared$row, c(1L, 3L))
})
