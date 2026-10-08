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

test_that("using and saving the current selection are separate operations", {
  sent <- NULL
  testthat::local_mocked_bindings(
    .rls_resolve_group = function(group) "cars",
    .rls_send = function(lines, expect_reply = TRUE) {
      sent <<- lines
      "OK"
    },
    .package = "LinkEDA"
  )
  expect_true(ls_use_selected_as_analysis_scope("cars"))
  expect_equal(sent, c("SET_ANALYSIS_SCOPE_FROM_SELECTION", "cars",
                       "Current selection", "R"))
  expect_error(ls_save_analysis_scope("cars", "   "), "name")
  expect_error(ls_save_analysis_scope("cars", "bad|name"), "cannot contain")
  expect_error(ls_save_analysis_scope("cars", "bad\nname"), "cannot contain")
  expect_true(ls_save_analysis_scope("cars", "High mileage cars"))
  expect_equal(sent, c("SAVE_ANALYSIS_SCOPE_FROM_SELECTION", "cars",
                       "High mileage cars", "R"))
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
      registered <<- list(name = name, data = data, options = list(...))
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
  expect_false(registered$options$infer_imported_types)
  metadata <- LinkEDA:::.rls_variable_metadata(
    registered$data, infer_imported_types = registered$options$infer_imported_types
  )
  expect_equal(LinkEDA:::.rls_metadata_type(metadata, "planet"), "factor")
  expect_equal(LinkEDA:::.rls_metadata_type(metadata, "happy"), "factor")
  expect_equal(LinkEDA:::.rls_metadata_levels(metadata, "planet"),
               c("Aurelia", "Borealis", "Cygnus"))
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

test_that("file imports clean variable names with janitor and retain the originals", {
  path <- tempfile(fileext = ".csv")
  writeLines(c(
    "Participant ID,Score Before,Score Before,Mood (final)",
    "A001,10,11,high",
    "A002,12,13,low"
  ), path, useBytes = TRUE)
  imported <- ls_import_csv(path, name = "janitor names", make_active = FALSE)
  on.exit(ls_unregister_dataset(imported), add = TRUE)

  record <- LinkEDA:::.rls_dataset_record(imported)
  expect_identical(
    names(record$data),
    c("participant_id", "score_before", "score_before_2", "mood_final")
  )
  expect_identical(
    record$import_name_map$original_name,
    c("Participant ID", "Score Before", "Score Before", "Mood (final)")
  )
  expect_identical(
    record$variable_metadata$original_name,
    record$import_name_map$original_name
  )
})

test_that("mixed CSV import preserves types, empty cells, and missing values", {
  path <- tempfile(fileext = ".csv")
  writeLines(c(
    "number,category,text,partly_missing,blank_text",
    "1,a,alpha,10,",
    "2,b,beta,,kept",
    ",a,gamma,30,",
    "4,,delta,NA,last"
  ), path)
  imported <- ls_import_data(path, name = "mixed missing csv")
  on.exit(ls_unregister_dataset(imported), add = TRUE)

  record <- LinkEDA:::.rls_dataset_record(imported)
  expect_equal(nrow(record$data), 4L)
  expect_equal(ncol(record$data), 5L)
  expect_equal(record$data$number, c(1, 2, NA, 4))
  expect_equal(record$data$partly_missing, c(10, NA, 30, NA))
  expect_equal(record$data$text, c("alpha", "beta", "gamma", "delta"))
  expect_true(is.na(record$data$category[[4L]]))
  expect_true(is.na(record$data$blank_text[[1L]]))
  expect_equal(record$data$blank_text[[2L]], "kept")
  expect_equal(
    record$variable_metadata$missing_count,
    c(1L, 1L, 0L, 2L, 2L)
  )
})

test_that("Excel import preserves mixed values and missing cells", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  source <- data.frame(
    number = c(1, NA, 3),
    category = c("a", "b", NA),
    text = c("alpha", NA, "gamma"),
    stringsAsFactors = FALSE
  )
  writexl::write_xlsx(source, path)
  imported <- ls_import_data(path, name = "mixed missing excel")
  on.exit(ls_unregister_dataset(imported), add = TRUE)

  record <- LinkEDA:::.rls_dataset_record(imported)
  expect_equal(record$data$number, source$number)
  expect_equal(record$data$category, source$category)
  expect_equal(record$data$text, source$text)
  expect_equal(record$variable_metadata$missing_count, c(1L, 1L, 1L))
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
  expect_equal(record$variable_metadata$description[record$variable_metadata$name == "choice"], "Choice")
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

  expect_true(dir.exists(staging))
  expect_equal(replies[[1L]][1:2], c("IMPORT_DATA_PREVIEW", "worksat_47719193"))
  expect_setequal(tail(replies[[1L]], 2L), c("score", "group"))

  LinkEDA:::.rls_handle_import_data_commit_needed(c(
    "IMPORT_DATA_COMMIT_NEEDED", "worksat_47719193", "0", "1", "score"
  ))
  expect_false(dir.exists(staging))
  expect_equal(LinkEDA:::.rls_dataset_record("worksat_47719193")$path,
               normalizePath(original, mustWork = FALSE))
  expect_equal(names(LinkEDA:::.rls_dataset_record("worksat_47719193")$data), "score")
  expect_equal(replies[[2L]][1:2], c("IMPORT_DATA_RESULT", "ok"))
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
  corrupt_rds <- tempfile(fileext = ".rds")
  writeBin(charToRaw("not an RDS stream"), corrupt_rds)
  expect_error(ls_import_rds(corrupt_rds), "Cannot import RDS file")
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
    "semantic_type", "storage_class", "category_order", "is_binary",
    "numeric_mapping", "type_change_provenance", "is_numeric", "is_factor",
    "labels", "value_labels", "missing_count"
  ) %in% names(metadata)))

  ls_set_variable_type(name, "x", "factor")
  x_meta <- ls_variable_metadata(name, "x")
  expect_equal(x_meta$current_analysis_type, "factor")
  expect_true(x_meta$is_factor)
  expect_false("x" %in% LinkEDA:::.rls_numeric_variable_names(
    LinkEDA:::.rls_dataset_record(name)$data,
    LinkEDA:::.rls_dataset_record(name)$variable_metadata
  ))

  factor_levels <- LinkEDA:::.rls_metadata_levels(
    LinkEDA:::.rls_dataset_record(name)$variable_metadata, "x"
  )
  ls_set_variable_type(name, "x", "character")
  x_text_meta <- ls_variable_metadata(name, "x")
  expect_equal(x_text_meta$current_analysis_type, "character")
  expect_false(x_text_meta$is_factor)
  expect_equal(LinkEDA:::.rls_metadata_levels(
    LinkEDA:::.rls_dataset_record(name)$variable_metadata, "x"
  ), factor_levels)
  ls_set_variable_type(name, "x", "factor")
  expect_equal(levels(LinkEDA:::.rls_dataset_record(name)$data$x), factor_levels)

  ls_set_variable_type(name, "x", "ordered")
  x_ordered_meta <- ls_variable_metadata(name, "x")
  expect_equal(x_ordered_meta$current_analysis_type, "ordered")
  expect_true(x_ordered_meta$is_factor)
  expect_true(is.ordered(LinkEDA:::.rls_dataset_record(name)$data$x))

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

test_that("logical storage is exposed as binary Categorical metadata", {
  data <- data.frame(answer = c(TRUE, FALSE, NA))
  metadata <- LinkEDA:::.rls_variable_metadata(data)
  expect_identical(metadata$current_analysis_type, "factor")
  expect_identical(metadata$semantic_type, "factor")
  expect_identical(metadata$storage_class, "logical")
  expect_true(metadata$is_logical)
  expect_true(metadata$is_factor)
  expect_true(metadata$is_binary)
  expect_identical(metadata$category_order[[1L]], c("FALSE", "TRUE"))
  payload <- LinkEDA:::.rls_dataframe_payload(data, metadata)
  marker <- match("DATATYPEMETA", payload)
  expect_false(is.na(marker))
  expect_identical(payload[[marker + 3L]], "factor")
  expect_identical(payload[[marker + 4L]], "logical")
  expect_identical(payload[[marker + 5L]], "1")
})

test_that("Categorical numeric conversion is explicit, deterministic, and reversible", {
  name <- ls_register_dataset("categorical_numeric_mapping", data.frame(
    binary = factor(c("No", "Yes", NA, "No"), levels = c("No", "Yes")),
    region = factor(c("North", "South", "East", "North"),
                    levels = c("North", "South", "East")),
    numeric_labels = factor(c("10", "20", "10", "20"), levels = c("10", "20"))
  ))
  on.exit(ls_unregister_dataset(name), add = TRUE)

  expect_warning(ls_set_variable_type(name, "binary", "numeric"), "No = 0; Yes = 1")
  expect_equal(LinkEDA:::.rls_dataset_record(name)$data$binary, c(0, 1, NA, 0))
  mapping <- ls_variable_metadata(name, "binary")$numeric_mapping[[1L]]
  expect_equal(unname(mapping), c(0, 1))
  expect_identical(names(mapping), c("No", "Yes"))
  ls_set_variable_type(name, "binary", "factor")
  restored <- LinkEDA:::.rls_dataset_record(name)$data$binary
  expect_identical(levels(restored), c("No", "Yes"))
  expect_identical(as.character(restored), c("No", "Yes", NA, "No"))

  expect_error(ls_set_variable_type(name, "region", "numeric"), "explicit named numeric `mapping`")
  ls_set_variable_type(name, "region", "numeric",
                       mapping = c(North = 10, South = 20, East = 30))
  expect_equal(LinkEDA:::.rls_dataset_record(name)$data$region, c(10, 20, 30, 10))
  ls_set_variable_type(name, "region", "factor")
  expect_identical(as.character(LinkEDA:::.rls_dataset_record(name)$data$region),
                   c("North", "South", "East", "North"))

  ls_set_variable_type(name, "numeric_labels", "numeric")
  expect_equal(LinkEDA:::.rls_dataset_record(name)$data$numeric_labels, c(10, 20, 10, 20))
})

test_that("binary mapping can be inverted and ordinals accept arbitrary level counts", {
  name <- ls_register_dataset("ordinal_mapping", data.frame(
    binary = factor(c("First", "Second", "Second"), levels = c("First", "Second")),
    rating = ordered(c("L1", "L7", "L12"), levels = paste0("L", 1:12)),
    score = c(0, 5, 10)
  ))
  on.exit(ls_unregister_dataset(name), add = TRUE)

  expect_warning(ls_set_variable_type(name, "binary", "numeric", invert_binary = TRUE),
                 "First = 1; Second = 0")
  expect_equal(LinkEDA:::.rls_dataset_record(name)$data$binary, c(1, 0, 0))
  expect_warning(ls_set_variable_type(name, "rating", "numeric"), "equally spaced")
  expect_equal(LinkEDA:::.rls_dataset_record(name)$data$rating, c(1, 7, 12))
  ls_set_variable_type(name, "score", "ordered",
                       breaks = c(-Inf, 2.5, 7.5, Inf),
                       labels = c("Low", "Middle", "High"))
  converted <- LinkEDA:::.rls_dataset_record(name)$data$score
  expect_true(is.ordered(converted))
  expect_identical(levels(converted), c("Low", "Middle", "High"))
  record <- LinkEDA:::.rls_dataset_record(name)
  score_history <- ls_variable_metadata(name, "score")$type_change_provenance[[1L]]
  score_step <- score_history[[length(score_history)]]
  expect_identical(score_step$breaks, c(-Inf, 2.5, 7.5, Inf))
  expect_identical(score_step$labels, c("Low", "Middle", "High"))
  provenance_step <- record$data_provenance$history[[length(record$data_provenance$history)]]
  expect_match(provenance_step$r_code, "cut\\(data\\[\\[\"score\"\\]\\]", perl = TRUE)
})

test_that("mostly numeric imported text can be converted with an explicit warning", {
  age <- as.character(rep(11:18, length.out = 270L))
  age[c(15L, 80L)] <- c("17,5", "15,")
  age[c(20L, 100L, 180L, 260L)] <-
    c("18 vuotias", "11.V", "11-vuotta", "14 vuotta")
  name <- ls_register_dataset("mostly_numeric_type_change", data.frame(age = age))
  on.exit(ls_unregister_dataset(name), add = TRUE)

  expect_warning(
    ls_set_variable_type(name, "age", "numeric"),
    "4 non-numeric values were set to missing"
  )
  converted <- LinkEDA:::.rls_dataset_record(name)$data$age
  expect_type(converted, "double")
  expect_equal(sum(is.na(converted)), 4L)
  expect_true(all(c(17.5, 15) %in% converted))
  expect_equal(ls_variable_metadata(name, "age")$current_analysis_type, "numeric")
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
