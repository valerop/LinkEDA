test_that("native data payloads are merged while preserving R types", {
  original <- data.frame(
    score = c(1.5, 2.5),
    group = factor(c("a", "b"), levels = c("a", "b")),
    when = as.Date(c("2026-01-01", "2026-01-02")),
    keep = c(TRUE, FALSE),
    check.names = FALSE
  )
  attr(original$score, "label") <- "Score label"
  payload <- list(
    rows = 2L,
    columns = list(
      score = c("9.25", "2.5"),
      group = c("c", "b"),
      when = c("2026-02-01", "2026-01-02"),
      keep = c("FALSE", "TRUE")
    ),
    types = c("numeric", "factor", "datetime", "logical")
  )
  result <- LinkEDA:::.rls_merge_native_data(payload, original)
  expect_equal(as.double(result$score), c(9.25, 2.5))
  expect_identical(attr(result$score, "label"), "Score label")
  expect_true(is.factor(result$group))
  expect_equal(levels(result$group), c("a", "b", "c"))
  expect_s3_class(result$when, "Date")
  expect_equal(result$when[[1L]], as.Date("2026-02-01"))
  expect_identical(result$keep, c(FALSE, TRUE))
})

test_that("native factor levels replace stale R levels after rapid type changes", {
  original <- data.frame(
    gender = factor(c("Female", "Male"), levels = c("Female", "Male"))
  )
  payload <- list(
    rows = 2L,
    columns = list(gender = c("0", "1")),
    types = c("factor"),
    factor_levels = list(gender = c("0", "1"))
  )

  result <- LinkEDA:::.rls_merge_native_data(payload, original)
  expect_identical(levels(result$gender), c("0", "1"))
  expect_identical(as.character(result$gender), c("0", "1"))
})

test_that("native multiple-imputation rebuild uses one authoritative factor coding", {
  payload <- list(
    rows = 2L,
    columns = list(gender = c("0", "1")),
    types = c("factor"),
    factor_levels = list(gender = c("0", "1")),
    imputation = list(
      dataset_type = "multiple_imputation",
      imputation_id = "gender-mi",
      source_dataset_id = "source",
      count = 2L,
      active_version = 1L,
      display_mode = "version",
      sparse = list(list(
        name = "gender", rows = 2L, original = "NA",
        versions = list("1", "0")
      ))
    )
  )
  stale <- data.frame(
    gender = factor(c("Female", "Male"), levels = c("Female", "Male"))
  )
  current <- LinkEDA:::.rls_merge_native_data(payload, stale)
  rebuilt <- LinkEDA:::.rls_rebuild_native_imputation(payload, current)

  expect_identical(levels(rebuilt$data$gender), c("0", "1"))
  expect_identical(as.character(rebuilt$completed_datasets[[1L]]$gender), c("0", "1"))
  expect_identical(as.character(rebuilt$completed_datasets[[2L]]$gender), c("0", "0"))
  expect_false(any(c("Female", "Male") %in% levels(rebuilt$data$gender)))
})

test_that("return handler supports selected rows and zero selected rows", {
  state <- LinkEDA:::.rls_state
  request_id <- paste0("test-return-", sample.int(1e8, 1L))
  original_path <- tempfile(fileext = ".rds")
  saveRDS(data.frame(x = 1:3, g = factor(c("a", "b", "a"))), original_path)
  return_dir <- file.path(tempdir(), paste0("rlispstat-return-", request_id))
  dir.create(return_dir)
  payload_path <- file.path(return_dir, "data.payload")
  writeLines(c("DATASET", "test", "3", "2", "x", "numeric", "10", "20", "30",
               "g", "factor", "a", "b", "c"), payload_path)
  assign(request_id, list(original_path = original_path),
         envir = state$r_data_return_requests)
  on.exit({
    unlink(original_path)
    if (exists(request_id, envir = state$r_data_return_requests, inherits = FALSE))
      rm(list = request_id, envir = state$r_data_return_requests)
    if (exists(request_id, envir = state$r_data_return_results, inherits = FALSE))
      rm(list = request_id, envir = state$r_data_return_results)
  }, add = TRUE)

  parts <- c("R_DATA_RETURN_NEEDED", request_id, "test", payload_path,
             "selected", "1", "2")
  expect_true(LinkEDA:::.rls_handle_r_data_return_needed(parts))
  result <- get(request_id, envir = state$r_data_return_results)
  expect_equal(result$data$x, 20)
  expect_equal(as.character(result$data$g), "b")

  rm(list = request_id, envir = state$r_data_return_results)
  assign(request_id, list(original_path = original_path),
         envir = state$r_data_return_requests)
  return_dir <- file.path(tempdir(), paste0("rlispstat-return-", request_id, "-empty"))
  dir.create(return_dir)
  payload_path <- file.path(return_dir, "data.payload")
  writeLines(c("DATASET", "test", "3", "1", "x", "numeric", "10", "20", "30"),
             payload_path)
  parts <- c("R_DATA_RETURN_NEEDED", request_id, "test", payload_path,
             "selected", "0")
  expect_true(LinkEDA:::.rls_handle_r_data_return_needed(parts))
  result <- get(request_id, envir = state$r_data_return_results)
  expect_equal(nrow(result$data), 0L)
})

test_that("edit-data entry points are exported and validate locally", {
  expect_true(all(c("edit_data", "choose_data") %in% getNamespaceExports("LinkEDA")))
  expect_error(edit_data(1), "data frame")
  expect_error(choose_data(new.env(parent = emptyenv())), "No data frames")
})

test_that("File-menu return assigns native data into the connected R session", {
  state <- LinkEDA:::.rls_state
  token <- paste0(sample.int(1e8, 1L))
  group <- paste0("menu_group_", token)
  object_name <- paste0("menu_result_", token)
  original <- data.frame(
    score = c(1, 2, 3),
    category = factor(c("a", "b", "a")),
    check.names = FALSE
  )
  LinkEDA:::.rls_register_dataset(group, original, activate = FALSE, replace = TRUE)
  return_dir <- file.path(tempdir(), paste0("rlispstat-return-menu-", token))
  dir.create(return_dir)
  payload_path <- file.path(return_dir, "data.payload")
  writeLines(c("DATASET", group, "3", "2", "score", "numeric", "10", "20", "30",
               "category", "factor", "a", "b", "c"), payload_path)
  on.exit({
    if (exists(group, envir = state$datasets, inherits = FALSE))
      rm(list = group, envir = state$datasets)
    if (exists(object_name, envir = .GlobalEnv, inherits = FALSE))
      rm(list = object_name, envir = .GlobalEnv)
  }, add = TRUE)

  parts <- c("R_DATA_ASSIGN_NEEDED", "assign-test", group, payload_path,
             object_name, "selected", "0", "1", "2")
  expect_true(LinkEDA:::.rls_handle_r_data_assign_needed(parts))
  result <- get(object_name, envir = .GlobalEnv, inherits = FALSE)
  expect_equal(result$score, 20)
  expect_true(is.factor(result$category))
  expect_equal(as.character(result$category), "b")
})

test_that("File-menu return does not replace an R object without permission", {
  token <- paste0(sample.int(1e8, 1L))
  object_name <- paste0("menu_existing_", token)
  assign(object_name, "keep", envir = .GlobalEnv)
  return_dir <- file.path(tempdir(), paste0("rlispstat-return-existing-", token))
  dir.create(return_dir)
  payload_path <- file.path(return_dir, "data.payload")
  writeLines(c("DATASET", "menu", "1", "1", "x", "numeric", "4"), payload_path)
  on.exit({
    if (exists(object_name, envir = .GlobalEnv, inherits = FALSE))
      rm(list = object_name, envir = .GlobalEnv)
  }, add = TRUE)

  parts <- c("R_DATA_ASSIGN_NEEDED", "assign-existing", "menu", payload_path,
             object_name, "all", "0", "0")
  expect_false(LinkEDA:::.rls_handle_r_data_assign_needed(parts))
  expect_identical(get(object_name, envir = .GlobalEnv, inherits = FALSE), "keep")
})

test_that("File-menu R chooser opens the selected global data frame", {
  state <- LinkEDA:::.rls_state
  token <- paste0(sample.int(1e8, 1L))
  request_id <- paste0("browse-", token)
  object_name <- paste0("menu_source_", token)
  previous_active <- state$active_dataset
  assign(object_name, data.frame(x = 1:2, y = 3:4), envir = .GlobalEnv)
  assign(request_id, TRUE, envir = state$r_data_menu_requests)
  on.exit({
    if (exists(object_name, envir = .GlobalEnv, inherits = FALSE))
      rm(list = object_name, envir = .GlobalEnv)
    if (exists(object_name, envir = state$datasets, inherits = FALSE))
      rm(list = object_name, envir = state$datasets)
    if (exists(request_id, envir = state$r_data_menu_requests, inherits = FALSE))
      rm(list = request_id, envir = state$r_data_menu_requests)
    state$active_dataset <- previous_active
  }, add = TRUE)

  expect_true(LinkEDA:::.rls_handle_r_data_choice_needed(
    c("R_DATA_CHOICE_NEEDED", request_id, "choose", object_name)
  ))
  expect_true(exists(object_name, envir = state$datasets, inherits = FALSE))
  expect_identical(state$active_dataset, object_name)
})

test_that("a one-numeric-variable dataset cannot create response-on-response models", {
  datos <- data.frame(
    Sujeto = c("A", "A", "B"),
    Condicion = c("Condición 1", "Condición 2", "Condición 3"),
    Valor = c(5, 6, 7),
    check.names = FALSE
  )
  expect_error(
    LinkEDA:::.rls_model_formula_object("Valor", "Valor"),
    "cannot also be a predictor"
  )
  expect_error(
    LinkEDA:::.rls_model_formula_object("Valor", "I(Valor^2)"),
    "cannot also be a predictor"
  )
  expect_equal(
    deparse(LinkEDA:::.rls_model_formula_object("Valor", "Condicion")),
    "Valor ~ Condicion"
  )
})
