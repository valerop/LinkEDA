test_that("contingency tables accept numeric codes without converting dataset types", {
  data <- datasets::mtcars
  data$car <- rownames(data)
  group <- ls_register_dataset("contingency-numeric", data)
  ds <- LinkEDA:::.rls_dataset_record(group)
  for (mode in c("count", "percent", "count_percent")) {
    result <- LinkEDA:::.rls_contingency_build(ds, "cyl", "am", "numeric-result", mode)
    reference <- with(data, table(cyl, am))
    expect_equal(unname(result$mean_counts[-nrow(result$mean_counts), -ncol(result$mean_counts)]),
                 unname(unclass(reference)))
    expect_identical(LinkEDA:::.rls_dataset_record(group)$data$cyl, data$cyl)
  }
  folder <- Sys.getenv("LINKEDA_NESTED_FIXTURES")
  if (nzchar(folder)) {
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    writeLines(c("REGISTER_DATASET", group,
      LinkEDA:::.rls_variable_payload(ds$data, ds$variable_metadata),
      LinkEDA:::.rls_dataframe_payload(ds$data, ds$variable_metadata, dataset_record = ds)),
      file.path(folder, "numeric-dataset.payload"))
  }
})


test_that("import detects short repeated categories while retaining text and storage", {
  alien <- utils::read.csv(system.file("examples", "Alien.csv", package = "LinkEDA"), stringsAsFactors = FALSE)
  alien$rating <- ordered(rep(c("low", "high"), length.out = nrow(alien)), levels = c("low", "high"))
  group <- LinkEDA:::.rls_register_imported_dataset(alien, tempfile(fileext = ".csv"),
    "alien-import-categories", "CSV file")
  record <- LinkEDA:::.rls_dataset_record(group)
  folder <- Sys.getenv("LINKEDA_NESTED_FIXTURES")
  if (nzchar(folder)) {
    writeLines(c("REGISTER_DATASET", group,
      LinkEDA:::.rls_variable_payload(record$data, record$variable_metadata),
      LinkEDA:::.rls_dataframe_payload(record$data, record$variable_metadata, dataset_record = record)),
      file.path(folder, "alien-dataset.payload"))
  }
  types <- setNames(record$variable_metadata$current_analysis_type, names(record$data))
  expect_identical(unname(types[c("planet", "happy")]), c("factor", "factor"))
  expect_identical(types[["alien_id"]], "character")
  expect_identical(types[["humans_eaten"]], "numeric")
  expect_identical(record$data$planet, alien$planet)
  expect_identical(LinkEDA:::.rls_metadata_levels(record$variable_metadata, "planet"), unique(alien$planet))
  group_test <- LinkEDA:::.rls_table1_group_factor(record, "happy")
  expect_true(is.factor(group_test))
  expect_equal(sort(levels(group_test)), sort(unique(alien$happy)))

  infer <- LinkEDA:::.rls_imported_variable_type
  expect_identical(infer(c("yes", "no", "yes", NA, "no", ""), "choice"), "factor")
  expect_identical(infer(rep("yes", 4), "choice"), "factor")
  expect_identical(infer(c("yes", "no"), "choice"), "character")
  expect_identical(infer(letters, "choice"), "character")
  expect_identical(infer(rep(letters, 4), "choice"), "character")
  expect_identical(infer(rep(c("Jane", "John"), 4), "name"), "character")
  expect_identical(infer(rep(c("A01", "B01"), 4), "case_id"), "character")
  expect_identical(infer(rep(c("OK", "Needs review"), 4), "comments"), "character")
  expect_identical(infer(rep(c("long\ncomment", "short"), 4), "response"), "character")
  expect_identical(infer(rep(strrep("a", 81), 4), "response"), "character")
  expect_identical(infer(rep(NA_character_, 4), "choice"), "character")
  expect_identical(infer(rep("", 4), "choice"), "character")
  expect_identical(infer(c(rep("12", 19), "12 years"), "age"), "character")
  expect_identical(infer(ordered(c("low", "high")), "level"), "ordered")
  # Registering an R object still honors its explicit classes.
  metadata <- LinkEDA:::.rls_variable_metadata(alien)
  expect_identical(metadata$current_analysis_type[match("planet", metadata$variable_name)], "character")
})
