test_that("the statistical example catalogue is complete and loadable", {
  root <- system.file("examples", "statistical", package = "LinkEDA")
  expect_true(dir.exists(root))
  catalog <- utils::read.csv(file.path(root, "catalog.csv"),
                             stringsAsFactors = FALSE, check.names = FALSE)
  expect_equal(nrow(catalog), 17L)
  expect_identical(anyDuplicated(catalog$id), 0L)
  expect_identical(anyDuplicated(catalog$file), 0L)
  expect_true(all(nzchar(catalog$title)))
  expect_true(all(nzchar(catalog$techniques)))
  expect_true(all(grepl("^https://", catalog$source_url)))
  expect_true(all(file.exists(file.path(root, catalog$file))))
  expect_true("categorical_columns" %in% names(catalog))
  expect_true(all(nzchar(catalog$analysis_objective)))
  expect_true(all(nzchar(catalog$citation)))

  variable_metadata <- utils::read.csv(
    file.path(root, "variable_descriptions.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_true(all(c("id", "variable", "description") %in% names(variable_metadata)))
  expect_true(all(variable_metadata$id %in% catalog$id))
  expect_true(all(nzchar(variable_metadata$description)))
  expect_identical(anyDuplicated(variable_metadata[c("id", "variable")]), 0L)

  for (example_id in catalog$id) {
    example_file <- file.path(root, catalog$file[catalog$id == example_id])
    example <- if (identical(tolower(tools::file_ext(example_file)), "rds")) {
      readRDS(example_file)
    } else {
      utils::read.csv(example_file, stringsAsFactors = FALSE,
                      check.names = FALSE)
    }
    columns <- if (inherits(example, "mids")) names(example$data) else names(example)
    documented <- variable_metadata$variable[variable_metadata$id == example_id]
    expect_identical(sort(documented), sort(columns))
  }

  coverage <- tolower(paste(catalog$techniques, collapse = "; "))
  required <- c("table 1", "contingency", "correlation", "pca/factor",
                "quick cluster", "one-sample", "two-sample", "paired-samples",
                "one-way anova", "scale analysis", "linear models",
                "binary models", "count models", "positive-continuous models",
                "proportion models", "scatterplot", "trellis", "time series",
                "scatterplot matrix", "parallel coordinates", "boxplot",
                "histogram", "bar chart", "offset", "multiple-imputation")
  expect_true(all(vapply(required, grepl, logical(1L), x = coverage,
                         fixed = TRUE)))

  csv <- catalog$file[grepl("[.]csv$", catalog$file)]
  frames <- lapply(file.path(root, csv), utils::read.csv,
                   stringsAsFactors = FALSE, check.names = FALSE)
  expect_true(all(vapply(frames, nrow, integer(1L)) > 0L))
  expect_true(all(vapply(frames, ncol, integer(1L)) > 1L))

  imputed <- readRDS(file.path(root, "nhanes_mice.rds"))
  expect_s3_class(imputed, "mids")
  expect_equal(imputed$m, 5L)
})

test_that("catalogue examples preserve numeric-looking categorical variables", {
  registered <- list()
  testthat::local_mocked_bindings(
    .rls_register_imported_dataset = function(data, path, name, source, ...) {
      registered[[name]] <<- data
      name
    },
    .rls_send = function(lines, expect_reply = TRUE) "OK",
    .package = "LinkEDA"
  )

  for (example in c("anscombe", "chickweight", "gasoline_yield", "theoph")) {
    expect_true(LinkEDA:::.rls_handle_welcome_action_needed(
      c("WELCOME_ACTION_NEEDED", paste0("example-", example), "example", example)
    ))
  }

  expect_true(is.factor(registered$anscombe$set))
  expect_true(is.factor(registered$chickweight$Chick))
  expect_true(is.factor(registered$chickweight$Diet))
  expect_true(is.numeric(registered$chickweight$Time))
  expect_true(is.factor(registered$gasoline_yield$batch))
  expect_true(is.factor(registered$theoph$subject))
  chick_label <- attr(registered$chickweight$Chick, "label", exact = TRUE)
  expect_match(chick_label, "Unique chick identifier", fixed = TRUE)
  expect_match(chick_label, "Analysis objective:", fixed = TRUE)
  expect_match(chick_label, "Crowder & Hand (1990)", fixed = TRUE)
})

test_that("a catalogue example is opened through the Welcome action", {
  registered <- NULL
  replies <- list()
  testthat::local_mocked_bindings(
    .rls_register_imported_dataset = function(data, path, name, source, ...) {
      registered <<- list(data = data, path = path, name = name, source = source)
      name
    },
    .rls_send = function(lines, expect_reply = TRUE) {
      replies[[length(replies) + 1L]] <<- lines
      "OK"
    },
    .package = "LinkEDA"
  )
  expect_true(LinkEDA:::.rls_handle_welcome_action_needed(
    c("WELCOME_ACTION_NEEDED", "example-public", "example", "iris")))
  expect_identical(registered$name, "iris")
  expect_equal(nrow(registered$data), 150L)
  expect_true(all(c("Sepal.Length", "Species") %in% names(registered$data)))
  expect_identical(registered$source, "Public statistical example")
  expect_match(attr(registered$data$Species, "label", exact = TRUE),
               "Iris species", fixed = TRUE)
  expect_match(attr(registered$data$Species, "label", exact = TRUE),
               "Fisher (1936)", fixed = TRUE)
  expect_equal(replies[[1L]][1:4],
               c("WELCOME_ACTION_RESULT", "example-public", "ok", "example"))
})
