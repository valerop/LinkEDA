test_that("native MI count requests fit the selected rows and their complement", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("mice")
  fixture <- Sys.getenv("LINKEDA_SELECTED_COUNT_REQUESTS")
  skip_if(!nzchar(fixture) || !file.exists(fixture), "Requires the native scope fixture")
  requests <- lapply(readLines(fixture), function(line)
    LinkEDA:::.rls_parse_generalized_glm_needed(strsplit(line, "\t", fixed = TRUE)[[1L]]))
  set.seed(4271)
  n <- 835L
  x <- rnorm(n)
  completed <- lapply(seq_len(3L), function(i) {
    xi <- x + rnorm(n, sd=.02)
    mu <- plogis(.4 + .3 * xi)
    data.frame(y=rbinom(n, 24, rbeta(n, mu*18, (1-mu)*18)), x=xi)
  })
  id <- ls_register_dataset("selected-count-mi", completed[[1L]])
  on.exit(ls_unregister_dataset(id), add=TRUE)
  dataset <- LinkEDA:::.rls_dataset_record(id)
  dataset$dataset_type <- "multiple_imputation"
  dataset$imputation_id <- "selected-count-mi-imputations"
  dataset$original_data <- completed[[1L]]
  dataset$completed_datasets <- completed
  dataset$imputation_count <- 3L
  dataset$original_row_ids <- seq_len(n)
  dataset$active_imputation_version <- 1L
  dataset$imputation_display_mode <- "version"
  LinkEDA:::.rls_set_dataset_record(dataset)
  expected <- list(216:835, 1:215, 1:835)
  for (i in seq_along(requests)) {
    request <- requests[[i]]
    model <- ls_new_count_regression(id, response=request$response, terms=request$terms,
      distribution=request$count_distribution, trials=request$trials_constant,
      scope=request$scope, .selected_rows=request$selected_rows, native=FALSE)
    state <- ls_count_regression_state(model)
    expect_equal(state$summary$n_used, length(expected[[i]]))
    expect_equal(state$rows_used, expected[[i]])
    expect_equal(state$data_scope$rows, if (i == 3L) integer() else expected[[i]])
    for (j in seq_along(completed)) {
      reference <- glmmTMB::glmmTMB(cbind(y, 24-y) ~ x,
        data=completed[[j]][expected[[i]], ], family=glmmTMB::betabinomial(link="logit"))
      expect_equal(glmmTMB::fixef(state$fits_by_imputation[[j]])$cond,
        glmmTMB::fixef(reference)$cond, tolerance=1e-7)
    }
  }
})
