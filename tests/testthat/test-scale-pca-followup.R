
# These tests check the matrix-based PCA shared by Scale Analysis and typed PCA.
test_that("scale PCA uses psych communalities after oblique rotation and covariance fitting", {
  skip_if_not_installed("psych"); skip_if_not_installed("GPArotation")
  withr::local_options(mc.cores = 1L)
  x <- as.data.frame(sweep(scale(mtcars[, c("mpg", "disp", "hp", "wt", "qsec")]),
    2, c(1, 2, 3, 1.5, 2.5), `*`))
  for (standardize in c(TRUE, FALSE)) for (rotation in c("none", "oblimin")) {
    r <- if (standardize) cor(x) else cov(x)
    actual <- LinkEDA:::.rls_dimension_pca_from_correlation(r, nrow(x), 2,
      rotation = rotation, parallel_iterations = 2, score_data = x, scale = standardize)
    reference <- psych::principal(r, nfactors = 2, rotate = rotation,
      n.obs = nrow(x), scores = FALSE, covar = !standardize)
    expect_equal(actual$loadings$h2, as.numeric(reference$communality), tolerance = 1e-10)
    expect_equal(actual$loadings$u2, as.numeric(reference$uniquenesses), tolerance = 1e-10)
    expect_equal(actual$loadings$h2 + actual$loadings$u2, unname(diag(r)))
    # The scree table continues to show eigenvalue proportions before rotation.
    expect_equal(actual$component_variance, as.numeric(proportions(reference$values)))
    expect_identical(actual$scores$row, seq_len(nrow(x)))
  }
})

test_that("non-finite complete-row PCA scores never reach plots or saved columns", {
  skip_if_not_installed("psych")
  withr::local_options(mc.cores = 1L)
  withr::local_seed(890)
  x <- as.data.frame(matrix(rnorm(150 * 5), 150, 5))
  for (complete_count in c(1L, 10L)) {
    bad <- x
    bad[seq_len(complete_count), 1] <- 1
    rows <- seq.int(complete_count + 1L, nrow(bad))
    bad[cbind(rows, 2L + rows %% 4L)] <- NA_real_
    r <- cor(bad, use = "pairwise.complete.obs")
    actual <- LinkEDA:::.rls_dimension_pca_from_correlation(r, nrow(x), 2,
      parallel_iterations = 2, score_data = bad)
    expect_equal(nrow(actual$loadings), 5L)
    expect_equal(nrow(actual$scores), 0L)
    expect_match(actual$score_status, "not finite")
    expect_identical(actual$status, actual$score_status)
    wire <- LinkEDA:::.rls_scale_dimensionality_series(list(actual))
    expect_false(any(vapply(wire, function(s) s$kind == "factor_score_imputation", logical(1))))
  }
  x[1:5,1] <- NA_real_
  actual <- LinkEDA:::.rls_dimension_pca_from_correlation(cor(x, use="pairwise.complete.obs"), nrow(x), 2,
    parallel_iterations = 2, score_data = x)
  expect_identical(actual$scores$row, 6:150)
  expect_true(all(is.finite(as.matrix(actual$scores[,-1]))))
  expect_identical(actual$status, "value")
})


test_that("MI scale PCA tables match the executable public native verification recipe", {
  driver <- Sys.getenv("LINKEDA_SCALE_RECIPE_DRIVER")
  skip_if(!nzchar(driver), "Build the scale verification export driver and set LINKEDA_SCALE_RECIPE_DRIVER")
  skip_if_not_installed("psych"); skip_if_not_installed("mice")
  withr::local_options(mc.cores = 1L)
  folder <- tempfile(); dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  wire <- file.path(folder,"result.txt"); recipe <- file.path(folder,"recipe.R")
  datafile <- file.path(folder,"data.rds")
  local_mocked_bindings(.rls_send = function(lines,...) {writeLines(lines, wire); TRUE}, .package = "LinkEDA")
  x <- as.data.frame(sweep(scale(mtcars[, c("mpg", "disp", "hp", "wt", "qsec")]),
    2, c(1, 2, 3, 1.5, 2.5), `*`))
  completed <- list(x, x * 1.1)
  specs <- LinkEDA:::.rls_scale_item_specifications(x, names(x))
  dataset <- list(group="pca_check", dataset_id="pca_check", dataset_type="multiple_imputation",
    data=x, original_data=x, completed_datasets=completed, imputation_count=2L,
    original_row_ids=seq_len(nrow(x)), missing_cell_mask=lapply(x,is.na))
  for (standardize in c(TRUE,FALSE)) {
    opts <- LinkEDA:::.rls_scale_default_options("pearson", dimensionality=TRUE,
      factors=2, dimensionality_method="pca", rotation="oblimin",
      dimensionality_scale=standardize, parallel_iterations=2)
    fit <- suppressMessages(suppressWarnings(LinkEDA:::.rls_scale_fit(dataset,specs,opts)))
    record <- list(id="pca_check",group="pca_check",item_specifications=specs,
      options=opts,model_version=1L,specification_fingerprint="frozen-pca",
      result=LinkEDA:::.rls_scale_attach_identity(fit,1L,"frozen-pca"))
    LinkEDA:::.rls_scale_sync_native(record, open=TRUE)
    expect_identical(system2(driver,c(shQuote(wire),shQuote(recipe),nrow(x))),0L)
    long <- do.call(rbind,lapply(seq_along(completed),function(i)
      cbind(.imp=i,.id=seq_len(nrow(x)),completed[[i]])))
    saveRDS(long,datafile)
    env <- new.env(parent=baseenv()); env$verification_data_path <- datafile
    code <- paste(readLines(recipe),collapse="\n")
    expect_false(grepl("LinkEDA:::|\\.rls_",code))
    suppressMessages(suppressWarnings(invisible(capture.output(eval(parse(text=code),env)))))
    for (i in seq_along(completed)) {
      actual <- fit$fits_by_imputation[[i]]$dimensionality
      reference <- env$scale_results[[i]]$dimensionality
      expect_equal(actual$loadings$h2,as.numeric(reference$communality),tolerance=1e-10)
      expect_equal(actual$loadings$u2,as.numeric(reference$uniquenesses),tolerance=1e-10)
      series <- LinkEDA:::.rls_scale_dimensionality_series(list(actual))
      u2 <- Filter(function(s) s$kind == "factor_uniqueness_imputation",series)[[1]]
      expect_equal(u2$values,as.numeric(reference$uniquenesses),tolerance=1e-10)
    }
  }
})
