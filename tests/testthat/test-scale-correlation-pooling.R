test_that("scale Fisher pooling uses mice without altering boundary inputs", {
  skip_if_not_installed("mice")
  for (r in list(c(.3, .5, .7), rep(.3, 3), c(.9999999, .9999998, .9999997))) {
    n <- c(40, 50, 60)
    actual <- LinkEDA:::.rls_scale_pool_fisher(r, n)
    reference <- mice::pool.scalar(atanh(r), 1/(n-3), n=Inf, k=1, rule="rubin1987")
    expect_true(actual$valid)
    expect_equal(actual$r, tanh(reference$qbar))
    expect_equal(actual$SE, sqrt(reference$t))
    expect_equal(actual$p, 2*stats::pt(-abs(reference$qbar/sqrt(reference$t)), reference$df))
  }
  for (case in list(list(c(.4,.5),c(3,3)), list(c(1,1),c(30,30)),
                   list(c(.4,-1),c(30,30)), list(c(.4,NA),c(30,30)),
                   list(c(.4,Inf),c(30,30)), list(.4,30), list(c(.4,.5),30))) {
    actual <- LinkEDA:::.rls_scale_pool_fisher(case[[1]],case[[2]])
    expect_false(actual$valid)
    expect_true(is.na(actual$r)); expect_true(is.na(actual$SE)); expect_true(is.na(actual$p))
    expect_true(nzchar(actual$reason))
  }
})

scale_pooling_fixture <- function(kind) {
  set.seed(3907)
  n <- if (kind == "small") 3L else 60L
  base <- rnorm(n)
  completed <- lapply(seq_len(3), function(i) data.frame(
    q1=base+rnorm(n,sd=.4), q2=base+rnorm(n,sd=.5),
    q3=base+rnorm(n,sd=.6), q4=base+rnorm(n,sd=.7)))
  if (kind == "perfect") for (i in seq_along(completed)) completed[[i]]$q2 <- completed[[i]]$q1
  if (kind == "one_invalid") completed[[2]]$q2 <- completed[[2]]$q1
  if (kind == "invariant") completed <- rep(completed[1],3)
  original <- completed[[1]]
  specs <- LinkEDA:::.rls_scale_item_specifications(original,names(original))
  options <- LinkEDA:::.rls_scale_default_options("pearson")
  dataset <- list(group="scale_pooling",dataset_id="scale_pooling",dataset_type="multiple_imputation",
    data=original,original_data=original,completed_datasets=completed,
    imputation_count=3L,original_row_ids=seq_len(n),missing_cell_mask=lapply(original,is.na))
  result <- suppressMessages(suppressWarnings(LinkEDA:::.rls_scale_fit(dataset,specs,options)))
  list(dataset=dataset, completed=completed, specs=specs, options=options, result=result)
}

test_that("complete MI scale outputs retain unavailable correlations without fallback p-values", {
  skip_if_not_installed("psych"); skip_if_not_installed("mice")
  for (kind in c("valid","invariant","perfect","one_invalid","small")) {
    fixture <- scale_pooling_fixture(kind); fit <- fixture$result
    for (i in 1:3) for (j in seq.int(i+1,4)) {
      r <- vapply(fit$fits_by_imputation,function(x)x$correlation$matrix[i,j],numeric(1))
      n <- vapply(fit$fits_by_imputation,function(x)x$correlation$sample_sizes[i,j],numeric(1))
      if (all(is.finite(r) & abs(r)<1 & n>3)) {
        reference <- mice::pool.scalar(atanh(r),1/(n-3),n=Inf,k=1,rule="rubin1987")
        expect_equal(fit$correlations$matrix[i,j],tanh(reference$qbar))
        expect_equal(fit$correlations$p_values[i,j],
          2*stats::pt(-abs(reference$qbar/sqrt(reference$t)),reference$df))
      } else {
        expect_true(is.na(fit$correlations$matrix[i,j]))
        expect_true(is.na(fit$correlations$p_values[i,j]))
        expect_true(nzchar(fit$correlations$unavailability_reasons[i,j]))
      }
    }
    if (kind %in% c("perfect","one_invalid","small")) {
      expect_match(fit$pooling_note,"unavailable")
      expect_true(is.na(fit$correlations$matrix[1,2]))
    }
    if (kind == "small") {
      expect_true(all(is.na(fit$items$item_rest_r)))
      expect_true(all(nzchar(fit$items$item_rest_reason)))
    }
  }
})

test_that("native MI scale verification recipe executes and agrees with the full R result", {
  driver <- Sys.getenv("LINKEDA_SCALE_RECIPE_DRIVER")
  skip_if(!nzchar(driver), "Build tests/native/scale_verification_export_driver.cpp and set LINKEDA_SCALE_RECIPE_DRIVER")
  skip_if_not_installed("psych"); skip_if_not_installed("mice")
  folder <- tempfile(); dir.create(folder); on.exit(unlink(folder,recursive=TRUE),add=TRUE)
  wire <- file.path(folder,"result.txt"); recipe <- file.path(folder,"recipe.R"); datafile <- file.path(folder,"data.rds")
  local_mocked_bindings(.rls_send=function(lines,...) {writeLines(lines,wire); TRUE},.package="LinkEDA")
  for (kind in c("valid","invariant","perfect","one_invalid","small")) {
    fixture <- scale_pooling_fixture(kind); fit <- fixture$result
    record <- list(id="scale_check",group="scale_pooling",item_specifications=fixture$specs,
      options=fixture$options,model_version=1L,specification_fingerprint="frozen-spec",
      result=LinkEDA:::.rls_scale_attach_identity(fit,1L,"frozen-spec"))
    LinkEDA:::.rls_scale_sync_native(record,open=TRUE)
    expect_identical(system2(driver,c(shQuote(wire),shQuote(recipe),nrow(fixture$dataset$data))),0L)
    long <- do.call(rbind,lapply(seq_along(fixture$completed),function(i)
      cbind(.imp=i,.id=seq_len(nrow(fixture$completed[[i]])),fixture$completed[[i]])))
    saveRDS(long,datafile)
    env <- new.env(parent=baseenv());env$verification_data_path <- datafile
    code <- paste(readLines(recipe),collapse="\n")
    expect_false(grepl("LinkEDA:::|.rls_|.999999",code))
    suppressMessages(suppressWarnings(eval(parse(text=code),env)))
    expect_equal(env$pooled_correlations,fit$correlations$matrix,tolerance=1e-12)
    expect_equal(env$pooled_item_table$Item_rest_r,fit$items$item_rest_r,tolerance=1e-12)
    expect_equal(env$pooled_item_table$Mean,fit$items$mean,tolerance=1e-12)
    expect_equal(env$pooled_item_table$Mean_SE,fit$items$mean_se,tolerance=1e-12)
    if (kind == "small") expect_true(any(grepl("Fisher-z pooling unavailable",readLines(wire),fixed=TRUE)))
  }
})
