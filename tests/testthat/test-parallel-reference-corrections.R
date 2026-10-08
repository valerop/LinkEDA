
.parallel_fixture <- function() {
  withr::local_seed(729)
  a <- rnorm(240); b <- .3*a + rnorm(240)
  x <- as.data.frame(cbind(a + matrix(rnorm(720),240,3), b + matrix(rnorm(720),240,3)))
  names(x) <- paste0("q",1:6)
  x
}

test_that("shared PCA and FA draw the decision percentile returned by library simulations", {
  skip_if_not_installed("psych")
  withr::local_options(mc.cores=1L)
  x <- .parallel_fixture();r <- cor(x)
  for (method in c("pca","factor")) {
    engine <- if(method == "pca") LinkEDA:::.rls_dimension_pca_from_correlation else LinkEDA:::.rls_dimension_factor_from_correlation
    set.seed(546); seed <- .Random.seed
    actual <- engine(r,nrow(x),2,rotation="none",parallel_iterations=30,score_data=x)
    expect_identical(.Random.seed, seed)
    set.seed(271828L)
    invisible(capture.output(ref <- psych::fa.parallel(r,n.obs=nrow(x),fm="minres",
      fa=if(method=="pca")"pc" else "fa",SMC=method=="pca",n.iter=30,quant=.95,plot=FALSE)))
    cols <- seq_len(ncol(x)) + if(method=="pca")0L else ncol(x)
    expected <- as.numeric(apply(ref$values[,cols,drop=FALSE],2,quantile,.95))
    expect_equal(actual$parallel$reference,expected,tolerance=1e-12)
    expect_gt(max(abs(expected-colMeans(ref$values[,cols,drop=FALSE]))),.01)
    expect_equal(actual$suggested_factors,if(method=="pca")ref$ncomp else ref$nfact)
    line <- Filter(function(z) z$kind=="scree_reference",
      LinkEDA:::.rls_scale_scree_series(list(actual)))[[1]]
    expect_equal(line$values,expected)
  }
})

test_that("covariance PCA keeps observed and reference eigenvalues in the same units", {
  skip_if_not_installed("psych")
  withr::local_options(mc.cores=1L)
  x <- .parallel_fixture(); x[1:6,1]<-NA; x[7:12,2]<-NA
  for (missing in c("pairwise","listwise")) {
    use <- if(missing=="listwise")"complete.obs" else "pairwise.complete.obs"
    engine <- function(d) LinkEDA:::.rls_dimension_pca_from_correlation(cov(d,use=use),nrow(d),2,
      rotation="none",parallel_iterations=15,score_data=d,scale=FALSE,missing=missing)
    actual <- engine(x); larger <- engine(x*100)
    expect_equal(larger$parallel$pc.values,actual$parallel$pc.values*10000,tolerance=1e-8)
    expect_equal(larger$parallel$reference,actual$parallel$reference*10000,tolerance=1e-8)
    expect_identical(larger$suggested_factors,actual$suggested_factors)
    raw <- if(missing=="listwise") x[complete.cases(x),] else x
    set.seed(271828L)
    invisible(capture.output(ref<-psych::fa.parallel(raw,fa="pc",cor="cov",sim=FALSE,SMC=TRUE,
      use=if(missing=="listwise")"complete" else "pairwise",n.iter=15,quant=.95,plot=FALSE)))
    expect_equal(actual$parallel$reference,as.numeric(apply(ref$values,2,quantile,.95)))
    expect_equal(actual$parallel$pc.values,ref$pc.values)
    expect_identical(actual$scores$row,which(complete.cases(x)))
  }
})

test_that("PCA refuses invalid matrices without silently smoothing them", {
  skip_if_not_installed("psych")
  x <- .parallel_fixture();r <- cor(x)
  r[1,2]<-r[2,1]<-.99;r[1,3]<-r[3,1]<-.99;r[2,3]<-r[3,2]<- -.99
  expect_lt(min(eigen(r,symmetric=TRUE)$values),-.1)
  local_mocked_bindings(principal=function(...) stop("PCA must not be called"),
    fa.parallel=function(...) stop("Parallel analysis must not be called"),.package="psych")
  invalid <- list(r,replace(r,c(1,2),NA_real_),replace(r,1,0))
  for (matrix in invalid) {
    actual <- LinkEDA:::.rls_dimension_pca_from_correlation(matrix,nrow(x),2,score_data=x)
    expect_false(identical(actual$status,"value"))
    expect_equal(nrow(actual$loadings),0L)
    expect_equal(nrow(actual$scores),0L)
    expect_length(actual$parallel,0L)
  }
})

test_that("semidefinite PCA still permits loadings but never unstable inverse scores", {
  skip_if_not_installed("psych")
  withr::local_options(mc.cores=1L)
  x <- .parallel_fixture(); x$q6 <- x$q5
  actual <- suppressWarnings(LinkEDA:::.rls_dimension_pca_from_correlation(cor(x),nrow(x),2,
    rotation="none",parallel_iterations=2,score_data=x))
  expect_equal(nrow(actual$loadings),6L)
  expect_equal(nrow(actual$scores),0L)
  expect_match(actual$score_status,"singular|not positive definite")
})

test_that("typed dimensionality exports the same parallel curve as the table", {
  skip_if_not_installed("psych")
  x <- .parallel_fixture()
  x$q1 <- ordered(cut(x$q1,quantile(x$q1,seq(0,1,length.out=5)),include.lowest=TRUE))
  for (method in c("pca","factor")) {
    model <- ls_new_dimensionality(x,variables=names(x),method=method,n_components=2,
      rotation="none",missing="listwise",parallel_iterations=5,native=FALSE)
    state <- ls_dimensionality_state(model)
    code <- state$analysis_provenance$verification_r_code$table
    expect_false(grepl("LinkEDA:::|\\.rls_",code))
    path <- tempfile(fileext=".rds");saveRDS(x,path)
    env <- new.env(parent=baseenv());env$verification_data_path <- path
    invisible(capture.output(eval(parse(text=code),env)))
    unlink(path)
    expect_equal(env$reference_result$components$Parallel,state$eigenvalues$parallel_eigenvalue,tolerance=1e-10)
  }
})

test_that("MI parallel curves and recommendations match the executable native R recipe", {
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
  for (standardize in c(TRUE,FALSE)) for (method in c("pca","factor")) {
    opts <- LinkEDA:::.rls_scale_default_options("pearson", dimensionality=TRUE,
      factors=2, dimensionality_method=method, rotation="none",
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
      expect_equal(actual$parallel$reference,reference$parallel$reference,tolerance=1e-10)
      expect_equal(actual$suggested_factors,if(method=="pca") reference$parallel$ncomp else reference$parallel$nfact)
      expect_equal(actual$loadings$h2,as.numeric(reference$communality),tolerance=1e-10)
      expect_equal(actual$loadings$u2,as.numeric(reference$uniquenesses),tolerance=1e-10)
      series <- LinkEDA:::.rls_scale_dimensionality_series(list(actual))
      u2 <- Filter(function(s) s$kind == "factor_uniqueness_imputation",series)[[1]]
      expect_equal(u2$values,as.numeric(reference$uniquenesses),tolerance=1e-10)
    }
  }
})
