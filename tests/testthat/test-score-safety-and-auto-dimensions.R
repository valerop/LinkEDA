
.score_safety_data <- function() {
  withr::local_seed(931)
  z <- rnorm(140)
  x <- as.data.frame(replicate(5,z+rnorm(140)));names(x)<-paste0("q",1:5)
  x
}
.score_safety_fit <- function(id, ...) ls_new_scale_analysis(id,items=paste0("q",1:5),
  dimensionality=TRUE,factors=1,rotation="none",parallel_iterations=3,native=FALSE,...)

test_that("saving scores preserves foreign columns and only reuses an owned column", {
  x <- .score_safety_data();x$F1_score <- 999;x$PC1_score<-888
  id <- ls_register_dataset("score_collision_audit",x)
  on.exit(ls_unregister_dataset(id))
  a <- .score_safety_fit(id)
  one <- ls_scale_analysis_save_scores(a,components=1)
  expect_identical(one,"F1_score_1")
  expect_identical(ls_scale_analysis_save_scores(a,components=1),one)
  b <- .score_safety_fit(id)
  two <- ls_scale_analysis_save_scores(b,components=1)
  expect_identical(two,"F1_score_2")
  p <- ls_new_dimensionality(id,variables=paste0("q",1:5),n_components=1,parallel=FALSE,native=FALSE)
  pc <- ls_dimensionality_save_scores(p,components=1)
  expect_identical(pc,"PC1_score_1")
  expect_identical(ls_dimensionality_save_scores(p,components=1),pc)
  record <- LinkEDA:::.rls_dataset_record(id)
  expect_true(all(record$data$F1_score==999));expect_true(all(record$data$PC1_score==888))
  # Lost ownership after reopen is not inferred from a matching column name.
  record$derived_score_columns <- NULL;LinkEDA:::.rls_set_dataset_record(record)
  three <- ls_scale_analysis_save_scores(a,components=1)
  expect_identical(three,"F1_score_3")
  expect_equal(LinkEDA:::.rls_dataset_record(id)$data[[one]],record$data[[one]])
})

test_that("changed inputs or row identities prevent saving stale scores without mutation", {
  x <- .score_safety_data()
  for(change in c("cell","rows","dropped")) {
    id <- ls_register_dataset(paste0("score_stale_",change),x)
    a <- .score_safety_fit(id)
    p <- ls_new_dimensionality(id,variables=names(x),n_components=1,parallel=FALSE,native=FALSE)
    record <- LinkEDA:::.rls_dataset_record(id)
    if(change=="cell") record$data$q1[1]<-999
    if(change=="dropped") record$data$q1<-NULL
    if(change=="rows") {
      record$data <- record$data[nrow(x):1,]
      record$stable_row_ids <- rev(record$stable_row_ids)
    }
    record$data_frame <- record$data
    record <- LinkEDA:::.rls_advance_data_version(record,"Fixture input edit")
    LinkEDA:::.rls_set_dataset_record(record)
    before <- LinkEDA:::.rls_dataset_record(id)
    expect_error(ls_scale_analysis_save_scores(a,components=1),"Refit")
    expect_error(ls_dimensionality_save_scores(p,components=1),"Refit")
    expect_identical(LinkEDA:::.rls_dataset_record(id),before)
    ls_unregister_dataset(id)
  }
  id <- ls_register_dataset("score_unrelated_edit",x)
  on.exit(ls_unregister_dataset(id))
  a <- .score_safety_fit(id)
  record <- LinkEDA:::.rls_dataset_record(id);record$data$unrelated <- 10
  record <- LinkEDA:::.rls_advance_data_version(record,"Unrelated column")
  LinkEDA:::.rls_set_dataset_record(record)
  expect_type(ls_scale_analysis_save_scores(a,components=1),"character")
})

test_that("MI score saving checks every completed dataset and preserves existing columns", {
  skip_if_not_installed("mice")
  x <- .score_safety_data();x$q1[c(2,4,7)]<-NA;x$F1_score<-1000+seq_len(nrow(x))
  withr::local_seed(987)
  imp <- mice::mice(x,m=2,maxit=1,printFlag=FALSE)
  id <- ls_import_mice(imp,name="score_mi_safety",make_active=FALSE)
  on.exit(ls_unregister_dataset(id))
  a <- .score_safety_fit(id)
  name <- ls_scale_analysis_save_scores(a,components=1,prefix="f")
  expect_identical(name,"f1_score_1")
  record <- LinkEDA:::.rls_dataset_record(id)
  for(i in 1:2) {
    expect_equal(record$completed_datasets[[i]]$f1_score,x$F1_score)
    expect_equal(mice::complete(record$mids_object,i)[[name]],record$completed_datasets[[i]][[name]])
  }
  record$completed_datasets[[2]]$q2[1]<-999
  record <- LinkEDA:::.rls_advance_data_version(record,"Edit second imputation")
  LinkEDA:::.rls_set_dataset_record(record)
  before <- LinkEDA:::.rls_dataset_record(id)
  expect_error(ls_scale_analysis_save_scores(a,components=1,prefix="f"),"Refit")
  expect_identical(LinkEDA:::.rls_dataset_record(id),before)
})

test_that("failed MI fits do not renumber remaining scree curves", {
  x <- .score_safety_data()
  good <- LinkEDA:::.rls_dimension_pca_from_correlation(cor(x),nrow(x),1,
    rotation="none",parallel_iterations=3,score_data=x)
  bad_matrix <- cor(x);bad_matrix[1,2]<-bad_matrix[2,1]<-.99
  bad_matrix[1,3]<-bad_matrix[3,1]<-.99;bad_matrix[2,3]<-bad_matrix[3,2]<- -.99
  bad <- LinkEDA:::.rls_dimension_pca_from_correlation(bad_matrix,nrow(x),1,score_data=x)
  for(fits in list(list(bad,good),list(good,bad,good),list(bad,bad,good))) {
    series <- LinkEDA:::.rls_scale_scree_series(fits,TRUE)
    expected <- which(vapply(fits,function(f)length(f$parallel$fa.values)>0,logical(1)))
    for(kind in c("scree_observed_imputation","scree_reference_imputation")) {
      curves <- Filter(function(s)s$kind==kind,series)
      expect_identical(vapply(curves,`[[`,character(1),"name"),paste("Imputation",expected))
      for(i in seq_along(expected)) expect_equal(curves[[i]]$values,
        if(kind=="scree_observed_imputation")fits[[expected[i]]]$parallel$fa.values else fits[[expected[i]]]$parallel$reference)
    }
    expect_match(series[[1]]$name,paste(length(expected),"of",length(fits)))
  }
})

.zero_dimension_data <- function() {
  withr::local_seed(75)
  x <- as.data.frame(qr.Q(qr(cbind(1,matrix(rnorm(150*5),150,5))))[,-1])
  names(x)<-paste0("q",1:5);x
}

test_that("automatic zero leaves the parallel plot available and does not invent a solution", {
  x <- .zero_dimension_data()
  for(method in c("pca","factor")) {
    a <- suppressWarnings(ls_new_scale_analysis(x,items=names(x),dimensionality=TRUE,
      dimensionality_method=method,rotation="none",parallel_iterations=5,native=FALSE))
    state <- ls_scale_analysis_state(a)
    value <- state$result$dimensionality
    expect_identical(value$suggested_factors,0L)
    expect_identical(value$factors,0L)
    expect_equal(nrow(value$loadings),0L);expect_equal(nrow(value$scores),0L)
    expect_match(value$status,"recommends 0")
    expect_length(value$parallel$reference,5)
    kinds <- vapply(state$result$plot_series,`[[`,character(1),"kind")
    expect_true("scree_observed" %in% kinds)
    expect_false("factor_score_imputation" %in% kinds)
    expect_error(ls_scale_analysis_save_scores(a),"recommends 0")
    # Choosing one dimension is still permitted as an explicit exploratory choice.
    suppressWarnings(ls_scale_analysis_set_options(a,factors=1))
    manual <- ls_scale_analysis_state(a)$result$dimensionality
    expect_identical(manual$factors,1L);expect_equal(nrow(manual$loadings),5L)
    ls_unregister_dataset(a$group)
  }
})


test_that("exported MI verification retains zero automatically and allows a manual choice", {
  driver <- Sys.getenv("LINKEDA_SCALE_RECIPE_DRIVER")
  skip_if(!nzchar(driver),"Build the shared scale recipe driver")
  skip_if_not_installed("mice")
  x <- .zero_dimension_data(); completed <- list(x,x*1.2)
  dataset <- list(group="zero_mi",dataset_id="zero_mi",dataset_type="multiple_imputation",
    data=x,original_data=x,completed_datasets=completed,imputation_count=2L,
    original_row_ids=seq_len(nrow(x)),missing_cell_mask=lapply(x,is.na))
  specs <- LinkEDA:::.rls_scale_item_specifications(x,names(x))
  folder <- tempfile();dir.create(folder);on.exit(unlink(folder,recursive=TRUE))
  wire <- file.path(folder,"wire.txt"); recipe <- file.path(folder,"recipe.R"); datafile <- file.path(folder,"data.rds")
  local_mocked_bindings(.rls_send=function(lines,...) {writeLines(lines,wire);TRUE},.package="LinkEDA")
  for(method in c("pca","factor")) for(count in c(0L,1L)) {
    opts <- LinkEDA:::.rls_scale_default_options("pearson",dimensionality=TRUE,
      dimensionality_method=method,factors=if(count==0)NULL else count,rotation="none",parallel_iterations=5)
    fit <- suppressMessages(suppressWarnings(LinkEDA:::.rls_scale_fit(dataset,specs,opts)))
    record <- list(id="zero_mi",group="zero_mi",item_specifications=specs,
      options=opts,model_version=1L,specification_fingerprint="zero-frozen",
      result=LinkEDA:::.rls_scale_attach_identity(fit,1L,"zero-frozen"))
    LinkEDA:::.rls_scale_sync_native(record,open=TRUE)
    expect_identical(system2(driver,c(shQuote(wire),shQuote(recipe),nrow(x))),0L)
    long <- do.call(rbind,lapply(seq_along(completed),function(i)cbind(.imp=i,.id=seq_len(nrow(x)),completed[[i]])))
    saveRDS(long,datafile)
    env <- new.env(parent=baseenv());env$verification_data_path <- datafile
    code <- paste(readLines(recipe),collapse="\n")
    expect_false(grepl("LinkEDA:::|\\.rls_",code))
    suppressMessages(suppressWarnings(invisible(capture.output(eval(parse(text=code),env)))))
    for(i in 1:2) {
      actual <- fit$dimensionality$results_by_imputation[[i]]
      ref <- env$scale_results[[i]]$dimensionality
      expect_equal(ref$factors,count)
      expect_equal(actual$factors,count)
      expect_equal(ref$parallel$reference,actual$parallel$reference)
      if(count==0L) {
        expect_null(ref$loadings);expect_null(ref$scores)
        expect_match(ref$status,"recommends 0")
      } else expect_equal(unname(as.matrix(actual$loadings[,2,drop=FALSE])),unname(unclass(ref$loadings)),ignore_attr=TRUE)
    }
  }
})
