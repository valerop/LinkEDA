test_that("dimensionality tasks freeze rows and return versioned results or errors", {
  data <- mtcars[,c("mpg","disp","hp","wt")]
  group <- ls_register_dataset("dimension-race",data)
  ds <- LinkEDA:::.rls_dataset_record(group)
  sent <- NULL
  local_mocked_bindings(.rls_send=function(lines,...) {sent <<- lines; "OK"}, .package="LinkEDA")
  runtime <- LinkEDA:::.rls_state
  old_started <- runtime$process_started
  withr::defer(runtime$process_started <- old_started)
  runtime$process_started <- TRUE
  make_task <- function(method, rows, revision=1L, version=ds$data_version, imputation=1L,
                        extraction="minres") {
    c("PCAFA_FACTOR_NEEDED",paste0("race-",method),group,method,"listwise","none","selected",
      "TRUE","1","4",names(data),as.character(length(rows)),as.character(rows),
      "IMPUTATION_V1",as.character(imputation),"EXTRACTION_V1",extraction,
      "REQUEST_V1",as.character(revision),as.character(version))
  }
  folder <- Sys.getenv("LINKEDA_DIMENSION_RACE_FIXTURES")
  if(nzchar(folder)) {
    dir.create(folder,showWarnings=FALSE,recursive=TRUE)
    writeLines(c("REGISTER_DATASET",group,LinkEDA:::.rls_variable_payload(ds$data,ds$variable_metadata),
      LinkEDA:::.rls_dataframe_payload(ds$data,ds$variable_metadata,dataset_record=ds)),file.path(folder,"dataset.payload"))
  }
  for(method in c("pca","factor")) for(i in 1:2) {
    rows <- if(i==1) 1:24 else 9:32
    LinkEDA:::.rls_handle_dimensionality_needed(make_task(method,rows,i))
    expect_identical(sent[1:6],c("PCAFA_UPDATE",paste0("race-",method),"REQUEST_V1",as.character(i),as.character(ds$data_version),"ok"))
    fit <- LinkEDA:::.rls_dimension_record(paste0("race-",method))
    expect_identical(fit$rows_used_original_ids,rows)
    expect_identical(fit$data_scope$rows,rows)
    reference <- if(method=="pca") stats::prcomp(data[rows,],scale.=TRUE)$sdev^2 else
      psych::fa(cor(data[rows,]),nfactors=1,fm="minres",rotate="none")$values
    expect_equal(fit$eigenvalues$eigenvalue,unname(reference),tolerance=1e-8)
    expect_identical(fit$analysis_provenance$dataset_version,ds$data_version)
    if(nzchar(folder))writeLines(sent,file.path(folder,paste0(method,"-",i,".payload")))
  }
  for(method in c("pca","factor")) {
    LinkEDA:::.rls_handle_dimensionality_needed(make_task(method,integer(),3))
    expect_identical(sent[6],"error")
    if(nzchar(folder))writeLines(sent,file.path(folder,paste0(method,"-error.payload")))
  }
  LinkEDA:::.rls_handle_dimensionality_needed(make_task("pca",1:24,4,99999))
  expect_identical(sent[6],"error");expect_match(sent[7],"data changed")
  LinkEDA:::.rls_handle_dimensionality_needed(make_task("pca",c(1,999),5))
  expect_identical(sent[6],"error");expect_match(sent[7],"row identities")
  LinkEDA:::.rls_handle_dimensionality_needed(make_task("factor",1:24,7,extraction="ml"))
  expect_identical(sent[6],"ok")
  fit <- LinkEDA:::.rls_dimension_record("race-factor")
  expect_identical(fit$extraction,"ml")
  expect_equal(fit$eigenvalues$eigenvalue,
    unname(psych::fa(cor(data[1:24,]),nfactors=1,fm="ml",rotate="none")$values),
    tolerance=1e-8)
  # The selected completion belongs to the request, not the current data-sheet tab.
  ds$dataset_type <- "multiple_imputation";ds$imputation_count <- 2L
  ds$completed_datasets <- list(data, transform(data,mpg=mpg+seq_len(nrow(data))/2))
  ds$original_data <- data;ds$active_imputation_version <- 1L
  LinkEDA:::.rls_set_dataset_record(ds)
  LinkEDA:::.rls_handle_dimensionality_needed(make_task("pca",1:24,6,imputation=2))
  expect_identical(sent[6],"ok")
  fit <- LinkEDA:::.rls_dimension_record("race-pca")
  expect_identical(fit$active_imputation_version,2L)
  expect_equal(fit$eigenvalues$eigenvalue,unname(stats::prcomp(ds$completed_datasets[[2]][1:24,],scale.=TRUE)$sdev^2))
})
