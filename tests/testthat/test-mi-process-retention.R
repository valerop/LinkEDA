.mi_sync_lines <- function(record, process="") {
  data <- record$completed_datasets[[1L]]
  lines <- c("DATASET",record$group,nrow(data),ncol(data))
  for (name in names(data)) lines <- c(lines,name,
    if(is.numeric(data[[name]]))"numeric" else "factor",as.character(data[[name]]))
  columns <- names(data)[vapply(record$original_data,anyNA,logical(1L))]
  lines <- c(lines,"IMPUTATION_SPARSE","multiple_imputation",record$imputation_id,
    record$source_dataset_id,length(record$completed_datasets),"1","version",length(columns))
  for (name in columns) {
    rows <- which(is.na(record$original_data[[name]]))
    lines <- c(lines,name,length(rows),rows,rep("NA",length(rows)))
    for (data in record$completed_datasets) lines <- c(lines,as.character(data[[name]][rows]))
  }
  if(nzchar(process)) lines <- c(lines,"IMPUTATION_PROCESS_V1",process)
  lines
}

test_that("LinkEDA imputation retains diagnostics through native synchronization", {
  skip_if_not_installed("mice")
  source <- LinkEDA:::.rls_register_dataset("mi_process_source",mice::nhanes,activate=FALSE)
  on.exit(ls_unregister_dataset(source),add=TRUE)
  imputation <- ls_new_missing_data_imputation(source,m=3,maxit=6,seed=31)
  run <- ls_run_imputation(imputation,open_data=FALSE,make_active=FALSE)
  id <- run$dataset_id; on.exit(ls_unregister_dataset(id),add=TRUE)
  record <- LinkEDA:::.rls_dataset_record(id)
  original <- record$mids_object
  folder <- tempfile("rlispstat-sync-"); dir.create(folder)
  on.exit(unlink(folder,recursive=TRUE),add=TRUE)
  payload <- file.path(folder,"data.txt")
  writeLines(.mi_sync_lines(record),payload)
  testthat::local_mocked_bindings(.rls_send=function(...)"OK",.package="LinkEDA")
  expect_true(LinkEDA:::.rls_handle_r_dataset_sync_needed(c("R_DATASET_SYNC_NEEDED","test",id,payload)))
  restored <- LinkEDA:::.rls_dataset_record(id)
  expect_identical(restored$mids_object,original)
  expect_no_error(ls_imputation_diagnostics(id,native=FALSE))
  restored$mids_object <- NULL
  expect_identical(LinkEDA:::.rls_mi_restore_process(restored)$mids_object,original)
  # Changing an imputed value must not retain the old process as current.
  restored$completed_datasets[[1]]$bmi[1] <- 999
  expect_null(LinkEDA:::.rls_mi_restore_process(restored)$mids_object)
})

test_that("the native process payload restores diagnostics in a fresh registry", {
  skip_if_not_installed("mice")
  imp <- mice::mice(mice::nhanes,m=3,maxit=6,seed=31,printFlag=FALSE)
  id <- ls_import_mice(imp,name="mi_process_roundtrip",make_active=FALSE)
  record <- LinkEDA:::.rls_dataset_record(id)
  encoded <- LinkEDA:::.rls_mi_encode_process(imp,record$import_name_map)
  expect_identical(LinkEDA:::.rls_mi_decode_process(encoded)$mids,imp)
  expect_error(LinkEDA:::.rls_mi_decode_process("invalid"),"Invalid retained")
  expect_true("IMPUTATION_PROCESS_V1" %in% LinkEDA:::.rls_dataframe_payload(record$data,record$variable_metadata,dataset_record=record))
  lines <- .mi_sync_lines(record,encoded)
  ls_unregister_dataset(id)
  on.exit(ls_unregister_dataset(id),add=TRUE)
  folder <- tempfile("rlispstat-sync-");dir.create(folder)
  on.exit(unlink(folder,recursive=TRUE),add=TRUE)
  payload <- file.path(folder,"data.txt");writeLines(lines,payload)
  testthat::local_mocked_bindings(.rls_send=function(...)"OK",.package="LinkEDA")
  expect_true(LinkEDA:::.rls_handle_r_dataset_sync_needed(c("R_DATASET_SYNC_NEEDED","test",id,payload)))
  restored <- LinkEDA:::.rls_dataset_record(id)
  expect_identical(restored$mids_object,imp)
  info <- ls_imputation_diagnostics(id,native=FALSE)$info
  expect_identical(info$chain_mean,imp$chainMean)
  expect_identical(info$logged_events,imp$loggedEvents)
  expect_identical(info$predictor_matrix,imp$predictorMatrix)
  for (section in c("model","events","chain_mean","chain_variance","convergence","distributions"))
    expect_no_error(ls_imputation_diagnostics(id,section=section,variable="bmi",native=FALSE))
})
