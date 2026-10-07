test_that("native Table 1 requests use R, frozen rows and versioned replies", {
  data <- transform(mtcars, am=factor(am), vs=factor(vs), gear=ordered(gear))
  group <- ls_register_dataset("table1-r",data)
  ds <- LinkEDA:::.rls_dataset_record(group)
  sent <- NULL
  local_mocked_bindings(.rls_send=function(lines,...) {sent <<- lines; "OK"},.package="LinkEDA")
  runtime <- LinkEDA:::.rls_state
  old <- runtime$process_started;withr::defer(runtime$process_started <- old)
  runtime$process_started <- TRUE
  request <- function(rows=NULL,revision=1L,version=ds$data_version,variables=c("mpg","vs","gear"))
    c("TABLE1_NEEDED","native-table",group,"am","TRUE","TRUE","TRUE","TRUE","ordinal",
      if(is.null(rows)) "all" else "selected",if(is.null(rows)) "All observations" else "Chosen cases",
      length(variables),variables,"TYPES","4","mpg","numeric","vs","categorical","gear","ordinal","am","categorical",
      length(rows),as.character(rows),"REQUEST_V1",revision,version)
  folder <- Sys.getenv("LINKEDA_TABLE1_FIXTURES")
  if(nzchar(folder)) {
    dir.create(folder,recursive=TRUE,showWarnings=FALSE)
    writeLines(c("REGISTER_DATASET",group,LinkEDA:::.rls_variable_payload(ds$data,ds$variable_metadata),
      LinkEDA:::.rls_dataframe_payload(ds$data,ds$variable_metadata,dataset_record=ds)),file.path(folder,"dataset.payload"))
  }
  for(i in 1:2) {
    rows <- if(i==1) NULL else c(1:12,25:32)
    LinkEDA:::.rls_handle_table1_needed(request(rows,i))
    expect_identical(sent[1:6],c("TABLE1_OPEN_STRUCTURED","native-table",group,"REQUEST_V1",as.character(i),as.character(ds$data_version)))
    rec <- LinkEDA:::.rls_table1_record("native-table")
    subset <- if(is.null(rows)) data else data[rows,]
    expect_identical(rec$data_scope$rows,if(is.null(rows)) integer() else as.integer(rows))
    mean_row <- Filter(function(x) identical(x$variable,"mpg") && identical(x$row_type,"numeric_mean_sd"),rec$summary_statistics)[[1]]
    expect_equal(mean_row$raw_statistics$Overall$mean,mean(subset$mpg))
    expect_equal(mean_row$raw_statistics$Overall$sd,sd(subset$mpg))
    expect_equal(mean_row$raw_statistics$Overall$se,sd(subset$mpg)/sqrt(nrow(subset)))
    expect_equal(rec$analysis_provenance$dataset_version,ds$data_version)
    expect_equal(rec$test_results$mpg$p,stats::t.test(mpg~am,subset)$p.value)
    expect_equal(rec$test_results$gear$p,stats::wilcox.test(as.numeric(gear)~am,subset,exact=FALSE)$p.value)
    association <- table(subset$vs,subset$am)
    chi <- suppressWarnings(stats::chisq.test(association,correct=FALSE))
    expected_p <- if(any(chi$expected<5)) stats::fisher.test(association)$p.value else chi$p.value
    expect_equal(rec$test_results$vs$p,expected_p)
    # Execute the public verification recipe; it must not need LinkEDA internals.
    recipe <- LinkEDA:::.rls_table1_verification_r_code(rec)$code
    expect_false(grepl("LinkEDA:::|\\.rls_",recipe))
    if(requireNamespace("gtsummary",quietly=TRUE)) {
      path <- tempfile(fileext=".rds");saveRDS(subset,path)
      env <- new.env(parent=globalenv());env$verification_data_path <- path
      invisible(eval(parse(text=recipe),env))
      expect_equal(unname(env$reference_group_n[["Overall"]]),nrow(subset))
    }
    if(nzchar(folder))writeLines(sent,file.path(folder,paste0("result-",i,".payload")))
  }
  LinkEDA:::.rls_handle_table1_needed(request(integer(),3))
  expect_identical(sent[1:6],c("TABLE1_OPEN_ERROR","native-table",group,"REQUEST_V1","3",as.character(ds$data_version)))
  expect_match(sent[7],"No rows")
  if(nzchar(folder))writeLines(sent,file.path(folder,"error.payload"))
  LinkEDA:::.rls_handle_table1_needed(request(NULL,4,999))
  expect_identical(sent[1],"TABLE1_OPEN_ERROR");expect_match(sent[7],"data changed")
  LinkEDA:::.rls_handle_table1_needed(request(c(1,999),5))
  expect_match(sent[7],"rows are invalid")
  LinkEDA:::.rls_handle_table1_needed(request(NULL,6,variables=character()))
  expect_identical(sent[1],"TABLE1_OPEN_STRUCTURED")
  expect_length(LinkEDA:::.rls_table1_record("native-table")$variables,0)
})


test_that("native Table 1 uses the exact R confidence interval for two observations", {
  group <- ls_register_dataset("table1-small",data.frame(x=c(1,3)))
  ds <- LinkEDA:::.rls_dataset_record(group);sent <- NULL
  local_mocked_bindings(.rls_send=function(lines,...) {sent <<- lines; "OK"},.package="LinkEDA")
  LinkEDA:::.rls_handle_table1_needed(c("TABLE1_NEEDED","small-table",group,"","TRUE","FALSE","FALSE","TRUE","ordinal",
    "all","All observations","1","x","TYPES","1","x","numeric","0","REQUEST_V1","1",ds$data_version))
  expect_identical(sent[1],"TABLE1_OPEN_STRUCTURED")
  rec <- LinkEDA:::.rls_table1_record("small-table")
  row <- Filter(function(x) identical(x$row_type,"numeric_mean_sd"),rec$summary_statistics)[[1]]
  expect_equal(unlist(row$raw_statistics$Overall[c("ci_lower","ci_upper")],use.names=FALSE),
    as.numeric(stats::t.test(c(1,3))$conf.int))
  expect_identical(row$statistics$Overall$ci95,"[-10.7, 14.7]")
})

test_that("native versioned Table 1 still pools every MI completion", {
  data <- transform(mtcars,am=factor(am))
  group <- ls_register_dataset("table1-mi-request",data)
  ds <- LinkEDA:::.rls_dataset_record(group)
  ds$dataset_type <- "multiple_imputation";ds$imputation_count <- 2L
  ds$completed_datasets <- list(data,transform(data,mpg=mpg+seq_len(nrow(data))/10))
  ds$original_data <- data;ds$active_imputation_version <- 1L
  LinkEDA:::.rls_set_dataset_record(ds);sent <- NULL
  local_mocked_bindings(.rls_send=function(lines,...) {sent <<- lines; "OK"},.package="LinkEDA")
  LinkEDA:::.rls_handle_table1_needed(c("TABLE1_NEEDED","mi-request",group,"am","TRUE","TRUE","TRUE","TRUE","ordinal",
    "selected","Chosen cases","1","mpg","TYPES","1","mpg","numeric","24",as.character(1:24),"REQUEST_V1","1",ds$data_version))
  expect_identical(sent[1],"TABLE1_OPEN_STRUCTURED")
  rec <- LinkEDA:::.rls_table1_record("mi-request")
  expect_identical(rec$analysis_backend,"multiple_imputation")
  expect_identical(rec$data_scope$rows,1:24)
  row <- Filter(function(x) identical(x$row_type,"numeric_mean_sd"),rec$summary_statistics)[[1]]
  expect_equal(row$raw_statistics$Overall$mean,mean(vapply(ds$completed_datasets,function(d) mean(d$mpg[1:24]),numeric(1))))
})
