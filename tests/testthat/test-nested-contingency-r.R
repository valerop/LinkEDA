
.nested_eval <- function(record, data) {
  code <- LinkEDA:::.rls_mi_contingency_verification_r_code(record)
  expect_false(grepl("LinkEDA:::|\\.rls_",code))
  path <- tempfile(fileext=".rds");on.exit(unlink(path));saveRDS(data,path)
  e <- new.env(parent=globalenv());e$verification_data_path <- path
  invisible(eval(parse(text=code),e));e
}
test_that("nested tables calculate in R with complete-case denominators and original identities", {
  d <- data.frame(a=factor(c("B","A","A","B","A",NA),levels=c("B","A","Empty")),
                  b=factor(c("Y","X","Y","Y","X","Y")),
                  g=factor(c("T","C","T","C",NA,"T")))
  group <- ls_register_dataset("nested-r",d);ds <- LinkEDA:::.rls_dataset_record(group)
  for (mode in c("count","percent","count_percent")) for (split in c("g","")) {
    r <- LinkEDA:::.rls_contingency_build(ds,c("a","b"),split,"nested-r-result",mode)
    vars <- c("a","b",if(nzchar(split)) split)
    tab <- do.call(table,unname(d[complete.cases(d[,vars,drop=FALSE]),vars,drop=FALSE]))
    expect_equal(sum(r$mean_counts[-nrow(r$mean_counts),ncol(r$mean_counts)]),sum(tab))
    expect_equal(r$summary_statistics[[length(r$summary_statistics)]]$row_ids,which(complete.cases(d[,vars,drop=FALSE])))
    for(row in r$summary_statistics) {
      for(j in seq_along(row$cell_ids)) expect_equal(length(row$cell_ids[[j]]),unname(row$raw_statistics[[j]]$n))
      expect_false(anyNA(row$row_ids))
    }
    e <- .nested_eval(r,d)
    expect_equal(e$reference_counts,r$mean_counts)
    expect_equal(e$reference_row_percentages,r$mean_row_percentages)
    expect_equal(unname(as.matrix(e$reference_table[,-(1:2)])),unname(as.matrix(r$display_table[,-1])))
    expect_identical(r$summary_statistics[[1]]$row_key,c("B","X"))
    expect_false(any(grepl("imput",r$footnotes)))
  }
  h <- LinkEDA:::.rls_nested_contingency_record(group,c("a","b"),"g","nested-scoped",c(2L,4L,5L),"percent","My scope")
  r <- LinkEDA:::.rls_table1_record(h)
  expect_identical(r$data_scope$rows,c(2L,4L,5L))
  expect_identical(r$summary_statistics[[length(r$summary_statistics)]]$row_ids,c(2L,4L))
  expect_equal(r$analysis_provenance$dataset_version,ds$data_version)
  expect_match(r$analysis_provenance$executed_r_code,"rls_contingency_build")
  expect_error(LinkEDA:::.rls_contingency_build(ds,c("a","a"),"g","bad"),"distinct")
  expect_error(LinkEDA:::.rls_contingency_build(ds,"a","a","bad"),"distinct")
  expect_error(LinkEDA:::.rls_contingency_build(ds,"a","g","bad","invalid"))
  empty <- ds;empty$data <- d[5:6,]
  expect_error(LinkEDA:::.rls_contingency_build(empty,c("a","b"),"g","bad"),"No complete observations.*scope")
  wide <- ds;wide$data <- data.frame(a=1:400,b=1:400)
  expect_error(LinkEDA:::.rls_contingency_build(wide,"a","b","bad"),"too many")
})

test_that("nested native requests preserve modes, scope and reject invalid data versions", {
  d <- data.frame(a=factor(c("A","A","B","B","A",NA)),b=factor(c("F","M","F","M","F","M")),g=factor(c("X","Y","X","Y",NA,"X")))
  group <- ls_register_dataset("nested-transport",d);ds<-LinkEDA:::.rls_dataset_record(group)
  sent <- NULL;local_mocked_bindings(.rls_send=function(lines,...) {sent<<-lines;"OK"},.package="LinkEDA")
  request <- function(mode="count_percent",rows=1:5,version=ds$data_version)
    c("TABLE1_NEEDED","nested-result",group,"g","TRUE","FALSE","FALSE","TRUE","ordinal",
      "selected","Chosen cases","2","a","b","TYPES","0",length(rows),rows,
      "ANALYSIS_KIND_V1","nested_contingency","CONTINGENCY_OPTIONS_V1",mode,"REQUEST_V1","1",version)
  for(mode in c("count","percent","count_percent")) {
    LinkEDA:::.rls_handle_table1_needed(request(mode))
    expect_identical(sent[1],"TABLE1_OPEN_STRUCTURED")
    expect_true("CONTINGENCY_ROWS_V1" %in% sent)
    r <- LinkEDA:::.rls_table1_record("nested-result")
    expect_identical(r$contingency_display_mode,mode)
    expect_equal(r$summary_statistics[[length(r$summary_statistics)]]$row_ids,1:4)
    folder <- Sys.getenv("LINKEDA_NESTED_FIXTURES")
    if(nzchar(folder)) {
      dir.create(folder,recursive=TRUE,showWarnings=FALSE)
      writeLines(sent,file.path(folder,paste0(mode,".payload")))
      writeLines(c("REGISTER_DATASET",group,LinkEDA:::.rls_variable_payload(ds$data,ds$variable_metadata),
        LinkEDA:::.rls_dataframe_payload(ds$data,ds$variable_metadata,dataset_record=ds)),file.path(folder,"dataset.payload"))
    }
  }
  LinkEDA:::.rls_handle_table1_needed(request(version=999L));expect_identical(sent[1],"TABLE1_OPEN_ERROR");expect_match(sent[7],"data changed")
  LinkEDA:::.rls_handle_table1_needed(request(rows=integer()));expect_identical(sent[1],"TABLE1_OPEN_ERROR");expect_match(sent[7],"No complete observations")
  LinkEDA:::.rls_handle_table1_needed(request(mode="invalid"));expect_identical(sent[1],"TABLE1_OPEN_ERROR")
})

test_that("individual linear MI verification uses the recorded D3 or D1 test and actual residual df", {
  skip_if_not_installed("mice");skip_if_not_installed("gtsummary")
  d <- data.frame(y=1:12+rep(c(0,2,1),4),g=gl(3,4))
  sets <- list(d,transform(d,y=y+rep(c(1,0,-1),4)),transform(d,y=y+rep(c(0,2,-1),4)))
  for(kind in c("D3","D1","zero","intercept")) {
    s <- if(kind=="D3") lapply(sets,function(d)droplevels(d[1:8,])) else if(kind=="zero") rep(list(d),3) else sets
    predictors <- if(kind=="intercept") character() else "g"
    formula <- if(length(predictors)) y~g else y~1
    fits <- lapply(s,function(d)lm(formula,d));nulls <- lapply(s,function(d)lm(y~1,d))
    actual <- if(length(predictors)) LinkEDA:::.rls_mi_pool_d1(fits,nulls) else list(method="mice::D1")
    if(kind=="D3") expect_match(actual$method,"D3")
    r <- list(dependent="y",predictors=predictors,summary=list(global_test_method=actual$method))
    code <- LinkEDA:::.rls_linear_verification_r_code(r,multiple_imputation=TRUE)$code
    path <- tempfile(fileext=".rds");withr::defer(unlink(path))
    saveRDS(do.call(rbind,lapply(seq_along(s),function(i)transform(s[[i]],.imp=i,.id=seq_len(nrow(s[[i]]))))),path)
    e <- new.env(parent=globalenv());e$verification_data_path<-path
    invisible(capture.output(invisible(eval(parse(text=code),e))))
    expect_equal(e$reference_fit_metrics$df.residual,df.residual(fits[[1]]))
    if(length(predictors)) {
      expect_equal(e$reference_fit_metrics$p.value,actual$p,tolerance=1e-10)
      expect_equal(e$reference_fit_metrics$df.denominator,actual$df2,tolerance=1e-10)
    } else expect_true(is.na(e$reference_fit_metrics$p.value))
  }
})
