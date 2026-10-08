test_that("ordinary correlations use cor.test with exact pairwise and listwise rows", {
  data <- data.frame(x=c(1,2,3,4,5,6),y=c(2,5,4,9,10,7),z=c(NA,2,4,NA,3,8))
  for (missing in c("pairwise","listwise")) {
    actual <- LinkEDA:::.rls_correlation_compute(data,names(data),"pearson",missing)
    for(i in seq_len(nrow(actual))) {
      a <- actual$x_variable[i]; b <- actual$y_variable[i]
      cols <- if(missing=="listwise") names(data) else unique(c(a,b))
      rows <- which(complete.cases(data[,cols,drop=FALSE]))
      expect_identical(actual$rows_used_original_ids[[i]],rows)
      expect_equal(actual$n[i],length(rows))
      if(a!=b) {
        ref <- stats::cor.test(data[[a]][rows],data[[b]][rows])
        expect_equal(actual$r[i],unname(ref$estimate));expect_equal(actual$p[i],ref$p.value)
      }
    }
  }
  for (y in list(rep(1,6),c(1,Inf,2,3,4,5),rep(NA_real_,6))) {
    bad <- LinkEDA:::.rls_correlation_cell(data.frame(x=1:6,y=y),c("x","y"),"x","y","pairwise")
    expect_false(bad$status=="valid");expect_true(is.na(bad$r));expect_true(is.na(bad$p))
  }
  perfect <- LinkEDA:::.rls_correlation_cell(data.frame(x=1:6,y=1:6),c("x","y"),"x","y","pairwise")
  expect_equal(perfect$r,1);expect_equal(perfect$p,0)
})

test_that("MI Fisher-z pooling matches mice without clipping or dropping imputations", {
  for (r in list(c(.3,.5,.7),rep(.3,3),c(.9999999,.9999998,.9999997))) {
    n <- c(40,50,60)
    actual <- LinkEDA:::.rls_mi_pool_correlation(r,n)
    ref <- mice::pool.scalar(atanh(r),1/(n-3),n=Inf,k=1,rule="rubin1987")
    expect_equal(actual$z,atanh(r));expect_equal(actual$pool$Qbar,ref$qbar)
    expect_equal(actual$pool$T,ref$t);expect_equal(actual$pool$df,ref$df)
    expect_equal(actual$pool$FMI,ref$fmi);expect_equal(actual$pool$RIV,ref$r)
    expect_equal(actual$pool$p,2*pt(-abs(ref$qbar/sqrt(ref$t)),df=ref$df))
    expect_equal(actual$pool$CI_low,ref$qbar-qt(.975,ref$df)*sqrt(ref$t))
  }
  for (r in list(c(.4,1),c(.4,-1),c(.4,NA),c(.4,Inf),.4)) {
    actual <- LinkEDA:::.rls_mi_pool_correlation(r,rep(40,length(r)))
    expect_false(actual$pool$valid);expect_true(is.na(actual$pool$p))
    expect_true(nzchar(actual$pool$reason))
  }
  expect_false(LinkEDA:::.rls_mi_pool_correlation(c(.4,.5),c(3,50))$pool$valid)
})

test_that("ordinary native payload contains results and a frozen executable public recipe", {
  input <- data.frame(x=c(1,2,3,4,5,6),y=c(2,5,4,9,10,7),z=c(NA,2,4,NA,3,8))
  handle <- ls_new_correlation_matrix(input,variables=names(input),native=FALSE)
  state <- ls_correlation_matrix_state(handle)
  code <- state$analysis_provenance$verification_r_code$table
  expect_match(code,"stats::cor.test",fixed=TRUE);expect_false(grepl("LinkEDA:::|.rls_",code))
  file <- tempfile();on.exit(unlink(file),add=TRUE);saveRDS(input,file)
  env <- new.env(parent=baseenv());env$verification_data_path <- file
  eval(parse(text=code),env)
  expect_equal(env$reference_correlations[[2]][[1]][['r']],state$results$r[4])
  expect_equal(env$reference_correlations[[2]][[1]][['p']],state$results$p[4])
  payload <- LinkEDA:::.rls_correlation_native_payload(state)
  expect_equal(payload[1],"CORR_OPEN_STRUCTURED");expect_true("ANALYSIS_PROVENANCE_V2" %in% payload)
  expect_identical(state$analysis_provenance,ls_correlation_matrix_state(handle)$analysis_provenance)
})

test_that("native correlation requests freeze scope, send terminal errors and preserve empty scopes", {
  input <- data.frame(x=c(1,2,3,4,5,6),y=c(2,5,4,9,10,7))
  ls_register_dataset("corr-wire",input)
  ds <- LinkEDA:::.rls_dataset_record("corr-wire");sent <- NULL
  local_mocked_bindings(.rls_send=function(x,...) { sent <<- x; "OK" },.package="LinkEDA")
  request <- as.character(c("CORRELATION_NEEDED","corr-wire-matrix","corr-wire","7",ds$data_version,
    "selected","pearson","pairwise","2","x","y","4","1","3","5","6"))
  result <- LinkEDA:::.rls_handle_correlation_needed(request)
  cell <- result$results[2,];ref <- cor.test(input$x[c(1,3,5,6)],input$y[c(1,3,5,6)])
  expect_equal(cell$r,unname(ref$estimate));expect_identical(cell$rows_used_original_ids[[1]],c(1L,3L,5L,6L))
  expect_equal(sent[1:5],c("CORR_UPDATE","corr-wire-matrix","7",as.character(ds$data_version),"ok"))
  expect_true("ANALYSIS_PROVENANCE_V2" %in% sent)
  empty <- c(request[1:11],"0");result <- LinkEDA:::.rls_handle_correlation_needed(empty)
  expect_equal(sent[5],"ok");expect_true(all(result$results$n==0L));expect_length(result$data_scope$rows,0)
  stale <- request;stale[5] <- "999999";LinkEDA:::.rls_handle_correlation_needed(stale)
  expect_equal(sent[5],"error");expect_match(sent[6],"data changed")
})

test_that("MI matrices and verification include all completed datasets", {
  input <- data.frame(x=c(1,2,NA,4,5,6,7,8),y=c(2,4,3,NA,8,7,10,9))
  imp <- mice::mice(input,m=3,maxit=1,printFlag=FALSE,seed=73)
  imp$imp$x[,1] <- 3;imp$imp$x[,2] <- 5;imp$imp$x[,3] <- 1
  imp$imp$y[,1] <- 5;imp$imp$y[,2] <- 6;imp$imp$y[,3] <- 7
  LinkEDA:::.rls_register_mids_dataset(imp,"corr-mi",make_active=FALSE)
  handle <- ls_new_correlation_matrix("corr-mi",variables=c("x","y"),native=FALSE)
  state <- ls_correlation_matrix_state(handle);cell <- state$results[2,]
  completed <- mice::complete(imp,"all");r <- vapply(completed,function(data)unname(cor.test(data$x,data$y)$estimate),numeric(1))
  ref <- mice::pool.scalar(atanh(r),rep(1/5,3))
  expect_equal(cell$r,tanh(ref$qbar));expect_equal(cell$pooled_SE,sqrt(ref$t));expect_equal(cell$pooled_df,ref$df)
  expect_equal(cell$p,2*pt(-abs(ref$qbar/sqrt(ref$t)),ref$df))
  expect_identical(cell$rows_used_original_ids[[1]],1:8)
  code <- state$analysis_provenance$verification_r_code$table
  expect_false(grepl(".999999|LinkEDA:::|.rls_",code))
  file <- tempfile();on.exit(unlink(file),add=TRUE);saveRDS(mice::complete(imp,"long"),file)
  env <- new.env(parent=baseenv());env$verification_data_path <- file;eval(parse(text=code),env)
  expect_equal(env$reference_correlations[[1]][[2]]$r,cell$r)
  expect_equal(env$reference_correlations[[1]][[2]]$p,cell$p)
  sent <- NULL;local_mocked_bindings(.rls_send=function(x,...) {sent <<- x;"OK"},.package="LinkEDA")
  ds <- LinkEDA:::.rls_dataset_record("corr-mi")
  request <- as.character(c("CORRELATION_NEEDED","corr-mi-wire","corr-mi","2",ds$data_version,"all","pearson","pairwise","2","x","y","0"))
  queued <- LinkEDA:::.rls_handle_correlation_needed(request)
  expect_equal(queued$results$r,state$results$r);expect_equal(sent[5],"ok")
})


test_that("R correlation tables honor stars, exact p and N without changing results", {
  input <- data.frame(x=1:6, y=c(1, 2, 3, 4, 6, 5))
  handle <- ls_new_correlation_matrix(input, variables=names(input), native=FALSE,
    show_significance_stars=TRUE, show_p_value=TRUE, show_n=TRUE)
  state <- ls_correlation_matrix_state(handle)
  table <- LinkEDA:::.rls_correlation_table(handle)
  cell <- state$results[state$results$x_variable == "x" & state$results$y_variable == "y", ]
  expected <- paste0(LinkEDA:::.rls_correlation_format_r(cell$r), LinkEDA:::.rls_export_stars(cell$p))
  expect_identical(strsplit(table$matrix$x[2], "\n")[[1]][1], expected)
  expect_match(table$matrix$x[2], "p=", fixed=TRUE)
  expect_match(table$matrix$x[2], "N = 6", fixed=TRUE)
  expect_identical(table$matrix$x[1], "—")
  expect_equal(ls_correlation_matrix_cells(handle), state$results)
  hidden <- ls_new_correlation_matrix(input, variables=names(input), native=FALSE,
    show_significance_stars=FALSE, show_p_value=FALSE, show_n=FALSE)
  hidden_table <- LinkEDA:::.rls_correlation_table(hidden)
  expect_identical(hidden_table$matrix$x[2], LinkEDA:::.rls_correlation_format_r(cell$r))
})
