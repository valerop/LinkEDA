.mi_diagnostic_fixture <- function(name="mi_diagnostic_test", maxit=2L, data=mice::nhanes) {
  skip_if_not_installed("mice")
  imp <- mice::mice(data, m=3, maxit=maxit, seed=91, printFlag=FALSE)
  id <- ls_import_mice(imp, name=name, make_active=FALSE)
  list(imp=imp,id=id)
}

test_that("only mids enters the imported MI workflow", {
  f <- .mi_diagnostic_fixture("mi_import_policy")
  on.exit(ls_unregister_dataset(f$id),add=TRUE)
  expect_identical(ls_analysis_backend(f$id)$backend,"multiple_imputation")
  path <- tempfile(fileext=".rds");on.exit(unlink(path),add=TRUE)
  saveRDS(mice::complete(f$imp,"all"),path)
  expect_error(ls_import_rds(path,make_active=FALSE),"Multiple-imputation data require a `mids` object")
  long <- mice::complete(f$imp,"long",include=TRUE)
  for (data in list(long,long[long$.imp>0,,drop=FALSE])) {
    saveRDS(data,path)
    expect_message(id<-ls_import_rds(path,name="stacked_ordinary",make_active=FALSE),"Stacked multiple imputations detected")
    expect_identical(ls_analysis_backend(id)$backend,"ordinary")
    expect_error(ls_imputation_diagnostics(id,native=FALSE),"original mice::mids")
    expect_match(LinkEDA:::.rls_dataset_record(id)$import_notice,"pooling and imputation diagnostics are disabled")
    ls_unregister_dataset(id)
  }
})

test_that("dataset diagnostics use original missingness and retained mids metadata", {
  f <- .mi_diagnostic_fixture()
  on.exit(ls_unregister_dataset(f$id),add=TRUE)
  out <- ls_imputation_diagnostics(f$id,native=FALSE)
  info <- out$info
  expect_equal(info$missingness_by_variable$Missing,colSums(is.na(f$imp$data)),ignore_attr=TRUE)
  expect_equal(info$missingness_by_variable$`Missing %`[info$missingness_by_variable$Variable=="age"],0)
  expect_equal(sum(info$missingness_patterns$Cases),nrow(f$imp$data))
  expect_identical(info$methods,f$imp$method)
  expect_identical(info$predictor_matrix,f$imp$predictorMatrix)
  expect_identical(info$chain_mean,f$imp$chainMean)
  expect_identical(info$where,f$imp$where)
  expect_identical(out$table$display_table$Value[1], "25")
  expect_true(any(grepl("original data before imputation", out$table$footnotes)))
  other <- .mi_diagnostic_fixture("different_active_imputed_dataset", data=mice::nhanes[1:20,])
  on.exit(ls_unregister_dataset(other$id), add=TRUE)
  ls_set_active_dataset(other$id)
  requested <- ls_imputation_diagnostics(f$id,native=FALSE)
  expect_identical(requested$table$dataset_id, f$id)
  expect_identical(requested$info$original_data, info$original_data)
  for(section in c("summary","variables","patterns","model","events","chain_mean","chain_variance","distributions")) {
    result<-ls_imputation_diagnostics(f$id,section=section,variable="bmi",native=FALSE)
    payload<-LinkEDA:::.rls_table1_native_payload(result$table)
    expect_true("MI_DIAGNOSTICS_V1" %in% payload)
    expect_true("mi_diagnostics" %in% payload)
    code<-result$table$analysis_provenance$verification_r_code$table
    expect_false(grepl("LinkEDA:::",code,fixed=TRUE))
    replay<-new.env(parent=globalenv())
    replay$verification_data_path<-result$table$analysis_provenance$prepared_data_path
    grDevices::pdf(tempfile(fileext=".pdf"))
    expect_error(invisible(capture.output(eval(parse(text=code),replay))),NA)
    grDevices::dev.off()
  }
  truncated<-ls_imputation_diagnostics(f$id,section="patterns",max_patterns=2,native=FALSE)
  expect_equal(nrow(truncated$table$display_table),2)
  expect_true(any(grepl("Showing 2",truncated$table$footnotes)))
})

test_that("logged events and missing convergence metadata remain explicit", {
  f <- .mi_diagnostic_fixture("mi_diag_events")
  on.exit(ls_unregister_dataset(f$id),add=TRUE)
  record<-LinkEDA:::.rls_dataset_record(f$id)
  record$mids_object$loggedEvents<-data.frame(it=1,im=1,dep="bmi",meth="pmm",out="collinear")
  record$mids_object$chainMean<-NULL;record$mids_object$chainVar<-NULL
  LinkEDA:::.rls_set_dataset_record(record)
  result<-ls_imputation_diagnostics(f$id,section="events",native=FALSE)
  expect_true(any(grepl("collinear",unlist(result$table$display_table))))
  result<-ls_imputation_diagnostics(f$id,section="summary",native=FALSE)
  expect_true(any(grepl("logged 1 event",result$table$footnotes)))
  result<-ls_imputation_diagnostics(f$id,section="chain_mean",variable="bmi",native=FALSE)
  expect_equal(nrow(result$plot_data),0)
  expect_true(any(grepl("Convergence information unavailable",result$table$footnotes)))
})

test_that("shared missing information agrees with mice without changing estimates", {
  f <- .mi_diagnostic_fixture("mi_diag_pool")
  on.exit(ls_unregister_dataset(f$id),add=TRUE)
  model<-ls_new_glm(f$id);ls_glm_set_dependent(model,"bmi")
  ls_glm_add_predictor(model,"age");ls_glm_add_predictor(model,"hyp");ls_glm_fit(model)
  record<-LinkEDA:::.rls_glm_model_record(model)
  before<-record$coefficients
  diagnostic<-ls_missing_information_diagnostics(model)
  reference<-mice::pool(mice::as.mira(lapply(seq_len(f$imp$m),function(i)lm(bmi~age+hyp,mice::complete(f$imp,i)))))$pooled
  expect_equal(diagnostic$FMI,reference$fmi,tolerance=1e-10)
  expect_equal(diagnostic$RIV,reference$riv,tolerance=1e-10)
  expect_equal(diagnostic$Ubar,reference$ubar,tolerance=1e-10)
  expect_equal(diagnostic$B,reference$b,tolerance=1e-10)
  expect_equal(diagnostic$T,reference$t,tolerance=1e-10)
  expect_equal(diagnostic$`MCSE (estimate)`,sqrt(reference$b/reference$m),tolerance=1e-10)
  expect_equal(diagnostic$`MCSE / SE (%)`,100*sqrt(reference$b/reference$m/reference$t),tolerance=1e-10)
  expect_equal(diagnostic$m,reference$m)
  expect_identical(LinkEDA:::.rls_glm_model_record(model)$coefficients,before)
  expect_equal(nrow(record$analysis_provenance$missing_information),nrow(reference))
  scalar<-LinkEDA:::.rls_mi_pool_scalar(c(1,2,3),c(.5,.8,.7),df_complete=25)
  normalized<-LinkEDA:::.rls_mi_pooling_diagnostics(scalar,3)
  reference<-mice::pool.scalar(c(1,2,3),c(.5,.8,.7),n=26,k=1)
  expect_equal(normalized$FMI,reference$fmi)
  expect_equal(normalized$RIV,reference$r)
  expect_equal(normalized$`MCSE (estimate)`,sqrt(reference$b/reference$m))
})

test_that("every pooled regression family exposes the shared diagnostics", {
  for(package in c("mice","broom","glmmTMB","gamlss","betareg"))skip_if_not_installed(package)
  set.seed(219)
  n<-240; x<-rnorm(n); g<-factor(rep(c("A","B"),length.out=n))
  p<-plogis(.3+.4*x)
  data<-data.frame(x=x,g=g,trials=10, count=rbinom(n,10,rbeta(n,4*p,4*(1-p))),
    binary=factor(rbinom(n,1,p)), positive=exp(.2+.3*x+rnorm(n,.0,.5)),
    proportion=rbeta(n,3,4),inflated=rbeta(n,3,4), y=.4*x+rnorm(n))
  data$count[1:12]<-10;data$inflated[1:12]<-1;data$x[13:30]<-NA
  imp<-mice::mice(data,m=2,maxit=2,printFlag=FALSE,seed=44)
  id<-ls_import_mice(imp,name="all_family_diagnostics",make_active=FALSE)
  on.exit(ls_unregister_dataset(id),add=TRUE)
  check<-function(model) {
    record<-ls_generalized_linear_model_state(model)
    diagnostic<-ls_missing_information_diagnostics(record)
    expect_gt(nrow(diagnostic),0)
    expect_true(all(c("FMI","RIV","Ubar","B","T","lambda","Relative efficiency","MCSE (estimate)","MCSE / SE (%)") %in% names(diagnostic)))
    expect_equal(diagnostic$`MCSE (estimate)`,sqrt(diagnostic$B/2),tolerance=1e-10)
    expect_equal(diagnostic$`MCSE / SE (%)`,100*sqrt(diagnostic$B/2)/diagnostic$SE,tolerance=1e-10)
    expect_identical(record$analysis_provenance$missing_information,diagnostic)
    display <- record$analysis_provenance$missing_information_display_rows
    if (is.data.frame(record$coefficient_rows) && "coefficient_name" %in% names(record$coefficient_rows)) {
      expect_true(is.data.frame(display),info=paste(record$family,record$count_distribution,
        paste(names(record$coefficient_rows),collapse=","),paste(diagnostic$Variable,collapse=",")))
      if (!is.data.frame(display)) return(invisible(NULL))
      expect_setequal(display$value_row[display$value_row>=0],seq_len(nrow(diagnostic))-1L)
      expect_true(all(display$label[display$row_type=="factor_level"] != "gB"))
    }
    expect_match(LinkEDA:::.rls_mi_status_note(record),"Multiple imputation: 2 imputations combined")
  }
  for(distribution in c("poisson","quasipoisson","negative_binomial","binomial_trials",
    "beta_binomial","hurdle_beta_binomial_ceiling","perfect_score")) {
    invisible(capture.output(model<-ls_new_count_regression(id,"count",c("x","g"),
      distribution=distribution,trials=if(distribution %in% c("poisson","quasipoisson","negative_binomial"))NULL else "trials",native=FALSE)))
    check(model)
  }
  for(link in c("logit","probit","cloglog","log"))check(ls_new_binary_regression(
    id,"binary",if(link=="log")character() else c("x","g"),event="1",reference="0",link=link,native=FALSE))
  for(distribution in c("beta","beta_one_inflated")) {
    invisible(capture.output(model<-ls_new_proportion_model(id,
      if(distribution=="beta")"proportion" else "inflated",c("x","g"),distribution=distribution,native=FALSE)))
    check(model)
  }
  check(ls_new_generalized_linear_model(id,"y",c("x","g"),family="gaussian",link="identity",native=FALSE))
  for(family in c("gaussian_log","Gamma","inverse.gaussian","lognormal"))check(ls_new_positive_continuous_model(
    id,"positive",c("x","g"),distribution=family,native=FALSE))
})

test_that("exporting unmodified mids retains process metadata", {
  f<-.mi_diagnostic_fixture("mi_diag_export");on.exit(ls_unregister_dataset(f$id),add=TRUE)
  saved<-LinkEDA:::.rls_export_dataset_mids(LinkEDA:::.rls_dataset_record(f$id))
  for(field in c("method","predictorMatrix","where","loggedEvents","chainMean","chainVar","seed","iteration"))
    expect_identical(saved[[field]],f$imp[[field]])
})

test_that("correlation diagnostics retain the Fisher-z scale and public recipe", {
  f<-.mi_diagnostic_fixture("mi_diag_corr");on.exit(ls_unregister_dataset(f$id),add=TRUE)
  record<-list(id="mi_corr",group=f$id,data=mice::complete(f$imp,1),variables=c("bmi","chl"),method="pearson",
    missing_mode="pairwise",model_version=0,analysis_backend="multiple_imputation")
  record<-LinkEDA:::.rls_correlation_refit(record)
  result<-ls_missing_information_diagnostics(record)
  expect_equal(nrow(result),1)
  expect_match(result$Variable,"Fisher z")
  expect_equal(result$Estimate,record$results$pooled_z[record$results$status=="valid"][1])
  z <- vapply(seq_len(f$imp$m), function(i) {
    data <- mice::complete(f$imp,i)
    atanh(cor(data$bmi,data$chl))
  },numeric(1L))
  expect_equal(result$`MCSE (estimate)`,sd(z)/sqrt(length(z)))
  expect_true("MI_DIAGNOSTICS_V1" %in% LinkEDA:::.rls_correlation_native_payload(record))
})

test_that("pooled pairwise diagnostics reproduce covariance and public R code", {
  f<-.mi_diagnostic_fixture("mi_diag_contrasts");on.exit(ls_unregister_dataset(f$id),add=TRUE)
  ls_set_variable_type(f$id,"age","factor")
  model<-ls_new_glm(f$id);ls_glm_set_dependent(model,"bmi")
  ls_glm_add_predictor(model,"age")
  ls_glm_add_predictor(model,"chl");ls_glm_fit(model)
  record<-LinkEDA:::.rls_glm_model_record(model)
  result<-ls_glm_pairwise(model,"age")
  diagnostics<-ls_missing_information_diagnostics(result)
  expect_equal(nrow(diagnostics),nrow(result))
  expect_equal(diagnostics$T,result$pooled_std_error^2,tolerance=1e-9)
  expect_equal(diagnostics$T,diagnostics$Ubar+(1+1/f$imp$m)*diagnostics$B,tolerance=1e-9)
  payload <- LinkEDA:::.rls_pairwise_native_payload(result)
  expect_true("MI_DIAGNOSTICS_V1" %in% payload)
  provenance <- attr(result,"analysis_provenance")
  display <- provenance$missing_information_display_rows
  expect_identical(display$value_row,seq_len(nrow(diagnostics))-1L)
  expect_identical(display$label,diagnostics$Variable)
  expect_true(all(display$variable=="age"))
  expect_identical(record$analysis_provenance$missing_information_display_rows,
    LinkEDA:::.rls_glm_model_record(model)$analysis_provenance$missing_information_display_rows)
  # Check the actual native wire mapping, including zero-based indices.
  offset <- match("MI_DIAGNOSTIC_ROWS_V1",payload)
  expect_false(is.na(offset))
  expect_equal(as.integer(payload[offset+1L]),nrow(diagnostics))
  rows <- matrix(payload[seq.int(offset+2L,length(payload))],ncol=4,byrow=TRUE)
  expect_equal(as.integer(rows[,1L]),seq_len(nrow(diagnostics))-1L)
  expect_identical(rows[,3L],diagnostics$Variable)
  path<-tempfile(fileext=".rds");on.exit(unlink(path),add=TRUE)
  saveRDS(LinkEDA:::.rls_export_dataset_data(LinkEDA:::.rls_dataset_record(f$id)),path)
  env<-new.env(parent=globalenv());env$verification_data_path<-path
  code<-attr(result,"analysis_provenance")$verification_r_code$pairwise
  expect_false(grepl("LinkEDA:::",code,fixed=TRUE))
  invisible(capture.output(eval(parse(text=code),env)))
  expect_equal(env$fmi,diagnostics$FMI,tolerance=1e-9)
  expect_equal(env$riv,diagnostics$RIV,tolerance=1e-9)
  expect_equal(env$mcse,diagnostics$`MCSE (estimate)`,tolerance=1e-9)
  expect_equal(env$mcse_percent_se,diagnostics$`MCSE / SE (%)`,tolerance=1e-9)
})

test_that("MCSE handles zero and unavailable variance without false precision", {
  pool <- LinkEDA:::.rls_mi_pool_scalar(rep(2,3),rep(1,3),df_complete=25)
  # The scalar pool's own imputation count is authoritative.
  out <- LinkEDA:::.rls_mi_pooling_diagnostics(pool,99)
  expect_equal(out$m,3)
  expect_equal(out$`MCSE (estimate)`,0)
  expect_equal(out$`MCSE / SE (%)`,0)
  for (b in c(NA_real_,-1,Inf)) {
    pool$B <- b
    expect_no_warning(out <- LinkEDA:::.rls_mi_pooling_diagnostics(pool,3))
    expect_true(is.na(out$`MCSE (estimate)`))
  }
  pool$B <- 0; pool$SE <- 0
  expect_true(is.na(LinkEDA:::.rls_mi_pooling_diagnostics(pool,3)$`MCSE / SE (%)`))
})

test_that("linear MCSE verification recomputes from the frozen analysis inputs", {
  f <- .mi_diagnostic_fixture("mi_mcse_recipe")
  on.exit(ls_unregister_dataset(f$id),add=TRUE)
  model <- ls_new_glm(f$id); ls_glm_set_dependent(model,"bmi")
  ls_glm_add_predictor(model,"age"); ls_glm_fit(model)
  record <- LinkEDA:::.rls_glm_model_record(model)
  path <- tempfile(fileext=".rds"); on.exit(unlink(path),add=TRUE)
  saveRDS(LinkEDA:::.rls_export_dataset_data(LinkEDA:::.rls_dataset_record(f$id)),path)
  env <- new.env(parent=globalenv()); env$verification_data_path <- path
  code <- record$analysis_provenance$verification_r_code$model
  expect_false(grepl("LinkEDA:::",code,fixed=TRUE))
  invisible(capture.output(eval(parse(text=code),env)))
  diagnostics <- ls_missing_information_diagnostics(model)
  expect_equal(env$missing_information$mcse,diagnostics$`MCSE (estimate)`)
  expect_equal(env$missing_information$mcse_percent_se,diagnostics$`MCSE / SE (%)`)
})

test_that("convergence summary agrees with coda and its public verification recipe", {
  skip_if_not_installed("coda")
  f <- .mi_diagnostic_fixture("mi_convergence",maxit=12L)
  on.exit(ls_unregister_dataset(f$id),add=TRUE)
  result <- ls_imputation_diagnostics(f$id,section="convergence",native=FALSE)
  actual <- LinkEDA:::.rls_mi_convergence_data(result$info)
  for(statistic in c("Mean","SD")) {
    values <- if(statistic=="Mean") f$imp$chainMean["bmi",,] else sqrt(f$imp$chainVar["bmi",,])
    chains <- coda::mcmc.list(lapply(seq_len(ncol(values)),function(i)coda::mcmc(values[,i])))
    psrf <- coda::gelman.diag(chains,confidence=.95,transform=FALSE,autoburnin=FALSE,multivariate=FALSE)$psrf[1,]
    row <- actual[actual$Variable=="bmi" & actual$Statistic==statistic,]
    expect_equal(row$`PSRF (classical)`,unname(psrf[1]))
    expect_equal(row$`Lag-1 autocorrelation`,mean(vapply(chains,function(x)as.numeric(coda::autocorr.diag(x,lags=1)),numeric(1))))
  }
  env <- new.env(parent=globalenv())
  env$verification_data_path <- result$table$analysis_provenance$prepared_data_path
  code <- result$table$analysis_provenance$verification_r_code$table
  expect_false(grepl("LinkEDA:::",code,fixed=TRUE))
  invisible(capture.output(eval(parse(text=code),env)))
  for (row in env$convergence) {
    expected <- actual[actual$Variable==row$variable & actual$Statistic==row$statistic,]
    expect_equal(unname(row$PSRF),expected$`PSRF (classical)`)
    expect_equal(unname(row$ac),expected$`Lag-1 autocorrelation`)
  }
  expect_true(any(grepl("not rank-normalized",result$table$footnotes)))
  expect_true("mi_diagnostics" %in% LinkEDA:::.rls_table1_native_payload(result$table))
  info <- result$info
  info$chain_mean["bmi",,] <- 1
  expect_match(LinkEDA:::.rls_mi_convergence_data(info)$Status[3],"Constant chain")
  info$chain_mean <- NULL; info$chain_var <- NULL
  expect_true(all(grepl("unavailable",LinkEDA:::.rls_mi_convergence_data(info)$Status)))
  info <- result$info; info$chain_mean <- info$chain_mean[,1:2,,drop=FALSE]
  expect_match(LinkEDA:::.rls_mi_convergence_data(info)$Status[3],"at least 4 iterations")
})

test_that("comparison verification covers MCSE for each constituent model", {
  f <- .mi_diagnostic_fixture("mi_comparison_mcse")
  on.exit(ls_unregister_dataset(f$id),add=TRUE)
  path <- tempfile(fileext=".rds"); on.exit(unlink(path),add=TRUE)
  saveRDS(LinkEDA:::.rls_export_dataset_data(LinkEDA:::.rls_dataset_record(f$id)),path)
  comparison <- ls_new_regression_comparison(f$id,response="bmi",
    models=list(c("age"),c("age","chl")),native=FALSE)
  record <- LinkEDA:::.rls_regcmp_record(comparison)
  env <- new.env(parent=globalenv()); env$verification_data_path <- path
  invisible(capture.output(eval(parse(text=record$analysis_provenance$verification_r_code$comparison),env)))
  diagnostics <- ls_missing_information_diagnostics(record)
  expect_equal(unlist(lapply(env$reference_missing_information,`[[`,"mcse"),use.names=FALSE),
    diagnostics$`MCSE (estimate)`)
  models <- lapply(list(c("age"),c("age","chl")),function(terms)
    ls_new_generalized_linear_model(f$id,"bmi",terms,family="gaussian",link="identity",native=FALSE))
  result <- ls_compare_generalized_linear_models(models,native=FALSE)
  code <- result$analysis_provenance$verification_r_code$missing_information
  expect_match(code,"Model 1")
  expect_match(code,"Model 2")
  expect_false(grepl("LinkEDA:::",code,fixed=TRUE))
  output <- capture.output(eval(parse(text=code),env))
  expect_true(any(grepl("mcse",output)))
  expect_equal(ls_missing_information_diagnostics(result)$`MCSE (estimate)`,
    unlist(lapply(models,function(model)ls_missing_information_diagnostics(model)$`MCSE (estimate)`),use.names=FALSE))
})

test_that("imputation plots identify their diagnostic kind without changing R coordinates", {
  f <- .mi_diagnostic_fixture("mi_plot_semantics")
  on.exit(ls_unregister_dataset(f$id),add=TRUE)
  sent <- list()
  testthat::local_mocked_bindings(.rls_send=function(lines,...) {
    sent[[length(sent)+1L]] <<- lines
    "OK"
  },.package="LinkEDA")
  for(section in c("distributions","chain_mean","chain_variance")) {
    result <- ls_imputation_diagnostics(f$id,section=section,variable="bmi",native=FALSE)
    LinkEDA:::.rls_mi_show_diagnostic_plot(result$table,result$plot_data,section)
    provenance <- sent[[length(sent)]]
    expect_identical(provenance[1], "SET_DIAGNOSTIC_PLOT_PROVENANCE")
    expect_identical(provenance[6], f$id)
    expect_identical(result$table$analysis_provenance$dataset_id, f$id)
    payload <- sent[[length(sent)-1L]]
    expect_identical(payload[match("IMPUTATION_PROCESS_DIAGNOSTIC",payload)+0:1],c("IMPUTATION_PROCESS_DIAGNOSTIC",section))
    expect_identical(payload[1],"ADD_PLOT")
    expect_match(payload[3],":unlinked$",perl=TRUE)
    n <- nrow(result$plot_data)
    coordinates <- do.call(rbind,lapply(payload[7L+seq_len(n)],function(x) as.numeric(strsplit(x," ",fixed=TRUE)[[1L]])))
    expect_equal(unname(coordinates[,1:2]),unname(as.matrix(result$plot_data[1:2])))
  }
})


test_that("graphical MI diagnostics open the plot without an unsolicited coordinate table", {
  f <- .mi_diagnostic_fixture("mi_native_plot_output")
  on.exit(ls_unregister_dataset(f$id), add=TRUE)
  sent <- list(); tables <- list()
  testthat::local_mocked_bindings(
    .rls_start_backend=function(...) invisible(TRUE),
    .rls_send=function(lines, ...) {sent[[length(sent)+1L]] <<- lines; "OK"},
    .rls_table1_sync_native=function(record) {tables[[length(tables)+1L]] <<- record; TRUE},
    .package="LinkEDA")
  for (section in c("distributions", "chain_mean", "chain_variance")) {
    sent <- list(); tables <- list()
    result <- ls_imputation_diagnostics(f$id, section=section, variable="bmi", native=TRUE)
    expect_gt(nrow(result$plot_data), 0L)
    expect_length(tables, 0L)
    expect_identical(vapply(sent, `[[`, character(1), 1L),
      c("ADD_PLOT", "SET_DIAGNOSTIC_PLOT_PROVENANCE"))
  }
  sent <- list(); tables <- list()
  ls_imputation_diagnostics(f$id, section="summary", native=TRUE)
  expect_length(tables, 1L)
  expect_length(sent, 0L)
})


test_that("large ECDF views have bounded size and retain R cumulative probabilities", {
  n <- 100000L
  original <- seq_len(n)/100
  original[seq.int(2L,n,2L)] <- NA_real_
  completed <- data.frame(x=seq_len(n)/100)
  info <- list(original_data=data.frame(x=original),name_map=c(x="x"),m=20L,
    completed_datasets=rep(list(completed),20L))
  result <- LinkEDA:::.rls_mi_distribution_data(info,"x")
  expect_lte(nrow(result$data),6L*2L*512L)
  expect_identical(unique(result$data$Series),c("Observed",paste("Imputed",1:5)))
  expect_true(result$view$simplified)
  for(label in unique(result$data$Series)) {
    curve <- result$data[result$data$Series==label,]
    x <- if(label=="Observed") original[!is.na(original)] else completed$x[is.na(original)]
    upper <- seq.int(2L,nrow(curve),2L)
    expect_equal(curve$Proportion[upper],as.numeric(stats::ecdf(x)(curve$Value[upper])))
    expect_equal(range(curve$Value),range(x))
    expect_equal(range(curve$Proportion),c(0,1))
    displayed <- stats::stepfun(curve$Value[upper],c(0,curve$Proportion[upper]))
    expect_lte(max(abs(displayed(x)-stats::ecdf(x)(x))),1/511+1/length(x))
  }
  next_page <- LinkEDA:::.rls_mi_distribution_data(info,"x",16L)
  expect_identical(unique(next_page$data$Series),c("Observed",paste("Imputed",16:20)))
  expect_identical(next_page$view$first,16L)
  expect_error(LinkEDA:::.rls_mi_distribution_data(info,"x",21L),"available imputation")
  # All-equal samples and no imputed values remain valid, without invented curves.
  info$original_data$x <- rep(1,4); info$completed_datasets <- rep(list(data.frame(x=rep(1,4))),20)
  same <- LinkEDA:::.rls_mi_distribution_data(info,"x")
  expect_equal(nrow(same$data),2L)
  expect_identical(unique(same$data$Series),"Observed")
  expect_false(same$view$simplified)
})

test_that("distribution pages preserve exact verification inputs and selected imputations", {
  f <- .mi_diagnostic_fixture("mi_distribution_pages")
  on.exit(ls_unregister_dataset(f$id),add=TRUE)
  first <- ls_imputation_diagnostics(f$id,"distributions","bmi",native=FALSE)
  second <- ls_imputation_diagnostics(f$id,"distributions","bmi",native=FALSE,imputation_start=2L)
  expect_identical(first$table$analysis_provenance$prepared_data_path,
    second$table$analysis_provenance$prepared_data_path)
  expect_match(second$table$analysis_provenance$verification_r_code$table,"indices <- 2:3",fixed=TRUE)
  expect_null(second$info$missingness_patterns)
  expect_identical(unique(second$plot_data$Series),c("Observed","Imputed 2","Imputed 3"))
  expected <- mice::complete(f$imp,2)$bmi[is.na(f$imp$data$bmi)]
  curve <- subset(second$plot_data,Series=="Imputed 2")
  expect_equal(curve$Proportion[seq.int(2,nrow(curve),2)],
    as.numeric(stats::ecdf(expected)(sort(unique(expected)))))
})


test_that("the native imputation selector reaches the requested R page", {
  seen <- NULL
  testthat::local_mocked_bindings(ls_imputation_diagnostics=function(...) {seen <<- list(...)},
    .package="LinkEDA")
  LinkEDA:::.rls_process_control_lines("MI_DIAGNOSTICS_NEEDED\tdata\tdistributions\tbmi\t6")
  expect_identical(seen$imputation_start,6L)
  expect_identical(seen$variable,"bmi")
  expect_identical(seen[[1]],"data")
})
