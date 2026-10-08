.wml_register <- function(name, sets) {
  id <- ls_register_dataset(name, sets[[1]])
  d <- LinkEDA:::.rls_dataset_record(id)
  d$dataset_type <- "multiple_imputation"; d$completed_datasets <- sets
  d$imputation_count <- length(sets); d$original_data <- sets[[1]]
  d$original_data$y[1:3] <- NA_real_
  d$missing_cell_mask <- lapply(d$original_data, is.na)
  LinkEDA:::.rls_set_dataset_record(d); id
}
.wml_sets <- function() {
  withr::local_seed(102)
  d <- data.frame(g=factor(rep(c("A","B","C"),c(10,20,60))))
  d$y <- c(rnorm(10,0,10),rnorm(20,2,2),rnorm(60,2.5,1))
  lapply(1:3,function(i)transform(d,y=replace(y,1:3,y[1:3]+i*.1)))
}
.wml_run <- function(code,data) {
  expect_false(grepl("LinkEDA:::|\\.rls_",code))
  path <- tempfile(fileext=".rds"); on.exit(unlink(path)); saveRDS(data,path)
  e <- new.env(parent=globalenv()); e$verification_data_path <- path
  invisible(capture.output(invisible(eval(parse(text=code),e)))); e
}

test_that("MI Welch uses oneway.test and micombine.F, distinct from classical D1", {
  sets <- .wml_sets(); id <- .wml_register("wml_welch",sets)
  on.exit(ls_unregister_dataset(id))
  h <- ls_new_one_way_anova(id,"y","g",method="welch")
  r <- LinkEDA:::.rls_compare_means_record(h[[1]])
  fits <- lapply(sets,function(d)stats::oneway.test(y~g,d,var.equal=FALSE))
  ref <- miceadds::micombine.F(vapply(fits,function(f)unname(f$statistic),numeric(1)),df1=2,display=FALSE)
  expect_equal(r$test_results$statistic,unname(ref['D']))
  expect_equal(r$test_results$p_value,unname(ref['p']))
  expect_equal(unname(r$test_results$parameter),unname(ref[c('df','df2')]))
  expect_equal(r$multiple_imputation$fits_by_imputation[[2]]$parameter,fits[[2]]$parameter)
  expect_match(r$test_results$pooling_method,"miceadds::micombine.F",fixed=TRUE)
  expect_match(r$warnings,"denominator degrees of freedom are not used",fixed=TRUE)
  classical <- LinkEDA:::.rls_compare_means_record(ls_new_one_way_anova(id,"y","g",method="classical")[[1]])
  full <- mice::as.mira(lapply(sets,function(d)lm(y~g,d)))
  null <- mice::as.mira(lapply(sets,function(d)lm(y~1,d)))
  expect_equal(classical$test_results$p_value,unname(mice::D1(full,null)$result[1,'P(>F)']))
  expect_gt(abs(r$test_results$p_value-classical$test_results$p_value),.01)
  mi <- LinkEDA:::.rls_mi_missing_information(r)
  expect_equal(mi$F,r$test_results$statistic); expect_equal(mi$p,r$test_results$p_value)
  code <- LinkEDA:::.rls_compare_means_verification_r_code(list(r))$code
  e <- .wml_run(code,LinkEDA:::.rls_export_dataset_data(LinkEDA:::.rls_dataset_record(id)))
  expect_equal(unname(e$raw_p),r$test_results$p_value)
  expect_equal(e$reference_results[[1]]$statistic,r$test_results$statistic)
  payload <- LinkEDA:::.rls_compare_means_batch_native_payload(list(r))
  expect_true(any(grepl("miceadds::micombine.F",payload,fixed=TRUE)))
})

test_that("MI Welch respects selected rows and refuses unusable imputations", {
  sets <- .wml_sets(); id <- .wml_register("wml_scope",sets)
  on.exit(ls_unregister_dataset(id))
  rows <- c(1:8,11:26,31:70)
  r <- LinkEDA:::.rls_compare_means_record(ls_new_one_way_anova(id,"y","g",scope="selected",.selected_rows=rows)[[1]])
  ref <- miceadds::micombine.F(vapply(sets,function(d)unname(stats::oneway.test(y~g,d[rows,],var.equal=FALSE)$statistic),numeric(1)),2,display=FALSE)
  expect_equal(r$rows_used_original_ids,rows)
  expect_equal(r$test_results$p_value,unname(ref['p']))
  prepared <- LinkEDA:::.rls_export_dataset_data(LinkEDA:::.rls_dataset_record(id))
  # The shared export supplies the frozen analysis scope, in long MI format.
  prepared <- prepared[prepared$.id %in% r$data_scope$rows,,drop=FALSE]
  e <- .wml_run(LinkEDA:::.rls_compare_means_verification_r_code(list(r))$code,prepared)
  expect_equal(unname(e$raw_p),r$test_results$p_value)
  expect_error(ls_new_one_way_anova(id,"y","g",scope="selected",.selected_rows=c(1,11:26,31:70)),"two observations")
  d <- LinkEDA:::.rls_dataset_record(id)
  d$completed_datasets[[2]]$y[11:30] <- 2
  LinkEDA:::.rls_set_dataset_record(d)
  expect_error(ls_new_one_way_anova(id,"y","g"),"not estimable in imputation 2")
  d$completed_datasets <- rep(sets[1],3); LinkEDA:::.rls_set_dataset_record(d)
  r <- LinkEDA:::.rls_compare_means_record(ls_new_one_way_anova(id,"y","g")[[1]])
  reference <- miceadds::micombine.F(rep(unname(stats::oneway.test(y~g,sets[[1]],var.equal=FALSE)$statistic),3),2,display=FALSE)
  expect_equal(unname(r$test_results$parameter['df2']),unname(reference['df2']))
})

test_that("McNemar retains defined binary categories at the boundary", {
  d <- data.frame(before=factor(rep("No",20),levels=c("No","Yes")),
                  after=factor(rep(c("No","Yes"),each=10),levels=c("No","Yes")))
  id <- ls_register_dataset("wml_mcnemar",d); on.exit(ls_unregister_dataset(id))
  for(method in list(NULL,"mcnemar")) {
    r <- LinkEDA:::.rls_compare_means_record(ls_new_paired_samples_t_test(id,list(c("before","after")),method=method)[[1]])
    expect_equal(r$test_results$p_value,stats::binom.test(0,10,.5)$p.value)
    expect_match(r$test_results$method,"McNemar")
    e <- .wml_run(LinkEDA:::.rls_compare_means_verification_r_code(list(r))$code,d)
    expect_equal(unname(e$raw_p),r$test_results$p_value)
  }
  reversed <- LinkEDA:::.rls_compare_means_record(ls_new_paired_samples_t_test(id,list(c("after","before")))[[1]])
  expect_equal(reversed$test_results$p_value,r$test_results$p_value)
  selected <- LinkEDA:::.rls_compare_means_record(ls_new_paired_samples_t_test(id,list(c("before","after")),scope="selected",.selected_rows=6:15)[[1]])
  expect_equal(selected$test_results$p_value,stats::binom.test(0,5,.5)$p.value)
  expect_error(ls_new_paired_samples_t_test(id,list(c("before","after")),scope="selected",.selected_rows=1:10),"discordant")
  for (alternative in c("less","greater")) {
    test <- LinkEDA:::.rls_compare_means_record(ls_new_paired_samples_t_test(id,
      list(c("before","after")),alternative=alternative)[[1]])
    expect_equal(test$test_results$p_value,stats::binom.test(0,10,.5,alternative=alternative)$p.value)
  }
  no_labels <- transform(d,before=as.character(before))
  id2 <- ls_register_dataset("wml_unknown_category",no_labels)
  on.exit(ls_unregister_dataset(id2),add=TRUE)
  expect_error(ls_new_paired_samples_t_test(id2,list(c("before","after")),method="mcnemar"),"two defined categories")
})

test_that("lognormal raw residuals and public recipe use the log response scale", {
  withr::local_seed(14)
  d <- data.frame(x=rnorm(60)); d$y <- exp(3+.35*d$x+rnorm(60,sd=.5))
  model <- ls_new_positive_continuous_model(d,"y","x",distribution="lognormal",native=FALSE)
  state <- ls_generalized_linear_model_state(model)
  residuals <- ls_generalized_linear_model_diagnostics(model)
  fit <- stats::lm(log(y)~x,d)
  expect_equal(residuals$raw_residual,unname(stats::residuals(fit,type="response")))
  expect_equal(residuals$raw_residual,residuals$observed-residuals$fitted)
  expect_equal(residuals$deviance_residual,unname(stats::residuals(fit,type="response")))
  expect_equal(residuals$fitted_response_scale,unname(exp(stats::fitted(fit)+stats::sigma(fit)^2/2)))
  e <- .wml_run(state$analysis_provenance$verification_r_code$model,d)
  expect_equal(unname(e$raw_residual),residuals$raw_residual)
  sets <- lapply(c(.8,1,1.2),function(s)transform(d,y=y^s))
  id <- .wml_register("wml_lognormal_mi",sets); on.exit(ls_unregister_dataset(id))
  model <- ls_new_positive_continuous_model(id,"y","x",distribution="lognormal",native=FALSE)
  state <- ls_generalized_linear_model_state(model)
  e <- .wml_run(state$analysis_provenance$verification_r_code$model,
    LinkEDA:::.rls_export_dataset_data(LinkEDA:::.rls_dataset_record(id)))
  expect_equal(unname(lapply(e$raw_residuals,unname)),lapply(sets,function(d)unname(stats::residuals(stats::lm(log(y)~x,d)))))
})
