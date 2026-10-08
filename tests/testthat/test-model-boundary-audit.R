.boundary_recipe <- function(code, data) {
  expect_false(grepl('LinkEDA:::', code, fixed = TRUE))
  path <- tempfile(fileext = '.rds'); on.exit(unlink(path))
  saveRDS(data, path)
  env <- new.env(parent = globalenv()); env$verification_data_path <- path
  invisible(capture.output(eval(parse(text = code), env)))
  env
}

test_that('lognormal likelihood and information criteria use the original response', {
  set.seed(14)
  d <- data.frame(x = rnorm(60)); d$y <- exp(3 + .35*d$x + rnorm(60, sd=.5))
  # Include scope and missing rows, so the Jacobian must use fitted cases only.
  d$y[3] <- NA_real_
  id <- ls_register_dataset('boundary_lognormal', d)
  on.exit(ls_unregister_dataset(id))
  model <- ls_new_positive_continuous_model(id, 'y', 'x', distribution='lognormal', native=FALSE)
  result <- ls_generalized_linear_model_state(model)
  fit <- lm(log(y) ~ x, d)
  y <- exp(model.response(model.frame(fit)))
  ll <- sum(stats::dlnorm(y, stats::fitted(fit), sqrt(mean(stats::residuals(fit)^2)), log=TRUE))
  k <- attr(stats::logLik(fit), 'df')
  expect_equal(result$summary$log_lik, ll)
  expect_equal(result$summary$aic, -2*ll + 2*k)
  expect_equal(result$summary$bic, -2*ll + log(length(y))*k)
  gamma <- ls_new_positive_continuous_model(id, 'y', 'x', distribution='Gamma', native=FALSE)
  ref_gamma <- glm(y ~ x, d, family=Gamma(link='log'))
  comparison <- ls_compare_generalized_linear_models(model, gamma, .model_type='positive_continuous', native=FALSE)
  expect_equal(comparison$models$AIC, c(-2*ll+2*k, AIC(ref_gamma)))
  expect_equal(comparison$models$BIC, c(-2*ll+log(length(y))*k, BIC(ref_gamma)))
  skip_if_not_installed('gtsummary')
  env <- .boundary_recipe(result$analysis_provenance$verification_r_code$model, na.omit(d))
  expect_equal(env$fit_statistics$AIC, result$summary$aic)
  expect_equal(env$fit_statistics$BIC, result$summary$bic)
  expect_equal(env$fit_statistics$logLik, result$summary$log_lik)
})

test_that('count comparisons reject different data versions but preserve valid tests', {
  set.seed(332)
  d <- data.frame(x=rnorm(60), z=rnorm(60)); d$y <- rpois(60,exp(1+.2*d$x))
  id <- ls_register_dataset('boundary_versions', d)
  on.exit(ls_unregister_dataset(id))
  a <- ls_new_count_regression(id,'y','x',native=FALSE)
  b <- ls_new_count_regression(id,'y',c('x','z'),native=FALSE)
  valid <- ls_compare_count_regression_models(a,b,native=FALSE)
  ref <- anova(glm(y~x, d, family=poisson()), glm(y~x+z,d,family=poisson()),test='Chisq')
  expect_true(valid$tests$available)
  expect_equal(valid$tests$p_value, ref$`Pr(>Chi)`[2])
  changed <- d; changed$y <- as.integer(round(exp(2+.8*d$x+.9*d$z)))
  LinkEDA:::.rls_register_dataset(id,changed,replace=TRUE)
  c <- ls_new_count_regression(id,'y',c('x','z'),native=FALSE)
  expect_error(ls_compare_count_regression_models(a,c,native=FALSE),'different data versions')
  a2 <- ls_new_count_regression(id,'y','x',native=FALSE)
  expect_true(ls_compare_count_regression_models(a2,c,native=FALSE)$tests$available)
  # Loaded older fits without provenance must still not compare changed responses.
  for (handle in list(a,c)) {
    record <- LinkEDA:::.rls_generalized_glm_record(handle)
    record$diagnostic_data_version <- NULL; record$analysis_provenance <- NULL
    record$data_scope$dataset_version <- NULL
    LinkEDA:::.rls_assign_generalized_glm(record)
  }
  expect_error(ls_compare_count_regression_models(a,c,native=FALSE),'different fitted responses')
})

test_that('interaction reports label every interval using the requested confidence', {
  skip_if_not_installed('emmeans')
  set.seed(23)
  d <- data.frame(a=gl(2,30),b=rep(gl(3,10),2),x=rnorm(60),y=rnorm(60))
  model <- ls_new_generalized_linear_model(d,'y',c('a','b','a:b','x'),family='gaussian',link='identity',native=FALSE)
  for (level in c(.9,.95,.99,.975)) {
    result <- ls_generalized_linear_model_interaction(model,'a:b',confidence_level=level)
    report <- LinkEDA:::.rls_interaction_native_report(result)
    label <- paste0(format(100*level,trim=TRUE),'%')
    expect_true(paste('Confidence level:',label) %in% report)
    headers <- report[grepl('Lower ',report,fixed=TRUE)]
    expect_gte(length(headers),2)
    expect_true(all(grepl(paste('Lower',label),headers,fixed=TRUE)))
    expect_true(all(grepl(paste('Upper',label),headers,fixed=TRUE)))
    if (level != .95) expect_false(any(grepl('95%',headers,fixed=TRUE)))
    ref <- as.data.frame(confint(emmeans::emmeans(ls_generalized_linear_model_state(model)$fit,~a|b),level=level))
    expect_equal(result$estimates$pooled_conf_low,ref$lower.CL)
    expect_equal(result$estimates$pooled_conf_high,ref$upper.CL)
  }
})

test_that('binomial tests preserve a defined event at zero and all successes', {
  for (event_count in c(0,7,20)) for (alternative in c('two.sided','less','greater')) {
    d <- data.frame(y=factor(c(rep('yes',event_count),rep('no',20-event_count)),levels=c('no','yes')))
    id <- ls_register_dataset(paste0('boundary_binomial_',event_count,'_',alternative),d)
    result <- LinkEDA:::.rls_compare_means_record(ls_new_one_sample_t_test(id,'y',mu=.5,method='binomial',alternative=alternative)[[1]])
    reference <- stats::binom.test(event_count,20,p=.5,alternative=alternative)
    expect_equal(result$test_results$p_value, reference$p.value)
    expect_equal(result$test_results$conf_int,unname(reference$conf.int),ignore_attr=TRUE)
    expect_equal(result$specification$event_level,'yes')
    env <- .boundary_recipe(LinkEDA:::.rls_compare_means_verification_r_code(list(result))$code,d)
    expect_equal(env$reference_results[[1]]$p.value,reference$p.value)
    ls_unregister_dataset(id)
  }
  d <- data.frame(y=factor(c(rep('yes',10),rep('no',10)),levels=c('no','yes')))
  id <- ls_register_dataset('boundary_binomial_scope',d);on.exit(ls_unregister_dataset(id))
  for (rows in list(1:10,11:20,1L)) {
    result <- LinkEDA:::.rls_compare_means_record(ls_new_one_sample_t_test(id,'y',mu=.5,method='binomial',scope='selected',.selected_rows=rows)[[1]])
    ref <- binom.test(sum(d$y[rows]=='yes'),length(rows),p=.5)
    expect_equal(result$test_results$p_value,ref$p.value)
    expect_equal(result$rows_used_original_ids,rows)
  }
  unknown <- ls_register_dataset('boundary_binomial_unknown',data.frame(y=rep('no',20)))
  on.exit(ls_unregister_dataset(unknown),add=TRUE)
  expect_error(ls_new_one_sample_t_test(unknown,'y',mu=.5,method='binomial'),'two defined categories')
})

test_that('MI model comparisons also reject changed imputation data versions', {
  skip_if_not_installed('mice')
  set.seed(29)
  d <- data.frame(x=rnorm(50),z=rnorm(50));d$y <- rpois(50,exp(1+.3*d$x))
  id <- ls_register_dataset('boundary_mi_versions', d);on.exit(ls_unregister_dataset(id))
  dataset <- LinkEDA:::.rls_dataset_record(id)
  dataset$dataset_type <- 'multiple_imputation'
  dataset$completed_datasets <- lapply(1:3,function(i) transform(d,y=replace(y,1:4,y[1:4]+i)))
  dataset$imputation_count <- 3L;dataset$original_data <- d;dataset$original_data$y[1:4] <- NA_real_
  dataset$missing_cell_mask <- lapply(dataset$original_data,is.na)
  LinkEDA:::.rls_set_dataset_record(dataset)
  LinkEDA:::.rls_store_data_version(dataset)
  a <- ls_new_count_regression(id,'y','x',native=FALSE)
  b <- ls_new_count_regression(id,'y',c('x','z'),native=FALSE)
  expect_equal(ls_generalized_linear_model_state(a)$analysis_backend,'multiple_imputation')
  expect_true(ls_compare_count_regression_models(a,b,native=FALSE)$common_rows)
  dataset$completed_datasets <- lapply(dataset$completed_datasets,function(x)transform(x,y=y+1))
  dataset <- LinkEDA:::.rls_advance_data_version(dataset,'Edit imputations in test')
  LinkEDA:::.rls_set_dataset_record(dataset)
  c <- ls_new_count_regression(id,'y',c('x','z'),native=FALSE)
  expect_error(ls_compare_count_regression_models(a,c,native=FALSE),'different data versions')
})

test_that('binary effect reports retain fractional confidence levels', {
  skip_if_not_installed('emmeans')
  set.seed(72)
  d <- data.frame(x=rnorm(100),g=gl(2,50),y=rbinom(100,1,.4))
  model <- ls_new_binary_regression(d,'y',c('x','g'),native=FALSE)
  result <- ls_binary_regression_interaction(model,'g',confidence_level=.975)
  report <- LinkEDA:::.rls_interaction_native_report(result)
  expect_true('Confidence level: 97.5%' %in% report)
  expect_true(any(grepl('Lower 97.5%',report,fixed=TRUE)))
})
