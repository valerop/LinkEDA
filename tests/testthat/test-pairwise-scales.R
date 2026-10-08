.pw_record <- function(distribution='identity',mi=FALSE) {
  set.seed(482);d<-expand.grid(g=factor(c('Control - A','Treatment','Third')),h=factor(c('X','Y')),i=1:18)
  d$x<-rnorm(nrow(d));eta<-.4+.25*(as.integer(d$g)-1)+.12*as.integer(d$h)+.1*d$x
  d$y<-switch(distribution,identity=eta+rnorm(nrow(d)),log=rpois(nrow(d),exp(eta)),logit=rbinom(nrow(d),1,plogis(eta)),probit=rbinom(nrow(d),1,pnorm(eta)))
  family<-switch(distribution,identity=gaussian(),log=poisson(),logit=binomial(),probit=binomial('probit'))
  sets<-if(mi)lapply(1:4,function(i){a<-d;a$x[1:12]<-a$x[1:12]+rnorm(12,sd=.3);a}) else list(d)
  fits<-lapply(sets,function(a)glm(y~g*h+x,family=family,data=a))
  list(fit=fits[[1]],fits_by_imputation=fits,predictors=c('g','h','g:h','x'),family=family$family,link=family$link)
}

# Public-package oracle uses the whole mira reference grid, including covariance.
.pw_reference <- function(record,term,scale='link',by=NULL) {
 fits<-record$fits_by_imputation
 target<-if(length(fits)>1L)mice::as.mira(fits) else fits[[1L]]
 grid<-emmeans::emmeans(target,term,by=by)
 # Keep the established MI covariance metadata when testing the legacy API.
 if(scale=='response') grid<-LinkEDA:::.rls_mi_regrid(grid,transform='response')
 LinkEDA:::.rls_emmeans_standardize_table(summary(emmeans::contrast(grid,'pairwise',adjust='tukey'),infer=c(TRUE,TRUE)))
}

test_that('pairwise ratio labels agree with the fitted distribution and link', {
  skip_if_not_installed('emmeans')
  identity <- .pw_record('identity')
  log_count <- .pw_record('log')
  logit <- .pw_record('logit')
  expect_error(LinkEDA:::.rls_regression_pairwise_record(identity, 'g', 'odds_ratio'),
    'not valid for this model')
  expect_error(LinkEDA:::.rls_regression_pairwise_record(logit, 'g', 'risk_ratio'),
    'not valid for this model')
  expect_error(LinkEDA:::.rls_regression_pairwise_record(log_count, 'g', 'probability'),
    'not valid for this model')
  odds <- LinkEDA:::.rls_regression_pairwise_record(logit, 'g', 'odds_ratio')
  means <- LinkEDA:::.rls_regression_pairwise_record(log_count, 'g', 'mean_ratio')
  expect_equal(odds$odds_ratio, exp(odds$link_estimate))
  expect_equal(means$mean_ratio, exp(means$link_estimate))
})

test_that('identity, log, logit and probit add descriptions without altering inference', {
 skip_if_not_installed('emmeans'); skip_if_not_installed('mice')
 for(family in c('identity','log','logit','probit')) for(mi in c(FALSE,TRUE)) {
  record<-.pw_record(family,mi)
  old<-.pw_reference(record,'g')
  now<-LinkEDA:::.rls_regression_pairwise_record(record,'g',scale='link')
  fields<-grep('^pooled_',names(old),value=TRUE)
  expect_equal(now[fields],old[fields],tolerance=1e-13)
  expect_equal(now$link_estimate,old$pooled_estimate,tolerance=1e-13)
  expect_equal(now$link_std_error,old$pooled_std_error,tolerance=1e-13)
  expect_equal(now$link_p_value,old$pooled_p_value,tolerance=1e-13)
  expect_equal(now$category_first[[1L]],"Control - A")
  expect_equal(now$response_difference,now$mean_first-now$mean_second)
  if(family=='identity')expect_equal(now$response_difference,now$link_estimate,tolerance=1e-12)
  if(family=='log') expect_equal(now$transformed_effect,now$mean_first/now$mean_second,tolerance=1e-12)
  if(family=='logit') {
   expect_true(all(c(now$mean_first,now$mean_second)>=0 & c(now$mean_first,now$mean_second)<=1))
   expect_equal(now$transformed_effect,(now$mean_first/(1-now$mean_first))/(now$mean_second/(1-now$mean_second)),tolerance=1e-12)
  }
  if(family %in% c('log','logit')) {
   expect_equal(now$transformed_conf_low,exp(old$pooled_conf_low))
   expect_equal(now$transformed_conf_high,exp(old$pooled_conf_high))
  }
  # Explicit response-scale callers keep their established response inference.
  legacy<-.pw_reference(record,'g',scale='response')
  response<-LinkEDA:::.rls_regression_pairwise_record(record,'g',scale='response')
  expect_equal(response[fields],legacy[fields],tolerance=1e-13)
  expect_equal(response$link_p_value,old$pooled_p_value)
 }
})

test_that('EMM effects preserve every inferential column and full-covariance interaction result', {
 skip_if_not_installed('emmeans'); skip_if_not_installed('mice')
 for(family in c('identity','log','logit','probit')) for(mi in c(FALSE,TRUE)) {
  record<-.pw_record(family,mi)
  reference<-.pw_reference(record,'g',by='h')
  now<-LinkEDA:::.rls_regression_interaction_record(record,'g:h')
  fields<-grep('^pooled_',names(reference),value=TRUE)
  expect_equal(now$comparisons[fields],reference[fields],tolerance=1e-13)
  expect_equal(now$comparisons$link_estimate,reference$pooled_estimate)
  lines<-LinkEDA:::.rls_interaction_native_report(now)
  expect_true('Estimated marginal means: response scale' %in% lines)
  expect_true(any(grepl('Interaction contrast estimand: difference of differences (response scale)',lines,fixed=TRUE)))
  # Numeric conditioning does not become a response-scale slope contrast.
  slopes<-LinkEDA:::.rls_regression_interaction_record(record,'x')
  expect_false('link_name' %in% names(slopes$comparisons))
  dir<-Sys.getenv('LINKEDA_PAIRWISE_FIXTURES')
  if(nzchar(dir) && family=='log' && mi) {
   writeLines(lines,file.path(dir,'interaction.txt'))
   result<-LinkEDA:::.rls_regression_pairwise_record(record,'g',scale='link')
   writeLines(LinkEDA:::.rls_pairwise_native_payload(result),file.path(dir,'pairwise.txt'))
   saveRDS(now$comparisons,file.path(dir,'comparison.rds'))
  }
 }
})

test_that('public verification reproduces both scales and complete interaction contrasts', {
 skip_if_not_installed('emmeans'); skip_if_not_installed('mice')
 for(family in c('identity','log','logit','probit')) for(mi in c(FALSE,TRUE)) {
  record<-.pw_record(family,mi)
  data<-lapply(record$fits_by_imputation,model.frame)
  family_code<-switch(family,identity='gaussian()',log='poisson()',logit='binomial()',probit="binomial('probit')")
  base<-paste0('completed <- ',paste(capture.output(dput(data)),collapse='\n'),
    '\nreference_fits <- lapply(completed,function(data) glm(y~g*h+x,data=data,family=',family_code,'))')
  record$analysis_provenance<-list(verification_r_code=list(model=base))
  pairwise<-LinkEDA:::.rls_regression_pairwise_record(record,'g')
  effect<-LinkEDA:::.rls_regression_interaction_record(record,'g:h')
  for(kind in c('pairwise','effect')) {
   code<-if(kind=='pairwise') attr(pairwise,'analysis_provenance')$verification_r_code$pairwise_scales else effect$analysis_provenance$verification_r_code$table
   expected<-if(kind=='pairwise') pairwise else effect$comparisons
   env<-new.env(parent=globalenv())
   expect_false(grepl('LinkEDA:::',code,fixed=TRUE))
   invisible(capture.output(eval(parse(text=code),env)))
   expect_equal(env$reference$estimate,expected$link_estimate,tolerance=1e-11)
   expect_equal(env$reference$SE,expected$link_std_error,tolerance=1e-11)
   expect_equal(env$reference$p.value,expected$link_p_value,tolerance=1e-11)
   expect_equal(env$reference$mean_first,expected$mean_first,tolerance=1e-11)
   expect_equal(env$reference$mean_second,expected$mean_second,tolerance=1e-11)
   expect_length(env$grid_fits,length(record$fits_by_imputation))
   if(kind=='effect') {
    # In this two-factor fixture each complete contrast has a unique estimate.
    actual<-effect$interaction_contrasts
    reproduced<-env$interaction_reference
    expect_equal(sort(reproduced$estimate),sort(actual$pooled_estimate),tolerance=1e-11)
    a<-order(actual$pooled_estimate);b<-order(reproduced$estimate)
    expect_equal(reproduced$SE[b],actual$pooled_std_error[a],tolerance=1e-11)
    expect_equal(reproduced$p.value[b],actual$pooled_p_value[a],tolerance=1e-11)
   }
  }
 }
})

test_that('lognormal effect verification distinguishes arithmetic means from log contrasts', {
 skip_if_not_installed('emmeans'); skip_if_not_installed('gtsummary')
 set.seed(51)
 d<-data.frame(g=factor(rep(c('A','B'),each=24)))
 d$y<-exp(1+.6*(d$g=='B')+rnorm(48,sd=.9))
 model<-ls_new_positive_continuous_model(d,'y','g',distribution='lognormal',native=FALSE)
 effect<-ls_generalized_linear_model_interaction(model,'g')
 path<-tempfile(fileext='.rds');on.exit(unlink(path));saveRDS(d,path)
 env<-new.env(parent=globalenv());env$verification_data_path<-path
 code<-effect$analysis_provenance$verification_r_code$table
 invisible(capture.output(eval(parse(text=code),env)))
 expect_equal(env$reference$mean_first,effect$comparisons$mean_first)
 expect_equal(env$reference$mean_second,effect$comparisons$mean_second)
 expect_equal(env$reference$estimate,effect$comparisons$link_estimate)
 expect_match(attr(effect$comparisons,'scale_note'),'arithmetic means',fixed=TRUE)
})

test_that('standalone native binary script emits finite asymptotic intervals and both scales', {
 skip_if_not_installed('emmeans')
 source<-testthat::test_path('..','..','src','core','glm_model.cpp')
 skip_if_not(file.exists(source))
 text<-paste(readLines(source,warn=FALSE),collapse='\n')
 code<-strsplit(strsplit(text,'return R"RBINARYPAIRWISE(',fixed=TRUE)[[1L]][2L],')RBINARYPAIRWISE";',fixed=TRUE)[[1L]][1L]
 set.seed(21);d<-expand.grid(g=factor(c('A','B','C')),i=1:40)
 d$x<-rnorm(nrow(d));d$y<-rbinom(nrow(d),1,plogis(.2+.4*as.numeric(d$g)+.2*d$x))
 path<-tempfile(fileext='.csv');on.exit(unlink(path));write.csv(d,path,row.names=FALSE)
 for(link in c('logit','probit')) {
  env<-new.env(parent=globalenv())
  env$commandArgs<-function(...) c(path,'y','1',link,'g','.95','2','g','x','3','y','g','x','numeric','factor','numeric')
  output<-capture.output(eval(parse(text=code),env))
  expect_true('PAIRWISE_SCALES_V1' %in% output)
  row<-strsplit(output[startsWith(output,'ROW\t')][1L],'\t')[[1L]]
  expect_true(all(is.finite(as.numeric(row[10:11]))))
  expect_equal(env$details$response_difference,env$details$mean_first-env$details$mean_second)
  if(link=='logit') expect_equal(as.numeric(row[10:11]),exp(c(env$details$link_conf_low[1L],env$details$link_conf_high[1L])))
 }
})
