.gc_execute <- function(result, block = 'comparison') {
  code <- result$analysis_provenance$verification_r_code[[block]]
  expect_true(is.character(code) && nzchar(code))
  expect_false(grepl('LinkEDA:::|\\.rls_',code))
  env <- new.env(parent=globalenv())
  env$verification_data_path <- result$analysis_provenance$prepared_data_path
  invisible(capture.output(eval(parse(text=code),env)))
  env
}
.gc_compare <- function(...) ls_compare_generalized_linear_models(...,.model_type='positive_continuous',native=FALSE)
.gc_register_mi <- function(name, sets, missing='y') {
  id <- ls_register_dataset(name,sets[[1]])
  d <- LinkEDA:::.rls_dataset_record(id)
  d$dataset_type <- 'multiple_imputation'; d$completed_datasets <- sets
  d$imputation_count <- length(sets);d$original_data <- sets[[1]]
  d$original_data[[missing]][1:4] <- NA_real_
  d$missing_cell_mask <- lapply(d$original_data,is.na)
  LinkEDA:::.rls_set_dataset_record(d); LinkEDA:::.rls_store_data_version(d); id
}
.gc_expect_replay <- function(result) {
  e <- .gc_execute(result)
  for(i in seq_len(nrow(result$tests))) {
    row <- result$tests[i,];actual <- e$reference_comparisons[[i]]
    if(isTRUE(row$available)) {
      expect_equal(unname(actual['statistic']),row$statistic,tolerance=1e-7)
      expect_equal(unname(actual['p']),row$p_value,tolerance=1e-7)
      expect_equal(unname(actual['df1']),row$df1,tolerance=1e-7)
    } else expect_false(actual$available)
  }
  e
}

test_that('scaled GLM comparisons agree with R and exported recipes', {
  set.seed(923); d <- data.frame(x=rnorm(80),z=rnorm(80))
  d$y <- rgamma(80,shape=3,scale=exp(1+.4*d$x+.3*d$z)/3)
  id <- ls_register_dataset('gc_gamma',d);on.exit(ls_unregister_dataset(id))
  for (family in c('Gamma','inverse.gaussian','gaussian_log','lognormal')) {
    a <- ls_new_positive_continuous_model(id,'y','x',distribution=family,native=FALSE)
    b <- ls_new_positive_continuous_model(id,'y',c('x','z'),distribution=family,native=FALSE)
    result <- .gc_compare(a,b)
    expect_true(result$tests$available)
    .gc_expect_replay(result)
    reverse <- .gc_compare(b,a)
    expect_equal(reverse$tests$p_value,result$tests$p_value)
    .gc_expect_replay(reverse)
    if(family %in% c('Gamma','inverse.gaussian')) {
      expect_equal(pchisq(result$tests$statistic,result$tests$df,lower.tail=FALSE),result$tests$p_value)
      ref <- anova(ls_generalized_linear_model_state(a)$fit,ls_generalized_linear_model_state(b)$fit,test='Chisq')
      expect_equal(result$tests$p_value,ref[['Pr(>Chi)']][2])
    }
  }
})

test_that('comparison inputs and scope retain fitted version after edits', {
  set.seed(923);d <- data.frame(x=rnorm(80),z=rnorm(80));d$y <- exp(1+.4*d$x+rnorm(80))
  d$z[20] <- NA
  id <- ls_register_dataset('gc_scope',d);on.exit(ls_unregister_dataset(id))
  a <- ls_new_positive_continuous_model(id,'y','x',distribution='lognormal',scope='selected',.selected_rows=11:50,native=FALSE)
  b <- ls_new_positive_continuous_model(id,'y',c('x','z'),distribution='lognormal',scope='selected',.selected_rows=11:50,native=FALSE)
  first <- .gc_compare(a,b)
  expect_equal(first$data_scope$rows,11:50)
  expect_equal(first$effective_sample$scope_n,40L)
  expect_equal(first$effective_sample$excluded_n,0L) # each model keeps its own sample
  expect_false(first$tests$available)
  original <- readRDS(first$analysis_provenance$prepared_data_path)
  frozen <- .gc_execute(first)$model_data
  expect_equal(nrow(frozen[[1]][[1]]),40L);expect_equal(nrow(frozen[[2]][[1]]),39L)
  LinkEDA:::.rls_register_dataset(id,transform(d,y=y+100),replace=TRUE)
  after <- .gc_compare(a,b)
  expect_equal(after$analysis_provenance$dataset_version,first$analysis_provenance$dataset_version)
  expect_equal(readRDS(after$analysis_provenance$prepared_data_path),original)
  .gc_expect_replay(after)
  c <- ls_new_positive_continuous_model(id,'y','z',distribution='lognormal',native=FALSE)
  expect_error(.gc_compare(a,c),'different data versions')
  payload <- LinkEDA:::.rls_analysis_provenance_payload(after)
  expect_true('FROZEN_ANALYSIS_SCOPE_V1' %in% payload)
  if (nzchar(Sys.getenv('LINKEDA_COMPARISON_FIXTURE_DIR'))) {
    path <- Sys.getenv('LINKEDA_COMPARISON_FIXTURE_DIR')
    dir.create(path,recursive=TRUE,showWarnings=FALSE)
    writeLines(payload,file.path(path,'provenance.payload'))
    sent <- NULL
    local_mocked_bindings(.rls_send=function(lines,...) {sent<<-lines;"OK"},.package="LinkEDA")
    native <- ls_compare_generalized_linear_models(a,b,.model_type='positive_continuous',native=TRUE,.native_id='gc-native')
    copy_path <- file.path(path,'prepared.rds')
    file.copy(native$analysis_provenance$prepared_data_path,copy_path,overwrite=TRUE)
    sent[sent==native$analysis_provenance$prepared_data_path] <- copy_path
    writeLines(sent,file.path(path,'comparison.payload'))
    ds <- LinkEDA:::.rls_dataset_record(id)
    writeLines(c("REGISTER_DATASET",id,LinkEDA:::.rls_variable_payload(ds$data,ds$variable_metadata),LinkEDA:::.rls_dataframe_payload(ds$data,ds$variable_metadata,dataset_record=ds)),file.path(path,'dataset.payload'))
  }
})

test_that('different requested scopes have an explicit union and independent inputs', {
  d <- data.frame(y=exp(seq_len(40)/20+sin(1:40)),x=1:40,z=cos(1:40))
  id <- ls_register_dataset('gc_union',d);on.exit(ls_unregister_dataset(id))
  a <- ls_new_positive_continuous_model(id,'y','x',distribution='lognormal',scope='selected',.selected_rows=1:25,native=FALSE)
  b <- ls_new_positive_continuous_model(id,'y',c('x','z'),distribution='lognormal',scope='selected',.selected_rows=15:40,native=FALSE)
  result <- .gc_compare(a,b)
  expect_match(result$data_scope$description,'Union')
  expect_equal(result$data_scope$rows,1:40)
  expect_equal(vapply(.gc_execute(result)$model_data,function(x)nrow(x[[1]]),integer(1)),c(25L,26L))
  .gc_expect_replay(result)
})

test_that('comparison IDs neither consume nor initialize R random state', {
  d <- data.frame(y=exp(sin(1:30)+1),x=1:30,z=cos(1:30))
  id <- ls_register_dataset('gc_rng',d);on.exit(ls_unregister_dataset(id))
  a <- ls_new_positive_continuous_model(id,'y','x',distribution='lognormal',native=FALSE)
  b <- ls_new_positive_continuous_model(id,'y',c('x','z'),distribution='lognormal',native=FALSE)
  set.seed(14);before <- .Random.seed
  one <- .gc_compare(a,b);two <- .gc_compare(a,b)
  expect_identical(.Random.seed,before);expect_false(identical(one$id,two$id))
  rm('.Random.seed',envir=.GlobalEnv)
  on.exit(assign('.Random.seed',before,envir=.GlobalEnv),add=TRUE)
  .gc_compare(a,b)
  expect_false(exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE))
})

test_that('count and binary recipes preserve offsets, trials and event coding', {
  set.seed(441);d <- data.frame(x=rnorm(100),g=factor(rep(c('A','B'),50)),exposure=runif(100,1,3),trials=rep(10L,100))
  d$count <- rpois(100,exp(.3+.2*d$x)*d$exposure)
  d$successes <- rbinom(100,10,plogis(-.2+.4*d$x))
  d$binary <- factor(ifelse(d$successes>5,'yes','no'),levels=c('no','yes'))
  id <- ls_register_dataset('gc_count',d);on.exit(ls_unregister_dataset(id))
  for (distribution in c('poisson','negative_binomial','binomial_trials','perfect_score')) {
    if (distribution=='perfect_score') next # exercised separately with boundary-rich data
    extra <- if(distribution=='binomial_trials') list(trials='trials') else list(exposure='exposure')
    y <- if(distribution=='binomial_trials') 'successes' else 'count'
    a <- do.call(ls_new_count_regression,c(list(id,y,'x',distribution=distribution,native=FALSE),extra))
    b <- do.call(ls_new_count_regression,c(list(id,y,c('x','g'),distribution=distribution,native=FALSE),extra))
    result <- ls_compare_count_regression_models(a,b,native=FALSE)
    expect_true(result$tests$available)
    .gc_expect_replay(result)
  }
  a <- ls_new_binary_regression(id,'binary','x',event='no',native=FALSE)
  b <- ls_new_binary_regression(id,'binary',c('x','g'),event='no',native=FALSE)
  result <- ls_compare_binary_regression_models(a,b,native=FALSE)
  .gc_expect_replay(result)
  # A model with no neighbour still has an executable recipe.
  .gc_expect_replay(ls_compare_binary_regression_models(a,native=FALSE))
})

test_that('MI comparison verification follows D1 and zero-RIV in either direction', {
  set.seed(56);d <- data.frame(y=rnorm(80),x=rnorm(80),z=rnorm(80))
  variants <- list(
    varying=lapply(1:8,function(i)transform(d,y=exp(y+i*.05*x+rnorm(80,sd=.1)))),
    identical=rep(list(transform(d,y=exp(y))),5))
  for(name in names(variants)) {
    id <- .gc_register_mi(paste0('gc_mi_',name),variants[[name]])
    reduced <- 'x'; full <- c('x','z')
    a <- ls_new_positive_continuous_model(id,'y',reduced,distribution='lognormal',native=FALSE,.allow_intercept_only=TRUE)
    b <- ls_new_positive_continuous_model(id,'y',full,distribution='lognormal',native=FALSE)
    if (!length(reduced)) a <- ls_generalized_linear_model_fit(a)
    result <- .gc_compare(a,b)
    expect_true(result$tests$available,info=name)
    .gc_expect_replay(result);.gc_expect_replay(.gc_compare(b,a))
    if(name=='varying') .gc_execute(result,'missing_information')
    old <- readRDS(result$analysis_provenance$prepared_data_path)
    dataset <- LinkEDA:::.rls_dataset_record(id)
    dataset$completed_datasets <- lapply(dataset$completed_datasets,function(x)transform(x,y=y+100))
    dataset <- LinkEDA:::.rls_advance_data_version(dataset,'test edit')
    LinkEDA:::.rls_set_dataset_record(dataset)
    after <- .gc_compare(a,b)
    expect_equal(after$analysis_provenance$dataset_version,result$analysis_provenance$dataset_version)
    expect_equal(readRDS(after$analysis_provenance$prepared_data_path),old)
    ls_unregister_dataset(id)
  }
})

test_that('bounded mean-model comparisons have executable public recipes', {
  skip_if_not_installed('betareg');skip_if_not_installed('gamlss')
  set.seed(402)
  d <- data.frame(x=rnorm(90),z=rnorm(90));d$y <- plogis(.3*d$x+.15*d$z+rnorm(90,sd=.7))
  sets <- lapply(1:6,function(i)transform(d,y=plogis(qlogis(y)+rnorm(90,sd=.02))))
  for(distribution in c('beta','beta_one_inflated')) for(mi in c(FALSE,TRUE)) {
    current <- sets
    if(distribution=='beta_one_inflated') current <- lapply(current,function(x) {x$y[1:15]<-1;x})
    id <- if(mi) .gc_register_mi(paste0('gc_',distribution,'_mi'),current) else ls_register_dataset(paste0('gc_',distribution),current[[1]])
    a <- ls_new_proportion_model(id,'y','x',distribution=distribution,native=FALSE)
    b <- ls_new_proportion_model(id,'y',c('x','z'),distribution=distribution,native=FALSE)
    result <- ls_compare_generalized_linear_models(a,b,.model_type='proportion',native=FALSE)
    expect_true(result$tests$available,info=paste(distribution,mi,result$tests$reason))
    .gc_expect_replay(result)
    ls_unregister_dataset(id)
  }
})


test_that('the comparison recipe replays a recorded D3 with public package fits', {
  d <- data.frame(y=1:12+rep(c(0,2,1),4),g=gl(3,4))
  sets <- lapply(list(d,transform(d,y=y+rep(c(1,0,-1),4)),transform(d,y=y+rep(c(0,2,-1),4))),function(x)droplevels(x[1:8,]))
  full <- lapply(sets,function(data)glm(y~g,data=data,family=gaussian()))
  reduced <- lapply(sets,function(data)glm(y~1,data=data,family=gaussian()))
  reference <- mice::D3(mice::as.mira(full),mice::as.mira(reduced))$result
  records <- list(list(id='null',family='gaussian',link='identity',response='y',terms=character(),analysis_backend='multiple_imputation'),
                  list(id='full',family='gaussian',link='identity',response='y',terms='g',analysis_backend='multiple_imputation'))
  tests <- data.frame(available=TRUE,method="mice::D3 (fallback because mice::D1 was undefined)",full='full',reduced='null',reason='',statistic_label='D3 F')
  recipe <- LinkEDA:::.rls_generalized_comparison_verification_r_code(records,tests,rep(list(rep(list(1:8),3)),2))
  path <- tempfile(fileext='.rds');on.exit(unlink(path));saveRDS(do.call(rbind,lapply(seq_along(sets),function(i)data.frame(.imp=i,.id=1:8,sets[[i]]))),path)
  e <- new.env(parent=globalenv());e$verification_data_path <- path
  expect_false(grepl('LinkEDA:::|\\.rls_',recipe$code))
  invisible(capture.output(eval(parse(text=recipe$code),e)))
  expect_equal(unname(e$reference_comparisons[[1]]),unname(reference[1:4]),tolerance=1e-8)
})


test_that('named model lists and model-local centering retain replayable cases', {
  set.seed(284);d <- data.frame(x=rnorm(80),g=factor(rep(c('A','B'),40)),z=rnorm(80))
  d$y <- exp(1+.2*d$x+rnorm(80))
  id <- ls_register_dataset('gc_named',d);on.exit(ls_unregister_dataset(id))
  a <- ls_new_positive_continuous_model(id,'y',c('x','g'),distribution='lognormal',centered_predictors='x',factor_reference_levels=list(g='B'),native=FALSE)
  b <- ls_new_positive_continuous_model(id,'y',c('x','g','x:g'),distribution='lognormal',centered_predictors='x',factor_reference_levels=list(g='B'),native=FALSE)
  result <- .gc_compare(list(reduced=a,full=b))
  e <- .gc_expect_replay(result)
  expect_equal(unname(coef(e$model_fits[[2]][[1]])),unname(coef(ls_generalized_linear_model_state(b)$fit)))
})
