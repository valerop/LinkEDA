# Regression tests for current data, ordinal coding and disabled parallel analysis.
.review_mi <- function(name, completed, original = completed[[1L]]) {
  id <- ls_register_dataset(name, original)
  d <- LinkEDA:::.rls_dataset_record(id)
  d$dataset_type <- 'multiple_imputation'; d$original_data <- original
  d$completed_datasets <- completed; d$imputation_count <- length(completed)
  d$active_imputation_version <- 1L; d$missing_cell_mask <- lapply(original,is.na)
  LinkEDA:::.rls_store_data_version(d);LinkEDA:::.rls_set_dataset_record(d)
  id
}

test_that('dimensionality refits and verification use the current data and version', {
  x <- mtcars[,c('mpg','disp','hp','wt')]
  id <- ls_register_dataset('fresh_dimension',x);on.exit(ls_unregister_dataset(id))
  p <- ls_new_dimensionality(id,variables=names(x),n_components=2,parallel=FALSE,native=FALSE)
  d <- LinkEDA:::.rls_dataset_record(id); d$data$mpg <- rev(d$data$mpg)
  d$data$extra <- seq_len(nrow(d$data))
  d$data_frame <- d$data
  d <- LinkEDA:::.rls_advance_data_version(d,'Change inputs and add a column')
  LinkEDA:::.rls_set_dataset_record(d)
  ls_dimensionality_remove_variable(p,'wt');s <- ls_dimensionality_state(p)
  current <- stats::prcomp(d$data[,c('mpg','disp','hp')],scale.=TRUE)
  expect_equal(s$eigenvalues$eigenvalue,unname(current$sdev^2))
  expect_identical(s$data_scope$dataset_version,d$data_version)
  expect_identical(s$score_input_data,d$data[,c('mpg','disp','hp')])
  expect_type(ls_dimensionality_save_scores(p,components=1),'character')
  # Validation must see columns created after the analysis was opened.
  expect_no_error(ls_dimensionality_add_variable(p,'extra'))
  s <- ls_dimensionality_state(p)
  expect_true('extra' %in% s$variables)
  path <- tempfile(fileext='.rds');on.exit(unlink(path),add=TRUE)
  saveRDS(d$data,path)
  env <- new.env(parent=baseenv());env$verification_data_path <- path
  invisible(capture.output(eval(parse(text=s$analysis_provenance$verification_r_code$table),env)))
  expect_equal(env$reference_result$components$Eigenvalue,s$eigenvalues$eigenvalue)
})

test_that('MI refits retain the chosen imputation while refreshing all scores', {
  x <- mtcars[,c('mpg','disp','hp','wt')];second <- x;second$mpg <- rev(x$mpg)
  id <- .review_mi('fresh_dimension_mi',list(x,second));on.exit(ls_unregister_dataset(id))
  d <- LinkEDA:::.rls_dataset_record(id);d$active_imputation_version <- 2L
  LinkEDA:::.rls_set_dataset_record(d)
  p <- ls_new_dimensionality(id,variables=names(x),n_components=2,parallel=FALSE,native=FALSE)
  d <- LinkEDA:::.rls_dataset_record(id)
  d$completed_datasets[[2]]$hp <- rev(x$hp)
  d$data <- d$completed_datasets[[1]] # A different worksheet preview must not be used.
  d <- LinkEDA:::.rls_advance_data_version(d,'Edit second completion')
  LinkEDA:::.rls_set_dataset_record(d)
  ls_dimensionality_remove_variable(p,'wt');s <- ls_dimensionality_state(p)
  expected <- stats::prcomp(d$completed_datasets[[2]][,1:3],scale.=TRUE)
  expect_equal(s$eigenvalues$eigenvalue,unname(expected$sdev^2))
  expect_identical(s$active_imputation_version,2L)
  expect_identical(s$data_scope$dataset_version,d$data_version)
  name <- ls_dimensionality_save_scores(p,components=1)
  saved <- LinkEDA:::.rls_dataset_record(id)
  expect_equal(saved$completed_datasets[[2]][[name]],s$scores[[1]])
  expect_false(isTRUE(all.equal(saved$completed_datasets[[1]][[name]],saved$completed_datasets[[2]][[name]])))
})

test_that('ordinal coding and reverse coding are fixed before selecting rows', {
  d <- data.frame(q=c(1,2,10,1,2,10))
  for(reverse in list(character(),'q')) {
    spec <- LinkEDA:::.rls_scale_item_specifications(d,'q',c(q='ordinal'),reverse)
    expect_identical(spec[[1]]$ordinal_levels,c('1','2','10'))
    all <- LinkEDA:::.rls_scale_prepare_items(d,spec)
    subset <- LinkEDA:::.rls_scale_prepare_items(d[c(1,3),,drop=FALSE],spec)
    expect_equal(as.numeric(subset$q),as.numeric(all$q)[c(1,3)])
    expect_error(LinkEDA:::.rls_scale_prepare_items(data.frame(q=99),spec),'outside the frozen')
  }
  # Explicit factor levels keep their declared order, including unused levels.
  d$q <- ordered(c('high','low','high','low','low','high'),levels=c('low','middle','high'))
  spec <- LinkEDA:::.rls_scale_item_specifications(d,'q')
  expect_identical(spec[[1]]$ordinal_levels,c('low','middle','high'))
  expect_equal(as.numeric(LinkEDA:::.rls_scale_prepare_items(d,spec)$q),c(3,1,3,1,1,3))
})

test_that('native scoped totals and their R recipe share full ordinal coding', {
  set.seed(680)
  d <- data.frame(a=sample(c(1,2,10),90,TRUE),b=rnorm(90),c=rnorm(90))
  id <- ls_register_dataset('ordinal_scope_total',d);on.exit(ls_unregister_dataset(id))
  rows <- which(d$a!=2)
  local_mocked_bindings(.rls_scale_sync_native=function(...) TRUE,.package='LinkEDA')
  parts <- c('SCALE_ANALYSIS_NEEDED','ordinal_scope',id,'1','ordinal-fingerprint',
    'pearson','sum','3','FALSE','FALSE','factor','pairwise','TRUE','0','minres','none','1',
    'selected',length(rows),rows,'3',
    'a','ordinal','TRUE','FALSE','0','0','b','numeric','FALSE','FALSE','0','0',
    'c','numeric','FALSE','FALSE','0','0')
  LinkEDA:::.rls_handle_scale_analysis_needed(parts)
  s <- ls_scale_analysis_state('ordinal_scope')
  expect_identical(s$item_specifications[[1]]$ordinal_levels,c('1','2','10'))
  name <- ls_scale_analysis_save_total('ordinal_scope','sum')
  saved <- LinkEDA:::.rls_dataset_record(id)
  expected <- 4-match(d$a[rows],c(1,2,10))+d$b[rows]+d$c[rows]
  expect_equal(saved$data[[name]][rows],expected)
  expect_true(all(is.na(saved$data[[name]][-rows])))
  env <- new.env(parent=baseenv());env$data <- d
  eval(parse(text=tail(saved$data_provenance$history,1)[[1]]$r_code),env)
  expect_equal(env$data[[name]],saved$data[[name]])
})

test_that('MI ordinal totals and native verification use one frozen category map', {
  set.seed(31);n<-90
  first<-data.frame(a=sample(c(1,10),n,TRUE),b=rnorm(n),c=rnorm(n))
  second<-first;second$a[seq(1,n,3)]<-2
  id<-.review_mi('ordinal_mi',list(first,second));on.exit(ls_unregister_dataset(id))
  a<-ls_new_scale_analysis(id,items=names(first),item_types=c(a='ordinal',b='numeric',c='numeric'),
    reverse_items='a',correlation_method='pearson',native=FALSE)
  s<-ls_scale_analysis_state(a)
  expect_identical(s$item_specifications[[1]]$ordinal_levels,c('1','2','10'))
  name<-ls_scale_analysis_save_total(a,'sum');saved<-LinkEDA:::.rls_dataset_record(id)
  for(i in 1:2) {
    frame<-list(first,second)[[i]]
    expected<-4-match(frame$a,c(1,2,10))+frame$b+frame$c
    expect_equal(saved$completed_datasets[[i]][[name]],expected)
    env<-new.env(parent=baseenv());env$data<-frame
    eval(parse(text=tail(saved$data_provenance$history,1)[[1]]$r_code),env)
    expect_equal(env$data[[name]],expected)
  }
  driver<-Sys.getenv('LINKEDA_SCALE_RECIPE_DRIVER');skip_if(!nzchar(driver),'Build the shared recipe driver')
  folder<-tempfile();dir.create(folder);on.exit(unlink(folder,recursive=TRUE),add=TRUE)
  wire<-file.path(folder,'wire');recipe<-file.path(folder,'recipe.R');datafile<-file.path(folder,'data.rds')
  local_mocked_bindings(.rls_send=function(lines,...) {writeLines(lines,wire);TRUE},.package='LinkEDA')
  LinkEDA:::.rls_scale_sync_native(s,open=TRUE)
  expect_identical(system2(driver,c(shQuote(wire),shQuote(recipe),n)),0L)
  long<-rbind(cbind(.imp=1,.id=seq_len(n),first),cbind(.imp=2,.id=seq_len(n),second));saveRDS(long,datafile)
  env<-new.env(parent=baseenv());env$verification_data_path<-datafile
  invisible(capture.output(eval(parse(text=paste(readLines(recipe),collapse='\n')),env)))
  expect_identical(env$ordinal_levels$a,c('1','2','10'))
  for(i in 1:2) expect_equal(env$scale_results[[i]]$scores,
    saved$completed_datasets[[i]][[name]]/3) # Scale defaults to mean; saved total is sum.
})

test_that('disabled parallel analysis makes no fa.parallel calls including MI score saves and recipes', {
  calls<-0L
  original<-psych::fa.parallel
  local_mocked_bindings(fa.parallel=function(...) {calls<<-calls+1L; original(...)},.package='psych')
  set.seed(98);z<-rnorm(240)
  x<-as.data.frame(replicate(5,z+rnorm(240)));names(x)<-paste0('q',1:5)
  # Numeric FA, typed PCA, typed FA.
  for(method in c('factor','pca')) {
    d<-x
    if(method=='pca') d$q1<-ordered(cut(d$q1,breaks=3))
    p<-ls_new_dimensionality(d,variables=names(d),method=method,n_components=1,
      parallel=FALSE,parallel_iterations=2,native=FALSE)
    s<-ls_dimensionality_state(p)
    expect_equal(calls,0L)
    expect_true(all(is.na(s$eigenvalues$parallel_eigenvalue)))
    expect_true(all(is.finite(s$eigenvalues$eigenvalue)))
    path<-tempfile(fileext='.rds');saveRDS(d,path)
    env<-new.env(parent=baseenv());env$verification_data_path<-path
    invisible(capture.output(eval(parse(text=s$analysis_provenance$verification_r_code$table),env)))
    expect_equal(calls,0L)
    expect_equal(env$reference_result$components$Eigenvalue,s$eigenvalues$eigenvalue)
    expect_equal(as.numeric(env$reference_result$loadings[[2]]),s$loadings[[2]])
    ls_unregister_dataset(p$group);unlink(path)
  }
  id<-.review_mi('skip_parallel_save',list(x,transform(x,q1=q1+.1*q2)))
  on.exit(ls_unregister_dataset(id))
  p<-ls_new_dimensionality(id,variables=names(x),method='factor',n_components=1,
    parallel=FALSE,native=FALSE)
  expect_type(ls_dimensionality_save_scores(p,components=1),'character')
  expect_equal(calls,0L)
  # Enabling the reference still uses the public library and preserves eigenvalues.
  yes<-LinkEDA:::.rls_dimension_compute(x,names(x),'factor',1,parallel=TRUE,parallel_iterations=2)
  no<-LinkEDA:::.rls_dimension_compute(x,names(x),'factor',1,parallel=FALSE)
  expect_equal(calls,1L)
  expect_equal(yes$eigenvalues$eigenvalue,no$eigenvalues$eigenvalue)
  expect_equal(yes$loadings,no$loadings)
})
