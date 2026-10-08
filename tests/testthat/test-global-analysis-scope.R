scope_test_data <- function() withr::with_seed(31, data.frame(x=rnorm(100), y=rnorm(100), group=factor(rep(c('A','B'),50))))

test_that('selection commands report native failures', {
  id <- ls_register_dataset('scope_selection_reply', scope_test_data())
  testthat::local_mocked_bindings(
    .rls_send = function(...) 'ERR selected row is outside the dataset',
    .package = 'LinkEDA')
  expect_error(ls_set_selected(id, 101L), 'outside the dataset')
  expect_error(ls_clear_selection(id), 'outside the dataset')
  expect_error(ls_invert_selection(id), 'outside the dataset')
  expect_error(ls_select_all(id), 'outside the dataset')
})

with_global_scope <- function(id, rows, code) {
  state <- LinkEDA:::.rls_state
  old <- state$process_started
  on.exit(state$process_started <- old)
  state$process_started <- TRUE
  current <- list(dataset_id=id,kind=if(is.null(rows)) 'all' else 'explicit',
    source=if(is.null(rows)) 'all_data' else 'current_selection',description=if(is.null(rows)) 'All observations' else 'Selected observations',
    rows=if(is.null(rows)) seq_len(100) else rows,n=if(is.null(rows)) 100L else length(rows),total_n=100L)
  testthat::local_mocked_bindings(ls_analysis_scope=function(group) { if(identical(group,id)) return(current); rec <- LinkEDA:::.rls_dataset_record(group); list(dataset_id=group,kind="all",description="All observations",rows=seq_len(nrow(rec$data)),n=nrow(rec$data),total_n=nrow(rec$data)) },
    .rls_send=function(...) 'OK', .package='LinkEDA')
  force(code)
}

test_that('all and selected global scopes govern public R model entry points', {
  id <- ls_register_dataset('global_scope_model',scope_test_data())
  with_global_scope(id,NULL,{
    r <- LinkEDA:::.rls_generalized_glm_record(ls_new_generalized_linear_model(id,'y','x',family='gaussian',link='identity',native=FALSE))
    expect_equal(r$data_scope$n,100L);expect_equal(length(r$rows_used),100L)
  })
  with_global_scope(id,1:20,{
    r <- LinkEDA:::.rls_generalized_glm_record(ls_new_generalized_linear_model(id,'y','x',family='gaussian',link='identity',native=FALSE))
    expect_equal(r$data_scope$n,20L);expect_equal(r$rows_used,1:20)
    expect_equal(r$data_scope$description,"Selected observations")
    expect_equal(r$analysis_provenance$analysis_scope$rows,1:20)
    expect_match(r$analysis_provenance$verification_r_code$model,'N = 20 of 100',fixed=TRUE)
  })
})

test_that('effective sample is distinct and old results retain their scope', {
  d <- scope_test_data(); d$x[1:10] <- NA
  id <- ls_register_dataset('global_scope_exclusions',d)
  result <- with_global_scope(id,1:80,LinkEDA:::.rls_generalized_glm_record(
    ls_new_generalized_linear_model(id,'y','x',family='gaussian',link='identity',native=FALSE)))
  expect_equal(result$effective_sample$scope_n,80L)
  expect_equal(result$effective_sample$analyzed_n,70L)
  expect_equal(result$effective_sample$excluded_n,10L)
  with_global_scope(id,31:100,{
    expect_equal(result$data_scope$rows,1:80)
    fresh <- LinkEDA:::.rls_capture_analysis_scope(LinkEDA:::.rls_dataset_record(id))
    expect_equal(fresh$rows,31:100)
  })
})

test_that('native request rows are immutable even if the current global scope changes', {
  id <- ls_register_dataset('global_scope_request',scope_test_data())
  with_global_scope(id,1:70,{
    snap <- LinkEDA:::.rls_capture_analysis_scope(LinkEDA:::.rls_dataset_record(id),'selected',1:40)
    expect_equal(snap$n,40L);expect_equal(snap$rows,1:40)
    all <- LinkEDA:::.rls_capture_analysis_scope(LinkEDA:::.rls_dataset_record(id),'all',integer())
    expect_equal(all$n,100L)
    empty <- LinkEDA:::.rls_capture_analysis_scope(LinkEDA:::.rls_dataset_record(id),'selected',integer())
    expect_equal(empty$n,0L)
  })
})

test_that('Missing Data and imputation capture the global scope before preparing variables', {
  d <- scope_test_data();d$y[c(1:5,31:40)] <- NA
  id <- ls_register_dataset('global_scope_missing',d)
  with_global_scope(id,1:20,{
    source <- LinkEDA:::.rls_missing_data_source(id)
    expect_equal(source$scope$n,20L);expect_equal(nrow(source$data),20L)
    expect_equal(sum(is.na(source$data$y)),5L)
    model <- ls_missingness_model(id,'y','x',native=FALSE)
    expect_equal(model$input$scope$n,20L)
    imp <- ls_new_missing_data_imputation(id,impute='y',predictors='x',m=2,maxit=1,seed=1)
    rec <- LinkEDA:::.rls_imputation_record(imp)
    expect_equal(nrow(rec$original_data),20L);expect_equal(rec$original_row_ids,1:20)
    expect_equal(rec$data_scope$n,20L)
  })
  with_global_scope(id,integer(),expect_error(ls_new_missing_data_imputation(id),'no observations'))
})


test_that("correlation, PCA and descriptives use the same global rows", {
  id <- ls_register_dataset("global_scope_other",scope_test_data())
  with_global_scope(id,31:70,{
    cor <- ls_correlation_matrix_state(ls_new_correlation_matrix(id,c("x","y"),native=FALSE))
    expect_equal(cor$data_scope$rows,31:70)
    expect_true(all(cor$results$n==40L))
    expect_equal(cor$results$rows_used_original_ids[[1L]],31:70)
    dim <- ls_dimensionality_state(ls_new_dimensionality(id,c("x","y"),parallel=FALSE,native=FALSE))
    expect_equal(dim$rows_used_original_ids,31:70)
    tab <- LinkEDA:::.rls_table1_record(ls_new_table1(id,"x",native=FALSE))
    expect_equal(tab$data_scope$rows,31:70)
    cluster <- LinkEDA:::.rls_dendrogram_record(ls_new_quick_cluster(id,c("x","y"),native=FALSE))
    expect_equal(sort(cluster$case_rows),31:70)
    if(requireNamespace("psych",quietly=TRUE)) {
      scale <- LinkEDA:::.rls_scale_record(ls_new_scale_analysis(id,c("x","y"),native=FALSE))
      expect_equal(scale$result$data_scope$rows,31:70)
      expect_equal(scale$result$summary$N,40L)
    }
  })
})

test_that("verification recipe recomputes the frozen sample after the global scope changes", {
  data <- scope_test_data();data$x[1:10] <- NA
  id <- ls_register_dataset("global_scope_export",data)
  old <- with_global_scope(id,1:80,LinkEDA:::.rls_generalized_glm_record(
    ls_new_generalized_linear_model(id,"y","x",family="gaussian",link="identity",native=FALSE)))
  with_global_scope(id,31:100,{
    path <- tempfile(fileext=".rds")
    saveRDS(data[old$data_scope$rows,,drop=FALSE],path)
    env <- new.env(parent=globalenv());env$verification_data_path <- path
    capture.output(eval(parse(text=old$analysis_provenance$verification_r_code$model),env))
    fit <- env$reference_model
    expect_equal(stats::nobs(fit),70L)
    expect_equal(unname(stats::coef(fit)),unname(stats::coef(old$fit)),tolerance=1e-10)
    expect_equal(old$data_scope$n,80L)
    expect_equal(old$effective_sample$excluded_n,10L)
  })
})
