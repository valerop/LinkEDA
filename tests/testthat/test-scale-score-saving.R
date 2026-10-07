test_that("scale totals use base R with fixed coding, scope and reproducible provenance", {
  skip_if_not_installed("psych")
  set.seed(62)
  d <- as.data.frame(matrix(sample(1:5,120,TRUE),40,3)); names(d) <- c("a","b","c")
  d$a[3] <- NA
  id <- ls_register_dataset("scale_totals_scoped",d)
  on.exit(ls_unregister_dataset(id),add=TRUE)
  testthat::local_mocked_bindings(.rls_scale_sync_native=function(...) TRUE,.package="LinkEDA")
  parts <- c("SCALE_ANALYSIS_NEEDED","score_scope",id,"1","score-fingerprint",
    "pearson","mean","2","FALSE","FALSE","factor","pairwise","TRUE","0","minres","none","1",
    "selected","5","1","3","7","9","12","3",
    "a","numeric","FALSE","FALSE","0","0", "b","numeric","TRUE","TRUE","1","5",
    "c","numeric","FALSE","FALSE","0","0")
  LinkEDA:::.rls_handle_scale_analysis_needed(parts)
  rows <- c(1,3,7,9,12)
  expected <- d[rows,]; expected$b <- 6-expected$b
  total <- ls_scale_analysis_save_total("score_scope","sum")
  mean_name <- ls_scale_analysis_save_total("score_scope","mean")
  record <- LinkEDA:::.rls_dataset_record(id)
  expect_equal(record$data[[total]][rows],unname(rowSums(expected,na.rm=TRUE)))
  expect_equal(record$data[[mean_name]][rows],unname(rowMeans(expected,na.rm=TRUE)))
  expect_true(all(is.na(record$data[[total]][-rows])))
  history <- tail(record$data_provenance$history,1)[[1]]
  e <- new.env(parent=baseenv());e$data <- d
  eval(parse(text=history$r_code),e)
  expect_equal(e$data[[mean_name]],record$data[[mean_name]])
  expect_identical(history$input_columns,names(d))
  expect_identical(history$stable_row_ids,record$stable_row_ids[rows])
  expect_identical(names(ls_get_data_version(id,1)),names(d))
  again <- ls_scale_analysis_save_total("score_scope","sum")
  expect_false(again==total)
  record <- LinkEDA:::.rls_dataset_record(id);record$data$a[1] <- 99
  LinkEDA:::.rls_set_dataset_record(record)
  expect_error(ls_scale_analysis_save_total("score_scope","sum"),"Refit")
})

test_that("factor and total scores survive all MI representations and mice round trips", {
  skip_if_not_installed("psych");skip_if_not_installed("mice")
  set.seed(121); n <- 100; z <- rnorm(n)
  d <- as.data.frame(replicate(5,z+rnorm(n)));names(d)<-paste0("q",1:5)
  d$q1[seq(1,n,7)]<-NA;d$q2[seq(3,n,8)]<-NA
  imp <- mice::mice(d,m=3,maxit=2,printFlag=FALSE)
  id <- ls_import_mice(imp,name="factor_totals_mi",make_active=FALSE)
  on.exit(ls_unregister_dataset(id),add=TRUE)
  a <- ls_new_scale_analysis(id,items=names(d),dimensionality=TRUE,factors=1,
    parallel_iterations=2,native=FALSE)
  state <- ls_scale_analysis_state(a)
  added <- ls_scale_analysis_save_scores(a,components=1)
  total <- ls_scale_analysis_save_total(a,"sum")
  average <- ls_scale_analysis_save_total(a,"mean")
  record <- LinkEDA:::.rls_dataset_record(id)
  for (i in seq_len(imp$m)) {
    frame <- record$completed_datasets[[i]]
    expected <- state$result$dimensionality$results_by_imputation[[i]]$scores
    expect_equal(frame[[added]][expected$row],expected[[2]])
    expect_equal(frame[[total]],rowSums(mice::complete(imp,i)))
    expect_equal(frame[[average]],rowMeans(mice::complete(imp,i)))
    for (v in c(added,total,average))
      expect_equal(mice::complete(record$mids_object,i)[[v]],frame[[v]])
  }
  expect_equal(record$data[[added]],record$completed_datasets[[1]][[added]])
  expect_identical(record$mids_object$method[names(imp$method)], imp$method)
  expect_identical(record$mids_object$predictorMatrix[names(d),names(d)], imp$predictorMatrix)
  expect_identical(record$mids_object$chainMean,imp$chainMean)
  expect_identical(record$mids_object$chainVar,imp$chainVar)
  expect_true(all(is.na(record$original_data[[added]])))
  expect_equal(record$original_data[[total]],rowSums(d))
  expect_identical(record$missing_cell_mask[[total]],is.na(rowSums(d)))
  expect_true(all(c(added,total,average) %in% record$variable_metadata$variable))
  # Saving again updates the same factor-score column instead of duplicating it.
  expect_identical(ls_scale_analysis_save_scores(a,components=1),added)
  different <- ls_register_dataset("wrong_scale_target",d)
  on.exit(ls_unregister_dataset(different),add=TRUE)
  expect_error(ls_scale_analysis_save_scores(a,dataset=different),"dataset used")
})


test_that("biplot score rows refer to original cases under a subset scope", {
  skip_if_not_installed("psych")
  set.seed(939)
  z <- rnorm(160)
  d <- as.data.frame(replicate(5, z + rnorm(160)))
  names(d) <- paste0("q", 1:5)
  id <- ls_register_dataset("biplot_scope_rows", d)
  on.exit(ls_unregister_dataset(id), add = TRUE)
  dataset <- LinkEDA:::.rls_dataset_record(id)
  rows <- seq(2L, 160L, by = 2L)
  dataset$analysis_scope_snapshot <- list(rows = rows)
  specs <- LinkEDA:::.rls_scale_item_specifications(d, names(d))
  options <- LinkEDA:::.rls_scale_default_options(dimensionality = TRUE, factors = 1,
    parallel_iterations = 1)
  result <- LinkEDA:::.rls_scale_fit(dataset, specs, options)
  series <- Filter(function(x) x$kind == "factor_score_imputation", result$plot_series)
  expect_length(series, 1L)
  expect_identical(as.integer(series[[1]]$labels), rows)
  expect_identical(result$dimensionality$scores$row, rows)
})


test_that("score saving reports failures across imputations without modifying data", {
  skip_if_not_installed("psych")
  skip_if_not_installed("mice")
  set.seed(1217)
  z <- stats::rnorm(90)
  d <- as.data.frame(replicate(5, z + stats::rnorm(90, sd = 0.6)))
  names(d) <- paste0("q", seq_len(5))
  d$q1[seq(2, 90, by = 9)] <- NA_real_
  imputed <- mice::mice(d, m = 2, maxit = 1, printFlag = FALSE)
  id <- ls_import_mice(imputed, name = "score_availability_mi", make_active = FALSE)
  on.exit(ls_unregister_dataset(id), add = TRUE)
  analysis <- ls_new_scale_analysis(id, items = names(d), dimensionality = TRUE,
    factors = 1, parallel_iterations = 1, native = FALSE)
  record <- LinkEDA:::.rls_scale_record(analysis)
  expect_true(nrow(record$result$dimensionality$results_by_imputation[[1L]]$scores) > 0L)
  record$result$dimensionality$results_by_imputation[[2L]]$scores <- data.frame()
  record$result$dimensionality$results_by_imputation[[2L]]$score_status <-
    "Correlation matrix is not positive definite (negative eigenvalue)."
  LinkEDA:::.rls_assign_scale(record)
  availability <- LinkEDA:::.rls_scale_score_availability(
    record$result$dimensionality$results_by_imputation)
  expect_identical(availability$available, 1L)
  expect_identical(availability$failed, 2L)
  before <- names(LinkEDA:::.rls_dataset_record(id)$data)
  expect_error(ls_scale_analysis_save_scores(analysis),
    "only 1 have valid scores.*Imputation 2")
  expect_identical(names(LinkEDA:::.rls_dataset_record(id)$data), before)
})
