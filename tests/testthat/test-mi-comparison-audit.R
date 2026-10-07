.more_register <- function(name, sets, missing = "y") {
  id <- ls_register_dataset(name, sets[[1L]])
  d <- LinkEDA:::.rls_dataset_record(id)
  d$dataset_type <- "multiple_imputation"; d$completed_datasets <- sets
  d$imputation_count <- length(sets); d$original_data <- sets[[1L]]
  for (v in missing) d$original_data[[v]][1:4] <- NA
  d$missing_cell_mask <- lapply(d$original_data, is.na)
  LinkEDA:::.rls_set_dataset_record(d); id
}
.more_execute <- function(code, data) {
  expect_false(grepl("LinkEDA:::|\\.rls_", code))
  path <- tempfile(fileext=".rds"); on.exit(unlink(path)); saveRDS(data, path)
  e <- new.env(parent=globalenv()); e$verification_data_path <- path
  invisible(capture.output(invisible(eval(parse(text=code), e)))); e
}
.more_sets <- function() {
  withr::local_seed(56)
  d <- data.frame(y=rnorm(80), x=rnorm(80), z=rnorm(80))
  lapply(1:5, function(i) transform(d, y=y+i*.05*x+rnorm(80,sd=.1)))
}

test_that("removing terms uses the same pooled nested comparison in reverse", {
  s <- .more_sets(); id <- .more_register("more_remove",s)
  h <- ls_new_regression_comparison(id,"y",list("x",c("x","z"),"x"),native=FALSE)
  r <- LinkEDA:::.rls_regcmp_record(h)
  added <- r$models[[2]]$comparison_vs_previous
  removed <- r$models[[3]]$comparison_vs_previous
  expect_equal(removed$status,"ok"); expect_equal(removed$direction,"removed")
  expect_equal(removed$p,added$p); expect_equal(removed$F,added$F)
  expect_equal(removed$df1,added$df1); expect_equal(removed$df2,added$df2)
  expect_match(removed$detail,"removed"); expect_match(r$analysis_provenance$executed_r_code,"rls_regcmp_mi_nested_test")
  d <- LinkEDA:::.rls_export_dataset_data(LinkEDA:::.rls_dataset_record(id))
  e <- .more_execute(r$analysis_provenance$verification_r_code$comparison,d)
  expect_equal(e$model_metrics[[2]]$comparison.p.value,added$p)
  expect_equal(e$model_metrics[[3]]$comparison.p.value,removed$p)
  q <- r$models[[1]]
  expect_equal(LinkEDA:::.rls_regcmp_mi_nested_test(q,q)$status,"unchanged")
})

test_that("D1 rejects reused coefficient names with incompatible values or coding", {
  s <- .more_sets()
  reduced <- lapply(s,function(d)stats::lm(y~x,d))
  full <- lapply(s,function(d)stats::lm(y~x+z,transform(d,x=x^2)))
  expect_error(LinkEDA:::.rls_mi_pool_d1(full,reduced),"different coding or predictor values")
  good <- lapply(s,function(d)stats::lm(y~x+z,d))
  expect_true(LinkEDA:::.rls_mi_pool_d1(good,reduced)$ok)
  weighted <- lapply(s,function(d)stats::lm(y~x+z,d,weights=1+abs(z)))
  expect_error(LinkEDA:::.rls_mi_pool_d1(weighted,reduced),"same weights and offset")
  offset <- lapply(s,function(d)stats::lm(y~x+z,d,offset=z/10))
  expect_error(LinkEDA:::.rls_mi_pool_d1(offset,reduced),"same weights and offset")
  reordered <- lapply(s,function(d)stats::lm(y~z+x,d))
  # Shared x alone is still in the same relative order; this valid pair is allowed.
  expect_true(LinkEDA:::.rls_mi_pool_d1(reordered,reduced)$ok)
  different <- list(terms=c("x","z"),fits_by_imputation=full)
  previous <- list(terms="x",fits_by_imputation=reduced)
  expect_equal(LinkEDA:::.rls_regcmp_mi_nested_test(previous,different)$status,"incompatible_models")
  restricted <- lapply(good,LinkEDA:::.rls_mi_term_restriction,remove_term="z")
  expect_true(LinkEDA:::.rls_mi_pool_d1(good,restricted,allow_d3_fallback=FALSE)$ok)
})

test_that("comparison verification reads the recorded D3 structured result", {
  d <- data.frame(y=1:12+rep(c(0,2,1),4),g=gl(3,4))
  s <- list(d,transform(d,y=y+rep(c(1,0,-1),4)),transform(d,y=y+rep(c(0,2,-1),4)))
  s <- lapply(s,function(d)droplevels(d[1:8,]))
  id <- .more_register("more_d3",s)
  h <- ls_new_regression_comparison(id,"y",list("g"),native=FALSE)
  r <- LinkEDA:::.rls_regcmp_record(h)
  expect_match(r$models[[1]]$summary$global_test_method,"mice::D3",fixed=TRUE)
  e <- .more_execute(r$analysis_provenance$verification_r_code$comparison,
    LinkEDA:::.rls_export_dataset_data(LinkEDA:::.rls_dataset_record(id)))
  expected <- mice::D3(mice::as.mira(lapply(s,function(d)stats::lm(y~g,d))),
                      mice::as.mira(lapply(s,function(d)stats::lm(y~1,d))))$result
  expect_equal(e$model_metrics[[1]]$p.value,unname(expected[4L]))
  expect_equal(e$model_metrics[[1]]$statistic,r$models[[1]]$summary$global_f)
  # The helper also accepts D1's matrix output and the exact-zero-RIV limit.
  repeated <- mice::as.mira(rep(list(stats::lm(y~g,s[[1]])),3))
  null <- mice::as.mira(rep(list(stats::lm(y~1,s[[1]])),3))
  test <- suppressWarnings(e$pooled_test(repeated,null,"mice::D1"))
  expect_equal(unname(test['p']),stats::anova(null$analyses[[1]],repeated$analyses[[1]])$`Pr(>F)`[2])
})

test_that("MI verification includes the displayed multiple-testing adjustment", {
  s <- lapply(.more_sets(),function(d)transform(d,y=y+.25,x=x+.1))
  id <- .more_register("more_adjusted",s)
  for(method in c("holm","bonferroni","none")) {
    ids <- ls_new_one_sample_t_test(id,c("y","x"),p_adjust=method)
    r <- lapply(ids,LinkEDA:::.rls_compare_means_record)
    e <- .more_execute(LinkEDA:::.rls_compare_means_verification_r_code(r)$code,
      LinkEDA:::.rls_export_dataset_data(LinkEDA:::.rls_dataset_record(id)))
    expect_equal(unname(e$raw_p),vapply(r,function(x)x$test_results$p_value,numeric(1)))
    expect_equal(unname(e$adjusted_p),vapply(r,function(x)x$test_results$p_adjusted,numeric(1)))
  }
  id <- .more_register("more_adjusted_anova",lapply(s,function(d)transform(d,g=factor(rep(c("A","B"),40)))))
  ids <- ls_new_one_way_anova(id,c("y","x"),"g",p_adjust="bonferroni")
  r <- lapply(ids,LinkEDA:::.rls_compare_means_record)
  e <- .more_execute(LinkEDA:::.rls_compare_means_verification_r_code(r)$code,
    LinkEDA:::.rls_export_dataset_data(LinkEDA:::.rls_dataset_record(id)))
  expect_equal(unname(e$adjusted_p),vapply(r,function(x)x$test_results$p_adjusted,numeric(1)))
})

test_that("comparison export does not pool wholly observed model inputs", {
  s <- .more_sets(); for(i in seq_along(s)) {s[[i]]$y<-s[[1]]$y;s[[i]]$x<-s[[1]]$x}
  id <- .more_register("more_observed",s,missing="z")
  h <- ls_new_regression_comparison(id,"y",list(character(),"x"),native=FALSE)
  r <- LinkEDA:::.rls_regcmp_record(h)
  expect_false(r$mi_pooling_required)
  code <- r$analysis_provenance$verification_r_code$comparison
  expect_match(code,"No model inputs were imputed",fixed=TRUE)
  expect_false(grepl("mice::pool(",code,fixed=TRUE))
  e <- .more_execute(code,LinkEDA:::.rls_export_dataset_data(LinkEDA:::.rls_dataset_record(id)))
  expect_equal(e$model_metrics[[2]]$comparison.p.value,r$models[[2]]$comparison_vs_previous$p)
  expect_equal(e$model_metrics[[2]]$r.squared,r$models[[2]]$summary$r_squared)
  expect_equal(e$model_metrics[[2]]$nobs,nrow(s[[1]]))
})
