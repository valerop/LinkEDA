
.ten_record <- function(ids) LinkEDA:::.rls_compare_means_record(ids[[1L]])
.ten_recipe <- function(code,data) {
  expect_false(grepl("LinkEDA:::|\\.rls_",code))
  path<-tempfile(fileext=".rds");saveRDS(data,path);on.exit(unlink(path))
  e<-new.env(parent=globalenv());e$verification_data_path<-path
  invisible(capture.output(invisible(eval(parse(text=code),e))));e
}
.ten_cm_recipe <- function(r,data) .ten_recipe(LinkEDA:::.rls_compare_means_verification_r_code(list(r))$code,data)
.ten_mi <- function(name,sets) {
  group<-ls_register_dataset(name,sets[[1]])
  d<-LinkEDA:::.rls_dataset_record(group);d$dataset_type<-"multiple_imputation"
  d$completed_datasets<-sets;d$original_data<-sets[[1]];d$imputation_count<-length(sets)
  LinkEDA:::.rls_set_dataset_record(d);group
}

test_that("two-sample grouping levels come from the frozen scope", {
  d<-data.frame(y=seq_len(12)+rep(c(0,2,1),4),g=gl(3,4))
  ls_register_dataset("ten_groups",d)
  for(rows in list(1:8,5:12)) {
    r<-.ten_record(ls_new_independent_samples_t_test("ten_groups","y","g",scope="selected",.selected_rows=rows))
    expected<-stats::t.test(y~g,d[rows,])
    expect_equal(r$specification$group_levels,levels(droplevels(d$g[rows])))
    expect_equal(r$test_results$p_value,expected$p.value)
    expect_equal(r$rows_used_original_ids,rows)
    e<-.ten_cm_recipe(r,d[rows,]);expect_equal(e$reference_results[[1]]$p.value,expected$p.value)
  }
  expect_error(ls_new_independent_samples_t_test("ten_groups","y","g"),"exactly two")
})

test_that("automatic independent binary tests see only effective groups and complete cases", {
  d<-data.frame(y=factor(c("no","yes","no","yes","yes","no","yes","yes",rep("other",4))),g=gl(3,4))
  ls_register_dataset("ten_binary_scope",d)
  for(selected in c(FALSE,TRUE)) {
    r<-.ten_record(if(selected) ls_new_independent_samples_t_test("ten_binary_scope","y","g",scope="selected",.selected_rows=1:8) else
      ls_new_independent_samples_t_test("ten_binary_scope","y","g",group_order=c("1","2")))
    expect_equal(r$specification$method,"proportion")
    expect_equal(r$specification$event_level,"yes")
    expect_equal(r$test_results$p_value,suppressWarnings(stats::prop.test(c(2,3),c(4,4),correct=FALSE)$p.value))
    expect_equal(r$rows_used_original_ids,1:8)
    e<-suppressWarnings(.ten_cm_recipe(r,d));expect_equal(e$reference_results[[1]]$p.value,r$test_results$p_value)
  }
  # A missing grouping value must not create a third response category.
  d$g[9:12]<-NA;ls_register_dataset("ten_binary_missing_group",d)
  r<-.ten_record(ls_new_independent_samples_t_test("ten_binary_missing_group","y","g"))
  expect_equal(r$specification$event_level,"yes")
})

test_that("ANOVA group subsets exclude other cases from descriptives and row linkage", {
  d<-data.frame(y=seq_len(12)+rep(c(0,2,1),4),g=gl(3,4))
  ls_register_dataset("ten_anova_subset",d)
  r<-.ten_record(ls_new_one_way_anova("ten_anova_subset","y","g",group_order=c("1","2"),method="classical"))
  expect_equal(unname(r$descriptives$n),c(4,4))
  expect_equal(unname(r$descriptives$mean),c(mean(d$y[1:4]),mean(d$y[5:8])))
  expect_equal(r$rows_used_original_ids,1:8)
  expect_equal(r$rows_excluded_original_ids,9:12)
  expect_equal(r$descriptives$rows_original_ids,list(1:4,5:8))
  expect_equal(r$test_results$p_value,stats::oneway.test(y~g,d[1:8,],var.equal=TRUE)$p.value)
  e<-.ten_cm_recipe(r,d);expect_equal(e$reference_results[[1]]$p.value,r$test_results$p_value)
  sets<-list(d,transform(d,y=y+rep(c(1,0,-1),4)),transform(d,y=y+rep(c(0,2,-1),4)))
  group<-.ten_mi("ten_anova_subset_mi",sets)
  r<-.ten_record(ls_new_one_way_anova(group,"y","g",group_order=c("1","2"),method="classical"))
  expect_equal(r$rows_used_original_ids,1:8)
  expect_equal(r$multiple_imputation$m,3L)
  expect_equal(r$specification$group_levels,c("1","2"))
  fits<-lapply(sets,function(x) stats::lm(y~g,x[1:8,]))
  reduced<-lapply(sets,function(x) stats::lm(y~1,x[1:8,]))
  expect_match(r$multiple_imputation$pooling_method,"mice::D3",fixed=TRUE)
  ref<-mice::D3(mice::as.mira(fits),mice::as.mira(reduced))
  expect_equal(r$test_results$p_value,unname(ref$result[4L]))
  long<-do.call(rbind,lapply(seq_along(sets),function(i) transform(sets[[i]],.imp=i,.id=1:12)))
  e<-.ten_cm_recipe(r,long);expect_equal(unname(e$reference_results[[1]]$result[4L]),r$test_results$p_value)
  # An unusable completion must not disappear from the pooled analysis.
  sets[[2]]$y[3:12]<-NA;group<-.ten_mi("ten_anova_incomplete",sets)
  expect_error(ls_new_one_way_anova(group,"y","g",group_order=c("1","2")),"imputation 2")
})

test_that("Kruskal-Wallis accepts ordered responses with explicit scoring", {
  d<-data.frame(y=ordered(c("low","low","mid","low","mid","high","high","mid","high"),levels=c("low","mid","high")),g=gl(3,3))
  ls_register_dataset("ten_kw_ordinal",d)
  r<-.ten_record(ls_new_one_way_anova("ten_kw_ordinal","y","g",method="kruskal_wallis"))
  scores<-match(as.character(d$y),levels(d$y))
  expect_equal(r$test_results$p_value,stats::kruskal.test(scores,d$g)$p.value)
  expect_equal(r$effect_sizes$epsilon_squared,effectsize::rank_epsilon_squared(scores,d$g,ci=NULL)$rank_epsilon_squared)
  expect_equal(r$specification$ordinal_levels,levels(d$y))
  e<-.ten_cm_recipe(r,d);expect_equal(e$reference_results[[1]]$p.value,r$test_results$p_value)
  payload<-LinkEDA:::.rls_compare_means_batch_native_payload(list(r),"ten_kw_ordinal_table")
  expect_true(any(grepl("M and SD summarize category scores",payload,fixed=TRUE)))
  expect_error(ls_new_one_way_anova("ten_kw_ordinal","y","g",method="classical"),"numeric")
  d$y<-factor(d$y,ordered=FALSE);ls_register_dataset("ten_kw_nominal",d)
  expect_error(ls_new_one_way_anova("ten_kw_nominal","y","g",method="kruskal_wallis"),"ordinal")
})

test_that("nonnested linear models do not produce a nested F p-value", {
  h<-ls_new_regression_comparison(mtcars,"mpg",list("hp",c("wt","disp")),native=FALSE,name="ten_nonnested_public")
  r<-ls_regression_comparison_state(h);test<-r$models[[2]]$comparison_vs_previous
  expect_equal(test$status,"not_nested");expect_true(is.na(test$p))
  # anova.lm alone produces the inappropriate finite value in this reproduction.
  expect_true(is.finite(stats::anova(r$models[[1]]$fit,r$models[[2]]$fit)$`Pr(>F)`[2]))
  e<-.ten_recipe(LinkEDA:::.rls_regression_comparison_verification_r_code(r)$code,mtcars)
  expect_true(is.na(e$model_metrics[[2]]$comparison.p.value))
  left<-list(fit=stats::lm(mpg~wt,mtcars));right<-list(fit=stats::lm(mpg~wt+hp,mtcars))
  for(reverse in c(FALSE,TRUE)) {
    a<-if(reverse)right else left;b<-if(reverse)left else right
    test<-LinkEDA:::.rls_regcmp_ordinary_nested_test(a,b)
    ref<-stats::anova(a$fit,b$fit)
    expect_equal(test$status,"ok")
    expect_equal(test$p,ref$`Pr(>F)`[2]);expect_equal(test$df1,1)
    expect_equal(test$df2,29)
  }
  same<-LinkEDA:::.rls_regcmp_ordinary_nested_test(left,left)
  expect_equal(same$status,"unchanged");expect_true(is.na(same$p))
})

test_that("equal sample sizes cannot conceal different regression cases or responses", {
  f1<-stats::lm(mpg~wt,mtcars,subset=1:28);f2<-stats::lm(mpg~wt+hp,mtcars,subset=5:32)
  test<-LinkEDA:::.rls_regcmp_ordinary_nested_test(list(fit=f1),list(fit=f2))
  expect_equal(test$status,"different_cases");expect_true(is.na(test$p))
  d<-transform(mtcars,x=wt,z=wt);d$x[1:4]<-NA;d$z[5:8]<-NA
  h<-ls_new_regression_comparison(d,"mpg",list("x",c("z","hp")),native=FALSE,name="ten_different_cases")
  r<-ls_regression_comparison_state(h)
  expect_equal(vapply(r$models,function(m) stats::nobs(m$fit),numeric(1)),c(28,28))
  expect_equal(r$models[[2]]$comparison_vs_previous$status,"different_cases")
  e<-.ten_recipe(LinkEDA:::.rls_regression_comparison_verification_r_code(r)$code,d)
  expect_true(is.na(e$model_metrics[[2]]$comparison.p.value))
  a<-list(fit=stats::lm(mpg~wt,mtcars));b<-list(fit=stats::lm(hp~wt+disp,mtcars))
  expect_equal(LinkEDA:::.rls_regcmp_ordinary_nested_test(a,b)$status,"incompatible_models")
})

test_that("a reference-only binary specification selects the other event", {
  d<-transform(mtcars,b=factor(am,levels=c(0,1),labels=c("no","yes")))
  for(reference in c("no","yes")) {
    h<-ls_new_binary_regression(d,"b","wt",reference=reference,native=FALSE,name=paste0("ten_ref_",reference))
    r<-ls_binary_regression_state(h);event<-setdiff(c("no","yes"),reference)
    ref<-stats::glm(I(b==event)~wt,d,family=stats::binomial())
    expect_equal(r$reference,reference);expect_equal(r$event,event)
    expect_equal(unname(stats::coef(r$fit)),unname(stats::coef(ref)))
    e<-.ten_recipe(LinkEDA:::.rls_generalized_verification_r_code(r)$code,d)
    expect_equal(unname(stats::coef(e$reference_fit)),unname(stats::coef(ref)))
  }
  expect_error(LinkEDA:::.rls_binary_resolve_coding(d$b,reference="absent"),"reference")
  expect_error(LinkEDA:::.rls_binary_resolve_coding(d$b,event="yes",reference="yes"),"distinct")
})

test_that("binary calibration uses log odds for every fitting link", {
  d<-data.frame(x=rep(seq(-1,1,length.out=10),30),y=factor(rep(c(0,0,1,0,0,1,0,1,0,1),30)))
  for(link in c("logit","probit","cloglog","log")) {
    h<-ls_new_binary_regression(d,"y","x",link=link,native=FALSE,name=paste0("ten_cal_",link))
    r<-ls_binary_regression_state(h)
    ref<-stats::coef(stats::glm(I(d$y=="1")~stats::qlogis(stats::fitted(r$fit)),family=stats::binomial()))
    expect_equal(r$summary$calibration_intercept,unname(ref[1]))
    expect_equal(r$summary$calibration_slope,unname(ref[2]))
    expect_match(r$summary$calibration_method,"Apparent")
    e<-.ten_recipe(LinkEDA:::.rls_generalized_verification_r_code(r)$code,d)
    expect_equal(unname(e$reference_calibration),unname(ref))
  }
})

test_that("McNemar odds preserve zero and infinity without pseudocounts", {
  d<-data.frame(a=factor(c("yes","yes","yes","yes","no","yes")),b=factor(c("no","no","no","no","no","yes")))
  ls_register_dataset("ten_matched_odds",d)
  for(reverse in c(FALSE,TRUE)) {
    pair<-if(reverse)c("b","a") else c("a","b")
    r<-.ten_record(ls_new_paired_samples_t_test("ten_matched_odds",list(pair)))
    expect_equal(r$effect_sizes$matched_odds_ratio,if(reverse)0 else Inf)
    expect_equal(r$test_results$p_value,stats::binom.test(if(reverse)0 else 4,4)$p.value)
    e<-.ten_cm_recipe(r,d)
    expect_equal(e$reference_effects[[1]]$Matched_odds_ratio,r$effect_sizes$matched_odds_ratio)
    payload<-LinkEDA:::.rls_compare_means_batch_native_payload(list(r),"ten_or_table")
    if(!reverse) {
      expect_true("Inf" %in% payload)
      fixture_dir<-Sys.getenv("LINKEDA_TEN_FIXTURES")
      if(nzchar(fixture_dir)) {
        dir.create(fixture_dir,recursive=TRUE,showWarnings=FALSE)
        writeLines(payload[-1L],file.path(fixture_dir,"matched.payload"))
      }
    }
  }
})

test_that("analysis identifiers preserve the user's R random stream", {
  withr::local_seed(3281)
  before<-.Random.seed
  ids<-replicate(100,LinkEDA:::.rls_compare_means_id())
  expect_identical(.Random.seed,before);expect_equal(length(unique(ids)),100)
  ls_register_dataset("ten_rng",data.frame(y=c(1,2,3,5,8,9)))
  before<-.Random.seed
  ls_new_one_sample_t_test("ten_rng","y")
  expect_identical(.Random.seed,before)
})
