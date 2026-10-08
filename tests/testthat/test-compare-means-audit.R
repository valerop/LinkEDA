
.cma_record <- function(ids) LinkEDA:::.rls_compare_means_record(ids[[1L]])
.cma_recipe <- function(result, data) {
  code <- LinkEDA:::.rls_compare_means_verification_r_code(list(result))$code
  expect_false(grepl("LinkEDA:::|\\.rls_", code))
  path <- tempfile(fileext=".rds");saveRDS(data,path);on.exit(unlink(path))
  e <- new.env(parent=globalenv());e$verification_data_path <- path
  invisible(capture.output(eval(parse(text=code),e)))
  e
}

test_that("constant groups do not incorrectly block estimable t or classical F tests", {
  d <- data.frame(y=c(1,1,1,2,3,5),g=factor(rep(c("A","B"),each=3)))
  ls_register_dataset("cma_constant",d)
  for(equal in c(FALSE,TRUE)) {
    r <- .cma_record(ls_new_independent_samples_t_test("cma_constant","y","g",var_equal=equal))
    ref <- stats::t.test(y~g,d,var.equal=equal)
    expect_equal(r$test_results$p_value,ref$p.value)
    expect_equal(r$test_results$statistic,unname(ref$statistic))
    expect_equal(r$test_results$conf_int,unname(ref$conf.int),ignore_attr=TRUE)
    expect_equal(r$test_results$mean_diff_se,unname(ref$stderr))
    e <- .cma_recipe(r,d)
    expect_equal(e$reference_results[[1]]$p.value,ref$p.value)
  }
  r <- .cma_record(ls_new_one_way_anova("cma_constant","y","g",method="classical"))
  expect_equal(r$test_results$p_value,stats::oneway.test(y~g,d,var.equal=TRUE)$p.value)
  d$y <- c(1,1,1,2,2,2);ls_register_dataset("cma_all_constant",d)
  expect_error(ls_new_independent_samples_t_test("cma_all_constant","y","g"),"constant")
})

test_that("MI t tests keep valid constant-group completions and reproduce mice", {
  d <- data.frame(y=c(1,1,1,2,3,5),g=factor(rep(c("A","B"),each=3)))
  completed <- list(d,transform(d,y=y+c(1,1,1,0,1,2)),transform(d,y=y+c(2,2,2,1,3,1)))
  group <- ls_register_dataset("cma_constant_mi",d)
  ds <- LinkEDA:::.rls_dataset_record(group);ds$dataset_type <- "multiple_imputation"
  ds$completed_datasets <- completed;ds$original_data <- d;ds$imputation_count <- 3L
  LinkEDA:::.rls_set_dataset_record(ds)
  for(equal in c(FALSE,TRUE)) {
    r <- .cma_record(ls_new_independent_samples_t_test(group,"y","g",var_equal=equal))
    refs <- lapply(completed,function(x) stats::t.test(y~g,x,var.equal=equal))
    q <- vapply(refs,function(x) unname(x$estimate[1]-x$estimate[2]),numeric(1))
    u <- vapply(refs,function(x) unname(x$stderr)^2,numeric(1))
    df <- min(vapply(refs,function(x) unname(x$parameter),numeric(1)))
    pool <- mice::pool.scalar(q,u,n=df+1,k=1)
    expect_equal(r$test_results$mean_diff,pool$qbar)
    expect_equal(r$test_results$mean_diff_se,sqrt(pool$t))
    expect_equal(r$test_results$parameter,pool$df)
    expect_equal(r$descriptives$n_imputations,3L)
    long <- do.call(rbind,lapply(seq_along(completed),function(i) transform(completed[[i]],.imp=i,.id=1:6)))
    e <- .cma_recipe(r,long)
    expect_equal(e$reference_results[[1]]$estimate,pool$qbar)
    expect_equal(e$reference_results[[1]]$std.error,sqrt(pool$t))
  }
})

test_that("standardized effects and one-sided intervals use public effectsize routines", {
  d <- data.frame(x=c(1,2,3,6,4,8),y=c(0,1,4,2,3,4),g=factor(rep(c("A","B"),each=3)))
  ls_register_dataset("cma_effects",d)
  for(alt in c("two.sided","less","greater")) {
    a <- .cma_record(ls_new_one_sample_t_test("cma_effects","x",mu=2,alternative=alt))
    ref <- effectsize::cohens_d(d$x,mu=2,alternative=alt,verbose=FALSE)
    expect_equal(a$effect_sizes$cohens_d,ref$Cohens_d)
    expect_equal(a$effect_sizes$effect_size_ci,c(ref$CI_low,ref$CI_high))
    e <- .cma_recipe(a,d)
    expect_equal(e$reference_effects[[1]]$Cohens_d,ref$Cohens_d)
    b <- .cma_record(ls_new_paired_samples_t_test("cma_effects",list(c("x","y")),alternative=alt))
    ref <- effectsize::cohens_d(d$x,d$y,paired=TRUE,alternative=alt,verbose=FALSE)
    expect_equal(b$effect_sizes$cohens_dz,ref$Cohens_d)
    expect_equal(b$effect_sizes$effect_size_ci,c(ref$CI_low,ref$CI_high))
    e <- .cma_recipe(b,d)
    expect_equal(e$reference_effects[[1]]$CI_low,ref$CI_low)
  }
  tiny <- data.frame(y=c(1,2,3,6),g=factor(c("A","A","B","B")))
  ls_register_dataset("cma_hedges",tiny)
  a <- .cma_record(ls_new_independent_samples_t_test("cma_hedges","y","g"))
  ref <- effectsize::hedges_g(c(1,2),c(3,6),verbose=FALSE)
  expect_equal(a$effect_sizes$hedges_g,ref$Hedges_g,tolerance=1e-14)
  expect_equal(a$effect_sizes$effect_size_ci,c(ref$CI_low,ref$CI_high))
  expect_gt(abs(a$effect_sizes$hedges_g-(-1.084209)),.01)
  e <- .cma_recipe(a,tiny)
  expect_equal(e$reference_effects[[1]]$Hedges_g,ref$Hedges_g)
  reverse <- .cma_record(ls_new_independent_samples_t_test("cma_hedges","y","g",group_order=c("B","A")))
  expect_equal(reverse$effect_sizes$hedges_g,-a$effect_sizes$hedges_g)
})

test_that("Kruskal-Wallis reports rank epsilon rather than rank eta", {
  d <- data.frame(y=c(1,2,3,4,5,8,6,9,10),g=gl(3,3))
  ls_register_dataset("cma_epsilon",d)
  r <- .cma_record(ls_new_one_way_anova("cma_epsilon","y","g",method="kruskal_wallis"))
  ref <- effectsize::rank_epsilon_squared(d$y,d$g,ci=NULL)$rank_epsilon_squared
  eta <- effectsize::rank_eta_squared(d$y,d$g,ci=NULL)$rank_eta_squared
  expect_equal(r$effect_sizes$epsilon_squared,ref)
  expect_gt(abs(ref-eta),.05)
  e <- .cma_recipe(r,d)
  expect_equal(e$reference_effects[[1]]$rank_epsilon_squared,ref)
})

test_that("paired ordinal scores require identical scales and export their actual mapping", {
  levels <- c("low","mid","high")
  d <- data.frame(a=ordered(c("low","mid","high","low","mid","high","mid","low"),levels=levels),
    b=ordered(c("low","mid","high","mid","high","high","mid","high"),levels=rev(levels)))
  ls_register_dataset("cma_incompatible",d)
  expect_error(ls_new_paired_samples_t_test("cma_incompatible",list(c("a","b")),method="wilcoxon"),"common scale")
  d$b <- ordered(d$b,levels=levels);ls_register_dataset("cma_ordinal",d)
  r <- suppressWarnings(.cma_record(ls_new_paired_samples_t_test("cma_ordinal",list(c("a","b")),method="wilcoxon")))
  ref <- suppressWarnings(stats::wilcox.test(match(as.character(d$a),levels),match(as.character(d$b),levels),paired=TRUE,exact=FALSE,conf.int=TRUE))
  expect_equal(r$specification$ordinal_levels,levels)
  expect_equal(r$test_results$p_value,ref$p.value)
  e <- suppressWarnings(.cma_recipe(r,d))
  expect_equal(e$reference_results[[1]]$p.value,ref$p.value)
  payload <- LinkEDA:::.rls_compare_means_batch_native_payload(list(r),"cma_ordinal_output")
  expect_true(any(grepl("low = 1; mid = 2; high = 3",payload,fixed=TRUE)))
  # Independently verify the two other ordinal routes formerly passed factors to wilcox.test.
  d$g <- gl(2,4);ls_register_dataset("cma_ordinal_other",d)
  r <- suppressWarnings(.cma_record(ls_new_one_sample_t_test("cma_ordinal_other","a",mu=2,method="wilcoxon")))
  e <- suppressWarnings(.cma_recipe(r,d));expect_equal(e$reference_results[[1]]$p.value,r$test_results$p_value)
  r <- suppressWarnings(.cma_record(ls_new_independent_samples_t_test("cma_ordinal_other","a","g",method="mann_whitney")))
  e <- suppressWarnings(.cma_recipe(r,d));expect_equal(e$reference_results[[1]]$p.value,r$test_results$p_value)
})

test_that("automatic paired tests use complete pairs in the frozen scope", {
  d <- data.frame(a=factor(c("no","yes","no","yes","no","yes","other","other")),
    b=factor(c("yes","yes","yes","no","yes","yes","other",NA)))
  ls_register_dataset("cma_scope_method",d)
  for(rows in list(1:6,c(1:6,8))) {
    r <- .cma_record(ls_new_paired_samples_t_test("cma_scope_method",list(c("a","b")),scope="selected",.selected_rows=rows))
    expect_identical(r$specification$method,"mcnemar")
    expect_equal(r$rows_used_original_ids,1:6)
    expect_equal(r$data_scope$rows,as.integer(rows))
    expect_equal(r$test_results$p_value,stats::binom.test(1,4)$p.value)
    e <- .cma_recipe(r,d[1:6,])
    expect_equal(e$reference_results[[1]]$p.value,r$test_results$p_value)
  }
  expect_error(ls_new_paired_samples_t_test("cma_scope_method",list(c("a","b"))),"must be numeric")
})

# Opt-in fixtures generated from real R analyses for the shared native parser.
fixture_folder <- Sys.getenv("LINKEDA_COMPARE_AUDIT_FIXTURES")
if (nzchar(fixture_folder)) {
  dir.create(fixture_folder,recursive=TRUE,showWarnings=FALSE)
  d <- data.frame(y=c(1,2,3,6),g=factor(c("A","A","B","B")))
  ls_register_dataset("cma_fixture_g",d)
  g <- .cma_record(ls_new_independent_samples_t_test("cma_fixture_g","y","g"))
  k <- data.frame(y=c(1,2,3,4,5,8,6,9,10),g=gl(3,3))
  ls_register_dataset("cma_fixture_kw",k)
  kw <- .cma_record(ls_new_one_way_anova("cma_fixture_kw","y","g",method="kruskal_wallis"))
  results <- list(hedges=g,epsilon=kw)
  expected <- list(hedges=c(stats::t.test(y~g,d)$p.value,effectsize::hedges_g(c(1,2),c(3,6),ci=NULL,verbose=FALSE)$Hedges_g),
    epsilon=c(stats::kruskal.test(k$y,k$g)$p.value,effectsize::rank_epsilon_squared(k$y,k$g,ci=NULL)$rank_epsilon_squared))
  for (name in names(results)) {
    payload <- LinkEDA:::.rls_compare_means_batch_native_payload(list(results[[name]]),paste0("cma_native_",name))
    writeLines(payload[-1],file.path(fixture_folder,paste0(name,".payload")))
    writeLines(format(expected[[name]],digits=17,scientific=TRUE),file.path(fixture_folder,paste0(name,".expected")))
  }
}
