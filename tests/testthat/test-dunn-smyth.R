nb_qr <- function(y,mu,theta,u) qnorm(pnbinom(y-1,mu=mu,size=theta)+u*(pnbinom(y,mu=mu,size=theta)-pnbinom(y-1,mu=mu,size=theta)))
nb_uniform <- function(...) LinkEDA:::.rls_stable_diagnostic_uniform(...)

test_that("case-keyed RNG is reproducible, well spread, and invariant to display order", {
  record <- list(id="rng-test", response="y", diagnostic_seed=319L)
  rows <- 1:835; u <- nb_uniform(record,rows)
  expect_equal(u,nb_uniform(record,rows),tolerance=0)
  expect_lt(min(u),.02); expect_gt(max(u),.98)
  expect_lt(abs(cor(u,rows)),.15)
  expect_equal(as.numeric(nb_uniform(record,rev(rows))), rev(as.numeric(u)),tolerance=0)
  expect_equal(as.numeric(nb_uniform(record,c(500,2,720))),as.numeric(u[c(500,2,720)]),tolerance=0)
  r <- nb_qr(rep(2,835),rep(3,835),2,u)
  expect_equal(length(unique(r)),835L)
  record$diagnostic_seed <- 320L
  expect_false(identical(as.numeric(u),as.numeric(nb_uniform(record,rows))))
  record$diagnostic_data_version <- 2L
  expect_false(identical(LinkEDA:::.rls_diagnostic_cache_key(record,NULL,rows,1L),LinkEDA:::.rls_diagnostic_cache_key(modifyList(record,list(diagnostic_data_version=3L)),NULL,rows,1L)))
  expect_error(nb_uniform(record,c(1,1)),"distinct")
})

test_that("diagnostics restore existing and absent RNG states and RNG kinds", {
  set.seed(620); before <- .Random.seed; kinds <- RNGkind()
  nb_uniform(list(id="rng"),1:20)
  expect_identical(.Random.seed,before);expect_identical(RNGkind(),kinds)
  rm(".Random.seed",envir=.GlobalEnv)
  on.exit(assign(".Random.seed",before,envir=.GlobalEnv),add=TRUE)
  nb_uniform(list(id="rng"),1:20)
  expect_false(exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))
  expect_error(LinkEDA:::.rls_with_diagnostic_seed(23,stop("test error")),"test error")
  expect_false(exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))
})

test_that("library tail limits are missing explicitly without clipping", {
  set.seed(622);d<-data.frame(y=rnbinom(150,mu=3,size=2),x=rnorm(150))
  f<-MASS::glm.nb(y~x,data=d)
  f$y[1]<-100000
  expect_warning(out<-LinkEDA:::.rls_statmod_diagnostic(list(),f,1:150),"non-finite")
  expect_true(is.na(out$residual[1]))
  f$y[1]<-.5
  expect_error(LinkEDA:::.rls_statmod_diagnostic(list(),f,1:150),"integer counts")
  expect_warning(r<-LinkEDA:::.rls_quantile_from_library_cdf(c(0,1),c(1,1),c(.5,.5)),"without clipping")
  expect_equal(r,c(0,NA_real_))
})

test_that("MASS NB diagnostics agree with statmod with shared uniforms and preserve fits", {
  skip_if_not_installed("MASS"); skip_if_not_installed("statmod")
  set.seed(622);d<-data.frame(x=rnorm(500),exposure=runif(500,.4,3))
  d$y<-rnbinom(500,mu=exp(.5+.3*d$x)*d$exposure,size=2)
  f<-MASS::glm.nb(y~x+offset(log(exposure)),data=d)
  set.seed(134);u<-runif(nrow(d));set.seed(134)
  expected<-statmod::qresiduals(f)
  expect_equal(nb_qr(d$y,as.numeric(fitted(f)),f$theta,u),as.numeric(expected),tolerance=1e-10)
  model<-ls_new_count_regression(d,"y","x",distribution="negative_binomial",exposure="exposure",native=FALSE,name="nb-qr")
  s<-ls_count_regression_state(model);diag<-s$diagnostics
  expect_equal(diag$fitted,as.numeric(fitted(f)),tolerance=1e-10)
  expect_equal(diag$diagnostic_theta,rep(f$theta,nrow(d)),tolerance=1e-10)
  expect_equal(diag$dunn_smyth_residual,nb_qr(d$y,diag$fitted,f$theta,diag$diagnostic_uniform),tolerance=1e-12)
  s$diagnostic_seed<-812L
  changed<-LinkEDA:::.rls_generalized_glm_diagnostics(s,s$fit,list(data=d,rows=seq_len(nrow(d))))
  expect_false(identical(diag$dunn_smyth_residual,changed$dunn_smyth_residual))
  expect_equal(changed$fitted,diag$fitted);expect_equal(changed$diagnostic_theta,diag$diagnostic_theta)
  expect_equal(coef(s$fit),coef(f),tolerance=1e-10)
})

test_that("NB simulation has continuous approximately standard-normal residuals", {
  set.seed(894);n<-12000;mu<-exp(runif(n,-1,2));y<-rnbinom(n,mu=mu,size=1.7)
  u<-nb_uniform(list(id="simulation",diagnostic_seed=77L),seq_len(n))
  f<-MASS::glm.nb(y~offset(log(mu))); r<-LinkEDA:::.rls_statmod_diagnostic(list(diagnostic_seed=77L),f,seq_len(n))$residual
  expect_lt(abs(mean(r)),.06);expect_lt(abs(sd(r)-1),.06)
  expect_lt(abs(cor(r,mu)),.06)
  expect_equal(as.numeric(quantile(r,c(.025,.5,.975))),qnorm(c(.025,.5,.975)),tolerance=.12)
})

test_that("two deliberately different completed fits use their own theta, mean, rows and residuals", {
  skip_if_not_installed("MASS");skip_if_not_installed("mice")
  set.seed(551);n<-220;x<-rnorm(n)
  completed<-list(data.frame(x=x,y=rnbinom(n,mu=exp(.4+.2*x),size=1)),
                  data.frame(x=x+.3,y=rnbinom(n,mu=exp(1.8-.3*x),size=6)))
  id<-ls_register_dataset("nb-two-imputations",completed[[1]])
  on.exit(ls_unregister_dataset(id),add=TRUE)
  dataset<-LinkEDA:::.rls_dataset_record(id);dataset$dataset_type<-"multiple_imputation"
  dataset$imputation_id<-"nb-two";dataset$original_data<-completed[[1]]
  dataset$completed_datasets<-completed;dataset$imputation_count<-2L
  dataset$original_row_ids<-seq_len(n);dataset$active_imputation_version<-1L;dataset$imputation_display_mode<-"version"
  LinkEDA:::.rls_set_dataset_record(dataset)
  model<-ls_new_count_regression(id,"y","x",distribution="negative_binomial",native=FALSE)
  s<-ls_count_regression_state(model)
  for(i in 1:2){
    d<-s$diagnostics_by_imputation[[i]];f<-s$fits_by_imputation[[i]]
    expect_equal(d$row_id,seq_len(n));expect_equal(d$observed,completed[[i]]$y)
    expect_equal(d$fitted,as.numeric(fitted(f)))
    expect_equal(d$diagnostic_theta,rep(f$theta,n))
    expect_equal(d$dunn_smyth_residual,nb_qr(d$observed,d$fitted,f$theta,d$diagnostic_uniform),tolerance=1e-12)
  }
  expect_false(identical(s$diagnostics_by_imputation[[1]]$diagnostic_seed,s$diagnostics_by_imputation[[2]]$diagnostic_seed))
  expect_gt(abs(s$fits_by_imputation[[1]]$theta-s$fits_by_imputation[[2]]$theta),.5)
  recipe<-LinkEDA:::.rls_dunn_smyth_verification_code(s)
  recipe_path <- tempfile("dunn-seed-recipe-", fileext = ".R")
  completed_path <- tempfile("dunn-completed-", fileext = ".rds")
  on.exit(unlink(c(recipe_path, completed_path)), add = TRUE)
  writeLines(recipe, recipe_path); saveRDS(completed, completed_path)
  expect_match(recipe,"statmod::qresiduals",fixed=TRUE)
  for(i in 1:2) {
    e<-new.env();e$observed<-completed[[i]]$y;e$diagnostic_imputation<-i
    eval(parse(text=recipe),e)
    expected<-e$diagnostic_statmod_residual(s$fits_by_imputation[[i]])
    expect_equal(expected,s$diagnostics_by_imputation[[i]]$dunn_smyth_residual)
    expect_equal(e$diagnostic_row_ids,s$diagnostics_by_imputation[[i]]$row_id)
  }
  payload<-LinkEDA:::.rls_native_generalized_state_payload(s)
  payload_path <- tempfile("linkeda-nb-mi-payload-", fileext = ".txt")
  state_path <- tempfile("linkeda-nb-mi-state-", fileext = ".rds")
  on.exit(unlink(c(payload_path, state_path)), add = TRUE)
  writeLines(payload, payload_path)
  saveRDS(s, state_path)
})

test_that("GLM discrete families agree directly with statmod and preserve RNG", {
  set.seed(318);n<-120;x<-rnorm(n)
  for(family in c("poisson","binomial")) {
    y<-if(family=="poisson")rpois(n,exp(.3+.2*x)) else rbinom(n,1,plogis(.3+.2*x))
    d<-data.frame(x=x,y=y);f<-glm(y~x,data=d,family=family)
    before<-.Random.seed
    got<-LinkEDA:::.rls_discrete_diagnostic_components(list(family=family,response="y",diagnostic_seed=93L),f,list(data=d,rows=seq_len(n)))
    expected<-withr::with_seed(93L,statmod::qresiduals(f),.rng_kind="Mersenne-Twister",.rng_normal_kind="Inversion",.rng_sample_kind="Rejection")
    expect_equal(got$dunn_smyth_residual,as.numeric(expected))
    expect_identical(.Random.seed,before)
  }
})

test_that("known NB ceiling only comes from explicit configuration or metadata", {
  rec<-list(count_regression=TRUE,count_distribution="negative_binomial",response="y")
  d<-data.frame(y=0:24)
  expect_true(is.na(LinkEDA:::.rls_nb_known_maximum(rec,d)))
  attr(d$y,"theoretical_range")<-c(0,24)
  expect_equal(LinkEDA:::.rls_nb_known_maximum(rec,d),24)
  rec$response_bounds<-c(0,30)
  expect_equal(LinkEDA:::.rls_nb_known_maximum(rec,d),30)
})

test_that("hurdle CDF and randomization use public GAMLSS distribution functions", {
  skip_if_not_installed("gamlss.dist")
  d<-data.frame(y=0:10)
  rec<-list(count_regression=TRUE,count_distribution="hurdle_beta_binomial_ceiling",response="y",trials_constant=10,diagnostic_seed=102L)
  f<-list(mu.fv=rep(.4,11),sigma.fv=rep(.2,11),nu.fv=rep(.3,11))
  got<-LinkEDA:::.rls_discrete_diagnostic_components(rec,f,list(data=d,rows=seq_len(11)))
  lo<-1-gamlss.dist::pZABB(10-d$y,mu=.4,sigma=.2,nu=.3,bd=10)
  hi<-1-gamlss.dist::pZABB(10-d$y-1,mu=.4,sigma=.2,nu=.3,bd=10)
  u<-withr::with_seed(102L,runif(11),.rng_kind="Mersenne-Twister",.rng_normal_kind="Inversion",.rng_sample_kind="Rejection")
  expect_equal(got$cdf_lower,lo);expect_equal(got$cdf_upper,hi)
  expect_equal(got$dunn_smyth_residual,qnorm(lo+u*(hi-lo)))
  expect_equal(got$pmf_observed,gamlss.dist::dZABB(10-d$y,mu=.4,sigma=.2,nu=.3,bd=10))
})

test_that("beta-binomial boundary residual uses library mass instead of a rounded CDF", {
  skip_if_not_installed("gamlss.dist")
  phi <- 7.742
  point <- gamlss.dist::dBB(24, mu = .1, sigma = 1 / phi, bd = 24)
  expect_gt(point, 0)
  result <- LinkEDA:::.rls_beta_binomial_library_quantile(
    observed = c(24, 12), trials = c(24, 24), mu = c(.1, .5),
    precision = c(phi, phi), uniform = c(.4, .6),
    lower = c(1, .45), upper = c(1, .55)
  )
  expect_equal(result$residual[[1L]],
               stats::qnorm(.6 * point, lower.tail = FALSE), tolerance = 1e-10)
  expect_equal(result$lower[[1L]], 1 - point, tolerance = 1e-12)
  expect_equal(result$upper[[1L]], 1)
  expect_equal(result$residual[[2L]], stats::qnorm(.51), tolerance = 1e-12)
})
