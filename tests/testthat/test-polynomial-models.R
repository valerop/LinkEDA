.polynomial_data <- function() {
old_kind <- RNGkind()
on.exit(do.call(RNGkind, as.list(old_kind)), add = TRUE)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(827)
n <- 180; d <- data.frame(x=runif(n,-1,1), g=factor(rep(c('a','b'),90)), cluster=factor(rep(1:18,each=10)))
d$y <- 2+.6*d$x+1.5*d$x^2+rnorm(n,.0,.4)
d$count <- rpois(n,exp(.7+.4*d$x+.7*d$x^2));d$binary <- factor(rbinom(n,1,plogis(-.5+.7*d$x+.5*d$x^2)))
d$positive <- exp(.5+.3*d$x+.5*d$x^2+rnorm(n,0,.3));d$prop <- rbeta(n,3+2*d$x^2,4)
d$trials <- 12; d$score <- rbinom(n,12,rbeta(n,3,2));d$score[1:12] <- 12
terms <- c('x','I(x^2)')
 d
}
.polynomial_assert <- function(m) {
 st <- ls_generalized_linear_model_state(m)
 rows <- st$coefficient_rows
 ix <- rows$term == 'I(x^2)' & rows$row_type != 'term_parent'
 expect_true(any(ix))
 expect_true(all(is.finite(rows$estimate[ix])))
 code <- LinkEDA:::.rls_generalized_verification_r_code(st, multiple_imputation=FALSE)$code
 expect_match(code, 'I(x^2)', fixed=TRUE)
 expect_false(grepl('LinkEDA:::',code,fixed=TRUE))
 expect_silent(parse(text=code))
 st
}
test_that('polynomial addition fits a hierarchy and refits the linear model', {
 d <- .polynomial_data(); id <- ls_register_dataset('polynomial_linear_test',d)
 on.exit(ls_unregister_dataset(id),add=TRUE)
 m <- ls_new_glm(id);ls_glm_set_dependent(m,'y');ls_glm_add_polynomial(m,'x',3)
 expect_equal(ls_glm_terms(m),c('x','I(x^2)','I(x^3)'))
 ls_glm_fit(m)
 ref <- lm(y~x+I(x^2)+I(x^3),d)
 expect_equal(ls_glm_coefficients(m)$estimate,unname(coef(ref)),tolerance=1e-10)
 record <- LinkEDA:::.rls_glm_model_record(m)
 code <- LinkEDA:::.rls_linear_verification_r_code(record,FALSE)$code
 path <- tempfile(fileext='.rds');saveRDS(d,path);on.exit(unlink(path),add=TRUE)
 env <- new.env(parent=globalenv());env$verification_data_path <- path
 invisible(capture.output(eval(parse(text=code),env)))
 expect_equal(unname(coef(env$reference_model)),unname(coef(ref)),tolerance=1e-10)
 expect_error(ls_glm_add_polynomial(m,'x',2.5),'degree')
 expect_error(ls_glm_add_polynomial(m,'g',2),'numeric')
 expect_equal(LinkEDA:::.rls_model_remove_hierarchical_term(ls_glm_terms(m),'x'),character())
})
test_that('binary predictors cannot add unidentifiable polynomial powers', {
 d <- mtcars
 id <- ls_register_dataset('polynomial_binary_mtcars_test', d)
 on.exit(ls_unregister_dataset(id), add = TRUE)
 m <- ls_new_glm(id)
 ls_glm_set_dependent(m, 'drat')
 expect_error(ls_glm_add_polynomial(m, 'am', 2),
              'cannot be estimated from only 2 distinct finite values')
 expect_equal(ls_glm_terms(m), character())
 ls_glm_add_polynomial(m, 'wt', 2)
 ls_glm_fit(m)
 expect_true('I(wt^2)' %in% ls_glm_coefficients(m)$term)
 expect_true(is.finite(ls_glm_coefficients(m)$estimate[
   ls_glm_coefficients(m)$term == 'I(wt^2)']))
})
test_that('all regression families retain and estimate polynomial terms', {
 for(pkg in c('glmmTMB','gamlss','betareg','lme4')) skip_if_not_installed(pkg)
 d <- .polynomial_data(); terms <- c('x','I(x^2)')
 for(link in c('logit','probit','cloglog','log')) .polynomial_assert(ls_new_binary_regression(d,'binary',terms,link=link,native=FALSE))
 for(dist in c('poisson','quasipoisson','negative_binomial','binomial_trials','beta_binomial','hurdle_beta_binomial_ceiling','perfect_score')) {
   count <- dist %in% c('poisson','quasipoisson','negative_binomial')
   .polynomial_assert(ls_new_count_regression(d,if(count)'count'else'score',terms,distribution=dist,trials=if(count)NULL else 'trials',native=FALSE))
 }
 for(dist in c('Gamma','inverse.gaussian','gaussian_log','lognormal')) .polynomial_assert(ls_new_positive_continuous_model(d,'positive',terms,distribution=dist,native=FALSE))
 for(dist in c('beta','beta_one_inflated')) {
   dd<-d;if(dist!='beta')dd$prop[1:12]<-1
   invisible(capture.output(.polynomial_assert(ls_new_proportion_model(dd,'prop',terms,distribution=dist,native=FALSE))))
 }
 d$binary <- as.integer(as.character(d$binary))
 for(kind in c('linear','binomial','poisson','Gamma')) {
   m <- if(kind=='linear') ls_new_linear_mixed_model(d,'y',fixed=terms,random=list(list(group='cluster',terms='1')),native=FALSE) else ls_new_generalized_mixed_model(d,switch(kind,binomial='binary',poisson='count',Gamma='positive'),fixed=terms,random=list(list(group='cluster',terms='1')),family=kind,link=if(kind=='binomial')'logit'else'log',native=FALSE)
   rows<-ls_mixed_model_state(m)$fixed_effect_table
   expect_true('I(x^2)' %in% rows$term)
   expect_true(all(is.finite(rows$estimate[rows$term=='I(x^2)'])))
 }
})
test_that('multiple imputation fits polynomial terms in every completed dataset', {
 skip_if_not_installed('mice');skip_if_not_installed('glmmTMB')
 d <- .polynomial_data(); completed <- lapply(1:3,function(i){out<-d;out$x[1:5]<-out$x[1:5]+i/20;out})
 id <- ls_register_dataset('polynomial_mi_test',d);on.exit(ls_unregister_dataset(id),add=TRUE)
 dataset <- LinkEDA:::.rls_dataset_record(id)
 dataset$dataset_type <- 'multiple_imputation';dataset$imputation_id <- 'poly_imp'
 dataset$imputation_count <- 3L;dataset$completed_datasets <- completed
 dataset$original_data <- d;dataset$original_data$x[1:5]<-NA_real_
 dataset$original_row_ids <- seq_len(nrow(d));dataset$missing_cell_mask <- as.data.frame(lapply(dataset$original_data,is.na))
 LinkEDA:::.rls_set_dataset_record(dataset)
 m<-ls_new_glm(id);ls_glm_set_dependent(m,'y');ls_glm_add_polynomial(m,'x',2);ls_glm_fit(m)
 expect_true('I(x^2)' %in% ls_glm_coefficients(m)$term)
 expect_true(all(is.finite(ls_glm_coefficients(m)$estimate)))
 m<-ls_new_count_regression(id,'score',c('x','I(x^2)'),distribution='beta_binomial',trials='trials',native=FALSE)
 st<-ls_count_regression_state(m)
 expect_true('I(x^2)' %in% st$coefficient_rows$term)
 expect_true(all(is.finite(st$coefficient_rows$estimate[st$coefficient_rows$term=='I(x^2)'])))
})
