
.pca_reference_recipe <- function(state, data) {
  code <- state$analysis_provenance$verification_r_code$table
  expect_false(grepl("LinkEDA:::|\\.rls_", code))
  p <- tempfile(fileext = ".rds"); on.exit(unlink(p)); saveRDS(data,p)
  env <- new.env(parent = globalenv());env$verification_data_path <- p
  invisible(capture.output(eval(parse(text=code),env)))
  env$reference_result
}

test_that("numeric PCA uses available pairs and preserves separate score identities", {
  d <- iris[,1:4];d[1:4,1]<-NA;d[5:8,2]<-NA
  for(standardize in c(TRUE,FALSE)) {
    m<-ls_new_dimensionality(d,variables=names(d),missing="pairwise",scale=standardize,parallel=FALSE,native=FALSE)
    s<-ls_dimensionality_state(m)
    matrix<-if(standardize) cor(d,use="pairwise.complete.obs") else cov(d,use="pairwise.complete.obs")
    reference<-stats::princomp(covmat=list(cov=matrix,n.obs=nrow(d)))
    expect_equal(unname(s$eigenvalues$eigenvalue),unname(reference$sdev^2))
    expect_identical(s$rows_used_original_ids,1:150)
    expect_identical(s$score_rows_original_ids,9:150)
    expect_equal(nrow(s$scores),142)
    recipe<-.pca_reference_recipe(s,d)
    expect_equal(unname(as.matrix(recipe$loadings[,-1])),unname(as.matrix(s$loadings[,-1])))
    expect_false(grepl("falls back",paste(s$analysis_provenance$verification_warnings,collapse=" ")))
  }
  x <- matrix(NA_real_,6,3);x[1:2,1:2]<-c(0,1,0,1);x[3:4,c(1,3)]<-c(0,1,0,1);x[5:6,2:3]<-c(0,1,1,0)
  x<-as.data.frame(x)
  expect_error(LinkEDA:::.rls_dimension_compute(x,names(x),missing="pairwise",parallel=FALSE),"not non-negative definite")
})

test_that("PCA parallel reference and unexplained variance preserve original units", {
  d<-iris[,1:4]
  set.seed(318);seed<-.Random.seed;opt<-getOption("mc.cores")
  a<-ls_new_dimensionality(d,variables=names(d),scale=FALSE,parallel_iterations=4,native=FALSE)
  sa<-ls_dimensionality_state(a)
  expect_identical(.Random.seed,seed);expect_identical(getOption("mc.cores"),opt)
  b<-ls_new_dimensionality(d*100,variables=names(d),scale=FALSE,parallel_iterations=4,native=FALSE)
  sb<-ls_dimensionality_state(b)
  expect_equal(sb$eigenvalues$parallel_eigenvalue,sa$eigenvalues$parallel_eigenvalue*10000)
  expect_equal(sb$loadings$uniqueness,sa$loadings$uniqueness*10000)
  p<-stats::prcomp(d,scale.=FALSE)
  remaining<-as.matrix(d)-predict(p)[,1:2]%*%t(p$rotation[,1:2])
  expect_equal(unname(sa$loadings$uniqueness),unname(diag(cov(remaining))),tolerance=1e-8)
  recipe<-.pca_reference_recipe(sa,d)
  expect_equal(recipe$components$Parallel,sa$eigenvalues$parallel_eigenvalue)
  expect_equal(recipe$loadings$u2,sa$loadings$uniqueness)
})

test_that("PCA rotations use public library loadings and score transformations", {
  d<-mtcars[,c("mpg","disp","hp","wt","qsec")]
  unrotated<-LinkEDA:::.rls_dimension_pca(d,names(d),3,rotation="none")
  for(method in c("varimax","quartimax")) {
    ref<-if(method=="varimax") stats::varimax(unrotated$loadings,normalize=FALSE) else GPArotation::quartimax(unrotated$loadings)
    rotated<-LinkEDA:::.rls_dimension_pca(d,names(d),3,rotation=method)
    expect_equal(unname(rotated$loadings),unname(unclass(as.matrix(ref$loadings))))
    transform<-if(method=="varimax") ref$rotmat else ref$Th
    expect_equal(unname(rotated$scores),unname(unrotated$scores%*%transform))
    model<-ls_new_dimensionality(d,variables=names(d),n_components=3,rotation=method,parallel=FALSE,native=FALSE)
    s<-ls_dimensionality_state(model);r<-.pca_reference_recipe(s,d)
    expect_equal(unname(as.matrix(r$loadings[,-1])),unname(as.matrix(s$loadings[,-1])))
  }
})

test_that("PCA bounds requested dimensions by the fitted numerical rank", {
  d<-as.data.frame(matrix(c(1,2,3,4,6,5,7,8,10,1,4,2,3,1,7),nrow=3))
  m<-ls_new_dimensionality(d,variables=names(d),n_components=5,parallel=FALSE,native=FALSE)
  s<-ls_dimensionality_state(m)
  expect_equal(s$n_components,2);expect_equal(ncol(s$scores),2)
  expect_match(s$calculation_method,"zero numerical variance")
  r<-.pca_reference_recipe(s,d)
  expect_equal(unname(as.matrix(r$loadings[,-1])),unname(as.matrix(s$loadings[,-1])))
})
