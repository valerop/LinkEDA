
.followup_register <- function(name, sets) {
  id <- ls_register_dataset(name,sets[[1L]])
  d <- LinkEDA:::.rls_dataset_record(id)
  d$dataset_type <- "multiple_imputation"; d$completed_datasets <- sets
  d$original_data <- sets[[1L]];d$imputation_count <- length(sets)
  LinkEDA:::.rls_set_dataset_record(d);id
}
.followup_replay <- function(record, group) {
  recipe <- record$analysis_provenance$verification_r_code$table
  expect_false(grepl("LinkEDA:::|\\.rls_",recipe))
  path <- tempfile(fileext=".rds");on.exit(unlink(path))
  saveRDS(LinkEDA:::.rls_export_dataset_data(LinkEDA:::.rls_dataset_record(group)),path)
  e <- new.env(parent=globalenv());e$verification_data_path<-path
  invisible(capture.output(invisible(eval(parse(text=recipe),e))));e
}

test_that("scoping ordinary and MI data preserves value-label semantics", {
  x <- structure(rep(1:3,each=4),labels=c(Yes=1,No=2),label="Agreement",units="score")
  d <- data.frame(y=c(1,2,4,3,5,7,9,8,10,11,15,13),g=x,
                  ordered=ordered(rep(c("Low","High"),6)), date=as.Date("2020-01-01")+0:11)
  for(mi in c(FALSE,TRUE)) {
    id <- if(mi) .followup_register("label_scope_mi",list(d,transform(d,y=y+rep(c(1,0,2),4)))) else ls_register_dataset("label_scope",d)
    rec <- LinkEDA:::.rls_dataset_record(id);LinkEDA:::.rls_store_data_version(rec)
    data <- ls_get_data_version(id);ids<-ls_row_ids(data)
    selected <- ls_select_rows_by_id(data,ids[c(11,3,7)])
    expect_identical(ls_row_ids(selected),ids[c(11,3,7)])
    for(s in ls_complete_data_version(selected)) {
      expect_identical(as.integer(s$g),c(3L,1L,2L))
      expect_identical(attr(s$g,"labels"),attr(x,"labels"))
      expect_identical(attr(s$g,"label"),"Agreement");expect_identical(attr(s$g,"units"),"score")
      expect_s3_class(s$ordered,"ordered");expect_s3_class(s$date,"Date")
    }
    empty <- ls_select_rows_by_id(data,character())
    expect_identical(ls_row_ids(empty),character())
    expect_identical(attr(ls_complete_data_version(empty)[[1]]$g,"labels"),attr(x,"labels"))
    internal <- LinkEDA:::.rls_dataset_subset_original_rows(rec,c(3,7,11))
    expect_identical(internal$original_row_ids,c(3L,7L,11L))
    expect_identical(attr(internal$data$g,"labels"),attr(x,"labels"))
    if(mi) {
      expect_identical(attr(internal$original_data$g,"labels"),attr(x,"labels"))
      for(s in internal$completed_datasets) expect_identical(attr(s$g,"labels"),attr(x,"labels"))
    }
    # Public Table 1 executes against the scoped data-version representation.
    h <- ls_new_table1(id,variables="y",group="g",native=FALSE)
    r <- LinkEDA:::.rls_table1_record(h)
    expect_true(all(c("Yes","No","3") %in% names(r$display_table)))
    expect_identical(r$test_results$y$rows_used_original_ids,1:12)
    if(mi) {
      e <- .followup_replay(r,id)
      expect_identical(e$group_levels,c("Yes","No","3"))
      expect_equal(e$reference_pooled_means$y$Yes$qbar,mean(c(d$y[1:4],d$y[1:4]+c(1,0,2,1))))
    }
  }
})

test_that("MI Table 1 replay distinguishes reserved group labels and omits unused levels", {
  d <- data.frame(y=c(2,4,7,8,9,13,15,18),g=factor(rep(c("Overall","p"),each=4),levels=c("Overall","p","Unused")))
  sets <- list(d,transform(d,y=y+c(0,1,0,2,0,1,0,1)))
  id <- .followup_register("table1_reserved_mi",sets)
  r <- LinkEDA:::.rls_table1_record(ls_new_table1(id,variables="y",group="g",native=FALSE))
  e <- .followup_replay(r,id)
  expect_identical(e$summary_groups,c("Overall","Overall (group)","p (group)"))
  expect_false("Unused" %in% e$group_levels)
  expect_equal(e$reference_pooled_means$y[["Overall (group)"]]$qbar,5.625)
  expect_equal(e$reference_pooled_means$y$Overall$qbar,mean(unlist(lapply(sets,`[[`,"y"))))
  expect_equal(e$reference_pooled_means$y[["p (group)"]]$qbar,mean(unlist(lapply(sets,function(d)d$y[5:8]))))
})

test_that("MI Table 1 verification reproduces D1 boundaries and recorded D3 fallback", {
  for(kind in c("zero","D3","D1")) {
    if(kind=="D3") {
      withr::local_seed(121)
      d <- data.frame(y=rnorm(9),g=gl(3,3))
      sets <- lapply(1:3,function(i)transform(d,y=y+rnorm(9,sd=2)))
    } else {
      d <- data.frame(y=c(1,4,2,3,5,9,6,7,8,13,9,11),g=gl(3,4))
      sets <- if(kind=="zero") rep(list(d),3) else lapply(1:3,function(i)transform(d,y=y+i*rep(c(0,1,2),4)))
    }
    id <- .followup_register(paste0("table1_replay_",kind),sets)
    r <- LinkEDA:::.rls_table1_record(ls_new_table1(id,variables="y",group="g",native=FALSE))
    e <- .followup_replay(r,id)
    expected <- r$test_results$y$pooled_result
    if(kind=="D3") expect_match(expected$method,"D3")
    expect_equal(unname(e$reference_numeric_tests$y$result[1,"P(>F)"]),expected$p,tolerance=1e-10)
    expect_equal(unname(e$reference_numeric_tests$y$result[1,"df2"]),expected$df2,tolerance=1e-10)
    if(kind=="zero") expect_equal(expected$p,stats::anova(lm(y~1,d),lm(y~g,d))$`Pr(>F)`[2])
  }
})

test_that("MI Table 1 never removes failed imputations before its omnibus test", {
  d <- data.frame(y=c(1,4,2,3,5,9,6,7,8,13,9,11),g=gl(3,4))
  for(position in 1:3) {
    sets <- lapply(1:3,function(i)transform(d,y=y+i*rep(c(0,1,2),4)))
    sets[[position]]$y[] <- NA_real_
    id <- .followup_register(paste0("table1_missing_fit_",position),sets)
    dataset <- LinkEDA:::.rls_dataset_record(id)
    expect_error(LinkEDA:::.rls_mi_table1_numeric_group_test(dataset,"y","g",levels(d$g)),"imputation|fit")
    expect_error(ls_new_table1(id,variables="y",group="g",native=FALSE),"imputation|fit")
  }
})


test_that("D1 accepts fitted negative-binomial theta changes without relaxing family checks", {
  skip_if_not_installed("MASS");withr::local_seed(9)
  d <- data.frame(x=rnorm(100),z=rnorm(100))
  d$y <- rnbinom(100,mu=exp(.5+.3*d$x+.1*d$z),size=2)
  sets <- lapply(1:3,function(i)transform(d,y=y+as.integer(seq_len(nrow(d)) %% (i+3)==0)))
  full <- lapply(sets,function(d)MASS::glm.nb(y~x+z,d))
  reduced <- lapply(sets,function(d)MASS::glm.nb(y~x,d))
  expect_false(identical(full[[1]]$family$family,reduced[[1]]$family$family))
  result <- LinkEDA:::.rls_mi_pool_d1(full,reduced)
  direct <- mice::D1(mice::as.mira(full),mice::as.mira(reduced))$result
  expect_equal(result$p,unname(direct[1,"P(>F)"]))
  expect_equal(result$F,unname(direct[1,"F.value"]))
  poisson <- lapply(sets,function(d)glm(y~x,d,family=poisson()))
  expect_error(LinkEDA:::.rls_mi_pool_d1(full,poisson),"same distribution")
  # Fixed-theta GLMs must not be treated as models that re-estimate theta.
  fixed <- lapply(sets,function(d)glm(y~x,d,family=MASS::negative.binomial(full[[1]]$theta)))
  expect_error(LinkEDA:::.rls_mi_pool_d1(full,fixed),"same distribution")
})
