.audit_mi_dataset <- function(name, completed) {
  group <- ls_register_dataset(name,completed[[1L]])
  ds <- LinkEDA:::.rls_dataset_record(group)
  ds$dataset_type <- "multiple_imputation";ds$completed_datasets <- completed
  ds$original_data <- completed[[1L]];ds$imputation_count <- length(completed)
  LinkEDA:::.rls_set_dataset_record(ds);group
}
.audit_recipe <- function(code,data) {
  expect_false(grepl("LinkEDA:::|\\.rls_",code))
  path <- tempfile(fileext=".rds");saveRDS(data,path);on.exit(unlink(path))
  env <- new.env(parent=globalenv());env$verification_data_path <- path
  invisible(eval(parse(text=code),env));env
}

test_that("removing MI contingency split uses total N, not 100 percent per category", {
  sets <- list(data.frame(x=factor(c("A","A","B","B"),levels=c("A","B","unused"))),
    data.frame(x=factor(c("A","B","B","B"),levels=c("A","B","unused"))))
  group <- .audit_mi_dataset("audit_unsplit",sets)
  r <- LinkEDA:::.rls_mi_contingency_record(group,"x","",id="audit_unsplit_table")
  expect_equal(as.numeric(r$mean_row_percentages),c(.375,.625,0,1))
  expect_equal(as.numeric(r$mean_counts),c(1.5,2.5,0,4))
  long <- do.call(rbind,lapply(seq_along(sets),function(i) transform(sets[[i]],.imp=i,.id=1:4)))
  e <- .audit_recipe(r$analysis_provenance$verification_r_code$table,long)
  expect_equal(e$reference_mean_row_percentages,r$mean_row_percentages)
  sets[[2]]$x[] <- NA
  group <- .audit_mi_dataset("audit_unsplit_empty",sets)
  r <- LinkEDA:::.rls_mi_contingency_record(group,"x","",id="audit_unsplit_empty_table")
  expect_equal(as.numeric(r$mean_row_percentages),c(.5,.5,0,1))
  expect_equal(as.numeric(r$mean_counts),c(1,1,0,2))
})

test_that("reserved group labels do not overwrite Table 1 statistics or contrasts", {
  groups <- c("Overall","p","Test","Variable","Overall (group)")
  data <- data.frame(y=seq_len(50),g=factor(rep(groups,each=10),levels=c(groups,"unused")))
  expected <- c("Overall","Overall (group)","p (group)","Test (group)","Variable (group)","Overall (group).1")
  for(mi in c(FALSE,TRUE)) {
    group <- if(mi) .audit_mi_dataset("audit_reserved_mi",list(data,transform(data,y=y+2))) else ls_register_dataset("audit_reserved",data)
    tab <- ls_new_table1(group,"y",group="g",variable_types=c(y="numeric",g="categorical"),native=FALSE)
    rec <- LinkEDA:::.rls_table1_record(tab)
    expect_identical(setdiff(names(rec$display_table),c("Variable","p","Test")),expected)
    n <- rec$summary_statistics[[1]]$values
    expect_equal(as.numeric(n),c(50,rep(10,5)))
    row <- Filter(function(r) r$row_type=="numeric_mean_sd",rec$summary_statistics)[[1]]
    observed <- vapply(row$raw_statistics,`[[`,numeric(1),"mean")
    expect_equal(unname(observed),c(25.5,5.5,15.5,25.5,35.5,45.5)+if(mi) 1 else 0)
    expect_true(nzchar(row$p));expect_true(nzchar(row$test))
    if(!mi) {
      e <- .audit_recipe(rec$analysis_provenance$verification_r_code$table,data)
      expect_identical(names(e$reference_group_n),expected)
      expect_equal(unname(e$reference_group_n),c(50,rep(10,5)))
    }
  }
})

test_that("simulated Table 1 tests preserve RNG and are publicly reproducible", {
  set.seed(987)
  data <- data.frame(x=factor(sample(letters[1:5],35,TRUE)),g=factor(sample(LETTERS[1:5],35,TRUE)))
  group <- ls_register_dataset("audit_simulation",data)
  before <- .Random.seed
  tab <- ls_new_table1(group,"x",group="g",variable_types=c(x="categorical",g="categorical"),native=FALSE)
  expect_identical(.Random.seed,before)
  rec <- LinkEDA:::.rls_table1_record(tab);test <- rec$test_results$x
  expect_identical(test$simulation_seed,104729L)
  expect_identical(test$simulation_replicates,2000L)
  set.seed(1234);other <- ls_new_table1(group,"x",group="g",variable_types=c(x="categorical",g="categorical"),native=FALSE)
  expect_identical(LinkEDA:::.rls_table1_record(other)$test_results$x$p,test$p)
  reference <- withr::with_seed(104729L,stats::chisq.test(table(data$x,data$g),simulate.p.value=TRUE,B=2000),
    .rng_kind="Mersenne-Twister",.rng_normal_kind="Inversion",.rng_sample_kind="Rejection")
  expect_identical(test$p,reference$p.value)
  e <- .audit_recipe(rec$analysis_provenance$verification_r_code$table,data)
  expect_identical(e$reference_tests$x$p.value,test$p)
  # A saved Fisher test must drop unused levels in the public recipe as R did.
  tiny <- data.frame(x=factor(c("a","a","b","b"),levels=c("a","b","unused")),g=factor(c("c","d","d","d")))
  id <- ls_register_dataset("audit_fisher",tiny)
  r <- LinkEDA:::.rls_table1_record(ls_new_table1(id,"x",group="g",variable_types=c(x="categorical",g="categorical"),native=FALSE))
  e <- .audit_recipe(r$analysis_provenance$verification_r_code$table,tiny)
  expect_equal(e$reference_tests$x$p.value,r$test_results$x$p)
})

test_that("ordinary and MI frequencies use R counts for numeric and categorical variables", {
  data <- data.frame(x=c(1,1,2,NA,2,3),g=factor(c("a","b","a","b","a","b")))
  completed <- list(data,transform(data,x=c(1,2,2,3,2,3)))
  folder <- Sys.getenv("LINKEDA_AUDIT_FIVE_FIXTURES")
  for(mi in c(FALSE,TRUE)) {
    group <- if(mi) .audit_mi_dataset("audit_frequency_mi",completed) else ls_register_dataset("audit_frequency",data)
    ds <- LinkEDA:::.rls_dataset_record(group);sent <- NULL
    local_mocked_bindings(.rls_send=function(lines,...) {sent <<- lines;"OK"},.package="LinkEDA")
    task <- c("TABLE1_NEEDED",paste0(group,"-table"),group,"","TRUE","FALSE","FALSE","TRUE","ordinal",
      "selected","Chosen cases","1","x","TYPES","1","x","numeric","4","1","2","3","4",
      "ANALYSIS_KIND_V1","frequency","REQUEST_V1","1",ds$data_version)
    LinkEDA:::.rls_handle_table1_needed(task)
    expect_identical(sent[1],"TABLE1_OPEN_STRUCTURED");expect_true("frequency" %in% sent)
    r <- LinkEDA:::.rls_table1_record(paste0(group,"-table"))
    expect_identical(r$table_type,"frequency");expect_identical(r$data_scope$rows,1:4)
    expect_equal(r$mean_counts,if(mi) c(3.5,1.5,1.5,.5,.5) else c(3,2,1,1))
    if(mi) expect_equal(r$mean_percentages,c(1,(2/3+1/4)/2,(1/3+2/4)/2,1/8,1/8)) else
      expect_equal(r$mean_percentages,c(1,2/3,1/3,1/4))
    prepared <- if(mi) do.call(rbind,lapply(seq_along(completed),function(i) transform(completed[[i]][1:4,,drop=FALSE],.imp=i,.id=1:4))) else data[1:4,,drop=FALSE]
    e <- .audit_recipe(r$analysis_provenance$verification_r_code$table,prepared)
    expect_equal(e$reference_counts,r$mean_counts);expect_equal(e$reference_percentages,r$mean_percentages)
    if(nzchar(folder)) {
      dir.create(folder,recursive=TRUE,showWarnings=FALSE)
      prefix <- if(mi) "mi" else "ordinary"
      writeLines(c("REGISTER_DATASET",group,LinkEDA:::.rls_variable_payload(ds$data,ds$variable_metadata),
        LinkEDA:::.rls_dataframe_payload(ds$data,ds$variable_metadata,dataset_record=ds)),file.path(folder,paste0(prefix,"-dataset.payload")))
      writeLines(sent,file.path(folder,paste0(prefix,"-result.payload")))
    }
  }
  id <- ls_register_dataset("audit_frequency_missing",data.frame(x=rep(NA_real_,4)))
  r <- LinkEDA:::.rls_table1_record(LinkEDA:::.rls_frequency_record(id,"x","all-missing"))
  expect_equal(r$mean_counts,c(0,4));expect_equal(r$mean_percentages,c(NA,1))
})
