test_that("batched MI comparisons retain variable labels and matching raw p values", {
  completed <- lapply(1:3, function(i) data.frame(
    perceived_risk_before=seq_len(24)+sin(seq_len(24)*i),
    perceived_risk_after=seq_len(24)+cos(seq_len(24)*i),
    genero=factor(rep(c("Female","Male"),12)),
    age_group_after=factor(rep(c("young","middle","older"),8))))
  id <- ls_register_dataset("mi-comparison-presentation",completed[[1]])
  dataset <- LinkEDA:::.rls_dataset_record(id)
  dataset$dataset_type <- "multiple_imputation";dataset$completed_datasets <- completed
  dataset$imputation_count <- 3L;dataset$original_data <- completed[[1]]
  LinkEDA:::.rls_set_dataset_record(dataset)
  responses <- c("perceived_risk_before","perceived_risk_after")
  handles <- NULL
  expect_warning(handles <- ls_new_independent_samples_t_test(id,response=c(responses,"age_group_after"),
    group="genero",group_order=c("Female","Male")), NA)
  records <- lapply(handles,LinkEDA:::.rls_compare_means_record)
  rows <- lapply(records,LinkEDA:::.rls_mi_missing_information)
  expect_match(paste(records[[3]]$warnings,collapse=" "),"mean comparisons require a numeric variable",fixed=TRUE)
  expect_false(any(grepl("Calling var",records[[3]]$warnings,fixed=TRUE)))
  long <- do.call(rbind,lapply(seq_along(completed),function(i)
    data.frame(.imp=i,.id=seq_len(nrow(completed[[i]])),completed[[i]])))
  path <- tempfile(fileext=".rds");saveRDS(long,path);on.exit(unlink(path),add=TRUE)
  recipe <- LinkEDA:::.rls_compare_means_verification_r_code(records)$code
  environment <- new.env(parent=globalenv());environment$verification_data_path <- path
  invisible(capture.output(eval(parse(text=recipe),envir=environment)))
  for(i in seq_along(responses)) {
    display <- LinkEDA:::.rls_mi_diagnostic_display_rows(records[[i]],rows[[i]])
    expect_identical(display$label,c(responses[i],"Female","Male","Difference (Female − Male)"))
    expect_identical(display$row_type,c("term_parent","summary_mean","summary_mean","group_comparison"))
    expect_true(all(is.na(rows[[i]]$p[1:2])))
    expect_equal(rows[[i]]$p[3],records[[i]]$test_results$p_value)
    expect_equal(rows[[i]]$Estimate[3],records[[i]]$multiple_imputation$pooled_result$Qbar)
    expect_equal(rows[[i]]$Estimate[1:2],unname(records[[i]]$descriptives$mean))
    reference <- environment$reference_group_diagnostics[[i]]
    for(column in c("Estimate","SE","df","FMI","RIV","Ubar","B","T","m"))
      expect_equal(rows[[i]][[column]][1:2],reference[[column]],tolerance=1e-10)
    expect_equal(rows[[i]]$`CI low`[1:2],reference$CI_low)
    expect_equal(rows[[i]]$`MCSE (estimate)`[1:2],reference$MCSE)
  }
  payload <- LinkEDA:::.rls_compare_means_batch_native_payload(records,run_id="mi-comparison-presentation")
  expect_true("MI_DIAGNOSTICS_V1" %in% payload)
  expect_true("MI_DIAGNOSTIC_ROWS_V1" %in% payload)
  if(identical(Sys.getenv("LINKEDA_PRESENTATION_FIXTURE"),"1"))
    writeLines(payload,"/tmp/linkeda-mi-comparison-presentation.payload")
})

test_that("paired MI diagnostics identify both means and the directed contrast", {
  completed <- lapply(1:3, function(i) data.frame(
    age_before=seq_len(24)+sin(seq_len(24)*i),
    age_after=seq_len(24)+cos(seq_len(24)*i),
    risk_before=sin(seq_len(24)) + seq_len(24)/3,
    risk_after=cos(seq_len(24)) + seq_len(24)/4+i/5))
  completed[[1]]$age_after[1:2] <- NA_real_
  id <- ls_register_dataset("mi-paired-presentation",completed[[1]])
  dataset <- LinkEDA:::.rls_dataset_record(id)
  dataset$dataset_type <- "multiple_imputation";dataset$completed_datasets <- completed
  dataset$imputation_count <- 3L;dataset$original_data <- completed[[1]]
  LinkEDA:::.rls_set_dataset_record(dataset)
  pairs <- list(c("age_before","age_after"),c("risk_before","risk_after"))
  for (alternative in c("two.sided","greater","less")) {
    handles <- ls_new_paired_samples_t_test(id,pairs=pairs,alternative=alternative)
    records <- lapply(handles,LinkEDA:::.rls_compare_means_record)
    for(i in seq_along(pairs)) {
      values <- LinkEDA:::.rls_mi_missing_information(records[[i]])
      display <- LinkEDA:::.rls_mi_diagnostic_display_rows(records[[i]],values)
      label <- paste(pairs[[i]],collapse=" − ")
      expect_identical(display$label,c(label,pairs[[i]],paste0("Difference (",label,")")))
      expect_equal(values$Estimate[1:2],unname(records[[i]]$descriptives$mean))
      expect_true(all(is.na(values$p[1:2])))
      expect_equal(values$p[3],records[[i]]$test_results$p_value)
      expect_equal(c(values$`CI low`[3],values$`CI high`[3]),unname(records[[i]]$test_results$conf_int))
    }
    if (alternative!="two.sided") next
    long <- do.call(rbind,lapply(seq_along(completed),function(i)
      data.frame(.imp=i,.id=seq_len(nrow(completed[[i]])),completed[[i]])))
    path <- tempfile(fileext=".rds");saveRDS(long,path);on.exit(unlink(path),add=TRUE)
    recipe <- LinkEDA:::.rls_compare_means_verification_r_code(records)$code
    environment <- new.env(parent=globalenv());environment$verification_data_path <- path
    invisible(capture.output(eval(parse(text=recipe),envir=environment)))
    for(i in seq_along(pairs)) {
      values <- LinkEDA:::.rls_mi_missing_information(records[[i]])
      reference <- environment$reference_group_diagnostics[[i]]
      expect_identical(reference$Group,pairs[[i]])
      for(column in c("Estimate","SE","df","FMI","RIV","Ubar","B","T","m"))
        expect_equal(values[[column]][1:2],reference[[column]],tolerance=1e-10)
    }
    payload <- LinkEDA:::.rls_compare_means_batch_native_payload(records,run_id="mi-paired-presentation")
    if(identical(Sys.getenv("LINKEDA_PRESENTATION_FIXTURE"),"1"))
      writeLines(payload,"/tmp/linkeda-mi-paired-presentation.payload")
  }
})

test_that("ANOVA MI diagnostics report the joint test instead of scalar placeholders", {
  completed <- lapply(1:3,function(i)data.frame(
    age_after=seq_len(36)/3+sin(seq_len(36)*i),
    risk_after=seq_len(36)/4+cos(seq_len(36)*i),
    country=factor(rep(c("Finland","Greece","Italy"),12))))
  id <- ls_register_dataset("mi-anova-diagnostics",completed[[1]])
  dataset <- LinkEDA:::.rls_dataset_record(id)
  dataset$dataset_type <- "multiple_imputation";dataset$completed_datasets <- completed
  dataset$imputation_count <- 3L;dataset$original_data <- completed[[1]]
  LinkEDA:::.rls_set_dataset_record(dataset)
  responses <- c("age_after","risk_after")
  handles <- ls_new_one_way_anova(id,responses,"country",method="classical")
  records <- lapply(handles,LinkEDA:::.rls_compare_means_record)
  for(i in seq_along(records)) {
    values <- LinkEDA:::.rls_mi_missing_information(records[[i]])
    expect_identical(names(values),c("Variable","Group","Test","F","df1","df2","p","RIV","m"))
    expect_identical(values$Variable,responses[i]);expect_identical(values$Group,"country")
    expect_equal(values$p,records[[i]]$test_results$p_value)
    expect_equal(values$F,records[[i]]$test_results$statistic)
    full <- lapply(completed,function(d)stats::lm(reformulate("country",responses[i]),data=d))
    null <- lapply(completed,function(d)stats::lm(as.formula(paste(responses[i],"~ 1")),data=d))
    reference <- mice::D1(mice::as.mira(full),mice::as.mira(null))$result[1,]
    expect_equal(unname(unlist(values[c("F","df1","df2","p","RIV")])),
      unname(reference[c("F.value","df1","df2","P(>F)","RIV")]),tolerance=1e-10)
  }
  payload <- LinkEDA:::.rls_compare_means_batch_native_payload(records,run_id="mi-anova-diagnostics")
  if(identical(Sys.getenv("LINKEDA_PRESENTATION_FIXTURE"),"1"))
    writeLines(payload,"/tmp/linkeda-anova-diagnostics.payload")
})
