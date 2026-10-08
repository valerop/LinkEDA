.missing_fixture <- function() {
  withr::with_seed(42, {
    d <- data.frame(age=rnorm(150,40,5),score=rnorm(150),group=factor(rep(c("Control","Treatment"),75)))
    d$age[c(1:10,31:40)] <- NA; d$score[21:40] <- NA; d
  })
}

test_that("overview uses the same variables for patterns and Table 1 descriptors", {
  d <- .missing_fixture(); id <- ls_register_dataset("missing_overview_test",d)
  x <- ls_missing_data_overview(id,c("age","score"),c("age","group"),native=FALSE)
  expect_equal(sum(x$patterns$summary$N),150)
  expect_equal(sum(x$patterns$summary$`Number of missing cells`),40)
  expect_equal(x$patterns$summary$`Missing variables`[1],"none")
  expect_equal(sort(x$patterns$summary$N),c(10L,10L,10L,120L))
  x <- LinkEDA:::.rls_missing_pattern_action(x$summary$id,"descriptives","all",native=FALSE)
  tab <- LinkEDA:::.rls_table1_record(x$descriptive)
  expect_equal(tab$variables,c("age","score"))
  expect_equal(tab$analysis_backend,"ordinary")
  expect_equal(levels(tab$data[[tab$group_variable]]),levels(x$patterns$pattern))
  shuffled <- LinkEDA:::.rls_missing_patterns(d[150:1,],c("age","score"))
  expect_equal(as.character(shuffled$pattern),rev(as.character(x$patterns$pattern)))
  names_saved <- ls_save_missing_data(x)
  saved <- LinkEDA:::.rls_dataset_record(id)
  expect_equal(saved$data$age_missing,is.na(d$age))
  expect_equal(saved$data$score_missing,is.na(d$score))
  expect_equal(saved$data$missing_pattern,x$patterns$pattern)
  expect_equal(saved$data$n_missing,rowSums(is.na(d[c("age","score")])) )
  expect_equal(saved$data$pct_missing,50*saved$data$n_missing)
  expect_match(saved$variable_metadata$description[saved$variable_metadata$name=="missing_pattern"],"P1 = none",fixed=TRUE)
  expect_equal(LinkEDA:::.rls_metadata_type(saved$variable_metadata,"age_missing"),"factor")
  expect_error(ls_save_missing_data(x),"changed")
  env <- new.env();env$data <- d
  eval(parse(text=tail(saved$data_provenance$history,1)[[1]]$r_code),env)
  for(n in names_saved) expect_equal(as.character(env$data[[n]]),as.character(saved$data[[n]]))
})

test_that("Little uses naniar with explicit unsupported and complete-data states", {
  skip_if_not_installed("naniar")
  d <- .missing_fixture()
  expect_equal(LinkEDA:::.rls_little_mcar(d[c("age","score")])$result,naniar::mcar_test(d[c("age","score")]))
  expect_null(LinkEDA:::.rls_little_mcar(d)$result)
  expect_match(LinkEDA:::.rls_little_mcar(d)$note,"not converted")
  expect_match(LinkEDA:::.rls_little_mcar(d)$note,"does not establish MAR")
  id <- ls_register_dataset("missing_complete_test",data.frame(x=1:8,y=2:9))
  cap <- LinkEDA:::.rls_missing_data_capabilities(id)
  expect_true(cap$overview); expect_false(cap$models);expect_false(cap$imputation);expect_false(cap$diagnostics)
  expect_null(ls_missing_data_overview(id,native=FALSE)$little$result)
  expect_error(ls_missingness_model(id,"x","y",native=FALSE),"both missing and observed")
  expect_error(ls_missing_data_overview(id,.selected_rows=integer(),native=FALSE),"no observations")
})

test_that("missingness models reuse binary inference and export a public verification recipe", {
  d <- .missing_fixture(); id <- ls_register_dataset("missing_model_test",d)
  model <- ls_missingness_model(id,"score",c("age","group"),native=FALSE)
  record <- LinkEDA:::.rls_generalized_glm_record(model$model)
  reference <- glm(is.na(score)~age+group,data=d,family=binomial())
  expect_equal(unname(coef(record$fit)),unname(coef(reference)))
  expect_equal(model$probability,as.numeric(predict(reference,newdata=d,type="response")))
  expect_equal(nrow(record$term_tests),2L)
  recipe <- record$analysis_provenance$verification_r_code$model
  expect_match(recipe,"is.na(analysis_data[[\"score\"]])",fixed=TRUE)
  expect_false(grepl("LinkEDA:::|LinkEDA::",recipe))
  env <- new.env();env$verification_data_path <- record$analysis_provenance$prepared_data_path
  eval(parse(text=recipe),env)
  expect_equal(unname(coef(env$reference_model)),unname(coef(reference)))
  ls_binary_regression_fit(model$model)
  refitted <- LinkEDA:::.rls_generalized_glm_record(model$model)
  expect_match(refitted$analysis_provenance$verification_r_code$model,"is.na(analysis_data[[\"score\"]])",fixed=TRUE)
  ls_save_missing_data(model,c("indicator","probability"))
  saved <- LinkEDA:::.rls_dataset_record(id)
  expect_equal(saved$data$score_missing,is.na(d$score))
  expect_equal(saved$data$score_missing_probability,model$probability)
  expect_error(ls_missingness_model(id,"score",c("score","age"),native=FALSE),"own missingness")
})

test_that("external mids retains original missingness, chains and saved columns across versions", {
  skip_if_not_installed("mice")
  d <- .missing_fixture(); imp <- mice::mice(d,m=2,maxit=2,printFlag=FALSE,seed=5)
  id <- ls_import_mice(imp,name="missing_external_test",make_active=FALSE)
  expect_true(LinkEDA:::.rls_missing_data_capabilities(id)$diagnostics)
  expect_false(LinkEDA:::.rls_missing_data_capabilities(id)$models)
  for(source in c("original","active"))
    expect_error(ls_missingness_model(id,"score","age",source=source,native=FALSE),"original incomplete dataset")
  expect_error(ls_missingness_model(id,"score","age",native=FALSE),"disabled for imputed datasets")
  x <- ls_missing_data_overview(id,c("age","score"),c("age","group"),native=FALSE)
  expect_true(anyNA(x$input$data))
  expect_false(anyNA(LinkEDA:::.rls_missing_data_source(id,"active")$data))
  ls_save_missing_data(x,c("pattern","indicators"))
  r <- LinkEDA:::.rls_dataset_record(id)
  expect_equal(r$data$age_missing,is.na(d$age))
  expect_equal(r$completed_datasets[[2]]$age_missing,is.na(d$age))
  expect_true(LinkEDA:::.rls_mi_process_matches(r$mids_object,r$original_data,r$completed_datasets,r$import_name_map))
  expect_equal(r$mids_object$chainMean,imp$chainMean)
  expect_no_error(ls_imputation_diagnostics(id,native=FALSE))
  ls_set_imputed_dataset_version(r$imputation_id,2)
  expect_equal(LinkEDA:::.rls_dataset_record(id)$data$age_missing,is.na(d$age))
  path <- tempfile(fileext=".rds");ls_export_data(id,path)
  imported <- ls_import_mice(path,name="missing_external_roundtrip",make_active=FALSE)
  expect_true(LinkEDA:::.rls_missing_data_capabilities(imported)$diagnostics)
  expect_equal(LinkEDA:::.rls_dataset_record(imported)$original_data$age,d$age)
  plain <- ls_register_dataset("missing_plain_completed",mice::complete(imp,1))
  expect_false(LinkEDA:::.rls_missing_data_capabilities(plain)$diagnostics)
})

test_that("scope, source version and derived-column row identities are preserved", {
  d <- .missing_fixture(); id <- ls_register_dataset("missing_scope_test",d)
  x <- ls_missing_data_overview(id,c("age","score"),character(),.selected_rows=1:30,native=FALSE)
  expect_equal(sum(x$patterns$summary$N),30L)
  expect_equal(x$input$scope$rows,1:30)
  ls_save_missing_data(x,c("n_missing","indicators"))
  r <- LinkEDA:::.rls_dataset_record(id)
  expect_true(all(is.na(r$data$n_missing[31:150])))
  expect_equal(r$data$age_missing[1:30],is.na(d$age[1:30]))
  expect_error(LinkEDA:::.rls_handle_missing_data_workflow("missing_data_overview",id,"","original",c("age"),"","","1","all",integer()),"changed")
})


test_that("opening an overview registers prepared data without opening a data sheet", {
  id <- ls_register_dataset("missing_overview_no_sheet", .missing_fixture())
  state <- LinkEDA:::.rls_state
  before <- LinkEDA:::.rls_dataset_record(id)
  active <- state$active_dataset
  old_started <- state$process_started
  on.exit(state$process_started <- old_started)
  state$process_started <- TRUE
  commands <- list()
  testthat::local_mocked_bindings(
    .rls_send=function(lines, ...) { commands[[length(commands)+1L]] <<- lines; "OK" },
    ls_analysis_scope=function(group) {
      data <- LinkEDA:::.rls_dataset_record(group)$data
      list(dataset_id=group, kind="all", description="All observations",
           rows=seq_len(nrow(data)), n=nrow(data), total_n=nrow(data))
    }, .package="LinkEDA")
  result <- ls_missing_data_overview(id, c("age","score"), c("age","group"), native=TRUE)
  actions <- vapply(commands, `[[`, character(1L), 1L)
  expect_false(any(actions %in% c("REGISTER_DATASET", "DATA_OPEN_DATA_SHEET")))
  expect_true("REGISTER_DATASET_SILENT" %in% actions)
  expect_false("MISSING_DATA_REPORT" %in% actions)
  expect_equal(sum(actions=="TABLE1_OPEN_STRUCTURED"), 1L)
  registered <- commands[[which(actions=="REGISTER_DATASET_SILENT")]]
  expect_equal(registered[2], result$summary$group)
  expect_equal(commands[[which(actions=="SET_ACTIVE_DATASET")]][2], active)
  expect_identical(state$active_dataset, active)
  expect_identical(LinkEDA:::.rls_dataset_record(id), before)
  expect_equal(sum(result$patterns$summary$N), nrow(before$data))
  expect_true(file.exists(result$summary$analysis_provenance$prepared_data_path))
})


test_that("Little has an independent table and executable R verification", {
  skip_if_not_installed("naniar")
  id <- ls_register_dataset("little_separate", .missing_fixture())
  overview <- ls_missing_data_overview(id, c("age", "score"), native=FALSE)
  overview <- LinkEDA:::.rls_missing_pattern_action(overview$summary$id,"little",native=FALSE)
  record <- overview$little_table
  expect_equal(record$title, "Little's MCAR test")
  expect_equal(record$table_type, "missing_data_test")
  expect_true(record$display_options$show_p)
  reference <- naniar::mcar_test(overview$input$data[c("age", "score")])
  expect_equal(record$display_table$`χ²`, sprintf("%.3f", reference$statistic))
  expect_equal(record$display_table$df, as.character(reference$df))
  expect_equal(record$display_table$p, LinkEDA:::.rls_export_format_p(reference$p.value))
  expect_false(any(grepl("chi-square =|p =", overview$summary$footnotes)))
  env <- new.env()
  env$verification_data_path <- record$analysis_provenance$prepared_data_path
  eval(parse(text=record$analysis_provenance$verification_r_code$table), env)
  expect_equal(env$little_test, reference)
  payload <- LinkEDA:::.rls_table1_native_payload(record)
  expect_true("missing_data_test" %in% payload)
  path <- tempfile(fileext=".csv")
  ls_export_table1(structure(list(id=record$id), class="rlispstat_table1"), path, "csv")
  expect_match(paste(readLines(path), collapse="\n"), "Little's MCAR test", fixed=TRUE)
  unavailable_overview <- ls_missing_data_overview(id, c("age", "group"), native=FALSE)
  unavailable <- LinkEDA:::.rls_missing_pattern_action(unavailable_overview$summary$id,"little",native=FALSE)$little_table
  expect_equal(unavailable$display_table$Status, "Not calculated")
  expect_false("p" %in% names(unavailable$display_table))
  expect_true(any(grepl("at least two continuous", unavailable$footnotes)))
})


test_that("pattern matrix maps symbols and selections to original scoped rows", {
  id <- ls_register_dataset("missing_matrix_rows", .missing_fixture())
  x <- ls_missing_data_overview(id,c("age","score","group"),.selected_rows=c(1:15,21:35),native=FALSE)
  matrix <- x$summary$display_table
  expect_equal(matrix$Variable,c("age","score","group","Observations","% of observations"))
  expect_equal(names(matrix)[-1],as.character(x$patterns$summary$Pattern))
  for(label in names(matrix)[-1]) {
    positions <- which(x$patterns$pattern==label)
    expect_equal(matrix[[label]][1:3],ifelse(is.na(x$input$data[positions[1],c("age","score","group")]),"—","•") |> as.character())
    expect_equal(x$summary$missingness_layout$pattern_rows[[label]],c(1:15,21:35)[positions])
    expect_equal(as.integer(matrix[[label]][4]),length(positions))
  }
  expect_true("MISSINGNESS_LAYOUT_V1" %in% LinkEDA:::.rls_table1_native_payload(x$summary))
})

test_that("editing patterns updates open Little and all requested descriptives", {
  skip_if_not_installed("naniar")
  id <- ls_register_dataset("missing_matrix_edit", .missing_fixture())
  x <- ls_missing_data_overview(id,c("age","score","group"),native=FALSE)
  expect_null(x$little_table);expect_null(x$descriptive)
  key <- x$summary$id
  x <- LinkEDA:::.rls_missing_pattern_action(key,"little",native=FALSE)
  expect_equal(x$little$result,naniar::mcar_test(x$input$data[c("age","score")]))
  expect_true(any(grepl("Excluded non-continuous variables: group",x$little_table$footnotes,fixed=TRUE)))
  x <- LinkEDA:::.rls_missing_pattern_action(key,"descriptives","P2",native=FALSE)
  one <- LinkEDA:::.rls_table1_record(x$descriptive)
  expect_equal(nrow(one$data),sum(x$patterns$pattern=="P2"))
  expect_equal(one$variables,c("age","score","group"))
  x <- LinkEDA:::.rls_missing_pattern_action(key,"descriptives","all",native=FALSE)
  all_id <- x$descriptive$id; little_id <- x$little_table$id
  x <- LinkEDA:::.rls_missing_pattern_action(key,"remove","score",native=FALSE)
  expect_identical(x$summary$id,key); expect_identical(x$little_table$id,little_id)
  expect_null(x$little$result)
  expect_equal(x$little_table$display_table$Status,"Not calculated")
  expect_equal(LinkEDA:::.rls_table1_record(all_id)$variables,c("age","group"))
  expect_equal(x$summary$missingness_layout$available_variables,"score")
  x <- LinkEDA:::.rls_missing_pattern_action(key,"add","score",native=FALSE)
  expect_equal(x$little$result,naniar::mcar_test(x$input$data[c("age","score")]))
  expect_equal(LinkEDA:::.rls_table1_record(all_id)$variables,c("age","group","score"))
  LinkEDA:::.rls_missing_pattern_action(key,"save","pattern",native=FALSE)
  LinkEDA:::.rls_missing_pattern_action(key,"save","pct_missing",native=FALSE)
  saved <- LinkEDA:::.rls_dataset_record(id)
  expect_equal(as.character(saved$data$missing_pattern),as.character(x$patterns$pattern))
  expect_equal(saved$data$pct_missing,unname(100*rowMeans(is.na(x$input$data[c("age","group","score")])) ) )
})


test_that("native missingness actions reach the R overview controller", {
  id <- ls_register_dataset("missing_matrix_wire", .missing_fixture())
  x <- ls_missing_data_overview(id,c("age","score"),native=FALSE)
  state <- LinkEDA:::.rls_state; old_started <- state$process_started
  on.exit(state$process_started <- old_started)
  state$process_started <- TRUE
  sent <- list()
  testthat::local_mocked_bindings(.rls_send=function(lines,...) {
    sent[[length(sent)+1L]] <<- lines; "OK"
  }, .package="LinkEDA")
  task <- c("ANALYSIS_WORKFLOW_NEEDED","missing_data_action",id,x$summary$id,
    "add","","all","identity","","group","","0","0","")
  expect_true(LinkEDA:::.rls_handle_analysis_workflow_needed(task))
  expect_equal(LinkEDA:::.rls_table1_record(x$summary$id)$variables,c("age","score","group"))
  expect_true(any(vapply(sent,function(command) command[1]=="TABLE1_OPEN_STRUCTURED",logical(1))))
})
