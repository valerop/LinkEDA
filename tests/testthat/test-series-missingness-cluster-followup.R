test_that("time series keep raw categories separate despite label collisions", {
  groups <- c("A|B", "A B", "A B #1", "(missing)", NA, "(missing values)")
  d <- data.frame(t=rep(1:2,each=6),y=1:12,g=rep(groups,2))
  p <- LinkEDA:::.rls_prepare_time_series_data(d,"t","y","g")
  expect_equal(p$labels,c("A B","A B #2","A B #1","(missing)","(missing values)","(missing values) #1"))
  expect_length(unique(p$labels),6L)
  expect_equal(p$point_series,c(0L,1L,2L,3L,5L,4L,0L,1L,2L,3L,5L,4L))
  expect_equal(p$rows,1:12)
  d$g <- factor(d$g,levels=rev(groups[!is.na(groups)]))
  p <- LinkEDA:::.rls_prepare_time_series_data(d,"t","y","g")
  expect_length(unique(p$labels),6L)
  expect_equal(p$point_series[1:6],p$point_series[7:12])
  expect_equal(length(unique(p$point_series)),6L)
})

test_that("missingness edits reuse one helper and preserve old verification data", {
  d <- data.frame(a=c(1,2,NA,4),b=c(NA,2,3,4),c=c(1,NA,3,4))
  id <- ls_register_dataset("followup-patterns",d)
  source <- LinkEDA:::.rls_dataset_record(id)
  count <- function() length(ls(envir=LinkEDA:::.rls_state$datasets))
  before <- count()
  first <- ls_missing_data_overview(id,native=FALSE)
  helper <- first$summary$group
  path <- first$summary$analysis_provenance$prepared_data_path
  old_data <- readRDS(path)
  old_version <- LinkEDA:::.rls_dataset_record(helper)$data_version
  version_key <- LinkEDA:::.rls_data_version_key(helper,old_version)
  old_snapshot <- get(version_key,envir=LinkEDA:::.rls_state$data_versions)
  expect_equal(count(),before+1L)
  expect_equal(attr(old_data,"linkeda_row_ids"),source$stable_row_ids)
  change <- function(action,value="") LinkEDA:::.rls_missing_pattern_action(first$summary$id,action,value,native=FALSE)
  for (action in c("remove","add","remove","add")) {
    current <- change(action,"c")
    expect_identical(current$summary$group,helper)
    expect_equal(count(),before+1L)
    expect_equal(readRDS(path),old_data)
    expect_identical(get(version_key,envir=LinkEDA:::.rls_state$data_versions),old_snapshot)
    snapshot <- LinkEDA:::.rls_dataset_record(helper)
    e <- new.env(parent=baseenv());e$data <- d
    capture.output(eval(parse(text=tail(snapshot$data_provenance$history,1)[[1]]$r_code),e))
    expect_equal(e$data$missing_pattern,snapshot$data$missing_pattern)
  }
  snapshot <- LinkEDA:::.rls_dataset_record(helper)
  expect_gt(snapshot$data_version,old_version)
  current_path <- current$summary$analysis_provenance$prepared_data_path
  current_version <- snapshot$data_version
  current <- change("descriptives","all")
  expect_equal(LinkEDA:::.rls_dataset_record(helper)$data_version,current_version)
  expect_identical(current$summary$analysis_provenance$prepared_data_path,current_path)
  expect_equal(count(),before+1L)
  expect_equal(LinkEDA:::.rls_table1_record(current$descriptive)$variables,names(d))
  expect_equal(LinkEDA:::.rls_dataset_record(id)$data_version,source$data_version)
  expect_equal(LinkEDA:::.rls_dataset_record(id)$data,source$data)
  # An independent report still owns a distinct snapshot.
  second <- ls_missing_data_overview(id,"a",native=FALSE)
  expect_false(identical(second$summary$group,helper))
  expect_equal(count(),before+2L)
})

test_that("pattern verification works with single variables and single patterns", {
  for (d in list(data.frame(a=c(1,NA,3)),data.frame(a=1:3),
                 data.frame(a=rep(NA_real_,3)),data.frame(a=1:3,b=4:6))) {
    id <- ls_register_dataset(paste0("followup-export-",ncol(d),sum(is.na(d))),d)
    x <- ls_missing_data_overview(id,native=FALSE)
    e <- new.env(parent=baseenv())
    e$verification_data_path <- x$summary$analysis_provenance$prepared_data_path
    capture.output(eval(parse(text=x$summary$analysis_provenance$verification_r_code$table),e))
    expect_equal(dim(e$missingness_matrix),c(ncol(d),nrow(x$patterns$summary)))
    expect_equal(rownames(e$missingness_matrix),names(d))
    expect_equal(colnames(e$missingness_matrix),as.character(x$patterns$summary$Pattern))
    expect_equal(unname(e$missingness_matrix),unname(as.matrix(x$summary$display_table[seq_len(ncol(d)),-1,drop=FALSE])))
    expect_equal(e$counts$Freq,x$patterns$summary$N)
  }
})

test_that("Quick Cluster retains the R tree without a quadratic dense copy", {
  d <- withr::with_seed(5,data.frame(a=rnorm(1200),b=rnorm(1200)))
  reference <- stats::hclust(stats::dist(scale(d)),method="average")
  f <- LinkEDA:::.rls_dendrogram_compute(d,names(d),"euclidean","average","listwise")
  expect_equal(f$merge,reference$merge)
  expect_equal(f$height,reference$height)
  expect_equal(f$order,reference$order)
  expect_equal(dim(f$distance_matrix),c(0L,0L))
  expect_lt(as.numeric(object.size(f)),1200^2) # substantially below even one byte per pair
})

test_that("existing dendrograms can add newly created numeric columns", {
  id <- ls_register_dataset("followup-cluster",mtcars[c("mpg","hp","wt")])
  model <- ls_new_quick_cluster(id,variables=c("mpg","hp"),native=FALSE)
  d <- LinkEDA:::.rls_dataset_record(id)
  d$data$new_variable <- seq_len(nrow(d$data)); d$data_frame <- d$data
  d$metadata <- d$variable_metadata <- LinkEDA:::.rls_refresh_metadata_row(d$variable_metadata,d$data,"new_variable","numeric")
  d <- LinkEDA:::.rls_advance_data_version(d,"Add numeric column")
  LinkEDA:::.rls_set_dataset_record(d)
  expect_no_error(ls_dendrogram_add_variable(model,"new_variable"))
  state <- ls_dendrogram_state(model)
  reference <- stats::hclust(stats::dist(scale(d$data[c("mpg","hp","new_variable")])),method="average")
  expect_equal(state$height,reference$height)
  expect_equal(state$merge,reference$merge)
  expect_equal(state$case_rows,1:32)
  expect_match(state$analysis_provenance$verification_r_code$plot,"new_variable",fixed=TRUE)
  d$data$text <- letters[seq_len(nrow(d$data)) %% 26L + 1L]; d$data_frame <- d$data
  LinkEDA:::.rls_set_dataset_record(d)
  expect_error(ls_dendrogram_add_variable(model,"text"),"numeric")
})

test_that("adding a cluster variable uses the chosen completed imputation", {
  x <- mtcars[c("mpg","hp","wt")]
  id <- ls_register_dataset("followup-cluster-mi",x)
  d <- LinkEDA:::.rls_dataset_record(id)
  second <- x; second$mpg <- rev(second$mpg)
  d$dataset_type <- "multiple_imputation";d$original_data <- x
  d$completed_datasets <- list(x,second);d$imputation_count <- 2L
  d$active_imputation_version <- 2L
  LinkEDA:::.rls_set_dataset_record(d)
  model <- suppressWarnings(ls_new_quick_cluster(id,variables=c("mpg","hp"),native=FALSE))
  d <- LinkEDA:::.rls_dataset_record(id)
  d$completed_datasets[[1]]$extra <- seq_len(nrow(x))
  d$completed_datasets[[2]]$extra <- rev(seq_len(nrow(x)))^2
  d$original_data$extra <- seq_len(nrow(x))
  # Worksheet preview is deliberately still the original, stale set of columns.
  d <- LinkEDA:::.rls_advance_data_version(d,"New column in both imputations")
  LinkEDA:::.rls_set_dataset_record(d)
  expect_no_error(ls_dendrogram_add_variable(model,"extra"))
  s <- ls_dendrogram_state(model)
  reference <- stats::hclust(stats::dist(scale(d$completed_datasets[[2]][c("mpg","hp","extra")])),method="average")
  expect_equal(s$height,reference$height)
  expect_equal(s$merge,reference$merge)
  expect_identical(s$active_imputation_version,2L)
  expect_identical(s$data_scope$dataset_version,d$data_version)
})

test_that("dataset transport escapes delimiters without changing raw categories", {
  x <- c("A|B","A B","A\tB","A\nB","100%","literal%7C","Espa\u00f1a")
  encoded <- LinkEDA:::.rls_encode_data_value(x)
  expect_equal(unname(vapply(encoded,utils::URLdecode,character(1L))),x)
  expect_false(any(grepl("[\r\n\t|]",encoded)))
  wire <- LinkEDA:::.rls_dataframe_payload(data.frame(g=x))
  expect_equal(wire[4],"DATACELLS_PERCENT_V1")
  expect_equal(wire[7:13],encoded)
  path <- tempfile();on.exit(unlink(path))
  writeLines(c("DATASET","transport",as.character(length(x)),"1","DATACELLS_PERCENT_V1",
               "g","character",encoded),path)
  expect_equal(LinkEDA:::.rls_read_native_data_payload(path)$columns$g,x)
  # Older payloads do not decode literal percent sequences.
  writeLines(c("DATASET","legacy","1","1","g","character","literal%7C"),path)
  expect_equal(LinkEDA:::.rls_read_native_data_payload(path)$columns$g,"literal%7C")
})

test_that("real R payload preserves grouped cases through the native transport", {
  driver <- Sys.getenv("LINKEDA_SERIES_TRANSPORT_TEST")
  skip_if(!nzchar(driver) || !file.exists(driver),"Shared native transport executable not provided")
  groups <- c("A|B","A B","A B #1","(missing)",NA,"(missing values)")
  d <- data.frame(t=rep(1:2,each=6),y=1:12,g=factor(rep(groups,2)))
  wire <- tempfile();back <- tempfile();on.exit(unlink(c(wire,back)))
  for (mi in c(FALSE,TRUE)) {
    record <- NULL
    if (mi) {
      one <- two <- d
      one$g[is.na(one$g)] <- "A|B"
      two$g[is.na(two$g)] <- "A B"
      record <- list(dataset_type="multiple_imputation",data=d,original_data=d,
        completed_datasets=list(one,two),missing_cell_mask=lapply(d,is.na),
        active_imputation_version=1L,imputation_display_mode="original",group="wire-series",
        dataset_id="wire-series",data_version=1L,original_row_ids=1:12)
    }
    writeLines(LinkEDA:::.rls_dataframe_payload(d,dataset_record=record),wire)
    expect_identical(system2(driver,c(wire,back)),0L)
    restored <- LinkEDA:::.rls_read_native_data_payload(back)
    expect_equal(restored$columns$g,replace(as.character(d$g),is.na(d$g),"NA"))
    expect_equal(restored$factor_levels$g,levels(d$g))
    if(mi) {
      expect_equal(restored$imputation$sparse[[1L]]$original,c("NA","NA"))
      expect_equal(restored$imputation$sparse[[1L]]$versions,list(c("A|B","A|B"),c("A B","A B")))
    }
  }
})
