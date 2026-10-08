test_that("clustering agrees with public R for every supported linkage and missing mode", {
  x <- data.frame(a=c(1, 2, 5, 9, 13), b=c(8, 6, 4, 1, 3), c=c(4, 7, 3, 8, 2))
  for (missing in c("pairwise", "listwise")) for (linkage in c("average", "complete", "single")) {
    input <- x
    input$b[2] <- NA_real_
    prepared <- if (missing == "listwise") input[complete.cases(input), ] else input
    expected <- stats::hclust(stats::dist(base::scale(prepared)), method=linkage)
    actual <- LinkEDA:::.rls_dendrogram_compute(input, names(input), "euclidean", linkage, missing)
    expect_equal(actual$merge, expected$merge)
    expect_equal(actual$height, expected$height)
    expect_equal(actual$order, expected$order)
    expect_equal(dim(actual$distance_matrix), c(0L, 0L))
    expect_equal(actual$case_rows, if(missing=="listwise") c(1L,3L,4L,5L) else 1:5)
  }
})

test_that("undefined distances and unstandardisable variables are never fabricated", {
  fit <- function(x) LinkEDA:::.rls_dendrogram_compute(x, names(x), "euclidean", "average", "pairwise")
  expect_error(fit(data.frame(a=c(1,2,NA,NA),b=c(NA,NA,3,4))), "undefined")
  expect_error(fit(data.frame(a=rep(3,4),b=1:4)), "no variation")
  expect_error(fit(data.frame(a=rep(NA_real_,4),b=1:4)), "insufficient observations")
  expect_error(fit(data.frame(a=c(1,Inf,3),b=1:3)), "infinite")
  expect_error(fit(data.frame(a=c(Inf,Inf))), "infinite")
  expect_error(fit(data.frame(a=1,b=2)), "two usable cases")
  expect_error(LinkEDA:::.rls_dendrogram_validate_distance("correlation"), "not supported")
})

test_that("every supported distance matches stats::dist and the verification recipe", {
  input <- data.frame(a = c(1, 2, 4, 8), b = c(5, 3, 6, 1))
  for (distance in c("euclidean", "manhattan", "maximum", "canberra")) {
    expected <- stats::hclust(stats::dist(scale(input), method = distance),
                              method = "average")
    actual <- LinkEDA:::.rls_dendrogram_compute(
      input, names(input), distance, "average", "listwise")
    expect_equal(actual$height, expected$height)
    model <- ls_new_quick_cluster(input, variables = names(input),
                                  distance = distance, native = FALSE)
    code <- ls_dendrogram_state(model)$analysis_provenance$verification_r_code$plot
    expect_match(code, paste0('method = "', distance, '"'), fixed = TRUE)
  }
})

test_that("verification recalculates the same R clustering with no private engine", {
  input <- data.frame(a=c(1,4,7,9),b=c(7,2,4,1))
  model <- ls_new_quick_cluster(input, variables=names(input), native=FALSE)
  state <- ls_dendrogram_state(model)
  code <- state$analysis_provenance$verification_r_code$plot
  expect_match(code, "stats::dist", fixed=TRUE)
  expect_match(code, "base::scale", fixed=TRUE)
  expect_false(grepl("LinkEDA:::|pairwise_distance|else 1",code))
  file <- tempfile(fileext=".rds"); saveRDS(input,file)
  pdf <- tempfile(fileext=".pdf"); grDevices::pdf(pdf)
  on.exit({grDevices::dev.off(); unlink(c(file,pdf))}, add=TRUE)
  env <- new.env(parent=baseenv()); env$verification_data_path <- file
  eval(parse(text=code),env)
  expect_equal(env$reference_cluster$merge, state$hclust$merge)
  expect_equal(env$reference_cluster$height, state$hclust$height)
})

test_that("native requests use their immutable rows and report terminal failures", {
  input <- data.frame(a=c(1,4,7,9),b=c(7,2,4,1))
  ls_register_dataset("cluster-wire",input)
  ds <- LinkEDA:::.rls_dataset_record("cluster-wire")
  sent <- NULL
  local_mocked_bindings(.rls_send=function(x,...) { sent <<- x; "OK" }, .package="LinkEDA")
  request <- c("DENDROGRAM_NEEDED","cluster-wire-result","cluster-wire","7",ds$data_version,
    "1","selected","euclidean","complete","pairwise","2","a","b","3","1","3","4")
  result <- LinkEDA:::.rls_handle_dendrogram_needed(as.character(request))
  reference <- stats::hclust(stats::dist(scale(input[c(1,3,4),])),method="complete")
  expect_equal(result$case_rows,c(1L,3L,4L))
  expect_equal(result$height,reference$height)
  expect_equal(sent[1:6],as.character(c("DENDRO_UPDATE","cluster-wire-result","7",ds$data_version,"ok",
    "Calculated in R: base::scale, stats::dist and stats::hclust.")))
  expect_true("ANALYSIS_PROVENANCE_V2" %in% sent)
  bad <- request; bad[5] <- "999999"
  LinkEDA:::.rls_handle_dendrogram_needed(as.character(bad))
  expect_equal(sent[5],"error"); expect_match(sent[6],"data changed")
  # Empty explicit scope stays empty; it must never fall back to all rows.
  empty <- c(request[1:13],"0")
  result <- LinkEDA:::.rls_handle_dendrogram_needed(as.character(empty))
  expect_length(result$case_rows,0)
})

test_that("distance matrix uses the fitted tree's scope, metric and request revision", {
  input <- data.frame(a=c(1,4,7,9), b=c(7,2,4,1))
  ls_register_dataset("cluster-matrix", input)
  ds <- LinkEDA:::.rls_dataset_record("cluster-matrix")
  messages <- list()
  local_mocked_bindings(
    .rls_send=function(x,...) {messages[[length(messages)+1L]] <<- x; "OK"},
    .rls_register_native_dataset_if_needed=function(...) invisible(TRUE),
    .package="LinkEDA")
  request <- c("DENDROGRAM_NEEDED", "matrix-tree", "cluster-matrix", "12",
               ds$data_version, "1", "selected", "manhattan", "average",
               "listwise", "2", "a", "b", "3", "1", "3", "4")
  fit <- LinkEDA:::.rls_handle_dendrogram_needed(as.character(request))
  expect_equal(fit$case_rows, c(1L,3L,4L))
  LinkEDA:::.rls_handle_dendrogram_distance_matrix_needed(
    as.character(c("DENDRO_DISTANCE_MATRIX_NEEDED", "matrix-tree",
                   "cluster-matrix", ds$data_version, "12")))
  expect_equal(tail(messages, 1L)[[1L]][[1L]], "DATA_OPEN_DATA_SHEET")
  output <- LinkEDA:::.rls_dataset_record(tail(messages, 1L)[[1L]][[2L]])$data
  expected <- as.matrix(stats::dist(scale(input[c(1,3,4), ]), method="manhattan"))
  expect_equal(as.matrix(output[-1L]), unname(expected), ignore_attr=TRUE)
  expect_equal(output$case, paste0("case_", c(1,3,4)))
  LinkEDA:::.rls_handle_dendrogram_distance_matrix_needed(
    as.character(c("DENDRO_DISTANCE_MATRIX_NEEDED", "matrix-tree",
                   "cluster-matrix", ds$data_version, "11")))
  expect_equal(tail(messages, 1L)[[1L]][[1L]], "WORKBENCH_MESSAGE")
})


test_that("MI requests cluster the requested completed imputation", {
  input <- data.frame(a=c(1,4,NA,9,6,2),b=c(7,2,4,NA,5,8))
  imp <- mice::mice(input,m=2,maxit=1,printFlag=FALSE,seed=73)
  # Make imputations distinguishable while retaining the original mids object.
  imp$imp$a[,1] <- 3; imp$imp$a[,2] <- 12
  LinkEDA:::.rls_register_mids_dataset(imp,"cluster-mi",make_active=FALSE)
  ds <- LinkEDA:::.rls_dataset_record("cluster-mi")
  sent <- NULL
  local_mocked_bindings(.rls_send=function(x,...) {sent <<- x; "OK"},.package="LinkEDA")
  request <- c("DENDROGRAM_NEEDED","cluster-mi-result","cluster-mi","1",ds$data_version,
    "2","all","euclidean","average","pairwise","2","a","b","0")
  result <- LinkEDA:::.rls_handle_dendrogram_needed(as.character(request))
  expected <- stats::hclust(stats::dist(scale(mice::complete(imp,2))),method="average")
  expect_equal(result$height,expected$height)
  expect_match(result$analysis_provenance$verification_r_code$plot,"$.imp == 2L",fixed=TRUE)
  expect_equal(result$case_rows,1:6)
  expect_equal(sent[5],"ok")
})
