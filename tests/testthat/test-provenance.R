test_that("immutable ordinary data versions preserve stable row identities", {
  name <- paste0("provenance_", sample.int(1e7, 1L))
  on.exit({
    if (exists(name, envir = LinkEDA:::.rls_state$datasets, inherits = FALSE))
      rm(list = name, envir = LinkEDA:::.rls_state$datasets)
    versions <- ls(LinkEDA:::.rls_state$data_versions, pattern = paste0("^", name, "@"))
    if (length(versions)) rm(list = versions, envir = LinkEDA:::.rls_state$data_versions)
  }, add = TRUE)

  LinkEDA::ls_register_dataset(name, data.frame(y = c(2, 4, 8), group = c("a", "b", "a")))
  version <- LinkEDA::ls_get_data_version(name, 1L)
  ids <- LinkEDA::ls_row_ids(version)
  expect_identical(ids, paste0(name, ":row:", 1:3))
  selected <- LinkEDA::ls_select_rows_by_id(version, ids[c(3, 1)])
  expect_equal(selected$y, c(8, 2))
  expect_identical(LinkEDA::ls_row_ids(selected), ids[c(3, 1)])
})

test_that("multiple-imputation versions preserve every completed data set", {
  name <- paste0("provenance_mi_", sample.int(1e7, 1L))
  on.exit({
    if (exists(name, envir = LinkEDA:::.rls_state$datasets, inherits = FALSE))
      rm(list = name, envir = LinkEDA:::.rls_state$datasets)
    versions <- ls(LinkEDA:::.rls_state$data_versions, pattern = paste0("^", name, "@"))
    if (length(versions)) rm(list = versions, envir = LinkEDA:::.rls_state$data_versions)
  }, add = TRUE)

  LinkEDA::ls_register_dataset(name, data.frame(y = c(1, 2, 3)))
  record <- LinkEDA:::.rls_dataset_record(name)
  record$dataset_type <- "multiple_imputation"
  record$imputation_count <- 2L
  record$original_data <- data.frame(y = c(1, NA, 3))
  record$completed_datasets <- list(
    data.frame(y = c(1, 2, 3)),
    data.frame(y = c(1, 2.5, 3))
  )
  LinkEDA:::.rls_set_dataset_record(record)
  LinkEDA:::.rls_store_data_version(record)

  version <- LinkEDA::ls_get_data_version(name, 1L)
  expect_s3_class(version, "linkeda_multiple_imputation_version")
  selected <- LinkEDA::ls_select_rows_by_id(
    version, LinkEDA::ls_row_ids(version)[c(2, 3)]
  )
  completed <- LinkEDA::ls_complete_data_version(selected)
  expect_length(completed, 2L)
  expect_equal(completed[[1L]]$y, c(2, 3))
  expect_equal(completed[[2L]]$y, c(2.5, 3))
  expect_identical(LinkEDA::ls_row_ids(completed[[1L]]),
                   LinkEDA::ls_row_ids(selected))
})

test_that("recorded analysis code is qualified, readable, and MI-aware", {
  ordinary <- list(
    dependent = "score", predictors = c("group", "time", "group:time"),
    term_types = list(group = "factor", time = "factor"),
    factor_reference_levels = list(group = "control"),
    centered_predictors = character()
  )
  code <- LinkEDA:::.rls_linear_executed_r_code(ordinary, FALSE)
  expect_match(code, "stats::lm", fixed = TRUE)
  expect_match(code, "na.action = stats::na.fail", fixed = TRUE)

  mi_code <- LinkEDA:::.rls_linear_executed_r_code(ordinary, TRUE)
  expect_match(mi_code, "LinkEDA::ls_complete_data_version", fixed = TRUE)
  expect_match(mi_code, "mice::pool", fixed = TRUE)
  expect_false(grepl("imputed_data", mi_code, fixed = TRUE))
  expect_match(mi_code, "pooled_global_test <- LinkEDA:::.rls_mi_pool_d1",
               fixed = TRUE)

  bounded <- list(
    response = "score", terms = "group", predictors = "group",
    term_types = list(group = "factor"), factor_reference_levels = list(),
    centered_predictors = character(), family = "beta", link = "logit",
    response_bounds = c(0, 25), binary_regression = FALSE,
    count_regression = FALSE, exposure = ""
  )
  bounded_code <- LinkEDA:::.rls_generalized_executed_r_code(
    bounded, multiple_imputation = FALSE
  )
  expect_match(bounded_code, "model <- betareg::betareg", fixed = TRUE)
  expect_match(bounded_code,
               'analysis_data[["score"]] <- (analysis_data[["score"]]',
               fixed = TRUE)
  expect_match(bounded_code, "data = analysis_data", fixed = TRUE)
  expect_silent(parse(text = bounded_code))

  mi_outputs <- LinkEDA:::.rls_generalized_output_r_code(TRUE)
  expect_match(mi_outputs$coefficients, "pooled_coefficients", fixed = TRUE)
  expect_false(grepl("summary\\(model\\)", mi_outputs$coefficients))
})

test_that("linear verification recipes present a semantic statistical table", {
  ordinary <- list(
    dependent = "score", predictors = "group",
    term_types = list(group = "factor"),
    factor_reference_levels = list(group = "control"),
    centered_predictors = character()
  )
  ordinary_recipe <- LinkEDA:::.rls_linear_verification_r_code(
    ordinary, multiple_imputation = FALSE
  )$code
  mi_recipe <- LinkEDA:::.rls_linear_verification_r_code(
    ordinary, multiple_imputation = TRUE
  )$code

  for (recipe in list(ordinary_recipe, mi_recipe)) {
    expect_match(recipe, "gtsummary::tbl_regression", fixed = TRUE)
    expect_match(recipe, "gtsummary::add_glance_table", fixed = TRUE)
    expect_match(recipe, "columns = c(std.error, statistic)", fixed = TRUE)
    expect_match(recipe, "Residual s", fixed = TRUE)
    expect_match(recipe, "df residual", fixed = TRUE)
    expect_match(recipe, "add_estimate_to_reference_rows = TRUE", fixed = TRUE)
    expect_match(recipe, " (reference)", fixed = TRUE)
    expect_false(grepl("print(summary(reference_model", recipe, fixed = TRUE))
    expect_false(grepl("LinkEDA:::", recipe, fixed = TRUE))
    expect_silent(parse(text = recipe))
  }
  expect_match(mi_recipe, "reference_mira <- mice::as.mira", fixed = TRUE)
  expect_match(mi_recipe, "mice::pool.r.squared", fixed = TRUE)
  expect_match(mi_recipe, "mice::D1", fixed = TRUE)
  expect_match(ordinary_recipe, "AIC", fixed = TRUE)
  expect_match(ordinary_recipe, "BIC", fixed = TRUE)
})

test_that("linear-comparison verification recipes refit and merge semantic tables", {
  record <- list(
    response = "score", uses_shared_response = TRUE,
    analysis_backend = "ordinary",
    models = list(
      list(label = "Model 1", response = "score", terms = "age",
           term_types = list(age = "numeric"),
           factor_reference_levels = list(), centered_predictors = character()),
      list(label = "Model 2", response = "score", terms = c("age", "group"),
           term_types = list(age = "numeric", group = "factor"),
           factor_reference_levels = list(group = "Control"), centered_predictors = character())
    )
  )
  ordinary <- LinkEDA:::.rls_regression_comparison_verification_r_code(record)
  record$analysis_backend <- "multiple_imputation"
  pooled <- LinkEDA:::.rls_regression_comparison_verification_r_code(record)

  for (recipe in list(ordinary$code, pooled$code)) {
    expect_match(recipe, "gtsummary::tbl_regression", fixed = TRUE)
    expect_match(recipe, "gtsummary::tbl_merge", fixed = TRUE)
    expect_match(recipe, "gtsummary::add_glance_table", fixed = TRUE)
    expect_match(recipe, "Delta R-squared vs previous", fixed = TRUE)
    expect_match(recipe, "order(data$row_type == \"glance_statistic\")", fixed = TRUE)
    expect_false(grepl("LinkEDA:::", recipe, fixed = TRUE))
    expect_silent(parse(text = recipe))
  }
  expect_match(pooled$code, "mice::D1", fixed = TRUE)
  expect_match(pooled$code, "mice::D3", fixed = TRUE)
  expect_setequal(ordinary$variables, c("score", "age", "group"))
})

test_that("generalized-model R plus Data recipes recalculate and present results", {
  binary <- list(
    response = "am", terms = "wt", predictors = "wt",
    term_types = list(wt = "numeric"), factor_reference_levels = list(),
    centered_predictors = character(), family = "binomial", link = "logit",
    response_bounds = NULL, binary_regression = TRUE,
    event = "1", reference = "0", count_regression = FALSE, exposure = ""
  )
  recipe <- LinkEDA:::.rls_generalized_verification_r_code(
    binary, multiple_imputation = FALSE
  )$code
  expect_match(recipe, "stats::glm", fixed = TRUE)
  expect_match(recipe, "gtsummary::tbl_regression", fixed = TRUE)
  expect_match(recipe, "exponentiate = TRUE", fixed = TRUE)
  expect_match(recipe, " (reference)", fixed = TRUE)
  expect_false(grepl("LinkEDA:::", recipe, fixed = TRUE))
  expect_silent(parse(text = recipe))
})

test_that("negative-binomial verification is concise and exposes fitted theta", {
  record <- list(
    response = "events",
    terms = c("group", "moment", "group:moment"),
    predictors = c("group", "moment", "group:moment"),
    term_types = list(group = "factor", moment = "factor"),
    factor_reference_levels = list(group = "Control", moment = "Before"),
    centered_predictors = character(), family = "poisson", link = "log",
    response_bounds = NULL, binary_regression = FALSE,
    count_regression = TRUE, count_distribution = "negative_binomial",
    exposure = ""
  )
  ordinary <- LinkEDA:::.rls_generalized_verification_r_code(
    record, multiple_imputation = FALSE
  )$code
  pooled <- LinkEDA:::.rls_generalized_verification_r_code(
    record, multiple_imputation = TRUE
  )$code

  expect_match(ordinary, "MASS::glm.nb(model_formula, data = model_data", fixed = TRUE)
  expect_match(ordinary, "theta <- fit$theta", fixed = TRUE)
  expect_match(pooled, "fits <- lapply(completed_sets", fixed = TRUE)
  expect_match(pooled, "MASS::glm.nb(model_formula, data = data", fixed = TRUE)
  expect_match(pooled, "mice::as.mira(fits)", fixed = TRUE)
  expect_match(pooled, "mice::pool(mira_model)", fixed = TRUE)
  expect_match(pooled, "vapply(fits, function(model) model$theta", fixed = TRUE)
  expect_match(pooled, "events ~ group * moment", fixed = TRUE)
  for (internal in c(
    "term_types", "factor_reference_levels", "centered_predictors",
    "numeric_interpretation", "factor_interpretation",
    "prepare_linkeda_model_data", "response_bounds", "analysis_data"
  )) {
    expect_false(grepl(internal, pooled, fixed = TRUE), info = internal)
  }
  expect_silent(parse(text = ordinary))
  expect_silent(parse(text = pooled))
})

test_that("bounded-count verification uses concise public grouped-binomial recipes", {
  record <- list(
    response = "successes", terms = c("group", "moment", "group:moment"),
    predictors = c("group", "moment", "group:moment"),
    term_types = list(group = "factor", moment = "factor"),
    factor_reference_levels = list(group = "Control", moment = "Before"),
    centered_predictors = character(), family = "binomial", link = "logit",
    response_bounds = NULL, binary_regression = FALSE,
    count_regression = TRUE, count_distribution = "binomial_trials",
    exposure = "", trials_variable = "", trials_constant = 12
  )
  ordinary <- LinkEDA:::.rls_generalized_verification_r_code(
    record, multiple_imputation = FALSE
  )$code
  pooled <- LinkEDA:::.rls_generalized_verification_r_code(
    record, multiple_imputation = TRUE
  )$code

  expect_match(ordinary, "cbind(successes, 12 - successes)", fixed = TRUE)
  expect_match(ordinary, "stats::glm", fixed = TRUE)
  expect_match(ordinary, "stats::binomial(link = \"logit\")", fixed = TRUE)
  expect_match(pooled, "fits <- lapply(completed_sets", fixed = TRUE)
  expect_match(pooled, "mice::as.mira(fits)", fixed = TRUE)
  expect_match(pooled, "mice::pool(mira_model)", fixed = TRUE)
  expect_match(pooled, "group * moment", fixed = TRUE)
  for (internal in c(
    "term_types", "factor_reference_levels", "centered_predictors",
    "numeric_interpretation", "factor_interpretation",
    "prepare_linkeda_model_data", "analysis_data"
  )) {
    expect_false(grepl(internal, pooled, fixed = TRUE), info = internal)
  }
  expect_silent(parse(text = ordinary))
  expect_silent(parse(text = pooled))

  beta_record <- modifyList(record, list(count_distribution = "beta_binomial"))
  beta_code <- LinkEDA:::.rls_generalized_verification_r_code(
    beta_record, multiple_imputation = TRUE
  )$code
  expect_match(beta_code, "glmmTMB::glmmTMB", fixed = TRUE)
  expect_match(beta_code, "glmmTMB::betabinomial", fixed = TRUE)
  expect_match(beta_code, "mice::pool.scalar(estimates, variances)", fixed = TRUE)
  expect_match(beta_code, "glmmTMB::fixef(model)$cond", fixed = TRUE)
  expect_match(beta_code, "stats::vcov(model)$cond", fixed = TRUE)
  expect_false(grepl("mice::pool(mira_model)", beta_code, fixed = TRUE))
  expect_match(beta_code, "precision_values <- vapply(fits, stats::sigma", fixed = TRUE)
  expect_silent(parse(text = beta_code))
})

test_that("ceiling-hurdle verification is exact, concise, and pools components separately", {
  record <- list(
    response = "score", terms = c("group", "moment", "group:moment"),
    predictors = c("group", "moment", "group:moment"),
    term_types = list(group = "factor", moment = "factor"),
    factor_reference_levels = list(group = "Control", moment = "Before"),
    centered_predictors = character(), family = "binomial", link = "logit",
    response_bounds = NULL, binary_regression = FALSE,
    count_regression = TRUE,
    count_distribution = "hurdle_beta_binomial_ceiling",
    exposure = "", trials_variable = "", trials_constant = 12
  )
  ordinary <- LinkEDA:::.rls_generalized_verification_r_code(
    record, multiple_imputation = FALSE
  )$code
  pooled <- LinkEDA:::.rls_generalized_verification_r_code(
    record, multiple_imputation = TRUE
  )$code
  for (code in list(ordinary, pooled)) {
    expect_match(code, "gamlss::gamlss", fixed = TRUE)
    expect_match(code, "gamlss.dist::ZABB", fixed = TRUE)
    expect_match(code, "cbind(12 - score, score)", fixed = TRUE)
    expect_match(code, "group * moment", fixed = TRUE)
    expect_false(grepl("prepare_linkeda_model_data", code, fixed = TRUE))
    expect_false(grepl("LinkEDA:::", code, fixed = TRUE))
    expect_silent(parse(text = code))
  }
  expect_match(pooled, "fits <- lapply(completed_sets, fit_hurdle_model)",
               fixed = TRUE)
  expect_match(pooled, "pool_component(\"perfect_score\")", fixed = TRUE)
  expect_match(pooled, "pool_component(\"below_ceiling\")", fixed = TRUE)
  expect_match(pooled, "mice::pool.scalar", fixed = TRUE)
  expect_match(pooled, "dispersion_values <- vapply(fits", fixed = TRUE)
})

test_that("perfect-score verification uses a public logistic model", {
  record <- list(
    response = "score", terms = "group", predictors = "group",
    term_types = list(group = "factor"), factor_reference_levels = list(),
    centered_predictors = character(), family = "binomial", link = "logit",
    response_bounds = NULL, binary_regression = FALSE,
    count_regression = TRUE, count_distribution = "perfect_score",
    exposure = "", trials_variable = "", trials_constant = 12
  )
  code <- LinkEDA:::.rls_generalized_verification_r_code(
    record, multiple_imputation = TRUE
  )$code
  expect_match(code, "I(score == 12)", fixed = TRUE)
  expect_match(code, "stats::glm", fixed = TRUE)
  expect_match(code, "mice::pool(mira_model)", fixed = TRUE)
  expect_false(grepl("gamlss", code, fixed = TRUE))
  expect_silent(parse(text = code))
})

test_that("new model verification recipes expose their public R formulations", {
  base <- list(
    response = "outcome", terms = c("group", "moment", "group:moment"),
    predictors = c("group", "moment", "group:moment"),
    term_types = list(group = "factor", moment = "factor"),
    factor_reference_levels = list(group = "Control", moment = "Before"),
    centered_predictors = character(), response_bounds = NULL,
    binary_regression = FALSE, count_regression = FALSE, exposure = "",
    event = NULL, reference = NULL
  )
  recipes <- lapply(list(
    gaussian_log = modifyList(base, list(family = "gaussian_log", link = "log")),
    lognormal = modifyList(base, list(family = "lognormal", link = "identity")),
    log_binomial = modifyList(base, list(
      family = "binomial", link = "log", binary_regression = TRUE,
      event = "yes", reference = "no"
    ))
  ), function(record) list(
    ordinary = LinkEDA:::.rls_generalized_verification_r_code(
      record, multiple_imputation = FALSE
    )$code,
    pooled = LinkEDA:::.rls_generalized_verification_r_code(
      record, multiple_imputation = TRUE
    )$code
  ))

  expect_match(recipes$gaussian_log$ordinary,
               'stats::gaussian(link = "log")', fixed = TRUE)
  expect_match(recipes$lognormal$ordinary,
               "stats::lm(model_formula", fixed = TRUE)
  expect_match(recipes$lognormal$ordinary,
               "log(outcome) ~ group * moment", fixed = TRUE)
  expect_match(recipes$lognormal$ordinary,
               "mean = exp(eta + sigma_squared / 2)", fixed = TRUE)
  expect_match(recipes$lognormal$ordinary,
               "Multiplicative ratio", fixed = TRUE)
  expect_false(grepl(
    "tbl_regression(fit, intercept = TRUE, conf.int = TRUE, exponentiate = TRUE",
    recipes$lognormal$ordinary, fixed = TRUE
  ))
  expect_match(recipes$log_binomial$ordinary,
               'stats::binomial(link = "log")', fixed = TRUE)

  for (recipe in recipes) {
    expect_match(recipe$pooled, "fits <- lapply(completed_sets", fixed = TRUE)
    expect_match(recipe$pooled, "mice::as.mira(fits)", fixed = TRUE)
    expect_match(recipe$pooled, "mice::pool(mira_model)", fixed = TRUE)
    expect_silent(parse(text = recipe$ordinary))
    expect_silent(parse(text = recipe$pooled))
    for (internal in c(
      "term_types", "factor_reference_levels", "centered_predictors",
      "numeric_interpretation", "factor_interpretation",
      "prepare_linkeda_model_data", "LinkEDA:::"
    )) {
      expect_false(grepl(internal, recipe$pooled, fixed = TRUE), info = internal)
    }
  }
})

test_that("analysis provenance payload carries exact UTF-8 code", {
  name <- paste0("provenance_payload_", sample.int(1e7, 1L))
  on.exit({
    if (exists(name, envir = LinkEDA:::.rls_state$datasets, inherits = FALSE))
      rm(list = name, envir = LinkEDA:::.rls_state$datasets)
    versions <- ls(LinkEDA:::.rls_state$data_versions, pattern = paste0("^", name, "@"))
    if (length(versions)) rm(list = versions, envir = LinkEDA:::.rls_state$data_versions)
  }, add = TRUE)
  LinkEDA::ls_register_dataset(name, data.frame(`puntuación` = 1:3, check.names = FALSE))
  record <- list(id = "analysis-1", group = name)
  code <- "model <- stats::lm(`puntuación` ~ 1, data = analysis_data)"
  record <- LinkEDA:::.rls_attach_analysis_provenance(
    record, code, "Modelo", list(model = "summary(model)"),
    verification_code = list(model = "reference_model <- stats::lm(y ~ 1, data = analysis_data)"),
    verification_variables = "puntuación",
    verification_warnings = "Prepared columns are used."
  )
  payload <- LinkEDA:::.rls_analysis_provenance_payload(record)
  expect_identical(payload[[1L]], "ANALYSIS_PROVENANCE_V2")
  encoded <- payload[[9L]]
  decoded <- rawToChar(as.raw(strtoi(substring(encoded,
    seq(1, nchar(encoded), 2), seq(2, nchar(encoded), 2)), 16L)))
  Encoding(decoded) <- "UTF-8"
  expect_identical(decoded, enc2utf8(code))
  expect_true("puntuación" %in% payload)
  expect_true("Prepared columns are used." %in% payload)
})

test_that("data-frame payload carries native data provenance", {
  name <- paste0("provenance_native_", sample.int(1e7, 1L))
  on.exit({
    if (exists(name, envir = LinkEDA:::.rls_state$datasets, inherits = FALSE))
      rm(list = name, envir = LinkEDA:::.rls_state$datasets)
    versions <- ls(LinkEDA:::.rls_state$data_versions, pattern = paste0("^", name, "@"))
    if (length(versions)) rm(list = versions, envir = LinkEDA:::.rls_state$data_versions)
  }, add = TRUE)

  source_frame <- data.frame(score = c(4, 8))
  LinkEDA::ls_register_dataset(name, source_frame)
  record <- LinkEDA:::.rls_dataset_record(name)
  record <- LinkEDA:::.rls_advance_data_version(
    record, "Recode score", "data$score <- data$score * 2",
    origin = "recorded", columns = "score"
  )
  LinkEDA:::.rls_set_dataset_record(record)
  payload <- LinkEDA:::.rls_dataframe_payload(record$data, dataset_record = record)
  marker <- match("DATAPROVENANCE_V1", payload)
  expect_false(is.na(marker))
  expect_identical(payload[[marker + 1L]], "2")
  expect_identical(payload[[marker + 2L]], "recorded")
  expect_match(payload[[marker + 3L]], "^[[:xdigit:]]+$")
  expect_identical(payload[[marker + 5L]], "2")
  expect_identical(payload[(marker + 6L):(marker + 7L)], record$stable_row_ids)
  expect_true("Recode score" %in% payload)
})

test_that("public registration records its real R expression", {
  name <- paste0("provenance_registration_", sample.int(1e7, 1L))
  on.exit({
    if (exists(name, envir = LinkEDA:::.rls_state$datasets, inherits = FALSE))
      rm(list = name, envir = LinkEDA:::.rls_state$datasets)
    versions <- ls(LinkEDA:::.rls_state$data_versions, pattern = paste0("^", name, "@"))
    if (length(versions)) rm(list = versions, envir = LinkEDA:::.rls_state$data_versions)
  }, add = TRUE)

  LinkEDA::ls_register_dataset(name, data.frame(score = c(1, 2)))
  provenance <- LinkEDA:::.rls_dataset_record(name)$data_provenance
  expect_identical(provenance$origin, "recorded")
  expect_match(provenance$origin_code, "data <- data.frame\\(score = c\\(1, 2\\)\\)")
  expect_false(grepl("get\\(", provenance$origin_code))
})

test_that("multiple-imputation provenance records the R workflow", {
  record <- list(
    source_dataset_id = "source data",
    source_data_version = 3L,
    variable_metadata = data.frame(
      variable_name = c("score", "group"),
      type = c("numeric", "factor"),
      stringsAsFactors = FALSE
    )
  )
  arguments <- list(
    m = 5L, maxit = 10L, method = c(score = "pmm", group = ""),
    predictorMatrix = matrix(c(0, 1, 1, 0), 2L, 2L,
                             dimnames = list(c("score", "group"),
                                             c("score", "group"))),
    printFlag = FALSE, seed = 42L
  )
  code <- LinkEDA:::.rls_mi_recorded_provenance_code(
    record, c("score", "group"), arguments
  )
  expect_match(code, "LinkEDA::ls_get_data_version(\"source data\", version = 3L)",
               fixed = TRUE)
  expect_match(code, "do.call(mice::mice", fixed = TRUE)
  expect_false(grepl("mice::complete", code, fixed = TRUE))
  expect_false(grepl("data <- completed_datasets", code, fixed = TRUE))
  expect_match(code, "predictorMatrix", fixed = TRUE)
  expect_match(code, "seed = 42L", fixed = TRUE)

  imported <- LinkEDA:::.rls_mi_import_provenance_code(
    file.path(tempdir(), "rlispstat-import-example/example.rds"),
    "mice mids RDS file",
    structure(list(
      m = 20L, method = c(y = "pmm"),
      predictorMatrix = matrix(0, 1L, 1L, dimnames = list("y", "y")),
      visitSequence = "y", iteration = 5L, call = quote(mice::mice(data))
    ), class = "mids")
  )
  expect_match(imported, "Reconstructed from mids metadata", fixed = TRUE)
  expect_match(imported, "may not be identical", fixed = TRUE)
  expect_match(imported, "m = 20L", fixed = TRUE)
  expect_false(grepl("readRDS\\(|load\\(", imported))
  expect_false(grepl("rlispstat-import|/private/var|tempdir", imported))
})

test_that("Table 1 executes and records one readable statistical R implementation", {
  skip_if_not_installed("mice")
  original <- data.frame(
    y = c(1.2, NA, 2.9, 4.4, 5.1, NA, 7.3, 8.8),
    g = factor(rep(c("a", "b"), each = 4L))
  )
  completed <- list(
    transform(original, y = c(1.2, 2.0, 2.9, 4.4, 5.1, 6.0, 7.3, 8.8)),
    transform(original, y = c(1.2, 2.5, 2.9, 4.4, 5.1, 6.5, 7.3, 8.8))
  )
  name <- paste0("provenance_table1_mi_", sample.int(1e7, 1L))
  LinkEDA::ls_register_dataset(name, completed[[1L]])
  dataset <- LinkEDA:::.rls_dataset_record(name)
  dataset$dataset_type <- "multiple_imputation"
  dataset$original_data <- original
  dataset$completed_datasets <- completed
  dataset$imputation_count <- 2L
  LinkEDA:::.rls_set_dataset_record(dataset)
  LinkEDA:::.rls_store_data_version(dataset)
  on.exit(LinkEDA::ls_unregister_dataset(name), add = TRUE)

  table <- LinkEDA::ls_new_table1(name, variables = "y", group = "g",
                                  native = FALSE)
  record <- LinkEDA:::.rls_table1_record(table)
  code <- record$analysis_provenance$executed_r_code
  expect_identical(code, record$statistical_r_code_executed)
  expect_false(grepl("LinkEDA::ls_new_table1", code, fixed = TRUE))
  expect_true(all(vapply(
    c("mean(", "stats::sd", "stats::quantile", "stats::lm",
      "mice::pool.scalar", "mice::D1", "stats::pt", "stats::qt"),
    grepl, logical(1L), x = code, fixed = TRUE
  )))
  expect_identical(record$analysis_provenance$output_r_code$table,
                   "table_values <- table1_result$display_table")
  expect_true(nzchar(record$analysis_provenance$output_r_code$table))
  expect_equal(record$multiple_imputation$m, 2L)
  expect_true(is.finite(record$test_results$y$p))

  replay <- new.env(parent = baseenv())
  replay$source_data <- LinkEDA::ls_get_data_version(name)
  replay$scoped_source_data <- replay$source_data
  for (expression in parse(text = code, keep.source = FALSE)) {
    eval(expression, envir = replay)
  }
  expect_equal(replay$table_values, record$display_table)
})

test_that("ordinary Table 1 emits and executes a portable public-package recipe", {
  skip_if_not_installed("gtsummary")
  name <- paste0("verification_table1_", sample.int(1e7, 1L))
  data <- data.frame(
    score = c(1.25, 2.5, NA_real_, 7.125, 9.75, 11.5),
    arm = factor(c("control", "control", "control", "treated", "treated", "treated"),
                 levels = c("control", "treated")),
    item = ordered(c("low", "medium", "high", "low", "medium", "high"),
                   levels = c("low", "medium", "high"))
  )
  LinkEDA::ls_register_dataset(name, data)
  on.exit(LinkEDA::ls_unregister_dataset(name), add = TRUE)
  table <- LinkEDA::ls_new_table1(
    name, variables = c("score", "item"), group = "arm", native = FALSE,
    numeric_stats = c("mean_sd", "median_iqr"), include_missing = TRUE
  )
  record <- LinkEDA:::.rls_table1_record(table)
  recipe <- record$analysis_provenance$verification_r_code$table
  expect_match(recipe, "gtsummary::tbl_summary", fixed = TRUE)
  expect_match(recipe, "stats::t.test", fixed = TRUE)
  expect_false(grepl("LinkEDA:::", recipe, fixed = TRUE))
  expect_false(grepl("table1_engine", recipe, fixed = TRUE))
  expect_false(grepl("display_table", recipe, fixed = TRUE))
  expect_silent(parse(text = recipe))

  directory <- file.path(
    tempdir(), paste0("LinkEDA comprobación ü ", sample.int(1e7, 1L))
  )
  dir.create(directory)
  data_path <- file.path(directory, "datos comprobación final.rds")
  saveRDS(data, data_path, version = 3L)
  script <- file.path(directory, "comprobar_resultado.R")
  runnable <- c(
    paste0(
      "verification_data_path <- ",
      encodeString(normalizePath(data_path, winslash = "/", mustWork = TRUE),
                   quote = "\"")
    ),
    recipe
  )
  writeLines(runnable, script, useBytes = TRUE)
  log <- file.path(directory, "recipe.log")
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)
  status <- system2(file.path(R.home("bin"), "Rscript"),
                    shQuote(script), stdout = log, stderr = log)
  expect_identical(as.integer(status), 0L,
                   info = paste(readLines(log, warn = FALSE), collapse = "\n"))

  replay <- new.env(parent = globalenv())
  replay$verification_data_path <- data_path
  for (expression in parse(text = recipe)) eval(expression, replay)
  reference_statistic <- unname(replay$reference_tests$score$statistic)
  expected_statistic <- unname(record$test_results$score$statistic)
  agrees <- function(actual, expected) isTRUE(all.equal(actual, expected, tolerance = 1e-10))
  expect_true(agrees(reference_statistic, expected_statistic))
  expect_false(agrees(reference_statistic, expected_statistic + 1))
})

test_that("multiple-imputation Table 1 recipe preserves all imputations and pools means", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("mice")
  original <- data.frame(
    score = c(1.2, NA, 2.9, 4.4, 5.1, NA, 7.3, 8.8),
    arm = factor(rep(c("control", "treated"), each = 4L))
  )
  completed <- list(
    transform(original, score = c(1.2, 2.0, 2.9, 4.4, 5.1, 6.0, 7.3, 8.8)),
    transform(original, score = c(1.2, 2.5, 2.9, 4.4, 5.1, 6.5, 7.3, 8.8))
  )
  name <- paste0("verification_table1_mi_", sample.int(1e7, 1L))
  LinkEDA::ls_register_dataset(name, completed[[1L]])
  dataset <- LinkEDA:::.rls_dataset_record(name)
  dataset$dataset_type <- "multiple_imputation"
  dataset$original_data <- original
  dataset$completed_datasets <- completed
  dataset$imputation_count <- length(completed)
  LinkEDA:::.rls_set_dataset_record(dataset)
  LinkEDA:::.rls_store_data_version(dataset)
  on.exit(LinkEDA::ls_unregister_dataset(name), add = TRUE)

  table <- LinkEDA::ls_new_table1(
    name, variables = "score", group = "arm", native = FALSE,
    numeric_stats = c("mean_sd", "median_iqr"), include_missing = TRUE
  )
  record <- LinkEDA:::.rls_table1_record(table)
  recipe <- record$analysis_provenance$verification_r_code$table
  expect_match(recipe, "mice::pool.scalar", fixed = TRUE)
  expect_match(recipe, "gtsummary::tbl_summary", fixed = TRUE)
  expect_false(grepl("LinkEDA:::", recipe, fixed = TRUE))
  expect_false(grepl("mice::mice", recipe, fixed = TRUE))
  expect_silent(parse(text = recipe))

  long <- do.call(rbind, c(
    list(transform(original, .imp = 0L, .id = seq_len(nrow(original)))),
    lapply(seq_along(completed), function(index) {
      transform(completed[[index]], .imp = index, .id = seq_len(nrow(original)))
    })
  ))
  directory <- file.path(
    tempdir(), paste0("LinkEDA comprobación MI ü ", sample.int(1e7, 1L))
  )
  dir.create(directory)
  data_path <- file.path(directory, "datos preparados finales.rds")
  saveRDS(long, data_path, version = 3L)
  script <- file.path(directory, "comprobar_resultado.R")
  runnable <- c(
    paste0(
      "verification_data_path <- ",
      encodeString(normalizePath(data_path, winslash = "/", mustWork = TRUE),
                   quote = "\"")
    ),
    recipe
  )
  writeLines(runnable, script, useBytes = TRUE)
  log <- file.path(directory, "recipe.log")
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)
  status <- system2(file.path(R.home("bin"), "Rscript"),
                    shQuote(script), stdout = log, stderr = log)
  expect_identical(as.integer(status), 0L,
                   info = paste(readLines(log, warn = FALSE), collapse = "\n"))

  replay <- new.env(parent = globalenv())
  replay$verification_data_path <- data_path
  for (expression in parse(text = recipe)) eval(expression, replay)
  expect_length(replay$completed_sets, length(completed))
  expect_identical(as.character(replay$completed_sets[[1L]]$arm),
                   as.character(completed[[1L]]$arm))
  direct <- mice::pool.scalar(
    vapply(completed, function(data) mean(data$score), numeric(1L)),
    vapply(completed, function(data) stats::var(data$score) / nrow(data), numeric(1L)),
    n = nrow(original), k = 1L
  )$qbar
  expect_equal(replay$reference_pooled_means$score$Overall$qbar, direct,
               tolerance = 1e-12)
})

test_that("user-visible LinkEDA data-version calls are exported stable APIs", {
  visible_calls <- c(
    "ls_get_data_version", "ls_complete_data_version",
    "ls_row_ids", "ls_select_rows_by_id"
  )
  expect_true(all(vapply(visible_calls, function(name) {
    name %in% getNamespaceExports("LinkEDA") &&
      is.function(getExportedValue("LinkEDA", name))
  }, logical(1L))))
})

test_that("comparison provenance materializes the comparisons it displays", {
  ordinary <- list(
    analysis_backend = "ordinary", mi_pooling_required = FALSE,
    response = "score", term_types = list(group = "factor", age = "numeric"),
    models = list(
      list(label = "Model 1", terms = "group"),
      list(label = "Model 2", terms = c("group", "age"))
    )
  )
  ordinary_code <- LinkEDA:::.rls_regression_comparison_executed_r_code(ordinary)
  expect_match(ordinary_code, "comparison_1_2 <- LinkEDA:::.rls_regcmp_ordinary_nested_test", fixed = TRUE)
  expect_match(ordinary_code, "comparisons <- list(comparison_1_2)", fixed = TRUE)

  ordinary$analysis_backend <- "multiple_imputation"
  ordinary$mi_pooling_required <- TRUE
  mi_code <- LinkEDA:::.rls_regression_comparison_executed_r_code(ordinary)
  expect_match(mi_code, "LinkEDA:::.rls_regcmp_mi_nested_test", fixed = TRUE)
  expect_match(mi_code, "fits_by_imputation = Model.1_fits", fixed = TRUE)
  expect_match(mi_code, 'terms = c("group", "age"), fits_by_imputation = Model.2_fits', fixed = TRUE)
  expect_match(mi_code, "comparisons <- list(comparison_1_2)", fixed = TRUE)
  expect_false(grepl("all.vars(model_formula)", mi_code, fixed = TRUE))
  expect_silent(parse(text = mi_code))
})
