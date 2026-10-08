test_that("coefficient labels preserve requested interaction order without mismatching estimates", {
  set.seed(142)
  data <- expand.grid(country = factor(c("Italy", "Cyprus", "Finland")),
                      group = factor(c("Control", "Treatment", "Other")),
                      sex = factor(c("Female", "Male")), replicate = 1:10)
  data$x <- rnorm(nrow(data))
  data$y <- rnorm(nrow(data)) + data$x + as.integer(data$country)
  requested <- c("country", "group", "sex", "x", "group:country", "sex:group", "sex:country", "sex:group:country", "group:x")
  fit <- lm(LinkEDA:::.rls_model_formula_object("y", requested), data)
  tab <- coef(summary(fit))
  coefficients <- data.frame(term = rownames(tab), estimate = tab[,1], std_error = tab[,2],
                             statistic = tab[,3], p_value = tab[,4])
  rows <- LinkEDA:::.rls_model_coefficient_display_rows(fit, coefficients, data, requested)
  for (term in requested[grepl(":", requested, fixed=TRUE)]) {
    parent <- rows[rows$source_term == term & rows$row_type == "term_parent", ]
    expect_equal(nrow(parent), 1)
    expect_identical(parent$display_label, gsub(":", " × ", term, fixed=TRUE))
    children <- rows[rows$source_term == term & rows$row_type == "coefficient", ]
    expect_equal(children$estimate, unname(coef(fit)[children$coefficient_name]))
  }
  child <- rows[rows$coefficient_name %in% "countryItaly:groupTreatment", ]
  expect_match(child$display_label, "Treatment × Italy", fixed=TRUE)
  expect_match(child$display_label, "vs Control × Cyprus", fixed=TRUE)
  child <- rows[rows$coefficient_name %in% "countryItaly:groupTreatment:sexMale", ]
  expect_match(child$display_label, "Male × Treatment × Italy", fixed=TRUE)
  child <- rows[rows$coefficient_name %in% "groupTreatment:x", ]
  expect_match(child$display_label, "group = Treatment × x", fixed=TRUE)
  recipe <- LinkEDA:::.rls_readable_r_formula_text(formula(fit), requested)
  expect_match(recipe, "group * country", fixed=TRUE)
  expect_match(recipe, "sex:group:country", fixed=TRUE)
})

test_that("a reverse-order MI interaction retains its default contrasts and native identity", {
  skip_if_not_installed("mice")
  skip_if_not_installed("emmeans")
  set.seed(735)
  d <- expand.grid(country = factor(c("Italy", "Cyprus", "Finland")),
                   group = factor(c("Control", "Treatment")), replicate = 1:20)
  completed <- lapply(1:3, function(i) transform(d, y = rnorm(nrow(d)) + as.integer(country)))
  id <- ls_register_dataset("interaction-order-mi", completed[[1]])
  on.exit(ls_unregister_dataset(id))
  dataset <- LinkEDA:::.rls_dataset_record(id)
  dataset$dataset_type <- "multiple_imputation"
  dataset$imputation_id <- "ordered-mi"
  dataset$original_data <- completed[[1]]
  dataset$completed_datasets <- completed
  dataset$imputation_count <- 3L
  dataset$original_row_ids <- seq_len(nrow(d))
  dataset$active_imputation_version <- 1L
  dataset$imputation_display_mode <- "version"
  LinkEDA:::.rls_set_dataset_record(dataset)
  model <- ls_new_glm(id)
  ls_glm_set_dependent(model, "y")
  ls_glm_add_predictor(model, "country")
  ls_glm_add_predictor(model, "group")
  ls_glm_add_interaction(model, "group", "country")
  ls_glm_fit(model)
  state <- LinkEDA:::.rls_glm_model_record(model)
  expect_identical(tail(state$predictors, 1), "group:country")
  ls_glm_add_interaction(model, "country", "group")
  expect_identical(LinkEDA:::.rls_glm_model_record(model)$predictors, state$predictors)
  expect_true(any(state$coefficient_rows$source_term == "group:country"))
  expect_true(any(grepl("group:country", LinkEDA:::.rls_glm_pooled_native_payload(state), fixed=TRUE)))
  interaction <- ls_glm_interaction(model, "group:country")
  expect_identical(interaction$focal, "group")
  expect_identical(interaction$moderator, "country")
  direct <- summary(emmeans::contrast(emmeans::emmeans(mice::as.mira(state$fits_by_imputation),
    specs = "group", by = "country"), method="pairwise"))
  expect_equal(interaction$comparisons$pooled_estimate, direct$estimate, tolerance=1e-10)
  expect_equal(interaction$comparisons$pooled_p_value, direct$p.value, tolerance=1e-10)
  code <- state$analysis_provenance$verification_r_code$model
  expect_match(code, "group:country", fixed=TRUE)
})
