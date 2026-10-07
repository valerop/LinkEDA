test_that("ordinary scale analysis delegates reliability to psych", {
  skip_if_not_installed("psych")
  set.seed(8102)
  n <- 180L
  latent <- rnorm(n)
  data <- data.frame(
    q1 = latent + rnorm(n, sd = 0.45),
    q2 = latent + rnorm(n, sd = 0.55),
    q3 = latent + rnorm(n, sd = 0.50),
    q4 = latent + rnorm(n, sd = 0.60),
    weak = rnorm(n)
  )
  data$q3[c(7, 31, 88)] <- NA_real_
  specs <- LinkEDA:::.rls_scale_item_specifications(
    data, names(data), setNames(rep("numeric", ncol(data)), names(data))
  )
  options <- LinkEDA:::.rls_scale_default_options("pearson")
  fit <- LinkEDA:::.rls_scale_fit_one(data, specs, options)
  reference <- suppressWarnings(psych::alpha(
    data, check.keys = FALSE, warnings = FALSE, delete = TRUE, use = "pairwise"
  ))

  expect_equal(fit$alpha$summary$alpha, unname(reference$total[["raw_alpha"]]), tolerance = 1e-12)
  expect_equal(fit$alpha$summary$standardized_alpha,
               unname(reference$total[["std.alpha"]]), tolerance = 1e-12)
  expect_equal(fit$alpha$items$item_rest_r,
               as.numeric(reference$item.stats[names(data), "r.drop"]), tolerance = 1e-12)
  expect_equal(fit$alpha$items$alpha_if_deleted,
               as.numeric(reference$alpha.drop[names(data), "raw_alpha"]), tolerance = 1e-12)
  expect_gt(fit$alpha$items$alpha_if_deleted[fit$alpha$items$item == "weak"],
            fit$alpha$summary$alpha)
  expect_identical(fit$alpha$function_name, "psych::alpha")
  expect_identical(fit$correlation$function_name, "psych::corr.test")
  correlation_reference <- suppressWarnings(psych::corr.test(
    data, use = "pairwise", method = "pearson", adjust = "none", ci = FALSE
  ))
  expect_equal(fit$correlation$p_values, unclass(correlation_reference$p),
               tolerance = 1e-12)
  expect_equal(diag(fit$correlation$sample_sizes), colSums(!is.na(data)))
  expect_equal(fit$correlation$sample_sizes["q1", "q3"],
               sum(stats::complete.cases(data[, c("q1", "q3")])))
})

test_that("native Scale Analysis requests honor their frozen analysis scope", {
  skip_if_not_installed("psych")
  group <- ls_register_dataset("scale_scope_protocol", data.frame(
    q1 = c(1, 2, 3, 4, 5, 6),
    q2 = c(2, 3, 4, 5, 6, 7)
  ))
  testthat::local_mocked_bindings(
    .rls_scale_sync_native = function(record, open = FALSE) TRUE,
    .package = "LinkEDA"
  )
  parts <- c(
    "SCALE_ANALYSIS_NEEDED", "scale_scope_result", group,
    "1", "scope-fingerprint", "pearson", "mean", "0",
    "FALSE", "FALSE", "factor", "pairwise", "TRUE", "0",
    "minres", "oblimin", "1",
    "selected", "3", "2", "4", "6",
    "2",
    "q1", "numeric", "FALSE", "FALSE", "0", "0",
    "q2", "numeric", "FALSE", "FALSE", "0", "0"
  )

  expect_true(LinkEDA:::.rls_handle_scale_analysis_needed(parts))
  record <- LinkEDA:::.rls_scale_record("scale_scope_result")
  expect_identical(record$scope, "selected")
  expect_identical(record$selected_rows, c(2L, 4L, 6L))
  expect_identical(record$result$summary$N, 3L)
  expect_identical(record$result$summary$n_used, 3L)
})

test_that("reverse scoring is explicit and non-destructive", {
  data <- data.frame(
    numeric_item = c(1, 2, 4, NA),
    ordinal_item = ordered(c("low", "medium", "high", NA),
                           levels = c("low", "medium", "high"))
  )
  original <- data
  expect_error(
    LinkEDA:::.rls_scale_item_specifications(
      data, "numeric_item", c(numeric_item = "numeric"), reverse_items = "numeric_item"
    ),
    "theoretical range"
  )
  specs <- LinkEDA:::.rls_scale_item_specifications(
    data, names(data), c(numeric_item = "numeric", ordinal_item = "ordinal"),
    reverse_items = names(data), scoring_ranges = list(numeric_item = c(1, 5))
  )
  prepared <- LinkEDA:::.rls_scale_prepare_items(data, specs)
  expect_equal(prepared$numeric_item, c(5, 4, 2, NA))
  expect_equal(as.numeric(prepared$ordinal_item), c(3, 2, 1, NA))
  expect_identical(data, original)
})

test_that("unordered variables with more than two categories require explicit ordering", {
  data <- data.frame(item = factor(c("a", "b", "c")))
  expect_error(LinkEDA:::.rls_scale_item_specifications(data, "item"), "is Categorical")
  expect_silent(LinkEDA:::.rls_scale_item_specifications(
    data, "item", c(item = "ordinal")
  ))
})

test_that("dichotomous factor items can retain an explicit numeric 0/1 interpretation", {
  data <- data.frame(
    q1 = factor(c("No", "Yes", "No", NA), levels = c("No", "Yes")),
    q2 = factor(c("Yes", "Yes", "No", "No"), levels = c("No", "Yes"))
  )
  specs <- LinkEDA:::.rls_scale_item_specifications(
    data, names(data), c(q1 = "numeric", q2 = "numeric")
  )
  prepared <- LinkEDA:::.rls_scale_prepare_items(data, specs)

  expect_identical(specs[[1L]]$type, "numeric")
  expect_identical(specs[[1L]]$numeric_factor_encoding$method, "binary_zero_one")
  expect_equal(prepared$q1, c(0, 1, 0, NA))
  expect_equal(prepared$q2, c(1, 1, 0, 0))
  expect_error(
    LinkEDA:::.rls_scale_item_specifications(
      data.frame(q = factor(c("a", "b", "c"))), "q", c(q = "numeric")
    ),
    "cannot be treated as a numeric scale item"
  )
})

test_that("binary scale items tolerate imported storage variants and unused levels", {
  data <- data.frame(
    character_item = c("No", "Yes", "No", NA),
    logical_item = c(FALSE, TRUE, FALSE, NA),
    factor_with_unused_level = factor(
      c("No", "Yes", "No", NA), levels = c("No", "Yes", "Not applicable")
    ),
    stringsAsFactors = FALSE
  )
  specs <- LinkEDA:::.rls_scale_item_specifications(
    data, names(data), setNames(rep("numeric", ncol(data)), names(data))
  )
  prepared <- LinkEDA:::.rls_scale_prepare_items(data, specs)

  expect_equal(prepared$character_item, c(0, 1, 0, NA))
  expect_equal(prepared$logical_item, c(0, 1, 0, NA))
  expect_equal(prepared$factor_with_unused_level, c(0, 1, 0, NA))

  changed_imputation <- data
  changed_imputation$character_item[[1L]] <- "Maybe"
  expect_error(
    LinkEDA:::.rls_scale_prepare_items(changed_imputation, specs),
    "category not present in the binary scale-item specification"
  )
})

test_that("MI dichotomous factor scales produce reliability and item results", {
  skip_if_not_installed("psych")
  skip_if_not_installed("mice")
  set.seed(8253)
  n <- 120L
  latent <- stats::rnorm(n)
  make_item <- function(offset) {
    factor(ifelse(latent + stats::rnorm(n, sd = 0.8) > offset, "Yes", "No"),
           levels = c("No", "Yes"))
  }
  original <- as.data.frame(setNames(lapply(seq(-0.4, 0.4, length.out = 8L), make_item),
                                     paste0("q", seq_len(8L))))
  original$q1[seq(4L, n, by = 11L)] <- NA
  completed <- lapply(seq_len(20L), function(index) {
    value <- original
    missing <- is.na(value$q1)
    value$q1[missing] <- factor(
      ifelse(latent[missing] + stats::rnorm(sum(missing), sd = 0.9) > -0.4,
             "Yes", "No"), levels = c("No", "Yes")
    )
    value
  })
  dataset <- list(
    group = "mi_binary_scale", dataset_id = "mi_binary_scale",
    dataset_type = "multiple_imputation", data = original,
    original_data = original, completed_datasets = completed,
    imputation_count = 20L, original_row_ids = seq_len(n),
    missing_cell_mask = lapply(original, is.na)
  )
  specs <- LinkEDA:::.rls_scale_item_specifications(
    original, names(original), setNames(rep("numeric", 8L), names(original))
  )
  fit <- LinkEDA:::.rls_scale_fit(
    dataset, specs, LinkEDA:::.rls_scale_default_options("polychoric", score = "sum")
  )

  expect_identical(fit$backend, "multiple_imputation")
  expect_equal(fit$m, 20L)
  expect_equal(nrow(fit$items), 8L)
  expect_true(all(is.finite(fit$items$item_rest_r)))
  expect_true(is.finite(fit$summary$alpha$mean))
  expect_equal(nrow(fit$reliability_by_imputation), 20L)
  expect_identical(fit$correlations$status,
                   "descriptive_mean_across_imputations_not_pooled")
  expect_true(is.matrix(fit$correlations$matrix))
})

test_that("MI scale specifications never inspect the compact worksheet preview", {
  completed <- lapply(seq_len(5L), function(index) data.frame(
    q1 = factor(c("No", "Yes", "No", "Yes"), levels = c("No", "Yes")),
    q2 = factor(c("Yes", "Yes", "No", "No"), levels = c("No", "Yes"))
  ))
  preview <- data.frame(
    q1 = c("No | Yes", "Yes", "No", "Yes | No"),
    q2 = c("Yes", "Yes | No", "No", "No | Yes"),
    stringsAsFactors = FALSE
  )
  dataset <- list(
    group = "mi_compact_scale", dataset_id = "mi_compact_scale",
    dataset_type = "multiple_imputation", data = preview,
    original_data = completed[[1L]], completed_datasets = completed,
    imputation_count = 5L, original_row_ids = seq_len(nrow(preview)),
    missing_cell_mask = lapply(completed[[1L]], is.na)
  )

  specification_data <- LinkEDA:::.rls_scale_specification_data(dataset)
  expect_identical(specification_data, completed[[1L]])
  specs <- LinkEDA:::.rls_scale_item_specifications(
    specification_data, c("q1", "q2"), c(q1 = "numeric", q2 = "numeric")
  )
  expect_equal(
    LinkEDA:::.rls_scale_prepare_items(completed[[1L]], specs)$q1,
    c(0, 1, 0, 1)
  )
})

test_that("multiple-imputation scale semantics use mice only where pooling is valid", {
  skip_if_not_installed("psych")
  skip_if_not_installed("mice")
  set.seed(8127)
  n <- 140L
  latent <- rnorm(n)
  original <- data.frame(
    q1 = latent + rnorm(n, sd = 0.5),
    q2 = latent + rnorm(n, sd = 0.5),
    q3 = latent + rnorm(n, sd = 0.5),
    q4 = latent + rnorm(n, sd = 0.5),
    q5 = latent + rnorm(n, sd = 0.5)
  )
  original$q2[sample.int(n, 20L)] <- NA_real_
  completed <- lapply(seq_len(5L), function(i) {
    out <- original
    missing <- is.na(out$q2)
    out$q2[missing] <- latent[missing] + rnorm(sum(missing), sd = 0.65)
    out
  })
  dataset <- list(
    group = "mi_scale", dataset_id = "mi_scale", dataset_type = "multiple_imputation",
    data = original, original_data = original, completed_datasets = completed,
    imputation_count = 5L, original_row_ids = seq_len(n),
    missing_cell_mask = lapply(original, is.na)
  )
  specs <- LinkEDA:::.rls_scale_item_specifications(
    original, names(original), setNames(rep("numeric", 5L), names(original))
  )
  fit <- LinkEDA:::.rls_scale_fit(
    dataset, specs, LinkEDA:::.rls_scale_default_options("pearson")
  )

  expect_identical(fit$backend, "multiple_imputation")
  expect_equal(fit$m, 5L)
  expect_identical(fit$summary$reliability_status,
                   "descriptive_by_imputation_not_Rubin_pooled")
  expect_identical(fit$correlations$status,
                   "formal_fisher_z_plus_mice_pool_scalar")
  expect_true(all(is.finite(fit$correlations$p_values[
    upper.tri(fit$correlations$p_values)
  ])))
  expect_equal(fit$correlations$sample_sizes,
               matrix(n, 5L, 5L, dimnames = list(names(original), names(original))))
  expect_match(fit$provenance$item_means, "mice::pool.scalar", fixed = TRUE)
  expect_match(fit$provenance$alpha, "not Rubin-pooled", fixed = TRUE)
  expect_equal(fit$items$missing_percent[fit$items$item == "q2"], 100 * 20 / n)
  expect_equal(sort(unique(fit$scores$imputation)), 1:5)
  expect_equal(nrow(fit$scores), n * 5L)
  expect_identical(fit$score_summary$valid_n, as.character(n))
  expect_equal(fit$score_summary$valid_n_by_imputation, rep(n, 5L))
  expect_equal(fit$score_summary$score_estimates, n * 5L)
  expect_match(fit$score_summary$status, "not Rubin-pooled", fixed = TRUE)

  means <- vapply(completed, function(x) mean(x$q2), numeric(1L))
  variances <- vapply(completed, function(x) stats::var(x$q2) / n, numeric(1L))
  reference <- mice::pool.scalar(means, variances, n = Inf, k = 1, rule = "rubin1987")
  expect_equal(fit$items$mean[fit$items$item == "q2"], reference$qbar, tolerance = 1e-12)
})

test_that("MI polychoric results stay descriptive by imputation", {
  skip_if_not_installed("psych")
  skip_if_not_installed("mice")
  set.seed(8181)
  original <- data.frame(
    a = ordered(sample(1:5, 80, TRUE)),
    b = ordered(sample(1:5, 80, TRUE)),
    c = ordered(sample(1:5, 80, TRUE)),
    d = ordered(sample(1:5, 80, TRUE))
  )
  completed <- replicate(5L, original, simplify = FALSE)
  dataset <- list(
    group = "mi_ordinal", dataset_id = "mi_ordinal", dataset_type = "multiple_imputation",
    data = original, original_data = original, completed_datasets = completed,
    imputation_count = 5L, original_row_ids = 1:80,
    missing_cell_mask = lapply(original, is.na)
  )
  specs <- LinkEDA:::.rls_scale_item_specifications(
    original, names(original), setNames(rep("ordinal", 4L), names(original))
  )
  fit <- LinkEDA:::.rls_scale_fit(
    dataset, specs, LinkEDA:::.rls_scale_default_options("polychoric")
  )
  expect_identical(fit$correlations$status,
                   "descriptive_mean_across_imputations_not_pooled")
  expect_true(is.matrix(fit$correlations$matrix))
  expect_equal(fit$correlations$matrix, fit$correlations$matrices_by_imputation[[1L]],
               tolerance = 1e-12)
  expect_true(all(is.na(fit$correlations$p_values)))
  expect_equal(fit$correlations$sample_sizes,
               matrix(80, 4L, 4L, dimnames = list(names(original), names(original))))
  expect_length(fit$correlations$matrices_by_imputation, 5L)
})

test_that("omega and ordinary EFA are the psych results, not local reconstructions", {
  skip_if_not_installed("psych")
  set.seed(8194)
  n <- 260L
  latent <- rnorm(n)
  data <- as.data.frame(replicate(6L, latent + rnorm(n, sd = 0.55)))
  names(data) <- paste0("q", seq_len(ncol(data)))
  specs <- LinkEDA:::.rls_scale_item_specifications(
    data, names(data), setNames(rep("numeric", ncol(data)), names(data))
  )
  options <- LinkEDA:::.rls_scale_default_options(
    "pearson", omega = TRUE, dimensionality = TRUE, factors = 1L,
    extraction = "minres", rotation = "none", parallel_iterations = 5L
  )
  set.seed(9021)
  fit <- LinkEDA:::.rls_scale_fit_one(data, specs, options)
  omega_reference <- suppressMessages(suppressWarnings(invisible(capture.output(
    omega_fit <- psych::omega(data, plot = FALSE, warnings = FALSE, flip = FALSE)
  ))))
  efa_reference <- psych::fa(
    fit$correlation$matrix, nfactors = 1L, n.obs = n,
    fm = "minres", rotate = "none"
  )

  expect_identical(fit$omega$function_name, "psych::omega")
  expect_equal(fit$omega$value, unname(omega_fit$omega.tot), tolerance = 1e-12)
  expect_identical(fit$dimensionality$function_name,
                   "psych::fa / psych::fa.parallel")
  expect_identical(fit$dimensionality$status, "value")
  expect_equal(abs(fit$dimensionality$loadings$MR1),
               abs(as.numeric(unclass(efa_reference$loadings)[, 1L])),
               tolerance = 1e-10)
  expect_equal(fit$dimensionality$loadings$h2,
               as.numeric(efa_reference$communality), tolerance = 1e-10)
})

test_that("Scale Analysis dimensionality supports PCA options and saved MI scores", {
  skip_if_not_installed("psych")
  set.seed(9367)
  n <- 90L
  latent <- rnorm(n)
  base <- data.frame(
    q1 = latent + rnorm(n, sd = .4),
    q2 = latent + rnorm(n, sd = .5),
    q3 = -latent + rnorm(n, sd = .6),
    q4 = rnorm(n)
  )
  completed <- list(
    base,
    transform(base, q2 = q2 + seq_len(n) / 100)
  )
  dataset_id <- ls_register_dataset("mi_scale_dimension_scores", base)
  dataset <- LinkEDA:::.rls_dataset_record(dataset_id)
  dataset$dataset_type <- "multiple_imputation"
  dataset$original_data <- base
  dataset$completed_datasets <- completed
  dataset$imputation_count <- 2L
  dataset$active_imputation_version <- 1L
  dataset$missing_cell_mask <- lapply(base, is.na)
  # The hand-built MI fixture must also have an MI source snapshot.
  LinkEDA:::.rls_store_data_version(dataset)
  LinkEDA:::.rls_set_dataset_record(dataset)

  analysis <- ls_new_scale_analysis(
    dataset_id, items = names(base),
    item_types = setNames(rep("numeric", ncol(base)), names(base)),
    dimensionality = TRUE, dimensionality_method = "pca",
    dimensionality_missing = "listwise", dimensionality_scale = TRUE,
    factors = 2L, rotation = "quartimax", parallel_iterations = 2L,
    native = FALSE
  )
  state <- ls_scale_analysis_state(analysis)
  expect_identical(state$options$dimensionality$method, "pca")
  expect_identical(state$options$dimensionality$missing, "listwise")
  expect_identical(state$options$dimensionality$rotation, "quartimax")
  expect_true(all(vapply(
    state$result$dimensionality$results_by_imputation,
    function(value) identical(value$method, "pca") &&
      all(c("PC1", "PC2") %in% names(value$scores)), logical(1L)
  )))

  added <- ls_scale_analysis_save_scores(analysis, components = 1:2)
  saved <- LinkEDA:::.rls_dataset_record(dataset_id)
  expect_identical(added, c("PC1_score", "PC2_score"))
  expect_true(all(vapply(saved$completed_datasets, function(data) {
    all(added %in% names(data))
  }, logical(1L))))
  expect_identical(saved$data_version, 2L)
  scale_score_step <- tail(saved$data_provenance$history, 1L)[[1L]]
  expect_identical(scale_score_step$output_columns, added)
  expect_match(scale_score_step$r_code, "LinkEDA::ls_scale_analysis_save_scores",
               fixed = TRUE)
  before <- ncol(saved$data)
  expect_identical(ls_scale_analysis_save_scores(analysis, components = 1:2), added)
  expect_equal(ncol(LinkEDA:::.rls_dataset_record(dataset_id)$data), before)
})

test_that("twenty imputations still use mice scalar pooling for formal item means", {
  skip_if_not_installed("psych")
  skip_if_not_installed("mice")
  set.seed(8207)
  n <- 70L
  latent <- rnorm(n)
  original <- data.frame(
    q1 = latent + rnorm(n, sd = 0.45),
    q2 = latent + rnorm(n, sd = 0.55),
    q3 = latent + rnorm(n, sd = 0.50),
    q4 = latent + rnorm(n, sd = 0.60)
  )
  original$q2[seq(3L, n, by = 7L)] <- NA_real_
  completed <- lapply(seq_len(20L), function(index) {
    value <- original
    missing <- is.na(value$q2)
    value$q2[missing] <- latent[missing] + rnorm(sum(missing), sd = 0.65)
    value
  })
  dataset <- list(
    group = "mi_scale_20", dataset_id = "mi_scale_20",
    dataset_type = "multiple_imputation", data = original,
    original_data = original, completed_datasets = completed,
    imputation_count = 20L, original_row_ids = seq_len(n),
    missing_cell_mask = lapply(original, is.na)
  )
  specs <- LinkEDA:::.rls_scale_item_specifications(
    original, names(original), setNames(rep("numeric", ncol(original)), names(original))
  )
  fit <- LinkEDA:::.rls_scale_fit(
    dataset, specs, LinkEDA:::.rls_scale_default_options("pearson")
  )
  means <- vapply(completed, function(value) mean(value$q2), numeric(1L))
  variances <- vapply(completed, function(value) stats::var(value$q2) / n, numeric(1L))
  reference <- mice::pool.scalar(means, variances, n = Inf, k = 1, rule = "rubin1987")

  expect_equal(fit$m, 20L)
  expect_equal(nrow(fit$reliability_by_imputation), 20L)
  expect_equal(fit$items$mean[fit$items$item == "q2"],
               reference$qbar, tolerance = 1e-12)
  expect_identical(fit$summary$reliability_status,
                   "descriptive_by_imputation_not_Rubin_pooled")
})

test_that("native plot payloads retain item, score, scree, and MI stability semantics", {
  skip_if_not_installed("psych")
  skip_if_not_installed("mice")
  set.seed(8241)
  n <- 90L
  latent <- rnorm(n)
  original <- data.frame(
    q1 = latent + rnorm(n, sd = 0.5),
    q2 = latent + rnorm(n, sd = 0.5),
    q3 = latent + rnorm(n, sd = 0.5),
    q4 = latent + rnorm(n, sd = 0.5)
  )
  completed <- lapply(seq_len(5L), function(index) {
    value <- original
    value$q2[seq(index, n, by = 17L)] <- value$q2[seq(index, n, by = 17L)] + 0.1 * index
    value
  })
  dataset <- list(
    group = "mi_scale_plots", dataset_id = "mi_scale_plots",
    dataset_type = "multiple_imputation", data = original,
    original_data = original, completed_datasets = completed,
    imputation_count = 5L, original_row_ids = seq_len(n),
    missing_cell_mask = lapply(original, is.na)
  )
  specs <- LinkEDA:::.rls_scale_item_specifications(
    original, names(original), setNames(rep("numeric", ncol(original)), names(original))
  )
  fit <- LinkEDA:::.rls_scale_fit(
    dataset, specs,
    LinkEDA:::.rls_scale_default_options(
      "pearson", dimensionality = TRUE, factors = 2L,
      rotation = "varimax", parallel_iterations = 3L
    )
  )
  kinds <- vapply(fit$plot_series, `[[`, character(1L), "kind")
  expect_equal(sum(kinds == "item_distribution"), ncol(original))
  expect_equal(sum(kinds == "score_distribution"), 5L)
  expect_true(all(c("scree_observed", "scree_reference", "factor_stability") %in% kinds))
  expect_equal(sum(kinds == "scree_observed_imputation"), 5L)
  expect_equal(sum(kinds == "scree_reference_imputation"), 5L)
  expect_equal(sum(kinds == "factor_loading_imputation"), 10L)
  expect_equal(sum(kinds == "factor_communality_imputation"), 5L)
  expect_equal(sum(kinds == "factor_uniqueness_imputation"), 5L)
  expect_equal(sum(kinds == "factor_score_imputation"), 10L)
  expect_length(fit$dimensionality$results_by_imputation, 5L)
  expect_true(all(vapply(fit$dimensionality$results_by_imputation, function(value) {
    is.data.frame(value$loadings) && nrow(value$loadings) == ncol(original) &&
      length(setdiff(names(value$loadings), c("item", "h2", "u2"))) == 2L &&
      is.data.frame(value$scores) && nrow(value$scores) == n &&
      ncol(value$scores) == 3L
  }, logical(1L))))
  expect_true(any(vapply(fit$plot_series, function(series) {
    identical(series$kind, "factor_loading_imputation") &&
      startsWith(series$name, "2|")
  }, logical(1L))))
  expect_true(all(vapply(fit$plot_series, function(series) {
    length(series$labels) == length(series$values) &&
      all(is.finite(as.numeric(series$values)))
  }, logical(1L))))
})

test_that("large MI factor-score payloads are serialized in one linear batch", {
  sent <- NULL
  testthat::local_mocked_bindings(
    .rls_send = function(lines, expect_reply = TRUE) {
      sent <<- lines
      "OK"
    },
    .package = "LinkEDA"
  )
  point_count <- 5000L
  series <- list(
    kind = "factor_score_imputation", name = "1|F1",
    labels = as.character(seq_len(point_count)),
    values = seq_len(point_count) / point_count
  )
  record <- list(
    id = "large_scale_wire", group = "mi_scale_wire",
    model_version = 2L, specification_fingerprint = "fingerprint",
    item_specifications = list(), options = list(score = "mean"),
    result = list(
      backend = "multiple_imputation", m = 20L, status = "test",
      revision = 2L, fingerprint = "fingerprint", summary = list(),
      items = data.frame(), provenance = list(),
      reliability_by_imputation = data.frame(), correlations = list(),
      dimensionality = list(), score_summary = list(),
      plot_series = list(series)
    )
  )

  expect_true(LinkEDA:::.rls_scale_sync_native(record))
  marker <- match("SCALE_PLOTS_V1", sent)
  expect_false(is.na(marker))
  expect_identical(sent[[marker + 1L]], "1")
  expect_identical(sent[[marker + 2L]], series$kind)
  expect_identical(sent[[marker + 3L]], series$name)
  expect_identical(sent[[marker + 4L]], as.character(point_count))
  expect_identical(sent[[marker + 5L]], "1")
  expect_identical(sent[[marker + 6L]], "0.000200")
  expect_identical(sent[[marker + 3L + 2L * point_count]], as.character(point_count))
  expect_identical(match("SCALE_SPEC_V1", sent), marker + 5L + 2L * point_count)
  expect_identical(length(sent), marker + 4L + 2L * point_count + 13L)
})

test_that("macOS derived scale reports keep the full scrollable document size", {
  root <- linkeda_source_test_root()
  mac <- paste(readLines(file.path(
    root, "src", "platform", "macos", "linkeda_macos_app.mm"
  ), warn = FALSE), collapse = "\n")

  expect_match(
    mac,
    "model.layout.preferredHeight + stabilityHeight + 130.0",
    fixed = TRUE
  )
  expect_false(grepl(
    "std::min(900.0, model.layout.preferredHeight + stabilityHeight + 130.0)",
    mac,
    fixed = TRUE
  ))
  expect_match(mac, "[_scrollView setAutohidesScrollers:NO]", fixed = TRUE)
})


test_that("binary categorical items are detected without changing worksheet types", {
  data <- data.frame(q = factor(c("No", "Yes", "No", NA), levels = c("No", "Yes")))
  specs <- LinkEDA:::.rls_scale_item_specifications(data, "q")
  expect_identical(specs[[1L]]$type, "ordinal")
  expect_false(is.ordered(data$q))
  expect_identical(levels(LinkEDA:::.rls_scale_prepare_items(data, specs)$q), c("No", "Yes"))
})

test_that("factor score validation blocks indefinite and numerically singular matrices", {
  skip_if_not_installed("psych")
  set.seed(924)
  x <- as.data.frame(matrix(rnorm(200 * 11), 200, 11))
  r <- cor(x)
  r[1, 2] <- r[2, 1] <- .95
  r[1, 3] <- r[3, 1] <- .95
  r[2, 3] <- r[3, 2] <- -.95
  raw <- LinkEDA:::.rls_dimension_factor_from_correlation(r, 200, 5, score_data = x)
  expect_match(raw$status, "not positive definite")
  expect_equal(nrow(raw$scores), 0L)
  smoothed <- suppressWarnings(psych::cor.smooth(r))
  # The previous pathway returns scores of order 10^8 on this matrix.
  fit <- suppressMessages(suppressWarnings(psych::fa(smoothed, nfactors = 5,
    fm = "minres", rotate = "oblimin", n.obs = 200)))
  old <- suppressMessages(suppressWarnings(psych::factor.scores(x, fit,
    method = "Thurstone", rho = smoothed)$scores))
  expect_gt(max(abs(old)), 1e6)
  checked <- LinkEDA:::.rls_dimension_factor_from_correlation(smoothed, 200, 5, score_data = x)
  expect_match(checked$status, "numerically singular")
  expect_equal(nrow(checked$scores), 0L)
})

test_that("valid factor scores remain psych regression scores and metadata is truthful", {
  skip_if_not_installed("psych")
  set.seed(933)
  z <- matrix(rnorm(1200), 600, 2)
  x <- as.data.frame(cbind(z[,1] + matrix(rnorm(1800), 600, 3),
                          z[,2] + matrix(rnorm(1800), 600, 3)))
  names(x) <- paste0("q", 1:6)
  x$q1[1:4] <- NA_real_
  specs <- LinkEDA:::.rls_scale_item_specifications(x, names(x))
  opts <- LinkEDA:::.rls_scale_default_options(dimensionality = TRUE, factors = 2,
    extraction = "minres", rotation = "oblimin", parallel_iterations = 2)
  fitted <- LinkEDA:::.rls_scale_fit_one(x, specs, opts)
  r <- cor(x, use = "pairwise.complete.obs")
  reference <- psych::fa(r, nfactors = 2, n.obs = 596, fm = "minres", rotate = "oblimin")
  actual <- fitted$dimensionality
  # psych uses multiple rotation starts; compare the re-fit at its numerical
  # optimization tolerance, then check scoring against the exact returned fit.
  loads <- as.matrix(actual$loadings[, setdiff(names(actual$loadings), c("item", "h2", "u2"))])
  expect_equal(unname(loads), unname(unclass(reference$loadings)), ignore_attr = TRUE, tolerance = 1e-4)
  reference$loadings <- loads
  reference$Phi <- actual$factor_correlations
  expected <- psych::factor.scores(x[complete.cases(x), ], reference,
    method = "Thurstone", rho = fitted$correlation$matrix, missing = FALSE)$scores
  expect_equal(unname(as.matrix(actual$scores[, -1])), unname(expected), tolerance = 1e-12)
  expect_identical(actual$scores$row, which(complete.cases(x)))
  expect_match(actual$report_method, "psych::fa (minres); oblimin; pearson; pairwise", fixed = TRUE)
  expect_match(actual$report_summary, "596 complete item rows; 4 incomplete rows", fixed = TRUE)
  metadata <- LinkEDA:::.rls_scale_dimensionality_series(list(actual))[[1L]]
  expect_identical(metadata$kind, "dimensionality_metadata")
  expect_false(any(grepl("imputation", metadata$labels, ignore.case = TRUE)))
})

test_that("scale polychoric matrices are not silently smoothed and listwise removes incomplete rows", {
  skip_if_not_installed("psych")
  set.seed(921)
  x <- as.data.frame(matrix(sample(1:3, 200*4, replace = TRUE), 200, 4))
  x[1:10, 1] <- NA
  specs <- LinkEDA:::.rls_scale_item_specifications(x, names(x), setNames(rep("ordinal", 4), names(x)))
  prep <- LinkEDA:::.rls_scale_prepare_items(x, specs)
  result <- LinkEDA:::.rls_scale_correlations_one(prep, specs, "polychoric", "listwise")
  reference <- psych::polychoric(x[complete.cases(x), ], smooth = FALSE, progress = FALSE)$rho
  expect_equal(result$matrix, reference, tolerance = 1e-12)
  expect_true(all(result$sample_sizes == 190))
})

test_that("a Heywood solution is shown with a warning but never supplies factor scores", {
  skip_if_not_installed("psych")
  r <- matrix(c(1, .9, .9, .9, 1, .65, .9, .65, 1), 3, 3)
  dimnames(r) <- list(paste0("q", 1:3), paste0("q", 1:3))
  set.seed(949)
  x <- as.data.frame(matrix(rnorm(500 * 3), 500, 3))
  result <- suppressWarnings(LinkEDA:::.rls_dimension_factor_from_correlation(
    r, 500, 1, rotation = "none", parallel_iterations = 1, score_data = x))
  expect_equal(nrow(result$loadings), 3L)
  expect_true(any(result$loadings$u2 < 0))
  expect_match(result$score_status, "Improper factor solution")
  expect_equal(nrow(result$scores), 0L)
})
