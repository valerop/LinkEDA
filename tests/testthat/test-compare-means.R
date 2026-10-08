test_that("One-sample t test matches stats::t.test", {
  set.seed(42)
  x <- rnorm(30, mean = 0.5, sd = 1)
  d <- data.frame(y = x, id = seq_along(x))
  ls_register_dataset("cm_onesamp", d)

  r_ref <- stats::t.test(x, mu = 0, alternative = "two.sided", conf.level = 0.95)
  ids <- ls_new_one_sample_t_test("cm_onesamp", response = "y", mu = 0, alternative = "two.sided", conf_level = 0.95)
  result <- LinkEDA:::.rls_compare_means_record(ids[[1L]])

  expect_equal(result$analysis_type, "one_sample_t_test")
  expect_equal(result$analysis_backend, "ordinary")
  expect_equal(result$test_results$statistic, unname(r_ref$statistic), tolerance = 1e-10)
  expect_equal(result$test_results$parameter, unname(r_ref$parameter), tolerance = 1e-10)
  expect_equal(result$test_results$p_value, r_ref$p.value, tolerance = 1e-10)
  expect_equal(result$test_results$mean_diff, unname(r_ref$estimate), tolerance = 1e-10)
  expect_equal(result$test_results$conf_int[[1L]], r_ref$conf.int[[1L]], tolerance = 1e-10)
  expect_equal(result$test_results$conf_int[[2L]], r_ref$conf.int[[2L]], tolerance = 1e-10)
  expect_equal(result$descriptives$n, 30L)
  expect_equal(result$descriptives$mean, mean(x), tolerance = 1e-10)
  expect_equal(result$descriptives$sd, stats::sd(x), tolerance = 1e-10)
  expect_true(is.finite(result$effect_sizes$cohens_d))
  expect_false(is.null(result$result_id))
  expect_equal(result$result_version, 1L)

  ids_less <- ls_new_one_sample_t_test("cm_onesamp", response = "y", mu = 0, alternative = "less", conf_level = 0.95)
  r_less <- stats::t.test(x, mu = 0, alternative = "less", conf.level = 0.95)
  result_less <- LinkEDA:::.rls_compare_means_record(ids_less[[1L]])
  expect_equal(result_less$test_results$p_value, r_less$p.value, tolerance = 1e-10)

  ids_greater <- ls_new_one_sample_t_test("cm_onesamp", response = "y", mu = 0.5, alternative = "greater", conf_level = 0.95)
  r_greater <- stats::t.test(x, mu = 0.5, alternative = "greater", conf.level = 0.95)
  result_greater <- LinkEDA:::.rls_compare_means_record(ids_greater[[1L]])
  expect_equal(result_greater$test_results$p_value, r_greater$p.value, tolerance = 1e-10)
})

test_that("compare-means native tables carry public R verification recipes", {
  data <- data.frame(
    outcome = c(1, 2, 3, 5, 6, 8),
    group = factor(c("A", "A", "A", "B", "B", "B"))
  )
  ls_register_dataset("compare_means_r_code", data)
  id <- ls_new_independent_samples_t_test(
    "compare_means_r_code", response = "outcome", group = "group",
    p_adjust = "none"
  )
  result <- LinkEDA:::.rls_compare_means_record(id[[1L]])
  recipe <- LinkEDA:::.rls_compare_means_verification_r_code(list(result))$code
  expect_silent(parse(text = recipe))
  expect_match(recipe, "stats::t.test", fixed = TRUE)
  expect_false(grepl("LinkEDA:::", recipe, fixed = TRUE))

  payload <- LinkEDA:::.rls_compare_means_batch_native_payload(
    list(result), "compare_means_r_code_output"
  )
  expect_true("ANALYSIS_PROVENANCE_V2" %in% payload)
})

test_that("One-sample t test handles missing values", {
  d <- data.frame(y = c(1, 2, 3, NA, 5, NA, 7))
  ls_register_dataset("cm_onesamp_na", d)
  ids <- ls_new_one_sample_t_test("cm_onesamp_na", response = "y", mu = 0)
  result <- LinkEDA:::.rls_compare_means_record(ids[[1L]])
  expect_equal(result$descriptives$n, 5L)
  expect_equal(length(result$rows_used_original_ids), 5L)
  expect_equal(length(result$rows_excluded_original_ids), 2L)
})

test_that("One-sample Wilcoxon is calculated by stats::wilcox.test", {
  x <- c(1, 2, 4, 5, 7, 8, 10)
  ls_register_dataset("cm_wilcoxon_one", data.frame(y = x))
  reference <- stats::wilcox.test(x, mu = 3, exact = FALSE, conf.int = TRUE)
  id <- ls_new_one_sample_t_test("cm_wilcoxon_one", "y", mu = 3,
                                 method = "wilcoxon", p_adjust = "none")
  result <- LinkEDA:::.rls_compare_means_record(id[[1L]])
  expect_equal(result$test_results$method, "Wilcoxon signed-rank test")
  expect_equal(result$test_results$statistic, unname(reference$statistic), tolerance = 1e-12)
  expect_equal(result$test_results$p_value, reference$p.value, tolerance = 1e-12)
  expect_true(is.finite(result$effect_sizes$rank_biserial))
})

test_that("one-sample responses can use separate null values and automatic test families", {
  d <- data.frame(
    score = c(54, 57, 58, 60, 61, 64, 65, 68),
    rank = ordered(paste0("level", seq_len(8L)),
                   levels = paste0("level", seq_len(8L))),
    happy = factor(c("no", "yes", "yes", "no", "yes", "yes", "yes", "yes"),
                   levels = c("no", "yes"))
  )
  ls_register_dataset("cm_mixed_one", d)
  ids <- ls_new_one_sample_t_test(
    "cm_mixed_one", c("score", "rank", "happy"),
    mu = c(score = 60, rank = 2, happy = 0.5),
    method = c(score = "student", rank = "wilcoxon", happy = "binomial"),
    p_adjust = "holm"
  )
  records <- lapply(ids, LinkEDA:::.rls_compare_means_record)
  expect_equal(vapply(records, function(x) x$specification$null_mu, numeric(1L)),
               c(60, 2, 0.5))
  expect_equal(vapply(records, function(x) x$specification$test_family, character(1L)),
               c("student", "wilcoxon", "binomial"))
  expect_equal(records[[2L]]$test_results$method, "Wilcoxon signed-rank test")
  expect_equal(records[[3L]]$test_results$method, "Exact binomial test")
  expect_equal(records[[3L]]$specification$event_level, "yes")
  expect_equal(records[[3L]]$test_results$successes, 6)
  expect_equal(records[[3L]]$test_results$observed_proportion, 0.75)
  reference <- stats::binom.test(6, 8, p = 0.5)
  expect_equal(records[[3L]]$test_results$p_value, reference$p.value)
})

test_that("Welch independent t test matches stats::t.test(var.equal = FALSE)", {
  set.seed(42)
  g1 <- rnorm(15, mean = 1, sd = 1)
  g2 <- rnorm(12, mean = 0.5, sd = 1.5)
  d <- data.frame(y = c(g1, g2), g = factor(rep(c("A", "B"), c(15, 12))))
  ls_register_dataset("cm_ind_welch", d)

  r_ref <- stats::t.test(y ~ g, data = d, var.equal = FALSE, alternative = "two.sided", conf.level = 0.95)
  ids <- ls_new_independent_samples_t_test("cm_ind_welch", response = "y", group = "g", alternative = "two.sided", conf_level = 0.95)
  result <- LinkEDA:::.rls_compare_means_record(ids[[1L]])

  expect_equal(result$analysis_type, "independent_samples_t_test")
  expect_equal(result$test_results$method, "Welch t-test")
  expect_equal(result$test_results$statistic, unname(r_ref$statistic), tolerance = 1e-10)
  expect_equal(result$test_results$parameter, unname(r_ref$parameter), tolerance = 1e-8)
  expect_equal(result$test_results$p_value, r_ref$p.value, tolerance = 1e-10)
  expect_equal(result$test_results$mean_diff, unname(r_ref$estimate[[1L]] - r_ref$estimate[[2L]]), tolerance = 1e-10)
  expect_equal(result$test_results$conf_int[[1L]], r_ref$conf.int[[1L]], tolerance = 1e-10)
  expect_equal(result$test_results$conf_int[[2L]], r_ref$conf.int[[2L]], tolerance = 1e-10)
  expect_equal(result$descriptives$n, c(15, 12))
  expect_equal(result$descriptives$mean[[1L]], mean(g1), tolerance = 1e-10)
  expect_equal(result$descriptives$mean[[2L]], mean(g2), tolerance = 1e-10)
  expect_true(is.finite(result$effect_sizes$cohens_d))
  expect_equal(length(result$rows_used_original_ids), 27L)
})

test_that("Independent t test group level ordering and contrast direction", {
  d <- data.frame(y = c(10, 12, 14, 16, 18, 20, 1, 3, 5, 7, 9, 11),
                  g = factor(c(rep("Z", 6), rep("A", 6)), levels = c("Z", "A")))
  ls_register_dataset("cm_ind_order", d)
  ids <- ls_new_independent_samples_t_test("cm_ind_order", response = "y", group = "g")
  result <- LinkEDA:::.rls_compare_means_record(ids[[1L]])
  expect_equal(result$specification$group_levels, c("Z", "A"))
  expect_equal(result$specification$group, "g")
  expect_equal(result$test_results$group_reference, "A")
  expect_equal(result$test_results$group_comparison, "Z")
  expect_equal(result$test_results$mean_diff, 9)
  expect_match(result$test_results$direction_note, "Z - A", fixed = TRUE)
})

test_that("Two-sample tests choose any ordered pair from a factor with more than two levels", {
  d <- data.frame(
    y = c(5, 6, 7, 3, 4, 5, 20, 21, 22),
    g = factor(rep(c("Control", "Treatment", "Waitlist"), each = 3L),
               levels = c("Control", "Treatment", "Waitlist"))
  )
  ls_register_dataset("cm_ind_three_groups", d)

  forward_id <- ls_new_independent_samples_t_test(
    "cm_ind_three_groups", "y", "g",
    group_order = c("Control", "Treatment"), p_adjust = "none"
  )
  reverse_id <- ls_new_independent_samples_t_test(
    "cm_ind_three_groups", "y", "g",
    group_order = c("Treatment", "Control"), p_adjust = "none"
  )
  forward <- LinkEDA:::.rls_compare_means_record(forward_id[[1L]])
  reverse <- LinkEDA:::.rls_compare_means_record(reverse_id[[1L]])

  expect_identical(forward$specification$group_levels,
                   c("Control", "Treatment"))
  expect_identical(reverse$specification$group_levels,
                   c("Treatment", "Control"))
  expect_equal(forward$descriptives$n, c(3, 3))
  expect_equal(forward$test_results$mean_diff, 2)
  expect_equal(reverse$test_results$mean_diff, -2)
  expect_equal(reverse$test_results$statistic,
               -forward$test_results$statistic, tolerance = 1e-12)
  expect_equal(as.numeric(reverse$test_results$conf_int),
               as.numeric(-rev(forward$test_results$conf_int)), tolerance = 1e-12)
  expect_equal(reverse$test_results$p_value,
               forward$test_results$p_value, tolerance = 1e-12)
  expect_equal(reverse$effect_sizes$hedges_g,
               -forward$effect_sizes$hedges_g, tolerance = 1e-12)
  expect_false(any(forward$rows_used_original_ids > 6L))
})

test_that("Two-sample selected-row scope never falls back to all data", {
  d <- data.frame(
    y = c(5, 6, 50, 3, 4, -30),
    g = factor(rep(c("Control", "Treatment"), each = 3L))
  )
  ls_register_dataset("cm_ind_selected_scope", d)

  all_id <- ls_new_independent_samples_t_test(
    "cm_ind_selected_scope", "y", "g", p_adjust = "none"
  )
  selected_id <- ls_new_independent_samples_t_test(
    "cm_ind_selected_scope", "y", "g", scope = "selected",
    .selected_rows = c(1L, 2L, 4L, 5L), p_adjust = "none"
  )
  all_result <- LinkEDA:::.rls_compare_means_record(all_id[[1L]])
  selected <- LinkEDA:::.rls_compare_means_record(selected_id[[1L]])

  expect_equal(all_result$descriptives$n, c(3, 3))
  expect_equal(selected$descriptives$n, c(2, 2))
  expect_equal(selected$test_results$mean_diff, 2)
  expect_identical(sort(selected$rows_used_original_ids), c(1L, 2L, 4L, 5L))
  expect_error(
    ls_new_independent_samples_t_test(
      "cm_ind_selected_scope", "y", "g", scope = "selected",
      .selected_rows = integer(), p_adjust = "none"
    ),
    "Not enough complete cases across both groups"
  )
})

test_that("Mann-Whitney is calculated by stats::wilcox.test", {
  d <- data.frame(y = c(1, 2, 3, 5, 6, 8, 10, 11),
                  g = factor(rep(c("A", "B"), each = 4)))
  ls_register_dataset("cm_mann_whitney", d)
  reference <- stats::wilcox.test(d$y[d$g == "A"], d$y[d$g == "B"],
                                  exact = FALSE, conf.int = TRUE)
  id <- ls_new_independent_samples_t_test(
    "cm_mann_whitney", "y", "g", method = "mann_whitney", p_adjust = "none"
  )
  result <- LinkEDA:::.rls_compare_means_record(id[[1L]])
  expect_equal(result$test_results$method, "Mann-Whitney U test")
  expect_equal(result$test_results$statistic, unname(reference$statistic), tolerance = 1e-12)
  expect_equal(result$test_results$p_value, reference$p.value, tolerance = 1e-12)
  expect_true(is.finite(result$effect_sizes$rank_biserial))
})

test_that("independent samples dispatch continuous, ordinal, and binary responses", {
  d <- data.frame(
    group = factor(rep(c("control", "treatment"), each = 8),
                   levels = c("control", "treatment")),
    score = c(10, 11, 12, 13, 14, 15, 16, 17,
              14, 15, 16, 17, 18, 19, 20, 21),
    rating = ordered(c(1, 2, 2, 3, 3, 4, 4, 5,
                       2, 3, 3, 4, 4, 5, 5, 5)),
    improved = factor(c("no", "no", "no", "no", "no", "yes", "yes", "yes",
                        "no", "yes", "yes", "yes", "yes", "yes", "yes", "yes"),
                      levels = c("no", "yes"))
  )
  ls_register_dataset("cm_independent_mixed", d)
  ids <- ls_new_independent_samples_t_test(
    "cm_independent_mixed", c("score", "rating", "improved"), "group",
    method = c(score = "welch", rating = "mann_whitney", improved = "proportion"),
    p_adjust = "holm"
  )
  results <- lapply(ids, LinkEDA:::.rls_compare_means_record)
  expect_equal(vapply(results, function(x) x$specification$test_family, character(1L)),
               c("welch", "mann_whitney", "proportion"))

  welch_reference <- stats::t.test(score ~ group, d, var.equal = FALSE)
  expect_equal(results[[1L]]$test_results$p_value, welch_reference$p.value,
               tolerance = 1e-12)
  rank_reference <- stats::wilcox.test(
    as.numeric(d$rating[d$group == "control"]),
    as.numeric(d$rating[d$group == "treatment"]),
    exact = FALSE, conf.int = TRUE
  )
  expect_equal(results[[2L]]$test_results$p_value, rank_reference$p.value,
               tolerance = 1e-12)
  expect_equal(results[[2L]]$descriptives$mean,
               c(stats::median(as.numeric(d$rating[d$group == "control"])),
                 stats::median(as.numeric(d$rating[d$group == "treatment"]))))

  events <- c(3, 7)
  proportion_reference <- suppressWarnings(stats::prop.test(events, c(8, 8), correct = FALSE))
  expect_equal(results[[3L]]$specification$event_level, "yes")
  expect_equal(unname(results[[3L]]$test_results$successes), events)
  expect_equal(results[[3L]]$test_results$p_value, proportion_reference$p.value,
               tolerance = 1e-12)
  expect_equal(results[[3L]]$test_results$mean_diff, 3 / 8 - 7 / 8)
  expect_true(is.finite(results[[3L]]$effect_sizes$cohens_h))
  expect_true(all(vapply(results, function(x)
    x$test_results$adjustment_family_size, integer(1L)) == 3L))

  state_env <- get(".rls_state", envir = asNamespace("LinkEDA"))
  old_mixed <- state_env$compare_means_mixed_independent
  old_method <- state_env$compare_means_batch_method
  if (is.null(old_mixed)) old_mixed <- FALSE
  if (is.null(old_method)) old_method <- ""
  on.exit({
    state_env$compare_means_mixed_independent <- old_mixed
    state_env$compare_means_batch_method <- old_method
  }, add = TRUE)
  state_env$compare_means_mixed_independent <- TRUE
  state_env$compare_means_batch_method <- "mann_whitney"
  payload <- LinkEDA:::.rls_compare_means_batch_native_payload(results, "mixed_independent")
  expect_identical(payload[[2L]], "COMPARE_MEANS_BATCH_V3")
  expect_true("Mann-Whitney for numeric/ordinal; proportions for binary" %in% payload)
  expect_true(all(c("mann_whitney", "proportion", "yes") %in% payload))
  expect_true(all(!is.na(iconv(payload, from = "UTF-8", to = "UTF-8"))))
  connection <- rawConnection(raw(), "wb")
  on.exit(close(connection), add = TRUE)
  expect_silent(writeLines(payload, connection, useBytes = TRUE))
  expect_false(any(rawConnectionValue(connection) == as.raw(0L)))
})

test_that("Paired t test matches stats::t.test(paired = TRUE)", {
  set.seed(42)
  pre <- rnorm(20, mean = 5, sd = 1)
  post <- pre + rnorm(20, mean = 0.3, sd = 0.5)
  d <- data.frame(pre = pre, post = post)
  ls_register_dataset("cm_paired", d)

  r_ref <- stats::t.test(pre, post, paired = TRUE, alternative = "two.sided", conf.level = 0.95)
  ids <- ls_new_paired_samples_t_test("cm_paired", pairs = list(c("pre", "post")),
                                      alternative = "two.sided", conf_level = 0.95)
  result <- LinkEDA:::.rls_compare_means_record(ids[[1L]])

  expect_equal(result$analysis_type, "paired_samples_t_test")
  expect_equal(result$test_results$statistic, unname(r_ref$statistic), tolerance = 1e-10)
  expect_equal(result$test_results$parameter, unname(r_ref$parameter), tolerance = 1e-10)
  expect_equal(result$test_results$p_value, r_ref$p.value, tolerance = 1e-10)
  expect_equal(result$test_results$mean_diff, unname(r_ref$estimate), tolerance = 1e-10)
  expect_equal(result$test_results$conf_int[[1L]], r_ref$conf.int[[1L]], tolerance = 1e-10)
  expect_equal(result$test_results$conf_int[[2L]], r_ref$conf.int[[2L]], tolerance = 1e-10)
  expect_equal(result$descriptives$differences$n, 20L)
  expect_equal(result$descriptives$differences$mean, mean(pre - post), tolerance = 1e-10)
  expect_equal(result$descriptives$differences$sd, stats::sd(pre - post), tolerance = 1e-10)
  expect_true(is.finite(result$effect_sizes$cohens_dz))
  expect_equal(length(result$rows_used_original_ids), 20L)
})

test_that("Paired t test handles missing values (complete pairs)", {
  d <- data.frame(a = c(1, 2, NA, 4, 5), b = c(2, NA, 3, NA, 7))
  ls_register_dataset("cm_paired_na", d)
  ids <- ls_new_paired_samples_t_test("cm_paired_na", pairs = list(c("a", "b")))
  result <- LinkEDA:::.rls_compare_means_record(ids[[1L]])
  expect_equal(result$descriptives$differences$n, 2L)
  expect_equal(length(result$rows_used_original_ids), 2L)
  expect_equal(length(result$rows_excluded_original_ids), 3L)
})

test_that("Paired Wilcoxon is calculated by stats::wilcox.test", {
  before <- c(9, 7, 8, 6, 10, 5, 11)
  after <- c(7, 6, 5, 7, 8, 4, 9)
  ls_register_dataset("cm_wilcoxon_paired", data.frame(before, after))
  reference <- stats::wilcox.test(before, after, paired = TRUE, exact = FALSE,
                                  conf.int = TRUE)
  id <- ls_new_paired_samples_t_test(
    "cm_wilcoxon_paired", list(c("before", "after")),
    method = "wilcoxon", p_adjust = "none"
  )
  result <- LinkEDA:::.rls_compare_means_record(id[[1L]])
  expect_equal(result$test_results$method, "Paired Wilcoxon signed-rank test")
  expect_equal(result$test_results$statistic, unname(reference$statistic), tolerance = 1e-12)
  expect_equal(result$test_results$p_value, reference$p.value, tolerance = 1e-12)
  expect_true(is.finite(result$effect_sizes$rank_biserial))
})

test_that("Paired reports select t, Wilcoxon and McNemar per pair", {
  d <- data.frame(
    pre = c(1, 2, 3, 4, 5, 6),
    post = c(1, 1, 2, 4, 4, 5),
    ordinal_pre = ordered(c("low", "low", "mid", "mid", "high", "high"),
                          levels = c("low", "mid", "high")),
    ordinal_post = ordered(c("low", "mid", "mid", "mid", "high", "mid"),
                           levels = c("low", "mid", "high")),
    binary_pre = factor(c("no", "yes", "yes", "no", "yes", "no"),
                        levels = c("no", "yes")),
    binary_post = factor(c("yes", "yes", "no", "no", "no", "no"),
                         levels = c("no", "yes"))
  )
  ls_register_dataset("cm_paired_mixed", d)
  ids <- ls_new_paired_samples_t_test(
    "cm_paired_mixed",
    list(c("pre", "post"), c("ordinal_pre", "ordinal_post"),
         c("binary_pre", "binary_post")),
    method = NULL, p_adjust = "holm"
  )
  results <- lapply(ids, LinkEDA:::.rls_compare_means_record)
  expect_identical(vapply(results, function(x) x$specification$test_family, character(1L)),
                   c("student", "wilcoxon", "mcnemar"))
  expect_equal(results[[3L]]$test_results$p_value,
               stats::binom.test(1, 3, p = 0.5)$p.value)
  paired_binary_difference <-
    as.numeric(d$binary_pre == "yes") - as.numeric(d$binary_post == "yes")
  expect_equal(results[[3L]]$descriptives$differences$sd,
               stats::sd(paired_binary_difference))
  expect_equal(results[[3L]]$descriptives$differences$se,
               stats::sd(paired_binary_difference) / sqrt(nrow(d)))
  expect_equal(results[[3L]]$test_results$mean_diff_se,
               results[[3L]]$descriptives$differences$se)
  expect_true(is.finite(results[[3L]]$effect_sizes$matched_odds_ratio))

  state_env <- LinkEDA:::.rls_state
  old <- state_env$compare_means_mixed_paired
  on.exit(state_env$compare_means_mixed_paired <- old, add = TRUE)
  state_env$compare_means_mixed_paired <- TRUE
  payload <- LinkEDA:::.rls_compare_means_batch_native_payload(results, "paired_mixed")
  expect_identical(payload[[2L]], "COMPARE_MEANS_BATCH_V3")
  expect_true(all(c("student", "wilcoxon", "mcnemar", "yes") %in% payload))
  individual <- LinkEDA:::.rls_compare_means_native_payload(results[[3L]])
  sd_label <- match("SD of differences", individual)
  se_label <- match("SE of differences", individual)
  expect_equal(as.numeric(individual[[sd_label + 1L]]),
               stats::sd(paired_binary_difference))
  expect_equal(as.numeric(individual[[se_label + 1L]]),
               stats::sd(paired_binary_difference) / sqrt(nrow(d)))
  odds_label <- match("Matched odds ratio", individual)
  expect_equal(as.numeric(individual[[odds_label + 1L]]), 2)
})

test_that("native compare-means delivery retries once and reports failure", {
  state <- LinkEDA:::.rls_state
  previous <- list(
    process_started = state$process_started,
    backend_kind = state$backend_kind,
    task_poll_connection = state$task_poll_connection
  )
  on.exit({
    state$process_started <- previous$process_started
    state$backend_kind <- previous$backend_kind
    state$task_poll_connection <- previous$task_poll_connection
  }, add = TRUE)
  state$process_started <- TRUE
  state$backend_kind <- "winui"
  state$task_poll_connection <- NULL

  attempts <- 0L
  local_mocked_bindings(
    .rls_send = function(payload) {
      attempts <<- attempts + 1L
      if (attempts == 1L) stop("stale socket")
      "OK"
    },
    .rls_close_winui_task_poll_connection = function() invisible(NULL),
    .package = "LinkEDA"
  )
  expect_true(LinkEDA:::.rls_send_native_analysis_result("payload", "test result"))
  expect_equal(attempts, 2L)

  attempts <- 0L
  local_mocked_bindings(
    .rls_send = function(payload) {
      attempts <<- attempts + 1L
      stop("still unavailable")
    },
    .rls_close_winui_task_poll_connection = function() invisible(NULL),
    .package = "LinkEDA"
  )
  expect_error(
    LinkEDA:::.rls_send_native_analysis_result("payload", "test result"),
    "Could not deliver the test result"
  )
  expect_equal(attempts, 2L)
})

test_that("Windows paired-mixed task renders the initial Alien pair", {
  alien_path <- system.file("examples", "Alien.csv", package = "LinkEDA")
  expect_true(nzchar(alien_path))
  ls_register_dataset("paired_alien_restart", utils::read.csv(alien_path))

  state <- LinkEDA:::.rls_state
  previous <- list(
    process_started = state$process_started,
    backend_kind = state$backend_kind,
    run_id = state$compare_means_run_id,
    mixed = state$compare_means_mixed_paired,
    method = state$compare_means_batch_method
  )
  on.exit({
    state$process_started <- previous$process_started
    state$backend_kind <- previous$backend_kind
    state$compare_means_run_id <- previous$run_id
    state$compare_means_mixed_paired <- previous$mixed
    state$compare_means_batch_method <- previous$method
  }, add = TRUE)
  state$process_started <- TRUE
  state$backend_kind <- "winui"
  delivered <- NULL
  local_mocked_bindings(
    .rls_send = function(payload) {
      delivered <<- payload
      "OK"
    },
    .package = "LinkEDA"
  )

  pair_key <- paste0("humans_eaten", intToUtf8(31L), "size")
  parts <- c(
    "COMPARE_MEANS_BATCH_NEEDED", "paired_alien_restart_run",
    "paired_alien_restart", "paired_mixed", "two.sided", "0.95",
    "student", "holm", "0", "", "0", "1", "humans_eaten", "size",
    "0", "all", "0", "1", pair_key, "0", "student"
  )
  LinkEDA:::.rls_handle_compare_means_batch_needed(parts)

  expect_identical(delivered[[1L]], "COMPARE_MEANS_BATCH_OPEN")
  expect_identical(delivered[[2L]], "COMPARE_MEANS_BATCH_V3")
  expect_identical(delivered[[3L]], "paired_alien_restart_run")
  expect_true(all(c("humans_eaten", "size", "student") %in% delivered))
})

test_that("native two-sample tasks discard a stale order from another grouping variable", {
  d <- data.frame(
    age = c(12, 13, 14, 15, 16, 17, 18, 19),
    country = factor(rep(c("Finland", "Greece"), each = 4)),
    gender = factor(rep(c("Female", "Male"), 4))
  )
  ls_register_dataset("cm_native_group_order", d)

  state <- LinkEDA:::.rls_state
  previous <- list(
    process_started = state$process_started,
    backend_kind = state$backend_kind,
    run_id = state$compare_means_run_id,
    mixed = state$compare_means_mixed_independent,
    method = state$compare_means_batch_method
  )
  on.exit({
    state$process_started <- previous$process_started
    state$backend_kind <- previous$backend_kind
    state$compare_means_run_id <- previous$run_id
    state$compare_means_mixed_independent <- previous$mixed
    state$compare_means_batch_method <- previous$method
  }, add = TRUE)
  state$process_started <- TRUE
  state$backend_kind <- "winui"
  delivered <- NULL
  local_mocked_bindings(
    .rls_send = function(payload) {
      delivered <<- payload
      "OK"
    },
    .package = "LinkEDA"
  )

  # The grouping variable has changed to country, but the window submits the
  # previous gender order.  R must derive the current levels, not reject the
  # otherwise valid analysis.
  parts <- c(
    "COMPARE_MEANS_BATCH_NEEDED", "native_group_order_run",
    "cm_native_group_order", "independent_t", "two.sided", "0.95",
    "welch", "holm", "0", "country", "1", "age", "0",
    "2", "Female", "Male", "all", "0", "0"
  )
  LinkEDA:::.rls_handle_compare_means_batch_needed(parts)

  expect_identical(delivered[[1L]], "COMPARE_MEANS_BATCH_OPEN")
  expect_identical(delivered[[3L]], "native_group_order_run")
  expect_true(all(c("Finland", "Greece") %in% delivered))
  expect_false("Group order must contain every effective group exactly once." %in% delivered)
})

test_that("Welch one-way ANOVA matches stats::oneway.test(var.equal = FALSE)", {
  set.seed(42)
  a <- rnorm(10, mean = 0, sd = 1)
  b <- rnorm(10, mean = 0.5, sd = 1.2)
  c <- rnorm(10, mean = 1, sd = 0.8)
  d <- data.frame(y = c(a, b, c), g = factor(rep(c("A", "B", "C"), each = 10)))
  ls_register_dataset("cm_anova", d)

  r_ref <- stats::oneway.test(y ~ g, data = d, var.equal = FALSE)
  ids <- ls_new_one_way_anova("cm_anova", response = "y", group = "g", conf_level = 0.95)
  result <- LinkEDA:::.rls_compare_means_record(ids[[1L]])

  expect_equal(result$analysis_type, "one_way_anova")
  expect_equal(result$analysis_backend, "ordinary")
  expect_equal(result$test_results$statistic, unname(r_ref$statistic), tolerance = 1e-10)
  expect_equal(result$test_results$parameter[["df1"]], unname(r_ref$parameter[[1L]]), tolerance = 1e-10)
  expect_equal(result$test_results$parameter[["df2"]], unname(r_ref$parameter[[2L]]), tolerance = 1e-8)
  expect_equal(result$test_results$p_value, r_ref$p.value, tolerance = 1e-10)
  expect_equal(unname(result$descriptives$n), c(10, 10, 10))
  expect_equal(length(result$rows_used_original_ids), 30L)
})

test_that("Kruskal-Wallis is calculated by stats::kruskal.test", {
  d <- data.frame(y = c(1, 2, 3, 4, 4, 5, 7, 8, 8, 9, 10, 12),
                  g = factor(rep(c("A", "B", "C"), each = 4)))
  ls_register_dataset("cm_kruskal", d)
  reference <- stats::kruskal.test(d$y, d$g)
  id <- ls_new_one_way_anova(
    "cm_kruskal", "y", "g", method = "kruskal_wallis", p_adjust = "none"
  )
  result <- LinkEDA:::.rls_compare_means_record(id[[1L]])
  expect_equal(result$test_results$method, "Kruskal-Wallis test")
  expect_equal(result$test_results$statistic, unname(reference$statistic), tolerance = 1e-12)
  expect_equal(result$test_results$parameter[[1L]], unname(reference$parameter), tolerance = 1e-12)
  expect_equal(result$test_results$p_value, reference$p.value, tolerance = 1e-12)
  expect_true(is.finite(result$effect_sizes$epsilon_squared))
})

test_that("Row ID preservation across all analysis types", {
  d <- data.frame(id = 101:130, y = rnorm(30), g = factor(rep(c("X", "Y"), 15)))
  ls_register_dataset("cm_rowids", d)
  ids1 <- ls_new_one_sample_t_test("cm_rowids", response = "y")
  r1 <- LinkEDA:::.rls_compare_means_record(ids1[[1L]])
  expect_true(all(r1$rows_used_original_ids %in% 1:30))

  ids2 <- ls_new_independent_samples_t_test("cm_rowids", response = "y", group = "g")
  r2 <- LinkEDA:::.rls_compare_means_record(ids2[[1L]])
  expect_true(all(r2$rows_used_original_ids %in% 1:30))
})

test_that("Multiple responses produce multiple results", {
  d <- data.frame(y1 = rnorm(20), y2 = rnorm(20), y3 = rnorm(20))
  ls_register_dataset("cm_multi", d)
  ids <- ls_new_one_sample_t_test("cm_multi", response = c("y1", "y2", "y3"))
  expect_length(ids, 3L)
  for (id in ids) {
    r <- LinkEDA:::.rls_compare_means_record(id)
    expect_equal(r$analysis_type, "one_sample_t_test")
  }
})

test_that("Native payload is structured correctly", {
  d <- data.frame(y = rnorm(15))
  ls_register_dataset("cm_payload", d)
  ids <- ls_new_one_sample_t_test("cm_payload", response = "y")
  result <- LinkEDA:::.rls_compare_means_record(ids[[1L]])
  payload <- LinkEDA:::.rls_compare_means_native_payload(result)

  expect_match(payload[[1L]], "COMPARE_MEANS_OPEN")
  expect_true(any(grepl("One-Sample Tests", payload)))
  expect_true(any(grepl("Mean", payload)))
  expect_true(any(grepl("SD", payload)))
  expect_true(any(grepl("Cohen", payload)))
  expect_true("COMPARE_MEANS_V2" %in% payload)
  marker <- match("COMPARE_MEANS_V2", payload)
  expect_true(is.finite(suppressWarnings(as.numeric(payload[[marker + 1L]]))))
  expect_true(is.finite(suppressWarnings(as.numeric(payload[[marker + 2L]]))))
  expect_true(any(grepl("[.][0-9]{6,}", payload)))
})

test_that("Native payload for ANOVA contains group names", {
  d <- data.frame(y = c(rnorm(5, 0), rnorm(5, 1), rnorm(5, 2)),
                  g = factor(rep(c("low", "med", "high"), each = 5)))
  ls_register_dataset("cm_anova_payload", d)
  ids <- ls_new_one_way_anova("cm_anova_payload", response = "y", group = "g")
  result <- LinkEDA:::.rls_compare_means_record(ids[[1L]])
  payload <- LinkEDA:::.rls_compare_means_native_payload(result)

  expect_match(payload[[1L]], "COMPARE_MEANS_OPEN")
  expect_true(any(grepl("One-Way ANOVA", payload)))
  expect_true(any(grepl("low|med|high", payload)))
})

test_that("No silent fallback to single imputation - MI datasets are detected", {
  d <- data.frame(y = c(1, 2, NA, 4, 5, NA, 7, 8, 9, 10))
  ls_register_dataset("cm_mi_test", d)
  backend <- LinkEDA:::.rls_analysis_backend(LinkEDA:::.rls_dataset_record("cm_mi_test"), "one_sample_t_test")
  expect_equal(backend, "ordinary")
})

test_that("MI one-sample batches retain case descriptives and mice metadata", {
  age_x <- seq(11.2, 16.2, length.out = 169)
  completed <- replicate(20L, data.frame(age_x = age_x), simplify = FALSE)
  dataset <- list(
    dataset_id = "mi_age_probe",
    group = "mi_age_probe",
    dataset_type = "multiple_imputation",
    data = completed[[1L]],
    completed_datasets = completed,
    imputation_count = 20L,
    original_row_ids = seq_along(age_x)
  )

  result <- LinkEDA:::.rls_one_sample_t_test_mi(
    list(selected_rows = integer()), dataset, "age_x", 5,
    "two.sided", 0.95, "all", seq_along(age_x)
  )

  expect_equal(result$analysis_backend, "multiple_imputation")
  expect_equal(result$descriptives$n, 169L)
  expect_equal(result$descriptives$mean, mean(age_x), tolerance = 1e-12)
  expect_equal(result$descriptives$se,
               stats::sd(age_x) / sqrt(length(age_x)), tolerance = 1e-12)
  expect_equal(result$descriptives$n_imputations, 20L)
  expect_true(is.finite(result$test_results$parameter))

  # LinkEDA delegates the zero-between-imputation-variance boundary to mice.
  expected_df <- mice::pool.scalar(
    rep(mean(age_x) - 5, 20L),
    rep(stats::var(age_x) / length(age_x), 20L),
    n = 169,
    k = 1
  )$df
  expect_equal(result$test_results$parameter, expected_df, tolerance = 1e-10)

  payload <- LinkEDA:::.rls_compare_means_batch_native_payload(list(result), "mi_probe")
  expect_identical(payload[[2L]], "COMPARE_MEANS_BATCH_V4")
  expect_identical(payload[[15L]], "multiple_imputation")
  expect_identical(payload[[16L]], "20")
  expect_identical(payload[[17L]], "mice::pool.scalar (Rubin's rules)")
  response_at <- match("age_x", payload)
  expect_true(is.finite(response_at))
  values_at <- response_at + 8L
  expect_equal(as.numeric(payload[[values_at]]), 169)
  expect_equal(as.numeric(payload[[values_at + 1L]]), mean(age_x), tolerance = 1e-12)
  expect_true(is.finite(as.numeric(payload[[values_at + 14L]])))
})

test_that("MI paired tests retain N and pooled pair descriptives", {
  completed <- lapply(seq_len(5L), function(i) {
    data.frame(
      first = c(10, 12, 14, 16, 18, 20) + i / 10,
      second = c(8, 11, 13, 15, 15, 19) - i / 20
    )
  })
  dataset <- list(
    dataset_id = "mi_paired_probe",
    group = "mi_paired_probe",
    dataset_type = "multiple_imputation",
    data = completed[[1L]],
    completed_datasets = completed,
    imputation_count = 5L,
    original_row_ids = seq_len(6L)
  )

  result <- LinkEDA:::.rls_paired_samples_t_test_mi(
    list(selected_rows = integer()), dataset, "first", "second",
    "two.sided", 0.95, "all", seq_len(6L)
  )

  first_means <- vapply(completed, function(d) mean(d$first), numeric(1L))
  first_mean_variances <- vapply(
    completed, function(d) stats::var(d$first) / nrow(d), numeric(1L)
  )
  second_means <- vapply(completed, function(d) mean(d$second), numeric(1L))
  second_mean_variances <- vapply(
    completed, function(d) stats::var(d$second) / nrow(d), numeric(1L)
  )
  expected_first <- mice::pool.scalar(
    first_means, first_mean_variances, n = 6L, k = 1L
  )
  expected_second <- mice::pool.scalar(
    second_means, second_mean_variances, n = 6L, k = 1L
  )
  expected_sd_difference <- sqrt(mean(vapply(
    completed, function(d) stats::var(d$first - d$second), numeric(1L)
  )))

  expect_identical(result$analysis_backend, "multiple_imputation")
  expect_equal(result$descriptives$n, c(6, 6))
  expect_equal(result$descriptives$differences$n, 6)
  expect_equal(result$descriptives$mean,
               c(expected_first$qbar, expected_second$qbar), tolerance = 1e-12)
  expect_equal(result$descriptives$differences$sd,
               expected_sd_difference, tolerance = 1e-12)
  expect_equal(result$descriptives$differences$se,
               result$test_results$mean_diff_se, tolerance = 1e-12)
  expect_match(result$descriptives$note, "mice::pool.scalar", fixed = TRUE)

  payload <- LinkEDA:::.rls_compare_means_batch_native_payload(
    list(result), "mi_paired_probe"
  )
  response_at <- match("first", payload)
  expect_true(is.finite(response_at))
  values_at <- response_at + 8L
  expect_equal(as.numeric(payload[[values_at]]), 6)
  expect_equal(as.numeric(payload[[values_at + 1L]]),
               expected_first$qbar, tolerance = 1e-12)
  expect_equal(as.numeric(payload[[values_at + 5L]]),
               expected_second$qbar, tolerance = 1e-12)
  expect_equal(as.numeric(payload[[values_at + 9L]]),
               expected_sd_difference, tolerance = 1e-12)
  expect_true(is.finite(as.numeric(payload[[values_at + 10L]])))
})

test_that("MI independent tests honor displayed group order and pool descriptives", {
  finland <- c(10, 12, 14, 16)
  greece <- c(20, 22, 24, 26)
  completed <- lapply(seq_len(20L), function(i) {
    data.frame(
      age = c(finland, greece),
      country = factor(rep(c("Finland", "Greece"), each = 4L),
                       levels = c("Finland", "Greece"))
    )
  })
  dataset <- list(
    dataset_id = "mi_country_probe",
    group = "mi_country_probe",
    dataset_type = "multiple_imputation",
    data = completed[[1L]],
    completed_datasets = completed,
    imputation_count = 20L,
    original_row_ids = seq_len(8L)
  )

  result <- LinkEDA:::.rls_independent_samples_t_test_mi(
    list(selected_rows = integer()), dataset, "age", "country",
    c("Greece", "Finland"), "two.sided", 0.95, FALSE, "all",
    seq_len(8L)
  )

  expect_identical(result$specification$group_levels, c("Greece", "Finland"))
  expect_identical(result$descriptives$group, c("Greece", "Finland"))
  expect_equal(result$descriptives$n, c(4, 4))
  expect_equal(result$descriptives$mean, c(mean(greece), mean(finland)),
               tolerance = 1e-12)
  expect_equal(result$test_results$mean_diff,
               mean(greece) - mean(finland), tolerance = 1e-12)
  expect_identical(result$test_results$group_reference, "Finland")
  expect_identical(result$test_results$group_comparison, "Greece")
  expect_match(result$test_results$direction_note, "Greece - Finland",
               fixed = TRUE)
  expect_equal(result$multiple_imputation$m, 20L)
  expect_identical(result$test_results$pooling_method, "mice::pool.scalar (Rubin's rules)")
  expect_true(is.finite(result$test_results$parameter))
})

test_that("MI two-sample scope uses the same selected subject ids in every imputation", {
  completed <- lapply(seq_len(5L), function(i) {
    data.frame(
      y = c(5, 6, 50 + i, 3, 4, -30 - i, 100, 101),
      g = factor(rep(c("Control", "Treatment", "Waitlist"), c(3L, 3L, 2L)),
                 levels = c("Control", "Treatment", "Waitlist"))
    )
  })
  dataset <- list(
    dataset_id = "mi_selected_pair_probe",
    group = "mi_selected_pair_probe",
    dataset_type = "multiple_imputation",
    data = completed[[1L]],
    completed_datasets = completed,
    imputation_count = 5L,
    original_row_ids = seq_len(8L)
  )
  selected_ids <- c(1L, 2L, 4L, 5L)

  forward <- LinkEDA:::.rls_independent_samples_t_test_mi(
    list(selected_rows = selected_ids), dataset, "y", "g",
    c("Control", "Treatment"), "two.sided", 0.95, FALSE,
    "selected", seq_len(8L)
  )
  reverse <- LinkEDA:::.rls_independent_samples_t_test_mi(
    list(selected_rows = selected_ids), dataset, "y", "g",
    c("Treatment", "Control"), "two.sided", 0.95, FALSE,
    "selected", seq_len(8L)
  )

  expect_equal(forward$descriptives$n, c(2, 2))
  expect_identical(sort(forward$rows_used_original_ids), selected_ids)
  expect_equal(forward$test_results$mean_diff, 2, tolerance = 1e-12)
  expect_equal(reverse$test_results$mean_diff, -2, tolerance = 1e-12)
  expect_equal(reverse$test_results$mean_diff_se,
               forward$test_results$mean_diff_se, tolerance = 1e-12)
  expect_equal(reverse$test_results$statistic,
               -forward$test_results$statistic, tolerance = 1e-12)
  expect_equal(as.numeric(reverse$test_results$conf_int),
               as.numeric(-rev(forward$test_results$conf_int)), tolerance = 1e-12)
  expect_equal(reverse$test_results$p_value,
               forward$test_results$p_value, tolerance = 1e-12)
  expect_equal(forward$multiple_imputation$m, 5L)
  expect_equal(reverse$multiple_imputation$m, 5L)
})

test_that("Validation errors for invalid inputs", {
  d <- data.frame(y = 1:5, g = factor(c("a", "a", "b", "b", "b")), cat = letters[1:5])
  ls_register_dataset("cm_validation", d)
  expect_error(ls_new_one_sample_t_test("cm_validation", response = "cat"), "must be numeric")
  expect_error(ls_new_independent_samples_t_test("cm_validation", response = "y", group = "nonexistent"), "not found")
  expect_error(ls_new_paired_samples_t_test("cm_validation", pairs = list(c("y", "nonexistent"))), "not found")
  expect_error(ls_new_one_way_anova("cm_validation", response = "y", group = "nonexistent"), "not found")
})

test_that("two-group validation separates categorical semantics from level count", {
  d <- data.frame(
    y = seq_len(12),
    planet = rep(c("Aurelia", "Borealis", "Cygnus"), 4),
    stringsAsFactors = FALSE
  )
  name <- LinkEDA:::.rls_register_dataset(
    "cm_imported_planet", d, replace = TRUE, infer_imported_types = TRUE
  )
  on.exit(ls_unregister_dataset(name), add = TRUE)

  metadata <- ls_variable_metadata(name, "planet")
  expect_equal(metadata$current_analysis_type, "factor")
  expect_error(
    ls_new_independent_samples_t_test(name, response = "y", group = "planet"),
    "must have exactly two categories, but has 3",
    fixed = TRUE
  )

  ls_set_variable_type(name, "planet", "character")
  expect_error(
    ls_new_independent_samples_t_test(name, response = "y", group = "planet"),
    "is Text",
    fixed = TRUE
  )
  ls_set_variable_type(name, "planet", "factor")
  expect_equal(levels(LinkEDA:::.rls_dataset_record(name)$data$planet),
               c("Aurelia", "Borealis", "Cygnus"))
  expect_error(
    ls_new_independent_samples_t_test(name, response = "y", group = "planet"),
    "must have exactly two categories, but has 3",
    fixed = TRUE
  )
})

test_that("Result schema matches specification", {
  d <- data.frame(y = rnorm(25))
  ls_register_dataset("cm_schema", d)
  ids <- ls_new_one_sample_t_test("cm_schema", response = "y")
  result <- LinkEDA:::.rls_compare_means_record(ids[[1L]])

  mandatory <- c("result_id", "dataset_id", "analysis_type", "analysis_backend",
                 "specification", "descriptives", "test_results", "effect_sizes",
                 "post_hoc", "rows_used_original_ids", "rows_excluded_original_ids",
                 "multiple_imputation", "result_version", "created_at")
  for (field in mandatory) {
    expect_true(field %in% names(result), info = sprintf("Missing field: %s", field))
  }
  expect_equal(result$result_version, 1L)
  expect_s3_class(result$created_at, "POSIXct")
})

test_that("No regression in existing Table 1 functionality", {
  d <- data.frame(group = factor(rep(c("a", "b"), each = 5)),
                  score = c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.1))
  ls_register_dataset("cm_regression_t1", d)
  table <- ls_new_table1("cm_regression_t1", variables = "score", group = "group")
  display <- ls_table1_table(table)
  expect_true("p" %in% names(display))
  expect_true("Test" %in% names(display))
  expect_match(display$Test[display$Variable == "score"][[1L]], "Welch")
})

test_that("No regression in existing GLM functionality", {
  d <- data.frame(y = rnorm(20), x = rnorm(20))
  ls_register_dataset("cm_regression_glm", d)
  model <- ls_new_glm("cm_regression_glm")
  model <- ls_glm_set_dependent(model, "y")
  model <- ls_glm_add_predictor(model, "x")
  model <- ls_glm_fit(model)
  coefs <- ls_glm_coefficients(model)
  expect_true(nrow(coefs) >= 2L)
  expect_true("(Intercept)" %in% coefs$term)
})

test_that("Paired t test direction note is correct", {
  d <- data.frame(a = rnorm(15, 5), b = rnorm(15, 4.5))
  ls_register_dataset("cm_dir_paired", d)
  ids <- ls_new_paired_samples_t_test("cm_dir_paired", pairs = list(c("a", "b")))
  result <- LinkEDA:::.rls_compare_means_record(ids[[1L]])
  expect_equal(result$specification$difference, "a - b")
  expect_match(result$test_results$direction_note, "a - b")
})

test_that("Independent t test effect size interval comes from effectsize", {
  d <- data.frame(y = c(rnorm(10, 0), rnorm(10, 1)),
                  g = factor(rep(c("ctrl", "trt"), each = 10)))
  ls_register_dataset("cm_es_ind", d)
  ids <- ls_new_independent_samples_t_test("cm_es_ind", response = "y", group = "g")
  result <- LinkEDA:::.rls_compare_means_record(ids[[1L]])
  expect_true(is.finite(result$effect_sizes$cohens_d))
  reference <- effectsize::hedges_g(d$y[d$g == "ctrl"], d$y[d$g == "trt"], verbose = FALSE)
  expect_equal(result$effect_sizes$effect_size_ci, c(reference$CI_low, reference$CI_high))
  expect_true(is.na(result$effect_sizes$cohens_d_se))
})

test_that("One-way ANOVA with missing values preserves row IDs", {
  d <- data.frame(y = c(1, 2, NA, 4, 5, 6, NA, 8, 9, 10),
                  g = factor(rep(c("A", "B", "C"), length.out = 10)))
  ls_register_dataset("cm_anova_na", d)
  ids <- ls_new_one_way_anova("cm_anova_na", response = "y", group = "g")
  result <- LinkEDA:::.rls_compare_means_record(ids[[1L]])
  expect_true(length(result$rows_used_original_ids) >= 6L)
  expect_true(8 %in% result$rows_excluded_original_ids || 7 %in% result$rows_excluded_original_ids)
})

test_that("All analysis types produce unique result IDs", {
  d <- data.frame(y = rnorm(20), y2 = rnorm(20), g = factor(rep(c("A", "B"), 10)))
  ls_register_dataset("cm_unique_ids", d)
  ids1 <- ls_new_one_sample_t_test("cm_unique_ids", response = "y")
  ids2 <- ls_new_independent_samples_t_test("cm_unique_ids", response = "y", group = "g")
  ids3 <- ls_new_paired_samples_t_test("cm_unique_ids", pairs = list(c("y", "y2")))
  all_ids <- c(ids1, ids2, ids3)
  expect_equal(length(all_ids), length(unique(all_ids)))
})

test_that("mtcars one-sample and paired confidence intervals match R", {
  ls_register_dataset("cm_mtcars", mtcars)
  one_id <- ls_new_one_sample_t_test("cm_mtcars", response = "mpg", mu = 0)
  one <- LinkEDA:::.rls_compare_means_record(one_id[[1L]])
  one_ref <- stats::t.test(mtcars$mpg, mu = 0)
  expect_equal(one$test_results$conf_int, unname(one_ref$conf.int), tolerance = 1e-12)
  expect_equal(one$test_results$mean_diff_se, stats::sd(mtcars$mpg) / sqrt(nrow(mtcars)), tolerance = 1e-12)

  pair_id <- ls_new_paired_samples_t_test("cm_mtcars", pairs = list(c("mpg", "cyl")))
  pair <- LinkEDA:::.rls_compare_means_record(pair_id[[1L]])
  pair_ref <- stats::t.test(mtcars$mpg, mtcars$cyl, paired = TRUE)
  expect_equal(pair$test_results$conf_int, unname(pair_ref$conf.int), tolerance = 1e-12)
  expect_equal(pair$test_results$mean_diff_se, stats::sd(mtcars$mpg - mtcars$cyl) / sqrt(nrow(mtcars)), tolerance = 1e-12)
})

test_that("batch payload keeps multiple responses and separate numeric CI bounds", {
  d <- data.frame(y1 = rnorm(18), y2 = rnorm(18, 1))
  ls_register_dataset("cm_batch", d)
  ids <- ls_new_one_sample_t_test("cm_batch", response = c("y1", "y2"), mu = 0.25)
  results <- lapply(ids, LinkEDA:::.rls_compare_means_record)
  payload <- LinkEDA:::.rls_compare_means_batch_native_payload(results, "batch_test")
  expect_identical(payload[[1L]], "COMPARE_MEANS_BATCH_OPEN")
  expect_identical(payload[[2L]], "COMPARE_MEANS_BATCH_V2")
  expect_true("y1" %in% payload && "y2" %in% payload)
  expect_identical(payload[[14L]], "holm")
  expect_identical(payload[[15L]], "2")
  expect_false(any(grepl("^\\[.*,.+\\]$", payload)))
})

test_that("Holm, Bonferroni, and None adjustments use full-precision R p values", {
  d <- data.frame(a = c(1:12), b = c(2:13) + c(rep(0, 6), rep(1, 6)),
                  c = c(4:15) + rep(c(0, 2), 6))
  ls_register_dataset("cm_adjust", d)
  ids <- ls_new_one_sample_t_test("cm_adjust", c("a", "b", "c"), p_adjust = "holm")
  results <- lapply(ids, LinkEDA:::.rls_compare_means_record)
  p <- vapply(results, function(x) x$test_results$p_value, numeric(1L))
  expect_equal(vapply(results, function(x) x$test_results$p_adjusted, numeric(1L)),
               stats::p.adjust(p, method = "holm"), tolerance = 1e-15)
  expect_true(all(vapply(results, function(x) x$test_results$adjustment_family_size, integer(1L)) == 3L))

  bonf <- lapply(ls_new_one_sample_t_test("cm_adjust", c("a", "b", "c"), p_adjust = "bonferroni"),
                 LinkEDA:::.rls_compare_means_record)
  expect_equal(vapply(bonf, function(x) x$test_results$p_adjusted, numeric(1L)),
               stats::p.adjust(p, method = "bonferroni"), tolerance = 1e-15)
  none <- lapply(ls_new_one_sample_t_test("cm_adjust", c("a", "b", "c"), p_adjust = "none"),
                 LinkEDA:::.rls_compare_means_record)
  expect_equal(vapply(none, function(x) x$test_results$p_adjusted, numeric(1L)), p, tolerance = 1e-15)
})

test_that("a non-estimable row is retained but excluded from the adjustment family", {
  d <- data.frame(valid = c(1, 2, 4, 5, 7), constant = rep(3, 5))
  ls_register_dataset("cm_partial", d)
  ids <- ls_new_one_sample_t_test("cm_partial", c("valid", "constant"), p_adjust = "holm")
  expect_length(ids, 2L)
  valid <- LinkEDA:::.rls_compare_means_record(ids[[1L]])
  failed <- LinkEDA:::.rls_compare_means_record(ids[[2L]])
  expect_true(is.finite(valid$test_results$p_adjusted))
  expect_equal(valid$test_results$adjustment_family_size, 1L)
  expect_true(is.na(failed$test_results$p_value))
  expect_true(is.na(failed$test_results$p_adjusted))
  expect_equal(failed$test_results$adjustment_family_size, 1L)
  expect_match(failed$warnings[[1L]], "variance is zero", ignore.case = TRUE)

  payload <- LinkEDA:::.rls_compare_means_batch_native_payload(list(valid, failed), "cm_partial_run")
  expect_true(any(grepl("variance is zero", payload, ignore.case = TRUE)))
})

test_that("one-sided p values are adjusted without conversion to two-sided values", {
  d <- data.frame(a = c(3, 4, 5, 8, 9), b = c(2, 3, 3, 5, 7))
  ls_register_dataset("cm_adjust_one_sided", d)
  ids <- ls_new_one_sample_t_test(
    "cm_adjust_one_sided", c("a", "b"), mu = 1,
    alternative = "greater", p_adjust = "holm"
  )
  records <- lapply(ids, LinkEDA:::.rls_compare_means_record)
  raw <- c(
    stats::t.test(d$a, mu = 1, alternative = "greater")$p.value,
    stats::t.test(d$b, mu = 1, alternative = "greater")$p.value
  )
  expect_equal(vapply(records, function(x) x$test_results$p_value, numeric(1L)), raw)
  expect_equal(vapply(records, function(x) x$test_results$p_adjusted, numeric(1L)),
               stats::p.adjust(raw, method = "holm"))
})

test_that("independent tests report Hedges g and honor group order", {
  d <- data.frame(y = c(1, 2, 4, 5, 8, 9, 10, 13), g = factor(rep(c("A", "B"), each = 4)))
  ls_register_dataset("cm_hedges", d)
  id <- ls_new_independent_samples_t_test("cm_hedges", "y", "g", group_order = c("B", "A"))
  result <- LinkEDA:::.rls_compare_means_record(id[[1L]])
  expect_equal(result$specification$group_levels, c("B", "A"))
  expect_true(is.finite(result$effect_sizes$hedges_g))
  expect_gt(result$test_results$mean_diff, 0)
})

test_that("classical and Welch one-way ANOVA are explicit R methods", {
  d <- data.frame(y = c(1, 2, 4, 5, 7, 11, 10, 12, 13), g = factor(rep(c("A", "B", "C"), each = 3)))
  ls_register_dataset("cm_anova_methods", d)
  welch <- LinkEDA:::.rls_compare_means_record(ls_new_one_way_anova("cm_anova_methods", "y", "g", method = "welch")[[1L]])
  classical <- LinkEDA:::.rls_compare_means_record(ls_new_one_way_anova("cm_anova_methods", "y", "g", method = "classical")[[1L]])
  expect_match(welch$test_results$method, "Welch")
  expect_match(classical$test_results$method, "Classical")
  expect_equal(classical$test_results$parameter[[2L]], nrow(d) - nlevels(d$g))
  expect_true(is.finite(classical$effect_sizes$omega_squared))
})

test_that("invalid zero-variance and identical-pair specifications are rejected", {
  d <- data.frame(a = 1:6, b = 1:6, constant = rep(2, 6))
  ls_register_dataset("cm_invalid_new", d)
  expect_error(ls_new_one_sample_t_test("cm_invalid_new", "constant"), "variance is zero")
  expect_error(ls_new_paired_samples_t_test("cm_invalid_new", list(c("a", "a"))), "two different")
})

test_that("multiple responses and pairs retain analysis-specific N under missingness", {
  d <- data.frame(
    y1 = c(1, 2, 3, 4, 5, 6, NA, NA),
    y2 = c(2, 4, 4, 7, 6, 9, 8, 11),
    y3 = c(1, NA, 2, NA, 4, 5, 7, 8),
    g = factor(rep(c("A", "B"), each = 4), levels = c("A", "B", "unused"))
  )
  ls_register_dataset("cm_multi_missing", d)
  one_ids <- ls_new_one_sample_t_test("cm_multi_missing", c("y1", "y2"))
  expect_equal(unname(vapply(one_ids, function(id) LinkEDA:::.rls_compare_means_record(id)$descriptives$n, numeric(1L))), c(6, 8))
  pair_ids <- ls_new_paired_samples_t_test("cm_multi_missing", list(c("y1", "y2"), c("y2", "y3")))
  expect_equal(unname(vapply(pair_ids, function(id) LinkEDA:::.rls_compare_means_record(id)$descriptives$differences$n, numeric(1L))), c(6, 6))
  independent_ids <- ls_new_independent_samples_t_test("cm_multi_missing", c("y1", "y2"), "g", var_equal = TRUE)
  expect_length(independent_ids, 2L)
  expect_true(all(vapply(independent_ids, function(id) grepl("Student", LinkEDA:::.rls_compare_means_record(id)$test_results$method), logical(1L))))
  anova_ids <- ls_new_one_way_anova("cm_multi_missing", c("y1", "y2"), "g")
  expect_length(anova_ids, 2L)
  expect_equal(LinkEDA:::.rls_compare_means_record(anova_ids[[1L]])$specification$group_levels, c("A", "B"))
})

test_that("invalid ANOVA responses do not cancel other responses", {
  data <- data.frame(y=seq_len(30)+sin(seq_len(30)),
    g=factor(rep(c("Finland","Greece","Italy"),10)),
    age_group_before=factor(rep(c("young","older"),15)))
  for (mi in c(FALSE,TRUE)) {
    id <- ls_register_dataset(paste0("anova_invalid_",mi),data)
    if(mi) {
      record <- LinkEDA:::.rls_dataset_record(id)
      record$dataset_type <- "multiple_imputation";record$imputation_count <- 3L
      record$original_data <- data
      record$completed_datasets <- lapply(1:3,function(i)transform(data,y=y+cos(seq_len(30)*i)))
      LinkEDA:::.rls_set_dataset_record(record)
    }
    handles <- ls_new_one_way_anova(id,c("age_group_before","y"),"g")
    results <- lapply(handles,LinkEDA:::.rls_compare_means_record)
    expect_match(paste(results[[1]]$warnings,collapse=" "),"numeric dependent variable")
    expect_true(is.finite(results[[2]]$test_results$p_value))
    expect_identical(results[[2]]$specification$response,"y")
    if(!mi) expect_equal(results[[2]]$test_results$p_value,stats::oneway.test(y~g,data=data)$p.value)
    expect_error(ls_new_one_way_anova(id,"age_group_before","g"),"numeric dependent variable")
  }
})
