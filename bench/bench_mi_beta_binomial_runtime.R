# Representative multiple-imputation generalized-model benchmark.
# Usage: Rscript bench/bench_mi_beta_binomial_runtime.R [workers] [output.csv]

suppressPackageStartupMessages(library(LinkEDA))

args <- commandArgs(trailingOnly = TRUE)
workers <- if (length(args)) as.integer(args[[1L]]) else 1L
output <- if (length(args) >= 2L) args[[2L]] else ""
if (!is.finite(workers) || workers < 1L) stop("workers must be a positive integer")
for (package in c("mice", "glmmTMB", "emmeans")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(sprintf("The benchmark requires `%s`.", package))
  }
}

set.seed(20260921)
n <- 600L
m <- 50L
x <- stats::rnorm(n)
z <- stats::runif(n, -1, 1)
g <- factor(rep(c("Control", "Treatment", "Alternative"), length.out = n),
  levels = c("Control", "Treatment", "Alternative"))
eta <- -.6 + .5 * x - .25 * z + c(
  Control = 0, Treatment = .35, Alternative = -.2
)[as.character(g)] + x * c(
  Control = 0, Treatment = .2, Alternative = -.12
)[as.character(g)]
mean_probability <- stats::plogis(eta)
latent_probability <- stats::rbeta(
  n, 7 * mean_probability, 7 * (1 - mean_probability)
)
successes <- stats::rbinom(n, 24L, latent_probability)
completed <- lapply(seq_len(m), function(imputation) data.frame(
  successes = successes, x = x + stats::rnorm(n, sd = .012),
  z = z + stats::rnorm(n, sd = .008), g = g
))
original <- completed[[1L]]
original$x[seq(4L, n, by = 17L)] <- NA_real_
original$z[seq(9L, n, by = 23L)] <- NA_real_

id <- ls_register_dataset("benchmark_mi_beta_binomial", completed[[1L]])
on.exit(ls_unregister_dataset(id), add = TRUE)
dataset <- LinkEDA:::.rls_dataset_record(id)
dataset$dataset_type <- "multiple_imputation"
dataset$imputation_id <- "benchmark_mi_beta_binomial_imp"
dataset$imputation_count <- m
dataset$completed_datasets <- completed
dataset$original_data <- original
dataset$original_row_ids <- seq_len(n)
dataset$missing_cell_mask <- as.data.frame(lapply(original, is.na),
  stringsAsFactors = FALSE)
LinkEDA:::.rls_set_dataset_record(dataset)

gc(reset = TRUE)
wall_started <- proc.time()[["elapsed"]]
model <- withr::with_options(list(LinkEDA.mi_workers = workers),
  suppressWarnings(ls_new_count_regression(
    id, "successes", c("x", "z", "g", "x:g"),
    distribution = "beta_binomial", trials = 24L, native = FALSE,
    name = paste0("benchmark_mi_beta_binomial_", workers)
  )))
model_wall <- unname(proc.time()[["elapsed"]] - wall_started)
state <- ls_count_regression_state(model)

pairwise_started <- proc.time()[["elapsed"]]
pairwise <- ls_count_regression_pairwise(model, "g")
pairwise_wall <- unname(proc.time()[["elapsed"]] - pairwise_started)

interaction_started <- proc.time()[["elapsed"]]
interaction <- ls_count_regression_interaction(model, "x:g")
interaction_wall <- unname(proc.time()[["elapsed"]] - interaction_started)

fit <- state$execution_diagnostics
pairwise_timing <- attr(pairwise, "execution_diagnostics")
interaction_timing <- interaction$execution_diagnostics
fit_times <- fit$fits$seconds
memory <- gc()
row <- data.frame(
  timestamp = format(Sys.time(), tz = "UTC", usetz = TRUE),
  R_version = paste(R.version$major, R.version$minor, sep = "."),
  LinkEDA_version = as.character(utils::packageVersion("LinkEDA")),
  glmmTMB_version = as.character(utils::packageVersion("glmmTMB")),
  imputations = m, rows = n, requested_workers = workers,
  used_workers = fit$workers,
  model_wall_seconds = model_wall,
  preparation_seconds = fit$preparation_seconds,
  worker_startup_seconds = fit$worker_startup_seconds,
  worker_fit_diagnostic_phase_seconds = fit$worker_fit_and_diagnostics_seconds,
  imputation_fit_work_seconds = fit$imputation_fit_work_seconds,
  diagnostic_work_seconds = fit$diagnostic_work_seconds,
  result_collection_seconds = fit$result_collection_seconds,
  pooling_seconds = fit$pooling_seconds,
  global_tests_seconds = fit$global_tests_seconds,
  formatting_seconds = fit$formatting_seconds,
  mean_imputation_fit_seconds = mean(fit_times),
  median_imputation_fit_seconds = stats::median(fit_times),
  slowest_imputation_fit_seconds = max(fit_times),
  pairwise_wall_seconds = pairwise_wall,
  pairwise_emmeans_seconds = pairwise_timing$emmeans_seconds,
  pairwise_contrasts_seconds = pairwise_timing$pairwise_contrasts_seconds,
  interaction_wall_seconds = interaction_wall,
  interaction_emmeans_seconds = interaction_timing$emmeans_seconds,
  interaction_pairwise_seconds = interaction_timing$pairwise_contrasts_seconds,
  interaction_contrasts_seconds = interaction_timing$interaction_contrasts_seconds,
  max_used_vector_heap_mb = memory[2L, ncol(memory)],
  stringsAsFactors = FALSE
)
if (nzchar(output)) {
  utils::write.table(row, output, sep = ",", row.names = FALSE,
    col.names = !file.exists(output), append = file.exists(output))
}
print(row, row.names = FALSE)
