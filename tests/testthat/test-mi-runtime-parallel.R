.runtime_tasks <- function(n, delays = rep(0, n)) {
  lapply(seq_len(n), function(index) list(
    index = index, value = index, delay = delays[[index]]
  ))
}

.runtime_fit <- function(task) {
  if (task$delay > 0) Sys.sleep(task$delay)
  if (isTRUE(task$warn)) warning("fit warning ", task$index)
  if (isTRUE(task$fail)) stop("fit failure ", task$index)
  task$value * 2
}

test_that("worker policy is conservative and configurable", {
  withr::local_options(list(LinkEDA.mi_parallel = TRUE, LinkEDA.mi_workers = NULL))
  withr::local_envvar(c(LINKEDA_MI_WORKERS = ""))
  expect_lte(LinkEDA:::.rls_mi_worker_count(50), 4L)
  expect_equal(LinkEDA:::.rls_mi_worker_count(2, workers = 8), 2L)
  expect_equal(LinkEDA:::.rls_mi_worker_count(8, workers = 1), 1L)
  expect_equal(LinkEDA:::.rls_mi_worker_count(8, workers = 4, parallel = FALSE), 1L)
  withr::local_envvar(c(LINKEDA_MI_WORKERS = "2"))
  expect_equal(LinkEDA:::.rls_mi_worker_count(8), 2L)
})

test_that("parallel completion order never changes imputation order", {
  events <- list()
  callback <- function(...) events[[length(events) + 1L]] <<- list(...)
  tasks <- .runtime_tasks(4, c(.20, .01, .10, .02))
  result <- LinkEDA:::.rls_mi_fit_imputations(
    tasks, .runtime_fit, workers = 2, progress = callback,
    heartbeat_seconds = .05
  )
  expect_equal(vapply(result$results, `[[`, integer(1), "index"), 1:4)
  expect_equal(vapply(result$results, `[[`, numeric(1), "fit"), 2 * (1:4))
  completions <- vapply(Filter(function(x) !is.null(x$result), events),
    function(x) x$result$index, integer(1))
  expect_false(identical(completions, 1:4))
  expect_equal(result$workers, 2L)
  expect_true(all(result$worker_pids > 0L))
})

test_that("parallel fits execute in distinct R processes", {
  fit_pid <- function(task) {
    Sys.sleep(.15)
    Sys.getpid()
  }
  result <- LinkEDA:::.rls_mi_fit_imputations(
    .runtime_tasks(4), fit_pid, workers = 2
  )
  fitted_pids <- vapply(result$results, `[[`, integer(1L), "fit")
  expect_setequal(unique(fitted_pids), result$worker_pids)
  expect_length(unique(fitted_pids), 2L)
  expect_false(Sys.getpid() %in% fitted_pids)
})

test_that("TMB build notices do not masquerade as fit warnings", {
  fitted <- LinkEDA:::.rls_mi_run_fit_task(
    list(index = 1L),
    function(task) {
      warning(paste("package version mismatch: glmmTMB was built with TMB",
                    "package version 1.9.21; current TMB package version is 1.9.25"))
      warning("optimizer warning")
      1
    }, list()
  )
  expect_equal(fitted$warnings, "optimizer warning")
  expect_length(fitted$technical_warnings, 1L)
})

test_that("warnings and fitting errors are structured and retained", {
  tasks <- .runtime_tasks(3)
  tasks[[2]]$warn <- TRUE
  tasks[[3]]$fail <- TRUE
  result <- LinkEDA:::.rls_mi_fit_imputations(tasks, .runtime_fit, workers = 2)
  expect_equal(result$results[[2]]$warnings, "fit warning 2")
  expect_null(result$results[[3]]$fit)
  expect_equal(result$results[[3]]$error, "fit failure 3")
  expect_false(result$results[[3]]$convergence)
})

test_that("worker startup failure falls back to inline sequential execution", {
  attempted <- integer()
  unavailable <- function(count, ...) {
    attempted <<- c(attempted, count)
    stop("PSOCK unavailable")
  }
  messages <- character()
  result <- LinkEDA:::.rls_mi_fit_imputations(
    .runtime_tasks(3), .runtime_fit, workers = 3,
    cluster_factory = unavailable,
    progress = function(..., message = "") messages <<- c(messages, message)
  )
  expect_equal(attempted, c(3L, 2L))
  expect_equal(result$workers, 1L)
  expect_equal(vapply(result$results, `[[`, numeric(1), "fit"), 2 * (1:3))
  expect_true(any(grepl("Falling back", messages, fixed = TRUE)))
})

test_that("a lost worker restarts the complete task with fewer workers", {
  skip_on_cran()
  crashing_fit <- local({
    main_pid <- Sys.getpid()
    function(task) {
      if (Sys.getpid() != main_pid) quit(save = "no", status = 1L)
      task$value * 2
    }
  })
  messages <- character()
  result <- LinkEDA:::.rls_mi_fit_imputations(
    .runtime_tasks(3), crashing_fit, workers = 3,
    progress = function(..., message = "") messages <<- c(messages, message)
  )
  expect_equal(result$workers, 1L)
  expect_equal(vapply(result$results, `[[`, numeric(1), "fit"), 2 * (1:3))
  expect_equal(sum(grepl("Restarting all imputations", messages,
    fixed = TRUE)), 2L)
})

test_that("cancellation stops scheduling and reports completed fits", {
  signal <- tempfile(fileext = ".cancel")
  on.exit(unlink(signal), add = TRUE)
  completed <- 0L
  callback <- function(result = NULL, ...) {
    if (!is.null(result)) {
      completed <<- completed + 1L
      if (completed == 1L) file.create(signal)
    }
  }
  condition <- tryCatch(
    LinkEDA:::.rls_mi_fit_imputations(
      .runtime_tasks(5, rep(.05, 5)), .runtime_fit, workers = 2,
      progress = callback, cancel_path = signal
    ),
    linkeda_mi_cancelled = identity
  )
  expect_s3_class(condition, "linkeda_mi_cancelled")
  expect_equal(condition$completed, 1L)
})

test_that("optional RNG streams are reproducible across worker counts", {
  random_fit <- function(task) stats::runif(3)
  withr::local_options(list(LinkEDA.mi_fit_seed = 8831L))
  sequential <- LinkEDA:::.rls_mi_fit_imputations(
    .runtime_tasks(3), random_fit, workers = 1
  )
  parallel <- LinkEDA:::.rls_mi_fit_imputations(
    .runtime_tasks(3), random_fit, workers = 2
  )
  expect_equal(lapply(sequential$results, `[[`, "fit"),
               lapply(parallel$results, `[[`, "fit"))
})

test_that("one worker is a genuine in-process sequential path", {
  factory_called <- FALSE
  result <- LinkEDA:::.rls_mi_fit_imputations(
    .runtime_tasks(3), .runtime_fit, workers = 1,
    cluster_factory = function(...) {
      factory_called <<- TRUE
      stop("must not be called")
    }
  )
  expect_false(factory_called)
  expect_equal(result$workers, 1L)
  expect_length(result$worker_pids, 0L)
  expect_equal(vapply(result$results, `[[`, numeric(1), "fit"), 2 * (1:3))
})

test_that("a failed MI task emits a terminal progress event", {
  events <- list()
  withr::local_options(list(LinkEDA.mi_progress_callback = function(event)
    events[[length(events) + 1L]] <<- event))
  expect_error(LinkEDA:::.rls_mi_fit_generalized_model_record(list(
    id = "missing-model", group = "missing-dataset", response = "y")))
  expect_length(events, 1L)
  expect_identical(events[[1L]]$phase, "failed")
  expect_identical(events[[1L]]$status, "failed")
  expect_true(nzchar(events[[1L]]$message))
})
