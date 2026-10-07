test_that("backend task replies are drained without recursive duplicate retries", {
  state <- LinkEDA:::.rls_state
  previous <- list(
    process_started = state$process_started,
    processing_backend_tasks = state$processing_backend_tasks,
    deferred_control_lines = state$deferred_control_lines,
    backend_kind = state$backend_kind
  )
  on.exit({
    state$process_started <- previous$process_started
    state$processing_backend_tasks <- previous$processing_backend_tasks
    state$deferred_control_lines <- previous$deferred_control_lines
    state$backend_kind <- previous$backend_kind
  }, add = TRUE)

  state$process_started <- TRUE
  state$processing_backend_tasks <- FALSE
  state$deferred_control_lines <- character()
  state$backend_kind <- "native"
  task <- paste(
    "GLM_NEEDED", "mtcars", "mpg", "all", "0", "0", "0", "0", "32",
    paste(seq_len(32), collapse = "\t"),
    sep = "\t"
  )
  handled <- 0L

  local_mocked_bindings(
    .rls_send = function(lines, expect_reply = TRUE) {
      if (identical(lines, "MAIN_R_TASKS")) {
        state$deferred_control_lines <- c(
          state$deferred_control_lines,
          task,
          task
        )
      }
      "OK"
    },
    .rls_process_control_lines = function(lines) {
      handled <<- handled + length(lines)
      state$deferred_control_lines <- c(state$deferred_control_lines, lines)
      invisible(NULL)
    },
    .rls_backend_connection_alive = function() TRUE,
    .package = "LinkEDA"
  )

  expect_true(LinkEDA:::.rls_process_backend_tasks())
  expect_equal(handled, 1L)
  expect_length(state$deferred_control_lines, 0L)
  expect_false(state$processing_backend_tasks)
})

test_that("one failed backend task does not stop the remaining queue", {
  state <- LinkEDA:::.rls_state
  previous <- list(
    process_started = state$process_started,
    processing_backend_tasks = state$processing_backend_tasks,
    deferred_control_lines = state$deferred_control_lines,
    backend_kind = state$backend_kind
  )
  on.exit({
    state$process_started <- previous$process_started
    state$processing_backend_tasks <- previous$processing_backend_tasks
    state$deferred_control_lines <- previous$deferred_control_lines
    state$backend_kind <- previous$backend_kind
  }, add = TRUE)

  state$process_started <- TRUE
  state$processing_backend_tasks <- FALSE
  state$deferred_control_lines <- character()
  state$backend_kind <- "native"
  handled <- character()

  local_mocked_bindings(
    .rls_send = function(lines, expect_reply = TRUE) {
      state$deferred_control_lines <- c(state$deferred_control_lines, "bad", "good")
      "OK"
    },
    .rls_process_control_lines = function(lines) {
      handled <<- c(handled, lines)
      if (identical(lines, "bad")) stop("deliberate test failure")
      invisible(NULL)
    },
    .rls_backend_connection_alive = function() TRUE,
    .package = "LinkEDA"
  )

  expect_warning(
    expect_true(LinkEDA:::.rls_process_backend_tasks()),
    "skipped a failed native analysis request"
  )
  expect_equal(handled, c("bad", "good"))
  expect_length(state$deferred_control_lines, 0L)
  expect_false(state$processing_backend_tasks)
})

test_that("a live backend polling failure is explicit and leaves the queue retryable", {
  state <- LinkEDA:::.rls_state
  previous <- list(
    process_started = state$process_started,
    processing_backend_tasks = state$processing_backend_tasks,
    deferred_control_lines = state$deferred_control_lines,
    backend_kind = state$backend_kind
  )
  on.exit({
    state$process_started <- previous$process_started
    state$processing_backend_tasks <- previous$processing_backend_tasks
    state$deferred_control_lines <- previous$deferred_control_lines
    state$backend_kind <- previous$backend_kind
  }, add = TRUE)

  state$process_started <- TRUE
  state$processing_backend_tasks <- FALSE
  state$deferred_control_lines <- character()
  state$backend_kind <- "native"

  local_mocked_bindings(
    .rls_send = function(lines, expect_reply = TRUE) {
      stop("temporary queue transport failure")
    },
    .rls_backend_connection_alive = function() TRUE,
    .package = "LinkEDA"
  )

  expect_warning(
    expect_false(LinkEDA:::.rls_process_backend_tasks()),
    "remains pending and will be retried"
  )
  expect_false(state$processing_backend_tasks)
  expect_length(state$deferred_control_lines, 0L)
})

test_that("the native transport accepts an exact generalized-comparison delivery acknowledgement", {
  state <- LinkEDA:::.rls_state
  previous_started <- isTRUE(state$process_started)
  on.exit({
    if (!previous_started) {
      if (isTRUE(state$process_started)) try(ls_close_all(), silent = TRUE)
      LinkEDA:::.rls_reset_backend_connection(clear_views = TRUE)
    }
  }, add = TRUE)
  backend_ready <- tryCatch({
    backend <- LinkEDA:::.rls_backend_path()
    nzchar(backend) && file.exists(backend) &&
      isTRUE(LinkEDA:::.rls_start_backend())
  }, error = function(e) FALSE)
  skip_if_not(backend_ready)

  expect_identical(
    LinkEDA:::.rls_send(c(
      "GCOMP_TASK_RECEIVED",
      "generalized-comparison-transport-smoke",
      "73"
    )),
    "OK\tgeneralized-comparison-transport-smoke"
  )
})
