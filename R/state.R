.rls_state <- new.env(parent = emptyenv())
.rls_state$process_started <- FALSE
.rls_state$sync_session_token <- paste0(Sys.getpid(), "-", format(Sys.time(), "%Y%m%d%H%M%OS6"))
.rls_state$control_path <- NULL
.rls_state$control_port <- NULL
.rls_state$backend_kind <- NULL
.rls_state$notify_path <- NULL
.rls_state$task_handler_started <- FALSE
.rls_state$processing_backend_tasks <- FALSE
.rls_state$deferred_control_lines <- character()
.rls_state$task_poll_generation <- 0L
.rls_state$task_poll_connection <- NULL
.rls_state$backend <- NULL
.rls_state$plots <- new.env(parent = emptyenv())
.rls_state$groups <- new.env(parent = emptyenv())
.rls_state$datasets <- new.env(parent = emptyenv())
.rls_state$data_versions <- new.env(parent = emptyenv())
.rls_state$glm_models <- new.env(parent = emptyenv())
.rls_state$generalized_glm_models <- new.env(parent = emptyenv())
.rls_state$glm_diagnostic_plots <- new.env(parent = emptyenv())
.rls_state$regression_comparisons <- new.env(parent = emptyenv())
.rls_state$correlation_matrices <- new.env(parent = emptyenv())
.rls_state$dimensionality_models <- new.env(parent = emptyenv())
.rls_state$scale_analyses <- new.env(parent = emptyenv())
.rls_state$dendrograms <- new.env(parent = emptyenv())
.rls_state$missing_imputations <- new.env(parent = emptyenv())
.rls_state$table1_tables <- new.env(parent = emptyenv())
.rls_state$spreadplot_listeners <- new.env(parent = emptyenv())
.rls_state$spreadplot_messages <- list()
.rls_state$r_data_return_requests <- new.env(parent = emptyenv())
.rls_state$r_data_return_results <- new.env(parent = emptyenv())
.rls_state$r_data_choice_results <- new.env(parent = emptyenv())
.rls_state$r_data_menu_requests <- new.env(parent = emptyenv())
.rls_state$pending_imports <- new.env(parent = emptyenv())
.rls_state$import_selected_columns <- NULL
.rls_state$active_dataset <- NULL
.rls_state$plot_theme <- "publication"

.rls_option <- function(name, default = NULL) {
  value <- getOption(paste0("LinkEDA.", name), NULL)
  if (!is.null(value)) return(value)
  getOption(paste0("rlispstat.", name), default)
}

.rls_spreadplot_message_types <- c(
  "ROW_SELECTION_CHANGED",
  "ROW_COLORS_CHANGED",
  "DATASET_CHANGED",
  "ACTIVE_DATASET_CHANGED",
  "GLM_SPEC_CHANGED",
  "GLM_DEPENDENT_CHANGED",
  "GLM_PREDICTOR_ADDED",
  "GLM_PREDICTOR_REMOVED",
  "GLM_PREDICTOR_REPLACED",
  "GLM_TERM_TYPE_CHANGED",
  "GLM_SCOPE_CHANGED",
  "GLM_REFIT_REQUESTED",
  "GLM_REFIT_COMPLETED",
  "GLM_DIAGNOSTICS_UPDATED",
  "DIAGNOSTIC_PLOT_OPENED",
  "DIAGNOSTIC_PLOT_CLOSED",
  "VIEW_NEEDS_REDRAW",
  "VIEW_CLOSED"
)

.rls_spreadplot_register <- function(id, group, listens) {
  id <- .rls_validate_protocol_name(id, "id")
  group <- .rls_validate_group(group, allow_null = FALSE)
  listens <- match.arg(listens, .rls_spreadplot_message_types, several.ok = TRUE)
  listener <- list(id = id, group = group, listens = unique(listens), received = list())
  assign(id, listener, envir = .rls_state$spreadplot_listeners)
  invisible(listener)
}

.rls_spreadplot_emit <- function(type, group, sender_id = "", model_id = "",
                                 dataset_id = group, rows = integer(), payload = list()) {
  type <- match.arg(type, .rls_spreadplot_message_types)
  group <- .rls_validate_group(group, allow_null = FALSE)
  message <- list(
    type = type,
    sender_id = sender_id,
    group_id = group,
    model_id = model_id,
    dataset_id = dataset_id,
    row_ids = as.integer(rows),
    payload = payload
  )
  .rls_state$spreadplot_messages[[length(.rls_state$spreadplot_messages) + 1L]] <- message
  for (id in ls(.rls_state$spreadplot_listeners, all.names = TRUE)) {
    listener <- get(id, envir = .rls_state$spreadplot_listeners)
    if (identical(listener$group, group) && type %in% listener$listens) {
      listener$received[[length(listener$received) + 1L]] <- message
      assign(id, listener, envir = .rls_state$spreadplot_listeners)
    }
  }
  invisible(message)
}

.rls_spreadplot_messages <- function() {
  .rls_state$spreadplot_messages
}

.rls_spreadplot_clear_messages <- function() {
  .rls_state$spreadplot_messages <- list()
  for (id in ls(.rls_state$spreadplot_listeners, all.names = TRUE)) {
    listener <- get(id, envir = .rls_state$spreadplot_listeners)
    listener$received <- list()
    assign(id, listener, envir = .rls_state$spreadplot_listeners)
  }
  invisible(TRUE)
}

.rls_spreadplot_listener <- function(id) {
  get(id, envir = .rls_state$spreadplot_listeners)
}

.rls_validate_group <- function(group, allow_null = TRUE) {
  if (is.null(group) && allow_null) {
    return(NULL)
  }
  if (!is.character(group) || length(group) != 1L || is.na(group) ||
      !nzchar(group)) {
    stop("`group` must be a single non-empty string.", call. = FALSE)
  }
  if (grepl("[\r\n\t|]", group)) {
    stop("`group` must not contain control characters or `|`.", call. = FALSE)
  }
  group
}

.rls_validate_protocol_name <- function(value, what) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop(sprintf("`%s` must be a single non-empty string.", what), call. = FALSE)
  }
  if (grepl("[\r\n\t|]", value)) {
    stop(sprintf("`%s` must not contain control characters or `|`.", what), call. = FALSE)
  }
  value
}

.rls_backend_path <- function() {
  names <- if (.Platform$OS.type == "windows") {
    c("LinkEDA_backend.exe", "LinkEDA_backend",
      "rlispstat_backend.exe", "rlispstat_backend")
  } else {
    c("LinkEDA", "LinkEDA_backend", "rlispstat_backend")
  }
  for (name in names) {
    path <- system.file("bin", name, package = "LinkEDA")
    if (nzchar(path) && file.exists(path)) return(path)
    dev_path <- file.path(getwd(), "inst", "bin", name)
    if (file.exists(dev_path)) return(dev_path)
  }

  stop(
    "The LinkEDA native backend was not found. Reinstall the package with `R CMD INSTALL .`.",
    call. = FALSE
  )
}

.rls_open_winui_connection <- function(port, timeout = 1) {
  suppressWarnings(
    tryCatch(
      socketConnection("127.0.0.1", port = port, blocking = TRUE,
                       open = "r+b", timeout = timeout),
      error = function(e) NULL
    )
  )
}

.rls_start_winui <- function() {
  if (.Platform$OS.type != "windows" ||
      !isTRUE(.rls_option("winui_autostart", TRUE))) {
    return(invisible(FALSE))
  }

  portable <- ""
  for (name in c("LinkEDA.exe", "rlispstatWinUI.exe")) {
    candidate <- system.file("bin", "winui", name, package = "LinkEDA")
    if (nzchar(candidate) && file.exists(candidate)) {
      portable <- candidate
      break
    }
  }
  if (nzchar(portable) && file.exists(portable)) {
    log <- tempfile("linkeda-winui-", fileext = ".log")
    status <- tryCatch(
      system2(portable, wait = FALSE, stdout = log, stderr = log),
      error = function(e) 1L
    )
    return(invisible(identical(status, 0L)))
  }

  app_id <- .rls_option(
    "winui_app_id",
    "a456de96-bf6e-4e49-abf7-3a6c27f81367_agce76m72713e!App"
  )
  if (!is.character(app_id) || length(app_id) != 1L ||
      is.na(app_id) || !nzchar(app_id)) {
    stop("`LinkEDA.winui_app_id` must be a single non-empty string.",
         call. = FALSE)
  }

  explorer <- file.path(Sys.getenv("WINDIR", unset = "C:/Windows"), "explorer.exe")
  target <- paste0("shell:AppsFolder\\", app_id)
  status <- tryCatch(
    system2(explorer, args = shQuote(target), wait = FALSE,
            stdout = FALSE, stderr = FALSE),
    error = function(e) 1L
  )
  invisible(identical(status, 0L))
}

.rls_send_winui <- function(lines) {
  lines <- .rls_native_wire_value(lines)
  port <- as.integer(.rls_option("winui_port", 39072L))
  con <- tryCatch(
    .rls_open_winui_connection(port),
    error = function(e) NULL
  )
  if (is.null(con)) {
    .rls_start_winui()
    timeout <- as.numeric(.rls_option("winui_startup_timeout", 10))
    deadline <- Sys.time() + max(0, timeout)
    repeat {
      con <- .rls_open_winui_connection(port)
      if (!is.null(con) || Sys.time() >= deadline) break
      Sys.sleep(0.1)
    }
  }
  if (is.null(con)) {
    stop(
      "Could not start or connect to LinkEDA. Open it from the Windows Start menu and try again.",
      call. = FALSE
    )
  }
  on.exit(close(con), add = TRUE)
  # TCP is a stream, not a message queue.  The WinUI process cannot use the
  # connection close as an end marker because R waits for its reply before
  # closing the connection.  Keep the existing line protocol intact and add a
  # transport-only terminator that WinUI removes before dispatching the command.
  writeLines(c(lines, "__LINKEDA_TCP_MESSAGE_END__"), con, sep = "\n", useBytes = TRUE)
  flush(con)
  reply <- readLines(con, n = 1L, warn = FALSE)
  if (!length(reply)) stop("The LinkEDA WinUI preview closed the connection without replying.", call. = FALSE)
  if (startsWith(reply[[1L]], "ERR ")) stop(sub("^ERR ", "", reply[[1L]]), call. = FALSE)
  .rls_state$process_started <- TRUE
  .rls_state$control_port <- as.integer(port)
  .rls_state$backend_kind <- "winui"
  reply[[1L]]
}

.rls_close_winui_task_poll_connection <- function() {
  con <- .rls_state$task_poll_connection
  .rls_state$task_poll_connection <- NULL
  if (!is.null(con)) try(close(con), silent = TRUE)
  invisible(NULL)
}

.rls_send_winui_task_poll <- function() {
  for (attempt in seq_len(2L)) {
    con <- .rls_state$task_poll_connection
    if (is.null(con)) {
      con <- .rls_open_winui_connection(.rls_state$control_port, timeout = 5)
      if (is.null(con)) next
      .rls_state$task_poll_connection <- con
    }
    response <- tryCatch({
      writeLines(c("MAIN_R_TASKS_KEEPALIVE", "__LINKEDA_TCP_MESSAGE_END__"),
                 con, sep = "\n", useBytes = TRUE)
      flush(con)
      lines <- character()
      repeat {
        line <- readLines(con, n = 1L, warn = FALSE)
        if (!length(line)) stop("task polling connection closed")
        if (identical(line[[1L]], "__LINKEDA_TCP_RESPONSE_END__")) break
        lines <- c(lines, line[[1L]])
      }
      lines
    }, error = function(e) NULL)
    if (!is.null(response) && length(response)) {
      reply <- response[[1L]]
      if (startsWith(reply, "ERR ")) stop(sub("^ERR ", "", reply), call. = FALSE)
      if (length(response) > 1L) {
        .rls_state$deferred_control_lines <- c(
          .rls_state$deferred_control_lines, response[-1L]
        )
      }
      return(reply)
    }
    .rls_close_winui_task_poll_connection()
  }
  stop("Could not poll LinkEDA backend tasks.", call. = FALSE)
}

.rls_send <- function(lines, expect_reply = TRUE, timeout = 5) {
  if (!isTRUE(.rls_state$process_started)) {
    stop("No active LinkEDA backend. Open a plot first with `ls_scatter()`.", call. = FALSE)
  }
  # Centralize protocol encoding for every analysis, plot, data-sheet and
  # callback command.  Individual payload builders still delimit their own
  # fields, but no caller can accidentally write locale-dependent bytes.
  lines <- .rls_native_wire_value(lines)

  if (.Platform$OS.type == "windows") {
    con <- .rls_open_winui_connection(.rls_state$control_port, timeout = timeout)
    if (is.null(con)) {
      .rls_reset_backend_connection(clear_views = TRUE)
      stop("Could not connect to the LinkEDA native backend.", call. = FALSE)
    }
    on.exit(close(con), add = TRUE)
    wire_lines <- if (identical(.rls_state$backend_kind, "winui")) {
      c(lines, "__LINKEDA_TCP_MESSAGE_END__")
    } else {
      lines
    }
    writeLines(wire_lines, con, sep = "\n", useBytes = TRUE)
    flush(con)
    if (!expect_reply) return(invisible(NULL))
    reply_lines <- readLines(con, warn = FALSE)
    if (!length(reply_lines)) stop("The LinkEDA backend closed the connection without replying.", call. = FALSE)
    reply <- reply_lines[[1L]]
    if (startsWith(reply, "ERR ")) stop(sub("^ERR ", "", reply), call. = FALSE)
    if (length(reply_lines) > 1L) {
      control_lines <- reply_lines[-1L]
      if (isTRUE(.rls_state$processing_backend_tasks)) {
        .rls_state$deferred_control_lines <- c(
          .rls_state$deferred_control_lines,
          control_lines
        )
      } else {
        .rls_process_control_lines(control_lines)
      }
    }
    return(reply)
  }
  control_path <- .rls_state$control_path
  if (!is.character(control_path) || length(control_path) != 1L ||
      is.na(control_path) || !nzchar(control_path) || !file.exists(control_path)) {
    .rls_reset_backend_connection(clear_views = TRUE)
    stop("Could not connect to the LinkEDA native backend.", call. = FALSE)
  }

  reply_path <- tempfile("rlispstat-reply-")
  status <- system2("mkfifo", reply_path, stdout = FALSE, stderr = FALSE)
  if (!identical(status, 0L)) {
    stop("Could not create a local reply pipe for the LinkEDA backend.", call. = FALSE)
  }
  on.exit(unlink(reply_path), add = TRUE)

  # Startup commands are short, so open their FIFO writer without blocking.
  # Quit from the macOS menu can remove the last reader between a successful
  # PING and the next LinkEDA() call. file(..., "w") would wait forever then.
  startup_command <- length(lines) <= 8L &&
    lines[[1L]] %in% c("PING", "WELCOME_LAUNCH", "PLOT_THEME")
  if (startup_command) {
    con <- NULL
    for (attempt in seq_len(50L)) {
      con <- tryCatch(suppressWarnings(fifo(control_path,
                                           open = "w", blocking = FALSE)),
                      error = function(e) NULL)
      if (!is.null(con)) break
      if (attempt < 50L) Sys.sleep(0.02)
    }
    if (is.null(con)) {
      stop("The LinkEDA backend is no longer accepting commands.", call. = FALSE)
    }
  } else {
    con <- suppressWarnings(file(control_path, open = "w"))
  }
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  writeLines(c(paste("REPLY", reply_path), lines), con = con, sep = "\n", useBytes = TRUE)
  close(con)

  if (!expect_reply) {
    return(invisible(NULL))
  }

  reply_con <- suppressWarnings(file(reply_path, open = "r"))
  on.exit(try(close(reply_con), silent = TRUE), add = TRUE)
  reply_lines <- readLines(reply_con, warn = FALSE)
  if (!length(reply_lines)) {
    stop("The LinkEDA backend closed the connection without replying.", call. = FALSE)
  }
  reply <- reply_lines[[1L]]
  if (startsWith(reply, "ERR ")) {
    stop(sub("^ERR ", "", reply), call. = FALSE)
  }
  if (length(reply_lines) > 1L) {
    control_lines <- reply_lines[-1L]
    if (isTRUE(.rls_state$processing_backend_tasks)) {
      .rls_state$deferred_control_lines <- c(
        .rls_state$deferred_control_lines,
        control_lines
      )
    } else {
      .rls_process_control_lines(control_lines)
    }
  }
  reply
}

.rls_start_backend_task_poll <- function() {
  enabled <- .rls_option(
    "task_polling",
    identical(Sys.getenv("RSTUDIO"), "1") || .Platform$OS.type == "windows"
  )
  if (!isTRUE(enabled) || !requireNamespace("later", quietly = TRUE)) {
    return(invisible(FALSE))
  }
  interval <- suppressWarnings(as.numeric(.rls_option("task_poll_interval", 0.2)))
  if (!is.finite(interval) || interval < 0.05) interval <- 0.2
  .rls_state$task_poll_generation <- .rls_state$task_poll_generation + 1L
  generation <- .rls_state$task_poll_generation
  tick <- function() {
    if (!isTRUE(.rls_state$process_started) ||
        !identical(.rls_state$task_poll_generation, generation)) {
      return(invisible(NULL))
    }
    # A single malformed or failed analysis request must not permanently stop
    # the queue that services every native menu command. Schedule the next
    # tick from `on.exit()` so it also happens when a handler raises an error.
    on.exit({
      if (isTRUE(.rls_state$process_started) &&
          identical(.rls_state$task_poll_generation, generation)) {
        later::later(tick, interval)
      }
    }, add = TRUE)
    tryCatch(
      .rls_process_backend_tasks(),
      error = function(e) warning(
        "LinkEDA could not process a native analysis request: ",
        conditionMessage(e),
        ". The request queue remains active.",
        call. = FALSE
      )
    )
    invisible(NULL)
  }
  later::later(tick, interval)
  invisible(TRUE)
}

.rls_backend_connection_alive <- function() {
  if (!isTRUE(.rls_state$process_started)) {
    return(FALSE)
  }
  if (.Platform$OS.type == "windows") {
    port <- .rls_state$control_port
    if (is.null(port) || !length(port) || is.na(port)) {
      return(FALSE)
    }
    con <- .rls_open_winui_connection(port, timeout = 0.2)
    if (is.null(con)) {
      return(FALSE)
    }
    on.exit(close(con), add = TRUE)
    probe <- if (identical(.rls_state$backend_kind, "winui")) {
      c("PING", "__LINKEDA_TCP_MESSAGE_END__")
    } else {
      "PING"
    }
    writeLines(probe, con, sep = "\n", useBytes = TRUE)
    flush(con)
    reply <- tryCatch(
      readLines(con, n = 1L, warn = FALSE),
      error = function(e) character()
    )
    return(length(reply) == 1L && identical(reply[[1L]], "OK"))
  }

  path <- .rls_state$control_path
  if (is.null(path) || !length(path) || is.na(path) || !file.exists(path)) {
    return(FALSE)
  }
  for (attempt in seq_len(3L)) {
    available <- try(
      .Call("rls_fifo_has_reader", path, PACKAGE = "LinkEDA"),
      silent = TRUE
    )
    if (isTRUE(available)) {
      return(TRUE)
    }
    if (attempt < 3L) Sys.sleep(0.02)
  }
  FALSE
}

.rls_backend_connection_ready <- function() {
  if (!isTRUE(.rls_backend_connection_alive())) {
    return(FALSE)
  }

  reply <- try(.rls_send("PING"), silent = TRUE)
  if (!identical(reply, "OK")) {
    return(FALSE)
  }
  if (.Platform$OS.type == "windows") {
    return(TRUE)
  }

  # A macOS workbench that is already closing can still have one pending FIFO
  # reader. It will answer this final PING and disappear immediately afterwards.
  # Only reuse the backend after its command loop has opened the next reader;
  # otherwise the following launch command could be sent to a dying instance.
  for (attempt in seq_len(20L)) {
    if (isTRUE(.rls_backend_connection_alive())) {
      return(TRUE)
    }
    Sys.sleep(0.01)
  }
  FALSE
}

.rls_reset_backend_connection <- function(clear_views = FALSE) {
  control_path <- .rls_state$control_path
  .rls_stop_backend_task_handler()
  .rls_state$process_started <- FALSE
  .rls_state$processing_backend_tasks <- FALSE
  .rls_state$deferred_control_lines <- character()
  .rls_state$control_path <- NULL
  .rls_state$control_port <- NULL
  .rls_state$backend_kind <- NULL
  .rls_state$backend <- NULL
  if (!is.null(control_path) && length(control_path) == 1L &&
      !is.na(control_path) && file.exists(control_path)) {
    unlink(control_path)
  }
  if (isTRUE(clear_views)) {
    rm(list = ls(envir = .rls_state$plots), envir = .rls_state$plots)
    rm(list = ls(envir = .rls_state$groups), envir = .rls_state$groups)
  }
  invisible(TRUE)
}

.rls_start_backend <- function() {
  if (isTRUE(.rls_state$process_started)) {
    if (isTRUE(.rls_backend_connection_ready())) {
      # The native application may have been closed and reopened while the R
      # session survived. Its socket can be healthy even though an older
      # `later` callback has already terminated, so always re-arm the poller.
      .rls_start_backend_task_poll()
      return(invisible(TRUE))
    }
    .rls_reset_backend_connection(clear_views = TRUE)
  }
  if (isFALSE(.rls_option("launch", TRUE))) {
    stop("Backend launching is disabled by option `LinkEDA.launch = FALSE`.", call. = FALSE)
  }

  if (.Platform$OS.type == "windows") {
    # Prefer the native WinUI application when it is installed.  It owns the
    # visible event loop; the console backend below remains a useful Rtools
    # fallback for protocol and core-model validation.
    winui_error <- NULL
    if (isTRUE(.rls_option("use_winui", TRUE))) {
      winui_ready <- tryCatch({
        ping <- .rls_send_winui("PING")
        protocol <- .rls_send_winui("LIST_PLOTS")
        identical(ping, "OK") && startsWith(protocol, "OK")
      },
        error = function(e) {
          winui_error <<- conditionMessage(e)
          FALSE
        }
      )
      if (isTRUE(winui_ready)) {
        .rls_start_backend_task_poll()
        return(invisible(TRUE))
      }
      .rls_reset_backend_connection(clear_views = FALSE)
    }
    backend <- tryCatch(.rls_backend_path(), error = function(e) NULL)
    if (is.null(backend)) {
      detail <- if (nzchar(winui_error %||% "")) {
        paste0(" WinUI reported: ", winui_error)
      } else {
        ""
      }
      stop(
        paste0(
          "Could not connect to the installed LinkEDA WinUI application, and no ",
          "Rtools console backend was found.", detail
        ),
        call. = FALSE
      )
    }
    port <- sample.int(20000L, 1L) + 40000L
    log <- tempfile("rlispstat-backend-", fileext = ".log")
    system2(backend, args = c("--port", port, "--parent-pid", Sys.getpid()),
            stdout = log, stderr = log, wait = FALSE)
    .rls_state$backend <- backend
    .rls_state$control_port <- port
    .rls_state$backend_kind <- "native"
    timeout <- .rls_option("startup_timeout", 20)
    for (i in seq_len(max(1L, ceiling(timeout / 0.1)))) {
      .rls_state$process_started <- TRUE
      if (isTRUE(try(identical(.rls_send("PING"), "OK"), silent = TRUE))) {
        .rls_start_backend_task_poll()
        return(invisible(TRUE))
      }
      .rls_state$process_started <- FALSE
      Sys.sleep(0.1)
    }
    stop(sprintf("The LinkEDA native backend did not start. Backend log: %s", log), call. = FALSE)
  }
  backend <- .rls_backend_path()
  control_path <- tempfile("rlispstat-cmd-")
  notify_path <- tempfile("rlispstat-notify-")
  log <- tempfile("rlispstat-backend-", fileext = ".log")
  rscript <- file.path(R.home("bin"), "Rscript")
  notify_ok <- identical(system2("mkfifo", notify_path, stdout = FALSE, stderr = FALSE), 0L)
  if (isTRUE(notify_ok)) {
    notify_ok <- isTRUE(try(.Call("rls_register_task_handler", notify_path, PACKAGE = "LinkEDA"),
                            silent = TRUE))
  }

  system2(
    backend,
    args = c(
      "--fifo", control_path,
      "--rscript", rscript,
      if (isTRUE(notify_ok)) c("--notify-fifo", notify_path),
      "--parent-pid", Sys.getpid()
    ),
    stdout = log,
    stderr = log,
    wait = FALSE
  )

  .rls_state$backend <- backend
  .rls_state$control_path <- control_path
  .rls_state$notify_path <- if (isTRUE(notify_ok)) notify_path else NULL
  .rls_state$task_handler_started <- isTRUE(notify_ok)

  timeout <- .rls_option("startup_timeout", 20)
  attempts <- max(1L, ceiling(timeout / 0.1))
  for (i in seq_len(attempts)) {
    if (file.exists(control_path)) {
      .rls_state$process_started <- TRUE
      ok <- try(identical(.rls_send("PING"), "OK"), silent = TRUE)
      if (isTRUE(ok)) {
        try(.rls_send(c("PLOT_THEME", .rls_state$plot_theme %||% "publication")), silent = TRUE)
        .rls_start_backend_task_poll()
        return(invisible(TRUE))
      }
      .rls_state$process_started <- FALSE
    }
    Sys.sleep(0.1)
  }

  .rls_state$process_started <- FALSE
  .rls_stop_backend_task_handler()
  stop(
    sprintf("The LinkEDA native backend did not start. Backend log: %s", log),
    call. = FALSE
  )
}

.rls_process_backend_tasks <- function() {
  if (!isTRUE(.rls_state$process_started) ||
      isTRUE(.rls_state$processing_backend_tasks)) {
    return(invisible(FALSE))
  }
  .rls_state$processing_backend_tasks <- TRUE
  on.exit(.rls_state$processing_backend_tasks <- FALSE)
  outcome <- try(
    if (.Platform$OS.type == "windows" &&
        identical(.rls_state$backend_kind, "winui")) {
      .rls_send_winui_task_poll()
    } else {
      .rls_send("MAIN_R_TASKS")
    },
    silent = TRUE
  )
  if (inherits(outcome, "try-error")) {
    if (!.rls_backend_connection_alive()) {
      .rls_reset_backend_connection(clear_views = TRUE)
      return(invisible(FALSE))
    }
    warning(
      "LinkEDA could not retrieve the pending native analysis queue. The native request remains pending and will be retried.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }

  # Backend commands can reply with more work for R. Process that work
  # iteratively instead of recursively through .rls_send(), and execute an
  # identical task at most once per poll cycle. Besides keeping the R call
  # stack bounded, this prevents a failed task from being immediately queued
  # and retried forever by an older or inconsistent native frontend.
  seen <- character()
  processed <- 0L
  max_tasks_per_poll <- 256L
  repeat {
    pending <- .rls_state$deferred_control_lines
    .rls_state$deferred_control_lines <- character()
    if (!length(pending)) break

    pending <- pending[!duplicated(pending) & !(pending %in% seen)]
    if (!length(pending)) break
    for (line in pending) {
      seen <- c(seen, line)
      tryCatch(
        .rls_process_control_lines(line),
        error = function(e) warning(
          "LinkEDA skipped a failed native analysis request: ",
          conditionMessage(e),
          call. = FALSE
        )
      )
      processed <- processed + 1L
      if (processed >= max_tasks_per_poll) break
    }
    if (processed >= max_tasks_per_poll) {
      .rls_state$deferred_control_lines <- character()
      warning(
        "LinkEDA stopped an excessive chain of backend tasks; the next polling cycle will continue normally.",
        call. = FALSE
      )
      break
    }
  }
  invisible(!inherits(outcome, "try-error"))
}

.rls_stop_backend_task_handler <- function() {
  .rls_state$task_poll_generation <- .rls_state$task_poll_generation + 1L
  .rls_close_winui_task_poll_connection()
  if (.Platform$OS.type != "windows") {
    try(.Call("rls_unregister_task_handler", TRUE, PACKAGE = "LinkEDA"), silent = TRUE)
  }
  if (!is.null(.rls_state$notify_path)) {
    unlink(.rls_state$notify_path)
  }
  .rls_state$notify_path <- NULL
  .rls_state$task_handler_started <- FALSE
  invisible(TRUE)
}

.rls_prepare_scatter_data <- function(data, x, y, group = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  for (col in c(x, y)) {
    col <- .rls_validate_protocol_name(col, if (identical(col, x)) "x" else "y")
    if (!col %in% names(data)) {
      stop(sprintf("Column `%s` was not found in `data`.", col), call. = FALSE)
    }
    if (!is.numeric(data[[col]])) {
      stop(sprintf("Column `%s` must be numeric.", col), call. = FALSE)
    }
  }

  group <- .rls_validate_group(group, allow_null = TRUE)
  ok <- stats::complete.cases(data[, c(x, y), drop = FALSE])
  rows <- which(ok)

  list(
    x = unname(as.double(data[[x]][ok])),
    y = unname(as.double(data[[y]][ok])),
    row = rows,
    x_name = x,
    y_name = y,
    group = group,
    n_total = nrow(data)
  )
}

.rls_numeric_variable_names <- function(data, metadata = NULL) {
  if (!is.null(metadata) && nrow(metadata)) {
    return(names(data)[vapply(names(data), function(name) {
      identical(.rls_metadata_type(metadata, name, .rls_variable_type(data[[name]])), "numeric")
    }, logical(1L))])
  }
  names(data)[vapply(data, is.numeric, logical(1L))]
}

.rls_variable_type <- function(x) {
  if (is.ordered(x)) {
    "ordered"
  } else if (is.factor(x)) {
    "factor"
  } else if (is.character(x)) {
    "character"
  } else if (is.logical(x)) {
    "logical"
  } else if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    "datetime"
  } else if (is.numeric(x)) {
    "numeric"
  } else {
    "other"
  }
}

.rls_variable_payload <- function(data, metadata = NULL) {
  numeric_names <- .rls_numeric_variable_names(data, metadata)
  for (name in names(data)) {
    .rls_validate_protocol_name(name, "variable name")
  }
  lines <- c("VARS", as.character(length(numeric_names)))
  for (name in numeric_names) {
    values <- as.double(data[[name]])
    value_lines <- ifelse(is.na(values) | !is.finite(values), "NA", sprintf("%.17g", values))
    lines <- c(lines, name, as.character(length(values)), value_lines)
  }
  lines <- c(lines, "VARMETA", as.character(length(names(data))))
  for (name in names(data)) {
    lines <- c(lines, name, .rls_metadata_type(metadata, name, .rls_variable_type(data[[name]])))
  }
  lines
}

# max_cell_chars is retained for older internal callers but no longer limits data.
.rls_dataframe_payload <- function(data, metadata = NULL, dataset_record = NULL, max_cell_chars = NULL) {
  names <- names(data)
  for (name in names) {
    .rls_validate_protocol_name(name, "variable name")
  }
  wire_value <- function(value) {
    .rls_encode_data_value(value)
  }
  metadata_value <- function(name, column, default = "") {
    if (is.null(metadata) || !nrow(metadata) || !column %in% names(metadata)) {
      return(default)
    }
    name_col <- if ("variable_name" %in% names(metadata)) "variable_name" else "name"
    row <- match(name, metadata[[name_col]])
    if (is.na(row)) {
      return(default)
    }
    value <- metadata[[column]][[row]]
    if (is.null(value) || !length(value) || is.na(value[[1L]])) {
      return(default)
    }
    as.character(value[[1L]])
  }
  clean_values <- function(values) {
    values[is.na(values)] <- "NA"
    # Transport complete values: shortening is a view concern, and truncating
    # here can merge distinct categories or damage data returned to R.
    .rls_encode_data_value(values)
  }
  raw_values <- function(x) {
    if (!is.null(attr(x, "labels", exact = TRUE))) {
      raw <- unclass(x)
      attributes(raw) <- NULL
      return(as.character(raw))
    }
    as.character(x)
  }
  labelled_values <- function(x) {
    labels <- attr(x, "labels", exact = TRUE)
    raw <- if (!is.null(labels)) {
      tmp <- unclass(x)
      attributes(tmp) <- NULL
      tmp
    } else {
      x
    }
    out <- as.character(raw)
    if (!is.null(labels) && length(labels)) {
      label_values <- unclass(labels)
      attributes(label_values) <- NULL
      label_names <- names(labels)
      if (is.null(label_names)) {
        label_names <- rep("", length(labels))
      }
      if (length(label_names) < length(labels)) {
        label_names <- c(label_names, rep("", length(labels) - length(label_names)))
      }
      for (i in seq_along(label_values)) {
        label <- label_names[[i]]
        if (!length(label) || is.na(label) || !nzchar(label)) {
          label <- as.character(label_values[[i]])
        }
        out[!is.na(raw) & raw == label_values[[i]]] <- label
      }
    }
    out
  }
  lines <- c("DATAFRAME", as.character(nrow(data)), as.character(length(names)), "DATACELLS_PERCENT_V1")
  display <- list()
  for (name in names) {
    values <- clean_values(raw_values(data[[name]]))
    shown <- clean_values(labelled_values(data[[name]]))
    if (!identical(values, shown)) {
      display[[name]] <- shown
    }
    lines <- c(lines, name, .rls_metadata_type(metadata, name, .rls_variable_type(data[[name]])), values)
  }
  if (length(display)) {
    lines <- c(lines, "DATADISPLAY", as.character(length(display)))
    for (name in names(display)) {
      lines <- c(lines, name, display[[name]])
    }
  }
  level_columns <- names[vapply(names, function(name) {
    .rls_metadata_type(metadata, name, .rls_variable_type(data[[name]])) %in%
      c("factor", "ordered", "logical")
  }, logical(1L))]
  if (length(level_columns)) {
    lines <- c(lines, "DATLEVELS", as.character(length(level_columns)))
    for (name in level_columns) {
      type <- .rls_metadata_type(metadata, name, .rls_variable_type(data[[name]]))
      defined <- .rls_metadata_levels(
        metadata, name, .rls_semantic_variable_levels(data[[name]], type)
      )
      defined <- clean_values(defined)
      lines <- c(lines, name, as.character(length(defined)), defined)
    }
  }
  lines <- c(lines, "DATAMETA", as.character(length(names)))
  for (name in names) {
    display_name <- metadata_value(name, "display_name", name)
    description <- metadata_value(name, "description", metadata_value(name, "label", ""))
    decimals <- suppressWarnings(as.integer(metadata_value(name, "decimals", NA_character_)))
    if (!length(decimals) || is.na(decimals)) decimals <- -1L
    lines <- c(lines, name, wire_value(display_name), wire_value(description), as.character(decimals))
  }
  # Keep the statistical meaning of a variable separate from its R storage
  # representation.  In particular, an R logical vector is transported as a
  # binary Categorical variable without pretending that its storage ceased to
  # be logical.  The section is optional so the current parser remains
  # compatible with payloads produced before semantic metadata was added.
  metadata_list_value <- function(name, column, default = NULL) {
    if (is.null(metadata) || !nrow(metadata) || !column %in% names(metadata)) return(default)
    name_col <- if ("variable_name" %in% names(metadata)) "variable_name" else "name"
    row <- match(name, metadata[[name_col]])
    if (is.na(row)) return(default)
    value <- metadata[[column]][[row]]
    if (is.null(value)) default else value
  }
  lines <- c(lines, "DATATYPEMETA", as.character(length(names)))
  for (name in names) {
    semantic <- .rls_metadata_type(metadata, name, .rls_variable_type(data[[name]]))
    if (identical(semantic, "logical")) semantic <- "factor"
    storage <- metadata_value(name, "storage_class", paste(class(data[[name]]), collapse = ", "))
    order <- as.character(metadata_list_value(
      name, "category_order",
      .rls_metadata_levels(metadata, name, .rls_semantic_variable_levels(data[[name]], semantic))
    ))
    mapping <- metadata_list_value(name, "numeric_mapping", numeric())
    if (is.null(names(mapping))) names(mapping) <- rep("", length(mapping))
    mapping_values <- if (length(mapping)) {
      format(as.double(mapping), digits = 17, scientific = FALSE, trim = TRUE)
    } else character()
    history <- metadata_list_value(name, "type_change_provenance", list())
    reversible_type <- ""
    if (identical(semantic, "numeric") && length(mapping) && length(history)) {
      previous <- history[[length(history)]]$from %||% "factor"
      if (previous %in% c("factor", "ordered")) reversible_type <- previous
    }
    binary <- semantic %in% c("factor", "ordered") && length(order) == 2L
    lines <- c(
      lines, name, semantic, wire_value(storage), if (binary) "1" else "0",
      reversible_type,
      as.character(length(order)), wire_value(order),
      as.character(length(mapping))
    )
    if (length(mapping)) {
      for (index in seq_along(mapping)) {
        lines <- c(lines, wire_value(names(mapping)[[index]]), wire_value(mapping_values[[index]]))
      }
    }
  }
  if (!is.null(dataset_record) &&
      identical(dataset_record$dataset_type %||% "data_frame", "multiple_imputation")) {
    completed <- dataset_record$completed_datasets %||% list()
    original <- dataset_record$original_data %||% data
    mask <- dataset_record$missing_cell_mask %||% .rls_mi_missing_mask(original, character())
    m <- length(completed)
    active <- dataset_record$active_imputation_version %||% 1L
    mode <- dataset_record$imputation_display_mode %||% "version"
    imputed_names <- names[vapply(names, function(name) {
      name %in% names(mask) && any(mask[[name]])
    }, logical(1L))]
    lines <- c(
      lines,
      "IMPUTATION_SPARSE",
      "multiple_imputation",
      dataset_record$imputation_id %||% "",
      dataset_record$source_dataset_id %||% "",
      as.character(m),
      as.character(active),
      mode,
      as.character(length(imputed_names))
    )
    # Build the sparse section in chunks. Appending each imputation's values
    # to `lines` copies the entire growing wire vector thousands of times for
    # wide mids files (for example, 340 variables x 50 imputations).
    sparse_chunks <- vector("list", length(imputed_names))
    for (column_index in seq_along(imputed_names)) {
      name <- imputed_names[[column_index]]
      missing_rows <- which(mask[[name]])
      original_values <- clean_values(raw_values(original[[name]])[missing_rows])
      version_values <- lapply(seq_len(m), function(version) {
        clean_values(raw_values(completed[[version]][[name]])[missing_rows])
      })
      sparse_chunks[[column_index]] <- c(
        name, as.character(length(missing_rows)), as.character(missing_rows),
        original_values, unlist(version_values, use.names = FALSE)
      )
    }
    lines <- c(lines, unlist(sparse_chunks, use.names = FALSE))
  }
  if (!is.null(dataset_record) && inherits(dataset_record$mids_object,"mids")) {
    lines <- c(lines,"IMPUTATION_PROCESS_V1",.rls_mi_encode_process(
      dataset_record$mids_object,dataset_record$import_name_map))
  }
  if (!is.null(dataset_record)) {
    lines <- c(lines, .rls_data_provenance_payload(dataset_record, data = data))
  }
  lines <- c(lines, "DATA_SYNC_SESSION_V1", .rls_state$sync_session_token)
  lines
}

.rls_native_dataset_matches <- function(dataset, data = dataset$data, sender = .rls_send) {
  reply <- tryCatch(sender(c("DATASET_SYNC_STATUS", dataset$group)),
                    error = function(e) "")
  if (!startsWith(reply, "OK\tpresent\t")) return(FALSE)
  fields <- strsplit(reply, "\t", fixed = TRUE)[[1L]]
  if (length(fields) != 13L) return(FALSE)
  native_version <- suppressWarnings(as.integer(fields[[3L]]))
  r_version <- as.integer(dataset$data_version %||% 1L)
  if (identical(fields[[12L]], .rls_state$sync_session_token) &&
      !is.na(native_version) && native_version > r_version) {
    stop("The data sheet changed while R was calculating. Synchronize it before refitting.",
         call. = FALSE)
  }
  if (!identical(data, dataset$data)) return(FALSE)
  expected <- c(
    "OK", "present", as.character(as.integer(dataset$data_version %||% 1L)),
    as.character(nrow(data)), as.character(ncol(data)),
    dataset$dataset_type %||% "data_frame",
    dataset$imputation_id %||% "",
    as.character(as.integer(dataset$imputation_count %||% 0L)),
    as.character(as.integer(dataset$active_imputation_version %||% 1L)),
    dataset$imputation_display_mode %||% "version",
    dataset$source_dataset_id %||% "", .rls_state$sync_session_token, "END"
  )
  identical(fields, expected)
}

.rls_register_native_dataset_if_needed <- function(dataset, data = dataset$data,
                                                    visible = FALSE, sender = .rls_send) {
  if (.rls_native_dataset_matches(dataset, data, sender = sender)) {
    if (isTRUE(visible)) sender(c("DATA_OPEN_DATA_SHEET", dataset$group))
    return(invisible(FALSE))
  }
  sender(c(
    if (isTRUE(visible)) "REGISTER_DATASET" else "REGISTER_DATASET_SILENT",
    dataset$group,
    .rls_variable_payload(data, dataset$variable_metadata),
    .rls_dataframe_payload(data, dataset$variable_metadata, dataset_record = dataset)
  ))
  invisible(TRUE)
}

.rls_plot_id <- function(plot) {
  if (inherits(plot, "rlispstat_plot")) {
    return(plot$id)
  }
  if (is.character(plot) && length(plot) == 1L && !is.na(plot) && nzchar(plot)) {
    return(plot)
  }
  stop("`plot` must be a LinkEDA plot object or a plot id.", call. = FALSE)
}

.rls_parse_records <- function(reply) {
  if (identical(reply, "OK")) {
    return(character())
  }
  if (!startsWith(reply, "OK\t")) {
    stop("Unexpected reply from the LinkEDA backend.", call. = FALSE)
  }
  strsplit(sub("^OK\t", "", reply), "\t", fixed = FALSE)[[1L]]
}

.rls_active_group_names <- function() {
  unique(unname(vapply(ls(envir = .rls_state$plots), function(id) {
    get(id, envir = .rls_state$plots)$group
  }, character(1L))))
}

.rls_resolve_group <- function(group = NULL) {
  if (!is.null(group)) {
    return(.rls_validate_group(group, allow_null = FALSE))
  }
  groups <- .rls_active_group_names()
  if (!length(groups)) {
    stop("No active plot/group.", call. = FALSE)
  }
  if (length(groups) > 1L) {
    stop("`group` is required because more than one active group exists.", call. = FALSE)
  }
  groups[[1L]]
}

.rls_register_plot <- function(plot_id, group, x, y, data, rows, type = "scatter", title = NULL, ...) {
  assign(
    plot_id,
    c(
      list(id = plot_id, group = group, x = x, y = y, n = length(rows), rows = rows,
           data = data, type = type, title = title),
      list(...)
    ),
    envir = .rls_state$plots
  )
  if (!exists(group, envir = .rls_state$groups, inherits = FALSE)) {
    assign(group, list(data = data, rows = seq_len(nrow(data))), envir = .rls_state$groups)
  }
  if (!exists(group, envir = .rls_state$datasets, inherits = FALSE)) {
    .rls_register_dataset(group, data, source = "plot", activate = is.null(.rls_state$active_dataset))
  }
}

.rls_unregister_plot <- function(plot_id) {
  if (!exists(plot_id, envir = .rls_state$plots, inherits = FALSE)) {
    return(invisible(FALSE))
  }
  group <- get(plot_id, envir = .rls_state$plots)$group
  rm(list = plot_id, envir = .rls_state$plots)
  still_used <- any(vapply(ls(envir = .rls_state$plots), function(id) {
    identical(get(id, envir = .rls_state$plots)$group, group)
  }, logical(1L)))
  if (!still_used && exists(group, envir = .rls_state$groups, inherits = FALSE)) {
    rm(list = group, envir = .rls_state$groups)
  }
  invisible(TRUE)
}

.rls_parse_generalized_glm_needed <- function(parts) {
  if (length(parts) < 8L) return(NULL)
  cursor <- 9L
  term_count <- suppressWarnings(as.integer(parts[8L]))
  terms <- character()
  if (is.finite(term_count) && term_count > 0L &&
      length(parts) >= cursor + term_count - 1L) {
    terms <- parts[seq.int(cursor, cursor + term_count - 1L)]
    cursor <- cursor + term_count
  }

  term_types <- list()
  type_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[cursor])) else 0L
  cursor <- cursor + 1L
  if (is.finite(type_count) && type_count > 0L &&
      length(parts) >= cursor + 2L * type_count - 1L) {
    for (i in seq_len(type_count)) {
      term <- parts[cursor]
      type <- parts[cursor + 1L]
      cursor <- cursor + 2L
      if (nzchar(term) && type %in% c("numeric", "factor")) term_types[[term]] <- type
    }
  }

  # The fields through task_rows are the stable v0 prefix.  Keep parsing
  # them before any optional shared-model extension so a newly built native
  # UI remains compatible with an already-loaded older R namespace.
  mode <- if (length(parts) >= cursor) parts[cursor] else "GENERALIZED"; cursor <- cursor + 1L
  event <- if (length(parts) >= cursor) parts[cursor] else ""; cursor <- cursor + 1L
  reference <- if (length(parts) >= cursor) parts[cursor] else ""; cursor <- cursor + 1L
  row_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[cursor])) else 0L
  cursor <- cursor + 1L
  task_rows <- if (is.finite(row_count) && row_count > 0L &&
      length(parts) >= cursor + row_count - 1L) {
    suppressWarnings(as.integer(parts[seq.int(cursor, cursor + row_count - 1L)]))
  } else integer()
  task_rows <- task_rows[is.finite(task_rows) & task_rows > 0L]
  cursor <- cursor + if (is.finite(row_count) && row_count > 0L) row_count else 0L

  centered_predictors <- character()
  factor_reference_levels <- list()
  if (length(parts) >= cursor && identical(parts[cursor], "MODEL_SPEC_V1")) {
    cursor <- cursor + 1L
    centered_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[cursor])) else 0L
    cursor <- cursor + 1L
    if (is.finite(centered_count) && centered_count > 0L &&
        length(parts) >= cursor + centered_count - 1L) {
      centered_predictors <- parts[seq.int(cursor, cursor + centered_count - 1L)]
      cursor <- cursor + centered_count
    }
    reference_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[cursor])) else 0L
    cursor <- cursor + 1L
    if (is.finite(reference_count) && reference_count > 0L &&
        length(parts) >= cursor + 2L * reference_count - 1L) {
      for (i in seq_len(reference_count)) {
        variable <- parts[cursor]
        reference_level <- parts[cursor + 1L]
        cursor <- cursor + 2L
        if (nzchar(variable) && nzchar(reference_level)) {
          factor_reference_levels[[variable]] <- reference_level
        }
      }
    }
  }

  count_regression <- identical(mode, "COUNT")
  count_distribution <- "poisson"
  exposure <- ""
  offset <- ""
  trials_variable <- ""
  trials_constant <- NA_real_
  if (length(parts) >= cursor && parts[cursor] %in% c("COUNT_SPEC_V1", "COUNT_SPEC_V2")) {
    count_spec_version <- parts[cursor]
    cursor <- cursor + 1L
    if (length(parts) >= cursor) {
      count_distribution <- parts[cursor]
      cursor <- cursor + 1L
    }
    if (length(parts) >= cursor) exposure <- parts[cursor]
    cursor <- cursor + 1L
    if (identical(count_spec_version, "COUNT_SPEC_V2")) {
      if (length(parts) >= cursor) trials_variable <- parts[cursor]
      cursor <- cursor + 1L
      if (length(parts) >= cursor) trials_constant <- suppressWarnings(as.numeric(parts[cursor]))
      cursor <- cursor + 1L
    }
  }
  if (length(parts) >= cursor && identical(parts[cursor], "OFFSET_SPEC_V1")) {
    cursor <- cursor + 1L
    offset <- if (length(parts) >= cursor) parts[cursor] else ""
    cursor <- cursor + 1L
  }

  response_bounds <- NULL
  if (length(parts) >= cursor && identical(parts[cursor], "BOUNDED_RESPONSE_V1")) {
    cursor <- cursor + 1L
    lower <- if (length(parts) >= cursor) suppressWarnings(as.numeric(parts[cursor])) else NA_real_
    cursor <- cursor + 1L
    upper <- if (length(parts) >= cursor) suppressWarnings(as.numeric(parts[cursor])) else NA_real_
    cursor <- cursor + 1L
    if (is.finite(lower) && is.finite(upper) && lower < upper) {
      response_bounds <- c(lower, upper)
    }
  }

  model_type <- .rls_model_type_for_family(
    parts[5L], binary = identical(mode, "BINARY"), count = count_regression
  )
  if (length(parts) >= cursor && identical(parts[cursor], "MODEL_TYPE_V1")) {
    cursor <- cursor + 1L
    if (length(parts) >= cursor && parts[cursor] %in% names(.rls_model_type_catalogue)) {
      model_type <- parts[cursor]
    }
    cursor <- cursor + 1L
  }

  generation <- 0L
  if (length(parts) >= cursor && identical(parts[cursor], "GGLM_REQUEST_V1")) {
    cursor <- cursor + 1L
    generation <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[cursor])) else 0L
    if (!is.finite(generation) || generation < 0L) generation <- 0L
  }

  list(
    id = parts[2L], group = parts[3L], response = parts[4L],
    family = parts[5L], link = parts[6L], scope = parts[7L],
    terms = terms, term_types = term_types,
    centered_predictors = centered_predictors,
    factor_reference_levels = factor_reference_levels,
    binary = identical(mode, "BINARY"), count_regression = count_regression,
    count_distribution = count_distribution, exposure = exposure, offset = offset,
    trials_variable = trials_variable, trials_constant = trials_constant,
    response_bounds = response_bounds,
    model_type = model_type,
    event = event, reference = reference,
    selected_rows = task_rows, generation = generation
  )
}

.rls_process_control_lines <- function(lines) {
  for (line in lines) {
    if (startsWith(line, "SMOOTH_NEEDED|")) {
      parts <- strsplit(line, "|", fixed = TRUE)[[1L]]
      if (length(parts) >= 4L) {
        x_col <- if (length(parts) >= 5L) parts[5L] else NULL
        y_col <- if (length(parts) >= 6L) parts[6L] else NULL
        span <- if (length(parts) >= 7L) suppressWarnings(as.double(parts[7L])) else 0.75
        cursor <- 8L
        selected_rows <- NULL
        row_colors <- NULL
        if (length(parts) >= cursor) {
          selected_count <- suppressWarnings(as.integer(parts[cursor])); cursor <- cursor + 1L
          if (is.finite(selected_count) && selected_count >= 0L &&
              length(parts) >= cursor + selected_count - 1L) {
            selected_rows <- if (selected_count > 0L) {
              suppressWarnings(as.integer(parts[seq.int(cursor, cursor + selected_count - 1L)]))
            } else integer()
            selected_rows <- selected_rows[is.finite(selected_rows) & selected_rows > 0L]
            cursor <- cursor + selected_count
          }
        }
        if (length(parts) >= cursor) {
          color_count <- suppressWarnings(as.integer(parts[cursor])); cursor <- cursor + 1L
          if (is.finite(color_count) && color_count >= 0L &&
              length(parts) >= cursor + 2L * color_count - 1L) {
            row_colors <- character()
            if (color_count > 0L) {
              for (index in seq_len(color_count)) {
                row <- parts[cursor]; color <- parts[cursor + 1L]; cursor <- cursor + 2L
                if (nzchar(row) && nzchar(color)) row_colors[[row]] <- color
              }
            }
          }
        }
        imputation_point_sets <- list()
        if (length(parts) >= cursor && identical(parts[cursor], "MI_POINT_SETS_V1")) {
          cursor <- cursor + 1L
          set_count <- if (length(parts) >= cursor) {
            suppressWarnings(as.integer(parts[cursor]))
          } else 0L
          cursor <- cursor + 1L
          if (is.finite(set_count) && set_count > 0L) {
            for (set_index in seq_len(set_count)) {
              if (length(parts) < cursor + 1L) break
              imputation_index <- suppressWarnings(as.integer(parts[cursor]))
              point_count <- suppressWarnings(as.integer(parts[cursor + 1L]))
              cursor <- cursor + 2L
              rows <- integer(); x <- numeric(); y <- numeric()
              if (is.finite(point_count) && point_count > 0L &&
                  length(parts) >= cursor + 3L * point_count - 1L) {
                rows <- integer(point_count)
                x <- numeric(point_count)
                y <- numeric(point_count)
                for (point_index in seq_len(point_count)) {
                  rows[point_index] <- suppressWarnings(as.integer(parts[cursor]))
                  x[point_index] <- suppressWarnings(as.double(parts[cursor + 1L]))
                  y[point_index] <- suppressWarnings(as.double(parts[cursor + 2L]))
                  cursor <- cursor + 3L
                }
              }
              valid <- is.finite(rows) & rows > 0L & is.finite(x) & is.finite(y)
              imputation_point_sets[[length(imputation_point_sets) + 1L]] <- list(
                imputation = imputation_index, rows = rows[valid],
                x = x[valid], y = y[valid]
              )
            }
          }
        }
        visible_rows <- NULL
        if (length(parts) >= cursor && identical(parts[cursor], "VISIBLE_ROWS_V1")) {
          cursor <- cursor + 1L
          row_count <- if (length(parts) >= cursor) {
            suppressWarnings(as.integer(parts[cursor]))
          } else NA_integer_
          cursor <- cursor + 1L
          if (is.finite(row_count) && row_count >= 0L &&
              length(parts) >= cursor + row_count - 1L) {
            visible_rows <- if (row_count > 0L) {
              suppressWarnings(as.integer(parts[seq.int(
                cursor, cursor + row_count - 1L)]))
            } else integer()
            visible_rows <- visible_rows[is.finite(visible_rows) & visible_rows > 0L]
            cursor <- cursor + row_count
          }
        }
        fit_method <- "loess"
        confidence_level <- 0.95
        if (length(parts) >= cursor && identical(parts[cursor], "FIT_CURVE_V2")) {
          fit_method <- parts[cursor + 1L]
          confidence_level <- suppressWarnings(as.double(parts[cursor + 2L]))
          if (!fit_method %in% c("loess", "lm")) fit_method <- "loess"
          if (!is.finite(confidence_level) || confidence_level <= 0 || confidence_level >= 1)
            confidence_level <- 0.95
        }
        .rls_handle_smooth_needed(parts[2L], parts[3L], parts[4L], x_col, y_col, span,
                                  selected_rows, row_colors, imputation_point_sets,
                                  fit_method, confidence_level, visible_rows)
      }
    } else if (startsWith(line, "TRELLIS_SMOOTH_NEEDED\t")) {
      parts <- strsplit(line, "\t", fixed = TRUE)[[1L]]
      if (length(parts) >= 9L) {
        row_count <- suppressWarnings(as.integer(parts[9L]))
        panel_rows <- integer()
        if (is.finite(row_count) && row_count > 0L && length(parts) >= 9L + row_count) {
          panel_rows <- suppressWarnings(as.integer(parts[seq.int(10L, 9L + row_count)]))
          panel_rows <- panel_rows[is.finite(panel_rows) & panel_rows > 0L]
        }
        safe_row_count <- if (is.finite(row_count) && row_count > 0L) row_count else 0L
        color_cursor <- 10L + safe_row_count
        color_count <- if (length(parts) >= color_cursor) {
          suppressWarnings(as.integer(parts[color_cursor]))
        } else 0L
        panel_colors <- character()
        if (is.finite(color_count) && color_count > 0L &&
            length(parts) >= color_cursor + color_count) {
          panel_colors <- parts[seq.int(color_cursor + 1L, color_cursor + color_count)]
        }
        selection_cursor <- color_cursor + 1L +
          if (is.finite(color_count) && color_count > 0L) color_count else 0L
        selected_rows <- NULL
        if (length(parts) >= selection_cursor) {
          selected_count <- suppressWarnings(as.integer(parts[selection_cursor]))
          selection_cursor <- selection_cursor + 1L
          if (is.finite(selected_count) && selected_count >= 0L &&
              length(parts) >= selection_cursor + selected_count - 1L) {
            selected_rows <- if (selected_count > 0L) {
              suppressWarnings(as.integer(parts[seq.int(
                selection_cursor, selection_cursor + selected_count - 1L)]))
            } else integer()
            selected_rows <- selected_rows[is.finite(selected_rows) & selected_rows > 0L]
            selection_cursor <- selection_cursor + selected_count
          }
        }
        row_colors <- NULL
        if (length(parts) >= selection_cursor) {
          row_color_count <- suppressWarnings(as.integer(parts[selection_cursor]))
          selection_cursor <- selection_cursor + 1L
          if (is.finite(row_color_count) && row_color_count >= 0L &&
              length(parts) >= selection_cursor + 2L * row_color_count - 1L) {
            row_colors <- character()
            if (row_color_count > 0L) {
              for (index in seq_len(row_color_count)) {
                row <- parts[selection_cursor]
                color <- parts[selection_cursor + 1L]
                selection_cursor <- selection_cursor + 2L
                if (nzchar(row) && nzchar(color)) row_colors[[row]] <- color
              }
            }
          }
        }
        fit_method <- "loess"
        confidence_level <- 0.95
        if (length(parts) >= selection_cursor &&
            identical(parts[selection_cursor], "FIT_CURVE_V2")) {
          fit_method <- parts[selection_cursor + 1L]
          confidence_level <- suppressWarnings(as.double(parts[selection_cursor + 2L]))
          if (!fit_method %in% c("loess", "lm")) fit_method <- "loess"
          if (!is.finite(confidence_level) || confidence_level <= 0 || confidence_level >= 1)
            confidence_level <- 0.95
          selection_cursor <- selection_cursor + 3L
        }
        explicit_points <- NULL
        if (length(parts) >= selection_cursor &&
            identical(parts[selection_cursor], "EXPLICIT_POINTS_V1")) {
          point_count <- suppressWarnings(as.integer(parts[selection_cursor + 1L]))
          explicit_points <- list(rows = integer(), x = numeric(), y = numeric())
          first_point <- selection_cursor + 2L
          if (is.finite(point_count) && point_count > 0L &&
              length(parts) >= first_point + 3L * point_count - 1L) {
            indices <- seq.int(first_point, by = 3L, length.out = point_count)
            explicit_points <- list(
              rows = suppressWarnings(as.integer(parts[indices])),
              x = suppressWarnings(as.double(parts[indices + 1L])),
              y = suppressWarnings(as.double(parts[indices + 2L]))
            )
          }
        }
        .rls_handle_trellis_smooth_needed(
          parts[2L], parts[3L], parts[4L], parts[5L], parts[6L], parts[7L],
          suppressWarnings(as.double(parts[8L])), panel_rows, panel_colors,
          selected_rows, row_colors, fit_method, confidence_level,
          explicit_points
        )
      }
    } else if (startsWith(line, "IMPORT_DATA_NEEDED\t")) {
      parts <- strsplit(line, "\t", fixed = TRUE)[[1L]]
      if (length(parts) >= 2L) {
        source_path <- if (length(parts) >= 3L) parts[[3L]] else ""
        remove_after <- length(parts) >= 4L && identical(parts[[4L]], "1")
        .rls_handle_import_data_needed(parts[[2L]], source_path, remove_after)
      }
    } else if (startsWith(line, "IMPORT_DATA_COMMIT_NEEDED\t")) {
      .rls_handle_import_data_commit_needed(strsplit(line, "\t", fixed = TRUE)[[1L]])
    } else if (startsWith(line, "R_DATA_RETURN_NEEDED\t")) {
      .rls_handle_r_data_return_needed(strsplit(line, "\t", fixed = TRUE)[[1L]])
    } else if (startsWith(line, "R_DATA_CHOICE_NEEDED\t")) {
      .rls_handle_r_data_choice_needed(strsplit(line, "\t", fixed = TRUE)[[1L]])
    } else if (startsWith(line, "R_DATA_BROWSE_NEEDED\t")) {
      .rls_handle_r_data_browse_needed(strsplit(line, "\t", fixed = TRUE)[[1L]])
    } else if (startsWith(line, "WELCOME_ACTION_NEEDED\t")) {
      .rls_handle_welcome_action_needed(strsplit(line, "\t", fixed = TRUE)[[1L]])
    } else if (startsWith(line, "R_DATA_ASSIGN_NEEDED\t")) {
      .rls_handle_r_data_assign_needed(strsplit(line, "\t", fixed = TRUE)[[1L]])
    } else if (startsWith(line, "R_DATASET_SYNC_NEEDED\t")) {
      .rls_handle_r_dataset_sync_needed(strsplit(line, "\t", fixed = TRUE)[[1L]])
    } else if (startsWith(line, "PLOT_EXPORT_NEEDED\t")) {
      .rls_handle_plot_export_needed(strsplit(line, "\t", fixed = TRUE)[[1L]])
    } else if (startsWith(line, "TRELLIS_PANEL_ANALYSIS_NEEDED\t")) {
      .rls_handle_trellis_panel_analysis_needed(strsplit(line, "\t", fixed = TRUE)[[1L]])
    } else if (startsWith(line, "COMPARE_MEANS_BATCH_NEEDED\t")) {
      parts <- strsplit(line, "\t", fixed = TRUE)[[1L]]
      .rls_handle_compare_means_batch_needed(parts)
    } else if (startsWith(line, "TABLE1_NEEDED\t")) {
      .rls_handle_table1_needed(strsplit(line, "\t", fixed = TRUE)[[1L]])
    } else if (startsWith(line, "COMPARE_MEANS_NEEDED|")) {
      parts <- strsplit(line, "|", fixed = TRUE)[[1L]]
      if (length(parts) >= 4L) {
        var2 <- if (length(parts) >= 5L) parts[5L] else ""
        group_var <- if (length(parts) >= 6L) parts[6L] else ""
        .rls_handle_compare_means_needed(parts[2L], parts[3L], parts[4L], var2, group_var)
      }
    } else if (startsWith(line, "GLM_NEEDED\t")) {
      parts <- strsplit(line, "\t", fixed = TRUE)[[1L]]
      .rls_handle_glm_needed(parts)
    } else if (startsWith(line, "MODEL_TRELLIS_NEEDED\t")) {
      parts <- strsplit(line, "\t", fixed = TRUE)[[1L]]
      .rls_handle_model_trellis_needed(parts)
    } else if (startsWith(line, "REGCMP_NEEDED\t")) {
      parts <- strsplit(line, "\t", fixed = TRUE)[[1L]]
      .rls_handle_regcmp_needed(parts)
    } else if (startsWith(line, "GCOMP_NEEDED\t")) {
      .rls_handle_generalized_comparison_needed(strsplit(line, "\t", fixed = TRUE)[[1L]])
    } else if (startsWith(line, "GGLM_NEEDED\t")) {
      parts <- strsplit(line, "\t", fixed = TRUE)[[1L]]
      request <- .rls_parse_generalized_glm_needed(parts)
      if (!is.null(request)) do.call(.rls_handle_generalized_glm_needed, request)
    } else if (startsWith(line, "MIXED_MODEL_NEEDED\t")) {
      parts <- strsplit(line, "\t", fixed = TRUE)[[1L]]
      .rls_handle_mixed_model_needed(parts)
    } else if (startsWith(line, "CORRELATION_NEEDED\t")) {
      .rls_handle_correlation_needed(strsplit(line, "\t", fixed = TRUE)[[1L]])
    } else if (startsWith(line, "DENDROGRAM_NEEDED\t")) {
      .rls_handle_dendrogram_needed(strsplit(line, "\t", fixed = TRUE)[[1L]])
    } else if (startsWith(line, "DENDRO_DISTANCE_MATRIX_NEEDED\t")) {
      .rls_handle_dendrogram_distance_matrix_needed(
        strsplit(line, "\t", fixed = TRUE)[[1L]])
    } else if (startsWith(line, "PCAFA_FACTOR_NEEDED\t")) {
      parts <- strsplit(line, "\t", fixed = TRUE)[[1L]]
      .rls_handle_dimensionality_needed(parts)
    } else if (startsWith(line, "DIMENSIONALITY_SAVE_SCORES_NEEDED\t")) {
      .rls_handle_dimensionality_save_scores_needed(
        strsplit(line, "\t", fixed = TRUE)[[1L]]
      )
    } else if (startsWith(line, "SCALE_ANALYSIS_NEEDED\t")) {
      .rls_handle_scale_analysis_needed(strsplit(line, "\t", fixed = TRUE)[[1L]])
    } else if (startsWith(line, "ANALYSIS_WORKFLOW_NEEDED\t")) {
      .rls_handle_analysis_workflow_needed(strsplit(line, "\t", fixed = TRUE)[[1L]])
    } else if (startsWith(line, "MI_DIAGNOSTICS_NEEDED\t")) {
      parts <- strsplit(line, "\t", fixed=TRUE)[[1L]]
      tryCatch(ls_imputation_diagnostics(parts[[2L]], section=if(length(parts)>2L) sub("_all$", "", parts[[3L]]) else "summary",
        variable=if(length(parts)>3L && nzchar(parts[[4L]]))parts[[4L]] else NULL,
        max_patterns=if(length(parts)>2L && parts[[3L]]=="patterns_all")Inf else 20L,
        imputation_start=if(length(parts)>4L)as.integer(parts[[5L]]) else 1L),
        error=function(e) .rls_send(c("MI_DIAGNOSTICS_ERROR",.rls_native_wire_value(conditionMessage(e)))))
    } else if (startsWith(line, "MULTIPLE_IMPUTATION_NEEDED\t")) {
      .rls_handle_multiple_imputation_needed(strsplit(line, "\t", fixed = TRUE)[[1L]])
    }
  }
}

.rls_handle_multiple_imputation_needed <- function(parts) {
  if (length(parts) < 9L) return(invisible(FALSE))
  group <- parts[[2L]]
  m <- suppressWarnings(as.integer(parts[[3L]]))
  maxit <- suppressWarnings(as.integer(parts[[4L]]))
  seed_text <- parts[[5L]]
  open_data <- identical(parts[[6L]], "TRUE")
  cursor <- 7L

  take_strings <- function() {
    count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[[cursor]])) else NA_integer_
    cursor <<- cursor + 1L
    if (!is.finite(count) || count < 0L ||
        (count > 0L && length(parts) < cursor + count - 1L)) {
      stop("The multiple-imputation request is malformed.", call. = FALSE)
    }
    values <- if (count > 0L) parts[seq.int(cursor, cursor + count - 1L)] else character()
    cursor <<- cursor + count
    values
  }

  outcome <- tryCatch({
    if (!is.finite(m) || m < 1L || !is.finite(maxit) || maxit < 0L) {
      stop("The imputation and iteration counts are not valid.", call. = FALSE)
    }
    impute <- take_strings()
    predictors <- take_strings()
    method_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[[cursor]])) else NA_integer_
    cursor <- cursor + 1L
    if (!is.finite(method_count) || method_count < 0L ||
        (method_count > 0L && length(parts) < cursor + 2L * method_count - 1L)) {
      stop("The multiple-imputation method request is malformed.", call. = FALSE)
    }
    methods <- character()
    if (method_count > 0L) {
      for (index in seq_len(method_count)) {
        methods[[parts[[cursor]]]] <- parts[[cursor + 1L]]
        cursor <- cursor + 2L
      }
    }
    seed <- if (nzchar(seed_text)) suppressWarnings(as.integer(seed_text)) else NULL
    if (nzchar(seed_text) && (!is.finite(seed))) {
      stop("The random seed must be an integer.", call. = FALSE)
    }
    imputation <- ls_new_missing_data_imputation(
      data = group,
      impute = impute,
      predictors = predictors,
      m = m,
      maxit = maxit,
      seed = seed,
      method = methods,
      name = paste(group, "imputed")
    )
    result <- ls_run_imputation(imputation, open_data = open_data, make_active = TRUE)
    list(ok = TRUE, group = result$group, message = sprintf(
      "Created imputed dataset `%s` with %d imputations.", result$group, m
    ))
  }, error = function(error) list(ok = FALSE, group = "", message = conditionMessage(error)))

  try(.rls_send(c(
    "MULTIPLE_IMPUTATION_RESULT",
    if (outcome$ok) "ok" else "error",
    outcome$group,
    gsub("[[:cntrl:]]+", " ", outcome$message)
  )), silent = TRUE)
  if (!outcome$ok) warning(outcome$message, call. = FALSE)
  invisible(outcome$ok)
}

.rls_handle_generalized_comparison_needed <- function(parts) {
  if (length(parts) < 12L) return(invisible(FALSE))
  comparison_id <- parts[[2L]]
  group <- parts[[3L]]
  response <- parts[[4L]]
  family <- parts[[5L]]
  link <- parts[[6L]]
  scope <- parts[[7L]]
  binary <- identical(parts[[8L]], "BINARY")
  count_comparison <- identical(parts[[8L]], "COUNT")
  event <- parts[[9L]]
  reference <- parts[[10L]]
  cursor <- 11L

  type_count <- suppressWarnings(as.integer(parts[[cursor]])); cursor <- cursor + 1L
  if (!is.finite(type_count) || type_count < 0L) return(invisible(FALSE))
  term_types <- list()
  if (type_count > 0L) {
    if (length(parts) < cursor + 2L * type_count - 1L) return(invisible(FALSE))
    for (i in seq_len(type_count)) {
      term_types[[parts[[cursor]]]] <- parts[[cursor + 1L]]
      cursor <- cursor + 2L
    }
  }

  if (length(parts) < cursor) return(invisible(FALSE))
  model_count <- suppressWarnings(as.integer(parts[[cursor]])); cursor <- cursor + 1L
  minimum_models <- 1L
  if (!is.finite(model_count) || model_count < minimum_models) return(invisible(FALSE))
  model_ids <- labels <- character(model_count)
  model_terms <- vector("list", model_count)
  for (i in seq_len(model_count)) {
    if (length(parts) < cursor + 2L) return(invisible(FALSE))
    model_ids[[i]] <- parts[[cursor]]
    labels[[i]] <- parts[[cursor + 1L]]
    term_count <- suppressWarnings(as.integer(parts[[cursor + 2L]]))
    cursor <- cursor + 3L
    if (!is.finite(term_count) || term_count < 0L ||
        (term_count > 0L && length(parts) < cursor + term_count - 1L)) return(invisible(FALSE))
    model_terms[[i]] <- if (term_count > 0L)
      parts[seq.int(cursor, cursor + term_count - 1L)] else character()
    cursor <- cursor + term_count
  }

  row_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[[cursor]])) else 0L
  cursor <- cursor + 1L
  rows <- if (is.finite(row_count) && row_count > 0L &&
              length(parts) >= cursor + row_count - 1L)
    suppressWarnings(as.integer(parts[seq.int(cursor, cursor + row_count - 1L)])) else integer()
  rows <- rows[is.finite(rows) & rows > 0L]
  cursor <- cursor + if (is.finite(row_count) && row_count > 0L) row_count else 0L
  selected_rows <- rows

  generation <- 0L
  auto_refit <- TRUE
  requested_dataset_type <- ""
  requested_imputation_id <- ""
  requested_source_dataset_id <- ""
  requested_imputation_count <- 0L
  model_type <- if (binary) "binary" else if (count_comparison) "count" else "legacy_generalized"
  model_specs <- lapply(seq_len(model_count), function(i) list(
    id = model_ids[[i]], response = response, family = family, link = link,
    scope = scope, terms = model_terms[[i]], term_types = term_types,
    centered_predictors = character(), factor_reference_levels = list(),
    count_regression = count_comparison, count_distribution = "poisson", exposure = "", offset = "",
    trials_variable = "", trials_constant = NA_real_,
    specification_revision = 0L, specification_fingerprint = ""
  ))
  spec_version <- if (length(parts) >= cursor) parts[[cursor]] else ""
  if (spec_version %in% c("GCOMP_SPEC_V2", "GCOMP_SPEC_V3", "GCOMP_SPEC_V4", "GCOMP_SPEC_V5", "GCOMP_SPEC_V6", "GCOMP_SPEC_V7", "GCOMP_SPEC_V8")) {
    cursor <- cursor + 1L
    generation <- suppressWarnings(as.integer(parts[[cursor]])); cursor <- cursor + 1L
    if (!is.finite(generation) || generation < 0L) generation <- 0L
    auto_refit <- identical(parts[[cursor]], "TRUE"); cursor <- cursor + 1L
    if (spec_version %in% c("GCOMP_SPEC_V3", "GCOMP_SPEC_V4", "GCOMP_SPEC_V5", "GCOMP_SPEC_V6", "GCOMP_SPEC_V7", "GCOMP_SPEC_V8")) {
      count_comparison <- identical(parts[[cursor]], "TRUE")
      cursor <- cursor + 1L
    }
    if (spec_version %in% c("GCOMP_SPEC_V6", "GCOMP_SPEC_V7", "GCOMP_SPEC_V8")) {
      model_type <- parts[[cursor]]
      cursor <- cursor + 1L
      if (!model_type %in% names(.rls_model_type_catalogue)) {
        stop("The generalized comparison model type is invalid.", call. = FALSE)
      }
    }
    if (spec_version %in% c("GCOMP_SPEC_V5", "GCOMP_SPEC_V6", "GCOMP_SPEC_V7", "GCOMP_SPEC_V8")) {
      if (length(parts) < cursor + 3L) {
        stop("The generalized comparison dataset identity is incomplete.", call. = FALSE)
      }
      requested_dataset_type <- parts[[cursor]]
      requested_imputation_id <- parts[[cursor + 1L]]
      requested_source_dataset_id <- parts[[cursor + 2L]]
      requested_imputation_count <- suppressWarnings(as.integer(parts[[cursor + 3L]]))
      cursor <- cursor + 4L
      if (!nzchar(requested_dataset_type) ||
          !is.finite(requested_imputation_count) || requested_imputation_count < 0L) {
        stop("The generalized comparison dataset identity is invalid.", call. = FALSE)
      }
    }
    spec_count <- suppressWarnings(as.integer(parts[[cursor]])); cursor <- cursor + 1L
    if (!is.finite(spec_count) || spec_count != model_count) return(invisible(FALSE))

    take_strings <- function() {
      count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[[cursor]])) else NA_integer_
      cursor <<- cursor + 1L
      if (!is.finite(count) || count < 0L ||
          (count > 0L && length(parts) < cursor + count - 1L)) {
        stop("The generalized comparison model specification is malformed.", call. = FALSE)
      }
      values <- if (count > 0L) parts[seq.int(cursor, cursor + count - 1L)] else character()
      cursor <<- cursor + count
      values
    }
    take_named <- function() {
      count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[[cursor]])) else NA_integer_
      cursor <<- cursor + 1L
      if (!is.finite(count) || count < 0L ||
          (count > 0L && length(parts) < cursor + 2L * count - 1L)) {
        stop("The generalized comparison model metadata are malformed.", call. = FALSE)
      }
      values <- list()
      if (count > 0L) for (j in seq_len(count)) {
        key <- parts[[cursor]]; value <- parts[[cursor + 1L]]; cursor <<- cursor + 2L
        if (nzchar(key)) values[[key]] <- value
      }
      values
    }
    model_specs <- lapply(seq_len(model_count), function(i) {
      if (length(parts) < cursor + 4L) {
        stop("The generalized comparison model specification is incomplete.", call. = FALSE)
      }
      spec <- list(
        id = parts[[cursor]], response = parts[[cursor + 1L]],
        family = parts[[cursor + 2L]], link = parts[[cursor + 3L]],
        scope = parts[[cursor + 4L]]
      )
      cursor <<- cursor + 5L
      if (spec_version %in% c("GCOMP_SPEC_V3", "GCOMP_SPEC_V4", "GCOMP_SPEC_V5", "GCOMP_SPEC_V6", "GCOMP_SPEC_V7", "GCOMP_SPEC_V8")) {
        if (length(parts) < cursor + 2L) {
          stop("The count-comparison model specification is incomplete.", call. = FALSE)
        }
        spec$count_regression <- identical(parts[[cursor]], "TRUE")
        spec$count_distribution <- parts[[cursor + 1L]]
        spec$exposure <- parts[[cursor + 2L]]
        cursor <<- cursor + 3L
        spec$offset <- if (identical(spec_version, "GCOMP_SPEC_V8")) {
          value <- parts[[cursor]]
          cursor <<- cursor + 1L
          value
        } else ""
        if (spec_version %in% c("GCOMP_SPEC_V7", "GCOMP_SPEC_V8")) {
          if (length(parts) < cursor + 1L) {
            stop("The bounded-count comparison trials specification is incomplete.", call. = FALSE)
          }
          spec$trials_variable <- parts[[cursor]]
          spec$trials_constant <- suppressWarnings(as.numeric(parts[[cursor + 1L]]))
          cursor <<- cursor + 2L
        } else {
          spec$trials_variable <- ""
          spec$trials_constant <- NA_real_
        }
      } else {
        spec$count_regression <- count_comparison
        spec$count_distribution <- "poisson"
        spec$exposure <- ""
        spec$offset <- ""
      }
      if (spec_version %in% c("GCOMP_SPEC_V6", "GCOMP_SPEC_V7", "GCOMP_SPEC_V8")) {
        if (length(parts) < cursor + 2L) {
          stop("The bounded-response comparison specification is incomplete.", call. = FALSE)
        }
        bounds_configured <- identical(parts[[cursor]], "TRUE")
        lower <- suppressWarnings(as.numeric(parts[[cursor + 1L]]))
        upper <- suppressWarnings(as.numeric(parts[[cursor + 2L]]))
        cursor <<- cursor + 3L
        spec$response_bounds <- if (bounds_configured) {
          if (!is.finite(lower) || !is.finite(upper) || lower >= upper) {
            stop("The bounded-response comparison limits are invalid.", call. = FALSE)
          }
          c(lower, upper)
        } else NULL
      } else {
        spec$response_bounds <- NULL
      }
      if (spec_version %in% c("GCOMP_SPEC_V4", "GCOMP_SPEC_V5", "GCOMP_SPEC_V6", "GCOMP_SPEC_V7", "GCOMP_SPEC_V8")) {
        if (length(parts) < cursor + 1L) {
          stop("The generalized comparison specification identity is incomplete.", call. = FALSE)
        }
        spec$specification_revision <- suppressWarnings(as.integer(parts[[cursor]]))
        spec$specification_fingerprint <- parts[[cursor + 1L]]
        cursor <<- cursor + 2L
        if (!is.finite(spec$specification_revision) || spec$specification_revision < 1L ||
            !nzchar(spec$specification_fingerprint)) {
          stop("The generalized comparison specification identity is invalid.", call. = FALSE)
        }
      } else {
        spec$specification_revision <- 0L
        spec$specification_fingerprint <- ""
      }
      spec$terms <- take_strings()
      spec$term_types <- take_named()
      spec$centered_predictors <- take_strings()
      spec$factor_reference_levels <- take_named()
      spec
    })
  }

  tryCatch({
    if (spec_version %in% c("GCOMP_SPEC_V5", "GCOMP_SPEC_V6", "GCOMP_SPEC_V7", "GCOMP_SPEC_V8") && generation > 0L &&
        isTRUE(.rls_state$process_started)) {
      .rls_send_native_analysis_result(c(
        "GCOMP_TASK_RECEIVED",
        .rls_native_wire_value(comparison_id),
        .rls_native_wire_integer(generation)
      ), "generalized-comparison task acknowledgement")
    }
    dataset <- .rls_dataset_record(group)
    if (spec_version %in% c("GCOMP_SPEC_V5", "GCOMP_SPEC_V6", "GCOMP_SPEC_V7", "GCOMP_SPEC_V8")) {
      actual_dataset_type <- dataset$dataset_type %||% "data_frame"
      actual_imputation_id <- dataset$imputation_id %||% ""
      actual_source_dataset_id <- dataset$source_dataset_id %||% ""
      actual_imputation_count <- if (identical(actual_dataset_type, "multiple_imputation"))
        as.integer(dataset$imputation_count %||% length(dataset$completed_datasets %||% list())) else 0L
      if (!identical(requested_dataset_type, actual_dataset_type) ||
          !identical(requested_imputation_id, actual_imputation_id) ||
          !identical(requested_source_dataset_id, actual_source_dataset_id) ||
          !identical(as.integer(requested_imputation_count), actual_imputation_count)) {
        stop("The generalized comparison request belongs to a different dataset/imputation revision.", call. = FALSE)
      }
      if (identical(actual_dataset_type, "multiple_imputation") &&
          length(.rls_mi_completed_datasets(dataset)) != actual_imputation_count) {
        stop("The registered multiple-imputation dataset set is incomplete.", call. = FALSE)
      }
    }
    comparison_rows <- if (identical(
      .rls_analysis_backend(dataset, "generalized_comparison"),
      "multiple_imputation"
    )) {
      .rls_mi_common_model_rows(dataset, model_specs, selected_rows %||% integer())
    } else {
      integer()
    }
    models <- vector("list", model_count)
    for (i in seq_len(model_count)) {
      spec <- model_specs[[i]]
      if (binary) {
        models[[i]] <- ls_new_binary_regression(
          group, spec$response, spec$terms, link = spec$link,
          event = event, reference = reference, scope = spec$scope,
          name = spec$id, native = FALSE, term_types = spec$term_types,
          centered_predictors = spec$centered_predictors,
          factor_reference_levels = spec$factor_reference_levels,
          offset = if (nzchar(spec$offset %||% "")) spec$offset else NULL,
          .selected_rows = selected_rows,
          .comparison_rows = comparison_rows
        )
      } else if (isTRUE(spec$count_regression) || count_comparison) {
        models[[i]] <- ls_new_count_regression(
          group, spec$response, spec$terms,
          distribution = spec$count_distribution,
          exposure = if (nzchar(spec$exposure)) spec$exposure else NULL,
          offset = if (nzchar(spec$offset %||% "")) spec$offset else NULL,
          trials = if (nzchar(spec$trials_variable %||% "")) spec$trials_variable else spec$trials_constant,
          scope = spec$scope, name = spec$id, native = FALSE,
          term_types = spec$term_types,
          centered_predictors = spec$centered_predictors,
          factor_reference_levels = spec$factor_reference_levels,
          .selected_rows = selected_rows,
          .allow_intercept_only = TRUE,
          .comparison_rows = comparison_rows
        )
      } else {
        models[[i]] <- ls_new_generalized_linear_model(
          group, spec$response, spec$terms, family = spec$family, link = spec$link,
          offset = if (nzchar(spec$offset %||% "")) spec$offset else NULL,
          .model_type = model_type,
          response_bounds = spec$response_bounds,
          scope = spec$scope, name = spec$id, native = FALSE,
          term_types = spec$term_types,
          centered_predictors = spec$centered_predictors,
          factor_reference_levels = spec$factor_reference_levels,
          .selected_rows = selected_rows,
          .allow_intercept_only = TRUE,
          .comparison_rows = comparison_rows
        )
      }
      fitted_record <- .rls_generalized_glm_record(models[[i]])
      fitted_record$fit_specification_witness <-
        .rls_assert_generalized_fit_matches_specification(
          fitted_record, spec,
          event = if (binary) event else NULL,
          reference = if (binary) reference else NULL
        )
      models[[i]] <- .rls_assign_generalized_glm(fitted_record)
    }
    if (binary) {
      ls_compare_binary_regression_models(models, .native_id = comparison_id,
                                          native = TRUE,
                                          .native_labels = labels,
                                          .native_generation = generation,
                                          .native_specification_revisions = vapply(model_specs, `[[`, integer(1L), "specification_revision"),
                                          .native_specification_fingerprints = vapply(model_specs, `[[`, character(1L), "specification_fingerprint"))
    } else if (count_comparison) {
      ls_compare_count_regression_models(models, .native_id = comparison_id,
                                         native = TRUE,
                                         .native_labels = labels,
                                         .native_generation = generation,
                                         .native_specification_revisions = vapply(model_specs, `[[`, integer(1L), "specification_revision"),
                                         .native_specification_fingerprints = vapply(model_specs, `[[`, character(1L), "specification_fingerprint"))
    } else {
      ls_compare_generalized_linear_models(models, .native_id = comparison_id,
                                           native = TRUE,
                                           .model_type = model_type,
                                           .native_labels = labels,
                                           .native_generation = generation,
                                           .native_specification_revisions = vapply(model_specs, `[[`, integer(1L), "specification_revision"),
                                           .native_specification_fingerprints = vapply(model_specs, `[[`, character(1L), "specification_fingerprint"))
    }
    invisible(TRUE)
  }, error = function(e) {
    message_text <- conditionMessage(e)
    install_accepted <- .rls_offer_missing_packages_native(
      e, "generalized_comparison", comparison_id, group
    )
    if (isTRUE(install_accepted)) return(invisible(FALSE))
    if (isTRUE(.rls_state$process_started)) {
      tryCatch(
        .rls_send_native_analysis_result(c(
          "GENERALIZED_COMPARISON_UPDATE_ERROR",
          .rls_native_wire_value(comparison_id),
          .rls_native_wire_integer(generation),
          .rls_native_wire_value(message_text)
        ), "generalized-comparison error"),
        error = function(send_error) {
          message("LinkEDA could not deliver the generalized-comparison error: ",
                  conditionMessage(send_error))
        }
      )
    }
    warning(sprintf("Could not refit model comparison: %s", message_text), call. = FALSE)
    invisible(FALSE)
  })
}

.rls_handle_analysis_workflow_needed <- function(parts) {
  if (length(parts) < 12L) return(invisible(FALSE))
  kind <- parts[[2L]]
  group <- parts[[3L]]
  response <- parts[[4L]]
  secondary <- parts[[5L]]
  group_variable <- parts[[6L]]
  family <- parts[[7L]]
  link <- parts[[8L]]
  method <- parts[[9L]]
  row_condition <- parts[[10L]]
  column_condition <- parts[[11L]]
  variable_count <- suppressWarnings(as.integer(parts[[12L]]))
  if (!is.finite(variable_count) || variable_count < 0L) return(invisible(FALSE))
  cursor <- 13L
  variables <- character()
  if (variable_count > 0L && length(parts) >= cursor + variable_count - 1L) {
    variables <- parts[seq.int(cursor, cursor + variable_count - 1L)]
    cursor <- cursor + variable_count
  }
  row_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[[cursor]])) else 0L
  cursor <- cursor + 1L
  rows <- if (is.finite(row_count) && row_count > 0L && length(parts) >= cursor + row_count - 1L) {
    suppressWarnings(as.integer(parts[seq.int(cursor, cursor + row_count - 1L)]))
  } else integer()
  rows <- rows[is.finite(rows) & rows > 0L]
  cursor <- cursor + if (is.finite(row_count) && row_count > 0L) row_count else 0L
  preferred_id <- if (length(parts) >= cursor) parts[[cursor]] else ""
  if (identical(kind, "install_r_packages")) {
    outcome <- tryCatch({
      library <- .rls_install_optional_packages(variables)
      list(
        ok = TRUE,
        message = sprintf(
          "Installed %s in LinkEDA's private R library `%s`.",
          paste(variables, collapse = ", "), library
        )
      )
    }, error = function(e) list(ok = FALSE, message = conditionMessage(e)))
    if (isTRUE(.rls_state$process_started)) {
      .rls_send_native_analysis_result(c(
        "R_PACKAGE_INSTALL_RESULT",
        .rls_native_wire_value(response),
        .rls_native_wire_value(preferred_id),
        .rls_native_wire_value(group),
        if (isTRUE(outcome$ok)) "ok" else "error",
        .rls_native_wire_value(outcome$message),
        as.character(length(variables)),
        .rls_native_wire_value(variables)
      ), "optional R package installation result", timeout = 300)
    }
    return(invisible(isTRUE(outcome$ok)))
  }
  scope <- if (length(rows)) "selected" else "all"
  selected_rows <- rows
  # Freeze the queued workflow scope for nested public R entry points.
  previous_scope_request <- .rls_state$analysis_scope_request
  on.exit(.rls_state$analysis_scope_request <- previous_scope_request, add=TRUE)
  .rls_state$analysis_scope_request <- .rls_capture_analysis_scope(.rls_dataset_record(group),scope,rows)
  base_terms <- variables[seq_len(min(1L, length(variables)))]
  extended_terms <- variables

  tryCatch({
    if (kind == "scale_save_total") {
      analysis <- .rls_scale_record(response)
      if (!identical(analysis$group, group) ||
          !identical(as.integer(analysis$model_version), as.integer(row_condition)) ||
          !identical(analysis$specification_fingerprint, column_condition))
        stop("The scale changed. Wait for it to finish updating before saving scores.", call. = FALSE)
      saved <- ls_scale_analysis_save_total(response, method = method)
      if (identical(secondary, "histogram")) {
        ls_new_histogram(group, x = saved)
      } else .rls_send(c("WORKBENCH_MESSAGE", "Scale scores", paste("Added", saved, "to", group)))
    } else if (kind == "missing_data_action") {
      .rls_missing_pattern_action(response, secondary, row_condition, native=TRUE)
    } else if (kind %in% c("missing_data_overview", "missingness_model")) {
      .rls_handle_missing_data_workflow(kind, group, response, secondary, variables,
        row_condition, method, column_condition, family, rows)
    } else if (identical(kind, "contingency")) {
      result <- .rls_mi_contingency_record(group, variables, group_variable,
        id = if (nzchar(preferred_id)) preferred_id else NULL,
        selected_rows = .rls_state$analysis_scope_request$rows,
        display_mode = if(nzchar(method)) method else "count_percent")
      .rls_table1_sync_native(result)
    } else if (identical(kind, "correlation_matrix")) {
      ls_new_correlation_matrix(group, variables = variables)
    } else if (identical(kind, "dimensionality")) {
      ls_new_dimensionality(group, variables = variables,
                            method = if (identical(method, "factor")) "factor" else "pca",
                            n_components = min(2L, length(variables)), scope = scope,
                            selected_rows = selected_rows)
    } else if (identical(kind, "quick_cluster")) {
      ls_new_quick_cluster(group, variables = variables)
    } else if (identical(kind, "one_sample_t")) {
      ls_new_one_sample_t_test(group, response, scope = scope,
                               .selected_rows = selected_rows)
    } else if (identical(kind, "independent_t")) {
      ls_new_independent_samples_t_test(group, response, group_variable,
                                        scope = scope, .selected_rows = selected_rows)
    } else if (identical(kind, "paired_t")) {
      ls_new_paired_samples_t_test(group, list(c(response, secondary)),
                                   scope = scope, .selected_rows = selected_rows)
    } else if (identical(kind, "oneway_anova")) {
      ls_new_one_way_anova(group, response, group_variable, scope = scope,
                           .selected_rows = selected_rows)
    } else if (identical(kind, "linear_model")) {
      model <- ls_new_glm(group)
      model <- ls_glm_set_dependent(model, response)
      for (term in variables) model <- ls_glm_add_predictor(model, term)
    } else if (identical(kind, "linear_model_trellis")) {
      ls_new_model_trellis(group, response, terms = variables,
                           columns = column_condition,
                           rows = if (nzchar(row_condition)) row_condition else NULL)
    } else if (identical(kind, "regression_comparison")) {
      ls_new_regression_comparison(group, response,
        list(Base = base_terms, Extended = extended_terms), scope = scope)
    } else if (identical(kind, "binary_regression")) {
      ls_new_binary_regression(group, response, variables, link = link,
                               scope = scope, .selected_rows = selected_rows)
    } else if (identical(kind, "binary_regression_comparison")) {
      first <- ls_new_binary_regression(group, response, character(), link = link,
                                        scope = scope, .selected_rows = selected_rows)
      second <- ls_new_binary_regression(group, response, extended_terms, link = link,
                                         scope = scope, .selected_rows = selected_rows)
      ls_compare_binary_regression_models(first, second, .native_id = preferred_id)
    } else if (identical(kind, "generalized_linear_model")) {
      ls_new_generalized_linear_model(group, response, variables, family = family,
                                      link = link, scope = scope,
                                      .selected_rows = selected_rows)
    } else if (identical(kind, "generalized_comparison")) {
      first <- ls_new_generalized_linear_model(group, response, character(),
                                                 family = family, link = link,
                                                 scope = scope, .selected_rows = selected_rows,
                                                 .allow_intercept_only = TRUE)
      second <- ls_new_generalized_linear_model(group, response, extended_terms,
                                                 family = family, link = link,
                                                 scope = scope, .selected_rows = selected_rows)
      ls_compare_generalized_linear_models(first, second, .native_id = preferred_id)
    } else if (identical(kind, "linear_mixed_model")) {
      ls_new_linear_mixed_model(group, response, fixed = variables,
        random = list(list(group = group_variable, terms = "1")), scope = scope,
        .selected_rows = selected_rows)
    } else if (identical(kind, "generalized_mixed_model")) {
      ls_new_generalized_mixed_model(group, response, fixed = variables,
        random = list(list(group = group_variable, terms = "1")), family = family,
        link = link, scope = scope, .selected_rows = selected_rows)
    }
    invisible(TRUE)
  }, error = function(e) {
    if (kind == "scale_save_total")
      .rls_send(c("WORKBENCH_MESSAGE", "Scale scores", .rls_native_wire_value(conditionMessage(e))))
    else if(kind %in% c("missing_data_overview","missingness_model","missing_data_action"))
      .rls_send(c("MISSING_DATA_ERROR",.rls_native_wire_value(conditionMessage(e))))
    else warning(sprintf("Could not run %s: %s", kind, conditionMessage(e)), call. = FALSE)
    invisible(FALSE)
  })
}

.rls_cleanup_staged_import_file <- function(path, remove_after = FALSE) {
  if (!isTRUE(remove_after) || !nzchar(path)) return(invisible(FALSE))
  staged_directory <- dirname(path)
  canonical_path <- function(value) {
    normalized <- normalizePath(value, winslash = "/", mustWork = FALSE)
    if (.Platform$OS.type == "windows") tolower(normalized) else normalized
  }
  normalized_staging <- canonical_path(staged_directory)
  temporary_roots <- unique(vapply(
    c(tempdir(), Sys.getenv("TMPDIR", unset = dirname(tempdir()))),
    canonical_path, character(1L)
  ))
  safe <- startsWith(basename(staged_directory), "rlispstat-import-") &&
    any(vapply(temporary_roots, function(root) {
      identical(dirname(normalized_staging), root) ||
        startsWith(paste0(normalized_staging, "/"), paste0(root, "/"))
    }, logical(1L)))
  if (safe) unlink(staged_directory, recursive = TRUE, force = TRUE)
  invisible(safe)
}

.rls_discard_staged_import <- function(dataset) {
  if (!exists(dataset, envir = .rls_state$datasets, inherits = FALSE))
    return(invisible(FALSE))
  record <- get(dataset, envir = .rls_state$datasets, inherits = FALSE)
  imputation_id <- record$imputation_id %||% ""
  rm(list = dataset, envir = .rls_state$datasets)
  if (nzchar(imputation_id) &&
      exists(imputation_id, envir = .rls_state$missing_imputations, inherits = FALSE))
    rm(list = imputation_id, envir = .rls_state$missing_imputations)
  invisible(TRUE)
}

.rls_handle_import_data_needed <- function(path, source_path = "", remove_after = FALSE) {
  clean_message <- function(x) gsub("[\r\n\t|]+", " ", as.character(x))
  tryCatch({
    import_name <- if (nzchar(source_path)) {
      tools::file_path_sans_ext(basename(source_path))
    } else NULL
    dataset <- ls_import_data(path, name = import_name, make_active = FALSE)
    recent_path <- if (nzchar(source_path)) source_path else path
    record <- get(dataset, envir = .rls_state$datasets, inherits = FALSE)
    assign(dataset, list(path = path, source_path = source_path,
      recent_path = recent_path, import_name = import_name,
      remove_after = isTRUE(remove_after)), envir = .rls_state$pending_imports)
    variables <- names(record$data)
    .rls_send(c("IMPORT_DATA_PREVIEW", dataset,
      clean_message(paste(sprintf("Choose variables from `%s`.", basename(recent_path)), record$import_notice %||% "")),
      normalizePath(recent_path, mustWork = FALSE),
      as.character(length(variables)), variables))
  }, error = function(e) {
    .rls_cleanup_staged_import_file(path, remove_after)
    .rls_send(c("IMPORT_DATA_RESULT", "error", clean_message(conditionMessage(e))))
  })
  invisible(NULL)
}

.rls_handle_import_data_commit_needed <- function(parts) {
  clean_message <- function(x) gsub("[\r\n\t|]+", " ", as.character(x))
  if (length(parts) < 4L) return(invisible(NULL))
  dataset <- parts[[2L]]
  cancelled <- identical(parts[[3L]], "1")
  count <- suppressWarnings(as.integer(parts[[4L]]))
  columns <- if (!cancelled && is.finite(count) && count > 0L &&
                 length(parts) >= 4L + count) parts[seq.int(5L, 4L + count)] else character()
  if (!exists(dataset, envir = .rls_state$pending_imports, inherits = FALSE)) {
    .rls_send(c("IMPORT_DATA_RESULT", "error", "The staged import is no longer available."))
    return(invisible(NULL))
  }
  pending <- get(dataset, envir = .rls_state$pending_imports, inherits = FALSE)
  rm(list = dataset, envir = .rls_state$pending_imports)
  if (cancelled) {
    .rls_discard_staged_import(dataset)
    .rls_cleanup_staged_import_file(pending$path, pending$remove_after)
    .rls_send(c("IMPORT_DATA_RESULT", "cancelled", "Import cancelled."))
    return(invisible(NULL))
  }
  if (!length(columns)) {
    .rls_discard_staged_import(dataset)
    .rls_cleanup_staged_import_file(pending$path, pending$remove_after)
    .rls_send(c("IMPORT_DATA_RESULT", "error", "Select at least one variable."))
    return(invisible(NULL))
  }
  tryCatch({
    staged_record <- .rls_dataset_record(dataset)
    reuse_preview <- identical(columns, names(staged_record$data))
    if (reuse_preview) {
      # The chooser initially selects every variable. Keep the already-read
      # dataset (and all completed mids versions) instead of reading the RDS
      # and reconstructing every imputation a second time.
      imported <- dataset
    } else {
      .rls_discard_staged_import(dataset)
      .rls_state$import_selected_columns <- columns
      on.exit({ .rls_state$import_selected_columns <- NULL }, add = TRUE)
      imported <- ls_import_data(pending$path, name = pending$import_name,
                                 make_active = FALSE)
    }
    record <- .rls_dataset_record(imported)
    import_warnings <- record$import_warnings %||% character()
    if (nzchar(pending$source_path)) {
      record$path <- normalizePath(pending$source_path, mustWork = FALSE)
      if (identical(record$dataset_type %||% "data_frame", "multiple_imputation")) {
        record$data_provenance$origin <- "reconstructed"
        record$data_provenance$origin_code <- .rls_mi_import_provenance_code(
          pending$source_path, record$source %||% "mice mids object",
          record$mids_object %||% NULL
        )
      } else {
        record$data_provenance$origin_code <- .rls_import_reconstruction_code(
          pending$source_path, record$source %||% "Imported data"
        )
      }
      .rls_set_dataset_record(record)
      .rls_store_data_version(record)
    }
    ls_set_active_dataset(imported)
    if (isTRUE(.rls_state$process_started) &&
        !.rls_native_dataset_matches(.rls_dataset_record(imported))) {
      stop("The imported data did not reach the data sheet. Try importing again.",
           call. = FALSE)
    }
    .rls_cleanup_staged_import_file(pending$path, pending$remove_after)
    import_message <- sprintf("Imported `%s` with %d variables.", imported, length(columns))
    if (length(import_warnings)) {
      import_message <- paste(
        import_message,
        "Review imported variable types:",
        paste(import_warnings, collapse = " ")
      )
    }
    .rls_send(c("IMPORT_DATA_RESULT", "ok",
      clean_message(import_message),
      normalizePath(pending$recent_path, mustWork = FALSE)))
  }, error = function(e) {
    .rls_cleanup_staged_import_file(pending$path, pending$remove_after)
    .rls_send(c("IMPORT_DATA_RESULT", "error", clean_message(conditionMessage(e))))
  })
  invisible(NULL)
}

.rls_handle_plot_export_needed <- function(parts) {
  if (length(parts) < 8L) return(invisible(NULL))
  request_id <- parts[[2L]]
  plot_id <- parts[[3L]]
  path <- parts[[4L]]
  format <- tolower(parts[[5L]])
  operation <- parts[[6L]]
  width <- suppressWarnings(as.numeric(parts[[7L]]))
  height <- suppressWarnings(as.numeric(parts[[8L]]))
  group <- if (length(parts) >= 9L) parts[[9L]] else ""
  plot_kind <- if (length(parts) >= 10L) parts[[10L]] else "scatter"
  x <- if (length(parts) >= 11L) parts[[11L]] else ""
  y <- if (length(parts) >= 12L) parts[[12L]] else ""
  title <- if (length(parts) >= 13L) parts[[13L]] else ""
  option_count <- if (length(parts) >= 14L) suppressWarnings(as.integer(parts[[14L]])) else 0L
  options <- list()
  cursor <- 15L
  if (is.finite(option_count) && option_count > 0L && length(parts) >= 14L + 2L * option_count) {
    for (index in seq_len(option_count)) {
      key <- parts[[cursor]]
      value <- parts[[cursor + 1L]]
      options[[key]] <- c(options[[key]], value)
      cursor <- cursor + 2L
    }
  }
  option_one <- function(name, default = "") {
    value <- options[[name]]
    if (is.null(value) || !length(value)) default else value[[1L]]
  }
  option_flag <- function(name, default = FALSE) {
    value <- option_one(name, if (default) "TRUE" else "FALSE")
    identical(toupper(value), "TRUE")
  }
  option_number <- function(name, default = NA_real_) {
    value <- suppressWarnings(as.double(option_one(name, as.character(default))))
    if (length(value) && is.finite(value[[1L]])) value[[1L]] else default
  }
  conditions <- lapply(options$condition %||% character(), function(value) {
    fields <- strsplit(value, "\037", fixed = TRUE)[[1L]]
    list(variable = fields[[1L]],
         kind = if (length(fields) >= 2L) fields[[2L]] else "categorical",
         dimension = if (length(fields) >= 3L) fields[[3L]] else "columns",
         method = if (length(fields) >= 4L) fields[[4L]] else "equal_width",
         bins = if (length(fields) >= 5L) suppressWarnings(as.integer(fields[[5L]])) else 4L)
  })
  clean <- function(value) gsub("[\r\n\t|]+", " ", as.character(value))
  status <- "ok"
  message <- "Export completed."
  tryCatch({
    if (!format %in% c("svg")) stop("The requested vector format is unavailable.", call. = FALSE)
    if (!is.finite(width) || width <= 0 || !is.finite(height) || height <= 0) {
      stop("The requested export dimensions are invalid.", call. = FALSE)
    }
    if (!exists(plot_id, envir = .rls_state$plots, inherits = FALSE)) {
      if (!nzchar(group)) stop("No local plot metadata are available for this plot.", call. = FALSE)
      dataset <- .rls_dataset_record(group)
      extra <- list()
      if (identical(plot_kind, "trellis_scatterplot")) {
        extra <- list(
          condition = if (length(conditions)) conditions[[1L]]$variable else "",
          conditions = conditions,
          plot_type = (options$plot_type %||% "scatter")[[1L]],
          layout = (options$layout %||% "automatic")[[1L]],
          scale_mode = (options$scale_mode %||% "common_xy")[[1L]],
          grouping = option_one("grouping"), split = option_one("split"),
          bar_measure = option_one("bar_measure", "count"),
          histogram_measure = option_one("histogram_measure", "count"),
          histogram_bins = max(1L, as.integer(option_number("histogram_bins", 10))),
          custom_title = nzchar(title)
        )
      } else if (identical(plot_kind, "time_series")) {
        extra <- list(series = option_one("series"), time_type = option_one("time_type", "numeric"),
                      identification = option_one("identification", "legend"),
                      legend_position = option_one("legend_position", "top_right"))
      } else if (identical(plot_kind, "scatter_matrix")) {
        extra <- list(variables = options$variable %||% character())
      } else if (identical(plot_kind, "boxplot")) {
        extra <- list(
          boxplot_variables = options$variable %||% character(),
          standardize = identical((options$standardize %||% "FALSE")[[1L]], "TRUE"),
          connect_rows = identical((options$connect_rows %||% "FALSE")[[1L]], "TRUE")
        )
      } else if (identical(plot_kind, "barplot")) {
        variables <- options$variable %||% character()
        if (length(variables)) x <- variables
        extra <- list(split = (options$split %||% "")[[1L]])
      }
      do.call(.rls_register_plot, c(list(
        plot_id = plot_id, group = group, x = x, y = y,
        data = dataset$data, rows = seq_len(nrow(dataset$data)),
        type = plot_kind, title = title
      ), extra))
    }
    # Native windows may already have registered the plot in R.  Refresh the
    # display metadata on every request so the SVG represents its current
    # overlays, smoothing, density and legend configuration rather than the
    # state at window creation time.
    record <- get(plot_id, envir = .rls_state$plots, inherits = FALSE)
    analysis_scope <- option_one("analysis_scope")
    if (nzchar(analysis_scope)) {
      base_title <- title %||% record$title %||% ""
      record$title <- paste(base_title, analysis_scope, sep = "\n")
      record$analysis_scope <- analysis_scope
    }
    record$lm <- option_flag("lm")
    record$smooth <- option_flag("smooth")
    record$smooth_span <- option_number("smooth_span", 0.75)
    record$shade_overlap <- option_flag("shade_overlap", TRUE)
    record$size_by_overlap <- option_flag("size_by_overlap")
    record$selection_export_policy <- option_one("selection", "excluded")
    split_option <- function(value) strsplit(value, "\037", fixed = TRUE)[[1L]]
    if (length(options$point %||% character())) {
      values <- lapply(options$point, split_option)
      record$render_points <- data.frame(
        x = vapply(values, function(v) suppressWarnings(as.double(v[[1L]])), numeric(1L)),
        y = vapply(values, function(v) suppressWarnings(as.double(v[[2L]])), numeric(1L)),
        row = vapply(values, function(v) if (length(v) >= 3L) suppressWarnings(as.integer(v[[3L]])) else NA_integer_, integer(1L))
      )
    }
    if (length(options$parallel_point %||% character())) {
      values <- lapply(options$parallel_point, split_option)
      record$parallel_points <- data.frame(
        x = vapply(values, function(v) suppressWarnings(as.double(v[[1L]])), numeric(1L)),
        y = vapply(values, function(v) suppressWarnings(as.double(v[[2L]])), numeric(1L))
      )
    }
    if (length(options$line_point %||% character())) {
      values <- lapply(options$line_point, split_option)
      record$line_points <- data.frame(
        label = vapply(values, function(v) v[[1L]], character(1L)),
        color = vapply(values, function(v) if (length(v) >= 2L) v[[2L]] else "black", character(1L)),
        x = vapply(values, function(v) if (length(v) >= 3L) suppressWarnings(as.double(v[[3L]])) else NA_real_, numeric(1L)),
        y = vapply(values, function(v) if (length(v) >= 4L) suppressWarnings(as.double(v[[4L]])) else NA_real_, numeric(1L))
      )
    }
    if (length(options$loading %||% character())) {
      values <- lapply(options$loading, split_option)
      record$loadings <- data.frame(
        variable = vapply(values, function(v) v[[1L]], character(1L)),
        x = vapply(values, function(v) if (length(v) >= 2L) suppressWarnings(as.double(v[[2L]])) else NA_real_, numeric(1L)),
        y = vapply(values, function(v) if (length(v) >= 3L) suppressWarnings(as.double(v[[3L]])) else NA_real_, numeric(1L))
      )
    }
    if (length(options$x_tick %||% character())) {
      values <- lapply(options$x_tick, split_option)
      record$x_ticks <- data.frame(
        at = vapply(values, function(v) suppressWarnings(as.double(v[[1L]])), numeric(1L)),
        label = vapply(values, function(v) if (length(v) >= 2L) v[[2L]] else "", character(1L)),
        stringsAsFactors = FALSE
      )
    }
    record$diagnostic_kind <- option_one("diagnostic_kind", record$diagnostic_kind %||% "")
    if (length(conditions)) record$conditions <- conditions
    if (identical(plot_kind, "trellis_scatterplot")) {
      record$plot_type <- option_one("plot_type", record$plot_type %||% "scatter")
      record$layout <- option_one("layout", record$layout %||% "automatic")
      record$scale_mode <- option_one("scale_mode", record$scale_mode %||% "common_xy")
      record$grouping <- option_one("grouping", record$grouping %||% "")
      record$split <- option_one("split", record$split %||% "")
      record$bar_measure <- option_one("bar_measure", record$bar_measure %||% "count")
      record$histogram_measure <- option_one("histogram_measure", record$histogram_measure %||% "count")
      record$histogram_bins <- max(1L, as.integer(option_number("histogram_bins", record$histogram_bins %||% 10L)))
    } else if (identical(plot_kind, "time_series")) {
      record$series <- option_one("series", record$series %||% "")
      record$time_type <- option_one("time_type", record$time_type %||% "numeric")
      record$identification <- option_one("identification", record$identification %||% "legend")
      record$legend_position <- option_one("legend_position", record$legend_position %||% "top_right")
    } else if (identical(plot_kind, "barplot")) {
      if (length(options$variable)) record$x <- options$variable
      if (!is.null(options$split)) record$y <- option_one("split")
      record$mode <- option_one("bar_mode", record$mode %||% "count")
      record$bar_width <- option_one("bar_width", record$bar_width %||% "equal")
      record$include_missing <- option_flag("include_missing", record$include_missing %||% TRUE)
      record$sort_x <- option_flag("sort_x", record$sort_x %||% FALSE)
      record$sort_y <- option_flag("sort_y", record$sort_y %||% FALSE)
    } else if (identical(plot_kind, "boxplot")) {
      if (length(options$variable)) record$boxplot_variables <- options$variable
      record$standardize <- option_flag("standardize", record$standardize %||% FALSE)
      record$connect_rows <- option_flag("connect_rows", record$connect_rows %||% FALSE)
    } else if (identical(plot_kind, "histogram")) {
      if (length(options$histogram_break)) {
        record$breaks <- as.double(options$histogram_break)
      } else if (!is.null(options$histogram_bins)) {
        record$breaks <- NULL
      }
      record$histogram_bins <- max(1L, as.integer(option_number("histogram_bins", record$histogram_bins %||% 10L)))
      record$show_density <- option_flag("show_density")
      record$density_mode <- option_one("density_mode", "all")
      record$density_bw <- option_number("density_bw", 0)
      record$density_adjust <- option_number("density_adjust", 1)
      record$show_rug <- option_flag("show_rug")
    }
    assign(plot_id, record, envir = .rls_state$plots)
    ls_export_plot(plot_id, path, width = width, height = height, format = format)
    if (!file.exists(path) || !isTRUE(file.info(path)$size > 0)) {
      stop("The SVG file could not be generated.", call. = FALSE)
    }
  }, error = function(e) {
    status <<- "error"
    message <<- clean(conditionMessage(e))
  })
  .rls_send(c("PLOT_EXPORT_RESULT", clean(request_id), status, clean(operation),
              clean(path), clean(message)))
  invisible(NULL)
}

.rls_handle_trellis_panel_analysis_needed <- function(parts) {
  if (length(parts) < 11L) return(invisible(NULL))
  request_id <- parts[[2L]]
  group <- parts[[4L]]
  panel_label <- parts[[6L]]
  plot_type <- parts[[7L]]
  x <- parts[[8L]]
  y <- parts[[9L]]
  grouping <- parts[[10L]]
  row_count <- suppressWarnings(as.integer(parts[[11L]]))
  if (!is.finite(row_count) || row_count < 1L || length(parts) < 11L + row_count) {
    return(invisible(NULL))
  }
  rows <- suppressWarnings(as.integer(parts[seq.int(12L, 11L + row_count)]))
  cursor <- 12L + row_count
  model_terms <- character()
  term_types <- list()
  if (length(parts) >= cursor) {
    term_count <- suppressWarnings(as.integer(parts[[cursor]])); cursor <- cursor + 1L
    if (is.finite(term_count) && term_count > 0L && length(parts) >= cursor + term_count - 1L) {
      model_terms <- parts[seq.int(cursor, cursor + term_count - 1L)]
      cursor <- cursor + term_count
    }
    if (length(parts) >= cursor) {
      type_count <- suppressWarnings(as.integer(parts[[cursor]])); cursor <- cursor + 1L
      if (is.finite(type_count) && type_count > 0L && length(parts) >= cursor + 2L * type_count - 1L) {
        for (i in seq_len(type_count)) {
          term_types[[parts[[cursor]]]] <- parts[[cursor + 1L]]
          cursor <- cursor + 2L
        }
      }
    }
  }
  tryCatch({
    source <- .rls_dataset_record(group)
    rows <- unique(rows[is.finite(rows) & rows >= 1L & rows <= nrow(source$data)])
    if (!length(rows)) stop("The selected trellis panel contains no usable rows.", call. = FALSE)
    panel_data <- source$data[rows, , drop = FALSE]
    if (identical(plot_type, "time_series") && x %in% names(panel_data) &&
        inherits(panel_data[[x]], c("Date", "POSIXct", "POSIXlt"))) {
      panel_data[[x]] <- as.numeric(panel_data[[x]])
    }
    safe_label <- gsub("[\r\n\t|]+", " ", panel_label)
    open_panel_linear_model <- function(terms, types) {
      model_name <- paste0("panel_glm_", request_id, "_", format(Sys.time(), "%H%M%OS3"))
      ls_new_generalized_linear_model(
        data = group, response = y, terms = terms,
        family = "gaussian", link = "identity", scope = "selected",
        name = model_name, native = TRUE, term_types = types,
        .selected_rows = rows,
        .scope_description = paste0("Trellis panel \u2014 ", safe_label),
        .scope_source_kind = "trellis_panel"
      )
    }

    if (identical(plot_type, "model_trellis_scatter")) {
      if (!nzchar(y) || !x %in% names(panel_data) || !y %in% names(panel_data)) {
        stop("The variables required for this panel model are unavailable.", call. = FALSE)
      }
      open_panel_linear_model(x, stats::setNames("numeric", x))
    } else if (identical(plot_type, "model_trellis")) {
      open_panel_linear_model(model_terms, term_types)
    } else if (plot_type %in% c("scatter", "time_series", "boxplot")) {
      if (!nzchar(y) || !x %in% names(panel_data) || !y %in% names(panel_data)) {
        stop("The variables required for this panel model are unavailable.", call. = FALSE)
      }
      term_type <- if (identical(plot_type, "boxplot")) "factor" else "numeric"
      open_panel_linear_model(x, stats::setNames(term_type, x))
    } else {
      variables <- x
      by <- if (identical(plot_type, "bar") && nzchar(grouping) &&
                grouping %in% names(panel_data) && grouping != x) grouping else NULL
      table <- ls_new_table1(
        data = group, variables = variables, group = by,
        show_p = !is.null(by), show_test = !is.null(by),
        name = paste0("panel_table_", request_id),
        .selected_rows = rows,
        .scope_description = paste0("Trellis panel \u2014 ", safe_label)
      )
      record <- .rls_table1_record(table)
      record$title <- paste0("Panel analysis \u2014 ", safe_label)
      .rls_assign_table1(record)
      .rls_table1_sync_native(record)
    }
  }, error = function(e) {
    clean <- gsub("[\r\n\t|]+", " ", conditionMessage(e))
    try(.rls_send(c("WORKBENCH_MESSAGE", "Panel analysis", paste("Panel analysis failed:", clean))), silent = TRUE)
    message("LinkEDA trellis-panel analysis failed: ", conditionMessage(e))
  })
  invisible(NULL)
}

.rls_handle_compare_means_needed <- function(group, test_type, var1, var2 = "", group_var = "") {
  tryCatch({
    switch(test_type,
      one_sample_t = ls_new_one_sample_t_test(data = group, response = var1),
      independent_t = ls_new_independent_samples_t_test(data = group, response = var1, group = group_var),
      paired_t = ls_new_paired_samples_t_test(data = group, pairs = list(c(var1, var2))),
      oneway_anova = ls_new_one_way_anova(data = group, response = var1, group = group_var),
      stop(sprintf("Unknown compare-means task `%s`.", test_type), call. = FALSE)
    )
  }, error = function(e) {
    message("LinkEDA compare-means task failed: ", conditionMessage(e))
  })
  invisible(NULL)
}

.rls_handle_table1_needed <- function(parts) {
  if (length(parts) < 13L) return(invisible(NULL))
  cursor <- 2L
  result_id <- parts[[cursor]]; cursor <- cursor + 1L
  group <- parts[[cursor]]; cursor <- cursor + 1L
  group_variable <- parts[[cursor]]; cursor <- cursor + 1L
  include_missing <- identical(parts[[cursor]], "TRUE"); cursor <- cursor + 1L
  show_p <- identical(parts[[cursor]], "TRUE"); cursor <- cursor + 1L
  show_test <- identical(parts[[cursor]], "TRUE"); cursor <- cursor + 1L
  show_n <- identical(parts[[cursor]], "TRUE"); cursor <- cursor + 1L
  ordinal_as <- parts[[cursor]]; cursor <- cursor + 1L
  scope <- parts[[cursor]]; cursor <- cursor + 1L
  scope_description <- parts[[cursor]]; cursor <- cursor + 1L
  variable_count <- suppressWarnings(as.integer(parts[[cursor]])); cursor <- cursor + 1L
  if (!is.finite(variable_count) || variable_count < 0L ||
      length(parts) < cursor + variable_count - 1L) return(invisible(NULL))
  variables <- if (variable_count > 0L) parts[seq.int(cursor, cursor + variable_count - 1L)] else character()
  cursor <- cursor + variable_count

  # Current Windows senders include the canonical LinkEDA variable types after
  # the selected-variable list.  Keep accepting the older payload (whose next
  # field is the row count) so an already-installed R package can still serve
  # an older application while both sides are being upgraded.
  variable_types <- NULL
  if (length(parts) >= cursor && identical(parts[[cursor]], "TYPES")) {
    cursor <- cursor + 1L
    type_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[[cursor]])) else NA_integer_
    cursor <- cursor + 1L
    if (!is.finite(type_count) || type_count < 0L ||
        length(parts) < cursor + 2L * type_count - 1L) return(invisible(NULL))
    if (type_count > 0L) {
      type_names <- character(type_count)
      type_values <- character(type_count)
      for (index in seq_len(type_count)) {
        type_names[[index]] <- parts[[cursor]]; cursor <- cursor + 1L
        type_values[[index]] <- parts[[cursor]]; cursor <- cursor + 1L
      }
      variable_types <- stats::setNames(type_values, type_names)
    }
  }
  row_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[[cursor]])) else 0L
  cursor <- cursor + 1L
  scope_rows <- if (is.finite(row_count) && row_count > 0L &&
                    length(parts) >= cursor + row_count - 1L) {
    suppressWarnings(as.integer(parts[seq.int(cursor, cursor + row_count - 1L)]))
  } else integer()
  cursor <- cursor + if (is.finite(row_count) && row_count > 0L) row_count else 0L
  analysis_kind <- "table1"
  if (length(parts) >= cursor + 1L && identical(parts[[cursor]], "ANALYSIS_KIND_V1")) {
    analysis_kind <- parts[[cursor + 1L]]; cursor <- cursor + 2L
  }
  contingency_mode <- "count_percent"
  if (length(parts) >= cursor + 1L && identical(parts[[cursor]], "CONTINGENCY_OPTIONS_V1")) {
    contingency_mode <- parts[[cursor + 1L]]; cursor <- cursor + 2L
  }
  modern <- length(parts) >= cursor && identical(parts[[cursor]], "REQUEST_V1")
  revision <- version <- NA_integer_
  if (modern) {
    if (length(parts) < cursor + 2L) return(invisible(NULL))
    revision <- suppressWarnings(as.integer(parts[[cursor + 1L]]))
    version <- suppressWarnings(as.integer(parts[[cursor + 2L]]))
    if (!is.finite(revision) || revision < 1L || !is.finite(version)) return(invisible(NULL))
  }
  tryCatch({
    dataset <- .rls_dataset_record(group)
    if (modern && !identical(as.integer(dataset$data_version %||% 1L), version))
      stop("The source data changed before Table 1 could be calculated.", call. = FALSE)
    ids <- dataset$original_row_ids %||% seq_len(nrow(dataset$data))
    if (!is.finite(row_count) || row_count < 0L || length(scope_rows) != row_count ||
        anyNA(scope_rows) || any(!scope_rows %in% ids))
      stop("The requested Table 1 rows are invalid.", call. = FALSE)
    if (!analysis_kind %in% c("table1", "frequency", "nested_contingency")) stop("Unsupported descriptive analysis.", call.=FALSE)
    result <- if (analysis_kind == "nested_contingency") .rls_nested_contingency_record(
      group, variables, group_variable, result_id,
      selected_rows=if(scope=="selected") scope_rows else NULL,
      display_mode=contingency_mode, scope_description=scope_description) else if (analysis_kind == "frequency") .rls_frequency_record(group, variables, result_id,
      selected_rows=if(scope=="selected") scope_rows else NULL, scope_description=scope_description) else ls_new_table1(
      data = group,
      variables = variables,
      group = if (nzchar(group_variable)) group_variable else NULL,
      include_missing = include_missing,
      show_p = show_p,
      show_test = show_test,
      show_n = show_n,
      variable_types = variable_types,
      ordinal_as = if (identical(ordinal_as, "categorical")) "categorical" else "ordinal",
      name = result_id, native = !modern && isTRUE(.rls_state$process_started),
      .selected_rows = if (identical(scope, "selected")) scope_rows else NULL,
      .scope_description = if (identical(scope, "selected")) scope_description else "All observations"
    )
    if (modern) {
      if (!identical(as.integer(.rls_dataset_record(group)$data_version %||% 1L), version))
        stop("The source data changed during the Table 1 calculation.", call. = FALSE)
      record <- .rls_table1_record(result)
      record$native_request_revision <- revision
      record$native_data_version <- version
      assign(record$id, record, envir = .rls_state$table1_tables)
      .rls_send(.rls_table1_native_payload(record))
    }
  }, error = function(e) {
    if (modern || isTRUE(.rls_state$process_started)) {
      try(.rls_send(c("TABLE1_OPEN_ERROR", result_id, group,
                      if (modern) c("REQUEST_V1", as.character(revision), as.character(version)),
                      gsub("[\r\n\t]+", " ", conditionMessage(e)))), silent = TRUE)
    }
    message("LinkEDA descriptive-statistics task failed: ", conditionMessage(e))
  })
  invisible(NULL)
}

.rls_handle_compare_means_batch_needed <- function(parts) {
  if (length(parts) < 10L) return(invisible(NULL))
  cursor <- 2L
  run_id <- parts[[cursor]]; cursor <- cursor + 1L
  group <- parts[[cursor]]; cursor <- cursor + 1L
  test_type <- parts[[cursor]]; cursor <- cursor + 1L
  alternative <- parts[[cursor]]; cursor <- cursor + 1L
  conf_level <- suppressWarnings(as.numeric(parts[[cursor]])); cursor <- cursor + 1L
  method <- parts[[cursor]]; cursor <- cursor + 1L
  p_adjust <- if (length(parts) >= cursor && parts[[cursor]] %in% c("holm", "bonferroni", "none")) {
    value <- parts[[cursor]]; cursor <- cursor + 1L; value
  } else "holm"
  test_value <- suppressWarnings(as.numeric(parts[[cursor]])); cursor <- cursor + 1L
  group_var <- parts[[cursor]]; cursor <- cursor + 1L
  response_count <- suppressWarnings(as.integer(parts[[cursor]])); cursor <- cursor + 1L
  if (!is.finite(response_count) || response_count < 0L || length(parts) < cursor + response_count - 1L) return(invisible(NULL))
  responses <- if (response_count > 0L) parts[seq.int(cursor, cursor + response_count - 1L)] else character()
  cursor <- cursor + response_count
  if (length(parts) < cursor) return(invisible(NULL))
  pair_count <- suppressWarnings(as.integer(parts[[cursor]])); cursor <- cursor + 1L
  if (!is.finite(pair_count) || pair_count < 0L || length(parts) < cursor + 2L * pair_count - 1L) return(invisible(NULL))
  pairs <- vector("list", pair_count)
  if (pair_count > 0L) {
    for (i in seq_len(pair_count)) {
      pairs[[i]] <- c(parts[[cursor]], parts[[cursor + 1L]])
      cursor <- cursor + 2L
    }
  }
  if (length(parts) < cursor) return(invisible(NULL))
  order_count <- suppressWarnings(as.integer(parts[[cursor]])); cursor <- cursor + 1L
  if (!is.finite(order_count) || order_count < 0L || length(parts) < cursor + order_count - 1L) return(invisible(NULL))
  group_order <- if (order_count > 0L) parts[seq.int(cursor, cursor + order_count - 1L)] else character()
  cursor <- cursor + order_count
  task_scope <- if (length(parts) >= cursor && parts[[cursor]] %in% c("all", "selected")) {
    value <- parts[[cursor]]; cursor <- cursor + 1L; value
  } else "all"
  row_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[[cursor]])) else 0L
  cursor <- cursor + 1L
  scope_rows <- if (is.finite(row_count) && row_count > 0L && length(parts) >= cursor + row_count - 1L)
    suppressWarnings(as.integer(parts[seq.int(cursor, cursor + row_count - 1L)])) else integer()
  scope_rows <- scope_rows[is.finite(scope_rows) & scope_rows > 0L]
  cursor <- cursor + if (is.finite(row_count) && row_count > 0L) row_count else 0L
  test_spec_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[[cursor]])) else 0L
  cursor <- cursor + 1L
  one_sample_values <- numeric()
  one_sample_methods <- character()
  if (is.finite(test_spec_count) && test_spec_count > 0L &&
      length(parts) >= cursor + 3L * test_spec_count - 1L) {
    for (i in seq_len(test_spec_count)) {
      response_name <- parts[[cursor]]
      one_sample_values[[response_name]] <- suppressWarnings(as.numeric(parts[[cursor + 1L]]))
      one_sample_methods[[response_name]] <- parts[[cursor + 2L]]
      cursor <- cursor + 3L
    }
  }

  previous_run_id <- .rls_state$compare_means_run_id
  previous_mixed_one_sample <- .rls_state$compare_means_mixed_one_sample %||% FALSE
  previous_mixed_independent <- .rls_state$compare_means_mixed_independent %||% FALSE
  previous_mixed_paired <- .rls_state$compare_means_mixed_paired %||% FALSE
  previous_batch_method <- .rls_state$compare_means_batch_method %||% ""
  .rls_state$compare_means_run_id <- run_id
  .rls_state$compare_means_mixed_one_sample <- identical(test_type, "one_sample_mixed")
  .rls_state$compare_means_mixed_independent <- identical(test_type, "independent_mixed")
  .rls_state$compare_means_mixed_paired <- identical(test_type, "paired_mixed")
  .rls_state$compare_means_batch_method <- method
  on.exit({
    .rls_state$compare_means_run_id <- previous_run_id
    .rls_state$compare_means_mixed_one_sample <- previous_mixed_one_sample
    .rls_state$compare_means_mixed_independent <- previous_mixed_independent
    .rls_state$compare_means_mixed_paired <- previous_mixed_paired
    .rls_state$compare_means_batch_method <- previous_batch_method
  }, add = TRUE)
  tryCatch({
    if (test_type %in% c("independent_t", "independent_mixed", "oneway_anova")) {
      dataset <- .rls_dataset_record(group)
      group_order <- .rls_compare_means_native_group_order(
        dataset, group_var, group_order
      )
    }
    switch(test_type,
      one_sample_t = ls_new_one_sample_t_test(data = group, response = responses, mu = test_value,
                                              alternative = alternative, conf_level = conf_level,
                                              p_adjust = p_adjust,
                                              method = if (identical(method, "wilcoxon")) "wilcoxon" else "student",
                                              scope = task_scope, .selected_rows = scope_rows),
      one_sample_mixed = ls_new_one_sample_t_test(
                                              data = group, response = responses,
                                              mu = one_sample_values,
                                              alternative = alternative, conf_level = conf_level,
                                              p_adjust = p_adjust, method = one_sample_methods,
                                              scope = task_scope, .selected_rows = scope_rows),
      independent_t = ls_new_independent_samples_t_test(data = group, response = responses, group = group_var,
                                                        alternative = alternative, conf_level = conf_level,
                                                        var_equal = identical(method, "student"), method = method,
                                                        group_order = group_order,
                                                        p_adjust = p_adjust, scope = task_scope,
                                                        .selected_rows = scope_rows),
      independent_mixed = ls_new_independent_samples_t_test(
                                                        data = group, response = responses, group = group_var,
                                                        alternative = alternative, conf_level = conf_level,
                                                        method = one_sample_methods,
                                                        group_order = group_order,
                                                        p_adjust = p_adjust, scope = task_scope,
                                                        .selected_rows = scope_rows),
      paired_t = ls_new_paired_samples_t_test(data = group, pairs = pairs,
                                              alternative = alternative, conf_level = conf_level,
                                              p_adjust = p_adjust,
                                              method = if (identical(method, "wilcoxon")) "wilcoxon" else "student",
                                              scope = task_scope, .selected_rows = scope_rows),
      paired_mixed = ls_new_paired_samples_t_test(data = group, pairs = pairs,
                                                  alternative = alternative, conf_level = conf_level,
                                                  p_adjust = p_adjust,
                                                  method = unname(one_sample_methods),
                                                  scope = task_scope, .selected_rows = scope_rows),
      oneway_anova = ls_new_one_way_anova(data = group, response = responses, group = group_var,
                                          conf_level = conf_level, method = method, group_order = group_order,
                                          p_adjust = p_adjust, scope = task_scope,
                                          .selected_rows = scope_rows),
      stop(sprintf("Unknown compare-means task `%s`.", test_type), call. = FALSE)
    )
  }, error = function(e) {
    title <- switch(test_type,
      one_sample_t = "One-Sample Tests", one_sample_mixed = "One-Sample Tests",
      independent_t = "Two-Sample Tests", independent_mixed = "Two-Sample Tests",
      paired_t = "Paired-Samples Tests", paired_mixed = "Paired-Samples Tests",
      oneway_anova = "One-Way ANOVA", "Compare Means")
    if (isTRUE(.rls_state$process_started)) {
      try(.rls_send(c("COMPARE_MEANS_BATCH_ERROR", run_id, title, conditionMessage(e))), silent = TRUE)
    }
    message("LinkEDA compare-means task failed: ", conditionMessage(e))
  })
  invisible(NULL)
}

.rls_handle_regcmp_needed <- function(parts) {
  if (length(parts) < 9L) return(invisible(NULL))
  cursor <- 2L
  id <- parts[cursor]; cursor <- cursor + 1L
  request_generation <- suppressWarnings(as.integer(parts[cursor])); cursor <- cursor + 1L
  if (!is.finite(request_generation) || request_generation < 0L) return(invisible(NULL))
  group <- parts[cursor]; cursor <- cursor + 1L
  response <- parts[cursor]; cursor <- cursor + 1L
  scope <- parts[cursor]; cursor <- cursor + 1L
  auto_refit <- !identical(parts[cursor], "FALSE"); cursor <- cursor + 1L
  term_row_count <- suppressWarnings(as.integer(parts[cursor])); cursor <- cursor + 1L
  if (!is.finite(term_row_count) || term_row_count < 0L || length(parts) < cursor + term_row_count - 1L) {
    return(invisible(NULL))
  }
  term_rows <- character()
  if (term_row_count > 0L) {
    term_rows <- parts[seq.int(cursor, cursor + term_row_count - 1L)]
    cursor <- cursor + term_row_count
  }
  if (length(parts) < cursor) return(invisible(NULL))
  type_count <- suppressWarnings(as.integer(parts[cursor])); cursor <- cursor + 1L
  if (!is.finite(type_count) || type_count < 0L) return(invisible(NULL))
  term_types <- list()
  if (type_count > 0L) {
    if (length(parts) < cursor + 2L * type_count - 1L) return(invisible(NULL))
    for (i in seq_len(type_count)) {
      term <- parts[cursor]
      type <- parts[cursor + 1L]
      cursor <- cursor + 2L
      if (nzchar(term) && type %in% c("numeric", "factor")) {
        term_types[[term]] <- type
      }
    }
  }
  if (length(parts) < cursor) return(invisible(NULL))
  model_count <- suppressWarnings(as.integer(parts[cursor])); cursor <- cursor + 1L
  if (!is.finite(model_count) || model_count < 0L) return(invisible(NULL))
  models <- vector("list", model_count)
  for (i in seq_len(model_count)) {
    if (length(parts) < cursor + 3L) return(invisible(NULL))
    model_id <- parts[cursor]; cursor <- cursor + 1L
    label <- parts[cursor]; cursor <- cursor + 1L
    model_response <- parts[cursor]; cursor <- cursor + 1L
    term_count <- suppressWarnings(as.integer(parts[cursor])); cursor <- cursor + 1L
    if (!is.finite(term_count) || term_count < 0L || length(parts) < cursor + term_count - 1L) {
      return(invisible(NULL))
    }
    terms <- character()
    if (term_count > 0L) {
      terms <- parts[seq.int(cursor, cursor + term_count - 1L)]
      cursor <- cursor + term_count
    }
    model_type_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[cursor])) else 0L
    if (!is.finite(model_type_count) || model_type_count < 0L) return(invisible(NULL))
    cursor <- cursor + 1L
    model_term_types <- list()
    if (model_type_count > 0L) {
      if (length(parts) < cursor + 2L * model_type_count - 1L) return(invisible(NULL))
      for (j in seq_len(model_type_count)) {
        term <- parts[cursor]
        type <- parts[cursor + 1L]
        cursor <- cursor + 2L
        if (nzchar(term) && type %in% c("numeric", "factor")) model_term_types[[term]] <- type
      }
    }
    centered_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[cursor])) else 0L
    if (!is.finite(centered_count) || centered_count < 0L) return(invisible(NULL))
    cursor <- cursor + 1L
    centered_predictors <- character()
    if (centered_count > 0L) {
      if (length(parts) < cursor + centered_count - 1L) return(invisible(NULL))
      centered_predictors <- parts[seq.int(cursor, cursor + centered_count - 1L)]
      cursor <- cursor + centered_count
    }
    reference_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[cursor])) else 0L
    if (!is.finite(reference_count) || reference_count < 0L) return(invisible(NULL))
    cursor <- cursor + 1L
    factor_reference_levels <- list()
    if (reference_count > 0L) {
      if (length(parts) < cursor + 2L * reference_count - 1L) return(invisible(NULL))
      for (j in seq_len(reference_count)) {
        variable <- parts[cursor]
        reference <- parts[cursor + 1L]
        cursor <- cursor + 2L
        if (nzchar(variable) && nzchar(reference)) {
          factor_reference_levels[[variable]] <- reference
        }
      }
    }
    models[[i]] <- list(id = model_id, label = label, response = model_response,
                        terms = terms, term_types = model_term_types,
                        centered_predictors = centered_predictors,
                        factor_reference_levels = factor_reference_levels)
  }
  row_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[cursor])) else 0L
  cursor <- cursor + 1L
  task_rows <- if (is.finite(row_count) && row_count > 0L && length(parts) >= cursor + row_count - 1L)
    suppressWarnings(as.integer(parts[seq.int(cursor, cursor + row_count - 1L)])) else integer()
  task_rows <- task_rows[is.finite(task_rows) & task_rows > 0L]
  cursor <- cursor + if (is.finite(row_count) && row_count > 0L) row_count else 0L
  requested_dataset_type <- ""
  requested_imputation_id <- ""
  requested_source_dataset_id <- ""
  requested_imputation_count <- 0L
  if (length(parts) >= cursor && identical(parts[[cursor]], "REGCMP_DATASET_V1")) {
    if (length(parts) < cursor + 4L) return(invisible(NULL))
    requested_dataset_type <- parts[[cursor + 1L]]
    requested_imputation_id <- parts[[cursor + 2L]]
    requested_source_dataset_id <- parts[[cursor + 3L]]
    requested_imputation_count <- suppressWarnings(as.integer(parts[[cursor + 4L]]))
    if (!nzchar(requested_dataset_type) || !is.finite(requested_imputation_count) ||
        requested_imputation_count < 0L) return(invisible(NULL))
  }
  tryCatch({
    dataset <- .rls_dataset_record(group)
    if (nzchar(requested_dataset_type)) {
      actual_dataset_type <- dataset$dataset_type %||% "data_frame"
      actual_imputation_id <- dataset$imputation_id %||% ""
      actual_source_dataset_id <- dataset$source_dataset_id %||% ""
      actual_imputation_count <- if (identical(actual_dataset_type, "multiple_imputation"))
        as.integer(dataset$imputation_count %||% length(dataset$completed_datasets %||% list())) else 0L
      if (!identical(requested_dataset_type, actual_dataset_type) ||
          !identical(requested_imputation_id, actual_imputation_id) ||
          !identical(requested_source_dataset_id, actual_source_dataset_id) ||
          !identical(as.integer(requested_imputation_count), actual_imputation_count)) {
        stop("The General Linear Model comparison request belongs to a different dataset/imputation revision.", call. = FALSE)
      }
    }
    record <- list(
      id = id,
      request_generation = request_generation,
      group = group,
      data = dataset$data,
      dataset_type = dataset$dataset_type %||% "data_frame",
      analysis_backend = .rls_analysis_backend(dataset, "regression_comparison"),
      response = .rls_model_validate_response(dataset$data, response, "response"),
      uses_shared_response = TRUE,
      scope = scope,
      selected_rows = task_rows,
      scope_request_pending = TRUE,
      auto_refit = auto_refit,
      term_rows = unique(c("(Intercept)", term_rows)),
      term_types = term_types,
      models = vector("list", length(models)),
      active_model = 1L
    )
    for (i in seq_along(models)) {
      spec <- models[[i]]
      # Native model-comparison columns normally inherit the comparison's
      # shared response.  The transport represents that inheritance as an
      # empty string, so `%||%` alone is not sufficient here: `""` is not
      # NULL and would otherwise be validated as a literal column name.
      model_response <- as.character(spec$response %||% "")[[1L]]
      if (!nzchar(model_response)) model_response <- record$response
      model_response <- .rls_model_validate_response(dataset$data, model_response, "response")
      terms <- unique(as.character(spec$terms %||% character()))
      record$term_rows <- unique(c(record$term_rows, terms))
      record$models[[i]] <- list(
        id = spec$id,
        label = spec$label,
        response = model_response,
        terms = terms,
        term_types = spec$term_types %||% term_types,
        centered_predictors = unique(spec$centered_predictors[nzchar(spec$centered_predictors)]),
        factor_reference_levels = spec$factor_reference_levels %||% list(),
        fit = NULL,
        fitted_lm = NULL,
        coefficients = data.frame(),
        coefficient_rows = data.frame(),
        summary = list(),
        rows_used = integer(),
        rows_excluded = integer(),
        diagnostics = data.frame(),
        model_version = 0L,
        fit_version = 0L,
        is_stale = TRUE,
        status = "Not fitted."
      )
    }
    record <- .rls_regcmp_fit_models(record, force = TRUE)
    record <- .rls_regcmp_update_mi_nested_tests(record)
    record <- .rls_regcmp_update_ordinary_nested_tests(record)
    .rls_assign_regcmp(record)
    .rls_regcmp_sync_native_update(record)
  }, error = function(e) {
    .rls_regcmp_sync_native_error(id, request_generation, conditionMessage(e))
    message("LinkEDA General Linear Model comparison task failed: ", conditionMessage(e))
  })
  invisible(NULL)
}

.rls_handle_glm_needed <- function(parts) {
  if (length(parts) < 6L) return(invisible(NULL))
  group <- parts[2L]
  dependent <- parts[3L]
  scope <- parts[4L]
  term_count <- suppressWarnings(as.integer(parts[5L]))
  if (!is.finite(term_count) || term_count < 0L || length(parts) < 5L + term_count) {
    return(invisible(NULL))
  }
  cursor <- 6L
  terms <- character()
  if (term_count > 0L) {
    terms <- parts[seq.int(cursor, cursor + term_count - 1L)]
    cursor <- cursor + term_count
  }
  type_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[cursor])) else 0L
  if (!is.finite(type_count) || type_count < 0L) type_count <- 0L
  cursor <- cursor + 1L
  term_types <- list()
  if (type_count > 0L) {
    if (length(parts) < cursor + 2L * type_count - 1L) return(invisible(NULL))
    for (i in seq_len(type_count)) {
      term <- parts[cursor]
      type <- parts[cursor + 1L]
      cursor <- cursor + 2L
      if (nzchar(term) && type %in% c("numeric", "factor")) {
        term_types[[term]] <- type
      }
    }
  }
  centered_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[cursor])) else 0L
  if (!is.finite(centered_count) || centered_count < 0L) centered_count <- 0L
  cursor <- cursor + 1L
  centered_predictors <- character()
  if (centered_count > 0L) {
    if (length(parts) < cursor + centered_count - 1L) return(invisible(NULL))
    centered_predictors <- parts[seq.int(cursor, cursor + centered_count - 1L)]
    cursor <- cursor + centered_count
  }
  reference_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[cursor])) else 0L
  if (!is.finite(reference_count) || reference_count < 0L) reference_count <- 0L
  cursor <- cursor + 1L
  factor_reference_levels <- list()
  if (reference_count > 0L) {
    if (length(parts) < cursor + 2L * reference_count - 1L) return(invisible(NULL))
    for (i in seq_len(reference_count)) {
      variable <- parts[cursor]
      reference <- parts[cursor + 1L]
      cursor <- cursor + 2L
      if (nzchar(variable) && nzchar(reference))
        factor_reference_levels[[variable]] <- reference
    }
  }
  row_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[cursor])) else 0L
  cursor <- cursor + 1L
  task_rows <- if (is.finite(row_count) && row_count > 0L && length(parts) >= cursor + row_count - 1L)
    suppressWarnings(as.integer(parts[seq.int(cursor, cursor + row_count - 1L)])) else integer()
  task_rows <- task_rows[is.finite(task_rows) & task_rows > 0L]
  if (is.finite(row_count) && row_count > 0L) cursor <- cursor + row_count
  request_identity <- ""
  if (length(parts) >= cursor + 1L && identical(parts[cursor], "FIT_ID_V1")) {
    request_identity <- parts[cursor + 1L]
    cursor <- cursor + 2L
  }
  model_id <- group
  if (length(parts) >= cursor + 1L && identical(parts[cursor], "LINEAR_MODEL_ID_V1")) {
    model_id <- parts[cursor + 1L]
  }
  tryCatch({
    dataset <- .rls_dataset_record(group)
    internal_id <- .rls_glm_model_id(group)
    handle <- if (exists(internal_id, envir = .rls_state$glm_models, inherits = FALSE)) {
      structure(list(id = internal_id, group = group), class = "rlispstat_glm")
    } else {
      ls_new_glm(group)
    }
    record <- .rls_glm_model_record(handle)
    record$data <- dataset$data
    record$dataset_type <- dataset$dataset_type %||% "data_frame"
    record$analysis_backend <- .rls_analysis_backend(dataset, "linear_model")
    record$dependent <- .rls_model_validate_response(record$data, dependent, "dependent")
    record$predictors <- terms
    record$scope <- scope
    record$selected_rows <- task_rows
    record$scope_request_pending <- TRUE
    record$term_types <- term_types
    record$centered_predictors <- unique(centered_predictors[nzchar(centered_predictors)])
    record$factor_reference_levels <- factor_reference_levels
    record$is_stale <- TRUE
    record$fit <- NULL
    # This request already came from the visible native window.  Fitting must
    # not re-register the dataset or issue MODEL_OPEN_POOLED while R is still
    # handling it; that duplicated transport used to make Linear Regression
    # noticeably slower than the generalized routes and could enqueue a
    # second refresh.  Publish exactly one MODEL_UPDATE below instead.
    record$native_sync_enabled <- FALSE
    .rls_assign_glm_model(record)
    handle <- ls_glm_fit(handle)
    record <- .rls_glm_model_record(handle)
    record$native_sync_enabled <- TRUE
    .rls_assign_glm_model(record)
    .rls_glm_sync_native_update(record, request_identity, model_id)
  }, error = function(e) {
    .rls_glm_sync_native_error(group, conditionMessage(e), request_identity, model_id)
    message("LinkEDA GLM task failed: ", conditionMessage(e))
  })
  invisible(NULL)
}

.rls_handle_dimensionality_needed <- function(parts) {
  if (length(parts) < 10L) return(invisible(NULL))
  id <- parts[2L]
  group <- parts[3L]
  method <- parts[4L]
  missing <- parts[5L]
  rotation <- parts[6L]
  scope <- parts[7L]
  scale <- !identical(parts[8L], "FALSE")
  n_components <- suppressWarnings(as.integer(parts[9L]))
  variable_count <- suppressWarnings(as.integer(parts[10L]))
  if (!is.finite(n_components) || !is.finite(variable_count) || variable_count < 0L) {
    return(invisible(NULL))
  }
  variables <- character()
  if (variable_count > 0L) {
    if (length(parts) < 10L + variable_count) return(invisible(NULL))
    variables <- parts[seq.int(11L, 10L + variable_count)]
  }
  cursor <- 11L + max(0L, variable_count)
  row_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[cursor])) else 0L
  cursor <- cursor + 1L
  task_rows <- if (is.finite(row_count) && row_count > 0L && length(parts) >= cursor + row_count - 1L)
    suppressWarnings(as.integer(parts[seq.int(cursor, cursor + row_count - 1L)])) else integer()
  cursor <- cursor + if (is.finite(row_count)) max(0L, row_count) else 0L
  requested_imputation <- if (length(parts) >= cursor + 1L &&
                              identical(parts[cursor], "IMPUTATION_V1"))
    suppressWarnings(as.integer(parts[cursor + 1L])) else 0L
  if (length(parts) >= cursor + 1L && identical(parts[cursor], "IMPUTATION_V1")) cursor <- cursor + 2L
  extraction <- if (length(parts) >= cursor + 1L &&
                    identical(parts[cursor], "EXTRACTION_V1")) parts[cursor + 1L] else "minres"
  if (length(parts) >= cursor + 1L && identical(parts[cursor], "EXTRACTION_V1")) cursor <- cursor + 2L
  identified <- length(parts) >= cursor && identical(parts[cursor], "REQUEST_V1")
  revision <- if (identified && length(parts) >= cursor + 2L) parts[cursor + 1L] else NULL
  version <- if (identified && length(parts) >= cursor + 2L) parts[cursor + 2L] else NULL
  tryCatch({
    dataset <- .rls_dataset_record(group)
    if (identified) {
      if (is.null(revision) || is.null(version) || length(parts) != cursor + 2L)
        stop("Malformed dimensionality request identity.", call. = FALSE)
      if (!identical(as.character(dataset$data_version %||% 1L), version))
        stop("The data changed before dimensionality could run. Recalculate the analysis.", call. = FALSE)
      if (!is.finite(row_count) || row_count < 0L || length(task_rows) != row_count ||
          anyNA(task_rows) || any(!task_rows %in% .rls_mi_original_row_ids(dataset)))
        stop("Invalid dimensionality row identities.", call. = FALSE)
    }
    active <- 1L
    analysis_data <- if (.rls_mi_is_dataset(dataset)) {
      completed <- .rls_mi_completed_datasets(dataset)
      active <- if (is.finite(requested_imputation) && requested_imputation > 0L)
        requested_imputation else as.integer(dataset$active_imputation_version %||% 1L)
      if (!is.finite(active) || active < 1L || active > length(completed))
        stop("Invalid imputation number.", call. = FALSE)
      completed[[active]]
    } else dataset$data
    selected_rows <- task_rows
    record <- list(
      id = id,
      group = group,
      dataset_id = group,
      data = analysis_data,
      active_imputation_version = active,
      variables = variables,
      method = method,
      n_components = n_components,
      scale = scale,
      missing_mode = missing,
      rotation = rotation,
      extraction = extraction,
      scope = scope,
      selected_rows = selected_rows,
      scope_request_pending = TRUE,
      native_request_revision = revision,
      native_request_version = version,
      parallel = TRUE,
      parallel_iterations = 100L,
      eigenvalues = data.frame(),
      loadings = data.frame(),
      scores = data.frame(),
      rows_used_original_ids = integer(),
      rows_excluded_original_ids = integer(),
      model_version = 0L
    )
    record <- .rls_dimension_refit(record)
    if (identified && !identical(as.character(.rls_dataset_record(group)$data_version %||% 1L), version))
      stop("The data changed while dimensionality was running. Recalculate the analysis.", call. = FALSE)
    .rls_assign_dimension(record)
    if (identified) .rls_send(.rls_dimension_native_update_payload(record)) else
      .rls_dimension_sync_native_update(record)
  }, error = function(e) {
    if (identified && !is.null(revision) && !is.null(version)) {
      .rls_send(c("PCAFA_UPDATE", id, "REQUEST_V1", revision, version, "error",
                  .rls_native_wire_value(conditionMessage(e))))
    } else message("LinkEDA dimensionality task failed: ", conditionMessage(e))
  })
  invisible(NULL)
}

.rls_handle_dimensionality_save_scores_needed <- function(parts) {
  if (length(parts) < 6L) return(invisible(FALSE))
  request_id <- parts[[2L]]
  source_kind <- parts[[3L]]
  analysis_id <- parts[[4L]]
  group <- parts[[5L]]
  count <- suppressWarnings(as.integer(parts[[6L]]))
  send_result <- function(status, saved_count, message) {
    if (isTRUE(.rls_state$process_started)) {
      try(.rls_send(c(
        "DIMENSIONALITY_SAVE_SCORES_RESULT", request_id, status,
        source_kind, analysis_id, group, as.character(saved_count),
        .rls_native_wire_value(message)
      )), silent = TRUE)
    }
  }
  if (!nzchar(request_id) || !source_kind %in% c("dimensionality", "scale_analysis") ||
      !nzchar(analysis_id) || !nzchar(group) || !is.finite(count) || count < 1L) {
    send_result("error", 0L, "The score-save request is malformed.")
    return(invisible(FALSE))
  }
  tryCatch({
    saved <- if (identical(source_kind, "dimensionality")) {
      ls_dimensionality_save_scores(
        analysis_id, components = seq_len(count), dataset = group
      )
    } else {
      ls_scale_analysis_save_scores(
        analysis_id, components = seq_len(count), dataset = group
      )
    }
    saved <- as.character(saved)
    send_result(
      "ok", length(saved),
      sprintf("Saved %d score column%s: %s",
              length(saved), if (length(saved) == 1L) "" else "s",
              paste(saved, collapse = ", "))
    )
  }, error = function(e) {
    send_result("error", 0L, conditionMessage(e))
    message("LinkEDA dimensionality score save failed: ", conditionMessage(e))
  })
  invisible(TRUE)
}

.rls_handle_generalized_glm_needed <- function(id, group, response, family, link, scope, terms,
                                               term_types = list(), centered_predictors = character(),
                                               factor_reference_levels = list(), binary = FALSE,
                                               count_regression = FALSE,
                                               count_distribution = "poisson", exposure = "", offset = "",
                                               trials_variable = "", trials_constant = NA_real_,
                                               response_bounds = NULL,
                                               model_type = "legacy_generalized",
                                               event = "", reference = "", selected_rows = integer(),
                                               generation = 0L) {
  tryCatch({
    if (isTRUE(count_regression)) {
      ls_new_count_regression(
        data = group, response = response, terms = terms,
        distribution = count_distribution,
        exposure = if (nzchar(exposure)) exposure else NULL,
        offset = if (nzchar(offset)) offset else NULL,
        trials = if (nzchar(trials_variable)) trials_variable else trials_constant,
        scope = scope, name = id, native = TRUE, term_types = term_types,
        centered_predictors = centered_predictors,
        factor_reference_levels = factor_reference_levels,
        .selected_rows = selected_rows, .native_generation = generation,
        .allow_intercept_only = TRUE
      )
    } else if (isTRUE(binary)) {
      ls_new_binary_regression(
        data = group, response = response, terms = terms, link = link,
        offset = if (nzchar(offset)) offset else NULL,
        event = if (nzchar(event)) event else NULL,
        reference = if (nzchar(reference)) reference else NULL,
        scope = scope, name = id, native = TRUE, term_types = term_types,
        centered_predictors = centered_predictors,
        factor_reference_levels = factor_reference_levels,
        .selected_rows = selected_rows, .native_generation = generation
      )
    } else {
      ls_new_generalized_linear_model(
        data = group, response = response, terms = terms, family = family, link = link,
        response_bounds = response_bounds,
        offset = if (nzchar(offset)) offset else NULL,
        scope = scope, name = id, native = TRUE, term_types = term_types,
        centered_predictors = centered_predictors,
        factor_reference_levels = factor_reference_levels,
        .model_type = model_type,
        .selected_rows = selected_rows, .native_generation = generation
      )
    }
  }, error = function(e) {
    msg <- conditionMessage(e)
    message("LinkEDA generalized GLM task failed: ", msg)
    install_accepted <- .rls_offer_missing_packages_native(
      e, "generalized_glm", id, group
    )
    if (isTRUE(install_accepted)) return(invisible(NULL))
    .rls_mi_progress_event(id, group,
      if (inherits(e, "linkeda_mi_cancelled")) "cancelled" else "failed",
      if (inherits(e, "linkeda_mi_cancelled")) "cancelled" else "failed",
      completed = e$completed %||% 0L, total = e$total %||% 0L,
      elapsed = e$elapsed %||% 0, message = msg)
    if (inherits(e, "linkeda_mi_cancelled")) return(invisible(NULL))
    .rls_generalized_glm_sync_native_error(
      id = id, group = group, response = response, family = family, link = link,
      scope = scope, terms = terms, term_types = term_types,
      centered_predictors = centered_predictors,
      factor_reference_levels = factor_reference_levels,
      message = paste(if (isTRUE(count_regression)) "Could not fit count regression:" else if (isTRUE(binary)) "Could not fit binary regression:" else "Could not fit generalized linear model:", msg),
      binary = binary, event = event, reference = reference,
      count = count_regression, count_distribution = count_distribution,
      exposure = exposure, offset = offset,
      trials_variable = trials_variable, trials_constant = trials_constant,
      response_bounds = response_bounds,
      model_type = model_type, generation = generation
    )
  })
  invisible(NULL)
}

.rls_handle_mixed_model_needed <- function(parts) {
  if (length(parts) < 9L) return(invisible(NULL))
  id <- parts[2L]
  group <- parts[3L]
  model_type <- parts[4L]
  response <- parts[5L]
  method <- parts[6L]
  family <- parts[7L]
  link <- parts[8L]
  cursor <- 9L
  fixed_count <- suppressWarnings(as.integer(parts[cursor]))
  if (!is.finite(fixed_count) || fixed_count < 0L) return(invisible(NULL))
  cursor <- cursor + 1L
  fixed <- character()
  if (fixed_count > 0L) {
    if (length(parts) < cursor + fixed_count - 1L) return(invisible(NULL))
    fixed <- parts[seq.int(cursor, cursor + fixed_count - 1L)]
    cursor <- cursor + fixed_count
  }
  if (length(parts) < cursor) return(invisible(NULL))
  random_count <- suppressWarnings(as.integer(parts[cursor]))
  if (!is.finite(random_count) || random_count < 0L) return(invisible(NULL))
  cursor <- cursor + 1L
  random <- vector("list", random_count)
  for (i in seq_len(random_count)) {
    if (length(parts) < cursor + 2L) return(invisible(NULL))
    random_group <- parts[cursor]
    covariance <- parts[cursor + 1L]
    term_count <- suppressWarnings(as.integer(parts[cursor + 2L]))
    if (!is.finite(term_count) || term_count < 0L) return(invisible(NULL))
    cursor <- cursor + 3L
    terms <- "1"
    if (term_count > 0L) {
      if (length(parts) < cursor + term_count - 1L) return(invisible(NULL))
      terms <- parts[seq.int(cursor, cursor + term_count - 1L)]
      cursor <- cursor + term_count
    }
    random[[i]] <- list(group = random_group, terms = terms, covariance_structure = covariance)
  }
  task_scope <- if (length(parts) >= cursor && parts[[cursor]] %in% c("all", "selected")) {
    value <- parts[[cursor]]; cursor <- cursor + 1L; value
  } else "all"
  row_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[cursor])) else 0L
  cursor <- cursor + 1L
  task_rows <- if (is.finite(row_count) && row_count > 0L && length(parts) >= cursor + row_count - 1L)
    suppressWarnings(as.integer(parts[seq.int(cursor, cursor + row_count - 1L)])) else integer()
  task_rows <- task_rows[is.finite(task_rows) & task_rows > 0L]

  tryCatch({
    if (identical(model_type, "generalized_linear_mixed_model")) {
      ls_new_generalized_mixed_model(
        data = group, response = response, fixed = fixed, random = random,
        family = family, link = link, scope = task_scope, name = id, native = TRUE,
        .selected_rows = task_rows
      )
    } else {
      ls_new_linear_mixed_model(
        data = group, response = response, fixed = fixed, random = random,
        method = if (nzchar(method)) method else "REML", scope = task_scope, name = id, native = TRUE,
        .selected_rows = task_rows
      )
    }
  }, error = function(e) {
    message("LinkEDA mixed-model task failed: ", conditionMessage(e))
    lines <- strsplit(paste("Mixed model failed:", conditionMessage(e)), "\n", fixed = TRUE)[[1L]]
    try(.rls_send(c("MIXED_MODEL_OPEN_TEXT", id, group, model_type, as.character(length(lines)), lines)), silent = TRUE)
  })
  invisible(NULL)
}

.rls_smooth_data_record <- function(plot_id, group) {
  plot_rec <- .rls_get_plot(plot_id)
  if (!is.null(plot_rec)) return(plot_rec)
  if (!is.character(group) || length(group) != 1L || !nzchar(group) ||
      !exists(group, envir = .rls_state$datasets, inherits = FALSE)) {
    return(NULL)
  }
  dataset <- get(group, envir = .rls_state$datasets, inherits = FALSE)
  list(group = dataset$group, data = dataset$data)
}

.rls_smooth_curves_for_point_set <- function(rows, x, y, scope, span,
                                             selected_rows = NULL,
                                             row_colors = NULL,
                                             imputation = NULL,
                                             fit_method = "loess",
                                             confidence_level = 0.95) {
  finite <- is.finite(rows) & rows > 0L & is.finite(x) & is.finite(y)
  rows <- as.integer(rows[finite]); x <- as.double(x[finite]); y <- as.double(y[finite])
  if (!length(rows)) return(list())

  tag <- function(curve) {
    if (!is.null(imputation) && is.finite(imputation) && imputation > 0L) {
      curve$group <- paste0(curve$group, "\x1fmi:", as.integer(imputation))
    }
    curve
  }
  if (scope == "selected") {
    idx <- which(rows %in% (selected_rows %||% integer()))
    if (!length(idx)) return(list())
    curve <- .rls_compute_smooth(x[idx], y[idx], ".", span, 200,
                                 confidence_level, fit_method)
    return(if (isTRUE(curve$ok)) list(tag(curve)) else list())
  }
  if (scope == "color") {
    color_groups <- split(seq_along(rows), .rls_colors_for_rows(rows, row_colors))
    curves <- list()
    for (color_name in names(color_groups)) {
      idx <- color_groups[[color_name]]
      if (length(idx) < (if (identical(fit_method, "lm")) 2L else 3L)) next
      curve <- .rls_compute_smooth(x[idx], y[idx], color_name, span, 200,
                                   confidence_level, fit_method)
      if (isTRUE(curve$ok)) curves <- c(curves, list(tag(curve)))
    }
    return(curves)
  }
  curve <- .rls_compute_smooth(x, y, ".", span, 200,
                               confidence_level, fit_method)
  if (isTRUE(curve$ok)) list(tag(curve)) else list()
}

.rls_handle_smooth_needed <- function(plot_id, group, scope,
                                      x_col = NULL, y_col = NULL, span = 0.75,
                                      selected_rows = NULL, row_colors = NULL,
                                      imputation_point_sets = list(),
                                      fit_method = "loess",
                                      confidence_level = 0.95,
                                      visible_rows = NULL) {
  if (isTRUE(.rls_state$processing_control)) {
    return(invisible(NULL))
  }
  .rls_state$processing_control <- TRUE
  on.exit(.rls_state$processing_control <- FALSE)

  plot_rec <- .rls_smooth_data_record(plot_id, group)
  if (is.null(plot_rec)) return(invisible(NULL))

  if (length(imputation_point_sets)) {
    if (!is.finite(span)) span <- 0.75
    span <- max(0.20, min(2.00, span))
    curves <- list()
    for (point_set in imputation_point_sets) {
      keep <- if (is.null(visible_rows)) rep.int(TRUE, length(point_set$rows)) else
        point_set$rows %in% visible_rows
      curves <- c(curves, .rls_smooth_curves_for_point_set(
        point_set$rows[keep], point_set$x[keep], point_set$y[keep], scope, span,
        selected_rows, row_colors, point_set$imputation,
        fit_method, confidence_level
      ))
    }
    if (!length(curves)) {
      .rls_send_smooth(plot_id, scope, list(), fit_method)
    } else {
      .rls_send_smooth(plot_id, scope, curves, fit_method)
    }
    return(invisible(NULL))
  }

  if (!is.null(x_col) && !is.null(y_col) &&
      x_col %in% names(plot_rec$data) && y_col %in% names(plot_rec$data)) {
    plot_rec$x <- x_col
    plot_rec$y <- y_col
    ok <- stats::complete.cases(plot_rec$data[, c(x_col, y_col), drop = FALSE])
    plot_rec$rows <- which(ok)
    plot_rec$n <- length(plot_rec$rows)
    assign(plot_id, plot_rec, envir = .rls_state$plots)
  }

  data <- plot_rec$data
  x_col <- plot_rec$x
  y_col <- plot_rec$y
  ok <- stats::complete.cases(data[, c(x_col, y_col), drop = FALSE])
  rows <- which(ok)
  if (!is.null(visible_rows)) rows <- rows[rows %in% visible_rows]
  if (!length(rows)) {
    .rls_send_smooth(plot_id, scope, list(), fit_method)
    return(invisible(NULL))
  }

  x <- as.double(data[[x_col]][rows])
  y <- as.double(data[[y_col]][rows])
  if (!is.finite(span)) span <- 0.75
  span <- max(0.20, min(2.00, span))

  if (scope == "selected") {
    if (is.null(selected_rows)) selected_rows <- ls_selected(plot_rec$group)
    idx <- which(rows %in% selected_rows)
    if (!length(idx)) {
      .rls_send_smooth(plot_id, scope, list(), fit_method)
      return(invisible(NULL))
    }
    curves <- list(.rls_compute_smooth(x[idx], y[idx], ".", span, 200,
                                       confidence_level, fit_method))
  } else if (scope == "color") {
    if (is.null(row_colors)) {
      color_reply <- .rls_send(c("POINT_COLORS", plot_rec$group))
      if (identical(color_reply, "OK")) {
        .rls_send_smooth(plot_id, scope, list(), fit_method)
        return(invisible(NULL))
      }
      records <- .rls_parse_records(color_reply)
      row_colors <- character()
      for (rec in records) {
        parts <- strsplit(rec, "|", fixed = TRUE)[[1L]]
        if (length(parts) >= 2L) {
          row_colors[[parts[1L]]] <- parts[2L]
        }
      }
    }
    color_groups <- split(seq_along(rows), .rls_colors_for_rows(rows, row_colors))
    curves <- list()
    for (color_name in names(color_groups)) {
      idx <- color_groups[[color_name]]
      if (length(idx) < (if (identical(fit_method, "lm")) 2L else 3L)) next
      cv <- .rls_compute_smooth(x[idx], y[idx], color_name, span, 200,
                                confidence_level, fit_method)
      if (isTRUE(cv$ok)) curves <- c(curves, list(cv))
    }
    if (!length(curves)) {
      .rls_send_smooth(plot_id, scope, list(), fit_method)
      return(invisible(NULL))
    }
  } else {
    curves <- list(.rls_compute_smooth(x, y, ".", span, 200,
                                       confidence_level, fit_method))
  }

  .rls_send_smooth(plot_id, scope, curves, fit_method)
  invisible(NULL)
}

.rls_handle_trellis_smooth_needed <- function(plot_id, group, panel_id, scope,
                                               x_col, y_col, span = 0.75,
                                               panel_rows = integer(),
                                               panel_colors = character(),
                                               selected_rows = NULL,
                                               row_colors = NULL,
                                               fit_method = "loess",
                                               confidence_level = 0.95,
                                               explicit_points = NULL) {
  if (isTRUE(.rls_state$processing_control)) return(invisible(NULL))
  .rls_state$processing_control <- TRUE
  on.exit(.rls_state$processing_control <- FALSE)

  plot_rec <- .rls_smooth_data_record(plot_id, group)
  if (is.null(plot_rec)) return(invisible(NULL))
  if (is.null(explicit_points) &&
      (!x_col %in% names(plot_rec$data) || !y_col %in% names(plot_rec$data)))
    return(invisible(NULL))
  data <- plot_rec$data
  rows <- if (is.null(explicit_points)) as.integer(panel_rows) else
    as.integer(explicit_points$rows)
  colors <- as.character(panel_colors)
  if (length(colors) != length(rows)) colors <- character()
  valid <- is.finite(rows) & rows >= 1L &
    if (is.null(explicit_points)) rows <= nrow(data) else
      is.finite(explicit_points$x) & is.finite(explicit_points$y)
  rows <- rows[valid]
  if (length(colors)) colors <- colors[valid]
  if (is.null(explicit_points)) {
    complete <- if (length(rows)) stats::complete.cases(data[rows, c(x_col, y_col), drop = FALSE]) else logical()
    rows <- rows[complete]
    if (length(colors)) colors <- colors[complete]
  }
  if (!length(rows)) {
    .rls_send_trellis_smooth(plot_id, panel_id, scope, list(), fit_method)
    return(invisible(NULL))
  }
  x <- if (is.null(explicit_points)) as.double(data[[x_col]][rows]) else
    as.double(explicit_points$x[valid])
  y <- if (is.null(explicit_points)) as.double(data[[y_col]][rows]) else
    as.double(explicit_points$y[valid])
  if (!is.finite(span)) span <- 0.75
  span <- max(0.20, min(2.00, span))

  eligible_rows <- if (scope == "selected") {
    if (is.null(selected_rows)) ls_selected(plot_rec$group) else selected_rows
  } else rows
  eligible <- which(rows %in% eligible_rows)
  if (length(colors)) {
    color_groups <- split(eligible, colors[eligible])
    curves <- list()
    for (color_name in names(color_groups)) {
      idx <- color_groups[[color_name]]
      if (length(idx) < (if (identical(fit_method, "lm")) 2L else 3L)) next
      cv <- .rls_compute_smooth(x[idx], y[idx], color_name, span, 200,
                                confidence_level, fit_method)
      if (isTRUE(cv$ok)) curves <- c(curves, list(cv))
    }
  } else if (scope == "selected") {
    idx <- eligible
    curves <- if (length(idx)) list(.rls_compute_smooth(
      x[idx], y[idx], ".", span, 200, confidence_level, fit_method
    )) else list()
  } else if (scope == "color") {
    if (is.null(row_colors)) {
      color_reply <- .rls_send(c("POINT_COLORS", plot_rec$group))
      records <- if (identical(color_reply, "OK")) list() else .rls_parse_records(color_reply)
      row_colors <- character()
      for (rec in records) {
        color_parts <- strsplit(rec, "|", fixed = TRUE)[[1L]]
        if (length(color_parts) >= 2L) row_colors[[color_parts[1L]]] <- color_parts[2L]
      }
    }
    color_groups <- split(seq_along(rows), .rls_colors_for_rows(rows, row_colors))
    curves <- list()
    for (color_name in names(color_groups)) {
      idx <- color_groups[[color_name]]
      if (length(idx) < (if (identical(fit_method, "lm")) 2L else 3L)) next
      cv <- .rls_compute_smooth(x[idx], y[idx], color_name, span, 200,
                                confidence_level, fit_method)
      if (isTRUE(cv$ok)) curves <- c(curves, list(cv))
    }
  } else {
    curves <- list(.rls_compute_smooth(x, y, ".", span, 200,
                                       confidence_level, fit_method))
  }
  curves <- Filter(function(curve) isTRUE(curve$ok), curves)
  .rls_send_trellis_smooth(plot_id, panel_id, scope, curves, fit_method)
  invisible(NULL)
}

.rls_send_trellis_smooth <- function(plot_id, panel_id, scope, curves,
                                     fit_method = "loess") {
  lines <- c("ADD_TRELLIS_SMOOTH", plot_id, panel_id, scope,
             "FIT_CURVE_V2", fit_method, as.character(length(curves)))
  for (cv in curves) {
    n <- length(cv$x)
    lines <- c(lines, cv$group, if (isTRUE(cv$ok)) "1" else "0", as.character(n),
               if (n) paste(sprintf("%.17g", cv$x), collapse = " ") else "",
               if (n) paste(sprintf("%.17g", cv$y), collapse = " ") else "",
               if (n) paste(sprintf("%.17g", cv$lower %||% numeric()), collapse = " ") else "",
               if (n) paste(sprintf("%.17g", cv$upper %||% numeric()), collapse = " ") else "")
  }
  .rls_send(lines)
}
