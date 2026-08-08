.rls_state <- new.env(parent = emptyenv())
.rls_state$process_started <- FALSE
.rls_state$control_path <- NULL
.rls_state$control_port <- NULL
.rls_state$notify_path <- NULL
.rls_state$task_handler_started <- FALSE
.rls_state$processing_backend_tasks <- FALSE
.rls_state$task_poll_generation <- 0L
.rls_state$backend <- NULL
.rls_state$plots <- new.env(parent = emptyenv())
.rls_state$groups <- new.env(parent = emptyenv())
.rls_state$datasets <- new.env(parent = emptyenv())
.rls_state$glm_models <- new.env(parent = emptyenv())
.rls_state$generalized_glm_models <- new.env(parent = emptyenv())
.rls_state$glm_diagnostic_plots <- new.env(parent = emptyenv())
.rls_state$regression_comparisons <- new.env(parent = emptyenv())
.rls_state$correlation_matrices <- new.env(parent = emptyenv())
.rls_state$dimensionality_models <- new.env(parent = emptyenv())
.rls_state$dendrograms <- new.env(parent = emptyenv())
.rls_state$missing_imputations <- new.env(parent = emptyenv())
.rls_state$table1_tables <- new.env(parent = emptyenv())
.rls_state$spreadplot_listeners <- new.env(parent = emptyenv())
.rls_state$spreadplot_messages <- list()
.rls_state$r_data_return_requests <- new.env(parent = emptyenv())
.rls_state$r_data_return_results <- new.env(parent = emptyenv())
.rls_state$r_data_choice_results <- new.env(parent = emptyenv())
.rls_state$r_data_menu_requests <- new.env(parent = emptyenv())
.rls_state$active_dataset <- NULL
.rls_state$plot_theme <- "classic"

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
    c("rlispstat_backend.exe", "rlispstat_backend")
  } else {
    c("LinkEDA", "rlispstat_backend")
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
  writeLines(lines, con, sep = "\n", useBytes = TRUE)
  flush(con)
  reply <- readLines(con, n = 1L, warn = FALSE)
  if (!length(reply)) stop("The LinkEDA WinUI preview closed the connection without replying.", call. = FALSE)
  if (startsWith(reply[[1L]], "ERR ")) stop(sub("^ERR ", "", reply[[1L]]), call. = FALSE)
  .rls_state$process_started <- TRUE
  .rls_state$control_port <- as.integer(port)
  reply[[1L]]
}

.rls_send <- function(lines, expect_reply = TRUE) {
  if (!isTRUE(.rls_state$process_started)) {
    stop("No active LinkEDA backend. Open a plot first with `ls_scatter()`.", call. = FALSE)
  }

  if (.Platform$OS.type == "windows") {
    con <- tryCatch(socketConnection("127.0.0.1", port = .rls_state$control_port,
                                     blocking = TRUE, open = "r+b", timeout = 5),
                    error = function(e) NULL)
    if (is.null(con)) {
      .rls_reset_backend_connection(clear_views = TRUE)
      stop("Could not connect to the LinkEDA native backend.", call. = FALSE)
    }
    on.exit(close(con), add = TRUE)
    writeLines(lines, con, sep = "\n", useBytes = TRUE)
    flush(con)
    if (!expect_reply) return(invisible(NULL))
    reply <- readLines(con, n = 1L, warn = FALSE)
    if (!length(reply)) stop("The LinkEDA backend closed the connection without replying.", call. = FALSE)
    if (startsWith(reply[[1L]], "ERR ")) stop(sub("^ERR ", "", reply[[1L]]), call. = FALSE)
    return(reply[[1L]])
  }
  if (!file.exists(.rls_state$control_path)) {
    .rls_reset_backend_connection(clear_views = TRUE)
    stop("Could not connect to the LinkEDA native backend.", call. = FALSE)
  }

  reply_path <- tempfile("rlispstat-reply-")
  status <- system2("mkfifo", reply_path, stdout = FALSE, stderr = FALSE)
  if (!identical(status, 0L)) {
    stop("Could not create a local reply pipe for the LinkEDA backend.", call. = FALSE)
  }
  on.exit(unlink(reply_path), add = TRUE)

  con <- suppressWarnings(file(.rls_state$control_path, open = "w"))
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
    .rls_process_control_lines(reply_lines[-1L])
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
    .rls_process_backend_tasks()
    later::later(tick, interval)
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
    writeLines("PING", con, sep = "\n", useBytes = TRUE)
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
  .rls_state$control_path <- NULL
  .rls_state$control_port <- NULL
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
      return(invisible(TRUE))
    }
    .rls_reset_backend_connection(clear_views = TRUE)
  }
  if (isFALSE(.rls_option("launch", TRUE))) {
    stop("Backend launching is disabled by option `LinkEDA.launch = FALSE`.", call. = FALSE)
  }

  backend <- .rls_backend_path()
  if (.Platform$OS.type == "windows") {
    port <- sample.int(20000L, 1L) + 40000L
    log <- tempfile("rlispstat-backend-", fileext = ".log")
    system2(backend, args = c("--port", port, "--parent-pid", Sys.getpid()),
            stdout = log, stderr = log, wait = FALSE)
    .rls_state$backend <- backend
    .rls_state$control_port <- port
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
        try(.rls_send(c("PLOT_THEME", .rls_state$plot_theme %||% "classic")), silent = TRUE)
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
  outcome <- try(.rls_send("MAIN_R_TASKS"), silent = TRUE)
  if (inherits(outcome, "try-error") && !.rls_backend_connection_alive()) {
    .rls_reset_backend_connection(clear_views = TRUE)
    return(invisible(FALSE))
  }
  invisible(!inherits(outcome, "try-error"))
}

.rls_stop_backend_task_handler <- function() {
  .rls_state$task_poll_generation <- .rls_state$task_poll_generation + 1L
  try(.Call("rls_unregister_task_handler", TRUE, PACKAGE = "LinkEDA"), silent = TRUE)
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
  if (is.numeric(x)) {
    "numeric"
  } else if (is.factor(x)) {
    "factor"
  } else if (is.character(x)) {
    "character"
  } else if (is.logical(x)) {
    "logical"
  } else if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    "datetime"
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

.rls_dataframe_payload <- function(data, metadata = NULL, dataset_record = NULL, max_cell_chars = 120) {
  names <- names(data)
  for (name in names) {
    .rls_validate_protocol_name(name, "variable name")
  }
  wire_value <- function(value) {
    value <- as.character(value %||% "")
    value[is.na(value)] <- ""
    gsub("[\r\n\t]+", " ", value)
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
    values <- substr(values, 1L, max_cell_chars)
    gsub("[\r\n\t|]", " ", values)
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
  lines <- c("DATAFRAME", as.character(nrow(data)), as.character(length(names)))
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
  level_columns <- names[vapply(data, is.factor, logical(1L))]
  if (length(level_columns)) {
    lines <- c(lines, "DATLEVELS", as.character(length(level_columns)))
    for (name in level_columns) {
      defined <- clean_values(levels(data[[name]]))
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
    for (name in imputed_names) {
      missing_rows <- which(mask[[name]])
      original_values <- clean_values(raw_values(original[[name]])[missing_rows])
      lines <- c(lines, name, as.character(length(missing_rows)), as.character(missing_rows), original_values)
      for (version in seq_len(m)) {
        version_values <- clean_values(raw_values(completed[[version]][[name]])[missing_rows])
        lines <- c(lines, version_values)
      }
    }
  }
  lines
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

.rls_process_control_lines <- function(lines) {
  for (line in lines) {
    if (startsWith(line, "SMOOTH_NEEDED|")) {
      parts <- strsplit(line, "|", fixed = TRUE)[[1L]]
      if (length(parts) >= 4L) {
        x_col <- if (length(parts) >= 5L) parts[5L] else NULL
        y_col <- if (length(parts) >= 6L) parts[6L] else NULL
        span <- if (length(parts) >= 7L) suppressWarnings(as.double(parts[7L])) else 0.75
        .rls_handle_smooth_needed(parts[2L], parts[3L], parts[4L], x_col, y_col, span)
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
        .rls_handle_trellis_smooth_needed(
          parts[2L], parts[3L], parts[4L], parts[5L], parts[6L], parts[7L],
          suppressWarnings(as.double(parts[8L])), panel_rows, panel_colors
        )
      }
    } else if (startsWith(line, "IMPORT_DATA_NEEDED\t")) {
      parts <- strsplit(line, "\t", fixed = TRUE)[[1L]]
      if (length(parts) >= 2L) {
        source_path <- if (length(parts) >= 3L) parts[[3L]] else ""
        remove_after <- length(parts) >= 4L && identical(parts[[4L]], "1")
        .rls_handle_import_data_needed(parts[[2L]], source_path, remove_after)
      }
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
    } else if (startsWith(line, "PLOT_EXPORT_NEEDED\t")) {
      .rls_handle_plot_export_needed(strsplit(line, "\t", fixed = TRUE)[[1L]])
    } else if (startsWith(line, "TRELLIS_PANEL_ANALYSIS_NEEDED\t")) {
      .rls_handle_trellis_panel_analysis_needed(strsplit(line, "\t", fixed = TRUE)[[1L]])
    } else if (startsWith(line, "COMPARE_MEANS_BATCH_NEEDED\t")) {
      parts <- strsplit(line, "\t", fixed = TRUE)[[1L]]
      .rls_handle_compare_means_batch_needed(parts)
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
    } else if (startsWith(line, "GGLM_NEEDED\t")) {
      parts <- strsplit(line, "\t", fixed = TRUE)[[1L]]
      if (length(parts) >= 8L) {
        term_count <- suppressWarnings(as.integer(parts[8L]))
        terms <- character()
        cursor <- 9L
        if (is.finite(term_count) && term_count > 0L && length(parts) >= cursor + term_count - 1L) {
          terms <- parts[seq.int(cursor, cursor + term_count - 1L)]
          cursor <- cursor + term_count
        }
        term_types <- list()
        if (length(parts) >= cursor) {
          type_count <- suppressWarnings(as.integer(parts[cursor])); cursor <- cursor + 1L
          if (is.finite(type_count) && type_count > 0L && length(parts) >= cursor + 2L * type_count - 1L) {
            for (i in seq_len(type_count)) {
              term <- parts[cursor]
              type <- parts[cursor + 1L]
              cursor <- cursor + 2L
              if (nzchar(term) && type %in% c("numeric", "factor")) {
                term_types[[term]] <- type
              }
            }
          }
        }
        mode <- if (length(parts) >= cursor) parts[cursor] else "GENERALIZED"; cursor <- cursor + 1L
        event <- if (length(parts) >= cursor) parts[cursor] else ""; cursor <- cursor + 1L
        reference <- if (length(parts) >= cursor) parts[cursor] else ""; cursor <- cursor + 1L
        row_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[cursor])) else 0L
        cursor <- cursor + 1L
        task_rows <- if (is.finite(row_count) && row_count > 0L && length(parts) >= cursor + row_count - 1L)
          suppressWarnings(as.integer(parts[seq.int(cursor, cursor + row_count - 1L)])) else integer()
        task_rows <- task_rows[is.finite(task_rows) & task_rows > 0L]
        .rls_handle_generalized_glm_needed(parts[2L], parts[3L], parts[4L], parts[5L], parts[6L], parts[7L], terms, term_types,
                                           binary = identical(mode, "BINARY"), event = event, reference = reference,
                                           selected_rows = task_rows)
      }
    } else if (startsWith(line, "MIXED_MODEL_NEEDED\t")) {
      parts <- strsplit(line, "\t", fixed = TRUE)[[1L]]
      .rls_handle_mixed_model_needed(parts)
    } else if (startsWith(line, "PCAFA_FACTOR_NEEDED\t")) {
      parts <- strsplit(line, "\t", fixed = TRUE)[[1L]]
      .rls_handle_dimensionality_needed(parts)
    }
  }
}

.rls_handle_import_data_needed <- function(path, source_path = "", remove_after = FALSE) {
  clean_message <- function(x) gsub("[\r\n\t|]+", " ", as.character(x))
  if (isTRUE(remove_after)) {
    staged_directory <- dirname(path)
    normalized_staging <- normalizePath(staged_directory, mustWork = FALSE)
    temporary_roots <- unique(normalizePath(
      c(tempdir(), Sys.getenv("TMPDIR", unset = dirname(tempdir()))),
      mustWork = FALSE
    ))
    safe_staging_directory <- startsWith(basename(staged_directory), "rlispstat-import-") &&
      any(vapply(temporary_roots, function(root) {
        identical(dirname(normalized_staging), root) ||
          startsWith(paste0(normalized_staging, .Platform$file.sep),
                     paste0(root, .Platform$file.sep))
      }, logical(1L)))
    if (safe_staging_directory) {
      on.exit(unlink(staged_directory, recursive = TRUE, force = TRUE), add = TRUE)
    }
  }
  tryCatch({
    import_name <- if (nzchar(source_path)) {
      tools::file_path_sans_ext(basename(source_path))
    } else NULL
    dataset <- ls_import_data(path, name = import_name)
    if (nzchar(source_path) && exists(dataset, envir = .rls_state$datasets, inherits = FALSE)) {
      record <- get(dataset, envir = .rls_state$datasets, inherits = FALSE)
      record$path <- normalizePath(source_path, mustWork = FALSE)
      assign(dataset, record, envir = .rls_state$datasets)
    }
    recent_path <- if (nzchar(source_path)) source_path else path
    .rls_send(c("IMPORT_DATA_RESULT", "ok",
                clean_message(sprintf("Imported `%s`.", dataset)),
                normalizePath(recent_path, mustWork = FALSE)))
  }, error = function(e) {
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
    } else if (identical(plot_kind, "histogram")) {
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

  previous_run_id <- .rls_state$compare_means_run_id
  .rls_state$compare_means_run_id <- run_id
  on.exit({ .rls_state$compare_means_run_id <- previous_run_id }, add = TRUE)
  tryCatch({
    switch(test_type,
      one_sample_t = ls_new_one_sample_t_test(data = group, response = responses, mu = test_value,
                                              alternative = alternative, conf_level = conf_level,
                                              p_adjust = p_adjust,
                                              method = if (identical(method, "wilcoxon")) "wilcoxon" else "student",
                                              scope = task_scope, .selected_rows = scope_rows),
      independent_t = ls_new_independent_samples_t_test(data = group, response = responses, group = group_var,
                                                        alternative = alternative, conf_level = conf_level,
                                                        var_equal = identical(method, "student"), method = method,
                                                        group_order = group_order,
                                                        p_adjust = p_adjust, scope = task_scope,
                                                        .selected_rows = scope_rows),
      paired_t = ls_new_paired_samples_t_test(data = group, pairs = pairs,
                                              alternative = alternative, conf_level = conf_level,
                                              p_adjust = p_adjust,
                                              method = if (identical(method, "wilcoxon")) "wilcoxon" else "student",
                                              scope = task_scope, .selected_rows = scope_rows),
      oneway_anova = ls_new_one_way_anova(data = group, response = responses, group = group_var,
                                          conf_level = conf_level, method = method, group_order = group_order,
                                          p_adjust = p_adjust, scope = task_scope,
                                          .selected_rows = scope_rows),
      stop(sprintf("Unknown compare-means task `%s`.", test_type), call. = FALSE)
    )
  }, error = function(e) {
    title <- switch(test_type,
      one_sample_t = "One-Sample t Test", independent_t = "Independent-Samples t Test",
      paired_t = "Paired-Samples t Test", oneway_anova = "One-Way ANOVA", "Compare Means")
    if (isTRUE(.rls_state$process_started)) {
      try(.rls_send(c("COMPARE_MEANS_BATCH_ERROR", run_id, title, conditionMessage(e))), silent = TRUE)
    }
    message("LinkEDA compare-means task failed: ", conditionMessage(e))
  })
  invisible(NULL)
}

.rls_handle_regcmp_needed <- function(parts) {
  if (length(parts) < 8L) return(invisible(NULL))
  cursor <- 2L
  id <- parts[cursor]; cursor <- cursor + 1L
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
    models[[i]] <- list(id = model_id, label = label, response = model_response, terms = terms)
  }
  row_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[cursor])) else 0L
  cursor <- cursor + 1L
  task_rows <- if (is.finite(row_count) && row_count > 0L && length(parts) >= cursor + row_count - 1L)
    suppressWarnings(as.integer(parts[seq.int(cursor, cursor + row_count - 1L)])) else integer()
  task_rows <- task_rows[is.finite(task_rows) & task_rows > 0L]
  tryCatch({
    dataset <- .rls_dataset_record(group)
    record <- list(
      id = id,
      group = group,
      data = dataset$data,
      dataset_type = dataset$dataset_type %||% "data_frame",
      analysis_backend = .rls_analysis_backend(dataset, "regression_comparison"),
      response = .rls_model_validate_response(dataset$data, response, "response"),
      uses_shared_response = TRUE,
      scope = scope,
      selected_rows = task_rows,
      auto_refit = auto_refit,
      term_rows = unique(c("(Intercept)", term_rows)),
      term_types = term_types,
      models = vector("list", length(models)),
      active_model = 1L
    )
    for (i in seq_along(models)) {
      spec <- models[[i]]
      model_response <- .rls_model_validate_response(dataset$data, spec$response %||% record$response, "response")
      terms <- unique(as.character(spec$terms %||% character()))
      record$term_rows <- unique(c(record$term_rows, terms))
      record$models[[i]] <- list(
        id = spec$id,
        label = spec$label,
        response = model_response,
        terms = terms,
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
    .rls_regcmp_sync_native_error(id, conditionMessage(e))
    message("LinkEDA regression comparison task failed: ", conditionMessage(e))
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
  row_count <- if (length(parts) >= cursor) suppressWarnings(as.integer(parts[cursor])) else 0L
  cursor <- cursor + 1L
  task_rows <- if (is.finite(row_count) && row_count > 0L && length(parts) >= cursor + row_count - 1L)
    suppressWarnings(as.integer(parts[seq.int(cursor, cursor + row_count - 1L)])) else integer()
  task_rows <- task_rows[is.finite(task_rows) & task_rows > 0L]
  tryCatch({
    dataset <- .rls_dataset_record(group)
    model_id <- .rls_glm_model_id(group)
    handle <- if (exists(model_id, envir = .rls_state$glm_models, inherits = FALSE)) {
      structure(list(id = model_id, group = group), class = "rlispstat_glm")
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
    record$term_types <- term_types
    record$is_stale <- TRUE
    record$fit <- NULL
    .rls_assign_glm_model(record)
    handle <- ls_glm_fit(handle)
    record <- .rls_glm_model_record(handle)
    .rls_glm_sync_native_update(record)
  }, error = function(e) {
    .rls_glm_sync_native_error(group, conditionMessage(e))
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
  task_rows <- task_rows[is.finite(task_rows) & task_rows > 0L]
  tryCatch({
    dataset <- .rls_dataset_record(group)
    selected_rows <- task_rows
    record <- list(
      id = id,
      group = group,
      dataset_id = group,
      data = dataset$data,
      variables = variables,
      method = method,
      n_components = n_components,
      scale = scale,
      missing_mode = missing,
      rotation = rotation,
      scope = scope,
      selected_rows = selected_rows,
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
    .rls_assign_dimension(record)
    .rls_dimension_sync_native_update(record)
  }, error = function(e) {
    message("LinkEDA dimensionality task failed: ", conditionMessage(e))
  })
  invisible(NULL)
}

.rls_handle_generalized_glm_needed <- function(id, group, response, family, link, scope, terms,
                                               term_types = list(), binary = FALSE,
                                               event = "", reference = "", selected_rows = integer()) {
  tryCatch({
    if (isTRUE(binary)) {
      ls_new_binary_regression(
        data = group, response = response, terms = terms, link = link,
        event = if (nzchar(event)) event else NULL,
        reference = if (nzchar(reference)) reference else NULL,
        scope = scope, name = id, native = TRUE, term_types = term_types,
        .selected_rows = selected_rows
      )
    } else {
      ls_new_generalized_linear_model(
        data = group, response = response, terms = terms, family = family, link = link,
        scope = scope, name = id, native = TRUE, term_types = term_types,
        .selected_rows = selected_rows
      )
    }
  }, error = function(e) {
    msg <- conditionMessage(e)
    message("LinkEDA generalized GLM task failed: ", msg)
    .rls_generalized_glm_sync_native_error(
      id = id, group = group, response = response, family = family, link = link,
      scope = scope, terms = terms, term_types = term_types,
      message = paste(if (isTRUE(binary)) "Could not fit binary regression:" else "Could not fit generalized linear model:", msg),
      binary = binary, event = event, reference = reference
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

.rls_handle_smooth_needed <- function(plot_id, group, scope,
                                      x_col = NULL, y_col = NULL, span = 0.75) {
  if (isTRUE(.rls_state$processing_control)) {
    return(invisible(NULL))
  }
  .rls_state$processing_control <- TRUE
  on.exit(.rls_state$processing_control <- FALSE)

  plot_rec <- .rls_smooth_data_record(plot_id, group)
  if (is.null(plot_rec)) return(invisible(NULL))

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
  if (!length(rows)) return(invisible(NULL))

  x <- as.double(data[[x_col]][rows])
  y <- as.double(data[[y_col]][rows])
  if (!is.finite(span)) span <- 0.75
  span <- max(0.20, min(2.00, span))

  if (scope == "selected") {
    selected_rows <- ls_selected(plot_rec$group)
    idx <- which(rows %in% selected_rows)
    if (!length(idx)) {
      .rls_send(c("ADD_SMOOTH", plot_id, scope, "0"))
      return(invisible(NULL))
    }
    curves <- list(.rls_compute_smooth(x[idx], y[idx], ".", span, 200))
  } else if (scope == "color") {
    color_reply <- .rls_send(c("POINT_COLORS", plot_id))
    if (identical(color_reply, "OK")) {
      .rls_send(c("ADD_SMOOTH", plot_id, scope, "0"))
      return(invisible(NULL))
    }
    records <- .rls_parse_records(color_reply)
    row_to_color <- list()
    for (rec in records) {
      parts <- strsplit(rec, "|", fixed = TRUE)[[1L]]
      if (length(parts) >= 2L) {
        row_to_color[[parts[1L]]] <- parts[2L]
      }
    }
    color_groups <- split(seq_along(rows), vapply(seq_along(rows), function(i) {
      row_to_color[[as.character(rows[i])]] %||% "black"
    }, character(1L)))
    curves <- list()
    for (color_name in names(color_groups)) {
      idx <- color_groups[[color_name]]
      if (length(idx) < 3L) next
      cv <- .rls_compute_smooth(x[idx], y[idx], color_name, span, 200)
      if (isTRUE(cv$ok)) curves <- c(curves, list(cv))
    }
    if (!length(curves)) {
      .rls_send(c("ADD_SMOOTH", plot_id, scope, "0"))
      return(invisible(NULL))
    }
  } else {
    curves <- list(.rls_compute_smooth(x, y, ".", span, 200))
  }

  .rls_send_smooth(plot_id, scope, curves)
  invisible(NULL)
}

.rls_handle_trellis_smooth_needed <- function(plot_id, group, panel_id, scope,
                                               x_col, y_col, span = 0.75,
                                               panel_rows = integer(),
                                               panel_colors = character()) {
  if (isTRUE(.rls_state$processing_control)) return(invisible(NULL))
  .rls_state$processing_control <- TRUE
  on.exit(.rls_state$processing_control <- FALSE)

  plot_rec <- .rls_smooth_data_record(plot_id, group)
  if (is.null(plot_rec) || !x_col %in% names(plot_rec$data) ||
      !y_col %in% names(plot_rec$data)) return(invisible(NULL))
  data <- plot_rec$data
  rows <- as.integer(panel_rows)
  colors <- as.character(panel_colors)
  if (length(colors) != length(rows)) colors <- character()
  valid <- is.finite(rows) & rows >= 1L & rows <= nrow(data)
  rows <- rows[valid]
  if (length(colors)) colors <- colors[valid]
  complete <- if (length(rows)) stats::complete.cases(data[rows, c(x_col, y_col), drop = FALSE]) else logical()
  rows <- rows[complete]
  if (length(colors)) colors <- colors[complete]
  if (!length(rows)) {
    .rls_send_trellis_smooth(plot_id, panel_id, scope, list())
    return(invisible(NULL))
  }
  x <- as.double(data[[x_col]][rows])
  y <- as.double(data[[y_col]][rows])
  if (!is.finite(span)) span <- 0.75
  span <- max(0.20, min(2.00, span))

  selected_rows <- if (scope == "selected") ls_selected(plot_rec$group) else rows
  eligible <- which(rows %in% selected_rows)
  if (length(colors)) {
    color_groups <- split(eligible, colors[eligible])
    curves <- list()
    for (color_name in names(color_groups)) {
      idx <- color_groups[[color_name]]
      if (length(idx) < 3L) next
      cv <- .rls_compute_smooth(x[idx], y[idx], color_name, span, 200)
      if (isTRUE(cv$ok)) curves <- c(curves, list(cv))
    }
  } else if (scope == "selected") {
    selected_rows <- ls_selected(plot_rec$group)
    idx <- which(rows %in% selected_rows)
    curves <- if (length(idx)) list(.rls_compute_smooth(x[idx], y[idx], ".", span, 200)) else list()
  } else if (scope == "color") {
    color_reply <- .rls_send(c("POINT_COLORS", plot_id))
    records <- if (identical(color_reply, "OK")) list() else .rls_parse_records(color_reply)
    row_to_color <- list()
    for (rec in records) {
      color_parts <- strsplit(rec, "|", fixed = TRUE)[[1L]]
      if (length(color_parts) >= 2L) row_to_color[[color_parts[1L]]] <- color_parts[2L]
    }
    color_groups <- split(seq_along(rows), vapply(rows, function(row) {
      row_to_color[[as.character(row)]] %||% "black"
    }, character(1L)))
    curves <- list()
    for (color_name in names(color_groups)) {
      idx <- color_groups[[color_name]]
      if (length(idx) < 3L) next
      cv <- .rls_compute_smooth(x[idx], y[idx], color_name, span, 200)
      if (isTRUE(cv$ok)) curves <- c(curves, list(cv))
    }
  } else {
    curves <- list(.rls_compute_smooth(x, y, ".", span, 200))
  }
  curves <- Filter(function(curve) isTRUE(curve$ok), curves)
  .rls_send_trellis_smooth(plot_id, panel_id, scope, curves)
  invisible(NULL)
}

.rls_send_trellis_smooth <- function(plot_id, panel_id, scope, curves) {
  lines <- c("ADD_TRELLIS_SMOOTH", plot_id, panel_id, scope, as.character(length(curves)))
  for (cv in curves) {
    n <- length(cv$x)
    lines <- c(lines, cv$group, if (isTRUE(cv$ok)) "1" else "0", as.character(n),
               if (n) paste(sprintf("%.17g", cv$x), collapse = " ") else "",
               if (n) paste(sprintf("%.17g", cv$y), collapse = " ") else "")
  }
  .rls_send(lines)
}
