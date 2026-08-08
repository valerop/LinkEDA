.rls_data_exchange_id <- function(prefix = "data") {
  paste(prefix, Sys.getpid(), format(Sys.time(), "%Y%m%d%H%M%OS6"),
        sample.int(.Machine$integer.max, 1L), sep = "-")
}

.rls_repair_data_names <- function(data) {
  old <- names(data)
  if (is.null(old)) old <- rep("", ncol(data))
  clean <- trimws(gsub("[\r\n\t|]+", " ", old))
  clean[is.na(clean) | !nzchar(clean)] <- "variable"
  clean <- make.unique(clean, sep = "_")
  names(data) <- clean
  data
}

.rls_safe_remove_exchange_directory <- function(path, prefix) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    return(invisible(FALSE))
  }
  normalized <- normalizePath(path, mustWork = FALSE)
  allowed_roots <- unique(normalizePath(c(tempdir(), dirname(tempdir())), mustWork = FALSE))
  inside_root <- any(startsWith(paste0(normalized, .Platform$file.sep),
                                paste0(allowed_roots, .Platform$file.sep)))
  if (!inside_root || !startsWith(basename(normalized), prefix)) {
    return(invisible(FALSE))
  }
  unlink(normalized, recursive = TRUE, force = TRUE)
  invisible(!file.exists(normalized))
}

.rls_read_native_data_payload <- function(path) {
  if (!file.exists(path)) {
    stop("LinkEDA did not produce the data-return payload.", call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (length(lines) < 4L || !identical(lines[[1L]], "DATASET")) {
    stop("The data returned by LinkEDA is malformed.", call. = FALSE)
  }
  n_rows <- suppressWarnings(as.integer(lines[[3L]]))
  n_columns <- suppressWarnings(as.integer(lines[[4L]]))
  if (is.na(n_rows) || is.na(n_columns) || n_rows < 0L || n_columns < 0L) {
    stop("The returned data dimensions are invalid.", call. = FALSE)
  }
  cursor <- 5L
  columns <- vector("list", n_columns)
  column_names <- character(n_columns)
  types <- character(n_columns)
  for (column in seq_len(n_columns)) {
    if (cursor + 1L > length(lines)) {
      stop("The returned data column header is incomplete.", call. = FALSE)
    }
    column_names[[column]] <- lines[[cursor]]
    types[[column]] <- lines[[cursor + 1L]]
    cursor <- cursor + 2L
    last <- cursor + n_rows - 1L
    if (n_rows > 0L && last > length(lines)) {
      stop("The returned data column is incomplete.", call. = FALSE)
    }
    columns[[column]] <- if (n_rows) lines[cursor:last] else character()
    cursor <- cursor + n_rows
  }
  names(columns) <- column_names
  list(group = lines[[2L]], rows = n_rows, columns = columns, types = types)
}

.rls_native_missing <- function(values) {
  is.na(values) | values %in% c("", "NA", "NaN")
}

.rls_restore_native_column <- function(values, type, original = NULL) {
  values <- as.character(values)
  missing <- .rls_native_missing(values)

  if (!is.null(original) && inherits(original, "Date")) {
    restored <- suppressWarnings(as.Date(values))
    restored[missing] <- as.Date(NA)
    return(restored)
  }
  if (!is.null(original) && inherits(original, c("POSIXct", "POSIXlt"))) {
    timezone <- attr(original, "tzone", exact = TRUE)
    if (is.null(timezone) || !length(timezone) || !nzchar(timezone[[1L]])) timezone <- ""
    restored <- suppressWarnings(as.POSIXct(values, tz = timezone[[1L]]))
    restored[missing] <- as.POSIXct(NA, tz = timezone[[1L]])
    if (inherits(original, "POSIXlt")) restored <- as.POSIXlt(restored, tz = timezone[[1L]])
    return(restored)
  }
  if (!is.null(original) && inherits(original, "difftime")) {
    numeric_values <- suppressWarnings(as.numeric(values))
    numeric_values[missing] <- NA_real_
    return(as.difftime(numeric_values, units = attr(original, "units") %||% "secs"))
  }
  if ((!is.null(original) && is.factor(original)) || type %in% c("factor", "ordered")) {
    existing <- if (!is.null(original) && is.factor(original)) levels(original) else character()
    observed <- unique(values[!missing])
    return(factor(replace(values, missing, NA_character_),
                  levels = unique(c(existing, observed)),
                  ordered = !is.null(original) && is.ordered(original)))
  }
  if ((!is.null(original) && is.logical(original)) || identical(type, "logical")) {
    text <- tolower(values)
    restored <- rep(NA, length(values))
    restored[text %in% c("true", "t", "1", "yes", "y")] <- TRUE
    restored[text %in% c("false", "f", "0", "no", "n")] <- FALSE
    return(restored)
  }
  if ((!is.null(original) && (is.numeric(original) || is.integer(original))) ||
      identical(type, "numeric")) {
    restored <- suppressWarnings(as.numeric(values))
    restored[missing] <- NA_real_
    if (!is.null(original) && is.integer(original) &&
        all(is.na(restored) | restored == trunc(restored))) {
      restored <- as.integer(restored)
    }
    if (!is.null(original)) {
      preserved <- attributes(original)
      preserved$names <- NULL
      if (length(preserved)) attributes(restored) <- preserved
    }
    return(restored)
  }
  restored <- values
  restored[missing] <- NA_character_
  if (!is.null(original)) {
    preserved <- attributes(original)
    preserved$names <- NULL
    preserved$levels <- NULL
    preserved$class <- NULL
    if (length(preserved)) attributes(restored) <- preserved
  }
  restored
}

.rls_merge_native_data <- function(payload, original) {
  columns <- vector("list", length(payload$columns))
  names(columns) <- names(payload$columns)
  for (index in seq_along(payload$columns)) {
    name <- names(payload$columns)[[index]]
    source <- if (name %in% names(original)) original[[name]] else NULL
    columns[[index]] <- .rls_restore_native_column(
      payload$columns[[index]], payload$types[[index]], source
    )
  }
  result <- as.data.frame(columns, check.names = FALSE, stringsAsFactors = FALSE,
                          optional = TRUE)
  if (!length(columns) && payload$rows > 0L) {
    result <- structure(list(), row.names = .set_row_names(payload$rows),
                        class = "data.frame")
  }
  if (nrow(result) == nrow(original)) row.names(result) <- row.names(original)
  original_attributes <- attributes(original)
  for (attribute in setdiff(names(original_attributes), c("names", "row.names", "class"))) {
    attr(result, attribute) <- original_attributes[[attribute]]
  }
  class(result) <- class(original)
  result
}

.rls_handle_r_data_return_needed <- function(parts) {
  if (length(parts) < 6L) return(invisible(FALSE))
  request_id <- parts[[2L]]
  mode <- parts[[5L]]
  count <- suppressWarnings(as.integer(parts[[6L]]))
  if (is.na(count) || count < 0L) count <- 0L
  rows <- if (count > 0L && length(parts) >= 6L + count) {
    suppressWarnings(as.integer(parts[seq.int(7L, 6L + count)]))
  } else integer()
  rows <- rows[!is.na(rows) & rows > 0L]
  requests <- .rls_state$r_data_return_requests
  results <- .rls_state$r_data_return_results
  if (!exists(request_id, envir = requests, inherits = FALSE)) {
    if (nzchar(parts[[4L]])) {
      .rls_safe_remove_exchange_directory(dirname(parts[[4L]]), "rlispstat-return-")
    }
    return(invisible(FALSE))
  }
  request <- get(request_id, envir = requests, inherits = FALSE)
  outcome <- tryCatch({
    if (identical(mode, "cancel")) {
      list(status = "cancel", data = NULL)
    } else {
      payload <- .rls_read_native_data_payload(parts[[4L]])
      original <- readRDS(request$original_path)
      data <- .rls_merge_native_data(payload, original)
      if (identical(mode, "selected")) data <- data[rows, , drop = FALSE]
      list(status = "return", data = data, mode = mode, rows = rows)
    }
  }, error = function(error) list(status = "error", error = conditionMessage(error)))
  assign(request_id, outcome, envir = results)
  if (nzchar(parts[[4L]])) {
    .rls_safe_remove_exchange_directory(dirname(parts[[4L]]), "rlispstat-return-")
  }
  invisible(TRUE)
}

.rls_handle_r_data_choice_needed <- function(parts) {
  if (length(parts) < 4L) return(invisible(FALSE))
  request_id <- parts[[2L]]
  menu_requests <- .rls_state$r_data_menu_requests
  if (exists(request_id, envir = menu_requests, inherits = FALSE)) {
    rm(list = request_id, envir = menu_requests)
    if (identical(parts[[3L]], "cancel")) return(invisible(TRUE))
    object_name <- parts[[4L]]
    outcome <- tryCatch({
      if (!exists(object_name, envir = .GlobalEnv, inherits = FALSE) ||
          !is.data.frame(get(object_name, envir = .GlobalEnv, inherits = FALSE))) {
        stop("The selected R data frame is no longer available.", call. = FALSE)
      }
      .rls_register_dataset(
        object_name,
        get(object_name, envir = .GlobalEnv, inherits = FALSE),
        source = "R menu",
        activate = TRUE,
        replace = TRUE
      )
      NULL
    }, error = function(error) conditionMessage(error))
    if (!is.null(outcome)) {
      try(.rls_send(c("WORKBENCH_MESSAGE", "Open data from R",
                      gsub("[[:cntrl:]]+", " ", outcome))), silent = TRUE)
    }
    return(invisible(TRUE))
  }
  assign(parts[[2L]], list(
    status = if (identical(parts[[3L]], "cancel")) "cancel" else "choose",
    object = parts[[4L]]
  ), envir = .rls_state$r_data_choice_results)
  invisible(TRUE)
}

.rls_handle_r_data_browse_needed <- function(parts) {
  if (length(parts) < 2L) return(invisible(FALSE))
  request_id <- parts[[2L]]
  objects <- ls(envir = .GlobalEnv, all.names = FALSE)
  objects <- objects[vapply(objects, function(name) {
    is.data.frame(get(name, envir = .GlobalEnv, inherits = FALSE)) &&
      !grepl("[[:cntrl:]|]", name)
  }, logical(1L))]
  if (!length(objects)) {
    try(.rls_send(c("WORKBENCH_MESSAGE", "Open data from R",
                    "There are no data frames in .GlobalEnv.")), silent = TRUE)
    return(invisible(TRUE))
  }
  assign(request_id, TRUE, envir = .rls_state$r_data_menu_requests)
  lines <- c("OPEN_R_DATA_CHOOSER_V2", request_id, ".GlobalEnv",
             as.character(length(objects)))
  for (object in objects) {
    data <- get(object, envir = .GlobalEnv, inherits = FALSE)
    lines <- c(lines, object, as.character(nrow(data)), as.character(ncol(data)),
               gsub("[[:cntrl:]|]+", " ", paste(class(data), collapse = ", ")))
  }
  outcome <- tryCatch(.rls_send(lines), error = function(error) error)
  if (inherits(outcome, "error")) {
    if (exists(request_id, envir = .rls_state$r_data_menu_requests, inherits = FALSE)) {
      rm(list = request_id, envir = .rls_state$r_data_menu_requests)
    }
    try(.rls_send(c("WORKBENCH_MESSAGE", "Open data from R",
                    gsub("[[:cntrl:]]+", " ", conditionMessage(outcome)))), silent = TRUE)
  }
  invisible(TRUE)
}

.rls_handle_welcome_action_needed <- function(parts) {
  if (length(parts) < 4L) return(invisible(FALSE))
  request_id <- parts[[2L]]
  action <- parts[[3L]]
  value <- parts[[4L]]
  clean <- function(x) gsub("[[:cntrl:]|]+", " ", as.character(x))
  outcome <- tryCatch({
    if (identical(action, "example")) {
      if (!value %in% c("mtcars", "Alien")) {
        stop("The requested example dataset is not available.", call. = FALSE)
      }
      if (identical(value, "Alien")) {
        path <- system.file("examples", "Alien.csv", package = "LinkEDA")
        if (!nzchar(path) || !file.exists(path)) {
          stop("The Alien example dataset could not be loaded.", call. = FALSE)
        }
        data <- utils::read.csv(path, stringsAsFactors = FALSE,
                                check.names = FALSE)
      } else {
        example_environment <- new.env(parent = emptyenv())
        utils::data(list = value, package = "datasets", envir = example_environment)
        if (!exists(value, envir = example_environment, inherits = FALSE)) {
          stop("The example dataset could not be loaded.", call. = FALSE)
        }
        data <- get(value, envir = example_environment, inherits = FALSE)
      }
      if (!is.data.frame(data)) stop("The example is not a data frame.", call. = FALSE)
      .rls_register_dataset(value, data, source = "Example dataset",
                            activate = TRUE, replace = TRUE)
    } else {
      stop("The requested Welcome action is not supported.", call. = FALSE)
    }
    list(ok = TRUE, message = "")
  }, error = function(error) list(ok = FALSE, message = conditionMessage(error)))
  try(.rls_send(c("WELCOME_ACTION_RESULT", request_id,
                  if (outcome$ok) "ok" else "error", action,
                  clean(outcome$message))), silent = TRUE)
  invisible(outcome$ok)
}

.rls_handle_r_data_assign_needed <- function(parts) {
  if (length(parts) < 8L) return(invisible(FALSE))
  request_id <- parts[[2L]]
  group <- parts[[3L]]
  payload_path <- parts[[4L]]
  object_name <- parts[[5L]]
  mode <- parts[[6L]]
  replace_existing <- identical(parts[[7L]], "1")
  count <- suppressWarnings(as.integer(parts[[8L]]))
  if (is.na(count) || count < 0L) count <- 0L
  rows <- if (count > 0L && length(parts) >= 8L + count) {
    suppressWarnings(as.integer(parts[seq.int(9L, 8L + count)]))
  } else integer()
  rows <- rows[!is.na(rows) & rows > 0L]

  outcome <- tryCatch({
    if (!nzchar(object_name) || grepl("[[:cntrl:]]", object_name)) {
      stop("The R object name is not valid.", call. = FALSE)
    }
    if (exists(object_name, envir = .GlobalEnv, inherits = FALSE) &&
        !replace_existing) {
      stop(sprintf("Object `%s` already exists. Choose another name or allow replacement.",
                   object_name), call. = FALSE)
    }
    payload <- .rls_read_native_data_payload(payload_path)
    original <- if (exists(group, envir = .rls_state$datasets, inherits = FALSE)) {
      get(group, envir = .rls_state$datasets, inherits = FALSE)$data
    } else data.frame()
    data <- .rls_merge_native_data(payload, original)
    full_data <- data
    if (identical(mode, "selected")) data <- data[rows, , drop = FALSE]
    assign(object_name, data, envir = .GlobalEnv)
    if (!identical(mode, "selected")) {
      .rls_register_dataset(group, full_data, source = "Returned from LinkEDA",
                            activate = FALSE, replace = TRUE)
    }
    list(ok = TRUE, message = sprintf(
      "Created `%s` in .GlobalEnv (%d rows, %d columns).",
      object_name, nrow(data), ncol(data)
    ))
  }, error = function(error) list(ok = FALSE, message = conditionMessage(error)))

  if (nzchar(payload_path)) {
    .rls_safe_remove_exchange_directory(dirname(payload_path), "rlispstat-return-")
  }
  message <- gsub("[[:cntrl:]]+", " ", outcome$message)
  try(.rls_send(c("R_DATA_ASSIGN_RESULT", request_id,
                  if (outcome$ok) "ok" else "error", object_name, message)),
      silent = TRUE)
  invisible(outcome$ok)
}

.rls_wait_for_exchange_result <- function(request_id, results, what) {
  repeat {
    if (exists(request_id, envir = results, inherits = FALSE)) {
      result <- get(request_id, envir = results, inherits = FALSE)
      rm(list = request_id, envir = results)
      return(result)
    }
    if (!isTRUE(.rls_state$process_started)) {
      stop(sprintf("LinkEDA closed before %s was completed.", what), call. = FALSE)
    }
    try(.rls_process_backend_tasks(), silent = TRUE)
    if (!isTRUE(.rls_state$process_started)) {
      stop(sprintf("LinkEDA closed before %s was completed.", what), call. = FALSE)
    }
    Sys.sleep(0.05)
  }
}

#' Edit a data frame in LinkEDA and return it to R
#'
#' Opens an editable, linked data sheet and waits until the user chooses
#' `File > Return Data to R`, `Return Selected Rows to R`, or cancels.
#' The original R object is never overwritten automatically.
#'
#' @param x A data frame, tibble, or tribble.
#' @param name Optional dataset name shown in LinkEDA.
#' @param cancel Value returned after `Cancel without Returning`: the original
#'   object (`"original"`) or `NULL` (`"null"`).
#' @return The edited data frame, the selected rows, the original object, or
#'   `NULL`, according to the action chosen in LinkEDA.
#' @export
edit_data <- function(x, name = NULL, cancel = c("original", "null")) {
  cancel <- match.arg(cancel)
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame, tibble, or tribble.", call. = FALSE)
  }
  expression <- deparse(substitute(x), nlines = 1L)
  x <- .rls_repair_data_names(x)
  if (is.null(name)) {
    name <- if (grepl("^[.A-Za-z][.A-Za-z0-9_]*$", expression)) expression else "data"
  }
  name <- .rls_validate_group(name, allow_null = FALSE)
  request_id <- .rls_data_exchange_id("edit")
  session_directory <- tempfile(paste0("rlispstat-edit-", request_id, "-"))
  dir.create(session_directory, recursive = TRUE)
  original_path <- file.path(session_directory, "original.rds")
  saveRDS(x, original_path, version = 3)

  .rls_start_backend()
  group <- .rls_register_dataset(name, x, source = "R edit_data", activate = TRUE,
                                 replace = TRUE)
  assign(request_id, list(group = group, original_path = original_path),
         envir = .rls_state$r_data_return_requests)
  complete <- FALSE
  on.exit({
    if (!complete && isTRUE(.rls_state$process_started)) {
      try(.rls_send(c("CLOSE_R_EDIT_SESSION", request_id)), silent = TRUE)
    }
    if (exists(request_id, envir = .rls_state$r_data_return_requests, inherits = FALSE)) {
      rm(list = request_id, envir = .rls_state$r_data_return_requests)
    }
    if (exists(request_id, envir = .rls_state$r_data_return_results, inherits = FALSE)) {
      rm(list = request_id, envir = .rls_state$r_data_return_results)
    }
    .rls_safe_remove_exchange_directory(session_directory, "rlispstat-edit-")
  }, add = TRUE)

  .rls_send(c("OPEN_R_EDIT_SESSION", request_id, group))
  result <- .rls_wait_for_exchange_result(
    request_id, .rls_state$r_data_return_results, "the data return"
  )
  complete <- TRUE
  if (identical(result$status, "error")) stop(result$error, call. = FALSE)
  if (identical(result$status, "cancel")) {
    return(if (identical(cancel, "original")) readRDS(original_path) else NULL)
  }
  result$data
}

#' Choose a data frame from R and edit it in LinkEDA
#'
#' @param envir Environment containing the data frames. The default is the
#'   calling session's global environment.
#' @param cancel Passed to [edit_data()] after a data frame is chosen.
#' @return The value returned by [edit_data()], or `NULL` if the chooser is
#'   cancelled.
#' @export
choose_data <- function(envir = .GlobalEnv, cancel = c("original", "null")) {
  cancel <- match.arg(cancel)
  if (!is.environment(envir)) stop("`envir` must be an environment.", call. = FALSE)
  objects <- ls(envir = envir, all.names = FALSE)
  objects <- objects[vapply(objects, function(name) {
    is.data.frame(get(name, envir = envir, inherits = FALSE)) &&
      !grepl("[\r\n\t|]", name)
  }, logical(1L))]
  if (!length(objects)) {
    stop("No data frames are available in the selected R environment.", call. = FALSE)
  }
  environment_label <- environmentName(envir)
  if (!nzchar(environment_label)) environment_label <- "R environment"
  request_id <- .rls_data_exchange_id("choose")
  .rls_start_backend()
  lines <- c("OPEN_R_DATA_CHOOSER", request_id, environment_label,
             as.character(length(objects)))
  for (object in objects) {
    data <- get(object, envir = envir, inherits = FALSE)
    lines <- c(lines, object, as.character(nrow(data)), as.character(ncol(data)))
  }
  .rls_send(lines)
  result <- .rls_wait_for_exchange_result(
    request_id, .rls_state$r_data_choice_results, "the R data choice"
  )
  if (identical(result$status, "cancel")) return(NULL)
  if (!result$object %in% objects) {
    stop("The data frame selected in LinkEDA is no longer available.", call. = FALSE)
  }
  edit_data(get(result$object, envir = envir, inherits = FALSE),
            name = result$object, cancel = cancel)
}
