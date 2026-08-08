#' Return selected row indices
#'
#' Queries the native backend for the selected original row indices in a linked
#' brushing group.
#'
#' @param group A non-empty group name. If `NULL`, the only active group is
#'   used.
#' @return An integer vector of selected row indices.
#' @export
ls_selected <- function(group = NULL) {
  group <- .rls_resolve_group(group)
  reply <- .rls_send(c("SELECTED", group))
  if (identical(reply, "OK")) {
    return(integer())
  }
  if (!startsWith(reply, "OK ")) {
    stop("Unexpected reply from the LinkEDA backend.", call. = FALSE)
  }
  values <- strsplit(sub("^OK ", "", reply), " ", fixed = TRUE)[[1L]]
  values <- values[nzchar(values)]
  if (!length(values)) {
    integer()
  } else {
    as.integer(values)
  }
}

#' Set selected row indices
#'
#' Replaces the current selection for a linked brushing group. Row numbers are
#' original R data-frame row indices, not positions after missing-value removal.
#'
#' @param group A non-empty group name. If `NULL`, the only active group is used.
#' @param rows Integer row indices.
#' @return Invisibly returns `TRUE`.
#' @export
ls_set_selected <- function(group = NULL, rows) {
  group <- .rls_resolve_group(group)
  if (!is.numeric(rows) && !is.integer(rows)) {
    stop("`rows` must be numeric or integer row indices.", call. = FALSE)
  }
  rows <- sort(unique(as.integer(rows)))
  rows <- rows[!is.na(rows) & rows > 0L]
  .rls_send(c("SET_SELECTED", group, as.character(length(rows)), as.character(rows)))
  invisible(TRUE)
}

#' Clear selected points
#'
#' Clears the current selection in a linked brushing group and redraws any
#' matching native plots.
#'
#' @param group A non-empty group name. If `NULL`, the only active group is
#'   used.
#' @return Invisibly returns `TRUE`.
#' @export
ls_clear_selection <- function(group = NULL) {
  group <- .rls_resolve_group(group)
  .rls_send(c("CLEAR", group))
  invisible(TRUE)
}

#' Invert the current selection
#'
#' @param group A non-empty group name. If `NULL`, the only active group is used.
#' @return Invisibly returns `TRUE`.
#' @export
ls_invert_selection <- function(group = NULL) {
  group <- .rls_resolve_group(group)
  .rls_send(c("INVERT", group))
  invisible(TRUE)
}

#' Select all visible points in a group
#'
#' @param group A non-empty group name. If `NULL`, the only active group is used.
#' @return Invisibly returns `TRUE`.
#' @export
ls_select_all <- function(group = NULL) {
  group <- .rls_resolve_group(group)
  .rls_send(c("SELECT_ALL", group))
  invisible(TRUE)
}

#' Select rows by evaluating an expression in the group's data
#'
#' @param group A non-empty group name. If `NULL`, the only active group is used.
#' @param expr An expression evaluated in the data frame used to create the
#'   first plot in the group.
#' @return Invisibly returns `TRUE`.
#' @export
ls_select_where <- function(group = NULL, expr) {
  group <- .rls_resolve_group(group)
  if (!exists(group, envir = .rls_state$groups, inherits = FALSE)) {
    stop("No R data are registered for this group.", call. = FALSE)
  }
  data <- get(group, envir = .rls_state$groups)$data
  result <- eval(substitute(expr), envir = data, enclos = parent.frame())
  if (!is.logical(result) || length(result) != nrow(data)) {
    stop("`expr` must evaluate to a logical vector with one value per data row.", call. = FALSE)
  }
  rows <- which(stats::complete.cases(result) & result)
  ls_set_selected(group, rows)
}

#' Inspect or change the persistent analysis scope
#'
#' The analysis scope is an immutable snapshot of original data-frame row IDs
#' used by analyses and plots created afterwards. It is independent of the
#' linked brushing selection.
#'
#' @param group Registered dataset/group. If `NULL`, the active group is used.
#' @param name Required name for the immutable snapshot made from the current
#'   selection. This name is displayed in subsequent analysis output.
#' @param rows Original 1-based row IDs.
#' @param description Short description stored with the snapshot.
#' @return `ls_analysis_scope()` returns a list; the setters invisibly return
#'   `TRUE`.
#' @export
ls_analysis_scope <- function(group = NULL) {
  group <- .rls_resolve_group(group)
  reply <- .rls_send(c("GET_ANALYSIS_SCOPE", group))
  if (!startsWith(reply, "OK\t")) {
    stop(sub("^ERR\\s*", "", reply), call. = FALSE)
  }
  fields <- strsplit(sub("^OK\t", "", reply), "|", fixed = TRUE)[[1L]]
  count <- if (length(fields) >= 6L) suppressWarnings(as.integer(fields[[6L]])) else 0L
  rows <- if (is.finite(count) && count > 0L && length(fields) >= 6L + count)
    suppressWarnings(as.integer(fields[seq.int(7L, 6L + count)])) else integer()
  list(
    dataset_id = group,
    kind = fields[[1L]],
    source = fields[[2L]],
    description = utils::URLdecode(fields[[3L]]),
    n = suppressWarnings(as.integer(fields[[4L]])),
    total_n = suppressWarnings(as.integer(fields[[5L]])),
    rows = rows
  )
}

.rls_analysis_selection_name <- function(name, argument = "name") {
  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    stop(sprintf("`%s` must identify the selected observations.", argument), call. = FALSE)
  }
  name <- trimws(name)
  if (!nzchar(name)) {
    stop(sprintf("`%s` must identify the selected observations.", argument), call. = FALSE)
  }
  codepoints <- utf8ToInt(enc2utf8(name))
  if (grepl("|", name, fixed = TRUE) ||
      any(codepoints < 32L | codepoints == 127L)) {
    stop(sprintf("`%s` cannot contain control characters or `|`.", argument), call. = FALSE)
  }
  name
}

#' @rdname ls_analysis_scope
#' @export
ls_use_selected_as_analysis_scope <- function(group = NULL, name = NULL) {
  group <- .rls_resolve_group(group)
  name <- .rls_analysis_selection_name(name)
  reply <- .rls_send(c("SET_ANALYSIS_SCOPE_FROM_SELECTION", group,
                       paste0("Selection: ", name), "R"))
  if (startsWith(reply, "ERR")) stop(sub("^ERR\\s*", "", reply), call. = FALSE)
  invisible(TRUE)
}

#' @rdname ls_analysis_scope
#' @export
ls_saved_selections <- function(group = NULL) {
  group <- .rls_resolve_group(group)
  reply <- .rls_send(c("GET_SAVED_ANALYSIS_SCOPES", group))
  if (!startsWith(reply, "OK\t")) {
    stop(sub("^ERR\\s*", "", reply), call. = FALSE)
  }
  fields <- strsplit(sub("^OK\t", "", reply), "|", fixed = TRUE)[[1L]]
  count <- suppressWarnings(as.integer(fields[[1L]]))
  if (!is.finite(count) || count <= 0L) {
    return(data.frame(name = character(), n = integer(), rows = I(list())))
  }
  cursor <- 2L
  names <- character(count)
  sizes <- integer(count)
  rows <- vector("list", count)
  for (index in seq_len(count)) {
    names[[index]] <- utils::URLdecode(fields[[cursor]])
    sizes[[index]] <- suppressWarnings(as.integer(fields[[cursor + 1L]]))
    cursor <- cursor + 2L
    if (!is.finite(sizes[[index]]) || sizes[[index]] < 0L ||
        cursor + sizes[[index]] - 1L > length(fields)) {
      stop("Malformed saved-selection reply from LinkEDA.", call. = FALSE)
    }
    rows[[index]] <- if (sizes[[index]] == 0L) integer() else
      suppressWarnings(as.integer(fields[seq.int(cursor, cursor + sizes[[index]] - 1L)]))
    cursor <- cursor + sizes[[index]]
  }
  data.frame(name = names, n = sizes, rows = I(rows), check.names = FALSE)
}

#' @rdname ls_analysis_scope
#' @export
ls_use_saved_analysis_scope <- function(group = NULL, name) {
  group <- .rls_resolve_group(group)
  name <- .rls_analysis_selection_name(name)
  reply <- .rls_send(c("USE_SAVED_ANALYSIS_SCOPE", group, name))
  if (startsWith(reply, "ERR")) stop(sub("^ERR\\s*", "", reply), call. = FALSE)
  invisible(TRUE)
}

#' @rdname ls_analysis_scope
#' @export
ls_add_saved_analysis_scope <- function(group = NULL, name, result_name) {
  group <- .rls_resolve_group(group)
  name <- .rls_analysis_selection_name(name)
  result_name <- .rls_analysis_selection_name(result_name, "result_name")
  reply <- .rls_send(c("ADD_SAVED_ANALYSIS_SCOPE", group, name, result_name))
  if (startsWith(reply, "ERR")) stop(sub("^ERR\\s*", "", reply), call. = FALSE)
  invisible(TRUE)
}

#' @rdname ls_analysis_scope
#' @export
ls_use_all_observations <- function(group = NULL) {
  group <- .rls_resolve_group(group)
  reply <- .rls_send(c("SET_ANALYSIS_SCOPE_ALL", group))
  if (startsWith(reply, "ERR")) stop(sub("^ERR\\s*", "", reply), call. = FALSE)
  invisible(TRUE)
}

#' @rdname ls_analysis_scope
#' @export
ls_set_analysis_scope <- function(group = NULL, rows, description = "Explicit rows from R") {
  group <- .rls_resolve_group(group)
  rows <- unique(as.integer(rows))
  rows <- rows[is.finite(rows) & rows > 0L]
  reply <- .rls_send(c("SET_ANALYSIS_SCOPE_FROM_ROWS", group,
                       as.character(length(rows)), as.character(rows),
                       "other_explicit_subset", as.character(description), "R", ""))
  if (startsWith(reply, "ERR")) stop(sub("^ERR\\s*", "", reply), call. = FALSE)
  invisible(TRUE)
}
