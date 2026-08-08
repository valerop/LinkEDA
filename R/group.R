#' List active linked brushing groups
#'
#' @return A data frame with group name, plot count, and selected row count.
#' @export
ls_groups <- function() {
  if (!isTRUE(.rls_state$process_started)) {
    groups <- .rls_active_group_names()
    return(data.frame(
      group = groups,
      plots = vapply(groups, function(group) {
        sum(vapply(ls(envir = .rls_state$plots), function(id) {
          identical(get(id, envir = .rls_state$plots)$group, group)
        }, logical(1L)))
      }, integer(1L)),
      selected_n = integer(length(groups)),
      stringsAsFactors = FALSE
    ))
  }

  records <- .rls_parse_records(.rls_send("GROUPS"))
  if (!length(records)) {
    return(data.frame(group = character(), plots = integer(), selected_n = integer()))
  }
  parts <- strsplit(records, "|", fixed = TRUE)
  data.frame(
    group = vapply(parts, `[`, character(1L), 1L),
    plots = as.integer(vapply(parts, `[`, character(1L), 2L)),
    selected_n = as.integer(vapply(parts, `[`, character(1L), 3L)),
    stringsAsFactors = FALSE
  )
}

#' Return metadata for a linked brushing group
#'
#' @param group A non-empty group name.
#' @return A named list with group metadata.
#' @export
ls_group_info <- function(group) {
  group <- .rls_validate_group(group, allow_null = FALSE)
  records <- .rls_parse_records(.rls_send(c("GROUP_INFO", group)))
  if (length(records) != 1L) {
    stop("Unexpected group info reply from the LinkEDA backend.", call. = FALSE)
  }
  parts <- strsplit(records, "|", fixed = TRUE)[[1L]]
  list(
    group = parts[[1L]],
    selected_n = as.integer(parts[[2L]]),
    plots = parts[seq.int(3L, length(parts))]
  )
}
