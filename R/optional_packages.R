.rls_optional_packages <- c(
  "GPArotation", "MASS", "betareg", "coda", "emmeans", "gamlss",
  "gamlss.dist", "ggplot2", "glmmTMB", "gtsummary", "haven", "knitr",
  "lme4", "lmerTest", "mitml", "naniar", "readxl", "rstatix",
  "svglite", "tibble", "tidyselect", "tinytable", "writexl", "xml2"
)

.rls_optional_library <- function(create = FALSE) {
  version <- paste(R.version$major, sub("\\..*$", "", R.version$minor), sep = ".")
  path <- getOption(
    "LinkEDA.optional_library",
    file.path(tools::R_user_dir("LinkEDA", "data"), "library", version)
  )
  path <- path.expand(as.character(path)[[1L]])
  if (isTRUE(create) && !dir.exists(path)) {
    if (!dir.create(path, recursive = TRUE, showWarnings = FALSE) && !dir.exists(path)) {
      stop(
        sprintf("LinkEDA could not create its R package library at `%s`.", path),
        call. = FALSE
      )
    }
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}
.rls_activate_optional_library <- function(create = FALSE) {
  path <- .rls_optional_library(create = create)
  if (dir.exists(path)) .libPaths(unique(c(path, .libPaths())))
  invisible(path)
}

.rls_namespace_available <- function(package) {
  requireNamespace(package, quietly = TRUE)
}

.rls_missing_package_condition <- function(packages, context) {
  packages <- unique(as.character(packages))
  packages <- packages[nzchar(packages)]
  context <- as.character(context %||% "This analysis")[[1L]]
  package_text <- if (length(packages) == 1L) {
    sprintf("the R package `%s`", packages)
  } else {
    sprintf("the R packages %s", paste(sprintf("`%s`", packages), collapse = ", "))
  }
  structure(
    list(
      message = sprintf(
        "%s requires %s. LinkEDA can install %s in its private user library.",
        context, package_text, if (length(packages) == 1L) "it" else "them"
      ),
      call = NULL,
      packages = packages,
      context = context
    ),
    class = c("linkeda_missing_packages", "error", "condition")
  )
}

.rls_require_optional_packages <- function(packages, context = "This analysis") {
  packages <- unique(as.character(packages))
  packages <- packages[nzchar(packages)]
  .rls_activate_optional_library()
  missing <- packages[!vapply(packages, .rls_namespace_available, logical(1L))]
  if (length(missing)) stop(.rls_missing_package_condition(missing, context))
  invisible(TRUE)
}

.rls_install_packages_impl <- function(packages, lib, repos) {
  utils::install.packages(
    packages, lib = lib, repos = repos, dependencies = NA,
    type = .rls_optional_package_type()
  )
}

.rls_optional_package_type <- function() {
  # A LinkEDA user on Windows must not need Rtools merely because CRAN has a
  # newer source release than its compatible Windows binary.  Binary packages
  # also keep compiled dependency versions (notably TMB/glmmTMB) aligned.
  if (.Platform$OS.type == "windows") "binary" else getOption("pkgType")
}

.rls_install_optional_packages <- function(packages) {
  packages <- unique(as.character(packages))
  packages <- packages[nzchar(packages)]
  invalid <- setdiff(packages, .rls_optional_packages)
  if (length(invalid)) {
    stop(
      sprintf(
        "LinkEDA refused an unknown optional package request: %s.",
        paste(invalid, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  library <- .rls_activate_optional_library(create = TRUE)
  if (file.access(library, 2L) != 0L) {
    stop(
      sprintf("LinkEDA's private R package library is not writable: `%s`.", library),
      call. = FALSE
    )
  }
  repos <- getOption("repos")
  if (!length(repos) || all(is.na(repos)) || identical(unname(repos[[1L]]), "@CRAN@")) {
    repos <- c(CRAN = "https://cloud.r-project.org")
  }
  missing <- packages[!vapply(packages, .rls_namespace_available, logical(1L))]
  if (length(missing)) {
    .rls_install_packages_impl(missing, lib = library, repos = repos)
    .rls_activate_optional_library()
  }
  remaining <- packages[!vapply(packages, .rls_namespace_available, logical(1L))]
  if (length(remaining)) {
    stop(
      sprintf(
        "R did not install the required package%s: %s.",
        if (length(remaining) == 1L) "" else "s",
        paste(remaining, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(library)
}

.rls_offer_missing_packages_native <- function(error, retry_kind, analysis_id, group) {
  if (!inherits(error, "linkeda_missing_packages") ||
      !isTRUE(.rls_state$process_started)) return(invisible(FALSE))
  packages <- unique(as.character(error$packages %||% character()))
  packages <- packages[nzchar(packages)]
  if (!length(packages)) return(invisible(FALSE))
  tryCatch({
    payload <- c(
      "R_PACKAGES_REQUIRED",
      .rls_native_wire_value(retry_kind),
      .rls_native_wire_value(analysis_id),
      .rls_native_wire_value(group),
      .rls_native_wire_value(error$context %||% "This analysis"),
      as.character(length(packages)),
      .rls_native_wire_value(packages)
    )
    reply <- tryCatch(.rls_send(payload, timeout = 300), error = identity)
    if (inherits(reply, "error") && identical(.rls_state$backend_kind, "winui")) {
      .rls_close_winui_task_poll_connection()
      reply <- tryCatch(.rls_send(payload, timeout = 300), error = identity)
    }
    if (inherits(reply, "error")) stop(reply)
    identical(reply, "OK install")
  }, error = function(e) {
    message("LinkEDA could not present the package installation request: ",
            conditionMessage(e))
    FALSE
  })
}
