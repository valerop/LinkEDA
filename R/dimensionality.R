.rls_dimension_id <- function(group, name = NULL) {
  .rls_validate_protocol_name(name %||% paste0("dim_", group, "_", format(Sys.time(), "%Y%m%d%H%M%OS3")), "name")
}

.rls_dimension_record <- function(model) {
  id <- if (inherits(model, "rlispstat_dimensionality")) model$id else model
  id <- .rls_validate_protocol_name(id, "model")
  if (!exists(id, envir = .rls_state$dimensionality_models, inherits = FALSE)) {
    stop("Unknown principal components/factor analysis model.", call. = FALSE)
  }
  get(id, envir = .rls_state$dimensionality_models)
}

.rls_assign_dimension <- function(record) {
  assign(record$id, record, envir = .rls_state$dimensionality_models)
  structure(list(id = record$id, group = record$group), class = "rlispstat_dimensionality")
}

.rls_dimension_validate_variables <- function(data, variables, metadata = NULL, min_variables = 0L) {
  variables <- unique(vapply(variables %||% character(), .rls_validate_protocol_name,
                             character(1L), what = "variable"))
  # Share item eligibility and ordered/binary encoding with Scale Analysis.
  .rls_scale_item_specifications(data, variables, metadata = metadata)
  if (length(variables) < min_variables) {
    stop(sprintf("At least %d numeric, ordinal, or binary variables are required.", min_variables), call. = FALSE)
  }
  variables
}

.rls_dimension_complete_matrix <- function(data, variables, missing = c("listwise", "pairwise"),
                                           scope = c("all", "selected", "unselected"),
                                           selected_rows = integer()) {
  missing <- match.arg(missing)
  scope <- match.arg(scope)
  x <- data[, variables, drop = FALSE]
  rows <- stats::complete.cases(x)
  if (!identical(scope, "all")) {
    selected_rows <- unique(as.integer(selected_rows %||% integer()))
    selected_rows <- selected_rows[!is.na(selected_rows) & selected_rows >= 1L & selected_rows <= nrow(data)]
    selected <- rep(FALSE, nrow(data))
    selected[selected_rows] <- TRUE
    if (identical(scope, "selected")) {
      rows <- rows & selected
    } else {
      rows <- rows & !selected
    }
  }
  list(
    x = as.matrix(x[rows, , drop = FALSE]),
    rows_used = which(rows),
    rows_excluded = which(!rows)
  )
}

.rls_dimension_parallel <- function(data, scale = TRUE, missing = "listwise",
                                    iterations = 100L, seed = 271828L) {
  if (!requireNamespace("psych", quietly = TRUE))
    stop("Parallel analysis requires the R package `psych`.", call. = FALSE)
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  old_options <- options(mc.cores = 1L)
  on.exit({
    options(old_options)
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
  capture.output(reference <- psych::fa.parallel(
    data, fa = "pc", n.iter = max(1L, as.integer(iterations)),
    cor = if (scale) "cor" else "cov", sim = FALSE, SMC = TRUE,
    use = if (missing == "pairwise") "pairwise" else "complete",
    quant = .95, plot = FALSE))
  # pc.simr is the mean; the displayed decision reference is the 95th percentile.
  as.numeric(apply(reference$values[, seq_len(ncol(data)), drop = FALSE],
                   2L, stats::quantile, probs = .95, names = FALSE))
}

.rls_dimension_matrix_parallel <- function(r, n, method, extraction, iterations,
                               data = NULL, covariance = FALSE, missing = "pairwise") {
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  old_options <- options(mc.cores = 1L)
  on.exit({
    options(old_options)
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(271828L)
  raw_covariance <- covariance && method == "pca"
  if (raw_covariance) {
    if (is.null(data)) stop("Covariance PCA parallel analysis requires the source observations.")
    data <- as.data.frame(data)
    data <- data[if (missing == "listwise") stats::complete.cases(data) else
      rowSums(!is.na(data)) > 0L, , drop = FALSE]
    utils::capture.output(result <- psych::fa.parallel(data, fa = "pc", cor = "cov",
      sim = FALSE, SMC = TRUE, use = if (missing == "listwise") "complete" else "pairwise",
      n.iter = iterations, quant = .95, plot = FALSE))
  } else {
    # Factor parallel analysis uses reduced correlations, including for covariance fits.
    utils::capture.output(result <- psych::fa.parallel(stats::cov2cor(r), n.obs = n,
      fm = extraction, fa = if (method == "pca") "pc" else "fa",
      SMC = method == "pca", n.iter = iterations, quant = .95, plot = FALSE))
  }
  columns <- seq_len(ncol(r)) + if (method == "pca") 0L else ncol(r)
  result$reference <- as.numeric(apply(result$values[, columns, drop = FALSE],
    2L, stats::quantile, probs = .95, names = FALSE))
  result$reference_method <- if (raw_covariance) "column resampling (covariance units)" else
    if (method == "factor") "normal simulations (reduced correlations)" else "normal simulations (correlations)"
  result
}

.rls_dimension_orthomax <- function(loadings, scores, rotation = c("none", "varimax", "quartimax")) {
  rotation <- match.arg(rotation)
  loadings <- as.matrix(loadings); scores <- as.matrix(scores)
  if (rotation == "none" || ncol(loadings) < 2L)
    return(list(loadings = loadings, scores = scores))
  if (rotation == "varimax") {
    rotated <- stats::varimax(loadings, normalize = FALSE)
    transform <- rotated$rotmat
  } else {
    if (!requireNamespace("GPArotation", quietly = TRUE))
      stop("Quartimax rotation requires the R package `GPArotation`.", call. = FALSE)
    rotated <- GPArotation::quartimax(loadings)
    transform <- rotated$Th
  }
  list(loadings = as.matrix(rotated$loadings), scores = scores %*% transform)
}

.rls_dimension_pca <- function(x, variables, n_components = NULL, scale = TRUE,
                               rotation = c("none", "varimax", "quartimax"), missing = "listwise") {
  rotation <- match.arg(rotation)
  complete <- stats::complete.cases(x)
  if (missing == "pairwise") {
    covariance <- stats::cov(x, use = "pairwise.complete.obs")
    if (any(!is.finite(covariance)) || any(diag(covariance) <= 0))
      stop("PCA requires finite pairwise covariances and nonzero variable variances.", call. = FALSE)
    matrix <- if (scale) stats::cor(x, use = "pairwise.complete.obs") else covariance
    fit <- tryCatch(stats::princomp(covmat = list(cov = matrix,
      n.obs = nrow(x), center = if (scale) rep(0, ncol(x)) else colMeans(x, na.rm = TRUE))),
      error = function(error) stop(paste("Pairwise PCA could not be fitted:", conditionMessage(error)), call. = FALSE))
    vectors <- unclass(fit$loadings)
    scoring_data <- if (scale) base::scale(x[complete, , drop = FALSE],
      center = colMeans(x, na.rm = TRUE), scale = apply(x, 2L, stats::sd, na.rm = TRUE)) else x[complete, , drop = FALSE]
    all_scores <- if (any(complete)) stats::predict(fit, newdata = scoring_data)
      else matrix(numeric(), 0L, ncol(x))
  } else {
    covariance <- stats::cov(x)
    fit <- stats::prcomp(x, center = TRUE, scale. = isTRUE(scale))
    vectors <- fit$rotation
    all_scores <- fit$x
  }
  eigenvalues <- fit$sdev^2
  rank <- sum(fit$sdev > max(fit$sdev) * sqrt(.Machine$double.eps))
  requested <- as.integer(n_components %||% min(2L, length(variables)))
  if (!length(requested) || is.na(requested) || requested < 1L)
    stop("Choose a positive number of components.", call. = FALSE)
  n_components <- min(requested, rank)
  if (n_components < 1L) stop("No nonzero principal components are available.", call. = FALSE)
  retained <- seq_len(n_components)
  loadings <- sweep(vectors[, retained, drop = FALSE], 2L, fit$sdev[retained], `*`)
  scores <- all_scores[, retained, drop = FALSE]
  rotated <- .rls_dimension_orthomax(loadings, scores, rotation)
  loadings <- unclass(rotated$loadings); scores <- rotated$scores
  colnames(loadings) <- colnames(scores) <- paste0("PC", retained)
  baseline <- if (scale) rep(1, ncol(x)) else diag(covariance)
  list(method = "pca", eigenvalues = eigenvalues,
       variance = eigenvalues / sum(eigenvalues), cumulative = cumsum(eigenvalues / sum(eigenvalues)),
       loadings = loadings, scores = scores,
       communalities = rowSums(loadings^2), uniquenesses = pmax(0, baseline - rowSums(loadings^2)),
       score_rows = which(complete), retained = n_components, requested = requested)
}

.rls_dimension_max_factor_count <- function(p) {
  p <- as.integer(p)
  if (is.na(p) || p < 3L) return(1L)
  max_factors <- 1L
  for (m in seq_len(p - 1L)) {
    df <- ((p - m)^2 - p - m) / 2
    if (is.finite(df) && df >= 0) {
      max_factors <- m
    }
  }
  max_factors
}

# Check the matrix in R before psych estimates or inverts it. In particular,
# polychoric smoothing can produce positive but numerically singular matrices.
# This is a numerical precision check, not a cut-off on the resulting scores.
.rls_dimension_matrix_problem <- function(correlation, require_invertible = TRUE) {
  if (any(!is.finite(correlation))) return("Correlation matrix contains missing or non-finite values.")
  if (any(diag(correlation) <= 0)) return("At least one item has zero or invalid variance.")
  if (!isTRUE(isSymmetric(unname(correlation)))) return("Correlation matrix is not symmetric.")
  r <- stats::cov2cor(correlation)
  values <- eigen(r, symmetric = TRUE, only.values = TRUE)$values
  tolerance <- max(abs(values)) * sqrt(.Machine$double.eps)
  if (min(values) < -tolerance)
    return(paste(if (require_invertible) "Correlation matrix is not positive definite (negative eigenvalue)." else
      "Correlation matrix is not positive semidefinite.",
      "Review item correlations and missing-data handling; no matrix correction was applied."))
  if (require_invertible && min(values) <= 0)
    return("Correlation matrix is not positive definite. Review redundant items, sparse categories and missing-data handling.")
  if (require_invertible && rcond(r) <= sqrt(.Machine$double.eps))
    return("Correlation matrix is numerically singular. Review redundant items and sparse categories before estimating factor scores.")
  ""
}

# Shared factor-analysis engine for analyses that already have an appropriate
# correlation matrix.  Scale Analysis uses this route for Pearson, polychoric,
# or mixed item correlations; the ordinary dimensionality workflow can keep its
# raw-data/scores route while sharing the statistical EFA implementation.
.rls_dimension_factor_from_correlation <- function(
    correlation, n_obs, n_components = NULL,
    extraction = c("minres", "ml", "pa"),
    rotation = c("oblimin", "varimax", "quartimax", "promax", "none"),
    parallel_iterations = 20L, score_data = NULL, scale = TRUE, missing = "pairwise",
    run_parallel = TRUE) {
  extraction <- match.arg(tolower(extraction), c("minres", "ml", "pa"))
  rotation <- match.arg(tolower(rotation), c("oblimin", "varimax", "quartimax", "promax", "none"))
  correlation <- as.matrix(correlation)
  n_obs <- as.integer(n_obs)
  parallel_iterations <- max(1L, as.integer(parallel_iterations %||% 20L))
  if (ncol(correlation) < 3L || nrow(correlation) != ncol(correlation) ||
      is.na(n_obs) || n_obs < 4L) {
    return(list(status = "unavailable_insufficient_data", loadings = data.frame(),
                parallel = list(), package = "psych",
                function_name = "psych::fa / psych::fa.parallel"))
  }
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("Factor analysis requires the R package `psych`.", call. = FALSE)
  }
  matrix_problem <- .rls_dimension_matrix_problem(correlation)
  if (nzchar(matrix_problem)) return(list(
    status = matrix_problem, score_status = matrix_problem,
    loadings = data.frame(), scores = data.frame(), parallel = list(),
    package = "psych", function_name = "psych::fa / psych::fa.parallel"))
  parallel <- if (isTRUE(run_parallel)) tryCatch(.rls_dimension_matrix_parallel(
    correlation, n_obs, "factor", extraction, parallel_iterations,
    data = score_data, covariance = !isTRUE(scale), missing = missing), error = identity) else list(
      # Same observed reduced-correlation eigenvalues as fa.parallel, without simulation.
      fa.values = psych::fa(stats::cov2cor(correlation), nfactors = 1L,
        rotate = "none", fm = extraction, warnings = FALSE)$values)
  suggested <- if (inherits(parallel, "error")) NA_integer_
    else as.integer(parallel$nfact %||% NA_integer_)
  if (is.null(n_components) && (!length(suggested) || is.na(suggested) || suggested == 0L)) {
    status <- if (length(suggested) && !is.na(suggested) && suggested == 0L)
      "Parallel analysis recommends 0 factors. No dimensions were extracted; choose a count explicitly to explore a solution."
      else "The automatic dimension count is unavailable. Review the parallel analysis or choose a count explicitly."
    return(list(status = status, score_status = status, factors = if (is.na(suggested)) NA_integer_ else 0L,
      suggested_factors = suggested, loadings = data.frame(), scores = data.frame(),
      parallel = parallel, package = "psych", function_name = "psych::fa / psych::fa.parallel"))
  }
  factors <- as.integer(n_components %||% suggested)
  if (!length(factors) || is.na(factors)) factors <- 1L
  factors <- max(1L, min(factors, ncol(correlation) - 1L))
  fit <- tryCatch(psych::fa(
    correlation, nfactors = factors, n.obs = n_obs,
    fm = extraction, rotate = rotation, covar = !isTRUE(scale)
  ), error = identity)
  if (inherits(fit, "error")) {
    return(list(status = paste0("unavailable: ", conditionMessage(fit)),
                loadings = data.frame(), suggested_factors = suggested,
                parallel = parallel, package = "psych",
                function_name = "psych::fa / psych::fa.parallel"))
  }
  loadings <- as.data.frame(unclass(fit$loadings), stringsAsFactors = FALSE)
  loadings$item <- rownames(loadings)
  loadings$h2 <- as.numeric(fit$communality %||% rep(NA_real_, nrow(loadings)))
  loadings$u2 <- as.numeric(fit$uniquenesses %||% rep(NA_real_, nrow(loadings)))
  rownames(loadings) <- NULL
  loading_names <- setdiff(names(loadings), c("item", "h2", "u2"))
  loadings <- loadings[, c("item", loading_names, "h2", "u2"), drop = FALSE]
  scores <- data.frame()
  score_status <- if (any(!is.finite(fit$uniquenesses)) || any(fit$uniquenesses < 0))
    "Improper factor solution (negative or invalid uniqueness). Factor scores and biplot are unavailable; review the number of factors and item correlations."
    else if (identical(fit$converged, FALSE))
      "Factor analysis did not converge. Factor scores and biplot are unavailable."
    else ""
  if (!is.null(score_data) && !nzchar(score_status)) {
    score_data <- as.data.frame(score_data, stringsAsFactors = FALSE)
    if (ncol(score_data) == nrow(correlation)) {
      names(score_data) <- colnames(correlation)
      complete <- stats::complete.cases(score_data)
      if (any(complete)) {
        scored <- tryCatch(
          psych::factor.scores(
            score_data[complete, , drop = FALSE], f = fit,
            method = "Thurstone", rho = correlation, missing = FALSE
          )$scores,
          error = function(error) {
            score_status <<- paste("Factor scores unavailable:", conditionMessage(error))
            NULL
          }
        )
        if (!is.null(scored) && any(!is.finite(scored))) {
          score_status <- "psych::factor.scores returned non-finite scores. Factor scores and biplot are unavailable."
          scored <- NULL
        }
        if (!is.null(scored)) {
          scored <- as.data.frame(scored, stringsAsFactors = FALSE)
          score_count <- min(length(loading_names), ncol(scored))
          scored <- scored[, seq_len(score_count), drop = FALSE]
          names(scored) <- loading_names[seq_len(score_count)]
          scores <- data.frame(row = which(complete), scored, check.names = FALSE,
                               stringsAsFactors = FALSE)
        }
      }
    }
  }
  if (!nrow(scores) && !nzchar(score_status))
    score_status <- "No complete-row factor scores are available from psych::factor.scores."
  list(status = if (nzchar(score_status)) score_status else "value",
       eigenvalues = fit$values,
       component_variance = as.numeric(fit$Vaccounted["Proportion Var", ]),
       component_cumulative = as.numeric(cumsum(fit$Vaccounted["Proportion Var", ])),
       score_status = score_status, factors = factors, suggested_factors = suggested,
       loadings = loadings, factor_correlations = fit$Phi,
       scores = scores, parallel = parallel, package = "psych",
       function_name = "psych::fa / psych::fa.parallel")
}

# Principal-components counterpart to the shared correlation-matrix EFA
# engine.  Scale Analysis may use Pearson, polychoric, or mixed correlations,
# so PCA must consume that same matrix instead of silently reverting to a
# different raw-data correlation calculation.
.rls_dimension_pca_from_correlation <- function(
    correlation, n_obs, n_components = NULL, extraction = "minres",
    rotation = c("none", "varimax", "quartimax", "oblimin", "promax"),
    parallel_iterations = 20L, score_data = NULL, scale = TRUE, missing = "pairwise",
    run_parallel = TRUE) {
  invisible(extraction)
  correlation <- as.matrix(correlation)
  n_obs <- as.integer(n_obs)
  rotation <- match.arg(tolower(rotation),
                        c("none", "varimax", "quartimax", "oblimin", "promax"))
  parallel_iterations <- max(1L, as.integer(parallel_iterations %||% 20L))
  if (ncol(correlation) < 2L || nrow(correlation) != ncol(correlation) ||
      is.na(n_obs) || n_obs < 3L) {
    return(list(status = "unavailable_insufficient_data", loadings = data.frame(),
                scores = data.frame(), parallel = list(), package = "psych",
                function_name = "psych::principal / psych::fa.parallel"))
  }
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("Principal components requires the R package `psych`.", call. = FALSE)
  }
  matrix_problem <- .rls_dimension_matrix_problem(correlation, require_invertible = FALSE)
  if (nzchar(matrix_problem)) return(list(
    status = matrix_problem, score_status = matrix_problem,
    loadings = data.frame(), scores = data.frame(), parallel = list(),
    package = "psych", function_name = "psych::principal / psych::fa.parallel"))
  parallel <- if (isTRUE(run_parallel)) tryCatch({
    par <- .rls_dimension_matrix_parallel(
      correlation, n_obs, "pca", extraction, parallel_iterations,
      data = score_data, covariance = !isTRUE(scale), missing = missing)
    # Preserve the shared observed-eigenvalue transport used by Scale Analysis.
    par$fa.values <- par$pc.values
    par
  }, error = identity) else {
    observed <- eigen(correlation, symmetric = TRUE, only.values = TRUE)$values
    list(pc.values = observed, fa.values = observed)
  }
  suggested <- if (inherits(parallel, "error")) NA_integer_
    else as.integer(parallel$ncomp %||% NA_integer_)
  if (is.null(n_components) && (!length(suggested) || is.na(suggested) || suggested == 0L)) {
    status <- if (length(suggested) && !is.na(suggested) && suggested == 0L)
      "Parallel analysis recommends 0 components. No dimensions were extracted; choose a count explicitly to explore a solution."
      else "The automatic dimension count is unavailable. Review the parallel analysis or choose a count explicitly."
    return(list(status = status, score_status = status, factors = if (is.na(suggested)) NA_integer_ else 0L,
      suggested_factors = suggested, loadings = data.frame(), scores = data.frame(),
      parallel = parallel, package = "psych", function_name = "psych::principal / psych::fa.parallel"))
  }
  components <- as.integer(n_components %||% suggested %||% 1L)
  components <- max(1L, min(components, ncol(correlation)))
  fit <- tryCatch(psych::principal(
    correlation, nfactors = components, n.obs = n_obs,
    rotate = rotation, scores = FALSE, covar = !isTRUE(scale)
  ), error = identity)
  if (inherits(fit, "error")) {
    return(list(status = paste0("unavailable: ", conditionMessage(fit)),
                loadings = data.frame(), scores = data.frame(),
                suggested_factors = suggested, parallel = parallel,
                package = "psych",
                function_name = "psych::principal / psych::fa.parallel"))
  }
  loadings_matrix <- unclass(fit$loadings)
  loading_names <- paste0("PC", seq_len(ncol(loadings_matrix)))
  colnames(loadings_matrix) <- loading_names
  loadings <- as.data.frame(loadings_matrix, stringsAsFactors = FALSE)
  loadings$item <- rownames(loadings)
  # psych accounts for oblique factor correlations and covariance units.
  # Squared pattern loadings alone are not communalities after oblique rotation.
  loadings$h2 <- as.numeric(fit$communality)
  loadings$u2 <- as.numeric(fit$uniquenesses)
  rownames(loadings) <- NULL
  loadings <- loadings[, c("item", loading_names, "h2", "u2"), drop = FALSE]
  scores <- data.frame()
  score_status <- .rls_dimension_matrix_problem(correlation)
  if (!is.null(score_data) && !nzchar(score_status)) {
    score_data <- as.data.frame(score_data, stringsAsFactors = FALSE)
    if (ncol(score_data) == nrow(correlation)) {
      names(score_data) <- colnames(correlation)
      complete <- stats::complete.cases(score_data)
      if (any(complete)) {
        scored <- tryCatch({
          values <- as.matrix(score_data[complete, , drop = FALSE])
          values <- scale(values, center = TRUE, scale = isTRUE(scale))
          weights <- as.matrix(fit$weights %||%
            solve(correlation, loadings_matrix))
          values %*% weights
        }, error = function(error) {
          score_status <<- paste("Component scores unavailable:", conditionMessage(error))
          NULL
        })
        if (!is.null(scored) && any(!is.finite(scored))) {
          score_status <- paste("Complete-row component scores are not finite.",
            "Scores and biplot are unavailable; review complete cases and constant items.")
          scored <- NULL
        }
        if (!is.null(scored)) {
          scored <- as.data.frame(scored, stringsAsFactors = FALSE)
          scored <- scored[, seq_len(min(components, ncol(scored))), drop = FALSE]
          names(scored) <- loading_names[seq_len(ncol(scored))]
          scores <- data.frame(row = which(complete), scored, check.names = FALSE,
                               stringsAsFactors = FALSE)
        }
      }
    }
  }
  if (!nrow(scores) && !nzchar(score_status))
    score_status <- "No complete-row component scores are available."
  list(status = if (nzchar(score_status)) score_status else "value",
       score_status = score_status, factors = components, suggested_factors = suggested,
       component_variance = as.numeric(proportions(fit$values)),
       component_cumulative = as.numeric(cumsum(proportions(fit$values))),
       loadings = loadings, scores = scores, parallel = parallel,
       package = "psych", function_name = "psych::principal / psych::fa.parallel")
}

.rls_dimension_factor <- function(x, variables, n_components = NULL, scale = TRUE,
                                  rotation = c("none", "varimax", "quartimax")) {
  rotation <- match.arg(rotation)
  p <- length(variables)
  max_factors <- .rls_dimension_max_factor_count(p)
  n_components <- min(max_factors, max(1L, as.integer(n_components %||% min(2L, max_factors))))
  if (n_components < 1L) {
    stop("At least one factor is required.", call. = FALSE)
  }
  x_fit <- if (isTRUE(scale)) scale(x) else x
  fit <- try(stats::factanal(x_fit, factors = n_components, scores = "regression", rotation = "none"), silent = TRUE)
  if (inherits(fit, "try-error")) {
    stop(sprintf("Could not fit factor analysis: %s", conditionMessage(attr(fit, "condition"))), call. = FALSE)
  }
  loadings <- unclass(fit$loadings)[, seq_len(n_components), drop = FALSE]
  colnames(loadings) <- paste0("F", seq_len(n_components))
  scores <- fit$scores
  colnames(scores) <- paste0("F", seq_len(n_components))
  rotated <- .rls_dimension_orthomax(loadings, scores, rotation)
  loadings <- rotated$loadings
  scores <- rotated$scores
  colnames(loadings) <- colnames(scores) <- paste0("F", seq_len(n_components))
  ss <- colSums(loadings^2)
  variance <- ss / p
  list(
    method = "factor",
    eigenvalues = eigen(stats::cor(x_fit), symmetric = TRUE, only.values = TRUE)$values,
    variance = variance,
    cumulative = cumsum(variance),
    loadings = loadings,
    scores = scores,
    communalities = rowSums(loadings^2),
    uniquenesses = as.numeric(fit$uniquenesses)
  )
}

.rls_dimension_compute_typed <- function(data, variables, method, n_components, scale,
                                         missing, rotation, scope, selected_rows,
                                         parallel, parallel_iterations, item_specs = NULL,
                                         extraction = "minres") {
  scope_rows <- seq_len(nrow(data))
  if (scope == "selected") scope_rows <- intersect(scope_rows, selected_rows)
  if (scope == "unselected") scope_rows <- setdiff(scope_rows, selected_rows)
  scoped <- data[scope_rows, , drop = FALSE]
  specs <- item_specs %||% .rls_scale_item_specifications(data, variables)
  prepared <- .rls_scale_prepare_items(scoped, specs)
  correlation <- .rls_scale_correlations_one(prepared, specs, missing = missing)
  result <- .rls_scale_dimensionality_one(prepared, specs, correlation,
    list(enabled = TRUE, method = method, factors = n_components, scale = scale,
         missing = missing, rotation = rotation, extraction = extraction,
         parallel_iterations = parallel_iterations, parallel = parallel))
  if (!nrow(result$loadings)) stop(result$status, call. = FALSE)
  loading_names <- setdiff(names(result$loadings), c("item", "h2", "u2"))
  dimension_names <- paste0(if (method == "factor") "F" else "PC", seq_along(loading_names))
  loadings <- result$loadings[, c("item", loading_names, "h2", "u2")]
  names(loadings) <- c("variable", dimension_names, "communality", "uniqueness")
  scores <- result$scores[, setdiff(names(result$scores), "row"), drop = FALSE]
  if (ncol(scores)) names(scores) <- dimension_names
  score_rows <- scope_rows[result$scores$row %||% integer()]
  numeric_data <- .rls_scale_numeric_matrix(prepared, specs)
  matrix_rows <- if (missing == "listwise") stats::complete.cases(numeric_data) else rowSums(!is.na(numeric_data)) > 0L
  rows_used <- scope_rows[matrix_rows]
  eigenvalues <- if (method == "factor") result$parallel$fa.values %||% result$eigenvalues else result$parallel$pc.values
  reference <- result$parallel$reference
  n <- length(eigenvalues)
  list(method = method, rotation = rotation, correlation_method = correlation$method,
       calculation_method = paste(result$report_method,
         if (parallel) result$report_parallel
         else "Parallel reference hidden.", sep = "\n"),
       score_status = result$score_status %||% "",
       eigenvalues = data.frame(component = paste0(if (method == "factor") "F" else "PC", seq_len(n)),
         eigenvalue = eigenvalues,
         parallel_eigenvalue = if (parallel) reference[seq_len(n)] else rep(NA_real_, n),
         variance = result$component_variance[seq_len(n)],
         cumulative = result$component_cumulative[seq_len(n)]),
       loadings = loadings, scores = scores,
       score_rows_original_ids = score_rows,
       rows_used_original_ids = rows_used,
       rows_excluded_original_ids = setdiff(seq_len(nrow(data)), rows_used))
}

.rls_dimension_compute <- function(data, variables, method = c("pca", "factor"),
                                   n_components = NULL, scale = TRUE,
                                   missing = c("listwise", "pairwise"),
                                   rotation = c("none", "varimax", "quartimax", "oblimin", "promax"),
                                   extraction = c("minres", "ml", "pa"),
                                   scope = c("all", "selected", "unselected"),
                                   selected_rows = integer(),
                                   parallel = TRUE, parallel_iterations = 100L, item_specs = NULL) {
  method <- match.arg(method)
  missing <- match.arg(missing)
  rotation <- match.arg(rotation)
  extraction <- match.arg(tolower(extraction), c("minres", "ml", "pa"))
  scope <- match.arg(scope)
  specs <- item_specs %||% .rls_scale_item_specifications(data, variables)
  if (method == "factor" || rotation %in% c("oblimin", "promax") ||
      any(vapply(specs, `[[`, character(1L), "type") != "numeric")) {
    return(.rls_dimension_compute_typed(data, variables, method, n_components, scale,
      missing, rotation, scope, selected_rows, parallel, parallel_iterations, specs,
      extraction = extraction))
  }
  scope_rows <- seq_len(nrow(data))
  if (scope == "selected") scope_rows <- intersect(scope_rows, selected_rows)
  if (scope == "unselected") scope_rows <- setdiff(scope_rows, selected_rows)
  x <- as.matrix(data[scope_rows, variables, drop = FALSE])
  used <- if (missing == "listwise") stats::complete.cases(x) else rowSums(!is.na(x)) >= 1L
  scope_rows <- scope_rows[used]; x <- x[used, , drop = FALSE]
  if (nrow(x) < 2L) stop("At least two usable rows are required.", call. = FALSE)
  result <- .rls_dimension_pca(x, variables, n_components, scale, rotation, missing)
  parallel_values <- if (parallel) .rls_dimension_parallel(x, scale, missing, parallel_iterations)
    else rep(NA_real_, length(result$eigenvalues))
  eigen_table <- data.frame(component = paste0("PC", seq_along(result$eigenvalues)),
    eigenvalue = result$eigenvalues, parallel_eigenvalue = parallel_values[seq_along(result$eigenvalues)],
    variance = result$variance, cumulative = result$cumulative)
  loadings <- data.frame(variable = variables, result$loadings,
    communality = unname(result$communalities), uniqueness = unname(result$uniquenesses), check.names = FALSE, row.names = NULL)
  note <- paste(c(
    paste0(if (missing == "pairwise") paste0("stats::princomp; available-pair ", if (scale) "correlations." else "covariances.") else "stats::prcomp; complete-row decomposition.",
      if (scale) " Standardized variables." else " Original units."),
    if (!scale) "h2 and u2 are explained and residual variances in squared original units.",
    if (rotation != "none") paste0("Rotation: ", if (rotation == "varimax") "stats::varimax" else "GPArotation::quartimax", "."),
    if (parallel) paste0("Parallel reference: psych::fa.parallel; 95th percentile of ", parallel_iterations, " column-resampled datasets."),
    if (result$retained < result$requested) paste0("Retained ", result$retained, " components; remaining components have zero numerical variance.")
  ), collapse = "\n")
  list(method = method, rotation = rotation, typed_engine = FALSE, calculation_method = note,
       eigenvalues = eigen_table, loadings = loadings, scores = as.data.frame(result$scores),
       score_rows_original_ids = scope_rows[result$score_rows],
       rows_used_original_ids = scope_rows, rows_excluded_original_ids = setdiff(seq_len(nrow(data)), scope_rows))
}

.rls_dimension_source_data <- function(record, dataset) {
  if (!.rls_mi_is_dataset(dataset)) return(dataset$data)
  completed <- .rls_mi_completed_datasets(dataset)
  active <- as.integer(record$active_imputation_version %||% dataset$active_imputation_version %||% 1L)
  if (length(active) != 1L || is.na(active) || active < 1L || active > length(completed))
    stop("The displayed imputation is unavailable. Choose a valid imputation.", call. = FALSE)
  completed[[active]]
}

.rls_dimension_refit <- function(record) {
  dataset <- .rls_dataset_record(record$group)
  record$data <- .rls_dimension_source_data(record, dataset)
  record <- .rls_apply_scope_to_model_request(record, dataset)
  record$item_specifications <- .rls_scale_item_specifications(
    .rls_scale_specification_data(dataset), record$variables)
  if (length(record$variables %||% character()) < 2L) {
    record$eigenvalues <- data.frame()
    record$loadings <- data.frame()
    record$scores <- data.frame()
    record$rows_used_original_ids <- integer()
    record$score_rows_original_ids <- integer()
    record$rows_excluded_original_ids <- integer()
    record$status <- "Choose at least two numeric, ordinal, or binary variables."
    record$model_version <- record$model_version + 1L
    return(record)
  }
  computed <- .rls_dimension_compute(
    record$data, record$variables, method = record$method,
    n_components = record$n_components, scale = record$scale,
    missing = record$missing_mode, rotation = record$rotation %||% "none",
    extraction = record$extraction %||% "minres",
    scope = record$scope %||% "all",
    selected_rows = record$selected_rows %||% integer(),
    parallel = record$parallel %||% TRUE,
    parallel_iterations = record$parallel_iterations %||% 100L,
    item_specs = record$item_specifications
  )
  record$score_input_data <- record$data[, record$variables, drop = FALSE]
  record$correlation_method <- computed$correlation_method %||% "pearson"
  record$calculation_method <- computed$calculation_method %||% ""
  record$typed_engine <- computed$typed_engine %||% nzchar(record$calculation_method)
  record$eigenvalues <- computed$eigenvalues
  record$loadings <- computed$loadings
  record$scores <- computed$scores
  record$rotation <- computed$rotation
  record$n_components <- max(1L, min(record$n_components, ncol(record$loadings) - 3L))
  record$score_rows_original_ids <- computed$score_rows_original_ids %||% computed$rows_used_original_ids
  record$rows_used_original_ids <- computed$rows_used_original_ids
  record$rows_excluded_original_ids <- computed$rows_excluded_original_ids
  record$status <- sprintf(
    "%s: %d variables, %d complete rows%s",
    if (identical(record$method, "factor")) "Factor analysis" else "Principal components",
    length(record$variables), length(record$rows_used_original_ids),
    if (!identical(record$rotation %||% "none", "none")) paste0(", ", record$rotation, " rotation") else ""
  )
  if (isTRUE(record$typed_engine) || record$method == "pca") record$status <- sprintf(
    "%s: %d variables; %d cases for the matrix; %d complete-case scores.",
    if (record$method == "factor") "Factor analysis" else "Principal components",
    length(record$variables), length(record$rows_used_original_ids), nrow(record$scores))
  if (nzchar(computed$score_status %||% "")) record$status <- computed$score_status
  dataset <- .rls_dataset_record(record$group %||% record$dataset_id)
  multiple_imputation <- .rls_mi_is_dataset(dataset)
  record$analysis_backend <- if (multiple_imputation) "multiple_imputation" else "ordinary"
  record$imputation_count <- if (multiple_imputation)
    max(1L, as.integer(dataset$imputation_count %||% length(.rls_mi_completed_datasets(dataset)))) else 1L
  record$active_imputation_version <- if (multiple_imputation)
    max(1L, min(record$imputation_count,
                as.integer(record$active_imputation_version %||% dataset$active_imputation_version %||% 1L))) else 1L
  verification <- .rls_dimensionality_verification_r_code(
    record, multiple_imputation = multiple_imputation
  )
  executed_code <- paste0(
    "dimensionality_result <- LinkEDA:::.rls_dimension_compute(",
    "data = analysis_data, variables = ", .rls_r_character_vector(record$variables),
    ", method = ", .rls_r_string_literal(record$method),
    ", n_components = ", as.integer(record$n_components), "L",
    ", scale = ", if (isTRUE(record$scale)) "TRUE" else "FALSE",
    ", missing = ", .rls_r_string_literal(record$missing_mode),
    ", rotation = ", .rls_r_string_literal(record$rotation %||% "none"),
    ", extraction = ", .rls_r_string_literal(record$extraction %||% "minres"),
    ", scope = ", .rls_r_string_literal(record$scope %||% "all"),
    ", selected_rows = ", paste(capture.output(dput(as.integer(record$selected_rows %||% integer()))), collapse = ""),
    ", parallel = ", if (isTRUE(record$parallel)) "TRUE" else "FALSE",
    ", parallel_iterations = ", as.integer(record$parallel_iterations), "L)"
  )
  record <- .rls_attach_analysis_provenance(
    record, executed_code,
    title = if (identical(record$method, "factor")) "Factor Analysis" else "Principal Components",
    output_code = list(table = "component_table <- dimensionality_result$eigenvalues\nloading_table <- dimensionality_result$loadings"),
    verification_code = list(table = verification$code),
    verification_variables = verification$variables,
    verification_warnings = verification$warnings
  )
  record$model_version <- record$model_version + 1L
  record
}

.rls_dimension_sync_native_open <- function(record) {
  .rls_start_backend()
  dataset <- .rls_dataset_record(record$group)
  try(.rls_register_native_dataset_if_needed(dataset, visible = TRUE), silent = TRUE)
  try(.rls_send(c(
    "PCAFA_OPEN", record$id, record$group, record$method, record$missing_mode,
    if (isTRUE(record$scale)) "TRUE" else "FALSE",
    as.character(record$n_components),
    record$rotation %||% "none",
    record$scope %||% "all",
    as.character(length(record$variables)), record$variables,
    "EXTRACTION_V1", record$extraction %||% "minres"
  )), silent = TRUE)
}

.rls_dimension_native_update_payload <- function(record) {
  eigen <- record$eigenvalues %||% data.frame()
  loadings <- record$loadings %||% data.frame()
  scores <- record$scores %||% data.frame()
  component_count <- max(1L, as.integer(record$n_components %||% 1L))
  component_names <- if (is.data.frame(loadings)) {
    setdiff(names(loadings), c("variable", "communality", "uniqueness"))
  } else {
    character()
  }
  score_names <- if (is.data.frame(scores)) names(scores) else character()

  payload <- c(
    "PCAFA_UPDATE",
    .rls_native_wire_value(record$id),
    if (!is.null(record$native_request_revision)) c("REQUEST_V1",
      as.character(record$native_request_revision), as.character(record$native_request_version), "ok"),
    .rls_native_wire_value(record$group),
    .rls_native_wire_value(record$method),
    .rls_native_wire_value(record$missing_mode),
    .rls_native_wire_value(record$rotation %||% "none"),
    .rls_native_wire_value(record$scope %||% "all"),
    .rls_native_wire_bool(record$scale),
    .rls_native_wire_integer(component_count),
    .rls_native_wire_value(record$status %||% sprintf(
      "%s: %d variables, %d complete rows%s",
      if (identical(record$method, "factor")) "Factor analysis" else "Principal components",
      length(record$variables %||% character()),
      length(record$rows_used_original_ids %||% integer()),
      if (!identical(record$rotation %||% "none", "none")) paste0(", ", record$rotation, " rotation") else ""
    )),
    as.character(length(record$variables %||% character())),
    .rls_native_wire_value(record$variables %||% character()),
    .rls_native_int_vector_payload(record$rows_used_original_ids %||% integer()),
    .rls_native_int_vector_payload(record$rows_excluded_original_ids %||% integer())
  )

  component_rows <- if (is.data.frame(eigen)) nrow(eigen) else 0L
  payload <- c(payload, as.character(component_rows))
  if (component_rows) {
    for (i in seq_len(component_rows)) {
      payload <- c(
        payload,
        as.character(i),
        .rls_native_wire_number(eigen$eigenvalue[[i]]),
        .rls_native_wire_number(eigen$parallel_eigenvalue[[i]]),
        .rls_native_wire_number(eigen$variance[[i]]),
        .rls_native_wire_number(eigen$cumulative[[i]])
      )
    }
  }

  loading_rows <- if (is.data.frame(loadings)) nrow(loadings) else 0L
  payload <- c(payload, as.character(loading_rows))
  if (loading_rows) {
    for (i in seq_len(loading_rows)) {
      values <- unname(vapply(component_names, function(name) {
        .rls_native_wire_number(loadings[[name]][[i]])
      }, character(1L)))
      payload <- c(
        payload,
        .rls_native_wire_value(loadings$variable[[i]]),
        .rls_native_wire_number(loadings$communality[[i]]),
        .rls_native_wire_number(loadings$uniqueness[[i]]),
        as.character(length(values)),
        values
      )
    }
  }

  score_rows <- if (is.data.frame(scores)) nrow(scores) else 0L
  payload <- c(payload, as.character(score_rows))
  if (score_rows) {
    rows_used <- as.integer(record$score_rows_original_ids %||% record$rows_used_original_ids %||% seq_len(score_rows))
    for (i in seq_len(score_rows)) {
      values <- unname(vapply(score_names, function(name) {
        .rls_native_wire_number(scores[[name]][[i]])
      }, character(1L)))
      payload <- c(payload, as.character(rows_used[[i]]), as.character(length(values)), values)
    }
  }
  calculation_lines <- strsplit(record$calculation_method %||% "", "\n", fixed = TRUE)[[1L]]
  c(payload, "IMPUTATION_V1", as.character(record$active_imputation_version %||% 1L),
    as.character(record$imputation_count %||% 1L), "CALCULATION_V1",
    as.character(length(calculation_lines)), .rls_native_wire_value(calculation_lines), .rls_analysis_provenance_payload(record))
}

.rls_dimension_sync_native_update <- function(record) {
  if (!isTRUE(.rls_state$process_started)) return(FALSE)
  result <- try(.rls_send(.rls_dimension_native_update_payload(record)), silent = TRUE)
  is.character(result) && length(result) && startsWith(result[[1L]], "OK")
}

#' Create a principal components / factor analysis window
#'
#' @param data Registered dataset name, a data frame, or `NULL` for active dataset.
#' @param variables Numeric variables to include. If `NULL`, the analysis starts
#'   blank and the native window can add or remove any eligible numeric, ordinal, or binary variable.
#' @param method `"pca"` for principal components or `"factor"` for factor analysis.
#' @param n_components Number of components/factors retained in the loading table.
#' @param scale Logical. Use standardized variables.
#' @param missing Missing-data mode. Listwise uses complete rows; pairwise PCA uses available-pair correlations or covariances. Individual scores require complete rows.
#' @param rotation Rotation for the retained loadings: `"none"`, `"varimax"`,
#'   `"quartimax"`, `"oblimin"`, or `"promax"`.
#' @param extraction Factor-extraction method: minimum residual (`"minres"`),
#'   maximum likelihood (`"ml"`), or principal axis (`"pa"`).
#' @param scope Rows used by the analysis: `"all"`, `"selected"`, or
#'   `"unselected"`.
#' @param selected_rows Original one-based row ids used when `scope` is
#'   `"selected"` or `"unselected"`. The native window updates this from the
#'   linked selection state.
#' @param parallel Logical. Include a 95th percentile parallel-analysis
#'   reference in the scree table/plot.
#' @param parallel_iterations Number of simulated datasets for parallel analysis.
#' @param name Optional model id.
#' @param native Logical. Open the native window when possible.
#' @return A `rlispstat_dimensionality` handle.
#' @export
ls_new_dimensionality <- function(data = NULL, variables = NULL,
                                  method = c("pca", "factor"),
                                  n_components = NULL, scale = TRUE,
                                  missing = c("listwise", "pairwise"),
                                  rotation = c("none", "varimax", "quartimax", "oblimin", "promax"),
                                  extraction = c("minres", "ml", "pa"),
                                  scope = c("all", "selected", "unselected"),
                                  selected_rows = NULL,
                                  parallel = TRUE,
                                  parallel_iterations = 100L,
                                  name = NULL, native = TRUE) {
  method <- match.arg(method)
  missing <- match.arg(missing)
  rotation <- match.arg(rotation)
  extraction <- match.arg(tolower(extraction), c("minres", "ml", "pa"))
  scope <- match.arg(scope)
  if (is.data.frame(data)) {
    group <- .rls_register_dataset(name %||% "dimensionality", data, activate = TRUE)
    dataset <- .rls_dataset_record(group)
    id_name <- NULL
  } else {
    dataset <- .rls_dataset_record(data)
    id_name <- name
  }
  analysis_data <- if (.rls_mi_is_dataset(dataset)) {
    completed <- .rls_mi_completed_datasets(dataset)
    active <- max(1L, min(length(completed),
                          as.integer(dataset$active_imputation_version %||% 1L)))
    completed[[active]]
  } else dataset$data
  variables <- .rls_dimension_validate_variables(analysis_data, variables, dataset$variable_metadata)
  n_components <- max(1L, min(
    max(1L, length(variables)),
    as.integer(n_components %||% min(2L, max(1L, length(variables))))
  ))
  captured_scope <- .rls_capture_analysis_scope(dataset, scope, selected_rows)
  scope <- captured_scope$fit_scope
  selected_rows <- captured_scope$rows
  record <- list(
    id = .rls_dimension_id(dataset$group, id_name),
    group = dataset$group,
    dataset_id = dataset$group,
    data = analysis_data,
    variables = variables,
    method = method,
    n_components = n_components,
    scale = isTRUE(scale),
    missing_mode = missing,
    rotation = rotation,
    extraction = extraction,
    scope = scope,
    data_scope = captured_scope,
    scope_request_pending = TRUE,
    selected_rows = selected_rows,
    parallel = isTRUE(parallel),
    parallel_iterations = max(1L, as.integer(parallel_iterations %||% 100L)),
    eigenvalues = data.frame(),
    loadings = data.frame(),
    scores = data.frame(),
    rows_used_original_ids = integer(),
    rows_excluded_original_ids = integer(),
    model_version = 0L
  )
  record <- .rls_dimension_refit(record)
  handle <- .rls_assign_dimension(record)
  if (isTRUE(native)) {
    .rls_dimension_sync_native_open(record)
  }
  invisible(handle)
}

#' @rdname ls_new_dimensionality
#' @export
ls_dimensionality_state <- function(model) {
  .rls_dimension_record(model)
}

#' @rdname ls_new_dimensionality
#' @export
ls_dimensionality_eigenvalues <- function(model) {
  .rls_dimension_record(model)$eigenvalues
}

#' @rdname ls_new_dimensionality
#' @export
ls_dimensionality_loadings <- function(model) {
  .rls_dimension_record(model)$loadings
}

#' @rdname ls_new_dimensionality
#' @export
ls_dimensionality_scores <- function(model) {
  .rls_dimension_record(model)$scores
}

.rls_validate_dimension_score_source <- function(target, scope, input_columns) {
  fail <- function() stop("The score input data or row identities changed. Refit before saving scores.", call. = FALSE)
  if (is.null(scope$dataset_version) || !length(input_columns)) fail()
  frozen <- tryCatch(ls_get_data_version(target$group, scope$dataset_version), error = function(e) NULL)
  if (is.null(frozen) || !identical(as.character(attr(frozen, "linkeda_row_ids")),
                                    as.character(target$stable_row_ids))) fail()
  mi <- .rls_mi_is_dataset(target)
  if (mi != inherits(frozen, "linkeda_multiple_imputation_version")) fail()
  old_sets <- if (mi) c(list(frozen$data), frozen$completed_datasets) else list(frozen)
  sets <- if (mi) c(list(target$original_data), .rls_mi_completed_datasets(target)) else list(target$data)
  same_inputs <- function(a, b) all(input_columns %in% names(a)) &&
    all(input_columns %in% names(b)) && all(vapply(input_columns,
      function(v) identical(a[[v]], b[[v]]), logical(1L)))
  if (length(old_sets) != length(sets) || !all(mapply(same_inputs, old_sets, sets))) fail()
  invisible(TRUE)
}

.rls_save_dimension_score_sets <- function(target, score_sets, row_sets,
                                           method, components, source_key,
                                           prefix = NULL,
                                           provenance_code = "",
                                           input_columns = character(), source_scope = NULL) {
  .rls_validate_dimension_score_source(target, source_scope, input_columns)
  if (!length(score_sets) || length(score_sets) != length(row_sets)) {
    stop("No component/factor scores are available to save.", call. = FALSE)
  }
  available <- min(vapply(score_sets, ncol, integer(1L)))
  if (!is.finite(available) || available < 1L) {
    stop("No component/factor scores are available to save.", call. = FALSE)
  }
  components <- unique(as.integer(components %||% seq_len(available)))
  if (anyNA(components) || any(components < 1L | components > available)) {
    stop("`components` must identify available score columns.", call. = FALSE)
  }
  n <- nrow(target$data)
  full_scores <- lapply(seq_along(score_sets), function(i) {
    rows <- as.integer(row_sets[[i]])
    scores <- as.data.frame(score_sets[[i]], stringsAsFactors = FALSE)
    if (length(rows) != nrow(scores) || anyNA(rows) || any(rows < 1L | rows > n)) {
      stop("Factor-score rows do not match the source dataset.", call. = FALSE)
    }
    lapply(components, function(component) {
      values <- rep(NA_real_, n)
      values[rows] <- as.numeric(scores[[component]])
      values
    })
  })

  cache <- target$derived_score_columns %||% list()
  cached_names <- cache[[source_key]] %||% character()
  prefix <- prefix %||% if (identical(method, "factor")) "F" else "PC"
  new_names <- character(length(components))
  data <- target$data
  for (i in seq_along(components)) {
    component_key <- as.character(components[[i]])
    cached <- if (component_key %in% names(cached_names)) {
      unname(cached_names[[component_key]])
    } else ""
    base <- paste0(prefix, components[[i]], "_score")
    if (nzchar(cached) && cached %in% names(data)) {
      name <- cached
    } else {
      name <- make.unique(c(names(data), base), sep = "_")[[ncol(data) + 1L]]
    }
    new_names[[i]] <- name
    data[[name]] <- full_scores[[1L]][[i]]
  }

  if (.rls_mi_is_dataset(target)) {
    completed <- .rls_mi_completed_datasets(target)
    if (length(score_sets) != length(completed)) {
      stop("Scores are unavailable for one or more imputations.", call. = FALSE)
    }
    original <- target$original_data %||% target$data
    for (i in seq_along(components)) {
      name <- new_names[[i]]
      original[[name]] <- rep(NA_real_, n)
      for (imputation in seq_along(completed))
        completed[[imputation]][[name]] <- full_scores[[imputation]][[i]]
    }
    active <- max(1L, min(length(completed),
                          as.integer(target$active_imputation_version %||% 1L)))
    for (i in seq_along(new_names))
      data[[new_names[[i]]]] <- full_scores[[active]][[i]]
    mask <- target$missing_cell_mask %||% as.data.frame(
      lapply(original, is.na), check.names = FALSE
    )
    for (name in new_names) mask[[name]] <- rep(TRUE, n)
    target$original_data <- original
    target$completed_datasets <- completed
    target$missing_cell_mask <- mask
  }

  target <- .rls_sync_derived_mi_columns(target, new_names)
  target$data <- data
  target$data_frame <- data
  target$n_rows <- nrow(data)
  target$n_columns <- ncol(data)
  metadata <- target$variable_metadata %||% target$metadata %||%
    .rls_variable_metadata(data)
  for (name in new_names)
    metadata <- .rls_refresh_metadata_row(metadata, data, name, "numeric")
  target$metadata <- metadata
  target$variable_metadata <- metadata
  updated_names <- cached_names
  updated_names[as.character(components)] <- new_names
  cache[[source_key]] <- updated_names
  target$derived_score_columns <- cache
  target$modified <- TRUE
  affected_rows <- sort(unique(unlist(row_sets, use.names = FALSE)))
  stable_ids <- as.character(target$stable_row_ids %||%
    paste0(target$group, ":row:", seq_len(nrow(target$data))))
  affected_ids <- stable_ids[affected_rows[
    affected_rows >= 1L & affected_rows <= length(stable_ids)
  ]]
  target <- .rls_advance_data_version(
    target,
    sprintf("Save %s score columns: %s",
            if (identical(method, "factor")) "factor" else "component",
            paste(new_names, collapse = ", ")),
    code = provenance_code,
    origin = if (nzchar(provenance_code)) "recorded" else "unavailable",
    input_columns = input_columns,
    output_columns = new_names,
    stable_row_ids = affected_ids
  )
  .rls_set_dataset_record(target)
  .rls_sync_derived_imputation_record(target)
  .rls_notify_dataset_changed(target, variable = NULL)
  new_names
}

#' @rdname ls_new_dimensionality
#' @export
ls_dimensionality_save_scores <- function(model, prefix = NULL, components = NULL, dataset = NULL) {
  record <- .rls_dimension_record(model)
  target <- .rls_dataset_record(dataset %||% record$group)
  if (!identical(target$group, record$group))
    stop("Save scores to the dataset used by this dimensionality analysis.", call. = FALSE)
  .rls_validate_dimension_score_source(target, record$data_scope, record$variables)
  fitted_input <- record$score_input_data %||% record$data[, record$variables, drop = FALSE]
  current_input <- if (.rls_mi_is_dataset(target))
    .rls_mi_completed_datasets(target)[[record$active_imputation_version %||% 1L]] else target$data
  if (!all(vapply(record$variables, function(v) identical(fitted_input[[v]], current_input[[v]]), logical(1L))))
    stop("The score input data changed. Refit before saving scores.", call. = FALSE)
  prefix <- prefix %||% if (identical(record$method, "factor")) "F" else "PC"
  prefix <- .rls_validate_protocol_name(prefix, "prefix")
  if (.rls_mi_is_dataset(target)) {
    computed <- lapply(.rls_mi_completed_datasets(target), function(data) {
      .rls_dimension_compute(
        data, record$variables, method = record$method,
        n_components = record$n_components, scale = record$scale,
        missing = record$missing_mode, rotation = record$rotation %||% "none",
        extraction = record$extraction %||% "minres",
        scope = record$scope %||% "all",
        selected_rows = record$selected_rows %||% integer(), parallel = FALSE,
        item_specs = record$item_specifications
      )
    })
    score_sets <- lapply(computed, `[[`, "scores")
    row_sets <- lapply(computed, function(value) value$score_rows_original_ids %||% value$rows_used_original_ids)
  } else {
    score_sets <- list(record$scores)
    row_sets <- list(record$score_rows_original_ids %||% record$rows_used_original_ids)
  }
  if (is.character(components)) components <- match(components, names(score_sets[[1L]]))
  new_names <- .rls_save_dimension_score_sets(
    target, score_sets, row_sets, record$method, components,
    paste("dimensionality", record$id, record$method, prefix, sep = ":"), prefix,
    provenance_code = paste0(
      "LinkEDA::ls_dimensionality_save_scores(model = ",
      .rls_r_string_literal(record$id), ", components = ",
      if (is.null(components)) "NULL" else
        .rls_r_character_vector(as.character(components)), ", prefix = ",
      .rls_r_string_literal(prefix), ", dataset = ",
      .rls_r_string_literal(target$group), ")"
    ),
    input_columns = record$variables, source_scope = record$data_scope
  )
  record$data <- .rls_dataset_record(target$group)$data
  .rls_assign_dimension(record)
  invisible(new_names)
}

#' @rdname ls_new_dimensionality
#' @export
ls_dimensionality_add_variable <- function(model, variable) {
  record <- .rls_dimension_record(model)
  dataset <- .rls_dataset_record(record$group)
  variable <- .rls_dimension_validate_variables(.rls_dimension_source_data(record, dataset),
    variable, dataset$variable_metadata, min_variables = 1L)
  record$variables <- unique(c(record$variables, variable))
  record$n_components <- min(record$n_components, length(record$variables))
  record <- .rls_dimension_refit(record)
  handle <- .rls_assign_dimension(record)
  if (isTRUE(.rls_state$process_started)) {
    try(.rls_send(c("PCAFA_SET_VARIABLES", record$id, as.character(length(record$variables)), record$variables)), silent = TRUE)
  }
  invisible(handle)
}

#' @rdname ls_new_dimensionality
#' @export
ls_dimensionality_remove_variable <- function(model, variable) {
  record <- .rls_dimension_record(model)
  variable <- .rls_validate_protocol_name(variable, "variable")
  record$variables <- setdiff(record$variables, variable)
  record$n_components <- max(1L, min(record$n_components, max(1L, length(record$variables))))
  record <- .rls_dimension_refit(record)
  handle <- .rls_assign_dimension(record)
  if (isTRUE(.rls_state$process_started)) {
    try(.rls_send(c("PCAFA_SET_VARIABLES", record$id, as.character(length(record$variables)), record$variables)), silent = TRUE)
  }
  invisible(handle)
}
