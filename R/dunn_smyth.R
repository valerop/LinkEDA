# Use established R libraries for both residuals and local RNG management.
.rls_with_diagnostic_seed <- function(seed, code) {
  withr::with_seed(seed, force(code), .rng_kind="Mersenne-Twister",
                  .rng_normal_kind="Inversion", .rng_sample_kind="Rejection")
}

.rls_diagnostic_seed <- function(record, imputation_index=1L, fit=NULL) {
  seed <- record$diagnostic_seed %||% 104729L
  if (length(seed)!=1L || !is.finite(seed) || seed<0 || seed!=floor(seed) ||
      seed+imputation_index-1 > .Machine$integer.max)
    stop("diagnostic_seed must be a non-negative integer within the R seed range.",call.=FALSE)
  as.integer(seed+imputation_index-1L)
}

.rls_diagnostic_cache_key <- function(record, fit, rows, imputation_index) {
  frame <- tryCatch(stats::model.frame(fit),error=function(e)NULL)
  digest::digest(list(algorithm="statmod-quantile-v1", dataset=record$group,
    data_version=record$diagnostic_data_version, model=record$id,
    model_version=record$model_version, terms=record$terms, response=record$response,
    scope=record$scope, rows=rows, imputation=imputation_index,
    seed=.rls_diagnostic_seed(record,imputation_index),
    family=record$family, link=record$link, distribution=record$count_distribution,
    data=if(is.null(frame))NULL else as.list(frame),
    coefficients=tryCatch(stats::coef(fit),error=function(e)NULL), theta=fit$theta %||% NULL),
    algo="sha256",serializeVersion=2)
}

.rls_stable_diagnostic_uniform <- function(record, rows, imputation_index=1L, fit=NULL) {
  rows <- as.numeric(rows)
  if (anyNA(rows) || any(!is.finite(rows) | rows<1 | rows!=floor(rows)) || anyDuplicated(rows))
    stop("Diagnostic rows must have distinct positive integer IDs.",call.=FALSE)
  seed <- .rls_diagnostic_seed(record,imputation_index)
  values <- if(length(rows)) .rls_with_diagnostic_seed(seed,stats::runif(max(rows))[rows]) else numeric()
  attr(values,"diagnostic_seed") <- seed
  values
}

.rls_statmod_diagnostic <- function(record, fit, rows, imputation_index=1L) {
  if (!requireNamespace("statmod",quietly=TRUE))
    stop("Install the R package 'statmod' to calculate randomized quantile residuals.",call.=FALSE)
  mu <- as.numeric(stats::fitted(fit))
  if(length(rows)!=length(mu) || anyDuplicated(rows))
    stop("The diagnostic and fitted model must use the same distinct rows.",call.=FALSE)
  if (inherits(fit,"negbin")) {
    y <- as.numeric(fit$y)
    if(length(y)!=length(mu) || any(!is.na(y) & (!is.finite(y) | y<0 | y!=floor(y))) ||
       any(!is.na(mu) & (!is.finite(mu) | mu<0)) ||
       length(fit$theta)!=1L || !is.finite(fit$theta) || fit$theta<=0)
      stop("NB diagnostics require integer counts, valid fitted means and positive theta.",call.=FALSE)
  }
  seed <- .rls_diagnostic_seed(record,imputation_index)
  residual <- .rls_with_diagnostic_seed(seed,statmod::qresiduals(fit))
  # Record the draws for validation. statmod draws once per fitted observation.
  uniform <- .rls_with_diagnostic_seed(seed,stats::runif(length(mu)))
  invalid <- !is.na(residual) & !is.finite(residual)
  if(any(invalid)) {
    warning(paste("statmod returned non-finite quantile residuals in extreme probability tails.",
      "These diagnostic values are missing; no clipping or substitute residual was applied."),call.=FALSE)
    residual[invalid] <- NA_real_
  }
  list(residual=as.numeric(residual), uniform=uniform, seed=seed,
       cache_key=.rls_diagnostic_cache_key(record,fit,rows,imputation_index))
}

.rls_dunn_smyth_verification_code <- function(record) {
  tables <- record$diagnostics_by_imputation %||% list(record$diagnostics)
  valid <- vapply(tables, function(d) is.data.frame(d) && nrow(d) &&
    all(c("diagnostic_seed", "row_id") %in% names(d)), logical(1L))
  if (!all(valid) || !length(tables)) return("")
  seed_code <- paste(vapply(tables,function(d)as.character(d$diagnostic_seed[[1L]]),character(1L)),collapse=",")
  row_code <- paste(vapply(tables,function(d)paste0("c(",paste(d$row_id,collapse=","),")"),character(1L)),collapse=",")
  maximum <- .rls_nb_known_maximum(record)
  paste(c(
    "# Dunn-Smyth: statmod and withr; captured per-fit seeds and immutable original row IDs.",
    paste0("diagnostic_seeds <- c(",seed_code,")"),
    paste0("diagnostic_rows_by_imputation <- list(",row_code,")"),
    paste0("diagnostic_configured_seed <- ",record$diagnostic_seed %||% 104729L),
    paste0("diagnostic_maximum <- ",if(is.finite(maximum))as.character(maximum) else "NA_real_"),
    "stopifnot(diagnostic_imputation <= length(diagnostic_seeds))",
    "diagnostic_seed <- diagnostic_seeds[[diagnostic_imputation]]",
    "diagnostic_row_ids <- diagnostic_rows_by_imputation[[diagnostic_imputation]]",
    "stopifnot(length(diagnostic_row_ids) == length(observed))",
    "diagnostic_uniform <- withr::with_seed(diagnostic_seed, stats::runif(max(diagnostic_row_ids))[diagnostic_row_ids],",
    "  .rng_kind='Mersenne-Twister', .rng_normal_kind='Inversion', .rng_sample_kind='Rejection')",
    "diagnostic_beta_binomial_residual <- function(observed, trials, mu, precision, lower, upper) {",
    "  p <- lower + diagnostic_uniform * (upper - lower)",
    "  bad <- !is.finite(lower) | !is.finite(upper) | lower < 0 | upper > 1 | upper < lower | !is.finite(p) | p <= 0 | p >= 1",
    "  p[bad] <- NA_real_; r <- stats::qnorm(p); bad <- bad | !is.finite(r)",
    "  for (k in which(bad)) {",
    "    y <- observed[k]; size <- trials[k]; m <- mu[k]; phi <- precision[k]; u <- diagnostic_uniform[k]",
    "    if (!all(is.finite(c(y, size, m, phi, u))) || y < 0 || y > size || size < 0 || y != floor(y) || size != floor(size) || m <= 0 || m >= 1 || phi <= 0 || u <= 0 || u >= 1) next",
    "    point <- gamlss.dist::dBB(y, mu=m, sigma=1/phi, bd=size)",
    "    above <- if (y < size) sum(gamlss.dist::dBB(seq.int(y+1L, size), mu=m, sigma=1/phi, bd=size)) else 0",
    "    tail <- above + (1-u)*point",
    "    if (is.finite(tail) && tail > 0 && tail < 1) r[k] <- stats::qnorm(tail, lower.tail=FALSE)",
    "  }",
    "  bad <- !is.finite(r)",
    "  if (any(bad)) {warning('R beta-binomial probabilities were invalid; left missing without clipping.'); r[bad] <- NA_real_}",
    "  as.numeric(r)",
    "}",
    "diagnostic_cdf_residual <- function(lower, upper) {",
    "  bad <- !is.finite(lower) | !is.finite(upper) | lower<0 | upper>1 | upper<lower",
    "  p <- lower + diagnostic_uniform * (upper-lower); p[bad] <- NA_real_",
    "  r <- stats::qnorm(p); bad <- bad | !is.finite(r)",
    "  if(any(bad)) {warning('R distribution functions returned invalid or extreme-tail values; left missing without clipping.'); r[bad] <- NA_real_}",
    "  r",
    "}",
    "# Compatible GLMs use statmod directly. Its extreme-tail limitations are not hidden.",
    "diagnostic_statmod_residual <- function(f) {",
    "  r <- withr::with_seed(diagnostic_seed, statmod::qresiduals(f),",
    "    .rng_kind='Mersenne-Twister', .rng_normal_kind='Inversion', .rng_sample_kind='Rejection')",
    "  bad <- !is.na(r) & !is.finite(r)",
    "  if(any(bad)) {warning('statmod returned non-finite extreme-tail residuals; left missing without clipping.'); r[bad] <- NA_real_}",
    "  as.numeric(r)",
    "}"
  ),collapse="\n")
}

.rls_nb_known_maximum <- function(record, data = NULL) {
  if (!isTRUE(record$count_regression) || !identical(record$count_distribution,"negative_binomial")) return(NA_real_)
  bounds <- record$response_bounds
  if (is.null(bounds) && !is.null(data)) bounds <- attr(data[[record$response]],"theoretical_range",exact=TRUE)
  if (is.null(bounds)) return(NA_real_)
  bounds <- suppressWarnings(as.numeric(bounds))
  if (length(bounds)!=2L || any(!is.finite(bounds)) || bounds[2]<0 || bounds[1]>=bounds[2])
    stop("The configured theoretical range must contain two finite increasing bounds.",call.=FALSE)
  bounds[[2L]]
}

.rls_nb_diagnostic_note <- function(d) {
  if (!is.data.frame(d) || !nrow(d) || !"diagnostic_above_max" %in% names(d)) return("")
  p <- d$diagnostic_above_max; p <- p[is.finite(p)]
  if (!length(p)) return("")
  sprintf("Selected fit: P(Y > %s), mean %.3g; max %.3g (descriptive).",
          format(d$diagnostic_maximum[[1L]],trim=TRUE),mean(p),max(p))
}

.rls_native_diagnostic_notes_payload <- function(record) {
  tables <- record$diagnostics_by_imputation %||% list(record$diagnostics)
  notes <- vapply(tables,.rls_nb_diagnostic_note,character(1L))
  c("GENERALIZED_DIAGNOSTIC_NOTES_V1",as.character(length(notes)),.rls_native_wire_value(notes))
}

.rls_beta_binomial_library_quantile <- function(observed, trials, mu, precision,
                                               uniform, lower, upper) {
  # pBB sums point masses from zero. At the right boundary that sum can be
  # slightly above one even though the fitted beta-binomial mass is valid.
  # Re-evaluate only non-finite randomized quantiles from the opposite tail,
  # using the same public GAMLSS distribution and R's normal quantile.
  probability <- lower + uniform * (upper - lower)
  invalid <- !is.finite(lower) | !is.finite(upper) |
    lower < 0 | upper > 1 | upper < lower |
    !is.finite(probability) | probability <= 0 | probability >= 1
  probability[invalid] <- NA_real_
  residual <- stats::qnorm(probability)
  invalid <- invalid | !is.finite(residual)
  for (i in which(invalid)) {
    y <- observed[[i]]
    size <- trials[[i]]
    p <- mu[[i]]
    phi <- precision[[i]]
    u <- uniform[[i]]
    if (!all(is.finite(c(y, size, p, phi, u))) ||
        y < 0 || size < 0 || y > size || y != floor(y) ||
        size != floor(size) || p <= 0 || p >= 1 || phi <= 0 ||
        u <= 0 || u >= 1) next
    masses <- tryCatch({
      point <- gamlss.dist::dBB(y, mu = p, sigma = 1 / phi, bd = size)
      above <- if (y < size) sum(gamlss.dist::dBB(
        seq.int(y + 1L, size), mu = p, sigma = 1 / phi, bd = size
      )) else 0
      c(point = point, above = above)
    }, error = function(e) c(point = NA_real_, above = NA_real_))
    point <- masses[["point"]]
    above <- masses[["above"]]
    right_probability <- above + (1 - u) * point
    if (!all(is.finite(c(point, above, right_probability))) ||
        point < 0 || above < 0 || right_probability <= 0 ||
        right_probability >= 1) next
    recovered <- stats::qnorm(right_probability, lower.tail = FALSE)
    if (!is.finite(recovered)) next
    residual[[i]] <- recovered
    lower[[i]] <- 1 - (above + point)
    upper[[i]] <- 1 - above
  }
  missing <- !is.finite(residual)
  if (any(missing)) {
    warning(sprintf(
      "R distribution functions could not supply finite beta-binomial quantile residuals for %d row(s); left missing without clipping.",
      sum(missing)
    ), call. = FALSE)
    residual[missing] <- NA_real_
  }
  list(residual = as.numeric(residual), lower = as.numeric(lower),
       upper = as.numeric(upper))
}

.rls_quantile_from_library_cdf <- function(lower, upper, uniform) {
  invalid <- !is.finite(lower) | !is.finite(upper) | lower<0 | upper>1 | upper<lower
  probability <- lower + uniform * (upper-lower)
  probability[invalid] <- NA_real_
  r <- stats::qnorm(probability)
  bad <- invalid | !is.finite(r)
  if(any(bad)) {
    warning("R distribution functions could not supply finite quantile residuals for some rows; left missing without clipping.",call.=FALSE)
    r[bad] <- NA_real_
  }
  as.numeric(r)
}
