.rls_generalized_glm_id <- function(group, name = NULL) {
  .rls_validate_protocol_name(name %||% paste0("gglm:", group, ":", format(Sys.time(), "%Y%m%d%H%M%OS3")), "name")
}

.rls_generalized_glm_record <- function(model) {
  id <- if (inherits(model, "rlispstat_generalized_linear_model")) model$id else model
  id <- .rls_validate_protocol_name(id, "model")
  if (!exists(id, envir = .rls_state$generalized_glm_models, inherits = FALSE)) {
    stop("Unknown generalized linear model.", call. = FALSE)
  }
  get(id, envir = .rls_state$generalized_glm_models)
}

.rls_assign_generalized_glm <- function(record) {
  assign(record$id, record, envir = .rls_state$generalized_glm_models)
  if (is.null(record$fit)) .rls_glm_mark_diagnostic_plots_stale(record$id)
  classes <- if (isTRUE(record$count_regression)) {
    c("rlispstat_count_regression", "rlispstat_generalized_linear_model")
  } else if (identical(record$model_type %||% "", "positive_continuous")) {
    c("rlispstat_positive_continuous_model", "rlispstat_generalized_linear_model")
  } else if (identical(record$model_type %||% "", "proportion")) {
    c("rlispstat_proportion_model", "rlispstat_generalized_linear_model")
  } else {
    "rlispstat_generalized_linear_model"
  }
  structure(list(id = record$id, group = record$group), class = classes)
}

.rls_glm_supported_families <- c(
  "gaussian", "gaussian_log", "lognormal", "binomial", "poisson", "Gamma", "inverse.gaussian",
  "quasibinomial", "quasipoisson", "beta", "beta_one_inflated",
  "gamma_distance"
)

.rls_glm_bounded_families <- c("beta", "beta_one_inflated", "gamma_distance")

.rls_glm_is_bounded_family <- function(family) {
  as.character(family)[[1L]] %in% .rls_glm_bounded_families
}

.rls_glm_family_links <- list(
  gaussian = c("identity", "log", "inverse"),
  gaussian_log = "log",
  lognormal = "identity",
  binomial = c("logit", "probit", "cloglog", "cauchit", "log"),
  poisson = c("log", "identity", "sqrt"),
  Gamma = c("inverse", "identity", "log"),
  inverse.gaussian = c("1/mu^2", "inverse", "identity", "log"),
  quasibinomial = c("logit", "probit", "cloglog", "cauchit", "log"),
  quasipoisson = c("log", "identity", "sqrt"),
  beta = "logit",
  beta_one_inflated = "logit",
  gamma_distance = "log"
)

.rls_glm_default_link <- function(family) {
  switch(family,
    gaussian = "identity",
    gaussian_log = "log",
    lognormal = "identity",
    binomial = "logit",
    poisson = "log",
    Gamma = "inverse",
    inverse.gaussian = "1/mu^2",
    quasibinomial = "logit",
    quasipoisson = "log",
    beta = "logit",
    beta_one_inflated = "logit",
    gamma_distance = "log"
  )
}

.rls_glm_make_family <- function(family, link = NULL) {
  family <- match.arg(family, .rls_glm_supported_families)
  link <- link %||% .rls_glm_default_link(family)
  allowed <- .rls_glm_family_links[[family]]
  if (!link %in% allowed) {
    stop(sprintf(
      "Invalid link `%s` for family `%s`. Valid links are: %s.",
      link, family, paste(allowed, collapse = ", ")
    ), call. = FALSE)
  }
  if (family %in% c("beta", "beta_one_inflated")) {
    return(structure(list(family = family, link = link), class = "rlispstat_bounded_family"))
  }
  if (identical(family, "gamma_distance")) {
    object <- stats::Gamma(link = "log")
    object$family <- family
    return(object)
  }
  if (identical(family, "gaussian_log")) {
    return(stats::gaussian(link = "log"))
  }
  if (identical(family, "lognormal")) {
    object <- stats::gaussian(link = "identity")
    object$family <- "lognormal"
    return(object)
  }
  constructor <- get(family, envir = asNamespace("stats"))
  tryCatch(
    constructor(link = link),
    error = function(e) stop(sprintf("Invalid family/link combination: %s", conditionMessage(e)), call. = FALSE)
  )
}

.rls_glm_validate_response_bounds <- function(response_bounds, family) {
  if (!.rls_glm_is_bounded_family(family) && is.null(response_bounds)) return(NULL)
  bounds <- suppressWarnings(as.numeric(response_bounds))
  if (length(bounds) != 2L || any(!is.finite(bounds)) || bounds[[1L]] >= bounds[[2L]]) {
    message <- if (.rls_glm_is_bounded_family(family)) {
      sprintf(
        "Family `%s` requires `response_bounds = c(lower, upper)` with two finite values and lower < upper.",
        family
      )
    } else "`response_bounds` must contain two finite values with lower < upper."
    stop(message, call. = FALSE)
  }
  unname(bounds)
}

.rls_glm_transform_bounded_response <- function(record, complete) {
  if (!.rls_glm_is_bounded_family(record$family)) return(complete)
  bounds <- .rls_glm_validate_response_bounds(record$response_bounds, record$family)
  response <- record$response
  observed <- complete$data[[response]]
  if (!is.numeric(observed) || any(!is.finite(observed))) {
    stop("Bounded-response models require a finite numeric response.", call. = FALSE)
  }
  lower <- bounds[[1L]]
  upper <- bounds[[2L]]
  if (any(observed < lower | observed > upper)) {
    stop(sprintf(
      "The response contains values outside the configured bounds [%s, %s].",
      format(lower), format(upper)
    ), call. = FALSE)
  }
  scaled <- (observed - lower) / (upper - lower)
  if (identical(record$family, "beta")) {
    if (any(scaled <= 0 | scaled >= 1)) {
      stop("Beta regression requires every used response value to lie strictly between the configured bounds. Use `beta_one_inflated` when exact upper-bound values are structural.", call. = FALSE)
    }
    transformed <- scaled
  } else if (identical(record$family, "beta_one_inflated")) {
    if (any(scaled <= 0 | scaled > 1)) {
      stop("One-inflated beta regression allows values inside the bounds and at the upper bound, but not at the lower bound.", call. = FALSE)
    }
    if (!any(scaled == 1)) {
      stop("One-inflated beta regression requires at least one response value exactly at the configured upper bound.", call. = FALSE)
    }
    transformed <- scaled
  } else {
    transformed <- upper - observed
    if (any(transformed <= 0)) {
      stop("Gamma distance-to-maximum regression requires Y < upper for every used observation; Gamma cannot model zero distance.", call. = FALSE)
    }
  }
  complete$original_response <- observed
  complete$data[[response]] <- transformed
  complete$response_bounds <- bounds
  complete
}

.rls_hurdle_attach_separate_component_fits <- function(
    record, fit, data, formula, control) {
  grouped <- .rls_count_grouped_response(record, data)
  component_data <- data
  component_data$.linkeda_perfect_score <- as.numeric(
    grouped$successes == grouped$trials
  )
  perfect_score_boundary <- if (all(component_data$.linkeda_perfect_score == 0)) {
    "no_perfect_scores"
  } else if (all(component_data$.linkeda_perfect_score == 1)) {
    "only_perfect_scores"
  } else ""
  if (identical(perfect_score_boundary, "only_perfect_scores")) {
    stop(
      paste(
        "The ceiling-hurdle model cannot estimate its below-ceiling component",
        "because every used response is a perfect score."
      ),
      call. = FALSE
    )
  }
  perfect_formula <- stats::reformulate(
    record$terms %||% character(), response = ".linkeda_perfect_score"
  )
  perfect_fit <- tryCatch(
    stats::glm(
      perfect_formula, data = component_data,
      family = stats::binomial(link = "logit"),
      # The IRLS default is only 25 iterations.  In a multiply-imputed hurdle
      # model a small change in one completed dataset can make just that fit
      # need more iterations even though the same finite MLE exists.
      control = stats::glm.control(maxit = 200L, epsilon = 1e-8)
    ),
    error = function(error) stop(
      "Perfect-score logistic component failed: ", conditionMessage(error),
      call. = FALSE
    )
  )

  below_rows <- grouped$successes < grouped$trials
  below_data <- data[below_rows, , drop = FALSE]
  if (nrow(below_data) <= length(record$terms %||% character()) + 1L) {
    stop(
      "The truncated beta-binomial component has too few observations below the ceiling.",
      call. = FALSE
    )
  }
  conditional_fit <- tryCatch(
    gamlss::gamlss(
      formula, data = below_data,
      family = gamlss.dist::ZABB(
        mu.link = "logit", sigma.link = "log", nu.link = "logit"
      ),
      # With no zero failures in this subset, fixing nu removes only an
      # irrelevant likelihood constant.  The remaining likelihood is exactly
      # the upper-truncated beta-binomial component of the hurdle model.
      # Explicit interior starting values avoid the non-finite mu working
      # weights that GAMLSS's automatic dispersion initialisation can produce
      # for sparse, high-order interactions in individual imputations.  They
      # initialise the optimiser only; sigma remains freely estimated.
      nu.fix = TRUE, nu.start = 0.001, sigma.start = 0.05,
      control = control
    ),
    error = function(error) stop(
      "Truncated beta-binomial component below the ceiling failed: ",
      conditionMessage(error), call. = FALSE
    )
  )
  conditional_fit$call$family <- quote(gamlss.dist::ZABB(
    mu.link = "logit", sigma.link = "log", nu.link = "logit"
  ))

  # A joint ZABB object is convenient when it exists, but it is not required
  # by the factorized hurdle likelihood.  If the joint optimiser itself
  # failed, retain the conditional GAMLSS fit as the carrier object and attach
  # both independently fitted components below.
  if (is.null(fit) || !inherits(fit, "gamlss")) fit <- conditional_fit

  full_model_frame <- stats::model.frame(
    formula, data = data, na.action = stats::na.fail
  )
  fit$linkeda_model_data <- data
  fit$xlevels <- stats::.getXlevels(stats::terms(formula), full_model_frame)
  fit$contrasts <- attr(
    stats::model.matrix(formula, data = full_model_frame), "contrasts"
  )
  attr(fit, "linkeda_ceiling_hurdle") <- TRUE
  attr(fit, "linkeda_trials_variable") <- record$trials_variable %||% ""
  attr(fit, "linkeda_trials_constant") <-
    record$trials_constant %||% NA_real_

  design_terms <- stats::delete.response(stats::terms(formula))
  model_frame <- stats::model.frame(formula, data = data, na.action = stats::na.fail)
  design <- stats::model.matrix(design_terms, data = model_frame)
  mu_lp <- as.numeric(design %*% conditional_fit$mu.coefficients)
  sigma_lp <- rep(unname(conditional_fit$sigma.coefficients[[1L]]), nrow(data))
  perfect_design <- stats::model.matrix(perfect_formula, data = component_data)
  nu_lp <- as.numeric(perfect_design %*% stats::coef(perfect_fit))

  fit$linkeda_perfect_score_fit <- perfect_fit
  fit$linkeda_below_ceiling_fit <- conditional_fit
  fit$linkeda_hurdle_component_fallback <- TRUE
  fit$linkeda_perfect_score_boundary <- perfect_score_boundary
  fit$linkeda_component_mu_lp <- mu_lp
  fit$linkeda_component_mu_fv <- stats::plogis(mu_lp)
  fit$linkeda_component_sigma_lp <- sigma_lp
  fit$linkeda_component_sigma_fv <- exp(sigma_lp)
  fit$linkeda_component_nu_lp <- if (
    identical(perfect_score_boundary, "no_perfect_scores")
  ) rep(-Inf, nrow(data)) else nu_lp
  fit$linkeda_component_nu_fv <- if (
    identical(perfect_score_boundary, "no_perfect_scores")
  ) rep(0, nrow(data)) else stats::plogis(nu_lp)
  fit
}

.rls_glm_fit_engine <- function(record, data) {
  formula <- .rls_generalized_glm_formula_object(record)
  if (identical(record$family, "lognormal")) {
    return(stats::lm(formula, data = data))
  }
  if (identical(record$family, "beta")) {
    .rls_require_optional_packages("betareg", "Beta regression")
    return(betareg::betareg(formula, data = data, link = record$link))
  }
  if (identical(record$family, "beta_one_inflated")) {
    .rls_require_optional_packages(
      c("gamlss", "gamlss.dist"), "One-inflated beta regression"
    )
    family_object <- do.call(
      gamlss.dist::BEOI, list(mu.link = record$link, nu.link = "logit")
    )
    fit <- gamlss::gamlss(
      formula, data = data, family = family_object, trace = FALSE
    )
    # gamlss records the local symbol `family_object` in the fitted call.
    # Methods such as vcov() later re-evaluate that call after this helper has
    # returned, so retain a self-contained family expression instead.
    fit$call$family <- substitute(
      gamlss.dist::BEOI(mu.link = LINK, nu.link = "logit"),
      list(LINK = record$link)
    )
    return(fit)
  }
  if (isTRUE(record$count_regression) &&
      identical(record$count_distribution, "negative_binomial")) {
    .rls_require_optional_packages("MASS", "Negative-binomial Count Model")
    return(MASS::glm.nb(formula, data = data, link = "log"))
  }
  if (.rls_count_is_ceiling_hurdle(record)) {
    .rls_require_optional_packages(
      c("gamlss", "gamlss.dist"), "Hurdle beta-binomial Count Model"
    )
    nu_formula <- stats::reformulate(record$terms %||% character())
    family_object <- gamlss.dist::ZABB(
      mu.link = "logit", sigma.link = "log", nu.link = "logit"
    )
    hurdle_control <- gamlss::gamlss.control(
      n.cyc = 200L, c.crit = 0.001, trace = FALSE
    )
    grouped <- .rls_count_grouped_response(record, data)
    if (!any(grouped$successes == grouped$trials)) {
      fit <- .rls_hurdle_attach_separate_component_fits(
        record, NULL, data, formula, hurdle_control
      )
      fit$linkeda_joint_fit_error <- paste(
        "The joint perfect-score component is not identifiable because",
        "there are no perfect scores."
      )
    } else {
    fit <- tryCatch(
      gamlss::gamlss(
        formula, nu.formula = nu_formula, data = data,
        family = family_object,
        # The default GAMLSS limit is only 20 outer cycles.  That is often too
        # small for a joint ZABB model and can make otherwise identical MI fits
        # fail in only a few completed datasets.  A larger iteration budget does
        # not change the statistical model; it lets the R optimiser reach the
        # convergence criterion consistently across imputations.
        control = hurdle_control
      ),
      error = identity
    )
    if (inherits(fit, "error")) {
      joint_error <- conditionMessage(fit)
      fit <- .rls_hurdle_attach_separate_component_fits(
        record, NULL, data, formula, hurdle_control
      )
      fit$linkeda_joint_fit_error <- joint_error
    }
    }
    fit$call$family <- quote(gamlss.dist::ZABB(
      mu.link = "logit", sigma.link = "log", nu.link = "logit"
    ))
    fit$call$nu.formula <- nu_formula
    model_frame <- stats::model.frame(formula, data = data, na.action = stats::na.fail)
    # Retain the analysis-ready rows, including a Trials column when Trials is
    # variable.  The ordinary GAMLSS model frame stores the two-column response
    # as one matrix column and would otherwise lose the original Trials field
    # needed for response-scale effects.
    fit$linkeda_model_data <- data
    fit$xlevels <- stats::.getXlevels(stats::terms(formula), model_frame)
    fit$contrasts <- attr(stats::model.matrix(formula, data = model_frame), "contrasts")
    attr(fit, "linkeda_ceiling_hurdle") <- TRUE
    attr(fit, "linkeda_trials_variable") <- record$trials_variable %||% ""
    attr(fit, "linkeda_trials_constant") <- record$trials_constant %||% NA_real_
    covariance_ok <- tryCatch({
      covariance_fit <- fit
      covariance_fit$call$data <- NULL
      covariance <- as.matrix(stats::vcov(covariance_fit))
      nrow(covariance) > 0L && all(is.finite(covariance))
    }, error = function(error) FALSE)
    if (!isTRUE(fit$converged) || !isTRUE(covariance_ok)) {
      fit <- .rls_hurdle_attach_separate_component_fits(
        record, fit, data, formula, hurdle_control
      )
    }
    return(fit)
  }
  if (isTRUE(record$count_regression) &&
      identical(record$count_distribution, "beta_binomial")) {
    .rls_require_optional_packages("glmmTMB", "Beta-binomial Count Model")
    return(glmmTMB::glmmTMB(
      formula, data = data,
      family = glmmTMB::betabinomial(link = "logit")
    ))
  }
  stats::glm(formula, data = data, family = record$family_object)
}

.rls_hurdle_component_coefficients <- function(
    fit, component = c("perfect_score", "below_ceiling")) {
  component <- match.arg(component)
  if (!inherits(fit, "gamlss") ||
      !isTRUE(attr(fit, "linkeda_ceiling_hurdle"))) {
    stop("The fitted object is not a ceiling-hurdle beta-binomial model.", call. = FALSE)
  }
  coefficients <- if (identical(component, "perfect_score") &&
      identical(fit$linkeda_perfect_score_boundary %||% "",
                "no_perfect_scores")) {
    template <- if (inherits(fit$linkeda_perfect_score_fit, "glm")) {
      stats::coef(fit$linkeda_perfect_score_fit)
    } else fit$nu.coefficients
    stats::setNames(rep(0, length(template)), names(template))
  } else if (identical(component, "perfect_score")) {
    if (inherits(fit$linkeda_perfect_score_fit, "glm")) {
      stats::coef(fit$linkeda_perfect_score_fit)
    } else fit$nu.coefficients
  } else {
    # The ZABB response is failures.  Negating its logit coefficients reports
    # the conditional component in the original success direction.
    conditional_fit <- fit$linkeda_below_ceiling_fit
    if (inherits(conditional_fit, "gamlss")) {
      -conditional_fit$mu.coefficients
    } else -fit$mu.coefficients
  }
  stats::setNames(unname(as.numeric(coefficients)), names(coefficients))
}

.rls_hurdle_component_vcov <- function(
    fit, component = c("perfect_score", "below_ceiling")) {
  component <- match.arg(component)
  if (identical(component, "perfect_score") &&
      identical(fit$linkeda_perfect_score_boundary %||% "",
                "no_perfect_scores")) {
    coefficient_names <- names(.rls_hurdle_component_coefficients(fit, component))
    covariance <- matrix(
      0, length(coefficient_names), length(coefficient_names),
      dimnames = list(coefficient_names, coefficient_names)
    )
    return(covariance)
  }
  if (identical(component, "perfect_score") &&
      inherits(fit$linkeda_perfect_score_fit, "glm")) {
    covariance <- as.matrix(stats::vcov(fit$linkeda_perfect_score_fit))
    coefficient_names <- names(.rls_hurdle_component_coefficients(fit, component))
    dimnames(covariance) <- list(coefficient_names, coefficient_names)
    return(covariance)
  }
  if (identical(component, "below_ceiling") &&
      inherits(fit$linkeda_below_ceiling_fit, "gamlss")) {
    conditional_fit <- fit$linkeda_below_ceiling_fit
    covariance_fit <- conditional_fit
    covariance_fit$call$data <- NULL
    full <- as.matrix(stats::vcov(covariance_fit))
    n_mu <- length(conditional_fit$mu.coefficients %||% numeric())
    covariance <- full[seq_len(n_mu), seq_len(n_mu), drop = FALSE]
    coefficient_names <- names(.rls_hurdle_component_coefficients(fit, component))
    dimnames(covariance) <- list(coefficient_names, coefficient_names)
    return(covariance)
  }
  covariance_fit <- fit
  covariance_fit$call$data <- NULL
  full <- as.matrix(stats::vcov(covariance_fit))
  n_mu <- length(fit$mu.coefficients %||% numeric())
  n_sigma <- length(fit$sigma.coefficients %||% numeric())
  n_nu <- length(fit$nu.coefficients %||% numeric())
  indices <- if (identical(component, "perfect_score")) {
    n_mu + n_sigma + seq_len(n_nu)
  } else {
    seq_len(n_mu)
  }
  covariance <- full[indices, indices, drop = FALSE]
  coefficient_names <- names(.rls_hurdle_component_coefficients(fit, component))
  dimnames(covariance) <- list(coefficient_names, coefficient_names)
  covariance
}

.rls_hurdle_component_coefficient_table <- function(fit, component) {
  estimate <- .rls_hurdle_component_coefficients(fit, component)
  covariance <- .rls_hurdle_component_vcov(fit, component)
  std_error <- sqrt(diag(covariance))
  statistic <- estimate / std_error
  critical <- stats::qnorm(0.975)
  data.frame(
    term = names(estimate), estimate = unname(estimate),
    std_error = unname(std_error), statistic = unname(statistic),
    p_value = 2 * stats::pnorm(abs(statistic), lower.tail = FALSE),
    ci_lower = unname(estimate - critical * std_error),
    ci_upper = unname(estimate + critical * std_error),
    partial_r = NA_real_, delta_r2 = NA_real_,
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

.rls_empty_hurdle_coefficient_table <- function(multiple_imputation = FALSE) {
  out <- data.frame(
    term = character(), estimate = numeric(), std_error = numeric(),
    statistic = numeric(), p_value = numeric(), ci_lower = numeric(),
    ci_upper = numeric(), partial_r = numeric(), delta_r2 = numeric(),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  if (isTRUE(multiple_imputation)) {
    out$t_value <- numeric()
    out$df <- numeric()
    out$within_imputation_variance <- numeric()
    out$between_imputation_variance <- numeric()
    out$total_variance <- numeric()
    out$relative_increase_variance <- numeric()
    out$fraction_missing_information <- numeric()
    out$statistic_name <- character()
    out <- out[, c(
      "term", "estimate", "std_error", "statistic", "t_value", "p_value",
      "df", "ci_lower", "ci_upper", "partial_r", "delta_r2",
      "within_imputation_variance", "between_imputation_variance",
      "total_variance", "relative_increase_variance",
      "fraction_missing_information", "statistic_name"
    ), drop = FALSE]
  }
  out
}

.rls_hurdle_component_term_tests <- function(fit, component, source_terms = NULL) {
  coefficients <- .rls_hurdle_component_coefficients(fit, component)
  covariance <- .rls_hurdle_component_vcov(fit, component)
  labels <- attr(stats::terms(fit), "term.labels") %||% character()
  if (!is.null(source_terms)) {
    labels <- labels[vapply(labels, function(label) {
      any(vapply(source_terms, .rls_model_terms_equivalent, logical(1L), b = label))
    }, logical(1L))]
  }
  if (!length(labels)) return(.rls_empty_global_term_tests())
  tests <- lapply(labels, function(term) {
    coefficient_names <- .rls_model_term_coefficient_names(fit, term)
    indices <- match(coefficient_names, names(coefficients))
    statistic <- p_value <- df1 <- NA_real_
    if (length(indices) && !anyNA(indices)) {
      estimate <- coefficients[indices]
      within <- covariance[indices, indices, drop = FALSE]
      solution <- tryCatch(qr.solve(within, estimate), error = function(e) NULL)
      if (!is.null(solution)) {
        statistic <- unname(sum(estimate * solution))
        df1 <- length(indices)
        p_value <- stats::pchisq(statistic, df = df1, lower.tail = FALSE)
      }
    }
    data.frame(
      term = term, coefficient_names = paste(coefficient_names, collapse = "\u001f"),
      statistic_name = "Wald chi-square", statistic = statistic,
      df1 = df1, df2 = NA_real_, p_value = p_value,
      method = paste("joint Wald chi-square for the", component, "component"),
      stringsAsFactors = FALSE
    )
  })
  .rls_as_global_term_tests(do.call(rbind, tests))
}

.rls_validate_hurdle_component <- function(fit, component, imputation = NA_integer_) {
  label <- if (identical(component, "perfect_score")) {
    "Perfect-score logistic component"
  } else {
    "Truncated beta-binomial component below the ceiling"
  }
  failure <- function(message) list(
    ok = FALSE, component = component, imputation = imputation,
    status = paste0(label, " failed: ", message)
  )
  if (is.null(fit) || !inherits(fit, "gamlss")) return(failure("no fitted model was returned."))
  if (identical(component, "perfect_score") &&
      identical(fit$linkeda_perfect_score_boundary %||% "",
                "no_perfect_scores")) {
    return(list(
      ok = TRUE, component = component, imputation = imputation,
      status = paste(
        "Perfect-score logistic component not estimated because no perfect",
        "scores were observed; its probability is fixed at zero."
      )
    ))
  }
  component_fit <- if (identical(component, "perfect_score")) {
    fit$linkeda_perfect_score_fit %||% fit
  } else fit$linkeda_below_ceiling_fit %||% fit
  if (!isTRUE(component_fit$converged)) {
    return(failure("the R fit did not converge."))
  }
  coefficients <- tryCatch(
    .rls_hurdle_component_coefficients(fit, component), error = identity
  )
  if (inherits(coefficients, "error") || !length(coefficients) ||
      any(!is.finite(coefficients))) {
    return(failure("coefficients are absent or non-finite."))
  }
  covariance <- tryCatch(.rls_hurdle_component_vcov(fit, component), error = identity)
  if (inherits(covariance, "error") || any(!is.finite(covariance)) ||
      nrow(covariance) != length(coefficients)) {
    return(failure("the covariance matrix is absent, singular, or non-finite."))
  }
  list(
    ok = TRUE, component = component, imputation = imputation,
    status = paste0(label, " fitted successfully.")
  )
}

.rls_assert_hurdle_one_df_consistency <- function(coefficient_rows, term_tests) {
  if (!isTRUE(getOption("linkeda.development_checks", FALSE))) return(invisible(TRUE))
  tests <- .rls_as_global_term_tests(term_tests)
  for (component in unique(as.character(tests$component %||% ""))) {
    component_tests <- tests[(tests$component %||% "") == component &
                               is.finite(tests$term_test_df) & tests$term_test_df == 1, , drop = FALSE]
    component_rows <- coefficient_rows[(coefficient_rows$component %||% "") == component, , drop = FALSE]
    for (i in seq_len(nrow(component_tests))) {
      source <- component_tests$term[[i]]
      children <- component_rows[component_rows$source_term == source &
                                   component_rows$row_type == "coefficient", , drop = FALSE]
      children <- children[is.finite(children$statistic), , drop = FALSE]
      if (nrow(children) == 1L && !isTRUE(all.equal(
          component_tests$term_test_statistic[[i]], children$statistic[[1L]]^2,
          tolerance = 1e-7))) {
        stop(sprintf("Internal hurdle test mismatch for %s in %s.", source, component), call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}

.rls_glm_mean_coefficients <- function(fit) {
  coefficients <- if (inherits(fit, "glmmTMB")) {
    glmmTMB::fixef(fit)$cond
  } else if (inherits(fit, "betareg")) {
    fit$coefficients$mean
  } else if (inherits(fit, "gamlss")) {
    fit$mu.coefficients
  } else {
    stats::coef(fit)
  }
  stats::setNames(unname(as.numeric(coefficients)), names(coefficients))
}

.rls_glm_mean_vcov <- function(fit) {
  coefficients <- .rls_glm_mean_coefficients(fit)
  covariance <- if (inherits(fit, "glmmTMB")) {
    stats::vcov(fit)$cond
  } else if (inherits(fit, "betareg")) {
    stats::vcov(fit, model = "mean")
  } else if (inherits(fit, "gamlss")) {
    # vcov.gamlss() needlessly re-resolves the expression stored in call$data,
    # even though its likelihood calculation uses the response, weights and
    # design matrices already stored in the fitted object.  LinkEDA fits each
    # imputation inside a local function, so that transient symbol no longer
    # exists later.  Removing only the call metadata avoids the lookup while
    # leaving the fitted statistical object and covariance calculation intact.
    covariance_fit <- fit
    covariance_fit$call$data <- NULL
    full <- stats::vcov(covariance_fit)
    full[seq_along(coefficients), seq_along(coefficients), drop = FALSE]
  } else {
    stats::vcov(fit)
  }
  covariance <- as.matrix(covariance)
  if (nrow(covariance) == length(coefficients) && ncol(covariance) == length(coefficients)) {
    dimnames(covariance) <- list(names(coefficients), names(coefficients))
  }
  covariance
}

.rls_glm_fit_rank <- function(fit) {
  design <- tryCatch(stats::model.matrix(fit), error = function(e) NULL)
  as.integer(fit$rank %||% if (is.matrix(design)) qr(design)$rank else NA_integer_)
}

.rls_glm_fit_parameter_count <- function(fit) {
  if (inherits(fit, "glmmTMB")) {
    as.integer(attr(stats::logLik(fit), "df") %||% length(.rls_glm_mean_coefficients(fit)))
  } else if (inherits(fit, "betareg")) {
    length(unlist(fit$coefficients, use.names = FALSE))
  } else if (inherits(fit, "gamlss")) {
    as.integer(fit$df.fit %||% length(fit$mu.coefficients))
  } else {
    length(stats::coef(fit))
  }
}

.rls_glm_fit_iterations <- function(fit) {
  if (inherits(fit, "glmmTMB")) {
    as.integer(fit$fit$iterations %||% NA_integer_)
  } else if (inherits(fit, "betareg")) {
    as.integer(fit$optim$counts[["function"]] %||% NA_integer_)
  } else if (inherits(fit, "lm") && !inherits(fit, "glm")) {
    NA_integer_
  } else {
    as.integer(fit$iter %||% NA_integer_)
  }
}

.rls_glm_fit_residual_deviance <- function(fit) {
  # glmmTMB deliberately has no deviance residual/deviance definition for its
  # beta-binomial family.  Calling stats::deviance() emits a warning and leaves
  # a non-finite value, so capability-based diagnostics report it as unavailable.
  if (inherits(fit, "glmmTMB")) {
    value <- unname(fit$deviance %||% NA_real_)
    return(if (length(value) == 1L && is.finite(value)) value else NA_real_)
  }
  value <- unname(fit$deviance %||% fit$G.deviance %||%
                    tryCatch(stats::deviance(fit), error = function(e) NA_real_))
  if (length(value) == 1L && is.finite(value)) value else NA_real_
}

.rls_glm_fit_dispersion <- function(fit) {
  value <- if (inherits(fit, "glmmTMB")) {
    unname(stats::sigma(fit))
  } else if (inherits(fit, "betareg")) {
    mean(as.numeric(stats::predict(fit, type = "precision")), na.rm = TRUE)
  } else if (inherits(fit, "gamlss")) {
    mean(as.numeric(
      fit$linkeda_component_sigma_fv %||% fit$sigma.fv %||% NA_real_
    ), na.rm = TRUE)
  } else if (inherits(fit, "lm") && !inherits(fit, "glm")) {
    unname(summary(fit)$sigma^2)
  } else {
    unname(summary(fit)$dispersion)
  }
  if (length(value) == 1L && is.finite(value)) value else NA_real_
}

# Family diagnostics are calculated from each fitted R object. They are
# descriptive quantities, never inputs to coefficient pooling or inference.
.rls_beta_binomial_variance_diagnostics <- function(phi, trials) {
  phi <- as.numeric(phi)
  trials <- as.numeric(trials)
  if (length(phi) != 1L || !is.finite(phi) || phi <= 0 ||
      !length(trials) || any(!is.finite(trials)) || any(trials < 1))
    return(list())
  rho <- 1 / (1 + phi)
  list(
    beta_binomial_rho = rho,
    beta_binomial_variance_inflation = mean(1 + (trials - 1) * rho),
    beta_binomial_trials_mean = mean(trials),
    beta_binomial_trials_vary = length(unique(trials)) > 1L
  )
}

.rls_negative_binomial_variance_diagnostics <- function(theta, fitted_mean) {
  theta <- as.numeric(theta)
  fitted_mean <- as.numeric(fitted_mean)
  if (length(theta) != 1L || !is.finite(theta) || theta <= 0 ||
      !length(fitted_mean) || any(!is.finite(fitted_mean)) ||
      any(fitted_mean < 0)) return(list())
  mu <- mean(fitted_mean)
  list(negative_binomial_mean_fitted_count = mu,
       negative_binomial_variance_inflation = 1 + mu / theta)
}

.rls_glm_family_diagnostics <- function(record, fit, complete) {
  result <- list()
  family <- as.character(record$family %||% "")
  distribution <- if (isTRUE(record$count_regression))
    as.character(record$count_distribution %||% "") else ""
  pearson_ratio <- function() {
    df <- suppressWarnings(as.numeric(stats::df.residual(fit)))
    residual <- tryCatch(as.numeric(stats::residuals(fit, type = "pearson")),
                         error = function(error) numeric())
    if (length(df) != 1L || !is.finite(df) || df <= 0L ||
        !length(residual) || any(!is.finite(residual))) return(NA_real_)
    sum(residual^2) / df
  }
  if (distribution == "beta_binomial") {
    phi <- .rls_glm_fit_dispersion(fit)
    grouped <- .rls_count_grouped_response(record, complete$data)
    result <- .rls_beta_binomial_variance_diagnostics(phi, grouped$trials)
  } else if (distribution == "negative_binomial") {
    mu <- tryCatch(as.numeric(stats::fitted(fit)), error = function(error) numeric())
    result <- .rls_negative_binomial_variance_diagnostics(
      fit$theta %||% NA_real_, mu)
  } else if (distribution == "poisson" ||
             (!nzchar(distribution) && family == "poisson")) {
    result$pearson_dispersion_ratio <- pearson_ratio()
  } else if (distribution == "binomial_trials") {
    grouped <- .rls_count_grouped_response(record, complete$data)
    trials <- as.numeric(grouped$trials)
    if (length(trials) && all(is.finite(trials)) && any(trials > 1))
      result$pearson_dispersion_ratio <- pearson_ratio()
  } else if (family %in% c("Gamma", "gamma_distance")) {
    phi <- .rls_glm_fit_dispersion(fit)
    if (is.finite(phi) && phi >= 0) result$gamma_cv <- sqrt(phi)
  }
  result
}

.rls_glm_family_diagnostics_mi <- function(record, fits, completes) {
  per_imputation <- Map(function(fit, complete) {
    .rls_glm_family_diagnostics(record, fit, complete)
  }, fits, completes)
  keys <- unique(unlist(lapply(per_imputation, names), use.names = FALSE))
  result <- list()
  for (key in keys) {
    values <- vapply(per_imputation, function(item) {
      as.numeric(item[[key]] %||% NA_real_)
    }, numeric(1L))
    if (all(is.finite(values))) result[[key]] <- if (
        key == "beta_binomial_trials_vary") as.numeric(any(values > 0)) else mean(values)
  }
  result
}

.rls_count_distribution_uses_trials <- function(distribution) {
  as.character(distribution %||% "") %in% c(
    "binomial_trials", "beta_binomial", "hurdle_beta_binomial_ceiling",
    "perfect_score"
  )
}

.rls_count_is_ceiling_hurdle <- function(record) {
  isTRUE(record$count_regression) &&
    identical(record$count_distribution %||% "", "hurdle_beta_binomial_ceiling")
}

.rls_count_is_perfect_score <- function(record) {
  isTRUE(record$count_regression) &&
    identical(record$count_distribution %||% "", "perfect_score")
}

.rls_glm_exponentiated_effect <- function(record) {
  bounded_count <- isTRUE(record$count_regression) &&
    .rls_count_distribution_uses_trials(record$count_distribution)
  binomial_model <- isTRUE(record$binary_regression) || bounded_count ||
    identical(record$family, "binomial")
  if (binomial_model && identical(record$link, "logit")) {
    return(list(id = "odds_ratio", label = "Odds ratio"))
  }
  if (binomial_model && identical(record$link, "log")) {
    return(list(id = "risk_ratio", label = "Risk ratio"))
  }
  if (isTRUE(record$count_regression) && identical(record$link, "log")) {
    return(list(id = "rate_ratio", label = "Rate ratio"))
  }
  if (identical(record$family, "gaussian_log")) {
    return(list(id = "mean_ratio", label = "Mean ratio"))
  }
  if (identical(record$family, "lognormal")) {
    return(list(id = "multiplicative_ratio", label = "Multiplicative ratio"))
  }
  NULL
}

.rls_glm_augment_exponentiated <- function(rows, record) {
  if (!is.data.frame(rows) || !nrow(rows)) return(rows)
  effect <- .rls_glm_exponentiated_effect(record)
  if (is.null(effect)) return(rows)
  rows$exponentiated_estimate <- exp(rows$estimate)
  rows$exponentiated_lower <- exp(rows$ci_lower)
  rows$exponentiated_upper <- exp(rows$ci_upper)
  rows$exponentiated_effect <- effect$id
  rows$exponentiated_label <- effect$label
  if (identical(effect$id, "odds_ratio")) {
    rows$odds_ratio <- rows$exponentiated_estimate
    rows$odds_ratio_lower <- rows$exponentiated_lower
    rows$odds_ratio_upper <- rows$exponentiated_upper
  }
  if (identical(effect$id, "rate_ratio")) {
    rows$rate_ratio <- rows$exponentiated_estimate
    rows$rate_ratio_lower <- rows$exponentiated_lower
    rows$rate_ratio_upper <- rows$exponentiated_upper
  }
  rows
}

.rls_generalized_glm_formula_object <- function(record) {
  terms <- record$terms
  exposure <- as.character(record$exposure %||% "")[[1L]]
  offset_variable <- as.character(record$offset_variable %||% "")[[1L]]
  bounded_count <- isTRUE(record$count_regression) &&
    .rls_count_distribution_uses_trials(record$count_distribution)
  if (isTRUE(record$count_regression) && !bounded_count && nzchar(exposure)) {
    escaped <- gsub("`", "``", exposure, fixed = TRUE)
    terms <- c(terms, sprintf("offset(log(`%s`))", escaped))
  }
  if (nzchar(offset_variable)) {
    escaped <- gsub("`", "``", offset_variable, fixed = TRUE)
    terms <- c(terms, sprintf("offset(`%s`)", escaped))
  }
  # stats::reformulate() rejects character(0) on R 4.4.  An explicitly
  # requested intercept-only model is nevertheless a valid GLM, including
  # transformed and bounded-count responses.
  formula_terms <- if (length(terms)) terms else "1"
  if (identical(record$family, "lognormal")) {
    response <- sprintf("log(`%s`)", gsub("`", "``", record$response, fixed = TRUE))
    return(stats::reformulate(formula_terms, response = response))
  }
  if (bounded_count) {
    successes <- sprintf("`%s`", gsub("`", "``", record$response, fixed = TRUE))
    trials <- if (nzchar(record$trials_variable %||% "")) {
      sprintf("`%s`", gsub("`", "``", record$trials_variable, fixed = TRUE))
    } else {
      format(record$trials_constant, digits = 17L, scientific = FALSE,
             trim = TRUE)
    }
    response <- if (.rls_count_is_ceiling_hurdle(record)) {
      # ZABB is a zero-adjusted beta-binomial.  Expressing failures first makes
      # its zero mass exactly the requested upper-bound mass Y = Trials.
      sprintf("cbind(%s - %s, %s)", trials, successes, successes)
    } else if (.rls_count_is_perfect_score(record)) {
      sprintf("I(%s == %s)", successes, trials)
    } else {
      sprintf("cbind(%s, %s - %s)", successes, trials, successes)
    }
    return(stats::reformulate(formula_terms, response = response))
  }
  .rls_model_formula_object(record$response, terms)
}

.rls_generalized_validate_response <- function(record, values) {
  distribution <- if (isTRUE(record$count_regression)) {
    record$count_distribution
  } else record$family
  specification <- .rls_model_distribution_spec(distribution)
  .rls_validate_response_domain(values, specification$domain, specification$label)
}

.rls_count_validate_response <- function(data, response) {
  value <- data[[response]]
  if (!is.numeric(value)) {
    stop("Count Model requires a numeric response.", call. = FALSE)
  }
  observed <- value[!is.na(value)]
  if (!length(observed)) {
    stop("The count response has no observed values in the selected scope.", call. = FALSE)
  }
  if (any(!is.finite(observed)) || any(observed < 0) ||
      any(abs(observed - round(observed)) > sqrt(.Machine$double.eps))) {
    stop("Count Model requires finite, non-negative integer response values.", call. = FALSE)
  }
  invisible(TRUE)
}

.rls_count_validate_exposure <- function(data, exposure) {
  if (is.null(exposure) || !nzchar(exposure)) return(invisible(TRUE))
  if (!exposure %in% names(data)) {
    stop(sprintf("Exposure column `%s` was not found in the dataset.", exposure), call. = FALSE)
  }
  value <- data[[exposure]]
  if (!is.numeric(value)) stop("Exposure must be numeric.", call. = FALSE)
  observed <- value[!is.na(value)]
  if (!length(observed) || any(!is.finite(observed)) || any(observed <= 0)) {
    stop("Exposure values must be finite and greater than zero.", call. = FALSE)
  }
  invisible(TRUE)
}

.rls_generalized_glm_data_for_fit <- function(record) {
  data <- .rls_model_data_for_term_types(
    record$data, record$term_types %||% list(), response = record$response
  )
  .rls_glm_apply_factor_references(data, record$factor_reference_levels %||% list())
}

.rls_count_validate_bounded_response <- function(record, data) {
  if (!isTRUE(record$count_regression) ||
      !.rls_count_distribution_uses_trials(record$count_distribution)) {
    return(invisible(TRUE))
  }
  grouped <- .rls_count_grouped_response(record, data)
  if (!any(!grouped$missing)) {
    stop("The bounded-count response has no observed successes/trials pairs in the selected scope.",
         call. = FALSE)
  }
  invisible(TRUE)
}

.rls_generalized_glm_complete_data <- function(record) {
  .rls_model_complete_data(record$data, .rls_generalized_glm_formula_object(record),
                           record$scope %||% "all", record$selected_rows %||% integer())
}

.rls_generalized_fit_specification_witness <- function(record) {
  inspectable <- function(fit) inherits(fit, c("lm", "glm", "negbin", "betareg", "gamlss", "glmmTMB"))
  fits <- record$fits_by_imputation %||% list()
  fits <- Filter(inspectable, fits)
  if (!length(fits) && inspectable(record$fit)) fits <- list(record$fit)
  if (!length(fits)) {
    stop("The fitted generalized model has no inspectable R fit.", call. = FALSE)
  }

  witness_for <- function(fit) {
    matrix <- stats::model.matrix(fit)
    labels <- attr(stats::terms(fit), "term.labels") %||% character()
    assignment <- attr(matrix, "assign") %||% integer()
    columns <- lapply(seq_along(labels), function(index) {
      setdiff(colnames(matrix)[assignment == index], "(Intercept)")
    })
    names(columns) <- labels
    list(
      formula = paste(deparse(stats::formula(fit), width.cutoff = 500L), collapse = " "),
      terms = labels,
      coefficient_columns = columns,
      matrix_columns = colnames(matrix),
      n = nrow(matrix),
      parameter_count = ncol(matrix),
      rank = as.integer(fit$rank %||% qr(matrix)$rank),
      df_residual = as.integer(stats::df.residual(fit)),
      fitted = unname(stats::fitted(fit))
    )
  }
  witnesses <- lapply(fits, witness_for)
  first <- witnesses[[1L]]
  if (length(witnesses) > 1L) {
    same_structure <- vapply(witnesses[-1L], function(current) {
      identical(current$terms, first$terms) &&
        identical(current$matrix_columns, first$matrix_columns) &&
        identical(current$coefficient_columns, first$coefficient_columns)
    }, logical(1L))
    if (!all(same_structure)) {
      stop("The imputed generalized fits do not share one model specification.", call. = FALSE)
    }
  }
  first$imputation_count <- length(witnesses)
  first
}

.rls_assert_generalized_fit_matches_specification <- function(record, specification,
                                                               event = NULL,
                                                               reference = NULL) {
  expected_terms <- unique(as.character(specification$terms %||% character()))
  expected_types <- .rls_generalized_glm_normalize_term_types(
    specification$term_types %||% list(), expected_terms
  )
  expected_references <- specification$factor_reference_levels %||% list()
  if (!is.list(expected_references)) expected_references <- as.list(expected_references)
  if (is.null(names(expected_references))) expected_references <- list()

  same_named_values <- function(left, right) {
    left <- left %||% list()
    right <- right %||% list()
    identical(sort(names(left)), sort(names(right))) &&
      all(vapply(sort(names(left)), function(name) {
        identical(as.character(left[[name]])[[1L]], as.character(right[[name]])[[1L]])
      }, logical(1L)))
  }
  fail <- function(detail) {
    stop(sprintf("Fitted generalized model does not match its immutable specification: %s", detail),
         call. = FALSE)
  }

  if (!identical(as.character(record$response), as.character(specification$response)))
    fail("response changed")
  if (!identical(as.character(record$family), as.character(specification$family)))
    fail("family changed")
  if (!identical(as.character(record$link), as.character(specification$link)))
    fail("link changed")
  if (!identical(as.character(record$scope %||% "all"),
                 as.character(specification$scope %||% "all"))) fail("scope changed")
  if (!identical(as.character(record$offset_variable %||% ""),
                 as.character(specification$offset %||% ""))) fail("offset changed")
  if (!identical(as.character(record$terms %||% character()), expected_terms))
    fail("terms changed")
  if (!same_named_values(record$term_types %||% list(), expected_types))
    fail("term types changed")
  if (!identical(sort(unique(as.character(record$centered_predictors %||% character()))),
                 sort(unique(as.character(specification$centered_predictors %||% character())))))
    fail("centering changed")
  if (!same_named_values(record$factor_reference_levels %||% list(), expected_references))
    fail("reference categories changed")
  if (!identical(as.numeric(record$response_bounds %||% numeric()),
                 as.numeric(specification$response_bounds %||% numeric())))
    fail("response bounds changed")
  if (isTRUE(record$binary_regression)) {
    if (!is.null(event) && !identical(as.character(record$event), as.character(event)))
      fail("event changed")
    if (!is.null(reference) &&
        !identical(as.character(record$reference), as.character(reference)))
      fail("response reference changed")
  }

  witness <- .rls_generalized_fit_specification_witness(record)
  expected_labels <- attr(
    stats::terms(.rls_generalized_glm_formula_object(record)),
    "term.labels"
  ) %||% character()
  if (!identical(witness$terms, expected_labels)) fail("fitted formula terms changed")
  if (length(expected_labels) &&
      any(!vapply(witness$coefficient_columns[expected_labels], length, integer(1L)))) {
    fail("a fitted term has no design-matrix columns")
  }
  summary <- record$summary %||% list()
  if (!identical(record$analysis_backend, "multiple_imputation")) {
    if (is.finite(as.integer(summary$n_used %||% NA_integer_)) &&
        as.integer(summary$n_used) != witness$n) fail("N differs from the fitted design")
    if (is.finite(as.integer(summary$parameter_count %||% NA_integer_)) &&
        as.integer(summary$parameter_count) != witness$parameter_count) {
      fail("parameter count differs from the fitted design")
    }
    if (is.finite(as.integer(summary$rank %||% NA_integer_)) &&
        as.integer(summary$rank) != witness$rank) fail("rank differs from the fitted design")
    if (is.finite(as.integer(summary$df_residual %||% NA_integer_)) &&
        as.integer(summary$df_residual) != witness$df_residual) {
      fail("residual degrees of freedom differ from the fitted design")
    }
  }

  fits <- record$fits_by_imputation %||% list()
  fits <- Filter(function(fit) inherits(fit, c("glm", "negbin", "betareg", "gamlss", "glmmTMB")), fits)
  if (!length(fits) && inherits(record$fit, c("glm", "negbin", "betareg", "gamlss", "glmmTMB"))) fits <- list(record$fit)
  for (fit in fits) {
    if (!inherits(fit, c("betareg", "gamlss", "glmmTMB"))) {
      if (!identical(as.character(fit$family$link), as.character(record$link)))
        fail("the fitted link changed")
      if (!identical(record$count_distribution %||% "poisson", "negative_binomial") &&
          !identical(as.character(fit$family$family), as.character(record$family))) {
        fail("the fitted family changed")
      }
    }
    model_data <- stats::model.frame(fit)
    for (variable in names(expected_types)) {
      if (!variable %in% names(model_data)) next
      if (identical(expected_types[[variable]], "factor") && !is.factor(model_data[[variable]]))
        fail(sprintf("%s was not fitted as a categorical predictor", variable))
      if (identical(expected_types[[variable]], "numeric") && !is.numeric(model_data[[variable]]))
        fail(sprintf("%s was not fitted as numeric", variable))
    }
    for (variable in names(expected_references)) {
      if (!variable %in% names(model_data) || !is.factor(model_data[[variable]]) ||
          !identical(levels(model_data[[variable]])[[1L]],
                     as.character(expected_references[[variable]])[[1L]])) {
        fail(sprintf("%s was fitted with another reference", variable))
      }
    }
    for (variable in specification$centered_predictors %||% character()) {
      if (variable %in% names(model_data) && is.numeric(model_data[[variable]]) &&
          abs(mean(model_data[[variable]])) > sqrt(.Machine$double.eps)) {
        fail(sprintf("%s was not fitted with the requested centering", variable))
      }
    }
  }

  rows <- record$coefficient_rows %||% data.frame()
  represented <- if (is.data.frame(rows) && nrow(rows) && "source_term" %in% names(rows))
    unique(as.character(rows$source_term)) else character()
  if (any(!expected_terms %in% represented)) fail("semantic coefficient rows are incomplete")
  witness
}

.rls_generalized_glm_diagnostics <- function(
    record, fit, complete, imputation_index = 1L) {
  response <- record$response
  n <- nrow(complete$data)
  safe_vector <- function(expression, fallback = NA_real_) {
    value <- tryCatch(expression, error = function(e) rep(fallback, n))
    value <- unname(as.numeric(value))
    if (length(value) == 1L && n != 1L) value <- rep(value, n)
    if (length(value) != n) value <- rep(fallback, n)
    value
  }
  if (.rls_count_is_ceiling_hurdle(record)) {
    grouped <- .rls_count_grouped_response(record, complete$data)
    mu_failure <- safe_vector(fit$linkeda_component_mu_fv %||% fit$mu.fv)
    sigma <- safe_vector(fit$linkeda_component_sigma_fv %||% fit$sigma.fv)
    perfect_probability <- safe_vector(
      fit$linkeda_component_nu_fv %||% fit$nu.fv
    )
    conditional_failure <- vapply(seq_len(n), function(index) {
      trials <- as.integer(grouped$trials[[index]])
      p0 <- gamlss.dist::dBB(
        0, mu = mu_failure[[index]], sigma = sigma[[index]], bd = trials
      )
      denominator <- 1 - p0
      if (!is.finite(denominator) || denominator <= 0) return(NA_real_)
      failures <- seq_len(trials)
      sum(failures * gamlss.dist::dBB(
        failures, mu = mu_failure[[index]], sigma = sigma[[index]], bd = trials
      )) / denominator
    }, numeric(1L))
    conditional_score <- grouped$trials - conditional_failure
    overall_score <- perfect_probability * grouped$trials +
      (1 - perfect_probability) * conditional_score
    conditional_residual <- ifelse(
      grouped$successes < grouped$trials,
      grouped$successes - conditional_score,
      NA_real_
    )
    # Diagnostics for this model must describe the conditional, truncated
    # beta-binomial component rather than silently mixing it with the
    # perfect-score hurdle.  Retain the joint GAMLSS residual separately for
    # auditing, while the ordinary residual views use the explicit
    # below-ceiling response residual.
    joint_residual <- safe_vector(stats::residuals(fit))
    discrete <- .rls_discrete_diagnostic_components(
      record, fit, complete, fitted_probability = perfect_probability,
      imputation_index = imputation_index
    )
    return(data.frame(
      row_id = complete$rows, original_row_id = complete$rows,
      observed = grouped$successes,
      fitted = overall_score,
      fitted_probability = overall_score / grouped$trials,
      fitted_response_scale = overall_score,
      perfect_score_observed = as.numeric(grouped$successes == grouped$trials),
      perfect_score_probability = perfect_probability,
      conditional_expected_score = conditional_score,
      conditional_residual = conditional_residual,
      joint_model_residual = joint_residual,
      linear_predictor = safe_vector(
        fit$linkeda_component_mu_lp %||% fit$mu.lp
      ),
      residual = discrete$dunn_smyth_residual,
      dunn_smyth_residual = discrete$dunn_smyth_residual,
      diagnostic_seed = discrete$diagnostic_seed,
      diagnostic_uniform = discrete$uniform,
      diagnostic_cache_key = discrete$cache_key,
      raw_residual = discrete$raw_residual,
      deviance_residual = NA_real_,
      pearson_residual = discrete$pearson_residual,
      working_residual = NA_real_,
      standardized_residual = NA_real_, studentized_residual = NA_real_,
      leverage = NA_real_, cooks_distance = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  if (.rls_count_is_perfect_score(record)) {
    grouped <- .rls_count_grouped_response(record, complete$data)
    fitted_probability <- safe_vector(stats::fitted(fit))
    observed_perfect <- as.numeric(grouped$successes == grouped$trials)
    deviance_residual <- safe_vector(stats::residuals(fit, type = "deviance"))
    discrete <- .rls_discrete_diagnostic_components(
      record, fit, complete, fitted_probability = fitted_probability,
      imputation_index = imputation_index
    )
    return(data.frame(
      row_id = complete$rows, original_row_id = complete$rows,
      observed = observed_perfect, fitted = fitted_probability,
      fitted_probability = fitted_probability,
      fitted_response_scale = fitted_probability,
      perfect_score_observed = observed_perfect,
      perfect_score_probability = fitted_probability,
      original_successes = grouped$successes,
      linear_predictor = safe_vector(stats::predict(fit, type = "link")),
      residual = discrete$dunn_smyth_residual,
      dunn_smyth_residual = discrete$dunn_smyth_residual,
      diagnostic_seed = discrete$diagnostic_seed,
      diagnostic_uniform = discrete$uniform,
      diagnostic_cache_key = discrete$cache_key,
      raw_residual = discrete$raw_residual,
      deviance_residual = deviance_residual,
      pearson_residual = discrete$pearson_residual,
      working_residual = safe_vector(stats::residuals(fit, type = "working")),
      standardized_residual = safe_vector(stats::rstandard(fit, type = "deviance")),
      studentized_residual = safe_vector(stats::rstudent(fit)),
      leverage = safe_vector(stats::hatvalues(fit)),
      cooks_distance = safe_vector(stats::cooks.distance(fit)),
      stringsAsFactors = FALSE
    ))
  }
  fitted_model <- if (inherits(fit, "gamlss")) {
    mu <- safe_vector(fit$mu.fv)
    nu <- safe_vector(fit$nu.fv, 0)
    nu + (1 - nu) * mu
  } else {
    safe_vector(stats::fitted(fit))
  }
  bounds <- record$response_bounds %||% c(NA_real_, NA_real_)
  bounded_count <- isTRUE(record$count_regression) &&
    .rls_count_distribution_uses_trials(record$count_distribution)
  fitted_probability <- if (bounded_count) fitted_model else rep(NA_real_, n)
  fitted_response <- if (bounded_count) {
    trials <- if (nzchar(record$trials_variable %||% "")) {
      complete$data[[record$trials_variable]]
    } else rep(record$trials_constant, n)
    fitted_probability * trials
  } else if (record$family %in% c("beta", "beta_one_inflated")) {
    bounds[[1L]] + (bounds[[2L]] - bounds[[1L]]) * fitted_model
  } else if (identical(record$family, "gamma_distance")) {
    bounds[[2L]] - fitted_model
  } else if (identical(record$family, "lognormal")) {
    sigma2 <- unname(summary(fit)$sigma^2)
    exp(fitted_model + sigma2 / 2)
  } else fitted_model
  observed <- if (identical(record$family, "lognormal")) {
    log(complete$data[[record$response]])
  } else complete$original_response %||% complete$data[[response]]
  diagnostic_capabilities <- .rls_discrete_diagnostic_capabilities(record)
  discrete_model <- nzchar(diagnostic_capabilities$distribution)
  leverage <- if (discrete_model || inherits(fit, "glmmTMB")) {
    rep(NA_real_, n)
  } else safe_vector(stats::hatvalues(fit))
  cooks <- if (discrete_model) rep(NA_real_, n) else
    safe_vector(stats::cooks.distance(fit))
  standardized <- if (discrete_model) rep(NA_real_, n) else tryCatch(
    safe_vector(stats::rstandard(fit, type = "deviance")),
    error = function(e) rep(NA_real_, n)
  )
  studentized <- if (discrete_model) rep(NA_real_, n) else tryCatch(
    safe_vector(stats::rstudent(fit)),
    error = function(e) rep(NA_real_, n)
  )
  deviance_residual <- if (!"deviance" %in% diagnostic_capabilities$residuals &&
      discrete_model) {
    rep(NA_real_, n)
  } else if (inherits(fit, "gamlss")) {
    safe_vector(stats::residuals(fit))
  } else safe_vector(stats::residuals(fit, type = "deviance"))
  pearson_residual <- if (inherits(fit, "gamlss")) {
    rep(NA_real_, n)
  } else safe_vector(stats::residuals(fit, type = "pearson"))
  working_residual <- if (discrete_model ||
      inherits(fit, c("betareg", "gamlss", "glmmTMB"))) {
    rep(NA_real_, n)
  } else safe_vector(stats::residuals(fit, type = "working"))
  linear_predictor <- if (inherits(fit, "gamlss")) {
    safe_vector(fit$mu.lp)
  } else if (inherits(fit, "glmmTMB")) {
    safe_vector(stats::predict(fit, type = "link"))
  } else if (inherits(fit, "glm")) {
    safe_vector(stats::predict(fit, type = "link"))
  } else safe_vector(stats::predict(fit))
  discrete <- .rls_discrete_diagnostic_components(
    record, fit, complete, fitted_probability = if (bounded_count) {
      fitted_probability
    } else fitted_model,
    imputation_index = imputation_index
  )
  if (!is.null(discrete)) {
    fitted_response <- discrete$expected
    pearson_residual <- discrete$pearson_residual
  }
  data.frame(
    row_id = complete$rows,
    original_row_id = complete$rows,
    observed = observed,
    # For successes-out-of-Trials responses, every diagnostic that compares
    # observed and fitted values must use one common response scale.  Keep the
    # raw probability explicitly, but make the canonical fitted diagnostic
    # value the expected count.
    fitted = if (bounded_count) fitted_response else fitted_model,
    fitted_probability = fitted_probability,
    fitted_response_scale = fitted_response,
    linear_predictor = linear_predictor,
    residual = if (is.null(discrete)) deviance_residual else
      if ("dunn_smyth" %in% discrete$capabilities$residuals)
        discrete$dunn_smyth_residual else pearson_residual,
    dunn_smyth_residual = if (is.null(discrete)) rep(NA_real_, n)
      else discrete$dunn_smyth_residual,
    diagnostic_uniform = if (is.null(discrete)) rep(NA_real_, n) else discrete$uniform,
    diagnostic_seed = if (is.null(discrete)) rep(NA_integer_, n) else discrete$diagnostic_seed,
    diagnostic_cache_key = if (is.null(discrete)) rep(NA_character_, n) else discrete$cache_key,
    diagnostic_theta = if (is.null(discrete)) rep(NA_real_, n) else discrete$theta,
    diagnostic_maximum = .rls_nb_known_maximum(record, complete$data),
    diagnostic_above_max = if (!is.null(discrete) && is.finite(.rls_nb_known_maximum(record, complete$data)))
      stats::pnbinom(.rls_nb_known_maximum(record, complete$data), mu=discrete$expected,
                    size=discrete$theta, lower.tail=FALSE) else rep(NA_real_,n),
    # Lognormal diagnostics use log(Y) throughout: the lm response residual
    # must not subtract an original-scale fitted mean from log(Y).
    raw_residual = if (identical(record$family, "lognormal"))
      safe_vector(stats::residuals(fit, type = "response")) else
      if (is.null(discrete)) observed - fitted_response else discrete$raw_residual,
    deviance_residual = deviance_residual,
    pearson_residual = pearson_residual,
    working_residual = working_residual,
    standardized_residual = standardized,
    studentized_residual = studentized,
    leverage = leverage,
    cooks_distance = cooks,
    stringsAsFactors = FALSE
  )
}

.rls_hurdle_section_row <- function(label, component) {
  row <- .rls_model_coefficient_row(
    "section_header", label, label, source_term = "", term_type = "component",
    statistic_name = "z"
  )
  row$component <- component
  row
}

.rls_hurdle_basic_ceiling_summary <- function(record, complete, perfect_probability) {
  grouped <- .rls_count_grouped_response(record, complete$data)
  ceiling <- grouped$successes == grouped$trials
  trials <- as.integer(grouped$trials)
  probability <- as.numeric(perfect_probability)
  probability <- probability[is.finite(probability)]
  predicted_count <- if (length(probability)) sum(probability) else NA_real_
  list(
    trials = if (length(unique(trials)) == 1L) unique(trials) else NA_real_,
    perfect_scores = sum(ceiling),
    non_perfect_scores = sum(!ceiling),
    perfect_score_proportion = mean(ceiling),
    predicted_perfect_score_proportion = if (length(probability)) {
      mean(probability)
    } else NA_real_,
    predicted_perfect_score_count = predicted_count,
    floor_scores = NA_real_, floor_score_proportion = NA_real_,
    predicted_floor_count = NA_real_, predicted_floor_proportion = NA_real_,
    observed_predicted_distribution = data.frame()
  )
}

.rls_hurdle_distribution_summary <- function(record, complete, diagnostics) {
  grouped <- .rls_count_grouped_response(record, complete$data)
  ceiling <- grouped$successes == grouped$trials
  trials <- as.integer(grouped$trials)
  fixed_trials <- if (length(unique(trials)) == 1L) unique(trials) else NA_real_
  distribution <- data.frame()
  if (length(trials) && all(is.finite(trials)) && all(trials >= 0L)) {
    support <- 0:max(trials)
    distribution <- data.frame(
      score = support,
      observed_frequency = vapply(
        support, function(value) sum(grouped$successes == value), numeric(1L)
      ),
      predicted_frequency = vapply(support, function(value) {
        sum(vapply(seq_len(nrow(diagnostics)), function(index) {
          current_trials <- trials[[index]]
          if (value > current_trials) return(0)
          if (value == current_trials) {
            diagnostics$perfect_score_probability[[index]]
          } else {
            mu <- diagnostics$.mu_failure[[index]]
            sigma <- diagnostics$.sigma[[index]]
            p0 <- gamlss.dist::dBB(
              0, mu = mu, sigma = sigma, bd = current_trials
            )
            (1 - diagnostics$perfect_score_probability[[index]]) *
              gamlss.dist::dBB(
                current_trials - value, mu = mu, sigma = sigma,
                bd = current_trials
              ) / (1 - p0)
          }
        }, numeric(1L)), na.rm = TRUE)
      }, numeric(1L)),
      stringsAsFactors = FALSE
    )
    distribution$observed <- distribution$observed_frequency / nrow(diagnostics)
    distribution$predicted <- distribution$predicted_frequency / nrow(diagnostics)
  }
  floor_count <- if (nrow(distribution)) distribution$observed_frequency[[1L]] else NA_real_
  predicted_floor <- if (nrow(distribution)) distribution$predicted_frequency[[1L]] else NA_real_
  predicted_ceiling <- sum(
    diagnostics$perfect_score_probability[is.finite(
      diagnostics$perfect_score_probability
    )]
  )
  result <- .rls_hurdle_basic_ceiling_summary(
    record, complete, diagnostics$perfect_score_probability
  )
  result[c(
    "trials", "perfect_scores", "non_perfect_scores",
    "perfect_score_proportion", "predicted_perfect_score_proportion",
    "predicted_perfect_score_count"
  )] <- list(
    fixed_trials, sum(ceiling), sum(!ceiling), mean(ceiling),
    mean(diagnostics$perfect_score_probability, na.rm = TRUE), predicted_ceiling
  )
  result[c(
    "floor_scores", "floor_score_proportion", "predicted_floor_count",
    "predicted_floor_proportion", "observed_predicted_distribution"
  )] <- list(
    floor_scores = floor_count,
    floor_score_proportion = floor_count / nrow(diagnostics),
    predicted_floor_count = predicted_floor,
    predicted_floor_proportion = predicted_floor / nrow(diagnostics),
    observed_predicted_distribution = distribution
  )
  result
}

.rls_beta_binomial_pmf <- function(value, trials, probability, precision) {
  if (!is.finite(value) || !is.finite(trials) || !is.finite(probability) ||
      !is.finite(precision) || trials < 0 || value < 0 || value > trials ||
      probability <= 0 || probability >= 1 || precision <= 0) {
    return(NA_real_)
  }
  gamlss.dist::dBB(value,bd=trials,mu=probability,sigma=1/precision)
}

.rls_discrete_diagnostic_distribution <- function(record) {
  if (isTRUE(record$count_regression)) {
    return(as.character(record$count_distribution %||% "poisson")[[1L]])
  }
  if (isTRUE(record$binary_regression) || identical(record$family, "binomial")) {
    return("binomial")
  }
  if (identical(record$family, "quasibinomial")) return("quasibinomial")
  if (identical(record$family, "poisson")) return("poisson")
  if (identical(record$family, "quasipoisson")) return("quasipoisson")
  ""
}

# This capability map is the statistical source of truth for discrete-model
# diagnostics.  Native code mirrors it only to construct controls; every
# numerical residual below is calculated in R by the fitted distribution.
.rls_discrete_diagnostic_capabilities <- function(record) {
  distribution <- .rls_discrete_diagnostic_distribution(record)
  result <- switch(distribution,
    binomial = c("dunn_smyth", "pearson", "deviance", "raw"),
    binomial_trials = c("dunn_smyth", "pearson", "deviance", "raw"),
    beta_binomial = c("dunn_smyth", "pearson", "raw"),
    hurdle_beta_binomial_ceiling = c("dunn_smyth", "pearson", "raw"),
    perfect_score = c("dunn_smyth", "pearson", "deviance", "raw"),
    poisson = c("dunn_smyth", "pearson", "deviance", "raw"),
    negative_binomial = c("dunn_smyth", "pearson", "deviance", "raw"),
    quasipoisson = c("pearson", "deviance", "raw"),
    quasibinomial = c("pearson", "deviance", "raw"),
    character()
  )
  list(
    distribution = distribution,
    residuals = result,
    default_residual = if ("dunn_smyth" %in% result) {
      "dunn_smyth"
    } else if (length(result)) {
      result[[1L]]
    } else {
      "deviance"
    },
    full_probability_distribution = distribution %in% c(
      "binomial", "binomial_trials", "beta_binomial",
      "hurdle_beta_binomial_ceiling", "perfect_score", "poisson",
      "negative_binomial"
    ),
    boundary_kind = if (distribution %in% c(
      "binomial_trials", "beta_binomial", "hurdle_beta_binomial_ceiling"
    )) "floor_ceiling" else if (distribution %in% c(
      "binomial", "perfect_score", "poisson", "negative_binomial"
    )) "zero" else "none"
  )
}

.rls_discrete_diagnostic_components <- function(
    record, fit, complete, fitted_probability = NULL, imputation_index = 1L) {
  capabilities <- .rls_discrete_diagnostic_capabilities(record)
  distribution <- capabilities$distribution
  if (!nzchar(distribution)) return(NULL)
  n <- nrow(complete$data)
  safe <- function(value) {
    value <- suppressWarnings(as.numeric(value))
    if (length(value) == 1L && n != 1L) value <- rep(value, n)
    if (length(value) != n) rep(NA_real_, n) else value
  }
  grouped <- if (isTRUE(record$count_regression) &&
      .rls_count_distribution_uses_trials(record$count_distribution)) {
    .rls_count_grouped_response(record, complete$data)
  } else NULL
  if (!is.null(grouped)) {
    observed <- safe(grouped$successes)
    trials <- safe(grouped$trials)
  } else {
    observed <- safe(complete$original_response %||% complete$data[[record$response]])
    trials <- rep(1, n)
  }
  probability <- safe(fitted_probability %||% tryCatch(
    stats::fitted(fit), error = function(e) rep(NA_real_, n)
  ))
  expected <- probability
  variance <- rep(NA_real_, n)
  pmf <- cdf_lower <- cdf_upper <- rep(NA_real_, n)

  if (distribution %in% c("binomial", "perfect_score")) {
    if (identical(distribution, "perfect_score") && !is.null(grouped)) {
      observed <- as.numeric(grouped$successes == grouped$trials)
    }
    expected <- probability
    variance <- probability * (1 - probability)
    pmf <- stats::dbinom(observed, 1, probability)
    cdf_lower <- stats::pbinom(observed - 1, 1, probability)
    cdf_upper <- stats::pbinom(observed, 1, probability)
  } else if (distribution == "binomial_trials") {
    expected <- trials * probability
    variance <- trials * probability * (1 - probability)
    pmf <- stats::dbinom(observed, trials, probability)
    cdf_lower <- stats::pbinom(observed - 1, trials, probability)
    cdf_upper <- stats::pbinom(observed, trials, probability)
  } else if (distribution == "beta_binomial") {
    precision <- .rls_glm_fit_dispersion(fit)
    expected <- trials * probability
    variance <- trials * probability * (1 - probability) *
      (precision + trials) / (precision + 1)
    pmf <- gamlss.dist::dBB(observed,mu=probability,sigma=1/precision,bd=trials)
    cdf_lower <- gamlss.dist::pBB(observed-1,mu=probability,sigma=1/precision,bd=trials)
    cdf_upper <- gamlss.dist::pBB(observed,mu=probability,sigma=1/precision,bd=trials)
  } else if (distribution == "hurdle_beta_binomial_ceiling") {
    mu_failure <- safe(fit$linkeda_component_mu_fv %||% fit$mu.fv)
    sigma <- safe(fit$linkeda_component_sigma_fv %||% fit$sigma.fv)
    perfect <- safe(fit$linkeda_component_nu_fv %||% fit$nu.fv)
    probability <- perfect
    pmf_one <- function(value, i) {
      current_trials <- as.integer(trials[[i]])
      if (!is.finite(value) || value < 0 || value > current_trials) return(0)
      gamlss.dist::dZABB(current_trials-value,mu=mu_failure[[i]],
                         sigma=sigma[[i]],nu=perfect[[i]],bd=current_trials)
    }
    supports <- lapply(seq_len(n), function(i) 0:as.integer(trials[[i]]))
    probabilities <- lapply(seq_len(n), function(i) vapply(
      supports[[i]], pmf_one, numeric(1L), i = i
    ))
    expected <- vapply(seq_len(n), function(i) sum(
      supports[[i]] * probabilities[[i]], na.rm = TRUE
    ), numeric(1L))
    variance <- vapply(seq_len(n), function(i) sum(
      (supports[[i]] - expected[[i]])^2 * probabilities[[i]], na.rm = TRUE
    ), numeric(1L))
    pmf <- vapply(seq_len(n), function(i) pmf_one(observed[[i]], i), numeric(1L))
    cdf_lower <- 1-gamlss.dist::pZABB(trials-observed,mu=mu_failure,sigma=sigma,nu=perfect,bd=trials)
    cdf_upper <- 1-gamlss.dist::pZABB(trials-observed-1,mu=mu_failure,sigma=sigma,nu=perfect,bd=trials)
  } else if (distribution %in% c("poisson", "quasipoisson")) {
    expected <- probability
    dispersion <- if (distribution == "quasipoisson") {
      suppressWarnings(as.numeric(summary(fit)$dispersion))
    } else 1
    variance <- dispersion * expected
    if (distribution == "poisson") {
      pmf <- stats::dpois(observed, expected)
      cdf_lower <- stats::ppois(observed - 1, expected)
      cdf_upper <- stats::ppois(observed, expected)
    }
  } else if (distribution == "negative_binomial") {
    expected <- probability
    theta <- suppressWarnings(as.numeric(fit$theta %||% NA_real_))
    variance <- expected + expected^2 / theta
    pmf <- stats::dnbinom(observed, mu = expected, size = theta)
    cdf_lower <- stats::pnbinom(observed - 1, mu = expected, size = theta)
    cdf_upper <- stats::pnbinom(observed, mu = expected, size = theta)
  } else if (distribution == "quasibinomial") {
    expected <- probability
    dispersion <- suppressWarnings(as.numeric(summary(fit)$dispersion))
    variance <- dispersion * probability * (1 - probability)
  }

  raw <- observed - expected
  pearson <- ifelse(is.finite(variance) & variance > 0,
                    raw / sqrt(variance), NA_real_)
  uniform <- .rls_stable_diagnostic_uniform(record, complete$rows, imputation_index, fit)
  dunn_smyth <- rep(NA_real_, n)
  library_glm <- inherits(fit,"glm") && distribution %in% c("negative_binomial","poisson","binomial","binomial_trials","perfect_score")
  if (isTRUE(capabilities$full_probability_distribution) && !library_glm) {
    if (distribution == "beta_binomial") {
      library_quantile <- .rls_beta_binomial_library_quantile(
        observed, trials, probability, rep(precision, length.out = n),
        uniform, cdf_lower, cdf_upper
      )
      dunn_smyth <- library_quantile$residual
      cdf_lower <- library_quantile$lower
      cdf_upper <- library_quantile$upper
    } else {
      dunn_smyth <- .rls_quantile_from_library_cdf(cdf_lower,cdf_upper,uniform)
    }
  }
  cache_key <- .rls_diagnostic_cache_key(record,fit,complete$rows,imputation_index)
  if (library_glm) {
    if (distribution == "negative_binomial" &&
        !isTRUE(all.equal(as.numeric(fit$y),observed,check.attributes=FALSE)))
      stop("NB diagnostic responses are not aligned with the fitted rows.",call.=FALSE)
    library_result <- .rls_statmod_diagnostic(record,fit,complete$rows,imputation_index)
    dunn_smyth <- library_result$residual
    uniform <- library_result$uniform
    attr(uniform,"diagnostic_seed") <- library_result$seed
    cache_key <- library_result$cache_key
  }
  list(
    cache_key = cache_key,
    uniform = as.numeric(uniform), diagnostic_seed = attr(uniform, "diagnostic_seed"),
    theta = if (distribution == "negative_binomial") theta else NA_real_,
    capabilities = capabilities, observed = observed, expected = expected,
    variance = variance, probability = probability, trials = trials,
    pmf_observed = pmf, cdf_lower = cdf_lower, cdf_upper = cdf_upper,
    raw_residual = raw, pearson_residual = pearson,
    dunn_smyth_residual = dunn_smyth
  )
}

.rls_bounded_count_distribution_summary <- function(
    record, fit, complete, diagnostics) {
  if (!isTRUE(record$count_regression) ||
      !.rls_count_distribution_uses_trials(record$count_distribution) ||
      .rls_count_is_perfect_score(record) ||
      (!nzchar(record$trials_variable %||% "") &&
       !is.finite(record$trials_constant %||% NA_real_))) {
    return(list(observed_predicted_distribution = data.frame()))
  }
  if (.rls_count_is_ceiling_hurdle(record)) {
    distribution_input <- diagnostics
    distribution_input$.mu_failure <- as.numeric(
      fit$linkeda_component_mu_fv %||% fit$mu.fv
    )
    distribution_input$.sigma <- as.numeric(
      fit$linkeda_component_sigma_fv %||% fit$sigma.fv
    )
    return(.rls_hurdle_distribution_summary(record, complete, distribution_input))
  }

  grouped <- .rls_count_grouped_response(record, complete$data)
  trials <- as.integer(grouped$trials)
  if (!length(trials) || any(!is.finite(trials)) || any(trials < 0L)) {
    return(list(observed_predicted_distribution = data.frame()))
  }
  support <- 0:max(trials)
  fitted_probability <- as.numeric(diagnostics$fitted_probability)
  distribution <- record$count_distribution %||% ""
  precision <- if (identical(distribution, "beta_binomial")) {
    .rls_glm_fit_dispersion(fit)
  } else NA_real_
  predicted_frequency <- vapply(support, function(value) {
    probabilities <- if (identical(distribution, "binomial_trials")) {
      stats::dbinom(value, size = trials, prob = fitted_probability)
    } else if (identical(distribution, "beta_binomial")) {
      vapply(seq_along(fitted_probability), function(index) {
        .rls_beta_binomial_pmf(
          value, trials[[index]], fitted_probability[[index]], precision
        )
      }, numeric(1L))
    } else {
      rep(NA_real_, length(fitted_probability))
    }
    if (!any(is.finite(probabilities))) NA_real_
    else sum(probabilities[is.finite(probabilities)])
  }, numeric(1L))
  observed_frequency <- vapply(
    support, function(value) sum(grouped$successes == value), numeric(1L)
  )
  sample_size <- length(grouped$successes)
  ceiling_probability <- if (identical(distribution, "binomial_trials")) {
    stats::dbinom(trials, size = trials, prob = fitted_probability)
  } else if (identical(distribution, "beta_binomial")) {
    vapply(seq_along(fitted_probability), function(index) {
      .rls_beta_binomial_pmf(
        trials[[index]], trials[[index]], fitted_probability[[index]], precision
      )
    }, numeric(1L))
  } else rep(NA_real_, sample_size)
  predicted_ceiling_count <- if (any(is.finite(ceiling_probability))) {
    sum(ceiling_probability[is.finite(ceiling_probability)])
  } else NA_real_
  result <- data.frame(
    score = support,
    observed_frequency = observed_frequency,
    predicted_frequency = predicted_frequency,
    observed = observed_frequency / sample_size,
    predicted = predicted_frequency / sample_size,
    stringsAsFactors = FALSE
  )
  list(
    floor_scores = observed_frequency[[1L]],
    floor_score_proportion = observed_frequency[[1L]] / sample_size,
    predicted_floor_count = predicted_frequency[[1L]],
    predicted_floor_proportion = predicted_frequency[[1L]] / sample_size,
    predicted_perfect_score_count = predicted_ceiling_count,
    predicted_perfect_score_proportion = predicted_ceiling_count / sample_size,
    observed_predicted_distribution = result
  )
}

.rls_unbounded_discrete_distribution_summary <- function(
    record, fit, complete, diagnostics) {
  distribution <- .rls_discrete_diagnostic_distribution(record)
  if (!distribution %in% c(
    "binomial", "perfect_score", "poisson", "negative_binomial"
  )) return(list(observed_predicted_distribution = data.frame()))
  components <- .rls_discrete_diagnostic_components(
    record, fit, complete,
    fitted_probability = if ("fitted_probability" %in% names(diagnostics) &&
      any(is.finite(diagnostics$fitted_probability))) {
      diagnostics$fitted_probability
    } else diagnostics$fitted
  )
  if (is.null(components)) {
    return(list(observed_predicted_distribution = data.frame()))
  }
  observed <- as.integer(round(components$observed))
  mu <- as.numeric(components$expected)
  if (distribution %in% c("binomial", "perfect_score")) {
    support <- 0:1
    probability_at <- function(value) stats::dbinom(
      value, size = 1, prob = components$probability
    )
  } else if (distribution == "poisson") {
    quantiles <- stats::qpois(.9995, lambda = mu)
    upper <- max(c(observed, quantiles[is.finite(quantiles)]), na.rm = TRUE)
    upper <- max(1L, min(as.integer(ceiling(upper)), 5000L))
    support <- 0:upper
    probability_at <- function(value) {
      if (value == upper) stats::ppois(upper - 1, mu, lower.tail = FALSE)
      else stats::dpois(value, mu)
    }
  } else {
    theta <- as.numeric(fit$theta %||% NA_real_)
    quantiles <- stats::qnbinom(.9995, mu = mu, size = theta)
    upper <- max(c(observed, quantiles[is.finite(quantiles)]), na.rm = TRUE)
    upper <- max(1L, min(as.integer(ceiling(upper)), 5000L))
    support <- 0:upper
    probability_at <- function(value) {
      if (value == upper) {
        stats::pnbinom(upper - 1, mu = mu, size = theta, lower.tail = FALSE)
      } else stats::dnbinom(value, mu = mu, size = theta)
    }
  }
  last_is_tail <- distribution %in% c("poisson", "negative_binomial")
  observed_frequency <- vapply(support, function(value) {
    if (last_is_tail && value == tail(support, 1L)) sum(observed >= value)
    else sum(observed == value)
  }, numeric(1L))
  predicted_frequency <- vapply(support, function(value) {
    probability <- probability_at(value)
    if (!any(is.finite(probability))) NA_real_ else sum(probability, na.rm = TRUE)
  }, numeric(1L))
  sample_size <- length(observed)
  result <- data.frame(
    score = support,
    observed_frequency = observed_frequency,
    predicted_frequency = predicted_frequency,
    observed = observed_frequency / sample_size,
    predicted = predicted_frequency / sample_size,
    tail_aggregated = last_is_tail & support == tail(support, 1L),
    stringsAsFactors = FALSE
  )
  list(
    floor_scores = result$observed_frequency[[1L]],
    floor_score_proportion = result$observed[[1L]],
    predicted_floor_count = result$predicted_frequency[[1L]],
    predicted_floor_proportion = result$predicted[[1L]],
    observed_predicted_distribution = result
  )
}

.rls_discrete_distribution_summary <- function(record, fit, complete, diagnostics) {
  capabilities <- .rls_discrete_diagnostic_capabilities(record)
  if (!isTRUE(capabilities$full_probability_distribution)) {
    return(list(
      observed_predicted_distribution = data.frame(),
      diagnostic_capabilities = capabilities
    ))
  }
  result <- if (isTRUE(record$count_regression) &&
      .rls_count_distribution_uses_trials(record$count_distribution) &&
      !.rls_count_is_perfect_score(record)) {
    .rls_bounded_count_distribution_summary(record, fit, complete, diagnostics)
  } else {
    .rls_unbounded_discrete_distribution_summary(record, fit, complete, diagnostics)
  }
  result$diagnostic_capabilities <- capabilities
  result
}

.rls_discrete_diagnostic_warnings <- function(summary, n) {
  n <- suppressWarnings(as.numeric(n))
  if (!is.finite(n) || n <= 0) return(character())
  substantial_underprediction <- function(observed, predicted) {
    if (!is.finite(observed) || !is.finite(predicted)) return(FALSE)
    difference <- observed - predicted
    difference >= max(.05, 2 * sqrt(max(predicted * (1 - predicted), 0) / n))
  }
  warnings <- character()
  distribution <- summary$count_distribution %||% summary$family %||% ""
  if (distribution %in% c(
      "binomial_trials", "beta_binomial", "hurdle_beta_binomial_ceiling"
    ) && substantial_underprediction(
      summary$perfect_score_proportion %||% NA_real_,
      summary$predicted_perfect_score_proportion %||% NA_real_
    )) {
    warnings <- c(warnings, paste(
      "The fitted model underpredicts the number of observations at the maximum score.",
      "Consider a ceiling-hurdle model."
    ))
  }
  if (distribution %in% c("poisson", "negative_binomial") &&
      substantial_underprediction(
        summary$floor_score_proportion %||% NA_real_,
        summary$predicted_floor_proportion %||% NA_real_
      )) {
    warnings <- c(warnings, paste(
      sprintf("The fitted %s model underpredicts zero counts.",
              if (distribution == "poisson") "Poisson" else "negative-binomial"),
      "Consider an appropriate hurdle or zero-inflated alternative."
    ))
  }
  warnings
}

.rls_extract_ceiling_hurdle_fit <- function(record, fit, complete) {
  no_perfect_scores <- identical(
    fit$linkeda_perfect_score_boundary %||% "", "no_perfect_scores"
  )
  components <- c("perfect_score", "below_ceiling")
  labels <- c(
    perfect_score = "Perfect score \u2014 logistic component",
    below_ceiling = "Score below ceiling \u2014 truncated beta-binomial component"
  )
  # Validate both required parts before deriving any table or diagnostic.  This
  # guarantees that a failed current fit cannot leave a plausible-looking
  # partial result behind.
  component_status <- lapply(components, function(component) {
    .rls_validate_hurdle_component(fit, component)
  })
  names(component_status) <- components
  if (!all(vapply(component_status, `[[`, logical(1L), "ok"))) {
    stop(paste(vapply(component_status, `[[`, character(1L), "status"), collapse = " "), call. = FALSE)
  }
  coefficient_tables <- lapply(components, function(component) {
    table <- if (identical(component, "perfect_score") && no_perfect_scores) {
      .rls_empty_hurdle_coefficient_table()
    } else .rls_hurdle_component_coefficient_table(fit, component)
    table$component <- rep(component, nrow(table))
    .rls_glm_augment_exponentiated(table, record)
  })
  names(coefficient_tables) <- components
  term_tests <- lapply(components, function(component) {
    tests <- if (identical(component, "perfect_score") && no_perfect_scores) {
      .rls_empty_global_term_tests()
    } else .rls_hurdle_component_term_tests(fit, component, record$terms)
    tests$component <- rep(component, nrow(tests))
    tests
  })
  names(term_tests) <- components
  rows <- lapply(components, function(component) {
    component_rows <- if (!nrow(coefficient_tables[[component]])) NULL else
      .rls_model_coefficient_display_rows(
        fit, coefficient_tables[[component]], record$data, record$terms,
        statistic_name = "z"
      )
    if (is.null(component_rows)) {
      section_label <- paste0(
        labels[[component]], " (not estimable: no perfect scores)"
      )
      return(.rls_glm_augment_exponentiated(
        .rls_hurdle_section_row(section_label, component), record
      ))
    }
    component_rows <- .rls_model_apply_parent_term_tests(
      component_rows, term_tests[[component]]
    )
    component_rows <- .rls_glm_augment_exponentiated(component_rows, record)
    component_rows$component <- component
    section <- .rls_glm_augment_exponentiated(
      .rls_hurdle_section_row(labels[[component]], component), record
    )
    rbind(section, component_rows)
  })
  coefficient_rows <- do.call(rbind, rows)
  rownames(coefficient_rows) <- NULL
  coefficients <- do.call(rbind, coefficient_tables)
  rownames(coefficients) <- NULL
  diagnostic_warning <- character()
  diagnostics <- tryCatch(
    .rls_generalized_glm_diagnostics(record, fit, complete),
    error = function(error) {
      diagnostic_warning <<- paste(
        "This diagnostic could not be computed. The fitted model remains valid.",
        conditionMessage(error)
      )
      data.frame()
    }
  )
  if (nrow(diagnostics)) {
    diagnostics$.mu_failure <- as.numeric(
      fit$linkeda_component_mu_fv %||% fit$mu.fv
    )
    diagnostics$.sigma <- as.numeric(
      fit$linkeda_component_sigma_fv %||% fit$sigma.fv
    )
    distribution_summary <- tryCatch(
      .rls_discrete_distribution_summary(record, fit, complete, diagnostics),
      error = function(error) {
        diagnostic_warning <<- unique(c(diagnostic_warning, paste(
          "This diagnostic could not be computed. The fitted model remains valid.",
          conditionMessage(error)
        )))
        .rls_hurdle_basic_ceiling_summary(
          record, complete, fit$linkeda_component_nu_fv %||% fit$nu.fv
        )
      }
    )
    diagnostics$.mu_failure <- diagnostics$.sigma <- NULL
  } else {
    distribution_summary <- .rls_hurdle_basic_ceiling_summary(
      record, complete, fit$linkeda_component_nu_fv %||% fit$nu.fv
    )
  }
  dispersion <- .rls_glm_fit_dispersion(fit)
  aic <- tryCatch(stats::AIC(fit), error = function(e) NA_real_)
  bic <- tryCatch(stats::BIC(fit), error = function(e) NA_real_)
  log_lik <- tryCatch(as.numeric(stats::logLik(fit)), error = function(e) NA_real_)
  all_tests <- do.call(rbind, term_tests)
  rownames(all_tests) <- NULL
  .rls_assert_hurdle_one_df_consistency(coefficient_rows, all_tests)
  list(
    fit = fit, fitted_glm = fit, coefficients = coefficients,
    coefficient_rows = coefficient_rows,
    component_coefficients = coefficient_tables,
    component_term_tests = term_tests,
    parent_term_tests = all_tests, term_tests = all_tests,
    summary = c(list(
      n_used = length(complete$rows), n_excluded = complete$excluded,
      null_deviance = NA_real_, residual_deviance = .rls_glm_fit_residual_deviance(fit),
      df_residual = stats::df.residual(fit), aic = aic, bic = bic,
      dispersion = dispersion, beta_binomial_dispersion = dispersion,
      log_lik = log_lik, family = record$family, link = record$link,
      count_distribution = record$count_distribution,
      fitter = if (isTRUE(fit$linkeda_hurdle_component_fallback)) {
        paste(
          "Exact factorized hurdle likelihood: stats::glm plus",
          "gamlss::gamlss with gamlss.dist::ZABB"
        )
      } else "gamlss::gamlss with gamlss.dist::ZABB",
      statistic_name = "z", likelihood_available = TRUE,
      likelihood_ratio_available = TRUE, converged = isTRUE(fit$converged),
      iterations = .rls_glm_fit_iterations(fit), boundary = FALSE,
      rank = .rls_glm_fit_rank(fit), parameter_count = .rls_glm_fit_parameter_count(fit),
      rank_deficient = FALSE,
      warnings = diagnostic_warning,
      fit_information_method = if (isTRUE(fit$linkeda_hurdle_component_fallback)) {
        paste(
          "Exact ceiling-hurdle model fitted through its factorized likelihood:",
          "stats::glm for the perfect-score logistic component and gamlss.dist::ZABB",
          "with fixed zero-adjustment on below-ceiling observations for the",
          "upper-truncated beta-binomial component."
        )
      } else paste(
        "Exact ceiling-hurdle model: logistic perfect-score component and",
        "upper-truncated beta-binomial component fitted jointly with gamlss.dist::ZABB."
      ),
      perfect_score_component_status = component_status$perfect_score$status,
      below_ceiling_component_status = component_status$below_ceiling$status,
      hurdle_successful_imputations = 1L,
      hurdle_attempted_imputations = 1L,
      observed_ceiling_count_constant = TRUE
    ), distribution_summary),
    rows_used = complete$rows,
    rows_excluded = setdiff(seq_len(nrow(record$data)), complete$rows),
    diagnostics = diagnostics, diagnostic_data = diagnostics,
    warnings = diagnostic_warning,
    status = sprintf(
      "Ceiling-hurdle beta-binomial fitted successfully; %d rows used, %d excluded.",
      length(complete$rows), complete$excluded
    )
  )
}

.rls_generalized_glm_extract_fit <- function(record, fit, complete) {
  if (.rls_count_is_ceiling_hurdle(record)) {
    return(.rls_extract_ceiling_hurdle_fit(record, fit, complete))
  }
  summary_fit <- if (inherits(fit, "gamlss")) NULL else summary(fit)
  raw_coefficients <- if (inherits(fit, "glmmTMB")) {
    summary_fit$coefficients$cond
  } else if (inherits(fit, "betareg")) {
    summary_fit$coefficients$mean
  } else if (inherits(fit, "gamlss")) {
    estimate <- unname(fit$mu.coefficients)
    names(estimate) <- names(fit$mu.coefficients)
    covariance <- tryCatch(stats::vcov(fit), error = function(e) NULL)
    std_error <- if (is.matrix(covariance) && nrow(covariance) >= length(estimate)) {
      sqrt(diag(covariance)[seq_along(estimate)])
    } else rep(NA_real_, length(estimate))
    statistic <- estimate / std_error
    p_value <- 2 * stats::pt(abs(statistic), df = stats::df.residual(fit), lower.tail = FALSE)
    result <- cbind(estimate, std_error, statistic, p_value)
    rownames(result) <- names(fit$mu.coefficients)
    result
  } else summary_fit$coefficients
  coef_matrix <- as.data.frame(unclass(raw_coefficients), stringsAsFactors = FALSE)
  if (ncol(coef_matrix) < 4L) {
    stop("Could not extract generalized linear model coefficients.", call. = FALSE)
  }
  statistic_label <- if (inherits(fit, "gamlss")) "t" else if (grepl("^z", names(coef_matrix)[[3L]], ignore.case = TRUE)) "z" else "t"
  names(coef_matrix)[1:4] <- c("estimate", "std_error", "statistic", "p_value")
  # summary.glm reports t tests when dispersion is estimated (for example
  # Gamma and quasi-Poisson). Match the interval to the reported test and its
  # residual degrees of freedom rather than always using a normal critical.
  critical <- if (identical(statistic_label, "t")) {
    stats::qt(0.975, df = stats::df.residual(fit))
  } else stats::qnorm(0.975)
  coef_matrix$ci_lower <- coef_matrix$estimate - critical * coef_matrix$std_error
  coef_matrix$ci_upper <- coef_matrix$estimate + critical * coef_matrix$std_error
  coef_matrix$term <- rownames(raw_coefficients)
  coef_matrix$partial_r <- NA_real_
  coef_matrix$delta_r2 <- NA_real_
  coef_matrix <- coef_matrix[, c(
    "term", "estimate", "std_error", "statistic", "p_value",
    "ci_lower", "ci_upper", "partial_r", "delta_r2"
  )]
  rownames(coef_matrix) <- NULL
  aic <- tryCatch(stats::AIC(fit), error = function(e) NA_real_)
  bic <- tryCatch(stats::BIC(fit), error = function(e) NA_real_)
  log_lik <- tryCatch(as.numeric(stats::logLik(fit)), error = function(e) NA_real_)
  if (identical(record$family, "lognormal")) {
    # lm(log(Y)) reports a density for log(Y). Preserve logLik's df/nobs
    # attributes, but include the transformation Jacobian for the density of Y.
    likelihood <- stats::logLik(fit) - sum(stats::model.response(stats::model.frame(fit)))
    aic <- stats::AIC(likelihood)
    bic <- stats::BIC(likelihood)
    log_lik <- as.numeric(likelihood)
  }
  parameter_count <- .rls_glm_fit_parameter_count(fit)
  design <- tryCatch(stats::model.matrix(fit), error = function(e) NULL)
  fit_rank <- as.integer(fit$rank %||% if (is.matrix(design)) qr(design)$rank else NA_integer_)
  converged <- if (inherits(fit, "glmmTMB")) {
    isTRUE(fit$sdr$pdHess) && identical(as.integer(fit$fit$convergence %||% 1L), 0L)
  } else if (inherits(fit, "lm") && !inherits(fit, "glm")) TRUE else isTRUE(fit$converged)
  coefficient_rows <- .rls_model_coefficient_display_rows(
    fit, coef_matrix, record$data, record$terms, statistic_name = statistic_label
  )
  likelihood_available <- !identical(record$count_distribution, "quasipoisson") &&
    !record$family %in% c("quasipoisson", "quasibinomial")
  global_term_tests <- tryCatch(
    .rls_model_global_term_tests(
      fit,
      attr(stats::terms(fit), "term.labels") %||% character(),
      likelihood_available = likelihood_available,
      model_data = complete$data
    ),
    error = function(e) .rls_empty_global_term_tests()
  )
  parent_test_terms <- tryCatch(
    .rls_model_parent_test_terms(fit, record$data), error = function(e) character()
  )
  parent_term_tests <- global_term_tests[vapply(global_term_tests$term, function(term) {
    any(vapply(parent_test_terms, .rls_model_terms_equivalent, logical(1L),
               b = term))
  }, logical(1L)), , drop = FALSE]
  coefficient_rows <- .rls_model_apply_parent_term_tests(
    coefficient_rows, parent_term_tests
  )
  coef_matrix <- .rls_glm_augment_exponentiated(coef_matrix, record)
  coefficient_rows <- .rls_glm_augment_exponentiated(coefficient_rows, record)
  if (!likelihood_available) aic <- bic <- log_lik <- NA_real_
  theta <- if (isTRUE(record$count_regression) &&
               identical(record$count_distribution, "negative_binomial")) {
    unname(fit$theta %||% NA_real_)
  } else NA_real_
  null_deviance <- unname(fit$null.deviance %||% NA_real_)
  residual_deviance <- .rls_glm_fit_residual_deviance(fit)
  dispersion <- if (inherits(fit, "glmmTMB")) {
    unname(stats::sigma(fit))
  } else if (inherits(fit, "betareg")) {
    mean(as.numeric(stats::predict(fit, type = "precision")), na.rm = TRUE)
  } else if (inherits(fit, "gamlss")) {
    .rls_glm_fit_dispersion(fit)
  } else if (isTRUE(record$count_regression) &&
             identical(record$count_distribution, "negative_binomial")) {
    NA_real_
  } else if (inherits(fit, "lm") && !inherits(fit, "glm")) {
    unname(summary_fit$sigma^2)
  } else unname(summary_fit$dispersion)
  if (!is.finite(dispersion)) dispersion <- NA_real_
  binomial_overdispersion <- if (isTRUE(record$count_regression) &&
      identical(record$count_distribution, "binomial_trials")) {
    .rls_glm_family_diagnostics(record, fit, complete)$pearson_dispersion_ratio %||% NA_real_
  } else NA_real_
  inflation_probability <- if (inherits(fit, "gamlss")) {
    mean(as.numeric(
      fit$linkeda_component_nu_fv %||% fit$nu.fv %||% NA_real_
    ), na.rm = TRUE)
  } else NA_real_
  if (!is.finite(inflation_probability)) inflation_probability <- NA_real_
  iterations <- if (inherits(fit, "glmmTMB")) {
    as.integer(fit$fit$iterations %||% NA_integer_)
  } else if (inherits(fit, "betareg")) {
    as.integer(fit$optim$counts[["function"]] %||% NA_integer_)
  } else if (inherits(fit, "lm") && !inherits(fit, "glm")) NA_integer_
  else as.integer(fit$iter %||% NA_integer_)
  diagnostics <- .rls_generalized_glm_diagnostics(record, fit, complete)
  ceiling_summary <- if (isTRUE(record$count_regression) &&
      .rls_count_distribution_uses_trials(record$count_distribution)) {
    grouped <- .rls_count_grouped_response(record, complete$data)
    perfect <- grouped$successes == grouped$trials
    list(
      trials = if (nzchar(record$trials_variable %||% "")) NA_real_
        else record$trials_constant,
      perfect_scores = sum(perfect), non_perfect_scores = sum(!perfect),
      perfect_score_proportion = mean(perfect),
      predicted_perfect_score_proportion = if (
        "perfect_score_probability" %in% names(diagnostics)
      ) mean(diagnostics$perfect_score_probability, na.rm = TRUE) else NA_real_
    )
  } else list()
  bounded_distribution <- .rls_discrete_distribution_summary(
    record, fit, complete, diagnostics
  )
  for (name in names(bounded_distribution)) {
    ceiling_summary[[name]] <- bounded_distribution[[name]]
  }
  list(
    fit = fit,
    fitted_glm = fit,
    coefficients = coef_matrix,
    coefficient_rows = coefficient_rows,
    parent_term_tests = parent_term_tests,
    term_tests = global_term_tests,
    summary = c(list(
      n_used = length(complete$rows),
      n_excluded = complete$excluded,
      null_deviance = null_deviance,
      residual_deviance = residual_deviance,
      df_residual = stats::df.residual(fit),
      aic = aic,
      bic = bic,
      dispersion = dispersion,
      binomial_overdispersion_ratio = binomial_overdispersion,
      family_diagnostics = .rls_glm_family_diagnostics(record, fit, complete),
      multiple_imputation = FALSE,
      beta_binomial_precision = if (identical(record$count_distribution %||% "", "beta_binomial")) dispersion else NA_real_,
      log_lik = log_lik,
      family = record$family,
      link = record$link,
      count_distribution = record$count_distribution %||% "poisson",
      fitter = if (identical(record$count_distribution %||% "", "beta_binomial")) {
        "glmmTMB::glmmTMB"
      } else if (identical(record$count_distribution %||% "", "negative_binomial")) {
        "MASS::glm.nb"
      } else if (identical(record$family, "beta")) {
        "betareg::betareg"
      } else if (identical(record$family, "beta_one_inflated")) {
        "gamlss::gamlss with gamlss.dist::BEOI"
      } else if (identical(record$family, "lognormal")) {
        "stats::lm"
      } else {
        "stats::glm"
      },
      response_bounds = record$response_bounds,
      response_transformation = record$response_transformation,
      inflation_probability = inflation_probability,
      statistic_name = statistic_label,
      likelihood_available = likelihood_available,
      likelihood_ratio_available = likelihood_available,
      theta = theta,
      theta_descriptive_mean = theta,
      theta_descriptive_min = theta,
      theta_descriptive_max = theta,
      converged = converged,
      iterations = iterations,
      boundary = isTRUE(fit$boundary),
      rank = fit_rank,
      parameter_count = parameter_count,
      rank_deficient = is.finite(fit_rank) && is.matrix(design) && fit_rank < ncol(design)
    ), ceiling_summary),
    rows_used = complete$rows,
    rows_excluded = setdiff(seq_len(nrow(record$data)), complete$rows),
    diagnostics = diagnostics,
    status = sprintf(
      if (identical(record$family, "beta")) "Beta regression fitted successfully; %d rows used, %d excluded."
      else if (identical(record$family, "beta_one_inflated")) "One-inflated beta regression fitted successfully; %d rows used, %d excluded."
      else if (identical(record$family, "gamma_distance")) "Gamma distance-to-maximum regression fitted successfully; %d rows used, %d excluded."
      else if (identical(record$family, "gaussian_log")) "Gaussian regression with log link fitted successfully; %d rows used, %d excluded."
      else if (identical(record$family, "lognormal")) "Lognormal regression fitted successfully; diagnostics use residuals on the log-response scale; %d rows used, %d excluded."
      else if (isTRUE(record$count_regression) && converged) "Count regression fitted successfully; %d rows used, %d excluded."
      else if (isTRUE(record$count_regression)) "Count regression returned a fit but did not converge; %d rows used, %d excluded."
      else if (converged) "GLM fitted successfully; %d rows used, %d excluded."
      else "GLM returned a fit but did not converge; %d rows used, %d excluded.",
      length(complete$rows), complete$excluded
    )
  )
}

.rls_count_parameter_label <- function(distribution, multiple = FALSE) {
  label <- switch(distribution,
    beta_binomial = "Beta-binomial precision (phi)",
    negative_binomial = "Theta",
    hurdle_beta_binomial_ceiling = "Beta-binomial dispersion (sigma)",
    binomial_trials = "Pearson dispersion ratio",
    quasi_poisson = "Quasi-Poisson dispersion (phi)",
    poisson = "Poisson dispersion (fixed)",
    perfect_score = "Binomial dispersion (fixed)",
    "Dispersion")
  if (multiple && !distribution %in% c("poisson", "perfect_score"))
    paste0(label, " \u2014 mean across imputations (not Rubin-pooled)") else label
}

.rls_generalized_glm_fit_rows_base <- function(summary) {
  if (identical(summary$count_distribution %||% "", "hurdle_beta_binomial_ceiling")) {
    dispersion_values <- summary$beta_binomial_dispersion_by_imputation %||% numeric()
    dispersion_values <- dispersion_values[is.finite(dispersion_values)]
    multiple <- length(dispersion_values) > 1L
    dispersion <- if (multiple) mean(dispersion_values) else
      summary$beta_binomial_dispersion %||% summary$dispersion %||% NA_real_
    count_text <- function(value) {
      if (!is.finite(value)) return(.rls_em_dash)
      if (abs(value - round(value)) < 1e-8) as.character(as.integer(round(value)))
      else .rls_export_format_number(value, 1L)
    }
    return(data.frame(
      label = c(
        "N", "Trials",
        if (multiple && !isTRUE(summary$observed_ceiling_count_constant))
          "Mean observed perfect scores across imputations" else "Perfect scores",
        if (multiple && !isTRUE(summary$observed_ceiling_count_constant))
          "Mean observed non-perfect scores across imputations" else "Non-perfect scores",
        "Observed proportion at ceiling", "Predicted proportion at ceiling",
        if (multiple) "Mean predicted count at ceiling across imputations"
          else "Predicted count at ceiling",
        "df residual", "AIC", "BIC",
        .rls_count_parameter_label("hurdle_beta_binomial_ceiling", multiple),
        if (multiple) "Beta-binomial dispersion (sigma) range across imputations"
          else character(),
        "logLik"
      ),
      value = c(
        count_text(summary$n_used %||% NA_real_),
        count_text(summary$trials %||% NA_real_),
        count_text(summary$perfect_scores %||% NA_real_),
        count_text(summary$non_perfect_scores %||% NA_real_),
        .rls_export_format_percent(summary$perfect_score_proportion %||% NA_real_),
        .rls_export_format_percent(summary$predicted_perfect_score_proportion %||% NA_real_),
        count_text(summary$predicted_perfect_score_count %||% NA_real_),
        count_text(summary$df_residual %||% NA_real_),
        .rls_export_format_number(summary$aic %||% NA_real_, 1L),
        .rls_export_format_number(summary$bic %||% NA_real_, 1L),
        .rls_export_format_number(dispersion, 3L),
        if (multiple) paste0(
          .rls_export_format_number(min(dispersion_values), 3L), "\u2013",
          .rls_export_format_number(max(dispersion_values), 3L)
        ) else character(),
        .rls_export_format_number(summary$log_lik %||% NA_real_, 3L)
      ), stringsAsFactors = FALSE
    ))
  }
  if (identical(summary$count_distribution %||% "", "perfect_score")) {
    return(data.frame(
      label = c(
        "N", "Trials", "Perfect scores", "Non-perfect scores",
        "Observed proportion at ceiling", "Null deviance", "Residual deviance",
        "df residual", "AIC", "BIC", "logLik"
      ),
      value = c(
        as.character(summary$n_used %||% NA_integer_),
        .rls_export_format_number(summary$trials %||% NA_real_, 0L),
        .rls_export_format_number(summary$perfect_scores %||% NA_real_, 0L),
        .rls_export_format_number(summary$non_perfect_scores %||% NA_real_, 0L),
        .rls_export_format_percent(summary$perfect_score_proportion %||% NA_real_),
        .rls_export_format_number(summary$null_deviance %||% NA_real_, 3L),
        .rls_export_format_number(summary$residual_deviance %||% NA_real_, 3L),
        as.character(summary$df_residual %||% NA_integer_),
        .rls_export_format_number(summary$aic %||% NA_real_, 1L),
        .rls_export_format_number(summary$bic %||% NA_real_, 1L),
        .rls_export_format_number(summary$log_lik %||% NA_real_, 3L)
      ), stringsAsFactors = FALSE
    ))
  }
  negative_binomial <- identical(
    summary$count_distribution %||% "", "negative_binomial"
  )
  if (negative_binomial) {
    theta <- summary$theta %||% NA_real_
    theta_mean <- summary$theta_descriptive_mean %||% theta
    theta_min <- summary$theta_descriptive_min %||% theta
    theta_max <- summary$theta_descriptive_max %||% theta
    multiple <- !is.finite(theta) && is.finite(theta_mean)
    labels <- c(
      "N", "Null deviance", "Residual deviance", "df residual",
      "AIC", "BIC",
      .rls_count_parameter_label("negative_binomial", multiple),
      if (multiple) "Theta range across imputations" else character(),
      "logLik"
    )
    values <- c(
      as.character(summary$n_used %||% NA_integer_),
      .rls_export_format_number(summary$null_deviance %||% NA_real_, 3L),
      .rls_export_format_number(summary$residual_deviance %||% NA_real_, 3L),
      as.character(summary$df_residual %||% NA_integer_),
      .rls_export_format_number(summary$aic %||% NA_real_, 1L),
      .rls_export_format_number(summary$bic %||% NA_real_, 1L),
      .rls_export_format_number(theta_mean, 3L),
      if (multiple) paste0(
        .rls_export_format_number(theta_min, 3L), "\u2013",
        .rls_export_format_number(theta_max, 3L)
      ) else character(),
      .rls_export_format_number(summary$log_lik %||% NA_real_, 3L)
    )
    return(data.frame(label = labels, value = values, stringsAsFactors = FALSE))
  }
  if (identical(summary$count_distribution %||% "", "binomial_trials")) {
    return(data.frame(
      label = c("N", "Null deviance", "Residual deviance", "df residual",
                "AIC", "BIC", .rls_count_parameter_label("binomial_trials",
                  length(summary$binomial_overdispersion_by_imputation) > 1L), "logLik"),
      value = c(
        as.character(summary$n_used %||% NA_integer_),
        .rls_export_format_number(summary$null_deviance %||% NA_real_, 3L),
        .rls_export_format_number(summary$residual_deviance %||% NA_real_, 3L),
        as.character(summary$df_residual %||% NA_integer_),
        .rls_export_format_number(summary$aic %||% NA_real_, 1L),
        .rls_export_format_number(summary$bic %||% NA_real_, 1L),
        .rls_export_format_number(summary$binomial_overdispersion_ratio %||% NA_real_, 3L),
        .rls_export_format_number(summary$log_lik %||% NA_real_, 3L)
      ), stringsAsFactors = FALSE
    ))
  }
  if (identical(summary$count_distribution %||% "", "beta_binomial")) {
    values <- summary$beta_binomial_precision_by_imputation %||% numeric()
    values <- values[is.finite(values)]
    multiple <- length(values) > 1L
    precision <- summary$beta_binomial_precision %||% NA_real_
    if (!length(precision) || !is.finite(precision[[1L]])) {
      precision <- summary$dispersion %||% NA_real_
    }
    return(data.frame(
      label = c("N", "df residual", "AIC", "BIC",
        .rls_count_parameter_label("beta_binomial", multiple),
        if (multiple) "Beta-binomial precision (phi) range across imputations" else character(),
        "logLik"),
      value = c(
        as.character(summary$n_used %||% NA_integer_),
        as.character(summary$df_residual %||% NA_integer_),
        .rls_export_format_number(summary$aic %||% NA_real_, 1L),
        .rls_export_format_number(summary$bic %||% NA_real_, 1L),
        .rls_export_format_number(if (multiple) mean(values) else precision, 3L),
        if (multiple) paste0(.rls_export_format_number(min(values), 3L), "\u2013",
                             .rls_export_format_number(max(values), 3L)) else character(),
        .rls_export_format_number(summary$log_lik %||% NA_real_, 3L)
      ), stringsAsFactors = FALSE
    ))
  }
  data.frame(
    label = c("N", "Null deviance", "Residual deviance", "df residual", "AIC", "BIC",
      .rls_count_parameter_label(summary$count_distribution %||% "",
        grepl("across imputations", summary$fit_information_method %||% "", fixed = TRUE)), "logLik"),
    value = c(
      as.character(summary$n_used %||% NA_integer_),
      .rls_export_format_number(summary$null_deviance %||% NA_real_, 3L),
      .rls_export_format_number(summary$residual_deviance %||% NA_real_, 3L),
      as.character(summary$df_residual %||% NA_integer_),
      .rls_export_format_number(summary$aic %||% NA_real_, 1L),
      .rls_export_format_number(summary$bic %||% NA_real_, 1L),
      .rls_export_format_number(summary$dispersion %||% NA_real_, 3L),
      .rls_export_format_number(summary$log_lik %||% NA_real_, 3L)
    ),
    stringsAsFactors = FALSE
  )
}

.rls_generalized_glm_fit_rows <- function(summary) {
  rows <- .rls_generalized_glm_fit_rows_base(summary)
  diagnostics <- summary$family_diagnostics %||% list()
  if (identical(summary$count_distribution %||% "", "binomial_trials") &&
      !is.finite(summary$binomial_overdispersion_ratio %||% NA_real_)) {
    rows <- rows[!grepl("Pearson dispersion ratio", rows$label, fixed = TRUE), , drop = FALSE]
  }
  if (as.character(summary$family %||% "") %in% c("Gamma", "gamma_distance")) {
    rows$label[rows$label == "Dispersion"] <- "Gamma dispersion (phi)"
  }
  labels <- c(
    beta_binomial_rho = "Intra-trial correlation (rho)",
    beta_binomial_variance_inflation = "Variance inflation vs binomial",
    negative_binomial_variance_inflation = "Variance inflation vs Poisson at mean fitted count",
    pearson_dispersion_ratio = "Pearson dispersion ratio",
    gamma_cv = "Implied coefficient of variation"
  )
  multiple <- isTRUE(summary$multiple_imputation) ||
    length(summary$theta_by_imputation %||% numeric()) > 1L ||
    length(summary$beta_binomial_precision_by_imputation %||% numeric()) > 1L ||
    grepl("across imputations", summary$fit_information_method %||% "", fixed = TRUE)
  for (key in names(labels)) {
    value <- diagnostics[[key]] %||% NA_real_
    if (!is.finite(value) ||
        (key == "pearson_dispersion_ratio" &&
         identical(summary$count_distribution %||% "", "binomial_trials"))) next
    label <- unname(labels[[key]])
    if (key == "beta_binomial_variance_inflation" &&
        isTRUE(diagnostics$beta_binomial_trials_vary > 0.5)) {
      label <- paste0(label, " (mean across fitted observations)")
    }
    if (multiple) label <- paste0(label, " \u2014 mean across imputations (not Rubin-pooled)")
    formatted <- .rls_export_format_number(value,
      if (grepl("inflation", key)) 2L else 3L)
    if (grepl("inflation", key)) formatted <- paste0(formatted, "\u00D7")
    rows <- rbind(rows, data.frame(label = label, value = formatted,
                                   stringsAsFactors = FALSE))
  }
  rownames(rows) <- NULL
  rows
}

.rls_generalized_glm_normalize_term_types <- function(term_types, terms) {
  if (is.null(term_types) || !length(term_types)) return(list())
  if (!is.list(term_types)) term_types <- as.list(term_types)
  term_names <- names(term_types)
  if (is.null(term_names)) return(list())
  out <- list()
  for (i in seq_along(term_types)) {
    term <- term_names[[i]]
    type <- as.character(term_types[[i]])[[1L]]
    if (!nzchar(term) || !term %in% terms || !type %in% c("numeric", "factor")) next
    out[[term]] <- type
  }
  out
}

.rls_generalized_glm_native_payload <- function(record) {
  term_types <- record$term_types %||% list()
  type_payload <- as.character(length(term_types))
  if (length(term_types)) {
    for (term in names(term_types)) {
      type_payload <- c(type_payload, .rls_native_wire_value(term), .rls_native_wire_value(term_types[[term]]))
    }
  }
  centered <- unique(as.character(record$centered_predictors %||% character()))
  centered <- centered[nzchar(centered)]
  references <- record$factor_reference_levels %||% list()
  reference_payload <- as.character(length(references))
  if (length(references)) {
    for (term in names(references)) {
      reference_payload <- c(
        reference_payload,
        .rls_native_wire_value(term),
        .rls_native_wire_value(references[[term]])
      )
    }
  }
  specification_payload <- c(
    "MODEL_SPEC_V1",
    as.character(length(centered)),
    .rls_native_wire_value(centered),
    reference_payload
  )
  data_scope <- record$data_scope %||% list()
  scope_kind <- as.character(data_scope$kind %||% if (identical(record$scope, "all")) "all" else "explicit")[[1L]]
  scope_rows <- as.integer(data_scope$rows %||% if (identical(record$scope, "selected")) {
    record$selected_rows %||% integer()
  } else if (identical(record$scope, "unselected")) {
    setdiff(seq_len(nrow(record$data)), record$selected_rows %||% integer())
  } else {
    integer()
  })
  scope_rows <- unique(scope_rows[is.finite(scope_rows) & scope_rows >= 1L & scope_rows <= nrow(record$data)])
  scope_source_kind <- as.character(data_scope$source_kind %||%
    if (identical(scope_kind, "all")) "all_data" else "other_explicit_subset")[[1L]]
  scope_description <- as.character(data_scope$description %||%
    if (identical(scope_kind, "all")) "All observations" else "Explicit subset")[[1L]]
  scope_payload <- c(
    "ANALYSIS_SCOPE_V1",
    .rls_native_wire_value(scope_kind),
    .rls_native_wire_value(scope_source_kind),
    .rls_native_wire_value(scope_description),
    as.character(nrow(record$data)),
    as.character(length(scope_rows)),
    as.character(scope_rows)
  )
  bounds_payload <- if (length(record$response_bounds %||% numeric()) == 2L) {
    c("BOUNDED_RESPONSE_V1", as.character(record$response_bounds[[1L]]),
      as.character(record$response_bounds[[2L]]))
  } else character()
  offset_payload <- c("OFFSET_SPEC_V1", .rls_native_wire_value(record$offset_variable %||% ""))
  c(
    "GENERALIZED_GLM_OPEN_STRUCTURED",
    .rls_native_wire_value(record$id),
    .rls_native_wire_value(record$group),
    .rls_native_wire_value(.rls_model_window_title(
      record$model_type %||% .rls_model_type_for_family(
        record$family, record$binary_regression, record$count_regression
      ), identical(record$analysis_backend %||% "ordinary", "multiple_imputation")
    )),
    .rls_native_wire_value(record$response),
    .rls_native_wire_value(record$family),
    .rls_native_wire_value(record$link),
    .rls_native_wire_value(record$scope %||% "all"),
    .rls_native_wire_value(record$status %||% ""),
    as.character(length(record$terms %||% character())),
    .rls_native_wire_value(record$terms %||% character()),
    type_payload,
    specification_payload,
    scope_payload,
    bounds_payload,
    offset_payload,
    .rls_native_generalized_state_payload(record),
    "GGLM_RESULT_V1",
    .rls_native_wire_integer(record$native_generation %||% 0L),
    .rls_analysis_provenance_payload(record)
  )
}

.rls_generalized_glm_sync_native <- function(record) {
  .rls_start_backend()
  dataset <- .rls_dataset_record(record$group)
  .rls_register_native_dataset_if_needed(dataset, record$data)
  try(.rls_send(.rls_generalized_glm_native_payload(record)), silent = TRUE)
}

.rls_generalized_glm_sync_native_error <- function(id, group, response, family, link, scope, terms,
                                                   term_types = list(), centered_predictors = character(),
                                                   factor_reference_levels = list(), message,
                                                   binary = FALSE, event = "", reference = "",
                                                   count = FALSE, count_distribution = "poisson",
                                                   exposure = "", offset = "",
                                                   trials_variable = "", trials_constant = NA_real_,
                                                   response_bounds = NULL,
                                                   model_type = "legacy_generalized",
                                                   generation = 0L) {
  dataset <- tryCatch(.rls_dataset_record(group), error = function(e) NULL)
  record <- list(
    id = id,
    glm_model_id = id,
    group = group,
    dataset_id = group,
    data = if (is.null(dataset)) data.frame() else dataset$data,
    response = response,
    response_variable = response,
    terms = terms %||% character(),
    term_types = .rls_generalized_glm_normalize_term_types(term_types, terms %||% character()),
    centered_predictors = unique(as.character(centered_predictors %||% character())),
    factor_reference_levels = factor_reference_levels %||% list(),
    family = family,
    link = link,
    model_type = model_type,
    response_bounds = response_bounds,
    response_transformation = if (family %in% c("beta", "beta_one_inflated")) {
      "(Y - lower) / (upper - lower)"
    } else if (identical(family, "gamma_distance")) "upper - Y"
    else if (identical(family, "lognormal")) "log(Y)" else "none",
    binary_regression = isTRUE(binary),
    count_regression = isTRUE(count),
    count_distribution = count_distribution,
    exposure = exposure,
    trials_variable = trials_variable,
    trials_constant = trials_constant,
    offset_variable = offset,
    native_generation = as.integer(generation %||% 0L),
    event = event,
    reference = reference,
    scope = scope %||% "all",
    fitted_glm = NULL,
    fit = NULL,
    coefficients = data.frame(),
    coefficient_rows = data.frame(),
    fit_statistics = list(),
    summary = list(),
    diagnostic_data = data.frame(),
    diagnostics = data.frame(),
    diagnostics_by_imputation = NULL,
    fits_by_imputation = NULL,
    multiple_imputation = NULL,
    rows_used = integer(),
    rows_excluded = integer(),
    status = message
  )
  try(.rls_send(.rls_generalized_glm_native_payload(record)), silent = TRUE)
}

#' Create a generalized linear model using stats::glm()
#'
#' @param data Registered dataset name, a data frame, or `NULL` for active dataset.
#' @param response Response variable.
#' @param terms Predictor terms.
#' @param family GLM family.
#' @param link Link function. If `NULL`, the family default is used.
#' @param offset Optional numeric column added to the linear predictor with a
#'   fixed coefficient of one. This is on the link scale; for count exposure
#'   use `exposure` instead, which adds `log(exposure)`.
#' @param response_bounds Optional numeric `c(lower, upper)` bounds. Required
#'   for `beta`, `beta_one_inflated`, and `gamma_distance`.
#'   For negative binomial, an explicit upper bound also enables a descriptive
#'   probability-above-maximum diagnostic without truncating the model.
#' @param diagnostic_seed Non-negative integer seed for reproducible randomized
#'   quantile diagnostics. Does not change fitting or the imputations.
#' @param scope One of `"all"`, `"selected"`, or `"unselected"`.
#' @param name Optional model id or dataset name for data-frame input.
#' @param native Logical. Reserved for native window integration.
#' @param term_types Optional named vector/list with per-term `"numeric"` or `"factor"` overrides.
#' @param centered_predictors Optional predictor names to center within this model only.
#' @param factor_reference_levels Optional named list of model-local reference categories for categorical predictors.
#' @param .selected_rows Internal explicit original-row IDs supplied by the native analysis-scope bridge.
#' @param .scope_description Internal immutable analysis-scope description for a native result window.
#' @param .scope_source_kind Internal analysis-scope source identifier for a native result window.
#' @return An `rlispstat_generalized_linear_model` handle.
#' @export
ls_new_generalized_linear_model <- function(data = NULL, response = NULL, terms = NULL,
                                            family = "binomial", link = NULL,
                                            scope = "all", name = NULL, native = TRUE,
                                            term_types = NULL, centered_predictors = NULL,
                                            factor_reference_levels = NULL,
                                            response_bounds = NULL, .selected_rows = NULL,
                                            .scope_description = NULL,
                                            .scope_source_kind = NULL,
                                            .allow_intercept_only = FALSE,
                                            .count_regression = FALSE,
                                            .count_distribution = "poisson",
                                            .exposure = NULL,
                                            .trials = NULL,
                                            .model_type = "legacy_generalized",
                                            .native_generation = 0L,
                                            .comparison_rows = NULL,
                                            diagnostic_seed = 104729L,
                                            offset = NULL) {
  scope <- match.arg(scope, c("all", "selected", "unselected"))
  model_type <- match.arg(as.character(.model_type)[[1L]],
                          names(.rls_model_type_catalogue))
  if (is.data.frame(data)) {
    group <- .rls_register_dataset(name %||% "generalized_linear_model", data, activate = TRUE)
    dataset <- .rls_dataset_record(group)
  } else {
    dataset <- .rls_dataset_record(data)
  }
  scope_snapshot <- .rls_capture_analysis_scope(dataset, scope, .selected_rows)
  scope <- scope_snapshot$fit_scope
  .selected_rows <- if(scope=="all") integer() else scope_snapshot$rows
  .scope_description <- .scope_description %||% scope_snapshot$description
  .scope_source_kind <- .scope_source_kind %||% scope_snapshot$source_kind
  response <- response %||% names(dataset$data)[[1L]]
  response <- .rls_validate_protocol_name(response, "response")
  if (!response %in% names(dataset$data)) {
    stop(sprintf("Column `%s` was not found in the dataset.", response), call. = FALSE)
  }
  offset_variable <- as.character(offset %||% "")[[1L]]
  if (nzchar(offset_variable) &&
      (!offset_variable %in% names(dataset$data) ||
       !is.numeric(dataset$data[[offset_variable]]) ||
       identical(offset_variable, response))) {
    stop("Offset must be a numeric column distinct from the response.", call. = FALSE)
  }
  if (nzchar(offset_variable) &&
      (identical(family, "beta_one_inflated") ||
       (isTRUE(.count_regression) && .count_distribution %in%
          c("hurdle_beta_binomial_ceiling", "perfect_score")))) {
    stop("An offset is not supported for this model distribution.", call. = FALSE)
  }
  count_distribution <- match.arg(
    as.character(.count_distribution)[[1L]],
    c("poisson", "quasipoisson", "negative_binomial",
      "binomial_trials", "beta_binomial", "hurdle_beta_binomial_ceiling",
      "perfect_score")
  )
  effective_distribution <- if (isTRUE(.count_regression)) count_distribution else family
  distribution_spec <- .rls_validate_model_distribution(
    model_type, effective_distribution, allow_inference_option = TRUE
  )
  allowed_links <- if (identical(model_type, "legacy_generalized")) {
    .rls_glm_family_links[[family]]
  } else distribution_spec$links
  default_link <- if (identical(model_type, "legacy_generalized")) {
    .rls_glm_default_link(family)
  } else distribution_spec$default_link
  if (is.null(link)) link <- default_link
  if (!link %in% allowed_links) {
    stop(sprintf(
      "Link `%s` is not available for %s in %s. Available links: %s.",
      link, distribution_spec$label, .rls_model_type_spec(model_type)$label,
      paste(allowed_links, collapse = ", ")
    ), call. = FALSE)
  }
  family_object <- .rls_glm_make_family(family, link)
  link <- family_object$link
  response_bounds <- .rls_glm_validate_response_bounds(response_bounds, family)
  exposure <- as.character(.exposure %||% "")[[1L]]
  trials_variable <- ""
  trials_constant <- NA_real_
  if (isTRUE(.count_regression)) {
    if (!is.numeric(dataset$data[[response]])) {
      stop("Count Model requires a numeric response.", call. = FALSE)
    }
    if (nzchar(exposure) && (!exposure %in% names(dataset$data) ||
                            !is.numeric(dataset$data[[exposure]]))) {
      stop("Exposure must be a numeric column.", call. = FALSE)
    }
    bounded_count <- .rls_count_distribution_uses_trials(count_distribution)
    if (bounded_count) {
      trial_spec <- .rls_count_trial_specification(dataset$data, .trials)
      trials_variable <- trial_spec$variable
      trials_constant <- trial_spec$constant
      if (nzchar(exposure)) {
        stop("Exposure is not used by bounded-count binomial models; specify Trials instead.",
             call. = FALSE)
      }
    }
    family <- if (identical(count_distribution, "quasipoisson")) "quasipoisson" else
      if (bounded_count) "binomial" else "poisson"
    link <- if (bounded_count) "logit" else "log"
    family_object <- .rls_glm_make_family(family, link)
  }
  terms <- unlist(lapply(terms %||% character(), function(term) {
    term <- .rls_model_validate_term(dataset$data, term, response = response, what = "term")
    .rls_model_hierarchical_terms(dataset$data, term, response = response)
  }), use.names = FALSE)
  terms <- if (length(terms)) unique(as.character(terms)) else character()
  term_types <- .rls_generalized_glm_normalize_term_types(term_types, terms)
  term_variables <- if (length(terms)) {
    setdiff(all.vars(.rls_model_formula_object(response, terms)), response)
  } else character()
  centered_predictors <- intersect(
    unique(as.character(centered_predictors %||% character())), term_variables
  )
  if (is.null(factor_reference_levels)) factor_reference_levels <- list()
  if (!is.list(factor_reference_levels)) factor_reference_levels <- as.list(factor_reference_levels)
  if (is.null(names(factor_reference_levels))) factor_reference_levels <- list()
  factor_reference_levels <- factor_reference_levels[
    intersect(names(factor_reference_levels), term_variables)
  ]
  model_id <- .rls_generalized_glm_id(dataset$group, name)
  fit_requested <- length(terms) > 0L || isTRUE(.allow_intercept_only)
  sync_during_fit <- isTRUE(native) && fit_requested
  if (sync_during_fit) .rls_start_backend()
  record <- list(
    id = model_id,
    glm_model_id = model_id,
    group = dataset$group,
    dataset_id = dataset$group,
    data = dataset$data,
    dataset_type = dataset$dataset_type %||% "data_frame",
    analysis_backend = .rls_analysis_backend(dataset, "generalized_linear_model"),
    imputation_id = dataset$imputation_id %||% NULL,
    imputation_count = dataset$imputation_count %||% NULL,
    response = response,
    response_variable = response,
    terms = terms,
    term_types = term_types,
    centered_predictors = centered_predictors,
    factor_reference_levels = factor_reference_levels,
    allow_intercept_only = isTRUE(.allow_intercept_only),
    count_regression = isTRUE(.count_regression),
    count_distribution = count_distribution,
    model_type = model_type,
    exposure = exposure,
    offset_variable = offset_variable,
    trials_variable = trials_variable,
    trials_constant = trials_constant,
    family = family,
    link = link,
    family_object = family_object,
    response_bounds = response_bounds,
    response_transformation = if (family %in% c("beta", "beta_one_inflated")) {
      "(Y - lower) / (upper - lower)"
    } else if (identical(family, "gamma_distance")) "upper - Y"
    else if (identical(family, "lognormal")) "log(Y)" else "none",
    scope = scope,
    selected_rows = if (!is.null(.selected_rows)) as.integer(.selected_rows) else
      if (scope != "all") ls_selected(dataset$group) else integer(),
    comparison_rows = if (is.null(.comparison_rows)) NULL else as.integer(.comparison_rows),
    data_scope = list(
      kind = if (identical(scope, "all")) "all" else "explicit",
      rows = if (identical(scope, "all")) integer() else
        if (identical(scope, "unselected")) {
          setdiff(seq_len(nrow(dataset$data)), if (!is.null(.selected_rows)) as.integer(.selected_rows) else ls_selected(dataset$group))
        } else if (!is.null(.selected_rows)) as.integer(.selected_rows) else ls_selected(dataset$group),
      source_kind = .scope_source_kind %||% if (identical(scope, "all")) "all_data" else "other_explicit_subset",
      description = .scope_description %||% if (identical(scope, "all")) "All observations" else "Explicit subset"
    ),
    fitted_glm = NULL,
    fit = NULL,
    coefficients = data.frame(),
    coefficient_rows = data.frame(),
    fit_statistics = list(),
    summary = list(),
    diagnostic_data = data.frame(),
    diagnostics = data.frame(),
    diagnostics_by_imputation = NULL,
    fits_by_imputation = NULL,
    multiple_imputation = NULL,
    scope_request_pending = TRUE,
    native_generation = as.integer(.native_generation %||% 0L),
    model_version = 0L,
    fit_version = 0L,
    diagnostics_version = 0L,
    diagnostic_seed = diagnostic_seed,
    rows_used = integer(),
    rows_excluded = integer(),
    # Whether later mutations/refits of this model should be published to a
    # standalone native GLM window.  Comparison models deliberately keep this
    # FALSE: they publish only the enclosing comparison result.
    native_sync_enabled = sync_during_fit,
    status = if (length(terms) || isTRUE(.allow_intercept_only)) "Not fitted." else "Add at least one predictor."
  )
  handle <- .rls_assign_generalized_glm(record)
  if (fit_requested) {
    handle <- ls_generalized_linear_model_fit(handle)
  }
  record <- .rls_generalized_glm_record(handle)
  record$native_sync_enabled <- isTRUE(native)
  handle <- .rls_assign_generalized_glm(record)
  if (isTRUE(native) && identical(record$analysis_backend, "multiple_imputation")) {
    if (is.null(record$fit)) {
      warning("Add at least one predictor before opening the pooled MI Generalized Linear Model table.", call. = FALSE)
    } else if (!sync_during_fit) {
      .rls_generalized_glm_sync_native_pooled(record)
    }
  } else if (isTRUE(native) && !sync_during_fit) {
    .rls_start_backend()
    .rls_register_native_dataset_if_needed(dataset)
    if (is.null(record$fit) && !isTRUE(record$count_regression)) {
      try(.rls_send(c("GENERALIZED_GLM_OPEN", dataset$group, response, family, link, as.character(length(terms)), terms)), silent = TRUE)
    } else {
      .rls_generalized_glm_sync_native(record)
    }
  }
  invisible(handle)
}

#' @rdname ls_new_generalized_linear_model
#' @export
ls_generalized_linear_model_fit <- function(model) {
  record <- .rls_generalized_glm_record(model)
  if (!length(record$terms) && !isTRUE(record$allow_intercept_only)) {
    stop("Add at least one predictor before fitting.", call. = FALSE)
  }
  record$family_object <- .rls_glm_make_family(record$family, record$link)
  record$response_bounds <- .rls_glm_validate_response_bounds(
    record$response_bounds, record$family
  )
  dataset <- .rls_dataset_record(record$group)
  record <- .rls_apply_scope_to_model_request(record, dataset)
  record$diagnostic_data_version <- as.integer(dataset$data_version %||% 1L)
  if (isTRUE(record$count_regression) && identical(record$count_distribution,"negative_binomial") &&
      is.null(record$response_bounds))
    record$response_bounds <- attr(dataset$data[[record$response]],"theoretical_range",exact=TRUE)
  record$analysis_backend <- .rls_analysis_backend(dataset, "generalized_linear_model")
  executed_r_code <- .rls_generalized_executed_r_code(
    record, identical(record$analysis_backend, "multiple_imputation")
  )
  if (identical(record$analysis_backend, "multiple_imputation")) {
    extracted <- .rls_mi_fit_generalized_model_record(record)
  } else {
    fit_record <- record
    fit_record$data <- .rls_generalized_glm_data_for_fit(record)
    if (isTRUE(record$count_regression)) {
      scoped_rows <- if (identical(record$scope, "selected")) {
        record$selected_rows %||% integer()
      } else if (identical(record$scope, "unselected")) {
        setdiff(seq_len(nrow(fit_record$data)), record$selected_rows %||% integer())
      } else seq_len(nrow(fit_record$data))
      scoped_data <- fit_record$data[scoped_rows, , drop = FALSE]
      bounded_count <- .rls_count_distribution_uses_trials(record$count_distribution)
      if (bounded_count) {
        .rls_count_validate_bounded_response(record, scoped_data)
      } else {
        .rls_count_validate_response(scoped_data, record$response)
        .rls_count_validate_exposure(scoped_data, record$exposure %||% "")
      }
    }
    complete <- .rls_generalized_glm_complete_data(fit_record)
    complete <- .rls_glm_center_complete_data(
      complete, record$centered_predictors %||% character()
    )
    if (identical(record$model_type, "positive_continuous")) {
      .rls_generalized_validate_response(record, complete$data[[record$response]])
    } else if (identical(record$model_type, "proportion")) {
      domain <- if (identical(record$family, "beta"))
        "open_unit_interval" else "open_closed_unit_interval"
      .rls_validate_response_domain(
        complete$data[[record$response]], domain, "Proportion Model"
      )
    }
    complete <- .rls_glm_transform_bounded_response(record, complete)
    if (nrow(complete$data) <= max(1L, length(record$terms))) {
      stop("Not enough complete cases to fit the generalized linear model.", call. = FALSE)
    }
    binary_context <- NULL
    if (isTRUE(record$binary_regression)) {
      binary_context <- .rls_binary_prepare_generalized_fit(record, fit_record, complete)
      fit_record <- binary_context$fit_record
      complete <- binary_context$complete
    }
    captured_warnings <- character()
    fit <- tryCatch(withCallingHandlers(
      .rls_glm_fit_engine(fit_record, complete$data),
      warning = function(w) {
        captured_warnings <<- unique(c(captured_warnings, conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    ), error = function(e) stop(sprintf("Could not fit generalized linear model: %s", conditionMessage(e)), call. = FALSE))
    extracted <- .rls_generalized_glm_extract_fit(fit_record, fit, complete)
    if (isTRUE(record$binary_regression)) {
      extracted <- .rls_binary_finalize_generalized_fit(
        record, fit_record, fit, complete, binary_context$coding, extracted,
        captured_warnings
      )
    } else {
      extracted$warnings <- unique(c(extracted$warnings %||% character(),
                                     captured_warnings))
      extracted$summary$warnings <- extracted$warnings
    }
  }
  report_preparation_started <- proc.time()[["elapsed"]]
  if (identical(record$analysis_backend, "multiple_imputation") &&
      isTRUE(record$native_sync_enabled) && isTRUE(.rls_state$process_started)) {
    .rls_mi_progress_event(record$id, record$group, "preparing_output",
      completed = length(extracted$multiple_imputation$fits_by_imputation %||% list()),
      total = length(extracted$multiple_imputation$fits_by_imputation %||% list()),
      workers = extracted$execution_diagnostics$workers %||% 1L,
      message = "Building the model table and verification code\u2026")
  }
  diagnostic_warnings <- .rls_discrete_diagnostic_warnings(
    extracted$summary, extracted$summary$n_used %||% length(extracted$rows_used)
  )
  if (length(diagnostic_warnings)) {
    extracted$warnings <- unique(c(
      extracted$warnings %||% character(), diagnostic_warnings
    ))
    extracted$summary$warnings <- extracted$warnings
    extracted$status <- paste(
      extracted$status %||% "", paste(diagnostic_warnings, collapse = " ")
    )
  }
  record[names(extracted)] <- extracted
  verification <- .rls_generalized_verification_r_code(
    record, identical(record$analysis_backend, "multiple_imputation")
  )
  record <- .rls_attach_analysis_provenance(
    record,
    executed_r_code,
    title = .rls_model_window_title(
      record$model_type %||% .rls_model_type_for_family(
        record$family, record$binary_regression, record$count_regression
      ), identical(record$analysis_backend, "multiple_imputation")
    ),
    output_code = .rls_generalized_output_r_code(
      identical(record$analysis_backend, "multiple_imputation")
    ),
    verification_code = c(list(model = verification$code),
      if (nzchar(.rls_dunn_smyth_verification_code(record)))
        list(diagnostic_randomization = .rls_dunn_smyth_verification_code(record)) else list()),
    verification_variables = verification$variables,
    verification_warnings = verification$warnings
  )
  record <- .rls_missingness_attach_model_provenance(record)
  record$fit_statistics <- extracted$summary
  record$diagnostic_data <- extracted$diagnostics
  record$fit_version <- record$fit_version + 1L
  record$diagnostics_version <- record$diagnostics_version + 1L
  handle <- .rls_assign_generalized_glm(record)
  .rls_glm_refresh_diagnostic_plots(record)
  if (isTRUE(record$native_sync_enabled) &&
      identical(record$analysis_backend, "multiple_imputation") &&
      isTRUE(.rls_state$process_started)) {
    report_preparation_seconds <- proc.time()[["elapsed"]] -
      report_preparation_started
    .rls_mi_progress_event(record$id, record$group, "transferring",
      completed = length(record$multiple_imputation$fits_by_imputation %||% list()),
      total = length(record$multiple_imputation$fits_by_imputation %||% list()),
      workers = record$execution_diagnostics$workers %||% 1L,
      message = "Sending model results to LinkEDA\u2026")
    presentation <- .rls_generalized_glm_sync_native_pooled(record)
    dataset_sync_seconds <- presentation$backend_startup_seconds +
      presentation$dataset_preparation_seconds +
      presentation$dataset_delivery_seconds
    result_delivery_seconds <- presentation$model_preparation_seconds +
      presentation$model_delivery_seconds
    fit_seconds <- record$execution_diagnostics$model_fitting_seconds %||% NA_real_
    timing <- sprintf(
      "Results ready \u00B7 R fit %.1fs \u00B7 report %.1fs \u00B7 data sync %.1fs \u00B7 result send/draw %.1fs",
      fit_seconds, report_preparation_seconds, dataset_sync_seconds,
      result_delivery_seconds
    )
    .rls_mi_progress_event(record$id, record$group, "completed", "completed",
      message = timing)
  } else if (isTRUE(record$native_sync_enabled) && isTRUE(.rls_state$process_started)) {
    try(.rls_generalized_glm_sync_native(record), silent = TRUE)
  }
  handle
}

#' @rdname ls_new_generalized_linear_model
#' @export
ls_generalized_linear_model_set_family <- function(model, family, link = NULL) {
  record <- .rls_generalized_glm_record(model)
  distribution_spec <- .rls_validate_model_distribution(
    record$model_type %||% "legacy_generalized", family,
    allow_inference_option = TRUE
  )
  legacy <- identical(record$model_type %||% "legacy_generalized", "legacy_generalized")
  allowed_links <- if (legacy) .rls_glm_family_links[[family]] else distribution_spec$links
  link <- link %||% if (legacy) .rls_glm_default_link(family) else distribution_spec$default_link
  if (!link %in% allowed_links) {
    stop(sprintf("Link `%s` is not available for %s.", link, distribution_spec$label),
         call. = FALSE)
  }
  family_object <- .rls_glm_make_family(family, link)
  .rls_glm_validate_response_bounds(record$response_bounds, family)
  record$family <- family
  record$link <- family_object$link
  record$family_object <- family_object
  record$response_transformation <- if (family %in% c("beta", "beta_one_inflated")) {
    "(Y - lower) / (upper - lower)"
  } else if (identical(family, "gamma_distance")) "upper - Y"
  else if (identical(family, "lognormal")) "log(Y)" else "none"
  record$model_version <- record$model_version + 1L
  record$fit <- NULL
  record$fitted_glm <- NULL
  .rls_assign_generalized_glm(record)
  if (length(record$terms) || isTRUE(record$allow_intercept_only)) {
    ls_generalized_linear_model_fit(model)
  } else invisible(model)
}

#' @rdname ls_new_generalized_linear_model
#' @export
ls_generalized_linear_model_set_response_bounds <- function(model, lower, upper) {
  record <- .rls_generalized_glm_record(model)
  bounds <- suppressWarnings(as.numeric(c(lower, upper)))
  if (length(bounds) != 2L || any(!is.finite(bounds)) || bounds[[1L]] >= bounds[[2L]]) {
    stop("Response bounds must be two finite values with lower < upper.", call. = FALSE)
  }
  record$response_bounds <- unname(bounds)
  record$model_version <- record$model_version + 1L
  record$fit <- NULL
  record$fitted_glm <- NULL
  handle <- .rls_assign_generalized_glm(record)
  if (length(record$terms) || isTRUE(record$allow_intercept_only)) {
    ls_generalized_linear_model_fit(handle)
  } else invisible(handle)
}

#' @rdname ls_new_generalized_linear_model
#' @export
ls_generalized_linear_model_set_link <- function(model, link) {
  record <- .rls_generalized_glm_record(model)
  ls_generalized_linear_model_set_family(model, record$family, link)
}

#' @rdname ls_new_generalized_linear_model
#' @export
ls_generalized_linear_model_coefficients <- function(model) {
  record <- .rls_generalized_glm_record(model)
  if (is.null(record$fit)) record <- .rls_generalized_glm_record(ls_generalized_linear_model_fit(model))
  record$coefficients
}

#' @rdname ls_new_generalized_linear_model
#' @export
ls_generalized_linear_model_coefficient_rows <- function(model) {
  record <- .rls_generalized_glm_record(model)
  if (is.null(record$fit)) record <- .rls_generalized_glm_record(ls_generalized_linear_model_fit(model))
  record$coefficient_rows
}

#' @rdname ls_new_generalized_linear_model
#' @export
ls_generalized_linear_model_fit_summary <- function(model) {
  record <- .rls_generalized_glm_record(model)
  if (is.null(record$fit)) record <- .rls_generalized_glm_record(ls_generalized_linear_model_fit(model))
  record$summary
}

#' @rdname ls_new_generalized_linear_model
#' @export
ls_generalized_linear_model_diagnostics <- function(model) {
  record <- .rls_generalized_glm_record(model)
  if (is.null(record$fit)) record <- .rls_generalized_glm_record(ls_generalized_linear_model_fit(model))
  record$diagnostics
}

#' @rdname ls_new_generalized_linear_model
#' @export
ls_generalized_linear_model_open_diagnostic <- function(
    model,
    type = c("observed_vs_fitted", "residuals_vs_fitted", "residual_histogram",
             "normal_qq", "scale_location", "residuals_leverage",
             "cooks_distance", "roc_curve", "calibration_plot",
             "observed_predicted_distribution", "boundary_zero_fit"),
    native = isTRUE(.rls_state$process_started)) {
  type <- match.arg(type)
  record <- .rls_generalized_glm_record(model)
  if (is.null(record$fit)) record <- .rls_generalized_glm_record(ls_generalized_linear_model_fit(model))
  id <- paste(record$id, type, length(ls(.rls_state$glm_diagnostic_plots)) + 1L, sep = ":")
  native_plot_id <- NULL
  if (isTRUE(native)) {
    .rls_start_backend()
    if (!isTRUE(.rls_state$process_started)) {
      stop("The native backend could not be started.", call. = FALSE)
    }
    reply <- .rls_send(c("GENERALIZED_GLM_OPEN_DIAGNOSTIC", record$id, type))
    records <- .rls_parse_records(reply)
    if (!length(records) || !nzchar(records[[1L]])) {
      stop("The native backend did not return a diagnostic plot id.", call. = FALSE)
    }
    native_plot_id <- records[[1L]]
  }
  plot <- list(
    id = id,
    model_id = record$id,
    type = type,
    native_plot_id = native_plot_id,
    displayed_fit_version = record$fit_version,
    displayed_diagnostics_version = record$diagnostics_version,
    is_stale = FALSE,
    data = if (type %in% c("observed_predicted_distribution", "boundary_zero_fit"))
      record$summary$observed_predicted_distribution %||% data.frame()
    else record$diagnostics
  )
  assign(id, plot, envir = .rls_state$glm_diagnostic_plots)
  structure(plot, class = "rlispstat_glm_diagnostic")
}

#' @rdname ls_new_generalized_linear_model
#' @export
ls_generalized_linear_model_state <- function(model) {
  .rls_generalized_glm_record(model)
}

#' @rdname ls_new_generalized_linear_model
#' @export
ls_generalized_linear_model_term_tests <- function(model) {
  record <- .rls_generalized_glm_record(model)
  if (is.null(record$fit)) {
    record <- .rls_generalized_glm_record(
      ls_generalized_linear_model_fit(model)
    )
  }
  .rls_as_global_term_tests(record$term_tests %||% data.frame())
}

#' Compare fitted generalized linear models
#'
#' Likelihood-ratio tests are reported only for adjacent nested models fitted
#' to identical rows with the same distribution, link, and response bounds.
#' Other fit statistics remain
#' available as a descriptive comparison.
#'
#' @param ... Fitted generalized-linear-model handles, or one list of handles.
#' @param native Logical; open the native Windows comparison window.
#' @return A structured comparison. On Windows, also opens the native window.
#' @export
.rls_generalized_likelihood_ratio <- function(model_a, model_b) {
  log_likelihood_a <- stats::logLik(model_a)
  log_likelihood_b <- stats::logLik(model_b)
  if (attr(log_likelihood_a, "df") <= attr(log_likelihood_b, "df")) {
    reduced_log_likelihood <- log_likelihood_a
    full_log_likelihood <- log_likelihood_b
  } else {
    reduced_log_likelihood <- log_likelihood_b
    full_log_likelihood <- log_likelihood_a
  }
  statistic <- 2 * (as.numeric(full_log_likelihood) -
                      as.numeric(reduced_log_likelihood))
  df <- as.integer(attr(full_log_likelihood, "df") -
                     attr(reduced_log_likelihood, "df"))
  list(
    statistic = statistic,
    df = df,
    p_value = if (is.finite(statistic) && is.finite(df) && df > 0L) {
      stats::pchisq(max(0, statistic), df = df, lower.tail = FALSE)
    } else NA_real_
  )
}

ls_compare_generalized_linear_models <- function(...,
                                                  native = isTRUE(.rls_state$process_started),
                                                  .native_id = NULL,
                                                  .native_labels = NULL,
                                                  .native_generation = 0L,
                                                  .native_specification_revisions = integer(),
                                                  .native_specification_fingerprints = character(),
                                                  .binary_comparison = FALSE,
                                                  .count_comparison = FALSE,
                                                  .model_type = NULL) {
  models <- list(...)
  if (length(models) == 1L && is.list(models[[1L]]) &&
      !inherits(models[[1L]], "rlispstat_generalized_linear_model")) models <- models[[1L]]
  if (length(models) < 1L) stop("Choose at least one stored Generalized Linear Model.", call. = FALSE)
  records <- lapply(models, .rls_generalized_glm_record)
  mi_flags <- vapply(records, function(record) {
    identical(record$analysis_backend %||% "ordinary", "multiple_imputation")
  }, logical(1L))
  if (any(mi_flags) && !all(mi_flags)) {
    stop("Ordinary and multiple-imputation generalized models cannot be compared together.", call. = FALSE)
  }
  mi_comparison <- all(mi_flags)
  rows_by_imputation <- lapply(records, function(record) {
    complete <- record$multiple_imputation$complete_data_result_by_imputation %||% list()
    lapply(complete, function(item) sort(as.integer(item$original_rows %||% item$rows %||% integer())))
  })
  expected_binary <- isTRUE(.binary_comparison)
  expected_count <- isTRUE(.count_comparison)
  if (expected_binary && expected_count) {
    stop("A comparison cannot be both a Binary Model and a Count Model.", call. = FALSE)
  }
  model_type <- if (expected_binary) "binary" else if (expected_count) "count" else {
    value <- as.character(.model_type %||% "legacy_generalized")[[1L]]
    match.arg(value, names(.rls_model_type_catalogue))
  }
  model_label <- .rls_model_type_spec(model_type)$label
  valid_kind <- vapply(records, function(x) {
    identical(isTRUE(x$binary_regression), expected_binary) &&
      identical(isTRUE(x$count_regression), expected_count) &&
      identical(x$model_type %||% "legacy_generalized", model_type) && !is.null(x$fit)
  }, logical(1L))
  if (!all(valid_kind)) {
    stop(if (expected_binary) "All models must be fitted Binary Models."
         else if (expected_count) "All models must be fitted Count Models."
         else sprintf("All models must be fitted %ss.", model_label),
         call. = FALSE)
  }
  base <- records[[1L]]
  same_identity <- vapply(records, function(x) {
    identical(x$group, base$group) && identical(x$response, base$response) &&
      (!expected_binary || (identical(x$event, base$event) && identical(x$reference, base$reference)))
  }, logical(1L))
  if (!all(same_identity)) {
    stop(if (expected_binary) "Models must use the same dataset, response, and event/reference coding."
         else "Models must use the same dataset and response.", call. = FALSE)
  }
  # Stored models must refer to the same immutable data version, including MI.
  # Matching dataset names and row numbers alone does not establish this.
  versions <- vapply(records, function(x) as.integer(
    x$diagnostic_data_version %||% x$analysis_provenance$dataset_version %||%
      x$data_scope$dataset_version %||% NA_integer_), integer(1L))
  if (length(unique(versions[!is.na(versions)])) > 1L) {
    stop("Models were fitted to different data versions. Refit all models on the same data before comparing them.",
         call. = FALSE)
  }
  # Check fitted inputs too: this also protects older stored lm/glm fits without
  # version metadata. Use the immutable model frame, never current sheet data.
  for (i in seq_along(records)[-1L]) {
    left <- records[[i - 1L]]; right <- records[[i]]
    left_fits <- if (mi_comparison) left$fits_by_imputation else list(left$fit)
    right_fits <- if (mi_comparison) right$fits_by_imputation else list(right$fit)
    if (identical(left$family, right$family) && identical(left$rows_used, right$rows_used) &&
        identical(left$exposure, right$exposure) &&
        identical(left$trials_variable, right$trials_variable) &&
        identical(left$trials_constant, right$trials_constant) &&
        identical(left$response_bounds, right$response_bounds) &&
        length(left_fits) == length(right_fits)) {
      for (j in seq_along(left_fits)) {
        if (!inherits(left_fits[[j]], "lm") || !inherits(right_fits[[j]], "lm")) next
        a <- tryCatch(stats::model.frame(left_fits[[j]]), error = function(e) NULL)
        b <- tryCatch(stats::model.frame(right_fits[[j]]), error = function(e) NULL)
        if (is.null(a) || is.null(b)) next # Other engines are guarded by data version above.
        same <- all(vapply(list(stats::model.response, stats::model.weights, stats::model.offset),
          function(extract) isTRUE(all.equal(extract(a), extract(b), check.attributes = FALSE)), logical(1L)))
        if (!same) stop("Models use different fitted responses, weights, or offsets. Refit all models on the same data before comparing them.",
                        call. = FALSE)
      }
    }
  }
  summaries <- lapply(records, function(x) x$summary %||% list())
  model_capabilities <- lapply(seq_along(records), function(i) {
    record <- records[[i]]
    summary <- summaries[[i]]
    ordinary_likelihood_by_imputation <- if (mi_comparison) {
      fits <- record$fits_by_imputation %||% list()
      length(fits) > 0L && all(vapply(fits, .rls_mi_fit_has_ordinary_likelihood, logical(1L)))
    } else {
      isTRUE(summary$likelihood_available %||%
        !identical(record$count_distribution, "quasipoisson"))
    }
    has_likelihood <- !mi_comparison && ordinary_likelihood_by_imputation
    list(
      has_likelihood = has_likelihood,
      has_aic = has_likelihood && is.finite(as.numeric(summary$aic %||% NA_real_)),
      supports_nested_lr = has_likelihood &&
        isTRUE(summary$likelihood_ratio_available %||% has_likelihood),
      ordinary_likelihood_by_imputation = ordinary_likelihood_by_imputation
    )
  })
  model_matrix_nested <- function(reduced, full) {
    reduced_matrix <- tryCatch(stats::model.matrix(reduced$fit), error = function(e) NULL)
    full_matrix <- tryCatch(stats::model.matrix(full$fit), error = function(e) NULL)
    if (is.null(reduced_matrix) || is.null(full_matrix) ||
        nrow(reduced_matrix) != nrow(full_matrix)) return(FALSE)
    full_rank <- qr(full_matrix)$rank
    qr(cbind(full_matrix, reduced_matrix))$rank == full_rank
  }
  model_table <- data.frame(
    model = vapply(records, function(x) x$id, character(1L)),
    family = vapply(records, function(x) x$family, character(1L)),
    link = vapply(records, function(x) x$link, character(1L)),
    count_distribution = vapply(records, function(x) {
      if (isTRUE(x$count_regression)) as.character(x$count_distribution %||% "poisson") else ""
    }, character(1L)),
    exposure = vapply(records, function(x) as.character(x$exposure %||% ""), character(1L)),
    offset = vapply(records, function(x) as.character(x$offset_variable %||% ""), character(1L)),
    trials_variable = vapply(records, function(x) as.character(x$trials_variable %||% ""), character(1L)),
    trials_constant = vapply(records, function(x) as.numeric(x$trials_constant %||% NA_real_), numeric(1L)),
    has_likelihood = vapply(model_capabilities, function(x) x$has_likelihood, logical(1L)),
    has_aic = vapply(model_capabilities, function(x) x$has_aic, logical(1L)),
    supports_nested_lr = vapply(model_capabilities, function(x) x$supports_nested_lr, logical(1L)),
    n = vapply(summaries, function(x) as.integer(x$n_used %||% NA_integer_), integer(1L)),
    logLik = vapply(summaries, function(x) as.numeric(x$log_lik %||% NA_real_), numeric(1L)),
    AIC = vapply(summaries, function(x) as.numeric(x$aic %||% NA_real_), numeric(1L)),
    BIC = vapply(summaries, function(x) as.numeric(x$bic %||% NA_real_), numeric(1L)),
    residual_deviance = vapply(summaries, function(x) as.numeric(x$residual_deviance %||% NA_real_), numeric(1L)),
    df_residual = vapply(summaries, function(x) as.integer(x$df_residual %||% NA_integer_), integer(1L)),
    parameter_count = vapply(summaries, function(x) as.integer(x$parameter_count %||% NA_integer_), integer(1L)),
    theta = vapply(summaries, function(x) as.numeric(x$theta %||% NA_real_), numeric(1L)),
    AUC = vapply(summaries, function(x) as.numeric(x$auc %||% NA_real_), numeric(1L)),
    analysis_backend = if (mi_comparison) "multiple_imputation" else "ordinary",
    terms = vapply(records, function(x) paste(x$terms, collapse = " + "), character(1L)),
    stringsAsFactors = FALSE)
  tests <- vector("list", max(0L, length(records) - 1L))
  for (i in seq_len(max(0L, length(records) - 1L))) {
    left <- records[[i]]; right <- records[[i + 1L]]
    beta_hurdle_pair <- expected_count && setequal(
      c(left$count_distribution, right$count_distribution),
      c("beta_binomial", "hurdle_beta_binomial_ceiling")
    )
    same_distribution <- if (expected_count) {
      identical(left$count_distribution, right$count_distribution) &&
        identical(left$link, right$link)
    } else {
      identical(left$family, right$family) && identical(left$link, right$link) &&
        identical(as.numeric(left$response_bounds %||% numeric()),
                  as.numeric(right$response_bounds %||% numeric()))
    }
    same_exposure <- !expected_count || identical(left$exposure %||% "", right$exposure %||% "")
    same_offset <- identical(left$offset_variable %||% "", right$offset_variable %||% "")
    bounded_count <- expected_count &&
      .rls_count_distribution_uses_trials(left$count_distribution) &&
      .rls_count_distribution_uses_trials(right$count_distribution)
    same_trials <- !bounded_count || (
      identical(left$trials_variable %||% "", right$trials_variable %||% "") &&
      isTRUE(all.equal(
        as.numeric(left$trials_constant %||% NA_real_),
        as.numeric(right$trials_constant %||% NA_real_),
        check.attributes = FALSE
      ))
    )
    same_rows <- if (mi_comparison) {
      identical(rows_by_imputation[[i]], rows_by_imputation[[i + 1L]])
    } else {
      identical(sort(left$rows_used), sort(right$rows_used))
    }
    left_capabilities <- model_capabilities[[i]]
    right_capabilities <- model_capabilities[[i + 1L]]
    supports_lr <- isTRUE(left_capabilities$supports_nested_lr) &&
      isTRUE(right_capabilities$supports_nested_lr)
    left_in_right <- same_distribution && same_exposure && same_offset && same_trials && same_rows &&
      model_matrix_nested(left, right)
    right_in_left <- same_distribution && same_exposure && same_offset && same_trials && same_rows &&
      model_matrix_nested(right, left)
    nested <- left_in_right || right_in_left
    row <- data.frame(reduced = NA_character_, full = NA_character_,
      link = if (same_distribution) left$link else NA_character_,
      method = if (mi_comparison) "mice::D1" else "Likelihood-ratio",
      statistic_label = if (mi_comparison) "F" else "Chi-square",
      df = NA_integer_, df1 = NA_real_, df2 = NA_real_, statistic = NA_real_, p_value = NA_real_, available = FALSE,
      reason = if (!same_rows) "Different analysis rows: descriptive comparison only."
      else if (!same_exposure) "Different exposure specifications: descriptive comparison only."
      else if (!same_offset) "Different offset specifications: descriptive comparison only."
      else if (!same_trials) "Different Trials specifications: descriptive comparison only."
      else if (!same_distribution) {
        if (beta_hurdle_pair && mi_comparison) {
          paste(
            "Beta-binomial and ceiling-hurdle beta-binomial are not nested;",
            "likelihood statistics are not pooled across imputations."
          )
        } else if (beta_hurdle_pair) {
          paste(
            "Beta-binomial and ceiling-hurdle beta-binomial are not nested;",
            "compare their AIC values (no likelihood-ratio p-value)."
          )
        } else if (expected_count) {
          "Different count distributions: descriptive comparison only."
        } else if (identical(left$family, right$family) &&
                   identical(left$link, right$link)) {
          "Different response bounds: descriptive comparison only."
        } else if (identical(left$family, right$family)) {
          "Different links or response bounds: descriptive comparison only."
        } else if (identical(left$link, right$link)) {
          "Different distributions: descriptive comparison only."
        } else {
          "Different distributions, links, or response bounds: descriptive comparison only."
        }
      }
      else if (!mi_comparison && !supports_lr) "Likelihood-ratio comparison is unavailable for this distribution."
      else if (!nested) "Models are not nested." else "", stringsAsFactors = FALSE)
    if (mi_comparison && same_rows && same_distribution && same_exposure && same_offset && same_trials &&
        nested && !(left_in_right && right_in_left)) {
      reduced <- if (left_in_right) left else right
      full <- if (left_in_right) right else left
      added_terms <- setdiff(full$terms, reduced$terms)
      pool <- tryCatch(
        .rls_mi_pool_d1(full$fits_by_imputation, reduced$fits_by_imputation,
                        term_names = added_terms),
        error = function(e) e
      )
      row$reduced <- reduced$id
      row$full <- full$id
      if (inherits(pool, "error")) {
        row$reason <- sprintf("MI nested-model comparison failed in mice::D1: %s", conditionMessage(pool))
      } else {
        row$method <- pool$method
        row$statistic_label <- if (grepl("mice::D3", pool$method, fixed = TRUE)) "D3 F" else "D1 F"
        row$df <- if (is.finite(pool$df1)) as.integer(round(pool$df1)) else NA_integer_
        row$df1 <- pool$df1
        row$df2 <- pool$df2
        row$statistic <- pool$F
        row$p_value <- pool$p
        row$available <- isTRUE(pool$ok) && is.finite(row$statistic) && is.finite(row$p_value)
        row$reason <- if (row$available) {
          sprintf("MI nested-model comparison calculated with %s.", pool$method)
        } else {
          paste(
            sprintf("MI nested-model comparison with %s is unavailable.", pool$method),
            pool$fallback_reason %||% "mice did not return a finite test."
          )
        }
      }
    } else if (!mi_comparison && same_rows && same_distribution && same_exposure && same_offset && same_trials && supports_lr &&
        nested && !(left_in_right && right_in_left)) {
      reduced <- if (left_in_right) left else right
      full <- if (left_in_right) right else left
      if (reduced$family %in% c("gaussian_log", "lognormal")) {
        tab <- tryCatch(
          as.data.frame(stats::anova(reduced$fit, full$fit, test = "F")),
          error = function(e) NULL
        )
        if (!is.null(tab) && nrow(tab) >= 2L) {
          p_name <- grep("Pr\\(", names(tab), value = TRUE)
          statistic_name <- intersect(c("F", "F value"), names(tab))
          row$reduced <- reduced$id
          row$full <- full$id
          row$method <- "Nested-model F test"
          row$statistic_label <- "F"
          row$df <- as.integer(abs(stats::df.residual(reduced$fit) - stats::df.residual(full$fit)))
          row$df1 <- row$df
          row$df2 <- stats::df.residual(full$fit)
          row$statistic <- if (length(statistic_name)) {
            as.numeric(tab[[statistic_name[[1L]]]][[2L]])
          } else NA_real_
          row$p_value <- if (length(p_name)) as.numeric(tab[[p_name[[1L]]]][[2L]]) else NA_real_
          row$available <- is.finite(row$statistic) && is.finite(row$p_value)
          row$reason <- if (row$available) {
            "R: stats::anova(..., test = 'F') for nested Gaussian/log-response models."
          } else "R did not return an estimable nested-model F test."
        }
      } else if (expected_count ||
          reduced$family %in% c("beta", "beta_one_inflated")) {
        likelihood_ratio <- tryCatch(
          .rls_generalized_likelihood_ratio(left$fit, right$fit),
          error = function(e) NULL
        )
        statistic <- likelihood_ratio$statistic %||% NA_real_
        df <- likelihood_ratio$df %||% NA_integer_
        row$reduced <- reduced$id
        row$full <- full$id
        row$df <- df
        row$df1 <- df
        row$statistic <- statistic
        row$p_value <- likelihood_ratio$p_value %||% NA_real_
        row$available <- is.finite(row$statistic) && is.finite(row$p_value)
        row$reason <- if (row$available) "Likelihood-ratio test from fitted log likelihoods." else
          "R did not return an estimable likelihood-ratio test."
      } else {
        tab <- tryCatch(as.data.frame(stats::anova(reduced$fit, full$fit, test = "Chisq")),
                        error = function(e) NULL)
        if (!is.null(tab) && nrow(tab) >= 2L) {
          p_name <- grep("Pr\\(", names(tab), value = TRUE)
          row$reduced <- reduced$id
          row$full <- full$id
          row$df <- as.integer(abs(stats::df.residual(reduced$fit) - stats::df.residual(full$fit)))
          row$df1 <- row$df
          row$statistic <- if ("Deviance" %in% names(tab)) {
            as.numeric(tab$Deviance[[2L]]) / summary(full$fit)$dispersion
          } else NA_real_
          row$p_value <- if (length(p_name)) as.numeric(tab[[p_name[[1L]]]][[2L]]) else NA_real_
          row$available <- is.finite(row$statistic) && is.finite(row$p_value)
          row$reason <- if (row$available) "R: stats::anova(..., test = 'Chisq')" else
            "R did not return an estimable likelihood-ratio test."
        }
      }
    }
    tests[[i]] <- row
  }
  comparison_tests <- if (length(tests)) do.call(rbind, tests) else data.frame(
    reduced = character(), full = character(), link = character(), method = character(),
    statistic_label = character(), df = integer(), df1 = numeric(), df2 = numeric(),
    statistic = numeric(), p_value = numeric(),
    available = logical(), reason = character(), stringsAsFactors = FALSE)
  .rls_state$generalized_comparison_sequence <-
    (.rls_state$generalized_comparison_sequence %||% 0) + 1
  comparison_id <- if (is.character(.native_id) && length(.native_id) == 1L &&
                       !is.na(.native_id) && nzchar(.native_id)) .native_id else
    paste0(if (expected_binary) "binary_comparison_" else if (expected_count) "count_comparison_" else paste0(model_type, "_comparison_"),
           format(Sys.time(), "%Y%m%d%H%M%OS3"),
           "_", .rls_state$generalized_comparison_sequence)
  inputs <- .rls_generalized_comparison_inputs(records, versions)
  verification <- .rls_generalized_comparison_verification_r_code(records, comparison_tests, inputs$model_rows)
  result <- structure(list(response = base$response,
    id = comparison_id,
    group = base$group,
    event = if (expected_binary) base$event else NULL,
    reference = if (expected_binary) base$reference else NULL,
    common_rows = if (mi_comparison) {
      all(vapply(rows_by_imputation, identical, logical(1L), rows_by_imputation[[1L]]))
    } else {
      all(vapply(records, function(x) identical(sort(x$rows_used), sort(base$rows_used)), logical(1L)))
    },
    data_scope = inputs$scope,
    rows_used = inputs$used,
    rows_used_by_imputation = if (mi_comparison) rows_by_imputation[[1L]] else NULL,
    analysis_backend = if (mi_comparison) "multiple_imputation" else "ordinary",
    pooling_method = if (mi_comparison && nrow(comparison_tests))
      paste(unique(comparison_tests$method), collapse = "; ") else NULL,
    models = model_table, tests = comparison_tests),
    class = if (expected_binary)
      c("rlispstat_binary_regression_comparison", "rlispstat_generalized_linear_model_comparison")
    else if (expected_count)
      c("rlispstat_count_regression_comparison", "rlispstat_generalized_linear_model_comparison")
    else "rlispstat_generalized_linear_model_comparison")
  result <- .rls_attach_analysis_provenance(
    result,
    .rls_generalized_comparison_executed_r_code(records),
    title = .rls_model_comparison_title(model_type, mi_comparison),
    dataset_version = versions[[1L]],
    verification_code = list(comparison = verification$code, models = verification$code),
    verification_variables = verification$variables,
    verification_warnings = verification$warnings,
    output_code = list(
      comparison = paste(
        "comparison_names <- ls(pattern = '^comparison_[0-9]+_[0-9]+$')",
        "comparison_results <- mget(comparison_names, inherits = FALSE)",
        sep = "\n"
      ),
      models = paste(
        "model_summaries <- lapply(models, function(x) {",
        "  if (is.list(x) && !is.null(x$pooled)) x$pooled else summary(x)",
        "})",
        sep = "\n"
      )
    )
  )
  result$analysis_provenance$frozen_scope <- TRUE
  result$analysis_provenance$object_type <- base$analysis_provenance$object_type %||% if (mi_comparison) "multiple_imputation" else "data.frame"
  result$analysis_provenance$imputation_count <- inputs$imputation_count
  prepared_path <- tempfile("linkeda-model-comparison-", fileext = ".rds")
  saveRDS(inputs$data, prepared_path)
  result$analysis_provenance$prepared_data_path <- prepared_path
  result$analysis_provenance$missing_information <- .rls_mi_missing_information(list(models=records))
  if (nrow(result$analysis_provenance$missing_information)) {
    # Reuse each immutable model's public verification recipe. Keeping each
    # recipe local avoids accidentally reporting only the last pooled model.
    checks <- vapply(seq_along(records), function(i) {
      recipe <- records[[i]]$analysis_provenance$verification_r_code$model
      if (is.null(recipe) || !nzchar(recipe)) stop("A pooled comparison requires a verification recipe for every model.", call.=FALSE)
      input <- paste0("local({ sets <- model_data[[", i, "]]; do.call(rbind, lapply(seq_along(sets), function(j) data.frame(.imp=j, .id=seq_len(nrow(sets[[j]])), sets[[j]]))) })")
      recipe <- gsub("base::readRDS(verification_data_path)", input, recipe, fixed=TRUE)
      paste0("cat(", .rls_r_string_literal(paste0("Model ",i," \u2014 pooled coefficient diagnostics\n")), ")\nlocal({\n",recipe,"\n})")
    }, character(1L))
    result$analysis_provenance$verification_r_code$missing_information <- paste(c(
      "# Checks coefficient diagnostics for each model; not the between-model hypothesis tests.",
      verification$code, checks),collapse="\n\n")
    result$analysis_provenance$verification_variables <- unique(unlist(lapply(records,
      function(x)x$analysis_provenance$verification_variables),use.names=FALSE))
    result$analysis_provenance$verification_warnings <- unique(unlist(lapply(records,
      function(x)x$analysis_provenance$verification_warnings),use.names=FALSE))
  }
  if (isTRUE(native)) {
    payload <- c("GENERALIZED_COMPARISON_OPEN_STRUCTURED", comparison_id, base$group,
                 base$response,
                 if (expected_binary) as.character(base$event %||% "1") else "",
                 if (expected_binary) as.character(base$reference %||% "0") else "",
                 if (isTRUE(result$common_rows)) "TRUE" else "FALSE",
                 as.character(nrow(model_table)))
    for (i in seq_len(nrow(model_table))) {
      adjacent <- if (i > 1L) result$tests[i - 1L, , drop = FALSE] else NULL
      available <- !is.null(adjacent) && isTRUE(adjacent$available[[1L]])
      native_label <- if (length(.native_labels) >= i && nzchar(as.character(.native_labels[[i]])))
        as.character(.native_labels[[i]]) else paste("Model", i)
      payload <- c(payload, .rls_native_wire_value(model_table$model[[i]]), .rls_native_wire_value(native_label),
        .rls_native_wire_value(paste0(model_table$family[[i]], "|", model_table$link[[i]])),
        as.character(model_table$n[[i]]), .rls_native_wire_number(model_table$logLik[[i]]),
        .rls_native_wire_number(model_table$AIC[[i]]), .rls_native_wire_number(model_table$BIC[[i]]),
        .rls_native_wire_number(model_table$AUC[[i]]),
        .rls_native_wire_value(model_table$terms[[i]]), if (available) "TRUE" else "FALSE",
        if (available) as.character(adjacent$df[[1L]]) else "0",
        if (available) .rls_native_wire_number(adjacent$statistic[[1L]]) else "NA",
        if (available) .rls_native_wire_number(adjacent$p_value[[1L]]) else "NA",
        .rls_native_wire_value(if (is.null(adjacent)) "Baseline model" else adjacent$reason[[1L]]))
    }
    # Full per-model semantic/result extension.  Keeping it after the legacy
    # model records makes the protocol safe for an already-loaded older native
    # executable while allowing the shared comparison UI to preserve type,
    # centering and reference-level edits independently in every model.
    identity_v5 <- length(.native_specification_revisions) == length(records) &&
      length(.native_specification_fingerprints) == length(records) &&
      all(is.finite(as.integer(.native_specification_revisions))) &&
      all(as.integer(.native_specification_revisions) >= 1L) &&
      all(nzchar(as.character(.native_specification_fingerprints)))
    has_offset <- any(vapply(records, function(record)
      nzchar(record$offset_variable %||% ""), logical(1L)))
    payload <- c(payload, if (identity_v5) {
      if (has_offset) "GENERALIZED_COMPARISON_MODELS_V8" else "GENERALIZED_COMPARISON_MODELS_V7"
    } else if (has_offset) "GENERALIZED_COMPARISON_MODELS_V9" else "GENERALIZED_COMPARISON_MODELS_V4",
                 .rls_native_wire_integer(.native_generation),
                 if (expected_count) "TRUE" else "FALSE",
                 if (mi_comparison) "TRUE" else "FALSE",
                 if (identity_v5) .rls_native_wire_value(model_type) else NULL,
                 as.character(length(records)))
    for (record_index in seq_along(records)) {
      record <- records[[record_index]]
      capabilities <- model_capabilities[[record_index]]
      type_values <- record$term_types %||% list()
      type_payload <- as.character(length(type_values))
      if (length(type_values)) for (term in names(type_values)) {
        type_payload <- c(type_payload, .rls_native_wire_value(term),
                          .rls_native_wire_value(type_values[[term]]))
      }
      centered <- unique(as.character(record$centered_predictors %||% character()))
      centered <- centered[nzchar(centered)]
      reference_values <- record$factor_reference_levels %||% list()
      reference_payload <- as.character(length(reference_values))
      if (length(reference_values)) for (term in names(reference_values)) {
        reference_payload <- c(reference_payload, .rls_native_wire_value(term),
                               .rls_native_wire_value(reference_values[[term]]))
      }
      adjacent <- if (record_index > 1L) result$tests[record_index - 1L, , drop = FALSE] else NULL
      comparison_method <- if (is.null(adjacent)) "" else as.character(adjacent$method[[1L]] %||% "")
      comparison_df2 <- if (is.null(adjacent)) NA_real_ else as.numeric(adjacent$df2[[1L]] %||% NA_real_)
      specification_revision <- if (length(.native_specification_revisions) >= record_index)
        as.integer(.native_specification_revisions[[record_index]]) else 0L
      specification_fingerprint <- if (length(.native_specification_fingerprints) >= record_index)
        as.character(.native_specification_fingerprints[[record_index]]) else ""
      payload <- c(payload,
        .rls_native_wire_value(record$id),
        .rls_native_wire_value(record$response),
        .rls_native_wire_value(record$family),
        .rls_native_wire_value(record$link),
        .rls_native_wire_value(record$scope %||% "all"),
        if (isTRUE(record$count_regression)) "TRUE" else "FALSE",
        .rls_native_wire_value(record$count_distribution %||% "poisson"),
        .rls_native_wire_value(record$exposure %||% ""),
        if (has_offset) .rls_native_wire_value(record$offset_variable %||% "") else character(),
        if (identity_v5) c(
          .rls_native_wire_value(record$trials_variable %||% ""),
          .rls_native_wire_number(record$trials_constant %||% NA_real_)
        ) else character(),
        if (isTRUE(capabilities$has_likelihood)) "TRUE" else "FALSE",
        if (isTRUE(capabilities$has_aic)) "TRUE" else "FALSE",
        if (isTRUE(capabilities$supports_nested_lr)) "TRUE" else "FALSE",
        .rls_native_wire_value(comparison_method),
        .rls_native_wire_number(comparison_df2),
        if (identity_v5) c(.rls_native_wire_integer(specification_revision),
                           .rls_native_wire_value(specification_fingerprint)) else character(),
        as.character(length(record$terms)), .rls_native_wire_value(record$terms),
        type_payload,
        as.character(length(centered)), .rls_native_wire_value(centered),
        reference_payload,
        .rls_native_generalized_state_payload(record)
      )
    }
    payload <- c(payload, .rls_analysis_provenance_payload(result))
    if (.Platform$OS.type == "windows") {
      .rls_send_winui(payload)
    } else {
      .rls_send(payload)
    }
  }
  result
}
