# Enrich existing inferential results; never regrid or replace their statistics.
.rls_pairwise_scale_details <- function(tab, grid, contrast, confidence_level = .95,
                                        response_table = NULL, mean_odds = FALSE) {
  if (!nrow(tab)) return(tab)
  link_result <- .rls_emmeans_standardize_table(
    summary(contrast, type = "link", infer = c(TRUE, TRUE), level = confidence_level))
  means <- if (is.null(response_table)) .rls_emmeans_standardize_table(
    summary(grid, type = "response", infer = c(TRUE, FALSE), level = confidence_level)) else response_table
  coefficients <- stats::coef(contrast)
  weights <- coefficients[grep("^c\\.[0-9]+$", names(coefficients))]
  if (length(weights) != nrow(tab) || nrow(means) != nrow(coefficients))
    stop("Pairwise means and contrast coefficients are not aligned.", call. = FALSE)
  # Use emmeans' coefficients, not the printed 'A - B' label: category names
  # can themselves contain minus signs, parentheses or separator characters.
  first <- vapply(weights, function(w) { i <- which(w == 1); if(length(i)==1L) i else NA_integer_ }, integer(1L))
  second <- vapply(weights, function(w) { i <- which(w == -1); if(length(i)==1L) i else NA_integer_ }, integer(1L))
  if (anyNA(c(first, second)) || any(vapply(weights, function(w) sum(w != 0) != 2L, logical(1L))))
    stop("Response differences require a simple first-minus-second pairwise contrast.", call. = FALSE)
  primary <- setdiff(names(grid@levels), grid@misc$by.vars)
  if (length(primary)==1L && primary %in% names(coefficients)) {
    tab$category_first <- as.character(coefficients[[primary]][first])
    tab$category_second <- as.character(coefficients[[primary]][second])
  }
  tab$mean_first <- means$pooled_estimate[first]
  tab$mean_second <- means$pooled_estimate[second]
  tab$response_difference <- tab$mean_first - tab$mean_second
  for (name in c("estimate", "std_error", "df", "statistic", "p_value", "conf_low", "conf_high"))
    tab[[paste0("link_", name)]] <- link_result[[paste0("pooled_", name)]]
  tran <- grid@misc$tran
  link <- if (is.null(tran)) "identity" else if (is.character(tran)) tran[[1L]] else "other"
  tab$link_name <- link
  tab$contrast_scale <- if (identical(link, "identity")) "response (= link)" else "link"
  tab$effect_label <- if (identical(link, "log")) "Ratio" else if (identical(link, "logit")) {
    if (isTRUE(mean_odds)) "Ratio of mean odds" else "Odds ratio"
  } else ""
  if (link %in% c("log", "logit")) {
    tab$transformed_effect <- exp(tab$link_estimate)
    tab$transformed_conf_low <- exp(tab$link_conf_low)
    tab$transformed_conf_high <- exp(tab$link_conf_high)
  }
  tab
}

.rls_pairwise_scale_notes <- function(tab, adjusted = TRUE) {
  if (!nrow(tab) || !"link_name" %in% names(tab)) return(character())
  identity <- identical(tab$link_name[[1L]], "identity")
  conditional_median <- "response_measure" %in% names(tab) &&
    identical(tab$response_measure[[1L]], "conditional median")
  c(if (conditional_median) "Conditional medians: response scale" else
      "Estimated marginal means: response scale",
    paste0("Statistical contrasts: ", if (identity) "response scale (= identity link)" else
      paste0("link scale (", tab$link_name[[1L]], ")")),
    if (nzchar(tab$effect_label[[1L]])) paste("Transformed contrast:", tolower(tab$effect_label[[1L]])),
    if (identity) paste0("Note: response difference is first mean minus second mean; SE, df, statistic, ", if (adjusted) "adjusted p" else "p", " and intervals describe this same inferential contrast.") else paste0("Note: response difference is first ", if (conditional_median) "conditional median minus second conditional median" else "response mean minus second response mean", "; it is descriptive. SE, df, statistic, ", if (adjusted) "adjusted p" else "p", " and link-scale intervals belong to the inferential contrast."),
    if (nzchar(tab$effect_label[[1L]])) "Note: transformed effects and their intervals are exp(link contrast) and exp(link interval); orientation is first / second. These are not confidence intervals for the response difference.",
    if (identical(tab$effect_label[[1L]], "Ratio of mean odds")) "Note: in beta regression, mean odds are mu / (1 - mu), where mu is the fitted mean of the bounded response; these are not binary-event odds ratios.",
    attr(tab,"scale_note"))
}

.rls_numeric_identity_labels <- function(values) {
  values <- as.numeric(values)
  distinct <- unique(values[is.finite(values)])
  for (digits in 2L:10L) {
    labels <- sprintf(paste0("%.", digits, "f"), values)
    if (!anyDuplicated(labels[match(distinct, values)])) return(labels)
  }
  as.character(values)
}

.rls_pairwise_scale_table_lines <- function(tab, confidence_level=.95,
                                            conditioning=character(), adjusted=TRUE) {
  if (!nrow(tab) || !"link_name" %in% names(tab)) return(character())
  label <- paste0(format(100*confidence_level,trim=TRUE),"%")
  identity <- identical(tab$link_name[[1L]], "identity")
  title <- if(identity) "Response difference" else if(tab$link_name[[1L]]=="log") "Log ratio" else
    if(tab$link_name[[1L]]=="logit") {
      if (identical(tab$effect_label[[1L]], "Ratio of mean odds"))
        "Log ratio of mean odds" else "Log odds ratio"
    } else "Link-scale estimate"
  desc <- c(conditioning,"contrast","mean_first","mean_second")
  conditional_median <- "response_measure" %in% names(tab) &&
    identical(tab$response_measure[[1L]], "conditional median")
  headers <- c(conditioning,"Comparison",
    if (conditional_median) "Median first (response)" else "Mean first (response)",
    if (conditional_median) "Median second (response)" else "Mean second (response)")
  if (!identity) {desc<-c(desc,"response_difference");headers<-c(headers,
    if (conditional_median) "Median difference" else "Response difference")}
  if (nzchar(tab$effect_label[[1L]])) {
    desc<-c(desc,"transformed_effect","transformed_conf_low","transformed_conf_high")
    headers<-c(headers,tab$effect_label[[1L]],paste(tab$effect_label[[1L]],"lower",label),paste(tab$effect_label[[1L]],"upper",label))
  }
  df <- any(is.finite(tab$link_df))
  fields<-c(desc,"link_estimate","link_std_error",if(df)"link_df","link_statistic","link_p_value","link_conf_low","link_conf_high")
  headers<-c(headers,title,"SE",if(df)"df","Statistic",if(adjusted)"Adjusted p" else "p",
    paste("Lower",label,if(!identity)"link scale" else "response"),paste("Upper",label,if(!identity)"link scale" else "response"))
  numeric_condition_labels <- lapply(conditioning, function(name) {
    if (is.numeric(tab[[name]])) .rls_numeric_identity_labels(tab[[name]]) else NULL
  })
  names(numeric_condition_labels) <- conditioning
  rows<-vapply(seq_len(nrow(tab)),function(i) paste(vapply(fields,function(nm){
    x<-tab[[nm]][[i]]
    if (nm %in% conditioning && !is.null(numeric_condition_labels[[nm]]))
      return(numeric_condition_labels[[nm]][[i]])
    if(is.character(x)||is.factor(x)) return(.rls_post_estimation_wire_text(as.character(x)))
    if(!is.finite(x))return(if(is.infinite(x))"Inf" else "\u2014")
    if(nm=="link_df" && abs(x)>=1e6)return(sprintf("%.3g",x))
    if(nm=="link_p_value")return(if(x<.001)"< .001" else sub("^0","",sprintf("%.3f",x)))
    sprintf("%.4f",x)
  },character(1L)),collapse="\t"),character(1L))
  c("Pairwise comparisons",paste(headers,collapse="\t"),rows,"")
}

.rls_pairwise_verification_recipe <- function(record, term, adjust, confidence_level, by=NULL, at=NULL, response_code=NULL, multiplier=1, interactions=FALSE, link_grid=FALSE) {
  provenance <- record$analysis_provenance
  if (is.null(provenance)) return(NULL)
  base <- provenance$verification_r_code$model %||% provenance$verification_r_code$table
  if (is.null(base)) return(NULL)
  literal <- function(x) paste(deparse(x, width.cutoff=500L),collapse="\n")
  reference_fit <- .rls_regression_post_estimation_fits(record)[[1L]]
  offset_arg <- if (!is.null(stats::model.offset(stats::model.frame(reference_fit))))
    ", offset=0" else ""
  mode_arg <- if (isTRUE(link_grid) && inherits(reference_fit, "betareg"))
    ", mode='link'" else ""
  interaction_mode_arg <- if (inherits(reference_fit, "betareg"))
    ", mode='link'" else ""
  paste(c(base,
    "# EMMs are back-transformed after averaging; inference stays on the link grid.",
    "grid_fits <- if(exists('reference_fits',inherits=FALSE)) reference_fits else if(exists('fits',inherits=FALSE)) fits else if(exists('reference_model',inherits=FALSE)) list(reference_model) else list(fit)",
    "target <- if(length(grid_fits)>1L) mice::as.mira(grid_fits) else grid_fits[[1L]]",
    paste0("grid <- tryCatch(emmeans::emmeans(target, specs=",literal(term),", by=",literal(by),", at=",literal(at),offset_arg,mode_arg,"), error=function(e) emmeans::emmeans(target, specs=",literal(term),", by=",literal(by),", at=",literal(at),offset_arg,mode_arg,", data=stats::model.frame(grid_fits[[1L]])))"),
    if(identical(record$family %||% "","lognormal")) "grid <- stats::update(grid,tran='log')",
    "response_means <- as.data.frame(summary(grid,type='response'))",
    response_code,
    paste0("pairwise <- emmeans::contrast(grid,'pairwise',adjust=",literal(adjust),")"),
    paste0("reference <- as.data.frame(summary(pairwise,type='link',infer=c(TRUE,TRUE),level=",literal(confidence_level),"))"),
    "weights <- coef(pairwise); weights <- weights[grep('^c[.][0-9]+$',names(weights))]",
    "mean_column <- intersect(c('response','prob','rate','emmean'),names(response_means))[[1L]]",
    paste0("response_means[[mean_column]] <- response_means[[mean_column]] * ",literal(multiplier)),
    "reference$mean_first <- vapply(weights,function(w) response_means[[mean_column]][which(w==1)],numeric(1L))",
    "reference$mean_second <- vapply(weights,function(w) response_means[[mean_column]][which(w== -1)],numeric(1L))",
    "reference$response_difference <- reference$mean_first-reference$mean_second",
    "if(identical(grid@misc$tran,'log') || identical(grid@misc$tran,'logit')) {",
    "  reference$transformed_effect <- exp(reference$estimate)",
    "  reference$transformed_lower <- exp(reference[[intersect(c('lower.CL','asymp.LCL'),names(reference))[[1L]]]])",
    "  reference$transformed_upper <- exp(reference[[intersect(c('upper.CL','asymp.UCL'),names(reference))[[1L]]]])",
    "}",
    if (interactions && length(by)) c(
      "# Complete differences of simple contrasts, with full within-fit covariance.",
      "interaction_tables <- lapply(grid_fits, function(fit) {",
      paste0("  g <- emmeans::emmeans(fit,specs=",literal(term),",by=",literal(by),offset_arg,interaction_mode_arg,",data=stats::model.frame(fit))"),
      if (inherits(reference_fit, "glm"))
        "  if(!is.null(g@misc$tran) && !identical(g@misc$tran,'identity')) g <- emmeans::regrid(g,transform='response')",
      "  first <- emmeans::contrast(g,'pairwise',adjust='none')",
      paste0("  as.data.frame(summary(emmeans::contrast(first,'pairwise',by=",literal(c("contrast",by[-1L])),",adjust='none'),infer=c(TRUE,TRUE),level=",literal(confidence_level),"))"),
      "})",
      "interaction_reference <- interaction_tables[[1L]]",
      "if(length(grid_fits)>1L) for(i in seq_len(nrow(interaction_reference))) {",
      "  q <- vapply(interaction_tables,function(x)x$estimate[i],numeric(1L))",
      "  u <- vapply(interaction_tables,function(x)x$SE[i]^2,numeric(1L))",
      "  dfs <- vapply(interaction_tables,function(x)x$df[i],numeric(1L))",
      "  dfcom <- if(any(is.finite(dfs)&dfs>0)) min(dfs[is.finite(dfs)&dfs>0]) else Inf",
      "  pooled <- mice::pool.scalar(q,u,n=dfcom+1,k=1,rule='rubin1987')",
      "  interaction_reference$estimate[i] <- pooled$qbar; interaction_reference$SE[i] <- sqrt(pooled$t)",
      "  interaction_reference$df[i] <- pooled$df",
      "  interaction_reference$p.value[i] <- 2*pt(-abs(pooled$qbar/sqrt(pooled$t)),df=pooled$df)",
      paste0("  ci <- pooled$qbar+qt(c(",literal((1-confidence_level)/2),",",literal((1+confidence_level)/2),"),df=pooled$df)*sqrt(pooled$t)"),
      "  interaction_reference[i,intersect(c('lower.CL','asymp.LCL'),names(interaction_reference))] <- ci[1L]",
      "  interaction_reference[i,intersect(c('upper.CL','asymp.UCL'),names(interaction_reference))] <- ci[2L]",
      "}","print(interaction_reference)"),
    "print(response_means)","reference"),collapse="\n")
}

.rls_regression_post_estimation_fits <- function(record) {
  fits <- record$fits_by_imputation %||% list()
  fits <- fits[!vapply(fits, is.null, logical(1L))]
  if (!length(fits) && !is.null(record$fit)) fits <- list(record$fit)
  if (!length(fits)) stop("Fit the model before requesting post-estimation results.", call. = FALSE)
  fits
}

.rls_regression_emmeans_target <- function(fits) {
  if (length(fits) == 1L) return(fits[[1L]])
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop(.rls_mice_missing_message, call. = FALSE)
  }
  mice::as.mira(fits)
}

.rls_regression_post_estimation_metadata <- function(fits) {
  list(
    multiple_imputation = length(fits) > 1L,
    imputation_count = length(fits),
    pooling_method = if (length(fits) > 1L) {
      "emmeans support for mice::mira (Rubin-combined reference grid)"
    } else {
      "emmeans on the fitted R model"
    }
  )
}

.rls_emmeans_with_fitted_data <- function(operation, object, data, ...) {
  args <- c(list(object = object), list(...))
  # EMMs for offset models use a common zero offset: unit exposure for
  # log-exposure count models. Averaging the fitted offset would silently
  # turn a reported rate into a count at the sample's mean exposure.
  if (!"offset" %in% names(args) && !is.null(stats::model.offset(data))) {
    args$offset <- 0
  }
  tryCatch(
    do.call(operation, args),
    error = function(original_error) {
      retry <- tryCatch(
        do.call(operation, c(args, list(data = data))),
        error = function(error) error
      )
      if (inherits(retry, "error")) stop(original_error)
      retry
    }
  )
}

.rls_regression_partial_residual <- function(fit, residual_type = "default") {
  residual_type <- as.character(residual_type %||% "default")[[1L]]
  is_glm <- inherits(fit, "glm")
  if (identical(residual_type, "default")) residual_type <- if (is_glm) "working" else "raw"
  if (identical(residual_type, "raw")) return(unname(stats::residuals(fit, type = "response")))
  if (identical(residual_type, "standardized")) {
    return(unname(if (is_glm) stats::rstandard(fit, type = "deviance") else stats::rstandard(fit)))
  }
  if (identical(residual_type, "studentized")) return(unname(stats::rstudent(fit)))
  if (residual_type %in% c("deviance", "pearson", "working", "response")) {
    return(unname(stats::residuals(fit, type = residual_type)))
  }
  stop(sprintf("Unsupported residual type `%s`.", residual_type), call. = FALSE)
}

# The mean/conditional design matrix works for engines without predict(type="terms")
# (notably glmmTMB and betareg). Keep lm's established term centering convention.
.rls_regression_term_contribution <- function(fit, term) {
  hurdle <- isTRUE(attr(fit, "linkeda_ceiling_hurdle"))
  design <- if (hurdle && !is.null(fit$linkeda_model_data)) {
    stats::model.matrix(stats::delete.response(stats::terms(fit)), fit$linkeda_model_data)
  } else stats::model.matrix(fit)
  labels <- attr(stats::terms(fit), "term.labels")
  index <- match(gsub("`", "", term, fixed = TRUE),
                 gsub("`", "", labels, fixed = TRUE))
  columns <- which(attr(design, "assign") == index)
  if (is.na(index) || !length(columns)) {
    stop(sprintf("The fitted R model has no term contribution for `%s`.", term), call. = FALSE)
  }
  beta <- if (hurdle) .rls_hurdle_component_coefficients(fit, "below_ceiling") else
    if (inherits(fit, "glmmTMB")) glmmTMB::fixef(fit)$cond else
    if (inherits(fit, "betareg")) stats::coef(fit, model = "mean") else
    if (inherits(fit, "gamlss")) stats::coef(fit, what = "mu") else stats::coef(fit)
  beta <- beta[colnames(design)[columns]]
  # Aliased columns contribute zero, as in predict.lm(type = "terms").
  beta[is.na(beta)] <- 0
  block <- design[, columns, drop = FALSE]
  if (isTRUE(attr(stats::terms(fit), "intercept") == 1L)) {
    block <- sweep(block, 2L, colMeans(block), "-")
  }
  setNames(as.numeric(block %*% beta), rownames(design))
}

# Transform both coordinates of a linear component-plus-residual plot together.
# Scaling only the residual mixes units and artificially tightens the cloud.
.rls_regression_partial_linear_scale <- function(fit, residual_type) {
  if (!inherits(fit, "lm") || inherits(fit, "glm") ||
      !residual_type %in% c("standardized", "studentized")) return(NULL)
  influence <- stats::lm.influence(fit, do.coef = FALSE)
  sigma <- if (identical(residual_type, "studentized")) influence$sigma else
    sqrt(stats::deviance(fit) / stats::df.residual(fit))
  weights <- stats::weights(fit) %||% rep(1, length(influence$hat))
  sigma * sqrt(1 - influence$hat) / sqrt(weights)
}

.rls_regression_partial_plot_record <- function(record, term, residual_type = "default") {
  term <- .rls_validate_protocol_name(term, "term")
  terms <- record$predictors %||% record$terms %||% character()
  if (!term %in% terms) stop("The requested partial plot term is not included in the fitted model.", call. = FALSE)
  fits <- .rls_regression_post_estimation_fits(record)
  actual_type <- as.character(residual_type %||% "default")[[1L]]
  if (actual_type == "response") actual_type <- "raw"
  if (identical(actual_type, "default")) {
    discrete <- .rls_discrete_diagnostic_capabilities(record)
    actual_type <- if (length(discrete$residuals)) discrete$default_residual else
      if (inherits(fits[[1L]], c("betareg", "gamlss"))) "deviance" else
      if (inherits(fits[[1L]], "glm")) "working" else "raw"
  }
  diagnostic_sets <- record$diagnostics_by_imputation %||% list()
  if (!length(diagnostic_sets) && length(fits) == 1L) {
    diagnostic_sets <- list(record$diagnostics %||% record$diagnostic_data)
  }
  discrete <- .rls_discrete_diagnostic_capabilities(record)
  choices <- if (length(discrete$residuals)) discrete$residuals else
    if (identical(record$family, "beta")) c("deviance", "pearson") else
    if (identical(record$family, "beta_one_inflated")) "deviance" else
    if (!is.null(record$family)) c("deviance", "pearson", "working", "standardized", "studentized") else
    if (inherits(fits[[1L]], "glm")) c("raw", "deviance", "pearson", "working", "standardized", "studentized") else
    c("raw", "standardized", "studentized")
  rows <- lapply(seq_along(fits), function(imputation) {
    fit <- fits[[imputation]]
    contribution <- .rls_regression_term_contribution(fit, term)
    row_id <- suppressWarnings(as.integer(names(contribution)))
    diagnostics <- if (length(diagnostic_sets) >= imputation) diagnostic_sets[[imputation]] else NULL
    column_for_type <- function(type) {
      column <- paste0(type, "_residual")
      # lm diagnostics expose their raw vector as `residual`.
      if (type == "raw" && inherits(fit, "lm") && !inherits(fit, "glm") &&
          is.data.frame(diagnostics) && !column %in% names(diagnostics) &&
          "residual" %in% names(diagnostics)) column <- "residual"
      column
    }
    available <- choices[vapply(choices, function(type) {
      column <- column_for_type(type)
      value <- if (is.data.frame(diagnostics) && column %in% names(diagnostics)) {
        diagnostics[[column]]
      } else tryCatch(.rls_regression_partial_residual(fit, type), error = function(e) numeric())
      length(value) == length(contribution) && any(is.finite(value))
    }, logical(1L))]
    choices <<- intersect(choices, available)
    if (!actual_type %in% available) {
      stop(sprintf("Residual type `%s` is not available for this fitted model.", actual_type), call. = FALSE)
    }
    column <- column_for_type(actual_type)
    if (is.data.frame(diagnostics) && column %in% names(diagnostics)) {
      # Diagnostics retain the original case ids, including MI and scoped fits.
      if (length(row_id) != length(contribution) || anyNA(row_id)) {
        stop("The fitted design has no original row identifiers.", call. = FALSE)
      }
      matched <- match(row_id, diagnostics$row_id)
      if (anyNA(matched)) stop("Term contributions and residuals do not refer to the same fitted rows.", call. = FALSE)
      residual <- diagnostics[[column]][matched]
    } else {
      residual <- .rls_regression_partial_residual(fit, actual_type)
      if (length(row_id) != length(contribution) || anyNA(row_id)) row_id <- seq_along(contribution)
    }
    scale <- .rls_regression_partial_linear_scale(fit, actual_type)
    if (!is.null(scale)) contribution <- contribution / scale
    if (length(contribution) != length(residual)) stop("Term contributions and residuals do not refer to the same fitted rows.", call. = FALSE)
    if (!any(is.finite(contribution) & is.finite(residual))) {
      stop(sprintf("Residual type `%s` is not available for this fitted model.", actual_type), call. = FALSE)
    }
    data.frame(imputation = imputation, row_id = row_id,
               contribution = unname(contribution),
               partial_residual = unname(contribution) + residual, stringsAsFactors = FALSE)
  })
  result <- structure(list(term = term, residual_type = actual_type,
                 available_residual_types = choices, available_terms = terms,
                 contribution_scale = if (!is.null(.rls_regression_partial_linear_scale(fits[[1L]], actual_type))) actual_type else "",
                 imputation_count = length(fits),
                 rows = do.call(rbind, rows)), class = "linkeda_regression_partial_plot")
  if (!is.null(record$analysis_provenance)) {
    result$analysis_provenance <- record$analysis_provenance
    result$analysis_provenance$executed_r_code <- paste(
      result$analysis_provenance$executed_r_code,
      paste0("partial_plot <- LinkEDA:::.rls_regression_partial_plot_record(model_record, ",
             .rls_r_string_literal(term), ", ", .rls_r_string_literal(actual_type), ")"), sep = "\n")
    result$analysis_provenance$verification_r_code$partial <-
      .rls_partial_plot_verification_r_code(record, term, actual_type)
  }
  result
}

.rls_emmeans_standardize_table <- function(value, estimate_candidates = character()) {
  tab <- as.data.frame(value, stringsAsFactors = FALSE)
  first_name <- function(candidates) {
    found <- intersect(candidates, names(tab))
    if (length(found)) found[[1L]] else ""
  }
  estimate_name <- first_name(c(
    estimate_candidates, "estimate", "emmean", "response", "prob", "rate", "trend"
  ))
  statistic_name <- first_name(c("t.ratio", "z.ratio", "statistic"))
  lower_name <- first_name(c("lower.CL", "asymp.LCL", "conf.low"))
  upper_name <- first_name(c("upper.CL", "asymp.UCL", "conf.high"))
  tab$pooled_estimate <- if (nzchar(estimate_name)) as.numeric(tab[[estimate_name]]) else NA_real_
  tab$pooled_std_error <- if ("SE" %in% names(tab)) as.numeric(tab$SE) else NA_real_
  tab$pooled_df <- if ("df" %in% names(tab)) as.numeric(tab$df) else NA_real_
  tab$pooled_statistic <- if (nzchar(statistic_name)) as.numeric(tab[[statistic_name]]) else NA_real_
  tab$pooled_p_value <- if ("p.value" %in% names(tab)) as.numeric(tab$p.value) else NA_real_
  tab$pooled_conf_low <- if (nzchar(lower_name)) as.numeric(tab[[lower_name]]) else NA_real_
  tab$pooled_conf_high <- if (nzchar(upper_name)) as.numeric(tab[[upper_name]]) else NA_real_
  tab
}

.rls_mi_post_estimation_event <- function(record, operation, phase,
    status = "running", elapsed = 0, message = "") {
  if (length(record$fits_by_imputation %||% list()) <= 1L)
    return(invisible(NULL))
  .rls_mi_progress_event(
    paste0(record$id %||% "generalized-model", "-", operation),
    record$group %||% "", phase, status = status,
    completed = as.integer(identical(status, "completed")), total = 1L,
    running = as.integer(identical(status, "running")),
    elapsed = elapsed, message = message
  )
}

.rls_pairwise_validate_scale <- function(record, scale) {
  bounded_count <- isTRUE(record$count_regression) &&
    .rls_count_distribution_uses_trials(record$count_distribution)
  binomial <- isTRUE(record$binary_regression) || bounded_count ||
    isTRUE(record$family %in% c("binomial", "quasibinomial"))
  lognormal <- identical(record$family, "lognormal")
  link <- record$link %||% if (inherits(record$fit, "glm"))
    stats::family(record$fit)$link else "identity"
  allowed <- c("response", "link")
  if (binomial) allowed <- c(allowed, "probability")
  if (binomial && identical(link, "logit")) allowed <- c(allowed, "odds_ratio")
  if (binomial && identical(link, "log")) allowed <- c(allowed, "risk_ratio")
  if (!binomial && !lognormal && identical(link, "log"))
    allowed <- c(allowed, "mean_ratio")
  if (lognormal) allowed <- c(allowed, "multiplicative_ratio")
  if (!scale %in% allowed) {
    stop(sprintf("Scale `%s` is not valid for this model; available scales: %s.",
      scale, paste(allowed, collapse = ", ")), call. = FALSE)
  }
  invisible(scale)
}

.rls_regression_pairwise_record_impl <- function(record, term, scale = "response",
                                            adjust = "tukey", confidence_level = .95) {
  operation_started <- proc.time()[["elapsed"]]
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop("Pairwise comparisons require the suggested R package `emmeans`.", call. = FALSE)
  }
  term <- .rls_validate_protocol_name(term, "term")
  terms <- record$predictors %||% record$terms %||% character()
  if (!term %in% terms || length(.rls_model_interaction_parts(term)) != 1L) {
    stop("Pairwise comparisons are available only for included simple terms.", call. = FALSE)
  }
  fits <- .rls_regression_post_estimation_fits(record)
  reference_data <- stats::model.frame(fits[[1L]])
  if (!term %in% names(reference_data) || !is.factor(reference_data[[term]])) {
    stop("Pairwise comparisons require a Categorical or Ordinal term.", call. = FALSE)
  }
  scale <- match.arg(scale, c(
    "response", "link", "probability", "odds_ratio", "risk_ratio",
    "mean_ratio", "multiplicative_ratio"
  ))
  .rls_pairwise_validate_scale(record, scale)
  .rls_mi_post_estimation_event(record, "pairwise", "emmeans",
    message = "Computing estimated marginal means in R\u2026")
  emmeans_started <- proc.time()[["elapsed"]]
  target <- .rls_regression_emmeans_target(fits)
  # `mice::as.mira()` retains the fitted calls, but bounded-response engines
  # such as betareg may refer there to the per-imputation local object
  # (`data = completed`). That object no longer exists when emmeans tries to
  # recover the reference grid. Preserve emmeans' ordinary mira path whenever
  # it works and use the fitted model frame only as a reconstruction fallback.
  grid_args <- list(operation = emmeans::emmeans, object = target,
                    data = reference_data, specs = term)
  if (inherits(fits[[1L]], "betareg")) grid_args$mode <- "link"
  grid <- do.call(.rls_emmeans_with_fitted_data, grid_args)
  lognormal <- identical(record$family %||% "", "lognormal")
  if (lognormal) grid <- stats::update(grid, tran = "log")
  link_grid <- grid
  if (scale %in% c("response", "probability")) {
    grid <- .rls_mi_regrid(grid, transform = "response")
  }
  emmeans_seconds <- unname(proc.time()[["elapsed"]] - emmeans_started)
  .rls_mi_post_estimation_event(record, "pairwise", "pairwise",
    elapsed = proc.time()[["elapsed"]] - operation_started,
    message = sprintf("Estimated marginal means completed in %.2f s. Computing pairwise comparisons\u2026",
      emmeans_seconds))
  pairwise_started <- proc.time()[["elapsed"]]
  contrast <- emmeans::contrast(grid, method = "pairwise", adjust = adjust)
  tab <- .rls_emmeans_standardize_table(
    summary(contrast, infer = c(TRUE, TRUE), level = confidence_level),
    estimate_candidates = "estimate"
  )
  pairwise_seconds <- unname(proc.time()[["elapsed"]] - pairwise_started)
  .rls_mi_post_estimation_event(record, "pairwise", "formatting",
    elapsed = proc.time()[["elapsed"]] - operation_started,
    message = sprintf("Pairwise comparisons completed in %.2f s. Formatting\u2026",
      pairwise_seconds))
  formatting_started <- proc.time()[["elapsed"]]
  metadata <- .rls_regression_post_estimation_metadata(fits)
  attr(tab,"confidence_level") <- confidence_level
  tab$term <- term
  tab$scale <- scale
  tab$response_measure <- if (lognormal) "conditional median" else "mean"
  tab$adjustment <- adjust
  tab$imputation_count <- metadata$imputation_count
  tab$pooling_method <- metadata$pooling_method
  if (lognormal) {
    tab$response_scale <- if (scale %in% c("response", "probability"))
      "conditional_median_difference" else "log_response"
    tab$pooling_method <- paste(metadata$pooling_method,
      if (scale %in% c("response", "probability"))
        "Differences of conditional medians on the original response scale (not arithmetic means)." else
        "Contrasts on the log-response scale; exponentiated contrasts are multiplicative ratios.")
  }
  if (scale %in% c("odds_ratio", "risk_ratio", "mean_ratio", "multiplicative_ratio")) {
    tab$ratio <- exp(tab$pooled_estimate)
    tab$ratio_conf_low <- exp(tab$pooled_conf_low)
    tab$ratio_conf_high <- exp(tab$pooled_conf_high)
    tab[[scale]] <- tab$ratio
    tab[[paste0(scale, "_conf_low")]] <- tab$ratio_conf_low
    tab[[paste0(scale, "_conf_high")]] <- tab$ratio_conf_high
  }
  attr(tab, "post_estimation") <- metadata
  attr(tab,"missing_information") <- .rls_mi_emmeans_diagnostics(contrast)
  if(nrow(attr(tab,"missing_information")) && !is.null(record$analysis_provenance)) {
    provenance<-record$analysis_provenance
    base<-provenance$verification_r_code$model %||% provenance$verification_r_code$table
    if(is.null(base))base<-provenance$verification_r_code[[1L]]
    recipe<-c(base,
      "fits <- if(exists('reference_fits', inherits=FALSE)) reference_fits else fits",
      paste0("grid <- tryCatch(emmeans::emmeans(mice::as.mira(fits), specs=",.rls_r_string_literal(term),
        if (!is.null(stats::model.offset(reference_data))) ", offset=0" else "",
        if (inherits(fits[[1L]], "betareg")) ", mode='link'" else "", "),"),
      paste0("  error=function(e) emmeans::emmeans(mice::as.mira(fits), specs=",.rls_r_string_literal(term),
        if (!is.null(stats::model.offset(reference_data))) ", offset=0" else "",
        if (inherits(fits[[1L]], "betareg")) ", mode='link'" else "",
        ", data=stats::model.frame(fits[[1]])))"),
      if (lognormal) "grid <- stats::update(grid, tran='log')",
      "m <- grid@dfargs$m; B <- grid@dfargs$B; T <- grid@dfargs$T")
    if(scale %in% c("response","probability"))recipe<-c(recipe,
      "before <- grid; grid <- emmeans::regrid(grid, transform='response')",
      "s <- sqrt(diag(vcov(grid))/diag(vcov(before)))",
      "B <- (before@linfct %*% B %*% t(before@linfct)) * outer(s,s); T <- vcov(grid)")
    recipe<-c(recipe,paste0("contrast <- emmeans::contrast(grid, 'pairwise', adjust=",.rls_r_string_literal(adjust),")"),
      "# FMI uses unadjusted scalar degrees of freedom, independently of multiplicity adjustment.",
      paste0("reference <- as.data.frame(summary(contrast, infer=c(TRUE,TRUE), adjust='none', level=",format(confidence_level,digits=17),"))"),
      "L <- contrast@linfct; b <- rowSums((L %*% B)*L); total <- rowSums((L %*% T)*L)",
      "ubar <- total-(1+1/m)*b; riv <- (1+1/m)*b/ubar",
      "df <- reference$df; if(is.null(df)) df <- Inf",
      "fmi <- (riv+2/(df+3))/(riv+1)",
      "mcse <- sqrt(b/m); mcse_percent_se <- ifelse(total>0,100*mcse/sqrt(total),NA_real_)",
      "data.frame(contrast=reference$contrast,FMI=fmi,RIV=riv,Ubar=ubar,B=b,T=total,m=m,lambda=(1+1/m)*b/total,relative_efficiency=1/(1+fmi/m),mcse=mcse,mcse_percent_se=mcse_percent_se)")
    provenance$verification_r_code$pairwise<-paste(recipe,collapse="\n")
    provenance$output_r_code$pairwise<-"reference"
    provenance$missing_information<-attr(tab,"missing_information")
    # These rows describe contrasts, not the source model's coefficients.
    # Replace the inherited display mapping together with its diagnostic values.
    diagnostics <- provenance$missing_information
    provenance$missing_information_display_rows <- data.frame(
      value_row = seq_len(nrow(diagnostics)) - 1L,
      row_type = "coefficient", label = diagnostics$Variable, variable = term,
      stringsAsFactors = FALSE)
    attr(tab,"analysis_provenance")<-provenance
  }
  if (lognormal && !is.null(record$analysis_provenance)) {
    provenance <- attr(tab, "analysis_provenance") %||% record$analysis_provenance
    if (nrow(attr(tab, "missing_information"))) {
      provenance$verification_r_code$missing_information <- provenance$verification_r_code$pairwise
    }
    provenance$verification_r_code$pairwise <- .rls_lognormal_pairwise_verification_r_code(
      record, term, scale, adjust, confidence_level)
    provenance$output_r_code$pairwise <- "reference"
    attr(tab, "analysis_provenance") <- provenance
  }
  tab <- .rls_pairwise_scale_details(tab, link_grid,
    emmeans::contrast(link_grid, method="pairwise", adjust=adjust), confidence_level,
    mean_odds = inherits(fits[[1L]], "betareg"))
  if (!is.null(stats::model.offset(reference_data))) {
    attr(tab, "scale_note") <- if (isTRUE(record$count_regression) &&
      nzchar(record$exposure %||% ""))
      "Note: estimated means and response differences use unit exposure and zero for any additional offset." else
      "Note: estimated means and response differences hold the fitted offset at zero."
  }
  recipe <- .rls_pairwise_verification_recipe(record, term, adjust, confidence_level,
                                              link_grid = TRUE)
  if (!is.null(recipe)) {
    provenance <- attr(tab,"analysis_provenance") %||% record$analysis_provenance
    if (nrow(attr(tab,"missing_information")))
      provenance$verification_r_code$missing_information <- provenance$verification_r_code$pairwise
    provenance$verification_r_code$pairwise_scales <- recipe
    provenance$output_r_code$pairwise_scales <- "reference"
    attr(tab,"analysis_provenance") <- provenance
  }
  attr(tab, "execution_diagnostics") <- list(
    operation = "pairwise_comparisons",
    imputations = length(fits),
    emmeans_seconds = emmeans_seconds,
    pairwise_contrasts_seconds = pairwise_seconds,
    formatting_seconds = unname(proc.time()[["elapsed"]] - formatting_started),
    total_seconds = unname(proc.time()[["elapsed"]] - operation_started)
  )
  tab
}

.rls_regression_pairwise_record <- function(record, term, scale = "response",
                                            adjust = "tukey", confidence_level = .95) {
  started <- proc.time()[["elapsed"]]
  .rls_mi_post_estimation_event(record, "pairwise", "preparing",
    message = sprintf("Preparing pairwise comparisons for %s\u2026", term))
  tryCatch({
    result <- .rls_regression_pairwise_record_impl(record, term, scale,
      adjust, confidence_level)
    timing <- attr(result, "execution_diagnostics")
    .rls_mi_post_estimation_event(record, "pairwise", "completed",
      status = "completed", elapsed = proc.time()[["elapsed"]] - started,
      message = sprintf(
        "Pairwise comparisons complete. EMMs %.2f s; contrasts %.2f s; total %.2f s.",
        timing$emmeans_seconds, timing$pairwise_contrasts_seconds,
        timing$total_seconds))
    result
  }, error = function(error) {
    .rls_mi_post_estimation_event(record, "pairwise", "failed",
      status = "failed", elapsed = proc.time()[["elapsed"]] - started,
      message = conditionMessage(error))
    stop(error)
  })
}

.rls_interaction_numeric_grid <- function(fits, variable, points = 3L) {
  values <- unlist(lapply(fits, function(fit) {
    data <- stats::model.frame(fit)
    if (variable %in% names(data)) as.numeric(data[[variable]]) else numeric()
  }), use.names = FALSE)
  values <- values[is.finite(values)]
  if (!length(values)) stop(sprintf("`%s` has no finite fitted values.", variable), call. = FALSE)
  if (points == 3L) {
    center <- mean(values)
    spread <- stats::sd(values)
    if (!is.finite(spread) || spread <= 0) spread <- diff(range(values)) / 2
    unique(pmax(min(values), pmin(max(values), c(center - spread, center, center + spread))))
  } else {
    limits <- range(values)
    if (!all(is.finite(limits)) || limits[[1L]] == limits[[2L]]) return(limits[[1L]])
    # A regular sequence is both the conventional grid for a continuous
    # effect curve and numerically safer than pooled empirical quantiles.
    # With multiple imputations, nearly equal quantiles can be distinct as
    # doubles but receive the same formatted level inside emmeans, which
    # causes a duplicated-factor-level error instead of producing a plot.
    seq(limits[[1L]], limits[[2L]], length.out = max(2L, as.integer(points)))
  }
}

.rls_emmeans_test_reference <- function(value) {
  if (is.null(value)) return(NULL)
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || !is.finite(value)) {
    stop("`emm_test_reference` must be NULL or one finite numeric value.", call. = FALSE)
  }
  unname(value)
}

.rls_emmeans_summary <- function(object, confidence_level = .95, type = NULL,
                                 estimate_candidates = character(),
                                 test_reference = 0) {
  test_reference <- .rls_emmeans_test_reference(test_reference)
  args <- list(
    object = object,
    infer = c(TRUE, !is.null(test_reference)),
    level = confidence_level
  )
  if (!is.null(test_reference)) args$null <- test_reference
  if (!is.null(type)) args$type <- type
  tab <- .rls_emmeans_standardize_table(
    do.call(summary, args), estimate_candidates = estimate_candidates
  )
  attr(tab,"missing_information") <- .rls_mi_emmeans_diagnostics(object)
  tab
}

.rls_lognormal_emmeans_summary <- function(fits, specs, by = NULL, at = NULL,
                                            confidence_level = .95) {
  per_imputation <- lapply(fits, function(fit) {
    reference_data <- stats::model.frame(fit)
    args <- list(
      operation = emmeans::emmeans,
      object = fit,
      data = reference_data,
      specs = specs,
      offset = stats::sigma(fit)^2 / 2
    )
    if (length(by)) args$by <- by
    if (length(at)) args$at <- at
    grid <- do.call(.rls_emmeans_with_fitted_data, args)
    # Programmatic lm() calls retain `formula` as the call expression, so
    # emmeans cannot always rediscover log(response) from the call alone.
    # Declare the fitted response transformation explicitly before requesting
    # exact lognormal arithmetic means. emmeans' bias.adjust=TRUE uses a
    # second-order approximation, not exp(eta + sigma^2/2). The public offset
    # argument adds the fitted variance correction before back-transformation.
    # As in emmeans bias adjustment, sigma is treated as a plug-in constant.
    grid <- stats::update(grid, tran = "log")
    .rls_emmeans_standardize_table(
      summary(
        grid, infer = c(TRUE, FALSE), level = confidence_level,
        type = "response", bias.adjust = FALSE
      )
    )
  })
  key_columns <- intersect(unique(c(specs, by, names(at))), names(per_imputation[[1L]]))
  semantic_keys <- function(tab) {
    if (!length(key_columns)) return(rep("", nrow(tab)))
    vapply(seq_len(nrow(tab)), function(row) {
      .rls_interaction_semantic_key(unlist(lapply(key_columns, function(column) {
        c(column, as.character(tab[[column]][[row]]))
      }), use.names = FALSE))
    }, character(1L))
  }
  expected <- semantic_keys(per_imputation[[1L]])
  aligned <- lapply(seq_along(per_imputation), function(imputation) {
    tab <- per_imputation[[imputation]]
    keys <- semantic_keys(tab)
    if (!setequal(keys, expected) || anyDuplicated(keys)) {
      stop(sprintf(
        "Imputation %d produced a different lognormal effect grid.", imputation
      ), call. = FALSE)
    }
    tab[match(expected, keys), , drop = FALSE]
  })
  out <- aligned[[1L]]
  if (length(aligned) > 1L) {
    pools <- lapply(seq_len(nrow(out)), function(row) {
      estimates <- vapply(aligned, function(tab) tab$pooled_estimate[[row]], numeric(1L))
      variances <- vapply(aligned, function(tab) tab$pooled_std_error[[row]]^2, numeric(1L))
      dfs <- vapply(aligned, function(tab) tab$pooled_df[[row]], numeric(1L))
      finite_dfs <- dfs[is.finite(dfs) & dfs > 0]
      .rls_mi_pool_scalar(
        estimates, variances,
        if (length(finite_dfs)) min(finite_dfs) else NA_real_,
        confidence_level
      )
    })
    out$pooled_estimate <- vapply(pools, `[[`, numeric(1L), "Qbar")
    out$pooled_std_error <- vapply(pools, `[[`, numeric(1L), "SE")
    out$pooled_df <- vapply(pools, `[[`, numeric(1L), "df")
    out$pooled_conf_low <- vapply(pools, `[[`, numeric(1L), "CI_low")
    out$pooled_conf_high <- vapply(pools, `[[`, numeric(1L), "CI_high")
    out$pooled_statistic <- NA_real_
    out$pooled_p_value <- NA_real_
    out$response <- out$pooled_estimate
    out$SE <- out$pooled_std_error
    out$df <- out$pooled_df
    out$lower.CL <- out$pooled_conf_low
    out$upper.CL <- out$pooled_conf_high
    attr(out,"missing_information") <- do.call(rbind,lapply(seq_along(pools),function(i)
      .rls_mi_pooling_diagnostics(pools[[i]],length(fits),paste("Marginal mean",i))))
  }
  out$response_scale <- "arithmetic_mean"
  out$back_transformation <- "exp(eta + sigma^2 / 2) within each imputation"
  out
}

.rls_binary_effect_options <- function(quantity = "predicted_probability",
                                       adjustment = "average_sample",
                                       presentation = "percentage") {
  list(
    quantity = match.arg(quantity, c("predicted_probability", "probability_difference")),
    adjustment = match.arg(adjustment, c("average_sample", "reference_profile")),
    presentation = match.arg(presentation, c("percentage", "probability"))
  )
}

.rls_binary_effect_grid <- function(fits, focal, conditioning, grid_points) {
  first <- stats::model.frame(fits[[1L]])
  variables <- c(focal, conditioning)
  values <- lapply(variables, function(variable) {
    column <- first[[variable]]
    if (is.factor(column)) {
      levels(column)
    } else {
      points <- if (identical(variable, focal)) max(3L, as.integer(grid_points)) else 3L
      observed <- unlist(lapply(fits, function(fit) {
        as.numeric(stats::model.frame(fit)[[variable]])
      }), use.names = FALSE)
      observed <- sort(unique(observed[is.finite(observed)]))
      if (identical(variable, focal) && length(observed) > 0L &&
          length(observed) <= points &&
          all(abs(observed - round(observed)) < 1e-8)) {
        # A count-like predictor should be shown only at observed integer values,
        # rather than at fractional points from a continuous curve grid.
        observed
      } else {
        .rls_interaction_numeric_grid(fits, variable, points)
      }
    }
  })
  names(values) <- variables
  grid <- do.call(expand.grid, c(values, list(
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )))
  for (variable in variables) {
    if (is.factor(first[[variable]])) {
      grid[[variable]] <- factor(
        as.character(grid[[variable]]), levels = levels(first[[variable]]),
        ordered = is.ordered(first[[variable]])
      )
    } else {
      grid[[variable]] <- as.numeric(grid[[variable]])
    }
  }
  grid
}

.rls_binary_reference_profile <- function(fit) {
  frame <- stats::model.frame(fit)
  response_index <- attr(stats::terms(fit), "response")
  response_name <- names(frame)[[response_index]]
  predictors <- frame[, setdiff(names(frame), response_name), drop = FALSE]
  profile <- predictors[1L, , drop = FALSE]
  for (name in names(profile)) {
    column <- predictors[[name]]
    if (is.factor(column)) {
      profile[[name]] <- factor(levels(column)[[1L]], levels = levels(column),
                                ordered = is.ordered(column))
    } else if (is.numeric(column)) {
      profile[[name]] <- mean(column, na.rm = TRUE)
    } else if (is.logical(column)) {
      profile[[name]] <- FALSE
    } else {
      observed <- as.character(column[!is.na(column)])
      profile[[name]] <- if (length(observed)) observed[[1L]] else NA_character_
    }
  }
  profile
}

.rls_binary_design_terms <- function(fit) {
  design_terms <- stats::delete.response(stats::terms(fit))
  offset_positions <- attr(design_terms, "offset") %||% integer()
  if (!length(offset_positions)) return(design_terms)
  for (field in c("variables", "predvars")) {
    variables <- attr(design_terms, field)
    if (!is.null(variables)) {
      attr(design_terms, field) <- as.call(
        as.list(variables)[-(offset_positions + 1L)]
      )
    }
  }
  factors <- attr(design_terms, "factors")
  if (!is.null(factors)) {
    removed <- rownames(factors)[offset_positions]
    attr(design_terms, "factors") <- factors[-offset_positions, , drop = FALSE]
    classes <- attr(design_terms, "dataClasses")
    if (!is.null(classes)) {
      attr(design_terms, "dataClasses") <- classes[!names(classes) %in% removed]
    }
  }
  attr(design_terms, "offset") <- NULL
  design_terms
}

.rls_binary_scenario_estimate <- function(fit, scenario, adjustment) {
  frame <- stats::model.frame(fit)
  response_index <- attr(stats::terms(fit), "response")
  response_name <- names(frame)[[response_index]]
  source <- frame[, setdiff(names(frame), response_name), drop = FALSE]
  newdata <- if (identical(adjustment, "average_sample")) {
    source
  } else {
    .rls_binary_reference_profile(fit)
  }
  for (name in names(scenario)) {
    if (!name %in% names(newdata)) next
    original <- source[[name]]
    value <- scenario[[name]]
    if (is.factor(original)) {
      newdata[[name]] <- factor(
        rep(as.character(value), nrow(newdata)), levels = levels(original),
        ordered = is.ordered(original)
      )
    } else {
      newdata[[name]] <- rep(as.numeric(value), nrow(newdata))
    }
  }
  design <- stats::model.matrix(
    .rls_binary_design_terms(fit), newdata,
    contrasts.arg = fit$contrasts, xlev = fit$xlevels
  )
  coefficients <- stats::coef(fit)
  if (anyNA(coefficients) || fit$rank < length(coefficients)) {
    stop(
      "This binary model cannot be interpreted because some coefficients are not estimable (rank-deficient fit). Remove unsupported interaction terms or combine sparse categories, then refit the model.",
      call. = FALSE
    )
  }
  aligned <- matrix(0, nrow = nrow(design), ncol = length(coefficients),
                    dimnames = list(NULL, names(coefficients)))
  matched <- match(colnames(design), names(coefficients))
  if (anyNA(matched)) {
    stop("The binary-effect design matrix does not match the fitted model.", call. = FALSE)
  }
  aligned[, matched] <- design
  fitted_offset <- stats::model.offset(frame)
  scenario_offset <- if (is.null(fitted_offset)) rep(0, nrow(aligned)) else
    if (identical(adjustment, "average_sample")) fitted_offset else
      rep(mean(fitted_offset), nrow(aligned))
  if (length(scenario_offset) != nrow(aligned) || any(!is.finite(scenario_offset))) {
    stop("The fitted offset cannot be aligned with the effect scenario.", call. = FALSE)
  }
  eta <- drop(aligned %*% coefficients) + scenario_offset
  probability <- fit$family$linkinv(eta)
  derivative <- fit$family$mu.eta(eta)
  gradient <- colMeans(aligned * derivative)
  covariance <- stats::vcov(fit)
  variance <- drop(crossprod(gradient, covariance %*% gradient))
  if (!is.finite(variance) || !is.finite(mean(probability))) {
    stop(
      "This binary model has non-finite predicted probabilities or uncertainty. Simplify the model and check for separated outcomes before interpreting it.",
      call. = FALSE
    )
  }
  list(
    estimate = mean(probability),
    variance = max(0, variance),
    gradient = gradient,
    covariance = covariance
  )
}

.rls_binary_pool_scalar <- function(estimates, variances, fits) {
  if (length(estimates) == 1L) {
    return(list(estimate = estimates[[1L]], variance = variances[[1L]], df = Inf))
  }
  pooled <- mice::pool.scalar(
    Q = estimates, U = variances,
    n = min(vapply(fits, stats::nobs, numeric(1L))),
    k = max(vapply(fits, function(fit) length(stats::coef(fit)), numeric(1L)))
  )
  list(estimate = pooled$qbar, variance = pooled$t, df = pooled$df)
}

.rls_binary_probability_interval <- function(estimate, std_error, critical) {
  if (!is.finite(estimate) || !is.finite(std_error) ||
      estimate <= 0 || estimate >= 1) return(c(NA_real_, NA_real_))
  logit_se <- std_error / (estimate * (1 - estimate))
  stats::plogis(stats::qlogis(estimate) + c(-1, 1) * critical * logit_se)
}

.rls_binary_effect_verification_recipe <- function(record, grid, adjustment) {
  provenance <- record$analysis_provenance
  if (is.null(provenance)) return(NULL)
  base <- provenance$verification_r_code$model %||% provenance$verification_r_code$table
  if (is.null(base)) return(NULL)
  grid_code <- paste(capture.output(dput(grid)), collapse = "\n")
  paste(c(base,
    "# Check response-scale effect estimates with public R functions.",
    "# Inference additionally uses the fitted covariance and, for MI, Rubin pooling.",
    "verification_fits <- if(exists('reference_fits', inherits=FALSE)) reference_fits else if(exists('fits', inherits=FALSE)) fits else if(exists('reference_fit', inherits=FALSE)) list(reference_fit) else list(fit)",
    paste0("verification_grid <- ", grid_code),
    paste0("verification_adjustment <- ", .rls_r_string_literal(adjustment)),
    "verification_one_fit <- function(fit) {",
    "  frame <- stats::model.frame(fit)",
    "  response <- names(frame)[[attr(stats::terms(fit), 'response')]]",
    "  source <- frame[, setdiff(names(frame), response), drop=FALSE]",
    "  fitted_offset <- stats::model.offset(frame)",
    "  if(is.null(fitted_offset)) fitted_offset <- rep(0, nrow(source))",
    "  vapply(seq_len(nrow(verification_grid)), function(row) {",
    "    current <- if(verification_adjustment == 'average_sample') source else source[1L,,drop=FALSE]",
    "    if(verification_adjustment != 'average_sample') for(name in names(current)) {",
    "      column <- source[[name]]",
    "      if(is.factor(column)) current[[name]] <- factor(levels(column)[[1L]],levels=levels(column),ordered=is.ordered(column))",
    "      else if(is.numeric(column)) current[[name]] <- mean(column,na.rm=TRUE)",
    "    }",
    "    for(name in names(verification_grid)) {",
    "      value <- verification_grid[[name]][[row]]",
    "      if(is.factor(source[[name]])) current[[name]] <- factor(rep(as.character(value),nrow(current)),levels=levels(source[[name]]),ordered=is.ordered(source[[name]]))",
    "      else current[[name]] <- rep(as.numeric(value),nrow(current))",
    "    }",
    "    design_terms <- stats::delete.response(stats::terms(fit))",
    "    offset_positions <- attr(design_terms, 'offset')",
    "    if(length(offset_positions)) {",
    "      for(field in c('variables','predvars')) {",
    "        variables <- attr(design_terms,field)",
    "        if(!is.null(variables)) attr(design_terms,field) <- as.call(as.list(variables)[-(offset_positions+1L)])",
    "      }",
    "      factors <- attr(design_terms,'factors')",
    "      if(!is.null(factors)) attr(design_terms,'factors') <- factors[-offset_positions,,drop=FALSE]",
    "      attr(design_terms,'offset') <- NULL",
    "    }",
    "    design <- stats::model.matrix(design_terms,current,contrasts.arg=fit$contrasts,xlev=fit$xlevels)",
    "    offset <- if(verification_adjustment == 'average_sample') fitted_offset else mean(fitted_offset)",
    "    eta <- drop(design[,names(stats::coef(fit)),drop=FALSE] %*% stats::coef(fit)) + offset",
    "    mean(stats::family(fit)$linkinv(eta))",
    "  }, numeric(1L))",
    "}",
    "verification_by_imputation <- vapply(verification_fits, verification_one_fit, numeric(nrow(verification_grid)))",
    "reference <- data.frame(verification_grid, estimated_probability=rowMeans(as.matrix(verification_by_imputation)))",
    "reference"), collapse="\n")
}

.rls_binary_effect_record <- function(record, fits, parts, focal, conditioning,
                                      confidence_level, grid_points,
                                      quantity, adjustment, presentation) {
  options <- .rls_binary_effect_options(quantity, adjustment, presentation)
  confidence_level <- suppressWarnings(as.numeric(confidence_level)[[1L]])
  if (!is.finite(confidence_level) || confidence_level <= 0 || confidence_level >= 1) {
    stop("`confidence_level` must be one number strictly between 0 and 1.",
         call. = FALSE)
  }
  first <- stats::model.frame(fits[[1L]])
  factor_part <- vapply(parts, function(variable) is.factor(first[[variable]]), logical(1L))
  grid <- .rls_binary_effect_grid(fits, focal, conditioning, grid_points)
  per_imputation <- lapply(fits, function(fit) {
    lapply(seq_len(nrow(grid)), function(row) {
      .rls_binary_scenario_estimate(
        fit, as.list(grid[row, c(focal, conditioning), drop = FALSE]),
        options$adjustment
      )
    })
  })
  metadata <- .rls_regression_post_estimation_metadata(fits)
  estimates <- grid
  estimates$pooled_estimate <- estimates$pooled_std_error <-
    estimates$pooled_df <- estimates$pooled_statistic <-
    estimates$pooled_p_value <- estimates$pooled_conf_low <-
    estimates$pooled_conf_high <- NA_real_
  alpha <- 1 - confidence_level
  for (row in seq_len(nrow(grid))) {
    pooled <- .rls_binary_pool_scalar(
      vapply(per_imputation, function(one) one[[row]]$estimate, numeric(1L)),
      vapply(per_imputation, function(one) one[[row]]$variance, numeric(1L)),
      fits
    )
    se <- sqrt(max(0, pooled$variance))
    critical <- if (is.finite(pooled$df)) stats::qt(1 - alpha / 2, pooled$df) else
      stats::qnorm(1 - alpha / 2)
    interval <- .rls_binary_probability_interval(pooled$estimate, se, critical)
    estimates$pooled_estimate[[row]] <- pooled$estimate
    estimates$pooled_std_error[[row]] <- se
    estimates$pooled_df[[row]] <- pooled$df
    estimates$pooled_conf_low[[row]] <- interval[[1L]]
    estimates$pooled_conf_high[[row]] <- interval[[2L]]
  }

  comparison_rows <- list()
  conditioning_key <- if (length(conditioning)) {
    interaction(grid[, conditioning, drop = FALSE], drop = TRUE, sep = "\r")
  } else factor(rep("", nrow(grid)))
  for (key in levels(conditioning_key)) {
    rows <- which(conditioning_key == key)
    if (length(rows) < 2L) next
    focal_is_factor <- is.factor(grid[[focal]])
    pairs <- if (focal_is_factor) {
      lapply(utils::combn(rows, 2L, simplify = FALSE), rev)
    } else {
      list(c(rows[[length(rows)]], rows[[1L]]))
    }
    for (pair in pairs) {
      left <- pair[[1L]]
      right <- pair[[2L]]
      per_q <- per_u <- numeric(length(fits))
      for (imputation in seq_along(fits)) {
        one_left <- per_imputation[[imputation]][[left]]
        one_right <- per_imputation[[imputation]][[right]]
        gradient <- one_left$gradient - one_right$gradient
        per_q[[imputation]] <- one_left$estimate - one_right$estimate
        per_u[[imputation]] <- max(0, drop(crossprod(
          gradient, one_left$covariance %*% gradient
        )))
      }
      pooled <- .rls_binary_pool_scalar(per_q, per_u, fits)
      se <- sqrt(max(0, pooled$variance))
      critical <- if (is.finite(pooled$df)) stats::qt(1 - alpha / 2, pooled$df) else
        stats::qnorm(1 - alpha / 2)
      label <- paste(as.character(grid[[focal]][[left]]), "\u2212",
                     as.character(grid[[focal]][[right]]))
      comparison <- grid[left, conditioning, drop = FALSE]
      comparison[[focal]] <- label
      comparison$contrast <- label
      comparison$pooled_estimate <- pooled$estimate
      comparison$pooled_std_error <- se
      comparison$pooled_df <- pooled$df
      comparison$pooled_statistic <- if (se > 0) pooled$estimate / se else NA_real_
      comparison$pooled_p_value <- if (se > 0) {
        2 * if (is.finite(pooled$df))
          stats::pt(-abs(comparison$pooled_statistic), pooled$df) else
          stats::pnorm(-abs(comparison$pooled_statistic))
      } else NA_real_
      comparison$pooled_conf_low <- pooled$estimate - critical * se
      comparison$pooled_conf_high <- pooled$estimate + critical * se
      comparison_rows[[length(comparison_rows) + 1L]] <- comparison
    }
  }
  comparisons <- if (length(comparison_rows)) do.call(rbind, comparison_rows) else data.frame()

  binary_pooling_method <- paste0(
    if (metadata$multiple_imputation)
      "Per-imputation probability-scale delta method plus mice::pool.scalar()" else
      "Probability-scale delta method from stats::glm()",
    "; within-model covariance retained for differences"
  )

  for (tab_name in c("estimates", "comparisons")) {
    tab <- get(tab_name)
    if (nrow(tab)) {
      tab$interaction <- paste(parts, collapse = ":")
      tab$imputation_count <- metadata$imputation_count
      tab$pooling_method <- binary_pooling_method
      tab$quantity <- if (identical(tab_name, "estimates"))
        "predicted_probability" else "probability_difference"
      tab$adjustment_mode <- options$adjustment
      tab$event <- record$event %||% record$summary$event %||% "event"
    }
    assign(tab_name, tab)
  }
  plot_data <- if (identical(options$quantity, "probability_difference")) comparisons else estimates
  if (!nrow(plot_data)) {
    stop("The requested binary probability difference is not estimable.", call. = FALSE)
  }
  interaction_type <- if (length(parts) == 1L) {
    if (factor_part[[match(focal, parts)]]) "simple_factor" else "simple_numeric"
  } else if (all(factor_part)) {
    if (length(parts) == 2L) "factor_factor" else "factor_factor_factor"
  } else if (length(parts) == 2L && is.factor(first[[conditioning[[1L]]]])) {
    "numeric_factor"
  } else if (length(parts) == 2L) {
    "numeric_numeric"
  } else "three_way_with_numeric"
  event <- as.character(record$event %||% record$summary$event %||% "event")[[1L]]
  adjustment_text <- if (identical(options$adjustment, "average_sample")) {
    "Predictions are averaged over the same analysis sample after assigning each displayed scenario to every observation."
  } else {
    "Predictions use a reference profile: numeric covariates at their analysis-sample means and other categorical predictors at their fitted reference categories."
  }
  if (!is.null(stats::model.offset(first))) {
    adjustment_text <- paste(adjustment_text,
      if (identical(options$adjustment, "average_sample"))
        "Each observation retains its fitted offset." else
        "The reference profile uses the mean fitted offset.")
  }
  provenance <- record$analysis_provenance
  if (!is.null(provenance)) {
    recipe <- .rls_binary_effect_verification_recipe(record, grid, options$adjustment)
    if (!is.null(recipe)) {
      provenance$verification_r_code$effect <- recipe
      provenance$verification_r_code$table <- recipe
      provenance$output_r_code$effect <- "reference"
      provenance$output_r_code$table <- "reference"
    }
  }
  structure(list(
    interaction = paste(parts, collapse = ":"), interaction_type = interaction_type,
    focal = focal,
    moderator = if (length(conditioning)) conditioning[[1L]] else "",
    conditioning = conditioning,
    all_factor = all(factor_part),
    interpretation = paste0("Predicted probability of response = ", event, ". ", adjustment_text),
    estimates = estimates, comparisons = comparisons,
    interaction_contrasts = data.frame(), plot_data = plot_data,
    emm_test_reference = NULL, emm_reference_test_enabled = FALSE,
    comparison_adjustment = "none", confidence_level = confidence_level,
    multiple_imputation = metadata$multiple_imputation,
    imputation_count = metadata$imputation_count,
    pooling_method = binary_pooling_method,
    binary_probability = TRUE, event = event, quantity = options$quantity,
    adjustment_mode = options$adjustment, presentation = options$presentation,
    analysis_provenance = provenance
  ), class = "linkeda_regression_interaction")
}

.rls_interaction_contrast_method <- function(method, argument) {
  value <- tolower(trimws(as.character(method %||% "pairwise")[[1L]]))
  aliases <- c(
    pairwise = "pairwise", all = "pairwise", all_pairwise = "pairwise",
    reference = "reference", vs_reference = "reference", each_vs_reference = "reference",
    consecutive = "consecutive", adjacent = "consecutive"
  )
  if (!value %in% names(aliases)) {
    stop(sprintf(
      "`%s` must be one of `pairwise`, `reference`, or `consecutive`.", argument
    ), call. = FALSE)
  }
  unname(aliases[[value]])
}

.rls_interaction_contrast_levels <- function(data, variable, level_order = NULL) {
  values <- data[[variable]]
  if (!is.factor(values)) {
    stop(sprintf("Interaction contrasts require `%s` to be a categorical predictor.", variable),
         call. = FALSE)
  }
  available <- as.character(levels(values))
  if (!length(available)) {
    stop(sprintf("Categorical predictor `%s` has no fitted categories.", variable),
         call. = FALSE)
  }
  if (is.null(level_order)) return(available)
  requested <- as.character(level_order)
  if (length(requested) != length(available) ||
      anyDuplicated(requested) || !setequal(requested, available)) {
    stop(sprintf(
      "The requested category order for `%s` must contain every fitted category exactly once.",
      variable
    ), call. = FALSE)
  }
  requested
}

.rls_interaction_contrast_definitions <- function(levels, method = "pairwise",
                                                  reference = NULL, variable = "factor") {
  method <- .rls_interaction_contrast_method(method, paste0(variable, "_method"))
  levels <- as.character(levels)
  if (length(levels) < 2L) {
    stop(sprintf("Categorical predictor `%s` must have at least two fitted categories.", variable),
         call. = FALSE)
  }
  pairs <- switch(
    method,
    pairwise = utils::combn(levels, 2L, simplify = FALSE),
    reference = {
      reference <- as.character(reference %||% levels[[1L]])[[1L]]
      if (!reference %in% levels) {
        stop(sprintf("Reference category `%s` is not a fitted category of `%s`.",
                     reference, variable), call. = FALSE)
      }
      lapply(levels[levels != reference], function(value) c(value, reference))
    },
    consecutive = lapply(seq_len(length(levels) - 1L), function(i) levels[c(i, i + 1L)])
  )
  Map(function(pair, index) {
    coefficients <- stats::setNames(rep(0, length(levels)), levels)
    coefficients[[pair[[1L]]]] <- 1
    coefficients[[pair[[2L]]]] <- -1
    list(
      id = sprintf("contrast_%04d", index),
      label = paste(pair, collapse = " - "),
      first = pair[[1L]], second = pair[[2L]], coefficients = coefficients
    )
  }, pairs, seq_along(pairs))
}

.rls_interaction_contrast_methods <- function(definitions, observed_order, variable) {
  observed_order <- as.character(observed_order)
  expected <- names(definitions[[1L]]$coefficients)
  if (length(observed_order) != length(expected) ||
      anyDuplicated(observed_order) || !setequal(observed_order, expected)) {
    stop(sprintf(
      "The estimated-marginal-means grid for `%s` does not preserve every fitted level.",
      variable
    ), call. = FALSE)
  }
  methods <- lapply(definitions, function(definition) {
    unname(definition$coefficients[observed_order])
  })
  names(methods) <- vapply(definitions, `[[`, character(1L), "id")
  methods
}

.rls_interaction_semantic_key <- function(values) {
  values <- as.character(values)
  paste(paste0(nchar(values, type = "bytes"), ":", values), collapse = "|")
}

.rls_interaction_contrast_one_fit <- function(fit, contrast_factor, comparison_factor,
                                              conditioning_factors,
                                              contrast_definitions,
                                              comparison_definitions,
                                              confidence_level = .95,
                                              scale = "response") {
  by_first <- c(comparison_factor, conditioning_factors)
  reference_data <- stats::model.frame(fit)
  grid_args <- list(
    operation = emmeans::emmeans, object = fit, data = reference_data,
    specs = contrast_factor,
    by = if (length(by_first)) by_first else NULL
  )
  if (inherits(fit, "betareg")) {
    grid_args$mode <- if (identical(scale, "link")) "link" else "response"
  }
  grid <- do.call(.rls_emmeans_with_fitted_data, grid_args)
  transformation <- grid@misc$tran
  link_name <- if (is.character(transformation) && length(transformation))
    transformation[[1L]] else "identity"
  # glmmTMB (including beta-binomial) also carries a transformed EMM grid.
  # Restricting this conversion to glm mislabeled link-scale contrasts as
  # response-scale differences for those fitted R engines.
  if (identical(scale, "response") && !is.null(grid@misc$tran) &&
      !identical(grid@misc$tran, "identity")) {
    grid <- emmeans::regrid(grid, transform = "response")
  }
  grid_table <- as.data.frame(grid, stringsAsFactors = FALSE)
  contrast_order <- unique(as.character(grid_table[[contrast_factor]]))
  contrast_methods <- .rls_interaction_contrast_methods(
    contrast_definitions, contrast_order, contrast_factor
  )
  first <- emmeans::contrast(
    grid, method = contrast_methods,
    by = if (length(by_first)) by_first else NULL,
    adjust = "none"
  )
  first_table <- .rls_emmeans_standardize_table(
    summary(first, infer = c(TRUE, TRUE), level = confidence_level),
    estimate_candidates = "estimate"
  )
  if (!all(c("contrast", comparison_factor) %in% names(first_table))) {
    stop("The first-stage simple-contrast grid is incomplete.", call. = FALSE)
  }
  comparison_order <- unique(as.character(first_table[[comparison_factor]]))
  comparison_methods <- .rls_interaction_contrast_methods(
    comparison_definitions, comparison_order, comparison_factor
  )
  second <- emmeans::contrast(
    first, method = comparison_methods,
    by = c("contrast", conditioning_factors),
    adjust = "none"
  )
  second_table <- .rls_emmeans_standardize_table(
    summary(second, infer = c(TRUE, TRUE), level = confidence_level),
    estimate_candidates = "estimate"
  )
  second_name <- setdiff(
    intersect(c("contrast1", "contrast2", "contrast3"), names(second_table)),
    "contrast"
  )
  if (!length(second_name)) {
    stop("The second-stage interaction-contrast grid is incomplete.", call. = FALSE)
  }
  second_name <- second_name[[1L]]
  names(second_table)[names(second_table) == "contrast"] <- "contrast_A"
  names(second_table)[names(second_table) == second_name] <- "contrast_B"

  definition_frame <- function(definitions, prefix) {
    out <- data.frame(
      id = vapply(definitions, `[[`, character(1L), "id"),
      label = vapply(definitions, `[[`, character(1L), "label"),
      first = vapply(definitions, `[[`, character(1L), "first"),
      second = vapply(definitions, `[[`, character(1L), "second"),
      stringsAsFactors = FALSE
    )
    names(out) <- paste0(prefix, c("id", "label", "first", "second"))
    out
  }
  a_defs <- definition_frame(contrast_definitions, "A_")
  b_defs <- definition_frame(comparison_definitions, "B_")
  second_table <- merge(
    second_table, a_defs, by.x = "contrast_A", by.y = "A_id",
    sort = FALSE, all.x = TRUE
  )
  second_table <- merge(
    second_table, b_defs, by.x = "contrast_B", by.y = "B_id",
    sort = FALSE, all.x = TRUE
  )
  if (anyNA(second_table[c("A_first", "A_second", "B_first", "B_second")])) {
    stop("Interaction contrasts could not be aligned to their semantic categories.",
         call. = FALSE)
  }

  condition_key <- function(tab) {
    if (!length(conditioning_factors)) return(rep("", nrow(tab)))
    vapply(seq_len(nrow(tab)), function(i) {
      .rls_interaction_semantic_key(unlist(Map(
        c, conditioning_factors,
        lapply(conditioning_factors, function(variable) {
          as.character(tab[[variable]][[i]])
        })
      ), use.names = FALSE))
    }, character(1L))
  }
  first_table$.condition_key <- condition_key(first_table)
  second_table$.condition_key <- condition_key(second_table)
  first_lookup <- stats::setNames(
    first_table$pooled_estimate,
    vapply(seq_len(nrow(first_table)), function(i) .rls_interaction_semantic_key(c(
      first_table$contrast[[i]], as.character(first_table[[comparison_factor]][[i]]),
      first_table$.condition_key[[i]]
    )), character(1L))
  )
  second_table$simple_A_at_B_first <- vapply(seq_len(nrow(second_table)), function(i) {
    key <- .rls_interaction_semantic_key(c(
      second_table$contrast_A[[i]], second_table$B_first[[i]],
      second_table$.condition_key[[i]]
    ))
    value <- unname(first_lookup[key])
    if (length(value) && is.finite(value[[1L]])) value[[1L]] else NA_real_
  }, numeric(1L))
  second_table$simple_A_at_B_second <- vapply(seq_len(nrow(second_table)), function(i) {
    key <- .rls_interaction_semantic_key(c(
      second_table$contrast_A[[i]], second_table$B_second[[i]],
      second_table$.condition_key[[i]]
    ))
    value <- unname(first_lookup[key])
    if (length(value) && is.finite(value[[1L]])) value[[1L]] else NA_real_
  }, numeric(1L))
  second_table$semantic_key <- vapply(seq_len(nrow(second_table)), function(i) {
    .rls_interaction_semantic_key(c(
      contrast_factor, second_table$A_first[[i]], second_table$A_second[[i]],
      comparison_factor, second_table$B_first[[i]], second_table$B_second[[i]],
      second_table$.condition_key[[i]]
    ))
  }, character(1L))
  if (anyDuplicated(second_table$semantic_key)) {
    stop("The interaction-contrast grid produced duplicate semantic keys.", call. = FALSE)
  }
  second_table$contrast_A <- second_table$A_label
  second_table$contrast_B <- second_table$B_label
  attr(second_table, "effect_link") <- link_name
  second_table
}

.rls_regression_interaction_contrasts_record <- function(
    record, term, contrast_factor = NULL, comparison_factor = NULL,
    conditioning_factors = NULL, contrast_method = "pairwise",
    comparison_method = "pairwise", contrast_reference = NULL,
    comparison_reference = NULL, level_order = NULL,
    confidence_level = .95, scale = "response") {
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop("Interaction contrasts require the suggested R package `emmeans`.",
         call. = FALSE)
  }
  term <- .rls_validate_protocol_name(term, "term")
  parts <- .rls_model_interaction_parts(term)
  if (length(parts) < 2L) {
    stop("Interaction contrasts require an included interaction of at least two categorical predictors.",
         call. = FALSE)
  }
  terms <- record$predictors %||% record$terms %||% character()
  if (!term %in% terms) stop("The requested interaction is not included in the model.", call. = FALSE)
  contrast_factor <- as.character(contrast_factor %||% parts[[1L]])[[1L]]
  comparison_factor <- as.character(comparison_factor %||% parts[[2L]])[[1L]]
  if (identical(contrast_factor, comparison_factor) ||
      !all(c(contrast_factor, comparison_factor) %in% parts)) {
    stop("Contrast predictor A and comparison predictor B must be different variables in the interaction.",
         call. = FALSE)
  }
  remaining <- setdiff(parts, c(contrast_factor, comparison_factor))
  if (is.null(conditioning_factors)) conditioning_factors <- remaining
  conditioning_factors <- as.character(conditioning_factors)
  if (anyDuplicated(conditioning_factors) || !setequal(conditioning_factors, remaining)) {
    stop("Conditioning categorical predictors must contain every remaining interaction variable exactly once.",
         call. = FALSE)
  }
  fits <- .rls_regression_post_estimation_fits(record)
  reference_data <- stats::model.frame(fits[[1L]])
  if (!all(parts %in% names(reference_data)) ||
      !all(vapply(parts, function(variable) is.factor(reference_data[[variable]]), logical(1L)))) {
    stop("Interaction contrasts currently require an all-categorical interaction.", call. = FALSE)
  }
  scale <- match.arg(scale, c("response", "link"))
  level_order <- level_order %||% list()
  order_for <- function(variable) {
    requested <- if (!is.null(names(level_order)) && variable %in% names(level_order)) {
      level_order[[variable]]
    } else NULL
    .rls_interaction_contrast_levels(reference_data, variable, requested)
  }
  levels_A <- order_for(contrast_factor)
  levels_B <- order_for(comparison_factor)
  definitions_A <- .rls_interaction_contrast_definitions(
    levels_A, contrast_method, contrast_reference, contrast_factor
  )
  definitions_B <- .rls_interaction_contrast_definitions(
    levels_B, comparison_method, comparison_reference, comparison_factor
  )
  per_imputation <- lapply(fits, .rls_interaction_contrast_one_fit,
    contrast_factor = contrast_factor,
    comparison_factor = comparison_factor,
    conditioning_factors = conditioning_factors,
    contrast_definitions = definitions_A,
    comparison_definitions = definitions_B,
    confidence_level = confidence_level,
    scale = scale
  )
  links <- vapply(per_imputation, function(tab)
    attr(tab, "effect_link") %||% "identity", character(1L))
  if (length(unique(links)) != 1L) {
    stop("Imputations produced incompatible post-hoc link scales.", call. = FALSE)
  }
  link_name <- links[[1L]]
  expected_keys <- per_imputation[[1L]]$semantic_key
  aligned <- lapply(seq_along(per_imputation), function(i) {
    tab <- per_imputation[[i]]
    if (!setequal(tab$semantic_key, expected_keys)) {
      stop(sprintf(
        "Imputation %d produced a different semantic interaction-contrast grid.", i
      ), call. = FALSE)
    }
    tab[match(expected_keys, tab$semantic_key), , drop = FALSE]
  })
  out <- aligned[[1L]]
  if (length(aligned) == 1L) {
    # emmeans already used the exact full covariance matrix for the four-cell
    # linear combination within this fitted model.
  } else {
    pools <- lapply(seq_len(nrow(out)), function(row) {
      estimates <- vapply(aligned, function(tab) tab$pooled_estimate[[row]], numeric(1L))
      variances <- vapply(aligned, function(tab) tab$pooled_std_error[[row]]^2, numeric(1L))
      dfs <- vapply(aligned, function(tab) tab$pooled_df[[row]], numeric(1L))
      finite_dfs <- dfs[is.finite(dfs) & dfs > 0]
      df_complete <- if (length(finite_dfs)) min(finite_dfs) else NA_real_
      tryCatch(
        .rls_mi_pool_scalar(estimates, variances, df_complete, confidence_level),
        error = function(error) .rls_mi_empty_scalar_pool(
          length(aligned), conditionMessage(error)
        )
      )
    })
    out$pooled_estimate <- vapply(pools, `[[`, numeric(1L), "Qbar")
    out$pooled_std_error <- vapply(pools, `[[`, numeric(1L), "SE")
    out$pooled_df <- vapply(pools, `[[`, numeric(1L), "df")
    out$pooled_statistic <- vapply(pools, `[[`, numeric(1L), "statistic")
    out$pooled_p_value <- vapply(pools, `[[`, numeric(1L), "p")
    out$pooled_conf_low <- vapply(pools, `[[`, numeric(1L), "CI_low")
    out$pooled_conf_high <- vapply(pools, `[[`, numeric(1L), "CI_high")
    out$estimable <- vapply(pools, `[[`, logical(1L), "valid")
    out$non_estimable_reason <- vapply(pools, function(pool) pool$reason %||% "", character(1L))
    out$simple_A_at_B_first <- rowMeans(do.call(cbind, lapply(
      aligned, `[[`, "simple_A_at_B_first"
    )), na.rm = TRUE)
    out$simple_A_at_B_second <- rowMeans(do.call(cbind, lapply(
      aligned, `[[`, "simple_A_at_B_second"
    )), na.rm = TRUE)
    out$simple_A_at_B_first[is.nan(out$simple_A_at_B_first)] <- NA_real_
    out$simple_A_at_B_second[is.nan(out$simple_A_at_B_second)] <- NA_real_
    attr(out,"missing_information") <- do.call(rbind,lapply(seq_along(pools),function(i)
      .rls_mi_pooling_diagnostics(pools[[i]],length(fits),out$semantic_key[[i]])))
  }
  if (!"estimable" %in% names(out)) {
    out$estimable <- is.finite(out$pooled_estimate) & is.finite(out$pooled_std_error)
    out$non_estimable_reason <- ifelse(out$estimable, "", "Non-estimable contrast")
  }
  out$contrast_factor <- contrast_factor
  out$comparison_factor <- comparison_factor
  out$contrast_method <- .rls_interaction_contrast_method(contrast_method, "contrast_method")
  out$comparison_method <- .rls_interaction_contrast_method(comparison_method, "comparison_method")
  out$scale <- scale
  out$effect_link <- link_name
  out$effect_scale <- if (identical(scale, "response") ||
      identical(link_name, "identity")) {
    if (isTRUE(record$count_regression) &&
        .rls_count_distribution_uses_trials(record$count_distribution))
      "probability difference of differences (response scale)" else
      "difference of differences (response scale)"
  } else paste(link_name, "difference of differences (link scale)")
  if (identical(scale, "link") && identical(link_name, "logit")) {
    out$ratio_of_odds_ratios <- exp(out$pooled_estimate)
    out$ratio_of_odds_ratios_conf_low <- exp(out$pooled_conf_low)
    out$ratio_of_odds_ratios_conf_high <- exp(out$pooled_conf_high)
    out$ratio_label <- if (inherits(fits[[1L]], "betareg"))
      "Ratio of mean-odds ratios" else "Ratio of odds ratios"
  }
  out$imputation_count <- length(fits)
  out$pooling_method <- if (length(fits) > 1L) {
    paste0(
      "mice::pool.scalar (Rubin's rules) applied to the complete within-imputation ",
      "interaction contrast and its full-covariance variance"
    )
  } else {
    "emmeans linear contrast using the fitted model covariance matrix"
  }
  preferred <- c(
    conditioning_factors, "contrast_A", "contrast_B", "A_first", "A_second",
    "B_first", "B_second", "simple_A_at_B_first", "simple_A_at_B_second",
    "pooled_estimate", "pooled_std_error", "pooled_df", "pooled_statistic",
    "pooled_p_value", "pooled_conf_low", "pooled_conf_high", "estimable",
    "non_estimable_reason", "semantic_key", "contrast_factor", "comparison_factor",
    "contrast_method", "comparison_method", "scale", "imputation_count", "pooling_method"
  )
  out <- out[, c(intersect(preferred, names(out)), setdiff(names(out), preferred)), drop = FALSE]
  attr(out, "interaction_contrast") <- list(
    term = term, contrast_factor = contrast_factor,
    comparison_factor = comparison_factor,
    conditioning_factors = conditioning_factors,
    confidence_level = confidence_level,
    definition = sprintf(
      "(%s[a1,b1] - %s[a2,b1]) - (%s[a1,b2] - %s[a2,b2])",
      if (identical(scale, "link") && !identical(link_name, "identity")) "eta" else "mu",
      if (identical(scale, "link") && !identical(link_name, "identity")) "eta" else "mu",
      if (identical(scale, "link") && !identical(link_name, "identity")) "eta" else "mu",
      if (identical(scale, "link") && !identical(link_name, "identity")) "eta" else "mu"
    ),
    orientation_note = paste0(
      "Each label is first category minus second category; the reported interaction estimate is ",
      "the first simple A contrast minus the second simple A contrast."
    ),
    pooling_method = unique(out$pooling_method)[[1L]],
    per_imputation = per_imputation
  )
  class(out) <- c("linkeda_interaction_contrasts", class(out))
  out
}

.rls_hurdle_effect_grid <- function(fits, parts, focal, grid_points) {
  reference_data <- fits[[1L]]$linkeda_model_data
  if (!is.data.frame(reference_data)) {
    stop("The fitted hurdle model has no retained R model frame.", call. = FALSE)
  }
  predictors <- all.vars(stats::delete.response(stats::terms(fits[[1L]])))
  trials_variable <- as.character(
    attr(fits[[1L]], "linkeda_trials_variable") %||% ""
  )[[1L]]
  retained <- unique(c(predictors, trials_variable[nzchar(trials_variable)]))
  reference_data <- reference_data[, retained, drop = FALSE]
  numeric_grid <- function(variable, points) {
    observed <- unlist(lapply(fits, function(fit) {
      data <- fit$linkeda_model_data
      if (is.data.frame(data) && variable %in% names(data)) {
        as.numeric(data[[variable]])
      } else numeric()
    }), use.names = FALSE)
    observed <- observed[is.finite(observed)]
    if (!length(observed)) {
      stop(sprintf("`%s` has no finite fitted values.", variable), call. = FALSE)
    }
    if (points == 3L) {
      center <- mean(observed)
      spread <- stats::sd(observed)
      if (!is.finite(spread) || spread <= 0) spread <- diff(range(observed)) / 2
      return(unique(pmax(
        min(observed), pmin(max(observed), c(center - spread, center, center + spread))
      )))
    }
    limits <- range(observed)
    if (limits[[1L]] == limits[[2L]]) return(limits[[1L]])
    seq(limits[[1L]], limits[[2L]], length.out = max(2L, as.integer(points)))
  }
  values <- lapply(parts, function(variable) {
    value <- reference_data[[variable]]
    if (is.factor(value)) levels(value) else
      numeric_grid(
        variable,
        if (identical(variable, focal)) max(3L, as.integer(grid_points)) else 3L
      )
  })
  names(values) <- parts
  grid <- do.call(expand.grid, c(values, KEEP.OUT.ATTRS = FALSE,
                                 stringsAsFactors = FALSE))
  for (variable in parts) {
    if (is.factor(reference_data[[variable]])) {
      grid[[variable]] <- factor(
        grid[[variable]], levels = levels(reference_data[[variable]]),
        ordered = is.ordered(reference_data[[variable]])
      )
    } else grid[[variable]] <- as.numeric(grid[[variable]])
  }
  grid
}

.rls_hurdle_conditional_score <- function(trials, eta_mu, eta_sigma) {
  mu <- stats::plogis(eta_mu)
  sigma <- exp(eta_sigma)
  trials <- as.numeric(trials)
  p_zero <- gamlss.dist::dBB(0, mu = mu, sigma = sigma, bd = trials)
  denominator <- 1 - p_zero
  # For the BB parameterization used by gamlss.dist, E(K) = bd * mu.
  # Because K = 0 contributes nothing to that expectation,
  # E(K | K > 0) = E(K) / P(K > 0).  This is exactly the same upper-
  # truncated expectation as summing dBB(1:bd), while avoiding one complete
  # probability-mass calculation for every row, grid point and imputation.
  result <- trials - trials * mu / denominator
  result[!is.finite(denominator) | denominator <= 0] <- NA_real_
  result
}

.rls_hurdle_effect_one_fit <- function(fit, record, grid, parts, quantity) {
  model_data <- fit$linkeda_model_data
  if (!is.data.frame(model_data)) {
    stop("The fitted hurdle model has no retained R model frame.", call. = FALSE)
  }
  predictors <- all.vars(stats::delete.response(stats::terms(fit)))
  retained_variables <- unique(c(
    predictors,
    if (nzchar(record$trials_variable %||% "")) record$trials_variable else character()
  ))
  model_data <- model_data[, retained_variables, drop = FALSE]
  design_terms <- stats::delete.response(stats::terms(fit))
  conditional_fit <- fit$linkeda_below_ceiling_fit
  perfect_fit <- fit$linkeda_perfect_score_fit
  no_perfect_scores <- identical(
    fit$linkeda_perfect_score_boundary %||% "", "no_perfect_scores"
  )
  separate_components <- inherits(conditional_fit, "gamlss") &&
    inherits(perfect_fit, "glm")
  mu_beta <- unname(if (separate_components) {
    conditional_fit$mu.coefficients
  } else fit$mu.coefficients)
  sigma_beta <- unname(if (separate_components) {
    conditional_fit$sigma.coefficients
  } else fit$sigma.coefficients)
  nu_beta <- unname(if (separate_components) {
    stats::coef(perfect_fit)
  } else fit$nu.coefficients)
  n_mu <- length(mu_beta)
  n_sigma <- length(sigma_beta)
  n_nu <- length(nu_beta)
  covariance <- if (separate_components) {
    conditional_covariance_fit <- conditional_fit
    conditional_covariance_fit$call$data <- NULL
    conditional_covariance <- as.matrix(stats::vcov(conditional_covariance_fit))
    conditional_covariance <- conditional_covariance[
      seq_len(n_mu + n_sigma), seq_len(n_mu + n_sigma), drop = FALSE
    ]
    perfect_covariance <- .rls_hurdle_component_vcov(
      fit, "perfect_score"
    )
    combined <- matrix(0, n_mu + n_sigma + n_nu, n_mu + n_sigma + n_nu)
    combined[seq_len(n_mu + n_sigma), seq_len(n_mu + n_sigma)] <-
      conditional_covariance
    nu_indices <- n_mu + n_sigma + seq_len(n_nu)
    combined[nu_indices, nu_indices] <- perfect_covariance
    combined
  } else {
    covariance_fit <- fit
    covariance_fit$call$data <- NULL
    as.matrix(stats::vcov(covariance_fit))
  }
  critical <- stats::qnorm(.975)
  evaluate <- function(data) {
    design <- stats::model.matrix(
      design_terms, data = data, contrasts.arg = fit$contrasts,
      xlev = fit$xlevels
    )
    eta_mu <- as.numeric(design %*% mu_beta)
    eta_nu <- if (no_perfect_scores) rep(-Inf, nrow(data)) else
      as.numeric(design %*% nu_beta)
    eta_sigma <- rep(unname(sigma_beta[[1L]]), nrow(data))
    trials <- if (nzchar(record$trials_variable %||% "")) {
      as.numeric(data[[record$trials_variable]])
    } else rep(as.numeric(record$trials_constant), nrow(data))
    perfect <- if (no_perfect_scores) rep(0, nrow(data)) else
      stats::plogis(eta_nu)
    conditional <- .rls_hurdle_conditional_score(trials, eta_mu, eta_sigma)
    estimate <- switch(quantity,
      perfect_score_probability = perfect,
      conditional_expected_score = conditional,
      overall_expected_score = perfect * trials + (1 - perfect) * conditional
    )
    step <- 1e-5
    conditional_mu_plus <- .rls_hurdle_conditional_score(
      trials, eta_mu + step, eta_sigma
    )
    conditional_mu_minus <- .rls_hurdle_conditional_score(
      trials, eta_mu - step, eta_sigma
    )
    conditional_sigma_plus <- .rls_hurdle_conditional_score(
      trials, eta_mu, eta_sigma + step
    )
    conditional_sigma_minus <- .rls_hurdle_conditional_score(
      trials, eta_mu, eta_sigma - step
    )
    derivative_mu <- (conditional_mu_plus - conditional_mu_minus) / (2 * step)
    derivative_sigma <- (conditional_sigma_plus - conditional_sigma_minus) / (2 * step)
    derivative_nu <- perfect * (1 - perfect)
    weight_mu <- switch(quantity,
      perfect_score_probability = rep(0, nrow(data)),
      conditional_expected_score = derivative_mu,
      overall_expected_score = (1 - perfect) * derivative_mu
    )
    weight_sigma <- switch(quantity,
      perfect_score_probability = rep(0, nrow(data)),
      conditional_expected_score = derivative_sigma,
      overall_expected_score = (1 - perfect) * derivative_sigma
    )
    weight_nu <- switch(quantity,
      perfect_score_probability = derivative_nu,
      conditional_expected_score = rep(0, nrow(data)),
      overall_expected_score = derivative_nu * (trials - conditional)
    )
    gradient <- c(
      colMeans(design * weight_mu),
      c(mean(weight_sigma), rep(0, max(0L, n_sigma - 1L))),
      colMeans(design * weight_nu)
    )
    if (length(gradient) != n_mu + n_sigma + n_nu ||
        any(!is.finite(c(estimate, gradient)))) {
      return(c(estimate = NA_real_, variance = NA_real_))
    }
    c(
      estimate = mean(estimate),
      variance = max(0, as.numeric(crossprod(gradient, covariance %*% gradient)))
    )
  }
  rows <- lapply(seq_len(nrow(grid)), function(index) {
    data <- model_data
    for (variable in parts) {
      replacement <- grid[[variable]][[index]]
      if (is.factor(model_data[[variable]])) {
        replacement <- factor(
          as.character(replacement), levels = levels(model_data[[variable]]),
          ordered = is.ordered(model_data[[variable]])
        )
      }
      data[[variable]] <- rep(replacement, nrow(data))
    }
    evaluate(data)
  })
  do.call(rbind, rows)
}

.rls_ceiling_hurdle_effect_record <- function(
    record, fits, term, parts, focal, confidence_level, grid_points, quantity) {
  quantity <- if (identical(quantity, "expected_count")) {
    "overall_expected_score"
  } else if (identical(quantity, "predicted_probability")) {
    "perfect_score_probability"
  } else match.arg(quantity, c(
    "overall_expected_score", "perfect_score_probability",
    "conditional_expected_score"
  ))
  reference_data <- fits[[1L]]$linkeda_model_data
  factor_part <- vapply(parts, function(variable) {
    is.factor(reference_data[[variable]])
  }, logical(1L))
  if (is.null(focal)) {
    focal <- if (any(!factor_part)) parts[[which(!factor_part)[[1L]]]] else parts[[1L]]
  }
  parts <- c(focal, setdiff(parts, focal))
  grid <- .rls_hurdle_effect_grid(fits, parts, focal, grid_points)
  per_imputation <- lapply(fits, .rls_hurdle_effect_one_fit,
                           record = record, grid = grid, parts = parts,
                           quantity = quantity)
  critical <- stats::qnorm(1 - (1 - confidence_level) / 2)
  no_perfect_scores <- all(vapply(fits, function(fit) {
    identical(fit$linkeda_perfect_score_boundary %||% "", "no_perfect_scores")
  }, logical(1L)))
  pooled <- lapply(seq_len(nrow(grid)), function(index) {
    if (no_perfect_scores && identical(quantity, "perfect_score_probability")) {
      return(c(
        estimate = 0, std_error = 0, df = Inf, statistic = NA_real_,
        p_value = NA_real_, conf_low = 0, conf_high = 0
      ))
    }
    estimates <- vapply(per_imputation, function(value) value[index, "estimate"], numeric(1L))
    variances <- vapply(per_imputation, function(value) value[index, "variance"], numeric(1L))
    if (length(fits) > 1L) {
      result <- .rls_mi_pool_scalar(estimates, variances, conf_level = confidence_level)
      c(estimate = result$Qbar, std_error = result$SE, df = result$df,
        statistic = result$statistic, p_value = result$p,
        conf_low = result$CI_low, conf_high = result$CI_high)
    } else {
      standard_error <- sqrt(variances[[1L]])
      c(estimate = estimates[[1L]], std_error = standard_error, df = Inf,
        statistic = estimates[[1L]] / standard_error,
        p_value = 2 * stats::pnorm(abs(estimates[[1L]] / standard_error),
                                  lower.tail = FALSE),
        conf_low = estimates[[1L]] - critical * standard_error,
        conf_high = estimates[[1L]] + critical * standard_error)
    }
  })
  pooled <- as.data.frame(do.call(rbind, pooled), stringsAsFactors = FALSE)
  names(pooled) <- paste0("pooled_", names(pooled))
  plot_data <- cbind(grid, pooled)
  limits <- if (identical(quantity, "perfect_score_probability")) c(0, 1)
    else c(0, as.numeric(record$trials_constant %||% NA_real_))
  if (all(is.finite(limits))) {
    plot_data$pooled_estimate <- pmax(limits[[1L]], pmin(limits[[2L]], plot_data$pooled_estimate))
    plot_data$pooled_conf_low <- pmax(limits[[1L]], pmin(limits[[2L]], plot_data$pooled_conf_low))
    plot_data$pooled_conf_high <- pmax(limits[[1L]], pmin(limits[[2L]], plot_data$pooled_conf_high))
  }
  metadata <- .rls_regression_post_estimation_metadata(fits)
  metadata$pooling_method <- if (no_perfect_scores && length(fits) > 1L) {
    paste(
      "Rubin pooling of the upper-truncated beta-binomial component across",
      "fitted imputations; perfect-score probability fixed at zero"
    )
  } else if (no_perfect_scores) {
    paste(
      "Exact upper-truncated beta-binomial response-scale effect;",
      "perfect-score probability fixed at zero"
    )
  } else if (length(fits) > 1L) {
    "Rubin pooling of exact ZABB-derived marginal effects across fitted imputations"
  } else "Exact response-scale effect from the fitted ZABB model"
  conditioning <- parts[-1L]
  structure(list(
    interaction = term,
    interaction_type = if (length(parts) == 1L) {
      if (is.factor(reference_data[[focal]])) "simple_factor" else "simple_numeric"
    } else if (all(factor_part)) {
      if (length(parts) == 2L) "factor_factor" else "factor_factor_factor"
    } else if (length(parts) == 2L) "numeric_factor" else "three_way_with_numeric",
    focal = focal, moderator = if (length(conditioning)) conditioning[[1L]] else "",
    conditioning = conditioning, all_factor = all(factor_part),
    interpretation = switch(quantity,
      perfect_score_probability = if (no_perfect_scores) {
        "No perfect scores were observed; the perfect-score probability is fixed at zero and is not an estimated effect."
      } else "Probability of attaining the maximum possible score.",
      conditional_expected_score = "Expected score among observations below the ceiling, from the upper-truncated beta-binomial component.",
      overall_expected_score = if (no_perfect_scores) {
        "Expected score from the upper-truncated beta-binomial component; the unobserved perfect-score mass is fixed at zero."
      } else "Overall expected score combining the perfect-score hurdle and the upper-truncated beta-binomial component."
    ),
    estimates = plot_data, comparisons = data.frame(),
    interaction_contrasts = data.frame(), plot_data = plot_data,
    emm_test_reference = NULL, emm_reference_test_enabled = FALSE,
    comparison_adjustment = "none", confidence_level = confidence_level,
    bounded_count = TRUE, ceiling_hurdle = TRUE, quantity = quantity,
    trials_variable = record$trials_variable %||% "",
    trials_constant = record$trials_constant %||% NA_real_,
    multiple_imputation = metadata$multiple_imputation,
    imputation_count = metadata$imputation_count,
    pooling_method = metadata$pooling_method
  ), class = "linkeda_regression_interaction")
}

.rls_regression_interaction_record_impl <- function(record, term, confidence_level = .95,
                                               adjust = "tukey", grid_points = 25L,
                                               emm_test_reference = NULL,
                                               focal = NULL,
                                               quantity = "predicted_probability",
                                               adjustment = "average_sample",
                                               presentation = "percentage",
                                               group_by = NULL) {
  operation_started <- proc.time()[["elapsed"]]
  timings <- c(emmeans = 0, pairwise = 0, interaction_contrasts = 0)
  last_phase <- ""
  timed <- function(phase, value) {
    event_phase <- switch(phase, emmeans = "emmeans", pairwise = "pairwise",
      interaction_contrasts = "interactions")
    if (!identical(phase, last_phase)) {
      .rls_mi_post_estimation_event(record, "interaction", event_phase,
        elapsed = proc.time()[["elapsed"]] - operation_started,
        message = switch(phase,
          emmeans = "Computing estimated marginal means in R\u2026",
          pairwise = "Computing pairwise comparisons in R\u2026",
          interaction_contrasts = "Computing interaction contrasts in R\u2026"))
      last_phase <<- phase
    }
    started <- proc.time()[["elapsed"]]
    on.exit({
      timings[[phase]] <<- timings[[phase]] +
        unname(proc.time()[["elapsed"]] - started)
    }, add = TRUE)
    force(value)
  }
  add_diagnostics <- function(result) {
    total <- unname(proc.time()[["elapsed"]] - operation_started)
    .rls_mi_post_estimation_event(record, "interaction", "formatting",
      elapsed = total, message = "Formatting interaction results\u2026")
    result$execution_diagnostics <- list(
      operation = "interaction_interpretation",
      imputations = length(fits),
      emmeans_seconds = unname(timings[["emmeans"]]),
      pairwise_contrasts_seconds = unname(timings[["pairwise"]]),
      interaction_contrasts_seconds = unname(timings[["interaction_contrasts"]]),
      formatting_seconds = max(0, total - sum(timings)),
      total_seconds = total
    )
    result
  }
  term <- .rls_validate_protocol_name(term, "term")
  parts <- .rls_model_interaction_parts(term)
  if (!length(parts) %in% c(1L, 2L, 3L)) {
    stop("Effect plots require an included main effect or a two- or three-way interaction.", call. = FALSE)
  }
  terms <- record$predictors %||% record$terms %||% character()
  if (!term %in% terms) stop("The requested effect is not included in the model.", call. = FALSE)
  fits <- .rls_regression_post_estimation_fits(record)
  bounded_count <- isTRUE(record$count_regression) &&
    .rls_count_distribution_uses_trials(record$count_distribution)
  if (bounded_count) {
    if (.rls_count_is_ceiling_hurdle(record)) {
      quantity <- match.arg(quantity, c(
        "expected_count", "predicted_probability", "overall_expected_score",
        "perfect_score_probability", "conditional_expected_score"
      ))
    } else {
      quantity <- match.arg(quantity, c("expected_count", "predicted_probability"))
    }
    if (!.rls_count_is_ceiling_hurdle(record) &&
        identical(quantity, "expected_count") &&
        nzchar(record$trials_variable %||% "")) {
      stop(paste0(
        "Expected-count effect plots currently require one fixed Trials value. ",
        "Use `quantity = \"predicted_probability\"` when Trials is a variable."
      ), call. = FALSE)
    }
  }
  if (.rls_count_is_ceiling_hurdle(record)) {
    if (!is.null(group_by)) {
      stop("Choosing a grouping factor is unavailable for ceiling-hurdle effects.",
           call. = FALSE)
    }
    result <- timed("emmeans", .rls_ceiling_hurdle_effect_record(
      record = record, fits = fits, term = term, parts = parts, focal = focal,
      confidence_level = confidence_level, grid_points = grid_points,
      quantity = quantity
    ))
    return(add_diagnostics(result))
  }
  reference_data <- stats::model.frame(fits[[1L]])
  if (!all(parts %in% names(reference_data))) {
    stop("The fitted model does not contain every requested effect variable.", call. = FALSE)
  }
  factor_part <- vapply(parts, function(variable) is.factor(reference_data[[variable]]), logical(1L))
  if (!all(factor_part | vapply(parts, function(variable) is.numeric(reference_data[[variable]]), logical(1L)))) {
    stop("Interactions can be interpreted only for numeric and categorical terms.", call. = FALSE)
  }
  if (!is.null(focal)) {
    focal <- .rls_validate_protocol_name(focal, "focal variable")
    if (!focal %in% parts) {
      stop("The requested horizontal-axis variable is not part of this effect.", call. = FALSE)
    }
  }
  if (!is.null(group_by)) {
    group_by <- .rls_validate_protocol_name(group_by, "grouping factor")
    if (length(parts) < 2L || !group_by %in% names(factor_part)[factor_part]) {
      stop("The grouping factor must be a categorical variable in this interaction.",
           call. = FALSE)
    }
    if (is.null(focal)) focal <- setdiff(parts, group_by)[[1L]]
    if (identical(focal, group_by)) {
      stop("The comparison and grouping factors must be different.", call. = FALSE)
    }
  }
  if (isTRUE(record$binary_regression)) {
    if (is.null(focal)) {
      focal <- if (any(!factor_part)) {
        parts[[which(!factor_part)[[1L]]]]
      } else {
        parts[[1L]]
      }
    }
    ordered_parts <- c(focal, if (!is.null(group_by)) group_by,
                       setdiff(parts, c(focal, group_by)))
    result <- timed("emmeans", .rls_binary_effect_record(
      record = record, fits = fits, parts = ordered_parts, focal = focal,
      conditioning = ordered_parts[-1L], confidence_level = confidence_level,
      grid_points = grid_points, quantity = quantity, adjustment = adjustment,
      presentation = presentation
    ))
    return(add_diagnostics(result))
  }
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop("Interaction interpretation requires the suggested R package `emmeans`.", call. = FALSE)
  }
  target <- .rls_regression_emmeans_target(fits)
  metadata <- .rls_regression_post_estimation_metadata(fits)
  emm_test_reference <- .rls_emmeans_test_reference(emm_test_reference)
  response_type <- if (inherits(fits[[1L]], "glm") || bounded_count) "response" else NULL
  estimates <- comparisons <- plot_data <- interaction_contrasts <- data.frame()
  interpretation <- ""
  at <- NULL

  if (length(parts) == 1L) {
    focal <- parts[[1L]]
    moderator <- ""
    conditioning <- character()
    if (factor_part[[1L]]) {
      grid <- timed("emmeans", .rls_emmeans_with_fitted_data(
        emmeans::emmeans, target, reference_data, specs = focal
      ))
      estimates <- timed("emmeans", .rls_emmeans_summary(
        grid, confidence_level, response_type,
        test_reference = emm_test_reference
      ))
      comparisons <- timed("pairwise", .rls_emmeans_summary(
        emmeans::contrast(grid, method = "pairwise", adjust = adjust), confidence_level
      ))
      plot_data <- estimates
      interaction_type <- "simple_factor"
      interpretation <- sprintf(
        "Estimated marginal means for %s with pooled confidence intervals.", focal
      )
    } else {
      at <- setNames(list(.rls_interaction_numeric_grid(
        fits, focal, max(3L, as.integer(grid_points))
      )), focal)
      prediction_grid <- timed("emmeans", .rls_emmeans_with_fitted_data(
        emmeans::emmeans, target, reference_data, specs = focal, at = at
      ))
      plot_data <- timed("emmeans", .rls_emmeans_summary(
        prediction_grid, confidence_level, response_type,
        test_reference = NULL
      ))
      estimates <- plot_data
      interaction_type <- "simple_numeric"
      interpretation <- sprintf(
        "Pooled fitted effect of %s with confidence intervals.", focal
      )
    }
  } else if (all(factor_part)) {
    if (is.null(focal)) focal <- parts[[1L]]
    parts <- c(focal, if (!is.null(group_by)) group_by,
               setdiff(parts, c(focal, group_by)))
    factor_part <- setNames(rep(TRUE, length(parts)), parts)
    moderator <- parts[[2L]]
    conditioning <- parts[-1L]
    grid <- timed("emmeans", .rls_emmeans_with_fitted_data(
      emmeans::emmeans, target, reference_data,
      specs = focal, by = conditioning
    ))
    estimates <- timed("emmeans", .rls_emmeans_summary(
      grid, confidence_level, response_type,
      test_reference = emm_test_reference
    ))
    comparisons <- timed("pairwise", .rls_emmeans_summary(
      emmeans::contrast(grid, method = "pairwise", adjust = adjust), confidence_level
    ))
    plot_data <- estimates
    comparison_wording <- if (identical(tolower(adjust), "none")) {
      "unadjusted"
    } else {
      sprintf("%s-adjusted", adjust)
    }
    conditioning_text <- paste(conditioning, collapse = " and ")
    interpretation <- if (length(conditioning) == 1L) {
      sprintf(
        paste0(
          "Estimated marginal means for %s at each category of %s, followed by ",
          "%s pairwise comparisons of %s within each %s."
        ),
        focal, moderator, comparison_wording, focal, moderator
      )
    } else {
      sprintf(
        paste0(
          "Estimated marginal means for %s at every combination of %s, followed by ",
          "%s pairwise comparisons of %s within each conditioning combination."
        ),
        focal, conditioning_text, comparison_wording, focal
      )
    }
    interaction_type <- if (length(parts) == 2L) {
      "factor_factor"
    } else {
      "factor_factor_factor"
    }
    interaction_contrasts <- timed("interaction_contrasts", .rls_regression_interaction_contrasts_record(
      record = record, term = term,
      contrast_factor = parts[[1L]], comparison_factor = parts[[2L]],
      conditioning_factors = parts[-c(1L, 2L)],
      contrast_method = "pairwise", comparison_method = "pairwise",
      confidence_level = confidence_level,
      scale = if (inherits(fits[[1L]], "glm")) "response" else "link"
    ))
  } else {
    # A numeric variable is the most useful focal axis for both two- and
    # three-way interactions. Every remaining component conditions the simple
    # slope and fitted-value grid, so no imputation is reduced to a preview.
    if (is.null(focal)) focal <- parts[[which(!factor_part)[[1L]]]]
    parts <- c(focal, if (!is.null(group_by)) group_by,
               setdiff(parts, c(focal, group_by)))
    conditioning <- parts[parts != focal]
    moderator <- conditioning[[1L]]
    remaining_conditioning <- conditioning[-1L]
    numeric_conditioning <- conditioning[vapply(
      conditioning, function(variable) is.numeric(reference_data[[variable]]),
      logical(1L)
    )]
    trend_at <- lapply(numeric_conditioning, function(variable) {
      .rls_interaction_numeric_grid(fits, variable, 3L)
    })
    names(trend_at) <- numeric_conditioning
    focal_is_factor <- is.factor(reference_data[[focal]])
    if (focal_is_factor) {
      grid <- timed("emmeans", .rls_emmeans_with_fitted_data(
        emmeans::emmeans, target, reference_data,
        specs = focal, by = conditioning, at = trend_at
      ))
      estimates <- timed("emmeans", .rls_emmeans_summary(
        grid, confidence_level, response_type,
        test_reference = emm_test_reference
      ))
      comparisons <- timed("pairwise", .rls_emmeans_summary(
        emmeans::contrast(grid, method = "pairwise", adjust = adjust), confidence_level
      ))
      at <- trend_at
    } else {
      trends <- timed("emmeans", .rls_emmeans_with_fitted_data(
        emmeans::emtrends, target, reference_data,
        specs = moderator,
        by = if (length(remaining_conditioning)) remaining_conditioning else NULL,
        var = focal, at = trend_at
      ))
      estimates <- timed("emmeans", .rls_emmeans_summary(
        trends, confidence_level, estimate_candidates = paste0(focal, ".trend")
      ))
      comparisons <- timed("pairwise", .rls_emmeans_summary(
        emmeans::contrast(trends, method = "pairwise", adjust = adjust), confidence_level
      ))
      at <- c(
        setNames(list(.rls_interaction_numeric_grid(
          fits, focal, max(3L, as.integer(grid_points))
        )), focal),
        trend_at
      )
    }
    prediction_grid <- timed("emmeans", .rls_emmeans_with_fitted_data(
      emmeans::emmeans, target, reference_data,
      specs = focal, by = conditioning, at = at
    ))
    plot_data <- timed("emmeans", .rls_emmeans_summary(
      prediction_grid, confidence_level, response_type,
      test_reference = NULL
    ))
    conditioning_text <- paste(conditioning, collapse = " and ")
    pooled_prefix <- if (length(fits) > 1L) "Pooled " else ""
    interpretation <- if (focal_is_factor) {
      sprintf(
        "%sfitted means for %s at every displayed combination of %s.",
        pooled_prefix, focal, conditioning_text
      )
    } else {
      sprintf(
        paste0(
          "%ssimple slopes for %s at every displayed combination of %s, ",
          "their adjusted differences within each remaining conditioning level, ",
          "and fitted lines with confidence intervals."
        ),
        pooled_prefix, focal, conditioning_text
      )
    }
    if (length(parts) == 2L) {
      interaction_type <- if (is.factor(reference_data[[moderator]])) {
        "numeric_factor"
      } else {
        "numeric_numeric"
      }
    } else {
      interaction_type <- "three_way_with_numeric"
    }
  }

  if (identical(record$family %||% "", "lognormal")) {
    plot_data <- .rls_lognormal_emmeans_summary(
      fits = fits, specs = focal,
      by = if (length(conditioning)) conditioning else NULL,
      at = at, confidence_level = confidence_level
    )
    if (interaction_type %in% c(
      "simple_factor", "simple_numeric", "factor_factor",
      "factor_factor_factor"
    ) || is.factor(reference_data[[focal]])) {
      estimates <- plot_data
    }
    interpretation <- paste0(
      interpretation,
      " Displayed response-scale values are arithmetic means, computed as ",
      "exp(eta + sigma^2 / 2). Intervals treat the fitted residual variance as fixed.",
      if (length(fits) > 1L) " Arithmetic means are then combined with mice::pool.scalar." else "",
      " Simple slopes and pairwise comparisons in this effect report remain on the log-response scale."
    )
    metadata$pooling_method <- if (length(fits) > 1L) {
      "Exact lognormal mean transformation in emmeans; means combined with mice::pool.scalar"
    } else "Exact lognormal mean transformation in emmeans"
  }

  if (bounded_count) {
    multiplier <- if (identical(quantity, "expected_count")) {
      as.numeric(record$trials_constant)
    } else 1
    value_columns <- c(
      "pooled_estimate", "pooled_std_error", "pooled_conf_low", "pooled_conf_high"
    )
    for (tab_name in c("estimates", "comparisons", "plot_data")) {
      tab <- get(tab_name)
      if (nrow(tab) && multiplier != 1) {
        columns <- intersect(value_columns, names(tab))
        tab[columns] <- lapply(tab[columns], function(value) as.numeric(value) * multiplier)
      }
      if (nrow(tab)) {
        tab$quantity <- quantity
        tab$trials <- if (nzchar(record$trials_variable %||% "")) {
          record$trials_variable
        } else {
          as.character(record$trials_constant)
        }
      }
      assign(tab_name, tab)
    }
    interpretation <- paste0(
      interpretation,
      if (identical(quantity, "expected_count")) {
        sprintf(
          " Values are expected successes out of %s trials; R first estimates response-scale probabilities and then applies the fixed trial count.",
          format(record$trials_constant, trim = TRUE, scientific = FALSE)
        )
      } else {
        " Values are response-scale predicted probabilities."
      }
    )
  }

  if (!is.null(stats::model.offset(reference_data))) {
    interpretation <- paste0(interpretation,
      if (isTRUE(record$count_regression) && nzchar(record$exposure %||% ""))
        " Estimated means and fitted values use one unit of exposure and zero for any additional offset." else
        " Estimated means and fitted values hold the fitted offset at zero.")
  }

  for (tab_name in c("estimates", "comparisons", "plot_data")) {
    tab <- get(tab_name)
    if (nrow(tab)) {
      tab$interaction <- term
      tab$imputation_count <- metadata$imputation_count
      tab$pooling_method <- metadata$pooling_method
    }
    assign(tab_name, tab)
  }
  if (nrow(comparisons) && is.factor(reference_data[[focal]])) {
    if (identical(record$family %||% "", "lognormal")) grid <- stats::update(grid,tran="log")
    comparisons <- .rls_pairwise_scale_details(comparisons, grid,
      emmeans::contrast(grid, method="pairwise", adjust=adjust), confidence_level,
      response_table=estimates, mean_odds=inherits(fits[[1L]], "betareg"))
  }
  provenance <- if (identical(record$family %||% "", "lognormal")) {
    .rls_lognormal_effect_provenance(record, focal, conditioning, at, confidence_level,
      means_table = is.factor(reference_data[[focal]]) || interaction_type == "simple_numeric")
  } else record$analysis_provenance
  if (nrow(comparisons) && "link_name" %in% names(comparisons)) {
    response_code <- NULL
    if (identical(record$family %||% "", "lognormal")) {
      attr(comparisons,"scale_note") <- "Note: lognormal response means are arithmetic means pooled on the response scale; exp(pooled log contrast) need not equal the ratio of those pooled arithmetic means."
      arithmetic <- provenance$verification_r_code$effect
      if (!is.null(arithmetic)) response_code <- c("fits <- grid_fits",
        substring(arithmetic,nchar(paste(.rls_lognormal_recipe_base(record),collapse="\n"))+2L),
        "response_means <- reference")
    }
    recipe <- .rls_pairwise_verification_recipe(record, focal, adjust, confidence_level,
      by=if(length(conditioning)) conditioning else NULL, at=at,
      response_code=response_code, multiplier=if(bounded_count) multiplier else 1,
      interactions=!is.null(interaction_contrasts) && nrow(interaction_contrasts)>0)
    if (!is.null(recipe)) provenance$verification_r_code$table <- recipe
  }
  result <- structure(list(
    interaction = term,
    interaction_type = interaction_type,
    focal = focal,
    moderator = moderator,
    conditioning = conditioning,
    all_factor = all(factor_part),
    interpretation = interpretation,
    estimates = estimates,
    comparisons = comparisons,
    interaction_contrasts = interaction_contrasts,
    plot_data = plot_data,
    emm_test_reference = emm_test_reference,
    emm_reference_test_enabled = !is.null(emm_test_reference),
    comparison_adjustment = adjust,
    confidence_level = confidence_level,
    bounded_count = bounded_count,
    quantity = if (bounded_count) quantity else "response",
    trials_variable = if (bounded_count) record$trials_variable %||% "" else "",
    trials_constant = if (bounded_count) record$trials_constant %||% NA_real_ else NA_real_,
    multiple_imputation = metadata$multiple_imputation,
    imputation_count = metadata$imputation_count,
    pooling_method = metadata$pooling_method,
    analysis_provenance = provenance
  ), class = "linkeda_regression_interaction")
  add_diagnostics(result)
}

.rls_regression_interaction_record <- function(record, term,
    confidence_level = .95, adjust = "tukey", grid_points = 25L,
    emm_test_reference = NULL, focal = NULL,
    quantity = "predicted_probability", adjustment = "average_sample",
    presentation = "percentage", group_by = NULL) {
  started <- proc.time()[["elapsed"]]
  .rls_mi_post_estimation_event(record, "interaction", "preparing",
    message = sprintf("Preparing interaction results for %s\u2026", term))
  tryCatch({
    result <- .rls_regression_interaction_record_impl(
      record, term, confidence_level, adjust, grid_points,
      emm_test_reference, focal, quantity, adjustment, presentation, group_by)
    timing <- result$execution_diagnostics
    .rls_mi_post_estimation_event(record, "interaction", "completed",
      status = "completed", elapsed = proc.time()[["elapsed"]] - started,
      message = sprintf(
        "Interaction results complete. EMMs %.2f s; pairwise %.2f s; contrasts %.2f s; total %.2f s.",
        timing$emmeans_seconds, timing$pairwise_contrasts_seconds,
        timing$interaction_contrasts_seconds, timing$total_seconds))
    result
  }, error = function(error) {
    .rls_mi_post_estimation_event(record, "interaction", "failed",
      status = "failed", elapsed = proc.time()[["elapsed"]] - started,
      message = conditionMessage(error))
    stop(error)
  })
}

#' Pooled pairwise comparisons for regression models
#'
#' Multiple-imputation results use the official `emmeans` support for a
#' `mice::mira` object. No LinkEDA pooling formula is used.
#' @param model A fitted LinkEDA regression model.
#' @param term Included categorical or ordered-factor main effect.
#' @param adjust Multiplicity adjustment passed to `emmeans`.
#' @param confidence_level Confidence level.
#' @return A data frame of pairwise comparisons.
#' @export
ls_glm_pairwise <- function(model, term, adjust = "tukey", confidence_level = .95) {
  .rls_regression_pairwise_record(
    .rls_glm_model_record(model), term, "response", adjust, confidence_level
  )
}

#' @rdname ls_glm_pairwise
#' @param scale Result scale, normally `"response"` or `"link"`.
#'   `"probability"` is available for binomial outcomes; `"odds_ratio"`
#'   requires a logit link and `"risk_ratio"` a log link. `"mean_ratio"`
#'   requires a non-binomial log link, and `"multiplicative_ratio"` applies
#'   to lognormal models.
#'   For lognormal models, `"response"` compares conditional medians
#'   (back-transformed log means), not arithmetic means. With multiple
#'   imputations, emmeans combines the log-scale reference grid before
#'   transforming it. `"link"` compares log means.
#'   For beta regression with a logit link, `"link"` compares logits of the
#'   fitted response means; exp(link contrast) compares their mean odds
#'   `mu / (1 - mu)`, not binary-event odds.
#' @export
ls_generalized_linear_model_pairwise <- function(model, term, scale = "response",
                                                 adjust = "tukey", confidence_level = .95) {
  .rls_regression_pairwise_record(
    .rls_generalized_glm_record(model), term, scale, adjust, confidence_level
  )
}

#' @rdname ls_glm_pairwise
#' @export
ls_count_regression_pairwise <- function(model, term, scale = "response",
                                        adjust = "tukey", confidence_level = .95) {
  ls_generalized_linear_model_pairwise(model, term, scale, adjust, confidence_level)
}

#' Pooled interpretation of a regression interaction
#'
#' Numeric-by-factor interactions report pooled simple slopes; factor-by-factor
#' interactions report pooled marginal means and simple contrasts; and
#' numeric-by-numeric interactions report slopes at low, mean, and high values
#' of the moderator. Plot data always contain pooled estimates and confidence
#' intervals from all imputations.
#' @param model A fitted LinkEDA regression model.
#' @param term Included two- or three-way interaction.
#' @param confidence_level Confidence level.
#' @param adjust Multiplicity adjustment for simple comparisons.
#' @param grid_points Number of focal-variable values used for fitted lines.
#' @param emm_test_reference Optional numeric null value for tests of individual
#'   factor-by-factor estimated marginal means. The default `NULL` reports only
#'   estimates, standard errors, and confidence intervals.
#' @param focal Optional variable from `term` to place on the horizontal axis.
#'   The default retains LinkEDA's automatic choice.
#' @param group_by Optional categorical factor whose levels define the first
#'   grouping within a two- or three-factor interaction. Any remaining factor
#'   defines strata. It must differ from `focal`.
#' @param quantity Quantity shown for binary and bounded-count effects. Bounded
#'   count models support `"expected_count"` (the default) and
#'   `"predicted_probability"`.
#' @param adjustment Binary-model covariate-adjustment strategy.
#' @param presentation Binary-model probability display scale.
#' @return A `linkeda_regression_interaction` object.
#' @export
ls_glm_interaction <- function(model, term, confidence_level = .95,
                               adjust = "tukey", grid_points = 25L,
                               emm_test_reference = NULL, focal = NULL,
                               quantity = "predicted_probability",
                               adjustment = "average_sample",
                               presentation = "percentage", group_by = NULL) {
  .rls_regression_interaction_record(
    .rls_glm_model_record(model), term, confidence_level, adjust, grid_points,
    emm_test_reference, focal, quantity, adjustment, presentation, group_by
  )
}

#' @rdname ls_glm_interaction
#' @export
ls_generalized_linear_model_interaction <- function(model, term, confidence_level = .95,
                                                    adjust = "tukey", grid_points = 25L,
                                                    emm_test_reference = NULL,
                                                    focal = NULL,
                                                    quantity = "predicted_probability",
                                                    adjustment = "average_sample",
                                                    presentation = "percentage",
                                                    group_by = NULL) {
  .rls_regression_interaction_record(
    .rls_generalized_glm_record(model), term, confidence_level, adjust, grid_points,
    emm_test_reference, focal, quantity, adjustment, presentation, group_by
  )
}

#' @rdname ls_glm_interaction
#' @export
ls_binary_regression_interaction <- function(model, term, confidence_level = .95,
                                             adjust = "tukey", grid_points = 25L,
                                             emm_test_reference = NULL,
                                             focal = NULL,
                                             quantity = "predicted_probability",
                                             adjustment = "average_sample",
                                             presentation = "percentage",
                                             group_by = NULL) {
  ls_generalized_linear_model_interaction(
    model, term, confidence_level, adjust, grid_points, emm_test_reference, focal,
    quantity, adjustment, presentation, group_by
  )
}

#' @rdname ls_glm_interaction
#' @export
ls_count_regression_interaction <- function(model, term, confidence_level = .95,
                                            adjust = "tukey", grid_points = 25L,
                                            emm_test_reference = NULL,
                                            focal = NULL,
                                            quantity = c(
                                              "expected_count", "predicted_probability",
                                              "overall_expected_score",
                                              "perfect_score_probability",
                                              "conditional_expected_score"
                                            ), group_by = NULL) {
  quantity <- match.arg(quantity)
  ls_generalized_linear_model_interaction(
    model, term, confidence_level, adjust, grid_points, emm_test_reference, focal,
    quantity = quantity, group_by = group_by
  )
}

#' Formal contrasts of factor-by-factor regression interactions
#'
#' Constructs a complete difference of differences within every fitted model.
#' For multiple-imputation models, each scalar contrast and its variance from
#' the full fitted covariance matrix are combined by `mice::pool.scalar()`.
#' @param model A fitted LinkEDA regression model.
#' @param term Included all-factor interaction term.
#' @param contrast_factor Factor A, whose simple contrast is compared.
#' @param comparison_factor Factor B, across whose levels the simple contrast
#'   is compared.
#' @param conditioning_factors Remaining interaction factors that define strata.
#' @param contrast_method,comparison_method One of `"pairwise"`,
#'   `"reference"`, or `"consecutive"`.
#' @param contrast_reference,comparison_reference Reference categories used by a
#'   `"reference"` method.
#' @param level_order Optional named list containing the visible level order for
#'   either factor. Reversing one order reverses the corresponding contrast.
#' @param confidence_level Confidence level.
#' @param scale `"response"` or `"link"` for generalized models.
#' @return A `linkeda_interaction_contrasts` data frame.
#' @export
ls_glm_interaction_contrasts <- function(
    model, term, contrast_factor = NULL, comparison_factor = NULL,
    conditioning_factors = NULL, contrast_method = "pairwise",
    comparison_method = "pairwise", contrast_reference = NULL,
    comparison_reference = NULL, level_order = NULL,
    confidence_level = .95, scale = "response") {
  .rls_regression_interaction_contrasts_record(
    .rls_glm_model_record(model), term, contrast_factor, comparison_factor,
    conditioning_factors, contrast_method, comparison_method,
    contrast_reference, comparison_reference, level_order,
    confidence_level, scale
  )
}

#' @rdname ls_glm_interaction_contrasts
#' @export
ls_generalized_linear_model_interaction_contrasts <- function(
    model, term, contrast_factor = NULL, comparison_factor = NULL,
    conditioning_factors = NULL, contrast_method = "pairwise",
    comparison_method = "pairwise", contrast_reference = NULL,
    comparison_reference = NULL, level_order = NULL,
    confidence_level = .95, scale = "response") {
  .rls_regression_interaction_contrasts_record(
    .rls_generalized_glm_record(model), term, contrast_factor, comparison_factor,
    conditioning_factors, contrast_method, comparison_method,
    contrast_reference, comparison_reference, level_order,
    confidence_level, scale
  )
}

#' @rdname ls_glm_interaction_contrasts
#' @export
ls_binary_regression_interaction_contrasts <- function(...) {
  ls_generalized_linear_model_interaction_contrasts(...)
}

#' @rdname ls_glm_interaction_contrasts
#' @export
ls_count_regression_interaction_contrasts <- function(...) {
  ls_generalized_linear_model_interaction_contrasts(...)
}

.rls_post_estimation_wire_text <- function(value) {
  value <- as.character(value %||% "")[[1L]]
  gsub("[\t\r\n]", " ", value)
}

.rls_post_estimation_wire_number <- function(value) {
  value <- suppressWarnings(as.numeric(value)[[1L]])
  if (is.finite(value)) format(value, digits = 17L, scientific = TRUE, trim = TRUE) else "NA"
}

.rls_pairwise_native_payload <- function(result) {
  if (!is.data.frame(result) || !nrow(result)) {
    stop("No estimable pooled pairwise comparisons were returned.", call. = FALSE)
  }
  labels <- as.character(result$contrast %||% rep("", nrow(result)))
  levels <- strsplit(labels, " - ", fixed = TRUE)
  method <- unique(as.character(result$pooling_method %||% "emmeans"))[[1L]]
  adjustment <- unique(as.character(result$adjustment %||% "tukey"))[[1L]]
  lines <- c("OK", paste("METHOD", .rls_post_estimation_wire_text(method),
                         .rls_post_estimation_wire_text(adjustment), sep = "\t"))
  for (i in seq_len(nrow(result))) {
    pair <- if (all(c("category_first","category_second") %in% names(result)))
      c(result$category_first[[i]],result$category_second[[i]]) else levels[[i]]
    if (length(pair) < 2L) pair <- c(labels[[i]], "")
    estimate <- if ("ratio" %in% names(result)) result$ratio[[i]] else result$pooled_estimate[[i]]
    conf_low <- if ("ratio_conf_low" %in% names(result)) result$ratio_conf_low[[i]] else result$pooled_conf_low[[i]]
    conf_high <- if ("ratio_conf_high" %in% names(result)) result$ratio_conf_high[[i]] else result$pooled_conf_high[[i]]
    lines <- c(lines, paste(
      "ROW", .rls_post_estimation_wire_text(labels[[i]]),
      .rls_post_estimation_wire_text(pair[[1L]]),
      .rls_post_estimation_wire_text(pair[[2L]]),
      .rls_post_estimation_wire_number(estimate),
      .rls_post_estimation_wire_number(result$pooled_std_error[[i]]),
      .rls_post_estimation_wire_number(result$pooled_df[[i]]),
      .rls_post_estimation_wire_number(result$pooled_statistic[[i]]),
      .rls_post_estimation_wire_number(result$pooled_p_value[[i]]),
      .rls_post_estimation_wire_number(conf_low),
      .rls_post_estimation_wire_number(conf_high),
      sep = "\t"
    ))
  }
  if ("link_name" %in% names(result)) {
    adjusted <- !identical(tolower(adjustment), "none")
    report <- c(paste("Interaction term:", result$term[[1L]]), "Type: pairwise",
      paste("Confidence level:", 100*(attr(result,"confidence_level") %||% .95), "%"),
      if (adjusted) paste("Multiple comparisons: emmeans with", adjustment, "adjustment")
        else "Multiple comparisons: emmeans, unadjusted",
      paste("Imputations:", result$imputation_count[[1L]]),
      paste("Pooling:", result$pooling_method[[1L]]),
      .rls_pairwise_scale_notes(result, adjusted=adjusted), "",
      .rls_pairwise_scale_table_lines(result, attr(result,"confidence_level") %||% .95,
        adjusted=adjusted))
    lines <- c(lines, "PAIRWISE_SCALES_V1", as.character(length(report)), report)
  }
  provenance<-attr(result,"analysis_provenance",exact=TRUE)
  if(!is.null(provenance))lines<-c(lines,.rls_analysis_provenance_payload(list(analysis_provenance=provenance)))
  lines
}

.rls_effect_numeric_legend_labels <- function(values, minimum_decimals = 2L,
                                              maximum_decimals = 6L) {
  values <- as.numeric(values)
  labels <- as.character(values)
  finite <- is.finite(values)
  if (!any(finite)) return(labels)

  trim_fraction <- function(x) {
    x <- sub("(\\.[0-9]*?)0+$", "\\1", x)
    x <- sub("\\.$", "", x)
    x[x %in% c("-0", "+0")] <- "0"
    x
  }
  distinct_values <- length(unique(values[finite]))
  for (decimals in seq.int(minimum_decimals, maximum_decimals)) {
    candidate <- trim_fraction(formatC(
      values[finite], format = "f", digits = decimals
    ))
    if (length(unique(candidate)) == distinct_values) {
      labels[finite] <- candidate
      return(labels)
    }
  }

  # Very close or extreme values are clearer in compact significant-digit
  # notation than as long fixed-decimal strings. Increase precision only as
  # far as necessary to keep different plotted series distinguishable.
  for (digits in seq.int(4L, 10L)) {
    candidate <- formatC(values[finite], format = "g", digits = digits)
    if (length(unique(candidate)) == distinct_values) {
      labels[finite] <- candidate
      return(labels)
    }
  }
  labels
}

.rls_effect_conditioning_labels <- function(tab, conditioning) {
  if (!length(conditioning)) return(character(nrow(tab)))
  display <- lapply(conditioning, function(variable) {
    values <- tab[[variable]]
    if (is.numeric(values)) .rls_effect_numeric_legend_labels(values) else as.character(values)
  })
  names(display) <- conditioning
  display <- as.data.frame(display, stringsAsFactors = FALSE, check.names = FALSE)
  vapply(seq_len(nrow(display)), function(row) {
    paste(paste(conditioning, unlist(display[row, , drop = FALSE], use.names = FALSE),
                sep = " = "), collapse = " \u00B7 ")
  }, character(1L))
}

.rls_interaction_native_plot_payload <- function(result) {
  if (!inherits(result, "linkeda_regression_interaction")) {
    stop("Invalid regression-interaction result.", call. = FALSE)
  }
  tab <- result$plot_data
  if (!is.data.frame(tab) || !nrow(tab)) {
    stop("No pooled fitted interaction values were returned.", call. = FALSE)
  }
  focal <- result$focal
  moderator <- result$moderator
  conditioning <- result$conditioning %||% moderator
  if (!all(c(focal, conditioning, "pooled_estimate") %in% names(tab))) {
    stop("The pooled interaction grid is incomplete.", call. = FALSE)
  }
  focal_factor <- is.factor(tab[[focal]]) || is.character(tab[[focal]])
  focal_labels <- as.character(tab[[focal]])
  focal_levels <- if (focal_factor) unique(focal_labels) else character()
  focal_x <- if (focal_factor) match(focal_labels, focal_levels) else as.numeric(tab[[focal]])
  moderator_labels <- if (length(conditioning)) {
    .rls_effect_conditioning_labels(tab, conditioning)
  } else {
    rep(paste(focal, "effect"), nrow(tab))
  }
  binary_probability <- isTRUE(result$binary_probability)
  display_multiplier <- if (binary_probability &&
      identical(result$presentation %||% "percentage", "percentage")) 100 else 1
  lines <- c(
    "OK",
    paste("META", .rls_post_estimation_wire_text(result$interaction),
          .rls_post_estimation_wire_text(result$interaction_type),
          .rls_post_estimation_wire_text(focal),
          .rls_post_estimation_wire_text(moderator),
          as.integer(result$imputation_count),
          vapply(conditioning, .rls_post_estimation_wire_text, character(1L)),
          sep = "\t"),
    paste("CONFIDENCE", .rls_post_estimation_wire_number(
      result$confidence_level %||% .95
    ), sep = "\t")
  )
  if (binary_probability) {
    lines <- c(lines, paste(
      "BINARY",
      .rls_post_estimation_wire_text(result$event %||% "event"),
      .rls_post_estimation_wire_text(result$quantity %||% "predicted_probability"),
      .rls_post_estimation_wire_text(result$adjustment_mode %||% "average_sample"),
      .rls_post_estimation_wire_text(result$presentation %||% "percentage"),
      sep = "\t"
    ))
  }
  if (isTRUE(result$bounded_count)) {
    lines <- c(lines, paste(
      "BOUNDED",
      .rls_post_estimation_wire_text(result$quantity %||% "expected_count"),
      .rls_post_estimation_wire_text(result$trials_variable %||% ""),
      .rls_post_estimation_wire_number(result$trials_constant %||% NA_real_),
      if (isTRUE(result$ceiling_hurdle)) "1" else "0",
      sep = "\t"
    ))
  }
  if (length(focal_levels)) {
    lines <- c(lines, vapply(seq_along(focal_levels), function(i) paste(
      "TICK", i, .rls_post_estimation_wire_text(focal_levels[[i]]), sep = "\t"
    ), character(1L)))
  }
  lines <- c(lines, vapply(seq_len(nrow(tab)), function(i) paste(
    "ROW",
    .rls_post_estimation_wire_text(moderator_labels[[i]]),
    .rls_post_estimation_wire_number(focal_x[[i]]),
    .rls_post_estimation_wire_number(tab$pooled_estimate[[i]] * display_multiplier),
    .rls_post_estimation_wire_number(tab$pooled_conf_low[[i]] * display_multiplier),
    .rls_post_estimation_wire_number(tab$pooled_conf_high[[i]] * display_multiplier),
    sep = "\t"
  ), character(1L)))
  c(lines, .rls_analysis_provenance_payload(result))
}

.rls_partial_native_plot_payload <- function(result) {
  if (!inherits(result, "linkeda_regression_partial_plot")) stop("Invalid partial-regression result.", call. = FALSE)
  tab <- result$rows
  if (!is.data.frame(tab) || !nrow(tab)) stop("No finite partial-regression values were returned.", call. = FALSE)
  lines <- c("OK", paste("META", .rls_post_estimation_wire_text(result$term),
                         .rls_post_estimation_wire_text(result$residual_type),
                         as.integer(result$imputation_count),
                         .rls_post_estimation_wire_text(result$contribution_scale %||% ""), sep = "\t"))
  tab <- tab[is.finite(tab$contribution) & is.finite(tab$partial_residual), , drop = FALSE]
  c(lines, paste("RESIDUALS", paste(result$available_residual_types, collapse = "\t"), sep = "\t"),
    paste("TERMS", paste(result$available_terms, collapse = "\t"), sep = "\t"),
    vapply(seq_len(nrow(tab)), function(i) paste(
    "ROW", as.integer(tab$imputation[[i]]), as.integer(tab$row_id[[i]]),
    .rls_post_estimation_wire_number(tab$contribution[[i]]),
    .rls_post_estimation_wire_number(tab$partial_residual[[i]]), sep = "\t"
  ), character(1L)), .rls_analysis_provenance_payload(result))
}

.rls_interaction_native_report <- function(result) {
  if (!inherits(result, "linkeda_regression_interaction")) {
    stop("Invalid regression-interaction result.", call. = FALSE)
  }
  confidence_label <- paste0(format(100 * (result$confidence_level %||% .95), trim = TRUE), "%")
  p_text <- function(value) {
    value <- suppressWarnings(as.numeric(value)[[1L]])
    if (!is.finite(value)) return("\u2014")
    if (value < .001) return("< .001")
    sub("^0", "", sprintf("%.3f", value))
  }
  n_text <- function(value, digits = 4L) {
    value <- suppressWarnings(as.numeric(value)[[1L]])
    if (is.finite(value)) sprintf(paste0("%.", digits, "f"), value) else "\u2014"
  }
  if (isTRUE(result$binary_probability)) {
    identity_text <- function(values) {
      if (!is.numeric(values)) return(as.character(values))
      labels <- .rls_numeric_identity_labels(values)
      labels <- sub("(\\.[0-9]*[1-9])0+$", "\\1", labels)
      sub("\\.0+$", "", labels)
    }
    presentation <- result$presentation %||% "percentage"
    multiplier <- if (identical(presentation, "percentage")) 100 else 1
    quantity <- result$quantity %||% "predicted_probability"
    adjustment_label <- if (identical(
      result$adjustment_mode %||% "average_sample", "average_sample"
    )) "Average over analysis sample" else "Reference profile"
    tab <- if (identical(quantity, "probability_difference")) {
      result$comparisons
    } else {
      result$estimates
    }
    conditioning <- result$conditioning %||% character()
    difference <- identical(quantity, "probability_difference")
    identity_columns <- if (difference) c(conditioning, "contrast") else
      c(result$focal, conditioning)
    identity_columns <- identity_columns[identity_columns %in% names(tab)]
    identity_headers <- ifelse(identity_columns == "contrast", "Comparison",
                               identity_columns)
    numeric_conditioning_notes <- vapply(conditioning, function(column) {
      if (!column %in% names(tab) || !is.numeric(tab[[column]])) return("")
      labels <- unique(identity_text(tab[[column]]))
      paste0("Numeric conditioning values for ", column, ": ",
             paste(labels, collapse = ", "), ".")
    }, character(1L))
    numeric_conditioning_notes <- numeric_conditioning_notes[nzchar(numeric_conditioning_notes)]
    estimate_header <- if (difference) {
      if (identical(presentation, "percentage"))
        "Difference (percentage points)" else "Probability difference"
    } else if (identical(presentation, "percentage")) {
      "Predicted probability (%)"
    } else {
      "Predicted probability"
    }
    headers <- c(identity_headers, estimate_header, "SE")
    if (difference) headers <- c(headers, "Statistic", "p")
    headers <- c(headers,
      paste("Lower", confidence_label),
      paste("Upper", confidence_label))
    rows <- if (is.data.frame(tab) && nrow(tab)) {
      identity_values <- lapply(identity_columns, function(column) identity_text(tab[[column]]))
      vapply(seq_len(nrow(tab)), function(i) {
        fields <- c(
          vapply(identity_values, function(values) {
            .rls_post_estimation_wire_text(values[[i]])
          }, character(1L)),
          n_text(tab$pooled_estimate[[i]] * multiplier),
          n_text(tab$pooled_std_error[[i]] * multiplier)
        )
        if (difference) fields <- c(
          fields, n_text(tab$pooled_statistic[[i]], 3L),
          p_text(tab$pooled_p_value[[i]])
        )
        paste(c(
          fields,
          n_text(tab$pooled_conf_low[[i]] * multiplier),
          n_text(tab$pooled_conf_high[[i]] * multiplier)
        ), collapse = "\t")
      }, character(1L))
    } else character()
    return(c(
      paste("Effect term:", result$interaction),
      paste("Event:", result$event %||% "event"),
      paste("Quantity:", if (difference) "Probability difference" else
        "Predicted probability"),
      paste("Adjustment:", adjustment_label),
      paste("Presentation:", if (identical(presentation, "percentage"))
        if (difference) "Percentage points" else "Percentage" else
        if (difference) "Probability difference (-1 to 1)" else "Probability (0 to 1)"),
      paste("Confidence level:", confidence_label),
      if (result$imputation_count > 1L) paste("Imputations:", result$imputation_count),
      if (result$imputation_count > 1L) paste("Pooling:", result$pooling_method),
      numeric_conditioning_notes,
      "",
      paste("Interpretation:", result$interpretation),
      "",
      if (difference) "Probability differences" else "Predicted probabilities",
      paste(headers, collapse = "\t"), rows, "",
      if (difference) paste0(
        "Note: each comparison is oriented as shown (first minus second); ",
        "zero is the no-difference reference."
      ) else paste0(
        "Note: probabilities describe response = ", result$event %||% "event",
        "; they are not tests against zero."
      ),
      paste0(
        "Note: every estimate is calculated in R from the complete fitted model; ",
        "the native interface only displays the returned values."
      )
    ))
  }
  table_lines <- function(title, tab, include_test = TRUE, adjusted = FALSE,
                          identity_columns = NULL, identity_headers = NULL) {
    if (!is.data.frame(tab) || !nrow(tab)) return(character())
    structured_identity <- !is.null(identity_columns) &&
      length(identity_columns) && all(identity_columns %in% names(tab))
    if (structured_identity) {
      identity_values <- lapply(identity_columns, function(column) {
        if (is.numeric(tab[[column]]))
          .rls_numeric_identity_labels(tab[[column]]) else
          as.character(tab[[column]])
      })
      if (is.null(identity_headers) || length(identity_headers) != length(identity_columns)) {
        identity_headers <- identity_columns
      }
    } else {
      labels <- if ("contrast" %in% names(tab)) as.character(tab$contrast) else {
        identity_columns <- setdiff(
          names(tab),
          c("emmean", "response", "prob", "rate", "null", "SE", "df", "lower.CL", "upper.CL",
            "asymp.LCL", "asymp.UCL", "t.ratio", "z.ratio", "p.value",
            grep("^pooled_|^imputation_count$|^pooling_method$|^interaction$", names(tab), value = TRUE))
        )
        apply(tab[, identity_columns, drop = FALSE], 1L, function(row) {
          paste(paste(identity_columns, row, sep = " = "), collapse = ", ")
        })
      }
      identity_values <- list(labels)
      identity_headers <- "Effect"
    }
    header <- c(identity_headers, "Estimate", "SE")
    if (isTRUE(include_test)) {
      header <- c(header, "Statistic", if (isTRUE(adjusted)) "Adjusted p" else "p")
    }
    header <- c(header, paste("Lower", confidence_label), paste("Upper", confidence_label))
    rows <- vapply(seq_len(nrow(tab)), function(i) {
      fields <- c(
        vapply(identity_values, function(values) {
          .rls_post_estimation_wire_text(values[[i]])
        }, character(1L)),
        n_text(tab$pooled_estimate[[i]]),
        n_text(tab$pooled_std_error[[i]])
      )
      if (isTRUE(include_test)) {
        fields <- c(fields, n_text(tab$pooled_statistic[[i]], 3L),
                    p_text(tab$pooled_p_value[[i]]))
      }
      paste(c(fields, n_text(tab$pooled_conf_low[[i]]),
              n_text(tab$pooled_conf_high[[i]])), collapse = "\t")
    }, character(1L))
    c(title, paste(header, collapse = "\t"), rows, "")
  }
  interaction_contrast_lines <- function(tab) {
    if (!inherits(tab, "linkeda_interaction_contrasts") || !nrow(tab)) {
      return(character())
    }
    metadata <- attr(tab, "interaction_contrast") %||% list()
    condition_columns <- metadata$conditioning_factors %||% character()
    contrast_factor <- tab$contrast_factor[[1L]]
    comparison_factor <- tab$comparison_factor[[1L]]
    effect_link <- if ("effect_link" %in% names(tab)) tab$effect_link[[1L]] else "identity"
    logit_ratio <- identical(tab$scale[[1L]], "link") &&
      identical(effect_link, "logit")
    ratio_label <- if ("ratio_label" %in% names(tab)) tab$ratio_label[[1L]] else
      "Ratio of odds ratios"
    estimate_label <- if (logit_ratio) paste0("Log ", tolower(ratio_label), " (logit)") else
      if (identical(tab$scale[[1L]], "response")) "Response difference of differences" else
        paste0("Difference of differences (", effect_link, " link)")
    headers <- c(
      condition_columns, "Contrast in A", "Contrast in B",
      paste(contrast_factor, "contrast at first", comparison_factor),
      paste(contrast_factor, "contrast at second", comparison_factor),
      estimate_label, "SE", "df", "Statistic", "p",
      paste("Lower", confidence_label), paste("Upper", confidence_label)
    )
    rows <- vapply(seq_len(nrow(tab)), function(i) {
      conditions <- if (length(condition_columns)) {
        vapply(condition_columns, function(column) {
          .rls_post_estimation_wire_text(tab[[column]][[i]])
        }, character(1L))
      } else character()
      paste(c(
        conditions,
        .rls_post_estimation_wire_text(tab$contrast_A[[i]]),
        .rls_post_estimation_wire_text(tab$contrast_B[[i]]),
        n_text(tab$simple_A_at_B_first[[i]]),
        n_text(tab$simple_A_at_B_second[[i]]),
        n_text(tab$pooled_estimate[[i]]),
        n_text(tab$pooled_std_error[[i]]),
        n_text(tab$pooled_df[[i]], 2L),
        n_text(tab$pooled_statistic[[i]], 3L),
        p_text(tab$pooled_p_value[[i]]),
        n_text(tab$pooled_conf_low[[i]]),
        n_text(tab$pooled_conf_high[[i]])
      ), collapse = "\t")
    }, character(1L))
    ratio_lines <- if (logit_ratio) {
      ratio_headers <- c(condition_columns, "Contrast in A", "Contrast in B",
        ratio_label, "SE", paste("Lower", confidence_label),
        paste("Upper", confidence_label))
      ratio_rows <- vapply(seq_len(nrow(tab)), function(i) paste(c(
        if (length(condition_columns)) vapply(condition_columns, function(column)
          .rls_post_estimation_wire_text(tab[[column]][[i]]), character(1L)),
        .rls_post_estimation_wire_text(tab$contrast_A[[i]]),
        .rls_post_estimation_wire_text(tab$contrast_B[[i]]),
        n_text(tab$ratio_of_odds_ratios[[i]]), "\u2014",
        n_text(tab$ratio_of_odds_ratios_conf_low[[i]]),
        n_text(tab$ratio_of_odds_ratios_conf_high[[i]])
      ), collapse = "\t"), character(1L))
      c("Exponentiated interaction contrasts", paste(ratio_headers, collapse = "\t"),
        ratio_rows, "")
    } else character()
    c(
      "Interaction contrasts",
      paste(headers, collapse = "\t"), rows, "",
      ratio_lines,
      paste("Contrast factor A:", tab$contrast_factor[[1L]]),
      paste("Comparison factor B:", tab$comparison_factor[[1L]]),
      paste("Interaction contrast estimand:", tab$effect_scale[[1L]]),
      if (logit_ratio) paste0("The exponentiated interaction contrast is a ",
        tolower(ratio_label), ", not a single odds ratio."),
      if (logit_ratio) "Note: SE and Wald tests apply to the logit contrast; the exponentiated table displays its transformed estimate and confidence interval.",
      if (logit_ratio && identical(ratio_label, "Ratio of mean-odds ratios"))
        "For beta regression, mean odds are mu / (1 - mu); they do not describe binary-event odds.",
      paste("Interaction contrast pooling:", tab$pooling_method[[1L]]),
      paste("Orientation:", metadata$orientation_note %||% paste0(
        "Each comparison is first category minus second category; interaction estimate ",
        "is the first simple contrast minus the second."
      )),
      paste0("Definition: (", if (identical(tab$scale[[1L]], "response")) "mu" else "eta",
        "[A1,B1] - ", if (identical(tab$scale[[1L]], "response")) "mu" else "eta",
        "[A2,B1]) - (", if (identical(tab$scale[[1L]], "response")) "mu" else "eta",
        "[A1,B2] - ", if (identical(tab$scale[[1L]], "response")) "mu" else "eta",
        "[A2,B2])."),
      "Note: estimates quantify statistical differences of differences; no substantive direction or practical importance is inferred.",
      ""
    )
  }
  conditioning <- result$conditioning %||% result$moderator
  numeric_conditions <- conditioning[vapply(conditioning, function(column) {
    column %in% names(result$estimates) &&
      is.numeric(result$estimates[[column]])
  }, logical(1L))]
  numeric_condition_notes <- if (isTRUE(result$ceiling_hurdle)) character() else
    vapply(numeric_conditions, function(column) {
      labels <- unique(.rls_numeric_identity_labels(result$estimates[[column]]))
      paste0("Numeric conditioning values for ", column,
        " (mean \u2212 SD, mean, mean + SD of fitted data, clipped to the observed range): ",
        paste(labels, collapse = ", "), ".")
    }, character(1L))
  estimates_are_emms <- isTRUE(result$all_factor) ||
    isTRUE(result$focal %in% names(result$estimates) &&
      is.factor(result$estimates[[result$focal]])) ||
    startsWith(result$interaction_type, "factor_factor")
  estimates_have_tests <- !estimates_are_emms ||
    isTRUE(result$emm_reference_test_enabled)
  comparisons_are_adjusted <- !identical(
    tolower(result$comparison_adjustment %||% "none"), "none"
  )
  ceiling_hurdle <- isTRUE(result$ceiling_hurdle)
  reference_note <- if (!estimates_are_emms) character() else if (estimates_have_tests) {
    paste("EMM reference test:", n_text(result$emm_test_reference))
  } else if (!ceiling_hurdle) {
    "EMM reference test: None"
  } else character()
  c(
    paste("Interaction term:", result$interaction),
    paste("Type:", result$interaction_type),
    paste("Confidence level:", confidence_label),
    if (ceiling_hurdle) {
      paste0(
        "Method: exact response-scale quantities derived in R from both fitted ",
        "ZABB components"
      )
    } else if (comparisons_are_adjusted) {
      paste("Multiple comparisons:", "emmeans", "with", result$comparison_adjustment,
            "adjustment")
    } else {
      "Multiple comparisons: emmeans, unadjusted"
    },
    if (result$imputation_count > 1L) paste("Imputations:", result$imputation_count),
    if (result$imputation_count > 1L) paste("Pooling:", result$pooling_method),
    reference_note,
    numeric_condition_notes,
    .rls_pairwise_scale_notes(result$comparisons, adjusted=comparisons_are_adjusted),
    "",
    paste("Interpretation:", result$interpretation),
    "",
    table_lines(if (ceiling_hurdle) "Response-scale effect estimates" else
                  if (estimates_are_emms) "Estimated marginal means" else
                    "Simple slopes",
                result$estimates, estimates_have_tests, adjusted = FALSE,
                identity_columns = if (estimates_are_emms) {
                  c(result$focal, conditioning)
                } else if (length(conditioning) > 1L) conditioning else NULL,
                identity_headers = if (estimates_are_emms) {
                  c(result$focal, conditioning)
                } else if (length(conditioning) > 1L) conditioning else NULL),
    if ("link_name" %in% names(result$comparisons))
      .rls_pairwise_scale_table_lines(result$comparisons, result$confidence_level,
        conditioning[conditioning %in% names(result$comparisons)],
        adjusted=comparisons_are_adjusted) else
    table_lines(if (estimates_are_emms) "Pairwise comparisons" else
                  "Pairwise slope comparisons",
                result$comparisons, include_test = TRUE,
                adjusted = comparisons_are_adjusted,
                identity_columns = if (estimates_are_emms) {
                  c(conditioning, "contrast")
                } else c(conditioning[-1L], "contrast"),
                identity_headers = if (estimates_are_emms) {
                  c(conditioning, "Comparison")
                } else c(conditioning[-1L], "Comparison")),
    interaction_contrast_lines(result$interaction_contrasts),
    if (result$imputation_count > 1L) paste0(
      "Note: estimates use every fitted imputation through ", result$pooling_method,
      "; no single-imputation preview or LinkEDA pooling formula is used."
    ),
    if (estimates_are_emms && !ceiling_hurdle) paste0(
      "Note: pairwise comparisons decompose the interaction for interpretation; ",
      "the interaction itself is tested by its interaction term in the model table."
    ) else character(),
    .rls_analysis_provenance_payload(result)
  )
}
