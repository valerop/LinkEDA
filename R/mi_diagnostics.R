# Shared dataset-level diagnostics. The original mids is the source of truth;
# completed tables alone cannot reconstruct the imputation process.
.rls_mi_diagnostic_display_rows <- function(record, diagnostics) {
  if (identical(record$multiple_imputation$analysis_type, "table1"))
    return(.rls_mi_table1_diagnostics(record)$display_rows)
  if ((record$analysis_type %||% "") %in% c("independent_samples_t_test", "paired_samples_t_test") &&
      length(record$multiple_imputation$group_mean_pools))
    return(.rls_mi_comparison_diagnostics(record)$display_rows)
  rows <- record$coefficient_rows
  required <- c("row_type", "display_label", "source_term", "coefficient_name")
  if (!is.data.frame(rows) || !all(required %in% names(rows)) || !nrow(diagnostics)) return(NULL)
  index <- match(rows$coefficient_name, diagnostics$Variable)
  if ("component" %in% names(rows)) {
    component_index <- match(paste(rows$component, rows$coefficient_name, sep=" \u2014 "), diagnostics$Variable)
    index[!is.na(component_index)] <- component_index[!is.na(component_index)]
  }
  structural <- rows$row_type %in% c("factor_parent", "term_parent", "reference", "section_header")
  keep <- structural | !is.na(index)
  if (!any(!is.na(index))) return(NULL)
  result <- data.frame(value_row=ifelse(is.na(index[keep]), -1L, index[keep]-1L),
    row_type=rows$row_type[keep], label=rows$display_label[keep], variable=rows$source_term[keep],
    stringsAsFactors=FALSE)
  result$label[result$row_type=="reference"] <- paste0(result$label[result$row_type=="reference"], " (reference)")
  child <- result$row_type=="coefficient" & result$variable!=result$label & result$variable!="(Intercept)"
  result$row_type[child] <- "factor_level"
  remaining <- which(!seq_len(nrow(diagnostics)) %in% index)
  if (length(remaining)) result <- rbind(result, data.frame(value_row=remaining-1L,
    row_type="coefficient", label=diagnostics$Variable[remaining], variable=diagnostics$Variable[remaining]))
  result
}

.rls_mi_encode_process <- function(mids, name_map=NULL) {
  if (!inherits(mids,"mids")) return("")
  bytes <- memCompress(serialize(list(mids=mids,name_map=name_map),NULL,version=3),"gzip")
  paste(sprintf("%02x",as.integer(bytes)),collapse="")
}

.rls_mi_decode_process <- function(text) {
  if (is.null(text) || !nzchar(text)) return(NULL)
  if (nchar(text) %% 2L || grepl("[^0-9a-fA-F]",text))
    stop("Invalid retained imputation metadata.",call.=FALSE)
  starts <- seq.int(1L,nchar(text),by=2L)
  result <- unserialize(memDecompress(as.raw(strtoi(substring(text,starts,starts+1L),16L)),"gzip"))
  if (!is.list(result) || !inherits(result$mids,"mids"))
    stop("Invalid retained imputation metadata.",call.=FALSE)
  result
}

.rls_mi_process_matches <- function(mids, original, completed, name_map=NULL) {
  if (!inherits(mids,"mids") || mids$m != length(completed)) return(FALSE)
  source <- if (is.data.frame(name_map)) name_map$original_name[match(names(original),name_map$variable_name)] else names(original)
  if (anyNA(source) || !all(source %in% names(mids$data))) return(FALSE)
  same <- function(x,y) {
    if (nrow(x)!=nrow(y) || ncol(x)!=ncol(y)) return(FALSE)
    all(vapply(seq_along(x),function(i) {
      a<-x[[i]]; b<-y[[i]]
      if (is.numeric(a) && is.numeric(b)) isTRUE(all.equal(as.numeric(a),as.numeric(b),tolerance=1e-12))
      else identical(as.character(a),as.character(b))
    },logical(1L)))
  }
  tryCatch(same(mids$data[source],original) && all(vapply(seq_along(completed),function(i)
    same(mice::complete(mids,i)[source],completed[[i]]),logical(1L))),error=function(e)FALSE)
}

.rls_mi_restore_process <- function(record, encoded="") {
  candidates <- list(list(mids=record$mids_object,name_map=record$import_name_map))
  id <- record$imputation_id %||% ""
  if (nzchar(id) && exists(id,envir=.rls_state$missing_imputations,inherits=FALSE)) {
    saved <- get(id,envir=.rls_state$missing_imputations)
    candidates <- c(candidates,list(list(mids=saved$mids_object,name_map=saved$import_name_map)))
  }
  if (nzchar(encoded)) candidates <- c(candidates,list(.rls_mi_decode_process(encoded)))
  for (candidate in candidates) {
    if (.rls_mi_process_matches(candidate$mids,record$original_data,record$completed_datasets,candidate$name_map)) {
      record$mids_object <- candidate$mids
      record$import_name_map <- candidate$name_map
      return(record)
    }
  }
  # Edited or legacy completed data must not inherit unrelated chain statistics.
  record$mids_object <- NULL
  record
}

.rls_mi_dataset_info <- function(dataset, include_missingness=TRUE) {
  record <- if (is.list(dataset) && !is.null(dataset$data)) dataset else .rls_dataset_record(dataset)
  imp <- record$mids_object
  if (!.rls_mi_is_dataset(record))
    stop(sprintf("Dataset '%s' is not a multiple-imputation dataset. Select the imputed dataset to inspect its imputation process. Imputation diagnostics require the original mice::mids object.", record$group), call. = FALSE)
  if (!inherits(imp, "mids"))
    stop(sprintf("Dataset '%s' has no retained original mice::mids object for its current data. Imputation diagnostics require the original mice::mids object. Completed or stacked data alone are insufficient.", record$group), call. = FALSE)
  original <- record$original_data %||% imp$data
  n <- nrow(original); p <- ncol(original)
  map <- record$import_name_map
  source_names <- if (is.data.frame(map)) map$original_name[match(names(original), map$variable_name)] else names(original)
  source_names[is.na(source_names)] <- names(original)[is.na(source_names)]
  summary <- by_variable <- patterns <- NULL
  if (include_missingness) {
    mask <- is.na(original)
    blocks <- imp$blocks %||% setNames(as.list(names(imp$data)), names(imp$data))
    methods <- vapply(source_names, function(variable) {
      selected <- names(blocks)[vapply(blocks, function(block) variable %in% block, logical(1L))]
      if (!length(selected)) return("")
      paste(unique(as.character(imp$method[selected])), collapse = "; ")
    }, character(1L))
    by_variable <- data.frame(Variable = names(original), N = n,
      Observed = colSums(!mask), Missing = colSums(mask),
      `Missing %` = 100 * colMeans(mask), `Imputation method` = methods,
      check.names = FALSE, row.names = NULL)
    pattern <- apply(mask, 1L, function(row) paste(names(original)[row], collapse = ", "))
    pattern[!nzchar(pattern)] <- "(complete case)"
    counts <- sort(table(pattern), decreasing = TRUE)
    patterns <- data.frame(`Missing variables` = names(counts), Cases = as.integer(counts),
      `Cases %` = 100 * as.integer(counts) / n, check.names = FALSE, row.names = NULL)
    complete <- sum(rowSums(mask) == 0L)
    summary <- data.frame(Variable = c("Observations", "Variables", "Imputations", "Originally missing cells",
      "Originally missing cells (%)", "Complete cases", "Complete cases (%)", "Cases with missing values", "Cases with missing values (%)"),
      Value = c(n, p, imp$m, sum(mask), 100 * mean(mask), complete, 100 * complete / n, n-complete, 100 * (n-complete) / n))
  }
  list(is_mids = TRUE, m = imp$m, original_data = original, where = imp$where,
    completed_datasets=record$completed_datasets,
    missingness_summary = summary, missingness_by_variable = by_variable,
    missingness_patterns = patterns, methods = imp$method, blocks = imp$blocks,
    predictor_matrix = imp$predictorMatrix, visit_sequence = imp$visitSequence,
    logged_events = imp$loggedEvents, chain_mean = imp$chainMean, chain_var = imp$chainVar,
    iterations = imp$iteration, seed = imp$seed, mids = imp, source_names = source_names,
    name_map = setNames(source_names, names(original)), dataset_id = record$group,
    dataset_version = record$data_version %||% 1L)
}

.rls_mi_chain_data <- function(info, variable, statistic = c("mean", "variance")) {
  statistic <- match.arg(statistic)
  chain <- if(statistic == "mean") info$chain_mean else info$chain_var
  if (length(dim(chain)) != 3L || any(dim(chain) == 0L)) return(data.frame())
  source <- info$name_map[[variable]] %||% variable
  index <- match(source, dimnames(chain)[[1L]])
  if (is.na(index)) return(data.frame())
  out <- do.call(rbind, lapply(seq_len(dim(chain)[3L]), function(i)
    data.frame(Iteration=seq_len(dim(chain)[2L]), Value=as.numeric(chain[index,,i]), Imputation=paste("Imputation", i))))
  out[is.finite(out$Value),,drop=FALSE]
}

.rls_mi_convergence_data <- function(info) {
  rows <- list()
  for (variable in names(info$original_data)) for (statistic in c("Mean", "SD")) {
    chain <- if (statistic == "Mean") info$chain_mean else info$chain_var
    source <- info$name_map[[variable]]
    row <- data.frame(Variable=variable, Statistic=statistic, Iterations=NA_integer_,
      Chains=NA_integer_, `Lag-1 autocorrelation`=NA_real_, `PSRF (classical)`=NA_real_,
      `PSRF upper 95%`=NA_real_, Status="", check.names=FALSE)
    if (length(dim(chain)) != 3L || !source %in% dimnames(chain)[[1L]]) {
      row$Status <- "Chain statistics unavailable"
    } else {
      values <- matrix(chain[source,,,drop=FALSE], nrow=dim(chain)[2L], ncol=dim(chain)[3L])
      if (statistic == "SD") values <- sqrt(values)
      row$Iterations <- nrow(values); row$Chains <- ncol(values)
      if (nrow(values) < 4L || ncol(values) < 2L) {
        row$Status <- "Requires at least 4 iterations and 2 chains"
      } else if (any(!is.finite(values))) {
        row$Status <- "Chain statistics unavailable or non-finite"
      } else if (any(apply(values, 2L, stats::var) == 0)) {
        row$Status <- "Constant chain; convergence cannot be assessed"
      } else if (!requireNamespace("coda", quietly=TRUE)) {
        row$Status <- "Install package 'coda' to compute convergence diagnostics"
      } else {
        diagnostics <- tryCatch({
          chains <- coda::mcmc.list(lapply(seq_len(ncol(values)), function(i) coda::mcmc(values[,i])))
          list(ac=mean(vapply(chains, function(x) as.numeric(coda::autocorr.diag(x, lags=1)), numeric(1L))),
            psrf=coda::gelman.diag(chains, confidence=.95, transform=FALSE,
              autoburnin=FALSE, multivariate=FALSE)$psrf[1,])
        }, error=function(e) e)
        if (inherits(diagnostics, "error")) row$Status <- conditionMessage(diagnostics) else {
          row$`Lag-1 autocorrelation` <- diagnostics$ac
          row$`PSRF (classical)` <- diagnostics$psrf[1L]
          row$`PSRF upper 95%` <- diagnostics$psrf[2L]
          row$Status <- "Inspect alongside chain traces"
        }
      }
    }
    rows[[length(rows)+1L]] <- row
  }
  do.call(rbind, rows)
}

.rls_mi_distribution_data <- function(info, variable, imputation_start=1L) {
  if (!variable %in% names(info$original_data)) stop("Unknown diagnostic variable.", call.=FALSE)
  if(length(imputation_start)!=1L || !is.numeric(imputation_start) || !is.finite(imputation_start) ||
     imputation_start!=as.integer(imputation_start) || imputation_start<1L || imputation_start>info$m)
    stop("imputation_start must identify an available imputation.",call.=FALSE)
  source <- info$name_map[[variable]]
  original <- info$original_data[[variable]]
  missing <- is.na(original)
  # Reuse the completed columns already registered by the R backend. Process
  # one imputation at a time, never reconstruct or retain m full data frames.
  values <- function(i) {
    complete <- info$completed_datasets[[i]]
    if (!is.null(complete) && variable %in% names(complete)) return(complete[[variable]][missing])
    mice::complete(info$mids, i)[[source]][missing]
  }
  if (is.numeric(original) && !is.factor(original)) {
    indices <- seq.int(as.integer(imputation_start), min(info$m, imputation_start+4L))
    max_steps <- 512L
    simplified <- FALSE
    ecdf_points <- function(x, label) {
      x <- x[is.finite(x)]; if (!length(x)) return(data.frame())
      distribution <- stats::ecdf(x)
      grid <- stats::knots(distribution)
      if (length(grid)>max_steps) {
        grid <- unique(as.numeric(stats::quantile(x, probs=seq(0,1,length.out=max_steps),
          type=1, names=FALSE)))
        simplified <<- TRUE
      }
      cumulative <- as.numeric(distribution(grid))
      data.frame(Value=rep(grid, each=2L),
        Proportion=as.vector(rbind(c(0, head(cumulative,-1L)), cumulative)), Series=label)
    }
    out <- do.call(rbind, c(list(ecdf_points(original[!missing], "Observed")),
      lapply(indices, function(i) ecdf_points(values(i), paste("Imputed", i)))))
    view <- list(variable=variable,total=info$m,first=min(indices),last=max(indices),
      max_steps=max_steps,simplified=simplified)
    return(list(kind="continuous", data=out, view=view))
  }
  levels <- if (is.factor(original)) levels(original) else unique(as.character(original[!missing]))
  for (i in seq_len(info$m)) levels <- union(levels,as.character(values(i)))
  levels <- levels[!is.na(levels)]
  proportions <- function(x) {
    x <- as.character(x); x <- x[!is.na(x)]
    if (!length(x)) return(rep(NA_real_, length(levels)))
    as.numeric(table(factor(x, levels=levels))) / length(x)
  }
  out <- data.frame(Variable=levels, Observed=proportions(original[!missing]), check.names=FALSE)
  for(i in seq_len(info$m)) out[[paste("Imputed", i)]] <- proportions(values(i))
  list(kind="categorical", data=out)
}

.rls_mi_diagnostic_recipe <- function(section, variable="", max_patterns=20L, plot_view=NULL) {
  intro <- c("# The prepared RDS contains the original mids and its retained metadata.",
    "prepared <- base::readRDS(verification_data_path)", "imp <- prepared$mids",
    'stopifnot(inherits(imp, "mids"))', "original <- prepared$original_data", "missing <- is.na(original)")
  body <- switch(section,
    summary=c("n <- nrow(original); p <- ncol(original)",
      "c(observations=n, variables=p, imputations=imp$m, missing_cells=sum(missing), missing_percent=100*mean(missing), complete_cases=sum(rowSums(missing)==0), complete_percent=100*mean(rowSums(missing)==0), incomplete_cases=sum(rowSums(missing)>0), incomplete_percent=100*mean(rowSums(missing)>0))"),
    variables=c("data.frame(variable=names(original), N=nrow(original), observed=colSums(!missing), missing=colSums(missing), missing_percent=100*colMeans(missing))", "imp$method; imp$blocks"),
    patterns=c("# Public mice pattern table (different ordering/layout, same cases and original missingness).", "mice::md.pattern(original, plot=FALSE)"),
    model=c("imp$method", "imp$predictorMatrix", "imp$blocks", "imp$visitSequence", "imp$iteration", "imp$seed", "imp$formulas", "imp$post"),
    events="imp$loggedEvents",
    convergence=c(
      "if (!requireNamespace('coda', quietly=TRUE)) stop(\"Install package 'coda' for this check.\")",
      "# Classical Gelman-Rubin PSRF, not the rank-normalized split R-hat.",
      "# Use all retained iterations (no automatic burn-in); inspect traces too.",
      "convergence <- list()",
      "for (variable in names(original)) for (statistic in c('Mean','SD')) {",
      "  chain <- if (statistic=='Mean') imp$chainMean else imp$chainVar",
      "  source <- prepared$name_map[[variable]]",
      "  if (length(dim(chain))!=3L || !source %in% dimnames(chain)[[1L]]) next",
      "  values <- matrix(chain[source,,,drop=FALSE], nrow=dim(chain)[2L], ncol=dim(chain)[3L])",
      "  if (statistic=='SD') values <- sqrt(values)",
      "  if (nrow(values)<4L || ncol(values)<2L || any(!is.finite(values))) next",
      "  if (any(apply(values,2,stats::var)==0)) next",
      "  chains <- coda::mcmc.list(lapply(seq_len(ncol(values)), function(i) coda::mcmc(values[,i])))",
      "  ac <- mean(vapply(chains, function(x) as.numeric(coda::autocorr.diag(x,lags=1)), numeric(1)))",
      "  psrf <- coda::gelman.diag(chains, confidence=.95, transform=FALSE, autoburnin=FALSE, multivariate=FALSE)$psrf[1,]",
      "  convergence[[paste(variable,statistic)]] <- data.frame(variable,statistic,ac,PSRF=psrf[1],upper=psrf[2])",
      "}",
      "# Variables with absent, short, non-finite or constant chains are unavailable, not converged.",
      "if (length(convergence)) do.call(rbind, convergence) else message('Convergence diagnostics unavailable')"),
    chain_mean=,chain_variance=c(paste0("variable <- ", .rls_r_string_literal(variable)),
      paste0("chain <- imp$", if(section=="chain_mean") "chainMean" else "chainVar"),
      'if (length(dim(chain)) != 3L || !variable %in% dimnames(chain)[[1L]]) stop("Convergence information unavailable")',
      "values <- matrix(chain[variable,,], nrow=dim(chain)[2L], ncol=dim(chain)[3L])",
      'graphics::matplot(seq_len(nrow(values)), values, type="l", xlab="Iteration", ylab="Imputed chain statistic")'),
    distributions=c(paste0("variable <- ", .rls_r_string_literal(variable)),
      "x <- imp$data[[variable]]; missing <- is.na(x)",
      if(is.null(plot_view)) "indices <- seq_len(imp$m)" else paste0("indices <- ",plot_view$first,":",plot_view$last),
      "# The native view shows at most five imputations and 512 ECDF steps per curve.",
      "# This public R check draws the exact ECDF of the same values, without display simplification.",
      "imputed <- lapply(indices, function(i) mice::complete(imp, i)[[variable]][missing])",
      "if (is.numeric(x)) {",
      '  graphics::plot(stats::ecdf(x[!missing]), main=variable)',
      '  for(i in seq_along(imputed)) if(any(is.finite(imputed[[i]]))) graphics::lines(stats::ecdf(imputed[[i]]), col=i+1)',
      "} else {",
      "  print(prop.table(table(x[!missing])))",
      "  print(lapply(imputed, function(z) prop.table(table(z))))",
      "}"))
  paste(c(intro,body),collapse="\n")
}

.rls_mi_diagnostic_table <- function(dataset, frame, title, section, info, variable="", notes=character(), plot_view=NULL) {
  frame <- as.data.frame(frame, check.names=FALSE, stringsAsFactors=FALSE)
  if (!ncol(frame)) frame <- data.frame(Variable="Information unavailable", Details="Not retained in this mids object.")
  names(frame)[1L] <- "Variable"
  if (!nrow(frame)) frame <- as.data.frame(setNames(lapply(names(frame), function(n) if(n=="Variable") "No entries" else ""), names(frame)),check.names=FALSE)
  display <- as.data.frame(lapply(frame, function(x) if(is.numeric(x))
    vapply(x, function(value) if(is.na(value)) "\u2014" else format(signif(value,5),trim=TRUE), character(1))
    else as.character(x)),check.names=FALSE)
  if (section %in% c("summary", "variables", "patterns"))
    notes <- c("Missingness is evaluated on the original data before imputation, using the retained imputation object.", notes)
  rows <- lapply(seq_len(nrow(display)),function(i) list(row_index=i,row_type="text",variable=display$Variable[i],level="",
    values=as.list(display[i,setdiff(names(display),"Variable"),drop=FALSE]),p="",test="",detail=""))
  record <- list(id=paste0("mi_diagnostics:",dataset$group,":",section,":",variable), dataset_id=dataset$group, group=dataset$group,
    title=title, variables=names(info$original_data), variable_types=setNames(rep("numeric",ncol(info$original_data)),names(info$original_data)),
    table_type="mi_diagnostics", display_table=display, summary_statistics=rows,
    data=dataset$original_data, footnotes=c(notes,"Imputation diagnostics describe the imputation process; they do not prove that imputations are correct."),
    display_options=list(show_p=FALSE,show_test=FALSE),data_scope=list(kind="all",description="Original data before imputation",total_n=nrow(info$original_data),rows=seq_len(nrow(info$original_data))))
  # Pages of the same immutable dataset share their prepared input on disk.
  previous <- get0(record$id,envir=.rls_state$table1_tables,inherits=FALSE)
  prior <- previous$analysis_provenance
  source <- prior$prepared_data_path %||% ""
  if (!identical(prior$dataset_version,as.integer(info$dataset_version)) || !file.exists(source)) {
    source <- tempfile("linkeda-mi-diagnostic-input-",fileext=".rds")
    saveRDS(list(mids=info$mids,original_data=info$original_data,name_map=info$name_map),source)
  }
  record <- .rls_attach_analysis_provenance(record,
    paste0("diagnostics <- LinkEDA::ls_imputation_diagnostics(",.rls_r_string_literal(dataset$group),", section=",.rls_r_string_literal(section),", variable=",.rls_r_string_literal(variable),", native=FALSE",
      if(!is.null(plot_view))paste0(", imputation_start=",plot_view$first),")"),
    title=title,output_code=list(table="diagnostics"),
    verification_code=list(table=.rls_mi_diagnostic_recipe(section, info$name_map[[variable]] %||% variable, plot_view=plot_view)),verification_variables=names(info$original_data))
  record$diagnostic_plot_view <- plot_view
  record$analysis_provenance$prepared_data_path <- source
  .rls_assign_table1(record)
  record
}

#' Inspect the original missing-data and imputation process
#' @param dataset Registered MI dataset (or NULL for the active dataset).
#' @param section Summary, variables, patterns, model, events, chain_mean,
#'   chain_variance, convergence, or distributions.
#' @param variable Variable for convergence/distribution diagnostics.
#' @param max_patterns Maximum patterns to display; Inf displays all.
#' @param sort_missing Sort the variable table by decreasing missing percentage.
#' @param native Show the native diagnostic table/plot.
#' @param imputation_start First imputation in a numeric distribution view (up to five shown).
#' @return Invisibly a list with the diagnostic inputs, table and plotted data.
#' @export
ls_imputation_diagnostics <- function(dataset=NULL, section=c("summary","variables","patterns","model","events","chain_mean","chain_variance","convergence","distributions"),
                                     variable=NULL,max_patterns=20L,sort_missing=TRUE,native=TRUE,imputation_start=1L) {
  section <- match.arg(section); dataset <- .rls_dataset_record(dataset)
  info <- .rls_mi_dataset_info(dataset, include_missingness=section %in% c("summary","variables","patterns"))
  if (is.null(variable)) variable <- names(info$original_data)[which.max(colSums(is.na(info$original_data)))]
  if (!variable %in% names(info$original_data)) stop("Unknown diagnostic variable.",call.=FALSE)
  notes <- character(); plot_data <- NULL; plot_view <- NULL
  frame <- switch(section,
    summary=info$missingness_summary,
    variables={d<-info$missingness_by_variable;if(sort_missing)d<-d[order(-d$`Missing %`),,drop=FALSE];d},
    patterns={d<-info$missingness_patterns;if(!is.numeric(max_patterns)||length(max_patterns)!=1L||is.na(max_patterns)||max_patterns<1)stop("max_patterns must be positive.",call.=FALSE);
      if(nrow(d)>max_patterns)notes<-sprintf("Showing %d of %d patterns. Choose Show all patterns to see the rest.",max_patterns,nrow(d));head(d,min(nrow(d),max_patterns))},
    model={pm<-info$predictor_matrix;d<-data.frame(Variable=rownames(pm),Method=as.character(info$methods[rownames(pm)]),
      Predictors=vapply(seq_len(nrow(pm)),function(i)paste(colnames(pm)[is.finite(pm[i,]) & pm[i,]!=0],collapse=", "),character(1L)),check.names=FALSE);
      notes<-c(paste("Imputations:",info$m),paste("Iterations:",info$iterations),paste("Visit sequence:",paste(info$visit_sequence,collapse=", ")),paste("Seed:",if(length(info$seed)&&!is.na(info$seed))info$seed else "Not available"),
        if(length(info$mids$formulas)) paste("Formulas:",paste(vapply(info$mids$formulas,function(f)paste(deparse(f),collapse=" "),character(1L)),collapse="; ")),
        if(any(nzchar(info$mids$post %||% "")))paste("Post-processing:",paste(info$mids$post[nzchar(info$mids$post)],collapse="; ")));d},
    events=as.data.frame(info$logged_events %||% data.frame()),
    convergence={notes<-c(
      "Classical Gelman-Rubin PSRF from coda (not rank-normalized split R-hat). Values near 1 support mixing but do not prove convergence or valid imputations.",
      "Lag-1 autocorrelation is averaged over chains. Persistent dependence and differences between chains should be examined with the trace plots.",
      "Uses all retained iterations without automatic burn-in, for chain means and SDs. Short chains give unstable diagnostics; at least 4 iterations and 2 chains are required.",
      "For categorical variables, chain summaries describe category codes and may hide problems in individual categories.");.rls_mi_convergence_data(info)},
    chain_mean=,chain_variance={plot_data<-.rls_mi_chain_data(info,variable,if(section=="chain_mean")"mean" else "variance");
      if(!nrow(plot_data))notes<-"Convergence information unavailable: this mids object does not contain finite chain statistics for this variable.";plot_data},
    distributions={distribution<-.rls_mi_distribution_data(info,variable,imputation_start);plot_view<-distribution$view;notes<-"Observed values are those originally present; each imputed series uses only originally missing cells. Proportions are computed separately within each series.";
      if(distribution$kind=="continuous")plot_data<-distribution$data;distribution$data})
  if (length(info$logged_events) && NROW(info$logged_events)>0L && section!="events") notes<-c(sprintf("Imputation logged %d event(s). Review Logged events.",NROW(info$logged_events)),notes)
  title<-paste("Imputation diagnostics \u2014",switch(section,summary="Missingness summary",variables="Missingness by variable",patterns="Missing-data patterns",model="Imputation model",events="Logged events",convergence="Convergence summary",chain_mean=paste(variable,"\u2014 Chain means"),chain_variance=paste(variable,"\u2014 Chain variances"),distributions=paste(variable,"\u2014 Observed vs imputed")))
  table <- .rls_mi_diagnostic_table(dataset,frame,title,section,info,variable,notes,plot_view)
  if (isTRUE(native)) {
    .rls_start_backend()
    if (!is.null(plot_data) && nrow(plot_data)) {
      .rls_mi_show_diagnostic_plot(table, plot_data, section)
    } else {
      .rls_table1_sync_native(table)
    }
  }
  invisible(list(info=info,table=table,plot_data=plot_data))
}
# One normalizer for quantities already computed by mice::pool/pool.scalar.
# No model family reimplements Rubin's rules here or in native code.
.rls_mi_status_note <- function(record) {
  mi <- record$multiple_imputation
  if(!is.list(mi) || (mi$m %||% 0L)<2L)return("")
  paste0("Multiple imputation: ",mi$m," imputations combined using Rubin's rules.",
    if(nrow(.rls_mi_missing_information(record)))
      " Missing-information diagnostics are available in the Multiple imputation options." else "")
}

.rls_mi_pooling_diagnostics <- function(pooled, m, label="Estimate") {
  if (is.data.frame(pooled)) {
    if (!"fraction_missing_information" %in% names(pooled)) return(data.frame())
    if ("pooled_z" %in% names(pooled)) {
      pooled <- pooled[pooled$status == "valid" &
        match(pooled$x_variable, unique(pooled$x_variable)) <
        match(pooled$y_variable, unique(pooled$y_variable)), , drop=FALSE]
      pooled$term <- paste(pooled$x_variable, pooled$y_variable, "(Fisher z)", sep=" \u2014 ")
      pooled$estimate <- pooled$pooled_z; pooled$std_error <- pooled$pooled_SE
      pooled$df <- pooled$pooled_df; pooled$p_value <- pooled$p
      pooled$ci_lower <- atanh(pooled$pooled_CI_low)
      pooled$ci_upper <- atanh(pooled$pooled_CI_high)
    }
    n <- nrow(pooled)
    frame_get <- function(key, fallback=NA_real_)
      pooled[[key]] %||% rep(fallback,n)
    out <- data.frame(Variable=frame_get("term",label), Estimate=frame_get("estimate"),
      SE=frame_get("std_error"), df=frame_get("df"), p=frame_get("p_value"),
      `CI low`=frame_get("ci_lower"), `CI high`=frame_get("ci_upper"),
      FMI=frame_get("fraction_missing_information"),
      RIV=frame_get("relative_increase_variance"),
      Ubar=frame_get("within_imputation_variance"),
      B=frame_get("between_imputation_variance"),
      T=frame_get("total_variance"), check.names=FALSE)
  } else if(is.list(pooled) && !is.null(pooled$FMI)) {
    m <- pooled$m %||% m
    pool_get <- function(key) pooled[[key]] %||% NA_real_
    out <- data.frame(Variable=label,Estimate=pool_get("Qbar"),
      SE=pool_get("SE"),df=pool_get("df"),p=pool_get("p"),
      `CI low`=pool_get("CI_low"),`CI high`=pool_get("CI_high"),
      FMI=pool_get("FMI"),RIV=pool_get("RIV"),Ubar=pool_get("Ubar"),
      B=pool_get("B"),T=pool_get("T"),check.names=FALSE)
  } else return(data.frame())
  out$m <- rep_len(m, nrow(out))
  out$lambda <- ifelse(is.finite(out$T) & out$T>0,(1+1/m)*out$B/out$T,NA_real_)
  out$`Relative efficiency` <- 1/(1+out$FMI/m)
  # Simulation error of the pooled point estimate, assuming independent,
  # converged imputations. This is not MCSE of its SE or p-value.
  valid <- is.finite(out$B) & out$B >= 0 & is.finite(out$m) & out$m >= 2
  out$`MCSE (estimate)` <- NA_real_
  out$`MCSE (estimate)`[valid] <- sqrt(out$B[valid]/out$m[valid])
  out$`MCSE / SE (%)` <- ifelse(is.finite(out$SE) & out$SE>0,
    100*out$`MCSE (estimate)`/out$SE, NA_real_)
  out
}

# emmeans retains the Rubin between/total covariance of its mira reference
# grid. Project these matrices with the same linear functions as the estimates.
.rls_mi_emmeans_diagnostics <- function(grid) {
  args<-grid@dfargs; m<-args$m %||% 0L
  if(m<2L || is.null(args$B) || is.null(args$T))return(data.frame())
  L<-grid@linfct
  if(ncol(L)!=ncol(args$B))return(data.frame())
  tab<-as.data.frame(summary(grid,type="link",infer=c(TRUE,TRUE),adjust="none"))
  B<-rowSums((L %*% args$B)*L); T<-rowSums((L %*% args$T)*L)
  Ubar<-T-(1+1/m)*B
  riv<-ifelse(Ubar>0,(1+1/m)*B/Ubar,NA_real_)
  df<-tab$df %||% rep(Inf,nrow(tab))
  fmi<-(riv+2/(df+3))/(riv+1)
  estimate<-as.numeric(L %*% grid@bhat)
  labels<-apply(grid@grid[,!startsWith(names(grid@grid),"."),drop=FALSE],1,paste,collapse="; ")
  pooled<-data.frame(term=paste0(labels," (pooling scale)"),estimate=estimate,
    std_error=sqrt(T),df=df,p_value=tab$p.value %||% NA_real_,
    fraction_missing_information=fmi,relative_increase_variance=riv,
    within_imputation_variance=Ubar,between_imputation_variance=B,total_variance=T)
  .rls_mi_pooling_diagnostics(pooled,m)
}

.rls_mi_regrid <- function(grid, ...) {
  result<-emmeans::regrid(grid,...)
  args<-grid@dfargs
  if((args$m %||% 0L)<2L || is.null(args$B) || ncol(grid@linfct)!=ncol(args$B))return(result)
  total<-as.matrix(stats::vcov(grid))
  transformed<-as.matrix(stats::vcov(result))
  # Supported response transformations are monotonic. Check that the public
  # regrid covariance is exactly this delta-method transformation before
  # retaining a between-imputation decomposition on the response scale.
  multiplier<-sqrt(diag(transformed)/diag(total))
  jacobian<-outer(multiplier,multiplier)
  if(any(!is.finite(jacobian)) || !isTRUE(all.equal(transformed,total*jacobian,tolerance=1e-7,check.attributes=FALSE)))return(result)
  result@dfargs$m<-args$m
  result@dfargs$B<-(grid@linfct %*% args$B %*% t(grid@linfct))*jacobian
  result@dfargs$T<-transformed
  result
}

.rls_mi_missing_information <- function(record) {
  retained<-attr(record,"missing_information",exact=TRUE)
  if(is.data.frame(retained))return(retained)
  derived<-intersect(c("estimates","comparisons","interaction_contrasts"),names(record))
  if(length(derived)) {
    rows<-lapply(derived,function(component) {
      value<-attr(record[[component]],"missing_information",exact=TRUE)
      if(is.data.frame(value) && nrow(value))value$Variable<-paste(component,value$Variable,sep=" \u2014 ")
      value
    })
    rows<-Filter(function(x)is.data.frame(x)&&nrow(x)>0L,rows)
    if(length(rows))return(do.call(rbind,rows))
  }
  if(is.list(record$models) && !is.data.frame(record$models) && length(record$models)) {
    rows<-lapply(seq_along(record$models),function(i) {
      value<-.rls_mi_missing_information(record$models[[i]])
      if(nrow(value))value$Variable<-paste0("Model ",i," \u2014 ",value$Variable)
      value
    })
    rows<-Filter(function(x)nrow(x)>0L,rows)
    if(length(rows))return(do.call(rbind,rows))
  }
  mi <- record$multiple_imputation
  if(!is.list(mi))return(data.frame())
  m <- mi$m %||% 0L; if(m<2L)return(data.frame())
  if (identical(mi$analysis_type,"table1")) return(.rls_mi_table1_diagnostics(record)$diagnostics)
  if (identical(record$analysis_type,"one_way_anova") && isTRUE(mi$pooled_result$ok)) {
    pool <- mi$pooled_result
    return(data.frame(Variable=record$specification$response,
      Group=record$specification$group, Test=pool$method,
      F=pool$F, df1=pool$df1, df2=pool$df2,
      p=record$test_results$p_value, RIV=pool$RIV, m=pool$m,
      check.names=FALSE, stringsAsFactors=FALSE))
  }
  if ((record$analysis_type %||% "") %in% c("independent_samples_t_test", "paired_samples_t_test") && length(mi$group_mean_pools))
    return(.rls_mi_comparison_diagnostics(record)$diagnostics)
  label <- record$response %||% record$specification$response %||%
    record$specification$difference %||% "Estimate"
  out <- .rls_mi_pooling_diagnostics(mi$pooled_result,m,label)
  if(nrow(out))return(out)
  if(is.list(mi$pooled_result) && !is.data.frame(mi$pooled_result)) {
    components <- lapply(names(mi$pooled_result), function(component) {
      value <- .rls_mi_pooling_diagnostics(mi$pooled_result[[component]],m)
      if(nrow(value))value$Variable <- paste(component,value$Variable,sep=" \u2014 ")
      value
    })
    components <- Filter(function(value)nrow(value)>0L,components)
    if(length(components))return(do.call(rbind,components))
  }
  data.frame()
}

.rls_mi_comparison_diagnostics <- function(record) {
  mi <- record$multiple_imputation
  paired <- identical(record$analysis_type, "paired_samples_t_test")
  groups <- if (paired) c(record$specification$response1, record$specification$response2) else
    record$specification$group_levels
  variable <- if (paired) paste(groups, collapse=" \u2212 ") else record$specification$response
  values <- list()
  display <- list(data.frame(value_row=-1L,row_type="term_parent",label=variable,variable=variable))
  add <- function(value, label, type) {
    if (!nrow(value)) return(invisible(NULL))
    value$Variable <- paste(variable,label,sep=" \u2014 ")
    display[[length(display)+1L]] <<- data.frame(value_row=length(values),row_type=type,
      label=label,variable=variable)
    values[[length(values)+1L]] <<- value
  }
  for (group in names(mi$group_mean_pools)) {
    value <- .rls_mi_pooling_diagnostics(mi$group_mean_pools[[group]],mi$m)
    value$p <- NA_real_
    add(value,group,"summary_mean")
  }
  contrast <- .rls_mi_pooling_diagnostics(mi$pooled_result,mi$m)
  contrast$p <- record$test_results$p_value
  contrast$`CI low` <- record$test_results$conf_int[1L]
  contrast$`CI high` <- record$test_results$conf_int[2L]
  add(contrast,paste0("Difference (",groups[1L]," \u2212 ",groups[2L],")"),"group_comparison")
  list(diagnostics=do.call(rbind,values),display_rows=do.call(rbind,display))
}

# Keep descriptive means separate from the group contrast tested in Table 1.
# A mean's test against zero is not a test of differences between groups.
.rls_mi_table1_diagnostics <- function(record) {
  mi <- record$multiple_imputation
  values <- display <- list()
  add <- function(value, label, variable, type) {
    index <- -1L
    if (!is.null(value)) {
      value$Variable <- paste(variable, label, sep=" \u2014 ")
      index <- length(values)
      values[[index+1L]] <<- value
    }
    display[[length(display)+1L]] <<- data.frame(value_row=index,
      row_type=type, label=label, variable=variable, stringsAsFactors=FALSE)
  }
  for (variable in names(mi$estimates_by_imputation)) {
    entries <- mi$estimates_by_imputation[[variable]]
    add(NULL, variable, variable, "term_parent")
    prototype <- NULL
    for (column in names(entries)) {
      entry <- entries[[column]]
      if (!is.list(entry$pool)) next
      value <- .rls_mi_pooling_diagnostics(entry$pool, mi$m)
      if (!nrow(value)) next
      value$p <- NA_real_
      prototype <- value
      add(value, column, variable, "summary_mean")
    }
    test <- mi$pooled_result[[variable]]
    if (is.null(test) || is.null(prototype)) next
    contrast <- .rls_mi_pooling_diagnostics(test$pooled_result, mi$m)
    if (nrow(contrast) && !is.null(test$pooled_result$Qbar)) {
      # Reuse the exact pooled contrast behind the parent table's p-value.
      contrast$p <- test$p
      groups <- names(entries)[-1L]
      label <- if (length(groups)==2L)
        paste0("Difference (", groups[2L], " \u2212 ", groups[1L], ")") else "Group comparison"
    } else {
      # An omnibus test has no single estimate or scalar FMI decomposition.
      contrast <- prototype
      contrast[-1L] <- NA_real_
      contrast$p <- test$p
      contrast$df <- test$pooled_result$df2 %||% NA_real_
      contrast$m <- mi$m
      label <- "Group comparison (omnibus)"
    }
    add(contrast, label, variable, "group_comparison")
  }
  list(diagnostics=if(length(values))do.call(rbind,values) else data.frame(),
       display_rows=if(length(display))do.call(rbind,display) else NULL)
}

#' Missing-information diagnostics for a pooled analytical result
#' @param result A model handle or a LinkEDA result record.
#' @return A data frame with pooled estimates, FMI, RIV and Rubin variances.
#' @export
ls_missing_information_diagnostics <- function(result) {
  retained<-attr(result,"missing_information",exact=TRUE)
  if(is.data.frame(retained))return(retained)
  if(is.list(result) && is.null(result$multiple_imputation) && !is.null(result$id)) {
    id<-result$id
    for(env in list(.rls_state$glm_models,.rls_state$generalized_glm_models,.rls_state$table1_tables,.rls_state$correlation_matrices,.rls_state$regression_comparisons))
      if(exists(id,envir=env,inherits=FALSE)){result<-get(id,envir=env);break}
  }
  diagnostics<-.rls_mi_missing_information(result)
  if(!nrow(diagnostics) && is.list(result$analysis_provenance))
    diagnostics<-result$analysis_provenance$missing_information %||% diagnostics
  diagnostics
}


.rls_mi_show_diagnostic_plot <- function(record, data, section) {
  distribution <- section == "distributions"
  x <- if(distribution) "Value" else "Iteration"
  y <- if(distribution) "Proportion" else "Value"
  series <- if(distribution) "Series" else "Imputation"
  id <- paste0(record$id, ":plot")
  labels <- unique(data[[series]]); rows <- seq_len(nrow(data))
  lines <- c("ADD_PLOT", id, paste0(id,":unlinked"),x,y,record$title,
    as.character(nrow(data)),sprintf("%.17g %.17g %d %d",data[[x]],data[[y]],rows,match(data[[series]],labels)-1L),
    "TIME_SERIES","numeric",series,as.character(length(labels)),labels,
    "TIME_SERIES_OPTIONS","legend","top_right",
    "IMPUTATION_PROCESS_DIAGNOSTIC",section)
  if (!is.null(record$diagnostic_plot_view)) {
    view <- record$diagnostic_plot_view
    lines <- c(lines,"IMPUTATION_PLOT_VIEW",view$variable,as.character(view$total),
      as.character(view$first),as.character(view$last),as.character(view$max_steps),
      if(view$simplified) "1" else "0")
  }
  .rls_send(lines)
  .rls_send(c("SET_DIAGNOSTIC_PLOT_PROVENANCE",id,.rls_analysis_provenance_payload(record)))
}
