# Process orchestration only. Statistical fitting remains in the existing R engines.
.rls_mi_worker_count <- function(imputations, workers = NULL, parallel = NULL) {
  total <- as.integer(imputations)
  if (length(total)!=1L || is.na(total) || total<1L) stop("At least one imputation is required.")
  if (is.null(parallel)) parallel <- getOption("LinkEDA.mi_parallel",TRUE)
  if (!isTRUE(parallel)) return(1L)
  if (is.null(workers)) {
    workers <- getOption("LinkEDA.mi_workers",NA_integer_)
    value <- suppressWarnings(as.integer(Sys.getenv("LINKEDA_MI_WORKERS","")))
    if(length(value)==1L && is.finite(value) && value>0L) workers <- value
  }
  if(length(workers)!=1L || !is.finite(workers) || workers<1L) {
    cores <- parallel::detectCores(logical=TRUE)
    if(length(cores)!=1L || !is.finite(cores)) cores <- 1L
    workers <- min(4L,total,max(1L,cores-1L))
  }
  min(total,as.integer(workers))
}

.rls_mi_fit_task <- function(task) {
  record <- task$specification
  complete <- task$complete
  data <- complete$data
  if(nrow(data)<=length(record$terms)+1L) stop("insufficient complete observations",call.=FALSE)
  if(isTRUE(record$count_regression)) {
    if(.rls_count_distribution_uses_trials(record$count_distribution))
      .rls_count_validate_bounded_response(record,data)
    else {
      .rls_count_validate_response(data,record$response)
      .rls_count_validate_exposure(data,record$exposure %||% "")
    }
  }
  record$family_object <- .rls_glm_make_family(record$family,record$link)
  fit_started <- proc.time()[["elapsed"]]
  fit <- .rls_glm_fit_engine(record,data)
  if(isTRUE(record$binary_regression) && identical(record$link,"log"))
    .rls_binary_validate_log_fit(fit,record$link,character(),
      sprintf("The log-binomial model for imputation %d",task$index))
  fit_seconds <- unname(proc.time()[["elapsed"]] - fit_started)
  diagnostic_started <- proc.time()[["elapsed"]]
  diagnostic_error <- distribution_error <- ""
  # The ceiling-hurdle extractor already computes its component diagnostics.
  # Avoid doing that expensive work a second time inside the fit worker.
  diagnostics <- if (.rls_count_is_ceiling_hurdle(record)) data.frame() else
    tryCatch(
      .rls_generalized_glm_diagnostics(record, fit, complete,
        imputation_index = task$index),
      error = function(error) {
        diagnostic_error <<- conditionMessage(error)
        data.frame()
      }
    )
  distribution <- if (is.data.frame(diagnostics) && nrow(diagnostics)) {
    tryCatch(
      .rls_discrete_distribution_summary(
        record, fit, complete, diagnostics
      )$observed_predicted_distribution,
      error = function(error) {
        distribution_error <<- conditionMessage(error)
        data.frame()
      }
    )
  } else data.frame()
  structure(list(
    fit = fit, diagnostics = diagnostics, distribution = distribution,
    diagnostic_error = diagnostic_error,
    distribution_error = distribution_error,
    fit_seconds = fit_seconds,
    diagnostic_seconds = unname(proc.time()[["elapsed"]] - diagnostic_started)
  ), class = "linkeda_mi_fit_payload")
}

.rls_mi_fit_specification <- function(record) {
  # Explicit allowlist: never serialize the model registry, prior fits or MI datasets.
  fields <- c("response","terms","family","link","binary_regression","count_regression",
    "count_distribution","trials_variable","trials_constant","exposure","offset_variable","model_type",
    "response_bounds","response_transformation","event","reference","group","id",
    "model_version","diagnostic_data_version","diagnostic_seed","scope")
  record[intersect(fields,names(record))]
}

.rls_mi_run_fit_task <- function(task, fit_function, fit_options) {
  old <- options(fit_options); on.exit(options(old),add=TRUE)
  if(!is.null(task$rng_stream)) assign(".Random.seed",task$rng_stream,.GlobalEnv)
  started <- proc.time()[["elapsed"]]; warnings <- technical_warnings <- character()
  value <- tryCatch(withCallingHandlers(fit_function(task),warning=function(w) {
    message <- conditionMessage(w)
    if (grepl("package version mismatch:", message, fixed = TRUE) &&
        grepl("glmmTMB was built with TMB package version", message, fixed = TRUE))
      technical_warnings <<- unique(c(technical_warnings, message))
    else warnings <<- unique(c(warnings, message))
    invokeRestart("muffleWarning")
  }),error=identity)
  error <- if(inherits(value,"error")) conditionMessage(value) else ""
  if(nzchar(error)) value <- NULL
  payload <- if(inherits(value,"linkeda_mi_fit_payload")) value else NULL
  fit <- if(is.null(payload)) value else payload$fit
  convergence <- if(is.null(fit)) FALSE else if(inherits(fit,"glmmTMB"))
    isTRUE(fit$sdr$pdHess) && identical(as.integer(fit$fit$convergence),0L)
    else if(inherits(fit,"lm") && !inherits(fit,"glm")) TRUE
    else if(is.list(fit) && !is.null(fit$converged)) isTRUE(fit$converged) else NA
  if(!is.null(fit) && identical(convergence,FALSE))
    warnings <- unique(c(warnings,"The optimizer did not report convergence."))
  list(index=task$index,fit=fit,error=error,warnings=warnings,
    technical_warnings=technical_warnings,
    seconds=unname(proc.time()[["elapsed"]]-started),convergence=convergence,
    fit_seconds=payload$fit_seconds %||% unname(proc.time()[["elapsed"]]-started),
    diagnostic_seconds=payload$diagnostic_seconds %||% 0,
    diagnostics=payload$diagnostics %||% NULL,
    distribution=payload$distribution %||% NULL,
    diagnostic_error=payload$diagnostic_error %||% "",
    distribution_error=payload$distribution_error %||% "")
}

.rls_mi_condition_summary <- function(messages, indices = seq_along(messages)) {
  messages <- as.character(messages)
  indices <- as.integer(indices)
  present <- nzchar(messages)
  if (!any(present)) return(character())
  messages <- messages[present]
  indices <- indices[present]
  groups <- split(indices, factor(messages, levels = unique(messages)))
  labels <- names(groups)
  vapply(seq_along(groups), function(i) {
    affected <- groups[[i]]
    suffix <- if (length(affected) == 1L) {
      sprintf("imputation %d", affected)
    } else {
      shown <- head(affected, 8L)
      sprintf("%d imputations (%s%s)", length(affected),
        paste(shown, collapse = ", "),
        if (length(affected) > length(shown)) ", \u2026" else "")
    }
    sprintf("%s [%s]", labels[[i]], suffix)
  }, character(1L), USE.NAMES = FALSE)
}

.rls_mi_rng_streams <- function(n,seed) {
  old_kind <- RNGkind(); existed <- exists(".Random.seed",.GlobalEnv,inherits=FALSE)
  old <- if(existed) get(".Random.seed",.GlobalEnv) else NULL
  on.exit({do.call(RNGkind,as.list(old_kind)); if(existed) assign(".Random.seed",old,.GlobalEnv)
    else if(exists(".Random.seed",.GlobalEnv,inherits=FALSE)) rm(".Random.seed",envir=.GlobalEnv)},add=TRUE)
  RNGkind("L'Ecuyer-CMRG"); set.seed(seed)
  out <- vector("list",n); out[[1]] <- .Random.seed
  if(n>1L) for(i in 2:n) out[[i]] <- parallel::nextRNGStream(out[[i-1L]])
  out
}

.rls_mi_terminate_workers <- function(cluster, pids=integer(), force=TRUE) {
  if(is.null(cluster)) return(invisible(NULL))
  # Only PIDs returned by our own local PSOCK nodes are eligible for termination.
  if(force) for(pid in pids) try(tools::pskill(pid,tools::SIGTERM),silent=TRUE)
  try(parallel::stopCluster(cluster),silent=TRUE)
  invisible(NULL)
}

.rls_mi_psock_port <- function(candidate, attempt) {
  # Avoid the fixed R_PARALLEL_PORT across repeated LinkEDA tasks.  This uses
  # process/time entropy without consuming R's statistical RNG stream.
  stamp <- as.numeric(Sys.time()) * 1000
  12000L + as.integer((stamp + Sys.getpid() * 104729 +
    as.integer(candidate) * 1009 + as.integer(attempt) * 7919) %% 28000)
}

.rls_mi_fit_imputations_once <- function(tasks,fit_function=.rls_mi_fit_task,workers=NULL,
    parallel=NULL,progress=NULL,cancel_path="",heartbeat_seconds=25,
    cluster_factory=parallel::makePSOCKcluster) {
  total <- length(tasks); workers <- .rls_mi_worker_count(total,workers,parallel)
  ids <- vapply(tasks,`[[`,integer(1),"index")
  if(!identical(sort(ids),seq_len(total))) stop("Imputation IDs must be unique and cover 1..m.")
  started <- proc.time()[["elapsed"]]; startup_seconds <- 0; collection_seconds <- 0
  emit <- function(...) if(is.function(progress)) try(progress(...),silent=TRUE)
  check_cancel <- function(completed) if(.rls_mi_cancelled(cancel_path))
    stop(.rls_mi_cancel_condition(completed,total,proc.time()[["elapsed"]]-started))
  seed <- getOption("LinkEDA.mi_fit_seed",NULL)
  if(!is.null(seed)) {
    streams <- .rls_mi_rng_streams(total,seed)
    tasks <- lapply(tasks,function(task) {task$rng_stream <- streams[[task$index]];task})
  }
  fit_options <- options()[intersect(c("contrasts","na.action","warn"),names(options()))]
  fit_options$LinkEDA.mi_parallel <- FALSE # Prevent accidental nested LinkEDA pools.
  # One worker is the explicit in-process sequential/debug path.  Cancellation
  # is checked between fits; multi-process execution remains cancellable while
  # fits are running through the socket polling loop below.
  inline <- workers == 1L || isTRUE(getOption("LinkEDA.mi_inline",FALSE))
  cluster <- NULL; pids <- integer(); stopped <- FALSE
  on.exit(if(!stopped) .rls_mi_terminate_workers(cluster,pids),add=TRUE)
  startup_started <- proc.time()[["elapsed"]]
  if(!inline) for(attempt in seq_along(unique(c(workers,if(workers>2L)2L)))) {
    candidate <- unique(c(workers,if(workers>2L)2L))[[attempt]]
    check_cancel(0L)
    nodes <- tryCatch(suppressWarnings(withr::with_envvar(c(OMP_NUM_THREADS="1",OPENBLAS_NUM_THREADS="1",
      MKL_NUM_THREADS="1",VECLIB_MAXIMUM_THREADS="1",BLIS_NUM_THREADS="1"),
      cluster_factory(candidate,methods=FALSE,setup_strategy="sequential",
        setup_timeout=10,timeout=86400,
        port=.rls_mi_psock_port(candidate,attempt)))),error=identity)
    failure <- if(inherits(nodes,"error")) nodes else NULL
    node_pids <- if(is.null(failure)) tryCatch(
      unlist(parallel::clusterCall(nodes,Sys.getpid),use.names=FALSE),error=identity) else integer()
    if(inherits(node_pids,"error")) failure <- node_pids
    setup <- if(is.null(failure)) tryCatch(parallel::clusterCall(nodes,function(paths) {
      .libPaths(paths); suppressPackageStartupMessages(loadNamespace("LinkEDA"));
      Sys.setenv(OMP_NUM_THREADS="1",OPENBLAS_NUM_THREADS="1",MKL_NUM_THREADS="1",
        VECLIB_MAXIMUM_THREADS="1",BLIS_NUM_THREADS="1"); NULL
    },.libPaths()),error=identity) else NULL
    if(inherits(setup,"error")) failure <- setup
    if(is.null(failure)) {cluster<-nodes;pids<-node_pids;workers<-candidate;break}
    if(!inherits(nodes,"error")) .rls_mi_terminate_workers(nodes,node_pids)
    emit(completed=0L,total=total,running=0L,workers=candidate,
      elapsed=proc.time()[["elapsed"]]-started,
      message=sprintf("Could not start %d worker(s): %s. Trying fewer workers.",candidate,conditionMessage(failure)))
  }
  startup_seconds <- unname(proc.time()[["elapsed"]]-startup_started)
  results <- vector("list",total)
  if(is.null(cluster)) {
    if(!inline) emit(completed=0L,total=total,running=0L,workers=1L,
      elapsed=proc.time()[["elapsed"]]-started,
      message="Parallel execution unavailable. Falling back to in-process sequential execution; cancellation is checked between fits.")
    for(i in seq_along(tasks)) {
      check_cancel(i-1L)
      result <- .rls_mi_run_fit_task(tasks[[i]],fit_function,fit_options)
      results[[result$index]] <- result
      emit(result=result,completed=i,total=total,running=0L,workers=1L,
        elapsed=proc.time()[["elapsed"]]-started)
    }
    return(list(results=results,workers=1L,startup_seconds=startup_seconds,
      collection_seconds=0,fitting_seconds=unname(proc.time()[["elapsed"]]-started-startup_seconds),worker_pids=integer()))
  }
  assigned <- integer(workers); next_task <- 1L; completed <- 0L
  send_call <- utils::getFromNamespace("sendCall", "parallel")
  receive_result <- utils::getFromNamespace("recvResult", "parallel")
  send <- function(node) {
    check_cancel(completed)
    if(next_task>total) return(FALSE)
    assigned[[node]] <<- tasks[[next_task]]$index
    send_call(cluster[[node]],.rls_mi_run_fit_task,
      list(tasks[[next_task]],fit_function,fit_options),tag=assigned[[node]])
    next_task <<- next_task+1L; TRUE
  }
  for(node in seq_len(workers)) send(node)
  last_event <- proc.time()[["elapsed"]]
  emit(completed=0L,total=total,running=sum(assigned>0L),workers=workers,
    elapsed=last_event-started,message="Fitting models\u2026")
  repeat {
    check_cancel(completed); active <- which(assigned>0L)
    if(!length(active)) break
    ready <- socketSelect(lapply(cluster[active],`[[`,"con"),timeout=.25)
    if(!any(ready)) {
      now <- proc.time()[["elapsed"]]
      if(now-last_event>=heartbeat_seconds) {
        emit(completed=completed,total=total,running=length(active),workers=workers,
          elapsed=now-started,message=sprintf("Still fitting; no new fit completed in the last %.0f s. Elapsed %s.",heartbeat_seconds,.rls_mi_elapsed_text(now-started)))
        last_event<-now
      }
      next
    }
    for(position in which(ready)) {
      node <- active[[position]]; received_at <- proc.time()[["elapsed"]]
      result <- tryCatch(receive_result(cluster[[node]]),error=identity)
      collection_seconds <- collection_seconds+proc.time()[["elapsed"]]-received_at
      if(inherits(result,"error") || !is.list(result) || !identical(result$index,assigned[[node]]))
        stop(structure(list(message=sprintf(
          "Worker failed while fitting imputation %d: %s",
          assigned[[node]], if(inherits(result,"error")) conditionMessage(result)
            else "invalid or mismatched worker response"),
          call=NULL,imputation=assigned[[node]],completed=completed,total=total,
          workers=workers,results=results),
          class=c("linkeda_mi_worker_failed","error","condition")))
      results[[result$index]] <- result; assigned[[node]]<-0L; completed<-completed+1L
      emit(result=result,completed=completed,total=total,running=sum(assigned>0L),workers=workers,
        elapsed=proc.time()[["elapsed"]]-started)
      send(node);last_event<-proc.time()[["elapsed"]]
    }
  }
  fitting_seconds <- unname(proc.time()[["elapsed"]]-started-startup_seconds-collection_seconds)
  .rls_mi_terminate_workers(cluster,pids,force=FALSE);stopped<-TRUE
  list(results=results,workers=workers,startup_seconds=startup_seconds,
    collection_seconds=unname(collection_seconds),fitting_seconds=fitting_seconds,worker_pids=pids)
}

.rls_mi_fit_imputations <- function(tasks,fit_function=.rls_mi_fit_task,workers=NULL,
    parallel=NULL,progress=NULL,cancel_path="",heartbeat_seconds=25,
    cluster_factory=parallel::makePSOCKcluster) {
  candidate <- .rls_mi_worker_count(length(tasks),workers,parallel)
  repeat {
    outcome <- tryCatch(.rls_mi_fit_imputations_once(tasks,fit_function,
      workers=candidate,parallel=parallel,progress=progress,
      cancel_path=cancel_path,heartbeat_seconds=heartbeat_seconds,
      cluster_factory=cluster_factory),linkeda_mi_worker_failed=identity)
    if(!inherits(outcome,"linkeda_mi_worker_failed")) return(outcome)
    failed_workers <- outcome$workers %||% candidate
    if(failed_workers<=1L) stop(outcome)
    candidate <- if(failed_workers>2L) 2L else 1L
    if(is.function(progress)) try(progress(completed=0L,total=length(tasks),
      running=0L,workers=candidate,elapsed=0,
      message=sprintf("%s Restarting all imputations with %d worker(s); no partial results will be pooled.",
        conditionMessage(outcome),candidate)),silent=TRUE)
  }
}
