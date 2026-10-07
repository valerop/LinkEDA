options(warn = 1)

suppressPackageStartupMessages(library(LinkEDA))
LinkEDA::LinkEDA()

# The native WinUI process asks this R process to perform every statistical
# calculation. Keep R's event loop alive until the LinkEDA window closes.
# Window maximization can briefly keep the UI thread busy, so a single missed
# probe is not evidence that the application has closed.
disconnected_since <- NULL
repeat {
  tryCatch(
    later::run_now(timeoutSecs = 0.2),
    error = function(error) warning(
      "LinkEDA could not process a queued action: ",
      conditionMessage(error),
      call. = FALSE
    )
  )
  connected <- tryCatch(
    isTRUE(LinkEDA:::.rls_backend_connection_alive()),
    error = function(error) FALSE
  )
  if (connected) {
    disconnected_since <- NULL
  } else {
    if (is.null(disconnected_since)) disconnected_since <- Sys.time()
    if (as.numeric(difftime(
      Sys.time(), disconnected_since, units = "secs"
    )) >= 15) break
  }
  Sys.sleep(0.05)
}
