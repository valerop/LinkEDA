# Run after building tests/core/mi_process_roundtrip_test.cpp:
# Rscript tests/native/mi_process_roundtrip.R /path/to/bridge /path/to/script-prefix
args <- commandArgs(TRUE)
stopifnot(length(args)==2L)
pkgload::load_all(".",quiet=TRUE,compile=FALSE)
folder <- tempfile("linkeda-mi-process-"); dir.create(folder)
data <- mice::nhanes
lines <- c("DATASET","native_source",nrow(data),ncol(data))
for(name in names(data)) lines <- c(lines,name,"numeric",ifelse(is.na(data[[name]]),"NA",as.character(data[[name]])))
input <- file.path(folder,"input.txt"); writeLines(lines,input)
output <- file.path(folder,"imputed.txt")
script <- new.env(parent=globalenv())
script$commandArgs <- function(...) c(input,output,"native_imputed","bmi,hyp,chl","age,bmi,hyp,chl","","3","8","31")
sys.source(paste0(args[2],"-impute.R"),envir=script)
original_mids <- script$mids
native_return <- file.path(folder,"returned.txt")
stopifnot(system2(args[1],shQuote(c(output,native_return)))==0L)
payload <- LinkEDA:::.rls_read_native_data_payload(native_return)
data <- LinkEDA:::.rls_merge_native_data(payload,data.frame())
record <- LinkEDA:::.rls_rebuild_native_imputation(payload,data)
record <- LinkEDA:::.rls_mi_restore_process(record,payload$imputation_process)
stopifnot(inherits(record$mids_object,"mids"))
for(field in c("data","imp","m","where","method","predictorMatrix","chainMean","chainVar","loggedEvents","iteration","seed"))
  stopifnot(identical(record$mids_object[[field]],original_mids[[field]]))
record$group <- "native_imputed"
LinkEDA:::.rls_set_dataset_record(record)
for(section in c("summary","model","events","chain_mean","chain_variance","convergence","distributions")) {
  result <- ls_imputation_diagnostics("native_imputed",section=section,variable="bmi",native=FALSE)
  stopifnot(identical(result$info$chain_mean,original_mids$chainMean),
    identical(result$info$logged_events,original_mids$loggedEvents))
  recipe <- result$table$analysis_provenance$verification_r_code$table
  check <- new.env(parent=globalenv())
  check$verification_data_path <- result$table$analysis_provenance$prepared_data_path
  grDevices::pdf(file.path(folder,"check.pdf"))
  invisible(capture.output(eval(parse(text=recipe),check)))
  grDevices::dev.off()
}
unlink(folder,recursive=TRUE)
cat("Native imputation -> document -> R -> diagnostics and verification: OK\n")
