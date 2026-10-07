# Manual cross-platform phase-4 journey: Rscript --vanilla tests/scripts/phase4_linked_workflow.R
# The visible application must be installed from the same checkout first.
suppressPackageStartupMessages(library(LinkEDA))

main <- function() {
  group <- paste0("phase4_mtcars_", Sys.getpid())
  output <- file.path(tempdir(), "linkeda_phase4_workflow")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  on.exit(ls_close_all(), add = TRUE)

  LinkEDA(mtcars, name = group)
  plot <- ls_scatter(mtcars, "wt", "mpg", group = group)
  ls_set_selected(group, 1:12)
  stopifnot(identical(ls_selected(group), 1:12))
  ls_save_analysis_scope(group, "first twelve")
  ls_set_selected(group, 21:24)
  ls_use_saved_analysis_scope(group, "first twelve")
  scope <- ls_analysis_scope(group)
  stopifnot(scope$n == 12L, identical(scope$rows, 1:12))

  model <- ls_new_generalized_linear_model(
    group, response = "mpg", terms = "wt", family = "gaussian",
    link = "identity", native = FALSE
  )
  summary <- ls_generalized_linear_model_fit_summary(model)
  stopifnot(summary$n_used == 12L, summary$n_excluded == 20L)

  pdf <- file.path(output, "scatter.pdf")
  table <- file.path(output, "model.md")
  ls_export_plot(plot, pdf, format = "pdf")
  ls_export_generalized_linear_model_table(model, table, format = "md")
  stopifnot(
    file.exists(pdf), file.info(pdf)$size > 1024,
    identical(readChar(pdf, nchars = 5L, useBytes = TRUE), "%PDF-"),
    file.exists(table), file.info(table)$size > 100
  )
  cat("PASS: named scope, 12-row model, scatter PDF, model table\n")
  cat("LinkEDA ", as.character(packageVersion("LinkEDA")), ": ", output, "\n", sep = "")
}

main()
