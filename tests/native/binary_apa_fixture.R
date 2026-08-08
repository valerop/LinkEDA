args <- commandArgs(TRUE)
if (length(args) != 2L) stop("usage: binary_apa_fixture.R <logit|probit> <output>", call. = FALSE)
link <- match.arg(args[[1L]], c("logit", "probit"))
output <- args[[2L]]

data <- transform(mtcars, cyl = factor(cyl))
model <- rlispstat::ls_new_binary_regression(
  data, "vs", c("mpg", "cyl"), link = link, event = "0", reference = "1",
  native = FALSE, name = paste0("binary_apa_", link), term_types = list(cyl = "factor")
)
state <- rlispstat::ls_binary_regression_state(model)
fit <- state$fit_statistics
clean <- function(x) gsub("[\t\r\n]", " ", as.character(x))
value <- function(x) {
  if (length(x) != 1L || is.na(x) || is.nan(x)) return("NA")
  if (is.infinite(x)) return(if (x > 0) "Inf" else "-Inf")
  sprintf("%.17g", x)
}
text <- function(x) if (length(x) != 1L || is.na(x)) "" else clean(x)

connection <- file(output, open = "wb")
on.exit(close(connection), add = TRUE)
emit <- function(...) writeLines(paste(..., sep = "\t"), connection, useBytes = TRUE)
emit("META", state$response, state$link, state$event, state$reference,
     fit$n_used, fit$event_count, fit$reference_count, fit$df_model,
     value(fit$global_lr), value(fit$global_p), value(fit$nagelkerke_r2),
     value(fit$log_lik), value(fit$residual_deviance), value(fit$aic),
     value(fit$bic), value(fit$auc))
for (index in seq_len(nrow(state$coefficient_rows))) {
  row <- state$coefficient_rows[index, , drop = FALSE]
  emit("ROW", text(row$row_type), text(row$term), text(row$display_label),
       text(row$source_term), text(row$term_type), text(row$level),
       text(row$reference_level), value(row$estimate), value(row$std_error),
       value(row$statistic), value(row$p_value), value(row$ci_lower),
       value(row$ci_upper), value(row$odds_ratio), value(row$odds_ratio_lower),
       value(row$odds_ratio_upper))
}
for (index in seq_len(nrow(state$term_tests))) {
  row <- state$term_tests[index, , drop = FALSE]
  emit("TEST", text(row$term), row$df, value(row$statistic), value(row$p_value))
}
for (warning in state$warnings) emit("WARN", clean(warning))
