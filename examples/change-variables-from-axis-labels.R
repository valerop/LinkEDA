library(LinkEDA)

p <- ls_scatter(
  mtcars,
  x = "wt",
  y = "mpg",
  group = "cars",
  labels = rownames(mtcars),
  title = "Click axis labels to change variables"
)

ls_panel(TRUE)

message("Click the X-axis label to choose another numeric X variable.")
message("Click the rotated Y-axis label to choose another numeric Y variable.")
message("You can also use:")
message("  ls_numeric_variables(p)")
message("  ls_set_xvar(p, 'hp')")
message("  ls_set_yvar(p, 'qsec')")
