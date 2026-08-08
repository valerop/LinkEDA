library(LinkEDA)

p <- ls_scatter(
  mtcars,
  x = "wt",
  y = "mpg",
  group = "cars",
  labels = rownames(mtcars),
  title = "Regression overlays"
)

ls_panel(TRUE)

ls_lm_line(p, data = "all")
ls_set_selected("cars", c(1, 3, 5, 7, 9, 11))
ls_lm_line(p, data = "selected")

ls_overlays(p)

message("Right-click the plot and use Overlays -> Linear regression for menu-driven overlays.")
