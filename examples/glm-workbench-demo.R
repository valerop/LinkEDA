library(LinkEDA)

p <- ls_scatter(
  mtcars,
  x = "wt",
  y = "mpg",
  group = "cars",
  title = "GLM workbench demo"
)

ls_set_selected("cars", c(1, 3, 5, 7, 9))
ls_glm(p)

message("Use the GLM window's Scope menu to switch between all data and selected rows.")
message("Right-click the scatterplot and choose Model > General Linear Model to reopen it.")
