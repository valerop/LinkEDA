library(LinkEDA)

p1 <- ls_scatter(mtcars, "wt", "mpg", group = "cars")
p2 <- ls_scatter(mtcars, "hp", "mpg", group = "cars")

ls_plots()
ls_groups()

# Drag-select in either native window, then query:
ls_selected("cars")

ls_close_all()
