library(LinkEDA)

p <- ls_scatter(mtcars, "wt", "mpg", group = "cars")

ls_selection_mode(p, "replace")
ls_set_selected("cars", c(1, 3, 5))
ls_invert_selection("cars")
ls_clear_selection("cars")

ls_select_where("cars", mpg > 25)
ls_selected("cars")

ls_close_all()
