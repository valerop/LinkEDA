library(LinkEDA)

p1 <- ls_scatter(mtcars, "wt", "mpg", group = "cars",
                 title = "Selected point palette")
p2 <- ls_scatter(mtcars, "hp", "mpg", group = "cars",
                 title = "Linked colors")

ls_panel(TRUE)
ls_palette(TRUE)

ls_set_selected("cars", c(1, 3, 5, 7))
ls_set_selected_color("cars", "purple")

message("Select points and click a color swatch in the palette.")
