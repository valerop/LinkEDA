library(LinkEDA)

p1 <- ls_scatter(mtcars, "wt", "mpg", group = "cars",
                 labels = rownames(mtcars),
                 title = "Weight vs MPG")

p2 <- ls_scatter(mtcars, "hp", "mpg", group = "cars",
                 labels = rownames(mtcars),
                 title = "Horsepower vs MPG")

ls_panel(TRUE)

message("Use the floating panel to change modes and selection operations.")
message("Try Select, Brush, Pan and Zoom. Then run ls_selected('cars').")
