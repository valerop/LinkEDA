library(LinkEDA)

ls_register_dataset("cars", mtcars)
ls_set_active_dataset("cars")

p1 <- ls_new_scatterplot(x = "wt", y = "mpg", title = "Weight vs MPG")
p2 <- ls_new_scatterplot(x = "hp", y = "mpg", title = "Horsepower vs MPG")
b1 <- ls_new_boxplot(y = "mpg", x = "cyl", title = "MPG by Cylinders")

message("Select cases in any plot; linked plots share the same row ids.")
