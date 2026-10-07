library(LinkEDA)

ls_register_dataset("cars", mtcars)
ls_set_active_dataset("cars")

b1 <- ls_new_boxplot(y = "mpg", x = "cyl", title = "MPG points by cylinders",
                     show_box = FALSE, show_whiskers = FALSE)
