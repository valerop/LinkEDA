library(LinkEDA)

ls_register_dataset("cars", mtcars)
ls_set_active_dataset("cars")

ls_datasets()
ls_active_dataset()

p1 <- ls_new_scatterplot(x = "wt", y = "mpg")
p2 <- ls_new_scatterplot(x = "hp", y = "mpg")

ls_selected("cars")
