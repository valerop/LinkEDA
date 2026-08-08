library(LinkEDA)

# data_name <- ls_import_data("/path/to/data.xlsx")
# ls_set_active_dataset(data_name)

ls_register_dataset("cars", mtcars)
ls_set_active_dataset("cars")
ls_new_scatterplot(x = "wt", y = "mpg")
ls_new_boxplot(y = "mpg", x = "cyl")
