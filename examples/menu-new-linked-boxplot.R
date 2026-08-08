library(LinkEDA)

ls_register_dataset("cars", mtcars)
ls_set_active_dataset("cars")

ls_new_scatterplot(x = "wt", y = "mpg", title = "Weight vs MPG")

# In the native UI:
# Plot > New Linked Boxplot...
