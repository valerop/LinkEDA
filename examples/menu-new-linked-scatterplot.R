library(LinkEDA)

ls_register_dataset("cars", mtcars)
ls_set_active_dataset("cars")

p <- ls_new_scatterplot(x = "wt", y = "mpg", title = "mpg vs weight")

# In the native UI, use:
# Plot > New Linked Scatterplot
#
# Choose another X/Y pair. The new plot uses the same group ("cars"), so row
# selections are linked with this plot.
