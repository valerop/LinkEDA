library(LinkEDA)

p <- ls_scatter(mtcars, "wt", "mpg", group = "cars",
                labels = rownames(mtcars),
                title = "Context menu demo")

message("Right-click or two-finger-click inside the native plot window.")
message("Use the context menu for modes, selection actions, reset, export, and close.")
