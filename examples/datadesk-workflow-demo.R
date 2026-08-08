library(LinkEDA)

cars <- mtcars
cars$car <- rownames(mtcars)

p1 <- ls_scatter(cars, "wt", "mpg", group = "cars",
                 labels = cars$car,
                 title = "Weight vs MPG")

p2 <- ls_scatter(cars, "hp", "mpg", group = "cars",
                 labels = cars$car,
                 title = "Horsepower vs MPG")

ls_variables_window("cars")

m <- ls_model("cars")
ls_model_set_y(m, "mpg")
ls_model_add_term(m, "wt")
ls_model_add_term(m, "hp")
ls_model_fit(m)
ls_model_open_residuals_fitted(m)

message("Try this in the UI:")
message("1. Right-click variables in the Variables window.")
message("2. Make mpg dependent and add wt/hp as predictors.")
message("3. Right-click model terms to remove terms or open diagnostics.")
message("4. Select points in any linked plot, including residual diagnostics.")
message("5. Switch model scope to selected or unselected rows.")
