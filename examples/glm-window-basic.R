library(LinkEDA)

ls_register_dataset("cars", mtcars)
ls_set_active_dataset("cars")

m <- ls_glm_window("cars")

# In the graphical window:
# 1. Click dependent variable and choose mpg.
# 2. Click Add independent variable and choose wt.
# 3. Click Add independent variable and choose hp.
# 4. Confirm that coefficients, p values, R2, partial R2 and global p appear.

ls_glm_set_dependent(m, "mpg")
ls_glm_add_predictor(m, "wt")
ls_glm_add_predictor(m, "hp")
m <- ls_glm_fit(m)

ls_glm_coefficients(m)
ls_glm_fit_summary(m)
