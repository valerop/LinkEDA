# LinkEDA

`LinkEDA` is a native, interactive statistical application distributed in the
`LinkEDA` R package. It provides statistical
graphics inspired by XLISP-STAT / Lisp-Stat.

The current prototype focuses on fluid interaction rather than publication
graphics:

- native macOS scatterplot windows;
- rectangular mouse brushing;
- highlighted selected points;
- linked brushing for plots that share a `group`;
- R functions to query and clear the current selection.

It deliberately does **not** use Shiny, htmlwidgets, plotly, ggplot2, JavaScript,
or a browser.

## Architecture

R is the statistical frontend. The graphics backend is a separate native macOS
process compiled from `src/native/backend_macos.mm`. R starts that process on demand and
communicates with it through a local TCP socket.

This keeps the R session responsive and avoids embedding an AppKit event loop
inside R. The command protocol is intentionally small, so the macOS Cocoa backend
can later be replaced or complemented by an SDL2/GLFW backend for Linux.

## macOS requirements

No Homebrew graphics dependency is needed for the first prototype. You need:

- R;
- Apple command-line tools / `clang++`;
- macOS Cocoa/AppKit frameworks, included with macOS.

Install Apple command-line tools if needed:

```sh
xcode-select --install
```

## Install

From the directory that contains this package:

```sh
R CMD INSTALL rlispstat
```

Or from inside the package directory:

```sh
R CMD INSTALL .
```

## Example

```r
library(LinkEDA)

# Open LinkEDA, optionally with an R data frame.
LinkEDA(mtcars)

p1 <- ls_scatter(mtcars, x = "wt", y = "mpg", group = "cars")
p2 <- ls_scatter(mtcars, x = "hp", y = "mpg", group = "cars")

# Drag a rectangle in either plot.
ls_selected("cars")

ls_plots()
ls_plot_info(p1)
ls_set_selected("cars", c(1, 3, 5))
ls_invert_selection("cars")
ls_selection_mode(p1, "toggle")
ls_panel(TRUE)

ls_clear_selection("cars")
ls_close_all()
```

## Interactive UI

Each native scatterplot supports a right-click / two-finger-click context menu.
Use it to change mouse mode, clear/invert/select all rows, reset the view, copy
selected row indices, export the view, show the tool panel, or close the plot.

The shared floating tool panel controls the active plot:

```r
ls_panel(TRUE)
```

Click inside a plot to make it active. The plot status line and window title show
the current mode and selection operation. The context menu's `Export` submenu
copies PDF/SVG/PNG on macOS and saves SVG/PDF/PNG. SVG is genuine vector output;
the Windows backend uses a native Enhanced Metafile for its default visual copy.

Keyboard shortcuts:

- `s`: select mode
- `b`: brush mode
- `i`: identify mode placeholder
- `z`: zoom rectangle mode
- `p`: pan mode placeholder
- `x`: open X-variable menu
- `y`: open Y-variable menu
- `r`: reset/rescale
- `c`: clear selection
- `a`: select all visible
- `v`: invert selection
- `g`: open the native GLM workbench
- `+` / `-`: change brush size
- `Esc`: pointer/none mode
- `Cmd+S` / `Ctrl+S`: open the default SVG save panel

Modifier selection behavior:

- `Shift`: add to selection
- `Option/Alt`: subtract from selection
- `Command` or `Control`: toggle selected points

R can query and set the same UI state:

```r
ls_mode(p1)
ls_set_mode(p1, "brush")
ls_selection_operation(p1)
ls_set_selection_operation(p1, "add")
ls_numeric_variables(p1)
ls_set_xvar(p1, "hp")
ls_set_yvar(p1, "qsec")
ls_lm_line(p1, "all")
ls_lm_line(p1, "selected")
ls_overlays(p1)
ls_glm(p1)
ls_variables_window("cars")
m <- ls_model("cars")
ls_model_set_y(m, "mpg")
ls_model_add_term(m, "wt")
ls_model_add_term(m, "hp")
ls_model_open_residuals_fitted(m)
ls_model_scope(m, "unselected")
ls_model_fit(m)
ls_palette(TRUE)
ls_set_selected_color("cars", "purple")
ls_set_point_color(p1, rows = c(1, 3, 5), color = "green")
```

The plot context menu also includes DataDesk-style object actions:

- `Variables > Open variables window`;
- `Model > General Linear Model`;
- `Model > Use Y as dependent variable`;
- `Model > Add X as predictor`.

## Pairwise comparisons in the General Linear Model

For a fitted simple categorical main effect, right-click its row in the
General Linear Model terms table and choose `Comparaciones por pares…`.
All calculations are delegated to R. For a model containing only the selected
factor, the report uses `rstatix::games_howell_test()` (Games–Howell). For a
model with covariates or other terms, R fits a heteroscedastic
`nlme::gls(..., weights = varIdent(...))` model and obtains adjusted marginal
means and Tukey-adjusted contrasts with `emmeans`. The report includes the
contrast, estimate, standard error, degrees of freedom, t statistic, adjusted
p-value, and confidence interval. The command is disabled for continuous
variables, interactions, transformed terms, and factors with fewer than two
fitted levels.

Manual macOS check: fit a model with a factor, right-click its row, confirm the
menu item and report; then confirm the item is unavailable or disabled for a
numeric covariate and an interaction.

## Binary regression

`Analyze > Regression > Binary Regression…` fits a binary model in the main R
session with `stats::glm(..., family = binomial(link = ...))`. The response may
use any two observed numeric or factor values. The window shows and lets the
user invert the event/reference coding without modifying the source column.
Only logit and probit links are offered. Odds ratios and their Wald intervals
are reported for logit; probit remains on the latent coefficient scale and does
not display odds ratios.

The R API is `ls_new_binary_regression()`. It exposes fit statistics, LR term
tests, pseudo-R-squared measures, AUC, calibration, per-case diagnostics and
`emmeans` pairwise comparisons. `ls_compare_binary_regression_models()` compares
stored models without refitting them; likelihood-ratio tests are produced by R
only for models with identical response coding, rows and link, and nested term
sets. Different links receive descriptive AIC/BIC/AUC comparison only.

The native Variables window is a first DataDesk-style variable palette. It shows
variable names, types, and role badges. Double-clicking and right-clicking a
variable can assign it as the dependent variable or add it as a predictor. The
GLM workbench reads that same group-level model object.

Limitations in this phase: identify labels, true panning, label movement, hidden
points, smoother overlays, full OLS for all model terms, full live
ANOVA/coefficient row objects, Cook/leverage diagnostics, case tables, and
polished export dialogs are not implemented yet. Linear regression overlays, a
selected-point color palette, the native variable palette, the group model
window, and residual/fitted linked diagnostic plots are available from R and from
the native UI. PNG/PDF export saves a snapshot to the Desktop.

## Current limitations

- macOS backend only;
- scatterplots only;
- rectangular brushing only;
- no labels, callbacks, formulas, histograms, scatterplot matrices, or theming;
- the backend stores simple in-memory plot/group state and is meant for up to
  about 100,000 points per plot.
