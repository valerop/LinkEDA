# LinkEDA

`LinkEDA` is a native, interactive statistical application distributed in the
`LinkEDA` R package. It provides statistical
graphics inspired by XLISP-STAT / Lisp-Stat.

## Multiple imputation: import and diagnostics

Import the original `mice::mids` object for MI analyses. Completed lists (`mild`)
are rejected with instructions to save the original object using
`saveRDS(imp, "my_imputation.rds")`. Stacked `.imp`/`.id` tables open as ordinary
data with an explanatory notice; LinkEDA does not reconstruct the imputation
process from completed tables.
Imputations created in LinkEDA retain their original `mids` through native
synchronization and new `.linkeda` saves, including methods, predictor matrix,
logged events and chains. Older documents without this metadata still open,
but cannot reconstruct the original chain diagnostics from completed values.

The imputation dialog keeps its method and predictor controls. Its default
uses the selected predictors across the imputation models, which may be
unsuitable for a particular study, hinder convergence or consume unnecessary
resources. Use `mice` in R for study-specific predictor matrices, interactions,
blocks and constraints, and import the resulting original `mids`.

**Data > Missing Data > Imputation diagnostics** opens the original missingness
summary. Its context menu provides missingness by variable, frequent or all
patterns, the imputation specification, logged events, available chain means
and variances, and observed versus imputed distributions. Each imputation has
its own series; these checks cannot establish that imputations are correct.
**Convergence summary** adds lag-1 autocorrelation and classical Gelman-Rubin
PSRF for the retained chain means and SDs, using `coda` in R. It uses all
iterations without automatic burn-in and reports unavailable or constant
chains explicitly. This is not rank-normalized split R-hat; inspect traces
alongside the numbers, particularly with few iterations.

Pooled result menus offer **Multiple imputation > Show missing-information
diagnostics** where the pooling decomposition is retained. The expanded table
reports FMI, RIV, within/between/total variance, lambda and approximate relative
efficiency on the pooling scale. FMI measures uncertainty associated with
missing information, not the percentage of missing cells. No good/bad cutoffs
are used. Ordinary result columns stay unchanged.
The expanded table also includes `m`, **MCSE (estimate)** (`sqrt(B / m)`) and
**MCSE / SE (%)**. These assess simulation error of the pooled point estimate
assuming independent, converged imputations, not stability of its standard
error or p-value. More imputations generally reduce MCSE, but small `m` makes
the diagnostic itself uncertain. Verification exports recompute it from the
same scoped data with public R functions.

`ls_imputation_diagnostics(..., native = FALSE)` returns dataset diagnostics;
`ls_missing_information_diagnostics(result)` returns analytical diagnostics.
Calculations remain in R. Native views provide R verification through
**Export > R Code**; imputation-process recipes export a frozen copy of the
original `mids` and its original data, including retained process metadata.

LinkEDA combines fluid native interaction with statistical analysis and
publication export:

- native macOS scatterplot windows and a WinUI 3 Windows scatterplot surface;
- rectangular mouse brushing;
- highlighted selected points;
- linked brushing for plots that share a `group`;
- R functions to query and clear the current selection;
- linear, generalized, count, binary, mixed, scale, dimensionality and
  missing-data analyses;
- linked diagnostics and immutable analysis scopes;
- vector/raster export, publication tables, and executable R verification
  recipes.

It deliberately does **not** use Shiny, htmlwidgets, plotly, ggplot2, JavaScript,
or a browser.

## Architecture

R is the statistical frontend. The graphics backend is a separate native
process: the macOS application is compiled from `src/native/backend_macos.mm`,
and the Windows application is a WinUI 3/C++/WinRT target under
`src/platform/windows/winui`. R communicates with the visible Windows target
through a local TCP socket.

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

The public Windows release is **v0.0.113R**, containing LinkEDA **0.0.113**.
Download either the complete installer, which includes a private copy of R, or
the smaller standard installer for computers with R 4.1 or newer from the
[LinkEDA releases page](https://github.com/valerop/LinkEDA/releases/tag/v0.0.113R).
The `R` suffix marks the snapshot published for other users and is not part of
the numeric R package version.

From the directory that contains this package:

```sh
R CMD INSTALL .
```

LinkEDA requires R 4.1 or newer. Core package dependencies are installed by R.
Some analysis and export features use suggested packages, including
`emmeans`, `glmmTMB`, `lme4`, `lmerTest`, `mitml`, `gamlss`, `tinytable`, and
`svglite`; LinkEDA reports a clear message when an optional dependency needed
for a requested feature is unavailable.

On Windows, the downloadable release provides two double-click installers.
The standard installer uses an existing R 4.1 or newer and installs LinkEDA in
that R version's personal library. The complete installer includes an isolated
private copy of R and its own package library. Both create a `LinkEDA` desktop
shortcut. See `docs/windows-download.md` for packaging and licence details.

For a distribution check from the repository root:

```sh
R CMD build .
R CMD check --no-manual LinkEDA_*.tar.gz
```

The build compiles the native macOS backend through `configure`. The Windows
TCP backend uses Rtools; the optional WinUI application is built separately
from `src/platform/windows/winui/LinkEDA` with Visual Studio and the
Windows App SDK.

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
Click a mark or drag a rectangle to select observations. Use the context menu
to clear/invert/select all rows, copy selected row indices, or export the view.

The shared floating tool panel controls the active plot:

```r
ls_panel(TRUE)
```

Click inside a plot to make it active. The plot status line and window title show
the selection operation. The context menu's `Export` submenu
copies PDF/SVG/PNG on macOS and saves SVG/PDF/PNG. SVG is genuine vector output;
the Windows backend uses a native Enhanced Metafile for its default visual copy.

Keyboard shortcuts:

- `x`: open X-variable menu
- `y`: open Y-variable menu
- `c`: clear selection
- `a`: select all visible
- `v`: invert selection
- `g`: open the native GLM workbench
- `Cmd+S` / `Ctrl+S`: open the default SVG save panel

Modifier selection behavior:

- `Shift`: add to selection
- `Option/Alt`: subtract from selection
- `Command` or `Control`: toggle selected points

The Windows scatterplot surface also provides its native right-click menu for
selecting all visible points, inverting the current selection, and clearing it.
Its keyboard equivalents are `A`, `V`, and `C`/`Esc`.

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

Limitations in this phase: identify labels, true panning, label movement and
hidden points remain incomplete. Result windows use a common `Export` hierarchy
with `Copy`, `Save`, and `R Code` sections. Tables support formatted and
tab-delimited copy plus CSV, Markdown, SVG, PNG and PDF output; statistical
plots support vector and raster copy/save formats. Verification export creates
a self-contained Quarto document and its prepared RDS data in one folder.

## Current limitations

- the Windows WinUI surface renders linked scatterplots, bar charts,
  histograms, boxplots, trellis plots, time-series plots, scatterplot matrices,
  parallel coordinates and linked data sheets. It also provides the native
  menu, variable palette, analysis workflows and result windows for the
  currently supported analyses;
- Windows and macOS share the command/state layer, but new or platform-specific
  commands still need parity verification and visual polish on both native
  surfaces;
- click, rectangular brushing, keyboard/context-menu selection and data-sheet
  row selection are implemented on Windows;
- identify labels, true panning and label movement remain incomplete;
- the backend stores simple in-memory plot/group state and is meant for up to
  about 100,000 points per plot.

## Licences

LinkEDA software is free software licensed under the GNU General Public License,
version 3 or, at your option, any later version. See [LICENSE](LICENSE).

The original LinkEDA documentation is licensed separately under the Creative
Commons Attribution-NonCommercial-NoDerivatives 4.0 International licence. Its
scope, required attribution and third-party exceptions are described in
[LICENSE-DOCUMENTATION.md](LICENSE-DOCUMENTATION.md). Source code and executable
examples embedded in the documentation remain under the software licence.
