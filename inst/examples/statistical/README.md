# LinkEDA statistical examples

Choose **Open a statistical example…** in LinkEDA's Welcome window.  The list
opens a prepared copy of the selected public dataset; it does not depend on the
dataset's physical location or on an internet connection.

## Coverage

| Example | Principal LinkEDA checks |
|---|---|
| Motor Trend cars | Table 1, correlation, one-sample tests, linear and binary models, model comparison, scatterplot, matrix, parallel coordinates, boxplot, histogram, bar chart |
| Anscombe's quartet | Linear models and diagnostics, grouped scatterplots, Trellis scatterplots |
| New York air quality | Missing-data overview and imputation, time series, scatterplot, histogram |
| Chick growth | Time series, grouped scatterplots, Trellis plots, linear models |
| Iris flowers | Correlation, PCA/factor analysis, quick cluster, scatterplot matrix, parallel coordinates |
| Paired sleep data | Paired-samples tests, histogram, scatterplot |
| Guinea-pig tooth growth | Two-sample tests, one-way ANOVA, factorial models, interaction interpretation, boxplot, bar chart, Trellis plots |
| US arrests | Correlation, PCA/factor analysis, quick cluster, scatterplot matrix, parallel coordinates |
| Warp breaks | Poisson and negative-binomial models, factorial interactions, boxplot, bar chart, Trellis plots |
| Titanic passengers | Contingency tables, binary models, interactions, bar charts, Table 1 |
| School absence | Overdispersed count models, interactions, boxplot, histogram |
| Car insurance claims | Poisson models using `log_holders` as an offset, model comparison, categorical predictors |
| Gasoline yield | Beta proportion models, linear models, diagnostics, scatterplot |
| Theophylline concentration | Positive-continuous models, time series, Trellis plots, grouped scatterplots |
| Big Five personality items | Scale analysis, correlation, PCA/factor analysis, missing data, Table 1 |
| NHANES with missing values | Missing-data overview, imputation, descriptives |
| NHANES multiple imputation | Pooled analyses, MI diagnostics, plots, and tables using an original `mice::mids` object |

Every visible plot family is covered: scatterplot, Trellis scatterplot, time
series, scatterplot matrix, parallel coordinates, boxplot/violin, histogram,
and bar chart.  Every visible Analyze entry is also covered.  Several datasets
intentionally overlap because they expose different failure modes, such as
Anscombe's influential patterns, missing values in air quality and NHANES, or
overdispersion in school absence.

## Sources and transformations

The files are local snapshots of datasets distributed by R or established CRAN
packages.  Their online documentation is recorded in `catalog.csv`.  The build
script is `tools/build_statistical_examples.R`.

- R `datasets`: [official dataset index](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/00Index.html).
- `MASS`: [school absence (`quine`)](https://search.r-project.org/CRAN/refmans/MASS/html/quine.html) and [car insurance claims](https://search.r-project.org/CRAN/refmans/MASS/html/Insurance.html).
- `betareg`: [GasolineYield](https://search.r-project.org/CRAN/refmans/betareg/html/GasolineYield.html).
- `psych`: [bfi](https://search.r-project.org/CRAN/refmans/psych/html/bfi.html).
- `mice`: [NHANES](https://search.r-project.org/CRAN/refmans/mice/html/nhanes.html).

Only transparent preparation is applied: row names become identifier columns;
Anscombe is reshaped to long form; `sleep` is reshaped to paired columns;
Titanic frequency cells are expanded to passenger rows; a date is added to
`airquality`; and the insurance offset is supplied as `log_holders`.  The
NHANES `mids` file uses five imputations, five iterations, and seed `20260925`.
The catalogue also records numeric-looking factor columns that CSV cannot
preserve by itself: `set` in Anscombe, `Chick` and `Diet` in Chick growth,
`batch` in Gasoline yield, and `subject` in Theophylline concentration.
`variable_descriptions.csv` supplies every example variable with a plain-language
definition.  When an example is opened, LinkEDA combines that definition with
the example's analysis objective and source citation and shows the result in
**Variable View > Description**.  This metadata path is used only by the
bundled example chooser; ordinary imported files are unaffected.

The source packages retain their respective licenses.  Base R datasets are
distributed as part of R; MASS and betareg use GPL-2 or GPL-3; psych and mice
use GPL version 2 or later.  Citations for the original studies are available
on the linked documentation pages.
