# Example datasets

The `statistical/` directory is LinkEDA's public testing library.  Open it from
the Welcome window with **Open a statistical example…**.  The chooser shows a
short purpose for every dataset; `statistical/README.md` contains the complete
analysis and plot coverage matrix, and `statistical/catalog.csv` contains the
same catalogue in machine-readable form.

`mtcars.csv` is the R `datasets::mtcars` data frame with its row names retained
as the `car` column.

`Alien.csv` is a deterministic example generated in R for LinkEDA from the
Alien generalized-linear-model scheme used by the `LM2GLMM` teaching package.
It contains continuous, count, binary, categorical, and identifier variables so
that several LinkEDA plots and models can be explored from the Welcome Window.
The fixed generation seed is `20260802`.
