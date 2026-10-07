# LinkEDA 0.0.113: Windows release handoff to macOS

This document is the continuation checkpoint for producing the macOS build that
matches public tag `v0.0.113R`. Read `AGENTS.md` completely before changing
files.

## Release identity

- Development repository: `https://github.com/valerop/LinkEDA-dev.git`
- Public repository: `https://github.com/valerop/LinkEDA.git`
- Public tag: `v0.0.113R`
- Shared functional version: `0.0.113`
- Validated Windows native build: `1.0.0.155`

The final `R` belongs only to the public Git tag and release title. Keep
`Version: 0.0.113` in `DESCRIPTION` and display `0.0.113` in the
application on both platforms.

Resolve the exact release commit from the publication reference rather than
copying a mutable branch name:

```bash
git ls-remote --tags https://github.com/valerop/LinkEDA.git v0.0.113R
```

## Keep the macOS checkout separate

Do not merge the Windows and macOS repositories or create a nested
`rlispstat/` package directory. Preserve every local macOS change first.
Create a read-only reference clone of the public release:

```bash
git clone --branch v0.0.113R --single-branch \
  https://github.com/valerop/LinkEDA.git ../LinkEDA-0.0.113R-reference
git -C ../LinkEDA-0.0.113R-reference describe --tags --exact-match
```

The final command must print `v0.0.113R`.

From the macOS checkout, locate the package root containing `DESCRIPTION`,
`R/`, `src/core/` and `tests/`. Transfer files relative to that root and
never copy the reference repository as a directory into it.

## Areas to reconcile

Review and transfer the release path by path:

- package metadata: `DESCRIPTION`, `NAMESPACE`, `NEWS.md`, licences,
  `.Rbuildignore`, `configure` and `cleanup`;
- R implementation and documentation: `R/`, `man/` and `inst/`;
- portable native logic: `src/core/`;
- native macOS adapter: `src/native/backend_macos.mm` and
  `src/platform/macos/linkeda_macos_app.{h,mm}`;
- tests: `tests/core/`, `tests/testthat/`, `tests/native/` and
  `tests/scripts/`;
- public documentation where it helps the macOS package.

Do not copy `src/platform/windows/`, Windows installers, private R runtimes,
`build/`, `bin/`, `obj/`, `Generated Files/`, installed applications or
Git metadata.

## Behaviour added since the previous 0.0.77 handoff

### Inclusion, selection and analysis scope

- Explicitly excluded cases are stored separately from the live selection.
  Exclusion removes a case from active selections and analyses without changing
  the selected/saved-scope mode currently shown.
- A saved selection captures the cases present when it is created and can still
  identify cases excluded later. Effective analyses apply later exclusions.
- Data sheets expose include/exclude actions. An excluded case number has a red
  rising diagonal; a case outside the active scope has a blue scope mark, and
  both marks can appear together.
- Double-clicking a case number alternates explicit inclusion. The Analysis
  Scope panel includes a compact Include all action and remains usable with
  many saved selections.
- Every computed result retains the exact data version and immutable
  `AnalysisScope`.

### Linked plots and models

- Scatterplots, histograms, bars, time series, trellis views, boxplots,
  dendrograms, dimensionality plots and model diagnostics respect effective
  inclusion and selection scopes.
- Selection updates preserve view state and update marks in place where
  possible. Large scatter matrices and biplots avoid rebuilding complete
  geometry for selection-only changes.
- Linear and generalized model windows support shared model specifications,
  auto-refit, diagnostics, interactions, effect plots and comparison outputs.
- Response types, distributions and links are restricted by the shared model
  catalogue. Statistical calculations remain in R.

### Data, variables and multiple imputation

- Variable roles distinguish Numeric, Categorical, Ordinal and Text from their
  physical R storage. Logical values are binary categorical variables.
- Searchable multiple-selection choosers are used for imports and analysis
  variable/term additions.
- Type changes use the shared dataset mutation/versioning path and apply to
  every representation of a multiple-imputation dataset.
- Missing-data, imputation diagnostics, pooling information and publication
  outputs retain their analysis scope and provenance.

### Reproducibility and publication

- `Export > R Code > Show R Code…` produces a coordinated Quarto bundle with
  prepared data and verification code generated from the immutable analysis
  specification.
- Publication tables use shared semantic specifications; PDF tables use real
  LaTeX and `tinytable`.
- The software licence is GPL version 3 or later. Original published
  documentation is CC BY-NC-ND 4.0, with code and third-party exceptions stated
  separately.

### Naming and release packaging

- User-facing application, package and preferred native source names are
  LinkEDA. Former `rlispstat` entry points and selected internal identifiers
  remain only where compatibility requires them.
- Windows has standard and complete double-click installers. Their launcher
  loop and private R packaging are Windows-only and must not be copied into the
  macOS application.
- The public release tag is `v0.0.113R`; the shared version remains numeric.

## Native macOS work

Treat the current macOS implementation as the visual and behavioural reference
where it already has an established native control. Reconcile shared changes
rather than replacing the AppKit source wholesale. Platform-specific UI must
call the shared commands and model structures.

Increase the independent macOS native build number monotonically. Do not alter
the shared functional version merely because macOS is rebuilt. If macOS
validation reveals a functional correction, prepare the next shared version
instead of silently changing the published 0.0.113 behaviour.

## Required validation on macOS

From the resolved package root:

```bash
Rscript -e 'devtools::test()'
R CMD build .
R CMD check --no-manual LinkEDA_0.0.113.tar.gz
R CMD INSTALL LinkEDA_0.0.113.tar.gz
Rscript --vanilla tests/scripts/phase4_linked_workflow.R
Rscript -e 'cat(as.character(packageVersion("LinkEDA")), "\n")'
git diff --check
```

The version command must print `0.0.113`. Build and launch the native macOS
application from the same checkout and confirm it loads that installed package.

Manually verify at least:

1. Welcome, examples, import, recent files, save and reopen.
2. Data-sheet selection, explicit exclusion/inclusion, saved selections and
   every Analysis Scope mode.
3. Scatterplot, histogram, bar, boxplot, time series, trellis, matrix,
   parallel-coordinate and dendrogram linking.
4. Linear and generalized models, comparisons, diagnostics, effect and
   interaction plots, auto-refit and response-specific eligibility.
5. Table 1, scale analysis, factor/components workflows, missing data and
   multiple imputation.
6. SVG/PDF/PNG export, Office copy, semantic APA tables, R verification Quarto
   bundles, notes and snapshot albums.
7. Window resizing, maximization, scrolling and selection responsiveness with a
   large dataset.

For each discrepancy, identify whether it is in shared R/C++, the AppKit
adapter, a stale test or the invocation. Keep all statistical calculations in R
and add a regression test for each correction.

## Completion record

Before publishing the macOS binary, record:

- exact source commit and tag;
- shared version and macOS native build number;
- R and macOS versions and architecture;
- package test/check results;
- native workflows exercised;
- remaining platform differences;
- binary filename and SHA-256 checksum.

Commit and push only with the user's explicit authorization.
