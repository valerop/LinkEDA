# LinkEDA 0.0.113

- Prepared the public `v0.0.113R` snapshot. The suffix identifies the release
  made available to other users; the package and application version remain
  the numeric `0.0.113` on Windows and macOS.
- The double-click Windows launcher now tolerates temporary communication
  pauses while native windows are maximized or resized, instead of treating a
  single 0.2-second timeout as an application shutdown.
- Release builds now record unhandled XAML exceptions in
  `%LOCALAPPDATA%\LinkEDA\logs\LinkEDA.log` to make unexpected native window
  failures diagnosable.

# LinkEDA 0.0.112

- Windows now offers two double-click installers. The standard installer uses
  an existing compatible R installation; the complete installer carries an
  isolated private R runtime. Both install LinkEDA into its own library, create
  a desktop shortcut and launch the native application without Rtools or
  Developer Mode.
- Windows distributions include the corresponding LinkEDA source archive. The
  complete distribution also includes the exact R source archive and retains
  R's licence and copyright notices alongside the private runtime.

# LinkEDA 0.0.111

- LinkEDA software is now licensed under GPL version 3 or later. Original
  published documentation is licensed separately under CC BY-NC-ND 4.0, with
  explicit scope, attribution and third-party exceptions.
- Windows now builds and distributes `LinkEDA.exe` from the `LinkEDA` WinUI
  project. Platform source files, build scripts and current documentation use
  the LinkEDA name while the former R entry point, S3 classes and configuration
  names remain available as compatibility aliases.

# LinkEDA 0.0.110

- Windows installs `later` as a required dependency so the R session processes
  actions from the welcome window and other native windows. A single Windows
  executable now extracts the package and installs LinkEDA after a double click.

# LinkEDA 0.0.109

- Windows can be distributed as a portable R package download. Its bundled
  WinUI application starts directly from the package, and an included R script
  installs the required CRAN binaries for the user's R version without Rtools
  or Developer Mode. Portable WinUI stores interface font and size preferences
  in the user's local application data folder.

# LinkEDA 0.0.108

- Data sheets show scope membership in a narrow S column beside the case
  number. A blue dot marks cases outside the active scope, while the case
  number keeps the red diagonal only for explicit exclusions. Both marks can
  appear on the same row without obscuring the case number.

# LinkEDA 0.0.107

- Data sheets distinguish explicit exclusions with a red rising diagonal from
  cases outside the active analysis scope with a blue falling diagonal. Cases
  affected by both conditions show both lines.

# LinkEDA 0.0.106

- The data-sheet case-number mark follows the effective analysis scope,
  including an applied saved selection. Returning to Included removes marks
  caused only by that selection while preserving explicit exclusions.

# LinkEDA 0.0.105

- Excluded case numbers use the opposite red diagonal in the data sheet on
  Windows and macOS.

# LinkEDA 0.0.104

- Data sheets on Windows and macOS now mark excluded cases with a red diagonal
  line through the case number. Double-clicking that number alternates inclusion
  through the shared dataset command, while keeping the active analysis-scope
  mode. The floating Analysis Scope panel adds a compact Include all button.

# LinkEDA 0.0.103

- Row-linked residual diagnostic plots now expose case exclusion in their
  exploration menus on Windows and macOS. Excluding selected cases updates the
  shared immutable analysis scope and refits the source model in R, so all
  diagnostics and verification exports use the same scoped result. Aggregate
  diagnostics that do not represent individual rows omit the action.

# LinkEDA 0.0.102

- Windows now attaches application-menu lifetime tracking only after WinUI has
  actually created each internal flyout, including when template creation
  finishes after the menu item's Loaded event. Menu rebuilds also remain
  blocked for an additional dispatcher turn after closing, so the selected
  command receives its first click before any state refresh can replace it.

# LinkEDA 0.0.101

- An anchored `Add polynomial term…` command is now a single menu item on
  Windows and macOS. Selecting it opens a degree dialog that accepts only the
  available integer degrees for that variable. The hierarchical interaction
  submenu and its existing term-order choices remain unchanged.

# LinkEDA 0.0.100

- Searchable `Add terms` windows now list base variables instead of flattening
  polynomial expressions and interactions into ambiguous rows. A secondary
  click on any listed variable opens the same `Model terms` hierarchy used by
  fitted-model term rows, with polynomial degrees and interactions anchored to
  that variable. Variables already in the model remain available for this
  contextual operation. Windows and macOS share the same term rules.

# LinkEDA 0.0.99

- Scrollable 270-degree dendrograms now show every horizontal case label when
  `Fit tree to window` is off. Label thinning remains active only when the
  available leaf spacing would cause overlap, including fitted views and
  labels rotated 90 degrees. Windows and macOS use the same corrected geometry.

# LinkEDA 0.0.98

- File import previews on Windows now display the available variables in a
  searchable multiple-selection list before committing the import. Filtering
  preserves selections that are temporarily hidden, selection counts remain
  visible, and Select all/none applies to the complete file. The macOS import
  chooser now provides the same searchable, persistent selection behaviour.

# LinkEDA 0.0.97

- A secondary click on visible `Add variable` and `Add term` controls now opens
  a shared searchable, multiple-selection chooser on Windows and macOS. The
  primary click keeps each analysis's existing quick menu. The chooser uses
  the candidates already supplied by correlation, dimensionality, dendrogram,
  mean comparison, Table 1, and regression model state, and list based
  analyses apply multiple additions as one update.

# LinkEDA 0.0.96

- APA 7 inter-item correlation tables on Windows now use the shared semantic
  publication specification already used on macOS. Numeric column headings
  are centered over their values in both the vector PDF fallback and formatted
  Office output, fixing the visible offset in correlation matrices.

# LinkEDA 0.0.95

- Scale Dimensionality APA 7 PDF export now lets the user keep the complete
  solution in one table or export parallel-analysis results and loadings as
  two independently named PDF files. In the two-file layout, factors or
  components remain horizontal columns in both tables. Windows and macOS use
  the same shared semantic table specifications.

# LinkEDA 0.0.94

- Scale Dimensionality APA 7 export now uses one shared publication table for
  factors or components, combining candidate eigenvalues, parallel reference,
  explained variance for retained dimensions, item loadings, communalities,
  and uniquenesses. Windows
  uses the same semantic table as macOS instead of exporting only scree rows.

# LinkEDA 0.0.93

- Scale Analysis APA 7 item tables omit the routine scoring-direction column.
  Reverse-scored items are identified in the table note when present; the
  results continue to use the scored item values on both platforms.

# LinkEDA 0.0.92

- Scale Analysis APA exports offer a compact three-row table for scale-level
  results or a scale-level note below the item table. The same presentation
  choice is available in the Windows and macOS APA details dialogs.
- macOS APA clipboard and PDF exports now include every Scale Analysis report
  section rather than selecting only the first table.

# LinkEDA 0.0.91

- Windows application-menu commands remain active if a queued state refresh
  rebuilds the menu before WinUI delivers the click. This fixes the first
  selection of `Analyze > Scale Analysis...` being ignored.

# LinkEDA 0.0.90

- The three dimensionality plots are grouped under Plots again in the Windows
  result menu, matching macOS after the Windows submenu response fix.
- APA 7 Scale Analysis exports use separate scale-level and item-level result
  sections on both platforms. Windows no longer repeats Summary and Items on
  every row or places reliability statistics in the subtitle. Its APA PDF now
  compiles the shared publication tables with R, tinytable and LaTeX.

# LinkEDA 0.0.89

- Windows Scale Analysis biplots now update point colors and selection rings in
  place when cases are selected. Changing the selection no longer rebuilds
  every point, axis, loading arrow, and menu in the plot.
- Large selection changes in ordinary Windows scatterplots and PCA biplots
  now replace their point glyphs as one batch instead of repeatedly searching
  the WinUI child collection for each changed case.
- Windows linked selection now preserves the existing view in dendrograms,
  boxplots, time series, trellis point and boxplot panels, and regression
  interaction plots. It updates marks, selected line segments, confidence
  bands, and legends in place. Scatterplot overlays based on selected cases
  refresh in their own layer. macOS no longer sends a redundant second full
  refresh after each coordinated selection change.
- Windows context menus now take focus when opened, so their submenus and
  commands respond on the first click. The shared window menu router respects
  a local control's own menu, and plot legends no longer have competing
  automatic and manual right-click openers. Scale dimensionality choices
  refresh their single selected option when the menu reopens.
- Windows Scale Analysis biplots now select linked data cases by clicking a
  point or brushing a region. Selected cases are highlighted in the biplot,
  and selections made in the data sheet update the biplot. The context menu
  also selects, inverts, or clears displayed cases. The three dimensionality
  plots are direct menu entries on Windows so they open on the first click.
- Windows Scale Analysis scree plots and biplots now resize with their windows
  and use the shared plot title, method, bounds, and geometry also used on
  macOS. They show centered titles, full frames, grid and axis ticks; the scree
  reference is orange and dashed, and biplot loadings are orange. The macOS
  scree legend now shows colored line keys beside its labels.
- Clicking a factor or component axis label in a Scale Analysis biplot now
  opens the available dimensions on Windows and macOS. Selecting one redraws
  the case scores and loading arrows for that axis, while keeping distinct
  dimensions on the two axes.
- Factor-loading, item-rest, and reliability bars now use a marked zero axis;
  negative values extend to the left as in macOS. Item distributions retain a
  zero-to-one proportion scale. Score distributions label their actual score
  values on the horizontal axis. Derived tables reopen at the top of the report.
# LinkEDA 0.0.88

- Windows scatterplot matrices now update point selection styles in place,
  without rebuilding every cell and fitted curve when cases are selected.
  Selection halos are batched by colour, avoiding a new WinUI element for
  every selected point in every cell.
  Selection dimming also covers cases visible only in some matrix cells.
- Windows selection updates now change menu enabled and checked states in
  place, so they do not replace menu controls between a click and its action.
  Data-sheet context menus are built when requested from the current scope.

# LinkEDA 0.0.87

- Clicking a time-series segment now selects its whole series on Windows and
  macOS, while clicking a plotted marker still selects its individual case.
  Series geometry and hit testing now follow included cases after exclusions.

# LinkEDA 0.0.86

- Scatterplots now recalculate their axis limits from included cases when
  cases are excluded or included again. Existing fitted curves disappear
  immediately while R refits them on the new visible points. This also keeps
  multiple-imputation uncertainty ranges aligned with the included cases.

# LinkEDA 0.0.85

- Scatterplot fit and smooth curves now use the cases actually displayed by
  each plot after exclusions or a scoped selection. This applies to overall,
  selected, colour-group, and multiple-imputation curves on Windows and macOS.
  Linear and generalized model diagnostic plots also refit their added curves
  when the underlying R model fit changes.

# LinkEDA 0.0.84

- Fixed a Windows startup stack overflow when opening a data sheet: activation
  now queues the active-dataset update after the current command completes.
- Excluding selected cases from a plot or data sheet now removes them from the
  included cases shared by all live plots and analyses on Windows and macOS.
  The data sheet keeps the cases visible but muted, and allows selected or all
  excluded cases to be included again. Current selection remains a live
  analysis scope; a saved selection fixes its members while respecting later
  exclusions. Document saves retain exclusions without treating them as data
  edits in the quit prompt.
- Windows estimated marginal means, partial regression, diagnostic, and
  dimensionality plots now show the source fit's captured analysis scope and
  case count, including after refitting an open plot.
- Explore Plot selection modes on Windows now use the shared modifier-key
  precedence for replace, add, subtract, and toggle. A moving brush continues
  to apply each gesture to its original selection instead of repeatedly
  applying it to intermediate results.
- Added the macOS-style Auto-refit switch to Windows Linear Model results.
  Turning it off keeps the last completed fit while editing the model or
  selection; turning it on refits the current specification in R.
- Kept Windows interaction-plot legend labels above the legend background
  after enabling case-selection clicks, so every category remains readable.
- Closing LinkEDA on Windows now asks to save only when the dataset or its
  variable roles have changed. Opening examples, running analyses, changing
  selections, and viewing plots or tables no longer mark data as unsaved.
- Improved two-sample test tables: readable column headings, one copy of each
  methodological note or warning, and a single p column when only one test is
  adjusted. The group menu now shows the two possible subtraction directions
  instead of asking for a "reference group". Descriptives can now be shown or
  opened in a separate window directly from the first level of the Windows
  context menu. PDF exports present Hedges' g methodology as a single neutral
  note and include only warnings belonging to the exported table. The adjusted
  p heading now fits in the Windows results table. Notes and warnings in the
  Windows results view now follow the add-variable action, including in
  one-way ANOVA. The redundant grouping-variable menu was removed from both
  two-sample tests and one-way ANOVA; the selector remains in the window.
- Enlarged the statistical examples chooser on Windows and centered it in the
  work area of the monitor showing the welcome window. Each example now uses
  one line as on macOS, with its description available in a tooltip.
- The Scale Analysis item picker on Windows now supports Shift-click,
  Ctrl-click, and Ctrl+A selection. Selected items are added together in one
  update, while search and keyboard navigation remain available.
- Aligned every Scale Analysis item row and removed technical fingerprint
  messages and the redundant Analysis information menu from its results. The
  internal score status `value` no longer appears as a note or PDF footnote.

# LinkEDA 0.0.83

- A theme chosen from a plot's context menu now changes only that plot on
  macOS, matching Windows. The menu-bar option is labelled "Global theme" on
  both platforms and applies to all open plots.
- Added a small margin at both sides of bar charts and histograms, keeping
  labels, density curves, linked selection, trellis panels, and SVG snapshots
  aligned with the bars.
- Draw bar-chart values in dark text over unselected bars and white text over
  selected portions, including split-bar percentages.
- Kept the data sheet behind a bar chart when selecting cases for the first
  time. Linked row highlights still update immediately, and the sheet's row
  control synchronizes when the sheet is activated.
- Added a small vertical margin inside native bar-chart and histogram frames.
  Their zero baseline sits just above the lower edge, and the tallest bar
  stays below the upper edge. Shared geometry keeps bars, density overlays,
  rugs, axis marks, trellis panels, and SVG snapshots aligned.

# LinkEDA 0.0.82

- Removed the broken Windows bar-segment context flyout. Right-clicking any
  bar or segment now opens the same general bar-chart menu as the rest of the
  plot, matching the normal macOS behaviour while preserving linked selection
  with the left mouse button.

# LinkEDA 0.0.81

- Placed the intense selected portion at the bottom of each histogram colour
  block, with its pale unselected portion immediately above, matching the
  established linked bar-chart stacking order.

# LinkEDA 0.0.80

- Corrected linked histogram colour geometry: every bin now stacks case counts
  by colour, keeps the pale unselected and intense selected portions of each
  hue adjacent, and retains one thin black outline around the complete bin.
  The same colour-intensity rules are used by native Windows, macOS, trellis,
  and SVG rendering without changing bins, scope, or imputation semantics.

# LinkEDA 0.0.79

- Aligned native bar charts with the visible macOS reference: bars now use
  colour and borders without pattern fills or pattern-related menu items,
  including when opening plots saved with the former pattern mode.

# LinkEDA 0.0.78

- Made all four sides of native Windows plot frames use the same axis colour
  and stroke width, including scatterplots, boxplots, histograms, bar charts,
  time series, trellis plots, and scatterplot matrices.
- Kept interaction-plot legend category names readable at full contrast and
  made legend clicks immediately activate the corresponding fitted series.
- Kept variable descriptions in the variable view instead of substituting
  them into effect-plot titles, axes, and legend headings.
- Simplified every Windows result-table Save menu to one displayed PDF and one
  APA 7 PDF, and made APA 7 copy/save ask for the table number and title just
  as on macOS.
- Compact APA 7 tables pasted into Word, use readable response labels without
  identifier underscores, and omit an all-empty standardized-beta column for
  factors-only linear models.
- Size ordinary Windows data-sheet columns to preserve complete variable names
  and render the status-bar separator without mojibake.
- Keep distinct continuous measurements available in paired-samples selectors;
  only integer sequences or explicitly named identifiers are treated as IDs.
- Present multiple-imputation fit methodology as a compact neutral note in the
  Windows linear-model report while retaining the full method in provenance.
- Recover a missing transient R dataset registration without flashing an error;
  Windows resends the canonical dataset and retries the model once.
- Draw black histogram-bin outlines above linked-selection and manually assigned
  case-colour fills on macOS, Windows, trellis panels, and SVG snapshots.
- Make bar charts use the histogram's neutral selection tones while preserving
  proportional, linked case-colour composition in both native applications.

# LinkEDA 0.0.77

- Prevented the Windows Welcome identity tagline from being clipped at the
  default window size.

# LinkEDA 0.0.76

- When an optional R package is required, LinkEDA now asks in the native
  interface before downloading it from CRAN, installs it in a private
  per-user library, and retries the interrupted generalized model or model
  comparison. Users do not need to open an R console or have administrator
  permissions.
- On Windows, optional packages use CRAN binaries so end users do not need
  Rtools and compiled dependency versions remain compatible.

# LinkEDA 0.0.74

- Expanded Windows/macOS menu parity for plots and statistical workflows,
  including live brushing, linked time-series selection, annotations, and the
  contextual Model Trellis panel actions.
- Made count-model exposure an explicit validated log offset and aligned the
  Windows model controls, comparison output, and R fitting requests.
- Added R-controlled factor-extraction choices (minimum residual, maximum
  likelihood, and principal axis), including reproducible provenance and
  score-saving behavior.
- Improved compact Office table copying and the shared native application
  menus while keeping mixed-model workflows hidden for this release.

# LinkEDA 0.0.73

- Reject empty native plot exports when a window has not yet completed layout;
  PDF and PNG saving then use the R renderer instead of reporting success with
  an unusable file.
- Rechecked the packaged macOS build, native scope and export workflows, and
  a linked selection-to-model workflow before the Windows handoff.

# LinkEDA 0.0.72

- Quick Cluster now offers Euclidean, Manhattan, maximum, and Canberra
  distances in its dendrogram menus on macOS and Windows. The tree and
  verification recipe use the selected metric in R.
- The dendrogram's Analyze menu opens its case-by-case distance matrix on
  demand, using the same scoped cases, standardisation, and distance metric.

# LinkEDA 0.0.71

- Consolidated case-colour actions under one `Explore plot > Colors` submenu:
  colouring selected observations, opening the floating palette, resetting
  colours, overriding colours by a categorical variable, cancelling that
  override, and saving or applying named schemes now appear together.
- Replaced the extra `Group / Condition` menu level with a direct
  `Conditioning variables` menu whose first level contains the available
  conditioning controls.

# LinkEDA 0.0.70

- Restored `Override colors by` and its cancel action to `Group / Condition`;
  cancelling the override reveals the persistent case colours that were active
  before it.
- Manual recolouring while an override is active now asks whether to promote
  the visible override colours to the persistent colour set or restore the
  previous colours before applying the edit.
- Kept named colour-scheme saving and application under `Explore plot > Colors`.

# LinkEDA 0.0.69

- Moved case-colour controls, including saving and applying named colour
  schemes, from `Group / Condition` to `Explore plot > Colors`. Conditioning
  menus now contain only variables that divide or condition the graph.

# LinkEDA 0.0.68

- Removed the redundant `Sticker` heading. Editable notes retain a minimal
  control strip, while snapshots and PDF/SVG exports devote the complete note
  body to its text.

# LinkEDA 0.0.67

- Made the floating Analysis Scope panel substantially more compact. Its height
  now follows the number of available scopes and only grows to a capped,
  scrollable list when named scopes are present.

# LinkEDA 0.0.66

- Sticker pointers now preserve their position inside a graph's plotting area
  across resized windows, snapshots, and PDF/SVG output; table stickers keep
  their placement relative to the complete table.
- Stickers can be collapsed to a small coloured marker and expanded again;
  the collapsed state is also retained in snapshots and exports.

# LinkEDA 0.0.65

- Replaced the floating Analysis Scope drop-down with a compact, immediately
  applied radio-button list showing built-in and named scopes with aligned
  observation counts and a `Save selection…` action.

# LinkEDA 0.0.64

- Separated persistent case colours from a reversible dataset-wide categorical
  colour override shared by linked plots, tables, and dendrograms.
- Manual recolouring now cancels the active override before applying the edit,
  so the saved case-colour layer is never overwritten implicitly.
- Added named colour schemes with dataset-version and analysis-scope provenance;
  applying a scheme to a different scope now requires an explicit choice.
- Added a floating Analysis Scope panel on macOS and Windows that mirrors the
  global scope menu, and made the per-plot freeze command explicit for
  retaining an exploratory view.
- Grouped Trellis-only display controls under a disabled `Trellis` submenu when
  a plot has no conditioning variable.
- Stickers now retain their relative box and pointer positions when windows are
  resized or when snapshots are rendered to PDF, SVG, or another output size.

# LinkEDA 0.0.62

- Statistical examples now preserve catalogue-declared categorical columns
  whose visible values look numeric. In particular, `Chick` and `Diet` in the
  Chick growth example open as Categorical while `Time` remains Numeric.
- Variable View now gives every bundled example variable a definition, the
  intended analysis objective, and a source citation.
- Trellis time-series creation now distinguishes the optional series variable
  from panel conditioning, so high-cardinality identifiers such as `Chick`
  can group trajectories without being hidden by the panel-count limit.

# LinkEDA 0.0.61

- Added a documented library of 17 public statistical examples covering every
  visible analysis and plot family, including offsets and multiple imputation.
- Replaced the two fixed Welcome examples with a compact chooser that opens the
  selected bundled dataset on macOS and Windows.

# LinkEDA 0.0.60

- Fixed the macOS bar-chart X-axis variable label so a left click reliably
  opens the variable chooser, including where category-label hit areas overlap.
- Restored complete Default-theme panel frames across scatterplots, Trellis
  panels, bar charts, histograms, and boxplots, and aligned their macOS,
  Windows, SVG, PDF, and bitmap rendering.
- Standardized fitted straight-line menus as `Regression lines` on macOS and
  Windows.
- Gave boxplot boxes a clear, consistent outline on screen and in vector or
  bitmap output, including Trellis boxplots.

# LinkEDA 0.0.59

- Unified result-window menus, analysis-scope updates, model diagnostics, and
  export hierarchies across the native interfaces.
- Added and audited generalized, count, binary, mixed, multiple-imputation,
  scale, dimensionality, missing-data, and post-estimation workflows.
- Added publication tables, vector and raster exports, and executable Quarto
  verification bundles.
- Improved multiple-imputation progress reporting and parallel execution.
- Improved linked selection performance and synchronized diagnostic windows.
- Corrected scatterplot legend layout and top/bottom placement on screen and
  in copied or saved output.
- Updated package metadata, dependency declarations, documentation, source
  portability, and distribution tests.
