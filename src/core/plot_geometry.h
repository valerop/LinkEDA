#ifndef RLISPSTAT_CORE_PLOT_GEOMETRY_H
#define RLISPSTAT_CORE_PLOT_GEOMETRY_H

#include <cmath>
#include <cstdint>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>

#include "analysis_scope.h"
#include "provenance_model.h"

namespace rlispstat {
namespace core {

struct DataColumn;
struct DataFrameModel;

struct Point {
    double x = 0.0;
    double y = 0.0;
};

struct Rect {
    double x = 0.0;
    double y = 0.0;
    double width = 0.0;
    double height = 0.0;
};

struct DataViewport {
    double xmin = 0.0;
    double xmax = 1.0;
    double ymin = 0.0;
    double ymax = 1.0;
};

struct DataPoint {
    double x = 0.0;
    double y = 0.0;
    int row = -1;
};

// Values for one row obtained from the independently fitted models in a
// multiple-imputation analysis.  They are presentation data for diagnostic
// plots, never pooled coefficients or a replacement fitted model.
struct ScatterplotPointImputationValues {
    int row = 0;
    std::vector<Point> values;
};

struct NumericVariable {
    std::string name;
    std::vector<double> values;
};

struct VariableMeta {
    std::string name;
    std::string type;
};

struct Overlay {
    int id = 0;
    std::string type;
    std::string source;
    bool visible = true;
};

enum class TrellisPlotType {
    Scatter,
    TimeSeries,
    Boxplot,
    Bar,
    Histogram,
    DataTable
};

enum class TrellisDataTableRowOrder {
    OriginalDatasetOrder,
    SelectedFirst,
    VariableAscending,
    VariableDescending
};

enum class TrellisDataTableVerticalScrollMode {
    Shared,
    Independent
};

struct TrellisDataTableSortSpecification {
    TrellisDataTableRowOrder order = TrellisDataTableRowOrder::OriginalDatasetOrder;
    std::optional<std::string> variableId;
};

struct TrellisDataTableSpecification {
    std::vector<std::string> displayedVariableIds;
    TrellisDataTableSortSpecification sort;
    TrellisDataTableVerticalScrollMode verticalScrollMode =
        TrellisDataTableVerticalScrollMode::Shared;
    bool showOriginalRowIds = true;
    bool showSelectionCounts = true;
    std::unordered_map<std::string, double> columnWidths;
    std::size_t firstVisibleRow = 0;
    std::unordered_map<std::string, std::size_t> firstVisibleRowsByPanel;
    double horizontalOffset = 0.0;
    bool initialized = false;
};

enum class TrellisConditioningVariableKind {
    Categorical,
    ContinuousBinned
};

enum class TrellisContinuousBinningMethod {
    EqualWidth,
    EqualCount,
    CustomBreaks
};

enum class TrellisDimension {
    Rows,
    Columns,
    Nested
};

struct TrellisContinuousBinningSpecification {
    TrellisContinuousBinningMethod method = TrellisContinuousBinningMethod::EqualWidth;
    std::size_t binCount = 4;
    std::vector<double> customBreaks;
    bool includeLowest = true;
};

struct TrellisConditioningVariable {
    std::string variableId;
    std::string variableLabel;
    TrellisConditioningVariableKind kind = TrellisConditioningVariableKind::Categorical;
    TrellisDimension dimension = TrellisDimension::Nested;
    std::optional<TrellisContinuousBinningSpecification> binning;
    // Records whether a numeric column is being interpreted as an ordered
    // factor. Panel levels remain deterministic in either mode, and the
    // dataset's canonical variable type is unchanged.
    bool orderedCategories = false;
};

struct TrellisPlotSpecification {
    TrellisPlotType plotType = TrellisPlotType::Scatter;
    std::string xVariableId;
    // For trellis boxplots, ordered outer-to-inner categorical variables.
    // xVariableId remains the first entry for backward compatibility.
    std::vector<std::string> boxplotGroupingVariableIds;
    std::string yVariableId;
    std::string groupingVariableId;
    std::string splitVariableId;
    std::vector<TrellisConditioningVariable> conditioningVariables;
    std::string scaleMode = "common_xy";
    std::string layoutMode = "automatic";
    std::string panelOrder = "defined";
    std::string barMeasure = "count";
    std::string histogramMeasure = "count";
    std::size_t histogramBinCount = 10;
    bool connectObservations = false;
    bool hasCustomTitle = false;
    std::string customTitle;
    TrellisDataTableSpecification dataTable;
};

struct BoxplotPoint {
    double y = 0.0;
    std::string category;
    int row = -1;
};

struct HistogramPoint {
    double x = 0.0;
    int row = -1;
    int bin = -1;
};

struct HistogramBin {
    double lower = 0.0;
    double upper = 0.0;
    std::vector<int> rows;
};

struct BarplotSegment {
    std::string level;
    std::vector<int> rows;
    int count = 0;
    int barN = 0;
    int totalN = 0;
    double conditionalPercent = 0.0;
    double overallPercent = 0.0;
    double barPercent = 0.0;
    double barWidthValue = 0.0;
};

struct BarplotBin {
    std::string category;
    std::vector<int> rows;
    int n = 0;
    double percent = 0.0;
    double widthValue = 0.0;
    std::vector<BarplotSegment> segments;
};

struct BiplotLoading {
    std::string variable;
    double x = NAN;
    double y = NAN;
};

struct InteractionPlotLine {
    std::string label;
    std::string colorKey;
    std::vector<DataPoint> points;
    // Pointwise confidence limits aligned with `points`. Empty vectors mean
    // that the fitted series has no interval information.
    std::vector<DataPoint> confidenceLower;
    std::vector<DataPoint> confidenceUpper;
    // Source observations represented by this legend entry.  Fitted
    // interaction lines contain prediction-grid points rather than source
    // rows, so row identity must be carried separately from `points`.
    std::vector<int> caseIds;
};

struct TimeSeriesAxisTick {
    double value = 0.0;
    std::string label;
};

struct TimeSeriesLegendItem {
    std::size_t seriesIndex = 0;
    Point sampleStart;
    Point sampleEnd;
    Point labelAnchor;
};

struct TimeSeriesDirectLabel {
    std::size_t seriesIndex = 0;
    std::string label;
    std::string colorKey;
    Point seriesAnchor;
    Point leaderEnd;
    Point labelAnchor;
};

enum class SmoothCurveScope {
    Overall,
    Selection,
    ColorGroup
};

struct SmoothCurveData {
    SmoothCurveScope scope = SmoothCurveScope::Overall;
    std::string groupId;
    std::vector<double> x;
    std::vector<double> y;
    bool ok = false;
    std::string message;
    // Both straight fitted lines and LOESS curves are calculated in R.  The
    // native layer only retains and renders the returned prediction grid.
    // These fields follow the original aggregate members to preserve source
    // compatibility for callers that initialize the legacy six-field shape.
    std::string fitMethod = "loess";
    std::vector<double> confidenceLower;
    std::vector<double> confidenceUpper;
};

struct ROCThresholdLink {
    DataPoint point;
    double threshold = NAN;
    int truePositive = 0;
    int falsePositive = 0;
    int trueNegative = 0;
    int falseNegative = 0;
    std::vector<int> crossingRows;
    std::vector<int> falsePositiveRows;
    std::vector<int> falseNegativeRows;
};

// Shared point representation for model diagnostics.  It lives with the
// plot model (rather than glm_model.h) because PlotModel retains exact
// per-imputation diagnostic clouds for live presentation changes.
struct DiagnosticPlotPoint {
    double x = NAN;
    double y = NAN;
    int row = 0;
};

NumericVariable *FindNumericVariable(std::vector<NumericVariable> &vars, const std::string &name);
VariableMeta *FindVariableMeta(std::vector<VariableMeta> &metas, const std::string &name);
std::vector<std::string> VariableNames(const std::vector<VariableMeta> &metas);
std::vector<std::string> NumericVariableNames(const std::vector<NumericVariable> &vars);
bool VariableExists(const std::vector<VariableMeta> &metas, const std::string &name);

// Freeze and apply an analysis scope to a newly-created plot. This is shared
// by the native frontends so plots use the same row semantics as analyses.
struct PlotModel;
void ApplyAnalysisScopeToPlot(PlotModel &model,
                              const AnalysisScope &scope,
                              std::size_t totalRows);

struct PlotModel {
    std::string kind = "scatter";
    std::string id;
    std::string group;
    std::string xLabel;
    std::string yLabel;
    std::string title;
    AnalysisScope dataScope;
    bool dataScopeCaptured = false;
    // Ordinary plots follow the one dataset-wide analysis scope. Freezing is
    // an explicit exception used to compare an earlier subset with the live
    // views. `dataScopeCaptured` records the rows used for the current
    // rendering; it does not by itself make a plot private or frozen.
    bool analysisScopeFrozen = false;
    std::string frozenScopeNotice;
    OutputCodeReference codeReference;
    // Theme captured for user-visible R verification/publication code.  This
    // is presentation state only and never triggers a statistical refit.
    std::string rExportTheme = "bw";
    // Used by immutable Snapshot Album renderables.  These maps intentionally
    // exclude selection and detach appearance from the live dataset registry.
    bool frozenAppearanceCaptured = false;
    std::map<int, std::string> frozenRowColors;
    std::map<int, std::string> frozenRowLabels;
    bool isDatasetSeed = false;
    bool isGLMDiagnostic = false;
    std::string glmModelId;
    std::string glmDiagnosticKind;
    // Local curve highlighting; these indices are never source-dataset row IDs.
    std::set<std::size_t> selectedDiagnosticSeries;
    std::string imputationDiagnosticVariable;
    int imputationDiagnosticTotal = 0;
    int imputationDiagnosticFirst = 1;
    int imputationDiagnosticLast = 0;
    int imputationDiagnosticMaxSteps = 512;
    bool imputationDiagnosticSimplified = false;
    bool diagnosticShowIdentityLine = false;
    // Post-estimation plots (for example an observed-data boxplot opened from
    // a pairwise-comparisons table) remain live children of the exact fitted
    // regression model that created them.  The source identity is deliberately
    // independent of `glmModelId`: diagnostic plots and derived analyses have
    // different refresh semantics, but both must reject stale revisions.
    bool isRegressionDerivedPlot = false;
    std::string regressionDerivedKind;
    std::string regressionDerivedSourceKind;
    std::string regressionDerivedSourceModelId;
    std::string regressionDerivedTerm;
    int regressionDerivedSourceRevision = 0;
    int regressionDerivedSourceFitVersion = 0;
    // MI diagnostics inspect one concrete fit at a time.  The index is
    // one-based so window titles and menus match R's imputation numbering.
    int diagnosticImputationIndex = 1;
    std::string diagnosticSummary;
    int diagnosticImputationCount = 0;
    bool diagnosticShowImputationUncertainty = false;
    std::vector<ScatterplotPointImputationValues> diagnosticImputationValues;
    // Always retains the full per-imputation point clouds, even when the
    // visible glyph mode is filtered to directly imputed cases.  Overlays and
    // smoothers must still use the complete fitted dataset in that mode.
    std::vector<ScatterplotPointImputationValues> diagnosticAllImputationValues;
    // Original row ids for which the response or a predictor in the exact
    // fitted model was imputed.  Per-imputation fits can move every fitted
    // value, so coordinate variation alone must never define this mask.
    std::set<int> diagnosticRowsWithImputedModelInputs;
    // "direct" shows uncertainty only for rows with an imputed model input;
    // "all" also shows fitted-value movement propagated through the models.
    std::string diagnosticImputationUncertaintyScope = "all";
    // Partial-regression plots are computed in R because they must use the
    // exact per-imputation design matrix and term coding.  Retaining those
    // point clouds lets the native views switch imputations without silently
    // falling back to the ordinary residuals-vs-fitted diagnostic.
    std::vector<std::vector<DiagnosticPlotPoint>> regressionPartialPointsByImputation;
    std::string regressionPartialResidualType;
    std::string regressionPartialContributionScale;
    std::vector<std::string> regressionPartialAvailableTerms;
    std::string regressionPartialPendingAnalysisId;
    int displayedFitVersion = 0;
    int displayedDiagnosticsVersion = 0;
    int displayedDiagnosticOptionsVersion = 0;
    // Residual menus are keyed to the fitted model family, not inferred from
    // the selected residual name: standardized and studentized residuals are
    // valid for both lm and glm fits.
    bool generalizedDiagnosticResiduals = false;
    std::string displayedResidualType;
    // Captured from the R-backed model capability map when the diagnostic is
    // opened.  Frontends must not reconstruct this list from generic GLM
    // assumptions because discrete families expose different residuals.
    std::vector<std::string> diagnosticAvailableResidualTypes;
    std::vector<DataPoint> points;
    std::vector<NumericVariable> variables;
    std::vector<VariableMeta> variableMeta;
    std::vector<std::string> scatterMatrixVariables;
    // R fits for matrix cells are keyed by this generation. A late reply for
    // an older selection, colour map, variable set or dataset is discarded.
    std::uint64_t scatterMatrixFitGeneration = 0;
    bool scatterMatrixFitsPending = false;
    std::vector<Overlay> overlays;
    std::vector<BoxplotPoint> boxplotPoints;
    std::vector<std::string> boxplotCategories;
    std::vector<std::string> boxplotDefinedCategories;
    // Ordered outer-to-inner grouping variables and the corresponding level
    // values for each defined category. Keeping this separate from the
    // display label lets renderers add meaningful gaps between nested groups.
    std::vector<std::string> boxplotGroupingVariables;
    std::vector<std::vector<std::string>> boxplotCategoryLevels;
    std::vector<std::string> boxplotVariables;
    std::vector<HistogramPoint> histogramPoints;
    std::vector<HistogramBin> histogramBins;
    std::vector<BarplotBin> barplotBins;
    std::vector<BiplotLoading> biplotLoadings;
    std::vector<InteractionPlotLine> interactionPlotLines;
    std::string timeSeriesGroupVariable;
    std::string timeSeriesTimeType = "numeric";
    std::string timeSeriesIdentification = "legend";
    std::string timeSeriesLegendPosition = "top_right";
    // Effect plots for interactions share the grouped-line legend geometry, but keep
    // their placement independently so changing it never refits the model.
    std::string interactionLegendPosition = "right";
    std::string interactionLegendTitle;
    // Display-only labels; the original xLabel/yLabel remain identifiers used by
    // R-code provenance and commands.
    std::string presentationXLabel;
    std::string presentationYLabel;
    std::string interactionLegendDefaultTitle;
    std::string interactionFocalVariable;
    std::string interactionLegendTitleOverride;
    bool interactionLegendTitleVisible = true;
    std::string subtitle;
    // A dragged effect or time-series legend stores the first legend sample as a normalized
    // point inside the plot rectangle. This keeps manual placement stable when
    // the window is resized and lets every renderer reproduce the same layout.
    bool interactionLegendUsesCustomPosition = false;
    double interactionLegendX = 0.72;
    double interactionLegendY = 0.10;
    // Connecting fitted estimates is independent from interval geometry.
    // Nominal main effects default to unconnected points; interaction and
    // continuous-effect curves can retain their meaningful connecting lines.
    bool regressionConnectEstimates = true;
    // Confidence intervals remain available from the plot menu, but new
    // effect plots start without interval bars/bands so the fitted estimates
    // and their connecting line are the uncluttered default view.
    bool regressionConfidenceIntervalsVisible = false;
    bool regressionConfidenceLevelVisible = true;
    double regressionConfidenceLevel = 0.95;
    bool regressionBinaryProbability = false;
    bool regressionBoundedCount = false;
    bool regressionCeilingHurdle = false;
    std::string regressionTrialsVariable;
    double regressionTrialsConstant = std::numeric_limits<double>::quiet_NaN();
    std::string regressionEffectEvent;
    std::string regressionEffectQuantity = "predicted_probability";
    std::string regressionEffectAdjustment = "average_sample";
    std::string regressionEffectPresentation = "percentage";
    // Monotonic transient request identity used by native adapters when an
    // effect plot is recalculated with another interaction variable on X.
    std::uint64_t regressionEffectRequestGeneration = 0;
    std::vector<std::string> timeSeriesPointGroups;
    std::string trellisConditionVariable;
    std::vector<std::string> trellisPointPanels;
    std::vector<std::vector<std::string>> trellisPointConditionValues;
    std::vector<std::string> trellisPointCategories;
    std::vector<std::string> trellisPointSplitValues;
    std::vector<std::string> trellisPanelLevels;
    std::vector<std::vector<std::string>> trellisPanelLevelValues;
    std::vector<std::string> trellisPanelLabels;
    std::string trellisLayoutMode = "automatic";
    std::string trellisPanelOrder = "defined";
    // Conventional/default placement: shared Y axis on the left and any
    // row-conditioning strips on the right.  The inverse arrangement remains
    // an explicit plot option.
    bool trellisRowStripsOnLeft = false;
    TrellisPlotSpecification trellisSpecification;
    bool trellisSpecificationInitialized = false;
    std::vector<DataPoint> rocStepPoints;
    std::vector<ROCThresholdLink> rocThresholds;
    std::vector<SmoothCurveData> smoothCurves;
    std::map<std::string, std::vector<SmoothCurveData>> trellisPanelSmoothCurves;
    double smoothSpan = 0.75;
    bool smoothShowSpanSlider = false;
    // Confidence bands are deliberately opt-in because several coloured fits
    // can otherwise hide the observations that the plot is meant to explore.
    // Straight regression fits and smoothed fits have independent controls.
    bool scatterShadeOverlap = true;
    bool scatterSizeByOverlap = false;
    bool scatterFitConfidenceIntervalsVisible = false;
    bool scatterSmoothConfidenceIntervalsVisible = false;
    double scatterFitConfidenceLevel = 0.95;
    double pointSizeScale = 1.0;
    bool pointSizeSliderVisible = false;
    // Categorical colour mapping selected by `Color by`. The owning front end
    // also publishes this mapping to the dataset's linked row-colour registry
    // so every coordinated view receives the same colours. Keeping the
    // semantic mapping here lets the plot build and validate its legend.
    std::string colorByVariable;
    std::map<int, std::string> colorByRowColors;
    std::vector<std::pair<std::string, std::string>> colorByLegendItems;
    // Semantic level-to-row mapping used by linked legend selection.  Do not
    // infer membership from the palette colour: palettes may legitimately
    // reuse a colour when they contain more levels than swatches.
    std::map<std::string, std::vector<int>> colorByLegendRows;
    bool colorByLegendVisible = true;
    // Normalized from the top-left of the complete plot surface so the same
    // placement survives platform changes, resizing, snapshots and export.
    double colorByLegendX = 0.72;
    double colorByLegendY = 0.10;
    std::vector<std::pair<double, std::string>> interactionXTicks;
    // Source observations represented by each categorical effect-axis tick,
    // aligned with `interactionXTicks`. Prediction-grid points do not carry
    // source row identity, so categorical linked selection needs this map.
    std::vector<std::vector<int>> interactionXTickRows;
    std::vector<DataPoint> screeParallelPoints;
    std::string dimensionalityModelId;
    bool isGLMInteractionPlot = false;
    std::string glmInteractionTerm;
    int biplotXComponent = 1;
    int biplotYComponent = 2;
    // Component indices and their analysis-specific labels (PC1, F1, ...).
    // Axis menus for biplots must use these fitted dimensions rather than
    // the source columns retained in `variables` for linked selection.
    std::vector<std::pair<int, std::string>> biplotComponentOptions;
    std::vector<std::string> barplotXVariables;
    std::string barplotMode = "count";
    std::string barplotWidthMode = "equal";
    std::string barplotSplitVariable;
    std::string barplotSegmentPalette = "automatic_color";
    std::map<std::string, std::string> barplotYLevelColors;
    std::map<std::string, double> barplotYLevelAlpha;
    std::map<std::string, std::string> barplotYLevelPatterns;
    std::map<std::string, std::string> barplotSegmentColorOverrides;
    std::map<std::string, double> barplotSegmentAlphaOverrides;
    std::map<std::string, std::string> barplotSegmentPatternOverrides;
    double barplotDefaultSegmentAlpha = 0.70;
    double barplotSplitStrokeWidth = 3.0;
    bool barplotShowPatterns = false;
    std::string barplotSegmentEncodingMode = "transparent_color_only";
    std::string barplotRowColorDisplay = "hide";
    std::string barplotSelectionDisplay = "overlay";
    bool barplotShowConditionalPercent = false;
    int barplotTotalN = 0;
    std::vector<int> barplotExcludedRows;
    int barplotSelectionVersionDisplayed = 0;
    bool boxplotShowPoints = true;
    bool boxplotShowBox = true;
    bool boxplotShowWhiskers = true;
    bool boxplotConnectRows = false;
    bool boxplotStandardizeVariables = false;
    bool boxplotShowConnectionWidthSlider = false;
    double boxplotConnectionLineWidth = 1.0;
    bool boxplotShowViolin = false;
    std::string boxplotGroupOrder = "defined";
    bool boxplotSplitViolin = false;
    std::string boxplotSplitAlternative = "less";
    double boxplotSplitLower = NAN;
    double boxplotSplitUpper = NAN;
    bool boxplotShowH0Simulation = false;
    std::string boxplotH0Alternative = "two.sided";
    double boxplotH0 = 0.0;
    int boxplotH0Draws = 1000;
    std::vector<double> boxplotH0Values;
    std::vector<double> boxplotH0ReferenceLines;
    double boxplotH0Lower = NAN;
    double boxplotH0Upper = NAN;
    double boxplotH0PValue = NAN;
    double boxplotH0Effect = NAN;
    std::string boxplotH0Label = "H0";
    std::string boxplotH0Status;
    bool histogramShowCounts = true;
    bool histogramShowTickMarks = true;
    bool histogramShowTickLabels = true;
    bool histogramShowRug = false;
    bool histogramShowDensity = false;
    std::string histogramDensityMode = "all";
    std::string labelDisplayMode = "none";
    std::string scatterImputationUncertainty = "central80";
    double histogramDensityBw = 0.0;
    double histogramDensityAdjust = 1.0;
    int nextOverlayId = 1;
    std::string interactionMode = "select";
    std::string selectionMode = "replace";
    double dataXmin = 0.0;
    double dataXmax = 1.0;
    double dataYmin = 0.0;
    double dataYmax = 1.0;
    double xmin = 0.0;
    double xmax = 1.0;
    double ymin = 0.0;
    double ymax = 1.0;
    double brushWidth = 80.0;
    double brushHeight = 60.0;
};

// Derived plots are refreshed by their source analysis. Ordinary exploratory
// plots are refreshed directly from the dataset whenever the global scope
// changes, unless the user explicitly froze that plot.
bool PlotIsDerivedAnalysisView(const PlotModel &model);
bool PlotReceivesGlobalAnalysisScopeUpdates(const PlotModel &model);

struct SpreadPlotMessage {
    std::string type;
    std::string senderId;
    std::string groupId;
    std::string modelId;
    std::vector<int> rowIds;
    int modelVersion = 0;
    int fitVersion = 0;
    int diagnosticsVersion = 0;
    int selectionVersion = 0;
};

NumericVariable *FindNumericVariable(PlotModel &model, const std::string &name);
const NumericVariable *FindNumericVariable(const PlotModel &model, const std::string &name);
const VariableMeta *FindVariableMeta(const PlotModel &model, const std::string &name);
std::vector<std::string> VariableNames(const PlotModel &model);
std::vector<std::string> NumericVariableNames(const PlotModel &model);
inline std::vector<std::string> NumericVariableNames(const PlotModel *model) {
    return model ? NumericVariableNames(*model) : std::vector<std::string>();
}
bool VariableExists(const PlotModel &model, const std::string &name);
std::vector<std::string> AvailableVariableNames(const PlotModel &model,
                                                const DataFrameModel *df = nullptr);
bool AvailableVariableExists(const PlotModel &model,
                             const std::string &name,
                             const DataFrameModel *df = nullptr);
std::string VariableRole(const PlotModel &model, const std::string &variableName);
std::string VariableRole(const PlotModel &model,
                         const std::string &variableName,
                         const std::string &dependent,
                         const std::vector<std::string> &terms);
std::string DefaultTermType(const PlotModel &model, const std::string &term);
inline std::string DefaultTermType(const PlotModel *model, const std::string &term) {
    return model ? DefaultTermType(*model, term) : "numeric";
}
std::string ModelTermDisplayType(const PlotModel &model,
                                 const std::string &term,
                                 const std::map<std::string, std::string> &overrides);
inline std::string ModelTermDisplayType(const PlotModel *model,
                                        const std::string &term,
                                        const std::map<std::string, std::string> &overrides) {
    return model ? ModelTermDisplayType(*model, term, overrides) : term;
}
std::string TermDisplayType(const std::string &term, const std::string &type, const std::string &response);
bool PlotIsAggregateDiagnostic(const PlotModel &model);
bool PlotHasConfidenceIntervals(const PlotModel &model);
bool PlotIsImputationDiagnostic(const PlotModel &model);
void SelectDiagnosticPlotSeries(PlotModel &model, std::optional<std::size_t> series, bool extend = false);
std::set<int> DiagnosticPlotSelectedPointIds(const PlotModel &model);
std::vector<std::pair<int, std::string>> ImputationDiagnosticBatchOptions(const PlotModel &model);
std::string ImputationDiagnosticDisplayStatus(const PlotModel &model);
std::string TimeSeriesPlotExplanation(const PlotModel &model);
bool SetTimeSeriesLegendPosition(PlotModel &model, const std::string &position);
bool PlotLinksToDataRows(const PlotModel &model);
// Case exclusion changes the dataset-wide AnalysisScope and refits analyses.
// Row-linked residual diagnostics support this workflow even though their
// plotted values are derived from a fitted model. Aggregate diagnostics do not.
bool PlotSupportsCaseExclusion(const PlotModel &model);
bool PlotSupportsObservationLabels(const PlotModel &model);
std::string TitleForPlot(const PlotModel &model);
std::string PlotListItemText(const PlotModel &model);
std::string PlotInfoResponseText(const PlotModel &model, std::size_t selectedCount);
std::string PlotVariablesResponseText(const PlotModel &model);
std::string PlotOverlaysResponseText(const PlotModel &model);
std::string PlotDiagnosticInfoResponseText(const PlotModel &model);

struct IndexedScreenPoint {
    int index = 0;
    Point point;
};

struct LabeledScreenSegment {
    std::string id;
    Point start;
    Point end;
    Rect labelRect;
    bool hasLabelRect = false;
};

bool IsValidRect(const Rect &rect);
// Keep zero-based bars and their scale inside the panel frame.
Rect ZeroBaselineContentRect(const Rect &plotRect);
double ZeroBaselineY(const Rect &plotRect, double fractionOfMaximum);
bool IsValidViewport(const DataViewport &viewport);
DataViewport DataViewportForPoints(const std::vector<Point> &points,
                                   double paddingFraction = 0.05);

Point DataToScreen(const Point &data,
                   const DataViewport &viewport,
                   const Rect &plotRect,
                   bool yAxisDown = true);

Point ScreenToData(const Point &screen,
                   const DataViewport &viewport,
                   const Rect &plotRect,
                   bool yAxisDown = true);

bool PointInRect(const Point &point, const Rect &rect);
bool RectIntersects(const Rect &a, const Rect &b);
bool ClipLineToRect(Point &start, Point &end, const Rect &clip);
Rect ExpandRect(const Rect &rect, double amount);
Rect ExpandRect(const Rect &rect, double xAmount, double yAmount);
Rect RectBetweenPoints(const Point &a, const Point &b);
Rect BrushRectForGesture(const Point &start,
                         const Point &current,
                         bool useFixedBrush,
                         double brushWidth,
                         double brushHeight,
                         double clickThreshold = 3.0);

// Only modes with implemented linked-row semantics may start or complete a
// selection gesture.  Keeping this decision in core prevents platform views
// from treating display-only/legacy modes as ordinary selection.
bool PlotInteractionModeAllowsRowSelection(const std::string &mode);
bool RectMeetsMinimumSize(const Rect &rect, double minimumWidth, double minimumHeight);
double DistanceSquared(const Point &a, const Point &b);
double DistanceSquaredToRect(const Point &point, const Rect &rect);
double DistanceSquaredToSegment(const Point &point,
                                const Point &start,
                                const Point &end);
int HitNearestIndexedPoint(const Point &point,
                           const std::vector<IndexedScreenPoint> &candidates,
                           double maxDistance);
std::string HitNearestLabeledSegment(const Point &point,
                                      const std::vector<LabeledScreenSegment> &candidates,
                                      double maxDistance);

void SetVariableMetaType(std::vector<VariableMeta> &meta, const std::string &variable, const std::string &type);

bool HasOverlaySource(const PlotModel &model, const std::string &source);
inline bool HasOverlaySource(const PlotModel *model, const std::string &source) {
    return model && HasOverlaySource(*model, source);
}
void AddOverlaySource(PlotModel &model, const std::string &source);
inline void AddOverlaySource(PlotModel *model, const std::string &source) {
    if (model) AddOverlaySource(*model, source);
}
void RemoveOverlaySource(PlotModel &model, const std::string &source);
inline void RemoveOverlaySource(PlotModel *model, const std::string &source) {
    if (model) RemoveOverlaySource(*model, source);
}
inline void ToggleOverlaySource(PlotModel &model, const std::string &source) {
    if (HasOverlaySource(model, source)) {
        RemoveOverlaySource(model, source);
    } else {
        AddOverlaySource(model, source);
    }
}
inline void ToggleOverlaySource(PlotModel *model, const std::string &source) {
    if (model) ToggleOverlaySource(*model, source);
}

SmoothCurveData PendingSmoothCurve(SmoothCurveScope scope,
                                   const std::string &fitMethod = "loess");
SmoothCurveData EnabledEmptySmoothCurve(SmoothCurveScope scope,
                                        const std::string &fitMethod = "loess");
double ClampSmoothSpan(double span);
double ClampPointSizeScale(double scale);
bool SmoothCurveScopeIsPresent(const std::vector<SmoothCurveData> &curves,
                               SmoothCurveScope scope);
bool FitCurveScopeIsPresent(const std::vector<SmoothCurveData> &curves,
                            SmoothCurveScope scope,
                            const std::string &fitMethod,
                            bool requireValid = false);
bool MarkSmoothCurveScopePendingIfPresent(std::vector<SmoothCurveData> &curves,
                                          SmoothCurveScope scope);
bool MarkExistingSmoothCurvesPending(std::vector<SmoothCurveData> &curves);
// A coordinate change makes every fitted curve geometrically invalid. Keep
// the enabled scopes, discard the curves in the old coordinate system, and
// leave one pending marker per scope for the R refit.
bool InvalidateSmoothCurvesForCoordinateChange(
    std::vector<SmoothCurveData> &curves);
bool ToggleSmoothCurveScopePending(std::vector<SmoothCurveData> &curves,
                                   SmoothCurveScope scope);
std::vector<InteractionPlotLine> BuildTimeSeriesLines(
    const std::vector<DataPoint> &points,
    const std::vector<std::string> &pointGroups,
    const std::string &defaultLabel);
std::set<int> PlotColorLegendRows(const PlotModel &plot,
                                  const std::string &level);
// Applies a changed plot-level `Color by` mapping to the shared linked-row
// colour registry. Colours previously contributed by this plot are removed
// only while they still match the old mapping, preserving later manual
// overrides. A new mapping intentionally becomes the current linked colour
// assignment for its rows.
std::vector<int> SynchronizePlotColorByLinkedRows(
    std::map<int, std::string> &linkedRowColors,
    const std::map<int, std::string> &oldColorByRows,
    const std::map<int, std::string> &newColorByRows);
// A categorical legend is truthful only while linked/manual colours do not
// override any of its semantic category colours.
bool PlotColorLegendMatchesLinkedRowColors(
    const PlotModel &plot,
    const std::map<int, std::string> &linkedRowColors);
std::set<int> PlotSeriesLegendRows(const PlotModel &plot,
                                   std::size_t seriesIndex);
std::set<int> PlotEffectCategoryRows(const PlotModel &plot,
                                     std::size_t categoryIndex);
bool PlotSeriesIntersectsSelection(const PlotModel &plot,
                                   std::size_t seriesIndex,
                                   const std::set<int> &selectedRows);
bool PlotSeriesIsFullySelected(const PlotModel &plot,
                               std::size_t seriesIndex,
                               const std::set<int> &selectedRows);
std::map<int, std::string> TimeSeriesRowColors(const PlotModel &plot);
std::optional<std::size_t> HitPlotSeriesAtScreenPoint(
    const PlotModel &plot,
    const DataViewport &viewport,
    const Rect &plotRect,
    const Point &screenPoint,
    double maxDistance = 8.0);
// A click on a visible time-series marker selects its case; a click on the
// connecting segment selects the complete series.
std::optional<std::size_t> HitTimeSeriesSegmentAtScreenPoint(
    const PlotModel &plot,
    const DataViewport &viewport,
    const Rect &plotRect,
    const Point &screenPoint,
    double markerRadius,
    double lineTolerance = 9.0);
bool ParseTimeSeriesCellValue(const std::string &text,
                              const std::string &timeType,
                              double &value);
std::string InferTimeSeriesTimeType(const DataColumn &column);
bool RebuildTimeSeriesFromDataFrame(PlotModel &plot,
                                    const DataFrameModel &df,
                                    std::string *error = nullptr);
std::string FormatTimeSeriesTick(double value, const std::string &timeType);
// Component positions are discrete, including when the viewport has fractional padding.
std::vector<TimeSeriesAxisTick> BuildScreeAxisTicks(const PlotModel &model);
std::vector<TimeSeriesAxisTick> BuildTimeSeriesAxisTicks(
    double minimum,
    double maximum,
    const std::string &timeType,
    int targetCount = 6);
std::vector<TimeSeriesLegendItem> BuildTimeSeriesLegendLayout(
    std::size_t seriesCount,
    const Rect &plotRect,
    const std::string &position,
    double rowHeight = 17.0,
    double legendWidth = 156.0);
bool SetInteractionLegendPosition(PlotModel &model,
                                  const std::string &position);
bool SetInteractionLegendCustomPosition(PlotModel &model,
                                        double normalizedX,
                                        double normalizedY);
std::vector<TimeSeriesLegendItem> BuildInteractionLegendLayout(
    const PlotModel &model,
    const Rect &plotRect,
    double rowHeight = 17.0,
    double legendWidth = 156.0);
// Shared native/SVG panel geometry. Outside legends reserve space before axes are drawn.
Rect RegressionEffectPlotRect(const PlotModel &model, double width, double height);
std::vector<std::string> RegressionEffectTitleLines(const PlotModel &model, double width);
std::string RegressionEffectTickLabel(double value, double minimum, double maximum);
// The presence of categorical tick labels is set by the R model
// specification, not inferred from the storage type of the source column.
bool RegressionEffectXAxisIsCategorical(const PlotModel &model);
// True only for R-backed regression effect/interaction plots whose estimates
// represent the fitted model across imputations.  Descriptive plots that show
// one completed dataset must not be confused with these pooled results.
bool IsPooledRegressionEffectPlot(const PlotModel &model);
// Pixel offset shared by native renderers so estimates and their pointwise
// intervals are dodged by exactly the same amount within a category.
double RegressionEffectSeriesDodgeOffset(std::size_t seriesIndex,
                                         std::size_t seriesCount);
std::vector<TimeSeriesDirectLabel> BuildTimeSeriesDirectLabels(
    const std::vector<InteractionPlotLine> &lines,
    const DataViewport &viewport,
    const Rect &plotRect,
    double lineHeight = 14.0);

NumericVariable NumericVariableFromColumn(const DataColumn &col);
// Builds the two numeric coordinate vectors required by a scatterplot from
// their canonical worksheet cells, irrespective of the columns' declared
// analysis types.  This is used by analysis-derived plots (for example an
// ordinal scale-item correlation): the analysis type remains unchanged in
// variableMeta while parseable numeric item scores become plot coordinates.
bool PrepareScatterplotVariablesFromDataFrame(PlotModel &plot,
                                              const DataFrameModel &df,
                                              const std::string &xVariable,
                                              const std::string &yVariable,
                                              std::string *error = nullptr);
void SyncPlotVariableFromColumn(PlotModel &plot, const DataColumn &col);
inline void SyncPlotVariableFromColumn(PlotModel *plot, const DataColumn &col) {
    if (plot) SyncPlotVariableFromColumn(*plot, col);
}
void SyncPlotVariablesFromDataFrame(PlotModel &plot, const DataFrameModel &df);
inline void SyncPlotVariablesFromDataFrame(PlotModel *plot, const DataFrameModel &df) {
    if (plot) SyncPlotVariablesFromDataFrame(*plot, df);
}
void PopulateDatasetSeedPlot(PlotModel &seed,
                             const DataFrameModel &df,
                             const std::string &group);
// Builds the analysis seed used by correlations and model configuration from
// a portable data frame. It deliberately has no view or platform ownership.
bool BuildComparisonSeedPlot(const DataFrameModel &df,
                             const std::string &group,
                             PlotModel &seed);
void RefreshDatasetSeedPlotAfterVariableSync(PlotModel &seed);

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_PLOT_GEOMETRY_H
