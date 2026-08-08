#ifndef RLISPSTAT_CORE_PLOT_GEOMETRY_H
#define RLISPSTAT_CORE_PLOT_GEOMETRY_H

#include <cmath>
#include <map>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

#include "analysis_scope.h"

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
};

struct TrellisPlotSpecification {
    TrellisPlotType plotType = TrellisPlotType::Scatter;
    std::string xVariableId;
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

NumericVariable *FindNumericVariable(std::vector<NumericVariable> &vars, const std::string &name);
VariableMeta *FindVariableMeta(std::vector<VariableMeta> &metas, const std::string &name);
std::vector<std::string> VariableNames(const std::vector<VariableMeta> &metas);
std::vector<std::string> NumericVariableNames(const std::vector<NumericVariable> &vars);
bool VariableExists(const std::vector<VariableMeta> &metas, const std::string &name);

struct PlotModel {
    std::string kind = "scatter";
    std::string id;
    std::string group;
    std::string xLabel;
    std::string yLabel;
    std::string title;
    AnalysisScope dataScope;
    bool dataScopeCaptured = false;
    // Used by immutable Snapshot Album renderables.  These maps intentionally
    // exclude selection and detach appearance from the live dataset registry.
    bool frozenAppearanceCaptured = false;
    std::map<int, std::string> frozenRowColors;
    std::map<int, std::string> frozenRowLabels;
    bool isDatasetSeed = false;
    bool isGLMDiagnostic = false;
    std::string glmModelId;
    std::string glmDiagnosticKind;
    int displayedFitVersion = 0;
    int displayedDiagnosticsVersion = 0;
    std::vector<DataPoint> points;
    std::vector<NumericVariable> variables;
    std::vector<VariableMeta> variableMeta;
    std::vector<std::string> scatterMatrixVariables;
    std::vector<Overlay> overlays;
    std::vector<BoxplotPoint> boxplotPoints;
    std::vector<std::string> boxplotCategories;
    std::vector<std::string> boxplotDefinedCategories;
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
    bool trellisRowStripsOnLeft = true;
    TrellisPlotSpecification trellisSpecification;
    bool trellisSpecificationInitialized = false;
    std::vector<DataPoint> rocStepPoints;
    std::vector<ROCThresholdLink> rocThresholds;
    std::vector<SmoothCurveData> smoothCurves;
    std::map<std::string, std::vector<SmoothCurveData>> trellisPanelSmoothCurves;
    double smoothSpan = 0.75;
    bool smoothShowSpanSlider = false;
    std::vector<std::pair<double, std::string>> interactionXTicks;
    std::vector<DataPoint> screeParallelPoints;
    std::string dimensionalityModelId;
    bool isGLMInteractionPlot = false;
    std::string glmInteractionTerm;
    int biplotXComponent = 1;
    int biplotYComponent = 2;
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
    bool barplotShowPatterns = true;
    std::string barplotSegmentEncodingMode = "transparent_color_pattern";
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
bool PlotLinksToDataRows(const PlotModel &model);
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
                         bool useFixedBrushWhenSmall,
                         double brushWidth,
                         double brushHeight,
                         double clickThreshold = 3.0);
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

SmoothCurveData PendingSmoothCurve(SmoothCurveScope scope);
double ClampSmoothSpan(double span);
bool SmoothCurveScopeIsPresent(const std::vector<SmoothCurveData> &curves,
                               SmoothCurveScope scope);
bool MarkSmoothCurveScopePendingIfPresent(std::vector<SmoothCurveData> &curves,
                                          SmoothCurveScope scope);
bool MarkExistingSmoothCurvesPending(std::vector<SmoothCurveData> &curves);
bool ToggleSmoothCurveScopePending(std::vector<SmoothCurveData> &curves,
                                   SmoothCurveScope scope);
std::vector<InteractionPlotLine> BuildTimeSeriesLines(
    const std::vector<DataPoint> &points,
    const std::vector<std::string> &pointGroups,
    const std::string &defaultLabel);
bool ParseTimeSeriesCellValue(const std::string &text,
                              const std::string &timeType,
                              double &value);
std::string InferTimeSeriesTimeType(const DataColumn &column);
bool RebuildTimeSeriesFromDataFrame(PlotModel &plot,
                                    const DataFrameModel &df,
                                    std::string *error = nullptr);
std::string FormatTimeSeriesTick(double value, const std::string &timeType);
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
std::vector<TimeSeriesDirectLabel> BuildTimeSeriesDirectLabels(
    const std::vector<InteractionPlotLine> &lines,
    const DataViewport &viewport,
    const Rect &plotRect,
    double lineHeight = 14.0);

NumericVariable NumericVariableFromColumn(const DataColumn &col);
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
