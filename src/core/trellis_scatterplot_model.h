#ifndef RLISPSTAT_CORE_TRELLIS_SCATTERPLOT_MODEL_H
#define RLISPSTAT_CORE_TRELLIS_SCATTERPLOT_MODEL_H

#include "dataset_model.h"
#include "barplot_model.h"
#include "boxplot_model.h"
#include "histogram_model.h"
#include "plot_geometry.h"
#include "scatterplot_model.h"
#include "selection_model.h"

#include <optional>
#include <set>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

struct TrellisAxisTick {
    double value = 0.0;
    double position = 0.0;
    std::string label;
};

struct NumericDomain {
    double minimum = 0.0;
    double maximum = 1.0;
};

struct PlotAxisState {
    NumericDomain dataDomain;
    NumericDomain displayDomain;
    std::vector<TrellisAxisTick> ticks;
};

struct PlotPanelContext {
    Rect outerRect;
    Rect rowStripRect;
    Rect columnStripRect;
    Rect nestedStripRect;
    Rect plotRect;
    Rect plotClip;
    PlotAxisState xAxis;
    PlotAxisState yAxis;
    DataViewport viewport;
    std::vector<CaseId> originalRowIndices;
    bool drawXAxis = true;
    bool drawYAxis = true;
    bool drawXTickLabels = true;
    bool drawYTickLabels = true;
};

struct TrellisPanelLayout {
    std::string panelId;
    std::string levelId;
    std::string label;
    std::vector<std::string> conditionValues;
    std::string columnStripLabel;
    std::string rowStripLabel;
    std::size_t row = 0;
    std::size_t column = 0;
    Rect frame;
    Rect headerRect;
    Rect rowStripRect;
    Rect columnStripRect;
    Rect nestedStripRect;
    Rect plotRect;
    DataViewport viewport;
    PlotPanelContext context;
    std::vector<TrellisAxisTick> xTicks;
    std::vector<TrellisAxisTick> yTicks;
    bool showXTickLabels = false;
    bool showYTickLabels = false;
    bool hasObservations = false;
};

struct TrellisCaseGeometry {
    CaseId caseId = 0;
    std::size_t panelIndex = 0;
    Point dataPoint;
    Point screenPoint;
};

enum class TrellisAggregateKind {
    Box,
    Bar,
    HistogramBin
};

struct TrellisAggregateGeometry {
    TrellisAggregateKind kind = TrellisAggregateKind::Bar;
    std::size_t panelIndex = 0;
    std::string id;
    std::string label;
    Rect rect;
    std::vector<CaseId> caseIds;
    double value = 0.0;
    double lower = NAN;
    double upper = NAN;
    double median = NAN;
    double whiskerLow = NAN;
    double whiskerHigh = NAN;
};

struct TrellisScatterplotLayout {
    std::vector<TrellisPanelLayout> panels;
    std::vector<TrellisCaseGeometry> cases;
    std::vector<TrellisAggregateGeometry> aggregates;
    std::size_t columns = 1;
    std::size_t rows = 1;
    DataViewport viewport;
    Rect panelsBoundingRect;
    Rect titleRect;
    Point xAxisLabelAnchor;
    Point yAxisLabelAnchor;
    double minimumUsefulPanelWidth = 170.0;
    double minimumUsefulPanelHeight = 120.0;
};

struct TrellisTimeSeriesLine {
    std::string label;
    std::string colorKey;
    std::vector<Point> points;
    std::vector<CaseId> caseIds;
};

struct TrellisPanelRenderPlan {
    std::size_t panelIndex = 0;
    PlotPanelContext context;
    ScatterplotRenderPlan scatterplot;
    HistogramRenderPlan histogram;
    BoxplotLayout boxplotLayout;
    std::vector<BoxplotCaseGeometry> boxplotCases;
    std::vector<BoxplotPointDrawItem> boxplotPoints;
    std::vector<BoxplotStatsRenderItem> boxplotStats;
    BarplotLayout barplotLayout;
    std::vector<BarplotBinSummary> barplotBins;
    std::vector<std::vector<BarplotSegmentDrawItem>> barplotSegments;
    std::vector<TrellisTimeSeriesLine> timeSeriesLines;
    std::vector<TimeSeriesLegendItem> timeSeriesLegend;
    std::vector<TimeSeriesDirectLabel> timeSeriesDirectLabels;
};

struct TrellisDerivedMetadata {
    std::string xVariableId;
    std::string yVariableId;
    std::string xAxisLabel;
    std::string yAxisLabel;
    std::string internalTitle;
    std::string windowTitle;
    std::string accessibleDescription;
    std::vector<std::string> conditioningVariableIds;
};

std::string TrellisPlotTypeName(TrellisPlotType type);
std::optional<TrellisPlotType> ParseTrellisPlotType(const std::string &name);

void InitializeTrellisDataTableSpecification(PlotModel &plot,
                                             const DataFrameModel &df,
                                             bool restoreFromPlot = false);
std::vector<CaseId> TrellisPanelOriginalRows(const PlotModel &plot,
                                             const std::string &panelId);
std::vector<CaseId> OrderedTrellisDataTableRows(
    const TrellisDataTableSpecification &specification,
    const DataFrameModel &df,
    const std::vector<CaseId> &panelRows,
    const std::set<CaseId> &selection);
std::string TrellisDataTableTSV(const PlotModel &plot,
                                const DataFrameModel &df,
                                const std::vector<CaseId> &rows,
                                bool includePanelColumns = false,
                                const std::string &panelId = "");
std::string TrellisDataTableAllPanelsCSV(const PlotModel &plot,
                                         const DataFrameModel &df,
                                         const std::set<CaseId> &selection);
std::string TrellisDataTablePanelCSV(const PlotModel &plot,
                                     const DataFrameModel &df,
                                     const std::string &panelId,
                                     const std::set<CaseId> &selection);

void InitializeTrellisSpecificationFromLegacy(PlotModel &plot);
TrellisDerivedMetadata DeriveTrellisMetadata(const PlotModel &plot);
bool ValidateTrellisSpecification(const PlotModel &plot,
                                  const DataFrameModel &df,
                                  std::string *error = nullptr);
bool RebuildTrellisPlotFromDataFrame(PlotModel &plot,
                                     const DataFrameModel &df,
                                     std::string *error = nullptr);

// A trellis smooth is derived from both the displayed variables and the panel
// membership. Preserve which scopes the user enabled, but discard every
// panel curve when either input changes so curves in obsolete coordinates can
// never be drawn while R refits the panels.
bool InvalidateTrellisSmoothCurvesForDataChange(PlotModel &plot);

bool AddTrellisConditioningVariable(PlotModel &plot,
                                    const DataFrameModel &df,
                                    const std::string &variable,
                                    TrellisConditioningVariableKind kind,
                                    TrellisContinuousBinningMethod method,
                                    std::string *error = nullptr);
bool ReplaceTrellisConditioningVariable(PlotModel &plot,
                                        const DataFrameModel &df,
                                        const std::string &currentVariable,
                                        const std::string &replacementVariable,
                                        std::string *error = nullptr);
bool RemoveTrellisConditioningVariable(PlotModel &plot,
                                       const DataFrameModel &df,
                                       const std::string &variable,
                                       std::string *error = nullptr);
bool MoveTrellisConditioningVariable(PlotModel &plot,
                                     const DataFrameModel &df,
                                     const std::string &variable,
                                     int direction,
                                     std::string *error = nullptr);
bool ConfigureTrellisContinuousCondition(PlotModel &plot,
                                         const DataFrameModel &df,
                                         const std::string &variable,
                                         TrellisContinuousBinningMethod method,
                                         std::size_t binCount,
                                         std::string *error = nullptr);
bool SetTrellisConditioningDimension(PlotModel &plot,
                                     const DataFrameModel &df,
                                     const std::string &variable,
                                     TrellisDimension dimension,
                                     std::string *error = nullptr);
bool SwapTrellisRowsAndColumns(PlotModel &plot,
                              const DataFrameModel &df,
                              std::string *error = nullptr);

struct TrellisViewSize {
    double width = 720.0;
    double height = 520.0;
};

bool TrellisConditionColumnIsValid(const DataColumn &column,
                                   int rowCount,
                                   std::string *error = nullptr);

bool RebuildTrellisScatterplotFromDataFrame(PlotModel &plot,
                                            const DataFrameModel &df,
                                            std::string *error = nullptr);

TrellisScatterplotLayout BuildTrellisScatterplotLayout(const PlotModel &plot,
                                                        const Rect &bounds);

std::vector<TrellisPanelRenderPlan> BuildTrellisPanelRenderPlans(
    const PlotModel &plot,
    const TrellisScatterplotLayout &layout,
    const std::set<CaseId> &selection,
    const std::map<CaseId, std::string> &rowColors,
    const std::map<CaseId, std::string> &rowLabels = {});

std::vector<std::string> OrderedTrellisPanelLevels(const PlotModel &plot);

std::vector<TrellisAxisTick> BuildTrellisAxisTicks(double minimum,
                                                   double maximum,
                                                   const Rect &plotRect,
                                                   bool horizontal,
                                                   int targetCount = 6);

std::optional<std::size_t> TrellisPanelIndexAtPoint(
    const TrellisScatterplotLayout &layout,
    const Point &point);

std::vector<std::size_t> TrellisConditioningVariableIndicesAtPoint(
    const PlotModel &plot,
    const TrellisScatterplotLayout &layout,
    const Point &point);

Point TrellisScreenToData(const TrellisScatterplotLayout &layout,
                          std::size_t panelIndex,
                          const Point &point);

std::optional<TrellisCaseGeometry> HitTrellisScatterplotCase(
    const TrellisScatterplotLayout &layout,
    const Point &point,
    double maximumDistance = 8.0);

std::optional<TrellisAggregateGeometry> HitTrellisAggregate(
    const TrellisScatterplotLayout &layout,
    const Point &point);

std::set<CaseId> SelectTrellisScatterplotCasesForGesture(
    const TrellisScatterplotLayout &layout,
    const Rect &brush,
    const Point &clickPoint,
    bool dragBrush,
    double maxClickDistance = 8.0,
    double glyphPadding = 2.0);

TrellisViewSize TrellisScatterplotPreferredViewSize(std::size_t panelCount);

std::string TrellisScatterplotDefaultTitle(const std::string &xVariable,
                                           const std::string &yVariable,
                                           const std::string &conditionVariable);

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_TRELLIS_SCATTERPLOT_MODEL_H
