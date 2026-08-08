#ifndef RLISPSTAT_CORE_SCATTERPLOT_MODEL_H
#define RLISPSTAT_CORE_SCATTERPLOT_MODEL_H

#include "dataset_model.h"
#include "plot_geometry.h"
#include "selection_model.h"

#include <map>
#include <optional>
#include <set>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

struct ScatterplotCaseGeometry {
    CaseId caseId = 0;
    Point point;
    bool hasGlyphRect = false;
    Rect glyphRect;
};

struct ScatterplotPointValue {
    CaseId caseId = 0;
    double x = 0.0;
    double y = 0.0;
};

struct ScatterplotImputationGlyph {
    bool hasImputation = false;
    int axisMask = 0;
    Rect rect;
};

struct ScatterplotPointDrawInput {
    CaseId caseId = 0;
    Point point;
    bool hasImputationGlyph = false;
    ScatterplotImputationGlyph imputationGlyph;
};

struct ScatterplotPointDrawItem {
    CaseId caseId = 0;
    Point point;
    bool selected = false;
    bool hasImputationGlyph = false;
    ScatterplotImputationGlyph imputationGlyph;
    std::string colorName;
    bool hasExplicitColor = false;
    double fillAlpha = 0.72;
    double strokeAlpha = 0.42;
    double radius = 3.0;
    double strokeWidth = 0.6;
    bool hasHalo = false;
    double haloRadius = 0.0;
    double haloAlpha = 0.0;
    bool showLabel = false;
    std::string label;
    Point labelAnchor;
};

struct SimpleLinearFitResult {
    bool ok = false;
    int n = 0;
    int excluded = 0;
    double intercept = 0.0;
    double slope = 0.0;
    double r2 = 0.0;
    double sse = 0.0;
    double sigma = 0.0;
    double xMean = 0.0;
    double yMean = 0.0;
    std::string warning;
};

struct ScatterplotFittedLine {
    bool ok = false;
    int n = 0;
    double intercept = 0.0;
    double slope = 0.0;
    Point start;
    Point end;
};

struct ScatterplotOverlaySpec {
    std::string type;
    std::string source;
    bool visible = false;
};

struct ScatterplotOverlayLineItem {
    Point start;
    Point end;
    std::string colorName;
    double alpha = 0.90;
    double lineWidth = 2.0;
    bool dashed = false;
    bool useDefaultDarkColor = false;
};

struct ScatterplotOverlayDrawItem {
    Point start;
    Point end;
    std::string colorName;
    double alpha = 0.90;
    double lineWidth = 2.0;
    bool dashed = false;
    bool useDefaultDarkColor = false;
};

struct ScatterplotSmoothCurveDrawItem {
    std::vector<Point> points;
    std::string colorName;
    double alpha = 0.85;
    double lineWidth = 2.5;
    bool dashed = false;
};

struct ScatterplotRenderInput {
    std::vector<ScatterplotPointValue> points;
    DataViewport viewport;
    Rect plotRect;
    std::set<CaseId> selectedRows;
    std::map<CaseId, std::string> rowColors;
    std::map<CaseId, std::string> rowLabels;
    std::string labelDisplayMode = "none";
    std::vector<ScatterplotOverlaySpec> overlays;
    std::string selectedColorName = "black";
    const DataFrameModel *imputationDataFrame = nullptr;
    const DataColumn *xColumn = nullptr;
    const DataColumn *yColumn = nullptr;
    std::string imputationUncertaintyMode = "central80";
    bool skipNonCaseRows = false;
    std::vector<SmoothCurveData> smoothCurves;
};

struct ScatterplotRenderPlan {
    std::vector<ScatterplotPointDrawItem> points;
    std::vector<ScatterplotOverlayDrawItem> overlayLines;
    std::vector<ScatterplotSmoothCurveDrawItem> smoothCurves;
};

struct ScatterplotMenuOption {
    std::string title;
    std::string value;
    std::string command;
    bool enabled = true;
    bool checked = false;
};

struct ScatterplotVariableMenuState {
    std::vector<ScatterplotMenuOption> xOptions;
    std::vector<ScatterplotMenuOption> yOptions;
    ScatterplotMenuOption openVariablesWindow;
};

struct ScatterplotMenuState {
    ScatterplotVariableMenuState variables;
    std::vector<ScatterplotMenuOption> mouseModeOptions;
    std::vector<ScatterplotMenuOption> selectionModeOptions;
    std::vector<ScatterplotMenuOption> selectionActionOptions;
    std::vector<ScatterplotMenuOption> brushOptions;
    std::vector<ScatterplotMenuOption> viewOptions;
    bool showImputationUncertaintyOptions = false;
    std::vector<ScatterplotMenuOption> imputationUncertaintyOptions;
    ScatterplotMenuOption chooseLabelColumn;
    std::vector<ScatterplotMenuOption> labelDisplayOptions;
    std::vector<ScatterplotMenuOption> overlayOptions;
    ScatterplotMenuOption clearOverlays;
    std::vector<ScatterplotMenuOption> smoothOptions;
    std::vector<ScatterplotMenuOption> analysisOptions;
    std::vector<ScatterplotMenuOption> plotOptions;
    ScatterplotMenuOption closePlot;
};

struct ScatterplotCreationDialogState {
    std::string title = "Scatterplot";
    std::string informativeText = "Choose variables from the active dataset. The new plot will share row selection with existing plots in this dataset.";
    std::string createButtonTitle = "Create";
    std::string cancelButtonTitle = "Cancel";
    std::string optionalTitlePlaceholder = "Optional title";
    std::string needsTwoNumericVariablesStatus = "The active dataset needs at least two numeric variables.";
    std::string noActiveDatasetStatus = "Open a LinkEDA plot first, or use ls_register_dataset() and ls_new_scatterplot() from R.";
};

std::set<CaseId> SelectCasesInBrush(const std::vector<ScatterplotCaseGeometry> &cases,
                                    const Rect &brush,
                                    double glyphPadding = 2.0);

std::optional<CaseId> NearestCaseToPoint(const std::vector<ScatterplotCaseGeometry> &cases,
                                         const Point &point,
                                         double maxDistance = 8.0,
                                         double glyphPadding = 2.0);

std::set<CaseId> SelectCasesForGesture(const std::vector<ScatterplotCaseGeometry> &cases,
                                       const Rect &brush,
                                       const Point &clickPoint,
                                       bool dragBrush,
                                       double maxClickDistance = 8.0,
                                       double glyphPadding = 2.0);

std::vector<ScatterplotPointDrawItem> BuildScatterplotPointDrawPlan(
    const std::vector<ScatterplotPointDrawInput> &points,
    const std::set<CaseId> &selectedRows,
    const std::map<CaseId, std::string> &rowColors,
    const std::map<CaseId, std::string> &rowLabels,
    const std::string &labelDisplayMode,
    bool skipNonCaseRows = false);

std::optional<DataViewport> ScatterplotViewportIncludingImputations(
    const DataFrameModel &df,
    const DataColumn &xColumn,
    const DataColumn &yColumn,
    const std::vector<ScatterplotPointValue> &points,
    const std::string &uncertaintyMode);

ScatterplotImputationGlyph ScatterplotImputationGlyphForPoint(
    const DataFrameModel &df,
    const DataColumn &xColumn,
    const DataColumn &yColumn,
    const ScatterplotPointValue &point,
    const DataViewport &viewport,
    const Rect &plotRect,
    const std::string &uncertaintyMode,
    double singleAxisMinimumSize = 9.0,
    double twoAxisMinimumSize = 14.0);

std::vector<ScatterplotPointDrawInput> BuildScatterplotPointDrawInputs(
    const std::vector<ScatterplotPointValue> &points,
    const DataViewport &viewport,
    const Rect &plotRect,
    const DataFrameModel *imputationDataFrame = nullptr,
    const DataColumn *xColumn = nullptr,
    const DataColumn *yColumn = nullptr,
    const std::string &uncertaintyMode = "central80",
    bool skipNonCaseRows = false);

std::vector<ScatterplotCaseGeometry> BuildScatterplotCaseGeometry(
    const std::vector<ScatterplotPointValue> &points,
    const DataViewport &viewport,
    const Rect &plotRect,
    const DataFrameModel *imputationDataFrame = nullptr,
    const DataColumn *xColumn = nullptr,
    const DataColumn *yColumn = nullptr,
    const std::string &uncertaintyMode = "central80",
    bool skipNonCaseRows = false);

SimpleLinearFitResult FitSimpleLinearModel(const std::vector<ScatterplotPointValue> &points,
                                           const std::set<CaseId> &selectedCases = {},
                                           const std::string &scope = "all",
                                           int totalRows = -1);
ScatterplotFittedLine FitScatterplotLineForViewport(
    const std::vector<ScatterplotPointValue> &points,
    const DataViewport &viewport);
std::vector<ScatterplotOverlayLineItem> BuildScatterplotOverlayLinePlan(
    const std::vector<ScatterplotPointValue> &points,
    const std::vector<ScatterplotOverlaySpec> &overlays,
    const std::set<CaseId> &selectedRows,
    const std::map<CaseId, std::string> &rowColors,
    const std::string &selectedColorName,
    const DataViewport &viewport);
ScatterplotRenderPlan BuildScatterplotRenderPlan(const ScatterplotRenderInput &input);
std::vector<ScatterplotSmoothCurveDrawItem> BuildSmoothCurveDrawItems(
    const std::vector<SmoothCurveData> &curves,
    const DataViewport &viewport,
    const Rect &plotRect);
bool ScatterplotHasOverlaySource(const std::vector<ScatterplotOverlaySpec> &overlays,
                                 const std::string &source);
ScatterplotVariableMenuState BuildScatterplotVariableMenuState(
    const std::vector<std::string> &numericVariables,
    const std::string &currentX,
    const std::string &currentY,
    const std::string &group);
std::vector<ScatterplotMenuOption> ScatterplotMouseModeMenuOptions(bool includePanZoom);
std::vector<ScatterplotMenuOption> ScatterplotSelectionModeMenuOptions();
std::vector<ScatterplotMenuOption> ScatterplotSelectionActionMenuOptions();
std::vector<ScatterplotMenuOption> ScatterplotBrushMenuOptions();
std::vector<ScatterplotMenuOption> ScatterplotViewMenuOptions();
ScatterplotMenuOption ScatterplotChooseLabelColumnOption(const std::string &labelColumn);
std::vector<ScatterplotMenuOption> ScatterplotLabelDisplayMenuOptions(
    const std::string &labelDisplayMode);
std::vector<ScatterplotMenuOption> ScatterplotImputationUncertaintyMenuOptions(
    const std::string &currentMode);
std::vector<ScatterplotMenuOption> ScatterplotPlotMenuOptions();
ScatterplotMenuOption ScatterplotClosePlotOption();
ScatterplotMenuState BuildScatterplotMenuState(
    const std::vector<std::string> &numericVariables,
    const std::string &currentX,
    const std::string &currentY,
    const std::string &group,
    const std::string &labelColumn,
    const std::string &labelDisplayMode,
    const std::vector<ScatterplotOverlaySpec> &overlays,
    bool hasMultipleImputation,
    const std::string &imputationUncertaintyMode,
    const std::vector<SmoothCurveData> &smoothCurves = {});
ScatterplotCreationDialogState BuildScatterplotCreationDialogState();
std::string ScatterplotDefaultTitle(const std::string &x,
                                    const std::string &y);
std::string ScatterplotNoValidXYStatus();
std::string ScatterplotAddIndependentVariableTitle();
std::string ScatterplotReplacePredictorTitle();
std::string ScatterplotSetActiveModelTitle();
std::string ScatterplotXFieldLabel();
std::string ScatterplotYFieldLabel();
std::string ScatterplotGroupFieldLabel();

void RebuildPointsForCurrentVariables(PlotModel &model);
inline void RebuildPointsForCurrentVariables(PlotModel *model) {
    if (model) RebuildPointsForCurrentVariables(*model);
}
void ComputeRanges(PlotModel &model);
inline void ComputeRanges(PlotModel *model) {
    if (model) ComputeRanges(*model);
}

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_SCATTERPLOT_MODEL_H
