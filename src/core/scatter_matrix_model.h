#ifndef RLISPSTAT_CORE_SCATTER_MATRIX_MODEL_H
#define RLISPSTAT_CORE_SCATTER_MATRIX_MODEL_H

#include "plot_geometry.h"
#include "selection_model.h"

#include <map>
#include <optional>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace rlispstat {
namespace core {

struct ScatterMatrixLayout {
    Rect plotRect;
    std::size_t variableCount = 0;
};

struct ScatterMatrixCell {
    std::size_t row = 0;
    std::size_t column = 0;
    Rect rect;
};

struct ScatterMatrixCaseGeometry {
    CaseId caseId = 0;
    std::size_t row = 0;
    std::size_t column = 0;
    Point point;
};

struct ScatterMatrixVariableSeries {
    std::string name;
    std::vector<double> values;
};

struct ScatterMatrixNumericRange {
    double minimum = 0.0;
    double maximum = 1.0;
};

struct ScatterMatrixVariableMenuState {
    std::string variablesTitle = "Variables";
    std::string addVariableTitle = "Add variable";
    std::string removeVariableTitle = "Remove variable";
    std::string noAvailableVariablesTitle = "No available variables";
    std::string keepAtLeastTwoVariablesTitle = "Keep at least two variables";
    std::string openVariablesWindowTitle = "Open variables window";
    std::vector<std::string> addVariables;
    std::vector<std::string> removeVariables;
    bool canRemoveVariables = false;
};

struct ScatterMatrixCreationDialogState {
    std::string title = "Scatterplot Matrix";
    std::string informativeText = "Choose numeric variables. The matrix shares row selection and point colors with the other plots in this dataset.";
    std::string createButtonTitle = "Create";
    std::string cancelButtonTitle = "Cancel";
    std::string optionalTitlePlaceholder = "Optional title";
    std::string hintText = "Tip: add or remove variables later from the plot context menu.";
    std::string noActiveDatasetStatus = "Open or import a dataset first.";
    std::string needsTwoNumericVariablesStatus = "The active dataset needs at least two numeric variables.";
    std::string selectAtLeastTwoNumericVariablesStatus = "Select at least two numeric variables.";
    std::string defaultTitle = "Scatterplot matrix";
};

struct ScatterMatrixTextRect {
    std::string text;
    Rect rect;
};

struct ScatterMatrixCellRenderItem {
    std::size_t row = 0;
    std::size_t column = 0;
    Rect rect;
    bool diagonal = false;
};

struct ScatterMatrixRenderPlan {
    ScatterMatrixLayout layout;
    Rect panelRect;
    bool showTitle = false;
    ScatterMatrixTextRect title;
    bool showEmptyMessage = false;
    ScatterMatrixTextRect emptyMessage;
    std::vector<ScatterMatrixCellRenderItem> cells;
    std::vector<ScatterMatrixTextRect> diagonalLabels;
};

struct ScatterMatrixPointDrawItem {
    CaseId caseId = 0;
    std::size_t row = 0;
    std::size_t column = 0;
    Point point;
    double radius = 2.7;
    bool selected = false;
    bool hasHalo = false;
    double haloRadius = 0.0;
    double haloAlpha = 0.0;
    std::string colorName;
    bool hasExplicitColor = false;
    double alpha = 0.72;
    bool showLabel = false;
    std::string label;
};

Rect ScatterMatrixPlotRectForBounds(double width, double height);
double ScatterMatrixPointRadius(std::size_t variableCount);
bool IsValidScatterMatrixLayout(const ScatterMatrixLayout &layout);
Rect ScatterMatrixCellRect(const ScatterMatrixLayout &layout,
                           std::size_t row,
                           std::size_t column);
Rect ScatterMatrixInnerCellRect(const ScatterMatrixLayout &layout,
                                std::size_t row,
                                std::size_t column,
                                double padding);
std::optional<ScatterMatrixCell> ScatterMatrixCellAtPoint(const ScatterMatrixLayout &layout,
                                                          const Point &point);
ScatterMatrixNumericRange ScatterMatrixRangeForValues(const std::vector<double> &values,
                                                       double paddingFraction = 0.05);
std::vector<CaseId> ScatterMatrixVisibleRows(const std::vector<ScatterMatrixVariableSeries> &variables);
std::vector<ScatterMatrixCaseGeometry> BuildScatterMatrixCaseGeometry(
    const std::vector<ScatterMatrixVariableSeries> &variables,
    const std::vector<CaseId> &visibleRows,
    const ScatterMatrixLayout &layout,
    double innerPadding = 5.0);
std::vector<std::string> ScatterMatrixVariablesForInputs(
    const std::vector<std::string> &requestedVariables,
    const std::string &xLabel,
    const std::string &yLabel,
    const std::vector<std::string> &availableVariables);
std::vector<std::string> ScatterMatrixVariablesAvailableToAdd(
    const std::vector<std::string> &currentVariables,
    const std::vector<std::string> &availableVariables);
std::vector<std::string> ScatterMatrixVariablesAfterAdd(
    const std::vector<std::string> &currentVariables,
    const std::string &variable,
    const std::vector<std::string> &availableVariables);
std::vector<std::string> ScatterMatrixVariablesAfterRemove(
    const std::vector<std::string> &currentVariables,
    const std::string &variable,
    std::size_t minimumVariables = 2);
std::vector<std::string> ScatterMatrixVariablesAvailableForReplacement(
    const std::vector<std::string> &currentVariables,
    std::size_t variableIndex,
    const std::vector<std::string> &availableVariables);
std::vector<std::string> ScatterMatrixVariablesAfterReplacement(
    const std::vector<std::string> &currentVariables,
    std::size_t variableIndex,
    const std::string &replacementVariable,
    const std::vector<std::string> &availableVariables);
ScatterMatrixVariableMenuState BuildScatterMatrixVariableMenuState(
    const std::vector<std::string> &currentVariables,
    const std::vector<std::string> &availableVariables,
    std::size_t minimumVariables = 2);
ScatterMatrixCreationDialogState BuildScatterMatrixCreationDialogState();
ScatterMatrixRenderPlan BuildScatterMatrixRenderPlan(
    const std::vector<std::string> &variables,
    const std::string &title,
    const ScatterMatrixLayout &layout);
std::vector<ScatterMatrixPointDrawItem> BuildScatterMatrixPointDrawPlan(
    const std::vector<ScatterMatrixCaseGeometry> &geometry,
    const std::set<CaseId> &selectedRows,
    const std::map<CaseId, std::string> &rowColors,
    const std::map<CaseId, std::string> &rowLabels,
    const std::string &labelDisplayMode,
    std::size_t variableCount);

std::set<CaseId> SelectScatterMatrixCasesForGesture(
    const std::vector<ScatterMatrixCaseGeometry> &cases,
    const ScatterMatrixLayout &layout,
    const Rect &brush,
    const Point &clickPoint,
    bool dragBrush,
    double maxClickDistance = 8.0);

std::vector<std::string> ScatterMatrixVariablesForModel(const PlotModel &model);
inline std::vector<std::string> ScatterMatrixVariablesForModel(const PlotModel *model) {
    return model ? ScatterMatrixVariablesForModel(*model) : std::vector<std::string>();
}
std::vector<ScatterMatrixVariableSeries> ScatterMatrixVariableSeriesForModel(
    PlotModel &model,
    const std::vector<std::string> &variables);
inline std::vector<ScatterMatrixVariableSeries> ScatterMatrixVariableSeriesForModel(
    PlotModel *model,
    const std::vector<std::string> &variables) {
    return model ? ScatterMatrixVariableSeriesForModel(*model, variables) : std::vector<ScatterMatrixVariableSeries>();
}
std::vector<CaseId> ScatterMatrixVisibleCaseIdsForModel(const PlotModel &model);
inline std::vector<CaseId> ScatterMatrixVisibleCaseIdsForModel(const PlotModel *model) {
    return model ? ScatterMatrixVisibleCaseIdsForModel(*model) : std::vector<CaseId>();
}
void NormalizeScatterMatrixVariables(PlotModel &model);
inline void NormalizeScatterMatrixVariables(PlotModel *model) {
    if (model) NormalizeScatterMatrixVariables(*model);
}
void RebuildScatterMatrixPoints(PlotModel &model);
inline void RebuildScatterMatrixPoints(PlotModel *modelPtr) {
    if (modelPtr) RebuildScatterMatrixPoints(*modelPtr);
}

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_SCATTER_MATRIX_MODEL_H
