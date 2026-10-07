#include "scatter_matrix_model.h"
#include "string_utils.h"

#include <algorithm>
#include <cmath>

namespace rlispstat {
namespace core {

std::string ScatterMatrixFitPanelId(const PlotModel &model,
                                    std::size_t row, std::size_t column)
{
    return "matrix:" + std::to_string(model.scatterMatrixFitGeneration) +
        ":" + std::to_string(row) + ":" + std::to_string(column);
}

void InvalidateScatterMatrixFits(PlotModel &model)
{
    if (model.kind != "scatter_matrix") return;
    ++model.scatterMatrixFitGeneration;
    model.scatterMatrixFitsPending = !model.overlays.empty();
    model.trellisPanelSmoothCurves.clear();
    model.smoothCurves.erase(
        std::remove_if(model.smoothCurves.begin(), model.smoothCurves.end(),
            [](const SmoothCurveData &curve) { return curve.fitMethod == "lm"; }),
        model.smoothCurves.end());
}

namespace {

bool ContainsString(const std::vector<std::string> &values, const std::string &value)
{
    return std::find(values.begin(), values.end(), value) != values.end();
}

void AppendIfAvailableAndUnique(std::vector<std::string> &out,
                                const std::string &value,
                                const std::vector<std::string> &availableVariables)
{
    if (!value.empty() &&
        ContainsString(availableVariables, value) &&
        !ContainsString(out, value)) {
        out.push_back(value);
    }
}

} // namespace

Rect ScatterMatrixPlotRectForBounds(double width, double height)
{
    return Rect{72.0, 46.0, std::max(10.0, width - 96.0), std::max(10.0, height - 86.0)};
}

double ScatterMatrixPointRadius(std::size_t variableCount)
{
    return variableCount > 5 ? 2.0 : 2.7;
}

bool IsValidScatterMatrixLayout(const ScatterMatrixLayout &layout)
{
    return layout.variableCount >= 2 && IsValidRect(layout.plotRect);
}

Rect ScatterMatrixCellRect(const ScatterMatrixLayout &layout,
                           std::size_t row,
                           std::size_t column)
{
    if (!IsValidScatterMatrixLayout(layout) ||
        row >= layout.variableCount ||
        column >= layout.variableCount) {
        return {};
    }
    const double cellWidth = layout.plotRect.width / static_cast<double>(layout.variableCount);
    const double cellHeight = layout.plotRect.height / static_cast<double>(layout.variableCount);
    return {
        layout.plotRect.x + static_cast<double>(column) * cellWidth,
        layout.plotRect.y + static_cast<double>(row) * cellHeight,
        cellWidth,
        cellHeight
    };
}

Rect ScatterMatrixInnerCellRect(const ScatterMatrixLayout &layout,
                                std::size_t row,
                                std::size_t column,
                                double padding)
{
    Rect cell = ScatterMatrixCellRect(layout, row, column);
    if (!IsValidRect(cell)) {
        return {};
    }
    const double inset = std::max(0.0, padding);
    const double width = std::max(0.0, cell.width - inset * 2.0);
    const double height = std::max(0.0, cell.height - inset * 2.0);
    return {
        cell.x + inset,
        cell.y + inset,
        width,
        height
    };
}

std::optional<ScatterMatrixCell> ScatterMatrixCellAtPoint(const ScatterMatrixLayout &layout,
                                                          const Point &point)
{
    if (!IsValidScatterMatrixLayout(layout) ||
        !std::isfinite(point.x) ||
        !std::isfinite(point.y) ||
        !PointInRect(point, layout.plotRect)) {
        return std::nullopt;
    }
    const double cellWidth = layout.plotRect.width / static_cast<double>(layout.variableCount);
    const double cellHeight = layout.plotRect.height / static_cast<double>(layout.variableCount);
    std::size_t column = static_cast<std::size_t>(std::floor((point.x - layout.plotRect.x) / cellWidth));
    std::size_t row = static_cast<std::size_t>(std::floor((point.y - layout.plotRect.y) / cellHeight));
    column = std::min(column, layout.variableCount - 1);
    row = std::min(row, layout.variableCount - 1);
    return ScatterMatrixCell{row, column, ScatterMatrixCellRect(layout, row, column)};
}

ScatterMatrixNumericRange ScatterMatrixRangeForValues(const std::vector<double> &values,
                                                       double paddingFraction)
{
    bool found = false;
    double lo = 0.0;
    double hi = 1.0;
    for (double value : values) {
        if (!std::isfinite(value)) {
            continue;
        }
        if (!found) {
            lo = hi = value;
            found = true;
        } else {
            lo = std::min(lo, value);
            hi = std::max(hi, value);
        }
    }
    if (!found) {
        return {0.0, 1.0};
    }
    if (lo == hi) {
        lo -= 0.5;
        hi += 0.5;
    }
    double pad = (hi - lo) * std::max(0.0, paddingFraction);
    return {lo - pad, hi + pad};
}

std::vector<CaseId> ScatterMatrixVisibleRows(const std::vector<ScatterMatrixVariableSeries> &variables)
{
    std::vector<CaseId> rows;
    if (variables.size() < 2) {
        return rows;
    }

    std::size_t n = 0;
    for (const ScatterMatrixVariableSeries &variable : variables) {
        n = std::max(n, variable.values.size());
    }
    rows.reserve(n);
    for (std::size_t row = 0; row < n; ++row) {
        int finiteCount = 0;
        for (const ScatterMatrixVariableSeries &variable : variables) {
            if (row < variable.values.size() && std::isfinite(variable.values[row])) {
                ++finiteCount;
            }
        }
        if (finiteCount >= 2) {
            rows.push_back(static_cast<CaseId>(row + 1));
        }
    }
    return rows;
}

std::vector<ScatterMatrixCaseGeometry> BuildScatterMatrixCaseGeometry(
    const std::vector<ScatterMatrixVariableSeries> &variables,
    const std::vector<CaseId> &visibleRows,
    const ScatterMatrixLayout &layout,
    double innerPadding)
{
    std::vector<ScatterMatrixCaseGeometry> geometry;
    if (!IsValidScatterMatrixLayout(layout) || variables.size() < layout.variableCount) {
        return geometry;
    }

    std::vector<ScatterMatrixNumericRange> ranges;
    ranges.reserve(layout.variableCount);
    for (std::size_t i = 0; i < layout.variableCount; ++i) {
        ranges.push_back(ScatterMatrixRangeForValues(variables[i].values));
    }

    for (std::size_t row = 0; row < layout.variableCount; ++row) {
        for (std::size_t column = 0; column < layout.variableCount; ++column) {
            if (row == column) continue;
            const ScatterMatrixVariableSeries &xVar = variables[column];
            const ScatterMatrixVariableSeries &yVar = variables[row];
            const ScatterMatrixNumericRange &xRange = ranges[column];
            const ScatterMatrixNumericRange &yRange = ranges[row];
            if (xRange.minimum == xRange.maximum || yRange.minimum == yRange.maximum) continue;
            Rect innerCell = ScatterMatrixInnerCellRect(layout, row, column, innerPadding);
            if (!IsValidRect(innerCell)) continue;
            for (CaseId caseId : visibleRows) {
                if (caseId <= 0) continue;
                const std::size_t index = static_cast<std::size_t>(caseId) - 1;
                if (index >= xVar.values.size() || index >= yVar.values.size()) continue;
                const double x = xVar.values[index];
                const double y = yVar.values[index];
                if (!std::isfinite(x) || !std::isfinite(y)) continue;
                Point screen = DataToScreen(
                    Point{x, y},
                    DataViewport{xRange.minimum, xRange.maximum, yRange.minimum, yRange.maximum},
                    innerCell,
                    true);
                if (std::isfinite(screen.x) && std::isfinite(screen.y)) {
                    geometry.push_back(ScatterMatrixCaseGeometry{
                        caseId,
                        row,
                        column,
                        screen
                    });
                }
            }
        }
    }
    return geometry;
}

std::vector<std::string> ScatterMatrixVariablesForInputs(
    const std::vector<std::string> &requestedVariables,
    const std::string &xLabel,
    const std::string &yLabel,
    const std::vector<std::string> &availableVariables)
{
    std::vector<std::string> out;

    for (const std::string &name : requestedVariables) {
        AppendIfAvailableAndUnique(out, name, availableVariables);
    }

    if (out.size() < 2) {
        AppendIfAvailableAndUnique(out, xLabel, availableVariables);
        AppendIfAvailableAndUnique(out, yLabel, availableVariables);
    }

    for (const std::string &name : availableVariables) {
        if (out.size() >= 2 && !requestedVariables.empty()) {
            break;
        }
        if (!ContainsString(out, name)) {
            out.push_back(name);
        }
    }

    return out;
}

std::vector<std::string> ScatterMatrixVariablesAvailableToAdd(
    const std::vector<std::string> &currentVariables,
    const std::vector<std::string> &availableVariables)
{
    std::vector<std::string> out;
    for (const std::string &name : availableVariables) {
        if (!ContainsString(currentVariables, name) && !ContainsString(out, name)) {
            out.push_back(name);
        }
    }
    return out;
}

std::vector<std::string> ScatterMatrixVariablesAfterAdd(
    const std::vector<std::string> &currentVariables,
    const std::string &variable,
    const std::vector<std::string> &availableVariables)
{
    std::vector<std::string> out = currentVariables;
    AppendIfAvailableAndUnique(out, variable, availableVariables);
    return out;
}

std::vector<std::string> ScatterMatrixVariablesAfterRemove(
    const std::vector<std::string> &currentVariables,
    const std::string &variable,
    std::size_t minimumVariables)
{
    if (currentVariables.size() <= minimumVariables || variable.empty()) {
        return currentVariables;
    }

    std::vector<std::string> out;
    out.reserve(currentVariables.size());
    for (const std::string &name : currentVariables) {
        if (name != variable) {
            out.push_back(name);
        }
    }
    if (out.size() < minimumVariables) {
        return currentVariables;
    }
    return out;
}

std::vector<std::string> ScatterMatrixVariablesAvailableForReplacement(
    const std::vector<std::string> &currentVariables,
    std::size_t variableIndex,
    const std::vector<std::string> &availableVariables)
{
    std::vector<std::string> out;
    if (variableIndex >= currentVariables.size()) {
        return out;
    }
    for (const std::string &name : availableVariables) {
        if (!name.empty() &&
            !ContainsString(currentVariables, name) &&
            !ContainsString(out, name)) {
            out.push_back(name);
        }
    }
    return out;
}

std::vector<std::string> ScatterMatrixVariablesAfterReplacement(
    const std::vector<std::string> &currentVariables,
    std::size_t variableIndex,
    const std::string &replacementVariable,
    const std::vector<std::string> &availableVariables)
{
    if (variableIndex >= currentVariables.size() ||
        replacementVariable.empty() ||
        !ContainsString(availableVariables, replacementVariable) ||
        ContainsString(currentVariables, replacementVariable)) {
        return currentVariables;
    }
    std::vector<std::string> out = currentVariables;
    out[variableIndex] = replacementVariable;
    return out;
}

ScatterMatrixVariableMenuState BuildScatterMatrixVariableMenuState(
    const std::vector<std::string> &currentVariables,
    const std::vector<std::string> &availableVariables,
    std::size_t minimumVariables)
{
    ScatterMatrixVariableMenuState state;
    state.addVariables = ScatterMatrixVariablesAvailableToAdd(currentVariables, availableVariables);
    if (currentVariables.size() > minimumVariables) {
        state.canRemoveVariables = true;
        state.removeVariables = currentVariables;
    }
    return state;
}

ScatterMatrixCreationDialogState BuildScatterMatrixCreationDialogState()
{
    return ScatterMatrixCreationDialogState{};
}

ScatterMatrixRenderPlan BuildScatterMatrixRenderPlan(
    const std::vector<std::string> &variables,
    const std::string &title,
    const ScatterMatrixLayout &layout)
{
    ScatterMatrixRenderPlan plan;
    plan.layout = layout;
    plan.panelRect = layout.plotRect;
    if (!title.empty()) {
        plan.showTitle = true;
        plan.title = ScatterMatrixTextRect{
            title,
            Rect{layout.plotRect.x, 12.0, layout.plotRect.width, 22.0}
        };
    }
    if (variables.size() < 2 || !IsValidScatterMatrixLayout(layout)) {
        plan.showEmptyMessage = true;
        plan.emptyMessage = ScatterMatrixTextRect{
            "Select at least two numeric variables.",
            Rect{layout.plotRect.x + 18.0, layout.plotRect.y + 18.0, 360.0, 20.0}
        };
        return plan;
    }

    const std::size_t n = variables.size();
    plan.cells.reserve(n * n);
    plan.diagonalLabels.reserve(n);
    for (std::size_t row = 0; row < n; ++row) {
        for (std::size_t column = 0; column < n; ++column) {
            Rect cell = ScatterMatrixCellRect(layout, row, column);
            plan.cells.push_back(ScatterMatrixCellRenderItem{
                row,
                column,
                cell,
                row == column
            });
            if (row == column) {
                plan.diagonalLabels.push_back(ScatterMatrixTextRect{
                    variables[row],
                    Rect{
                        cell.x + 5.0,
                        cell.y + std::max(4.0, cell.height / 2.0 - 10.0),
                        std::max(0.0, cell.width - 10.0),
                        std::max(0.0, cell.height - 2.0 * std::max(4.0, cell.height / 2.0 - 10.0))
                    }
                });
            }
        }
    }
    return plan;
}

std::vector<ScatterMatrixPointDrawItem> BuildScatterMatrixPointDrawPlan(
    const std::vector<ScatterMatrixCaseGeometry> &geometry,
    const std::set<CaseId> &selectedRows,
    const std::map<CaseId, std::string> &rowColors,
    const std::map<CaseId, std::string> &rowLabels,
    const std::string &labelDisplayMode,
    std::size_t variableCount)
{
    std::vector<ScatterMatrixPointDrawItem> plan;
    plan.reserve(geometry.size());

    const bool hasSelection = !selectedRows.empty();
    const bool showAllLabels = labelDisplayMode == "all";
    const bool showSelectedLabels = labelDisplayMode == "selected";
    const double radius = ScatterMatrixPointRadius(variableCount);

    for (int pass = 0; pass < 2; ++pass) {
        for (const ScatterMatrixCaseGeometry &entry : geometry) {
            if (entry.caseId <= 0) {
                continue;
            }
            const bool selected = selectedRows.find(entry.caseId) != selectedRows.end();
            if ((pass == 0 && selected) || (pass == 1 && !selected)) {
                continue;
            }

            ScatterMatrixPointDrawItem item;
            item.caseId = entry.caseId;
            item.row = entry.row;
            item.column = entry.column;
            item.point = entry.point;
            item.radius = radius;
            item.selected = selected;

            auto colorIt = rowColors.find(entry.caseId);
            const bool hasRowColor = colorIt != rowColors.end() && !colorIt->second.empty();
            item.hasExplicitColor = hasRowColor;
            if (selected) {
                item.colorName = hasRowColor ? colorIt->second : "black";
                item.alpha = 1.0;
                item.hasHalo = true;
                item.haloRadius = radius + 2.8;
                item.haloAlpha = 0.24;
            } else if (hasRowColor) {
                item.colorName = colorIt->second;
                item.alpha = hasSelection ? 0.35 : 0.78;
            } else {
                item.alpha = hasSelection ? 0.22 : 0.72;
            }

            if (showAllLabels || (showSelectedLabels && selected)) {
                auto labelIt = rowLabels.find(entry.caseId);
                if (labelIt != rowLabels.end() && !labelIt->second.empty()) {
                    item.showLabel = true;
                    item.label = labelIt->second;
                }
            }

            plan.push_back(item);
        }
    }

    return plan;
}

std::set<CaseId> SelectScatterMatrixCasesForGesture(
    const std::vector<ScatterMatrixCaseGeometry> &cases,
    const ScatterMatrixLayout &layout,
    const Rect &brush,
    const Point &clickPoint,
    bool dragBrush,
    double maxClickDistance)
{
    std::set<CaseId> selected;
    if (!IsValidScatterMatrixLayout(layout)) {
        return selected;
    }
    if (dragBrush) {
        if (!IsValidRect(brush)) {
            return selected;
        }
        for (const ScatterMatrixCaseGeometry &entry : cases) {
            if (entry.caseId > 0 && PointInRect(entry.point, brush)) {
                selected.insert(entry.caseId);
            }
        }
        return selected;
    }

    std::optional<ScatterMatrixCell> cell = ScatterMatrixCellAtPoint(layout, clickPoint);
    if (!cell.has_value() || cell->row == cell->column || maxClickDistance < 0.0) {
        return selected;
    }

    double bestDistanceSquared = maxClickDistance * maxClickDistance;
    CaseId bestCase = 0;
    for (const ScatterMatrixCaseGeometry &entry : cases) {
        if (entry.caseId <= 0 || entry.row != cell->row || entry.column != cell->column) {
            continue;
        }
        const double distanceSquared = DistanceSquared(entry.point, clickPoint);
        if (std::isfinite(distanceSquared) && distanceSquared < bestDistanceSquared) {
            bestDistanceSquared = distanceSquared;
            bestCase = entry.caseId;
        }
    }
    if (bestCase > 0) {
        selected.insert(bestCase);
    }
    return selected;
}

std::vector<std::string> ScatterMatrixVariablesForModel(const PlotModel &model)
{
    std::vector<std::string> available;
    for (const NumericVariable &var : model.variables) {
        available.push_back(var.name);
    }
    return ScatterMatrixVariablesForInputs(
        model.scatterMatrixVariables,
        model.xLabel,
        model.yLabel,
        available);
}

std::vector<ScatterMatrixVariableSeries> ScatterMatrixVariableSeriesForModel(
    PlotModel &model,
    const std::vector<std::string> &variables)
{
    std::vector<ScatterMatrixVariableSeries> series;
    for (const std::string &name : variables) {
        NumericVariable *var = FindNumericVariable(model, name);
        if (var) {
            series.push_back(ScatterMatrixVariableSeries{var->name, var->values});
        }
    }
    return series;
}

std::vector<CaseId> ScatterMatrixVisibleCaseIdsForModel(const PlotModel &model)
{
    std::vector<CaseId> rows;
    rows.reserve(model.points.size());
    for (const DataPoint &point : model.points) {
        if (point.row > 0) rows.push_back(point.row);
    }
    return rows;
}

void NormalizeScatterMatrixVariables(PlotModel &model)
{
    model.scatterMatrixVariables = ScatterMatrixVariablesForModel(model);
    if (!model.scatterMatrixVariables.empty()) {
        model.xLabel = JoinStrings(model.scatterMatrixVariables, ", ");
        model.yLabel.clear();
    }
    if (model.title.empty()) {
        model.title = "Scatterplot matrix";
    }
}

void RebuildScatterMatrixPoints(PlotModel &model)
{
    InvalidateScatterMatrixFits(model);
    NormalizeScatterMatrixVariables(model);
    std::vector<ScatterMatrixVariableSeries> vars =
        ScatterMatrixVariableSeriesForModel(model, model.scatterMatrixVariables);
    model.points.clear();
    std::vector<CaseId> visibleRows = ScatterMatrixVisibleRows(vars);
    model.points.reserve(visibleRows.size());
    for (CaseId row : visibleRows) {
        model.points.push_back(DataPoint{0.0, 0.0, (int)row});
    }
}

} // namespace core
} // namespace rlispstat
