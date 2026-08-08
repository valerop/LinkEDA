#include "scatterplot_model.h"

#include "command_model.h"

#include <algorithm>
#include <cmath>

namespace rlispstat {
namespace core {

std::set<CaseId> SelectCasesInBrush(const std::vector<ScatterplotCaseGeometry> &cases,
                                    const Rect &brush,
                                    double glyphPadding)
{
    std::set<CaseId> selected;
    if (!IsValidRect(brush)) {
        return selected;
    }
    for (const ScatterplotCaseGeometry &entry : cases) {
        if (entry.caseId <= 0) {
            continue;
        }
        if (entry.hasGlyphRect) {
            if (RectIntersects(ExpandRect(entry.glyphRect, glyphPadding), brush)) {
                selected.insert(entry.caseId);
            }
        } else if (PointInRect(entry.point, brush)) {
            selected.insert(entry.caseId);
        }
    }
    return selected;
}

std::optional<CaseId> NearestCaseToPoint(const std::vector<ScatterplotCaseGeometry> &cases,
                                         const Point &point,
                                         double maxDistance,
                                         double glyphPadding)
{
    if (!std::isfinite(point.x) || !std::isfinite(point.y) || maxDistance < 0.0) {
        return std::nullopt;
    }
    double bestDistanceSquared = maxDistance * maxDistance;
    CaseId bestCase = 0;
    for (const ScatterplotCaseGeometry &entry : cases) {
        if (entry.caseId <= 0) {
            continue;
        }
        double distanceSquared = NAN;
        if (entry.hasGlyphRect) {
            distanceSquared = DistanceSquaredToRect(point, ExpandRect(entry.glyphRect, glyphPadding));
        } else {
            distanceSquared = DistanceSquared(point, entry.point);
        }
        if (std::isfinite(distanceSquared) && distanceSquared < bestDistanceSquared) {
            bestDistanceSquared = distanceSquared;
            bestCase = entry.caseId;
        }
    }
    if (bestCase <= 0) {
        return std::nullopt;
    }
    return bestCase;
}

std::set<CaseId> SelectCasesForGesture(const std::vector<ScatterplotCaseGeometry> &cases,
                                       const Rect &brush,
                                       const Point &clickPoint,
                                       bool dragBrush,
                                       double maxClickDistance,
                                       double glyphPadding)
{
    if (dragBrush) {
        return SelectCasesInBrush(cases, brush, glyphPadding);
    }
    std::set<CaseId> selected;
    std::optional<CaseId> nearest = NearestCaseToPoint(cases, clickPoint, maxClickDistance, glyphPadding);
    if (nearest.has_value()) {
        selected.insert(*nearest);
    }
    return selected;
}

std::vector<ScatterplotPointDrawItem> BuildScatterplotPointDrawPlan(
    const std::vector<ScatterplotPointDrawInput> &points,
    const std::set<CaseId> &selectedRows,
    const std::map<CaseId, std::string> &rowColors,
    const std::map<CaseId, std::string> &rowLabels,
    const std::string &labelDisplayMode,
    bool skipNonCaseRows)
{
    std::vector<ScatterplotPointDrawItem> plan;
    plan.reserve(points.size());

    const bool hasSelection = !selectedRows.empty();
    const bool showAllLabels = labelDisplayMode == "all";
    const bool showSelectedLabels = labelDisplayMode == "selected";

    for (const ScatterplotPointDrawInput &input : points) {
        if (skipNonCaseRows && input.caseId <= 0) {
            continue;
        }

        const bool isCase = input.caseId > 0;
        const bool selected = isCase && selectedRows.find(input.caseId) != selectedRows.end();
        auto colorIt = isCase ? rowColors.find(input.caseId) : rowColors.end();
        const bool hasRowColor = colorIt != rowColors.end() && !colorIt->second.empty();

        ScatterplotPointDrawItem item;
        item.caseId = input.caseId;
        item.point = input.point;
        item.selected = selected;
        item.hasImputationGlyph = input.hasImputationGlyph && input.imputationGlyph.hasImputation;
        item.imputationGlyph = input.imputationGlyph;
        item.hasExplicitColor = hasRowColor;
        item.labelAnchor = item.hasImputationGlyph
            ? Point{input.imputationGlyph.rect.x + input.imputationGlyph.rect.width / 2.0,
                    input.imputationGlyph.rect.y + input.imputationGlyph.rect.height / 2.0}
            : input.point;

        if (item.hasImputationGlyph) {
            item.colorName = hasRowColor ? colorIt->second : "black";
        } else if (selected) {
            item.colorName = hasRowColor ? colorIt->second : "black";
            item.fillAlpha = 1.0;
            item.strokeAlpha = 0.90;
            item.radius = 3.6;
            item.strokeWidth = 0.8;
            item.hasHalo = true;
            item.haloRadius = 6.0;
            item.haloAlpha = 0.24;
        } else if (hasRowColor) {
            item.colorName = colorIt->second;
            item.fillAlpha = hasSelection ? 0.35 : 0.78;
            item.strokeAlpha = hasSelection ? 0.25 : 0.55;
            item.radius = 3.1;
            item.strokeWidth = 0.6;
        } else {
            item.fillAlpha = hasSelection ? 0.22 : 0.72;
            item.strokeAlpha = hasSelection ? 0.18 : 0.42;
            item.radius = 3.0;
            item.strokeWidth = 0.6;
        }

        if (isCase && (showAllLabels || (showSelectedLabels && selected))) {
            auto labelIt = rowLabels.find(input.caseId);
            if (labelIt != rowLabels.end() && !labelIt->second.empty()) {
                item.showLabel = true;
                item.label = labelIt->second;
            }
        }

        plan.push_back(item);
    }

    return plan;
}

std::optional<DataViewport> ScatterplotViewportIncludingImputations(
    const DataFrameModel &df,
    const DataColumn &xColumn,
    const DataColumn &yColumn,
    const std::vector<ScatterplotPointValue> &points,
    const std::string &uncertaintyMode)
{
    if (!DataFrameShowsAllImputations(df) || points.empty()) {
        return std::nullopt;
    }

    NumericImputationRange xRange;
    NumericImputationRange yRange;
    for (const ScatterplotPointValue &point : points) {
        if (point.caseId <= 0) {
            NumericRangeInclude(xRange, point.x);
            NumericRangeInclude(yRange, point.y);
            continue;
        }

        const std::size_t row = static_cast<std::size_t>(point.caseId - 1);
        NumericImputationRange xi = NumericImputationRangeForCell(df, xColumn, row, uncertaintyMode);
        NumericImputationRange yi = NumericImputationRangeForCell(df, yColumn, row, uncertaintyMode);
        if (xi.any) {
            NumericRangeInclude(xRange, xi.min);
            NumericRangeInclude(xRange, xi.max);
        } else {
            NumericRangeInclude(xRange, point.x);
        }
        if (yi.any) {
            NumericRangeInclude(yRange, yi.min);
            NumericRangeInclude(yRange, yi.max);
        } else {
            NumericRangeInclude(yRange, point.y);
        }
    }

    if (!xRange.any || !yRange.any) {
        return std::nullopt;
    }
    if (xRange.min == xRange.max) {
        xRange.min -= 0.5;
        xRange.max += 0.5;
    }
    if (yRange.min == yRange.max) {
        yRange.min -= 0.5;
        yRange.max += 0.5;
    }

    const double xPad = (xRange.max - xRange.min) * 0.05;
    const double yPad = (yRange.max - yRange.min) * 0.05;
    return DataViewport{
        xRange.min - xPad,
        xRange.max + xPad,
        yRange.min - yPad,
        yRange.max + yPad
    };
}

ScatterplotImputationGlyph ScatterplotImputationGlyphForPoint(
    const DataFrameModel &df,
    const DataColumn &xColumn,
    const DataColumn &yColumn,
    const ScatterplotPointValue &point,
    const DataViewport &viewport,
    const Rect &plotRect,
    const std::string &uncertaintyMode,
    double singleAxisMinimumSize,
    double twoAxisMinimumSize)
{
    ScatterplotImputationGlyph glyph;
    if (point.caseId <= 0 ||
        !DataFrameShowsAllImputations(df) ||
        !IsValidViewport(viewport) ||
        !IsValidRect(plotRect)) {
        return glyph;
    }

    const std::size_t row = static_cast<std::size_t>(point.caseId - 1);
    NumericImputationRange xi = NumericImputationRangeForCell(df, xColumn, row, uncertaintyMode);
    NumericImputationRange yi = NumericImputationRangeForCell(df, yColumn, row, uncertaintyMode);
    if (!xi.any && !yi.any) {
        return glyph;
    }

    glyph.axisMask = (xi.any ? 1 : 0) | (yi.any ? 2 : 0);
    const double centerX = xi.any ? xi.center : point.x;
    const double centerY = yi.any ? yi.center : point.y;
    if (!std::isfinite(centerX) || !std::isfinite(centerY)) {
        glyph.axisMask = 0;
        return glyph;
    }

    Point center = DataToScreen({centerX, centerY}, viewport, plotRect, true);
    if (!std::isfinite(center.x) || !std::isfinite(center.y)) {
        glyph.axisMask = 0;
        return glyph;
    }

    const bool twoAxisGlyph = xi.any && yi.any;
    const double minimumSize = std::max(0.0, twoAxisGlyph ? twoAxisMinimumSize : singleAxisMinimumSize);
    double width = minimumSize;
    double height = minimumSize;
    if (xi.any) {
        Point lo = DataToScreen({xi.min, centerY}, viewport, plotRect, true);
        Point hi = DataToScreen({xi.max, centerY}, viewport, plotRect, true);
        if (std::isfinite(lo.x) && std::isfinite(hi.x)) {
            width = std::max(std::fabs(hi.x - lo.x), minimumSize);
        }
    }
    if (yi.any) {
        Point lo = DataToScreen({centerX, yi.min}, viewport, plotRect, true);
        Point hi = DataToScreen({centerX, yi.max}, viewport, plotRect, true);
        if (std::isfinite(lo.y) && std::isfinite(hi.y)) {
            height = std::max(std::fabs(hi.y - lo.y), minimumSize);
        }
    }

    glyph.hasImputation = true;
    glyph.rect = {center.x - width / 2.0, center.y - height / 2.0, width, height};
    return glyph;
}

std::vector<ScatterplotPointDrawInput> BuildScatterplotPointDrawInputs(
    const std::vector<ScatterplotPointValue> &points,
    const DataViewport &viewport,
    const Rect &plotRect,
    const DataFrameModel *imputationDataFrame,
    const DataColumn *xColumn,
    const DataColumn *yColumn,
    const std::string &uncertaintyMode,
    bool skipNonCaseRows)
{
    std::vector<ScatterplotPointDrawInput> inputs;
    inputs.reserve(points.size());
    const bool useImputationGlyphs =
        imputationDataFrame && xColumn && yColumn && DataFrameShowsAllImputations(*imputationDataFrame);

    for (const ScatterplotPointValue &point : points) {
        if (skipNonCaseRows && point.caseId <= 0) {
            continue;
        }

        ScatterplotPointDrawInput input;
        input.caseId = point.caseId;
        input.point = DataToScreen({point.x, point.y}, viewport, plotRect, true);
        if (useImputationGlyphs) {
            ScatterplotImputationGlyph glyph =
                ScatterplotImputationGlyphForPoint(*imputationDataFrame,
                                                   *xColumn,
                                                   *yColumn,
                                                   point,
                                                   viewport,
                                                   plotRect,
                                                   uncertaintyMode);
            if (glyph.hasImputation) {
                input.hasImputationGlyph = true;
                input.imputationGlyph = glyph;
            }
        }
        inputs.push_back(input);
    }

    return inputs;
}

std::vector<ScatterplotCaseGeometry> BuildScatterplotCaseGeometry(
    const std::vector<ScatterplotPointValue> &points,
    const DataViewport &viewport,
    const Rect &plotRect,
    const DataFrameModel *imputationDataFrame,
    const DataColumn *xColumn,
    const DataColumn *yColumn,
    const std::string &uncertaintyMode,
    bool skipNonCaseRows)
{
    std::vector<ScatterplotPointDrawInput> inputs =
        BuildScatterplotPointDrawInputs(points,
                                        viewport,
                                        plotRect,
                                        imputationDataFrame,
                                        xColumn,
                                        yColumn,
                                        uncertaintyMode,
                                        skipNonCaseRows);
    std::vector<ScatterplotCaseGeometry> geometry;
    geometry.reserve(inputs.size());
    for (const ScatterplotPointDrawInput &input : inputs) {
        ScatterplotCaseGeometry entry;
        entry.caseId = input.caseId;
        entry.point = input.point;
        entry.hasGlyphRect = input.hasImputationGlyph && input.imputationGlyph.hasImputation;
        if (entry.hasGlyphRect) {
            entry.glyphRect = input.imputationGlyph.rect;
        }
        geometry.push_back(entry);
    }
    return geometry;
}

SimpleLinearFitResult FitSimpleLinearModel(const std::vector<ScatterplotPointValue> &points,
                                           const std::set<CaseId> &selectedCases,
                                           const std::string &scope,
                                           int totalRows)
{
    SimpleLinearFitResult fit;
    std::vector<ScatterplotPointValue> rows;
    rows.reserve(points.size());
    for (const ScatterplotPointValue &point : points) {
        if (!std::isfinite(point.x) || !std::isfinite(point.y)) {
            continue;
        }
        if (scope == "selected" && selectedCases.find(point.caseId) == selectedCases.end()) {
            continue;
        }
        if (scope == "unselected" && selectedCases.find(point.caseId) != selectedCases.end()) {
            continue;
        }
        rows.push_back(point);
    }

    if (totalRows < 0) {
        totalRows = static_cast<int>(points.size());
    }
    fit.n = static_cast<int>(rows.size());
    fit.excluded = std::max(0, totalRows - fit.n);
    if (fit.n < 2) {
        fit.warning = scope == "selected"
            ? "Select at least two visible rows to fit the selected-data model."
            : (scope == "unselected"
                ? "At least two unselected visible rows are required to fit the model."
                : "At least two visible rows are required to fit the model.");
        return fit;
    }

    double sx = 0.0;
    double sy = 0.0;
    for (const ScatterplotPointValue &point : rows) {
        sx += point.x;
        sy += point.y;
    }
    fit.xMean = sx / static_cast<double>(fit.n);
    fit.yMean = sy / static_cast<double>(fit.n);

    double sxx = 0.0;
    double sxy = 0.0;
    double syy = 0.0;
    for (const ScatterplotPointValue &point : rows) {
        double dx = point.x - fit.xMean;
        double dy = point.y - fit.yMean;
        sxx += dx * dx;
        sxy += dx * dy;
        syy += dy * dy;
    }
    if (sxx <= 0.0) {
        fit.warning = "The current x variable has no variation in the chosen scope.";
        return fit;
    }

    fit.slope = sxy / sxx;
    fit.intercept = fit.yMean - fit.slope * fit.xMean;
    for (const ScatterplotPointValue &point : rows) {
        double residual = point.y - (fit.intercept + fit.slope * point.x);
        fit.sse += residual * residual;
    }
    fit.r2 = syy > 0.0 ? std::max(0.0, 1.0 - fit.sse / syy) : 1.0;
    fit.sigma = fit.n > 2 ? std::sqrt(fit.sse / static_cast<double>(fit.n - 2)) : 0.0;
    fit.ok = true;
    return fit;
}

ScatterplotFittedLine FitScatterplotLineForViewport(
    const std::vector<ScatterplotPointValue> &points,
    const DataViewport &viewport)
{
    ScatterplotFittedLine line;
    if (!IsValidViewport(viewport)) {
        return line;
    }

    SimpleLinearFitResult fit = FitSimpleLinearModel(points);
    if (!fit.ok) {
        return line;
    }

    line.ok = true;
    line.n = fit.n;
    line.intercept = fit.intercept;
    line.slope = fit.slope;
    line.start = Point{viewport.xmin, fit.intercept + fit.slope * viewport.xmin};
    line.end = Point{viewport.xmax, fit.intercept + fit.slope * viewport.xmax};
    return line;
}

std::vector<ScatterplotOverlayLineItem> BuildScatterplotOverlayLinePlan(
    const std::vector<ScatterplotPointValue> &points,
    const std::vector<ScatterplotOverlaySpec> &overlays,
    const std::set<CaseId> &selectedRows,
    const std::map<CaseId, std::string> &rowColors,
    const std::string &selectedColorName,
    const DataViewport &viewport)
{
    std::vector<ScatterplotOverlayLineItem> plan;
    if (points.empty() || !IsValidViewport(viewport)) {
        return plan;
    }

    auto appendLine = [&](const std::vector<ScatterplotPointValue> &rows,
                          const std::string &colorName,
                          double alpha,
                          double lineWidth,
                          bool dashed,
                          bool useDefaultDarkColor) {
        ScatterplotFittedLine fitted = FitScatterplotLineForViewport(rows, viewport);
        if (!fitted.ok) {
            return;
        }
        plan.push_back(ScatterplotOverlayLineItem{
            fitted.start,
            fitted.end,
            colorName,
            alpha,
            lineWidth,
            dashed,
            useDefaultDarkColor
        });
    };

    for (const ScatterplotOverlaySpec &overlay : overlays) {
        if (!overlay.visible || overlay.type != "lm") {
            continue;
        }
        if (overlay.source == "color") {
            std::map<std::string, std::vector<ScatterplotPointValue>> pointsByColor;
            for (const ScatterplotPointValue &point : points) {
                auto colorIt = rowColors.find(point.caseId);
                const std::string colorName =
                    (colorIt != rowColors.end() && !colorIt->second.empty())
                        ? colorIt->second
                        : "black";
                pointsByColor[colorName].push_back(point);
            }
            for (const auto &entry : pointsByColor) {
                appendLine(entry.second, entry.first, 0.98, 2.4, false, false);
            }
            continue;
        }

        std::vector<ScatterplotPointValue> rows;
        rows.reserve(points.size());
        for (const ScatterplotPointValue &point : points) {
            if (overlay.source == "selected" &&
                selectedRows.find(point.caseId) == selectedRows.end()) {
                continue;
            }
            rows.push_back(point);
        }
        if (overlay.source == "selected") {
            appendLine(rows, selectedColorName, 0.95, 2.6, true, false);
        } else {
            appendLine(rows, "", 0.90, 2.0, false, true);
        }
    }

    return plan;
}

ScatterplotRenderPlan BuildScatterplotRenderPlan(const ScatterplotRenderInput &input)
{
    ScatterplotRenderPlan plan;
    std::vector<ScatterplotPointDrawInput> drawInputs =
        BuildScatterplotPointDrawInputs(input.points,
                                        input.viewport,
                                        input.plotRect,
                                        input.imputationDataFrame,
                                        input.xColumn,
                                        input.yColumn,
                                        input.imputationUncertaintyMode,
                                        input.skipNonCaseRows);
    plan.points = BuildScatterplotPointDrawPlan(drawInputs,
                                                input.selectedRows,
                                                input.rowColors,
                                                input.rowLabels,
                                                input.labelDisplayMode,
                                                input.skipNonCaseRows);
    std::vector<ScatterplotOverlayLineItem> overlayLines =
        BuildScatterplotOverlayLinePlan(input.points,
                                        input.overlays,
                                        input.selectedRows,
                                        input.rowColors,
                                        input.selectedColorName,
                                        input.viewport);
    plan.overlayLines.reserve(overlayLines.size());
    for (const ScatterplotOverlayLineItem &line : overlayLines) {
        Point start = DataToScreen(line.start, input.viewport, input.plotRect, true);
        Point end = DataToScreen(line.end, input.viewport, input.plotRect, true);
        if (!ClipLineToRect(start, end, input.plotRect)) continue;
        plan.overlayLines.push_back(ScatterplotOverlayDrawItem{
            start,
            end,
            line.colorName,
            line.alpha,
            line.lineWidth,
            line.dashed,
            line.useDefaultDarkColor
        });
    }
    plan.smoothCurves = BuildSmoothCurveDrawItems(input.smoothCurves,
                                                  input.viewport,
                                                  input.plotRect);
    return plan;
}

std::vector<ScatterplotSmoothCurveDrawItem> BuildSmoothCurveDrawItems(
    const std::vector<SmoothCurveData> &curves,
    const DataViewport &viewport,
    const Rect &plotRect)
{
    std::vector<ScatterplotSmoothCurveDrawItem> items;
    if (!IsValidViewport(viewport) || !IsValidRect(plotRect)) {
        return items;
    }
    items.reserve(curves.size());
    for (const SmoothCurveData &curve : curves) {
        if (!curve.ok || curve.x.empty() || curve.y.empty()) {
            continue;
        }
        if (curve.x.size() != curve.y.size()) {
            continue;
        }
        size_t n = curve.x.size();
        bool hasNonFinite = false;
        for (size_t i = 0; i < n; ++i) {
            if (!std::isfinite(curve.x[i]) || !std::isfinite(curve.y[i])) {
                hasNonFinite = true;
                break;
            }
        }
        if (hasNonFinite) {
            continue;
        }
        ScatterplotSmoothCurveDrawItem item;
        item.points.reserve(n);
        item.colorName = curve.groupId;
        if (curve.groupId == "." || curve.groupId.empty()) {
            item.colorName = "";
        }
        if (curve.scope == SmoothCurveScope::Selection) {
            item.colorName = "gray";
            item.alpha = 0.95;
            item.lineWidth = 2.6;
            item.dashed = true;
        }
        for (size_t i = 0; i < n; ++i) {
            Point screen = DataToScreen({curve.x[i], curve.y[i]}, viewport, plotRect, true);
            if (!std::isfinite(screen.x) || !std::isfinite(screen.y)) {
                continue;
            }
            item.points.push_back(screen);
        }
        if (item.points.size() >= 2) {
            items.push_back(item);
        }
    }
    return items;
}

bool ScatterplotHasOverlaySource(const std::vector<ScatterplotOverlaySpec> &overlays,
                                 const std::string &source)
{
    for (const ScatterplotOverlaySpec &overlay : overlays) {
        if (overlay.type == "lm" && overlay.source == source && overlay.visible) {
            return true;
        }
    }
    return false;
}

ScatterplotVariableMenuState BuildScatterplotVariableMenuState(
    const std::vector<std::string> &numericVariables,
    const std::string &currentX,
    const std::string &currentY,
    const std::string &group)
{
    ScatterplotVariableMenuState state;
    for (const std::string &name : numericVariables) {
        if (name.empty()) {
            continue;
        }
        state.xOptions.push_back({
            name,
            name,
            "CHANGE_X_VARIABLE|" + name,
            true,
            name == currentX
        });
        state.yOptions.push_back({
            name,
            name,
            "CHANGE_Y_VARIABLE|" + name,
            true,
            name == currentY
        });
    }
    state.openVariablesWindow = {
        "Open variables window",
        "variables",
        "OPEN_VARIABLES_WINDOW|" + group
    };
    return state;
}

std::vector<ScatterplotMenuOption> ScatterplotMouseModeMenuOptions(bool includePanZoom)
{
    std::vector<ScatterplotMenuOption> options = {
        {"Pointer / None", "none", "SET_MODE_NONE"},
        {"Select", "select", "SET_MODE_SELECT"},
        {"Brush", "brush", "SET_MODE_BRUSH"},
        {"Identify", "identify", "SET_MODE_IDENTIFY"},
        {"Label", "label", "SET_MODE_LABEL"}
    };
    if (includePanZoom) {
        options.push_back({"Pan", "pan", "SET_MODE_PAN"});
        options.push_back({"Zoom", "zoom", "SET_MODE_ZOOM"});
    }
    return options;
}

std::vector<ScatterplotMenuOption> ScatterplotSelectionModeMenuOptions()
{
    return {
        {"Replace selection", "replace", "SET_SELECTION_REPLACE"},
        {"Add to selection", "add", "SET_SELECTION_ADD"},
        {"Subtract from selection", "subtract", "SET_SELECTION_SUBTRACT"},
        {"Toggle selection", "toggle", "SET_SELECTION_TOGGLE"}
    };
}

std::vector<ScatterplotMenuOption> ScatterplotSelectionActionMenuOptions()
{
    std::vector<ScatterplotMenuOption> options;
    for (const LinkedPlotCommandOption &selectionOption : SelectionCommandOptions(false)) {
        std::string title = selectionOption.title;
        if (selectionOption.value == "clear") {
            title = "Clear selection";
        } else if (selectionOption.value == "invert") {
            title = "Invert selection";
        }
        options.push_back({
            title,
            selectionOption.value,
            selectionOption.command
        });
    }
    options.push_back({
        "Select all visible",
        "select_all_visible",
        "SELECT_ALL_VISIBLE"
    });
    return options;
}

std::vector<ScatterplotMenuOption> ScatterplotBrushMenuOptions()
{
    return {
        {"Brush mode", "brush", "SET_MODE_BRUSH"},
        {"Increase brush size", "larger", "BRUSH_LARGER"},
        {"Decrease brush size", "smaller", "BRUSH_SMALLER"}
    };
}

std::vector<ScatterplotMenuOption> ScatterplotViewMenuOptions()
{
    return {
        {"Reset zoom", "reset_zoom", "RESET_ZOOM"},
        {"Rescale to data", "rescale", "RESCALE"}
    };
}

ScatterplotMenuOption ScatterplotChooseLabelColumnOption(const std::string &labelColumn)
{
    return {
        labelColumn.empty() ? "Choose label column..." : "Label column: " + labelColumn,
        "label_column",
        "CHOOSE_LABEL_COLUMN"
    };
}

std::vector<ScatterplotMenuOption> ScatterplotLabelDisplayMenuOptions(
    const std::string &labelDisplayMode)
{
    std::vector<ScatterplotMenuOption> options = {
        {"Hide point labels", "none", "SET_LABEL_DISPLAY|none"},
        {"Show labels for selected points", "selected", "SET_LABEL_DISPLAY|selected"},
        {"Show labels for all points", "all", "SET_LABEL_DISPLAY|all"}
    };
    for (ScatterplotMenuOption &option : options) {
        option.checked = option.value == labelDisplayMode;
    }
    return options;
}

std::vector<ScatterplotMenuOption> ScatterplotImputationUncertaintyMenuOptions(
    const std::string &currentMode)
{
    const std::vector<std::string> modes = {"central80", "iqr", "sd", "se"};
    std::string current = NormalizedScatterImputationUncertaintyMode(currentMode);
    std::vector<ScatterplotMenuOption> options;
    options.reserve(modes.size());
    for (const std::string &mode : modes) {
        options.push_back({
            ScatterImputationUncertaintyDisplayName(mode),
            mode,
            "SET_IMPUTATION_UNCERTAINTY|" + mode,
            true,
            mode == current
        });
    }
    return options;
}

std::vector<ScatterplotMenuOption> ScatterplotPlotMenuOptions()
{
    std::vector<ScatterplotMenuOption> options;
    for (const LinkedPlotCommandOption &plotOption : LinkedPlotCommandOptions()) {
        options.push_back({
            plotOption.title,
            plotOption.value,
            plotOption.command
        });
    }
    return options;
}

ScatterplotMenuOption ScatterplotClosePlotOption()
{
    return {"Close plot", "close", "CLOSE_PLOT"};
}

static bool ScatterplotHasSmoothScope(const std::vector<SmoothCurveData> &curves,
                                       SmoothCurveScope scope)
{
    for (const SmoothCurveData &c : curves) {
        if (c.scope == scope) {
            return true;
        }
    }
    return false;
}

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
    const std::vector<SmoothCurveData> &smoothCurves)
{
    ScatterplotMenuState state;
    state.variables = BuildScatterplotVariableMenuState(numericVariables, currentX, currentY, group);
    state.mouseModeOptions = ScatterplotMouseModeMenuOptions(true);
    state.selectionModeOptions = ScatterplotSelectionModeMenuOptions();
    state.selectionActionOptions = ScatterplotSelectionActionMenuOptions();
    state.brushOptions = ScatterplotBrushMenuOptions();
    state.viewOptions = ScatterplotViewMenuOptions();
    state.showImputationUncertaintyOptions = hasMultipleImputation;
    if (hasMultipleImputation) {
        state.imputationUncertaintyOptions =
            ScatterplotImputationUncertaintyMenuOptions(imputationUncertaintyMode);
    }
    state.chooseLabelColumn = ScatterplotChooseLabelColumnOption(labelColumn);
    state.labelDisplayOptions = ScatterplotLabelDisplayMenuOptions(labelDisplayMode);

    const bool hasAll = ScatterplotHasOverlaySource(overlays, "all");
    const bool hasSelected = ScatterplotHasOverlaySource(overlays, "selected");
    state.overlayOptions = {
        {"Linear regression - all data", "all", "TOGGLE_LM_ALL", true, hasAll},
        {"Linear regression - selected data", "selected", "TOGGLE_LM_SELECTED", true, hasSelected},
        {"Linear regression - both", "both", "TOGGLE_LM_BOTH", true, hasAll && hasSelected},
        {"Linear regression - by point color", "color", "TOGGLE_LM_COLOR", true,
            ScatterplotHasOverlaySource(overlays, "color")}
    };
    state.clearOverlays = {"Remove all overlays", "clear", "CLEAR_OVERLAYS"};
    state.smoothOptions = {
        {"Smooth curve - overall", "overall", "TOGGLE_SMOOTH_OVERALL", true,
            ScatterplotHasSmoothScope(smoothCurves, SmoothCurveScope::Overall)},
        {"Smooth curve - selected data", "selected", "TOGGLE_SMOOTH_SELECTED", true,
            ScatterplotHasSmoothScope(smoothCurves, SmoothCurveScope::Selection)},
        {"Smooth curve - by point color", "color", "TOGGLE_SMOOTH_COLOR", true,
            ScatterplotHasSmoothScope(smoothCurves, SmoothCurveScope::ColorGroup)}
    };
    state.analysisOptions = {
        {"Correlation: " + currentY + " with " + currentX, "correlation", "CONTEXT_CORRELATION_XY"},
        {"Linear model: " + currentY + " ~ " + currentX, "linear_model", "CONTEXT_LINEAR_MODEL_XY"},
        {"Descriptives: " + currentX + " and " + currentY, "descriptives", "CONTEXT_DESCRIPTIVES_XY"}
    };
    state.plotOptions = ScatterplotPlotMenuOptions();
    state.closePlot = ScatterplotClosePlotOption();
    return state;
}

ScatterplotCreationDialogState BuildScatterplotCreationDialogState()
{
    return ScatterplotCreationDialogState{};
}

std::string ScatterplotDefaultTitle(const std::string &x,
                                     const std::string &y)
{
    return y + " vs " + x;
}

std::string ScatterplotNoValidXYStatus()
{
    return "This scatterplot does not have a valid X/Y pair for a simple linear model.";
}

std::string ScatterplotAddIndependentVariableTitle()
{
    return "Add independent variable";
}

std::string ScatterplotReplacePredictorTitle()
{
    return "Replace predictor";
}

std::string ScatterplotSetActiveModelTitle()
{
    return "Set as active model";
}

std::string ScatterplotXFieldLabel()
{
    return "X:";
}

std::string ScatterplotYFieldLabel()
{
    return "Y:";
}

std::string ScatterplotGroupFieldLabel()
{
    return "Group:";
}

void RebuildPointsForCurrentVariables(PlotModel &model)
{
    NumericVariable *xvar = FindNumericVariable(model, model.xLabel);
    NumericVariable *yvar = FindNumericVariable(model, model.yLabel);
    if (!xvar || !yvar) {
        return;
    }
    size_t n = std::min(xvar->values.size(), yvar->values.size());
    model.points.clear();
    model.points.reserve(n);
    for (size_t i = 0; i < n; ++i) {
        double x = xvar->values[i];
        double y = yvar->values[i];
        if (std::isfinite(x) && std::isfinite(y)) {
            model.points.push_back(DataPoint{x, y, (int)i + 1});
        }
    }
}

void ComputeRanges(PlotModel &model)
{
    std::vector<Point> points;
    points.reserve(model.points.size());
    for (const DataPoint &p : model.points) {
        points.push_back(Point{p.x, p.y});
    }
    DataViewport viewport = DataViewportForPoints(points);
    model.dataXmin = viewport.xmin;
    model.dataXmax = viewport.xmax;
    model.dataYmin = viewport.ymin;
    model.dataYmax = viewport.ymax;
    model.xmin = viewport.xmin;
    model.xmax = viewport.xmax;
    model.ymin = viewport.ymin;
    model.ymax = viewport.ymax;
}

} // namespace core
} // namespace rlispstat
