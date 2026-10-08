#include "scatterplot_model.h"

#include "command_model.h"

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <unordered_map>

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

std::vector<std::size_t> ScatterplotVisualOverlapCounts(const std::vector<Point> &positions)
{
    // Pixel cells coalesce dense stacks before neighbourhood lookup. Work is
    // bounded by occupied cells, not by all pairs of observations.
    using Key = std::uint64_t;
    auto key = [](int x, int y) -> Key {
        return (static_cast<Key>(static_cast<std::uint32_t>(x)) << 32) |
            static_cast<std::uint32_t>(y);
    };
    auto usable = [](const Point &p) {
        return std::isfinite(p.x) && std::isfinite(p.y) &&
            std::abs(p.x) < 1e9 && std::abs(p.y) < 1e9;
    };
    struct Cell { int x, y; std::size_t count = 0, overlap = 0; };
    std::unordered_map<Key, Cell> cells;
    for (const auto &p : positions) {
        if (!usable(p)) continue;
        const int x = static_cast<int>(std::lround(p.x)), y = static_cast<int>(std::lround(p.y));
        auto inserted = cells.emplace(key(x, y), Cell{x, y});
        ++inserted.first->second.count;
    }
    for (auto &entry : cells) {
        auto &cell = entry.second;
        // Two ordinary circles touch when their centres are six points apart.
        // Do not let the enlarged marks recursively create extra overlap.
        for (int dx = -6; dx <= 6; ++dx) for (int dy = -6; dy <= 6; ++dy) {
            if (dx * dx + dy * dy > 36) continue;
            const auto neighbour = cells.find(key(cell.x + dx, cell.y + dy));
            if (neighbour != cells.end()) cell.overlap += neighbour->second.count;
        }
    }
    std::vector<std::size_t> result; result.reserve(positions.size());
    for (const auto &p : positions) {
        result.push_back(usable(p) ? cells.at(key(static_cast<int>(std::lround(p.x)),
            static_cast<int>(std::lround(p.y)))).overlap : 1U);
    }
    return result;
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
            // Selection is expressed by an exterior ring.  Preserve the
            // point's ordinary size so linked plots do not appear to encode
            // a larger quantitative value.
            item.radius = hasRowColor ? 3.1 : 3.0;
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

    // The vector is the painter's order used by every native and exported
    // scatter renderer. Keep background observations first so coincident
    // unselected cases can never cover a selected case or its selection ring.
    std::stable_partition(
        plan.begin(), plan.end(),
        [](const ScatterplotPointDrawItem &item) { return !item.selected; });

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

static NumericImputationRange DiagnosticPointAxisRange(
    const ScatterplotPointImputationValues &values,
    bool xAxis,
    const std::string &uncertaintyMode)
{
    std::vector<double> axisValues;
    axisValues.reserve(values.values.size());
    for (const Point &value : values.values) {
        const double coordinate = xAxis ? value.x : value.y;
        if (std::isfinite(coordinate)) axisValues.push_back(coordinate);
    }
    if (axisValues.size() < 2) return {};
    const auto limits = std::minmax_element(axisValues.begin(), axisValues.end());
    if (std::fabs(*limits.second - *limits.first) <= 1.0e-12) return {};
    return NumericImputationRangeForValues(std::move(axisValues), uncertaintyMode);
}

std::optional<DataViewport> ScatterplotViewportIncludingPointImputations(
    const std::vector<ScatterplotPointValue> &points,
    const std::vector<ScatterplotPointImputationValues> &imputationValues,
    const std::string &uncertaintyMode)
{
    if (points.empty() || imputationValues.empty()) return std::nullopt;
    std::map<CaseId, const ScatterplotPointImputationValues *> byRow;
    for (const auto &values : imputationValues) byRow[values.row] = &values;
    NumericImputationRange xRange;
    NumericImputationRange yRange;
    for (const ScatterplotPointValue &point : points) {
        auto found = byRow.find(point.caseId);
        NumericImputationRange xi;
        NumericImputationRange yi;
        if (found != byRow.end()) {
            xi = DiagnosticPointAxisRange(*found->second, true, uncertaintyMode);
            yi = DiagnosticPointAxisRange(*found->second, false, uncertaintyMode);
        }
        if (xi.any) {
            NumericRangeInclude(xRange, xi.min);
            NumericRangeInclude(xRange, xi.max);
        } else NumericRangeInclude(xRange, point.x);
        if (yi.any) {
            NumericRangeInclude(yRange, yi.min);
            NumericRangeInclude(yRange, yi.max);
        } else NumericRangeInclude(yRange, point.y);
    }
    if (!xRange.any || !yRange.any) return std::nullopt;
    if (xRange.min == xRange.max) { xRange.min -= 0.5; xRange.max += 0.5; }
    if (yRange.min == yRange.max) { yRange.min -= 0.5; yRange.max += 0.5; }
    const double xPad = (xRange.max - xRange.min) * 0.05;
    const double yPad = (yRange.max - yRange.min) * 0.05;
    return DataViewport{xRange.min - xPad, xRange.max + xPad,
                        yRange.min - yPad, yRange.max + yPad};
}

static ScatterplotImputationGlyph ScatterplotGlyphForRanges(
    const NumericImputationRange &xi,
    const NumericImputationRange &yi,
    const ScatterplotPointValue &point,
    const DataViewport &viewport,
    const Rect &plotRect,
    double singleAxisMinimumSize,
    double twoAxisMinimumSize)
{
    ScatterplotImputationGlyph glyph;
    if ((!xi.any && !yi.any) || !IsValidViewport(viewport) || !IsValidRect(plotRect)) {
        return glyph;
    }
    glyph.axisMask = (xi.any ? 1 : 0) | (yi.any ? 2 : 0);
    const double centerX = xi.any ? xi.center : point.x;
    const double centerY = yi.any ? yi.center : point.y;
    if (!std::isfinite(centerX) || !std::isfinite(centerY)) return {};
    Point center = DataToScreen({centerX, centerY}, viewport, plotRect, true);
    if (!std::isfinite(center.x) || !std::isfinite(center.y)) return {};
    const bool twoAxisGlyph = xi.any && yi.any;
    const double minimumSize = std::max(
        0.0, twoAxisGlyph ? twoAxisMinimumSize : singleAxisMinimumSize);
    double width = minimumSize;
    double height = minimumSize;
    if (xi.any) {
        Point lo = DataToScreen({xi.min, centerY}, viewport, plotRect, true);
        Point hi = DataToScreen({xi.max, centerY}, viewport, plotRect, true);
        if (std::isfinite(lo.x) && std::isfinite(hi.x))
            width = std::max(std::fabs(hi.x - lo.x), minimumSize);
    }
    if (yi.any) {
        Point lo = DataToScreen({centerX, yi.min}, viewport, plotRect, true);
        Point hi = DataToScreen({centerX, yi.max}, viewport, plotRect, true);
        if (std::isfinite(lo.y) && std::isfinite(hi.y))
            height = std::max(std::fabs(hi.y - lo.y), minimumSize);
    }
    glyph.hasImputation = true;
    glyph.rect = {center.x - width / 2.0, center.y - height / 2.0, width, height};
    return glyph;
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
    return ScatterplotGlyphForRanges(xi, yi, point, viewport, plotRect,
                                     singleAxisMinimumSize, twoAxisMinimumSize);
}

ScatterplotImputationGlyph ScatterplotImputationGlyphForPointValues(
    const ScatterplotPointImputationValues &values,
    const ScatterplotPointValue &point,
    const DataViewport &viewport,
    const Rect &plotRect,
    const std::string &uncertaintyMode,
    double singleAxisMinimumSize,
    double twoAxisMinimumSize)
{
    return ScatterplotGlyphForRanges(
        DiagnosticPointAxisRange(values, true, uncertaintyMode),
        DiagnosticPointAxisRange(values, false, uncertaintyMode),
        point, viewport, plotRect, singleAxisMinimumSize, twoAxisMinimumSize);
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

std::vector<ScatterplotCaseGeometry> BuildScatterplotCaseGeometry(
    const ScatterplotRenderInput &input)
{
    const ScatterplotRenderPlan plan = BuildScatterplotRenderPlan(input);
    std::vector<ScatterplotCaseGeometry> geometry;
    geometry.reserve(plan.points.size());
    for (const ScatterplotPointDrawItem &item : plan.points) {
        ScatterplotCaseGeometry entry;
        entry.caseId = item.caseId;
        entry.point = item.point;
        entry.hasGlyphRect = item.hasImputationGlyph &&
            item.imputationGlyph.hasImputation;
        if (entry.hasGlyphRect) entry.glyphRect = item.imputationGlyph.rect;
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

std::vector<ScatterplotImputationPointSet> BuildScatterplotImputationPointSets(
    const ScatterplotRenderInput &input)
{
    std::vector<ScatterplotImputationPointSet> sets;

    // Regression diagnostic plots already carry the exact point generated by
    // each separately fitted imputation.  Use those values directly.
    const std::vector<ScatterplotPointImputationValues> *completeValues =
        input.completeImputationPointValues &&
        !input.completeImputationPointValues->empty()
            ? input.completeImputationPointValues
            : input.pointImputationValues;
    if (completeValues && !completeValues->empty()) {
        std::size_t count = 0;
        for (const ScatterplotPointImputationValues &values :
             *completeValues) {
            if (count == 0) count = values.values.size();
            else count = std::min(count, values.values.size());
        }
        if (count > 1) {
            sets.resize(count);
            for (std::size_t index = 0; index < count; ++index)
                sets[index].imputationIndex = static_cast<int>(index + 1);
            for (const ScatterplotPointImputationValues &values :
                 *completeValues) {
                for (std::size_t index = 0; index < count; ++index) {
                    const Point point = values.values[index];
                    if (!std::isfinite(point.x) || !std::isfinite(point.y)) continue;
                    sets[index].points.push_back(
                        ScatterplotPointValue{values.row, point.x, point.y});
                }
            }
            return sets;
        }
    }

    // Ordinary MI scatterplots reconstruct each completed dataset from the
    // worksheet's sparse imputation storage.  Fully observed cells naturally
    // contribute the same value to every completed dataset.
    if (!input.imputationDataFrame || !input.xColumn || !input.yColumn ||
        !DataFrameShowsAllImputations(*input.imputationDataFrame) ||
        input.imputationDataFrame->imputationCount <= 1) {
        return sets;
    }
    const DataFrameModel &df = *input.imputationDataFrame;
    sets.resize(static_cast<std::size_t>(df.imputationCount));
    for (int version = 0; version < df.imputationCount; ++version) {
        ScatterplotImputationPointSet &set = sets[static_cast<std::size_t>(version)];
        set.imputationIndex = version + 1;
        set.points.reserve(input.points.size());
        for (const ScatterplotPointValue &base : input.points) {
            if (base.caseId <= 0 || base.caseId > df.rows) continue;
            const std::size_t row = static_cast<std::size_t>(base.caseId - 1);
            const double x = NumericValueForDataFrameCellVersion(
                df, *input.xColumn, row, version);
            const double y = NumericValueForDataFrameCellVersion(
                df, *input.yColumn, row, version);
            if (!std::isfinite(x) || !std::isfinite(y)) continue;
            set.points.push_back(ScatterplotPointValue{base.caseId, x, y});
        }
    }
    return sets;
}

std::vector<ScatterplotImputationPointSet>
BuildRegressionDiagnosticSmoothPointSets(const PlotModel &model)
{
    std::vector<ScatterplotImputationPointSet> sets;
    if (!RegressionDiagnosticSupportsAddedLines(model)) return sets;

    if (model.diagnosticShowImputationUncertainty &&
        (!model.diagnosticAllImputationValues.empty() ||
         !model.diagnosticImputationValues.empty())) {
        ScatterplotRenderInput input;
        input.points.reserve(model.points.size());
        for (const DataPoint &point : model.points)
            input.points.push_back({point.row, point.x, point.y});
        input.pointImputationValues = &model.diagnosticImputationValues;
        input.completeImputationPointValues =
            &model.diagnosticAllImputationValues;
        sets = BuildScatterplotImputationPointSets(input);
        if (!sets.empty()) return sets;
    }

    ScatterplotImputationPointSet displayed;
    displayed.imputationIndex = std::max(1, model.diagnosticImputationIndex);
    displayed.points.reserve(model.points.size());
    for (const DataPoint &point : model.points) {
        if (!std::isfinite(point.x) || !std::isfinite(point.y)) continue;
        displayed.points.push_back({point.row, point.x, point.y});
    }
    if (!displayed.points.empty()) sets.push_back(std::move(displayed));
    return sets;
}

bool RegressionDiagnosticSupportsAddedLines(const PlotModel &model)
{
    return model.isGLMDiagnostic && model.kind == "scatter" &&
        model.glmDiagnosticKind != "roc_curve";
}

ScatterplotRenderPlan BuildScatterplotRenderPlan(const ScatterplotRenderInput &input)
{
    ScatterplotRenderPlan plan;
    std::vector<ScatterplotPointDrawInput> computedDrawInputs;
    if (!input.precomputedPointDrawInputs) {
        computedDrawInputs = BuildScatterplotPointDrawInputs(
            input.points, input.viewport, input.plotRect,
            input.imputationDataFrame, input.xColumn, input.yColumn,
            input.imputationUncertaintyMode, input.skipNonCaseRows);
    } else if (input.pointImputationValues && !input.pointImputationValues->empty()) {
        // Diagnostic glyphs override worksheet glyphs below. Keep the caller's
        // cached geometry immutable when that override is active.
        computedDrawInputs = *input.precomputedPointDrawInputs;
    }
    std::vector<ScatterplotPointDrawInput> &drawInputs = computedDrawInputs;
    if (input.pointImputationValues && !input.pointImputationValues->empty()) {
        std::map<CaseId, const ScatterplotPointImputationValues *> byRow;
        for (const auto &values : *input.pointImputationValues) byRow[values.row] = &values;
        for (std::size_t index = 0; index < drawInputs.size() && index < input.points.size(); ++index) {
            auto found = byRow.find(input.points[index].caseId);
            if (found == byRow.end()) continue;
            ScatterplotImputationGlyph glyph = ScatterplotImputationGlyphForPointValues(
                *found->second, input.points[index], input.viewport, input.plotRect,
                input.imputationUncertaintyMode);
            if (glyph.hasImputation) {
                drawInputs[index].hasImputationGlyph = true;
                drawInputs[index].imputationGlyph = glyph;
            }
        }
    }
    const std::vector<ScatterplotPointDrawInput> &effectiveDrawInputs =
        input.precomputedPointDrawInputs &&
        !(input.pointImputationValues && !input.pointImputationValues->empty())
            ? *input.precomputedPointDrawInputs : drawInputs;
    plan.points = BuildScatterplotPointDrawPlan(effectiveDrawInputs,
                                                input.selectedRows,
                                                input.rowColors,
                                                input.rowLabels,
                                                input.labelDisplayMode,
                                                input.skipNonCaseRows);
    std::map<std::pair<double, double>, std::size_t> overlapCounts;
    if (input.sizeByOverlap && !input.sizeByVisualOverlap) {
        for (const auto &item : plan.points)
            if (!item.hasImputationGlyph && std::isfinite(item.point.x) && std::isfinite(item.point.y))
                ++overlapCounts[{item.point.x, item.point.y}];
    }
    std::vector<std::size_t> visualCounts;
    if (input.sizeByOverlap && input.sizeByVisualOverlap) {
        std::vector<Point> positions; positions.reserve(plan.points.size());
        for (const auto &item : plan.points)
            positions.push_back(item.hasImputationGlyph ? Point{NAN, NAN} : item.point);
        visualCounts = ScatterplotVisualOverlapCounts(positions);
    }
    std::size_t pointIndex = 0;
    for (ScatterplotPointDrawItem &item : plan.points) {
        if (input.sizeByOverlap && !item.hasImputationGlyph &&
            std::isfinite(item.point.x) && std::isfinite(item.point.y)) {
            // Area, rather than radius, represents the number of coincident cases.
            // Keep every case in the plan for linked selection and labels.
            const auto count = input.sizeByVisualOverlap ? visualCounts[pointIndex]
                : overlapCounts.at({item.point.x, item.point.y});
            item.radius = 3.0 * std::sqrt(static_cast<double>(count));
        }
        ++pointIndex;
        item.shadeOverlap = input.shadeOverlap;
        // Density must not appear or disappear when an unrelated case is selected.
        if (item.shadeOverlap && !item.selected && !item.hasImputationGlyph) {
            item.fillAlpha = 0.35;
            item.strokeAlpha = 0.35;
        }
    }
    if (input.distinguishDiagnosticImputationRows) {
        for (ScatterplotPointDrawItem &item : plan.points) {
            const bool directlyImputed =
                input.directlyImputedModelRows.find(item.caseId) !=
                input.directlyImputedModelRows.end();
            item.colorName = directlyImputed ? "red" : "black";
            item.hasExplicitColor = true;
        }
        // Selection remains the outer painter layer. Within each selection
        // state, directly imputed cases are drawn after fitted-propagation
        // glyphs so their red semantic layer remains visible too.
        std::stable_sort(
            plan.points.begin(), plan.points.end(),
            [](const ScatterplotPointDrawItem &left,
               const ScatterplotPointDrawItem &right) {
                if (left.selected != right.selected)
                    return !left.selected;
                const bool leftDirect = left.colorName == "red";
                const bool rightDirect = right.colorName == "red";
                return leftDirect != rightDirect && !leftDirect;
            });
    }
    // Fitted lines are returned by R in smoothCurves. Reconstructing every
    // completed imputation here only to discard the native line calculation
    // added a second cases-by-imputations pass on every selection repaint.
    plan.smoothCurves = BuildSmoothCurveDrawItems(input.smoothCurves,
                                                  input.viewport,
                                                  input.plotRect,
                                                  input.showFitConfidenceIntervals,
                                                  input.showSmoothConfidenceIntervals);
    return plan;
}

std::vector<ScatterplotSmoothCurveDrawItem> BuildSmoothCurveDrawItems(
    const std::vector<SmoothCurveData> &curves,
    const DataViewport &viewport,
    const Rect &plotRect,
    bool showConfidenceIntervals)
{
    return BuildSmoothCurveDrawItems(curves, viewport, plotRect,
                                     showConfidenceIntervals,
                                     showConfidenceIntervals);
}

std::vector<ScatterplotSmoothCurveDrawItem> BuildSmoothCurveDrawItems(
    const std::vector<SmoothCurveData> &curves,
    const DataViewport &viewport,
    const Rect &plotRect,
    bool showLinearConfidenceIntervals,
    bool showSmoothConfidenceIntervals)
{
    std::vector<ScatterplotSmoothCurveDrawItem> items;
    if (!IsValidViewport(viewport) || !IsValidRect(plotRect)) {
        return items;
    }
    items.reserve(curves.size());
    std::size_t colouredIntervalCount = 0;
    for (const SmoothCurveData &curve : curves) {
        if (curve.ok && curve.confidenceLower.size() == curve.x.size() &&
            curve.confidenceUpper.size() == curve.x.size() &&
            !curve.groupId.empty() && curve.groupId != ".")
            ++colouredIntervalCount;
    }
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
        ScatterplotSmoothCurveDrawItem style;
        std::string displayGroup = curve.groupId;
        const std::size_t imputationMarker = displayGroup.find("\x1f" "mi:");
        const bool isImputationCurve = imputationMarker != std::string::npos;
        if (isImputationCurve) displayGroup.resize(imputationMarker);
        style.colorName = displayGroup;
        if (displayGroup == "." || displayGroup.empty()) style.colorName = "";
        if (curve.scope == SmoothCurveScope::Selection) {
            style.colorName = "gray";
            style.alpha = 0.95;
            style.lineWidth = 2.6;
            style.dashed = true;
        }
        if (isImputationCurve) {
            style.alpha = std::min(style.alpha, 0.52);
            style.lineWidth = std::min(style.lineWidth, 1.75);
        }
        style.confidenceAlpha = colouredIntervalCount > 1
            ? std::max(0.055, 0.20 / std::sqrt(static_cast<double>(colouredIntervalCount)))
            : 0.16;
        if (curve.scope == SmoothCurveScope::Selection)
            style.confidenceAlpha = std::min(style.confidenceAlpha, 0.12);
        if (isImputationCurve)
            style.confidenceAlpha = std::min(style.confidenceAlpha, 0.055);
        const bool showConfidenceIntervals = curve.fitMethod == "loess"
            ? showSmoothConfidenceIntervals
            : showLinearConfidenceIntervals;
        if (showConfidenceIntervals &&
            curve.confidenceLower.size() == n && curve.confidenceUpper.size() == n) {
            std::vector<Point> lower;
            std::vector<Point> upper;
            lower.reserve(n); upper.reserve(n);
            for (size_t i = 0; i < n; ++i) {
                if (!std::isfinite(curve.confidenceLower[i]) ||
                    !std::isfinite(curve.confidenceUpper[i])) continue;
                Point lo = DataToScreen({curve.x[i], curve.confidenceLower[i]},
                                        viewport, plotRect, true);
                Point hi = DataToScreen({curve.x[i], curve.confidenceUpper[i]},
                                        viewport, plotRect, true);
                if (!std::isfinite(lo.x) || !std::isfinite(lo.y) ||
                    !std::isfinite(hi.x) || !std::isfinite(hi.y)) continue;
                lo.x = std::max(plotRect.x, std::min(plotRect.x + plotRect.width, lo.x));
                lo.y = std::max(plotRect.y, std::min(plotRect.y + plotRect.height, lo.y));
                hi.x = std::max(plotRect.x, std::min(plotRect.x + plotRect.width, hi.x));
                hi.y = std::max(plotRect.y, std::min(plotRect.y + plotRect.height, hi.y));
                lower.push_back(lo); upper.push_back(hi);
            }
            if (lower.size() >= 2 && lower.size() == upper.size()) {
                style.confidencePolygon = lower;
                for (auto it = upper.rbegin(); it != upper.rend(); ++it)
                    style.confidencePolygon.push_back(*it);
            }
        }
        std::vector<Point> screenPoints;
        screenPoints.reserve(n);
        for (size_t i = 0; i < n; ++i) {
            Point screen = DataToScreen({curve.x[i], curve.y[i]}, viewport, plotRect, true);
            if (!std::isfinite(screen.x) || !std::isfinite(screen.y)) {
                continue;
            }
            screenPoints.push_back(screen);
        }
        ScatterplotSmoothCurveDrawItem run = style;
        auto flush = [&]() {
            if (run.points.size() >= 2) items.push_back(run);
            run = style;
        };
        for (size_t i = 1; i < screenPoints.size(); ++i) {
            Point start = screenPoints[i - 1];
            Point end = screenPoints[i];
            if (!ClipLineToRect(start, end, plotRect)) {
                flush();
                continue;
            }
            const bool contiguous = !run.points.empty() &&
                std::fabs(run.points.back().x - start.x) < 1e-6 &&
                std::fabs(run.points.back().y - start.y) < 1e-6;
            if (!contiguous) flush();
            if (run.points.empty()) run.points.push_back(start);
            run.points.push_back(end);
        }
        flush();
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

std::vector<ScatterplotMenuOption> ScatterplotMouseModeMenuOptions(bool)
{
    // Native views use direct click/rectangle selection. Keep the API for clients.
    return {};
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
    return {};
}

std::vector<ScatterplotMenuOption> ScatterplotViewMenuOptions()
{
    return {};
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

std::string ScatterplotImputationDisplayMenuTitle(
    int imputationCount,
    int activeImputationVersion,
    const std::string &displayMode)
{
    const int count = std::max(1, imputationCount);
    const int active = std::max(1, std::min(count, activeImputationVersion));
    if (displayMode == "all") {
        return "Imputations: All (m = " + std::to_string(count) + ")";
    }
    if (displayMode == "original") {
        return "Imputations: Original data";
    }
    return "Imputation: " + std::to_string(active) + " of " +
        std::to_string(count);
}

std::vector<ScatterplotMenuOption> ScatterplotImputationDisplayMenuOptions(
    int imputationCount,
    int activeImputationVersion,
    const std::string &displayMode)
{
    std::vector<ScatterplotMenuOption> options;
    if (imputationCount <= 0) return options;
    const int active = std::max(1, std::min(imputationCount, activeImputationVersion));
    options.reserve(static_cast<std::size_t>(imputationCount) + 2);
    for (int version = 1; version <= imputationCount; ++version) {
        options.push_back({
            "Imputation " + std::to_string(version) + " of " +
                std::to_string(imputationCount),
            "version:" + std::to_string(version),
            "SET_IMPUTATION_DISPLAY|version:" + std::to_string(version),
            true,
            displayMode == "version" && version == active
        });
    }
    options.push_back({
        "All imputations (m = " + std::to_string(imputationCount) + ")",
        "all",
        "SET_IMPUTATION_DISPLAY|all",
        true,
        displayMode == "all"
    });
    options.push_back({
        "Original incomplete data",
        "original",
        "SET_IMPUTATION_DISPLAY|original",
        true,
        displayMode == "original"
    });
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
    // A straight lm line and a LOESS curve can share a scope.  The smooth
    // menu must reflect only its own curve, including a pending R fit.
    return SmoothCurveScopeIsPresent(curves, scope);
}

ScatterplotMenuState BuildScatterplotMenuState(
    const std::vector<std::string> &numericVariables,
    const std::string &currentX,
    const std::string &currentY,
    const std::string &group,
    const std::string &labelColumn,
    const std::string &labelDisplayMode,
    const std::vector<ScatterplotOverlaySpec> &overlays,
    int imputationCount,
    int activeImputationVersion,
    const std::string &imputationDisplayMode,
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
    state.showImputationDisplayOptions = imputationCount > 0;
    if (state.showImputationDisplayOptions) {
        state.imputationDisplayTitle = ScatterplotImputationDisplayMenuTitle(
            imputationCount, activeImputationVersion, imputationDisplayMode);
        state.imputationDisplayOptions = ScatterplotImputationDisplayMenuOptions(
            imputationCount, activeImputationVersion, imputationDisplayMode);
    }
    state.showImputationUncertaintyOptions =
        state.showImputationDisplayOptions && imputationDisplayMode == "all";
    if (state.showImputationUncertaintyOptions) {
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
    state.clearOverlays = {"Remove all regression lines", "clear", "CLEAR_OVERLAYS"};
    state.smoothOptions = {
        {"Smooth curve - overall", "overall", "TOGGLE_SMOOTH_OVERALL", true,
            ScatterplotHasSmoothScope(smoothCurves, SmoothCurveScope::Overall)},
        {"Smooth curve - selected data", "selected", "TOGGLE_SMOOTH_SELECTED", true,
            ScatterplotHasSmoothScope(smoothCurves, SmoothCurveScope::Selection)},
        {"Smooth curve - by point color", "color", "TOGGLE_SMOOTH_COLOR", true,
            ScatterplotHasSmoothScope(smoothCurves, SmoothCurveScope::ColorGroup)}
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
    // Hidden intervals must not determine the visible viewport.  Keeping them
    // in the model is still useful for an instantaneous display toggle, but a
    // point-only effect plot should be scaled from the geometry it actually
    // shows.  The native adapters recompute the range when intervals are
    // toggled back on.
    if (model.kind == "glm_interaction" &&
        model.regressionConfidenceIntervalsVisible) {
        for (const InteractionPlotLine &line : model.interactionPlotLines) {
            for (const DataPoint &p : line.confidenceLower) {
                if (std::isfinite(p.x) && std::isfinite(p.y)) points.push_back({p.x, p.y});
            }
            for (const DataPoint &p : line.confidenceUpper) {
                if (std::isfinite(p.x) && std::isfinite(p.y)) points.push_back({p.x, p.y});
            }
        }
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
