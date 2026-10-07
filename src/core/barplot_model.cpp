#include "barplot_model.h"
#include "command_model.h"
#include "dataset_model.h"
#include "format_model.h"
#include "string_utils.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdlib>
#include <iomanip>
#include <numeric>
#include <sstream>
#include <vector>

namespace rlispstat {
namespace core {

namespace {

template <typename T>
void EraseKeysNotIn(std::map<std::string, T> &values, const std::set<std::string> &validKeys)
{
    for (auto it = values.begin(); it != values.end();) {
        if (validKeys.find(it->first) == validKeys.end()) {
            it = values.erase(it);
        } else {
            ++it;
        }
    }
}

std::string JoinDisplayValues(const std::vector<std::string> &values,
                              const std::string &separator)
{
    std::ostringstream out;
    for (std::size_t i = 0; i < values.size(); ++i) {
        if (i) {
            out << separator;
        }
        out << values[i];
    }
    return out.str();
}

} // namespace

bool IsValidBarplotLayout(const BarplotLayout &layout)
{
    return IsValidRect(layout.plotRect) &&
        !layout.values.empty() &&
        std::isfinite(layout.yMaximum) &&
        layout.yMaximum > 0.0;
}

std::vector<Rect> BarplotBarRects(const BarplotLayout &layout)
{
    std::vector<Rect> rects;
    if (!IsValidBarplotLayout(layout)) {
        return rects;
    }

    const std::size_t n = layout.values.size();
    const Rect contentRect = ZeroBaselineContentRect(layout.plotRect);
    rects.reserve(n);
    const std::size_t nestingDepth = std::max<std::size_t>(1, layout.nestingDepth);
    const bool nestedX = nestingDepth > 1;
    const double slot = contentRect.width / static_cast<double>(std::max<std::size_t>(1, n));
    const double baseGap = layout.widthMode == BarplotWidthMode::Equal
        ? (nestedX ? std::min(10.0, std::max(3.0, slot * 0.075))
                   : std::min(14.0, std::max(2.0, slot * 0.18)))
        : 4.0;

    std::vector<double> gaps;
    gaps.reserve(n > 0 ? n - 1 : 0);
    for (std::size_t i = 1; i < n; ++i) {
        double gap = baseGap;
        if (nestedX) {
            int shared = 0;
            if (i < layout.sharedPrefixDepthWithPrevious.size()) {
                shared = layout.sharedPrefixDepthWithPrevious[i];
            }
            const int changedLevels = std::max<int>(1, static_cast<int>(nestingDepth) - shared);
            gap = baseGap * (1.0 + 1.45 * static_cast<double>(changedLevels - 1));
        }
        gaps.push_back(gap);
    }

    const double outerPad = layout.widthMode == BarplotWidthMode::Equal ? baseGap / 2.0 : 0.0;
    double totalGap = outerPad * 2.0;
    for (double gap : gaps) {
        totalGap += gap;
    }
    const double availableWidth = std::max(1.0, contentRect.width - totalGap);

    double totalWeight = 0.0;
    if (layout.widthMode != BarplotWidthMode::Equal) {
        for (double value : layout.widthValues) {
            totalWeight += std::max(0.0, value);
        }
        if (!(totalWeight > 0.0)) {
            totalWeight = static_cast<double>(n);
        }
    }

    auto widthForIndex = [&](std::size_t index) -> double {
        if (layout.widthMode == BarplotWidthMode::Equal) {
            return availableWidth / static_cast<double>(std::max<std::size_t>(1, n));
        }
        double weight = index < layout.widthValues.size() ? layout.widthValues[index] : 0.0;
        double width = availableWidth * std::max(0.0, weight) / totalWeight;
        if (!(width > 0.0)) {
            width = availableWidth / static_cast<double>(std::max<std::size_t>(1, n));
        }
        return width;
    };

    double x = contentRect.x + outerPad;
    for (std::size_t i = 0; i < n; ++i) {
        const double width = widthForIndex(i);
        const double value = i < layout.values.size() ? layout.values[i] : 0.0;
        const double h = contentRect.height * std::max(0.0, value) / std::max(1.0, layout.yMaximum);
        rects.push_back({
            x,
            contentRect.y + contentRect.height - h,
            std::max(1.0, width),
            h
        });
        x += width;
        if (i < gaps.size()) {
            x += gaps[i];
        }
    }
    return rects;
}

Rect BarplotBarRect(const BarplotLayout &layout, std::size_t index)
{
    std::vector<Rect> rects = BarplotBarRects(layout);
    if (index >= rects.size()) {
        return {};
    }
    return rects[index];
}

std::optional<std::size_t> BarplotBarIndexAtPoint(const BarplotLayout &layout,
                                                  const Point &point)
{
    if (!IsValidBarplotLayout(layout) ||
        !std::isfinite(point.x) ||
        !std::isfinite(point.y) ||
        !PointInRect(point, layout.plotRect)) {
        return std::nullopt;
    }
    std::vector<Rect> rects = BarplotBarRects(layout);
    for (std::size_t i = 0; i < rects.size(); ++i) {
        const Rect &rect = rects[i];
        if (point.x >= rect.x && point.x <= rect.x + rect.width) {
            return i;
        }
    }
    return std::nullopt;
}

std::optional<std::pair<std::size_t, std::size_t>> BarplotBarRangeForGesture(
    const BarplotLayout &layout,
    const Point &start,
    const Point &current)
{
    std::optional<std::size_t> first = BarplotBarIndexAtPoint(layout, start);
    std::optional<std::size_t> last = BarplotBarIndexAtPoint(layout, current);
    if (!first.has_value() || !last.has_value()) {
        return std::nullopt;
    }
    return std::make_pair(std::min(*first, *last), std::max(*first, *last));
}

std::optional<Rect> BarplotDragHighlightRect(const BarplotLayout &layout,
                                             const Point &start,
                                             const Point &current)
{
    std::optional<std::pair<std::size_t, std::size_t>> range =
        BarplotBarRangeForGesture(layout, start, current);
    if (!range.has_value()) {
        return std::nullopt;
    }

    std::vector<Rect> rects = BarplotBarRects(layout);
    if (range->first >= rects.size() || range->second >= rects.size()) {
        return std::nullopt;
    }

    const Rect &first = rects[range->first];
    const Rect &last = rects[range->second];
    const double left = std::min(first.x, last.x);
    const double right = std::max(first.x + first.width, last.x + last.width);
    return Rect{left, layout.plotRect.y, right - left, layout.plotRect.height};
}

std::set<int> BarplotRowsForBarRange(const std::vector<std::vector<int>> &rowsByBar,
                                     std::size_t first,
                                     std::size_t last)
{
    std::set<int> rows;
    if (rowsByBar.empty()) {
        return rows;
    }
    if (first > last) {
        std::swap(first, last);
    }
    if (first >= rowsByBar.size()) {
        return rows;
    }
    last = std::min(last, rowsByBar.size() - 1);
    for (std::size_t i = first; i <= last; ++i) {
        rows.insert(rowsByBar[i].begin(), rowsByBar[i].end());
    }
    return rows;
}

Rect BarplotSegmentRect(const Rect &barRect,
                        int segmentCount,
                        int barCount,
                        double *cursor)
{
    if (!cursor || !IsValidRect(barRect)) {
        return {};
    }
    const double h = barCount > 0
        ? barRect.height * static_cast<double>(std::max(0, segmentCount)) / static_cast<double>(barCount)
        : 0.0;
    *cursor -= h;
    return {barRect.x, *cursor, barRect.width, h};
}

std::vector<Rect> BarplotSegmentRects(const Rect &barRect,
                                      const std::vector<int> &segmentCounts,
                                      int barCount)
{
    std::vector<Rect> rects;
    if (!IsValidRect(barRect)) {
        return rects;
    }
    rects.reserve(segmentCounts.size());
    double cursor = barRect.y + barRect.height;
    for (int count : segmentCounts) {
        rects.push_back(BarplotSegmentRect(barRect, count, barCount, &cursor));
    }
    return rects;
}

std::vector<BarplotSegmentGeometry> BarplotSegmentGeometryForLevels(
    const Rect &barRect,
    const std::vector<std::string> &orderedLevels,
    const std::map<std::string, int> &countsByLevel,
    int barCount)
{
    std::vector<BarplotSegmentGeometry> geometry;
    if (!IsValidRect(barRect)) {
        return geometry;
    }

    geometry.reserve(orderedLevels.size());
    double cursor = barRect.y + barRect.height;
    for (const std::string &level : orderedLevels) {
        auto it = countsByLevel.find(level);
        if (it == countsByLevel.end()) {
            continue;
        }
        BarplotSegmentGeometry item;
        item.level = level;
        item.count = it->second;
        item.rect = BarplotSegmentRect(barRect, item.count, barCount, &cursor);
        geometry.push_back(item);
    }
    return geometry;
}

std::optional<BarplotSegmentGeometry> BarplotSegmentGeometryAtPoint(
    const Rect &barRect,
    const std::vector<std::string> &orderedLevels,
    const std::map<std::string, int> &countsByLevel,
    int barCount,
    const Point &point)
{
    if (!PointInRect(point, barRect)) {
        return std::nullopt;
    }
    std::vector<BarplotSegmentGeometry> geometry = BarplotSegmentGeometryForLevels(
        barRect,
        orderedLevels,
        countsByLevel,
        barCount);
    for (const BarplotSegmentGeometry &item : geometry) {
        if (PointInRect(point, item.rect)) {
            return item;
        }
    }
    return std::nullopt;
}

std::optional<BarplotSegmentHit> BarplotSegmentAtPoint(
    const BarplotLayout &layout,
    const std::vector<BarplotBinSummary> &bins,
    const std::vector<std::string> &orderedLevels,
    bool hasSplit,
    const Point &point)
{
    if (!hasSplit) {
        return std::nullopt;
    }
    std::optional<std::size_t> barIndex = BarplotBarIndexAtPoint(layout, point);
    if (!barIndex.has_value() || *barIndex >= bins.size()) {
        return std::nullopt;
    }
    const BarplotBinSummary &bin = bins[*barIndex];
    if (bin.segments.empty()) {
        return std::nullopt;
    }

    std::map<std::string, int> countsByLevel;
    for (const BarplotSegmentSummary &segment : bin.segments) {
        countsByLevel[segment.level] = segment.count;
    }
    std::optional<BarplotSegmentGeometry> geometry = BarplotSegmentGeometryAtPoint(
        BarplotBarRect(layout, *barIndex),
        orderedLevels,
        countsByLevel,
        std::max(1, bin.n),
        point);
    if (!geometry.has_value()) {
        return std::nullopt;
    }

    for (std::size_t segmentIndex = 0; segmentIndex < bin.segments.size(); ++segmentIndex) {
        if (bin.segments[segmentIndex].level == geometry->level) {
            BarplotSegmentHit hit;
            hit.barIndex = *barIndex;
            hit.segmentIndex = segmentIndex;
            hit.level = geometry->level;
            hit.rect = geometry->rect;
            return hit;
        }
    }
    return std::nullopt;
}

std::vector<int> BarplotRowsAtPoint(
    const BarplotLayout &layout,
    const std::vector<BarplotBinSummary> &bins,
    const std::vector<std::string> &orderedLevels,
    bool hasSplit,
    const Point &point)
{
    std::optional<std::size_t> barIndex = BarplotBarIndexAtPoint(layout, point);
    if (!barIndex.has_value() || *barIndex >= bins.size()) {
        return {};
    }
    const BarplotBinSummary &bin = bins[*barIndex];
    Rect barRect = BarplotBarRect(layout, *barIndex);
    if (!PointInRect(point, barRect)) {
        return bin.rows;
    }
    std::optional<BarplotSegmentHit> segmentHit =
        BarplotSegmentAtPoint(layout, bins, orderedLevels, hasSplit, point);
    if (segmentHit.has_value() && segmentHit->segmentIndex < bin.segments.size()) {
        return bin.segments[segmentHit->segmentIndex].rows;
    }
    return bin.rows;
}

std::set<int> BarplotRowsForGesture(
    const BarplotLayout &layout,
    const std::vector<BarplotBinSummary> &bins,
    const std::vector<std::string> &orderedLevels,
    bool hasSplit,
    const Point &start,
    const Point &current,
    double minimumWidth,
    double minimumHeight)
{
    std::set<int> rows;
    std::optional<std::pair<std::size_t, std::size_t>> range =
        BarplotBarRangeForGesture(layout, start, current);
    if (!range.has_value()) {
        return rows;
    }

    Rect gestureRect = RectBetweenPoints(start, current);
    if (!RectMeetsMinimumSize(gestureRect, minimumWidth, minimumHeight)) {
        std::vector<int> pointRows = BarplotRowsAtPoint(
            layout,
            bins,
            orderedLevels,
            hasSplit,
            current);
        rows.insert(pointRows.begin(), pointRows.end());
        return rows;
    }

    std::vector<std::vector<int>> rowsByBar;
    rowsByBar.reserve(bins.size());
    for (const BarplotBinSummary &bin : bins) {
        rowsByBar.push_back(bin.rows);
    }
    return BarplotRowsForBarRange(rowsByBar, range->first, range->second);
}

Rect BarplotCategoryLabelRect(const Rect &plotRect,
                              const Rect &barRect,
                              std::size_t componentCount,
                              double verticalOffset)
{
    if (!IsValidRect(plotRect) || !IsValidRect(barRect)) {
        return {};
    }
    const std::size_t count = std::max<std::size_t>(1, componentCount);
    const double height = std::max(30.0, 13.0 * static_cast<double>(count));
    return {
        barRect.x + 2.0,
        plotRect.y + plotRect.height + verticalOffset,
        std::max(8.0, barRect.width - 4.0),
        height
    };
}

Rect BarplotCategoryComponentRect(const Rect &labelRect,
                                  std::size_t componentCount,
                                  std::size_t componentIndex)
{
    if (!IsValidRect(labelRect)) {
        return {};
    }
    const std::size_t count = std::max<std::size_t>(1, componentCount);
    if (componentIndex >= count) {
        return {};
    }
    const double lineHeight = labelRect.height / static_cast<double>(count);
    return {
        labelRect.x,
        labelRect.y + lineHeight * static_cast<double>(componentIndex),
        labelRect.width,
        lineHeight
    };
}

static std::vector<int> SortedUniqueRows(std::vector<int> rows)
{
    std::sort(rows.begin(), rows.end());
    rows.erase(std::unique(rows.begin(), rows.end()), rows.end());
    return rows;
}

std::optional<BarplotCategoryComponentHit> BarplotCategoryComponentAtPoint(
    const BarplotLayout &layout,
    const std::vector<BarplotBinSummary> &bins,
    const std::vector<std::string> &xVariables,
    const Point &point,
    double verticalOffset,
    double labelHitPaddingX,
    double labelHitPaddingY,
    double componentHitPaddingX,
    double componentHitPaddingY)
{
    if (!IsValidBarplotLayout(layout) || bins.empty()) {
        return std::nullopt;
    }

    const std::size_t componentCount = std::max<std::size_t>(1, xVariables.size());
    std::vector<Rect> bars = BarplotBarRects(layout);
    const std::size_t count = std::min(bins.size(), bars.size());
    for (std::size_t bar = 0; bar < count; ++bar) {
        Rect labelRect = BarplotCategoryLabelRect(layout.plotRect, bars[bar], componentCount, verticalOffset);
        if (!PointInRect(point, ExpandRect(labelRect, labelHitPaddingX, labelHitPaddingY))) {
            continue;
        }
        std::vector<std::string> components = BarplotCategoryComponents(xVariables, bins[bar].category);
        for (std::size_t componentIndex = 0; componentIndex < componentCount; ++componentIndex) {
            Rect componentRect = BarplotCategoryComponentRect(labelRect, componentCount, componentIndex);
            if (!PointInRect(point, ExpandRect(componentRect, componentHitPaddingX, componentHitPaddingY))) {
                continue;
            }
            BarplotCategoryComponentHit hit;
            hit.barIndex = bar;
            hit.variableIndex = componentIndex;
            hit.value = componentIndex < components.size() ? components[componentIndex] : bins[bar].category;
            hit.labelRect = labelRect;
            hit.componentRect = componentRect;
            return hit;
        }
    }
    return std::nullopt;
}

std::vector<int> BarplotRowsForXVariableValue(
    const std::vector<BarplotBinSummary> &bins,
    const std::vector<std::string> &xVariables,
    std::size_t variableIndex,
    const std::string &value)
{
    std::vector<int> rows;
    for (const BarplotBinSummary &bin : bins) {
        std::vector<std::string> components = BarplotCategoryComponents(xVariables, bin.category);
        if (variableIndex < components.size() && components[variableIndex] == value) {
            rows.insert(rows.end(), bin.rows.begin(), bin.rows.end());
        }
    }
    return SortedUniqueRows(std::move(rows));
}

std::vector<BarplotCategoryLabelDrawItem> BuildBarplotCategoryLabelDrawPlan(
    const BarplotLayout &layout,
    const std::vector<BarplotBinSummary> &bins,
    const std::vector<std::string> &xVariables,
    double verticalOffset)
{
    std::vector<BarplotCategoryLabelDrawItem> items;
    if (!IsValidBarplotLayout(layout) || bins.empty()) {
        return items;
    }

    const std::size_t componentCount = std::max<std::size_t>(1, xVariables.size());
    std::vector<Rect> bars = BarplotBarRects(layout);
    const std::size_t count = std::min(bins.size(), bars.size());
    for (std::size_t bar = 0; bar < count; ++bar) {
        Rect labelRect = BarplotCategoryLabelRect(layout.plotRect, bars[bar], componentCount, verticalOffset);
        if (xVariables.size() <= 1) {
            items.push_back(BarplotCategoryLabelDrawItem{bar, 0, labelRect, bins[bar].category});
            continue;
        }
        std::vector<std::string> components = BarplotCategoryComponents(xVariables, bins[bar].category);
        for (std::size_t componentIndex = 0; componentIndex < xVariables.size(); ++componentIndex) {
            std::string value = componentIndex < components.size() ? components[componentIndex] : "";
            std::string label = xVariables[componentIndex] + "=" + value;
            items.push_back(BarplotCategoryLabelDrawItem{
                bar,
                componentIndex,
                BarplotCategoryComponentRect(labelRect, componentCount, componentIndex),
                label
            });
        }
    }
    return items;
}

Rect BarplotSplitLevelLabelRect(const Rect &plotRect,
                                std::size_t index,
                                const std::vector<int> &totals,
                                double left,
                                double rightPadding,
                                double height)
{
    if (!IsValidRect(plotRect) || index >= totals.size()) {
        return {};
    }
    const double right = plotRect.x - rightPadding;
    const double width = std::max(42.0, right - left);

    int total = 0;
    for (int value : totals) {
        total += std::max(0, value);
    }

    double cumulative = 0.0;
    for (std::size_t i = 0; i < index; ++i) {
        const double share = total > 0
            ? static_cast<double>(std::max(0, totals[i])) / static_cast<double>(total)
            : 1.0 / static_cast<double>(std::max<std::size_t>(1, totals.size()));
        cumulative += share;
    }

    const double share = total > 0
        ? static_cast<double>(std::max(0, totals[index])) / static_cast<double>(total)
        : 1.0 / static_cast<double>(std::max<std::size_t>(1, totals.size()));
    const double centerFraction = std::min(1.0, std::max(0.0, cumulative + share / 2.0));
    const double centerY = plotRect.y + plotRect.height - centerFraction * plotRect.height;
    const double y = std::min(plotRect.y + plotRect.height - height,
                              std::max(plotRect.y, centerY - height / 2.0));
    return {left, y, width, height};
}

std::vector<std::string> BarplotSplitLevelsForBins(
    const std::vector<BarplotBinSummary> &bins,
    bool hasSplit)
{
    std::vector<std::string> levels;
    if (!hasSplit) {
        return levels;
    }
    std::set<std::string> seen;
    for (const BarplotBinSummary &bin : bins) {
        for (const BarplotSegmentSummary &segment : bin.segments) {
            if (seen.insert(segment.level).second) {
                levels.push_back(segment.level);
            }
        }
    }
    if (levels.size() > 1) {
        levels = CanonicalLevelOrder(levels);
    }
    return levels;
}

std::size_t BarplotLevelIndex(const std::vector<std::string> &levels,
                              const std::string &level,
                              std::size_t fallbackIndex)
{
    auto found = std::find(levels.begin(), levels.end(), level);
    if (found == levels.end()) {
        return fallbackIndex;
    }
    return static_cast<std::size_t>(std::distance(levels.begin(), found));
}

std::map<std::string, int> BarplotSplitLevelTotalsForBins(
    const std::vector<BarplotBinSummary> &bins,
    const std::vector<std::string> &levels)
{
    std::map<std::string, int> totals;
    for (const std::string &level : levels) {
        totals[level] = 0;
    }
    for (const BarplotBinSummary &bin : bins) {
        for (const BarplotSegmentSummary &segment : bin.segments) {
            totals[segment.level] += static_cast<int>(segment.rows.size());
        }
    }
    return totals;
}

BarplotSideLabelDrawPlan BuildBarplotSideLabelDrawPlan(
    const Rect &plotRect,
    const std::string &splitVariable,
    const std::vector<std::string> &levels,
    const std::map<std::string, int> &totals,
    bool showCompositionStrip)
{
    BarplotSideLabelDrawPlan plan;
    if (!IsValidRect(plotRect) || levels.empty()) {
        return plan;
    }

    // The title uses the full left gutter.  The former 46 px reservation was
    // appropriate for the category labels, but truncated ordinary variable
    // names to just "Split:" even in a wide plot window.
    const double sideWidth = std::max(42.0, plotRect.x - 16.0);
    plan.splitTitle = "Split: " + splitVariable;
    plan.splitTitleRect = {8.0, plotRect.y - 18.0, sideWidth, 14.0};

    std::vector<int> orderedTotals;
    orderedTotals.reserve(levels.size());
    for (const std::string &level : levels) {
        auto it = totals.find(level);
        orderedTotals.push_back(it == totals.end() ? 0 : it->second);
    }
    plan.splitLabels.reserve(levels.size());
    for (std::size_t i = 0; i < levels.size(); ++i) {
        plan.splitLabels.push_back(BarplotCategoryLabelDrawItem{
            i,
            0,
            BarplotSplitLevelLabelRect(plotRect, i, orderedTotals),
            levels[i]
        });
    }

    plan.showRowStripLabel = showCompositionStrip;
    if (showCompositionStrip) {
        plan.rowStripLabel = "Row strip";
        plan.rowStripLabelRect = {
            8.0,
            plotRect.y + plotRect.height + 4.0,
            sideWidth,
            14.0
        };
    }
    return plan;
}

std::optional<std::size_t> BarplotSplitLevelLabelIndexAtPoint(
    const Rect &plotRect,
    const std::vector<std::string> &levels,
    const std::map<std::string, int> &totalsByLevel,
    const Point &point,
    double labelHitPaddingX,
    double labelHitPaddingY)
{
    if (levels.empty() || !IsValidRect(plotRect)) {
        return std::nullopt;
    }
    std::vector<int> orderedTotals;
    orderedTotals.reserve(levels.size());
    for (const std::string &level : levels) {
        auto it = totalsByLevel.find(level);
        orderedTotals.push_back(it == totalsByLevel.end() ? 0 : it->second);
    }
    for (std::size_t i = 0; i < levels.size(); ++i) {
        Rect rect = ExpandRect(BarplotSplitLevelLabelRect(plotRect, i, orderedTotals),
                               labelHitPaddingX,
                               labelHitPaddingY);
        if (PointInRect(point, rect)) {
            return i;
        }
    }
    return std::nullopt;
}

std::vector<int> BarplotRowsForSplitLevel(
    const std::vector<BarplotBinSummary> &bins,
    const std::string &level)
{
    std::vector<int> rows;
    for (const BarplotBinSummary &bin : bins) {
        for (const BarplotSegmentSummary &segment : bin.segments) {
            if (segment.level == level) {
                rows.insert(rows.end(), segment.rows.begin(), segment.rows.end());
            }
        }
    }
    return SortedUniqueRows(std::move(rows));
}

std::string BarplotPatternAtIndex(std::size_t index)
{
    static const std::vector<std::string> patterns = {
        "diagonal_slash",
        "dots",
        "crosshatch",
        "vertical",
        "horizontal",
        "diagonal_backslash"
    };
    return patterns.empty() ? "none" : patterns[index % patterns.size()];
}

bool BarplotPatternIsKnown(const std::string &pattern)
{
    return pattern == "none" ||
        pattern == "diagonal_slash" ||
        pattern == "diagonal_backslash" ||
        pattern == "crosshatch" ||
        pattern == "dots" ||
        pattern == "vertical" ||
        pattern == "horizontal";
}

std::string BarplotPatternDisplayName(const std::string &pattern)
{
    if (pattern == "diagonal_slash") return "Diagonal Slash";
    if (pattern == "diagonal_backslash") return "Diagonal Backslash";
    if (pattern == "crosshatch") return "Crosshatch";
    if (pattern == "dots") return "Dots";
    if (pattern == "vertical") return "Vertical";
    if (pattern == "horizontal") return "Horizontal";
    if (pattern == "none") return "None";
    return pattern;
}

std::string BarplotSegmentOverrideKey(const std::string &xCondition,
                                      const std::string &level)
{
    return xCondition + "\x1f" + level;
}

double ClampUnitInterval(double value)
{
    if (!std::isfinite(value)) {
        return 0.0;
    }
    return std::max(0.0, std::min(1.0, value));
}

double ClampBarplotSplitStrokeWidth(double value)
{
    if (!std::isfinite(value)) {
        return 3.0;
    }
    if (value < 1.0) {
        return 1.0;
    }
    if (value > 12.0) {
        return 12.0;
    }
    return value;
}

static std::optional<double> ParseFiniteDouble(const std::string &text)
{
    std::string trimmed = TrimCopy(text);
    if (trimmed.empty()) {
        return std::nullopt;
    }
    char *end = nullptr;
    double value = std::strtod(trimmed.c_str(), &end);
    if (!end || *end != '\0' || !std::isfinite(value)) {
        return std::nullopt;
    }
    return value;
}

std::optional<double> ParseBarplotSplitStrokeWidthCommandValue(const std::string &text)
{
    std::optional<double> value = ParseFiniteDouble(text);
    if (!value.has_value()) {
        return std::nullopt;
    }
    return ClampBarplotSplitStrokeWidth(*value);
}

std::optional<double> ParseBarplotSegmentAlphaCommandValue(const std::string &text)
{
    std::optional<double> value = ParseFiniteDouble(text);
    if (!value.has_value()) {
        return std::nullopt;
    }
    return ClampUnitInterval(*value);
}

bool BarplotSegmentEncodingModeIsValid(const std::string &encodingMode)
{
    return encodingMode == "transparent_color_pattern" ||
        encodingMode == "transparent_color_only" ||
        encodingMode == "pattern_only";
}

std::string NormalizedBarplotSegmentEncodingMode(const std::string &encodingMode)
{
    return BarplotSegmentEncodingModeIsValid(encodingMode)
        ? encodingMode
        : "transparent_color_pattern";
}

bool BarplotEncodingShowsPatterns(bool showPatterns,
                                  const std::string &encodingMode)
{
    if (!showPatterns) {
        return false;
    }
    return encodingMode != "transparent_color_only";
}

bool BarplotEncodingShowsColor(const std::string &encodingMode)
{
    return encodingMode != "pattern_only";
}

bool BarplotSelectionDisplayShowsOverlay(const std::string &selectionDisplay)
{
    return selectionDisplay == "overlay" || selectionDisplay == "overlay_outline";
}

bool BarplotSelectionDisplayShowsOutline(const std::string &selectionDisplay)
{
    return selectionDisplay == "outline" || selectionDisplay == "overlay_outline";
}

bool BarplotLevelIsMissing(const std::string &level)
{
    return level.empty() || level == "NA" || level == "NaN";
}

bool ParseBarplotSegmentCommandPayload(const std::string &payload,
                                       int *barIndex,
                                       int *segmentIndex,
                                       std::string *tail)
{
    if (!barIndex || !segmentIndex) {
        return false;
    }
    std::size_t p1 = payload.find('|');
    if (p1 == std::string::npos) {
        return false;
    }
    std::size_t p2 = payload.find('|', p1 + 1);
    std::string first = payload.substr(0, p1);
    std::string second = p2 == std::string::npos
        ? payload.substr(p1 + 1)
        : payload.substr(p1 + 1, p2 - p1 - 1);
    char *end1 = nullptr;
    char *end2 = nullptr;
    long bar = std::strtol(first.c_str(), &end1, 10);
    long segment = std::strtol(second.c_str(), &end2, 10);
    if (!end1 || *end1 != '\0' || !end2 || *end2 != '\0' ||
        bar < 0 || segment < 0) {
        return false;
    }
    *barIndex = static_cast<int>(bar);
    *segmentIndex = static_cast<int>(segment);
    if (tail) {
        *tail = p2 == std::string::npos ? "" : payload.substr(p2 + 1);
    }
    return true;
}

std::vector<std::string> OrderedUniqueBarplotLevels(const std::vector<std::string> &levels)
{
    std::vector<std::string> out;
    std::set<std::string> seen;
    for (const std::string &level : levels) {
        if (seen.insert(level).second) {
            out.push_back(level);
        }
    }
    return out;
}

std::set<std::string> BarplotSegmentOverrideKeys(
    const std::vector<std::pair<std::string, std::string>> &segments)
{
    std::set<std::string> keys;
    for (const auto &segment : segments) {
        keys.insert(BarplotSegmentOverrideKey(segment.first, segment.second));
    }
    return keys;
}

std::vector<std::string> SplitBarplotXLabel(const std::string &label)
{
    std::vector<std::string> out;
    std::size_t start = 0;
    while (start <= label.size()) {
        std::size_t pos = label.find(" + ", start);
        std::string part = TrimCopy(label.substr(start, pos == std::string::npos ? std::string::npos : pos - start));
        if (!part.empty() && std::find(out.begin(), out.end(), part) == out.end()) {
            out.push_back(part);
        }
        if (pos == std::string::npos) {
            break;
        }
        start = pos + 3;
    }
    return out;
}

std::vector<std::string> BarplotXVariablesForLabels(
    const std::vector<std::string> &barplotXVariables,
    const std::string &fallbackXLabel)
{
    std::vector<std::string> out;
    for (const std::string &name : barplotXVariables) {
        std::string trimmed = TrimCopy(name);
        if (!trimmed.empty() && std::find(out.begin(), out.end(), trimmed) == out.end()) {
            out.push_back(trimmed);
        }
    }
    if (out.empty() && !fallbackXLabel.empty()) {
        out = SplitBarplotXLabel(fallbackXLabel);
    }
    return out;
}

std::vector<std::string> NormalizedBarplotXVariables(
    const std::vector<std::string> &barplotXVariables,
    const std::string &fallbackXLabel)
{
    return BarplotXVariablesForLabels(barplotXVariables, fallbackXLabel);
}

std::vector<std::string> SplitBarplotConditionLabel(const std::string &label)
{
    std::vector<std::string> out;
    std::size_t start = 0;
    while (start <= label.size()) {
        std::size_t pos = label.find("; ", start);
        out.push_back(label.substr(start, pos == std::string::npos ? std::string::npos : pos - start));
        if (pos == std::string::npos) {
            break;
        }
        start = pos + 2;
    }
    return out;
}

std::vector<std::string> BarplotCategoryComponents(
    const std::vector<std::string> &barplotXVariables,
    const std::string &category)
{
    std::vector<std::string> xVariables = BarplotXVariablesForLabels(barplotXVariables, "");
    if (xVariables.size() <= 1) {
        return std::vector<std::string>{category};
    }
    std::vector<std::string> parts = SplitBarplotConditionLabel(category);
    std::vector<std::string> out;
    for (std::size_t i = 0; i < xVariables.size(); ++i) {
        std::string value = i < parts.size() ? TrimCopy(parts[i]) : "";
        std::string prefix = xVariables[i] + "=";
        if (value.rfind(prefix, 0) == 0) {
            value = value.substr(prefix.size());
        }
        out.push_back(value);
    }
    return out;
}

std::string GeneratedBarplotTitle(const std::string &xLabel,
                                  const std::string &splitVariable)
{
    std::string label = xLabel.empty() ? "variable" : xLabel;
    if (splitVariable.empty()) {
        return "Bar chart of " + label;
    }
    return "Bar chart of " + label + " by " + splitVariable;
}

bool BarplotTitleIsAutoGenerated(const std::string &title)
{
    return title.empty() || title.rfind("Bar chart of ", 0) == 0;
}

std::string RefreshedBarplotTitle(const std::string &currentTitle,
                                  const std::string &xLabel,
                                  const std::string &splitVariable)
{
    return BarplotTitleIsAutoGenerated(currentTitle)
        ? GeneratedBarplotTitle(xLabel, splitVariable)
        : currentTitle;
}

std::string BarplotHeightScaleLabel(const std::string &mode)
{
    if (mode == "conditional_percent") return "% within X";
    if (mode == "overall_percent") return "% of total";
    return "Counts";
}

std::string BarplotWidthScaleLabel(const std::string &widthMode)
{
    if (widthMode == "proportional_n") return "proportional to N";
    if (widthMode == "proportional_percent") return "proportional to % of total";
    return "equal";
}

std::string BarplotSegmentEncodingLabel(bool hasSplit,
                                        const std::string &segmentEncodingMode)
{
    if (hasSplit) return "solid color + border";
    (void)segmentEncodingMode;
    return "transparent color";
}

std::string BarplotSubtitle(const std::string &mode,
                            const std::string &widthMode,
                            const std::string &splitVariable,
                            double splitStrokeWidth,
                            const std::string &segmentEncodingMode)
{
    std::ostringstream out;
    out << "Height: " << BarplotHeightScaleLabel(mode)
        << " | Width: " << BarplotWidthScaleLabel(widthMode)
        << " | Split: " << (splitVariable.empty() ? "none" : splitVariable)
        << " | Bar border: " << std::fixed << std::setprecision(1)
        << ClampBarplotSplitStrokeWidth(splitStrokeWidth) << " px"
        << " | Segment encoding: "
        << BarplotSegmentEncodingLabel(!splitVariable.empty(), segmentEncodingMode);
    return out.str();
}

int BarplotSharedCategoryPrefixDepth(
    const std::vector<std::string> &barplotXVariables,
    const std::string &left,
    const std::string &right)
{
    std::vector<std::string> a = BarplotCategoryComponents(barplotXVariables, left);
    std::vector<std::string> b = BarplotCategoryComponents(barplotXVariables, right);
    int shared = 0;
    std::size_t count = std::min(a.size(), b.size());
    for (std::size_t i = 0; i < count; ++i) {
        if (a[i] != b[i]) {
            break;
        }
        ++shared;
    }
    return shared;
}

namespace {

bool NaturalLess(const std::string &a, const std::string &b)
{
    std::size_t i = 0, j = 0;
    while (i < a.size() && j < b.size()) {
        if (std::isdigit(static_cast<unsigned char>(a[i])) && std::isdigit(static_cast<unsigned char>(b[j]))) {
            std::size_t iEnd = i;
            while (iEnd < a.size() && std::isdigit(static_cast<unsigned char>(a[iEnd]))) iEnd++;
            std::size_t jEnd = j;
            while (jEnd < b.size() && std::isdigit(static_cast<unsigned char>(b[jEnd]))) jEnd++;
            std::string numA = a.substr(i, iEnd - i);
            std::string numB = b.substr(j, jEnd - j);
            std::string trimA = numA, trimB = numB;
            trimA.erase(0, trimA.find_first_not_of('0'));
            trimB.erase(0, trimB.find_first_not_of('0'));
            if (trimA.empty()) trimA = "0";
            if (trimB.empty()) trimB = "0";
            if (trimA.size() != trimB.size()) return trimA.size() < trimB.size();
            if (numA != numB) return numA < numB;
            i = iEnd;
            j = jEnd;
        } else {
            const int ca = std::tolower(static_cast<unsigned char>(a[i]));
            const int cb = std::tolower(static_cast<unsigned char>(b[j]));
            if (ca != cb) return ca < cb;
            i++;
            j++;
        }
    }
    return i == a.size() && j < b.size();
}

} // anonymous namespace

std::vector<std::string> CanonicalLevelOrder(const std::vector<std::string> &values)
{
    if (values.size() <= 1) return values;
    std::vector<std::string> valid, missing;
    bool allNumeric = true;
    for (const std::string &v : values) {
        if (v.empty() || DataCellIsMissing(v)) {
            missing.push_back(v);
        } else {
            valid.push_back(v);
            if (allNumeric) {
                char *end = nullptr;
                std::strtod(v.c_str(), &end);
                if (!end || end == v.c_str() || *end != '\0') allNumeric = false;
            }
        }
    }
    if (allNumeric && valid.size() > 1) {
        std::sort(valid.begin(), valid.end(), [](const std::string &a, const std::string &b) {
            return std::strtod(a.c_str(), nullptr) < std::strtod(b.c_str(), nullptr);
        });
    } else if (valid.size() > 1) {
        std::sort(valid.begin(), valid.end(), [](const std::string &a, const std::string &b) {
            return NaturalLess(a, b);
        });
    }
    valid.insert(valid.end(), missing.begin(), missing.end());
    return valid;
}

std::vector<std::size_t> BarplotCategorySortOrder(
    const std::vector<std::string> &barplotXVariables,
    const std::vector<std::string> &categories)
{
    std::vector<std::size_t> order(categories.size());
    std::iota(order.begin(), order.end(), 0);

    std::vector<std::string> xVariables = BarplotXVariablesForLabels(barplotXVariables, "");
    if (categories.size() <= 1) {
        return order;
    }

    const std::size_t depth = std::max<std::size_t>(xVariables.size(), 1);
    std::vector<std::vector<std::string>> componentsByIndex;
    componentsByIndex.reserve(categories.size());
    std::vector<std::vector<std::string>> levelOrders(depth);
    for (const std::string &category : categories) {
        std::vector<std::string> components = BarplotCategoryComponents(xVariables, category);
        componentsByIndex.push_back(components);
        for (std::size_t vi = 0; vi < depth; ++vi) {
            std::string value = vi < components.size() ? components[vi] : "";
            if (std::find(levelOrders[vi].begin(), levelOrders[vi].end(), value) == levelOrders[vi].end()) {
                levelOrders[vi].push_back(value);
            }
        }
    }

    for (std::vector<std::string> &levels : levelOrders) {
        levels = CanonicalLevelOrder(levels);
    }

    auto rankValue = [&](std::size_t variableIndex, const std::string &value) -> std::size_t {
        if (variableIndex >= levelOrders.size()) {
            return 0;
        }
        const std::vector<std::string> &levels = levelOrders[variableIndex];
        auto found = std::find(levels.begin(), levels.end(), value);
        return found == levels.end()
            ? levels.size() + 1
            : static_cast<std::size_t>(std::distance(levels.begin(), found));
    };

    std::stable_sort(order.begin(), order.end(), [&](std::size_t ai, std::size_t bi) {
        const std::vector<std::string> &av = componentsByIndex[ai];
        const std::vector<std::string> &bv = componentsByIndex[bi];
        for (std::size_t vi = 0; vi < depth; ++vi) {
            std::size_t ar = rankValue(vi, vi < av.size() ? av[vi] : "");
            std::size_t br = rankValue(vi, vi < bv.size() ? bv[vi] : "");
            if (ar != br) {
                return ar < br;
            }
        }
        return ai < bi;
    });
    return order;
}

std::string BarplotCategoryLabelForValues(
    const std::vector<std::string> &barplotXVariables,
    const std::vector<std::string> &values)
{
    if (barplotXVariables.size() == 1) {
        return values.empty() ? "NA" : values[0];
    }
    std::vector<std::string> parts;
    for (std::size_t i = 0; i < barplotXVariables.size(); ++i) {
        std::string name = TrimCopy(barplotXVariables[i]);
        if (name.empty()) {
            continue;
        }
        std::string value = i < values.size() ? values[i] : "NA";
        parts.push_back(name + "=" + value);
    }
    return parts.empty() ? "NA" : JoinDisplayValues(parts, "; ");
}

BarplotBuildResult BuildBarplotBins(const BarplotBuildInput &input)
{
    BarplotBuildResult result;
    std::vector<std::string> xVariables = BarplotXVariablesForLabels(input.xVariables, "");
    if (xVariables.empty()) {
        xVariables.push_back("x");
    }

    std::map<std::string, std::size_t> binIndex;
    std::map<std::string, std::map<std::string, std::size_t>> segmentIndex;
    for (const BarplotInputRow &row : input.rows) {
        std::vector<std::string> values = row.xValues;
        if (values.empty()) {
            values.push_back("NA");
        }
        while (values.size() < xVariables.size()) {
            values.push_back("NA");
        }

        std::string category = BarplotCategoryLabelForValues(xVariables, values);
        std::string level = row.splitLevel.empty() ? "NA" : row.splitLevel;
        auto foundBin = binIndex.find(category);
        if (foundBin == binIndex.end()) {
            binIndex[category] = result.bins.size();
            BarplotBinSummary bin;
            bin.category = category;
            result.bins.push_back(bin);
            foundBin = binIndex.find(category);
        }

        BarplotBinSummary &bin = result.bins[foundBin->second];
        bin.rows.push_back(row.row);
        auto segmentFound = segmentIndex[category].find(level);
        if (segmentFound == segmentIndex[category].end()) {
            segmentIndex[category][level] = bin.segments.size();
            BarplotSegmentSummary segment;
            segment.level = level;
            bin.segments.push_back(segment);
            segmentFound = segmentIndex[category].find(level);
        }
        bin.segments[segmentFound->second].rows.push_back(row.row);
    }

    if (result.bins.size() > 1) {
        std::vector<std::string> categories;
        categories.reserve(result.bins.size());
        for (const BarplotBinSummary &bin : result.bins) {
            categories.push_back(bin.category);
        }
        std::vector<std::size_t> order = BarplotCategorySortOrder(xVariables, categories);
        if (order.size() == result.bins.size()) {
            std::vector<BarplotBinSummary> sorted;
            sorted.reserve(result.bins.size());
            for (std::size_t index : order) {
                if (index < result.bins.size()) {
                    sorted.push_back(result.bins[index]);
                }
            }
            if (sorted.size() == result.bins.size()) {
                result.bins = std::move(sorted);
            }
        }
    }

    for (BarplotBinSummary &bin : result.bins) {
        if (bin.segments.size() > 1) {
            std::vector<std::string> segLevels;
            for (const auto &seg : bin.segments) segLevels.push_back(seg.level);
            std::vector<std::string> canonicalSegLevels = CanonicalLevelOrder(segLevels);
            std::stable_sort(bin.segments.begin(), bin.segments.end(),
                [&](const BarplotSegmentSummary &a, const BarplotSegmentSummary &b) {
                    auto itA = std::find(canonicalSegLevels.begin(), canonicalSegLevels.end(), a.level);
                    auto itB = std::find(canonicalSegLevels.begin(), canonicalSegLevels.end(), b.level);
                    std::size_t rankA = itA != canonicalSegLevels.end()
                        ? static_cast<std::size_t>(std::distance(canonicalSegLevels.begin(), itA))
                        : canonicalSegLevels.size();
                    std::size_t rankB = itB != canonicalSegLevels.end()
                        ? static_cast<std::size_t>(std::distance(canonicalSegLevels.begin(), itB))
                        : canonicalSegLevels.size();
                    return rankA < rankB;
                });
        }
    }
    for (BarplotBinSummary &bin : result.bins) {
        bin.n = static_cast<int>(bin.rows.size());
        result.totalN += bin.n;
    }
    for (BarplotBinSummary &bin : result.bins) {
        bin.percent = result.totalN > 0
            ? 100.0 * static_cast<double>(bin.n) / static_cast<double>(result.totalN)
            : 0.0;
        bin.widthValue = input.widthMode == "proportional_percent"
            ? bin.percent
            : static_cast<double>(bin.n);
        for (BarplotSegmentSummary &segment : bin.segments) {
            segment.count = static_cast<int>(segment.rows.size());
            segment.barN = bin.n;
            segment.totalN = result.totalN;
            segment.conditionalPercent = bin.n > 0
                ? 100.0 * static_cast<double>(segment.count) / static_cast<double>(bin.n)
                : 0.0;
            segment.overallPercent = result.totalN > 0
                ? 100.0 * static_cast<double>(segment.count) / static_cast<double>(result.totalN)
                : 0.0;
            segment.barPercent = bin.percent;
            segment.barWidthValue = bin.widthValue;
        }
    }
    return result;
}

BarplotBuildResult BuildBarplotBinsForColumns(
    const std::vector<const DataColumn *> &xColumns,
    const DataColumn *splitColumn,
    const std::string &widthMode)
{
    BarplotBuildInput input;
    input.widthMode = widthMode;
    for (const DataColumn *xColumn : xColumns) {
        if (xColumn) {
            input.xVariables.push_back(xColumn->name);
        }
    }
    if (xColumns.empty()) {
        return BuildBarplotBins(input);
    }

    std::size_t nRows = xColumns[0] ? xColumns[0]->values.size() : 0;
    for (const DataColumn *xColumn : xColumns) {
        if (xColumn) {
            nRows = std::min(nRows, xColumn->values.size());
        }
    }
    if (splitColumn) {
        nRows = std::min(nRows, splitColumn->values.size());
    }
    for (std::size_t i = 0; i < nRows; ++i) {
        BarplotInputRow row;
        row.row = static_cast<int>(i) + 1;
        row.xValues = DisplayValuesForColumnsAtRow(xColumns, i);
        std::string level = splitColumn ? DisplayValueForCell(*splitColumn, i) : "All";
        if (DataCellIsMissing(level)) {
            level = "NA";
        }
        row.splitLevel = level;
        input.rows.push_back(row);
    }
    return BuildBarplotBins(input);
}

std::vector<std::string> BarplotAvailableVariables(const DataFrameModel &df)
{
    std::vector<std::string> variables;
    for (const DataColumn &col : df.columns) {
        if (!DataColumnLooksLikeId(col)) {
            variables.push_back(col.name);
        }
    }
    return variables;
}

void ApplyBarplotBuildResultToModel(PlotModel &model,
                                    const BarplotBuildResult &result)
{
    model.barplotBins.clear();
    model.barplotExcludedRows.clear();
    model.barplotTotalN = result.totalN;
    model.barplotBins.reserve(result.bins.size());
    for (const BarplotBinSummary &sourceBin : result.bins) {
        BarplotBin bin;
        bin.category = sourceBin.category;
        bin.rows = sourceBin.rows;
        bin.n = sourceBin.n;
        bin.percent = sourceBin.percent;
        bin.widthValue = sourceBin.widthValue;
        for (const BarplotSegmentSummary &sourceSegment : sourceBin.segments) {
            BarplotSegment segment;
            segment.level = sourceSegment.level;
            segment.rows = sourceSegment.rows;
            segment.count = sourceSegment.count;
            segment.barN = sourceSegment.barN;
            segment.totalN = sourceSegment.totalN;
            segment.conditionalPercent = sourceSegment.conditionalPercent;
            segment.overallPercent = sourceSegment.overallPercent;
            segment.barPercent = sourceSegment.barPercent;
            segment.barWidthValue = sourceSegment.barWidthValue;
            bin.segments.push_back(segment);
        }
        model.barplotBins.push_back(bin);
    }
}

void RebuildBarplotBinsForColumns(PlotModel &model,
                                  const std::vector<const DataColumn *> &xColumns,
                                  const DataColumn *splitColumn)
{
    ApplyBarplotBuildResultToModel(
        model,
        BuildBarplotBinsForColumns(xColumns, splitColumn, model.barplotWidthMode));
}

bool RebuildBarplotFromDataFrame(PlotModel &model,
                                 const DataFrameModel &df,
                                 std::string *message)
{
    if (model.kind != "barplot") {
        if (message) {
            *message = BarplotNotAvailableStatus();
        }
        return false;
    }
    NormalizeBarplotXVariables(model);
    if (model.barplotXVariables.empty()) {
        if (message) {
            *message = BarplotChooseXVariableStatus();
        }
        return false;
    }

    std::vector<const DataColumn *> xColumns;
    for (const std::string &name : model.barplotXVariables) {
        const DataColumn *found = FindDataColumnInDataFrame(df, name);
        if (!found) {
            if (message) {
                *message = VariableUnavailableStatus(name);
            }
            return false;
        }
        xColumns.push_back(found);
    }

    const DataColumn *splitColumn = nullptr;
    if (!model.barplotSplitVariable.empty()) {
        const bool splitIsX = std::find(model.barplotXVariables.begin(),
                                        model.barplotXVariables.end(),
                                        model.barplotSplitVariable) != model.barplotXVariables.end();
        if (splitIsX) {
            model.barplotSplitVariable.clear();
            model.barplotShowConditionalPercent = false;
        } else {
            splitColumn = FindDataColumnInDataFrame(df, model.barplotSplitVariable);
            if (!splitColumn) {
                model.barplotSplitVariable.clear();
                model.barplotShowConditionalPercent = false;
            }
        }
    }

    RebuildBarplotBinsForColumns(model, xColumns, splitColumn);
    NormalizeBarplotVisualState(model);
    RefreshGeneratedBarplotTitle(model);
    return true;
}

bool RefreshBarplotFromDataFrame(PlotModel &model,
                                 const DataFrameModel &df)
{
    if (model.kind != "barplot") {
        return false;
    }
    NormalizeBarplotXVariables(model);

    std::vector<const DataColumn *> xColumns;
    for (const std::string &name : model.barplotXVariables) {
        const DataColumn *found = FindDataColumnInDataFrame(df, name);
        if (found) {
            xColumns.push_back(found);
        }
    }
    if (xColumns.empty()) {
        model.barplotBins.clear();
        model.barplotTotalN = 0;
        return true;
    }

    const DataColumn *splitColumn = nullptr;
    if (!model.barplotSplitVariable.empty()) {
        const bool splitIsX = std::find(model.barplotXVariables.begin(),
                                        model.barplotXVariables.end(),
                                        model.barplotSplitVariable) != model.barplotXVariables.end();
        if (splitIsX) {
            model.barplotSplitVariable.clear();
            model.barplotShowConditionalPercent = false;
        } else {
            splitColumn = FindDataColumnInDataFrame(df, model.barplotSplitVariable);
            if (!splitColumn) {
                model.barplotSplitVariable.clear();
                model.barplotShowConditionalPercent = false;
            }
        }
    }

    RebuildBarplotBinsForColumns(model, xColumns, splitColumn);
    NormalizeBarplotVisualState(model);
    RefreshGeneratedBarplotTitle(model);
    return true;
}

void NormalizeBarplotVisualState(BarplotVisualState &state,
                                 const std::vector<std::string> &levels,
                                 const std::set<std::string> &validSegmentKeys)
{
    if (!(state.defaultSegmentAlpha > 0.0 && state.defaultSegmentAlpha <= 1.0)) {
        state.defaultSegmentAlpha = 0.70;
    }
    state.segmentEncodingMode = NormalizedBarplotSegmentEncodingMode(state.segmentEncodingMode);
    state.splitStrokeWidth = ClampBarplotSplitStrokeWidth(state.splitStrokeWidth);

    std::set<std::string> levelSet(levels.begin(), levels.end());
    for (std::size_t i = 0; i < levels.size(); ++i) {
        const std::string &level = levels[i];
        auto colorIt = state.levelColors.find(level);
        if (colorIt == state.levelColors.end() || colorIt->second.empty()) {
            state.levelColors[level] = "white";
        }
        auto alphaIt = state.levelAlpha.find(level);
        if (alphaIt == state.levelAlpha.end()) {
            state.levelAlpha[level] = state.defaultSegmentAlpha;
        } else {
            alphaIt->second = ClampUnitInterval(alphaIt->second);
        }
        auto patternIt = state.levelPatterns.find(level);
        if (patternIt == state.levelPatterns.end() || !BarplotPatternIsKnown(patternIt->second)) {
            state.levelPatterns[level] = BarplotPatternAtIndex(i);
        }
    }

    EraseKeysNotIn(state.levelColors, levelSet);
    EraseKeysNotIn(state.levelAlpha, levelSet);
    EraseKeysNotIn(state.levelPatterns, levelSet);
    EraseKeysNotIn(state.segmentColorOverrides, validSegmentKeys);
    EraseKeysNotIn(state.segmentAlphaOverrides, validSegmentKeys);
    EraseKeysNotIn(state.segmentPatternOverrides, validSegmentKeys);
}

std::set<std::string> BarplotSegmentOverrideKeysForBins(
    const std::vector<BarplotBinSummary> &bins)
{
    std::set<std::string> keys;
    for (const BarplotBinSummary &bin : bins) {
        for (const BarplotSegmentSummary &segment : bin.segments) {
            keys.insert(BarplotSegmentOverrideKey(bin.category, segment.level));
        }
    }
    return keys;
}

void NormalizeBarplotVisualStateForBins(BarplotVisualState &state,
                                        const std::vector<BarplotBinSummary> &bins)
{
    NormalizeBarplotVisualState(
        state,
        BarplotSplitLevelsForBins(bins, true),
        BarplotSegmentOverrideKeysForBins(bins));
}

void ClearBarplotSplitVisualState(BarplotVisualState &state,
                                  bool includeLevelState)
{
    state.segmentColorOverrides.clear();
    state.segmentAlphaOverrides.clear();
    state.segmentPatternOverrides.clear();
    if (includeLevelState) {
        state.levelColors.clear();
        state.levelAlpha.clear();
        state.levelPatterns.clear();
    }
}

bool SetBarplotSegmentColorOverride(BarplotVisualState &state,
                                    const BarplotSegmentReference &reference,
                                    const std::string &colorName)
{
    if (colorName.empty()) {
        return false;
    }
    state.segmentColorOverrides[BarplotSegmentOverrideKey(reference.category, reference.segment.level)] = colorName;
    return true;
}

bool SetBarplotLevelColorOverride(BarplotVisualState &state,
                                  const std::string &level,
                                  const std::string &colorName)
{
    if (level.empty() || colorName.empty()) {
        return false;
    }
    state.levelColors[level] = colorName;
    return true;
}

bool SetBarplotSegmentAlphaOverride(BarplotVisualState &state,
                                    const BarplotSegmentReference &reference,
                                    double alpha)
{
    state.segmentAlphaOverrides[BarplotSegmentOverrideKey(reference.category, reference.segment.level)] =
        ClampUnitInterval(alpha);
    return true;
}

bool SetBarplotLevelPatternOverride(BarplotVisualState &state,
                                    const std::string &level,
                                    const std::string &pattern)
{
    if (level.empty() || !BarplotPatternIsKnown(pattern)) {
        return false;
    }
    state.levelPatterns[level] = pattern;
    return true;
}

bool SetBarplotSegmentPatternOverride(BarplotVisualState &state,
                                      const BarplotSegmentReference &reference,
                                      const std::string &pattern)
{
    if (!BarplotPatternIsKnown(pattern)) {
        return false;
    }
    state.segmentPatternOverrides[BarplotSegmentOverrideKey(reference.category, reference.segment.level)] = pattern;
    return true;
}

bool ResetBarplotSegmentColorOverride(BarplotVisualState &state,
                                      const BarplotSegmentReference &reference)
{
    state.segmentColorOverrides.erase(BarplotSegmentOverrideKey(reference.category, reference.segment.level));
    return true;
}

bool ResetBarplotLevelColorOverride(BarplotVisualState &state,
                                    const std::string &level)
{
    if (level.empty()) {
        return false;
    }
    state.levelColors.erase(level);
    return true;
}

BarplotSegmentVisualStyle ResolveBarplotSegmentVisual(
    double defaultSegmentAlpha,
    const std::map<std::string, std::string> &levelColors,
    const std::map<std::string, double> &levelAlpha,
    const std::map<std::string, std::string> &levelPatterns,
    const std::map<std::string, std::string> &segmentColorOverrides,
    const std::map<std::string, double> &segmentAlphaOverrides,
    const std::map<std::string, std::string> &segmentPatternOverrides,
    const std::string &xCondition,
    const std::string &level,
    std::size_t levelIndex)
{
    BarplotSegmentVisualStyle style;
    style.colorKey = "white";
    style.alpha = defaultSegmentAlpha;
    style.pattern = BarplotPatternAtIndex(levelIndex);

    auto levelColorIt = levelColors.find(level);
    if (levelColorIt != levelColors.end() && !levelColorIt->second.empty()) {
        style.colorKey = levelColorIt->second;
    }
    auto levelAlphaIt = levelAlpha.find(level);
    if (levelAlphaIt != levelAlpha.end()) {
        style.alpha = levelAlphaIt->second;
    }
    auto levelPatternIt = levelPatterns.find(level);
    if (levelPatternIt != levelPatterns.end() && BarplotPatternIsKnown(levelPatternIt->second)) {
        style.pattern = levelPatternIt->second;
    }

    std::string key = BarplotSegmentOverrideKey(xCondition, level);
    auto segmentColorIt = segmentColorOverrides.find(key);
    if (segmentColorIt != segmentColorOverrides.end() && !segmentColorIt->second.empty()) {
        style.colorKey = segmentColorIt->second;
        style.hasSegmentColorOverride = true;
    }
    auto segmentAlphaIt = segmentAlphaOverrides.find(key);
    if (segmentAlphaIt != segmentAlphaOverrides.end()) {
        style.alpha = segmentAlphaIt->second;
        style.hasSegmentAlphaOverride = true;
    }
    auto segmentPatternIt = segmentPatternOverrides.find(key);
    if (segmentPatternIt != segmentPatternOverrides.end() && BarplotPatternIsKnown(segmentPatternIt->second)) {
        style.pattern = segmentPatternIt->second;
        style.hasSegmentPatternOverride = true;
    }

    style.alpha = std::max(0.0, std::min(1.0, style.alpha));
    if (!BarplotPatternIsKnown(style.pattern)) {
        style.pattern = "none";
    }
    return style;
}

std::string RowColorKeyForRow(const std::map<int, std::string> &rowColors,
                              int row,
                              const std::string &defaultKey)
{
    auto it = rowColors.find(row);
    if (it == rowColors.end() || it->second.empty()) {
        return defaultKey;
    }
    return it->second;
}

std::string RowColorDisplayName(const std::string &name,
                                const std::string &defaultKey)
{
    return name == defaultKey ? "default" : name;
}

bool ColorKeyIsDefaultNeutral(const std::string &colorKey)
{
    std::string lowered = colorKey;
    std::transform(lowered.begin(), lowered.end(), lowered.begin(),
                   [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    return lowered == "black" || lowered == "#000000" ||
        lowered == "white" || lowered == "#ffffff";
}

std::map<std::string, int> RowColorCompositionForRows(
    const std::vector<int> &rows,
    const std::map<int, std::string> &rowColors,
    const std::string &defaultKey)
{
    std::map<std::string, int> composition;
    for (int row : rows) {
        composition[RowColorKeyForRow(rowColors, row, defaultKey)]++;
    }
    return composition;
}

std::string DominantManualRowColorForRows(const std::vector<int> &rows,
                                          const std::map<int, std::string> &rowColors,
                                          const std::string &defaultKey)
{
    if (rows.empty()) {
        return "";
    }
    std::map<std::string, int> composition = RowColorCompositionForRows(rows, rowColors, defaultKey);
    auto defaultIt = composition.find(defaultKey);
    if (defaultIt != composition.end() && defaultIt->second > 0) {
        return "";
    }

    std::string uniformColor;
    int coloredCount = 0;
    for (const auto &entry : composition) {
        if (entry.first == defaultKey || entry.second <= 0) {
            continue;
        }
        if (uniformColor.empty()) {
            uniformColor = entry.first;
            coloredCount = entry.second;
            continue;
        }
        return "";
    }

    return (!uniformColor.empty() && coloredCount == static_cast<int>(rows.size())) ? uniformColor : "";
}

std::vector<BarplotSegmentDrawItem> BuildBarplotSegmentDrawPlan(
    const Rect &barRect,
    const BarplotBinSummary &bin,
    const std::vector<std::string> &orderedLevels,
    const BarplotVisualState &visualState,
    const std::map<int, std::string> &rowColors)
{
    std::vector<BarplotSegmentDrawItem> plan;
    if (!IsValidRect(barRect) || bin.segments.empty() || orderedLevels.empty()) {
        return plan;
    }

    std::map<std::string, int> countsByLevel;
    for (const BarplotSegmentSummary &segment : bin.segments) {
        countsByLevel[segment.level] = segment.count;
    }
    std::vector<BarplotSegmentGeometry> geometry = BarplotSegmentGeometryForLevels(
        barRect,
        orderedLevels,
        countsByLevel,
        std::max(1, bin.n));

    for (const BarplotSegmentGeometry &item : geometry) {
        if (item.count <= 0) {
            continue;
        }
        auto segmentIt = std::find_if(
            bin.segments.begin(),
            bin.segments.end(),
            [&](const BarplotSegmentSummary &segment) { return segment.level == item.level; });
        if (segmentIt == bin.segments.end()) {
            continue;
        }
        std::size_t segmentIndex = static_cast<std::size_t>(std::distance(bin.segments.begin(), segmentIt));
        auto levelIt = std::find(orderedLevels.begin(), orderedLevels.end(), item.level);
        std::size_t levelIndex = levelIt == orderedLevels.end()
            ? 0
            : static_cast<std::size_t>(std::distance(orderedLevels.begin(), levelIt));

        BarplotSegmentVisualStyle style = ResolveBarplotSegmentVisual(
            visualState.defaultSegmentAlpha,
            visualState.levelColors,
            visualState.levelAlpha,
            visualState.levelPatterns,
            visualState.segmentColorOverrides,
            visualState.segmentAlphaOverrides,
            visualState.segmentPatternOverrides,
            bin.category,
            segmentIt->level,
            levelIndex);

        bool levelUsesDefaultNeutral = ColorKeyIsDefaultNeutral(style.colorKey);
        if (!style.hasSegmentColorOverride) {
            auto levelColorIt = visualState.levelColors.find(segmentIt->level);
            levelUsesDefaultNeutral = true;
            if (levelColorIt != visualState.levelColors.end() && !levelColorIt->second.empty()) {
                levelUsesDefaultNeutral = ColorKeyIsDefaultNeutral(levelColorIt->second);
            }
            if (levelUsesDefaultNeutral) {
                std::string linkedColor = DominantManualRowColorForRows(segmentIt->rows, rowColors);
                if (!linkedColor.empty()) {
                    style.colorKey = linkedColor;
                }
            }
        }
        style.alpha = std::max(style.alpha, 0.82);

        BarplotSegmentDrawItem drawItem;
        drawItem.segmentIndex = segmentIndex;
        drawItem.levelIndex = levelIndex;
        drawItem.rect = item.rect;
        drawItem.style = style;
        drawItem.useRowColorIdentityLayer = !style.hasSegmentColorOverride && levelUsesDefaultNeutral;
        drawItem.conditionalPercent = segmentIt->conditionalPercent;
        drawItem.rows = segmentIt->rows;
        plan.push_back(drawItem);
    }

    if (!plan.empty()) {
        plan.back().drawBottomEdge = true;
    }
    return plan;
}

std::vector<BarplotAxisTick> BarplotYAxisTicks(
    double yMaximum,
    const std::string &mode,
    int divisions)
{
    std::vector<BarplotAxisTick> ticks;
    if (!std::isfinite(yMaximum) || yMaximum <= 0.0 || divisions <= 0) {
        return ticks;
    }
    double lastTick = -1.0;
    ticks.reserve(static_cast<std::size_t>(divisions + 1));
    for (int i = 0; i <= divisions; ++i) {
        double tick = yMaximum * static_cast<double>(i) / static_cast<double>(divisions);
        if (std::fabs(tick - lastTick) < 1.0e-9 && i != 0 && i != divisions) {
            continue;
        }
        lastTick = tick;
        std::ostringstream label;
        label << static_cast<int>(std::round(tick));
        if (mode != "count") {
            label << "%";
        }
        ticks.push_back(BarplotAxisTick{tick, label.str()});
    }
    return ticks;
}

std::vector<BarplotMenuOption> BarplotModeMenuOptions()
{
    return {
        {"Counts", "count", "BARPLOT_MODE|count"},
        {"Overall %", "overall_percent", "BARPLOT_MODE|overall_percent"},
        {"Conditional %: each X bar sums to 100%", "conditional_percent", "BARPLOT_MODE|conditional_percent"}
    };
}

std::vector<BarplotMenuOption> BarplotWidthMenuOptions()
{
    return {
        {"Equal", "equal", "BARPLOT_WIDTH|equal"},
        {"Proportional to N in X group", "proportional_n", "BARPLOT_WIDTH|proportional_n"},
        {"Proportional to % of total sample", "proportional_percent", "BARPLOT_WIDTH|proportional_percent"}
    };
}

std::vector<BarplotMenuOption> BarplotRowColorDisplayMenuOptions(bool hasSplit)
{
    return {
        {"Hide", "hide", "BARPLOT_ROW_COLORS|hide", true},
        {"Show in tooltip only", "tooltip", "BARPLOT_ROW_COLORS|tooltip", true},
        {"Show as composition strip", "composition_strip", "BARPLOT_ROW_COLORS|composition_strip", true},
        {"Show as bar fill", "bar_fill", "BARPLOT_ROW_COLORS|bar_fill", !hasSplit}
    };
}

std::vector<BarplotMenuOption> BarplotSelectionDisplayMenuOptions()
{
    return {
        {"Hide selection", "hide", "BARPLOT_SELECTION_DISPLAY|hide"},
        {"Outline only", "outline", "BARPLOT_SELECTION_DISPLAY|outline"},
        {"Overlay rectangle", "overlay", "BARPLOT_SELECTION_DISPLAY|overlay"},
        {"Overlay rectangle + outline", "overlay_outline", "BARPLOT_SELECTION_DISPLAY|overlay_outline"}
    };
}

std::vector<BarplotMenuOption> BarplotSegmentEncodingMenuOptions()
{
    return {
        {"Transparent color + pattern", "transparent_color_pattern", "BARPLOT_SEGMENT_ENCODING|transparent_color_pattern"},
        {"Transparent color only", "transparent_color_only", "BARPLOT_SEGMENT_ENCODING|transparent_color_only"},
        {"Pattern only", "pattern_only", "BARPLOT_SEGMENT_ENCODING|pattern_only"}
    };
}

BarplotMenuOption BarplotConditionalPercentMenuOption(bool showConditionalPercent,
                                                      bool hasSplit)
{
    BarplotMenuOption option;
    option.title = "Show Conditional Percentages";
    option.value = "toggle";
    option.command = "BARPLOT_TOGGLE_CONDITIONAL_PERCENT";
    option.checked = showConditionalPercent;
    option.enabled = hasSplit;
    return option;
}

BarplotMenuOption BarplotSplitStrokeWidthMenuOption(double splitStrokeWidth)
{
    std::ostringstream title;
    title << "Bar Border Width... (" << std::fixed << std::setprecision(1)
          << ClampBarplotSplitStrokeWidth(splitStrokeWidth) << " px)";
    BarplotMenuOption option;
    option.title = title.str();
    option.value = "split_stroke_width";
    option.command = "BARPLOT_SET_SPLIT_STROKE_WIDTH_PROMPT";
    return option;
}

BarplotMenuOption BarplotSplitStyleMenuOption()
{
    BarplotMenuOption option;
    option.title = "Split style: solid colors + thick borders";
    option.value = "split_style";
    option.command = "";
    option.enabled = false;
    return option;
}

static std::vector<std::string> CleanUniqueVariableNames(const std::vector<std::string> &variables)
{
    std::vector<std::string> out;
    for (const std::string &variable : variables) {
        std::string trimmed = TrimCopy(variable);
        if (trimmed.empty()) {
            continue;
        }
        if (std::find(out.begin(), out.end(), trimmed) == out.end()) {
            out.push_back(trimmed);
        }
    }
    return out;
}

static bool ContainsVariable(const std::vector<std::string> &variables,
                             const std::string &variable)
{
    return std::find(variables.begin(), variables.end(), variable) != variables.end();
}

static std::vector<BarplotMenuOption> CheckedMenuOptions(
    std::vector<BarplotMenuOption> options,
    const std::string &selectedValue)
{
    for (BarplotMenuOption &option : options) {
        option.checked = option.value == selectedValue;
    }
    return options;
}

BarplotMenuOption BarplotShowPatternsMenuOption(bool showPatterns)
{
    BarplotMenuOption option;
    option.title = "Show patterns";
    option.value = "toggle";
    option.command = "BARPLOT_SHOW_PATTERNS|toggle";
    option.checked = showPatterns;
    return option;
}

BarplotXMenuState BuildBarplotXMenuState(
    const std::vector<std::string> &availableVariables,
    const std::vector<std::string> &currentXVariables,
    const std::string &splitVariable)
{
    BarplotXMenuState state;
    std::vector<std::string> candidates = CleanUniqueVariableNames(availableVariables);
    std::vector<std::string> currentX = CleanUniqueVariableNames(currentXVariables);

    for (const std::string &variable : candidates) {
        if (variable == splitVariable) {
            continue;
        }
        state.setOptions.push_back(BarplotMenuOption{
            variable,
            variable,
            "BARPLOT_SET_X|" + variable,
            true,
            currentX.size() == 1 && currentX.front() == variable
        });
        if (!ContainsVariable(currentX, variable)) {
            state.addOptions.push_back(BarplotMenuOption{
                variable,
                variable,
                "BARPLOT_ADD_X|" + variable
            });
        }
    }

    if (currentX.size() > 1) {
        for (const std::string &oldVariable : currentX) {
            BarplotReplaceXMenuSection section;
            section.variable = oldVariable;
            for (const std::string &candidate : candidates) {
                if (candidate == splitVariable) {
                    continue;
                }
                if (ContainsVariable(currentX, candidate) && candidate != oldVariable) {
                    continue;
                }
                section.options.push_back(BarplotMenuOption{
                    candidate,
                    candidate,
                    "BARPLOT_REPLACE_X|" + oldVariable + "|" + candidate,
                    true,
                    candidate == oldVariable
                });
            }
            state.replaceSections.push_back(section);
            state.removeOptions.push_back(BarplotMenuOption{
                oldVariable,
                oldVariable,
                "BARPLOT_REMOVE_X|" + oldVariable
            });
        }
    }

    return state;
}

BarplotSplitMenuState BuildBarplotSplitMenuState(
    const std::vector<std::string> &availableVariables,
    const std::vector<std::string> &currentXVariables,
    const std::string &splitVariable)
{
    BarplotSplitMenuState state;
    std::vector<std::string> candidates = CleanUniqueVariableNames(availableVariables);
    std::vector<std::string> currentX = CleanUniqueVariableNames(currentXVariables);
    state.noneOption = BarplotMenuOption{
        "None",
        "",
        "BARPLOT_CLEAR_SPLIT",
        true,
        splitVariable.empty()
    };
    for (const std::string &variable : candidates) {
        if (ContainsVariable(currentX, variable)) {
            continue;
        }
        state.splitOptions.push_back(BarplotMenuOption{
            variable,
            variable,
            "BARPLOT_SPLIT_BY|" + variable,
            true,
            splitVariable == variable
        });
    }
    return state;
}

BarplotCreationDialogState BuildBarplotCreationDialogState()
{
    return BarplotCreationDialogState{};
}

std::string BarplotDefaultTitle(const std::string &xLabel,
                                const std::string &splitVariable)
{
    return GeneratedBarplotTitle(xLabel, splitVariable);
}

BarplotSplitStrokeWidthDialogState BuildBarplotSplitStrokeWidthDialogState(
    double currentWidth)
{
    BarplotSplitStrokeWidthDialogState state;
    std::ostringstream value;
    value << std::fixed << std::setprecision(2)
          << ClampBarplotSplitStrokeWidth(currentWidth);
    state.currentValueText = value.str();
    return state;
}

std::string BarplotAnalysisMenuTitle(const std::vector<std::string> &xVariables,
                                     const std::string &splitVariable)
{
    std::vector<std::string> currentX = CleanUniqueVariableNames(xVariables);
    if (!splitVariable.empty() || currentX.size() > 1) {
        return "Contingency Table\u2026";
    }
    if (currentX.empty()) {
        return "Frequency table";
    }
    return "Frequency table: " + currentX.front();
}

BarplotDisplayMenuState BuildBarplotDisplayMenuState(
    const std::string &mode,
    const std::string &widthMode,
    const std::string &splitVariable,
    const std::string &rowColorDisplay,
    bool showConditionalPercent,
    double splitStrokeWidth,
    const std::string &segmentEncodingMode,
    bool showPatterns,
    const std::string &selectionDisplay,
    const std::vector<std::string> &xVariables)
{
    bool hasSplit = !splitVariable.empty();
    BarplotDisplayMenuState state;
    state.modeOptions = CheckedMenuOptions(BarplotModeMenuOptions(), mode);
    state.widthOptions = CheckedMenuOptions(BarplotWidthMenuOptions(), widthMode);
    state.conditionalPercent = BarplotConditionalPercentMenuOption(showConditionalPercent, hasSplit);
    state.rowColorOptions = CheckedMenuOptions(BarplotRowColorDisplayMenuOptions(hasSplit), rowColorDisplay);
    state.splitStrokeWidth = BarplotSplitStrokeWidthMenuOption(splitStrokeWidth);
    // Keep the ordinary bar-chart menu aligned with the macOS reference UI:
    // patterns and alternate segment encodings are not user-facing options.
    state.showSegmentEncodingMenu = false;
    state.segmentEncodingOptions = CheckedMenuOptions(BarplotSegmentEncodingMenuOptions(), segmentEncodingMode);
    state.showPatterns = BarplotShowPatternsMenuOption(showPatterns);
    state.splitStyle = BarplotSplitStyleMenuOption();
    state.selectionDisplayOptions = CheckedMenuOptions(
        BarplotSelectionDisplayMenuOptions(),
        selectionDisplay);
    state.analysis = {
        BarplotAnalysisMenuTitle(xVariables, splitVariable),
        "analysis",
        "CONTEXT_BARCHART_TABLE"
    };
    state.nestedAnalysis = {
        "Nested Contingency Table \u2014 Row Percentages",
        "nested_analysis",
        "CONTEXT_BARCHART_NESTED_TABLE"
    };
    for (const LinkedPlotCommandOption &selectionOption : SelectionCommandOptions(false)) {
        state.selectionOptions.push_back({
            selectionOption.title,
            selectionOption.value,
            selectionOption.command
        });
    }
    for (const LinkedPlotCommandOption &plotOption : LinkedPlotCommandOptions()) {
        state.plotOptions.push_back({
            plotOption.title,
            plotOption.value,
            plotOption.command
        });
    }
    return state;
}

std::vector<std::string> BarplotXVariablesAfterSet(const std::string &variable)
{
    std::string trimmed = TrimCopy(variable);
    return trimmed.empty() ? std::vector<std::string>() : std::vector<std::string>{trimmed};
}

std::vector<std::string> BarplotXVariablesAfterAdd(
    const std::vector<std::string> &currentXVariables,
    const std::string &fallbackXLabel,
    const std::string &variable)
{
    std::vector<std::string> updated = NormalizedBarplotXVariables(currentXVariables, fallbackXLabel);
    std::string trimmed = TrimCopy(variable);
    if (!trimmed.empty() && !ContainsVariable(updated, trimmed)) {
        updated.push_back(trimmed);
    }
    return updated;
}

std::vector<std::string> BarplotXVariablesAfterReplace(
    const std::vector<std::string> &currentXVariables,
    const std::string &fallbackXLabel,
    const std::string &oldVariable,
    const std::string &newVariable)
{
    std::vector<std::string> updated = NormalizedBarplotXVariables(currentXVariables, fallbackXLabel);
    std::string oldTrimmed = TrimCopy(oldVariable);
    std::string newTrimmed = TrimCopy(newVariable);
    if (oldTrimmed.empty() || newTrimmed.empty()) {
        return updated;
    }
    for (std::string &name : updated) {
        if (name == oldTrimmed) {
            name = newTrimmed;
            break;
        }
    }
    return NormalizedBarplotXVariables(updated, "");
}

std::vector<std::string> BarplotXVariablesAfterRemove(
    const std::vector<std::string> &currentXVariables,
    const std::string &fallbackXLabel,
    const std::string &variable)
{
    std::vector<std::string> updated = NormalizedBarplotXVariables(currentXVariables, fallbackXLabel);
    std::string trimmed = TrimCopy(variable);
    if (trimmed.empty() || updated.size() <= 1) {
        return updated;
    }
    updated.erase(std::remove(updated.begin(), updated.end(), trimmed), updated.end());
    return updated.empty() ? NormalizedBarplotXVariables(currentXVariables, fallbackXLabel) : updated;
}

BarplotSplitState BarplotStateAfterSplitBy(
    const std::vector<std::string> &currentXVariables,
    const std::string &fallbackXLabel,
    const std::string &currentSplitVariable,
    const std::string &rowColorDisplay,
    bool showConditionalPercent,
    const std::string &variable)
{
    BarplotSplitState state;
    state.splitVariable = currentSplitVariable;
    state.rowColorDisplay = rowColorDisplay;
    state.showConditionalPercent = showConditionalPercent;
    std::string trimmed = TrimCopy(variable);
    std::vector<std::string> currentX = NormalizedBarplotXVariables(currentXVariables, fallbackXLabel);
    if (trimmed.empty() || ContainsVariable(currentX, trimmed)) {
        return state;
    }
    state.splitVariable = trimmed;
    state.rowColorDisplay = rowColorDisplay == "bar_fill" ? "tooltip" : rowColorDisplay;
    state.showConditionalPercent = true;
    state.resetSplitVisualState = true;
    state.changed = state.splitVariable != currentSplitVariable ||
        state.rowColorDisplay != rowColorDisplay ||
        state.showConditionalPercent != showConditionalPercent;
    return state;
}

BarplotSplitState BarplotStateAfterClearSplit(
    const std::string &currentSplitVariable,
    const std::string &rowColorDisplay,
    bool showConditionalPercent)
{
    BarplotSplitState state;
    state.splitVariable = "";
    state.rowColorDisplay =
        (rowColorDisplay == "tooltip" || rowColorDisplay == "composition_strip")
            ? "bar_fill"
            : rowColorDisplay;
    state.showConditionalPercent = false;
    state.resetSplitVisualState = true;
    state.changed = !currentSplitVariable.empty() ||
        state.rowColorDisplay != rowColorDisplay ||
        showConditionalPercent;
    return state;
}

bool BarplotConditionalPercentForMode(const std::string &mode,
                                      bool hasSplit)
{
    return mode == "conditional_percent" && hasSplit;
}

std::string BarplotRowColorDisplayAfterSet(const std::string &requestedMode,
                                           bool hasSplit,
                                           const std::string &currentMode)
{
    return BarplotRowColorDisplayAllowed(requestedMode, hasSplit)
        ? requestedMode
        : currentMode;
}

std::string BarplotSelectionDisplayAfterSet(const std::string &requestedMode,
                                            const std::string &currentMode)
{
    return BarplotSelectionDisplayModeIsValid(requestedMode)
        ? requestedMode
        : currentMode;
}

std::string BarplotModeAfterSet(const std::string &requestedMode,
                                const std::string &currentMode)
{
    return BarplotModeIsValid(requestedMode)
        ? requestedMode
        : currentMode;
}

std::string BarplotWidthModeAfterSet(const std::string &requestedMode,
                                     const std::string &currentMode)
{
    return BarplotWidthModeIsValid(requestedMode)
        ? requestedMode
        : currentMode;
}

BarplotSegmentEncodingState BarplotStateAfterSegmentEncoding(
    const std::string &currentMode,
    bool currentShowPatterns,
    const std::string &requestedMode)
{
    BarplotSegmentEncodingState state;
    state.segmentEncodingMode = currentMode;
    state.showPatterns = currentShowPatterns;
    if (!BarplotSegmentEncodingModeIsValid(requestedMode)) {
        return state;
    }
    state.segmentEncodingMode = requestedMode;
    state.showPatterns = requestedMode != "transparent_color_only";
    state.changed = state.segmentEncodingMode != currentMode ||
        state.showPatterns != currentShowPatterns;
    return state;
}

bool BarplotShowPatternsAfterCommand(bool currentShowPatterns,
                                     const std::string &commandValue)
{
    return commandValue == "toggle"
        ? !currentShowPatterns
        : commandValue == "on";
}

std::string BarplotSegmentCommandPayload(std::size_t barIndex,
                                         std::size_t segmentIndex)
{
    return std::to_string(barIndex) + "|" + std::to_string(segmentIndex);
}

std::string BarplotSegmentMenuTitle(const std::string &xLabel,
                                    const std::string &splitVariable,
                                    const BarplotSegmentReference &reference)
{
    std::ostringstream out;
    out << xLabel << " = " << reference.category;
    if (!splitVariable.empty()) {
        out << " | " << splitVariable << " = " << reference.segment.level;
    }
    return out.str();
}

std::vector<double> BarplotSegmentOpacityMenuValues()
{
    return {0.25, 0.40, 0.55, 0.70, 0.85, 1.00};
}

std::vector<std::string> BarplotPatternMenuValues()
{
    return {
        "diagonal_slash",
        "dots",
        "crosshatch",
        "vertical",
        "horizontal",
        "diagonal_backslash",
        "none"
    };
}

BarplotSegmentMenuState BuildBarplotSegmentMenuState(
    const std::string &xLabel,
    const std::string &splitVariable,
    const BarplotSegmentReference &reference,
    const BarplotSegmentVisualStyle &style,
    const std::vector<std::string> &paletteNames,
    const std::string &levelColor,
    const std::string &levelPattern)
{
    BarplotSegmentMenuState state;
    state.title = BarplotSegmentMenuTitle(xLabel, splitVariable, reference);
    state.payload = BarplotSegmentCommandPayload(reference.barIndex, reference.segmentIndex);
    state.level = reference.segment.level;
    std::string encodedLevel = EncodeCommandField(state.level);

    state.selectRows = {"Select rows in this segment", "select", "BARPLOT_SEGMENT_SELECT|" + state.payload};
    state.applyCurrentColorToRows = {
        "Apply current row color to rows in this segment...",
        "apply_color_to_rows",
        "BARPLOT_SEGMENT_APPLY_COLOR_TO_ROWS|" + state.payload
    };

    state.segmentColorOptions.reserve(paletteNames.size());
    state.levelColorOptions.reserve(paletteNames.size());
    for (const std::string &name : paletteNames) {
        BarplotMenuOption segmentColor;
        segmentColor.title = name;
        segmentColor.value = name;
        segmentColor.command = "BARPLOT_SEGMENT_SET_COLOR|" + state.payload + "|" + name;
        segmentColor.checked = style.colorKey == name && style.hasSegmentColorOverride;
        state.segmentColorOptions.push_back(segmentColor);

        BarplotMenuOption levelColorOption;
        levelColorOption.title = name;
        levelColorOption.value = name;
        levelColorOption.command = "BARPLOT_LEVEL_SET_COLOR|" + encodedLevel + "|" + name;
        levelColorOption.checked = levelColor == name;
        state.levelColorOptions.push_back(levelColorOption);
    }

    for (double opacity : BarplotSegmentOpacityMenuValues()) {
        std::ostringstream label;
        label << (int)std::round(opacity * 100.0) << "%";
        std::ostringstream commandValue;
        commandValue << std::fixed << std::setprecision(2) << opacity;
        BarplotMenuOption option;
        option.title = label.str();
        option.value = commandValue.str();
        option.command = "BARPLOT_SEGMENT_SET_ALPHA|" + state.payload + "|" + commandValue.str();
        option.checked = std::fabs(style.alpha - opacity) < 0.01;
        state.segmentOpacityOptions.push_back(option);
    }

    std::vector<std::string> patterns = BarplotPatternMenuValues();
    state.levelPatternOptions.reserve(patterns.size());
    state.segmentPatternOptions.reserve(patterns.size());
    for (const std::string &pattern : patterns) {
        BarplotMenuOption levelPatternOption;
        levelPatternOption.title = BarplotPatternDisplayName(pattern);
        levelPatternOption.value = pattern;
        levelPatternOption.command = "BARPLOT_LEVEL_SET_PATTERN|" + encodedLevel + "|" + pattern;
        levelPatternOption.checked = levelPattern == pattern;
        state.levelPatternOptions.push_back(levelPatternOption);

        BarplotMenuOption segmentPatternOption;
        segmentPatternOption.title = BarplotPatternDisplayName(pattern);
        segmentPatternOption.value = pattern;
        segmentPatternOption.command = "BARPLOT_SEGMENT_SET_PATTERN|" + state.payload + "|" + pattern;
        segmentPatternOption.checked = style.pattern == pattern && style.hasSegmentPatternOverride;
        state.segmentPatternOptions.push_back(segmentPatternOption);
    }

    state.levelColorTitle = "Set color for '" + state.level + "' everywhere...";
    state.levelPatternTitle = "Set pattern for '" + state.level + "'...";
    state.resetSegmentColor = {
        "Reset this segment color",
        "reset_segment_color",
        "BARPLOT_SEGMENT_RESET_COLOR|" + state.payload
    };
    state.resetLevelColor = {
        "Reset '" + state.level + "' color",
        "reset_level_color",
        "BARPLOT_LEVEL_RESET_COLOR|" + encodedLevel
    };
    state.details = {
        "Show segment details",
        "details",
        "BARPLOT_SEGMENT_DETAILS|" + state.payload
    };
    return state;
}

std::optional<BarplotSegmentReference> BarplotSegmentReferenceForIndices(
    const std::vector<BarplotBinSummary> &bins,
    std::size_t barIndex,
    std::size_t segmentIndex)
{
    if (barIndex >= bins.size()) {
        return std::nullopt;
    }
    const BarplotBinSummary &bin = bins[barIndex];
    if (segmentIndex >= bin.segments.size()) {
        return std::nullopt;
    }
    BarplotSegmentReference reference;
    reference.barIndex = barIndex;
    reference.segmentIndex = segmentIndex;
    reference.category = bin.category;
    reference.segment = bin.segments[segmentIndex];
    return reference;
}

std::vector<int> BarplotRowsForSegmentIndices(
    const std::vector<BarplotBinSummary> &bins,
    std::size_t barIndex,
    std::size_t segmentIndex)
{
    std::optional<BarplotSegmentReference> reference =
        BarplotSegmentReferenceForIndices(bins, barIndex, segmentIndex);
    return reference.has_value() ? reference->segment.rows : std::vector<int>();
}

bool BarplotModeIsValid(const std::string &mode)
{
    std::vector<BarplotMenuOption> options = BarplotModeMenuOptions();
    return std::any_of(options.begin(), options.end(), [&](const BarplotMenuOption &option) {
        return option.value == mode;
    });
}

bool BarplotWidthModeIsValid(const std::string &widthMode)
{
    std::vector<BarplotMenuOption> options = BarplotWidthMenuOptions();
    return std::any_of(options.begin(), options.end(), [&](const BarplotMenuOption &option) {
        return option.value == widthMode;
    });
}

bool BarplotRowColorDisplayIsValid(const std::string &mode)
{
    std::vector<BarplotMenuOption> options = BarplotRowColorDisplayMenuOptions(false);
    return std::any_of(options.begin(), options.end(), [&](const BarplotMenuOption &option) {
        return option.value == mode;
    });
}

bool BarplotRowColorDisplayAllowed(const std::string &mode,
                                   bool hasSplit)
{
    std::vector<BarplotMenuOption> options = BarplotRowColorDisplayMenuOptions(hasSplit);
    return std::any_of(options.begin(), options.end(), [&](const BarplotMenuOption &option) {
        return option.value == mode && option.enabled;
    });
}

bool BarplotSelectionDisplayModeIsValid(const std::string &mode)
{
    std::vector<BarplotMenuOption> options = BarplotSelectionDisplayMenuOptions();
    return std::any_of(options.begin(), options.end(), [&](const BarplotMenuOption &option) {
        return option.value == mode;
    });
}

BarplotViewSize BarplotPreferredViewSize(std::size_t categoryCount,
                                         bool hasSplit,
                                         std::size_t xVariableCount)
{
    const std::size_t categories = std::max<std::size_t>(1, categoryCount);
    const std::size_t xCount = std::max<std::size_t>(1, xVariableCount);
    double width = 620.0 + static_cast<double>(std::min<std::size_t>(8, categories > 5 ? categories - 5 : 0)) * 14.0;
    double height = 460.0;
    if (hasSplit) {
        width += 42.0;
        height += 12.0;
    }
    if (xCount > 1) {
        height += static_cast<double>(std::min<std::size_t>(3, xCount - 1)) * 14.0;
    }
    return {
        std::min(760.0, width),
        std::min(540.0, height)
    };
}

double BarplotCategoryLabelVerticalOffset(bool showCompositionStrip)
{
    return showCompositionStrip ? 9.0 : 7.0;
}

Rect BarplotPlotRect(const Rect &viewRect,
                     bool hasSplit,
                     bool showCompositionStrip,
                     std::size_t xVariableCount)
{
    const double top = (hasSplit ? 52.0 : 42.0) + (hasSplit && showCompositionStrip ? 14.0 : 0.0);
    const double left = hasSplit ? 118.0 : 64.0;
    const std::size_t xCount = std::max<std::size_t>(1, xVariableCount);
    const double extraBottom = xCount > 1
        ? static_cast<double>(std::min<std::size_t>(42, (xCount - 1) * 14))
        : 0.0;
    const double bottom = 70.0 + extraBottom;
    return {
        left,
        top,
        std::max(10.0, viewRect.width - left - 32.0),
        std::max(10.0, viewRect.height - top - bottom)
    };
}

Rect BarplotXAxisLabelRect(const Rect &viewRect,
                           const Rect &plotRect)
{
    return {
        plotRect.x,
        viewRect.height - 47.0,
        plotRect.width,
        24.0
    };
}

int BarplotMaxBarCount(const std::vector<BarplotBinSummary> &bins)
{
    int maxCount = 1;
    for (const BarplotBinSummary &bin : bins) {
        maxCount = std::max(maxCount, bin.n > 0 ? bin.n : static_cast<int>(bin.rows.size()));
    }
    return maxCount;
}

double BarplotValueForBin(const BarplotBinSummary &bin,
                          const std::string &mode)
{
    if (mode == "conditional_percent") {
        return bin.n > 0 ? 100.0 : 0.0;
    }
    if (mode == "overall_percent") {
        return bin.percent;
    }
    return static_cast<double>(bin.n > 0 ? bin.n : static_cast<int>(bin.rows.size()));
}

double BarplotWidthValueForBin(const std::string &widthMode,
                               int count,
                               double percent)
{
    return widthMode == "proportional_percent" ? percent : static_cast<double>(count);
}

double BarplotYMaximum(const std::vector<BarplotBinSummary> &bins,
                       const std::string &mode)
{
    if (mode == "conditional_percent" || mode == "overall_percent") {
        return 100.0;
    }
    return static_cast<double>(std::max(1, BarplotMaxBarCount(bins)));
}

BarplotLayout BuildBarplotLayout(const BarplotLayoutInput &input)
{
    const std::vector<std::string> xVariables = BarplotXVariablesForLabels(input.xVariables, "");
    const bool compositionStrip = input.hasSplit && input.showCompositionStrip;
    return BuildBarplotLayoutForPlotRect(BarplotPlotRect(
        input.viewRect,
        input.hasSplit,
        compositionStrip,
        std::max<std::size_t>(1, xVariables.size())),
        xVariables, input.bins, input.mode, input.widthMode);
}

BarplotLayout BuildBarplotLayoutForPlotRect(const Rect &plotRect,
                                            const std::vector<std::string> &xVariables,
                                            const std::vector<BarplotBinSummary> &bins,
                                            const std::string &mode,
                                            const std::string &widthMode)
{
    BarplotLayout layout;
    layout.plotRect = plotRect;
    layout.nestingDepth = std::max<std::size_t>(1, xVariables.size());
    layout.yMaximum = BarplotYMaximum(bins, mode);
    layout.widthMode = widthMode == "equal"
        ? BarplotWidthMode::Equal
        : BarplotWidthMode::Proportional;
    layout.sharedPrefixDepthWithPrevious.resize(bins.size(), 0);
    layout.values.reserve(bins.size());
    layout.widthValues.reserve(bins.size());
    for (std::size_t i = 0; i < bins.size(); ++i) {
        const BarplotBinSummary &bin = bins[i];
        layout.values.push_back(BarplotValueForBin(bin, mode));
        layout.widthValues.push_back(bin.widthValue);
        if (i > 0 && layout.nestingDepth > 1) {
            layout.sharedPrefixDepthWithPrevious[i] =
                BarplotSharedCategoryPrefixDepth(xVariables, bins[i - 1].category, bin.category);
        }
    }
    return layout;
}

std::vector<std::pair<std::string, int>> OrderedRowColorComposition(
    const std::map<std::string, int> &composition,
    const std::vector<std::string> &paletteOrder,
    const std::string &defaultKey)
{
    std::vector<std::pair<std::string, int>> out;
    auto defaultIt = composition.find(defaultKey);
    if (defaultIt != composition.end()) {
        out.push_back(*defaultIt);
    }

    for (const std::string &name : paletteOrder) {
        auto it = composition.find(name);
        if (it != composition.end()) {
            out.push_back(*it);
        }
    }

    for (const auto &entry : composition) {
        if (entry.first == defaultKey) {
            continue;
        }
        if (std::find(paletteOrder.begin(), paletteOrder.end(), entry.first) == paletteOrder.end()) {
            out.push_back(entry);
        }
    }
    return out;
}

static bool CompositionHasManualColor(const std::vector<std::pair<std::string, int>> &composition,
                                      const std::string &defaultKey)
{
    for (const auto &entry : composition) {
        if (entry.second > 0 && entry.first != defaultKey) {
            return true;
        }
    }
    return false;
}

std::vector<BarplotCompositionSlice> BarplotVerticalRowColorCompositionSlices(
    const Rect &geometry,
    const std::vector<int> &rows,
    const std::map<int, std::string> &rowColors,
    const std::vector<std::string> &paletteOrder,
    bool includeDefault,
    bool requireManualColor,
    const std::string &defaultKey)
{
    std::vector<BarplotCompositionSlice> slices;
    if (rows.empty() || !IsValidRect(geometry)) {
        return slices;
    }

    std::vector<std::pair<std::string, int>> composition =
        OrderedRowColorComposition(RowColorCompositionForRows(rows, rowColors, defaultKey),
                                   paletteOrder,
                                   defaultKey);
    if (requireManualColor && !CompositionHasManualColor(composition, defaultKey)) {
        return slices;
    }

    const int total = std::max<int>(1, static_cast<int>(rows.size()));
    double y = geometry.y + geometry.height;
    slices.reserve(composition.size());
    for (const auto &entry : composition) {
        if (entry.second <= 0) {
            continue;
        }
        const double h = geometry.height * static_cast<double>(entry.second) / static_cast<double>(total);
        y -= h;
        if (!includeDefault && entry.first == defaultKey) {
            continue;
        }
        slices.push_back(BarplotCompositionSlice{
            Rect{geometry.x, y, geometry.width, h},
            entry.first,
            entry.second
        });
    }
    return slices;
}

std::vector<BarplotCompositionSlice> BarplotHorizontalRowColorCompositionSlices(
    const Rect &geometry,
    const std::vector<int> &rows,
    const std::map<int, std::string> &rowColors,
    const std::vector<std::string> &paletteOrder,
    bool includeDefault,
    bool requireManualColor,
    const std::string &defaultKey)
{
    std::vector<BarplotCompositionSlice> slices;
    if (rows.empty() || !IsValidRect(geometry)) {
        return slices;
    }

    std::vector<std::pair<std::string, int>> composition =
        OrderedRowColorComposition(RowColorCompositionForRows(rows, rowColors, defaultKey),
                                   paletteOrder,
                                   defaultKey);
    if (requireManualColor && !CompositionHasManualColor(composition, defaultKey)) {
        return slices;
    }

    const int total = std::max<int>(1, static_cast<int>(rows.size()));
    double x = geometry.x;
    slices.reserve(composition.size());
    for (const auto &entry : composition) {
        if (entry.second <= 0) {
            continue;
        }
        const double w = geometry.width * static_cast<double>(entry.second) / static_cast<double>(total);
        if (includeDefault || entry.first != defaultKey) {
            slices.push_back(BarplotCompositionSlice{
                Rect{x, geometry.y, w, geometry.height},
                entry.first,
                entry.second
            });
        }
        x += w;
    }
    return slices;
}

BarplotSelectionSlicePlan BuildBarplotSelectionSlicePlan(
    const Rect &geometry,
    const std::vector<int> &rows,
    const std::set<int> &selection,
    const std::map<int, std::string> &rowColors,
    const std::vector<std::string> &paletteOrder,
    const std::string &fallbackSelectedColorKey,
    const std::string &defaultKey)
{
    BarplotSelectionSlicePlan plan;
    plan.dominantSelectedColorKey = fallbackSelectedColorKey.empty()
        ? defaultKey
        : fallbackSelectedColorKey;
    plan.total = static_cast<int>(rows.size());
    if (rows.empty() || !IsValidRect(geometry)) {
        return plan;
    }

    std::vector<std::pair<std::string, int>> allComposition =
        OrderedRowColorComposition(RowColorCompositionForRows(rows, rowColors, defaultKey),
                                   paletteOrder,
                                   defaultKey);
    const int total = std::max<int>(1, static_cast<int>(rows.size()));
    double y = geometry.y + geometry.height;
    plan.backgroundSlices.reserve(allComposition.size());
    for (const auto &entry : allComposition) {
        if (entry.second <= 0) continue;
        const double bandH = geometry.height * static_cast<double>(entry.second) /
            static_cast<double>(total);
        y -= bandH;
        plan.backgroundSlices.push_back(BarplotCompositionSlice{
            Rect{geometry.x, y, geometry.width, bandH},
            entry.first,
            entry.second
        });
    }

    if (selection.empty()) {
        return plan;
    }

    std::map<std::string, int> selectedCompositionMap;
    for (int row : rows) {
        if (selection.find(row) == selection.end()) {
            continue;
        }
        selectedCompositionMap[RowColorKeyForRow(rowColors, row, defaultKey)]++;
    }

    int dominantCount = -1;
    for (const auto &entry : selectedCompositionMap) {
        if (entry.second <= 0) {
            continue;
        }
        plan.selected += entry.second;
        if (entry.second > dominantCount) {
            dominantCount = entry.second;
            plan.dominantSelectedColorKey = entry.first;
        }
    }
    if (plan.selected <= 0) {
        return plan;
    }
    plan.selectedFraction = SelectedFractionForRows(rows, plan.selected);

    y = geometry.y + geometry.height;
    plan.slices.reserve(allComposition.size());
    for (const auto &entry : allComposition) {
        const std::string &rowColorKey = entry.first;
        const int colorTotal = entry.second;
        if (colorTotal <= 0) {
            continue;
        }
        const double bandH = geometry.height * static_cast<double>(colorTotal) / static_cast<double>(total);
        y -= bandH;

        auto selectedIt = selectedCompositionMap.find(rowColorKey);
        const int selectedInColor = selectedIt == selectedCompositionMap.end() ? 0 : selectedIt->second;
        if (selectedInColor <= 0) {
            continue;
        }
        const double selectedBandH = bandH * static_cast<double>(selectedInColor) / static_cast<double>(colorTotal);
        plan.slices.push_back(BarplotCompositionSlice{
            Rect{geometry.x, y + bandH - selectedBandH, geometry.width, selectedBandH},
            rowColorKey,
            selectedInColor
        });
    }
    return plan;
}

bool BarplotSelectedSliceCoversY(const BarplotSelectionSlicePlan &plan, double y)
{
    if (!std::isfinite(y)) return false;
    for (const BarplotCompositionSlice &slice : plan.slices) {
        if (y >= slice.rect.y && y <= slice.rect.y + slice.rect.height)
            return true;
    }
    return false;
}

int CountSelectedRowsForRows(const std::vector<int> &rows,
                             const std::set<int> &selection)
{
    int selected = 0;
    for (int row : rows) {
        if (selection.find(row) != selection.end()) {
            ++selected;
        }
    }
    return selected;
}

double SelectedFractionForRows(const std::vector<int> &rows,
                               int selected)
{
    return rows.empty() ? 0.0 : static_cast<double>(selected) / static_cast<double>(rows.size());
}

std::string BarplotCategoryTooltipText(
    const std::string &variableName,
    const std::string &value,
    const std::vector<int> &rows,
    const std::set<int> &selection)
{
    int selectedRows = CountSelectedRowsForRows(rows, selection);
    std::ostringstream out;
    out << variableName << " = " << value
        << "\nrows = " << rows.size()
        << "\nselected = " << selectedRows << " of " << rows.size()
        << "\nclick to select this X category";
    return out.str();
}

std::string BarplotSplitLevelTooltipText(
    const std::string &splitVariable,
    const std::string &level,
    const std::vector<int> &rows,
    const std::set<int> &selection)
{
    int selectedRows = CountSelectedRowsForRows(rows, selection);
    std::ostringstream out;
    out << splitVariable << " = " << level
        << "\nrows = " << rows.size()
        << "\nselected = " << selectedRows << " of " << rows.size()
        << "\nclick to select this split category";
    return out.str();
}

std::string BarplotSegmentTooltipText(
    const std::string &xLabel,
    const std::string &splitVariable,
    const std::string &widthMode,
    const BarplotBinSummary &bin,
    int totalN,
    const std::optional<BarplotSegmentSummary> &segment,
    const std::optional<BarplotSegmentVisualStyle> &visualStyle,
    const std::map<int, std::string> &rowColors,
    const std::set<int> &selection,
    const std::vector<std::string> &paletteOrder,
    bool showRowColors)
{
    BarplotSegmentSummary fallback;
    const BarplotSegmentSummary *activeSegment = nullptr;
    bool tooltipWholeBar = false;
    if (segment.has_value()) {
        activeSegment = &*segment;
    } else {
        tooltipWholeBar = true;
        fallback.level = splitVariable.empty() ? "All" : "Bar total";
        fallback.count = bin.n;
        fallback.barN = bin.n;
        fallback.totalN = totalN;
        fallback.conditionalPercent = bin.n > 0 ? 100.0 : 0.0;
        fallback.overallPercent = totalN > 0
            ? 100.0 * static_cast<double>(bin.n) / static_cast<double>(totalN)
            : 0.0;
        fallback.barPercent = bin.percent;
        fallback.barWidthValue = bin.widthValue;
        fallback.rows = bin.rows;
        activeSegment = &fallback;
    }

    std::ostringstream out;
    out << xLabel << " = " << bin.category;
    if (!splitVariable.empty()) {
        out << "\n" << splitVariable << " = " << activeSegment->level;
    }
    out << "\nn = " << activeSegment->count
        << "\nbar denominator = " << activeSegment->barN
        << "\nconditional percent = " << FormatPercent(activeSegment->conditionalPercent / 100.0, 1)
        << "\noverall percent = " << FormatPercent(activeSegment->overallPercent / 100.0, 1)
        << "\nbar N = " << bin.n
        << "\nbar percent of total = " << FormatPercent(bin.percent / 100.0, 1)
        << "\nbar width = " << BarplotWidthScaleLabel(widthMode);

    if (!tooltipWholeBar && !splitVariable.empty() && visualStyle.has_value()) {
        std::ostringstream alphaText;
        alphaText << std::fixed << std::setprecision(2) << visualStyle->alpha;
        out << "\nsegment color = " << visualStyle->colorKey
            << "\nsegment alpha = " << alphaText.str()
            << "\nsegment pattern = " << visualStyle->pattern;
        if (visualStyle->hasSegmentColorOverride ||
            visualStyle->hasSegmentAlphaOverride ||
            visualStyle->hasSegmentPatternOverride) {
            out << "\nsegment override = yes";
        }
    }

    if (showRowColors) {
        std::vector<std::pair<std::string, int>> composition =
            OrderedRowColorComposition(RowColorCompositionForRows(activeSegment->rows, rowColors),
                                       paletteOrder);
        out << "\n\nmanual row colors:";
        if (composition.empty()) {
            out << "\n  none";
        } else {
            for (const auto &entry : composition) {
                out << "\n  " << RowColorDisplayName(entry.first) << ": " << entry.second;
            }
        }
    }

    int selectedRows = CountSelectedRowsForRows(activeSegment->rows, selection);
    double selectedFraction = SelectedFractionForRows(activeSegment->rows, selectedRows);
    out << "\nselected = " << selectedRows << " of " << activeSegment->rows.size() << " rows"
        << "\nselected fraction within " << (tooltipWholeBar ? "bar" : "segment")
        << " = " << FormatPercent(selectedFraction, 1);
    return out.str();
}

std::string BarplotNotAvailableStatus()
{
    return "No active bar chart is available.";
}

std::string BarplotChooseXVariableStatus()
{
    return "Choose at least one X variable.";
}

std::string BarplotNoValuesToPlotStatus()
{
    return "The selected variable has no values to plot.";
}

std::string BarplotWindowTitle()
{
    return "Bar Chart";
}

std::string BarplotSegmentMenuTitle()
{
    return "Segment";
}

std::string BarplotSetSegmentColorMenuItemTitle()
{
    return "Set segment color...";
}

std::string BarplotSetSegmentOpacityMenuItemTitle()
{
    return "Set segment opacity...";
}

std::string BarplotSetPatternForThisSegmentMenuItemTitle()
{
    return "Set pattern for this segment...";
}

BarplotSegmentSummary ToBarplotSegmentSummary(const BarplotSegment &segment)
{
    BarplotSegmentSummary summary;
    summary.level = segment.level;
    summary.rows = segment.rows;
    summary.count = segment.count;
    summary.barN = segment.barN;
    summary.totalN = segment.totalN;
    summary.conditionalPercent = segment.conditionalPercent;
    summary.overallPercent = segment.overallPercent;
    summary.barPercent = segment.barPercent;
    summary.barWidthValue = segment.barWidthValue;
    return summary;
}

BarplotBinSummary ToBarplotBinSummary(const BarplotBin &bin)
{
    BarplotBinSummary summary;
    summary.category = bin.category;
    summary.rows = bin.rows;
    summary.n = bin.n;
    summary.percent = bin.percent;
    summary.widthValue = bin.widthValue;
    summary.segments.reserve(bin.segments.size());
    for (const BarplotSegment &segment : bin.segments) {
        summary.segments.push_back(ToBarplotSegmentSummary(segment));
    }
    return summary;
}

std::vector<BarplotBinSummary> BarplotBinsForModel(const PlotModel &model)
{
    std::vector<BarplotBinSummary> bins;
    bins.reserve(model.barplotBins.size());
    for (const BarplotBin &bin : model.barplotBins) {
        bins.push_back(ToBarplotBinSummary(bin));
    }
    return bins;
}

std::vector<std::string> BarplotXVariablesForModel(const PlotModel &model)
{
    return BarplotXVariablesForLabels(model.barplotXVariables, model.xLabel);
}

void NormalizeBarplotXVariables(PlotModel &model)
{
    model.barplotXVariables = NormalizedBarplotXVariables(model.barplotXVariables, model.xLabel);
    model.xLabel = JoinStrings(model.barplotXVariables, " + ");
}

void SortBarplotBinsByXHierarchy(PlotModel &model)
{
    if (model.barplotBins.size() <= 1) return;
    std::vector<std::string> xVariables = BarplotXVariablesForModel(model);
    // A single categorical X still has a meaningful canonical order.  The
    // macOS renderer orders numeric-looking levels numerically (4, 6, 8), so
    // only an actually missing X specification should bypass sorting.
    if (xVariables.empty()) return;

    std::vector<std::string> categories;
    categories.reserve(model.barplotBins.size());
    for (const BarplotBin &bin : model.barplotBins) {
        categories.push_back(bin.category);
    }
    std::vector<size_t> order = BarplotCategorySortOrder(xVariables, categories);
    if (order.size() != model.barplotBins.size()) return;

    std::vector<BarplotBin> sorted;
    sorted.reserve(model.barplotBins.size());
    for (size_t index : order) {
        if (index < model.barplotBins.size()) {
            sorted.push_back(model.barplotBins[index]);
        }
    }
    if (sorted.size() == model.barplotBins.size()) {
        model.barplotBins = std::move(sorted);
    }
}

void RefreshGeneratedBarplotTitle(PlotModel &model)
{
    model.title = RefreshedBarplotTitle(model.title, model.xLabel, model.barplotSplitVariable);
}

std::vector<std::string> BarplotSplitLevels(const PlotModel &model)
{
    return BarplotSplitLevelsForBins(BarplotBinsForModel(model), true);
}

BarplotVisualState BarplotVisualStateForModel(const PlotModel &model)
{
    BarplotVisualState state;
    state.defaultSegmentAlpha = model.barplotDefaultSegmentAlpha;
    state.segmentEncodingMode = model.barplotSegmentEncodingMode;
    state.splitStrokeWidth = model.barplotSplitStrokeWidth;
    state.levelColors = model.barplotYLevelColors;
    state.levelAlpha = model.barplotYLevelAlpha;
    state.levelPatterns = model.barplotYLevelPatterns;
    state.segmentColorOverrides = model.barplotSegmentColorOverrides;
    state.segmentAlphaOverrides = model.barplotSegmentAlphaOverrides;
    state.segmentPatternOverrides = model.barplotSegmentPatternOverrides;
    return state;
}

void ApplyBarplotVisualStateToModel(PlotModel &model, const BarplotVisualState &state)
{
    model.barplotDefaultSegmentAlpha = state.defaultSegmentAlpha;
    model.barplotSegmentEncodingMode = state.segmentEncodingMode;
    model.barplotSplitStrokeWidth = state.splitStrokeWidth;
    model.barplotYLevelColors = state.levelColors;
    model.barplotYLevelAlpha = state.levelAlpha;
    model.barplotYLevelPatterns = state.levelPatterns;
    model.barplotSegmentColorOverrides = state.segmentColorOverrides;
    model.barplotSegmentAlphaOverrides = state.segmentAlphaOverrides;
    model.barplotSegmentPatternOverrides = state.segmentPatternOverrides;
}

void NormalizeBarplotVisualState(PlotModel &model)
{
    BarplotVisualState state = BarplotVisualStateForModel(model);
    NormalizeBarplotVisualStateForBins(state, BarplotBinsForModel(model));
    ApplyBarplotVisualStateToModel(model, state);
}

void ResetBarplotSplitVisualState(PlotModel &model, bool includeLevelState)
{
    BarplotVisualState state = BarplotVisualStateForModel(model);
    ClearBarplotSplitVisualState(state, includeLevelState);
    ApplyBarplotVisualStateToModel(model, state);
}

void ApplyNormalizedBarplotVisualState(PlotModel &model, BarplotVisualState state)
{
    NormalizeBarplotVisualStateForBins(state, BarplotBinsForModel(model));
    ApplyBarplotVisualStateToModel(model, state);
}

BarplotSegmentVisualStyle ResolveBarplotSegmentVisual(
    const PlotModel &model,
    const std::string &xCondition,
    const std::string &level,
    std::size_t levelIndex)
{
    return ResolveBarplotSegmentVisual(
        model.barplotDefaultSegmentAlpha,
        model.barplotYLevelColors,
        model.barplotYLevelAlpha,
        model.barplotYLevelPatterns,
        model.barplotSegmentColorOverrides,
        model.barplotSegmentAlphaOverrides,
        model.barplotSegmentPatternOverrides,
        xCondition,
        level,
        levelIndex);
}

bool BarplotEncodingShowsPatterns(const PlotModel &model)
{
    (void)model;
    return false;
}

bool BarplotEncodingShowsColor(const PlotModel &model)
{
    (void)model;
    return true;
}

std::optional<BarplotSegmentReference> BarplotCoreSegmentReferenceForIndices(
    const PlotModel &model, int barIndex, int segmentIndex)
{
    if (barIndex < 0 || segmentIndex < 0) {
        return std::nullopt;
    }
    return BarplotSegmentReferenceForIndices(
        BarplotBinsForModel(model), (std::size_t)barIndex, (std::size_t)segmentIndex);
}

std::vector<std::string> BarplotPaletteOrder()
{
    const auto &colors = PaletteColors();
    std::vector<std::string> paletteOrder;
    paletteOrder.reserve(colors.size());
    for (const auto &color : colors) {
        paletteOrder.push_back(color.name);
    }
    return paletteOrder;
}

} // namespace core
} // namespace rlispstat
