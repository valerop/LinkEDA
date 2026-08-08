#include "histogram_model.h"

#include "command_model.h"

#include <algorithm>
#include <cstdlib>
#include <cmath>
#include <iomanip>
#include <map>
#include <sstream>

namespace rlispstat {
namespace core {

namespace {

std::vector<double> FiniteValues(const std::vector<double> &values)
{
    std::vector<double> out;
    out.reserve(values.size());
    for (double value : values) {
        if (std::isfinite(value)) {
            out.push_back(value);
        }
    }
    return out;
}

std::optional<double> ParseFiniteDouble(const std::string &text)
{
    std::size_t start = 0;
    while (start < text.size() &&
           (text[start] == ' ' || text[start] == '\t' || text[start] == '\n' || text[start] == '\r')) {
        ++start;
    }
    std::size_t finish = text.size();
    while (finish > start &&
           (text[finish - 1] == ' ' || text[finish - 1] == '\t' ||
            text[finish - 1] == '\n' || text[finish - 1] == '\r')) {
        --finish;
    }
    std::string trimmed = text.substr(start, finish - start);
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

std::string FormatHistogramDialogDouble(double value)
{
    std::ostringstream out;
    out << std::setprecision(6) << std::defaultfloat << value;
    return out.str();
}

} // namespace

bool IsValidHistogramLayout(const HistogramLayout &layout)
{
    return IsValidRect(layout.plotRect) &&
        (!layout.counts.empty() || !layout.values.empty()) &&
        ((layout.values.empty() && layout.maxCount > 0) ||
         (!layout.values.empty() && std::isfinite(layout.maxValue) && layout.maxValue > 0.0)) &&
        std::isfinite(layout.gap);
}

std::vector<Rect> HistogramBinRects(const HistogramLayout &layout)
{
    std::vector<Rect> rects;
    if (!IsValidHistogramLayout(layout)) {
        return rects;
    }
    const std::size_t n = !layout.values.empty() ? layout.values.size() : layout.counts.size();
    rects.reserve(n);
    const double slotWidth = layout.plotRect.width / static_cast<double>(std::max<std::size_t>(1, n));
    const double gap = std::max(0.0, layout.gap);
    for (std::size_t i = 0; i < n; ++i) {
        const double value = !layout.values.empty()
            ? std::max(0.0, layout.values[i])
            : static_cast<double>(std::max(0, layout.counts[i]));
        const double maximum = !layout.values.empty()
            ? layout.maxValue : static_cast<double>(std::max(1, layout.maxCount));
        const double h = layout.plotRect.height * value / maximum;
        rects.push_back({
            layout.plotRect.x + slotWidth * static_cast<double>(i) + gap / 2.0,
            layout.plotRect.y + layout.plotRect.height - h,
            std::max(1.0, slotWidth - gap),
            h
        });
    }
    return rects;
}

Rect HistogramBinRect(const HistogramLayout &layout, std::size_t index)
{
    std::vector<Rect> rects = HistogramBinRects(layout);
    if (index >= rects.size()) {
        return {};
    }
    return rects[index];
}

std::optional<std::size_t> HistogramBinIndexAtPoint(const HistogramLayout &layout,
                                                    const Point &point)
{
    if (!IsValidHistogramLayout(layout) ||
        !std::isfinite(point.x) ||
        !std::isfinite(point.y) ||
        !PointInRect(point, layout.plotRect)) {
        return std::nullopt;
    }
    const std::size_t n = !layout.values.empty() ? layout.values.size() : layout.counts.size();
    const double slotWidth = layout.plotRect.width / static_cast<double>(std::max<std::size_t>(1, n));
    std::size_t index = static_cast<std::size_t>(std::floor((point.x - layout.plotRect.x) / slotWidth));
    index = std::min(index, n - 1);
    return index;
}

std::optional<std::pair<std::size_t, std::size_t>> HistogramBinRangeForGesture(
    const HistogramLayout &layout,
    const Point &start,
    const Point &current)
{
    std::optional<std::size_t> a = HistogramBinIndexAtPoint(layout, start);
    std::optional<std::size_t> b = HistogramBinIndexAtPoint(layout, current);
    if (!a.has_value() || !b.has_value()) {
        return std::nullopt;
    }
    return std::make_pair(std::min(*a, *b), std::max(*a, *b));
}

std::optional<Rect> HistogramDragHighlightRect(
    const HistogramLayout &layout,
    const Point &start,
    const Point &current)
{
    std::optional<std::pair<std::size_t, std::size_t>> range =
        HistogramBinRangeForGesture(layout, start, current);
    if (!range.has_value()) {
        return std::nullopt;
    }
    Rect lo = HistogramBinRect(layout, range->first);
    Rect hi = HistogramBinRect(layout, range->second);
    if (!std::isfinite(lo.x) || !std::isfinite(hi.x) ||
        !std::isfinite(lo.width) || !std::isfinite(hi.width) ||
        lo.width <= 0.0 || hi.width <= 0.0) {
        return std::nullopt;
    }
    const double x1 = std::min(lo.x, hi.x);
    const double x2 = std::max(lo.x + lo.width, hi.x + hi.width);
    return Rect{x1, layout.plotRect.y, x2 - x1, layout.plotRect.height};
}

std::set<CaseId> SelectHistogramCasesForGesture(
    const std::vector<std::vector<CaseId>> &binRows,
    const HistogramLayout &layout,
    const Point &start,
    const Point &current)
{
    std::set<CaseId> selected;
    std::optional<std::pair<std::size_t, std::size_t>> range =
        HistogramBinRangeForGesture(layout, start, current);
    if (!range.has_value()) {
        return selected;
    }
    for (std::size_t i = range->first; i <= range->second && i < binRows.size(); ++i) {
        for (CaseId row : binRows[i]) {
            if (row > 0) {
                selected.insert(row);
            }
        }
    }
    return selected;
}

int HistogramMaximumBinCount(const std::vector<std::vector<CaseId>> &binRows)
{
    int maximum = 1;
    for (const std::vector<CaseId> &rows : binRows) {
        maximum = std::max(maximum, static_cast<int>(rows.size()));
    }
    return maximum;
}

double QuantileSorted(const std::vector<double> &sortedValues, double probability)
{
    if (sortedValues.empty()) {
        return 0.0;
    }
    if (sortedValues.size() == 1) {
        return sortedValues.front();
    }
    probability = std::max(0.0, std::min(1.0, probability));
    double h = static_cast<double>(sortedValues.size() - 1) * probability;
    std::size_t lo = static_cast<std::size_t>(std::floor(h));
    std::size_t hi = static_cast<std::size_t>(std::ceil(h));
    double fraction = h - static_cast<double>(lo);
    return sortedValues[lo] * (1.0 - fraction) + sortedValues[hi] * fraction;
}

int HistogramBinCountForRule(const std::vector<double> &values, const std::string &rule)
{
    std::vector<double> finite = FiniteValues(values);
    if (finite.empty()) {
        return 1;
    }
    double minValue = *std::min_element(finite.begin(), finite.end());
    double maxValue = *std::max_element(finite.begin(), finite.end());
    double width = maxValue - minValue;
    int n = static_cast<int>(finite.size());
    if (rule == "sqrt") {
        return std::max(1, static_cast<int>(std::ceil(std::sqrt(static_cast<double>(n)))));
    }
    if ((rule == "fd" || rule == "scott") && width > 0.0) {
        std::sort(finite.begin(), finite.end());
        double h = 0.0;
        if (rule == "fd") {
            h = 2.0 * (QuantileSorted(finite, 0.75) - QuantileSorted(finite, 0.25)) /
                std::pow(static_cast<double>(n), 1.0 / 3.0);
        } else {
            double mean = 0.0;
            for (double value : finite) {
                mean += value;
            }
            mean /= static_cast<double>(n);
            double ss = 0.0;
            for (double value : finite) {
                ss += (value - mean) * (value - mean);
            }
            double sd = n > 1 ? std::sqrt(ss / static_cast<double>(n - 1)) : 0.0;
            h = 3.5 * sd / std::pow(static_cast<double>(n), 1.0 / 3.0);
        }
        if (std::isfinite(h) && h > 0.0) {
            return std::max(1, static_cast<int>(std::ceil(width / h)));
        }
    }
    return std::max(1, static_cast<int>(std::ceil(std::log2(static_cast<double>(n)) + 1.0)));
}

bool RebinHistogramCases(const std::vector<HistogramCaseValue> &cases,
                         int binCount,
                         HistogramBinningResult &result)
{
    result = HistogramBinningResult();
    if (cases.empty()) {
        return false;
    }
    binCount = std::max(1, binCount);
    std::vector<double> values;
    values.reserve(cases.size());
    for (const HistogramCaseValue &point : cases) {
        if (std::isfinite(point.x)) {
            values.push_back(point.x);
        }
    }
    if (values.empty()) {
        return false;
    }
    double minValue = *std::min_element(values.begin(), values.end());
    double maxValue = *std::max_element(values.begin(), values.end());
    if (minValue == maxValue) {
        minValue -= 0.5;
        maxValue += 0.5;
    }
    const double width = (maxValue - minValue) / static_cast<double>(binCount);
    if (!(width > 0.0) || !std::isfinite(width)) {
        return false;
    }

    result.minimum = minValue;
    result.maximum = maxValue;
    result.bins.resize(static_cast<std::size_t>(binCount));
    result.pointBins.assign(cases.size(), 0);
    for (int i = 0; i < binCount; ++i) {
        result.bins[static_cast<std::size_t>(i)].lower = minValue + width * static_cast<double>(i);
        result.bins[static_cast<std::size_t>(i)].upper =
            i == binCount - 1 ? maxValue : minValue + width * static_cast<double>(i + 1);
    }
    for (std::size_t i = 0; i < cases.size(); ++i) {
        const HistogramCaseValue &point = cases[i];
        if (!std::isfinite(point.x)) {
            continue;
        }
        int bin = static_cast<int>(std::floor((point.x - minValue) / width)) + 1;
        if (point.x == maxValue) {
            bin = binCount;
        }
        bin = std::max(1, std::min(binCount, bin));
        result.pointBins[i] = bin;
        result.bins[static_cast<std::size_t>(bin - 1)].rows.push_back(point.row);
    }
    return true;
}

double HistogramAverageBinWidth(const std::vector<HistogramBinData> &bins)
{
    if (bins.empty()) {
        return 1.0;
    }
    double total = 0.0;
    int count = 0;
    for (const HistogramBinData &bin : bins) {
        double width = bin.upper - bin.lower;
        if (std::isfinite(width) && width > 0.0) {
            total += width;
            ++count;
        }
    }
    return count > 0 ? total / static_cast<double>(count) : 1.0;
}

std::vector<HistogramBarRenderItem> BuildHistogramBarRenderPlan(
    const HistogramLayout &layout,
    const std::vector<std::vector<CaseId>> &binRows,
    const std::set<CaseId> &selection,
    const std::map<CaseId, std::string> &rowColors,
    bool showCounts,
    bool showColorSegments)
{
    std::vector<HistogramBarRenderItem> plan;
    std::vector<Rect> rects = HistogramBinRects(layout);
    if (rects.empty()) {
        return plan;
    }

    plan.reserve(rects.size());
    for (std::size_t i = 0; i < rects.size(); ++i) {
        const std::vector<CaseId> emptyRows;
        const std::vector<CaseId> &rows = i < binRows.size() ? binRows[i] : emptyRows;
        HistogramBarRenderItem item;
        item.rect = rects[i];
        item.count = static_cast<int>(rows.size());
        item.showCount = showCounts && item.rect.height > 14.0;
        if (item.showCount) {
            item.countLabel = std::to_string(rows.size());
        }

        if (showColorSegments) {
            std::map<std::string, int> colorCounts;
            int selectedCount = 0;
            for (CaseId row : rows) {
                auto colorIt = rowColors.find(row);
                if (colorIt != rowColors.end() && !colorIt->second.empty()) {
                    colorCounts[colorIt->second] += 1;
                }
                if (selection.find(row) != selection.end()) {
                    selectedCount += 1;
                }
            }
            const double maximum = !layout.values.empty()
                ? std::max(1.0, layout.maxValue)
                : static_cast<double>(std::max(1, layout.maxCount));
            const double baseline = layout.plotRect.y + layout.plotRect.height;
            auto subsetRect = [&](int count) {
                const double height = layout.plotRect.height * static_cast<double>(count) / maximum;
                return Rect{item.rect.x, baseline - height, item.rect.width, height};
            };
            for (const auto &entry : colorCounts) {
                Rect overlay = subsetRect(entry.second);
                if (IsValidRect(overlay) && overlay.height > 0.0) {
                    item.colorSegments.push_back(
                        HistogramBarSegment{overlay, entry.first, 0.18, false});
                }
            }
            if (selectedCount > 0) {
                Rect overlay = subsetRect(selectedCount);
                if (IsValidRect(overlay) && overlay.height > 0.0) {
                    item.selectedSegments.push_back(
                        HistogramBarSegment{overlay, std::string(), 0.30, true});
                }
            }
        }

        plan.push_back(item);
    }
    return plan;
}

std::vector<HistogramRugRenderItem> BuildHistogramRugRenderPlan(
    const HistogramLayout &layout,
    const std::vector<HistogramRugCase> &cases,
    double rugHeight)
{
    std::vector<HistogramRugRenderItem> plan;
    if (!IsValidHistogramLayout(layout) || !(rugHeight > 0.0) || !std::isfinite(rugHeight)) {
        return plan;
    }

    std::vector<Rect> rects = HistogramBinRects(layout);
    if (rects.empty()) {
        return plan;
    }

    plan.reserve(cases.size());
    const double baseline = layout.plotRect.y + layout.plotRect.height;
    for (const HistogramRugCase &rugCase : cases) {
        if (rugCase.caseId <= 0 || rugCase.binIndex >= rects.size()) {
            continue;
        }
        const Rect &rect = rects[rugCase.binIndex];
        if (!IsValidRect(rect) || !(rect.width > 0.0)) {
            continue;
        }
        const double x = rect.x + rect.width * 0.5;
        plan.push_back({
            rugCase.caseId,
            Point{x, baseline},
            Point{x, baseline - rugHeight}
        });
    }
    return plan;
}

HistogramDensityRenderPlan BuildHistogramDensityRenderPlan(
    const Rect &plotRect,
    const std::vector<HistogramDensityCurve> &curves,
    double xMinimum,
    double xMaximum,
    double maximumHeightFraction)
{
    HistogramDensityRenderPlan plan;
    plan.xMinimum = xMinimum;
    plan.xMaximum = xMaximum;
    if (!IsValidRect(plotRect) ||
        curves.empty() ||
        !std::isfinite(xMinimum) ||
        !std::isfinite(xMaximum) ||
        xMinimum == xMaximum) {
        return plan;
    }

    maximumHeightFraction = (maximumHeightFraction > 0.0 && maximumHeightFraction <= 1.0)
        ? maximumHeightFraction
        : 0.45;
    double densityMax = 0.0;
    for (const HistogramDensityCurve &curve : curves) {
        for (double y : curve.y) {
            if (std::isfinite(y)) {
                densityMax = std::max(densityMax, y);
            }
        }
    }
    plan.yMaximum = std::max(1.0e-12, densityMax / maximumHeightFraction);
    if (!(densityMax > 0.0)) {
        return plan;
    }

    auto pointForDensity = [&](double x, double y) -> Point {
        return DataToScreen({x, y}, {xMinimum, xMaximum, 0.0, plan.yMaximum}, plotRect, true);
    };

    plan.curves.reserve(curves.size());
    for (const HistogramDensityCurve &curve : curves) {
        const std::size_t n = std::min(curve.x.size(), curve.y.size());
        if (n < 2) {
            continue;
        }
        HistogramDensityRenderItem item;
        item.kind = curve.kind;
        item.colorName = curve.colorName;
        item.label = curve.label;
        item.n = curve.n;
        item.linePoints.reserve(n);
        for (std::size_t i = 0; i < n; ++i) {
            if (std::isfinite(curve.x[i]) && std::isfinite(curve.y[i])) {
                item.linePoints.push_back(pointForDensity(curve.x[i], curve.y[i]));
            }
        }
        if (item.linePoints.size() < 2) {
            continue;
        }
        item.fillPolygon = item.linePoints;
        item.fillPolygon.push_back(pointForDensity(curve.x[n - 1], 0.0));
        item.fillPolygon.push_back(pointForDensity(curve.x[0], 0.0));
        plan.curves.push_back(std::move(item));
    }

    for (std::size_t i = 0; i < curves.size(); ++i) {
        const HistogramDensityCurve &a = curves[i];
        if (a.kind != "color_group") {
            continue;
        }
        for (std::size_t j = i + 1; j < curves.size(); ++j) {
            const HistogramDensityCurve &b = curves[j];
            if (b.kind != "color_group") {
                continue;
            }
            const std::size_t n = std::min(a.x.size(), std::min(a.y.size(), b.y.size()));
            if (n < 2) {
                continue;
            }
            HistogramDensityOverlapRenderItem overlap;
            overlap.firstColorName = a.colorName;
            overlap.secondColorName = b.colorName;
            overlap.fillPolygon.reserve(n + 2);
            for (std::size_t k = 0; k < n; ++k) {
                if (std::isfinite(a.x[k]) && std::isfinite(a.y[k]) && std::isfinite(b.y[k])) {
                    overlap.fillPolygon.push_back(pointForDensity(a.x[k], std::min(a.y[k], b.y[k])));
                }
            }
            if (overlap.fillPolygon.size() < 2) {
                continue;
            }
            overlap.fillPolygon.push_back(pointForDensity(a.x[n - 1], 0.0));
            overlap.fillPolygon.push_back(pointForDensity(a.x[0], 0.0));
            plan.overlaps.push_back(std::move(overlap));
        }
    }

    return plan;
}

HistogramRenderPlan BuildHistogramRenderPlan(const HistogramRenderInput &input)
{
    HistogramRenderPlan plan;
    plan.bars = BuildHistogramBarRenderPlan(
        input.layout,
        input.binRows,
        input.selection,
        input.rowColors,
        input.showCounts,
        input.showColorSegments);
    if (input.showRug) {
        plan.rugs = BuildHistogramRugRenderPlan(input.layout, input.rugCases);
    }
    plan.density = BuildHistogramDensityRenderPlan(
        input.layout.plotRect,
        input.densityCurves,
        input.densityXMinimum,
        input.densityXMaximum,
        input.densityMaximumHeightFraction);
    return plan;
}

bool HistogramDensityModeIsValid(const std::string &mode)
{
    return mode == "none" || mode == "all" || mode == "selected" ||
        mode == "colors" || mode == "selected_and_colors";
}

bool HistogramDensityDisplayActive(bool showDensity,
                                   const std::string &densityMode)
{
    return showDensity &&
        densityMode != "none" &&
        HistogramDensityModeIsValid(densityMode);
}

bool HistogramColorSegmentsVisible(bool showDensity,
                                   const std::string &densityMode)
{
    (void)showDensity;
    (void)densityMode;
    return true;
}

std::vector<HistogramMenuOption> HistogramDensityModeMenuOptions()
{
    return {
        {"All Cases", "all", "HIST_SET_DENSITY_MODE|all"},
        {"Selected Cases", "selected", "HIST_SET_DENSITY_MODE|selected"},
        {"Color Groups", "colors", "HIST_SET_DENSITY_MODE|colors"},
        {"Selected + Color Groups", "selected_and_colors", "HIST_SET_DENSITY_MODE|selected_and_colors"}
    };
}

HistogramMenuState BuildHistogramMenuState(const std::string &xLabel,
                                           bool showCounts,
                                           bool showRug,
                                           bool showDensity,
                                           const std::string &densityMode)
{
    HistogramMenuState state;
    state.counts = {
        showCounts ? "Hide Counts" : "Show Counts",
        showCounts ? "hide_counts" : "show_counts",
        "HIST_TOGGLE_COUNTS"
    };
    state.rug = {
        showRug ? "Hide Rug" : "Show Rug",
        showRug ? "hide_rug" : "show_rug",
        "HIST_TOGGLE_RUG"
    };
    state.densityToggle = {
        showDensity ? "Hide Density Curves" : "Show Density Curves",
        showDensity ? "hide_density" : "show_density",
        "HIST_TOGGLE_DENSITY"
    };
    state.densityModes = HistogramDensityModeMenuOptions();
    for (HistogramMenuOption &option : state.densityModes) {
        option.checked = option.value == densityMode;
    }
    state.densityBandwidth = {"Bandwidth...", "bandwidth", "HIST_SET_DENSITY_BW"};
    state.densityAdjust = {"Smoother / Adjust...", "adjust", "HIST_SET_DENSITY_ADJUST"};
    const std::string variable = xLabel.empty() ? "variable" : xLabel;
    state.frequencyTable = {
        "Frequency table: " + variable,
        "frequency_table",
        "CONTEXT_HISTOGRAM_FREQUENCY"
    };
    state.descriptives = {
        "Descriptives: " + variable,
        "descriptives",
        "CONTEXT_HISTOGRAM_DESCRIPTIVES"
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

HistogramCreationDialogState BuildHistogramCreationDialogState()
{
    return HistogramCreationDialogState{};
}

std::string HistogramDefaultTitle(const std::string &xVariable)
{
    return "Histogram of " + xVariable;
}

HistogramDensityParameterDialogState BuildHistogramDensityBandwidthDialogState(
    double currentBandwidth)
{
    HistogramDensityParameterDialogState state;
    state.title = "Histogram Density Bandwidth";
    state.informativeText = "Leave empty to use the automatic bandwidth.";
    if (currentBandwidth > 0.0 && std::isfinite(currentBandwidth)) {
        state.currentValueText = FormatHistogramDialogDouble(currentBandwidth);
    }
    return state;
}

HistogramDensityParameterDialogState BuildHistogramDensityAdjustDialogState(
    double currentAdjust)
{
    HistogramDensityParameterDialogState state;
    state.title = "Histogram Density Smoother";
    state.informativeText = "Use values above 1 for smoother curves and below 1 for less smoothing.";
    if (currentAdjust > 0.0 && std::isfinite(currentAdjust)) {
        state.currentValueText = FormatHistogramDialogDouble(currentAdjust);
    }
    return state;
}

HistogramDensityState HistogramStateAfterToggleDensity(bool currentShowDensity,
                                                       const std::string &currentDensityMode)
{
    HistogramDensityState state;
    state.showDensity = !currentShowDensity;
    state.densityMode = HistogramDensityModeIsValid(currentDensityMode)
        ? currentDensityMode
        : "all";
    if (state.showDensity && state.densityMode == "none") {
        state.densityMode = "all";
    }
    state.changed = true;
    return state;
}

HistogramDensityState HistogramStateAfterSetDensityMode(bool currentShowDensity,
                                                        const std::string &currentDensityMode,
                                                        const std::string &requestedDensityMode)
{
    HistogramDensityState state;
    state.showDensity = currentShowDensity;
    state.densityMode = HistogramDensityModeIsValid(currentDensityMode)
        ? currentDensityMode
        : "all";
    if (!HistogramDensityModeIsValid(requestedDensityMode)) {
        return state;
    }
    state.densityMode = requestedDensityMode;
    state.showDensity = requestedDensityMode != "none";
    state.changed = state.showDensity != currentShowDensity ||
        state.densityMode != currentDensityMode;
    return state;
}

std::optional<double> ParseHistogramDensityBandwidthCommandValue(const std::string &text)
{
    if (text.empty()) {
        return 0.0;
    }
    std::optional<double> value = ParseFiniteDouble(text);
    if (!value.has_value()) {
        return std::nullopt;
    }
    return std::max(0.0, *value);
}

std::optional<double> ParseHistogramDensityAdjustCommandValue(const std::string &text)
{
    std::optional<double> value = ParseFiniteDouble(text);
    if (!value.has_value() || !(*value > 0.0)) {
        return std::nullopt;
    }
    return *value;
}

HistogramDensityCurve DensityCurveForValues(const std::vector<double> &values,
                                            const std::string &kind,
                                            const std::string &colorName,
                                            const std::string &label,
                                            double xmin,
                                            double xmax,
                                            double bandwidth,
                                            double adjust)
{
    HistogramDensityCurve curve;
    curve.kind = kind;
    curve.colorName = colorName;
    curve.label = label;
    std::vector<double> finite = FiniteValues(values);
    curve.n = static_cast<int>(finite.size());
    if (finite.size() < 2 || !std::isfinite(xmin) || !std::isfinite(xmax) || xmin == xmax) {
        return curve;
    }

    double mean = 0.0;
    for (double value : finite) {
        mean += value;
    }
    mean /= static_cast<double>(finite.size());
    double ss = 0.0;
    for (double value : finite) {
        ss += (value - mean) * (value - mean);
    }
    double sd = finite.size() > 1 ? std::sqrt(ss / static_cast<double>(finite.size() - 1)) : 0.0;
    double range = xmax - xmin;
    if (!(bandwidth > 0.0 && std::isfinite(bandwidth))) {
        bandwidth = 1.06 * sd * std::pow(static_cast<double>(finite.size()), -0.2);
        if (!(bandwidth > 0.0 && std::isfinite(bandwidth))) {
            bandwidth = range / 24.0;
        }
    }
    adjust = (adjust > 0.0 && std::isfinite(adjust)) ? adjust : 1.0;
    bandwidth *= adjust;
    if (!(bandwidth > 0.0 && std::isfinite(bandwidth))) {
        return curve;
    }

    const int grid = 160;
    const double invSqrt2Pi = 0.3989422804014327;
    curve.x.reserve(grid);
    curve.y.reserve(grid);
    for (int i = 0; i < grid; ++i) {
        double x = xmin + range * static_cast<double>(i) / static_cast<double>(grid - 1);
        double sum = 0.0;
        for (double value : finite) {
            double z = (x - value) / bandwidth;
            sum += std::exp(-0.5 * z * z);
        }
        curve.x.push_back(x);
        curve.y.push_back(sum * invSqrt2Pi / (static_cast<double>(finite.size()) * bandwidth));
    }
    return curve;
}

std::vector<HistogramDensityCurve> DensityCurvesForHistogramCases(
    const std::vector<HistogramCaseValue> &cases,
    double xmin,
    double xmax,
    bool showDensity,
    const std::string &mode,
    double bandwidth,
    double adjust,
    const std::set<CaseId> &selection,
    const std::map<CaseId, std::string> &rowColors)
{
    std::vector<HistogramDensityCurve> curves;
    if (!showDensity || mode == "none" || !HistogramDensityModeIsValid(mode) ||
        cases.size() < 2 || !std::isfinite(xmin) || !std::isfinite(xmax)) {
        return curves;
    }

    std::vector<double> allValues;
    std::vector<double> selectedValues;
    std::map<std::string, std::vector<double>> colorValues;
    allValues.reserve(cases.size());
    for (const HistogramCaseValue &point : cases) {
        if (!std::isfinite(point.x)) {
            continue;
        }
        allValues.push_back(point.x);
        if (selection.find(point.row) != selection.end()) {
            selectedValues.push_back(point.x);
        }
        auto colorIt = rowColors.find(point.row);
        if (colorIt != rowColors.end() && !colorIt->second.empty()) {
            colorValues[colorIt->second].push_back(point.x);
        }
    }

    if (mode == "all") {
        curves.push_back(DensityCurveForValues(allValues, "all", "", "All Cases", xmin, xmax, bandwidth, adjust));
    } else if (mode == "selected") {
        curves.push_back(DensityCurveForValues(selectedValues, "selected", "", "Selected Cases", xmin, xmax, bandwidth, adjust));
    } else if (mode == "colors" || mode == "selected_and_colors") {
        for (const auto &entry : colorValues) {
            curves.push_back(DensityCurveForValues(entry.second, "color_group", entry.first, entry.first, xmin, xmax, bandwidth, adjust));
        }
        if (mode == "selected_and_colors") {
            curves.push_back(DensityCurveForValues(selectedValues, "selected", "", "Selected Cases", xmin, xmax, bandwidth, adjust));
        }
    }

    curves.erase(std::remove_if(curves.begin(), curves.end(), [](const HistogramDensityCurve &curve) {
        return curve.x.empty() || curve.y.empty() || curve.n < 2;
    }), curves.end());
    return curves;
}

std::string HistogramNoFiniteValuesStatus()
{
    return "The selected variable has no finite values to plot.";
}

std::string HistogramXFieldLabel()
{
    return "X:";
}

std::string HistogramBinsFieldLabel()
{
    return "Bins:";
}

std::string HistogramWindowTitle()
{
    return "Histogram";
}

void RebinHistogram(PlotModel &model, int binCount)
{
    if (model.histogramPoints.empty()) {
        return;
    }
    std::vector<HistogramCaseValue> cases;
    cases.reserve(model.histogramPoints.size());
    for (const HistogramPoint &point : model.histogramPoints) {
        cases.push_back({point.x, point.row});
    }
    HistogramBinningResult result;
    if (!RebinHistogramCases(cases, binCount, result)) {
        return;
    }
    model.histogramBins.clear();
    model.histogramBins.reserve(result.bins.size());
    for (const HistogramBinData &bin : result.bins) {
        HistogramBin nativeBin;
        nativeBin.lower = bin.lower;
        nativeBin.upper = bin.upper;
        nativeBin.rows.assign(bin.rows.begin(), bin.rows.end());
        model.histogramBins.push_back(nativeBin);
    }
    for (size_t i = 0; i < model.histogramPoints.size(); ++i) {
        model.histogramPoints[i].bin = i < result.pointBins.size() ? result.pointBins[i] : 0;
    }
}

void RebinHistogramByRule(PlotModel &model, const std::string &rule)
{
    std::vector<double> values;
    values.reserve(model.histogramPoints.size());
    for (const HistogramPoint &point : model.histogramPoints) {
        if (std::isfinite(point.x)) {
            values.push_back(point.x);
        }
    }
    RebinHistogram(model, HistogramBinCountForRule(values, rule));
}

void RebuildHistogramPointsFromCurrentVariable(PlotModel &model)
{
    const NumericVariable *xvar = FindNumericVariable(model, model.xLabel);
    model.histogramPoints.clear();
    if (!xvar) {
        model.histogramBins.clear();
        return;
    }
    model.histogramPoints.reserve(xvar->values.size());
    for (std::size_t i = 0; i < xvar->values.size(); ++i) {
        double x = xvar->values[i];
        if (std::isfinite(x)) {
            model.histogramPoints.push_back(HistogramPoint{x, static_cast<int>(i) + 1, 0});
        }
    }
    const int binCount = static_cast<int>(model.histogramBins.size());
    if (binCount < 1) {
        RebinHistogramByRule(model, "sturges");
    } else {
        RebinHistogram(model, binCount);
    }
}

double HistogramAverageBinWidth(const PlotModel &model)
{
    std::vector<HistogramBinData> bins;
    bins.reserve(model.histogramBins.size());
    for (const HistogramBin &bin : model.histogramBins) {
        HistogramBinData coreBin;
        coreBin.lower = bin.lower;
        coreBin.upper = bin.upper;
        coreBin.rows.assign(bin.rows.begin(), bin.rows.end());
        bins.push_back(coreBin);
    }
    return HistogramAverageBinWidth(bins);
}

std::string HistogramBreaksResponseText(const PlotModel &model)
{
    std::ostringstream out;
    out << "OK";
    if (!model.histogramBins.empty()) {
        out << "\t" << std::setprecision(17) << model.histogramBins.front().lower;
        for (const HistogramBin &bin : model.histogramBins) {
            out << "\t" << std::setprecision(17) << bin.upper;
        }
    }
    return out.str();
}

std::string HistogramDensityInfoResponseText(const PlotModel &model)
{
    std::ostringstream out;
    out << "OK\t" << (model.histogramShowDensity ? "TRUE" : "FALSE")
        << "|" << model.histogramDensityMode
        << "|" << std::setprecision(17) << model.histogramDensityBw
        << "|" << std::setprecision(17) << model.histogramDensityAdjust;
    return out.str();
}

std::vector<HistogramDensityCurve> DensityCurvesForHistogram(
    PlotModel &model,
    const std::set<CaseId> &selection,
    const std::map<CaseId, std::string> &rowColors)
{
    if (!HistogramDensityDisplayActive(model.histogramShowDensity, model.histogramDensityMode) ||
        model.histogramPoints.size() < 2 || model.histogramBins.empty()) {
        return {};
    }
    double xmin = model.histogramBins.front().lower;
    double xmax = model.histogramBins.back().upper;
    std::vector<HistogramCaseValue> cases;
    cases.reserve(model.histogramPoints.size());
    for (const HistogramPoint &point : model.histogramPoints) {
        if (std::isfinite(point.x)) {
            cases.push_back({point.x, point.row});
        }
    }
    return DensityCurvesForHistogramCases(
        cases, xmin, xmax,
        model.histogramShowDensity, model.histogramDensityMode,
        model.histogramDensityBw, model.histogramDensityAdjust,
        selection, rowColors);
}

} // namespace core
} // namespace rlispstat
