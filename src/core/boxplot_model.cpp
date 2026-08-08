#include "boxplot_model.h"

#include "command_model.h"
#include "dataset_model.h"
#include "format_model.h"

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <map>
#include <random>
#include <sstream>

namespace rlispstat {
namespace core {

namespace {

double BoxplotJitterForCase(CaseId caseId, double width)
{
    const std::uint64_t row = static_cast<std::uint64_t>(std::max<CaseId>(0, caseId));
    const double fraction = static_cast<double>((row * 1103515245ull + 12345ull) % 1000ull) / 1000.0;
    return (fraction - 0.5) * width;
}

} // namespace

bool IsValidBoxplotLayout(const BoxplotLayout &layout)
{
    return IsValidRect(layout.plotRect) &&
        !layout.categories.empty() &&
        std::isfinite(layout.yMinimum) &&
        std::isfinite(layout.yMaximum);
}

DataViewport BoxplotViewport(const BoxplotLayout &layout)
{
    double ymin = layout.yMinimum;
    double ymax = layout.yMaximum;
    if (!std::isfinite(ymin) || !std::isfinite(ymax)) {
        return {};
    }
    if (ymin == ymax) {
        ymin -= 0.5;
        ymax += 0.5;
    }
    const double pad = (ymax - ymin) * std::max(0.0, layout.yPaddingFraction);
    return {0.0, 1.0, ymin - pad, ymax + pad};
}

BoxplotDisplayRange BoxplotDisplayRangeForCases(
    const std::vector<BoxplotCase> &cases,
    bool includeH0Simulation,
    const std::vector<double> &h0Values,
    const std::vector<double> &h0ReferenceLines,
    double h0Lower,
    double h0Upper)
{
    BoxplotDisplayRange range;
    auto includeValue = [&](double value) {
        if (!std::isfinite(value)) {
            return;
        }
        if (!range.hasFiniteValue) {
            range.minimum = value;
            range.maximum = value;
            range.hasFiniteValue = true;
            return;
        }
        range.minimum = std::min(range.minimum, value);
        range.maximum = std::max(range.maximum, value);
    };

    for (const BoxplotCase &boxplotCase : cases) {
        includeValue(boxplotCase.value);
    }
    if (includeH0Simulation && range.hasFiniteValue) {
        for (double value : h0Values) {
            includeValue(value);
        }
        for (double value : h0ReferenceLines) {
            includeValue(value);
        }
        includeValue(h0Lower);
        includeValue(h0Upper);
    }
    return range;
}

std::vector<BoxplotAxisTick> BoxplotYAxisTicks(double minimum,
                                               double maximum,
                                               int targetCount)
{
    std::vector<BoxplotAxisTick> ticks;
    if (!std::isfinite(minimum) || !std::isfinite(maximum) || targetCount < 2) return ticks;
    if (minimum > maximum) std::swap(minimum, maximum);
    if (minimum == maximum) {
        double span = std::max(1.0, std::fabs(minimum) * 0.1);
        minimum -= span / 2.0;
        maximum += span / 2.0;
    }
    const double raw = (maximum - minimum) / static_cast<double>(targetCount - 1);
    const double power = std::pow(10.0, std::floor(std::log10(raw)));
    const double scaled = raw / power;
    double nice = 10.0;
    if (scaled <= 1.0) nice = 1.0;
    else if (scaled <= 2.0) nice = 2.0;
    else if (scaled <= 2.5) nice = 2.5;
    else if (scaled <= 5.0) nice = 5.0;
    const double step = nice * power;
    const double first = std::ceil(minimum / step - 1.0e-12) * step;
    const double last = std::floor(maximum / step + 1.0e-12) * step;
    for (double value = first; value <= last + step * 1.0e-8 && ticks.size() < 12;
         value += step) {
        double normalized = std::fabs(value) < step * 1.0e-10 ? 0.0 : value;
        ticks.push_back({normalized, FormatModelNumber(normalized)});
    }
    if (ticks.size() < 4) {
        ticks.clear();
        const double finer = step / 2.0;
        const double finerFirst = std::ceil(minimum / finer - 1.0e-12) * finer;
        const double finerLast = std::floor(maximum / finer + 1.0e-12) * finer;
        for (double value = finerFirst; value <= finerLast + finer * 1.0e-8 && ticks.size() < 12;
             value += finer) {
            double normalized = std::fabs(value) < finer * 1.0e-10 ? 0.0 : value;
            ticks.push_back({normalized, FormatModelNumber(normalized)});
        }
    }
    return ticks;
}

static bool BoxplotNumericLabel(const std::string &label, double &value)
{
    char *end = nullptr;
    value = std::strtod(label.c_str(), &end);
    return end && end != label.c_str() && *end == '\0' && std::isfinite(value);
}

std::vector<std::string> OrderedBoxplotCategories(
    const std::vector<std::string> &definedCategories,
    const std::vector<BoxplotCase> &cases,
    const std::string &order)
{
    std::vector<std::string> categories = definedCategories.empty()
        ? BoxplotCategoriesForCases(cases) : definedCategories;
    if (order == "defined" || categories.size() < 2) return categories;
    std::map<std::string, std::size_t> original;
    for (std::size_t i = 0; i < categories.size(); ++i) original[categories[i]] = i;
    const bool descending = order == "label_desc" || order == "median_desc";
    if (order == "label_asc" || order == "label_desc") {
        bool allNumeric = true;
        std::map<std::string, double> numbers;
        for (const std::string &category : categories) {
            double value = NAN;
            if (!BoxplotNumericLabel(category, value)) allNumeric = false;
            numbers[category] = value;
        }
        std::stable_sort(categories.begin(), categories.end(), [&](const std::string &a, const std::string &b) {
            int comparison = 0;
            if (allNumeric) comparison = numbers[a] < numbers[b] ? -1 : (numbers[a] > numbers[b] ? 1 : 0);
            else comparison = a < b ? -1 : (a > b ? 1 : 0);
            return descending ? comparison > 0 : comparison < 0;
        });
        return categories;
    }
    if (order == "median_asc" || order == "median_desc") {
        std::map<std::string, std::vector<double>> values = BoxplotValuesByCategory(cases);
        std::map<std::string, double> medians;
        for (const std::string &category : categories) {
            std::vector<double> current = values[category];
            std::sort(current.begin(), current.end());
            medians[category] = current.empty() ? NAN : BoxplotQuantileSorted(current, 0.5);
        }
        std::stable_sort(categories.begin(), categories.end(), [&](const std::string &a, const std::string &b) {
            const bool af = std::isfinite(medians[a]);
            const bool bf = std::isfinite(medians[b]);
            if (af != bf) return af;
            if (!af) return original[a] < original[b];
            if (medians[a] == medians[b]) return original[a] < original[b];
            return descending ? medians[a] > medians[b] : medians[a] < medians[b];
        });
    }
    return categories;
}

std::vector<BoxplotMenuOption> BoxplotGroupOrderMenuOptions(const std::string &currentOrder)
{
    std::vector<BoxplotMenuOption> options = {
        {"As defined", "defined", "BOXPLOT_GROUP_ORDER|defined"},
        {"Label ascending", "label_asc", "BOXPLOT_GROUP_ORDER|label_asc"},
        {"Label descending", "label_desc", "BOXPLOT_GROUP_ORDER|label_desc"},
        {"Median ascending", "median_asc", "BOXPLOT_GROUP_ORDER|median_asc"},
        {"Median descending", "median_desc", "BOXPLOT_GROUP_ORDER|median_desc"}
    };
    for (BoxplotMenuOption &option : options) option.checked = option.value == currentOrder;
    return options;
}

double BoxplotCategoryCenter(const BoxplotLayout &layout, const std::string &category)
{
    if (!IsValidBoxplotLayout(layout)) {
        return NAN;
    }
    auto it = std::find(layout.categories.begin(), layout.categories.end(), category);
    const std::size_t index = it == layout.categories.end()
        ? 0
        : static_cast<std::size_t>(it - layout.categories.begin());
    const std::size_t slots = std::max<std::size_t>(1, layout.categories.size());
    return layout.plotRect.x + layout.plotRect.width *
        ((static_cast<double>(index) + 0.5) / static_cast<double>(slots));
}

double BoxplotH0SimulationCenterX(const BoxplotLayout &layout)
{
    if (!IsValidRect(layout.plotRect)) {
        return NAN;
    }
    if (layout.categories.empty()) {
        return layout.plotRect.x + layout.plotRect.width / 2.0;
    }
    if (layout.categories.size() == 1) {
        return BoxplotCategoryCenter(layout, layout.categories.front());
    }
    if (layout.categories.size() == 2 && !layout.variableAxes) {
        return (BoxplotCategoryCenter(layout, layout.categories[0]) +
                BoxplotCategoryCenter(layout, layout.categories[1])) / 2.0;
    }
    return BoxplotCategoryCenter(layout, layout.categories.front());
}

std::vector<BoxplotValueInterval> BoxplotTailIntervalsForAlternative(
    double displayMinimum,
    double displayMaximum,
    double lowerCut,
    double upperCut,
    const std::string &alternative)
{
    std::vector<BoxplotValueInterval> intervals;
    if (!std::isfinite(displayMinimum) ||
        !std::isfinite(displayMaximum) ||
        displayMinimum >= displayMaximum) {
        return intervals;
    }

    auto addInterval = [&](double lower, double upper) {
        if (!std::isfinite(lower) || !std::isfinite(upper)) {
            return;
        }
        lower = std::max(displayMinimum, lower);
        upper = std::min(displayMaximum, upper);
        if (lower < upper) {
            intervals.push_back({lower, upper});
        }
    };

    if (alternative == "two.sided") {
        addInterval(displayMinimum, lowerCut);
        addInterval(upperCut, displayMaximum);
    } else if (alternative == "greater") {
        addInterval(lowerCut, displayMaximum);
    } else {
        addInterval(displayMinimum, lowerCut);
    }
    return intervals;
}

Point BoxplotCasePoint(const BoxplotLayout &layout, const BoxplotCase &boxplotCase)
{
    if (!IsValidBoxplotLayout(layout) || !std::isfinite(boxplotCase.value)) {
        return {NAN, NAN};
    }
    const DataViewport viewport = BoxplotViewport(layout);
    const Point screen = DataToScreen({0.5, boxplotCase.value}, viewport, layout.plotRect, true);
    double x = BoxplotCategoryCenter(layout, boxplotCase.category);
    if (!(layout.variableAxes && layout.connectRows)) {
        x += BoxplotJitterForCase(boxplotCase.caseId, layout.jitterWidth);
    }
    return {x, screen.y};
}

std::vector<BoxplotCaseGeometry> BoxplotCaseGeometryForLayout(
    const BoxplotLayout &layout,
    const std::vector<BoxplotCase> &cases)
{
    std::vector<BoxplotCaseGeometry> geometry;
    if (!IsValidBoxplotLayout(layout)) {
        return geometry;
    }
    geometry.reserve(cases.size());
    for (const BoxplotCase &boxplotCase : cases) {
        if (boxplotCase.caseId <= 0 || !std::isfinite(boxplotCase.value)) {
            continue;
        }
        Point point = BoxplotCasePoint(layout, boxplotCase);
        if (!std::isfinite(point.x) || !std::isfinite(point.y)) {
            continue;
        }
        geometry.push_back({boxplotCase.caseId, point, boxplotCase.category, boxplotCase.value});
    }
    return geometry;
}

std::vector<BoxplotPointDrawItem> BuildBoxplotPointDrawPlan(
    const std::vector<BoxplotCaseGeometry> &cases,
    const std::set<CaseId> &selection)
{
    std::vector<BoxplotPointDrawItem> plan;
    plan.reserve(cases.size());
    const bool hasSelection = !selection.empty();
    for (const BoxplotCaseGeometry &boxplotCase : cases) {
        if (boxplotCase.caseId <= 0 ||
            !std::isfinite(boxplotCase.point.x) ||
            !std::isfinite(boxplotCase.point.y)) {
            continue;
        }
        const bool selected = selection.find(boxplotCase.caseId) != selection.end();
        plan.push_back({
            boxplotCase.caseId,
            boxplotCase.point,
            selected,
            hasSelection && !selected
        });
    }
    return plan;
}

std::vector<BoxplotConnectionLine> BuildBoxplotConnectionLinePlan(
    const std::vector<BoxplotCaseGeometry> &cases,
    const std::vector<std::string> &categories,
    const std::set<CaseId> &selection,
    bool variableAxes,
    bool connectRows)
{
    std::vector<BoxplotConnectionLine> plan;
    if (!variableAxes || !connectRows || categories.size() <= 1) {
        return plan;
    }

    std::map<std::string, int> categoryOrder;
    for (std::size_t i = 0; i < categories.size(); ++i) {
        categoryOrder[categories[i]] = static_cast<int>(i);
    }

    std::map<CaseId, std::vector<BoxplotCaseGeometry>> casesByRow;
    for (const BoxplotCaseGeometry &boxplotCase : cases) {
        if (boxplotCase.caseId <= 0 ||
            !std::isfinite(boxplotCase.point.x) ||
            !std::isfinite(boxplotCase.point.y)) {
            continue;
        }
        casesByRow[boxplotCase.caseId].push_back(boxplotCase);
    }

    const bool hasSelection = !selection.empty();
    for (auto &entry : casesByRow) {
        std::vector<BoxplotCaseGeometry> rowCases = entry.second;
        if (rowCases.size() < 2) {
            continue;
        }
        std::stable_sort(rowCases.begin(), rowCases.end(), [&](const BoxplotCaseGeometry &a,
                                                               const BoxplotCaseGeometry &b) {
            auto ai = categoryOrder.find(a.category);
            auto bi = categoryOrder.find(b.category);
            const int ar = ai == categoryOrder.end() ? static_cast<int>(categories.size()) : ai->second;
            const int br = bi == categoryOrder.end() ? static_cast<int>(categories.size()) : bi->second;
            if (ar != br) {
                return ar < br;
            }
            return a.category < b.category;
        });

        BoxplotConnectionLine line;
        line.caseId = entry.first;
        line.selected = selection.find(entry.first) != selection.end();
        line.dimmed = hasSelection && !line.selected;
        line.points.reserve(rowCases.size());
        for (const BoxplotCaseGeometry &boxplotCase : rowCases) {
            line.points.push_back(boxplotCase.point);
        }
        plan.push_back(line);
    }
    return plan;
}

std::vector<BoxplotStatsRenderItem> BuildBoxplotStatsRenderPlan(
    const BoxplotLayout &layout,
    const std::vector<BoxplotStats> &stats)
{
    std::vector<BoxplotStatsRenderItem> plan;
    if (!IsValidBoxplotLayout(layout)) {
        return plan;
    }

    const std::size_t slotCount = std::max<std::size_t>(1, layout.categories.size());
    const double boxWidth = std::max(32.0, std::min(74.0, layout.plotRect.width / static_cast<double>(slotCount) * 0.45));
    const double capHalfWidth = boxWidth * 0.28;
    plan.reserve(stats.size());

    for (const BoxplotStats &stat : stats) {
        if (!std::isfinite(stat.q1) ||
            !std::isfinite(stat.median) ||
            !std::isfinite(stat.q3) ||
            !std::isfinite(stat.lower) ||
            !std::isfinite(stat.upper)) {
            continue;
        }
        const double centerX = BoxplotCategoryCenter(layout, stat.category);
        if (!std::isfinite(centerX)) {
            continue;
        }

        const double q1 = DataToScreen({0.5, stat.q1}, BoxplotViewport(layout), layout.plotRect, true).y;
        const double med = DataToScreen({0.5, stat.median}, BoxplotViewport(layout), layout.plotRect, true).y;
        const double q3 = DataToScreen({0.5, stat.q3}, BoxplotViewport(layout), layout.plotRect, true).y;
        const double lo = DataToScreen({0.5, stat.lower}, BoxplotViewport(layout), layout.plotRect, true).y;
        const double hi = DataToScreen({0.5, stat.upper}, BoxplotViewport(layout), layout.plotRect, true).y;

        BoxplotStatsRenderItem item;
        item.category = stat.category;
        item.centerX = centerX;
        item.boxWidth = boxWidth;
        item.boxRect = {
            centerX - boxWidth / 2.0,
            std::min(q1, q3),
            boxWidth,
            std::fabs(q3 - q1)
        };
        item.medianStart = {centerX - boxWidth / 2.0, med};
        item.medianEnd = {centerX + boxWidth / 2.0, med};
        item.upperWhiskerStart = {centerX, hi};
        item.upperWhiskerEnd = {centerX, q3};
        item.lowerWhiskerStart = {centerX, q1};
        item.lowerWhiskerEnd = {centerX, lo};
        item.upperCapStart = {centerX - capHalfWidth, hi};
        item.upperCapEnd = {centerX + capHalfWidth, hi};
        item.lowerCapStart = {centerX - capHalfWidth, lo};
        item.lowerCapEnd = {centerX + capHalfWidth, lo};
        plan.push_back(item);
    }
    return plan;
}

std::set<CaseId> SelectBoxplotCasesInBrush(const std::vector<BoxplotCaseGeometry> &cases,
                                           const Rect &brush,
                                           double pointPadding)
{
    std::set<CaseId> selected;
    if (!IsValidRect(brush)) {
        return selected;
    }
    for (const BoxplotCaseGeometry &boxplotCase : cases) {
        if (boxplotCase.caseId <= 0) {
            continue;
        }
        if (!(pointPadding > 0.0)) {
            if (PointInRect(boxplotCase.point, brush)) {
                selected.insert(boxplotCase.caseId);
            }
            continue;
        }
        Rect hitRect = {
            boxplotCase.point.x - pointPadding,
            boxplotCase.point.y - pointPadding,
            pointPadding * 2.0,
            pointPadding * 2.0
        };
        if (RectIntersects(hitRect, brush)) {
            selected.insert(boxplotCase.caseId);
        }
    }
    return selected;
}

std::optional<CaseId> NearestBoxplotCaseToPoint(const std::vector<BoxplotCaseGeometry> &cases,
                                                const Point &point,
                                                double maxDistance)
{
    if (!std::isfinite(point.x) || !std::isfinite(point.y) || !(maxDistance >= 0.0)) {
        return std::nullopt;
    }
    double best = maxDistance * maxDistance;
    std::optional<CaseId> bestCase;
    for (const BoxplotCaseGeometry &boxplotCase : cases) {
        if (boxplotCase.caseId <= 0) {
            continue;
        }
        double d2 = DistanceSquared(boxplotCase.point, point);
        if (d2 <= best) {
            best = d2;
            bestCase = boxplotCase.caseId;
        }
    }
    return bestCase;
}

std::set<CaseId> SelectBoxplotCasesForGesture(const std::vector<BoxplotCaseGeometry> &cases,
                                              const Rect &brush,
                                              const Point &clickPoint,
                                              bool dragBrush,
                                              double maxClickDistance)
{
    if (dragBrush) {
        return SelectBoxplotCasesInBrush(cases, brush);
    }
    std::set<CaseId> selected;
    std::optional<CaseId> nearest = NearestBoxplotCaseToPoint(cases, clickPoint, maxClickDistance);
    if (nearest.has_value()) {
        selected.insert(*nearest);
    }
    return selected;
}

double BoxplotQuantileSorted(const std::vector<double> &sortedValues, double probability)
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

double MeanOfValues(const std::vector<double> &values)
{
    if (values.empty()) {
        return NAN;
    }
    double total = 0.0;
    for (double value : values) {
        total += value;
    }
    return total / static_cast<double>(values.size());
}

double SampleSDOfValues(const std::vector<double> &values)
{
    if (values.size() < 2) {
        return NAN;
    }
    double mean = MeanOfValues(values);
    double ss = 0.0;
    for (double value : values) {
        double d = value - mean;
        ss += d * d;
    }
    return std::sqrt(ss / static_cast<double>(values.size() - 1));
}

std::map<std::string, std::vector<double>> BoxplotValuesByCategory(const std::vector<BoxplotCase> &cases)
{
    std::map<std::string, std::vector<double>> values;
    for (const BoxplotCase &boxplotCase : cases) {
        if (std::isfinite(boxplotCase.value)) {
            values[boxplotCase.category].push_back(boxplotCase.value);
        }
    }
    return values;
}

std::vector<std::string> BoxplotCategoriesForCases(const std::vector<BoxplotCase> &cases)
{
    std::vector<std::string> categories;
    for (const BoxplotCase &boxplotCase : cases) {
        if (std::find(categories.begin(), categories.end(), boxplotCase.category) == categories.end()) {
            categories.push_back(boxplotCase.category);
        }
    }
    return categories;
}

bool BoxplotVariableListContains(const std::vector<std::string> &variables,
                                 const std::string &name)
{
    return std::find(variables.begin(), variables.end(), name) != variables.end();
}

bool BoxplotUsesVariableAxes(const std::string &kind,
                             const std::string &xLabel)
{
    return kind == "boxplot" && xLabel.empty();
}

std::string BoxplotAddVariableTitle()
{
    return "Add Variable";
}

std::string BoxplotRemoveVariableTitle()
{
    return "Remove Variable";
}

std::string BoxplotNoMoreNumericVariablesTitle()
{
    return "No more numeric variables";
}

BoxplotCreationDialogState BuildBoxplotCreationDialogState()
{
    return BoxplotCreationDialogState{};
}

ParallelCoordinatesDialogState BuildParallelCoordinatesDialogState()
{
    return ParallelCoordinatesDialogState{};
}

std::string BoxplotDefaultTitle(const std::string &yVariable,
                                const std::string &groupVariable)
{
    return groupVariable.empty() ? yVariable : (yVariable + " by " + groupVariable);
}

std::vector<std::string> BoxplotVariablesAvailableToAdd(
    const std::vector<std::string> &currentVariables,
    const std::vector<std::string> &availableVariables)
{
    std::vector<std::string> out;
    for (const std::string &name : availableVariables) {
        if (!BoxplotVariableListContains(currentVariables, name) &&
            !BoxplotVariableListContains(out, name)) {
            out.push_back(name);
        }
    }
    return out;
}

BoxplotVariableUpdateResult BoxplotVariablesAfterAdd(
    const std::vector<std::string> &currentVariables,
    const std::string &name,
    const std::vector<std::string> &availableVariables)
{
    BoxplotVariableUpdateResult result;
    result.variables = currentVariables;
    if (!BoxplotVariableListContains(availableVariables, name)) {
        result.error = "numeric variable not found: " + name;
        return result;
    }
    result.ok = true;
    if (!BoxplotVariableListContains(result.variables, name)) {
        result.variables.push_back(name);
        result.changed = true;
    }
    return result;
}

BoxplotVariableUpdateResult BoxplotVariablesAfterRemove(
    const std::vector<std::string> &currentVariables,
    const std::string &name,
    std::size_t minimumVariables)
{
    BoxplotVariableUpdateResult result;
    result.variables = currentVariables;
    if (currentVariables.size() <= minimumVariables) {
        result.error = "a boxplot must keep at least one variable";
        return result;
    }

    auto it = std::find(result.variables.begin(), result.variables.end(), name);
    if (it == result.variables.end()) {
        result.error = "variable is not in this boxplot: " + name;
        return result;
    }
    result.variables.erase(it);
    result.ok = true;
    result.changed = true;
    return result;
}

BoxplotVariableUpdateResult BoxplotVariablesAfterReplacement(
    const std::vector<std::string> &currentVariables,
    const std::string &oldName,
    const std::string &newName,
    const std::vector<std::string> &availableVariables)
{
    BoxplotVariableUpdateResult result;
    result.variables = currentVariables;
    auto oldIt = std::find(result.variables.begin(), result.variables.end(), oldName);
    if (oldIt == result.variables.end()) {
        result.error = "variable is not in this boxplot: " + oldName;
        return result;
    }
    if (!BoxplotVariableListContains(availableVariables, newName)) {
        result.error = "numeric variable not found: " + newName;
        return result;
    }
    if (oldName == newName) {
        result.ok = true;
        return result;
    }
    if (BoxplotVariableListContains(result.variables, newName)) {
        result.error = "variable is already in this boxplot: " + newName;
        return result;
    }
    *oldIt = newName;
    result.ok = true;
    result.changed = true;
    return result;
}

double BoxplotConnectionStrokeWidth(double requestedWidth, bool selected)
{
    (void)selected;
    const double base = std::max(0.5, std::min(8.0,
        std::isfinite(requestedWidth) ? requestedWidth : 1.0));
    return base;
}

ParallelBoxplotLabelState ParallelBoxplotLabelsForVariables(
    const std::vector<std::string> &variables,
    bool standardize,
    const std::string &currentTitle)
{
    ParallelBoxplotLabelState state;
    state.title = currentTitle;
    if (variables.empty()) {
        return state;
    }
    if (standardize) {
        state.yLabel = "Standardized value";
        if (state.title.empty()) {
            state.title = variables.size() == 1 ? variables.front() : "Parallel boxplots";
        }
        return state;
    }
    if (variables.size() == 1) {
        state.yLabel = variables.front();
        if (state.title.empty() || state.title == "Parallel boxplots") {
            state.title = state.yLabel;
        }
        return state;
    }
    state.yLabel = "Value";
    if (state.title.empty() || BoxplotVariableListContains(variables, state.title)) {
        state.title = "Parallel boxplots";
    }
    return state;
}

BoxplotMenuState BuildBoxplotMenuState(
    bool showPoints,
    bool showBox,
    bool showWhiskers,
    bool showViolin,
    bool splitViolin,
    bool showH0Simulation,
    bool variableAxes,
    bool connectRows,
    bool standardizeVariables,
    const std::vector<std::string> &currentVariables,
    const std::vector<std::string> &availableVariables)
{
    BoxplotMenuState state;
    state.togglePoints = {
        showPoints ? "Hide Points" : "Show Points",
        showPoints ? "hide_points" : "show_points",
        "BOXPLOT_TOGGLE_POINTS"
    };
    state.toggleBox = {
        "Toggle Box",
        showBox ? "shown" : "hidden",
        "BOXPLOT_TOGGLE_BOX"
    };
    state.toggleWhiskers = {
        "Toggle Whiskers",
        showWhiskers ? "shown" : "hidden",
        "BOXPLOT_TOGGLE_WHISKERS"
    };
    state.toggleViolin = {
        "Show Violin",
        "violin",
        "BOXPLOT_TOGGLE_VIOLIN",
        true,
        showViolin
    };
    state.configureSplitViolin = {
        "Split Violin...",
        "split_violin",
        "BOXPLOT_CONFIGURE_SPLIT_VIOLIN"
    };
    state.showClearSplitViolin = splitViolin;
    state.clearSplitViolin = {
        "Clear Split Violin",
        "clear_split_violin",
        "BOXPLOT_CLEAR_SPLIT_VIOLIN"
    };
    state.configureH0 = {
        "H0 Simulation...",
        "h0_simulation",
        "BOXPLOT_CONFIGURE_H0"
    };
    state.showClearH0 = showH0Simulation;
    state.clearH0 = {
        "Clear H0 Simulation",
        "clear_h0_simulation",
        "BOXPLOT_CLEAR_H0"
    };

    state.showVariableAxisOptions = variableAxes;
    state.addVariableTitle = BoxplotAddVariableTitle();
    state.noMoreNumericVariablesTitle = BoxplotNoMoreNumericVariablesTitle();
    state.removeVariableTitle = BoxplotRemoveVariableTitle();
    if (variableAxes) {
        for (const std::string &name : BoxplotVariablesAvailableToAdd(currentVariables, availableVariables)) {
            state.addVariableOptions.push_back({
                name,
                name,
                "BOXPLOT_ADD_VARIABLE|" + name
            });
        }
        state.showRemoveVariableMenu = currentVariables.size() > 1;
        if (state.showRemoveVariableMenu) {
            for (const std::string &name : currentVariables) {
                state.removeVariableOptions.push_back({
                    name,
                    name,
                    "BOXPLOT_REMOVE_VARIABLE|" + name
                });
            }
        }
        state.connectRows = {
            "Connect Matching Rows",
            "connect_rows",
            "BOXPLOT_TOGGLE_CONNECT_ROWS",
            true,
            connectRows
        };
        state.standardizeVariables = {
            "Standardize Variables",
            "standardize",
            "BOXPLOT_TOGGLE_STANDARDIZE",
            true,
            standardizeVariables
        };
    }

    state.descriptives = {
        "Descriptive statistics",
        "descriptives",
        "CONTEXT_BOXPLOT_DESCRIPTIVES"
    };
    for (const LinkedPlotCommandOption &selectionOption : SelectionCommandOptions(true)) {
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

ParallelBoxplotBuildResult BuildParallelBoxplotCases(const ParallelBoxplotBuildInput &input)
{
    ParallelBoxplotBuildResult result;
    if (input.selectedVariables.empty()) {
        result.error = "choose at least one boxplot variable";
        return result;
    }

    for (const std::string &name : input.selectedVariables) {
        auto varIt = std::find_if(input.variables.begin(), input.variables.end(),
                                  [&](const BoxplotVariableSeries &series) {
                                      return series.name == name;
                                  });
        if (varIt == input.variables.end()) {
            result.error = "numeric variable not found: " + name;
            return result;
        }

        std::vector<double> finiteValues;
        finiteValues.reserve(varIt->values.size());
        for (double value : varIt->values) {
            if (std::isfinite(value)) {
                finiteValues.push_back(value);
            }
        }

        double mean = 0.0;
        double sd = 0.0;
        if (input.standardize && !finiteValues.empty()) {
            mean = MeanOfValues(finiteValues);
            sd = SampleSDOfValues(finiteValues);
            if (!std::isfinite(sd)) {
                sd = 0.0;
            }
        }

        for (std::size_t i = 0; i < varIt->values.size(); ++i) {
            double value = varIt->values[i];
            if (!std::isfinite(value)) {
                continue;
            }
            if (input.standardize) {
                value = sd > 0.0 ? (value - mean) / sd : 0.0;
            }
            result.cases.push_back({static_cast<CaseId>(i + 1), value, name});
        }
        result.categories.push_back(name);
    }

    result.ok = true;
    return result;
}

std::vector<BoxplotStats> BoxplotStatsForCases(const std::vector<BoxplotCase> &cases,
                                               const std::vector<std::string> &categories)
{
    std::vector<BoxplotStats> stats;
    for (const std::string &category : categories) {
        std::vector<double> values;
        for (const BoxplotCase &boxplotCase : cases) {
            if (boxplotCase.category == category && std::isfinite(boxplotCase.value)) {
                values.push_back(boxplotCase.value);
            }
        }
        if (values.empty()) {
            continue;
        }
        std::sort(values.begin(), values.end());
        BoxplotStats s;
        s.category = category;
        s.n = static_cast<int>(values.size());
        s.q1 = BoxplotQuantileSorted(values, 0.25);
        s.median = BoxplotQuantileSorted(values, 0.50);
        s.q3 = BoxplotQuantileSorted(values, 0.75);
        double iqr = s.q3 - s.q1;
        double lowerLimit = s.q1 - 1.5 * iqr;
        double upperLimit = s.q3 + 1.5 * iqr;
        s.lower = values.front();
        for (double value : values) {
            if (value >= lowerLimit) {
                s.lower = value;
                break;
            }
        }
        s.upper = values.back();
        for (auto it = values.rbegin(); it != values.rend(); ++it) {
            if (*it <= upperLimit) {
                s.upper = *it;
                break;
            }
        }
        stats.push_back(s);
    }
    return stats;
}

bool BuildBoxplotH0Simulation(const std::vector<BoxplotCase> &cases,
                              const std::vector<std::string> &categories,
                              bool variableAxes,
                              double h0,
                              const std::string &alternative,
                              int requestedDraws,
                              BoxplotH0SimulationResult &result,
                              std::string *error)
{
    result = BoxplotH0SimulationResult();
    if (alternative != "less" && alternative != "greater" && alternative != "two.sided") {
        if (error) {
            *error = "invalid H0 simulation alternative";
        }
        return false;
    }
    int draws = std::max(100, std::min(200000, requestedDraws));
    result.draws = draws;

    std::map<std::string, std::vector<double>> byCategory = BoxplotValuesByCategory(cases);
    std::vector<std::string> observedCategories;
    for (const std::string &category : categories) {
        auto it = byCategory.find(category);
        if (it != byCategory.end() && !it->second.empty()) {
            observedCategories.push_back(category);
        }
    }
    if (observedCategories.empty()) {
        if (error) {
            *error = "no finite boxplot values are available";
        }
        return false;
    }

    std::mt19937 rng(5489u);
    std::vector<double> simulated;
    std::vector<double> referenceLines;
    double observedContrast = NAN;
    double lower = NAN;
    double upper = NAN;
    double p = NAN;
    double effect = NAN;
    std::string label;

    if (observedCategories.size() == 1) {
        const std::vector<double> &values = byCategory[observedCategories.front()];
        if (values.size() < 2) {
            if (error) {
                *error = "H0 simulation needs at least two finite values.";
            }
            return false;
        }
        double observedMean = MeanOfValues(values);
        double sd = SampleSDOfValues(values);
        if (!std::isfinite(sd)) {
            sd = 0.0;
        }
        double se = sd > 0.0 ? sd / std::sqrt(static_cast<double>(values.size())) : 0.0;
        std::normal_distribution<double> normal(h0, se);
        simulated.reserve(static_cast<std::size_t>(draws));
        int extreme = 0;
        observedContrast = observedMean - h0;
        for (int i = 0; i < draws; ++i) {
            double sampleMean = se > 0.0 ? normal(rng) : h0;
            simulated.push_back(sampleMean);
            double simContrast = sampleMean - h0;
            if (alternative == "two.sided") {
                if (std::fabs(simContrast) >= std::fabs(observedContrast)) {
                    ++extreme;
                }
            } else if (alternative == "greater") {
                if (simContrast >= observedContrast) {
                    ++extreme;
                }
            } else {
                if (simContrast <= observedContrast) {
                    ++extreme;
                }
            }
        }
        p = (static_cast<double>(extreme) + 1.0) / (static_cast<double>(draws) + 1.0);
        effect = sd > 0.0 ? observedContrast / sd : NAN;
        referenceLines.push_back(observedMean);
        referenceLines.push_back(h0);
        if (alternative == "two.sided") {
            double distance = std::fabs(observedContrast);
            lower = h0 - distance;
            upper = h0 + distance;
        } else {
            lower = observedMean;
            upper = observedMean;
        }
        label = "H0 mean";
    } else if (observedCategories.size() == 2 && !variableAxes) {
        const std::vector<double> &values1 = byCategory[observedCategories[0]];
        const std::vector<double> &values2 = byCategory[observedCategories[1]];
        if (values1.size() < 2 || values2.size() < 2) {
            if (error) {
                *error = "two-group H0 simulation needs at least two finite values per group.";
            }
            return false;
        }
        double mean1 = MeanOfValues(values1);
        double mean2 = MeanOfValues(values2);
        double sd1 = SampleSDOfValues(values1);
        double sd2 = SampleSDOfValues(values2);
        if (!std::isfinite(sd1)) {
            sd1 = 0.0;
        }
        if (!std::isfinite(sd2)) {
            sd2 = 0.0;
        }
        double center = (mean1 + mean2) / 2.0;
        double observedDiff = mean1 - mean2;
        observedContrast = observedDiff - h0;
        double se1 = sd1 > 0.0 ? sd1 / std::sqrt(static_cast<double>(values1.size())) : 0.0;
        double se2 = sd2 > 0.0 ? sd2 / std::sqrt(static_cast<double>(values2.size())) : 0.0;
        std::normal_distribution<double> normal1(center + h0 / 2.0, se1);
        std::normal_distribution<double> normal2(center - h0 / 2.0, se2);
        simulated.reserve(static_cast<std::size_t>(draws));
        int extreme = 0;
        for (int i = 0; i < draws; ++i) {
            double simMean1 = se1 > 0.0 ? normal1(rng) : center + h0 / 2.0;
            double simMean2 = se2 > 0.0 ? normal2(rng) : center - h0 / 2.0;
            double simDiff = simMean1 - simMean2;
            double simContrast = simDiff - h0;
            simulated.push_back(center + simContrast / 2.0);
            if (alternative == "two.sided") {
                if (std::fabs(simContrast) >= std::fabs(observedContrast)) {
                    ++extreme;
                }
            } else if (alternative == "greater") {
                if (simContrast >= observedContrast) {
                    ++extreme;
                }
            } else {
                if (simContrast <= observedContrast) {
                    ++extreme;
                }
            }
        }
        p = (static_cast<double>(extreme) + 1.0) / (static_cast<double>(draws) + 1.0);
        double pooledDenom = static_cast<double>(values1.size()) + static_cast<double>(values2.size()) - 2.0;
        double pooled = pooledDenom > 0.0
            ? std::sqrt(((values1.size() - 1) * sd1 * sd1 + (values2.size() - 1) * sd2 * sd2) / pooledDenom)
            : NAN;
        effect = (std::isfinite(pooled) && pooled > 0.0) ? observedContrast / pooled : NAN;
        referenceLines.push_back(mean1);
        referenceLines.push_back(mean2);
        if (alternative == "two.sided") {
            double distance = std::fabs(observedContrast) / 2.0;
            lower = center - distance;
            upper = center + distance;
        } else {
            lower = center + observedContrast / 2.0;
            upper = lower;
        }
        label = "H0 diff";
    } else {
        if (error) {
            *error = "H0 simulation currently supports one variable or exactly two observed groups.";
        }
        return false;
    }

    if (simulated.size() < 2) {
        if (error) {
            *error = "H0 simulation did not produce enough values.";
        }
        return false;
    }
    result.draws = draws;
    result.simulatedValues = simulated;
    result.referenceLines = referenceLines;
    result.lower = lower;
    result.upper = upper;
    result.pValue = p;
    result.effect = effect;
    result.label = label;
    return true;
}

std::string BoxplotSplitViolinInfoText()
{
    return "Highlight the tail or tails of the violin using thresholds on the displayed scale.";
}

std::string BoxplotH0SimulationInfoText()
{
    return "Simulate the null distribution and show its p-value/effect layer in the boxplot.";
}

std::string BoxplotXVariableFieldLabel()
{
    return "X variable:";
}

std::string BoxplotAddXFieldLabel()
{
    return "Add X:";
}

std::string BoxplotSplitByFieldLabel()
{
    return "Split by:";
}

std::string BoxplotWindowTitle()
{
    return "Boxplot";
}

std::string BoxplotSegmentDetailsWindowTitle()
{
    return "Segment details";
}

std::string BoxplotH0SimulationWindowTitle()
{
    return "H0 Simulation";
}

std::string BoxplotObservedLabel()
{
    return "Observed";
}

std::string BoxplotParallelLabel()
{
    return "Parallel";
}

std::string BoxplotAlternativeFieldLabel()
{
    return "Alternative:";
}

std::string BoxplotLessOptionLabel()
{
    return "less";
}

std::string BoxplotGreaterOptionLabel()
{
    return "greater";
}

std::string BoxplotTwoSidedOptionLabel()
{
    return "two.sided";
}

std::string BoxplotThresholdLowerFieldLabel()
{
    return "Threshold / lower:";
}

std::string BoxplotUpperFieldLabel()
{
    return "Upper:";
}

std::string BoxplotH0FieldLabel()
{
    return "H0:";
}

std::string BoxplotDrawsFieldLabel()
{
    return "Draws:";
}

std::string BoxplotSplitViolinDialogTitle()
{
    return "Split Violin";
}

std::string BoxplotPBracketLabel()
{
    return "p";
}

std::string BoxplotLeafLabel()
{
    return "leaf";
}

bool BoxplotUsesVariableAxes(const PlotModel &model)
{
    return BoxplotUsesVariableAxes(model.kind, model.xLabel);
}

bool BoxplotHasVariable(const PlotModel &model, const std::string &name)
{
    return BoxplotVariableListContains(model.boxplotVariables, name);
}

void UpdateParallelBoxplotLabels(PlotModel &model)
{
    if (!BoxplotUsesVariableAxes(model)) return;
    if (model.boxplotVariables.empty()) return;
    ParallelBoxplotLabelState labels =
        ParallelBoxplotLabelsForVariables(model.boxplotVariables,
                                           model.boxplotStandardizeVariables,
                                           model.title);
    model.yLabel = labels.yLabel;
    model.title = labels.title;
}

bool RebuildParallelBoxplotPoints(PlotModel &model, std::string *error)
{
    if (!BoxplotUsesVariableAxes(model)) {
        if (error) *error = "plot is not an ungrouped boxplot";
        return false;
    }
    if (model.boxplotVariables.empty() && !model.yLabel.empty()) {
        model.boxplotVariables.push_back(model.yLabel);
    }
    ParallelBoxplotBuildInput input;
    input.selectedVariables = model.boxplotVariables;
    input.standardize = model.boxplotStandardizeVariables;
    input.variables.reserve(model.variables.size());
    for (const NumericVariable &var : model.variables) {
        input.variables.push_back(BoxplotVariableSeries{var.name, var.values});
    }
    ParallelBoxplotBuildResult result = BuildParallelBoxplotCases(input);
    if (!result.ok) {
        if (error) *error = result.error;
        return false;
    }
    std::vector<BoxplotPoint> points;
    points.reserve(result.cases.size());
    for (const BoxplotCase &boxplotCase : result.cases) {
        points.push_back(BoxplotPoint{boxplotCase.value, boxplotCase.category, boxplotCase.caseId});
    }
    model.boxplotPoints = points;
    model.boxplotCategories = result.categories;
    model.boxplotDefinedCategories = result.categories;
    model.boxplotGroupOrder = "defined";
    UpdateParallelBoxplotLabels(model);
    return true;
}

void RebuildGroupedBoxplotPointsFromDataFrame(PlotModel &model,
                                              const DataFrameModel &df)
{
    const DataColumn *yCol = nullptr;
    const DataColumn *xCol = nullptr;
    for (const DataColumn &col : df.columns) {
        if (col.name == model.yLabel) {
            yCol = &col;
        }
        if (!model.xLabel.empty() && col.name == model.xLabel) {
            xCol = &col;
        }
    }
    model.boxplotPoints.clear();
    if (!yCol) {
        model.boxplotCategories.clear();
        model.boxplotDefinedCategories.clear();
        return;
    }
    std::size_t n = yCol->values.size();
    if (xCol) {
        n = std::min(n, xCol->values.size());
    }
    for (std::size_t i = 0; i < n; ++i) {
        double y = NAN;
        if (!ParseDataCellDouble(yCol->values[i], y) || !std::isfinite(y)) {
            continue;
        }
        std::string category = xCol ? DisplayValueForCell(*xCol, i) : "All";
        if (DataCellIsMissing(category)) {
            category = "NA";
        }
        model.boxplotPoints.push_back(BoxplotPoint{y, category, static_cast<int>(i) + 1});
    }
    model.boxplotCategories = CategoriesForPlot(model);
    model.boxplotDefinedCategories = model.boxplotCategories;
    model.boxplotGroupOrder = "defined";
}

std::vector<BoxplotCase> BoxplotCasesForModel(const PlotModel &model)
{
    std::vector<BoxplotCase> cases;
    cases.reserve(model.boxplotPoints.size());
    for (const BoxplotPoint &point : model.boxplotPoints) {
        cases.push_back({point.row, point.y, point.category});
    }
    return cases;
}

std::vector<std::string> CategoriesForPlot(const PlotModel &model)
{
    return BoxplotCategoriesForCases(BoxplotCasesForModel(model));
}

void ClearBoxplotH0Simulation(PlotModel &model)
{
    model.boxplotShowH0Simulation = false;
    model.boxplotH0Values.clear();
    model.boxplotH0ReferenceLines.clear();
    model.boxplotH0Lower = NAN;
    model.boxplotH0Upper = NAN;
    model.boxplotH0PValue = NAN;
    model.boxplotH0Effect = NAN;
    model.boxplotH0Status.clear();
}

std::vector<BoxplotStats> BoxplotStatsForModel(const PlotModel &model)
{
    return BoxplotStatsForCases(BoxplotCasesForModel(model), model.boxplotCategories);
}

std::string BoxplotOptionsResponseText(const PlotModel &model)
{
    std::ostringstream out;
    out << "OK\t" << (model.boxplotShowPoints ? "TRUE" : "FALSE")
        << "|" << (model.boxplotShowBox ? "TRUE" : "FALSE")
        << "|" << (model.boxplotShowWhiskers ? "TRUE" : "FALSE")
        << "|" << (model.boxplotConnectRows ? "TRUE" : "FALSE")
        << "|" << (model.boxplotStandardizeVariables ? "TRUE" : "FALSE")
        << "|" << (model.boxplotShowViolin ? "TRUE" : "FALSE")
        << "|" << (model.boxplotSplitViolin ? "TRUE" : "FALSE")
        << "|" << model.boxplotSplitAlternative
        << "|" << FormatDoubleOrDash(model.boxplotSplitLower, 6)
        << "|" << FormatDoubleOrDash(model.boxplotSplitUpper, 6)
        << "|" << (model.boxplotShowH0Simulation ? "TRUE" : "FALSE")
        << "|" << model.boxplotH0Alternative
        << "|" << FormatDoubleOrDash(model.boxplotH0, 6)
        << "|" << model.boxplotH0Draws
        << "|" << FormatPValue(model.boxplotH0PValue)
        << "|" << FormatDoubleOrDash(model.boxplotH0Effect, 6);
    return out.str();
}

void EnsureParallelBoxplotState(PlotModel &model)
{
    if (!BoxplotUsesVariableAxes(model)) return;
    if (model.boxplotVariables.empty() && !model.yLabel.empty()) {
        model.boxplotVariables.push_back(model.yLabel);
    }
    if (!model.variables.empty()) {
        std::string ignored;
        if (RebuildParallelBoxplotPoints(model, &ignored)) return;
    }
    for (BoxplotPoint &point : model.boxplotPoints) {
        if (point.category == "All" && !model.boxplotVariables.empty()) {
            point.category = model.boxplotVariables.front();
        }
    }
    model.boxplotCategories = model.boxplotVariables.empty() ? CategoriesForPlot(model) : model.boxplotVariables;
    model.boxplotDefinedCategories = model.boxplotCategories;
    model.boxplotGroupOrder = "defined";
    UpdateParallelBoxplotLabels(model);
}

bool AddParallelBoxplotVariable(PlotModel &model,
                                const std::string &name,
                                std::string *error)
{
    if (!BoxplotUsesVariableAxes(model)) {
        if (error) *error = "variables can only be added to an ungrouped boxplot";
        return false;
    }
    std::vector<std::string> available;
    for (const NumericVariable &var : model.variables) {
        available.push_back(var.name);
    }
    BoxplotVariableUpdateResult update =
        BoxplotVariablesAfterAdd(model.boxplotVariables, name, available);
    if (!update.ok) {
        if (error) *error = update.error;
        return false;
    }
    model.boxplotVariables = update.variables;
    return RebuildParallelBoxplotPoints(model, error);
}

bool RemoveParallelBoxplotVariable(PlotModel &model,
                                   const std::string &name,
                                   std::string *error)
{
    if (!BoxplotUsesVariableAxes(model)) {
        if (error) *error = "variables can only be removed from an ungrouped boxplot";
        return false;
    }
    BoxplotVariableUpdateResult update =
        BoxplotVariablesAfterRemove(model.boxplotVariables, name);
    if (!update.ok) {
        if (error) *error = update.error;
        return false;
    }
    model.boxplotVariables = update.variables;
    return RebuildParallelBoxplotPoints(model, error);
}

bool ReplaceParallelBoxplotVariable(PlotModel &model,
                                    const std::string &oldName,
                                    const std::string &newName,
                                    std::string *error)
{
    if (!BoxplotUsesVariableAxes(model)) {
        if (error) *error = "variables can only be replaced in an ungrouped boxplot";
        return false;
    }
    std::vector<std::string> available;
    for (const NumericVariable &var : model.variables) available.push_back(var.name);
    BoxplotVariableUpdateResult update =
        BoxplotVariablesAfterReplacement(model.boxplotVariables, oldName, newName, available);
    if (!update.ok) {
        if (error) *error = update.error;
        return false;
    }
    if (!update.changed) return true;
    model.boxplotVariables = update.variables;
    return RebuildParallelBoxplotPoints(model, error);
}

bool RebuildBoxplotH0Simulation(PlotModel &model, std::string *error)
{
    if (model.kind != "boxplot") {
        if (error) *error = "plot is not a boxplot";
        return false;
    }
    if (BoxplotUsesVariableAxes(model) && model.boxplotVariables.size() > 1) {
        if (error) *error = "H0 simulation currently works for one variable or a two-group boxplot.";
        return false;
    }
    BoxplotH0SimulationResult result;
    if (!BuildBoxplotH0Simulation(BoxplotCasesForModel(model),
                                  model.boxplotCategories,
                                  BoxplotUsesVariableAxes(model),
                                  model.boxplotH0,
                                  model.boxplotH0Alternative,
                                  model.boxplotH0Draws,
                                  result,
                                  error)) {
        return false;
    }
    model.boxplotH0Draws = result.draws;
    model.boxplotH0Values = result.simulatedValues;
    model.boxplotH0ReferenceLines = result.referenceLines;
    model.boxplotH0Lower = result.lower;
    model.boxplotH0Upper = result.upper;
    model.boxplotH0PValue = result.pValue;
    model.boxplotH0Effect = result.effect;
    model.boxplotH0Label = result.label;
    std::ostringstream status;
    status << result.label << ": p = " << FormatPValue(result.pValue)
           << ", draws = " << result.draws;
    model.boxplotH0Status = status.str();
    model.boxplotShowH0Simulation = true;
    return true;
}

} // namespace core
} // namespace rlispstat
