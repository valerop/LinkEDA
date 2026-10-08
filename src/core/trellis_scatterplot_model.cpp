#include "trellis_scatterplot_model.h"

#include "boxplot_model.h"
#include "format_model.h"
#include "scatter_matrix_model.h"
#include "scatterplot_model.h"

#include <algorithm>
#include <cmath>
#include <limits>
#include <map>
#include <numeric>
#include <sstream>
#include <set>

namespace rlispstat {
namespace core {

namespace {

constexpr std::size_t kMaximumTrellisPanels = 20;

bool NumericLabel(const std::string &label, double &value)
{
    return ParseDataCellDouble(label, value) && std::isfinite(value);
}

std::vector<std::string> OrderedObservedLevels(const DataColumn &column,
                                               const std::vector<std::size_t> &rows,
                                               const std::vector<std::string> &definedOrder)
{
    std::set<std::string> observed;
    for (std::size_t row : rows) observed.insert(DisplayValueForCell(column, row));

    std::vector<std::string> levels;
    for (const std::string &level : definedOrder) {
        if (observed.erase(level) > 0) levels.push_back(level);
    }
    std::vector<std::string> remaining(observed.begin(), observed.end());
    const bool numeric = NormalizeVariableType(column.type) == "numeric";
    if (numeric) {
        std::stable_sort(remaining.begin(), remaining.end(), [](const std::string &left,
                                                                const std::string &right) {
            double leftValue = NAN;
            double rightValue = NAN;
            const bool leftNumeric = NumericLabel(left, leftValue);
            const bool rightNumeric = NumericLabel(right, rightValue);
            if (leftNumeric != rightNumeric) return leftNumeric;
            if (leftNumeric && leftValue != rightValue) return leftValue < rightValue;
            return left < right;
        });
    }
    // std::set already provides deterministic lexical ordering for text values.
    levels.insert(levels.end(), remaining.begin(), remaining.end());
    return levels;
}

DataViewport CommonViewport(const PlotModel &plot)
{
    TrellisPlotType type = plot.trellisSpecificationInitialized
        ? plot.trellisSpecification.plotType : TrellisPlotType::Scatter;
    double xmin = std::numeric_limits<double>::infinity();
    double xmax = -std::numeric_limits<double>::infinity();
    double ymin = std::numeric_limits<double>::infinity();
    double ymax = -std::numeric_limits<double>::infinity();
    for (const DataPoint &point : plot.points) {
        if (!std::isfinite(point.x) || !std::isfinite(point.y)) continue;
        xmin = std::min(xmin, point.x);
        xmax = std::max(xmax, point.x);
        ymin = std::min(ymin, point.y);
        ymax = std::max(ymax, point.y);
    }
    if (type == TrellisPlotType::Boxplot || type == TrellisPlotType::Bar) {
        xmin = -0.5;
        xmax = std::max(0.5, static_cast<double>(plot.boxplotCategories.size()) - 0.5);
    }
    if (type == TrellisPlotType::Bar) {
        std::map<std::string, std::map<std::string, int>> counts;
        for (std::size_t index = 0; index < plot.points.size(); ++index) {
            if (index >= plot.trellisPointPanels.size() || index >= plot.trellisPointCategories.size()) continue;
            ++counts[plot.trellisPointPanels[index]][plot.trellisPointCategories[index]];
        }
        double maximum = 0.0;
        for (const auto &panel : counts) {
            int total = 0;
            for (const auto &category : panel.second) total += category.second;
            for (const auto &category : panel.second) {
                double value = static_cast<double>(category.second);
                if (plot.trellisSpecification.barMeasure == "percent" && total > 0) {
                    value = 100.0 * value / static_cast<double>(total);
                } else if (plot.trellisSpecification.barMeasure == "conditional_percent" &&
                           category.second > 0) {
                    value = 100.0;
                }
                maximum = std::max(maximum, value);
            }
        }
        ymin = 0.0;
        ymax = (plot.trellisSpecification.barMeasure == "percent" ||
                plot.trellisSpecification.barMeasure == "conditional_percent")
            ? 100.0 : std::max(1.0, maximum * 1.08);
    }
    if (type == TrellisPlotType::Histogram) {
        const std::size_t binCount = std::max<std::size_t>(1, plot.trellisSpecification.histogramBinCount);
        if (std::isfinite(xmin) && std::isfinite(xmax) && xmax > xmin) {
            std::map<std::string, std::vector<int>> counts;
            for (std::size_t index = 0; index < plot.points.size(); ++index) {
                if (index >= plot.trellisPointPanels.size()) continue;
                std::size_t bin = static_cast<std::size_t>(std::floor(
                    (plot.points[index].x - xmin) / (xmax - xmin) * static_cast<double>(binCount)));
                bin = std::min(bin, binCount - 1);
                std::vector<int> &panel = counts[plot.trellisPointPanels[index]];
                if (panel.empty()) panel.assign(binCount, 0);
                ++panel[bin];
            }
            double maximum = 0.0;
            for (const auto &panel : counts) {
                const int total = std::accumulate(panel.second.begin(), panel.second.end(), 0);
                for (int count : panel.second) {
                    double value = static_cast<double>(count);
                    if (plot.trellisSpecification.histogramMeasure == "percent" && total > 0) {
                        value = 100.0 * value / static_cast<double>(total);
                    } else if (plot.trellisSpecification.histogramMeasure == "density" && total > 0) {
                        const double width = (xmax - xmin) / static_cast<double>(binCount);
                        value /= static_cast<double>(total) * width;
                    }
                    maximum = std::max(maximum, value);
                }
            }
            ymin = 0.0;
            ymax = std::max(1.0e-9, maximum * 1.08);
        }
    }
    if (!std::isfinite(xmin) || !std::isfinite(xmax)) { xmin = 0.0; xmax = 1.0; }
    if (!std::isfinite(ymin) || !std::isfinite(ymax)) { ymin = 0.0; ymax = 1.0; }
    if (xmin == xmax) { const double span = std::max(1.0, std::fabs(xmin) * 0.1); xmin -= span / 2.0; xmax += span / 2.0; }
    if (ymin == ymax) { const double span = std::max(1.0, std::fabs(ymin) * 0.1); ymin -= span / 2.0; ymax += span / 2.0; }
    const double xPadding = (xmax - xmin) * 0.05;
    const double yPadding = (ymax - ymin) * 0.05;
    const bool baselineAtZero = type == TrellisPlotType::Bar || type == TrellisPlotType::Histogram;
    return {xmin - xPadding, xmax + xPadding,
            baselineAtZero ? 0.0 : ymin - yPadding,
            ymax + (baselineAtZero ? 0.0 : yPadding)};
}

std::size_t PreferredAutomaticColumns(std::size_t panelCount)
{
    if (panelCount <= 1) return 1;
    if (panelCount <= 3) return panelCount;
    if (panelCount == 4) return 2;
    if (panelCount <= 6) return 3;
    if (panelCount <= 8) return 4;
    if (panelCount == 9) return 3;
    if (panelCount <= 12) return 4;
    return static_cast<std::size_t>(std::ceil(std::sqrt(static_cast<double>(panelCount))));
}

std::size_t ManualColumns(const std::string &mode, std::size_t panelCount)
{
    if (mode == "one_row") return panelCount;
    if (mode == "one_column") return 1;
    if (mode == "grid") return static_cast<std::size_t>(
        std::ceil(std::sqrt(static_cast<double>(std::max<std::size_t>(1, panelCount)))));
    if (mode == "two_columns") return std::min<std::size_t>(2, panelCount);
    if (mode == "three_columns") return std::min<std::size_t>(3, panelCount);
    return 0;
}

std::size_t AutomaticColumns(std::size_t panelCount,
                             double availableWidth,
                             double availableHeight)
{
    if (panelCount == 3 && availableHeight > 0.0) {
        const double windowAspect = availableWidth / availableHeight;
        const double threePanelWidth = (availableWidth - 32.0) / 3.0;
        if (windowAspect >= 1.15 && threePanelWidth >= 120.0) return 3;
        if (windowAspect <= 0.75) return 1;
    }
    const std::size_t preferred = PreferredAutomaticColumns(panelCount);
    constexpr double gapX = 16.0;
    constexpr double gapY = 16.0;
    constexpr double headerHeight = 22.0;
    constexpr double targetAspect = 1.25;
    constexpr double minimumWidth = 170.0;
    constexpr double minimumHeight = 120.0;
    double bestScore = std::numeric_limits<double>::infinity();
    std::size_t bestColumns = preferred;
    for (std::size_t columns = 1; columns <= panelCount; ++columns) {
        const std::size_t rows = (panelCount + columns - 1) / columns;
        const double frameWidth = (availableWidth - gapX * static_cast<double>(columns - 1)) /
            static_cast<double>(columns);
        const double frameHeight = (availableHeight - gapY * static_cast<double>(rows - 1)) /
            static_cast<double>(rows);
        const double plotHeight = std::max(1.0, frameHeight - headerHeight);
        const double aspect = std::max(0.05, frameWidth / plotHeight);
        const std::size_t emptyCells = rows * columns - panelCount;
        double score = 2.2 * static_cast<double>(emptyCells) +
            0.55 * std::fabs(static_cast<double>(columns) - static_cast<double>(preferred)) +
            2.8 * std::fabs(std::log(aspect / targetAspect));
        if (frameWidth < minimumWidth) score += (minimumWidth - frameWidth) * 0.10;
        if (plotHeight < minimumHeight) score += (minimumHeight - plotHeight) * 0.12;
        if (score < bestScore - 1.0e-9 ||
            (std::fabs(score - bestScore) <= 1.0e-9 && columns == preferred)) {
            bestScore = score;
            bestColumns = columns;
        }
    }
    return bestColumns;
}

Rect BoundingRect(const std::vector<TrellisPanelLayout> &panels)
{
    if (panels.empty()) return {};
    double left = panels.front().frame.x;
    double top = panels.front().frame.y;
    double right = left + panels.front().frame.width;
    double bottom = top + panels.front().frame.height;
    for (const TrellisPanelLayout &panel : panels) {
        left = std::min(left, panel.frame.x);
        top = std::min(top, panel.frame.y);
        right = std::max(right, panel.frame.x + panel.frame.width);
        bottom = std::max(bottom, panel.frame.y + panel.frame.height);
    }
    return {left, top, right - left, bottom - top};
}

Rect IntersectionRect(const Rect &left, const Rect &right)
{
    const double x1 = std::max(left.x, right.x);
    const double y1 = std::max(left.y, right.y);
    const double x2 = std::min(left.x + left.width, right.x + right.width);
    const double y2 = std::min(left.y + left.height, right.y + right.height);
    if (x2 <= x1 || y2 <= y1) return {};
    return {x1, y1, x2 - x1, y2 - y1};
}

std::string Join(const std::vector<std::string> &values, const std::string &separator)
{
    std::string result;
    for (const std::string &value : values) {
        if (!result.empty()) result += separator;
        result += value;
    }
    return result;
}

std::string PanelKey(const std::vector<std::string> &levelIds)
{
    return levelIds.empty() ? "all" : Join(levelIds, "\x1f");
}

std::string ConditionPhrase(const std::vector<TrellisConditioningVariable> &conditions)
{
    if (conditions.empty()) return "";
    if (conditions.size() >= 3) {
        return "conditioned by " + std::to_string(conditions.size()) + " variables";
    }
    std::vector<std::string> labels;
    for (const TrellisConditioningVariable &condition : conditions) {
        labels.push_back(condition.variableLabel.empty() ? condition.variableId : condition.variableLabel);
    }
    return "conditioned by " + Join(labels, " and ");
}

void NormalizeConditionDimensions(std::vector<TrellisConditioningVariable> &conditions)
{
    if (conditions.empty()) return;
    if (conditions.size() == 1) {
        if (conditions.front().dimension == TrellisDimension::Nested) {
            conditions.front().dimension = TrellisDimension::Columns;
        }
        return;
    }
    bool hasRows = false;
    bool hasColumns = false;
    for (const TrellisConditioningVariable &condition : conditions) {
        hasRows = hasRows || condition.dimension == TrellisDimension::Rows;
        hasColumns = hasColumns || condition.dimension == TrellisDimension::Columns;
    }
    if (!hasColumns) conditions.front().dimension = TrellisDimension::Columns;
    if (!hasRows) {
        for (TrellisConditioningVariable &condition : conditions) {
            if (condition.dimension != TrellisDimension::Columns) {
                condition.dimension = TrellisDimension::Rows;
                hasRows = true;
                break;
            }
        }
    }
}

bool IsNumericColumn(const DataColumn &column)
{
    return NormalizeVariableType(column.type) == "numeric" && DataColumnAllowsNumeric(column);
}

bool IsCategoricalColumn(const DataColumn &column, int rowCount)
{
    if (VariableTypeIsFactorLike(column.type)) return true;
    return IsNumericColumn(column) && DataColumnLooksGroupingCandidate(column, rowCount);
}

std::vector<double> FiniteColumnValues(const DataColumn &column)
{
    std::vector<double> values;
    values.reserve(column.values.size());
    for (const std::string &cell : column.values) {
        double value = NAN;
        if (ParseDataCellDouble(cell, value) && std::isfinite(value)) values.push_back(value);
    }
    std::sort(values.begin(), values.end());
    return values;
}

std::vector<double> EqualWidthBreaks(const std::vector<double> &values, std::size_t requested)
{
    if (values.empty()) return {};
    const double minimum = values.front();
    const double maximum = values.back();
    if (minimum == maximum) return {minimum, maximum};
    const std::size_t count = std::max<std::size_t>(1, requested);
    std::vector<double> breaks;
    breaks.reserve(count + 1);
    for (std::size_t index = 0; index <= count; ++index) {
        breaks.push_back(minimum + (maximum - minimum) * static_cast<double>(index) /
            static_cast<double>(count));
    }
    breaks.back() = maximum;
    return breaks;
}

std::vector<double> EqualCountBreaks(const std::vector<double> &values, std::size_t requested)
{
    if (values.empty()) return {};
    std::vector<double> unique = values;
    unique.erase(std::unique(unique.begin(), unique.end()), unique.end());
    if (unique.size() == 1) return {unique.front(), unique.front()};
    const std::size_t count = std::min<std::size_t>(std::max<std::size_t>(1, requested), unique.size());
    std::vector<double> breaks{values.front()};
    for (std::size_t index = 1; index < count; ++index) {
        const double probability = static_cast<double>(index) / static_cast<double>(count);
        const std::size_t position = std::min(values.size() - 1,
            static_cast<std::size_t>(std::floor(probability * static_cast<double>(values.size()))));
        const double candidate = values[position];
        if (candidate > breaks.back() && candidate < values.back()) breaks.push_back(candidate);
    }
    if (values.back() > breaks.back()) breaks.push_back(values.back());
    return breaks;
}

struct PreparedCondition {
    TrellisConditioningVariable specification;
    const DataColumn *column = nullptr;
    std::vector<std::string> levelIds;
    std::vector<std::string> levelLabels;
    std::vector<std::string> rowLevelIds;
    std::vector<std::string> rowLevelLabels;
};

std::string IntervalLabel(double lower, double upper, bool last)
{
    return std::string("[") + FormatModelNumber(lower) + ", " +
        FormatModelNumber(upper) + (last ? "]" : ")");
}

bool PrepareCondition(const TrellisConditioningVariable &condition,
                      const DataColumn &column,
                      int rowCount,
                      PreparedCondition &prepared,
                      std::string *error)
{
    prepared.specification = condition;
    prepared.column = &column;
    prepared.rowLevelIds.resize(column.values.size());
    prepared.rowLevelLabels.resize(column.values.size());
    if (condition.kind == TrellisConditioningVariableKind::Categorical) {
        const bool numericInterpretation = IsNumericColumn(column);
        if (!IsCategoricalColumn(column, rowCount) && !numericInterpretation) {
            if (error) *error = "A categorical conditioning variable must have discrete categories.";
            return false;
        }
        std::vector<std::size_t> observedRows;
        for (std::size_t row = 0; row < column.values.size(); ++row) {
            const std::string value = DisplayValueForCell(column, row);
            if (!DataCellIsMissing(value)) observedRows.push_back(row);
        }
        prepared.levelIds = OrderedObservedLevels(column, observedRows, column.definedLevels);
        prepared.levelLabels = prepared.levelIds;
        for (std::size_t row : observedRows) {
            const std::string value = DisplayValueForCell(column, row);
            prepared.rowLevelIds[row] = value;
            prepared.rowLevelLabels[row] = value;
        }
        return !prepared.levelIds.empty();
    }
    if (!IsNumericColumn(column)) {
        if (error) *error = "A continuous conditioning variable must be numeric.";
        return false;
    }
    TrellisContinuousBinningSpecification binning = condition.binning.value_or(
        TrellisContinuousBinningSpecification{});
    binning.binCount = std::max<std::size_t>(2, std::min<std::size_t>(6, binning.binCount));
    const std::vector<double> values = FiniteColumnValues(column);
    std::vector<double> breaks;
    if (binning.method == TrellisContinuousBinningMethod::EqualCount) {
        breaks = EqualCountBreaks(values, binning.binCount);
    } else if (binning.method == TrellisContinuousBinningMethod::CustomBreaks) {
        breaks = binning.customBreaks;
        std::sort(breaks.begin(), breaks.end());
        breaks.erase(std::unique(breaks.begin(), breaks.end()), breaks.end());
    } else {
        breaks = EqualWidthBreaks(values, binning.binCount);
    }
    if (breaks.size() < 2) {
        if (error) *error = "The continuous conditioning variable has too few unique values.";
        return false;
    }
    for (std::size_t bin = 0; bin + 1 < breaks.size(); ++bin) {
        const bool last = bin + 2 == breaks.size();
        prepared.levelIds.push_back(condition.variableId + "#bin" + std::to_string(bin));
        prepared.levelLabels.push_back(IntervalLabel(breaks[bin], breaks[bin + 1], last));
    }
    for (std::size_t row = 0; row < column.values.size(); ++row) {
        double value = NAN;
        if (!ParseDataCellDouble(column.values[row], value) || !std::isfinite(value)) continue;
        for (std::size_t bin = 0; bin + 1 < breaks.size(); ++bin) {
            const bool last = bin + 2 == breaks.size();
            if (value >= breaks[bin] && (value < breaks[bin + 1] || (last && value <= breaks[bin + 1]))) {
                prepared.rowLevelIds[row] = prepared.levelIds[bin];
                prepared.rowLevelLabels[row] = prepared.levelLabels[bin];
                break;
            }
        }
    }
    return true;
}

void BuildPanelCombinationsRecursive(const std::vector<PreparedCondition> &conditions,
                                     std::size_t dimension,
                                     std::vector<std::string> &ids,
                                     std::vector<std::string> &labels,
                                     std::vector<std::vector<std::string>> &outIds,
                                     std::vector<std::vector<std::string>> &outLabels)
{
    if (dimension >= conditions.size()) {
        outIds.push_back(ids);
        outLabels.push_back(labels);
        return;
    }
    // The first condition is the column dimension and therefore varies fastest.
    const std::size_t current = conditions.size() - 1 - dimension;
    for (std::size_t level = 0; level < conditions[current].levelIds.size(); ++level) {
        ids[current] = conditions[current].levelIds[level];
        labels[current] = conditions[current].levelLabels[level];
        BuildPanelCombinationsRecursive(conditions, dimension + 1, ids, labels, outIds, outLabels);
    }
}

double Quantile(std::vector<double> values, double probability)
{
    if (values.empty()) return NAN;
    std::sort(values.begin(), values.end());
    const double position = probability * static_cast<double>(values.size() - 1);
    const std::size_t lower = static_cast<std::size_t>(std::floor(position));
    const std::size_t upper = static_cast<std::size_t>(std::ceil(position));
    const double fraction = position - static_cast<double>(lower);
    return values[lower] * (1.0 - fraction) + values[upper] * fraction;
}

const DataColumn *AnalysisColumn(const DataFrameModel &df, const std::string &name)
{
    const auto found = std::find_if(df.columns.begin(), df.columns.end(),
        [&](const DataColumn &column) { return column.name == name; });
    return found == df.columns.end() ? nullptr : &*found;
}

PlotAnalysisEffectiveType AnalysisTypeForColumn(const DataColumn *column)
{
    if (!column) return PlotAnalysisEffectiveType::Factor;
    const std::string type = NormalizeVariableType(column->type);
    if (type == "numeric") return PlotAnalysisEffectiveType::Numeric;
    if (type == "ordered") return PlotAnalysisEffectiveType::OrderedFactor;
    return PlotAnalysisEffectiveType::Factor;
}

bool AnalysisColumnIsBinary(const DataColumn *column)
{
    if (!column) return false;
    std::set<std::string> observed;
    for (const std::string &value : column->values) {
        if (DataCellIsMissing(value)) continue;
        observed.insert(value);
        if (observed.size() > 2) return false;
    }
    return observed.size() == 2;
}

void AppendUnique(std::vector<std::string> &values, const std::string &value)
{
    if (value.empty() || std::find(values.begin(), values.end(), value) != values.end()) return;
    values.push_back(value);
}

} // namespace

std::string TrellisPlotTypeName(TrellisPlotType type)
{
    switch (type) {
    case TrellisPlotType::Scatter: return "scatter";
    case TrellisPlotType::TimeSeries: return "time_series";
    case TrellisPlotType::Boxplot: return "boxplot";
    case TrellisPlotType::Bar: return "bar";
    case TrellisPlotType::Histogram: return "histogram";
    case TrellisPlotType::DataTable: return "data_table";
    }
    return "scatter";
}

std::optional<TrellisPlotType> ParseTrellisPlotType(const std::string &name)
{
    if (name == "scatter" || name == "scatterplot") return TrellisPlotType::Scatter;
    if (name == "time_series" || name == "timeseries" || name == "time") {
        return TrellisPlotType::TimeSeries;
    }
    if (name == "box" || name == "boxplot") return TrellisPlotType::Boxplot;
    if (name == "bar" || name == "barplot") return TrellisPlotType::Bar;
    if (name == "histogram" || name == "hist") return TrellisPlotType::Histogram;
    if (name == "data_table" || name == "table" || name == "data") {
        return TrellisPlotType::DataTable;
    }
    return std::nullopt;
}

PlotAnalysisContext BuildTrellisPlotAnalysisContext(
    const PlotModel &plot,
    const DataFrameModel &df)
{
    PlotAnalysisContext context;
    if (plot.kind != "trellis_scatterplot" || !plot.trellisSpecificationInitialized) return context;
    const TrellisPlotSpecification &specification = plot.trellisSpecification;

    auto appendVariable = [&](const std::string &variable,
                              PlotAnalysisVariableRole role,
                              std::optional<PlotAnalysisEffectiveType> forcedType = std::nullopt)
    {
        if (variable.empty() || !AnalysisColumn(df, variable)) return;
        const auto existing = std::find_if(context.variables.begin(), context.variables.end(),
            [&](const PlotAnalysisVariable &item) { return item.variableId == variable; });
        if (existing != context.variables.end()) return;
        const DataColumn *column = AnalysisColumn(df, variable);
        const PlotAnalysisEffectiveType type = forcedType.value_or(AnalysisTypeForColumn(column));
        context.variables.push_back({variable,
            column && !column->displayName.empty() ? column->displayName : variable,
            role, type, AnalysisColumnIsBinary(column)});
    };

    // Plot-type semantics determine the dependent variable.  In particular,
    // bars have no observed Y axis: their height is a derived count/percentage.
    // A split variable, when present, is the response represented by the bar
    // composition and is therefore the only valid bar-plot dependent variable.
    switch (specification.plotType) {
    case TrellisPlotType::Scatter:
    case TrellisPlotType::TimeSeries:
    case TrellisPlotType::Boxplot:
        context.dependentVariable = specification.yVariableId;
        break;
    case TrellisPlotType::Bar:
        context.dependentVariable = specification.splitVariableId;
        break;
    case TrellisPlotType::Histogram:
        // The observed variable is the response represented by a histogram.
        // Bin counts, percentages and densities are derived drawing values
        // and must never become model variables.
        context.dependentVariable = specification.xVariableId;
        break;
    case TrellisPlotType::DataTable:
        break;
    }

    if (specification.plotType == TrellisPlotType::Bar) {
        appendVariable(specification.xVariableId, PlotAnalysisVariableRole::X,
                       PlotAnalysisEffectiveType::Factor);
        appendVariable(specification.splitVariableId, PlotAnalysisVariableRole::Y,
                       PlotAnalysisEffectiveType::Factor);
    } else if (specification.plotType == TrellisPlotType::Histogram) {
        appendVariable(specification.xVariableId, PlotAnalysisVariableRole::X,
                       PlotAnalysisEffectiveType::Numeric);
    } else if (specification.plotType == TrellisPlotType::Boxplot) {
        // Every nested X variable identifies groups whose distributions are
        // shown and is categorical in the model even when stored numerically.
        std::vector<std::string> groups = specification.boxplotGroupingVariableIds;
        if (groups.empty()) groups.push_back(specification.xVariableId);
        for (const std::string &group : groups)
            appendVariable(group, PlotAnalysisVariableRole::X,
                           PlotAnalysisEffectiveType::Factor);
        appendVariable(specification.yVariableId, PlotAnalysisVariableRole::Y,
                       PlotAnalysisEffectiveType::Numeric);
    } else {
        appendVariable(specification.xVariableId, PlotAnalysisVariableRole::X);
        appendVariable(specification.yVariableId, PlotAnalysisVariableRole::Y);
        appendVariable(specification.splitVariableId, PlotAnalysisVariableRole::Split,
                       PlotAnalysisEffectiveType::Factor);
    }
    appendVariable(specification.groupingVariableId, PlotAnalysisVariableRole::Grouping,
                   PlotAnalysisEffectiveType::Factor);
    for (const TrellisConditioningVariable &condition : specification.conditioningVariables) {
        PlotAnalysisVariableRole role = PlotAnalysisVariableRole::ConditioningNested;
        if (condition.dimension == TrellisDimension::Rows) role = PlotAnalysisVariableRole::ConditioningRow;
        else if (condition.dimension == TrellisDimension::Columns) role = PlotAnalysisVariableRole::ConditioningColumn;
        appendVariable(condition.variableId, role,
            condition.orderedCategories ? PlotAnalysisEffectiveType::OrderedFactor
                                        : PlotAnalysisEffectiveType::Factor);
    }

    for (const PlotAnalysisVariable &variable : context.variables) {
        if (variable.effectiveType == PlotAnalysisEffectiveType::Numeric)
            AppendUnique(context.numericVariables, variable.variableId);
        else {
            AppendUnique(context.categoricalVariables, variable.variableId);
            if (variable.role == PlotAnalysisVariableRole::Split ||
                variable.role == PlotAnalysisVariableRole::Grouping ||
                variable.role == PlotAnalysisVariableRole::ConditioningRow ||
                variable.role == PlotAnalysisVariableRole::ConditioningColumn ||
                variable.role == PlotAnalysisVariableRole::ConditioningNested)
                AppendUnique(context.groupingVariables, variable.variableId);
        }
        if (variable.variableId != context.dependentVariable)
            AppendUnique(context.predictors, variable.variableId);
    }

    const auto dependent = std::find_if(context.variables.begin(), context.variables.end(),
        [&](const PlotAnalysisVariable &variable)
        { return variable.variableId == context.dependentVariable; });
    const bool supportsInterceptOnly =
        specification.plotType == TrellisPlotType::Histogram ||
        specification.plotType == TrellisPlotType::Boxplot;
    if (dependent != context.variables.end() &&
        (!context.predictors.empty() || supportsInterceptOnly)) {
        if (dependent->effectiveType == PlotAnalysisEffectiveType::Numeric)
            context.modelKind = PlotAnalysisModelKind::Linear;
        else if (dependent->binary)
            context.modelKind = PlotAnalysisModelKind::Binomial;
    }
    context.offersModel = context.modelKind != PlotAnalysisModelKind::None;
    context.offersCorrelations = context.numericVariables.size() >= 2;
    context.offersContingencyTables = context.categoricalVariables.size() >= 2;
    context.offersDescriptives = !context.variables.empty();
    return context;
}

PlotAnalysisContext BuildPlotAnalysisContext(
    const PlotModel &plot,
    const DataFrameModel &df)
{
    if (plot.kind == "trellis_scatterplot")
        return BuildTrellisPlotAnalysisContext(plot, df);

    PlotAnalysisContext context;
    auto appendVariable = [&](const std::string &variable,
                              PlotAnalysisVariableRole role,
                              std::optional<PlotAnalysisEffectiveType> forcedType = std::nullopt)
    {
        const DataColumn *column = AnalysisColumn(df, variable);
        if (variable.empty() || !column) return;
        if (std::any_of(context.variables.begin(), context.variables.end(),
            [&](const PlotAnalysisVariable &item) { return item.variableId == variable; })) return;
        context.variables.push_back({variable,
            column->displayName.empty() ? variable : column->displayName,
            role, forcedType.value_or(AnalysisTypeForColumn(column)),
            AnalysisColumnIsBinary(column)});
    };

    if (plot.kind == "scatter" || plot.kind == "time_series") {
        context.dependentVariable = plot.yLabel;
        appendVariable(plot.xLabel, PlotAnalysisVariableRole::X);
        appendVariable(plot.yLabel, PlotAnalysisVariableRole::Y);
        if (plot.kind == "time_series")
            appendVariable(plot.timeSeriesGroupVariable,
                           PlotAnalysisVariableRole::Grouping,
                           PlotAnalysisEffectiveType::Factor);
    } else if (plot.kind == "barplot") {
        context.dependentVariable = plot.barplotSplitVariable;
        const std::vector<std::string> xVariables = plot.barplotXVariables.empty()
            ? std::vector<std::string>{plot.xLabel} : plot.barplotXVariables;
        for (const std::string &variable : xVariables)
            appendVariable(variable, PlotAnalysisVariableRole::X,
                           PlotAnalysisEffectiveType::Factor);
        appendVariable(plot.barplotSplitVariable, PlotAnalysisVariableRole::Y,
                       PlotAnalysisEffectiveType::Factor);
    } else if (plot.kind == "histogram") {
        context.dependentVariable = plot.xLabel;
        appendVariable(plot.xLabel, PlotAnalysisVariableRole::X,
                       PlotAnalysisEffectiveType::Numeric);
    } else if (plot.kind == "scatter_matrix") {
        const std::vector<std::string> matrixVariables =
            plot.scatterMatrixVariables.empty()
            ? ScatterMatrixVariablesForModel(plot)
            : plot.scatterMatrixVariables;
        for (const std::string &variable : matrixVariables)
            appendVariable(variable, PlotAnalysisVariableRole::X,
                           PlotAnalysisEffectiveType::Numeric);
    } else if (plot.kind == "boxplot") {
        if (!BoxplotUsesVariableAxes(plot)) {
            context.dependentVariable = plot.yLabel;
            const std::vector<std::string> groups = plot.boxplotGroupingVariables.empty()
                ? std::vector<std::string>{plot.xLabel}
                : plot.boxplotGroupingVariables;
            for (const std::string &group : groups)
                appendVariable(group, PlotAnalysisVariableRole::X,
                               PlotAnalysisEffectiveType::Factor);
            appendVariable(plot.yLabel, PlotAnalysisVariableRole::Y,
                           PlotAnalysisEffectiveType::Numeric);
        } else if (plot.boxplotVariables.size() == 1) {
            // A one-variable boxplot can seed an intercept-only model whose
            // predictors can then be edited in the normal model window.
            context.dependentVariable = plot.boxplotVariables.front();
            appendVariable(context.dependentVariable,
                           PlotAnalysisVariableRole::Y,
                           PlotAnalysisEffectiveType::Numeric);
        } else if (!plot.boxplotVariables.empty()) {
            for (const std::string &variable : plot.boxplotVariables)
                appendVariable(variable, PlotAnalysisVariableRole::X,
                               PlotAnalysisEffectiveType::Numeric);
        }
    } else {
        return context;
    }

    for (const PlotAnalysisVariable &variable : context.variables) {
        if (variable.effectiveType == PlotAnalysisEffectiveType::Numeric)
            AppendUnique(context.numericVariables, variable.variableId);
        else AppendUnique(context.categoricalVariables, variable.variableId);
        if (variable.role == PlotAnalysisVariableRole::Grouping ||
            variable.role == PlotAnalysisVariableRole::Split)
            AppendUnique(context.groupingVariables, variable.variableId);
        if (variable.variableId != context.dependentVariable)
            AppendUnique(context.predictors, variable.variableId);
    }
    const auto dependent = std::find_if(context.variables.begin(), context.variables.end(),
        [&](const PlotAnalysisVariable &variable)
        { return variable.variableId == context.dependentVariable; });
    const bool supportsInterceptOnly = plot.kind == "histogram" ||
        (plot.kind == "boxplot" && !context.dependentVariable.empty());
    if (dependent != context.variables.end() &&
        (!context.predictors.empty() || supportsInterceptOnly)) {
        if (dependent->effectiveType == PlotAnalysisEffectiveType::Numeric)
            context.modelKind = PlotAnalysisModelKind::Linear;
        else if (dependent->binary)
            context.modelKind = PlotAnalysisModelKind::Binomial;
    }
    context.offersModel = context.modelKind != PlotAnalysisModelKind::None;
    context.offersCorrelations = context.numericVariables.size() >= 2;
    context.offersContingencyTables = context.categoricalVariables.size() >= 2;
    context.offersDescriptives = !context.variables.empty();
    return context;
}

std::map<std::string, std::string> PlotAnalysisTermTypes(
    const PlotAnalysisContext &context)
{
    std::map<std::string, std::string> result;
    for (const PlotAnalysisVariable &variable : context.variables) {
        if (variable.variableId.empty() ||
            variable.variableId == context.dependentVariable) continue;
        result[variable.variableId] =
            variable.effectiveType == PlotAnalysisEffectiveType::Numeric
            ? "numeric" : "factor";
    }
    return result;
}

std::vector<PlotAnalysisMenuOption> PlotAnalysisMenuOptions(
    const PlotAnalysisContext &context)
{
    std::vector<PlotAnalysisMenuOption> options;
    if (context.offersModel) options.push_back({"Model...", "PLOT_ANALYZE_MODEL"});
    if (context.offersCorrelations)
        options.push_back({"Correlations...", "PLOT_ANALYZE_CORRELATIONS"});
    if (context.numericVariables.size() >= 2)
        options.push_back({"Factor analysis...", "PLOT_ANALYZE_FACTOR"});
    if (context.offersContingencyTables)
        options.push_back({"Contingency tables...", "PLOT_ANALYZE_CONTINGENCY"});
    if (context.offersDescriptives)
        options.push_back({"Descriptives...", "PLOT_ANALYZE_DESCRIPTIVES"});
    return options;
}

void InitializeTrellisDataTableSpecification(PlotModel &plot,
                                             const DataFrameModel &df,
                                             bool restoreFromPlot)
{
    InitializeTrellisSpecificationFromLegacy(plot);
    TrellisDataTableSpecification &table = plot.trellisSpecification.dataTable;
    if (table.initialized && !restoreFromPlot) return;
    std::vector<std::string> variables;
    auto add = [&](const std::string &name) {
        if (name.empty() || !FindDataColumnInDataFrame(df, name) ||
            std::find(variables.begin(), variables.end(), name) != variables.end() ||
            variables.size() >= 8) return;
        variables.push_back(name);
    };
    add(plot.trellisSpecification.xVariableId);
    for (const std::string &group :
         plot.trellisSpecification.boxplotGroupingVariableIds) add(group);
    add(plot.trellisSpecification.yVariableId);
    add(plot.trellisSpecification.splitVariableId);
    add(plot.trellisSpecification.groupingVariableId);
    for (const TrellisConditioningVariable &condition :
         plot.trellisSpecification.conditioningVariables) add(condition.variableId);
    for (const DataColumn &column : df.columns) {
        if (variables.size() >= 6) break;
        add(column.name);
    }
    table.displayedVariableIds = std::move(variables);
    table.initialized = true;
}

std::vector<CaseId> TrellisPanelOriginalRows(const PlotModel &plot,
                                             const std::string &panelId)
{
    std::vector<CaseId> rows;
    for (std::size_t index = 0; index < plot.points.size(); ++index) {
        if (index < plot.trellisPointPanels.size() &&
            plot.trellisPointPanels[index] == panelId) {
            rows.push_back(plot.points[index].row);
        }
    }
    std::sort(rows.begin(), rows.end());
    rows.erase(std::unique(rows.begin(), rows.end()), rows.end());
    return rows;
}

std::vector<CaseId> OrderedTrellisDataTableRows(
    const TrellisDataTableSpecification &specification,
    const DataFrameModel &df,
    const std::vector<CaseId> &panelRows,
    const std::set<CaseId> &selection)
{
    std::vector<CaseId> rows = panelRows;
    std::stable_sort(rows.begin(), rows.end());
    if (specification.sort.order == TrellisDataTableRowOrder::SelectedFirst) {
        std::stable_partition(rows.begin(), rows.end(), [&](CaseId row) {
            return selection.find(row) != selection.end();
        });
        return rows;
    }
    if (specification.sort.order != TrellisDataTableRowOrder::VariableAscending &&
        specification.sort.order != TrellisDataTableRowOrder::VariableDescending) return rows;
    const DataColumn *column = specification.sort.variableId
        ? FindDataColumnInDataFrame(df, *specification.sort.variableId) : nullptr;
    if (!column) return rows;
    const bool numeric = NormalizeVariableType(column->type) == "numeric" &&
        DataColumnAllowsNumeric(*column);
    const bool descending =
        specification.sort.order == TrellisDataTableRowOrder::VariableDescending;
    auto less = [&](CaseId leftId, CaseId rightId) {
        const std::size_t left = leftId > 0 ? static_cast<std::size_t>(leftId - 1) : 0;
        const std::size_t right = rightId > 0 ? static_cast<std::size_t>(rightId - 1) : 0;
        const std::string leftValue = left < column->values.size() ? column->values[left] : "";
        const std::string rightValue = right < column->values.size() ? column->values[right] : "";
        const bool leftMissing = DataCellIsMissing(leftValue);
        const bool rightMissing = DataCellIsMissing(rightValue);
        if (leftMissing != rightMissing) return !leftMissing;
        if (leftMissing) return leftId < rightId;
        if (numeric) {
            double leftNumber = NAN, rightNumber = NAN;
            const bool leftOk = ParseDataCellDouble(leftValue, leftNumber);
            const bool rightOk = ParseDataCellDouble(rightValue, rightNumber);
            if (leftOk && rightOk && leftNumber != rightNumber) {
                return descending ? leftNumber > rightNumber : leftNumber < rightNumber;
            }
        }
        if (VariableTypeIsFactorLike(column->type) && !column->definedLevels.empty()) {
            const std::string leftLabel = DisplayValueForCell(*column, left);
            const std::string rightLabel = DisplayValueForCell(*column, right);
            const auto leftLevel = std::find(column->definedLevels.begin(), column->definedLevels.end(), leftLabel);
            const auto rightLevel = std::find(column->definedLevels.begin(), column->definedLevels.end(), rightLabel);
            if (leftLevel != rightLevel) {
                return descending ? leftLevel > rightLevel : leftLevel < rightLevel;
            }
        }
        if (leftValue != rightValue) {
            return descending ? leftValue > rightValue : leftValue < rightValue;
        }
        return leftId < rightId;
    };
    std::stable_sort(rows.begin(), rows.end(), less);
    return rows;
}

namespace {
std::string TsvCell(std::string value)
{
    for (char &ch : value) if (ch == '\t' || ch == '\r' || ch == '\n') ch = ' ';
    return value;
}

std::vector<std::string> TrellisDataTableExportVariables(const PlotModel &plot)
{
    std::vector<std::string> variables =
        plot.trellisSpecification.dataTable.displayedVariableIds;
    for (const std::string &group :
         plot.trellisSpecification.boxplotGroupingVariableIds) {
        if (std::find(variables.begin(), variables.end(), group) == variables.end())
            variables.push_back(group);
    }
    for (const TrellisConditioningVariable &condition :
         plot.trellisSpecification.conditioningVariables) {
        if (std::find(variables.begin(), variables.end(), condition.variableId) == variables.end()) {
            variables.push_back(condition.variableId);
        }
    }
    return variables;
}
}

std::string TrellisDataTableTSV(const PlotModel &plot,
                                const DataFrameModel &df,
                                const std::vector<CaseId> &rows,
                                bool includePanelColumns,
                                const std::string &panelId)
{
    std::ostringstream out;
    if (includePanelColumns) out << "Panel_ID\t";
    if (plot.trellisSpecification.dataTable.showOriginalRowIds) out << "Row\t";
    const std::vector<std::string> variables = TrellisDataTableExportVariables(plot);
    for (std::size_t i = 0; i < variables.size(); ++i) {
        if (i) out << '\t';
        out << TsvCell(variables[i]);
    }
    out << '\n';
    for (CaseId caseId : rows) {
        const std::size_t row = caseId > 0 ? static_cast<std::size_t>(caseId - 1) : 0;
        if (row >= static_cast<std::size_t>(std::max(0, df.rows))) continue;
        if (includePanelColumns) out << TsvCell(panelId) << '\t';
        if (plot.trellisSpecification.dataTable.showOriginalRowIds) out << caseId << '\t';
        for (std::size_t i = 0; i < variables.size(); ++i) {
            if (i) out << '\t';
            const DataColumn *column = FindDataColumnInDataFrame(df, variables[i]);
            if (column) out << TsvCell(DisplayValueForDataFrameCell(df, *column, row));
        }
        out << '\n';
    }
    return out.str();
}

std::string TrellisDataTableAllPanelsCSV(const PlotModel &plot,
                                         const DataFrameModel &df,
                                         const std::set<CaseId> &selection)
{
    std::ostringstream out;
    out << "Panel_ID,Row_Condition_ID,Row_Condition_Label,Column_Condition_ID,"
           "Column_Condition_Label,Original_Row_ID,Selected";
    const std::vector<std::string> exportVariables = TrellisDataTableExportVariables(plot);
    for (const std::string &variable : exportVariables) {
        out << ',' << CsvEscape(variable);
    }
    out << '\n';
    for (std::size_t pointIndex = 0; pointIndex < plot.points.size(); ++pointIndex) {
        const CaseId caseId = plot.points[pointIndex].row;
        const std::size_t row = caseId > 0 ? static_cast<std::size_t>(caseId - 1) : 0;
        if (row >= static_cast<std::size_t>(std::max(0, df.rows))) continue;
        const std::string panelId = pointIndex < plot.trellisPointPanels.size()
            ? plot.trellisPointPanels[pointIndex] : "all";
        std::string rowConditionId, rowConditionLabel, columnConditionId, columnConditionLabel;
        const std::vector<std::string> values = pointIndex < plot.trellisPointConditionValues.size()
            ? plot.trellisPointConditionValues[pointIndex] : std::vector<std::string>{};
        for (std::size_t conditionIndex = 0;
             conditionIndex < plot.trellisSpecification.conditioningVariables.size() &&
             conditionIndex < values.size(); ++conditionIndex) {
            const TrellisConditioningVariable &condition =
                plot.trellisSpecification.conditioningVariables[conditionIndex];
            auto append = [](std::string &target, const std::string &value) {
                if (!target.empty()) target += " | ";
                target += value;
            };
            if (condition.dimension == TrellisDimension::Rows) {
                append(rowConditionId, condition.variableId);
                append(rowConditionLabel, values[conditionIndex]);
            } else if (condition.dimension == TrellisDimension::Columns) {
                append(columnConditionId, condition.variableId);
                append(columnConditionLabel, values[conditionIndex]);
            }
        }
        out << CsvEscape(panelId) << ',' << CsvEscape(rowConditionId) << ','
            << CsvEscape(rowConditionLabel) << ',' << CsvEscape(columnConditionId) << ','
            << CsvEscape(columnConditionLabel) << ',' << caseId << ','
            << (selection.find(caseId) != selection.end() ? "TRUE" : "FALSE");
        for (const std::string &variable : exportVariables) {
            const DataColumn *column = FindDataColumnInDataFrame(df, variable);
            out << ',' << (column && row < column->values.size() ? CsvEscape(column->values[row]) : "");
        }
        out << '\n';
    }
    return out.str();
}

std::string TrellisDataTablePanelCSV(const PlotModel &plot,
                                     const DataFrameModel &df,
                                     const std::string &panelId,
                                     const std::set<CaseId> &selection)
{
    std::ostringstream out;
    out << "Panel_ID,Original_Row_ID,Selected";
    const std::vector<std::string> exportVariables = TrellisDataTableExportVariables(plot);
    for (const std::string &variable : exportVariables) {
        out << ',' << CsvEscape(variable);
    }
    out << '\n';
    const std::vector<CaseId> rows = TrellisPanelOriginalRows(plot, panelId);
    for (CaseId caseId : rows) {
        const std::size_t row = caseId > 0 ? static_cast<std::size_t>(caseId - 1) : 0;
        if (row >= static_cast<std::size_t>(std::max(0, df.rows))) continue;
        out << CsvEscape(panelId) << ',' << caseId << ','
            << (selection.find(caseId) != selection.end() ? "TRUE" : "FALSE");
        for (const std::string &variable : exportVariables) {
            const DataColumn *column = FindDataColumnInDataFrame(df, variable);
            out << ',' << (column && row < column->values.size()
                ? CsvEscape(column->values[row]) : "");
        }
        out << '\n';
    }
    return out.str();
}

void InitializeTrellisSpecificationFromLegacy(PlotModel &plot)
{
    if (plot.trellisSpecificationInitialized) return;
    plot.trellisSpecification = TrellisPlotSpecification{};
    plot.trellisSpecification.plotType = TrellisPlotType::Scatter;
    plot.trellisSpecification.xVariableId = plot.xLabel;
    plot.trellisSpecification.yVariableId = plot.yLabel;
    plot.trellisSpecification.splitVariableId = plot.barplotSplitVariable;
    plot.trellisSpecification.layoutMode = plot.trellisLayoutMode;
    plot.trellisSpecification.panelOrder = plot.trellisPanelOrder;
    if (!plot.trellisConditionVariable.empty()) {
        plot.trellisSpecification.conditioningVariables.push_back({
            plot.trellisConditionVariable,
            plot.trellisConditionVariable,
            TrellisConditioningVariableKind::Categorical,
            TrellisDimension::Columns,
            std::nullopt
        });
    }
    const std::string legacyDefault = TrellisScatterplotDefaultTitle(
        plot.xLabel, plot.yLabel, plot.trellisConditionVariable);
    if (!plot.title.empty() && plot.title != legacyDefault) {
        plot.trellisSpecification.hasCustomTitle = true;
        plot.trellisSpecification.customTitle = plot.title;
    }
    plot.trellisSpecificationInitialized = true;
}

TrellisDerivedMetadata DeriveTrellisMetadata(const PlotModel &plot)
{
    TrellisPlotSpecification specification = plot.trellisSpecification;
    if (!plot.trellisSpecificationInitialized) {
        specification.xVariableId = plot.xLabel;
        specification.yVariableId = plot.yLabel;
        if (!plot.trellisConditionVariable.empty()) {
            specification.conditioningVariables.push_back({plot.trellisConditionVariable,
                plot.trellisConditionVariable,
                TrellisConditioningVariableKind::Categorical,
                TrellisDimension::Columns, std::nullopt});
        }
    }
    TrellisDerivedMetadata result;
    result.xVariableId = specification.xVariableId;
    result.yVariableId = specification.yVariableId;
    for (const TrellisConditioningVariable &condition : specification.conditioningVariables) {
        result.conditioningVariableIds.push_back(condition.variableId);
    }
    const std::string condition = ConditionPhrase(specification.conditioningVariables);
    switch (specification.plotType) {
    case TrellisPlotType::Scatter:
        result.xAxisLabel = specification.xVariableId;
        result.yAxisLabel = specification.yVariableId;
        result.internalTitle = specification.yVariableId + " by " + specification.xVariableId;
        break;
    case TrellisPlotType::TimeSeries:
        result.xAxisLabel = specification.xVariableId;
        result.yAxisLabel = specification.yVariableId;
        result.internalTitle = specification.yVariableId + " over " + specification.xVariableId;
        break;
    case TrellisPlotType::Boxplot:
        result.xAxisLabel = BoxplotGroupingLabel(
            specification.boxplotGroupingVariableIds.empty()
                ? std::vector<std::string>{specification.xVariableId}
                : specification.boxplotGroupingVariableIds);
        result.yAxisLabel = specification.yVariableId;
        result.internalTitle = specification.yVariableId + " by " + result.xAxisLabel;
        break;
    case TrellisPlotType::Bar:
        result.xAxisLabel = specification.xVariableId;
        {
            const std::string measure = specification.barMeasure == "percent"
                ? "Percent within panel"
                : (specification.barMeasure == "conditional_percent"
                    ? "Percent within X" : "Count");
            // A split bar chart represents a categorical response on its Y
            // dimension.  Keep the derived height measure, but identify the
            // variable so the axis remains meaningful and interactive.
            result.yAxisLabel = specification.splitVariableId.empty()
                ? measure : specification.splitVariableId + " (" + measure + ")";
        }
        result.internalTitle = specification.xVariableId;
        break;
    case TrellisPlotType::Histogram:
        result.xAxisLabel = specification.xVariableId;
        result.yAxisLabel = specification.histogramMeasure == "density" ? "Density" :
            (specification.histogramMeasure == "percent" ? "Percent" : "Count");
        result.internalTitle = "Distribution of " + specification.xVariableId;
        break;
    case TrellisPlotType::DataTable:
        result.xAxisLabel.clear();
        result.yAxisLabel.clear();
        result.internalTitle = "Data table";
        break;
    }
    if (!condition.empty()) result.internalTitle += ", " + condition;
    if (specification.hasCustomTitle && !specification.customTitle.empty()) {
        result.internalTitle = specification.customTitle;
    }
    result.windowTitle = "Trellis Plot — " + plot.group;
    result.accessibleDescription = result.internalTitle + ". X axis: " + result.xAxisLabel +
        ". Y axis: " + result.yAxisLabel + ".";
    return result;
}

bool ValidateTrellisSpecification(const PlotModel &plot,
                                  const DataFrameModel &df,
                                  std::string *error)
{
    TrellisPlotSpecification specification = plot.trellisSpecification;
    if (!plot.trellisSpecificationInitialized) {
        specification.xVariableId = plot.xLabel;
        specification.yVariableId = plot.yLabel;
    }
    const DataColumn *x = FindDataColumnInDataFrame(df, specification.xVariableId);
    const DataColumn *y = specification.yVariableId.empty() ? nullptr :
        FindDataColumnInDataFrame(df, specification.yVariableId);
    if (specification.plotType != TrellisPlotType::DataTable && !x) {
        if (error) *error = "The selected X variable is unavailable.";
        return false;
    }
    if ((specification.plotType == TrellisPlotType::Scatter ||
         specification.plotType == TrellisPlotType::TimeSeries ||
         specification.plotType == TrellisPlotType::Boxplot) && !y) {
        if (error) *error = "The selected Y variable is unavailable.";
        return false;
    }
    if (specification.plotType == TrellisPlotType::Scatter) {
        if (specification.xVariableId == specification.yVariableId) {
            if (error) *error = "X and Y must be different numeric variables.";
            return false;
        }
        if (!IsNumericColumn(*x) || !IsNumericColumn(*y)) {
            if (error) *error = "Scatterplot X and Y must be numeric variables.";
            return false;
        }
    } else if (specification.plotType == TrellisPlotType::TimeSeries) {
        if (specification.xVariableId == specification.yVariableId || !IsNumericColumn(*y) ||
            (NormalizeVariableType(x->type) != "numeric" &&
             NormalizeVariableType(x->type) != "datetime")) {
            if (error) *error = "A trellis time series needs different time and numeric value variables.";
            return false;
        }
    } else if (specification.plotType == TrellisPlotType::Boxplot) {
        std::vector<std::string> groups = specification.boxplotGroupingVariableIds;
        if (groups.empty()) groups.push_back(specification.xVariableId);
        bool validGroups = !groups.empty();
        std::set<std::string> seenGroups;
        for (const std::string &group : groups) {
            const DataColumn *column = FindDataColumnInDataFrame(df, group);
            validGroups = validGroups && column && seenGroups.insert(group).second &&
                IsCategoricalColumn(*column, df.rows);
        }
        if (!validGroups || !IsNumericColumn(*y)) {
            if (error) *error = "A trellis boxplot needs a categorical X and numeric Y variable.";
            return false;
        }
    } else if (specification.plotType == TrellisPlotType::Bar) {
        if (!IsCategoricalColumn(*x, df.rows)) {
            if (error) *error = "A trellis bar chart needs a categorical X variable.";
            return false;
        }
    } else if (specification.plotType == TrellisPlotType::Histogram && !IsNumericColumn(*x)) {
        if (error) *error = "A trellis histogram needs a numeric X variable.";
        return false;
    }
    std::set<std::string> seen;
    for (const TrellisConditioningVariable &condition : specification.conditioningVariables) {
        if (condition.variableId.empty() || !seen.insert(condition.variableId).second) {
            if (error) *error = "Conditioning variable IDs must be non-empty and unique.";
            return false;
        }
        const DataColumn *column = FindDataColumnInDataFrame(df, condition.variableId);
        if (!column) { if (error) *error = "A conditioning variable is unavailable."; return false; }
        if (condition.kind == TrellisConditioningVariableKind::Categorical &&
            !IsCategoricalColumn(*column, df.rows)) {
            if (error) *error = "A categorical conditioning variable must have discrete categories.";
            return false;
        }
        if (condition.kind == TrellisConditioningVariableKind::ContinuousBinned &&
            !IsNumericColumn(*column)) {
            if (error) *error = "A continuous conditioning variable must be numeric.";
            return false;
        }
    }
    const std::string groupingVariable = specification.plotType == TrellisPlotType::TimeSeries
        ? specification.groupingVariableId : specification.splitVariableId;
    if (specification.plotType != TrellisPlotType::DataTable && !groupingVariable.empty()) {
        const DataColumn *split = FindDataColumnInDataFrame(df, groupingVariable);
        if (!split || !IsCategoricalColumn(*split, df.rows)) {
            if (error) *error = specification.plotType == TrellisPlotType::TimeSeries
                ? "A time-series grouping variable must be categorical."
                : "A split variable must be categorical.";
            return false;
        }
    }
    return true;
}

std::vector<std::string> OrderedTrellisPanelLevels(const PlotModel &plot)
{
    std::vector<std::string> levels = plot.trellisPanelLevels;
    if (plot.trellisSpecificationInitialized &&
        plot.trellisSpecification.conditioningVariables.size() > 1) return levels;
    if (plot.trellisPanelOrder == "defined") return levels;
    const bool allNumeric = std::all_of(levels.begin(), levels.end(), [](const std::string &label) {
        double value = NAN;
        return NumericLabel(label, value);
    });
    std::stable_sort(levels.begin(), levels.end(), [allNumeric](const std::string &left,
                                                               const std::string &right) {
        if (allNumeric) {
            double leftValue = NAN;
            double rightValue = NAN;
            NumericLabel(left, leftValue);
            NumericLabel(right, rightValue);
            if (leftValue != rightValue) return leftValue < rightValue;
        }
        return left < right;
    });
    if (plot.trellisPanelOrder == "descending") std::reverse(levels.begin(), levels.end());
    return levels;
}

std::vector<TrellisAxisTick> BuildTrellisAxisTicks(double minimum,
                                                   double maximum,
                                                   const Rect &plotRect,
                                                   bool horizontal,
                                                   int targetCount)
{
    std::vector<TrellisAxisTick> result;
    if (!IsValidRect(plotRect) || !std::isfinite(minimum) || !std::isfinite(maximum) ||
        maximum <= minimum) return result;
    const std::vector<BoxplotAxisTick> niceTicks =
        BoxplotYAxisTicks(minimum, maximum, std::max(4, std::min(7, targetCount)));
    result.reserve(niceTicks.size());
    for (const BoxplotAxisTick &tick : niceTicks) {
        const double fraction = (tick.value - minimum) / (maximum - minimum);
        const double position = horizontal
            ? plotRect.x + plotRect.width * fraction
            : plotRect.y + plotRect.height * (1.0 - fraction);
        result.push_back({tick.value, position, tick.label});
    }
    if (horizontal && result.size() > 2) {
        std::vector<TrellisAxisTick> filtered;
        filtered.reserve(result.size());
        double lastRight = -std::numeric_limits<double>::infinity();
        for (std::size_t index = 0; index < result.size(); ++index) {
            const double estimatedWidth = std::max(8.0, result[index].label.size() * 5.7);
            const double left = result[index].position - estimatedWidth / 2.0;
            const double right = result[index].position + estimatedWidth / 2.0;
            const bool last = index + 1 == result.size();
            if (filtered.empty() || left >= lastRight + 4.0) {
                filtered.push_back(result[index]);
                lastRight = right;
            } else if (last && filtered.size() > 1) {
                filtered.back() = result[index];
                lastRight = right;
            }
        }
        result = std::move(filtered);
    }
    return result;
}

void PlaceZeroBaselineTicks(std::vector<TrellisAxisTick> &ticks,
                            const Rect &plotRect, double minimum, double maximum)
{
    if (!(maximum > minimum)) return;
    for (TrellisAxisTick &tick : ticks) {
        tick.position = ZeroBaselineY(plotRect,
            (tick.value - minimum) / (maximum - minimum));
    }
}

bool TrellisConditionColumnIsValid(const DataColumn &column,
                                   int rowCount,
                                   std::string *error)
{
    const bool supportedType = VariableTypeIsFactorLike(column.type) ||
        (NormalizeVariableType(column.type) == "numeric" && DataColumnAllowsNumeric(column));
    if (!supportedType || !DataColumnLooksGroupingCandidate(column, rowCount)) {
        if (error) *error = "The conditioning variable must be categorical or a discrete numeric variable.";
        return false;
    }
    const int levels = DataColumnObservedLevelCount(column);
    if (NormalizeVariableType(column.type) == "numeric" &&
        levels > std::max(3, static_cast<int>(std::ceil(std::max(0, rowCount) / 4.0)))) {
        if (error) *error = "A continuous conditioning variable must be discrete and have relatively few categories.";
        return false;
    }
    if (levels > static_cast<int>(kMaximumTrellisPanels)) {
        if (error) *error = "The conditioning variable has too many categories for a trellis plot (maximum 20).";
        return false;
    }
    return true;
}

bool RebuildTrellisPlotFromDataFrame(PlotModel &plot,
                                     const DataFrameModel &df,
                                     std::string *error)
{
    InitializeTrellisSpecificationFromLegacy(plot);
    if (!ValidateTrellisSpecification(plot, df, error)) return false;
    TrellisPlotSpecification &specification = plot.trellisSpecification;
    NormalizeConditionDimensions(specification.conditioningVariables);
    const DataColumn *xColumn = FindDataColumnInDataFrame(df, specification.xVariableId);
    const DataColumn *yColumn = specification.yVariableId.empty() ? nullptr :
        FindDataColumnInDataFrame(df, specification.yVariableId);
    const std::string seriesOrSplitVariable = specification.plotType == TrellisPlotType::TimeSeries
        ? specification.groupingVariableId : specification.splitVariableId;
    const DataColumn *splitColumn = seriesOrSplitVariable.empty() ? nullptr :
        FindDataColumnInDataFrame(df, seriesOrSplitVariable);
    if (specification.plotType == TrellisPlotType::TimeSeries) {
        plot.timeSeriesTimeType = InferTimeSeriesTimeType(*xColumn);
    }

    std::vector<PreparedCondition> conditions;
    conditions.reserve(specification.conditioningVariables.size());
    for (const TrellisConditioningVariable &condition : specification.conditioningVariables) {
        const DataColumn *column = FindDataColumnInDataFrame(df, condition.variableId);
        PreparedCondition prepared;
        if (!column || !PrepareCondition(condition, *column, df.rows, prepared, error)) return false;
        conditions.push_back(std::move(prepared));
    }

    std::vector<std::vector<std::string>> panelIds;
    std::vector<std::vector<std::string>> panelLabels;
    std::vector<std::string> currentIds(conditions.size());
    std::vector<std::string> currentLabels(conditions.size());
    BuildPanelCombinationsRecursive(conditions, 0, currentIds, currentLabels, panelIds, panelLabels);
    if (panelIds.empty() || panelIds.size() > 60) {
        if (error) *error = "The conditioning variables produce too many panels (maximum 60).";
        return false;
    }

    std::vector<std::string> categories;
    std::vector<std::vector<std::string>> categoryLevels;
    std::map<int, std::string> boxplotCategoryByRow;
    if (specification.plotType == TrellisPlotType::Boxplot ||
        specification.plotType == TrellisPlotType::Bar) {
        std::vector<std::size_t> rows;
        rows.reserve(xColumn->values.size());
        for (std::size_t row = 0; row < xColumn->values.size(); ++row) {
            if (!DataCellIsMissing(DisplayValueForCell(*xColumn, row))) rows.push_back(row);
        }
        categories = OrderedObservedLevels(*xColumn, rows, xColumn->definedLevels);
        if (specification.plotType == TrellisPlotType::Boxplot) {
            std::vector<std::string> groups = specification.boxplotGroupingVariableIds;
            if (groups.empty()) groups.push_back(specification.xVariableId);
            PlotModel grouped;
            grouped.kind = "boxplot";
            grouped.yLabel = specification.yVariableId;
            grouped.boxplotGroupingVariables = groups;
            grouped.xLabel = BoxplotGroupingLabel(groups);
            RebuildGroupedBoxplotPointsFromDataFrame(grouped, df);
            categories = OrderedBoxplotCategories(
                grouped.boxplotDefinedCategories,
                BoxplotCasesForModel(grouped), plot.boxplotGroupOrder);
            categoryLevels = BoxplotCategoryLevelsForCategories(grouped, categories);
            for (const BoxplotPoint &point : grouped.boxplotPoints)
                boxplotCategoryByRow[point.row] = point.category;
        }
    }

    plot.points.clear();
    plot.trellisPointPanels.clear();
    plot.trellisPointConditionValues.clear();
    plot.trellisPointCategories.clear();
    plot.trellisPointSplitValues.clear();
    std::size_t rowCount = specification.plotType == TrellisPlotType::DataTable
        ? static_cast<std::size_t>(std::max(0, df.rows)) : xColumn->values.size();
    if (yColumn) rowCount = std::min(rowCount, yColumn->values.size());
    for (const PreparedCondition &condition : conditions) {
        rowCount = std::min(rowCount, condition.rowLevelIds.size());
    }
    for (std::size_t row = 0; row < rowCount; ++row) {
        std::vector<std::string> rowConditionIds;
        std::vector<std::string> rowConditionLabels;
        bool complete = true;
        for (const PreparedCondition &condition : conditions) {
            if (condition.rowLevelIds[row].empty()) { complete = false; break; }
            rowConditionIds.push_back(condition.rowLevelIds[row]);
            rowConditionLabels.push_back(condition.rowLevelLabels[row]);
        }
        if (!complete) continue;
        double x = NAN;
        double y = 0.0;
        std::string category;
        if (specification.plotType == TrellisPlotType::Scatter) {
            if (!ParseDataCellDouble(xColumn->values[row], x) || !std::isfinite(x) ||
                !ParseDataCellDouble(yColumn->values[row], y) || !std::isfinite(y)) continue;
        } else if (specification.plotType == TrellisPlotType::TimeSeries) {
            if (!ParseTimeSeriesCellValue(xColumn->values[row], plot.timeSeriesTimeType, x) ||
                !std::isfinite(x) || !ParseDataCellDouble(yColumn->values[row], y) ||
                !std::isfinite(y)) continue;
        } else if (specification.plotType == TrellisPlotType::Boxplot) {
            const auto categoryForRow = boxplotCategoryByRow.find(static_cast<int>(row + 1));
            if (categoryForRow == boxplotCategoryByRow.end() ||
                !ParseDataCellDouble(yColumn->values[row], y) || !std::isfinite(y)) continue;
            category = categoryForRow->second;
            const auto found = std::find(categories.begin(), categories.end(), category);
            if (found == categories.end()) continue;
            x = static_cast<double>(std::distance(categories.begin(), found));
        } else if (specification.plotType == TrellisPlotType::Bar) {
            category = DisplayValueForCell(*xColumn, row);
            if (DataCellIsMissing(category)) continue;
            const auto found = std::find(categories.begin(), categories.end(), category);
            if (found == categories.end()) continue;
            x = static_cast<double>(std::distance(categories.begin(), found));
        } else if (specification.plotType == TrellisPlotType::Histogram) {
            if (!ParseDataCellDouble(xColumn->values[row], x) || !std::isfinite(x)) continue;
        } else {
            x = static_cast<double>(row + 1);
            y = 0.0;
        }
        plot.points.push_back({x, y, static_cast<int>(row + 1)});
        plot.trellisPointPanels.push_back(PanelKey(rowConditionIds));
        plot.trellisPointConditionValues.push_back(std::move(rowConditionLabels));
        plot.trellisPointCategories.push_back(std::move(category));
        plot.trellisPointSplitValues.push_back(splitColumn && row < splitColumn->values.size()
            ? DisplayValueForCell(*splitColumn, row) : "");
    }
    if (plot.points.empty() && specification.plotType != TrellisPlotType::DataTable) {
        if (error) *error = "No complete observations are available for this trellis specification.";
        return false;
    }

    plot.kind = "trellis_scatterplot";
    plot.trellisPanelLevels.clear();
    plot.trellisPanelLevelValues = panelLabels;
    plot.trellisPanelLabels.clear();
    for (std::size_t panel = 0; panel < panelIds.size(); ++panel) {
        plot.trellisPanelLevels.push_back(PanelKey(panelIds[panel]));
        std::vector<std::string> pieces;
        for (std::size_t dimension = 0; dimension < conditions.size(); ++dimension) {
            pieces.push_back(conditions[dimension].specification.variableLabel + " = " +
                panelLabels[panel][dimension]);
        }
        plot.trellisPanelLabels.push_back(Join(pieces, " · "));
    }
    plot.boxplotCategories = categories;
    plot.boxplotDefinedCategories = categories;
    plot.boxplotCategoryLevels = categoryLevels;
    if (specification.plotType == TrellisPlotType::Boxplot) {
        if (specification.boxplotGroupingVariableIds.empty())
            specification.boxplotGroupingVariableIds = {specification.xVariableId};
        plot.boxplotGroupingVariables = specification.boxplotGroupingVariableIds;
    }
    plot.barplotSplitVariable = specification.splitVariableId;
    plot.timeSeriesGroupVariable = specification.groupingVariableId;
    plot.trellisConditionVariable = specification.conditioningVariables.empty()
        ? "" : specification.conditioningVariables.front().variableId;
    plot.trellisLayoutMode = specification.layoutMode;
    plot.trellisPanelOrder = specification.panelOrder;
    const TrellisDerivedMetadata metadata = DeriveTrellisMetadata(plot);
    plot.xLabel = metadata.xAxisLabel;
    plot.yLabel = metadata.yAxisLabel;
    plot.title = metadata.internalTitle;
    ComputeRanges(&plot);
    return true;
}

bool RebuildTrellisScatterplotFromDataFrame(PlotModel &plot,
                                            const DataFrameModel &df,
                                            std::string *error)
{
    return RebuildTrellisPlotFromDataFrame(plot, df, error);
}

TrellisConditioningVariableKind TrellisConditioningKindForColumn(
    const DataColumn &column)
{
    return VariableTypeIsFactorLike(column.type)
        ? TrellisConditioningVariableKind::Categorical
        : TrellisConditioningVariableKind::ContinuousBinned;
}

bool SynchronizeTrellisConditioningVariableType(
    PlotModel &plot,
    const DataFrameModel &df,
    const std::string &variable,
    bool *changed,
    std::string *error)
{
    if (changed) *changed = false;
    if (plot.kind != "trellis_scatterplot" || variable.empty()) return true;

    InitializeTrellisSpecificationFromLegacy(plot);
    const DataColumn *column = FindDataColumnInDataFrame(df, variable);
    if (!column) {
        if (error) *error = "The conditioning variable is unavailable.";
        return false;
    }

    PlotModel candidate = plot;
    auto condition = std::find_if(
        candidate.trellisSpecification.conditioningVariables.begin(),
        candidate.trellisSpecification.conditioningVariables.end(),
        [&](const TrellisConditioningVariable &item) {
            return item.variableId == variable;
        });
    if (condition == candidate.trellisSpecification.conditioningVariables.end()) {
        // Grouping is separate from panel conditioning.  A categorical type
        // change can make a previously numeric identifier (for example
        // Chick in ChickWeight) a valid time-series grouping variable.  The
        // existing plot must be rebuilt so its series and legend immediately
        // use the new semantic type.
        if (candidate.trellisSpecification.plotType == TrellisPlotType::TimeSeries &&
            candidate.trellisSpecification.groupingVariableId == variable) {
            if (!RebuildTrellisPlotFromDataFrame(candidate, df, error)) return false;
            plot = std::move(candidate);
            if (changed) *changed = true;
        }
        return true;
    }

    const TrellisConditioningVariableKind desired =
        TrellisConditioningKindForColumn(*column);
    bool metadataChanged = condition->kind != desired;
    condition->kind = desired;
    if (desired == TrellisConditioningVariableKind::Categorical) {
        metadataChanged = metadataChanged || condition->binning.has_value();
        condition->binning.reset();
    } else if (!condition->binning) {
        TrellisContinuousBinningSpecification binning;
        binning.method = TrellisContinuousBinningMethod::EqualWidth;
        binning.binCount = 4;
        condition->binning = std::move(binning);
        metadataChanged = true;
    }
    if (!metadataChanged) return true;
    if (!RebuildTrellisPlotFromDataFrame(candidate, df, error)) return false;
    plot = std::move(candidate);
    if (changed) *changed = true;
    return true;
}

bool InvalidateTrellisSmoothCurvesForDataChange(PlotModel &plot)
{
    std::vector<std::pair<SmoothCurveScope, std::string>> enabled;
    const auto remember = [&enabled](const SmoothCurveData &curve) {
        const auto key = std::make_pair(curve.scope, curve.fitMethod);
        if (std::find(enabled.begin(), enabled.end(), key) == enabled.end())
            enabled.push_back(key);
    };
    for (const auto &curve : plot.smoothCurves) remember(curve);
    for (const auto &overlay : plot.overlays) {
        if (overlay.type != "lm" || !overlay.visible) continue;
        SmoothCurveScope scope = SmoothCurveScope::Overall;
        if (overlay.source == "selected") scope = SmoothCurveScope::Selection;
        else if (overlay.source == "color") scope = SmoothCurveScope::ColorGroup;
        else if (overlay.source != "all") continue;
        const auto key = std::make_pair(scope, std::string("lm"));
        if (std::find(enabled.begin(), enabled.end(), key) == enabled.end())
            enabled.push_back(key);
    }

    plot.trellisPanelSmoothCurves.clear();
    plot.smoothCurves.clear();
    for (const auto &entry : enabled)
        plot.smoothCurves.push_back(PendingSmoothCurve(entry.first, entry.second));
    return !enabled.empty();
}

bool AddTrellisConditioningVariable(PlotModel &plot,
                                    const DataFrameModel &df,
                                    const std::string &variable,
                                    TrellisConditioningVariableKind kind,
                                    TrellisContinuousBinningMethod method,
                                    std::string *error,
                                    bool orderedCategories)
{
    InitializeTrellisSpecificationFromLegacy(plot);
    for (const TrellisConditioningVariable &condition : plot.trellisSpecification.conditioningVariables) {
        if (condition.variableId == variable) {
            if (error) *error = "The variable is already used for conditioning.";
            return false;
        }
    }
    TrellisConditioningVariable condition;
    condition.variableId = variable;
    condition.variableLabel = variable;
    condition.kind = kind;
    condition.orderedCategories = orderedCategories;
    const bool hasRows = std::any_of(
        plot.trellisSpecification.conditioningVariables.begin(),
        plot.trellisSpecification.conditioningVariables.end(),
        [](const TrellisConditioningVariable &item) {
            return item.dimension == TrellisDimension::Rows;
        });
    condition.dimension = hasRows ? TrellisDimension::Nested : TrellisDimension::Rows;
    if (kind == TrellisConditioningVariableKind::ContinuousBinned) {
        TrellisContinuousBinningSpecification binning;
        binning.method = method;
        binning.binCount = 4;
        condition.binning = binning;
    }
    PlotModel candidate = plot;
    candidate.trellisSpecification.conditioningVariables.push_back(condition);
    if (!RebuildTrellisPlotFromDataFrame(candidate, df, error)) return false;
    plot = std::move(candidate);
    return true;
}

bool SetTrellisConditioningInterpretation(
    PlotModel &plot,
    const DataFrameModel &df,
    const std::string &variable,
    TrellisConditioningVariableKind kind,
    TrellisContinuousBinningMethod method,
    bool orderedCategories,
    std::string *error)
{
    InitializeTrellisSpecificationFromLegacy(plot);
    PlotModel candidate = plot;
    auto &conditions = candidate.trellisSpecification.conditioningVariables;
    auto found = std::find_if(conditions.begin(), conditions.end(),
        [&](const TrellisConditioningVariable &item) {
            return item.variableId == variable;
        });
    if (found == conditions.end()) {
        if (error) *error = "Conditioning variable not found.";
        return false;
    }
    const auto *column = FindDataColumnInDataFrame(df, variable);
    if (!column) {
        if (error) *error = "Conditioning variable is unavailable.";
        return false;
    }
    if (kind == TrellisConditioningVariableKind::ContinuousBinned &&
        !IsNumericColumn(*column)) {
        if (error) *error = "Only a numeric conditioning variable can be binned.";
        return false;
    }
    found->kind = kind;
    found->orderedCategories = kind == TrellisConditioningVariableKind::Categorical &&
        orderedCategories;
    if (kind == TrellisConditioningVariableKind::ContinuousBinned) {
        auto binning = found->binning.value_or(TrellisContinuousBinningSpecification{});
        binning.method = method;
        binning.binCount = std::max<std::size_t>(2,
            std::min<std::size_t>(6, binning.binCount));
        found->binning = std::move(binning);
    } else {
        found->binning.reset();
    }
    if (!RebuildTrellisPlotFromDataFrame(candidate, df, error)) return false;
    plot = std::move(candidate);
    return true;
}

bool ReplaceTrellisConditioningVariable(PlotModel &plot,
                                        const DataFrameModel &df,
                                        const std::string &currentVariable,
                                        const std::string &replacementVariable,
                                        std::string *error)
{
    InitializeTrellisSpecificationFromLegacy(plot);
    PlotModel candidate = plot;
    auto &conditions = candidate.trellisSpecification.conditioningVariables;
    const auto current = std::find_if(conditions.begin(), conditions.end(),
        [&](const TrellisConditioningVariable &condition) {
            return condition.variableId == currentVariable;
        });
    if (current == conditions.end()) {
        if (error) *error = "The conditioning variable to replace is unavailable.";
        return false;
    }
    current->variableId = replacementVariable;
    current->variableLabel = replacementVariable;
    if (current->kind == TrellisConditioningVariableKind::Categorical) {
        current->binning.reset();
    }
    if (!RebuildTrellisPlotFromDataFrame(candidate, df, error)) return false;
    plot = std::move(candidate);
    return true;
}

bool RemoveTrellisConditioningVariable(PlotModel &plot,
                                       const DataFrameModel &df,
                                       const std::string &variable,
                                       std::string *error)
{
    InitializeTrellisSpecificationFromLegacy(plot);
    PlotModel candidate = plot;
    auto &conditions = candidate.trellisSpecification.conditioningVariables;
    const auto found = std::find_if(conditions.begin(), conditions.end(), [&](const TrellisConditioningVariable &item) {
        return item.variableId == variable;
    });
    if (found == conditions.end()) { if (error) *error = "Conditioning variable not found."; return false; }
    conditions.erase(found);
    if (!RebuildTrellisPlotFromDataFrame(candidate, df, error)) return false;
    plot = std::move(candidate);
    return true;
}

bool MoveTrellisConditioningVariable(PlotModel &plot,
                                     const DataFrameModel &df,
                                     const std::string &variable,
                                     int direction,
                                     std::string *error)
{
    InitializeTrellisSpecificationFromLegacy(plot);
    PlotModel candidate = plot;
    auto &conditions = candidate.trellisSpecification.conditioningVariables;
    const auto found = std::find_if(conditions.begin(), conditions.end(), [&](const TrellisConditioningVariable &item) {
        return item.variableId == variable;
    });
    if (found == conditions.end()) { if (error) *error = "Conditioning variable not found."; return false; }
    const std::size_t index = static_cast<std::size_t>(std::distance(conditions.begin(), found));
    if ((direction < 0 && index == 0) || (direction > 0 && index + 1 >= conditions.size())) return true;
    const std::size_t other = direction < 0 ? index - 1 : index + 1;
    std::swap(conditions[index], conditions[other]);
    if (!RebuildTrellisPlotFromDataFrame(candidate, df, error)) return false;
    plot = std::move(candidate);
    return true;
}

bool ConfigureTrellisContinuousCondition(PlotModel &plot,
                                         const DataFrameModel &df,
                                         const std::string &variable,
                                         TrellisContinuousBinningMethod method,
                                         std::size_t binCount,
                                         std::string *error)
{
    InitializeTrellisSpecificationFromLegacy(plot);
    PlotModel candidate = plot;
    auto &conditions = candidate.trellisSpecification.conditioningVariables;
    const auto found = std::find_if(conditions.begin(), conditions.end(), [&](const TrellisConditioningVariable &item) {
        return item.variableId == variable;
    });
    if (found == conditions.end() || found->kind != TrellisConditioningVariableKind::ContinuousBinned) {
        if (error) *error = "Continuous conditioning variable not found.";
        return false;
    }
    TrellisContinuousBinningSpecification binning = found->binning.value_or(
        TrellisContinuousBinningSpecification{});
    binning.method = method;
    binning.binCount = std::max<std::size_t>(2, std::min<std::size_t>(6, binCount));
    found->binning = binning;
    if (!RebuildTrellisPlotFromDataFrame(candidate, df, error)) return false;
    plot = std::move(candidate);
    return true;
}

bool SetTrellisConditioningDimension(PlotModel &plot,
                                     const DataFrameModel &df,
                                     const std::string &variable,
                                     TrellisDimension dimension,
                                     std::string *error)
{
    InitializeTrellisSpecificationFromLegacy(plot);
    PlotModel candidate = plot;
    auto &conditions = candidate.trellisSpecification.conditioningVariables;
    auto target = std::find_if(conditions.begin(), conditions.end(), [&](const TrellisConditioningVariable &item) {
        return item.variableId == variable;
    });
    if (target == conditions.end()) {
        if (error) *error = "Conditioning variable not found.";
        return false;
    }
    if (dimension != TrellisDimension::Nested) {
        for (TrellisConditioningVariable &condition : conditions) {
            if (&condition != &*target && condition.dimension == dimension) {
                condition.dimension = TrellisDimension::Nested;
            }
        }
    }
    target->dimension = dimension;
    NormalizeConditionDimensions(conditions);
    if (!RebuildTrellisPlotFromDataFrame(candidate, df, error)) return false;
    plot = std::move(candidate);
    return true;
}

bool SwapTrellisRowsAndColumns(PlotModel &plot,
                              const DataFrameModel &df,
                              std::string *error)
{
    InitializeTrellisSpecificationFromLegacy(plot);
    PlotModel candidate = plot;
    for (TrellisConditioningVariable &condition : candidate.trellisSpecification.conditioningVariables) {
        if (condition.dimension == TrellisDimension::Rows) {
            condition.dimension = TrellisDimension::Columns;
        } else if (condition.dimension == TrellisDimension::Columns) {
            condition.dimension = TrellisDimension::Rows;
        }
    }
    NormalizeConditionDimensions(candidate.trellisSpecification.conditioningVariables);
    if (!RebuildTrellisPlotFromDataFrame(candidate, df, error)) return false;
    plot = std::move(candidate);
    return true;
}

static BoxplotLayout TrellisBoxplotLayoutForPanel(const PlotModel &plot,
                                                  const TrellisPanelLayout &panel)
{
    BoxplotLayout layout{panel.plotRect, plot.boxplotCategories,
                         panel.viewport.ymin, panel.viewport.ymax,
                         false, false, 0.0, 34.0};
    layout.categoryLevels = BoxplotCategoryLevelsForCategories(
        plot, plot.boxplotCategories);
    return layout;
}

bool SetPlotColorByVariable(PlotModel &plot,
                            const DataFrameModel &df,
                            const std::string &variable,
                            std::string *error)
{
    if (variable.empty() || variable == ".") {
        plot.colorByVariable.clear();
        plot.colorByRowColors.clear();
        plot.colorByLegendItems.clear();
        plot.colorByLegendRows.clear();
        return true;
    }
    const DataColumn *column = FindDataColumnInDataFrame(df, variable);
    if (!column || !TrellisConditionColumnIsValid(*column, df.rows)) {
        if (error) *error = "A split variable must be categorical or have a manageable number of categories.";
        return false;
    }
    std::vector<std::string> levels;
    auto appendLevel = [&](const std::string &level) {
        if (level.empty() || std::find(levels.begin(), levels.end(), level) != levels.end()) return;
        levels.push_back(level);
    };
    for (const std::string &level : column->definedLevels) appendLevel(level);
    for (std::size_t row = 0; row < column->values.size(); ++row)
        appendLevel(DisplayValueForCell(*column, row));
    const std::vector<std::string> palette = BarplotPaletteOrder();
    if (levels.empty() || palette.empty()) {
        if (error) *error = "The split variable has no displayable categories.";
        return false;
    }
    std::map<std::string, std::string> colors;
    for (std::size_t index = 0; index < levels.size(); ++index)
        colors[levels[index]] = palette[index % palette.size()];
    std::map<int, std::string> rowColors;
    std::map<std::string, std::vector<int>> legendRows;
    const std::size_t rowCount = std::min<std::size_t>(
        column->values.size(), static_cast<std::size_t>(std::max(0, df.rows)));
    for (std::size_t row = 0; row < rowCount; ++row) {
        const auto color = colors.find(DisplayValueForCell(*column, row));
        if (color != colors.end()) {
            const int caseId = static_cast<int>(row + 1);
            rowColors[caseId] = color->second;
            legendRows[DisplayValueForCell(*column, row)].push_back(caseId);
        }
    }
    plot.colorByVariable = variable;
    plot.colorByRowColors = std::move(rowColors);
    plot.colorByLegendRows = std::move(legendRows);
    plot.colorByLegendItems.clear();
    for (const std::string &level : levels)
        plot.colorByLegendItems.push_back({level, colors[level]});
    plot.colorByLegendVisible = true;
    return true;
}

bool SetPlotColorLegendPosition(PlotModel &plot,
                                const std::string &position)
{
    if (position == "top_left") {
        plot.colorByLegendX = 0.08;
        plot.colorByLegendY = 0.10;
    } else if (position == "top_right") {
        plot.colorByLegendX = 0.72;
        plot.colorByLegendY = 0.10;
    } else if (position == "bottom_left") {
        plot.colorByLegendX = 0.08;
        plot.colorByLegendY = 0.68;
    } else if (position == "bottom_right") {
        plot.colorByLegendX = 0.72;
        plot.colorByLegendY = 0.68;
    } else {
        return false;
    }
    plot.colorByLegendVisible = true;
    return true;
}

bool ConvertPlotToTrellisWithCondition(
    PlotModel &plot,
    const DataFrameModel &df,
    const std::string &variable,
    TrellisConditioningVariableKind kind,
    TrellisContinuousBinningMethod method,
    bool orderedCategories,
    std::string *error)
{
    if (plot.kind == "trellis_scatterplot") {
        return AddTrellisConditioningVariable(
            plot, df, variable, kind, method, error, orderedCategories);
    }

    PlotModel candidate = plot;
    TrellisPlotSpecification specification;
    if (plot.kind == "scatter") {
        specification.plotType = TrellisPlotType::Scatter;
        specification.xVariableId = plot.xLabel;
        specification.yVariableId = plot.yLabel;
        specification.splitVariableId = plot.colorByVariable;
    } else if (plot.kind == "time_series") {
        specification.plotType = TrellisPlotType::TimeSeries;
        specification.xVariableId = plot.xLabel;
        specification.yVariableId = plot.yLabel;
        specification.groupingVariableId = plot.timeSeriesGroupVariable;
        specification.connectObservations = true;
    } else if (plot.kind == "histogram") {
        specification.plotType = TrellisPlotType::Histogram;
        specification.xVariableId = plot.xLabel;
        specification.splitVariableId = plot.colorByVariable;
        specification.histogramBinCount = plot.histogramBins.empty()
            ? 10 : plot.histogramBins.size();
    } else if (plot.kind == "barplot") {
        const std::vector<std::string> xVariables = BarplotXVariablesForModel(&plot);
        if (xVariables.size() != 1) {
            if (error) *error = "Conditioning a bar chart currently requires exactly one X variable.";
            return false;
        }
        specification.plotType = TrellisPlotType::Bar;
        specification.xVariableId = xVariables.front();
        specification.splitVariableId = plot.barplotSplitVariable;
        specification.barMeasure = plot.barplotMode == "conditional_percent"
            ? "conditional_percent"
            : (plot.barplotMode == "overall_percent" ? "percent" : "count");
    } else if (plot.kind == "boxplot" && !BoxplotUsesVariableAxes(plot)) {
        std::vector<std::string> groups = plot.boxplotGroupingVariables;
        if (groups.empty() && !plot.xLabel.empty()) groups.push_back(plot.xLabel);
        if (groups.empty()) {
            if (error) *error = "A grouped boxplot needs at least one grouping variable before it can be conditioned.";
            return false;
        }
        specification.plotType = TrellisPlotType::Boxplot;
        specification.xVariableId = groups.front();
        specification.yVariableId = plot.yLabel;
        specification.boxplotGroupingVariableIds = std::move(groups);
    } else {
        if (error) *error = "This plot type cannot be converted to a trellis plot.";
        return false;
    }

    specification.layoutMode = plot.trellisLayoutMode;
    specification.panelOrder = plot.trellisPanelOrder;
    candidate.kind = "trellis_scatterplot";
    candidate.trellisSpecification = std::move(specification);
    candidate.trellisSpecificationInitialized = true;
    candidate.trellisConditionVariable.clear();
    if (!AddTrellisConditioningVariable(
            candidate, df, variable, kind, method, error, orderedCategories)) return false;
    plot = std::move(candidate);
    return true;
}

TrellisScatterplotLayout BuildTrellisScatterplotLayout(const PlotModel &plot,
                                                        const Rect &bounds)
{
    TrellisScatterplotLayout layout;
    const std::size_t panelCount = std::max<std::size_t>(1, plot.trellisPanelLevels.size());
    layout.viewport = CommonViewport(plot);
    const std::vector<BoxplotAxisTick> yPreview =
        BoxplotYAxisTicks(layout.viewport.ymin, layout.viewport.ymax, 6);
    std::size_t maximumYLabelLength = 0;
    for (const BoxplotAxisTick &tick : yPreview) {
        maximumYLabelLength = std::max(maximumYLabelLength, tick.label.size());
    }
    const bool semanticMatrix = plot.trellisSpecificationInitialized &&
        plot.trellisSpecification.conditioningVariables.size() > 1 &&
        !plot.trellisPanelLevelValues.empty();
    const double rowStripWidth = semanticMatrix ? 26.0 : 0.0;
    const double yTickLabelWidth = std::max(18.0, maximumYLabelLength * 5.7);
    const double rowStripGap = semanticMatrix ? 6.0 : 0.0;
    // Row-conditioning strips and the shared Y scale occupy opposite sides.
    // Their placement is model state so every platform can expose the same choice.
    const double rowSideWidth = 32.0 + rowStripWidth + rowStripGap;
    // Reserve room for both the tick text and the rotated variable name.  The
    // old gutter left the Y title pressed against the window edge at common
    // Windows display scales.
    const double yAxisSideWidth = std::max(70.0, 33.0 + yTickLabelWidth);
    const double outerLeft = plot.trellisRowStripsOnLeft ? rowSideWidth : yAxisSideWidth;
    const double outerRight = plot.trellisRowStripsOnLeft ? yAxisSideWidth : rowSideWidth;
    const bool showBarSplitLegend = plot.trellisSpecificationInitialized &&
        plot.trellisSpecification.plotType == TrellisPlotType::Bar &&
        !plot.trellisSpecification.splitVariableId.empty();
    const double outerTop = showBarSplitLegend ? 62.0 : 40.0;
    std::size_t boxplotGroupingDepth = 0;
    if (plot.trellisSpecificationInitialized &&
        plot.trellisSpecification.plotType == TrellisPlotType::Boxplot)
        for (const auto &levels : plot.boxplotCategoryLevels)
            boxplotGroupingDepth = std::max(boxplotGroupingDepth, levels.size());
    const double hierarchyHeight = boxplotGroupingDepth > 1
        ? 18.0 * static_cast<double>(boxplotGroupingDepth - 1) : 0.0;
    const double outerBottom = 42.0 + hierarchyHeight;
    const double gapX = 16.0;
    const double gapY = 16.0 + hierarchyHeight;
    const double headerHeight = 22.0;
    const double availableWidth = std::max(10.0, bounds.width - outerLeft - outerRight);
    const double availableHeight = std::max(10.0, bounds.height - outerTop - outerBottom);
    std::vector<std::size_t> rowDimensions;
    std::vector<std::size_t> columnDimensions;
    std::vector<std::size_t> nestedDimensions;
    if (plot.trellisSpecificationInitialized) {
        const auto &conditions = plot.trellisSpecification.conditioningVariables;
        for (std::size_t index = 0; index < conditions.size(); ++index) {
            if (conditions[index].dimension == TrellisDimension::Rows) rowDimensions.push_back(index);
            else if (conditions[index].dimension == TrellisDimension::Columns) columnDimensions.push_back(index);
            else nestedDimensions.push_back(index);
        }
    }
    auto levelsForDimensions = [&](const std::vector<std::size_t> &dimensions) {
        std::vector<std::vector<std::string>> result(dimensions.size());
        for (const std::vector<std::string> &values : plot.trellisPanelLevelValues) {
            for (std::size_t slot = 0; slot < dimensions.size(); ++slot) {
                const std::size_t dimension = dimensions[slot];
                if (dimension >= values.size()) continue;
                if (std::find(result[slot].begin(), result[slot].end(), values[dimension]) == result[slot].end()) {
                    result[slot].push_back(values[dimension]);
                }
            }
        }
        return result;
    };
    const auto rowLevels = levelsForDimensions(rowDimensions);
    const auto columnLevels = levelsForDimensions(columnDimensions);
    const auto nestedLevels = levelsForDimensions(nestedDimensions);
    auto product = [](const std::vector<std::vector<std::string>> &levels) {
        std::size_t value = 1;
        for (const auto &dimension : levels) value *= std::max<std::size_t>(1, dimension.size());
        return value;
    };
    const std::size_t semanticColumns = semanticMatrix ? product(columnLevels) : 0;
    const std::size_t semanticRowsPerBlock = semanticMatrix ? product(rowLevels) : 0;
    const std::size_t nestedBlocks = semanticMatrix ? product(nestedLevels) : 1;
    const std::size_t requestedColumns = ManualColumns(plot.trellisLayoutMode, panelCount);
    layout.columns = semanticColumns > 0 ? semanticColumns : (requestedColumns > 0
        ? requestedColumns
        : AutomaticColumns(panelCount, availableWidth, availableHeight));
    layout.columns = std::max<std::size_t>(1, std::min(layout.columns, panelCount));
    layout.rows = semanticMatrix
        ? std::max<std::size_t>(1, semanticRowsPerBlock * nestedBlocks)
        : (panelCount + layout.columns - 1) / layout.columns;
    const double cellWidth = std::max(1.0,
        (availableWidth - gapX * static_cast<double>(layout.columns - 1)) /
        static_cast<double>(layout.columns));
    double cellHeight = std::max(1.0,
        (availableHeight - gapY * static_cast<double>(layout.rows - 1)) /
        static_cast<double>(layout.rows));
    const double maximumCellHeight = cellWidth / 1.05 + headerHeight;
    cellHeight = std::min(cellHeight, maximumCellHeight);
    const double gridHeight = cellHeight * static_cast<double>(layout.rows) +
        gapY * static_cast<double>(layout.rows - 1);
    const double gridTop = bounds.y + outerTop + std::max(0.0, (availableHeight - gridHeight) / 2.0);
    std::vector<std::string> visualLevels = OrderedTrellisPanelLevels(plot);
    if (semanticMatrix) {
        auto coordinateFor = [&](std::size_t panelIndex,
                                 const std::vector<std::size_t> &dimensions,
                                 const std::vector<std::vector<std::string>> &levels) {
            std::size_t coordinate = 0;
            std::size_t stride = 1;
            const std::vector<std::string> &values = plot.trellisPanelLevelValues[panelIndex];
            for (std::size_t slot = 0; slot < dimensions.size(); ++slot) {
                const std::size_t dimension = dimensions[slot];
                const auto found = dimension < values.size()
                    ? std::find(levels[slot].begin(), levels[slot].end(), values[dimension])
                    : levels[slot].end();
                const std::size_t level = found == levels[slot].end()
                    ? 0 : static_cast<std::size_t>(std::distance(levels[slot].begin(), found));
                coordinate += level * stride;
                stride *= std::max<std::size_t>(1, levels[slot].size());
            }
            return coordinate;
        };
        std::stable_sort(visualLevels.begin(), visualLevels.end(), [&](const std::string &left,
                                                                      const std::string &right) {
            const auto leftIt = std::find(plot.trellisPanelLevels.begin(), plot.trellisPanelLevels.end(), left);
            const auto rightIt = std::find(plot.trellisPanelLevels.begin(), plot.trellisPanelLevels.end(), right);
            const std::size_t li = static_cast<std::size_t>(std::distance(plot.trellisPanelLevels.begin(), leftIt));
            const std::size_t ri = static_cast<std::size_t>(std::distance(plot.trellisPanelLevels.begin(), rightIt));
            const std::size_t ln = coordinateFor(li, nestedDimensions, nestedLevels);
            const std::size_t rn = coordinateFor(ri, nestedDimensions, nestedLevels);
            if (ln != rn) return ln < rn;
            const std::size_t lr = coordinateFor(li, rowDimensions, rowLevels);
            const std::size_t rr = coordinateFor(ri, rowDimensions, rowLevels);
            if (lr != rr) return lr < rr;
            return coordinateFor(li, columnDimensions, columnLevels) <
                coordinateFor(ri, columnDimensions, columnLevels);
        });
    }

    layout.panels.reserve(panelCount);
    for (std::size_t index = 0; index < panelCount; ++index) {
        const std::size_t column = index % layout.columns;
        const std::size_t row = index / layout.columns;
        const std::size_t rowStart = row * layout.columns;
        const std::size_t rowCount = std::min(layout.columns, panelCount - rowStart);
        const double rowWidth = cellWidth * static_cast<double>(rowCount) +
            gapX * static_cast<double>(rowCount - 1);
        const double rowLeft = bounds.x + outerLeft + (availableWidth - rowWidth) / 2.0;
        Rect frame{rowLeft + column * (cellWidth + gapX),
                   gridTop + row * (cellHeight + gapY),
                   cellWidth, cellHeight};
        Rect headerRect{frame.x, frame.y, frame.width, std::min(headerHeight, frame.height)};
        Rect plotRect{frame.x, frame.y + headerRect.height,
                      frame.width, std::max(1.0, frame.height - headerRect.height)};
        TrellisPanelLayout panel;
        panel.levelId = index < visualLevels.size() ? visualLevels[index] : "";
        const auto original = std::find(plot.trellisPanelLevels.begin(),
                                        plot.trellisPanelLevels.end(), panel.levelId);
        const std::size_t originalIndex = original == plot.trellisPanelLevels.end()
            ? index : static_cast<std::size_t>(std::distance(plot.trellisPanelLevels.begin(), original));
        panel.label = originalIndex < plot.trellisPanelLabels.size()
            ? plot.trellisPanelLabels[originalIndex]
            : panel.levelId;
        if (originalIndex < plot.trellisPanelLevelValues.size()) {
            panel.conditionValues = plot.trellisPanelLevelValues[originalIndex];
        }
        panel.panelId = plot.trellisSpecificationInitialized &&
            plot.trellisSpecification.conditioningVariables.size() > 1
            ? panel.levelId
            : (plot.trellisSpecification.conditioningVariables.empty()
                ? "all" : plot.trellisConditionVariable + "=" + panel.levelId);
        panel.row = row;
        panel.column = column;
        panel.frame = frame;
        panel.headerRect = headerRect;
        panel.columnStripRect = headerRect;
        panel.plotRect = plotRect;
        panel.showXTickLabels = row + 1 == layout.rows;
        panel.showYTickLabels = plot.trellisRowStripsOnLeft
            ? column + 1 == layout.columns
            : column == 0;
        if (plot.trellisSpecificationInitialized &&
            plot.trellisSpecification.plotType == TrellisPlotType::DataTable) {
            panel.showXTickLabels = false;
            panel.showYTickLabels = false;
        }
        const int xTarget = plotRect.width < 210.0 ? 4 : 6;
        const int yTarget = plotRect.height < 150.0 ? 4 : 6;
        const Rect xScaleRect = plot.trellisSpecificationInitialized &&
            plot.trellisSpecification.plotType == TrellisPlotType::Histogram
                ? ZeroBaselineContentRect(plotRect) : plotRect;
        panel.xTicks = BuildTrellisAxisTicks(
            layout.viewport.xmin, layout.viewport.xmax, xScaleRect, true, xTarget);
        if (plot.trellisSpecificationInitialized &&
            (plot.trellisSpecification.plotType == TrellisPlotType::Boxplot ||
             plot.trellisSpecification.plotType == TrellisPlotType::Bar)) {
            panel.xTicks.clear();
            const BoxplotLayout boxplotLayout = TrellisBoxplotLayoutForPanel(plot, panel);
            BarplotLayout barTickLayout;
            barTickLayout.plotRect = plotRect;
            barTickLayout.values.assign(plot.boxplotCategories.size(), 1.0);
            const std::vector<Rect> barTickRects = BarplotBarRects(barTickLayout);
            for (std::size_t category = 0; category < plot.boxplotCategories.size(); ++category) {
                const std::string &name = plot.boxplotCategories[category];
                const double position = plot.trellisSpecification.plotType == TrellisPlotType::Boxplot
                    ? BoxplotCategoryCenter(boxplotLayout, name)
                    : barTickRects[category].x + barTickRects[category].width / 2.0;
                panel.xTicks.push_back({static_cast<double>(category), position,
                    plot.trellisSpecification.plotType == TrellisPlotType::Boxplot
                        ? BoxplotInnermostCategoryLabel(boxplotLayout, name) : name});
            }
        }
        panel.yTicks = BuildTrellisAxisTicks(
            layout.viewport.ymin, layout.viewport.ymax, plotRect, false, yTarget);
        if (plot.trellisSpecificationInitialized &&
            (plot.trellisSpecification.plotType == TrellisPlotType::Bar ||
             plot.trellisSpecification.plotType == TrellisPlotType::Histogram)) {
            PlaceZeroBaselineTicks(panel.yTicks, plotRect,
                layout.viewport.ymin, layout.viewport.ymax);
        }
        if (plot.trellisSpecificationInitialized &&
            plot.trellisSpecification.plotType == TrellisPlotType::DataTable) {
            panel.xTicks.clear();
            panel.yTicks.clear();
        }
        if (plot.trellisSpecificationInitialized && !panel.conditionValues.empty()) {
            const auto &conditions = plot.trellisSpecification.conditioningVariables;
            std::vector<std::string> columnLabels;
            std::vector<std::string> rowLabels;
            std::vector<std::string> nestedLabels;
            for (std::size_t dimension = 0; dimension < conditions.size() && dimension < panel.conditionValues.size(); ++dimension) {
                const std::string label = conditions[dimension].variableLabel + " = " + panel.conditionValues[dimension];
                if (conditions[dimension].dimension == TrellisDimension::Rows) rowLabels.push_back(label);
                else if (conditions[dimension].dimension == TrellisDimension::Columns) columnLabels.push_back(label);
                else nestedLabels.push_back(label);
            }
            panel.columnStripLabel = Join(columnLabels, " · ");
            panel.rowStripLabel = Join(rowLabels, " · ");
            if (!nestedLabels.empty()) panel.label = Join(nestedLabels, " · ");
            else if (conditions.size() > 1) panel.label.clear();
        }
        const bool rowStripEdgePanel = plot.trellisRowStripsOnLeft
            ? column == 0
            : column + 1 == layout.columns;
        if (semanticMatrix && rowStripEdgePanel) {
            panel.rowStripRect = {
                plot.trellisRowStripsOnLeft
                    ? frame.x - rowStripGap - rowStripWidth
                    : frame.x + frame.width + rowStripGap,
                plotRect.y,
                rowStripWidth, plotRect.height};
        }
        panel.viewport = layout.viewport;
        panel.context.outerRect = frame;
        panel.context.rowStripRect = panel.rowStripRect;
        panel.context.columnStripRect = panel.columnStripRect;
        panel.context.nestedStripRect = panel.nestedStripRect;
        panel.context.plotRect = plotRect;
        panel.context.plotClip = plotRect;
        panel.context.viewport = panel.viewport;
        panel.context.drawXTickLabels = panel.showXTickLabels;
        panel.context.drawYTickLabels = panel.showYTickLabels;
        layout.panels.push_back(std::move(panel));
    }
    layout.panelsBoundingRect = BoundingRect(layout.panels);
    layout.titleRect = {layout.panelsBoundingRect.x, bounds.y + 9.0,
                        layout.panelsBoundingRect.width, 22.0};
    layout.xAxisLabelAnchor = {
        layout.panelsBoundingRect.x + layout.panelsBoundingRect.width / 2.0,
        std::min(bounds.y + bounds.height - 18.0,
                 layout.panelsBoundingRect.y + layout.panelsBoundingRect.height + 28.0)};
    layout.yAxisLabelAnchor = {
        plot.trellisRowStripsOnLeft
            ? layout.panelsBoundingRect.x + layout.panelsBoundingRect.width +
                  yTickLabelWidth + 13.0
            : layout.panelsBoundingRect.x - yTickLabelWidth - 13.0,
        layout.panelsBoundingRect.y + layout.panelsBoundingRect.height / 2.0};

    std::map<std::string, std::size_t> panelIndices;
    for (std::size_t index = 0; index < visualLevels.size(); ++index) {
        panelIndices[visualLevels[index]] = index;
    }
    const std::string scaleMode = plot.trellisSpecificationInitialized
        ? plot.trellisSpecification.scaleMode : "common_xy";
    const bool freeX = scaleMode == "free_x" || scaleMode == "free_xy";
    const bool freeY = scaleMode == "free_y" || scaleMode == "free_xy";
    for (const auto &panelEntry : panelIndices) {
        if (panelEntry.second >= layout.panels.size()) continue;
        PlotModel subset = plot;
        subset.points.clear();
        subset.trellisPointPanels.clear();
        subset.trellisPointCategories.clear();
        for (std::size_t pointIndex = 0; pointIndex < plot.points.size(); ++pointIndex) {
            if (pointIndex >= plot.trellisPointPanels.size() ||
                plot.trellisPointPanels[pointIndex] != panelEntry.first) continue;
            subset.points.push_back(plot.points[pointIndex]);
            subset.trellisPointPanels.push_back(panelEntry.first);
            subset.trellisPointCategories.push_back(pointIndex < plot.trellisPointCategories.size()
                ? plot.trellisPointCategories[pointIndex] : "");
        }
        DataViewport local = subset.points.empty() ? layout.viewport : CommonViewport(subset);
        if (!freeX) { local.xmin = layout.viewport.xmin; local.xmax = layout.viewport.xmax; }
        if (!freeY) { local.ymin = layout.viewport.ymin; local.ymax = layout.viewport.ymax; }
        TrellisPanelLayout &panel = layout.panels[panelEntry.second];
        panel.viewport = local;
        panel.context.viewport = local;
        const int xTarget = panel.plotRect.width < 210.0 ? 4 : 6;
        const int yTarget = panel.plotRect.height < 150.0 ? 4 : 6;
        const Rect xScaleRect = plot.trellisSpecificationInitialized &&
            plot.trellisSpecification.plotType == TrellisPlotType::Histogram
                ? ZeroBaselineContentRect(panel.plotRect) : panel.plotRect;
        panel.xTicks = BuildTrellisAxisTicks(local.xmin, local.xmax, xScaleRect, true, xTarget);
        if (plot.trellisSpecificationInitialized &&
            plot.trellisSpecification.plotType == TrellisPlotType::TimeSeries &&
            plot.timeSeriesTimeType != "numeric") {
            panel.xTicks.clear();
            for (const TimeSeriesAxisTick &tick : BuildTimeSeriesAxisTicks(
                     local.xmin, local.xmax, plot.timeSeriesTimeType, xTarget)) {
                const Point position = DataToScreen({tick.value, local.ymin},
                    local, panel.plotRect, true);
                panel.xTicks.push_back({tick.value, position.x, tick.label});
            }
        }
        if (plot.trellisSpecificationInitialized &&
            (plot.trellisSpecification.plotType == TrellisPlotType::Boxplot ||
             plot.trellisSpecification.plotType == TrellisPlotType::Bar)) {
            panel.xTicks.clear();
            const BoxplotLayout boxplotLayout = TrellisBoxplotLayoutForPanel(plot, panel);
            BarplotLayout barTickLayout;
            barTickLayout.plotRect = panel.plotRect;
            barTickLayout.values.assign(plot.boxplotCategories.size(), 1.0);
            const std::vector<Rect> barTickRects = BarplotBarRects(barTickLayout);
            for (std::size_t category = 0; category < plot.boxplotCategories.size(); ++category) {
                const std::string &name = plot.boxplotCategories[category];
                const double position = plot.trellisSpecification.plotType == TrellisPlotType::Boxplot
                    ? BoxplotCategoryCenter(boxplotLayout, name)
                    : barTickRects[category].x + barTickRects[category].width / 2.0;
                panel.xTicks.push_back({static_cast<double>(category), position,
                    plot.trellisSpecification.plotType == TrellisPlotType::Boxplot
                        ? BoxplotInnermostCategoryLabel(boxplotLayout, name) : name});
            }
        }
        panel.yTicks = BuildTrellisAxisTicks(local.ymin, local.ymax, panel.plotRect, false, yTarget);
        if (plot.trellisSpecificationInitialized &&
            (plot.trellisSpecification.plotType == TrellisPlotType::Bar ||
             plot.trellisSpecification.plotType == TrellisPlotType::Histogram)) {
            PlaceZeroBaselineTicks(panel.yTicks, panel.plotRect,
                local.ymin, local.ymax);
        }
        if (plot.trellisSpecificationInitialized &&
            plot.trellisSpecification.plotType == TrellisPlotType::DataTable) {
            panel.xTicks.clear();
            panel.yTicks.clear();
        }
        panel.context.xAxis.displayDomain = {local.xmin, local.xmax};
        panel.context.xAxis.dataDomain = {subset.dataXmin, subset.dataXmax};
        panel.context.xAxis.ticks = panel.xTicks;
        panel.context.yAxis.displayDomain = {local.ymin, local.ymax};
        panel.context.yAxis.dataDomain = {subset.dataYmin, subset.dataYmax};
        panel.context.yAxis.ticks = panel.yTicks;
    }
    for (std::size_t index = 0; index < plot.points.size(); ++index) {
        const std::string panel = index < plot.trellisPointPanels.size() ? plot.trellisPointPanels[index] : "";
        const auto found = panelIndices.find(panel);
        if (found == panelIndices.end() || found->second >= layout.panels.size()) continue;
        const std::size_t panelIndex = found->second;
        const DataPoint &source = plot.points[index];
        const Point data{source.x, source.y};
        layout.panels[panelIndex].hasObservations = true;
        layout.panels[panelIndex].context.originalRowIndices.push_back(source.row);
        if (!plot.trellisSpecificationInitialized ||
            plot.trellisSpecification.plotType == TrellisPlotType::Scatter ||
            plot.trellisSpecification.plotType == TrellisPlotType::TimeSeries) {
            const Point screen = DataToScreen(data, layout.panels[panelIndex].viewport,
                                              layout.panels[panelIndex].plotRect, true);
            layout.cases.push_back({source.row, panelIndex, data, screen});
        }
    }
    if (plot.trellisSpecificationInitialized &&
        plot.trellisSpecification.plotType == TrellisPlotType::Bar) {
        for (const auto &panelEntry : panelIndices) {
            const std::size_t panelIndex = panelEntry.second;
            std::map<std::string, std::vector<CaseId>> rowsByCategory;
            int panelTotal = 0;
            for (std::size_t index = 0; index < plot.points.size(); ++index) {
                if (index >= plot.trellisPointPanels.size() || index >= plot.trellisPointCategories.size() ||
                    plot.trellisPointPanels[index] != panelEntry.first) continue;
                rowsByCategory[plot.trellisPointCategories[index]].push_back(plot.points[index].row);
                ++panelTotal;
            }
            std::vector<BarplotBinSummary> bins;
            bins.reserve(plot.boxplotCategories.size());
            for (std::size_t category = 0; category < plot.boxplotCategories.size(); ++category) {
                const std::string &label = plot.boxplotCategories[category];
                BarplotBinSummary bin;
                bin.category = label;
                bin.rows = rowsByCategory[label];
                bin.n = static_cast<int>(bin.rows.size());
                bin.percent = panelTotal > 0
                    ? 100.0 * static_cast<double>(bin.n) / static_cast<double>(panelTotal)
                    : 0.0;
                bins.push_back(std::move(bin));
            }
            const BarplotLayout barplotLayout = BuildBarplotLayoutForPlotRect(
                layout.panels[panelIndex].plotRect,
                {plot.trellisSpecification.xVariableId},
                bins,
                plot.trellisSpecification.barMeasure == "percent" ? "overall_percent" :
                    (plot.trellisSpecification.barMeasure == "conditional_percent"
                        ? "conditional_percent" : "count"),
                "equal");
            const std::vector<Rect> barRects = BarplotBarRects(barplotLayout);
            for (std::size_t category = 0;
                 category < bins.size() && category < barRects.size(); ++category) {
                TrellisAggregateGeometry aggregate;
                aggregate.kind = TrellisAggregateKind::Bar;
                aggregate.panelIndex = panelIndex;
                aggregate.id = panelEntry.first + "|" + bins[category].category;
                aggregate.label = bins[category].category;
                aggregate.rect = barRects[category];
                aggregate.caseIds = bins[category].rows;
                aggregate.value = category < barplotLayout.values.size()
                    ? barplotLayout.values[category] : 0.0;
                layout.aggregates.push_back(std::move(aggregate));
            }
        }
    } else if (plot.trellisSpecificationInitialized &&
               plot.trellisSpecification.plotType == TrellisPlotType::Histogram) {
        const std::size_t binCount = std::max<std::size_t>(1, plot.trellisSpecification.histogramBinCount);
        std::vector<HistogramCaseValue> allCases;
        allCases.reserve(plot.points.size());
        for (const DataPoint &point : plot.points) allCases.push_back({point.x, point.row});
        HistogramBinningResult commonBinning;
        const bool haveCommonBinning = RebinHistogramCases(allCases, static_cast<int>(binCount), commonBinning);
        for (const auto &panelEntry : panelIndices) {
            const std::size_t panelIndex = panelEntry.second;
            std::vector<HistogramCaseValue> panelCases;
            for (std::size_t index = 0; index < plot.points.size(); ++index) {
                if (index >= plot.trellisPointPanels.size() || plot.trellisPointPanels[index] != panelEntry.first) continue;
                panelCases.push_back({plot.points[index].x, plot.points[index].row});
            }
            HistogramBinningResult panelBinning;
            if (freeX) RebinHistogramCases(panelCases, static_cast<int>(binCount), panelBinning);
            const HistogramBinningResult &binning = freeX ? panelBinning : commonBinning;
            if ((!freeX && !haveCommonBinning) || binning.bins.empty()) continue;
            std::vector<std::vector<CaseId>> rows(binning.bins.size());
            for (const HistogramCaseValue &item : panelCases) {
                std::size_t bin = binning.bins.size() - 1;
                for (std::size_t candidate = 0; candidate < binning.bins.size(); ++candidate) {
                    const bool last = candidate + 1 == binning.bins.size();
                    if (item.x >= binning.bins[candidate].lower &&
                        (item.x < binning.bins[candidate].upper ||
                         (last && item.x <= binning.bins[candidate].upper))) {
                        bin = candidate;
                        break;
                    }
                }
                rows[bin].push_back(item.row);
            }
            std::size_t total = 0;
            for (const auto &binRows : rows) total += binRows.size();
            const double averageWidth = HistogramAverageBinWidth(binning.bins);
            for (std::size_t bin = 0; bin < binning.bins.size(); ++bin) {
                double value = static_cast<double>(rows[bin].size());
                if (plot.trellisSpecification.histogramMeasure == "percent" && total > 0) {
                    value = 100.0 * value / static_cast<double>(total);
                } else if (plot.trellisSpecification.histogramMeasure == "density" && total > 0) {
                    value /= static_cast<double>(total) * averageWidth;
                }
                const Rect &panelRect = layout.panels[panelIndex].plotRect;
                const Rect contentRect = ZeroBaselineContentRect(panelRect);
                const double slot = contentRect.width /
                    static_cast<double>(binning.bins.size());
                const double baseline = ZeroBaselineY(panelRect, 0.0);
                const double height = contentRect.height * value /
                    std::max(1.0e-12, layout.panels[panelIndex].viewport.ymax);
                Rect rawRect{contentRect.x + slot * static_cast<double>(bin) + 1.0,
                             baseline - height, std::max(1.0, slot - 2.0), height};
                Rect clipped = IntersectionRect(rawRect, layout.panels[panelIndex].plotRect);
                if (!IsValidRect(clipped)) {
                    const double left = std::max(panelRect.x, rawRect.x);
                    const double right = std::min(panelRect.x + panelRect.width,
                                                  rawRect.x + rawRect.width);
                    clipped = {left, panelRect.y + panelRect.height,
                               std::max(0.0, right - left), 0.0};
                }
                const double lower = binning.bins[bin].lower;
                const double upper = binning.bins[bin].upper;
                TrellisAggregateGeometry aggregate;
                aggregate.kind = TrellisAggregateKind::HistogramBin;
                aggregate.panelIndex = panelIndex;
                aggregate.id = panelEntry.first + "|bin" + std::to_string(bin);
                aggregate.label = IntervalLabel(lower, upper, bin + 1 == binning.bins.size());
                aggregate.rect = clipped;
                aggregate.caseIds = rows[bin];
                aggregate.value = value;
                aggregate.lower = lower;
                aggregate.upper = upper;
                layout.aggregates.push_back(std::move(aggregate));
            }
        }
    } else if (plot.trellisSpecificationInitialized &&
               plot.trellisSpecification.plotType == TrellisPlotType::Boxplot) {
        for (const auto &panelEntry : panelIndices) {
            const std::size_t panelIndex = panelEntry.second;
            std::vector<BoxplotCase> panelCases;
            for (std::size_t index = 0; index < plot.points.size(); ++index) {
                if (index >= plot.trellisPointPanels.size() ||
                    plot.trellisPointPanels[index] != panelEntry.first) continue;
                panelCases.push_back({
                    plot.points[index].row,
                    plot.points[index].y,
                    index < plot.trellisPointCategories.size()
                        ? plot.trellisPointCategories[index] : ""
                });
            }
            if (plot.boxplotShowPoints) {
                const BoxplotLayout boxplotLayout =
                    TrellisBoxplotLayoutForPanel(plot, layout.panels[panelIndex]);
                for (const BoxplotCaseGeometry &entry :
                     BoxplotCaseGeometryForLayout(boxplotLayout, panelCases)) {
                    const auto category = std::find(
                        plot.boxplotCategories.begin(), plot.boxplotCategories.end(), entry.category);
                    if (category == plot.boxplotCategories.end()) continue;
                    layout.cases.push_back({
                        entry.caseId,
                        panelIndex,
                        {static_cast<double>(std::distance(plot.boxplotCategories.begin(), category)),
                         entry.value},
                        entry.point
                    });
                }
            }
            for (std::size_t category = 0; category < plot.boxplotCategories.size(); ++category) {
                std::vector<double> values;
                std::vector<CaseId> rows;
                for (std::size_t index = 0; index < plot.points.size(); ++index) {
                    if (index >= plot.trellisPointPanels.size() || index >= plot.trellisPointCategories.size() ||
                        plot.trellisPointPanels[index] != panelEntry.first ||
                        plot.trellisPointCategories[index] != plot.boxplotCategories[category]) continue;
                    values.push_back(plot.points[index].y);
                    rows.push_back(plot.points[index].row);
                }
                if (values.empty()) continue;
                const double q1 = Quantile(values, 0.25);
                const double median = Quantile(values, 0.5);
                const double q3 = Quantile(values, 0.75);
                const double iqr = q3 - q1;
                double whiskerLow = q1;
                double whiskerHigh = q3;
                for (double value : values) {
                    if (value >= q1 - 1.5 * iqr) whiskerLow = std::min(whiskerLow, value);
                    if (value <= q3 + 1.5 * iqr) whiskerHigh = std::max(whiskerHigh, value);
                }
                const BoxplotLayout boxplotLayout =
                    TrellisBoxplotLayoutForPanel(plot, layout.panels[panelIndex]);
                const auto draw = BuildBoxplotStatsRenderPlan(boxplotLayout,
                    {{plot.boxplotCategories[category], q1, median, q3,
                      whiskerLow, whiskerHigh, static_cast<int>(values.size())}});
                TrellisAggregateGeometry aggregate;
                aggregate.kind = TrellisAggregateKind::Box;
                aggregate.panelIndex = panelIndex;
                aggregate.id = panelEntry.first + "|" + plot.boxplotCategories[category];
                aggregate.label = plot.boxplotCategories[category];
                if (!draw.empty()) aggregate.rect = draw.front().boxRect;
                aggregate.caseIds = rows;
                aggregate.lower = q1;
                aggregate.upper = q3;
                aggregate.median = median;
                aggregate.whiskerLow = whiskerLow;
                aggregate.whiskerHigh = whiskerHigh;
                layout.aggregates.push_back(aggregate);
            }
        }
    }
    return layout;
}

std::vector<TrellisPanelRenderPlan> BuildTrellisPanelRenderPlans(
    const PlotModel &plot,
    const TrellisScatterplotLayout &layout,
    const std::set<CaseId> &selection,
    const std::map<CaseId, std::string> &rowColors,
    const std::map<CaseId, std::string> &rowLabels)
{
    std::vector<TrellisPanelRenderPlan> result;
    result.reserve(layout.panels.size());
    TrellisPlotType type = plot.trellisSpecificationInitialized
        ? plot.trellisSpecification.plotType : TrellisPlotType::Scatter;
    std::vector<ScatterplotOverlaySpec> overlays;
    for (const Overlay &overlay : plot.overlays) {
        overlays.push_back({overlay.type, overlay.source, overlay.visible});
    }
    const BarplotVisualState barVisualState = BarplotVisualStateForModel(plot);
    const std::string seriesOrSplitVariable = type == TrellisPlotType::TimeSeries
        ? plot.trellisSpecification.groupingVariableId
        : plot.trellisSpecification.splitVariableId;
    const std::vector<std::string> splitLevels =
        seriesOrSplitVariable.empty()
            ? std::vector<std::string>{}
            : OrderedUniqueBarplotLevels(plot.trellisPointSplitValues);
    std::map<CaseId, std::string> effectiveRowColors = rowColors;
    if ((type == TrellisPlotType::Scatter || type == TrellisPlotType::TimeSeries ||
         type == TrellisPlotType::Histogram) &&
        !splitLevels.empty()) {
        const std::vector<std::string> palette = BarplotPaletteOrder();
        std::map<std::string, std::string> splitColors;
        for (std::size_t index = 0; index < splitLevels.size(); ++index) {
            if (!palette.empty()) splitColors[splitLevels[index]] = palette[index % palette.size()];
        }
        for (std::size_t index = 0; index < plot.points.size(); ++index) {
            if (index >= plot.trellisPointSplitValues.size() ||
                effectiveRowColors.find(plot.points[index].row) != effectiveRowColors.end()) continue;
            const auto color = splitColors.find(plot.trellisPointSplitValues[index]);
            if (color != splitColors.end()) effectiveRowColors[plot.points[index].row] = color->second;
        }
    }
    for (std::size_t panelIndex = 0; panelIndex < layout.panels.size(); ++panelIndex) {
        const TrellisPanelLayout &panel = layout.panels[panelIndex];
        TrellisPanelRenderPlan plan;
        plan.panelIndex = panelIndex;
        plan.context = panel.context;
        if (type == TrellisPlotType::Scatter || type == TrellisPlotType::TimeSeries) {
            ScatterplotRenderInput input;
            input.shadeOverlap = plot.scatterShadeOverlap;
            input.sizeByOverlap = plot.scatterSizeByOverlap;
            input.viewport = panel.viewport;
            input.plotRect = panel.plotRect;
            input.selectedRows = selection;
            input.rowColors = effectiveRowColors;
            input.rowLabels = rowLabels;
            input.labelDisplayMode = plot.labelDisplayMode;
            if (type == TrellisPlotType::Scatter) {
                input.overlays = overlays;
                const auto panelSmooth = plot.trellisPanelSmoothCurves.find(panel.levelId);
                if (panelSmooth != plot.trellisPanelSmoothCurves.end()) {
                    for (const SmoothCurveData &curve : panelSmooth->second) {
                        if (FitCurveScopeIsPresent(plot.smoothCurves, curve.scope,
                                                   curve.fitMethod)) {
                            input.smoothCurves.push_back(curve);
                        }
                    }
                }
            }
            for (const TrellisCaseGeometry &entry : layout.cases) {
                if (entry.panelIndex == panelIndex) {
                    input.points.push_back({entry.caseId, entry.dataPoint.x, entry.dataPoint.y});
                }
            }
            input.showFitConfidenceIntervals =
                plot.scatterFitConfidenceIntervalsVisible;
            input.showSmoothConfidenceIntervals =
                plot.scatterSmoothConfidenceIntervalsVisible;
            plan.scatterplot = BuildScatterplotRenderPlan(input);
            if (type == TrellisPlotType::TimeSeries) {
                std::vector<DataPoint> panelPoints;
                std::vector<std::string> panelGroups;
                for (std::size_t pointIndex = 0; pointIndex < plot.points.size(); ++pointIndex) {
                    if (pointIndex >= plot.trellisPointPanels.size() ||
                        plot.trellisPointPanels[pointIndex] != panel.levelId) continue;
                    panelPoints.push_back(plot.points[pointIndex]);
                    if (!seriesOrSplitVariable.empty()) {
                        panelGroups.push_back(pointIndex < plot.trellisPointSplitValues.size()
                            ? plot.trellisPointSplitValues[pointIndex] : "");
                    }
                }
                const std::vector<InteractionPlotLine> lines = BuildTimeSeriesLines(
                    panelPoints, panelGroups, plot.trellisSpecification.yVariableId);
                for (const InteractionPlotLine &line : lines) {
                    TrellisTimeSeriesLine renderLine;
                    renderLine.label = line.label;
                    renderLine.colorKey = line.colorKey;
                    for (const DataPoint &point : line.points) {
                        renderLine.points.push_back(DataToScreen(
                            {point.x, point.y}, panel.viewport, panel.plotRect, true));
                        renderLine.caseIds.push_back(point.row);
                    }
                    plan.timeSeriesLines.push_back(std::move(renderLine));
                }
                if (!seriesOrSplitVariable.empty() && plot.timeSeriesIdentification == "legend") {
                    plan.timeSeriesLegend = BuildTimeSeriesLegendLayout(
                        lines.size(), panel.plotRect, plot.timeSeriesLegendPosition);
                } else if (!seriesOrSplitVariable.empty() &&
                           plot.timeSeriesIdentification == "start_labels") {
                    plan.timeSeriesDirectLabels = BuildTimeSeriesDirectLabels(
                        lines, panel.viewport, panel.plotRect);
                }
            }
        } else if (type == TrellisPlotType::Histogram) {
            std::vector<const TrellisAggregateGeometry *> bins;
            for (const TrellisAggregateGeometry &aggregate : layout.aggregates) {
                if (aggregate.panelIndex == panelIndex &&
                    aggregate.kind == TrellisAggregateKind::HistogramBin) bins.push_back(&aggregate);
            }
            std::sort(bins.begin(), bins.end(), [](const auto *left, const auto *right) {
                return left->lower < right->lower;
            });
            HistogramRenderInput input;
            input.layout.plotRect = panel.plotRect;
            input.layout.maxCount = 1;
            input.layout.gap = 2.0;
            input.layout.maxValue = panel.viewport.ymax;
            for (const TrellisAggregateGeometry *bin : bins) {
                const int count = static_cast<int>(bin->caseIds.size());
                input.layout.counts.push_back(count);
                input.layout.values.push_back(bin->value);
                input.layout.maxCount = std::max(input.layout.maxCount, count);
                input.binRows.push_back(bin->caseIds);
            }
            input.selection = selection;
            input.rowColors = effectiveRowColors;
            input.showCounts = plot.histogramShowCounts;
            input.showColorSegments = !seriesOrSplitVariable.empty() ||
                HistogramColorSegmentsVisible(
                    plot.histogramShowDensity, plot.histogramDensityMode);
            input.showRug = plot.histogramShowRug;
            std::vector<HistogramCaseValue> densityCases;
            for (std::size_t pointIndex = 0; pointIndex < plot.points.size(); ++pointIndex) {
                if (pointIndex < plot.trellisPointPanels.size() &&
                    plot.trellisPointPanels[pointIndex] == panel.levelId) {
                    densityCases.push_back({plot.points[pointIndex].x, plot.points[pointIndex].row});
                }
            }
            input.densityCurves = DensityCurvesForHistogramCases(
                densityCases, panel.viewport.xmin, panel.viewport.xmax,
                plot.histogramShowDensity, plot.histogramDensityMode,
                plot.histogramDensityBw, plot.histogramDensityAdjust,
                selection, effectiveRowColors);
            input.densityXMinimum = panel.viewport.xmin;
            input.densityXMaximum = panel.viewport.xmax;
            for (std::size_t bin = 0; bin < input.binRows.size(); ++bin) {
                for (CaseId row : input.binRows[bin]) input.rugCases.push_back({row, bin});
            }
            plan.histogram = BuildHistogramRenderPlan(input);
        } else if (type == TrellisPlotType::Boxplot) {
            std::vector<BoxplotCase> cases;
            for (std::size_t index = 0; index < plot.points.size(); ++index) {
                if (index >= plot.trellisPointPanels.size() ||
                    plot.trellisPointPanels[index] != panel.levelId) continue;
                cases.push_back({plot.points[index].row, plot.points[index].y,
                    index < plot.trellisPointCategories.size()
                        ? plot.trellisPointCategories[index] : ""});
            }
            plan.boxplotLayout = TrellisBoxplotLayoutForPanel(plot, panel);
            plan.boxplotCases = BoxplotCaseGeometryForLayout(plan.boxplotLayout, cases);
            plan.boxplotPoints = BuildBoxplotPointDrawPlan(plan.boxplotCases, selection);
            plan.boxplotStats = BuildBoxplotStatsRenderPlan(
                plan.boxplotLayout, BoxplotStatsForCases(cases, plot.boxplotCategories));
        } else if (type == TrellisPlotType::Bar) {
            for (const std::string &category : plot.boxplotCategories) {
                BarplotBinSummary bin;
                bin.category = category;
                for (std::size_t index = 0; index < plot.points.size(); ++index) {
                    if (index >= plot.trellisPointPanels.size() ||
                        index >= plot.trellisPointCategories.size() ||
                        plot.trellisPointPanels[index] != panel.levelId ||
                        plot.trellisPointCategories[index] != category) continue;
                    bin.rows.push_back(plot.points[index].row);
                }
                bin.n = static_cast<int>(bin.rows.size());
                bin.percent = panel.context.originalRowIndices.empty() ? 0.0 :
                    100.0 * static_cast<double>(bin.n) /
                    static_cast<double>(panel.context.originalRowIndices.size());
                if (!splitLevels.empty()) {
                    for (const std::string &level : splitLevels) {
                        BarplotSegmentSummary segment;
                        segment.level = level;
                        for (std::size_t pointIndex = 0; pointIndex < plot.points.size(); ++pointIndex) {
                            if (pointIndex >= plot.trellisPointPanels.size() ||
                                pointIndex >= plot.trellisPointCategories.size() ||
                                pointIndex >= plot.trellisPointSplitValues.size() ||
                                plot.trellisPointPanels[pointIndex] != panel.levelId ||
                                plot.trellisPointCategories[pointIndex] != category ||
                                plot.trellisPointSplitValues[pointIndex] != level) continue;
                            segment.rows.push_back(plot.points[pointIndex].row);
                        }
                        segment.count = static_cast<int>(segment.rows.size());
                        segment.barN = bin.n;
                        segment.totalN = static_cast<int>(panel.context.originalRowIndices.size());
                        segment.barPercent = bin.n > 0 ?
                            100.0 * static_cast<double>(segment.count) / static_cast<double>(bin.n) : 0.0;
                        bin.segments.push_back(std::move(segment));
                    }
                }
                plan.barplotBins.push_back(std::move(bin));
            }
            plan.barplotLayout = BuildBarplotLayoutForPlotRect(
                panel.plotRect,
                {plot.trellisSpecification.xVariableId},
                plan.barplotBins,
                plot.trellisSpecification.barMeasure == "percent" ? "overall_percent" :
                    (plot.trellisSpecification.barMeasure == "conditional_percent"
                        ? "conditional_percent" : "count"),
                "equal");
            const std::vector<Rect> barRects = BarplotBarRects(plan.barplotLayout);
            for (std::size_t index = 0; index < plan.barplotBins.size(); ++index) {
                if (index >= barRects.size()) break;
                plan.barplotSegments.push_back(BuildBarplotSegmentDrawPlan(
                    barRects[index], plan.barplotBins[index], splitLevels,
                    barVisualState, rowColors));
            }
        }
        result.push_back(std::move(plan));
    }
    return result;
}

std::optional<std::size_t> TrellisPanelIndexAtPoint(const TrellisScatterplotLayout &layout,
    const Point &point)
{
    for (std::size_t index = 0; index < layout.panels.size(); ++index) {
        if (PointInRect(point, layout.panels[index].plotRect)) return index;
    }
    return std::nullopt;
}

std::vector<std::size_t> TrellisConditioningVariableIndicesAtPoint(
    const PlotModel &plot,
    const TrellisScatterplotLayout &layout,
    const Point &point)
{
    std::vector<std::size_t> indices;
    if (!plot.trellisSpecificationInitialized) return indices;
    auto addDimension = [&](TrellisDimension dimension) {
        const auto &conditions = plot.trellisSpecification.conditioningVariables;
        for (std::size_t index = 0; index < conditions.size(); ++index) {
            if (conditions[index].dimension == dimension &&
                std::find(indices.begin(), indices.end(), index) == indices.end()) {
                indices.push_back(index);
            }
        }
    };
    for (const TrellisPanelLayout &panel : layout.panels) {
        if (IsValidRect(panel.rowStripRect) && PointInRect(point, panel.rowStripRect)) {
            addDimension(TrellisDimension::Rows);
            break;
        }
        if (IsValidRect(panel.columnStripRect) && PointInRect(point, panel.columnStripRect)) {
            addDimension(TrellisDimension::Columns);
            addDimension(TrellisDimension::Nested);
            break;
        }
    }
    return indices;
}

Point TrellisScreenToData(const TrellisScatterplotLayout &layout,
                          std::size_t panelIndex,
                          const Point &point)
{
    if (panelIndex >= layout.panels.size()) return {NAN, NAN};
    return ScreenToData(point, layout.panels[panelIndex].viewport,
                        layout.panels[panelIndex].plotRect, true);
}

std::optional<TrellisCaseGeometry> HitTrellisScatterplotCase(
    const TrellisScatterplotLayout &layout,
    const Point &point,
    double maximumDistance)
{
    const std::optional<std::size_t> panelIndex = TrellisPanelIndexAtPoint(layout, point);
    if (!panelIndex) return std::nullopt;
    const double maximumSquared = maximumDistance * maximumDistance;
    const TrellisCaseGeometry *nearest = nullptr;
    double nearestSquared = maximumSquared;
    for (const TrellisCaseGeometry &entry : layout.cases) {
        if (entry.panelIndex != *panelIndex) continue;
        const double distance = DistanceSquared(entry.screenPoint, point);
        if (distance <= nearestSquared) { nearest = &entry; nearestSquared = distance; }
    }
    return nearest ? std::optional<TrellisCaseGeometry>(*nearest) : std::nullopt;
}

std::optional<TrellisAggregateGeometry> HitTrellisAggregate(
    const TrellisScatterplotLayout &layout,
    const Point &point)
{
    const std::optional<std::size_t> panelIndex = TrellisPanelIndexAtPoint(layout, point);
    if (!panelIndex) return std::nullopt;
    for (auto iterator = layout.aggregates.rbegin(); iterator != layout.aggregates.rend(); ++iterator) {
        if (iterator->panelIndex == *panelIndex && PointInRect(point, iterator->rect)) return *iterator;
    }
    return std::nullopt;
}

std::set<CaseId> SelectTrellisScatterplotCasesForGesture(
    const TrellisScatterplotLayout &layout,
    const Rect &brush,
    const Point &clickPoint,
    bool dragBrush,
    double maxClickDistance,
    double glyphPadding)
{
    if (!dragBrush) {
        const std::optional<TrellisCaseGeometry> hit =
            HitTrellisScatterplotCase(layout, clickPoint, maxClickDistance);
        if (hit) return {hit->caseId};
        const std::optional<TrellisAggregateGeometry> aggregate = HitTrellisAggregate(layout, clickPoint);
        return aggregate ? std::set<CaseId>(aggregate->caseIds.begin(), aggregate->caseIds.end())
                         : std::set<CaseId>{};
    }
    const std::optional<std::size_t> panelIndex = TrellisPanelIndexAtPoint(layout, clickPoint);
    if (!panelIndex) return {};
    const Rect clippedBrush = IntersectionRect(brush, layout.panels[*panelIndex].plotRect);
    if (!IsValidRect(clippedBrush)) return {};
    std::vector<ScatterplotCaseGeometry> cases;
    cases.reserve(layout.cases.size());
    for (const TrellisCaseGeometry &entry : layout.cases) {
        if (entry.panelIndex != *panelIndex) continue;
        cases.push_back({entry.caseId, entry.screenPoint, false, {}});
    }
    std::set<CaseId> selected = SelectCasesForGesture(cases, clippedBrush, clickPoint, true,
                                                      maxClickDistance, glyphPadding);
    for (const TrellisAggregateGeometry &aggregate : layout.aggregates) {
        if (aggregate.panelIndex != *panelIndex) continue;
        const Rect overlap = IntersectionRect(clippedBrush, aggregate.rect);
        if (!IsValidRect(overlap)) continue;
        selected.insert(aggregate.caseIds.begin(), aggregate.caseIds.end());
    }
    return selected;
}

TrellisViewSize TrellisScatterplotPreferredViewSize(std::size_t panelCount)
{
    if (panelCount <= 1) return {};
    const std::size_t columns = PreferredAutomaticColumns(panelCount);
    const std::size_t rows = (panelCount + columns - 1) / columns;
    return {std::max(720.0, 300.0 * columns + 80.0),
            std::max(460.0, 230.0 * rows + 90.0)};
}

std::string TrellisScatterplotDefaultTitle(const std::string &xVariable,
                                           const std::string &yVariable,
                                           const std::string &conditionVariable)
{
    return yVariable + " by " + xVariable + ", conditioned by " + conditionVariable;
}

} // namespace core
} // namespace rlispstat
