#include "table1_model.h"

#include "barplot_model.h"
#include "format_model.h"
#include "statistics_model.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <ctime>
#include <functional>
#include <map>
#include <set>
#include <sstream>

namespace rlispstat {
namespace core {

Table1ContextMenuTitles DefaultTable1ContextMenuTitles()
{
    return {
        "Table 1",
        "Copy Table",
        "Export",
        "CSV...",
        "Markdown...",
        "PDF...",
        "Add variable",
        "No available variables",
        "Change variable",
        "Show variable information",
        "Grouping variable",
        "Clear grouping variable"
    };
}

std::string Table1RemoveVariableTitle(const std::string &variable)
{
    return "Remove " + variable;
}

std::string Table1TypeMenuTitle(const std::string &type)
{
    return "Treat as " + type;
}

std::string Table1HistogramMenuTitle(const std::string &variable)
{
    return "Histogram of " + variable;
}

std::string Table1BarChartMenuTitle(const std::string &variable)
{
    return "Bar chart of " + variable;
}

std::string Table1BoxplotMenuTitle(const std::string &variable,
                                   const std::string &groupVariable)
{
    if (groupVariable.empty()) {
        return "Boxplot of " + variable;
    }
    return "Boxplot of " + variable + " by " + groupVariable;
}

namespace {

struct Table1NativeTestResult {
    std::string test;
    std::string pText;
    double p = NAN;
};

struct Table1RankedObservation {
    double score;
    int group;
    double rank = 0.0;
};

std::string Table1CSVCell(const std::string &value)
{
    std::string out = "\"";
    for (char ch : value) {
        if (ch == '"') out += "\"\"";
        else out.push_back(ch);
    }
    out += "\"";
    return out;
}

std::string Table1MarkdownCell(std::string value)
{
    std::string out;
    for (char ch : value) {
        if (ch == '|') out += "\\|";
        else out.push_back(ch);
    }
    return out;
}

double Table1Variance(const std::vector<double> &values)
{
    if (values.size() < 2) return NAN;
    double mean = 0.0;
    for (double value : values) mean += value;
    mean /= static_cast<double>(values.size());
    double ss = 0.0;
    for (double value : values) ss += (value - mean) * (value - mean);
    return ss / static_cast<double>(values.size() - 1);
}

double Table1Mean(const std::vector<double> &values)
{
    if (values.empty()) return NAN;
    double sum = 0.0;
    for (double value : values) sum += value;
    return sum / static_cast<double>(values.size());
}

Table1NativeTestResult Table1WelchTest(const DataColumn &col,
                                       const std::vector<std::string> &columns,
                                       const std::map<std::string, std::vector<int>> &slices)
{
    Table1NativeTestResult result;
    std::vector<std::vector<double>> groups;
    for (std::size_t i = 1; i < columns.size(); ++i) {
        groups.push_back(Table1NumericValuesForRows(col, slices.at(columns[i])));
    }
    if (groups.size() == 2) {
        result.test = "Welch t";
        const std::vector<double> &a = groups[0];
        const std::vector<double> &b = groups[1];
        if (a.size() < 2 || b.size() < 2) {
            result.pText = FormatPValue(NAN);
            return result;
        }
        double va = Table1Variance(a);
        double vb = Table1Variance(b);
        double se2 = va / static_cast<double>(a.size()) + vb / static_cast<double>(b.size());
        if (!(se2 > 0.0) || !std::isfinite(se2)) {
            result.pText = FormatPValue(NAN);
            return result;
        }
        double t = (Table1Mean(a) - Table1Mean(b)) / std::sqrt(se2);
        double termA = va / static_cast<double>(a.size());
        double termB = vb / static_cast<double>(b.size());
        double df = (termA + termB) * (termA + termB) /
            (termA * termA / static_cast<double>(a.size() - 1) +
             termB * termB / static_cast<double>(b.size() - 1));
        result.p = FDistributionUpperTail(t * t, 1.0, df);
        result.pText = FormatPValue(result.p);
        return result;
    }

    result.test = "Welch ANOVA";
    int k = static_cast<int>(groups.size());
    std::vector<double> means, weights;
    for (const std::vector<double> &g : groups) {
        if (g.size() < 2) {
            result.pText = FormatPValue(NAN);
            return result;
        }
        double v = Table1Variance(g);
        if (!(v > 0.0) || !std::isfinite(v)) {
            result.pText = FormatPValue(NAN);
            return result;
        }
        means.push_back(Table1Mean(g));
        weights.push_back(static_cast<double>(g.size()) / v);
    }
    double wsum = 0.0;
    double weightedMean = 0.0;
    for (int i = 0; i < k; ++i) {
        wsum += weights[static_cast<std::size_t>(i)];
        weightedMean += weights[static_cast<std::size_t>(i)] * means[static_cast<std::size_t>(i)];
    }
    weightedMean /= wsum;
    double numerator = 0.0;
    double lambda = 0.0;
    for (int i = 0; i < k; ++i) {
        double rel = 1.0 - weights[static_cast<std::size_t>(i)] / wsum;
        numerator += weights[static_cast<std::size_t>(i)] *
            std::pow(means[static_cast<std::size_t>(i)] - weightedMean, 2.0);
        lambda += rel * rel / static_cast<double>(groups[static_cast<std::size_t>(i)].size() - 1);
    }
    double df1 = static_cast<double>(k - 1);
    double denom = 1.0 + (2.0 * static_cast<double>(k - 2) /
        (static_cast<double>(k) * static_cast<double>(k) - 1.0)) * lambda;
    double f = (numerator / df1) / denom;
    double df2 = (static_cast<double>(k) * static_cast<double>(k) - 1.0) / (3.0 * lambda);
    result.p = FDistributionUpperTail(f, df1, df2);
    result.pText = FormatPValue(result.p);
    return result;
}

double Table1LogChoose(int n, int k)
{
    if (k < 0 || k > n) return -INFINITY;
    return std::lgamma(n + 1.0) - std::lgamma(k + 1.0) - std::lgamma(n - k + 1.0);
}

double Table1HypergeometricProb(int a, int r1, int r2, int c1, int n)
{
    int b = r1 - a;
    int c = c1 - a;
    int d = r2 - c;
    if (a < 0 || b < 0 || c < 0 || d < 0) return 0.0;
    return std::exp(Table1LogChoose(r1, a) + Table1LogChoose(r2, c) -
                    Table1LogChoose(n, c1));
}

Table1NativeTestResult Table1CategoricalTest(const DataColumn &col,
                                             const std::vector<std::string> &columns,
                                             const std::map<std::string, std::vector<int>> &slices)
{
    Table1NativeTestResult result;
    std::vector<std::string> levels = Table1Levels(col, "categorical");
    int r = static_cast<int>(levels.size());
    int c = static_cast<int>(columns.size()) - 1;
    if (r < 2 || c < 2) {
        result.test = "\u2014";
        result.pText = FormatPValue(NAN);
        return result;
    }
    std::vector<std::vector<int>> counts(static_cast<std::size_t>(r), std::vector<int>(static_cast<std::size_t>(c), 0));
    for (int j = 0; j < c; ++j) {
        const std::vector<int> &rows = slices.at(columns[static_cast<std::size_t>(j) + 1]);
        for (int row : rows) {
            std::string value = DisplayValueForCell(col, static_cast<std::size_t>(row));
            if (DataCellIsMissing(value)) continue;
            auto it = std::find(levels.begin(), levels.end(), value);
            if (it != levels.end()) {
                counts[static_cast<std::size_t>(it - levels.begin())][static_cast<std::size_t>(j)]++;
            }
        }
    }
    std::vector<int> rowTotals(static_cast<std::size_t>(r), 0);
    std::vector<int> colTotals(static_cast<std::size_t>(c), 0);
    int total = 0;
    for (int i = 0; i < r; ++i) {
        for (int j = 0; j < c; ++j) {
            rowTotals[static_cast<std::size_t>(i)] += counts[static_cast<std::size_t>(i)][static_cast<std::size_t>(j)];
            colTotals[static_cast<std::size_t>(j)] += counts[static_cast<std::size_t>(i)][static_cast<std::size_t>(j)];
            total += counts[static_cast<std::size_t>(i)][static_cast<std::size_t>(j)];
        }
    }
    bool sparse = false;
    double chisq = 0.0;
    for (int i = 0; i < r; ++i) {
        for (int j = 0; j < c; ++j) {
            double expected = total > 0
                ? static_cast<double>(rowTotals[static_cast<std::size_t>(i)]) *
                  static_cast<double>(colTotals[static_cast<std::size_t>(j)]) /
                  static_cast<double>(total)
                : NAN;
            if (expected < 5.0) sparse = true;
            if (expected > 0.0) {
                double diff = counts[static_cast<std::size_t>(i)][static_cast<std::size_t>(j)] - expected;
                chisq += diff * diff / expected;
            }
        }
    }
    if (r == 2 && c == 2 && sparse) {
        result.test = "Fisher exact";
        int a = counts[0][0];
        int r1 = rowTotals[0];
        int r2 = rowTotals[1];
        int c1 = colTotals[0];
        double observed = Table1HypergeometricProb(a, r1, r2, c1, total);
        int minA = std::max(0, c1 - r2);
        int maxA = std::min(r1, c1);
        double p = 0.0;
        for (int ai = minA; ai <= maxA; ++ai) {
            double prob = Table1HypergeometricProb(ai, r1, r2, c1, total);
            if (prob <= observed + 1.0e-12) p += prob;
        }
        result.p = std::min(1.0, p);
    } else {
        result.test = "\u03c7\u00b2";
        result.p = ChiSquareUpperTail(chisq, static_cast<double>((r - 1) * (c - 1)));
    }
    result.pText = FormatPValue(result.p);
    return result;
}

std::vector<Table1RankedObservation> Table1OrdinalRanks(
    const DataColumn &col,
    const std::vector<std::string> &columns,
    const std::map<std::string, std::vector<int>> &slices)
{
    std::vector<Table1RankedObservation> obs;
    std::vector<std::string> levels = Table1Levels(col, "ordinal");
    for (std::size_t g = 1; g < columns.size(); ++g) {
        for (int row : slices.at(columns[g])) {
            if (row < 0 || static_cast<std::size_t>(row) >= col.values.size()) continue;
            std::string display = DisplayValueForCell(col, static_cast<std::size_t>(row));
            if (DataCellIsMissing(display)) continue;
            double score = NAN;
            if (!ParseDataCellDouble(col.values[static_cast<std::size_t>(row)], score)) {
                auto it = std::find(levels.begin(), levels.end(), display);
                if (it == levels.end()) continue;
                score = static_cast<double>(it - levels.begin() + 1);
            }
            obs.push_back(Table1RankedObservation{score, static_cast<int>(g) - 1, 0.0});
        }
    }
    std::sort(obs.begin(), obs.end(), [](const Table1RankedObservation &a, const Table1RankedObservation &b) {
        return a.score < b.score;
    });
    std::size_t i = 0;
    while (i < obs.size()) {
        std::size_t j = i + 1;
        while (j < obs.size() && std::fabs(obs[j].score - obs[i].score) < 1.0e-12) ++j;
        double rank = (static_cast<double>(i) + 1.0 + static_cast<double>(j)) / 2.0;
        for (std::size_t k = i; k < j; ++k) obs[k].rank = rank;
        i = j;
    }
    return obs;
}

double Table1TieCorrection(const std::vector<Table1RankedObservation> &obs)
{
    if (obs.size() < 2) return 1.0;
    double tieSum = 0.0;
    std::size_t i = 0;
    while (i < obs.size()) {
        std::size_t j = i + 1;
        while (j < obs.size() && std::fabs(obs[j].score - obs[i].score) < 1.0e-12) ++j;
        double t = static_cast<double>(j - i);
        tieSum += t * t * t - t;
        i = j;
    }
    double n = static_cast<double>(obs.size());
    double correction = 1.0 - tieSum / (n * n * n - n);
    return correction > 0.0 ? correction : 1.0;
}

Table1NativeTestResult Table1OrdinalTest(
    const DataColumn &col,
    const std::vector<std::string> &columns,
    const std::map<std::string, std::vector<int>> &slices)
{
    Table1NativeTestResult result;
    std::vector<Table1RankedObservation> obs = Table1OrdinalRanks(col, columns, slices);
    int groups = static_cast<int>(columns.size()) - 1;
    if (groups == 2) {
        result.test = "Wilcoxon rank-sum";
        int n1 = 0, n2 = 0;
        double r1 = 0.0;
        for (const Table1RankedObservation &o : obs) {
            if (o.group == 0) {
                ++n1;
                r1 += o.rank;
            } else if (o.group == 1) {
                ++n2;
            }
        }
        if (n1 > 0 && n2 > 0) {
            double u = r1 - static_cast<double>(n1) * static_cast<double>(n1 + 1) / 2.0;
            double mean = static_cast<double>(n1) * static_cast<double>(n2) / 2.0;
            double n = static_cast<double>(n1 + n2);
            double correction = Table1TieCorrection(obs);
            double var = static_cast<double>(n1) * static_cast<double>(n2) * (n + 1.0) / 12.0 * correction;
            double z = (u - mean) / std::sqrt(var);
            result.p = NormalTwoSidedP(z);
        }
        result.pText = FormatPValue(result.p);
        return result;
    }

    result.test = "Kruskal-Wallis";
    std::vector<int> ns(static_cast<std::size_t>(groups), 0);
    std::vector<double> rankSums(static_cast<std::size_t>(groups), 0.0);
    for (const Table1RankedObservation &o : obs) {
        if (o.group >= 0 && o.group < groups) {
            ns[static_cast<std::size_t>(o.group)]++;
            rankSums[static_cast<std::size_t>(o.group)] += o.rank;
        }
    }
    double n = static_cast<double>(obs.size());
    if (n > 0.0) {
        double h = 0.0;
        for (int g = 0; g < groups; ++g) {
            if (ns[static_cast<std::size_t>(g)] > 0) {
                h += rankSums[static_cast<std::size_t>(g)] *
                     rankSums[static_cast<std::size_t>(g)] /
                     static_cast<double>(ns[static_cast<std::size_t>(g)]);
            }
        }
        h = 12.0 / (n * (n + 1.0)) * h - 3.0 * (n + 1.0);
        h /= Table1TieCorrection(obs);
        result.p = ChiSquareUpperTail(h, static_cast<double>(groups - 1));
    }
    result.pText = FormatPValue(result.p);
    return result;
}

} // namespace

bool Table1RowIsIndented(const Table1DisplayRow &row)
{
    return row.rowType.find("level") != std::string::npos ||
           row.rowType.find("median") != std::string::npos ||
           row.rowType == "missing" ||
           row.rowType == "nested_sub_variable";
}

bool Table1RowIsParent(const Table1DisplayRow &row)
{
    return row.rowType.find("parent") != std::string::npos;
}

bool Table1RowIsSubVariable(const Table1DisplayRow &row)
{
    return row.rowType == "nested_sub_variable";
}

bool Table1RowHasValues(const Table1DisplayRow &row)
{
    return row.rowType == "n" ||
           row.rowType == "missing" ||
           row.rowType == "nested_leaf_level" ||
           row.rowType == "nested_total" ||
           row.rowType.find("_level") != std::string::npos ||
           row.rowType.find("numeric_") != std::string::npos ||
           row.rowType.find("median") != std::string::npos;
}

std::string Table1DisplayLabelText(const Table1DisplayRow &row)
{
    std::string label = row.label;
    while (!label.empty() && std::isspace(static_cast<unsigned char>(label.front()))) {
        label.erase(label.begin());
    }
    return label;
}

Table1ReportLayout BuildTable1ReportLayout(const Table1DisplayState &state,
                                           double width)
{
    Table1ReportLayout layout;
    layout.width = width;
    layout.hasGroup = !state.groupVariable.empty();
    layout.hasStubs = !state.stubHeaders.empty();
    layout.hasSpanningHeader = !state.groupSpanningHeader.empty() &&
        state.groupSpanningColumnCount > 0;
    layout.variableWidth = layout.hasGroup
        ? std::min(240.0, std::max(160.0, width * 0.28))
        : std::min(200.0, std::max(120.0, width * 0.20));
    layout.pWidth = state.showP ? 62.0 : 0.0;
    layout.testWidth = state.showTest ? 128.0 : 0.0;
    layout.stubColumnCount = std::max(1, layout.hasStubs ? static_cast<int>(state.stubHeaders.size()) : 1);
    layout.stubWidth = layout.hasStubs
        ? std::max(50.0, layout.variableWidth / static_cast<double>(layout.stubColumnCount))
        : layout.variableWidth;
    layout.stubArea = layout.hasStubs
        ? layout.stubWidth * static_cast<double>(layout.stubColumnCount)
        : layout.variableWidth;
    layout.valueColumnCount = std::max(1, static_cast<int>(state.columns.size()));
    layout.valueArea = width - 2.0 * layout.margin - layout.stubArea -
        layout.pWidth - layout.testWidth - 16.0;
    layout.columnWidth = std::max(78.0, layout.valueArea / static_cast<double>(layout.valueColumnCount));
    layout.headerOffset = 66.0 + (layout.hasSpanningHeader ? 14.0 : 0.0);
    return layout;
}

double Table1PreferredHeight(const Table1DisplayState &state)
{
    Table1ReportLayout layout = BuildTable1ReportLayout(state, 720.0);
    return 48.0 + static_cast<double>(state.rows.size()) * layout.rowHeight +
        72.0 + (layout.hasSpanningHeader ? 14.0 : 0.0) +
        static_cast<double>(state.footnotes.size() + state.warnings.size()) * 18.0;
}

int Table1ReportRowAtPoint(const Table1DisplayState &state,
                           const Table1ReportLayout &layout,
                           const Point &point)
{
    double y = point.y - layout.headerOffset;
    if (y < 0.0) {
        return -1;
    }
    int row = static_cast<int>(std::floor(y / layout.rowHeight));
    if (row < 0 || static_cast<std::size_t>(row) >= state.rows.size()) {
        return -1;
    }
    return row;
}

int Table1ReportColumnAtPoint(const Table1DisplayState &state,
                              const Table1ReportLayout &layout,
                              const Point &point)
{
    double dataX = layout.margin + layout.stubArea;
    if (point.x < dataX) {
        return -1;
    }
    int column = static_cast<int>((point.x - dataX) / layout.columnWidth);
    if (column < 0 || static_cast<std::size_t>(column) >= state.columns.size()) {
        return -1;
    }
    return column;
}

bool Table1ReportPointIsColumnHeader(const Table1ReportLayout &layout,
                                     const Point &point)
{
    return point.y < layout.headerOffset;
}

ContingencySelection Table1ContingencySelectionAtPoint(
    const Table1DisplayState &state,
    const Table1ReportLayout &layout,
    const Point &point,
    int rowIndex)
{
    ContingencySelection selection;
    if (state.tableType != "nested_contingency") {
        return selection;
    }
    if (Table1ReportPointIsColumnHeader(layout, point)) {
        int column = Table1ReportColumnAtPoint(state, layout, point);
        if (column >= 0) {
            selection.kind = ContingencySelectionKind::SplitColumn;
            selection.columnIndex = static_cast<std::size_t>(column);
        }
        return selection;
    }

    if (rowIndex < 0 || static_cast<std::size_t>(rowIndex) >= state.rows.size()) {
        return selection;
    }
    const Table1DisplayRow &row = state.rows[static_cast<std::size_t>(rowIndex)];
    if ((row.rowType != "nested_leaf_level" && row.rowType != "nested_total") || row.rowRows.empty()) {
        return selection;
    }

    selection.rowIndex = static_cast<std::size_t>(rowIndex);
    int column = Table1ReportColumnAtPoint(state, layout, point);
    if (column >= 0) {
        selection.kind = ContingencySelectionKind::Cell;
        selection.columnIndex = static_cast<std::size_t>(column);
        return selection;
    }

    if (row.rowType == "nested_total") {
        selection.kind = ContingencySelectionKind::LeafRow;
        return selection;
    }

    const double stubStart = layout.margin;
    const double stubEnd = layout.margin + layout.stubArea;
    if (!layout.hasStubs || point.x < stubStart || point.x >= stubEnd) {
        return selection;
    }
    std::size_t keySize = row.contingencyRowKey.x_levels.size();
    if (keySize == 0) {
        return selection;
    }
    int rawDepth = static_cast<int>((point.x - layout.margin) / layout.stubWidth);
    std::size_t depth = static_cast<std::size_t>(std::min(
        std::max(0, rawDepth),
        static_cast<int>(keySize) - 1));
    selection.x_prefix.assign(row.contingencyRowKey.x_levels.begin(),
                              row.contingencyRowKey.x_levels.begin() + depth + 1);
    selection.kind = (depth + 1 >= keySize)
        ? ContingencySelectionKind::LeafRow
        : ContingencySelectionKind::RowPrefix;
    return selection;
}

std::vector<double> Table1NumericValuesForRows(const DataColumn &col,
                                               const std::vector<int> &rows)
{
    std::vector<double> values;
    for (int row : rows) {
        if (row < 0 || static_cast<std::size_t>(row) >= col.values.size()) continue;
        double numeric = NAN;
        if (ParseDataCellDouble(col.values[static_cast<std::size_t>(row)], numeric)) {
            values.push_back(numeric);
        }
    }
    return values;
}

std::string Table1MeanSd(const std::vector<double> &values)
{
    if (values.empty()) return "\u2014";
    double mean = Table1Mean(values);
    double sd = std::sqrt(Table1Variance(values));
    return FormatDoubleOrDash(mean, 1) + " (" + FormatDoubleOrDash(sd, 1) + ")";
}

double Table1Quantile(std::vector<double> values, double p)
{
    if (values.empty()) return NAN;
    std::sort(values.begin(), values.end());
    if (values.size() == 1) return values.front();
    p = std::max(0.0, std::min(1.0, p));
    double h = static_cast<double>(values.size() - 1) * p;
    std::size_t lo = static_cast<std::size_t>(std::floor(h));
    std::size_t hi = static_cast<std::size_t>(std::ceil(h));
    double frac = h - static_cast<double>(lo);
    return values[lo] * (1.0 - frac) + values[hi] * frac;
}

std::string Table1MedianIqr(const std::vector<double> &values)
{
    if (values.empty()) return "\u2014";
    return FormatDoubleOrDash(Table1Quantile(values, 0.5), 1) + " [" +
           FormatDoubleOrDash(Table1Quantile(values, 0.25), 1) + ", " +
           FormatDoubleOrDash(Table1Quantile(values, 0.75), 1) + "]";
}

std::string Table1InferType(const DataColumn &col,
                            const std::map<std::string, std::string> &overrides)
{
    auto oit = overrides.find(col.name);
    if (oit != overrides.end() && !oit->second.empty()) return oit->second;
    std::string type = NormalizeVariableType(col.type);
    if (type == "factor" || type == "character" || type == "logical") {
        return "categorical";
    }
    if (type == "ordered") {
        return "ordinal";
    }
    std::set<double> unique;
    int observed = 0;
    bool allNumeric = true;
    bool integerLike = true;
    for (const std::string &value : col.values) {
        if (DataCellIsMissing(value)) continue;
        ++observed;
        double numeric = NAN;
        if (!ParseDataCellDouble(value, numeric)) {
            allNumeric = false;
            break;
        }
        unique.insert(numeric);
        if (std::fabs(numeric - std::round(numeric)) > 1.0e-9) integerLike = false;
    }
    if (!allNumeric) return "categorical";
    if (static_cast<int>(unique.size()) <= 2 && integerLike) return "categorical";
    int lowLimit = std::min(7, std::max(3, observed / 4));
    if (static_cast<int>(unique.size()) <= lowLimit && integerLike) return "ordinal";
    return "numeric";
}

std::vector<std::string> Table1Levels(const DataColumn &col,
                                      const std::string &type)
{
    std::vector<std::string> levels;
    std::map<double, std::string> numericLevels;
    std::set<std::string> textLevels;
    bool numericOrdinal = type == "ordinal";
    for (std::size_t row = 0; row < col.values.size(); ++row) {
        std::string label = DisplayValueForCell(col, row);
        if (DataCellIsMissing(label)) continue;
        if (numericOrdinal) {
            double numeric = NAN;
            if (ParseDataCellDouble(col.values[row], numeric)) {
                numericLevels[numeric] = label;
                continue;
            }
        }
        textLevels.insert(label);
    }
    if (!numericLevels.empty()) {
        for (const auto &entry : numericLevels) levels.push_back(entry.second);
    }
    for (const std::string &level : textLevels) {
        if (std::find(levels.begin(), levels.end(), level) == levels.end()) {
            levels.push_back(level);
        }
    }
    return levels;
}

std::string Table1CountPercent(const DataColumn &col,
                               const std::vector<int> &rows,
                               const std::string &level)
{
    int n = 0;
    int denom = 0;
    for (int row : rows) {
        if (row < 0 || static_cast<std::size_t>(row) >= col.values.size()) continue;
        std::string value = DisplayValueForCell(col, static_cast<std::size_t>(row));
        if (DataCellIsMissing(value)) continue;
        ++denom;
        if (value == level) ++n;
    }
    return std::to_string(n) + " (" +
        FormatPercent(denom > 0 ? static_cast<double>(n) / static_cast<double>(denom) : NAN, 1) +
        ")";
}

Table1DisplayState Table1StateForDataFrame(
    const DataFrameModel &df,
    const std::string &id,
    std::vector<std::string> variables,
    const std::string &groupVariable,
    const std::map<std::string, std::string> &types,
    const AnalysisScope *dataScope)
{
    Table1DisplayState state;
    state.id = id.empty() ? "table1_" + df.group + "_" + std::to_string(static_cast<long long>(std::time(nullptr))) : id;
    state.datasetId = df.group;
    state.groupVariable = groupVariable;
    state.title = groupVariable.empty()
        ? "Table 1. Descriptive statistics"
        : "Table 1. Descriptive statistics by " + groupVariable;
    state.nativeGenerated = true;
    if (dataScope) {
        state.dataScope = *dataScope;
        state.dataScopeCaptured = true;
    }
    state.variableTypes = types;
    if (variables.empty()) {
        for (const DataColumn &col : df.columns) {
            if (!DataColumnLooksLikeId(col) && col.name != groupVariable) {
                variables.push_back(col.name);
            }
            if (variables.size() >= 8) break;
        }
    }
    variables.erase(std::remove(variables.begin(), variables.end(), groupVariable), variables.end());
    state.variables = variables;

    std::map<std::string, std::vector<int>> slices;
    slices["Overall"] = std::vector<int>();
    if (dataScope && dataScope->kind == AnalysisScopeKind::ExplicitRowIds) {
        for (int rowId : ResolveAnalysisScopeRowIds(*dataScope, static_cast<std::size_t>(std::max(0, df.rows)))) {
            slices["Overall"].push_back(rowId - 1);
        }
    } else {
        for (int i = 0; i < df.rows; ++i) slices["Overall"].push_back(i);
    }
    state.columns.push_back("Overall");
    const DataColumn *groupCol = nullptr;
    for (const DataColumn &col : df.columns) {
        if (col.name == groupVariable) groupCol = &col;
    }
    if (groupCol) {
        for (const std::string &level : Table1Levels(*groupCol, "categorical")) {
            slices[level] = std::vector<int>();
            state.columns.push_back(level);
        }
        for (int row : slices["Overall"]) {
            std::string value = DisplayValueForCell(*groupCol, static_cast<std::size_t>(row));
            if (!DataCellIsMissing(value) && slices.find(value) != slices.end()) {
                slices[value].push_back(row);
            }
        }
        state.showP = true;
        state.showTest = true;
    }

    Table1DisplayRow nrow;
    nrow.rowType = "n";
    nrow.label = "N";
    for (const std::string &colName : state.columns) nrow.values.push_back(std::to_string(slices[colName].size()));
    state.rows.push_back(nrow);

    for (const std::string &variable : variables) {
        const DataColumn *col = nullptr;
        for (const DataColumn &candidate : df.columns) {
            if (candidate.name == variable) col = &candidate;
        }
        if (!col) continue;
        std::string type = Table1InferType(*col, state.variableTypes);
        state.variableTypes[variable] = type;
        Table1NativeTestResult test;
        if (groupCol) {
            if (type == "numeric") {
                test = Table1WelchTest(*col, state.columns, slices);
            } else if (type == "ordinal") {
                test = Table1OrdinalTest(*col, state.columns, slices);
            } else {
                test = Table1CategoricalTest(*col, state.columns, slices);
            }
        }
        if (type == "numeric") {
            Table1DisplayRow mean;
            mean.rowType = "numeric_mean_sd";
            mean.variable = variable;
            mean.label = variable;
            for (const std::string &colName : state.columns) {
                mean.values.push_back(Table1MeanSd(Table1NumericValuesForRows(*col, slices[colName])));
            }
            if (groupCol) {
                mean.test = test.test;
                mean.p = test.pText;
            }
            state.rows.push_back(mean);

            Table1DisplayRow median;
            median.rowType = "numeric_median_iqr";
            median.variable = variable;
            median.label = "  Median [Q1, Q3]";
            for (const std::string &colName : state.columns) {
                median.values.push_back(Table1MedianIqr(Table1NumericValuesForRows(*col, slices[colName])));
            }
            state.rows.push_back(median);
        } else {
            Table1DisplayRow parent;
            parent.rowType = type == "ordinal" ? "ordinal_parent" : "categorical_parent";
            parent.variable = variable;
            parent.label = variable;
            parent.values.assign(state.columns.size(), "");
            if (groupCol) {
                parent.test = test.test;
                parent.p = test.pText;
            }
            state.rows.push_back(parent);
            for (const std::string &level : Table1Levels(*col, type)) {
                Table1DisplayRow levelRow;
                levelRow.rowType = type + "_level";
                levelRow.variable = variable;
                levelRow.level = level;
                levelRow.label = "  " + level;
                for (const std::string &colName : state.columns) {
                    levelRow.values.push_back(Table1CountPercent(*col, slices[colName], level));
                }
                state.rows.push_back(levelRow);
            }
            if (type == "ordinal") {
                Table1DisplayRow median;
                median.rowType = "ordinal_median_iqr";
                median.variable = variable;
                median.label = "  Median [Q1, Q3]";
                for (const std::string &colName : state.columns) {
                    median.values.push_back(Table1MedianIqr(Table1NumericValuesForRows(*col, slices[colName])));
                }
                state.rows.push_back(median);
            }
        }

        Table1DisplayRow missing;
        missing.rowType = "missing";
        missing.variable = variable;
        missing.label = "  Missing";
        for (const std::string &colName : state.columns) {
            int n = 0;
            for (int row : slices[colName]) {
                if (static_cast<std::size_t>(row) < col->values.size() &&
                    DataCellIsMissing(col->values[static_cast<std::size_t>(row)])) {
                    ++n;
                }
            }
            int denom = static_cast<int>(slices[colName].size());
            missing.values.push_back(std::to_string(n) + " (" +
                FormatPercent(denom > 0 ? static_cast<double>(n) / static_cast<double>(denom) : NAN, 1) +
                ")");
        }
        state.rows.push_back(missing);
    }
    state.footnotes.push_back("Numeric variables are shown as mean (SD) and median [Q1, Q3].");
    state.footnotes.push_back("Categorical variables are shown as n (%). Percentages exclude missing values.");
    state.footnotes.push_back("Ordinal variables preserve level order and include median [Q1, Q3].");
    return state;
}

std::string Table1Subtitle(const Table1DisplayState &state,
                           bool includeUngroupedN)
{
    std::ostringstream out;
    if (!state.subtitle.empty()) {
        out << state.subtitle;
    } else {
        if (!state.datasetId.empty()) out << "Dataset: " << state.datasetId;
        if (!state.groupVariable.empty()) {
            if (!state.datasetId.empty()) out << "    ";
            out << state.groupVariableLabel << ": " << state.groupVariable;
        } else if (includeUngroupedN && !state.rows.empty() && !state.rows[0].values.empty()) {
            out << "    N = " << state.rows[0].values[0];
        }
    }
    if (state.dataScopeCaptured && state.dataScope.kind == AnalysisScopeKind::ExplicitRowIds) {
        if (out.tellp() > 0) out << "    ";
        out << AnalysisScopeWindowSummary(state.dataScope, state.dataScope.totalDatasetRows);
    }
    return out.str();
}

std::string Table1PlainText(const Table1DisplayState &state)
{
    std::ostringstream out;
    out << state.title << "\n";
    out << Table1Subtitle(state);
    out << "\n\n";
    if (!state.stubHeaders.empty()) {
        for (const std::string &header : state.stubHeaders) {
            out << header << "\t";
        }
    } else {
        out << "Variable\t";
    }
    for (const std::string &col : state.columns) out << col << "\t";
    if (state.showP) out << "p\t";
    if (state.showTest) out << "Test\t";
    out << "\n";
    for (const Table1DisplayRow &row : state.rows) {
        if (!state.stubHeaders.empty()) {
            for (const std::string &sv : row.stubValues) out << sv << "\t";
        } else {
            out << row.label << "\t";
        }
        for (const std::string &value : row.values) out << value << "\t";
        if (state.showP) out << row.p << "\t";
        if (state.showTest) out << row.test << "\t";
        out << "\n";
    }
    for (const std::string &note : state.footnotes) out << "\n" << note;
    return out.str();
}

std::string Table1CSVText(const Table1DisplayState &state)
{
    std::ostringstream out;
    if (!state.stubHeaders.empty()) {
        bool first = true;
        for (const std::string &header : state.stubHeaders) {
            if (!first) out << ",";
            out << Table1CSVCell(header);
            first = false;
        }
    } else {
        out << Table1CSVCell("Variable");
    }
    for (const std::string &col : state.columns) out << "," << Table1CSVCell(col);
    if (state.showP) out << "," << Table1CSVCell("p");
    if (state.showTest) out << "," << Table1CSVCell("Test");
    out << "\n";
    for (const Table1DisplayRow &row : state.rows) {
        if (!state.stubHeaders.empty()) {
            bool first = true;
            for (const std::string &sv : row.stubValues) {
                if (!first) out << ",";
                out << Table1CSVCell(sv);
                first = false;
            }
        } else {
            out << Table1CSVCell(row.label);
        }
        const std::vector<std::string> &exportValues = row.rawValues.empty()
            ? row.values
            : row.rawValues;
        for (const std::string &value : exportValues) out << "," << Table1CSVCell(value);
        if (state.showP) out << "," << Table1CSVCell(row.p);
        if (state.showTest) out << "," << Table1CSVCell(row.test);
        out << "\n";
    }
    if (state.dataScopeCaptured && state.dataScope.kind == AnalysisScopeKind::ExplicitRowIds) {
        out << "\n" << Table1CSVCell("Analysis scope") << ","
            << Table1CSVCell(AnalysisScopeWindowSummary(
                   state.dataScope, state.dataScope.totalDatasetRows)) << "\n";
    }
    return out.str();
}

std::string Table1MarkdownText(const Table1DisplayState &state)
{
    std::vector<std::string> headers;
    if (!state.stubHeaders.empty()) {
        for (const std::string &header : state.stubHeaders) headers.push_back(header);
    } else {
        headers.push_back("Variable");
    }
    for (const std::string &col : state.columns) headers.push_back(col);
    if (state.showP) headers.push_back("p");
    if (state.showTest) headers.push_back("Test");
    std::ostringstream out;
    out << "### " << state.title << "\n\n";
    const std::string subtitle = Table1Subtitle(state);
    if (!subtitle.empty()) out << "*" << Table1MarkdownCell(subtitle) << "*\n\n";
    out << "|";
    for (const std::string &header : headers) out << " " << Table1MarkdownCell(header) << " |";
    out << "\n|";
    for (std::size_t i = 0; i < headers.size(); ++i) {
        bool isLast = (state.showTest && i == headers.size() - 1);
        bool isStub = !state.stubHeaders.empty() && i < state.stubHeaders.size();
        out << (isStub || isLast ? " :--- |" : " ---: |");
    }
    out << "\n";
    for (const Table1DisplayRow &row : state.rows) {
        out << "|";
        if (!state.stubHeaders.empty()) {
            for (const std::string &sv : row.stubValues) {
                out << " " << Table1MarkdownCell(sv) << " |";
            }
        } else {
            out << " " << Table1MarkdownCell(row.label) << " |";
        }
        for (const std::string &value : row.values) out << " " << Table1MarkdownCell(value) << " |";
        if (state.showP) out << " " << Table1MarkdownCell(row.p) << " |";
        if (state.showTest) out << " " << Table1MarkdownCell(row.test) << " |";
        out << "\n";
    }
    for (const std::string &note : state.footnotes) out << "\n" << note;
    return out.str();
}

std::string Table1ExportedStatus()
{
    return "Exported Table 1.";
}

std::string Table1ExportFailedStatus()
{
    return "Could not export Table 1.";
}

std::string Table1ExportedPDFStatus()
{
    return "Exported Table 1 PDF.";
}

std::string Table1ExportPDFFailedStatus()
{
    return "Could not export Table 1 PDF.";
}

std::string Table1CopiedStatus()
{
    return "Copied Table 1.";
}

std::string Table1StatusFieldHint()
{
    return "Right-click variables to change type, grouping, or copy the table.";
}

std::string Table1WindowTitle()
{
    return "Table 1";
}

std::string Table1ExportPDFPanelTitle()
{
    return "Export Table 1 as PDF";
}

std::string Table1PlotWindowTitle()
{
    return "Table 1 Plot";
}

std::string Table1ExportCSVDialogTitle()
{
    return "Export Table 1 as CSV";
}

std::string Table1ExportMarkdownDialogTitle()
{
    return "Export Table 1 as Markdown";
}

std::string Table1ReportVariableHeader()
{
    return "Variable";
}

std::string Table1ReportPHeader()
{
    return "p";
}

std::string Table1ReportTestHeader()
{
    return "Test";
}

Table1DisplayState NestedContingencyTableStateForDataFrame(
    const DataFrameModel &df,
    const std::string &id,
    const std::vector<std::string> &xVariables,
    const std::string &splitVariable,
    const std::string &displayMode,
    const AnalysisScope *dataScope)
{
    Table1DisplayState state;
    state.id = id;
    state.datasetId = df.group;
    state.nativeGenerated = true;
    if (dataScope) {
        state.dataScope = *dataScope;
        state.dataScopeCaptured = true;
    }
    state.nestedDisplayMode = displayMode;
    state.groupVariableLabel = "Split";
    state.tableType = "nested_contingency";
    if (state.nestedDisplayMode == "count") {
        state.title = "Nested Contingency Table \u2014 Counts";
    } else if (state.nestedDisplayMode == "percent") {
        state.title = "Nested Contingency Table \u2014 Row Percentages";
    } else {
        state.title = "Nested Contingency Table \u2014 Counts and Row Percentages";
    }

    if (xVariables.empty()) return state;

    const DataColumn *splitCol = nullptr;
    if (!splitVariable.empty()) {
        splitCol = FindDataColumnInDataFrame(df, splitVariable);
    }

    std::vector<const DataColumn *> xCols;
    for (const std::string &var : xVariables) {
        const DataColumn *col = FindDataColumnInDataFrame(df, var);
        if (col) xCols.push_back(col);
    }
    if (xCols.empty()) return state;

    std::vector<std::string> splitLevels;
    if (splitCol) {
        {
            std::set<std::string> unique;
            for (std::size_t r = 0; r < splitCol->values.size(); ++r) {
                std::string val = DisplayValueForCell(*splitCol, r);
                if (!DataCellIsMissing(val)) unique.insert(val);
            }
            splitLevels.assign(unique.begin(), unique.end());
        }
        splitLevels = CanonicalLevelOrder(splitLevels);
        state.groupVariable = splitVariable;
    }
    state.variables = xVariables;

    if (splitCol) {
        for (const std::string &level : splitLevels) {
            state.columns.push_back(level);
        }
        state.columns.push_back("Total");
        state.groupSpanningHeader = splitVariable;
        state.groupSpanningColumnCount = (int)splitLevels.size();
    } else {
        state.columns.push_back("N");
    }

    using KeyVec = std::vector<std::string>;
    std::map<KeyVec, int> rowTotal;
    std::map<KeyVec, std::map<std::string, int>> splitCounts;
    std::map<KeyVec, std::vector<int>> rowRows;
    std::map<KeyVec, std::map<std::string, std::vector<int>>> splitCellRows;

    int totalRows = df.rows;
    std::set<int> allowedRows;
    const bool restrictRows = dataScope && dataScope->kind == AnalysisScopeKind::ExplicitRowIds;
    if (restrictRows) {
        const std::vector<int> rows = ResolveAnalysisScopeRowIds(
            *dataScope, static_cast<std::size_t>(std::max(0, df.rows)));
        allowedRows.insert(rows.begin(), rows.end());
    }
    int includedRows = 0;
    std::vector<int> allIncludedRows;
    for (int r = 0; r < totalRows; ++r) {
        if (restrictRows && !allowedRows.count(r + 1)) continue;
        KeyVec key;
        bool anyMissing = false;
        for (const DataColumn *col : xCols) {
            std::string val = (static_cast<std::size_t>(r) < col->values.size())
                ? DisplayValueForCell(*col, static_cast<std::size_t>(r))
                : "";
            if (DataCellIsMissing(val)) { anyMissing = true; break; }
            key.push_back(val);
        }
        if (anyMissing) continue;

        std::string splitVal;
        if (splitCol) {
            splitVal = (static_cast<std::size_t>(r) < splitCol->values.size())
                ? DisplayValueForCell(*splitCol, static_cast<std::size_t>(r)) : "";
            if (DataCellIsMissing(splitVal)) continue;
        }

        int rowId = r + 1;
        ++includedRows;
        allIncludedRows.push_back(rowId);
        rowTotal[key]++;
        rowRows[key].push_back(rowId);

        if (splitCol) {
            splitCounts[key][splitVal]++;
            splitCellRows[key][splitVal].push_back(rowId);
        }
    }

    state.stubHeaders = xVariables;

    std::vector<std::vector<std::string>> canonicalOrders(xCols.size());
    for (std::size_t vi = 0; vi < xCols.size(); ++vi) {
        std::set<std::string> unique;
        const DataColumn *col = xCols[vi];
        for (std::size_t r = 0; r < col->values.size(); ++r) {
            std::string val = DisplayValueForCell(*col, r);
            if (!DataCellIsMissing(val)) unique.insert(val);
        }
        canonicalOrders[vi] = CanonicalLevelOrder(
            std::vector<std::string>(unique.begin(), unique.end()));
    }

    std::vector<KeyVec> allKeys;
    std::function<void(std::size_t, KeyVec &)> buildKeys =
        [&](std::size_t depth, KeyVec &current) {
            if (depth >= canonicalOrders.size()) {
                allKeys.push_back(current);
                return;
            }
            for (const std::string &level : canonicalOrders[depth]) {
                current.push_back(level);
                buildKeys(depth + 1, current);
                current.pop_back();
            }
        };
    KeyVec currentKey;
    buildKeys(0, currentKey);

    std::vector<std::pair<KeyVec, int>> sortedRows;
    for (const KeyVec &key : allKeys) {
        sortedRows.push_back({key, rowTotal[key]});
    }

    KeyVec prevKey;
    bool firstRow = true;

    for (const auto &entry : sortedRows) {
        const KeyVec &key = entry.first;
        int total = entry.second;

        Table1DisplayRow leaf;
        leaf.rowType = "nested_leaf_level";
        leaf.variable = xCols.back()->name;
        leaf.level = key.back();
        leaf.label = key.back();

        for (size_t d = 0; d < key.size(); ++d) {
            if (firstRow) {
                leaf.stubValues.push_back(key[d]);
            } else {
                bool prefixChanged = false;
                for (size_t j = 0; j <= d; ++j) {
                    if (j < key.size() && j < prevKey.size() && key[j] != prevKey[j]) {
                        prefixChanged = true;
                        break;
                    }
                }
                leaf.stubValues.push_back(prefixChanged ? key[d] : "");
            }
        }

        leaf.rowRows = rowRows[key];
        leaf.contingencyRowKey.x_levels = key;

        auto formatValue = [&](int cnt, int denom, bool emptyAsDash) -> std::string {
            if (emptyAsDash && cnt == 0) {
                return "\u2014";
            }
            double pct = (denom > 0) ? (100.0 * cnt / denom) : 0.0;
            if (state.nestedDisplayMode == "count") {
                return std::to_string(cnt);
            } else if (state.nestedDisplayMode == "percent") {
                return FormatPercent(pct / 100.0, 1);
            }
            return std::to_string(cnt) + " (" + FormatPercent(pct / 100.0, 1) + ")";
        };
        if (splitCol) {
            for (const std::string &sl : splitLevels) {
                int cnt = splitCounts[key][sl];
                leaf.values.push_back(formatValue(cnt, total, true));
                leaf.cellRows.push_back(splitCellRows[key][sl]);
            }
            leaf.values.push_back(formatValue(total, total, true));
            leaf.cellRows.push_back(rowRows[key]);
        } else {
            leaf.values.push_back(formatValue(total, total, true));
        }

        state.rows.push_back(leaf);
        prevKey = key;
        firstRow = false;
    }
    if (splitCol) {
        Table1DisplayRow totalRow;
        totalRow.rowType = "nested_total";
        totalRow.label = "Total";
        totalRow.stubValues.assign(xVariables.size(), "");
        if (!totalRow.stubValues.empty()) totalRow.stubValues.front() = "Total";
        totalRow.rowRows = allIncludedRows;
        for (const std::string &level : splitLevels) {
            std::vector<int> rows;
            for (const auto &entry : splitCellRows) {
                auto it = entry.second.find(level);
                if (it != entry.second.end()) rows.insert(rows.end(), it->second.begin(), it->second.end());
            }
            totalRow.values.push_back(std::to_string(rows.size()));
            totalRow.cellRows.push_back(rows);
        }
        totalRow.values.push_back(std::to_string(includedRows));
        totalRow.cellRows.push_back(allIncludedRows);
        state.rows.push_back(totalRow);
    }
    state.statusText = "N = " + std::to_string(includedRows) + ", " +
        std::to_string(std::max(0, totalRows - includedRows)) + " excluded.";
    state.footnotes.push_back(state.statusText);
    return state;
}

static void InsertRows(std::set<int> &target, const std::vector<int> &rows)
{
    target.insert(rows.begin(), rows.end());
}

static bool RowKeyHasPrefix(const ContingencyRowKey &key,
                            const std::vector<LevelKey> &prefix)
{
    if (prefix.empty() || prefix.size() > key.x_levels.size()) {
        return false;
    }
    return std::equal(prefix.begin(), prefix.end(), key.x_levels.begin());
}

std::set<int> ContingencyRowsForRowPrefix(const Table1DisplayState &state,
                                          const std::vector<LevelKey> &prefix)
{
    std::set<int> rows;
    if (state.tableType != "nested_contingency") {
        return rows;
    }
    for (const Table1DisplayRow &row : state.rows) {
        if (row.rowType != "nested_leaf_level") {
            continue;
        }
        if (RowKeyHasPrefix(row.contingencyRowKey, prefix)) {
            InsertRows(rows, row.rowRows);
        }
    }
    return rows;
}

std::set<int> ContingencyRowsForSplitColumn(const Table1DisplayState &state,
                                            std::size_t columnIndex)
{
    std::set<int> rows;
    if (state.tableType != "nested_contingency" || columnIndex >= state.columns.size()) {
        return rows;
    }
    for (const Table1DisplayRow &row : state.rows) {
        if (row.rowType != "nested_leaf_level") {
            continue;
        }
        if (columnIndex < row.cellRows.size()) {
            InsertRows(rows, row.cellRows[columnIndex]);
        } else {
            InsertRows(rows, row.rowRows);
        }
    }
    return rows;
}

std::set<int> ContingencyRowsForCell(const Table1DisplayState &state,
                                     std::size_t rowIndex,
                                     std::size_t columnIndex)
{
    std::set<int> rows;
    if (state.tableType != "nested_contingency" ||
        rowIndex >= state.rows.size() ||
        columnIndex >= state.columns.size()) {
        return rows;
    }
    const Table1DisplayRow &row = state.rows[rowIndex];
    if (row.rowType != "nested_leaf_level" && row.rowType != "nested_total") {
        return rows;
    }
    if (columnIndex < row.cellRows.size()) {
        InsertRows(rows, row.cellRows[columnIndex]);
    } else {
        InsertRows(rows, row.rowRows);
    }
    return rows;
}

std::set<int> ContingencyRowsForSelection(const Table1DisplayState &state,
                                          const ContingencySelection &selection)
{
    if (state.tableType != "nested_contingency") {
        return {};
    }
    switch (selection.kind) {
        case ContingencySelectionKind::RowPrefix:
            return ContingencyRowsForRowPrefix(state, selection.x_prefix);
        case ContingencySelectionKind::LeafRow:
            if (selection.rowIndex >= state.rows.size()) return {};
            return std::set<int>(state.rows[selection.rowIndex].rowRows.begin(),
                                 state.rows[selection.rowIndex].rowRows.end());
        case ContingencySelectionKind::SplitColumn:
            return ContingencyRowsForSplitColumn(state, selection.columnIndex);
        case ContingencySelectionKind::Cell:
            return ContingencyRowsForCell(state, selection.rowIndex, selection.columnIndex);
        case ContingencySelectionKind::None:
            break;
    }
    return {};
}

AggregateCoverage AggregateCoverageForRows(const std::vector<int> &sourceRows,
                                           const std::set<int> &selectedRows)
{
    if (sourceRows.empty()) {
        return AggregateCoverage::Empty;
    }
    std::size_t selectedCount = 0;
    for (int rid : sourceRows) {
        if (selectedRows.count(rid)) {
            ++selectedCount;
        }
    }
    if (selectedCount == 0) {
        return AggregateCoverage::None;
    }
    if (selectedCount < sourceRows.size()) {
        return AggregateCoverage::Partial;
    }
    return AggregateCoverage::Full;
}

AggregateCoverage ContingencyRowCoverage(const Table1DisplayState &state,
                                         std::size_t rowIndex,
                                         const std::set<int> &selectedRows)
{
    if (state.tableType != "nested_contingency" || rowIndex >= state.rows.size()) {
        return AggregateCoverage::Empty;
    }
    const Table1DisplayRow &row = state.rows[rowIndex];
    if (row.rowType != "nested_leaf_level" && row.rowType != "nested_total") {
        return AggregateCoverage::Empty;
    }
    return AggregateCoverageForRows(row.rowRows, selectedRows);
}

AggregateCoverage ContingencyCellCoverage(const Table1DisplayState &state,
                                          std::size_t rowIndex,
                                          std::size_t columnIndex,
                                          const std::set<int> &selectedRows)
{
    if (state.tableType != "nested_contingency" ||
        rowIndex >= state.rows.size() ||
        columnIndex >= state.columns.size()) {
        return AggregateCoverage::Empty;
    }
    const Table1DisplayRow &row = state.rows[rowIndex];
    if (row.rowType != "nested_leaf_level" && row.rowType != "nested_total") {
        return AggregateCoverage::Empty;
    }
    if (columnIndex < row.cellRows.size()) {
        return AggregateCoverageForRows(row.cellRows[columnIndex], selectedRows);
    }
    return AggregateCoverageForRows(row.rowRows, selectedRows);
}

} // namespace core
} // namespace rlispstat
