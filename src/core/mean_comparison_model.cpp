#include "mean_comparison_model.h"

#include "dataset_model.h"
#include "format_model.h"

#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <iomanip>
#include <sstream>

namespace rlispstat {
namespace core {
namespace {

struct NativeResult {
    std::string response1;
    std::string response2;
    std::string method;
    std::string effectLabel;
    std::vector<double> values;
    struct Group {
        std::string label;
        double n = NAN;
        double mean = NAN;
        double sd = NAN;
        double se = NAN;
        std::vector<int> rows;
    };
    std::vector<Group> groups;
    std::vector<int> rowsUsed;
    std::vector<int> rowsExcluded;
    std::vector<std::string> warnings;
};

double WireNumber(const std::string &text)
{
    if (text.empty() || text == "NA" || text == "NaN") return NAN;
    if (text == "Inf") return INFINITY;
    if (text == "-Inf") return -INFINITY;
    char *end = nullptr;
    const double value = std::strtod(text.c_str(), &end);
    return end && *end == '\0' ? value : NAN;
}

bool ReadCount(const std::vector<std::string> &args, std::size_t &cursor, long &count)
{
    if (cursor >= args.size()) return false;
    char *end = nullptr;
    count = std::strtol(args[cursor++].c_str(), &end, 10);
    return end && *end == '\0' && count >= 0;
}

bool ReadRows(const std::vector<std::string> &args, std::size_t &cursor,
              std::vector<int> &rows)
{
    long count = 0;
    if (!ReadCount(args, cursor, count) || cursor + static_cast<std::size_t>(count) > args.size()) return false;
    rows.reserve(static_cast<std::size_t>(count));
    for (long i = 0; i < count; ++i) {
        rows.push_back(static_cast<int>(std::strtol(args[cursor++].c_str(), nullptr, 10)));
    }
    return true;
}

MeanComparisonCell Number(double value)
{
    MeanComparisonCell cell;
    if (std::isnan(value)) cell.notApplicable = true;
    else cell.numericValue = value;
    return cell;
}

MeanComparisonCell Text(const std::string &value)
{
    MeanComparisonCell cell;
    if (value.empty()) cell.notApplicable = true;
    else cell.textValue = value;
    return cell;
}

MeanComparisonCell CI(double lower, double upper)
{
    MeanComparisonCell cell;
    if (std::isnan(lower) || std::isnan(upper)) cell.notApplicable = true;
    else {
        cell.numericValue = lower;
        cell.numericUpperValue = upper;
    }
    return cell;
}

std::string AlternativeTitle(const std::string &alternative)
{
    if (alternative == "greater") return "mean greater than reference";
    if (alternative == "less") return "mean less than reference";
    return "two-sided";
}

std::string Percent(double value)
{
    std::ostringstream out;
    out << std::fixed << std::setprecision(std::fabs(value * 100.0 - std::round(value * 100.0)) < 1e-9 ? 0 : 1)
        << value * 100.0 << "%";
    return out.str();
}

std::string CSVNumber(double value)
{
    if (std::isnan(value)) return "";
    if (std::isinf(value)) return value > 0 ? "Inf" : "-Inf";
    std::ostringstream out;
    out << std::setprecision(17) << value;
    return out.str();
}

std::string CSVCell(const std::string &text)
{
    return CsvEscape(text);
}

void AddCommonWarnings(MeanComparisonTable &table, const NativeResult &result)
{
    table.warnings.insert(table.warnings.end(), result.warnings.begin(), result.warnings.end());
}

AlternativeHypothesis AlternativeFromName(const std::string &name)
{
    if (name == "greater") return AlternativeHypothesis::Greater;
    if (name == "less") return AlternativeHypothesis::Less;
    return AlternativeHypothesis::TwoSided;
}

MeanComparisonMethod MethodForState(const MeanComparisonState &state)
{
    if (state.method.find("Wilcoxon") != std::string::npos) {
        return MeanComparisonMethod::WilcoxonSignedRank;
    }
    if (state.analysisType == "independent_samples_t_test") {
        if (state.method.find("Mann") != std::string::npos) return MeanComparisonMethod::MannWhitney;
        return state.method.find("Student") != std::string::npos
            ? MeanComparisonMethod::StudentT : MeanComparisonMethod::WelchT;
    }
    if (state.analysisType == "one_way_anova") {
        if (state.method.find("Kruskal") != std::string::npos) return MeanComparisonMethod::KruskalWallis;
        return state.method.find("Classical") != std::string::npos
            ? MeanComparisonMethod::ClassicalAnova : MeanComparisonMethod::WelchAnova;
    }
    return MeanComparisonMethod::OneSampleT;
}

void AppendAdjustedPColumn(MeanComparisonTable &table, MultipleTestingAdjustment adjustment)
{
    if (adjustment != MultipleTestingAdjustment::None) {
        table.columns.push_back({"p_adjusted", "p adjusted", MeanComparisonCellFormat::PValue});
    }
}

void AppendAdjustedPCell(std::vector<MeanComparisonCell> &cells, const std::vector<double> &values,
                         MultipleTestingAdjustment adjustment)
{
    if (adjustment != MultipleTestingAdjustment::None) cells.push_back(Number(values.size() > 18 ? values[18] : NAN));
}

std::string AdjustmentNote(MultipleTestingAdjustment adjustment, std::size_t familySize)
{
    if (adjustment == MultipleTestingAdjustment::None) return "";
    return "P values were adjusted using the " + MeanComparisonAdjustmentName(adjustment) +
        " method across the " + std::to_string(familySize) + " tests shown.";
}

std::string AdjustmentToken(MultipleTestingAdjustment adjustment)
{
    if (adjustment == MultipleTestingAdjustment::Bonferroni) return "bonferroni";
    if (adjustment == MultipleTestingAdjustment::None) return "none";
    return "holm";
}

} // namespace

std::string MeanComparisonAdjustmentName(MultipleTestingAdjustment adjustment)
{
    if (adjustment == MultipleTestingAdjustment::Bonferroni) return "Bonferroni";
    if (adjustment == MultipleTestingAdjustment::None) return "None";
    return "Holm";
}

MultipleTestingAdjustment MeanComparisonAdjustmentFromName(const std::string &name)
{
    if (name == "bonferroni") return MultipleTestingAdjustment::Bonferroni;
    if (name == "none") return MultipleTestingAdjustment::None;
    return MultipleTestingAdjustment::Holm;
}

std::string MeanComparisonGroupingVariableLine(const std::string &groupVariable)
{
    return "Grouping variable: " + (groupVariable.empty() ? std::string("\u2014") : groupVariable);
}

std::string MeanComparisonSubtitleWithoutGroupingVariable(
    const std::string &subtitle,
    const std::string &groupVariable)
{
    if (groupVariable.empty()) return subtitle;
    const std::string prefix = MeanComparisonGroupingVariableLine(groupVariable) + " \u00b7 ";
    return subtitle.rfind(prefix, 0) == 0 ? subtitle.substr(prefix.size()) : subtitle;
}

std::vector<MeanComparisonPlotMenuOption> MeanComparisonPlotMenuOptions(
    const std::string &analysisType,
    bool hasSelectedVariable,
    bool hasPairedVariables)
{
    if (!hasSelectedVariable) return {};
    if (analysisType == "one_sample_t_test") {
        return {{"Histogram\u2026", "histogram"}, {"Boxplot\u2026", "boxplot"}};
    }
    if (analysisType == "independent_samples_t_test" || analysisType == "one_way_anova") {
        return {{"Boxplot\u2026", "grouped_boxplot"}};
    }
    if (analysisType == "paired_samples_t_test" && hasPairedVariables) {
        return {{"Forest plot of paired differences\u2026", "paired_forest_plot"},
                {"Scatterplot of paired variables\u2026", "paired_scatterplot"},
                {"Histogram of paired differences\u2026", "paired_difference_histogram"}};
    }
    return {};
}

std::vector<MeanComparisonPairedForestRow> MeanComparisonPairedForestRows(
    const MeanComparisonState &state)
{
    std::vector<MeanComparisonPairedForestRow> out;
    for (const MeanComparisonTable &table : state.tables) {
        if (table.tableId != "paired") continue;
        auto columnIndex = [&](const std::string &key) {
            for (std::size_t index = 0; index < table.columns.size(); ++index) {
                if (table.columns[index].key == key) return index;
            }
            return table.columns.size();
        };
        const std::size_t difference = columnIndex("difference");
        const std::size_t standardError = columnIndex("se_difference");
        const std::size_t confidenceInterval = columnIndex("ci");
        std::size_t statistic = columnIndex("statistic");
        if (statistic == table.columns.size()) statistic = columnIndex("t");
        const std::size_t degreesOfFreedom = columnIndex("df");
        const std::size_t pValue = columnIndex("p");
        const std::size_t adjustedPValue = columnIndex("p_adjusted");
        for (const MeanComparisonRow &row : table.rows) {
            if (row.variable.empty() || row.secondVariable.empty() ||
                difference >= row.cells.size() || confidenceInterval >= row.cells.size()) continue;
            const MeanComparisonCell &estimateCell = row.cells[difference];
            const MeanComparisonCell &confidenceCell = row.cells[confidenceInterval];
            if (!estimateCell.numericValue || !confidenceCell.numericValue ||
                !confidenceCell.numericUpperValue ||
                !std::isfinite(*estimateCell.numericValue) ||
                !std::isfinite(*confidenceCell.numericValue) ||
                !std::isfinite(*confidenceCell.numericUpperValue)) continue;
            auto numeric = [&](std::size_t index) {
                return index < row.cells.size() && row.cells[index].numericValue
                    ? *row.cells[index].numericValue : NAN;
            };
            MeanComparisonPairedForestRow item;
            item.label = row.label;
            item.firstVariable = row.variable;
            item.secondVariable = row.secondVariable;
            item.estimate = *estimateCell.numericValue;
            item.standardError = numeric(standardError);
            item.statistic = numeric(statistic);
            item.degreesOfFreedom = numeric(degreesOfFreedom);
            item.pValue = numeric(pValue);
            item.adjustedPValue = numeric(adjustedPValue);
            if (!std::isfinite(item.adjustedPValue)) item.adjustedPValue = item.pValue;
            item.confidenceLower = *confidenceCell.numericValue;
            item.confidenceUpper = *confidenceCell.numericUpperValue;
            item.originalRowIndices = row.originalRowIndices;
            out.push_back(std::move(item));
        }
    }
    return out;
}

std::vector<int> MeanComparisonLinkedRows(const MeanComparisonRow &row)
{
    // Inferential rows describe a statistic, not a subset of cases. Linking their
    // complete analysis sample would make a row click select every plotted point.
    // A grouped descriptive row, however, has an unambiguous case-level meaning.
    if (row.kind != MeanComparisonRowKind::Descriptive || row.group.empty()) return {};
    return row.originalRowIndices;
}

std::string MeanComparisonCellText(const MeanComparisonCell &cell,
                                   MeanComparisonCellFormat format)
{
    if (cell.notApplicable) return "\u2014";
    if (cell.textValue) return *cell.textValue;
    if (!cell.numericValue) return "\u2014";
    if (format == MeanComparisonCellFormat::ConfidenceInterval) {
        if (!cell.numericUpperValue) return "\u2014";
        return "[" + FormatModelNumberOrDash(*cell.numericValue) + ", " +
            FormatModelNumberOrDash(*cell.numericUpperValue) + "]";
    }
    if (format == MeanComparisonCellFormat::PValue) return FormatPValue(*cell.numericValue);
    if (format == MeanComparisonCellFormat::Integer && std::isfinite(*cell.numericValue)) {
        return std::to_string(static_cast<long long>(std::llround(*cell.numericValue)));
    }
    return FormatModelNumberOrDash(*cell.numericValue);
}

bool ParseMeanComparisonBatch(const std::vector<std::string> &args,
                              MeanComparisonState &state,
                              std::string *error)
{
    auto fail = [&](const std::string &message) {
        if (error) *error = message;
        return false;
    };
    const bool version2 = !args.empty() && args[0] == "COMPARE_MEANS_BATCH_V2";
    if (args.size() < 12 || (!version2 && args[0] != "COMPARE_MEANS_BATCH_V1")) {
        return fail("Malformed compare-means batch header.");
    }
    std::size_t cursor = 1;
    state = MeanComparisonState();
    state.id = args[cursor++];
    state.datasetId = args[cursor++];
    state.analysisType = args[cursor++];
    state.title = args[cursor++];
    state.groupVariable = args[cursor++];
    state.method = args[cursor++];
    state.alternative = args[cursor++];
    state.confidenceLevel = WireNumber(args[cursor++]);
    state.testValue = WireNumber(args[cursor++]);
    state.firstGroup = args[cursor++];
    state.secondGroup = args[cursor++];
    const std::string adjustmentName = version2 && cursor < args.size() ? args[cursor++] : "none";
    state.specification.pAdjustment = MeanComparisonAdjustmentFromName(adjustmentName);
    long resultCount = 0;
    if (!ReadCount(args, cursor, resultCount)) return fail("Malformed compare-means result count.");

    std::vector<NativeResult> results;
    results.reserve(static_cast<std::size_t>(resultCount));
    for (long resultIndex = 0; resultIndex < resultCount; ++resultIndex) {
        const int valueCount = version2 ? 20 : 18;
        if (cursor + 4 + static_cast<std::size_t>(valueCount) > args.size()) return fail("Truncated compare-means result.");
        NativeResult result;
        result.response1 = args[cursor++];
        result.response2 = args[cursor++];
        result.method = args[cursor++];
        result.effectLabel = args[cursor++];
        for (int i = 0; i < valueCount; ++i) result.values.push_back(WireNumber(args[cursor++]));
        long groupCount = 0;
        if (!ReadCount(args, cursor, groupCount)) return fail("Malformed descriptive-group count.");
        for (long groupIndex = 0; groupIndex < groupCount; ++groupIndex) {
            if (cursor + 5 > args.size()) return fail("Truncated descriptive-group row.");
            NativeResult::Group group;
            group.label = args[cursor++];
            group.n = WireNumber(args[cursor++]);
            group.mean = WireNumber(args[cursor++]);
            group.sd = WireNumber(args[cursor++]);
            group.se = WireNumber(args[cursor++]);
            if (!ReadRows(args, cursor, group.rows)) return fail("Malformed descriptive-group row mapping.");
            result.groups.push_back(std::move(group));
        }
        if (!ReadRows(args, cursor, result.rowsUsed) || !ReadRows(args, cursor, result.rowsExcluded)) {
            return fail("Malformed compare-means row mapping.");
        }
        long warningCount = 0;
        if (!ReadCount(args, cursor, warningCount) || cursor + static_cast<std::size_t>(warningCount) > args.size()) {
            return fail("Malformed compare-means warning block.");
        }
        for (long i = 0; i < warningCount; ++i) result.warnings.push_back(args[cursor++]);
        results.push_back(std::move(result));
    }

    state.specification.alternative = AlternativeFromName(state.alternative);
    state.specification.confidenceLevel = state.confidenceLevel;
    state.specification.testValue = state.testValue;
    state.specification.method = MethodForState(state);
    if (!state.groupVariable.empty()) state.specification.groupingVariableId = state.groupVariable;
    if (!results.empty() && results.front().values.size() > 19 && std::isfinite(results.front().values[19])) {
        state.adjustmentFamilySize = static_cast<std::size_t>(std::max(0.0, results.front().values[19]));
    }
    const MultipleTestingAdjustment adjustment = state.specification.pAdjustment;

    const std::string level = Percent(state.confidenceLevel);
    const bool signedRank = state.specification.method == MeanComparisonMethod::WilcoxonSignedRank;
    const bool mannWhitney = state.specification.method == MeanComparisonMethod::MannWhitney;
    const bool kruskalWallis = state.specification.method == MeanComparisonMethod::KruskalWallis;
    if (state.analysisType == "one_sample_t_test") {
        state.specification.kind = MeanComparisonKind::OneSample;
        state.subtitle = state.method + " \u00b7 Test value: " + FormatModelNumberOrDash(state.testValue) +
            " \u00b7 Alternative: " + AlternativeTitle(state.alternative) +
            " \u00b7 Confidence level: " + level;
        MeanComparisonTable table;
        table.pAdjustment = adjustment; table.adjustmentFamilySize = state.adjustmentFamilySize;
        table.tableId = "one_sample";
        table.stubTitle = "Variable";
        table.columns = signedRank ? std::vector<MeanComparisonColumn>{
            {"n", "N", MeanComparisonCellFormat::Integer}, {"mean", "M", MeanComparisonCellFormat::Number},
            {"sd", "SD", MeanComparisonCellFormat::Number, false}, {"test_value", "Test value", MeanComparisonCellFormat::Number},
            {"difference", "Location shift", MeanComparisonCellFormat::Number},
            {"ci", level + " CI", MeanComparisonCellFormat::ConfidenceInterval},
            {"statistic", "V", MeanComparisonCellFormat::Number}, {"p", "p", MeanComparisonCellFormat::PValue}
        } : std::vector<MeanComparisonColumn>{
            {"n", "N", MeanComparisonCellFormat::Integer}, {"mean", "M", MeanComparisonCellFormat::Number},
            {"sd", "SD", MeanComparisonCellFormat::Number, false}, {"test_value", "Test value", MeanComparisonCellFormat::Number},
            {"difference", "Mean difference", MeanComparisonCellFormat::Number},
            {"se_difference", "SE difference", MeanComparisonCellFormat::Number},
            {"ci", level + " CI", MeanComparisonCellFormat::ConfidenceInterval},
            {"t", "t", MeanComparisonCellFormat::Number}, {"df", "df", MeanComparisonCellFormat::Number},
            {"p", "p", MeanComparisonCellFormat::PValue}
        };
        AppendAdjustedPColumn(table, adjustment);
        table.columns.push_back({"effect", signedRank ? "r\u1d63\u1d66" : "Cohen\u2019s d", MeanComparisonCellFormat::Number});
        MeanComparisonTable desc;
        desc.tableId = "group_descriptives"; desc.title = "Descriptives"; desc.stubTitle = "Variable";
        desc.columns = {{"n", "N", MeanComparisonCellFormat::Integer}, {"mean", "M", MeanComparisonCellFormat::Number},
                        {"sd", "SD", MeanComparisonCellFormat::Number}, {"se", "SE", MeanComparisonCellFormat::Number}};
        for (std::size_t i = 0; i < results.size(); ++i) {
            const auto &r = results[i]; const auto &v = r.values;
            MeanComparisonRow row;
            row.rowId = "one_sample:" + r.response1; row.label = r.response1; row.variable = r.response1;
            state.specification.dependentVariableIds.push_back(r.response1);
            row.originalRowIndices = r.rowsUsed;
            row.cells = signedRank
                ? std::vector<MeanComparisonCell>{Number(v[0]), Number(v[1]), Number(v[2]), Number(state.testValue),
                                                  Number(v[8]), CI(v[11], v[12]), Number(v[13]), Number(v[16])}
                : std::vector<MeanComparisonCell>{Number(v[0]), Number(v[1]), Number(v[2]), Number(state.testValue), Number(v[8]),
                                                  Number(v[10]), CI(v[11], v[12]), Number(v[13]), Number(v[14]), Number(v[16])};
            AppendAdjustedPCell(row.cells, v, adjustment); row.cells.push_back(Number(v[17]));
            table.rows.push_back(std::move(row)); AddCommonWarnings(table, r);
            MeanComparisonRow descriptive;
            descriptive.rowId = "desc:" + r.response1; descriptive.label = r.response1;
            descriptive.variable = r.response1; descriptive.kind = MeanComparisonRowKind::Descriptive;
            descriptive.originalRowIndices = r.rowsUsed;
            descriptive.cells = {Number(v[0]), Number(v[1]), Number(v[2]), Number(v[3])};
            desc.rows.push_back(std::move(descriptive));
        }
        table.notes.push_back(signedRank
            ? "The Wilcoxon signed-rank test and its Hodges\u2013Lehmann location-shift interval were calculated in R."
            : "Differences are calculated as observed mean minus test value. Confidence intervals are " + level + " intervals for the mean difference.");
        if (adjustment != MultipleTestingAdjustment::None) table.notes.push_back(AdjustmentNote(adjustment, state.adjustmentFamilySize));
        state.tables.push_back(std::move(table)); state.tables.push_back(std::move(desc));
    } else if (state.analysisType == "independent_samples_t_test") {
        state.specification.kind = MeanComparisonKind::IndependentSamples;
        state.specification.groupOrderIds = {state.firstGroup, state.secondGroup};
        state.subtitle = MeanComparisonGroupingVariableLine(state.groupVariable) + " \u00b7 " + state.method +
            " \u00b7 Difference: " + state.firstGroup + " \u2212 " + state.secondGroup;
        MeanComparisonTable desc;
        desc.tableId = "group_descriptives";
        desc.title = "Group descriptives \u2014 grouped by " +
            (state.groupVariable.empty() ? std::string("\u2014") : state.groupVariable);
        desc.stubTitle = "Variable";
        desc.columns = {{"group", state.groupVariable.empty() ? "Group" : state.groupVariable, MeanComparisonCellFormat::Text}, {"n", "N", MeanComparisonCellFormat::Integer},
                        {"mean", "M", MeanComparisonCellFormat::Number}, {"sd", "SD", MeanComparisonCellFormat::Number},
                        {"se", "SE", MeanComparisonCellFormat::Number}};
        MeanComparisonTable tests;
        tests.pAdjustment = adjustment; tests.adjustmentFamilySize = state.adjustmentFamilySize;
        tests.tableId = "mean_comparisons"; tests.title = mannWhitney ? "Rank comparison" : "Mean comparisons"; tests.stubTitle = "Variable";
        tests.firstGroup = state.firstGroup; tests.secondGroup = state.secondGroup; tests.method = state.method;
        tests.columns = mannWhitney ? std::vector<MeanComparisonColumn>{
                         {"difference", "Location shift", MeanComparisonCellFormat::Number},
                         {"ci", level + " CI", MeanComparisonCellFormat::ConfidenceInterval},
                         {"statistic", "U", MeanComparisonCellFormat::Number},
                         {"p", "p", MeanComparisonCellFormat::PValue}}
            : std::vector<MeanComparisonColumn>{{"difference", "Mean difference", MeanComparisonCellFormat::Number},
                         {"se_difference", "SE difference", MeanComparisonCellFormat::Number},
                         {"ci", level + " CI", MeanComparisonCellFormat::ConfidenceInterval},
                         {"t", "t", MeanComparisonCellFormat::Number}, {"df", "df", MeanComparisonCellFormat::Number},
                         {"p", "p", MeanComparisonCellFormat::PValue}};
        AppendAdjustedPColumn(tests, adjustment);
        tests.columns.push_back({"effect", mannWhitney ? "r\u1d63\u1d66" : "Hedges\u2019 g", MeanComparisonCellFormat::Number});
        for (std::size_t i = 0; i < results.size(); ++i) {
            const auto &r = results[i]; const auto &v = r.values;
            for (std::size_t g = 0; g < r.groups.size(); ++g) {
                MeanComparisonRow row;
                row.rowId = "desc:" + r.response1 + "\x1f" + r.groups[g].label; row.label = r.response1;
                row.variable = r.response1; row.group = r.groups[g].label; row.kind = MeanComparisonRowKind::Descriptive;
                row.originalRowIndices = r.groups[g].rows;
                row.cells = {Text(r.groups[g].label), Number(r.groups[g].n), Number(r.groups[g].mean), Number(r.groups[g].sd), Number(r.groups[g].se)};
                desc.rows.push_back(std::move(row));
            }
            MeanComparisonRow row;
            row.rowId = "test:" + r.response1; row.label = r.response1; row.variable = r.response1;
            state.specification.dependentVariableIds.push_back(r.response1);
            row.originalRowIndices = r.rowsUsed;
            row.cells = mannWhitney
                ? std::vector<MeanComparisonCell>{Number(v[8]), CI(v[11], v[12]), Number(v[13]), Number(v[16])}
                : std::vector<MeanComparisonCell>{Number(v[8]), Number(v[10]), CI(v[11], v[12]), Number(v[13]), Number(v[14]), Number(v[16])};
            AppendAdjustedPCell(row.cells, v, adjustment); row.cells.push_back(Number(v[17]));
            tests.rows.push_back(std::move(row)); AddCommonWarnings(tests, r);
        }
        tests.notes.push_back(MeanComparisonGroupingVariableLine(state.groupVariable) + ". " +
            (mannWhitney ? "Hodges\u2013Lehmann location shifts" : "Mean differences") + " are calculated as " +
            state.firstGroup + " minus " + state.secondGroup + ". " + state.method + " was used in R.");
        if (adjustment != MultipleTestingAdjustment::None) tests.notes.push_back(AdjustmentNote(adjustment, state.adjustmentFamilySize));
        state.tables.push_back(std::move(desc)); state.tables.push_back(std::move(tests));
    } else if (state.analysisType == "paired_samples_t_test") {
        state.specification.kind = MeanComparisonKind::PairedSamples;
        state.subtitle = state.method + " \u00b7 Differences: first variable \u2212 second variable \u00b7 Alternative: " +
            AlternativeTitle(state.alternative) + " \u00b7 Confidence level: " + level;
        MeanComparisonTable table;
        table.pAdjustment = adjustment; table.adjustmentFamilySize = state.adjustmentFamilySize;
        table.tableId = "paired"; table.stubTitle = "Pair";
        table.columns = {{"n", "N", MeanComparisonCellFormat::Integer}, {"mean_first", "M first", MeanComparisonCellFormat::Number},
                         {"sd_first", "SD first", MeanComparisonCellFormat::Number, false}, {"mean_second", "M second", MeanComparisonCellFormat::Number},
                         {"sd_second", "SD second", MeanComparisonCellFormat::Number, false}};
        if (signedRank) {
            table.columns.insert(table.columns.end(), {
                {"difference", "Location shift", MeanComparisonCellFormat::Number},
                {"ci", level + " CI", MeanComparisonCellFormat::ConfidenceInterval},
                {"statistic", "V", MeanComparisonCellFormat::Number}, {"p", "p", MeanComparisonCellFormat::PValue}
            });
        } else {
            table.columns.insert(table.columns.end(), {
                {"difference", "Mean difference", MeanComparisonCellFormat::Number},
                {"sd_difference", "SD difference", MeanComparisonCellFormat::Number},
                {"se_difference", "SE difference", MeanComparisonCellFormat::Number},
                {"ci", level + " CI", MeanComparisonCellFormat::ConfidenceInterval}, {"t", "t", MeanComparisonCellFormat::Number},
                {"df", "df", MeanComparisonCellFormat::Number}, {"p", "p", MeanComparisonCellFormat::PValue}
            });
        }
        AppendAdjustedPColumn(table, adjustment);
        table.columns.push_back({"effect", signedRank ? "r\u1d63\u1d66" : "Cohen\u2019s dz", MeanComparisonCellFormat::Number});
        MeanComparisonTable desc;
        desc.tableId = "group_descriptives"; desc.title = "Pair descriptives"; desc.stubTitle = "Pair";
        desc.columns = {{"variable", "Variable", MeanComparisonCellFormat::Text},
                        {"n", "N", MeanComparisonCellFormat::Integer}, {"mean", "M", MeanComparisonCellFormat::Number},
                        {"sd", "SD", MeanComparisonCellFormat::Number}};
        for (std::size_t i = 0; i < results.size(); ++i) {
            const auto &r = results[i]; const auto &v = r.values;
            MeanComparisonRow row;
            row.rowId = "pair:" + r.response1 + "\x1f" + r.response2; row.label = r.response1 + " \u2212 " + r.response2;
            row.variable = r.response1; row.secondVariable = r.response2; row.originalRowIndices = r.rowsUsed;
            state.specification.pairs.push_back({row.rowId, r.response1, r.response2});
            row.cells = signedRank
                ? std::vector<MeanComparisonCell>{Number(v[0]), Number(v[1]), Number(v[2]), Number(v[5]), Number(v[6]),
                                                  Number(v[8]), CI(v[11], v[12]), Number(v[13]), Number(v[16])}
                : std::vector<MeanComparisonCell>{Number(v[0]), Number(v[1]), Number(v[2]), Number(v[5]), Number(v[6]), Number(v[8]),
                                                  Number(v[9]), Number(v[10]), CI(v[11], v[12]), Number(v[13]), Number(v[14]), Number(v[16])};
            AppendAdjustedPCell(row.cells, v, adjustment); row.cells.push_back(Number(v[17]));
            table.rows.push_back(std::move(row)); AddCommonWarnings(table, r);
            for (int variableIndex = 0; variableIndex < 2; ++variableIndex) {
                MeanComparisonRow descriptive;
                const std::string variable = variableIndex == 0 ? r.response1 : r.response2;
                descriptive.rowId = "desc:" + r.response1 + "\x1f" + r.response2 + "\x1f" + variable;
                descriptive.label = r.response1 + " \u2212 " + r.response2;
                descriptive.variable = variable; descriptive.secondVariable = r.response2;
                descriptive.kind = MeanComparisonRowKind::Descriptive; descriptive.originalRowIndices = r.rowsUsed;
                descriptive.cells = {Text(variable), Number(v[0]), Number(variableIndex == 0 ? v[1] : v[5]),
                                     Number(variableIndex == 0 ? v[2] : v[6])};
                desc.rows.push_back(std::move(descriptive));
            }
        }
        table.notes.push_back(signedRank
            ? "The paired Wilcoxon signed-rank test and its Hodges\u2013Lehmann location-shift interval were calculated in R."
            : "Mean differences are calculated as the first variable minus the second variable. Confidence intervals are " + level + " intervals for the mean difference.");
        if (adjustment != MultipleTestingAdjustment::None) table.notes.push_back(AdjustmentNote(adjustment, state.adjustmentFamilySize));
        state.tables.push_back(std::move(table)); state.tables.push_back(std::move(desc));
    } else if (state.analysisType == "one_way_anova") {
        state.specification.kind = MeanComparisonKind::OneWayAnova;
        state.subtitle = MeanComparisonGroupingVariableLine(state.groupVariable) + " \u00b7 " + state.method;
        MeanComparisonTable desc;
        desc.tableId = "group_descriptives";
        desc.title = "Group descriptives \u2014 grouped by " +
            (state.groupVariable.empty() ? std::string("\u2014") : state.groupVariable);
        desc.stubTitle = "Variable";
        desc.columns = {{"group", state.groupVariable.empty() ? "Group" : state.groupVariable, MeanComparisonCellFormat::Text}, {"n", "N", MeanComparisonCellFormat::Integer},
                        {"mean", "M", MeanComparisonCellFormat::Number}, {"sd", "SD", MeanComparisonCellFormat::Number},
                        {"se", "SE", MeanComparisonCellFormat::Number}};
        MeanComparisonTable tests;
        tests.pAdjustment = adjustment; tests.adjustmentFamilySize = state.adjustmentFamilySize;
        tests.tableId = "omnibus"; tests.title = "Omnibus tests"; tests.stubTitle = "Variable";
        tests.columns = kruskalWallis ? std::vector<MeanComparisonColumn>{
                         {"method", "Method", MeanComparisonCellFormat::Text}, {"statistic", "\u03c7\u00b2", MeanComparisonCellFormat::Number},
                         {"df1", "gl", MeanComparisonCellFormat::Number}, {"p", "p", MeanComparisonCellFormat::PValue}}
            : std::vector<MeanComparisonColumn>{{"method", "Method", MeanComparisonCellFormat::Text}, {"f", "F", MeanComparisonCellFormat::Number},
                         {"df1", "df1", MeanComparisonCellFormat::Number}, {"df2", "df2", MeanComparisonCellFormat::Number},
                         {"p", "p", MeanComparisonCellFormat::PValue}};
        AppendAdjustedPColumn(tests, adjustment);
        tests.columns.push_back({"effect", kruskalWallis ? "\u03b5\u00b2" : "\u03c9\u00b2", MeanComparisonCellFormat::Number});
        for (std::size_t i = 0; i < results.size(); ++i) {
            const auto &r = results[i]; const auto &v = r.values;
            for (std::size_t g = 0; g < r.groups.size(); ++g) {
                MeanComparisonRow row;
                row.rowId = "desc:" + r.response1 + "\x1f" + r.groups[g].label; row.label = r.response1;
                row.variable = r.response1; row.group = r.groups[g].label; row.kind = MeanComparisonRowKind::Descriptive;
                row.originalRowIndices = r.groups[g].rows;
                row.cells = {Text(r.groups[g].label), Number(r.groups[g].n), Number(r.groups[g].mean), Number(r.groups[g].sd), Number(r.groups[g].se)};
                desc.rows.push_back(std::move(row));
            }
            MeanComparisonRow row;
            row.rowId = "omnibus:" + r.response1; row.label = r.response1; row.variable = r.response1;
            state.specification.dependentVariableIds.push_back(r.response1);
            if (state.specification.groupOrderIds.empty()) {
                for (const auto &group : r.groups) state.specification.groupOrderIds.push_back(group.label);
            }
            row.originalRowIndices = r.rowsUsed;
            row.cells = kruskalWallis
                ? std::vector<MeanComparisonCell>{Text(r.method), Number(v[13]), Number(v[14]), Number(v[16])}
                : std::vector<MeanComparisonCell>{Text(r.method), Number(v[13]), Number(v[14]), Number(v[15]), Number(v[16])};
            AppendAdjustedPCell(row.cells, v, adjustment); row.cells.push_back(Number(v[17]));
            tests.rows.push_back(std::move(row)); AddCommonWarnings(tests, r);
        }
        tests.notes.push_back(MeanComparisonGroupingVariableLine(state.groupVariable) + ". " + state.method +
            " was calculated in R separately for each dependent variable. " +
            (kruskalWallis ? "\u03b5\u00b2" : "\u03c9\u00b2") + " is reported as the effect size.");
        if (adjustment != MultipleTestingAdjustment::None) tests.notes.push_back(AdjustmentNote(adjustment, state.adjustmentFamilySize));
        state.tables.push_back(std::move(desc)); state.tables.push_back(std::move(tests));
    } else {
        return fail("Unsupported compare-means analysis type.");
    }
    for (const auto &table : state.tables) {
        state.warnings.insert(state.warnings.end(), table.warnings.begin(), table.warnings.end());
    }
    return true;
}

std::string MeanComparisonTableText(const MeanComparisonTable &table,
                                    bool selectedOnly,
                                    std::size_t selectedRow)
{
    std::ostringstream out;
    out << table.stubTitle;
    for (const auto &column : table.columns) if (column.visible) out << '\t' << column.title;
    out << '\n';
    for (std::size_t r = 0; r < table.rows.size(); ++r) {
        if (selectedOnly && r != selectedRow) continue;
        out << table.rows[r].label;
        for (std::size_t c = 0; c < table.columns.size(); ++c) {
            if (!table.columns[c].visible) continue;
            out << '\t' << (c < table.rows[r].cells.size() ? MeanComparisonCellText(table.rows[r].cells[c], table.columns[c].format) : "");
        }
        out << '\n';
    }
    return out.str();
}

std::string MeanComparisonTableCSV(const MeanComparisonTable &table)
{
    auto key = [&](const MeanComparisonColumn &column) {
        const std::string &value = column.key;
        if (value == "n") return std::string("N");
        if (value == "mean") return std::string("Mean");
        if (value == "sd") return std::string("SD");
        if (value == "se") return std::string("SE");
        if (value == "test_value") return std::string("Test_Value");
        if (value == "difference") return std::string("Mean_Difference");
        if (value == "sd_difference") return std::string("SD_Difference");
        if (value == "se_difference") return std::string("SE_Difference");
        if (value == "mean_first") return std::string("Mean_First");
        if (value == "sd_first") return std::string("SD_First");
        if (value == "mean_second") return std::string("Mean_Second");
        if (value == "sd_second") return std::string("SD_Second");
        if (value == "group") return std::string("Group");
        if (value == "method") return std::string("Method");
        if (value == "p_adjusted") return std::string("p_adjusted");
        if (value == "effect") {
            if (table.tableId == "one_sample") return std::string("Cohens_d");
            if (table.tableId == "mean_comparisons") return std::string("Hedges_g");
            if (table.tableId == "paired") return std::string("Cohens_dz");
            return std::string("Effect_Size");
        }
        return value;
    };
    std::ostringstream out;
    const bool inferential = table.tableId == "one_sample" || table.tableId == "mean_comparisons" ||
        table.tableId == "paired" || table.tableId == "omnibus";
    const std::string metadataAfter = table.pAdjustment == MultipleTestingAdjustment::None
        ? "p" : "p_adjusted";
    if (table.tableId == "paired") out << "First_Variable,Second_Variable";
    else if (table.tableId == "mean_comparisons") out << "Variable,First_Group,Second_Group";
    else out << CSVCell(table.stubTitle);
    for (const auto &column : table.columns) {
        if (column.format == MeanComparisonCellFormat::ConfidenceInterval) {
            out << ",CI_Lower,CI_Upper";
        } else out << ',' << CSVCell(key(column));
        if (inferential && column.key == metadataAfter) {
            out << ",p_adjustment,adjustment_family_size";
        }
    }
    if (table.tableId == "mean_comparisons") out << ",Method";
    out << '\n';
    for (const auto &row : table.rows) {
        if (table.tableId == "paired") out << CSVCell(row.variable) << ',' << CSVCell(row.secondVariable);
        else if (table.tableId == "mean_comparisons") out << CSVCell(row.variable) << ',' << CSVCell(table.firstGroup) << ',' << CSVCell(table.secondGroup);
        else out << CSVCell(row.label);
        for (std::size_t c = 0; c < table.columns.size(); ++c) {
            const MeanComparisonCell empty;
            const MeanComparisonCell &cell = c < row.cells.size() ? row.cells[c] : empty;
            if (table.columns[c].format == MeanComparisonCellFormat::ConfidenceInterval) {
                out << ',' << (cell.numericValue ? CSVNumber(*cell.numericValue) : "")
                    << ',' << (cell.numericUpperValue ? CSVNumber(*cell.numericUpperValue) : "");
            } else if (cell.numericValue) out << ',' << CSVNumber(*cell.numericValue);
            else if (cell.textValue) out << ',' << CSVCell(*cell.textValue);
            else out << ',';
            if (inferential && table.columns[c].key == metadataAfter) {
                out << ',' << CSVCell(AdjustmentToken(table.pAdjustment))
                    << ',' << table.adjustmentFamilySize;
            }
        }
        if (table.tableId == "mean_comparisons") out << ',' << CSVCell(table.method);
        out << '\n';
    }
    return out.str();
}

std::string MeanComparisonAllTablesCSV(const MeanComparisonState &state)
{
    std::ostringstream out;
    for (std::size_t i = 0; i < state.tables.size(); ++i) {
        if (i) out << '\n';
        if (!state.tables[i].title.empty()) out << CSVCell(state.tables[i].title) << '\n';
        out << MeanComparisonTableCSV(state.tables[i]);
    }
    return out.str();
}

namespace {

std::size_t DisplayCharacterCount(const std::string &text)
{
    return static_cast<std::size_t>(std::count_if(text.begin(), text.end(), [](unsigned char byte) {
        return (byte & 0xc0) != 0x80;
    }));
}

double EstimatedTextWidth(const std::string &text)
{
    return 18.0 + 7.0 * static_cast<double>(DisplayCharacterCount(text));
}

} // namespace

double MeanComparisonPreferredStubWidth(const MeanComparisonTable &table)
{
    double width = EstimatedTextWidth(table.stubTitle);
    for (const MeanComparisonRow &row : table.rows) {
        width = std::max(width, EstimatedTextWidth(row.label));
    }
    return std::max(148.0, std::min(280.0, width));
}

std::vector<double> MeanComparisonPreferredColumnWidths(const MeanComparisonTable &table)
{
    std::vector<double> widths(table.columns.size(), 0.0);
    for (std::size_t columnIndex = 0; columnIndex < table.columns.size(); ++columnIndex) {
        const MeanComparisonColumn &column = table.columns[columnIndex];
        if (!column.visible) continue;
        double width = EstimatedTextWidth(column.title);
        for (const MeanComparisonRow &row : table.rows) {
            if (columnIndex >= row.cells.size()) continue;
            width = std::max(width, EstimatedTextWidth(
                MeanComparisonCellText(row.cells[columnIndex], column.format)));
        }
        const double minimum = column.format == MeanComparisonCellFormat::ConfidenceInterval ? 118.0 :
            (column.format == MeanComparisonCellFormat::Text ? 70.0 : 58.0);
        // Confidence intervals contain two signed values plus punctuation.  Keep
        // their measured width instead of forcing them into the same narrow cap
        // as scalar statistics; the platform view can scroll horizontally.
        const double maximum = column.format == MeanComparisonCellFormat::ConfidenceInterval ? 420.0 : 280.0;
        widths[columnIndex] = std::max(minimum, std::min(maximum, width));
    }
    return widths;
}

double MeanComparisonPreferredWidth(const MeanComparisonState &state)
{
    double widest = 520.0;
    for (const auto &table : state.tables) {
        double width = MeanComparisonPreferredStubWidth(table);
        const std::vector<double> columns = MeanComparisonPreferredColumnWidths(table);
        for (double column : columns) width += column;
        widest = std::max(widest, width);
    }
    return std::min(2000.0, widest + 36.0);
}

double MeanComparisonPreferredHeight(const MeanComparisonState &state)
{
    double height = 112.0;
    for (const auto &table : state.tables) {
        height += (table.title.empty() ? 0.0 : 24.0) + 30.0 + 25.0 * table.rows.size();
        height += 18.0 * (table.notes.size() + table.warnings.size()) + 18.0;
    }
    return std::min(820.0, std::max(250.0, height));
}

} // namespace core
} // namespace rlispstat
