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
    std::string testFamily;
    std::string responseType;
    std::string eventLevel;
    double nullValue = NAN;
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
    for (const std::string &message : result.warnings) {
        // R transports effect-size methodology with its warnings.  It is
        // explanatory text, so show it once as a note rather than once per
        // response as a red warning.
        auto &destination = message.rfind("Hedges' g uses pooled SD", 0) == 0
            ? table.notes : table.warnings;
        if (std::find(destination.begin(), destination.end(), message) == destination.end())
            destination.push_back(message);
    }
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
        // Retain the adjusted value for CSV and provenance, but avoid a second
        // identical p column in the visual table when the family has one test.
        table.columns.push_back({"p_adjusted", MeanComparisonAdjustmentName(adjustment) + " p",
            MeanComparisonCellFormat::PValue, table.adjustmentFamilySize != 1});
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
    if (familySize == 1) return MeanComparisonAdjustmentName(adjustment) +
        " adjustment was requested; with one test, p is unchanged.";
    return "P values were adjusted with " + MeanComparisonAdjustmentName(adjustment) +
        " across " + std::to_string(familySize) + " tests.";
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

std::vector<std::string> MeanComparisonDefaultIndependentGroupOrder(
    const std::vector<std::string> &availableLevels)
{
    if (availableLevels.size() < 2) return {};
    return {availableLevels[0], availableLevels[1]};
}

std::vector<std::string> MeanComparisonIndependentGroupOrderForReference(
    const std::vector<std::string> &availableLevels,
    const std::string &referenceLevel)
{
    if (availableLevels.size() != 2) return {};
    const auto reference = std::find(
        availableLevels.begin(), availableLevels.end(), referenceLevel);
    if (reference == availableLevels.end()) return {};
    const std::string &other = reference == availableLevels.begin()
        ? availableLevels[1] : availableLevels[0];
    return {other, referenceLevel};
}

bool MeanComparisonIndependentGroupOrderIsValid(
    const std::vector<std::string> &groupOrder,
    const std::vector<std::string> &availableLevels)
{
    if (groupOrder.size() != 2 || groupOrder[0] == groupOrder[1]) return false;
    return std::all_of(groupOrder.begin(), groupOrder.end(),
        [&](const std::string &level) {
            return std::find(availableLevels.begin(), availableLevels.end(), level) !=
                availableLevels.end();
        });
}

std::vector<MeanComparisonPairedForestRow> MeanComparisonPairedForestRows(
    const MeanComparisonState &state)
{
    std::vector<MeanComparisonPairedForestRow> out;
    for (const MeanComparisonTable &table : state.tables) {
        if (table.tableId != "paired" && table.tableId != "paired_t" &&
            table.tableId != "paired_wilcoxon") continue;
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

static std::vector<std::string> BinaryResponseLevels(const DataColumn &column)
{
    const auto defined = DataColumnFactorLevels(column);
    return defined.size() == 2 ? defined : DataColumnObservedLevels(column);
}

bool MeanComparisonBinaryResponse(const DataColumn &column)
{
    return VariableTypeIsCategorical(column.type) && BinaryResponseLevels(column).size() == 2;
}

bool MeanComparisonBinaryPairCompatible(const DataColumn &first, const DataColumn &second)
{
    if (!MeanComparisonBinaryResponse(first) || !MeanComparisonBinaryResponse(second)) return false;
    auto left = BinaryResponseLevels(first), right = BinaryResponseLevels(second);
    std::sort(left.begin(), left.end());
    std::sort(right.begin(), right.end());
    return left == right;
}

bool MeanComparisonResponseTypeAllowed(const std::string &analysisType, const std::string &type,
    bool binaryCategorical, bool multipleImputation)
{
    const auto normalized = NormalizeVariableType(type);
    if (multipleImputation || analysisType == "one_way_anova") return normalized == "numeric";
    return normalized == "numeric" || normalized == "ordered" || binaryCategorical;
}

PublicationTableSpec MeanComparisonPublicationTable(const MeanComparisonTable &source)
{
    PublicationTableSpec table;
    table.title = source.title;
    table.stubColumns = {0};
    table.columns.push_back({"variable", source.stubTitle, "", -1, false});
    for (const auto &column : source.columns) {
        if (!column.visible) continue;
        const int decimals = column.format == MeanComparisonCellFormat::Integer ? 0 :
            (column.format == MeanComparisonCellFormat::Number ? 2 : -1);
        table.columns.push_back({column.key, column.title, "", decimals,
            column.format == MeanComparisonCellFormat::PValue});
    }
    auto text = [](const std::string &label) {
        PublicationValue value; value.kind = PublicationValueKind::Text; value.text = label; return value;
    };
    for (const auto &row : source.rows) {
        if (row.kind != MeanComparisonRowKind::GroupHeader &&
            std::none_of(row.cells.begin(), row.cells.end(), [](const auto &cell) {
                return !cell.notApplicable && (cell.numericValue || cell.textValue);
            })) continue;
        const bool child = row.kind == MeanComparisonRowKind::Descriptive && !row.group.empty();
        std::vector<PublicationValue> cells{text((child ? "    " : "") + row.label)};
        for (std::size_t i=0; i<source.columns.size(); ++i) {
            const auto &column = source.columns[i];
            if (!column.visible) continue;
            PublicationValue value;
            if (i < row.cells.size() && !row.cells[i].notApplicable) {
                const auto &cell = row.cells[i];
                if (cell.numericValue && column.format != MeanComparisonCellFormat::ConfidenceInterval) {
                    value.kind = PublicationValueKind::Number; value.number = *cell.numericValue;
                } else if (cell.numericValue && cell.numericUpperValue &&
                           column.format == MeanComparisonCellFormat::ConfidenceInterval) {
                    std::ostringstream interval;
                    interval << std::fixed << std::setprecision(2) << "[" << *cell.numericValue
                             << ", " << *cell.numericUpperValue << "]";
                    value = text(interval.str());
                } else if (cell.numericValue || cell.textValue) {
                    value = text(MeanComparisonCellText(cell, column.format));
                }
            }
            cells.push_back(value);
        }
        table.rows.push_back(std::move(cells));
    }
    table.footnotes = source.notes;
    table.footnotes.insert(table.footnotes.end(), source.warnings.begin(), source.warnings.end());
    return table;
}

bool ParseMeanComparisonBatch(const std::vector<std::string> &args,
                              MeanComparisonState &state,
                              std::string *error)
{
    auto fail = [&](const std::string &message) {
        if (error) *error = message;
        return false;
    };
    const bool version4 = !args.empty() && args[0] == "COMPARE_MEANS_BATCH_V4";
    const bool version3 = version4 || (!args.empty() && args[0] == "COMPARE_MEANS_BATCH_V3");
    const bool version2 = version3 || (!args.empty() && args[0] == "COMPARE_MEANS_BATCH_V2");
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
    if (version4) {
        if (cursor + 3 > args.size()) return fail("Truncated multiple-imputation metadata.");
        state.multipleImputation = args[cursor++] == "multiple_imputation";
        const double imputationCount = WireNumber(args[cursor++]);
        if (std::isfinite(imputationCount) && imputationCount > 0) {
            state.imputationCount = static_cast<std::size_t>(std::llround(imputationCount));
        }
        state.poolingMethod = args[cursor++];
    }
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
        if (version3) {
            if (cursor + 4 > args.size()) return fail("Truncated one-sample test metadata.");
            result.testFamily = args[cursor++];
            result.responseType = args[cursor++];
            result.eventLevel = args[cursor++];
            result.nullValue = WireNumber(args[cursor++]);
        }
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
        if (version3) {
            MeanComparisonTable tTests;
            tTests.tableId = "one_sample_t"; tTests.stubTitle = "Variable";
            tTests.pAdjustment = adjustment; tTests.adjustmentFamilySize = state.adjustmentFamilySize;
            tTests.columns = {
                {"n", "N", MeanComparisonCellFormat::Integer}, {"mean", "M", MeanComparisonCellFormat::Number},
                {"test_value", "μ₀", MeanComparisonCellFormat::Number},
                {"difference", "\u0394M", MeanComparisonCellFormat::Number},
                {"se_difference", "SE\u0394M", MeanComparisonCellFormat::Number},
                {"ci", level + " CI", MeanComparisonCellFormat::ConfidenceInterval},
                {"t", "t", MeanComparisonCellFormat::Number}, {"df", "df", MeanComparisonCellFormat::Number},
                {"p", "p", MeanComparisonCellFormat::PValue}
            };
            AppendAdjustedPColumn(tTests, adjustment);
            tTests.columns.push_back({"effect", "d", MeanComparisonCellFormat::Number});

            MeanComparisonTable rankTests;
            rankTests.tableId = "one_sample_wilcoxon"; rankTests.title = "Wilcoxon signed-rank tests";
            rankTests.stubTitle = "Variable"; rankTests.pAdjustment = adjustment;
            rankTests.adjustmentFamilySize = state.adjustmentFamilySize;
            rankTests.columns = {
                {"n", "N", MeanComparisonCellFormat::Integer}, {"mean", "Mdn", MeanComparisonCellFormat::Number},
                {"test_value", "θ₀", MeanComparisonCellFormat::Number},
                {"difference", "\u0394HL", MeanComparisonCellFormat::Number},
                {"ci", level + " CI", MeanComparisonCellFormat::ConfidenceInterval},
                {"statistic", "V", MeanComparisonCellFormat::Number},
                {"p", "p", MeanComparisonCellFormat::PValue}
            };
            AppendAdjustedPColumn(rankTests, adjustment);
            rankTests.columns.push_back({"effect", "r_rb", MeanComparisonCellFormat::Number});

            MeanComparisonTable binomialTests;
            binomialTests.tableId = "one_sample_binomial"; binomialTests.title = "Binomial tests";
            binomialTests.stubTitle = "Variable"; binomialTests.pAdjustment = adjustment;
            binomialTests.adjustmentFamilySize = state.adjustmentFamilySize;
            binomialTests.columns = {
                {"event", "Event category", MeanComparisonCellFormat::Text},
                {"n", "N", MeanComparisonCellFormat::Integer},
                {"successes", "x", MeanComparisonCellFormat::Integer},
                {"proportion", "p̂", MeanComparisonCellFormat::Number},
                {"test_value", "p₀", MeanComparisonCellFormat::Number},
                {"ci", level + " CI(p)", MeanComparisonCellFormat::ConfidenceInterval},
                {"p", "p", MeanComparisonCellFormat::PValue}
            };
            AppendAdjustedPColumn(binomialTests, adjustment);

            MeanComparisonTable desc;
            desc.tableId = "group_descriptives"; desc.title = "Descriptives"; desc.stubTitle = "Variable";
            desc.columns = {{"n", "N", MeanComparisonCellFormat::Integer}, {"mean", "M", MeanComparisonCellFormat::Number},
                            {"sd", "SD", MeanComparisonCellFormat::Number}, {"se", "SE", MeanComparisonCellFormat::Number}};
            MeanComparisonTable rankDesc;
            rankDesc.tableId = "rank_descriptives"; rankDesc.title = "Ordinal descriptives";
            rankDesc.stubTitle = "Variable";
            rankDesc.columns = {{"n", "N", MeanComparisonCellFormat::Integer},
                                {"median", "Mdn", MeanComparisonCellFormat::Number},
                                {"iqr", "IQR", MeanComparisonCellFormat::Number}};

            for (const auto &r : results) {
                const auto &v = r.values;
                const double nullValue = std::isfinite(r.nullValue) ? r.nullValue : state.testValue;
                state.specification.dependentVariableIds.push_back(r.response1);
                state.specification.testValues[r.response1] = nullValue;
                MeanComparisonRow row;
                row.rowId = "one_sample:" + r.response1; row.label = r.response1;
                row.variable = r.response1; row.originalRowIndices = r.rowsUsed;
                if (r.testFamily == "binomial") {
                    row.cells = {Text(r.eventLevel), Number(v[0]), Number(v[13]), Number(v[1]),
                                 Number(nullValue), CI(v[11], v[12]), Number(v[16])};
                    AppendAdjustedPCell(row.cells, v, adjustment);
                    binomialTests.rows.push_back(std::move(row)); AddCommonWarnings(binomialTests, r);
                    continue;
                }
                MeanComparisonRow descriptive;
                descriptive.rowId = "desc:" + r.response1; descriptive.label = r.response1;
                descriptive.variable = r.response1; descriptive.kind = MeanComparisonRowKind::Descriptive;
                descriptive.originalRowIndices = r.rowsUsed;
                descriptive.cells = r.testFamily == "wilcoxon"
                    ? std::vector<MeanComparisonCell>{Number(v[0]), Number(v[1]), Number(v[2])}
                    : std::vector<MeanComparisonCell>{Number(v[0]), Number(v[1]), Number(v[2]), Number(v[3])};
                (r.testFamily == "wilcoxon" ? rankDesc : desc).rows.push_back(std::move(descriptive));
                if (r.testFamily == "wilcoxon") {
                    row.cells = {Number(v[0]), Number(v[1]), Number(nullValue), Number(v[8]),
                                 CI(v[11], v[12]), Number(v[13]), Number(v[16])};
                    AppendAdjustedPCell(row.cells, v, adjustment); row.cells.push_back(Number(v[17]));
                    rankTests.rows.push_back(std::move(row)); AddCommonWarnings(rankTests, r);
                } else {
                    row.cells = {Number(v[0]), Number(v[1]), Number(nullValue), Number(v[8]), Number(v[10]),
                                 CI(v[11], v[12]), Number(v[13]), Number(v[14]), Number(v[16])};
                    AppendAdjustedPCell(row.cells, v, adjustment); row.cells.push_back(Number(v[17]));
                    tTests.rows.push_back(std::move(row)); AddCommonWarnings(tTests, r);
                }
            }
            const bool hasTTests = !tTests.rows.empty();
            const bool hasRankTests = !rankTests.rows.empty();
            const bool hasBinomialTests = !binomialTests.rows.empty();
            const int inferentialFamilies = (hasTTests ? 1 : 0) +
                (hasRankTests ? 1 : 0) + (hasBinomialTests ? 1 : 0);
            if (!tTests.rows.empty()) {
                if (inferentialFamilies > 1) tTests.title = "One-sample t tests";
                tTests.notes.push_back("Differences are calculated as observed mean minus the test value. Confidence intervals are " + level + " intervals for the mean difference.");
                state.tables.push_back(std::move(tTests));
            }
            if (!rankTests.rows.empty()) {
                rankTests.notes.push_back("Wilcoxon signed-rank tests and Hodges\u2013Lehmann location-shift intervals were calculated in R.");
                state.tables.push_back(std::move(rankTests));
            }
            if (!binomialTests.rows.empty()) {
                binomialTests.notes.push_back("Exact binomial tests use the displayed event category and test proportion.");
                state.tables.push_back(std::move(binomialTests));
            }
            if (adjustment != MultipleTestingAdjustment::None && !state.tables.empty())
                state.tables.back().notes.push_back(AdjustmentNote(adjustment, state.adjustmentFamilySize));
            if (!desc.rows.empty()) state.tables.push_back(std::move(desc));
            if (!rankDesc.rows.empty()) state.tables.push_back(std::move(rankDesc));
            state.method = inferentialFamilies > 1 ? "Automatic by variable type" :
                (hasBinomialTests ? "Exact binomial test" :
                 hasRankTests ? "Wilcoxon signed-rank test" : "One-sample t-test");
            state.subtitle = "One-sample tests \u00b7 Default test value: " +
                FormatModelNumberOrDash(state.testValue) + " \u00b7 Alternative: " +
                AlternativeTitle(state.alternative) + " \u00b7 Confidence level: " + level;
        } else {
        MeanComparisonTable table;
        table.pAdjustment = adjustment; table.adjustmentFamilySize = state.adjustmentFamilySize;
        table.tableId = "one_sample";
        table.stubTitle = "Variable";
        table.columns = signedRank ? std::vector<MeanComparisonColumn>{
            {"n", "N", MeanComparisonCellFormat::Integer}, {"mean", "Mdn", MeanComparisonCellFormat::Number},
            {"sd", "IQR", MeanComparisonCellFormat::Number, false}, {"test_value", "θ₀", MeanComparisonCellFormat::Number},
            {"difference", "\u0394HL", MeanComparisonCellFormat::Number},
            {"ci", level + " CI", MeanComparisonCellFormat::ConfidenceInterval},
            {"statistic", "V", MeanComparisonCellFormat::Number}, {"p", "p", MeanComparisonCellFormat::PValue}
        } : std::vector<MeanComparisonColumn>{
            {"n", "N", MeanComparisonCellFormat::Integer}, {"mean", "M", MeanComparisonCellFormat::Number},
            {"sd", "SD", MeanComparisonCellFormat::Number, false}, {"test_value", "μ₀", MeanComparisonCellFormat::Number},
            {"difference", "\u0394M", MeanComparisonCellFormat::Number},
            {"se_difference", "SE\u0394M", MeanComparisonCellFormat::Number},
            {"ci", level + " CI", MeanComparisonCellFormat::ConfidenceInterval},
            {"t", "t", MeanComparisonCellFormat::Number}, {"df", "df", MeanComparisonCellFormat::Number},
            {"p", "p", MeanComparisonCellFormat::PValue}
        };
        AppendAdjustedPColumn(table, adjustment);
        table.columns.push_back({"effect", signedRank ? "r_rb" : "d", MeanComparisonCellFormat::Number});
        MeanComparisonTable desc;
        desc.tableId = "group_descriptives"; desc.title = "Descriptives"; desc.stubTitle = "Variable";
        desc.columns = signedRank ? std::vector<MeanComparisonColumn>{
            {"n", "N", MeanComparisonCellFormat::Integer}, {"mean", "Mdn", MeanComparisonCellFormat::Number},
            {"sd", "IQR", MeanComparisonCellFormat::Number}}
            : std::vector<MeanComparisonColumn>{
            {"n", "N", MeanComparisonCellFormat::Integer}, {"mean", "M", MeanComparisonCellFormat::Number},
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
            descriptive.cells = signedRank
                ? std::vector<MeanComparisonCell>{Number(v[0]), Number(v[1]), Number(v[2])}
                : std::vector<MeanComparisonCell>{Number(v[0]), Number(v[1]), Number(v[2]), Number(v[3])};
            desc.rows.push_back(std::move(descriptive));
        }
        table.notes.push_back(signedRank
            ? "The Wilcoxon signed-rank test and its Hodges\u2013Lehmann location-shift interval were calculated in R."
            : "Differences are calculated as observed mean minus test value. Confidence intervals are " + level + " intervals for the mean difference.");
        if (adjustment != MultipleTestingAdjustment::None) table.notes.push_back(AdjustmentNote(adjustment, state.adjustmentFamilySize));
        state.tables.push_back(std::move(table)); state.tables.push_back(std::move(desc));
        }
    } else if (state.analysisType == "independent_samples_t_test") {
        state.specification.kind = MeanComparisonKind::IndependentSamples;
        state.specification.groupOrderIds = {state.firstGroup, state.secondGroup};
        state.subtitle = MeanComparisonGroupingVariableLine(state.groupVariable) + " \u00b7 " + state.method +
            " \u00b7 Difference: " + state.firstGroup + " \u2212 " + state.secondGroup;
        if (version3) {
            MeanComparisonTable tTests;
            tTests.tableId = "independent_t"; tTests.title = "Mean differences";
            tTests.stubTitle = "Variable"; tTests.pAdjustment = adjustment;
            tTests.adjustmentFamilySize = state.adjustmentFamilySize;
            tTests.firstGroup = state.firstGroup; tTests.secondGroup = state.secondGroup;
            tTests.method = state.method;
            tTests.columns = {
                {"difference", "Mean difference", MeanComparisonCellFormat::Number},
                {"se_difference", "SE", MeanComparisonCellFormat::Number},
                {"ci", level + " CI", MeanComparisonCellFormat::ConfidenceInterval},
                {"t", "t", MeanComparisonCellFormat::Number},
                {"df", "df", MeanComparisonCellFormat::Number},
                {"p", "p", MeanComparisonCellFormat::PValue}
            };
            AppendAdjustedPColumn(tTests, adjustment);
            tTests.columns.push_back({"effect", "Hedges' g", MeanComparisonCellFormat::Number});

            MeanComparisonTable rankTests;
            rankTests.tableId = "independent_mann_whitney";
            rankTests.title = "Rank comparisons"; rankTests.stubTitle = "Variable";
            rankTests.pAdjustment = adjustment;
            rankTests.adjustmentFamilySize = state.adjustmentFamilySize;
            rankTests.firstGroup = state.firstGroup; rankTests.secondGroup = state.secondGroup;
            rankTests.method = "Mann\u2013Whitney U test";
            rankTests.columns = {
                {"difference", "\u0394HL", MeanComparisonCellFormat::Number},
                {"ci", level + " CI", MeanComparisonCellFormat::ConfidenceInterval},
                {"statistic", "U", MeanComparisonCellFormat::Number},
                {"p", "p", MeanComparisonCellFormat::PValue}
            };
            AppendAdjustedPColumn(rankTests, adjustment);
            rankTests.columns.push_back({"effect", "r_rb", MeanComparisonCellFormat::Number});

            MeanComparisonTable proportionTests;
            proportionTests.tableId = "independent_proportions";
            proportionTests.title = "Proportion comparisons";
            proportionTests.stubTitle = "Variable";
            proportionTests.pAdjustment = adjustment;
            proportionTests.adjustmentFamilySize = state.adjustmentFamilySize;
            proportionTests.firstGroup = state.firstGroup;
            proportionTests.secondGroup = state.secondGroup;
            proportionTests.method = "Two-sample proportion test";
            proportionTests.columns = {
                {"event", "Event category", MeanComparisonCellFormat::Text},
                {"first_group", state.firstGroup + " event / N", MeanComparisonCellFormat::Text},
                {"second_group", state.secondGroup + " event / N", MeanComparisonCellFormat::Text},
                {"difference", "\u0394p\u0302", MeanComparisonCellFormat::Number},
                {"se_difference", "SE\u0394p\u0302", MeanComparisonCellFormat::Number},
                {"ci", level + " CI", MeanComparisonCellFormat::ConfidenceInterval},
                {"statistic", "\u03c7\u00b2", MeanComparisonCellFormat::Number},
                {"df", "df", MeanComparisonCellFormat::Number},
                {"p", "p", MeanComparisonCellFormat::PValue}
            };
            AppendAdjustedPColumn(proportionTests, adjustment);
            proportionTests.columns.push_back({"effect", "h", MeanComparisonCellFormat::Number});

            MeanComparisonTable desc;
            desc.tableId = "group_descriptives";
            desc.title = "Group descriptives \u2014 grouped by " +
                (state.groupVariable.empty() ? std::string("\u2014") : state.groupVariable);
            desc.stubTitle = "Variable";
            desc.columns = {
                {"group", state.groupVariable.empty() ? "Group" : state.groupVariable, MeanComparisonCellFormat::Text},
                {"n", "N", MeanComparisonCellFormat::Integer},
                {"mean", "M", MeanComparisonCellFormat::Number},
                {"sd", "SD", MeanComparisonCellFormat::Number},
                {"se", "SE", MeanComparisonCellFormat::Number}
            };
            MeanComparisonTable rankDesc;
            rankDesc.tableId = "rank_descriptives";
            rankDesc.title = "Rank descriptives \u2014 grouped by " +
                (state.groupVariable.empty() ? std::string("\u2014") : state.groupVariable);
            rankDesc.stubTitle = "Variable";
            rankDesc.columns = {
                {"group", state.groupVariable.empty() ? "Group" : state.groupVariable, MeanComparisonCellFormat::Text},
                {"n", "N", MeanComparisonCellFormat::Integer},
                {"median", "Mdn", MeanComparisonCellFormat::Number},
                {"iqr", "IQR", MeanComparisonCellFormat::Number}
            };

            auto groupCountText = [](double n, double proportion) {
                if (!std::isfinite(n) || !std::isfinite(proportion)) return std::string("\u2014");
                return std::to_string(static_cast<long long>(std::llround(n * proportion))) + " / " +
                    std::to_string(static_cast<long long>(std::llround(n))) + " (" +
                    FormatModelNumberOrDash(proportion) + ")";
            };
            for (const auto &r : results) {
                const auto &v = r.values;
                state.specification.dependentVariableIds.push_back(r.response1);
                const bool proportion = r.testFamily == "proportion";
                const bool rank = r.testFamily == "mann_whitney";
                if (!proportion) {
                    auto &target = rank ? rankDesc : desc;
                    for (const auto &group : r.groups) {
                        MeanComparisonRow descriptive;
                        descriptive.rowId = "desc:" + r.response1 + "\x1f" + group.label;
                        descriptive.label = r.response1; descriptive.variable = r.response1;
                        descriptive.group = group.label; descriptive.kind = MeanComparisonRowKind::Descriptive;
                        descriptive.originalRowIndices = group.rows;
                        descriptive.cells = rank
                            ? std::vector<MeanComparisonCell>{Text(group.label), Number(group.n),
                                  Number(group.mean), Number(group.sd)}
                            : std::vector<MeanComparisonCell>{Text(group.label), Number(group.n),
                                  Number(group.mean), Number(group.sd), Number(group.se)};
                        target.rows.push_back(std::move(descriptive));
                    }
                }
                MeanComparisonRow row;
                row.rowId = "test:" + r.response1; row.label = r.response1;
                row.variable = r.response1; row.originalRowIndices = r.rowsUsed;
                if (proportion) {
                    const NativeResult::Group emptyGroup{};
                    const auto &first = r.groups.size() > 0 ? r.groups[0] : emptyGroup;
                    const auto &second = r.groups.size() > 1 ? r.groups[1] : emptyGroup;
                    row.cells = {
                        Text(r.eventLevel),
                        Text(groupCountText(first.n, first.mean)),
                        Text(groupCountText(second.n, second.mean)),
                        Number(v[8]), Number(v[10]), CI(v[11], v[12]),
                        Number(v[13]), Number(v[14]), Number(v[16])
                    };
                    AppendAdjustedPCell(row.cells, v, adjustment);
                    row.cells.push_back(Number(v[17]));
                    proportionTests.rows.push_back(std::move(row));
                    AddCommonWarnings(proportionTests, r);
                } else if (rank) {
                    row.cells = {Number(v[8]), CI(v[11], v[12]), Number(v[13]), Number(v[16])};
                    AppendAdjustedPCell(row.cells, v, adjustment);
                    row.cells.push_back(Number(v[17]));
                    rankTests.rows.push_back(std::move(row)); AddCommonWarnings(rankTests, r);
                } else {
                    row.cells = {Number(v[8]), Number(v[10]), CI(v[11], v[12]),
                                 Number(v[13]), Number(v[14]), Number(v[16])};
                    AppendAdjustedPCell(row.cells, v, adjustment);
                    row.cells.push_back(Number(v[17]));
                    tTests.rows.push_back(std::move(row)); AddCommonWarnings(tTests, r);
                }
            }
            const int inferentialFamilies = (!tTests.rows.empty() ? 1 : 0) +
                (!rankTests.rows.empty() ? 1 : 0) + (!proportionTests.rows.empty() ? 1 : 0);
            if (!tTests.rows.empty()) {
                tTests.notes.insert(tTests.notes.begin(), "Mean difference = " + state.firstGroup +
                    " \u2212 " + state.secondGroup + "; negative values mean a lower mean in " +
                    state.firstGroup + ".");
                if (adjustment != MultipleTestingAdjustment::None)
                    tTests.notes.insert(tTests.notes.begin() + 1,
                        AdjustmentNote(adjustment, state.adjustmentFamilySize));
                state.tables.push_back(std::move(tTests));
            }
            if (!rankTests.rows.empty()) {
                rankTests.notes.push_back("Mann\u2013Whitney tests, Hodges\u2013Lehmann location shifts, and rank-biserial correlations were calculated in R.");
                if (adjustment != MultipleTestingAdjustment::None)
                    rankTests.notes.push_back(AdjustmentNote(adjustment, state.adjustmentFamilySize));
                state.tables.push_back(std::move(rankTests));
            }
            if (!proportionTests.rows.empty()) {
                proportionTests.notes.push_back("Two-sample score tests without continuity correction compare the displayed event proportions. Differences are " +
                    state.firstGroup + " minus " + state.secondGroup + ".");
                if (adjustment != MultipleTestingAdjustment::None)
                    proportionTests.notes.push_back(AdjustmentNote(adjustment, state.adjustmentFamilySize));
                state.tables.push_back(std::move(proportionTests));
            }
            if (!desc.rows.empty()) state.tables.push_back(std::move(desc));
            if (!rankDesc.rows.empty()) state.tables.push_back(std::move(rankDesc));
        } else {
        MeanComparisonTable desc;
        desc.tableId = "group_descriptives";
        desc.title = "Group descriptives \u2014 grouped by " +
            (state.groupVariable.empty() ? std::string("\u2014") : state.groupVariable);
        desc.stubTitle = "Variable";
        desc.columns = mannWhitney ? std::vector<MeanComparisonColumn>{
            {"group", state.groupVariable.empty() ? "Group" : state.groupVariable, MeanComparisonCellFormat::Text},
            {"n", "N", MeanComparisonCellFormat::Integer}, {"mean", "Mdn", MeanComparisonCellFormat::Number},
            {"sd", "IQR", MeanComparisonCellFormat::Number}}
            : std::vector<MeanComparisonColumn>{
            {"group", state.groupVariable.empty() ? "Group" : state.groupVariable, MeanComparisonCellFormat::Text},
            {"n", "N", MeanComparisonCellFormat::Integer}, {"mean", "M", MeanComparisonCellFormat::Number},
            {"sd", "SD", MeanComparisonCellFormat::Number}, {"se", "SE", MeanComparisonCellFormat::Number}};
        MeanComparisonTable tests;
        tests.pAdjustment = adjustment; tests.adjustmentFamilySize = state.adjustmentFamilySize;
        tests.tableId = "mean_comparisons"; tests.title = mannWhitney ? "Rank comparison" : "Mean differences"; tests.stubTitle = "Variable";
        tests.firstGroup = state.firstGroup; tests.secondGroup = state.secondGroup; tests.method = state.method;
        tests.columns = mannWhitney ? std::vector<MeanComparisonColumn>{
                         {"difference", "\u0394HL", MeanComparisonCellFormat::Number},
                         {"ci", level + " CI", MeanComparisonCellFormat::ConfidenceInterval},
                         {"statistic", "U", MeanComparisonCellFormat::Number},
                         {"p", "p", MeanComparisonCellFormat::PValue}}
            : std::vector<MeanComparisonColumn>{{"difference", "Mean difference", MeanComparisonCellFormat::Number},
                         {"se_difference", "SE", MeanComparisonCellFormat::Number},
                         {"ci", level + " CI", MeanComparisonCellFormat::ConfidenceInterval},
                         {"t", "t", MeanComparisonCellFormat::Number}, {"df", "df", MeanComparisonCellFormat::Number},
                         {"p", "p", MeanComparisonCellFormat::PValue}};
        AppendAdjustedPColumn(tests, adjustment);
        tests.columns.push_back({"effect", mannWhitney ? "Rank-biserial r" : "Hedges' g", MeanComparisonCellFormat::Number});
        for (std::size_t i = 0; i < results.size(); ++i) {
            const auto &r = results[i]; const auto &v = r.values;
            for (std::size_t g = 0; g < r.groups.size(); ++g) {
                MeanComparisonRow row;
                row.rowId = "desc:" + r.response1 + "\x1f" + r.groups[g].label; row.label = r.response1;
                row.variable = r.response1; row.group = r.groups[g].label; row.kind = MeanComparisonRowKind::Descriptive;
                row.originalRowIndices = r.groups[g].rows;
                row.cells = mannWhitney
                    ? std::vector<MeanComparisonCell>{Text(r.groups[g].label), Number(r.groups[g].n),
                          Number(r.groups[g].mean), Number(r.groups[g].sd)}
                    : std::vector<MeanComparisonCell>{Text(r.groups[g].label), Number(r.groups[g].n),
                          Number(r.groups[g].mean), Number(r.groups[g].sd), Number(r.groups[g].se)};
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
        }
    } else if (state.analysisType == "paired_samples_t_test") {
        state.specification.kind = MeanComparisonKind::PairedSamples;
        state.subtitle = state.method + " \u00b7 Differences: first variable \u2212 second variable \u00b7 Alternative: " +
            AlternativeTitle(state.alternative) + " \u00b7 Confidence level: " + level;
        if (version3) {
        MeanComparisonTable tTests;
        tTests.tableId = "paired_t"; tTests.title = "Paired t tests"; tTests.stubTitle = "Pair";
        tTests.pAdjustment = adjustment; tTests.adjustmentFamilySize = state.adjustmentFamilySize;
        tTests.columns = {
            {"n", "N", MeanComparisonCellFormat::Integer},
            {"mean_first", "M\u2081", MeanComparisonCellFormat::Number},
            {"sd_first", "SD\u2081", MeanComparisonCellFormat::Number, false},
            {"mean_second", "M\u2082", MeanComparisonCellFormat::Number},
            {"sd_second", "SD\u2082", MeanComparisonCellFormat::Number, false},
            {"difference", "\u0394M", MeanComparisonCellFormat::Number},
            {"sd_difference", "SD\u0394M", MeanComparisonCellFormat::Number},
            {"se_difference", "SE\u0394M", MeanComparisonCellFormat::Number},
            {"ci", level + " CI", MeanComparisonCellFormat::ConfidenceInterval},
            {"t", "t", MeanComparisonCellFormat::Number},
            {"df", "df", MeanComparisonCellFormat::Number},
            {"p", "p", MeanComparisonCellFormat::PValue}
        };
        AppendAdjustedPColumn(tTests, adjustment);
        tTests.columns.push_back({"effect", "d_z", MeanComparisonCellFormat::Number});

        MeanComparisonTable rankTests;
        rankTests.tableId = "paired_wilcoxon"; rankTests.title = "Wilcoxon signed-rank tests";
        rankTests.stubTitle = "Pair"; rankTests.pAdjustment = adjustment;
        rankTests.adjustmentFamilySize = state.adjustmentFamilySize;
        rankTests.columns = {
            {"n", "N", MeanComparisonCellFormat::Integer},
            {"median_first", "Mdn\u2081", MeanComparisonCellFormat::Number},
            {"iqr_first", "IQR\u2081", MeanComparisonCellFormat::Number, false},
            {"median_second", "Mdn\u2082", MeanComparisonCellFormat::Number},
            {"iqr_second", "IQR\u2082", MeanComparisonCellFormat::Number, false},
            {"difference", "\u0394HL", MeanComparisonCellFormat::Number},
            {"ci", level + " CI", MeanComparisonCellFormat::ConfidenceInterval},
            {"statistic", "V", MeanComparisonCellFormat::Number},
            {"p", "p", MeanComparisonCellFormat::PValue}
        };
        AppendAdjustedPColumn(rankTests, adjustment);
        rankTests.columns.push_back({"effect", "r_rb", MeanComparisonCellFormat::Number});

        MeanComparisonTable binaryTests;
        binaryTests.tableId = "paired_mcnemar"; binaryTests.title = "McNemar tests";
        binaryTests.stubTitle = "Pair"; binaryTests.pAdjustment = adjustment;
        binaryTests.adjustmentFamilySize = state.adjustmentFamilySize;
        binaryTests.columns = {
            {"n", "N", MeanComparisonCellFormat::Integer},
            {"event", "Event category", MeanComparisonCellFormat::Text},
            {"proportion_first", "p\u0302\u2081", MeanComparisonCellFormat::Number},
            {"proportion_second", "p\u0302\u2082", MeanComparisonCellFormat::Number},
            {"difference", "\u0394p\u0302", MeanComparisonCellFormat::Number},
            {"first_only", "b", MeanComparisonCellFormat::Integer},
            {"second_only", "c", MeanComparisonCellFormat::Integer},
            {"ci", level + " CI", MeanComparisonCellFormat::ConfidenceInterval},
            {"p", "p", MeanComparisonCellFormat::PValue}
        };
        AppendAdjustedPColumn(binaryTests, adjustment);
        binaryTests.columns.push_back({"effect", "OR_m", MeanComparisonCellFormat::Number});

        MeanComparisonTable desc;
        desc.tableId = "paired_descriptives"; desc.title = "Pair descriptives"; desc.stubTitle = "Pair";
        desc.columns = {{"variable", "Variable", MeanComparisonCellFormat::Text},
                        {"n", "N", MeanComparisonCellFormat::Integer},
                        {"mean", "M", MeanComparisonCellFormat::Number},
                        {"sd", "SD", MeanComparisonCellFormat::Number}};
        MeanComparisonTable rankDesc;
        rankDesc.tableId = "paired_rank_descriptives"; rankDesc.title = "Ordinal pair descriptives";
        rankDesc.stubTitle = "Pair";
        rankDesc.columns = {{"variable", "Variable", MeanComparisonCellFormat::Text},
                            {"n", "N", MeanComparisonCellFormat::Integer},
                            {"median", "Mdn", MeanComparisonCellFormat::Number},
                            {"iqr", "IQR", MeanComparisonCellFormat::Number}};
        MeanComparisonTable binaryDesc;
        binaryDesc.tableId = "paired_binary_descriptives"; binaryDesc.title = "Binary pair descriptives";
        binaryDesc.stubTitle = "Pair";
        binaryDesc.columns = {{"variable", "Variable", MeanComparisonCellFormat::Text},
                              {"event", "Event category", MeanComparisonCellFormat::Text},
                              {"n", "N", MeanComparisonCellFormat::Integer},
                              {"proportion", "p\u0302", MeanComparisonCellFormat::Number}};

        for (const auto &r : results) {
            const auto &v = r.values;
            MeanComparisonRow row;
            row.rowId = "pair:" + r.response1 + "\x1f" + r.response2;
            row.label = r.response1 + " \u2212 " + r.response2;
            row.variable = r.response1; row.secondVariable = r.response2;
            row.originalRowIndices = r.rowsUsed;
            state.specification.pairs.push_back({row.rowId, r.response1, r.response2});
            if (r.testFamily == "mcnemar") {
                row.cells = {Number(v[0]), Text(r.eventLevel), Number(v[1]), Number(v[5]),
                             Number(v[8]), Number(v[9]), Number(v[10]),
                             CI(v[11], v[12]), Number(v[16])};
                AppendAdjustedPCell(row.cells, v, adjustment); row.cells.push_back(Number(v[17]));
                binaryTests.rows.push_back(std::move(row)); AddCommonWarnings(binaryTests, r);
                for (int variableIndex = 0; variableIndex < 2; ++variableIndex) {
                    MeanComparisonRow descriptive;
                    const std::string variable = variableIndex == 0 ? r.response1 : r.response2;
                    descriptive.rowId = "desc:" + r.response1 + "\x1f" + r.response2 + "\x1f" + variable;
                    descriptive.label = r.response1 + " \u2212 " + r.response2;
                    descriptive.variable = variable; descriptive.secondVariable = r.response2;
                    descriptive.kind = MeanComparisonRowKind::Descriptive;
                    descriptive.originalRowIndices = r.rowsUsed;
                    descriptive.cells = {Text(variable), Text(r.eventLevel), Number(v[0]),
                                         Number(variableIndex == 0 ? v[1] : v[5])};
                    binaryDesc.rows.push_back(std::move(descriptive));
                }
            } else {
                const bool rank = r.testFamily == "wilcoxon";
                row.cells = rank
                    ? std::vector<MeanComparisonCell>{Number(v[0]), Number(v[1]), Number(v[2]),
                          Number(v[5]), Number(v[6]), Number(v[8]), CI(v[11], v[12]),
                          Number(v[13]), Number(v[16])}
                    : std::vector<MeanComparisonCell>{Number(v[0]), Number(v[1]), Number(v[2]),
                          Number(v[5]), Number(v[6]), Number(v[8]), Number(v[9]), Number(v[10]),
                          CI(v[11], v[12]), Number(v[13]), Number(v[14]), Number(v[16])};
                AppendAdjustedPCell(row.cells, v, adjustment); row.cells.push_back(Number(v[17]));
                auto &target = rank ? rankTests : tTests;
                target.rows.push_back(std::move(row)); AddCommonWarnings(target, r);
                auto &descriptiveTarget = rank ? rankDesc : desc;
                for (int variableIndex = 0; variableIndex < 2; ++variableIndex) {
                    MeanComparisonRow descriptive;
                    const std::string variable = variableIndex == 0 ? r.response1 : r.response2;
                    descriptive.rowId = "desc:" + r.response1 + "\x1f" + r.response2 + "\x1f" + variable;
                    descriptive.label = r.response1 + " \u2212 " + r.response2;
                    descriptive.variable = variable; descriptive.secondVariable = r.response2;
                    descriptive.kind = MeanComparisonRowKind::Descriptive;
                    descriptive.originalRowIndices = r.rowsUsed;
                    descriptive.cells = {Text(variable), Number(v[0]),
                                         Number(variableIndex == 0 ? v[1] : v[5]),
                                         Number(variableIndex == 0 ? v[2] : v[6])};
                    descriptiveTarget.rows.push_back(std::move(descriptive));
                }
            }
        }
        const int inferentialFamilies = (!tTests.rows.empty() ? 1 : 0) +
            (!rankTests.rows.empty() ? 1 : 0) + (!binaryTests.rows.empty() ? 1 : 0);
        if (!tTests.rows.empty()) {
            if (inferentialFamilies == 1) tTests.title.clear();
            tTests.notes.push_back("\u0394M is the first-variable mean minus the second-variable mean. Paired t tests were calculated in R.");
            if (adjustment != MultipleTestingAdjustment::None)
                tTests.notes.push_back(AdjustmentNote(adjustment, state.adjustmentFamilySize));
            state.tables.push_back(std::move(tTests));
        }
        if (!rankTests.rows.empty()) {
            if (inferentialFamilies == 1) rankTests.title.clear();
            rankTests.notes.push_back("\u0394HL is the Hodges\u2013Lehmann location shift for the first variable minus the second. Wilcoxon signed-rank tests were calculated in R.");
            if (adjustment != MultipleTestingAdjustment::None)
                rankTests.notes.push_back(AdjustmentNote(adjustment, state.adjustmentFamilySize));
            state.tables.push_back(std::move(rankTests));
        }
        if (!binaryTests.rows.empty()) {
            if (inferentialFamilies == 1) binaryTests.title.clear();
            binaryTests.notes.push_back("b and c are the two discordant counts. Exact conditional McNemar tests compare the paired event proportions; \u0394p\u0302 is first minus second.");
            if (adjustment != MultipleTestingAdjustment::None)
                binaryTests.notes.push_back(AdjustmentNote(adjustment, state.adjustmentFamilySize));
            state.tables.push_back(std::move(binaryTests));
        }
        if (!desc.rows.empty()) state.tables.push_back(std::move(desc));
        if (!rankDesc.rows.empty()) state.tables.push_back(std::move(rankDesc));
        if (!binaryDesc.rows.empty()) state.tables.push_back(std::move(binaryDesc));
        } else {
        MeanComparisonTable table;
        table.pAdjustment = adjustment; table.adjustmentFamilySize = state.adjustmentFamilySize;
        table.tableId = "paired"; table.stubTitle = "Pair";
        table.columns = {{"n", "N", MeanComparisonCellFormat::Integer},
                         {"mean_first", signedRank ? "Mdn\u2081" : "M\u2081", MeanComparisonCellFormat::Number},
                         {"sd_first", signedRank ? "IQR\u2081" : "SD\u2081", MeanComparisonCellFormat::Number, false},
                         {"mean_second", signedRank ? "Mdn\u2082" : "M\u2082", MeanComparisonCellFormat::Number},
                         {"sd_second", signedRank ? "IQR\u2082" : "SD\u2082", MeanComparisonCellFormat::Number, false}};
        if (signedRank) {
            table.columns.insert(table.columns.end(), {
                {"difference", "\u0394HL", MeanComparisonCellFormat::Number},
                {"ci", level + " CI", MeanComparisonCellFormat::ConfidenceInterval},
                {"statistic", "V", MeanComparisonCellFormat::Number}, {"p", "p", MeanComparisonCellFormat::PValue}
            });
        } else {
            table.columns.insert(table.columns.end(), {
                {"difference", "\u0394M", MeanComparisonCellFormat::Number},
                {"sd_difference", "SDΔM", MeanComparisonCellFormat::Number},
                {"se_difference", "SE\u0394M", MeanComparisonCellFormat::Number},
                {"ci", level + " CI", MeanComparisonCellFormat::ConfidenceInterval}, {"t", "t", MeanComparisonCellFormat::Number},
                {"df", "df", MeanComparisonCellFormat::Number}, {"p", "p", MeanComparisonCellFormat::PValue}
            });
        }
        AppendAdjustedPColumn(table, adjustment);
        table.columns.push_back({"effect", signedRank ? "r_rb" : "d_z", MeanComparisonCellFormat::Number});
        MeanComparisonTable desc;
        desc.tableId = "group_descriptives"; desc.title = "Pair descriptives"; desc.stubTitle = "Pair";
        desc.columns = {{"variable", "Variable", MeanComparisonCellFormat::Text},
                        {"n", "N", MeanComparisonCellFormat::Integer},
                        {"mean", signedRank ? "Mdn" : "M", MeanComparisonCellFormat::Number},
                        {"sd", signedRank ? "IQR" : "SD", MeanComparisonCellFormat::Number}};
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
        }
    } else if (state.analysisType == "one_way_anova") {
        state.specification.kind = MeanComparisonKind::OneWayAnova;
        state.subtitle = MeanComparisonGroupingVariableLine(state.groupVariable);
        if (!state.multipleImputation) state.subtitle += " \u00b7 " + state.method;
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
        if (state.multipleImputation) {
            // The pooling method belongs to the table note, not every response row.
            tests.columns.erase(tests.columns.begin());
            for (auto &row : tests.rows) row.cells.erase(row.cells.begin());
        } else {
            tests.notes.push_back(MeanComparisonGroupingVariableLine(state.groupVariable) + ". " + state.method +
                " was calculated in R separately for each dependent variable. " +
                (kruskalWallis ? "\u03b5\u00b2" : "\u03c9\u00b2") + " is reported as the effect size.");
        }
        if (adjustment != MultipleTestingAdjustment::None) tests.notes.push_back(AdjustmentNote(adjustment, state.adjustmentFamilySize));
        state.tables.push_back(std::move(desc)); state.tables.push_back(std::move(tests));
    } else {
        return fail("Unsupported compare-means analysis type.");
    }
    if (state.multipleImputation) {
        if (state.title.find("Multiple Imputation") == std::string::npos) {
            state.title += " - Multiple Imputation";
        }
        std::ostringstream detail;
        detail << "Multiple imputation";
        if (state.imputationCount > 0) detail << ": m = " << state.imputationCount;
        if (!state.poolingMethod.empty()) detail << " · Pooled using " << state.poolingMethod;
        const std::string miDetail = detail.str();
        if (state.analysisType != "one_way_anova")
            state.subtitle = state.subtitle.empty() ? miDetail : miDetail + " · " + state.subtitle;
        auto inferential = std::find_if(state.tables.begin(), state.tables.end(),
            [](const MeanComparisonTable &table) {
                return table.tableId != "group_descriptives" &&
                    table.tableId != "rank_descriptives";
            });
        if (inferential != state.tables.end()) inferential->notes.push_back(miDetail + ".");
    }
    // Present grouped descriptives with one variable heading and indented groups.
    for (auto &table : state.tables) {
        if (table.columns.empty() || table.columns.front().key != "group" ||
            (table.tableId != "group_descriptives" && table.tableId != "rank_descriptives")) continue;
        std::vector<MeanComparisonRow> rows;
        std::string previousVariable;
        for (auto row : table.rows) {
            const bool hasValues = std::any_of(row.cells.begin()+std::min<std::size_t>(1,row.cells.size()),row.cells.end(),
                [](const auto &cell){return cell.numericValue && std::isfinite(*cell.numericValue);});
            if (!hasValues) continue;
            if (rows.empty() || row.variable != previousVariable) {
                MeanComparisonRow heading;
                heading.rowId = "variable:"+row.variable;heading.label=row.variable;heading.variable=row.variable;
                heading.kind=MeanComparisonRowKind::GroupHeader;
                rows.push_back(std::move(heading));previousVariable=row.variable;
            }
            row.label=row.group;
            if (!row.cells.empty()) row.cells.erase(row.cells.begin());
            rows.push_back(std::move(row));
        }
        table.columns.erase(table.columns.begin());table.rows=std::move(rows);
    }
    for (const auto &table : state.tables) {
        state.warnings.insert(state.warnings.end(), table.warnings.begin(), table.warnings.end());
    }
    if (cursor < args.size()) {
        std::string provenanceError;
        if (!ReadAnalysisProvenancePayload(args, cursor, state.provenance,
                                           &provenanceError)) {
            return fail(provenanceError.empty()
                ? "Malformed compare-means R-code provenance."
                : provenanceError);
        }
    }
    if (cursor != args.size()) return fail("Unexpected trailing compare-means fields.");
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
        if (value == "difference") {
            if (table.tableId == "one_sample_wilcoxon" ||
                table.tableId == "independent_mann_whitney" ||
                table.tableId == "paired_wilcoxon") return std::string("Location_Shift");
            if (table.tableId == "independent_proportions" ||
                table.tableId == "paired_mcnemar") return std::string("Proportion_Difference");
            return std::string("Mean_Difference");
        }
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
            if (table.tableId == "one_sample" || table.tableId == "one_sample_t")
                return std::string("Cohens_d");
            if (table.tableId == "one_sample_wilcoxon") return std::string("Rank_Biserial_r");
            if (table.tableId == "mean_comparisons" || table.tableId == "independent_t")
                return std::string("Hedges_g");
            if (table.tableId == "independent_mann_whitney") return std::string("Rank_Biserial_r");
            if (table.tableId == "independent_proportions") return std::string("Cohens_h");
            if (table.tableId == "paired" || table.tableId == "paired_t") return std::string("Cohens_dz");
            if (table.tableId == "paired_wilcoxon") return std::string("Rank_Biserial_r");
            if (table.tableId == "paired_mcnemar") return std::string("Matched_Odds_Ratio");
            return std::string("Effect_Size");
        }
        return value;
    };
    std::ostringstream out;
    const bool inferential = table.tableId == "one_sample" || table.tableId == "one_sample_t" ||
        table.tableId == "one_sample_wilcoxon" || table.tableId == "one_sample_binomial" ||
        table.tableId == "mean_comparisons" || table.tableId == "independent_t" ||
        table.tableId == "independent_mann_whitney" ||
        table.tableId == "independent_proportions" || table.tableId == "paired" ||
        table.tableId == "paired_t" || table.tableId == "paired_wilcoxon" ||
        table.tableId == "paired_mcnemar" ||
        table.tableId == "omnibus";
    const std::string metadataAfter = table.pAdjustment == MultipleTestingAdjustment::None
        ? "p" : "p_adjusted";
    const bool independentComparison = table.tableId == "mean_comparisons" ||
        table.tableId == "independent_t" || table.tableId == "independent_mann_whitney" ||
        table.tableId == "independent_proportions";
    const bool pairedComparison = table.tableId == "paired" || table.tableId == "paired_t" ||
        table.tableId == "paired_wilcoxon" || table.tableId == "paired_mcnemar";
    if (pairedComparison) out << "First_Variable,Second_Variable";
    else if (independentComparison) out << "Variable,First_Group,Second_Group";
    else out << CSVCell(table.stubTitle);
    for (const auto &column : table.columns) {
        if (column.format == MeanComparisonCellFormat::ConfidenceInterval) {
            out << ",CI_Lower,CI_Upper";
        } else out << ',' << CSVCell(key(column));
        if (inferential && column.key == metadataAfter) {
            out << ",p_adjustment,adjustment_family_size";
        }
    }
    if (independentComparison) out << ",Method";
    out << '\n';
    for (const auto &row : table.rows) {
        if (pairedComparison) out << CSVCell(row.variable) << ',' << CSVCell(row.secondVariable);
        else if (independentComparison) out << CSVCell(row.variable) << ',' << CSVCell(table.firstGroup) << ',' << CSVCell(table.secondGroup);
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
        if (independentComparison) out << ',' << CSVCell(table.method);
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
            (column.key == "p_adjusted" ? 80.0 :
                (column.format == MeanComparisonCellFormat::Text ? 70.0 : 58.0));
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

bool MeanComparisonIsDescriptiveTable(const MeanComparisonTable &table)
{
    return table.tableId == "group_descriptives" ||
        table.tableId == "rank_descriptives" ||
        table.tableId == "paired_descriptives" ||
        table.tableId == "paired_rank_descriptives" ||
        table.tableId == "paired_binary_descriptives";
}

double MeanComparisonVisiblePreferredHeight(const MeanComparisonState &state,
                                             bool showDescriptives,
                                             bool descriptivesOnly)
{
    double height = 112.0;
    for (const auto &table : state.tables) {
        const bool descriptive = MeanComparisonIsDescriptiveTable(table);
        if ((descriptivesOnly && !descriptive) ||
            (!descriptivesOnly && descriptive && !showDescriptives)) continue;
        height += (table.title.empty() ? 0.0 : 24.0) + 30.0 + 25.0 * table.rows.size();
        height += 18.0 * (table.notes.size() + table.warnings.size()) + 18.0;
    }
    return std::min(820.0, std::max(250.0, height));
}

double MeanComparisonPreferredHeight(const MeanComparisonState &state)
{
    return MeanComparisonVisiblePreferredHeight(state, true, false);
}

} // namespace core
} // namespace rlispstat
