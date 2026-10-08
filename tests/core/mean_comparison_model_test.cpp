#include "core/mean_comparison_model.h"
#include "core/dataset_model.h"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <string>
#include <vector>

using namespace rlispstat::core;

int main()
{
    DataColumn before;
    before.type = "factor";
    before.values = {"No", "No", "No", "No"};
    before.definedLevels = {"No", "Yes"};
    DataColumn after = before;
    after.values = {"No", "Yes", "No", "Yes"};
    assert(MeanComparisonBinaryResponse(before));
    assert(MeanComparisonBinaryPairCompatible(before, after));
    after.definedLevels = {"Yes", "No"};
    assert(MeanComparisonBinaryPairCompatible(before, after));
    after.definedLevels = {"No", "Maybe"}; after.values = {"No", "Maybe"};
    assert(!MeanComparisonBinaryPairCompatible(before, after));
    before.definedLevels.clear();
    assert(!MeanComparisonBinaryResponse(before));
    before.values = {"No", "Yes", "Maybe"};
    assert(!MeanComparisonBinaryResponse(before));
    before.values = {"0", "1"}; before.type = "numeric";
    assert(!MeanComparisonBinaryResponse(before));

    const std::vector<std::string> availableGroups = {"Control", "Treatment", "Waitlist"};
    assert(MeanComparisonDefaultIndependentGroupOrder(availableGroups) ==
           std::vector<std::string>({"Control", "Treatment"}));
    assert(MeanComparisonIndependentGroupOrderIsValid(
        {"Treatment", "Waitlist"}, availableGroups));
    assert(!MeanComparisonIndependentGroupOrderIsValid(
        {"Treatment", "Treatment"}, availableGroups));
    assert(!MeanComparisonIndependentGroupOrderIsValid(
        {"Treatment", "Unknown"}, availableGroups));
    assert(!MeanComparisonIndependentGroupOrderIsValid(
        {"Treatment"}, availableGroups));
    const std::vector<std::string> twoGroups = {"Control", "Treatment"};
    assert(MeanComparisonIndependentGroupOrderForReference(
               twoGroups, "Treatment") ==
           std::vector<std::string>({"Control", "Treatment"}));
    assert(MeanComparisonIndependentGroupOrderForReference(
               twoGroups, "Control") ==
           std::vector<std::string>({"Treatment", "Control"}));
    assert(MeanComparisonIndependentGroupOrderForReference(
               twoGroups, "Unknown").empty());
    assert(MeanComparisonIndependentGroupOrderForReference(
               availableGroups, "Treatment").empty());

    assert(MeanComparisonGroupingVariableLine("gear") == "Grouping variable: gear");
    assert(MeanComparisonGroupingVariableLine("") == "Grouping variable: \u2014");
    assert(MeanComparisonSubtitleWithoutGroupingVariable(
               "Grouping variable: gear \u00b7 Welch one-way ANOVA", "gear") ==
           "Welch one-way ANOVA");
    assert(MeanComparisonSubtitleWithoutGroupingVariable("Welch", "gear") == "Welch");
    const auto oneSamplePlots = MeanComparisonPlotMenuOptions("one_sample_t_test", true, false);
    assert(oneSamplePlots.size() == 2);
    assert(oneSamplePlots[0].plotKind == "histogram");
    assert(oneSamplePlots[1].title == "Boxplot\u2026");
    const auto independentPlots = MeanComparisonPlotMenuOptions(
        "independent_samples_t_test", true, false);
    assert(independentPlots.size() == 1);
    assert(independentPlots[0].plotKind == "grouped_boxplot");
    const auto pairedPlots = MeanComparisonPlotMenuOptions(
        "paired_samples_t_test", true, true);
    assert(pairedPlots.size() == 3);
    assert(pairedPlots[0].plotKind == "paired_forest_plot");
    assert(pairedPlots[1].plotKind == "paired_scatterplot");
    assert(pairedPlots[2].plotKind == "paired_difference_histogram");
    assert(MeanComparisonPlotMenuOptions("paired_samples_t_test", true, false).empty());
    assert(MeanComparisonPlotMenuOptions("one_way_anova", false, false).empty());

    MeanComparisonState forestState;
    MeanComparisonTable forestTable;
    forestTable.tableId = "paired";
    forestTable.columns = {
        {"difference", "Mean difference", MeanComparisonCellFormat::Number},
        {"se_difference", "SE difference", MeanComparisonCellFormat::Number},
        {"ci", "95% CI", MeanComparisonCellFormat::ConfidenceInterval},
        {"t", "t", MeanComparisonCellFormat::Number},
        {"df", "df", MeanComparisonCellFormat::Number},
        {"p", "p", MeanComparisonCellFormat::PValue}
    };
    MeanComparisonRow forestSource;
    forestSource.label = "before \u2212 after";
    forestSource.variable = "before";
    forestSource.secondVariable = "after";
    forestSource.originalRowIndices = {1, 3, 4};
    forestSource.cells.resize(6);
    forestSource.cells[0].numericValue = 2.5;
    forestSource.cells[1].numericValue = 0.7;
    forestSource.cells[2].numericValue = 1.1;
    forestSource.cells[2].numericUpperValue = 3.9;
    forestSource.cells[3].numericValue = 3.57;
    forestSource.cells[4].numericValue = 12.0;
    forestSource.cells[5].numericValue = 0.004;
    forestTable.rows.push_back(forestSource);
    forestState.tables.push_back(forestTable);
    const auto forestRows = MeanComparisonPairedForestRows(forestState);
    assert(forestRows.size() == 1);
    assert(forestRows[0].label == "before \u2212 after");
    assert(forestRows[0].estimate == 2.5);
    assert(forestRows[0].confidenceLower == 1.1);
    assert(forestRows[0].confidenceUpper == 3.9);
    assert(forestRows[0].adjustedPValue == 0.004);
    assert(forestRows[0].originalRowIndices == std::vector<int>({1, 3, 4}));
    const std::vector<double> forestColumnWidths = MeanComparisonPreferredColumnWidths(forestTable);
    assert(forestColumnWidths.size() == forestTable.columns.size());
    assert(forestColumnWidths[2] >= 118.0);
    assert(forestColumnWidths[2] > forestColumnWidths[5]);
    MeanComparisonTable wideIntervalTable = forestTable;
    wideIntervalTable.rows[0].cells[2].numericValue = -123456.789;
    wideIntervalTable.rows[0].cells[2].numericUpperValue = 987654.321;
    const std::vector<double> wideIntervalWidths =
        MeanComparisonPreferredColumnWidths(wideIntervalTable);
    assert(wideIntervalWidths[2] > forestColumnWidths[2]);

    MeanComparisonRow inferentialRow;
    inferentialRow.kind = MeanComparisonRowKind::Result;
    inferentialRow.originalRowIndices = {1, 2, 3, 4};
    assert(MeanComparisonLinkedRows(inferentialRow).empty());
    MeanComparisonRow ungroupedDescriptiveRow;
    ungroupedDescriptiveRow.kind = MeanComparisonRowKind::Descriptive;
    ungroupedDescriptiveRow.originalRowIndices = {1, 2, 3, 4};
    assert(MeanComparisonLinkedRows(ungroupedDescriptiveRow).empty());
    MeanComparisonRow groupedDescriptiveRow;
    groupedDescriptiveRow.kind = MeanComparisonRowKind::Descriptive;
    groupedDescriptiveRow.group = "6";
    groupedDescriptiveRow.originalRowIndices = {2, 4};
    assert(MeanComparisonLinkedRows(groupedDescriptiveRow) == std::vector<int>({2, 4}));

    std::vector<std::string> args = {
        "COMPARE_MEANS_BATCH_V1", "run_1", "mtcars", "one_sample_t_test", "One-Sample t Test",
        "", "One-sample t-test", "two.sided", "0.95", "0", "", "", "1",
        "mpg", "", "One-sample t-test", "Cohen's d",
        "32", "20.090625", "6.026948", "1.065424", "NA", "NA", "NA", "NA",
        "20.090625", "NA", "1.065424", "17.917679", "22.263571", "18.856933", "31", "NA",
        "1.2e-19", "3.333",
        "0",
        "3", "1", "7", "11",
        "1", "4",
        "1", "One observation was excluded."
    };
    MeanComparisonState state;
    std::string error;
    assert(ParseMeanComparisonBatch(args, state, &error));
    assert(state.tables.size() == 2);
    assert(state.tables[1].tableId == "group_descriptives");
    assert(state.tables[0].rows.size() == 1);
    assert(state.tables[0].rows[0].originalRowIndices == std::vector<int>({1, 7, 11}));
    assert(!state.tables[0].columns[2].visible);
    assert(state.tables[0].columns[6].format == MeanComparisonCellFormat::ConfidenceInterval);
    const auto &ci = state.tables[0].rows[0].cells[6];
    assert(ci.numericValue && ci.numericUpperValue);
    assert(std::fabs(*ci.numericValue - 17.917679) < 1e-9);
    assert(std::fabs(*ci.numericUpperValue - 22.263571) < 1e-9);
    assert(MeanComparisonCellText(state.tables[0].rows[0].cells[7], MeanComparisonCellFormat::Number) == "18.857");
    assert(MeanComparisonCellText(state.tables[0].rows[0].cells[9], MeanComparisonCellFormat::PValue) == "< .001");
    std::string csv = MeanComparisonTableCSV(state.tables[0]);
    assert(csv.find("CI_Lower") != std::string::npos);
    assert(csv.find("Variable,N,Mean,SD,Test_Value") == 0);
    assert(MeanComparisonTableText(state.tables[0]).find("\tSD\t") == std::string::npos);
    assert(csv.find("CI_Upper") != std::string::npos);
    assert(csv.find("e-19") != std::string::npos);

    std::vector<std::string> v2 = {
        "COMPARE_MEANS_BATCH_V2", "run_2", "mtcars", "one_sample_t_test", "One-Sample t Test",
        "", "One-sample t-test", "two.sided", "0.95", "0", "", "", "holm", "1",
        "mpg", "", "One-sample t-test", "Cohen's d",
        "32", "20.090625", "6.026948", "1.065424", "NA", "NA", "NA", "NA",
        "20.090625", "NA", "1.065424", "17.917679", "22.263571", "18.856933", "31", "NA",
        "1.2e-19", "3.333", "1.2e-19", "1",
        "0", "3", "1", "7", "11", "1", "4", "1", "One observation was excluded."
    };
    MeanComparisonState adjusted;
    assert(ParseMeanComparisonBatch(v2, adjusted, &error));
    assert(adjusted.specification.kind == MeanComparisonKind::OneSample);
    assert(adjusted.specification.pAdjustment == MultipleTestingAdjustment::Holm);
    assert(adjusted.adjustmentFamilySize == 1);
    assert(adjusted.specification.dependentVariableIds == std::vector<std::string>({"mpg"}));
    assert(adjusted.tables[0].columns[10].key == "p_adjusted");
    assert(!adjusted.tables[0].columns[10].visible);
    const auto oneTestPublication = MeanComparisonPublicationTable(adjusted.tables[0]);
    assert(std::none_of(oneTestPublication.columns.begin(), oneTestPublication.columns.end(),
        [](const auto &column) { return column.key == "p_adjusted"; }));
    const std::string adjustedCsv = MeanComparisonTableCSV(adjusted.tables[0]);
    assert(adjustedCsv.find("p,p_adjusted,p_adjustment,adjustment_family_size,Cohens_d") != std::string::npos);
    assert(adjustedCsv.find(",holm,1,") != std::string::npos);

    std::vector<std::string> signedRankArgs = v2;
    signedRankArgs[6] = "Wilcoxon signed-rank test";
    signedRankArgs[16] = "Wilcoxon signed-rank test";
    signedRankArgs[17] = "Rank-biserial r";
    signedRankArgs[32] = "NA";
    MeanComparisonState signedRank;
    assert(ParseMeanComparisonBatch(signedRankArgs, signedRank, &error));
    assert(signedRank.specification.method == MeanComparisonMethod::WilcoxonSignedRank);
    assert(signedRank.tables[0].columns[6].title == "V");
    assert(signedRank.tables[0].columns.back().title == "r_rb");
    assert(signedRank.tables[0].columns[4].title == "\u0394HL");

    auto appendV3Result = [](std::vector<std::string> &payload,
                             const std::string &response,
                             const std::string &family,
                             const std::string &type,
                             const std::string &event,
                             const std::string &nullValue,
                             std::vector<std::string> values) {
        payload.insert(payload.end(), {response, "", family, "", family, type, event, nullValue});
        payload.insert(payload.end(), values.begin(), values.end());
        payload.insert(payload.end(), {"0", "3", "1", "2", "3", "0", "0"});
    };
    std::vector<std::string> mixed = {
        "COMPARE_MEANS_BATCH_V3", "run_3", "Alien", "one_sample_t_test",
        "One-Sample Test", "", "Automatic by variable type", "two.sided",
        "0.95", "0", "", "", "holm", "3"
    };
    appendV3Result(mixed, "size", "student", "numeric", "", "60",
        {"60", "62.1", "8", "1.03", "NA", "NA", "NA", "NA", "2.1", "NA", "1.03",
         "0.07", "4.13", "2.04", "59", "NA", "0.045", "0.26", "0.09", "3"});
    appendV3Result(mixed, "rank", "wilcoxon", "ordinal", "", "3",
        {"60", "3.2", "1.1", "0.14", "NA", "NA", "NA", "NA", "0.2", "NA", "NA",
         "0.01", "0.40", "1110", "NA", "NA", "0.03", "0.22", "0.09", "3"});
    appendV3Result(mixed, "happy", "binomial", "categorical", "yes", "0.5",
        {"60", "0.65", "NA", "0.062", "NA", "NA", "NA", "NA", "0.15", "NA", "0.062",
         "0.516", "0.769", "39", "60", "NA", "0.028", "NA", "0.084", "3"});
    MeanComparisonState mixedState;
    assert(ParseMeanComparisonBatch(mixed, mixedState, &error));
    assert(mixedState.tables.size() == 5);
    assert(mixedState.tables[0].tableId == "one_sample_t");
    assert(mixedState.tables[1].tableId == "one_sample_wilcoxon");
    assert(mixedState.tables[2].tableId == "one_sample_binomial");
    assert(mixedState.tables[3].tableId == "group_descriptives");
    assert(mixedState.tables[4].tableId == "rank_descriptives");
    assert(mixedState.tables[2].rows[0].cells[0].textValue == "yes");
    assert(mixedState.tables[1].columns[1].title == "Mdn");
    assert(mixedState.tables[1].columns[2].title == "\u03b8\u2080");
    assert(mixedState.tables[2].columns[2].title == "x");
    assert(mixedState.tables[2].columns[3].title == "p\u0302");
    assert(mixedState.tables[2].columns[4].title == "p\u2080");
    assert(mixedState.specification.testValues.at("size") == 60.0);
    assert(mixedState.specification.testValues.at("rank") == 3.0);
    assert(mixedState.specification.testValues.at("happy") == 0.5);
    assert(mixedState.method == "Automatic by variable type");
    const std::string mixedTTestCsv = MeanComparisonTableCSV(mixedState.tables[0]);
    assert(mixedTTestCsv.find("p,p_adjusted,p_adjustment,adjustment_family_size,Cohens_d") !=
        std::string::npos);
    const std::string mixedBinomialCsv = MeanComparisonTableCSV(mixedState.tables[2]);
    assert(mixedBinomialCsv.find("p,p_adjusted,p_adjustment,adjustment_family_size") !=
        std::string::npos);

    auto appendIndependentV3Result = [](std::vector<std::string> &payload,
                                        const std::string &response,
                                        const std::string &method,
                                        const std::string &effect,
                                        const std::string &family,
                                        const std::string &type,
                                        const std::string &event,
                                        std::vector<std::string> values,
                                        std::vector<std::string> firstGroup,
                                        std::vector<std::string> secondGroup,
                                        std::vector<std::string> warnings = {}) {
        payload.insert(payload.end(), {response, "", method, effect, family, type, event, "0"});
        payload.insert(payload.end(), values.begin(), values.end());
        payload.push_back("2");
        payload.insert(payload.end(), firstGroup.begin(), firstGroup.end());
        payload.insert(payload.end(), secondGroup.begin(), secondGroup.end());
        payload.insert(payload.end(), {"4", "1", "2", "9", "10", "0",
                                       std::to_string(warnings.size())});
        payload.insert(payload.end(), warnings.begin(), warnings.end());
    };
    std::vector<std::string> mixedIndependent = {
        "COMPARE_MEANS_BATCH_V3", "run_4", "Alien", "independent_samples_t_test",
        "Independent-Samples Test", "group", "Automatic by variable type; continuous: Welch",
        "two.sided", "0.95", "0", "control", "treatment", "holm", "3"
    };
    appendIndependentV3Result(mixedIndependent, "score", "Welch t-test", "Hedges' g",
        "welch", "numeric", "",
        {"16", "13.5", "2.45", "0.87", "8", "17.5", "2.45", "0.87",
         "-4", "NA", "1.22", "-6.61", "-1.39", "-3.27", "14", "NA",
         "0.006", "-1.55", "0.018", "3"},
        {"control", "8", "13.5", "2.45", "0.87", "2", "1", "2"},
        {"treatment", "8", "17.5", "2.45", "0.87", "2", "9", "10"});
    appendIndependentV3Result(mixedIndependent, "rating", "Mann\u2013Whitney U test",
        "Rank-biserial r", "mann_whitney", "ordinal", "",
        {"16", "3", "2", "NA", "8", "4", "2", "NA",
         "-1", "NA", "NA", "-2", "0", "12", "NA", "NA",
         "0.02", "-0.63", "0.03", "3"},
        {"control", "8", "3", "2", "NA", "2", "1", "2"},
        {"treatment", "8", "4", "2", "NA", "2", "9", "10"});
    appendIndependentV3Result(mixedIndependent, "improved", "Two-sample proportion test",
        "Cohen's h", "proportion", "categorical", "yes",
        {"16", "0.375", "0.518", "0.171", "8", "0.875", "0.354", "0.117",
         "-0.5", "NA", "0.177", "-0.78", "-0.12", "8", "1", "NA",
         "0.0047", "-1.12", "0.014", "3"},
        {"control", "8", "0.375", "0.518", "0.171", "2", "1", "2"},
        {"treatment", "8", "0.875", "0.354", "0.117", "2", "9", "10"});
    MeanComparisonState mixedIndependentState;
    assert(ParseMeanComparisonBatch(mixedIndependent, mixedIndependentState, &error));
    assert(mixedIndependentState.title == "Independent-Samples Test");
    assert(mixedIndependentState.tables.size() == 5);
    assert(mixedIndependentState.tables[0].tableId == "independent_t");
    assert(mixedIndependentState.tables[1].tableId == "independent_mann_whitney");
    assert(mixedIndependentState.tables[2].tableId == "independent_proportions");
    assert(mixedIndependentState.tables[3].tableId == "group_descriptives");
    assert(mixedIndependentState.tables[4].tableId == "rank_descriptives");
    assert(mixedIndependentState.tables[2].rows[0].cells[0].textValue == "yes");
    assert(mixedIndependentState.tables[2].rows[0].cells[1].textValue == "3 / 8 (0.375)");
    assert(mixedIndependentState.tables[4].columns[1].title == "Mdn");
    assert(mixedIndependentState.specification.dependentVariableIds ==
        std::vector<std::string>({"score", "rating", "improved"}));
    const std::string proportionCsv = MeanComparisonTableCSV(mixedIndependentState.tables[2]);
    assert(proportionCsv.find("Proportion_Difference") != std::string::npos);
    assert(proportionCsv.find("Cohens_h") != std::string::npos);

    const std::string effectNote = "Hedges' g uses pooled SD and the exact small-sample correction, including when the test is Welch's.";
    std::vector<std::string> repeatedEffectNotes = {
        "COMPARE_MEANS_BATCH_V3", "run_notes", "Alien", "independent_samples_t_test",
        "Two-Sample Tests", "happy", "Automatic by variable type; continuous: Welch",
        "two.sided", "0.95", "0", "no", "yes", "holm", "2"
    };
    for (const std::string &response : {"blue_eyes", "size"}) {
        appendIndependentV3Result(repeatedEffectNotes, response, "Welch t-test", "Hedges' g",
            "welch", "numeric", "",
            {"16", "13.5", "2.45", "0.87", "8", "17.5", "2.45", "0.87",
             "-4", "NA", "1.22", "-6.61", "-1.39", "-3.27", "14", "NA",
             "0.006", "-1.55", "0.018", "2"},
            {"no", "8", "13.5", "2.45", "0.87", "2", "1", "2"},
            {"yes", "8", "17.5", "2.45", "0.87", "2", "9", "10"},
            {effectNote, "Some observations were excluded."});
    }
    MeanComparisonState notesState;
    assert(ParseMeanComparisonBatch(repeatedEffectNotes, notesState, &error));
    assert(notesState.tables[0].title == "Mean differences");
    assert(notesState.tables[0].columns[0].title == "Mean difference");
    assert(notesState.tables[0].columns.back().title == "Hedges' g");
    assert(notesState.tables[0].columns[6].title == "Holm p");
    assert(notesState.tables[0].columns[6].visible);
    assert(MeanComparisonPreferredColumnWidths(notesState.tables[0])[6] >= 80.0);
    assert(std::count(notesState.tables[0].notes.begin(), notesState.tables[0].notes.end(),
                      effectNote) == 1);
    assert(notesState.tables[0].warnings ==
           std::vector<std::string>({"Some observations were excluded."}));
    assert(notesState.warnings == notesState.tables[0].warnings);

    auto appendPairedV3Result = [](std::vector<std::string> &payload,
                                   const std::string &first,
                                   const std::string &second,
                                   const std::string &method,
                                   const std::string &effect,
                                   const std::string &family,
                                   const std::string &type,
                                   const std::string &event,
                                   std::vector<std::string> values) {
        payload.insert(payload.end(), {first, second, method, effect, family, type, event, "0"});
        payload.insert(payload.end(), values.begin(), values.end());
        payload.insert(payload.end(), {"0", "3", "1", "2", "3", "0", "0"});
    };
    std::vector<std::string> mixedPaired = {
        "COMPARE_MEANS_BATCH_V3", "run_5", "Alien", "paired_samples_t_test",
        "Paired-Samples Tests", "", "Automatic by pair type; continuous: paired t",
        "two.sided", "0.95", "0", "", "", "holm", "3"
    };
    appendPairedV3Result(mixedPaired, "before", "after", "Paired t-test", "Cohen's dz",
        "paired_t", "numeric", "",
        {"12", "10", "2", "0.58", "12", "8", "1.5", "0.43", "2", "1.2",
         "0.35", "1.23", "2.77", "5.71", "11", "NA", "0.0001", "1.67", "0.0003", "3"});
    appendPairedV3Result(mixedPaired, "rank_1", "rank_2", "Wilcoxon signed-rank test",
        "Rank-biserial r", "wilcoxon", "ordinal", "",
        {"12", "3", "1", "NA", "12", "4", "1.5", "NA", "-1", "NA", "NA",
         "-2", "0", "9", "NA", "NA", "0.02", "-0.58", "0.03", "3"});
    appendPairedV3Result(mixedPaired, "yes_1", "yes_2", "Exact conditional McNemar test",
        "Matched odds ratio", "mcnemar", "categorical", "yes",
        {"12", "0.6", "NA", "NA", "12", "0.4", "NA", "NA", "0.2", "5", "1",
         "-0.08", "0.48", "5", "6", "NA", "0.0625", "5", "0.09375", "3"});
    MeanComparisonState mixedPairedState;
    assert(ParseMeanComparisonBatch(mixedPaired, mixedPairedState, &error));
    assert(mixedPairedState.title == "Paired-Samples Tests");
    assert(mixedPairedState.tables.size() == 6);
    assert(mixedPairedState.tables[0].tableId == "paired_t");
    assert(mixedPairedState.tables[1].tableId == "paired_wilcoxon");
    assert(mixedPairedState.tables[2].tableId == "paired_mcnemar");
    assert(mixedPairedState.tables[0].columns[1].title == "M\u2081");
    assert(mixedPairedState.tables[0].columns[3].title == "M\u2082");
    assert(mixedPairedState.tables[0].columns[5].title == "\u0394M");
    assert(mixedPairedState.tables[1].columns[1].title == "Mdn\u2081");
    assert(mixedPairedState.tables[1].columns[5].title == "\u0394HL");
    assert(mixedPairedState.tables[2].columns[2].title == "p\u0302\u2081");
    assert(mixedPairedState.tables[2].columns[4].title == "\u0394p\u0302");
    assert(mixedPairedState.tables[2].rows[0].cells[1].textValue == "yes");
    assert(mixedPairedState.specification.pairs.size() == 3);
    const auto pairedForestRows = MeanComparisonPairedForestRows(mixedPairedState);
    assert(pairedForestRows.size() == 2);
    const std::string pairedCSV = MeanComparisonTableCSV(mixedPairedState.tables[0]);
    assert(pairedCSV.find("First_Variable,Second_Variable") == 0);
    assert(pairedCSV.find("Mean_First") != std::string::npos);
    const std::string mcnemarCSV = MeanComparisonTableCSV(mixedPairedState.tables[2]);
    assert(mcnemarCSV.find("Matched_Odds_Ratio") != std::string::npos);

    std::vector<std::string> pooled = {
        "COMPARE_MEANS_BATCH_V4", "mi_probe", "imputaciones_mice2ndandfirst",
        "one_sample_t_test", "One-Sample Tests", "",
        "One-sample t-test (MI pooled)", "two.sided", "0.95", "5", "", "",
        "holm", "multiple_imputation", "20", "Rubin's rules", "1",
        "age_x", "", "One-sample t-test (MI pooled)", "Cohen's d",
        "student", "numeric", "", "5",
        "169", "13.710059171597633", "NA", "0.14643227039459247",
        "NA", "NA", "NA", "NA", "8.7100591715976332", "NA",
        "0.14643227039459247", "8.421021", "8.999097", "59.481828343756135",
        "166.03508771929825", "NA", "0", "NA", "0", "1",
        "0", "3", "1", "2", "3", "0", "0"
    };
    MeanComparisonState pooledState;
    assert(ParseMeanComparisonBatch(pooled, pooledState, &error));
    assert(pooledState.multipleImputation);
    assert(pooledState.imputationCount == 20);
    assert(pooledState.poolingMethod == "Rubin's rules");
    assert(pooledState.title == "One-Sample Tests - Multiple Imputation");
    assert(pooledState.subtitle.find("m = 20") != std::string::npos);
    assert(pooledState.subtitle.find("Rubin's rules") != std::string::npos);
    assert(pooledState.tables.size() == 2);
    const auto &pooledRow = pooledState.tables[0].rows[0];
    assert(pooledRow.cells[0].numericValue && *pooledRow.cells[0].numericValue == 169);
    assert(pooledRow.cells[1].numericValue &&
           std::fabs(*pooledRow.cells[1].numericValue - 13.710059171597633) < 1e-12);
    assert(pooledRow.cells[7].numericValue && std::isfinite(*pooledRow.cells[7].numericValue));
    assert(std::any_of(pooledState.tables[0].notes.begin(), pooledState.tables[0].notes.end(),
        [](const std::string &note) { return note.find("Rubin's rules") != std::string::npos; }));

    MeanComparisonState layoutState;
    MeanComparisonTable descriptives;
    descriptives.tableId = "group_descriptives";
    descriptives.title = "Group descriptives";
    descriptives.rows.resize(6);
    MeanComparisonTable omnibus;
    omnibus.tableId = "omnibus";
    omnibus.title = "Omnibus tests";
    omnibus.rows.resize(1);
    layoutState.tables = {descriptives, omnibus};
    const double resultsOnlyHeight = MeanComparisonVisiblePreferredHeight(
        layoutState, false, false);
    const double fullHeight = MeanComparisonVisiblePreferredHeight(
        layoutState, true, false);
    const double descriptivesOnlyHeight = MeanComparisonVisiblePreferredHeight(
        layoutState, false, true);
    assert(fullHeight > resultsOnlyHeight);
    assert(fullHeight > descriptivesOnlyHeight);
    assert(MeanComparisonPreferredHeight(layoutState) == fullHeight);
    const auto publication = MeanComparisonPublicationTable(pooledState.tables[0]);
    assert(publication.rows.size() == pooledState.tables[0].rows.size());
    assert(publication.rows[0][1].number == *pooledRow.cells[0].numericValue);
    LatexPublicationOptions apa;
    apa.apa7 = true; apa.tableNumber = "3"; apa.title = "Comparison of Groups";
    const auto apaCode = BuildLatexPublicationRCode({publication}, true, apa);
    assert(apaCode.find("caption = NULL") != std::string::npos);
    assert(apaCode.find("theme = \"empty\"") != std::string::npos);
    assert(apaCode.find("theme_latex") != std::string::npos);
    assert(apaCode.find("rowhead = 1") != std::string::npos);
    assert(apaCode.find("textit{Note.}") != std::string::npos);
    assert(apaCode.find("25.4mm") != std::string::npos);
    assert(apaCode.find("xelatex") != std::string::npos);
    assert(MeanComparisonResponseTypeAllowed("one_way_anova","numeric",false,false));
    assert(!MeanComparisonResponseTypeAllowed("one_way_anova","factor",true,false));
    assert(!MeanComparisonResponseTypeAllowed("one_way_anova","ordered",false,true));
    assert(MeanComparisonResponseTypeAllowed("independent_samples_t_test","factor",true,false));
    assert(!MeanComparisonResponseTypeAllowed("independent_samples_t_test","factor",true,true));
    return 0;
}
