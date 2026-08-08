#include "core/mean_comparison_model.h"

#include <cassert>
#include <cmath>
#include <string>
#include <vector>

using namespace rlispstat::core;

int main()
{
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
    assert(signedRank.tables[0].columns.back().title == "r\u1d63\u1d66");
    assert(signedRank.tables[0].columns[4].title == "Location shift");
    return 0;
}
