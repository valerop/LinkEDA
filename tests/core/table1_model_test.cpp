#include "../../src/core/table1_model.h"

#include <cassert>
#include <cmath>
#include <map>
#include <set>
#include <string>
#include <vector>

using rlispstat::core::ContingencyRowsForCell;
using rlispstat::core::ContingencyRowsForRowPrefix;
using rlispstat::core::ContingencyRowsForSelection;
using rlispstat::core::ContingencyRowsForSplitColumn;
using rlispstat::core::ContingencySelection;
using rlispstat::core::ContingencySelectionKind;
using rlispstat::core::ContingencyCellCoverage;
using rlispstat::core::ContingencyRowCoverage;
using rlispstat::core::AggregateCoverage;
using rlispstat::core::AnalysisScopeSourceKind;
using rlispstat::core::BuildTable1ReportLayout;
using rlispstat::core::DataColumn;
using rlispstat::core::DataFrameModel;
using rlispstat::core::DefaultTable1ContextMenuTitles;
using rlispstat::core::ExplicitAnalysisScope;
using rlispstat::core::NestedContingencyTableStateForDataFrame;
using rlispstat::core::Point;
using rlispstat::core::Table1CountPercent;
using rlispstat::core::Table1CSVText;
using rlispstat::core::Table1DisplayLabelText;
using rlispstat::core::Table1DisplayRow;
using rlispstat::core::Table1DisplayState;
using rlispstat::core::Table1BarChartMenuTitle;
using rlispstat::core::Table1BoxplotMenuTitle;
using rlispstat::core::Table1HistogramMenuTitle;
using rlispstat::core::Table1InferType;
using rlispstat::core::Table1Levels;
using rlispstat::core::Table1MarkdownText;
using rlispstat::core::Table1MeanSd;
using rlispstat::core::Table1MedianIqr;
using rlispstat::core::Table1NumericValuesForRows;
using rlispstat::core::Table1PlainText;
using rlispstat::core::Table1Quantile;
using rlispstat::core::Table1RemoveVariableTitle;
using rlispstat::core::Table1RowIsIndented;
using rlispstat::core::Table1RowIsParent;
using rlispstat::core::Table1StateForDataFrame;
using rlispstat::core::Table1Subtitle;
using rlispstat::core::Table1ExportedStatus;
using rlispstat::core::Table1ExportFailedStatus;
using rlispstat::core::Table1ExportedPDFStatus;
using rlispstat::core::Table1ExportPDFFailedStatus;
using rlispstat::core::Table1CopiedStatus;
using rlispstat::core::Table1StatusFieldHint;
using rlispstat::core::Table1TypeMenuTitle;
using rlispstat::core::Table1ReportVariableHeader;
using rlispstat::core::Table1ReportPHeader;
using rlispstat::core::Table1ReportTestHeader;
using rlispstat::core::Table1ContingencySelectionAtPoint;
using rlispstat::core::Table1PreferredHeight;
using rlispstat::core::Table1ReportColumnAtPoint;
using rlispstat::core::Table1ReportPointIsColumnHeader;
using rlispstat::core::Table1ReportRowAtPoint;
using rlispstat::core::Table1RowHasValues;

static bool closeEnough(double a, double b)
{
    return std::fabs(a - b) < 1.0e-9;
}

static std::size_t findNestedRow(const Table1DisplayState &state,
                                 const std::vector<std::string> &levels)
{
    for (std::size_t i = 0; i < state.rows.size(); ++i) {
        if (state.rows[i].contingencyRowKey.x_levels == levels) {
            return i;
        }
    }
    return state.rows.size();
}

int main()
{
    DataFrameModel df;
    df.group = "demo";
    df.rows = 6;

    DataColumn group;
    group.name = "group";
    group.type = "factor";
    group.values = {"A", "A", "A", "B", "B", "B"};
    df.columns.push_back(group);

    DataColumn age;
    age.name = "age";
    age.type = "numeric";
    age.values = {"10", "12", "14", "20", "22", "NA"};
    df.columns.push_back(age);

    DataColumn sex;
    sex.name = "sex";
    sex.type = "factor";
    sex.values = {"F", "M", "F", "M", "M", "F"};
    df.columns.push_back(sex);

    DataColumn grade;
    grade.name = "grade";
    grade.type = "numeric";
    grade.values = {"1", "2", "3", "1", "2", "3"};
    df.columns.push_back(grade);

    assert(Table1InferType(age, {}) == "numeric");
    assert(Table1InferType(sex, {}) == "categorical");
    assert(Table1InferType(grade, {}) == "ordinal");
    assert(Table1InferType(age, {{"age", "categorical"}}) == "categorical");

    std::vector<double> ages = Table1NumericValuesForRows(age, {0, 1, 2, 3, 4, 5});
    assert(ages.size() == 5);
    assert(Table1MeanSd({10.0, 12.0, 14.0}) == "12.0 (2.0)");
    assert(Table1MedianIqr({10.0, 12.0, 14.0}) == "12.0 [11.0, 13.0]");
    assert(closeEnough(Table1Quantile({10.0, 12.0, 14.0}, 0.25), 11.0));
    assert(Table1Levels(sex, "categorical") == std::vector<std::string>({"F", "M"}));
    assert(Table1Levels(grade, "ordinal") == std::vector<std::string>({"1", "2", "3"}));
    assert(Table1CountPercent(sex, {0, 1, 2}, "F") == "2 (66.7%)");
    auto menuTitles = DefaultTable1ContextMenuTitles();
    assert(menuTitles.root == "Table 1");
    assert(menuTitles.copyTable == "Copy Table");
    assert(menuTitles.exportPDF == "PDF...");
    assert(menuTitles.noAvailableVariables == "No available variables");
    assert(Table1RemoveVariableTitle("age") == "Remove age");
    assert(Table1TypeMenuTitle("ordinal") == "Treat as ordinal");
    assert(Table1HistogramMenuTitle("age") == "Histogram of age");
    assert(Table1BarChartMenuTitle("sex") == "Bar chart of sex");
    assert(Table1BoxplotMenuTitle("age", "") == "Boxplot of age");
    assert(Table1BoxplotMenuTitle("age", "group") == "Boxplot of age by group");

    Table1DisplayState state = Table1StateForDataFrame(
        df,
        "table1_demo",
        {"age", "sex", "grade"},
        "group",
        {});
    assert(state.id == "table1_demo");
    assert(state.datasetId == "demo");
    assert(state.groupVariable == "group");
    assert(state.showP);
    assert(state.showTest);
    assert(state.columns == std::vector<std::string>({"Overall", "A", "B"}));
    assert(!state.rows.empty());
    assert(state.rows[0].label == "N");
    assert(state.rows[0].values == std::vector<std::string>({"6", "3", "3"}));

    const auto scopedRows = ExplicitAnalysisScope(
        "demo", {1, 2, 3}, AnalysisScopeSourceKind::CurrentSelection,
        "First three rows", 6);
    Table1DisplayState scopedState = Table1StateForDataFrame(
        df, "table1_scoped", {"age", "sex"}, "group", {}, &scopedRows);
    assert(scopedState.dataScopeCaptured);
    assert(scopedState.rows[0].values == std::vector<std::string>({"3", "3", "0"}));
    assert(Table1Subtitle(scopedState).find("Scope:") != std::string::npos);
    assert(Table1CSVText(scopedState).find("Analysis scope") != std::string::npos);

    bool sawAge = false;
    bool sawSexLevel = false;
    bool sawGradeMedian = false;
    bool sawGradeLevel = false;
    for (const Table1DisplayRow &row : state.rows) {
        if (row.variable == "age" && row.rowType == "numeric_mean_sd") {
            sawAge = true;
            assert(row.values[1] == "12.0 (2.0)");
            assert(row.test == "Welch t");
            assert(!row.p.empty());
        }
        if (row.variable == "sex" && row.level == "F") {
            sawSexLevel = true;
            assert(row.values[1] == "2 (66.7%)");
        }
        if (row.variable == "grade" && row.rowType == "ordinal_median_iqr") {
            sawGradeMedian = true;
        }
        if (row.variable == "grade" && row.rowType == "ordinal_level" && row.level == "1") {
            sawGradeLevel = true;
            assert(row.values == std::vector<std::string>({"2 (33.3%)", "1 (33.3%)", "1 (33.3%)"}));
            assert(Table1RowHasValues(row));
        }
    }
    assert(sawAge);
    assert(sawSexLevel);
    assert(sawGradeMedian);
    assert(sawGradeLevel);

    assert(Table1RowIsParent(Table1DisplayRow{0, "categorical_parent"}));
    assert(Table1RowIsIndented(Table1DisplayRow{0, "categorical_level"}));
    assert(Table1RowHasValues(Table1DisplayRow{0, "ordinal_level"}));
    assert(Table1RowIsIndented(Table1DisplayRow{0, "missing"}));
    assert(Table1DisplayLabelText(Table1DisplayRow{0, "categorical_level", "", "", "  Female"}) == "Female");
    assert(Table1DisplayLabelText(Table1DisplayRow{0, "continuous_mean_sd", "", "", "Age"}) == "Age");

    std::string plain = Table1PlainText(state);
    assert(plain.find("Table 1. Descriptive statistics") != std::string::npos);
    assert(Table1Subtitle(state) == "Dataset: demo    Group: group");
    assert(Table1Subtitle(state, true) == "Dataset: demo    Group: group");
    assert(plain.find("Dataset: demo    Group: group") != std::string::npos);
    assert(plain.find("Variable\tOverall\tA\tB\tp\tTest") != std::string::npos);

    Table1DisplayState ungroupedState = state;
    ungroupedState.groupVariable.clear();
    ungroupedState.columns = {"Overall"};
    ungroupedState.rows = {Table1DisplayRow{0, "n", "", "", "N", {}, {"6"}, {}, {}, {}, "", "", ""}};
    assert(Table1Subtitle(ungroupedState) == "Dataset: demo");
    assert(Table1Subtitle(ungroupedState, true) == "Dataset: demo    N = 6");

    std::string csv = Table1CSVText(state);
    assert(csv.find("\"Variable\",\"Overall\",\"A\",\"B\",\"p\",\"Test\"") == 0);
    assert(csv.find("\"N\",\"6\",\"3\",\"3\"") != std::string::npos);

    std::string markdown = Table1MarkdownText(state);
    assert(markdown.find("### Table 1. Descriptive statistics") == 0);
    assert(markdown.find("| Variable | Overall | A | B | p | Test |") != std::string::npos);

    assert(Table1ExportedStatus() == "Exported Table 1.");
    assert(Table1ExportFailedStatus() == "Could not export Table 1.");
    assert(Table1ExportedPDFStatus() == "Exported Table 1 PDF.");
    assert(Table1ExportPDFFailedStatus() == "Could not export Table 1 PDF.");
    assert(Table1CopiedStatus() == "Copied Table 1.");
    assert(Table1StatusFieldHint() == "Right-click variables to change type, grouping, or copy the table.");
    assert(Table1ReportVariableHeader() == "Variable");
    assert(Table1ReportPHeader() == "p");
    assert(Table1ReportTestHeader() == "Test");

    Table1DisplayState nested = NestedContingencyTableStateForDataFrame(
        df,
        "nested_demo",
        {"group", "sex"},
        "grade",
        "count");
    assert(nested.tableType == "nested_contingency");
    assert(nested.datasetId == "demo");
    assert(nested.columns == std::vector<std::string>({"1", "2", "3", "Total"}));
    assert(closeEnough(Table1PreferredHeight(nested),
                       48.0 + static_cast<double>(nested.rows.size()) * 24.0 + 72.0 + 14.0 + 18.0));

    auto nestedLayout = BuildTable1ReportLayout(nested, 720.0);
    assert(nestedLayout.hasStubs);
    assert(nestedLayout.hasSpanningHeader);
    assert(Table1ReportPointIsColumnHeader(nestedLayout, Point{nestedLayout.margin + nestedLayout.stubArea + 2.0, 40.0}));
    assert(Table1ReportColumnAtPoint(nested, nestedLayout, Point{nestedLayout.margin + nestedLayout.stubArea + 2.0, 40.0}) == 0);
    assert(Table1ReportRowAtPoint(nested, nestedLayout, Point{nestedLayout.margin, nestedLayout.headerOffset + 2.0}) == 0);

    std::set<int> groupARows = ContingencyRowsForRowPrefix(nested, {"A"});
    assert(groupARows == std::set<int>({1, 2, 3}));

    std::set<int> leafRows = ContingencyRowsForRowPrefix(nested, {"A", "F"});
    assert(leafRows == std::set<int>({1, 3}));

    std::set<int> splitGradeTwoRows = ContingencyRowsForSplitColumn(nested, 1);
    assert(splitGradeTwoRows == std::set<int>({2, 5}));

    std::size_t afRowIndex = nested.rows.size();
    for (std::size_t i = 0; i < nested.rows.size(); ++i) {
        if (nested.rows[i].contingencyRowKey.x_levels == std::vector<std::string>({"A", "F"})) {
            afRowIndex = i;
            break;
        }
    }
    assert(afRowIndex < nested.rows.size());
    auto headerHit = Table1ContingencySelectionAtPoint(
        nested,
        nestedLayout,
        Point{nestedLayout.margin + nestedLayout.stubArea + 2.0, 40.0},
        -1);
    assert(headerHit.kind == ContingencySelectionKind::SplitColumn);
    assert(headerHit.columnIndex == 0);
    auto prefixHit = Table1ContingencySelectionAtPoint(
        nested,
        nestedLayout,
        Point{nestedLayout.margin + 2.0, nestedLayout.headerOffset + static_cast<double>(afRowIndex) * nestedLayout.rowHeight + 2.0},
        static_cast<int>(afRowIndex));
    assert(prefixHit.kind == ContingencySelectionKind::RowPrefix);
    assert(prefixHit.x_prefix == std::vector<std::string>({"A"}));
    auto cellHit = Table1ContingencySelectionAtPoint(
        nested,
        nestedLayout,
        Point{nestedLayout.margin + nestedLayout.stubArea + nestedLayout.columnWidth * 2.0 + 2.0,
              nestedLayout.headerOffset + static_cast<double>(afRowIndex) * nestedLayout.rowHeight + 2.0},
        static_cast<int>(afRowIndex));
    assert(cellHit.kind == ContingencySelectionKind::Cell);
    assert(cellHit.rowIndex == afRowIndex);
    assert(cellHit.columnIndex == 2);
    assert(nested.rows[afRowIndex].values[1] == "\u2014");
    assert(nested.rows[afRowIndex].values[3] == "2");
    assert(ContingencyRowsForCell(nested, afRowIndex, 0) == std::set<int>({1}));
    assert(ContingencyRowsForCell(nested, afRowIndex, 1).empty());
    assert(ContingencyRowsForCell(nested, afRowIndex, 3) == std::set<int>({1, 3}));

    ContingencySelection prefixSelection;
    prefixSelection.kind = ContingencySelectionKind::RowPrefix;
    prefixSelection.x_prefix = {"B"};
    assert(ContingencyRowsForSelection(nested, prefixSelection) == std::set<int>({4, 5, 6}));

    ContingencySelection cellSelection;
    cellSelection.kind = ContingencySelectionKind::Cell;
    cellSelection.rowIndex = afRowIndex;
    cellSelection.columnIndex = 2;
    assert(ContingencyRowsForSelection(nested, cellSelection) == std::set<int>({3}));

    DataFrameModel mtcarsLike;
    mtcarsLike.group = "mtcars_like";
    mtcarsLike.rows = 24;
    DataColumn cyl;
    cyl.name = "cyl";
    cyl.type = "factor";
    DataColumn vs;
    vs.name = "vs";
    vs.type = "factor";
    DataColumn am;
    am.name = "am";
    am.type = "factor";
    auto addRows = [&](int n, const std::string &c, const std::string &v, const std::string &a) {
        for (int i = 0; i < n; ++i) {
            cyl.values.push_back(c);
            vs.values.push_back(v);
            am.values.push_back(a);
        }
    };
    addRows(1, "4", "0", "1");
    addRows(3, "4", "1", "0");
    addRows(7, "4", "1", "1");
    addRows(3, "6", "0", "1");
    addRows(4, "6", "1", "0");
    addRows(12, "8", "0", "0");
    addRows(2, "8", "0", "1");
    mtcarsLike.rows = static_cast<int>(cyl.values.size());
    mtcarsLike.columns = {cyl, vs, am};

    Table1DisplayState nestedMtcars = NestedContingencyTableStateForDataFrame(
        mtcarsLike,
        "nested_mtcars",
        {"cyl", "vs"},
        "am",
        "count");
    assert(nestedMtcars.columns == std::vector<std::string>({"0", "1", "Total"}));
    std::size_t cyl60 = findNestedRow(nestedMtcars, {"6", "0"});
    std::size_t cyl61 = findNestedRow(nestedMtcars, {"6", "1"});
    std::size_t cyl81 = findNestedRow(nestedMtcars, {"8", "1"});
    assert(cyl60 < nestedMtcars.rows.size());
    assert(cyl61 < nestedMtcars.rows.size());
    assert(cyl81 < nestedMtcars.rows.size());
    assert(nestedMtcars.rows[cyl60].values == std::vector<std::string>({"\u2014", "3", "3"}));
    assert(nestedMtcars.rows[cyl61].values == std::vector<std::string>({"4", "\u2014", "4"}));
    assert(nestedMtcars.rows[cyl81].values == std::vector<std::string>({"\u2014", "\u2014", "\u2014"}));

    Table1DisplayState nestedMtcarsCountPercent = NestedContingencyTableStateForDataFrame(
        mtcarsLike,
        "nested_mtcars_count_percent",
        {"cyl", "vs"},
        "am",
        "count_percent");
    std::size_t cyl60CountPercent = findNestedRow(nestedMtcarsCountPercent, {"6", "0"});
    std::size_t cyl81CountPercent = findNestedRow(nestedMtcarsCountPercent, {"8", "1"});
    assert(cyl60CountPercent < nestedMtcarsCountPercent.rows.size());
    assert(cyl81CountPercent < nestedMtcarsCountPercent.rows.size());
    assert(nestedMtcarsCountPercent.rows[cyl60CountPercent].values[0] == "\u2014");
    assert(nestedMtcarsCountPercent.rows[cyl60CountPercent].values[1] == "3 (100.0%)");
    assert(nestedMtcarsCountPercent.rows[cyl81CountPercent].values == std::vector<std::string>({"\u2014", "\u2014", "\u2014"}));

    std::set<int> cyl6Rows = ContingencyRowsForRowPrefix(nestedMtcars, {"6"});
    assert(cyl6Rows.size() == 7);
    assert(cyl6Rows == std::set<int>({12, 13, 14, 15, 16, 17, 18}));
    assert(ContingencyRowsForCell(nestedMtcars, cyl60, 0).empty());
    assert(ContingencyRowsForCell(nestedMtcars, cyl60, 1) == std::set<int>({12, 13, 14}));
    assert(ContingencyRowsForCell(nestedMtcars, cyl61, 0) == std::set<int>({15, 16, 17, 18}));
    assert(ContingencyRowsForCell(nestedMtcars, cyl61, 1).empty());

    assert(ContingencyCellCoverage(nestedMtcars, cyl60, 0, cyl6Rows) == AggregateCoverage::Empty);
    assert(ContingencyCellCoverage(nestedMtcars, cyl60, 1, cyl6Rows) == AggregateCoverage::Full);
    assert(ContingencyCellCoverage(nestedMtcars, cyl61, 0, cyl6Rows) == AggregateCoverage::Full);
    assert(ContingencyCellCoverage(nestedMtcars, cyl61, 1, cyl6Rows) == AggregateCoverage::Empty);
    assert(ContingencyRowCoverage(nestedMtcars, cyl60, cyl6Rows) == AggregateCoverage::Full);
    assert(ContingencyRowCoverage(nestedMtcars, cyl61, cyl6Rows) == AggregateCoverage::Full);

    std::set<int> allRows;
    for (int i = 1; i <= mtcarsLike.rows; ++i) allRows.insert(i);
    assert(ContingencyCellCoverage(nestedMtcars, cyl60, 0, allRows) == AggregateCoverage::Empty);
    assert(ContingencyCellCoverage(nestedMtcars, cyl61, 1, allRows) == AggregateCoverage::Empty);
    assert(ContingencyCellCoverage(nestedMtcars, cyl61, 0, {15, 16}) == AggregateCoverage::Partial);

    ContingencySelection zeroCellSelection;
    zeroCellSelection.kind = ContingencySelectionKind::Cell;
    zeroCellSelection.rowIndex = cyl60;
    zeroCellSelection.columnIndex = 0;
    assert(ContingencyRowsForSelection(nestedMtcars, zeroCellSelection).empty());

    DataFrameModel missingTable;
    missingTable.group = "missing_table";
    missingTable.rows = 5;
    DataColumn rowVariable;
    rowVariable.name = "row";
    rowVariable.type = "factor";
    rowVariable.values = {"A", "A", "B", "B", "NA"};
    DataColumn columnVariable;
    columnVariable.name = "column";
    columnVariable.type = "factor";
    columnVariable.values = {"X", "NA", "X", "Y", "Y"};
    missingTable.columns = {rowVariable, columnVariable};
    Table1DisplayState missingContingency = NestedContingencyTableStateForDataFrame(
        missingTable, "missing_contingency", {"row"}, "column", "count");
    assert(missingContingency.rows.back().rowType == "nested_total");
    assert(missingContingency.rows.back().stubValues.front() == "Total");
    assert(missingContingency.rows.back().values == std::vector<std::string>({"2", "1", "3"}));
    assert(missingContingency.rows.back().rowRows == std::vector<int>({1, 3, 4}));
    assert(missingContingency.footnotes.back() == "N = 3, 2 excluded.");
    assert(ContingencyRowsForCell(missingContingency, missingContingency.rows.size() - 1, 0) ==
           std::set<int>({1, 3}));

    Table1DisplayState rawExport;
    rawExport.columns = {"Estimate"};
    Table1DisplayRow rawRow;
    rawRow.rowType = "numeric_level";
    rawRow.label = "Result";
    rawRow.values = {"1.235"};
    rawRow.rawValues = {"1.23456789012345"};
    rawExport.rows = {rawRow};
    assert(Table1CSVText(rawExport).find("1.23456789012345") != std::string::npos);
    return 0;
}
