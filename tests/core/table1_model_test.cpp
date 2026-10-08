#include "../../src/core/table1_model.h"

#include "../../src/core/command_dispatcher.h"
#include <fstream>
#include <iostream>
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
using rlispstat::core::Table1DisplayPreferences;
using rlispstat::core::Table1DisplayRow;
using rlispstat::core::Table1DisplayState;
using rlispstat::core::Table1ApplyDisplayPreferences;
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
using rlispstat::core::Table1PlotMenuOptions;
using rlispstat::core::Table1Quantile;
using rlispstat::core::Table1RemoveVariableTitle;
using rlispstat::core::Table1ReplaceVariableTitle;
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
using rlispstat::core::Table1VariableAnalysisType;
using rlispstat::core::Table1ReportVariableHeader;
using rlispstat::core::Table1ReportPHeader;
using rlispstat::core::Table1ReportTestHeader;
using rlispstat::core::Table1ContingencySelectionAtPoint;
using rlispstat::core::Table1PreferredHeight;
using rlispstat::core::Table1ReportColumnAtPoint;
using rlispstat::core::Table1ReportPointIsColumnHeader;
using rlispstat::core::Table1ReportRowAtPoint;
using rlispstat::core::Table1RowHasValues;
using rlispstat::core::Table1NaturalWidth;

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


static std::string fixtureFolder;
// Layout and linking are exercised with real R payloads, not native statistics.
static Table1DisplayState RContingency(const DataFrameModel& df,const std::string& id,
    const std::vector<std::string>& variables,const std::string& split,const std::string& mode) {
    auto pending=NestedContingencyTableStateForDataFrame(df,id,variables,split,mode);
    assert(pending.needsRFit && pending.rows.empty() && !pending.nativeGenerated);
    assert(pending.variables==variables && pending.groupVariable==split && pending.nestedDisplayMode==mode);
    Table1DisplayState result;rlispstat::core::CommandDispatcherServices services;
    services.ui.showTable1=[&](const auto& s){result=s;};rlispstat::core::CommandDispatcher dispatcher(services);
    for(const auto suffix:{"-dataset",""}) {
        std::ifstream in(fixtureFolder+"/"+id+suffix+".payload");assert(in);
        std::vector<std::string> lines;std::string line;while(std::getline(in,line))lines.push_back(line);
        auto reply=dispatcher.dispatch(lines);if(reply.rfind("OK",0)!=0)std::cerr<<reply<<'\n';assert(reply.rfind("OK",0)==0);
    }
    assert(!result.codeReference.provenance.executedRCode.empty());
    assert(result.codeReference.publication.table);
    return result;
}
int main(int argc,char** argv)
{
    assert(argc==2);fixtureFolder=argv[1];
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

    DataColumn notes;
    notes.name = "notes";
    notes.type = "character";
    notes.values = {"free text a", "free text b", "free text c",
                    "free text d", "free text e", "free text f"};
    df.columns.push_back(notes);

    assert(Table1InferType(age, {}) == "numeric");
    assert(Table1InferType(sex, {}) == "categorical");
    assert(Table1InferType(grade, {}) == "numeric");
    assert(Table1InferType(notes, {}) == "unsupported");
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
    assert(menuTitles.changeVariable == "Replace variable");
    assert(Table1RemoveVariableTitle("age") == "Remove age");
    assert(Table1ReplaceVariableTitle("age") == "Replace age with...");
    assert(Table1TypeMenuTitle("ordinal") == "Treat as ordinal");
    assert(Table1HistogramMenuTitle("age") == "Histogram of age");
    assert(Table1BarChartMenuTitle("sex") == "Bar chart of sex");
    assert(Table1BoxplotMenuTitle("age", "") == "Boxplot of age");
    assert(Table1BoxplotMenuTitle("age", "group") == "Boxplot of age by group");
    const auto numericPlots = Table1PlotMenuOptions("age", "numeric", "group");
    assert(numericPlots.size() == 2);
    assert(numericPlots[0].command == "TABLE1_OPEN_HISTOGRAM");
    assert(numericPlots[1].title == "Boxplot of age by group");
    const auto categoricalPlots = Table1PlotMenuOptions("sex", "categorical", "");
    assert(categoricalPlots.size() == 1);
    assert(categoricalPlots[0].command == "TABLE1_OPEN_BARPLOT");

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
    assert(Table1VariableAnalysisType(state, "age") == "numeric");
    assert(Table1VariableAnalysisType(state, "sex") == "categorical");
    assert(Table1VariableAnalysisType(state, "grade") == "numeric");

    // Empty is an intentional editable state.  Table 1 must not silently
    // choose the first dataset columns when initialization is unresolved.
    Table1DisplayState blankState = Table1StateForDataFrame(
        df, "table1_blank", {}, "", {});
    assert(blankState.variables.empty());
    assert(blankState.rows.size() == 1);
    assert(blankState.rows[0].label == "N");

    const auto scopedRows = ExplicitAnalysisScope(
        "demo", {1, 2, 3}, AnalysisScopeSourceKind::CurrentSelection,
        "First three rows", 6);
    Table1DisplayState scopedState = Table1StateForDataFrame(
        df, "table1_scoped", {"age", "sex"}, "group", {}, &scopedRows);
    assert(scopedState.dataScopeCaptured);
    assert(scopedState.columns == std::vector<std::string>({"Overall", "A"}));
    assert(scopedState.rows[0].values == std::vector<std::string>({"3", "3"}));
    assert(Table1Subtitle(scopedState).find("First three rows") != std::string::npos);
    assert(Table1CSVText(scopedState).find("Analysis scope") != std::string::npos);

    bool sawAge = false;
    bool sawSexLevel = false;
    bool sawGradeNumeric = false;
    for (const Table1DisplayRow &row : state.rows) {
        if (row.variable == "age" && row.rowType == "numeric_mean_sd") {
            sawAge = true;
            assert(row.values[1] == "12.0 (2.0)");
            assert(row.statisticValues.size() == state.columns.size());
            assert(row.statisticValues[1].at("mean") == "12.0");
            assert(row.statisticValues[1].at("sd") == "2.0");
            assert(row.statisticValues[1].at("se") == "1.2");
            assert(row.statisticValues[1].at("ci95") == "[7.2, 16.8]");
            assert(row.test == "Welch t");
            assert(!row.p.empty());
        }
        if (row.variable == "sex" && row.level == "F") {
            sawSexLevel = true;
            assert(row.values[1] == "2 (66.7%)");
        }
        if (row.variable == "grade" && row.rowType == "numeric_mean_sd") {
            sawGradeNumeric = true;
            assert(row.values[0] == "2.0 (0.9)");
        }
    }
    assert(sawAge);
    assert(sawSexLevel);
    assert(sawGradeNumeric);

    // Display choices derive a new view from the immutable statistical
    // result. They do not change the source rows and are also honoured by
    // copy/export helpers, which consume the derived display state.
    Table1DisplayPreferences compactPreferences;
    compactPreferences.showOverall = false;
    compactPreferences.showMissing = false;
    compactPreferences.showSD = false;
    compactPreferences.showSE = false;
    compactPreferences.showCI95 = true;
    compactPreferences.showP = false;
    compactPreferences.showTest = false;
    Table1DisplayState compact = Table1ApplyDisplayPreferences(state, compactPreferences);
    assert(compact.columns == std::vector<std::string>({"A", "B"}));
    assert(!compact.showP);
    assert(!compact.showTest);
    bool sawCI = false;
    for (const Table1DisplayRow &row : compact.rows) {
        assert(row.rowType != "missing");
        if (row.variable == "age" && row.rowType == "numeric_mean_sd") {
            sawCI = true;
            assert(row.values[0] == "12.0 (95% CI [7.2, 16.8])");
        }
    }
    assert(sawCI);
    const std::string compactCSV = Table1CSVText(compact);
    assert(compactCSV.find("\"Overall\"") == std::string::npos);
    assert(compactCSV.find("\"p\"") == std::string::npos);
    assert(compactCSV.find("Missing") == std::string::npos);
    // The source remains complete, so every hidden option can be restored
    // without asking R or mice to fit/summarize anything again.
    assert(state.columns.front() == "Overall");
    assert(state.showP && state.showTest);
    assert(Table1ApplyDisplayPreferences(state, Table1DisplayPreferences{}).rows.size() ==
           state.rows.size());

    Table1DisplayState longTable = state;
    longTable.rows.front().label =
        "A deliberately long descriptive variable label that must remain visible";
    longTable.rows.front().test =
        "A deliberately long inferential method description that must not be truncated";
    const auto narrowLayout = BuildTable1ReportLayout(longTable, 320.0);
    assert(Table1NaturalWidth(longTable) > 320.0);
    assert(narrowLayout.width == Table1NaturalWidth(longTable));
    assert(narrowLayout.variableWidth > 360.0);
    assert(narrowLayout.testWidth > 360.0);
    assert(narrowLayout.valueColumnWidths.size() == longTable.columns.size());
    assert(narrowLayout.valueColumnWidths[0] == narrowLayout.valueColumnWidths[1]);
    assert(narrowLayout.valueColumnWidths[1] == narrowLayout.valueColumnWidths[2]);

    Table1DisplayState convergence;
    convergence.tableType = "mi_diagnostics";
    convergence.columns = {"Statistic", "Iterations", "Chains",
        "Lag-1 autocorrelation", "PSRF (classical)", "PSRF upper 95%", "Status"};
    Table1DisplayRow convergenceRow;
    convergenceRow.label = "bmi";
    convergenceRow.values = {"Mean", "5", "5", "\u2014", "\u2014", "\u2014",
        "Install package 'coda' to compute convergence diagnostics"};
    convergence.rows.push_back(convergenceRow);
    const auto convergenceLayout = BuildTable1ReportLayout(convergence, 0.0);
    assert(convergenceLayout.valueColumnWidths.size() == convergence.columns.size());
    assert(convergenceLayout.valueColumnWidths.back() == 240.0);
    assert(convergenceLayout.width < 1280.0);

    auto categoricalLevels = [](const Table1DisplayState &table,
                                const std::string &variable) {
        std::vector<std::string> result;
        for (const Table1DisplayRow &row : table.rows) {
            if (row.variable == variable && row.rowType == "categorical_level")
                result.push_back(row.level);
        }
        return result;
    };
    assert(categoricalLevels(state, "sex") ==
           std::vector<std::string>({"F", "M"}));

    // Defined factor metadata determines row identity/order, including unused
    // levels. Group columns keep that order but omit unused groups, matching R.
    DataFrameModel categoricalFrame;
    categoricalFrame.group = "categorical_levels";
    categoricalFrame.rows = 10;
    DataColumn happy;
    happy.name = "happy";
    happy.type = "factor";
    happy.values = {"1", "1", "1", "1", "1", "2", "2", "2", "2", "2"};
    happy.displayValues = {"no", "no", "no", "no", "no",
                           "yes", "yes", "yes", "yes", "yes"};
    happy.definedLevels = {"no", "yes", "unused_group"};
    DataColumn planet;
    planet.name = "planet";
    planet.type = "factor";
    planet.values = {"2", "1", "3", "2", "1", "3", "2", "1", "3", "2"};
    planet.displayValues = {"Aurelia", "Cygnus", "Borealis", "Aurelia", "Cygnus",
                            "Borealis", "Aurelia", "Cygnus", "Borealis", "Aurelia"};
    planet.definedLevels = {"Cygnus", "Aurelia", "Borealis"};
    DataColumn colour;
    colour.name = "colour";
    colour.type = "factor";
    colour.values = {"1", "2", "3", "4", "1", "2", "3", "4", "1", "2"};
    colour.displayValues = {"red", "blue", "green", "amber", "red",
                            "blue", "green", "amber", "red", "blue"};
    colour.definedLevels = {"green", "red", "blue", "amber", "violet"};
    categoricalFrame.columns = {happy, planet, colour};

    Table1DisplayState groupedCategorical = Table1StateForDataFrame(
        categoricalFrame, "table1_categorical_grouped", {"planet", "colour"}, "happy", {});
    assert(groupedCategorical.columns ==
           std::vector<std::string>({"Overall", "no", "yes"}));
    assert(categoricalLevels(groupedCategorical, "planet") ==
           std::vector<std::string>({"Cygnus", "Aurelia", "Borealis"}));
    assert(categoricalLevels(groupedCategorical, "colour") ==
           std::vector<std::string>({"green", "red", "blue", "amber", "violet"}));
    std::set<int> categoricalRowIds;
    for (const Table1DisplayRow &row : groupedCategorical.rows) {
        assert(categoricalRowIds.insert(row.rowIndex).second);
        if (row.rowType == "categorical_level")
            assert(row.values.size() == groupedCategorical.columns.size());
    }

    Table1DisplayState ungroupedCategorical = Table1StateForDataFrame(
        categoricalFrame, "table1_categorical_ungrouped", {"planet", "colour"}, "", {});
    assert(ungroupedCategorical.columns == std::vector<std::string>({"Overall"}));
    assert(categoricalLevels(ungroupedCategorical, "planet") ==
           categoricalLevels(groupedCategorical, "planet"));
    assert(categoricalLevels(ungroupedCategorical, "colour") ==
           categoricalLevels(groupedCategorical, "colour"));

    assert(Table1RowIsParent(Table1DisplayRow{0, "categorical_parent"}));
    assert(Table1RowIsIndented(Table1DisplayRow{0, "categorical_level"}));
    assert(Table1RowHasValues(Table1DisplayRow{0, "ordinal_level"}));
    assert(Table1RowHasValues(Table1DisplayRow{0, "text"}));
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
    ungroupedState.rows[0].rowType = "text";
    assert(Table1Subtitle(ungroupedState, true) == "Dataset: demo");

    std::string csv = Table1CSVText(state);
    assert(csv.find("\"Variable\",\"Overall\",\"A\",\"B\",\"p\",\"Test\"") == 0);
    assert(csv.find("\"N\",\"6\",\"3\",\"3\"") != std::string::npos);

    std::string markdown = Table1MarkdownText(state);
    assert(markdown.find("### Table 1. Descriptive statistics") == 0);
    assert(markdown.find("| Variable | Overall | A | B | p | Test |") != std::string::npos);
    assert(markdown.find("&#160;&#160;F") != std::string::npos);

    assert(Table1ExportedStatus() == "Exported Table 1.");
    assert(Table1ExportFailedStatus() == "Could not export Table 1.");
    assert(Table1ExportedPDFStatus() == "Exported Table 1 PDF.");
    assert(Table1ExportPDFFailedStatus() == "Could not export Table 1 PDF.");
    assert(Table1CopiedStatus() == "Copied Table 1.");
    assert(Table1StatusFieldHint() ==
        "Click a variable name to replace it; right-click for type, grouping, or export.");
    assert(Table1ReportVariableHeader() == "Variable");
    assert(Table1ReportPHeader() == "p");
    assert(Table1ReportTestHeader() == "Test");

    Table1DisplayState nested = RContingency(
        df,
        "nested_demo",
        {"group", "sex"},
        "grade",
        "count");
    assert(nested.tableType == "nested_contingency");
    assert(nested.datasetId == "demo");
    assert(nested.columns == std::vector<std::string>({"1", "2", "3", "Total"}));
    assert(closeEnough(Table1PreferredHeight(nested),
                       48.0 + static_cast<double>(nested.rows.size()) * 24.0 + 72.0 + 14.0 + 18.0 * nested.footnotes.size()));

    auto nestedLayout = BuildTable1ReportLayout(nested, 720.0);
    assert(nestedLayout.hasStubs);
    assert(nestedLayout.hasSpanningHeader);
    assert(Table1ReportPointIsColumnHeader(nestedLayout, Point{nestedLayout.margin + nestedLayout.stubArea + 2.0, 40.0}));
    assert(Table1ReportColumnAtPoint(nested, nestedLayout, Point{nestedLayout.margin + nestedLayout.stubArea + 2.0, 40.0}) == 0);
    assert(Table1ReportRowAtPoint(nested, nestedLayout, Point{nestedLayout.margin, nestedLayout.headerOffset + 2.0}) == 0);

    Table1DisplayState longStubNested = nested;
    assert(!longStubNested.rows.empty());
    assert(!longStubNested.rows.front().stubValues.empty());
    const double originalNestedWidth = Table1NaturalWidth(longStubNested);
    longStubNested.rows.front().stubValues.front() =
        "A very long nested category label that must remain completely visible";
    assert(Table1NaturalWidth(longStubNested) > originalNestedWidth);

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
    assert(nested.rows[afRowIndex].values[1] == "0");
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

    Table1DisplayState nestedMtcars = RContingency(
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
    assert(nestedMtcars.rows[cyl60].values == std::vector<std::string>({"0", "3", "3"}));
    assert(nestedMtcars.rows[cyl61].values == std::vector<std::string>({"4", "0", "4"}));
    assert(nestedMtcars.rows[cyl81].values == std::vector<std::string>({"0", "0", "0"}));

    Table1DisplayState nestedMtcarsCountPercent = RContingency(
        mtcarsLike,
        "nested_mtcars_count_percent",
        {"cyl", "vs"},
        "am",
        "count_percent");
    std::size_t cyl60CountPercent = findNestedRow(nestedMtcarsCountPercent, {"6", "0"});
    std::size_t cyl81CountPercent = findNestedRow(nestedMtcarsCountPercent, {"8", "1"});
    assert(cyl60CountPercent < nestedMtcarsCountPercent.rows.size());
    assert(cyl81CountPercent < nestedMtcarsCountPercent.rows.size());
    assert(nestedMtcarsCountPercent.rows[cyl60CountPercent].values[0] == "0 (0.0%)");
    assert(nestedMtcarsCountPercent.rows[cyl60CountPercent].values[1] == "3 (100.0%)");
    assert(nestedMtcarsCountPercent.rows[cyl81CountPercent].values == std::vector<std::string>({"0 (\u2014)", "0 (\u2014)", "0 (\u2014)"}));

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
    Table1DisplayState missingContingency = RContingency(
        missingTable, "missing_contingency", {"row"}, "column", "count");
    assert(missingContingency.rows.back().rowType == "nested_total");
    assert(missingContingency.rows.back().stubValues.front() == "Total");
    assert(missingContingency.rows.back().values == std::vector<std::string>({"2", "1", "3"}));
    assert(missingContingency.rows.back().rowRows == std::vector<int>({1, 3, 4}));
    assert(missingContingency.footnotes.front() == "N = 3 complete observations; 2 excluded within the analysis scope.");
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
    // Hidden columns must remove their full-precision values too, or exports
    // associate the Overall estimate with the first group's label.
    Table1DisplayState rawColumns;rawColumns.columns={"Overall","Control","Treatment"};
    Table1DisplayRow filteredRawRow;filteredRawRow.rowType="numeric_mean_sd";
    filteredRawRow.values={"100","20","30"};
    filteredRawRow.rawStatisticValues={{{"mean",100}},{{"mean",20}},{{"mean",30}}};
    rawColumns.rows={filteredRawRow};Table1DisplayPreferences hideOverall;hideOverall.showOverall=false;
    auto filtered=Table1ApplyDisplayPreferences(rawColumns,hideOverall);
    assert(filtered.columns==std::vector<std::string>({"Control","Treatment"}));
    assert(filtered.rows[0].rawStatisticValues.size()==2);
    assert(filtered.rows[0].rawStatisticValues[0].at("mean")==20);
    assert(filtered.rows[0].rawStatisticValues[1].at("mean")==30);

    return 0;
}
