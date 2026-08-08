#include "../../src/core/boxplot_model.h"
#include "../../src/core/dataset_model.h"

#include <cassert>
#include <cmath>
#include <set>
#include <vector>

using rlispstat::core::BoxplotCase;
using rlispstat::core::BoxplotCaseGeometryForLayout;
using rlispstat::core::BoxplotCasePoint;
using rlispstat::core::BoxplotCategoriesForCases;
using rlispstat::core::BoxplotCategoryCenter;
using rlispstat::core::BoxplotAddVariableTitle;
using rlispstat::core::BoxplotCreationDialogState;
using rlispstat::core::BoxplotConnectionStrokeWidth;
using rlispstat::core::BoxplotDefaultTitle;
using rlispstat::core::BoxplotDisplayRange;
using rlispstat::core::BoxplotDisplayRangeForCases;
using rlispstat::core::BoxplotOptionsResponseText;
using rlispstat::core::BoxplotH0SimulationCenterX;
using rlispstat::core::BoxplotH0SimulationResult;
using rlispstat::core::BoxplotGroupOrderMenuOptions;
using rlispstat::core::BoxplotLayout;
using rlispstat::core::BoxplotMenuState;
using rlispstat::core::BoxplotNoMoreNumericVariablesTitle;
using rlispstat::core::BoxplotQuantileSorted;
using rlispstat::core::BoxplotRemoveVariableTitle;
using rlispstat::core::BoxplotStats;
using rlispstat::core::BoxplotStatsForCases;
using rlispstat::core::BoxplotStatsRenderItem;
using rlispstat::core::BoxplotTailIntervalsForAlternative;
using rlispstat::core::BoxplotValueInterval;
using rlispstat::core::BoxplotVariableListContains;
using rlispstat::core::BoxplotUsesVariableAxes;
using rlispstat::core::BoxplotVariablesAfterAdd;
using rlispstat::core::BoxplotVariablesAfterRemove;
using rlispstat::core::BoxplotVariablesAfterReplacement;
using rlispstat::core::BoxplotVariablesAvailableToAdd;
using rlispstat::core::BoxplotVariableUpdateResult;
using rlispstat::core::BoxplotViewport;
using rlispstat::core::BoxplotYAxisTicks;
using rlispstat::core::BuildBoxplotH0Simulation;
using rlispstat::core::BuildBoxplotCreationDialogState;
using rlispstat::core::BuildBoxplotConnectionLinePlan;
using rlispstat::core::BuildBoxplotMenuState;
using rlispstat::core::BuildBoxplotPointDrawPlan;
using rlispstat::core::BuildBoxplotStatsRenderPlan;
using rlispstat::core::BuildParallelBoxplotCases;
using rlispstat::core::BuildParallelCoordinatesDialogState;
using rlispstat::core::CaseId;
using rlispstat::core::DataColumn;
using rlispstat::core::DataFrameModel;
using rlispstat::core::MeanOfValues;
using rlispstat::core::NearestBoxplotCaseToPoint;
using rlispstat::core::OrderedBoxplotCategories;
using rlispstat::core::ParallelBoxplotLabelState;
using rlispstat::core::ParallelCoordinatesDialogState;
using rlispstat::core::ParallelBoxplotLabelsForVariables;
using rlispstat::core::ParallelBoxplotBuildInput;
using rlispstat::core::ParallelBoxplotBuildResult;
using rlispstat::core::Point;
using rlispstat::core::Rect;
using rlispstat::core::RebuildGroupedBoxplotPointsFromDataFrame;
using rlispstat::core::SampleSDOfValues;
using rlispstat::core::SelectBoxplotCasesForGesture;
using rlispstat::core::SelectBoxplotCasesInBrush;

static std::set<CaseId> S(std::initializer_list<CaseId> values)
{
    return std::set<CaseId>(values.begin(), values.end());
}

static bool closeEnough(double a, double b)
{
    return std::fabs(a - b) < 1.0e-9;
}

int main()
{
    BoxplotCreationDialogState creationDialog = BuildBoxplotCreationDialogState();
    assert(creationDialog.title == "Boxplot");
    assert(creationDialog.informativeText ==
           "Choose a numeric Y variable and an optional grouping variable from the active dataset.");
    assert(creationDialog.createButtonTitle == "Create");
    assert(creationDialog.cancelButtonTitle == "Cancel");
    assert(creationDialog.optionalTitlePlaceholder == "Optional title");
    assert(creationDialog.noActiveDatasetStatus ==
           "Open a LinkEDA plot first, or use ls_new_boxplot() from R.");
    assert(creationDialog.needsNumericDataStatus ==
           "The active dataset needs numeric variables and a backend data payload.");
    assert(creationDialog.selectedYMissingStatus == "Could not find the selected Y variable.");
    ParallelCoordinatesDialogState parallelDialog = BuildParallelCoordinatesDialogState();
    assert(parallelDialog.title == "Parallel Coordinates");
    assert(parallelDialog.defaultTitle == "Parallel Coordinates");
    assert(parallelDialog.informativeText.find("standardized") != std::string::npos);
    assert(parallelDialog.hintText.find("Click a variable name") != std::string::npos);
    assert(BoxplotDefaultTitle("mpg", "") == "mpg");
    assert(BoxplotDefaultTitle("mpg", "cyl") == "mpg by cyl");

    rlispstat::core::PlotModel boxplotOptions;
    boxplotOptions.boxplotShowPoints = false;
    boxplotOptions.boxplotShowBox = true;
    boxplotOptions.boxplotShowWhiskers = false;
    boxplotOptions.boxplotConnectRows = true;
    boxplotOptions.boxplotStandardizeVariables = true;
    boxplotOptions.boxplotShowViolin = true;
    boxplotOptions.boxplotSplitViolin = true;
    boxplotOptions.boxplotSplitAlternative = "greater";
    boxplotOptions.boxplotSplitLower = 1.25;
    boxplotOptions.boxplotSplitUpper = 3.5;
    boxplotOptions.boxplotShowH0Simulation = true;
    boxplotOptions.boxplotH0Alternative = "less";
    boxplotOptions.boxplotH0 = 2.0;
    boxplotOptions.boxplotH0Draws = 250;
    boxplotOptions.boxplotH0PValue = 0.0123;
    boxplotOptions.boxplotH0Effect = 0.75;
    assert(BoxplotOptionsResponseText(boxplotOptions) ==
           "OK\tFALSE|TRUE|FALSE|TRUE|TRUE|TRUE|TRUE|greater|1.250000|3.500000|TRUE|less|2.000000|250|.012|0.750000");

    BoxplotLayout layout;
    layout.plotRect = Rect{70.0, 40.0, 300.0, 200.0};
    layout.categories = {"mpg", "wt", "hp"};
    layout.yMinimum = 0.0;
    layout.yMaximum = 10.0;

    assert(closeEnough(BoxplotCategoryCenter(layout, "mpg"), 120.0));
    assert(closeEnough(BoxplotCategoryCenter(layout, "wt"), 220.0));
    assert(closeEnough(BoxplotH0SimulationCenterX(layout), BoxplotCategoryCenter(layout, "mpg")));

    BoxplotLayout twoGroupLayout = layout;
    twoGroupLayout.categories = {"A", "B"};
    twoGroupLayout.variableAxes = false;
    assert(closeEnough(BoxplotH0SimulationCenterX(twoGroupLayout), 220.0));
    twoGroupLayout.variableAxes = true;
    assert(closeEnough(BoxplotH0SimulationCenterX(twoGroupLayout), BoxplotCategoryCenter(twoGroupLayout, "A")));

    auto viewport = BoxplotViewport(layout);
    assert(closeEnough(viewport.ymin, -0.8));
    assert(closeEnough(viewport.ymax, 10.8));

    BoxplotCase c1{1, 5.0, "mpg"};
    BoxplotCase c2{2, 9.0, "wt"};
    BoxplotCase c3{3, 1.0, "hp"};
    Point p1 = BoxplotCasePoint(layout, c1);
    Point p2 = BoxplotCasePoint(layout, c2);
    assert(std::isfinite(p1.x) && std::isfinite(p1.y));
    assert(p2.y < p1.y);
    assert(std::fabs(p1.x - BoxplotCategoryCenter(layout, "mpg")) <= layout.jitterWidth / 2.0);

    layout.variableAxes = true;
    layout.connectRows = true;
    Point connected = BoxplotCasePoint(layout, c1);
    assert(closeEnough(connected.x, BoxplotCategoryCenter(layout, "mpg")));

    std::vector<BoxplotCase> cases = {c1, c2, c3, BoxplotCase{0, 4.0, "wt"}, BoxplotCase{4, NAN, "wt"}};
    BoxplotDisplayRange range = BoxplotDisplayRangeForCases(cases, false, {-20.0}, {30.0}, -40.0, 50.0);
    assert(range.hasFiniteValue);
    assert(closeEnough(range.minimum, 1.0));
    assert(closeEnough(range.maximum, 9.0));
    BoxplotDisplayRange h0Range = BoxplotDisplayRangeForCases(cases, true, {-20.0}, {30.0}, -40.0, 50.0);
    assert(h0Range.hasFiniteValue);
    assert(closeEnough(h0Range.minimum, -40.0));
    assert(closeEnough(h0Range.maximum, 50.0));
    BoxplotDisplayRange emptyRange = BoxplotDisplayRangeForCases({BoxplotCase{1, NAN, "A"}}, true, {-20.0}, {30.0}, -40.0, 50.0);
    assert(!emptyRange.hasFiniteValue);
    assert(closeEnough(emptyRange.minimum, 0.0));
    assert(closeEnough(emptyRange.maximum, 1.0));
    auto geometry = BoxplotCaseGeometryForLayout(layout, cases);
    assert(geometry.size() == 3);
    assert((BoxplotCategoriesForCases(cases) == std::vector<std::string>{"mpg", "wt", "hp"}));

    DataFrameModel groupedDf;
    groupedDf.group = "cars";
    groupedDf.rows = 5;
    groupedDf.columns = {
        DataColumn{"mpg", "numeric", "", "", -1, {"21", "bad", "23", "24", "25"}, {}, {}, {}, {}, {}, {}},
        DataColumn{"cyl", "factor", "", "", -1, {"4", "6", "", "8", "NA"}, {}, {}, {}, {}, {}, {}}
    };
    rlispstat::core::PlotModel groupedBoxplot;
    groupedBoxplot.kind = "boxplot";
    groupedBoxplot.yLabel = "mpg";
    groupedBoxplot.xLabel = "cyl";
    RebuildGroupedBoxplotPointsFromDataFrame(groupedBoxplot, groupedDf);
    assert(groupedBoxplot.boxplotPoints.size() == 4);
    assert(groupedBoxplot.boxplotPoints[0].row == 1);
    assert(groupedBoxplot.boxplotPoints[0].category == "4");
    assert(groupedBoxplot.boxplotPoints[1].row == 3);
    assert(groupedBoxplot.boxplotPoints[1].category == "NA");
    assert(groupedBoxplot.boxplotCategories == std::vector<std::string>({"4", "NA", "8"}));

    Rect brush{connected.x - 4.0, connected.y - 4.0, 8.0, 8.0};
    assert(SelectBoxplotCasesInBrush(geometry, brush) == S({1}));

    auto nearest = NearestBoxplotCaseToPoint(geometry, Point{connected.x + 1.0, connected.y + 1.0});
    assert(nearest.has_value() && *nearest == 1);

    assert(SelectBoxplotCasesForGesture(geometry, brush, Point{}, true) == S({1}));
    assert(SelectBoxplotCasesForGesture(geometry, Rect{}, Point{connected.x, connected.y}, false) == S({1}));
    assert(SelectBoxplotCasesForGesture(geometry, Rect{}, Point{0.0, 0.0}, false).empty());

    auto pointPlan = BuildBoxplotPointDrawPlan(geometry, S({2}));
    assert(pointPlan.size() == 3);
    assert(pointPlan[0].caseId == 1);
    assert(!pointPlan[0].selected);
    assert(pointPlan[0].dimmed);
    assert(pointPlan[1].caseId == 2);
    assert(pointPlan[1].selected);
    assert(!pointPlan[1].dimmed);
    assert(BuildBoxplotConnectionLinePlan(geometry, layout.categories, S({1}), true, false).empty());

    std::vector<BoxplotCase> repeatedCases = {
        {10, 2.0, "wt"},
        {10, 1.0, "mpg"},
        {11, 4.0, "hp"},
        {10, 3.0, "hp"},
        {11, 5.0, "mpg"}
    };
    auto repeatedGeometry = BoxplotCaseGeometryForLayout(layout, repeatedCases);
    auto connectionPlan = BuildBoxplotConnectionLinePlan(
        repeatedGeometry,
        layout.categories,
        S({10}),
        true,
        true);
    assert(connectionPlan.size() == 2);
    assert(connectionPlan[0].caseId == 10);
    assert(connectionPlan[0].selected);
    assert(!connectionPlan[0].dimmed);
    assert(connectionPlan[0].points.size() == 3);
    assert(connectionPlan[0].points[0].x < connectionPlan[0].points[1].x);
    assert(connectionPlan[0].points[1].x < connectionPlan[0].points[2].x);
    assert(connectionPlan[1].caseId == 11);
    assert(!connectionPlan[1].selected);
    assert(connectionPlan[1].dimmed);
    assert(connectionPlan[1].points.size() == 2);

    std::vector<double> sorted = {1.0, 3.0, 5.0, 7.0};
    assert(closeEnough(BoxplotQuantileSorted(sorted, 0.25), 2.5));
    assert(closeEnough(BoxplotQuantileSorted(sorted, 0.50), 4.0));
    assert(closeEnough(BoxplotQuantileSorted(sorted, 1.0), 7.0));

    std::vector<BoxplotCase> statCases = {
        {1, 1.0, "A"}, {2, 2.0, "A"}, {3, 3.0, "A"}, {4, 4.0, "A"}, {5, 100.0, "A"},
        {6, 10.0, "B"}, {7, 14.0, "B"}, {8, NAN, "B"}
    };
    std::vector<BoxplotStats> stats = BoxplotStatsForCases(statCases, {"A", "B", "C"});
    assert(stats.size() == 2);
    assert(stats[0].category == "A");
    assert(stats[0].n == 5);
    assert(closeEnough(stats[0].q1, 2.0));
    assert(closeEnough(stats[0].median, 3.0));
    assert(closeEnough(stats[0].q3, 4.0));
    assert(closeEnough(stats[0].lower, 1.0));
    assert(closeEnough(stats[0].upper, 4.0));
    assert(stats[1].category == "B");
    assert(stats[1].n == 2);
    assert(closeEnough(stats[1].median, 12.0));
    BoxplotLayout statLayout;
    statLayout.plotRect = Rect{60.0, 30.0, 240.0, 160.0};
    statLayout.categories = {"A", "B"};
    statLayout.yMinimum = 0.0;
    statLayout.yMaximum = 100.0;
    std::vector<BoxplotStatsRenderItem> statPlan = BuildBoxplotStatsRenderPlan(statLayout, stats);
    assert(statPlan.size() == 2);
    assert(statPlan[0].category == "A");
    assert(closeEnough(statPlan[0].centerX, BoxplotCategoryCenter(statLayout, "A")));
    assert(statPlan[0].boxWidth >= 32.0);
    assert(statPlan[0].boxWidth <= 74.0);
    assert(statPlan[0].boxRect.x < statPlan[0].centerX);
    assert(statPlan[0].boxRect.x + statPlan[0].boxRect.width > statPlan[0].centerX);
    assert(statPlan[0].medianStart.y >= statPlan[0].boxRect.y);
    assert(statPlan[0].medianStart.y <= statPlan[0].boxRect.y + statPlan[0].boxRect.height);

    std::vector<BoxplotValueInterval> twoSidedTails =
        BoxplotTailIntervalsForAlternative(0.0, 10.0, 2.0, 8.0, "two.sided");
    assert(twoSidedTails.size() == 2);
    assert(closeEnough(twoSidedTails[0].lower, 0.0));
    assert(closeEnough(twoSidedTails[0].upper, 2.0));
    assert(closeEnough(twoSidedTails[1].lower, 8.0));
    assert(closeEnough(twoSidedTails[1].upper, 10.0));
    std::vector<BoxplotValueInterval> greaterTail =
        BoxplotTailIntervalsForAlternative(0.0, 10.0, 7.0, NAN, "greater");
    assert(greaterTail.size() == 1);
    assert(closeEnough(greaterTail[0].lower, 7.0));
    assert(closeEnough(greaterTail[0].upper, 10.0));
    std::vector<BoxplotValueInterval> lessTail =
        BoxplotTailIntervalsForAlternative(0.0, 10.0, 4.0, NAN, "less");
    assert(lessTail.size() == 1);
    assert(closeEnough(lessTail[0].lower, 0.0));
    assert(closeEnough(lessTail[0].upper, 4.0));
    assert(BoxplotTailIntervalsForAlternative(0.0, 10.0, NAN, NAN, "two.sided").empty());

    assert(closeEnough(MeanOfValues({2.0, 4.0, 6.0}), 4.0));
    assert(closeEnough(SampleSDOfValues({2.0, 4.0, 6.0}), 2.0));
    assert(!std::isfinite(SampleSDOfValues({2.0})));

    std::vector<std::string> available = {"mpg", "wt", "hp", "wt"};
    assert(BoxplotVariableListContains({"mpg", "wt"}, "wt"));
    assert(!BoxplotVariableListContains({"mpg", "wt"}, "hp"));
    assert(BoxplotUsesVariableAxes("boxplot", ""));
    assert(!BoxplotUsesVariableAxes("boxplot", "am"));
    assert(!BoxplotUsesVariableAxes("scatter", ""));
    assert(BoxplotAddVariableTitle() == "Add Variable");
    assert(BoxplotRemoveVariableTitle() == "Remove Variable");
    assert(BoxplotNoMoreNumericVariablesTitle() == "No more numeric variables");
    assert((BoxplotVariablesAvailableToAdd({"mpg"}, available) ==
            std::vector<std::string>{"wt", "hp"}));

    BoxplotVariableUpdateResult addResult = BoxplotVariablesAfterAdd({"mpg"}, "wt", available);
    assert(addResult.ok);
    assert(addResult.changed);
    assert((addResult.variables == std::vector<std::string>{"mpg", "wt"}));

    BoxplotVariableUpdateResult duplicateAdd = BoxplotVariablesAfterAdd({"mpg", "wt"}, "wt", available);
    assert(duplicateAdd.ok);
    assert(!duplicateAdd.changed);
    assert((duplicateAdd.variables == std::vector<std::string>{"mpg", "wt"}));

    BoxplotVariableUpdateResult badAdd = BoxplotVariablesAfterAdd({"mpg"}, "qsec", available);
    assert(!badAdd.ok);
    assert(badAdd.error == "numeric variable not found: qsec");

    BoxplotVariableUpdateResult removeResult = BoxplotVariablesAfterRemove({"mpg", "wt"}, "wt");
    assert(removeResult.ok);
    assert(removeResult.changed);
    assert((removeResult.variables == std::vector<std::string>{"mpg"}));

    BoxplotVariableUpdateResult removeLast = BoxplotVariablesAfterRemove({"mpg"}, "mpg");
    assert(!removeLast.ok);
    assert(removeLast.error == "a boxplot must keep at least one variable");

    BoxplotVariableUpdateResult missingRemove = BoxplotVariablesAfterRemove({"mpg", "wt"}, "hp");
    assert(!missingRemove.ok);
    assert(missingRemove.error == "variable is not in this boxplot: hp");

    BoxplotVariableUpdateResult replacement =
        BoxplotVariablesAfterReplacement({"mpg", "wt"}, "mpg", "hp", available);
    assert(replacement.ok);
    assert(replacement.changed);
    assert((replacement.variables == std::vector<std::string>{"hp", "wt"}));
    BoxplotVariableUpdateResult duplicateReplacement =
        BoxplotVariablesAfterReplacement({"mpg", "wt"}, "mpg", "wt", available);
    assert(!duplicateReplacement.ok);
    BoxplotVariableUpdateResult missingReplacement =
        BoxplotVariablesAfterReplacement({"mpg", "wt"}, "qsec", "hp", available);
    assert(!missingReplacement.ok);

    assert(closeEnough(BoxplotConnectionStrokeWidth(2.0, false), 2.0));
    assert(closeEnough(BoxplotConnectionStrokeWidth(2.0, true), 2.0));
    assert(closeEnough(BoxplotConnectionStrokeWidth(0.1, false), 0.5));
    assert(closeEnough(BoxplotConnectionStrokeWidth(20.0, false), 8.0));

    BoxplotMenuState plainMenu = BuildBoxplotMenuState(
        true, true, true, false, false, false, false, false, false, {"mpg"}, available);
    assert(plainMenu.title == "Boxplot");
    assert(plainMenu.togglePoints.title == "Hide Points");
    assert(plainMenu.toggleBox.title == "Toggle Box");
    assert(plainMenu.toggleWhiskers.title == "Toggle Whiskers");
    assert(!plainMenu.toggleViolin.checked);
    assert(!plainMenu.showClearSplitViolin);
    assert(!plainMenu.showClearH0);
    assert(!plainMenu.showVariableAxisOptions);
    assert(plainMenu.descriptives.command == "CONTEXT_BOXPLOT_DESCRIPTIVES");
    assert(plainMenu.selectionOptions.size() == 3);
    assert(plainMenu.selectionOptions[1].command == "SELECT_ALL_VISIBLE");
    assert(plainMenu.plotOptions[2].command == "PLOT_NEW_TIME_SERIES");
    assert(plainMenu.plotOptions[4].command == "PLOT_NEW_PARALLEL_COORDINATES");
    assert(plainMenu.plotOptions[6].command == "PLOT_NEW_LINKED_HISTOGRAM");

    BoxplotMenuState variableAxisMenu = BuildBoxplotMenuState(
        false, true, true, true, true, true, true, true, true, {"mpg", "wt"}, available);
    assert(variableAxisMenu.togglePoints.title == "Show Points");
    assert(variableAxisMenu.toggleViolin.checked);
    assert(variableAxisMenu.showClearSplitViolin);
    assert(variableAxisMenu.showClearH0);
    assert(variableAxisMenu.showVariableAxisOptions);
    assert(variableAxisMenu.addVariableTitle == "Add Variable");
    assert(variableAxisMenu.addVariableOptions.size() == 1);
    assert(variableAxisMenu.addVariableOptions[0].command == "BOXPLOT_ADD_VARIABLE|hp");
    assert(variableAxisMenu.showRemoveVariableMenu);
    assert(variableAxisMenu.removeVariableOptions.size() == 2);
    assert(variableAxisMenu.removeVariableOptions[1].command == "BOXPLOT_REMOVE_VARIABLE|wt");
    assert(variableAxisMenu.connectRows.checked);
    assert(variableAxisMenu.standardizeVariables.checked);

    BoxplotMenuState fullVariableAxisMenu = BuildBoxplotMenuState(
        true, true, true, false, false, false, true, false, false, {"mpg", "wt", "hp"}, available);
    assert(fullVariableAxisMenu.addVariableOptions.empty());
    assert(fullVariableAxisMenu.noMoreNumericVariablesTitle == "No more numeric variables");

    ParallelBoxplotLabelState oneLabel = ParallelBoxplotLabelsForVariables({"mpg"}, false, "");
    assert(oneLabel.yLabel == "mpg");
    assert(oneLabel.title == "mpg");
    ParallelBoxplotLabelState manyLabel = ParallelBoxplotLabelsForVariables({"mpg", "wt"}, false, "mpg");
    assert(manyLabel.yLabel == "Value");
    assert(manyLabel.title == "Parallel boxplots");
    ParallelBoxplotLabelState customLabel = ParallelBoxplotLabelsForVariables({"mpg", "wt"}, false, "Custom");
    assert(customLabel.yLabel == "Value");
    assert(customLabel.title == "Custom");
    ParallelBoxplotLabelState standardizedLabel = ParallelBoxplotLabelsForVariables({"mpg", "wt"}, true, "");
    assert(standardizedLabel.yLabel == "Standardized value");
    assert(standardizedLabel.title == "Parallel boxplots");

    ParallelBoxplotBuildInput parallel;
    parallel.variables = {
        {"x", {1.0, 2.0, NAN, 4.0}},
        {"z", {10.0, 10.0, 10.0}}
    };
    parallel.selectedVariables = {"x", "z"};
    ParallelBoxplotBuildResult parallelResult = BuildParallelBoxplotCases(parallel);
    assert(parallelResult.ok);
    assert(parallelResult.categories == std::vector<std::string>({"x", "z"}));
    assert(parallelResult.cases.size() == 6);
    assert(parallelResult.cases[0].caseId == 1);
    assert(parallelResult.cases[0].category == "x");
    assert(closeEnough(parallelResult.cases[0].value, 1.0));
    assert(parallelResult.cases[2].caseId == 4);
    assert(closeEnough(parallelResult.cases[2].value, 4.0));
    assert(parallelResult.cases[3].category == "z");

    parallel.standardize = true;
    ParallelBoxplotBuildResult standardized = BuildParallelBoxplotCases(parallel);
    assert(standardized.ok);
    assert(closeEnough(standardized.cases[0].value, (1.0 - 7.0 / 3.0) / 1.5275252316519468));
    assert(closeEnough(standardized.cases[1].value, (2.0 - 7.0 / 3.0) / 1.5275252316519468));
    assert(closeEnough(standardized.cases[2].value, (4.0 - 7.0 / 3.0) / 1.5275252316519468));
    assert(closeEnough(standardized.cases[3].value, 0.0));
    assert(closeEnough(standardized.cases[4].value, 0.0));
    assert(closeEnough(standardized.cases[5].value, 0.0));

    ParallelBoxplotBuildInput missingParallel = parallel;
    missingParallel.selectedVariables = {"missing"};
    ParallelBoxplotBuildResult missingResult = BuildParallelBoxplotCases(missingParallel);
    assert(!missingResult.ok);
    assert(missingResult.error == "numeric variable not found: missing");

    BoxplotH0SimulationResult h0One;
    std::string error;
    std::vector<BoxplotCase> oneSample = {
        {1, 2.0, "Score"}, {2, 4.0, "Score"}, {3, 6.0, "Score"}, {4, 8.0, "Score"}
    };
    assert(BuildBoxplotH0Simulation(oneSample, {"Score"}, false, 0.0, "two.sided", 40, h0One, &error));
    assert(h0One.draws == 100);
    assert(h0One.simulatedValues.size() == 100);
    assert(h0One.referenceLines.size() == 2);
    assert(h0One.label == "H0 mean");
    assert(closeEnough(h0One.referenceLines[0], 5.0));
    assert(closeEnough(h0One.referenceLines[1], 0.0));
    assert(closeEnough(h0One.lower, -5.0));
    assert(closeEnough(h0One.upper, 5.0));
    assert(h0One.pValue > 0.0 && h0One.pValue <= 1.0);
    assert(h0One.effect > 1.9 && h0One.effect < 2.0);

    BoxplotH0SimulationResult h0Groups;
    std::vector<BoxplotCase> twoGroups = {
        {1, 1.0, "A"}, {2, 2.0, "A"}, {3, 3.0, "A"}, {4, 4.0, "A"},
        {5, 6.0, "B"}, {6, 7.0, "B"}, {7, 8.0, "B"}, {8, 9.0, "B"}
    };
    assert(BuildBoxplotH0Simulation(twoGroups, {"A", "B"}, false, 0.0, "less", 120, h0Groups, &error));
    assert(h0Groups.draws == 120);
    assert(h0Groups.label == "H0 diff");
    assert(h0Groups.referenceLines.size() == 2);
    assert(closeEnough(h0Groups.referenceLines[0], 2.5));
    assert(closeEnough(h0Groups.referenceLines[1], 7.5));
    assert(h0Groups.pValue > 0.0 && h0Groups.pValue <= 1.0);
    assert(h0Groups.effect < -3.0);

    BoxplotH0SimulationResult failed;
    assert(!BuildBoxplotH0Simulation(oneSample, {"Score"}, false, 0.0, "bad", 100, failed, &error));
    assert(error == "invalid H0 simulation alternative");
    assert(!BuildBoxplotH0Simulation(twoGroups, {"A", "B"}, true, 0.0, "two.sided", 100, failed, &error));
    assert(error == "H0 simulation currently supports one variable or exactly two observed groups.");

    assert(rlispstat::core::BoxplotSplitViolinInfoText() == "Highlight the tail or tails of the violin using thresholds on the displayed scale.");
    assert(rlispstat::core::BoxplotH0SimulationInfoText() == "Simulate the null distribution and show its p-value/effect layer in the boxplot.");
    assert(rlispstat::core::BoxplotWindowTitle() == "Boxplot");
    assert(rlispstat::core::BoxplotSegmentDetailsWindowTitle() == "Segment details");
    assert(rlispstat::core::BoxplotH0SimulationWindowTitle() == "H0 Simulation");
    assert(rlispstat::core::BoxplotObservedLabel() == "Observed");
    assert(rlispstat::core::BoxplotParallelLabel() == "Parallel");
    assert(rlispstat::core::BoxplotAlternativeFieldLabel() == "Alternative:");
    assert(rlispstat::core::BoxplotLessOptionLabel() == "less");
    assert(rlispstat::core::BoxplotGreaterOptionLabel() == "greater");
    assert(rlispstat::core::BoxplotTwoSidedOptionLabel() == "two.sided");
    assert(rlispstat::core::BoxplotThresholdLowerFieldLabel() == "Threshold / lower:");
    assert(rlispstat::core::BoxplotUpperFieldLabel() == "Upper:");
    assert(rlispstat::core::BoxplotH0FieldLabel() == "H0:");
    assert(rlispstat::core::BoxplotDrawsFieldLabel() == "Draws:");
    assert(rlispstat::core::BoxplotSplitViolinDialogTitle() == "Split Violin");
    assert(rlispstat::core::BoxplotPBracketLabel() == "p");
    assert(rlispstat::core::BoxplotLeafLabel() == "leaf");

    for (const auto &rangePair : std::vector<std::pair<double, double>>{
             {-8.0, 14.0}, {0.000001, 0.000009}, {-1.0e8, 3.0e8}, {4.0, 4.0}}) {
        auto ticks = BoxplotYAxisTicks(rangePair.first, rangePair.second, 5);
        assert(ticks.size() >= 4 && ticks.size() <= 7);
        for (std::size_t i = 1; i < ticks.size(); ++i) assert(ticks[i].value > ticks[i - 1].value);
        for (const auto &tick : ticks) assert(!tick.label.empty());
    }

    std::vector<BoxplotCase> orderCases = {
        {1, 5.0, "10"}, {2, 5.0, "10"},
        {3, 1.0, "2"}, {4, 3.0, "2"},
        {5, 2.0, "8"}, {6, 2.0, "8"}
    };
    const std::vector<std::string> definedOrder = {"10", "2", "8"};
    assert(OrderedBoxplotCategories(definedOrder, orderCases, "defined") == definedOrder);
    assert(OrderedBoxplotCategories(definedOrder, orderCases, "label_asc") ==
           std::vector<std::string>({"2", "8", "10"}));
    const std::vector<std::string> definedWithEmpty = {"10", "2", "8", "empty"};
    assert(OrderedBoxplotCategories(definedWithEmpty, orderCases, "median_asc") ==
           std::vector<std::string>({"2", "8", "10", "empty"}));
    assert(OrderedBoxplotCategories(definedWithEmpty, orderCases, "median_desc") ==
           std::vector<std::string>({"10", "2", "8", "empty"}));
    auto orderMenu = BoxplotGroupOrderMenuOptions("median_desc");
    assert(orderMenu.size() == 5);
    assert(orderMenu[4].checked);
    assert(orderMenu[4].command == "BOXPLOT_GROUP_ORDER|median_desc");

    BoxplotLayout reorderedLayout = layout;
    reorderedLayout.categories = {"hp", "mpg", "wt"};
    auto reorderedGeometry = BoxplotCaseGeometryForLayout(reorderedLayout, cases);
    assert(reorderedGeometry.size() == geometry.size());
    for (const auto &item : reorderedGeometry) {
        assert(item.caseId == 1 || item.caseId == 2 || item.caseId == 3);
    }

    return 0;
}
