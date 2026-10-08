#include "../../src/core/histogram_model.h"

#include <cassert>
#include <cmath>
#include <map>
#include <set>
#include <string>
#include <vector>

using rlispstat::core::CaseId;
using rlispstat::core::DensityCurveForValues;
using rlispstat::core::DensityCurvesForHistogramCases;
using rlispstat::core::BuildHistogramBarRenderPlan;
using rlispstat::core::BuildHistogramCreationDialogState;
using rlispstat::core::BuildHistogramDensityAdjustDialogState;
using rlispstat::core::BuildHistogramDensityBandwidthDialogState;
using rlispstat::core::BuildHistogramDensityRenderPlan;
using rlispstat::core::BuildHistogramMenuState;
using rlispstat::core::BuildHistogramRenderPlan;
using rlispstat::core::BuildHistogramRugRenderPlan;
using rlispstat::core::HistogramAverageBinWidth;
using rlispstat::core::HistogramBreaksResponseText;
using rlispstat::core::HistogramBinningResult;
using rlispstat::core::HistogramBarRenderItem;
using rlispstat::core::HistogramBinCountForRule;
using rlispstat::core::HistogramBinData;
using rlispstat::core::HistogramBinIndexAtPoint;
using rlispstat::core::HistogramBinRangeForGesture;
using rlispstat::core::HistogramBinRect;
using rlispstat::core::HistogramCaseValue;
using rlispstat::core::HistogramCreationDialogState;
using rlispstat::core::HistogramDensityCurve;
using rlispstat::core::HistogramDensityDisplayActive;
using rlispstat::core::HistogramDensityParameterDialogState;
using rlispstat::core::HistogramDensityState;
using rlispstat::core::HistogramDensityModeIsValid;
using rlispstat::core::HistogramDensityRenderPlan;
using rlispstat::core::HistogramColorSegmentsVisible;
using rlispstat::core::HistogramDefaultTitle;
using rlispstat::core::HistogramDensityInfoResponseText;
using rlispstat::core::HistogramDragHighlightRect;
using rlispstat::core::HistogramLayout;
using rlispstat::core::HistogramMaximumBinCount;
using rlispstat::core::HistogramRenderInput;
using rlispstat::core::HistogramRenderPlan;
using rlispstat::core::HistogramRugCase;
using rlispstat::core::HistogramRugRenderItem;
using rlispstat::core::HistogramStateAfterSetDensityMode;
using rlispstat::core::HistogramStateAfterToggleDensity;
using rlispstat::core::ParseHistogramDensityAdjustCommandValue;
using rlispstat::core::ParseHistogramDensityBandwidthCommandValue;
using rlispstat::core::Point;
using rlispstat::core::PlotModel;
using rlispstat::core::QuantileSorted;
using rlispstat::core::Rect;
using rlispstat::core::RebinHistogramCases;
using rlispstat::core::RebuildHistogramPointsFromCurrentVariable;
using rlispstat::core::SelectHistogramCasesForGesture;

static bool closeEnough(double a, double b)
{
    return std::fabs(a - b) < 1.0e-9;
}

static std::set<CaseId> S(std::initializer_list<CaseId> values)
{
    return std::set<CaseId>(values.begin(), values.end());
}

int main()
{
    HistogramCreationDialogState creationDialog = BuildHistogramCreationDialogState();
    assert(creationDialog.title == "Histogram");
    assert(creationDialog.informativeText ==
           "Choose a numeric variable. The histogram will share row selection with existing plots in this dataset.");
    assert(creationDialog.createButtonTitle == "Create");
    assert(creationDialog.cancelButtonTitle == "Cancel");
    assert(creationDialog.binsPlaceholder == "auto");
    assert(creationDialog.optionalTitlePlaceholder == "Optional title");
    assert(creationDialog.noActiveDatasetStatus ==
           "Open a LinkEDA plot first, or use ls_new_histogram() from R.");
    assert(creationDialog.needsNumericVariableStatus ==
           "The active dataset needs at least one numeric variable.");
    assert(creationDialog.selectedXMissingStatus == "Could not find the selected X variable.");
    assert(HistogramDefaultTitle("mpg") == "Histogram of mpg");

    rlispstat::core::PlotModel histogramOptions;
    histogramOptions.histogramBins = {
        rlispstat::core::HistogramBin{1.0, 2.5, {1, 2}},
        rlispstat::core::HistogramBin{2.5, 4.0, {3}}
    };
    histogramOptions.histogramShowDensity = true;
    histogramOptions.histogramDensityMode = "color";
    histogramOptions.histogramDensityBw = 0.25;
    histogramOptions.histogramDensityAdjust = 1.5;
    assert(HistogramBreaksResponseText(histogramOptions) == "OK\t1\t2.5\t4");
    assert(HistogramDensityInfoResponseText(histogramOptions) == "OK\tTRUE|color|0.25|1.5");

    HistogramDensityParameterDialogState bandwidthDialog =
        BuildHistogramDensityBandwidthDialogState(0.0);
    assert(bandwidthDialog.title == "Histogram Density Bandwidth");
    assert(bandwidthDialog.informativeText == "Leave empty to use the automatic bandwidth.");
    assert(bandwidthDialog.setButtonTitle == "Set");
    assert(bandwidthDialog.cancelButtonTitle == "Cancel");
    assert(bandwidthDialog.currentValueText.empty());
    bandwidthDialog = BuildHistogramDensityBandwidthDialogState(0.123456789);
    assert(bandwidthDialog.currentValueText == "0.123457");

    HistogramDensityParameterDialogState adjustDialog =
        BuildHistogramDensityAdjustDialogState(1.25);
    assert(adjustDialog.title == "Histogram Density Smoother");
    assert(adjustDialog.informativeText ==
           "Use values above 1 for smoother curves and below 1 for less smoothing.");
    assert(adjustDialog.setButtonTitle == "Set");
    assert(adjustDialog.cancelButtonTitle == "Cancel");
    assert(adjustDialog.currentValueText == "1.25");

    HistogramLayout layout;
    layout.plotRect = Rect{10.0, 20.0, 400.0, 200.0};
    layout.counts = {5, 10, 0, 20};
    layout.maxCount = 20;
    layout.gap = 2.0;

    Rect first = HistogramBinRect(layout, 0);
    Rect fourth = HistogramBinRect(layout, 3);
    const Rect contentRect = rlispstat::core::ZeroBaselineContentRect(layout.plotRect);
    assert(closeEnough(contentRect.x, layout.plotRect.x + 10.0));
    assert(closeEnough(contentRect.width, 380.0));
    assert(closeEnough(first.x, contentRect.x + 1.0));
    assert(closeEnough(first.width, contentRect.width / 4.0 - 2.0));
    assert(fourth.x + fourth.width < layout.plotRect.x + layout.plotRect.width);
    assert(contentRect.y > layout.plotRect.y);
    assert(contentRect.y + contentRect.height < layout.plotRect.y + layout.plotRect.height);
    assert(closeEnough(first.y, contentRect.y + contentRect.height * 0.75));
    assert(closeEnough(first.height, contentRect.height * 0.25));
    assert(closeEnough(fourth.y, contentRect.y));
    assert(closeEnough(fourth.height, contentRect.height));

    auto firstHit = HistogramBinIndexAtPoint(layout, Point{20.0, 30.0});
    assert(firstHit.has_value() && *firstHit == 0);
    auto lastHit = HistogramBinIndexAtPoint(layout, Point{399.0, 220.0});
    assert(lastHit.has_value() && *lastHit == 3);
    auto noHit = HistogramBinIndexAtPoint(layout, Point{9.0, 30.0});
    assert(!noHit.has_value());
    assert(!HistogramBinIndexAtPoint(layout, Point{15.0, 30.0}).has_value());
    assert(!HistogramBinIndexAtPoint(layout, Point{405.0, 30.0}).has_value());

    auto range = HistogramBinRangeForGesture(layout, Point{20.0, 50.0}, Point{250.0, 50.0});
    assert(range.has_value());
    assert(range->first == 0);
    assert(range->second == 2);
    auto dragHighlight = HistogramDragHighlightRect(layout, Point{20.0, 50.0}, Point{250.0, 50.0});
    assert(dragHighlight.has_value());
    assert(closeEnough(dragHighlight->x, first.x));
    assert(closeEnough(dragHighlight->y, 20.0));
    assert(closeEnough(dragHighlight->width,
                       HistogramBinRect(layout, 2).x +
                       HistogramBinRect(layout, 2).width - first.x));
    assert(closeEnough(dragHighlight->height, 200.0));
    assert(!HistogramDragHighlightRect(layout, Point{0.0, 50.0}, Point{250.0, 50.0}).has_value());

    std::vector<std::vector<CaseId>> rows = {
        {1, 2},
        {3},
        {0},
        {4, 5}
    };
    assert(SelectHistogramCasesForGesture(rows, layout, Point{20.0, 50.0}, Point{250.0, 50.0}) == S({1, 2, 3}));
    assert(SelectHistogramCasesForGesture(rows, layout, Point{0.0, 50.0}, Point{250.0, 50.0}).empty());
    assert(HistogramMaximumBinCount(rows) == 2);
    assert(HistogramMaximumBinCount({}) == 1);

    std::vector<double> sorted = {1, 3, 5, 7};
    assert(closeEnough(QuantileSorted(sorted, 0.25), 2.5));
    assert(closeEnough(QuantileSorted(sorted, 0.50), 4.0));
    assert(closeEnough(QuantileSorted(sorted, 1.0), 7.0));

    std::vector<double> values = {1, 2, 3, 4, 5, 6, 7, 8};
    assert(HistogramBinCountForRule({}, "sturges") == 1);
    assert(HistogramBinCountForRule(values, "sqrt") == 3);
    assert(HistogramBinCountForRule(values, "sturges") == 4);
    assert(HistogramBinCountForRule(values, "fd") == 2);
    assert(HistogramBinCountForRule(values, "scott") == 2);

    HistogramBinningResult binned;
    std::vector<HistogramCaseValue> rebinCases = {
        {1.0, 1}, {2.0, 2}, {3.0, 3}, {4.0, 4}, {NAN, 5}
    };
    assert(RebinHistogramCases(rebinCases, 3, binned));
    assert(binned.bins.size() == 3);
    assert(binned.pointBins.size() == rebinCases.size());
    assert(closeEnough(binned.minimum, 1.0));
    assert(closeEnough(binned.maximum, 4.0));
    assert(closeEnough(binned.bins[0].lower, 1.0));
    assert(closeEnough(binned.bins[0].upper, 2.0));
    assert(closeEnough(binned.bins[2].lower, 3.0));
    assert(closeEnough(binned.bins[2].upper, 4.0));
    assert((binned.pointBins == std::vector<int>{1, 2, 3, 3, 0}));
    assert((binned.bins[0].rows == std::vector<CaseId>{1}));
    assert((binned.bins[2].rows == std::vector<CaseId>{3, 4}));
    assert(closeEnough(HistogramAverageBinWidth(binned.bins), 1.0));

    std::vector<HistogramBarRenderItem> barPlan = BuildHistogramBarRenderPlan(
        layout,
        std::vector<std::vector<CaseId>>{{1, 2, 3, 4}, {5, 6}, {}, {7}},
        S({2, 3, 5}),
        std::map<CaseId, std::string>{{3, "orange"}, {4, "blue"}, {5, "orange"}},
        true,
        true);
    assert(barPlan.size() == 4);
    assert(closeEnough(barPlan[0].rect.x, first.x));
    assert(barPlan[0].count == 4);
    assert(barPlan[0].showCount);
    assert(barPlan[0].countLabel == "4");
    assert(barPlan[0].colorSegments.size() == 2);
    assert(barPlan[0].colorSegments[0].colorName.empty());
    assert(barPlan[0].colorSegments[0].defaultSelection);
    assert(closeEnough(barPlan[0].colorSegments[0].alpha, 1.0));
    const double segmentHeight = first.height / 4.0;
    const double barBaseline = first.y + first.height;
    assert(closeEnough(barPlan[0].colorSegments[0].rect.y, barBaseline - 2.0 * segmentHeight));
    assert(closeEnough(barPlan[0].colorSegments[0].rect.height, segmentHeight));
    assert(barPlan[0].colorSegments[1].colorName == "blue");
    assert(closeEnough(barPlan[0].colorSegments[1].rect.y, barBaseline - 3.0 * segmentHeight));
    assert(closeEnough(barPlan[0].colorSegments[1].rect.height, segmentHeight));
    assert(barPlan[0].selectedSegments.size() == 2);
    assert(barPlan[0].selectedSegments[0].defaultSelection);
    assert(barPlan[0].selectedSegments[0].colorName.empty());
    assert(closeEnough(barPlan[0].selectedSegments[0].alpha, 0.30));
    assert(closeEnough(barPlan[0].selectedSegments[0].rect.y, barBaseline - segmentHeight));
    assert(closeEnough(barPlan[0].selectedSegments[0].rect.height, segmentHeight));
    assert(!barPlan[0].selectedSegments[1].defaultSelection);
    assert(barPlan[0].selectedSegments[1].colorName == "orange");
    assert(closeEnough(barPlan[0].selectedSegments[1].rect.y, first.y));
    assert(closeEnough(barPlan[0].selectedSegments[1].rect.height, segmentHeight));
    assert(barPlan[2].count == 0);
    assert(!barPlan[2].showCount);
    std::vector<HistogramBarRenderItem> puzzlePlan = BuildHistogramBarRenderPlan(
        layout,
        std::vector<std::vector<CaseId>>{{1, 2, 3, 4}},
        S({3}),
        std::map<CaseId, std::string>{{1, "orange"}, {3, "orange"}},
        false,
        true);
    assert(puzzlePlan.size() == 4);
    assert(puzzlePlan[0].colorSegments.size() == 2);
    assert(puzzlePlan[0].colorSegments[0].colorName.empty());
    assert(closeEnough(puzzlePlan[0].colorSegments[0].rect.y, barBaseline - 2.0 * segmentHeight));
    assert(closeEnough(puzzlePlan[0].colorSegments[0].rect.height, 2.0 * segmentHeight));
    assert(puzzlePlan[0].colorSegments[1].colorName == "orange");
    assert(closeEnough(puzzlePlan[0].colorSegments[1].rect.y, first.y));
    assert(closeEnough(puzzlePlan[0].colorSegments[1].rect.height, segmentHeight));
    assert(puzzlePlan[0].selectedSegments.size() == 1);
    assert(!puzzlePlan[0].selectedSegments[0].defaultSelection);
    assert(puzzlePlan[0].selectedSegments[0].colorName == "orange");
    assert(closeEnough(puzzlePlan[0].selectedSegments[0].alpha, 1.0));
    assert(closeEnough(puzzlePlan[0].selectedSegments[0].rect.y, first.y + segmentHeight));
    assert(closeEnough(puzzlePlan[0].selectedSegments[0].rect.height, segmentHeight));
    assert(closeEnough(
        puzzlePlan[0].colorSegments[1].rect.y +
            puzzlePlan[0].colorSegments[1].rect.height,
        puzzlePlan[0].selectedSegments[0].rect.y));
    std::vector<HistogramBarRenderItem> densityBarPlan = BuildHistogramBarRenderPlan(
        layout,
        std::vector<std::vector<CaseId>>{{1, 2, 3, 4}, {5, 6}, {}, {7}},
        S({2, 3, 5}),
        std::map<CaseId, std::string>{{3, "orange"}, {4, "blue"}, {5, "orange"}},
        true,
        false);
    assert(densityBarPlan[0].colorSegments.empty());
    assert(densityBarPlan[0].selectedSegments.empty());

    std::vector<HistogramRugRenderItem> rugPlan = BuildHistogramRugRenderPlan(
        layout,
        std::vector<HistogramRugCase>{{1, 0}, {2, 3}, {0, 1}, {3, 99}},
        7.0);
    assert(rugPlan.size() == 2);
    assert(rugPlan[0].caseId == 1);
    assert(closeEnough(rugPlan[0].start.x, first.x + first.width / 2.0));
    assert(closeEnough(rugPlan[0].start.y, barBaseline));
    assert(closeEnough(rugPlan[0].end.y, rugPlan[0].start.y - 7.0));
    assert(rugPlan[1].caseId == 2);
    assert(closeEnough(rugPlan[1].start.x, fourth.x + fourth.width / 2.0));
    assert(BuildHistogramRugRenderPlan(layout, {{1, 0}}, 0.0).empty());

    HistogramBinningResult equalBinned;
    assert(RebinHistogramCases({{5.0, 1}, {5.0, 2}}, 2, equalBinned));
    assert(closeEnough(equalBinned.minimum, 4.5));
    assert(closeEnough(equalBinned.maximum, 5.5));
    assert((equalBinned.pointBins == std::vector<int>{2, 2}));
    assert(!RebinHistogramCases({{NAN, 1}}, 2, equalBinned));

    PlotModel histogramModel;
    histogramModel.kind = "histogram";
    histogramModel.xLabel = "mpg";
    histogramModel.variables = {
        rlispstat::core::NumericVariable{"mpg", {21.0, NAN, 23.0, 24.0}},
        rlispstat::core::NumericVariable{"wt", {2.6, 2.8, 3.2, 3.4}}
    };
    RebuildHistogramPointsFromCurrentVariable(histogramModel);
    assert(histogramModel.histogramPoints.size() == 3);
    assert(histogramModel.histogramPoints[0].row == 1);
    assert(histogramModel.histogramPoints[1].row == 3);
    assert(!histogramModel.histogramBins.empty());
    const int originalBinCount = static_cast<int>(histogramModel.histogramBins.size());
    RebuildHistogramPointsFromCurrentVariable(histogramModel);
    assert(static_cast<int>(histogramModel.histogramBins.size()) == originalBinCount);
    histogramModel.xLabel = "missing";
    RebuildHistogramPointsFromCurrentVariable(histogramModel);
    assert(histogramModel.histogramPoints.empty());
    assert(histogramModel.histogramBins.empty());

    assert(HistogramDensityModeIsValid("all"));
    assert(HistogramDensityModeIsValid("selected_and_colors"));
    assert(!HistogramDensityModeIsValid("mystery"));
    assert(HistogramDensityDisplayActive(true, "all"));
    assert(!HistogramDensityDisplayActive(true, "none"));
    assert(!HistogramDensityDisplayActive(true, "mystery"));
    assert(!HistogramDensityDisplayActive(false, "all"));
    assert(HistogramColorSegmentsVisible(true, "all"));
    assert(HistogramColorSegmentsVisible(true, "none"));
    assert(HistogramColorSegmentsVisible(false, "all"));
    const auto binningRules = rlispstat::core::HistogramBinningRuleMenuOptions();
    assert(binningRules.size() == 4);
    rlispstat::core::PlotModel binningPlot;
    binningPlot.kind = "histogram";
    std::vector<double> binningValues;
    for (int i = 0; i < 64; ++i) {
        const double value = i == 63 ? 100.0 : i * .1;
        binningValues.push_back(value);
        binningPlot.histogramPoints.push_back({value, i + 1, 0});
        binningPlot.points.push_back({value, 0, i + 1});
    }
    for (const auto &rule : binningRules) {
        assert(rule.command == "HIST_SET_BINNING_RULE|" + rule.value);
        assert(rlispstat::core::HistogramBinCountForChoice(binningPlot, rule.value) ==
               HistogramBinCountForRule(binningValues, rule.value));
    }
    binningPlot.kind = "trellis_scatterplot";
    assert(rlispstat::core::HistogramBinCountForChoice(binningPlot, "sqrt") == 8);
    assert(rlispstat::core::HistogramBinCountForChoice(binningPlot, "27") == 27);
    for (const auto &invalid : {"", "0", "201", "3.5", "bogus", "10x"})
        assert(!rlispstat::core::HistogramBinCountForChoice(binningPlot, invalid));
    const auto binCounts = rlispstat::core::HistogramBinCountMenuOptions(15);
    assert(binCounts.size() == 6 && binCounts[2].checked);

    auto histogramMenu = BuildHistogramMenuState(
        "mpg", true, true, false, false, true, "colors");
    assert(histogramMenu.title == "Histogram");
    assert(histogramMenu.densityCurvesTitle == "Density Curves");
    assert(histogramMenu.densityModeTitle == "Density Mode");
    assert(histogramMenu.counts.title == "Hide Counts");
    assert(histogramMenu.tickMarks.title == "Hide Tick Marks");
    assert(histogramMenu.tickMarks.checked);
    assert(histogramMenu.tickMarks.command == "HIST_TOGGLE_TICK_MARKS");
    assert(histogramMenu.tickLabels.title == "Show Tick Labels");
    assert(!histogramMenu.tickLabels.checked);
    assert(histogramMenu.tickLabels.command == "HIST_TOGGLE_TICK_LABELS");
    assert(histogramMenu.rug.title == "Show Rug");
    assert(histogramMenu.densityToggle.title == "Hide Density Curves");
    assert(histogramMenu.densityModes.size() == 4);
    assert(histogramMenu.densityModes[2].checked);
    assert(histogramMenu.densityModes[2].command == "HIST_SET_DENSITY_MODE|colors");
    assert(histogramMenu.frequencyTable.title == "Frequency table: mpg");
    assert(histogramMenu.descriptives.command == "CONTEXT_HISTOGRAM_DESCRIPTIVES");
    assert(histogramMenu.selectionOptions.size() == 2);
    assert(histogramMenu.selectionOptions[0].command == "CLEAR_SELECTION");
    assert(histogramMenu.selectionOptions[1].command == "INVERT_SELECTION");
    assert(histogramMenu.plotOptions.size() == 8);
    assert(histogramMenu.plotOptions[0].command == "PLOT_NEW_LINKED_SCATTERPLOT");
    assert(histogramMenu.plotOptions[1].value == "trellis_scatterplot");
    assert(histogramMenu.plotOptions[2].value == "time_series");
    assert(histogramMenu.plotOptions[4].value == "parallel_coordinates");
    assert(histogramMenu.plotOptions[7].value == "bar_chart");
    HistogramDensityState toggledDensity = HistogramStateAfterToggleDensity(false, "none");
    assert(toggledDensity.changed);
    assert(toggledDensity.showDensity);
    assert(toggledDensity.densityMode == "all");
    HistogramDensityState hiddenDensity = HistogramStateAfterSetDensityMode(true, "all", "none");
    assert(hiddenDensity.changed);
    assert(!hiddenDensity.showDensity);
    assert(hiddenDensity.densityMode == "none");
    HistogramDensityState invalidDensity = HistogramStateAfterSetDensityMode(true, "all", "mystery");
    assert(!invalidDensity.changed);
    assert(invalidDensity.showDensity);
    assert(invalidDensity.densityMode == "all");
    auto parsedBandwidth = ParseHistogramDensityBandwidthCommandValue(" 0.75 ");
    assert(parsedBandwidth.has_value() && closeEnough(*parsedBandwidth, 0.75));
    auto resetBandwidth = ParseHistogramDensityBandwidthCommandValue("");
    assert(resetBandwidth.has_value() && closeEnough(*resetBandwidth, 0.0));
    auto clampedBandwidth = ParseHistogramDensityBandwidthCommandValue("-2");
    assert(clampedBandwidth.has_value() && closeEnough(*clampedBandwidth, 0.0));
    assert(!ParseHistogramDensityBandwidthCommandValue("auto").has_value());
    auto parsedAdjust = ParseHistogramDensityAdjustCommandValue("1.25");
    assert(parsedAdjust.has_value() && closeEnough(*parsedAdjust, 1.25));
    assert(!ParseHistogramDensityAdjustCommandValue("0").has_value());
    assert(!ParseHistogramDensityAdjustCommandValue("smooth").has_value());

    HistogramDensityCurve all = DensityCurveForValues(values, "all", "", "All", 0.0, 9.0, 0.0, 1.0);
    assert(all.kind == "all");
    assert(all.label == "All");
    assert(all.n == 8);
    assert(all.x.size() == 160);
    assert(all.y.size() == 160);
    assert(closeEnough(all.x.front(), 0.0));
    assert(closeEnough(all.x.back(), 9.0));
    assert(all.y[80] > 0.0);

    std::vector<HistogramCaseValue> cases = {
        {1.0, 1}, {2.0, 2}, {3.0, 3}, {4.0, 4}, {5.0, 5}, {6.0, 6}
    };
    std::map<CaseId, std::string> rowColors = {
        {1, "orange"}, {2, "orange"}, {3, "blue"}, {4, "blue"}
    };
    std::vector<HistogramDensityCurve> curves = DensityCurvesForHistogramCases(
        cases, 0.0, 7.0, true, "selected_and_colors", 0.0, 1.0, S({5, 6}), rowColors);
    assert(curves.size() == 3);
    assert(curves[0].kind == "color_group");
    assert(curves[0].colorName == "blue");
    assert(curves[1].kind == "color_group");
    assert(curves[1].colorName == "orange");
    assert(curves[2].kind == "selected");
    assert(curves[2].label == "Selected Cases");
    assert(DensityCurvesForHistogramCases(cases, 0.0, 7.0, false, "all", 0.0, 1.0, S({}), {}).empty());

    HistogramDensityCurve blueCurve;
    blueCurve.kind = "color_group";
    blueCurve.colorName = "blue";
    blueCurve.label = "blue";
    blueCurve.n = 3;
    blueCurve.x = {0.0, 5.0, 10.0};
    blueCurve.y = {0.0, 2.0, 0.0};
    HistogramDensityCurve orangeCurve;
    orangeCurve.kind = "color_group";
    orangeCurve.colorName = "orange";
    orangeCurve.label = "orange";
    orangeCurve.n = 3;
    orangeCurve.x = {0.0, 5.0, 10.0};
    orangeCurve.y = {0.0, 1.0, 0.0};
    HistogramDensityCurve selectedCurve = orangeCurve;
    selectedCurve.kind = "selected";
    selectedCurve.colorName = "";
    HistogramDensityRenderPlan densityPlan = BuildHistogramDensityRenderPlan(
        layout.plotRect,
        {blueCurve, orangeCurve, selectedCurve},
        0.0,
        10.0);
    assert(densityPlan.curves.size() == 3);
    assert(densityPlan.overlaps.size() == 1);
    assert(densityPlan.overlaps[0].firstColorName == "blue");
    assert(densityPlan.overlaps[0].secondColorName == "orange");
    assert(closeEnough(densityPlan.yMaximum, 2.0 / 0.45));
    assert(densityPlan.curves[0].linePoints.size() == 3);
    assert(densityPlan.curves[0].fillPolygon.size() == 5);
    assert(closeEnough(densityPlan.curves[0].linePoints[0].x, layout.plotRect.x));
    assert(closeEnough(densityPlan.curves[0].linePoints[1].x, layout.plotRect.x + layout.plotRect.width / 2.0));
    assert(closeEnough(densityPlan.curves[0].linePoints[1].y,
                       layout.plotRect.y + layout.plotRect.height - layout.plotRect.height * 0.45));
    assert(closeEnough(densityPlan.curves[0].fillPolygon[3].y,
                       layout.plotRect.y + layout.plotRect.height));
    assert(closeEnough(densityPlan.overlaps[0].fillPolygon[1].y,
                       layout.plotRect.y + layout.plotRect.height - layout.plotRect.height * 0.225));
    assert(BuildHistogramDensityRenderPlan(layout.plotRect, {blueCurve}, 1.0, 1.0).curves.empty());

    HistogramRenderInput renderInput;
    renderInput.layout = layout;
    renderInput.binRows = {{1, 2, 3, 4}, {5, 6}, {}, {7}};
    renderInput.selection = S({2, 3, 5});
    renderInput.rowColors = {{3, "orange"}, {4, "blue"}, {5, "orange"}};
    renderInput.showCounts = true;
    renderInput.showColorSegments = true;
    renderInput.showRug = true;
    renderInput.rugCases = {{1, 0}, {2, 3}, {3, 99}};
    renderInput.densityCurves = {blueCurve, orangeCurve};
    renderInput.densityXMinimum = 0.0;
    renderInput.densityXMaximum = 10.0;
    HistogramRenderPlan renderPlan = BuildHistogramRenderPlan(renderInput);
    assert(renderPlan.bars.size() == 4);
    assert(renderPlan.bars[0].selectedSegments.size() == 2);
    assert(renderPlan.rugs.size() == 2);
    assert(renderPlan.density.curves.size() == 2);
    assert(renderPlan.density.overlaps.size() == 1);
    assert(closeEnough(renderPlan.density.curves[0].fillPolygon.back().y, barBaseline));
    renderInput.showRug = false;
    renderInput.showColorSegments = false;
    HistogramRenderPlan sparseRenderPlan = BuildHistogramRenderPlan(renderInput);
    assert(sparseRenderPlan.bars.size() == 4);
    assert(sparseRenderPlan.bars[0].colorSegments.empty());
    assert(sparseRenderPlan.rugs.empty());

    assert(rlispstat::core::HistogramNoFiniteValuesStatus() == "The selected variable has no finite values to plot.");
    assert(rlispstat::core::HistogramWindowTitle() == "Histogram");

    return 0;
}
