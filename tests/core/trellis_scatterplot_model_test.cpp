#include "../../src/core/trellis_scatterplot_model.h"
#include "../../src/core/command_dispatcher.h"

#include <cassert>
#include <algorithm>
#include <cmath>
#include <set>
#include <string>
#include <vector>

using namespace rlispstat::core;

static DataFrameModel CarsData()
{
    DataFrameModel data;
    data.group = "cars";
    data.rows = 7;
    DataColumn x;
    x.name = "mpg";
    x.type = "numeric";
    x.values = {"33.9", "21.0", "15.2", "24.4", "19.2", "10.4", "NA"};
    DataColumn y;
    y.name = "wt";
    y.type = "numeric";
    y.values = {"1.835", "3.215", "3.780", "3.190", "3.845", "5.424", "2.620"};
    DataColumn condition;
    condition.name = "cyl";
    condition.type = "factor";
    condition.values = {"4", "6", "8", "4", "6", "8", "4"};
    condition.definedLevels = {"4", "6", "8"};
    DataColumn qsec;
    qsec.name = "qsec";
    qsec.type = "numeric";
    qsec.values = {"19.47", "19.44", "17.30", "17.05", "18.30", "17.82", "18.61"};
    DataColumn am;
    am.name = "am";
    am.type = "factor";
    am.values = {"1", "0", "0", "0", "1", "0", "1"};
    am.definedLevels = {"0", "1"};
    DataColumn hp;
    hp.name = "hp";
    hp.type = "numeric";
    hp.values = {"66", "110", "150", "62", "123", "215", "91"};
    DataColumn vs;
    vs.name = "vs";
    vs.type = "factor";
    vs.values = {"1", "1", "0", "1", "0", "0", "1"};
    vs.definedLevels = {"0", "1"};
    DataColumn date;
    date.name = "date";
    date.type = "datetime";
    date.values = {"2026-01-01", "2026-01-02", "2026-01-03", "2026-01-04",
                   "2026-01-05", "2026-01-06", "2026-01-07"};
    data.columns = {x, y, condition, qsec, am, hp, vs, date};
    return data;
}

static PlotModel PlotWithPanelCount(std::size_t panelCount)
{
    PlotModel plot;
    plot.kind = "trellis_scatterplot";
    plot.xLabel = "x";
    plot.yLabel = "y";
    plot.trellisConditionVariable = "group";
    for (std::size_t index = 0; index < panelCount; ++index) {
        const std::string level = std::to_string(index + 1);
        plot.trellisPanelLevels.push_back(level);
        plot.trellisPointPanels.push_back(level);
        plot.points.push_back(DataPoint{
            10.0 + static_cast<double>(index),
            1.0 + static_cast<double>(index % 4),
            static_cast<int>(index + 1)
        });
    }
    return plot;
}

static bool RectInside(const Rect &inner, const Rect &outer)
{
    const double epsilon = 1.0e-8;
    return inner.x + epsilon >= outer.x && inner.y + epsilon >= outer.y &&
        inner.x + inner.width <= outer.x + outer.width + epsilon &&
        inner.y + inner.height <= outer.y + outer.height + epsilon;
}

static void AssertEqualPanelSizesAndBounds(const TrellisScatterplotLayout &layout,
                                           const Rect &bounds)
{
    assert(!layout.panels.empty());
    const double width = layout.panels.front().plotRect.width;
    const double height = layout.panels.front().plotRect.height;
    for (const TrellisPanelLayout &panel : layout.panels) {
        assert(std::fabs(panel.plotRect.width - width) < 1.0e-8);
        assert(std::fabs(panel.plotRect.height - height) < 1.0e-8);
        assert(RectInside(panel.frame, bounds));
    }
    assert(RectInside(layout.panelsBoundingRect, bounds));
}

int main()
{
    DataFrameModel data = CarsData();
    PlotModel plot;
    plot.group = data.group;
    plot.xLabel = "mpg";
    plot.yLabel = "wt";
    plot.trellisConditionVariable = "cyl";
    std::string error;
    assert(RebuildTrellisScatterplotFromDataFrame(plot, data, &error));
    assert(plot.kind == "trellis_scatterplot");
    assert(plot.points.size() == 6);
    assert((plot.trellisPanelLevels == std::vector<std::string>{"4", "6", "8"}));
    assert(plot.trellisPointPanels[0] == "4");
    assert(plot.points.front().row == 1);
    TrellisDerivedMetadata metadata = DeriveTrellisMetadata(plot);
    assert(metadata.xVariableId == "mpg");
    assert(metadata.xAxisLabel == "mpg");
    assert(metadata.yVariableId == "wt");
    assert(metadata.internalTitle == "wt by mpg, conditioned by cyl");
    assert(metadata.internalTitle.find("qsec") == std::string::npos);

    plot.trellisSpecification.xVariableId = "qsec";
    assert(RebuildTrellisPlotFromDataFrame(plot, data, &error));
    metadata = DeriveTrellisMetadata(plot);
    assert(metadata.xAxisLabel == "qsec");
    assert(metadata.internalTitle == "wt by qsec, conditioned by cyl");
    assert(std::fabs(plot.points.front().x - 19.47) < 1e-9);
    plot.trellisSpecification.xVariableId = "mpg";
    assert(RebuildTrellisPlotFromDataFrame(plot, data, &error));
    assert(DeriveTrellisMetadata(plot).internalTitle == "wt by mpg, conditioned by cyl");

    TrellisScatterplotLayout layout = BuildTrellisScatterplotLayout(plot, Rect{0, 0, 980, 500});
    assert(layout.panels.size() == 3);
    assert(layout.columns == 3);
    assert(layout.rows == 1);
    assert(layout.cases.size() == 6);
    assert(layout.viewport.xmin < 10.4 && layout.viewport.xmax > 33.9);
    assert(layout.viewport.ymin < 1.835 && layout.viewport.ymax > 5.424);
    assert(layout.panels[0].xTicks.size() == layout.panels[1].xTicks.size());
    for (std::size_t i = 0; i < layout.panels[0].xTicks.size(); ++i) {
        assert(layout.panels[0].xTicks[i].value == layout.panels[1].xTicks[i].value);
    }
    assert(layout.panels[0].yTicks.size() == layout.panels[2].yTicks.size());
    assert(!layout.panels[0].showYTickLabels);
    assert(!layout.panels[1].showYTickLabels);
    assert(layout.panels[2].showYTickLabels);
    assert(layout.panels[0].showXTickLabels);
    assert(layout.panels[0].panelId == "cyl=4");
    AssertEqualPanelSizesAndBounds(layout, Rect{0, 0, 980, 500});
    assert(layout.xAxisLabelAnchor.x > layout.panelsBoundingRect.x);
    assert(layout.xAxisLabelAnchor.x < layout.panelsBoundingRect.x + layout.panelsBoundingRect.width);
    assert(layout.yAxisLabelAnchor.y > layout.panelsBoundingRect.y);
    assert(layout.yAxisLabelAnchor.y < layout.panelsBoundingRect.y + layout.panelsBoundingRect.height);

    const TrellisCaseGeometry first = layout.cases.front();
    assert(first.panelIndex == 0);
    const Point inverse = TrellisScreenToData(layout, first.panelIndex, first.screenPoint);
    assert(std::fabs(inverse.x - first.dataPoint.x) < 1e-9);
    assert(std::fabs(inverse.y - first.dataPoint.y) < 1e-9);
    assert(TrellisPanelIndexAtPoint(layout, first.screenPoint) == first.panelIndex);
    assert(!TrellisPanelIndexAtPoint(layout, Point{2, 2}));

    std::set<CaseId> clicked = SelectTrellisScatterplotCasesForGesture(
        layout, Rect{}, first.screenPoint, false, 8.0, 2.0);
    assert(clicked == std::set<CaseId>{first.caseId});
    Rect brush{first.screenPoint.x - 4.0, first.screenPoint.y - 4.0, 8.0, 8.0};
    std::set<CaseId> brushed = SelectTrellisScatterplotCasesForGesture(
        layout, brush, first.screenPoint, true, 8.0, 2.0);
    assert(brushed.count(first.caseId) == 1);

    TrellisScatterplotLayout resized = BuildTrellisScatterplotLayout(plot, Rect{0, 0, 1220, 620});
    assert(resized.panels.size() == layout.panels.size());
    assert(resized.panels[0].plotRect.width > layout.panels[0].plotRect.width);
    assert(resized.viewport.xmin == layout.viewport.xmin);
    assert(resized.viewport.ymax == layout.viewport.ymax);

    TrellisScatterplotLayout narrow = BuildTrellisScatterplotLayout(plot, Rect{0, 0, 420, 800});
    assert(narrow.columns == 1);
    assert(narrow.rows == 3);
    assert(narrow.panels[0].showYTickLabels);
    assert(narrow.panels[1].showYTickLabels);
    assert(!narrow.panels[0].showXTickLabels);
    assert(narrow.panels[2].showXTickLabels);
    AssertEqualPanelSizesAndBounds(narrow, Rect{0, 0, 420, 800});

    const std::vector<std::pair<std::size_t, std::size_t>> expectedWideColumns = {
        {1, 1}, {2, 2}, {3, 3}, {4, 2}, {5, 3}, {6, 3},
        {7, 4}, {8, 4}, {9, 3}, {12, 4}
    };
    for (const auto &expectation : expectedWideColumns) {
        PlotModel candidate = PlotWithPanelCount(expectation.first);
        const Rect candidateBounds{0, 0,
            expectation.first >= 7 ? 1400.0 : 1100.0,
            expectation.first >= 9 ? 760.0 : 620.0};
        const TrellisScatterplotLayout candidateLayout =
            BuildTrellisScatterplotLayout(candidate, candidateBounds);
        assert(candidateLayout.columns == expectation.second);
        assert(candidateLayout.rows ==
               (expectation.first + expectation.second - 1) / expectation.second);
        AssertEqualPanelSizesAndBounds(candidateLayout, candidateBounds);
    }

    PlotModel five = PlotWithPanelCount(5);
    const TrellisScatterplotLayout fiveLayout =
        BuildTrellisScatterplotLayout(five, Rect{0, 0, 1100, 620});
    assert(fiveLayout.columns == 3 && fiveLayout.rows == 2);
    assert(fiveLayout.panels[3].frame.x > fiveLayout.panels[0].frame.x);
    assert(fiveLayout.panels[4].frame.x + fiveLayout.panels[4].frame.width <
           fiveLayout.panels[2].frame.x + fiveLayout.panels[2].frame.width);

    five.trellisLayoutMode = "one_row";
    assert(BuildTrellisScatterplotLayout(five, Rect{0, 0, 1100, 620}).columns == 5);
    five.trellisLayoutMode = "one_column";
    assert(BuildTrellisScatterplotLayout(five, Rect{0, 0, 620, 1100}).columns == 1);
    five.trellisLayoutMode = "two_columns";
    assert(BuildTrellisScatterplotLayout(five, Rect{0, 0, 900, 700}).columns == 2);
    five.trellisLayoutMode = "three_columns";
    assert(BuildTrellisScatterplotLayout(five, Rect{0, 0, 900, 700}).columns == 3);
    five.trellisLayoutMode = "grid";
    assert(BuildTrellisScatterplotLayout(five, Rect{0, 0, 900, 700}).columns == 3);
    five.trellisLayoutMode = "automatic";
    assert(BuildTrellisScatterplotLayout(five, Rect{0, 0, 1100, 620}).columns == 3);

    PlotModel ordered = PlotWithPanelCount(3);
    ordered.trellisPanelLevels = {"10", "2", "8"};
    assert((OrderedTrellisPanelLevels(ordered) == std::vector<std::string>{"10", "2", "8"}));
    ordered.trellisPanelOrder = "ascending";
    assert((OrderedTrellisPanelLevels(ordered) == std::vector<std::string>{"2", "8", "10"}));
    ordered.trellisPanelOrder = "descending";
    assert((OrderedTrellisPanelLevels(ordered) == std::vector<std::string>{"10", "8", "2"}));

    const Rect tickRect{0, 0, 400, 240};
    const std::vector<TrellisAxisTick> mpgTicks =
        BuildTrellisAxisTicks(9.225, 35.075, tickRect, true, 6);
    assert(mpgTicks.size() >= 4 && mpgTicks.size() <= 7);
    assert(mpgTicks.front().label == "10");
    assert(mpgTicks.back().label == "35");
    for (const TrellisAxisTick &tick : mpgTicks) {
        assert(tick.label.find(".0000") == std::string::npos);
    }
    assert(!BuildTrellisAxisTicks(-3.4, 8.7, tickRect, true, 6).empty());
    assert(!BuildTrellisAxisTicks(0.00011, 0.00019, tickRect, true, 6).empty());
    const std::vector<TrellisAxisTick> largeTicks =
        BuildTrellisAxisTicks(1.0e6, 6.0e6, tickRect, true, 6);
    assert(!largeTicks.empty());
    assert(largeTicks.front().label.find('e') != std::string::npos);

    PlotModel constant = PlotWithPanelCount(2);
    for (DataPoint &point : constant.points) { point.x = 3.0; point.y = -2.0; }
    const TrellisScatterplotLayout constantLayout =
        BuildTrellisScatterplotLayout(constant, Rect{0, 0, 800, 480});
    assert(!constantLayout.panels[0].xTicks.empty());
    assert(!constantLayout.panels[0].yTicks.empty());

    const Rect spanningBrush = layout.panelsBoundingRect;
    const std::set<CaseId> clippedSelection = SelectTrellisScatterplotCasesForGesture(
        layout, spanningBrush, layout.cases.front().screenPoint, true, 8.0, 2.0);
    assert(!clippedSelection.empty());
    for (CaseId selectedCase : clippedSelection) {
        const auto found = std::find_if(layout.cases.begin(), layout.cases.end(),
            [selectedCase](const TrellisCaseGeometry &entry) {
                return entry.caseId == selectedCase;
            });
        assert(found != layout.cases.end());
        assert(found->panelIndex == layout.cases.front().panelIndex);
    }

    PlotModel invalid = plot;
    invalid.trellisSpecification.yVariableId = invalid.trellisSpecification.xVariableId;
    assert(!RebuildTrellisScatterplotFromDataFrame(invalid, data, &error));
    invalid = plot;
    invalid.trellisSpecification.conditioningVariables.front().variableId = "wt";
    invalid.trellisSpecification.conditioningVariables.front().variableLabel = "wt";
    assert(!RebuildTrellisScatterplotFromDataFrame(invalid, data, &error));

    PlotModel general = plot;
    assert(AddTrellisConditioningVariable(general, data, "am",
        TrellisConditioningVariableKind::Categorical,
        TrellisContinuousBinningMethod::EqualWidth, &error));
    assert(general.trellisSpecification.conditioningVariables.size() == 2);
    assert(ReplaceTrellisConditioningVariable(general, data, "am", "vs", &error));
    assert(general.trellisSpecification.conditioningVariables[1].variableId == "vs");
    assert(ReplaceTrellisConditioningVariable(general, data, "vs", "am", &error));
    assert(general.trellisPanelLevels.size() == 6);
    const TrellisScatterplotLayout matrix = BuildTrellisScatterplotLayout(
        general, Rect{0, 0, 1100, 650});
    assert(matrix.columns == 3);
    assert(matrix.rows == 2);
    assert(matrix.panels.size() == 6);
    assert(general.trellisSpecification.conditioningVariables[0].dimension == TrellisDimension::Columns);
    assert(general.trellisSpecification.conditioningVariables[1].dimension == TrellisDimension::Rows);
    assert(matrix.panels.front().columnStripLabel.find("cyl =") == 0);
    assert(matrix.panels.front().rowStripLabel.find("am =") == 0);
    const auto rowStripPanel = std::find_if(matrix.panels.begin(), matrix.panels.end(),
        [](const TrellisPanelLayout &panel) { return IsValidRect(panel.rowStripRect); });
    assert(rowStripPanel != matrix.panels.end());
    assert(rowStripPanel->column == 0);
    assert(rowStripPanel->rowStripRect.x + rowStripPanel->rowStripRect.width <=
           rowStripPanel->plotRect.x - 6.0 + 1.0e-9);
    assert(RectInside(rowStripPanel->rowStripRect, Rect{0, 0, 1100, 650}));
    const Point rowStripCenter{
        rowStripPanel->rowStripRect.x + rowStripPanel->rowStripRect.width / 2.0,
        rowStripPanel->rowStripRect.y + rowStripPanel->rowStripRect.height / 2.0};
    const std::vector<std::size_t> rowConditionHits =
        TrellisConditioningVariableIndicesAtPoint(general, matrix, rowStripCenter);
    assert(rowConditionHits.size() == 1);
    assert(general.trellisSpecification.conditioningVariables[rowConditionHits.front()].variableId == "am");
    const Point columnStripCenter{
        matrix.panels.front().columnStripRect.x + matrix.panels.front().columnStripRect.width / 2.0,
        matrix.panels.front().columnStripRect.y + matrix.panels.front().columnStripRect.height / 2.0};
    const std::vector<std::size_t> columnConditionHits =
        TrellisConditioningVariableIndicesAtPoint(general, matrix, columnStripCenter);
    assert(columnConditionHits.size() == 1);
    assert(general.trellisSpecification.conditioningVariables[columnConditionHits.front()].variableId == "cyl");
    assert(std::all_of(matrix.panels.begin(), matrix.panels.end(),
        [&](const TrellisPanelLayout &panel) {
            return panel.showYTickLabels == (panel.column + 1 == matrix.columns);
        }));
    PlotModel reversedSides = general;
    reversedSides.trellisRowStripsOnLeft = false;
    const TrellisScatterplotLayout reversedSideLayout = BuildTrellisScatterplotLayout(
        reversedSides, Rect{0, 0, 1100, 650});
    const auto reversedRowStrip = std::find_if(reversedSideLayout.panels.begin(),
        reversedSideLayout.panels.end(),
        [](const TrellisPanelLayout &panel) { return IsValidRect(panel.rowStripRect); });
    assert(reversedRowStrip != reversedSideLayout.panels.end());
    assert(reversedRowStrip->column + 1 == reversedSideLayout.columns);
    assert(reversedRowStrip->rowStripRect.x >=
           reversedRowStrip->plotRect.x + reversedRowStrip->plotRect.width + 6.0 - 1.0e-9);
    assert(std::all_of(reversedSideLayout.panels.begin(), reversedSideLayout.panels.end(),
        [&](const TrellisPanelLayout &panel) {
            return panel.showYTickLabels == (panel.column == 0);
        }));
    assert(std::any_of(matrix.panels.begin(), matrix.panels.end(),
        [](const TrellisPanelLayout &panel) { return !panel.hasObservations; }));
    const std::vector<std::string> panelIdsBeforeSwap = general.trellisPanelLevels;
    const std::vector<DataPoint> pointsBeforeSwap = general.points;
    assert(SwapTrellisRowsAndColumns(general, data, &error));
    const TrellisScatterplotLayout swapped = BuildTrellisScatterplotLayout(
        general, Rect{0, 0, 1100, 650});
    assert(swapped.columns == 2);
    assert(swapped.rows == 3);
    assert(general.trellisPanelLevels == panelIdsBeforeSwap);
    assert(general.points.size() == pointsBeforeSwap.size());
    for (std::size_t i = 0; i < pointsBeforeSwap.size(); ++i) {
        assert(general.points[i].row == pointsBeforeSwap[i].row);
    }
    assert(SwapTrellisRowsAndColumns(general, data, &error));
    general.trellisSpecification.scaleMode = "free_y";
    const TrellisScatterplotLayout freeY = BuildTrellisScatterplotLayout(
        general, Rect{0, 0, 1100, 650});
    assert(freeY.panels[0].viewport.xmin == freeY.panels[1].viewport.xmin);
    assert(freeY.panels[0].viewport.xmax == freeY.panels[1].viewport.xmax);
    assert(std::any_of(freeY.panels.begin() + 1, freeY.panels.end(), [&](const TrellisPanelLayout &panel) {
        return panel.viewport.ymin != freeY.panels.front().viewport.ymin ||
               panel.viewport.ymax != freeY.panels.front().viewport.ymax;
    }));
    general.trellisSpecification.scaleMode = "common_xy";
    assert(AddTrellisConditioningVariable(general, data, "hp",
        TrellisConditioningVariableKind::ContinuousBinned,
        TrellisContinuousBinningMethod::EqualWidth, &error));
    assert(general.trellisSpecification.conditioningVariables.size() == 3);
    assert(general.trellisPanelLevels.size() == 24);
    assert(ConfigureTrellisContinuousCondition(general, data, "hp",
        TrellisContinuousBinningMethod::EqualCount, 2, &error));
    assert(general.trellisPanelLevels.size() <= 12);
    assert(MoveTrellisConditioningVariable(general, data, "am", -1, &error));
    assert(general.trellisSpecification.conditioningVariables.front().variableId == "am");
    assert(RemoveTrellisConditioningVariable(general, data, "hp", &error));

    PlotModel histogram = plot;
    histogram.trellisSpecification.plotType = TrellisPlotType::Histogram;
    histogram.trellisSpecification.histogramBinCount = 5;
    assert(RebuildTrellisPlotFromDataFrame(histogram, data, &error));
    TrellisScatterplotLayout histogramLayout = BuildTrellisScatterplotLayout(
        histogram, Rect{0, 0, 980, 500});
    assert(histogramLayout.aggregates.size() == histogram.trellisPanelLevels.size() * 5);
    assert(DeriveTrellisMetadata(histogram).internalTitle == "Distribution of mpg, conditioned by cyl");
    for (const TrellisAggregateGeometry &bin : histogramLayout.aggregates) {
        assert(RectInside(bin.rect, histogramLayout.panels[bin.panelIndex].plotRect));
    }
    const auto histogramPlans = BuildTrellisPanelRenderPlans(histogram, histogramLayout, {}, {});
    assert(histogramPlans.size() == histogramLayout.panels.size());
    for (const auto &panelPlan : histogramPlans) {
        for (const auto &barItem : panelPlan.histogram.bars) {
            assert(RectInside(barItem.rect, panelPlan.context.plotRect));
        }
    }

    PlotModel bars = plot;
    bars.trellisSpecification.plotType = TrellisPlotType::Bar;
    bars.trellisSpecification.xVariableId = "am";
    assert(RebuildTrellisPlotFromDataFrame(bars, data, &error));
    TrellisScatterplotLayout barLayout = BuildTrellisScatterplotLayout(bars, Rect{0, 0, 980, 500});
    assert(!barLayout.aggregates.empty());
    const auto barPlans = BuildTrellisPanelRenderPlans(bars, barLayout, {}, {});
    for (const auto &panelPlan : barPlans) {
        const std::vector<Rect> drawnRects = BarplotBarRects(panelPlan.barplotLayout);
        for (std::size_t index = 0;
             index < drawnRects.size() && index < panelPlan.barplotBins.size(); ++index) {
            const auto aggregate = std::find_if(barLayout.aggregates.begin(), barLayout.aggregates.end(),
                [&](const TrellisAggregateGeometry &candidate) {
                    return candidate.panelIndex == panelPlan.panelIndex &&
                           candidate.label == panelPlan.barplotBins[index].category;
                });
            assert(aggregate != barLayout.aggregates.end());
            assert(std::fabs(aggregate->rect.x - drawnRects[index].x) < 1.0e-9);
            assert(std::fabs(aggregate->rect.y - drawnRects[index].y) < 1.0e-9);
            assert(std::fabs(aggregate->rect.width - drawnRects[index].width) < 1.0e-9);
            assert(std::fabs(aggregate->rect.height - drawnRects[index].height) < 1.0e-9);
            if (!aggregate->caseIds.empty() && drawnRects[index].height > 1.0) {
                const Point nearDrawnTop{
                    drawnRects[index].x + drawnRects[index].width * 0.5,
                    drawnRects[index].y + 0.5};
                const std::set<CaseId> selectedNearTop =
                    SelectTrellisScatterplotCasesForGesture(
                        barLayout, {}, nearDrawnTop, false, 8.0, 2.0);
                assert(selectedNearTop ==
                       std::set<CaseId>(aggregate->caseIds.begin(), aggregate->caseIds.end()));
            }
        }
    }
    const TrellisAggregateGeometry bar = barLayout.aggregates.front();
    if (!bar.caseIds.empty()) {
        const Point center{bar.rect.x + bar.rect.width / 2.0, bar.rect.y + bar.rect.height / 2.0};
        const std::set<CaseId> selectedBar = SelectTrellisScatterplotCasesForGesture(
            barLayout, {}, center, false, 8.0, 2.0);
        assert(selectedBar.size() == bar.caseIds.size());
    }
    DataColumn split;
    split.name = "split";
    split.type = "factor";
    split.values = {"A", "B", "A", "B", "A", "B", "A"};
    split.definedLevels = {"A", "B"};
    data.columns.push_back(split);
    bars.trellisSpecification.splitVariableId = "split";
    assert(RebuildTrellisPlotFromDataFrame(bars, data, &error));
    barLayout = BuildTrellisScatterplotLayout(bars, Rect{0, 0, 980, 500});
    const auto splitBarPlans = BuildTrellisPanelRenderPlans(bars, barLayout, {}, {});
    assert(std::any_of(splitBarPlans.begin(), splitBarPlans.end(), [](const auto &panelPlan) {
        return std::any_of(panelPlan.barplotSegments.begin(), panelPlan.barplotSegments.end(),
            [](const auto &segments) { return segments.size() > 1; });
    }));
    for (const auto &panelPlan : splitBarPlans) {
        assert(panelPlan.barplotLayout.plotRect.x == panelPlan.context.plotRect.x);
        assert(panelPlan.barplotLayout.plotRect.width == panelPlan.context.plotRect.width);
    }

    PlotModel boxes = plot;
    boxes.trellisSpecification.plotType = TrellisPlotType::Boxplot;
    boxes.trellisSpecification.xVariableId = "am";
    boxes.trellisSpecification.yVariableId = "mpg";
    assert(RebuildTrellisPlotFromDataFrame(boxes, data, &error));
    const TrellisScatterplotLayout boxLayout =
        BuildTrellisScatterplotLayout(boxes, Rect{0, 0, 980, 500});
    assert(!boxLayout.aggregates.empty());
    assert(boxLayout.cases.size() == boxes.points.size());
    const auto boxPlans = BuildTrellisPanelRenderPlans(boxes, boxLayout, {}, {});
    std::size_t renderedBoxPoints = 0;
    for (const auto &panelPlan : boxPlans) {
        renderedBoxPoints += panelPlan.boxplotPoints.size();
        for (const auto &point : panelPlan.boxplotPoints) {
            const auto interactive = std::find_if(
                boxLayout.cases.begin(), boxLayout.cases.end(),
                [&](const TrellisCaseGeometry &entry) {
                    return entry.caseId == point.caseId &&
                        entry.panelIndex == panelPlan.panelIndex;
                });
            assert(interactive != boxLayout.cases.end());
            assert(std::fabs(interactive->screenPoint.x - point.point.x) < 1.0e-9);
            assert(std::fabs(interactive->screenPoint.y - point.point.y) < 1.0e-9);
            assert(SelectTrellisScatterplotCasesForGesture(
                boxLayout, {}, point.point, false, 8.0, 2.0).count(point.caseId) == 1);
        }
    }
    assert(renderedBoxPoints == boxes.points.size());
    boxes.boxplotShowPoints = false;
    assert(BuildTrellisScatterplotLayout(boxes, Rect{0, 0, 980, 500}).cases.empty());

    PlotModel timeSeries = plot;
    timeSeries.trellisSpecification.plotType = TrellisPlotType::TimeSeries;
    timeSeries.trellisSpecification.xVariableId = "date";
    timeSeries.trellisSpecification.yVariableId = "mpg";
    timeSeries.trellisSpecification.groupingVariableId = "am";
    timeSeries.timeSeriesIdentification = "legend";
    assert(RebuildTrellisPlotFromDataFrame(timeSeries, data, &error));
    assert(timeSeries.timeSeriesTimeType == "date");
    const TrellisScatterplotLayout timeLayout =
        BuildTrellisScatterplotLayout(timeSeries, Rect{0, 0, 980, 500});
    assert(timeLayout.cases.size() == timeSeries.points.size());
    const auto timePlans = BuildTrellisPanelRenderPlans(timeSeries, timeLayout, {}, {});
    assert(std::any_of(timePlans.begin(), timePlans.end(), [](const auto &panelPlan) {
        return !panelPlan.timeSeriesLines.empty();
    }));
    for (const auto &panelPlan : timePlans) {
        for (const auto &line : panelPlan.timeSeriesLines) {
            assert(line.points.size() == line.caseIds.size());
            for (std::size_t i = 1; i < line.points.size(); ++i) {
                assert(line.points[i - 1].x <= line.points[i].x);
            }
        }
    }
    assert(DeriveTrellisMetadata(timeSeries).internalTitle.find("mpg over date") == 0);

    PlotModel regression = plot;
    AddOverlaySource(regression, "all");
    regression.overlays.back().type = "lm";
    const TrellisScatterplotLayout regressionLayout = BuildTrellisScatterplotLayout(
        regression, Rect{0, 0, 980, 500});
    const auto regressionPlans = BuildTrellisPanelRenderPlans(regression, regressionLayout, {}, {});
    for (const auto &panelPlan : regressionPlans) {
        for (const auto &line : panelPlan.scatterplot.overlayLines) {
            assert(PointInRect(line.start, panelPlan.context.plotClip));
            assert(PointInRect(line.end, panelPlan.context.plotClip));
        }
    }
    regression.smoothCurves = {PendingSmoothCurve(SmoothCurveScope::Overall)};
    regression.trellisPanelSmoothCurves[regression.trellisPanelLevels.front()] = {
        SmoothCurveData{SmoothCurveScope::Overall, ".", {10.0, 20.0, 40.0},
                        {100.0, 20.0, -50.0}, true, ""}
    };
    const auto smoothPlans = BuildTrellisPanelRenderPlans(regression, regressionLayout, {}, {});
    assert(!smoothPlans.front().scatterplot.smoothCurves.empty());

    PlotModel changedSmoothInputs = regression;
    changedSmoothInputs.smoothCurves.push_back(
        PendingSmoothCurve(SmoothCurveScope::Selection));
    changedSmoothInputs.trellisPanelSmoothCurves["orphaned-panel"] = {
        SmoothCurveData{SmoothCurveScope::ColorGroup, "blue", {10.0, 20.0},
                        {30.0, 40.0}, true, ""}
    };
    assert(InvalidateTrellisSmoothCurvesForDataChange(changedSmoothInputs));
    assert(changedSmoothInputs.trellisPanelSmoothCurves.empty());
    assert(SmoothCurveScopeIsPresent(changedSmoothInputs.smoothCurves,
                                     SmoothCurveScope::Overall));
    assert(SmoothCurveScopeIsPresent(changedSmoothInputs.smoothCurves,
                                     SmoothCurveScope::Selection));
    assert(SmoothCurveScopeIsPresent(changedSmoothInputs.smoothCurves,
                                     SmoothCurveScope::ColorGroup));
    assert(std::all_of(changedSmoothInputs.smoothCurves.begin(),
                       changedSmoothInputs.smoothCurves.end(),
                       [](const SmoothCurveData &curve) {
                           return !curve.ok && curve.message == "needed";
                       }));

    PlotModel dataTables = regression;
    InitializeTrellisDataTableSpecification(dataTables, data);
    assert(dataTables.trellisSpecification.dataTable.initialized);
    assert(dataTables.trellisSpecification.dataTable.displayedVariableIds.size() >= 4);
    dataTables.trellisSpecification.dataTable.displayedVariableIds = {"mpg", "wt", "hp", "qsec"};
    dataTables.trellisSpecification.dataTable.columnWidths["mpg"] = 112.0;
    dataTables.trellisSpecification.plotType = TrellisPlotType::DataTable;
    assert(RebuildTrellisPlotFromDataFrame(dataTables, data, &error));
    assert(dataTables.points.size() == 7); // A missing displayed value does not remove its row.
    assert(dataTables.overlays.size() == regression.overlays.size());
    assert(dataTables.smoothCurves.size() == regression.smoothCurves.size());
    const TrellisScatterplotLayout tableLayout = BuildTrellisScatterplotLayout(
        dataTables, Rect{0, 0, 980, 500});
    assert(tableLayout.panels.size() == 3);
    assert(tableLayout.cases.empty());
    assert(tableLayout.panels.front().xTicks.empty());
    assert((TrellisPanelOriginalRows(dataTables, "4") == std::vector<int>{1, 4, 7}));
    auto &tableSpec = dataTables.trellisSpecification.dataTable;
    tableSpec.sort.order = TrellisDataTableRowOrder::SelectedFirst;
    assert((OrderedTrellisDataTableRows(tableSpec, data, {1, 4, 7}, {4}) ==
            std::vector<int>{4, 1, 7}));
    tableSpec.sort.order = TrellisDataTableRowOrder::VariableDescending;
    tableSpec.sort.variableId = "mpg";
    assert((OrderedTrellisDataTableRows(tableSpec, data, {1, 4, 7}, {}) ==
            std::vector<int>{1, 4, 7})); // Missing values remain last in both directions.
    tableSpec.sort.order = TrellisDataTableRowOrder::VariableAscending;
    assert((OrderedTrellisDataTableRows(tableSpec, data, {1, 4, 7}, {}) ==
            std::vector<int>{4, 1, 7}));
    const std::string tableTsv = TrellisDataTableTSV(dataTables, data, {1, 4, 7}, true, "4");
    assert(tableTsv.find("Panel_ID\tRow\tmpg\twt\thp\tqsec\tcyl") == 0);
    assert(tableTsv.find("\t7\t") != std::string::npos);
    const std::string tableCsv = TrellisDataTableAllPanelsCSV(dataTables, data, {4});
    assert(tableCsv.find("Original_Row_ID,Selected") != std::string::npos);
    assert(tableCsv.find(",4,TRUE,") != std::string::npos);
    assert(TrellisDataTablePanelCSV(dataTables, data, "4", {4}).find("4,4,TRUE") != std::string::npos);
    assert(ParseTrellisPlotType("data_table") == TrellisPlotType::DataTable);

    tableSpec.verticalScrollMode = TrellisDataTableVerticalScrollMode::Independent;
    tableSpec.firstVisibleRow = 2;
    tableSpec.firstVisibleRowsByPanel["4"] = 1;
    tableSpec.horizontalOffset = 37.0;
    dataTables.trellisSpecification.plotType = TrellisPlotType::Scatter;
    assert(RebuildTrellisPlotFromDataFrame(dataTables, data, &error));
    assert(dataTables.trellisSpecification.dataTable.displayedVariableIds ==
           std::vector<std::string>({"mpg", "wt", "hp", "qsec"}));
    assert(dataTables.trellisSpecification.dataTable.columnWidths["mpg"] == 112.0);
    assert(dataTables.trellisSpecification.dataTable.horizontalOffset == 37.0);
    dataTables.trellisSpecification.plotType = TrellisPlotType::DataTable;
    assert(RebuildTrellisPlotFromDataFrame(dataTables, data, &error));
    assert(dataTables.trellisSpecification.dataTable.verticalScrollMode ==
           TrellisDataTableVerticalScrollMode::Independent);
    assert(dataTables.trellisSpecification.dataTable.firstVisibleRowsByPanel["4"] == 1);
    assert(dataTables.overlays.size() == regression.overlays.size());
    assert(dataTables.smoothCurves.size() == regression.smoothCurves.size());

    TrellisConditioningVariable secondCondition;
    secondCondition.variableId = "am";
    secondCondition.variableLabel = "am";
    secondCondition.kind = TrellisConditioningVariableKind::Categorical;
    secondCondition.dimension = TrellisDimension::Rows;
    dataTables.trellisSpecification.conditioningVariables.front().dimension = TrellisDimension::Columns;
    dataTables.trellisSpecification.conditioningVariables.push_back(secondCondition);
    assert(RebuildTrellisPlotFromDataFrame(dataTables, data, &error));
    const TrellisScatterplotLayout crossedTableLayout = BuildTrellisScatterplotLayout(
        dataTables, Rect{0, 0, 980, 620});
    assert(crossedTableLayout.panels.size() == 6);
    assert(std::any_of(crossedTableLayout.panels.begin(), crossedTableLayout.panels.end(),
        [](const TrellisPanelLayout &panel) { return !panel.hasObservations; }));
    assert(dataTables.trellisSpecification.dataTable.displayedVariableIds ==
           std::vector<std::string>({"mpg", "wt", "hp", "qsec"}));
    assert(dataTables.trellisSpecification.dataTable.columnWidths["mpg"] == 112.0);

    dataTables.trellisSpecification.conditioningVariables.clear();
    assert(RebuildTrellisPlotFromDataFrame(dataTables, data, &error));
    const TrellisScatterplotLayout unconditionedTableLayout = BuildTrellisScatterplotLayout(
        dataTables, Rect{0, 0, 720, 500});
    assert(unconditionedTableLayout.panels.size() == 1);
    assert(unconditionedTableLayout.panels.front().panelId == "all");
    assert(TrellisPanelOriginalRows(dataTables, "all").size() == 7);

    DataColumn numericCondition;
    numericCondition.name = "gear";
    numericCondition.type = "numeric";
    numericCondition.values = {"5", "3", "4", "5", "3", "4", "5"};
    data.columns.push_back(numericCondition);
    plot.trellisSpecification.conditioningVariables.front().variableId = "gear";
    plot.trellisSpecification.conditioningVariables.front().variableLabel = "gear";
    assert(RebuildTrellisScatterplotFromDataFrame(plot, data, &error));
    assert((plot.trellisPanelLevels == std::vector<std::string>{"3", "4", "5"}));

    PlotModel *created = nullptr;
    CommandDispatcherServices services;
    services.ui.addPlot = [&](PlotModel *candidate) { created = candidate; };
    CommandDispatcher dispatcher(services);
    assert(dispatcher.dispatch({
        "ADD_PLOT", "trellis1", "cars", "mpg", "wt", "wt by mpg, conditioned by cyl", "3",
        "26 2.6 1 0", "21 3.1 2 1", "18 3.4 3 2",
        "TRELLIS_SCATTERPLOT", "cyl", "3", "4", "6", "8",
        "TRELLIS_SCATTERPLOT_OPTIONS", "two_columns", "descending"
    }) == "OK");
    assert(created != nullptr);
    assert(created->kind == "trellis_scatterplot");
    assert(created->trellisConditionVariable == "cyl");
    assert(created->trellisPanelLevels.size() == 3);
    assert(created->trellisPointPanels[2] == "8");
    assert(created->trellisLayoutMode == "two_columns");
    assert(created->trellisPanelOrder == "descending");
    assert(TitleForPlot(*created) == "Trellis Plot — cars");
    assert(ParseCommandRequest({"TRELLIS_SCATTERPLOT_SET_LAYOUT", "trellis1", "automatic"}).action ==
           CommandAction::SetTrellisScatterplotLayout);
    assert(ParseCommandRequest({"TRELLIS_SCATTERPLOT_SET_ORDER", "trellis1", "ascending"}).action ==
           CommandAction::SetTrellisScatterplotOrder);
    assert(ParseCommandRequest({"TRELLIS_SCATTERPLOT_SET_TYPE", "trellis1", "histogram"}).action ==
           CommandAction::SetTrellisPlotType);
    assert(ParseCommandRequest({"TRELLIS_SCATTERPLOT_ADD_CONDITION_EQUAL_COUNT", "trellis1", "hp"}).action ==
           CommandAction::AddTrellisConditionEqualCount);
    assert(ParseCommandRequest({"TRELLIS_SCATTERPLOT_CONDITION_BINS", "trellis1", "hp", "4"}).action ==
           CommandAction::ConfigureTrellisConditionBins);
    assert(ParseCommandRequest({"TRELLIS_SCATTERPLOT_DIMENSION_ROWS", "trellis1", "am"}).action ==
           CommandAction::SetTrellisConditionDimensionRows);
    assert(ParseCommandRequest({"TRELLIS_SCATTERPLOT_SWAP_DIMENSIONS", "trellis1"}).action ==
           CommandAction::SwapTrellisRowsAndColumns);
    assert(ParseCommandRequest({"ADD_TRELLIS_SMOOTH", "trellis1", "4", "overall", "0"}).action ==
           CommandAction::AddTrellisSmooth);
    assert(dispatcher.dispatch({"SET_SELECTED", "cars", "2", "1", "3"}) == "OK");
    assert(dispatcher.dispatch({"SELECTED", "cars"}) == "OK 1 3");
    delete created;
    return 0;
}
