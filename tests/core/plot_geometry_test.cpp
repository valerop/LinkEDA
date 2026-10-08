#include "../../src/core/plot_geometry.h"
#include "../../src/core/dataset_model.h"
#include "../../src/core/histogram_model.h"
#include "../../src/core/scatterplot_model.h"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <map>

using rlispstat::core::DataToScreen;
using rlispstat::core::AvailableVariableExists;
using rlispstat::core::AvailableVariableNames;
using rlispstat::core::ApplyAnalysisScopeToPlot;
using rlispstat::core::DataColumn;
using rlispstat::core::DataFrameModel;
using rlispstat::core::DataPoint;
using rlispstat::core::DataViewport;
using rlispstat::core::DataViewportForPoints;
using rlispstat::core::ClampSmoothSpan;
using rlispstat::core::ClampPointSizeScale;
using rlispstat::core::ClipLineToRect;
using rlispstat::core::DefaultTermType;
using rlispstat::core::DistanceSquaredToSegment;
using rlispstat::core::BrushRectForGesture;
using rlispstat::core::BuildComparisonSeedPlot;
using rlispstat::core::BuildTimeSeriesLines;
using rlispstat::core::ExpandRect;
using rlispstat::core::FormatTimeSeriesTick;
using rlispstat::core::HitNearestIndexedPoint;
using rlispstat::core::HitNearestLabeledSegment;
using rlispstat::core::HitPlotSeriesAtScreenPoint;
using rlispstat::core::HitTimeSeriesSegmentAtScreenPoint;
using rlispstat::core::IndexedScreenPoint;
using rlispstat::core::InteractionPlotLine;
using rlispstat::core::IsValidViewport;
using rlispstat::core::LabeledScreenSegment;
using rlispstat::core::PlotLinksToDataRows;
using rlispstat::core::PlotInteractionModeAllowsRowSelection;
using rlispstat::core::PlotColorLegendMatchesLinkedRowColors;
using rlispstat::core::PlotEffectCategoryRows;
using rlispstat::core::PlotSeriesIntersectsSelection;
using rlispstat::core::PlotSeriesIsFullySelected;
using rlispstat::core::TimeSeriesRowColors;
using rlispstat::core::RegressionEffectSeriesDodgeOffset;
using rlispstat::core::RegressionEffectXAxisIsCategorical;
using rlispstat::core::IsPooledRegressionEffectPlot;
using rlispstat::core::PlotInfoResponseText;
using rlispstat::core::PlotListItemText;
using rlispstat::core::PlotDiagnosticInfoResponseText;
using rlispstat::core::PlotModel;
using rlispstat::core::PlotOverlaysResponseText;
using rlispstat::core::PopulateDatasetSeedPlot;
using rlispstat::core::PrepareScatterplotVariablesFromDataFrame;
using rlispstat::core::PlotVariablesResponseText;
using rlispstat::core::Point;
using rlispstat::core::PointInRect;
using rlispstat::core::MarkExistingSmoothCurvesPending;
using rlispstat::core::InvalidateSmoothCurvesForCoordinateChange;
using rlispstat::core::RebuildTimeSeriesFromDataFrame;
using rlispstat::core::MarkSmoothCurveScopePendingIfPresent;
using rlispstat::core::ModelTermDisplayType;
using rlispstat::core::NumericVariableNames;
using rlispstat::core::Rect;
using rlispstat::core::RectBetweenPoints;
using rlispstat::core::RectMeetsMinimumSize;
using rlispstat::core::RefreshDatasetSeedPlotAfterVariableSync;
using rlispstat::core::ScreenToData;
using rlispstat::core::SmoothCurveData;
using rlispstat::core::SmoothCurveScope;
using rlispstat::core::SmoothCurveScopeIsPresent;
using rlispstat::core::SyncPlotVariablesFromDataFrame;
using rlispstat::core::SynchronizePlotColorByLinkedRows;
using rlispstat::core::TitleForPlot;
using rlispstat::core::ToggleSmoothCurveScopePending;
using rlispstat::core::VariableRole;

static bool closeEnough(double a, double b)
{
    return std::fabs(a - b) < 1.0e-9;
}

int main()
{
    {
        PlotModel scatter;
        scatter.kind = "scatter";
        scatter.group = "cars";
        scatter.points = {{1.0, 2.0, 1}, {2.0, 3.0, 2}, {100.0, 200.0, 3}};
        rlispstat::core::ComputeRanges(scatter);
        assert(scatter.dataXmax > 100.0);
        const auto visible = rlispstat::core::ExplicitAnalysisScope(
            "cars", {1, 2},
            rlispstat::core::AnalysisScopeSourceKind::CurrentSelection,
            "Included cases", 3);
        ApplyAnalysisScopeToPlot(scatter, visible, 3);
        assert(scatter.points.size() == 2);
        assert(scatter.dataXmax < 3.0 && scatter.xmax < 3.0);
        assert(scatter.dataYmax < 4.0 && scatter.ymax < 4.0);
    }
    {
        PlotModel scoped;
        scoped.group = "cars";
        scoped.points = {{1.0, 1.0, 1}, {2.0, 2.0, 2}, {3.0, 3.0, 3}};
        scoped.histogramPoints = {{1.0, 1, 0}, {2.0, 2, 0}, {3.0, 3, 0}};
        scoped.histogramBins = {{0.5, 3.5, {1, 2, 3}}};
        scoped.boxplotPoints = {{1.0, "A", 1}, {2.0, "A", 2}, {3.0, "B", 3}};
        rlispstat::core::BarplotBin bar;
        bar.category = "A";
        bar.rows = {1, 2, 3};
        bar.n = 3;
        bar.segments.push_back({"one", {1, 2}, 2});
        bar.segments.push_back({"two", {3}, 1});
        scoped.barplotBins.push_back(bar);
        rlispstat::core::BarplotBin excludedBar;
        excludedBar.category = "B";
        excludedBar.rows = {1};
        excludedBar.n = 1;
        excludedBar.segments.push_back({"one", {1}, 1});
        scoped.barplotBins.push_back(excludedBar);
        const auto scope = rlispstat::core::ExplicitAnalysisScope(
            "cars", {2, 3}, rlispstat::core::AnalysisScopeSourceKind::CurrentSelection,
            "Selected cases", 3);

        ApplyAnalysisScopeToPlot(scoped, scope, 3);
        assert(scoped.dataScopeCaptured);
        assert(scoped.points.size() == 2 && scoped.points.front().row == 2);
        assert(scoped.histogramPoints.size() == 2);
        assert((scoped.histogramBins.front().rows == std::vector<int>{2, 3}));
        assert(closeEnough(scoped.histogramBins.front().lower, 2.0));
        assert(closeEnough(scoped.histogramBins.front().upper, 3.0));
        assert(scoped.boxplotPoints.size() == 2);
        assert(scoped.barplotTotalN == 2);
        assert(scoped.barplotBins.size() == 1);
        assert(scoped.barplotBins.front().n == 2);
        assert(closeEnough(scoped.barplotBins.front().percent, 100.0));
        assert(closeEnough(scoped.barplotBins.front().widthValue, 2.0));
        assert(scoped.barplotBins.front().segments[0].count == 1);
        assert(scoped.barplotBins.front().segments[1].count == 1);
        assert(closeEnough(scoped.barplotBins.front().segments[0].barPercent, 100.0));
        assert(closeEnough(scoped.barplotBins.front().segments[0].barWidthValue, 2.0));
    }
    {
        PlotModel scopedHistogram;
        scopedHistogram.group = "cars";
        for (int row = 1; row <= 64; ++row) {
            scopedHistogram.histogramPoints.push_back(
                {static_cast<double>(row), row, 0});
        }
        rlispstat::core::RebinHistogramByRule(scopedHistogram, "sturges");
        const auto scope = rlispstat::core::ExplicitAnalysisScope(
            "cars", {50, 51, 52, 53},
            rlispstat::core::AnalysisScopeSourceKind::CurrentSelection,
            "Selected cases", 64);

        ApplyAnalysisScopeToPlot(scopedHistogram, scope, 64);
        assert(scopedHistogram.histogramBins.size() == 3);
        assert(closeEnough(scopedHistogram.histogramBins.front().lower, 50.0));
        assert(closeEnough(scopedHistogram.histogramBins.back().upper, 53.0));
    }
    {
        PlotModel effectDefaults;
        assert(effectDefaults.regressionConnectEstimates);
        assert(!effectDefaults.regressionConfidenceIntervalsVisible);
    }
    {
        std::map<int, std::string> linked = {
            {1, "blue"}, {2, "red"}, {4, "black"}};
        const std::map<int, std::string> oldMapping = {
            {1, "blue"}, {2, "blue"}};
        const auto cleared = SynchronizePlotColorByLinkedRows(
            linked, oldMapping, {});
        assert((cleared == std::vector<int>{1}));
        assert(linked.count(1) == 0);
        assert(linked.at(2) == "red");
        assert(linked.at(4) == "black");

        const std::map<int, std::string> newMapping = {
            {1, "orange"}, {2, "green"}, {3, "orange"}};
        const auto recolored = SynchronizePlotColorByLinkedRows(
            linked, {}, newMapping);
        assert((recolored == std::vector<int>{1, 2, 3}));
        assert(linked.at(1) == "orange");
        assert(linked.at(2) == "green");
        assert(linked.at(3) == "orange");
        assert(linked.at(4) == "black");

        PlotModel categorical;
        categorical.colorByVariable = "group";
        categorical.colorByLegendItems = {{"A", "orange"}, {"B", "green"}};
        categorical.colorByRowColors = newMapping;
        assert(PlotColorLegendMatchesLinkedRowColors(categorical, linked));
        linked[2] = "purple";
        assert(!PlotColorLegendMatchesLinkedRowColors(categorical, linked));
        linked[99] = "red";
        assert(!PlotColorLegendMatchesLinkedRowColors(categorical, linked));
        linked[2] = "green";
        assert(PlotColorLegendMatchesLinkedRowColors(categorical, linked));
    }
    DataViewport viewport{0.0, 10.0, -5.0, 5.0};
    Rect rect{100.0, 50.0, 400.0, 300.0};

    DataViewport fitted = DataViewportForPoints({
        Point{1.0, 10.0},
        Point{5.0, 20.0},
        Point{NAN, 100.0}
    });
    assert(closeEnough(fitted.xmin, 0.8));
    assert(closeEnough(fitted.xmax, 5.2));
    assert(closeEnough(fitted.ymin, 9.5));
    assert(closeEnough(fitted.ymax, 20.5));

    DataViewport constant = DataViewportForPoints({Point{2.0, 3.0}});
    assert(closeEnough(constant.xmin, 1.45));
    assert(closeEnough(constant.xmax, 2.55));
    assert(closeEnough(constant.ymin, 2.45));
    assert(closeEnough(constant.ymax, 3.55));

    DataViewport empty = DataViewportForPoints({});
    assert(closeEnough(empty.xmin, 0.0));
    assert(closeEnough(empty.xmax, 1.0));
    assert(closeEnough(empty.ymin, 0.0));
    assert(closeEnough(empty.ymax, 1.0));

    Point screen = DataToScreen(Point{5.0, 0.0}, viewport, rect);
    assert(closeEnough(screen.x, 300.0));
    assert(closeEnough(screen.y, 200.0));

    Point data = ScreenToData(screen, viewport, rect);
    assert(closeEnough(data.x, 5.0));
    assert(closeEnough(data.y, 0.0));

    Point lowerLeft = DataToScreen(Point{0.0, -5.0}, viewport, rect);
    assert(closeEnough(lowerLeft.x, 100.0));
    assert(closeEnough(lowerLeft.y, 350.0));

    Point upperRight = DataToScreen(Point{10.0, 5.0}, viewport, rect);
    assert(closeEnough(upperRight.x, 500.0));
    assert(closeEnough(upperRight.y, 50.0));

    Point upward = DataToScreen(Point{0.0, -5.0}, viewport, rect, false);
    assert(closeEnough(upward.y, 50.0));

    assert(PointInRect(Point{100.0, 50.0}, rect));
    assert(PointInRect(Point{500.0, 350.0}, rect));
    assert(!PointInRect(Point{99.0, 50.0}, rect));
    Rect expanded = ExpandRect(Rect{10.0, 20.0, 30.0, 40.0}, 3.0, 2.0);
    assert(closeEnough(expanded.x, 7.0));
    assert(closeEnough(expanded.y, 18.0));
    assert(closeEnough(expanded.width, 36.0));
    assert(closeEnough(expanded.height, 44.0));
    Rect gestureRect = RectBetweenPoints(Point{30.0, 50.0}, Point{10.0, 20.0});
    assert(closeEnough(gestureRect.x, 10.0));
    assert(closeEnough(gestureRect.y, 20.0));
    assert(closeEnough(gestureRect.width, 20.0));
    assert(closeEnough(gestureRect.height, 30.0));
    Rect fixedBrush = BrushRectForGesture(Point{11.0, 11.0}, Point{12.0, 13.0},
                                          true, 80.0, 60.0);
    assert(closeEnough(fixedBrush.x, -28.0));
    assert(closeEnough(fixedBrush.y, -17.0));
    assert(closeEnough(fixedBrush.width, 80.0));
    assert(closeEnough(fixedBrush.height, 60.0));
    Rect dragBrush = BrushRectForGesture(Point{10.0, 20.0}, Point{20.0, 26.0},
                                         true, 80.0, 60.0);
    assert(closeEnough(dragBrush.x, -20.0));
    assert(closeEnough(dragBrush.y, -4.0));
    assert(closeEnough(dragBrush.width, 80.0));
    assert(closeEnough(dragBrush.height, 60.0));
    assert(RectMeetsMinimumSize(dragBrush, 3.0, 3.0));
    assert(PlotInteractionModeAllowsRowSelection("select"));
    assert(PlotInteractionModeAllowsRowSelection("brush"));
    assert(!PlotInteractionModeAllowsRowSelection("none"));
    assert(!PlotInteractionModeAllowsRowSelection("identify"));

    PlotModel variableSource;
    variableSource.variableMeta = {{"mpg", "numeric"}, {"wt", "numeric"}, {"cyl", "factor"}};
    DataFrameModel variableFrame;
    DataColumn frameWt;
    frameWt.name = "wt";
    frameWt.type = "numeric";
    frameWt.values = {"2.6"};
    DataColumn frameCyl;
    frameCyl.name = "cyl";
    frameCyl.type = "factor";
    frameCyl.values = {"4"};
    variableFrame.columns = {frameWt, frameCyl};
    std::vector<std::string> availableNames = AvailableVariableNames(variableSource, &variableFrame);
    assert(availableNames.size() == 3);
    assert(availableNames[0] == "mpg");
    assert(availableNames[1] == "wt");
    assert(availableNames[2] == "cyl");
    assert(AvailableVariableExists(variableSource, "mpg", &variableFrame));
    assert(AvailableVariableExists(variableSource, "cyl", &variableFrame));
    assert(!AvailableVariableExists(variableSource, "missing", &variableFrame));
    assert(DefaultTermType(variableSource, "mpg") == "numeric");
    assert(DefaultTermType(variableSource, "cyl") == "factor");
    assert(DefaultTermType(static_cast<const PlotModel *>(nullptr), "mpg") == "numeric");
    std::map<std::string, std::string> termOverrides{{"mpg", "factor"}, {"wt", "numeric"}};
    assert(ModelTermDisplayType(variableSource, "mpg", termOverrides) == "factor");
    assert(ModelTermDisplayType(variableSource, "wt:cyl", termOverrides) == "numeric_factor_interaction");
    assert(ModelTermDisplayType(variableSource, "mpg:wt:cyl", termOverrides) == "higher_order_interaction");
    assert(ModelTermDisplayType(static_cast<const PlotModel *>(nullptr), "wt:cyl", termOverrides) == "wt:cyl");
    variableSource.variables = {{"mpg", {1.0}}, {"wt", {2.0}}};
    assert(NumericVariableNames(&variableSource).size() == 2);
    assert(NumericVariableNames(static_cast<const PlotModel *>(nullptr)).empty());
    assert(!RectMeetsMinimumSize(Rect{0.0, 0.0, 2.0, 4.0}, 3.0, 3.0));

    assert(IsValidViewport(viewport));
    Point invalid = DataToScreen(Point{1.0, 1.0}, DataViewport{0.0, 0.0, 0.0, 1.0}, rect);
    assert(std::isnan(invalid.x));
    assert(std::isnan(invalid.y));

    assert(closeEnough(DistanceSquaredToSegment(Point{5.0, 2.0}, Point{0.0, 0.0}, Point{10.0, 0.0}), 4.0));
    assert(closeEnough(DistanceSquaredToSegment(Point{12.0, 0.0}, Point{0.0, 0.0}, Point{10.0, 0.0}), 4.0));

    std::vector<IndexedScreenPoint> indexedPoints = {
        IndexedScreenPoint{1, Point{100.0, 100.0}},
        IndexedScreenPoint{2, Point{130.0, 100.0}},
        IndexedScreenPoint{3, Point{160.0, 100.0}}
    };
    assert(HitNearestIndexedPoint(Point{132.0, 101.0}, indexedPoints, 8.0) == 2);
    assert(HitNearestIndexedPoint(Point{145.0, 100.0}, indexedPoints, 8.0) == 0);

    std::vector<LabeledScreenSegment> segments = {
        LabeledScreenSegment{"x", Point{0.0, 0.0}, Point{100.0, 0.0}, Rect{}, false},
        LabeledScreenSegment{"y", Point{0.0, 0.0}, Point{0.0, 100.0}, Rect{8.0, 92.0, 20.0, 14.0}, true}
    };
    assert(HitNearestLabeledSegment(Point{50.0, 4.0}, segments, 8.0) == "x");
    assert(HitNearestLabeledSegment(Point{12.0, 98.0}, segments, 8.0) == "y");
    assert(HitNearestLabeledSegment(Point{60.0, 60.0}, segments, 8.0).empty());

    PlotModel plot;
    plot.title = "Observed vs fitted";
    plot.group = "cars";
    plot.xLabel = "fitted";
    plot.yLabel = "observed";
    plot.id = "plot-1";
    plot.selectionMode = "replace";
    plot.interactionMode = "select";
    plot.xmin = 1.0;
    plot.xmax = 5.0;
    plot.ymin = 10.0;
    plot.ymax = 30.0;
    plot.points = {rlispstat::core::DataPoint{1.0, 10.0, 1},
                   rlispstat::core::DataPoint{2.0, 20.0, 2}};
    plot.variables = {rlispstat::core::NumericVariable{"mpg", {1.0, 2.0}},
                      rlispstat::core::NumericVariable{"wt", {3.0, 4.0}}};
    plot.overlays = {rlispstat::core::Overlay{7, "lm", "all", true},
                     rlispstat::core::Overlay{8, "lm", "selected", false}};
    plot.glmModelId = "glm-1";
    plot.glmDiagnosticKind = "observed_fitted";
    plot.displayedFitVersion = 3;
    plot.displayedDiagnosticsVersion = 4;

    DataFrameModel seedData;
    seedData.group = "cars";
    seedData.rows = 3;
    seedData.columns = {
        DataColumn{"mpg", "numeric", "", "", -1, {"21", "22", "bad"}, {}, {}, {}, {}, {}, {}},
        DataColumn{"wt", "numeric", "", "", -1, {"2.62", "2.87", "3.21"}, {}, {}, {}, {}, {}, {}},
        DataColumn{"name", "text", "", "", -1, {"A", "B", "C"}, {}, {}, {}, {}, {}, {}}
    };
    PlotModel synced;
    synced.variables = {rlispstat::core::NumericVariable{"old", {1.0}}};
    synced.variableMeta = {rlispstat::core::VariableMeta{"old", "numeric"}};
    SyncPlotVariablesFromDataFrame(synced, seedData);
    assert(synced.variableMeta.size() == 3);
    assert(synced.variables.size() == 2);
    assert(synced.variables[0].name == "mpg");
    assert(std::isnan(synced.variables[0].values[2]));
    synced.isDatasetSeed = true;
    synced.xLabel = "old_x";
    synced.yLabel = "old_y";
    RefreshDatasetSeedPlotAfterVariableSync(synced);
    assert(synced.xLabel == "mpg");
    assert(synced.yLabel == "wt");
    assert(synced.points.size() == 2);

    PlotModel seed;
    PopulateDatasetSeedPlot(seed, seedData, "cars");
    assert(seed.isDatasetSeed);
    assert(seed.id == "dataset_seed_cars");
    assert(seed.group == "cars");
    assert(seed.xLabel == "mpg");
    assert(seed.yLabel == "wt");
    assert(seed.title == "wt vs mpg");
    assert(seed.variableMeta.size() == 3);
    assert(seed.variables.size() == 2);
    assert(seed.points.size() == 2);
    assert(seed.points[0].row == 1);
    assert(closeEnough(seed.points[0].x, 21.0));
    assert(closeEnough(seed.points[0].y, 2.62));
    assert(seed.points[1].row == 2);

    PlotModel comparisonSeed;
    assert(BuildComparisonSeedPlot(seedData, "cars", comparisonSeed));
    assert(!comparisonSeed.isDatasetSeed);
    assert(comparisonSeed.kind == "comparison-seed");
    assert(comparisonSeed.id == "seed_cars");
    assert(comparisonSeed.group == "cars");
    assert(comparisonSeed.xLabel == "wt");
    assert(comparisonSeed.yLabel == "mpg");
    assert(comparisonSeed.points.size() == 2);
    assert(closeEnough(comparisonSeed.points[0].x, 2.62));
    assert(closeEnough(comparisonSeed.points[0].y, 21.0));

    // Analysis-derived scatterplots must be able to display ordinal scale
    // items whose canonical worksheet values are numeric scores.  Populating
    // a normal dataset seed deliberately excludes them from the ordinary
    // numeric-variable list, so this exercises the Scale Analysis path that
    // previously made "Open Scatterplot" return silently.
    DataFrameModel ordinalScaleData;
    ordinalScaleData.group = "scale-items";
    ordinalScaleData.rows = 4;
    ordinalScaleData.columns = {
        DataColumn{"q1", "ordinal", "", "", -1, {"0", "1", "0", "1"}, {}, {}, {}, {}, {}, {}},
        DataColumn{"q2", "ordinal", "", "", -1, {"1", "1", "0", "1"}, {}, {}, {}, {}, {}, {}}
    };
    PlotModel ordinalScatter;
    PopulateDatasetSeedPlot(ordinalScatter, ordinalScaleData, "scale-items");
    assert(ordinalScatter.variables.empty());
    std::string scatterError;
    assert(PrepareScatterplotVariablesFromDataFrame(
        ordinalScatter, ordinalScaleData, "q2", "q1", &scatterError));
    assert(scatterError.empty());
    assert(ordinalScatter.xLabel == "q2");
    assert(ordinalScatter.yLabel == "q1");
    assert(ordinalScatter.variables.size() == 2);
    assert(ordinalScatter.points.size() == 4);
    assert(ordinalScatter.variableMeta.size() == 2);
    assert(ordinalScatter.variableMeta[0].type == "ordinal");
    assert(ordinalScatter.variableMeta[1].type == "ordinal");

    ordinalScaleData.columns[1].values = {"No", "Yes", "No", "Yes"};
    assert(!PrepareScatterplotVariablesFromDataFrame(
        ordinalScatter, ordinalScaleData, "q2", "q1", &scatterError));
    assert(!scatterError.empty());

    assert(TitleForPlot(plot) == "Observed vs fitted [cars]");
    assert(VariableRole(plot, "mpg", "mpg", {"wt"}) == "Y");
    assert(VariableRole(plot, "wt", "mpg", {"wt"}) == "Predictor");
    assert(VariableRole(plot, "fitted", "", {}) == "X");
    assert(VariableRole(plot, "observed", "", {}) == "Y");
    assert(PlotLinksToDataRows(plot));
    using rlispstat::core::TrellisPlotType;
    PlotModel labelCapabilities;
    for (const auto &kind : {"scatter", "scatter_matrix", "histogram"}) {
        labelCapabilities.kind = kind;
        assert(rlispstat::core::PlotSupportsObservationLabels(labelCapabilities));
    }
    for (const auto &kind : {"glm_interaction", "pca_scree", "boxplot", "barplot"}) {
        labelCapabilities.kind = kind;
        assert(!rlispstat::core::PlotSupportsObservationLabels(labelCapabilities));
    }
    labelCapabilities.kind = "trellis_scatterplot";
    labelCapabilities.trellisSpecificationInitialized = true;
    labelCapabilities.trellisSpecification.plotType = TrellisPlotType::Scatter;
    assert(rlispstat::core::PlotSupportsObservationLabels(labelCapabilities));
    for (const auto type : {TrellisPlotType::Histogram, TrellisPlotType::Bar,
                           TrellisPlotType::Boxplot, TrellisPlotType::TimeSeries,
                           TrellisPlotType::DataTable}) {
        labelCapabilities.trellisSpecification.plotType = type;
        assert(!rlispstat::core::PlotSupportsObservationLabels(labelCapabilities));
    }

    PlotModel aggregateDiagnostic = plot;
    aggregateDiagnostic.kind = "glm_interaction";
    for (const auto &kind : {"boundary_zero_fit", "observed_predicted_score_distribution"}) {
        aggregateDiagnostic.glmDiagnosticKind = kind;
        assert(rlispstat::core::PlotIsAggregateDiagnostic(aggregateDiagnostic));
        assert(!PlotLinksToDataRows(aggregateDiagnostic));
    }
    aggregateDiagnostic.glmDiagnosticKind.clear();
    assert(PlotLinksToDataRows(aggregateDiagnostic));
    assert(!rlispstat::core::PlotHasConfidenceIntervals(aggregateDiagnostic));
    assert(PlotListItemText(plot) == "plot-1|cars|fitted|observed|2|replace|select");
    assert(PlotInfoResponseText(plot, 1) ==
           "plot-1|cars|fitted|observed|2|1|1|5|10|30|replace|select|Observed vs fitted");
    assert(PlotVariablesResponseText(plot) == "OK\tmpg\twt");
    assert(PlotOverlaysResponseText(plot) == "OK\t7|lm|all|TRUE\t8|lm|selected|FALSE");
    assert(PlotDiagnosticInfoResponseText(plot) ==
           "OK\tDIAGNOSTIC|plot-1|cars|glm-1|observed_fitted|3|4|2|1,1.000000,10.000000|2,2.000000,20.000000");

    std::vector<SmoothCurveData> smoothCurves;
    assert(closeEnough(ClampSmoothSpan(NAN), 0.75));
    assert(closeEnough(ClampSmoothSpan(0.05), 0.20));
    assert(closeEnough(ClampSmoothSpan(0.80), 0.80));
    assert(closeEnough(ClampSmoothSpan(4.00), 2.00));
    assert(closeEnough(ClampPointSizeScale(NAN), 1.00));
    assert(closeEnough(ClampPointSizeScale(0.10), 0.50));
    assert(closeEnough(ClampPointSizeScale(1.40), 1.40));
    assert(closeEnough(ClampPointSizeScale(8.00), 3.00));
    assert(ToggleSmoothCurveScopePending(smoothCurves, SmoothCurveScope::Overall));
    assert(SmoothCurveScopeIsPresent(smoothCurves, SmoothCurveScope::Overall));
    assert(smoothCurves.size() == 1);
    assert(!smoothCurves[0].ok);
    assert(smoothCurves[0].message == "needed");
    assert(ToggleSmoothCurveScopePending(smoothCurves, SmoothCurveScope::Overall));
    assert(smoothCurves.empty());

    smoothCurves.push_back(SmoothCurveData{SmoothCurveScope::Overall, "all", {1.0}, {2.0}, true, ""});
    smoothCurves.push_back(SmoothCurveData{SmoothCurveScope::Selection, "selected", {1.0}, {3.0}, true, ""});
    assert(MarkSmoothCurveScopePendingIfPresent(smoothCurves, SmoothCurveScope::Selection));
    assert(smoothCurves.size() == 3);
    assert(smoothCurves[1].ok); // last valid selection curve stays drawable
    assert(smoothCurves[2].scope == SmoothCurveScope::Selection);
    assert(smoothCurves[2].groupId == ".");
    assert(smoothCurves[2].message == "needed");
    assert(!MarkSmoothCurveScopePendingIfPresent(smoothCurves, SmoothCurveScope::ColorGroup));
    smoothCurves.push_back(EnabledEmptySmoothCurve(SmoothCurveScope::ColorGroup));
    assert(SmoothCurveScopeIsPresent(smoothCurves, SmoothCurveScope::ColorGroup));
    assert(smoothCurves.back().message.empty());
    assert(MarkSmoothCurveScopePendingIfPresent(smoothCurves, SmoothCurveScope::ColorGroup));
    assert(smoothCurves.back().message == "needed");
    assert(MarkExistingSmoothCurvesPending(smoothCurves));
    assert(smoothCurves.size() == 5);
    assert(smoothCurves[0].scope == SmoothCurveScope::Overall);
    assert(smoothCurves[1].scope == SmoothCurveScope::Selection);
    assert(smoothCurves[0].ok && smoothCurves[1].ok);
    assert(smoothCurves[2].message == "needed");
    assert(smoothCurves[3].message == "needed");

    std::vector<SmoothCurveData> coordinateCurves{
        SmoothCurveData{SmoothCurveScope::Overall, "all", {1.0, 2.0}, {3.0, 4.0}, true, ""},
        SmoothCurveData{SmoothCurveScope::ColorGroup, "#0078D4", {1.0, 2.0}, {5.0, 6.0}, true, ""},
        EnabledEmptySmoothCurve(SmoothCurveScope::Selection)
    };
    assert(InvalidateSmoothCurvesForCoordinateChange(coordinateCurves));
    assert(coordinateCurves.size() == 3);
    assert(std::all_of(coordinateCurves.begin(), coordinateCurves.end(),
                       [](const SmoothCurveData &curve) {
                           return !curve.ok && curve.message == "needed" &&
                                  curve.x.empty() && curve.y.empty();
                       }));
    assert(SmoothCurveScopeIsPresent(coordinateCurves, SmoothCurveScope::Overall));
    assert(SmoothCurveScopeIsPresent(coordinateCurves, SmoothCurveScope::Selection));
    assert(SmoothCurveScopeIsPresent(coordinateCurves, SmoothCurveScope::ColorGroup));

    const Rect clip{10.0, 20.0, 100.0, 80.0};
    for (const std::pair<Point, Point> &source : std::vector<std::pair<Point, Point>>{
             {{-100.0, 40.0}, {300.0, 90.0}},
             {{30.0, 200.0}, {90.0, -100.0}},
             {{60.0, -500.0}, {61.0, 500.0}},
             {{-20.0, 60.0}, {140.0, 60.0}}}) {
        Point start = source.first;
        Point end = source.second;
        assert(ClipLineToRect(start, end, clip));
        assert(PointInRect(start, clip));
        assert(PointInRect(end, clip));
    }
    Point outsideStart{-30.0, -20.0};
    Point outsideEnd{-10.0, -5.0};
    assert(!ClipLineToRect(outsideStart, outsideEnd, clip));

    std::vector<DataPoint> timePoints{
        DataPoint{3.0, 30.0, 3}, DataPoint{1.0, 10.0, 1},
        DataPoint{2.0, 22.0, 4}, DataPoint{1.0, 20.0, 2}
    };
    std::vector<InteractionPlotLine> timeLines =
        BuildTimeSeriesLines(timePoints, {"A", "A", "B", "B"}, "value");
    assert(timeLines.size() == 2);
    assert(timeLines[0].label == "A" && timeLines[1].label == "B");
    assert(timeLines[0].points[0].x == 1.0 && timeLines[0].points[1].x == 3.0);
    assert(timeLines[1].points[0].row == 2 && timeLines[1].points[1].row == 4);
    PlotModel legendPlot;
    legendPlot.interactionPlotLines = timeLines;
    assert((PlotSeriesLegendRows(legendPlot, 0) == std::set<int>{1, 3}));
    assert((PlotSeriesLegendRows(legendPlot, 1) == std::set<int>{2, 4}));
    assert(PlotSeriesIntersectsSelection(legendPlot, 0, {1}));
    assert(!PlotSeriesIntersectsSelection(legendPlot, 1, {1}));
    assert(!PlotSeriesIsFullySelected(legendPlot, 0, {1}));
    assert(PlotSeriesIsFullySelected(legendPlot, 0, {1, 3}));
    assert(!PlotSeriesIsFullySelected(legendPlot, 1, {1, 3}));
    legendPlot.kind = "time_series";
    const std::map<int, std::string> timeSeriesColors =
        TimeSeriesRowColors(legendPlot);
    assert(timeSeriesColors.at(1) == timeLines[0].colorKey);
    assert(timeSeriesColors.at(3) == timeLines[0].colorKey);
    assert(timeSeriesColors.at(2) == timeLines[1].colorKey);
    assert(timeSeriesColors.at(4) == timeLines[1].colorKey);
    const DataViewport seriesViewport{0.5, 3.5, 5.0, 35.0};
    const Rect seriesRect{10.0, 20.0, 300.0, 300.0};
    const Point firstSeriesPoint = DataToScreen(
        {2.0, 20.0}, seriesViewport, seriesRect, true);
    const Point secondSeriesPoint = DataToScreen(
        {2.0, 22.0}, seriesViewport, seriesRect, true);
    assert(HitPlotSeriesAtScreenPoint(
        legendPlot, seriesViewport, seriesRect, firstSeriesPoint, 6.0) == 0);
    assert(HitPlotSeriesAtScreenPoint(
        legendPlot, seriesViewport, seriesRect, secondSeriesPoint, 6.0) == 1);
    assert(!HitPlotSeriesAtScreenPoint(
        legendPlot, seriesViewport, seriesRect, {500.0, 500.0}, 6.0));
    PlotModel denseSeries;
    denseSeries.kind = "time_series";
    denseSeries.group = "cars";
    denseSeries.yLabel = "value";
    denseSeries.points = {{0.1, 0.5, 1}, {0.2, 0.5, 2}};
    denseSeries.interactionPlotLines = BuildTimeSeriesLines(
        denseSeries.points, {}, denseSeries.yLabel);
    const DataViewport denseViewport{0.0, 1.0, 0.0, 1.0};
    const Rect denseRect{0.0, 0.0, 100.0, 100.0};
    assert(HitTimeSeriesSegmentAtScreenPoint(
        denseSeries, denseViewport, denseRect, {15.0, 50.0}, 4.0) == 0);
    assert(!HitTimeSeriesSegmentAtScreenPoint(
        denseSeries, denseViewport, denseRect, {10.0, 50.0}, 4.0));
    const auto includedSeriesCase = rlispstat::core::ExplicitAnalysisScope(
        "cars", {1}, rlispstat::core::AnalysisScopeSourceKind::CurrentSelection,
        "Included cases", 2);
    ApplyAnalysisScopeToPlot(denseSeries, includedSeriesCase, 2);
    assert(denseSeries.points.size() == 1);
    assert(denseSeries.interactionPlotLines.size() == 1);
    assert((PlotSeriesLegendRows(denseSeries, 0) == std::set<int>{1}));
    assert(timeLines[0].colorKey != timeLines[1].colorKey);

    PlotModel categoricalEffect;
    categoricalEffect.kind = "glm_interaction";
    categoricalEffect.interactionXTicks = {{1.0, "1"}, {2.0, "2"}};
    categoricalEffect.interactionXTickRows = {{1, 3}, {2, 4}};
    // Tick metadata comes from the fitted model, so this remains categorical
    // even when its factor labels happen to be numeric strings.
    assert(RegressionEffectXAxisIsCategorical(categoricalEffect));
    assert(!IsPooledRegressionEffectPlot(categoricalEffect));
    categoricalEffect.isRegressionDerivedPlot = true;
    categoricalEffect.regressionDerivedKind = "interaction_plot";
    assert(IsPooledRegressionEffectPlot(categoricalEffect));
    categoricalEffect.regressionDerivedKind = "effect_plot";
    assert(IsPooledRegressionEffectPlot(categoricalEffect));
    categoricalEffect.regressionDerivedKind = "partial_regression_plot";
    assert(!IsPooledRegressionEffectPlot(categoricalEffect));
    categoricalEffect.regressionDerivedKind = "interaction_plot";
    assert((PlotEffectCategoryRows(categoricalEffect, 0) ==
            std::set<int>{1, 3}));
    assert((PlotEffectCategoryRows(categoricalEffect, 1) ==
            std::set<int>{2, 4}));
    assert(PlotEffectCategoryRows(categoricalEffect, 2).empty());
    categoricalEffect.regressionConnectEstimates = false;
    assert(!categoricalEffect.regressionConnectEstimates);
    const double firstDodge = RegressionEffectSeriesDodgeOffset(0, 2);
    const double secondDodge = RegressionEffectSeriesDodgeOffset(1, 2);
    assert(firstDodge < 0.0 && secondDodge > 0.0);
    assert(std::abs(firstDodge + secondDodge) < 1e-12);
    categoricalEffect.interactionXTicks.clear();
    assert(!RegressionEffectXAxisIsCategorical(categoricalEffect));
    assert(RegressionEffectSeriesDodgeOffset(0, 1) == 0.0);

    PlotModel effectRange;
    effectRange.kind = "glm_interaction";
    effectRange.regressionBinaryProbability = true;
    effectRange.regressionEffectQuantity = "probability_difference";
    effectRange.points = {{1.0, 4.0, 0}, {1.0, -9.0, 0}};
    effectRange.interactionPlotLines = {
        {"Finland", "orange", {{1.0, 4.0, 0}}},
        {"Italy", "green", {{1.0, -9.0, 0}}}
    };
    assert(!rlispstat::core::PlotHasConfidenceIntervals(effectRange));
    effectRange.interactionPlotLines[0].confidenceLower = {{1.0, -22.0, 0}};
    effectRange.interactionPlotLines[0].confidenceUpper = {{1.0, 26.0, 0}};
    effectRange.interactionPlotLines[1].confidenceLower = {{1.0, -30.0, 0}};
    effectRange.interactionPlotLines[1].confidenceUpper = {{1.0, 18.0, 0}};
    assert(rlispstat::core::PlotHasConfidenceIntervals(effectRange));
    effectRange.regressionConfidenceIntervalsVisible = false;
    rlispstat::core::ComputeRanges(effectRange);
    const double pointOnlySpan = effectRange.ymax - effectRange.ymin;
    assert(effectRange.ymin > -20.0);
    assert(effectRange.ymax < 15.0);
    effectRange.regressionConfidenceIntervalsVisible = true;
    rlispstat::core::ComputeRanges(effectRange);
    const double intervalSpan = effectRange.ymax - effectRange.ymin;
    assert(effectRange.ymin <= -30.0);
    assert(effectRange.ymax >= 26.0);
    assert(intervalSpan > pointOnlySpan);

    DataFrameModel timeData;
    timeData.group = "series";
    timeData.rows = 4;
    timeData.columns.push_back(DataColumn{"time", "numeric", "", "", -1, {"3", "1", "2", "1"}});
    timeData.columns.push_back(DataColumn{"value", "numeric", "", "", -1, {"30", "10", "22", "20"}});
    timeData.columns.push_back(DataColumn{"id", "factor", "", "", -1, {"A", "A", "B", "B"}});
    PlotModel timePlot;
    timePlot.kind = "time_series";
    timePlot.xLabel = "time";
    timePlot.yLabel = "value";
    timePlot.timeSeriesGroupVariable = "id";
    std::string timeError;
    assert(RebuildTimeSeriesFromDataFrame(timePlot, timeData, &timeError));
    assert(timePlot.points.size() == 4);
    assert(timePlot.interactionPlotLines.size() == 2);
    assert(timePlot.interactionPlotLines[0].points[0].row == 2);
    assert(FormatTimeSeriesTick(0.0, "date") == "1970-01-01");
    assert(FormatTimeSeriesTick(86400.0, "datetime") == "1970-01-02 00:00");
    DataFrameModel dateData;
    dateData.rows = 2;
    dateData.columns.push_back(DataColumn{"day", "datetime", "", "", -1, {"1970-01-01", "1970-01-03"}});
    dateData.columns.push_back(DataColumn{"value", "numeric", "", "", -1, {"2", "4"}});
    PlotModel datePlot;
    datePlot.kind = "time_series";
    datePlot.xLabel = "day";
    datePlot.yLabel = "value";
    datePlot.timeSeriesTimeType = "date";
    assert(RebuildTimeSeriesFromDataFrame(datePlot, dateData));
    assert(datePlot.points[0].x == 0.0 && datePlot.points[1].x == 2.0);

    PlotModel histogramDiagnostic;
    histogramDiagnostic.kind = "histogram";
    histogramDiagnostic.id = "hist-1";
    histogramDiagnostic.group = "cars";
    histogramDiagnostic.glmModelId = "glm-2";
    histogramDiagnostic.glmDiagnosticKind = "residuals";
    histogramDiagnostic.displayedFitVersion = 5;
    histogramDiagnostic.displayedDiagnosticsVersion = 6;
    histogramDiagnostic.histogramPoints = {
        rlispstat::core::HistogramPoint{1.25, 4, 2},
        rlispstat::core::HistogramPoint{2.5, 7, 3}
    };
    assert(PlotDiagnosticInfoResponseText(histogramDiagnostic) ==
           "OK\tDIAGNOSTIC|hist-1|cars|glm-2|residuals|5|6|2|4,1.250000,bin2|7,2.500000,bin3");
    plot.kind = "pca_scree";
    assert(!PlotLinksToDataRows(plot));

    PlotModel liveExploratory;
    liveExploratory.kind = "scatter";
    assert(rlispstat::core::PlotSupportsCaseExclusion(liveExploratory));
    assert(rlispstat::core::PlotReceivesGlobalAnalysisScopeUpdates(
        liveExploratory));
    liveExploratory.analysisScopeFrozen = true;
    assert(!rlispstat::core::PlotReceivesGlobalAnalysisScopeUpdates(
        liveExploratory));
    PlotModel derivedDiagnostic;
    derivedDiagnostic.kind = "histogram";
    derivedDiagnostic.isGLMDiagnostic = true;
    assert(rlispstat::core::PlotIsDerivedAnalysisView(derivedDiagnostic));
    assert(rlispstat::core::PlotSupportsCaseExclusion(derivedDiagnostic));
    assert(!rlispstat::core::PlotReceivesGlobalAnalysisScopeUpdates(
        derivedDiagnostic));
    derivedDiagnostic.glmDiagnosticKind = "boundary_zero_fit";
    assert(!rlispstat::core::PlotSupportsCaseExclusion(derivedDiagnostic));
    derivedDiagnostic.glmDiagnosticKind = "residuals_vs_fitted";
    derivedDiagnostic.isDatasetSeed = true;
    assert(!rlispstat::core::PlotSupportsCaseExclusion(derivedDiagnostic));

    return 0;
}
