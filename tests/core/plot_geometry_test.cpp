#include "../../src/core/plot_geometry.h"
#include "../../src/core/dataset_model.h"

#include <cassert>
#include <cmath>
#include <map>

using rlispstat::core::DataToScreen;
using rlispstat::core::AvailableVariableExists;
using rlispstat::core::AvailableVariableNames;
using rlispstat::core::DataColumn;
using rlispstat::core::DataFrameModel;
using rlispstat::core::DataPoint;
using rlispstat::core::DataViewport;
using rlispstat::core::DataViewportForPoints;
using rlispstat::core::ClampSmoothSpan;
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
using rlispstat::core::IndexedScreenPoint;
using rlispstat::core::InteractionPlotLine;
using rlispstat::core::IsValidViewport;
using rlispstat::core::LabeledScreenSegment;
using rlispstat::core::PlotLinksToDataRows;
using rlispstat::core::PlotInfoResponseText;
using rlispstat::core::PlotListItemText;
using rlispstat::core::PlotDiagnosticInfoResponseText;
using rlispstat::core::PlotModel;
using rlispstat::core::PlotOverlaysResponseText;
using rlispstat::core::PopulateDatasetSeedPlot;
using rlispstat::core::PlotVariablesResponseText;
using rlispstat::core::Point;
using rlispstat::core::PointInRect;
using rlispstat::core::MarkExistingSmoothCurvesPending;
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
using rlispstat::core::TitleForPlot;
using rlispstat::core::ToggleSmoothCurveScopePending;
using rlispstat::core::VariableRole;

static bool closeEnough(double a, double b)
{
    return std::fabs(a - b) < 1.0e-9;
}

int main()
{
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
    assert(closeEnough(dragBrush.x, 10.0));
    assert(closeEnough(dragBrush.y, 20.0));
    assert(closeEnough(dragBrush.width, 10.0));
    assert(closeEnough(dragBrush.height, 6.0));
    assert(RectMeetsMinimumSize(dragBrush, 3.0, 3.0));

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

    assert(TitleForPlot(plot) == "Observed vs fitted [cars]");
    assert(VariableRole(plot, "mpg", "mpg", {"wt"}) == "Y");
    assert(VariableRole(plot, "wt", "mpg", {"wt"}) == "Predictor");
    assert(VariableRole(plot, "fitted", "", {}) == "X");
    assert(VariableRole(plot, "observed", "", {}) == "Y");
    assert(PlotLinksToDataRows(plot));
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
    assert(MarkExistingSmoothCurvesPending(smoothCurves));
    assert(smoothCurves.size() == 4);
    assert(smoothCurves[0].scope == SmoothCurveScope::Overall);
    assert(smoothCurves[1].scope == SmoothCurveScope::Selection);
    assert(smoothCurves[0].ok && smoothCurves[1].ok);
    assert(smoothCurves[2].message == "needed");
    assert(smoothCurves[3].message == "needed");

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
    assert(timeLines[0].colorKey != timeLines[1].colorKey);

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

    return 0;
}
