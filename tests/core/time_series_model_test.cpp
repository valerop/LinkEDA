#include "../../src/core/command_dispatcher.h"
#include "../../src/core/command_model.h"
#include "../../src/core/plot_geometry.h"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <set>
#include <string>
#include <vector>

int main()
{
    using namespace rlispstat::core;

    const auto options = LinkedPlotCommandOptions();
    assert(options.size() >= 2);
    assert(options[2].command == "PLOT_NEW_TIME_SERIES");

    PlotModel *created = nullptr;
    CommandDispatcherServices services;
    services.ui.addPlot = [&](PlotModel *plot) { created = plot; };
    CommandDispatcher dispatcher(services);
    assert(dispatcher.dispatch({
        "ADD_PLOT", "ts1", "sales", "year", "value", "Sales", "4",
        "2022 4 1 0", "2021 2 2 0", "2022 8 3 1", "2021 6 4 1",
        "TIME_SERIES", "numeric", "store", "2", "North", "South",
        "TIME_SERIES_OPTIONS", "start_labels", "bottom_left"
    }) == "OK");
    assert(created != nullptr);
    assert(created->kind == "time_series");
    assert(created->timeSeriesGroupVariable == "store");
    assert(created->interactionPlotLines.size() == 2);
    assert(created->interactionPlotLines[0].label == "North");
    assert(created->interactionPlotLines[0].points[0].row == 2);
    assert(created->interactionPlotLines[1].points[0].row == 4);
    assert(created->interactionPlotLines[0].colorKey != created->interactionPlotLines[1].colorKey);
    assert(created->timeSeriesIdentification == "start_labels");
    assert(created->timeSeriesLegendPosition == "bottom_left");

    const std::vector<TimeSeriesAxisTick> dateTicks =
        BuildTimeSeriesAxisTicks(-0.4, 10.4, "date", 6);
    assert(dateTicks.size() >= 5);
    std::set<std::string> dateLabels;
    for (const TimeSeriesAxisTick &tick : dateTicks) dateLabels.insert(tick.label);
    assert(dateLabels.size() == dateTicks.size());

    const std::vector<TimeSeriesAxisTick> secondTicks =
        BuildTimeSeriesAxisTicks(-2.0, 52.0, "datetime", 6);
    assert(secondTicks.size() >= 4);
    std::set<std::string> secondLabels;
    for (const TimeSeriesAxisTick &tick : secondTicks) secondLabels.insert(tick.label);
    assert(secondLabels.size() == secondTicks.size());
    assert(secondTicks.front().label.size() == std::string("1970-01-01 00:00:00").size());

    DataColumn years;
    years.name = "year";
    years.type = "numeric";
    years.values = {"2019", "2020", "2021", "2022"};
    assert(InferTimeSeriesTimeType(years) == "year");
    assert(FormatTimeSeriesTick(2020.0, "year") == "2020");
    const std::vector<TimeSeriesAxisTick> yearTicks =
        BuildTimeSeriesAxisTicks(2018.8, 2022.2, "year", 6);
    assert(!yearTicks.empty());
    for (const TimeSeriesAxisTick &tick : yearTicks) {
        assert(tick.label.find('e') == std::string::npos);
        assert(tick.label.find('E') == std::string::npos);
    }
    years.values = {"1", "2", "3"};
    assert(InferTimeSeriesTimeType(years) == "numeric");

    const Rect plotRect{10.0, 20.0, 400.0, 240.0};
    const auto topLeft = BuildTimeSeriesLegendLayout(2, plotRect, "top_left");
    const auto bottomRight = BuildTimeSeriesLegendLayout(2, plotRect, "bottom_right");
    assert(topLeft.size() == 2);
    assert(bottomRight.size() == 2);
    assert(topLeft.front().sampleStart.x < bottomRight.front().sampleStart.x);
    assert(topLeft.front().sampleStart.y < bottomRight.front().sampleStart.y);

    std::vector<InteractionPlotLine> closeLines(3);
    for (std::size_t index = 0; index < closeLines.size(); ++index) {
        closeLines[index].label = std::string("Series ") + std::to_string(index + 1);
        closeLines[index].colorKey = "black";
        closeLines[index].points.push_back(DataPoint{0.0, 5.0 + 0.01 * index, static_cast<int>(index + 1)});
        closeLines[index].points.push_back(DataPoint{10.0, 6.0 + index, static_cast<int>(index + 4)});
    }
    const auto directLabels = BuildTimeSeriesDirectLabels(
        closeLines, DataViewport{-1.0, 11.0, 0.0, 10.0}, plotRect, 14.0);
    assert(directLabels.size() == 3);
    std::vector<double> labelY;
    for (const TimeSeriesDirectLabel &label : directLabels) {
        labelY.push_back(label.labelAnchor.y);
        assert(label.leaderEnd.x > label.seriesAnchor.x);
    }
    std::sort(labelY.begin(), labelY.end());
    assert(labelY[1] - labelY[0] >= 13.99);
    assert(labelY[2] - labelY[1] >= 13.99);

    assert(ParseCommandRequest({"TIME_SERIES_SET_IDENTIFICATION", "ts1", "none"}).action ==
           CommandAction::SetTimeSeriesIdentification);
    assert(ParseCommandRequest({"TIME_SERIES_SET_LEGEND_POSITION", "ts1", "top_left"}).action ==
           CommandAction::SetTimeSeriesLegendPosition);

    return 0;
}
