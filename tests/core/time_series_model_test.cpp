#include "../../src/core/command_dispatcher.h"
#include "../../src/core/command_model.h"
#include "../../src/core/plot_geometry.h"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <fstream>
#include "../../src/core/dataset_protocol.h"
#include <set>
#include <string>
#include <vector>

int main(int argc, char **argv)
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
    const auto interactionRight = BuildTimeSeriesLegendLayout(2, plotRect, "right");
    const auto interactionLeft = BuildTimeSeriesLegendLayout(2, plotRect, "left");
    const auto interactionTop = BuildTimeSeriesLegendLayout(2, plotRect, "top");
    const auto interactionBottom = BuildTimeSeriesLegendLayout(2, plotRect, "bottom");
    assert(topLeft.size() == 2);
    assert(bottomRight.size() == 2);
    assert(interactionRight.size() == 2);
    assert(interactionLeft.size() == 2);
    assert(interactionTop.size() == 2);
    assert(interactionBottom.size() == 2);
    assert(topLeft.front().sampleStart.x < bottomRight.front().sampleStart.x);
    assert(topLeft.front().sampleStart.y < bottomRight.front().sampleStart.y);
    assert(interactionLeft.front().sampleStart.x < interactionRight.front().sampleStart.x);
    assert(interactionTop.front().sampleStart.y < interactionBottom.front().sampleStart.y);
    assert(interactionRight[1].sampleStart.y > interactionRight[0].sampleStart.y);
    assert(interactionTop[1].sampleStart.x > interactionTop[0].sampleStart.x);

    PlotModel interactionPlot;
    interactionPlot.kind = "glm_interaction";
    assert(interactionPlot.interactionLegendPosition == "right");
    assert(SetInteractionLegendPosition(interactionPlot, "left"));
    assert(interactionPlot.interactionLegendPosition == "left");
    assert(SetInteractionLegendPosition(interactionPlot, "top"));
    assert(interactionPlot.interactionLegendPosition == "top");
    assert(SetInteractionLegendPosition(interactionPlot, "bottom"));
    assert(interactionPlot.interactionLegendPosition == "bottom");
    assert(!SetInteractionLegendPosition(interactionPlot, "top_left"));
    assert(interactionPlot.interactionLegendPosition == "bottom");
    interactionPlot.interactionPlotLines.resize(2);
    assert(SetInteractionLegendCustomPosition(interactionPlot, 0.25, 0.40));
    assert(interactionPlot.interactionLegendUsesCustomPosition);
    const auto customInteraction = BuildInteractionLegendLayout(
        interactionPlot, plotRect);
    assert(customInteraction.size() == 2);
    assert(std::abs(customInteraction.front().sampleStart.x - 110.0) < 1e-9);
    assert(std::abs(customInteraction.front().sampleStart.y - 116.0) < 1e-9);
    assert(SetInteractionLegendPosition(interactionPlot, "right"));
    assert(!interactionPlot.interactionLegendUsesCustomPosition);
    assert(SetInteractionLegendPosition(interactionPlot, "outside_right"));
    auto outsideRect = RegressionEffectPlotRect(interactionPlot, 760.0, 520.0);
    assert(outsideRect.width < 760.0 - 128.0);
    auto outsideItems = BuildInteractionLegendLayout(interactionPlot, outsideRect);
    assert(outsideItems.front().sampleStart.x > outsideRect.x + outsideRect.width);
    assert(SetInteractionLegendPosition(interactionPlot, "outside_left"));
    outsideRect = RegressionEffectPlotRect(interactionPlot, 760.0, 520.0);
    outsideItems = BuildInteractionLegendLayout(interactionPlot, outsideRect);
    assert(outsideItems.front().sampleStart.x < outsideRect.x);
    assert(SetInteractionLegendPosition(interactionPlot, "outside_top"));
    outsideRect = RegressionEffectPlotRect(interactionPlot, 760.0, 520.0);
    outsideItems = BuildInteractionLegendLayout(interactionPlot, outsideRect);
    assert(outsideItems.front().sampleStart.y < outsideRect.y);
    assert(SetInteractionLegendPosition(interactionPlot, "outside_bottom"));
    outsideRect = RegressionEffectPlotRect(interactionPlot, 760.0, 520.0);
    outsideItems = BuildInteractionLegendLayout(interactionPlot, outsideRect);
    assert(outsideItems.front().sampleStart.y > outsideRect.y + outsideRect.height);
    assert(SetInteractionLegendPosition(interactionPlot, "manual"));
    assert(interactionPlot.interactionLegendUsesCustomPosition);
    assert(std::abs(interactionPlot.interactionLegendX - 0.25) < 1e-9);
    assert(std::abs(interactionPlot.interactionLegendY - 0.40) < 1e-9);
    assert(RegressionEffectTickLabel(21.85, 21.5, 22.1) !=
           RegressionEffectTickLabel(21.95, 21.5, 22.1));
    interactionPlot.title = "Estimated marginal means: Exposure to risk related situations × Country";
    const auto wrappedTitle = RegressionEffectTitleLines(interactionPlot, 380.0);
    assert(wrappedTitle.size() >= 2);
    assert(RegressionEffectPlotRect(interactionPlot, 380.0, 300.0).y > 52.0);
    interactionPlot.interactionPlotLines.resize(6);
    assert(SetInteractionLegendPosition(interactionPlot, "outside_right"));
    outsideRect = RegressionEffectPlotRect(interactionPlot, 760.0, 520.0);
    outsideItems = BuildInteractionLegendLayout(interactionPlot, outsideRect);
    assert(outsideItems.size() == 6);
    for (const auto &item : outsideItems)
        assert(item.sampleStart.x > outsideRect.x + outsideRect.width);

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
        assert(label.labelAnchor.x - 6.5 * label.label.size() >= plotRect.x);
        assert(label.labelAnchor.x <= plotRect.x + plotRect.width);
    }
    std::sort(labelY.begin(), labelY.end());
    assert(labelY[1] - labelY[0] >= 13.99);
    assert(labelY[2] - labelY[1] >= 13.99);

    assert(ParseCommandRequest({"TIME_SERIES_SET_IDENTIFICATION", "ts1", "none"}).action ==
           CommandAction::SetTimeSeriesIdentification);
    assert(ParseCommandRequest({"TIME_SERIES_SET_LEGEND_POSITION", "ts1", "top_left"}).action ==
           CommandAction::SetTimeSeriesLegendPosition);
    assert(ParseCommandRequest({"PLOT_INTERACTION_SET_LEGEND_POSITION", "plot1", "left"}).action ==
           CommandAction::SetInteractionLegendPosition);
    assert(ParseCommandRequest({"PLOT_REGRESSION_TOGGLE_CONFIDENCE_INTERVALS", "plot1"}).action ==
           CommandAction::ToggleRegressionConfidenceIntervals);
    assert(ParseCommandRequest({"PLOT_REGRESSION_TOGGLE_CONFIDENCE_LEVEL", "plot1"}).action ==
           CommandAction::ToggleRegressionConfidenceLevel);
    assert(CommandActionName(CommandAction::SetInteractionLegendPosition) ==
           "set_interaction_legend_position");

    PlotModel diagnostic;
    diagnostic.kind = "time_series";
    diagnostic.glmDiagnosticKind = "imputation_distributions";
    diagnostic.interactionPlotLines = created->interactionPlotLines;
    assert(!PlotLinksToDataRows(diagnostic));
    assert(PlotSeriesLegendRows(diagnostic,0).empty());
    assert(TimeSeriesPlotExplanation(diagnostic).find("80%") != std::string::npos);
    assert(TimeSeriesPlotExplanation(diagnostic).find("not a pooled estimate") != std::string::npos);
    assert(SetTimeSeriesLegendPosition(diagnostic,"left"));
    assert(diagnostic.timeSeriesIdentification=="legend");
    assert(SetInteractionLegendCustomPosition(diagnostic,.35,.25));
    auto placed=BuildInteractionLegendLayout(diagnostic,{10,20,600,400});
    assert(std::abs(placed.front().sampleStart.x-220)<1e-9);
    assert(std::abs(placed.front().sampleStart.y-120)<1e-9);
    auto resized=BuildInteractionLegendLayout(diagnostic,{10,20,1200,800});
    assert(std::abs(resized.front().sampleStart.x-430)<1e-9);
    assert(SetTimeSeriesLegendPosition(diagnostic,"bottom"));
    assert(!diagnostic.interactionLegendUsesCustomPosition);
    assert(!SetTimeSeriesLegendPosition(diagnostic,"invalid"));
    diagnostic.glmDiagnosticKind="imputation_chain_variance";
    assert(TimeSeriesPlotExplanation(diagnostic).find("variance")!=std::string::npos);
    diagnostic.glmDiagnosticKind.clear();
    assert(PlotLinksToDataRows(diagnostic));
    assert(TimeSeriesPlotExplanation(diagnostic).find("time or ordering")!=std::string::npos);
    // Distinct raw categories remain distinct after display-label cleanup,
    // including a literal missing label and reserved numeric suffixes.
    DataFrameModel collisionData;
    DataColumn time, value, group;
    time.name = "t"; time.type = "numeric";
    value.name = "y"; value.type = "numeric";
    group.name = "g"; group.type = "factor";
    const std::vector<std::string> rawGroups = {
        "A|B", "A B", "A B #1", "(missing)", "", "(missing values)"};
    for (int repeat = 0; repeat < 2; ++repeat) {
        for (std::size_t i = 0; i < rawGroups.size(); ++i) {
            time.values.push_back(std::to_string(repeat + 1));
            value.values.push_back(std::to_string(i + repeat));
            group.values.push_back(rawGroups[i]);
        }
    }
    collisionData.rows = 12;
    collisionData.columns = {time, value, group};
    PlotModel collisionPlot;
    collisionPlot.kind = "time_series";
    collisionPlot.xLabel = "t"; collisionPlot.yLabel = "y";
    collisionPlot.timeSeriesGroupVariable = "g";
    assert(RebuildTimeSeriesFromDataFrame(collisionPlot, collisionData));
    assert(collisionPlot.interactionPlotLines.size() == 6);
    const std::vector<std::string> expectedLabels = {
        "A B", "A B #2", "A B #1", "(missing)", "(missing values) #1", "(missing values)"};
    for (std::size_t i = 0; i < expectedLabels.size(); ++i) {
        const auto &line = collisionPlot.interactionPlotLines[i];
        assert(line.label == expectedLabels[i]);
        assert(line.points.size() == 2);
        assert(line.points[0].row == static_cast<int>(i + 1));
        assert(line.points[1].row == static_cast<int>(i + 7));
        assert(PlotSeriesLegendRows(collisionPlot, i).size() == 2);
    }
    // Labeled storage values must not merge when their display labels coincide.
    collisionData.columns[2].displayValues.assign(12, "Category");
    assert(RebuildTimeSeriesFromDataFrame(collisionPlot, collisionData));
    assert(collisionPlot.interactionPlotLines.size() == 6);
    std::set<std::string> uniqueLabels;
    for (const auto &line : collisionPlot.interactionPlotLines) {
        uniqueLabels.insert(line.label);
        assert(line.points.size() == 2);
    }
    assert(uniqueLabels.size() == 6);
    collisionPlot.timeSeriesGroupVariable.clear();
    assert(RebuildTimeSeriesFromDataFrame(collisionPlot, collisionData));
    assert(collisionPlot.interactionPlotLines.size() == 1);
    assert(collisionPlot.interactionPlotLines[0].points.size() == 12);
    // Optional real R payload: exercise both directions of dataset transport.
    if (argc == 3) {
        std::ifstream input(argv[1]);
        std::vector<std::string> wire;
        std::string line;
        while (std::getline(input, line)) wire.push_back(line);
        DataFrameModel transferred;
        std::size_t cursor = 0;
        bool parsed = false;
        std::string error;
        assert(ParseDataFramePayload(wire, cursor, "wire-series", transferred, &parsed, error));
        assert(parsed && cursor == wire.size());
        collisionPlot.timeSeriesGroupVariable = "g";
        assert(RebuildTimeSeriesFromDataFrame(collisionPlot, transferred));
        assert(collisionPlot.interactionPlotLines.size() == 6);
        for (const auto &series : collisionPlot.interactionPlotLines) assert(series.points.size() == 2);
        std::ofstream output(argv[2]);
        WriteDataFramePayloadForR(output, transferred, true);
    }
    return 0;
}
