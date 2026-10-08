#include "../../src/core/plot_coordinator.h"

#include <cassert>
#include <set>
#include <string>
#include <vector>

int main()
{
    using namespace rlispstat::core;

    ApplicationState state;
    PlotCoordinator coordinator(state);
    PlotModel first;
    first.id = "scatter_wt";
    first.group = "cars";
    PlotModel second;
    second.id = "scatter_hp";
    second.group = "cars";

    first.interactionMode = "none";
    second.interactionMode = "brush";
    auto firstAttached = coordinator.attachPlot(first);
    assert(first.interactionMode == "select");
    assert(firstAttached.accepted);
    assert(firstAttached.changed);
    assert(firstAttached.event.groupHasPlots);
    assert((firstAttached.event.affectedPlotIds ==
            std::vector<std::string>{"scatter_wt"}));

    auto secondAttached = coordinator.attachPlot(second);
    assert(secondAttached.accepted);
    assert(second.interactionMode == "select");
    assert(state.activePlotId().empty());
    assert(coordinator.activatePlot("scatter_hp"));
    assert(state.activePlotId() == "scatter_hp");
    assert((secondAttached.event.affectedPlotIds ==
            std::vector<std::string>{"scatter_hp", "scatter_wt"}));

    auto selected = coordinator.applySelection(
        "cars", {2, 5}, SelectionMode::Toggle);
    assert(selected.accepted);
    assert(selected.changed);
    assert(selected.event.selectionVersion == 1);
    assert((selected.event.selectedRows == std::set<int>{2, 5}));
    assert((selected.event.affectedPlotIds ==
            std::vector<std::string>{"scatter_hp", "scatter_wt"}));

    auto unchanged = coordinator.replaceSelection("cars", {5, 2});
    assert(unchanged.accepted);
    assert(!unchanged.changed);
    assert(unchanged.event.selectionVersion == 1);

    auto firstDetached = coordinator.detachPlot("scatter_wt");
    assert(firstDetached.accepted);
    assert(firstDetached.event.groupHasPlots);
    assert((firstDetached.event.affectedPlotIds ==
            std::vector<std::string>{"scatter_hp"}));
    assert((firstDetached.event.selectedRows == std::set<int>{2, 5}));

    auto cleared = coordinator.clearSelection("cars");
    assert(cleared.accepted);
    assert(cleared.changed);
    assert(cleared.event.selectionVersion == 2);
    assert(cleared.event.selectedRows.empty());
    assert((cleared.event.affectedPlotIds ==
            std::vector<std::string>{"scatter_hp"}));

    auto inverted = coordinator.invertSelection("cars", {1, 2, 3});
    assert(inverted.accepted);
    assert(inverted.changed);
    assert((inverted.event.selectedRows == std::set<int>{1, 2, 3}));

    auto invertedSubset = coordinator.invertSelection("cars", {2, 3, 4});
    assert(invertedSubset.accepted);
    assert(invertedSubset.changed);
    assert((invertedSubset.event.selectedRows == std::set<int>{4}));

    auto secondDetached = coordinator.detachPlot("scatter_hp");
    assert(secondDetached.accepted);
    assert(!secondDetached.event.groupHasPlots);
    assert(state.hasSelectionGroup("cars"));
    assert((secondDetached.event.selectedRows == std::set<int>{4}));
    assert(state.activePlotId().empty());

    auto missing = coordinator.applySelection(
        "missing", {1}, SelectionMode::Replace);
    assert(!missing.accepted);
    assert(!missing.changed);
    assert(!coordinator.activatePlot("missing"));

    // Explore Plot sends each pointer gesture through this shared coordinator.
    // Check the complete sequence, including modes that need an existing set.
    {
        ApplicationState exploreState;
        PlotCoordinator explore(exploreState);
        PlotModel plot;
        plot.id = "explore";
        plot.group = "cars";
        assert(explore.attachPlot(plot).accepted);
        assert((explore.applySelection("cars", {2}, SelectionMode::Toggle)
                    .event.selectedRows == std::set<int>{2}));
        assert((explore.applySelection("cars", {2}, SelectionMode::Toggle)
                    .event.selectedRows == std::set<int>{}));
        assert((explore.applySelection("cars", {1, 2}, SelectionMode::Replace)
                    .event.selectedRows == std::set<int>{1, 2}));
        assert((explore.applySelection("cars", {2, 3}, SelectionMode::Add)
                    .event.selectedRows == std::set<int>{1, 2, 3}));
        assert((explore.applySelection("cars", {1, 3}, SelectionMode::Subtract)
                    .event.selectedRows == std::set<int>{2}));
        assert((explore.applySelection("cars", {2, 4}, SelectionMode::Toggle)
                    .event.selectedRows == std::set<int>{4}));
        assert((explore.applySelection("cars", {}, SelectionMode::Subtract)
                    .event.selectedRows == std::set<int>{4}));
    }

    coordinator.attachPlot(first);
    coordinator.attachPlot(second);
    coordinator.activatePlot("scatter_wt");
    auto reset = coordinator.resetPlots();
    assert(reset.accepted);
    assert(reset.changed);
    assert(reset.event.type == PlotCoordinationEventType::PlotsReset);
    assert((reset.event.affectedPlotIds ==
            std::vector<std::string>{"scatter_hp", "scatter_wt"}));
    assert(state.plots().empty());
    assert(state.activePlotId().empty());
    assert(state.hasSelectionGroup("cars"));
    for (const auto &mode : {"none", "pan", "zoom", "brush", "identify", "label"}) {
        ApplicationState restoredState;
        PlotCoordinator restoredCoordinator(restoredState);
        PlotModel restored;
        restored.id = "saved_histogram";
        restored.group = "cars";
        restored.kind = "trellis_scatterplot";
        restored.trellisSpecificationInitialized = true;
        restored.trellisSpecification.plotType = TrellisPlotType::Histogram;
        restored.interactionMode = mode;
        assert(restoredCoordinator.attachPlot(restored).accepted);
        assert(restored.interactionMode == "select");
    }

    ApplicationState closingState;
    DataFrameModel cars;
    cars.group = "cars";
    cars.rows = 3;
    DataFrameModel other;
    other.group = "other";
    other.rows = 2;
    assert(closingState.registerDataset(cars));
    assert(closingState.registerDataset(other));
    PlotCoordinator closingCoordinator(closingState);
    PlotModel carsPlot;
    carsPlot.id = "cars-plot";
    carsPlot.group = "cars";
    PlotModel otherPlot;
    otherPlot.id = "other-plot";
    otherPlot.group = "other";
    assert(closingCoordinator.attachPlot(carsPlot).accepted);
    assert(closingCoordinator.attachPlot(otherPlot).accepted);
    assert(closingCoordinator.activatePlot("cars-plot"));
    closingState.groupModels()["cars-model-a"].group = "cars";
    closingState.groupModels()["cars-model-b"].group = "cars";
    closingState.groupModels()["other-model"].group = "other";
    OutputCodeReference carsOutput;
    carsOutput.outputId = "cars-table";
    carsOutput.provenance.dataVersion.datasetId = "cars";
    closingState.registerOutputCodeReference(carsOutput);
    OutputCodeReference otherOutput;
    otherOutput.outputId = "other-table";
    otherOutput.provenance.dataVersion.datasetId = "other";
    closingState.registerOutputCodeReference(otherOutput);
    assert(closingState.closeDatasetAndAnalyses("cars"));
    assert(!closingState.datasets().contains("cars"));
    assert(closingState.datasets().contains("other"));
    assert(!closingState.plots().count("cars-plot"));
    assert(closingState.plots().count("other-plot"));
    assert(closingState.activePlotId() == "other-plot");
    assert(!closingState.groupModels().count("cars-model-a"));
    assert(!closingState.groupModels().count("cars-model-b"));
    assert(closingState.groupModels().count("other-model"));
    assert(!closingState.outputCodeReference("cars-table"));
    assert(closingState.outputCodeReference("other-table"));
    assert(!closingState.closeDatasetAndAnalyses("cars"));

    return 0;
}
