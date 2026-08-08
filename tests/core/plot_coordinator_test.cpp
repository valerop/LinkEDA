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

    auto firstAttached = coordinator.attachPlot(first);
    assert(firstAttached.accepted);
    assert(firstAttached.changed);
    assert(firstAttached.event.groupHasPlots);
    assert((firstAttached.event.affectedPlotIds ==
            std::vector<std::string>{"scatter_wt"}));

    auto secondAttached = coordinator.attachPlot(second);
    assert(secondAttached.accepted);
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

    auto secondDetached = coordinator.detachPlot("scatter_hp");
    assert(secondDetached.accepted);
    assert(!secondDetached.event.groupHasPlots);
    assert(state.hasSelectionGroup("cars"));
    assert((secondDetached.event.selectedRows == std::set<int>{1, 2, 3}));
    assert(state.activePlotId().empty());

    auto missing = coordinator.applySelection(
        "missing", {1}, SelectionMode::Replace);
    assert(!missing.accepted);
    assert(!missing.changed);
    assert(!coordinator.activatePlot("missing"));

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
    return 0;
}
