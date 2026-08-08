#include "plot_coordinator.h"

namespace rlispstat {
namespace core {

PlotCoordinator::PlotCoordinator(ApplicationState &state)
    : state_(state)
{
}

std::vector<std::string> PlotCoordinator::plotIdsForGroup(
    const std::string &group) const
{
    std::vector<std::string> ids;
    if (group.empty()) return ids;
    for (const auto &entry : state_.plots()) {
        if (entry.second && entry.second->group == group) {
            ids.push_back(entry.first);
        }
    }
    return ids;
}

PlotCoordinationResult PlotCoordinator::attachPlot(PlotModel &plot)
{
    PlotCoordinationResult result;
    if (plot.id.empty() || plot.group.empty()) return result;

    auto existing = state_.plots().find(plot.id);
    result.accepted = true;
    result.changed = existing == state_.plots().end() || existing->second != &plot;
    state_.ensureSelectionGroup(plot.group);
    state_.plots()[plot.id] = &plot;

    result.event.type = PlotCoordinationEventType::PlotAttached;
    result.event.plotId = plot.id;
    result.event.group = plot.group;
    result.event.affectedPlotIds = plotIdsForGroup(plot.group);
    state_.selectedRows(plot.group, result.event.selectedRows);
    result.event.selectionVersion = state_.groupSelectionVersions()[plot.group];
    result.event.groupHasPlots = true;
    return result;
}

bool PlotCoordinator::activatePlot(const std::string &plotId)
{
    auto found = state_.plots().find(plotId);
    if (found == state_.plots().end() || !found->second) return false;
    state_.activePlotId() = plotId;
    return true;
}

PlotCoordinationResult PlotCoordinator::detachPlot(const std::string &plotId)
{
    PlotCoordinationResult result;
    auto found = state_.plots().find(plotId);
    if (found == state_.plots().end()) return result;

    result.accepted = true;
    result.changed = true;
    result.event.type = PlotCoordinationEventType::PlotDetached;
    result.event.plotId = plotId;
    result.event.group = found->second ? found->second->group : std::string();
    state_.plots().erase(found);

    if (state_.activePlotId() == plotId) {
        state_.activePlotId() =
            state_.plots().empty() ? std::string() : state_.plots().begin()->first;
    }

    result.event.affectedPlotIds = plotIdsForGroup(result.event.group);
    state_.selectedRows(result.event.group, result.event.selectedRows);
    auto version = state_.groupSelectionVersions().find(result.event.group);
    if (version != state_.groupSelectionVersions().end()) {
        result.event.selectionVersion = version->second;
    }
    result.event.groupHasPlots = !result.event.affectedPlotIds.empty();
    return result;
}

PlotCoordinationResult PlotCoordinator::resetPlots()
{
    PlotCoordinationResult result;
    result.accepted = true;
    result.changed = !state_.plots().empty() || !state_.activePlotId().empty();
    result.event.type = PlotCoordinationEventType::PlotsReset;
    for (const auto &entry : state_.plots()) {
        result.event.affectedPlotIds.push_back(entry.first);
    }
    state_.plots().clear();
    state_.activePlotId().clear();
    return result;
}

PlotCoordinationResult PlotCoordinator::storeSelection(
    const std::string &group,
    const std::set<int> &rows)
{
    PlotCoordinationResult result;
    if (!state_.hasSelectionGroup(group)) return result;

    result.accepted = true;
    result.changed =
        state_.setSelectedRows(group, rows, &result.event.selectionVersion);
    result.event.type = PlotCoordinationEventType::SelectionChanged;
    result.event.group = group;
    result.event.affectedPlotIds = plotIdsForGroup(group);
    state_.selectedRows(group, result.event.selectedRows);
    result.event.groupHasPlots = !result.event.affectedPlotIds.empty();
    return result;
}

PlotCoordinationResult PlotCoordinator::replaceSelection(
    const std::string &group,
    const std::set<int> &rows)
{
    return storeSelection(group, rows);
}

PlotCoordinationResult PlotCoordinator::applySelection(
    const std::string &group,
    const std::set<int> &rows,
    SelectionMode mode)
{
    std::set<int> current;
    if (!state_.selectedRows(group, current)) return {};
    return storeSelection(group, ApplySelectionOperation(current, rows, mode));
}

PlotCoordinationResult PlotCoordinator::clearSelection(const std::string &group)
{
    return storeSelection(group, {});
}

PlotCoordinationResult PlotCoordinator::invertSelection(
    const std::string &group,
    const std::set<int> &visibleRows)
{
    std::set<int> current;
    if (!state_.selectedRows(group, current)) return {};
    return storeSelection(group, InvertSelectionWithin(current, visibleRows));
}

PlotCoordinationResult PlotCoordinator::selectionSnapshot(
    const std::string &group) const
{
    PlotCoordinationResult result;
    if (!state_.selectedRows(group, result.event.selectedRows)) return result;
    result.accepted = true;
    result.event.type = PlotCoordinationEventType::SelectionChanged;
    result.event.group = group;
    result.event.affectedPlotIds = plotIdsForGroup(group);
    auto version = state_.groupSelectionVersions().find(group);
    if (version != state_.groupSelectionVersions().end()) {
        result.event.selectionVersion = version->second;
    }
    result.event.groupHasPlots = !result.event.affectedPlotIds.empty();
    return result;
}

} // namespace core
} // namespace rlispstat
