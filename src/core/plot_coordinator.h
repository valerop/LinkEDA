#ifndef RLISPSTAT_CORE_PLOT_COORDINATOR_H
#define RLISPSTAT_CORE_PLOT_COORDINATOR_H

#include "application_state.h"

#include <set>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

enum class PlotCoordinationEventType {
    None,
    PlotAttached,
    PlotDetached,
    SelectionChanged,
    PlotsReset
};

struct PlotCoordinationEvent {
    PlotCoordinationEventType type = PlotCoordinationEventType::None;
    std::string plotId;
    std::string group;
    std::vector<std::string> affectedPlotIds;
    std::set<int> selectedRows;
    int selectionVersion = 0;
    bool groupHasPlots = false;
};

struct PlotCoordinationResult {
    bool accepted = false;
    bool changed = false;
    PlotCoordinationEvent event;
};

// Coordinates portable plot membership and linked row selection. It owns no
// platform objects and performs no drawing; adapters translate its results
// into native window lifetime and invalidation operations.
class PlotCoordinator {
public:
    explicit PlotCoordinator(ApplicationState &state);

    PlotCoordinationResult attachPlot(PlotModel &plot);
    bool activatePlot(const std::string &plotId);
    PlotCoordinationResult detachPlot(const std::string &plotId);
    PlotCoordinationResult resetPlots();

    PlotCoordinationResult replaceSelection(const std::string &group,
                                            const std::set<int> &rows);
    PlotCoordinationResult applySelection(const std::string &group,
                                          const std::set<int> &rows,
                                          SelectionMode mode);
    PlotCoordinationResult clearSelection(const std::string &group);
    PlotCoordinationResult invertSelection(const std::string &group,
                                           const std::set<int> &visibleRows);
    PlotCoordinationResult selectionSnapshot(const std::string &group) const;

    std::vector<std::string> plotIdsForGroup(const std::string &group) const;

private:
    PlotCoordinationResult storeSelection(const std::string &group,
                                          const std::set<int> &rows);

    ApplicationState &state_;
};

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_PLOT_COORDINATOR_H
