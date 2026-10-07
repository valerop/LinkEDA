#include "session_controller.h"

#include "selection_model.h"

#include <cstdlib>
#include <sstream>
#include <utility>

namespace rlispstat {
namespace core {

namespace {

bool ParsePointCount(const std::string &text, long &count)
{
    count = std::strtol(text.c_str(), nullptr, 10);
    return count >= 0 && count <= 1000000;
}

} // namespace

SessionController::SessionController(SessionControllerServices services)
    : services_(std::move(services))
{
}

bool SessionController::readAddPlot(SessionReadLine readLine,
                                    SessionPlot &plot,
                                    std::string &error) const
{
    std::string countText;
    if (!readLine(plot.id) || !readLine(plot.group) || !readLine(plot.xLabel) ||
        !readLine(plot.yLabel) || !readLine(plot.title) || !readLine(countText)) {
        error = "ERR malformed ADD_PLOT command";
        return false;
    }

    long count = 0;
    if (!ParsePointCount(countText, count)) {
        error = "ERR invalid point count";
        return false;
    }

    plot.points.reserve(static_cast<std::size_t>(count));
    for (long i = 0; i < count; ++i) {
        std::string line;
        if (!readLine(line)) {
            error = "ERR missing point data";
            return false;
        }
        std::istringstream input(line);
        SessionPoint point;
        if (!(input >> point.x >> point.y >> point.row)) {
            error = "ERR malformed point data";
            return false;
        }
        plot.points.push_back(point);
    }
    return true;
}

std::string SessionController::handle(SessionReadLine readLine)
{
    std::string command;
    if (!readLine || !readLine(command)) {
        return "";
    }
    if (command == "PING") {
        return "OK";
    }
    if (command == "ADD_PLOT") {
        SessionPlot plot;
        std::string error;
        if (!readAddPlot(readLine, plot, error)) {
            return error;
        }
        if (services_.addPlot) {
            error = services_.addPlot(plot);
            if (!error.empty()) {
                return error;
            }
        }
        const std::string group = plot.group;
        plots_[plot.id] = std::move(plot);
        selectedRows_.emplace(group, std::set<int>());
        return "OK";
    }
    if (command == "SELECTED") {
        std::string group;
        if (!readLine(group)) {
            return "ERR missing group name";
        }
        std::set<int> rows;
        if (services_.selectedRows) {
            if (!services_.selectedRows(group, rows)) {
                return "ERR no active plot/group";
            }
        } else {
            auto it = selectedRows_.find(group);
            if (it == selectedRows_.end()) {
                return "ERR no active plot/group";
            }
            rows = it->second;
        }
        std::string text = CaseSetText(rows);
        return text.empty() ? "OK" : "OK " + text;
    }
    if (command == "CLEAR") {
        std::string group;
        if (!readLine(group)) {
            return "ERR missing group name";
        }
        auto it = selectedRows_.find(group);
        if (it == selectedRows_.end()) {
            return "ERR no active plot/group";
        }
        if (services_.clearSelection) {
            std::string error = services_.clearSelection(group);
            if (!error.empty()) {
                return error;
            }
        }
        it->second.clear();
        return "OK";
    }
    if (command == "CLOSE_ALL") {
        closed_ = true;
        plots_.clear();
        selectedRows_.clear();
        if (services_.closeAll) {
            services_.closeAll();
        }
        return "OK";
    }
    return "ERR unknown command";
}

bool SessionController::hasSession(const std::string &group) const
{
    return selectedRows_.find(group) != selectedRows_.end();
}

bool SessionController::isClosed() const
{
    return closed_;
}

} // namespace core
} // namespace rlispstat
