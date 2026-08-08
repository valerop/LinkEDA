#ifndef RLISPSTAT_CORE_SESSION_CONTROLLER_H
#define RLISPSTAT_CORE_SESSION_CONTROLLER_H

#include <functional>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

struct SessionPoint {
    double x = 0.0;
    double y = 0.0;
    int row = 0;
};

struct SessionPlot {
    std::string id;
    std::string group;
    std::string xLabel;
    std::string yLabel;
    std::string title;
    std::vector<SessionPoint> points;
};

using SessionReadLine = std::function<bool(std::string &)>;

struct SessionControllerServices {
    std::function<std::string(const SessionPlot &)> addPlot;
    std::function<bool(const std::string &, std::set<int> &)> selectedRows;
    std::function<std::string(const std::string &)> clearSelection;
    std::function<void()> closeAll;
};

class SessionController {
public:
    explicit SessionController(SessionControllerServices services = {});

    std::string handle(SessionReadLine readLine);
    bool hasSession(const std::string &group) const;
    bool isClosed() const;

private:
    bool readAddPlot(SessionReadLine readLine, SessionPlot &plot, std::string &error) const;

    SessionControllerServices services_;
    std::map<std::string, SessionPlot> plots_;
    std::map<std::string, std::set<int>> selectedRows_;
    bool closed_ = false;
};

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_SESSION_CONTROLLER_H
