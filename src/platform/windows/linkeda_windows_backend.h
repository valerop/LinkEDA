#ifndef RLISPSTAT_PLATFORM_WINDOWS_BACKEND_H
#define RLISPSTAT_PLATFORM_WINDOWS_BACKEND_H

#include "../platform_backend.h"

#include <map>
#include <string>

namespace rlispstat {
namespace platform {
namespace windows {

// Deliberately headless in the first Windows milestone. It fulfils the common
// platform contract without introducing a window toolkit before the command
// and transport boundaries are proven on Windows.
class WindowsBackend final : public PlatformBackend {
public:
    void createWindow(const std::string &windowId, const WindowSpec &spec) override;
    void closeWindow(const std::string &windowId) override;
    void requestRedraw(const std::string &windowId) override;
    void setWindowTitle(const std::string &windowId, const std::string &title) override;

    bool hasWindow(const std::string &windowId) const;
    bool redrawRequested(const std::string &windowId) const;
    std::string windowTitle(const std::string &windowId) const;

private:
    struct WindowState {
        WindowSpec spec;
        bool redrawRequested = false;
    };

    std::map<std::string, WindowState> windows_;
};

// Runs the headless TCP backend. The supported Windows transport is
// --port; named-pipe support is intentionally outside this milestone.
int RunBackend(int argc, char **argv);

} // namespace windows
} // namespace platform
} // namespace rlispstat

#endif // RLISPSTAT_PLATFORM_WINDOWS_BACKEND_H
