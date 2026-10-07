#ifndef RLISPSTAT_PLATFORM_BACKEND_H
#define RLISPSTAT_PLATFORM_BACKEND_H

#include <string>

namespace rlispstat {
namespace platform {

struct WindowSpec {
    std::string title;
    int width = 0;
    int height = 0;
};

struct Point {
    double x = 0.0;
    double y = 0.0;
};

struct Rect {
    double x = 0.0;
    double y = 0.0;
    double width = 0.0;
    double height = 0.0;
};

class PlatformBackend {
public:
    virtual ~PlatformBackend() = default;

    virtual void createWindow(const std::string &windowId,
                              const WindowSpec &spec) = 0;
    virtual void closeWindow(const std::string &windowId) = 0;
    virtual void requestRedraw(const std::string &windowId) = 0;
    virtual void setWindowTitle(const std::string &windowId,
                                const std::string &title) = 0;
};

} // namespace platform
} // namespace rlispstat

#endif // RLISPSTAT_PLATFORM_BACKEND_H
