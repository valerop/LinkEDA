#ifndef RLISPSTAT_CORE_BACKEND_RUNTIME_H
#define RLISPSTAT_CORE_BACKEND_RUNTIME_H

#include <functional>
#include <string>

namespace rlispstat {
namespace core {

struct BackendLaunchOptions {
    int port = 0;
    std::string fifoPath;
    std::string rscriptPath = "Rscript";
    std::string notifyFifoPath;
    long parentPid = 0;
};

struct BackendLaunchParseResult {
    bool ok = false;
    int exitCode = 2;
    BackendLaunchOptions options;
    std::string error;
};

// All common launch behavior is independent of the native window toolkit.
// Platform backends provide only their window-loop and command-server hooks.
struct BackendPlatformServices {
    std::function<void(const BackendLaunchOptions &)> configure;
    std::function<void()> initializeApplication;
    std::function<void(const BackendLaunchOptions &)> startCommandServer;
    std::function<void()> runEventLoop;
    std::function<void()> stopCommandServer;
};

BackendLaunchParseResult ParseBackendLaunchArguments(int argc, char **argv);
const char *BackendLaunchUsage();
int RunBackendApplication(int argc, char **argv,
                          const BackendPlatformServices &services);

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_BACKEND_RUNTIME_H
