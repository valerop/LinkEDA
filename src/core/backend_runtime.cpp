#include "backend_runtime.h"

#include <cstdio>
#include <cstdlib>

namespace rlispstat {
namespace core {

const char *BackendLaunchUsage()
{
    return "usage: LinkEDA_backend --fifo PATH [--rscript PATH] [--notify-fifo PATH] "
           "[--parent-pid PID] | --port PORT [--rscript PATH] [--notify-fifo PATH] "
           "[--parent-pid PID]";
}

BackendLaunchParseResult ParseBackendLaunchArguments(int argc, char **argv)
{
    BackendLaunchParseResult result;
    for (int i = 1; i < argc; ++i) {
        if (!argv || !argv[i]) continue;
        std::string argument = argv[i];
        if (argument == "--port" && i + 1 < argc && argv[i + 1]) {
            result.options.port = std::atoi(argv[++i]);
        } else if (argument == "--fifo" && i + 1 < argc && argv[i + 1]) {
            result.options.fifoPath = argv[++i];
        } else if (argument == "--rscript" && i + 1 < argc && argv[i + 1]) {
            result.options.rscriptPath = argv[++i];
        } else if (argument == "--notify-fifo" && i + 1 < argc && argv[i + 1]) {
            result.options.notifyFifoPath = argv[++i];
        } else if (argument == "--parent-pid" && i + 1 < argc && argv[i + 1]) {
            result.options.parentPid = std::strtol(argv[++i], nullptr, 10);
        }
    }
    if (result.options.port <= 0 && result.options.fifoPath.empty()) {
        result.error = BackendLaunchUsage();
        return result;
    }
    result.ok = true;
    result.exitCode = 0;
    return result;
}

int RunBackendApplication(int argc, char **argv,
                          const BackendPlatformServices &services)
{
    BackendLaunchParseResult parsed = ParseBackendLaunchArguments(argc, argv);
    if (!parsed.ok) {
        std::fprintf(stderr, "%s\n", parsed.error.c_str());
        return parsed.exitCode;
    }
    if (!services.configure || !services.initializeApplication ||
        !services.startCommandServer || !services.runEventLoop ||
        !services.stopCommandServer) {
        std::fprintf(stderr, "LinkEDA_backend: incomplete platform services\n");
        return 2;
    }

    services.configure(parsed.options);
    services.initializeApplication();
    services.startCommandServer(parsed.options);
    services.runEventLoop();
    services.stopCommandServer();
    return 0;
}

} // namespace core
} // namespace rlispstat
