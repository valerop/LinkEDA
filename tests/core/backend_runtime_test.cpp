#include "../../src/core/backend_runtime.h"

#include <cassert>
#include <string>
#include <vector>

int main()
{
    {
        char arg0[] = "LinkEDA_backend";
        char *argv[] = {arg0, nullptr};
        auto parsed = rlispstat::core::ParseBackendLaunchArguments(1, argv);
        assert(!parsed.ok);
        assert(parsed.exitCode == 2);
        assert(parsed.error == rlispstat::core::BackendLaunchUsage());
    }
    {
        char arg0[] = "LinkEDA_backend";
        char arg1[] = "--port";
        char arg2[] = "4242";
        char arg3[] = "--rscript";
        char arg4[] = "/opt/Rscript";
        char arg5[] = "--notify-fifo";
        char arg6[] = "/tmp/notify";
        char arg7[] = "--parent-pid";
        char arg8[] = "12345";
        char *argv[] = {arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, nullptr};
        auto parsed = rlispstat::core::ParseBackendLaunchArguments(9, argv);
        assert(parsed.ok);
        assert(parsed.options.port == 4242);
        assert(parsed.options.fifoPath.empty());
        assert(parsed.options.rscriptPath == "/opt/Rscript");
        assert(parsed.options.notifyFifoPath == "/tmp/notify");
        assert(parsed.options.parentPid == 12345);
    }
    {
        char arg0[] = "LinkEDA_backend";
        char arg1[] = "--fifo";
        char arg2[] = "/tmp/backend";
        char *argv[] = {arg0, arg1, arg2, nullptr};
        std::vector<std::string> calls;
        rlispstat::core::BackendPlatformServices services;
        services.configure = [&](const rlispstat::core::BackendLaunchOptions &options) {
            assert(options.fifoPath == "/tmp/backend");
            calls.push_back("configure");
        };
        services.initializeApplication = [&]() { calls.push_back("initialize"); };
        services.startCommandServer = [&](const rlispstat::core::BackendLaunchOptions &) {
            calls.push_back("server");
        };
        services.runEventLoop = [&]() { calls.push_back("loop"); };
        services.stopCommandServer = [&]() { calls.push_back("stop"); };
        assert(rlispstat::core::RunBackendApplication(3, argv, services) == 0);
        assert((calls == std::vector<std::string>{"configure", "initialize", "server", "loop", "stop"}));
    }
    return 0;
}
