#include "rlispstat_windows_backend.h"
#include "windows_emf_export.h"

#include "../../core/backend_runtime.h"
#include "../../core/command_dispatcher.h"

#include <atomic>
#include <cstdlib>
#include <cstdio>
#include <mutex>
#include <memory>
#include <set>
#include <string>
#include <thread>
#include <vector>

#include <winsock2.h>
#include <ws2tcpip.h>
#include <windows.h>

namespace rlispstat {
namespace platform {
namespace windows {

namespace {

class WindowsRuntime {
public:
    WindowsRuntime()
        : dispatcher_(dispatcherServices())
    {
    }

    bool start(int port)
    {
        WSADATA wsaData;
        if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
            std::fprintf(stderr, "rlispstat_backend: WSAStartup failed\n");
            return false;
        }
        winsockStarted_ = true;

        listenSocket_ = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
        if (listenSocket_ == INVALID_SOCKET) {
            std::fprintf(stderr, "rlispstat_backend: socket failed\n");
            return false;
        }

        sockaddr_in address{};
        address.sin_family = AF_INET;
        address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        address.sin_port = htons(static_cast<u_short>(port));
        if (bind(listenSocket_, reinterpret_cast<sockaddr *>(&address), sizeof(address)) == SOCKET_ERROR ||
            listen(listenSocket_, SOMAXCONN) == SOCKET_ERROR) {
            std::fprintf(stderr, "rlispstat_backend: could not listen on 127.0.0.1:%d\n", port);
            return false;
        }
        running_ = true;
        serverThread_ = std::thread(&WindowsRuntime::serverLoop, this);
        return true;
    }

    void run()
    {
        while (running_) {
            Sleep(20);
        }
    }

    void stop()
    {
        const bool wasRunning = running_.exchange(false);
        if (listenSocket_ != INVALID_SOCKET) {
            shutdown(listenSocket_, SD_BOTH);
            closesocket(listenSocket_);
            listenSocket_ = INVALID_SOCKET;
        }
        if (serverThread_.joinable()) {
            serverThread_.join();
        }
        if (winsockStarted_) {
            WSACleanup();
            winsockStarted_ = false;
        }
        (void)wasRunning;
    }

    ~WindowsRuntime()
    {
        stop();
    }

private:
    rlispstat::core::CommandDispatcherServices dispatcherServices()
    {
        rlispstat::core::CommandDispatcherServices services;
        services.session.addPlot = [this](const rlispstat::core::SessionPlot &plot) {
            auto model = std::make_unique<rlispstat::core::PlotModel>();
            model->id = plot.id;
            model->group = plot.group;
            model->xLabel = plot.xLabel;
            model->yLabel = plot.yLabel;
            model->title = plot.title;
            for (const auto &point : plot.points) {
                model->points.push_back({point.x, point.y, point.row});
            }
            auto &state = dispatcher_.applicationState();
            state.ensureSelectionGroup(plot.group);
            state.activePlotId() = plot.id;
            state.plots()[plot.id] = model.get();
            plots_[plot.id] = std::move(model);
            return std::string();
        };
        services.session.selectedRows = [this](const std::string &group, std::set<int> &rows) {
            return dispatcher_.applicationState().selectedRows(group, rows);
        };
        services.session.clearSelection = [this](const std::string &group) {
            return dispatcher_.applicationState().clearSelectedRows(group) ? std::string() :
                std::string("ERR no active plot/group");
        };
        services.ui.copyPlot = [this](const std::string &plotId, const std::string &format) {
            auto found = plots_.find(plotId);
            if (found == plots_.end() || !found->second || format != "EMF") return false;
            std::string error;
            return CopyPlotAsEnhancedMetafile(*found->second, rlispstat::core::ExportDimensions{}, &error);
        };
        return services;
    }

    static bool readLine(SOCKET client, std::string &line)
    {
        line.clear();
        char character = 0;
        while (true) {
            const int received = recv(client, &character, 1, 0);
            if (received <= 0) return !line.empty();
            if (character == '\n') return true;
            if (character != '\r') line.push_back(character);
        }
    }

    static void writeReply(SOCKET client, const std::string &reply)
    {
        const std::string message = reply + "\n";
        const char *cursor = message.data();
        int remaining = static_cast<int>(message.size());
        while (remaining > 0) {
            const int written = send(client, cursor, remaining, 0);
            if (written == SOCKET_ERROR || written == 0) return;
            cursor += written;
            remaining -= written;
        }
    }

    void serverLoop()
    {
        while (running_) {
            SOCKET client = accept(listenSocket_, nullptr, nullptr);
            if (client == INVALID_SOCKET) {
                if (running_) std::fprintf(stderr, "rlispstat_backend: accept failed\n");
                continue;
            }
            std::string reply;
            {
                std::lock_guard<std::mutex> lock(dispatchMutex_);
                reply = dispatcher_.dispatchSession(
                    [client](std::string &line) { return readLine(client, line); });
                if (dispatcher_.sessionClosed()) running_ = false;
            }
            if (!reply.empty()) writeReply(client, reply);
            shutdown(client, SD_BOTH);
            closesocket(client);
        }
    }

    std::atomic<bool> running_{false};
    SOCKET listenSocket_ = INVALID_SOCKET;
    bool winsockStarted_ = false;
    std::thread serverThread_;
    std::mutex dispatchMutex_;
    std::map<std::string, std::unique_ptr<rlispstat::core::PlotModel>> plots_;
    rlispstat::core::CommandDispatcher dispatcher_;
};

} // namespace

void WindowsBackend::createWindow(const std::string &windowId, const WindowSpec &spec)
{
    windows_[windowId] = WindowState{spec, true};
}

void WindowsBackend::closeWindow(const std::string &windowId)
{
    windows_.erase(windowId);
}

void WindowsBackend::requestRedraw(const std::string &windowId)
{
    auto window = windows_.find(windowId);
    if (window != windows_.end()) window->second.redrawRequested = true;
}

void WindowsBackend::setWindowTitle(const std::string &windowId, const std::string &title)
{
    auto window = windows_.find(windowId);
    if (window != windows_.end()) window->second.spec.title = title;
}

bool WindowsBackend::hasWindow(const std::string &windowId) const
{
    return windows_.find(windowId) != windows_.end();
}

bool WindowsBackend::redrawRequested(const std::string &windowId) const
{
    auto window = windows_.find(windowId);
    return window != windows_.end() && window->second.redrawRequested;
}

std::string WindowsBackend::windowTitle(const std::string &windowId) const
{
    auto window = windows_.find(windowId);
    return window == windows_.end() ? "" : window->second.spec.title;
}

int RunBackend(int argc, char **argv)
{
    const auto parsed = rlispstat::core::ParseBackendLaunchArguments(argc, argv);
    if (parsed.ok && !parsed.options.fifoPath.empty()) {
        std::fprintf(stderr, "rlispstat_backend: --fifo is not supported on Windows; use --port PORT\n");
        return 2;
    }

    WindowsRuntime runtime;
    rlispstat::core::BackendPlatformServices services;
    services.configure = [](const rlispstat::core::BackendLaunchOptions &) {};
    services.initializeApplication = []() {};
    services.startCommandServer = [&runtime](const rlispstat::core::BackendLaunchOptions &options) {
        if (!runtime.start(options.port)) std::exit(2);
    };
    services.runEventLoop = [&runtime]() { runtime.run(); };
    services.stopCommandServer = [&runtime]() { runtime.stop(); };
    return rlispstat::core::RunBackendApplication(argc, argv, services);
}

} // namespace windows
} // namespace platform
} // namespace rlispstat
