#include "pch.h"
#include "App.xaml.h"
#include <fstream>
#include <sstream>
#include <vector>

using namespace winrt;
using namespace Microsoft::UI::Xaml;

namespace
{
    void LogStartup(std::string const& message)
    {
        char tempPath[MAX_PATH]{};
        if (GetTempPathA(MAX_PATH, tempPath) == 0) return;
        std::ofstream output(std::string(tempPath) + "rlispstat_winui_startup.log", std::ios::app);
        output << message << '\n';
    }

    std::vector<std::wstring> SplitLines(std::wstring const& request)
    {
        std::wistringstream input(request);
        std::vector<std::wstring> lines;
        for (std::wstring line; std::getline(input, line);)
        {
            if (!line.empty() && line.back() == L'\r') line.pop_back();
            lines.push_back(std::move(line));
        }
        return lines;
    }

    bool IsCompleteRequest(std::wstring const& request)
    {
        const auto lines = SplitLines(request);
        if (lines.empty()) return false;
        if (lines[0] == L"PING" || lines[0] == L"CLOSE_ALL") return true;
        if (lines[0] == L"SELECTED" || lines[0] == L"CLEAR")
        {
            return lines.size() >= 2;
        }
        if (lines[0] != L"ADD_PLOT")
        {
            return request.find(L'\n') != std::wstring::npos;
        }
        if (lines.size() < 7) return false;

        wchar_t* end = nullptr;
        const auto count = std::wcstol(lines[6].c_str(), &end, 10);
        return end != lines[6].c_str()
            && count >= 0
            && lines.size() >= static_cast<size_t>(7 + count);
    }

    fire_and_forget ReplyToClient(
        winrt::Windows::Networking::Sockets::StreamSocket socket,
        winrt::Microsoft::UI::Dispatching::DispatcherQueue dispatcher,
        winrt::rlispstatWinUI::implementation::App* app)
    {
        try
        {
            winrt::Windows::Storage::Streams::DataReader reader(socket.InputStream());
            reader.UnicodeEncoding(winrt::Windows::Storage::Streams::UnicodeEncoding::Utf8);
            reader.InputStreamOptions(winrt::Windows::Storage::Streams::InputStreamOptions::Partial);

            std::wstring requestText;
            while (requestText.size() < 8 * 1024 * 1024)
            {
                const auto length = co_await reader.LoadAsync(64 * 1024);
                if (length == 0) break;
                const auto chunk = reader.ReadString(length);
                requestText.append(chunk.c_str(), chunk.size());
                if (IsCompleteRequest(requestText)) break;
            }

            std::string reply = "ERR incomplete WinUI command";
            if (IsCompleteRequest(requestText))
            {
                co_await wil::resume_foreground(dispatcher);
                reply = app->DispatchRequest(requestText);
            }

            winrt::Windows::Storage::Streams::DataWriter writer(socket.OutputStream());
            writer.UnicodeEncoding(winrt::Windows::Storage::Streams::UnicodeEncoding::Utf8);
            writer.WriteString(to_hstring(reply + "\n"));
            co_await writer.StoreAsync();
            co_await writer.FlushAsync();
        }
        catch (hresult_error const& error)
        {
            LogStartup("TCP client error: "
                + std::to_string(static_cast<uint32_t>(error.code().value))
                + " " + to_string(error.message()));
        }
        catch (...)
        {
            LogStartup("TCP client error: unknown exception");
        }
    }

}

// To learn more about WinUI, the WinUI project structure,
// and more about our project templates, see: http://aka.ms/winui-project-info.

namespace winrt::rlispstatWinUI::implementation
{
    /// <summary>
    /// Initializes the singleton application object.  This is the first line of authored code
    /// executed, and as such is the logical equivalent of main() or WinMain().
    /// </summary>
    App::App()
    {
        LogStartup("App constructor entered");
        // Xaml objects should not call InitializeComponent during construction.
        // See https://github.com/microsoft/cppwinrt/tree/master/nuget#initializecomponent

#if defined _DEBUG && !defined DISABLE_XAML_GENERATED_BREAK_ON_UNHANDLED_EXCEPTION
        UnhandledException([](IInspectable const&, UnhandledExceptionEventArgs const& e)
        {
            if (IsDebuggerPresent())
            {
                auto errorMessage = e.Message();
                __debugbreak();
            }
        });
#endif
    }

    /// <summary>
    /// Invoked when the application is launched.
    /// </summary>
    /// <param name="e">Details about the launch request and process.</param>
    void App::OnLaunched([[maybe_unused]] LaunchActivatedEventArgs const& e)
    {
        LogStartup("OnLaunched entered");
        try
        {
            pendingView_ = ScatterPlotView::Create();
            pendingView_->SetClosedCallback([this]()
            {
                pendingView_.reset();
            });
            dispatcherQueue_ = pendingView_->DispatcherQueue();
            pendingView_->Activate();
            LogStartup("Initial scatter view activated");
            InitializeDispatcher();
            StartTcpListener();
        }
        catch (hresult_error const& error)
        {
            LogStartup("OnLaunched hresult: " + std::to_string(static_cast<uint32_t>(error.code().value)) + " " + to_string(error.message()));
            MessageBoxW(nullptr, error.message().c_str(), L"LinkEDA startup error", MB_OK | MB_ICONERROR);
        }
    }

    fire_and_forget App::StartTcpListener()
    {
        try
        {
            listener_ = winrt::Windows::Networking::Sockets::StreamSocketListener();
            const auto dispatcher = dispatcherQueue_;
            listener_.ConnectionReceived([dispatcher, this](auto const&, auto const& args)
            {
                ReplyToClient(args.Socket(), dispatcher, this);
            });
            co_await listener_.BindServiceNameAsync(L"39072");
            if (pendingView_) pendingView_->Reset();
            LogStartup("TCP listener bound to 39072");
        }
        catch (hresult_error const& error)
        {
            LogStartup("TCP listener error: "
                + std::to_string(static_cast<uint32_t>(error.code().value))
                + " " + to_string(error.message()));
        }
    }

    void App::InitializeDispatcher()
    {
        ::rlispstat::core::CommandDispatcherServices services;
        services.session.addPlot = [this](::rlispstat::core::SessionPlot const& plot)
        {
            auto model = std::make_unique<::rlispstat::core::PlotModel>();
            model->id = plot.id;
            model->group = plot.group;
            model->xLabel = plot.xLabel;
            model->yLabel = plot.yLabel;
            model->title = plot.title;
            for (auto const& point : plot.points)
            {
                model->points.push_back({ point.x, point.y, point.row });
            }

            if (!commandDispatcher_) return std::string("ERR coordinator unavailable");
            auto* modelPointer = model.get();
            plots_[plot.id] = std::move(model);
            auto& coordinator = commandDispatcher_->plotCoordinator();
            const auto coordination = coordinator.attachPlot(*modelPointer);
            if (!coordination.accepted)
            {
                plots_.erase(plot.id);
                return std::string("ERR invalid plot identity");
            }
            coordinator.activatePlot(plot.id);
            ShowScatterPlot(plot);
            return std::string();
        };
        services.session.selectedRows = [this](std::string const& group, std::set<int>& rows)
        {
            return commandDispatcher_->applicationState().selectedRows(group, rows);
        };
        services.session.clearSelection = [this](std::string const& group)
        {
            if (!commandDispatcher_)
            {
                return std::string("ERR coordinator unavailable");
            }
            const auto coordination =
                commandDispatcher_->plotCoordinator().clearSelection(group);
            if (!coordination.accepted) return std::string("ERR no active plot/group");
            RefreshSelectionVisuals(coordination.event);
            return std::string();
        };
        services.session.closeAll = [this]()
        {
            ResetAllViews();
        };
        commandDispatcher_ =
            std::make_unique<::rlispstat::core::CommandDispatcher>(std::move(services));
    }

    std::string App::DispatchRequest(std::wstring const& request)
    {
        if (!commandDispatcher_) return "ERR dispatcher unavailable";

        const auto wideLines = SplitLines(request);
        std::vector<std::string> lines;
        lines.reserve(wideLines.size());
        for (auto const& line : wideLines)
        {
            lines.push_back(to_string(hstring(line)));
        }
        std::size_t cursor = 0;
        const auto reply = commandDispatcher_->dispatchSession(
            [&lines, &cursor](std::string& line)
            {
                if (cursor >= lines.size()) return false;
                line = lines[cursor++];
                return true;
            });
        LogStartup("CommandDispatcher reply: " + reply);
        return reply;
    }

    void App::ShowScatterPlot(::rlispstat::core::SessionPlot const& plot)
    {
        std::shared_ptr<ScatterPlotView> view;
        auto existing = views_.find(plot.id);
        if (existing != views_.end())
        {
            view = existing->second;
        }
        else if (pendingView_)
        {
            view = std::move(pendingView_);
        }
        else
        {
            view = ScatterPlotView::Create();
        }

        view->SetSelectionCallback([this](std::string const& group, int row)
        {
            ToggleSelectedRow(group, row);
        });
        const auto plotId = plot.id;
        view->SetClosedCallback([this, plotId]()
        {
            HandleViewClosed(plotId);
        });
        views_[plot.id] = view;
        view->Show(plot);

        const auto selection = commandDispatcher_
            ? commandDispatcher_->plotCoordinator().selectionSnapshot(plot.group)
            : ::rlispstat::core::PlotCoordinationResult{};
        view->RefreshSelection(selection.event.selectedRows);
        view->Activate();

        LogStartup("Rendered " + std::to_string(plot.points.size())
            + " scatter points in view " + plot.id
            + " through CommandDispatcher/ApplicationState");
    }

    bool App::ToggleSelectedRow(std::string const& group, int row)
    {
        if (!commandDispatcher_) return false;
        const auto coordination = commandDispatcher_->plotCoordinator().applySelection(
            group, { row }, ::rlispstat::core::SelectionMode::Toggle);
        if (!coordination.accepted) return false;
        RefreshSelectionVisuals(coordination.event);
        LogStartup("Selection changed for " + group + ": "
            + ::rlispstat::core::CaseSetText(coordination.event.selectedRows));
        return coordination.event.selectedRows.count(row) != 0;
    }

    void App::HandleViewClosed(std::string const& plotId)
    {
        if (resettingViews_ || plotId.empty()) return;
        const auto coordination = commandDispatcher_
            ? commandDispatcher_->plotCoordinator().detachPlot(plotId)
            : ::rlispstat::core::PlotCoordinationResult{};
        views_.erase(plotId);
        plots_.erase(plotId);
        LogStartup("Closed scatter view " + plotId
            + "; remaining views: " + std::to_string(views_.size())
            + "; group still active: "
            + (coordination.event.groupHasPlots ? "yes" : "no"));
    }

    void App::RefreshSelectionVisuals(
        ::rlispstat::core::PlotCoordinationEvent const& event)
    {
        std::size_t refreshed = 0;
        for (auto const& plotId : event.affectedPlotIds)
        {
            auto found = views_.find(plotId);
            if (found != views_.end() && found->second)
            {
                found->second->RefreshSelection(event.selectedRows);
                ++refreshed;
            }
        }
        LogStartup("Refreshed selection for " + event.group + " in "
            + std::to_string(refreshed) + " views: "
            + ::rlispstat::core::CaseSetText(event.selectedRows));
    }

    void App::ResetAllViews()
    {
        resettingViews_ = true;
        std::shared_ptr<ScatterPlotView> retained = pendingView_;
        if (!retained && !views_.empty()) retained = views_.begin()->second;
        for (auto const& [plotId, view] : views_)
        {
            if (view && view != retained) view->Close();
        }
        views_.clear();
        if (commandDispatcher_) commandDispatcher_->plotCoordinator().resetPlots();
        plots_.clear();
        if (!retained) retained = ScatterPlotView::Create();
        retained->SetSelectionCallback({});
        retained->SetClosedCallback([this]()
        {
            pendingView_.reset();
        });
        retained->Reset();
        retained->Activate();
        pendingView_ = std::move(retained);
        resettingViews_ = false;
        LogStartup("Reset all scatter views");
    }
}
