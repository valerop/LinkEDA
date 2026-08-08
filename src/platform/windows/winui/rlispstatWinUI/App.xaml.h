#pragma once

#include "App.xaml.g.h"
#include "ScatterPlotView.h"
#include "../../../../core/command_dispatcher.h"
#include "../../../../core/plot_coordinator.h"

#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

namespace winrt::rlispstatWinUI::implementation
{
    struct App : AppT<App>
    {
        App();

        void OnLaunched(Microsoft::UI::Xaml::LaunchActivatedEventArgs const&);
        winrt::fire_and_forget StartTcpListener();
        std::string DispatchRequest(std::wstring const& request);
        void ShowScatterPlot(::rlispstat::core::SessionPlot const& plot);

    private:
        void InitializeDispatcher();
        bool ToggleSelectedRow(std::string const& group, int row);
        void HandleViewClosed(std::string const& plotId);
        void RefreshSelectionVisuals(
            ::rlispstat::core::PlotCoordinationEvent const& event);
        void ResetAllViews();

        winrt::Windows::Networking::Sockets::StreamSocketListener listener_{ nullptr };
        winrt::Microsoft::UI::Dispatching::DispatcherQueue dispatcherQueue_{ nullptr };
        std::unique_ptr<::rlispstat::core::CommandDispatcher> commandDispatcher_;
        std::map<std::string, std::unique_ptr<::rlispstat::core::PlotModel>> plots_;
        std::map<std::string, std::shared_ptr<ScatterPlotView>> views_;
        std::shared_ptr<ScatterPlotView> pendingView_;
        bool resettingViews_ = false;
    };
}
