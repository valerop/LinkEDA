#pragma once

#include "../../../../core/session_controller.h"

#include <winrt/Microsoft.UI.Dispatching.h>
#include <winrt/Microsoft.UI.Xaml.h>
#include <winrt/Microsoft.UI.Xaml.Controls.h>
#include <winrt/Microsoft.UI.Xaml.Shapes.h>

#include <functional>
#include <memory>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace winrt::rlispstatWinUI::implementation
{
    class ScatterPlotView final
        : public std::enable_shared_from_this<ScatterPlotView>
    {
    public:
        using SelectionCallback = std::function<void(std::string const&, int)>;
        using ClosedCallback = std::function<void()>;

        static std::shared_ptr<ScatterPlotView> Create();

        void SetSelectionCallback(SelectionCallback callback);
        void SetClosedCallback(ClosedCallback callback);
        void Show(::rlispstat::core::SessionPlot const& plot);
        void RefreshSelection(std::set<int> const& selectedRows);
        void Reset();
        void Activate();
        void Close();

        std::string const& PlotId() const;
        std::string const& Group() const;
        winrt::Microsoft::UI::Dispatching::DispatcherQueue DispatcherQueue() const;

    private:
        ScatterPlotView();
        void Initialize();
        void AttachClosedHandler();

        winrt::Microsoft::UI::Xaml::Window window_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::Canvas pointsCanvas_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::TextBlock plotTitle_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::TextBlock emptyLabel_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::TextBlock xAxisLabel_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::TextBlock yAxisLabel_{ nullptr };
        std::vector<std::pair<int, winrt::Microsoft::UI::Xaml::Shapes::Ellipse>>
            renderedPoints_;
        SelectionCallback selectionCallback_;
        ClosedCallback closedCallback_;
        std::string plotId_;
        std::string group_;
        bool closed_ = false;
    };
}
