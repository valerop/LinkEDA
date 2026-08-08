#include "pch.h"
#include "ScatterPlotView.h"

#include "../../../../core/plot_geometry.h"

#include <string_view>
#include <vector>

using namespace winrt;
using namespace Microsoft::UI::Xaml;

namespace
{
    Media::SolidColorBrush Brush(uint8_t red, uint8_t green, uint8_t blue)
    {
        return Media::SolidColorBrush(
            winrt::Windows::UI::ColorHelper::FromArgb(255, red, green, blue));
    }

    Controls::TextBlock Label(
        std::wstring_view text,
        double left,
        double top,
        double fontSize = 12.0)
    {
        auto label = Controls::TextBlock();
        label.Text(hstring(text));
        label.FontSize(fontSize);
        label.Foreground(Brush(35, 43, 53));
        Controls::Canvas::SetLeft(label, left);
        Controls::Canvas::SetTop(label, top);
        return label;
    }

    Shapes::Line Line(
        double x1,
        double y1,
        double x2,
        double y2,
        Media::Brush const& stroke,
        double thickness = 1.5)
    {
        auto line = Shapes::Line();
        line.X1(x1);
        line.Y1(y1);
        line.X2(x2);
        line.Y2(y2);
        line.Stroke(stroke);
        line.StrokeThickness(thickness);
        return line;
    }
}

namespace winrt::rlispstatWinUI::implementation
{
    std::shared_ptr<ScatterPlotView> ScatterPlotView::Create()
    {
        auto view = std::shared_ptr<ScatterPlotView>(new ScatterPlotView());
        view->AttachClosedHandler();
        return view;
    }

    ScatterPlotView::ScatterPlotView()
    {
        Initialize();
    }

    void ScatterPlotView::Initialize()
    {
        window_ = Window();
        window_.Title(L"LinkEDA scatterplot");

        auto root = Controls::Grid();
        root.Background(Brush(242, 244, 247));

        plotTitle_ = Controls::TextBlock();
        plotTitle_.Text(L"LinkEDA scatterplot - esperando datos de R en TCP 39072");
        plotTitle_.FontSize(22);
        plotTitle_.FontWeight(winrt::Windows::UI::Text::FontWeights::SemiBold());
        plotTitle_.Foreground(Brush(25, 32, 41));
        plotTitle_.Margin(Thickness{ 28, 22, 28, 0 });
        plotTitle_.HorizontalAlignment(HorizontalAlignment::Left);
        plotTitle_.VerticalAlignment(VerticalAlignment::Top);
        root.Children().Append(plotTitle_);

        auto plot = Controls::Canvas();
        plot.Width(760);
        plot.Height(520);
        plot.Margin(Thickness{ 28, 72, 28, 28 });
        plot.HorizontalAlignment(HorizontalAlignment::Left);
        plot.VerticalAlignment(VerticalAlignment::Top);
        plot.Background(Brush(255, 255, 255));

        auto axisBrush = Brush(55, 65, 81);
        auto gridBrush = Brush(218, 223, 231);
        for (int tick = 0; tick <= 5; ++tick)
        {
            const double x = 82.0 + tick * 118.4;
            const double y = 42.0 + tick * 74.4;
            plot.Children().Append(Line(x, 42, x, 414, gridBrush, 1));
            plot.Children().Append(Line(82, y, 674, y, gridBrush, 1));
        }
        plot.Children().Append(Line(82, 414, 674, 414, axisBrush, 2));
        plot.Children().Append(Line(82, 42, 82, 414, axisBrush, 2));
        xAxisLabel_ = Label(L"x", 365, 462, 15);
        yAxisLabel_ = Label(L"y", 22, 218, 15);
        emptyLabel_ = Label(L"Los puntos recibidos desde R apareceran aqui", 235, 235, 14);
        plot.Children().Append(xAxisLabel_);
        plot.Children().Append(yAxisLabel_);
        plot.Children().Append(emptyLabel_);

        pointsCanvas_ = Controls::Canvas();
        pointsCanvas_.Width(760);
        pointsCanvas_.Height(520);
        plot.Children().Append(pointsCanvas_);
        root.Children().Append(plot);
        window_.Content(root);
    }

    void ScatterPlotView::AttachClosedHandler()
    {
        std::weak_ptr<ScatterPlotView> weak = shared_from_this();
        window_.Closed([weak](auto const&, auto const&)
        {
            if (auto view = weak.lock())
            {
                view->closed_ = true;
                if (view->closedCallback_) view->closedCallback_();
            }
        });
    }

    void ScatterPlotView::SetSelectionCallback(SelectionCallback callback)
    {
        selectionCallback_ = std::move(callback);
    }

    void ScatterPlotView::SetClosedCallback(ClosedCallback callback)
    {
        closedCallback_ = std::move(callback);
    }

    void ScatterPlotView::Show(::rlispstat::core::SessionPlot const& plot)
    {
        plotId_ = plot.id;
        group_ = plot.group;
        window_.Title(to_hstring(plot.title));
        plotTitle_.Text(to_hstring(plot.title));
        xAxisLabel_.Text(to_hstring(plot.xLabel));
        yAxisLabel_.Text(to_hstring(plot.yLabel));
        emptyLabel_.Visibility(
            plot.points.empty() ? Visibility::Visible : Visibility::Collapsed);
        pointsCanvas_.Children().Clear();
        renderedPoints_.clear();

        std::vector<::rlispstat::core::Point> dataPoints;
        dataPoints.reserve(plot.points.size());
        for (auto const& sample : plot.points)
        {
            dataPoints.push_back({ sample.x, sample.y });
        }
        const auto viewport = ::rlispstat::core::DataViewportForPoints(dataPoints);
        const ::rlispstat::core::Rect plotRect{ 82.0, 42.0, 592.0, 372.0 };

        for (auto const& sample : plot.points)
        {
            const auto screen = ::rlispstat::core::DataToScreen(
                { sample.x, sample.y }, viewport, plotRect, true);
            auto dot = Shapes::Ellipse();
            dot.Width(10);
            dot.Height(10);
            dot.Fill(Brush(224, 72, 64));
            Controls::Canvas::SetLeft(dot, screen.x - 5.0);
            Controls::Canvas::SetTop(dot, screen.y - 5.0);
            const auto group = plot.group;
            const auto row = sample.row;
            dot.PointerPressed([this, group, row](
                winrt::Windows::Foundation::IInspectable const&,
                winrt::Microsoft::UI::Xaml::Input::PointerRoutedEventArgs const& event)
            {
                if (selectionCallback_) selectionCallback_(group, row);
                event.Handled(true);
            });
            pointsCanvas_.Children().Append(dot);
            renderedPoints_.push_back({ sample.row, dot });
        }
    }

    void ScatterPlotView::RefreshSelection(std::set<int> const& selectedRows)
    {
        for (auto const& [row, dot] : renderedPoints_)
        {
            dot.Fill(selectedRows.count(row) != 0
                ? Brush(38, 99, 235)
                : Brush(224, 72, 64));
        }
    }

    void ScatterPlotView::Reset()
    {
        plotId_.clear();
        group_.clear();
        renderedPoints_.clear();
        pointsCanvas_.Children().Clear();
        window_.Title(L"LinkEDA scatterplot");
        plotTitle_.Text(L"LinkEDA scatterplot - escuchando R en TCP 39072");
        xAxisLabel_.Text(L"x");
        yAxisLabel_.Text(L"y");
        emptyLabel_.Visibility(Visibility::Visible);
    }

    void ScatterPlotView::Activate()
    {
        if (!closed_) window_.Activate();
    }

    void ScatterPlotView::Close()
    {
        if (!closed_) window_.Close();
    }

    std::string const& ScatterPlotView::PlotId() const
    {
        return plotId_;
    }

    std::string const& ScatterPlotView::Group() const
    {
        return group_;
    }

    Microsoft::UI::Dispatching::DispatcherQueue ScatterPlotView::DispatcherQueue() const
    {
        return pointsCanvas_.DispatcherQueue();
    }
}
