#include "pch.h"
#include "ScatterPlotView.h"
#include "PerformanceTrace.h"
#include "WindowBranding.h"
#include "../../windows_emf_export.h"

#include "../../../../core/export_model.h"
#include "../../../../core/format_model.h"
#include "../../../../core/dimensionality_model.h"
#include "../../../../core/plot_geometry.h"
#include "../../../../core/model_terms.h"
#include "../../../../core/scatterplot_model.h"
#include "../../../../core/scatter_matrix_model.h"
#include "../../../../core/svg_writer.h"

#include <microsoft.ui.xaml.window.h>
#include <winrt/Windows.ApplicationModel.DataTransfer.h>
#include <winrt/Windows.Storage.h>
#include <winrt/Windows.Storage.Streams.h>

#include <algorithm>
#include <chrono>
#include <cmath>
#include <fstream>
#include <iterator>
#include <limits>
#include <optional>
#include <shobjidl.h>
#include <sstream>
#include <string_view>
#include <tuple>
#include <vector>

using namespace winrt;
using namespace Microsoft::UI::Xaml;

namespace
{
    // Variable-type editing from a plot is deliberately retained only as an
    // experimental implementation. Canonical types remain editable from the
    // data sheet and Variable View, where the consequence is unambiguous.
    constexpr bool kEnableExperimentalPlotVariableTypeMenu = false;

    void AppendPlotRCodeExportItems(
        Controls::MenuFlyoutSubItem const& exportMenu,
        std::function<void(std::vector<std::string> const&)> const& dispatch,
        ::rlispstat::core::PlotModel const* model)
    {
        if (!dispatch || !model || model->codeReference.outputId.empty() ||
            model->codeReference.provenance.verificationRCode.empty()) return;
        auto rCode = Controls::MenuFlyoutSubItem();
        rCode.Text(L"R Code");
        const bool publication = !model->codeReference.publication.availableBackends.empty();
        for (auto const& action :
             ::rlispstat::core::BuildRCodeExportMenuActions(true, publication))
        {
            auto item = Controls::MenuFlyoutItem();
            item.Text(to_hstring(action.title));
            item.Click([dispatch, command = action.command,
                        outputId = model->codeReference.outputId]
                (auto const&, auto const&)
            {
                if (command == "SHOW_R_CODE")
                    dispatch({ command, "output", outputId });
                else
                    dispatch({ command, outputId });
            });
            rCode.Items().Append(item);
        }
        exportMenu.Items().Append(rCode);
    }

    HWND WindowHandle(Window const& window)
    {
        HWND handle{};
        check_hresult(window.as<::IWindowNative>()->get_WindowHandle(&handle));
        return handle;
    }

    std::optional<std::wstring> ChoosePlotExportPath(
        Window const& owner, std::string const& plotId, std::string const& format)
    {
        com_ptr<IFileSaveDialog> dialog;
        if (FAILED(CoCreateInstance(CLSID_FileSaveDialog, nullptr, CLSCTX_INPROC_SERVER,
                                    IID_PPV_ARGS(dialog.put())))) return std::nullopt;
        const std::wstring extension = to_hstring(
            ::rlispstat::core::PlotExportFileExtension(format)).c_str();
        const std::wstring pattern = L"*." + extension;
        const std::wstring label = to_hstring(format + " files").c_str();
        const COMDLG_FILTERSPEC filter{ label.c_str(), pattern.c_str() };
        dialog->SetFileTypes(1, &filter);
        dialog->SetDefaultExtension(extension.c_str());
        dialog->SetTitle(to_hstring(::rlispstat::core::PlotExportPanelTitle(format)).c_str());
        dialog->SetFileName(to_hstring(
            ::rlispstat::core::PlotExportFilename(plotId, format)).c_str());
        const HRESULT shown = dialog->Show(WindowHandle(owner));
        if (shown == HRESULT_FROM_WIN32(ERROR_CANCELLED) || FAILED(shown)) return std::nullopt;
        com_ptr<IShellItem> item;
        if (FAILED(dialog->GetResult(item.put()))) return std::nullopt;
        PWSTR rawPath{};
        if (FAILED(item->GetDisplayName(SIGDN_FILESYSPATH, &rawPath)) || !rawPath)
            return std::nullopt;
        std::wstring path(rawPath);
        CoTaskMemFree(rawPath);
        return path;
    }

    bool WriteBinaryFile(std::wstring const& path, std::vector<uint8_t> const& bytes)
    {
        std::ofstream output(path, std::ios::binary | std::ios::trunc);
        if (!output) return false;
        output.write(reinterpret_cast<char const*>(bytes.data()),
                     static_cast<std::streamsize>(bytes.size()));
        return output.good();
    }

    bool WriteTextFile(std::wstring const& path, std::string const& text)
    {
        return WriteBinaryFile(path, std::vector<uint8_t>(text.begin(), text.end()));
    }

    Media::SolidColorBrush Brush(uint8_t red, uint8_t green, uint8_t blue)
    {
        return Media::SolidColorBrush(
            winrt::Windows::UI::ColorHelper::FromArgb(255, red, green, blue));
    }

    Media::SolidColorBrush Brush(::rlispstat::core::PlotRGBA color,
                                 double alphaMultiplier = 1.0)
    {
        const auto channel = [](double value) {
            return static_cast<uint8_t>(std::clamp(std::lround(value * 255.0), 0L, 255L));
        };
        return Media::SolidColorBrush(winrt::Windows::UI::ColorHelper::FromArgb(
            channel(color.a * alphaMultiplier), channel(color.r), channel(color.g),
            channel(color.b)));
    }

    winrt::Windows::UI::Color GradientColor(
        ::rlispstat::core::PlotRGBA color, double alphaMultiplier)
    {
        const auto channel = [](double value) {
            return static_cast<uint8_t>(std::clamp(
                std::lround(value * 255.0), 0L, 255L));
        };
        return winrt::Windows::UI::ColorHelper::FromArgb(
            channel(color.a * alphaMultiplier), channel(color.r),
            channel(color.g), channel(color.b));
    }

    Media::GradientStop GradientStop(
        ::rlispstat::core::PlotRGBA color, double alpha, double offset)
    {
        auto stop = Media::GradientStop();
        stop.Color(GradientColor(color, alpha));
        stop.Offset(offset);
        return stop;
    }

    Media::SolidColorBrush TransparentBrush()
    {
        return Media::SolidColorBrush(
            winrt::Windows::UI::ColorHelper::FromArgb(1, 0, 0, 0));
    }

    ::rlispstat::core::SelectionMode SelectionModeFor(
        winrt::Windows::System::VirtualKeyModifiers modifiers,
        std::string const& configuredMode = "replace")
    {
        using winrt::Windows::System::VirtualKeyModifiers;
        const auto pressed = [&](VirtualKeyModifiers key)
        { return (modifiers & key) == key; };
        return ::rlispstat::core::SelectionModeFromString(
            ::rlispstat::core::EffectiveSelectionModeName(
                pressed(VirtualKeyModifiers::Menu), false,
                pressed(VirtualKeyModifiers::Control),
                pressed(VirtualKeyModifiers::Shift), configuredMode));
    }

    Controls::TextBlock Label(
        std::wstring_view text,
        double left,
        double top,
        double fontSize,
        Media::Brush const& foreground)
    {
        auto label = Controls::TextBlock();
        label.Text(hstring(text));
        label.FontSize(fontSize);
        label.Foreground(foreground);
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

    Shapes::Rectangle Rectangle(
        ::rlispstat::core::Rect const& rect,
        Media::Brush const& fill,
        Media::Brush const& stroke,
        double thickness = 1.0)
    {
        auto shape = Shapes::Rectangle();
        shape.Width(std::max(0.0, rect.width));
        shape.Height(std::max(0.0, rect.height));
        shape.Fill(fill);
        shape.Stroke(stroke);
        shape.StrokeThickness(thickness);
        Controls::Canvas::SetLeft(shape, rect.x);
        Controls::Canvas::SetTop(shape, rect.y);
        return shape;
    }

    void AppendPanelFrame(
        Controls::Canvas const& canvas,
        ::rlispstat::core::Rect const& rect,
        ::rlispstat::core::PlotThemeStyleSpec const& theme)
    {
        if (!theme.showPanelBorder) return;
        auto frame = Rectangle(rect, TransparentBrush(), Brush(theme.axis), 1.4);
        frame.IsHitTestVisible(false);
        Controls::Canvas::SetZIndex(frame, 11);
        canvas.Children().Append(frame);
    }

    void AppendHistogramBinOutline(
        Controls::Canvas const& canvas,
        ::rlispstat::core::Rect const& rect)
    {
        auto outline = Rectangle(rect, TransparentBrush(), Brush(0, 0, 0), 1.0);
        outline.IsHitTestVisible(false);
        canvas.Children().Append(outline);
    }

    Shapes::Polyline Polyline(
        std::vector<::rlispstat::core::Point> const& points,
        Media::Brush const& stroke,
        double thickness = 1.5)
    {
        auto line = Shapes::Polyline();
        auto collection = line.Points();
        for (auto const& point : points)
            collection.Append({ static_cast<float>(point.x), static_cast<float>(point.y) });
        line.Stroke(stroke);
        line.StrokeThickness(thickness);
        return line;
    }

    Shapes::Polygon Polygon(
        std::vector<::rlispstat::core::Point> const& points,
        Media::Brush const& fill)
    {
        auto polygon = Shapes::Polygon();
        auto collection = polygon.Points();
        for (auto const& point : points)
            collection.Append({ static_cast<float>(point.x), static_cast<float>(point.y) });
        polygon.Fill(fill);
        return polygon;
    }

    ::rlispstat::core::PlotRGBA LightPaletteColor(std::string const& name, double alpha = 1.0)
    {
        return ::rlispstat::core::PlotLightColorForNameOrHex(name, alpha);
    }

    std::vector<std::string> PaletteOrder()
    {
        std::vector<std::string> names;
        for (auto const& color : ::rlispstat::core::PaletteColors()) names.push_back(color.name);
        return names;
    }
}

namespace winrt::LinkEDA::implementation
{
    template <typename ParentMenu>
    void AppendMenuGroupWithoutRedundantSingleLevel(
        ParentMenu const& parent,
        Controls::MenuFlyoutSubItem const& group)
    {
        const auto count = group.Items().Size();
        if (count == 0) return;
        if (count == 1)
        {
            auto onlyItem = group.Items().GetAt(0);
            group.Items().RemoveAt(0);
            parent.Items().Append(onlyItem);
            return;
        }
        parent.Items().Append(group);
    }

    std::shared_ptr<ScatterPlotView> ScatterPlotView::Create()
    {
        auto view = std::shared_ptr<ScatterPlotView>(new ScatterPlotView(false));
        view->AttachClosedHandler();
        return view;
    }

    std::shared_ptr<ScatterPlotView> ScatterPlotView::CreateSnapshotHost(
        ::rlispstat::core::PlotModel const& plot,
        std::string themeName)
    {
        auto view = std::shared_ptr<ScatterPlotView>(new ScatterPlotView(true));
        view->ownedSnapshotModel_ = std::make_unique<::rlispstat::core::PlotModel>(plot);
        view->ownedSnapshotModel_->smoothShowSpanSlider = false;
        view->ownedSnapshotModel_->pointSizeSliderVisible = false;
        view->globalThemeName_ = themeName.empty() ? "bw" : std::move(themeName);
        view->themeName_ = view->globalThemeName_;
        view->pointColors_ = view->ownedSnapshotModel_->frozenRowColors;
        view->rowLabels_ = view->ownedSnapshotModel_->frozenRowLabels;
        view->Show(*view->ownedSnapshotModel_);
        view->root_.IsHitTestVisible(false);
        return view;
    }

    ScatterPlotView::ScatterPlotView(bool embedded)
        : embedded_(embedded)
    {
        Initialize();
    }

    void ScatterPlotView::Initialize()
    {
        if (!embedded_)
        {
            window_ = CreateLinkEDAWindow();
            window_.Title(L"LinkEDA scatterplot");
        }

        root_ = Controls::Grid();
        root_.Background(Brush(255, 255, 255));
        root_.RequestedTheme(ElementTheme::Light);
        auto plotColumn = Controls::ColumnDefinition();
        plotColumn.Width(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
        root_.ColumnDefinitions().Append(plotColumn);
        auto notesColumn = Controls::ColumnDefinition();
        notesColumn.Width(GridLengthHelper::FromValueAndType(1, GridUnitType::Auto));
        root_.ColumnDefinitions().Append(notesColumn);
        plotCanvas_ = Controls::Canvas();
        plotCanvas_.HorizontalAlignment(HorizontalAlignment::Stretch);
        plotCanvas_.VerticalAlignment(VerticalAlignment::Stretch);
        plotCanvas_.Background(Brush(255, 255, 255));

        pointsCanvas_ = Controls::Canvas();
        pointsCanvas_.Background(TransparentBrush());
        pointsCanvas_.IsTabStop(true);
        pointsCanvas_.PointerPressed([this](auto const&, auto const& event)
        {
            const auto current = event.GetCurrentPoint(pointsCanvas_);
            // Let WinUI raise ContextRequested for the right button.  The old
            // handler consumed every pointer press, making the macOS-parity
            // plot menu impossible to open even though it had been built.
            if (current.Properties().IsRightButtonPressed()) return;
            if (currentModel_ && ::rlispstat::core::PlotIsImputationDiagnostic(*currentModel_)) {
                const auto point = current.Position();
                const auto series = ::rlispstat::core::HitPlotSeriesAtScreenPoint(
                    *currentModel_, diagnosticSeriesViewport_, diagnosticSeriesRect_,
                    {point.X, point.Y}, 9.0);
                ::rlispstat::core::SelectDiagnosticPlotSeries(*currentModel_, series,
                    SelectionModeFor(event.KeyModifiers(), "replace") !=
                        ::rlispstat::core::SelectionMode::Replace);
                RenderPlot(std::max(360.0, plotCanvas_.ActualWidth()),
                           std::max(280.0, plotCanvas_.ActualHeight()));
                event.Handled(true);
                return;
            }
            if (!RowSelectionGestureEnabled() &&
                !ViewportNavigationGestureEnabled()) return;
            const auto point = current.Position();
            pointsCanvas_.Focus(FocusState::Programmatic);
            BeginSelection(point.X, point.Y, SelectionModeFor(
                event.KeyModifiers(), currentModel_ ? currentModel_->selectionMode : "replace"));
            pointsCanvas_.CapturePointer(event.Pointer());
            event.Handled(true);
        });
        pointsCanvas_.PointerMoved([this](auto const&, auto const& event)
        {
            if (!selecting_) return;
            const auto point = event.GetCurrentPoint(pointsCanvas_).Position();
            UpdateSelection(point.X, point.Y);
            event.Handled(true);
        });
        pointsCanvas_.PointerReleased([this](auto const&, auto const& event)
        {
            if (!selecting_) return;
            const auto point = event.GetCurrentPoint(pointsCanvas_).Position();
            CompleteSelection(point.X, point.Y, selectionMode_);
            pointsCanvas_.ReleasePointerCapture(event.Pointer());
            event.Handled(true);
        });
        pointsCanvas_.KeyDown([this](auto const&, auto const& event)
        {
            using winrt::Windows::System::VirtualKey;
            switch (event.Key())
            {
            case VirtualKey::A:
                ApplyVisibleSelection(::rlispstat::core::SelectionMode::Replace);
                break;
            case VirtualKey::V:
                ApplyVisibleSelection(::rlispstat::core::SelectionMode::Toggle);
                break;
            case VirtualKey::C:
            case VirtualKey::Escape:
                ClearSelection();
                break;
            default:
                return;
            }
            event.Handled(true);
        });
        if (!embedded_) ConfigureContextMenu();
        selectionRectangle_ = Shapes::Rectangle();
        selectionRectangle_.Stroke(Brush(38, 99, 235));
        selectionRectangle_.StrokeThickness(1.5);
        selectionRectangle_.Fill(Media::SolidColorBrush(
            winrt::Windows::UI::ColorHelper::FromArgb(48, 38, 99, 235)));
        selectionRectangle_.Visibility(Visibility::Collapsed);
        pointsCanvas_.Children().Append(selectionRectangle_);
        plotCanvas_.Children().Append(pointsCanvas_);
        root_.Children().Append(plotCanvas_);
        annotationCanvas_ = Controls::Canvas();
        annotationCanvas_.Background(nullptr);
        Controls::Grid::SetColumn(annotationCanvas_, 0);
        root_.Children().Append(annotationCanvas_);

        auto smoothPanel = Controls::StackPanel();
        smoothPanel.Orientation(Controls::Orientation::Horizontal);
        smoothPanel.Spacing(8);
        smoothSpanLabel_ = Controls::TextBlock();
        smoothSpanLabel_.Text(L"Span 0.75");
        smoothSpanLabel_.FontSize(11);
        smoothSpanLabel_.VerticalAlignment(VerticalAlignment::Center);
        smoothSpanSlider_ = Controls::Slider();
        smoothSpanSlider_.Minimum(0.20); smoothSpanSlider_.Maximum(2.00);
        smoothSpanSlider_.StepFrequency(0.05); smoothSpanSlider_.Width(160);
        smoothSpanSlider_.Value(0.75);
        smoothSpanSlider_.ValueChanged([this](auto const&, auto const&)
        {
            if (updatingSmoothSpan_ || !currentModel_) return;
            if (currentModel_->kind == "histogram") {
                const double value = smoothSpanSlider_.Value();
                smoothSpanLabel_.Text(to_hstring("Smoothing ×" + ::rlispstat::core::FormatDouble(value, 2)));
                if (commandCallback_ && !plotId_.empty())
                    commandCallback_({"HIST_SET_DENSITY_ADJUST", plotId_, std::to_string(value)});
                return;
            }
            const double value = ::rlispstat::core::ClampSmoothSpan(smoothSpanSlider_.Value());
            smoothSpanLabel_.Text(to_hstring("Span " +
                ::rlispstat::core::FormatDouble(value, 2)));
        });
        smoothSpanSlider_.PointerCaptureLost([this](auto const&, auto const&)
        {
            if (!currentModel_ || updatingSmoothSpan_) return;
            if (currentModel_->kind == "histogram") return;
            currentModel_->smoothSpan = ::rlispstat::core::ClampSmoothSpan(smoothSpanSlider_.Value());
            if (commandCallback_ && !plotId_.empty())
                commandCallback_({ "SET_SMOOTH_SPAN", plotId_,
                    std::to_string(currentModel_->smoothSpan) });
        });
        smoothSpanSlider_.KeyUp([this](auto const&, auto const&)
        {
            if (!currentModel_ || updatingSmoothSpan_) return;
            if (currentModel_->kind == "histogram") return;
            const double value = ::rlispstat::core::ClampSmoothSpan(smoothSpanSlider_.Value());
            if (std::abs(value - currentModel_->smoothSpan) < 1e-9) return;
            currentModel_->smoothSpan = value;
            if (commandCallback_ && !plotId_.empty())
                commandCallback_({ "SET_SMOOTH_SPAN", plotId_, std::to_string(value) });
        });
        smoothPanel.Children().Append(smoothSpanLabel_);
        smoothPanel.Children().Append(smoothSpanSlider_);
        smoothSpanControl_ = Controls::Border();
        smoothSpanControl_.Padding(Thickness{8, 3, 8, 3});
        smoothSpanControl_.CornerRadius(CornerRadius{6});
        smoothSpanControl_.Background(Brush(248, 248, 248));
        smoothSpanControl_.BorderBrush(Brush(220, 220, 220));
        smoothSpanControl_.BorderThickness(Thickness{1});
        smoothSpanControl_.Child(smoothPanel);
        smoothSpanControl_.HorizontalAlignment(HorizontalAlignment::Right);
        smoothSpanControl_.VerticalAlignment(VerticalAlignment::Bottom);
        smoothSpanControl_.Margin(Thickness{0, 0, 12, 10});
        smoothSpanControl_.Visibility(Visibility::Collapsed);
        Controls::Grid::SetColumn(smoothSpanControl_, 0);
        root_.Children().Append(smoothSpanControl_);

        auto pointSizePanel = Controls::StackPanel();
        pointSizePanel.Orientation(Controls::Orientation::Horizontal);
        pointSizePanel.Spacing(8);
        pointSizeLabel_ = Controls::TextBlock();
        pointSizeLabel_.Text(L"Point size 100%");
        pointSizeLabel_.FontSize(11);
        pointSizeLabel_.VerticalAlignment(VerticalAlignment::Center);
        pointSizeSlider_ = Controls::Slider();
        pointSizeSlider_.Minimum(0.50); pointSizeSlider_.Maximum(3.00);
        pointSizeSlider_.StepFrequency(0.10); pointSizeSlider_.Width(160);
        pointSizeSlider_.Value(1.00);
        pointSizeSlider_.ValueChanged([this](auto const&, auto const&)
        {
            const double value = ::rlispstat::core::ClampPointSizeScale(
                pointSizeSlider_.Value());
            pointSizeLabel_.Text(to_hstring("Point size " +
                std::to_string(static_cast<int>(std::lround(value * 100.0))) + "%"));
            if (updatingPointSize_) return;
            pointSizeScale_ = value;
            if (currentModel_) currentModel_->pointSizeScale = value;
            RenderPlot(std::max(360.0, plotCanvas_.ActualWidth()),
                       std::max(280.0, plotCanvas_.ActualHeight()));
        });
        pointSizePanel.Children().Append(pointSizeLabel_);
        pointSizePanel.Children().Append(pointSizeSlider_);
        pointSizeControl_ = Controls::Border();
        pointSizeControl_.Padding(Thickness{8, 3, 8, 3});
        pointSizeControl_.CornerRadius(CornerRadius{6});
        pointSizeControl_.Background(Brush(248, 248, 248));
        pointSizeControl_.BorderBrush(Brush(220, 220, 220));
        pointSizeControl_.BorderThickness(Thickness{1});
        pointSizeControl_.Child(pointSizePanel);
        pointSizeControl_.HorizontalAlignment(HorizontalAlignment::Left);
        pointSizeControl_.VerticalAlignment(VerticalAlignment::Bottom);
        pointSizeControl_.Margin(Thickness{12, 0, 0, 10});
        pointSizeControl_.Visibility(Visibility::Collapsed);
        Controls::Grid::SetColumn(pointSizeControl_, 0);
        root_.Children().Append(pointSizeControl_);
        // Kept in the plot window's visual tree so WebView2 can obtain a
        // composition host, but collapsed outside an export.  It renders the
        // canonical SVG independently of the visible XAML plot and therefore
        // adds no persistent redraw/composition cost to plot interaction.
        if (!embedded_)
        {
            exportWebView_ = Controls::WebView2();
            exportWebView_.Opacity(0.001);
            exportWebView_.Visibility(Visibility::Collapsed);
            exportWebView_.IsHitTestVisible(false);
            exportWebView_.HorizontalAlignment(HorizontalAlignment::Left);
            exportWebView_.VerticalAlignment(VerticalAlignment::Top);
            Controls::Grid::SetColumn(exportWebView_, 0);
            root_.Children().InsertAt(0, exportWebView_);
        }

        auto notesRoot = Controls::Grid();
        notesRoot.Padding(Thickness{12, 12, 12, 10});
        notesRoot.Width(270);
        auto notesTitleRow = Controls::RowDefinition();
        notesTitleRow.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Auto));
        auto notesBodyRow = Controls::RowDefinition();
        notesBodyRow.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
        auto notesFooterRow = Controls::RowDefinition();
        notesFooterRow.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Auto));
        notesRoot.RowDefinitions().Append(notesTitleRow);
        notesRoot.RowDefinitions().Append(notesBodyRow);
        notesRoot.RowDefinitions().Append(notesFooterRow);
        auto notesTitle = Controls::TextBlock();
        notesTitle.Text(L"Notes"); notesTitle.FontSize(15);
        notesTitle.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        notesTitle.Margin(Thickness{0, 0, 0, 8}); notesRoot.Children().Append(notesTitle);
        notesText_ = Controls::TextBox();
        notesText_.AcceptsReturn(true); notesText_.TextWrapping(TextWrapping::Wrap);
        notesText_.VerticalContentAlignment(VerticalAlignment::Top);
        Controls::Grid::SetRow(notesText_, 1); notesRoot.Children().Append(notesText_);
        includeNotes_ = Controls::CheckBox(); includeNotes_.Content(box_value(L"Include notes in export"));
        includeNotes_.Margin(Thickness{0, 8, 0, 0});
        Controls::Grid::SetRow(includeNotes_, 2); notesRoot.Children().Append(includeNotes_);
        notesPanel_ = Controls::Border();
        notesPanel_.BorderBrush(Brush(222, 224, 228));
        notesPanel_.BorderThickness(Thickness{1, 0, 0, 0});
        notesPanel_.Background(Brush(249, 249, 249)); notesPanel_.Child(notesRoot);
        notesPanel_.Visibility(Visibility::Collapsed);
        Controls::Grid::SetColumn(notesPanel_, 1); root_.Children().Append(notesPanel_);
        notesText_.TextChanged([this](auto const&, auto const&)
        {
            if (updatingNotes_) return;
            ::rlispstat::core::SetWindowNoteText(note_, to_string(notesText_.Text()));
            ConfigureContextMenu();
        });
        includeNotes_.Click([this](auto const&, auto const&)
        {
            const auto checked = includeNotes_.IsChecked();
            ::rlispstat::core::SetWindowNoteIncludedInExport(note_, checked && checked.Value());
        });
        resizeTimer_ = root_.DispatcherQueue().CreateTimer();
        resizeTimer_.Interval(std::chrono::milliseconds(16));
        resizeTimer_.IsRepeating(false);
        resizeTimer_.Tick([this](auto const&, auto const&)
        {
            if (!closed_) RenderPlot(pendingPlotWidth_, pendingPlotHeight_);
        });
        root_.SizeChanged([this](auto const&, SizeChangedEventArgs const& args)
        {
            ::rlispstat::windows::performance::Scope timing(
                "Plot.SizeChanged",
                "points=" + std::to_string(currentPlot_.points.size()));
            const double notesWidth = notesPanel_.Visibility() == Visibility::Visible ? 270.0 : 0.0;
            pendingPlotWidth_ = std::max(360.0, args.NewSize().Width - notesWidth);
            pendingPlotHeight_ = args.NewSize().Height;
            if (!resizeTimer_.IsRunning()) resizeTimer_.Start();
        });
        if (!embedded_)
        {
            window_.Content(root_);
            ResizeLinkEDAWindowClient(window_, 820, 620);
        }
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

    void ScatterPlotView::SetCommandCallback(CommandCallback callback)
    {
        commandCallback_ = std::move(callback);
        ConfigureContextMenu();
    }

    ::rlispstat::core::PlotModel ScatterPlotView::ExportModelSnapshot() const
    {
        ::rlispstat::core::PlotModel model;
        if (currentModel_) model = *currentModel_;
        else
        {
            model.id = currentPlot_.id;
            model.group = currentPlot_.group;
            model.kind = "scatter";
            model.xLabel = currentPlot_.xLabel;
            model.yLabel = currentPlot_.yLabel;
            model.title = currentPlot_.title;
            for (auto const& point : currentPlot_.points)
                model.points.push_back({ point.x, point.y, point.row });
        }
        model.pointSizeScale = pointSizeScale_;
        model.frozenAppearanceCaptured = true;
        model.frozenRowColors = pointColors_;
        model.frozenRowLabels = rowLabels_;
        return model;
    }

    Microsoft::UI::Xaml::FrameworkElement ScatterPlotView::ContentRoot() const
    {
        return root_;
    }

    void ScatterPlotView::ResizeSnapshotHost(double width, double height)
    {
        if (!embedded_) return;
        width = std::max(360.0, width);
        height = std::max(280.0, height);
        root_.Width(width);
        root_.Height(height);
        plotCanvas_.Width(width);
        plotCanvas_.Height(height);
        pendingPlotWidth_ = width;
        pendingPlotHeight_ = height;
        RenderPlot(width, height);
    }

    std::string ScatterPlotView::ExportSvgDocument() const
    {
        const auto model = ExportModelSnapshot();
        const double width = std::max(320.0, plotCanvas_.ActualWidth());
        const double height = std::max(240.0, plotCanvas_.ActualHeight());
        std::string svg = ::rlispstat::core::BuildSvgPlotSnapshotDocument(
            model, themeName_, model.title, width, height).svg;
        if (note_.include_in_export && note_.has_content)
            svg = ::rlispstat::core::ComposeSvgDocumentWithWindowNote(
                svg, { "Notes", note_.plain_text });
        if (!stickyNotes_.empty())
            svg = ::rlispstat::core::ComposeSvgDocumentWithWindowStickyNotes(svg, stickyNotes_);
        return svg;
    }

    Windows::Foundation::IAsyncOperation<Windows::Storage::Streams::InMemoryRandomAccessStream>
        ScatterPlotView::RenderSvgToPngAsync(std::string svg, double scale)
    {
        const double width = std::max(320.0, plotCanvas_.ActualWidth());
        const double height = std::max(240.0, plotCanvas_.ActualHeight());
        exportWebView_.Width(width * scale);
        exportWebView_.Height(height * scale);
        exportWebView_.Visibility(Visibility::Visible);
        co_await exportWebView_.EnsureCoreWebView2Async();
        const std::string html =
            "<!doctype html><html><head><meta charset=\"utf-8\"><style>"
            "html,body{margin:0;padding:0;overflow:hidden;background:white;}"
            "svg{display:block;width:100vw;height:100vh;}"
            "</style></head><body>" + svg + "</body></html>";
        winrt::handle completed(CreateEventW(nullptr, TRUE, FALSE, nullptr));
        event_token token{};
        token = exportWebView_.NavigationCompleted(
            [completed = completed.get()](auto const&, auto const&) { SetEvent(completed); });
        exportWebView_.NavigateToString(to_hstring(html));
        co_await winrt::resume_on_signal(completed.get());
        co_await wil::resume_foreground(root_.DispatcherQueue());
        exportWebView_.NavigationCompleted(token);
        Windows::Storage::Streams::InMemoryRandomAccessStream stream;
        co_await exportWebView_.CoreWebView2().CapturePreviewAsync(
            Microsoft::Web::WebView2::Core::CoreWebView2CapturePreviewImageFormat::Png,
            stream);
        exportWebView_.Visibility(Visibility::Collapsed);
        stream.Seek(0);
        co_return stream;
    }

    Windows::Foundation::IAsyncOperation<bool>
        ScatterPlotView::RenderSvgToPdfAsync(std::string svg, std::wstring path)
    {
        const double width = std::max(320.0, plotCanvas_.ActualWidth());
        const double height = std::max(240.0, plotCanvas_.ActualHeight());
        exportWebView_.Width(width);
        exportWebView_.Height(height);
        exportWebView_.Visibility(Visibility::Visible);
        co_await exportWebView_.EnsureCoreWebView2Async();
        const std::string html =
            "<!doctype html><html><head><meta charset=\"utf-8\"><style>"
            "@page{margin:0;size:" + std::to_string(width * 0.75) + "pt " +
            std::to_string(height * 0.75) + "pt;}"
            "html,body{margin:0;padding:0;overflow:hidden;background:white;}"
            "svg{display:block;width:100vw;height:100vh;}"
            "</style></head><body>" + svg + "</body></html>";
        winrt::handle completed(CreateEventW(nullptr, TRUE, FALSE, nullptr));
        event_token token{};
        token = exportWebView_.NavigationCompleted(
            [completed = completed.get()](auto const&, auto const&) { SetEvent(completed); });
        exportWebView_.NavigateToString(to_hstring(html));
        co_await winrt::resume_on_signal(completed.get());
        co_await wil::resume_foreground(root_.DispatcherQueue());
        exportWebView_.NavigationCompleted(token);
        auto core = exportWebView_.CoreWebView2();
        auto settings = core.Environment().CreatePrintSettings();
        settings.ShouldPrintBackgrounds(true);
        settings.ShouldPrintHeaderAndFooter(false);
        settings.MarginTop(0); settings.MarginRight(0);
        settings.MarginBottom(0); settings.MarginLeft(0);
        settings.PageWidth(width / 96.0);
        settings.PageHeight(height / 96.0);
        const bool result = co_await core.PrintToPdfAsync(path, settings);
        exportWebView_.Visibility(Visibility::Collapsed);
        co_return result;
    }

    fire_and_forget ScatterPlotView::CopyPlotAsSvgAsync()
    {
        auto lifetime = shared_from_this();
        // SVG has no universally honoured standard Windows clipboard slot.
        // Publish it together with the same proven bitmap fallback as the PNG
        // command so Word, PowerPoint and ordinary image clients all paste a
        // visible plot while vector-aware clients can retain the SVG payload.
        CopyPlotRichAsync(false);
        co_return;
    }

    fire_and_forget ScatterPlotView::CopyPlotAsPngAsync()
    {
        auto lifetime = shared_from_this();
        try
        {
            auto stream = co_await RenderSvgToPngAsync(ExportSvgDocument(), 2.0);
            Windows::ApplicationModel::DataTransfer::DataPackage package;
            package.SetBitmap(
                Windows::Storage::Streams::RandomAccessStreamReference::CreateFromStream(stream));
            Windows::ApplicationModel::DataTransfer::Clipboard::SetContent(package);
            Windows::ApplicationModel::DataTransfer::Clipboard::Flush();
        }
        catch (hresult_error const& error)
        {
            exportWebView_.Visibility(Visibility::Collapsed);
            MessageBoxW(WindowHandle(window_), error.message().c_str(), L"Copy Plot",
                        MB_OK | MB_ICONERROR);
        }
    }

    fire_and_forget ScatterPlotView::CopyPlotRichAsync(bool preferVector)
    {
        auto lifetime = shared_from_this();
        try
        {
            const std::string svg = ExportSvgDocument();
            const ::rlispstat::core::ExportDimensions dimensions{
                std::max(320.0, plotCanvas_.ActualWidth()),
                std::max(240.0, plotCanvas_.ActualHeight()), 2.0 };
            std::string error;
            if (!::rlispstat::platform::windows::CopyPlotWithOfficeClipboardFormats(
                    ExportModelSnapshot(), dimensions, svg, !preferVector, &error))
                throw hresult_error(E_FAIL, to_hstring(error));
        }
        catch (hresult_error const& error)
        {
            MessageBoxW(WindowHandle(window_), error.message().c_str(), L"Copy Plot",
                        MB_OK | MB_ICONERROR);
        }
        co_return;
    }

    fire_and_forget ScatterPlotView::SavePlotAsync(std::string format)
    {
        auto lifetime = shared_from_this();
        const auto path = ChoosePlotExportPath(window_, plotId_, format);
        if (!path) { Activate(); co_return; }
        bool saved = false;
        try
        {
            if (format == "SVG")
            {
                saved = WriteTextFile(*path, ExportSvgDocument());
            }
            else if (format == "PDF")
            {
                saved = co_await RenderSvgToPdfAsync(ExportSvgDocument(), *path);
            }
            else
            {
                auto stream = co_await RenderSvgToPngAsync(ExportSvgDocument(), 2.0);
                const uint32_t length = static_cast<uint32_t>(stream.Size());
                Windows::Storage::Streams::DataReader reader(stream.GetInputStreamAt(0));
                co_await reader.LoadAsync(length);
                std::vector<uint8_t> bytes(length);
                reader.ReadBytes(bytes);
                saved = WriteBinaryFile(*path, bytes);
            }
        }
        catch (hresult_error const&)
        {
            exportWebView_.Visibility(Visibility::Collapsed);
            saved = false;
        }
        if (!saved)
            MessageBoxW(WindowHandle(window_),
                to_hstring(::rlispstat::core::PlotExportFailedStatus()).c_str(),
                to_hstring(::rlispstat::core::PlotExportFailedTitle()).c_str(),
                MB_OK | MB_ICONERROR);
        Activate();
    }

    void ScatterPlotView::DispatchPlotCommand(std::vector<std::string> command)
    {
        if (!commandCallback_ || plotId_.empty() || command.empty()) return;
        auto dispatch = [weak = weak_from_this(), command = std::move(command)]()
        {
            auto self = weak.lock();
            if (!self || self->closed_ || !self->commandCallback_) return;
            self->commandCallback_(command);
        };
        // Context-menu commands frequently rebuild the owning plot. Performing
        // that rebuild inside MenuFlyoutItem::Click can strand WinUI's popup
        // input shield above the new canvas. Dispatch after the flyout closes.
        auto dispatcher = DispatcherQueue();
        if (!dispatcher || !dispatcher.TryEnqueue(dispatch)) dispatch();
    }

    void ScatterPlotView::DispatchEncodedPlotCommand(
        std::string const& encodedCommand)
    {
        if (!commandCallback_ || plotId_.empty() || encodedCommand.empty()) return;
        const auto parsed = ::rlispstat::core::ParseMenuCommand(encodedCommand);
        if (parsed.name.empty()) return;
        std::vector<std::string> command{ parsed.name, plotId_ };
        command.insert(command.end(), parsed.args.begin(), parsed.args.end());
        DispatchPlotCommand(std::move(command));
    }

    Controls::MenuFlyoutItemBase ScatterPlotView::CreateBarplotMenuItem(
        ::rlispstat::core::BarplotMenuOption const& option,
        bool showCheck)
    {
        const auto click = [this, command = option.command](auto const&, auto const&)
        {
            DispatchEncodedPlotCommand(command);
        };
        if (showCheck)
        {
            auto item = Controls::ToggleMenuFlyoutItem();
            item.Text(to_hstring(option.title));
            item.IsChecked(option.checked);
            item.IsEnabled(option.enabled);
            if (!option.command.empty()) item.Click(click);
            return item;
        }
        auto item = Controls::MenuFlyoutItem();
        item.Text(to_hstring(option.title));
        item.IsEnabled(option.enabled);
        if (!option.command.empty()) item.Click(click);
        return item;
    }

    void ScatterPlotView::SelectBarplotRows(
        std::vector<int> const& rows,
        ::rlispstat::core::SelectionMode mode)
    {
        if (!selectionCallback_ || group_.empty()) return;
        selectionCallback_(group_, std::set<int>(rows.begin(), rows.end()), mode);
    }

    void ScatterPlotView::ConfigureContextMenu()
    {
        if (embedded_) return;
        const bool aggregateDiagnostic = currentModel_ &&
            (::rlispstat::core::PlotIsAggregateDiagnostic(*currentModel_) ||
             ::rlispstat::core::PlotIsImputationDiagnostic(*currentModel_));
        auto menu = Controls::MenuFlyout();
        auto variablesMenu = Controls::MenuFlyoutSubItem();
        variablesMenu.Text(L"Variables");
        auto conditioningMenu = Controls::MenuFlyoutSubItem();
        conditioningMenu.Text(L"Conditioning variables");
        auto displayMenu = Controls::MenuFlyoutSubItem();
        displayMenu.Text(L"Display");
        Controls::MenuFlyoutSubItem colorOverrideMenu{ nullptr };
        auto commandItem = [this](std::wstring const& title, std::string command,
                                  bool checked = false, std::string argument = {})
            -> Controls::MenuFlyoutItemBase
        {
            auto click = [this, command = std::move(command),
                          argument = std::move(argument)](auto const&, auto const&)
            {
                if (argument.empty()) DispatchPlotCommand({ command, plotId_ });
                else DispatchPlotCommand({ command, plotId_, argument });
            };
            if (checked)
            {
                auto item = Controls::ToggleMenuFlyoutItem();
                item.Text(title); item.IsChecked(true); item.Click(click);
                return item;
            }
            auto item = Controls::MenuFlyoutItem();
            item.Text(title); item.Click(click);
            return item;
        };
        if (currentModel_ && !aggregateDiagnostic &&
            (currentModel_->kind == "scatter" ||
             (currentModel_->kind == "trellis_scatterplot" &&
              currentModel_->trellisSpecification.plotType == ::rlispstat::core::TrellisPlotType::Scatter)))
        {
            displayMenu.Items().Append(commandItem(L"Shade overlapping points",
                "SCATTER_TOGGLE_OVERLAP_SHADING", currentModel_->scatterShadeOverlap));
            displayMenu.Items().Append(commandItem(L"Size points by overlap",
                "SCATTER_TOGGLE_OVERLAP_SIZE", currentModel_->scatterSizeByOverlap));
        }
        auto commandPairItem = [this](std::wstring const& title, std::string command,
                                      std::string first, std::string second,
                                      bool checked = false)
            -> Controls::MenuFlyoutItemBase
        {
            auto click = [this, command = std::move(command), first = std::move(first),
                          second = std::move(second)](auto const&, auto const&)
            {
                DispatchPlotCommand({command, plotId_, first, second});
            };
            if (checked)
            {
                auto item = Controls::ToggleMenuFlyoutItem();
                item.Text(title); item.IsChecked(true); item.Click(click);
                return item;
            }
            auto item = Controls::MenuFlyoutItem();
            item.Text(title); item.Click(click);
            return item;
        };
        auto appendFitAndSmoothMenu = [&]()
        {
            auto fit = Controls::MenuFlyoutSubItem();
            fit.Text(L"Regression lines");
            const bool hasAll = currentModel_ &&
                ::rlispstat::core::HasOverlaySource(*currentModel_, "all");
            const bool hasSelected = currentModel_ &&
                ::rlispstat::core::HasOverlaySource(*currentModel_, "selected");
            const bool hasColor = currentModel_ &&
                ::rlispstat::core::HasOverlaySource(*currentModel_, "color");
            fit.Items().Append(commandItem(L"Linear regression - all data",
                "TOGGLE_LM_ALL", hasAll));
            fit.Items().Append(commandItem(L"Linear regression - selected data",
                "TOGGLE_LM_SELECTED", hasSelected));
            fit.Items().Append(commandItem(L"Linear regression - both",
                "TOGGLE_LM_BOTH", hasAll && hasSelected));
            fit.Items().Append(commandItem(L"Linear regression - by point color",
                "TOGGLE_LM_COLOR", hasColor));
            if (currentModel_ && (currentModel_->kind == "scatter" ||
                (currentModel_->kind == "trellis_scatterplot" &&
                 currentModel_->trellisSpecificationInitialized &&
                 currentModel_->trellisSpecification.plotType ==
                    ::rlispstat::core::TrellisPlotType::Scatter)))
            {
                fit.Items().Append(commandItem(L"Show confidence intervals",
                    "PLOT_REGRESSION_TOGGLE_CONFIDENCE_INTERVALS",
                    currentModel_->scatterFitConfidenceIntervalsVisible));
            }
            fit.Items().Append(Controls::MenuFlyoutSeparator());
            fit.Items().Append(commandItem(L"Remove all regression lines", "CLEAR_OVERLAYS"));
            displayMenu.Items().Append(fit);

            auto smooth = Controls::MenuFlyoutSubItem();
            smooth.Text(currentModel_ && currentModel_->isGLMDiagnostic
                ? L"Smooth lines" : L"Smooth curves");
            const auto hasSmooth = [this](::rlispstat::core::SmoothCurveScope scope)
            {
                return currentModel_ && ::rlispstat::core::SmoothCurveScopeIsPresent(
                    currentModel_->smoothCurves, scope);
            };
            smooth.Items().Append(commandItem(L"Smooth curve - overall",
                "TOGGLE_SMOOTH_OVERALL",
                hasSmooth(::rlispstat::core::SmoothCurveScope::Overall)));
            smooth.Items().Append(commandItem(L"Smooth curve - selected data",
                "TOGGLE_SMOOTH_SELECTED",
                hasSmooth(::rlispstat::core::SmoothCurveScope::Selection)));
            smooth.Items().Append(commandItem(L"Smooth curve - by point color",
                "TOGGLE_SMOOTH_COLOR",
                hasSmooth(::rlispstat::core::SmoothCurveScope::ColorGroup)));
            smooth.Items().Append(Controls::MenuFlyoutSeparator());
            smooth.Items().Append(commandItem(L"Show confidence intervals",
                "PLOT_SMOOTH_TOGGLE_CONFIDENCE_INTERVALS",
                currentModel_ &&
                    currentModel_->scatterSmoothConfidenceIntervalsVisible));
            smooth.Items().Append(Controls::MenuFlyoutSeparator());
            auto span = Controls::MenuFlyoutItem();
            span.Text(currentModel_ && currentModel_->smoothShowSpanSlider
                ? L"Hide smoothing slider" : L"Show smoothing slider");
            span.Click([this](auto const&, auto const&)
            {
                if (!currentModel_ || !smoothSpanControl_) return;
                currentModel_->smoothShowSpanSlider =
                    !currentModel_->smoothShowSpanSlider;
                smoothSpanControl_.Visibility(currentModel_->smoothShowSpanSlider
                    ? Visibility::Visible : Visibility::Collapsed);
                ConfigureContextMenu();
            });
            smooth.Items().Append(span);
            displayMenu.Items().Append(smooth);
        };
        const bool supportsCaseSplit = currentModel_ && hasDataFrame_ &&
            (currentModel_->kind == "scatter" || currentModel_->kind == "scatter_matrix" ||
             currentModel_->kind == "histogram" || currentModel_->kind == "barplot" ||
             currentModel_->kind == "boxplot");
        if (supportsCaseSplit)
        {
            colorOverrideMenu = Controls::MenuFlyoutSubItem();
            colorOverrideMenu.Text(L"Override colors by");
            colorOverrideMenu.Items().Append(commandItem(L"Cancel color override", "PLOT_COLOR_BY",
                currentModel_->colorByVariable.empty(), "."));
            for (auto const& column : dataFrame_.columns)
            {
                const bool displayed = column.name == currentModel_->xLabel ||
                    column.name == currentModel_->yLabel ||
                    std::find(currentModel_->boxplotVariables.begin(),
                        currentModel_->boxplotVariables.end(), column.name) !=
                        currentModel_->boxplotVariables.end() ||
                    std::find(currentModel_->scatterMatrixVariables.begin(),
                        currentModel_->scatterMatrixVariables.end(), column.name) !=
                        currentModel_->scatterMatrixVariables.end() ||
                    std::find(currentModel_->barplotXVariables.begin(),
                        currentModel_->barplotXVariables.end(), column.name) !=
                        currentModel_->barplotXVariables.end() ||
                    column.name == currentModel_->barplotSplitVariable;
                if (!displayed && ::rlispstat::core::TrellisConditionColumnIsValid(
                        column, dataFrame_.rows))
                    colorOverrideMenu.Items().Append(commandItem(to_hstring(column.name).c_str(),
                        "PLOT_COLOR_BY", currentModel_->colorByVariable == column.name,
                        column.name));
            }
        }
        const bool supportsConditioning = currentModel_ && hasDataFrame_ &&
            (currentModel_->kind == "scatter" || currentModel_->kind == "time_series" ||
             currentModel_->kind == "histogram" ||
             (currentModel_->kind == "barplot" &&
              currentModel_->barplotXVariables.size() <= 1) ||
             (currentModel_->kind == "boxplot" &&
              !::rlispstat::core::BoxplotUsesVariableAxes(*currentModel_)));
        if (supportsConditioning)
        {
            auto add = Controls::MenuFlyoutSubItem(); add.Text(L"Add conditioning variable");
            auto categorical = Controls::MenuFlyoutSubItem(); categorical.Text(L"Categorical");
            auto numeric = Controls::MenuFlyoutSubItem(); numeric.Text(L"Numeric");
            for (auto const& column : dataFrame_.columns)
            {
                const bool displayed = column.name == currentModel_->xLabel ||
                    column.name == currentModel_->yLabel ||
                    column.name == currentModel_->barplotSplitVariable ||
                    std::find(currentModel_->barplotXVariables.begin(),
                        currentModel_->barplotXVariables.end(), column.name) !=
                        currentModel_->barplotXVariables.end() ||
                    std::find(currentModel_->boxplotGroupingVariables.begin(),
                        currentModel_->boxplotGroupingVariables.end(), column.name) !=
                        currentModel_->boxplotGroupingVariables.end();
                if (displayed) continue;
                if (::rlispstat::core::NormalizeVariableType(column.type) == "numeric" &&
                    ::rlispstat::core::DataColumnAllowsNumeric(column))
                {
                    auto methods = Controls::MenuFlyoutSubItem();
                    methods.Text(to_hstring(column.name));
                    methods.Items().Append(commandItem(L"Categorical", "PLOT_CONDITION_CATEGORICAL",
                        false, column.name));
                    methods.Items().Append(commandItem(L"Ordinal", "PLOT_CONDITION_ORDERED",
                        false, column.name));
                    methods.Items().Append(Controls::MenuFlyoutSeparator());
                    methods.Items().Append(commandItem(L"Equal-width intervals",
                        "PLOT_CONDITION_EQUAL_WIDTH", false, column.name));
                    methods.Items().Append(commandItem(L"Equal-count intervals",
                        "PLOT_CONDITION_EQUAL_COUNT", false, column.name));
                    numeric.Items().Append(methods);
                }
                else if (::rlispstat::core::DataColumnLooksGroupingCandidate(
                             column, dataFrame_.rows))
                    categorical.Items().Append(commandItem(to_hstring(column.name).c_str(),
                        "PLOT_CONDITION_CATEGORICAL", false, column.name));
            }
            AppendMenuGroupWithoutRedundantSingleLevel(add, categorical);
            AppendMenuGroupWithoutRedundantSingleLevel(add, numeric);
            if (add.Items().Size() > 0)
                conditioningMenu.Items().Append(add);
        }
        if (currentModel_ && currentModel_->kind == "time_series")
        {
            auto xAxis = Controls::MenuFlyoutSubItem(); xAxis.Text(L"Time variable");
            auto yAxis = Controls::MenuFlyoutSubItem(); yAxis.Text(L"Value variable");
            auto groups = Controls::MenuFlyoutSubItem(); groups.Text(L"Series variable");
            groups.Items().Append(commandItem(L"None", "TIME_SERIES_SET_GROUP",
                currentModel_->timeSeriesGroupVariable.empty(), "."));
            for (auto const& variable : currentModel_->variables)
            {
                xAxis.Items().Append(commandItem(to_hstring(variable.name).c_str(),
                    "TIME_SERIES_SET_X", variable.name == currentModel_->xLabel, variable.name));
                yAxis.Items().Append(commandItem(to_hstring(variable.name).c_str(),
                    "TIME_SERIES_SET_Y", variable.name == currentModel_->yLabel, variable.name));
                groups.Items().Append(commandItem(to_hstring(variable.name).c_str(),
                    "TIME_SERIES_SET_GROUP", variable.name == currentModel_->timeSeriesGroupVariable, variable.name));
            }
            for (auto const& variable : currentModel_->variableMeta)
            {
                if (::rlispstat::core::FindNumericVariable(currentModel_->variables, variable.name)) continue;
                xAxis.Items().Append(commandItem(to_hstring(variable.name).c_str(),
                    "TIME_SERIES_SET_X", variable.name == currentModel_->xLabel, variable.name));
                groups.Items().Append(commandItem(to_hstring(variable.name).c_str(),
                    "TIME_SERIES_SET_GROUP", variable.name == currentModel_->timeSeriesGroupVariable, variable.name));
            }
            if (hasDataFrame_ && !::rlispstat::core::PlotIsImputationDiagnostic(*currentModel_)) {
                variablesMenu.Items().Append(xAxis);
                variablesMenu.Items().Append(yAxis);
                variablesMenu.Items().Append(groups);
            }
            auto explain = Controls::MenuFlyoutItem(); explain.Text(L"Explain plot");
            explain.Click([this](auto const&, auto const&) {
                if(currentModel_) ShowInformation("Explain plot",::rlispstat::core::TimeSeriesPlotExplanation(*currentModel_));
            });
            menu.Items().Append(explain);
            if(currentModel_->imputationDiagnosticTotal>0) {
                auto batches=Controls::MenuFlyoutSubItem();batches.Text(L"Imputations shown");
                for(const auto& option : ::rlispstat::core::ImputationDiagnosticBatchOptions(*currentModel_)) {
                    auto item=Controls::ToggleMenuFlyoutItem();item.Text(to_hstring(option.second));
                    item.IsChecked(option.first==currentModel_->imputationDiagnosticFirst);
                    item.Click([this,first=option.first](auto const&,auto const&) {
                        if(commandCallback_ && currentModel_) commandCallback_({"DATA_IMPUTATION_DIAGNOSTICS",
                            currentModel_->codeReference.provenance.dataVersion.datasetId,"distributions",
                            currentModel_->imputationDiagnosticVariable,std::to_string(first)});
                    });batches.Items().Append(item);
                }menu.Items().Append(batches);
            }
            auto identify = Controls::MenuFlyoutSubItem(); identify.Text(L"Series identification");
            for (auto const& pair : std::array<std::pair<std::string, std::wstring>, 3>{{
                {"legend", L"Legend"}, {"start_labels", L"Labels at start"}, {"none", L"None"}}})
                identify.Items().Append(commandItem(pair.second, "TIME_SERIES_SET_IDENTIFICATION",
                    currentModel_->timeSeriesIdentification == pair.first, pair.first));
            if (!currentModel_->timeSeriesGroupVariable.empty()) {
                displayMenu.Items().Append(identify);
                auto positions=Controls::MenuFlyoutSubItem(); positions.Text(L"Legend position");
                for (auto const& choice : std::vector<std::pair<std::string,std::wstring>>{
                    {"left",L"Left"},{"right",L"Right"},{"top",L"Top"},{"bottom",L"Bottom"},
                    {"top_left",L"Top left"},{"top_right",L"Top right"},
                    {"bottom_left",L"Bottom left"},{"bottom_right",L"Bottom right"}})
                    positions.Items().Append(commandItem(choice.second,"TIME_SERIES_SET_LEGEND_POSITION",
                        !currentModel_->interactionLegendUsesCustomPosition && currentModel_->timeSeriesIdentification=="legend" &&
                        currentModel_->timeSeriesLegendPosition==choice.first,choice.first));
                displayMenu.Items().Append(positions);
            }
        }
        else if (currentModel_ && currentModel_->kind == "trellis_scatterplot")
        {
            ::rlispstat::core::InitializeTrellisSpecificationFromLegacy(*currentModel_);
            auto const& specification = currentModel_->trellisSpecification;
            auto plotType = Controls::MenuFlyoutSubItem(); plotType.Text(L"Plot type");
            for (auto const& pair : std::array<std::pair<std::string, std::wstring>, 5>{{
                {"scatter", L"Scatterplot"}, {"time_series", L"Time series"},
                {"boxplot", L"Boxplot"}, {"bar", L"Bar chart"}, {"histogram", L"Histogram"}}})
                plotType.Items().Append(commandItem(pair.second, "TRELLIS_SCATTERPLOT_SET_TYPE",
                    ::rlispstat::core::TrellisPlotTypeName(specification.plotType) == pair.first, pair.first));
            displayMenu.Items().Append(plotType);
            auto xAxis = Controls::MenuFlyoutSubItem(); xAxis.Text(L"X variable");
            auto yAxis = Controls::MenuFlyoutSubItem(); yAxis.Text(L"Y variable");
            for (auto const& variable : currentModel_->variables)
            {
                xAxis.Items().Append(commandItem(to_hstring(variable.name).c_str(),
                    "TRELLIS_SCATTERPLOT_SET_X", variable.name == specification.xVariableId, variable.name));
                yAxis.Items().Append(commandItem(to_hstring(variable.name).c_str(),
                    "TRELLIS_SCATTERPLOT_SET_Y", variable.name == specification.yVariableId, variable.name));
            }
            variablesMenu.Items().Append(xAxis);
            if (specification.plotType == ::rlispstat::core::TrellisPlotType::Scatter ||
                specification.plotType == ::rlispstat::core::TrellisPlotType::TimeSeries ||
                specification.plotType == ::rlispstat::core::TrellisPlotType::Boxplot)
                variablesMenu.Items().Append(yAxis);
            if (specification.plotType == ::rlispstat::core::TrellisPlotType::Boxplot && hasDataFrame_)
            {
                std::vector<std::string> groups = specification.boxplotGroupingVariableIds;
                if (groups.empty()) groups.push_back(specification.xVariableId);
                auto grouping = Controls::MenuFlyoutSubItem();
                grouping.Text(L"Boxplot grouping variables");
                auto addGroup = Controls::MenuFlyoutSubItem(); addGroup.Text(L"Add inner level");
                for (auto const& column : dataFrame_.columns)
                {
                    const bool used = std::find(groups.begin(), groups.end(), column.name) != groups.end();
                    if (!used && column.name != specification.yVariableId &&
                        ::rlispstat::core::DataColumnLooksGroupingCandidate(column, dataFrame_.rows))
                        addGroup.Items().Append(commandItem(to_hstring(column.name).c_str(),
                            "TRELLIS_SCATTERPLOT_BOXPLOT_ADD_GROUP", false, column.name));
                }
                if (addGroup.Items().Size() > 0) grouping.Items().Append(addGroup);
                for (std::size_t index = 0; index < groups.size(); ++index)
                {
                    auto current = Controls::MenuFlyoutSubItem(); current.Text(to_hstring(groups[index]));
                    auto outward = commandItem(L"Move outward",
                        "TRELLIS_SCATTERPLOT_BOXPLOT_MOVE_GROUP_EARLIER", false, groups[index]);
                    outward.IsEnabled(index > 0); current.Items().Append(outward);
                    auto inward = commandItem(L"Move inward",
                        "TRELLIS_SCATTERPLOT_BOXPLOT_MOVE_GROUP_LATER", false, groups[index]);
                    inward.IsEnabled(index + 1 < groups.size()); current.Items().Append(inward);
                    if (groups.size() > 1)
                    {
                        current.Items().Append(Controls::MenuFlyoutSeparator());
                        current.Items().Append(commandItem(L"Remove",
                            "TRELLIS_SCATTERPLOT_BOXPLOT_REMOVE_GROUP", false, groups[index]));
                    }
                    grouping.Items().Append(current);
                }
                variablesMenu.Items().Append(grouping);

            }
            auto addCondition = Controls::MenuFlyoutSubItem(); addCondition.Text(L"Add conditioning variable");
            auto addCategorical = Controls::MenuFlyoutSubItem(); addCategorical.Text(L"Categorical");
            auto addNumeric = Controls::MenuFlyoutSubItem(); addNumeric.Text(L"Numeric");
            for (auto const& column : dataFrame_.columns)
            {
                const bool used = std::any_of(specification.conditioningVariables.begin(),
                    specification.conditioningVariables.end(), [&](auto const& condition)
                    { return condition.variableId == column.name; });
                const bool axisVariable = specification.plotType !=
                        ::rlispstat::core::TrellisPlotType::DataTable &&
                    (column.name == specification.xVariableId || column.name == specification.yVariableId);
                if (used || axisVariable) continue;
                if (::rlispstat::core::NormalizeVariableType(column.type) == "numeric" &&
                    ::rlispstat::core::DataColumnAllowsNumeric(column))
                {
                    auto methods = Controls::MenuFlyoutSubItem(); methods.Text(to_hstring(column.name));
                    methods.Items().Append(commandItem(L"Categorical",
                        "TRELLIS_SCATTERPLOT_ADD_CONDITION_CATEGORICAL", false, column.name));
                    methods.Items().Append(commandItem(L"Ordinal",
                        "TRELLIS_SCATTERPLOT_ADD_CONDITION_ORDERED", false, column.name));
                    methods.Items().Append(Controls::MenuFlyoutSeparator());
                    methods.Items().Append(commandItem(L"Equal-width intervals",
                        "TRELLIS_SCATTERPLOT_ADD_CONDITION_EQUAL_WIDTH", false, column.name));
                    methods.Items().Append(commandItem(L"Equal-count intervals",
                        "TRELLIS_SCATTERPLOT_ADD_CONDITION_EQUAL_COUNT", false, column.name));
                    addNumeric.Items().Append(methods);
                }
                else if (::rlispstat::core::DataColumnLooksGroupingCandidate(
                             column, dataFrame_.rows))
                    addCategorical.Items().Append(commandItem(to_hstring(column.name).c_str(),
                        "TRELLIS_SCATTERPLOT_ADD_CONDITION_CATEGORICAL", false, column.name));
            }
            AppendMenuGroupWithoutRedundantSingleLevel(addCondition, addCategorical);
            AppendMenuGroupWithoutRedundantSingleLevel(addCondition, addNumeric);
            if (addCondition.Items().Size() > 0) conditioningMenu.Items().Append(addCondition);

            for (std::size_t index = 0; index < specification.conditioningVariables.size(); ++index)
            {
                auto const& condition = specification.conditioningVariables[index];
                auto current = Controls::MenuFlyoutSubItem();
                std::wstring placement = condition.dimension == ::rlispstat::core::TrellisDimension::Rows
                    ? L"Rows" : condition.dimension == ::rlispstat::core::TrellisDimension::Columns
                        ? L"Columns" : L"Nested";
                current.Text(to_hstring(condition.variableLabel) + L" \u2014 " + placement);

                auto replace = Controls::MenuFlyoutSubItem(); replace.Text(L"Replace with");
                for (auto const& column : dataFrame_.columns)
                {
                    if (column.name == condition.variableId) continue;
                    const bool usedElsewhere = std::any_of(specification.conditioningVariables.begin(),
                        specification.conditioningVariables.end(), [&](auto const& other)
                        { return other.variableId != condition.variableId && other.variableId == column.name; });
                    const bool axisVariable = specification.plotType !=
                            ::rlispstat::core::TrellisPlotType::DataTable &&
                        (column.name == specification.xVariableId || column.name == specification.yVariableId);
                    const bool compatible = condition.kind ==
                            ::rlispstat::core::TrellisConditioningVariableKind::ContinuousBinned
                        ? (::rlispstat::core::NormalizeVariableType(column.type) == "numeric" &&
                           ::rlispstat::core::DataColumnAllowsNumeric(column))
                        : ::rlispstat::core::TrellisConditionColumnIsValid(column, dataFrame_.rows);
                    if (!usedElsewhere && !axisVariable && compatible)
                        replace.Items().Append(commandPairItem(to_hstring(column.name).c_str(),
                            "TRELLIS_SCATTERPLOT_SET_CONDITION", condition.variableId, column.name));
                }
                replace.IsEnabled(replace.Items().Size() > 0);
                current.Items().Append(replace);

                auto dimension = Controls::MenuFlyoutSubItem(); dimension.Text(L"Place in");
                dimension.Items().Append(commandItem(L"Rows", "TRELLIS_SCATTERPLOT_DIMENSION_ROWS",
                    condition.dimension == ::rlispstat::core::TrellisDimension::Rows, condition.variableId));
                dimension.Items().Append(commandItem(L"Columns", "TRELLIS_SCATTERPLOT_DIMENSION_COLUMNS",
                    condition.dimension == ::rlispstat::core::TrellisDimension::Columns, condition.variableId));
                dimension.Items().Append(commandItem(L"Nested", "TRELLIS_SCATTERPLOT_DIMENSION_NESTED",
                    condition.dimension == ::rlispstat::core::TrellisDimension::Nested, condition.variableId));
                current.Items().Append(dimension);

                auto column = std::find_if(dataFrame_.columns.begin(), dataFrame_.columns.end(),
                    [&](auto const& item) { return item.name == condition.variableId; });
                const bool numeric = column != dataFrame_.columns.end() &&
                    ::rlispstat::core::NormalizeVariableType(column->type) == "numeric";
                if (numeric)
                {
                    auto interpretation = Controls::MenuFlyoutSubItem(); interpretation.Text(L"Interpret as");
                    const auto binning = condition.binning.value_or(
                        ::rlispstat::core::TrellisContinuousBinningSpecification{});
                    interpretation.Items().Append(commandItem(L"Categorical",
                        "TRELLIS_SCATTERPLOT_SET_CONDITION_FACTOR",
                        condition.kind == ::rlispstat::core::TrellisConditioningVariableKind::Categorical &&
                            !condition.orderedCategories, condition.variableId));
                    interpretation.Items().Append(commandItem(L"Ordinal",
                        "TRELLIS_SCATTERPLOT_SET_CONDITION_ORDERED",
                        condition.kind == ::rlispstat::core::TrellisConditioningVariableKind::Categorical &&
                            condition.orderedCategories, condition.variableId));
                    interpretation.Items().Append(commandItem(L"Equal-width intervals",
                        "TRELLIS_SCATTERPLOT_SET_CONDITION_EQUAL_WIDTH",
                        condition.kind == ::rlispstat::core::TrellisConditioningVariableKind::ContinuousBinned &&
                            binning.method == ::rlispstat::core::TrellisContinuousBinningMethod::EqualWidth,
                        condition.variableId));
                    interpretation.Items().Append(commandItem(L"Equal-count intervals",
                        "TRELLIS_SCATTERPLOT_SET_CONDITION_EQUAL_COUNT",
                        condition.kind == ::rlispstat::core::TrellisConditioningVariableKind::ContinuousBinned &&
                            binning.method == ::rlispstat::core::TrellisContinuousBinningMethod::EqualCount,
                        condition.variableId));
                    current.Items().Append(interpretation);
                    auto bins = Controls::MenuFlyoutSubItem(); bins.Text(L"Number of bins");
                    bins.IsEnabled(condition.kind ==
                        ::rlispstat::core::TrellisConditioningVariableKind::ContinuousBinned);
                    for (std::size_t count = 2; count <= 6; ++count)
                        bins.Items().Append(commandPairItem(to_hstring(count).c_str(),
                            "TRELLIS_SCATTERPLOT_CONDITION_BINS", condition.variableId,
                            std::to_string(count), binning.binCount == count));
                    current.Items().Append(bins);
                }

                current.Items().Append(Controls::MenuFlyoutSeparator());
                auto earlier = commandItem(L"Move earlier",
                    "TRELLIS_SCATTERPLOT_MOVE_CONDITION_EARLIER", false, condition.variableId);
                earlier.IsEnabled(index > 0); current.Items().Append(earlier);
                auto later = commandItem(L"Move later",
                    "TRELLIS_SCATTERPLOT_MOVE_CONDITION_LATER", false, condition.variableId);
                later.IsEnabled(index + 1 < specification.conditioningVariables.size());
                current.Items().Append(later);
                current.Items().Append(Controls::MenuFlyoutSeparator());
                current.Items().Append(commandItem(L"Remove", "TRELLIS_SCATTERPLOT_REMOVE_CONDITION",
                    false, condition.variableId));
                conditioningMenu.Items().Append(current);
            }
            if (specification.conditioningVariables.size() > 1)
            {
                conditioningMenu.Items().Append(Controls::MenuFlyoutSeparator());
                conditioningMenu.Items().Append(commandItem(L"Swap rows and columns",
                    "TRELLIS_SCATTERPLOT_SWAP_DIMENSIONS"));
            }
            auto trellisDisplay = Controls::MenuFlyoutSubItem();
            trellisDisplay.Text(L"Trellis");
            trellisDisplay.IsEnabled(!specification.conditioningVariables.empty());
            auto layout = Controls::MenuFlyoutSubItem(); layout.Text(L"Panel layout");
            for (auto const& pair : std::array<std::pair<std::string, std::wstring>, 4>{{
                {"automatic", L"Automatic"}, {"one_row", L"One row"},
                {"one_column", L"One column"}, {"grid", L"Grid"}}})
                layout.Items().Append(commandItem(pair.second, "TRELLIS_SCATTERPLOT_SET_LAYOUT",
                    specification.layoutMode == pair.first, pair.first));
            if (specification.plotType != ::rlispstat::core::TrellisPlotType::DataTable)
            {
                layout.Items().Append(Controls::MenuFlyoutSeparator());
                layout.Items().Append(commandItem(L"Row labels left, Y axis right",
                    "TRELLIS_SCATTERPLOT_TOGGLE_Y_AXIS_SIDE",
                    currentModel_->trellisRowStripsOnLeft));
            }
            trellisDisplay.Items().Append(layout);
            auto order = Controls::MenuFlyoutSubItem(); order.Text(L"Panel order");
            for (auto const& pair : std::array<std::pair<std::string, std::wstring>, 3>{{
                {"defined", L"As defined"}, {"ascending", L"Label ascending"},
                {"descending", L"Label descending"}}})
                order.Items().Append(commandItem(pair.second, "TRELLIS_SCATTERPLOT_SET_ORDER",
                    specification.panelOrder == pair.first, pair.first));
            trellisDisplay.Items().Append(order);
            auto scale = Controls::MenuFlyoutSubItem(); scale.Text(L"Panel scales");
            for (auto const& pair : std::array<std::pair<std::string, std::wstring>, 4>{{
                {"common_xy", L"Common X and Y"}, {"free_x", L"Free X"},
                {"free_y", L"Free Y"}, {"free_xy", L"Free X and Y"}}})
                scale.Items().Append(commandItem(pair.second, "TRELLIS_SCATTERPLOT_SET_SCALE",
                    specification.scaleMode == pair.first, pair.first));
            trellisDisplay.Items().Append(scale);
            displayMenu.Items().Append(trellisDisplay);
            if (specification.plotType == ::rlispstat::core::TrellisPlotType::Scatter ||
                specification.plotType == ::rlispstat::core::TrellisPlotType::TimeSeries ||
                specification.plotType == ::rlispstat::core::TrellisPlotType::Histogram ||
                specification.plotType == ::rlispstat::core::TrellisPlotType::Bar)
            {
                auto split = Controls::MenuFlyoutSubItem();
                split.Text(specification.plotType == ::rlispstat::core::TrellisPlotType::TimeSeries
                    ? L"Series variable"
                    : specification.plotType == ::rlispstat::core::TrellisPlotType::Bar
                        ? L"Split variable" : L"Override colors by");
                std::string currentSplit =
                    specification.plotType == ::rlispstat::core::TrellisPlotType::TimeSeries
                    ? specification.groupingVariableId : specification.splitVariableId;
                const bool colorOverride =
                    specification.plotType == ::rlispstat::core::TrellisPlotType::Scatter ||
                    specification.plotType == ::rlispstat::core::TrellisPlotType::Histogram;
                if (colorOverride) currentSplit = currentModel_->colorByVariable;
                split.Items().Append(commandItem(colorOverride ? L"Cancel color override" : L"None",
                    colorOverride ? "PLOT_COLOR_BY" : "TRELLIS_SCATTERPLOT_SET_SPLIT",
                    currentSplit.empty(), "."));
                for (auto const& column : dataFrame_.columns)
                {
                    const bool axisVariable = column.name == specification.xVariableId ||
                        column.name == specification.yVariableId;
                    if (!axisVariable &&
                        ::rlispstat::core::TrellisConditionColumnIsValid(column, dataFrame_.rows))
                        split.Items().Append(commandItem(to_hstring(column.name).c_str(),
                            colorOverride ? "PLOT_COLOR_BY" : "TRELLIS_SCATTERPLOT_SET_SPLIT", currentSplit == column.name,
                            column.name));
                }
                if (specification.plotType == ::rlispstat::core::TrellisPlotType::Scatter ||
                    specification.plotType == ::rlispstat::core::TrellisPlotType::Histogram)
                    colorOverrideMenu = split;
                else variablesMenu.Items().Append(split);
            }
            if (specification.plotType == ::rlispstat::core::TrellisPlotType::Bar)
            {
                auto measure = Controls::MenuFlyoutSubItem(); measure.Text(L"Bar measure");
                measure.Items().Append(commandItem(L"Count", "TRELLIS_SCATTERPLOT_SET_BAR_MEASURE",
                    specification.barMeasure == "count", "count"));
                measure.Items().Append(commandItem(L"Percent within panel", "TRELLIS_SCATTERPLOT_SET_BAR_MEASURE",
                    specification.barMeasure == "percent", "percent"));
                measure.Items().Append(commandItem(L"Each X bar = 100%", "TRELLIS_SCATTERPLOT_SET_BAR_MEASURE",
                    specification.barMeasure == "conditional_percent", "conditional_percent"));
                displayMenu.Items().Append(measure);
            }
            if (specification.plotType == ::rlispstat::core::TrellisPlotType::Histogram)
            {
                auto measure = Controls::MenuFlyoutSubItem(); measure.Text(L"Histogram measure");
                for (auto const& pair : std::array<std::pair<std::string, std::wstring>, 3>{{
                    {"count", L"Count"}, {"percent", L"Percent"}, {"density", L"Density"}}})
                    measure.Items().Append(commandItem(pair.second,
                        "TRELLIS_SCATTERPLOT_SET_HISTOGRAM_MEASURE",
                        specification.histogramMeasure == pair.first, pair.first));
                displayMenu.Items().Append(measure);
                auto bins = Controls::MenuFlyoutSubItem(); bins.Text(L"Bins");
                for (int count : std::array<int, 6>{ 5, 10, 15, 20, 30, 40 })
                    bins.Items().Append(commandItem(to_hstring(std::to_string(count)).c_str(),
                        "TRELLIS_SCATTERPLOT_SET_HISTOGRAM_BINS",
                        specification.histogramBinCount == static_cast<std::size_t>(count),
                        std::to_string(count)));
                displayMenu.Items().Append(bins);
                auto rules = Controls::MenuFlyoutSubItem(); rules.Text(L"Automatic binning rule");
                for (const auto &option : ::rlispstat::core::HistogramBinningRuleMenuOptions())
                    rules.Items().Append(commandItem(to_hstring(option.title).c_str(),
                        "TRELLIS_SCATTERPLOT_SET_HISTOGRAM_BINS", false, option.value));
                displayMenu.Items().Append(rules);
            }
            if (specification.plotType == ::rlispstat::core::TrellisPlotType::Scatter)
                appendFitAndSmoothMenu();
            if (specification.plotType == ::rlispstat::core::TrellisPlotType::TimeSeries)
                displayMenu.Items().Append(commandItem(L"Connect observations", "TRELLIS_SCATTERPLOT_TOGGLE_CONNECT",
                    specification.connectObservations));
            if (specification.plotType == ::rlispstat::core::TrellisPlotType::TimeSeries)
            {
                auto identify = Controls::MenuFlyoutSubItem(); identify.Text(L"Series identification");
                for (auto const& pair : std::array<std::pair<std::string, std::wstring>, 3>{{
                    {"legend", L"Legend"}, {"start_labels", L"Labels at start"}, {"none", L"None"}}})
                    identify.Items().Append(commandItem(pair.second, "TIME_SERIES_SET_IDENTIFICATION",
                        currentModel_->timeSeriesIdentification == pair.first, pair.first));
                if (!currentModel_->timeSeriesGroupVariable.empty()) {
                displayMenu.Items().Append(identify);
                auto positions=Controls::MenuFlyoutSubItem(); positions.Text(L"Legend position");
                for (auto const& choice : std::vector<std::pair<std::string,std::wstring>>{
                    {"left",L"Left"},{"right",L"Right"},{"top",L"Top"},{"bottom",L"Bottom"},
                    {"top_left",L"Top left"},{"top_right",L"Top right"},
                    {"bottom_left",L"Bottom left"},{"bottom_right",L"Bottom right"}})
                    positions.Items().Append(commandItem(choice.second,"TIME_SERIES_SET_LEGEND_POSITION",
                        !currentModel_->interactionLegendUsesCustomPosition && currentModel_->timeSeriesIdentification=="legend" &&
                        currentModel_->timeSeriesLegendPosition==choice.first,choice.first));
                displayMenu.Items().Append(positions);
            }
            }
        }
        else if (currentModel_ && currentModel_->kind == "boxplot")
        {
            displayMenu.Items().Append(commandItem(L"Show points", "BOXPLOT_TOGGLE_POINTS",
                currentModel_->boxplotShowPoints));
            displayMenu.Items().Append(commandItem(L"Show box", "BOXPLOT_TOGGLE_BOX",
                currentModel_->boxplotShowBox));
            displayMenu.Items().Append(commandItem(L"Show whiskers", "BOXPLOT_TOGGLE_WHISKERS",
                currentModel_->boxplotShowWhiskers));
            displayMenu.Items().Append(commandItem(L"Show violin", "BOXPLOT_TOGGLE_VIOLIN",
                currentModel_->boxplotShowViolin));
            if (::rlispstat::core::BoxplotUsesVariableAxes(*currentModel_))
            {
                displayMenu.Items().Append(commandItem(L"Connect observations", "BOXPLOT_TOGGLE_CONNECT_ROWS",
                    currentModel_->boxplotConnectRows));
                displayMenu.Items().Append(commandItem(L"Standardize variables", "BOXPLOT_TOGGLE_STANDARDIZE",
                    currentModel_->boxplotStandardizeVariables));
                auto add = Controls::MenuFlyoutSubItem(); add.Text(L"Add variable");
                auto remove = Controls::MenuFlyoutSubItem(); remove.Text(L"Remove variable");
                for (auto const& variable : currentModel_->variables)
                {
                    const bool present = std::find(currentModel_->boxplotVariables.begin(),
                        currentModel_->boxplotVariables.end(), variable.name) != currentModel_->boxplotVariables.end();
                    if (!present) add.Items().Append(commandItem(to_hstring(variable.name).c_str(),
                        "BOXPLOT_ADD_VARIABLE", false, variable.name));
                    else if (currentModel_->boxplotVariables.size() > 2)
                        remove.Items().Append(commandItem(to_hstring(variable.name).c_str(),
                            "BOXPLOT_REMOVE_VARIABLE", false, variable.name));
                }
                if (add.Items().Size() > 0) variablesMenu.Items().Append(add);
                if (remove.Items().Size() > 0) variablesMenu.Items().Append(remove);
            }
            if (hasDataFrame_)
            {
                std::vector<std::string> groups = currentModel_->boxplotGroupingVariables;
                if (groups.empty() && !currentModel_->xLabel.empty())
                    groups.push_back(currentModel_->xLabel);
                auto grouping = Controls::MenuFlyoutSubItem();
                grouping.Text(L"Grouping variables");
                auto addGroup = Controls::MenuFlyoutSubItem();
                addGroup.Text(groups.empty() ? L"Add grouping variable" : L"Add inner level");
                for (auto const& column : dataFrame_.columns)
                {
                    const bool used = std::find(groups.begin(), groups.end(), column.name) != groups.end();
                    if (!used && column.name != currentModel_->yLabel &&
                        ::rlispstat::core::DataColumnLooksGroupingCandidate(column, dataFrame_.rows))
                        addGroup.Items().Append(commandItem(to_hstring(column.name).c_str(),
                            "BOXPLOT_ADD_GROUPING_VARIABLE", false, column.name));
                }
                if (addGroup.Items().Size() > 0) grouping.Items().Append(addGroup);
                for (std::size_t index = 0; index < groups.size(); ++index)
                {
                    auto current = Controls::MenuFlyoutSubItem();
                    current.Text(to_hstring(groups[index]));
                    auto outward = commandItem(L"Move outward",
                        "BOXPLOT_MOVE_GROUPING_VARIABLE_EARLIER", false, groups[index]);
                    outward.IsEnabled(index > 0); current.Items().Append(outward);
                    auto inward = commandItem(L"Move inward",
                        "BOXPLOT_MOVE_GROUPING_VARIABLE_LATER", false, groups[index]);
                    inward.IsEnabled(index + 1 < groups.size()); current.Items().Append(inward);
                    current.Items().Append(Controls::MenuFlyoutSeparator());
                    current.Items().Append(commandItem(L"Remove",
                        "BOXPLOT_REMOVE_GROUPING_VARIABLE", false, groups[index]));
                    grouping.Items().Append(current);
                }
                if (grouping.Items().Size() > 0) variablesMenu.Items().Append(grouping);

            }
        }
        else if (currentModel_ && currentModel_->kind == "scatter_matrix")
        {
            appendFitAndSmoothMenu();
            auto variables = Controls::MenuFlyoutSubItem(); variables.Text(L"Variables");
            auto add = Controls::MenuFlyoutSubItem(); add.Text(L"Add variable");
            auto remove = Controls::MenuFlyoutSubItem(); remove.Text(L"Remove variable");
            const auto current = ::rlispstat::core::ScatterMatrixVariablesForModel(*currentModel_);
            const auto state = ::rlispstat::core::BuildScatterMatrixVariableMenuState(
                current, ::rlispstat::core::NumericVariableNames(*currentModel_));
            for (auto const& name : state.addVariables)
                add.Items().Append(commandItem(to_hstring(name).c_str(),
                    "SCATTER_MATRIX_ADD_VARIABLE", false, name));
            for (auto const& name : state.removeVariables)
                remove.Items().Append(commandItem(to_hstring(name).c_str(),
                    "SCATTER_MATRIX_REMOVE_VARIABLE", false, name));
            if (add.Items().Size() > 0) variables.Items().Append(add);
            if (remove.Items().Size() > 0) variables.Items().Append(remove);
            variablesMenu.Items().Append(variables);
        }
        else if (currentModel_ && currentModel_->kind == "histogram")
        {
            if (currentModel_->isGLMDiagnostic &&
                currentModel_->glmDiagnosticKind == "residual_histogram")
            {
                auto residuals = Controls::MenuFlyoutSubItem();
                residuals.Text(L"Residuals");
                for (auto const& id : ::rlispstat::core::DiagnosticPlotResidualChoices(*currentModel_))
                    residuals.Items().Append(commandItem(to_hstring(::rlispstat::core::GeneralizedResidualTypeLabel(id)).c_str(),
                        "SET_DIAGNOSTIC_RESIDUAL_TYPE", currentModel_->displayedResidualType == id, id));
                displayMenu.Items().Append(residuals);
            }
            displayMenu.Items().Append(commandItem(L"Show counts", "HIST_TOGGLE_COUNTS",
                currentModel_->histogramShowCounts));
            displayMenu.Items().Append(commandItem(L"Show tick marks", "HIST_TOGGLE_TICK_MARKS",
                currentModel_->histogramShowTickMarks));
            displayMenu.Items().Append(commandItem(L"Show tick labels", "HIST_TOGGLE_TICK_LABELS",
                currentModel_->histogramShowTickLabels));
            displayMenu.Items().Append(commandItem(L"Show rug", "HIST_TOGGLE_RUG",
                currentModel_->histogramShowRug));
            displayMenu.Items().Append(commandItem(L"Show density curves", "HIST_TOGGLE_DENSITY",
                currentModel_->histogramShowDensity));
            auto density = Controls::MenuFlyoutSubItem(); density.Text(L"Density curves");
            for (auto const& option : ::rlispstat::core::HistogramDensityModeMenuOptions())
                density.Items().Append(commandItem(to_hstring(option.title).c_str(),
                    "HIST_SET_DENSITY_MODE",
                    currentModel_->histogramShowDensity &&
                        currentModel_->histogramDensityMode == option.value,
                    option.value));
            displayMenu.Items().Append(density);
            auto bins = Controls::MenuFlyoutSubItem(); bins.Text(L"Number of bins");
            for (int count : std::array<int, 6>{ 5, 10, 15, 20, 30, 40 })
                bins.Items().Append(commandItem(to_hstring(std::to_string(count)).c_str(),
                    "HIST_SET_BINS", currentModel_->histogramBins.size() ==
                        static_cast<std::size_t>(count), std::to_string(count)));
            displayMenu.Items().Append(bins);
            auto rule = Controls::MenuFlyoutSubItem(); rule.Text(L"Automatic binning rule");
            for (auto const& option : ::rlispstat::core::HistogramBinningRuleMenuOptions())
                rule.Items().Append(commandItem(to_hstring(option.title).c_str(), "HIST_SET_BINNING_RULE", false, option.value));
            displayMenu.Items().Append(rule);
        }
        else if (currentModel_ && currentModel_->kind == "barplot")
        {
            using namespace ::rlispstat::core;
            const auto xVariables = BarplotXVariablesForModel(*currentModel_);
            std::vector<std::string> available;
            if (hasDataFrame_) available = BarplotAvailableVariables(dataFrame_);
            else
                for (auto const& variable : currentModel_->variableMeta)
                    available.push_back(variable.name);
            const auto xState = BuildBarplotXMenuState(
                available, xVariables, currentModel_->barplotSplitVariable);
            const auto splitState = BuildBarplotSplitMenuState(
                available, xVariables, currentModel_->barplotSplitVariable);
            const auto display = BuildBarplotDisplayMenuState(
                currentModel_->barplotMode, currentModel_->barplotWidthMode,
                currentModel_->barplotSplitVariable,
                currentModel_->barplotRowColorDisplay,
                currentModel_->barplotShowConditionalPercent,
                currentModel_->barplotSplitStrokeWidth,
                currentModel_->barplotSegmentEncodingMode,
                currentModel_->barplotShowPatterns,
                currentModel_->barplotSelectionDisplay, xVariables);

            auto xAxis = Controls::MenuFlyoutSubItem();
            xAxis.Text(to_hstring(xState.title));
            for (auto const& option : xState.setOptions)
                xAxis.Items().Append(CreateBarplotMenuItem(option, true));
            if (!xState.addOptions.empty())
            {
                auto add = Controls::MenuFlyoutSubItem();
                add.Text(to_hstring(xState.addXVariableTitle));
                for (auto const& option : xState.addOptions)
                    add.Items().Append(CreateBarplotMenuItem(option));
                xAxis.Items().Append(add);
            }
            if (!xState.replaceSections.empty())
            {
                auto replace = Controls::MenuFlyoutSubItem();
                replace.Text(to_hstring(xState.replaceOneXVariableTitle));
                for (auto const& section : xState.replaceSections)
                {
                    auto variable = Controls::MenuFlyoutSubItem();
                    variable.Text(to_hstring(section.variable));
                    for (auto const& option : section.options)
                        variable.Items().Append(CreateBarplotMenuItem(option));
                    replace.Items().Append(variable);
                }
                xAxis.Items().Append(replace);
            }
            if (!xState.removeOptions.empty())
            {
                auto remove = Controls::MenuFlyoutSubItem();
                remove.Text(to_hstring(xState.removeXVariableTitle));
                for (auto const& option : xState.removeOptions)
                    remove.Items().Append(CreateBarplotMenuItem(option));
                xAxis.Items().Append(remove);
            }
            variablesMenu.Items().Append(xAxis);

            auto split = Controls::MenuFlyoutSubItem();
            split.Text(to_hstring(splitState.title));
            split.Items().Append(CreateBarplotMenuItem(splitState.noneOption, true));
            if (!splitState.splitOptions.empty())
                split.Items().Append(Controls::MenuFlyoutSeparator());
            for (auto const& option : splitState.splitOptions)
                split.Items().Append(CreateBarplotMenuItem(option, true));
            variablesMenu.Items().Append(split);

            auto yAxis = Controls::MenuFlyoutSubItem();
            yAxis.Text(to_hstring(display.yAxisTitle));
            for (auto const& option : display.modeOptions)
                yAxis.Items().Append(CreateBarplotMenuItem(option, true));
            displayMenu.Items().Append(yAxis);

            auto width = Controls::MenuFlyoutSubItem();
            width.Text(to_hstring(display.barWidthTitle));
            for (auto const& option : display.widthOptions)
                width.Items().Append(CreateBarplotMenuItem(option, true));
            displayMenu.Items().Append(width);
            displayMenu.Items().Append(CreateBarplotMenuItem(display.conditionalPercent, true));

            auto rowColors = Controls::MenuFlyoutSubItem();
            rowColors.Text(to_hstring(display.rowColorsTitle));
            for (auto const& option : display.rowColorOptions)
                rowColors.Items().Append(CreateBarplotMenuItem(option, true));
            displayMenu.Items().Append(rowColors);

            auto selection = Controls::MenuFlyoutSubItem();
            selection.Text(to_hstring(display.selectionDisplayTitle));
            for (auto const& option : display.selectionDisplayOptions)
                selection.Items().Append(CreateBarplotMenuItem(option, true));
            displayMenu.Items().Append(selection);

            auto border = Controls::MenuFlyoutSubItem();
            border.Text(L"Bar border width");
            for (double value : std::array<double, 6>{ 1.0, 2.0, 3.0, 4.0, 6.0, 8.0 })
            {
                BarplotMenuOption option;
                option.title = std::to_string(static_cast<int>(value)) + " px";
                option.command = "BARPLOT_SPLIT_STROKE_WIDTH|" + std::to_string(value);
                option.checked = std::fabs(currentModel_->barplotSplitStrokeWidth - value) < 0.01;
                border.Items().Append(CreateBarplotMenuItem(option, true));
            }
            displayMenu.Items().Append(border);
        }
        else if (currentModel_ && currentModel_->kind == "scatter")
        {
            if (!currentModel_->isGLMDiagnostic ||
                ::rlispstat::core::RegressionDiagnosticSupportsAddedLines(
                    *currentModel_))
                appendFitAndSmoothMenu();
            if (currentModel_->isGLMDiagnostic &&
                (currentModel_->glmDiagnosticKind == "residuals_fitted" ||
                 currentModel_->glmDiagnosticKind == "normal_qq" ||
                 currentModel_->glmDiagnosticKind == "scale_location" ||
                 currentModel_->glmDiagnosticKind == "residuals_leverage" ||
                 currentModel_->glmDiagnosticKind == "partial_regression"))
            {
                auto residuals = Controls::MenuFlyoutSubItem();
                residuals.Text(L"Residuals");
                for (auto const& id : ::rlispstat::core::DiagnosticPlotResidualChoices(*currentModel_))
                    residuals.Items().Append(commandItem(to_hstring(::rlispstat::core::GeneralizedResidualTypeLabel(id)).c_str(),
                        "SET_DIAGNOSTIC_RESIDUAL_TYPE", currentModel_->displayedResidualType == id, id));
                displayMenu.Items().Append(residuals);
            }
            if (currentModel_->isGLMDiagnostic &&
                currentModel_->diagnosticImputationCount > 1)
            {
                auto imputationDisplay = Controls::MenuFlyoutSubItem();
                if (currentModel_->diagnosticShowImputationUncertainty)
                {
                    imputationDisplay.Text(to_hstring("Imputations - All (m = " +
                        std::to_string(currentModel_->diagnosticImputationCount) + ")"));
                }
                else
                {
                    imputationDisplay.Text(to_hstring("Imputation - " +
                        std::to_string(currentModel_->diagnosticImputationIndex) + " of " +
                        std::to_string(currentModel_->diagnosticImputationCount)));
                }
                for (int index = 1;
                     index <= currentModel_->diagnosticImputationCount; ++index)
                {
                    imputationDisplay.Items().Append(commandItem(
                        to_hstring("Imputation " + std::to_string(index) + " of " +
                            std::to_string(currentModel_->diagnosticImputationCount)).c_str(),
                        "SET_DIAGNOSTIC_IMPUTATION_DISPLAY",
                        !currentModel_->diagnosticShowImputationUncertainty &&
                            index == currentModel_->diagnosticImputationIndex,
                        "version:" + std::to_string(index)));
                }
                if (currentModel_->glmDiagnosticKind != "roc_curve" &&
                    currentModel_->glmDiagnosticKind != "normal_qq")
                {
                    imputationDisplay.Items().Append(Controls::MenuFlyoutSeparator());
                    imputationDisplay.Items().Append(commandItem(
                        L"All imputations (uncertainty)",
                        "SET_DIAGNOSTIC_IMPUTATION_DISPLAY",
                        currentModel_->diagnosticShowImputationUncertainty, "all"));
                }
                displayMenu.Items().Append(imputationDisplay);
                if (currentModel_->diagnosticShowImputationUncertainty)
                {
                    auto uncertainty = Controls::MenuFlyoutSubItem();
                    uncertainty.Text(L"Imputation uncertainty");
                    uncertainty.Items().Append(commandItem(
                        L"Directly imputed cases",
                        "SET_DIAGNOSTIC_IMPUTATION_SCOPE",
                        currentModel_->diagnosticImputationUncertaintyScope == "direct",
                        "direct"));
                    uncertainty.Items().Append(commandItem(
                        L"All fitted values (model propagation)",
                        "SET_DIAGNOSTIC_IMPUTATION_SCOPE",
                        currentModel_->diagnosticImputationUncertaintyScope == "all",
                        "all"));
                    uncertainty.Items().Append(Controls::MenuFlyoutSeparator());
                    for (auto const& option :
                         ::rlispstat::core::ScatterplotImputationUncertaintyMenuOptions(
                             currentModel_->scatterImputationUncertainty))
                    {
                        uncertainty.Items().Append(commandItem(
                            to_hstring(option.title).c_str(),
                            "SET_IMPUTATION_UNCERTAINTY", option.checked, option.value));
                    }
                    displayMenu.Items().Append(uncertainty);
                }
            }
            else if (hasDataFrame_ &&
                     dataFrame_.datasetType == "multiple_imputation" &&
                     !::rlispstat::core::IsPooledRegressionEffectPlot(
                         *currentModel_))
            {
                auto imputationDisplay = Controls::MenuFlyoutSubItem();
                imputationDisplay.Text(to_hstring(
                    ::rlispstat::core::ScatterplotImputationDisplayMenuTitle(
                        dataFrame_.imputationCount,
                        dataFrame_.activeImputationVersion,
                        dataFrame_.imputationDisplayMode)));
                auto displayOptions =
                    ::rlispstat::core::ScatterplotImputationDisplayMenuOptions(
                        dataFrame_.imputationCount,
                        dataFrame_.activeImputationVersion,
                        dataFrame_.imputationDisplayMode);
                for (std::size_t index = 0; index < displayOptions.size(); ++index)
                {
                    if (index == static_cast<std::size_t>(dataFrame_.imputationCount))
                        imputationDisplay.Items().Append(Controls::MenuFlyoutSeparator());
                    auto const& option = displayOptions[index];
                    imputationDisplay.Items().Append(commandItem(
                        to_hstring(option.title).c_str(), "SET_IMPUTATION_DISPLAY",
                        option.checked, option.value));
                }
                displayMenu.Items().Append(imputationDisplay);
                if (dataFrame_.imputationDisplayMode == "all")
                {
                    auto uncertainty = Controls::MenuFlyoutSubItem();
                    uncertainty.Text(L"Imputation uncertainty");
                    for (auto const& option :
                         ::rlispstat::core::ScatterplotImputationUncertaintyMenuOptions(
                             currentModel_->scatterImputationUncertainty))
                    {
                        uncertainty.Items().Append(commandItem(
                            to_hstring(option.title).c_str(),
                            "SET_IMPUTATION_UNCERTAINTY", option.checked, option.value));
                    }
                    displayMenu.Items().Append(uncertainty);
                }
            }
            if (!currentModel_->isGLMDiagnostic && !currentModel_->variables.empty())
            {
                auto xAxis = Controls::MenuFlyoutSubItem(); xAxis.Text(L"X variable");
                auto yAxis = Controls::MenuFlyoutSubItem(); yAxis.Text(L"Y variable");
                for (auto const& variable : currentModel_->variables)
                {
                    xAxis.Items().Append(commandItem(to_hstring(variable.name).c_str(),
                        "SET_XVAR", variable.name == currentModel_->xLabel, variable.name));
                    yAxis.Items().Append(commandItem(to_hstring(variable.name).c_str(),
                        "SET_YVAR", variable.name == currentModel_->yLabel, variable.name));
                }
                variablesMenu.Items().Append(xAxis);
                variablesMenu.Items().Append(yAxis);
            }

        }

        // Every plot kind uses the same semantic analysis context. This keeps
        // plot-specific display statistics (for example bar counts) out of
        // analyses and makes the adaptive command set consistent everywhere.
        auto analyzeMenu = Controls::MenuFlyoutSubItem();
        analyzeMenu.Text(L"Analyze");
        if (currentModel_ && hasDataFrame_ && !aggregateDiagnostic)
        {
            const auto analysisContext =
                ::rlispstat::core::BuildPlotAnalysisContext(*currentModel_, dataFrame_);
            const auto options =
                ::rlispstat::core::PlotAnalysisMenuOptions(analysisContext);
            if (!options.empty())
            {
                auto analyses = Controls::MenuFlyoutSubItem();
                analyses.Text(L"Analyze this plot");
                for (auto const& option : options)
                    analyses.Items().Append(commandItem(
                        to_hstring(option.title).c_str(), option.command));
                analyzeMenu.Items().Append(analyses);
            }
        }

        if (aggregateDiagnostic && currentModel_->diagnosticImputationCount > 1)
        {
            auto imputation = Controls::MenuFlyoutSubItem();
            imputation.Text(L"Imputation");
            for (int index = 1; index <= currentModel_->diagnosticImputationCount; ++index)
                imputation.Items().Append(commandItem(
                    to_hstring("Imputation " + std::to_string(index) + " of " +
                        std::to_string(currentModel_->diagnosticImputationCount)).c_str(),
                    "SET_DIAGNOSTIC_IMPUTATION_DISPLAY",
                    index == currentModel_->diagnosticImputationIndex,
                    "version:" + std::to_string(index)));
            displayMenu.Items().Append(imputation);
        }

        // A contextual theme is a view override for this plot only. The main
        // View menu still dispatches PLOT_THEME and applies a new global theme
        // to every open plot.
        auto themeMenu = Controls::MenuFlyoutSubItem();
        themeMenu.Text(L"Theme");
        for (auto const& theme : ::rlispstat::core::PlotThemeNames())
        {
            auto item = Controls::ToggleMenuFlyoutItem();
            item.Text(to_hstring(::rlispstat::core::PlotThemeDisplayName(theme)));
            item.IsChecked(theme == themeName_);
            item.Click([this, theme](auto const&, auto const&)
            {
                SetLocalTheme(theme);
            });
            themeMenu.Items().Append(item);
        }
        displayMenu.Items().Append(themeMenu);
        if (currentModel_ && !currentModel_->colorByVariable.empty() &&
            !currentModel_->colorByLegendItems.empty())
        {
            auto legend = Controls::MenuFlyoutSubItem();
            legend.Text(L"Color legend");
            legend.Items().Append(commandItem(
                currentModel_->colorByLegendVisible ? L"Hide" : L"Show",
                "PLOT_COLOR_LEGEND_VISIBLE", currentModel_->colorByLegendVisible,
                currentModel_->colorByLegendVisible ? "0" : "1"));
            legend.Items().Append(Controls::MenuFlyoutSeparator());
            for (auto const& position : std::vector<std::pair<std::string, std::wstring>>{
                     {"top_left", L"Top left"}, {"top_right", L"Top right"},
                     {"bottom_left", L"Bottom left"}, {"bottom_right", L"Bottom right"}})
                legend.Items().Append(commandItem(position.second,
                    "PLOT_COLOR_LEGEND_POSITION", false, position.first));
            displayMenu.Items().Append(legend);
        }
        if (currentModel_ && currentModel_->kind == "glm_interaction" &&
            !currentModel_->interactionPlotLines.empty())
        {
            if (!aggregateDiagnostic)
            {
            auto const effectVariables = ::rlispstat::core::SplitInteractionTerm(
                currentModel_->glmInteractionTerm);
            if (effectVariables.size() >= 2)
            {
                auto axis = Controls::MenuFlyoutSubItem();
                axis.Text(L"Horizontal-axis variable");
                for (auto const& variable : effectVariables)
                    axis.Items().Append(commandItem(
                        to_hstring(variable).c_str(),
                        "PLOT_REGRESSION_SET_EFFECT_X",
                        currentModel_->interactionFocalVariable == variable, variable));
                displayMenu.Items().Append(axis);
            }
            if (currentModel_->regressionBinaryProbability ||
                currentModel_->regressionBoundedCount)
            {
                auto quantity = Controls::MenuFlyoutSubItem();
                quantity.Text(L"Quantity");
                if (currentModel_->regressionCeilingHurdle)
                {
                    quantity.Items().Append(commandItem(
                        L"Overall expected score",
                        "PLOT_REGRESSION_SET_EFFECT_QUANTITY",
                        currentModel_->regressionEffectQuantity == "overall_expected_score",
                        "overall_expected_score"));
                    quantity.Items().Append(commandItem(
                        L"Perfect score probability",
                        "PLOT_REGRESSION_SET_EFFECT_QUANTITY",
                        currentModel_->regressionEffectQuantity == "perfect_score_probability",
                        "perfect_score_probability"));
                    quantity.Items().Append(commandItem(
                        L"Expected score below ceiling",
                        "PLOT_REGRESSION_SET_EFFECT_QUANTITY",
                        currentModel_->regressionEffectQuantity == "conditional_expected_score",
                        "conditional_expected_score"));
                }
                else if (currentModel_->regressionBoundedCount)
                {
                    quantity.Items().Append(commandItem(
                        L"Expected count",
                        "PLOT_REGRESSION_SET_EFFECT_QUANTITY",
                        currentModel_->regressionEffectQuantity == "expected_count",
                        "expected_count"));
                }
                if (!currentModel_->regressionCeilingHurdle)
                    quantity.Items().Append(commandItem(
                        L"Predicted probability",
                        "PLOT_REGRESSION_SET_EFFECT_QUANTITY",
                        currentModel_->regressionEffectQuantity == "predicted_probability",
                        "predicted_probability"));
                if (!currentModel_->regressionBoundedCount)
                    quantity.Items().Append(commandItem(
                        L"Probability difference",
                        "PLOT_REGRESSION_SET_EFFECT_QUANTITY",
                        currentModel_->regressionEffectQuantity == "probability_difference",
                        "probability_difference"));
                displayMenu.Items().Append(quantity);

                if (!currentModel_->regressionBoundedCount)
                {
                auto adjustment = Controls::MenuFlyoutSubItem();
                adjustment.Text(L"Adjustment");
                adjustment.Items().Append(commandItem(
                    L"Average over analysis sample",
                    "PLOT_REGRESSION_SET_EFFECT_ADJUSTMENT",
                    currentModel_->regressionEffectAdjustment == "average_sample",
                    "average_sample"));
                adjustment.Items().Append(commandItem(
                    L"Reference profile",
                    "PLOT_REGRESSION_SET_EFFECT_ADJUSTMENT",
                    currentModel_->regressionEffectAdjustment == "reference_profile",
                    "reference_profile"));
                displayMenu.Items().Append(adjustment);

                auto presentation = Controls::MenuFlyoutSubItem();
                presentation.Text(L"Probability display");
                presentation.Items().Append(commandItem(
                    L"Percentage / percentage points",
                    "PLOT_REGRESSION_SET_EFFECT_PRESENTATION",
                    currentModel_->regressionEffectPresentation == "percentage",
                    "percentage"));
                presentation.Items().Append(commandItem(
                    L"Probability (0–1)",
                    "PLOT_REGRESSION_SET_EFFECT_PRESENTATION",
                    currentModel_->regressionEffectPresentation == "probability",
                    "probability"));
                displayMenu.Items().Append(presentation);
                }

                auto confidence = Controls::MenuFlyoutSubItem();
                confidence.Text(L"Confidence level");
                for (auto const& option : std::vector<std::pair<std::string, std::wstring>>{
                         {"0.90", L"90%"}, {"0.95", L"95%"}, {"0.99", L"99%"}})
                {
                    const double level = std::strtod(option.first.c_str(), nullptr);
                    confidence.Items().Append(commandItem(
                        option.second,
                        "PLOT_REGRESSION_SET_EFFECT_CONFIDENCE",
                        std::fabs(currentModel_->regressionConfidenceLevel - level) < 0.001,
                        option.first));
                }
                displayMenu.Items().Append(confidence);
            }
            displayMenu.Items().Append(commandItem(
                currentModel_->regressionConnectEstimates
                    ? L"Hide connecting line" : L"Show connecting line",
                "PLOT_REGRESSION_TOGGLE_CONNECTING_LINE",
                currentModel_->regressionConnectEstimates));
            displayMenu.Items().Append(commandItem(
                currentModel_->regressionConfidenceIntervalsVisible
                    ? L"Hide confidence intervals" : L"Show confidence intervals",
                "PLOT_REGRESSION_TOGGLE_CONFIDENCE_INTERVALS",
                currentModel_->regressionConfidenceIntervalsVisible));
            auto confidenceLevel = commandItem(
                currentModel_->regressionConfidenceLevelVisible
                    ? L"Hide confidence-level label" : L"Show confidence-level label",
                "PLOT_REGRESSION_TOGGLE_CONFIDENCE_LEVEL",
                currentModel_->regressionConfidenceLevelVisible);
            confidenceLevel.IsEnabled(
                currentModel_->regressionConfidenceIntervalsVisible);
            displayMenu.Items().Append(confidenceLevel);
            }
            auto legend = Controls::MenuFlyoutSubItem();
            legend.Text(L"Legend position");
            for (auto const& position : std::vector<std::pair<std::string, std::wstring>>{
                     {"right", L"Right"}, {"left", L"Left"},
                     {"top", L"Top"}, {"bottom", L"Bottom"},
                     {"outside_right", L"Outside right"},
                     {"outside_left", L"Outside left"},
                     {"outside_top", L"Outside top"},
                     {"outside_bottom", L"Outside bottom"},
                     {"manual", L"Manual"}})
                legend.Items().Append(commandItem(
                    position.second,
                    "PLOT_INTERACTION_SET_LEGEND_POSITION",
                    position.first == "manual"
                        ? currentModel_->interactionLegendUsesCustomPosition
                        : !currentModel_->interactionLegendUsesCustomPosition &&
                          currentModel_->interactionLegendPosition == position.first,
                    position.first));
            displayMenu.Items().Append(legend);
            auto renameLegend = Controls::MenuFlyoutItem();
            renameLegend.Text(L"Rename legend title…");
            renameLegend.Click([this](auto const&, auto const&)
            {
                if (!currentModel_ || !plotCanvas_.XamlRoot()) return;
                auto editor = Controls::TextBox();
                editor.Text(to_hstring(currentModel_->interactionLegendTitleOverride));
                editor.PlaceholderText(L"Variable label");
                auto dialog = Controls::ContentDialog();
                dialog.XamlRoot(plotCanvas_.XamlRoot());
                dialog.Title(box_value(L"Legend title"));
                dialog.Content(editor);
                dialog.PrimaryButtonText(L"Apply");
                dialog.CloseButtonText(L"Cancel");
                dialog.PrimaryButtonClick([this, editor](auto const&, auto const&)
                {
                    if (!currentModel_) return;
                    currentModel_->interactionLegendTitleOverride = to_string(editor.Text());
                    currentModel_->interactionLegendTitle =
                        currentModel_->interactionLegendTitleOverride.empty()
                        ? currentModel_->interactionLegendDefaultTitle
                        : currentModel_->interactionLegendTitleOverride;
                    Show(*currentModel_);
                });
                auto operation = dialog.ShowAsync();
                operation.Completed([dialog](auto const&, Windows::Foundation::AsyncStatus) {});
            });
            displayMenu.Items().Append(renameLegend);
            auto legendTitleVisible = Controls::ToggleMenuFlyoutItem();
            legendTitleVisible.Text(L"Show legend title");
            legendTitleVisible.IsChecked(currentModel_->interactionLegendTitleVisible);
            legendTitleVisible.Click([this](auto const&, auto const&)
            {
                if (!currentModel_) return;
                currentModel_->interactionLegendTitleVisible =
                    !currentModel_->interactionLegendTitleVisible;
                Show(*currentModel_);
            });
            displayMenu.Items().Append(legendTitleVisible);
        }
        if (!aggregateDiagnostic && SupportsPointSizeControl())
        {
            auto pointSize = Controls::ToggleMenuFlyoutItem();
            pointSize.Text(pointSizeControlVisible_
                ? L"Hide point size slider" : L"Show point size slider");
            pointSize.IsChecked(pointSizeControlVisible_);
            pointSize.Click([this](auto const&, auto const&)
            {
                pointSizeControlVisible_ = !pointSizeControlVisible_;
                if (currentModel_)
                    currentModel_->pointSizeSliderVisible = pointSizeControlVisible_;
                pointSizeControl_.Visibility(pointSizeControlVisible_
                    ? Visibility::Visible : Visibility::Collapsed);
                ConfigureContextMenu();
            });
            displayMenu.Items().Append(pointSize);
        }
        if (variablesMenu.Items().Size() > 0) menu.Items().Append(variablesMenu);
        AppendMenuGroupWithoutRedundantSingleLevel(menu, conditioningMenu);
        if (displayMenu.Items().Size() > 0) menu.Items().Append(displayMenu);


        if (!currentModel_ || ::rlispstat::core::PlotLinksToDataRows(*currentModel_) ||
            ViewportNavigationGestureEnabled())
        {
        auto selection = Controls::MenuFlyoutSubItem(); selection.Text(L"Explore plot");
        if (currentModel_)
        {
            const auto appendInteractionMode = [this, selection](wchar_t const* title,
                                                                 std::string mode)
            {
                auto item = Controls::ToggleMenuFlyoutItem();
                item.Text(title);
                item.IsChecked(currentModel_ && currentModel_->interactionMode == mode);
                item.Click([this, mode = std::move(mode)](auto const&, auto const&)
                {
                    if (!currentModel_) return;
                    currentModel_->interactionMode = mode;
                    if (commandCallback_ && !plotId_.empty())
                        commandCallback_({ "INTERACTION_MODE", plotId_, mode });
                    ConfigureContextMenu();
                });
                selection.Items().Append(item);
            };
            appendInteractionMode(L"Select cases", "select");
            appendInteractionMode(L"Brush cases", "brush");
            if (ViewportNavigationGestureEnabled())
            {
                appendInteractionMode(L"Pan", "pan");
                appendInteractionMode(L"Zoom", "zoom");
            }
            selection.Items().Append(Controls::MenuFlyoutSeparator());
            for (auto const& option : ::rlispstat::core::ScatterplotSelectionModeMenuOptions())
            {
                auto item = Controls::ToggleMenuFlyoutItem();
                item.Text(to_hstring(option.title));
                item.IsChecked(currentModel_->selectionMode == option.value);
                item.Click([this, value = option.value](auto const&, auto const&)
                {
                    if (!currentModel_) return;
                    currentModel_->selectionMode = value;
                    if (commandCallback_ && !plotId_.empty())
                        commandCallback_({ "SELECTION_MODE", plotId_, value });
                    ConfigureContextMenu();
                });
                selection.Items().Append(item);
            }
            selection.Items().Append(Controls::MenuFlyoutSeparator());
        }
        auto selectAll = Controls::MenuFlyoutItem(); selectAll.Text(L"Select all visible cases");
        selectAll.Click([this](auto const&, auto const&)
        { ApplyVisibleSelection(::rlispstat::core::SelectionMode::Replace); });
        selection.Items().Append(selectAll);
        auto clear = Controls::MenuFlyoutItem(); clear.Text(L"Clear selection");
        clear.Click([this](auto const&, auto const&) { ClearSelection(); });
        selection.Items().Append(clear);
        auto invert = Controls::MenuFlyoutItem(); invert.Text(L"Invert selection");
        invert.Click([this](auto const&, auto const&)
        { ApplyVisibleSelection(::rlispstat::core::SelectionMode::Toggle); });
        selection.Items().Append(invert);
        if (currentModel_ &&
            ::rlispstat::core::PlotSupportsCaseExclusion(*currentModel_))
        {
            selection.Items().Append(Controls::MenuFlyoutSeparator());
            auto exclude = Controls::MenuFlyoutItem();
            excludeSelectedPlotItem_ = exclude;
            exclude.Text(L"Exclude selected cases from all analyses");
            exclude.Click([this](auto const&, auto const&)
            { DispatchPlotCommand({ "PLOT_EXCLUDE_SELECTED", plotId_ }); });
            selection.Items().Append(exclude);
            auto restore = Controls::MenuFlyoutItem();
            restorePlotScopeItem_ = restore;
            restore.Text(L"Include all cases again");
            restore.Click([this](auto const&, auto const&)
            { DispatchPlotCommand({ "PLOT_RESTORE_SCOPE", plotId_ }); });
            selection.Items().Append(restore);
        }

        if (ViewportNavigationGestureEnabled())
        {
            auto resetZoom = Controls::MenuFlyoutItem();
            resetZoom.Text(L"Reset zoom");
            resetZoom.Click([this](auto const&, auto const&)
            {
                if (!currentModel_) return;
                currentModel_->xmin = currentModel_->dataXmin;
                currentModel_->xmax = currentModel_->dataXmax;
                currentModel_->ymin = currentModel_->dataYmin;
                currentModel_->ymax = currentModel_->dataYmax;
                RenderPlot(std::max(360.0, plotCanvas_.ActualWidth()),
                           std::max(280.0, plotCanvas_.ActualHeight()));
                ConfigureContextMenu();
            });
            selection.Items().Append(resetZoom);
        }

        selection.Items().Append(Controls::MenuFlyoutSeparator());
        auto colors = Controls::MenuFlyoutSubItem(); colors.Text(L"Color selected points");
        selectionColorsMenu_ = colors;
        colors.IsEnabled(!selectedRows_.empty());
        for (auto const& color : ::rlispstat::core::PaletteColors())
        {
            auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(color.name));
            item.Click([this, name = color.name](auto const&, auto const&)
            {
                if (commandCallback_ && !group_.empty())
                    commandCallback_({ "SET_SELECTED_COLOR", group_, name });
            });
            colors.Items().Append(item);
        }
        colors.Items().Append(Controls::MenuFlyoutSeparator());
        auto resetColors = Controls::MenuFlyoutItem();
        resetColors.Text(to_hstring(::rlispstat::core::FormatDefaultResetColorMenuItemTitle()));
        resetColors.Click([this](auto const&, auto const&)
        {
            if (!commandCallback_ || group_.empty() || selectedRows_.empty()) return;
            std::vector<std::string> command{
                "CLEAR_ROW_COLORS", group_, std::to_string(selectedRows_.size()) };
            for (int row : selectedRows_) command.push_back(std::to_string(row));
            commandCallback_(command);
        });
        colors.Items().Append(resetColors);
        auto colorState = Controls::MenuFlyoutSubItem();
        colorState.Text(L"Colors");
        colorState.Items().Append(colors);
        colorState.Items().Append(commandItem(L"Open color palette…", "OPEN_COLOR_PALETTE"));
        if (colorOverrideMenu)
        {
            colorState.Items().Append(Controls::MenuFlyoutSeparator());
            colorState.Items().Append(colorOverrideMenu);
        }
        selection.Items().Append(colorState);
        if (!group_.empty())
        {
            selection.Items().Append(Controls::MenuFlyoutSeparator());
            auto addColumn = Controls::MenuFlyoutSubItem();
            addColumn.Text(to_hstring(
                ::rlispstat::core::DefaultPlotContextMenuTitles().addDataColumn));
            for (auto const& option : ::rlispstat::core::DataColumnCommandOptions(group_))
            {
                auto item = Controls::MenuFlyoutItem();
                item.Text(to_hstring(option.title));
                const std::string command = option.value == "selection"
                    ? "DATA_ADD_SELECTION_COLUMN" : "DATA_ADD_POINT_COLOR_COLUMN";
                item.Click([this, command](auto const&, auto const&)
                {
                    if (commandCallback_ && !group_.empty())
                        commandCallback_({command, group_});
                });
                addColumn.Items().Append(item);
            }
            selection.Items().Append(addColumn);
        }
        menu.Items().Append(selection);

        auto futureScope = Controls::MenuFlyoutSubItem();
        futureScope.Text(L"Analysis scope");
        auto selectedScope = Controls::ToggleMenuFlyoutItem();
        selectedFutureScopeItem_ = selectedScope;
        const std::vector<int> selectedScopeRows(selectedRows_.begin(), selectedRows_.end());
        selectedScope.Text(to_hstring("Current Selection (live; " +
            std::to_string(selectedScopeRows.size()) + ")"));
        selectedScope.IsEnabled(!selectedScopeRows.empty());
        selectedScope.IsChecked(::rlispstat::core::AnalysisScopeTracksCurrentSelection(
            analysisScope_));
        selectedScope.Click([this](auto const&, auto const&)
        {
            if (!commandCallback_ || group_.empty() || selectedRows_.empty()) return;
            std::vector<int> rows(selectedRows_.begin(), selectedRows_.end());
            std::vector<std::string> command{ "SET_ANALYSIS_SCOPE_FROM_ROWS", group_,
                std::to_string(rows.size()) };
            for (int row : rows) command.push_back(std::to_string(row));
            command.insert(command.end(), { "scatterplot_selection",
                "Current selection", plotId_ });
            commandCallback_(command);
        });
        futureScope.Items().Append(selectedScope);
        auto saveScope = Controls::MenuFlyoutItem();
        saveScope.Text(L"Save Current Selection as Scope...");
        saveScope.IsEnabled(!selectedScopeRows.empty());
        saveScope.Click([this](auto const&, auto const&)
        {
            if (commandCallback_ && !group_.empty())
                commandCallback_({ "SAVE_ANALYSIS_SCOPE_FROM_SELECTION", group_ });
        });
        futureScope.Items().Append(saveScope);
        if (applicationCommands_)
        {
            const auto saved = applicationCommands_->SavedSelections(group_);
            if (!saved.empty())
            {
                const auto activeName =
                    ::rlispstat::core::AnalysisScopeSelectionName(analysisScope_);
                for (auto const& savedSelection : saved)
                {
                    auto item = Controls::ToggleMenuFlyoutItem();
                    item.Text(to_hstring("Saved: " + savedSelection.name + " (" +
                        std::to_string(savedSelection.originalRowIds.size()) + ")"));
                    item.IsChecked(activeName.has_value() &&
                                   *activeName == savedSelection.name);
                    item.Click([this, name = savedSelection.name](auto const&, auto const&)
                    {
                        if (commandCallback_ && !group_.empty())
                            commandCallback_({ "USE_SAVED_ANALYSIS_SCOPE", group_, name });
                    });
                    futureScope.Items().Append(item);
                }
            }
        }

        std::vector<int> visibleRows;
        if (currentModel_)
        {
            const auto rows = ::rlispstat::core::VisibleRowsForModel(currentModel_);
            visibleRows.assign(rows.begin(), rows.end());
        }
        else
        {
            visibleRows.reserve(currentPlot_.points.size());
            for (auto const& point : currentPlot_.points)
                if (point.row > 0) visibleRows.push_back(point.row);
        }
        visibleRows = ::rlispstat::core::NormalizeAnalysisScopeRowIds(visibleRows);
        visibleFutureScopeRows_ = visibleRows;
        auto visibleScope = Controls::ToggleMenuFlyoutItem();
        visibleFutureScopeItem_ = visibleScope;
        visibleScope.Text(to_hstring("Plot (" +
            std::to_string(visibleRows.size()) + ")"));
        visibleScope.IsEnabled(!visibleRows.empty());
        visibleScope.IsChecked(
            !::rlispstat::core::AnalysisScopeSelectionName(analysisScope_).has_value() &&
            analysisScope_.sourceKind ==
                ::rlispstat::core::AnalysisScopeSourceKind::VisibleObservations &&
            ::rlispstat::core::AnalysisScopeMatchesRows(analysisScope_, visibleRows));
        visibleScope.Click([this, rows = std::move(visibleRows)](auto const&, auto const&)
        {
            if (!commandCallback_ || group_.empty()) return;
            std::vector<std::string> command{ "SET_ANALYSIS_SCOPE_FROM_ROWS", group_,
                std::to_string(rows.size()) };
            for (int row : rows) command.push_back(std::to_string(row));
            command.insert(command.end(), { "visible_observations",
                "Observations visible in plot", plotId_ });
            commandCallback_(command);
        });
        futureScope.Items().Append(visibleScope);

        auto allScope = Controls::ToggleMenuFlyoutItem();
        allFutureScopeItem_ = allScope;
        allScope.Text(to_hstring("All (" +
            std::to_string(analysisScope_.totalDatasetRows) + ")"));
        allScope.IsChecked(analysisScope_.kind ==
            ::rlispstat::core::AnalysisScopeKind::AllObservations);
        allScope.Click([this](auto const&, auto const&)
        {
            if (commandCallback_ && !group_.empty())
                commandCallback_({ "SET_ANALYSIS_SCOPE_ALL", group_ });
        });
        futureScope.Items().Append(allScope);
        if (currentModel_ && !currentModel_->isDatasetSeed &&
            !::rlispstat::core::PlotIsDerivedAnalysisView(*currentModel_))
        {
            futureScope.Items().Append(Controls::MenuFlyoutSeparator());
            auto liveScope = Controls::ToggleMenuFlyoutItem();
            liveScope.Text(L"Freeze This Plot at Current Scope");
            liveScope.IsChecked(currentModel_->analysisScopeFrozen);
            liveScope.Click([this](auto const&, auto const&)
            {
                if (commandCallback_ && !plotId_.empty())
                    commandCallback_({ "PLOT_TOGGLE_SCOPE_FREEZE", plotId_ });
            });
            futureScope.Items().Append(liveScope);
        }
        if (analyzeMenu.Items().Size() > 0)
            analyzeMenu.Items().Append(Controls::MenuFlyoutSeparator());
        analyzeMenu.Items().Append(futureScope);
        menu.Items().Append(analyzeMenu);
        }

        if (currentModel_ &&
            ::rlispstat::core::PlotSupportsObservationLabels(*currentModel_))
        {
            auto labels = Controls::MenuFlyoutSubItem(); labels.Text(L"Labels");
            auto labelColumn = Controls::MenuFlyoutSubItem(); labelColumn.Text(L"Label column");
            labelColumn.Items().Append(commandItem(L"None", "SET_LABEL_COLUMN",
                labelColumn_.empty(), "."));
            for (auto const& variable : currentModel_->variableMeta)
                labelColumn.Items().Append(commandItem(to_hstring(variable.name).c_str(),
                    "SET_LABEL_COLUMN", labelColumn_ == variable.name, variable.name));
            if (currentModel_->variableMeta.empty())
                for (auto const& variable : currentModel_->variables)
                    labelColumn.Items().Append(commandItem(to_hstring(variable.name).c_str(),
                        "SET_LABEL_COLUMN", labelColumn_ == variable.name, variable.name));
            labels.Items().Append(labelColumn);
            auto display = Controls::MenuFlyoutSubItem(); display.Text(L"Display");
            for (auto const& option : ::rlispstat::core::ScatterplotLabelDisplayMenuOptions(
                currentModel_->labelDisplayMode))
                display.Items().Append(commandItem(to_hstring(option.title).c_str(),
                    "SET_LABEL_DISPLAY", currentModel_->labelDisplayMode == option.value,
                    option.value));
            labels.Items().Append(display);
            displayMenu.Items().Append(labels);
        }

        auto annotate = Controls::MenuFlyoutSubItem();
        annotate.Text(note_.has_content || !stickyNotes_.empty() ? L"Annotate •" : L"Annotate");
        auto toggleNotes = Controls::ToggleMenuFlyoutItem();
        toggleNotes.Text(note_.visible ? L"Hide Notes" :
            (note_.has_content ? L"Show Notes •" : L"Show Notes"));
        toggleNotes.IsChecked(note_.visible);
        toggleNotes.Click([this](auto const&, auto const&) { SetNotesVisible(!note_.visible); });
        annotate.Items().Append(toggleNotes);
        auto newSticker = Controls::MenuFlyoutItem(); newSticker.Text(L"New Sticker");
        newSticker.Click([this](auto const&, auto const&) { AddStickyNote(); });
        annotate.Items().Append(newSticker);
        annotate.Items().Append(Controls::MenuFlyoutSeparator());
        auto clearNotes = Controls::MenuFlyoutItem(); clearNotes.Text(L"Clear Notes");
        clearNotes.IsEnabled(note_.has_content);
        clearNotes.Click([this](auto const&, auto const&)
        {
            ::rlispstat::core::ClearWindowNote(note_);
            updatingNotes_ = true; notesText_.Text(L""); updatingNotes_ = false;
            ConfigureContextMenu();
        });
        annotate.Items().Append(clearNotes);
        AppendSnapshotAlbumContextItems(window_, annotate);
        menu.Items().Append(annotate);

        auto exportMenu = Controls::MenuFlyoutSubItem(); exportMenu.Text(L"Export");
        auto exportCapabilities = ::rlispstat::core::StandardVisualExportCapabilities(
            ::rlispstat::core::ExportPlatform::Windows);
        const auto exportActions = ::rlispstat::core::BuildExportMenuActions(
            exportCapabilities,
            ::rlispstat::core::ExportPlatform::Windows);
        auto copyMenu = Controls::MenuFlyoutSubItem(); copyMenu.Text(L"Copy");
        auto saveMenu = Controls::MenuFlyoutSubItem(); saveMenu.Text(L"Save");
        for (auto const& action : exportActions)
        {
            auto destination = action.command.rfind("COPY_", 0) == 0
                ? copyMenu : saveMenu;
            if (action.separatorBefore && destination.Items().Size() > 0)
                destination.Items().Append(Controls::MenuFlyoutSeparator());
            auto item = Controls::MenuFlyoutItem();
            item.Text(to_hstring(action.title));
            item.Click([this, command = action.command](auto const&, auto const&)
            {
                if (command == "COPY_RICH") CopyPlotRichAsync(true);
                else if (command == "COPY_SVG") CopyPlotAsSvgAsync();
                else if (command == "COPY_PNG") CopyPlotAsPngAsync();
                else if (command == "SAVE_SVG") SavePlotAsync("SVG");
                else if (command == "SAVE_PDF") SavePlotAsync("PDF");
                else if (command == "SAVE_PNG") SavePlotAsync("PNG");
            });
            destination.Items().Append(item);
        }
        if (!aggregateDiagnostic)
        {
        auto copyRows = Controls::MenuFlyoutItem();
        copySelectedRowsItem_ = copyRows;
        copyRows.Text(L"Copy selected row indices");
        copyRows.IsEnabled(!selectedRows_.empty());
        copyRows.Click([this](auto const&, auto const&)
        {
            std::ostringstream text;
            bool first = true;
            for (int row : selectedRows_)
            {
                if (!first) text << '\n';
                text << row; first = false;
            }
            Windows::ApplicationModel::DataTransfer::DataPackage package;
            package.SetText(to_hstring(text.str()));
            Windows::ApplicationModel::DataTransfer::Clipboard::SetContent(package);
            Windows::ApplicationModel::DataTransfer::Clipboard::Flush();
        });
        copyMenu.Items().Append(copyRows);
        }
        if (copyMenu.Items().Size() > 0) exportMenu.Items().Append(copyMenu);
        if (saveMenu.Items().Size() > 0) exportMenu.Items().Append(saveMenu);
        AppendPlotRCodeExportItems(exportMenu, commandCallback_, currentModel_);
        menu.Items().Append(exportMenu);

        AttachWindowContextFlyout(window_, menu, false);
        RefreshContextSelectionState();
    }

    void ScatterPlotView::RefreshContextSelectionState()
    {
        const bool any = !selectedRows_.empty();
        if (selectionColorsMenu_) selectionColorsMenu_.IsEnabled(any);
        if (copySelectedRowsItem_) copySelectedRowsItem_.IsEnabled(any);
        if (excludeSelectedPlotItem_)
        {
            const auto visible = ::rlispstat::core::VisibleRowsForModel(currentModel_);
            excludeSelectedPlotItem_.IsEnabled(std::any_of(
                selectedRows_.begin(), selectedRows_.end(),
                [this, &visible](int row) { return visible.count(row) != 0 &&
                    excludedRows_.count(row) == 0; }));
        }
        if (restorePlotScopeItem_)
            restorePlotScopeItem_.IsEnabled(!excludedRows_.empty());
        RefreshAnalysisScopeMenuState();
    }

    void ScatterPlotView::RefreshAnalysisScopeMenuState()
    {
        if (selectedFutureScopeItem_)
        {
            const std::vector<int> rows(selectedRows_.begin(), selectedRows_.end());
            selectedFutureScopeItem_.Text(to_hstring("Selected (live; " +
                std::to_string(rows.size()) + ")"));
            selectedFutureScopeItem_.IsEnabled(!rows.empty());
            selectedFutureScopeItem_.IsChecked(
                ::rlispstat::core::AnalysisScopeTracksCurrentSelection(analysisScope_));
        }
        if (visibleFutureScopeItem_)
        {
            visibleFutureScopeItem_.IsEnabled(!visibleFutureScopeRows_.empty());
            visibleFutureScopeItem_.IsChecked(
                !::rlispstat::core::AnalysisScopeSelectionName(analysisScope_).has_value() &&
                analysisScope_.sourceKind ==
                    ::rlispstat::core::AnalysisScopeSourceKind::VisibleObservations &&
                ::rlispstat::core::AnalysisScopeMatchesRows(
                    analysisScope_, visibleFutureScopeRows_));
        }
        if (allFutureScopeItem_)
        {
            allFutureScopeItem_.Text(to_hstring("Included (" +
                std::to_string(analysisScope_.totalDatasetRows -
                    excludedRows_.size()) + ")"));
            allFutureScopeItem_.IsChecked(analysisScope_.kind ==
                ::rlispstat::core::AnalysisScopeKind::AllObservations ||
                analysisScope_.sourceKind ==
                    ::rlispstat::core::AnalysisScopeSourceKind::IncludedObservations);
        }
    }

    void ScatterPlotView::SetAnalysisScope(::rlispstat::core::AnalysisScope scope)
    {
        analysisScope_ = std::move(scope);
        // Geometry is updated centrally by App when the global scope changes;
        // this method keeps the menu checks synchronized with that same scope.
        RefreshAnalysisScopeMenuState();
    }

    void ScatterPlotView::SetExcludedRows(std::set<int> rows)
    {
        excludedRows_ = std::move(rows);
        RefreshContextSelectionState();
    }

    void ScatterPlotView::SetApplicationCommands(
        std::shared_ptr<ApplicationCommandRegistry> registry, std::string token)
    {
        if (applicationCommands_ == registry && applicationWindowToken_ == token) return;
        applicationCommands_ = registry;
        applicationWindowToken_ = token;
        std::weak_ptr<ScatterPlotView> weak = shared_from_this();
        window_.Activated([weak, registry, token](auto const&, WindowActivatedEventArgs const& args)
        {
            if (args.WindowActivationState() != WindowActivationState::Deactivated &&
                !weak.expired() && registry)
                registry->ActivateContext(token);
        });
    }

    void ScatterPlotView::Show(::rlispstat::core::SessionPlot const& plot)
    {
        if (plotId_ != plot.id) { note_ = ::rlispstat::core::MakeWindowNote(plot.id); stickyNotes_.clear(); }
        currentModel_ = nullptr;
        currentPlot_ = plot;
        plotId_ = plot.id;
        group_ = plot.group;
        smoothSpanControl_.Visibility(Visibility::Collapsed);
        pointSizeControl_.Visibility(pointSizeControlVisible_
            ? Visibility::Visible : Visibility::Collapsed);
        if (!embedded_)
        {
            SetWindowDataSheetGroup(window_, group_);
            SetWindowSnapshotSource(window_, "plot", plotId_, group_);
            ConfigureContextMenu();
            window_.Title(to_hstring("Scatterplot: " + plot.title));
        }
        RenderPlot(std::max(360.0, plotCanvas_.ActualWidth()),
                   std::max(280.0, plotCanvas_.ActualHeight()));
    }

    void ScatterPlotView::Show(::rlispstat::core::PlotModel& plot)
    {
        if (plotId_ != plot.id) { note_ = ::rlispstat::core::MakeWindowNote(plot.id); stickyNotes_.clear(); }
        currentModel_ = &plot;
        currentPlot_ = {};
        currentPlot_.id = plot.id;
        currentPlot_.group = plot.group;
        currentPlot_.xLabel = plot.presentationXLabel.empty()
            ? plot.xLabel : plot.presentationXLabel;
        currentPlot_.yLabel = plot.presentationYLabel.empty()
            ? plot.yLabel : plot.presentationYLabel;
        currentPlot_.title = plot.title;
        for (auto const& point : plot.points)
            currentPlot_.points.push_back({ point.x, point.y, point.row });
        plotId_ = plot.id;
        group_ = plot.group;
        pointSizeScale_ = ::rlispstat::core::ClampPointSizeScale(plot.pointSizeScale);
        pointSizeControlVisible_ = plot.pointSizeSliderVisible;
        updatingPointSize_ = true;
        pointSizeSlider_.Value(pointSizeScale_);
        pointSizeLabel_.Text(to_hstring("Point size " +
            std::to_string(static_cast<int>(std::lround(pointSizeScale_ * 100.0))) + "%"));
        updatingPointSize_ = false;
        updatingSmoothSpan_ = true;
        const bool histogram = plot.kind == "histogram";
        const double smoothValue = histogram ? plot.histogramDensityAdjust : ::rlispstat::core::ClampSmoothSpan(plot.smoothSpan);
        smoothSpanSlider_.Minimum(histogram ? std::min(.1, smoothValue) : .2);
        smoothSpanSlider_.Maximum(histogram ? std::max(3.0, smoothValue) : 2.0);
        smoothSpanSlider_.Value(smoothValue);
        smoothSpanLabel_.Text(to_hstring(std::string(histogram ? "Smoothing ×" : "Span ") + ::rlispstat::core::FormatDouble(smoothValue, 2)));
        updatingSmoothSpan_ = false;
        const bool supportsSmooth = (plot.kind == "scatter" && plot.glmDiagnosticKind != "roc_curve") ||
            (plot.kind == "trellis_scatterplot" && plot.trellisSpecificationInitialized &&
             plot.trellisSpecification.plotType == ::rlispstat::core::TrellisPlotType::Scatter);
        smoothSpanControl_.Visibility((histogram ? plot.histogramShowDensity && plot.histogramDensityMode != "none" : plot.smoothShowSpanSlider && supportsSmooth)
            ? Visibility::Visible : Visibility::Collapsed);
        pointSizeControl_.Visibility(pointSizeControlVisible_ && SupportsPointSizeControl()
            ? Visibility::Visible : Visibility::Collapsed);
        if (!embedded_)
        {
            SetWindowDataSheetGroup(window_, group_);
            SetWindowSnapshotSource(window_, "plot", plotId_, group_);
            ConfigureContextMenu();
            std::string windowTitle = ::rlispstat::core::TitleForPlot(plot);
            if (plot.dataScopeCaptured)
                windowTitle += " — " + ::rlispstat::core::AnalysisScopeWindowSummary(
                    plot.dataScope, plot.dataScope.totalDatasetRows);
            if (plot.analysisScopeFrozen) windowTitle += " — Frozen";
            window_.Title(to_hstring(windowTitle));
        }
        RenderPlot(std::max(360.0, plotCanvas_.ActualWidth()),
                   std::max(280.0, plotCanvas_.ActualHeight()));
    }

    void ScatterPlotView::SetVisualStyle(
        std::string themeName,
        std::vector<std::pair<int, std::string>> const& pointColors)
    {
        ::rlispstat::windows::performance::Scope timing(
            "Plot.SetVisualStyle",
            "plot=" + plotId_ + ",colors=" + std::to_string(pointColors.size()));
        globalThemeName_ = std::move(themeName);
        const std::string effectiveTheme = localThemeOverride_.value_or(globalThemeName_);
        const bool themeChanged = themeName_ != effectiveTheme;
        std::map<int, std::string> updated;
        if (currentModel_) updated = currentModel_->colorByRowColors;
        for (auto const& [row, color] : pointColors)
        {
            if (row > 0 && !color.empty()) updated[row] = color;
        }
        std::set<int> changed;
        for (auto const& [row, color] : pointColors_)
        {
            auto found = updated.find(row);
            if (found == updated.end() || found->second != color) changed.insert(row);
        }
        for (auto const& [row, color] : updated)
        {
            auto found = pointColors_.find(row);
            if (found == pointColors_.end() || found->second != color) changed.insert(row);
        }
        const auto legendColorsAreAccurate = [this](
            std::map<int, std::string> const& displayedColors)
        {
            return !currentModel_ ||
                ::rlispstat::core::PlotColorLegendMatchesLinkedRowColors(
                    *currentModel_, displayedColors);
        };
        const bool legendAccuracyChanged =
            legendColorsAreAccurate(pointColors_) != legendColorsAreAccurate(updated);
        themeName_ = effectiveTheme;
        pointColors_ = std::move(updated);
        if (themeChanged) ConfigureContextMenu();
        else RefreshContextSelectionState();
        if (!themeChanged && !legendAccuracyChanged &&
            SupportsIncrementalPointUpdates() && !changed.empty())
            RefreshPointVisuals(changed);
        else if (themeChanged || !changed.empty())
            RenderPlot(std::max(360.0, plotCanvas_.ActualWidth()),
                       std::max(280.0, plotCanvas_.ActualHeight()));
    }

    void ScatterPlotView::ApplyGlobalTheme(std::string themeName)
    {
        globalThemeName_ = std::move(themeName);
        localThemeOverride_.reset();
        if (themeName_ == globalThemeName_)
        {
            ConfigureContextMenu();
            return;
        }
        themeName_ = globalThemeName_;
        ConfigureContextMenu();
        RenderPlot(std::max(360.0, plotCanvas_.ActualWidth()),
                   std::max(280.0, plotCanvas_.ActualHeight()));
    }

    void ScatterPlotView::SetLocalTheme(std::string themeName)
    {
        localThemeOverride_ = std::move(themeName);
        if (themeName_ == *localThemeOverride_)
        {
            ConfigureContextMenu();
            return;
        }
        themeName_ = *localThemeOverride_;
        ConfigureContextMenu();
        RenderPlot(std::max(360.0, plotCanvas_.ActualWidth()),
                   std::max(280.0, plotCanvas_.ActualHeight()));
    }

    void ScatterPlotView::SetDataFrame(
        ::rlispstat::core::DataFrameModel const* dataframe)
    {
        if (!dataframe)
        {
            dataFrame_ = {};
            hasDataFrame_ = false;
            return;
        }
        dataFrame_ = *dataframe;
        hasDataFrame_ = true;
    }

    void ScatterPlotView::RefreshVariableMetadata(
        ::rlispstat::core::DataFrameModel const* dataframe)
    {
        SetDataFrame(dataframe);
        if (currentModel_) ConfigureContextMenu();
    }

    void ScatterPlotView::SetLabelState(
        std::string labelColumn, std::map<int, std::string> rowLabels)
    {
        const std::string displayMode = currentModel_
            ? currentModel_->labelDisplayMode : "none";
        if (labelColumn_ == labelColumn && rowLabels_ == rowLabels &&
            renderedLabelDisplayMode_ == displayMode) return;
        labelColumn_ = std::move(labelColumn);
        rowLabels_ = std::move(rowLabels);
        renderedLabelDisplayMode_ = displayMode;
        if (labelRefreshQueued_) return;
        labelRefreshQueued_ = true;
        auto refresh = [weak = weak_from_this()]()
        {
            auto self = weak.lock();
            if (!self) return;
            self->labelRefreshQueued_ = false;
            if (self->closed_) return;
            // A label command originates in the plot's ContextFlyout. Rebuilding
            // and detaching the owning canvas synchronously from that Click event
            // can leave WinUI's popup input shield active, so subsequent point
            // clicks never reach PointerPressed. Run after the flyout has closed.
            self->ConfigureContextMenu();
            self->RenderPlot(std::max(360.0, self->plotCanvas_.ActualWidth()),
                             std::max(280.0, self->plotCanvas_.ActualHeight()));
        };
        auto dispatcher = DispatcherQueue();
        if (!dispatcher || !dispatcher.TryEnqueue(refresh)) refresh();
    }

    void ScatterPlotView::SetNotesVisible(bool visible)
    {
        const bool wasVisible = note_.visible;
        if (wasVisible == visible) return;
        const double clientWidth = root_.ActualWidth() > 0.0
            ? root_.ActualWidth() : 820.0;
        const double clientHeight = root_.ActualHeight() > 0.0
            ? root_.ActualHeight() : 620.0;
        ::rlispstat::core::SetWindowNoteVisible(note_, visible);
        notesPanel_.Visibility(visible ? Visibility::Visible : Visibility::Collapsed);
        // The notes column is additional workspace. Grow/shrink the window by
        // exactly its width so the plot column retains its arranged size.
        ResizeLinkEDAWindowClient(window_,
            std::max(360.0, clientWidth + (visible ? 270.0 : -270.0)),
            clientHeight);
        if (visible)
        {
            updatingNotes_ = true; notesText_.Text(to_hstring(note_.plain_text)); updatingNotes_ = false;
            includeNotes_.IsChecked(note_.include_in_export);
            notesText_.Focus(FocusState::Programmatic);
        }
        // Visibility changes a Grid column, not the window bounds.  Force the
        // plot to consume the newly arranged column width immediately instead
        // of waiting for a later resize/pointer event.
        root_.UpdateLayout();
        RenderPlot(std::max(360.0, plotCanvas_.ActualWidth()),
                   std::max(280.0, plotCanvas_.ActualHeight()));
        ConfigureContextMenu();
    }

    bool ScatterPlotView::SupportsPointSizeControl() const
    {
        if (!currentModel_) return !currentPlot_.points.empty();
        if (currentModel_->kind == "histogram" || currentModel_->kind == "barplot")
            return false;
        if (currentModel_->kind == "boxplot")
            return currentModel_->boxplotShowPoints;
        if (currentModel_->kind == "trellis_scatterplot" &&
            currentModel_->trellisSpecificationInitialized)
        {
            const auto type = currentModel_->trellisSpecification.plotType;
            return type == ::rlispstat::core::TrellisPlotType::Scatter ||
                type == ::rlispstat::core::TrellisPlotType::TimeSeries ||
                type == ::rlispstat::core::TrellisPlotType::Boxplot;
        }
        return !currentModel_->points.empty() ||
            currentModel_->kind == "scatter_matrix" ||
            currentModel_->kind == "pca_scree";
    }

    void ScatterPlotView::AddStickyNote()
    {
        auto note = ::rlispstat::core::MakeWindowStickyNote(
            ::rlispstat::core::GenerateWindowStickyNoteId());
        const double offset = 18.0 * static_cast<double>(stickyNotes_.size() % 8);
        ::rlispstat::core::MoveWindowStickyNote(note, 24.0 + offset, 24.0 + offset);
        stickyNotes_.push_back(std::move(note));
        RenderStickyNotes();
        ConfigureContextMenu();
    }

    void ScatterPlotView::RenderStickyNotes()
    {
        if (!annotationCanvas_) return;
        annotationCanvas_.Children().Clear();
        stickyVisuals_.clear();
        if (activeStickyIndex_ >= static_cast<int>(stickyNotes_.size()))
            activeStickyIndex_ = -1;
        annotationCanvas_.Width(std::max(360.0, plotCanvas_.ActualWidth()));
        annotationCanvas_.Height(std::max(280.0, plotCanvas_.ActualHeight()));
        const auto stickyBrush = [](::rlispstat::core::StickyNoteColor color)
        {
            switch (color)
            {
            case ::rlispstat::core::StickyNoteColor::Pink: return Brush(255, 184, 214);
            case ::rlispstat::core::StickyNoteColor::Blue: return Brush(184, 222, 250);
            case ::rlispstat::core::StickyNoteColor::Green: return Brush(199, 240, 179);
            case ::rlispstat::core::StickyNoteColor::Orange: return Brush(255, 207, 153);
            default: return Brush(255, 240, 143);
            }
        };
        for (std::size_t index = 0; index < stickyNotes_.size(); ++index)
        {
            auto& note = stickyNotes_[index];
            const double displayedWidth = note.collapsed ? 32.0 : note.width;
            const double displayedHeight = note.collapsed ? 28.0 : note.height;
            auto leader = Shapes::Line();
            leader.X1(note.x + displayedWidth / 2.0);
            leader.Y1(note.y + displayedHeight / 2.0);
            leader.X2(note.anchor_x); leader.Y2(note.anchor_y);
            leader.Stroke(Brush(56, 56, 56)); leader.Opacity(0.72);
            leader.StrokeThickness(1.5);
            annotationCanvas_.Children().Append(leader);
            auto anchor = Shapes::Ellipse(); anchor.Width(10); anchor.Height(10);
            anchor.Fill(Brush(51, 51, 51));
            Controls::Canvas::SetLeft(anchor, note.anchor_x - 5);
            Controls::Canvas::SetTop(anchor, note.anchor_y - 5);
            annotationCanvas_.Children().Append(anchor);

            auto sticker = Controls::Border();
            sticker.Width(displayedWidth); sticker.Height(displayedHeight);
            sticker.Background(stickyBrush(note.color));
            sticker.BorderBrush(Brush(85, 85, 85));
            sticker.BorderThickness(Thickness{
                activeStickyIndex_ == static_cast<int>(index) ? 2.0 : 1.0});
            sticker.CornerRadius(CornerRadius{5});
            if (note.collapsed)
            {
                auto expand = Controls::Button(); expand.Content(box_value(L"…"));
                expand.Padding(Thickness{0});
                expand.HorizontalAlignment(HorizontalAlignment::Stretch);
                expand.VerticalAlignment(VerticalAlignment::Stretch);
                Controls::ToolTipService::SetToolTip(
                    expand, box_value(L"Expand this note"));
                expand.Click([this, index](auto const&, auto const&)
                {
                    if (index >= stickyNotes_.size()) return;
                    ::rlispstat::core::SetWindowStickyNoteCollapsed(
                        stickyNotes_[index], false);
                    activeStickyIndex_ = static_cast<int>(index);
                    RenderStickyNotes(); ConfigureContextMenu();
                });
                sticker.Child(expand);
                stickyVisuals_.push_back(sticker);
                Controls::Canvas::SetLeft(sticker, note.x);
                Controls::Canvas::SetTop(sticker, note.y);
                annotationCanvas_.Children().Append(sticker);
                continue;
            }
            auto layout = Controls::Grid();
            auto headerRow = Controls::RowDefinition(); headerRow.Height(GridLength{24});
            auto bodyRow = Controls::RowDefinition();
            bodyRow.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
            layout.RowDefinitions().Append(headerRow);
            layout.RowDefinitions().Append(bodyRow);
            auto header = Controls::Grid();
            header.Background(stickyBrush(note.color));
            header.Opacity(0.88);
            auto titleColumn = Controls::ColumnDefinition();
            titleColumn.Width(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
            auto collapseColumn = Controls::ColumnDefinition(); collapseColumn.Width(GridLength{28});
            auto colorColumn = Controls::ColumnDefinition(); colorColumn.Width(GridLength{28});
            auto closeColumn = Controls::ColumnDefinition(); closeColumn.Width(GridLength{28});
            header.ColumnDefinitions().Append(titleColumn);
            header.ColumnDefinitions().Append(collapseColumn);
            header.ColumnDefinitions().Append(colorColumn);
            header.ColumnDefinitions().Append(closeColumn);
            auto collapseButton = Controls::Button(); collapseButton.Content(box_value(L"−"));
            collapseButton.Padding(Thickness{2});
            Controls::ToolTipService::SetToolTip(collapseButton, box_value(L"Collapse to marker"));
            Controls::Grid::SetColumn(collapseButton, 1); header.Children().Append(collapseButton);
            auto colorButton = Controls::Button(); colorButton.Content(box_value(L"\u25cf"));
            colorButton.Padding(Thickness{2});
            Controls::ToolTipService::SetToolTip(colorButton, box_value(L"Sticker color"));
            Controls::Grid::SetColumn(colorButton, 2); header.Children().Append(colorButton);
            auto closeButton = Controls::Button(); closeButton.Content(box_value(L"\u00d7"));
            closeButton.Padding(Thickness{2});
            Controls::ToolTipService::SetToolTip(closeButton, box_value(L"Delete Sticker"));
            Controls::Grid::SetColumn(closeButton, 3); header.Children().Append(closeButton);
            layout.Children().Append(header);
            auto editor = Controls::TextBox();
            editor.AcceptsReturn(true); editor.TextWrapping(TextWrapping::Wrap);
            editor.Text(to_hstring(note.plain_text));
            editor.HorizontalAlignment(HorizontalAlignment::Stretch);
            editor.VerticalAlignment(VerticalAlignment::Stretch);
            editor.Padding(Thickness{8}); editor.Background(stickyBrush(note.color));
            editor.BorderThickness(Thickness{0});
            editor.PlaceholderText(L"Write a note…");
            Controls::Grid::SetRow(editor, 1); layout.Children().Append(editor);
            auto grip = Controls::Primitives::Thumb();
            grip.Width(16); grip.Height(16);
            grip.HorizontalAlignment(HorizontalAlignment::Right);
            grip.VerticalAlignment(VerticalAlignment::Bottom);
            Controls::Grid::SetRow(grip, 1); layout.Children().Append(grip);
            sticker.Child(layout);
            stickyVisuals_.push_back(sticker);
            Controls::Canvas::SetLeft(sticker, note.x);
            Controls::Canvas::SetTop(sticker, note.y);
            editor.TextChanged([this, index, editor](auto const&, auto const&)
            {
                if (index >= stickyNotes_.size()) return;
                ::rlispstat::core::SetWindowStickyNoteText(stickyNotes_[index], to_string(editor.Text()));
                ConfigureContextMenu();
            });
            auto removeSticker = [this, index]()
            {
                if (index >= stickyNotes_.size()) return;
                stickyNotes_.erase(stickyNotes_.begin() + static_cast<std::ptrdiff_t>(index));
                activeStickyIndex_ = -1;
                RenderStickyNotes(); ConfigureContextMenu();
            };
            closeButton.Click([removeSticker](auto const&, auto const&) { removeSticker(); });
            collapseButton.Click([this, index](auto const&, auto const&)
            {
                if (index >= stickyNotes_.size()) return;
                ::rlispstat::core::SetWindowStickyNoteCollapsed(stickyNotes_[index], true);
                activeStickyIndex_ = -1;
                RenderStickyNotes(); ConfigureContextMenu();
            });
            auto colorMenu = Controls::MenuFlyout(); colorMenu.AreOpenCloseAnimationsEnabled(false);
            const std::array<std::pair<::rlispstat::core::StickyNoteColor, wchar_t const*>, 5> colors{{
                {::rlispstat::core::StickyNoteColor::Yellow, L"Yellow"},
                {::rlispstat::core::StickyNoteColor::Pink, L"Pink"},
                {::rlispstat::core::StickyNoteColor::Blue, L"Blue"},
                {::rlispstat::core::StickyNoteColor::Green, L"Green"},
                {::rlispstat::core::StickyNoteColor::Orange, L"Orange"}
            }};
            for (auto const& [color, name] : colors)
            {
                auto colorItem = Controls::ToggleMenuFlyoutItem(); colorItem.Text(name);
                colorItem.IsChecked(note.color == color);
                colorItem.Click([this, index, color](auto const&, auto const&)
                {
                    if (index >= stickyNotes_.size()) return;
                    ::rlispstat::core::SetWindowStickyNoteColor(stickyNotes_[index], color);
                    activeStickyIndex_ = static_cast<int>(index);
                    RenderStickyNotes();
                });
                colorMenu.Items().Append(colorItem);
            }
            colorButton.Flyout(colorMenu);
            grip.DragDelta([this, index, sticker, leader](auto const&, Controls::Primitives::DragDeltaEventArgs const& event)
            {
                if (index >= stickyNotes_.size()) return;
                auto& current = stickyNotes_[index];
                ::rlispstat::core::ResizeWindowStickyNote(current,
                    current.width + event.HorizontalChange(),
                    current.height + event.VerticalChange());
                sticker.Width(current.width); sticker.Height(current.height);
                leader.X1(current.x + current.width / 2.0);
                leader.Y1(current.y + current.height / 2.0);
            });
            // Match macOS: drag the small header normally; the editor remains
            // dedicated to text selection and editing.  The anchor stays put.
            header.PointerPressed([this, index, header](auto const&, auto const& event)
            {
                if (index >= stickyNotes_.size()) return;
                ActivateStickyNote(static_cast<int>(index));
                const auto point = event.GetCurrentPoint(annotationCanvas_);
                stickyDragIndex_ = static_cast<int>(index);
                stickyDragStartX_ = point.Position().X;
                stickyDragStartY_ = point.Position().Y;
                stickyOriginalX_ = stickyNotes_[index].x;
                stickyOriginalY_ = stickyNotes_[index].y;
                header.CapturePointer(event.Pointer()); event.Handled(true);
            });
            header.PointerMoved([this, sticker, leader](auto const&, auto const& event)
            {
                if (stickyDragIndex_ < 0 ||
                    static_cast<std::size_t>(stickyDragIndex_) >= stickyNotes_.size()) return;
                const auto point = event.GetCurrentPoint(annotationCanvas_).Position();
                ::rlispstat::core::MoveWindowStickyNote(stickyNotes_[stickyDragIndex_],
                    stickyOriginalX_ + point.X - stickyDragStartX_,
                    stickyOriginalY_ + point.Y - stickyDragStartY_);
                auto const& current = stickyNotes_[stickyDragIndex_];
                Controls::Canvas::SetLeft(sticker, current.x);
                Controls::Canvas::SetTop(sticker, current.y);
                leader.X1(current.x + current.width / 2.0);
                leader.Y1(current.y + current.height / 2.0);
                event.Handled(true);
            });
            header.PointerReleased([this](auto const&, auto const& event)
            {
                if (stickyDragIndex_ < 0) return;
                stickyDragIndex_ = -1; event.Handled(true);
            });
            anchor.PointerPressed([this, index, anchor](auto const&, auto const& event)
            {
                ActivateStickyNote(static_cast<int>(index));
                stickyAnchorDragIndex_ = static_cast<int>(index);
                anchor.CapturePointer(event.Pointer()); event.Handled(true);
            });
            anchor.PointerMoved([this, anchor, leader](auto const&, auto const& event)
            {
                if (stickyAnchorDragIndex_ < 0 ||
                    static_cast<std::size_t>(stickyAnchorDragIndex_) >= stickyNotes_.size()) return;
                const auto point = event.GetCurrentPoint(annotationCanvas_).Position();
                ::rlispstat::core::MoveWindowStickyNoteAnchor(
                    stickyNotes_[stickyAnchorDragIndex_], point.X, point.Y);
                Controls::Canvas::SetLeft(anchor, point.X - 5);
                Controls::Canvas::SetTop(anchor, point.Y - 5);
                leader.X2(point.X); leader.Y2(point.Y);
                event.Handled(true);
            });
            anchor.PointerReleased([this](auto const&, auto const& event)
            { stickyAnchorDragIndex_ = -1; event.Handled(true); });
            editor.GotFocus([this, index](auto const&, auto const&)
            { ActivateStickyNote(static_cast<int>(index)); });
            annotationCanvas_.Children().Append(sticker);
        }
    }

    void ScatterPlotView::ActivateStickyNote(int index)
    {
        activeStickyIndex_ = index;
        for (std::size_t visualIndex = 0; visualIndex < stickyVisuals_.size(); ++visualIndex)
            stickyVisuals_[visualIndex].BorderThickness(Thickness{
                static_cast<int>(visualIndex) == activeStickyIndex_ ? 2.0 : 1.0});
    }

    void ScatterPlotView::AttachAxisVariableMenu(
        Controls::TextBlock const& label, bool xAxis)
    {
        if (!currentModel_ || !label) return;
        auto menu = Controls::MenuFlyout();
        menu.AreOpenCloseAnimationsEnabled(false);
        if (currentModel_->isGLMDiagnostic && xAxis &&
            currentModel_->regressionDerivedKind == "partial_regression_plot")
        {
            for (auto const& term : currentModel_->regressionPartialAvailableTerms) {
                auto item = Controls::ToggleMenuFlyoutItem();
                item.Text(to_hstring(term));
                item.IsChecked(currentModel_->regressionDerivedTerm == term);
                item.Click([this, term](auto const&, auto const&) {
                    if (commandCallback_) commandCallback_({"SET_PARTIAL_PLOT_TERM", plotId_, term});
                });
                menu.Items().Append(item);
            }
        }
        else if (currentModel_->isGLMDiagnostic)
        {
            if (!::rlispstat::core::DiagnosticPlotAxisUsesResidualChoice(*currentModel_, xAxis)) return;
            for (auto const& id : ::rlispstat::core::DiagnosticPlotResidualChoices(*currentModel_)) {
                auto item = Controls::ToggleMenuFlyoutItem();
                item.Text(to_hstring(::rlispstat::core::GeneralizedResidualTypeLabel(id)));
                item.IsChecked(currentModel_->displayedResidualType == id);
                item.Click([this, id](auto const&, auto const&) {
                    if (commandCallback_) commandCallback_({"SET_DIAGNOSTIC_RESIDUAL_TYPE", plotId_, id});
                });
                menu.Items().Append(item);
            }
        }
        else if (currentModel_->kind == "glm_interaction")
        {
            // The fitted response on Y is immutable.  Only members of the
            // displayed interaction are valid alternatives for X.
            if (!xAxis) return;
            const auto effectVariables = ::rlispstat::core::SplitInteractionTerm(
                currentModel_->glmInteractionTerm);
            if (effectVariables.size() < 2) return;
            for (auto const& variable : effectVariables)
            {
                auto item = Controls::ToggleMenuFlyoutItem();
                item.Text(to_hstring(variable));
                item.IsChecked(variable == currentModel_->xLabel);
                item.Click([this, variable](auto const&, auto const&)
                {
                    if (commandCallback_ && !plotId_.empty())
                        commandCallback_({
                            "PLOT_REGRESSION_SET_EFFECT_X", plotId_, variable
                        });
                });
                menu.Items().Append(item);
            }
        }
        else if (currentModel_->kind == "pca_biplot")
        {
            const int current = xAxis ? currentModel_->biplotXComponent
                                      : currentModel_->biplotYComponent;
            const int other = xAxis ? currentModel_->biplotYComponent
                                    : currentModel_->biplotXComponent;
            const std::string command = xAxis ? "PCA_BIPLOT_SET_X"
                                              : "PCA_BIPLOT_SET_Y";
            for (auto const& [component, componentLabel] :
                 currentModel_->biplotComponentOptions)
            {
                auto item = Controls::ToggleMenuFlyoutItem();
                item.Text(to_hstring(componentLabel));
                item.IsChecked(component == current);
                item.IsEnabled(component != other);
                item.Click([this, command, component](auto const&, auto const&)
                {
                    if (commandCallback_ && !plotId_.empty())
                        commandCallback_({command, plotId_, std::to_string(component)});
                });
                menu.Items().Append(item);
            }
            if (menu.Items().Size() == 0) return;
        }
        else if (currentModel_->kind == "trellis_scatterplot" &&
                 currentModel_->trellisSpecification.plotType ==
                     ::rlispstat::core::TrellisPlotType::Bar && !xAxis)
        {
            if (!hasDataFrame_) return;
            auto none = Controls::ToggleMenuFlyoutItem();
            none.Text(L"None");
            none.IsChecked(currentModel_->trellisSpecification.splitVariableId.empty());
            none.Click([this](auto const&, auto const&)
            {
                if (commandCallback_ && !plotId_.empty())
                    commandCallback_({"TRELLIS_SCATTERPLOT_SET_SPLIT", plotId_, "."});
            });
            menu.Items().Append(none);
            menu.Items().Append(Controls::MenuFlyoutSeparator());
            for (auto const& column : dataFrame_.columns)
            {
                if (column.name == currentModel_->trellisSpecification.xVariableId ||
                    !::rlispstat::core::TrellisConditionColumnIsValid(column, dataFrame_.rows))
                    continue;
                bool conditioning = false;
                for (auto const& condition :
                     currentModel_->trellisSpecification.conditioningVariables)
                    conditioning = conditioning || condition.variableId == column.name;
                if (conditioning) continue;
                auto item = Controls::ToggleMenuFlyoutItem();
                item.Text(to_hstring(column.name));
                item.IsChecked(column.name ==
                    currentModel_->trellisSpecification.splitVariableId);
                item.Click([this, variable = column.name](auto const&, auto const&)
                {
                    if (commandCallback_ && !plotId_.empty())
                        commandCallback_({"TRELLIS_SCATTERPLOT_SET_SPLIT", plotId_, variable});
                });
                menu.Items().Append(item);
            }
        }
        else
        {
            if (currentModel_->variables.empty()) return;
        const std::string command = currentModel_->kind == "time_series"
            ? (xAxis ? "TIME_SERIES_SET_X" : "TIME_SERIES_SET_Y")
            : (currentModel_->kind == "trellis_scatterplot"
                ? (xAxis ? "TRELLIS_SCATTERPLOT_SET_X" : "TRELLIS_SCATTERPLOT_SET_Y")
                : (xAxis ? "SET_XVAR" : "SET_YVAR"));
        const std::string current = xAxis ? currentModel_->xLabel : currentModel_->yLabel;
        for (auto const& variable : currentModel_->variables)
        {
            auto item = Controls::ToggleMenuFlyoutItem();
            item.Text(to_hstring(variable.name));
            item.IsChecked(variable.name == current);
            item.Click([this, command, variable = variable.name](auto const&, auto const&)
            {
                if (commandCallback_ && !plotId_.empty())
                    commandCallback_({ command, plotId_, variable });
            });
            menu.Items().Append(item);
        }
        if (kEnableExperimentalPlotVariableTypeMenu)
        {
            menu.Items().Append(Controls::MenuFlyoutSeparator());
            menu.Items().Append(CreateVariableTypeMenu(current));
        }
        }
        const bool histogram = currentModel_->kind == "histogram";
        // Histogram secondary clicks bubble to the window's full options menu.
        if (!histogram) label.ContextFlyout(menu);
        label.IsHitTestVisible(true);
        Controls::Canvas::SetZIndex(label, 20);
        label.Padding(Thickness{5, 3, 5, 3});
        label.PointerPressed([menu, label, histogram](auto const&, auto const& event)
        {
            if (histogram && event.GetCurrentPoint(label).Properties().IsRightButtonPressed()) return;
            menu.ShowAt(label);
            event.Handled(true);
        });
    }

    void ScatterPlotView::AttachBoxplotGroupingVariableMenu(
        Controls::TextBlock const& label, std::string const& variable, bool trellis)
    {
        if (!label || variable.empty() || !currentModel_ || !hasDataFrame_) return;
        std::vector<std::string> groups = trellis
            ? currentModel_->trellisSpecification.boxplotGroupingVariableIds
            : currentModel_->boxplotGroupingVariables;
        if (groups.empty() && !currentModel_->xLabel.empty())
            groups.push_back(currentModel_->xLabel);

        auto menu = Controls::MenuFlyout();
        menu.AreOpenCloseAnimationsEnabled(false);
        for (auto const& column : dataFrame_.columns)
        {
            const bool used = std::find(groups.begin(), groups.end(), column.name) != groups.end();
            if ((used && column.name != variable) ||
                column.name == currentModel_->yLabel ||
                !::rlispstat::core::DataColumnLooksGroupingCandidate(column, dataFrame_.rows))
                continue;
            auto item = Controls::ToggleMenuFlyoutItem();
            item.Text(to_hstring(column.name));
            item.IsChecked(column.name == variable);
            item.IsEnabled(column.name != variable);
            item.Click([this, variable, replacement = column.name, trellis]
                (auto const&, auto const&)
            {
                if (!commandCallback_ || plotId_.empty()) return;
                commandCallback_({trellis
                    ? "TRELLIS_SCATTERPLOT_BOXPLOT_REPLACE_GROUP"
                    : "BOXPLOT_REPLACE_GROUPING_VARIABLE",
                    plotId_, variable, replacement});
            });
            menu.Items().Append(item);
        }
        if (menu.Items().Size() == 0) return;
        label.ContextFlyout(menu);
        label.IsHitTestVisible(true);
        label.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        label.Padding(Thickness{5, 3, 5, 3});
        Controls::Canvas::SetZIndex(label, 30);
        label.PointerPressed([menu, label](auto const&, auto const& event)
        {
            if (event.GetCurrentPoint(label).Properties().IsRightButtonPressed()) return;
            menu.ShowAt(label);
            event.Handled(true);
        });
    }

    void ScatterPlotView::AttachBoxplotSelection(
        Controls::TextBlock const& label, std::vector<int> rows)
    {
        if (!label || rows.empty()) return;
        label.IsHitTestVisible(true);
        label.Padding(Thickness{4, 2, 4, 2});
        Controls::Canvas::SetZIndex(label, 30);
        label.PointerPressed([this, label, rows = std::move(rows)]
            (auto const&, auto const& event)
        {
            if (event.GetCurrentPoint(label).Properties().IsRightButtonPressed()) return;
            if (!RowSelectionGestureEnabled()) return;
            if (selectionCallback_ && !group_.empty())
                selectionCallback_(group_, std::set<int>(rows.begin(), rows.end()),
                    SelectionModeFor(event.KeyModifiers(),
                        currentModel_ ? currentModel_->selectionMode : "replace"));
            event.Handled(true);
        });
    }

    Controls::MenuFlyoutSubItem ScatterPlotView::CreateVariableTypeMenu(
        std::string const& variable)
    {
        auto typeMenu = Controls::MenuFlyoutSubItem();
        typeMenu.Text(L"Variable type");

        std::string currentType;
        if (hasDataFrame_)
        {
            auto found = std::find_if(dataFrame_.columns.begin(), dataFrame_.columns.end(),
                [&](auto const& column) { return column.name == variable; });
            if (found != dataFrame_.columns.end())
                currentType = ::rlispstat::core::NormalizeVariableType(found->type);
        }
        if (currentType.empty() && currentModel_)
        {
            auto found = std::find_if(currentModel_->variableMeta.begin(),
                currentModel_->variableMeta.end(),
                [&](auto const& metadata) { return metadata.name == variable; });
            if (found != currentModel_->variableMeta.end())
                currentType = ::rlispstat::core::NormalizeVariableType(found->type);
        }
        if (currentType.empty() && currentModel_ &&
            ::rlispstat::core::FindNumericVariable(currentModel_->variables, variable))
            currentType = "numeric";
        if (currentType == "logical") currentType = "factor";
        typeMenu.IsEnabled(!currentType.empty());

        for (auto const& option : ::rlispstat::core::VariableViewTypeOptions())
        {
            auto item = Controls::ToggleMenuFlyoutItem();
            item.Text(to_hstring(option.title));
            item.IsChecked(currentType == option.value);
            item.Click([this, variable, value = option.value](auto const&, auto const&)
            {
                if (commandCallback_ && !group_.empty())
                    commandCallback_({ "SET_VARIABLE_TYPE", group_, variable, value });
            });
            typeMenu.Items().Append(item);
        }
        return typeMenu;
    }

    void ScatterPlotView::AttachVariableTypeMenu(
        Controls::TextBlock const& label, std::string const& variable)
    {
        if (!kEnableExperimentalPlotVariableTypeMenu) return;
        if (!label || variable.empty()) return;
        auto menu = Controls::MenuFlyout();
        menu.AreOpenCloseAnimationsEnabled(false);
        menu.Items().Append(CreateVariableTypeMenu(variable));
        label.ContextFlyout(menu);
        label.IsHitTestVisible(true);
        Controls::Canvas::SetZIndex(label, 20);
        label.Padding(Thickness{5, 3, 5, 3});
    }

    void ScatterPlotView::AttachNumericVariableReplacementMenu(
        Controls::TextBlock const& label,
        std::string const& currentVariable,
        std::string const& replacementToken,
        std::vector<std::string> const& currentVariables,
        std::string const& command)
    {
        if (!label || !currentModel_ || currentVariable.empty() ||
            replacementToken.empty() || command.empty()) return;
        auto menu = Controls::MenuFlyout();
        menu.AreOpenCloseAnimationsEnabled(false);
        for (auto const& variable : currentModel_->variables)
        {
            const bool usedElsewhere = variable.name != currentVariable &&
                std::find(currentVariables.begin(), currentVariables.end(),
                          variable.name) != currentVariables.end();
            if (usedElsewhere) continue;
            auto item = Controls::ToggleMenuFlyoutItem();
            item.Text(to_hstring(variable.name));
            item.IsChecked(variable.name == currentVariable);
            item.IsEnabled(variable.name != currentVariable);
            item.Click([this, command, replacementToken,
                        replacement = variable.name](auto const&, auto const&)
            {
                if (commandCallback_ && !plotId_.empty())
                    commandCallback_({command, plotId_, replacementToken, replacement});
            });
            menu.Items().Append(item);
        }
        if (menu.Items().Size() == 0) return;
        if (kEnableExperimentalPlotVariableTypeMenu)
        {
            menu.Items().Append(Controls::MenuFlyoutSeparator());
            menu.Items().Append(CreateVariableTypeMenu(currentVariable));
        }
        label.ContextFlyout(menu);
        label.IsHitTestVisible(true);
        label.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        label.Padding(Thickness{5, 3, 5, 3});
        Controls::Canvas::SetZIndex(label, 30);
        label.PointerPressed([menu, label](auto const&, auto const& event)
        {
            if (event.GetCurrentPoint(label).Properties().IsRightButtonPressed()) return;
            menu.ShowAt(label);
            event.Handled(true);
        });
    }

    void ScatterPlotView::AttachTrellisConditionMenu(
        FrameworkElement const& target,
        std::vector<std::size_t> conditionIndices)
    {
        if (!target || !currentModel_ || currentModel_->kind != "trellis_scatterplot" ||
            conditionIndices.empty()) return;
        auto const& conditions = currentModel_->trellisSpecification.conditioningVariables;
        conditionIndices.erase(std::remove_if(conditionIndices.begin(), conditionIndices.end(),
            [&](std::size_t index) { return index >= conditions.size(); }), conditionIndices.end());
        if (conditionIndices.empty()) return;

        auto menu = Controls::MenuFlyout();
        menu.AreOpenCloseAnimationsEnabled(false);
        auto appendCommands = [this](auto const& items,
                                     ::rlispstat::core::TrellisConditioningVariable const& condition)
        {
            auto const& specification = currentModel_->trellisSpecification;
            const auto conditionPosition = static_cast<std::size_t>(std::distance(
                specification.conditioningVariables.begin(),
                std::find_if(specification.conditioningVariables.begin(),
                    specification.conditioningVariables.end(), [&](auto const& item)
                    { return item.variableId == condition.variableId; })));
            bool numeric = false;
            if (hasDataFrame_)
            {
                auto found = std::find_if(dataFrame_.columns.begin(), dataFrame_.columns.end(),
                    [&](auto const& column) { return column.name == condition.variableId; });
                numeric = found != dataFrame_.columns.end() &&
                    ::rlispstat::core::NormalizeVariableType(found->type) == "numeric";
            }
            auto replace = Controls::MenuFlyoutSubItem(); replace.Text(L"Replace with");
            for (auto const& column : dataFrame_.columns)
            {
                if (column.name == condition.variableId) continue;
                const bool usedElsewhere = std::any_of(specification.conditioningVariables.begin(),
                    specification.conditioningVariables.end(), [&](auto const& other)
                    { return other.variableId != condition.variableId && other.variableId == column.name; });
                const bool axisVariable = specification.plotType !=
                        ::rlispstat::core::TrellisPlotType::DataTable &&
                    (column.name == specification.xVariableId || column.name == specification.yVariableId);
                const bool compatible = condition.kind ==
                        ::rlispstat::core::TrellisConditioningVariableKind::ContinuousBinned
                    ? (::rlispstat::core::NormalizeVariableType(column.type) == "numeric" &&
                       ::rlispstat::core::DataColumnAllowsNumeric(column))
                    : ::rlispstat::core::TrellisConditionColumnIsValid(column, dataFrame_.rows);
                if (usedElsewhere || axisVariable || !compatible) continue;
                auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(column.name));
                item.Click([this, current = condition.variableId, replacement = column.name]
                    (auto const&, auto const&)
                {
                    DispatchPlotCommand({"TRELLIS_SCATTERPLOT_SET_CONDITION", plotId_,
                        current, replacement});
                });
                replace.Items().Append(item);
            }
            replace.IsEnabled(replace.Items().Size() > 0);
            items.Append(replace);

            auto place = Controls::MenuFlyoutSubItem(); place.Text(L"Place in");
            for (auto const& entry : std::array<std::tuple<std::wstring, std::string,
                     ::rlispstat::core::TrellisDimension>, 3>{ {
                     {L"Rows", "TRELLIS_SCATTERPLOT_DIMENSION_ROWS", ::rlispstat::core::TrellisDimension::Rows},
                     {L"Columns", "TRELLIS_SCATTERPLOT_DIMENSION_COLUMNS", ::rlispstat::core::TrellisDimension::Columns},
                     {L"Nested", "TRELLIS_SCATTERPLOT_DIMENSION_NESTED", ::rlispstat::core::TrellisDimension::Nested} } })
            {
                auto item = Controls::ToggleMenuFlyoutItem(); item.Text(std::get<0>(entry));
                item.IsChecked(condition.dimension == std::get<2>(entry));
                item.Click([this, command = std::get<1>(entry), variable = condition.variableId]
                    (auto const&, auto const&)
                {
                    DispatchPlotCommand({command, plotId_, variable});
                });
                place.Items().Append(item);
            }
            items.Append(place);

            auto commandToggle = [this, &items, &condition](std::wstring const& text,
                                                            std::string command,
                                                            bool checked)
            {
                auto item = Controls::ToggleMenuFlyoutItem();
                item.Text(text); item.IsChecked(checked);
                item.Click([this, command = std::move(command), variable = condition.variableId]
                    (auto const&, auto const&)
                {
                    DispatchPlotCommand({command, plotId_, variable});
                });
                items.Append(item);
            };
            if (numeric)
            {
                items.Append(Controls::MenuFlyoutSeparator());
                commandToggle(L"Treat predictor as categorical", "TRELLIS_SCATTERPLOT_SET_CONDITION_FACTOR",
                    condition.kind == ::rlispstat::core::TrellisConditioningVariableKind::Categorical &&
                    !condition.orderedCategories);
                commandToggle(L"Treat predictor as ordinal", "TRELLIS_SCATTERPLOT_SET_CONDITION_ORDERED",
                    condition.kind == ::rlispstat::core::TrellisConditioningVariableKind::Categorical &&
                    condition.orderedCategories);
                items.Append(Controls::MenuFlyoutSeparator());
                const auto binning = condition.binning.value_or(
                    ::rlispstat::core::TrellisContinuousBinningSpecification{});
                commandToggle(L"Equal-width intervals", "TRELLIS_SCATTERPLOT_SET_CONDITION_EQUAL_WIDTH",
                    condition.kind == ::rlispstat::core::TrellisConditioningVariableKind::ContinuousBinned &&
                    binning.method == ::rlispstat::core::TrellisContinuousBinningMethod::EqualWidth);
                commandToggle(L"Equal-count intervals", "TRELLIS_SCATTERPLOT_SET_CONDITION_EQUAL_COUNT",
                    condition.kind == ::rlispstat::core::TrellisConditioningVariableKind::ContinuousBinned &&
                    binning.method == ::rlispstat::core::TrellisContinuousBinningMethod::EqualCount);
                auto bins = Controls::MenuFlyoutSubItem(); bins.Text(L"Number of bins");
                bins.IsEnabled(condition.kind ==
                    ::rlispstat::core::TrellisConditioningVariableKind::ContinuousBinned);
                for (std::size_t count = 2; count <= 6; ++count)
                {
                    auto item = Controls::ToggleMenuFlyoutItem();
                    item.Text(to_hstring(count)); item.IsChecked(binning.binCount == count);
                    item.Click([this, variable = condition.variableId, count]
                        (auto const&, auto const&)
                    {
                        DispatchPlotCommand({"TRELLIS_SCATTERPLOT_CONDITION_BINS", plotId_,
                            variable, std::to_string(count)});
                    });
                    bins.Items().Append(item);
                }
                items.Append(bins);
            }
            items.Append(Controls::MenuFlyoutSeparator());
            auto earlier = Controls::MenuFlyoutItem(); earlier.Text(L"Move earlier");
            earlier.IsEnabled(conditionPosition > 0);
            earlier.Click([this, variable = condition.variableId](auto const&, auto const&)
            {
                DispatchPlotCommand({"TRELLIS_SCATTERPLOT_MOVE_CONDITION_EARLIER", plotId_, variable});
            });
            items.Append(earlier);
            auto later = Controls::MenuFlyoutItem(); later.Text(L"Move later");
            later.IsEnabled(conditionPosition + 1 < specification.conditioningVariables.size());
            later.Click([this, variable = condition.variableId](auto const&, auto const&)
            {
                DispatchPlotCommand({"TRELLIS_SCATTERPLOT_MOVE_CONDITION_LATER", plotId_, variable});
            });
            items.Append(later);
            items.Append(Controls::MenuFlyoutSeparator());
            auto remove = Controls::MenuFlyoutItem(); remove.Text(L"Remove conditioning variable");
            remove.Click([this, variable = condition.variableId](auto const&, auto const&)
            {
                DispatchPlotCommand({"TRELLIS_SCATTERPLOT_REMOVE_CONDITION", plotId_, variable});
            });
            items.Append(remove);
        };

        if (conditionIndices.size() == 1)
            appendCommands(menu.Items(), conditions[conditionIndices.front()]);
        else
        {
            for (std::size_t index : conditionIndices)
            {
                auto variableMenu = Controls::MenuFlyoutSubItem();
                variableMenu.Text(to_hstring(conditions[index].variableLabel));
                appendCommands(variableMenu.Items(), conditions[index]);
                menu.Items().Append(variableMenu);
            }
        }
        // The points canvas spans the full plot and is added after the facet
        // decorations. Keep condition strips above it so their local menu is
        // available anywhere on the band, not only on its text.
        target.IsHitTestVisible(true);
        Controls::Canvas::SetZIndex(target, 30);
        target.ContextFlyout(menu);
    }

    bool ScatterPlotView::SupportsIncrementalPointUpdates() const
    {
        if (pointVisuals_.empty() || currentPlot_.points.empty()) return false;
        if (currentModel_)
        {
            const auto& kind = currentModel_->kind;
            if (kind == "boxplot" || kind == "histogram" || kind == "barplot" ||
                kind == "time_series" || kind == "trellis_scatterplot" ||
                kind == "scatter_matrix" || kind == "pca_scree") return false;
            // Series selection changes fitted-line emphasis and the legend,
            // not only the observed point glyphs.  A full render is therefore
            // required for interaction/effect plots and any future plot that
            // uses the shared series model.
            if (!currentModel_->interactionPlotLines.empty()) return false;
        }
        return true;
    }

    ::rlispstat::core::ScatterplotRenderPlan
    ScatterPlotView::BuildCurrentScatterRenderPlan() const
    {
        std::vector<::rlispstat::core::ScatterplotPointValue> pointValues;
        pointValues.reserve(currentPlot_.points.size());
        for (auto const& sample : currentPlot_.points)
            pointValues.push_back({ sample.row, sample.x, sample.y });
        ::rlispstat::core::ScatterplotRenderInput input;
        input.points = std::move(pointValues);
        input.viewport = renderedViewport_;
        input.plotRect = renderedPlotRect_;
        input.selectedRows = selectedRows_;
        input.rowColors = pointColors_;
        input.rowLabels = rowLabels_;
        input.labelDisplayMode = currentModel_ ? currentModel_->labelDisplayMode : "none";
        input.selectedColorName = "black";
        if (currentModel_)
        {
            if (currentModel_->diagnosticShowImputationUncertainty &&
                !currentModel_->diagnosticImputationValues.empty())
            {
                input.pointImputationValues =
                    &currentModel_->diagnosticImputationValues;
                input.completeImputationPointValues =
                    &currentModel_->diagnosticAllImputationValues;
                input.directlyImputedModelRows =
                    currentModel_->diagnosticRowsWithImputedModelInputs;
                input.distinguishDiagnosticImputationRows = true;
            }
            if (currentModel_->kind == "scatter" && hasDataFrame_ &&
                ::rlispstat::core::DataFrameShowsAllImputations(dataFrame_))
            {
                input.imputationDataFrame = &dataFrame_;
                input.xColumn = ::rlispstat::core::FindDataColumnInDataFrame(
                    dataFrame_, currentModel_->xLabel);
                input.yColumn = ::rlispstat::core::FindDataColumnInDataFrame(
                    dataFrame_, currentModel_->yLabel);
                input.imputationUncertaintyMode =
                    currentModel_->scatterImputationUncertainty;
            }
            for (auto const& overlay : currentModel_->overlays)
                input.overlays.push_back({ overlay.type, overlay.source, overlay.visible });
            input.smoothCurves = currentModel_->smoothCurves;
            input.shadeOverlap = currentModel_->scatterShadeOverlap;
            input.sizeByOverlap = currentModel_->scatterSizeByOverlap;
            input.sizeByVisualOverlap = currentModel_->kind == "pca_biplot";
            input.showFitConfidenceIntervals =
                currentModel_->scatterFitConfidenceIntervalsVisible;
            input.showSmoothConfidenceIntervals =
                currentModel_->scatterSmoothConfidenceIntervalsVisible;
        }
        auto plan = ::rlispstat::core::BuildScatterplotRenderPlan(input);
        if (currentModel_ && currentModel_->kind == "glm_interaction" &&
            !currentModel_->interactionPlotLines.empty())
            for (auto& point : plan.points)
            {
                if (point.selected || point.hasExplicitColor) continue;
                point.fillAlpha = std::min(point.fillAlpha, 0.34);
                point.strokeAlpha = std::min(point.strokeAlpha, 0.28);
                point.radius = std::min(point.radius, 2.7);
            }
        return plan;
    }

    std::vector<UIElement> ScatterPlotView::CreatePointVisuals(
        ::rlispstat::core::ScatterplotPointDrawItem const& item,
        ::rlispstat::core::PlotThemeStyleSpec const& theme)
    {
        std::vector<UIElement> visuals;
        const int baseZ = item.selected ? 30 : 20;
        auto append = [this, &visuals, baseZ](UIElement const& element, int offset = 0)
        {
            element.IsHitTestVisible(false);
            Controls::Canvas::SetZIndex(element, baseZ + offset);
            pointsCanvas_.Children().Append(element);
            visuals.push_back(element);
        };
        double radius = item.radius * pointSizeScale_;
        const auto explicitColor = item.selected
            ? ::rlispstat::core::PlotSelectedColorForNameOrHex(
                item.colorName.empty() ? "black" : item.colorName)
            : ::rlispstat::core::PlotColorForNameOrHex(
                item.colorName.empty() ? "black" : item.colorName);
        const auto fillColor = theme.coordinatedMarks && !item.hasExplicitColor
            ? (item.selected ? theme.selectedMarkFill
                : (theme.hollowUnselectedMarks ? theme.panel : theme.markFill))
            : explicitColor;
        const auto strokeColor = theme.coordinatedMarks
            ? (item.selected ? theme.selectedMarkStroke
                : (item.hasExplicitColor ? explicitColor : theme.markStroke))
            : explicitColor;

        if (item.hasImputationGlyph)
        {
            const auto& rect = item.imputationGlyph.rect;
            const bool twoAxis = item.imputationGlyph.axisMask == 3;
            const bool dimmed = !selectedRows_.empty() && !item.selected;
            const double dim = dimmed ? 0.62 : 1.0;
            const double fillAlpha = (item.selected ? 0.26 : 0.17) * dim;
            const double centerAlpha = (item.selected ? 0.62 : 0.38) * dim;
            const double edgeAlpha = (item.selected ? 0.02 : 0.01) * dim;
            const auto glyphColor = theme.coordinatedMarks && !item.hasExplicitColor
                ? (item.selected ? theme.selectedMarkFill : theme.markFill)
                : ::rlispstat::core::PlotColorForNameOrHex(
                    item.colorName.empty() ? "black" : item.colorName);
            const auto glyphStrokeColor = theme.coordinatedMarks
                ? (item.hasExplicitColor
                    ? glyphColor
                    : (item.selected ? theme.selectedMarkStroke : theme.markStroke))
                : glyphColor;

            Shapes::Shape glyph{ nullptr };
            if (twoAxis)
            {
                glyph = Shapes::Ellipse().as<Shapes::Shape>();
            }
            else
            {
                auto rounded = Shapes::Rectangle();
                const double corner = std::min(rect.width, rect.height) / 2.0;
                rounded.RadiusX(corner);
                rounded.RadiusY(corner);
                glyph = rounded.as<Shapes::Shape>();
            }
            glyph.Width(rect.width);
            glyph.Height(rect.height);
            if (twoAxis)
            {
                auto gradient = Media::RadialGradientBrush();
                gradient.Center({ 0.5, 0.5 });
                gradient.GradientOrigin({ 0.5, 0.5 });
                gradient.RadiusX(0.5);
                gradient.RadiusY(0.5);
                gradient.GradientStops().Append(
                    GradientStop(glyphColor, centerAlpha, 0.0));
                gradient.GradientStops().Append(
                    GradientStop(glyphColor, fillAlpha, 0.48));
                gradient.GradientStops().Append(
                    GradientStop(glyphColor, edgeAlpha, 1.0));
                glyph.Fill(gradient);
            }
            else
            {
                auto gradient = Media::LinearGradientBrush();
                if (item.imputationGlyph.axisMask == 1)
                {
                    gradient.StartPoint({ 0.0, 0.5 });
                    gradient.EndPoint({ 1.0, 0.5 });
                }
                else
                {
                    gradient.StartPoint({ 0.5, 0.0 });
                    gradient.EndPoint({ 0.5, 1.0 });
                }
                gradient.GradientStops().Append(
                    GradientStop(glyphColor, edgeAlpha, 0.0));
                gradient.GradientStops().Append(
                    GradientStop(glyphColor, centerAlpha, 0.5));
                gradient.GradientStops().Append(
                    GradientStop(glyphColor, edgeAlpha, 1.0));
                glyph.Fill(gradient);
            }
            glyph.Stroke(Brush(glyphStrokeColor));
            glyph.StrokeThickness(item.selected
                ? std::max(1.4, theme.selectedMarkStrokeWidth) : 0.8);
            Controls::Canvas::SetLeft(glyph, rect.x);
            Controls::Canvas::SetTop(glyph, rect.y);
            append(glyph);
        }
        else if (item.selected)
        {
            for (auto const& ring : CreateSelectionRingVisuals(item.point, radius, theme,
                     item.hasExplicitColor ? explicitColor : theme.selectedMarkFill))
                append(ring, -1);
        }
        if (!item.hasImputationGlyph && !item.selected && theme.crossUnselectedMarks)
        {
            const double arm = radius * 0.62;
            append(Line(item.point.x - arm, item.point.y, item.point.x + arm,
                item.point.y, Brush(::rlispstat::core::PlotMarkColor(strokeColor, theme.panel, item.shadeOverlap, item.strokeAlpha)), item.strokeWidth));
            append(Line(item.point.x, item.point.y - arm, item.point.x,
                item.point.y + arm, Brush(::rlispstat::core::PlotMarkColor(strokeColor, theme.panel, item.shadeOverlap, item.strokeAlpha)), item.strokeWidth));
        }
        else if (!item.hasImputationGlyph)
        {
            Shapes::Shape mark = item.selected && theme.squareSelectedMarks
                ? Shapes::Rectangle().as<Shapes::Shape>()
                : Shapes::Ellipse().as<Shapes::Shape>();
            mark.Width(radius * 2.0); mark.Height(radius * 2.0);
            mark.Fill(Brush(::rlispstat::core::PlotMarkColor(fillColor, theme.panel, item.shadeOverlap, item.fillAlpha)));
            mark.Stroke(Brush(::rlispstat::core::PlotMarkColor(strokeColor, theme.panel, item.shadeOverlap, item.strokeAlpha)));
            mark.StrokeThickness(theme.coordinatedMarks && item.selected
                ? theme.selectedMarkStrokeWidth : item.strokeWidth);
            Controls::Canvas::SetLeft(mark, item.point.x - radius);
            Controls::Canvas::SetTop(mark, item.point.y - radius);
            append(mark);
        }
        if (item.showLabel)
        {
            auto label = Label(to_hstring(item.label).c_str(),
                item.labelAnchor.x + 6.0, item.labelAnchor.y - 18.0,
                10.5, Brush(theme.text));
            append(label, 1);
        }
        return visuals;
    }

    std::vector<UIElement> ScatterPlotView::CreateSelectionRingVisuals(
        ::rlispstat::core::Point const& point, double pointRadius,
        ::rlispstat::core::PlotThemeStyleSpec const& theme,
        ::rlispstat::core::PlotRGBA color) const
    {
        (void)theme;
        std::vector<UIElement> visuals;
        const double ringRadius = pointRadius + 1.8 * pointSizeScale_;
        auto ring = Shapes::Ellipse();
        ring.Width(ringRadius * 2.0); ring.Height(ringRadius * 2.0);
        ring.Fill(Brush(color, 0.28));
        ring.Stroke(TransparentBrush()); ring.StrokeThickness(0.0);
        ring.IsHitTestVisible(false);
        Controls::Canvas::SetLeft(ring, point.x - ringRadius);
        Controls::Canvas::SetTop(ring, point.y - ringRadius);
        visuals.push_back(ring);
        return visuals;
    }

    void ScatterPlotView::RefreshPointVisuals(std::set<int> const& rows)
    {
        ::rlispstat::windows::performance::Scope timing(
            "Plot.RefreshPointVisuals",
            "plot=" + plotId_ + ",changed=" + std::to_string(rows.size()));
        if (rows.empty()) return;
        auto children = pointsCanvas_.Children();
        if (rows.size() > 64 && rows.size() * 4 >= pointVisuals_.size())
        {
            std::size_t visualCount = 0;
            for (auto const& entry : pointVisuals_)
                visualCount += entry.second.size();
            if (pointVisualStartIndex_ <= children.Size() &&
                children.Size() - pointVisualStartIndex_ == visualCount)
            {
                // Point glyphs occupy the tail of this canvas. Remove them
                // together instead of searching the WinUI child collection
                // separately for every point in a large selection change.
                while (children.Size() > pointVisualStartIndex_)
                    children.RemoveAtEnd();
                pointVisuals_.clear();
                const auto theme = ::rlispstat::core::PlotThemeStyleForName(themeName_);
                auto plan = BuildCurrentScatterRenderPlan();
                for (auto const& item : plan.points)
                {
                    auto visuals = CreatePointVisuals(item, theme);
                    auto& stored = pointVisuals_[item.caseId];
                    stored.insert(stored.end(), visuals.begin(), visuals.end());
                }
                return;
            }
        }
        for (int row : rows)
        {
            auto existing = pointVisuals_.find(row);
            if (existing == pointVisuals_.end()) continue;
            for (auto const& visual : existing->second)
            {
                uint32_t index = 0;
                if (children.IndexOf(visual, index)) children.RemoveAt(index);
            }
            pointVisuals_.erase(existing);
        }
        const auto theme = ::rlispstat::core::PlotThemeStyleForName(themeName_);
        auto plan = BuildCurrentScatterRenderPlan();
        for (auto const& item : plan.points)
        {
            if (!rows.count(item.caseId)) continue;
            auto visuals = CreatePointVisuals(item, theme);
            auto& stored = pointVisuals_[item.caseId];
            stored.insert(stored.end(), visuals.begin(), visuals.end());
        }
    }

    void ScatterPlotView::RefreshScatterOverlayVisuals()
    {
        if (!scatterOverlayCanvas_) return;
        scatterOverlayCanvas_.Children().Clear();
        if (currentModel_ && currentModel_->isGLMDiagnostic) return;
        const auto theme = ::rlispstat::core::PlotThemeStyleForName(themeName_);
        const auto plan = BuildCurrentScatterRenderPlan();
        for (const auto& overlay : plan.overlayLines)
        {
            const auto color = overlay.useDefaultDarkColor ? theme.geomStroke
                : ::rlispstat::core::PlotColorForNameOrHex(overlay.colorName);
            auto line = Line(overlay.start.x, overlay.start.y,
                overlay.end.x, overlay.end.y,
                Brush(color, overlay.alpha), overlay.lineWidth);
            if (overlay.dashed)
            {
                auto dashes = Media::DoubleCollection();
                dashes.Append(4.0); dashes.Append(3.0);
                line.StrokeDashArray(dashes);
            }
            scatterOverlayCanvas_.Children().Append(line);
        }
    }

    void ScatterPlotView::RefreshInteractionSeriesVisuals()
    {
        if (!currentModel_ || interactionSeriesVisuals_.size() !=
            currentModel_->interactionPlotLines.size()) return;
        std::vector<bool> selectedSeries(interactionSeriesVisuals_.size(), false);
        bool anySelectedSeries = false;
        for (std::size_t index = 0; index < selectedSeries.size(); ++index)
        {
            selectedSeries[index] = ::rlispstat::core::PlotSeriesIntersectsSelection(
                *currentModel_, index, selectedRows_);
            anySelectedSeries = anySelectedSeries || selectedSeries[index];
        }
        for (std::size_t index = 0; index < selectedSeries.size(); ++index)
        {
            const auto& series = currentModel_->interactionPlotLines[index];
            auto& visual = interactionSeriesVisuals_[index];
            const bool active = selectedSeries[index];
            const double alpha = anySelectedSeries && !active ? 0.22 : 0.96;
            const auto color = ::rlispstat::core::PlotColorForNameOrHex(
                series.colorKey.empty() ? "black" : series.colorKey);
            for (const auto& line : visual.intervalLines)
                line.Stroke(Brush(color, alpha));
            for (const auto& band : visual.confidenceBands)
                band.Fill(Brush(color,
                    anySelectedSeries && !active ? 0.055 : 0.16));
            if (visual.connectedLine)
            {
                visual.connectedLine.Stroke(Brush(color, alpha));
                visual.connectedLine.StrokeThickness(active ? 3.8 : 2.2);
            }
            const double diameter = active ? 7.6 : 6.0;
            for (std::size_t markerIndex = 0;
                 markerIndex < visual.markers.size(); ++markerIndex)
            {
                auto& marker = visual.markers[markerIndex];
                marker.Width(diameter); marker.Height(diameter);
                marker.Fill(Brush(color, alpha));
                marker.Stroke(Brush(color, alpha));
                if (markerIndex < visual.markerCenters.size())
                {
                    const auto& center = visual.markerCenters[markerIndex];
                    Controls::Canvas::SetLeft(marker, center.x - diameter / 2.0);
                    Controls::Canvas::SetTop(marker, center.y - diameter / 2.0);
                }
            }
            if (visual.legendSample)
            {
                visual.legendSample.Stroke(Brush(color,
                    anySelectedSeries && !active ? 0.22 : 1.0));
                visual.legendSample.StrokeThickness(active ? 3.8 : 2.2);
            }
            if (visual.legendMarker)
            {
                visual.legendMarker.Width(diameter);
                visual.legendMarker.Height(diameter);
                visual.legendMarker.Fill(Brush(color,
                    anySelectedSeries && !active ? 0.22 : 1.0));
                Controls::Canvas::SetLeft(visual.legendMarker,
                    visual.legendMarkerCenter.x - diameter / 2.0);
                Controls::Canvas::SetTop(visual.legendMarker,
                    visual.legendMarkerCenter.y - diameter / 2.0);
            }
            if (visual.legendLabel)
                visual.legendLabel.FontWeight(active
                    ? Windows::UI::Text::FontWeights::Bold()
                    : Windows::UI::Text::FontWeights::Medium());
        }
    }

    void ScatterPlotView::RefreshSelection(std::set<int> const& selectedRows)
    {
        ::rlispstat::windows::performance::Scope timing(
            "Plot.RefreshSelection",
            "plot=" + plotId_ + ",selected=" + std::to_string(selectedRows.size()));
        const bool hadSelection = !selectedRows_.empty();
        const bool hasSelection = !selectedRows.empty();
        std::set<int> changed;
        std::set_symmetric_difference(selectedRows_.begin(), selectedRows_.end(),
            selectedRows.begin(), selectedRows.end(),
            std::inserter(changed, changed.end()));
        // Selection changes the visual role of every mark: selected cases use
        // the saturated palette tone, while all other cases become muted.  On
        // the first selection (and when clearing the last one), updating only
        // the symmetric difference leaves all other point brushes stale.
        if (hadSelection != hasSelection)
        {
            if (currentModel_ && currentModel_->kind == "boxplot")
            {
                for (const auto& point : boxplotGeometry_)
                    if (point.caseId > 0) changed.insert(point.caseId);
            }
            else if (currentModel_ && currentModel_->kind == "scatter_matrix")
            {
                for (auto const& point : scatterMatrixGeometry_)
                    if (point.caseId > 0) changed.insert(point.caseId);
            }
            else if (currentModel_ && currentModel_->kind == "trellis_scatterplot" &&
                     currentModel_->trellisSpecificationInitialized &&
                     (currentModel_->trellisSpecification.plotType ==
                          ::rlispstat::core::TrellisPlotType::Scatter ||
                      currentModel_->trellisSpecification.plotType ==
                          ::rlispstat::core::TrellisPlotType::TimeSeries ||
                      currentModel_->trellisSpecification.plotType ==
                          ::rlispstat::core::TrellisPlotType::Boxplot))
            {
                for (auto const& [row, visuals] : trellisPointVisuals_)
                    if (!visuals.empty()) changed.insert(row);
            }
            else
            {
                for (auto const& point : currentPlot_.points)
                    if (point.row > 0) changed.insert(point.row);
            }
        }
        selectedRows_ = selectedRows;
        RefreshContextSelectionState();
        if (changed.empty()) return;
        if (currentModel_ && currentModel_->kind == "glm_interaction" &&
            !interactionSeriesVisuals_.empty())
        {
            if (!pointVisuals_.empty()) RefreshPointVisuals(changed);
            RefreshInteractionSeriesVisuals();
        }
        else if (SupportsIncrementalPointUpdates())
        {
            RefreshPointVisuals(changed);
            if (currentModel_ && std::any_of(currentModel_->overlays.begin(),
                currentModel_->overlays.end(), [](auto const& overlay) {
                    return overlay.visible && overlay.source == "selected";
                }))
                RefreshScatterOverlayVisuals();
        }
        else if (currentModel_ && currentModel_->kind == "barplot")
        {
            RefreshBarplotSelectionVisuals(
                ::rlispstat::core::PlotThemeStyleForName(themeName_), &changed);
        }
        else if (currentModel_ && currentModel_->kind == "boxplot" &&
                 boxplotSelectionCanvas_)
        {
            RefreshBoxplotSelectionVisuals(changed);
        }
        else if (currentModel_ && currentModel_->kind == "scatter_matrix" &&
                 !scatterMatrixPointVisuals_.empty())
        {
            RefreshScatterMatrixSelectionVisuals(changed);
        }
        else if (currentModel_ && currentModel_->kind == "trellis_scatterplot" &&
                 currentModel_->trellisSpecificationInitialized &&
                 (currentModel_->trellisSpecification.plotType ==
                      ::rlispstat::core::TrellisPlotType::Scatter ||
                  currentModel_->trellisSpecification.plotType ==
                      ::rlispstat::core::TrellisPlotType::TimeSeries) &&
                 !trellisPointVisuals_.empty() &&
                 std::none_of(currentModel_->overlays.begin(),
                     currentModel_->overlays.end(), [](auto const& overlay) {
                         return overlay.visible && overlay.source == "selected";
                     }))
        {
            RefreshTrellisScatterSelectionVisuals(changed);
        }
        else if (currentModel_ && currentModel_->kind == "time_series" &&
                 timeSeriesSelectionCanvas_)
        {
            RefreshTimeSeriesSelectionVisuals(changed);
        }
        else if (currentModel_ && currentModel_->kind == "trellis_scatterplot" &&
                 currentModel_->trellisSpecificationInitialized &&
                 currentModel_->trellisSpecification.plotType ==
                     ::rlispstat::core::TrellisPlotType::Boxplot &&
                 !trellisPointVisuals_.empty())
        {
            RefreshTrellisBoxplotSelectionVisuals(changed);
        }
        else RenderPlot(std::max(360.0, plotCanvas_.ActualWidth()),
                        std::max(280.0, plotCanvas_.ActualHeight()));
    }

    void ScatterPlotView::Reset()
    {
        plotId_.clear();
        group_.clear();
        renderedPoints_.clear();
        scatterGeometry_.clear();
        pointVisuals_.clear();
        pointVisualStartIndex_ = 0;
        scatterMatrixPointVisuals_.clear();
        scatterMatrixRingPaths_.clear();
        currentPlot_ = {};
        currentModel_ = nullptr;
        dataFrame_ = {};
        hasDataFrame_ = false;
        boxplotGeometry_.clear();
        histogramBinRows_.clear();
        barplotBins_.clear();
        barplotSelectionCanvas_ = nullptr;
        barplotSelectionLayers_.clear();
        barplotSelectionLayersByRow_.clear();
        selectedRows_.clear();
        pointColors_.clear();
        rowLabels_.clear();
        labelColumn_.clear();
        renderedLabelDisplayMode_ = "none";
        labelRefreshQueued_ = false;
        note_ = {};
        pointSizeScale_ = 1.0;
        pointSizeControlVisible_ = false;
        pointSizeControl_.Visibility(Visibility::Collapsed);
        stickyNotes_.clear();
        notesPanel_.Visibility(Visibility::Collapsed);
        annotationCanvas_.Children().Clear();
        pointsCanvas_.Children().Clear();
        selectionRectangle_.Visibility(Visibility::Collapsed);
        pointsCanvas_.Children().Append(selectionRectangle_);
        window_.Title(L"LinkEDA scatterplot");
        RenderPlot(std::max(360.0, plotCanvas_.ActualWidth()),
                   std::max(280.0, plotCanvas_.ActualHeight()));
    }

    void ScatterPlotView::RenderPlot(double width, double height)
    {
        if (!plotCanvas_ || !pointsCanvas_) return;
        ::rlispstat::windows::performance::Scope timing(
            "Plot.RenderPlot",
            "plot=" + plotId_ + ",kind=" +
                (currentModel_ ? currentModel_->kind : std::string("scatter")) +
                ",points=" + std::to_string(currentPlot_.points.size()) +
                ",width=" + std::to_string(static_cast<int>(width)) +
                ",height=" + std::to_string(static_cast<int>(height)));
        width = std::max(360.0, width);
        height = std::max(280.0, height);
        RenderStickyNotes();
        const auto theme = ::rlispstat::core::PlotThemeStyleForName(themeName_);
        const double left = 74.0;
        const double top = 70.0;
        const double right = 38.0;
        std::size_t boxplotGroupingDepth = 0;
        if (currentModel_ && (currentModel_->kind == "boxplot" ||
            (currentModel_->kind == "trellis_scatterplot" &&
             currentModel_->trellisSpecificationInitialized &&
             currentModel_->trellisSpecification.plotType ==
                 ::rlispstat::core::TrellisPlotType::Boxplot)))
            for (auto const& levels : currentModel_->boxplotCategoryLevels)
                boxplotGroupingDepth = std::max(boxplotGroupingDepth, levels.size());
        const double bottom = std::max(68.0,
            50.0 + 18.0 * static_cast<double>(boxplotGroupingDepth));
        const ::rlispstat::core::Rect plotRect = currentModel_ &&
            currentModel_->kind == "glm_interaction"
            ? ::rlispstat::core::RegressionEffectPlotRect(*currentModel_, width, height)
            : ::rlispstat::core::Rect{
                left, top, std::max(180.0, width - left - right),
                std::max(120.0, height - top - bottom) };
        const auto effectTitleLines = currentModel_ &&
            currentModel_->kind == "glm_interaction"
            ? ::rlispstat::core::RegressionEffectTitleLines(*currentModel_, width)
            : std::vector<std::string>{};
        const double effectTitleExtra = 18.0 *
            (std::max<std::size_t>(1, effectTitleLines.size()) - 1);
        const auto appendImputationStatus = [&]()
        {
            if(!currentModel_) return;
            if (!currentModel_->subtitle.empty()) {
                auto label = Label(to_hstring(currentModel_->subtitle).c_str(),
                    plotRect.x, 34.0 + effectTitleExtra, 10.0, Brush(theme.mutedText));
                label.Width(plotRect.width);
                label.TextAlignment(TextAlignment::Center);
                plotCanvas_.Children().Append(label);
                return;
            }
            const auto diagnosticStatus=::rlispstat::core::ImputationDiagnosticDisplayStatus(*currentModel_);
            if(!diagnosticStatus.empty()) {
                auto label=Label(to_hstring(diagnosticStatus).c_str(),left,45.0,10.0,Brush(theme.mutedText));
                label.Width(std::max(180.0,width-left-right));plotCanvas_.Children().Append(label);return;
            }
            if (!hasDataFrame_) return;
            const bool summarizesAll = currentModel_->kind == "scatter" &&
                !currentModel_->isGLMDiagnostic;
            std::string status =
                ::rlispstat::core::IsPooledRegressionEffectPlot(*currentModel_)
                ? ::rlispstat::core::PooledEffectPlotImputationStatus(dataFrame_)
                : ::rlispstat::core::PlotImputationDisplayStatus(
                    dataFrame_, summarizesAll);
            if (currentModel_->isGLMDiagnostic && currentModel_->diagnosticImputationCount > 0 &&
                !currentModel_->diagnosticShowImputationUncertainty) {
                status = "Showing imputation " + std::to_string(currentModel_->diagnosticImputationIndex) +
                    " of " + std::to_string(currentModel_->diagnosticImputationCount) + " (not pooled).";
            }
            if (currentModel_->kind == "time_series" && dataFrame_.rows > 0 &&
                currentModel_->points.size() !=
                    static_cast<std::size_t>(dataFrame_.rows))
            {
                if (!status.empty()) status += " ";
                status += std::to_string(currentModel_->points.size()) +
                    " plotted cases of " + std::to_string(dataFrame_.rows) +
                    " dataset rows.";
            }
            if (!currentModel_->diagnosticSummary.empty()) {
                auto note=Label(to_hstring(currentModel_->diagnosticSummary).c_str(),left,height-22,9.5,Brush(theme.mutedText));
                note.Width(std::max(180.0,width-left-right));note.TextWrapping(TextWrapping::Wrap);
                plotCanvas_.Children().Append(note);
            }
            if (status.empty()) return;
            auto label = Label(to_hstring(status).c_str(), left, 45.0, 10.0,
                Brush(theme.mutedText));
            label.Width(std::max(180.0, width - left - right));
            label.TextAlignment(TextAlignment::Center);
            label.TextTrimming(TextTrimming::CharacterEllipsis);
            label.IsHitTestVisible(false);
            Controls::Canvas::SetZIndex(label, 30);
            plotCanvas_.Children().Append(label);
        };

        if (currentModel_ && currentModel_->kind == "boxplot")
        {
            RenderBoxplot(plotRect, theme);
            appendImputationStatus();
            RenderColorLegend(width, height, theme);
            return;
        }
        if (currentModel_ && currentModel_->kind == "histogram")
        {
            RenderHistogram(plotRect, theme);
            appendImputationStatus();
            RenderColorLegend(width, height, theme);
            return;
        }
        if (currentModel_ && currentModel_->kind == "barplot")
        {
            RenderBarplot(width, height, theme);
            appendImputationStatus();
            RenderColorLegend(width, height, theme);
            return;
        }
        if (currentModel_ && currentModel_->kind == "time_series")
        {
            RenderTimeSeries(plotRect, theme);
            appendImputationStatus();
            RenderColorLegend(width, height, theme);
            return;
        }
        if (currentModel_ && currentModel_->kind == "trellis_scatterplot")
        {
            RenderTrellis(width, height, theme);
            appendImputationStatus();
            RenderColorLegend(width, height, theme);
            return;
        }
        if (currentModel_ && currentModel_->kind == "scatter_matrix")
        {
            RenderScatterMatrix(width, height, theme);
            appendImputationStatus();
            RenderColorLegend(width, height, theme);
            return;
        }

        plotCanvas_.Children().Clear();
        plotCanvas_.Background(Brush(theme.background));
        pointsCanvas_.Background(Media::SolidColorBrush(
            winrt::Windows::UI::ColorHelper::FromArgb(1, 0, 0, 0)));
        auto axisBrush = Brush(theme.axis);
        auto gridBrush = Brush(theme.majorGrid);
        auto textBrush = Brush(theme.text);
        auto mutedTextBrush = Brush(theme.mutedText);
        auto panel = Shapes::Rectangle();
        panel.Width(plotRect.width);
        panel.Height(plotRect.height);
        panel.Fill(Brush(theme.panel));
        Controls::Canvas::SetLeft(panel, plotRect.x);
        Controls::Canvas::SetTop(panel, plotRect.y);
        plotCanvas_.Children().Append(panel);

        std::vector<::rlispstat::core::Point> dataPoints;
        std::vector<::rlispstat::core::ScatterplotPointValue> pointValues;
        dataPoints.reserve(currentPlot_.points.size());
        pointValues.reserve(currentPlot_.points.size());
        for (auto const& sample : currentPlot_.points)
        {
            dataPoints.push_back({ sample.x, sample.y });
            pointValues.push_back({ sample.row, sample.x, sample.y });
        }
        auto viewport = ::rlispstat::core::DataViewportForPoints(dataPoints);
        const ::rlispstat::core::DataColumn* imputationXColumn = nullptr;
        const ::rlispstat::core::DataColumn* imputationYColumn = nullptr;
        const bool useImputationGlyphs = currentModel_ &&
            currentModel_->kind == "scatter" && hasDataFrame_ &&
            ::rlispstat::core::DataFrameShowsAllImputations(dataFrame_);
        if (useImputationGlyphs)
        {
            imputationXColumn = ::rlispstat::core::FindDataColumnInDataFrame(
                dataFrame_, currentModel_->xLabel);
            imputationYColumn = ::rlispstat::core::FindDataColumnInDataFrame(
                dataFrame_, currentModel_->yLabel);
            if (imputationXColumn && imputationYColumn)
            {
                auto expanded = ::rlispstat::core::ScatterplotViewportIncludingImputations(
                    dataFrame_, *imputationXColumn, *imputationYColumn, pointValues,
                    currentModel_->scatterImputationUncertainty);
                if (expanded) viewport = *expanded;
            }
        }
        if (currentModel_ && currentModel_->diagnosticShowImputationUncertainty &&
            !currentModel_->diagnosticImputationValues.empty())
        {
            if (auto expanded =
                ::rlispstat::core::ScatterplotViewportIncludingPointImputations(
                    pointValues, currentModel_->diagnosticImputationValues,
                    currentModel_->scatterImputationUncertainty))
                viewport = *expanded;
        }
        if (currentModel_ && (currentModel_->kind == "pca_scree" || currentModel_->kind == "pca_biplot"))
        {
            viewport = { currentModel_->xmin, currentModel_->xmax,
                         currentModel_->ymin, currentModel_->ymax };
        }
        if (currentModel_ && currentModel_->isGLMDiagnostic &&
            currentModel_->glmDiagnosticKind == "roc_curve")
        {
            viewport = { 0.0, 1.0, 0.0, 1.0 };
        }
        if (currentModel_ && ViewportNavigationGestureEnabled() &&
            std::isfinite(currentModel_->xmin) && std::isfinite(currentModel_->xmax) &&
            std::isfinite(currentModel_->ymin) && std::isfinite(currentModel_->ymax) &&
            currentModel_->xmax > currentModel_->xmin &&
            currentModel_->ymax > currentModel_->ymin)
        {
            // Use the model viewport so Pan, Zoom and Reset zoom operate on
            // the same persistent bounds as the macOS plot view. Interaction
            // predictions may also extend beyond the observed points.
            viewport = { currentModel_->xmin, currentModel_->xmax,
                         currentModel_->ymin, currentModel_->ymax };
        }
        renderedViewport_ = viewport;
        renderedPlotRect_ = plotRect;
        for (int tick = 0; tick <= 5; ++tick)
        {
            const double fraction = static_cast<double>(tick) / 5.0;
            const double y = plotRect.y + fraction * plotRect.height;
            plotCanvas_.Children().Append(Line(plotRect.x, y, plotRect.x + plotRect.width, y, gridBrush, 1));
            const double yValue = viewport.ymax - fraction * (viewport.ymax - viewport.ymin);
            auto yTick = Label(to_hstring(currentModel_ && currentModel_->kind == "glm_interaction"
                ? ::rlispstat::core::RegressionEffectTickLabel(
                    yValue, viewport.ymin, viewport.ymax)
                : ::rlispstat::core::FormatDouble(yValue, 1)).c_str(),
                               plotRect.x - 38.0, y - 7.0, 10.5, mutedTextBrush);
            plotCanvas_.Children().Append(yTick);
        }
        if (theme.showMinorGrid)
            for (int tick = 0; tick < 5; ++tick)
            {
                const double y = plotRect.y + (tick + 0.5) * plotRect.height / 5.0;
                plotCanvas_.Children().Append(Line(plotRect.x, y,
                    plotRect.x + plotRect.width, y, Brush(theme.minorGrid), 0.5));
            }
        if (currentModel_ && currentModel_->kind == "pca_scree")
        {
            for (const auto &tick : ::rlispstat::core::BuildScreeAxisTicks(*currentModel_))
            {
                const double x = plotRect.x + (tick.value - viewport.xmin) /
                    std::max(1e-12, viewport.xmax - viewport.xmin) * plotRect.width;
                plotCanvas_.Children().Append(Line(x, plotRect.y, x,
                    plotRect.y + plotRect.height, gridBrush, 1));
                auto label = Label(to_hstring(tick.label).c_str(), x - 20.0,
                    plotRect.y + plotRect.height + 7.0, 10.5, mutedTextBrush);
                label.Width(40.0); label.TextAlignment(TextAlignment::Center);
                plotCanvas_.Children().Append(label);
            }
        }
        else if (currentModel_ && !currentModel_->interactionXTicks.empty())
        {
            for (std::size_t tickIndex = 0;
                 tickIndex < currentModel_->interactionXTicks.size(); ++tickIndex)
            {
                auto const& tick = currentModel_->interactionXTicks[tickIndex];
                const double fraction = (tick.first - viewport.xmin) /
                    std::max(1e-12, viewport.xmax - viewport.xmin);
                const double x = plotRect.x + fraction * plotRect.width;
                if (currentModel_->kind != "glm_interaction" ||
                    currentModel_->interactionXTicks.size() > 3)
                    plotCanvas_.Children().Append(Line(x, plotRect.y, x,
                        plotRect.y + plotRect.height, gridBrush, 1));
                auto xTick = Label(to_hstring(tick.second).c_str(), x - 52.0,
                    plotRect.y + plotRect.height + 7.0, 10.5, mutedTextBrush);
                xTick.Width(104.0); xTick.TextAlignment(TextAlignment::Center);
                const auto rows = ::rlispstat::core::PlotEffectCategoryRows(
                    *currentModel_, tickIndex);
                AttachBoxplotSelection(
                    xTick, std::vector<int>(rows.begin(), rows.end()));
                plotCanvas_.Children().Append(xTick);
            }
        }
        else
        {
            for (int tick = 0; tick <= 5; ++tick)
            {
                const double fraction = static_cast<double>(tick) / 5.0;
                const double x = plotRect.x + fraction * plotRect.width;
                const double xValue = viewport.xmin + fraction * (viewport.xmax - viewport.xmin);
                plotCanvas_.Children().Append(Line(x, plotRect.y, x,
                    plotRect.y + plotRect.height, gridBrush, 1));
                auto xTick = Label(to_hstring(::rlispstat::core::FormatDouble(xValue, 1)).c_str(),
                    x - 14.0, plotRect.y + plotRect.height + 7.0, 10.5,
                    mutedTextBrush);
                plotCanvas_.Children().Append(xTick);
            }
        }
        if (!theme.showPanelBorder)
        {
            plotCanvas_.Children().Append(Line(plotRect.x, plotRect.y + plotRect.height,
                plotRect.x + plotRect.width, plotRect.y + plotRect.height, axisBrush, 1.4));
            plotCanvas_.Children().Append(Line(plotRect.x, plotRect.y,
                plotRect.x, plotRect.y + plotRect.height, axisBrush, 1.4));
        }
        if (theme.showAxisTickMarks)
        {
            for (int tick = 0; tick <= 5; ++tick)
            {
                const double y = plotRect.y + plotRect.height * tick / 5.0;
                plotCanvas_.Children().Append(Line(plotRect.x - 4.0, y,
                    plotRect.x, y, axisBrush, 1.0));
            }
            const auto addXTick = [&](double x)
            {
                plotCanvas_.Children().Append(Line(x, plotRect.y + plotRect.height,
                    x, plotRect.y + plotRect.height + 4.0, axisBrush, 1.0));
            };
            if (currentModel_ && currentModel_->kind == "pca_scree" &&
                viewport.xmax > viewport.xmin)
            {
                for (const auto& tick : ::rlispstat::core::BuildScreeAxisTicks(*currentModel_))
                    addXTick(plotRect.x + (tick.value - viewport.xmin) /
                        (viewport.xmax - viewport.xmin) * plotRect.width);
            }
            else if (currentModel_ && !currentModel_->interactionXTicks.empty() &&
                viewport.xmax > viewport.xmin)
            {
                for (const auto& tick : currentModel_->interactionXTicks)
                    if (std::isfinite(tick.first))
                        addXTick(plotRect.x + (tick.first - viewport.xmin) /
                            (viewport.xmax - viewport.xmin) * plotRect.width);
            }
            else
            {
                for (int tick = 0; tick <= 5; ++tick)
                    addXTick(plotRect.x + plotRect.width * tick / 5.0);
            }
        }
        std::string displayPlotTitle = currentPlot_.title.empty()
            ? "Scatterplot" : currentPlot_.title;
        if (!effectTitleLines.empty()) {
            displayPlotTitle.clear();
            for (const auto& line : effectTitleLines) {
                if (!displayPlotTitle.empty()) displayPlotTitle += "\n";
                displayPlotTitle += line;
            }
        }
        plotTitle_ = Label(to_hstring(displayPlotTitle).c_str(),
                           plotRect.x, effectTitleLines.empty() ? 24.0 : 10.0, 16, textBrush);
        plotTitle_.Width(plotRect.width);
        if (!effectTitleLines.empty()) plotTitle_.Height(20.0 * effectTitleLines.size());
        plotTitle_.TextAlignment(TextAlignment::Center);
        plotTitle_.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        xAxisLabel_ = Label(to_hstring(currentPlot_.xLabel.empty() ? "x" : currentPlot_.xLabel).c_str(),
                            plotRect.x, plotRect.y + plotRect.height + 35.0, 12.5,
                            textBrush);
        xAxisLabel_.Width(plotRect.width);
        xAxisLabel_.TextAlignment(TextAlignment::Center);
        yAxisLabel_ = Label(to_hstring(currentPlot_.yLabel.empty() ? "y" : currentPlot_.yLabel).c_str(),
                            16.0, plotRect.y + plotRect.height / 2.0 + 20.0, 12.5,
                            textBrush);
        auto yRotation = Media::RotateTransform();
        yRotation.Angle(-90.0);
        yAxisLabel_.RenderTransform(yRotation);
        plotCanvas_.Children().Append(plotTitle_);
        plotCanvas_.Children().Append(xAxisLabel_);
        plotCanvas_.Children().Append(yAxisLabel_);
        AttachAxisVariableMenu(xAxisLabel_, true);
        AttachAxisVariableMenu(yAxisLabel_, false);

        pointsCanvas_.Width(width);
        pointsCanvas_.Height(height);
        pointsCanvas_.Children().Clear();
        selectionRectangle_.Visibility(Visibility::Collapsed);
        selectionRectangle_.Stroke(Brush(theme.accent));
        selectionRectangle_.Fill(Brush(theme.accent, 0.16));
        pointsCanvas_.Children().Append(selectionRectangle_);
        renderedPoints_.clear();
        scatterGeometry_.clear();
        pointVisuals_.clear();
        pointVisualStartIndex_ = pointsCanvas_.Children().Size();

        // Axes and legends stay on plotCanvas_; only the biplot data layer clips.
        if (currentModel_ && currentModel_->kind == "pca_biplot") {
            auto clip = Media::RectangleGeometry();
            clip.Rect({static_cast<float>(plotRect.x), static_cast<float>(plotRect.y),
                static_cast<float>(plotRect.width), static_cast<float>(plotRect.height)});
            pointsCanvas_.Clip(clip);
        } else pointsCanvas_.Clip(nullptr);

        if (currentModel_ && currentModel_->kind == "pca_scree")
        {
            std::vector<::rlispstat::core::DimensionalityScreePoint> observed;
            std::vector<::rlispstat::core::DimensionalityScreePoint> parallel;
            for (auto const& point : currentModel_->points)
                observed.push_back({ point.row, point.x, point.y, point.row });
            for (auto const& point : currentModel_->screeParallelPoints)
                parallel.push_back({ point.row, point.x, point.y, point.row });
            const auto plan = ::rlispstat::core::BuildDimensionalityScreeRenderPlan(
                observed, parallel, viewport, plotRect, 0);
            if (plan.parallelLine.size() >= 2)
                pointsCanvas_.Children().Append(Polyline(plan.parallelLine, Brush(theme.accent, 0.48), 1.4));
            if (plan.observedLine.size() >= 2)
                pointsCanvas_.Children().Append(Polyline(plan.observedLine, Brush(theme.markStroke), 1.8));
        }
        else if (currentModel_ && currentModel_->kind == "pca_biplot")
        {
            std::vector<::rlispstat::core::DimensionalityBiplotLoadingVector> loadings;
            for (auto const& loading : currentModel_->biplotLoadings)
                loadings.push_back({ loading.variable, loading.x, loading.y });
            const auto plan = ::rlispstat::core::BuildDimensionalityBiplotRenderPlan(
                loadings, viewport, plotRect, "");
            for (auto const& loading : plan.loadings)
            {
                const auto stroke = Brush(theme.accent, loading.focused ? 1.0 : 0.82);
                pointsCanvas_.Children().Append(Line(loading.start.x, loading.start.y,
                    loading.end.x, loading.end.y, stroke, loading.focused ? 2.2 : 1.6));
                pointsCanvas_.Children().Append(Line(loading.end.x, loading.end.y,
                    loading.arrowHeadA.x, loading.arrowHeadA.y, stroke, 1.4));
                pointsCanvas_.Children().Append(Line(loading.end.x, loading.end.y,
                    loading.arrowHeadB.x, loading.arrowHeadB.y, stroke, 1.4));
                auto label = Label(to_hstring(loading.variable).c_str(), loading.labelAnchor.x,
                    loading.labelAnchor.y - 8.0, 11.0, Brush(theme.accent));
                pointsCanvas_.Children().Append(label);
            }
        }
        else if (currentModel_ && currentModel_->isGLMDiagnostic &&
                 currentModel_->glmDiagnosticKind == "roc_curve")
        {
            std::vector<::rlispstat::core::Point> step;
            step.reserve(currentModel_->rocStepPoints.size());
            for (auto const& point : currentModel_->rocStepPoints)
                step.push_back(::rlispstat::core::DataToScreen(
                    { point.x, point.y }, viewport, plotRect, true));
            pointsCanvas_.Children().Append(Line(
                plotRect.x, plotRect.y + plotRect.height,
                plotRect.x + plotRect.width, plotRect.y,
                Brush(theme.mutedText, 0.48), 1.0));
            if (step.size() >= 2)
                pointsCanvas_.Children().Append(Polyline(step, Brush(theme.accent), 2.0));
        }

        const std::set<int> noSelection;
        const std::map<int, std::string> noColors;
        const bool screePlot = currentModel_ && currentModel_->kind == "pca_scree";
        ::rlispstat::core::ScatterplotRenderInput renderInput;
        renderInput.points = pointValues;
        renderInput.viewport = viewport;
        renderInput.plotRect = plotRect;
        renderInput.selectedRows = screePlot ? noSelection : selectedRows_;
        renderInput.rowColors = screePlot ? noColors : pointColors_;
        renderInput.rowLabels = screePlot ? std::map<int, std::string>{} : rowLabels_;
        renderInput.labelDisplayMode = currentModel_ ? currentModel_->labelDisplayMode : "none";
        renderInput.selectedColorName = "black";
        if (currentModel_ && currentModel_->diagnosticShowImputationUncertainty &&
            !currentModel_->diagnosticImputationValues.empty())
        {
            renderInput.pointImputationValues =
                &currentModel_->diagnosticImputationValues;
            renderInput.completeImputationPointValues =
                &currentModel_->diagnosticAllImputationValues;
            renderInput.directlyImputedModelRows =
                currentModel_->diagnosticRowsWithImputedModelInputs;
            renderInput.distinguishDiagnosticImputationRows = true;
        }
        if (useImputationGlyphs && imputationXColumn && imputationYColumn)
        {
            renderInput.imputationDataFrame = &dataFrame_;
            renderInput.xColumn = imputationXColumn;
            renderInput.yColumn = imputationYColumn;
            renderInput.imputationUncertaintyMode =
                currentModel_->scatterImputationUncertainty;
        }
        if (currentModel_)
        {
            // Diagnostic fitted lines are computed by R and arrive as
            // SmoothCurveData.  The overlay records retain enabled/menu state
            // but must not trigger the native least-squares renderer too.
            if (!currentModel_->isGLMDiagnostic)
                for (auto const& overlay : currentModel_->overlays)
                    renderInput.overlays.push_back({ overlay.type, overlay.source,
                                                     overlay.visible });
            renderInput.smoothCurves = currentModel_->smoothCurves;
            renderInput.shadeOverlap = currentModel_->scatterShadeOverlap;
            renderInput.sizeByOverlap = currentModel_->scatterSizeByOverlap;
            renderInput.sizeByVisualOverlap = currentModel_->kind == "pca_biplot";
            renderInput.showFitConfidenceIntervals =
                currentModel_->scatterFitConfidenceIntervalsVisible;
            renderInput.showSmoothConfidenceIntervals =
                currentModel_->scatterSmoothConfidenceIntervalsVisible;
        }
        auto renderPlan = ::rlispstat::core::BuildScatterplotRenderPlan(renderInput);
        for (auto const& curve : renderPlan.smoothCurves)
        {
            if (curve.confidencePolygon.size() < 3) continue;
            const auto curveColor = curve.colorName.empty()
                ? theme.smooth
                : ::rlispstat::core::PlotColorForNameOrHex(curve.colorName);
            pointsCanvas_.Children().Append(Polygon(
                curve.confidencePolygon, Brush(curveColor, curve.confidenceAlpha)));
        }
        scatterOverlayCanvas_ = Controls::Canvas();
        scatterOverlayCanvas_.Width(width);
        scatterOverlayCanvas_.Height(height);
        scatterOverlayCanvas_.IsHitTestVisible(false);
        pointsCanvas_.Children().Append(scatterOverlayCanvas_);
        for (auto const& overlay : renderPlan.overlayLines)
        {
            auto color = overlay.useDefaultDarkColor ? theme.geomStroke
                : ::rlispstat::core::PlotColorForNameOrHex(overlay.colorName);
            auto line = Line(overlay.start.x, overlay.start.y, overlay.end.x, overlay.end.y,
                             Brush(color, overlay.alpha), overlay.lineWidth);
            if (overlay.dashed)
            {
                auto dashes = Media::DoubleCollection(); dashes.Append(4.0); dashes.Append(3.0);
                line.StrokeDashArray(dashes);
            }
            scatterOverlayCanvas_.Children().Append(line);
        }
        if (currentModel_ && currentModel_->diagnosticShowIdentityLine)
        {
            const double lower = std::max(currentModel_->xmin, currentModel_->ymin);
            const double upper = std::min(currentModel_->xmax, currentModel_->ymax);
            if (std::isfinite(lower) && std::isfinite(upper) && lower < upper)
            {
                const auto first = ::rlispstat::core::DataToScreen(
                    { lower, lower }, viewport, plotRect, true);
                const auto last = ::rlispstat::core::DataToScreen(
                    { upper, upper }, viewport, plotRect, true);
                auto identity = Line(first.x, first.y, last.x, last.y,
                    Brush(theme.auxiliary, 0.80), 1.0);
                auto dashes = Media::DoubleCollection();
                dashes.Append(5.0); dashes.Append(4.0);
                identity.StrokeDashArray(dashes);
                pointsCanvas_.Children().Append(identity);
            }
        }
        for (auto const& curve : renderPlan.smoothCurves)
        {
            const auto curveColor = curve.colorName.empty()
                ? theme.smooth
                : ::rlispstat::core::PlotColorForNameOrHex(curve.colorName);
            auto line = Polyline(curve.points,
                Brush(curveColor, curve.alpha),
                curve.lineWidth);
            if (curve.dashed)
            {
                auto dashes = Media::DoubleCollection(); dashes.Append(4.0); dashes.Append(3.0);
                line.StrokeDashArray(dashes);
            }
            // Wide MI uncertainty glyphs otherwise cover the complete curve.
            // Keep ordinary scatterplot smooths behind their marks, but place
            // diagnostic smooth bundles above the glyph layer.
            if (currentModel_ && currentModel_->diagnosticShowImputationUncertainty)
                Controls::Canvas::SetZIndex(line, 40);
            pointsCanvas_.Children().Append(line);
        }
        const bool interactionPlot = currentModel_ &&
            currentModel_->kind == "glm_interaction" &&
            !currentModel_->interactionPlotLines.empty();
        interactionSeriesVisuals_.clear();
        if (interactionPlot)
        {
            interactionSeriesVisuals_.resize(
                currentModel_->interactionPlotLines.size());
            // Render the fitted series as first-class interaction geometry.
            // Previously Windows discarded these canonical lines and showed
            // only the underlying observations, which made a correct fitted
            // interaction look like a crude ungrouped scatterplot.
            std::vector<bool> selectedSeries(
                currentModel_->interactionPlotLines.size(), false);
            bool anySelectedSeries = false;
            const bool categoricalEffectAxis =
                ::rlispstat::core::RegressionEffectXAxisIsCategorical(*currentModel_);
            if (currentModel_->regressionBinaryProbability &&
                currentModel_->regressionEffectQuantity == "probability_difference")
            {
                auto left = ::rlispstat::core::DataToScreen(
                    { currentModel_->xmin, 0.0 }, viewport, plotRect, true);
                auto right = ::rlispstat::core::DataToScreen(
                    { currentModel_->xmax, 0.0 }, viewport, plotRect, true);
                auto reference = Line(left.x, left.y, right.x, right.y,
                    Brush({ 0.35, 0.37, 0.39, 1.0 }, 0.75), 1.0);
                auto dashes = Media::DoubleCollection();
                dashes.Append(5.0); dashes.Append(4.0);
                reference.StrokeDashArray(dashes);
                pointsCanvas_.Children().Append(reference);
            }
            for (std::size_t seriesIndex = 0;
                 seriesIndex < selectedSeries.size(); ++seriesIndex)
            {
                selectedSeries[seriesIndex] =
                    ::rlispstat::core::PlotSeriesIntersectsSelection(
                        *currentModel_, seriesIndex, selectedRows_);
                anySelectedSeries = anySelectedSeries || selectedSeries[seriesIndex];
            }
            for (std::size_t seriesIndex = 0;
                 seriesIndex < currentModel_->interactionPlotLines.size(); ++seriesIndex)
            {
                auto const& series = currentModel_->interactionPlotLines[seriesIndex];
                auto& visual = interactionSeriesVisuals_[seriesIndex];
                const bool active = selectedSeries[seriesIndex];
                const double alpha = anySelectedSeries && !active ? 0.22 : 0.96;
                const double seriesOffset = categoricalEffectAxis
                    ? ::rlispstat::core::RegressionEffectSeriesDodgeOffset(
                        seriesIndex, currentModel_->interactionPlotLines.size())
                    : 0.0;
                std::vector<::rlispstat::core::Point> screen;
                screen.reserve(series.points.size());
                for (auto const& point : series.points)
                {
                    auto value = ::rlispstat::core::DataToScreen(
                        { point.x, point.y }, viewport, plotRect, true);
                    value.x += seriesOffset;
                    screen.push_back(value);
                }
                const auto color = ::rlispstat::core::PlotColorForNameOrHex(
                    series.colorKey.empty() ? "black" : series.colorKey);
                if (currentModel_->regressionConfidenceIntervalsVisible &&
                    series.confidenceLower.size() == series.points.size() &&
                    series.confidenceUpper.size() == series.points.size() &&
                    !series.confidenceLower.empty())
                {
                    if (categoricalEffectAxis)
                    {
                        constexpr double cap = 4.0;
                        const auto intervalBrush = Brush(color, alpha);
                        for (std::size_t pointIndex = 0;
                             pointIndex < series.points.size(); ++pointIndex)
                        {
                            auto lower = ::rlispstat::core::DataToScreen(
                                { series.confidenceLower[pointIndex].x,
                                  series.confidenceLower[pointIndex].y },
                                viewport, plotRect, true);
                            auto upper = ::rlispstat::core::DataToScreen(
                                { series.confidenceUpper[pointIndex].x,
                                  series.confidenceUpper[pointIndex].y },
                                viewport, plotRect, true);
                            lower.x += seriesOffset;
                            upper.x += seriesOffset;
                            auto stem = Line(
                                lower.x, lower.y, upper.x, upper.y, intervalBrush, 1.2);
                            auto lowerCap = Line(
                                lower.x - cap, lower.y, lower.x + cap, lower.y,
                                intervalBrush, 1.2);
                            auto upperCap = Line(
                                upper.x - cap, upper.y, upper.x + cap, upper.y,
                                intervalBrush, 1.2);
                            for (const auto& interval : {stem, lowerCap, upperCap})
                            {
                                pointsCanvas_.Children().Append(interval);
                                visual.intervalLines.push_back(interval);
                            }
                        }
                    }
                    else
                    {
                        std::vector<::rlispstat::core::Point> band;
                        band.reserve(series.confidenceUpper.size() + series.confidenceLower.size());
                        for (auto const& point : series.confidenceUpper)
                            band.push_back(::rlispstat::core::DataToScreen(
                                { point.x, point.y }, viewport, plotRect, true));
                        for (auto point = series.confidenceLower.rbegin();
                             point != series.confidenceLower.rend(); ++point)
                            band.push_back(::rlispstat::core::DataToScreen(
                                { point->x, point->y }, viewport, plotRect, true));
                        auto confidenceBand = Polygon(
                            band, Brush(color, anySelectedSeries && !active ? 0.055 : 0.16));
                        pointsCanvas_.Children().Append(confidenceBand);
                        visual.confidenceBands.push_back(confidenceBand);
                    }
                }
                if (currentModel_->regressionConnectEstimates && screen.size() >= 2)
                {
                    visual.connectedLine = Polyline(
                        screen, Brush(color, alpha), active ? 3.8 : 2.2);
                    pointsCanvas_.Children().Append(visual.connectedLine);
                }
                for (auto const& point : screen)
                {
                    const double diameter = active ? 7.6 : 6.0;
                    auto marker = Shapes::Ellipse();
                    marker.Width(diameter); marker.Height(diameter);
                    marker.Fill(Brush(color, alpha)); marker.Stroke(Brush(color, alpha));
                    marker.StrokeThickness(0.8);
                    Controls::Canvas::SetLeft(marker, point.x - diameter / 2.0);
                    Controls::Canvas::SetTop(marker, point.y - diameter / 2.0);
                    pointsCanvas_.Children().Append(marker);
                    visual.markers.push_back(marker);
                    visual.markerCenters.push_back(point);
                }
            }
            if (currentModel_->regressionConfidenceIntervalsVisible &&
                currentModel_->regressionConfidenceLevelVisible &&
                ::rlispstat::core::PlotHasConfidenceIntervals(*currentModel_))
            {
                const int percent = static_cast<int>(std::lround(
                    currentModel_->regressionConfidenceLevel * 100.0));
                auto confidence = Label(to_hstring(std::to_string(percent) +
                    "% confidence interval").c_str(), plotRect.x + 6.0,
                    plotRect.y + 4.0, 9.0, mutedTextBrush);
                Controls::Canvas::SetZIndex(confidence, 45);
                pointsCanvas_.Children().Append(confidence);
            }
            const auto legendItems =
                ::rlispstat::core::BuildInteractionLegendLayout(
                    *currentModel_, plotRect);
            std::vector<UIElement> legendVisuals;
            if (!legendItems.empty())
            {
                double left = std::numeric_limits<double>::infinity();
                double top = std::numeric_limits<double>::infinity();
                double right = -std::numeric_limits<double>::infinity();
                double bottom = -std::numeric_limits<double>::infinity();
                for (auto const& item : legendItems)
                {
                    auto const& series = currentModel_->interactionPlotLines[item.seriesIndex];
                    const double labelWidth = 6.2 * static_cast<double>(series.label.size());
                    left = std::min(left, item.sampleStart.x - 6.0);
                    right = std::max(right, item.labelAnchor.x + labelWidth + 6.0);
                    top = std::min(top, item.sampleStart.y - 11.0);
                    bottom = std::max(bottom, item.sampleStart.y + 11.0);
                }
                if (currentModel_->interactionLegendTitleVisible &&
                    !currentModel_->interactionLegendTitle.empty()) {
                    right = std::max(right, left + 12.0 + 6.2 *
                        static_cast<double>(currentModel_->interactionLegendTitle.size()));
                    top -= 18.0;
                }
                auto background = Rectangle({ left, top, right - left, bottom - top },
                    Brush(theme.panel, 0.96), Brush(theme.majorGrid, 0.75), 0.6);
                Controls::Canvas::SetZIndex(background, 49);
                pointsCanvas_.Children().Append(background);
                legendVisuals.push_back(background);
                if (currentModel_->interactionLegendTitleVisible &&
                    !currentModel_->interactionLegendTitle.empty()) {
                    auto legendTitle = Label(
                        to_hstring(currentModel_->interactionLegendTitle).c_str(),
                        left + 6.0, top + 3.0, 10.0, textBrush);
                    legendTitle.FontWeight(Windows::UI::Text::FontWeights::Bold());
                    Controls::Canvas::SetZIndex(legendTitle, 50);
                    pointsCanvas_.Children().Append(legendTitle);
                    legendVisuals.push_back(legendTitle);
                }
            }
            for (auto const& item : legendItems)
            {
                if (item.seriesIndex >= currentModel_->interactionPlotLines.size()) continue;
                auto const& series = currentModel_->interactionPlotLines[item.seriesIndex];
                const bool active = selectedSeries[item.seriesIndex];
                const double alpha = anySelectedSeries && !active ? 0.22 : 1.0;
                const auto color = ::rlispstat::core::PlotColorForNameOrHex(
                    series.colorKey.empty() ? "black" : series.colorKey);
                const bool lineKey = currentModel_->regressionConnectEstimates &&
                    series.points.size() >= 2;
                auto legendSample = Line(item.sampleStart.x, item.sampleStart.y,
                    item.sampleEnd.x, item.sampleEnd.y, Brush(color, alpha),
                    active ? 3.8 : 2.2);
                if (!lineKey) legendSample.Opacity(0.0);
                Controls::Canvas::SetZIndex(legendSample, 50);
                pointsCanvas_.Children().Append(legendSample);
                legendVisuals.push_back(legendSample);
                interactionSeriesVisuals_[item.seriesIndex].legendSample = legendSample;
                {
                    const double diameter = active ? 7.6 : 6.0;
                    auto marker = Shapes::Ellipse();
                    marker.Width(diameter); marker.Height(diameter);
                    marker.Fill(Brush(color, alpha));
                    Controls::Canvas::SetLeft(marker,
                        (item.sampleStart.x + item.sampleEnd.x - diameter) / 2.0);
                    Controls::Canvas::SetTop(marker, item.sampleStart.y - diameter / 2.0);
                    Controls::Canvas::SetZIndex(marker, 50);
                    pointsCanvas_.Children().Append(marker);
                    legendVisuals.push_back(marker);
                    auto& visual = interactionSeriesVisuals_[item.seriesIndex];
                    visual.legendMarker = marker;
                    visual.legendMarkerCenter = {
                        (item.sampleStart.x + item.sampleEnd.x) / 2.0,
                        item.sampleStart.y};
                }
                auto legendLabel = Label(to_hstring(series.label).c_str(),
                    item.labelAnchor.x, item.labelAnchor.y - 8.0, 10.0,
                    textBrush);
                // The coloured sample already carries selection emphasis. Keep
                // category names at full text contrast in every state.
                legendLabel.Opacity(1.0);
                legendLabel.FontWeight(active
                    ? Windows::UI::Text::FontWeights::Bold()
                    : Windows::UI::Text::FontWeights::Medium());
                const auto rows = ::rlispstat::core::PlotSeriesLegendRows(
                    *currentModel_, item.seriesIndex);
                if (!rows.empty())
                {
                    legendSample.IsHitTestVisible(true);
                    legendSample.PointerPressed(
                        [this, legendSample, rows](auto const&, auto const& event)
                        {
                            if (event.GetCurrentPoint(legendSample).Properties()
                                    .IsRightButtonPressed()) return;
                            if (!RowSelectionGestureEnabled()) return;
                            if (selectionCallback_ && !group_.empty())
                                selectionCallback_(group_, rows,
                                    SelectionModeFor(event.KeyModifiers(),
                                        currentModel_ ? currentModel_->selectionMode
                                                      : "replace"));
                            event.Handled(true);
                        });
                }
                AttachBoxplotSelection(legendLabel,
                    std::vector<int>(rows.begin(), rows.end()));
                // AttachBoxplotSelection gives ordinary labels z-index 30.
                // Legend labels must stay above their opaque background (49).
                Controls::Canvas::SetZIndex(legendLabel, 50);
                pointsCanvas_.Children().Append(legendLabel);
                legendVisuals.push_back(legendLabel);
                interactionSeriesVisuals_[item.seriesIndex].legendLabel = legendLabel;
            }
            if (!legendItems.empty())
            {
                double legendLeft = std::numeric_limits<double>::infinity();
                double legendTop = std::numeric_limits<double>::infinity();
                double legendRight = -std::numeric_limits<double>::infinity();
                double legendBottom = -std::numeric_limits<double>::infinity();
                for (auto const& item : legendItems)
                {
                    auto const& series = currentModel_->interactionPlotLines[item.seriesIndex];
                    const double labelWidth = 6.2 * static_cast<double>(series.label.size());
                    legendLeft = std::min(legendLeft, item.sampleStart.x - 6.0);
                    legendRight = std::max(legendRight,
                        item.labelAnchor.x + labelWidth + 6.0);
                    legendTop = std::min(legendTop, item.sampleStart.y - 11.0);
                    legendBottom = std::max(legendBottom, item.sampleStart.y + 11.0);
                }
                const double legendWidth = std::max(1.0, legendRight - legendLeft);
                const double legendHeight = std::max(1.0, legendBottom - legendTop);
                auto dragSurface = Shapes::Rectangle();
                dragSurface.Width(legendWidth); dragSurface.Height(legendHeight);
                dragSurface.Fill(Brush(theme.panel, 0.001));
                Controls::Canvas::SetLeft(dragSurface, legendLeft);
                Controls::Canvas::SetTop(dragSurface, legendTop);
                Controls::Canvas::SetZIndex(dragSurface, 51);
                legendVisuals.push_back(dragSurface);
                dragSurface.PointerPressed(
                    [this, dragSurface, legendItems](auto const&, auto const& event)
                    {
                        if (!event.GetCurrentPoint(dragSurface).Properties()
                                .IsLeftButtonPressed()) return;
                        const auto point = event.GetCurrentPoint(plotCanvas_).Position();
                        interactionLegendDragging_ = true;
                        interactionLegendMoved_ = false;
                        interactionLegendAppliedDx_ = 0.0;
                        interactionLegendAppliedDy_ = 0.0;
                        interactionLegendDragStartX_ = point.X;
                        interactionLegendDragStartY_ = point.Y;
                        interactionLegendPressedRows_.clear();
                        interactionLegendSelectionMode_ = SelectionModeFor(
                            event.KeyModifiers(), currentModel_
                                ? currentModel_->selectionMode : "replace");
                        if (currentModel_)
                            for (auto const& item : legendItems)
                            {
                                if (item.seriesIndex >=
                                    currentModel_->interactionPlotLines.size()) continue;
                                auto const& series =
                                    currentModel_->interactionPlotLines[item.seriesIndex];
                                const double labelWidth = 6.2 *
                                    static_cast<double>(series.label.size());
                                const double left = item.sampleStart.x - 6.0;
                                const double right = item.labelAnchor.x + labelWidth + 6.0;
                                if (point.X >= left && point.X <= right &&
                                    point.Y >= item.sampleStart.y - 11.0 &&
                                    point.Y <= item.sampleStart.y + 11.0)
                                {
                                    interactionLegendPressedRows_ =
                                        ::rlispstat::core::PlotSeriesLegendRows(
                                            *currentModel_, item.seriesIndex);
                                    break;
                                }
                            }
                        dragSurface.CapturePointer(event.Pointer());
                        event.Handled(true);
                    });
                dragSurface.PointerMoved(
                    [this, legendVisuals, plotRect, legendLeft, legendTop,
                     legendWidth, legendHeight](auto const&, auto const& event)
                    {
                        if (!interactionLegendDragging_) return;
                        const auto point = event.GetCurrentPoint(plotCanvas_).Position();
                        const double dx = point.X - interactionLegendDragStartX_;
                        const double dy = point.Y - interactionLegendDragStartY_;
                        if (std::hypot(dx, dy) > 3.0) interactionLegendMoved_ = true;
                        if (!interactionLegendMoved_) return;
                        const double nextLeft = std::clamp(legendLeft + dx,
                            0.0, std::max(0.0, plotCanvas_.ActualWidth() - legendWidth));
                        const double nextTop = std::clamp(legendTop + dy,
                            0.0, std::max(0.0, plotCanvas_.ActualHeight() - legendHeight));
                        interactionLegendAppliedDx_ = nextLeft - legendLeft;
                        interactionLegendAppliedDy_ = nextTop - legendTop;
                        for (auto const& visual : legendVisuals)
                        {
                            auto transform = Media::TranslateTransform();
                            transform.X(interactionLegendAppliedDx_);
                            transform.Y(interactionLegendAppliedDy_);
                            visual.RenderTransform(transform);
                        }
                        event.Handled(true);
                    });
                dragSurface.PointerReleased(
                    [this, dragSurface, legendItems, plotRect]
                    (auto const&, auto const& event)
                    {
                        if (!interactionLegendDragging_) return;
                        interactionLegendDragging_ = false;
                        dragSurface.ReleasePointerCapture(event.Pointer());
                        if (interactionLegendMoved_ && currentModel_ &&
                            !legendItems.empty())
                        {
                            const auto anchor = legendItems.front().sampleStart;
                            auto manualLayout = *currentModel_;
                            manualLayout.interactionLegendUsesCustomPosition = true;
                            const auto manualRect =
                                ::rlispstat::core::RegressionEffectPlotRect(
                                    manualLayout, plotCanvas_.ActualWidth(),
                                    plotCanvas_.ActualHeight());
                            ::rlispstat::core::SetInteractionLegendCustomPosition(
                                *currentModel_,
                                (anchor.x + interactionLegendAppliedDx_ - manualRect.x) /
                                    std::max(1.0, manualRect.width),
                                (anchor.y + interactionLegendAppliedDy_ - manualRect.y) /
                                    std::max(1.0, manualRect.height));
                            ConfigureContextMenu();
                        }
                        else if (RowSelectionGestureEnabled() && selectionCallback_ &&
                                 !group_.empty() &&
                                 !interactionLegendPressedRows_.empty())
                        {
                            selectionCallback_(group_, interactionLegendPressedRows_,
                                               interactionLegendSelectionMode_);
                        }
                        interactionLegendPressedRows_.clear();
                        event.Handled(true);
                    });
                pointsCanvas_.Children().Append(dragSurface);
            }
        }
        auto drawPlan = std::move(renderPlan.points);
        if (interactionPlot)
        {
            // Observed cases provide context; the coloured fitted lines carry
            // the interpretation.  Keep selected/explicitly coloured cases
            // fully visible while muting ordinary background observations.
            for (auto& item : drawPlan)
            {
                if (item.selected || item.hasExplicitColor) continue;
                item.fillAlpha = std::min(item.fillAlpha, 0.34);
                item.strokeAlpha = std::min(item.strokeAlpha, 0.28);
                item.radius = std::min(item.radius, 2.7);
            }
        }
        std::stable_sort(drawPlan.begin(), drawPlan.end(), [](auto const& a, auto const& b) {
            return static_cast<int>(a.selected) < static_cast<int>(b.selected);
        });
        pointVisualStartIndex_ = pointsCanvas_.Children().Size();
        for (auto const& item : drawPlan)
        {
            auto visuals = CreatePointVisuals(item, theme);
            auto& stored = pointVisuals_[item.caseId];
            stored.insert(stored.end(), visuals.begin(), visuals.end());
            renderedPoints_.push_back({ item.caseId, item.point.x, item.point.y });
            scatterGeometry_.push_back({ item.caseId, item.point,
                item.hasImputationGlyph, item.imputationGlyph.rect });
#if 0
            double radius = item.radius * pointSizeScale_;
            if (theme.coordinatedMarks && item.selected) radius *= theme.selectedMarkScale;
            const auto explicitColor = item.selected
                ? ::rlispstat::core::PlotSelectedColorForNameOrHex(
                    item.colorName.empty() ? "black" : item.colorName)
                : ::rlispstat::core::PlotColorForNameOrHex(
                    item.colorName.empty() ? "black" : item.colorName);
            const auto fillColor = theme.coordinatedMarks && !item.hasExplicitColor
                ? (item.selected ? theme.selectedMarkFill
                    : (theme.hollowUnselectedMarks ? theme.panel : theme.markFill))
                : explicitColor;
            const auto strokeColor = theme.coordinatedMarks
                ? (item.selected ? theme.selectedMarkStroke
                    : (item.hasExplicitColor ? explicitColor : theme.markStroke))
                : explicitColor;

        if (item.hasHalo)
            {
                auto halo = Shapes::Ellipse();
                halo.Width(item.haloRadius * 2.0); halo.Height(item.haloRadius * 2.0);
                halo.Fill(Brush(theme.coordinatedMarks && !item.hasExplicitColor
                    ? theme.selectedMarkFill : explicitColor, item.haloAlpha));
                Controls::Canvas::SetLeft(halo, item.point.x - item.haloRadius);
                Controls::Canvas::SetTop(halo, item.point.y - item.haloRadius);
                pointsCanvas_.Children().Append(halo);
            }

            if (!item.selected && theme.crossUnselectedMarks)
            {
                const double arm = radius * 0.62;
                pointsCanvas_.Children().Append(Line(item.point.x - arm, item.point.y,
                    item.point.x + arm, item.point.y, Brush(::rlispstat::core::PlotMarkColor(strokeColor, theme.panel, item.shadeOverlap, item.strokeAlpha)),
                    item.strokeWidth));
                pointsCanvas_.Children().Append(Line(item.point.x, item.point.y - arm,
                    item.point.x, item.point.y + arm, Brush(::rlispstat::core::PlotMarkColor(strokeColor, theme.panel, item.shadeOverlap, item.strokeAlpha)),
                    item.strokeWidth));
            }
            else
            {
                Shapes::Shape mark = item.selected && theme.squareSelectedMarks
                    ? Shapes::Rectangle().as<Shapes::Shape>()
                    : Shapes::Ellipse().as<Shapes::Shape>();
                mark.Width(radius * 2.0); mark.Height(radius * 2.0);
                mark.Fill(Brush(::rlispstat::core::PlotMarkColor(fillColor, theme.panel, item.shadeOverlap, item.fillAlpha)));
                mark.Stroke(Brush(::rlispstat::core::PlotMarkColor(strokeColor, theme.panel, item.shadeOverlap, item.strokeAlpha)));
                mark.StrokeThickness(theme.coordinatedMarks && item.selected
                    ? theme.selectedMarkStrokeWidth : item.strokeWidth);
                Controls::Canvas::SetLeft(mark, item.point.x - radius);
                Controls::Canvas::SetTop(mark, item.point.y - radius);
                pointsCanvas_.Children().Append(mark);
            }
            const bool labelInsidePanel = !currentModel_ || currentModel_->kind != "pca_biplot" ||
                (item.point.x >= plotRect.x && item.point.x <= plotRect.x + plotRect.width &&
                 item.point.y >= plotRect.y && item.point.y <= plotRect.y + plotRect.height);
            if (item.showLabel && labelInsidePanel)
            {
                auto label = Label(to_hstring(item.label).c_str(),
                    item.labelAnchor.x + 6.0, item.labelAnchor.y - 18.0,
                    10.5, Brush(theme.text));
                label.IsHitTestVisible(false);
                pointsCanvas_.Children().Append(label);
            }
            renderedPoints_.push_back({ item.caseId, item.point.x, item.point.y });
#endif
        }
        if (currentPlot_.points.empty())
        {
            emptyLabel_ = Label(L"No plottable observations", plotRect.x + 20,
                                plotRect.y + plotRect.height / 2.0, 13, mutedTextBrush);
            plotCanvas_.Children().Append(emptyLabel_);
        }
        plotCanvas_.Children().Append(pointsCanvas_);
        // The interaction canvas intentionally covers the complete client
        // area for brushing. Keep the variable labels above it so a direct
        // click reaches their flyouts instead of starting a selection.
        Controls::Canvas::SetZIndex(xAxisLabel_, 20);
        Controls::Canvas::SetZIndex(yAxisLabel_, 20);
        Controls::Canvas::SetZIndex(pointsCanvas_, 10);
        AppendPanelFrame(plotCanvas_, plotRect, theme);
        appendImputationStatus();
        RenderColorLegend(width, height, theme);
    }

    void ScatterPlotView::RenderColorLegend(
        double width, double height,
        ::rlispstat::core::PlotThemeStyleSpec const& theme)
    {
        if (!currentModel_ || currentModel_->colorByVariable.empty() ||
            currentModel_->colorByLegendItems.empty() ||
            !currentModel_->colorByLegendVisible) return;

        // A categorical legend is misleading as soon as a linked/manual row
        // colour overrides that category's colour. Keep the user's visibility
        // preference but suppress the stale legend until the mapping matches.
        if (!::rlispstat::core::PlotColorLegendMatchesLinkedRowColors(
                *currentModel_, pointColors_)) return;

        std::size_t longest = currentModel_->colorByVariable.size();
        for (auto const& item : currentModel_->colorByLegendItems)
            longest = std::max(longest, item.first.size());
        const double legendWidth = std::max(108.0,
            std::min(220.0, 48.0 + 7.0 * static_cast<double>(longest)));
        const double legendHeight = 24.0 + 22.0 *
            static_cast<double>(currentModel_->colorByLegendItems.size());
        const double left = std::clamp(currentModel_->colorByLegendX * width,
            4.0, std::max(4.0, width - legendWidth - 4.0));
        const double top = std::clamp(currentModel_->colorByLegendY * height,
            4.0, std::max(4.0, height - legendHeight - 4.0));

        auto border = Controls::Border();
        border.Width(legendWidth); border.Height(legendHeight);
        // Match the compact rounded legend used by effect plots while keeping
        // the title as a drag handle and every category row selectable.
        border.Background(Brush(theme.panel, 0.96));
        border.BorderBrush(Brush(theme.majorGrid, 0.75));
        border.BorderThickness(Thickness{0.6, 0.6, 0.6, 0.6});
        border.CornerRadius(CornerRadius{4.0});
        auto content = Controls::StackPanel();
        auto header = Controls::Grid();
        header.Height(24.0); header.Padding(Thickness{6, 3, 5, 1});
        auto title = Controls::TextBlock();
        title.Text(to_hstring(currentModel_->colorByVariable));
        title.FontSize(10.0); title.FontWeight(Windows::UI::Text::FontWeights::Bold());
        title.Foreground(Brush(theme.text));
        header.Children().Append(title);
        content.Children().Append(header);
        for (auto const& [level, colorName] : currentModel_->colorByLegendItems)
        {
            auto row = Controls::StackPanel();
            row.Orientation(Controls::Orientation::Horizontal);
            row.Height(22.0); row.Padding(Thickness{13, 4, 4, 2});
            // Give the full row a hit target, including the space after a
            // short category label, without changing its visual appearance.
            row.Background(Brush(theme.panel, 0.01));
            auto swatch = Shapes::Ellipse(); swatch.Width(6); swatch.Height(6);
            swatch.Fill(Brush(::rlispstat::core::PlotColorForNameOrHex(colorName)));
            swatch.Margin(Thickness{0, 2, 8, 0});
            auto label = Controls::TextBlock(); label.Text(to_hstring(level));
            label.FontSize(10.0); label.FontWeight(Windows::UI::Text::FontWeights::Medium());
            label.Foreground(Brush(theme.text));
            row.Children().Append(swatch); row.Children().Append(label);
            row.PointerPressed([this, row, level](auto const&, auto const& event)
            {
                if (!event.GetCurrentPoint(row).Properties().IsLeftButtonPressed()) return;
                if (!RowSelectionGestureEnabled()) return;
                const std::set<int> matchingRows = currentModel_
                    ? ::rlispstat::core::PlotColorLegendRows(*currentModel_, level)
                    : std::set<int>{};
                if (selectionCallback_ && !group_.empty() && !matchingRows.empty())
                    selectionCallback_(group_, matchingRows,
                        SelectionModeFor(event.KeyModifiers(),
                            currentModel_ ? currentModel_->selectionMode : "replace"));
                event.Handled(true);
            });
            content.Children().Append(row);
        }
        border.Child(content);
        Controls::Canvas::SetLeft(border, left);
        Controls::Canvas::SetTop(border, top);
        Controls::Canvas::SetZIndex(border, 50);

        auto legendMenu = Controls::MenuFlyout();
        auto hide = Controls::MenuFlyoutItem(); hide.Text(L"Hide legend");
        hide.Click([this](auto const&, auto const&)
        { DispatchPlotCommand({"PLOT_COLOR_LEGEND_VISIBLE", plotId_, "0"}); });
        legendMenu.Items().Append(hide);
        legendMenu.Items().Append(Controls::MenuFlyoutSeparator());
        for (auto const& position : std::vector<std::pair<std::string, std::wstring>>{
                 {"top_left", L"Top left"}, {"top_right", L"Top right"},
                 {"bottom_left", L"Bottom left"}, {"bottom_right", L"Bottom right"}})
        {
            auto item = Controls::MenuFlyoutItem(); item.Text(position.second);
            item.Click([this, value = position.first](auto const&, auto const&)
            { DispatchPlotCommand({"PLOT_COLOR_LEGEND_POSITION", plotId_, value}); });
            legendMenu.Items().Append(item);
        }
        header.PointerPressed([this, header, left, top](auto const&, auto const& event)
        {
            if (!event.GetCurrentPoint(header).Properties().IsLeftButtonPressed())
                return;
            const auto point = event.GetCurrentPoint(plotCanvas_).Position();
            colorLegendDragging_ = true;
            colorLegendMoved_ = false;
            colorLegendDragStartX_ = point.X; colorLegendDragStartY_ = point.Y;
            colorLegendOriginalX_ = left; colorLegendOriginalY_ = top;
            header.CapturePointer(event.Pointer()); event.Handled(true);
        });
        header.PointerMoved([this, border, width, height, legendWidth, legendHeight]
            (auto const&, auto const& event)
        {
            if (!colorLegendDragging_ || !currentModel_) return;
            const auto point = event.GetCurrentPoint(plotCanvas_).Position();
            if (std::hypot(point.X - colorLegendDragStartX_,
                           point.Y - colorLegendDragStartY_) > 3.0)
                colorLegendMoved_ = true;
            if (!colorLegendMoved_) return;
            const double nextLeft = std::clamp(colorLegendOriginalX_ + point.X - colorLegendDragStartX_,
                4.0, std::max(4.0, width - legendWidth - 4.0));
            const double nextTop = std::clamp(colorLegendOriginalY_ + point.Y - colorLegendDragStartY_,
                4.0, std::max(4.0, height - legendHeight - 4.0));
            currentModel_->colorByLegendX = nextLeft / width;
            currentModel_->colorByLegendY = nextTop / height;
            Controls::Canvas::SetLeft(border, nextLeft);
            Controls::Canvas::SetTop(border, nextTop);
            event.Handled(true);
        });
        header.PointerReleased([this](auto const&, auto const& event)
        {
            if (!colorLegendDragging_) return;
            colorLegendDragging_ = false;
            event.Handled(true);
        });
        // The legend owns its complete hit area.  In particular, right-click
        // must not bubble through to the plot's general context menu.
        border.ContextFlyout(legendMenu);
        plotCanvas_.Children().Append(border);
    }

    void ScatterPlotView::RenderBoxplot(
        ::rlispstat::core::Rect const& plotRect,
        ::rlispstat::core::PlotThemeStyleSpec const& theme)
    {
        using namespace ::rlispstat::core;
        plotCanvas_.Children().Clear();
        pointsCanvas_.Children().Clear();
        renderedPoints_.clear();
        boxplotGeometry_.clear();
        boxplotPointVisuals_.clear();
        boxplotSelectionCanvas_ = Controls::Canvas();
        boxplotSelectionCanvas_.Width(std::max(360.0, plotCanvas_.ActualWidth()));
        boxplotSelectionCanvas_.Height(std::max(280.0, plotCanvas_.ActualHeight()));
        boxplotSelectionCanvas_.IsHitTestVisible(false);
        Controls::Canvas::SetZIndex(boxplotSelectionCanvas_, 2);
        plotCanvas_.Background(Brush(theme.background));
        plotCanvas_.Children().Append(Rectangle(plotRect, Brush(theme.panel),
            TransparentBrush(), 0.0));

        const auto cases = BoxplotCasesForModel(*currentModel_);
        const auto displayRange = BoxplotDisplayRangeForCases(
            cases, currentModel_->boxplotShowH0Simulation,
            currentModel_->boxplotH0Values, currentModel_->boxplotH0ReferenceLines,
            currentModel_->boxplotH0Lower, currentModel_->boxplotH0Upper);
        BoxplotLayout layout;
        layout.plotRect = plotRect;
        layout.categories = OrderedBoxplotCategories(
            currentModel_->boxplotDefinedCategories, cases, currentModel_->boxplotGroupOrder);
        boxplotCategoryOrder_ = layout.categories;
        layout.yMinimum = displayRange.minimum;
        layout.yMaximum = displayRange.maximum;
        layout.variableAxes = BoxplotUsesVariableAxes(*currentModel_);
        layout.connectRows = currentModel_->boxplotConnectRows;
        layout.categoryLevels = BoxplotCategoryLevelsForCategories(
            *currentModel_, layout.categories);

        auto textBrush = Brush(theme.text);
        auto mutedBrush = Brush(theme.mutedText);
        for (auto const& tick : BoxplotYAxisTicks(layout.yMinimum, layout.yMaximum, 5))
        {
            const double fraction = (tick.value - layout.yMinimum) /
                std::max(1e-12, layout.yMaximum - layout.yMinimum);
            const double y = plotRect.y + plotRect.height - fraction * plotRect.height;
            plotCanvas_.Children().Append(Line(plotRect.x, y,
                plotRect.x + plotRect.width, y, Brush(theme.majorGrid), 1.0));
            auto tickLabel = Label(to_hstring(tick.label).c_str(),
                plotRect.x - 46.0, y - 7.0, 10.5, mutedBrush);
            tickLabel.Width(38.0);
            tickLabel.TextAlignment(TextAlignment::Right);
            plotCanvas_.Children().Append(tickLabel);
        }
        if (!theme.showPanelBorder)
        {
            plotCanvas_.Children().Append(Line(plotRect.x, plotRect.y,
                plotRect.x, plotRect.y + plotRect.height, Brush(theme.axis), 1.4));
            plotCanvas_.Children().Append(Line(plotRect.x, plotRect.y + plotRect.height,
                plotRect.x + plotRect.width, plotRect.y + plotRect.height, Brush(theme.axis), 1.4));
        }

        auto title = Label(to_hstring(currentModel_->title.empty() ? "Boxplot" : currentModel_->title).c_str(),
            plotRect.x, 24.0, 16.0, textBrush);
        title.Width(plotRect.width); title.TextAlignment(TextAlignment::Center);
        title.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        plotCanvas_.Children().Append(title);
        yAxisLabel_ = Label(to_hstring(currentModel_->yLabel).c_str(), 16.0,
            plotRect.y + plotRect.height / 2.0 + 20.0, 12.5, textBrush);
        auto rotation = Media::RotateTransform(); rotation.Angle(-90.0);
        yAxisLabel_.RenderTransform(rotation); plotCanvas_.Children().Append(yAxisLabel_);
        AttachAxisVariableMenu(yAxisLabel_, false);

        auto screenY = [&](double value)
        {
            const double fraction = (value - layout.yMinimum) /
                std::max(1e-12, layout.yMaximum - layout.yMinimum);
            return plotRect.y + plotRect.height - fraction * plotRect.height;
        };
        auto densityPolygon = [&](std::vector<double> const& values, double centerX,
                                  double maximumHalfWidth, double lower, double upper,
                                  Media::Brush const& fill, Media::Brush const& stroke)
        {
            if (values.size() < 2) return;
            double minimum = *std::min_element(values.begin(), values.end());
            double maximum = *std::max_element(values.begin(), values.end());
            if (!(maximum > minimum)) { minimum -= 0.5; maximum += 0.5; }
            auto curve = DensityCurveForValues(values, "boxplot", "", "",
                minimum, maximum, 0.0, 1.0);
            if (curve.x.size() < 2 || curve.y.size() != curve.x.size()) return;
            double maxDensity = 0.0;
            for (double value : curve.y) if (std::isfinite(value)) maxDensity = std::max(maxDensity, value);
            if (!(maxDensity > 0.0)) return;
            std::vector<std::size_t> indices;
            for (std::size_t index = 0; index < curve.x.size(); ++index)
                if (curve.x[index] >= lower && curve.x[index] <= upper &&
                    std::isfinite(curve.y[index])) indices.push_back(index);
            if (indices.size() < 2) return;
            std::vector<Point> polygon; polygon.reserve(indices.size() * 2);
            for (auto index : indices)
                polygon.push_back({ centerX - maximumHalfWidth * curve.y[index] / maxDensity,
                                    screenY(curve.x[index]) });
            for (auto it = indices.rbegin(); it != indices.rend(); ++it)
                polygon.push_back({ centerX + maximumHalfWidth * curve.y[*it] / maxDensity,
                                    screenY(curve.x[*it]) });
            auto shape = Polygon(polygon, fill); shape.Stroke(stroke); shape.StrokeThickness(0.9);
            plotCanvas_.Children().Append(shape);
        };
        if (currentModel_->boxplotShowViolin || currentModel_->boxplotSplitViolin)
        {
            const double halfWidth = std::max(18.0, std::min(60.0,
                plotRect.width / std::max<std::size_t>(1, layout.categories.size()) * 0.34));
            for (auto const& category : layout.categories)
            {
                std::vector<double> values;
                for (auto const& point : cases)
                    if (point.category == category && std::isfinite(point.value)) values.push_back(point.value);
                if (values.size() < 2) continue;
                const double minimum = *std::min_element(values.begin(), values.end());
                const double maximum = *std::max_element(values.begin(), values.end());
                const double center = BoxplotCategoryCenter(layout, category);
                densityPolygon(values, center, halfWidth, minimum, maximum,
                    Brush(theme.geomFill, 0.36), Brush(theme.geomStroke, 0.42));
                if (currentModel_->boxplotSplitViolin)
                    for (auto const& interval : BoxplotTailIntervalsForAlternative(
                        minimum, maximum, currentModel_->boxplotSplitLower,
                        currentModel_->boxplotSplitUpper,
                        currentModel_->boxplotSplitAlternative))
                        densityPolygon(values, center, halfWidth, interval.lower, interval.upper,
                            Brush(theme.accent2, 0.62), Brush(theme.accent, 0.72));
            }
        }
        if (currentModel_->boxplotShowH0Simulation && currentModel_->boxplotH0Values.size() >= 2)
        {
            const double center = BoxplotH0SimulationCenterX(layout);
            const double halfWidth = std::max(20.0, std::min(58.0,
                plotRect.width / std::max<std::size_t>(2, layout.categories.size() + 1) * 0.32));
            densityPolygon(currentModel_->boxplotH0Values, center, halfWidth,
                layout.yMinimum, layout.yMaximum, Brush(theme.accent2, 0.24),
                Brush(theme.accent, 0.62));
            for (double value : currentModel_->boxplotH0ReferenceLines)
                if (std::isfinite(value)) plotCanvas_.Children().Append(Line(
                    center - halfWidth, screenY(value), center + halfWidth, screenY(value),
                    Brush(theme.accent), 1.4));
            auto label = Label(to_hstring(currentModel_->boxplotH0Label + "  p = " +
                FormatPValue(currentModel_->boxplotH0PValue)).c_str(), center - 72.0,
                plotRect.y + plotRect.height + 8.0, 10.0, mutedBrush);
            label.Width(144.0); label.TextAlignment(TextAlignment::Center);
            plotCanvas_.Children().Append(label);
        }

        for (auto const& item : BuildBoxplotStatsRenderPlan(
                 layout, BoxplotStatsForModel(*currentModel_)))
        {
            if (currentModel_->boxplotShowWhiskers)
            {
                plotCanvas_.Children().Append(Line(item.upperWhiskerStart.x, item.upperWhiskerStart.y,
                    item.upperWhiskerEnd.x, item.upperWhiskerEnd.y, Brush(theme.geomStroke), 1.3));
                plotCanvas_.Children().Append(Line(item.lowerWhiskerStart.x, item.lowerWhiskerStart.y,
                    item.lowerWhiskerEnd.x, item.lowerWhiskerEnd.y, Brush(theme.geomStroke), 1.3));
                plotCanvas_.Children().Append(Line(item.upperCapStart.x, item.upperCapStart.y,
                    item.upperCapEnd.x, item.upperCapEnd.y, Brush(theme.geomStroke), 1.3));
                plotCanvas_.Children().Append(Line(item.lowerCapStart.x, item.lowerCapStart.y,
                    item.lowerCapEnd.x, item.lowerCapEnd.y, Brush(theme.geomStroke), 1.3));
            }
            if (currentModel_->boxplotShowBox)
            {
                plotCanvas_.Children().Append(Rectangle(item.boxRect, Brush(theme.geomFill),
                    Brush(theme.geomStroke), 1.4));
                plotCanvas_.Children().Append(Line(item.medianStart.x, item.medianStart.y,
                    item.medianEnd.x, item.medianEnd.y, Brush(theme.accent), 2.0));
            }
            const std::string categoryText = BoxplotInnermostCategoryLabel(layout, item.category);
            auto category = Label(to_hstring(categoryText).c_str(), item.centerX - 55.0,
                plotRect.y + plotRect.height + 4.0, 10.5, mutedBrush);
            category.Width(110.0); category.TextAlignment(TextAlignment::Center);
            plotCanvas_.Children().Append(category);
            std::set<std::string> matchingCategories;
            if (!layout.categoryLevels.empty() &&
                !layout.categoryLevels.front().empty())
                matchingCategories = BoxplotCategoriesForLevelValue(layout,
                    layout.categoryLevels.front().size() - 1, categoryText);
            if (matchingCategories.empty()) matchingCategories.insert(item.category);
            std::vector<int> categoryRows;
            for (auto const& point : cases)
                if (matchingCategories.count(point.category))
                    categoryRows.push_back(point.caseId);
            if (layout.variableAxes)
            {
                AttachNumericVariableReplacementMenu(category, item.category,
                    item.category, currentModel_->boxplotVariables,
                    "BOXPLOT_REPLACE_VARIABLE");
            }
            else AttachBoxplotSelection(category, std::move(categoryRows));
        }

        std::size_t groupingDepth = 0;
        for (auto const& levels : layout.categoryLevels)
            groupingDepth = std::max(groupingDepth, levels.size());
        const auto spans = BoxplotGroupSpans(layout);
        if (groupingDepth > 1)
        {
            const double innerY = plotRect.y + plotRect.height + 28.0;
            for (auto const& category : layout.categories)
            {
                const double x = BoxplotCategoryCenter(layout, category);
                plotCanvas_.Children().Append(Line(x,
                    plotRect.y + plotRect.height + 20.0, x, innerY,
                    Brush(theme.axis, 0.68), 1.15));
            }
        }
        for (auto const& span : spans)
        {
            const double y = plotRect.y + plotRect.height + 28.0 +
                22.0 * static_cast<double>(groupingDepth - 2 - span.level);
            plotCanvas_.Children().Append(Line(span.startX, y, span.endX, y,
                Brush(theme.axis, 0.78), 1.3));
            if (span.level > 0)
                plotCanvas_.Children().Append(Line(span.centerX, y,
                    span.centerX, y + 22.0, Brush(theme.axis, 0.68), 1.15));
            auto label = Label(to_hstring(span.label).c_str(), span.centerX - 60.0,
                y + 1.0, 10.0, textBrush);
            label.Width(120.0); label.TextAlignment(TextAlignment::Center);
            plotCanvas_.Children().Append(label);
            const auto covered = BoxplotCategoriesForLevelValue(
                layout, span.level, span.label);
            std::vector<int> branchRows;
            for (auto const& point : cases)
                if (covered.count(point.category)) branchRows.push_back(point.caseId);
            AttachBoxplotSelection(label, std::move(branchRows));
        }

        std::vector<std::string> groupingVariables = currentModel_->boxplotGroupingVariables;
        if (groupingVariables.empty() && !currentModel_->xLabel.empty())
            groupingVariables.push_back(currentModel_->xLabel);
        if (groupingVariables.size() == groupingDepth && groupingDepth > 0)
        {
            for (std::size_t level = 0; level < groupingDepth; ++level)
            {
                const double y = level + 1 == groupingDepth
                    ? plotRect.y + plotRect.height + 4.0
                    : plotRect.y + plotRect.height + 29.0 +
                        22.0 * static_cast<double>(groupingDepth - 2 - level);
                auto variableLabel = Label(to_hstring(groupingVariables[level] + ":").c_str(),
                    plotRect.x - 72.0, y, 9.5, Brush(theme.accent));
                variableLabel.Width(68.0);
                variableLabel.TextAlignment(TextAlignment::Right);
                plotCanvas_.Children().Append(variableLabel);
                AttachBoxplotGroupingVariableMenu(
                    variableLabel, groupingVariables[level], false);
                if (level == 0) xAxisLabel_ = variableLabel;
            }
        }
        else if (!currentModel_->xLabel.empty())
        {
            const double y = plotRect.y + plotRect.height + 36.0;
            xAxisLabel_ = Label(to_hstring(currentModel_->xLabel).c_str(),
                plotRect.x, y, 11.0, textBrush);
            xAxisLabel_.Width(plotRect.width); xAxisLabel_.TextAlignment(TextAlignment::Center);
            plotCanvas_.Children().Append(xAxisLabel_);
            AttachBoxplotGroupingVariableMenu(xAxisLabel_, currentModel_->xLabel, false);
        }

        boxplotGeometry_ = BoxplotCaseGeometryForLayout(layout, cases);
        const auto connections = BuildBoxplotConnectionLinePlan(
            boxplotGeometry_, layout.categories, selectedRows_, layout.variableAxes,
            currentModel_->boxplotConnectRows);
        for (int selectedPass = 0; selectedPass < 2; ++selectedPass)
        {
            for (auto const& connection : connections)
            {
                if (static_cast<int>(connection.selected) != selectedPass) continue;
                auto found = pointColors_.find(connection.caseId);
                auto color = found != pointColors_.end()
                    ? (connection.selected
                        ? PlotSelectedColorForNameOrHex(found->second, 0.96)
                        : LightPaletteColor(found->second, 0.96))
                    : (connection.selected ? theme.selectedMarkFill : theme.markStroke);
                if (found == pointColors_.end() && !connection.selected)
                    color.a = connection.dimmed ? 0.18 : 0.42;
                boxplotSelectionCanvas_.Children().Append(Polyline(connection.points, Brush(color),
                    std::max(0.5, currentModel_->boxplotConnectionLineWidth)));
            }
        }
        plotCanvas_.Children().Append(boxplotSelectionCanvas_);
        if (currentModel_->boxplotShowPoints)
        {
            const auto points = BuildBoxplotPointDrawPlan(boxplotGeometry_, selectedRows_);
            for (int selectedPass = 0; selectedPass < 2; ++selectedPass)
            {
                for (auto const& point : points)
                {
                    if (static_cast<int>(point.selected) != selectedPass) continue;
                    auto found = pointColors_.find(point.caseId);
                    const bool explicitColor = found != pointColors_.end();
                    const auto fill = explicitColor
                        ? (point.selected
                            ? PlotSelectedColorForNameOrHex(found->second)
                            : PlotColorForNameOrHex(found->second))
                        : (point.selected ? theme.selectedMarkFill : theme.markFill);
                    const double fillAlpha = point.selected ? 1.0
                        : (point.dimmed ? 0.24 : 0.72);
                    const auto stroke = point.selected ? theme.selectedMarkStroke
                        : (explicitColor ? fill : theme.markStroke);
                    const double radius = 3.2 * pointSizeScale_;
                    if (point.selected)
                        for (auto const& ring : CreateSelectionRingVisuals(
                                 point.point, radius, theme, fill))
                            boxplotSelectionCanvas_.Children().Append(ring);
                    auto dot = Shapes::Ellipse(); dot.Width(radius * 2.0); dot.Height(radius * 2.0);
                    dot.Fill(Brush(fill, fillAlpha));
                    dot.Stroke(Brush(stroke, point.selected ? 1.0
                        : (point.dimmed ? 0.18 : 0.55)));
                    dot.StrokeThickness(point.selected ? theme.selectedMarkStrokeWidth : 0.8);
                    Controls::Canvas::SetLeft(dot, point.point.x - radius);
                    Controls::Canvas::SetTop(dot, point.point.y - radius);
                    Controls::Canvas::SetZIndex(dot, point.selected ? 4 : 3);
                    plotCanvas_.Children().Append(dot);
                    boxplotPointVisuals_[point.caseId].push_back(dot);
                    renderedPoints_.push_back({ point.caseId, point.point.x, point.point.y });
                }
            }
        }
        pointsCanvas_.Width(plotCanvas_.ActualWidth());
        pointsCanvas_.Height(plotCanvas_.ActualHeight());
        selectionRectangle_.Visibility(Visibility::Collapsed);
        selectionRectangle_.Stroke(Brush(theme.accent));
        selectionRectangle_.Fill(Brush(theme.accent, 0.16));
        pointsCanvas_.Children().Append(selectionRectangle_);
        plotCanvas_.Children().Append(pointsCanvas_);
        Controls::Canvas::SetZIndex(pointsCanvas_, 10);
        AppendPanelFrame(plotCanvas_, plotRect, theme);
    }

    void ScatterPlotView::RefreshBoxplotSelectionVisuals(
        std::set<int> const& rows)
    {
        ::rlispstat::windows::performance::Scope timing(
            "Plot.RefreshBoxplotSelectionVisuals",
            "plot=" + plotId_ + ",changed=" + std::to_string(rows.size()));
        if (!currentModel_ || !boxplotSelectionCanvas_) return;
        using namespace ::rlispstat::core;
        const auto theme = PlotThemeStyleForName(themeName_);
        boxplotSelectionCanvas_.Children().Clear();
        const auto connections = BuildBoxplotConnectionLinePlan(
            boxplotGeometry_, boxplotCategoryOrder_, selectedRows_,
            BoxplotUsesVariableAxes(*currentModel_),
            currentModel_->boxplotConnectRows);
        for (int selectedPass = 0; selectedPass < 2; ++selectedPass)
            for (const auto& connection : connections)
            {
                if (static_cast<int>(connection.selected) != selectedPass) continue;
                const auto found = pointColors_.find(connection.caseId);
                auto color = found != pointColors_.end()
                    ? (connection.selected
                        ? PlotSelectedColorForNameOrHex(found->second, 0.96)
                        : LightPaletteColor(found->second, 0.96))
                    : (connection.selected ? theme.selectedMarkFill : theme.markStroke);
                if (found == pointColors_.end() && !connection.selected)
                    color.a = connection.dimmed ? 0.18 : 0.42;
                boxplotSelectionCanvas_.Children().Append(Polyline(
                    connection.points, Brush(color),
                    std::max(0.5, currentModel_->boxplotConnectionLineWidth)));
            }
        if (!currentModel_->boxplotShowPoints) return;
        const auto points = BuildBoxplotPointDrawPlan(
            boxplotGeometry_, selectedRows_);
        std::map<int, std::size_t> nextVisual;
        for (int selectedPass = 0; selectedPass < 2; ++selectedPass)
            for (const auto& point : points)
            {
                if (static_cast<int>(point.selected) != selectedPass) continue;
                const auto colorIt = pointColors_.find(point.caseId);
                const bool explicitColor = colorIt != pointColors_.end();
                const auto fill = explicitColor
                    ? (point.selected
                        ? PlotSelectedColorForNameOrHex(colorIt->second)
                        : PlotColorForNameOrHex(colorIt->second))
                    : (point.selected ? theme.selectedMarkFill : theme.markFill);
                const double fillAlpha = point.selected ? 1.0
                    : (point.dimmed ? 0.24 : 0.72);
                const auto stroke = point.selected ? theme.selectedMarkStroke
                    : (explicitColor ? fill : theme.markStroke);
                const double radius = 3.2 * pointSizeScale_;
                if (point.selected)
                    for (const auto& ring : CreateSelectionRingVisuals(
                             point.point, radius, theme, fill))
                        boxplotSelectionCanvas_.Children().Append(ring);
                if (!rows.count(point.caseId)) continue;
                auto found = boxplotPointVisuals_.find(point.caseId);
                const std::size_t index = nextVisual[point.caseId]++;
                if (found == boxplotPointVisuals_.end() ||
                    index >= found->second.size())
                {
                    RenderPlot(std::max(360.0, plotCanvas_.ActualWidth()),
                               std::max(280.0, plotCanvas_.ActualHeight()));
                    return;
                }
                auto& dot = found->second[index];
                dot.Fill(Brush(fill, fillAlpha));
                dot.Stroke(Brush(stroke, point.selected ? 1.0
                    : (point.dimmed ? 0.18 : 0.55)));
                dot.StrokeThickness(point.selected
                    ? theme.selectedMarkStrokeWidth : 0.8);
                Controls::Canvas::SetZIndex(dot, point.selected ? 4 : 3);
            }
    }

    void ScatterPlotView::RenderHistogram(
        ::rlispstat::core::Rect const& plotRect,
        ::rlispstat::core::PlotThemeStyleSpec const& theme)
    {
        using namespace ::rlispstat::core;
        plotCanvas_.Children().Clear(); pointsCanvas_.Children().Clear();
        renderedPoints_.clear(); histogramBinRows_.clear();
        plotCanvas_.Background(Brush(theme.background));
        plotCanvas_.Children().Append(Rectangle(plotRect, Brush(theme.panel),
            TransparentBrush(), 0.0));

        histogramBinRows_.reserve(currentModel_->histogramBins.size());
        std::vector<int> counts;
        for (auto const& bin : currentModel_->histogramBins)
        {
            histogramBinRows_.push_back(bin.rows);
            counts.push_back(static_cast<int>(bin.rows.size()));
        }
        histogramLayout_.plotRect = plotRect;
        histogramLayout_.counts = counts;
        histogramLayout_.maxCount = std::max(1, HistogramMaximumBinCount(histogramBinRows_));
        // HistogramLayout supports either one height per bin (`counts`) or an
        // arbitrary value series (`values`).  A histogram must use the former;
        // passing the raw observations as `values` turns every case into a bar.
        histogramLayout_.values.clear();
        histogramLayout_.maxValue = 0.0;

        std::vector<HistogramRugCase> rugCases;
        if (currentModel_->histogramShowRug)
            for (auto const& point : currentModel_->histogramPoints)
                if (point.bin > 0) rugCases.push_back({ point.row,
                    static_cast<std::size_t>(point.bin - 1) });
        HistogramRenderInput input;
        input.layout = histogramLayout_;
        input.binRows = histogramBinRows_;
        input.selection = selectedRows_;
        input.rowColors = pointColors_;
        input.showCounts = currentModel_->histogramShowCounts;
        input.showColorSegments = !currentModel_->colorByVariable.empty() ||
            HistogramColorSegmentsVisible(
                currentModel_->histogramShowDensity, currentModel_->histogramDensityMode);
        input.showRug = currentModel_->histogramShowRug;
        input.rugCases = rugCases;
        input.densityCurves = DensityCurvesForHistogram(
            *currentModel_, selectedRows_, pointColors_);
        if (!currentModel_->histogramBins.empty())
        {
            input.densityXMinimum = currentModel_->histogramBins.front().lower;
            input.densityXMaximum = currentModel_->histogramBins.back().upper;
        }
        const auto plan = BuildHistogramRenderPlan(input);
        auto mutedBrush = Brush(theme.mutedText);

        for (int tick = 0; tick <= 4; ++tick)
        {
            const double value = histogramLayout_.maxCount * tick / 4.0;
            const double y = ZeroBaselineY(plotRect, tick / 4.0);
            plotCanvas_.Children().Append(Line(plotRect.x, y,
                plotRect.x + plotRect.width, y, Brush(theme.majorGrid), 1.0));
            if (currentModel_->histogramShowTickMarks)
                plotCanvas_.Children().Append(Line(plotRect.x - 4.0, y,
                    plotRect.x, y, Brush(theme.axis), 1.0));
            if (currentModel_->histogramShowTickLabels)
                plotCanvas_.Children().Append(Label(to_hstring(FormatDouble(value, 1)).c_str(),
                    plotRect.x - 38.0, y - 7.0, 10.5, mutedBrush));
        }
        if (!theme.showPanelBorder)
        {
            plotCanvas_.Children().Append(Line(plotRect.x, plotRect.y,
                plotRect.x, plotRect.y + plotRect.height, Brush(theme.axis), 1.4));
            plotCanvas_.Children().Append(Line(plotRect.x, ZeroBaselineY(plotRect, 0.0),
                plotRect.x + plotRect.width, ZeroBaselineY(plotRect, 0.0), Brush(theme.axis), 1.4));
        }

        if (!currentModel_->histogramBins.empty())
        {
            const double minimum = currentModel_->histogramBins.front().lower;
            const double maximum = currentModel_->histogramBins.back().upper;
            const Rect contentRect = ZeroBaselineContentRect(plotRect);
            for (int tick = 0; tick <= 4; ++tick)
            {
                const double fraction = tick / 4.0;
                const double x = contentRect.x + fraction * contentRect.width;
                const double value = minimum + fraction * (maximum - minimum);
                if (currentModel_->histogramShowTickMarks)
                    plotCanvas_.Children().Append(Line(x, ZeroBaselineY(plotRect, 0.0),
                        x, ZeroBaselineY(plotRect, 0.0) + 4.0, Brush(theme.axis), 1.0));
                if (currentModel_->histogramShowTickLabels)
                {
                    auto label = Label(to_hstring(FormatDouble(value, 3)).c_str(),
                        x - 34.0, plotRect.y + plotRect.height + 7.0, 10.0, mutedBrush);
                    label.Width(68.0); label.TextAlignment(TextAlignment::Center);
                    plotCanvas_.Children().Append(label);
                }
            }
        }

        for (auto const& bar : plan.bars)
        {
            plotCanvas_.Children().Append(Rectangle(bar.rect, Brush(theme.geomFill),
                TransparentBrush(), 0.0));
            for (auto const& segment : bar.colorSegments)
            {
                auto color = segment.colorName.empty()
                    ? theme.geomFill : LightPaletteColor(segment.colorName);
                plotCanvas_.Children().Append(Rectangle(segment.rect,
                    Brush(color, segment.alpha), TransparentBrush(), 0.0));
            }
            for (auto const& segment : bar.selectedSegments)
            {
                auto color = segment.colorName.empty() || segment.defaultSelection
                    ? theme.selectedMarkFill : PlotSelectedColorForNameOrHex(segment.colorName);
                plotCanvas_.Children().Append(Rectangle(segment.rect,
                    Brush(color, std::max(0.18, segment.alpha)), TransparentBrush(), 0.0));
            }
            // Draw the structural boundary last so selection and manually
            // assigned case colours cannot cover the histogram bin outline.
            AppendHistogramBinOutline(plotCanvas_, bar.rect);
            if (bar.showCount)
            {
                auto count = Label(to_hstring(bar.countLabel).c_str(), bar.rect.x,
                    std::max(plotRect.y, bar.rect.y - 18.0), 10.5, mutedBrush);
                count.Width(bar.rect.width); count.TextAlignment(TextAlignment::Center);
                plotCanvas_.Children().Append(count);
            }
        }
        for (auto const& rug : plan.rugs)
            plotCanvas_.Children().Append(Line(rug.start.x, rug.start.y, rug.end.x, rug.end.y,
                Brush(theme.markStroke, selectedRows_.count(rug.caseId) ? 1.0 : 0.42), 1.0));
        if ((currentModel_->labelDisplayMode == "all" ||
             currentModel_->labelDisplayMode == "selected") &&
            !rowLabels_.empty() && !currentModel_->histogramBins.empty())
        {
            const double minimum = currentModel_->histogramBins.front().lower;
            const double maximum = currentModel_->histogramBins.back().upper;
            const double span = maximum - minimum;
            std::map<int, int> labelSlots;
            if (span > 0.0)
            {
                for (auto const& point : currentModel_->histogramPoints)
                {
                    if (currentModel_->labelDisplayMode == "selected" &&
                        selectedRows_.count(point.row) == 0) continue;
                    auto text = rowLabels_.find(point.row);
                    if (text == rowLabels_.end() || text->second.empty() || point.bin <= 0 ||
                        static_cast<std::size_t>(point.bin) > plan.bars.size() ||
                        !std::isfinite(point.x)) continue;
                    const double x = plotRect.x + std::clamp(
                        (point.x - minimum) / span, 0.0, 1.0) * plotRect.width;
                    const auto& bar = plan.bars[static_cast<std::size_t>(point.bin - 1)];
                    const int slot = labelSlots[point.bin]++ % 4;
                    auto label = Label(to_hstring(text->second).c_str(), x + 3.0,
                        std::max(plotRect.y + 2.0, bar.rect.y - 15.0 - slot * 12.0),
                        9.0, Brush(theme.text));
                    label.MaxWidth(120.0);
                    label.TextTrimming(TextTrimming::CharacterEllipsis);
                    label.IsHitTestVisible(false);
                    plotCanvas_.Children().Append(label);
                }
            }
        }
        for (auto const& overlap : plan.density.overlaps)
            plotCanvas_.Children().Append(Polygon(overlap.fillPolygon,
                Brush(PlotColorForNameOrHex(overlap.firstColorName), 0.12)));
        for (auto const& curve : plan.density.curves)
        {
            const auto color = curve.colorName.empty()
                ? theme.smooth : PlotColorForNameOrHex(curve.colorName);
            if (!curve.fillPolygon.empty())
                plotCanvas_.Children().Append(Polygon(curve.fillPolygon, Brush(color, 0.10)));
            plotCanvas_.Children().Append(Polyline(curve.linePoints, Brush(color), 1.8));
        }

        auto title = Label(to_hstring(currentModel_->title.empty() ? "Histogram" : currentModel_->title).c_str(),
            plotRect.x, 24.0, 16.0, Brush(theme.text));
        title.Width(plotRect.width); title.TextAlignment(TextAlignment::Center);
        title.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        plotCanvas_.Children().Append(title);
        xAxisLabel_ = Label(to_hstring(currentModel_->xLabel).c_str(), plotRect.x,
            plotRect.y + plotRect.height + 35.0, 12.5, Brush(theme.text));
        xAxisLabel_.Width(plotRect.width); xAxisLabel_.TextAlignment(TextAlignment::Center);
        plotCanvas_.Children().Append(xAxisLabel_);
        AttachAxisVariableMenu(xAxisLabel_, true);

        pointsCanvas_.Width(plotCanvas_.ActualWidth()); pointsCanvas_.Height(plotCanvas_.ActualHeight());
        selectionRectangle_.Visibility(Visibility::Collapsed);
        selectionRectangle_.Stroke(Brush(theme.accent));
        selectionRectangle_.Fill(Brush(theme.accent, 0.16));
        pointsCanvas_.Children().Append(selectionRectangle_);
        plotCanvas_.Children().Append(pointsCanvas_);
        Controls::Canvas::SetZIndex(pointsCanvas_, 10);
        AppendPanelFrame(plotCanvas_, plotRect, theme);
    }

    void ScatterPlotView::RenderBarplot(
        double width,
        double height,
        ::rlispstat::core::PlotThemeStyleSpec const& theme)
    {
        using namespace ::rlispstat::core;
        plotCanvas_.Children().Clear(); pointsCanvas_.Children().Clear();
        renderedPoints_.clear();
        barplotBins_ = BarplotBinsForModel(*currentModel_);
        const auto xVariables = BarplotXVariablesForModel(*currentModel_);
        BarplotLayoutInput input;
        input.viewRect = { 0.0, 0.0, width, height };
        input.xVariables = xVariables;
        input.bins = barplotBins_;
        input.mode = currentModel_->barplotMode;
        input.widthMode = currentModel_->barplotWidthMode;
        input.hasSplit = !currentModel_->barplotSplitVariable.empty();
        input.showCompositionStrip = currentModel_->barplotRowColorDisplay == "composition_strip";
        barplotLayout_ = BuildBarplotLayout(input);
        const auto& plotRect = barplotLayout_.plotRect;
        plotCanvas_.Background(Brush(theme.background));
        plotCanvas_.Children().Append(Rectangle(plotRect, Brush(theme.panel),
            TransparentBrush(), 0.0));
        auto mutedBrush = Brush(theme.mutedText);
        for (auto const& tick : BarplotYAxisTicks(barplotLayout_.yMaximum,
                                                  currentModel_->barplotMode))
        {
            const double y = ZeroBaselineY(plotRect,
                tick.value / std::max(1.0, barplotLayout_.yMaximum));
            plotCanvas_.Children().Append(Line(plotRect.x, y,
                plotRect.x + plotRect.width, y, Brush(theme.majorGrid), 1.0));
            auto tickLabel = Label(to_hstring(tick.label).c_str(),
                plotRect.x - 42.0, y - 7.0, 10.5, mutedBrush);
            tickLabel.Width(34.0);
            tickLabel.TextAlignment(TextAlignment::Right);
            plotCanvas_.Children().Append(tickLabel);
        }
        if (!theme.showPanelBorder)
        {
            plotCanvas_.Children().Append(Line(plotRect.x, plotRect.y,
                plotRect.x, plotRect.y + plotRect.height, Brush(theme.axis), 1.4));
            plotCanvas_.Children().Append(Line(plotRect.x, ZeroBaselineY(plotRect, 0.0),
                plotRect.x + plotRect.width, ZeroBaselineY(plotRect, 0.0), Brush(theme.axis), 1.4));
        }

        const bool hasSplit = !currentModel_->barplotSplitVariable.empty();
        const auto splitLevels = BarplotSplitLevelsForBins(barplotBins_, hasSplit);
        std::vector<Controls::TextBlock> valueLabels;
        if (hasSplit)
        {
            const auto totals = BarplotSplitLevelTotalsForBins(barplotBins_, splitLevels);
            const auto sideLabels = BuildBarplotSideLabelDrawPlan(
                plotRect, currentModel_->barplotSplitVariable, splitLevels, totals,
                currentModel_->barplotRowColorDisplay == "composition_strip");
            auto splitTitle = Label(to_hstring(sideLabels.splitTitle).c_str(),
                sideLabels.splitTitleRect.x, sideLabels.splitTitleRect.y,
                10.0, Brush(theme.text));
            splitTitle.Width(std::max(24.0,
                sideLabels.splitTitleRect.width - 14.0));
            splitTitle.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
            splitTitle.TextTrimming(TextTrimming::CharacterEllipsis);
            Controls::Canvas::SetZIndex(splitTitle, 20);
            plotCanvas_.Children().Append(splitTitle);
            if (hasDataFrame_)
            {
                const auto splitState = BuildBarplotSplitMenuState(
                    BarplotAvailableVariables(dataFrame_), xVariables,
                    currentModel_->barplotSplitVariable);
                auto splitMenu = Controls::MenuFlyout();
                splitMenu.AreOpenCloseAnimationsEnabled(false);
                splitMenu.Items().Append(CreateBarplotMenuItem(
                    splitState.noneOption, true));
                if (!splitState.splitOptions.empty())
                    splitMenu.Items().Append(Controls::MenuFlyoutSeparator());
                for (auto const& option : splitState.splitOptions)
                    splitMenu.Items().Append(CreateBarplotMenuItem(option, true));
                splitTitle.ContextFlyout(splitMenu);
                splitTitle.PointerPressed(
                    [splitMenu, splitTitle](auto const&, auto const& event)
                {
                    splitMenu.ShowAt(splitTitle);
                    event.Handled(true);
                });
            }
            for (auto const& item : sideLabels.splitLabels)
            {
                auto label = Label(to_hstring(item.label).c_str(), item.rect.x,
                    item.rect.y, 10.0, mutedBrush);
                // Keep split-level labels in their own column, distinctly to
                // the left of the numeric Y scale even at 150% DPI scaling.
                label.Width(std::max(24.0, item.rect.width - 26.0));
                label.TextAlignment(TextAlignment::Right);
                label.TextTrimming(TextTrimming::CharacterEllipsis);
                Controls::Canvas::SetZIndex(label, 20);
                const auto rows = BarplotRowsForSplitLevel(barplotBins_, item.label);
                Controls::ToolTipService::SetToolTip(label, box_value(to_hstring(
                    BarplotSplitLevelTooltipText(currentModel_->barplotSplitVariable,
                        item.label, rows, selectedRows_))));
                label.PointerPressed([this, rows, label](auto const&, auto const& event)
                {
                    auto point = event.GetCurrentPoint(label);
                    if (point.Properties().IsLeftButtonPressed())
                    {
                        if (!RowSelectionGestureEnabled()) return;
                        SelectBarplotRows(rows, SelectionModeFor(
                            event.KeyModifiers(), currentModel_
                                ? currentModel_->selectionMode : "replace"));
                        event.Handled(true);
                    }
                });
                plotCanvas_.Children().Append(label);
            }
            if (sideLabels.showRowStripLabel)
            {
                auto label = Label(to_hstring(sideLabels.rowStripLabel).c_str(),
                    sideLabels.rowStripLabelRect.x, sideLabels.rowStripLabelRect.y,
                    10.0, mutedBrush);
                label.Width(sideLabels.rowStripLabelRect.width);
                label.TextAlignment(TextAlignment::Right);
                plotCanvas_.Children().Append(label);
            }
        }
        const BarplotVisualState visual = BarplotVisualStateForModel(*currentModel_);
        const auto paletteOrder = BarplotPaletteOrder();
        const bool showPatterns = BarplotEncodingShowsPatterns(*currentModel_);
        const bool showSegmentColors = BarplotEncodingShowsColor(*currentModel_);

        auto keepAutomaticBarFillLight = [&](PlotRGBA color)
        {
            // Bar interiors are large surfaces, not point marks. Themes such
            // as DataDesk and Beige deliberately use a nearly black markFill;
            // applying it to an automatic bar destroys the linked-colour and
            // selection encoding. Preserve its hue on a light surface.
            const double luminance = 0.2126 * color.r + 0.7152 * color.g +
                0.0722 * color.b;
            if (luminance >= 0.38) return color;
            constexpr double ink = 0.24;
            return PlotRGBA{
                ink * color.r + (1.0 - ink) * theme.panel.r,
                ink * color.g + (1.0 - ink) * theme.panel.g,
                ink * color.b + (1.0 - ink) * theme.panel.b,
                color.a };
        };
        auto automaticSegmentColor = [&](std::size_t levelIndex)
        {
            if (!theme.coordinatedMarks) return theme.panel;
            switch (levelIndex % 3)
            {
            case 0: return keepAutomaticBarFillLight(theme.geomFill);
            case 1: return keepAutomaticBarFillLight(theme.auxiliary);
            default: return keepAutomaticBarFillLight(theme.markFill);
            }
        };
        auto baseColor = [&](BarplotSegmentVisualStyle const& style,
                             std::size_t levelIndex)
        {
            if (BarplotLevelIsMissing(style.colorKey))
                return PlotRGBA{ 0.72, 0.72, 0.72, 1.0 };
            if (!showSegmentColors || ColorKeyIsDefaultNeutral(style.colorKey))
                return automaticSegmentColor(levelIndex);
            return PlotColorForNameOrHex(style.colorKey);
        };
        auto lightRowColor = [&](std::string const& key)
        {
            // Neutral rows use the same fill role as histogram bins. Explicit
            // case colours keep the data-sheet light palette tone.
            if (key.empty() || key == "__default__" ||
                ColorKeyIsDefaultNeutral(key) || key == "white")
                return theme.geomFill;
            return LightPaletteColor(key, 1.0);
        };
        auto appendPattern = [&](Rect const& rect, std::string const& pattern,
                                 PlotRGBA color)
        {
            if (!showPatterns || pattern.empty() || pattern == "none" ||
                rect.width <= 1.0 || rect.height <= 1.0) return;
            auto layer = Controls::Canvas();
            layer.Width(rect.width); layer.Height(rect.height);
            Controls::Canvas::SetLeft(layer, rect.x);
            Controls::Canvas::SetTop(layer, rect.y);
            auto clip = Media::RectangleGeometry();
            clip.Rect({ 0.0f, 0.0f, static_cast<float>(rect.width),
                        static_cast<float>(rect.height) });
            layer.Clip(clip);
            const auto stroke = Brush(color, 0.58);
            auto diagonals = [&](bool backwards)
            {
                for (double offset = -rect.height; offset <= rect.width;
                     offset += 8.0)
                {
                    const double x1 = std::max(0.0, offset);
                    const double y1 = std::max(0.0, -offset);
                    const double x2 = std::min(rect.width, offset + rect.height);
                    const double y2 = x2 - offset;
                    layer.Children().Append(backwards
                        ? Line(rect.width - x1, y1, rect.width - x2, y2, stroke, 0.8)
                        : Line(x1, y1, x2, y2, stroke, 0.8));
                }
            };
            if (pattern == "diagonal_slash" || pattern == "crosshatch") diagonals(false);
            if (pattern == "diagonal_backslash" || pattern == "crosshatch") diagonals(true);
            if (pattern == "vertical" || pattern == "crosshatch")
                for (double x = 4.0; x < rect.width; x += 8.0)
                    layer.Children().Append(Line(x, 0.0, x, rect.height, stroke, 0.8));
            if (pattern == "horizontal" || pattern == "crosshatch")
                for (double y = 4.0; y < rect.height; y += 8.0)
                    layer.Children().Append(Line(0.0, y, rect.width, y, stroke, 0.8));
            if (pattern == "dots")
                for (double y = 4.0; y < rect.height; y += 8.0)
                    for (double x = 4.0; x < rect.width; x += 8.0)
                    {
                        auto dot = Shapes::Ellipse();
                        dot.Width(1.8); dot.Height(1.8); dot.Fill(stroke);
                        Controls::Canvas::SetLeft(dot, x - 0.9);
                        Controls::Canvas::SetTop(dot, y - 0.9);
                        layer.Children().Append(dot);
                    }
            plotCanvas_.Children().Append(layer);
        };
        auto appendRowIdentity = [&](Rect const& rect, std::vector<int> const& rows)
        {
            for (auto const& slice : BarplotVerticalRowColorCompositionSlices(
                     rect, rows, pointColors_, paletteOrder, true, true))
                plotCanvas_.Children().Append(Rectangle(
                    slice.rect, Brush(lightRowColor(slice.colorKey)),
                    TransparentBrush(), 0.0));
        };
        auto appendSegmentHitTarget = [&](Rect const& rect,
                                          std::string const& tooltip)
        {
            auto hit = Rectangle(rect, TransparentBrush(), TransparentBrush(), 0.0);
            Controls::ToolTipService::SetToolTip(hit, box_value(to_hstring(tooltip)));
            // Match macOS: bar hit targets keep their tooltip and left-click
            // selection behaviour, while right-click bubbles to the general
            // bar-chart context menu attached to the window.
            pointsCanvas_.Children().Append(hit);
        };

        for (std::size_t index = 0; index < barplotBins_.size(); ++index)
        {
            auto const& bin = barplotBins_[index];
            const auto rect = BarplotBarRect(barplotLayout_, index);
            if (hasSplit)
            {
                const auto segments = BuildBarplotSegmentDrawPlan(
                    rect, bin, splitLevels, visual, pointColors_);
                for (auto const& segment : segments)
                {
                    Controls::TextBlock valueLabel{ nullptr };
                    const auto fill = baseColor(segment.style, segment.levelIndex);
                    plotCanvas_.Children().Append(Rectangle(segment.rect,
                        Brush(fill, segment.style.alpha), Brush(theme.geomStroke),
                        std::max(0.8, currentModel_->barplotSplitStrokeWidth * 0.55)));
                    appendPattern(segment.rect, segment.style.pattern, theme.geomStroke);
                    if (segment.useRowColorIdentityLayer)
                        appendRowIdentity(segment.rect, segment.rows);
                    plotCanvas_.Children().Append(Rectangle(segment.rect,
                        TransparentBrush(), Brush(theme.geomStroke),
                        std::max(0.8, currentModel_->barplotSplitStrokeWidth * 0.55)));
                    if (currentModel_->barplotShowConditionalPercent &&
                        segment.rect.height >= 16.0 && segment.rect.width >= 34.0)
                    {
                        valueLabel = Label(to_hstring(FormatPercent(
                            segment.conditionalPercent / 100.0, 0)).c_str(),
                            segment.rect.x, segment.rect.y + segment.rect.height / 2.0 - 7.0,
                            10.0, Brush(PlotRGBA{ 0.10, 0.10, 0.10, 1.0 }));
                        valueLabel.Width(segment.rect.width);
                        valueLabel.TextAlignment(TextAlignment::Center);
                        valueLabel.IsHitTestVisible(false);
                        Controls::Canvas::SetZIndex(valueLabel, 20);
                        plotCanvas_.Children().Append(valueLabel);
                    }
                    valueLabels.push_back(valueLabel);
                    const auto reference = BarplotSegmentReferenceForIndices(
                        barplotBins_, index, segment.segmentIndex);
                    if (reference)
                    {
                        const auto style = ResolveBarplotSegmentVisual(
                            *currentModel_, reference->category,
                            reference->segment.level, segment.levelIndex);
                        appendSegmentHitTarget(segment.rect,
                            BarplotSegmentTooltipText(
                                currentModel_->xLabel,
                                currentModel_->barplotSplitVariable,
                                currentModel_->barplotWidthMode, bin,
                                currentModel_->barplotTotalN, reference->segment,
                                style, pointColors_, selectedRows_, paletteOrder,
                                currentModel_->barplotRowColorDisplay != "hide"));
                    }
                }
            }
            else
            {
                Controls::TextBlock valueLabel{ nullptr };
                plotCanvas_.Children().Append(Rectangle(rect, Brush(theme.geomFill),
                    Brush(theme.geomStroke), 1.2));
                const auto reference = BarplotSegmentReferenceForIndices(
                    barplotBins_, index, 0);
                const auto style = reference
                    ? ResolveBarplotSegmentVisual(*currentModel_, reference->category,
                          reference->segment.level, 0)
                    : BarplotSegmentVisualStyle{};
                if (currentModel_->barplotRowColorDisplay == "bar_fill")
                    appendRowIdentity(rect, bin.rows);
                appendPattern(rect, style.pattern, theme.geomStroke);
                plotCanvas_.Children().Append(Rectangle(rect, TransparentBrush(),
                    Brush(theme.geomStroke), 1.2));
                if (rect.height >= 22.0 && rect.width >= 28.0)
                {
                    valueLabel = Label(to_hstring(std::to_string(bin.n)).c_str(),
                        rect.x, rect.y + rect.height / 2.0 - 7.0,
                        10.0, Brush(PlotRGBA{ 0.10, 0.10, 0.10, 1.0 }));
                    valueLabel.Width(rect.width);
                    valueLabel.TextAlignment(TextAlignment::Center);
                    valueLabel.IsHitTestVisible(false);
                    Controls::Canvas::SetZIndex(valueLabel, 20);
                    plotCanvas_.Children().Append(valueLabel);
                }
                valueLabels.push_back(valueLabel);
                if (currentModel_->barplotRowColorDisplay == "composition_strip")
                {
                    const Rect strip{ rect.x, plotRect.y + plotRect.height + 2.0,
                                      rect.width, 4.0 };
                    for (auto const& slice : BarplotHorizontalRowColorCompositionSlices(
                             strip, bin.rows, pointColors_, paletteOrder, true, false))
                        plotCanvas_.Children().Append(Rectangle(
                            slice.rect, Brush(lightRowColor(slice.colorKey)),
                            TransparentBrush(), 0.0));
                }
                if (reference)
                    appendSegmentHitTarget(rect,
                        BarplotSegmentTooltipText(
                            currentModel_->xLabel, "", currentModel_->barplotWidthMode,
                            bin, currentModel_->barplotTotalN, reference->segment,
                            style, pointColors_, selectedRows_, paletteOrder,
                            currentModel_->barplotRowColorDisplay != "hide"));
            }
        }
        const double labelOffset = BarplotCategoryLabelVerticalOffset(
            currentModel_->barplotRowColorDisplay == "composition_strip");
        for (auto const& item : BuildBarplotCategoryLabelDrawPlan(
                 barplotLayout_, barplotBins_, xVariables, labelOffset))
        {
            auto label = Label(to_hstring(item.label).c_str(), item.rect.x, item.rect.y,
                10.0, mutedBrush);
            label.Width(item.rect.width); label.TextAlignment(TextAlignment::Center);
            label.TextTrimming(TextTrimming::CharacterEllipsis);
            Controls::Canvas::SetZIndex(label, 20);
            const auto components = item.barIndex < barplotBins_.size()
                ? BarplotCategoryComponents(xVariables,
                    barplotBins_[item.barIndex].category)
                : std::vector<std::string>{};
            const std::string value = item.variableIndex < components.size()
                ? components[item.variableIndex] : item.label;
            const auto rows = BarplotRowsForXVariableValue(
                barplotBins_, xVariables, item.variableIndex, value);
            const std::string variable = item.variableIndex < xVariables.size()
                ? xVariables[item.variableIndex] : currentModel_->xLabel;
            Controls::ToolTipService::SetToolTip(label, box_value(to_hstring(
                BarplotCategoryTooltipText(variable, value, rows, selectedRows_))));
            label.PointerPressed([this, rows, label](auto const&, auto const& event)
            {
                auto point = event.GetCurrentPoint(label);
                if (point.Properties().IsLeftButtonPressed())
                {
                    if (!RowSelectionGestureEnabled()) return;
                    SelectBarplotRows(rows, SelectionModeFor(
                        event.KeyModifiers(), currentModel_
                            ? currentModel_->selectionMode : "replace"));
                    event.Handled(true);
                }
            });
            plotCanvas_.Children().Append(label);
        }
        auto title = Label(to_hstring(currentModel_->title.empty() ? "Bar chart" : currentModel_->title).c_str(),
            plotRect.x, 10.0, 16.0, Brush(theme.text));
        title.Width(plotRect.width); title.TextAlignment(TextAlignment::Center);
        title.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        plotCanvas_.Children().Append(title);
        auto xLabel = Label(to_hstring(currentModel_->xLabel).c_str(), plotRect.x,
            plotRect.y + plotRect.height + 42.0 + 14.0 * std::max<std::size_t>(1, xVariables.size()),
            12.0, Brush(theme.text));
        xLabel.Width(plotRect.width); xLabel.TextAlignment(TextAlignment::Center);
        Controls::Canvas::SetZIndex(xLabel, 20);
        plotCanvas_.Children().Append(xLabel);
        if (hasDataFrame_)
        {
            const auto state = BuildBarplotXMenuState(
                BarplotAvailableVariables(dataFrame_), xVariables,
                currentModel_->barplotSplitVariable);
            auto xMenu = Controls::MenuFlyout();
            xMenu.AreOpenCloseAnimationsEnabled(false);
            for (auto const& option : state.setOptions)
                xMenu.Items().Append(CreateBarplotMenuItem(option, true));
            xLabel.ContextFlyout(xMenu);
            xLabel.PointerPressed([xMenu, xLabel](auto const&, auto const& event)
            {
                xMenu.ShowAt(xLabel);
                event.Handled(true);
            });
        }

        barplotSelectionCanvas_ = Controls::Canvas();
        barplotSelectionCanvas_.Width(width);
        barplotSelectionCanvas_.Height(height);
        barplotSelectionCanvas_.IsHitTestVisible(false);
        Controls::Canvas::SetZIndex(barplotSelectionCanvas_, 5);
        plotCanvas_.Children().Append(barplotSelectionCanvas_);

        // Keep one retained selection layer per bar segment. Selection changes
        // can then redraw only the segments that contain changed rows instead
        // of clearing and rebuilding the overlay for the entire chart.
        barplotSelectionLayers_.clear();
        barplotSelectionLayersByRow_.clear();
        auto appendSelectionLayer = [&](Rect const& rect,
                                        std::vector<int> const& rows,
                                        std::string colorOverride = {})
        {
            auto layerCanvas = Controls::Canvas();
            layerCanvas.Width(rect.width);
            layerCanvas.Height(rect.height);
            layerCanvas.IsHitTestVisible(false);
            Controls::Canvas::SetLeft(layerCanvas, rect.x);
            Controls::Canvas::SetTop(layerCanvas, rect.y);
            barplotSelectionCanvas_.Children().Append(layerCanvas);
            const std::size_t layerIndex = barplotSelectionLayers_.size();
            const auto valueLabel = layerIndex < valueLabels.size()
                ? valueLabels[layerIndex] : Controls::TextBlock{ nullptr };
            barplotSelectionLayers_.push_back({
                layerCanvas, Rect{0.0, 0.0, rect.width, rect.height}, rows,
                std::move(colorOverride), valueLabel });
            for (int row : rows)
                barplotSelectionLayersByRow_[row].push_back(layerIndex);
        };
        if (hasSplit)
        {
            for (std::size_t index = 0; index < barplotBins_.size(); ++index)
            {
                const auto rect = BarplotBarRect(barplotLayout_, index);
                for (auto const& segment : BuildBarplotSegmentDrawPlan(
                         rect, barplotBins_[index], splitLevels, visual,
                         pointColors_))
                {
                    std::string colorOverride;
                    if (segment.style.hasSegmentColorOverride &&
                        !ColorKeyIsDefaultNeutral(segment.style.colorKey))
                        colorOverride = segment.style.colorKey;
                    appendSelectionLayer(segment.rect, segment.rows,
                                         std::move(colorOverride));
                }
            }
        }
        else
        {
            for (std::size_t index = 0; index < barplotBins_.size(); ++index)
                appendSelectionLayer(BarplotBarRect(barplotLayout_, index),
                                     barplotBins_[index].rows);
        }
        RefreshBarplotSelectionVisuals(theme);

        pointsCanvas_.Width(width); pointsCanvas_.Height(height);
        selectionRectangle_.Visibility(Visibility::Collapsed);
        selectionRectangle_.Stroke(Brush(theme.accent));
        selectionRectangle_.Fill(Brush(theme.accent, 0.16));
        pointsCanvas_.Children().Append(selectionRectangle_);
        Controls::Canvas::SetZIndex(pointsCanvas_, 10);
        plotCanvas_.Children().Append(pointsCanvas_);
        AppendPanelFrame(plotCanvas_, plotRect, theme);
    }

    void ScatterPlotView::RefreshBarplotSelectionVisuals(
        ::rlispstat::core::PlotThemeStyleSpec const& theme,
        std::set<int> const* changedRows)
    {
        using namespace ::rlispstat::core;
        if (!barplotSelectionCanvas_ || !currentModel_ ||
            currentModel_->kind != "barplot") return;
        ::rlispstat::windows::performance::Scope timing(
            "Plot.RefreshBarplotSelection",
            "plot=" + plotId_ + ",selected=" +
                std::to_string(selectedRows_.size()));

        const bool showOverlay = BarplotSelectionDisplayShowsOverlay(
            currentModel_->barplotSelectionDisplay);
        const bool showOutline = BarplotSelectionDisplayShowsOutline(
            currentModel_->barplotSelectionDisplay);
        std::set<std::size_t> layersToRefresh;
        if (changedRows)
        {
            for (int row : *changedRows)
            {
                auto found = barplotSelectionLayersByRow_.find(row);
                if (found == barplotSelectionLayersByRow_.end()) continue;
                layersToRefresh.insert(found->second.begin(), found->second.end());
            }
        }
        else
        {
            for (std::size_t index = 0;
                 index < barplotSelectionLayers_.size(); ++index)
                layersToRefresh.insert(index);
        }
        if (layersToRefresh.empty()) return;

        const auto paletteOrder = BarplotPaletteOrder();
        auto lightRowColor = [&](std::string const& key)
        {
            if (key.empty() || key == "__default__" ||
                ColorKeyIsDefaultNeutral(key) || key == "white")
                return theme.geomFill;
            return LightPaletteColor(key, 1.0);
        };
        auto selectedRowColor = [&](std::string const& key)
        {
            if (key.empty() || key == "__default__" ||
                ColorKeyIsDefaultNeutral(key) || key == "white")
                return theme.selectedMarkFill;
            return PlotSelectedColorForNameOrHex(key);
        };
        const std::map<int, std::string> noRowColors;
        auto refreshLayer = [&](BarplotSelectionLayer& layer)
        {
            auto children = layer.canvas.Children();
            children.Clear();
            if (!showOverlay && !showOutline)
            {
                if (layer.valueLabel)
                    layer.valueLabel.Foreground(Brush(PlotRGBA{ 0.10, 0.10, 0.10, 1.0 }));
                return;
            }
            const bool usesOverride = !layer.colorOverride.empty();
            const auto& colors = usesOverride ? noRowColors : pointColors_;
            const std::string defaultKey = usesOverride
                ? layer.colorOverride : "__default__";
            const auto plan = BuildBarplotSelectionSlicePlan(
                layer.localRect, layer.rows, selectedRows_, colors,
                paletteOrder, "black", defaultKey);
            if (layer.valueLabel)
            {
                const bool selectedBehindText = showOverlay &&
                    BarplotSelectedSliceCoversY(plan, layer.localRect.height / 2.0);
                layer.valueLabel.Foreground(Brush(selectedBehindText
                    ? PlotRGBA{ 1.0, 1.0, 1.0, 1.0 }
                    : PlotRGBA{ 0.10, 0.10, 0.10, 1.0 }));
            }
            if (showOverlay)
            {
                for (auto const& slice : plan.backgroundSlices)
                    children.Append(Rectangle(slice.rect,
                        Brush(lightRowColor(slice.colorKey)),
                        TransparentBrush(), 0.0));
                for (auto const& slice : plan.slices)
                {
                    const bool neutral = slice.colorKey.empty() ||
                        slice.colorKey == "__default__" ||
                        ColorKeyIsDefaultNeutral(slice.colorKey) ||
                        slice.colorKey == "white";
                    children.Append(Rectangle(slice.rect,
                        Brush(selectedRowColor(slice.colorKey), neutral ? 0.30 : 0.96),
                        TransparentBrush(), 0.0));
                }
            }
            if (showOutline)
                for (auto const& slice : plan.slices)
                    children.Append(Rectangle(slice.rect, TransparentBrush(),
                        Brush(selectedRowColor(slice.colorKey)), 1.8));
        };
        for (std::size_t index : layersToRefresh)
            if (index < barplotSelectionLayers_.size())
                refreshLayer(barplotSelectionLayers_[index]);
    }

    void ScatterPlotView::RenderTimeSeries(
        ::rlispstat::core::Rect const& plotRect,
        ::rlispstat::core::PlotThemeStyleSpec const& theme)
    {
        using namespace ::rlispstat::core;
        plotCanvas_.Children().Clear(); pointsCanvas_.Children().Clear();
        renderedPoints_.clear();
        timeSeriesLineVisuals_.clear();
        timeSeriesPointVisuals_.clear();
        timeSeriesLegendVisuals_.clear();
        timeSeriesSelectionCanvas_ = Controls::Canvas();
        timeSeriesSelectionCanvas_.IsHitTestVisible(false);
        timeSeriesSelectionCanvas_.Width(std::max(360.0, plotCanvas_.ActualWidth()));
        timeSeriesSelectionCanvas_.Height(std::max(280.0, plotCanvas_.ActualHeight()));
        Controls::Canvas::SetZIndex(timeSeriesSelectionCanvas_, 2);
        plotCanvas_.Background(Brush(theme.background));
        plotCanvas_.Children().Append(Rectangle(plotRect, Brush(theme.panel),
            TransparentBrush(), 0.0));
        std::vector<Point> values;
        values.reserve(currentModel_->points.size());
        for (auto const& point : currentModel_->points) values.push_back({point.x, point.y});
        auto viewport = DataViewportForPoints(values);
        if (currentModel_ && ViewportNavigationGestureEnabled() &&
            std::isfinite(currentModel_->xmin) && std::isfinite(currentModel_->xmax) &&
            std::isfinite(currentModel_->ymin) && std::isfinite(currentModel_->ymax) &&
            currentModel_->xmax > currentModel_->xmin &&
            currentModel_->ymax > currentModel_->ymin)
        {
            viewport = {currentModel_->xmin, currentModel_->xmax,
                        currentModel_->ymin, currentModel_->ymax};
        }
        diagnosticSeriesViewport_ = viewport;
        diagnosticSeriesRect_ = plotRect;
        // CompleteSelection uses the generic rendered geometry for linked
        // row-series hit testing.  Time-series rendering previously updated
        // only the diagnostic geometry, so clicks on a line were tested
        // against the viewport left by an earlier scatterplot render.
        renderedViewport_ = viewport;
        renderedPlotRect_ = plotRect;
        auto muted = Brush(theme.mutedText);
        auto text = Brush(theme.text);
        auto timeTicks = BuildTimeSeriesAxisTicks(viewport.xmin, viewport.xmax,
            currentModel_->timeSeriesTimeType, 6);
        if (timeTicks.empty())
        {
            for (int tick = 0; tick <= 5; ++tick)
            {
                const double fraction = tick / 5.0;
                timeTicks.push_back({viewport.xmin + fraction * (viewport.xmax - viewport.xmin),
                    FormatDouble(viewport.xmin + fraction * (viewport.xmax - viewport.xmin), 1)});
            }
        }
        for (auto const& tick : timeTicks)
        {
            const double fraction = (tick.value - viewport.xmin) /
                std::max(1e-12, viewport.xmax - viewport.xmin);
            const double x = plotRect.x + fraction * plotRect.width;
            plotCanvas_.Children().Append(Line(x, plotRect.y, x,
                plotRect.y + plotRect.height, Brush(theme.majorGrid), 1.0));
            auto label = Label(to_hstring(tick.label).c_str(), x - 38.0,
                plotRect.y + plotRect.height + 7.0, 10.0, muted);
            label.Width(76.0); label.TextAlignment(TextAlignment::Center);
            plotCanvas_.Children().Append(label);
        }
        for (int tick = 0; tick <= 5; ++tick)
        {
            const double fraction = tick / 5.0;
            const double y = plotRect.y + fraction * plotRect.height;
            const double value = viewport.ymax - fraction * (viewport.ymax - viewport.ymin);
            plotCanvas_.Children().Append(Line(plotRect.x, y,
                plotRect.x + plotRect.width, y, Brush(theme.majorGrid), 1.0));
            plotCanvas_.Children().Append(Label(to_hstring(FormatDouble(value, 1)).c_str(),
                plotRect.x - 42.0, y - 7.0, 10.0, muted));
        }
        if (!theme.showPanelBorder)
        {
            plotCanvas_.Children().Append(Line(plotRect.x, plotRect.y,
                plotRect.x, plotRect.y + plotRect.height, Brush(theme.axis), 1.4));
            plotCanvas_.Children().Append(Line(plotRect.x, plotRect.y + plotRect.height,
                plotRect.x + plotRect.width, plotRect.y + plotRect.height, Brush(theme.axis), 1.4));
        }
        const auto seriesSelection = PlotIsImputationDiagnostic(*currentModel_)
            ? DiagnosticPlotSelectedPointIds(*currentModel_) : selectedRows_;
        const bool hasSelection = !seriesSelection.empty();
        for (std::size_t lineIndex = 0;
             lineIndex < currentModel_->interactionPlotLines.size(); ++lineIndex)
        {
            auto const& line = currentModel_->interactionPlotLines[lineIndex];
            std::vector<Point> screen;
            for (auto const& point : line.points)
                screen.push_back(DataToScreen({point.x, point.y}, viewport, plotRect, true));
            const auto color = PlotColorForNameOrHex(line.colorKey.empty() ? "black" : line.colorKey);
            const bool fullySelected = PlotSeriesIsFullySelected(
                *currentModel_, lineIndex, seriesSelection);
            auto baseLine = Polyline(screen,
                Brush(color, hasSelection ? 0.20 : 0.78),
                fullySelected ? 3.0 : 1.6);
            plotCanvas_.Children().Append(baseLine);
            timeSeriesLineVisuals_.push_back(baseLine);
            if (hasSelection && line.points.size() >= 2)
            {
                for (std::size_t pointIndex = 1;
                     pointIndex < line.points.size(); ++pointIndex)
                {
                    auto const& previous = line.points[pointIndex - 1];
                    auto const& current = line.points[pointIndex];
                    if (previous.row <= 0 || current.row <= 0 ||
                        !seriesSelection.count(previous.row) ||
                        !seriesSelection.count(current.row)) continue;
                    const Point start = DataToScreen(
                        {previous.x, previous.y}, viewport, plotRect, true);
                    const Point end = DataToScreen(
                        {current.x, current.y}, viewport, plotRect, true);
                    timeSeriesSelectionCanvas_.Children().Append(Line(start.x, start.y,
                        end.x, end.y, Brush(color, 0.98), 3.0));
                }
            }
        }
        plotCanvas_.Children().Append(timeSeriesSelectionCanvas_);
        std::vector<ScatterplotPointValue> pointValues;
        if (!currentModel_->imputationDiagnosticSimplified)
        for (auto const& point : currentModel_->points)
            pointValues.push_back({point.row, point.x, point.y});
        const auto inputs = BuildScatterplotPointDrawInputs(pointValues, viewport, plotRect);
        std::map<int, std::string> timeSeriesPointColors = pointColors_;
        for (auto const& entry : TimeSeriesRowColors(*currentModel_))
            timeSeriesPointColors.emplace(entry.first, entry.second);
        auto points = BuildScatterplotPointDrawPlan(
            inputs, seriesSelection, timeSeriesPointColors, {}, "none");
        std::stable_sort(points.begin(), points.end(), [](auto const& a, auto const& b) {
            return static_cast<int>(a.selected) < static_cast<int>(b.selected);
        });
        for (auto const& point : points)
        {
            const auto explicitTone = point.selected
                ? PlotSelectedColorForNameOrHex(point.colorName)
                : PlotColorForNameOrHex(point.colorName);
            const auto fill = point.hasExplicitColor ? explicitTone
                : (point.selected ? theme.selectedMarkFill : theme.markFill);
            const auto stroke = point.selected ? theme.selectedMarkStroke
                : (point.hasExplicitColor ? explicitTone : theme.markStroke);
            const double radius = 3.0 * pointSizeScale_;
            if (point.selected)
                for (auto const& ring : CreateSelectionRingVisuals(
                         point.point, radius, theme, fill))
                    timeSeriesSelectionCanvas_.Children().Append(ring);
            auto dot = Shapes::Ellipse(); dot.Width(radius * 2.0); dot.Height(radius * 2.0);
            dot.Fill(Brush(::rlispstat::core::PlotMarkColor(fill, theme.panel, point.shadeOverlap, point.fillAlpha)));
            dot.Stroke(Brush(::rlispstat::core::PlotMarkColor(stroke, theme.panel, point.shadeOverlap, point.strokeAlpha)));
            dot.StrokeThickness(point.selected ? theme.selectedMarkStrokeWidth : 0.8);
            Controls::Canvas::SetLeft(dot, point.point.x - radius);
            Controls::Canvas::SetTop(dot, point.point.y - radius);
            Controls::Canvas::SetZIndex(dot, point.selected ? 4 : 3);
            plotCanvas_.Children().Append(dot);
            timeSeriesPointVisuals_[point.caseId].push_back(dot);
            renderedPoints_.push_back({point.caseId, point.point.x, point.point.y});
        }
        if (!currentModel_->timeSeriesGroupVariable.empty() &&
            currentModel_->timeSeriesIdentification == "legend")
        {
            const auto legendItems = BuildInteractionLegendLayout(*currentModel_, plotRect);
            std::vector<UIElement> legendVisuals;
            for (auto const& item : legendItems)
            {
                if (item.seriesIndex >= currentModel_->interactionPlotLines.size()) continue;
                auto const& series = currentModel_->interactionPlotLines[item.seriesIndex];
                auto color = PlotColorForNameOrHex(series.colorKey.empty() ? "black" : series.colorKey);
                const bool fullySelected = PlotSeriesIsFullySelected(
                    *currentModel_, item.seriesIndex, seriesSelection);
                auto sample = Line(item.sampleStart.x, item.sampleStart.y,
                    item.sampleEnd.x, item.sampleEnd.y,
                    Brush(color, hasSelection && !fullySelected ? 0.24 : 1.0),
                    fullySelected ? 3.0 : 2.0);
                const auto rows = PlotSeriesLegendRows(*currentModel_, item.seriesIndex);
                if (!rows.empty())
                {
                    sample.IsHitTestVisible(true);
                    sample.PointerPressed([this, sample, rows]
                        (auto const&, auto const& event)
                    {
                        if (event.GetCurrentPoint(sample).Properties()
                                .IsRightButtonPressed()) return;
                        if (!RowSelectionGestureEnabled()) return;
                        if (selectionCallback_ && !group_.empty())
                            selectionCallback_(group_, rows,
                                SelectionModeFor(event.KeyModifiers(),
                                    currentModel_ ? currentModel_->selectionMode
                                                  : "replace"));
                        event.Handled(true);
                    });
                }
                plotCanvas_.Children().Append(sample);
                legendVisuals.push_back(sample);
                auto label = Label(to_hstring(series.label).c_str(),
                    item.labelAnchor.x, item.labelAnchor.y - 8.0, 10.0,
                    Brush(hasSelection && !fullySelected ? theme.mutedText : theme.text));
                AttachBoxplotSelection(label, std::vector<int>(rows.begin(), rows.end()));
                plotCanvas_.Children().Append(label);
                legendVisuals.push_back(label);
                timeSeriesLegendVisuals_.push_back({item.seriesIndex, sample, label});
            }
            if (!legendItems.empty())
            {
                double legendLeft = std::numeric_limits<double>::infinity();
                double legendTop = std::numeric_limits<double>::infinity();
                double legendRight = -std::numeric_limits<double>::infinity();
                double legendBottom = -std::numeric_limits<double>::infinity();
                for (auto const& item : legendItems)
                {
                    auto const& series = currentModel_->interactionPlotLines[item.seriesIndex];
                    const double labelWidth = 6.2 * static_cast<double>(series.label.size());
                    legendLeft = std::min(legendLeft, item.sampleStart.x - 6.0);
                    legendRight = std::max(legendRight,
                        item.labelAnchor.x + labelWidth + 6.0);
                    legendTop = std::min(legendTop, item.sampleStart.y - 11.0);
                    legendBottom = std::max(legendBottom, item.sampleStart.y + 11.0);
                }
                const double legendWidth = std::max(1.0, legendRight - legendLeft);
                const double legendHeight = std::max(1.0, legendBottom - legendTop);
                auto dragSurface = Shapes::Rectangle();
                dragSurface.Width(legendWidth); dragSurface.Height(legendHeight);
                dragSurface.Fill(Brush(theme.panel, 0.001));
                Controls::Canvas::SetLeft(dragSurface, legendLeft);
                Controls::Canvas::SetTop(dragSurface, legendTop);
                Controls::Canvas::SetZIndex(dragSurface, 51);
                legendVisuals.push_back(dragSurface);
                dragSurface.PointerPressed(
                    [this, dragSurface, legendItems](auto const&, auto const& event)
                    {
                        if (!event.GetCurrentPoint(dragSurface).Properties()
                                .IsLeftButtonPressed()) return;
                        const auto point = event.GetCurrentPoint(plotCanvas_).Position();
                        interactionLegendDragging_ = true;
                        interactionLegendMoved_ = false;
                        interactionLegendAppliedDx_ = 0.0;
                        interactionLegendAppliedDy_ = 0.0;
                        interactionLegendDragStartX_ = point.X;
                        interactionLegendDragStartY_ = point.Y;
                        interactionLegendPressedRows_.clear();
                        interactionLegendSelectionMode_ = SelectionModeFor(
                            event.KeyModifiers(), currentModel_
                                ? currentModel_->selectionMode : "replace");
                        if (currentModel_)
                            for (auto const& item : legendItems)
                            {
                                if (item.seriesIndex >=
                                    currentModel_->interactionPlotLines.size()) continue;
                                auto const& series =
                                    currentModel_->interactionPlotLines[item.seriesIndex];
                                const double labelWidth = 6.2 *
                                    static_cast<double>(series.label.size());
                                const double left = item.sampleStart.x - 6.0;
                                const double right = item.labelAnchor.x + labelWidth + 6.0;
                                if (point.X >= left && point.X <= right &&
                                    point.Y >= item.sampleStart.y - 11.0 &&
                                    point.Y <= item.sampleStart.y + 11.0)
                                {
                                    interactionLegendPressedRows_ =
                                        ::rlispstat::core::PlotSeriesLegendRows(
                                            *currentModel_, item.seriesIndex);
                                    break;
                                }
                            }
                        dragSurface.CapturePointer(event.Pointer());
                        event.Handled(true);
                    });
                dragSurface.PointerMoved(
                    [this, legendVisuals, plotRect, legendLeft, legendTop,
                     legendWidth, legendHeight](auto const&, auto const& event)
                    {
                        if (!interactionLegendDragging_) return;
                        const auto point = event.GetCurrentPoint(plotCanvas_).Position();
                        const double dx = point.X - interactionLegendDragStartX_;
                        const double dy = point.Y - interactionLegendDragStartY_;
                        if (std::hypot(dx, dy) > 3.0) interactionLegendMoved_ = true;
                        if (!interactionLegendMoved_) return;
                        const double nextLeft = std::clamp(legendLeft + dx,
                            plotRect.x,
                            std::max(plotRect.x,
                                plotRect.x + plotRect.width - legendWidth));
                        const double nextTop = std::clamp(legendTop + dy,
                            plotRect.y,
                            std::max(plotRect.y,
                                plotRect.y + plotRect.height - legendHeight));
                        interactionLegendAppliedDx_ = nextLeft - legendLeft;
                        interactionLegendAppliedDy_ = nextTop - legendTop;
                        for (auto const& visual : legendVisuals)
                        {
                            auto transform = Media::TranslateTransform();
                            transform.X(interactionLegendAppliedDx_);
                            transform.Y(interactionLegendAppliedDy_);
                            visual.RenderTransform(transform);
                        }
                        event.Handled(true);
                    });
                dragSurface.PointerReleased(
                    [this, dragSurface, legendItems, plotRect]
                    (auto const&, auto const& event)
                    {
                        if (!interactionLegendDragging_) return;
                        interactionLegendDragging_ = false;
                        dragSurface.ReleasePointerCapture(event.Pointer());
                        if (interactionLegendMoved_ && currentModel_ &&
                            !legendItems.empty())
                        {
                            const auto anchor = legendItems.front().sampleStart;
                            ::rlispstat::core::SetInteractionLegendCustomPosition(
                                *currentModel_,
                                (anchor.x + interactionLegendAppliedDx_ - plotRect.x) /
                                    std::max(1.0, plotRect.width),
                                (anchor.y + interactionLegendAppliedDy_ - plotRect.y) /
                                    std::max(1.0, plotRect.height));
                            ConfigureContextMenu();
                        }
                        else if (currentModel_ && PlotIsImputationDiagnostic(*currentModel_))
                        {
                            for (auto const& item : legendItems)
                            {
                                if (std::abs(interactionLegendDragStartY_ - item.sampleStart.y) > 10.0) continue;
                                SelectDiagnosticPlotSeries(*currentModel_, item.seriesIndex,
                                    interactionLegendSelectionMode_ != SelectionMode::Replace);
                                RenderPlot(std::max(360.0, plotCanvas_.ActualWidth()),
                                           std::max(280.0, plotCanvas_.ActualHeight()));
                                break;
                            }
                        }
                        else if (RowSelectionGestureEnabled() && selectionCallback_ &&
                                 !group_.empty() &&
                                 !interactionLegendPressedRows_.empty())
                        {
                            selectionCallback_(group_, interactionLegendPressedRows_,
                                               interactionLegendSelectionMode_);
                        }
                        interactionLegendPressedRows_.clear();
                        event.Handled(true);
                    });
                // pointsCanvas is the transparent input layer above
                // plotCanvas.  Keeping the legend hit surface on the lower
                // canvas meant the input layer intercepted every click before
                // the legend could select its series.  The visible legend
                // remains on plotCanvas; only its interaction surface belongs
                // on the top input layer.
                pointsCanvas_.Children().Append(dragSurface);
            }
        }
        else if (!currentModel_->timeSeriesGroupVariable.empty() &&
                 currentModel_->timeSeriesIdentification == "start_labels")
        {
            for (auto const& item : BuildTimeSeriesDirectLabels(
                     currentModel_->interactionPlotLines, viewport, plotRect))
            {
                auto color = PlotColorForNameOrHex(item.colorKey.empty() ? "black" : item.colorKey);
                plotCanvas_.Children().Append(Line(item.seriesAnchor.x, item.seriesAnchor.y,
                    item.leaderEnd.x, item.leaderEnd.y, Brush(color, 0.72), 1.0));
                plotCanvas_.Children().Append(Label(to_hstring(item.label).c_str(),
                    item.labelAnchor.x - 6.2 * static_cast<double>(item.label.size()), item.labelAnchor.y - 7.0, 10.0, Brush(color)));
            }
        }
        auto title = Label(to_hstring(currentModel_->title.empty() ? "Time series" : currentModel_->title).c_str(),
            plotRect.x, 24.0, 16.0, text);
        title.Width(plotRect.width); title.TextAlignment(TextAlignment::Center);
        title.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        plotCanvas_.Children().Append(title);
        xAxisLabel_ = Label(to_hstring(currentModel_->xLabel).c_str(), plotRect.x,
            plotRect.y + plotRect.height + 36.0, 12.0, text);
        xAxisLabel_.Width(plotRect.width); xAxisLabel_.TextAlignment(TextAlignment::Center);
        plotCanvas_.Children().Append(xAxisLabel_);
        yAxisLabel_ = Label(to_hstring(currentModel_->yLabel).c_str(), 16.0,
            plotRect.y + plotRect.height / 2.0 + 20.0, 12.0, text);
        auto rotation = Media::RotateTransform(); rotation.Angle(-90.0);
        yAxisLabel_.RenderTransform(rotation); plotCanvas_.Children().Append(yAxisLabel_);
        AttachAxisVariableMenu(xAxisLabel_, true);
        AttachAxisVariableMenu(yAxisLabel_, false);
        pointsCanvas_.Width(plotCanvas_.ActualWidth()); pointsCanvas_.Height(plotCanvas_.ActualHeight());
        selectionRectangle_.Visibility(Visibility::Collapsed);
        selectionRectangle_.Stroke(Brush(theme.accent));
        selectionRectangle_.Fill(Brush(theme.accent, 0.16));
        pointsCanvas_.Children().Append(selectionRectangle_);
        plotCanvas_.Children().Append(pointsCanvas_);
        Controls::Canvas::SetZIndex(pointsCanvas_, 10);
        AppendPanelFrame(plotCanvas_, plotRect, theme);
    }

    void ScatterPlotView::RefreshTimeSeriesSelectionVisuals(
        std::set<int> const& rows)
    {
        ::rlispstat::windows::performance::Scope timing(
            "Plot.RefreshTimeSeriesSelectionVisuals",
            "plot=" + plotId_ + ",changed=" + std::to_string(rows.size()));
        if (!currentModel_ || !timeSeriesSelectionCanvas_) return;
        if (timeSeriesLineVisuals_.size() !=
            currentModel_->interactionPlotLines.size())
        {
            RenderPlot(std::max(360.0, plotCanvas_.ActualWidth()),
                       std::max(280.0, plotCanvas_.ActualHeight()));
            return;
        }
        using namespace ::rlispstat::core;
        const auto theme = PlotThemeStyleForName(themeName_);
        const bool diagnostic = PlotIsImputationDiagnostic(*currentModel_);
        const auto seriesSelection = diagnostic
            ? DiagnosticPlotSelectedPointIds(*currentModel_) : selectedRows_;
        const bool hasSelection = !seriesSelection.empty();
        timeSeriesSelectionCanvas_.Children().Clear();
        for (std::size_t lineIndex = 0;
             lineIndex < currentModel_->interactionPlotLines.size(); ++lineIndex)
        {
            const auto& series = currentModel_->interactionPlotLines[lineIndex];
            const auto color = PlotColorForNameOrHex(
                series.colorKey.empty() ? "black" : series.colorKey);
            const bool fullySelected = PlotSeriesIsFullySelected(
                *currentModel_, lineIndex, seriesSelection);
            timeSeriesLineVisuals_[lineIndex].Stroke(
                Brush(color, hasSelection ? 0.20 : 0.78));
            timeSeriesLineVisuals_[lineIndex].StrokeThickness(
                fullySelected ? 3.0 : 1.6);
            if (!hasSelection) continue;
            for (std::size_t pointIndex = 1;
                 pointIndex < series.points.size(); ++pointIndex)
            {
                const auto& previous = series.points[pointIndex - 1];
                const auto& current = series.points[pointIndex];
                if (previous.row <= 0 || current.row <= 0 ||
                    !seriesSelection.count(previous.row) ||
                    !seriesSelection.count(current.row)) continue;
                const auto start = DataToScreen({previous.x, previous.y},
                    diagnosticSeriesViewport_, diagnosticSeriesRect_, true);
                const auto end = DataToScreen({current.x, current.y},
                    diagnosticSeriesViewport_, diagnosticSeriesRect_, true);
                timeSeriesSelectionCanvas_.Children().Append(Line(
                    start.x, start.y, end.x, end.y, Brush(color, 0.98), 3.0));
            }
        }
        std::vector<ScatterplotPointValue> pointValues;
        if (!currentModel_->imputationDiagnosticSimplified)
            for (const auto& point : currentModel_->points)
                pointValues.push_back({point.row, point.x, point.y});
        const auto inputs = BuildScatterplotPointDrawInputs(pointValues,
            diagnosticSeriesViewport_, diagnosticSeriesRect_);
        auto effectiveColors = pointColors_;
        for (const auto& entry : TimeSeriesRowColors(*currentModel_))
            effectiveColors.emplace(entry.first, entry.second);
        auto points = BuildScatterplotPointDrawPlan(
            inputs, seriesSelection, effectiveColors, {}, "none");
        std::stable_sort(points.begin(), points.end(), [](const auto& a, const auto& b) {
            return static_cast<int>(a.selected) < static_cast<int>(b.selected);
        });
        std::map<int, std::size_t> nextVisual;
        for (const auto& point : points)
        {
            const auto explicitTone = point.selected
                ? PlotSelectedColorForNameOrHex(point.colorName)
                : PlotColorForNameOrHex(point.colorName);
            const auto fill = point.hasExplicitColor ? explicitTone
                : (point.selected ? theme.selectedMarkFill : theme.markFill);
            const auto stroke = point.selected ? theme.selectedMarkStroke
                : (point.hasExplicitColor ? explicitTone : theme.markStroke);
            const double radius = 3.0 * pointSizeScale_;
            if (point.selected)
                for (const auto& ring : CreateSelectionRingVisuals(
                         point.point, radius, theme, fill))
                    timeSeriesSelectionCanvas_.Children().Append(ring);
            if (point.caseId > 0 && !rows.count(point.caseId) &&
                !diagnostic) continue;
            auto found = timeSeriesPointVisuals_.find(point.caseId);
            const std::size_t index = nextVisual[point.caseId]++;
            if (found == timeSeriesPointVisuals_.end() ||
                index >= found->second.size())
            {
                RenderPlot(std::max(360.0, plotCanvas_.ActualWidth()),
                           std::max(280.0, plotCanvas_.ActualHeight()));
                return;
            }
            auto& dot = found->second[index];
            dot.Fill(Brush(PlotMarkColor(
                fill, theme.panel, point.shadeOverlap, point.fillAlpha)));
            dot.Stroke(Brush(PlotMarkColor(
                stroke, theme.panel, point.shadeOverlap, point.strokeAlpha)));
            dot.StrokeThickness(point.selected
                ? theme.selectedMarkStrokeWidth : 0.8);
            Controls::Canvas::SetZIndex(dot, point.selected ? 4 : 3);
        }
        for (const auto& visual : timeSeriesLegendVisuals_)
        {
            if (visual.seriesIndex >= currentModel_->interactionPlotLines.size()) continue;
            const auto& series = currentModel_->interactionPlotLines[visual.seriesIndex];
            const auto color = PlotColorForNameOrHex(
                series.colorKey.empty() ? "black" : series.colorKey);
            const bool fullySelected = PlotSeriesIsFullySelected(
                *currentModel_, visual.seriesIndex, seriesSelection);
            visual.sample.Stroke(Brush(color,
                hasSelection && !fullySelected ? 0.24 : 1.0));
            visual.sample.StrokeThickness(fullySelected ? 3.0 : 2.0);
            visual.label.Foreground(Brush(
                hasSelection && !fullySelected ? theme.mutedText : theme.text));
        }
    }

    void ScatterPlotView::RenderTrellis(
        double width,
        double height,
        ::rlispstat::core::PlotThemeStyleSpec const& theme)
    {
        using namespace ::rlispstat::core;
        plotCanvas_.Children().Clear(); pointsCanvas_.Children().Clear();
        trellisPointVisuals_.clear(); trellisRingPaths_.clear();
        trellisBoxplotGeometry_.clear();
        trellisTimeSeriesLineVisuals_.clear();
        trellisSelectionCanvas_ = Controls::Canvas();
        trellisSelectionCanvas_.Width(width);
        trellisSelectionCanvas_.Height(height);
        trellisSelectionCanvas_.IsHitTestVisible(false);
        Controls::Canvas::SetZIndex(trellisSelectionCanvas_, 2);
        renderedPoints_.clear();
        plotCanvas_.Background(Brush(theme.background));
        trellisLayout_ = BuildTrellisScatterplotLayout(
            *currentModel_, {0.0, 0.0, width, height});
        const auto plans = BuildTrellisPanelRenderPlans(
            *currentModel_, trellisLayout_, selectedRows_, pointColors_, rowLabels_);
        auto text = Brush(theme.text);
        auto muted = Brush(theme.mutedText);
        auto title = Label(to_hstring(currentModel_->title.empty() ? "Trellis plot" : currentModel_->title).c_str(),
            trellisLayout_.titleRect.x, trellisLayout_.titleRect.y, 15.0, text);
        title.Width(trellisLayout_.titleRect.width); title.TextAlignment(TextAlignment::Center);
        title.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        plotCanvas_.Children().Append(title);
        const auto type = currentModel_->trellisSpecificationInitialized
            ? currentModel_->trellisSpecification.plotType : TrellisPlotType::Scatter;
        std::vector<std::size_t> rowConditionIndices;
        std::vector<std::size_t> columnConditionIndices;
        if (currentModel_->trellisSpecificationInitialized)
        {
            auto const& conditions = currentModel_->trellisSpecification.conditioningVariables;
            for (std::size_t index = 0; index < conditions.size(); ++index)
            {
                if (conditions[index].dimension == TrellisDimension::Rows)
                    rowConditionIndices.push_back(index);
                else
                    columnConditionIndices.push_back(index);
            }
        }
        for (std::size_t panelIndex = 0; panelIndex < trellisLayout_.panels.size(); ++panelIndex)
        {
            auto const& panel = trellisLayout_.panels[panelIndex];
            trellisTimeSeriesLineVisuals_.emplace_back();
            plotCanvas_.Children().Append(Rectangle(panel.plotRect, Brush(theme.panel),
                TransparentBrush(), 0.0));
            if (IsValidRect(panel.headerRect))
            {
                auto headerBackground = Rectangle(panel.headerRect,
                    Brush(theme.majorGrid, 0.24), TransparentBrush(), 0.0);
                AttachTrellisConditionMenu(headerBackground, columnConditionIndices);
                plotCanvas_.Children().Append(headerBackground);
                auto header = Label(to_hstring(panel.label).c_str(), panel.headerRect.x + 5.0,
                    panel.headerRect.y + 2.0, 10.0, text);
                header.Width(std::max(10.0, panel.headerRect.width - 10.0));
                header.TextAlignment(TextAlignment::Center);
                header.TextTrimming(TextTrimming::CharacterEllipsis);
                AttachTrellisConditionMenu(header, columnConditionIndices);
                plotCanvas_.Children().Append(header);
            }
            if (!panel.rowStripLabel.empty() && IsValidRect(panel.rowStripRect))
            {
                auto rowBackground = Rectangle(panel.rowStripRect,
                    Brush(theme.majorGrid, 0.24), TransparentBrush(), 0.0);
                AttachTrellisConditionMenu(rowBackground, rowConditionIndices);
                plotCanvas_.Children().Append(rowBackground);
                auto rowLabel = Label(to_hstring(panel.rowStripLabel).c_str(),
                    panel.rowStripRect.x, panel.rowStripRect.y + panel.rowStripRect.height / 2.0,
                    9.5, text);
                auto rotation = Media::RotateTransform(); rotation.Angle(-90.0);
                rowLabel.RenderTransform(rotation);
                AttachTrellisConditionMenu(rowLabel, rowConditionIndices);
                plotCanvas_.Children().Append(rowLabel);
            }
            for (std::size_t tickIndex = 0; tickIndex < panel.xTicks.size(); ++tickIndex)
            {
                auto const& tick = panel.xTicks[tickIndex];
                plotCanvas_.Children().Append(Line(tick.position, panel.plotRect.y,
                    tick.position, panel.plotRect.y + panel.plotRect.height,
                    Brush(theme.majorGrid), 0.7));
                if (panel.showXTickLabels)
                {
                    auto label = Label(to_hstring(tick.label).c_str(), tick.position - 30.0,
                        panel.plotRect.y + panel.plotRect.height + 4.0, 9.0, muted);
                    label.Width(60.0); label.TextAlignment(TextAlignment::Center);
                    plotCanvas_.Children().Append(label);
                    if (type == TrellisPlotType::Boxplot && panelIndex < plans.size())
                    {
                        auto const& plan = plans[panelIndex];
                        if (tickIndex < plan.boxplotLayout.categories.size())
                        {
                            std::set<std::string> matchingCategories;
                            std::vector<int> rows;
                            auto const& category = plan.boxplotLayout.categories[tickIndex];
                            if (!plan.boxplotLayout.categoryLevels.empty() &&
                                !plan.boxplotLayout.categoryLevels.front().empty())
                                matchingCategories = BoxplotCategoriesForLevelValue(
                                    plan.boxplotLayout,
                                    plan.boxplotLayout.categoryLevels.front().size() - 1,
                                    tick.label);
                            if (matchingCategories.empty()) matchingCategories.insert(category);
                            for (auto const& otherPlan : plans)
                                for (auto const& point : otherPlan.boxplotCases)
                                    if (matchingCategories.count(point.category))
                                        rows.push_back(point.caseId);
                            AttachBoxplotSelection(label, std::move(rows));
                        }
                    }
                }
            }
            if (type == TrellisPlotType::Boxplot && panel.showXTickLabels &&
                panelIndex < plans.size())
            {
                auto const& nestedLayout = plans[panelIndex].boxplotLayout;
                std::size_t depth = 0;
                for (auto const& levels : nestedLayout.categoryLevels)
                    depth = std::max(depth, levels.size());
                if (depth > 1)
                {
                    const double innerY = panel.plotRect.y + panel.plotRect.height + 24.0;
                    for (auto const& category : nestedLayout.categories)
                    {
                        const double x = BoxplotCategoryCenter(nestedLayout, category);
                        plotCanvas_.Children().Append(Line(x,
                            panel.plotRect.y + panel.plotRect.height + 17.0,
                            x, innerY, Brush(theme.axis, 0.68), 0.95));
                    }
                }
                for (auto const& span : BoxplotGroupSpans(nestedLayout))
                {
                    const double y = panel.plotRect.y + panel.plotRect.height + 24.0 +
                        19.0 * static_cast<double>(depth - 2 - span.level);
                    plotCanvas_.Children().Append(Line(span.startX, y, span.endX, y,
                        Brush(theme.axis, 0.78), 1.1));
                    if (span.level > 0)
                        plotCanvas_.Children().Append(Line(span.centerX, y,
                            span.centerX, y + 19.0, Brush(theme.axis, 0.68), 0.95));
                    auto label = Label(to_hstring(span.label).c_str(),
                        span.centerX - 45.0, y, 8.5, text);
                    label.Width(90.0); label.TextAlignment(TextAlignment::Center);
                    plotCanvas_.Children().Append(label);
                    const auto covered = BoxplotCategoriesForLevelValue(
                        nestedLayout, span.level, span.label);
                    std::vector<int> rows;
                    for (auto const& otherPlan : plans)
                        for (auto const& point : otherPlan.boxplotCases)
                            if (covered.count(point.category)) rows.push_back(point.caseId);
                    AttachBoxplotSelection(label, std::move(rows));
                }
            }
            for (auto const& tick : panel.yTicks)
            {
                plotCanvas_.Children().Append(Line(panel.plotRect.x, tick.position,
                    panel.plotRect.x + panel.plotRect.width, tick.position,
                    Brush(theme.majorGrid), 0.7));
                if (panel.showYTickLabels)
                {
                    const bool yAxisOnRight = currentModel_->trellisRowStripsOnLeft;
                    auto label = Label(to_hstring(tick.label).c_str(),
                        yAxisOnRight
                            ? panel.plotRect.x + panel.plotRect.width + 5.0
                            : panel.plotRect.x - 45.0,
                        tick.position - 7.0, 9.0, muted);
                    label.Width(40.0);
                    label.TextAlignment(yAxisOnRight
                        ? TextAlignment::Left : TextAlignment::Right);
                    plotCanvas_.Children().Append(label);
                }
            }
            plotCanvas_.Children().Append(Line(panel.plotRect.x,
                panel.plotRect.y + panel.plotRect.height,
                panel.plotRect.x + panel.plotRect.width,
                panel.plotRect.y + panel.plotRect.height, Brush(theme.axis), 1.0));
            const double yAxisX = currentModel_->trellisRowStripsOnLeft
                ? panel.plotRect.x + panel.plotRect.width : panel.plotRect.x;
            plotCanvas_.Children().Append(Line(yAxisX, panel.plotRect.y,
                yAxisX, panel.plotRect.y + panel.plotRect.height,
                Brush(theme.axis), 1.0));
            if (!panel.hasObservations)
            {
                auto empty = Label(L"No observations", panel.plotRect.x,
                    panel.plotRect.y + panel.plotRect.height / 2.0 - 8.0, 10.0, muted);
                empty.Width(panel.plotRect.width); empty.TextAlignment(TextAlignment::Center);
                plotCanvas_.Children().Append(empty);
            }
            if (panelIndex >= plans.size()) continue;
            auto const& plan = plans[panelIndex];
            if (type == TrellisPlotType::Scatter || type == TrellisPlotType::TimeSeries)
            {
                if (type == TrellisPlotType::TimeSeries)
                {
                    for (auto const& series : plan.timeSeriesLines)
                    {
                        const bool fullySelected = !selectedRows_.empty() &&
                            !series.caseIds.empty() &&
                            std::all_of(series.caseIds.begin(), series.caseIds.end(),
                                [&](int row) { return selectedRows_.count(row) != 0; });
                        const double alpha = selectedRows_.empty() ? 0.96 : 0.20;
                        auto color = currentModel_->trellisSpecification.groupingVariableId.empty()
                            ? theme.geomStroke : PlotColorForNameOrHex(series.colorKey);
                        auto line = Polyline(series.points,
                            Brush(color, alpha), fullySelected ? 3.0 : 1.6);
                        plotCanvas_.Children().Append(line);
                        trellisTimeSeriesLineVisuals_.back().push_back(line);
                        if (!selectedRows_.empty() &&
                            series.points.size() == series.caseIds.size())
                        {
                            for (std::size_t pointIndex = 1;
                                 pointIndex < series.points.size(); ++pointIndex)
                            {
                                if (!selectedRows_.count(series.caseIds[pointIndex - 1]) ||
                                    !selectedRows_.count(series.caseIds[pointIndex])) continue;
                                auto const& start = series.points[pointIndex - 1];
                                auto const& end = series.points[pointIndex];
                                trellisSelectionCanvas_.Children().Append(Line(start.x, start.y,
                                    end.x, end.y, Brush(color, 0.98), 3.0));
                            }
                        }
                    }
                }
                for (auto const& curve : plan.scatterplot.smoothCurves)
                {
                    if (curve.confidencePolygon.size() < 3) continue;
                    const auto color = curve.colorName.empty()
                        ? theme.smooth : PlotColorForNameOrHex(curve.colorName);
                    plotCanvas_.Children().Append(Polygon(curve.confidencePolygon,
                        Brush(color, curve.confidenceAlpha)));
                }
                for (auto const& overlay : plan.scatterplot.overlayLines)
                {
                    auto color = overlay.useDefaultDarkColor ? theme.geomStroke
                        : PlotColorForNameOrHex(overlay.colorName);
                    plotCanvas_.Children().Append(Line(overlay.start.x, overlay.start.y,
                        overlay.end.x, overlay.end.y, Brush(color, overlay.alpha), overlay.lineWidth));
                }
                for (auto const& curve : plan.scatterplot.smoothCurves)
                {
                    const auto color = curve.colorName.empty()
                        ? theme.smooth : PlotColorForNameOrHex(curve.colorName);
                    plotCanvas_.Children().Append(Polyline(curve.points,
                        Brush(color, curve.alpha), curve.lineWidth));
                }
                for (auto const& point : plan.scatterplot.points)
                {
                    const auto explicitTone = point.selected
                        ? PlotSelectedColorForNameOrHex(point.colorName)
                        : PlotColorForNameOrHex(point.colorName);
                    const auto fill = point.hasExplicitColor ? explicitTone
                        : (point.selected ? theme.selectedMarkFill : theme.markFill);
                    const auto stroke = point.selected ? theme.selectedMarkStroke
                        : (point.hasExplicitColor ? explicitTone : theme.markStroke);
                    const double radius = point.radius *
                        pointSizeScale_;
                    if (point.selected && type != TrellisPlotType::Scatter &&
                        type != TrellisPlotType::TimeSeries)
                        for (auto const& ring : CreateSelectionRingVisuals(
                                 point.point, radius, theme, fill))
                            plotCanvas_.Children().Append(ring);
                    auto dot = Shapes::Ellipse(); dot.Width(radius * 2.0); dot.Height(radius * 2.0);
                    dot.Fill(Brush(::rlispstat::core::PlotMarkColor(fill, theme.panel, point.shadeOverlap, point.fillAlpha)));
                    dot.Stroke(Brush(::rlispstat::core::PlotMarkColor(stroke, theme.panel, point.shadeOverlap, point.strokeAlpha)));
                    dot.StrokeThickness(point.selected ? theme.selectedMarkStrokeWidth : point.strokeWidth);
                    Controls::Canvas::SetLeft(dot, point.point.x - radius);
                    Controls::Canvas::SetTop(dot, point.point.y - radius);
                    plotCanvas_.Children().Append(dot);
                    if (type == TrellisPlotType::Scatter ||
                        type == TrellisPlotType::TimeSeries)
                        Controls::Canvas::SetZIndex(dot, point.selected ? 4 : 3);
                    Controls::TextBlock pointLabel{ nullptr };
                    if (point.showLabel)
                    {
                        pointLabel = Label(to_hstring(point.label).c_str(),
                            point.labelAnchor.x + 6.0, point.labelAnchor.y - 18.0,
                            9.0, text);
                        pointLabel.IsHitTestVisible(false);
                        Controls::Canvas::SetZIndex(pointLabel, point.selected ? 31 : 21);
                        plotCanvas_.Children().Append(pointLabel);
                    }
                    if ((type == TrellisPlotType::Scatter ||
                         type == TrellisPlotType::TimeSeries) && point.caseId > 0)
                        trellisPointVisuals_[point.caseId].push_back({ dot, pointLabel });
                    renderedPoints_.push_back({point.caseId, point.point.x, point.point.y});
                }
                for (auto const& item : plan.timeSeriesLegend)
                {
                    if (item.seriesIndex >= plan.timeSeriesLines.size()) continue;
                    auto const& series = plan.timeSeriesLines[item.seriesIndex];
                    auto color = PlotColorForNameOrHex(series.colorKey);
                    auto sample = Line(item.sampleStart.x, item.sampleStart.y,
                        item.sampleEnd.x, item.sampleEnd.y, Brush(color), 2.0);
                    if (!series.caseIds.empty())
                    {
                        const std::set<int> rows(
                            series.caseIds.begin(), series.caseIds.end());
                        sample.IsHitTestVisible(true);
                        sample.PointerPressed([this, sample, rows]
                            (auto const&, auto const& event)
                        {
                            if (event.GetCurrentPoint(sample).Properties()
                                    .IsRightButtonPressed()) return;
                            if (!RowSelectionGestureEnabled()) return;
                            if (selectionCallback_ && !group_.empty())
                                selectionCallback_(group_, rows,
                                    SelectionModeFor(event.KeyModifiers(),
                                        currentModel_ ? currentModel_->selectionMode
                                                      : "replace"));
                            event.Handled(true);
                        });
                    }
                    plotCanvas_.Children().Append(sample);
                    auto label = Label(to_hstring(series.label).c_str(),
                        item.labelAnchor.x, item.labelAnchor.y - 7.0, 9.0, Brush(color));
                    AttachBoxplotSelection(label, series.caseIds);
                    plotCanvas_.Children().Append(label);
                }
            }
            else if (type == TrellisPlotType::Histogram)
            {
                for (auto const& bar : plan.histogram.bars)
                {
                    plotCanvas_.Children().Append(Rectangle(bar.rect, Brush(theme.geomFill),
                        TransparentBrush(), 0.0));
                    for (auto const& segment : bar.colorSegments)
                    {
                        auto color = segment.colorName.empty()
                            ? theme.geomFill : LightPaletteColor(segment.colorName);
                        plotCanvas_.Children().Append(Rectangle(segment.rect,
                            Brush(color, segment.alpha), TransparentBrush(), 0.0));
                    }
                    for (auto const& segment : bar.selectedSegments)
                        plotCanvas_.Children().Append(Rectangle(segment.rect,
                            Brush(segment.defaultSelection ? theme.selectedMarkFill
                                : PlotSelectedColorForNameOrHex(segment.colorName), segment.alpha),
                            TransparentBrush(), 0.0));
                    AppendHistogramBinOutline(plotCanvas_, bar.rect);
                }
            }
            else if (type == TrellisPlotType::Boxplot)
            {
                trellisBoxplotGeometry_.insert(trellisBoxplotGeometry_.end(),
                    plan.boxplotCases.begin(), plan.boxplotCases.end());
                for (auto const& item : plan.boxplotStats)
                {
                    plotCanvas_.Children().Append(Line(item.upperWhiskerStart.x, item.upperWhiskerStart.y,
                        item.upperWhiskerEnd.x, item.upperWhiskerEnd.y, Brush(theme.geomStroke), 1.0));
                    plotCanvas_.Children().Append(Line(item.lowerWhiskerStart.x, item.lowerWhiskerStart.y,
                        item.lowerWhiskerEnd.x, item.lowerWhiskerEnd.y, Brush(theme.geomStroke), 1.0));
                    plotCanvas_.Children().Append(Rectangle(item.boxRect, Brush(theme.geomFill),
                        Brush(theme.geomStroke), 1.4));
                    plotCanvas_.Children().Append(Line(item.medianStart.x, item.medianStart.y,
                        item.medianEnd.x, item.medianEnd.y, Brush(theme.accent), 1.5));
                }
                for (auto const& point : plan.boxplotPoints)
                {
                    auto found = pointColors_.find(point.caseId);
                    const bool explicitColor = found != pointColors_.end();
                    const auto fill = explicitColor
                        ? (point.selected
                            ? PlotSelectedColorForNameOrHex(found->second)
                            : PlotColorForNameOrHex(found->second))
                        : (point.selected ? theme.selectedMarkFill : theme.markFill);
                    const auto stroke = point.selected ? theme.selectedMarkStroke
                        : (explicitColor ? fill : theme.markStroke);
                    const double alpha = point.selected ? 1.0
                        : (point.dimmed ? 0.24 : 0.65);
                    const double radius = 2.8 * pointSizeScale_;
                    if (point.selected)
                        for (const auto& ring : CreateSelectionRingVisuals(
                                 point.point, radius, theme, fill))
                            trellisSelectionCanvas_.Children().Append(ring);
                    auto dot = Shapes::Ellipse(); dot.Width(radius * 2); dot.Height(radius * 2);
                    dot.Fill(Brush(fill, alpha));
                    dot.Stroke(Brush(stroke, point.selected ? 1.0
                        : (point.dimmed ? 0.18 : 0.48)));
                    dot.StrokeThickness(point.selected ? theme.selectedMarkStrokeWidth : 0.7);
                    Controls::Canvas::SetLeft(dot, point.point.x - radius);
                    Controls::Canvas::SetTop(dot, point.point.y - radius);
                    Controls::Canvas::SetZIndex(dot, point.selected ? 4 : 3);
                    plotCanvas_.Children().Append(dot);
                    if (point.caseId > 0)
                        trellisPointVisuals_[point.caseId].push_back({dot, nullptr});
                }
            }
            else if (type == TrellisPlotType::Bar)
            {
                const auto rects = BarplotBarRects(plan.barplotLayout);
                for (std::size_t index = 0; index < rects.size() && index < plan.barplotBins.size(); ++index)
                {
                    plotCanvas_.Children().Append(Rectangle(rects[index], Brush(theme.panel),
                        Brush(theme.geomStroke), 0.8));
                    if (index < plan.barplotSegments.size())
                        for (auto const& segment : plan.barplotSegments[index])
                        {
                            auto color = segment.style.colorKey.empty() ? theme.geomFill
                                : PlotColorForNameOrHex(segment.style.colorKey);
                            plotCanvas_.Children().Append(Rectangle(segment.rect,
                                Brush(color, segment.style.alpha), Brush(theme.geomStroke), 0.8));
                        }
                }
            }
        }
        std::vector<std::string> trellisBoxplotGroups;
        if (type == TrellisPlotType::Boxplot)
            trellisBoxplotGroups = currentModel_->trellisSpecification.boxplotGroupingVariableIds;
        if (!trellisBoxplotGroups.empty())
        {
            double totalWidth = 0.0;
            for (auto const& group : trellisBoxplotGroups)
                totalWidth += std::max(42.0, 7.0 * static_cast<double>(group.size()) + 16.0);
            totalWidth += 12.0 * static_cast<double>(trellisBoxplotGroups.size() - 1);
            double x = trellisLayout_.xAxisLabelAnchor.x - totalWidth / 2.0;
            for (std::size_t index = 0; index < trellisBoxplotGroups.size(); ++index)
            {
                auto const& group = trellisBoxplotGroups[index];
                const double labelWidth = std::max(42.0,
                    7.0 * static_cast<double>(group.size()) + 16.0);
                auto label = Label(to_hstring(group).c_str(), x,
                    trellisLayout_.xAxisLabelAnchor.y, 10.5, Brush(theme.accent));
                label.Width(labelWidth); label.TextAlignment(TextAlignment::Center);
                plotCanvas_.Children().Append(label);
                AttachBoxplotGroupingVariableMenu(label, group, true);
                if (index == 0) xAxisLabel_ = label;
                x += labelWidth;
                if (index + 1 < trellisBoxplotGroups.size())
                {
                    auto separator = Label(L"›", x,
                        trellisLayout_.xAxisLabelAnchor.y, 10.5, muted);
                    separator.Width(12.0); separator.TextAlignment(TextAlignment::Center);
                    plotCanvas_.Children().Append(separator); x += 12.0;
                }
            }
        }
        else
        {
            xAxisLabel_ = Label(to_hstring(currentModel_->xLabel).c_str(),
                trellisLayout_.xAxisLabelAnchor.x - 120.0,
                trellisLayout_.xAxisLabelAnchor.y, 11.0, text);
            xAxisLabel_.Width(240.0); xAxisLabel_.TextAlignment(TextAlignment::Center);
            plotCanvas_.Children().Append(xAxisLabel_);
        }
        yAxisLabel_ = Label(to_hstring(currentModel_->yLabel).c_str(),
            trellisLayout_.yAxisLabelAnchor.x,
            trellisLayout_.yAxisLabelAnchor.y + 60.0, 11.0, text);
        auto rotation = Media::RotateTransform(); rotation.Angle(-90.0);
        yAxisLabel_.RenderTransform(rotation); plotCanvas_.Children().Append(yAxisLabel_);
        if (trellisBoxplotGroups.empty()) AttachAxisVariableMenu(xAxisLabel_, true);
        AttachAxisVariableMenu(yAxisLabel_, false);
        pointsCanvas_.Width(width); pointsCanvas_.Height(height);
        selectionRectangle_.Visibility(Visibility::Collapsed);
        selectionRectangle_.Stroke(Brush(theme.accent));
        selectionRectangle_.Fill(Brush(theme.accent, 0.16));
        pointsCanvas_.Children().Append(selectionRectangle_);
        plotCanvas_.Children().Append(pointsCanvas_);
        Controls::Canvas::SetZIndex(pointsCanvas_, 10);
        if (type == TrellisPlotType::Scatter || type == TrellisPlotType::TimeSeries)
            RefreshTrellisScatterRings(plans, theme);
        if (type == TrellisPlotType::TimeSeries || type == TrellisPlotType::Boxplot)
            plotCanvas_.Children().Append(trellisSelectionCanvas_);
        for (auto const& panel : trellisLayout_.panels)
            AppendPanelFrame(plotCanvas_, panel.plotRect, theme);
    }

    void ScatterPlotView::RefreshTrellisBoxplotSelectionVisuals(
        std::set<int> const& rows)
    {
        ::rlispstat::windows::performance::Scope timing(
            "Plot.RefreshTrellisBoxplotSelectionVisuals",
            "plot=" + plotId_ + ",changed=" + std::to_string(rows.size()));
        if (!currentModel_ || !trellisSelectionCanvas_) return;
        using namespace ::rlispstat::core;
        const auto theme = PlotThemeStyleForName(themeName_);
        trellisSelectionCanvas_.Children().Clear();
        const auto points = BuildBoxplotPointDrawPlan(
            trellisBoxplotGeometry_, selectedRows_);
        std::map<int, std::size_t> nextVisual;
        for (const auto& point : points)
        {
            const auto colorIt = pointColors_.find(point.caseId);
            const bool explicitColor = colorIt != pointColors_.end();
            const auto fill = explicitColor
                ? (point.selected
                    ? PlotSelectedColorForNameOrHex(colorIt->second)
                    : PlotColorForNameOrHex(colorIt->second))
                : (point.selected ? theme.selectedMarkFill : theme.markFill);
            const auto stroke = point.selected ? theme.selectedMarkStroke
                : (explicitColor ? fill : theme.markStroke);
            const double alpha = point.selected ? 1.0
                : (point.dimmed ? 0.24 : 0.65);
            const double radius = 2.8 * pointSizeScale_;
            if (point.selected)
                for (const auto& ring : CreateSelectionRingVisuals(
                         point.point, radius, theme, fill))
                    trellisSelectionCanvas_.Children().Append(ring);
            if (!rows.count(point.caseId)) continue;
            auto found = trellisPointVisuals_.find(point.caseId);
            const std::size_t index = nextVisual[point.caseId]++;
            if (found == trellisPointVisuals_.end() ||
                index >= found->second.size())
            {
                RenderPlot(std::max(360.0, plotCanvas_.ActualWidth()),
                           std::max(280.0, plotCanvas_.ActualHeight()));
                return;
            }
            auto& dot = found->second[index].dot;
            dot.Fill(Brush(fill, alpha));
            dot.Stroke(Brush(stroke, point.selected ? 1.0
                : (point.dimmed ? 0.18 : 0.48)));
            dot.StrokeThickness(point.selected
                ? theme.selectedMarkStrokeWidth : 0.7);
            Controls::Canvas::SetZIndex(dot, point.selected ? 4 : 3);
        }
    }

    void ScatterPlotView::RefreshTrellisScatterSelectionVisuals(
        std::set<int> const& rows)
    {
        ::rlispstat::windows::performance::Scope timing(
            "Plot.RefreshTrellisScatterSelectionVisuals",
            "plot=" + plotId_ + ",changed=" + std::to_string(rows.size()));
        if (!currentModel_ || rows.empty()) return;
        const auto theme = ::rlispstat::core::PlotThemeStyleForName(themeName_);
        const auto plans = ::rlispstat::core::BuildTrellisPanelRenderPlans(
            *currentModel_, trellisLayout_, selectedRows_, pointColors_, rowLabels_);
        if (currentModel_->trellisSpecification.plotType ==
            ::rlispstat::core::TrellisPlotType::TimeSeries)
        {
            if (trellisTimeSeriesLineVisuals_.size() != plans.size())
            {
                RenderPlot(std::max(360.0, plotCanvas_.ActualWidth()),
                           std::max(280.0, plotCanvas_.ActualHeight()));
                return;
            }
            trellisSelectionCanvas_.Children().Clear();
            for (std::size_t panelIndex = 0; panelIndex < plans.size(); ++panelIndex)
            {
                const auto& seriesLines = plans[panelIndex].timeSeriesLines;
                auto& visuals = trellisTimeSeriesLineVisuals_[panelIndex];
                if (visuals.size() != seriesLines.size())
                {
                    RenderPlot(std::max(360.0, plotCanvas_.ActualWidth()),
                               std::max(280.0, plotCanvas_.ActualHeight()));
                    return;
                }
                for (std::size_t seriesIndex = 0;
                     seriesIndex < seriesLines.size(); ++seriesIndex)
                {
                    const auto& series = seriesLines[seriesIndex];
                    const bool fullySelected = !selectedRows_.empty() &&
                        !series.caseIds.empty() &&
                        std::all_of(series.caseIds.begin(), series.caseIds.end(),
                            [&](int row) { return selectedRows_.count(row) != 0; });
                    const auto color = currentModel_->trellisSpecification.groupingVariableId.empty()
                        ? theme.geomStroke
                        : ::rlispstat::core::PlotColorForNameOrHex(series.colorKey);
                    visuals[seriesIndex].Stroke(Brush(color,
                        selectedRows_.empty() ? 0.96 : 0.20));
                    visuals[seriesIndex].StrokeThickness(
                        fullySelected ? 3.0 : 1.6);
                    if (selectedRows_.empty() ||
                        series.points.size() != series.caseIds.size()) continue;
                    for (std::size_t pointIndex = 1;
                         pointIndex < series.points.size(); ++pointIndex)
                    {
                        if (!selectedRows_.count(series.caseIds[pointIndex - 1]) ||
                            !selectedRows_.count(series.caseIds[pointIndex])) continue;
                        const auto& start = series.points[pointIndex - 1];
                        const auto& end = series.points[pointIndex];
                        trellisSelectionCanvas_.Children().Append(Line(
                            start.x, start.y, end.x, end.y,
                            Brush(color, 0.98), 3.0));
                    }
                }
            }
        }
        std::map<int, std::size_t> nextVisual;
        for (auto const& plan : plans)
            for (auto const& point : plan.scatterplot.points)
            {
                if (point.caseId <= 0 || !rows.count(point.caseId)) continue;
                auto found = trellisPointVisuals_.find(point.caseId);
                const std::size_t index = nextVisual[point.caseId]++;
                if (found == trellisPointVisuals_.end() ||
                    index >= found->second.size())
                {
                    RenderPlot(std::max(360.0, plotCanvas_.ActualWidth()),
                               std::max(280.0, plotCanvas_.ActualHeight()));
                    return;
                }
                auto& visual = found->second[index];
                const auto explicitTone = point.selected
                    ? ::rlispstat::core::PlotSelectedColorForNameOrHex(point.colorName)
                    : ::rlispstat::core::PlotColorForNameOrHex(point.colorName);
                const auto fill = point.hasExplicitColor ? explicitTone
                    : (point.selected ? theme.selectedMarkFill : theme.markFill);
                const auto stroke = point.selected ? theme.selectedMarkStroke
                    : (point.hasExplicitColor ? explicitTone : theme.markStroke);
                const double radius = point.radius * pointSizeScale_;
                visual.dot.Width(radius * 2.0);
                visual.dot.Height(radius * 2.0);
                Controls::Canvas::SetLeft(visual.dot, point.point.x - radius);
                Controls::Canvas::SetTop(visual.dot, point.point.y - radius);
                visual.dot.Fill(Brush(::rlispstat::core::PlotMarkColor(
                    fill, theme.panel, point.shadeOverlap, point.fillAlpha)));
                visual.dot.Stroke(Brush(::rlispstat::core::PlotMarkColor(
                    stroke, theme.panel, point.shadeOverlap, point.strokeAlpha)));
                visual.dot.StrokeThickness(point.selected
                    ? theme.selectedMarkStrokeWidth : point.strokeWidth);
                Controls::Canvas::SetZIndex(visual.dot, point.selected ? 4 : 3);
                if (point.showLabel)
                {
                    if (!visual.label)
                    {
                        visual.label = Label(to_hstring(point.label).c_str(),
                            point.labelAnchor.x + 6.0,
                            point.labelAnchor.y - 18.0, 9.0, Brush(theme.text));
                        visual.label.IsHitTestVisible(false);
                        plotCanvas_.Children().Append(visual.label);
                    }
                    visual.label.Text(to_hstring(point.label));
                    visual.label.Visibility(Visibility::Visible);
                    Controls::Canvas::SetZIndex(visual.label,
                        point.selected ? 31 : 21);
                }
                else if (visual.label)
                    visual.label.Visibility(Visibility::Collapsed);
            }
        RefreshTrellisScatterRings(plans, theme);
    }

    void ScatterPlotView::RefreshTrellisScatterRings(
        std::vector<::rlispstat::core::TrellisPanelRenderPlan> const& plans,
        ::rlispstat::core::PlotThemeStyleSpec const& theme)
    {
        struct RingBatch
        {
            Media::GeometryGroup geometry{ nullptr };
            ::rlispstat::core::PlotRGBA color;
        };
        std::map<std::string, RingBatch> batches;
        for (auto const& plan : plans)
            for (auto const& point : plan.scatterplot.points)
            {
                if (!point.selected) continue;
                const std::string key = point.hasExplicitColor
                    ? "color:" + point.colorName : "default";
                auto& batch = batches[key];
                if (!batch.geometry)
                {
                    batch.geometry = Media::GeometryGroup();
                    batch.color = point.hasExplicitColor
                        ? ::rlispstat::core::PlotSelectedColorForNameOrHex(
                            point.colorName)
                        : theme.selectedMarkFill;
                }
                const double radius = (point.radius + 1.8) * pointSizeScale_;
                auto ring = Media::EllipseGeometry();
                ring.Center({ static_cast<float>(point.point.x),
                              static_cast<float>(point.point.y) });
                ring.RadiusX(radius); ring.RadiusY(radius);
                batch.geometry.Children().Append(ring);
            }
        for (auto const& [key, path] : trellisRingPaths_)
            if (!batches.count(key)) path.Visibility(Visibility::Collapsed);
        for (auto& [key, batch] : batches)
        {
            auto found = trellisRingPaths_.find(key);
            if (found == trellisRingPaths_.end())
            {
                auto path = Shapes::Path();
                path.IsHitTestVisible(false);
                path.Fill(Brush(batch.color, 0.28));
                path.Stroke(TransparentBrush());
                Controls::Canvas::SetZIndex(path, 2);
                plotCanvas_.Children().Append(path);
                found = trellisRingPaths_.emplace(key, path).first;
            }
            found->second.Data(batch.geometry);
            found->second.Visibility(Visibility::Visible);
        }
    }

    void ScatterPlotView::RenderScatterMatrix(
        double width,
        double height,
        ::rlispstat::core::PlotThemeStyleSpec const& theme)
    {
        using namespace ::rlispstat::core;
        plotCanvas_.Children().Clear();
        pointsCanvas_.Children().Clear();
        renderedPoints_.clear();
        scatterMatrixGeometry_.clear();
        scatterMatrixPointVisuals_.clear();
        scatterMatrixRingPaths_.clear();
        plotCanvas_.Background(Brush(theme.background));

        auto variables = ScatterMatrixVariablesForModel(*currentModel_);
        auto series = ScatterMatrixVariableSeriesForModel(*currentModel_, variables);
        scatterMatrixLayout_.plotRect = ScatterMatrixPlotRectForBounds(width, height);
        scatterMatrixLayout_.variableCount = variables.size();
        const auto renderPlan = BuildScatterMatrixRenderPlan(
            variables, currentModel_->title, scatterMatrixLayout_);

        plotCanvas_.Children().Append(Rectangle(renderPlan.panelRect, Brush(theme.panel),
            TransparentBrush(), 0.0));
        for (auto const& cell : renderPlan.cells)
        {
            const auto fill = cell.diagonal ? Brush(theme.background) : Brush(theme.panel);
            plotCanvas_.Children().Append(Rectangle(cell.rect, fill,
                Brush(theme.majorGrid), 0.8));
        }
        for (std::size_t labelIndex = 0;
             labelIndex < renderPlan.diagonalLabels.size(); ++labelIndex)
        {
            auto const& labelItem = renderPlan.diagonalLabels[labelIndex];
            auto label = Label(to_hstring(labelItem.text).c_str(), labelItem.rect.x,
                labelItem.rect.y, 11.5, Brush(theme.text));
            label.Width(labelItem.rect.width);
            label.TextAlignment(TextAlignment::Center);
            label.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
            plotCanvas_.Children().Append(label);
            AttachNumericVariableReplacementMenu(label, labelItem.text,
                std::to_string(labelIndex), variables,
                "SCATTER_MATRIX_REPLACE_VARIABLE");
        }
        if (renderPlan.showTitle)
        {
            auto title = Label(to_hstring(renderPlan.title.text).c_str(),
                renderPlan.title.rect.x, renderPlan.title.rect.y, 16.0, Brush(theme.text));
            title.Width(renderPlan.title.rect.width);
            title.TextAlignment(TextAlignment::Center);
            title.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
            plotCanvas_.Children().Append(title);
        }
        if (renderPlan.showEmptyMessage)
        {
            plotCanvas_.Children().Append(Label(to_hstring(renderPlan.emptyMessage.text).c_str(),
                renderPlan.emptyMessage.rect.x, renderPlan.emptyMessage.rect.y,
                12.0, Brush(theme.mutedText)));
        }

        const auto visibleRows = ScatterMatrixVisibleRows(series);
        scatterMatrixGeometry_ = BuildScatterMatrixCaseGeometry(
            series, visibleRows, scatterMatrixLayout_, 5.0);

        // The matrix uses the R-returned fit curves for every cell. The
        // native renderer only converts their coordinates to screen space.
        if (currentModel_ && series.size() == variables.size())
        {
            for (std::size_t row = 0; row < variables.size(); ++row)
            {
                for (std::size_t column = 0; column < variables.size(); ++column)
                {
                    if (row == column) continue;
                    const auto xRange = ScatterMatrixRangeForValues(series[column].values);
                    const auto yRange = ScatterMatrixRangeForValues(series[row].values);
                    const DataViewport viewport{
                        xRange.minimum, xRange.maximum, yRange.minimum, yRange.maximum };
                    const Rect cellRect = ScatterMatrixInnerCellRect(
                        scatterMatrixLayout_, row, column, 5.0);
                    ScatterplotRenderInput input;
                    input.viewport = viewport;
                    input.plotRect = cellRect;
                    const std::string cellId = ScatterMatrixFitPanelId(
                        *currentModel_, row, column);
                    if (const auto curves = currentModel_->trellisPanelSmoothCurves.find(cellId);
                        curves != currentModel_->trellisPanelSmoothCurves.end())
                        input.smoothCurves = curves->second;
                    input.showFitConfidenceIntervals =
                        currentModel_->scatterFitConfidenceIntervalsVisible;
                    const auto cellPlan = BuildScatterplotRenderPlan(input);
                    for (auto const& curve : cellPlan.smoothCurves)
                    {
                        if (curve.confidencePolygon.size() < 3) continue;
                        const auto color = curve.colorName.empty()
                            ? theme.smooth : PlotColorForNameOrHex(curve.colorName);
                        plotCanvas_.Children().Append(Polygon(
                            curve.confidencePolygon,
                            Brush(color, curve.confidenceAlpha)));
                    }
                    for (auto const& curve : cellPlan.smoothCurves)
                    {
                        auto line = Polyline(curve.points,
                            Brush(PlotColorForNameOrHex(curve.colorName), curve.alpha),
                            curve.lineWidth);
                        if (curve.dashed)
                        {
                            auto dashes = Media::DoubleCollection();
                            dashes.Append(4.0); dashes.Append(3.0);
                            line.StrokeDashArray(dashes);
                        }
                        plotCanvas_.Children().Append(line);
                    }
                }
            }
        }
        const auto points = BuildScatterMatrixPointDrawPlan(
            scatterMatrixGeometry_, selectedRows_, pointColors_, rowLabels_,
            currentModel_->labelDisplayMode, variables.size());
        for (auto const& point : points)
        {
            ScatterMatrixPointVisual visual;
            const auto explicitColor = point.hasExplicitColor
                ? (point.selected
                    ? PlotSelectedColorForNameOrHex(point.colorName)
                    : PlotColorForNameOrHex(point.colorName))
                : (point.selected ? theme.selectedMarkFill : theme.markFill);
            const auto stroke = point.selected ? theme.selectedMarkStroke
                : (point.hasExplicitColor ? explicitColor : theme.markStroke);
            const double radius = point.radius * pointSizeScale_;
            auto dot = Shapes::Ellipse();
            dot.Width(radius * 2.0); dot.Height(radius * 2.0);
            dot.Fill(Brush(explicitColor));
            dot.Stroke(Brush(stroke));
            dot.StrokeThickness(point.selected ? theme.selectedMarkStrokeWidth : 0.7);
            dot.Opacity(point.alpha);
            dot.IsHitTestVisible(false);
            Controls::Canvas::SetZIndex(dot, point.selected ? 3 : 1);
            Controls::Canvas::SetLeft(dot, point.point.x - radius);
            Controls::Canvas::SetTop(dot, point.point.y - radius);
            plotCanvas_.Children().Append(dot);
            visual.dot = dot;
            visual.selected = point.selected;
            if (point.showLabel)
            {
                auto label = Label(to_hstring(point.label).c_str(),
                    point.point.x + radius + 2.0, point.point.y - radius - 12.0,
                    8.0, Brush(theme.text));
                label.IsHitTestVisible(false);
                Controls::Canvas::SetZIndex(label, point.selected ? 4 : 2);
                plotCanvas_.Children().Append(label);
                visual.label = label;
            }
            scatterMatrixPointVisuals_[point.caseId].push_back(std::move(visual));
        }
        RefreshScatterMatrixRings(points, theme);

        pointsCanvas_.Width(width); pointsCanvas_.Height(height);
        selectionRectangle_.Visibility(Visibility::Collapsed);
        selectionRectangle_.Stroke(Brush(theme.accent));
        selectionRectangle_.Fill(Brush(theme.accent, 0.16));
        pointsCanvas_.Children().Append(selectionRectangle_);
        plotCanvas_.Children().Append(pointsCanvas_);
        Controls::Canvas::SetZIndex(pointsCanvas_, 10);
        AppendPanelFrame(plotCanvas_, renderPlan.panelRect, theme);
    }

    void ScatterPlotView::RefreshScatterMatrixSelectionVisuals(
        std::set<int> const& rows)
    {
        ::rlispstat::windows::performance::Scope timing(
            "Plot.RefreshScatterMatrixSelectionVisuals",
            "plot=" + plotId_ + ",changed=" + std::to_string(rows.size()));
        if (!currentModel_ || rows.empty()) return;
        const auto theme = ::rlispstat::core::PlotThemeStyleForName(themeName_);
        const auto points = ::rlispstat::core::BuildScatterMatrixPointDrawPlan(
            scatterMatrixGeometry_, selectedRows_, pointColors_, rowLabels_,
            currentModel_->labelDisplayMode, scatterMatrixLayout_.variableCount);
        std::map<int, std::size_t> nextVisual;
        for (auto const& point : points)
        {
            if (!rows.count(point.caseId)) continue;
            auto found = scatterMatrixPointVisuals_.find(point.caseId);
            const std::size_t index = nextVisual[point.caseId]++;
            if (found == scatterMatrixPointVisuals_.end() ||
                index >= found->second.size())
            {
                RenderPlot(std::max(360.0, plotCanvas_.ActualWidth()),
                           std::max(280.0, plotCanvas_.ActualHeight()));
                return;
            }
            auto& visual = found->second[index];
            const auto explicitColor = point.hasExplicitColor
                ? (point.selected
                    ? ::rlispstat::core::PlotSelectedColorForNameOrHex(point.colorName)
                    : ::rlispstat::core::PlotColorForNameOrHex(point.colorName))
                : (point.selected ? theme.selectedMarkFill : theme.markFill);
            const auto stroke = point.selected ? theme.selectedMarkStroke
                : (point.hasExplicitColor ? explicitColor : theme.markStroke);
            if (visual.selected != point.selected)
            {
                visual.dot.Fill(Brush(explicitColor));
                visual.dot.Stroke(Brush(stroke));
                visual.dot.StrokeThickness(
                    point.selected ? theme.selectedMarkStrokeWidth : 0.7);
                Controls::Canvas::SetZIndex(visual.dot,
                    point.selected ? 3 : 1);
                visual.selected = point.selected;
            }
            visual.dot.Opacity(point.alpha);
            if (point.showLabel)
            {
                if (!visual.label)
                {
                    const double radius = point.radius * pointSizeScale_;
                    visual.label = Label(to_hstring(point.label).c_str(),
                        point.point.x + radius + 2.0,
                        point.point.y - radius - 12.0,
                        8.0, Brush(theme.text));
                    visual.label.IsHitTestVisible(false);
                    plotCanvas_.Children().Append(visual.label);
                }
                visual.label.Text(to_hstring(point.label));
                visual.label.Visibility(Visibility::Visible);
                Controls::Canvas::SetZIndex(visual.label,
                    point.selected ? 4 : 2);
            }
            else if (visual.label)
            {
                visual.label.Visibility(Visibility::Collapsed);
            }
        }
        RefreshScatterMatrixRings(points, theme);
    }

    void ScatterPlotView::RefreshScatterMatrixRings(
        std::vector<::rlispstat::core::ScatterMatrixPointDrawItem> const& points,
        ::rlispstat::core::PlotThemeStyleSpec const& theme)
    {
        ::rlispstat::windows::performance::Scope timing(
            "Plot.RefreshScatterMatrixRings",
            "plot=" + plotId_ + ",marks=" + std::to_string(points.size()));
        struct RingBatch
        {
            Media::GeometryGroup geometry{ nullptr };
            ::rlispstat::core::PlotRGBA color;
        };
        std::map<std::string, RingBatch> batches;
        for (auto const& point : points)
        {
            if (!point.selected) continue;
            const std::string key = point.hasExplicitColor
                ? "color:" + point.colorName : "default";
            auto& batch = batches[key];
            if (!batch.geometry)
            {
                batch.geometry = Media::GeometryGroup();
                batch.color = point.hasExplicitColor
                    ? ::rlispstat::core::PlotSelectedColorForNameOrHex(
                        point.colorName)
                    : theme.selectedMarkFill;
            }
            const double radius = point.radius * pointSizeScale_ +
                1.8 * pointSizeScale_;
            auto ring = Media::EllipseGeometry();
            ring.Center({ static_cast<float>(point.point.x),
                          static_cast<float>(point.point.y) });
            ring.RadiusX(radius);
            ring.RadiusY(radius);
            batch.geometry.Children().Append(ring);
        }
        for (auto const& [key, path] : scatterMatrixRingPaths_)
            if (!batches.count(key)) path.Visibility(Visibility::Collapsed);
        for (auto& [key, batch] : batches)
        {
            auto found = scatterMatrixRingPaths_.find(key);
            if (found == scatterMatrixRingPaths_.end())
            {
                auto path = Shapes::Path();
                path.IsHitTestVisible(false);
                path.Fill(Brush(batch.color, 0.28));
                path.Stroke(TransparentBrush());
                Controls::Canvas::SetZIndex(path, 2);
                plotCanvas_.Children().Append(path);
                found = scatterMatrixRingPaths_.emplace(key, path).first;
            }
            found->second.Data(batch.geometry);
            found->second.Visibility(Visibility::Visible);
        }
    }

    void ScatterPlotView::BeginSelection(double x, double y,
                                         ::rlispstat::core::SelectionMode mode)
    {
        selectionStartX_ = x;
        selectionStartY_ = y;
        selectionMode_ = mode;
        selecting_ = true;
        const bool fixedBrush = currentModel_ &&
            currentModel_->interactionMode == "brush";
        if (fixedBrush)
        {
            brushSelectionBase_ = selectedRows_;
            brushPublishedSelection_.clear();
            brushSelectionPublished_ = false;
        }
        const auto rect = ::rlispstat::core::BrushRectForGesture(
            {x, y}, {x, y}, fixedBrush,
            currentModel_ ? currentModel_->brushWidth : 80.0,
            currentModel_ ? currentModel_->brushHeight : 60.0);
        Controls::Canvas::SetLeft(selectionRectangle_, rect.x);
        Controls::Canvas::SetTop(selectionRectangle_, rect.y);
        selectionRectangle_.Width(rect.width);
        selectionRectangle_.Height(rect.height);
        selectionRectangle_.Visibility(currentModel_ &&
            currentModel_->interactionMode == "pan"
                ? Visibility::Collapsed : Visibility::Visible);
    }

    bool ScatterPlotView::RowSelectionGestureEnabled() const
    {
        return currentModel_ &&
            ::rlispstat::core::PlotLinksToDataRows(*currentModel_) &&
            ::rlispstat::core::PlotInteractionModeAllowsRowSelection(
                currentModel_->interactionMode);
    }

    bool ScatterPlotView::ViewportNavigationGestureEnabled() const
    {
        if (!currentModel_) return false;
        const auto& kind = currentModel_->kind;
        return kind == "scatter" || kind == "time_series" ||
            kind == "glm_interaction" || kind == "pca_scree" ||
            kind == "pca_biplot" || currentModel_->isGLMDiagnostic;
    }

    void ScatterPlotView::UpdateSelection(double x, double y)
    {
        if (currentModel_ && currentModel_->interactionMode == "pan" &&
            ViewportNavigationGestureEnabled() &&
            ::rlispstat::core::IsValidViewport(renderedViewport_) &&
            ::rlispstat::core::IsValidRect(renderedPlotRect_))
        {
            const double dx = (x - selectionStartX_) *
                (renderedViewport_.xmax - renderedViewport_.xmin) /
                renderedPlotRect_.width;
            const double dy = (y - selectionStartY_) *
                (renderedViewport_.ymax - renderedViewport_.ymin) /
                renderedPlotRect_.height;
            currentModel_->xmin -= dx;
            currentModel_->xmax -= dx;
            currentModel_->ymin += dy;
            currentModel_->ymax += dy;
            selectionStartX_ = x;
            selectionStartY_ = y;
            RenderPlot(std::max(360.0, plotCanvas_.ActualWidth()),
                       std::max(280.0, plotCanvas_.ActualHeight()));
            return;
        }
        if (currentModel_ && currentModel_->interactionMode == "brush")
        {
            const auto rect = ::rlispstat::core::BrushRectForGesture(
                {selectionStartX_, selectionStartY_}, {x, y}, true,
                currentModel_->brushWidth, currentModel_->brushHeight);
            Controls::Canvas::SetLeft(selectionRectangle_, rect.x);
            Controls::Canvas::SetTop(selectionRectangle_, rect.y);
            selectionRectangle_.Width(rect.width);
            selectionRectangle_.Height(rect.height);
            PublishBrushSelection(x, y);
            return;
        }
        const double left = std::min(selectionStartX_, x);
        const double top = std::min(selectionStartY_, y);
        Controls::Canvas::SetLeft(selectionRectangle_, left);
        Controls::Canvas::SetTop(selectionRectangle_, top);
        selectionRectangle_.Width(std::abs(x - selectionStartX_));
        selectionRectangle_.Height(std::abs(y - selectionStartY_));
    }

    std::set<int> ScatterPlotView::RowsForFixedBrush(double x, double y) const
    {
        if (!currentModel_) return {};
        const ::rlispstat::core::Point current{x, y};
        const auto brush = ::rlispstat::core::BrushRectForGesture(
            {selectionStartX_, selectionStartY_}, current, true,
            currentModel_->brushWidth, currentModel_->brushHeight);
        if (currentModel_->kind == "scatter_matrix")
            return ::rlispstat::core::SelectScatterMatrixCasesForGesture(
                scatterMatrixGeometry_, scatterMatrixLayout_, brush, current,
                true, 0.0);
        if (currentModel_->kind == "trellis_scatterplot")
            return ::rlispstat::core::SelectTrellisScatterplotCasesForGesture(
                trellisLayout_, brush, current, true, 0.0, 2.0);
        if (currentModel_->kind == "boxplot")
            return ::rlispstat::core::SelectBoxplotCasesForGesture(
                boxplotGeometry_, brush, current, true, 0.0);
        if (currentModel_->kind == "histogram")
            return ::rlispstat::core::SelectHistogramCasesForGesture(
                histogramBinRows_, histogramLayout_,
                {brush.x, brush.y}, {brush.x + brush.width, brush.y + brush.height});
        if (currentModel_->kind == "barplot")
        {
            const bool hasSplit = !currentModel_->barplotSplitVariable.empty();
            const auto levels = ::rlispstat::core::BarplotSplitLevelsForBins(
                barplotBins_, hasSplit);
            return ::rlispstat::core::BarplotRowsForGesture(
                barplotLayout_, barplotBins_, levels, hasSplit,
                {brush.x, brush.y}, {brush.x + brush.width, brush.y + brush.height});
        }
        std::vector<::rlispstat::core::ScatterplotCaseGeometry> geometry = scatterGeometry_;
        if (geometry.empty())
        {
            geometry.reserve(renderedPoints_.size());
            for (auto const& point : renderedPoints_)
                geometry.push_back({point.row, {point.x, point.y}, false, {}});
        }
        return ::rlispstat::core::SelectCasesForGesture(
            geometry, brush, current, true, 0.0, 2.0);
    }

    void ScatterPlotView::PublishBrushSelection(double x, double y)
    {
        if (!currentModel_ || currentModel_->interactionMode != "brush" ||
            !selectionCallback_ || group_.empty()) return;
        const auto rows = RowsForFixedBrush(x, y);
        const auto next = ::rlispstat::core::ApplySelectionOperation(
            brushSelectionBase_, rows, selectionMode_);
        if (brushSelectionPublished_ && next == brushPublishedSelection_) return;
        brushPublishedSelection_ = next;
        brushSelectionPublished_ = true;
        // Publish the fully resolved selection. Reapplying add/subtract/toggle
        // against every intermediate frame would otherwise accumulate or
        // oscillate while the brush moves.
        selectionCallback_(group_, next, ::rlispstat::core::SelectionMode::Replace);
    }

    void ScatterPlotView::CompleteSelection(double x, double y,
                                             ::rlispstat::core::SelectionMode mode)
    {
        ::rlispstat::windows::performance::Scope timing(
            "Plot.CompleteSelection", "plot=" + plotId_,
            currentModel_ && currentModel_->kind == "scatter_matrix");
        if (!selecting_) return;
        if (currentModel_ && currentModel_->interactionMode == "pan")
        {
            selecting_ = false;
            selectionRectangle_.Visibility(Visibility::Collapsed);
            ConfigureContextMenu();
            return;
        }
        if (currentModel_ && currentModel_->interactionMode == "zoom" &&
            ViewportNavigationGestureEnabled())
        {
            selecting_ = false;
            selectionRectangle_.Visibility(Visibility::Collapsed);
            const ::rlispstat::core::Rect brush =
                ::rlispstat::core::RectBetweenPoints(
                    {selectionStartX_, selectionStartY_}, {x, y});
            if (::rlispstat::core::RectMeetsMinimumSize(brush, 8.0, 8.0) &&
                ::rlispstat::core::IsValidViewport(renderedViewport_) &&
                ::rlispstat::core::IsValidRect(renderedPlotRect_))
            {
                const auto first = ::rlispstat::core::ScreenToData(
                    {brush.x, brush.y + brush.height}, renderedViewport_,
                    renderedPlotRect_, true);
                const auto second = ::rlispstat::core::ScreenToData(
                    {brush.x + brush.width, brush.y}, renderedViewport_,
                    renderedPlotRect_, true);
                currentModel_->xmin = std::min(first.x, second.x);
                currentModel_->xmax = std::max(first.x, second.x);
                currentModel_->ymin = std::min(first.y, second.y);
                currentModel_->ymax = std::max(first.y, second.y);
                RenderPlot(std::max(360.0, plotCanvas_.ActualWidth()),
                           std::max(280.0, plotCanvas_.ActualHeight()));
            }
            ConfigureContextMenu();
            return;
        }
        const bool liveBrush = currentModel_ &&
            currentModel_->interactionMode == "brush";
        if (liveBrush) PublishBrushSelection(x, y);
        selecting_ = false;
        selectionRectangle_.Visibility(Visibility::Collapsed);
        if (liveBrush)
        {
            brushSelectionBase_.clear();
            brushPublishedSelection_.clear();
            brushSelectionPublished_ = false;
            return;
        }

        const ::rlispstat::core::Point start{ selectionStartX_, selectionStartY_ };
        const ::rlispstat::core::Point current{ x, y };
        const bool fixedBrush = currentModel_ &&
            currentModel_->interactionMode == "brush";
        const ::rlispstat::core::Rect brush =
            ::rlispstat::core::BrushRectForGesture(
                start, current, fixedBrush,
                currentModel_ ? currentModel_->brushWidth : 80.0,
                currentModel_ ? currentModel_->brushHeight : 60.0);
        const bool dragBrush = ::rlispstat::core::RectMeetsMinimumSize(brush, 3.0, 3.0);
        const double pointHitRadius = std::max(8.0, 4.0 * pointSizeScale_);
        if (currentModel_ &&
            (currentModel_->kind == "glm_interaction" ||
             currentModel_->kind == "time_series") &&
            !dragBrush)
        {
            const auto series = currentModel_->kind == "time_series"
                ? ::rlispstat::core::HitTimeSeriesSegmentAtScreenPoint(
                    *currentModel_, renderedViewport_, renderedPlotRect_, current,
                    3.0 * pointSizeScale_ + 1.0,
                    std::max(9.0, pointHitRadius))
                : ::rlispstat::core::HitPlotSeriesAtScreenPoint(
                    *currentModel_, renderedViewport_, renderedPlotRect_, current,
                    std::max(9.0, pointHitRadius));
            if (series)
            {
                const auto rows = ::rlispstat::core::PlotSeriesLegendRows(
                    *currentModel_, *series);
                if (!rows.empty())
                {
                    if (selectionCallback_ && !group_.empty())
                        selectionCallback_(group_, rows, mode);
                    return;
                }
            }
        }
        if (currentModel_ && currentModel_->kind == "scatter_matrix")
        {
            const auto rows = ::rlispstat::core::SelectScatterMatrixCasesForGesture(
                scatterMatrixGeometry_, scatterMatrixLayout_, brush, current,
                dragBrush, pointHitRadius);
            if (selectionCallback_ && !group_.empty()) selectionCallback_(group_, rows, mode);
            return;
        }
        if (currentModel_ && currentModel_->kind == "trellis_scatterplot")
        {
            const auto rows = ::rlispstat::core::SelectTrellisScatterplotCasesForGesture(
                trellisLayout_, brush, current, dragBrush, pointHitRadius, 2.0);
            if (selectionCallback_ && !group_.empty()) selectionCallback_(group_, rows, mode);
            return;
        }
        if (currentModel_ && currentModel_->kind == "boxplot")
        {
            const auto rows = ::rlispstat::core::SelectBoxplotCasesForGesture(
                boxplotGeometry_, brush, current, dragBrush, pointHitRadius);
            if (selectionCallback_ && !group_.empty()) selectionCallback_(group_, rows, mode);
            return;
        }
        if (currentModel_ && currentModel_->kind == "histogram")
        {
            const ::rlispstat::core::Point gestureStart = fixedBrush
                ? ::rlispstat::core::Point{brush.x, brush.y} : start;
            const ::rlispstat::core::Point gestureCurrent = fixedBrush
                ? ::rlispstat::core::Point{brush.x + brush.width,
                                           brush.y + brush.height} : current;
            const auto rows = ::rlispstat::core::SelectHistogramCasesForGesture(
                histogramBinRows_, histogramLayout_, gestureStart, gestureCurrent);
            if (selectionCallback_ && !group_.empty()) selectionCallback_(group_, rows, mode);
            return;
        }
        if (currentModel_ && currentModel_->kind == "barplot")
        {
            const bool hasSplit = !currentModel_->barplotSplitVariable.empty();
            const auto levels = ::rlispstat::core::BarplotSplitLevelsForBins(
                barplotBins_, hasSplit);
            const std::set<int> rows = ::rlispstat::core::BarplotRowsForGesture(
                barplotLayout_, barplotBins_, levels, hasSplit,
                fixedBrush ? ::rlispstat::core::Point{brush.x, brush.y} : start,
                fixedBrush ? ::rlispstat::core::Point{brush.x + brush.width,
                    brush.y + brush.height} : current);
            if (selectionCallback_ && !group_.empty()) selectionCallback_(group_, rows, mode);
            return;
        }
        std::vector<::rlispstat::core::ScatterplotCaseGeometry> geometry = scatterGeometry_;
        if (geometry.empty())
        {
            geometry.reserve(renderedPoints_.size());
            for (auto const& point : renderedPoints_)
                geometry.push_back({ point.row, { point.x, point.y }, false, {} });
        }
        const std::set<int> rows = ::rlispstat::core::SelectCasesForGesture(
            geometry, brush, { x, y }, dragBrush, pointHitRadius, 2.0);
        if (currentModel_ && currentModel_->kind == "scatter")
        {
            mode = ::rlispstat::core::SelectionModeForPointerGesture(
                selectedRows_, rows, mode, dragBrush);
        }
        if (selectionCallback_ && !group_.empty()) selectionCallback_(group_, rows, mode);
    }

    void ScatterPlotView::ApplyVisibleSelection(::rlispstat::core::SelectionMode mode)
    {
        std::set<int> rows;
        if (currentModel_)
        {
            // Keep the definition of a plot's visible case universe in the
            // shared selection model.  This covers scatterplots, matrices,
            // histograms, bars, boxplots, time series and every Trellis form,
            // including aggregate Trellis panels that do not draw one XAML
            // element per observation.
            rows = ::rlispstat::core::VisibleRowsForModel(currentModel_);
        }
        else
        {
            for (const auto& point : renderedPoints_) rows.insert(point.row);
        }
        if (mode == ::rlispstat::core::SelectionMode::Toggle)
        {
            // "Invert selection" is a complete operation over this plot's
            // visible universe, not a pointer gesture. Compute the result in
            // the shared selection model and replace the linked selection so
            // every plot kind has identical semantics (including removing
            // selected cases that are outside the current plot).
            rows = ::rlispstat::core::InvertSelectionWithin(selectedRows_, rows);
            mode = ::rlispstat::core::SelectionMode::Replace;
        }
        if (selectionCallback_ && !group_.empty()) selectionCallback_(group_, rows, mode);
    }

    void ScatterPlotView::ClearSelection()
    {
        if (selectionCallback_ && !group_.empty()) {
            selectionCallback_(group_, {}, ::rlispstat::core::SelectionMode::Replace);
        }
    }

    void ScatterPlotView::Activate()
    {
        if (closed_) return;
        if (auto presenter = window_.AppWindow().Presenter().try_as<
                Microsoft::UI::Windowing::OverlappedPresenter>())
        {
            if (presenter.State() == Microsoft::UI::Windowing::OverlappedPresenterState::Minimized)
                presenter.Restore();
        }
        window_.Activate();
    }

    void ScatterPlotView::ShowInformation(std::string const& title,
                                          std::string const& message)
    {
        MessageBoxW(WindowHandle(window_), to_hstring(message).c_str(),
                    to_hstring(title).c_str(), MB_OK | MB_ICONINFORMATION);
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
