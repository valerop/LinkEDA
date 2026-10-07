#include "pch.h"
#include "WelcomeWindow.h"
#include "WindowBranding.h"

#include <microsoft.ui.xaml.window.h>
#include <winrt/Microsoft.UI.Xaml.Media.Imaging.h>

#include <algorithm>
#include <array>
#include <cctype>
#include <cmath>
#include <tuple>

using namespace winrt;
using namespace Microsoft::UI::Xaml;

namespace
{
    Media::SolidColorBrush Brush(uint8_t red, uint8_t green, uint8_t blue)
    {
        return Media::SolidColorBrush(
            Windows::UI::ColorHelper::FromArgb(255, red, green, blue));
    }

    HWND WindowHandle(Window const& window)
    {
        HWND handle{};
        check_hresult(window.as<::IWindowNative>()->get_WindowHandle(&handle));
        return handle;
    }

    void ResizeLogical(Window const& window, double width, double height)
    {
        winrt::LinkEDA::implementation::ResizeLinkEDAWindowClient(
            window, width, height);
    }

    void CenterOnOwnerMonitor(Window const& window, Window const& owner)
    {
        const HWND handle = WindowHandle(window);
        const HWND reference = owner ? WindowHandle(owner) : handle;
        MONITORINFO monitor{ sizeof(MONITORINFO) };
        RECT bounds{};
        if (!GetMonitorInfoW(MonitorFromWindow(reference, MONITOR_DEFAULTTONEAREST),
                             &monitor) || !GetWindowRect(handle, &bounds)) return;
        const auto& work = monitor.rcWork;
        const int width = bounds.right - bounds.left;
        const int height = bounds.bottom - bounds.top;
        const int left = work.left + (work.right - work.left - width) / 2;
        const int top = work.top + (work.bottom - work.top - height) / 2;
        SetWindowPos(handle, nullptr, left, top, 0, 0,
                     SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE);
    }

    Controls::TextBlock Label(hstring const& text, double size,
                              Windows::UI::Text::FontWeight const& weight,
                              Media::Brush const& foreground)
    {
        auto label = Controls::TextBlock();
        label.Text(text);
        label.FontSize(size);
        label.FontWeight(weight);
        label.Foreground(foreground);
        label.TextWrapping(TextWrapping::Wrap);
        return label;
    }

    Controls::Button ActionButton(hstring const& text, Controls::Symbol symbol,
                                  bool primary)
    {
        auto button = Controls::Button();
        button.Height(44);
        button.HorizontalAlignment(HorizontalAlignment::Stretch);
        button.HorizontalContentAlignment(HorizontalAlignment::Left);
        button.Padding(Thickness{ 16, 0, 16, 0 });
        button.CornerRadius(CornerRadius{ 6 });
        auto row = Controls::StackPanel();
        row.Orientation(Controls::Orientation::Horizontal);
        row.Spacing(12);
        auto icon = Controls::SymbolIcon(symbol);
        icon.Width(20);
        auto label = Controls::TextBlock();
        label.Text(text);
        label.FontSize(14);
        label.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        label.VerticalAlignment(VerticalAlignment::Center);
        row.Children().Append(icon);
        row.Children().Append(label);
        button.Content(row);
        if (primary)
        {
            button.Background(Brush(0, 105, 235));
            button.Foreground(Brush(255, 255, 255));
        }
        else
        {
            button.Background(Brush(232, 233, 235));
            button.BorderThickness(Thickness{ 0 });
        }
        return button;
    }

    Controls::Button LinkButton(hstring const& text)
    {
        auto button = Controls::Button();
        button.Content(box_value(text));
        button.Background(Brush(255, 255, 255));
        button.BorderThickness(Thickness{ 0 });
        button.Foreground(Brush(0, 92, 172));
        button.Padding(Thickness{ 5, 3, 5, 3 });
        return button;
    }

    Controls::Border PlotPreview(int kind)
    {
        auto card = Controls::Border();
        card.Width(92); card.Height(88); card.CornerRadius(CornerRadius{ 10 });
        card.Background(Brush(248, 249, 250)); card.BorderBrush(Brush(215, 218, 223));
        card.BorderThickness(Thickness{ 1 });
        auto canvas = Controls::Canvas(); canvas.Width(78); canvas.Height(68);
        auto axis = [&](double x1, double y1, double x2, double y2)
        {
            auto line = Shapes::Line(); line.X1(x1); line.Y1(y1); line.X2(x2); line.Y2(y2);
            line.Stroke(Brush(173, 177, 183)); line.StrokeThickness(1);
            canvas.Children().Append(line);
        };
        axis(8, 4, 8, 61); axis(8, 61, 73, 61);
        if (kind == 0)
        {
            const std::array<std::pair<double, double>, 8> points{{
                {15,51},{22,43},{29,48},{35,34},{43,40},{50,25},{60,31},{67,14}}};
            for (auto const& [x, y] : points)
            {
                auto dot = Shapes::Ellipse(); dot.Width(6); dot.Height(6); dot.Fill(Brush(0, 105, 235));
                Controls::Canvas::SetLeft(dot, x); Controls::Canvas::SetTop(dot, y);
                canvas.Children().Append(dot);
            }
        }
        else if (kind == 1)
        {
            const std::array<double, 6> heights{{22,36,47,40,31,19}};
            for (std::size_t index = 0; index < heights.size(); ++index)
            {
                auto bar = Shapes::Rectangle(); bar.Width(8); bar.Height(heights[index]);
                bar.Fill(Brush(145, 207, 142));
                Controls::Canvas::SetLeft(bar, 14.0 + 10.0 * static_cast<double>(index));
                Controls::Canvas::SetTop(bar, 61 - heights[index]); canvas.Children().Append(bar);
            }
            auto curve = Shapes::Polyline(); curve.Stroke(Brush(0, 105, 235)); curve.StrokeThickness(2);
            Windows::Foundation::Collections::IVector<Windows::Foundation::Point> curvePoints = curve.Points();
            curvePoints.Append({10,54}); curvePoints.Append({22,30}); curvePoints.Append({34,14});
            curvePoints.Append({46,22}); curvePoints.Append({58,39}); curvePoints.Append({70,55});
            canvas.Children().Append(curve);
        }
        else
        {
            const std::array<std::tuple<double, double, Windows::UI::Color>, 3> boxes{{
                {16,28,Windows::UI::ColorHelper::FromArgb(255,95,151,231)},
                {36,34,Windows::UI::ColorHelper::FromArgb(255,75,202,102)},
                {56,22,Windows::UI::ColorHelper::FromArgb(255,255,161,55)}}};
            for (auto const& [x, y, color] : boxes)
            {
                auto whisker = Shapes::Line(); whisker.X1(x + 6); whisker.X2(x + 6);
                whisker.Y1(y - 14); whisker.Y2(y + 32); whisker.Stroke(Brush(115, 118, 124));
                canvas.Children().Append(whisker);
                auto box = Shapes::Rectangle(); box.Width(12); box.Height(23);
                box.Fill(Media::SolidColorBrush(color)); box.Stroke(Brush(50, 105, 195));
                Controls::Canvas::SetLeft(box, x); Controls::Canvas::SetTop(box, y);
                canvas.Children().Append(box);
            }
        }
        card.Child(canvas); return card;
    }
}

namespace winrt::LinkEDA::implementation
{
    std::shared_ptr<WelcomeWindow> WelcomeWindow::Create()
    {
        auto view = std::shared_ptr<WelcomeWindow>(new WelcomeWindow());
        view->AttachLifetime();
        return view;
    }

    WelcomeWindow::WelcomeWindow()
    {
        Initialize();
    }

    void WelcomeWindow::Initialize()
    {
        window_ = CreateLinkEDAWindow();
        window_.Title(L"Welcome to LinkEDA");

        auto root = Controls::Grid();
        root.RequestedTheme(ElementTheme::Light);
        root.Background(Brush(242, 242, 243));
        auto identityColumn = Controls::ColumnDefinition();
        // Keep enough room for the two-line tagline beside the application icon.
        // At the default 900 px window width, 355 px clipped the word "data".
        identityColumn.Width(GridLengthHelper::FromPixels(390));
        root.ColumnDefinitions().Append(identityColumn);
        auto actionColumn = Controls::ColumnDefinition();
        actionColumn.Width(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
        root.ColumnDefinitions().Append(actionColumn);

        auto identity = Controls::Grid();
        identity.Padding(Thickness{ 38, 42, 30, 24 });
        identity.Background(Brush(235, 242, 255));
        auto identityRows = { 144.0, 1.0, 102.0, 30.0, 27.0, 24.0 };
        for (double height : identityRows)
        {
            auto row = Controls::RowDefinition();
            row.Height(height == 1.0
                ? GridLengthHelper::FromValueAndType(1, GridUnitType::Star)
                : GridLengthHelper::FromPixels(height));
            identity.RowDefinitions().Append(row);
        }

        auto brand = Controls::StackPanel();
        brand.Orientation(Controls::Orientation::Horizontal);
        brand.Spacing(16);
        auto image = Controls::Image();
        image.Width(108); image.Height(108);
        image.Stretch(Media::Stretch::Uniform);
        auto bitmap = Media::Imaging::BitmapImage();
        bitmap.UriSource(Windows::Foundation::Uri(
            L"ms-appx:///Assets/Square150x150Logo.scale-200.png"));
        image.Source(bitmap);
        brand.Children().Append(image);
        auto wordmark = Controls::StackPanel();
        wordmark.VerticalAlignment(VerticalAlignment::Center);
        auto nameRow = Controls::StackPanel();
        nameRow.Orientation(Controls::Orientation::Horizontal);
        nameRow.Children().Append(Label(L"Link", 39,
            Windows::UI::Text::FontWeights::Bold(), Brush(28, 32, 38)));
        nameRow.Children().Append(Label(L"EDA", 39,
            Windows::UI::Text::FontWeights::Bold(), Brush(0, 105, 235)));
        wordmark.Children().Append(nameRow);
        auto tagline = Label(L"Interactive exploratory data\nanalysis", 15,
            Windows::UI::Text::FontWeights::Normal(), Brush(91, 99, 115));
        tagline.Margin(Thickness{ 2, 8, 0, 0 }); wordmark.Children().Append(tagline);
        brand.Children().Append(wordmark);
        identity.Children().Append(brand);

        auto previews = Controls::StackPanel(); previews.Orientation(Controls::Orientation::Horizontal);
        previews.Spacing(8); previews.HorizontalAlignment(HorizontalAlignment::Center);
        previews.VerticalAlignment(VerticalAlignment::Bottom);
        previews.Children().Append(PlotPreview(0)); previews.Children().Append(PlotPreview(1));
        previews.Children().Append(PlotPreview(2)); Controls::Grid::SetRow(previews, 2);
        identity.Children().Append(previews);

        connection_ = Label(L"Connected to R", 13,
            Windows::UI::Text::FontWeights::SemiBold(), Brush(25, 132, 76));
        connection_.VerticalAlignment(VerticalAlignment::Bottom);
        Controls::Grid::SetRow(connection_, 3);
        identity.Children().Append(connection_);
        auto author = Label(L"Pedro Valero-Mora · University of Valencia", 12.5,
            Windows::UI::Text::FontWeights::Normal(), Brush(91, 96, 105));
        author.VerticalAlignment(VerticalAlignment::Bottom); Controls::Grid::SetRow(author, 4);
        identity.Children().Append(author);
        version_ = Label(L"Version", 11,
            Windows::UI::Text::FontWeights::Normal(), Brush(98, 103, 110));
        version_.VerticalAlignment(VerticalAlignment::Bottom);
        Controls::Grid::SetRow(version_, 5);
        identity.Children().Append(version_);
        auto waves = Controls::Canvas(); waves.Height(82); waves.VerticalAlignment(VerticalAlignment::Bottom);
        waves.IsHitTestVisible(false); Controls::Grid::SetRowSpan(waves, 6);
        auto waveBack = Shapes::Ellipse(); waveBack.Width(500); waveBack.Height(108);
        waveBack.Fill(Brush(218, 231, 253)); Controls::Canvas::SetLeft(waveBack, -80);
        Controls::Canvas::SetTop(waveBack, 34); waves.Children().Append(waveBack);
        auto waveFront = Shapes::Ellipse(); waveFront.Width(470); waveFront.Height(92);
        waveFront.Fill(Brush(202, 220, 250)); Controls::Canvas::SetLeft(waveFront, -45);
        Controls::Canvas::SetTop(waveFront, 52); waves.Children().Append(waveFront);
        identity.Children().InsertAt(0, waves);
        root.Children().Append(identity);

        auto separator = Controls::Border();
        separator.Width(1);
        separator.Background(Brush(222, 226, 230));
        separator.HorizontalAlignment(HorizontalAlignment::Right);
        Controls::Grid::SetColumn(separator, 0);
        root.Children().Append(separator);

        auto actions = Controls::StackPanel();
        actions.Padding(Thickness{ 42, 38, 42, 26 });
        actions.Spacing(9);
        Controls::Grid::SetColumn(actions, 1);
        actions.Children().Append(Label(L"Start with data", 28,
            Windows::UI::Text::FontWeights::Bold(), Brush(29, 32, 37)));
        auto description = Label(L"Choose a source to begin exploring.", 14,
            Windows::UI::Text::FontWeights::Normal(), Brush(80, 84, 90));
        description.Margin(Thickness{ 0, 0, 0, 12 });
        actions.Children().Append(description);

        chooseR_ = ActionButton(L"Choose a data frame from R…",
                                Controls::Symbol::ViewAll, true);
        chooseR_.Click([this](auto const&, auto const&)
        {
            if (chooseRCallback_) chooseRCallback_();
        });
        actions.Children().Append(chooseR_);
        openDocument_ = ActionButton(L"Open Data...",
                                     Controls::Symbol::OpenFile, false);
        openDocument_.Click([this](auto const&, auto const&)
        {
            if (openDocumentCallback_) openDocumentCallback_();
        });
        openFile_ = ActionButton(L"Import a data file…",
                                 Controls::Symbol::OpenFile, false);
        openFile_.Click([this](auto const&, auto const&)
        {
            if (openFileCallback_) openFileCallback_();
        });
        actions.Children().Append(openFile_);

        auto recentHeading = Label(L"Recent", 13,
            Windows::UI::Text::FontWeights::SemiBold(), Brush(42, 45, 50));
        recentHeading.Margin(Thickness{ 0, 10, 0, 0 });
        actions.Children().Append(recentHeading);
        recentPanel_ = Controls::StackPanel();
        recentPanel_.MinHeight(40);
        recentPanel_.MaxHeight(154);
        recentPanel_.Spacing(4);
        actions.Children().Append(recentPanel_);

        auto examplesHeading = Label(L"Example datasets", 13,
            Windows::UI::Text::FontWeights::SemiBold(), Brush(42, 45, 50));
        examplesHeading.Margin(Thickness{ 0, 7, 0, 0 }); actions.Children().Append(examplesHeading);
        examplesButton_ = ActionButton(L"Open a statistical example…", Controls::Symbol::List, false);
        examplesButton_.Click([this](auto const&, auto const&)
        {
            if (exampleCallback_) exampleCallback_("");
        });
        actions.Children().Append(examplesButton_);

        auto links = Controls::Grid(); links.ColumnSpacing(12); links.Margin(Thickness{ 0, 10, 0, 0 });
        for (int index = 0; index < 2; ++index)
        { auto column = Controls::ColumnDefinition(); column.Width(GridLengthHelper::FromValueAndType(1, GridUnitType::Star)); links.ColumnDefinitions().Append(column); }
        auto documentation = ActionButton(L"Documentation", Controls::Symbol::Help, false);
        documentation.Click([this](auto const&, auto const&)
        {
            if (documentationCallback_) documentationCallback_();
        });
        auto about = ActionButton(L"About LinkEDA", Controls::Symbol::Contact, false);
        about.Click([this](auto const&, auto const&)
        {
            if (aboutCallback_) aboutCallback_();
        });
        Controls::Grid::SetColumn(about, 1); links.Children().Append(documentation); links.Children().Append(about);
        actions.Children().Append(links);
        root.Children().Append(actions);

        window_.Content(root);
        ResizeLogical(window_, 900, 620);
        if (auto presenter = window_.AppWindow().Presenter().try_as<
                Microsoft::UI::Windowing::OverlappedPresenter>())
        {
            presenter.IsResizable(false);
            presenter.IsMaximizable(false);
        }
    }

    void WelcomeWindow::AttachLifetime()
    {
        std::weak_ptr<WelcomeWindow> weak = shared_from_this();
        window_.Closed([weak](auto const&, auto const&)
        {
            if (auto view = weak.lock())
            {
                view->closed_ = true;
                if (view->closedCallback_) view->closedCallback_();
            }
        });
    }

    void WelcomeWindow::SetClosedCallback(Action callback)
    {
        closedCallback_ = std::move(callback);
    }

    void WelcomeWindow::SetChooseRDataFrameCallback(Action callback)
    {
        chooseRCallback_ = std::move(callback);
    }

    void WelcomeWindow::SetOpenFileCallback(Action callback)
    {
        openFileCallback_ = std::move(callback);
    }

    void WelcomeWindow::SetOpenDocumentCallback(Action callback)
    {
        openDocumentCallback_ = std::move(callback);
    }

    void WelcomeWindow::SetRecentFileCallback(ValueAction callback)
    {
        recentFileCallback_ = std::move(callback);
    }

    void WelcomeWindow::SetExampleCallback(ValueAction callback)
    {
        exampleCallback_ = std::move(callback);
    }

    void WelcomeWindow::SetDocumentationCallback(Action callback)
    {
        documentationCallback_ = std::move(callback);
    }

    void WelcomeWindow::SetAboutCallback(Action callback)
    {
        aboutCallback_ = std::move(callback);
    }

    void WelcomeWindow::Show(::rlispstat::core::WelcomeWindowModel const& model)
    {
        window_.Title(to_hstring(model.strings.windowTitle));
        version_.Text(to_hstring(model.strings.versionPrefix + " " + model.version));
        connection_.Text(to_hstring(std::string("\xE2\x97\x8F  ") +
            (model.capabilities.connectedToExistingRSession
                ? model.strings.connectedToR : model.strings.connectionUnavailable)));
        connection_.Foreground(model.capabilities.connectedToExistingRSession
            ? Brush(25, 132, 76) : Brush(181, 105, 18));
        chooseR_.IsEnabled(model.capabilities.canChooseRDataFrame);
        if (auto label = openDocument_.Content().try_as<Controls::StackPanel>())
            label.Children().GetAt(1).as<Controls::TextBlock>().Text(
                to_hstring(model.strings.openDataDocument));
        if (auto label = openFile_.Content().try_as<Controls::StackPanel>())
            label.Children().GetAt(1).as<Controls::TextBlock>().Text(
                to_hstring(model.strings.openDataFile));
        examplesButton_.IsEnabled(model.capabilities.canOpenExamples);
        recentPanel_.Children().Clear();
        if (model.recentItems.empty())
        {
            recentPanel_.Children().Append(Label(to_hstring(model.strings.noRecentItems), 12,
                Windows::UI::Text::FontWeights::Normal(), Brush(100, 104, 110)));
        }
        else
        {
            const auto shown = std::min<std::size_t>(5, model.recentItems.size());
            for (std::size_t index = 0; index < shown; ++index)
            {
                auto const& recent = model.recentItems[index];
                auto button = Controls::Button();
                button.Height(27); button.Padding(Thickness{ 9, 0, 9, 0 });
                button.CornerRadius(CornerRadius{ 13 }); button.BorderThickness(Thickness{ 0 });
                button.Background(Brush(224, 225, 227));
                button.HorizontalAlignment(HorizontalAlignment::Stretch);
                button.HorizontalContentAlignment(HorizontalAlignment::Stretch);
                auto content = Controls::Grid();
                auto nameColumn = Controls::ColumnDefinition();
                nameColumn.Width(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
                content.ColumnDefinitions().Append(nameColumn);
                auto typeColumn = Controls::ColumnDefinition(); typeColumn.Width(GridLengthHelper::Auto());
                content.ColumnDefinitions().Append(typeColumn);
                auto name = Label(to_hstring(recent.displayName), 12.5,
                    Windows::UI::Text::FontWeights::SemiBold(), Brush(82, 84, 88));
                name.TextTrimming(TextTrimming::CharacterEllipsis); name.TextWrapping(TextWrapping::NoWrap);
                content.Children().Append(name);
                std::string typeLabel = recent.typeLabel;
                if (!typeLabel.empty() && typeLabel.front() == '.') typeLabel.erase(typeLabel.begin());
                std::transform(typeLabel.begin(), typeLabel.end(), typeLabel.begin(),
                    [](unsigned char value) { return static_cast<char>(std::toupper(value)); });
                auto type = Label(to_hstring(typeLabel), 12,
                    Windows::UI::Text::FontWeights::SemiBold(), Brush(95, 97, 101));
                Controls::Grid::SetColumn(type, 1); content.Children().Append(type);
                button.Content(content);
                button.IsEnabled(recent.exists);
                Controls::ToolTipService::SetToolTip(
                    button, box_value(to_hstring(recent.path)));
                button.Click([this, path = recent.path](auto const&, auto const&)
                {
                    if (recentFileCallback_) recentFileCallback_(path);
                });
                recentPanel_.Children().Append(button);
            }
        }
        // Hide() keeps this XAML window alive while hiding its HWND, so show
        // the AppWindow explicitly before activating it again. This is
        // especially visible when LinkEDA() is called again from the same R
        // session: the command succeeds, but the existing Welcome window
        // would otherwise stay invisible.
        window_.AppWindow().Show();
        window_.Activate();
    }

    void WelcomeWindow::SetStatus(std::string const& text, bool error)
    {
        connection_.Text(to_hstring(text));
        connection_.Foreground(error ? Brush(176, 48, 45) : Brush(0, 103, 192));
    }

    void WelcomeWindow::Hide()
    {
        if (!closed_) ShowWindow(WindowHandle(window_), SW_HIDE);
    }

    void WelcomeWindow::Activate()
    {
        if (!closed_)
        {
            window_.AppWindow().Show();
            window_.Activate();
        }
    }

    HWND WelcomeWindow::NativeHandle() const
    {
        return WindowHandle(window_);
    }

    std::shared_ptr<RDataFrameChooserWindow> RDataFrameChooserWindow::Create(
        Window const& owner,
        std::string requestId,
        std::vector<std::pair<std::string, std::string>> items,
        std::string windowTitle,
        std::string heading,
        std::string instruction,
        bool showItemId)
    {
        auto view = std::shared_ptr<RDataFrameChooserWindow>(
            new RDataFrameChooserWindow(owner, std::move(requestId), std::move(items),
                std::move(windowTitle), std::move(heading), std::move(instruction),
                showItemId));
        view->AttachLifetime();
        return view;
    }

    RDataFrameChooserWindow::RDataFrameChooserWindow(
        Window const& owner,
        std::string requestId,
        std::vector<std::pair<std::string, std::string>> items,
        std::string windowTitle,
        std::string heading,
        std::string instruction,
        bool showItemId)
        : owner_(owner), requestId_(std::move(requestId)), items_(std::move(items)),
          windowTitle_(std::move(windowTitle)), heading_(std::move(heading)),
          instruction_(std::move(instruction)), showItemId_(showItemId)
    {
        Initialize();
    }

    void RDataFrameChooserWindow::Initialize()
    {
        constexpr double dialogBodyFontSize = 12.0;
        constexpr double dialogTitleFontSize = 16.0;
        window_ = CreateLinkEDAWindow();
        window_.Title(to_hstring(windowTitle_));
        auto root = Controls::Grid();
        root.Padding(Thickness{ 20, 16, 20, 16 });
        root.RowDefinitions().Append(Controls::RowDefinition());
        root.RowDefinitions().GetAt(0).Height(GridLengthHelper::Auto());
        root.RowDefinitions().Append(Controls::RowDefinition());
        root.RowDefinitions().GetAt(1).Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
        root.RowDefinitions().Append(Controls::RowDefinition());
        root.RowDefinitions().GetAt(2).Height(GridLengthHelper::Auto());
        auto headingPanel = Controls::StackPanel(); headingPanel.Spacing(5);
        auto title = Label(to_hstring(heading_), dialogTitleFontSize,
            Windows::UI::Text::FontWeights::SemiBold(), Brush(28, 30, 34));
        headingPanel.Children().Append(title);
        auto heading = Label(to_hstring(instruction_), dialogBodyFontSize,
            Windows::UI::Text::FontWeights::Normal(), Brush(70, 74, 80));
        heading.TextWrapping(TextWrapping::Wrap); headingPanel.Children().Append(heading);
        root.Children().Append(headingPanel);
        list_ = Controls::ListView();
        list_.FontSize(dialogBodyFontSize);
        list_.Resources().Insert(box_value(L"ListViewItemMinHeight"), box_value(24.0));
        list_.SelectionMode(Controls::ListViewSelectionMode::Single);
        list_.HorizontalContentAlignment(HorizontalAlignment::Stretch);
        list_.Margin(Thickness{ 0, 14, 0, 14 });
        Controls::Grid::SetRow(list_, 1);
        for (auto const& item : items_)
        {
            auto text = to_hstring(showItemId_ ? item.first + "    " + item.second
                                                : item.second);
            if (requestId_ == "welcome-examples")
            {
                auto label = Controls::TextBlock();
                label.Text(text);
                label.TextWrapping(TextWrapping::NoWrap);
                label.TextTrimming(TextTrimming::CharacterEllipsis);
                if (auto const* example = ::rlispstat::core::FindWelcomeExample(item.first))
                    Controls::ToolTipService::SetToolTip(
                        label, box_value(to_hstring(example->description)));
                list_.Items().Append(label);
            }
            else
                list_.Items().Append(box_value(text));
        }
        if (!items_.empty()) list_.SelectedIndex(0);
        list_.DoubleTapped([this](auto const&, auto const&)
        {
            Complete(false);
        });
        root.Children().Append(list_);
        auto actions = Controls::StackPanel();
        actions.Orientation(Controls::Orientation::Horizontal);
        actions.HorizontalAlignment(HorizontalAlignment::Right); actions.Spacing(8);
        Controls::Grid::SetRow(actions, 2);
        auto cancel = Controls::Button(); cancel.Content(box_value(L"Cancel"));
        cancel.MinWidth(92); cancel.Click([this](auto const&, auto const&) { Complete(true); });
        auto open = Controls::Button(); open.Content(box_value(L"Open"));
        open.MinWidth(92); open.IsEnabled(!items_.empty());
        open.Click([this](auto const&, auto const&) { Complete(false); });
        actions.Children().Append(cancel); actions.Children().Append(open);
        root.Children().Append(actions);
        window_.Content(root);
        ResizeLogical(window_, requestId_ == "welcome-examples" ? 720 : 470,
                      requestId_ == "welcome-examples" ? 540 : 365);
        const HWND ownerHandle = owner_ ? WindowHandle(owner_) : nullptr;
        const HWND chooserHandle = WindowHandle(window_);
        if (ownerHandle && chooserHandle) SetWindowLongPtrW(chooserHandle, GWLP_HWNDPARENT,
                                                            reinterpret_cast<LONG_PTR>(ownerHandle));
    }

    void RDataFrameChooserWindow::AttachLifetime()
    {
        std::weak_ptr<RDataFrameChooserWindow> weak = shared_from_this();
        window_.Closed([weak](auto const&, auto const&)
        {
            if (auto view = weak.lock())
            {
                if (!view->completed_ && view->completedCallback_)
                    view->completedCallback_(view->requestId_, {}, true);
                view->completed_ = true;
            }
        });
    }

    void RDataFrameChooserWindow::SetCompletedCallback(Completed callback)
    {
        completedCallback_ = std::move(callback);
    }

    void RDataFrameChooserWindow::Show()
    {
        window_.Activate();
        if (requestId_ == "welcome-examples")
            CenterOnOwnerMonitor(window_, owner_);
        list_.Focus(FocusState::Programmatic);
    }

    void RDataFrameChooserWindow::Complete(bool cancelled)
    {
        if (completed_) return;
        const int index = list_.SelectedIndex();
        if (!cancelled && (index < 0 || static_cast<std::size_t>(index) >= items_.size()))
            return;
        completed_ = true;
        if (completedCallback_)
            completedCallback_(requestId_, cancelled ? std::string{} : items_[index].first,
                               cancelled);
        window_.Close();
    }
}
