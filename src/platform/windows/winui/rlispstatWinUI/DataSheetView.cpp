#include "pch.h"
#include "DataSheetView.h"
#include "PerformanceTrace.h"
#include "WindowBranding.h"

#include "../../../../core/format_model.h"
#include "../../../../core/command_model.h"
#include "../../../../core/export_model.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <iterator>
#include <map>
#include <string>

using namespace winrt;
using namespace Microsoft::UI::Xaml;

namespace
{
    Media::SolidColorBrush Brush(uint8_t alpha, uint8_t red, uint8_t green, uint8_t blue)
    {
        // A realized data sheet contains hundreds of cells but only a small,
        // finite set of colours. Reusing brushes avoids creating an equivalent
        // composition resource for every TextBlock and Border.
        const uint32_t key = (static_cast<uint32_t>(alpha) << 24) |
            (static_cast<uint32_t>(red) << 16) |
            (static_cast<uint32_t>(green) << 8) |
            static_cast<uint32_t>(blue);
        thread_local std::map<uint32_t, Media::SolidColorBrush> brushes;
        if (auto found = brushes.find(key); found != brushes.end())
            return found->second;
        auto brush = Media::SolidColorBrush(
            winrt::Windows::UI::ColorHelper::FromArgb(alpha, red, green, blue));
        brushes.emplace(key, brush);
        return brush;
    }

    Media::SolidColorBrush Brush(uint8_t red, uint8_t green, uint8_t blue)
    {
        return Brush(255, red, green, blue);
    }

    Media::SolidColorBrush NormalizedBrush(double red, double green, double blue)
    {
        auto channel = [](double value) {
            return static_cast<uint8_t>(std::clamp(std::lround(value * 255.0), 0L, 255L));
        };
        return Brush(channel(red), channel(green), channel(blue));
    }

    Controls::Border Cell(std::wstring const& text, double width, bool header)
    {
        auto label = Controls::TextBlock();
        label.Text(hstring(text));
        label.Padding(Thickness{ 7, 1, 7, 1 });
        label.FontSize(12);
        label.TextTrimming(TextTrimming::CharacterEllipsis);
        label.VerticalAlignment(VerticalAlignment::Center);
        // Data cells use an explicit light background.  Do not inherit the
        // app's dark-theme foreground or their text becomes white on white.
        label.Foreground(Brush(32, 35, 40));
        if (header) label.FontWeight(winrt::Windows::UI::Text::FontWeights::SemiBold());

        auto border = Controls::Border();
        border.Width(width);
        border.BorderBrush(Brush(226, 228, 231));
        border.BorderThickness(Thickness{ 0, 0, 1, header ? 1.0 : 0.0 });
        border.MinHeight(header ? 25.0 : 24.0);
        if (header) border.Background(Brush(247, 248, 249));
        border.Child(label);
        return border;
    }

    std::wstring DisplayValue(::rlispstat::core::DataFrameModel const& dataframe,
                              ::rlispstat::core::DataColumn const& column,
                              std::size_t row)
    {
        return to_hstring(::rlispstat::core::DisplayValueForDataFrameCell(
            dataframe, column, row)).c_str();
    }

    Controls::ScrollViewer FindScrollViewer(DependencyObject const& parent)
    {
        const int children = Media::VisualTreeHelper::GetChildrenCount(parent);
        for (int index = 0; index < children; ++index)
        {
            auto child = Media::VisualTreeHelper::GetChild(parent, index);
            if (auto scroll = child.try_as<Controls::ScrollViewer>()) return scroll;
            if (auto nested = FindScrollViewer(child)) return nested;
        }
        return nullptr;
    }
}

namespace winrt::rlispstatWinUI::implementation
{
    std::shared_ptr<DataSheetView> DataSheetView::Create()
    {
        auto view = std::shared_ptr<DataSheetView>(new DataSheetView());
        view->AttachClosedHandler();
        return view;
    }

    DataSheetView::DataSheetView()
    {
        Initialize();
    }

    void DataSheetView::Initialize()
    {
        window_ = CreateLinkEDAWindow();
        window_.Title(L"LinkEDA data sheet");

        auto windowRoot = Controls::Grid();
        windowRoot.RequestedTheme(ElementTheme::Light);
        auto titleRow = Controls::RowDefinition();
        titleRow.Height(GridLengthHelper::FromPixels(38));
        windowRoot.RowDefinitions().Append(titleRow);
        auto contentRow = Controls::RowDefinition();
        contentRow.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
        windowRoot.RowDefinitions().Append(contentRow);
        auto statusRow = Controls::RowDefinition();
        statusRow.Height(GridLengthHelper::FromPixels(26));
        windowRoot.RowDefinitions().Append(statusRow);
        applicationMenu_ = DataSheetMenuHost::Create();
        Controls::Grid::SetRow(applicationMenu_->Element(), 0);
        Controls::Canvas::SetZIndex(applicationMenu_->Element(), 10);
        windowRoot.Children().Append(applicationMenu_->Element());

        // The data sheet is deliberately light even when the application or
        // Windows uses a dark theme.  Spreadsheet chrome must remain compact
        // and readable instead of inheriting black gutters and white text.
        auto page = Controls::Border();
        page.Background(Brush(255, 255, 255));
        page.Padding(Thickness{ 10, 6, 10, 0 });
        page.RequestedTheme(ElementTheme::Light);
        rowsView_ = Controls::ListView();
        rowsView_.Background(Brush(255, 255, 255));
        rowsView_.SelectionMode(Controls::ListViewSelectionMode::Extended);
        rowsView_.IsMultiSelectCheckBoxEnabled(false);
        // Keep the sheet hit-testable even while its top-level window is
        // inactive. Disabling hit testing here discards the first cell or
        // context-menu click used to reactivate the window. PointerOver is
        // made visually inert by the transparent resources below, and any
        // retained state is cleared in the Window.Activated handler.
        rowsView_.HorizontalContentAlignment(HorizontalAlignment::Left);
        // ContainerContentChanging is intended to update the visual tree
        // created by an item template.  Replacing ListViewItem::Content there
        // also replaces the data item that owns the container and leaves the
        // generated presenter empty on WinUI 3.  Keep a tiny reusable template
        // root and put each realized row panel inside that host instead.
        rowsView_.ItemTemplate(Markup::XamlReader::Load(LR"(
            <DataTemplate xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation">
                <ContentControl HorizontalContentAlignment="Left"
                                VerticalContentAlignment="Stretch" />
            </DataTemplate>)").as<DataTemplate>());
        rowsView_.Resources().Insert(box_value(L"ListViewItemBackgroundSelected"),
                                     Brush(0, 0, 0, 0));
        rowsView_.Resources().Insert(box_value(L"ListViewItemBackgroundPointerOver"),
                                     Brush(0, 0, 0, 0));
        rowsView_.Resources().Insert(box_value(L"ListViewItemBackgroundPressed"),
                                     Brush(0, 0, 0, 0));
        rowsView_.Resources().Insert(box_value(L"ListViewItemBackgroundSelectedPointerOver"),
                                     Brush(0, 0, 0, 0));
        rowsView_.Resources().Insert(box_value(L"ListViewItemBackgroundSelectedPressed"),
                                     Brush(0, 0, 0, 0));
        rowsView_.Resources().Insert(box_value(L"ListViewItemForegroundSelected"),
                                     Brush(25, 25, 25));
        rowsView_.Resources().Insert(box_value(L"ListViewItemSelectionIndicatorBrush"),
                                     Brush(0, 0, 0, 0));
        Controls::ScrollViewer::SetHorizontalScrollBarVisibility(
            rowsView_, Controls::ScrollBarVisibility::Auto);
        Controls::ScrollViewer::SetVerticalScrollBarVisibility(
            rowsView_, Controls::ScrollBarVisibility::Auto);
        // Scroll-bar visibility alone does not enable horizontal scrolling in
        // a WinUI ListView.  In particular, the default mode can measure every
        // row at the viewport width even though its cells extend much farther
        // to the right.  Enable both axes explicitly; the fixed common width
        // assigned in PrepareRowContainer then becomes the horizontal extent.
        Controls::ScrollViewer::SetHorizontalScrollMode(
            rowsView_, Controls::ScrollMode::Enabled);
        Controls::ScrollViewer::SetVerticalScrollMode(
            rowsView_, Controls::ScrollMode::Enabled);
        rowsView_.SelectionChanged([this](auto const&, auto const&)
        {
            SelectionChanged();
        });
        rowsView_.LayoutUpdated([this](auto const&, auto const&)
        {
            if (pendingCellEdit_) TryBeginPendingCellEdit();
        });
        rowsView_.ContainerContentChanging([this](
            auto const&, Controls::ContainerContentChangingEventArgs const& args)
        {
            auto item = args.ItemContainer().try_as<Controls::ListViewItem>();
            if (!item) return;
            const int previousRow = unbox_value_or<int>(item.Tag(), -1);
            if (previousRow > 0) realizedRows_.erase(previousRow);
            if (args.InRecycleQueue())
            {
                if (::rlispstat::windows::performance::Enabled())
                    ++diagnosticRecycledRows_;
                // Keep the row panel attached to its recycled container. The
                // next realization updates its TextBlocks in place instead of
                // destroying and recreating every cell.
                return;
            }
            const int row = unbox_value_or<int>(args.Item(), -1);
            const auto prepareStarted = std::chrono::steady_clock::now();
            PrepareRowContainer(item, row);
            if (::rlispstat::windows::performance::Enabled())
            {
                const double elapsed = std::chrono::duration<double, std::milli>(
                    std::chrono::steady_clock::now() - prepareStarted).count();
                ++diagnosticPreparedRows_;
                diagnosticPrepareMilliseconds_ += elapsed;
                diagnosticMaxPrepareMilliseconds_ = std::max(
                    diagnosticMaxPrepareMilliseconds_, elapsed);
            }
            args.Handled(true);
        });
        page.Child(rowsView_);
        Controls::Grid::SetRow(page, 1);
        windowRoot.Children().Append(page);

        auto statusBorder = Controls::Border();
        statusBorder.Background(Brush(250, 250, 250));
        statusBorder.BorderBrush(Brush(224, 224, 224));
        statusBorder.BorderThickness(Thickness{ 0, 1, 0, 0 });
        statusBorder.Padding(Thickness{ 10, 2, 10, 2 });
        status_ = Controls::TextBlock();
        status_.FontSize(11);
        status_.Foreground(Brush(62, 62, 62));
        status_.VerticalAlignment(VerticalAlignment::Center);
        statusBorder.Child(status_);
        Controls::Grid::SetRow(statusBorder, 2);
        windowRoot.Children().Append(statusBorder);
        if (::rlispstat::windows::performance::Enabled())
        {
            window_.AppWindow().Changed([this](
                auto const&, Microsoft::UI::Windowing::AppWindowChangedEventArgs const& args)
            {
                if (args.DidPositionChange()) ++diagnosticMoves_;
                if (args.DidSizeChange())
                {
                    ++diagnosticResizes_;
                    diagnosticResizePending_ = true;
                    diagnosticResizeStarted_ = std::chrono::steady_clock::now();
                }
            });
            windowRoot.LayoutUpdated([this](auto const&, auto const&)
            {
                ++diagnosticLayouts_;
                if (!diagnosticResizePending_) return;
                const double elapsed = std::chrono::duration<double, std::milli>(
                    std::chrono::steady_clock::now() - diagnosticResizeStarted_).count();
                diagnosticMaxResizeLayoutMilliseconds_ = std::max(
                    diagnosticMaxResizeLayoutMilliseconds_, elapsed);
                diagnosticResizePending_ = false;
            });
            diagnosticsTimer_ = windowRoot.DispatcherQueue().CreateTimer();
            diagnosticsTimer_.Interval(std::chrono::seconds(1));
            diagnosticsTimer_.Tick([this](auto const&, auto const&)
            {
                std::ostringstream detail;
                detail << "group=" << group_
                       << ",moves=" << diagnosticMoves_
                       << ",resizes=" << diagnosticResizes_
                       << ",layouts=" << diagnosticLayouts_
                       << ",prepared=" << diagnosticPreparedRows_
                       << ",recycled=" << diagnosticRecycledRows_
                       << ",realized=" << realizedRows_.size()
                       << ",prepare_total_ms=" << diagnosticPrepareMilliseconds_
                       << ",prepare_max_ms=" << diagnosticMaxPrepareMilliseconds_;
                ::rlispstat::windows::performance::Write(
                    "DataSheet.Activity", diagnosticMaxResizeLayoutMilliseconds_,
                    detail.str());
                diagnosticMoves_ = diagnosticResizes_ = diagnosticLayouts_ = 0;
                diagnosticPreparedRows_ = diagnosticRecycledRows_ = 0;
                diagnosticPrepareMilliseconds_ = 0.0;
                diagnosticMaxPrepareMilliseconds_ = 0.0;
                diagnosticMaxResizeLayoutMilliseconds_ = 0.0;
            });
            diagnosticsTimer_.Start();
        }
        windowRoot.AddHandler(UIElement::PointerPressedEvent(), Input::PointerEventHandler(
            [this](auto const&, Input::PointerRoutedEventArgs const&) {
                if (commandCallback_ && !group_.empty())
                    commandCallback_({"SET_ACTIVE_DATASET", group_});
            }), true);
        window_.Content(windowRoot);
        window_.Activated([this](auto const&, WindowActivatedEventArgs const& args)
        {
            const bool active = args.WindowActivationState() !=
                WindowActivationState::Deactivated;
            if (active && commandCallback_ && !group_.empty())
                commandCallback_({"SET_ACTIVE_DATASET", group_});
            if (!active)
            {
                // Clear any pointer-over visual retained by a recycled
                // ListViewItem, then restore semantic row/column colours.
                for (auto const& [row, item] : realizedRows_)
                    VisualStateManager::GoToState(item, L"Normal", false);
                ApplyRowStyles();
            }
        });
        ResizeLinkEDAWindowClient(window_, 1100, 720);
        // The data sheet is the stable workbench for the linked result
        // windows.  Keep it flush with the monitor's usable left edge instead
        // of letting Windows cascade or centre it differently after each R
        // session. Result windows may come to the foreground without moving
        // this base window.
        AlignLinkEDAWindowToWorkAreaLeft(window_);
    }

    void DataSheetView::AttachClosedHandler()
    {
        std::weak_ptr<DataSheetView> weak = shared_from_this();
        window_.Closed([weak](auto const&, auto const&)
        {
            if (auto view = weak.lock())
            {
                view->closed_ = true;
                if (view->diagnosticsTimer_) view->diagnosticsTimer_.Stop();
                if (view->closedCallback_) view->closedCallback_();
            }
        });
    }

    void DataSheetView::SetSelectionCallback(SelectionCallback callback)
    {
        selectionCallback_ = std::move(callback);
    }

    void DataSheetView::SetClosedCallback(ClosedCallback callback)
    {
        closedCallback_ = std::move(callback);
    }

    void DataSheetView::SetImputationDisplayCallback(
        ImputationDisplayCallback callback)
    {
        imputationDisplayCallback_ = std::move(callback);
    }

    void DataSheetView::SetCommandCallback(CommandCallback callback)
    {
        commandCallback_ = std::move(callback);
    }

    void DataSheetView::SetLabelColumn(std::string labelColumn)
    {
        labelColumn_ = std::move(labelColumn);
    }

    std::string DataSheetView::Dispatch(std::vector<std::string> const& command)
    {
        if (!commandCallback_) return "ERR command dispatcher is unavailable";
        const std::string reply = commandCallback_(command);
        status_.Text(to_hstring(reply.rfind("OK\t", 0) == 0 ? reply.substr(3) : reply));
        return reply;
    }

    void DataSheetView::SetApplicationMenu(
        std::shared_ptr<ApplicationCommandRegistry> registry, std::string token)
    {
        if (applicationMenuAttached_) return;
        applicationMenuAttached_ = true;
        applicationCommands_ = registry;
        auto registryForActivation = registry;
        auto tokenForActivation = token;
        applicationMenu_->SetRegistry(std::move(registry), std::move(token));
        std::weak_ptr<DataSheetView> weak = shared_from_this();
        window_.Activated([weak, registryForActivation, tokenForActivation](
            auto const&, WindowActivatedEventArgs const& args)
        {
            if (args.WindowActivationState() != WindowActivationState::Deactivated &&
                !weak.expired() && registryForActivation)
                registryForActivation->ActivateContext(tokenForActivation);
        });
    }

    void DataSheetView::Show(::rlispstat::core::DataFrameModel const& dataframe,
                             bool activate)
    {
        group_ = dataframe.group;
        const auto title = to_hstring("Data: " + group_);
        window_.Title(title);
        SetWindowSnapshotSource(window_, "data", group_, group_);
        ConfigureImputationDisplayMenu(dataframe);
        RebuildRows(dataframe);
        if (activate) Activate();
    }

    void DataSheetView::RefreshStructure(
        ::rlispstat::core::DataFrameModel const& dataframe,
        std::size_t preferredColumn)
    {
        RebuildRows(dataframe);
        if (preferredColumn < dataframe.columns.size()) SelectColumn(preferredColumn);
        // A ListView recycles its row presenters. A structural change must
        // invalidate their width immediately instead of waiting for a later
        // scroll or subset operation to produce the new column.
        rowsView_.InvalidateMeasure();
        rowsView_.InvalidateArrange();
        rowsView_.UpdateLayout();
        if (auto scroll = FindScrollViewer(rowsView_))
            scroll.ChangeView(scroll.ScrollableWidth(), nullptr, nullptr, true);
    }

    void DataSheetView::RefreshColumnMetadata(
        ::rlispstat::core::DataFrameModel const& dataframe,
        std::size_t column)
    {
        dataframe_ = &dataframe;
        rowCount_ = dataframe.rows;
        columnCount_ = dataframe.columns.size();
        if (column >= columnCount_) return;

        // Variable type, description and decimal changes do not alter the
        // table structure.  Re-preparing only the currently realised rows
        // updates the header, displayed values and cell flyouts while keeping
        // the ListView's item source, scroll anchor, focus and selection.
        // Rebuilding all rows here made a change initiated in an analysis
        // window visibly jump the data sheet behind it.
        std::vector<std::pair<int, Controls::ListViewItem>> realised;
        realised.reserve(realizedRows_.size());
        for (auto const& entry : realizedRows_) realised.push_back(entry);
        for (auto const& [row, item] : realised)
            if (item) PrepareRowContainer(item, row);
        ConfigureImputationDisplayMenu(dataframe);
        UpdateStatus();
    }

    void DataSheetView::RefreshCell(
        ::rlispstat::core::DataFrameModel const& dataframe,
        int row)
    {
        dataframe_ = &dataframe;
        rowCount_ = dataframe.rows;
        columnCount_ = dataframe.columns.size();
        auto realized = realizedRows_.find(row);
        if (realized != realizedRows_.end())
            PrepareRowContainer(realized->second, row);
        UpdateStatus();
    }

    void DataSheetView::SetAnalysisScope(::rlispstat::core::AnalysisScope scope)
    {
        analysisScope_ = std::move(scope);
        UpdateStatus();
        if (dataframe_) ConfigureImputationDisplayMenu(*dataframe_);
    }

    void DataSheetView::ConfigureImputationDisplayMenu(
        ::rlispstat::core::DataFrameModel const& dataframe)
    {
        rowsView_.ContextFlyout(BuildRowsContextMenu(dataframe));
    }

    void DataSheetView::AppendImputationDisplayItems(
        Controls::MenuFlyout const& menu,
        ::rlispstat::core::DataFrameModel const& dataframe)
    {
        if (dataframe.datasetType != "multiple_imputation" ||
            dataframe.imputationCount <= 0) return;

        menu.Items().Append(Controls::MenuFlyoutSeparator());
        auto display = Controls::MenuFlyoutSubItem();
        display.Text(L"Imputed Data Display");
        auto append = [this, &dataframe, &display](std::wstring const& title,
                                                   std::string value,
                                                   bool checked)
        {
            auto item = Controls::ToggleMenuFlyoutItem();
            item.Text(title);
            item.IsChecked(checked);
            item.Click([this, value = std::move(value)](auto const&, auto const&)
            {
                if (imputationDisplayCallback_ && !group_.empty())
                    imputationDisplayCallback_(group_, value);
            });
            display.Items().Append(item);
        };
        for (int version = 1; version <= dataframe.imputationCount; ++version)
        {
            append(to_hstring(::rlispstat::core::DataSheetImputationVersionTitle(
                       version)).c_str(),
                   "version:" + std::to_string(version),
                   dataframe.imputationDisplayMode == "version" &&
                       dataframe.activeImputationVersion == version);
        }
        display.Items().Append(Controls::MenuFlyoutSeparator());
        const auto titles = ::rlispstat::core::DefaultDataSheetContextMenuTitles();
        append(to_hstring(titles.allImputedValuesDisplay).c_str(), "all",
               dataframe.imputationDisplayMode == "all");
        append(to_hstring(titles.originalIncompleteDataDisplay).c_str(), "original",
               dataframe.imputationDisplayMode == "original");
        menu.Items().Append(display);
    }

    Controls::MenuFlyout DataSheetView::BuildRowsContextMenu(
        ::rlispstat::core::DataFrameModel const& dataframe)
    {
        auto menu = Controls::MenuFlyout();
        menu.AreOpenCloseAnimationsEnabled(false);
        AppendRowsContextItems(menu, dataframe, true);
        AppendImputationDisplayItems(menu, dataframe);
        AppendSnapshotAlbumContextItems(window_, menu);
        return menu;
    }

    void DataSheetView::AppendRowsContextItems(
        Controls::MenuFlyout const& menu,
        ::rlispstat::core::DataFrameModel const& dataframe,
        bool includeVariableView)
    {
        auto colors = Controls::MenuFlyoutSubItem();
        colors.Text(L"Color Selected Rows");
        for (auto const& color : ::rlispstat::core::PaletteColors())
        {
            auto item = Controls::MenuFlyoutItem();
            item.Text(to_hstring(color.name));
            item.Click([this, value = color.name](auto const&, auto const&)
            {
                if (!group_.empty()) Dispatch({ "SET_SELECTED_COLOR", group_, value });
            });
            colors.Items().Append(item);
        }
        menu.Items().Append(colors);

        auto clearSelected = Controls::MenuFlyoutItem();
        clearSelected.Text(L"Clear Row Color");
        clearSelected.Click([this](auto const&, auto const&)
        {
            std::vector<std::string> command{ "CLEAR_ROW_COLORS", group_,
                std::to_string(selectedRows_.size()) };
            for (int row : selectedRows_) command.push_back(std::to_string(row));
            Dispatch(command);
        });
        menu.Items().Append(clearSelected);
        auto clearAll = Controls::MenuFlyoutItem();
        clearAll.Text(L"Clear All Row Colors");
        clearAll.Click([this](auto const&, auto const&)
        {
            Dispatch({ "CLEAR_ROW_COLORS", group_ });
        });
        menu.Items().Append(clearAll);
        menu.Items().Append(Controls::MenuFlyoutSeparator());

        auto scope = Controls::MenuFlyoutSubItem();
        scope.Text(to_hstring(::rlispstat::core::GlobalAnalysisScopeMenuTitle()));
        auto selectedScope = Controls::ToggleMenuFlyoutItem();
        selectedScope.Text(to_hstring("Use Current Selection (" +
            std::to_string(selectedRows_.size()) + ")"));
        selectedScope.IsEnabled(!selectedRows_.empty());
        selectedScope.IsChecked(::rlispstat::core::AnalysisScopeTracksCurrentSelection(
            analysisScope_) && ::rlispstat::core::AnalysisScopeMatchesRows(
                analysisScope_, std::vector<int>(selectedRows_.begin(), selectedRows_.end())));
        selectedScope.Click([this](auto const&, auto const&)
        {
            Dispatch({ "SET_ANALYSIS_SCOPE_FROM_SELECTION", group_ });
        });
        scope.Items().Append(selectedScope);
        auto saveScope = Controls::MenuFlyoutItem();
        saveScope.Text(L"Save Current Selection as Scope...");
        saveScope.IsEnabled(!selectedRows_.empty());
        saveScope.Click([this](auto const&, auto const&)
        {
            Dispatch({ "SAVE_ANALYSIS_SCOPE_FROM_SELECTION", group_ });
        });
        scope.Items().Append(saveScope);
        if (applicationCommands_)
        {
            const auto saved = applicationCommands_->SavedSelections(group_);
            if (!saved.empty())
            {
                const auto activeName =
                    ::rlispstat::core::AnalysisScopeSelectionName(analysisScope_);
                for (auto const& selection : saved)
                {
                    auto item = Controls::ToggleMenuFlyoutItem();
                    item.Text(to_hstring("Saved: " + selection.name + " (" +
                        std::to_string(selection.originalRowIds.size()) + ")"));
                    item.IsChecked(activeName.has_value() &&
                                   *activeName == selection.name);
                    item.Click([this, name = selection.name](auto const&, auto const&)
                    {
                        Dispatch({ "USE_SAVED_ANALYSIS_SCOPE", group_, name });
                    });
                    scope.Items().Append(item);
                }
            }
        }
        auto allScope = Controls::ToggleMenuFlyoutItem();
        allScope.Text(to_hstring("All (" + std::to_string(std::max(0, dataframe.rows)) + ")"));
        allScope.IsChecked(analysisScope_.kind ==
            ::rlispstat::core::AnalysisScopeKind::AllObservations);
        allScope.Click([this](auto const&, auto const&)
        {
            Dispatch({ "SET_ANALYSIS_SCOPE_ALL", group_ });
        });
        scope.Items().Append(allScope);
        menu.Items().Append(scope);

        auto cases = Controls::MenuFlyoutSubItem();
        cases.Text(L"Cases");
        auto clearSelection = Controls::MenuFlyoutItem();
        clearSelection.Text(L"Clear Selection");
        clearSelection.Click([this](auto const&, auto const&)
        {
            Dispatch({ "CLEAR", group_ });
        });
        cases.Items().Append(clearSelection);
        auto selectAll = Controls::MenuFlyoutItem();
        selectAll.Text(L"Select All");
        selectAll.Click([this](auto const&, auto const&)
        {
            Dispatch({ "SELECT_ALL", group_ });
        });
        cases.Items().Append(selectAll);
        auto invert = Controls::MenuFlyoutItem();
        invert.Text(L"Invert Selection");
        invert.Click([this](auto const&, auto const&)
        {
            Dispatch({ "INVERT_SELECTION", group_ });
        });
        cases.Items().Append(invert);
        menu.Items().Append(cases);
        menu.Items().Append(Controls::MenuFlyoutSeparator());

        auto addColumn = Controls::MenuFlyoutSubItem();
        addColumn.Text(L"Add Column to Data");
        for (auto const& option : ::rlispstat::core::DataColumnCommandOptions(dataframe.group))
        {
            auto item = Controls::MenuFlyoutItem();
            item.Text(to_hstring(option.title));
            const std::string command = option.value == "selection"
                ? "DATA_ADD_SELECTION_COLUMN" : "DATA_ADD_POINT_COLOR_COLUMN";
            item.Click([this, command](auto const&, auto const&)
            {
                Dispatch({ command, group_ });
            });
            addColumn.Items().Append(item);
        }
        menu.Items().Append(addColumn);

        auto subset = Controls::MenuFlyoutItem();
        subset.Text(L"Make Subset from Selection");
        subset.Click([this](auto const&, auto const&)
        {
            Dispatch({ "DATA_MAKE_SUBSET_FROM_SELECTION", group_ });
        });
        menu.Items().Append(subset);

        if (includeVariableView)
        {
            auto variables = Controls::MenuFlyoutItem();
            variables.Text(L"Variable View");
            variables.Click([this](auto const&, auto const&)
            {
                Dispatch({ "DATA_VARIABLE_VIEW", group_ });
            });
            menu.Items().Append(variables);

            auto exportMenu = Controls::MenuFlyoutSubItem();
            exportMenu.Text(L"Export");
            AppendRCodeExportItems(exportMenu, "dataset", group_);
            menu.Items().Append(exportMenu);
        }
    }

    void DataSheetView::AppendRCodeExportItems(
        Controls::MenuFlyoutSubItem const& exportMenu,
        std::string const& objectKind,
        std::string const& objectId,
        std::string const& column)
    {
        auto rCode = Controls::MenuFlyoutSubItem();
        rCode.Text(L"R Code");
        for (auto const& action : ::rlispstat::core::BuildRCodeExportMenuActions(true, false))
        {
            auto item = Controls::MenuFlyoutItem();
            item.Text(to_hstring(action.title));
            item.Click([this, command = action.command, objectKind, objectId, column]
                (auto const&, auto const&)
            {
                std::vector<std::string> request{ command, objectKind, objectId };
                if (!column.empty()) request.push_back(column);
                Dispatch(request);
            });
            rCode.Items().Append(item);
        }
        exportMenu.Items().Append(rCode);
    }

    Controls::MenuFlyout DataSheetView::BuildColumnContextMenu(
        ::rlispstat::core::DataColumn const& column,
        std::size_t columnIndex,
        bool includeRowActions)
    {
        ::rlispstat::windows::performance::Scope timing(
            "DataSheet.BuildColumnContextMenu",
            "column=" + std::to_string(columnIndex) +
                ",rows=" + (includeRowActions ? "true" : "false"));
        auto menu = Controls::MenuFlyout();
        menu.AreOpenCloseAnimationsEnabled(false);
        menu.Opening([this, columnIndex](auto const&, auto const&)
        {
            SelectColumn(columnIndex);
        });
        if (includeRowActions && dataframe_)
        {
            AppendRowsContextItems(menu, *dataframe_, false);
            AppendImputationDisplayItems(menu, *dataframe_);
            menu.Items().Append(Controls::MenuFlyoutSeparator());
        }
        auto information = Controls::MenuFlyoutItem();
        information.Text(L"Variable Information");
        const std::string details = dataframe_
            ? ::rlispstat::core::VariableInformationText(*dataframe_, column.name)
            : std::string{};
        information.Click([details](auto const&, auto const&)
        {
            MessageBoxW(nullptr, to_hstring(details).c_str(), L"Variable Information",
                        MB_OK | MB_ICONINFORMATION);
        });
        menu.Items().Append(information);

        auto rename = Controls::MenuFlyoutItem();
        rename.Text(L"Rename Variable...");
        rename.Click([this, variable = column.name](auto const&, auto const&)
        {
            ShowTextEditor(L"Rename Variable", L"Enter a unique variable name.",
                to_hstring(variable).c_str(),
                { "RENAME_VARIABLE", group_, variable });
        });
        menu.Items().Append(rename);

        auto editDescription = Controls::MenuFlyoutItem();
        editDescription.Text(L"Edit Description...");
        editDescription.Click([this, variable = column.name,
                               initial = column.description](auto const&, auto const&)
        {
            ShowTextEditor(to_hstring("Description for " + variable).c_str(),
                to_hstring(::rlispstat::core::VariableDescriptionDialogInformationText()).c_str(),
                to_hstring(initial).c_str(),
                { "SET_VARIABLE_DESCRIPTION", group_, variable }, true);
        });
        menu.Items().Append(editDescription);
        menu.Items().Append(Controls::MenuFlyoutSeparator());

        if (::rlispstat::core::DefaultVariableRolesExperimentalFeatureEnabled())
        {
            auto role = Controls::MenuFlyoutSubItem();
            role.Text(to_hstring(::rlispstat::core::FormatRoleColumnHeader()));
            for (auto const& option : ::rlispstat::core::VariableViewRoleOptions())
            {
                auto item = Controls::MenuFlyoutItem();
                item.Text(to_hstring(option.title));
                item.Click([this, selectedRole = option.value,
                            variable = column.name](auto const&, auto const&)
                {
                    Dispatch({ "SET_DEFAULT_VARIABLE_ROLE", group_, variable, selectedRole });
                });
                role.Items().Append(item);
            }
            menu.Items().Append(role);
        }

        auto type = Controls::MenuFlyoutSubItem();
        type.Text(L"Type");
        Controls::RadioMenuFlyoutItem::SetAreCheckStatesEnabled(type, true);
        std::string currentType = ::rlispstat::core::NormalizeVariableType(column.type);
        if (currentType == "logical") currentType = "factor";
        for (auto const& option : ::rlispstat::core::VariableViewTypeOptions())
        {
            auto item = Controls::RadioMenuFlyoutItem();
            item.GroupName(to_hstring("column-type:" + group_ + ":" + column.name));
            item.Text(to_hstring(option.title));
            item.IsChecked(currentType == option.value);
            item.Click([this, value = option.value, variable = column.name](auto const&, auto const&)
            {
                Dispatch({ "SET_VARIABLE_TYPE", group_, variable, value });
            });
            type.Items().Append(item);
        }
        menu.Items().Append(type);

        auto decimals = Controls::MenuFlyoutItem();
        decimals.Text(to_hstring(column.decimals < 0 ? "Decimals: Automatic..." :
            "Decimals: " + std::to_string(column.decimals) + "...").c_str());
        decimals.IsEnabled(currentType == "numeric");
        decimals.Click([this, column](auto const&, auto const&)
        {
            ShowDecimalsEditor(column);
        });
        menu.Items().Append(decimals);
        menu.Items().Append(Controls::MenuFlyoutSeparator());

        auto labelColumn = Controls::ToggleMenuFlyoutItem();
        labelColumn.Text(L"Use as Point Label Column");
        labelColumn.IsChecked(labelColumn_ == column.name);
        labelColumn.Click([this, variable = column.name](auto const&, auto const&)
        {
            const std::string next = labelColumn_ == variable ? std::string{} : variable;
            const std::string reply = Dispatch({ "SET_LABEL_COLUMN", group_, next });
            if (reply.rfind("ERR ", 0) != 0) labelColumn_ = next;
        });
        menu.Items().Append(labelColumn);

        auto annotate = Controls::MenuFlyoutSubItem();
        annotate.Text(note_.has_content ? L"Annotate \u2022" : L"Annotate");
        auto editNote = Controls::MenuFlyoutItem();
        editNote.Text(note_.has_content ? L"Edit Notes..." : L"Add Notes...");
        editNote.Click([this](auto const&, auto const&) { ShowAnnotationEditor(); });
        annotate.Items().Append(editNote);
        auto clearNote = Controls::MenuFlyoutItem();
        clearNote.Text(L"Clear Notes");
        clearNote.IsEnabled(note_.has_content);
        clearNote.Click([this](auto const&, auto const&)
        {
            ::rlispstat::core::ClearWindowNote(note_);
            if (noteChangedCallback_) noteChangedCallback_(group_, std::nullopt);
        });
        annotate.Items().Append(clearNote);
        menu.Items().Append(annotate);

        auto variableView = Controls::MenuFlyoutItem();
        variableView.Text(L"Open Variable View");
        variableView.Click([this](auto const&, auto const&)
        {
            Dispatch({ "DATA_VARIABLE_VIEW", group_ });
        });
        menu.Items().Append(variableView);

        auto exportMenu = Controls::MenuFlyoutSubItem();
        exportMenu.Text(L"Export");
        AppendRCodeExportItems(exportMenu, "column", group_, column.name);
        menu.Items().Append(exportMenu);
        AppendSnapshotAlbumContextItems(window_, menu);
        return menu;
    }

    void DataSheetView::ShowTextEditor(
        std::wstring const& title,
        std::wstring const& information,
        std::wstring const& initialValue,
        std::vector<std::string> commandPrefix,
        bool multiline)
    {
        if (!window_ || !window_.Content() || !window_.Content().XamlRoot()) return;
        editorDialog_ = Controls::ContentDialog();
        editorDialog_.XamlRoot(window_.Content().XamlRoot());
        editorDialog_.Title(box_value(hstring(title)));
        editorDialog_.PrimaryButtonText(L"Apply");
        editorDialog_.CloseButtonText(L"Cancel");
        editorDialog_.DefaultButton(Controls::ContentDialogButton::Primary);
        auto root = Controls::StackPanel();
        root.Spacing(10);
        auto help = Controls::TextBlock();
        help.Text(hstring(information)); help.FontSize(12); help.TextWrapping(TextWrapping::Wrap);
        root.Children().Append(help);
        auto field = Controls::TextBox();
        field.Text(hstring(initialValue)); field.FontSize(12);
        field.AcceptsReturn(multiline); field.TextWrapping(TextWrapping::Wrap);
        if (multiline) field.MinHeight(96);
        root.Children().Append(field);
        editorDialog_.Content(root);
        editorDialog_.PrimaryButtonClick(
            [this, field, commandPrefix = std::move(commandPrefix)](
                auto const&, Controls::ContentDialogButtonClickEventArgs const& args) mutable
            {
                commandPrefix.push_back(to_string(field.Text()));
                const std::string reply = Dispatch(commandPrefix);
                if (reply.rfind("ERR ", 0) == 0) args.Cancel(true);
            });
        editorDialog_.ShowAsync();
        field.SelectAll();
    }

    void DataSheetView::ShowDecimalsEditor(::rlispstat::core::DataColumn const& column)
    {
        if (!window_ || !window_.Content() || !window_.Content().XamlRoot()) return;
        editorDialog_ = Controls::ContentDialog();
        editorDialog_.XamlRoot(window_.Content().XamlRoot());
        editorDialog_.Title(box_value(to_hstring("Decimals for " + column.name)));
        editorDialog_.PrimaryButtonText(L"Apply");
        editorDialog_.CloseButtonText(L"Cancel");
        editorDialog_.DefaultButton(Controls::ContentDialogButton::Primary);
        auto root = Controls::StackPanel(); root.Spacing(10);
        auto help = Controls::TextBlock();
        help.Text(to_hstring(::rlispstat::core::VariableDecimalsInfoText()));
        help.FontSize(12); help.TextWrapping(TextWrapping::Wrap); root.Children().Append(help);
        auto value = Controls::NumberBox(); value.Minimum(-1); value.Maximum(12);
        value.SmallChange(1); value.PlaceholderText(L"Automatic");
        value.Value(column.decimals < 0 ? std::numeric_limits<double>::quiet_NaN()
                                       : static_cast<double>(column.decimals));
        root.Children().Append(value); editorDialog_.Content(root);
        editorDialog_.PrimaryButtonClick(
            [this, value, variable = column.name](
                auto const&, Controls::ContentDialogButtonClickEventArgs const& args)
            {
                const double current = value.Value();
                const int decimals = std::isnan(current) ? -1 :
                    static_cast<int>(std::lround(current));
                const std::string reply = Dispatch({ "SET_VARIABLE_DECIMALS", group_,
                                                     variable, std::to_string(decimals) });
                if (reply.rfind("ERR ", 0) == 0) args.Cancel(true);
            });
        editorDialog_.ShowAsync();
    }

    void DataSheetView::ShowAnnotationEditor()
    {
        if (!window_ || !window_.Content() || !window_.Content().XamlRoot()) return;
        editorDialog_ = Controls::ContentDialog();
        editorDialog_.XamlRoot(window_.Content().XamlRoot());
        editorDialog_.Title(box_value(L"Data Sheet Notes"));
        editorDialog_.PrimaryButtonText(L"Apply");
        editorDialog_.CloseButtonText(L"Cancel");
        editorDialog_.DefaultButton(Controls::ContentDialogButton::Primary);
        auto root = Controls::StackPanel(); root.Spacing(10);
        auto help = Controls::TextBlock();
        help.Text(L"Add notes for this data sheet.");
        help.FontSize(12); root.Children().Append(help);
        auto field = Controls::TextBox();
        field.Text(to_hstring(note_.plain_text));
        field.FontSize(12); field.AcceptsReturn(true);
        field.TextWrapping(TextWrapping::Wrap); field.MinHeight(110);
        root.Children().Append(field); editorDialog_.Content(root);
        editorDialog_.PrimaryButtonClick([this, field](auto const&, auto const&)
        {
            ::rlispstat::core::SetWindowNoteText(note_, to_string(field.Text()));
            if (noteChangedCallback_)
                noteChangedCallback_(group_, note_.has_content
                    ? std::optional<::rlispstat::core::WindowNote>{note_}
                    : std::optional<::rlispstat::core::WindowNote>{});
        });
        editorDialog_.ShowAsync();
        field.Focus(FocusState::Programmatic);
    }

    void DataSheetView::RebuildRows(::rlispstat::core::DataFrameModel const& dataframe)
    {
        ::rlispstat::windows::performance::Scope timing(
            "DataSheet.RebuildRows",
            "rows=" + std::to_string(dataframe.rows) + ",columns=" +
                std::to_string(dataframe.columns.size()));
        suppressSelection_ = true;
        dataframe_ = &dataframe;
        rowCount_ = dataframe.rows;
        columnCount_ = dataframe.columns.size();
        columnWidths_.assign(columnCount_, 92.0);
        if (::rlispstat::core::DataFrameShowsAllImputations(dataframe))
        {
            for (std::size_t columnIndex = 0;
                 columnIndex < dataframe.columns.size(); ++columnIndex)
            {
                auto const& column = dataframe.columns[columnIndex];
                std::size_t widest = (column.displayName.empty()
                    ? column.name : column.displayName).size();
                for (std::size_t row = 0;
                     row < static_cast<std::size_t>(std::max(0, dataframe.rows)); ++row)
                {
                    widest = std::max(widest,
                        ::rlispstat::core::DisplayValueForDataFrameCell(
                            dataframe, column, row).size());
                }
                // The macOS table widens columns when all imputations are
                // displayed. Keep the normal compact width, but make the
                // simultaneous representation readable instead of clipping it.
                columnWidths_[columnIndex] = std::clamp(
                    24.0 + static_cast<double>(widest) * 7.2, 92.0, 360.0);
            }
        }
        rowContentWidth_ = 42.0;
        for (double width : columnWidths_) rowContentWidth_ += width;
        selectedCount_ = 0;
        selectedRows_.clear();
        rowsView_.Items().Clear();
        realizedRows_.clear();
        // Only lightweight identifiers are stored. ListView realizes and
        // recycles the containers that intersect the viewport.
        rowsView_.Items().Append(box_value(0));
        for (int row = 1; row <= std::max(0, dataframe.rows); ++row)
            rowsView_.Items().Append(box_value(row));
        UpdateStatus();
        suppressSelection_ = false;
#if 0
        suppressSelection_ = true;
        rowCount_ = dataframe.rows;
        columnCount_ = dataframe.columns.size();
        selectedCount_ = 0;
        selectedRows_.clear();
        rowsView_.Items().Clear();
        rowItems_.clear();

        constexpr int maximumDisplayedRows = 2000;
        const int displayedRows = std::min(std::max(0, dataframe.rows), maximumDisplayedRows);
        std::string status = dataframe.group + ": " + std::to_string(dataframe.rows)
            + " rows, " + std::to_string(dataframe.columns.size())
            + " variables, 0 selected · Analysis scope: All observations · N = "
            + std::to_string(dataframe.rows);
        if (displayedRows < dataframe.rows)
            status += " · showing first " + std::to_string(displayedRows);
        status_.Text(to_hstring(status));

        auto headerPanel = Controls::StackPanel();
        headerPanel.Orientation(Controls::Orientation::Horizontal);
        headerPanel.Children().Append(Cell(L"#", 42, true));
        for (auto const& column : dataframe.columns)
        {
            const std::string name = column.displayName.empty() ? column.name : column.displayName;
            headerPanel.Children().Append(Cell(to_hstring(name).c_str(), 92, true));
        }
        auto header = Controls::ListViewItem();
        header.Content(headerPanel);
        header.Padding(Thickness{ 0 });
        header.Margin(Thickness{ 0 });
        header.MinHeight(0);
        header.VerticalContentAlignment(VerticalAlignment::Stretch);
        header.IsHitTestVisible(false);
        header.IsTabStop(false);
        rowsView_.Items().Append(header);

        rowItems_.reserve(static_cast<std::size_t>(displayedRows));
        for (int row = 0; row < displayedRows; ++row)
        {
            auto panel = Controls::StackPanel();
            panel.Orientation(Controls::Orientation::Horizontal);
            panel.Children().Append(Cell(std::to_wstring(row + 1), 42, false));
            for (auto const& column : dataframe.columns)
            {
                panel.Children().Append(Cell(DisplayValue(column, static_cast<std::size_t>(row)), 92, false));
            }

            auto item = Controls::ListViewItem();
            item.Content(panel);
            item.Padding(Thickness{ 0 });
            item.Margin(Thickness{ 0 });
            item.MinHeight(0);
            item.VerticalContentAlignment(VerticalAlignment::Stretch);
            item.Background(Brush(255, 255, 255));
            item.Tag(box_value(row + 1));
            item.HorizontalContentAlignment(HorizontalAlignment::Left);
            rowItems_.push_back(item);
            rowsView_.Items().Append(item);
        }
        suppressSelection_ = false;
        ApplyRowStyles();
#endif
    }

    void DataSheetView::PrepareRowContainer(
        Controls::ListViewItem const& item, int row)
    {
        auto host = item.ContentTemplateRoot().try_as<Controls::ContentControl>();
        if (!host) return;
        const int previousRow = unbox_value_or<int>(item.Tag(), -1);
        item.Padding(Thickness{ 0 });
        item.Margin(Thickness{ 0 });
        item.MinHeight(0);
        item.HorizontalAlignment(HorizontalAlignment::Left);
        item.VerticalContentAlignment(VerticalAlignment::Stretch);
        item.HorizontalContentAlignment(HorizontalAlignment::Left);
        // Give the item, its template host and its row the same natural width.
        // Without an explicit width the ListView presenter clamps the item to
        // the viewport, leaving its ScrollViewer with ScrollableWidth == 0.
        // Keeping the header in the same item stream also means it remains
        // perfectly aligned while the sheet moves horizontally.
        item.Width(rowContentWidth_);
        host.HorizontalAlignment(HorizontalAlignment::Left);
        host.Width(rowContentWidth_);
        const bool header = row == 0;
        const bool previousWasHeader = previousRow == 0;
        const uint32_t expectedCells = static_cast<uint32_t>(columnCount_ + 1);
        auto panel = host.Content().try_as<Controls::StackPanel>();
        if (!panel || panel.Children().Size() != expectedCells ||
            (previousRow >= 0 && previousWasHeader != header))
        {
            panel = Controls::StackPanel();
            panel.Orientation(Controls::Orientation::Horizontal);
            panel.Children().Append(Cell(L"", 42, header));
            for (std::size_t column = 0; column < columnCount_; ++column)
            {
                const double width = column < columnWidths_.size()
                    ? columnWidths_[column] : 92.0;
                auto border = Cell(L"", width, header);
                if (header)
                {
                    border.Tapped([this, column](auto const&,
                        Input::TappedRoutedEventArgs const& args)
                    {
                        SelectColumn(column);
                        args.Handled(true);
                    });
                }
                else
                {
                    border.DoubleTapped([this, item, column](
                        auto const& sender, Input::DoubleTappedRoutedEventArgs const& args)
                    {
                        if (!dataframe_ || column >= dataframe_->columns.size()) return;
                        const int row = unbox_value_or<int>(item.Tag(), 0);
                        if (row < 1 || row > dataframe_->rows) return;
                        BeginCellEdit(item, sender.as<Controls::Border>(), row, column);
                        args.Handled(true);
                    });
                }
                // Building the complete column/row flyout for every realised
                // cell creates hundreds of XAML menu trees and used to repeat
                // on every linked-selection refresh.  Construct just the one
                // menu the user actually requests.  Apart from reducing the
                // cost, handling ContextRequested directly preserves the
                // pointer position and makes the first right-click work even
                // when the data-sheet window was previously inactive.
                border.ContextRequested([this, column, header](
                    UIElement const& sender,
                    Input::ContextRequestedEventArgs const& args)
                {
                    if (!dataframe_ || column >= dataframe_->columns.size()) return;
                    auto target = sender.try_as<FrameworkElement>();
                    if (!target) return;
                    auto menu = BuildColumnContextMenu(
                        dataframe_->columns[column], column, !header);
                    Controls::Primitives::FlyoutShowOptions options;
                    Windows::Foundation::Point point{};
                    if (args.TryGetPosition(target, point))
                    {
                        options.Position(point);
                        options.ShowMode(
                            Controls::Primitives::FlyoutShowMode::Transient);
                        menu.ShowAt(target, options);
                    }
                    else menu.ShowAt(target);
                    args.Handled(true);
                });
                panel.Children().Append(border);
            }
            host.Content(panel);
        }
        panel.HorizontalAlignment(HorizontalAlignment::Left);
        panel.Width(rowContentWidth_);

        // Widths can change without the number of columns changing (for
        // example when switching the representation of multiple imputations).
        // Refresh recycled cells as well as newly created ones.
        if (panel.Children().Size() > 0)
            if (auto numberCell = panel.Children().GetAt(0).try_as<Controls::Border>())
                numberCell.Width(42.0);
        for (uint32_t column = 0;
             column < static_cast<uint32_t>(columnWidths_.size()) &&
             column + 1 < panel.Children().Size(); ++column)
        {
            if (auto cell = panel.Children().GetAt(column + 1).try_as<Controls::Border>())
                cell.Width(columnWidths_[column]);
        }

        auto setText = [&panel](uint32_t index, std::wstring const& text)
        {
            if (index >= panel.Children().Size()) return;
            if (auto border = panel.Children().GetAt(index).try_as<Controls::Border>())
            {
                if (auto label = border.Child().try_as<Controls::TextBlock>())
                    label.Text(hstring(text));
            }
        };

        item.Tag(box_value(row));
        if (header)
        {
            setText(0, L"#");
            if (dataframe_)
                for (uint32_t column = 0;
                     column < static_cast<uint32_t>(dataframe_->columns.size());
                     ++column)
                {
                    auto const& source = dataframe_->columns[column];
                    const std::string name = source.displayName.empty()
                        ? source.name : source.displayName;
                    setText(column + 1, to_hstring(name).c_str());
                    if (auto border = panel.Children().GetAt(column + 1).try_as<Controls::Border>())
                    {
                        border.IsHitTestVisible(true);
                        border.Background(selectedColumn_ == static_cast<int>(column)
                            ? Brush(220, 235, 248) : Brush(247, 248, 249));
                    }
                }
            item.IsEnabled(true);
            item.IsHitTestVisible(true);
            item.IsTabStop(false);
            realizedRows_[0] = item;
            return;
        }

        item.IsEnabled(true);
        item.IsHitTestVisible(true);
        item.IsTabStop(true);
        setText(0, std::to_wstring(row));
        if (dataframe_ && row <= dataframe_->rows)
        {
            const auto rowIndex = static_cast<std::size_t>(row - 1);
            for (uint32_t column = 0;
                 column < static_cast<uint32_t>(dataframe_->columns.size());
                 ++column)
            {
                auto const& source = dataframe_->columns[column];
                setText(column + 1, DisplayValue(*dataframe_, source, rowIndex));
                if (auto border = panel.Children().GetAt(column + 1).try_as<Controls::Border>())
                {
                    border.IsHitTestVisible(true);
                }
            }
        }
        realizedRows_[row] = item;
        ApplyRowStyle(row);
    }

    void DataSheetView::SelectColumn(std::size_t columnIndex)
    {
        if (columnIndex >= columnCount_) return;
        selectedColumn_ = static_cast<int>(columnIndex);
        ApplyRowStyles();
    }

    void DataSheetView::SetOperationStatus(std::string const& message)
    {
        if (status_) status_.Text(to_hstring(message));
    }

    void DataSheetView::BeginCellEdit(
        Controls::ListViewItem const& item,
        Controls::Border const& border,
        int row, std::size_t column)
    {
        (void)item;
        if (!dataframe_ || row < 1 || row > dataframe_->rows ||
            column >= dataframe_->columns.size()) return;
        if (editingTextBox_) CommitCellEdit();
        auto label = border.Child().try_as<Controls::TextBlock>();
        if (!label) return;
        editingRow_ = row;
        editingColumn_ = column;
        editingOriginalValue_ = label.Text().c_str();
        editingBorder_ = border;
        editingTextBox_ = Controls::TextBox();
        editingTextBox_.Text(label.Text());
        editingTextBox_.FontSize(12);
        editingTextBox_.Padding(Thickness{ 5, 0, 5, 0 });
        editingTextBox_.MinHeight(24);
        editingTextBox_.VerticalContentAlignment(VerticalAlignment::Center);
        editingTextBox_.KeyDown([this](auto const&,
            Input::KeyRoutedEventArgs const& args)
        {
            if (args.Key() == Windows::System::VirtualKey::Enter)
            {
                args.Handled(true);
                CommitCellEdit();
            }
            else if (args.Key() == Windows::System::VirtualKey::Tab)
            {
                args.Handled(true);
                CommitCellEditAndMove((GetKeyState(VK_SHIFT) & 0x8000) != 0);
            }
            else if (args.Key() == Windows::System::VirtualKey::Escape)
            {
                args.Handled(true);
                CancelCellEdit();
            }
        });
        editingTextBox_.LostFocus([this](auto const&, auto const&)
        {
            if (!endingCellEdit_ && editingTextBox_) CommitCellEdit();
        });
        border.Child(editingTextBox_);
        editingTextBox_.Focus(FocusState::Programmatic);
        editingTextBox_.SelectAll();
    }

    bool DataSheetView::CommitCellEdit()
    {
        if (!editingTextBox_ || !editingBorder_ || !dataframe_ || endingCellEdit_) return false;
        endingCellEdit_ = true;
        const std::string value = to_string(editingTextBox_.Text());
        const int row = editingRow_;
        const std::size_t column = editingColumn_;
        auto replacement = Cell(editingOriginalValue_, 92, false).Child();
        editingBorder_.Child(replacement);
        editingTextBox_ = nullptr;
        editingBorder_ = nullptr;
        editingRow_ = 0;
        endingCellEdit_ = false;
        if (column >= dataframe_->columns.size()) return false;
        const std::string reply = Dispatch({ "SET_DATA_CELL", group_,
            dataframe_->columns[column].name, std::to_string(row), value });
        return reply.rfind("ERR ", 0) != 0;
    }

    void DataSheetView::CommitCellEditAndMove(bool reverse)
    {
        if (!editingTextBox_ || rowCount_ <= 0 || columnCount_ == 0) return;
        int nextRow = editingRow_;
        std::size_t nextColumn = editingColumn_;
        if (reverse)
        {
            if (nextColumn > 0) --nextColumn;
            else
            {
                nextColumn = columnCount_ - 1;
                nextRow = nextRow > 1 ? nextRow - 1 : rowCount_;
            }
        }
        else
        {
            ++nextColumn;
            if (nextColumn >= columnCount_)
            {
                nextColumn = 0;
                nextRow = nextRow < rowCount_ ? nextRow + 1 : 1;
            }
        }
        if (CommitCellEdit()) QueueCellEdit(nextRow, nextColumn);
    }

    void DataSheetView::QueueCellEdit(int row, std::size_t column)
    {
        if (row < 1 || row > rowCount_ || column >= columnCount_) return;
        pendingCellEdit_ = true;
        pendingEditRow_ = row;
        pendingEditColumn_ = column;
        if (static_cast<uint32_t>(row) < rowsView_.Items().Size())
            rowsView_.ScrollIntoView(rowsView_.Items().GetAt(static_cast<uint32_t>(row)));
        std::weak_ptr<DataSheetView> weak = shared_from_this();
        rowsView_.DispatcherQueue().TryEnqueue([weak]()
        {
            if (auto view = weak.lock()) view->TryBeginPendingCellEdit();
        });
    }

    void DataSheetView::TryBeginPendingCellEdit()
    {
        if (!pendingCellEdit_ || closed_ || editingTextBox_) return;
        if (pendingEditRow_ < 1 || pendingEditRow_ > rowCount_ ||
            pendingEditColumn_ >= columnCount_)
        {
            pendingCellEdit_ = false;
            return;
        }
        auto item = rowsView_.ContainerFromIndex(pendingEditRow_)
            .try_as<Controls::ListViewItem>();
        if (!item) return;
        auto host = item.ContentTemplateRoot().try_as<Controls::ContentControl>();
        auto panel = host ? host.Content().try_as<Controls::StackPanel>() : nullptr;
        const uint32_t child = static_cast<uint32_t>(pendingEditColumn_ + 1);
        if (!panel || child >= panel.Children().Size()) return;
        auto border = panel.Children().GetAt(child).try_as<Controls::Border>();
        if (!border) return;
        const int row = pendingEditRow_;
        const std::size_t column = pendingEditColumn_;
        pendingCellEdit_ = false;
        BeginCellEdit(item, border, row, column);
    }

    void DataSheetView::CancelCellEdit()
    {
        if (!editingTextBox_ || !editingBorder_ || endingCellEdit_) return;
        endingCellEdit_ = true;
        auto replacement = Cell(editingOriginalValue_, 92, false).Child();
        editingBorder_.Child(replacement);
        editingTextBox_ = nullptr;
        editingBorder_ = nullptr;
        editingRow_ = 0;
        endingCellEdit_ = false;
    }

    void DataSheetView::SelectionChanged()
    {
        if (suppressSelection_ || !selectionCallback_ || group_.empty()) return;
        ::rlispstat::windows::performance::Scope timing(
            "DataSheet.SelectionChanged", "group=" + group_);
        std::set<int> rows;
        for (auto const& selected : rowsView_.SelectedItems())
        {
            const int row = unbox_value_or<int>(selected, 0);
            if (row > 0) rows.insert(row);
        }
        std::set<int> changed;
        std::set_symmetric_difference(selectedRows_.begin(), selectedRows_.end(),
            rows.begin(), rows.end(), std::inserter(changed, changed.end()));
        // WinUI can raise SelectionChanged more than once while a virtualized
        // ListView settles (this is especially visible after an imputed data
        // sheet has rebuilt its cell content). Do not turn an identical second
        // notification into another linked-brushing operation.
        if (changed.empty()) return;
        selectedRows_ = rows;
        selectedCount_ = rows.size();
        status_.Text(to_hstring(group_ + ": " + std::to_string(rowCount_) + " rows, "
            + std::to_string(columnCount_) + " variables, " + std::to_string(selectedCount_)
            + " selected · Analysis scope: All observations · N = " + std::to_string(rowCount_)));
        ApplyRowStyles(changed);
        selectionCallback_(group_, rows, ::rlispstat::core::SelectionMode::Replace);
    }

    void DataSheetView::RefreshRowColors(
        std::vector<std::pair<int, std::string>> const& pointColors)
    {
        ::rlispstat::windows::performance::Scope timing(
            "DataSheet.RefreshRowColors",
            "rows=" + std::to_string(pointColors.size()));
        std::map<int, std::string> updated;
        for (auto const& [row, color] : pointColors)
            if (row > 0 && !color.empty()) updated[row] = color;
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
        pointColors_ = std::move(updated);
        ApplyRowStyles(changed);
    }

    void DataSheetView::RefreshSelection(std::set<int> const& selectedRows)
    {
        ::rlispstat::windows::performance::Scope timing(
            "DataSheet.RefreshSelection",
            "selected=" + std::to_string(selectedRows.size()));
        std::set<int> changed;
        std::set_symmetric_difference(selectedRows_.begin(), selectedRows_.end(),
            selectedRows.begin(), selectedRows.end(),
            std::inserter(changed, changed.end()));
        if (changed.empty()) return;
        suppressSelection_ = true;
        auto selectedItems = rowsView_.SelectedItems();
        for (int row : changed)
        {
            if (row < 1 || row > rowCount_) continue;
            auto value = rowsView_.Items().GetAt(static_cast<uint32_t>(row));
            uint32_t index = 0;
            if (selectedRows.count(row))
            {
                if (!selectedItems.IndexOf(value, index)) selectedItems.Append(value);
            }
            else if (selectedItems.IndexOf(value, index))
            {
                selectedItems.RemoveAt(index);
            }
        }
        selectedRows_ = selectedRows;
        selectedCount_ = selectedRows.size();
        status_.Text(to_hstring(group_ + ": " + std::to_string(rowCount_) + " rows, "
            + std::to_string(columnCount_) + " variables, " + std::to_string(selectedCount_)
            + " selected · Analysis scope: All observations · N = " + std::to_string(rowCount_)));
        UpdateStatus();
        suppressSelection_ = false;
        ApplyRowStyles(changed);
    }

    void DataSheetView::ApplyRowStyles()
    {
        ::rlispstat::windows::performance::Scope timing(
            "DataSheet.ApplyRowStyles",
            "realized=" + std::to_string(realizedRows_.size()));
        for (auto const& [row, item] : realizedRows_) ApplyRowStyle(row);
#if 0
        for (std::size_t index = 0; index < rowItems_.size(); ++index)
        {
            const int row = static_cast<int>(index + 1);
            const bool selected = selectedRows_.count(row) != 0;
            auto color = pointColors_.find(row);
            auto background = selected ? Brush(233, 233, 233) : Brush(255, 255, 255);
            auto foreground = Brush(25, 25, 25);
            if (color != pointColors_.end())
            {
                if (auto palette = ::rlispstat::core::FindPaletteColor(color->second))
                {
                    background = selected
                        ? NormalizedBrush(palette->selectedR, palette->selectedG, palette->selectedB)
                        : NormalizedBrush(palette->lightR, palette->lightG, palette->lightB);
                    if (selected && palette->selectedTextWhite)
                        foreground = Brush(255, 255, 255);
                }
            }
            auto const& item = rowItems_[index];
            item.Background(background);
            item.Resources().Insert(box_value(L"ListViewItemBackgroundSelected"), background);
            item.Resources().Insert(box_value(L"ListViewItemBackgroundSelectedPointerOver"), background);
            item.Resources().Insert(box_value(L"ListViewItemBackgroundSelectedPressed"), background);
            item.Resources().Insert(box_value(L"ListViewItemForegroundSelected"), foreground);
            if (auto panel = item.Content().try_as<Controls::StackPanel>())
            {
                for (auto const& child : panel.Children())
                {
                    if (auto border = child.try_as<Controls::Border>())
                    {
                        border.Background(background);
                        if (auto label = border.Child().try_as<Controls::TextBlock>())
                            label.Foreground(foreground);
                    }
                }
            }
        }
#endif
    }

    void DataSheetView::ApplyRowStyles(std::set<int> const& rows)
    {
        for (int row : rows) ApplyRowStyle(row);
    }

    void DataSheetView::ApplyRowStyle(int row)
    {
        auto realized = realizedRows_.find(row);
        if (realized == realizedRows_.end()) return;
        if (row == 0)
        {
            auto const& item = realized->second;
            item.Background(Brush(0, 0, 0, 0));
            if (auto host = item.ContentTemplateRoot().try_as<Controls::ContentControl>())
                if (auto panel = host.Content().try_as<Controls::StackPanel>())
                    for (uint32_t index = 1; index < panel.Children().Size(); ++index)
                        if (auto border = panel.Children().GetAt(index).try_as<Controls::Border>())
                            border.Background(selectedColumn_ == static_cast<int>(index - 1)
                                ? Brush(220, 235, 248) : Brush(247, 248, 249));
            return;
        }
        const bool selected = selectedRows_.count(row) != 0;
        auto color = pointColors_.find(row);
        auto background = selected ? Brush(233, 233, 233) : Brush(255, 255, 255);
        auto foreground = Brush(25, 25, 25);
        if (color != pointColors_.end())
        {
            if (auto palette = ::rlispstat::core::FindPaletteColor(color->second))
            {
                background = selected
                    ? NormalizedBrush(palette->selectedR, palette->selectedG, palette->selectedB)
                    : NormalizedBrush(palette->lightR, palette->lightG, palette->lightB);
                if (selected && palette->selectedTextWhite)
                    foreground = Brush(255, 255, 255);
            }
        }
        auto const& item = realized->second;
        // Selection and row colours stop at the last actual cell. The unused
        // ListView width remains the ordinary data-sheet background.
        item.Background(Brush(0, 0, 0, 0));
        if (auto host = item.ContentTemplateRoot().try_as<Controls::ContentControl>())
        {
            if (auto panel = host.Content().try_as<Controls::StackPanel>())
            {
                uint32_t cellIndex = 0;
                for (auto const& child : panel.Children())
                {
                    if (auto border = child.try_as<Controls::Border>())
                    {
                        const bool customRow = selected || color != pointColors_.end();
                        const bool selectedColumn = cellIndex > 0 &&
                            selectedColumn_ == static_cast<int>(cellIndex - 1);
                        border.Background(customRow ? background :
                            (selectedColumn ? Brush(236, 245, 252) : Brush(255, 255, 255)));
                        if (auto label = border.Child().try_as<Controls::TextBlock>())
                            label.Foreground(foreground);
                    }
                    ++cellIndex;
                }
            }
        }
    }

    void DataSheetView::UpdateStatus()
    {
        if (dataframe_)
        {
            const auto scope = analysisScope_.datasetId == dataframe_->group
                ? analysisScope_
                : ::rlispstat::core::AllObservationsAnalysisScope(
                    dataframe_->group, static_cast<std::size_t>(std::max(0, rowCount_)));
            status_.Text(to_hstring(::rlispstat::core::DataFrameStatusText(
                *dataframe_, selectedCount_) +
                " Â· " + ::rlispstat::core::AnalysisScopeSummary(
                    scope, static_cast<std::size_t>(std::max(0, rowCount_)))));
            return;
        }
        status_.Text(to_hstring(group_ + ": " + std::to_string(rowCount_) + " rows, "
            + std::to_string(columnCount_) + " variables, " +
            std::to_string(selectedCount_) +
            " selected · Analysis scope: All observations · N = " +
            std::to_string(rowCount_)));
    }

    void DataSheetView::Activate()
    {
        if (closed_) return;
        if (commandCallback_ && !group_.empty())
            commandCallback_({"SET_ACTIVE_DATASET", group_});
        if (auto presenter = window_.AppWindow().Presenter().try_as<
                Microsoft::UI::Windowing::OverlappedPresenter>())
        {
            if (presenter.State() == Microsoft::UI::Windowing::OverlappedPresenterState::Minimized)
                presenter.Restore();
        }
        window_.Activate();
    }

    void DataSheetView::Close()
    {
        if (!closed_) window_.Close();
    }

    std::string const& DataSheetView::Group() const
    {
        return group_;
    }

    Window DataSheetView::NativeWindow() const
    {
        return window_;
    }
}
