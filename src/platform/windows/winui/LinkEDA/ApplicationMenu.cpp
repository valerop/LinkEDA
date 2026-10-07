#include "pch.h"
#include "ApplicationMenu.h"
#include "PerformanceTrace.h"
#include "../../../../core/command_model.h"
#include "../../../../core/format_model.h"

#include <algorithm>
#include <array>
#include <utility>
#include <winrt/Windows.ApplicationModel.DataTransfer.h>

using namespace winrt;
using namespace Microsoft::UI::Xaml;

namespace
{
    Media::SolidColorBrush MenuBrush(uint8_t red, uint8_t green, uint8_t blue)
    {
        return Media::SolidColorBrush(
            winrt::Windows::UI::ColorHelper::FromArgb(255, red, green, blue));
    }

    bool ConfigureButtonFlyout(
        DependencyObject const& object,
        std::function<void()> const& opened,
        std::function<void()> const& closed)
    {
        if (auto button = object.try_as<Controls::Button>())
        {
            if (auto flyout = button.Flyout())
            {
                flyout.AreOpenCloseAnimationsEnabled(false);
                auto isOpen = std::make_shared<bool>(false);
                flyout.Opened([opened, isOpen](auto const&, auto const&)
                {
                    if (*isOpen) return;
                    *isOpen = true;
                    if (opened) opened();
                });
                flyout.Closed([closed, isOpen](auto const&, auto const&)
                {
                    if (!*isOpen) return;
                    *isOpen = false;
                    if (closed) closed();
                });
                return true;
            }
        }
        const int count = Media::VisualTreeHelper::GetChildrenCount(object);
        for (int index = 0; index < count; ++index)
            if (ConfigureButtonFlyout(
                    Media::VisualTreeHelper::GetChild(object, index),
                    opened, closed)) return true;
        return false;
    }

    struct MenuBarItemLifecycleState
    {
        winrt::weak_ref<Controls::MenuBarItem> item;
        std::function<void()> opened;
        std::function<void()> closed;
        event_token layoutUpdatedToken{};
        bool observesLayout = false;
        bool attached = false;
    };

    void TryConfigureMenuBarItem(
        std::shared_ptr<MenuBarItemLifecycleState> const& state)
    {
        if (!state || state->attached) return;
        auto item = state->item.get();
        if (!item) return;
        if (!ConfigureButtonFlyout(item, state->opened, state->closed)) return;

        state->attached = true;
        if (state->observesLayout)
        {
            item.LayoutUpdated(state->layoutUpdatedToken);
            state->observesLayout = false;
        }
    }

    void ConfigureMenuBarItem(
        Controls::MenuBarItem const& item,
        std::function<void()> opened,
        std::function<void()> closed)
    {
        auto state = std::make_shared<MenuBarItemLifecycleState>();
        state->item = winrt::make_weak(item);
        state->opened = std::move(opened);
        state->closed = std::move(closed);

        // MenuBar creates its internal Button and MenuFlyout while applying
        // the template. Loaded can run before that visual tree is complete, so
        // a single dispatcher attempt is racy. Observe layout until the flyout
        // really exists, then detach the observer. This guarantees that an
        // open menu is protected from command-state rebuilds from its first
        // interaction onward.
        state->layoutUpdatedToken = item.LayoutUpdated(
            [state](auto const&, auto const&)
        {
            TryConfigureMenuBarItem(state);
        });
        state->observesLayout = true;
        auto ensureFlyoutHandler = Input::PointerEventHandler(
            [state](auto const&, Input::PointerRoutedEventArgs const&)
            {
                // Some WinUI versions finish the internal MenuBar template
                // lazily. Pointer press still precedes flyout opening, so this
                // is the final first-interaction fallback.
                TryConfigureMenuBarItem(state);
            });
        item.AddHandler(
            UIElement::PointerPressedEvent(), box_value(ensureFlyoutHandler), true);
        item.Loaded([state](auto const&, auto const&)
        {
            TryConfigureMenuBarItem(state);
            if (state->attached) return;
            if (auto loadedItem = state->item.get())
            {
                loadedItem.DispatcherQueue().TryEnqueue(
                    Microsoft::UI::Dispatching::DispatcherQueuePriority::Low,
                    [state]() { TryConfigureMenuBarItem(state); });
            }
        });
    }

    bool SameMenuCommand(
        winrt::LinkEDA::implementation::ApplicationCommandItem const& left,
        winrt::LinkEDA::implementation::ApplicationCommandItem const& right)
    {
        return left.id == right.id && left.label == right.label &&
            left.enabled == right.enabled &&
            left.disabledReason == right.disabledReason &&
            left.groupLabel == right.groupLabel &&
            left.subgroupLabel == right.subgroupLabel &&
            left.checkable == right.checkable && left.checked == right.checked &&
            left.value == right.value;
    }

    bool SameMenuSnapshot(
        winrt::LinkEDA::implementation::ApplicationMenuSnapshot const& left,
        winrt::LinkEDA::implementation::ApplicationMenuSnapshot const& right)
    {
        if (left.sections.size() != right.sections.size() ||
            left.windows.size() != right.windows.size()) return false;
        for (std::size_t section = 0; section < left.sections.size(); ++section)
        {
            auto const& a = left.sections[section];
            auto const& b = right.sections[section];
            if (a.title != b.title || a.commands.size() != b.commands.size()) return false;
            for (std::size_t command = 0; command < a.commands.size(); ++command)
                if (!SameMenuCommand(a.commands[command], b.commands[command])) return false;
        }
        for (std::size_t window = 0; window < left.windows.size(); ++window)
        {
            auto const& a = left.windows[window];
            auto const& b = right.windows[window];
            if (a.token != b.token || a.title != b.title || a.active != b.active)
                return false;
        }
        return true;
    }

    bool SameMenuStructure(
        winrt::LinkEDA::implementation::ApplicationMenuSnapshot const& left,
        winrt::LinkEDA::implementation::ApplicationMenuSnapshot const& right)
    {
        if (left.sections.size() != right.sections.size() ||
            left.windows.size() != right.windows.size()) return false;
        for (std::size_t section = 0; section < left.sections.size(); ++section)
        {
            auto const& a = left.sections[section];
            auto const& b = right.sections[section];
            if (a.title != b.title || a.commands.size() != b.commands.size())
                return false;
            for (std::size_t command = 0; command < a.commands.size(); ++command)
            {
                auto const& old = a.commands[command];
                auto const& current = b.commands[command];
                if (old.id != current.id || old.label != current.label ||
                    old.groupLabel != current.groupLabel ||
                    old.subgroupLabel != current.subgroupLabel ||
                    old.checkable != current.checkable ||
                    old.value != current.value) return false;
            }
        }
        for (std::size_t window = 0; window < left.windows.size(); ++window)
        {
            if (left.windows[window].token != right.windows[window].token ||
                left.windows[window].title != right.windows[window].title)
                return false;
        }
        return true;
    }

    void CollectMenuLeaves(
        winrt::Windows::Foundation::Collections::IVector<
            Controls::MenuFlyoutItemBase> const& items,
        std::vector<Controls::MenuFlyoutItemBase>& leaves)
    {
        for (auto const& item : items)
        {
            if (auto group = item.try_as<Controls::MenuFlyoutSubItem>())
                CollectMenuLeaves(group.Items(), leaves);
            else if (item.try_as<Controls::MenuFlyoutItem>() ||
                     item.try_as<Controls::ToggleMenuFlyoutItem>())
                leaves.push_back(item);
        }
    }
}

namespace winrt::LinkEDA::implementation
{
    ApplicationCommandRegistry::ApplicationCommandRegistry(
        Dispatch dispatch, DatasetExists datasetExists, SelectionExists selectionExists,
        CurrentInterfaceFont currentInterfaceFont,
        CurrentInterfaceSize currentInterfaceSize,
        CurrentPlotTheme currentPlotTheme,
        CurrentAnalysisScope currentAnalysisScope,
        CurrentSavedSelections currentSavedSelections,
        CurrentSelectedColor currentSelectedColor,
        DatasetGroups datasetGroups,
        CurrentActiveDataset currentActiveDataset)
        : dispatch_(std::move(dispatch)),
          datasetExists_(std::move(datasetExists)),
          selectionExists_(std::move(selectionExists)),
          currentInterfaceFont_(std::move(currentInterfaceFont)),
          currentInterfaceSize_(std::move(currentInterfaceSize)),
          currentPlotTheme_(std::move(currentPlotTheme)),
          currentAnalysisScope_(std::move(currentAnalysisScope)),
          currentSavedSelections_(std::move(currentSavedSelections)),
          currentSelectedColor_(std::move(currentSelectedColor)),
          datasetGroups_(std::move(datasetGroups)),
          currentActiveDataset_(std::move(currentActiveDataset))
    {
    }

    void ApplicationCommandRegistry::RegisterWindow(WindowContext context)
    {
        if (context.token.empty()) return;
        // Keep the key outside the object that is moved into the registry.  In
        // particular, do not use context.token and std::move(context) as two
        // arguments of the same call: their evaluation order would otherwise
        // allow the move to empty the key before it is read.
        const auto token = context.token;
        auto found = windows_.find(token);
        if (found != windows_.end())
        {
            // Show/update paths are allowed to refresh the callbacks stored for
            // a window, but they are not window-activation events.  Treating
            // every completed analysis update as a fresh registration used to
            // rebuild every observed menu (including the data sheet behind the
            // output window) and steal the registry's active context twice:
            // once when an R fit was queued and again when it completed.
            const bool visibleMetadataChanged =
                found->second.title != context.title ||
                found->second.group != context.group ||
                found->second.kind != context.kind ||
                found->second.plotId != context.plotId;
            found->second = std::move(context);
            if (visibleMetadataChanged) NotifyObservers();
            return;
        }

        activeToken_ = token;
        if (context.kind == "plot") lastPlotToken_ = token;
        windows_.emplace(token, std::move(context));
        NotifyObservers();
    }

    std::vector<::rlispstat::core::SavedSelection>
    ApplicationCommandRegistry::SavedSelections(std::string const& group) const
    {
        return currentSavedSelections_ ? currentSavedSelections_(group)
                                       : std::vector<::rlispstat::core::SavedSelection>{};
    }

    void ApplicationCommandRegistry::UnregisterWindow(std::string const& token)
    {
        windows_.erase(token);
        if (activeToken_ == token) activeToken_.clear();
        if (lastPlotToken_ == token) lastPlotToken_.clear();
        NotifyObservers();
    }

    void ApplicationCommandRegistry::ClearWindows()
    {
        windows_.clear();
        activeToken_.clear();
        lastPlotToken_.clear();
        NotifyObservers();
    }

    void ApplicationCommandRegistry::CloseDatasetWindows(std::string const& group)
    {
        std::vector<std::pair<std::string, std::function<void()>>> related;
        for (auto const& [token, window] : windows_)
            if (window.group == group) related.emplace_back(token, window.close);
        for (auto const& [token, close] : related)
        {
            if (close) close();
            // A closed view may unregister itself; remove any remaining entry.
            if (windows_.erase(token) && activeToken_ == token) activeToken_.clear();
            if (lastPlotToken_ == token) lastPlotToken_.clear();
        }
        if (!related.empty()) NotifyObservers();
    }

    void ApplicationCommandRegistry::ActivateContext(std::string const& token)
    {
        if (windows_.find(token) == windows_.end() || activeToken_ == token) return;
        activeToken_ = token;
        if (auto active = Context(token); active && active->kind == "plot")
            lastPlotToken_ = token;
        // Activating a window is part of the pointer sequence that may also be
        // opening its menu. Rebuilding every MenuBar here invalidates the item
        // under that pointer and can make the first click appear to be lost (or
        // handled twice). Command snapshots are scoped to each menu's fixed
        // context token, so activation itself does not require a rebuild.
    }

    ApplicationCommandRegistry::WindowContext const*
    ApplicationCommandRegistry::Context(std::string const& token) const
    {
        auto found = windows_.find(token);
        return found == windows_.end() ? nullptr : &found->second;
    }

    ApplicationMenuSnapshot ApplicationCommandRegistry::Snapshot(
        std::string const& contextToken) const
    {
        const auto* context = Context(contextToken);
        const auto* activePlot = Context(lastPlotToken_);
        const bool hasDataset = context && !context->group.empty() &&
            (!datasetExists_ || datasetExists_(context->group));
        const bool hasSelection = hasDataset && selectionExists_ &&
            selectionExists_(context->group);
        const auto analysisScope = hasDataset && currentAnalysisScope_
            ? currentAnalysisScope_(context->group)
            : ::rlispstat::core::AnalysisScope{};
        std::vector<ApplicationCommandItem> plotCommands;
        for (auto const& option : ::rlispstat::core::PlotCommandOptions())
        {
            ApplicationCommandId id{};
            if (option.command == "PLOT_NEW_LINKED_SCATTERPLOT") id = ApplicationCommandId::NewScatterplot;
            else if (option.command == "PLOT_NEW_TRELLIS_SCATTERPLOT")
            {
                const std::array<ApplicationCommandId, 5> ids{
                    ApplicationCommandId::NewTrellisPlot,
                    ApplicationCommandId::NewTrellisTimeSeries,
                    ApplicationCommandId::NewTrellisBoxplot,
                    ApplicationCommandId::NewTrellisBarChart,
                    ApplicationCommandId::NewTrellisHistogram };
                std::size_t index = 0;
                for (auto const& trellis : ::rlispstat::core::TrellisPlotStartCommandOptions())
                {
                    plotCommands.push_back({ ids[index++], to_hstring(trellis.title).c_str(),
                        hasDataset, {}, L"Trellis Plots" });
                }
                continue;
            }
            else if (option.command == "PLOT_NEW_TIME_SERIES") id = ApplicationCommandId::NewTimeSeries;
            else if (option.command == "PLOT_NEW_LINKED_SCATTER_MATRIX") id = ApplicationCommandId::NewScatterMatrix;
            else if (option.command == "PLOT_NEW_PARALLEL_COORDINATES") id = ApplicationCommandId::NewParallelCoordinates;
            else if (option.command == "PLOT_NEW_LINKED_BOXPLOT") id = ApplicationCommandId::NewBoxplot;
            else if (option.command == "PLOT_NEW_LINKED_HISTOGRAM") id = ApplicationCommandId::NewHistogram;
            else if (option.command == "PLOT_NEW_LINKED_BAR_CHART") id = ApplicationCommandId::NewBarChart;
            else continue;
            plotCommands.push_back({ id, to_hstring(option.title).c_str(), hasDataset, {} });
        }
        std::vector<ApplicationCommandItem> analysisCommands;
        for (auto const& group : ::rlispstat::core::VisibleAnalyzeCommandMenuGroups())
        {
            for (auto const& option : group.options)
            {
                ApplicationCommandId id{};
                std::wstring label = to_hstring(option.title).c_str();
                std::wstring groupLabel = to_hstring(group.title).c_str();
                if (option.command == "ANALYZE_TABLE1") id = ApplicationCommandId::AnalyzeTable1;
                else if (option.command == "ANALYZE_CONTINGENCY_TABLE") id = ApplicationCommandId::AnalyzeContingencyTable;
                else if (option.command == "ANALYZE_CORRELATION_MATRIX") id = ApplicationCommandId::AnalyzeCorrelationMatrix;
                else if (option.command == "ANALYZE_DIMENSIONALITY") id = ApplicationCommandId::AnalyzeDimensionality;
                else if (option.command == "ANALYZE_FACTOR_ANALYSIS") id = ApplicationCommandId::AnalyzeFactorAnalysis;
                else if (option.command == "ANALYZE_SCALE_ANALYSIS") id = ApplicationCommandId::AnalyzeScaleAnalysis;
                else if (option.command == "ANALYZE_QUICK_CLUSTER") id = ApplicationCommandId::AnalyzeQuickCluster;
                else if (option.command == "ANALYZE_ONE_SAMPLE_T") id = ApplicationCommandId::AnalyzeOneSampleT;
                else if (option.command == "ANALYZE_INDEPENDENT_T") id = ApplicationCommandId::AnalyzeIndependentT;
                else if (option.command == "ANALYZE_PAIRED_T") id = ApplicationCommandId::AnalyzePairedT;
                else if (option.command == "ANALYZE_ONEWAY_ANOVA") id = ApplicationCommandId::AnalyzeOneWayAnova;
                else if (option.command == "ANALYZE_GLM")
                {
                    id = ApplicationCommandId::AnalyzeLinearModel;
                }
                else if (option.command == "ANALYZE_LINEAR_MODEL_TRELLIS")
                {
                    id = ApplicationCommandId::AnalyzeLinearModelTrellis;
                }
                else if (option.command == "ANALYZE_REGRESSION_COMPARISON")
                {
                    id = ApplicationCommandId::AnalyzeRegressionComparison;
                }
                else if (option.command == "ANALYZE_BINARY_REGRESSION")
                {
                    id = ApplicationCommandId::AnalyzeBinaryRegression;
                }
                else if (option.command == "ANALYZE_BINARY_REGRESSION_COMPARISON")
                {
                    id = ApplicationCommandId::AnalyzeBinaryRegressionComparison;
                }
                else if (option.command == "ANALYZE_COUNT_REGRESSION")
                {
                    id = ApplicationCommandId::AnalyzeCountRegression;
                }
                else if (option.command == "ANALYZE_COUNT_REGRESSION_COMPARISON")
                    id = ApplicationCommandId::AnalyzeCountRegressionComparison;
                else if (option.command == "ANALYZE_POSITIVE_CONTINUOUS_MODEL")
                    id = ApplicationCommandId::AnalyzePositiveContinuousModel;
                else if (option.command == "ANALYZE_POSITIVE_CONTINUOUS_COMPARISON")
                    id = ApplicationCommandId::AnalyzePositiveContinuousComparison;
                else if (option.command == "ANALYZE_PROPORTION_MODEL")
                    id = ApplicationCommandId::AnalyzeProportionModel;
                else if (option.command == "ANALYZE_PROPORTION_COMPARISON")
                    id = ApplicationCommandId::AnalyzeProportionComparison;
                else if (option.command == "ANALYZE_GENERALIZED_GLM")
                {
                    id = ApplicationCommandId::AnalyzeGeneralizedLinearModel;
                    label = L"Single Model...";
                    groupLabel = L"Generalized Linear Models";
                }
                else if (option.command == "ANALYZE_GENERALIZED_COMPARISON")
                {
                    id = ApplicationCommandId::AnalyzeGeneralizedComparison;
                    label = L"Model Comparison...";
                    groupLabel = L"Generalized Linear Models";
                }
                else if (option.command == "ANALYZE_LINEAR_MIXED_MODEL")
                {
                    id = ApplicationCommandId::AnalyzeLinearMixedModel;
                    label = L"Linear Mixed Model (Experimental)...";
                }
                else if (option.command == "ANALYZE_GENERALIZED_MIXED_MODEL")
                {
                    id = ApplicationCommandId::AnalyzeGeneralizedMixedModel;
                    label = L"Generalized Linear Mixed Model (Experimental)...";
                }
                else continue;
                analysisCommands.push_back({ id, std::move(label), hasDataset, {},
                    std::move(groupLabel) });
                analysisCommands.back().subgroupLabel =
                    to_hstring(option.subgroupTitle).c_str();
            }
        }
        const auto menuTitles = ::rlispstat::core::DefaultApplicationMenuTitles();
        const std::wstring interfaceFont = currentInterfaceFont_
            ? currentInterfaceFont_() : L"Inter";
        const std::wstring interfaceSize = currentInterfaceSize_
            ? currentInterfaceSize_() : L"compact";
        const std::string plotTheme = currentPlotTheme_
            ? currentPlotTheme_() : "publication";
        const std::array<ApplicationCommandId, 17> plotThemeIds{
            ApplicationCommandId::PlotThemePublication,
            ApplicationCommandId::PlotThemeClassic,
            ApplicationCommandId::PlotThemeMinimal,
            ApplicationCommandId::PlotThemeBlackWhite,
            ApplicationCommandId::PlotThemeGray,
            ApplicationCommandId::PlotThemeCowplot,
            ApplicationCommandId::PlotThemeIpsum,
            ApplicationCommandId::PlotThemeTq,
            ApplicationCommandId::PlotThemeModern,
            ApplicationCommandId::PlotThemeTufte,
            ApplicationCommandId::PlotThemeEconomist,
            ApplicationCommandId::PlotThemeFiveThirtyEight,
            ApplicationCommandId::PlotThemeManet,
            ApplicationCommandId::PlotThemeVista,
            ApplicationCommandId::PlotThemeBeige,
            ApplicationCommandId::PlotThemeDataDesk,
            ApplicationCommandId::PlotThemeGarish };
        std::vector<ApplicationCommandItem> viewCommands;
        const auto themeNames = ::rlispstat::core::PlotThemeNames();
        for (std::size_t index = 0;
             index < themeNames.size() && index < plotThemeIds.size(); ++index)
        {
            const auto& name = themeNames[index];
            viewCommands.push_back({ plotThemeIds[index],
                to_hstring(::rlispstat::core::PlotThemeDisplayName(name)).c_str(),
                hasDataset, {}, L"Global theme", true, name == plotTheme });
        }
        viewCommands.insert(viewCommands.end(), {
            {ApplicationCommandId::InterfaceFontInter, L"Inter (recommended)", true, {},
                L"Interface font", true, interfaceFont == L"Inter"},
            {ApplicationCommandId::InterfaceFontSourceSans3, L"Source Sans 3", true, {},
                L"Interface font", true, interfaceFont == L"Source Sans 3"},
            {ApplicationCommandId::InterfaceFontIBMPlexSans, L"IBM Plex Sans", true, {},
                L"Interface font", true, interfaceFont == L"IBM Plex Sans"},
            {ApplicationCommandId::InterfaceFontAptos, L"Aptos", true, {},
                L"Interface font", true, interfaceFont == L"Aptos"},
            {ApplicationCommandId::InterfaceFontCalibri, L"Calibri", true, {},
                L"Interface font", true, interfaceFont == L"Calibri"},
            {ApplicationCommandId::InterfaceFontSegoeUI, L"Segoe UI", true, {},
                L"Interface font", true, interfaceFont == L"Segoe UI"},
            {ApplicationCommandId::InterfaceFontTahoma, L"Tahoma", true, {},
                L"Interface font", true, interfaceFont == L"Tahoma"},
            {ApplicationCommandId::InterfaceSizeCompact, L"Compact", true, {},
                L"Interface text size", true, interfaceSize == L"compact"},
            {ApplicationCommandId::InterfaceSizeStandard, L"Standard", true, {},
                L"Interface text size", true, interfaceSize == L"standard"},
            {ApplicationCommandId::InterfaceSizeLarge, L"Large", true, {},
                L"Interface text size", true, interfaceSize == L"large"},
            {ApplicationCommandId::ShowSnapshotAlbum, L"Show Snapshot Album", true, {}}
        });
        ApplicationMenuSnapshot snapshot;
        const std::wstring globalAnalysisScopeTitle = to_hstring(
            ::rlispstat::core::GlobalAnalysisScopeMenuTitle()).c_str();
        const auto missing=hasDataset && missingDataCapabilities_?missingDataCapabilities_(context->group) : ::rlispstat::core::MissingDataCapabilities{};
        std::vector<ApplicationCommandItem> dataCommands;
        const auto datasetGroups = datasetGroups_
            ? datasetGroups_() : std::vector<std::string>{};
        const std::string activeDataset = currentActiveDataset_
            ? currentActiveDataset_() : std::string{};
        if (datasetGroups.empty())
        {
            dataCommands.push_back({ApplicationCommandId::SetActiveDataset,
                to_hstring(::rlispstat::core::NoRegisteredDatasetsTitle()).c_str(),
                false, {}, to_hstring(menuTitles.activeDataset).c_str()});
        }
        else
        {
            for (auto const& option :
                 ::rlispstat::core::ActiveDatasetCommandOptions(datasetGroups))
            {
                dataCommands.push_back({ApplicationCommandId::SetActiveDataset,
                    to_hstring(option.title).c_str(), true, {},
                    to_hstring(menuTitles.activeDataset).c_str(), true,
                    option.value == activeDataset, option.value});
            }
        }
        dataCommands.insert(dataCommands.end(), {
            {ApplicationCommandId::ShowActiveDataset,
                L"Show Active Dataset", !datasetGroups.empty(), {}},
            {ApplicationCommandId::ShowDataSheet, L"Open Data Sheet", hasDataset, {}},
            {ApplicationCommandId::ShowVariableView, L"Variable View", hasDataset, {}},
            {ApplicationCommandId::ChooseLabelColumn, L"Choose Label Column...", hasDataset, {}}
        });
        const std::string selectedColor = hasDataset && currentSelectedColor_
            ? currentSelectedColor_(context->group) : std::string{};
        for (auto const& color : ::rlispstat::core::PaletteColors())
            dataCommands.push_back({ApplicationCommandId::SetSelectedColor,
                to_hstring(color.name).c_str(), hasSelection, {}, L"Color Selected Points",
                true, selectedColor == color.name, color.name});
        dataCommands.push_back({ApplicationCommandId::ResetSelectedColor,
            to_hstring(::rlispstat::core::FormatDefaultResetColorMenuItemTitle()).c_str(),
            hasSelection, {}, L"Color Selected Points", true, selectedColor == "black"});
        dataCommands.insert(dataCommands.end(), {
            {ApplicationCommandId::OpenColorPalette, L"Open Data Points Color Palette...", hasDataset, {}},
            {ApplicationCommandId::AddSelectionColumn,
                L"Selection: selected / not selected", hasDataset, {}, L"Add Column to Data"},
            {ApplicationCommandId::AddPointColorColumn,
                L"Point colors", hasDataset, {}, L"Add Column to Data"},
            {ApplicationCommandId::MakeSubsetFromSelection,
                L"Current selection", hasSelection, {}, L"Make Subset"},
            {ApplicationCommandId::SetActiveXVariableNumeric,
                to_hstring(::rlispstat::core::DataVariableTypeCommandOptions()[0].title).c_str(),
                activePlot && !activePlot->plotId.empty(), {},
                to_hstring(menuTitles.variableType).c_str()},
            {ApplicationCommandId::SetActiveXVariableCategorical,
                to_hstring(::rlispstat::core::DataVariableTypeCommandOptions()[1].title).c_str(),
                activePlot && !activePlot->plotId.empty(), {},
                to_hstring(menuTitles.variableType).c_str()},
            {ApplicationCommandId::ShowActiveXVariableInformation,
                to_hstring(::rlispstat::core::DataVariableInformationCommandOption().title).c_str(),
                activePlot && !activePlot->plotId.empty(), {}},
            {ApplicationCommandId::ShowMissingDataPatterns,
                L"Missing Data Overview...", missing.overview, {}, L"Missing Data"},
            {ApplicationCommandId::ShowMissingnessModels,
                L"Missingness Models...", missing.models, {}, L"Missing Data"},
            {ApplicationCommandId::RunMultipleImputation,
                L"Multiple Imputation...", missing.imputation, {}, L"Missing Data"},
            {ApplicationCommandId::ShowImputationDiagnostics,
                L"Imputation Diagnostics...", missing.diagnostics, {}, L"Missing Data"},
            {ApplicationCommandId::SelectAll, L"Select all observations", hasDataset, {}, L"Selection"},
            {ApplicationCommandId::InvertSelection, L"Invert selection", hasDataset, {}, L"Selection"},
            {ApplicationCommandId::ClearSelection, L"Clear selection", hasSelection, {}, L"Selection"},
            {ApplicationCommandId::ShowAnalysisScopePanel,
                L"Show Floating Scope Panel...", hasDataset, {},
                globalAnalysisScopeTitle},
            {ApplicationCommandId::UseSelectionScope, L"Use Current Selection", hasSelection, {},
                globalAnalysisScopeTitle, true,
                ::rlispstat::core::AnalysisScopeTracksCurrentSelection(analysisScope)},
            {ApplicationCommandId::UseUnselectedScope, L"Use Unselected Observations", hasDataset, {},
                globalAnalysisScopeTitle, true,
                analysisScope.sourceKind == ::rlispstat::core::AnalysisScopeSourceKind::CurrentUnselection},
            {ApplicationCommandId::SaveSelectionScope,
                L"Save Current Selection as Scope...", hasSelection, {},
                globalAnalysisScopeTitle},
            {ApplicationCommandId::UseAllScope, L"Use all observations", hasDataset, {},
                globalAnalysisScopeTitle, true,
                analysisScope.kind == ::rlispstat::core::AnalysisScopeKind::AllObservations}
        });
        const ApplicationCommandItem allScopeCommand = dataCommands.back();
        dataCommands.pop_back();
        if (hasDataset && currentSavedSelections_)
        {
            const auto savedSelections = currentSavedSelections_(context->group);
            for (auto const& saved : savedSelections)
            {
                dataCommands.push_back({ApplicationCommandId::ApplySavedScope,
                    to_hstring("Saved: " + saved.name + " (" +
                        std::to_string(saved.originalRowIds.size()) + ")").c_str(),
                    true, {}, globalAnalysisScopeTitle,
                    true,
                    ::rlispstat::core::AnalysisScopeSelectionName(analysisScope) ==
                        std::optional<std::string>(saved.name),
                    saved.name});
            }
        }
        dataCommands.push_back(allScopeCommand);
        snapshot.sections = {
            {to_hstring(menuTitles.application).c_str(), {
                {ApplicationCommandId::About, L"About LinkEDA", true, {}},
                {ApplicationCommandId::QuitApplication,
                    to_hstring(menuTitles.quitApplication).c_str(), true, {}}
            }},
            {to_hstring(menuTitles.file).c_str(), {
                {ApplicationCommandId::OpenData, L"Open Data...", true, {}},
                {ApplicationCommandId::SaveData, L"Save Data", hasDataset, {}},
                {ApplicationCommandId::SaveDataAs, L"Save Data As...", hasDataset, {}},
                {ApplicationCommandId::ExportData, L"Export Data...", hasDataset, {}},
                {ApplicationCommandId::CloseDataFileAndAnalyses,
                    L"Close Data File and Analyses", hasDataset, {}},
                {ApplicationCommandId::ImportData, L"Import Data...", true, {}},
                {ApplicationCommandId::OpenDataFromR, L"Open Data from R...", true, {}},
                {ApplicationCommandId::ReturnDataToR, L"Return Data to R...", hasDataset, {}},
                {ApplicationCommandId::ReturnSelectedRowsToR,
                    L"Return Selected Rows to R...", hasSelection, {}}
            }},
            {to_hstring(menuTitles.edit).c_str(), {
                {ApplicationCommandId::Copy, L"Copy", hasSelection, {}},
                {ApplicationCommandId::CopySelectedRowIndices,
                    L"Copy Selected Row Indices", hasSelection, {}},
                {ApplicationCommandId::ClearSelection,
                    L"Clear Selection", hasSelection, {}}
            }},
            {to_hstring(menuTitles.data).c_str(), std::move(dataCommands)},
            {to_hstring(menuTitles.plot).c_str(), std::move(plotCommands)},
            {to_hstring(menuTitles.analyze).c_str(), std::move(analysisCommands)},
            {L"View", std::move(viewCommands)},
            {L"Window", {
                {ApplicationCommandId::CloseContextWindow, L"Close window", context != nullptr, {}},
                {ApplicationCommandId::CloseAllWindows, L"Close all windows", !windows_.empty(), {}},
                {ApplicationCommandId::AddSnapshot,
                    L"Add Active Window as Snapshot", context != nullptr &&
                    (context->kind == "plot" || context->kind == "output" ||
                     context->kind == "data"), {}, L"Snapshot Album"},
                {ApplicationCommandId::ShowSnapshotAlbum,
                    L"Show Snapshot Album", true, {}, L"Snapshot Album"}
            }},
            {L"Help", {
                {ApplicationCommandId::Documentation, L"Documentation", true, {}},
                {ApplicationCommandId::ShowWelcome, L"Welcome to LinkEDA", true, {}}
            }}
        };
        for (auto const& [token, window] : windows_)
        {
            snapshot.windows.push_back({token, window.title, token == activeToken_});
        }
        return snapshot;
    }

    void ApplicationCommandRegistry::Execute(
        ApplicationCommandId command, std::string const& contextToken,
        std::string const& value)
    {
        ::rlispstat::windows::performance::Scope timing(
            "ApplicationMenu.Execute",
            "command=" + std::to_string(static_cast<int>(command)));
        const auto* context = Context(contextToken);
        const auto* activePlot = Context(lastPlotToken_);
        const auto useOwningDataset = [this, context]()
        {
            if (context && !context->group.empty() && dispatch_)
                (void)dispatch_({"DATA_SET_ACTIVE_DATASET", context->group});
        };
        switch (command)
        {
        case ApplicationCommandId::OpenData:
            if (dispatch_) (void)dispatch_({"FILE_OPEN_DATA"});
            break;
        case ApplicationCommandId::SaveData:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"FILE_SAVE_DATA", context->group}); }
            break;
        case ApplicationCommandId::SaveDataAs:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"FILE_SAVE_DATA_AS", context->group}); }
            break;
        case ApplicationCommandId::ExportData:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"FILE_EXPORT_DATA", context->group}); }
            break;
        case ApplicationCommandId::CloseDataFileAndAnalyses:
            if (context && dispatch_) (void)dispatch_({"FILE_CLOSE_DATASET", context->group});
            break;
        case ApplicationCommandId::ImportData:
            if (dispatch_) (void)dispatch_({"FILE_IMPORT_DATA"});
            break;
        case ApplicationCommandId::OpenDataFromR:
            if (dispatch_) (void)dispatch_({"FILE_OPEN_DATA_FROM_R"});
            break;
        case ApplicationCommandId::ReturnDataToR:
            if (context && dispatch_)
            {
                useOwningDataset();
                (void)dispatch_({"FILE_RETURN_DATA_TO_R", context->group});
            }
            break;
        case ApplicationCommandId::ReturnSelectedRowsToR:
            if (context && dispatch_)
            {
                useOwningDataset();
                (void)dispatch_({"FILE_RETURN_SELECTED_ROWS_TO_R", context->group});
            }
            break;
        case ApplicationCommandId::Copy:
        case ApplicationCommandId::CopySelectedRowIndices:
            if (context && dispatch_)
            {
                useOwningDataset();
                const std::string reply = dispatch_({"SELECTED", context->group});
                if (reply.rfind("ERR ", 0) != 0)
                {
                    const std::string text = reply.rfind("OK ", 0) == 0
                        ? reply.substr(3) : (reply == "OK" ? std::string{} : reply);
                    Windows::ApplicationModel::DataTransfer::DataPackage package;
                    package.SetText(to_hstring(text));
                    Windows::ApplicationModel::DataTransfer::Clipboard::SetContent(package);
                    Windows::ApplicationModel::DataTransfer::Clipboard::Flush();
                }
            }
            break;
        case ApplicationCommandId::ShowWelcome:
            if (dispatch_) (void)dispatch_({"WELCOME_SHOW"});
            break;
        case ApplicationCommandId::SetActiveDataset:
            if (dispatch_ && !value.empty())
                (void)dispatch_({"DATA_SET_ACTIVE_DATASET", value});
            break;
        case ApplicationCommandId::ShowActiveDataset:
            if (dispatch_) (void)dispatch_({"DATA_SHOW_ACTIVE_DATASET"});
            break;
        case ApplicationCommandId::CloseContextWindow:
            if (context && context->close) context->close();
            break;
        case ApplicationCommandId::CloseAllWindows:
            if (dispatch_) (void)dispatch_({"CLOSE_ALL"});
            break;
        case ApplicationCommandId::ShowDataSheet:
            if (context && !context->group.empty() && dispatch_)
            {
                useOwningDataset();
                (void)dispatch_({"DATA_OPEN_DATA_SHEET", context->group});
            }
            break;
        case ApplicationCommandId::ShowVariableView:
            if (context && !context->group.empty() && dispatch_)
            {
                useOwningDataset();
                (void)dispatch_({"DATA_VARIABLE_VIEW", context->group});
            }
            break;
        case ApplicationCommandId::ChooseLabelColumn:
            if (context && dispatch_)
            {
                useOwningDataset();
                (void)dispatch_({"DATA_CHOOSE_LABEL_COLUMN", context->group});
            }
            break;
        case ApplicationCommandId::SetActiveXVariableNumeric:
            if (activePlot && dispatch_ && !activePlot->plotId.empty())
                (void)dispatch_({"DATA_VARIABLE_TYPE_NUMERIC", activePlot->plotId});
            break;
        case ApplicationCommandId::SetActiveXVariableCategorical:
            if (activePlot && dispatch_ && !activePlot->plotId.empty())
                (void)dispatch_({"DATA_VARIABLE_TYPE_FACTOR", activePlot->plotId});
            break;
        case ApplicationCommandId::ShowActiveXVariableInformation:
            if (activePlot && dispatch_ && !activePlot->plotId.empty())
                (void)dispatch_({"DATA_SHOW_VARIABLE_INFORMATION", activePlot->plotId});
            break;
        case ApplicationCommandId::SetSelectedColor:
            if (context && dispatch_ && !value.empty())
            {
                useOwningDataset();
                (void)dispatch_({"SET_SELECTED_COLOR", context->group, value});
            }
            break;
        case ApplicationCommandId::ResetSelectedColor:
            if (context && dispatch_)
            {
                useOwningDataset();
                (void)dispatch_({"RESET_SELECTED_COLOR", context->group});
            }
            break;
        case ApplicationCommandId::AddSelectionColumn:
            if (context && dispatch_)
            {
                useOwningDataset();
                (void)dispatch_({"DATA_ADD_SELECTION_COLUMN", context->group});
            }
            break;
        case ApplicationCommandId::AddPointColorColumn:
            if (context && dispatch_)
            {
                useOwningDataset();
                (void)dispatch_({"DATA_ADD_POINT_COLOR_COLUMN", context->group});
            }
            break;
        case ApplicationCommandId::MakeSubsetFromSelection:
            if (context && dispatch_)
            {
                useOwningDataset();
                (void)dispatch_({"DATA_MAKE_SUBSET_FROM_SELECTION", context->group});
            }
            break;
        case ApplicationCommandId::ClearSelection:
            if (context && !context->group.empty() && dispatch_)
            {
                useOwningDataset();
                (void)dispatch_({"CLEAR", context->group});
            }
            break;
        case ApplicationCommandId::SelectAll:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"SELECT_ALL", context->group}); }
            break;
        case ApplicationCommandId::InvertSelection:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"INVERT_SELECTION", context->group}); }
            break;
        case ApplicationCommandId::OpenColorPalette:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"DATA_POINTS_COLOR", context->group}); }
            break;
        case ApplicationCommandId::RunMultipleImputation:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_MISSING_DATA_IMPUTATION", context->group}); }
            break;
        case ApplicationCommandId::ShowMissingnessModels:
            if(context && dispatch_) { useOwningDataset(); (void)dispatch_({"DATA_MISSINGNESS_MODELS",context->group}); }
            break;
        case ApplicationCommandId::ShowMissingDataPatterns:
            if (context && dispatch_)
            {
                useOwningDataset();
                (void)dispatch_({"DATA_MISSING_DATA_OVERVIEW", context->group});
            }
            break;
        case ApplicationCommandId::ShowImputationDiagnostics:
            if (context && dispatch_) {
                useOwningDataset();
                (void)dispatch_({"DATA_IMPUTATION_DIAGNOSTICS", context->group, "summary"});
            }
            break;
        case ApplicationCommandId::ShowAnalysisScopePanel:
            if (context && dispatch_) {
                useOwningDataset();
                (void)dispatch_({"SHOW_ANALYSIS_SCOPE_PANEL", context->group});
            }
            break;
        case ApplicationCommandId::UseSelectionScope:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"SET_ANALYSIS_SCOPE_FROM_SELECTION", context->group}); }
            break;
        case ApplicationCommandId::UseUnselectedScope:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"SET_ANALYSIS_SCOPE_UNSELECTED", context->group}); }
            break;
        case ApplicationCommandId::SaveSelectionScope:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"SAVE_ANALYSIS_SCOPE_FROM_SELECTION", context->group}); }
            break;
        case ApplicationCommandId::ApplySavedScope:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"USE_SAVED_ANALYSIS_SCOPE", context->group, value}); }
            break;
        case ApplicationCommandId::UseAllScope:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"SET_ANALYSIS_SCOPE_ALL", context->group}); }
            break;
        case ApplicationCommandId::PlotThemeClassic:
            if (dispatch_) (void)dispatch_({"PLOT_THEME", "classic"});
            break;
        case ApplicationCommandId::PlotThemeMinimal:
            if (dispatch_) (void)dispatch_({"PLOT_THEME", "minimal"});
            break;
        case ApplicationCommandId::PlotThemeBlackWhite:
            if (dispatch_) (void)dispatch_({"PLOT_THEME", "bw"});
            break;
        case ApplicationCommandId::PlotThemeGray:
            if (dispatch_) (void)dispatch_({"PLOT_THEME", "gray"});
            break;
        case ApplicationCommandId::PlotThemeCowplot:
            if (dispatch_) (void)dispatch_({"PLOT_THEME", "cowplot"});
            break;
        case ApplicationCommandId::PlotThemeIpsum:
            if (dispatch_) (void)dispatch_({"PLOT_THEME", "ipsum"});
            break;
        case ApplicationCommandId::PlotThemeTq:
            if (dispatch_) (void)dispatch_({"PLOT_THEME", "theme_tq"});
            break;
        case ApplicationCommandId::PlotThemeModern:
            if (dispatch_) (void)dispatch_({"PLOT_THEME", "theme_modern"});
            break;
        case ApplicationCommandId::PlotThemeTufte:
            if (dispatch_) (void)dispatch_({"PLOT_THEME", "tufte"});
            break;
        case ApplicationCommandId::PlotThemeEconomist:
            if (dispatch_) (void)dispatch_({"PLOT_THEME", "economist"});
            break;
        case ApplicationCommandId::PlotThemeFiveThirtyEight:
            if (dispatch_) (void)dispatch_({"PLOT_THEME", "fivethirtyeight"});
            break;
        case ApplicationCommandId::PlotThemeManet:
            if (dispatch_) (void)dispatch_({"PLOT_THEME", "manet"});
            break;
        case ApplicationCommandId::PlotThemeVista:
            if (dispatch_) (void)dispatch_({"PLOT_THEME", "vista"});
            break;
        case ApplicationCommandId::PlotThemeBeige:
            if (dispatch_) (void)dispatch_({"PLOT_THEME", "beige"});
            break;
        case ApplicationCommandId::PlotThemeDataDesk:
            if (dispatch_) (void)dispatch_({"PLOT_THEME", "datadesk"});
            break;
        case ApplicationCommandId::PlotThemeGarish:
            if (dispatch_) (void)dispatch_({"PLOT_THEME", "garish"});
            break;
        case ApplicationCommandId::PlotThemePublication:
            if (dispatch_) (void)dispatch_({"PLOT_THEME", "publication"});
            break;
        case ApplicationCommandId::InterfaceFontInter:
            if (dispatch_) (void)dispatch_({"UI_SET_INTERFACE_FONT", "Inter"});
            break;
        case ApplicationCommandId::InterfaceFontSourceSans3:
            if (dispatch_) (void)dispatch_({"UI_SET_INTERFACE_FONT", "Source Sans 3"});
            break;
        case ApplicationCommandId::InterfaceFontIBMPlexSans:
            if (dispatch_) (void)dispatch_({"UI_SET_INTERFACE_FONT", "IBM Plex Sans"});
            break;
        case ApplicationCommandId::InterfaceFontAptos:
            if (dispatch_) (void)dispatch_({"UI_SET_INTERFACE_FONT", "Aptos"});
            break;
        case ApplicationCommandId::InterfaceFontCalibri:
            if (dispatch_) (void)dispatch_({"UI_SET_INTERFACE_FONT", "Calibri"});
            break;
        case ApplicationCommandId::InterfaceFontSegoeUI:
            if (dispatch_) (void)dispatch_({"UI_SET_INTERFACE_FONT", "Segoe UI"});
            break;
        case ApplicationCommandId::InterfaceFontTahoma:
            if (dispatch_) (void)dispatch_({"UI_SET_INTERFACE_FONT", "Tahoma"});
            break;
        case ApplicationCommandId::InterfaceSizeCompact:
            if (dispatch_) (void)dispatch_({"UI_SET_INTERFACE_SIZE", "compact"});
            break;
        case ApplicationCommandId::InterfaceSizeStandard:
            if (dispatch_) (void)dispatch_({"UI_SET_INTERFACE_SIZE", "standard"});
            break;
        case ApplicationCommandId::InterfaceSizeLarge:
            if (dispatch_) (void)dispatch_({"UI_SET_INTERFACE_SIZE", "large"});
            break;
        case ApplicationCommandId::ShowSnapshotAlbum:
            if (dispatch_) (void)dispatch_({"SNAPSHOT_ALBUM_SHOW"});
            break;
        case ApplicationCommandId::AddSnapshot:
            if (context && dispatch_)
                (void)dispatch_({"SNAPSHOT_ALBUM_ADD_ACTIVE", context->kind,
                    context->plotId, context->group});
            break;
        case ApplicationCommandId::Documentation:
            if (dispatch_) (void)dispatch_({"WELCOME_DOCUMENTATION"});
            break;
        case ApplicationCommandId::About:
            if (dispatch_) (void)dispatch_({"WELCOME_ABOUT"});
            break;
        case ApplicationCommandId::QuitApplication:
            if (dispatch_) (void)dispatch_({"FILE_QUIT_APPLICATION"});
            break;
        case ApplicationCommandId::NewScatterplot:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"PLOT_NEW_LINKED_SCATTERPLOT", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeTable1:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_TABLE1", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeContingencyTable:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_CONTINGENCY_TABLE", context->group}); }
            break;
        case ApplicationCommandId::NewScatterMatrix:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"PLOT_NEW_LINKED_SCATTER_MATRIX", context->group}); }
            break;
        case ApplicationCommandId::NewParallelCoordinates:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"PLOT_NEW_PARALLEL_COORDINATES", context->group}); }
            break;
        case ApplicationCommandId::NewHistogram:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"PLOT_NEW_LINKED_HISTOGRAM", context->group}); }
            break;
        case ApplicationCommandId::NewTrellisPlot:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"PLOT_NEW_TRELLIS_SCATTERPLOT", context->group, "scatter"}); }
            break;
        case ApplicationCommandId::NewTrellisTimeSeries:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"PLOT_NEW_TRELLIS_SCATTERPLOT", context->group, "time_series"}); }
            break;
        case ApplicationCommandId::NewTrellisBoxplot:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"PLOT_NEW_TRELLIS_SCATTERPLOT", context->group, "boxplot"}); }
            break;
        case ApplicationCommandId::NewTrellisBarChart:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"PLOT_NEW_TRELLIS_SCATTERPLOT", context->group, "bar"}); }
            break;
        case ApplicationCommandId::NewTrellisHistogram:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"PLOT_NEW_TRELLIS_SCATTERPLOT", context->group, "histogram"}); }
            break;
        case ApplicationCommandId::NewTimeSeries:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"PLOT_NEW_TIME_SERIES", context->group}); }
            break;
        case ApplicationCommandId::NewBoxplot:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"PLOT_NEW_LINKED_BOXPLOT", context->group}); }
            break;
        case ApplicationCommandId::NewBarChart:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"PLOT_NEW_LINKED_BAR_CHART", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeLinearModel:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_GLM", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeLinearModelTrellis:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_LINEAR_MODEL_TRELLIS", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeCorrelationMatrix:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_CORRELATION_MATRIX", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeDimensionality:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_DIMENSIONALITY", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeFactorAnalysis:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_FACTOR_ANALYSIS", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeScaleAnalysis:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_SCALE_ANALYSIS", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeQuickCluster:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_QUICK_CLUSTER", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeOneSampleT:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_ONE_SAMPLE_T", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeIndependentT:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_INDEPENDENT_T", context->group}); }
            break;
        case ApplicationCommandId::AnalyzePairedT:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_PAIRED_T", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeOneWayAnova:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_ONEWAY_ANOVA", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeRegressionComparison:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_REGRESSION_COMPARISON", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeBinaryRegression:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_BINARY_REGRESSION", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeBinaryRegressionComparison:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_BINARY_REGRESSION_COMPARISON", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeCountRegression:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_COUNT_REGRESSION", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeCountRegressionComparison:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_COUNT_REGRESSION_COMPARISON", context->group}); }
            break;
        case ApplicationCommandId::AnalyzePositiveContinuousModel:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_POSITIVE_CONTINUOUS_MODEL", context->group}); }
            break;
        case ApplicationCommandId::AnalyzePositiveContinuousComparison:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_POSITIVE_CONTINUOUS_COMPARISON", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeProportionModel:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_PROPORTION_MODEL", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeProportionComparison:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_PROPORTION_COMPARISON", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeGeneralizedLinearModel:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_GENERALIZED_GLM", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeGeneralizedComparison:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_GENERALIZED_COMPARISON", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeLinearMixedModel:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_LINEAR_MIXED_MODEL", context->group}); }
            break;
        case ApplicationCommandId::AnalyzeGeneralizedMixedModel:
            if (context && dispatch_) { useOwningDataset(); (void)dispatch_({"ANALYZE_GENERALIZED_MIXED_MODEL", context->group}); }
            break;
        }
    }

    void ApplicationCommandRegistry::ActivateWindow(std::string const& token)
    {
        auto found = windows_.find(token);
        if (found == windows_.end()) return;
        activeToken_ = token;
        if (found->second.activate) found->second.activate();
        NotifyObservers();
    }

    void ApplicationCommandRegistry::AddObserver(Observer observer)
    {
        observers_.push_back(std::move(observer));
    }

    void ApplicationCommandRegistry::RefreshCommandState()
    {
        NotifyObservers();
    }

    void ApplicationCommandRegistry::NotifyObservers()
    {
        observers_.erase(std::remove_if(observers_.begin(), observers_.end(),
            [](Observer const& observer) { return !observer || !observer(); }),
            observers_.end());
    }

    std::shared_ptr<DataSheetMenuHost> DataSheetMenuHost::Create()
    {
        return std::shared_ptr<DataSheetMenuHost>(new DataSheetMenuHost());
    }

    DataSheetMenuHost::DataSheetMenuHost()
    {
        menuBar_ = Controls::MenuBar();
        menuBar_.Height(40);
        menuBar_.MinHeight(40);
        menuBar_.Background(MenuBrush(246, 247, 249));
        menuBar_.Foreground(MenuBrush(28, 31, 36));
        menuBar_.HorizontalAlignment(HorizontalAlignment::Stretch);
        menuBar_.VerticalAlignment(VerticalAlignment::Top);
    }

    FrameworkElement DataSheetMenuHost::Element() const
    {
        return menuBar_;
    }

    void DataSheetMenuHost::SetRegistry(
        std::shared_ptr<ApplicationCommandRegistry> registry,
        std::string contextToken)
    {
        registry_ = std::move(registry);
        contextToken_ = std::move(contextToken);
        std::weak_ptr<DataSheetMenuHost> weak = shared_from_this();
        if (registry_)
        {
            registry_->AddObserver([weak]()
            {
                if (auto host = weak.lock())
                {
                    host->RequestRebuild();
                    return true;
                }
                return false;
            });
        }
        RebuildMenu();
    }

    void DataSheetMenuHost::RequestRebuild()
    {
        rebuildPending_ = true;
        if (openMenuCount_ > 0 || menuDismissPending_ || rebuildQueued_) return;
        rebuildQueued_ = true;
        std::weak_ptr<DataSheetMenuHost> weak = shared_from_this();
        if (!menuBar_.DispatcherQueue().TryEnqueue(
                Microsoft::UI::Dispatching::DispatcherQueuePriority::Low,
                [weak]()
                {
                    auto host = weak.lock();
                    if (!host) return;
                    host->rebuildQueued_ = false;
                    if (host->openMenuCount_ == 0 && host->rebuildPending_)
                        host->RebuildMenu();
                }))
            rebuildQueued_ = false;
    }

    void DataSheetMenuHost::RebuildMenu()
    {
        if (openMenuCount_ > 0)
        {
            rebuildPending_ = true;
            return;
        }
        rebuildPending_ = false;
        if (!registry_) return;
        ::rlispstat::windows::performance::Scope timing(
            "ApplicationMenu.Rebuild", "context=" + contextToken_);
        const auto snapshot = registry_->Snapshot(contextToken_);
        if (hasLastSnapshot_ && SameMenuSnapshot(lastSnapshot_, snapshot)) return;
        // A selection only changes enabled and checked states. Replacing the
        // MenuBarItem objects for that update can discard the next pointer
        // action (and invalidate an item in a flyout that is still closing).
        // Retain their identity whenever the command tree is unchanged.
        if (hasLastSnapshot_ && SameMenuStructure(lastSnapshot_, snapshot) &&
            menuBar_.Items().Size() == snapshot.sections.size())
        {
            std::vector<std::vector<Controls::MenuFlyoutItemBase>> sectionLeaves;
            sectionLeaves.reserve(snapshot.sections.size());
            bool valid = true;
            for (std::size_t section = 0; section < snapshot.sections.size(); ++section)
            {
                std::vector<Controls::MenuFlyoutItemBase> leaves;
                CollectMenuLeaves(menuBar_.Items().GetAt(
                    static_cast<uint32_t>(section)).Items(), leaves);
                const std::size_t expected = snapshot.sections[section].commands.size() +
                    (snapshot.sections[section].title == L"Window"
                        ? snapshot.windows.size() : 0);
                if (leaves.size() != expected) { valid = false; break; }
                sectionLeaves.push_back(std::move(leaves));
            }
            if (valid)
            {
                for (std::size_t section = 0; section < snapshot.sections.size(); ++section)
                {
                    auto const& commands = snapshot.sections[section].commands;
                    auto const& leaves = sectionLeaves[section];
                    for (std::size_t command = 0; command < commands.size(); ++command)
                    {
                        auto const& state = commands[command];
                        auto const& item = leaves[command];
                        item.IsEnabled(state.enabled);
                        if (auto toggle = item.try_as<Controls::ToggleMenuFlyoutItem>())
                            toggle.IsChecked(state.checked);
                        if (state.enabled || state.disabledReason.empty())
                            Controls::ToolTipService::SetToolTip(item, nullptr);
                        else
                            Controls::ToolTipService::SetToolTip(item,
                                box_value(hstring(state.disabledReason)));
                    }
                    if (snapshot.sections[section].title == L"Window")
                        for (std::size_t window = 0; window < snapshot.windows.size(); ++window)
                            if (auto item = leaves[commands.size() + window]
                                    .try_as<Controls::MenuFlyoutItem>())
                                item.Text(hstring(std::wstring(
                                    snapshot.windows[window].active ? L"\u2713 " : L"   ") +
                                    snapshot.windows[window].title));
                }
                lastSnapshot_ = snapshot;
                return;
            }
        }
        lastSnapshot_ = snapshot;
        hasLastSnapshot_ = true;
        // Build a fresh visual menu from the immutable command snapshot.
        // Reusing MenuBarItem objects while replacing their flyout contents
        // can leave WinUI's internal button/flyout template subscribed to
        // more than one generation of items.  The visible result is duplicate
        // menu entries and a single pointer action dispatching a command more
        // than once.  Rebuild only after every menu has closed (guard above),
        // then replace the complete top-level collection atomically.
        menuBar_.Items().Clear();
        std::weak_ptr<DataSheetMenuHost> weak = shared_from_this();
        const auto appendCommand = [weak](auto const& items,
                                           ApplicationCommandItem const& command)
        {
            const auto id = command.id;
            const auto value = command.value;
            if (command.checkable)
            {
                auto item = Controls::ToggleMenuFlyoutItem();
                item.Text(hstring(command.label));
                item.IsEnabled(command.enabled);
                item.IsChecked(command.checked);
                if (!command.enabled && !command.disabledReason.empty())
                    Controls::ToolTipService::SetToolTip(
                        item, box_value(hstring(command.disabledReason)));
                item.Click([weak, id, value](auto const&, auto const&)
                {
                    // WinUI can deliver Click after closing the flyout. A
                    // queued state refresh may already have rebuilt its owner;
                    // the clicked item must still execute its original action.
                    if (auto host = weak.lock(); host && host->registry_)
                        host->registry_->Execute(id, host->contextToken_, value);
                });
                items.Append(item);
                return;
            }
            auto item = Controls::MenuFlyoutItem();
            item.Text(hstring(command.label));
            item.IsEnabled(command.enabled);
            if (!command.enabled && !command.disabledReason.empty())
                Controls::ToolTipService::SetToolTip(
                    item, box_value(hstring(command.disabledReason)));
            item.Click([weak, id, value](auto const&, auto const&)
            {
                if (auto host = weak.lock(); host && host->registry_)
                    host->registry_->Execute(id, host->contextToken_, value);
            });
            items.Append(item);
        };
        for (auto const& section : snapshot.sections)
        {
            auto menu = Controls::MenuBarItem();
            menu.Title(hstring(section.title));
            std::weak_ptr<DataSheetMenuHost> menuHost = shared_from_this();
            ConfigureMenuBarItem(menu,
                [menuHost]()
                {
                    if (auto host = menuHost.lock()) host->OnMenuOpened();
                },
                [menuHost]()
                {
                    if (auto host = menuHost.lock()) host->OnMenuClosed();
                });
            if (section.title == L"Window")
            {
                for (auto const& command : section.commands)
                    appendCommand(menu.Items(), command);
                if (!section.commands.empty() && !snapshot.windows.empty())
                    menu.Items().Append(Controls::MenuFlyoutSeparator());
                for (auto const& window : snapshot.windows)
                {
                    auto item = Controls::MenuFlyoutItem();
                    item.Text(hstring(std::wstring(window.active ? L"\u2713 " : L"   ") + window.title));
                    const auto token = window.token;
                    item.Click([weak, token](auto const&, auto const&)
                    {
                        if (auto host = weak.lock(); host && host->registry_)
                            host->registry_->ActivateWindow(token);
                    });
                    menu.Items().Append(item);
                }
            }
            else
            {
                Controls::MenuFlyoutSubItem commandGroup{ nullptr };
                Controls::MenuFlyoutSubItem commandSubgroup{ nullptr };
                std::wstring commandGroupLabel;
                std::wstring commandSubgroupLabel;
                for (auto const& command : section.commands)
                {
                    if (!command.groupLabel.empty())
                    {
                        if (!commandGroup || commandGroupLabel != command.groupLabel)
                        {
                            commandGroup = Controls::MenuFlyoutSubItem();
                            commandGroup.Text(hstring(command.groupLabel));
                            commandGroupLabel = command.groupLabel;
                            menu.Items().Append(commandGroup);
                            commandSubgroup = nullptr;
                            commandSubgroupLabel.clear();
                        }
                        if (!command.subgroupLabel.empty())
                        {
                            if (!commandSubgroup ||
                                commandSubgroupLabel != command.subgroupLabel)
                            {
                                commandSubgroup = Controls::MenuFlyoutSubItem();
                                commandSubgroup.Text(hstring(command.subgroupLabel));
                                commandSubgroupLabel = command.subgroupLabel;
                                commandGroup.Items().Append(commandSubgroup);
                            }
                            appendCommand(commandSubgroup.Items(), command);
                        }
                        else
                        {
                            commandSubgroup = nullptr;
                            commandSubgroupLabel.clear();
                            appendCommand(commandGroup.Items(), command);
                        }
                    }
                    else
                    {
                        commandGroup = nullptr;
                        commandGroupLabel.clear();
                        commandSubgroup = nullptr;
                        commandSubgroupLabel.clear();
                        appendCommand(menu.Items(), command);
                    }
                }
            }
            menuBar_.Items().Append(menu);
        }
    }

    void DataSheetMenuHost::OnMenuOpened()
    {
        ++openMenuCount_;
    }

    void DataSheetMenuHost::OnMenuClosed()
    {
        if (openMenuCount_ > 0) --openMenuCount_;
        if (openMenuCount_ != 0 || menuDismissPending_) return;

        // Closed can be raised before WinUI delivers the selected item's Click
        // event. Keep rebuilds blocked for one dispatcher turn so a state
        // notification cannot replace the item between pointer release and
        // command execution. RequestRebuild then uses its usual low-priority
        // turn, which also coalesces notifications produced by the command.
        menuDismissPending_ = true;
        std::weak_ptr<DataSheetMenuHost> weak = shared_from_this();
        if (!menuBar_.DispatcherQueue().TryEnqueue(
                Microsoft::UI::Dispatching::DispatcherQueuePriority::Low,
                [weak]()
                {
                    auto host = weak.lock();
                    if (!host) return;
                    host->menuDismissPending_ = false;
                    if (host->rebuildPending_) host->RequestRebuild();
                }))
        {
            menuDismissPending_ = false;
            if (rebuildPending_) RequestRebuild();
        }
    }
}
