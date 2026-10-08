#include "pch.h"
#include "WorkflowWindows.h"
#include "PerformanceTrace.h"
#include "TableExportService.h"
#include "WindowBranding.h"

#include "../../../../core/scatterplot_model.h"
#include "../../../../core/table1_model.h"
#include "../../../../core/correlation_model.h"
#include "../../../../core/mean_comparison_model.h"
#include "../../../../core/glm_model.h"
#include "../../../../core/model_terms.h"
#include "../../../../core/format_model.h"
#include "../../../../core/dimensionality_model.h"
#include "../../../../core/scale_analysis_model.h"
#include "../../../../core/dendrogram_model.h"
#include "../../../../core/selection_model.h"
#include "../../../../core/export_model.h"
#include "../../../../core/svg_writer.h"
#include "../../../../core/mixed_model.h"
#include "../../../../core/trellis_scatterplot_model.h"
#include <microsoft.ui.xaml.window.h>
#include <winrt/Windows.ApplicationModel.DataTransfer.h>
#include <winrt/Microsoft.UI.Xaml.Media.Imaging.h>

#include <algorithm>
#include <array>
#include <atomic>
#include <cctype>
#include <cwctype>
#include <cmath>
#include <fstream>
#include <iomanip>
#include <initializer_list>
#include <limits>
#include <optional>
#include <shobjidl.h>
#include <sstream>
#include <tuple>

using namespace winrt;
using namespace Microsoft::UI::Xaml;

namespace
{
    constexpr double kSpace = 8.0;
    constexpr double kPadding = 16.0;
    constexpr double kRowHeight = 27.0;
    constexpr double kDialogBodyFontSize = 12.0;
    constexpr double kDialogTitleFontSize = 16.0;
    constexpr std::size_t kInlineVariableMenuLimit = 48;

    template<class Parent>
    void AppendPolynomialMenu(Parent const& parent, std::vector<std::string> const& numeric,
        std::string const& response, std::vector<std::string> const& terms,
        std::function<void(std::string const&)> choose, std::string const& baseTerm = {})
    {
        const auto groups = ::rlispstat::core::PolynomialMenuTerms(numeric, response, terms, baseTerm);
        if (groups.empty()) return;
        auto root = Controls::MenuFlyoutSubItem(); root.Text(L"Add polynomial term");
        for (auto const& group : groups) {
            auto variable = Controls::MenuFlyoutSubItem(); variable.Text(to_hstring(group.first));
            for (auto const& term : group.second) {
                std::string name; int degree = 0; ::rlispstat::core::ParsePolynomialTerm(term, name, degree);
                auto item = Controls::MenuFlyoutItem();
                item.Text(to_hstring(degree == 2 ? "Quadratic (degree 2)" : degree == 3 ? "Cubic (degree 3)" : "Degree " + std::to_string(degree)));
                item.Click([choose, term](auto const&, auto const&) { choose(term); });
                if (baseTerm.empty()) variable.Items().Append(item); else root.Items().Append(item);
            }
            if (baseTerm.empty()) root.Items().Append(variable);
        }
        parent.Items().Append(root);
    }

    std::vector<std::string> PolynomialNumericVariables(
        std::vector<std::string> const& available, ::rlispstat::core::ModelSpecification const& state,
        ::rlispstat::core::PlotModel const* seed = nullptr)
    {
        std::vector<std::string> numeric;
        for (auto const& name : available)
            if (::rlispstat::core::ModelSpecificationTermType(state, name,
                    ::rlispstat::core::DefaultTermType(seed, name)) == "numeric") numeric.push_back(name);
        return numeric;
    }

    Controls::MenuFlyoutSubItem BuildInteractionMenuSubItem(
        std::vector<std::string> const& candidates,
        std::function<void(std::string const&)> const& choose,
        bool enabled = true,
        std::string const& baseTerm = {})
    {
        const auto groups = ::rlispstat::core::BuildInteractionMenuGroups(candidates, baseTerm);
        if (groups.empty() || !choose) return nullptr;

        auto interactions = Controls::MenuFlyoutSubItem();
        interactions.Text(to_hstring(::rlispstat::core::GLMInteractionsMenuTitle()));
        interactions.IsEnabled(enabled);
        if (!baseTerm.empty())
        {
            auto const& group = groups.front();
            const std::size_t directOrder =
                ::rlispstat::core::UniqueBaseVariablesForTerm(baseTerm).size() + 1;
            bool addedDirectChoice = false;
            for (auto const& orderGroup : group.orders)
            {
                if (orderGroup.order != directOrder) continue;
                for (auto const& choice : orderGroup.choices)
                {
                    auto item = Controls::MenuFlyoutItem();
                    item.Text(to_hstring(choice.displayLabel));
                    item.IsEnabled(enabled);
                    item.Click([choose, term = choice.term](auto const&, auto const&)
                    {
                        choose(term);
                    });
                    interactions.Items().Append(item);
                    addedDirectChoice = true;
                }
            }
            bool addedHigherOrder = false;
            for (auto const& orderGroup : group.orders)
            {
                if (orderGroup.order <= directOrder) continue;
                if (addedDirectChoice && !addedHigherOrder)
                    interactions.Items().Append(Controls::MenuFlyoutSeparator());
                auto order = Controls::MenuFlyoutSubItem();
                order.Text(to_hstring(std::to_string(orderGroup.order) + " terms"));
                for (auto const& choice : orderGroup.choices)
                {
                    auto item = Controls::MenuFlyoutItem();
                    item.Text(to_hstring(choice.displayLabel));
                    item.IsEnabled(enabled);
                    item.Click([choose, term = choice.term](auto const&, auto const&)
                    {
                        choose(term);
                    });
                    order.Items().Append(item);
                }
                interactions.Items().Append(order);
                addedHigherOrder = true;
            }
            return interactions;
        }
        for (auto const& group : groups)
        {
            auto predictor = Controls::MenuFlyoutSubItem();
            predictor.Text(to_hstring(group.predictor));
            for (auto const& orderGroup : group.orders)
            {
                auto order = Controls::MenuFlyoutSubItem();
                order.Text(to_hstring(std::to_string(orderGroup.order) + " terms"));
                for (auto const& choice : orderGroup.choices)
                {
                    auto item = Controls::MenuFlyoutItem();
                    item.Text(to_hstring(choice.displayLabel));
                    item.IsEnabled(enabled);
                    item.Click([choose, term = choice.term](auto const&, auto const&)
                    {
                        choose(term);
                    });
                    order.Items().Append(item);
                }
                predictor.Items().Append(order);
            }
            interactions.Items().Append(predictor);
        }
        return interactions;
    }

    void AppendMissingInformationMenu(
        Controls::MenuFlyout const& menu,
        std::function<void(std::vector<std::string> const&)> const& dispatch,
        std::string const& id, bool available)
    {
        if (!dispatch || !available) return;
        auto root=Controls::MenuFlyoutSubItem(); root.Text(L"Multiple imputation");
        auto item=Controls::MenuFlyoutItem(); item.Text(L"Show missing-information diagnostics");
        item.Click([dispatch,id](auto const&,auto const&) { dispatch({"SHOW_MISSING_INFORMATION",id}); });
        root.Items().Append(item); menu.Items().Append(root);
    }

    void AppendRCodeExportItems(
        Controls::MenuFlyoutSubItem const& exportMenu,
        std::function<void(std::vector<std::string> const&)> const& dispatch,
        std::string const& outputId,
        bool publication = true)
    {
        if (!dispatch || outputId.empty()) return;
        auto rCode = Controls::MenuFlyoutSubItem();
        rCode.Text(L"R Code");
        for (auto const& action :
             ::rlispstat::core::BuildRCodeExportMenuActions(true, publication))
        {
            auto item = Controls::MenuFlyoutItem();
            item.Text(to_hstring(action.title));
            item.Click([dispatch, command = action.command, outputId]
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

    void AppendRCodeOnlyExportMenu(
        Controls::MenuFlyout const& menu,
        std::function<void(std::vector<std::string> const&)> const& dispatch,
        std::string const& outputId,
        bool publication = true)
    {
        if (!dispatch || outputId.empty()) return;
        auto exportMenu = Controls::MenuFlyoutSubItem();
        exportMenu.Text(L"Export");
        AppendRCodeExportItems(exportMenu, dispatch, outputId, publication);
        if (menu.Items().Size() > 0)
            menu.Items().Append(Controls::MenuFlyoutSeparator());
        menu.Items().Append(exportMenu);
    }

    Media::SolidColorBrush Brush(uint8_t r, uint8_t g, uint8_t b)
    {
        return Media::SolidColorBrush(Windows::UI::ColorHelper::FromArgb(255, r, g, b));
    }

    Media::SolidColorBrush PlotBrush(::rlispstat::core::PlotRGBA color,
                                     double alpha = 1.0)
    {
        auto byte = [](double value) {
            return static_cast<uint8_t>(std::lround(
                std::clamp(value, 0.0, 1.0) * 255.0));
        };
        return Media::SolidColorBrush(Windows::UI::ColorHelper::FromArgb(
            byte(color.a * alpha), byte(color.r), byte(color.g), byte(color.b)));
    }

    Media::SolidColorBrush TransparentBrush()
    {
        return Media::SolidColorBrush(Windows::UI::ColorHelper::FromArgb(0, 0, 0, 0));
    }

    void PopulateAnalysisScopeCombo(
        Controls::ComboBox const& combo,
        std::vector<::rlispstat::core::AnalysisScopeChoice> const& choices,
        std::string const& selectedValue)
    {
        combo.Items().Clear();
        auto item = Controls::ComboBoxItem();
        std::string label = choices.size()==1 ? choices.front().label : "Global analysis scope";
        for (auto const& choice : choices)
            if (choice.value == selectedValue) label = choice.label;
        item.Content(box_value(to_hstring(label)));
        item.Tag(box_value(to_hstring(selectedValue)));
        combo.Items().Append(item);
        combo.SelectedIndex(0);
        combo.IsHitTestVisible(false);
        combo.IsTabStop(false);
        Controls::ToolTipService::SetToolTip(combo, box_value(L"Global Analysis Scope. Change it in the central menu; recalculating uses the current global scope."));
    }

    std::string SelectedAnalysisScopeChoice(Controls::ComboBox const& combo)
    {
        if (auto item = combo.SelectedItem().try_as<Controls::ComboBoxItem>())
        {
            auto tag = item.Tag();
            if (tag) return to_string(unbox_value<hstring>(tag));
        }
        const hstring value = unbox_value_or<hstring>(combo.SelectedItem(), L"");
        if (!value.empty()) return to_string(value);
        return "all";
    }

    void ApplyGLMResultTableLayout(
        Controls::Grid const& grid,
        ::rlispstat::core::GLMResultTableLayout const& layout)
    {
        grid.HorizontalAlignment(layout.stretchesToAvailableWidth ?
            HorizontalAlignment::Stretch : HorizontalAlignment::Left);
        double minimumWidth = 0.0;
        for (auto const& specification : layout.columns)
        {
            minimumWidth += specification.minimumWidth;
            auto column = Controls::ColumnDefinition();
            column.MinWidth(specification.minimumWidth);
            if (specification.maximumWidth > 0.0)
                column.MaxWidth(specification.maximumWidth);
            column.Width(specification.absorbsExtraWidth ?
                GridLengthHelper::FromValueAndType(1, GridUnitType::Star) :
                GridLengthHelper::FromValueAndType(1, GridUnitType::Auto));
            grid.ColumnDefinitions().Append(column);
        }
        grid.MinWidth(minimumWidth);
    }

    void AttachDirectMenu(FrameworkElement const& target,
                          Controls::MenuFlyout const& menu)
    {
        target.ContextFlyout(menu);
        // The window-wide context-menu router also walks table cells. If a
        // cell only exposes ContextFlyout, WinUI's automatic opener and that
        // router can both try to show the same flyout for one right-click;
        // the second transient popup then swallows the first item click.
        // Handle the request at the cell itself and stop it bubbling, while
        // retaining ContextFlyout so keyboard/context discovery still knows
        // which menu belongs to the cell.
        target.ContextRequested([menu](auto const& sender,
                                       Input::ContextRequestedEventArgs const& args)
        {
            auto element = sender.try_as<FrameworkElement>();
            if (!element) return;
            Controls::Primitives::FlyoutShowOptions options;
            Windows::Foundation::Point point{};
            if (args.TryGetPosition(element, point))
            {
                options.Position(point);
                options.ShowMode(Controls::Primitives::FlyoutShowMode::Transient);
                menu.ShowAt(element, options);
            }
            else menu.ShowAt(element);
            args.Handled(true);
        });
        target.Tapped([menu](auto const& sender,
                            Input::TappedRoutedEventArgs const& args)
        {
            if (auto element = sender.try_as<FrameworkElement>())
                menu.ShowAt(element);
            args.Handled(true);
        });
    }

    void AttachContextOnlyMenu(FrameworkElement const& target,
                               Controls::MenuFlyout const& menu)
    {
        target.ContextFlyout(menu);
        target.ContextRequested([menu](auto const& sender,
                                        Input::ContextRequestedEventArgs const& args)
        {
            auto element = sender.try_as<FrameworkElement>();
            if (!element) return;
            Controls::Primitives::FlyoutShowOptions options;
            Windows::Foundation::Point point{};
            if (args.TryGetPosition(element, point))
            {
                options.Position(point);
                options.ShowMode(Controls::Primitives::FlyoutShowMode::Transient);
                menu.ShowAt(element, options);
            }
            else menu.ShowAt(element);
            args.Handled(true);
        });
    }

    void AttachPrimaryAndContextMenus(
        FrameworkElement const& target,
        Controls::MenuFlyout const& primaryMenu,
        Controls::MenuFlyout const& contextMenu)
    {
        target.ContextFlyout(contextMenu);
        target.ContextRequested([contextMenu](auto const& sender,
                                               Input::ContextRequestedEventArgs const& args)
        {
            auto element = sender.try_as<FrameworkElement>();
            if (!element) return;
            Controls::Primitives::FlyoutShowOptions options;
            Windows::Foundation::Point point{};
            if (args.TryGetPosition(element, point)) {
                options.Position(point);
                options.ShowMode(Controls::Primitives::FlyoutShowMode::Transient);
                contextMenu.ShowAt(element, options);
            } else contextMenu.ShowAt(element);
            args.Handled(true);
        });
        target.Tapped([primaryMenu](auto const& sender,
                                    Input::TappedRoutedEventArgs const& args)
        {
            if (auto element = sender.try_as<FrameworkElement>())
                primaryMenu.ShowAt(element);
            args.Handled(true);
        });
    }

    void CopyTextToClipboard(hstring const& value)
    {
        Windows::ApplicationModel::DataTransfer::DataPackage package;
        package.SetText(value);
        Windows::ApplicationModel::DataTransfer::Clipboard::SetContent(package);
        Windows::ApplicationModel::DataTransfer::Clipboard::Flush();
    }

    void ShowStatisticExplanation(
        Controls::TextBlock const& owner,
        ::rlispstat::core::ModelStatisticExplanationContext const& context);

    ::rlispstat::core::ModelStatisticExplanationContext LinearStatisticContext(
        ::rlispstat::core::GroupModelState const& model,
        std::string statistic)
    {
        ::rlispstat::core::ModelStatisticExplanationContext context;
        context.statistic = std::move(statistic);
        context.modelFamily = "gaussian";
        context.link = "identity";
        context.multipleImputation = model.multipleImputation;
        context.rubinRulesApplied = model.multipleImputation &&
            model.note.find("Rubin's rules were not") == std::string::npos;
        context.miMethod = context.rubinRulesApplied ? "mice::pool" : "";
        return context;
    }

    ::rlispstat::core::ModelStatisticExplanationContext GeneralizedStatisticContext(
        ::rlispstat::core::GeneralizedGLMState const& model,
        std::string statistic)
    {
        return ::rlispstat::core::GeneralizedModelStatisticContext(model, statistic);
    }

    ::rlispstat::core::ModelStatisticExplanationContext RegressionComparisonStatisticContext(
        ::rlispstat::core::RegressionComparisonState const& state,
        std::size_t modelIndex,
        std::string statistic)
    {
        (void)modelIndex;
        ::rlispstat::core::ModelStatisticExplanationContext context;
        context.statistic = std::move(statistic);
        context.modelFamily = "gaussian";
        context.link = "identity";
        context.comparison = true;
        context.multipleImputation = state.multipleImputation;
        context.rubinRulesApplied = state.multipleImputation &&
            state.note.find("Rubin's rules were not") == std::string::npos;
        if (context.rubinRulesApplied) {
            if (state.note.find("mice::D3") != std::string::npos)
                context.miMethod = "mice::D3";
            else if (context.statistic.find("vs prev") != std::string::npos ||
                     context.statistic.find("D1") != std::string::npos ||
                     context.statistic.find("Wald") != std::string::npos)
                context.miMethod = "mice::D1";
            else context.miMethod = "mice::pool";
        }
        return context;
    }

    ::rlispstat::core::ModelStatisticExplanationContext GeneralizedComparisonStatisticContext(
        ::rlispstat::core::GeneralizedComparisonState const& state,
        std::size_t modelIndex,
        std::string statistic)
    {
        ::rlispstat::core::ModelStatisticExplanationContext context;
        context.statistic = std::move(statistic);
        context.comparison = true;
        context.multipleImputation = state.multipleImputation;
        context.rubinRulesApplied = state.multipleImputation;
        if (modelIndex < state.models.size()) {
            auto const& model = state.models[modelIndex];
            context.modelFamily = model.family;
            context.link = model.link;
            context.likelihoodAvailable = model.fit.likelihoodAvailable;
            context.rubinRulesApplied = state.multipleImputation &&
                model.fit.note.find("Rubin's rules were not") == std::string::npos;
            if (context.rubinRulesApplied) {
                context.miMethod = !model.comparisonMethod.empty() &&
                    (context.statistic.find("vs prev") != std::string::npos ||
                     context.statistic.find("D1") != std::string::npos ||
                     context.statistic.find("D3") != std::string::npos ||
                     context.statistic.find("Wald") != std::string::npos)
                    ? model.comparisonMethod : "mice::pool";
            }
        } else {
            context.modelFamily = state.family;
            context.link = state.link;
        }
        return context;
    }

    ::rlispstat::core::ModelStatisticExplanationContext AnalysisStatisticContext(
        std::string statistic,
        std::string analysisKind,
        std::string method = {},
        std::string adjustment = {},
        bool multipleImputation = false,
        std::string poolingMethod = {})
    {
        ::rlispstat::core::ModelStatisticExplanationContext context;
        context.statistic = std::move(statistic);
        context.analysisKind = std::move(analysisKind);
        context.method = std::move(method);
        context.adjustment = std::move(adjustment);
        context.multipleImputation = multipleImputation;
        context.rubinRulesApplied = multipleImputation && !poolingMethod.empty();
        context.miMethod = std::move(poolingMethod);
        return context;
    }

    Controls::MenuFlyout CreateStatisticCellMenu(
        std::wstring const& statistic,
        Controls::TextBlock const& value,
        ::rlispstat::core::ModelStatisticExplanationContext context = {})
    {
        if (context.statistic.empty()) context.statistic = to_string(hstring(statistic));
        auto menu = Controls::MenuFlyout();
        auto heading = Controls::MenuFlyoutItem();
        heading.Text(statistic);
        heading.IsEnabled(false);
        menu.Items().Append(heading);
        menu.Items().Append(Controls::MenuFlyoutSeparator());
        auto copy = Controls::MenuFlyoutItem();
        copy.Text(L"Copy value");
        copy.Click([value](auto const&, auto const&)
        {
            CopyTextToClipboard(value.Text());
        });
        menu.Items().Append(copy);
        auto copyNamed = Controls::MenuFlyoutItem();
        copyNamed.Text(L"Copy statistic and value");
        copyNamed.Click([value, statistic](auto const&, auto const&)
        {
            CopyTextToClipboard(hstring(statistic) + L"\t" + value.Text());
        });
        menu.Items().Append(copyNamed);
        auto explain = Controls::MenuFlyoutItem();
        explain.Text(to_hstring(
            ::rlispstat::core::DefaultModelContextMenuTitles().explainStatistic));
        explain.Click([value, context](auto const&, auto const&)
        {
            ShowStatisticExplanation(value, context);
        });
        menu.Items().Append(explain);
        return menu;
    }

    Controls::MenuFlyout CreateStructuralCellMenu(
        std::wstring const& title,
        Controls::TextBlock const& value)
    {
        auto menu = Controls::MenuFlyout();
        auto heading = Controls::MenuFlyoutItem();
        heading.Text(title);
        heading.IsEnabled(false);
        menu.Items().Append(heading);
        menu.Items().Append(Controls::MenuFlyoutSeparator());
        auto copy = Controls::MenuFlyoutItem();
        copy.Text(L"Copy");
        copy.Click([value](auto const&, auto const&)
        {
            CopyTextToClipboard(value.Text());
        });
        menu.Items().Append(copy);
        return menu;
    }

    HWND NativeWindowHandle(Window const& window)
    {
        HWND handle{};
        check_hresult(window.as<::IWindowNative>()->get_WindowHandle(&handle));
        return handle;
    }

    std::optional<std::wstring> ChooseDendrogramExportPath(
        Window const& owner, std::string const& identifier, std::string const& format)
    {
        com_ptr<IFileSaveDialog> dialog;
        if (FAILED(CoCreateInstance(CLSID_FileSaveDialog, nullptr, CLSCTX_INPROC_SERVER,
                                    IID_PPV_ARGS(dialog.put())))) return std::nullopt;
        const std::wstring extension = to_hstring(
            ::rlispstat::core::PlotExportFileExtension(format)).c_str();
        const std::wstring pattern = L"*." + extension;
        const std::wstring label = to_hstring(format + " files").c_str();
        const COMDLG_FILTERSPEC filter{label.c_str(), pattern.c_str()};
        dialog->SetFileTypes(1, &filter);
        dialog->SetDefaultExtension(extension.c_str());
        dialog->SetTitle(to_hstring("Export Quick Cluster as " + format).c_str());
        dialog->SetFileName(to_hstring(
            ::rlispstat::core::PlotExportFilename(identifier, format)).c_str());
        const HRESULT shown = dialog->Show(NativeWindowHandle(owner));
        if (shown == HRESULT_FROM_WIN32(ERROR_CANCELLED) || FAILED(shown))
            return std::nullopt;
        com_ptr<IShellItem> item;
        if (FAILED(dialog->GetResult(item.put()))) return std::nullopt;
        PWSTR rawPath{};
        if (FAILED(item->GetDisplayName(SIGDN_FILESYSPATH, &rawPath)) || !rawPath)
            return std::nullopt;
        std::wstring path(rawPath); CoTaskMemFree(rawPath); return path;
    }

    bool WriteDendrogramBytes(std::wstring const& path,
                              std::vector<uint8_t> const& bytes)
    {
        std::ofstream output(path, std::ios::binary | std::ios::trunc);
        if (!output) return false;
        output.write(reinterpret_cast<char const*>(bytes.data()),
                     static_cast<std::streamsize>(bytes.size()));
        return output.good();
    }

    winrt::rlispstatWinUI::implementation::TableExportPayload Table1ExportPayloadForState(
        ::rlispstat::core::Table1DisplayState const& state)
    {
        using winrt::rlispstatWinUI::implementation::TableExportCell;
        using winrt::rlispstatWinUI::implementation::TableExportPayload;
        using winrt::rlispstatWinUI::implementation::TableExportRow;

        TableExportPayload payload;
        payload.title = state.title;
        payload.subtitle = ::rlispstat::core::Table1Subtitle(state, true);
        payload.baseName = state.id;
        payload.csvText = ::rlispstat::core::Table1CSVText(state);
        payload.markdownText = ::rlispstat::core::Table1MarkdownText(state);
        payload.footnotes = state.footnotes;
        payload.warnings = state.warnings;

        const bool hasStubs = !state.stubHeaders.empty();
        const std::size_t stubCount = hasStubs ? state.stubHeaders.size() : 1;
        if (hasStubs)
        {
            for (auto const& header : state.stubHeaders)
                payload.headers.push_back(TableExportCell{header, false, true});
        }
        else payload.headers.push_back(TableExportCell{"Variable", false, true});
        for (auto const& column : state.columns)
            payload.headers.push_back(TableExportCell{column, true, true});
        if (state.showP) payload.headers.push_back(TableExportCell{"p", true, true});
        if (state.showTest) payload.headers.push_back(TableExportCell{"Test", false, true});

        payload.spanningHeader = state.groupSpanningHeader;
        payload.spanningColumnStart = stubCount;
        payload.spanningColumnCount = static_cast<std::size_t>(
            std::max(0, state.groupSpanningColumnCount));

        for (std::size_t rowNumber = 0; rowNumber < state.rows.size(); ++rowNumber)
        {
            auto const& row = state.rows[rowNumber];
            const bool parent = ::rlispstat::core::Table1RowIsParent(row);
            const bool indented = ::rlispstat::core::Table1RowIsIndented(row);
            const bool muted = row.rowType == "missing" ||
                ::rlispstat::core::Table1RowIsSubVariable(row);
            const bool nestedSeparator = hasStubs && rowNumber > 0 &&
                !row.stubValues.empty() && !row.stubValues.front().empty();

            TableExportRow exported;
            exported.separatorBefore = nestedSeparator || (parent && rowNumber > 0);
            if (hasStubs)
            {
                for (std::size_t index = 0; index < stubCount; ++index)
                {
                    const std::string value = index < row.stubValues.size()
                        ? row.stubValues[index] : std::string{};
                    exported.cells.push_back(TableExportCell{
                        value, false, row.rowType == "nested_total" || index == 0,
                        false, 0});
                }
            }
            else
            {
                exported.cells.push_back(TableExportCell{
                    ::rlispstat::core::Table1DisplayLabelText(row), false, parent,
                    muted, indented ? 1 : 0});
            }
            for (std::size_t index = 0; index < state.columns.size(); ++index)
            {
                exported.cells.push_back(TableExportCell{
                    index < row.values.size() ? row.values[index] : std::string{}, true,
                    row.rowType == "nested_total", row.rowType == "missing", 0});
            }
            const bool comparisonRow = parent || row.rowType == "numeric_mean_sd";
            if (state.showP)
                exported.cells.push_back(TableExportCell{
                    row.p.empty() && comparisonRow ? "-" : row.p, true});
            if (state.showTest)
                exported.cells.push_back(TableExportCell{
                    row.test.empty() && comparisonRow ? "-" : row.test, false});
            payload.rows.push_back(std::move(exported));
        }
        return payload;
    }

    std::string MeanComparisonTableDisplayTitle(
        ::rlispstat::core::MeanComparisonState const& state,
        ::rlispstat::core::MeanComparisonTable const& table)
    {
        if (!table.title.empty()) return table.title;
        if (table.tableId == "one_sample_t") return "One-sample t tests";
        if (table.tableId == "one_sample") return "One-sample t test";
        if (table.tableId == "paired" || table.tableId == "paired_t") return "Paired t tests";
        if (table.tableId == "paired_wilcoxon") return "Wilcoxon signed-rank tests";
        if (table.tableId == "paired_mcnemar") return "McNemar tests";
        return state.title.empty() ? std::string("Results") : state.title;
    }

    bool MeanComparisonDescriptiveTable(
        ::rlispstat::core::MeanComparisonTable const& table)
    {
        return table.tableId == "group_descriptives" ||
            table.tableId == "rank_descriptives" ||
            table.tableId == "paired_descriptives" ||
            table.tableId == "paired_rank_descriptives" ||
            table.tableId == "paired_binary_descriptives";
    }

    std::string MarkdownTableCell(std::string text)
    {
        std::string escaped;
        escaped.reserve(text.size());
        for (char value : text)
        {
            if (value == '|') escaped += "\\|";
            else if (value == '\r' || value == '\n') escaped += ' ';
            else escaped += value;
        }
        return escaped;
    }

    winrt::rlispstatWinUI::implementation::TableExportPayload MeanComparisonExportPayload(
        ::rlispstat::core::MeanComparisonState const& state,
        ::rlispstat::core::MeanComparisonTable const& table)
    {
        using namespace ::rlispstat::core;
        using winrt::rlispstatWinUI::implementation::TableExportCell;
        using winrt::rlispstatWinUI::implementation::TableExportPayload;
        using winrt::rlispstatWinUI::implementation::TableExportRow;

        TableExportPayload payload;
        payload.title = MeanComparisonTableDisplayTitle(state, table);
        payload.subtitle = state.subtitle;
        payload.baseName = state.id + "_" + table.tableId;
        payload.csvText = MeanComparisonTableCSV(table);
        payload.footnotes = table.notes;
        payload.warnings = table.warnings;
        for (auto const& warning : state.warnings)
            if (std::find(payload.warnings.begin(), payload.warnings.end(), warning) ==
                payload.warnings.end()) payload.warnings.push_back(warning);
        if (state.dataScopeCaptured)
        {
            const auto scopeN = AnalysisScopeRowCount(
                state.dataScope, state.dataScope.totalDatasetRows);
            payload.footnotes.push_back("Analysis scope: " +
                (state.dataScope.sourceDescription.empty()
                    ? AnalysisScopeKindId(state.dataScope.kind)
                    : state.dataScope.sourceDescription) +
                " (N = " + std::to_string(scopeN) + ").");
        }

        payload.headers.push_back(TableExportCell{
            table.stubTitle.empty() ? "Variable" : table.stubTitle, false, true});
        std::vector<std::size_t> visibleColumns;
        for (std::size_t index = 0; index < table.columns.size(); ++index)
        {
            if (!table.columns[index].visible) continue;
            visibleColumns.push_back(index);
            payload.headers.push_back(TableExportCell{
                table.columns[index].title,
                table.columns[index].format != MeanComparisonCellFormat::Text,
                true});
        }
        for (std::size_t rowIndex = 0; rowIndex < table.rows.size(); ++rowIndex)
        {
            auto const& source = table.rows[rowIndex];
            TableExportRow row;
            row.separatorBefore = rowIndex > 0;
            row.cells.push_back(TableExportCell{
                (source.kind==MeanComparisonRowKind::Descriptive && !source.group.empty()?"    ":"")+source.label, false, source.kind == MeanComparisonRowKind::Result || source.kind==MeanComparisonRowKind::GroupHeader,
                source.kind == MeanComparisonRowKind::Warning});
            for (auto index : visibleColumns)
            {
                const std::string value = index < source.cells.size()
                    ? MeanComparisonCellText(source.cells[index], table.columns[index].format)
                    : std::string{};
                row.cells.push_back(TableExportCell{
                    value, table.columns[index].format != MeanComparisonCellFormat::Text,
                    false, source.kind == MeanComparisonRowKind::Warning});
            }
            payload.rows.push_back(std::move(row));
        }

        std::ostringstream markdown;
        markdown << "| " << MarkdownTableCell(payload.headers.front().text);
        for (std::size_t index = 1; index < payload.headers.size(); ++index)
            markdown << " | " << MarkdownTableCell(payload.headers[index].text);
        markdown << " |\n| :---";
        for (std::size_t index = 1; index < payload.headers.size(); ++index)
            markdown << (payload.headers[index].alignRight ? " | ---:" : " | :---");
        markdown << " |\n";
        for (auto const& row : payload.rows)
        {
            markdown << "| ";
            for (std::size_t index = 0; index < payload.headers.size(); ++index)
            {
                if (index) markdown << " | ";
                markdown << MarkdownTableCell(index < row.cells.size()
                    ? row.cells[index].text : std::string{});
            }
            markdown << " |\n";
        }
        payload.markdownText = markdown.str();
        return payload;
    }

    std::string CsvTableCell(std::string text)
    {
        if (text.find_first_of(",\"\r\n") == std::string::npos) return text;
        std::string quoted = "\"";
        for (char value : text) quoted += value == '"' ? "\"\"" : std::string(1, value);
        return quoted + "\"";
    }

    winrt::rlispstatWinUI::implementation::TableExportPayload LinearModelExportPayload(
        ::rlispstat::core::GroupModelState const& model,
        ::rlispstat::core::GLMFitSummary const& fit)
    {
        using namespace ::rlispstat::core;
        using winrt::rlispstatWinUI::implementation::TableExportCell;
        using winrt::rlispstatWinUI::implementation::TableExportPayload;
        using winrt::rlispstatWinUI::implementation::TableExportRow;

        TableExportPayload payload;
        payload.title = model.title.empty() ? "Linear Model" : model.title;
        payload.subtitle = "Response: " + model.response + " · Scope: " + model.scope;
        payload.baseName = "general-linear-model-" + model.group;
        for (auto const& header : std::array<std::string, 9>{
                 "Variable", "Type", "b", "\xCE\xB2", "SE", "t", "p",
                 "Partial r", "\xCE\x94R\xC2\xB2"})
            payload.headers.push_back(TableExportCell{header, header != "Variable" &&
                header != "Type", true});

        for (std::size_t index = 0; index < fit.coefficients.size(); ++index) {
            auto const& source = fit.coefficients[index];
            const auto displayCells = GLMRegressionTableRow(
                source, model.centeredPredictors);
            const bool nested = source.rowType == "reference" ||
                source.rowType == "factor_level" ||
                (ModelTermTypeIsInteraction(source.termType) &&
                 source.rowType == "coefficient" && !source.sourceTerm.empty() &&
                 source.sourceTerm != source.term);
            TableExportRow row;
            row.separatorBefore = index > 0 && source.rowType == "term";
            for (std::size_t column = 0; column < displayCells.size(); ++column) {
                row.cells.push_back(TableExportCell{
                    GLMRegressionTableCellText(displayCells[column]),
                    column >= 2,
                    source.rowType == "term",
                    false,
                    column == 0 && nested ? 1 : 0});
            }
            payload.rows.push_back(std::move(row));
        }
        payload.footnotes.push_back("R\xC2\xB2 = " + FormatPercent(fit.r2, 1) +
            "; adjusted R\xC2\xB2 = " + FormatPercent(fit.adjR2, 1) +
            "; s = " + FormatDouble(fit.sigma, 3) +
            "; df = " + std::to_string(fit.dfResidual) + ".");
        payload.footnotes.push_back("Regression: SS = " + FormatDouble(fit.ssRegression, 4) +
            ", df = " + std::to_string(fit.dfModel) +
            ", MS = " + FormatDouble(fit.msRegression, 4) +
            ", F = " + FormatDouble(fit.globalF, 3) +
            ", p " + FormatPValue(fit.globalP) + ".");
        payload.footnotes.push_back("Residual: SS = " + FormatDouble(fit.ssResidual, 4) +
            ", df = " + std::to_string(fit.dfResidual) +
            ", MS = " + FormatDouble(fit.msResidual, 4) + ".");
        if (!fit.predictorCenters.empty()) {
            std::ostringstream centers;
            centers << "Centered predictors: ";
            bool first = true;
            for (auto const& entry : fit.predictorCenters) {
                if (!std::isfinite(entry.second)) continue;
                if (!first) centers << "; ";
                first = false;
                centers << entry.first << " (mean = "
                        << FormatDouble(entry.second, 4) << ")";
            }
            if (!first) payload.footnotes.push_back(centers.str() + ".");
        }
        if (!model.note.empty()) payload.footnotes.push_back(model.note);
        if (!fit.warning.empty() && fit.warning.find("success") == std::string::npos &&
            fit.warning.find("fitted") == std::string::npos)
            payload.warnings.push_back(fit.warning);

        std::ostringstream csv;
        std::ostringstream markdown;
        for (std::size_t index = 0; index < payload.headers.size(); ++index) {
            if (index) csv << ',';
            csv << CsvTableCell(payload.headers[index].text);
        }
        csv << '\n';
        markdown << "| ";
        for (std::size_t index = 0; index < payload.headers.size(); ++index) {
            if (index) markdown << " | ";
            markdown << MarkdownTableCell(payload.headers[index].text);
        }
        markdown << " |\n| :--- | :---";
        for (std::size_t index = 2; index < payload.headers.size(); ++index)
            markdown << " | ---:";
        markdown << " |\n";
        for (auto const& row : payload.rows) {
            for (std::size_t index = 0; index < payload.headers.size(); ++index) {
                const std::string value = index < row.cells.size() ? row.cells[index].text : "";
                if (index) csv << ',';
                csv << CsvTableCell(value);
                if (index == 0) markdown << "| "; else markdown << " | ";
                markdown << MarkdownTableCell(value);
            }
            csv << '\n'; markdown << " |\n";
        }
        payload.csvText = csv.str();
        payload.markdownText = markdown.str();
        return payload;
    }

    winrt::rlispstatWinUI::implementation::TableExportPayload
    GeneralizedModelExportPayload(
        ::rlispstat::core::GeneralizedGLMState const& state)
    {
        using namespace ::rlispstat::core;
        using winrt::rlispstatWinUI::implementation::TableExportCell;
        using winrt::rlispstatWinUI::implementation::TableExportPayload;
        using winrt::rlispstatWinUI::implementation::TableExportRow;

        TableExportPayload payload;
        payload.title = state.title.empty()
            ? StatisticalModelTypeLabel(state.modelType)
            : state.title;
        payload.subtitle = state.countRegression
            ? "Response: " + state.response + " | Distribution: " +
              CountDistributionLabel(state.countDistribution) + " | Link: log" +
              (state.exposure.empty() ? std::string{} : " | Exposure: " + state.exposure) +
              " | Scope: " + state.scope
            : "Response: " + state.response + " | Distribution: " +
              (FindDistributionSpecification(state.family)
                  ? FindDistributionSpecification(state.family)->visibleName : state.family) +
              " | Link: " + state.link + " | Scope: " + state.scope;
        if (!state.offsetVariable.empty())
          payload.subtitle += " | Link-scale offset: " + state.offsetVariable + " (used as stored)";
        payload.baseName = state.id.empty() ? "generalized-linear-model" : state.id;
        std::vector<std::string> headers{
            "Variable / category", "Type", "b", "SE", state.statisticName,
            "p", "95% CI"};
        if (GeneralizedModelHasExponentiatedEffect(state)) {
            const std::string label = GeneralizedExponentiatedEffectLabel(state);
            headers.push_back(label); headers.push_back(label + " 95% CI");
        }
        for (auto const& header : headers)
            payload.headers.push_back(TableExportCell{
                header, header != "Variable / category" && header != "Type", true});

        for (std::size_t index = 0; index < state.rows.size(); ++index) {
            auto const& source = state.rows[index];
            const bool nested = source.rowType == "factor_level" ||
                source.rowType == "reference";
            const bool parent = source.rowType == "factor_parent" ||
                source.rowType == "term_parent";
            const bool reference = source.rowType == "reference";
            std::string label = source.displayLabel.empty()
                ? source.term : source.displayLabel;
            const std::string sourceTerm = source.sourceTerm.empty()
                ? source.term : source.sourceTerm;
            if (sourceTerm.find(':') == std::string::npos &&
                state.centeredPredictors.count(sourceTerm) && !nested)
                label += " (centered)";
            const auto statistic = [&](double value, int digits) {
                if (parent) return std::string{};
                if (reference) return std::string("\xE2\x80\x94");
                return FormatDoubleOrDash(value, digits);
            };
            const auto structuralText = [&](std::string value) {
                if (parent) return std::string{};
                if (reference) return std::string("\xE2\x80\x94");
                return value;
            };
            std::vector<std::string> values{
                label, nested ? std::string{} : source.termType,
                statistic(source.estimate, 4), statistic(source.stdError, 4),
                statistic(source.statistic, 3),
                reference ? std::string("\xE2\x80\x94") : FormatPValue(source.pValue),
                structuralText("[" + FormatDoubleOrDash(source.ciLower, 3) + ", " +
                    FormatDoubleOrDash(source.ciUpper, 3) + "]")};
            if (GeneralizedModelHasExponentiatedEffect(state)) {
                values.push_back(statistic(source.exponentiatedEstimate, 3));
                values.push_back(structuralText("[" +
                    FormatDoubleOrDash(source.exponentiatedLower, 3) + ", " +
                    FormatDoubleOrDash(source.exponentiatedUpper, 3) + "]"));
            }
            TableExportRow row;
            row.separatorBefore = index > 0 &&
                (source.rowType == "factor_parent" || source.rowType == "term_parent");
            for (std::size_t column = 0; column < values.size(); ++column)
                row.cells.push_back(TableExportCell{values[column], column >= 2,
                    source.rowType == "factor_parent" || source.rowType == "term_parent",
                    false, column == 0 && nested ? 1 : 0});
            payload.rows.push_back(std::move(row));
        }
        payload.footnotes.push_back("N = " + std::to_string(state.n) +
            "; null deviance = " + FormatDoubleOrDash(state.nullDeviance, 3) +
            "; residual deviance = " + FormatDoubleOrDash(state.residualDeviance, 3) +
            "; df = " + std::to_string(state.dfResidual) + ".");
        if (state.likelihoodAvailable) {
            payload.footnotes.push_back("AIC = " + FormatDoubleOrDash(state.aic, 1) +
                "; BIC = " + FormatDoubleOrDash(state.bic, 1) +
                "; log likelihood = " + FormatDoubleOrDash(state.logLik, 3) + ".");
        } else if (state.countDistribution == CountDistribution::QuasiPoisson) {
            payload.footnotes.push_back(
                "Likelihood-based statistics are not available for quasi-Poisson models.");
        } else if (state.multipleImputation) {
            payload.footnotes.push_back(
                "Likelihood-based statistics are not pooled across imputations.");
        } else {
            payload.footnotes.push_back("Likelihood-based statistics are unavailable.");
        }
        if (state.countRegression) {
            if (state.countDistribution == CountDistribution::NegativeBinomial) {
                if (state.multipleImputation && std::isfinite(state.thetaDescriptiveMean)) {
                    payload.footnotes.push_back(
                        "Negative-binomial theta, mean across imputation-specific fits = " +
                        FormatDoubleOrDash(state.thetaDescriptiveMean, 3) + "; range = " +
                        FormatDoubleOrDash(state.thetaDescriptiveMin, 3) + "–" +
                        FormatDoubleOrDash(state.thetaDescriptiveMax, 3) +
                        "; not Rubin-pooled.");
                } else {
                    payload.footnotes.push_back(
                        "Negative-binomial shape (theta; Var = mu + mu^2/theta) = " +
                        FormatDoubleOrDash(state.theta, 3) + ".");
                }
            } else {
                payload.footnotes.push_back("Dispersion = " +
                    FormatDoubleOrDash(state.dispersion, 3) + ".");
            }
            payload.footnotes.push_back(
                "Exponentiated coefficients are incidence rate ratios (rate ratios).");
        } else if (GeneralizedModelHasExponentiatedEffect(state)) {
            payload.footnotes.push_back(
                "Exponentiated coefficients are reported as " +
                GeneralizedExponentiatedEffectLabel(state) + ".");
        }
        if (!state.note.empty()) payload.footnotes.push_back(state.note);
        for (auto const& warning : state.warnings) payload.warnings.push_back(warning);

        std::ostringstream csv;
        std::ostringstream markdown;
        for (std::size_t index = 0; index < payload.headers.size(); ++index) {
            if (index) csv << ',';
            csv << CsvTableCell(payload.headers[index].text);
            if (index == 0) markdown << "| "; else markdown << " | ";
            markdown << MarkdownTableCell(payload.headers[index].text);
        }
        csv << '\n'; markdown << " |\n| :--- | :---";
        for (std::size_t index = 2; index < payload.headers.size(); ++index)
            markdown << " | ---:";
        markdown << " |\n";
        for (auto const& row : payload.rows) {
            for (std::size_t index = 0; index < payload.headers.size(); ++index) {
                const std::string value = index < row.cells.size()
                    ? row.cells[index].text : "";
                if (index) csv << ',';
                csv << CsvTableCell(value);
                if (index == 0) markdown << "| "; else markdown << " | ";
                markdown << MarkdownTableCell(value);
            }
            csv << '\n'; markdown << " |\n";
        }
        payload.csvText = csv.str(); payload.markdownText = markdown.str();
        return payload;
    }

    winrt::rlispstatWinUI::implementation::TableExportPayload
    RegressionComparisonExportPayload(
        ::rlispstat::core::RegressionComparisonState const& state)
    {
        using namespace ::rlispstat::core;
        using winrt::rlispstatWinUI::implementation::TableExportCell;
        using winrt::rlispstatWinUI::implementation::TableExportPayload;
        using winrt::rlispstatWinUI::implementation::TableExportRow;

        const ComparisonCopyTable table = BuildRegressionComparisonCopyTable(state);
        TableExportPayload payload;
        payload.title = table.title;
        payload.subtitle = "Response: " + table.response + " · Scope: " + state.scope;
        payload.baseName = state.id.empty() ? "regression-model-comparison" : state.id;
        payload.headers.push_back(TableExportCell{"Term", false, true});
        for (const std::string& label : table.modelLabels) {
            payload.headers.push_back(TableExportCell{label, true, true});
        }
        for (std::size_t rowIndex = 0; rowIndex < table.termLabels.size(); ++rowIndex) {
            TableExportRow row;
            row.separatorBefore = rowIndex > 0;
            row.cells.push_back(TableExportCell{table.termLabels[rowIndex], false, false});
            for (std::size_t modelIndex = 0; modelIndex < table.modelLabels.size(); ++modelIndex) {
                const std::string value = rowIndex < table.termDisplaysByRow.size() &&
                    modelIndex < table.termDisplaysByRow[rowIndex].size()
                    ? table.termDisplaysByRow[rowIndex][modelIndex] : "";
                row.cells.push_back(TableExportCell{value, true, false});
            }
            payload.rows.push_back(std::move(row));
        }
        for (std::size_t rowIndex = 0; rowIndex < table.fitLabels.size(); ++rowIndex) {
            TableExportRow row;
            row.separatorBefore = rowIndex == 0;
            row.cells.push_back(TableExportCell{table.fitLabels[rowIndex], false, true});
            for (std::size_t modelIndex = 0; modelIndex < table.modelLabels.size(); ++modelIndex) {
                const std::string value = rowIndex < table.fitDisplaysByRow.size() &&
                    modelIndex < table.fitDisplaysByRow[rowIndex].size()
                    ? table.fitDisplaysByRow[rowIndex][modelIndex] : "";
                row.cells.push_back(TableExportCell{value, true, false});
            }
            payload.rows.push_back(std::move(row));
        }
        payload.footnotes.push_back(table.footnote);
        if (!state.note.empty()) payload.footnotes.push_back(state.note);

        std::ostringstream csv;
        std::ostringstream markdown;
        for (std::size_t index = 0; index < payload.headers.size(); ++index) {
            if (index) csv << ',';
            csv << CsvTableCell(payload.headers[index].text);
            if (index == 0) markdown << "| "; else markdown << " | ";
            markdown << MarkdownTableCell(payload.headers[index].text);
        }
        csv << '\n';
        markdown << " |\n| :---";
        for (std::size_t index = 1; index < payload.headers.size(); ++index) markdown << " | ---:";
        markdown << " |\n";
        for (const TableExportRow& row : payload.rows) {
            for (std::size_t index = 0; index < payload.headers.size(); ++index) {
                const std::string value = index < row.cells.size() ? row.cells[index].text : "";
                if (index) csv << ',';
                csv << CsvTableCell(value);
                if (index == 0) markdown << "| "; else markdown << " | ";
                markdown << MarkdownTableCell(value);
            }
            csv << '\n';
            markdown << " |\n";
        }
        payload.csvText = csv.str();
        payload.markdownText = markdown.str();
        return payload;
    }

    winrt::rlispstatWinUI::implementation::TableExportPayload
    GeneralizedComparisonExportPayload(
        ::rlispstat::core::GeneralizedComparisonState const& state)
    {
        using namespace ::rlispstat::core;
        using winrt::rlispstatWinUI::implementation::TableExportCell;
        using winrt::rlispstatWinUI::implementation::TableExportPayload;
        using winrt::rlispstatWinUI::implementation::TableExportRow;

        TableExportPayload payload;
        payload.title = StatisticalModelComparisonTitle(
            state.modelType, state.multipleImputation);
        payload.subtitle = "Response: " + state.response + " · Scope: " + state.scope;
        payload.baseName = state.id.empty()
            ? "generalized-model-comparison" : state.id;
        payload.headers.push_back(TableExportCell{"Term", false, true});
        for (std::size_t modelIndex = 0; modelIndex < state.models.size(); ++modelIndex) {
            const std::string label = state.models[modelIndex].label.empty()
                ? "Model " + std::to_string(modelIndex + 1)
                : state.models[modelIndex].label;
            payload.headers.push_back(TableExportCell{label, true, true});
        }
        for (std::size_t rowIndex = 0; rowIndex < state.termRows.size(); ++rowIndex) {
            TableExportRow row;
            row.separatorBefore = rowIndex > 0;
            row.cells.push_back(TableExportCell{
                GeneralizedComparisonDisplayLabel(state, state.termRows[rowIndex]),
                false, false});
            for (std::size_t modelIndex = 0; modelIndex < state.models.size(); ++modelIndex) {
                row.cells.push_back(TableExportCell{
                    GeneralizedComparisonTermCell(
                        state, static_cast<int>(modelIndex), state.termRows[rowIndex]).displayText,
                    true, false});
            }
            payload.rows.push_back(std::move(row));
        }
        auto fitRows = GeneralizedComparisonVisibleFitRows(state);
        if (state.countComparison)
            fitRows.erase(std::remove(fitRows.begin(), fitRows.end(), 1), fitRows.end());
        for (std::size_t rowIndex = 0; rowIndex < fitRows.size(); ++rowIndex) {
            TableExportRow row;
            row.separatorBefore = rowIndex == 0;
            const std::string label = GeneralizedComparisonFitLabel(fitRows[rowIndex]);
            row.cells.push_back(TableExportCell{label, false, true});
            for (std::size_t modelIndex = 0; modelIndex < state.models.size(); ++modelIndex) {
                row.cells.push_back(TableExportCell{
                    GeneralizedComparisonFitCell(
                        state, static_cast<int>(modelIndex), fitRows[rowIndex]).displayText,
                    true, false});
            }
            payload.rows.push_back(std::move(row));
        }
        payload.footnotes.push_back(state.countComparison
            ? CountRegressionComparisonFootnote()
            : GeneralizedComparisonFootnote());

        std::ostringstream csv;
        std::ostringstream markdown;
        for (std::size_t index = 0; index < payload.headers.size(); ++index) {
            if (index) csv << ',';
            csv << CsvTableCell(payload.headers[index].text);
            if (index == 0) markdown << "| "; else markdown << " | ";
            markdown << MarkdownTableCell(payload.headers[index].text);
        }
        csv << '\n';
        markdown << " |\n| :---";
        for (std::size_t index = 1; index < payload.headers.size(); ++index)
            markdown << " | ---:";
        markdown << " |\n";
        for (const TableExportRow& row : payload.rows) {
            for (std::size_t index = 0; index < payload.headers.size(); ++index) {
                const std::string value = index < row.cells.size()
                    ? row.cells[index].text : "";
                if (index) csv << ',';
                csv << CsvTableCell(value);
                if (index == 0) markdown << "| "; else markdown << " | ";
                markdown << MarkdownTableCell(value);
            }
            csv << '\n'; markdown << " |\n";
        }
        payload.csvText = csv.str();
        payload.markdownText = markdown.str();
        return payload;
    }

    std::vector<std::vector<std::string>> ParseExportCsv(std::string const& csv)
    {
        std::vector<std::vector<std::string>> rows;
        std::vector<std::string> row;
        std::string cell;
        bool quoted = false;
        for (std::size_t index = 0; index < csv.size(); ++index) {
            const char value = csv[index];
            if (value == '"') {
                if (quoted && index + 1 < csv.size() && csv[index + 1] == '"') {
                    cell.push_back('"'); ++index;
                } else quoted = !quoted;
            } else if (!quoted && value == ',') {
                row.push_back(std::move(cell)); cell.clear();
            } else if (!quoted && value == '\n') {
                row.push_back(std::move(cell)); cell.clear();
                if (!row.empty()) rows.push_back(std::move(row));
                row.clear();
            } else if (value != '\r') cell.push_back(value);
        }
        if (!cell.empty() || !row.empty()) {
            row.push_back(std::move(cell)); rows.push_back(std::move(row));
        }
        return rows;
    }

    winrt::rlispstatWinUI::implementation::TableExportPayload
    ModelTrellisExportPayload(::rlispstat::core::ModelTrellisState const& state)
    {
        using namespace ::rlispstat::core;
        using winrt::rlispstatWinUI::implementation::TableExportCell;
        using winrt::rlispstatWinUI::implementation::TableExportPayload;
        using winrt::rlispstatWinUI::implementation::TableExportRow;

        TableExportPayload payload;
        payload.title = "Linear Model Trellis";
        payload.subtitle = ModelTrellisFormulaText(state.specification.baseModel) +
            " · Scope: " + state.specification.baseModel.scope +
            " · Adjustment: " + ModelTrellisPAdjustmentLabel(state.specification.pAdjustment);
        payload.baseName = state.id.empty() ? "linear-model-trellis" : state.id;
        payload.csvText = ModelTrellisAllResultsCSV(state);
        const auto parsed = ParseExportCsv(payload.csvText);
        if (!parsed.empty()) {
            for (const std::string& value : parsed.front())
                payload.headers.push_back(TableExportCell{value, false, true});
            for (std::size_t rowIndex = 1; rowIndex < parsed.size(); ++rowIndex) {
                TableExportRow row;
                row.separatorBefore = rowIndex > 1 &&
                    parsed[rowIndex].size() > 2 && parsed[rowIndex - 1].size() > 2 &&
                    parsed[rowIndex][2] != parsed[rowIndex - 1][2];
                for (std::size_t column = 0; column < parsed[rowIndex].size(); ++column)
                    row.cells.push_back(TableExportCell{
                        parsed[rowIndex][column], column >= 7, false});
                payload.rows.push_back(std::move(row));
            }
        }
        payload.footnotes.push_back(ModelTrellisMethodologicalNote());
        std::ostringstream markdown;
        if (!payload.headers.empty()) {
            for (std::size_t index = 0; index < payload.headers.size(); ++index) {
                if (index == 0) markdown << "| "; else markdown << " | ";
                markdown << MarkdownTableCell(payload.headers[index].text);
            }
            markdown << " |\n| :---";
            for (std::size_t index = 1; index < payload.headers.size(); ++index)
                markdown << " | ---:";
            markdown << " |\n";
            for (const TableExportRow& row : payload.rows) {
                for (std::size_t index = 0; index < payload.headers.size(); ++index) {
                    if (index == 0) markdown << "| "; else markdown << " | ";
                    markdown << MarkdownTableCell(index < row.cells.size()
                        ? row.cells[index].text : std::string{});
                }
                markdown << " |\n";
            }
        }
        payload.markdownText = markdown.str();
        return payload;
    }

    void ResizeLogical(Window const& window, double width, double height)
    {
        winrt::rlispstatWinUI::implementation::ResizeLinkEDAWindowClient(
            window, width, height);
    }

    // Result windows share the same visual rhythm.  Narrow reports still get
    // enough room for their controls, while model tables use one common width
    // so changing analysis family does not produce a jarring size jump.
    constexpr double kResultWindowMinimumWidth = 760.0;
    constexpr double kModelResultWindowWidth = 1040.0;
    constexpr double kWideResultWindowMaximumWidth = 1320.0;

    void PresentWindowOnce(Window const& window,
                           bool& presented,
                           double width,
                           double height)
    {
        if (presented) return;
        ResizeLogical(window, width, height);
        window.Activate();
        presented = true;
    }

    // Analysis tables are live editors. Their preferred size changes when a
    // variable row is added or removed, so size on every refresh and activate
    // only on the first presentation. ResizeLinkEDAWindowClient applies the
    // monitor work-area cap; the existing scroll viewers then take over.
    void PresentOrResizeAnalysisWindow(Window const& window,
                                       bool& presented,
                                       double width,
                                       double height,
                                       bool lockToContent = true)
    {
        ResizeLogical(window, width, height);
        // Output tables are sized by their live semantic content.  Prevent a
        // manual resize from leaving empty canvas or becoming the preferred
        // size used after the next term/variable edit.
        if (lockToContent)
        {
            if (auto presenter = window.AppWindow().Presenter().try_as<
                    Microsoft::UI::Windowing::OverlappedPresenter>())
            {
                presenter.IsResizable(false);
                presenter.IsMaximizable(false);
            }
        }
        if (!presented)
        {
            window.Activate();
            presented = true;
        }
    }

    Controls::TextBlock FieldLabel(std::wstring const& text)
    {
        auto label = Controls::TextBlock();
        label.Text(hstring(text));
        label.FontSize(13);
        label.VerticalAlignment(VerticalAlignment::Center);
        return label;
    }

    Controls::TextBlock DialogFieldLabel(std::wstring const& text)
    {
        auto label = FieldLabel(text);
        label.FontSize(kDialogBodyFontSize);
        return label;
    }

    void ConfigureDialogList(Controls::ListView const& list)
    {
        list.FontSize(kDialogBodyFontSize);
        // Keep list rows aligned with the compact 24-DIP rhythm used by the
        // macOS accessory views. DIPs remain subject to Windows per-monitor DPI.
        list.Resources().Insert(box_value(L"ListViewItemMinHeight"), box_value(24.0));
    }

    Controls::StackPanel DialogContentIntroduction(
        std::wstring const& description,
        std::string const& dataset)
    {
        auto block = Controls::StackPanel(); block.Spacing(10);
        auto help = DialogFieldLabel(description); help.Opacity(0.78);
        help.TextWrapping(TextWrapping::Wrap); block.Children().Append(help);
        auto datasetLabel = DialogFieldLabel(std::wstring(L"Dataset: ") + to_hstring(dataset).c_str());
        datasetLabel.FontWeight(Windows::UI::Text::FontWeights::SemiBold()); block.Children().Append(datasetLabel);
        return block;
    }

    void ConfigureNativeDialog(
        Controls::ContentDialog const& dialog,
        Window const& owner,
        std::wstring const& title,
        UIElement const& content,
        std::wstring const& primaryButton,
        double minimumWidth)
    {
        auto ownerRoot = owner.Content().try_as<FrameworkElement>();
        if (!ownerRoot || !ownerRoot.XamlRoot())
            throw hresult_error(E_UNEXPECTED, L"The dialog owner is not loaded.");
        dialog.XamlRoot(ownerRoot.XamlRoot());
        auto compactResources = ResourceDictionary();
        compactResources.Source(Windows::Foundation::Uri(
            L"ms-appx:///Microsoft.UI.Xaml/DensityStyles/Compact.xaml"));
        dialog.Resources().MergedDictionaries().Append(compactResources);
        auto titleBlock = Controls::TextBlock();
        titleBlock.Text(hstring(title));
        titleBlock.FontSize(kDialogTitleFontSize);
        titleBlock.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        dialog.Title(titleBlock);
        dialog.FontSize(kDialogBodyFontSize);
        dialog.Content(content);
        dialog.PrimaryButtonText(hstring(primaryButton));
        dialog.CloseButtonText(L"Cancel");
        dialog.DefaultButton(Controls::ContentDialogButton::Primary);
        dialog.RequestedTheme(ElementTheme::Light);
        dialog.Resources().Insert(box_value(L"ContentDialogMinWidth"), box_value(minimumWidth));
        dialog.Resources().Insert(box_value(L"ContentDialogMaxWidth"), box_value(minimumWidth + 96.0));
    }

    void ShowStatisticExplanation(
        Controls::TextBlock const& owner,
        ::rlispstat::core::ModelStatisticExplanationContext const& context)
    {
        if (!owner || !owner.XamlRoot()) return;
        auto body = Controls::TextBlock();
        body.Text(to_hstring(::rlispstat::core::ExplainModelStatistic(context)));
        body.TextWrapping(TextWrapping::Wrap);
        body.MaxWidth(520.0);
        auto dialog = Controls::ContentDialog();
        dialog.XamlRoot(owner.XamlRoot());
        dialog.Title(box_value(to_hstring("Explain statistic: " + context.statistic)));
        dialog.Content(body);
        dialog.CloseButtonText(L"Close");
        dialog.DefaultButton(Controls::ContentDialogButton::Close);
        auto operation = dialog.ShowAsync();
        operation.Completed([dialog](auto const&, Windows::Foundation::AsyncStatus) {});
    }

    void ShowWorkflowMessage(Window const& owner,
                             std::wstring const& title,
                             std::string const& message)
    {
        if (!owner) return;
        auto root = owner.Content().try_as<FrameworkElement>();
        if (!root || !root.XamlRoot()) return;
        auto body = Controls::TextBlock();
        body.Text(to_hstring(message));
        body.TextWrapping(TextWrapping::Wrap);
        body.MaxWidth(520.0);
        auto dialog = Controls::ContentDialog();
        dialog.XamlRoot(root.XamlRoot());
        dialog.Title(box_value(hstring(title)));
        dialog.Content(body);
        dialog.CloseButtonText(L"Close");
        dialog.DefaultButton(Controls::ContentDialogButton::Close);
        auto operation = dialog.ShowAsync();
        operation.Completed([dialog](auto const&, Windows::Foundation::AsyncStatus) {});
    }

    Controls::Button FooterButton(std::wstring const& text)
    {
        auto button = Controls::Button();
        button.Content(box_value(hstring(text)));
        button.MinWidth(82);
        return button;
    }

    Shapes::Line ReportLine(double x1, double y1, double x2, double y2,
                            Media::Brush const& stroke, double thickness = 1.0)
    {
        auto line = Shapes::Line(); line.X1(x1); line.Y1(y1); line.X2(x2); line.Y2(y2);
        line.Stroke(stroke); line.StrokeThickness(thickness); return line;
    }

    Controls::TextBlock ReportLabel(std::wstring const& text, double left, double top,
                                    double fontSize, Media::Brush const& foreground)
    {
        auto label = Controls::TextBlock(); label.Text(hstring(text)); label.FontSize(fontSize);
        label.Foreground(foreground); Controls::Canvas::SetLeft(label, left);
        Controls::Canvas::SetTop(label, top); return label;
    }

    Controls::Border TableCell(std::wstring const& text, bool header, bool label,
                               bool semibold = false, bool muted = false,
                               bool indented = false, bool separator = false)
    {
        auto value = Controls::TextBlock();
        value.Text(hstring(text));
        value.FontSize(muted ? 12 : 13);
        value.TextWrapping(TextWrapping::NoWrap);
        value.TextTrimming(TextTrimming::CharacterEllipsis);
        value.HorizontalAlignment(label ? HorizontalAlignment::Left : HorizontalAlignment::Right);
        value.VerticalAlignment(VerticalAlignment::Center);
        value.Foreground(Brush(muted ? 100 : 39, muted ? 100 : 42, muted ? 100 : 46));
        if (header || semibold) value.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        auto border = Controls::Border();
        border.Padding(Thickness{ label && indented ? 30.0 : 10.0, 4, 10, 4 });
        border.MinHeight(28);
        border.BorderBrush(Brush(header ? 218 : 239, header ? 221 : 240, header ? 225 : 242));
        border.BorderThickness(Thickness{ 0, separator ? 1.0 : 0.0, 0, header ? 1.0 : 0.0 });
        border.Child(value);
        return border;
    }

    std::wstring ItemText(Windows::Foundation::IInspectable const& item)
    {
        return unbox_value_or<hstring>(item, L"").c_str();
    }

    std::string NewRequestId(std::string const& prefix)
    {
        static std::atomic<unsigned long long> counter{ 0 };
        return prefix + std::to_string(static_cast<unsigned long long>(GetTickCount64())) +
            "_" + std::to_string(++counter);
    }

    std::string DefaultRObjectName(std::string value)
    {
        if (value.empty()) return "linkeda_data";
        for (auto& ch : value)
            if (!(std::isalnum(static_cast<unsigned char>(ch)) || ch == '.' || ch == '_'))
                ch = '_';
        if (std::isdigit(static_cast<unsigned char>(value.front())) ||
            (value.front() == '.' && value.size() > 1 &&
             std::isdigit(static_cast<unsigned char>(value[1]))))
            value = "data_" + value;
        return value;
    }

    winrt::rlispstatWinUI::implementation::TableExportPayload
    ScaleAnalysisDerivedExportPayload(
        ::rlispstat::core::ScaleAnalysisState const& state,
        ::rlispstat::core::ScaleAnalysisChildKind kind,
        int selectedDimensionImputation = 1)
    {
        using namespace ::rlispstat::core;
        using winrt::rlispstatWinUI::implementation::TableExportCell;
        using winrt::rlispstatWinUI::implementation::TableExportPayload;
        using winrt::rlispstatWinUI::implementation::TableExportRow;
        TableExportPayload payload;
        const bool mi = state.result.imputationCount > 1 ||
            state.result.backend == "multiple_imputation";
        payload.title = ScaleAnalysisChildWindowTitle(kind, mi);
        payload.subtitle = "Scale Analysis revision " +
            std::to_string(state.specification.revision) + " · exact specification fingerprint";
        payload.baseName = state.id + "-" + std::to_string(static_cast<int>(kind));
        std::vector<std::string> headers;
        std::vector<std::vector<std::string>> rows;
        if (kind == ScaleAnalysisChildKind::Reliability) {
            headers = {"Imputation", "Alpha", "Standardized alpha", "Omega total"};
            for (auto const& row : state.result.reliabilityByImputation)
                rows.push_back({row.imputation,row.alpha,row.standardizedAlpha,row.omegaTotal});
            payload.footnotes.push_back("Reliability is computed in every completed dataset; MI aggregates are descriptive, not Rubin-pooled.");
        } else if (kind == ScaleAnalysisChildKind::InterItemCorrelations ||
                   kind == ScaleAnalysisChildKind::CorrelationHeatmap) {
            auto const& c = state.result.correlations; headers = {"Item"};
            headers.insert(headers.end(), c.variables.begin(), c.variables.end());
            const std::size_t n = c.variables.size();
            for (std::size_t r=0;r<n;++r) { std::vector<std::string> row{c.variables[r]};
                for (std::size_t column=0;column<n;++column) { const auto index=r*n+column;
                    row.push_back(index<c.values.size()?c.values[index]:""); } rows.push_back(std::move(row)); }
            payload.footnotes.push_back("Correlation basis: " + c.basis + " · " + c.status);
        } else if (kind == ScaleAnalysisChildKind::Dimensionality ||
                   kind == ScaleAnalysisChildKind::ScreePlot) {
            const auto dimensionality = BuildScaleDimensionalityPresentation(
                state.result, selectedDimensionImputation);
            headers = {"Section", "Label", "Estimate", "Reference"};
            for (auto const& row : ScaleFactorCountRows(state.result))
                rows.push_back({"Factor-count stability",std::to_string(row.factorCount),
                    std::to_string(row.imputations),FormatDouble(100*row.proportion,1)+"%"});
            for (auto const& component : dimensionality.components)
                rows.push_back({"Imputation " + std::to_string(dimensionality.imputation),
                    "F" + std::to_string(component.index),
                    FormatDouble(component.eigenvalue, 6),
                    FormatDouble(component.parallelEigenvalue, 6)});
            payload.footnotes.push_back(dimensionality.status);
        } else if (kind == ScaleAnalysisChildKind::ScaleScores) {
            auto const& s=state.result.scores;
            headers={"Method",mi?"N per imputation":"Valid N","Mean","SD","Minimum","Maximum"};
            rows.push_back({s.method,s.validN,s.mean,s.sd,s.minimum,s.maximum});
            payload.footnotes.push_back(s.status);
        } else if (kind == ScaleAnalysisChildKind::ItemRestPlot ||
                   kind == ScaleAnalysisChildKind::ReliabilityIfDeletedPlot) {
            headers={"Item",kind==ScaleAnalysisChildKind::ItemRestPlot?"Item-rest r":"Alpha if deleted"};
            for(auto const& row:state.result.items) rows.push_back({row.variable,
                kind==ScaleAnalysisChildKind::ItemRestPlot?row.itemRestCorrelation:row.alphaIfDeleted});
        } else if (kind == ScaleAnalysisChildKind::LoadingPlot ||
                   kind == ScaleAnalysisChildKind::Biplot) {
            const auto dimensionality = BuildScaleDimensionalityPresentation(
                state.result, selectedDimensionImputation);
            headers={"Item"};
            std::size_t factorCount = 0;
            for (auto const& row : dimensionality.loadings)
                factorCount = std::max(factorCount, row.values.size());
            for (std::size_t factor = 0; factor < factorCount; ++factor)
                headers.push_back("F" + std::to_string(factor + 1));
            headers.push_back("h2"); headers.push_back("u2");
            for (auto const& loading : dimensionality.loadings) {
                std::vector<std::string> row{loading.variable};
                for (std::size_t factor = 0; factor < factorCount; ++factor)
                    row.push_back(factor < loading.values.size()
                        ? FormatDouble(loading.values[factor], 6) : "");
                row.push_back(FormatDouble(loading.communality, 6));
                row.push_back(FormatDouble(loading.uniqueness, 6));
                rows.push_back(std::move(row));
            }
            payload.footnotes.push_back(dimensionality.status);
        } else {
            headers={"Series","Label","Value"};
            std::string requested = kind==ScaleAnalysisChildKind::ItemDistributionsPlot
                ? "item_distribution" : kind==ScaleAnalysisChildKind::ScaleScoreDistributionPlot
                ? "score_distribution" : "";
            for(auto const& series:state.result.plotSeries){if(!requested.empty()&&series.kind!=requested)continue;
                for(std::size_t i=0;i<std::min(series.labels.size(),series.values.size());++i)
                    rows.push_back({series.name,series.labels[i],series.values[i]});}
        }
        for (auto const& header : headers) payload.headers.push_back(TableExportCell{header,false,true});
        for (auto const& values : rows) { TableExportRow row;
            for (std::size_t i=0;i<values.size();++i) row.cells.push_back(TableExportCell{values[i],i>0,false});
            payload.rows.push_back(std::move(row)); }
        auto csv = [](std::string const& value) { std::string escaped=value; std::size_t pos=0;
            while((pos=escaped.find('"',pos))!=std::string::npos){escaped.insert(pos,"\"");pos+=2;}
            return "\""+escaped+"\""; };
        std::ostringstream csvText, markdown;
        for(std::size_t i=0;i<headers.size();++i){if(i)csvText<<',';csvText<<csv(headers[i]);}
        csvText<<'\n';
        markdown<<"|";for(auto const& h:headers)markdown<<" "<<h<<" |";markdown<<"\n|";
        for(std::size_t i=0;i<headers.size();++i)markdown<<" --- |";markdown<<"\n";
        for(auto const& row:rows){for(std::size_t i=0;i<row.size();++i){if(i)csvText<<',';csvText<<csv(row[i]);}
            csvText<<'\n';markdown<<"|";for(auto const& value:row)markdown<<" "<<value<<" |";markdown<<"\n";}
        payload.csvText=csvText.str(); payload.markdownText=markdown.str();
        return payload;
    }

    winrt::rlispstatWinUI::implementation::TableExportPayload
    InteractionReportExportPayload(
        std::string const& id, std::string const& title,
        std::string const& subtitle,
        ::rlispstat::core::GLMInteractionReport const& report)
    {
        using namespace ::rlispstat::core;
        using winrt::rlispstatWinUI::implementation::TableExportCell;
        using winrt::rlispstatWinUI::implementation::TableExportPayload;
        using winrt::rlispstatWinUI::implementation::TableExportRow;
        TableExportPayload payload;
        payload.title = title;
        payload.subtitle = subtitle;
        payload.baseName = id + "-interaction";
        for (auto const& header : std::array<std::string, 10>{
                 "Section", "Effect", "Measure", "Estimate", "SE", "df",
                 "Statistic", "p", "Lower 95%", "Upper 95%"})
            payload.headers.push_back(TableExportCell{
                header, header != "Section" && header != "Effect" &&
                    header != "Measure", true});
        for (auto const& section : report.sections) {
            for (std::size_t index = 0; index < section.second.size(); ++index) {
                auto const& estimate = section.second[index];
                std::string identity = estimate.label;
                if (index < section.identityRows.size() &&
                    !section.identityRows[index].empty()) {
                    identity.clear();
                    for (auto const& value : section.identityRows[index]) {
                        if (!identity.empty()) identity += " · ";
                        identity += value;
                    }
                }
                const bool estimateOnly = section.inference ==
                    GLMInteractionSectionInference::EstimateOnly;
                const bool adjusted = section.inference ==
                    GLMInteractionSectionInference::AdjustedComparison;
                TableExportRow row;
                row.separatorBefore = index == 0 && !payload.rows.empty();
                row.cells = {
                    {section.first, false, index == 0}, {identity},
                    {section.estimateHeader.empty() ? "Estimate" : section.estimateHeader},
                    {FormatDoubleOrDash(estimate.estimate, 4), true},
                    {FormatDoubleOrDash(estimate.stdError, 4), true},
                    {section.showDegreesOfFreedom
                        ? FormatDoubleOrDash(estimate.degreesOfFreedom, 2) : "—", true},
                    {estimateOnly ? "—" : FormatDoubleOrDash(estimate.statistic, 3), true},
                    {estimateOnly ? "—" : FormatPValue(
                        adjusted && std::isfinite(estimate.adjustedPValue)
                            ? estimate.adjustedPValue : estimate.pValue), true},
                    {FormatDoubleOrDash(estimate.ciLower, 4), true},
                    {FormatDoubleOrDash(estimate.ciUpper, 4), true}
                };
                payload.rows.push_back(std::move(row));
            }
        }
        payload.footnotes = report.notes;
        if (!report.interpretation.empty())
            payload.footnotes.insert(payload.footnotes.begin(), report.interpretation);
        if (!report.message.empty()) payload.warnings.push_back(report.message);
        return payload;
    }

    winrt::rlispstatWinUI::implementation::TableExportPayload
    CorrelationExportPayload(::rlispstat::core::CorrelationMatrixState const& state)
    {
        using winrt::rlispstatWinUI::implementation::TableExportCell;
        using winrt::rlispstatWinUI::implementation::TableExportPayload;
        using winrt::rlispstatWinUI::implementation::TableExportRow;
        TableExportPayload payload;
        payload.title = state.title;
        payload.subtitle = state.method + " · " + state.missingMode;
        payload.baseName = state.id + "-correlations";
        payload.headers.push_back(TableExportCell{"Variable", false, true});
        for (auto const& variable : state.variables)
            payload.headers.push_back(TableExportCell{variable, true, true});
        for (std::size_t rowIndex = 0; rowIndex < state.variables.size(); ++rowIndex) {
            TableExportRow row;
            row.cells.push_back(TableExportCell{state.variables[rowIndex], false, true});
            for (std::size_t column = 0; column < state.variables.size(); ++column)
                row.cells.push_back(TableExportCell{
                    ::rlispstat::core::CorrelationDisplayedCellText(
                        state, static_cast<int>(rowIndex), static_cast<int>(column)), true});
            payload.rows.push_back(std::move(row));
        }
        if (!state.note.empty()) payload.footnotes.push_back(state.note);
        if (state.multipleImputation)
            payload.footnotes.push_back("Multiple imputation: m = " +
                std::to_string(state.imputationCount) + "; " + state.poolingMethod + ".");
        return payload;
    }

    winrt::rlispstatWinUI::implementation::TableExportPayload
    DimensionalityExportPayload(::rlispstat::core::DimensionalityState const& state)
    {
        using namespace ::rlispstat::core;
        using winrt::rlispstatWinUI::implementation::TableExportCell;
        using winrt::rlispstatWinUI::implementation::TableExportPayload;
        using winrt::rlispstatWinUI::implementation::TableExportRow;
        TableExportPayload payload;
        payload.title = DimensionalityWindowTitle();
        payload.subtitle = state.method + " · " + state.missingMode + " · " + state.rotation;
        payload.baseName = state.id + "-dimensionality";
        const std::size_t loadingCount = state.loadings.empty()
            ? 0 : state.loadings.front().values.size();
        for (auto const& header : {"Section", "Variable / dimension", "Eigenvalue",
                                   "Parallel", "Variance", "Cumulative"})
            payload.headers.push_back(TableExportCell{header,
                std::string(header) != "Section" && std::string(header) != "Variable / dimension", true});
        for (std::size_t index = 0; index < loadingCount; ++index)
            payload.headers.push_back(TableExportCell{
                (state.method == "pca" ? "PC" : "F") + std::to_string(index + 1), true, true});
        payload.headers.push_back(TableExportCell{"h²", true, true});
        payload.headers.push_back(TableExportCell{"u²", true, true});
        for (auto const& component : state.components) {
            TableExportRow row;
            row.cells = {{"Components", false, true},
                {(state.method == "pca" ? "PC" : "F") + std::to_string(component.index)},
                {FormatDoubleOrDash(component.eigenvalue, 4), true},
                {FormatDoubleOrDash(component.parallelEigenvalue, 4), true},
                {FormatPercent(component.variance, 1), true},
                {FormatPercent(component.cumulative, 1), true}};
            while (row.cells.size() < payload.headers.size())
                row.cells.push_back(TableExportCell{"—", true});
            payload.rows.push_back(std::move(row));
        }
        for (auto const& loading : state.loadings) {
            TableExportRow row;
            row.separatorBefore = &loading == &state.loadings.front();
            row.cells = {{"Loadings", false, true}, {loading.variable},
                         {"—", true}, {"—", true}, {"—", true}, {"—", true}};
            for (std::size_t index = 0; index < loadingCount; ++index)
                row.cells.push_back(TableExportCell{
                    index < loading.values.size()
                        ? FormatDoubleOrDash(loading.values[index], 4) : "—", true});
            row.cells.push_back(TableExportCell{
                FormatDoubleOrDash(loading.communality, 4), true});
            row.cells.push_back(TableExportCell{
                FormatDoubleOrDash(loading.uniqueness, 4), true});
            payload.rows.push_back(std::move(row));
        }
        if (!state.status.empty()) payload.footnotes.push_back(state.status);
        if (!state.calculationMethod.empty())
            payload.footnotes.push_back(state.calculationMethod);
        return payload;
    }

    winrt::rlispstatWinUI::implementation::TableExportPayload
    ScaleAnalysisExportPayload(::rlispstat::core::ScaleAnalysisState const& state)
    {
        using winrt::rlispstatWinUI::implementation::TableExportCell;
        using winrt::rlispstatWinUI::implementation::TableExportPayload;
        using winrt::rlispstatWinUI::implementation::TableExportRow;
        TableExportPayload payload;
        const bool mi = state.result.imputationCount > 1 ||
            state.result.backend == "multiple_imputation";
        payload.title = ::rlispstat::core::ScaleAnalysisWindowTitle(mi);
        payload.subtitle = state.status;
        payload.baseName = state.id + "-scale-analysis";
        for (auto const& header : {"Section", "Item / statistic", "Scoring",
                                   "Value / M", "SD", "Missing (%)",
                                   "Item-rest r", "Alpha if deleted"})
            payload.headers.push_back(TableExportCell{header,
                std::string(header) != "Section" && std::string(header) != "Item / statistic" &&
                std::string(header) != "Scoring", true});
        for (auto const& field : ::rlispstat::core::ScaleAnalysisSummaryFields(state.result)) {
            TableExportRow row;
            row.cells = {{"Summary", false, true}, {field.first}, {""}, {field.second, true}};
            while (row.cells.size() < payload.headers.size()) row.cells.push_back(TableExportCell{});
            payload.rows.push_back(std::move(row));
        }
        for (auto const& item : state.result.items) {
            TableExportRow row;
            row.separatorBefore = &item == &state.result.items.front();
            row.cells = {{"Items", false, true}, {item.variable},
                {item.direction == "reversed" ? "Reversed" : "Forward"},
                {item.mean, true}, {item.sd, true}, {item.missingPercent, true},
                {item.itemRestCorrelation, true}, {item.alphaIfDeleted, true}};
            payload.rows.push_back(std::move(row));
        }
        payload.footnotes.push_back(
            "Item-rest r excludes the item from the remaining-item total. Alpha if deleted is recalculated after removing the item.");
        if (mi) payload.footnotes.push_back(
            "Reliability and scale summaries across imputations are descriptive and are not Rubin-pooled.");
        return payload;
    }
}

namespace winrt::rlispstatWinUI::implementation
{
    static std::optional<::rlispstat::core::SharedAnalysisKind>
    SharedKindForWorkflow(AnalysisWorkflowKind kind)
    {
        using Shared = ::rlispstat::core::SharedAnalysisKind;
        switch (kind)
        {
        case AnalysisWorkflowKind::Table1: return Shared::DescriptiveTable;
        case AnalysisWorkflowKind::CorrelationMatrix: return Shared::CorrelationMatrix;
        case AnalysisWorkflowKind::Dimensionality:
        case AnalysisWorkflowKind::FactorAnalysis: return Shared::Dimensionality;
        case AnalysisWorkflowKind::ScaleAnalysis: return Shared::ScaleAnalysis;
        case AnalysisWorkflowKind::QuickCluster: return Shared::QuickCluster;
        case AnalysisWorkflowKind::OneSampleT: return Shared::OneSampleComparison;
        case AnalysisWorkflowKind::IndependentT: return Shared::IndependentSamplesComparison;
        case AnalysisWorkflowKind::PairedT: return Shared::PairedSamplesComparison;
        case AnalysisWorkflowKind::OneWayAnova: return Shared::OneWayComparison;
        case AnalysisWorkflowKind::LinearModel: return Shared::LinearModel;
        case AnalysisWorkflowKind::LinearModelTrellis: return Shared::LinearModelTrellis;
        case AnalysisWorkflowKind::RegressionComparison: return Shared::RegressionComparison;
        case AnalysisWorkflowKind::BinaryRegression:
        case AnalysisWorkflowKind::BinaryRegressionComparison: return Shared::BinaryRegression;
        case AnalysisWorkflowKind::GeneralizedLinearModel: return Shared::GeneralizedLinearModel;
        case AnalysisWorkflowKind::CountRegression:
        case AnalysisWorkflowKind::CountRegressionComparison: return Shared::CountRegression;
        case AnalysisWorkflowKind::PositiveContinuousModel:
        case AnalysisWorkflowKind::PositiveContinuousComparison:
            return Shared::PositiveContinuousModel;
        case AnalysisWorkflowKind::ProportionModel:
        case AnalysisWorkflowKind::ProportionComparison:
            return Shared::ProportionModel;
        case AnalysisWorkflowKind::GeneralizedComparison: return Shared::GeneralizedComparison;
        case AnalysisWorkflowKind::LinearMixedModel: return Shared::LinearMixedModel;
        case AnalysisWorkflowKind::GeneralizedMixedModel: return Shared::GeneralizedMixedModel;
        }
        return std::nullopt;
    }

    static ::rlispstat::core::AnalysisSlotDefinition const*
    FindSharedSlot(::rlispstat::core::AnalysisDefinition const& definition,
                   std::initializer_list<const char*> ids)
    {
        for (const char* id : ids)
            for (auto const& slot : definition.slots)
                if (slot.id == id) return &slot;
        return nullptr;
    }

    static std::optional<::rlispstat::core::StatisticalModelType>
    StatisticalModelTypeForWorkflow(AnalysisWorkflowKind kind)
    {
        using Type = ::rlispstat::core::StatisticalModelType;
        switch (kind)
        {
        case AnalysisWorkflowKind::BinaryRegression:
        case AnalysisWorkflowKind::BinaryRegressionComparison: return Type::Binary;
        case AnalysisWorkflowKind::CountRegression:
        case AnalysisWorkflowKind::CountRegressionComparison: return Type::Count;
        case AnalysisWorkflowKind::PositiveContinuousModel:
        case AnalysisWorkflowKind::PositiveContinuousComparison:
            return Type::PositiveContinuous;
        case AnalysisWorkflowKind::ProportionModel:
        case AnalysisWorkflowKind::ProportionComparison:
            return Type::Proportion;
        case AnalysisWorkflowKind::GeneralizedLinearModel:
        case AnalysisWorkflowKind::GeneralizedComparison: return Type::LegacyGeneralized;
        default: return std::nullopt;
        }
    }

    static ::rlispstat::core::SharedAnalysisKind
    SharedKindForPlotWorkflow(PlotWorkflowKind kind,
                              std::string const& trellisType = "scatter")
    {
        using Shared = ::rlispstat::core::SharedAnalysisKind;
        if (kind == PlotWorkflowKind::Trellis)
        {
            if (trellisType == "time_series") return Shared::TimeSeries;
            if (trellisType == "boxplot") return Shared::Boxplot;
            if (trellisType == "bar") return Shared::BarChart;
            if (trellisType == "histogram") return Shared::Histogram;
            return Shared::TrellisPlot;
        }
        switch (kind)
        {
        case PlotWorkflowKind::TimeSeries: return Shared::TimeSeries;
        case PlotWorkflowKind::ScatterMatrix: return Shared::ScatterplotMatrix;
        case PlotWorkflowKind::ParallelCoordinates: return Shared::ParallelCoordinates;
        case PlotWorkflowKind::Boxplot: return Shared::Boxplot;
        case PlotWorkflowKind::Histogram: return Shared::Histogram;
        case PlotWorkflowKind::BarChart: return Shared::BarChart;
        default: return Shared::Scatterplot;
        }
    }

    std::shared_ptr<VariablesWindow> VariablesWindow::Create()
    {
        auto view = std::shared_ptr<VariablesWindow>(new VariablesWindow());
        view->AttachLifetime();
        return view;
    }

    VariablesWindow::VariablesWindow()
    {
        Initialize();
    }

    void VariablesWindow::Initialize()
    {
        window_ = CreateLinkEDAWindow();
        window_.Title(to_hstring(::rlispstat::core::VariableViewTitle()));

        auto root = Controls::Grid();
        root.Padding(Thickness{ 18, 14, 18, 10 });
        root.RowSpacing(6);
        root.Background(Brush(255, 255, 255));
        root.RequestedTheme(ElementTheme::Light);
        auto autoRow = []()
        {
            auto row = Controls::RowDefinition();
            row.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Auto));
            return row;
        };
        root.RowDefinitions().Append(autoRow());
        root.RowDefinitions().Append(autoRow());
        auto contentRow = Controls::RowDefinition();
        contentRow.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
        root.RowDefinitions().Append(contentRow);
        root.RowDefinitions().Append(autoRow());

        title_ = Controls::TextBlock();
        title_.FontSize(17);
        title_.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        title_.Text(to_hstring(::rlispstat::core::VariableViewTitle()));
        root.Children().Append(title_);

        hint_ = Controls::TextBlock();
        hint_.FontSize(11.5);
        hint_.Foreground(Brush(96, 96, 96));
        hint_.Text(to_hstring(::rlispstat::core::VariableViewInstructionText()));
        hint_.TextWrapping(TextWrapping::Wrap);
        Controls::Grid::SetRow(hint_, 1);
        root.Children().Append(hint_);

        auto scroll = Controls::ScrollViewer();
        scroll.HorizontalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);
        scroll.VerticalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);
        table_ = Controls::Grid();
        table_.MinWidth(620);
        table_.RowSpacing(0);
        std::vector<double> widths{ 1.5, 1.1, 0.8 };
        if (::rlispstat::core::DefaultVariableRolesExperimentalFeatureEnabled())
            widths.push_back(1.5);
        widths.push_back(2.5);
        for (double width : widths)
        {
            auto column = Controls::ColumnDefinition();
            column.Width(GridLengthHelper::FromValueAndType(width, GridUnitType::Star));
            table_.ColumnDefinitions().Append(column);
        }
        scroll.Content(table_);
        Controls::Grid::SetRow(scroll, 2);
        root.Children().Append(scroll);

        status_ = Controls::TextBlock();
        status_.FontSize(11.5);
        status_.Foreground(Brush(92, 92, 92));
        Controls::Grid::SetRow(status_, 3);
        root.Children().Append(status_);

        window_.Content(root);
        ResizeLogical(window_, 760, 520);
    }

    void VariablesWindow::AttachLifetime()
    {
        std::weak_ptr<VariablesWindow> weak = shared_from_this();
        window_.Closed([weak](auto const&, auto const&)
        {
            if (auto view = weak.lock())
            {
                if (view->closed_) return;
                view->closed_ = true;
                if (view->closedCallback_) view->closedCallback_();
            }
        });
    }

    void VariablesWindow::SetClosedCallback(Closed callback)
    {
        closedCallback_ = std::move(callback);
    }

    void VariablesWindow::SetCommandCallback(Command callback)
    {
        commandCallback_ = std::move(callback);
    }

    std::string VariablesWindow::Dispatch(std::vector<std::string> const& command)
    {
        if (!commandCallback_) return "ERR command dispatcher is unavailable";
        return commandCallback_(command);
    }

    void VariablesWindow::Show(
        ::rlispstat::core::DataFrameModel const& dataframe,
        std::map<std::string, std::string> const& roles,
        bool activate)
    {
        dataframe_ = dataframe;
        roles_ = roles;
        SetWindowDataSheetGroup(window_, dataframe_.group);
        const auto windowTitle = ::rlispstat::core::VariableViewWindowTitle(dataframe_.group);
        window_.Title(to_hstring(windowTitle));
        title_.Text(to_hstring(::rlispstat::core::VariableViewTitle()));
        status_.Text(to_hstring(std::to_string(dataframe_.columns.size()) +
            " variables in " + dataframe_.group));
        std::string dependent;
        std::size_t predictors = 0;
        for (auto const& [variable, role] : roles_)
        {
            if (role == "dependent") dependent = variable;
            else if (role == "independent") ++predictors;
        }
        if (::rlispstat::core::DefaultVariableRolesExperimentalFeatureEnabled())
            status_.Text(to_hstring(::rlispstat::core::VariableViewModelRoleSummary(
                dependent, predictors)));
        RebuildRows();
        ResizeLogical(window_, 760, 520);
        if (activate) window_.Activate();
    }

    void VariablesWindow::RebuildRows()
    {
        updating_ = true;
        table_.Children().Clear();
        table_.RowDefinitions().Clear();

        auto configureEditableCell = [](Controls::TextBox const& editor)
        {
            editor.FontSize(11.5);
            editor.Height(28);
            editor.Padding(Thickness{ 7, 0, 7, 0 });
            editor.Margin(Thickness{ 0 });
            editor.Background(TransparentBrush());
            editor.BorderThickness(Thickness{ 0 });
            editor.VerticalContentAlignment(VerticalAlignment::Center);
        };

        auto textCellButton = [](std::wstring const& text)
        {
            auto button = Controls::Button();
            button.Content(box_value(hstring(text)));
            button.FontSize(11.5);
            button.FontWeight(Windows::UI::Text::FontWeights::Normal());
            button.Height(28);
            button.Padding(Thickness{ 7, 0, 7, 0 });
            button.Margin(Thickness{ 0 });
            button.Background(TransparentBrush());
            button.BorderThickness(Thickness{ 0 });
            button.HorizontalAlignment(HorizontalAlignment::Stretch);
            button.HorizontalContentAlignment(HorizontalAlignment::Left);
            button.VerticalContentAlignment(VerticalAlignment::Center);
            return button;
        };

        auto addRow = [this]()
        {
            auto row = Controls::RowDefinition();
            row.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Auto));
            table_.RowDefinitions().Append(row);
            return static_cast<int>(table_.RowDefinitions().Size() - 1);
        };
        const int headerRow = addRow();
        const bool showDefaultRole =
            ::rlispstat::core::DefaultVariableRolesExperimentalFeatureEnabled();
        std::vector<std::string> headers{
            ::rlispstat::core::FormatNameColumnHeader(),
            ::rlispstat::core::FormatTypeColumnHeader(),
            ::rlispstat::core::FormatDecimalsColumnHeader()
        };
        if (showDefaultRole)
            headers.push_back(::rlispstat::core::FormatRoleColumnHeader());
        headers.push_back(::rlispstat::core::FormatDescriptionColumnHeader());
        for (int column = 0; column < static_cast<int>(headers.size()); ++column)
        {
            auto cell = TableCell(to_hstring(headers[static_cast<std::size_t>(column)]).c_str(),
                                  true, true);
            Controls::Grid::SetRow(cell, headerRow);
            Controls::Grid::SetColumn(cell, column);
            table_.Children().Append(cell);
        }

        const auto typeOptions = ::rlispstat::core::VariableViewTypeOptions();
        const auto roleOptions = ::rlispstat::core::VariableViewRoleOptions();
        for (auto const& variable : ::rlispstat::core::VariableViewRowsForDataFrame(dataframe_))
        {
            const int row = addRow();
            auto name = Controls::TextBox();
            name.Text(to_hstring(variable.name));
            configureEditableCell(name);
            name.LostFocus([this, name, original = variable.name](auto const&, auto const&)
            {
                if (updating_) return;
                const std::string value = to_string(name.Text());
                if (value == original) return;
                const std::string reply = Dispatch({ "RENAME_VARIABLE", dataframe_.group,
                                                     original, value });
                status_.Text(to_hstring(reply.rfind("OK\t", 0) == 0 ? reply.substr(3) : reply));
                if (reply.rfind("ERR ", 0) == 0) name.Text(to_hstring(original));
            });
            Controls::Grid::SetRow(name, row); Controls::Grid::SetColumn(name, 0);
            table_.Children().Append(name);

            auto type = textCellButton(to_hstring(
                ::rlispstat::core::VariableTypeDisplayName(variable.type)).c_str());
            auto typeMenu = Controls::MenuFlyout();
            for (auto const& option : typeOptions)
            {
                auto item = Controls::ToggleMenuFlyoutItem();
                item.Text(to_hstring(option.title));
                std::string currentType = ::rlispstat::core::NormalizeVariableType(variable.type);
                if (currentType == "logical") currentType = "factor";
                item.IsChecked(currentType == option.value);
                item.Click([this, variableName = variable.name, selectedType = option.value](auto const&, auto const&)
                {
                    if (updating_) return;
                    const std::string reply = Dispatch({ "SET_VARIABLE_TYPE", dataframe_.group,
                                                         variableName, selectedType });
                    status_.Text(to_hstring(reply.rfind("OK\t", 0) == 0 ? reply.substr(3) : reply));
                });
                typeMenu.Items().Append(item);
            }
            type.Flyout(typeMenu);
            Controls::Grid::SetRow(type, row); Controls::Grid::SetColumn(type, 1);
            table_.Children().Append(type);

            const bool numeric = ::rlispstat::core::NormalizeVariableType(variable.type) == "numeric";
            const std::string decimalText = !numeric ? "" : variable.decimals < 0
                ? ::rlispstat::core::FormatAutoLabel() : std::to_string(variable.decimals);
            auto decimals = textCellButton(to_hstring(decimalText).c_str());
            decimals.IsEnabled(numeric);
            if (numeric)
            {
                auto decimalsMenu = Controls::MenuFlyout();
                for (int value = -1; value <= 12; ++value)
                {
                    auto item = Controls::ToggleMenuFlyoutItem();
                    item.Text(value < 0 ? to_hstring(::rlispstat::core::FormatAutoLabel())
                                        : to_hstring(std::to_string(value)));
                    item.IsChecked(variable.decimals == value);
                    item.Click([this, variableName = variable.name, value](auto const&, auto const&)
                    {
                        if (updating_) return;
                        const std::string reply = Dispatch({ "SET_VARIABLE_DECIMALS", dataframe_.group,
                                                             variableName, std::to_string(value) });
                        status_.Text(to_hstring(reply.rfind("OK\t", 0) == 0 ? reply.substr(3) : reply));
                    });
                    decimalsMenu.Items().Append(item);
                }
                decimals.Flyout(decimalsMenu);
            }
            Controls::Grid::SetRow(decimals, row); Controls::Grid::SetColumn(decimals, 2);
            table_.Children().Append(decimals);

            if (showDefaultRole)
            {
                const std::string currentRole = roles_.count(variable.name)
                    ? roles_.at(variable.name) : "none";
                auto role = textCellButton(to_hstring(
                    ::rlispstat::core::VariableRoleDisplayName(currentRole)).c_str());
                auto roleMenu = Controls::MenuFlyout();
                for (auto const& option : roleOptions)
                {
                    auto item = Controls::ToggleMenuFlyoutItem();
                    item.Text(to_hstring(option.title));
                    item.IsChecked(currentRole == option.value);
                    item.Click([this, variableName = variable.name,
                                selectedRole = option.value](auto const&, auto const&)
                    {
                        if (updating_) return;
                        const std::string reply = Dispatch({ "SET_DEFAULT_VARIABLE_ROLE",
                            dataframe_.group, variableName, selectedRole });
                        if (reply.rfind("OK", 0) == 0)
                        {
                            if (selectedRole == "dependent")
                                for (auto& [name, roleValue] : roles_)
                                    if (roleValue == "dependent") roleValue = "none";
                            roles_[variableName] = selectedRole;
                            RebuildRows();
                        }
                        status_.Text(to_hstring(reply.rfind("OK\t", 0) == 0
                            ? reply.substr(3) : reply));
                    });
                    roleMenu.Items().Append(item);
                }
                role.Flyout(roleMenu);
                Controls::Grid::SetRow(role, row);
                Controls::Grid::SetColumn(role, 3);
                table_.Children().Append(role);
            }

            auto description = Controls::TextBox();
            description.Text(to_hstring(variable.description));
            configureEditableCell(description);
            description.LostFocus([this, description, variableName = variable.name,
                                   original = variable.description](auto const&, auto const&)
            {
                if (updating_) return;
                const std::string value = to_string(description.Text());
                if (value == original) return;
                const std::string reply = Dispatch({ "SET_VARIABLE_DESCRIPTION", dataframe_.group,
                                                     variableName, value });
                status_.Text(to_hstring(reply.rfind("OK\t", 0) == 0 ? reply.substr(3) : reply));
            });
            Controls::Grid::SetRow(description, row);
            Controls::Grid::SetColumn(description, showDefaultRole ? 4 : 3);
            table_.Children().Append(description);
        }
        updating_ = false;
    }

    void VariablesWindow::Activate()
    {
        if (!closed_) window_.Activate();
    }

    void VariablesWindow::Close()
    {
        if (!closed_) window_.Close();
    }

    std::shared_ptr<ScatterplotDialog> ScatterplotDialog::Create(
        Window const& owner, ::rlispstat::core::DataFrameModel const& dataframe,
        Submit submit, Closed closed,
        ::rlispstat::core::InitialAnalysisSpecification initial)
    {
        auto dialog = std::shared_ptr<ScatterplotDialog>(new ScatterplotDialog(
            owner, dataframe, std::move(submit), std::move(closed), std::move(initial)));
        dialog->AttachLifetime();
        return dialog;
    }

    ScatterplotDialog::ScatterplotDialog(Window const& owner,
        ::rlispstat::core::DataFrameModel const& dataframe, Submit submit, Closed closed,
        ::rlispstat::core::InitialAnalysisSpecification initial)
        : owner_(owner), dataframe_(dataframe), submit_(std::move(submit)),
          closedCallback_(std::move(closed)), initial_(std::move(initial))
    {
        Initialize();
    }

    void ScatterplotDialog::Initialize()
    {
        const auto labels = ::rlispstat::core::BuildScatterplotCreationDialogState();
        dialog_ = Controls::ContentDialog();
        auto root = Controls::StackPanel(); root.Width(360); root.Spacing(12);
        root.Children().Append(DialogContentIntroduction(
            to_hstring(labels.informativeText).c_str(), dataframe_.group));

        auto fields = Controls::Grid();
        fields.ColumnSpacing(8);
        fields.RowSpacing(8);
        auto labelColumn = Controls::ColumnDefinition(); labelColumn.Width(GridLengthHelper::FromPixels(30));
        fields.ColumnDefinitions().Append(labelColumn);
        auto variableColumn = Controls::ColumnDefinition();
        variableColumn.Width(GridLengthHelper::FromPixels(150));
        fields.ColumnDefinitions().Append(variableColumn);
        fields.ColumnDefinitions().Append(Controls::ColumnDefinition());
        for (int i = 0; i < 2; ++i) fields.RowDefinitions().Append(Controls::RowDefinition());
        fields.Children().Append(DialogFieldLabel(to_hstring(::rlispstat::core::ScatterplotXFieldLabel()).c_str()));
        xVariable_ = Controls::ComboBox();
        Controls::Grid::SetColumn(xVariable_, 1); fields.Children().Append(xVariable_);
        auto yLabel = DialogFieldLabel(to_hstring(::rlispstat::core::ScatterplotYFieldLabel()).c_str());
        Controls::Grid::SetRow(yLabel, 1); fields.Children().Append(yLabel);
        yVariable_ = Controls::ComboBox();
        Controls::Grid::SetRow(yVariable_, 1); Controls::Grid::SetColumn(yVariable_, 1); fields.Children().Append(yVariable_);
        title_ = Controls::TextBox(); title_.PlaceholderText(to_hstring(labels.optionalTitlePlaceholder));
        Controls::Grid::SetRow(title_, 1);
        Controls::Grid::SetColumn(title_, 2); fields.Children().Append(title_);
        root.Children().Append(fields);

        for (auto const& column : dataframe_.columns)
        {
            if (column.type == "numeric")
            {
                xVariable_.Items().Append(box_value(to_hstring(column.name)));
                yVariable_.Items().Append(box_value(to_hstring(column.name)));
            }
        }
        const auto select = [](Controls::ComboBox const& box, std::string const& name)
        {
            if (name.empty()) return;
            for (uint32_t index = 0; index < box.Items().Size(); ++index)
                if (to_string(ItemText(box.Items().GetAt(index))) == name)
                { box.SelectedIndex(static_cast<int32_t>(index)); return; }
        };
        select(xVariable_, ::rlispstat::core::InitialAnalysisVariable(initial_, "x"));
        select(yVariable_, ::rlispstat::core::InitialAnalysisVariable(initial_, "y"));

        validation_ = DialogFieldLabel(L"");
        validation_.Foreground(Brush(177, 48, 45));
        root.Children().Append(validation_);
        xVariable_.SelectionChanged([this](auto const&, auto const&) { Validate(); });
        yVariable_.SelectionChanged([this](auto const&, auto const&) { Validate(); });
        ConfigureNativeDialog(dialog_, owner_, to_hstring(labels.title).c_str(), root,
            to_hstring(labels.createButtonTitle).c_str(), 360);
        dialog_.PrimaryButtonClick([this](auto const&,
            Controls::ContentDialogButtonClickEventArgs const& args)
        {
            args.Cancel(true);
            SubmitNow();
        });
        Validate();
    }

    void ScatterplotDialog::AttachLifetime()
    {
        std::weak_ptr<ScatterplotDialog> weak = shared_from_this();
        dialog_.Closed([weak](auto const&, auto const&)
        {
            if (auto dialog = weak.lock())
            {
                dialog->closed_ = true;
                if (dialog->closedCallback_) dialog->closedCallback_();
            }
        });
    }

    void ScatterplotDialog::Validate()
    {
        const bool enough = xVariable_.Items().Size() >= 2;
        const bool selected = xVariable_.SelectedIndex() >= 0 &&
            yVariable_.SelectedIndex() >= 0;
        const bool distinct = !selected ||
            ItemText(xVariable_.SelectedItem()) != ItemText(yVariable_.SelectedItem());
        const bool valid = enough && selected && distinct;
        dialog_.IsPrimaryButtonEnabled(valid);
        if (!enough)
            validation_.Text(to_hstring(
                ::rlispstat::core::BuildScatterplotCreationDialogState().needsTwoNumericVariablesStatus));
        else if (selected && !distinct)
            validation_.Text(L"Choose two different numeric variables.");
        else
            validation_.Text(L"");
    }

    void ScatterplotDialog::SubmitNow()
    {
        if (!dialog_.IsPrimaryButtonEnabled() || !submit_) return;
        ScatterplotSpecification specification;
        specification.group = dataframe_.group;
        specification.xVariable = to_string(ItemText(xVariable_.SelectedItem()));
        specification.yVariable = to_string(ItemText(yVariable_.SelectedItem()));
        specification.title = to_string(title_.Text());
        // A ContentDialog restores its owner (the data sheet) while closing.
        // Creating and activating the plot before Hide() therefore allowed
        // that final owner restoration to cover the newly created plot.
        // Defer creation until the modal teardown has completed.
        auto submit = submit_;
        auto dispatcher = owner_.DispatcherQueue();
        Close();
        dispatcher.TryEnqueue(
            Microsoft::UI::Dispatching::DispatcherQueuePriority::Low,
            [submit = std::move(submit), specification = std::move(specification)]()
            {
                submit(specification);
            });
    }

    void ScatterplotDialog::Show()
    {
        if (closed_) return;
        dialog_.Opened([this](auto const&, auto const&)
        { xVariable_.Focus(FocusState::Programmatic); });
        dialog_.ShowAsync();
    }
    void ScatterplotDialog::Close() { if (!closed_) dialog_.Hide(); }

    std::shared_ptr<PlotWorkflowDialog> PlotWorkflowDialog::Create(
        Window const& owner, ::rlispstat::core::DataFrameModel const& dataframe,
        PlotWorkflowKind kind, Submit submit, Closed closed, std::string initialTrellisType,
        ::rlispstat::core::InitialAnalysisSpecification initial)
    {
        auto dialog = std::shared_ptr<PlotWorkflowDialog>(new PlotWorkflowDialog(
            owner, dataframe, kind, std::move(submit), std::move(closed),
            std::move(initialTrellisType), std::move(initial)));
        dialog->AttachLifetime();
        return dialog;
    }

    PlotWorkflowDialog::PlotWorkflowDialog(
        Window const& owner, ::rlispstat::core::DataFrameModel const& dataframe,
        PlotWorkflowKind kind, Submit submit, Closed closed, std::string initialTrellisType,
        ::rlispstat::core::InitialAnalysisSpecification initial)
        : owner_(owner), dataframe_(dataframe), kind_(kind), submit_(std::move(submit)),
          closedCallback_(std::move(closed)), initialTrellisType_(std::move(initialTrellisType)),
          initial_(std::move(initial))
    {
        Initialize();
    }

    void PlotWorkflowDialog::Initialize()
    {
        const wchar_t* heading = L"Plot";
        switch (kind_)
        {
        case PlotWorkflowKind::Trellis:
            heading = initialTrellisType_ == "time_series" ? L"Trellis Plots — Time Series" :
                initialTrellisType_ == "boxplot" ? L"Trellis Plots — Boxplot" :
                initialTrellisType_ == "bar" ? L"Trellis Plots — Bar Chart" :
                initialTrellisType_ == "histogram" ? L"Trellis Plots — Histogram" :
                L"Trellis Plots — Scatterplot";
            break;
        case PlotWorkflowKind::TimeSeries: heading = L"Time Series"; break;
        case PlotWorkflowKind::ScatterMatrix: heading = L"Scatterplot Matrix"; break;
        case PlotWorkflowKind::ParallelCoordinates: heading = L"Parallel Coordinates"; break;
        case PlotWorkflowKind::Boxplot: heading = L"Boxplot"; break;
        case PlotWorkflowKind::Histogram: heading = L"Histogram"; break;
        case PlotWorkflowKind::BarChart: heading = L"Bar Chart"; break;
        }
        dialog_ = Controls::ContentDialog();
        auto root = Controls::StackPanel();
        root.Width(kind_ == PlotWorkflowKind::ScatterMatrix ||
            kind_ == PlotWorkflowKind::ParallelCoordinates ? 440 : 380);
        root.Spacing(10);
        std::wstring description = L"Choose variables from the active dataset. The new plot shares row selection with existing LinkEDA windows.";
        if (kind_ == PlotWorkflowKind::Trellis) description = L"Choose variables and a conditioning variable. Each panel remains linked to the same dataset selection.";
        else if (kind_ == PlotWorkflowKind::ScatterMatrix) description = L"Choose two or more numeric variables for a linked scatterplot matrix.";
        else if (kind_ == PlotWorkflowKind::ParallelCoordinates) description = L"Choose numeric variables and how observations should be scaled and connected.";
        root.Children().Append(DialogContentIntroduction(description, dataframe_.group));

        auto field = [&](std::wstring const& label, Controls::Control const& control)
        {
            auto row = Controls::Grid(); row.ColumnSpacing(10);
            auto names = Controls::ColumnDefinition(); names.Width(GridLengthHelper::FromPixels(112));
            row.ColumnDefinitions().Append(names); row.ColumnDefinitions().Append(Controls::ColumnDefinition());
            row.Children().Append(DialogFieldLabel(label)); Controls::Grid::SetColumn(control, 1);
            row.Children().Append(control); root.Children().Append(row);
        };

        xVariable_ = Controls::ComboBox(); yVariable_ = Controls::ComboBox();
        secondaryVariable_ = Controls::ComboBox(); groupVariable_ = Controls::ComboBox();
        conditionVariable_ = Controls::ComboBox(); trellisType_ = Controls::ComboBox();
        variables_ = Controls::ListView(); ConfigureDialogList(variables_);
        bins_ = Controls::TextBox(); title_ = Controls::TextBox();
        standardize_ = Controls::CheckBox(); connect_ = Controls::CheckBox();
        const auto appendNone = [](Controls::ComboBox const& box)
        { box.Items().Append(box_value(L"(none)")); };
        appendNone(secondaryVariable_); appendNone(groupVariable_); appendNone(conditionVariable_);

        const auto definition = ::rlispstat::core::SharedAnalysisDefinition(
            SharedKindForPlotWorkflow(kind_, initialTrellisType_));
        const auto* xSlot = FindSharedSlot(definition, { "x" });
        const auto* ySlot = FindSharedSlot(definition, { "y" });
        const auto* conditioningSlot = FindSharedSlot(definition, { "conditioning" });
        const auto* variablesSlot = FindSharedSlot(definition, { "variables" });
        const auto compatible = [](auto const* slot,
                                   ::rlispstat::core::DataColumn const& column)
        {
            return slot && ::rlispstat::core::AnalysisVariableIsCompatible(
                column, slot->acceptedType);
        };

        for (auto const& column : dataframe_.columns)
        {
            const auto name = box_value(to_hstring(column.name));
            if (compatible(xSlot, column))
            {
                xVariable_.Items().Append(name);
                secondaryVariable_.Items().Append(name);
                if (kind_ == PlotWorkflowKind::Boxplot ||
                    kind_ == PlotWorkflowKind::BarChart ||
                    (kind_ == PlotWorkflowKind::Trellis &&
                     (initialTrellisType_ == "boxplot" || initialTrellisType_ == "bar")))
                    groupVariable_.Items().Append(name);
            }
            if (compatible(ySlot, column)) yVariable_.Items().Append(name);
            if (compatible(conditioningSlot, column))
            {
                conditionVariable_.Items().Append(name);
                if (kind_ == PlotWorkflowKind::TimeSeries)
                    groupVariable_.Items().Append(name);
            }
            if (compatible(variablesSlot, column)) variables_.Items().Append(name);
        }
        secondaryVariable_.SelectedIndex(0); groupVariable_.SelectedIndex(0);
        conditionVariable_.SelectedIndex(0);
        variables_.SelectionMode(Controls::ListViewSelectionMode::Multiple);
        variables_.MinHeight(150); variables_.MaxHeight(230);
        const auto selectCombo = [](Controls::ComboBox const& box, std::string const& name)
        {
            if (name.empty()) return;
            for (uint32_t index = 0; index < box.Items().Size(); ++index)
                if (to_string(ItemText(box.Items().GetAt(index))) == name)
                { box.SelectedIndex(static_cast<int32_t>(index)); return; }
        };
        const auto initialX = ::rlispstat::core::InitialAnalysisVariable(initial_, "x");
        const auto initialY = ::rlispstat::core::InitialAnalysisVariable(initial_, "y");
        auto initialGroup = ::rlispstat::core::InitialAnalysisVariable(initial_, "group");
        if (kind_ == PlotWorkflowKind::Boxplot && initialGroup.empty()) initialGroup = initialX;
        selectCombo(xVariable_, initialX);
        selectCombo(yVariable_, initialY);
        selectCombo(secondaryVariable_, initialX);
        selectCombo(groupVariable_, initialGroup);
        const auto conditions = ::rlispstat::core::InitialAnalysisVariables(initial_, "conditioning");
        if (conditions.size() == 1) selectCombo(conditionVariable_, conditions.front());
        for (auto const& selected :
             ::rlispstat::core::InitialAnalysisVariables(initial_, "variables"))
            for (uint32_t index = 0; index < variables_.Items().Size(); ++index)
                if (to_string(ItemText(variables_.Items().GetAt(index))) == selected)
                    variables_.SelectedItems().Append(variables_.Items().GetAt(index));

        if (kind_ == PlotWorkflowKind::Trellis)
        {
            static const std::array<const char*, 5> types{
                "scatter", "time_series", "boxplot", "bar", "histogram" };
            const auto found = std::find(types.begin(), types.end(), initialTrellisType_);
            trellisType_.SelectedIndex(found == types.end() ? 0 :
                static_cast<int32_t>(std::distance(types.begin(), found)));
            field(L"X:", xVariable_);
            if (initialTrellisType_ != "bar" && initialTrellisType_ != "histogram")
                field(L"Y:", yVariable_);
            field(L"Condition on:", conditionVariable_);
        }
        else if (kind_ == PlotWorkflowKind::TimeSeries)
        {
            field(L"Time:", xVariable_); field(L"Value:", yVariable_);
            field(L"Series:", groupVariable_);
        }
        else if (kind_ == PlotWorkflowKind::ScatterMatrix ||
                 kind_ == PlotWorkflowKind::ParallelCoordinates)
        {
            auto label = DialogFieldLabel(L"Variables (select at least two):"); root.Children().Append(label);
            root.Children().Append(variables_);
            if (kind_ == PlotWorkflowKind::ParallelCoordinates)
            {
                standardize_.Content(box_value(L"Standardize variables")); standardize_.IsChecked(true);
                connect_.Content(box_value(L"Connect observations")); connect_.IsChecked(true);
            }
        }
        else if (kind_ == PlotWorkflowKind::Boxplot)
        {
            field(L"Y:", yVariable_); field(L"Group:", groupVariable_);
        }
        else if (kind_ == PlotWorkflowKind::Histogram)
        {
            field(L"X:", xVariable_); bins_.PlaceholderText(L"Automatic (Sturges)");
            field(L"Bins:", bins_);
        }
        else if (kind_ == PlotWorkflowKind::BarChart)
        {
            field(L"X:", secondaryVariable_); field(L"Add X:", groupVariable_);
            field(L"Split by:", conditionVariable_);
        }
        title_.PlaceholderText(L"Optional title"); field(L"Title:", title_);
        validation_ = DialogFieldLabel(L""); validation_.Foreground(Brush(177, 48, 45));
        root.Children().Append(validation_);
        auto changed = [this](auto const&, auto const&) { Validate(); };
        xVariable_.SelectionChanged(changed); yVariable_.SelectionChanged(changed);
        secondaryVariable_.SelectionChanged(changed); groupVariable_.SelectionChanged(changed);
        conditionVariable_.SelectionChanged(changed); variables_.SelectionChanged(changed);
        trellisType_.SelectionChanged(changed);
        bins_.TextChanged(changed);
        const double width = kind_ == PlotWorkflowKind::ScatterMatrix ||
            kind_ == PlotWorkflowKind::ParallelCoordinates ? 440 : 380;
        ConfigureNativeDialog(dialog_, owner_, heading, root, L"Create", width);
        dialog_.PrimaryButtonClick([this](auto const&,
            Controls::ContentDialogButtonClickEventArgs const& args)
        {
            args.Cancel(true);
            SubmitNow();
        });
        Validate();
    }

    void PlotWorkflowDialog::AttachLifetime()
    {
        std::weak_ptr<PlotWorkflowDialog> weak = shared_from_this();
        dialog_.Closed([weak](auto const&, auto const&)
        {
            if (auto dialog = weak.lock())
            {
                dialog->closed_ = true;
                if (dialog->closedCallback_) dialog->closedCallback_();
            }
        });
    }

    void PlotWorkflowDialog::Validate()
    {
        bool valid = true; std::wstring message;
        if (kind_ == PlotWorkflowKind::ScatterMatrix ||
            kind_ == PlotWorkflowKind::ParallelCoordinates)
        {
            valid = variables_.SelectedItems().Size() >= 2;
            if (!valid) message = L"Select at least two numeric variables.";
        }
        else if (kind_ == PlotWorkflowKind::BarChart)
        {
            valid = secondaryVariable_.SelectedIndex() > 0;
            if (!valid) message = L"Choose an X variable.";
        }
        else
        {
            valid = (kind_ == PlotWorkflowKind::Boxplot ? yVariable_.SelectedIndex() >= 0
                                                        : xVariable_.SelectedIndex() >= 0);
            const bool trellisRequiresY = kind_ == PlotWorkflowKind::Trellis &&
                (initialTrellisType_ == "scatter" || initialTrellisType_ == "time_series" ||
                 initialTrellisType_ == "boxplot");
            const bool distinct = kind_ == PlotWorkflowKind::TimeSeries ||
                (kind_ == PlotWorkflowKind::Trellis &&
                 (initialTrellisType_ == "scatter" || initialTrellisType_ == "time_series"));
            if ((kind_ == PlotWorkflowKind::TimeSeries || trellisRequiresY) &&
                (yVariable_.SelectedIndex() < 0 ||
                 (distinct && ItemText(xVariable_.SelectedItem()) == ItemText(yVariable_.SelectedItem()))))
            { valid = false; message = L"X/time and Y/value must be different."; }
            if (kind_ == PlotWorkflowKind::Trellis && conditionVariable_.SelectedIndex() <= 0)
            { valid = false; message = L"Choose a conditioning variable."; }
        }
        if (kind_ == PlotWorkflowKind::Histogram && !bins_.Text().empty())
        {
            const int count = _wtoi(bins_.Text().c_str());
            if (count < 1) { valid = false; message = L"Bins must be a positive integer or left blank."; }
        }
        dialog_.IsPrimaryButtonEnabled(valid); validation_.Text(message);
    }

    void PlotWorkflowDialog::SubmitNow()
    {
        if (!dialog_.IsPrimaryButtonEnabled() || !submit_) return;
        PlotWorkflowSpecification specification; specification.kind = kind_;
        specification.group = dataframe_.group; specification.title = to_string(title_.Text());
        if (xVariable_.SelectedItem()) specification.xVariable = to_string(ItemText(xVariable_.SelectedItem()));
        if (yVariable_.SelectedItem()) specification.yVariable = to_string(ItemText(yVariable_.SelectedItem()));
        if (secondaryVariable_.SelectedIndex() > 0)
            specification.secondaryVariable = to_string(ItemText(secondaryVariable_.SelectedItem()));
        if (groupVariable_.SelectedIndex() > 0)
            specification.groupVariable = to_string(ItemText(groupVariable_.SelectedItem()));
        if (conditionVariable_.SelectedIndex() > 0)
            specification.conditionVariable = to_string(ItemText(conditionVariable_.SelectedItem()));
        // The Trellis subtype is chosen by the command that opens this dialog.
        // trellisType_ is intentionally not shown in the dialog, so using its
        // (empty) selection here used to fall back to the specification's
        // default, "scatter", for Histogram and the other specialised entries.
        if (kind_ == PlotWorkflowKind::Trellis)
            specification.trellisType = initialTrellisType_;
        for (auto const& item : variables_.SelectedItems())
            specification.variables.push_back(to_string(ItemText(item)));
        specification.bins = bins_.Text().empty() ? 0 : _wtoi(bins_.Text().c_str());
        specification.standardize = standardize_.IsChecked() && standardize_.IsChecked().GetBoolean();
        specification.connect = connect_.IsChecked() && connect_.IsChecked().GetBoolean();
        // Let ContentDialog finish returning focus to its owner before the
        // output window is created. Otherwise the data sheet is activated
        // after the plot and ends up covering it.
        auto submit = submit_;
        auto dispatcher = owner_.DispatcherQueue();
        Close();
        dispatcher.TryEnqueue(
            Microsoft::UI::Dispatching::DispatcherQueuePriority::Low,
            [submit = std::move(submit), specification = std::move(specification)]()
            {
                submit(specification);
            });
    }

    void PlotWorkflowDialog::Show()
    {
        if (closed_) return;
        dialog_.Opened([this](auto const&, auto const&)
        {
            if (kind_ == PlotWorkflowKind::ScatterMatrix ||
                kind_ == PlotWorkflowKind::ParallelCoordinates)
                variables_.Focus(FocusState::Programmatic);
            else xVariable_.Focus(FocusState::Programmatic);
        });
        dialog_.ShowAsync();
    }
    void PlotWorkflowDialog::Close() { if (!closed_) dialog_.Hide(); }

    std::shared_ptr<AnalysisWorkflowDialog> AnalysisWorkflowDialog::Create(
        Window const& owner, ::rlispstat::core::DataFrameModel const& dataframe,
        AnalysisWorkflowKind kind, Submit submit, Closed closed,
        ::rlispstat::core::InitialAnalysisSpecification initial,
        std::vector<::rlispstat::core::AnalysisScopeChoice> scopeChoices)
    {
        auto dialog = std::shared_ptr<AnalysisWorkflowDialog>(new AnalysisWorkflowDialog(
            owner, dataframe, kind, std::move(submit), std::move(closed),
            std::move(initial), std::move(scopeChoices)));
        dialog->AttachLifetime();
        return dialog;
    }

    AnalysisWorkflowDialog::AnalysisWorkflowDialog(
        Window const& owner, ::rlispstat::core::DataFrameModel const& dataframe,
        AnalysisWorkflowKind kind, Submit submit, Closed closed,
        ::rlispstat::core::InitialAnalysisSpecification initial,
        std::vector<::rlispstat::core::AnalysisScopeChoice> scopeChoices)
        : owner_(owner), dataframe_(dataframe), kind_(kind), submit_(std::move(submit)),
          closedCallback_(std::move(closed)), initial_(std::move(initial)),
          scopeChoices_(std::move(scopeChoices))
    {
        Initialize();
    }

    void AnalysisWorkflowDialog::Initialize()
    {
        const wchar_t* heading = L"Analysis";
        switch (kind_)
        {
        case AnalysisWorkflowKind::Table1: heading = L"Table 1"; break;
        case AnalysisWorkflowKind::CorrelationMatrix: heading = L"Correlation Matrix"; break;
        case AnalysisWorkflowKind::Dimensionality: heading = L"Principal Components / Factor Analysis"; break;
        case AnalysisWorkflowKind::FactorAnalysis: heading = L"Factor Analysis"; break;
        case AnalysisWorkflowKind::ScaleAnalysis: heading = L"Scale Analysis"; break;
        case AnalysisWorkflowKind::QuickCluster: heading = L"Quick Cluster"; break;
        case AnalysisWorkflowKind::OneSampleT: heading = L"One-Sample Tests"; break;
        case AnalysisWorkflowKind::IndependentT: heading = L"Two-Sample Tests"; break;
        case AnalysisWorkflowKind::PairedT: heading = L"Paired-Samples Tests"; break;
        case AnalysisWorkflowKind::OneWayAnova: heading = L"One-Way ANOVA"; break;
        case AnalysisWorkflowKind::LinearModel: heading = L"Linear Model"; break;
        case AnalysisWorkflowKind::LinearModelTrellis: heading = L"Linear Model Trellis"; break;
        case AnalysisWorkflowKind::RegressionComparison: heading = L"Compare Linear Models"; break;
        case AnalysisWorkflowKind::BinaryRegression: heading = L"Binary Model"; break;
        case AnalysisWorkflowKind::BinaryRegressionComparison: heading = L"Compare Binary Models"; break;
        case AnalysisWorkflowKind::GeneralizedLinearModel: heading = L"Generalized Linear Model"; break;
        case AnalysisWorkflowKind::CountRegression: heading = L"Count Model"; break;
        case AnalysisWorkflowKind::CountRegressionComparison: heading = L"Compare Count Models"; break;
        case AnalysisWorkflowKind::PositiveContinuousModel: heading = L"Positive Continuous Model"; break;
        case AnalysisWorkflowKind::PositiveContinuousComparison: heading = L"Compare Positive Continuous Models"; break;
        case AnalysisWorkflowKind::ProportionModel: heading = L"Proportion Model"; break;
        case AnalysisWorkflowKind::ProportionComparison: heading = L"Compare Proportion Models"; break;
        case AnalysisWorkflowKind::GeneralizedComparison: heading = L"Compare Generalized Linear Models"; break;
        case AnalysisWorkflowKind::LinearMixedModel: heading = L"Linear Mixed Model"; break;
        case AnalysisWorkflowKind::GeneralizedMixedModel: heading = L"Generalized Linear Mixed Model"; break;
        }
        dialog_ = Controls::ContentDialog();
        if (kind_ == AnalysisWorkflowKind::LinearModelTrellis)
        {
            auto root = Controls::StackPanel(); root.Width(420); root.Spacing(9);
            root.Children().Append(DialogContentIntroduction(
                L"Choose a dependent variable and one or two panel variables. Predictors can be added after creation.",
                dataframe_.group));
            response_ = Controls::ComboBox(); rowCondition_ = Controls::ComboBox();
            columnCondition_ = Controls::ComboBox(); scope_ = Controls::ComboBox();
            auto field = [&](std::wstring const& label, Controls::ComboBox const& control)
            {
                auto row = Controls::Grid(); row.ColumnSpacing(10);
                auto labelColumn = Controls::ColumnDefinition();
                labelColumn.Width(GridLengthHelper::FromPixels(132));
                row.ColumnDefinitions().Append(labelColumn); row.ColumnDefinitions().Append(Controls::ColumnDefinition());
                row.Children().Append(DialogFieldLabel(label)); Controls::Grid::SetColumn(control, 1);
                row.Children().Append(control); root.Children().Append(row);
            };
            ::rlispstat::core::ModelTrellisSpecification candidate;
            candidate.baseModel.group = dataframe_.group;
            const std::string initialResponse =
                ::rlispstat::core::InitialAnalysisVariable(initial_, "y");
            const auto initialConditions =
                ::rlispstat::core::InitialAnalysisVariables(initial_, "conditioning");
            candidate.baseModel.response = initialResponse;
            response_.Items().Append(box_value(L"(choose dependent variable)"));
            for (auto const& column : dataframe_.columns)
                if (::rlispstat::core::NormalizeVariableType(column.type) == "numeric" &&
                    ::rlispstat::core::DataColumnAllowsNumeric(column))
                    response_.Items().Append(box_value(to_hstring(column.name)));
            const auto categorical = ::rlispstat::core::ModelTrellisCategoricalVariables(dataframe_, candidate);
            rowCondition_.Items().Append(box_value(L"(none)"));
            columnCondition_.Items().Append(box_value(L"(none)"));
            for (auto const& name : categorical)
            {
                rowCondition_.Items().Append(box_value(to_hstring(name)));
                columnCondition_.Items().Append(box_value(to_hstring(name)));
            }
            if (scopeChoices_.empty())
                scopeChoices_ = ::rlispstat::core::BuildAnalysisScopeChoices(0, {}, true);
            PopulateAnalysisScopeCombo(scope_, scopeChoices_, "all");
            response_.SelectedIndex(0);
            for (uint32_t index = 1; index < response_.Items().Size(); ++index)
                if (to_string(ItemText(response_.Items().GetAt(index))) == initialResponse)
                { response_.SelectedIndex(static_cast<int32_t>(index)); break; }
            rowCondition_.SelectedIndex(0);
            columnCondition_.SelectedIndex(0);
            bool rowAssigned = false;
            for (auto const& term : initialConditions)
            {
                auto found = std::find(categorical.begin(), categorical.end(), term);
                if (found == categorical.end()) continue;
                const auto index = static_cast<int32_t>(std::distance(categorical.begin(), found) + 1);
                if (!rowAssigned) { rowCondition_.SelectedIndex(index); rowAssigned = true; }
                else { columnCondition_.SelectedIndex(index); break; }
            }
            field(L"Dependent variable:", response_); field(L"Panel rows:", rowCondition_);
            field(L"Panel columns:", columnCondition_); field(L"Scope:", scope_);
            validation_ = DialogFieldLabel(L""); validation_.Foreground(Brush(177, 48, 45));
            root.Children().Append(validation_);
            auto changed = [this](auto const&, auto const&) { Validate(); };
            response_.SelectionChanged(changed); rowCondition_.SelectionChanged(changed);
            columnCondition_.SelectionChanged(changed); scope_.SelectionChanged(changed);
            ConfigureNativeDialog(dialog_, owner_, heading, root, L"Create", 420);
            dialog_.PrimaryButtonClick([this](auto const&,
                Controls::ContentDialogButtonClickEventArgs const& args)
            {
                args.Cancel(true);
                SubmitNow();
            });
            Validate();
            return;
        }
        auto root = Controls::StackPanel(); root.Width(450); root.Spacing(9);
        root.Children().Append(DialogContentIntroduction(
            L"The analysis uses the active LinkEDA dataset and analysis scope.",
            dataframe_.group));

        response_ = Controls::ComboBox(); secondary_ = Controls::ComboBox();
        groupVariable_ = Controls::ComboBox(); rowCondition_ = Controls::ComboBox();
        columnCondition_ = Controls::ComboBox(); variables_ = Controls::ListView();
        ConfigureDialogList(variables_);
        family_ = Controls::ComboBox(); link_ = Controls::ComboBox(); method_ = Controls::ComboBox();
        responseLabel_ = DialogFieldLabel(L"Response:"); secondaryLabel_ = DialogFieldLabel(L"Second variable:");
        groupLabel_ = DialogFieldLabel(L"Group:"); rowLabel_ = DialogFieldLabel(L"Rows:");
        columnLabel_ = DialogFieldLabel(L"Columns:"); variablesLabel_ = DialogFieldLabel(L"Predictors:");
        familyLabel_ = DialogFieldLabel(L"Distribution:"); linkLabel_ = DialogFieldLabel(L"Link:");
        methodLabel_ = DialogFieldLabel(L"Method:");

        const auto addField = [&](Controls::TextBlock const& label, Controls::Control const& control)
        {
            auto row = Controls::Grid(); row.ColumnSpacing(10);
            auto labelColumn = Controls::ColumnDefinition();
            labelColumn.Width(GridLengthHelper::FromPixels(128));
            row.ColumnDefinitions().Append(labelColumn); row.ColumnDefinitions().Append(Controls::ColumnDefinition());
            row.Children().Append(label); Controls::Grid::SetColumn(control, 1);
            row.Children().Append(control); root.Children().Append(row);
        };
        addField(responseLabel_, response_); addField(secondaryLabel_, secondary_);
        addField(groupLabel_, groupVariable_); addField(rowLabel_, rowCondition_);
        addField(columnLabel_, columnCondition_);

        variables_.SelectionMode(Controls::ListViewSelectionMode::Multiple);
        variables_.MinHeight(150); variables_.MaxHeight(220);
        root.Children().Append(variablesLabel_); root.Children().Append(variables_);
        addField(familyLabel_, family_); addField(linkLabel_, link_); addField(methodLabel_, method_);

        response_.Items().Append(box_value(L"(choose response)"));
        secondary_.Items().Append(box_value(L"(choose second variable)"));
        groupVariable_.Items().Append(box_value(L"(choose group)"));
        rowCondition_.Items().Append(box_value(L"(none)")); rowCondition_.SelectedIndex(0);
        columnCondition_.Items().Append(box_value(L"(none)"));
        const auto sharedKind = SharedKindForWorkflow(kind_);
        const auto sharedDefinition = sharedKind
            ? ::rlispstat::core::SharedAnalysisDefinition(*sharedKind)
            : ::rlispstat::core::AnalysisDefinition{};
        const auto* responseSlot = FindSharedSlot(
            sharedDefinition, { "response", "first", "responses", "y" });
        const auto* secondarySlot = FindSharedSlot(sharedDefinition, { "second" });
        const auto* groupSlot = FindSharedSlot(sharedDefinition, { "group" });
        const auto* variableSlot = FindSharedSlot(
            sharedDefinition, { "variables", "predictors", "fixed_effects", "responses" });
        for (auto const& column : dataframe_.columns)
        {
            const auto item = box_value(to_hstring(column.name));
            if (responseSlot && ::rlispstat::core::AnalysisVariableIsCompatible(
                    column, responseSlot->acceptedType))
                response_.Items().Append(item);
            if (secondarySlot && ::rlispstat::core::AnalysisVariableIsCompatible(
                    column, secondarySlot->acceptedType))
                secondary_.Items().Append(item);
            if (variableSlot && ::rlispstat::core::AnalysisVariableIsCompatible(
                    column, variableSlot->acceptedType))
                variables_.Items().Append(item);
            if (groupSlot && ::rlispstat::core::AnalysisVariableIsCompatible(
                    column, groupSlot->acceptedType))
                groupVariable_.Items().Append(item);
            if (::rlispstat::core::AnalysisVariableIsCompatible(
                    column, ::rlispstat::core::AnalysisVariableType::Categorical))
            {
                rowCondition_.Items().Append(item);
                columnCondition_.Items().Append(item);
            }
        }
        response_.SelectedIndex(0);
        secondary_.SelectedIndex(0);
        groupVariable_.SelectedIndex(0);
        columnCondition_.SelectedIndex(0);

        if (const auto modelType = StatisticalModelTypeForWorkflow(kind_))
        {
            for (auto const& distribution :
                 ::rlispstat::core::DistributionSpecificationsForModel(*modelType))
            {
                distributionIds_.push_back(distribution.id);
                family_.Items().Append(box_value(to_hstring(distribution.visibleName)));
            }
            family_.SelectedIndex(0);
            RebuildDistributionLinks();
        }
        else
        {
            distributionIds_ = { "gaussian", "binomial", "poisson", "Gamma" };
            for (auto const& value : {L"Gaussian", L"Binomial", L"Poisson", L"Gamma"})
                family_.Items().Append(box_value(value));
            family_.SelectedIndex(kind_ == AnalysisWorkflowKind::GeneralizedMixedModel ? 1 : 0);
            linkIds_ = { "identity", "log", "logit", "probit", "inverse" };
            for (auto const& value : {L"identity", L"log", L"logit", L"probit", L"inverse"})
                link_.Items().Append(box_value(value));
            link_.SelectedIndex(kind_ == AnalysisWorkflowKind::GeneralizedMixedModel ? 2 : 0);
        }
        method_.Items().Append(box_value(L"Principal components"));
        method_.Items().Append(box_value(L"Factor analysis"));
        method_.SelectedIndex(kind_ == AnalysisWorkflowKind::FactorAnalysis ? 1 : 0);

        const bool variablesOnly = kind_ == AnalysisWorkflowKind::Table1 ||
            kind_ == AnalysisWorkflowKind::CorrelationMatrix ||
            kind_ == AnalysisWorkflowKind::Dimensionality ||
            kind_ == AnalysisWorkflowKind::FactorAnalysis ||
            kind_ == AnalysisWorkflowKind::QuickCluster;
        const bool multiResponse = kind_ == AnalysisWorkflowKind::OneSampleT ||
            kind_ == AnalysisWorkflowKind::IndependentT ||
            kind_ == AnalysisWorkflowKind::OneWayAnova;
        const bool hasSecondary = kind_ == AnalysisWorkflowKind::PairedT;
        const bool hasGroup = kind_ == AnalysisWorkflowKind::IndependentT ||
            kind_ == AnalysisWorkflowKind::OneWayAnova ||
            kind_ == AnalysisWorkflowKind::LinearMixedModel ||
            kind_ == AnalysisWorkflowKind::GeneralizedMixedModel;
        const bool hasConditions = kind_ == AnalysisWorkflowKind::LinearModelTrellis;
        const bool hasPredictors = kind_ == AnalysisWorkflowKind::LinearModel ||
            kind_ == AnalysisWorkflowKind::LinearModelTrellis ||
            kind_ == AnalysisWorkflowKind::RegressionComparison ||
            kind_ == AnalysisWorkflowKind::BinaryRegression ||
            kind_ == AnalysisWorkflowKind::BinaryRegressionComparison ||
            kind_ == AnalysisWorkflowKind::CountRegression ||
            kind_ == AnalysisWorkflowKind::CountRegressionComparison ||
            kind_ == AnalysisWorkflowKind::PositiveContinuousModel ||
            kind_ == AnalysisWorkflowKind::PositiveContinuousComparison ||
            kind_ == AnalysisWorkflowKind::ProportionModel ||
            kind_ == AnalysisWorkflowKind::ProportionComparison ||
            kind_ == AnalysisWorkflowKind::GeneralizedLinearModel ||
            kind_ == AnalysisWorkflowKind::GeneralizedComparison ||
            kind_ == AnalysisWorkflowKind::LinearMixedModel ||
            kind_ == AnalysisWorkflowKind::GeneralizedMixedModel;
        const bool hasFamily = kind_ == AnalysisWorkflowKind::GeneralizedLinearModel ||
            kind_ == AnalysisWorkflowKind::GeneralizedComparison ||
            kind_ == AnalysisWorkflowKind::GeneralizedMixedModel ||
            kind_ == AnalysisWorkflowKind::CountRegression ||
            kind_ == AnalysisWorkflowKind::CountRegressionComparison ||
            kind_ == AnalysisWorkflowKind::PositiveContinuousModel ||
            kind_ == AnalysisWorkflowKind::PositiveContinuousComparison ||
            kind_ == AnalysisWorkflowKind::ProportionModel ||
            kind_ == AnalysisWorkflowKind::ProportionComparison;
        const bool hasLink = hasFamily || kind_ == AnalysisWorkflowKind::BinaryRegression ||
            kind_ == AnalysisWorkflowKind::BinaryRegressionComparison;
        const bool hasMethod = kind_ == AnalysisWorkflowKind::Dimensionality ||
            kind_ == AnalysisWorkflowKind::FactorAnalysis;
        responseLabel_.Visibility((variablesOnly || multiResponse) ? Visibility::Collapsed : Visibility::Visible);
        response_.Visibility((variablesOnly || multiResponse) ? Visibility::Collapsed : Visibility::Visible);
        secondaryLabel_.Visibility(hasSecondary ? Visibility::Visible : Visibility::Collapsed);
        secondary_.Visibility(hasSecondary ? Visibility::Visible : Visibility::Collapsed);
        groupLabel_.Visibility(hasGroup ? Visibility::Visible : Visibility::Collapsed);
        groupVariable_.Visibility(hasGroup ? Visibility::Visible : Visibility::Collapsed);
        rowLabel_.Visibility(hasConditions ? Visibility::Visible : Visibility::Collapsed);
        rowCondition_.Visibility(hasConditions ? Visibility::Visible : Visibility::Collapsed);
        columnLabel_.Visibility(hasConditions ? Visibility::Visible : Visibility::Collapsed);
        columnCondition_.Visibility(hasConditions ? Visibility::Visible : Visibility::Collapsed);
        variablesLabel_.Visibility((variablesOnly || hasPredictors || multiResponse) ? Visibility::Visible : Visibility::Collapsed);
        variables_.Visibility((variablesOnly || hasPredictors || multiResponse) ? Visibility::Visible : Visibility::Collapsed);
        variablesLabel_.Text(multiResponse ? L"Dependent variables:" :
            variablesOnly ? L"Variables:" : L"Predictors:");
        familyLabel_.Visibility(hasFamily ? Visibility::Visible : Visibility::Collapsed);
        family_.Visibility(hasFamily ? Visibility::Visible : Visibility::Collapsed);
        linkLabel_.Visibility(hasLink ? Visibility::Visible : Visibility::Collapsed);
        link_.Visibility(hasLink ? Visibility::Visible : Visibility::Collapsed);
        methodLabel_.Visibility(hasMethod ? Visibility::Visible : Visibility::Collapsed);
        method_.Visibility(hasMethod ? Visibility::Visible : Visibility::Collapsed);

        const auto selectCombo = [](Controls::ComboBox const& combo,
                                    std::string const& value)
        {
            if (value.empty()) return;
            for (uint32_t index = 1; index < combo.Items().Size(); ++index)
                if (to_string(ItemText(combo.Items().GetAt(index))) == value)
                { combo.SelectedIndex(static_cast<int32_t>(index)); return; }
        };
        std::string initialResponse =
            ::rlispstat::core::InitialAnalysisVariable(initial_, "response");
        if (initialResponse.empty())
            initialResponse = ::rlispstat::core::InitialAnalysisVariable(initial_, "first");
        if (initialResponse.empty())
        {
            const auto responses =
                ::rlispstat::core::InitialAnalysisVariables(initial_, "responses");
            if (responses.size() == 1) initialResponse = responses.front();
        }
        selectCombo(response_, initialResponse);
        selectCombo(secondary_,
            ::rlispstat::core::InitialAnalysisVariable(initial_, "second"));
        selectCombo(groupVariable_,
            ::rlispstat::core::InitialAnalysisVariable(initial_, "group"));
        auto initialVariables = ::rlispstat::core::InitialAnalysisVariables(
            initial_, variablesOnly ? "variables" : "predictors");
        if (kind_ == AnalysisWorkflowKind::LinearMixedModel ||
            kind_ == AnalysisWorkflowKind::GeneralizedMixedModel)
            initialVariables = ::rlispstat::core::InitialAnalysisVariables(
                initial_, "fixed_effects");
        if (kind_ == AnalysisWorkflowKind::OneSampleT ||
            kind_ == AnalysisWorkflowKind::IndependentT ||
            kind_ == AnalysisWorkflowKind::OneWayAnova)
            initialVariables = ::rlispstat::core::InitialAnalysisVariables(initial_, "responses");
        for (uint32_t index = 0; index < variables_.Items().Size(); ++index)
        {
            const std::string name = to_string(ItemText(variables_.Items().GetAt(index)));
            if (std::find(initialVariables.begin(), initialVariables.end(), name) != initialVariables.end())
                variables_.SelectedItems().Append(variables_.Items().GetAt(index));
        }

        validation_ = DialogFieldLabel(L""); validation_.Foreground(Brush(177, 48, 45));
        root.Children().Append(validation_);
        auto changed = [this](auto const&, auto const&) { Validate(); };
        response_.SelectionChanged(changed); secondary_.SelectionChanged(changed);
        groupVariable_.SelectionChanged(changed); rowCondition_.SelectionChanged(changed);
        columnCondition_.SelectionChanged(changed); variables_.SelectionChanged(changed);
        family_.SelectionChanged([this](auto const&, auto const&) {
            RebuildDistributionLinks();
            Validate();
        });
        link_.SelectionChanged(changed); method_.SelectionChanged(changed);
        ConfigureNativeDialog(dialog_, owner_, heading, root, L"Run", 450);
        dialog_.PrimaryButtonClick([this](auto const&,
            Controls::ContentDialogButtonClickEventArgs const& args)
        {
            args.Cancel(true);
            SubmitNow();
        });
        Validate();
    }

    void AnalysisWorkflowDialog::RebuildDistributionLinks()
    {
        const auto modelType = StatisticalModelTypeForWorkflow(kind_);
        if (!modelType || distributionIds_.empty() || family_.SelectedIndex() < 0)
            return;
        const auto distributionIndex = static_cast<std::size_t>(family_.SelectedIndex());
        if (distributionIndex >= distributionIds_.size()) return;
        const std::string previous = link_.SelectedIndex() >= 0 &&
                static_cast<std::size_t>(link_.SelectedIndex()) < linkIds_.size()
            ? linkIds_[static_cast<std::size_t>(link_.SelectedIndex())] : std::string{};
        const std::string distribution = distributionIds_[distributionIndex];
        linkIds_ = ::rlispstat::core::LinksForModelDistribution(*modelType, distribution);
        link_.Items().Clear();
        for (auto const& value : linkIds_)
            link_.Items().Append(box_value(to_hstring(value)));
        const std::string preferred = std::find(linkIds_.begin(), linkIds_.end(), previous) != linkIds_.end()
            ? previous : ::rlispstat::core::DefaultLinkForModelDistribution(*modelType, distribution);
        const auto found = std::find(linkIds_.begin(), linkIds_.end(), preferred);
        link_.SelectedIndex(found == linkIds_.end() ? 0 :
            static_cast<int32_t>(std::distance(linkIds_.begin(), found)));
    }

    void AnalysisWorkflowDialog::AttachLifetime()
    {
        std::weak_ptr<AnalysisWorkflowDialog> weak = shared_from_this();
        dialog_.Closed([weak](auto const&, auto const&)
        {
            if (auto dialog = weak.lock())
            {
                dialog->closed_ = true;
                if (dialog->closedCallback_) dialog->closedCallback_();
            }
        });
    }

    void AnalysisWorkflowDialog::Validate()
    {
        if (kind_ == AnalysisWorkflowKind::LinearModelTrellis)
        {
            const bool hasResponse = response_.SelectedIndex() > 0;
            const bool hasRow = rowCondition_.SelectedIndex() > 0;
            const bool hasColumn = columnCondition_.SelectedIndex() > 0;
            const bool distinct = !hasRow || !hasColumn ||
                ItemText(rowCondition_.SelectedItem()) != ItemText(columnCondition_.SelectedItem());
            const bool valid = hasResponse && (hasRow || hasColumn) && distinct;
            validation_.Text(!hasResponse ? L"Choose a numeric dependent variable." :
                !(hasRow || hasColumn) ? L"Choose at least one panel variable." :
                !distinct ? L"Panel rows and columns must be different." : L"");
            dialog_.IsPrimaryButtonEnabled(valid); return;
        }
        const bool variablesOnly = kind_ == AnalysisWorkflowKind::Table1 ||
            kind_ == AnalysisWorkflowKind::CorrelationMatrix ||
            kind_ == AnalysisWorkflowKind::Dimensionality ||
            kind_ == AnalysisWorkflowKind::FactorAnalysis ||
            kind_ == AnalysisWorkflowKind::QuickCluster;
        const bool multiResponse = kind_ == AnalysisWorkflowKind::OneSampleT ||
            kind_ == AnalysisWorkflowKind::IndependentT ||
            kind_ == AnalysisWorkflowKind::OneWayAnova;
        const bool needsTwoVariables = kind_ == AnalysisWorkflowKind::CorrelationMatrix ||
            kind_ == AnalysisWorkflowKind::Dimensionality ||
            kind_ == AnalysisWorkflowKind::FactorAnalysis;
        const bool optionalFixedEffects = kind_ == AnalysisWorkflowKind::LinearMixedModel ||
            kind_ == AnalysisWorkflowKind::GeneralizedMixedModel;
        const bool needsPredictor = variables_.Visibility() == Visibility::Visible &&
            !variablesOnly && !multiResponse && !optionalFixedEffects;
        bool valid = variablesOnly || multiResponse || response_.SelectedIndex() > 0;
        std::wstring message;
        const uint32_t selected = variables_.SelectedItems().Size();
        if (variablesOnly && selected < (needsTwoVariables ? 2u : 1u))
        { valid = false; message = needsTwoVariables ? L"Select at least two variables." : L"Select at least one variable."; }
        if (multiResponse && selected < 1u)
        { valid = false; message = L"Select at least one dependent variable."; }
        if (needsPredictor && selected < (needsTwoVariables ? 2u : 1u))
        { valid = false; message = needsTwoVariables ? L"Select at least two predictors." : L"Select at least one predictor."; }
        if (kind_ == AnalysisWorkflowKind::PairedT &&
            (secondary_.SelectedIndex() <= 0 || ItemText(response_.SelectedItem()) == ItemText(secondary_.SelectedItem())))
        { valid = false; message = L"Choose two different compatible variables."; }
        if ((kind_ == AnalysisWorkflowKind::IndependentT || kind_ == AnalysisWorkflowKind::OneWayAnova ||
             kind_ == AnalysisWorkflowKind::LinearMixedModel || kind_ == AnalysisWorkflowKind::GeneralizedMixedModel) &&
            groupVariable_.SelectedIndex() <= 0)
        { valid = false; message = L"Choose a grouping variable."; }
        if (kind_ == AnalysisWorkflowKind::LinearModelTrellis && columnCondition_.SelectedIndex() < 0)
        { valid = false; message = L"Choose a column conditioning variable."; }
        dialog_.IsPrimaryButtonEnabled(valid); validation_.Text(message);
    }

    void AnalysisWorkflowDialog::SubmitNow()
    {
        if (!dialog_.IsPrimaryButtonEnabled() || !submit_) return;
        AnalysisWorkflowSpecification specification; specification.kind = kind_;
        specification.group = dataframe_.group;
        if (kind_ == AnalysisWorkflowKind::LinearModelTrellis)
        {
            if (response_.SelectedIndex() > 0)
                specification.response = to_string(ItemText(response_.SelectedItem()));
            if (rowCondition_.SelectedIndex() > 0)
                specification.rowCondition = to_string(ItemText(rowCondition_.SelectedItem()));
            if (columnCondition_.SelectedIndex() > 0)
                specification.columnCondition = to_string(ItemText(columnCondition_.SelectedItem()));
            specification.variables =
                ::rlispstat::core::InitialAnalysisVariables(initial_, "predictors");
            specification.scope = SelectedAnalysisScopeChoice(scope_);
            submit_(specification); Close(); return;
        }
        if (response_.SelectedIndex() > 0) specification.response = to_string(ItemText(response_.SelectedItem()));
        if (secondary_.SelectedIndex() > 0) specification.secondary = to_string(ItemText(secondary_.SelectedItem()));
        if (groupVariable_.SelectedIndex() > 0) specification.groupVariable = to_string(ItemText(groupVariable_.SelectedItem()));
        if (rowCondition_.SelectedIndex() > 0) specification.rowCondition = to_string(ItemText(rowCondition_.SelectedItem()));
        if (columnCondition_.SelectedItem()) specification.columnCondition = to_string(ItemText(columnCondition_.SelectedItem()));
        for (auto const& item : variables_.SelectedItems())
        {
            const std::string name = to_string(ItemText(item));
            if (name != specification.response) specification.variables.push_back(name);
        }
        if (!distributionIds_.empty())
            specification.family = distributionIds_[static_cast<std::size_t>(
                std::clamp(family_.SelectedIndex(), 0,
                    static_cast<int32_t>(distributionIds_.size() - 1)))];
        if (!linkIds_.empty())
            specification.link = linkIds_[static_cast<std::size_t>(
                std::clamp(link_.SelectedIndex(), 0,
                    static_cast<int32_t>(linkIds_.size() - 1)))];
        specification.method = method_.SelectedIndex() == 1 ? "factor" : "pca";
        submit_(specification); Close();
    }

    void AnalysisWorkflowDialog::Show()
    {
        if (closed_) return;
        dialog_.Opened([this](auto const&, auto const&)
        {
            if (variables_ && variables_.Visibility() == Visibility::Visible)
                variables_.Focus(FocusState::Programmatic);
            else if (response_) response_.Focus(FocusState::Programmatic);
        });
        dialog_.ShowAsync();
    }
    void AnalysisWorkflowDialog::Close() { if (!closed_) dialog_.Hide(); }

    std::shared_ptr<DescriptiveStatisticsDialog> DescriptiveStatisticsDialog::Create(
        Window const& owner, ::rlispstat::core::DataFrameModel const& dataframe,
        std::set<int> const& selectedRows,
        std::vector<::rlispstat::core::SavedSelection> savedSelections,
        ::rlispstat::core::InitialAnalysisSpecification initial,
        Submit submit, Closed closed)
    {
        auto dialog = std::shared_ptr<DescriptiveStatisticsDialog>(
            new DescriptiveStatisticsDialog(owner, dataframe, selectedRows,
                                             std::move(savedSelections),
                                             std::move(initial), std::move(submit), std::move(closed)));
        dialog->AttachLifetime();
        return dialog;
    }

    DescriptiveStatisticsDialog::DescriptiveStatisticsDialog(Window const& owner,
        ::rlispstat::core::DataFrameModel const& dataframe,
        std::set<int> const& selectedRows,
        std::vector<::rlispstat::core::SavedSelection> savedSelections,
        ::rlispstat::core::InitialAnalysisSpecification initial,
        Submit submit, Closed closed)
        : owner_(owner), dataframe_(dataframe), initialSelection_(selectedRows),
          savedSelections_(std::move(savedSelections)),
          initial_(std::move(initial)), submit_(std::move(submit)),
          closedCallback_(std::move(closed))
    {
        Initialize();
    }

    void DescriptiveStatisticsDialog::Initialize()
    {
        dialog_ = Controls::ContentDialog();
        auto root = Controls::Grid(); root.Width(600); root.RowSpacing(10);
        root.RowDefinitions().Append(Controls::RowDefinition());
        root.RowDefinitions().Append(Controls::RowDefinition());
        root.RowDefinitions().Append(Controls::RowDefinition());
        root.RowDefinitions().Append(Controls::RowDefinition());
        root.RowDefinitions().Append(Controls::RowDefinition());
        auto dataset = DialogFieldLabel(std::wstring(L"Dataset: ") + to_hstring(dataframe_.group).c_str());
        dataset.FontWeight(Windows::UI::Text::FontWeights::SemiBold()); root.Children().Append(dataset);

        auto variables = Controls::Grid(); variables.ColumnSpacing(8);
        auto listCol = Controls::ColumnDefinition(); listCol.Width(GridLengthHelper::FromValueAndType(1, GridUnitType::Star)); variables.ColumnDefinitions().Append(listCol);
        auto buttonsCol = Controls::ColumnDefinition(); buttonsCol.Width(GridLengthHelper::FromPixels(72)); variables.ColumnDefinitions().Append(buttonsCol);
        auto selectedCol = Controls::ColumnDefinition(); selectedCol.Width(GridLengthHelper::FromValueAndType(1, GridUnitType::Star)); variables.ColumnDefinitions().Append(selectedCol);
        available_ = Controls::ListView(); ConfigureDialogList(available_); available_.SelectionMode(Controls::ListViewSelectionMode::Extended); available_.Height(190);
        selected_ = Controls::ListView(); ConfigureDialogList(selected_); selected_.SelectionMode(Controls::ListViewSelectionMode::Extended); selected_.Height(190);
        const auto initialVariables =
            ::rlispstat::core::InitialAnalysisVariables(initial_, "variables");
        for (auto const& column : dataframe_.columns)
        {
            const std::string label = column.displayName.empty() ? column.name : column.displayName;
            auto item = box_value(to_hstring(label + "  [" +
                ::rlispstat::core::VariableTypeDisplayName(column.type) + "]"));
            if (std::find(initialVariables.begin(), initialVariables.end(), column.name) != initialVariables.end())
                selected_.Items().Append(item);
            else
                available_.Items().Append(item);
        }
        auto assign = Controls::StackPanel(); assign.Spacing(8); assign.VerticalAlignment(VerticalAlignment::Center);
        auto add = FooterButton(L"Add >"); auto remove = FooterButton(L"< Remove");
        add.MinWidth(68); remove.MinWidth(68); add.Click([this](auto const&, auto const&) { AddVariables(); });
        remove.Click([this](auto const&, auto const&) { RemoveVariables(); }); assign.Children().Append(add); assign.Children().Append(remove);
        Controls::Grid::SetColumn(assign, 1); Controls::Grid::SetColumn(selected_, 2);
        variables.Children().Append(available_); variables.Children().Append(assign); variables.Children().Append(selected_);
        available_.DoubleTapped([this](auto const&, auto const&) { AddVariables(); });
        selected_.DoubleTapped([this](auto const&, auto const&) { RemoveVariables(); });
        selected_.KeyDown([this](auto const&, Input::KeyRoutedEventArgs const& e) { if (e.Key() == Windows::System::VirtualKey::Delete || e.Key() == Windows::System::VirtualKey::Back) { RemoveVariables(); e.Handled(true); } });
        Controls::Grid::SetRow(variables, 1); root.Children().Append(variables);

        auto options = Controls::Grid(); options.ColumnSpacing(8); options.RowSpacing(6);
        auto lcol = Controls::ColumnDefinition(); lcol.Width(GridLengthHelper::FromPixels(120)); options.ColumnDefinitions().Append(lcol); options.ColumnDefinitions().Append(Controls::ColumnDefinition());
        for (int i = 0; i < 3; ++i) options.RowDefinitions().Append(Controls::RowDefinition());
        options.Children().Append(DialogFieldLabel(L"Group by:"));
        groupVariable_ = Controls::ComboBox(); groupVariable_.Items().Append(box_value(L"(none)"));
        for (auto const& column : dataframe_.columns)
            if (::rlispstat::core::AnalysisVariableIsCompatible(
                    column, ::rlispstat::core::AnalysisVariableType::Categorical))
                groupVariable_.Items().Append(box_value(to_hstring(column.name)));
        groupVariable_.SelectedIndex(0); Controls::Grid::SetColumn(groupVariable_, 1); options.Children().Append(groupVariable_);
        const std::string initialGroup =
            ::rlispstat::core::InitialAnalysisVariable(initial_, "group");
        for (uint32_t index = 1; index < groupVariable_.Items().Size(); ++index)
            if (to_string(ItemText(groupVariable_.Items().GetAt(index))) == initialGroup)
            { groupVariable_.SelectedIndex(static_cast<int32_t>(index)); break; }
        auto scopeLabel = DialogFieldLabel(L"Analysis scope:"); Controls::Grid::SetRow(scopeLabel, 1); options.Children().Append(scopeLabel);
        scope_ = Controls::ComboBox();
        PopulateAnalysisScopeCombo(scope_,
            ::rlispstat::core::BuildAnalysisScopeChoices(
                initialSelection_.size(), savedSelections_, false), "all");
        Controls::Grid::SetRow(scope_, 1); Controls::Grid::SetColumn(scope_, 1); options.Children().Append(scope_);
        auto ordinalLabel = DialogFieldLabel(L"Ordinal variables as:"); Controls::Grid::SetRow(ordinalLabel, 2); options.Children().Append(ordinalLabel);
        ordinalAs_ = Controls::ComboBox(); ordinalAs_.Items().Append(box_value(L"Ordinal")); ordinalAs_.Items().Append(box_value(L"Categorical")); ordinalAs_.SelectedIndex(0);
        Controls::Grid::SetRow(ordinalAs_, 2); Controls::Grid::SetColumn(ordinalAs_, 1); options.Children().Append(ordinalAs_);
        Controls::Grid::SetRow(options, 2); root.Children().Append(options);

        auto checks = Controls::StackPanel(); checks.Orientation(Controls::Orientation::Horizontal); checks.Spacing(16);
        includeMissing_ = Controls::CheckBox(); includeMissing_.Content(box_value(L"Show missing")); includeMissing_.IsChecked(true);
        showP_ = Controls::CheckBox(); showP_.Content(box_value(L"Show p-values")); showP_.IsChecked(true);
        showTest_ = Controls::CheckBox(); showTest_.Content(box_value(L"Show tests")); showTest_.IsChecked(true);
        checks.Children().Append(includeMissing_); checks.Children().Append(showP_); checks.Children().Append(showTest_);
        auto updateGrouped = [this]() { const bool grouped = groupVariable_.SelectedIndex() > 0; showP_.IsEnabled(grouped); showTest_.IsEnabled(grouped); };
        groupVariable_.SelectionChanged([updateGrouped](auto const&, auto const&) { updateGrouped(); }); updateGrouped();
        Controls::Grid::SetRow(checks, 3); root.Children().Append(checks);

        validation_ = DialogFieldLabel(L"Select at least one variable."); validation_.Foreground(Brush(177, 48, 45));
        Controls::Grid::SetRow(validation_, 4); root.Children().Append(validation_);
        ConfigureNativeDialog(dialog_, owner_, L"Descriptive statistics", root, L"Run", 600);
        dialog_.PrimaryButtonClick([this](auto const&,
            Controls::ContentDialogButtonClickEventArgs const& args)
        {
            args.Cancel(true);
            SubmitNow();
        });
        Validate();
    }

    void DescriptiveStatisticsDialog::AttachLifetime()
    {
        std::weak_ptr<DescriptiveStatisticsDialog> weak = shared_from_this();
        dialog_.Closed([weak](auto const&, auto const&) { if (auto dialog = weak.lock()) { dialog->closed_ = true; if (dialog->closedCallback_) dialog->closedCallback_(); } });
    }

    void DescriptiveStatisticsDialog::AddVariables()
    {
        std::vector<Windows::Foundation::IInspectable> items; for (auto const& item : available_.SelectedItems()) items.push_back(item);
        if (items.empty() && available_.SelectedItem()) items.push_back(available_.SelectedItem());
        for (auto const& item : items) { uint32_t index = 0; if (available_.Items().IndexOf(item, index)) available_.Items().RemoveAt(index); selected_.Items().Append(item); }
        Validate();
    }

    void DescriptiveStatisticsDialog::RemoveVariables()
    {
        std::vector<Windows::Foundation::IInspectable> items; for (auto const& item : selected_.SelectedItems()) items.push_back(item);
        if (items.empty() && selected_.SelectedItem()) items.push_back(selected_.SelectedItem());
        for (auto const& item : items) { uint32_t index = 0; if (selected_.Items().IndexOf(item, index)) selected_.Items().RemoveAt(index); available_.Items().Append(item); }
        Validate();
    }

    void DescriptiveStatisticsDialog::Validate()
    {
        const bool valid = !busy_ && selected_.Items().Size() > 0;
        dialog_.IsPrimaryButtonEnabled(valid);
        if (!busy_) validation_.Text(valid ? L"" : L"Select at least one variable.");
    }

    void DescriptiveStatisticsDialog::SubmitNow()
    {
        if (!dialog_.IsPrimaryButtonEnabled() || !submit_) return;
        DescriptiveStatisticsSpecification specification;
        specification.requestId = NewRequestId("table1_windows_"); specification.group = dataframe_.group;
        for (auto const& selectedItem : selected_.Items())
        {
            std::string label = to_string(ItemText(selectedItem));
            const auto marker = label.rfind("  ["); if (marker != std::string::npos) label.resize(marker);
            for (auto const& column : dataframe_.columns) { const std::string display = column.displayName.empty() ? column.name : column.displayName; if (display == label) { specification.variables.push_back(column.name); break; } }
        }
        if (groupVariable_.SelectedIndex() > 0) specification.groupVariable = to_string(ItemText(groupVariable_.SelectedItem()));
        specification.includeMissing = includeMissing_.IsChecked().GetBoolean();
        specification.showP = showP_.IsEnabled() && showP_.IsChecked().GetBoolean();
        specification.showTest = showTest_.IsEnabled() && showTest_.IsChecked().GetBoolean();
        specification.ordinalAs = ordinalAs_.SelectedIndex() == 1 ? "categorical" : "ordinal";
        // Execution captures the dataset global scope in App::QueueDescriptiveStatistics.
        busy_ = true; validation_.Text(L"Waiting for R…"); validation_.Foreground(Brush(75, 82, 92)); dialog_.IsPrimaryButtonEnabled(false);
        submit_(specification);
    }

    void DescriptiveStatisticsDialog::ReportError(std::string const& message)
    {
        busy_ = false; validation_.Foreground(Brush(177, 48, 45)); validation_.Text(to_hstring(message)); Validate(); available_.Focus(FocusState::Programmatic);
    }
    void DescriptiveStatisticsDialog::SetAnalysisScope(::rlispstat::core::AnalysisScope const& scope)
    {
        PopulateAnalysisScopeCombo(scope_, {{"global", ::rlispstat::core::AnalysisScopeSummary(scope,scope.totalDatasetRows,false)}}, "global");
    }

    void DescriptiveStatisticsDialog::Show()
    {
        if (closed_) return;
        dialog_.Opened([this](auto const&, auto const&)
        { available_.Focus(FocusState::Programmatic); });
        dialog_.ShowAsync();
    }
    void DescriptiveStatisticsDialog::Close() { if (!closed_) dialog_.Hide(); }

    std::shared_ptr<ContingencyTableDialog> ContingencyTableDialog::Create(
        Window const& owner, ::rlispstat::core::DataFrameModel const& dataframe,
        ::rlispstat::core::InitialAnalysisSpecification initial,
        Submit submit, Closed closed)
    {
        auto dialog = std::shared_ptr<ContingencyTableDialog>(
            new ContingencyTableDialog(owner, dataframe, std::move(initial),
                                       std::move(submit), std::move(closed)));
        dialog->AttachLifetime();
        return dialog;
    }

    ContingencyTableDialog::ContingencyTableDialog(
        Window const& owner, ::rlispstat::core::DataFrameModel const& dataframe,
        ::rlispstat::core::InitialAnalysisSpecification initial,
        Submit submit, Closed closed)
        : owner_(owner), dataframe_(dataframe), initial_(std::move(initial)),
          submit_(std::move(submit)),
          closedCallback_(std::move(closed))
    {
        Initialize();
    }

    void ContingencyTableDialog::Initialize()
    {
        dialog_ = Controls::ContentDialog();
        auto root = Controls::StackPanel(); root.Width(380); root.Spacing(10);
        root.Children().Append(DialogContentIntroduction(
            L"Choose row and column variables. The resulting table remains linked to the active dataset.",
            dataframe_.group));

        auto fields = Controls::Grid(); fields.ColumnSpacing(10); fields.RowSpacing(8);
        auto labels = Controls::ColumnDefinition(); labels.Width(GridLengthHelper::FromPixels(120));
        fields.ColumnDefinitions().Append(labels); fields.ColumnDefinitions().Append(Controls::ColumnDefinition());
        fields.RowDefinitions().Append(Controls::RowDefinition()); fields.RowDefinitions().Append(Controls::RowDefinition());
        fields.Children().Append(DialogFieldLabel(L"Row variable:"));
        rowVariable_ = Controls::ComboBox();
        rowVariable_.Items().Append(box_value(L"(choose row variable)"));
        Controls::Grid::SetColumn(rowVariable_, 1); fields.Children().Append(rowVariable_);
        auto columnLabel = DialogFieldLabel(L"Column variable:"); Controls::Grid::SetRow(columnLabel, 1); fields.Children().Append(columnLabel);
        columnVariable_ = Controls::ComboBox();
        columnVariable_.Items().Append(box_value(L"(choose column variable)"));
        Controls::Grid::SetRow(columnVariable_, 1); Controls::Grid::SetColumn(columnVariable_, 1); fields.Children().Append(columnVariable_);
        const auto definition = ::rlispstat::core::SharedAnalysisDefinition(
            ::rlispstat::core::SharedAnalysisKind::ContingencyTable);
        for (auto const& column : dataframe_.columns)
        {
            if (::rlispstat::core::AnalysisVariableIsCompatible(
                    column, definition.slots.front().acceptedType))
            {
                rowVariable_.Items().Append(box_value(to_hstring(column.name)));
                columnVariable_.Items().Append(box_value(to_hstring(column.name)));
            }
        }
        rowVariable_.SelectedIndex(0);
        columnVariable_.SelectedIndex(0);
        const auto selectInitial = [](Controls::ComboBox const& combo,
                                      std::string const& value)
        {
            for (uint32_t index = 1; !value.empty() && index < combo.Items().Size(); ++index)
                if (to_string(ItemText(combo.Items().GetAt(index))) == value)
                { combo.SelectedIndex(static_cast<int32_t>(index)); break; }
        };
        selectInitial(rowVariable_,
            ::rlispstat::core::InitialAnalysisVariable(initial_, "row"));
        selectInitial(columnVariable_,
            ::rlispstat::core::InitialAnalysisVariable(initial_, "column"));
        root.Children().Append(fields);

        validation_ = DialogFieldLabel(L""); validation_.Foreground(Brush(177, 48, 45));
        root.Children().Append(validation_);
        rowVariable_.SelectionChanged([this](auto const&, auto const&) { Validate(); });
        columnVariable_.SelectionChanged([this](auto const&, auto const&) { Validate(); });
        ConfigureNativeDialog(dialog_, owner_, L"Contingency Table", root, L"Run", 380);
        dialog_.PrimaryButtonClick([this](auto const&,
            Controls::ContentDialogButtonClickEventArgs const& args)
        {
            args.Cancel(true);
            SubmitNow();
        });
        Validate();
    }

    void ContingencyTableDialog::AttachLifetime()
    {
        std::weak_ptr<ContingencyTableDialog> weak = shared_from_this();
        dialog_.Closed([weak](auto const&, auto const&)
        {
            if (auto dialog = weak.lock())
            {
                dialog->closed_ = true;
                if (dialog->closedCallback_) dialog->closedCallback_();
            }
        });
    }

    void ContingencyTableDialog::Validate()
    {
        const bool enough = rowVariable_.Items().Size() >= 3;
        const bool chosen = rowVariable_.SelectedIndex() > 0 && columnVariable_.SelectedIndex() > 0;
        const bool different = chosen &&
            ItemText(rowVariable_.SelectedItem()) != ItemText(columnVariable_.SelectedItem());
        dialog_.IsPrimaryButtonEnabled(enough && different);
        validation_.Text(!enough ? L"At least two categorical variables are required." :
            !chosen ? L"Choose row and column variables." :
            (different ? L"" : L"Row variable and column variable must be different."));
    }

    void ContingencyTableDialog::SubmitNow()
    {
        if (!dialog_.IsPrimaryButtonEnabled() || !submit_) return;
        submit_({ dataframe_.group, to_string(ItemText(rowVariable_.SelectedItem())),
                  to_string(ItemText(columnVariable_.SelectedItem())) });
        Close();
    }

    void ContingencyTableDialog::Show()
    {
        if (closed_) return;
        dialog_.Opened([this](auto const&, auto const&)
        { rowVariable_.Focus(FocusState::Programmatic); });
        dialog_.ShowAsync();
    }
    void ContingencyTableDialog::Close() { if (!closed_) dialog_.Hide(); }

    std::shared_ptr<MissingDataImputationDialog> MissingDataImputationDialog::Create(
        Window const& owner, ::rlispstat::core::DataFrameModel const& dataframe,
        Submit submit, Closed closed, Failed failed)
    {
        auto dialog = std::shared_ptr<MissingDataImputationDialog>(
            new MissingDataImputationDialog(owner, dataframe, std::move(submit),
                                            std::move(closed), std::move(failed)));
        dialog->AttachLifetime();
        return dialog;
    }

    MissingDataImputationDialog::MissingDataImputationDialog(
        Window const& owner, ::rlispstat::core::DataFrameModel const& dataframe,
        Submit submit, Closed closed, Failed failed)
        : owner_(owner), dataframe_(dataframe), submit_(std::move(submit)),
          closedCallback_(std::move(closed)), failedCallback_(std::move(failed))
    {
        Initialize();
    }

    void MissingDataImputationDialog::Initialize()
    {
        const auto labels = ::rlispstat::core::BuildMissingDataImputationDialogState();
        dialog_ = Controls::ContentDialog();
        auto root = Controls::StackPanel();
        root.MinWidth(600); root.MaxWidth(680); root.Spacing(12);
        root.Children().Append(DialogContentIntroduction(
            to_hstring(labels.informativeText).c_str(), dataframe_.group));

        auto modelNote = Controls::TextBlock();
        modelNote.Text(to_hstring(labels.automaticModelNote));
        modelNote.TextWrapping(TextWrapping::Wrap);
        root.Children().Append(modelNote);

        auto settingsCard = Controls::Border();
        settingsCard.Background(Brush(247, 248, 249));
        settingsCard.BorderBrush(Brush(229, 231, 234));
        settingsCard.BorderThickness(Thickness{ 1 });
        settingsCard.CornerRadius(CornerRadius{ 4 });
        settingsCard.Padding(Thickness{ 12, 10, 12, 10 });
        auto settings = Controls::Grid(); settings.ColumnSpacing(8); settings.RowSpacing(8);
        for (int index = 0; index < 2; ++index)
        {
            auto row = Controls::RowDefinition();
            row.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Auto));
            settings.RowDefinitions().Append(row);
        }
        for (int index = 0; index < 6; ++index)
        {
            auto definition = Controls::ColumnDefinition();
            definition.Width(GridLengthHelper::FromValueAndType(
                index % 2 == 0 ? 1.0 : 0.72, GridUnitType::Star));
            settings.ColumnDefinitions().Append(definition);
        }
        auto label = [&](std::wstring const& text, int column)
        {
            auto value = DialogFieldLabel(text);
            value.HorizontalAlignment(HorizontalAlignment::Right);
            Controls::Grid::SetColumn(value, column);
            settings.Children().Append(value);
        };
        label(to_hstring(labels.imputationsLabel + ":").c_str(), 0);
        imputations_ = Controls::TextBox(); imputations_.Text(L"5");
        imputations_.HorizontalAlignment(HorizontalAlignment::Stretch);
        Controls::Grid::SetColumn(imputations_, 1); settings.Children().Append(imputations_);
        label(to_hstring(labels.iterationsLabel + ":").c_str(), 2);
        iterations_ = Controls::TextBox(); iterations_.Text(L"5");
        iterations_.HorizontalAlignment(HorizontalAlignment::Stretch);
        Controls::Grid::SetColumn(iterations_, 3); settings.Children().Append(iterations_);
        label(to_hstring(labels.seedLabel + ":").c_str(), 4);
        seed_ = Controls::TextBox(); seed_.PlaceholderText(to_hstring(labels.seedPlaceholder));
        seed_.HorizontalAlignment(HorizontalAlignment::Stretch);
        Controls::Grid::SetColumn(seed_, 5); settings.Children().Append(seed_);
        openDataSheet_ = Controls::CheckBox();
        openDataSheet_.Content(box_value(to_hstring(labels.openDataSheetTitle)));
        openDataSheet_.IsChecked(true);
        Controls::Grid::SetRow(openDataSheet_, 1);
        Controls::Grid::SetColumnSpan(openDataSheet_, 6);
        settings.Children().Append(openDataSheet_);
        settingsCard.Child(settings); root.Children().Append(settingsCard);

        auto tools = Controls::Grid(); tools.ColumnSpacing(8);
        auto summaryColumn = Controls::ColumnDefinition();
        summaryColumn.Width(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
        tools.ColumnDefinitions().Append(summaryColumn);
        tools.ColumnDefinitions().Append(Controls::ColumnDefinition());
        tools.ColumnDefinitions().Append(Controls::ColumnDefinition());
        summary_ = DialogFieldLabel(L""); summary_.Opacity(0.72); tools.Children().Append(summary_);
        auto deselectAll = FooterButton(to_hstring(labels.deselectAllTitle).c_str());
        Controls::Grid::SetColumn(deselectAll, 1); tools.Children().Append(deselectAll);
        auto deselectPredictors = FooterButton(to_hstring(labels.deselectPredictorsTitle).c_str());
        Controls::Grid::SetColumn(deselectPredictors, 2); tools.Children().Append(deselectPredictors);
        deselectAll.Click([this](auto const&, auto const&) { DeselectAll(); });
        deselectPredictors.Click([this](auto const&, auto const&) { DeselectPredictors(); });
        root.Children().Append(tools);

        auto variablesCard = Controls::Border();
        variablesCard.BorderBrush(Brush(224, 226, 229));
        variablesCard.BorderThickness(Thickness{ 1 });
        variablesCard.CornerRadius(CornerRadius{ 4 });
        auto variablesPanel = Controls::StackPanel();
        auto header = Controls::Grid(); header.ColumnSpacing(8);
        header.Background(Brush(247, 248, 249));
        header.Padding(Thickness{ 10, 6, 10, 6 });
        const std::array<double, 4> widths{ 2.0, 1.35, 1.0, 0.85 };
        for (double width : widths)
        {
            auto column = Controls::ColumnDefinition();
            column.Width(GridLengthHelper::FromValueAndType(width, GridUnitType::Star));
            header.ColumnDefinitions().Append(column);
        }
        const std::array<std::string, 4> titles{
            labels.imputeColumnTitle, labels.typeMissingColumnTitle,
            labels.methodColumnTitle, labels.predictorColumnTitle
        };
        for (int index = 0; index < 4; ++index)
        {
            auto title = DialogFieldLabel(to_hstring(titles[index]).c_str());
            title.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
            Controls::Grid::SetColumn(title, index); header.Children().Append(title);
        }
        variablesPanel.Children().Append(header);

        auto rowsPanel = Controls::StackPanel(); rowsPanel.Spacing(2);
        int missingColumnCount = 0;
        int defaultImputeCount = 0;
        for (auto const& column : dataframe_.columns)
        {
            const bool supported = ::rlispstat::core::DataColumnSupportedForMice(column);
            const bool allMissing = ::rlispstat::core::DataColumnAllMissing(column);
            const bool idLike = ::rlispstat::core::DataColumnLooksLikeId(column);
            const int missing = ::rlispstat::core::DataColumnMissingCount(column);
            if (missing > 0) ++missingColumnCount;
            const bool canImpute = supported && missing > 0 && !allMissing;
            const bool defaultImpute = canImpute && !idLike;
            const bool canPredict = supported && !allMissing && !idLike;
            if (defaultImpute) ++defaultImputeCount;

            auto row = Controls::Grid(); row.ColumnSpacing(8); row.MinHeight(28);
            row.Padding(Thickness{ 10, 1, 10, 1 });
            for (double width : widths)
            {
                auto definition = Controls::ColumnDefinition();
                definition.Width(GridLengthHelper::FromValueAndType(
                    width, GridUnitType::Star));
                row.ColumnDefinitions().Append(definition);
            }
            VariableRow controls; controls.name = column.name;
            controls.impute = Controls::CheckBox();
            controls.impute.Content(box_value(to_hstring(column.name)));
            controls.impute.FontSize(kDialogBodyFontSize);
            controls.impute.IsChecked(defaultImpute); controls.impute.IsEnabled(canImpute);
            row.Children().Append(controls.impute);
            auto status = DialogFieldLabel(to_hstring(
                ::rlispstat::core::MiceVariableStatusText(column, dataframe_.rows)).c_str());
            status.Opacity(0.68); Controls::Grid::SetColumn(status, 1); row.Children().Append(status);
            controls.method = Controls::ComboBox();
            controls.method.FontSize(kDialogBodyFontSize);
            controls.method.HorizontalAlignment(HorizontalAlignment::Stretch);
            const auto methods = ::rlispstat::core::MiceMethodOptions(column);
            if (methods.empty())
                controls.method.Items().Append(box_value(to_hstring(labels.unsupportedMethodTitle)));
            else
            {
                const auto defaultMethod = ::rlispstat::core::DefaultMiceMethod(column);
                int selected = 0;
                for (std::size_t index = 0; index < methods.size(); ++index)
                {
                    controls.method.Items().Append(box_value(to_hstring(methods[index])));
                    if (methods[index] == defaultMethod) selected = static_cast<int>(index);
                }
                controls.method.SelectedIndex(selected);
            }
            controls.method.IsEnabled(canImpute);
            Controls::Grid::SetColumn(controls.method, 2); row.Children().Append(controls.method);
            controls.predictor = Controls::CheckBox();
            controls.predictor.Content(box_value(to_hstring(labels.predictorUseTitle)));
            controls.predictor.FontSize(kDialogBodyFontSize);
            controls.predictor.IsChecked(canPredict); controls.predictor.IsEnabled(canPredict);
            Controls::Grid::SetColumn(controls.predictor, 3); row.Children().Append(controls.predictor);
            controls.impute.Checked([this](auto const&, auto const&) { Validate(); });
            controls.impute.Unchecked([this](auto const&, auto const&) { Validate(); });
            controls.predictor.Checked([this](auto const&, auto const&) { Validate(); });
            controls.predictor.Unchecked([this](auto const&, auto const&) { Validate(); });
            variableRows_.push_back(std::move(controls)); rowsPanel.Children().Append(row);
        }
        auto scroll = Controls::ScrollViewer(); scroll.Height(250);
        scroll.VerticalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);
        scroll.HorizontalScrollBarVisibility(Controls::ScrollBarVisibility::Disabled);
        scroll.Content(rowsPanel); variablesPanel.Children().Append(scroll);
        variablesCard.Child(variablesPanel); root.Children().Append(variablesCard);

        auto hint = DialogFieldLabel(to_hstring(labels.instructionHint).c_str());
        hint.Opacity(0.7); hint.TextWrapping(TextWrapping::Wrap); root.Children().Append(hint);
        auto methodHint = DialogFieldLabel(to_hstring(labels.methodHint).c_str());
        methodHint.Opacity(0.7); methodHint.TextWrapping(TextWrapping::Wrap);
        root.Children().Append(methodHint);
        validation_ = DialogFieldLabel(L""); validation_.Foreground(Brush(177, 48, 45));
        root.Children().Append(validation_);
        summary_.Text(to_hstring(::rlispstat::core::MiceDatasetSummaryText(
            dataframe_, missingColumnCount, defaultImputeCount)));

        imputations_.TextChanged([this](auto const&, auto const&) { Validate(); });
        iterations_.TextChanged([this](auto const&, auto const&) { Validate(); });
        seed_.TextChanged([this](auto const&, auto const&) { Validate(); });
        ConfigureNativeDialog(dialog_, owner_, to_hstring(labels.title).c_str(), root,
            to_hstring(labels.runButtonTitle).c_str(), 600);
        dialog_.PrimaryButtonClick([this](auto const&,
            Controls::ContentDialogButtonClickEventArgs const& args)
        {
            args.Cancel(true); SubmitNow();
        });
        Validate();
    }

    void MissingDataImputationDialog::AttachLifetime()
    {
        std::weak_ptr<MissingDataImputationDialog> weak = shared_from_this();
        dialog_.Opened([weak](auto const&, auto const&)
        {
            if (auto dialog = weak.lock())
            {
                dialog->visible_ = true;
                dialog->showPending_ = false;
                if (dialog->openWatchdog_) dialog->openWatchdog_.Stop();
                dialog->imputations_.Focus(FocusState::Programmatic);
            }
        });
        dialog_.Closed([weak](auto const&, auto const&)
        {
            if (auto dialog = weak.lock())
            {
                dialog->visible_ = false;
                dialog->showPending_ = false;
                if (dialog->openWatchdog_) dialog->openWatchdog_.Stop();
                if (dialog->closed_) return;
                dialog->closed_ = true;
                if (dialog->closedCallback_) dialog->closedCallback_();
            }
        });
    }

    void MissingDataImputationDialog::Validate()
    {
        const auto labels = ::rlispstat::core::BuildMissingDataImputationDialogState();
        const auto parseInteger = [](hstring const& text, int minimum, int& value)
        {
            const auto narrow = to_string(text);
            if (narrow.empty()) return false;
            char* end = nullptr; const long parsed = std::strtol(narrow.c_str(), &end, 10);
            if (!end || *end != '\0' || parsed < minimum || parsed > 1000000) return false;
            value = static_cast<int>(parsed); return true;
        };
        int m = 0, maxit = 0, seedValue = 0;
        const bool countsValid = parseInteger(imputations_.Text(), 1, m) &&
            parseInteger(iterations_.Text(), 0, maxit);
        const bool seedValid = seed_.Text().empty() || parseInteger(seed_.Text(), 0, seedValue);
        std::size_t imputeCount = 0, predictorCount = 0;
        for (auto const& row : variableRows_)
        {
            if (row.impute.IsChecked().GetBoolean()) ++imputeCount;
            if (row.predictor.IsChecked().GetBoolean()) ++predictorCount;
        }
        summary_.Text(to_hstring(::rlispstat::core::MiceSelectionSummaryText(
            imputeCount, predictorCount)));
        std::wstring error;
        if (!countsValid) error = L"Imputations must be positive and iterations cannot be negative.";
        else if (!seedValid) error = L"Seed must be empty or a non-negative integer.";
        else if (imputeCount == 0) error = to_hstring(labels.noImputeVariablesStatus).c_str();
        else if (predictorCount == 0) error = to_hstring(labels.noPredictorVariablesStatus).c_str();
        validation_.Text(hstring(error)); dialog_.IsPrimaryButtonEnabled(error.empty());
    }

    void MissingDataImputationDialog::DeselectAll()
    {
        for (auto const& row : variableRows_)
        {
            if (row.impute.IsEnabled()) row.impute.IsChecked(false);
            if (row.predictor.IsEnabled()) row.predictor.IsChecked(false);
        }
        Validate();
    }

    void MissingDataImputationDialog::DeselectPredictors()
    {
        for (auto const& row : variableRows_)
            if (row.predictor.IsEnabled()) row.predictor.IsChecked(false);
        Validate();
    }

    void MissingDataImputationDialog::SubmitNow()
    {
        if (!dialog_.IsPrimaryButtonEnabled() || !submit_) return;
        MissingDataImputationSpecification specification; specification.group = dataframe_.group;
        specification.imputations = std::atoi(to_string(imputations_.Text()).c_str());
        specification.iterations = std::atoi(to_string(iterations_.Text()).c_str());
        specification.seed = to_string(seed_.Text());
        specification.openDataSheet = openDataSheet_.IsChecked().GetBoolean();
        for (auto const& row : variableRows_)
        {
            if (row.impute.IsChecked().GetBoolean())
            {
                specification.imputeVariables.push_back(row.name);
                if (row.method.SelectedIndex() >= 0)
                    specification.methods[row.name] = to_string(ItemText(row.method.SelectedItem()));
            }
            if (row.predictor.IsChecked().GetBoolean())
                specification.predictorVariables.push_back(row.name);
        }
        submit_(specification); Close();
    }

    void MissingDataImputationDialog::Show()
    {
        if (closed_) return;
        // A ContentDialog belongs to its XamlRoot, not to the process as a
        // whole. Bring that owner forward before entering modal state so a
        // dialog requested from another LinkEDA window cannot open behind it.
        owner_.Activate();
        if (visible_ || showPending_) return;
        showPending_ = true;
        auto dispatcher = owner_.DispatcherQueue();
        if (dispatcher)
        {
            openWatchdog_ = dispatcher.CreateTimer();
            openWatchdog_.Interval(std::chrono::milliseconds(1500));
            openWatchdog_.IsRepeating(false);
            std::weak_ptr<MissingDataImputationDialog> weak = shared_from_this();
            openWatchdog_.Tick([weak](auto const&, auto const&)
            {
                if (auto dialog = weak.lock();
                    dialog && dialog->showPending_ && !dialog->visible_ && !dialog->closed_)
                {
                    dialog->ReportOpenFailure(
                        L"The Multiple Imputation dialog could not become visible. "
                        L"Its modal state has been cleared; the application menus remain available.");
                }
            });
            openWatchdog_.Start();
        }
        ShowAsyncOperation();
    }

    fire_and_forget MissingDataImputationDialog::ShowAsyncOperation()
    {
        auto lifetime = shared_from_this();
        try
        {
            co_await dialog_.ShowAsync();
            showPending_ = false;
        }
        catch (hresult_error const& error)
        {
            ReportOpenFailure(error.message().c_str());
        }
        catch (std::exception const& error)
        {
            ReportOpenFailure(to_hstring(error.what()).c_str());
        }
    }

    void MissingDataImputationDialog::ReportOpenFailure(std::wstring const& message)
    {
        if (closed_) return;
        auto lifetime = shared_from_this();
        showPending_ = false;
        visible_ = false;
        if (openWatchdog_) openWatchdog_.Stop();
        try { dialog_.Hide(); } catch (...) {}
        closed_ = true;
        if (failedCallback_) failedCallback_(message);
        if (closedCallback_) closedCallback_();
    }

    void MissingDataImputationDialog::Close()
    {
        if (closed_) return;
        // Keep the object alive while the owner removes it from its dialog map.
        // ContentDialog::Closed is not guaranteed to run synchronously after
        // Hide(), which previously left a stale entry that blocked a second run.
        auto keepAlive = shared_from_this();
        dialog_.Hide();
        showPending_ = false;
        if (openWatchdog_) openWatchdog_.Stop();
        visible_ = false;
        if (!closed_)
        {
            closed_ = true;
            if (closedCallback_) closedCallback_();
        }
    }

    std::shared_ptr<DatasetVariableDialog> DatasetVariableDialog::Create(
        Window const& owner, ::rlispstat::core::DataFrameModel const& dataframe,
        std::wstring title, std::wstring information, std::wstring primaryButton,
        bool multiple, std::string currentValue, Submit submit, Closed closed)
    {
        auto dialog = std::shared_ptr<DatasetVariableDialog>(new DatasetVariableDialog(
            owner, dataframe, std::move(title), std::move(information),
            std::move(primaryButton), multiple, std::move(currentValue),
            std::move(submit), std::move(closed)));
        dialog->AttachLifetime();
        return dialog;
    }

    DatasetVariableDialog::DatasetVariableDialog(
        Window const& owner, ::rlispstat::core::DataFrameModel const& dataframe,
        std::wstring title, std::wstring information, std::wstring primaryButton,
        bool multiple, std::string currentValue, Submit submit, Closed closed)
        : owner_(owner), dataframe_(dataframe), title_(std::move(title)),
          information_(std::move(information)), primaryButton_(std::move(primaryButton)),
          multiple_(multiple), currentValue_(std::move(currentValue)),
          submit_(std::move(submit)), closedCallback_(std::move(closed))
    {
        Initialize();
    }

    void DatasetVariableDialog::Initialize()
    {
        dialog_ = Controls::ContentDialog();
        auto root = Controls::StackPanel(); root.MinWidth(420); root.Spacing(10);
        root.Children().Append(DialogContentIntroduction(information_, dataframe_.group));
        if (!multiple_)
        {
            variableSearch_ = Controls::TextBox();
            variableSearch_.PlaceholderText(L"Search variables");
            variableSearch_.HorizontalAlignment(HorizontalAlignment::Stretch);
            variableSearch_.FontSize(kDialogBodyFontSize);
            root.Children().Append(variableSearch_);

            variableList_ = Controls::ListView();
            ConfigureDialogList(variableList_);
            variableList_.SelectionMode(Controls::ListViewSelectionMode::Single);
            variableList_.Height(300);
            variableList_.HorizontalAlignment(HorizontalAlignment::Stretch);
            root.Children().Append(variableList_);
            variableSearch_.TextChanged([this](auto const&, auto const&)
                { RebuildSingleVariableList(); });
            RebuildSingleVariableList();
        }
        else
        {
            auto actions = Controls::StackPanel();
            actions.Orientation(Controls::Orientation::Horizontal);
            actions.Spacing(8);
            auto selectAll = Controls::Button(); selectAll.Content(box_value(L"Select all"));
            auto selectNone = Controls::Button(); selectNone.Content(box_value(L"Select none"));
            selectAll.Click([this](auto const&, auto const&)
                {
                    variableList_.SelectedItems().Clear();
                    for (auto const& item : variableList_.Items())
                        variableList_.SelectedItems().Append(item);
                });
            selectNone.Click([this](auto const&, auto const&)
                { variableList_.SelectedItems().Clear(); });
            actions.Children().Append(selectAll); actions.Children().Append(selectNone);
            root.Children().Append(actions);

            auto selectionHint = DialogFieldLabel(
                L"Shift+click selects a range; Ctrl+click toggles individual variables.");
            selectionHint.FontSize(11.0);
            selectionHint.Foreground(Brush(96, 96, 96));
            selectionHint.TextWrapping(TextWrapping::Wrap);
            root.Children().Append(selectionHint);

            variableList_ = Controls::ListView();
            ConfigureDialogList(variableList_);
            variableList_.SelectionMode(Controls::ListViewSelectionMode::Extended);
            variableList_.Height(300);
            variableList_.HorizontalAlignment(HorizontalAlignment::Stretch);
            for (auto const& column : dataframe_.columns)
                variableList_.Items().Append(box_value(to_hstring(column.name)));
            for (auto const& item : variableList_.Items())
                variableList_.SelectedItems().Append(item);
            root.Children().Append(variableList_);
        }
        ConfigureNativeDialog(dialog_, owner_, title_, root, primaryButton_, 420);
        dialog_.PrimaryButtonClick([this](auto const&,
            Controls::ContentDialogButtonClickEventArgs const& args)
        {
            std::vector<std::string> selected;
            if (!multiple_)
            {
                if (variableList_.SelectedIndex() > 0)
                    selected.push_back(to_string(ItemText(variableList_.SelectedItem())));
            }
            else
            {
                std::set<std::string> chosen;
                for (auto const& item : variableList_.SelectedItems())
                    chosen.insert(to_string(ItemText(item)));
                for (auto const& column : dataframe_.columns)
                    if (chosen.find(column.name) != chosen.end())
                        selected.push_back(column.name);
                if (selected.empty())
                {
                    args.Cancel(true);
                    return;
                }
            }
            if (submit_) submit_(selected);
        });
    }

    void DatasetVariableDialog::RebuildSingleVariableList()
    {
        if (!variableList_) return;
        std::wstring query = variableSearch_ ? variableSearch_.Text().c_str() : L"";
        std::transform(query.begin(), query.end(), query.begin(),
            [](wchar_t value) { return static_cast<wchar_t>(std::towlower(value)); });
        variableList_.Items().Clear();
        variableList_.Items().Append(box_value(
            to_hstring(::rlispstat::core::NoVariableOptionTitle())));
        int selected = 0;
        for (auto const& column : dataframe_.columns)
        {
            std::wstring name = to_hstring(column.name).c_str();
            std::wstring searchable = name;
            std::transform(searchable.begin(), searchable.end(), searchable.begin(),
                [](wchar_t value) { return static_cast<wchar_t>(std::towlower(value)); });
            if (!query.empty() && searchable.find(query) == std::wstring::npos) continue;
            variableList_.Items().Append(box_value(hstring(name)));
            if (column.name == currentValue_)
                selected = static_cast<int>(variableList_.Items().Size()) - 1;
        }
        if (selected == 0 && !query.empty() && variableList_.Items().Size() > 1)
            selected = 1;
        variableList_.SelectedIndex(selected);
    }

    void DatasetVariableDialog::AttachLifetime()
    {
        std::weak_ptr<DatasetVariableDialog> weak = shared_from_this();
        dialog_.Closed([weak](auto const&, auto const&)
        {
            if (auto dialog = weak.lock())
            {
                if (dialog->closed_) return;
                dialog->closed_ = true;
                if (dialog->closedCallback_) dialog->closedCallback_();
            }
        });
    }

    void DatasetVariableDialog::Show()
    {
        if (closed_) return;
        owner_.Activate();
        dialog_.ShowAsync();
    }

    void DatasetVariableDialog::Close()
    {
        if (closed_) return;
        dialog_.Hide();
    }

    std::shared_ptr<RDataAssignmentDialog> RDataAssignmentDialog::Create(
        Window const& owner, ::rlispstat::core::DataFrameModel const& dataframe,
        bool selectedRowsOnly, std::size_t selectedRowCount,
        Submit submit, Closed closed)
    {
        auto dialog = std::shared_ptr<RDataAssignmentDialog>(
            new RDataAssignmentDialog(owner, dataframe, selectedRowsOnly,
                                      selectedRowCount, std::move(submit),
                                      std::move(closed)));
        dialog->AttachLifetime();
        return dialog;
    }

    RDataAssignmentDialog::RDataAssignmentDialog(
        Window const& owner, ::rlispstat::core::DataFrameModel const& dataframe,
        bool selectedRowsOnly, std::size_t selectedRowCount,
        Submit submit, Closed closed)
        : owner_(owner), dataframe_(dataframe),
          selectedRowsOnly_(selectedRowsOnly), selectedRowCount_(selectedRowCount),
          submit_(std::move(submit)), closedCallback_(std::move(closed))
    {
        Initialize();
    }

    void RDataAssignmentDialog::Initialize()
    {
        dialog_ = Controls::ContentDialog();
        auto root = Controls::StackPanel(); root.Width(380); root.Spacing(10);
        const auto description = selectedRowsOnly_
            ? L"Create a data frame in the current R session from the selected observations."
            : L"Create a data frame in the current R session from this LinkEDA dataset.";
        root.Children().Append(DialogContentIntroduction(description, dataframe_.group));

        if (selectedRowsOnly_)
        {
            auto count = DialogFieldLabel(L"Rows to return: " +
                std::to_wstring(selectedRowCount_));
            count.Opacity(0.78); root.Children().Append(count);
        }

        auto fields = Controls::Grid(); fields.ColumnSpacing(10); fields.RowSpacing(8);
        auto labels = Controls::ColumnDefinition();
        labels.Width(GridLengthHelper::FromPixels(105));
        fields.ColumnDefinitions().Append(labels);
        fields.ColumnDefinitions().Append(Controls::ColumnDefinition());
        fields.RowDefinitions().Append(Controls::RowDefinition());
        fields.Children().Append(DialogFieldLabel(L"R object name:"));
        objectName_ = Controls::TextBox();
        objectName_.Text(to_hstring(DefaultRObjectName(dataframe_.group)));
        objectName_.SelectAll();
        Controls::Grid::SetColumn(objectName_, 1); fields.Children().Append(objectName_);
        root.Children().Append(fields);

        replaceExisting_ = Controls::CheckBox();
        replaceExisting_.Content(box_value(L"Replace an existing object with this name"));
        root.Children().Append(replaceExisting_);
        validation_ = DialogFieldLabel(L"");
        validation_.Foreground(Brush(177, 48, 45)); root.Children().Append(validation_);
        objectName_.TextChanged([this](auto const&, auto const&) { Validate(); });
        ConfigureNativeDialog(dialog_, owner_, selectedRowsOnly_
            ? L"Return Selected Rows to R" : L"Return Data to R", root, L"Return", 380);
        dialog_.PrimaryButtonClick([this](auto const&,
            Controls::ContentDialogButtonClickEventArgs const& args)
        {
            args.Cancel(true);
            SubmitNow();
        });
        Validate();
    }

    void RDataAssignmentDialog::AttachLifetime()
    {
        std::weak_ptr<RDataAssignmentDialog> weak = shared_from_this();
        dialog_.Closed([weak](auto const&, auto const&)
        {
            if (auto dialog = weak.lock())
            {
                dialog->closed_ = true;
                if (dialog->closedCallback_) dialog->closedCallback_();
            }
        });
    }

    void RDataAssignmentDialog::Validate()
    {
        const auto name = to_string(objectName_.Text());
        const bool hasName = std::any_of(name.begin(), name.end(), [](unsigned char ch)
        {
            return !std::isspace(ch);
        });
        const bool hasControl = std::any_of(name.begin(), name.end(), [](unsigned char ch)
        {
            return std::iscntrl(ch) != 0;
        });
        const bool rowsValid = !selectedRowsOnly_ || selectedRowCount_ > 0;
        dialog_.IsPrimaryButtonEnabled(hasName && !hasControl && rowsValid);
        validation_.Text(!rowsValid ? L"Select at least one observation first." :
            (!hasName ? L"Enter an R object name." :
             (hasControl ? L"The object name contains an invalid character." : L"")));
    }

    void RDataAssignmentDialog::SubmitNow()
    {
        if (!dialog_.IsPrimaryButtonEnabled() || !submit_) return;
        submit_({ dataframe_.group, to_string(objectName_.Text()),
                  selectedRowsOnly_, replaceExisting_.IsChecked().GetBoolean() });
        Close();
    }

    void RDataAssignmentDialog::Show()
    {
        if (closed_) return;
        dialog_.Opened([this](auto const&, auto const&)
        { objectName_.Focus(FocusState::Programmatic); objectName_.SelectAll(); });
        dialog_.ShowAsync();
    }

    void RDataAssignmentDialog::Close() { if (!closed_) dialog_.Hide(); }

    static ::rlispstat::core::Table1DisplayPreferences gWindowsDescriptiveTablePreferences;

    std::shared_ptr<AnalysisOutputView> AnalysisOutputView::Create()
    {
        auto view = std::shared_ptr<AnalysisOutputView>(new AnalysisOutputView()); view->AttachLifetime(); return view;
    }
    AnalysisOutputView::AnalysisOutputView() { Initialize(); }
    void AnalysisOutputView::Initialize()
    {
        window_ = CreateLinkEDAWindow(); window_.Title(L"Table 1");
        auto root = Controls::Grid(); root.Padding(Thickness{ 18, 14, 18, 10 }); root.RowSpacing(5);
        root.Background(Brush(255, 255, 255)); root.RequestedTheme(ElementTheme::Light);
        auto autoRow = []() { auto row = Controls::RowDefinition(); row.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Auto)); return row; };
        root.RowDefinitions().Append(autoRow()); root.RowDefinitions().Append(autoRow());
        auto tableRow = Controls::RowDefinition(); tableRow.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Star)); root.RowDefinitions().Append(tableRow);
        root.RowDefinitions().Append(autoRow());
        auto heading = Controls::Grid();
        auto titleColumn = Controls::ColumnDefinition(); titleColumn.Width(GridLengthHelper::FromValueAndType(1, GridUnitType::Star)); heading.ColumnDefinitions().Append(titleColumn);
        auto badgeColumn = Controls::ColumnDefinition(); badgeColumn.Width(GridLengthHelper::FromValueAndType(1, GridUnitType::Auto)); heading.ColumnDefinitions().Append(badgeColumn);
        title_ = Controls::TextBlock(); title_.FontSize(21); title_.FontWeight(Windows::UI::Text::FontWeights::SemiBold()); heading.Children().Append(title_);
        badge_ = Controls::TextBlock(); badge_.FontSize(13); badge_.FontWeight(Windows::UI::Text::FontWeights::SemiBold()); badge_.Foreground(Brush(72, 72, 72)); badge_.VerticalAlignment(VerticalAlignment::Center); Controls::Grid::SetColumn(badge_, 1); heading.Children().Append(badge_);
        root.Children().Append(heading);
        subtitle_ = Controls::TextBlock(); subtitle_.FontSize(12.5); subtitle_.Foreground(Brush(98, 98, 98)); subtitle_.Margin(Thickness{ 0, 8, 0, 7 }); Controls::Grid::SetRow(subtitle_, 1); root.Children().Append(subtitle_);
        auto scroll = Controls::ScrollViewer(); scroll.HorizontalScrollBarVisibility(Controls::ScrollBarVisibility::Auto); scroll.VerticalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);
        report_ = Controls::StackPanel(); auto report=report_; report.Spacing(8);
        table_ = Controls::Grid(); table_.Name(L"LinkEDA.SnapshotTable"); report.Children().Append(table_);
        footnotes_ = Controls::StackPanel(); footnotes_.Spacing(2); report.Children().Append(footnotes_);
        scroll.Content(report); Controls::Grid::SetRow(scroll, 2); root.Children().Append(scroll);
        auto footer=Controls::StackPanel();footer.Orientation(Controls::Orientation::Horizontal);footer.Spacing(12);
        addModelVariable_=Controls::Button();addModelVariable_.Content(box_value(L"+ Add variable"));
        addModelVariable_.Visibility(Visibility::Collapsed);footer.Children().Append(addModelVariable_);
        hint_ = Controls::TextBlock(); hint_.Text(to_hstring(::rlispstat::core::Table1StatusFieldHint())); hint_.FontSize(11); hint_.Foreground(Brush(100, 100, 100));footer.Children().Append(hint_);
        Controls::Grid::SetRow(footer,3);root.Children().Append(footer);
        auto menu = Controls::MenuFlyout(); auto copy = Controls::MenuFlyoutItem(); copy.Text(L"Copy table"); copy.KeyboardAcceleratorTextOverride(L"Ctrl+C"); copy.Click([this](auto const&, auto const&) { CopyTable(); }); menu.Items().Append(copy); AttachWindowContextFlyout(window_, menu);
        root.KeyDown([this](auto const&, Input::KeyRoutedEventArgs const& e) { if (e.Key() == Windows::System::VirtualKey::C && (GetKeyState(VK_CONTROL) & 0x8000) != 0) { CopyTable(); e.Handled(true); } });
        exporter_ = TableExportService::Create(window_, root);
        root.IsTabStop(true); window_.Content(root); ResizeLogical(window_, 860, 620);
    }

    void AnalysisOutputView::AttachLifetime()
    {
        std::weak_ptr<AnalysisOutputView> weak = shared_from_this(); window_.Closed([weak](auto const&, auto const&) { if (auto view = weak.lock()) { view->closed_ = true; if(view->embeddedDescriptives_)view->embeddedDescriptives_->Close(); if (view->closedCallback_) view->closedCallback_(); } });
    }
    void AnalysisOutputView::SetClosedCallback(Closed callback) { closedCallback_ = std::move(callback); }
    void AnalysisOutputView::SetCommandCallback(
        Command callback, std::vector<std::string> availableVariables)
    {
        commandCallback_ = std::move(callback);
        availableVariables_ = std::move(availableVariables);
        ConfigureContextMenu();
    }
    void AnalysisOutputView::Dispatch(std::string command, std::string argument)
    {
        if (!commandCallback_) return;
        commandCallback_({ std::move(command), state_.id, std::move(argument) });
    }
    void AnalysisOutputView::SelectMissingnessPattern(std::string const& pattern)
    {
        auto found=state_.missingnessPatternRows.find(pattern);if(found==state_.missingnessPatternRows.end() || !commandCallback_)return;
        std::vector<std::string> command={"SET_SELECTED",state_.missingnessSource,std::to_string(found->second.size())};
        for(int row:found->second)command.push_back(std::to_string(row));commandCallback_(command);
    }
    Controls::MenuFlyout AnalysisOutputView::CreateMissingnessMenu(std::string pattern,std::string variable)
    {
        auto menu=Controls::MenuFlyout();
        auto item=[this](std::string title,std::string action,std::string value){
            auto i=Controls::MenuFlyoutItem();i.Text(to_hstring(title));
            i.Click([this,action,value](auto const&,auto const&){if(commandCallback_)commandCallback_({"MISSING_PATTERN_ACTION",state_.missingnessSource,state_.id,action,value});});return i;
        };
        if(!pattern.empty())menu.Items().Append(item("Descriptives for "+pattern,"descriptives",pattern));
        menu.Items().Append(item("Descriptives for all patterns","descriptives","all"));
        menu.Items().Append(item("Little's MCAR test (continuous variables)","little",""));
        auto add=Controls::MenuFlyoutSubItem();add.Text(L"Add variable");
        for(const auto& v:state_.addVariableOptions)add.Items().Append(item(v,"add",v));
        menu.Items().Append(add);
        if(!variable.empty()) {auto remove=item("Remove "+variable,"remove",variable);remove.IsEnabled(state_.variables.size()>1);menu.Items().Append(remove);}
        auto save=Controls::MenuFlyoutSubItem();save.Text(L"Save to data");
        for(const auto& option:std::vector<std::pair<std::string,std::string>>{{"pattern","Missingness pattern"},{"n_missing","Number missing"},{"pct_missing","Percent missing"},{"indicators","Variable indicators"}})
            save.Items().Append(item(option.second,"save",option.first));
        menu.Items().Append(save);
        if(exporter_){auto exports=exporter_->CreateMenu([this](){return Table1ExportPayloadForState(state_);});AppendRCodeExportItems(exports,commandCallback_,state_.id);menu.Items().Append(exports);}
        return menu;
    }
    void AnalysisOutputView::ConfigureContextMenu()
    {
        if(!state_.missingnessSource.empty()){AttachWindowContextFlyout(window_,CreateMissingnessMenu());return;}
        auto menu = Controls::MenuFlyout();
        AppendMissingInformationMenu(menu,commandCallback_,state_.id,
            !state_.codeReference.provenance.missingInformationRows.empty() && state_.tableType!="mi_missing_information");
        if (state_.tableType == "missing_data_overview" || state_.tableType == "missing_data_test" || state_.tableType == "missingness_descriptives" || state_.tableType == "mi_diagnostics" || state_.tableType == "mi_missing_information") {
            if (state_.tableType == "mi_diagnostics") {
                auto root=Controls::MenuFlyoutSubItem(); root.Text(L"Imputation diagnostics");
                for (const auto& section : std::vector<std::pair<std::string,std::string>>{
                    {"summary","Missingness summary"},{"variables","Missingness by variable"},
                    {"patterns","Missing-data patterns"},{"patterns_all","Show all patterns"},
                    {"model","Imputation model"},{"events","Logged events"},
                    {"convergence","Convergence summary"}}) {
                    auto item=Controls::MenuFlyoutItem(); item.Text(to_hstring(section.second));
                    item.Click([this,section](auto const&,auto const&) {
                        if(commandCallback_)commandCallback_({"DATA_IMPUTATION_DIAGNOSTICS",state_.datasetId,section.first});
                    }); root.Items().Append(item);
                }
                for (const auto& section : std::vector<std::pair<std::string,std::string>>{
                    {"chain_mean","Chain means"},{"chain_variance","Chain variances"},{"distributions","Observed vs imputed"}}) {
                    auto variables=Controls::MenuFlyoutSubItem(); variables.Text(to_hstring(section.second));
                    for(const auto& variable:state_.variables) {
                        auto item=Controls::MenuFlyoutItem(); item.Text(to_hstring(variable));
                        item.Click([this,section,variable](auto const&,auto const&) {
                            if(commandCallback_)commandCallback_({"DATA_IMPUTATION_DIAGNOSTICS",state_.datasetId,section.first,variable});
                        }); variables.Items().Append(item);
                    } root.Items().Append(variables);
                }
                menu.Items().Append(root);
            }
            if(exporter_) {
                auto exports=exporter_->CreateMenu([this](){return Table1ExportPayloadForState(state_);});
                AppendRCodeExportItems(exports,commandCallback_,state_.id);menu.Items().Append(exports);
            } else AppendRCodeOnlyExportMenu(menu,commandCallback_,state_.id);
            AttachWindowContextFlyout(window_,menu);
            return;
        }
        if (state_.tableType == "nested_contingency" || state_.tableType == "mi_contingency")
        {
            auto rows = Controls::MenuFlyoutSubItem(); rows.Text(L"Row variables");
            auto add = Controls::MenuFlyoutSubItem(); add.Text(L"Add row variable");
            for (auto const& variable : availableVariables_)
            {
                if (variable == state_.groupVariable ||
                    std::find(state_.variables.begin(), state_.variables.end(), variable) != state_.variables.end()) continue;
                auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(variable));
                item.Click([this, variable](auto const&, auto const&) { Dispatch("TABLE_ADD_ROW", variable); });
                add.Items().Append(item);
            }
            if (add.Items().Size() > 0) rows.Items().Append(add);
            auto replace = Controls::MenuFlyoutSubItem(); replace.Text(L"Replace row variable");
            for (auto const& currentVariable : state_.variables)
            {
                auto current = Controls::MenuFlyoutSubItem(); current.Text(to_hstring(currentVariable));
                for (auto const& candidate : availableVariables_)
                {
                    if (candidate == state_.groupVariable ||
                        std::find(state_.variables.begin(), state_.variables.end(), candidate) !=
                            state_.variables.end()) continue;
                    auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(candidate));
                    item.Click([this, currentVariable, candidate](auto const&, auto const&)
                        { Dispatch("TABLE_REPLACE_ROW", currentVariable + '\x1f' + candidate); });
                    current.Items().Append(item);
                }
                if (current.Items().Size() > 0) replace.Items().Append(current);
            }
            if (replace.Items().Size() > 0) rows.Items().Append(replace);
            auto remove = Controls::MenuFlyoutSubItem(); remove.Text(L"Remove row variable");
            for (auto const& variable : state_.variables)
            {
                auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(variable));
                item.IsEnabled(state_.variables.size() > 1);
                item.Click([this, variable](auto const&, auto const&) { Dispatch("TABLE_REMOVE_ROW", variable); });
                remove.Items().Append(item);
            }
            rows.Items().Append(remove);
            menu.Items().Append(rows);

            auto split = Controls::MenuFlyoutSubItem(); split.Text(L"Column (split) variable");
            auto none = Controls::ToggleMenuFlyoutItem();
            none.Text(L"No column split");
            none.IsChecked(state_.groupVariable.empty());
            none.Click([this](auto const&, auto const&) { Dispatch("TABLE_SET_SPLIT", ""); });
            split.Items().Append(none);
            for (auto const& variable : availableVariables_)
            {
                if (std::find(state_.variables.begin(), state_.variables.end(), variable) != state_.variables.end()) continue;
                auto item = Controls::ToggleMenuFlyoutItem();
                item.Text(to_hstring(variable));
                item.IsChecked(variable == state_.groupVariable);
                item.Click([this, variable](auto const&, auto const&) { Dispatch("TABLE_SET_SPLIT", variable); });
                split.Items().Append(item);
            }
            menu.Items().Append(split);

            auto display = Controls::MenuFlyoutSubItem(); display.Text(L"Cell contents");
            const std::array<std::pair<std::wstring, std::string>, 3> modes{{
                {L"Counts and percentages", "count_percent"},
                {L"Counts only", "count"}, {L"Percentages only", "percent"} }};
            for (auto const& [title, mode] : modes)
            {
                auto item = Controls::ToggleMenuFlyoutItem();
                item.Text(title);
                item.IsChecked(mode == state_.nestedDisplayMode);
                item.Click([this, mode](auto const&, auto const&) { Dispatch("TABLE_SET_DISPLAY", mode); });
                display.Items().Append(item);
            }
            menu.Items().Append(display);
        }
        else if (state_.tableType == "table1")
        {
            menu.Items().Append(CreateGroupingMenu());
            menu.Items().Append(CreateAddVariableMenu());
            auto options = Controls::MenuFlyoutSubItem(); options.Text(L"Statistics and columns");
            auto addGroup = [this, &options](wchar_t const* title,
                std::vector<std::tuple<std::wstring, std::string, bool*>> entries)
            {
                auto group = Controls::MenuFlyoutSubItem(); group.Text(title);
                for (auto const& [itemTitle, key, flag] : entries)
                {
                    auto item = Controls::ToggleMenuFlyoutItem();
                    item.Text(itemTitle); item.IsChecked(*flag);
                    item.Click([this, key, flag](auto const&, auto const&)
                    {
                        *flag = !*flag;
                        Show(sourceState_);
                    });
                    group.Items().Append(item);
                }
                options.Items().Append(group);
            };
            addGroup(L"Location", {
                {L"Mean", "mean", &gWindowsDescriptiveTablePreferences.showMean},
                {L"Median", "median", &gWindowsDescriptiveTablePreferences.showMedian}});
            addGroup(L"Dispersion", {
                {L"SD", "sd", &gWindowsDescriptiveTablePreferences.showSD},
                {L"Q1 and Q3", "quartiles", &gWindowsDescriptiveTablePreferences.showQuartiles}});
            addGroup(L"Uncertainty", {
                {L"SE", "se", &gWindowsDescriptiveTablePreferences.showSE},
                {L"95% CI", "ci95", &gWindowsDescriptiveTablePreferences.showCI95}});
            addGroup(L"Rows and columns", {
                {L"Missing in original", "missing", &gWindowsDescriptiveTablePreferences.showMissing},
                {L"Overall", "overall", &gWindowsDescriptiveTablePreferences.showOverall},
                {L"p", "p", &gWindowsDescriptiveTablePreferences.showP},
                {L"Test", "test", &gWindowsDescriptiveTablePreferences.showTest}});
            menu.Items().Append(options);
        }
        if (menu.Items().Size() > 0) menu.Items().Append(Controls::MenuFlyoutSeparator());
        if (exporter_)
        {
            auto exportMenu = exporter_->CreateMenu([this]()
            {
                return Table1ExportPayloadForState(state_);
            });
            AppendRCodeExportItems(exportMenu, commandCallback_, state_.id);
            menu.Items().Append(exportMenu);
        }
        AttachWindowContextFlyout(window_, menu);
    }

    Controls::MenuFlyoutSubItem AnalysisOutputView::CreateGroupingMenu()
    {
        auto grouping = Controls::MenuFlyoutSubItem(); grouping.Text(L"Grouping variable");
        auto none = Controls::ToggleMenuFlyoutItem(); none.Text(L"None");
        none.IsChecked(state_.groupVariable.empty());
        none.Click([this](auto const&, auto const&) { Dispatch("TABLE1_SET_GROUP", ""); });
        grouping.Items().Append(none);
        grouping.Items().Append(Controls::MenuFlyoutSeparator());
        for (auto const& variable : availableVariables_)
        {
            auto item = Controls::ToggleMenuFlyoutItem(); item.Text(to_hstring(variable));
            item.IsChecked(variable == state_.groupVariable);
            item.Click([this, variable](auto const&, auto const&)
                { Dispatch("TABLE1_SET_GROUP", variable); });
            grouping.Items().Append(item);
        }
        return grouping;
    }

    Controls::MenuFlyoutSubItem AnalysisOutputView::CreateAddVariableMenu()
    {
        const auto titles = ::rlispstat::core::DefaultTable1ContextMenuTitles();
        auto add = Controls::MenuFlyoutSubItem(); add.Text(to_hstring(titles.addVariable));
        for (auto const& variable : availableVariables_)
        {
            if (variable == state_.groupVariable ||
                std::find(state_.variables.begin(), state_.variables.end(), variable) != state_.variables.end()) continue;
            auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(variable));
            item.Click([this, variable](auto const&, auto const&)
                { Dispatch("TABLE1_ADD_VARIABLE", variable); });
            add.Items().Append(item);
        }
        if (add.Items().Size() == 0)
        {
            auto empty = Controls::MenuFlyoutItem();
            empty.Text(to_hstring(titles.noAvailableVariables)); empty.IsEnabled(false);
            add.Items().Append(empty);
        }
        return add;
    }

    Controls::MenuFlyout AnalysisOutputView::CreateTable1AddVariableFlyout()
    {
        const auto titles = ::rlispstat::core::DefaultTable1ContextMenuTitles();
        auto menu = Controls::MenuFlyout();
        for (auto const& variable : availableVariables_)
        {
            if (variable == state_.groupVariable ||
                std::find(state_.variables.begin(), state_.variables.end(), variable) !=
                    state_.variables.end()) continue;
            auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(variable));
            item.Click([this, variable](auto const&, auto const&)
                { Dispatch("TABLE1_ADD_VARIABLE", variable); });
            menu.Items().Append(item);
        }
        if (menu.Items().Size() == 0)
        {
            auto empty = Controls::MenuFlyoutItem();
            empty.Text(to_hstring(titles.noAvailableVariables)); empty.IsEnabled(false);
            menu.Items().Append(empty);
        }
        return menu;
    }

    Controls::MenuFlyout AnalysisOutputView::CreateTable1ReplacementMenu(
        std::string const& variable)
    {
        const auto titles = ::rlispstat::core::DefaultTable1ContextMenuTitles();
        auto menu = Controls::MenuFlyout();
        for (auto const& candidate : availableVariables_)
        {
            if (candidate == variable || candidate == state_.groupVariable ||
                std::find(state_.variables.begin(), state_.variables.end(), candidate) !=
                    state_.variables.end()) continue;
            auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(candidate));
            item.Click([this, variable, candidate](auto const&, auto const&)
                { Dispatch("TABLE1_REPLACE_VARIABLE", variable + '\x1f' + candidate); });
            menu.Items().Append(item);
        }
        if (menu.Items().Size() == 0)
        {
            auto empty = Controls::MenuFlyoutItem();
            empty.Text(to_hstring(titles.noAvailableVariables)); empty.IsEnabled(false);
            menu.Items().Append(empty);
        }
        return menu;
    }

    std::string AnalysisOutputView::VariableAnalysisType(std::string const& variable) const
    {
        return ::rlispstat::core::Table1VariableAnalysisType(state_, variable);
    }

    Controls::MenuFlyout AnalysisOutputView::CreateTable1VariableMenu(
        std::string const& variable)
    {
        const auto titles = ::rlispstat::core::DefaultTable1ContextMenuTitles();
        auto menu = Controls::MenuFlyout();
        menu.Items().Append(CreateGroupingMenu());
        menu.Items().Append(Controls::MenuFlyoutSeparator());

        menu.Items().Append(CreateAddVariableMenu());
        auto change = Controls::MenuFlyoutSubItem();
        change.Text(to_hstring(::rlispstat::core::Table1ReplaceVariableTitle(variable)));
        for (auto const& candidate : availableVariables_)
        {
            if (candidate == variable || candidate == state_.groupVariable ||
                std::find(state_.variables.begin(), state_.variables.end(), candidate) != state_.variables.end()) continue;
            auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(candidate));
            item.Click([this, variable, candidate](auto const&, auto const&)
                { Dispatch("TABLE1_REPLACE_VARIABLE", variable + '\x1f' + candidate); });
            change.Items().Append(item);
        }
        if (change.Items().Size() == 0)
        {
            auto empty = Controls::MenuFlyoutItem(); empty.Text(to_hstring(titles.noAvailableVariables));
            empty.IsEnabled(false); change.Items().Append(empty);
        }
        menu.Items().Append(change);
        auto remove = Controls::MenuFlyoutItem();
        remove.Text(to_hstring(::rlispstat::core::Table1RemoveVariableTitle(variable)));
        remove.IsEnabled(state_.variables.size() > 1);
        remove.Click([this, variable](auto const&, auto const&)
            { Dispatch("TABLE1_REMOVE_VARIABLE", variable); });
        menu.Items().Append(remove);

        auto types = Controls::MenuFlyoutSubItem(); types.Text(L"Type of variable");
        const std::string currentType = VariableAnalysisType(variable);
        for (auto const* type : {"numeric", "categorical", "ordinal"})
        {
            auto item = Controls::ToggleMenuFlyoutItem();
            item.Text(to_hstring(::rlispstat::core::Table1TypeMenuTitle(type)));
            item.IsChecked(currentType == type);
            item.Click([this, variable, type = std::string(type)](auto const&, auto const&)
                { Dispatch("TABLE1_SET_TYPE", variable + '\x1f' + type); });
            types.Items().Append(item);
        }
        menu.Items().Append(types);
        auto info = Controls::MenuFlyoutItem();
        info.Text(to_hstring(titles.showVariableInformation)); info.IsEnabled(false);
        menu.Items().Append(info);

        menu.Items().Append(Controls::MenuFlyoutSeparator());
        for (auto const& option : ::rlispstat::core::Table1PlotMenuOptions(
                 variable, currentType, state_.groupVariable))
        {
            auto plot = Controls::MenuFlyoutItem();
            plot.Text(to_hstring(option.title));
            plot.Click([this, variable, command = option.command](auto const&, auto const&)
                { Dispatch(command, variable); });
            menu.Items().Append(plot);
        }

        menu.Items().Append(Controls::MenuFlyoutSeparator());
        if (exporter_)
        {
            auto exportMenu = exporter_->CreateMenu([this]()
            {
                return Table1ExportPayloadForState(state_);
            });
            AppendRCodeExportItems(exportMenu, commandCallback_, state_.id);
            menu.Items().Append(exportMenu);
        }
        return menu;
    }

    Controls::MenuFlyout AnalysisOutputView::CreateNestedRowVariableMenu(
        std::string const& variable)
    {
        auto menu = Controls::MenuFlyout();
        auto heading = Controls::MenuFlyoutItem();
        heading.Text(to_hstring(variable)); heading.IsEnabled(false);
        menu.Items().Append(heading);

        auto replace = Controls::MenuFlyoutSubItem(); replace.Text(L"Replace with");
        for (auto const& candidate : availableVariables_)
        {
            if (candidate == state_.groupVariable ||
                std::find(state_.variables.begin(), state_.variables.end(), candidate) !=
                    state_.variables.end()) continue;
            auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(candidate));
            item.Click([this, variable, candidate](auto const&, auto const&)
                { Dispatch("TABLE_REPLACE_ROW", variable + '\x1f' + candidate); });
            replace.Items().Append(item);
        }
        if (replace.Items().Size() == 0)
        {
            auto empty = Controls::MenuFlyoutItem();
            empty.Text(L"No replacement variables"); empty.IsEnabled(false);
            replace.Items().Append(empty);
        }
        menu.Items().Append(replace);

        auto add = Controls::MenuFlyoutSubItem(); add.Text(L"Add row variable");
        for (auto const& candidate : availableVariables_)
        {
            if (candidate == state_.groupVariable ||
                std::find(state_.variables.begin(), state_.variables.end(), candidate) !=
                    state_.variables.end()) continue;
            auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(candidate));
            item.Click([this, candidate](auto const&, auto const&)
                { Dispatch("TABLE_ADD_ROW", candidate); });
            add.Items().Append(item);
        }
        if (add.Items().Size() > 0) menu.Items().Append(add);

        auto remove = Controls::MenuFlyoutItem();
        remove.Text(to_hstring("Remove " + variable));
        remove.IsEnabled(state_.variables.size() > 1);
        remove.Click([this, variable](auto const&, auto const&)
            { Dispatch("TABLE_REMOVE_ROW", variable); });
        menu.Items().Append(remove);
        AppendRCodeOnlyExportMenu(menu, commandCallback_, state_.id);
        return menu;
    }

    Controls::MenuFlyout AnalysisOutputView::CreateNestedSplitVariableMenu()
    {
        auto menu = Controls::MenuFlyout();
        auto heading = Controls::MenuFlyoutItem();
        heading.Text(to_hstring(state_.groupVariable.empty()
            ? "Column split" : state_.groupVariable));
        heading.IsEnabled(false); menu.Items().Append(heading);

        auto replace = Controls::MenuFlyoutSubItem();
        replace.Text(state_.groupVariable.empty() ? L"Set column variable" : L"Replace with");
        for (auto const& candidate : availableVariables_)
        {
            if (candidate == state_.groupVariable ||
                std::find(state_.variables.begin(), state_.variables.end(), candidate) !=
                    state_.variables.end()) continue;
            auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(candidate));
            item.Click([this, candidate](auto const&, auto const&)
                { Dispatch("TABLE_SET_SPLIT", candidate); });
            replace.Items().Append(item);
        }
        if (replace.Items().Size() == 0)
        {
            auto empty = Controls::MenuFlyoutItem();
            empty.Text(L"No available variables"); empty.IsEnabled(false);
            replace.Items().Append(empty);
        }
        menu.Items().Append(replace);
        if (!state_.groupVariable.empty())
        {
            auto remove = Controls::MenuFlyoutItem(); remove.Text(L"Remove column split");
            remove.Click([this](auto const&, auto const&)
                { Dispatch("TABLE_SET_SPLIT", ""); });
            menu.Items().Append(remove);
        }
        AppendRCodeOnlyExportMenu(menu, commandCallback_, state_.id);
        return menu;
    }
    void AnalysisOutputView::Show(::rlispstat::core::Table1DisplayState const& state)
    {
        ::rlispstat::windows::performance::Scope timing("Output.Table1.Show");
        sourceState_ = state;
        state_ = ::rlispstat::core::Table1ApplyDisplayPreferences(
            sourceState_, gWindowsDescriptiveTablePreferences);
        SetWindowDataSheetGroup(window_, state_.datasetId);
        SetWindowSnapshotSource(window_, "output", state_.id, state_.datasetId);
        window_.Title(state_.tableType == "table1" ? L"Table 1" : to_hstring(state_.title));
        title_.Text(to_hstring(state_.title));
        badge_.Text(to_hstring(state_.datasetId));
        badge_.Visibility(state_.tableType == "missing_data_overview" || state_.tableType == "missing_data_test" || state_.tableType == "missingness_descriptives" || state_.tableType == "mi_diagnostics" || state_.tableType == "mi_missing_information"
            ? Visibility::Collapsed : Visibility::Visible);
        subtitle_.Text(to_hstring(::rlispstat::core::Table1Subtitle(state_, true)));
        subtitle_.TextWrapping(TextWrapping::Wrap);
        const bool ordinaryTable1 = state_.tableType == "table1";
        const bool showAddVariable = ordinaryTable1 ||
            !state_.linkedModelOutputId.empty() || !state_.missingnessSource.empty();
        addModelVariable_.Visibility(showAddVariable ? Visibility::Visible : Visibility::Collapsed);
        auto variableMenu=Controls::MenuFlyout();
        bool hasAddVariable = false;
        if (ordinaryTable1) {
            variableMenu = CreateTable1AddVariableFlyout();
            hasAddVariable = std::any_of(availableVariables_.begin(), availableVariables_.end(),
                [this](std::string const& variable) {
                    return variable != state_.groupVariable &&
                        std::find(state_.variables.begin(), state_.variables.end(), variable) ==
                            state_.variables.end();
                });
        } else {
            hasAddVariable = !state_.addVariableOptions.empty();
            for(const auto& variable:state_.addVariableOptions) {
                auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(variable));
                item.Click([this,variable](auto const&,auto const&) {
                    if(commandCallback_) {
                        if(!state_.missingnessSource.empty())commandCallback_({"MISSING_PATTERN_ACTION",state_.missingnessSource,state_.id,"add",variable});
                        else commandCallback_({"MI_ADD_VARIABLE",state_.linkedModelOutputId,variable});
                    }
                });variableMenu.Items().Append(item);
            }
        }
        addModelVariable_.IsEnabled(hasAddVariable);
        addModelVariable_.Flyout(variableMenu);
        hint_.Text(state_.tableType == "nested_contingency"
            ? L"Counts and row percentages. Use the linked plot or data sheet to inspect cases."
            : to_hstring(::rlispstat::core::Table1StatusFieldHint()));
        BuildTable(state_);
        ConfigureContextMenu();
        const double naturalWidth = ::rlispstat::core::Table1NaturalWidth(state_);
        const int width = static_cast<int>(std::clamp(
            naturalWidth + 48.0,
            760.0, state_.tableType == "mi_missing_information" ? 1740.0 : 1280.0));
        const int height = static_cast<int>(std::clamp(
            ::rlispstat::core::Table1PreferredHeight(state_) + 130.0, 330.0, 680.0));
        // AppWindow sizes are physical pixels while the report layout copied
        // from macOS is expressed in logical units.  Preserve the 760-point
        // report width at every Windows display scale so the result column is
        // not pushed beyond the viewport on a high-DPI monitor.
        PresentOrResizeAnalysisWindow(window_, presented_, width, height);
    }
    void AnalysisOutputView::BuildTable(::rlispstat::core::Table1DisplayState const& state)
    {
        table_.Children().Clear();
        table_.ColumnDefinitions().Clear();
        table_.RowDefinitions().Clear();
        footnotes_.Children().Clear();
        const double reportWidth = ::rlispstat::core::Table1NaturalWidth(state);
        const auto layout = ::rlispstat::core::BuildTable1ReportLayout(state, reportWidth);
        table_.MinWidth(layout.width);
        const int stubCount = layout.hasStubs ? layout.stubColumnCount : 1;
        for (int index = 0; index < stubCount; ++index)
        {
            auto stub = Controls::ColumnDefinition();
            stub.Width(GridLengthHelper::FromPixels(
                layout.hasStubs ? layout.stubWidth : layout.stubArea));
            table_.ColumnDefinitions().Append(stub);
        }
        for (std::size_t i = 0; i < state.columns.size(); ++i) { auto column = Controls::ColumnDefinition(); column.Width(GridLengthHelper::FromPixels(i < layout.valueColumnWidths.size() ? layout.valueColumnWidths[i] : layout.columnWidth)); table_.ColumnDefinitions().Append(column); }
        if (state.showP) { auto column = Controls::ColumnDefinition(); column.Width(GridLengthHelper::FromPixels(layout.pWidth)); table_.ColumnDefinitions().Append(column); }
        if (state.showTest) { auto column = Controls::ColumnDefinition(); column.Width(GridLengthHelper::FromPixels(layout.testWidth)); table_.ColumnDefinitions().Append(column); }
        auto appendAutoRow = [this]() { auto row = Controls::RowDefinition(); row.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Auto)); table_.RowDefinitions().Append(row); };
        const int spanningRow = layout.hasSpanningHeader ? 0 : -1;
        const int headerRow = layout.hasSpanningHeader ? 1 : 0;
        if (layout.hasSpanningHeader) appendAutoRow();
        appendAutoRow();
        auto add = [this](FrameworkElement const& cell, int row, int column) { Controls::Grid::SetRow(cell, row); Controls::Grid::SetColumn(cell, column); table_.Children().Append(cell); };
        if (layout.hasSpanningHeader)
        {
            auto spanning = TableCell(to_hstring(state.groupSpanningHeader).c_str(),
                                      false, false, true);
            if (auto text = spanning.Child().try_as<Controls::TextBlock>())
                text.HorizontalAlignment(HorizontalAlignment::Center);
            add(spanning, spanningRow, stubCount);
            Controls::Grid::SetColumnSpan(spanning,
                std::max(1, state.groupSpanningColumnCount));
            if (state.tableType == "nested_contingency" || state.tableType == "mi_contingency")
                AttachDirectMenu(spanning, CreateNestedSplitVariableMenu());
            else if (state.tableType == "table1" && !state.groupVariable.empty())
            {
                auto groupMenu = Controls::MenuFlyout();
                groupMenu.Items().Append(CreateGroupingMenu());
                AttachDirectMenu(spanning, groupMenu);
            }
            else AttachDirectMenu(spanning, CreateStructuralCellMenu(
                to_hstring(state.groupSpanningHeader).c_str(),
                spanning.Child().as<Controls::TextBlock>()));
        }
        int columnIndex = 0;
        if (layout.hasStubs)
        {
            for (int index = 0; index < stubCount; ++index)
            {
                const std::string header = static_cast<std::size_t>(index) < state.stubHeaders.size()
                    ? state.stubHeaders[static_cast<std::size_t>(index)] : std::string{};
                auto headerCell = TableCell(to_hstring(header).c_str(), true, true);
                if ((state.tableType == "nested_contingency" || state.tableType == "mi_contingency") && !header.empty())
                    AttachDirectMenu(headerCell, CreateNestedRowVariableMenu(header));
                else AttachDirectMenu(headerCell, CreateStructuralCellMenu(
                    to_hstring(header).c_str(),
                    headerCell.Child().as<Controls::TextBlock>()));
                add(headerCell, headerRow, columnIndex++);
            }
        }
        else
        {
            auto cell = TableCell(L"Variable", true, true);
            AttachDirectMenu(cell, CreateStructuralCellMenu(
                L"Variable", cell.Child().as<Controls::TextBlock>()));
            add(cell, headerRow, columnIndex++);
        }
        const bool columnsAreStatistics = state.tableType != "table1" &&
            state.tableType != "nested_contingency";
        for (auto const& column : state.columns)
        {
            auto cell = TableCell(to_hstring(column).c_str(), true, false);
            if(!state.missingnessSource.empty()) {
                cell.ContextFlyout(CreateMissingnessMenu(column));
                cell.Tapped([this,column](auto const&,auto const& event){SelectMissingnessPattern(column);event.Handled(true);});
                cell.Child().as<Controls::TextBlock>().HorizontalAlignment(HorizontalAlignment::Center);
            } else AttachDirectMenu(cell, columnsAreStatistics
                ? CreateStatisticCellMenu(to_hstring(column).c_str(),
                    cell.Child().as<Controls::TextBlock>(),
                    AnalysisStatisticContext(column, state.tableType, state.subtitle))
                : CreateStructuralCellMenu(
                    to_hstring(column).c_str(), cell.Child().as<Controls::TextBlock>()));
            add(cell, headerRow, columnIndex++);
        }
        if (state.showP)
        {
            auto cell = TableCell(L"p", true, false);
            AttachDirectMenu(cell, CreateStatisticCellMenu(
                L"p", cell.Child().as<Controls::TextBlock>(),
                AnalysisStatisticContext("p", state.tableType, state.subtitle)));
            add(cell, headerRow, columnIndex++);
        }
        if (state.showTest)
        {
            auto cell = TableCell(L"Test", true, true);
            AttachDirectMenu(cell, CreateStatisticCellMenu(
                L"Test", cell.Child().as<Controls::TextBlock>(),
                AnalysisStatisticContext("Test", state.tableType, state.subtitle)));
            add(cell, headerRow, columnIndex++);
        }
        int rowIndex = headerRow + 1;
        for (std::size_t rowNumber = 0; rowNumber < state.rows.size(); ++rowNumber)
        {
            auto const& row = state.rows[rowNumber];
            appendAutoRow();
            const bool parent = ::rlispstat::core::Table1RowIsParent(row);
            const bool indented = ::rlispstat::core::Table1RowIsIndented(row);
            const bool muted = row.rowType == "missing" || ::rlispstat::core::Table1RowIsSubVariable(row);
            const bool nestedSeparator = layout.hasStubs && rowNumber > 0 &&
                !row.stubValues.empty() && !row.stubValues.front().empty();
            const bool separator = nestedSeparator || (parent && rowNumber > 0);
            auto addDataCell = [this, &add, &state, &row](Controls::Border const& cell,
                                                          int targetRow,
                                                          int targetColumn,
                                                          bool variableCell = false,
                                                          std::wstring const& menuTitle = {},
                                                          bool statisticCell = false)
            {
                if(!state.missingnessSource.empty()) {
                    if(variableCell && !row.variable.empty()) AttachDirectMenu(cell,CreateMissingnessMenu({},row.variable));
                    else if(!variableCell && !menuTitle.empty()) {
                        const auto pattern=to_string(hstring(menuTitle));
                        cell.ContextFlyout(CreateMissingnessMenu(pattern));
                        cell.Tapped([this,pattern](auto const&,auto const& event){SelectMissingnessPattern(pattern);event.Handled(true);});
                        cell.Child().as<Controls::TextBlock>().HorizontalAlignment(HorizontalAlignment::Center);
                    }
                } else if (variableCell && state.tableType == "table1" && !row.variable.empty()) {
                    auto context = CreateTable1VariableMenu(row.variable);
                    if (::rlispstat::core::Table1DisplayLabelText(row) == row.variable)
                        AttachPrimaryAndContextMenus(cell,
                            CreateTable1ReplacementMenu(row.variable), context);
                    else AttachContextOnlyMenu(cell, context);
                }
                else if (!menuTitle.empty())
                {
                    auto text = cell.Child().as<Controls::TextBlock>();
                    AttachDirectMenu(cell, statisticCell
                        ? CreateStatisticCellMenu(menuTitle, text,
                            AnalysisStatisticContext(to_string(hstring(menuTitle)),
                                state.tableType, state.subtitle))
                        : CreateStructuralCellMenu(menuTitle, text));
                }
                add(cell, targetRow, targetColumn);
            };
            columnIndex = 0;
            if (layout.hasStubs)
            {
                for (int index = 0; index < stubCount; ++index)
                {
                    const std::string value = static_cast<std::size_t>(index) < row.stubValues.size()
                        ? row.stubValues[static_cast<std::size_t>(index)] : std::string{};
                    addDataCell(TableCell(to_hstring(value).c_str(), false, true,
                                          row.rowType == "nested_total" || index == 0,
                                          false, false, separator), rowIndex, columnIndex++,
                                index == 0, to_hstring(value).c_str(), false);
                }
            }
            else
            {
                const auto displayLabel = ::rlispstat::core::Table1DisplayLabelText(row);
                addDataCell(TableCell(to_hstring(displayLabel).c_str(),
                                      false, true, parent, muted, indented, separator),
                            rowIndex, columnIndex++, true,
                            to_hstring(displayLabel).c_str(), false);
            }
            std::string rowStatistic = "Value";
            if (row.rowType == "n") rowStatistic = "N";
            else if (row.rowType == "numeric_mean_sd") rowStatistic = "Mean and SD";
            else if (row.rowType == "numeric_median_iqr" ||
                     row.rowType == "ordinal_median_iqr") rowStatistic = "Median and IQR";
            else if (row.rowType.find("level") != std::string::npos ||
                     row.rowType == "nested_cell") rowStatistic = "Observed count and Percent";
            for (std::size_t i = 0; i < state.columns.size(); ++i)
            {
                const std::string statistic = columnsAreStatistics
                    ? state.columns[i] : rowStatistic;
                addDataCell(TableCell(i < row.values.size() ? to_hstring(row.values[i]).c_str() : L"",
                                      false, false, row.rowType == "nested_total",
                                      row.rowType == "missing", false, separator),
                            rowIndex, columnIndex++, false,
                            to_hstring(statistic).c_str(), true);
            }
            const bool comparisonRow = parent || row.rowType == "numeric_mean_sd";
            if (state.showP) addDataCell(TableCell(to_hstring(row.p.empty() && comparisonRow ? "\u2014" : row.p).c_str(), false, false, false, false, false, separator), rowIndex, columnIndex++, false, L"p", true);
            if (state.showTest) addDataCell(TableCell(to_hstring(row.test.empty() && comparisonRow ? "\u2014" : row.test).c_str(), false, true, false, false, false, separator), rowIndex, columnIndex++, false, L"Test", true);
            ++rowIndex;
        }
        for (auto const& note : state.footnotes) { auto text = FieldLabel(to_hstring(note).c_str()); text.FontSize(11); text.Foreground(Brush(100, 100, 100)); text.TextWrapping(TextWrapping::Wrap); footnotes_.Children().Append(text); }
        for (auto const& warning : state.warnings) { auto text = FieldLabel(to_hstring("Warning: " + warning).c_str()); text.FontSize(11); text.Foreground(Brush(140, 46, 36)); text.TextWrapping(TextWrapping::Wrap); footnotes_.Children().Append(text); }
    }
    void AnalysisOutputView::CopyTable()
    {
        Windows::ApplicationModel::DataTransfer::DataPackage package; package.SetText(to_hstring(::rlispstat::core::Table1PlainText(state_))); Windows::ApplicationModel::DataTransfer::Clipboard::SetContent(package); Windows::ApplicationModel::DataTransfer::Clipboard::Flush();
    }
    void AnalysisOutputView::EmbedDescriptives(std::shared_ptr<AnalysisOutputView> const& child)
    {
        if(!child || child.get()==this || embeddedDescriptives_)return;
        embeddedDescriptives_=child;
        if(auto scroll=child->report_.Parent().try_as<Controls::ScrollViewer>())scroll.Content(nullptr);
        auto heading=Controls::TextBlock();heading.Text(L"Descriptives by missingness pattern");heading.FontWeight(Windows::UI::Text::FontWeights::SemiBold());heading.Margin(Thickness{0,20,0,6});
        report_.Children().Append(heading);report_.Children().Append(child->report_);
        child->window_.AppWindow().Hide();
        auto menu=Controls::MenuFlyout();
        if(child->exporter_)menu.Items().Append(child->exporter_->CreateMenu([child](){return Table1ExportPayloadForState(child->state_);}));
        AppendRCodeExportItems(menu,child->commandCallback_,child->state_.id);
        child->report_.ContextFlyout(menu);
        Activate();
    }
    void AnalysisOutputView::Activate() { if (!closed_) window_.Activate(); }
    void AnalysisOutputView::Close() { if(embeddedDescriptives_)embeddedDescriptives_->Close(); if (!closed_) window_.Close(); }

    std::shared_ptr<GLMInteractionReportView> GLMInteractionReportView::Create()
    {
        auto view = std::shared_ptr<GLMInteractionReportView>(new GLMInteractionReportView());
        view->AttachLifetime();
        return view;
    }

    GLMInteractionReportView::GLMInteractionReportView() { Initialize(); }

    void GLMInteractionReportView::Initialize()
    {
        window_ = CreateLinkEDAWindow();
        window_.Title(L"Interaction interpretation");
        auto root = Controls::Grid();
        root.Padding(Thickness{ 18, 14, 18, 12 });
        root.RowSpacing(5);
        root.Background(Brush(255, 255, 255));
        root.RequestedTheme(ElementTheme::Light);
        auto autoRow = []()
        {
            auto row = Controls::RowDefinition();
            row.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Auto));
            return row;
        };
        root.RowDefinitions().Append(autoRow());
        root.RowDefinitions().Append(autoRow());
        root.RowDefinitions().Append(autoRow());
        root.RowDefinitions().Append(autoRow());
        auto bodyRow = Controls::RowDefinition();
        bodyRow.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
        root.RowDefinitions().Append(bodyRow);

        auto heading = Controls::Grid();
        auto titleColumn = Controls::ColumnDefinition();
        titleColumn.Width(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
        heading.ColumnDefinitions().Append(titleColumn);
        heading.ColumnDefinitions().Append(Controls::ColumnDefinition());
        title_ = Controls::TextBlock();
        title_.FontSize(21);
        title_.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        heading.Children().Append(title_);
        badge_ = Controls::TextBlock();
        badge_.FontSize(13);
        badge_.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        badge_.Foreground(Brush(72, 72, 72));
        badge_.VerticalAlignment(VerticalAlignment::Center);
        Controls::Grid::SetColumn(badge_, 1);
        heading.Children().Append(badge_);
        root.Children().Append(heading);

        subtitle_ = Controls::TextBlock();
        subtitle_.FontSize(12.5);
        subtitle_.Foreground(Brush(84, 84, 84));
        subtitle_.Margin(Thickness{ 0, 6, 0, 7 });
        subtitle_.TextWrapping(TextWrapping::Wrap);
        Controls::Grid::SetRow(subtitle_, 1);
        root.Children().Append(subtitle_);

        referenceControls_ = Controls::StackPanel();
        referenceControls_.Orientation(Controls::Orientation::Horizontal);
        referenceControls_.Spacing(9);
        referenceControls_.Margin(Thickness{ 0, 2, 0, 7 });
        auto referenceLabel = FieldLabel(L"Test estimated means against:");
        referenceLabel.VerticalAlignment(VerticalAlignment::Center);
        referenceControls_.Children().Append(referenceLabel);
        emmReference_ = Controls::ComboBox();
        emmReference_.MinWidth(170);
        for (auto const& label : { L"None", L"0", L"Custom\u2026" }) {
            auto item = Controls::ComboBoxItem();
            item.Content(box_value(label));
            emmReference_.Items().Append(item);
        }
        emmReference_.SelectedIndex(0);
        emmReference_.SelectionChanged([this](auto const&, auto const&) {
            if (updatingReference_ || !commandCallback_ || id_.empty()) return;
            const int selected = emmReference_.SelectedIndex();
            if (selected == 0 || selected == 1) {
                commandCallback_({ "REGRESSION_INTERACTION_SET_REFERENCE", id_,
                    selected == 0 ? "none" : "0" });
                return;
            }
            if (selected != 2) return;
            auto input = Controls::TextBox();
            input.PlaceholderText(L"Finite numeric value");
            if (report_.emmReferenceTestEnabled &&
                std::isfinite(report_.emmTestReference))
                input.Text(to_hstring(FormatDouble(report_.emmTestReference, 8)));
            auto dialog = Controls::ContentDialog();
            ConfigureNativeDialog(dialog, window_, L"Reference value", input,
                                  L"Use reference", 380);
            auto operation = dialog.ShowAsync();
            operation.Completed([this, input](auto const& completed,
                                               Windows::Foundation::AsyncStatus status) {
                if (status == Windows::Foundation::AsyncStatus::Completed &&
                    completed.GetResults() == Controls::ContentDialogResult::Primary) {
                    const std::string text = to_string(input.Text());
                    char *end = nullptr;
                    const double value = std::strtod(text.c_str(), &end);
                    while (end && *end && std::isspace((unsigned char)*end)) ++end;
                    if (end != text.c_str() && end && *end == '\0' &&
                        std::isfinite(value)) {
                        commandCallback_({ "REGRESSION_INTERACTION_SET_REFERENCE",
                            id_, FormatDouble(value, 15) });
                        return;
                    }
                }
                updatingReference_ = true;
                emmReference_.SelectedIndex(report_.emmReferenceTestEnabled ?
                    (std::fabs(report_.emmTestReference) < 1e-15 ? 1 : 2) : 0);
                updatingReference_ = false;
            });
        });
        referenceControls_.Children().Append(emmReference_);
        Controls::Grid::SetRow(referenceControls_, 2);
        root.Children().Append(referenceControls_);

        binaryControls_ = Controls::StackPanel();
        binaryControls_.Orientation(Controls::Orientation::Horizontal);
        binaryControls_.Spacing(8);
        binaryControls_.Margin(Thickness{ 0, 2, 0, 7 });
        binaryControls_.Visibility(Visibility::Collapsed);
        auto addChoice = [&](wchar_t const* label,
                             std::initializer_list<wchar_t const*> choices,
                             Controls::ComboBox& selector)
        {
            auto field = FieldLabel(label);
            field.VerticalAlignment(VerticalAlignment::Center);
            binaryControls_.Children().Append(field);
            selector = Controls::ComboBox();
            selector.MinWidth(choices.size() > 2 ? 90 : 155);
            for (auto choice : choices)
            {
                auto item = Controls::ComboBoxItem();
                item.Content(box_value(choice));
                selector.Items().Append(item);
            }
            binaryControls_.Children().Append(selector);
        };
        addChoice(L"Quantity:", { L"Predicted probability", L"Probability difference" },
                  effectQuantity_);
        addChoice(L"Adjustment:", { L"Average over analysis sample", L"Reference profile" },
                  effectAdjustment_);
        addChoice(L"Display:", { L"Percentage", L"0–1" }, effectPresentation_);
        addChoice(L"CI:", { L"90%", L"95%", L"99%" }, effectConfidence_);
        auto binaryChanged = [this](std::string const& option,
                                    Controls::ComboBox const& selector)
        {
            if (updatingBinaryOptions_ || !commandCallback_ || id_.empty()) return;
            const int selected = selector.SelectedIndex();
            std::string value;
            if (option == "quantity") value = selected == 1
                ? "probability_difference" : "predicted_probability";
            else if (option == "adjustment") value = selected == 1
                ? "reference_profile" : "average_sample";
            else if (option == "presentation") value = selected == 1
                ? "probability" : "percentage";
            else value = selected == 0 ? "0.90" : (selected == 2 ? "0.99" : "0.95");
            commandCallback_({ "REGRESSION_INTERACTION_SET_BINARY_OPTION", id_,
                               option, value });
        };
        effectQuantity_.SelectionChanged([this, binaryChanged](auto const&, auto const&) {
            binaryChanged("quantity", effectQuantity_);
        });
        effectAdjustment_.SelectionChanged([this, binaryChanged](auto const&, auto const&) {
            binaryChanged("adjustment", effectAdjustment_);
        });
        effectPresentation_.SelectionChanged([this, binaryChanged](auto const&, auto const&) {
            binaryChanged("presentation", effectPresentation_);
        });
        effectConfidence_.SelectionChanged([this, binaryChanged](auto const&, auto const&) {
            binaryChanged("confidence", effectConfidence_);
        });
        Controls::Grid::SetRow(binaryControls_, 2);
        root.Children().Append(binaryControls_);

        groupingControls_ = Controls::StackPanel();
        groupingControls_.Orientation(Controls::Orientation::Horizontal);
        groupingControls_.Spacing(9);
        comparisonLabel_ = FieldLabel(L"Compare:");
        groupingControls_.Children().Append(comparisonLabel_);
        comparison_ = Controls::ComboBox();
        comparison_.MinWidth(190);
        groupingControls_.Children().Append(comparison_);
        groupingControls_.Children().Append(FieldLabel(L"Group by:"));
        grouping_ = Controls::ComboBox();
        grouping_.MinWidth(190);
        auto changeGrouping = [this](bool changedGroup) {
            if (updatingReference_ || !commandCallback_) return;
            const auto factors = ::rlispstat::core::GLMInteractionGroupingFactors(report_);
            const int groupIndex = grouping_.SelectedIndex();
            const int compareIndex = factors.size() == 2
                ? 1 - groupIndex : comparison_.SelectedIndex();
            if (factors.size() < 2 || groupIndex < 0 || compareIndex < 0 ||
                groupIndex >= static_cast<int>(factors.size()) ||
                compareIndex >= static_cast<int>(factors.size())) return;
            int focal = compareIndex;
            int group = groupIndex;
            if (focal == group) {
                if (factors.size() != 3) return;
                if (changedGroup) focal = 1; else group = 0;
                updatingReference_ = true;
                comparison_.SelectedIndex(focal);
                grouping_.SelectedIndex(group);
                updatingReference_ = false;
            }
            if (factors.size() == 3) {
                for (int index = 0; index < 3; ++index) {
                    if (index != focal && index != group) {
                        stratumLabel_.Text(to_hstring("Within: " + factors[index]));
                        break;
                    }
                }
            }
            commandCallback_({ "REGRESSION_INTERACTION_SET_FACTOR_ORDER", id_,
                               factors[focal], factors[group] });
        };
        comparison_.SelectionChanged([changeGrouping](auto const&, auto const&) {
            changeGrouping(false);
        });
        grouping_.SelectionChanged([changeGrouping](auto const&, auto const&) {
            changeGrouping(true);
        });
        groupingControls_.Children().Append(grouping_);
        stratumLabel_ = FieldLabel(L"");
        groupingControls_.Children().Append(stratumLabel_);
        Controls::Grid::SetRow(groupingControls_, 3);
        root.Children().Append(groupingControls_);

        auto scroll = Controls::ScrollViewer();
        scroll.HorizontalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);
        scroll.VerticalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);
        content_ = Controls::StackPanel();
        content_.Name(L"LinkEDA.SnapshotTable");
        content_.Spacing(12);
        scroll.Content(content_);
        Controls::Grid::SetRow(scroll, 4);
        root.Children().Append(scroll);

        auto menu = Controls::MenuFlyout();
        auto copy = Controls::MenuFlyoutItem();
        copy.Text(L"Copy report");
        copy.KeyboardAcceleratorTextOverride(L"Ctrl+C");
        copy.Click([this](auto const&, auto const&) { CopyReport(); });
        menu.Items().Append(copy);
        AttachWindowContextFlyout(window_, menu);
        root.KeyDown([this](auto const&, Input::KeyRoutedEventArgs const& event)
        {
            if (event.Key() == Windows::System::VirtualKey::C &&
                (GetKeyState(VK_CONTROL) & 0x8000) != 0)
            {
                CopyReport();
                event.Handled(true);
            }
        });
        exporter_ = TableExportService::Create(window_, root);
        root.IsTabStop(true);
        window_.Content(root);
        ResizeLogical(window_, 1220, 720);
    }

    void GLMInteractionReportView::AttachLifetime()
    {
        std::weak_ptr<GLMInteractionReportView> weak = shared_from_this();
        window_.Closed([weak](auto const&, auto const&)
        {
            if (auto view = weak.lock())
            {
                view->closed_ = true;
                if (view->closedCallback_) view->closedCallback_();
            }
        });
    }

    void GLMInteractionReportView::SetClosedCallback(Closed callback)
    {
        closedCallback_ = std::move(callback);
    }

    void GLMInteractionReportView::SetCommandCallback(Command callback)
    {
        commandCallback_ = std::move(callback);
    }

    void GLMInteractionReportView::SetPendingMessage(std::string const& message)
    {
        report_.ok = false;
        report_.message = message;
        Render();
    }

    void GLMInteractionReportView::Show(
        std::string const& id,
        std::string const& datasetId,
        std::string const& title,
        std::string const& subtitle,
        ::rlispstat::core::GLMInteractionReport const& report)
    {
        id_ = id;
        datasetId_ = datasetId;
        titleText_ = title;
        subtitleText_ = subtitle;
        report_ = report;
        SetWindowDataSheetGroup(window_, datasetId_);
        SetWindowSnapshotSource(window_, "output", id_, datasetId_);
        auto menu = Controls::MenuFlyout();
        if (exporter_) {
            auto exportMenu = exporter_->CreateMenu([this]() {
                return InteractionReportExportPayload(
                    id_, titleText_, subtitleText_, report_);
            });
            AppendRCodeExportItems(exportMenu, commandCallback_, id_);
            menu.Items().Append(exportMenu);
        } else {
            AppendRCodeOnlyExportMenu(menu, commandCallback_, id_);
        }
        AttachWindowContextFlyout(window_, menu);
        window_.Title(to_hstring(titleText_));
        title_.Text(to_hstring(titleText_));
        badge_.Text(to_hstring(datasetId_));
        std::string metadata;
        if (!report_.term.empty()) metadata =
            (report_.binaryProbability ? "Effect term: " : "Interaction term: ") +
            report_.term;
        if (!subtitleText_.empty()) {
            if (!metadata.empty()) metadata += "\n";
            metadata += subtitleText_;
        }
        if (!metadata.empty()) metadata += "\n";
        metadata += "Type: " + (report_.kind.empty() ? std::string("interaction") : report_.kind) +
            "\nConfidence level: " + FormatDouble(report_.confidenceLevel * 100.0, 0) + "%";
        if (report_.binaryProbability) {
            metadata += "\nEvent: " + report_.eventLabel;
            metadata += "\nAdjustment: " + std::string(
                report_.adjustmentMode == "reference_profile"
                    ? "Reference profile" : "Average over analysis sample");
        }
        if (!report_.multipleComparisons.empty())
            metadata += "\nMultiple comparisons: " + report_.multipleComparisons;
        if (report_.imputationCount > 1)
            metadata += "\nImputations: " + std::to_string(report_.imputationCount);
        if (!report_.poolingMethod.empty())
            metadata += "\nPooling: " + report_.poolingMethod;
        if (!report_.interpretation.empty())
            metadata += "\n\n" + report_.interpretation;
        subtitle_.Text(to_hstring(metadata));
        referenceControls_.Visibility(report_.binaryProbability
            ? Visibility::Collapsed : Visibility::Visible);
        binaryControls_.Visibility(report_.binaryProbability
            ? Visibility::Visible : Visibility::Collapsed);
        updatingBinaryOptions_ = true;
        effectQuantity_.SelectedIndex(
            report_.quantity == "probability_difference" ? 1 : 0);
        effectAdjustment_.SelectedIndex(
            report_.adjustmentMode == "reference_profile" ? 1 : 0);
        effectPresentation_.SelectedIndex(
            report_.presentation == "probability" ? 1 : 0);
        effectConfidence_.SelectedIndex(
            std::fabs(report_.confidenceLevel - 0.90) < 0.001 ? 0 :
            (std::fabs(report_.confidenceLevel - 0.99) < 0.001 ? 2 : 1));
        updatingBinaryOptions_ = false;
        updatingReference_ = true;
        if (!report_.emmReferenceTestEnabled ||
            !std::isfinite(report_.emmTestReference)) {
            emmReference_.SelectedIndex(0);
        } else if (std::fabs(report_.emmTestReference) < 1e-15) {
            emmReference_.SelectedIndex(1);
        } else {
            auto item = emmReference_.Items().GetAt(2).as<Controls::ComboBoxItem>();
            item.Content(box_value(to_hstring("Custom: " +
                FormatDouble(report_.emmTestReference, 8))));
            emmReference_.SelectedIndex(2);
        }
        const auto groupingFactors = ::rlispstat::core::GLMInteractionGroupingFactors(report_);
        groupingControls_.Visibility(groupingFactors.empty() ? Visibility::Collapsed : Visibility::Visible);
        comparisonLabel_.Visibility(groupingFactors.size() == 3 ? Visibility::Visible : Visibility::Collapsed);
        comparison_.Visibility(groupingFactors.size() == 3 ? Visibility::Visible : Visibility::Collapsed);
        stratumLabel_.Visibility(groupingFactors.size() == 3 ? Visibility::Visible : Visibility::Collapsed);
        grouping_.Items().Clear();
        comparison_.Items().Clear();
        for (const auto &factor : groupingFactors) {
            auto item = Controls::ComboBoxItem();
            item.Content(box_value(to_hstring(factor)));
            grouping_.Items().Append(item);
            auto comparisonItem = Controls::ComboBoxItem();
            comparisonItem.Content(box_value(to_hstring(factor)));
            comparison_.Items().Append(comparisonItem);
        }
        if (groupingFactors.size() == 3) {
            comparison_.SelectedIndex(0);
            stratumLabel_.Text(to_hstring("Within: " + groupingFactors[2]));
        }
        if (!groupingFactors.empty()) grouping_.SelectedIndex(1);
        updatingReference_ = false;
        Render();
        PresentWindowOnce(window_, presented_, 1220, 720);
    }

    void GLMInteractionReportView::Render()
    {
        using namespace ::rlispstat::core;
        content_.Children().Clear();
        if (!report_.ok)
        {
            auto warning = FieldLabel(to_hstring(report_.message.empty()
                ? "The interaction could not be interpreted." : report_.message).c_str());
            warning.FontSize(13);
            warning.Foreground(Brush(158, 54, 48));
            warning.TextWrapping(TextWrapping::Wrap);
            content_.Children().Append(warning);
            return;
        }

        for (auto const& section : report_.sections)
        {
            const bool estimateOnly = section.inference ==
                GLMInteractionSectionInference::EstimateOnly;
            const bool adjusted = section.inference ==
                GLMInteractionSectionInference::AdjustedComparison;
            const std::vector<std::string> identityHeaders =
                section.identityHeaders.empty()
                    ? std::vector<std::string>{ "Effect" }
                    : section.identityHeaders;
            const std::size_t identityCount = identityHeaders.size();
            std::vector<std::string> headers = identityHeaders;
            headers.push_back(section.estimateHeader);
            headers.push_back("SE");
            if (section.showDegreesOfFreedom)
                headers.push_back("df");
            if (!estimateOnly)
            {
                headers.push_back("Statistic");
                headers.push_back(adjusted ? "Adjusted p" : "p");
            }
            headers.push_back(section.lowerHeader);
            headers.push_back(section.upperHeader);
            auto sectionTitle = FieldLabel(to_hstring(section.first).c_str());
            sectionTitle.FontSize(14);
            sectionTitle.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
            content_.Children().Append(sectionTitle);

            auto grid = Controls::Grid();
            for (std::size_t index = 0; index < headers.size(); ++index)
            {
                auto column = Controls::ColumnDefinition();
                if (index < identityCount)
                {
                    const bool comparison = headers[index] == "Comparison";
                    const double width = identityCount == 1 ? 430.0
                        : comparison ? 300.0 : 170.0;
                    column.Width(GridLengthHelper::FromPixels(width));
                }
                else
                {
                    const std::string& header = headers[index];
                    const bool interval = header.rfind("Lower ", 0) == 0 ||
                        header.rfind("Upper ", 0) == 0 || header == "Adjusted p";
                    column.Width(GridLengthHelper::FromPixels(interval ? 112 : 92));
                }
                grid.ColumnDefinitions().Append(column);
            }
            auto addRow = [&grid]()
            {
                auto row = Controls::RowDefinition();
                row.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Auto));
                grid.RowDefinitions().Append(row);
            };
            auto add = [&grid](FrameworkElement const& cell, int row, int column)
            {
                Controls::Grid::SetRow(cell, row);
                Controls::Grid::SetColumn(cell, column);
                grid.Children().Append(cell);
            };
            addRow();
            for (std::size_t column = 0; column < headers.size(); ++column)
            {
                const bool identity = column < identityCount;
                auto cell = TableCell(to_hstring(headers[column]).c_str(), true, identity);
                if (identity)
                {
                    auto text = cell.Child().as<Controls::TextBlock>();
                    text.TextWrapping(TextWrapping::Wrap);
                    text.TextTrimming(TextTrimming::None);
                }
                add(cell, 0, static_cast<int>(column));
                AttachDirectMenu(cell, identity
                    ? CreateStructuralCellMenu(to_hstring(headers[column]).c_str(),
                        cell.Child().as<Controls::TextBlock>())
                    : CreateStatisticCellMenu(to_hstring(headers[column]).c_str(),
                        cell.Child().as<Controls::TextBlock>(),
                        InteractionReportStatisticContext(report_, section,
                            headers[column])));
            }
            int rowIndex = 1;
            for (std::size_t estimateIndex = 0;
                 estimateIndex < section.second.size(); ++estimateIndex)
            {
                auto const& estimate = section.second[estimateIndex];
                addRow();
                std::vector<std::string> values;
                if (estimateIndex < section.identityRows.size() &&
                    section.identityRows[estimateIndex].size() == identityCount)
                    values = section.identityRows[estimateIndex];
                else
                {
                    values.push_back(estimate.label);
                    while (values.size() < identityCount) values.push_back("\u2014");
                }
                values.push_back(FormatDoubleOrDash(estimate.estimate, 4));
                values.push_back(FormatDoubleOrDash(estimate.stdError, 4));
                if (section.showDegreesOfFreedom)
                    values.push_back(FormatDoubleOrDash(estimate.degreesOfFreedom, 2));
                if (!estimateOnly)
                {
                    values.push_back(FormatDoubleOrDash(estimate.statistic, 3));
                    values.push_back(adjusted && std::isfinite(estimate.adjustedPValue)
                        ? FormatPValue(estimate.adjustedPValue)
                        : FormatPValue(estimate.pValue));
                }
                values.push_back(FormatDoubleOrDash(estimate.ciLower, 4));
                values.push_back(FormatDoubleOrDash(estimate.ciUpper, 4));
                for (std::size_t column = 0; column < values.size(); ++column)
                {
                    const bool identity = column < identityCount;
                    auto cell = TableCell(to_hstring(values[column]).c_str(), false,
                                          identity, false, false, false,
                                          rowIndex > 1);
                    if (identity)
                    {
                        auto text = cell.Child().as<Controls::TextBlock>();
                        text.TextWrapping(TextWrapping::Wrap);
                        text.TextTrimming(TextTrimming::None);
                    }
                    add(cell, rowIndex, static_cast<int>(column));
                    AttachDirectMenu(cell, identity
                        ? CreateStructuralCellMenu(to_hstring(values[column]).c_str(),
                            cell.Child().as<Controls::TextBlock>())
                        : CreateStatisticCellMenu(to_hstring(headers[column]).c_str(),
                            cell.Child().as<Controls::TextBlock>(),
                            InteractionReportStatisticContext(report_, section,
                                headers[column])));
                }
                ++rowIndex;
            }
            content_.Children().Append(grid);
        }

        if (!report_.notes.empty())
        {
            for (auto const& text : report_.notes)
            {
                auto note = FieldLabel(to_hstring(text).c_str());
                note.FontSize(11);
                note.Foreground(Brush(92, 92, 92));
                note.TextWrapping(TextWrapping::Wrap);
                content_.Children().Append(note);
            }
        }
        else
        {
            auto note = FieldLabel(L"Estimates are linear functions of the fitted model coefficients (L beta); standard errors use L V L'.");
            note.FontSize(11);
            note.Foreground(Brush(92, 92, 92));
            note.TextWrapping(TextWrapping::Wrap);
            content_.Children().Append(note);
            if (report_.kind == "factor_factor" ||
                report_.kind == "categorical by categorical")
            {
                auto interactionNote = FieldLabel(
                    L"Pairwise comparisons decompose the interaction for interpretation; "
                    L"the interaction itself is tested by its interaction term in the model table.");
                interactionNote.FontSize(11);
                interactionNote.Foreground(Brush(92, 92, 92));
                interactionNote.TextWrapping(TextWrapping::Wrap);
                content_.Children().Append(interactionNote);
            }
        }
    }

    void GLMInteractionReportView::CopyReport()
    {
        CopyTextToClipboard(to_hstring(::rlispstat::core::GLMInteractionReportText(report_)));
    }

    void GLMInteractionReportView::Activate() { if (!closed_) window_.Activate(); }
    void GLMInteractionReportView::Close() { if (!closed_) window_.Close(); }

    std::shared_ptr<CorrelationMatrixView> CorrelationMatrixView::Create()
    {
        auto view = std::shared_ptr<CorrelationMatrixView>(new CorrelationMatrixView());
        view->AttachLifetime();
        return view;
    }

    CorrelationMatrixView::CorrelationMatrixView() { Initialize(); }

    void CorrelationMatrixView::Initialize()
    {
        window_ = CreateLinkEDAWindow();
        window_.Title(L"Correlation Matrix");
        auto root = Controls::Grid();
        root.Padding(Thickness{ 18, 14, 18, 10 });
        root.RowSpacing(7);
        root.Background(Brush(255, 255, 255));
        root.RequestedTheme(ElementTheme::Light);
        auto autoRow = []() { auto row = Controls::RowDefinition();
            row.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Auto)); return row; };
        root.RowDefinitions().Append(autoRow());
        root.RowDefinitions().Append(autoRow());
        auto contentRow = Controls::RowDefinition();
        contentRow.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
        root.RowDefinitions().Append(contentRow);
        root.RowDefinitions().Append(autoRow());

        auto heading = Controls::Grid();
        auto titleColumn = Controls::ColumnDefinition();
        titleColumn.Width(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
        heading.ColumnDefinitions().Append(titleColumn);
        heading.ColumnDefinitions().Append(Controls::ColumnDefinition());
        title_ = Controls::TextBlock(); title_.FontSize(17);
        title_.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        heading.Children().Append(title_);
        badge_ = Controls::TextBlock(); badge_.FontSize(12); badge_.Foreground(Brush(72,72,72));
        badge_.VerticalAlignment(VerticalAlignment::Center);
        Controls::Grid::SetColumn(badge_, 1); heading.Children().Append(badge_);
        root.Children().Append(heading);

        auto controls = Controls::StackPanel();
        controls.Orientation(Controls::Orientation::Horizontal); controls.Spacing(12);
        controls.VerticalAlignment(VerticalAlignment::Center);
        auto scopeLabel = FieldLabel(to_hstring(
            ::rlispstat::core::GLMScopeFieldLabel()).c_str());
        scopeLabel.VerticalAlignment(VerticalAlignment::Center);
        controls.Children().Append(scopeLabel);
        scope_ = Controls::ComboBox(); scope_.MinWidth(132); scope_.MinHeight(30);
        controls.Children().Append(scope_);
        // macOS exposes variable editing through the "+ Add variable" affordance
        // below the matrix, rather than as another control in this compact row.
        variablesButton_ = Controls::Button();
        scope_.SelectionChanged([this](auto const&, auto const&)
        {
            if (updatingControls_ || !commandCallback_) return;
            commandCallback_({ "CORR_SET_SCOPE", state_.id,
                SelectedAnalysisScopeChoice(scope_) });
        });
        Controls::Grid::SetRow(controls, 1); root.Children().Append(controls);

        auto scroll = Controls::ScrollViewer();
        scroll.HorizontalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);
        scroll.VerticalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);
        matrix_ = Controls::Canvas(); matrix_.Name(L"LinkEDA.SnapshotTable"); matrix_.Background(Brush(255,255,255));
        scroll.Content(matrix_); Controls::Grid::SetRow(scroll, 2); root.Children().Append(scroll);

        status_ = Controls::TextBlock(); status_.FontSize(11); status_.Foreground(Brush(100,100,100));
        Controls::Grid::SetRow(status_, 3); root.Children().Append(status_);
        exporter_ = TableExportService::Create(window_, root);
        window_.Content(root); ResizeLogical(window_, 760, 470);
    }

    void CorrelationMatrixView::AttachLifetime()
    {
        std::weak_ptr<CorrelationMatrixView> weak = shared_from_this();
        window_.Closed([weak](auto const&, auto const&)
        {
            if (auto view = weak.lock())
            {
                view->closed_ = true;
                if (view->closedCallback_) view->closedCallback_();
            }
        });
    }

    void CorrelationMatrixView::SetClosedCallback(Closed callback)
    {
        closedCallback_ = std::move(callback);
    }

    void CorrelationMatrixView::SetCommandCallback(Command callback)
    {
        commandCallback_ = std::move(callback);
        ConfigureContextMenu();
    }

    void CorrelationMatrixView::SetScopeChoices(
        std::vector<::rlispstat::core::AnalysisScopeChoice> choices)
    {
        scopeChoices_ = std::move(choices);
        if(scope_) PopulateAnalysisScopeCombo(scope_,scopeChoices_,"global");
    }

    void CorrelationMatrixView::SetVariables(std::vector<std::string> variables)
    {
        if (!commandCallback_) return;
        std::vector<std::string> command{ "CORR_SET_VARIABLES", state_.id,
            std::to_string(variables.size()) };
        command.insert(command.end(), variables.begin(), variables.end());
        commandCallback_(command);
    }

    Controls::MenuFlyout CorrelationMatrixView::CreateVariableMenu(
        std::size_t variableIndex)
    {
        auto menu = Controls::MenuFlyout();
        if (variableIndex >= state_.variables.size()) return menu;
        const std::string variable = state_.variables[variableIndex];
        auto heading = Controls::MenuFlyoutItem(); heading.Text(to_hstring(variable));
        heading.IsEnabled(false); menu.Items().Append(heading);

        if (state_.precomputed || !state_.hasSeed)
        {
            auto locked = Controls::MenuFlyoutItem();
            locked.Text(L"Variables are fixed for this result"); locked.IsEnabled(false);
            menu.Items().Append(locked); AppendDisplayOptions(menu); return menu;
        }

        const auto numeric = ::rlispstat::core::NumericVariableNames(&state_.seed);
        auto replace = Controls::MenuFlyoutSubItem(); replace.Text(L"Replace with");
        for (auto const& name : numeric)
        {
            if (std::find(state_.variables.begin(), state_.variables.end(), name) !=
                state_.variables.end()) continue;
            auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(name));
            item.Click([this, variableIndex, name](auto const&, auto const&)
            {
                auto updated = ::rlispstat::core::CorrelationVariablesAfterReplace(
                    state_.variables, variableIndex, name,
                    ::rlispstat::core::NumericVariableNames(&state_.seed));
                if (updated.ok && updated.changed) SetVariables(std::move(updated.variables));
            });
            replace.Items().Append(item);
        }
        if (replace.Items().Size() == 0)
        {
            auto empty = Controls::MenuFlyoutItem(); empty.Text(L"No replacement variables");
            empty.IsEnabled(false); replace.Items().Append(empty);
        }
        menu.Items().Append(replace);

        auto add = Controls::MenuFlyoutSubItem(); add.Text(L"Add variable");
        for (auto const& name : numeric)
        {
            if (std::find(state_.variables.begin(), state_.variables.end(), name) !=
                state_.variables.end()) continue;
            auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(name));
            item.Click([this, name](auto const&, auto const&)
            { auto updated = state_.variables; updated.push_back(name); SetVariables(std::move(updated)); });
            add.Items().Append(item);
        }
        if (add.Items().Size() > 0) menu.Items().Append(add);

        auto types = Controls::MenuFlyoutSubItem(); types.Text(L"Change type");
        auto numericType = Controls::ToggleMenuFlyoutItem(); numericType.Text(L"Numeric");
        numericType.IsChecked(true); numericType.IsEnabled(false); types.Items().Append(numericType);
        auto factorType = Controls::MenuFlyoutItem(); factorType.Text(L"Categorical");
        factorType.IsEnabled(state_.variables.size() > 2);
        factorType.Click([this, variable](auto const&, auto const&)
        {
            if (commandCallback_) commandCallback_({ "CORR_SET_TYPE", state_.id,
                variable + '\x1f' + "factor" });
        });
        types.Items().Append(factorType); menu.Items().Append(types);

        auto remove = Controls::MenuFlyoutItem();
        remove.Text(to_hstring("Remove " + variable));
        remove.IsEnabled(state_.variables.size() > 2);
        remove.Click([this, variable](auto const&, auto const&)
        {
            auto updated = state_.variables;
            updated.erase(std::remove(updated.begin(), updated.end(), variable), updated.end());
            SetVariables(std::move(updated));
        });
        menu.Items().Append(remove);
        AppendMissingInformationMenu(menu, commandCallback_, state_.id, state_.multipleImputation);
        AppendRCodeOnlyExportMenu(menu, commandCallback_, state_.id);
        AppendDisplayOptions(menu);
        return menu;
    }

    void CorrelationMatrixView::AppendDisplayOptions(Controls::MenuFlyout const& menu)
    {
        Controls::MenuFlyoutSubItem sectionMenu{nullptr};std::string section;
        for (auto const& option : ::rlispstat::core::CorrelationContextOptions(state_)) {
            if(section!=option.section) {
                section=option.section;sectionMenu=Controls::MenuFlyoutSubItem();
                sectionMenu.Text(to_hstring(section));menu.Items().Append(sectionMenu);
            }
            if(option.value=="show_p")sectionMenu.Items().Append(Controls::MenuFlyoutSeparator());
            auto item=Controls::ToggleMenuFlyoutItem();item.Text(to_hstring(option.title));
            item.IsChecked(option.checked);item.IsEnabled(option.enabled);
            item.Click([this,option](auto const&,auto const&) {
                if(commandCallback_)commandCallback_({option.command,state_.id,option.value});
            });
            sectionMenu.Items().Append(item);
        }
    }

    void CorrelationMatrixView::ConfigureContextMenu()
    {
        auto menu = Controls::MenuFlyout();
        AppendMissingInformationMenu(menu,commandCallback_,state_.id,state_.multipleImputation);
        if (!state_.precomputed && state_.hasSeed)
        {
            const auto numeric = ::rlispstat::core::NumericVariableNames(&state_.seed);
            auto variables = Controls::MenuFlyoutSubItem(); variables.Text(L"Variables");
            auto add = Controls::MenuFlyoutSubItem(); add.Text(to_hstring(
                ::rlispstat::core::CorrelationAddVariableMenuTitle()));
            for (auto const& name : numeric)
            {
                if (std::find(state_.variables.begin(), state_.variables.end(), name) != state_.variables.end()) continue;
                auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(name));
                item.Click([this, name](auto const&, auto const&)
                { auto updated = state_.variables; updated.push_back(name); SetVariables(std::move(updated)); });
                add.Items().Append(item);
            }
            if (add.Items().Size() > 0) variables.Items().Append(add);
            auto replace = Controls::MenuFlyoutSubItem(); replace.Text(L"Replace variable");
            for (std::size_t index = 0; index < state_.variables.size(); ++index)
            {
                auto current = Controls::MenuFlyoutSubItem();
                current.Text(to_hstring(state_.variables[index]));
                for (auto const& name : numeric)
                {
                    if (std::find(state_.variables.begin(), state_.variables.end(), name) !=
                        state_.variables.end()) continue;
                    auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(name));
                    item.Click([this, index, name](auto const&, auto const&)
                    {
                        auto updated = ::rlispstat::core::CorrelationVariablesAfterReplace(
                            state_.variables, index, name,
                            ::rlispstat::core::NumericVariableNames(&state_.seed));
                        if (updated.ok && updated.changed) SetVariables(std::move(updated.variables));
                    });
                    current.Items().Append(item);
                }
                if (current.Items().Size() > 0) replace.Items().Append(current);
            }
            if (replace.Items().Size() > 0) variables.Items().Append(replace);
            auto types = Controls::MenuFlyoutSubItem(); types.Text(L"Change type");
            for (auto const& name : state_.variables)
            {
                auto current = Controls::MenuFlyoutSubItem(); current.Text(to_hstring(name));
                auto numericType = Controls::ToggleMenuFlyoutItem(); numericType.Text(L"Numeric");
                numericType.IsChecked(true);
                numericType.IsEnabled(false); current.Items().Append(numericType);
                auto factorType = Controls::MenuFlyoutItem(); factorType.Text(L"Categorical");
                factorType.IsEnabled(state_.variables.size() > 2);
                factorType.Click([this, name](auto const&, auto const&)
                {
                    if (commandCallback_) commandCallback_({ "CORR_SET_TYPE", state_.id,
                        name + '\x1f' + "factor" });
                });
                current.Items().Append(factorType); types.Items().Append(current);
            }
            variables.Items().Append(types);
            if (state_.variables.size() > 2)
            {
                auto remove = Controls::MenuFlyoutSubItem(); remove.Text(L"Remove variable");
                for (auto const& name : state_.variables)
                {
                    auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(name));
                    item.Click([this, name](auto const&, auto const&)
                    { auto updated = state_.variables; updated.erase(std::remove(updated.begin(), updated.end(), name), updated.end()); SetVariables(std::move(updated)); });
                    remove.Items().Append(item);
                }
                variables.Items().Append(remove);
            }
            menu.Items().Append(variables);
        }
        AppendDisplayOptions(menu);
        if (exporter_) {
            auto exportMenu = exporter_->CreateMenu(
                [this]() { return CorrelationExportPayload(state_); });
            AppendRCodeExportItems(exportMenu, commandCallback_, state_.id);
            menu.Items().Append(exportMenu);
        } else {
            AppendRCodeOnlyExportMenu(menu, commandCallback_, state_.id);
        }
        AttachWindowContextFlyout(window_, menu);

        auto variableMenu = Controls::MenuFlyout();
        if (!state_.precomputed && state_.hasSeed)
        {
            const auto numeric = ::rlispstat::core::NumericVariableNames(&state_.seed);
            const auto addState = ::rlispstat::core::BuildCorrelationAddVariableMenuState(
                state_.variables, numeric, false);
            for (auto const& name : addState.variables)
            {
                auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(name));
                item.Click([this, name](auto const&, auto const&)
                { auto updated = state_.variables; updated.push_back(name); SetVariables(std::move(updated)); });
                variableMenu.Items().Append(item);
            }
            if (variableMenu.Items().Size() == 0)
            {
                auto empty = Controls::MenuFlyoutItem();
                empty.Text(to_hstring(addState.emptyTitle)); empty.IsEnabled(false);
                variableMenu.Items().Append(empty);
            }
        }
        else
        {
            auto locked = Controls::MenuFlyoutItem();
            locked.Text(L"Variables are fixed for this result"); locked.IsEnabled(false);
            variableMenu.Items().Append(locked);
        }
        AppendRCodeOnlyExportMenu(variableMenu, commandCallback_, state_.id);
        variablesButton_.Flyout(variableMenu);
        variablesButton_.IsEnabled(!state_.precomputed && state_.hasSeed);
    }

    void CorrelationMatrixView::Show(
        ::rlispstat::core::CorrelationMatrixState const& state,
        std::size_t selectedRowCount)
    {
        ::rlispstat::windows::performance::Scope timing("Output.Correlation.Show");
        SetWindowDataSheetGroup(window_, state.group);
        SetWindowSnapshotSource(window_, "output", state.id, state.group);
        state_ = state;
        const auto controls = ::rlispstat::core::BuildCorrelationWindowControlState(
            state.group, state.title, state.variables, state.missingMode,
            state.showP, state.showPValue, state.showN, state.precomputed,
            state.multipleImputation, state.imputationCount, state.note);
        window_.Title(to_hstring(controls.windowTitle));
        title_.Text(to_hstring(controls.title));
        badge_.Text(to_hstring(controls.badge));
        updatingControls_ = true;
        auto choices = scopeChoices_.empty()
            ? ::rlispstat::core::BuildAnalysisScopeChoices(selectedRowCount, {}, false)
            : scopeChoices_;
        PopulateAnalysisScopeCombo(scope_, choices,
            ::rlispstat::core::AnalysisScopeChoiceValue(
                "all", state.dataScope, state.dataScopeCaptured));
        scope_.IsEnabled(!state.precomputed);
        updatingControls_ = false;
        std::string status = controls.status;
        if (state.dataScopeCaptured)
            status += " \u00b7 " + ::rlispstat::core::AnalysisScopeWindowSummary(
                state.dataScope, state.dataScope.totalDatasetRows);
        status_.Text(to_hstring(status));
        Render();
        ConfigureContextMenu();
        const auto size = ::rlispstat::core::CorrelationMatrixPreferredContentSize(
            state.variables.size(), state.showPValue, state.showN);
        PresentOrResizeAnalysisWindow(window_, presented_,
            std::clamp(size.width + 72.0, 760.0, 1180.0),
            std::clamp(size.height + 135.0, 360.0, 760.0));
    }

    void CorrelationMatrixView::Render()
    {
        using namespace ::rlispstat::core;
        matrix_.Children().Clear();
        const auto plan = BuildCorrelationMatrixRenderPlan(
            state_.variables, state_.cells, state_.selectedRow, state_.selectedCol,
            state_.showP, state_.showPValue, state_.showN, state_.displayPart);
        matrix_.Width(plan.layout.width); matrix_.Height(plan.layout.height);
        auto addRect = [this](Rect const& rect, Media::Brush const& fill)
        {
            auto shape = Shapes::Rectangle(); shape.Width(rect.width); shape.Height(rect.height);
            shape.Fill(fill); Controls::Canvas::SetLeft(shape, rect.x);
            Controls::Canvas::SetTop(shape, rect.y); matrix_.Children().Append(shape);
        };
        auto addText = [this](std::string const& text, Rect const& rect, double size,
                              bool bold, bool muted, TextAlignment alignment)
        {
            auto label = Controls::TextBlock(); label.Text(to_hstring(text)); label.FontSize(size);
            label.Foreground(Brush(muted ? 105 : 42, muted ? 105 : 44, muted ? 105 : 48));
            if (bold) label.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
            label.Width(std::max(0.0, rect.width)); label.Height(std::max(0.0, rect.height));
            label.TextAlignment(alignment); label.TextTrimming(TextTrimming::CharacterEllipsis);
            Controls::Canvas::SetLeft(label, rect.x); Controls::Canvas::SetTop(label, rect.y);
            matrix_.Children().Append(label);
            return label;
        };
        addRect(plan.headerRuleRect, Brush(222,225,229));
        for (std::size_t index = 0; index < plan.columnHeaders.size(); ++index)
        {
            auto const& header = plan.columnHeaders[index];
            auto label = addText(header.text, header.rect, 11.5, true, false,
                TextAlignment::Center);
            AttachDirectMenu(label, CreateVariableMenu(index));
        }
        for (std::size_t index = 0; index < plan.rowHeaders.size(); ++index)
        {
            auto const& header = plan.rowHeaders[index];
            auto label = addText(header.text, header.rect, 11.5, true, false,
                TextAlignment::Left);
            AttachDirectMenu(label, CreateVariableMenu(index));
        }
        for (auto const& cell : plan.cells)
        {
            if (cell.background == CorrelationCellBackground::Diagonal)
                addRect(cell.rect, Brush(247,248,249));
            else if (cell.background == CorrelationCellBackground::Selected)
                addRect(cell.rect, Brush(223,237,250));
            for (auto const& item : cell.textItems)
            {
                auto label=addText(item.text, item.rect, item.muted ? 10.0 : 11.5,
                    !item.muted, item.muted, TextAlignment::Center);
                std::string statistic;
                if (item.text.rfind("p", 0) == 0) statistic = "p";
                else if (item.text.rfind("N", 0) == 0) statistic = "N";
                else if (state_.method == "spearman") statistic = "Spearman rho";
                else if (state_.method == "kendall") statistic = "Kendall tau";
                else statistic = "Pearson r";
                auto cellMenu=CreateStatisticCellMenu(
                    to_hstring(statistic).c_str(),label,
                    AnalysisStatisticContext(statistic,"correlation",state_.method,{},
                        state_.multipleImputation,state_.poolingMethod));
                AppendDisplayOptions(cellMenu);AttachDirectMenu(label,cellMenu);
            }
        }
        if (plan.showEmptyMessage)
            addText(plan.emptyMessage, plan.emptyMessageRect, 12.0, false, true,
                TextAlignment::Left);
        variablesButton_.Content(box_value(to_hstring(plan.addVariableLabel)));
        variablesButton_.FontSize(11.0);
        variablesButton_.Foreground(Brush(105, 105, 105));
        variablesButton_.Background(nullptr);
        variablesButton_.BorderBrush(nullptr);
        variablesButton_.Padding(Thickness{ 0, 0, 0, 0 });
        variablesButton_.HorizontalContentAlignment(HorizontalAlignment::Left);
        variablesButton_.Width(plan.addVariableRect.width);
        variablesButton_.Height(plan.addVariableRect.height);
        Controls::Canvas::SetLeft(variablesButton_, plan.addVariableRect.x);
        Controls::Canvas::SetTop(variablesButton_, plan.addVariableRect.y);
        matrix_.Children().Append(variablesButton_);
    }

    void CorrelationMatrixView::Activate() { if (!closed_) window_.Activate(); }
    void CorrelationMatrixView::Close() { if (!closed_) window_.Close(); }

    std::shared_ptr<MeanComparisonView> MeanComparisonView::Create(bool descriptivesOnly)
    {
        auto view = std::shared_ptr<MeanComparisonView>(new MeanComparisonView(descriptivesOnly));
        view->AttachLifetime(); return view;
    }

    MeanComparisonView::MeanComparisonView(bool descriptivesOnly)
        : descriptivesOnly_(descriptivesOnly) { Initialize(); }

    void MeanComparisonView::Initialize()
    {
        window_ = CreateLinkEDAWindow(); window_.Title(L"Mean comparison");
        auto root = Controls::Grid(); root.Padding(Thickness{18,14,18,12}); root.RowSpacing(5);
        root.Background(Brush(255,255,255)); root.RequestedTheme(ElementTheme::Light);
        auto autoRow = [](){ auto row=Controls::RowDefinition();
            row.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Auto)); return row; };
        root.RowDefinitions().Append(autoRow()); root.RowDefinitions().Append(autoRow());
        root.RowDefinitions().Append(autoRow());
        auto body=Controls::RowDefinition(); body.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Star));
        root.RowDefinitions().Append(body);
        auto heading=Controls::Grid(); auto grow=Controls::ColumnDefinition();
        grow.Width(GridLengthHelper::FromValueAndType(1,GridUnitType::Star));
        heading.ColumnDefinitions().Append(grow); heading.ColumnDefinitions().Append(Controls::ColumnDefinition());
        title_=Controls::TextBlock(); title_.FontSize(17); title_.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        heading.Children().Append(title_); badge_=Controls::TextBlock(); badge_.FontSize(12); badge_.Foreground(Brush(72,72,72));
        badge_.VerticalAlignment(VerticalAlignment::Center); Controls::Grid::SetColumn(badge_,1); heading.Children().Append(badge_);
        root.Children().Append(heading);
        subtitle_=Controls::TextBlock(); subtitle_.FontSize(11); subtitle_.Foreground(Brush(95,95,95));
        subtitle_.Margin(Thickness{0,5,0,7}); subtitle_.TextWrapping(TextWrapping::Wrap);
        subtitle_.MaxLines(3); Controls::Grid::SetRow(subtitle_,1); root.Children().Append(subtitle_);
        controls_=Controls::StackPanel(); controls_.Orientation(Controls::Orientation::Vertical);
        controls_.Spacing(8); controls_.Margin(Thickness{0,0,0,7});
        Controls::Grid::SetRow(controls_,2); root.Children().Append(controls_);
        auto scroll=Controls::ScrollViewer(); scroll.HorizontalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);
        scroll.VerticalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);
        content_=Controls::StackPanel(); content_.Name(L"LinkEDA.SnapshotTable"); content_.Spacing(10); scroll.Content(content_);
        Controls::Grid::SetRow(scroll,3); root.Children().Append(scroll);
        exporter_ = TableExportService::Create(window_, root);
        window_.Content(root); ResizeLogical(window_,820,620);
    }

    void MeanComparisonView::AttachLifetime()
    {
        std::weak_ptr<MeanComparisonView> weak=shared_from_this();
        window_.Closed([weak](auto const&,auto const&){ if(auto view=weak.lock()){
            view->closed_=true; if(view->closedCallback_) view->closedCallback_(); }});
    }
    void MeanComparisonView::SetClosedCallback(Closed callback){ closedCallback_=std::move(callback); }

    void MeanComparisonView::SetCommandCallback(
        Command callback,
        std::vector<std::string> responseVariables,
        std::vector<std::string> groupingVariables,
        std::map<std::string, std::string> variableTypes,
        std::map<std::string, std::vector<std::string>> groupingLevels,
        std::size_t selectedRowCount,
        std::size_t totalRowCount)
    {
        commandCallback_ = std::move(callback);
        responseVariables_ = std::move(responseVariables);
        groupingVariables_ = std::move(groupingVariables);
        variableTypes_ = std::move(variableTypes);
        groupingLevels_ = std::move(groupingLevels);
        selectedRowCount_ = selectedRowCount;
        totalRowCount_ = totalRowCount;
    }

    void MeanComparisonView::SetScopeChoices(
        std::vector<::rlispstat::core::AnalysisScopeChoice> choices)
    {
        scopeChoices_ = std::move(choices);
        RebuildControls();
    }

    void MeanComparisonView::OpenVariableChooser(
        std::vector<std::string> candidates,
        std::wstring title,
        std::wstring information,
        std::string currentValue,
        std::function<void(std::string const&)> submit,
        std::function<void()> afterClosed)
    {
        if (candidates.empty() || variableDialog_) return;
        ::rlispstat::core::DataFrameModel choices;
        choices.group = state_.datasetId;
        choices.columns.reserve(candidates.size());
        for (auto const& candidate : candidates)
        {
            ::rlispstat::core::DataColumn column;
            column.name = candidate;
            choices.columns.push_back(std::move(column));
        }
        std::weak_ptr<MeanComparisonView> weak = shared_from_this();
        variableDialog_ = DatasetVariableDialog::Create(
            window_, choices, std::move(title), std::move(information), L"Choose",
            false, std::move(currentValue),
            [weak, submit = std::move(submit)](std::vector<std::string> const& selected)
            {
                if (selected.empty()) return;
                if (auto view = weak.lock())
                    if (submit) submit(selected.front());
            },
            [weak, afterClosed = std::move(afterClosed)]()
            {
                if (auto view = weak.lock())
                {
                    view->variableDialog_.reset();
                    if (afterClosed) afterClosed();
                }
            });
        variableDialog_->Show();
    }

    void MeanComparisonView::AppendVariableChoices(
        Controls::MenuFlyoutSubItem const& parent,
        std::vector<std::string> const& candidates,
        std::wstring chooserTitle,
        std::wstring chooserInformation,
        std::string currentValue,
        std::function<void(std::string const&)> action)
    {
        if (candidates.size() <= kInlineVariableMenuLimit)
        {
            for (auto const& candidate : candidates)
            {
                if (candidate == currentValue)
                {
                    auto item = Controls::ToggleMenuFlyoutItem();
                    item.Text(to_hstring(candidate)); item.IsChecked(true);
                    item.Click([action, candidate](auto const&, auto const&)
                        { if (action) action(candidate); });
                    parent.Items().Append(item);
                }
                else
                {
                    auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(candidate));
                    item.Click([action, candidate](auto const&, auto const&)
                        { if (action) action(candidate); });
                    parent.Items().Append(item);
                }
            }
            return;
        }
        auto choose = Controls::MenuFlyoutItem(); choose.Text(L"Choose variable…");
        choose.Click([this, candidates, chooserTitle = std::move(chooserTitle),
                      chooserInformation = std::move(chooserInformation),
                      currentValue = std::move(currentValue), action = std::move(action)]
                     (auto const&, auto const&) mutable
        {
            OpenVariableChooser(std::move(candidates), std::move(chooserTitle),
                std::move(chooserInformation), std::move(currentValue), std::move(action));
        });
        parent.Items().Append(choose);
    }

    void MeanComparisonView::OpenPairChooser(std::vector<std::string> candidates)
    {
        if (candidates.size() < 2 || variableDialog_) return;
        auto first = std::make_shared<std::string>();
        std::weak_ptr<MeanComparisonView> weak = shared_from_this();
        OpenVariableChooser(candidates, L"Add pair — first variable",
            L"Choose the first variable in the paired comparison.", {},
            [first](std::string const& selected) { *first = selected; },
            [weak, candidates = std::move(candidates), first]() mutable
            {
                auto view = weak.lock();
                if (!view || first->empty()) return;
                candidates.erase(std::remove(candidates.begin(), candidates.end(), *first),
                    candidates.end());
                view->OpenVariableChooser(std::move(candidates),
                    L"Add pair — second variable",
                    L"Choose the second variable in the paired comparison.", {},
                    [weak, first](std::string const& second)
                    {
                        if (auto current = weak.lock())
                            if (current->commandCallback_)
                                current->commandCallback_({"MEAN_ADD_PAIR", current->state_.id,
                                    *first + '\x1f' + second});
                    });
            });
    }

    Controls::MenuFlyoutSubItem MeanComparisonView::CreatePlotsMenu(
        std::string const& variable,
        std::string const& secondVariable,
        std::wstring const& title)
    {
        auto plots = Controls::MenuFlyoutSubItem(); plots.Text(title);
        auto add = [this, plots, variable, secondVariable](
            std::wstring const& label, std::string const& kind)
        {
            auto item = Controls::MenuFlyoutItem(); item.Text(label);
            item.Click([this, kind, variable, secondVariable](auto const&, auto const&)
            {
                if (commandCallback_)
                    commandCallback_({"MEAN_OPEN_PLOT", state_.id,
                        kind + '\x1f' + variable + '\x1f' + secondVariable});
            });
            plots.Items().Append(item);
        };

        const auto type = variableTypes_.find(variable);
        const std::string variableType = type == variableTypes_.end()
            ? std::string("numeric") : type->second;
        const bool categorical =
            ::rlispstat::core::VariableTypeIsCategorical(variableType);
        if ((state_.analysisType == "one_sample_t_test" ||
             state_.analysisType == "independent_samples_t_test") && categorical)
        {
            add(L"Bar chart\u2026", "bar_chart");
            return plots;
        }

        const auto options = ::rlispstat::core::MeanComparisonPlotMenuOptions(
            state_.analysisType, !variable.empty(), !secondVariable.empty());
        for (auto const& option : options)
        {
            add(to_hstring(option.title).c_str(), option.plotKind);
        }
        return plots;
    }

    Controls::MenuFlyout MeanComparisonView::CreateVariableMenu(
        ::rlispstat::core::MeanComparisonRow const& row)
    {
        auto menu = Controls::MenuFlyout();
        auto send = [this](std::string command, std::string value = {})
        {
            if (commandCallback_)
                commandCallback_({ std::move(command), state_.id, std::move(value) });
        };
        const bool paired = state_.analysisType == "paired_samples_t_test";
        if (paired)
        {
            auto pair = std::find_if(state_.specification.pairs.begin(),
                state_.specification.pairs.end(), [&](auto const& candidate)
                {
                    return candidate.pairId == row.rowId ||
                        (candidate.firstVariableId == row.variable &&
                         candidate.secondVariableId == row.secondVariable);
                });
            if (pair == state_.specification.pairs.end()) return menu;
            auto heading = Controls::MenuFlyoutItem();
            heading.Text(to_hstring(pair->firstVariableId + " \u2212 " + pair->secondVariableId));
            heading.IsEnabled(false); menu.Items().Append(heading);
            for (auto const& position : std::array<std::pair<std::string, std::string>, 2>{{
                {"first", pair->firstVariableId}, {"second", pair->secondVariableId}}})
            {
                auto replace = Controls::MenuFlyoutSubItem();
                replace.Text(to_hstring(position.first == "first"
                    ? "Replace first variable" : "Replace second variable"));
                std::vector<std::string> candidates;
                for (auto const& candidate : responseVariables_)
                {
                    const std::string nextFirst = position.first == "first"
                        ? candidate : pair->firstVariableId;
                    const std::string nextSecond = position.first == "second"
                        ? candidate : pair->secondVariableId;
                    const bool duplicate = nextFirst == nextSecond || std::any_of(
                        state_.specification.pairs.begin(), state_.specification.pairs.end(),
                        [&](auto const& other)
                        {
                            return other.pairId != pair->pairId &&
                                other.firstVariableId == nextFirst &&
                                other.secondVariableId == nextSecond;
                        });
                    if (candidate == position.second || duplicate) continue;
                    candidates.push_back(candidate);
                }
                AppendVariableChoices(replace, candidates, L"Replace paired variable",
                    L"Choose the replacement variable.", {},
                    [send, id = pair->pairId, side = position.first]
                    (std::string const& candidate) mutable
                    { send("MEAN_REPLACE_PAIR", id + '\x1e' + side + '\x1e' + candidate); });
                if (replace.Items().Size() > 0) menu.Items().Append(replace);
            }
            auto types = Controls::MenuFlyoutSubItem(); types.Text(L"Change type");
            for (auto const& variable : {pair->firstVariableId, pair->secondVariableId})
            {
                auto current = Controls::MenuFlyoutSubItem(); current.Text(to_hstring(variable));
                const auto found = variableTypes_.find(variable);
                const std::string currentType = found == variableTypes_.end() ? "numeric" : found->second;
                for (auto const& [type, title] : std::array<std::pair<std::string, std::wstring>, 3>{ {
                    {"numeric", L"Numeric"}, {"ordered", L"Ordinal"}, {"factor", L"Categorical"} } })
                {
                    auto item = Controls::RadioMenuFlyoutItem();
                    item.GroupName(to_hstring("mean-pair-type:" + state_.id + ":" + variable));
                    item.Text(title); item.IsChecked(currentType == type);
                    item.Click([send, variable, type](auto const&, auto const&) mutable
                        { send("MEAN_SET_VARIABLE_TYPE", variable + '\x1f' + type); });
                    current.Items().Append(item);
                }
                types.Items().Append(current);
            }
            menu.Items().Append(types);
            auto reverse = Controls::MenuFlyoutItem(); reverse.Text(L"Reverse pair");
            reverse.Click([send, id = pair->pairId](auto const&, auto const&) mutable
                { send("MEAN_REVERSE_PAIR", id); });
            menu.Items().Append(reverse);
            auto remove = Controls::MenuFlyoutItem(); remove.Text(L"Remove pair");
            remove.IsEnabled(state_.specification.pairs.size() > 1);
            remove.Click([send, id = pair->pairId](auto const&, auto const&) mutable
                { send("MEAN_REMOVE_PAIR", id); });
            menu.Items().Append(remove);
            auto plots = CreatePlotsMenu(pair->firstVariableId,
                pair->secondVariableId, L"Plots");
            if (plots.Items().Size() > 0)
            {
                menu.Items().Append(Controls::MenuFlyoutSeparator());
                menu.Items().Append(plots);
            }
            return menu;
        }

        if (row.variable.empty()) return menu;
        auto heading = Controls::MenuFlyoutItem(); heading.Text(to_hstring(row.variable));
        heading.IsEnabled(false); menu.Items().Append(heading);
        std::vector<std::string> candidates;
        for (auto const& candidate : responseVariables_)
        {
            const bool used = std::find(state_.specification.dependentVariableIds.begin(),
                state_.specification.dependentVariableIds.end(), candidate) !=
                state_.specification.dependentVariableIds.end();
            const bool grouping = state_.specification.groupingVariableId &&
                *state_.specification.groupingVariableId == candidate;
            if (!used && !grouping) candidates.push_back(candidate);
        }
        auto replace = Controls::MenuFlyoutSubItem(); replace.Text(L"Replace with");
        AppendVariableChoices(replace, candidates, L"Replace dependent variable",
            L"Choose the replacement dependent variable.", {},
            [send, current = row.variable](std::string const& candidate) mutable
                { send("MEAN_REPLACE_DEPENDENT", current + '\x1f' + candidate); });
        if (replace.Items().Size() == 0)
        {
            auto empty = Controls::MenuFlyoutItem(); empty.Text(L"No replacement variables");
            empty.IsEnabled(false); replace.Items().Append(empty);
        }
        menu.Items().Append(replace);

        auto add = Controls::MenuFlyoutSubItem(); add.Text(L"Add dependent variable");
        AppendVariableChoices(add, candidates, L"Add dependent variable",
            L"Choose a dependent variable to add.", {},
            [send](std::string const& candidate) mutable
                { send("MEAN_ADD_DEPENDENT", candidate); });
        if (add.Items().Size() > 0) menu.Items().Append(add);

        if (state_.analysisType == "one_sample_t_test" ||
            state_.analysisType == "independent_samples_t_test")
        {
            auto types = Controls::MenuFlyoutSubItem(); types.Text(L"Change type");
            const auto found = variableTypes_.find(row.variable);
            const std::string currentType = found == variableTypes_.end() ? "numeric" : found->second;
            for (auto const& [type, title] : std::array<std::pair<std::string, std::wstring>, 3>{ {
                {"numeric", L"Numeric"}, {"ordered", L"Ordinal"}, {"factor", L"Categorical"} } })
            {
                auto item = Controls::RadioMenuFlyoutItem();
                item.GroupName(to_hstring("mean-type:" + state_.id + ":" + row.variable));
                item.Text(title); item.IsChecked(currentType == type);
                item.Click([send, variable = row.variable, type](auto const&, auto const&) mutable
                    { send("MEAN_SET_VARIABLE_TYPE", variable + '\x1f' + type); });
                types.Items().Append(item);
            }
            menu.Items().Append(types);
        }

        auto remove = Controls::MenuFlyoutItem();
        remove.Text(to_hstring("Remove " + row.variable));
        remove.IsEnabled(state_.specification.dependentVariableIds.size() > 1);
        remove.Click([send, variable = row.variable](auto const&, auto const&) mutable
            { send("MEAN_REMOVE_DEPENDENT", variable); });
        menu.Items().Append(remove);
        auto plots = CreatePlotsMenu(row.variable, {}, L"Plots");
        if (plots.Items().Size() > 0)
        {
            menu.Items().Append(Controls::MenuFlyoutSeparator());
            menu.Items().Append(plots);
        }
        return menu;
    }

    void MeanComparisonView::ConfigureContextMenu()
    {
        auto menu = Controls::MenuFlyout();
        AppendMissingInformationMenu(menu,commandCallback_,state_.id,!state_.provenance.missingInformationRows.empty());
        auto send = [this](std::string command, std::string value = {})
        {
            if (commandCallback_) commandCallback_({ std::move(command), state_.id, std::move(value) });
        };
        auto option = [](Controls::MenuFlyoutSubItem const& parent,
                         std::wstring const& title,
                         bool checked,
                         std::function<void()> action)
        {
            if (checked)
            {
                auto item = Controls::ToggleMenuFlyoutItem();
                item.Text(title);
                item.IsChecked(true);
                item.Click([action = std::move(action)](auto const&, auto const&) { action(); });
                parent.Items().Append(item);
            }
            else
            {
                auto item = Controls::MenuFlyoutItem();
                item.Text(title);
                item.Click([action = std::move(action)](auto const&, auto const&) { action(); });
                parent.Items().Append(item);
            }
        };

        if (descriptivesOnly_)
        {
            if (exporter_)
            {
                auto exports = Controls::MenuFlyoutSubItem(); exports.Text(L"Export");
                for (auto const& table : state_.tables)
                {
                    if (!MeanComparisonDescriptiveTable(table)) continue;
                    const auto tableId = table.tableId;
                    auto tableMenu = exporter_->CreateMenu([this, tableId]()
                    {
                        auto found = std::find_if(state_.tables.begin(), state_.tables.end(),
                            [&](auto const& candidate) { return candidate.tableId == tableId; });
                        return found == state_.tables.end() ? TableExportPayload{} :
                            MeanComparisonExportPayload(state_, *found);
                    });
                    AppendRCodeExportItems(
                        tableMenu, commandCallback_, state_.id);
                    tableMenu.Text(to_hstring(MeanComparisonTableDisplayTitle(state_, table)));
                    exports.Items().Append(tableMenu);
                }
                if (exports.Items().Size() > 0) menu.Items().Append(exports);
            }
            AttachWindowContextFlyout(window_, menu);
            return;
        }

        const bool paired = state_.analysisType == "paired_samples_t_test";
        const bool grouped = state_.analysisType == "independent_samples_t_test" ||
            state_.analysisType == "one_way_anova";
        if (!paired)
        {
            auto variables = Controls::MenuFlyoutSubItem(); variables.Text(L"Variables");
            auto add = Controls::MenuFlyoutSubItem(); add.Text(L"Add dependent variable");
            std::vector<std::string> availableResponses;
            for (auto const& variable : responseVariables_)
            {
                const bool used = std::find(state_.specification.dependentVariableIds.begin(),
                    state_.specification.dependentVariableIds.end(), variable) != state_.specification.dependentVariableIds.end();
                const bool grouping = state_.specification.groupingVariableId &&
                    *state_.specification.groupingVariableId == variable;
                if (used || grouping) continue;
                availableResponses.push_back(variable);
            }
            AppendVariableChoices(add, availableResponses, L"Add dependent variable",
                L"Choose a dependent variable to add.", {},
                [send](std::string const& variable) mutable
                    { send("MEAN_ADD_DEPENDENT", variable); });
            auto replace = Controls::MenuFlyoutSubItem(); replace.Text(L"Replace dependent variable");
            for (auto const& currentVariable : state_.specification.dependentVariableIds)
            {
                auto current = Controls::MenuFlyoutSubItem(); current.Text(to_hstring(currentVariable));
                AppendVariableChoices(current, availableResponses,
                    L"Replace dependent variable",
                    L"Choose the replacement dependent variable.", {},
                    [send, currentVariable](std::string const& candidate) mutable
                    { send("MEAN_REPLACE_DEPENDENT", currentVariable + '\x1f' + candidate); });
                if (current.Items().Size() > 0) replace.Items().Append(current);
            }
            auto remove = Controls::MenuFlyoutSubItem(); remove.Text(L"Remove dependent variable");
            for (auto const& variable : state_.specification.dependentVariableIds)
            {
                auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(variable));
                item.IsEnabled(state_.specification.dependentVariableIds.size() > 1);
                item.Click([send, variable](auto const&, auto const&) mutable
                    { send("MEAN_REMOVE_DEPENDENT", variable); });
                remove.Items().Append(item);
            }
            variables.Items().Append(add);
            if (replace.Items().Size() > 0) variables.Items().Append(replace);
            if (state_.analysisType == "one_sample_t_test" ||
                state_.analysisType == "independent_samples_t_test")
            {
                auto types = Controls::MenuFlyoutSubItem(); types.Text(L"Change type");
                for (auto const& variable : state_.specification.dependentVariableIds)
                {
                    auto current = Controls::MenuFlyoutSubItem(); current.Text(to_hstring(variable));
                    const auto found = variableTypes_.find(variable);
                    const std::string currentType = found == variableTypes_.end() ? "numeric" : found->second;
                    for (auto const& [type, title] : std::array<std::pair<std::string, std::wstring>, 3>{ {
                        {"numeric", L"Numeric"}, {"ordered", L"Ordinal"}, {"factor", L"Categorical"} } })
                    {
                        auto item = Controls::RadioMenuFlyoutItem();
                        item.GroupName(to_hstring("mean-type-menu:" + state_.id + ":" + variable));
                        item.Text(title); item.IsChecked(currentType == type);
                        item.Click([send, variable, type](auto const&, auto const&) mutable
                            { send("MEAN_SET_VARIABLE_TYPE", variable + '\x1f' + type); });
                        current.Items().Append(item);
                    }
                    types.Items().Append(current);
                }
                variables.Items().Append(types);
            }
            variables.Items().Append(remove); menu.Items().Append(variables);
        }
        else
        {
            auto pairs = Controls::MenuFlyoutSubItem(); pairs.Text(L"Pairs");
            auto add = Controls::MenuFlyoutSubItem(); add.Text(L"Add pair");
            if (responseVariables_.size() > kInlineVariableMenuLimit)
            {
                auto choose = Controls::MenuFlyoutItem(); choose.Text(L"Choose pair…");
                choose.Click([this](auto const&, auto const&)
                    { OpenPairChooser(responseVariables_); });
                add.Items().Append(choose);
            }
            else
            {
                for (auto const& first : responseVariables_)
                {
                    auto seconds = Controls::MenuFlyoutSubItem(); seconds.Text(to_hstring(first));
                    for (auto const& second : responseVariables_)
                    {
                        if (first == second) continue;
                        const bool duplicate = std::any_of(state_.specification.pairs.begin(),
                            state_.specification.pairs.end(), [&](auto const& pair)
                            { return pair.firstVariableId == first && pair.secondVariableId == second; });
                        if (duplicate) continue;
                        option(seconds, to_hstring(second).c_str(), false,
                            [send, first, second]() mutable
                            { send("MEAN_ADD_PAIR", first + "\x1f" + second); });
                    }
                    if (seconds.Items().Size() > 0) add.Items().Append(seconds);
                }
            }
            auto replace = Controls::MenuFlyoutSubItem(); replace.Text(L"Replace variable");
            for (auto const& pair : state_.specification.pairs)
            {
                auto current = Controls::MenuFlyoutSubItem();
                current.Text(to_hstring(pair.firstVariableId + " \u2212 " + pair.secondVariableId));
                for (auto const& position : std::array<std::pair<std::string, std::string>, 2>{{
                    {"first", pair.firstVariableId}, {"second", pair.secondVariableId}}})
                {
                    auto side = Controls::MenuFlyoutSubItem();
                    side.Text(to_hstring((position.first == "first" ? "First: " : "Second: ") +
                        position.second));
                    std::vector<std::string> candidates;
                    for (auto const& candidate : responseVariables_)
                    {
                        const auto nextFirst = position.first == "first" ? candidate : pair.firstVariableId;
                        const auto nextSecond = position.first == "second" ? candidate : pair.secondVariableId;
                        const bool duplicate = nextFirst == nextSecond || std::any_of(
                            state_.specification.pairs.begin(), state_.specification.pairs.end(),
                            [&](auto const& other)
                            {
                                return other.pairId != pair.pairId &&
                                    other.firstVariableId == nextFirst &&
                                    other.secondVariableId == nextSecond;
                            });
                        if (candidate == position.second || duplicate) continue;
                        candidates.push_back(candidate);
                    }
                    AppendVariableChoices(side, candidates, L"Replace paired variable",
                        L"Choose the replacement variable.", {},
                        [send, id = pair.pairId, sideName = position.first]
                        (std::string const& candidate) mutable
                        { send("MEAN_REPLACE_PAIR", id + '\x1e' + sideName + '\x1e' + candidate); });
                    if (side.Items().Size() > 0) current.Items().Append(side);
                }
                if (current.Items().Size() > 0) replace.Items().Append(current);
            }
            auto remove = Controls::MenuFlyoutSubItem(); remove.Text(L"Remove pair");
            auto reverse = Controls::MenuFlyoutSubItem(); reverse.Text(L"Reverse pair");
            for (auto const& pair : state_.specification.pairs)
            {
                const auto title = to_hstring(pair.firstVariableId + " \u2212 " + pair.secondVariableId);
                auto removeItem = Controls::MenuFlyoutItem(); removeItem.Text(title);
                removeItem.IsEnabled(state_.specification.pairs.size() > 1);
                removeItem.Click([send, id = pair.pairId](auto const&, auto const&) mutable
                    { send("MEAN_REMOVE_PAIR", id); }); remove.Items().Append(removeItem);
                option(reverse, title.c_str(), false,
                    [send, id = pair.pairId]() mutable { send("MEAN_REVERSE_PAIR", id); });
            }
            pairs.Items().Append(add);
            if (replace.Items().Size() > 0) pairs.Items().Append(replace);
            auto types = Controls::MenuFlyoutSubItem(); types.Text(L"Change type");
            std::vector<std::string> pairVariables;
            for (auto const& pair : state_.specification.pairs)
            {
                if (std::find(pairVariables.begin(), pairVariables.end(), pair.firstVariableId) == pairVariables.end())
                    pairVariables.push_back(pair.firstVariableId);
                if (std::find(pairVariables.begin(), pairVariables.end(), pair.secondVariableId) == pairVariables.end())
                    pairVariables.push_back(pair.secondVariableId);
            }
            for (auto const& variable : pairVariables)
            {
                auto current = Controls::MenuFlyoutSubItem(); current.Text(to_hstring(variable));
                const auto found = variableTypes_.find(variable);
                const std::string currentType = found == variableTypes_.end() ? "numeric" : found->second;
                for (auto const& [type, title] : std::array<std::pair<std::string, std::wstring>, 3>{ {
                    {"numeric", L"Numeric"}, {"ordered", L"Ordinal"}, {"factor", L"Categorical"} } })
                {
                    auto item = Controls::RadioMenuFlyoutItem();
                    item.GroupName(to_hstring("mean-pair-type-menu:" + state_.id + ":" + variable));
                    item.Text(title); item.IsChecked(currentType == type);
                    item.Click([send, variable, type](auto const&, auto const&) mutable
                        { send("MEAN_SET_VARIABLE_TYPE", variable + '\x1f' + type); });
                    current.Items().Append(item);
                }
                types.Items().Append(current);
            }
            pairs.Items().Append(types);
            pairs.Items().Append(remove); pairs.Items().Append(reverse);
            menu.Items().Append(pairs);
        }

        if (grouped)
        {
            auto grouping = Controls::MenuFlyoutSubItem(); grouping.Text(L"Grouping");
            auto groups = Controls::MenuFlyoutSubItem(); groups.Text(L"Grouping variable");
            std::vector<std::string> candidates;
            for (auto const& variable : groupingVariables_)
            {
                if (std::find(state_.specification.dependentVariableIds.begin(),
                    state_.specification.dependentVariableIds.end(), variable) != state_.specification.dependentVariableIds.end()) continue;
                candidates.push_back(variable);
            }
            AppendVariableChoices(groups, candidates, L"Grouping variable",
                L"Choose the variable that defines the groups.",
                state_.specification.groupingVariableId.value_or(""),
                [send](std::string const& variable) mutable
                    { send("MEAN_SET_GROUP", variable); });
            grouping.Items().Append(groups);
            menu.Items().Append(grouping);
            if (state_.analysisType == "independent_samples_t_test" &&
                state_.specification.groupingVariableId)
            {
                const auto found = groupingLevels_.find(
                    *state_.specification.groupingVariableId);
                if (found != groupingLevels_.end() && found->second.size() == 2)
                {
                    auto references = Controls::MenuFlyoutSubItem();
                    references.Text(L"Reference group");
                    for (auto const& level : found->second)
                    {
                        auto item = Controls::RadioMenuFlyoutItem();
                        item.GroupName(to_hstring(
                            "mean-reference:" + state_.id));
                        item.Text(to_hstring(level));
                        item.IsChecked(
                            state_.specification.groupOrderIds.size() == 2 &&
                            state_.specification.groupOrderIds[1] == level);
                        item.Click([send, level](auto const&, auto const&) mutable
                            { send("MEAN_SET_REFERENCE_GROUP", level); });
                        references.Items().Append(item);
                    }
                    menu.Items().Append(references);
                }
            }
        }

        auto test = Controls::MenuFlyoutSubItem(); test.Text(L"Test");
        if (state_.analysisType != "one_sample_t_test")
        {
            auto methods = Controls::MenuFlyoutSubItem(); methods.Text(L"Method");
            std::vector<std::pair<std::string, std::wstring>> methodOptions;
            if (paired)
            methodOptions = {{"student", L"Automatic by pair type — continuous: paired t"},
                {"wilcoxon", L"Automatic by pair type — numeric/ordinal: Wilcoxon"}};
            else if (state_.analysisType == "independent_samples_t_test")
            methodOptions = {{"welch", L"Welch for continuous variables"},
                {"student", L"Student for continuous variables — equal variances"},
                {"mann_whitney", L"Mann–Whitney for numeric and ordinal variables"}};
            else
            methodOptions = {{"welch", L"Welch one-way ANOVA"}, {"classical", L"Classical one-way ANOVA"}, {"kruskal_wallis", L"Kruskal–Wallis"}};
            auto methodId = [&]()
            {
                using M = ::rlispstat::core::MeanComparisonMethod;
                switch (state_.specification.method)
                {
                case M::OneSampleT: case M::StudentT: return std::string("student");
                case M::WilcoxonSignedRank: return std::string("wilcoxon");
                case M::MannWhitney: return std::string("mann_whitney");
                case M::ClassicalAnova: return std::string("classical");
                case M::KruskalWallis: return std::string("kruskal_wallis");
                default: return std::string("welch");
                }
            }();
            for (auto const& [value, title] : methodOptions)
                option(methods, title, value == methodId,
                    [send, value]() mutable { send("MEAN_SET_METHOD", value); });
            test.Items().Append(methods);
        }

        if (state_.analysisType != "one_way_anova")
        {
            auto alternatives = Controls::MenuFlyoutSubItem(); alternatives.Text(L"Alternative");
            const std::string current = state_.specification.alternative == ::rlispstat::core::AlternativeHypothesis::Greater
                ? "greater" : state_.specification.alternative == ::rlispstat::core::AlternativeHypothesis::Less ? "less" : "two.sided";
            for (auto const& [value, title] : std::array<std::pair<std::string, std::wstring>, 3>{{
                {"two.sided", L"Two-sided"}, {"greater", L"Greater"}, {"less", L"Less"}}})
                option(alternatives, title, current == value,
                    [send, value]() mutable { send("MEAN_SET_ALTERNATIVE", value); });
            test.Items().Append(alternatives);
        }
        auto confidence = Controls::MenuFlyoutSubItem(); confidence.Text(L"Confidence level");
        for (auto const level : {0.90, 0.95, 0.99})
            option(confidence, std::to_wstring(static_cast<int>(level * 100)) + L"%",
                std::fabs(state_.specification.confidenceLevel - level) < 1e-9,
                [send, level]() mutable { send("MEAN_SET_CONFIDENCE", std::to_string(level)); });
        test.Items().Append(confidence);
        auto adjustment = Controls::MenuFlyoutSubItem(); adjustment.Text(L"P-value adjustment");
        for (auto const& value : {std::string("none"), std::string("holm"), std::string("bonferroni")})
            option(adjustment, to_hstring(value == "none" ? "None" : value == "holm" ? "Holm" : "Bonferroni").c_str(),
                state_.specification.pAdjustment ==
                    ::rlispstat::core::MeanComparisonAdjustmentFromName(value),
                [send, value]() mutable { send("MEAN_SET_ADJUSTMENT", value); });
        test.Items().Append(adjustment);
        if (state_.analysisType == "one_way_anova" &&
            state_.specification.method !=
                ::rlispstat::core::MeanComparisonMethod::KruskalWallis)
        {
            auto pairwise = Controls::MenuFlyoutSubItem();
            pairwise.Text(L"Pairwise comparisons");
            for (auto const& table : state_.tables)
            {
                if (table.tableId != "omnibus") continue;
                for (auto const& row : table.rows)
                {
                    if (row.variable.empty() || row.originalRowIndices.empty()) continue;
                    auto item = Controls::MenuFlyoutItem();
                    item.Text(to_hstring(row.variable));
                    item.Click([send, response = row.variable](auto const&, auto const&) mutable
                        { send("MEAN_PAIRWISE", response); });
                    pairwise.Items().Append(item);
                }
            }
            if (pairwise.Items().Size() > 0) test.Items().Append(pairwise);
        }
        menu.Items().Append(test);

        auto plots = Controls::MenuFlyoutSubItem(); plots.Text(L"Plots");
        if (paired)
        {
            for (auto const& pair : state_.specification.pairs)
            {
                auto pairPlots = CreatePlotsMenu(pair.firstVariableId,
                    pair.secondVariableId,
                    to_hstring(pair.firstVariableId + " \u2212 " + pair.secondVariableId).c_str());
                if (pairPlots.Items().Size() > 0) plots.Items().Append(pairPlots);
            }
        }
        else
        {
            for (auto const& variable : state_.specification.dependentVariableIds)
            {
                auto variablePlots = CreatePlotsMenu(variable, {}, to_hstring(variable).c_str());
                if (variablePlots.Items().Size() > 0) plots.Items().Append(variablePlots);
            }
        }
        if (plots.Items().Size() > 0) menu.Items().Append(plots);

        auto display = Controls::MenuFlyoutSubItem(); display.Text(L"Display");
        auto descriptives = Controls::ToggleMenuFlyoutItem();
        descriptives.Text(L"Show descriptives"); descriptives.IsChecked(showDescriptives_);
        descriptives.Click([this](auto const&, auto const&)
        {
            showDescriptives_ = !showDescriptives_;
            Render(); ConfigureContextMenu(); ResizeForVisibleContent();
        });
        display.Items().Append(descriptives); menu.Items().Append(display);
        auto openDescriptives = Controls::MenuFlyoutItem();
        openDescriptives.Text(L"Open descriptives window");
        openDescriptives.Click([send](auto const&, auto const&) mutable
            { send("MEAN_OPEN_DESCRIPTIVES"); });
        display.Items().Append(openDescriptives);

        if (exporter_)
        {
            std::vector<std::string> tableIds;
            for (auto const& table : state_.tables)
                if (!MeanComparisonDescriptiveTable(table) || showDescriptives_)
                    tableIds.push_back(table.tableId);
            auto payloadFor = [this](std::string tableId)
            {
                auto found = std::find_if(state_.tables.begin(), state_.tables.end(),
                    [&](auto const& table) { return table.tableId == tableId; });
                if (found != state_.tables.end())
                    return MeanComparisonExportPayload(state_, *found);
                return TableExportPayload{};
            };
            if (tableIds.size() == 1)
            {
                const auto tableId = tableIds.front();
                auto exportMenu = exporter_->CreateMenu(
                    [payloadFor, tableId]() mutable { return payloadFor(tableId); });
                AppendRCodeExportItems(
                    exportMenu, commandCallback_, state_.id);
                menu.Items().Append(exportMenu);
            }
            else if (!tableIds.empty())
            {
                auto exports = Controls::MenuFlyoutSubItem(); exports.Text(L"Export");
                for (auto const& tableId : tableIds)
                {
                    auto found = std::find_if(state_.tables.begin(), state_.tables.end(),
                        [&](auto const& table) { return table.tableId == tableId; });
                    if (found == state_.tables.end()) continue;
                    auto tableMenu = exporter_->CreateMenu(
                        [payloadFor, tableId]() mutable { return payloadFor(tableId); });
                    AppendRCodeExportItems(
                        tableMenu, commandCallback_, state_.id);
                    tableMenu.Text(to_hstring(MeanComparisonTableDisplayTitle(state_, *found)));
                    exports.Items().Append(tableMenu);
                }
                menu.Items().Append(exports);
            }
        }
        AttachWindowContextFlyout(window_, menu);
    }

    void MeanComparisonView::RebuildControls()
    {
        controls_.Children().Clear();
        if (descriptivesOnly_) return;
        updatingControls_ = true;
        auto makeRow = [this]()
        {
            auto row = Controls::StackPanel();
            row.Orientation(Controls::Orientation::Horizontal);
            row.Spacing(8);
            controls_.Children().Append(row);
            return row;
        };
        auto primary = makeRow();
        auto addLabel = [](Controls::StackPanel const& row, std::wstring const& text)
        {
            auto label = FieldLabel(text); label.FontSize(11.5); label.Margin(Thickness{0,0,2,0});
            row.Children().Append(label);
        };
        const bool oneSample = state_.analysisType == "one_sample_t_test";
        if (oneSample)
        {
            addLabel(primary, L"Test value (all variables):");
            auto testValue = Controls::TextBox(); testValue.Width(112); testValue.FontSize(11.5);
            testValue.Text(to_hstring(::rlispstat::core::FormatModelNumberOrDash(
                state_.specification.testValue)));
            auto commit = [this, testValue]()
            {
                if (updatingControls_ || !commandCallback_) return;
                const std::string raw = to_string(testValue.Text());
                char* end = nullptr; const double parsed = std::strtod(raw.c_str(), &end);
                if (!end || *end != '\0' || !std::isfinite(parsed))
                {
                    testValue.Text(to_hstring(::rlispstat::core::FormatModelNumberOrDash(
                    state_.specification.testValue))); return;
                }
                for (auto const& variable : state_.specification.dependentVariableIds)
                {
                    const auto type = variableTypes_.find(variable);
                    if (type != variableTypes_.end() &&
                        (type->second == "factor" || type->second == "logical") &&
                        (parsed < 0.0 || parsed > 1.0))
                    {
                        testValue.Text(to_hstring(::rlispstat::core::FormatModelNumberOrDash(
                            state_.specification.testValue))); return;
                    }
                }
                if (std::fabs(parsed - state_.specification.testValue) < 1e-12) return;
                state_.specification.testValue = parsed; state_.testValue = parsed;
                for (auto const& variable : state_.specification.dependentVariableIds)
                    state_.specification.testValues[variable] = parsed;
                std::ostringstream encoded; encoded << std::setprecision(17) << parsed;
                commandCallback_({"MEAN_SET_ALL_TEST_VALUES", state_.id, encoded.str()});
            };
            testValue.LostFocus([commit](auto const&, auto const&) { commit(); });
            testValue.KeyDown([commit](auto const&, Input::KeyRoutedEventArgs const& args)
            {
                if (args.Key() == Windows::System::VirtualKey::Enter) { commit(); args.Handled(true); }
            });
            primary.Children().Append(testValue);
        }
        else if (state_.analysisType != "independent_samples_t_test" &&
                 state_.analysisType != "paired_samples_t_test")
        {
            addLabel(primary, L"Dependent variable:");
            auto dependent = Controls::ComboBox(); dependent.MinWidth(120); dependent.FontSize(11.5);
            for (auto const& variable : responseVariables_) dependent.Items().Append(box_value(to_hstring(variable)));
            const std::string current = state_.specification.dependentVariableIds.empty()
                ? std::string{} : state_.specification.dependentVariableIds.front();
            auto selected = std::find(responseVariables_.begin(), responseVariables_.end(), current);
            if (selected != responseVariables_.end()) dependent.SelectedIndex(static_cast<int>(selected - responseVariables_.begin()));
            dependent.SelectionChanged([this, dependent](auto const&, auto const&)
            {
                if (updatingControls_ || !commandCallback_ || dependent.SelectedIndex() < 0) return;
                commandCallback_({ "MEAN_SET_DEPENDENT", state_.id,
                    to_string(dependent.SelectedItem().as<hstring>()) });
            });
            primary.Children().Append(dependent);
        }
        const bool grouped = state_.analysisType == "independent_samples_t_test" ||
            state_.analysisType == "one_way_anova";
        if (grouped)
        {
            addLabel(primary, state_.analysisType == "one_way_anova" ? L"Categorical predictor:" : L"Grouping variable:");
            auto grouping = Controls::ComboBox(); grouping.MinWidth(120); grouping.FontSize(11.5);
            for (auto const& variable : groupingVariables_) grouping.Items().Append(box_value(to_hstring(variable)));
            const std::string currentGroup = state_.specification.groupingVariableId.value_or("");
            auto groupSelected = std::find(groupingVariables_.begin(), groupingVariables_.end(), currentGroup);
            if (groupSelected != groupingVariables_.end())
                grouping.SelectedIndex(static_cast<int>(groupSelected - groupingVariables_.begin()));
            grouping.SelectionChanged([this, grouping](auto const&, auto const&)
            {
                if (updatingControls_ || !commandCallback_ || grouping.SelectedIndex() < 0) return;
                commandCallback_({ "MEAN_SET_GROUP", state_.id,
                    to_string(grouping.SelectedItem().as<hstring>()) });
            });
            primary.Children().Append(grouping);
        }

        auto options = primary;

        addLabel(options, L"Scope:");
        auto scope = Controls::ComboBox(); scope.MinWidth(170); scope.FontSize(11.5);
        auto choices = scopeChoices_;
        if (choices.empty())
            choices = ::rlispstat::core::BuildAnalysisScopeChoices(
                selectedRowCount_, {}, false);
        PopulateAnalysisScopeCombo(scope, choices,
            ::rlispstat::core::AnalysisScopeChoiceValue(
                "all", state_.dataScope, state_.dataScopeCaptured));
        scope.SelectionChanged([this, scope](auto const&, auto const&)
        {
            if (updatingControls_ || !commandCallback_ || scope.SelectedIndex() < 0) return;
            commandCallback_({"MEAN_SET_SCOPE", state_.id,
                SelectedAnalysisScopeChoice(scope)});
        });
        options.Children().Append(scope);
        updatingControls_ = false;
    }

    static std::string MeanComparisonDisplaySubtitle(
        ::rlispstat::core::MeanComparisonState const& state)
    {
        std::string subtitle = state.subtitle;
        if (state.analysisType == "independent_samples_t_test" &&
            state.specification.groupOrderIds.size() == 2)
        {
            const std::string contrast = "Difference: " +
                state.specification.groupOrderIds[0] + " \xe2\x88\x92 " +
                state.specification.groupOrderIds[1];
            if (subtitle.find(contrast) == std::string::npos)
                subtitle += (subtitle.empty() ? "" : " \xc2\xb7 ") + contrast;
        }
        if (state.dataScopeCaptured)
        {
            const auto scope = ::rlispstat::core::AnalysisScopeWindowSummary(
                state.dataScope, state.dataScope.totalDatasetRows);
            if (!scope.empty() && subtitle.find(scope) == std::string::npos)
                subtitle += (subtitle.empty() ? "" : " \xc2\xb7 ") + scope;
        }
        return subtitle;
    }

    void MeanComparisonView::ShowPending(::rlispstat::core::MeanComparisonState const& state)
    {
        state_ = state;
        const auto detail = MeanComparisonDisplaySubtitle(state);
        const auto status = detail.empty()
            ? std::string("Updating…") : detail + " · Updating…";
        subtitle_.Text(to_hstring(status));
        RebuildControls();
    }

    void MeanComparisonView::ResizeForVisibleContent()
    {
        const double desiredWidth = std::clamp(
            ::rlispstat::core::MeanComparisonPreferredWidth(state_) + 80.0,
            kResultWindowMinimumWidth, kWideResultWindowMaximumWidth);
        const double desiredHeight = std::clamp(
            ::rlispstat::core::MeanComparisonVisiblePreferredHeight(
                state_, showDescriptives_, descriptivesOnly_) + 120.0 +
                (descriptivesOnly_ ? 0.0 : 32.0),
            360.0, 900.0);
        PresentOrResizeAnalysisWindow(
            window_, presented_, desiredWidth, desiredHeight);
    }

    void MeanComparisonView::Show(::rlispstat::core::MeanComparisonState const& state)
    {
        ::rlispstat::windows::performance::Scope timing("Output.MeanComparison.Show");
        SetWindowDataSheetGroup(window_, state.datasetId);
        SetWindowSnapshotSource(window_, "output", state.id, state.datasetId);
        state_=state;
        const std::string visibleTitle = descriptivesOnly_
            ? state.title + " — Descriptives" : state.title;
        window_.Title(to_hstring(visibleTitle)); title_.Text(to_hstring(visibleTitle));
        badge_.Text(to_hstring(state.datasetId));
        subtitle_.Text(to_hstring(MeanComparisonDisplaySubtitle(state)));
        RebuildControls(); Render(); ConfigureContextMenu();
        ResizeForVisibleContent();
    }

    void MeanComparisonView::Render()
    {
        using namespace ::rlispstat::core;
        content_.Children().Clear();
        if (state_.tables.empty() && !state_.subtitle.empty())
        {
            auto prompt = Controls::TextBlock();
            prompt.Text(to_hstring(state_.subtitle));
            prompt.FontSize(14.0);
            prompt.Foreground(Brush(88, 96, 105));
            prompt.TextWrapping(TextWrapping::Wrap);
            prompt.Margin(Thickness{ 8, 18, 8, 8 });
            content_.Children().Append(prompt);
        }
        for(auto const& table:state_.tables)
        {
            const bool descriptive = MeanComparisonDescriptiveTable(table);
            if ((descriptivesOnly_ && !descriptive) ||
                (!descriptivesOnly_ && descriptive && !showDescriptives_)) continue;
            if (!table.title.empty())
            {
                auto tableTitle=FieldLabel(to_hstring(table.title).c_str()); tableTitle.FontSize(13);
                tableTitle.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
                content_.Children().Append(tableTitle);
            }
            auto grid=Controls::Grid();
            auto stub=Controls::ColumnDefinition(); stub.Width(GridLengthHelper::FromPixels(MeanComparisonPreferredStubWidth(table)));
            grid.ColumnDefinitions().Append(stub);
            std::vector<std::size_t> visible;
            const auto widths=MeanComparisonPreferredColumnWidths(table);
            for(std::size_t index=0;index<table.columns.size();++index) if(table.columns[index].visible)
            {
                visible.push_back(index); auto column=Controls::ColumnDefinition();
                column.Width(GridLengthHelper::FromPixels(index<widths.size()?widths[index]:92.0));
                grid.ColumnDefinitions().Append(column);
            }
            auto addRow=[&](){ auto row=Controls::RowDefinition();
                row.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Auto)); grid.RowDefinitions().Append(row); };
            auto add=[&](FrameworkElement const& cell,int row,int column){ Controls::Grid::SetRow(cell,row);
                Controls::Grid::SetColumn(cell,column); grid.Children().Append(cell); };
            addRow();auto stubHeader=TableCell(to_hstring(table.stubTitle).c_str(),true,true);add(stubHeader,0,0);AttachDirectMenu(stubHeader,CreateStructuralCellMenu(to_hstring(table.stubTitle).c_str(),stubHeader.Child().as<Controls::TextBlock>()));
            const std::string explanationMethod = !table.method.empty() ? table.method
                : (!table.title.empty() ? table.title : state_.method);
            const std::string explanationAdjustment = MeanComparisonAdjustmentName(table.pAdjustment);
            auto explanationContext = [&](std::string statistic)
            {
                return AnalysisStatisticContext(std::move(statistic),
                    state_.analysisType.empty() ? "mean comparison" : state_.analysisType,
                    explanationMethod,
                    explanationAdjustment == "none" ? std::string{} : explanationAdjustment,
                    state_.multipleImputation,state_.poolingMethod);
            };
            int column=1;for(auto index:visible){auto header=TableCell(to_hstring(table.columns[index].title).c_str(),true,false);add(header,0,column++);AttachDirectMenu(header,table.columns[index].format==MeanComparisonCellFormat::Text?CreateStructuralCellMenu(to_hstring(table.columns[index].title).c_str(),header.Child().as<Controls::TextBlock>()):CreateStatisticCellMenu(to_hstring(table.columns[index].title).c_str(),header.Child().as<Controls::TextBlock>(),explanationContext(table.columns[index].title)));}
            int row=1;
            for(auto const& result:table.rows)
            {
                addRow(); const bool warning=result.kind==MeanComparisonRowKind::Warning;
                auto variableCell = TableCell(to_hstring(result.label).c_str(),false,true,
                    result.kind==MeanComparisonRowKind::Result || result.kind==MeanComparisonRowKind::GroupHeader,warning,false,row>1);
                if(result.kind==MeanComparisonRowKind::Descriptive && !result.group.empty())
                    variableCell.Padding(Thickness{24,6,8,6});
                const bool directVariable = !result.variable.empty() &&
                    (state_.analysisType != "paired_samples_t_test" ||
                     result.kind == MeanComparisonRowKind::Result);
                if (directVariable)
                    AttachDirectMenu(variableCell, CreateVariableMenu(result));
                else AttachDirectMenu(variableCell,CreateStructuralCellMenu(to_hstring(result.label).c_str(),variableCell.Child().as<Controls::TextBlock>()));
                add(variableCell,row,0);
                column=1; for(auto index:visible)
                {
                    const std::string value=index<result.cells.size()
                        ? MeanComparisonCellText(result.cells[index],table.columns[index].format):std::string{};
                    const bool editableTestValue = state_.analysisType == "one_sample_t_test" &&
                        result.kind == MeanComparisonRowKind::Result &&
                        table.columns[index].key == "test_value" && !result.variable.empty();
                    if (editableTestValue)
                    {
                        auto editor = Controls::TextBox(); editor.Text(to_hstring(value)); editor.FontSize(13);
                        editor.TextAlignment(TextAlignment::Right); editor.MinHeight(28);
                        editor.Padding(Thickness{10,1,10,1}); editor.BorderThickness(Thickness{0});
                        editor.Background(Brush(255,255,255));
                        auto commit = [this, editor, variable = result.variable]()
                        {
                            if (!commandCallback_) return;
                            const std::string raw = to_string(editor.Text());
                            char* end = nullptr; const double parsed = std::strtod(raw.c_str(), &end);
                            const auto current = state_.specification.testValues.find(variable);
                            const double previous = current == state_.specification.testValues.end()
                                ? state_.specification.testValue : current->second;
                            if (!end || *end != '\0' || !std::isfinite(parsed))
                            {
                                editor.Text(to_hstring(::rlispstat::core::FormatModelNumberOrDash(previous)));
                                return;
                            }
                            const auto type = variableTypes_.find(variable);
                            if (type != variableTypes_.end() &&
                                (type->second == "factor" || type->second == "logical") &&
                                (parsed < 0.0 || parsed > 1.0))
                            {
                                editor.Text(to_hstring(::rlispstat::core::FormatModelNumberOrDash(previous)));
                                return;
                            }
                            if (std::fabs(previous - parsed) < 1e-12) return;
                            state_.specification.testValues[variable] = parsed;
                            std::ostringstream encoded; encoded << variable << '\x1f'
                                << std::setprecision(17) << parsed;
                            commandCallback_({"MEAN_SET_TEST_VALUE", state_.id, encoded.str()});
                        };
                        editor.LostFocus([commit](auto const&, auto const&) { commit(); });
                        editor.KeyDown([commit](auto const&, Input::KeyRoutedEventArgs const& args)
                        {
                            if (args.Key() == Windows::System::VirtualKey::Enter)
                            { commit(); args.Handled(true); }
                        });
                        add(editor,row,column++);
                    }
                    else{auto valueCell=TableCell(to_hstring(value).c_str(),false,false,false,warning,false,row>1);add(valueCell,row,column++);AttachDirectMenu(valueCell,table.columns[index].format==MeanComparisonCellFormat::Text?CreateStructuralCellMenu(to_hstring(table.columns[index].title).c_str(),valueCell.Child().as<Controls::TextBlock>()):CreateStatisticCellMenu(to_hstring(table.columns[index].title).c_str(),valueCell.Child().as<Controls::TextBlock>(),explanationContext(table.columns[index].title)));}
                }
                ++row;
            }
            content_.Children().Append(grid);
            for(auto const& note:table.notes){ auto text=FieldLabel(to_hstring(note).c_str());
                text.FontSize(11); text.Foreground(Brush(100,100,100)); text.TextWrapping(TextWrapping::Wrap); content_.Children().Append(text); }
            for(auto const& warning:table.warnings){ auto text=FieldLabel(to_hstring("Warning: "+warning).c_str());
                text.FontSize(11); text.Foreground(Brush(145,54,45)); text.TextWrapping(TextWrapping::Wrap); content_.Children().Append(text); }
        }
        if (!descriptivesOnly_)
        {
            const bool paired = state_.analysisType == "paired_samples_t_test";
            auto addVariable = Controls::Button();
            addVariable.Content(box_value(paired ? L"+ Add pair" : L"+ Add variable"));
            addVariable.FontSize(11.5);
            addVariable.Foreground(Brush(105, 105, 105));
            addVariable.HorizontalAlignment(HorizontalAlignment::Left);
            addVariable.Background(TransparentBrush());
            addVariable.BorderBrush(TransparentBrush());
            addVariable.BorderThickness(Thickness{0,0,0,0});
            addVariable.Padding(Thickness{8,4,8,4});
            if (paired)
            {
                addVariable.IsEnabled(responseVariables_.size() >= 2);
                addVariable.Click([this](auto const&, auto const&)
                {
                    OpenPairChooser(responseVariables_);
                });
            }
            else
            {
                auto addMenu = Controls::MenuFlyout();
                for (auto const& variable : responseVariables_)
                {
                    const bool included = std::find(
                        state_.specification.dependentVariableIds.begin(),
                        state_.specification.dependentVariableIds.end(), variable) !=
                        state_.specification.dependentVariableIds.end();
                    const bool grouping = state_.specification.groupingVariableId &&
                        *state_.specification.groupingVariableId == variable;
                    if (included || grouping) continue;
                    auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(variable));
                    item.Click([this, variable](auto const&, auto const&)
                    {
                        if (commandCallback_)
                            commandCallback_({"MEAN_ADD_DEPENDENT", state_.id, variable});
                    });
                    addMenu.Items().Append(item);
                }
                if (addMenu.Items().Size() == 0)
                {
                    auto empty = Controls::MenuFlyoutItem();
                    empty.Text(L"No variables available"); empty.IsEnabled(false);
                    addMenu.Items().Append(empty);
                }
                addVariable.Flyout(addMenu);
            }
            content_.Children().Append(addVariable);
        }
        for(auto const& warning:state_.warnings){ auto text=FieldLabel(to_hstring("Warning: "+warning).c_str());
            text.FontSize(11); text.Foreground(Brush(145,54,45)); text.TextWrapping(TextWrapping::Wrap); content_.Children().Append(text); }
    }
    void MeanComparisonView::Activate(){ if(!closed_) window_.Activate(); }
    void MeanComparisonView::Close(){ if(!closed_) window_.Close(); }

    std::shared_ptr<LinearModelView> LinearModelView::Create()
    {
        auto view=std::shared_ptr<LinearModelView>(new LinearModelView()); view->AttachLifetime(); return view;
    }
    LinearModelView::LinearModelView(){ Initialize(); }
    void LinearModelView::Initialize()
    {
        window_=CreateLinkEDAWindow(); window_.Title(L"Linear Model");
        root_=Controls::Grid(); root_.Padding(Thickness{18,14,18,12}); root_.RowSpacing(5);
        root_.Background(Brush(255,255,255)); root_.RequestedTheme(ElementTheme::Light);
        auto autoRow=[](){auto row=Controls::RowDefinition();row.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Auto));return row;};
        root_.RowDefinitions().Append(autoRow()); root_.RowDefinitions().Append(autoRow());
        auto body=Controls::RowDefinition();body.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Star));root_.RowDefinitions().Append(body);
        auto heading=Controls::Grid();auto grow=Controls::ColumnDefinition();grow.Width(GridLengthHelper::FromValueAndType(1,GridUnitType::Star));
        heading.ColumnDefinitions().Append(grow);heading.ColumnDefinitions().Append(Controls::ColumnDefinition());
        title_=Controls::TextBlock();title_.Text(L"Linear Model");title_.FontSize(17);title_.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        auto headingActions=Controls::StackPanel();headingActions.Orientation(Controls::Orientation::Horizontal);headingActions.Spacing(16);
        title_.VerticalAlignment(VerticalAlignment::Center);headingActions.Children().Append(title_);
        modelActionsButton_=Controls::Button();modelActionsButton_.Content(box_value(L"Model ▾"));
        headingActions.Children().Append(modelActionsButton_);heading.Children().Append(headingActions);
        badge_=Controls::TextBlock();badge_.FontSize(12);badge_.Foreground(Brush(72,72,72));badge_.VerticalAlignment(VerticalAlignment::Center);Controls::Grid::SetColumn(badge_,1);heading.Children().Append(badge_);root_.Children().Append(heading);
        controls_=Controls::StackPanel();controls_.Orientation(Controls::Orientation::Horizontal);
        controls_.Spacing(8);controls_.Margin(Thickness{0,5,0,7});
        Controls::Grid::SetRow(controls_,1);root_.Children().Append(controls_);
        auto scroll=Controls::ScrollViewer();scroll.HorizontalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);scroll.VerticalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);
        scroll.HorizontalContentAlignment(HorizontalAlignment::Stretch);
        content_=Controls::StackPanel();content_.Name(L"LinkEDA.SnapshotTable");content_.Spacing(8);
        content_.HorizontalAlignment(HorizontalAlignment::Stretch);
        scroll.Content(content_);Controls::Grid::SetRow(scroll,2);root_.Children().Append(scroll);
        window_.Content(root_);
        exporter_=TableExportService::Create(window_,root_);
        ResizeLogical(window_,760,560);
    }
    void LinearModelView::AttachLifetime()
    {
        std::weak_ptr<LinearModelView> weak=shared_from_this();window_.Closed([weak](auto const&,auto const&){if(auto view=weak.lock()){view->closed_=true;if(view->closedCallback_)view->closedCallback_();}});
    }
    void LinearModelView::SetClosedCallback(Closed callback){closedCallback_=std::move(callback);}
    void LinearModelView::SetDiagnosticCallback(Diagnostic callback)
    {
        diagnosticCallback_=std::move(callback);
    }
    void LinearModelView::SetCommandCallback(
        Command callback, std::vector<std::string> numericVariables,
        std::vector<std::string> availableVariables)
    {
        commandCallback_ = std::move(callback);
        commandChoicesChanged_ = numericVariables_ != numericVariables ||
            availableVariables_ != availableVariables;
        numericVariables_ = std::move(numericVariables);
        availableVariables_ = std::move(availableVariables);
    }

    void LinearModelView::SetScopeChoices(
        std::vector<::rlispstat::core::AnalysisScopeChoice> choices)
    {
        scopeChoices_ = std::move(choices);
        commandChoicesChanged_ = true;
        RebuildControls();
    }
    void LinearModelView::ConfigureContextMenu()
    {
        auto menu=Controls::MenuFlyout();
        const std::string modelId = model_.modelId.empty() ? model_.group : model_.modelId;
        AppendMissingInformationMenu(menu,commandCallback_,"glm:"+modelId,!model_.provenance.missingInformationRows.empty());
        auto send=[this](std::string command,std::string value={})
        { if(commandCallback_) commandCallback_({std::move(command),model_.modelId.empty()?model_.group:model_.modelId,std::move(value)}); };
        if(!model_.precomputed || model_.multipleImputation)
        {
            auto response=Controls::MenuFlyoutSubItem();response.Text(L"Response variable");
            for(auto const& variable:numericVariables_){auto item=Controls::ToggleMenuFlyoutItem();item.Text(to_hstring(variable));item.IsChecked(variable==model_.response);item.Click([send,variable](auto const&,auto const&)mutable{send("MODEL_SET_Y",variable);});response.Items().Append(item);}menu.Items().Append(response);
            auto add=Controls::MenuFlyoutSubItem();add.Text(L"Add term");
            for(auto const& variable:availableVariables_)if(variable!=model_.response&&std::find(model_.terms.begin(),model_.terms.end(),variable)==model_.terms.end()){auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(variable));item.Click([send,variable](auto const&,auto const&)mutable{send("MODEL_ADD_TERM",variable);});add.Items().Append(item);}
            AppendPolynomialMenu(add, PolynomialNumericVariables(numericVariables_, model_), model_.response, model_.terms,
                [send](std::string const& term) mutable { send("MODEL_ADD_TERM", term); });
            const auto interactionCandidates=::rlispstat::core::InteractionCandidateTermsForModel(availableVariables_,model_.response,model_.terms,model_.terms);
            if(auto interactions=BuildInteractionMenuSubItem(interactionCandidates,[send](std::string const& term)mutable{send("MODEL_ADD_TERM",term);})){add.Items().Append(interactions);}
            if(add.Items().Size()>0)menu.Items().Append(add);
            if(!model_.terms.empty()){auto remove=Controls::MenuFlyoutSubItem();remove.Text(L"Remove term");for(auto const& term:model_.terms){auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(term));item.Click([send,term](auto const&,auto const&)mutable{send("MODEL_REMOVE_TERM",term);});remove.Items().Append(item);}menu.Items().Append(remove);
                auto types=Controls::MenuFlyoutSubItem();types.Text(L"Predictor type");for(auto const& term:model_.terms){if(term.find(':')!=std::string::npos)continue;auto termMenu=Controls::MenuFlyoutSubItem();termMenu.Text(to_hstring(term));Controls::RadioMenuFlyoutItem::SetAreCheckStatesEnabled(termMenu,true);const auto found=model_.termTypes.find(term);const std::string current=found==model_.termTypes.end()?"numeric":found->second;for(auto const& type:{std::string("numeric"),std::string("factor")}){auto item=Controls::RadioMenuFlyoutItem();item.GroupName(to_hstring("linear-type:"+(model_.modelId.empty()?model_.group:model_.modelId)+":"+term));item.Text(to_hstring(type=="numeric"?"Treat predictor as continuous":"Treat predictor as categorical"));item.IsChecked(current==type);item.Click([send,term,type](auto const&,auto const&)mutable{send("MODEL_SET_TYPE",term+"\x1f"+type);});termMenu.Items().Append(item);}types.Items().Append(termMenu);}if(types.Items().Size()>0)menu.Items().Append(types);}
                auto transformations=Controls::MenuFlyoutSubItem();transformations.Text(L"Predictor transformations");
                for(auto const& term:model_.terms)
                {
                    if(term.find(':')!=std::string::npos)continue;
                    const auto found=model_.termTypes.find(term);
                    const std::string type=found==model_.termTypes.end()?"numeric":found->second;
                    if(type=="factor"||type=="ordered")continue;
                    auto item=Controls::ToggleMenuFlyoutItem();item.Text(to_hstring(term));
                    item.IsChecked(model_.centeredPredictors.count(term)!=0);
                    item.Click([send,term](auto const&,auto const&)mutable{send("MODEL_TOGGLE_CENTER",term);});
                    transformations.Items().Append(item);
                }
                if(transformations.Items().Size()>0)menu.Items().Append(transformations);
                auto references=Controls::MenuFlyoutSubItem();references.Text(L"Reference category");
                for(auto const& term:model_.terms)
                {
                    if(term.find(':')!=std::string::npos)continue;
                    const auto found=model_.termTypes.find(term);
                    const std::string type=found==model_.termTypes.end()?"numeric":found->second;
                    if(type!="factor"&&type!="ordered")continue;
                    const auto coding=std::find_if(fit_.factorCodings.begin(),fit_.factorCodings.end(),
                        [&](auto const& value){return value.variable==term;});
                    if(coding==fit_.factorCodings.end()||coding->levels.empty())continue;
                    auto termMenu=Controls::MenuFlyoutSubItem();termMenu.Text(to_hstring(term));
                    Controls::RadioMenuFlyoutItem::SetAreCheckStatesEnabled(termMenu,true);
                    for(auto const& level:coding->levels)
                    {
                        auto item=Controls::RadioMenuFlyoutItem();
                        item.GroupName(to_hstring("linear-reference:"+(model_.modelId.empty()?model_.group:model_.modelId)+":"+term));
                        item.Text(to_hstring(level));item.IsChecked(level==coding->referenceLevel);
                        item.Click([send,term,level](auto const&,auto const&)mutable
                        {send("MODEL_SET_REFERENCE",term+'\x1f'+level);});
                        termMenu.Items().Append(item);
                    }
                    references.Items().Append(termMenu);
                }
                if(references.Items().Size()>0)menu.Items().Append(references);
            }

            menu.Items().Append(Controls::MenuFlyoutSeparator());
            auto refit=Controls::MenuFlyoutItem();refit.Text(L"Refit model");refit.Click([send](auto const&,auto const&)mutable{send("MODEL_REFIT","");});menu.Items().Append(refit);
        }
        if(!model_.response.empty())
        {
            auto compare=Controls::MenuFlyoutItem();
            compare.Text(to_hstring(::rlispstat::core::DefaultModelContextMenuTitles().compareModels));
            compare.Click([send](auto const&,auto const&)mutable
            { send("MODEL_COMPARE_MODELS",""); });
            menu.Items().Append(compare);
        }
        auto diagnostics=Controls::MenuFlyoutSubItem();diagnostics.Text(to_hstring(::rlispstat::core::DefaultModelContextMenuTitles().openDiagnostics));
        std::weak_ptr<LinearModelView> weak=shared_from_this();for(auto const& option : ::rlispstat::core::ModelDiagnosticPlotOptions(true)){auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(option.title));item.Click([weak,kind=option.value](auto const&,auto const&){if(auto view=weak.lock())if(view->diagnosticCallback_)view->diagnosticCallback_(kind);});diagnostics.Items().Append(item);}menu.Items().Append(diagnostics);
        if(exporter_)
        {
            menu.Items().Append(Controls::MenuFlyoutSeparator());
            auto exportMenu = exporter_->CreateMenu([this]()
            {
                return LinearModelExportPayload(model_,fit_);
            });
            AppendRCodeExportItems(exportMenu, commandCallback_, "glm:" + modelId);
            menu.Items().Append(exportMenu);
        }
        modelActionsButton_.Flyout(menu);
        AttachWindowContextFlyout(window_, menu);
    }

    void LinearModelView::RebuildControls()
    {
        controls_.Children().Clear();
        auto label = [](std::wstring const& text)
        {
            auto value = FieldLabel(text); value.FontSize(11.5); return value;
        };
        controls_.Children().Append(label(L"Response variable:"));
        auto response = Controls::ComboBox(); response.MinWidth(110); response.FontSize(11.5);
        for (auto const& variable : numericVariables_) response.Items().Append(box_value(to_hstring(variable)));
        auto selected = std::find(numericVariables_.begin(), numericVariables_.end(), model_.response);
        if (selected != numericVariables_.end()) response.SelectedIndex(static_cast<int>(selected - numericVariables_.begin()));
        response.SelectionChanged([this, response](auto const&, auto const&)
        {
            if (!commandCallback_ || response.SelectedIndex() < 0) return;
            const std::string variable = to_string(response.SelectedItem().as<hstring>());
            if (variable != model_.response) commandCallback_({ "MODEL_SET_Y", model_.modelId.empty()?model_.group:model_.modelId, variable });
        });
        controls_.Children().Append(response);

        auto scopeLabel=model_.scope;if(model_.dataScopeCaptured&&!model_.dataScope.sourceDescription.empty())scopeLabel=model_.dataScope.sourceDescription;controls_.Children().Append(label(to_hstring("Scope: "+scopeLabel).c_str()));
    }
    void LinearModelView::Show(::rlispstat::core::GroupModelState const& model,::rlispstat::core::GLMFitSummary const& fit)
    {
        ::rlispstat::windows::performance::Scope timing("Output.LinearModel.Show");
        SetWindowDataSheetGroup(window_, model.group);
        SetWindowSnapshotSource(window_, "output",
            model.modelId.empty() ? model.group : model.modelId, model.group);
        auto effectiveModel = model;
        effectiveModel.termTypes =
            ::rlispstat::core::EffectiveModelSpecificationTermTypes(model);
        std::ostringstream signature;
        signature << effectiveModel.group << '\x1f' << effectiveModel.response << '\x1f' << effectiveModel.scope
                  << '\x1f' << effectiveModel.precomputed;
        for (auto const& term : effectiveModel.terms) signature << '\x1e' << term;
        for (auto const& [term, type] : effectiveModel.termTypes)
            signature << '\x1d' << term << '=' << type;
        for (auto const& term : effectiveModel.centeredPredictors)
            signature << '\x1c' << term;
        for (auto const& [term, level] : effectiveModel.factorReferenceLevels)
            signature << '\x1b' << term << '=' << level;
        const std::string nextControlsSignature = signature.str();
        const bool scopeNoticeChanged = model.frozenScopeNotice != model_.frozenScopeNotice;
        const bool controlsChanged = !reportInitialized_ || commandChoicesChanged_ ||
            controlsSignature_ != nextControlsSignature;
        model_=effectiveModel;
        fit_=fit;
        window_.Title(to_hstring(model.title.empty()?"Linear Model":model.title));title_.Text(to_hstring(model.title.empty()?"Linear Model":model.title));badge_.Text(to_hstring(model.group));
        if (controlsChanged)
        {
            ::rlispstat::windows::performance::Scope controlsTiming(
                "Output.LinearModel.Controls.Rebuild");
            RebuildControls();
            ConfigureContextMenu();
            controlsSignature_ = nextControlsSignature;
            commandChoicesChanged_ = false;
        }
        // Keep the last completed report visible while R is fitting. Pending
        // state remains internal and does not cause a second visual refresh.
        if (!reportInitialized_ || scopeNoticeChanged || (!model.rFitPending && !model.isStale &&
            (model.fitVersion != renderedFitVersion_ || fit.ok != renderedFitOk_ ||
             fit.warning != renderedWarning_)))
            Render(effectiveModel,fit);
        // Let the live semantic row count determine the requested height.
        // ResizeLinkEDAWindowClient already caps the client area to the
        // monitor work area, where the report ScrollViewer takes over.
        const double desiredHeight = std::max(
            500.0, 357.0 + fit.coefficients.size() * 27.0);
        PresentOrResizeAnalysisWindow(window_, presented_,
                                      kModelResultWindowWidth, desiredHeight);
        windowCoefficientRows_ = fit.coefficients.size();
    }
    void LinearModelView::InitializeReport()
    {
        if (reportInitialized_) return;
        content_.Children().Clear();
        auto section=[this](std::wstring const& text)
        {
            auto label=FieldLabel(text);label.FontSize(13);
            label.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
            content_.Children().Append(label);
        };
        auto addRow=[](Controls::Grid const& grid)
        {
            auto row=Controls::RowDefinition();
            row.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Auto));
            grid.RowDefinitions().Append(row);
        };
        auto addCell=[](Controls::Grid const& grid,std::wstring const& text,
                        int row,int column,bool header,bool label)
        {
            auto border=TableCell(text,header,label);
            Controls::Grid::SetRow(border,row);Controls::Grid::SetColumn(border,column);
            grid.Children().Append(border);
            return border.Child().as<Controls::TextBlock>();
        };

        section(L"Global fit");
        fitSummary_=FieldLabel(L"");fitSummary_.FontSize(11.5);
        content_.Children().Append(fitSummary_);
        anovaGrid_=Controls::Grid();
        ApplyGLMResultTableLayout(anovaGrid_,
            ::rlispstat::core::GLMAnovaResultTableLayout());
        addRow(anovaGrid_);
        const std::array<std::wstring,6> headers{
            L"Source",L"Sum of Squares",L"df",L"Mean Square",L"F-ratio",L"p"};
        for(int column=0;column<6;++column)
        {
            auto text=addCell(anovaGrid_,headers[static_cast<std::size_t>(column)],0,column,true,column==0);
            auto target=text.Parent().try_as<FrameworkElement>();
            AttachDirectMenu(target?target:text.as<FrameworkElement>(),column==0?
                CreateStructuralCellMenu(headers[static_cast<std::size_t>(column)],text):
                CreateStatisticCellMenu(headers[static_cast<std::size_t>(column)],text,
                    LinearStatisticContext(model_, to_string(hstring(headers[static_cast<std::size_t>(column)])))));
        }
        for(int row=1;row<=2;++row)
        {
            addRow(anovaGrid_);
            for(int column=0;column<6;++column)
            {
                auto text=addCell(anovaGrid_,L"",row,column,false,column==0);
                anovaCells_.push_back(text);auto target=text.Parent().try_as<FrameworkElement>();
                AttachDirectMenu(target?target:text.as<FrameworkElement>(),column==0?
                    CreateStructuralCellMenu(L"Source",text):
                    CreateStatisticCellMenu(headers[static_cast<std::size_t>(column)],text,
                        LinearStatisticContext(model_, to_string(hstring(headers[static_cast<std::size_t>(column)])))));
            }
        }
        content_.Children().Append(anovaGrid_);
        section(L"Terms");
        termsGrid_=Controls::Grid();content_.Children().Append(termsGrid_);
        footer_=FieldLabel(L"");footer_.FontSize(11);footer_.Foreground(Brush(100,100,100));
        content_.Children().Append(footer_);
        note_=FieldLabel(L"");note_.FontSize(11);note_.Foreground(Brush(100,100,100));
        note_.TextWrapping(TextWrapping::Wrap);note_.Visibility(Visibility::Collapsed);
        content_.Children().Append(note_);
        warning_=FieldLabel(L"");warning_.FontSize(11);warning_.TextWrapping(TextWrapping::Wrap);
        warning_.Visibility(Visibility::Collapsed);content_.Children().Append(warning_);
        reportInitialized_=true;
    }

    void LinearModelView::RebuildTermRows(::rlispstat::core::GLMFitSummary const& fit)
    {
        ::rlispstat::windows::performance::Scope timing(
            "Output.LinearModel.TermStructure.Rebuild");
        termsGrid_.Children().Clear();termsGrid_.ColumnDefinitions().Clear();
        termsGrid_.RowDefinitions().Clear();termCells_.clear();termRowStructure_.clear();
        const std::array<std::wstring,9> headers{
            L"Variable",L"Type",L"b",L"\u03B2",L"SE",L"t",L"p",L"Partial r",L"\u0394R\u00B2"};
        ApplyGLMResultTableLayout(termsGrid_,
            ::rlispstat::core::GLMTermsResultTableLayout());
        auto addRow=[this](){auto row=Controls::RowDefinition();row.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Auto));termsGrid_.RowDefinitions().Append(row);};
        auto addCell=[this](std::wstring const& text,int row,int column,bool header,bool label,bool bold=false)
        {
            auto border=TableCell(text,header,label,bold,false,false,row>1);
            Controls::Grid::SetRow(border,row);Controls::Grid::SetColumn(border,column);
            termsGrid_.Children().Append(border);return border.Child().as<Controls::TextBlock>();
        };
        auto targetFor=[](Controls::TextBlock const& text)
        {
            auto parent=text.Parent().try_as<FrameworkElement>();
            return parent?parent:text.as<FrameworkElement>();
        };
        auto send=[this](std::string const& command,std::string const& value)
        {
            if(commandCallback_)commandCallback_({command,model_.modelId.empty()?model_.group:model_.modelId,value});
        };
        auto typeMenu=[this,send](std::string const& term)
        {
            auto menu=Controls::MenuFlyout();
            auto heading=Controls::MenuFlyoutItem();heading.Text(to_hstring(term));heading.IsEnabled(false);
            menu.Items().Append(heading);menu.Items().Append(Controls::MenuFlyoutSeparator());
            const auto found=model_.termTypes.find(term);
            const std::string current=found==model_.termTypes.end()?"numeric":found->second;
            for(auto const& entry:std::vector<std::pair<std::string,std::wstring>>{{"numeric",L"Treat predictor as continuous"},{"factor",L"Treat predictor as categorical"}})
            {
                auto item=Controls::RadioMenuFlyoutItem();
                item.GroupName(to_hstring("linear-cell-type:"+model_.group+":"+term));
                item.Text(entry.second);item.IsChecked(current==entry.first);
                item.Click([send,term,type=entry.first](auto const&,auto const&)mutable
                {send("MODEL_SET_TYPE",term+'\x1f'+type);});
                menu.Items().Append(item);
            }
            return menu;
        };
        auto replacementMenu=[this](std::string const& term)
        {
            auto menu=Controls::MenuFlyout();
            std::set<std::string> occupied;
            for(auto const& modelTerm:model_.terms)
            {
                if(modelTerm==term)continue;
                for(auto const& variable : ::rlispstat::core::UniqueBaseVariablesForTerm(modelTerm))
                    occupied.insert(variable);
            }
            const std::string current=::rlispstat::core::BaseVariableForTermComponent(term);
            for(auto const& candidate:availableVariables_)
            {
                if(candidate==model_.response||candidate==current||occupied.count(candidate)>0)
                    continue;
                auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(candidate));
                item.Click([this,term,candidate](auto const&,auto const&)
                {
                    if(commandCallback_)
                        commandCallback_({"MODEL_REPLACE_TERM",model_.modelId.empty()?model_.group:model_.modelId,term,candidate});
                });
                menu.Items().Append(item);
            }
            if(menu.Items().Size()==0)
            {
                auto empty=Controls::MenuFlyoutItem();empty.Text(L"No replacement variables available");
                empty.IsEnabled(false);menu.Items().Append(empty);
            }
            return menu;
        };
        auto variableMenu=[this,send,typeMenu](::rlispstat::core::GLMCoefficientRow const& row)
        {
            const std::string term=!row.sourceTerm.empty()?row.sourceTerm:row.term;
            const bool interaction=term.find(':')!=std::string::npos;
            const bool editable=!term.empty()&&term!="(Intercept)"&&row.rowType!="intercept";
            auto menu=Controls::MenuFlyout();
            if(editable)
            {
                auto remove=Controls::MenuFlyoutItem();remove.Text(interaction?L"Remove interaction":L"Remove predictor");
                remove.Click([send,term](auto const&,auto const&)mutable{send("MODEL_REMOVE_TERM",term);});
                menu.Items().Append(remove);menu.Items().Append(Controls::MenuFlyoutSeparator());
            }
            auto heading=Controls::MenuFlyoutItem();
            heading.Text(to_hstring(!row.displayLabel.empty()?row.displayLabel:term));heading.IsEnabled(false);
            menu.Items().Append(heading);menu.Items().Append(Controls::MenuFlyoutSeparator());
            auto analyze=Controls::MenuFlyoutSubItem();analyze.Text(L"Analyze predictor");
            auto modelTerms=Controls::MenuFlyoutSubItem();modelTerms.Text(L"Model terms");
            auto edit=Controls::MenuFlyoutSubItem();edit.Text(L"Edit predictor");
            if(editable&&!interaction)
            {
                auto effect=Controls::MenuFlyoutItem();effect.Text(L"Effect plot…");
                effect.Click([send,term](auto const&,auto const&)mutable{send("MODEL_OPEN_INTERACTION_PLOT",term);});
                analyze.Items().Append(effect);
                auto partial=Controls::MenuFlyoutItem();partial.Text(L"Partial regression plot…");
                partial.Click([send,term](auto const&,auto const&)mutable{send("MODEL_OPEN_PARTIAL_PLOT",term);});
                analyze.Items().Append(partial);
                const auto typeFound=model_.termTypes.find(term);
                const std::string effectiveType=typeFound==model_.termTypes.end()?"numeric":typeFound->second;
                if(effectiveType=="factor"||effectiveType=="ordered")
                {
                    auto pairwise=Controls::MenuFlyoutItem();pairwise.Text(L"Pairwise comparisons…");
                    pairwise.Click([send,term](auto const&,auto const&)mutable{send("MODEL_PAIRWISE",term);});
                    analyze.Items().Append(pairwise);

                }
                auto replace=Controls::MenuFlyoutSubItem();replace.Text(L"Change variable");
                for(auto const& candidate:availableVariables_)
                {
                    if(candidate==model_.response||candidate==term||
                       std::find(model_.terms.begin(),model_.terms.end(),candidate)!=model_.terms.end())continue;
                    auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(candidate));
                    item.Click([this,term,candidate](auto const&,auto const&)
                    {
                        if(commandCallback_)
                            commandCallback_({"MODEL_REPLACE_TERM",model_.modelId.empty()?model_.group:model_.modelId,term,candidate});
                    });
                    replace.Items().Append(item);
                }
                if(replace.Items().Size()>0)edit.Items().Append(replace);
                auto types=Controls::MenuFlyoutSubItem();types.Text(L"Change type");
                const auto found=model_.termTypes.find(term);
                const std::string current=found==model_.termTypes.end()?"numeric":found->second;
                for(auto const& entry:std::vector<std::pair<std::string,std::wstring>>{{"numeric",L"Treat predictor as continuous"},{"factor",L"Treat predictor as categorical"}})
                {
                    auto item=Controls::RadioMenuFlyoutItem();
                    item.GroupName(to_hstring("linear-variable-type:"+model_.group+":"+term));
                    item.Text(entry.second);item.IsChecked(current==entry.first);
                    item.Click([send,term,type=entry.first](auto const&,auto const&)mutable
                    {send("MODEL_SET_TYPE",term+'\x1f'+type);});
                    types.Items().Append(item);
                }
                edit.Items().Append(types);
                if(effectiveType=="factor"||effectiveType=="ordered")
                {
                    const std::string variable=::rlispstat::core::BaseVariableForTermComponent(term);
                    const auto coding=std::find_if(fit_.factorCodings.begin(),fit_.factorCodings.end(),
                        [&](auto const& value){return value.variable==variable;});
                    if(coding!=fit_.factorCodings.end()&&!coding->levels.empty())
                    {
                        auto references=Controls::MenuFlyoutSubItem();references.Text(L"Reference category");
                        Controls::RadioMenuFlyoutItem::SetAreCheckStatesEnabled(references,true);
                        for(auto const& level:coding->levels)
                        {
                            auto item=Controls::RadioMenuFlyoutItem();
                            item.GroupName(to_hstring("linear-reference:"+model_.group+":"+variable));
                            item.Text(to_hstring(level));item.IsChecked(level==coding->referenceLevel);
                            item.Click([send,variable,level](auto const&,auto const&)mutable
                            {send("MODEL_SET_REFERENCE",variable+'\x1f'+level);});
                            references.Items().Append(item);
                        }
                        edit.Items().Append(references);
                    }
                }
                else
                {
                    const bool centered=model_.centeredPredictors.count(term)>0;
                    auto centre=Controls::MenuFlyoutItem();
                    centre.Text(centered?L"Remove centering":L"Center predictor");
                    centre.Click([send,term](auto const&,auto const&)mutable{send("MODEL_TOGGLE_CENTER",term);});
                    edit.Items().Append(centre);
                }
            }
            if(editable)
            {
                if(interaction)
                {
                    auto report=Controls::MenuFlyoutItem();report.Text(L"Interaction report");
                    report.Click([send,term](auto const&,auto const&)mutable{send("MODEL_INTERACTION_REPORT",term);});
                    analyze.Items().Append(report);
                    auto plot=Controls::MenuFlyoutItem();plot.Text(L"Effect plot");
                    plot.Click([send,term](auto const&,auto const&)mutable{send("MODEL_OPEN_INTERACTION_PLOT",term);});
                    analyze.Items().Append(plot);
                }
                AppendPolynomialMenu(modelTerms, PolynomialNumericVariables(numericVariables_, model_), model_.response, model_.terms,
                    [send](std::string const& value) mutable { send("MODEL_ADD_TERM", value); }, term);
                const auto interactionCandidates=::rlispstat::core::InteractionCandidateTermsForModel(availableVariables_,model_.response,model_.terms,model_.terms,term);
                if(auto interactions=BuildInteractionMenuSubItem(interactionCandidates,[send](std::string const& combined)mutable{send("MODEL_ADD_TERM",combined);},true,term)){modelTerms.Items().Append(interactions);}
            }
            if(analyze.Items().Size()>0)menu.Items().Append(analyze);
            if(modelTerms.Items().Size()>0)menu.Items().Append(modelTerms);
            if(edit.Items().Size()>0)menu.Items().Append(edit);
            return menu;
        };
        addRow();for(int column=0;column<9;++column)
        {
            auto text=addCell(headers[static_cast<std::size_t>(column)],0,column,true,column<2);
            AttachDirectMenu(targetFor(text),column<2?
                CreateStructuralCellMenu(headers[static_cast<std::size_t>(column)],text):
                CreateStatisticCellMenu(headers[static_cast<std::size_t>(column)],text,
                    LinearStatisticContext(model_, to_string(hstring(headers[static_cast<std::size_t>(column)])))));
        }
        int rowIndex=1;
        for(auto const& row:fit.coefficients)
        {
            const std::string label=::rlispstat::core::GLMRegressionCoefficientDisplayLabel(
                row,model_.centeredPredictors);
            termRowStructure_.push_back(label+"\x1f"+row.termType+"\x1f"+row.rowType);
            addRow();for(int column=0;column<9;++column)
            {
                auto text=addCell(L"",rowIndex,column,false,column<2,row.rowType=="term");
                termCells_.push_back(text);
                if(column==0)
                {
                    // The variable column is intrinsically sized and capped.
                    // Preserve the complete term identity when WinUI has to
                    // ellipsize a genuinely long interaction label.
                    Controls::ToolTipService::SetToolTip(
                        targetFor(text), box_value(to_hstring(label)));
                    const std::string term=!row.sourceTerm.empty()?row.sourceTerm:row.term;
                    const bool replaceOnPrimary=(!model_.precomputed || model_.multipleImputation)&&!term.empty()&&
                        term!="(Intercept)"&&term.find(':')==std::string::npos&&
                        (row.rowType=="factor_parent"||row.sourceTerm.empty()||row.term==row.sourceTerm);
                    if(replaceOnPrimary)
                        AttachPrimaryAndContextMenus(targetFor(text),replacementMenu(term),variableMenu(row));
                    else AttachDirectMenu(targetFor(text),variableMenu(row));
                }
                else if(column==1)
                {
                    const std::string term=!row.sourceTerm.empty()?row.sourceTerm:row.term;
                    if(!term.empty()&&term!="(Intercept)"&&term.find(':')==std::string::npos)
                        AttachDirectMenu(targetFor(text),typeMenu(term));
                    else AttachDirectMenu(targetFor(text),CreateStructuralCellMenu(L"Term type",text));
                }
                else AttachDirectMenu(targetFor(text),CreateStatisticCellMenu(
                    headers[static_cast<std::size_t>(column)],text,
                    LinearStatisticContext(model_, to_string(hstring(headers[static_cast<std::size_t>(column)])))));
            }
            ++rowIndex;
        }

        // Match the macOS result table: term insertion is an action row at
        // the bottom of the terms table, not a second predictor control in
        // the window header.
        addRow();
        auto addTerm = Controls::Button();
        addTerm.Content(box_value(L"+ Add term"));
        addTerm.FontSize(11.5);
        addTerm.HorizontalAlignment(HorizontalAlignment::Left);
        addTerm.Background(TransparentBrush());
        addTerm.BorderBrush(TransparentBrush());
        addTerm.BorderThickness(Thickness{0,0,0,0});
        addTerm.Padding(Thickness{8,4,8,4});
        addTerm.IsEnabled(!model_.precomputed || model_.multipleImputation);
        auto addMenu = Controls::MenuFlyout();
        for (auto const& variable : availableVariables_)
        {
            if (variable == model_.response ||
                std::find(model_.terms.begin(), model_.terms.end(), variable) != model_.terms.end())
                continue;
            auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(variable));
            item.Click([this, variable](auto const&, auto const&)
            {
                if (commandCallback_)
                    commandCallback_({"MODEL_ADD_TERM", model_.modelId.empty()?model_.group:model_.modelId, variable});
            });
            addMenu.Items().Append(item);
        }
        const auto interactionCandidates = ::rlispstat::core::InteractionCandidateTermsForModel(
            availableVariables_, model_.response, model_.terms, model_.terms);
        if (!interactionCandidates.empty())
        {
            if (addMenu.Items().Size() > 0)
                addMenu.Items().Append(Controls::MenuFlyoutSeparator());
            auto interactions = BuildInteractionMenuSubItem(
                interactionCandidates,
                [this](std::string const& term)
                {
                    if (commandCallback_)
                        commandCallback_({"MODEL_ADD_TERM", model_.modelId.empty()?model_.group:model_.modelId, term});
                }, !model_.precomputed || model_.multipleImputation);
            if (interactions) addMenu.Items().Append(interactions);
        }
        if (addMenu.Items().Size() == 0)
        {
            auto empty = Controls::MenuFlyoutItem(); empty.Text(L"No terms available");
            empty.IsEnabled(false); addMenu.Items().Append(empty);
        }
        addTerm.Flyout(addMenu);
        Controls::Grid::SetRow(addTerm, rowIndex);
        Controls::Grid::SetColumn(addTerm, 0);
        Controls::Grid::SetColumnSpan(addTerm, 9);
        termsGrid_.Children().Append(addTerm);
        termRowsInitialized_ = true;
    }

    void LinearModelView::Render(::rlispstat::core::GroupModelState const& model,::rlispstat::core::GLMFitSummary const& fit)
    {
        ::rlispstat::windows::performance::Scope timing(
            "Output.LinearModel.Values.Update");
        using namespace ::rlispstat::core;
        InitializeReport();
        fitSummary_.Text(to_hstring("R\u00B2 = "+FormatPercent(fit.r2,1)+
            "     Adjusted R\u00B2 = "+FormatPercent(fit.adjR2,1)+
            "     s = "+FormatDouble(fit.sigma,3)+"     df = "+std::to_string(fit.dfResidual)));
        const std::array<std::string,12> anovaValues{
            "Regression",FormatDouble(fit.ssRegression,4),std::to_string(fit.dfModel),
            FormatDouble(fit.msRegression,4),FormatDouble(fit.globalF,3),FormatPValue(fit.globalP),
            "Residual",FormatDouble(fit.ssResidual,4),std::to_string(fit.dfResidual),
            FormatDouble(fit.msResidual,4),"\u2014","\u2014"};
        for(std::size_t index=0;index<anovaCells_.size();++index)
            anovaCells_[index].Text(to_hstring(anovaValues[index]));

        std::vector<std::string> structure;structure.reserve(fit.coefficients.size());
        for(auto const& row:fit.coefficients)
        {
            const std::string label=GLMRegressionCoefficientDisplayLabel(
                row,model.centeredPredictors);
            structure.push_back(label+"\x1f"+row.termType+"\x1f"+row.rowType);
        }
        if(GLMTermsTableNeedsRebuild(
            termRowsInitialized_,termRowStructure_,structure))RebuildTermRows(fit);
        std::size_t cell=0;
        for(auto const& row:fit.coefficients)
        {
            const auto values=GLMRegressionTableRow(row,model.centeredPredictors);
            for(auto const& value:values)
                termCells_[cell++].Text(to_hstring(GLMRegressionTableCellText(value)));
        }
        std::string footer="Analysis scope: "+model.scope+" \u00B7 "+
            std::to_string(fit.n)+" observations";
        if(!fit.predictorCenters.empty())
        {
            footer+=" \u00B7 Centered: ";bool first=true;
            for(auto const& entry:fit.predictorCenters)
            {
                if(!std::isfinite(entry.second))continue;
                if(!first)footer+=", ";first=false;
                footer+=entry.first+" (mean = "+FormatDouble(entry.second,4)+")";
            }
        }
        footer_.Text(to_hstring(footer));
        const std::string noteText = model.note +
            (model.note.empty() || model.frozenScopeNotice.empty() ? "" : "  ") +
            model.frozenScopeNotice;
        note_.Text(to_hstring(noteText));
        note_.Visibility(noteText.empty()?Visibility::Collapsed:Visibility::Visible);
        const bool success=fit.warning.find("success")!=std::string::npos||
            fit.warning.find("fitted")!=std::string::npos;
        warning_.Text(to_hstring((success?std::string{}:"Warning: ")+fit.warning));
        warning_.Foreground(success?Brush(92,92,92):Brush(145,54,45));
        warning_.Visibility(fit.warning.empty()?Visibility::Collapsed:Visibility::Visible);
        renderedFitVersion_=model.fitVersion;renderedFitOk_=fit.ok;renderedWarning_=fit.warning;
    }
    void LinearModelView::Activate(){if(!closed_)window_.Activate();}
    void LinearModelView::Close(){if(!closed_)window_.Close();}

    std::shared_ptr<DimensionalityView> DimensionalityView::Create()
    {
        auto view = std::shared_ptr<DimensionalityView>(new DimensionalityView());
        view->AttachLifetime();
        return view;
    }

    DimensionalityView::DimensionalityView() { Initialize(); }

    void DimensionalityView::Initialize()
    {
        using namespace ::rlispstat::core;
        window_ = CreateLinkEDAWindow();
        window_.Title(to_hstring(DimensionalityWindowTitle()));
        auto root = Controls::Grid();
        root.Padding(Thickness{ 16, 14, 16, 10 });
        root.RowSpacing(7);
        root.Background(Brush(255, 255, 255));
        root.RequestedTheme(ElementTheme::Light);
        auto autoRow = []() { auto row = Controls::RowDefinition();
            row.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Auto)); return row; };
        root.RowDefinitions().Append(autoRow());
        root.RowDefinitions().Append(autoRow());
        root.RowDefinitions().Append(autoRow());
        auto body = Controls::RowDefinition();
        body.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
        root.RowDefinitions().Append(body);
        root.RowDefinitions().Append(autoRow());

        auto heading = Controls::Grid();
        auto grow = Controls::ColumnDefinition();
        grow.Width(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
        heading.ColumnDefinitions().Append(grow);
        heading.ColumnDefinitions().Append(Controls::ColumnDefinition());
        title_ = Controls::TextBlock(); title_.Text(to_hstring(DimensionalityWindowTitle()));
        title_.FontSize(17); title_.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        heading.Children().Append(title_);
        badge_ = Controls::TextBlock(); badge_.FontSize(12); badge_.Foreground(Brush(72,72,72));
        badge_.VerticalAlignment(VerticalAlignment::Center);
        Controls::Grid::SetColumn(badge_, 1); heading.Children().Append(badge_);
        root.Children().Append(heading);

        auto controls = Controls::Grid(); controls.ColumnSpacing(10);
        for (double width : { 164.0, 126.0, 134.0, 126.0, 150.0 })
        {
            auto column = Controls::ColumnDefinition();
            column.Width(GridLengthHelper::FromPixels(width)); controls.ColumnDefinitions().Append(column);
        }
        method_ = Controls::ComboBox();
        method_.Items().Append(box_value(to_hstring(DimensionalityPCAMethodTitle())));
        method_.Items().Append(box_value(to_hstring(DimensionalityFactorAnalysisMethodTitle())));
        missing_ = Controls::ComboBox(); missing_.Items().Append(box_value(L"Listwise"));
        missing_.Items().Append(box_value(L"Pairwise"));
        components_ = Controls::ComboBox();
        rotation_ = Controls::ComboBox();
        rotation_.Items().Append(box_value(to_hstring(DimensionalityNoRotationTitle())));
        rotation_.Items().Append(box_value(to_hstring(DimensionalityVarimaxRotationTitle())));
        rotation_.Items().Append(box_value(to_hstring(DimensionalityQuartimaxRotationTitle())));
        scope_ = Controls::ComboBox();
        std::array<Controls::ComboBox, 5> combos{ method_, missing_, components_, rotation_, scope_ };
        for (int index = 0; index < static_cast<int>(combos.size()); ++index)
        {
            combos[static_cast<std::size_t>(index)].MinHeight(30);
            combos[static_cast<std::size_t>(index)].HorizontalAlignment(HorizontalAlignment::Stretch);
            Controls::Grid::SetColumn(combos[static_cast<std::size_t>(index)], index);
            controls.Children().Append(combos[static_cast<std::size_t>(index)]);
        }
        Controls::Grid::SetRow(controls, 1); root.Children().Append(controls);

        auto options = Controls::Grid();
        auto optionGrow = Controls::ColumnDefinition();
        optionGrow.Width(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
        options.ColumnDefinitions().Append(optionGrow); options.ColumnDefinitions().Append(Controls::ColumnDefinition());
        auto optionButtons = Controls::StackPanel();
        optionButtons.Orientation(Controls::Orientation::Horizontal);
        optionButtons.Spacing(14);
        scale_ = Controls::CheckBox(); scale_.Content(box_value(to_hstring(DimensionalityStandardizeButtonTitle())));
        autoFit_ = Controls::CheckBox(); autoFit_.Content(box_value(L"Auto-fit"));
        optionButtons.Children().Append(scale_);
        optionButtons.Children().Append(autoFit_);
        imputation_ = Controls::ComboBox(); imputation_.MinWidth(180);
        imputation_.SelectionChanged([this](auto const&,auto const&){
            if(!updating_ && commandCallback_ && imputation_.SelectedIndex()>=0)
                commandCallback_({"PCAFA_SET_IMPUTATION",state_.id,std::to_string(imputation_.SelectedIndex()+1)});
        });
        optionButtons.Children().Append(imputation_);
        options.Children().Append(optionButtons);
        selectedRows_ = Controls::TextBlock(); selectedRows_.FontSize(11.5); selectedRows_.Foreground(Brush(90,90,90));
        selectedRows_.VerticalAlignment(VerticalAlignment::Center); Controls::Grid::SetColumn(selectedRows_, 1);
        options.Children().Append(selectedRows_); Controls::Grid::SetRow(options, 2); root.Children().Append(options);

        auto scroll = Controls::ScrollViewer();
        scroll.HorizontalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);
        scroll.VerticalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);
        report_ = Controls::Canvas(); report_.Name(L"LinkEDA.SnapshotTable"); report_.Background(Brush(255,255,255));
        auto reportMenu = Controls::MenuFlyout();
        auto scree = Controls::MenuFlyoutItem(); scree.Text(L"Scree plot");
        scree.Click([this](auto const&, auto const&) { if (actionCallback_) actionCallback_("scree"); });
        auto biplot = Controls::MenuFlyoutItem(); biplot.Text(L"Biplot");
        biplot.Click([this](auto const&, auto const&) { if (actionCallback_) actionCallback_("biplot"); });
        reportMenu.Items().Append(scree); reportMenu.Items().Append(biplot);
        AttachWindowContextFlyout(window_, reportMenu);
        scroll.Content(report_); Controls::Grid::SetRow(scroll, 3); root.Children().Append(scroll);
        status_ = Controls::TextBlock(); status_.FontSize(11.5); status_.Foreground(Brush(92,92,92));
        status_.TextWrapping(TextWrapping::Wrap); Controls::Grid::SetRow(status_, 4); root.Children().Append(status_);

        auto changed = [this](auto const&, auto const&) { NotifyChanged(); };
        method_.SelectionChanged(changed); missing_.SelectionChanged(changed);
        components_.SelectionChanged(changed); rotation_.SelectionChanged(changed);
        scope_.SelectionChanged(changed); scale_.Click(changed); autoFit_.Click(changed);
        exporter_ = TableExportService::Create(window_, root);
        window_.Content(root); ResizeLogical(window_, 790, 570);
    }

    void DimensionalityView::AttachLifetime()
    {
        std::weak_ptr<DimensionalityView> weak = shared_from_this();
        window_.Closed([weak](auto const&, auto const&) { if (auto view = weak.lock())
        { view->closed_ = true; if (view->closedCallback_) view->closedCallback_(); } });
    }

    void DimensionalityView::SetClosedCallback(Closed callback) { closedCallback_ = std::move(callback); }
    void DimensionalityView::SetChangedCallback(Changed callback) { changedCallback_ = std::move(callback); }
    void DimensionalityView::SetActionCallback(Action callback) { actionCallback_ = std::move(callback); }
    void DimensionalityView::SetCommandCallback(Command callback) { commandCallback_ = std::move(callback); ConfigureContextMenu(state_); }
    void DimensionalityView::SetScopeChoices(std::vector<::rlispstat::core::AnalysisScopeChoice> choices) { scopeChoices_ = std::move(choices); if(scope_) { const bool wasUpdating=updating_;updating_=true;PopulateAnalysisScopeCombo(scope_,scopeChoices_,"global");updating_=wasUpdating; } }

    Controls::MenuFlyout DimensionalityView::CreateVariableMenu(
        std::size_t variableIndex)
    {
        using namespace ::rlispstat::core;
        auto menu = Controls::MenuFlyout();
        if (!state_.hasSeed || variableIndex >= state_.variables.size()) return menu;
        const auto numeric = state_.eligibleVariables;
        const auto menuState = BuildDimensionalityVariableMenuState(
            state_.variables, numeric, variableIndex);
        if (!menuState.ok) return menu;

        auto submit = [this](std::vector<std::string> updated)
        {
            if (!commandCallback_) return;
            std::vector<std::string> command{ "PCAFA_SET_VARIABLES", state_.id,
                std::to_string(updated.size()) };
            command.insert(command.end(), updated.begin(), updated.end());
            commandCallback_(command);
        };
        auto heading = Controls::MenuFlyoutItem();
        heading.Text(to_hstring(menuState.variable)); heading.IsEnabled(false);
        menu.Items().Append(heading);

        auto replace = Controls::MenuFlyoutSubItem(); replace.Text(L"Replace with");
        for (auto const& name : menuState.replacementVariables)
        {
            auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(name));
            item.Click([submit, variableIndex, name, variables = state_.variables]
                (auto const&, auto const&) mutable
            { auto updated = variables; updated[variableIndex] = name; submit(std::move(updated)); });
            replace.Items().Append(item);
        }
        if (replace.Items().Size() == 0)
        {
            auto empty = Controls::MenuFlyoutItem();
            empty.Text(to_hstring(menuState.replacementEmptyTitle)); empty.IsEnabled(false);
            replace.Items().Append(empty);
        }
        menu.Items().Append(replace);

        auto add = Controls::MenuFlyoutSubItem(); add.Text(L"Add variable");
        for (auto const& name : numeric)
        {
            if (std::find(state_.variables.begin(), state_.variables.end(), name) !=
                state_.variables.end()) continue;
            auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(name));
            item.Click([submit, name, variables = state_.variables]
                (auto const&, auto const&) mutable
            { auto updated = variables; updated.push_back(name); submit(std::move(updated)); });
            add.Items().Append(item);
        }
        if (add.Items().Size() > 0) menu.Items().Append(add);

        auto remove = Controls::MenuFlyoutItem();
        remove.Text(to_hstring(menuState.removeTitle)); remove.IsEnabled(menuState.canRemove);
        remove.Click([submit, variable = menuState.variable, variables = state_.variables]
            (auto const&, auto const&) mutable
        {
            auto updated = variables;
            updated.erase(std::remove(updated.begin(), updated.end(), variable), updated.end());
            submit(std::move(updated));
        });
        menu.Items().Append(remove);
        AppendRCodeOnlyExportMenu(menu, commandCallback_, state_.id);
        return menu;
    }

    void DimensionalityView::ConfigureContextMenu(::rlispstat::core::DimensionalityState const& state)
    {
        auto menu = Controls::MenuFlyout();
        if (state.hasSeed && commandCallback_)
        {
            const auto numeric = state.eligibleVariables;
            auto submit = [this](std::vector<std::string> updated)
            {
                std::vector<std::string> command{ "PCAFA_SET_VARIABLES", state_.id,
                    std::to_string(updated.size()) };
                command.insert(command.end(), updated.begin(), updated.end());
                commandCallback_(command);
            };
            auto variables = Controls::MenuFlyoutSubItem(); variables.Text(L"Variables");
            auto add = Controls::MenuFlyoutSubItem();
            add.Text(to_hstring(::rlispstat::core::DimensionalityAddVariableMenuTitle()));
            for (auto const& name : numeric)
            {
                if (std::find(state.variables.begin(), state.variables.end(), name) !=
                    state.variables.end()) continue;
                auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(name));
                item.Click([submit, name, variables = state.variables](auto const&, auto const&) mutable
                {
                    auto updated = variables; updated.push_back(name); submit(std::move(updated));
                });
                add.Items().Append(item);
            }
            if (add.Items().Size() > 0) variables.Items().Append(add);

            auto replace = Controls::MenuFlyoutSubItem();
            replace.Text(to_hstring(::rlispstat::core::DimensionalityReplaceVariableMenuTitle()));
            for (std::size_t index = 0; index < state.variables.size(); ++index)
            {
                auto current = Controls::MenuFlyoutSubItem();
                current.Text(to_hstring(state.variables[index]));
                const auto menuState = ::rlispstat::core::BuildDimensionalityVariableMenuState(
                    state.variables, numeric, index);
                for (auto const& name : menuState.replacementVariables)
                {
                    auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(name));
                    item.Click([submit, index, name, variables = state.variables]
                        (auto const&, auto const&) mutable
                    {
                        auto updated = variables; updated[index] = name; submit(std::move(updated));
                    });
                    current.Items().Append(item);
                }
                if (current.Items().Size() > 0) replace.Items().Append(current);
            }
            if (replace.Items().Size() > 0) variables.Items().Append(replace);

            if (state.variables.size() > 2)
            {
                auto remove = Controls::MenuFlyoutSubItem(); remove.Text(L"Remove variable");
                for (auto const& name : state.variables)
                {
                    auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(name));
                    item.Click([submit, name, variables = state.variables]
                        (auto const&, auto const&) mutable
                    {
                        auto updated = variables;
                        updated.erase(std::remove(updated.begin(), updated.end(), name), updated.end());
                        submit(std::move(updated));
                    });
                    remove.Items().Append(item);
                }
                variables.Items().Append(remove);
            }
            menu.Items().Append(variables);
            menu.Items().Append(Controls::MenuFlyoutSeparator());
        }
        auto explain = Controls::MenuFlyoutSubItem();
        explain.Text(to_hstring(
            ::rlispstat::core::DefaultModelContextMenuTitles().explainStatistic));
        std::string explanationMethod = state.method;
        if (state.rotation != "none" && !state.rotation.empty())
            explanationMethod += " with " + state.rotation + " rotation";
        for (auto const& statistic : std::vector<std::string>{
                 "Eigenvalue", "Parallel eigenvalue", "Proportion of variance",
                 "Cumulative variance", "Loading", "Communality", "Uniqueness" })
        {
            auto item = Controls::MenuFlyoutItem();
            item.Text(to_hstring(statistic));
            item.Click([owner = status_, context = AnalysisStatisticContext(
                            statistic, "dimensionality", explanationMethod)]
                       (auto const&, auto const&) mutable
            {
                ShowStatisticExplanation(owner, context);
            });
            explain.Items().Append(item);
        }
        menu.Items().Append(explain);
        menu.Items().Append(Controls::MenuFlyoutSeparator());
        auto scree = Controls::MenuFlyoutItem(); scree.Text(L"Scree plot");
        scree.Click([this](auto const&, auto const&)
            { if (actionCallback_) actionCallback_("scree"); });
        menu.Items().Append(scree);
        auto biplot = Controls::MenuFlyoutItem(); biplot.Text(L"Biplot");
        biplot.Click([this](auto const&, auto const&)
            { if (actionCallback_) actionCallback_("biplot"); });
        menu.Items().Append(biplot);
        if (exporter_) {
            auto exportMenu = exporter_->CreateMenu(
                [this]() { return DimensionalityExportPayload(state_); });
            AppendRCodeExportItems(exportMenu, commandCallback_, state.id);
            menu.Items().Append(exportMenu);
        } else {
            AppendRCodeOnlyExportMenu(menu, commandCallback_, state.id);
        }
        AttachWindowContextFlyout(window_, menu);
    }

    void DimensionalityView::Show(::rlispstat::core::DimensionalityState const& state)
    {
        ::rlispstat::windows::performance::Scope timing("Output.Dimensionality.Show");
        SetWindowDataSheetGroup(window_, state.group);
        SetWindowSnapshotSource(window_, "output", state.id, state.group);
        using namespace ::rlispstat::core;
        state_=state;
        updating_ = true;
        const auto controls = BuildDimensionalityControlState(state.method, state.missingMode,
            state.rotation, state.scope, state.scale, state.variables.size(), state.componentCount);
        window_.Title(to_hstring(DimensionalityWindowTitle())); badge_.Text(to_hstring(state.group));
        method_.SelectedIndex(controls.methodIndex); missing_.SelectedIndex(controls.missingMode == "pairwise" ? 1 : 0);
        rotation_.SelectedIndex(controls.rotationIndex);
        auto choices=scopeChoices_;if(choices.empty())choices=BuildAnalysisScopeChoices(0,{},true);
        PopulateAnalysisScopeCombo(scope_,choices,AnalysisScopeChoiceValue(state.scope,state.dataScope,state.dataScopeCaptured));
        scale_.IsChecked(controls.scale);
        autoFit_.IsChecked(state.autoFit);
        imputation_.Visibility(state.multipleImputation?Visibility::Visible:Visibility::Collapsed);
        imputation_.Items().Clear();
        if(state.multipleImputation){
            for(int i=1;i<=state.imputationCount;++i)
                imputation_.Items().Append(box_value(to_hstring("Imputation "+std::to_string(i)+" of "+std::to_string(state.imputationCount, state.calculationMethod))));
            imputation_.SelectedIndex(state.displayedImputation-1);
        }
        components_.Items().Clear();
        for (auto const& option : controls.componentOptions) components_.Items().Append(box_value(to_hstring(option)));
        components_.SelectedIndex(controls.componentIndex);
        selectedRows_.Text(to_hstring(std::to_string(state.rowsUsed.size()) + " rows used"));
        status_.Text(to_hstring(state.status)); updating_ = false;
        if (!state.rFitPending || !presented_) Render(state);
        ConfigureContextMenu(state);
        const auto viewModel = BuildDimensionalityReportViewModel(state.variables, state.components,
            state.loadings, state.rowsUsed.size(), state.rowsExcluded.size(), state.method,
            controls.componentCount, state.status, state.focusedComponent, state.focusedVariable, 8,
            state.missingMode, state.rotation, state.scale, state.multipleImputation,
            state.displayedImputation, state.imputationCount, state.calculationMethod);
        PresentOrResizeAnalysisWindow(window_, presented_,
            std::clamp(viewModel.layout.preferredWidth + 52.0, 760.0, 1220.0),
            std::clamp(viewModel.layout.preferredHeight + 172.0, 390.0, 820.0));
    }

    void DimensionalityView::Render(::rlispstat::core::DimensionalityState const& state)
    {
        using namespace ::rlispstat::core;
        report_.Children().Clear();
        const auto model = BuildDimensionalityReportViewModel(state.variables, state.components,
            state.loadings, state.rowsUsed.size(), state.rowsExcluded.size(), state.method,
            state.componentCount, state.status, state.focusedComponent, state.focusedVariable, 8,
            state.missingMode, state.rotation, state.scale, state.multipleImputation,
            state.displayedImputation, state.imputationCount, state.calculationMethod);
        report_.Width(model.layout.preferredWidth); report_.Height(model.layout.preferredHeight);
        auto addRect = [this](Rect const& rect, Media::Brush const& fill)
        {
            auto shape = Shapes::Rectangle(); shape.Width(rect.width); shape.Height(rect.height); shape.Fill(fill);
            Controls::Canvas::SetLeft(shape, rect.x); Controls::Canvas::SetTop(shape, rect.y); report_.Children().Append(shape);
        };
        for (auto const& highlight : model.renderPlan.highlights)
            addRect(highlight.rect, highlight.role == DimensionalityReportHighlightRole::Strong
                ? Brush(255, 235, 194) : Brush(230, 242, 251));
        for (auto const& rule : model.renderPlan.rules)
        {
            auto line = Shapes::Line(); line.X1(rule.start.x); line.Y1(rule.start.y);
            line.X2(rule.end.x); line.Y2(rule.end.y); line.Stroke(Brush(219, 221, 224)); line.StrokeThickness(1);
            report_.Children().Append(line);
        }
        for (auto const& item : model.renderPlan.texts)
        {
            auto text = Controls::TextBlock(); text.Text(to_hstring(item.text));
            const bool muted = item.role == DimensionalityReportTextRole::Muted;
            const bool header = item.role == DimensionalityReportTextRole::Section ||
                item.role == DimensionalityReportTextRole::RightHeader;
            const bool right = item.role == DimensionalityReportTextRole::Right ||
                item.role == DimensionalityReportTextRole::RightHeader;
            text.FontSize(muted ? 11 : 12); text.Foreground(Brush(muted ? 110 : 38, muted ? 110 : 40, muted ? 110 : 44));
            if (header) text.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
            text.TextAlignment(right ? TextAlignment::Right : TextAlignment::Left);
            text.TextTrimming(TextTrimming::CharacterEllipsis); text.Width(item.rect.width); text.Height(item.rect.height);
            Controls::Canvas::SetLeft(text, item.rect.x); Controls::Canvas::SetTop(text, item.rect.y);
            report_.Children().Append(text);
            if (item.role == DimensionalityReportTextRole::RightHeader &&
                (item.text == "Eigenvalue" || item.text == "Parallel" ||
                 item.text == "Variance" || item.text == "Cumulative" ||
                 item.text == "Communality" || item.text == "Uniqueness"))
            {
                const std::string statistic = item.text == "Parallel"
                    ? "Parallel eigenvalue" : item.text;
                AttachDirectMenu(text,CreateStatisticCellMenu(
                    to_hstring(statistic).c_str(),text,
                    AnalysisStatisticContext(statistic,"dimensionality",state.method)));
            }
        }
        for (std::size_t index = 0;
             index < model.report.loadingRows.size() && index < model.layout.variableRects.size();
             ++index)
        {
            auto const& rect = model.layout.variableRects[index];
            auto hitTarget = Controls::Border();
            hitTarget.Width(rect.width); hitTarget.Height(rect.height);
            hitTarget.Background(TransparentBrush());
            Controls::Canvas::SetLeft(hitTarget, rect.x);
            Controls::Canvas::SetTop(hitTarget, rect.y);
            AttachDirectMenu(hitTarget, CreateVariableMenu(index));
            report_.Children().Append(hitTarget);
        }
        if (state_.hasSeed && commandCallback_)
        {
            const auto numeric = state_.eligibleVariables;
            const auto addState = BuildDimensionalityAddVariableMenuState(
                state_.variables, numeric);
            auto addMenu = Controls::MenuFlyout();
            for (auto const& name : addState.variables)
            {
                auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(name));
                item.Click([this, name](auto const&, auto const&)
                {
                    auto updated = state_.variables; updated.push_back(name);
                    std::vector<std::string> command{ "PCAFA_SET_VARIABLES", state_.id,
                        std::to_string(updated.size()) };
                    command.insert(command.end(), updated.begin(), updated.end());
                    commandCallback_(command);
                });
                addMenu.Items().Append(item);
            }
            if (addMenu.Items().Size() == 0)
            {
                auto empty = Controls::MenuFlyoutItem();
                empty.Text(to_hstring(addState.emptyTitle)); empty.IsEnabled(false);
                addMenu.Items().Append(empty);
            }
            auto addTarget = Controls::Border();
            addTarget.Width(model.layout.addVariableRect.width);
            addTarget.Height(model.layout.addVariableRect.height);
            addTarget.Background(TransparentBrush());
            Controls::Canvas::SetLeft(addTarget, model.layout.addVariableRect.x);
            Controls::Canvas::SetTop(addTarget, model.layout.addVariableRect.y);
            AttachDirectMenu(addTarget, addMenu);
            report_.Children().Append(addTarget);
        }
    }

    void DimensionalityView::NotifyChanged()
    {
        if (updating_ || !changedCallback_ || method_.SelectedIndex() < 0 || missing_.SelectedIndex() < 0 ||
            components_.SelectedIndex() < 0 || rotation_.SelectedIndex() < 0 || scope_.SelectedIndex() < 0) return;
        changedCallback_(method_.SelectedIndex() == 1 ? "factor" : "pca",
            missing_.SelectedIndex() == 1 ? "pairwise" : "listwise",
            rotation_.SelectedIndex() == 1 ? "varimax" : rotation_.SelectedIndex() == 2 ? "quartimax" : "none",
            SelectedAnalysisScopeChoice(scope_),
            scale_.IsChecked().GetBoolean(), components_.SelectedIndex() + 1,
            autoFit_.IsChecked().GetBoolean());
    }

    void DimensionalityView::Activate() { if (!closed_) window_.Activate(); }
    void DimensionalityView::Close() { if (!closed_) window_.Close(); }

    std::shared_ptr<ScaleAnalysisView> ScaleAnalysisView::Create()
    {
        auto view = std::shared_ptr<ScaleAnalysisView>(new ScaleAnalysisView());
        view->AttachLifetime();
        return view;
    }

    ScaleAnalysisView::ScaleAnalysisView() { Initialize(); }

    void ScaleAnalysisView::Initialize()
    {
        window_ = CreateLinkEDAWindow();
        window_.Title(L"Scale Analysis");
        auto root = Controls::Grid();
        root.Padding(Thickness{ 16, 14, 16, 10 });
        root.RowSpacing(7);
        root.Background(Brush(255, 255, 255));
        root.RequestedTheme(ElementTheme::Light);
        auto autoRow = []() { auto row = Controls::RowDefinition();
            row.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Auto)); return row; };
        root.RowDefinitions().Append(autoRow());
        root.RowDefinitions().Append(autoRow());
        auto body = Controls::RowDefinition();
        body.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
        root.RowDefinitions().Append(body);
        root.RowDefinitions().Append(autoRow());

        auto heading = Controls::Grid();
        auto grow = Controls::ColumnDefinition();
        grow.Width(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
        heading.ColumnDefinitions().Append(grow);
        heading.ColumnDefinitions().Append(Controls::ColumnDefinition());
        title_ = Controls::TextBlock(); title_.Text(L"Scale Analysis");
        title_.FontSize(17); title_.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        heading.Children().Append(title_);
        badge_ = Controls::TextBlock(); badge_.FontSize(12); badge_.Foreground(Brush(72,72,72));
        badge_.VerticalAlignment(VerticalAlignment::Center);
        Controls::Grid::SetColumn(badge_, 1); heading.Children().Append(badge_);
        root.Children().Append(heading);

        auto controls = Controls::StackPanel();
        controls.Orientation(Controls::Orientation::Horizontal); controls.Spacing(10);
        correlation_ = Controls::ComboBox(); correlation_.MinWidth(150);
        for (auto const* value : { L"Auto correlations", L"Pearson", L"Polychoric", L"Mixed" })
            correlation_.Items().Append(box_value(value));
        score_ = Controls::ComboBox(); score_.MinWidth(115);
        score_.Items().Append(box_value(L"Mean score")); score_.Items().Append(box_value(L"Sum score"));
        addItem_ = Controls::Button(); addItem_.Content(box_value(L"+ Add item\u2026"));
        addItem_.Background(Brush(239, 241, 244));

        addItemFlyout_ = Controls::Flyout();
        auto itemSelector = Controls::StackPanel();
        itemSelector.Width(360); itemSelector.Spacing(7);
        addItemSearch_ = Controls::TextBox();
        addItemSearch_.PlaceholderText(L"Search items");
        addItemSearch_.HorizontalAlignment(HorizontalAlignment::Stretch);
        itemSelector.Children().Append(addItemSearch_);
        addItemList_ = Controls::ListView();
        addItemList_.Height(280);
        addItemList_.SelectionMode(Controls::ListViewSelectionMode::Single);
        addItemList_.IsItemClickEnabled(true);
        itemSelector.Children().Append(addItemList_);
        addItemEmpty_ = Controls::TextBlock();
        addItemEmpty_.Text(L"No eligible unused items");
        addItemEmpty_.Foreground(Brush(96, 96, 96));
        addItemEmpty_.FontSize(11.5);
        addItemEmpty_.Visibility(Visibility::Collapsed);
        itemSelector.Children().Append(addItemEmpty_);
        auto hint = Controls::TextBlock();
        hint.Text(L"Type to filter · Enter or click to add · ↑/↓ to move · Esc to close");
        hint.Foreground(Brush(96, 96, 96)); hint.FontSize(10.5);
        itemSelector.Children().Append(hint);
        addItemFlyout_.Content(itemSelector);
        addItem_.Flyout(addItemFlyout_);
        addItemFlyout_.Opened([this](auto const&, auto const&)
        {
            addItemSearch_.Text(L"");
            RefreshAddItemSelector(0);
            addItemSearch_.Focus(FocusState::Programmatic);
        });
        addItemSearch_.TextChanged([this](auto const&, auto const&)
            { RefreshAddItemSelector(0); });
        addItemSearch_.KeyDown([this](auto const&, Input::KeyRoutedEventArgs const& event)
        {
            if (event.Key() == Windows::System::VirtualKey::Enter)
            {
                AddSelectedQuickItem(); event.Handled(true);
            }
            else if (event.Key() == Windows::System::VirtualKey::Down)
            {
                MoveAddItemSelection(1); event.Handled(true);
            }
            else if (event.Key() == Windows::System::VirtualKey::Up)
            {
                MoveAddItemSelection(-1); event.Handled(true);
            }
            else if (event.Key() == Windows::System::VirtualKey::Escape)
            {
                addItemFlyout_.Hide(); event.Handled(true);
            }
        });
        addItemList_.KeyDown([this](auto const&, Input::KeyRoutedEventArgs const& event)
        {
            if (event.Key() == Windows::System::VirtualKey::Enter)
            {
                AddSelectedQuickItem(); event.Handled(true);
            }
            else if (event.Key() == Windows::System::VirtualKey::Escape)
            {
                addItemFlyout_.Hide(); event.Handled(true);
            }
        });
        addItemList_.ItemClick([this](auto const&, auto const& event)
        {
            const auto clicked = event.ClickedItem();
            if (!clicked) return;
            const std::string name = to_string(unbox_value<hstring>(clicked));
            auto found = std::find_if(filteredAddItems_.begin(), filteredAddItems_.end(),
                [&](auto const& item) { return item.variable == name; });
            if (found != filteredAddItems_.end()) AddQuickItem(*found);
        });
        controls.Children().Append(correlation_); controls.Children().Append(score_);
        Controls::Grid::SetRow(controls, 1); root.Children().Append(controls);

        auto scroll = Controls::ScrollViewer();
        scroll.HorizontalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);
        scroll.VerticalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);
        report_ = Controls::StackPanel(); report_.Name(L"LinkEDA.SnapshotTable"); report_.Spacing(5);
        scroll.Content(report_); Controls::Grid::SetRow(scroll, 2); root.Children().Append(scroll);
        status_ = Controls::TextBlock(); status_.FontSize(11.5); status_.Foreground(Brush(92,92,92));
        status_.TextWrapping(TextWrapping::Wrap); Controls::Grid::SetRow(status_, 3); root.Children().Append(status_);

        auto changed = [this](auto const&, auto const&)
        {
            if (updating_) return;
            auto candidate = state_.specification;
            const int correlation = correlation_.SelectedIndex();
            candidate.correlationBasis = correlation == 1 ? "pearson" : correlation == 2
                ? "polychoric" : correlation == 3 ? "mixed" : "auto";
            candidate.scoreMethod = score_.SelectedIndex() == 1 ? "sum" : "mean";
            Submit(std::move(candidate));
        };
        correlation_.SelectionChanged(changed); score_.SelectionChanged(changed);
        exporter_ = TableExportService::Create(window_, root);
        window_.Content(root); ResizeLogical(window_, 900, 520);
    }

    void ScaleAnalysisView::AttachLifetime()
    {
        std::weak_ptr<ScaleAnalysisView> weak = shared_from_this();
        window_.Closed([weak](auto const&, auto const&) { if (auto view = weak.lock())
        {
            view->closed_ = true;
            auto children = std::move(view->derivedWindows_); view->derivedWindows_.clear();
            for (auto const& entry : children)
                if (entry.second->window) entry.second->window.Close();
            if (view->closedCallback_) view->closedCallback_();
        } });
    }

    void ScaleAnalysisView::SetClosedCallback(Closed callback)
    { closedCallback_ = std::move(callback); }

    void ScaleAnalysisView::SetChangedCallback(Changed callback)
    { changedCallback_ = std::move(callback); }

    void ScaleAnalysisView::SetCommandCallback(Command callback)
    { commandCallback_ = std::move(callback); ConfigureContextMenu(); }

    void ScaleAnalysisView::SetOpenScatterplotCallback(OpenScatterplot callback)
    { openScatterplotCallback_ = std::move(callback); }

    void ScaleAnalysisView::SetAvailableItems(
        std::vector<::rlispstat::core::ScaleItemSpecification> items)
    {
        availableItems_ = std::move(items);
        ConfigureAddMenu();
    }

    void ScaleAnalysisView::Submit(::rlispstat::core::ScaleAnalysisSpecification candidate)
    {
        if (changedCallback_) changedCallback_(candidate);
    }

    void ScaleAnalysisView::ConfigureAddMenu()
    {
        RefreshAddItemSelector(addItemList_ ? addItemList_.SelectedIndex() : 0);
    }

    void ScaleAnalysisView::RefreshAddItemSelector(int preferredIndex)
    {
        if (!addItemList_) return;
        auto lower = [](std::string value)
        {
            std::transform(value.begin(), value.end(), value.begin(),
                [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
            return value;
        };
        const std::string query = addItemSearch_ ? lower(to_string(addItemSearch_.Text())) : "";
        filteredAddItems_.clear();
        addItemList_.Items().Clear();
        for (auto const& available : availableItems_)
        {
            const bool used = std::any_of(state_.specification.items.begin(),
                state_.specification.items.end(), [&](auto const& item)
                { return item.variable == available.variable; });
            if (used) continue;
            if (!query.empty() && lower(available.variable).find(query) == std::string::npos)
                continue;
            filteredAddItems_.push_back(available);
            addItemList_.Items().Append(box_value(to_hstring(available.variable)));
        }
        const int count = static_cast<int>(filteredAddItems_.size());
        addItemEmpty_.Visibility(count == 0 ? Visibility::Visible : Visibility::Collapsed);
        addItemList_.Visibility(count == 0 ? Visibility::Collapsed : Visibility::Visible);
        if (count > 0)
        {
            addItemList_.SelectedIndex(std::clamp(preferredIndex, 0, count - 1));
            addItemList_.ScrollIntoView(addItemList_.SelectedItem());
        }
    }

    void ScaleAnalysisView::MoveAddItemSelection(int delta)
    {
        const int count = static_cast<int>(filteredAddItems_.size());
        if (count == 0) return;
        int index = addItemList_.SelectedIndex();
        if (index < 0) index = 0;
        else index = std::clamp(index + delta, 0, count - 1);
        addItemList_.SelectedIndex(index);
        addItemList_.ScrollIntoView(addItemList_.SelectedItem());
    }

    void ScaleAnalysisView::AddSelectedQuickItem()
    {
        int index = addItemList_.SelectedIndex();
        if (index < 0 && !filteredAddItems_.empty()) index = 0;
        if (index < 0 || index >= static_cast<int>(filteredAddItems_.size())) return;
        AddQuickItem(filteredAddItems_[static_cast<std::size_t>(index)]);
        addItemSearch_.Focus(FocusState::Programmatic);
    }

    void ScaleAnalysisView::AddQuickItem(
        ::rlispstat::core::ScaleItemSpecification const& item)
    {
        const bool used = std::any_of(state_.specification.items.begin(),
            state_.specification.items.end(), [&](auto const& current)
            { return current.variable == item.variable; });
        if (used) return;
        const int index = std::max(0, addItemList_ ? addItemList_.SelectedIndex() : 0);
        auto candidate = state_.specification;
        candidate.items.push_back(item);
        // Keep the selector responsive even while the exact scale analysis is
        // recomputed. Show() will replace this optimistic copy with the
        // canonical state as soon as the host accepts the specification.
        state_.specification = candidate;
        Submit(std::move(candidate));
        RefreshAddItemSelector(index);
    }

    void ScaleAnalysisView::ConfigureContextMenu()
    {
        auto menu = Controls::MenuFlyout();
        menu.Items().Append(CreateAddItemSubmenu());
        menu.Items().Append(Controls::MenuFlyoutSeparator());
        const bool enabled = state_.specification.items.size() >= 2;
        auto addCommand = [this, &menu, enabled](std::wstring const& label,
                                                 ::rlispstat::core::ScaleAnalysisChildKind kind)
        {
            auto item = Controls::MenuFlyoutItem(); item.Text(label); item.IsEnabled(enabled);
            item.Click([this, kind](auto const&, auto const&) { OpenDerived(kind); });
            menu.Items().Append(item);
        };
        addCommand(L"Reliability...", ::rlispstat::core::ScaleAnalysisChildKind::Reliability);
        addCommand(L"Inter-item correlations...", ::rlispstat::core::ScaleAnalysisChildKind::InterItemCorrelations);
        addCommand(L"Dimensionality...", ::rlispstat::core::ScaleAnalysisChildKind::Dimensionality);
        addCommand(L"Scale scores...", ::rlispstat::core::ScaleAnalysisChildKind::ScaleScores);
        auto save = Controls::MenuFlyoutSubItem(); save.Text(L"Save total scores to data");
        save.IsEnabled(!state_.rFitPending && ::rlispstat::core::ScaleAnalysisResultMatchesSpecification(state_.specification, state_.result));
        for (const std::string method : {"mean", "sum"}) {
            auto item = Controls::MenuFlyoutItem();
            item.Text(method == "mean" ? L"Mean of items" : L"Sum of items");
            item.Click([this, method](auto const&, auto const&) {
                if (commandCallback_) commandCallback_({"SCALE_SAVE_TOTAL", state_.id, method});
            });
            save.Items().Append(item);
        }
        menu.Items().Append(save);
        menu.Items().Append(Controls::MenuFlyoutSeparator());
        auto plots = Controls::MenuFlyoutSubItem(); plots.Text(L"Plots"); plots.IsEnabled(enabled);
        auto addPlot = [this, &plots](std::wstring const& label,
                                      ::rlispstat::core::ScaleAnalysisChildKind kind)
        {
            auto item = Controls::MenuFlyoutItem(); item.Text(label);
            item.Click([this, kind](auto const&, auto const&) { OpenDerived(kind); });
            plots.Items().Append(item);
        };
        addPlot(L"Item-rest correlations", ::rlispstat::core::ScaleAnalysisChildKind::ItemRestPlot);
        addPlot(L"Reliability if deleted", ::rlispstat::core::ScaleAnalysisChildKind::ReliabilityIfDeletedPlot);
        addPlot(L"Item distributions", ::rlispstat::core::ScaleAnalysisChildKind::ItemDistributionsPlot);
        addPlot(L"Scale-score distribution", ::rlispstat::core::ScaleAnalysisChildKind::ScaleScoreDistributionPlot);
        plots.Items().Append(Controls::MenuFlyoutSeparator());
        addPlot(L"Correlation heatmap", ::rlispstat::core::ScaleAnalysisChildKind::CorrelationHeatmap);
        menu.Items().Append(plots);
        menu.Items().Append(Controls::MenuFlyoutSeparator());
        auto information = Controls::MenuFlyoutItem(); information.Text(L"Analysis information...");
        information.Click([this](auto const&, auto const&) { ShowAnalysisInformation(); });
        menu.Items().Append(information);
        if (exporter_) {
            auto exportMenu = exporter_->CreateMenu(
                [this]() { return ScaleAnalysisExportPayload(state_); });
            AppendRCodeExportItems(exportMenu, commandCallback_, state_.id);
            menu.Items().Append(exportMenu);
        } else {
            AppendRCodeOnlyExportMenu(menu, commandCallback_, state_.id);
        }
        AttachWindowContextFlyout(window_, menu);
    }

    Controls::MenuFlyoutSubItem ScaleAnalysisView::CreateAddItemSubmenu()
    {
        auto add = Controls::MenuFlyoutSubItem();
        add.Text(L"Add item...");
        for (auto const& available : availableItems_)
        {
            const bool used = std::any_of(state_.specification.items.begin(),
                state_.specification.items.end(), [&](auto const& item)
                { return item.variable == available.variable; });
            if (used) continue;
            auto item = Controls::MenuFlyoutItem();
            item.Text(to_hstring(available.variable));
            item.Click([this, available](auto const&, auto const&)
                { AddQuickItem(available); });
            add.Items().Append(item);
        }
        if (add.Items().Size() == 0)
        {
            auto empty = Controls::MenuFlyoutItem();
            empty.Text(L"No eligible unused items");
            empty.IsEnabled(false);
            add.Items().Append(empty);
        }
        return add;
    }

    void ScaleAnalysisView::ShowAnalysisInformation()
    {
        std::ostringstream details;
        details << "Source Scale Analysis revision: " << state_.specification.revision
                << "\nSpecification fingerprint: " << state_.specification.fingerprint
                << "\nBackend: " << state_.result.backend
                << "\nImputations: " << state_.result.imputationCount
                << "\n\n" << state_.result.provenance;
        auto body = Controls::TextBlock(); body.Text(to_hstring(details.str()));
        body.TextWrapping(TextWrapping::Wrap);
        auto dialog = Controls::ContentDialog();
        ConfigureNativeDialog(dialog, window_, L"Scale Analysis information", body, L"Close", 560);
        dialog.ShowAsync();
    }

    void ScaleAnalysisView::OpenDerived(::rlispstat::core::ScaleAnalysisChildKind kind)
    {
        using ::rlispstat::core::ScaleAnalysisChildKind;
        auto candidate = state_.specification;
        if (kind == ScaleAnalysisChildKind::Reliability) {
            candidate.showReliability = true; candidate.computeOmega = true;
            candidate.showReliabilityStability = true;
        } else if (kind == ScaleAnalysisChildKind::InterItemCorrelations ||
                   kind == ScaleAnalysisChildKind::CorrelationHeatmap) {
            candidate.showCorrelations = true;
            candidate.showCorrelationHeatmap = kind == ScaleAnalysisChildKind::CorrelationHeatmap;
        } else if (kind == ScaleAnalysisChildKind::Dimensionality ||
                   kind == ScaleAnalysisChildKind::ScreePlot ||
                   kind == ScaleAnalysisChildKind::LoadingPlot ||
                   kind == ScaleAnalysisChildKind::Biplot) {
            candidate.showDimensionality = true;
            candidate.showScreePlot = kind == ScaleAnalysisChildKind::ScreePlot;
            candidate.showLoadingPlot = kind == ScaleAnalysisChildKind::LoadingPlot;
        } else if (kind == ScaleAnalysisChildKind::ScaleScores ||
                   kind == ScaleAnalysisChildKind::ScaleScoreDistributionPlot) {
            candidate.showScoreDistribution = true;
        } else if (kind == ScaleAnalysisChildKind::ItemDistributionsPlot) {
            candidate.showItemDistributions = true;
        } else if (kind == ScaleAnalysisChildKind::ItemRestPlot ||
                   kind == ScaleAnalysisChildKind::ReliabilityIfDeletedPlot) {
            candidate.showItemPlots = true;
        }
        if (::rlispstat::core::ScaleAnalysisSpecificationFingerprint(candidate) !=
            ::rlispstat::core::ScaleAnalysisSpecificationFingerprint(state_.specification))
            Submit(candidate);

        const int key = static_cast<int>(kind);
        auto found = derivedWindows_.find(key);
        if (found == derivedWindows_.end()) {
            auto derived = std::make_shared<DerivedWindow>();
            derived->window = CreateLinkEDAWindow();
            const bool mi = state_.result.imputationCount > 1 ||
                state_.result.backend == "multiple_imputation";
            derived->window.Title(to_hstring(
                ::rlispstat::core::ScaleAnalysisChildWindowTitle(kind, mi)));
            auto root = Controls::Grid(); root.Padding(Thickness{ 16,14,16,10 });
            root.RowSpacing(7); root.Background(Brush(255,255,255));
            root.RequestedTheme(ElementTheme::Light);
            auto bodyRow = Controls::RowDefinition();
            bodyRow.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
            root.RowDefinitions().Append(bodyRow);
            auto statusRow = Controls::RowDefinition();
            statusRow.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Auto));
            root.RowDefinitions().Append(statusRow);
            auto scroll = Controls::ScrollViewer();
            scroll.HorizontalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);
            scroll.VerticalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);
            derived->report = Controls::StackPanel(); derived->report.Spacing(7);
            derived->report.Name(L"LinkEDA.SnapshotTable"); scroll.Content(derived->report);
            root.Children().Append(scroll);
            derived->status = Controls::TextBlock(); derived->status.FontSize(11.5);
            derived->status.Foreground(Brush(92,92,92));
            derived->status.TextWrapping(TextWrapping::Wrap);
            Controls::Grid::SetRow(derived->status, 1); root.Children().Append(derived->status);
            derived->exporter = TableExportService::Create(derived->window, root);
            auto menu = Controls::MenuFlyout();
            const bool correlationResult = kind == ScaleAnalysisChildKind::InterItemCorrelations ||
                kind == ScaleAnalysisChildKind::CorrelationHeatmap;
            const bool dimensionalityResult = kind == ScaleAnalysisChildKind::Dimensionality ||
                kind == ScaleAnalysisChildKind::ScreePlot ||
                kind == ScaleAnalysisChildKind::LoadingPlot ||
                kind == ScaleAnalysisChildKind::Biplot;
            if (correlationResult) {
                auto basis = Controls::MenuFlyoutSubItem(); basis.Text(L"Correlation basis");
                const std::array<std::pair<std::wstring, std::string>, 4> bases{{
                    {L"Automatic", "auto"}, {L"Pearson", "pearson"},
                    {L"Polychoric", "polychoric"}, {L"Mixed correlations", "mixed"}}};
                for (auto const& value : bases) {
                    auto item = Controls::ToggleMenuFlyoutItem(); item.Text(value.first);
                    item.IsChecked(state_.specification.correlationBasis == value.second);
                    item.Click([this, selected=value.second](auto const&, auto const&) {
                        auto candidate=state_.specification; candidate.correlationBasis=selected; Submit(std::move(candidate)); });
                    basis.Items().Append(item);
                }
                menu.Items().Append(basis);
                if (kind == ScaleAnalysisChildKind::InterItemCorrelations) {
                    auto display = Controls::MenuFlyoutSubItem();
                    display.Text(L"Display");
                    std::weak_ptr<ScaleAnalysisView> weakView = shared_from_this();
                    std::weak_ptr<DerivedWindow> weakDerived = derived;
                    auto appendDisplayToggle = [&](std::wstring const& label, int option) {
                        auto item = Controls::ToggleMenuFlyoutItem();
                        item.Text(label);
                        item.Click([weakView, weakDerived, option](auto const&, auto const&) {
                            auto view = weakView.lock();
                            auto current = weakDerived.lock();
                            if (!view || !current) return;
                            if (option == 0)
                                current->showCorrelationStars = !current->showCorrelationStars;
                            else if (option == 1)
                                current->showCorrelationPValues = !current->showCorrelationPValues;
                            else
                                current->showCorrelationN = !current->showCorrelationN;
                            view->RenderDerived(ScaleAnalysisChildKind::InterItemCorrelations);
                        });
                        display.Items().Append(item);
                        return item;
                    };
                    derived->correlationStarsItem = appendDisplayToggle(
                        L"Significance stars", 0);
                    derived->correlationPValuesItem = appendDisplayToggle(L"p-values", 1);
                    derived->correlationNItem = appendDisplayToggle(L"Sample sizes", 2);
                    menu.Items().Append(display);
                }
            }
            if (dimensionalityResult) {
                if (state_.result.imputationCount > 1) {
                    auto imputations = Controls::MenuFlyoutSubItem();
                    imputations.Text(L"Imputation");
                    std::weak_ptr<ScaleAnalysisView> weakView = shared_from_this();
                    std::weak_ptr<DerivedWindow> weakDerived = derived;
                    for (int imputation = 1;
                         imputation <= state_.result.imputationCount; ++imputation) {
                        auto item = Controls::ToggleMenuFlyoutItem();
                        item.Text(to_hstring("Imputation " + std::to_string(imputation)));
                        item.IsChecked(derived->selectedDimensionImputation == imputation);
                        item.Click([weakView, weakDerived, kind, imputation]
                                   (auto const&, auto const&) {
                            auto view = weakView.lock();
                            auto current = weakDerived.lock();
                            if (!view || !current) return;
                            current->selectedDimensionImputation = imputation;
                            view->RenderDerived(kind);
                        });
                        imputations.Items().Append(item);
                    }
                    menu.Items().Append(imputations);
                }
                auto factors = Controls::MenuFlyoutSubItem(); factors.Text(L"Number of factors");
                const int maximum = std::max(1,std::min(8,static_cast<int>(state_.specification.items.size())-1));
                for(int count=0;count<=maximum;++count){auto item=Controls::ToggleMenuFlyoutItem();
                    if (count == 0) item.Text(L"Automatic (parallel analysis)");
                    else item.Text(to_hstring(std::to_string(count)));
                    item.IsChecked(state_.specification.factorCount==count);
                    item.Click([this,count](auto const&,auto const&){auto candidate=state_.specification;
                        candidate.factorCount=count;Submit(std::move(candidate));});factors.Items().Append(item);}
                menu.Items().Append(factors);
                auto extraction=Controls::MenuFlyoutSubItem();extraction.Text(L"Extraction");
                const std::array<std::pair<std::wstring, std::string>, 3> extractions{{
                    {L"Minimum residual", "minres"}, {L"Maximum likelihood", "ml"},
                    {L"Principal axis", "pa"}}};
                for(auto const& value : extractions){
                    auto item=Controls::ToggleMenuFlyoutItem();item.Text(value.first);item.IsChecked(state_.specification.extraction==value.second);
                    item.Click([this,selected=value.second](auto const&,auto const&){auto candidate=state_.specification;
                        candidate.extraction=selected;Submit(std::move(candidate));});extraction.Items().Append(item);}menu.Items().Append(extraction);
                auto rotation=Controls::MenuFlyoutSubItem();rotation.Text(L"Rotation");
                const std::array<std::pair<std::wstring, std::string>, 4> rotations{{
                    {L"Oblimin", "oblimin"}, {L"Varimax", "varimax"},
                    {L"Promax", "promax"}, {L"None", "none"}}};
                for(auto const& value : rotations){
                    auto item=Controls::ToggleMenuFlyoutItem();item.Text(value.first);item.IsChecked(state_.specification.rotation==value.second);
                    item.Click([this,selected=value.second](auto const&,auto const&){auto candidate=state_.specification;
                        candidate.rotation=selected;Submit(std::move(candidate));});rotation.Items().Append(item);}menu.Items().Append(rotation);

                auto plots = Controls::MenuFlyoutSubItem(); plots.Text(L"Plots");
                std::weak_ptr<ScaleAnalysisView> weakView = shared_from_this();
                std::weak_ptr<DerivedWindow> weakDerived = derived;
                auto addPlot = [&](std::wstring const& label,
                                   ScaleAnalysisChildKind plotKind) {
                    auto item = Controls::MenuFlyoutItem(); item.Text(label);
                    item.Click([weakView, weakDerived, plotKind](auto const&, auto const&) {
                        auto view = weakView.lock();
                        auto source = weakDerived.lock();
                        if (!view || !source) return;
                        const int imputation = source->selectedDimensionImputation;
                        view->OpenDerived(plotKind);
                        auto found = view->derivedWindows_.find(static_cast<int>(plotKind));
                        if (found == view->derivedWindows_.end()) return;
                        found->second->selectedDimensionImputation = imputation;
                        view->RenderDerived(plotKind);
                    });
                    plots.Items().Append(item);
                };
                addPlot(L"Scree / parallel analysis", ScaleAnalysisChildKind::ScreePlot);
                addPlot(L"Factor loadings", ScaleAnalysisChildKind::LoadingPlot);
                addPlot(L"Factor biplot", ScaleAnalysisChildKind::Biplot);
                menu.Items().Append(plots);
            }
            if (menu.Items().Size()) menu.Items().Append(Controls::MenuFlyoutSeparator());
            auto information=Controls::MenuFlyoutItem();information.Text(L"Analysis information...");
            information.Click([this](auto const&,auto const&){ShowAnalysisInformation();});menu.Items().Append(information);
            menu.Items().Append(Controls::MenuFlyoutSeparator());
            std::weak_ptr<ScaleAnalysisView> exportWeak=shared_from_this();
            std::weak_ptr<DerivedWindow> exportDerived = derived;
            auto exportMenu = derived->exporter->CreateMenu([exportWeak,exportDerived,kind](){
                if(auto view=exportWeak.lock()){
                    if(ScaleAnalysisResultMatchesSpecification(view->state_.specification,view->state_.result))
                        if (auto source = exportDerived.lock())
                            return ScaleAnalysisDerivedExportPayload(view->state_,kind,
                                source->selectedDimensionImputation);
                }
                return TableExportPayload{};
            });
            AppendRCodeExportItems(exportMenu, commandCallback_, state_.id);
            menu.Items().Append(exportMenu);
            AttachWindowContextFlyout(derived->window, menu);
            derived->window.Content(root);
            SetWindowDataSheetGroup(derived->window, state_.group);
            SetWindowSnapshotSource(derived->window, "output", state_.id, state_.group);
            std::weak_ptr<ScaleAnalysisView> weak = shared_from_this();
            derived->window.Closed([weak, key](auto const&, auto const&) {
                if (auto view = weak.lock()) view->derivedWindows_.erase(key);
            });
            derivedWindows_[key] = derived; found = derivedWindows_.find(key);
        }
        RenderDerived(kind);
        PresentWindowOnce(found->second->window, found->second->presented, 920, 600);
    }

    void ScaleAnalysisView::RenderDerived(::rlispstat::core::ScaleAnalysisChildKind kind)
    {
        using namespace ::rlispstat::core;
        const int key = static_cast<int>(kind);
        auto found = derivedWindows_.find(key); if (found == derivedWindows_.end()) return;
        auto derived = found->second; derived->report.Children().Clear();
        const bool current = ScaleAnalysisResultMatchesSpecification(
            state_.specification, state_.result);
        if (!ScaleAnalysisResultMayBePresented(state_)) {
            auto waiting = Controls::TextBlock();
            waiting.Text(L"Updating this result from the exact current scale specification...");
            waiting.Foreground(Brush(92,92,92)); derived->report.Children().Append(waiting);
            derived->status.Text(L"Waiting for the current Scale Analysis revision; stale results are not displayed.");
            return;
        }
        if (current) derived->identity = BuildScaleAnalysisDerivedIdentity(state_, kind);
        if (kind == ScaleAnalysisChildKind::InterItemCorrelations) {
            const bool inferential = state_.result.correlations.basis == "pearson" &&
                std::any_of(state_.result.correlations.pValues.begin(),
                    state_.result.correlations.pValues.end(), [](std::string const& text) {
                        char* end = nullptr;
                        const double value = std::strtod(text.c_str(), &end);
                        return end != text.c_str() && end && *end == '\0' &&
                            std::isfinite(value);
                    });
            const bool hasSampleSizes =
                state_.result.correlations.sampleSizes.size() ==
                    state_.result.correlations.values.size() ||
                !state_.result.scaleSummary.nUsed.empty();
            if (derived->correlationStarsItem) {
                derived->correlationStarsItem.IsChecked(derived->showCorrelationStars);
                derived->correlationStarsItem.IsEnabled(inferential);
            }
            if (derived->correlationPValuesItem) {
                derived->correlationPValuesItem.IsChecked(derived->showCorrelationPValues);
                derived->correlationPValuesItem.IsEnabled(inferential);
            }
            if (derived->correlationNItem) {
                derived->correlationNItem.IsChecked(derived->showCorrelationN);
                derived->correlationNItem.IsEnabled(hasSampleSizes);
            }
        }
        auto appendTitle = [&derived](std::wstring const& value) {
            auto title = Controls::TextBlock(); title.Text(value); title.FontSize(16);
            title.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
            title.Margin(Thickness{0,5,0,4}); derived->report.Children().Append(title);
        };
        auto appendNote = [&derived](std::string const& value) {
            if (value.empty()) return; auto note = Controls::TextBlock();
            note.Text(to_hstring(value)); note.FontSize(11); note.Foreground(Brush(92,92,92));
            note.TextWrapping(TextWrapping::Wrap); derived->report.Children().Append(note);
        };
        auto appendTable = [&derived](std::vector<double> const& widths,
                                      std::vector<std::string> const& labels,
                                      std::vector<std::vector<std::string>> const& rows) {
            auto makeRow = [&](std::vector<std::string> const& values, bool header, std::size_t rowIndex) {
                auto grid = Controls::Grid();
                for (double width : widths) { auto column = Controls::ColumnDefinition();
                    column.Width(GridLengthHelper::FromPixels(width)); grid.ColumnDefinitions().Append(column); }
                for (std::size_t column = 0; column < values.size(); ++column) {
                    auto cell = TableCell(to_hstring(values[column]).c_str(), header,
                        column == 0, false, false, !header && rowIndex % 2 == 1);
                    Controls::Grid::SetColumn(cell, static_cast<int>(column)); grid.Children().Append(cell);
                }
                return grid;
            };
            derived->report.Children().Append(makeRow(labels, true, 0));
            for (std::size_t row = 0; row < rows.size(); ++row)
                derived->report.Children().Append(makeRow(rows[row], false, row));
        };
        auto appendCorrelationMatrix = [this, &derived](
            CorrelationMatrixRenderPlan const& plan, std::string const& basis) {
            auto canvas = Controls::Canvas();
            canvas.Name(L"LinkEDA.SnapshotTable");
            canvas.Width(plan.layout.width); canvas.Height(plan.layout.height);
            canvas.Background(Brush(255,255,255));
            auto addRect = [&canvas](Rect const& rect, Media::Brush const& fill) {
                auto shape = Shapes::Rectangle(); shape.Width(rect.width);
                shape.Height(rect.height); shape.Fill(fill);
                Controls::Canvas::SetLeft(shape, rect.x);
                Controls::Canvas::SetTop(shape, rect.y);
                canvas.Children().Append(shape);
            };
            auto addText = [&canvas](std::string const& value, Rect const& rect,
                                      double size, bool bold, bool muted,
                                      TextAlignment alignment) {
                auto label = Controls::TextBlock(); label.Text(to_hstring(value));
                label.FontSize(size);
                label.Foreground(Brush(muted ? 105 : 42, muted ? 105 : 44,
                    muted ? 105 : 48));
                if (bold) label.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
                label.Width(std::max(0.0, rect.width));
                label.Height(std::max(0.0, rect.height));
                label.TextAlignment(alignment);
                label.TextTrimming(TextTrimming::CharacterEllipsis);
                Controls::Canvas::SetLeft(label, rect.x);
                Controls::Canvas::SetTop(label, rect.y);
                canvas.Children().Append(label);
                return label;
            };
            addRect(plan.headerRuleRect, Brush(222,225,229));
            for (std::size_t index = 0; index < plan.columnHeaders.size(); ++index) {
                auto const& header = plan.columnHeaders[index];
                auto label = addText(header.text, header.rect, 11.5, true, false,
                    TextAlignment::Center);
                AttachDirectMenu(label, CreateItemMenu(index));
            }
            for (std::size_t index = 0; index < plan.rowHeaders.size(); ++index) {
                auto const& header = plan.rowHeaders[index];
                auto label = addText(header.text, header.rect, 11.5, true, false,
                    TextAlignment::Left);
                AttachDirectMenu(label, CreateItemMenu(index));
            }
            for (auto const& cell : plan.cells) {
                if (cell.background == CorrelationCellBackground::Diagonal)
                    addRect(cell.rect, Brush(247,248,249));
                else if (cell.background == CorrelationCellBackground::Selected)
                    addRect(cell.rect, Brush(223,237,250));
                for (auto const& item : cell.textItems) {
                    auto label = addText(item.text, item.rect,
                        item.muted ? 10.0 : 11.5, !item.muted, item.muted,
                        TextAlignment::Center);
                    const std::string statistic = basis == "polychoric"
                        ? "Polychoric correlation" : basis == "mixed"
                        ? "Mixed correlation" : "Pearson r";
                    const bool pooledPearson =
                        state_.result.imputationCount > 1 && basis == "pearson";
                    auto menu = CreateStatisticCellMenu(
                        to_hstring(statistic).c_str(), label,
                        AnalysisStatisticContext(statistic, "correlation", basis,
                            {}, state_.result.imputationCount > 1,
                            pooledPearson ? "mice::pool.scalar" : ""));
                    auto const& variables = state_.result.correlations.variables;
                    if (cell.row != cell.column && cell.row >= 0 && cell.column >= 0 &&
                        static_cast<std::size_t>(cell.row) < variables.size() &&
                        static_cast<std::size_t>(cell.column) < variables.size())
                    {
                        const auto xVariable = variables[static_cast<std::size_t>(cell.column)];
                        const auto yVariable = variables[static_cast<std::size_t>(cell.row)];
                        auto open = Controls::MenuFlyoutItem();
                        open.Text(L"Open Scatterplot");
                        std::weak_ptr<ScaleAnalysisView> weak = shared_from_this();
                        open.Click([weak, xVariable, yVariable](auto const&, auto const&)
                        {
                            if (auto view = weak.lock(); view && view->openScatterplotCallback_)
                                view->openScatterplotCallback_(xVariable, yVariable);
                        });
                        menu.Items().InsertAt(0, open);
                        menu.Items().InsertAt(1, Controls::MenuFlyoutSeparator());
                    }
                    AttachDirectMenu(label, menu);
                }
            }
            if (plan.showEmptyMessage)
                addText(plan.emptyMessage, plan.emptyMessageRect, 12.0, false,
                    true, TextAlignment::Left);
            std::weak_ptr<ScaleAnalysisView> weak = shared_from_this();
            canvas.PointerPressed([weak, derived, canvas, layout = plan.layout]
                (auto const&, auto const& event) {
                if (!event.GetCurrentPoint(canvas).Properties().IsLeftButtonPressed()) return;
                const auto point = event.GetCurrentPoint(canvas).Position();
                const auto hit = HitTestCorrelationMatrix(layout, {point.X, point.Y});
                if (hit.kind != CorrelationMatrixHitKind::Cell) return;
                derived->selectedCorrelationRow = hit.row;
                derived->selectedCorrelationColumn = hit.column;
                event.Handled(true);
                if (auto view = weak.lock())
                    view->RenderDerived(ScaleAnalysisChildKind::InterItemCorrelations);
            });
            derived->report.Children().Append(canvas);
        };
        auto appendDimensionalityReport = [this, &derived](
            DimensionalityReportViewModel const& model) {
            auto canvas = Controls::Canvas();
            canvas.Name(L"LinkEDA.SnapshotTable");
            canvas.Width(model.layout.preferredWidth);
            canvas.Height(model.layout.preferredHeight);
            canvas.Background(Brush(255,255,255));
            auto addRect = [&canvas](Rect const& rect, Media::Brush const& fill) {
                auto shape = Shapes::Rectangle(); shape.Width(rect.width);
                shape.Height(rect.height); shape.Fill(fill);
                Controls::Canvas::SetLeft(shape, rect.x);
                Controls::Canvas::SetTop(shape, rect.y);
                canvas.Children().Append(shape);
            };
            for (auto const& highlight : model.renderPlan.highlights)
                addRect(highlight.rect,
                    highlight.role == DimensionalityReportHighlightRole::Strong
                        ? Brush(255,235,194) : Brush(230,242,251));
            for (auto const& rule : model.renderPlan.rules) {
                auto line = Shapes::Line(); line.X1(rule.start.x); line.Y1(rule.start.y);
                line.X2(rule.end.x); line.Y2(rule.end.y);
                line.Stroke(Brush(219,221,224)); line.StrokeThickness(1);
                canvas.Children().Append(line);
            }
            for (auto const& item : model.renderPlan.texts) {
                auto label = Controls::TextBlock(); label.Text(to_hstring(item.text));
                const bool muted = item.role == DimensionalityReportTextRole::Muted;
                const bool header = item.role == DimensionalityReportTextRole::Section ||
                    item.role == DimensionalityReportTextRole::RightHeader;
                const bool right = item.role == DimensionalityReportTextRole::Right ||
                    item.role == DimensionalityReportTextRole::RightHeader;
                label.FontSize(muted ? 11 : 12);
                label.Foreground(Brush(muted ? 110 : 38, muted ? 110 : 40,
                    muted ? 110 : 44));
                if (header)
                    label.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
                label.TextAlignment(right ? TextAlignment::Right : TextAlignment::Left);
                label.TextTrimming(TextTrimming::CharacterEllipsis);
                label.Width(item.rect.width); label.Height(item.rect.height);
                Controls::Canvas::SetLeft(label, item.rect.x);
                Controls::Canvas::SetTop(label, item.rect.y);
                canvas.Children().Append(label);
                if (item.role == DimensionalityReportTextRole::RightHeader) {
                    std::string statistic = item.text == "Parallel"
                        ? "Parallel eigenvalue" : item.text;
                    AttachDirectMenu(label, CreateStatisticCellMenu(
                        to_hstring(statistic).c_str(), label,
                        AnalysisStatisticContext(statistic, "dimensionality",
                            state_.specification.extraction + " with " +
                            state_.specification.rotation + " rotation",
                            {}, state_.result.imputationCount > 1, "")));
                }
            }
            for (std::size_t index = 0;
                 index < model.report.loadingRows.size() &&
                 index < model.layout.variableRects.size(); ++index) {
                auto const& rect = model.layout.variableRects[index];
                auto target = Controls::Border(); target.Width(rect.width);
                target.Height(rect.height); target.Background(TransparentBrush());
                Controls::Canvas::SetLeft(target, rect.x);
                Controls::Canvas::SetTop(target, rect.y);
                AttachDirectMenu(target, CreateItemMenu(index));
                canvas.Children().Append(target);
            }
            std::weak_ptr<ScaleAnalysisView> weak = shared_from_this();
            canvas.PointerPressed([weak, derived, canvas, report = model.report,
                                   layout = model.layout]
                (auto const&, auto const& event) {
                if (!event.GetCurrentPoint(canvas).Properties().IsLeftButtonPressed()) return;
                const auto point = event.GetCurrentPoint(canvas).Position();
                const auto hit = HitTestDimensionalityReport(report, layout,
                    {point.X, point.Y});
                const auto action = DimensionalityReportPrimaryActionForHit(hit);
                if (action.kind == DimensionalityReportActionKind::None) return;
                if (action.kind == DimensionalityReportActionKind::ClearFocus) {
                    derived->focusedDimensionComponent = 0;
                    derived->focusedDimensionVariable.clear();
                } else {
                    derived->focusedDimensionComponent = action.component;
                    derived->focusedDimensionVariable =
                        action.kind == DimensionalityReportActionKind::FocusLoading
                            ? action.variable : std::string{};
                }
                event.Handled(true);
                if (auto view = weak.lock())
                    view->RenderDerived(ScaleAnalysisChildKind::Dimensionality);
            });
            derived->report.Children().Append(canvas);
        };
        auto appendBars = [&derived, &appendTitle](std::wstring const& title,
                                                   std::vector<std::pair<std::string,std::string>> const& rows) {
            if (rows.empty()) return; appendTitle(title); double largest = 0.0;
            for (auto const& row : rows) largest = std::max(largest, std::abs(std::strtod(row.second.c_str(), nullptr)));
            if (!(largest > 0.0)) largest = 1.0;
            for (auto const& row : rows) {
                auto grid = Controls::Grid(); grid.Height(25); grid.ColumnSpacing(8);
                for (double width : {220.0, 420.0, 100.0}) { auto column = Controls::ColumnDefinition();
                    column.Width(GridLengthHelper::FromPixels(width)); grid.ColumnDefinitions().Append(column); }
                auto label = Controls::TextBlock(); label.Text(to_hstring(row.first)); label.FontSize(11);
                label.TextTrimming(TextTrimming::CharacterEllipsis); grid.Children().Append(label);
                const double number = std::strtod(row.second.c_str(), nullptr);
                auto track = Controls::Grid(); track.Width(420); track.Height(12); track.Background(Brush(242,244,247));
                auto bar = Shapes::Rectangle(); bar.Height(12);
                bar.Width(std::max(1.0, 420.0 * std::abs(number) / largest));
                bar.Fill(number < 0 ? Brush(225,87,89) : Brush(52,122,184)); track.Children().Append(bar);
                Controls::Grid::SetColumn(track, 1); grid.Children().Append(track);
                auto value = Controls::TextBlock(); value.Text(to_hstring(row.second)); value.FontSize(11);
                value.TextAlignment(TextAlignment::Right); Controls::Grid::SetColumn(value, 2); grid.Children().Append(value);
                derived->report.Children().Append(grid);
            }
        };
        auto parseNumber = [](std::string const& raw, double& value) {
            char* end = nullptr; value = std::strtod(raw.c_str(), &end);
            return end != raw.c_str() && std::isfinite(value);
        };
        auto appendLineSeries = [&derived, &appendTitle, &parseNumber](
            std::wstring const& title, std::vector<ScalePlotSeriesResult const*> const& source)
        {
            if (source.empty()) return;
            struct Series { std::string name, kind; std::vector<double> values; };
            std::vector<Series> series; double low=std::numeric_limits<double>::infinity();
            double high=-std::numeric_limits<double>::infinity(); std::size_t count=0;
            for(auto const* input:source){Series parsed{input->name,input->kind,{}};
                for(auto const& raw:input->values){double number=0;if(parseNumber(raw,number)){
                    parsed.values.push_back(number);low=std::min(low,number);high=std::max(high,number);}}
                count=std::max(count,parsed.values.size());if(!parsed.values.empty())series.push_back(std::move(parsed));}
            if(series.empty()||!count)return;if(!(high>low)){low-=.5;high+=.5;}appendTitle(title);
            constexpr double width=780,height=260,left=52,right=190,top=14,bottom=30;
            const double plotWidth=width-left-right,plotHeight=height-top-bottom;
            auto canvas=Controls::Canvas();canvas.Width(width);canvas.Height(height);canvas.Background(Brush(255,255,255));
            canvas.Children().Append(ReportLine(left,top,left,top+plotHeight,Brush(95,100,108),1));
            canvas.Children().Append(ReportLine(left,top+plotHeight,left+plotWidth,top+plotHeight,Brush(95,100,108),1));
            const std::array<Windows::UI::Color,5> palette{
                Windows::UI::ColorHelper::FromArgb(255,32,113,180),Windows::UI::ColorHelper::FromArgb(255,225,87,89),
                Windows::UI::ColorHelper::FromArgb(255,42,157,143),Windows::UI::ColorHelper::FromArgb(255,111,78,153),
                Windows::UI::ColorHelper::FromArgb(255,244,162,97)};
            std::size_t legend=0;
            for(std::size_t s=0;s<series.size();++s){auto const& current=series[s];
                const bool per=current.kind.find("_imputation")!=std::string::npos ||
                    (current.kind=="score_distribution"&&series.size()>1);
                auto raw=palette[s%palette.size()];raw.A=per?62:242;auto stroke=Media::SolidColorBrush(raw);
                double px=0,py=0;bool started=false;
                for(std::size_t i=0;i<current.values.size();++i){const double x=left+(count<=1?plotWidth/2:
                    plotWidth*static_cast<double>(i)/static_cast<double>(count-1));
                    const double y=top+(high-current.values[i])/(high-low)*plotHeight;
                    if(started)canvas.Children().Append(ReportLine(px,py,x,y,stroke,per?.8:2.2));
                    if(!per){auto point=Shapes::Ellipse();point.Width(5);point.Height(5);point.Fill(stroke);
                        Controls::Canvas::SetLeft(point,x-2.5);Controls::Canvas::SetTop(point,y-2.5);canvas.Children().Append(point);}
                    px=x;py=y;started=true;}
                if(!per||series.size()<=6){auto label=ReportLabel(to_hstring(current.name).c_str(),left+plotWidth+12,
                    top+17*legend++,9.5,Brush(72,72,72));label.Width(right-18);label.TextTrimming(TextTrimming::CharacterEllipsis);
                    canvas.Children().Append(label);}}
            if(series.size()>6){auto label=ReportLabel(L"Faint lines: individual imputations",left+plotWidth+12,top,9.5,Brush(72,72,72));
                label.Width(right-18);label.TextWrapping(TextWrapping::Wrap);canvas.Children().Append(label);}
            canvas.Children().Append(ReportLabel(to_hstring(FormatDouble(high,2)).c_str(),2,top-5,9,Brush(92,92,92)));
            canvas.Children().Append(ReportLabel(to_hstring(FormatDouble(low,2)).c_str(),2,top+plotHeight-5,9,Brush(92,92,92)));
            derived->report.Children().Append(canvas);
        };
        auto appendBiplot = [&derived, &appendTitle, &appendNote](
            DimensionalityBiplotPlotState const& plot,
            std::string const& focusedVariable)
        {
            if (!plot.ok) {
                appendNote("A factor biplot requires at least two retained factors and valid factor scores.");
                return;
            }
            appendTitle(to_hstring(plot.title).c_str());
            constexpr double width = 800.0;
            constexpr double height = 520.0;
            constexpr double left = 70.0;
            constexpr double right = 24.0;
            constexpr double top = 20.0;
            constexpr double bottom = 54.0;
            const Rect plotRect{left, top, width - left - right, height - top - bottom};
            const DataViewport viewport{plot.xmin, plot.xmax, plot.ymin, plot.ymax};
            auto canvas = Controls::Canvas();
            canvas.Width(width); canvas.Height(height); canvas.Background(Brush(255,255,255));
            const auto origin = DataToScreen({0.0, 0.0}, viewport, plotRect, true);
            if (std::isfinite(origin.x))
                canvas.Children().Append(ReportLine(origin.x, plotRect.y,
                    origin.x, plotRect.y + plotRect.height, Brush(223,225,229), 1.0));
            if (std::isfinite(origin.y))
                canvas.Children().Append(ReportLine(plotRect.x, origin.y,
                    plotRect.x + plotRect.width, origin.y, Brush(223,225,229), 1.0));
            canvas.Children().Append(ReportLine(plotRect.x, plotRect.y,
                plotRect.x, plotRect.y + plotRect.height, Brush(95,100,108), 1.0));
            canvas.Children().Append(ReportLine(plotRect.x, plotRect.y + plotRect.height,
                plotRect.x + plotRect.width, plotRect.y + plotRect.height,
                Brush(95,100,108), 1.0));
            for (auto const& score : plot.scores) {
                const auto point = DataToScreen({score.x, score.y}, viewport, plotRect, true);
                if (!std::isfinite(point.x) || !std::isfinite(point.y)) continue;
                auto mark = Shapes::Ellipse(); mark.Width(5.5); mark.Height(5.5);
                mark.Fill(Brush(63,67,73));
                Controls::Canvas::SetLeft(mark, point.x - 2.75);
                Controls::Canvas::SetTop(mark, point.y - 2.75);
                canvas.Children().Append(mark);
            }
            const auto loadings = BuildDimensionalityBiplotRenderPlan(
                plot.loadings, viewport, plotRect, focusedVariable);
            for (auto const& loading : loadings.loadings) {
                const bool focused = loading.focused;
                auto stroke = focused ? Brush(196,62,68) : Brush(31,111,174);
                const double thickness = focused ? 2.4 : 1.7;
                canvas.Children().Append(ReportLine(loading.start.x, loading.start.y,
                    loading.end.x, loading.end.y, stroke, thickness));
                canvas.Children().Append(ReportLine(loading.end.x, loading.end.y,
                    loading.arrowHeadA.x, loading.arrowHeadA.y, stroke, thickness));
                canvas.Children().Append(ReportLine(loading.end.x, loading.end.y,
                    loading.arrowHeadB.x, loading.arrowHeadB.y, stroke, thickness));
                auto label = ReportLabel(to_hstring(loading.variable).c_str(),
                    loading.labelAnchor.x, loading.labelAnchor.y, 10.5, stroke);
                label.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
                canvas.Children().Append(label);
            }
            auto xLabel = ReportLabel(to_hstring(plot.xLabel).c_str(),
                left + plotRect.width / 2.0 - 55.0, height - 27.0, 11.5, Brush(54,54,54));
            xLabel.Width(110); xLabel.TextAlignment(TextAlignment::Center);
            canvas.Children().Append(xLabel);
            auto yLabel = ReportLabel(to_hstring(plot.yLabel).c_str(), 4.0,
                top + plotRect.height / 2.0 - 10.0, 11.5, Brush(54,54,54));
            yLabel.Width(62); yLabel.TextAlignment(TextAlignment::Center);
            canvas.Children().Append(yLabel);
            derived->report.Children().Append(canvas);
        };
        auto appendHeatmap = [&derived,&appendTitle,&parseNumber](ScaleCorrelationResult const& correlations){
            const std::size_t count=correlations.variables.size();if(!count||correlations.values.size()!=count*count)return;
            appendTitle(L"Correlation heatmap");const double cell=std::clamp(500.0/static_cast<double>(count),28.0,58.0),label=170.0;
            auto canvas=Controls::Canvas();canvas.Width(label+cell*count+10);canvas.Height(label+cell*count+10);canvas.Background(Brush(255,255,255));
            for(std::size_t i=0;i<count;++i){auto topLabel=ReportLabel(to_hstring(correlations.variables[i]).c_str(),label+i*cell,label-24,9,Brush(72,72,72));
                topLabel.Width(cell);topLabel.TextAlignment(TextAlignment::Center);topLabel.TextTrimming(TextTrimming::CharacterEllipsis);canvas.Children().Append(topLabel);
                auto rowLabel=ReportLabel(to_hstring(correlations.variables[i]).c_str(),0,label+i*cell+6,9,Brush(72,72,72));
                rowLabel.Width(label-8);rowLabel.TextAlignment(TextAlignment::Right);rowLabel.TextTrimming(TextTrimming::CharacterEllipsis);canvas.Children().Append(rowLabel);}
            for(std::size_t row=0;row<count;++row)for(std::size_t column=0;column<count;++column){double value=0;
                if(!parseNumber(correlations.values[row*count+column],value))continue;const double strength=std::clamp(std::abs(value),0.0,1.0);
                const uint8_t pale=static_cast<uint8_t>(245-125*strength);auto rect=Shapes::Rectangle();rect.Width(cell-1);rect.Height(cell-1);
                rect.Fill(value<0?Brush(225,pale,pale):Brush(pale,pale,225));Controls::Canvas::SetLeft(rect,label+column*cell);
                Controls::Canvas::SetTop(rect,label+row*cell);canvas.Children().Append(rect);
                if(cell>=40){auto text=ReportLabel(to_hstring(correlations.values[row*count+column]).c_str(),label+column*cell+3,
                    label+row*cell+7,8.5,Brush(42,42,42));text.Width(cell-6);text.TextAlignment(TextAlignment::Center);canvas.Children().Append(text);}}
            derived->report.Children().Append(canvas);
        };

        appendTitle(to_hstring(ScaleAnalysisChildWindowTitle(kind,
            state_.result.imputationCount > 1 || state_.result.backend == "multiple_imputation")).c_str());
        appendNote(current
            ? "Source revision " + std::to_string(state_.specification.revision) +
                " · exact specification fingerprint verified"
            : "Auto-fit is off. This is the last completed result; it does not reflect the current scale specification.");
        if (kind == ScaleAnalysisChildKind::Reliability) {
            const auto& s = state_.result.scaleSummary;
            appendTable({210,130}, {"Statistic","Estimate"}, {
                {"Cronbach alpha",s.alpha}, {"Standardized alpha",s.standardizedAlpha},
                {"Omega total",s.omegaTotal}, {"Mean inter-item correlation",s.meanInterItemCorrelation}});
            std::vector<std::vector<std::string>> rows;
            for (auto const& value : state_.result.reliabilityByImputation)
                rows.push_back({value.imputation,value.alpha,value.standardizedAlpha,value.omegaTotal});
            appendTitle(L"Reliability across imputations");
            appendTable({120,130,170,130}, {"Imputation","Alpha","Standardized alpha","Omega total"}, rows);
            appendNote("Reliability is computed separately by psych in every completed dataset; aggregates are descriptive, not Rubin-pooled.");
        } else if (kind == ScaleAnalysisChildKind::InterItemCorrelations) {
            auto const& correlations = state_.result.correlations;
            appendNote("Basis: " + correlations.basis + " · " + correlations.status);
            appendCorrelationMatrix(BuildScaleCorrelationRenderPlan(
                state_.result, derived->selectedCorrelationRow,
                derived->selectedCorrelationColumn,
                derived->showCorrelationStars,
                derived->showCorrelationPValues,
                derived->showCorrelationN), correlations.basis);
            appendNote("Pearson correlations use Fisher-z plus mice::pool.scalar. "
                "Polychoric and mixed matrices are descriptive element-wise means "
                "across imputations and are explicitly not pooled.");
        } else if (kind == ScaleAnalysisChildKind::Dimensionality) {
            const auto dimensionality = BuildScaleDimensionalityPresentation(
                state_.result, derived->selectedDimensionImputation);
            std::vector<std::vector<std::string>> factorRows;
            for (auto const& row : ScaleFactorCountRows(state_.result))
                factorRows.push_back({std::to_string(row.factorCount),std::to_string(row.imputations),FormatDouble(100*row.proportion,1)+"%"});
            if (state_.result.imputationCount > 1) {
                appendTitle(L"Suggested factor count across imputations");
                appendTable({120,140,120},{"Factors","Imputations","Proportion"},factorRows);
            }
            appendDimensionalityReport(BuildScaleDimensionalityReportViewModel(
                state_.result, derived->focusedDimensionComponent,
                derived->focusedDimensionVariable, dimensionality.imputation, state_.specification.dimensionalityMethod));
            appendNote(dimensionality.status);
        } else if (kind == ScaleAnalysisChildKind::ScaleScores) {
            auto const& s=state_.result.scores;
            appendTable({120,145,120,120,120,120},{"Method",
                state_.result.imputationCount>1?"N per imputation":"Valid N",
                "Mean","SD","Minimum","Maximum"},
                {{s.method,s.validN,s.mean,s.sd,s.minimum,s.maximum}}); appendNote(s.status);
        } else if (kind == ScaleAnalysisChildKind::CorrelationHeatmap) {
            appendHeatmap(state_.result.correlations);
        } else {
            std::vector<std::pair<std::string,std::string>> values; std::string requested;
            if(kind==ScaleAnalysisChildKind::ItemRestPlot||kind==ScaleAnalysisChildKind::ReliabilityIfDeletedPlot){
                for(auto const& row:state_.result.items)values.push_back({row.variable,
                    kind==ScaleAnalysisChildKind::ItemRestPlot?row.itemRestCorrelation:row.alphaIfDeleted});
            } else if(kind==ScaleAnalysisChildKind::LoadingPlot){
                const auto dimensionality = BuildScaleDimensionalityPresentation(
                    state_.result, derived->selectedDimensionImputation);
                for (auto const& row : dimensionality.loadings)
                    for (std::size_t factor = 0; factor < row.values.size(); ++factor)
                        values.push_back({row.variable + " · F" + std::to_string(factor + 1),
                            FormatDouble(row.values[factor], 4)});
                appendNote(dimensionality.status);
            } else if(kind==ScaleAnalysisChildKind::Biplot) {
                appendBiplot(BuildScaleDimensionalityBiplotPlotState(
                    state_.result, derived->selectedDimensionImputation),
                    derived->focusedDimensionVariable);
                appendNote(BuildScaleDimensionalityPresentation(
                    state_.result, derived->selectedDimensionImputation).status);
            } else if(kind==ScaleAnalysisChildKind::ScaleScoreDistributionPlot||kind==ScaleAnalysisChildKind::ScreePlot){
                std::vector<ScalePlotSeriesResult const*> series;
                ScalePlotSeriesResult observed;
                ScalePlotSeriesResult parallel;
                if (kind == ScaleAnalysisChildKind::ScreePlot) {
                    const auto plot = BuildScaleDimensionalityScreePlotState(
                        state_.result, derived->selectedDimensionImputation);
                    observed.name = "Observed"; observed.kind = "scree_observed";
                    parallel.name = "Parallel reference"; parallel.kind = "scree_reference";
                    for (auto const& point : plot.observed) {
                        observed.labels.push_back(std::to_string(point.component));
                        observed.values.push_back(FormatDouble(point.y, 6));
                    }
                    for (auto const& point : plot.parallel) {
                        parallel.labels.push_back(std::to_string(point.component));
                        parallel.values.push_back(FormatDouble(point.y, 6));
                    }
                    if (!observed.values.empty()) series.push_back(&observed);
                    if (!parallel.values.empty()) series.push_back(&parallel);
                } else {
                    for(auto const& entry:state_.result.plotSeries)
                        if(entry.kind=="score_distribution" &&
                           entry.labels.size()==entry.values.size()) series.push_back(&entry);
                }
                appendLineSeries(kind==ScaleAnalysisChildKind::ScreePlot?L"Scree and parallel analysis":L"Scale-score distributions",series);
                if(kind==ScaleAnalysisChildKind::ScreePlot)appendNote(
                    BuildScaleDimensionalityPresentation(state_.result,
                        derived->selectedDimensionImputation).status);
            } else {if(kind==ScaleAnalysisChildKind::ItemDistributionsPlot)requested="item_distribution";
                for(auto const& series:state_.result.plotSeries){
                    if(series.kind!=requested && !(kind==ScaleAnalysisChildKind::ScreePlot&&series.kind=="scree_reference"))continue;
                    for(std::size_t i=0;i<std::min(series.labels.size(),series.values.size());++i)
                        values.push_back({series.name+" · "+series.labels[i],series.values[i]});}}
            if(!values.empty())appendBars(L"Plot",values);
        }
        derived->status.Text(L"Derived from the exact current Scale Analysis specification.");
    }

    Controls::MenuFlyout ScaleAnalysisView::CreateItemMenu(std::size_t index)
    {
        auto menu = Controls::MenuFlyout();
        if (index >= state_.specification.items.size()) return menu;
        const auto current = state_.specification.items[index];
        auto heading = Controls::MenuFlyoutItem(); heading.Text(to_hstring(current.variable));
        heading.IsEnabled(false); menu.Items().Append(heading);

        auto replace = Controls::MenuFlyoutSubItem(); replace.Text(L"Replace with");
        for (auto const& available : availableItems_)
        {
            if (available.variable == current.variable) continue;
            const bool used = std::any_of(state_.specification.items.begin(),
                state_.specification.items.end(), [&](auto const& item)
                { return item.variable == available.variable; });
            if (used) continue;
            auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(available.variable));
            item.Click([this, index, available](auto const&, auto const&)
            {
                auto candidate = state_.specification; candidate.items[index] = available;
                Submit(std::move(candidate));
            }); replace.Items().Append(item);
        }
        if (replace.Items().Size() > 0) menu.Items().Append(replace);

        auto type = Controls::MenuFlyoutSubItem(); type.Text(L"Item type");
        for (auto const& value : { ::rlispstat::core::ScaleItemType::Numeric,
                                  ::rlispstat::core::ScaleItemType::Ordinal })
        {
            auto item = Controls::ToggleMenuFlyoutItem();
            item.Text(to_hstring(::rlispstat::core::ScaleItemTypeDisplayName(value)));
            item.IsChecked(current.type == value);
            item.Click([this, index, value](auto const&, auto const&)
            {
                auto candidate = state_.specification; candidate.items[index].type = value;
                if (value == ::rlispstat::core::ScaleItemType::Ordinal)
                    candidate.items[index].hasScoringRange = false;
                Submit(std::move(candidate));
            }); type.Items().Append(item);
        }
        menu.Items().Append(type);

        auto direction = Controls::MenuFlyoutSubItem(); direction.Text(L"Direction");
        auto forward = Controls::ToggleMenuFlyoutItem(); forward.Text(L"Forward");
        forward.IsChecked(!current.reversed);
        forward.Click([this, index](auto const&, auto const&)
        { auto candidate = state_.specification; candidate.items[index].reversed = false; Submit(std::move(candidate)); });
        auto reversed = Controls::ToggleMenuFlyoutItem(); reversed.Text(L"Reverse-scored");
        reversed.IsChecked(current.reversed);
        reversed.Click([this, index, current](auto const&, auto const&)
        {
            if (current.type == ::rlispstat::core::ScaleItemType::Numeric && !current.hasScoringRange)
                PromptRange(index);
            else { auto candidate = state_.specification; candidate.items[index].reversed = true;
                Submit(std::move(candidate)); }
        });
        direction.Items().Append(forward); direction.Items().Append(reversed); menu.Items().Append(direction);

        if (current.type == ::rlispstat::core::ScaleItemType::Numeric)
        {
            auto range = Controls::MenuFlyoutItem(); range.Text(L"Set theoretical scoring range...");
            range.Click([this, index](auto const&, auto const&) { PromptRange(index); });
            menu.Items().Append(range);
        }
        auto remove = Controls::MenuFlyoutItem(); remove.Text(L"Remove item");
        remove.Click([this, index](auto const&, auto const&)
        {
            auto candidate = state_.specification;
            candidate.items.erase(candidate.items.begin() + static_cast<std::ptrdiff_t>(index));
            Submit(std::move(candidate));
        }); menu.Items().Append(remove);
        menu.Items().Append(Controls::MenuFlyoutSeparator());
        menu.Items().Append(CreateAddItemSubmenu());
        AppendRCodeOnlyExportMenu(menu, commandCallback_, state_.id);
        return menu;
    }

    winrt::fire_and_forget ScaleAnalysisView::PromptRange(std::size_t index)
    {
        auto lifetime = shared_from_this();
        if (index >= state_.specification.items.size()) co_return;
        auto panel = Controls::StackPanel(); panel.Spacing(8);
        auto message = Controls::TextBlock();
        message.Text(L"Enter the theoretical minimum and maximum. Observed data limits are not used.");
        message.TextWrapping(TextWrapping::Wrap); panel.Children().Append(message);
        auto minimum = Controls::TextBox(); minimum.Header(box_value(L"Minimum"));
        auto maximum = Controls::TextBox(); maximum.Header(box_value(L"Maximum"));
        const auto current = state_.specification.items[index];
        if (current.hasScoringRange)
        {
            minimum.Text(to_hstring(std::to_string(current.scoringMinimum)));
            maximum.Text(to_hstring(std::to_string(current.scoringMaximum)));
        }
        panel.Children().Append(minimum); panel.Children().Append(maximum);
        auto dialog = Controls::ContentDialog(); dialog.Title(box_value(L"Theoretical scoring range"));
        dialog.Content(panel); dialog.PrimaryButtonText(L"Apply"); dialog.CloseButtonText(L"Cancel");
        dialog.DefaultButton(Controls::ContentDialogButton::Primary);
        dialog.XamlRoot(window_.Content().as<FrameworkElement>().XamlRoot());
        const auto result = co_await dialog.ShowAsync();
        if (result != Controls::ContentDialogResult::Primary) co_return;
        try
        {
            const double low = std::stod(to_string(minimum.Text()));
            const double high = std::stod(to_string(maximum.Text()));
            if (!std::isfinite(low) || !std::isfinite(high) || low >= high) co_return;
            auto candidate = state_.specification;
            candidate.items[index].hasScoringRange = true;
            candidate.items[index].scoringMinimum = low;
            candidate.items[index].scoringMaximum = high;
            candidate.items[index].reversed = true;
            Submit(std::move(candidate));
        }
        catch (...) { co_return; }
    }

    void ScaleAnalysisView::Show(::rlispstat::core::ScaleAnalysisState const& state)
    {
        state_ = state; updating_ = true;
        const bool mi = state_.result.imputationCount > 1 || state_.result.backend == "multiple_imputation";
        const auto windowTitle = ::rlispstat::core::ScaleAnalysisWindowTitle(mi);
        window_.Title(to_hstring(windowTitle)); title_.Text(to_hstring(windowTitle));
        badge_.Text(to_hstring(state_.group + (mi ? " · m = " +
            std::to_string(std::max(1, state_.result.imputationCount)) : "")));
        correlation_.SelectedIndex(state_.specification.correlationBasis == "pearson" ? 1 :
            state_.specification.correlationBasis == "polychoric" ? 2 :
            state_.specification.correlationBasis == "mixed" ? 3 : 0);
        score_.SelectedIndex(state_.specification.scoreMethod == "sum" ? 1 : 0);
        updating_ = false; ConfigureAddMenu(); ConfigureContextMenu(); Render();
        for (auto const& entry : derivedWindows_)
            RenderDerived(static_cast<::rlispstat::core::ScaleAnalysisChildKind>(entry.first));
        const auto layout = ::rlispstat::core::BuildScaleAnalysisWindowLayout(
            state_.specification.items.size(), 12);
        PresentOrResizeAnalysisWindow(window_, presented_,
            std::clamp(layout.preferredWidth, 760.0, 1120.0),
            std::clamp(layout.preferredHeight, 430.0, 760.0));
    }

    void ScaleAnalysisView::Render()
    {
        report_.Children().Clear();
        auto appendTitle = [this](std::wstring const& value)
        {
            auto title = Controls::TextBlock(); title.Text(value); title.FontSize(14);
            title.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
            title.Margin(Thickness{ 0, 9, 0, 2 }); report_.Children().Append(title);
        };
        auto appendNote = [this](std::string const& value)
        {
            if (value.empty()) return;
            auto note = Controls::TextBlock(); note.Text(to_hstring(value)); note.FontSize(11);
            note.Foreground(Brush(92,92,92)); note.TextWrapping(TextWrapping::Wrap);
            report_.Children().Append(note);
        };
        auto appendTable = [this](std::vector<double> const& widths,
                                  std::vector<std::wstring> const& labels,
                                  std::vector<std::vector<std::string>> const& rows,
                                  std::vector<Controls::MenuFlyout> const& menus = {})
        {
            auto makeGrid = [&](std::vector<std::string> const& values, bool header,
                                std::size_t rowIndex)
            {
                auto grid = Controls::Grid();
                for (double width : widths) { auto column = Controls::ColumnDefinition();
                    column.Width(GridLengthHelper::FromPixels(width));
                    grid.ColumnDefinitions().Append(column); }
                for (std::size_t column = 0; column < values.size(); ++column) {
                    auto cell = TableCell(to_hstring(values[column]).c_str(), header,
                        column == 0, false, false, !header && rowIndex % 2 == 1);
                    Controls::Grid::SetColumn(cell, static_cast<int>(column));
                    grid.Children().Append(cell);
                    if (column < labels.size()) {
                        const std::string statistic = to_string(labels[column]);
                        auto value = cell.Child().as<Controls::TextBlock>();
                        const bool structural = column == 0 || statistic == "Item" ||
                            statistic == "Type" || statistic == "Direction" ||
                            statistic == "Range" || statistic == "Method" ||
                            statistic == "Imputation";
                        AttachDirectMenu(cell, structural
                            ? CreateStructuralCellMenu(labels[column], value)
                            : CreateStatisticCellMenu(labels[column], value,
                                AnalysisStatisticContext(statistic, "scale",
                                    state_.specification.scoreMethod, "",
                                    state_.result.imputationCount > 1)));
                    }
                }
                return grid;
            };
            std::vector<std::string> headerValues;
            headerValues.reserve(labels.size());
            for (auto const& label : labels) headerValues.push_back(to_string(label));
            report_.Children().Append(makeGrid(headerValues, true, 0));
            for (std::size_t index = 0; index < rows.size(); ++index) {
                auto grid = makeGrid(rows[index], false, index);
                if (index < menus.size() && menus[index]) AttachDirectMenu(grid, menus[index]);
                report_.Children().Append(grid);
            }
        };
        auto parseNumber = [](std::string const& text, double& value)
        {
            char* end = nullptr;
            value = std::strtod(text.c_str(), &end);
            return end != text.c_str() && end && *end == '\0' && std::isfinite(value);
        };
        auto seriesForKind = [this](std::string const& kind)
        {
            std::vector<::rlispstat::core::ScalePlotSeriesResult const*> matches;
            for (auto const& series : state_.result.plotSeries)
                if (series.kind == kind) matches.push_back(&series);
            return matches;
        };
        auto appendBars = [this, &appendTitle, &parseNumber](
            std::wstring const& title,
            std::vector<std::string> const& labels,
            std::vector<std::string> const& rawValues)
        {
            const std::size_t count = std::min(labels.size(), rawValues.size());
            if (count == 0) return;
            std::vector<double> values(count, 0.0);
            double largest = 0.0;
            for (std::size_t index = 0; index < count; ++index) {
                if (!parseNumber(rawValues[index], values[index])) values[index] = 0.0;
                largest = std::max(largest, std::abs(values[index]));
            }
            if (!(largest > 0.0)) largest = 1.0;
            appendTitle(title);
            for (std::size_t index = 0; index < count; ++index) {
                auto row = Controls::Grid(); row.Height(24.0); row.ColumnSpacing(8.0);
                auto labelColumn = Controls::ColumnDefinition();
                labelColumn.Width(GridLengthHelper::FromPixels(180));
                auto barColumn = Controls::ColumnDefinition();
                barColumn.Width(GridLengthHelper::FromPixels(390));
                auto valueColumn = Controls::ColumnDefinition();
                valueColumn.Width(GridLengthHelper::FromPixels(100));
                row.ColumnDefinitions().Append(labelColumn);
                row.ColumnDefinitions().Append(barColumn);
                row.ColumnDefinitions().Append(valueColumn);
                auto label = Controls::TextBlock(); label.Text(to_hstring(labels[index]));
                label.FontSize(11); label.TextTrimming(TextTrimming::CharacterEllipsis);
                label.VerticalAlignment(VerticalAlignment::Center); row.Children().Append(label);
                auto track = Controls::Grid(); track.Height(12.0);
                track.Background(Brush(242,244,247)); track.HorizontalAlignment(HorizontalAlignment::Left);
                track.Width(390.0); track.VerticalAlignment(VerticalAlignment::Center);
                auto bar = Shapes::Rectangle(); bar.Height(12.0);
                bar.Width(std::max(1.0, 390.0 * std::abs(values[index]) / largest));
                bar.Fill(values[index] < 0.0 ? Brush(225,87,89) : Brush(52,122,184));
                bar.HorizontalAlignment(values[index] < 0.0
                    ? HorizontalAlignment::Right : HorizontalAlignment::Left);
                track.Children().Append(bar); Controls::Grid::SetColumn(track, 1);
                row.Children().Append(track);
                auto value = Controls::TextBlock(); value.Text(to_hstring(rawValues[index]));
                value.FontSize(11); value.VerticalAlignment(VerticalAlignment::Center);
                value.TextAlignment(TextAlignment::Right); Controls::Grid::SetColumn(value, 2);
                row.Children().Append(value); report_.Children().Append(row);
            }
        };
        auto appendLineSeries = [this, &appendTitle, &parseNumber](
            std::wstring const& title,
            std::vector<::rlispstat::core::ScalePlotSeriesResult const*> const& series)
        {
            if (series.empty()) return;
            struct NumericSeries { std::string name; std::vector<double> values; };
            std::vector<NumericSeries> parsed;
            double low = std::numeric_limits<double>::infinity();
            double high = -std::numeric_limits<double>::infinity();
            std::size_t maximumCount = 0;
            for (auto const* source : series) {
                NumericSeries value; value.name = source->name;
                for (auto const& raw : source->values) {
                    double number = 0.0;
                    if (parseNumber(raw, number)) {
                        value.values.push_back(number);
                        low = std::min(low, number); high = std::max(high, number);
                    }
                }
                maximumCount = std::max(maximumCount, value.values.size());
                if (!value.values.empty()) parsed.push_back(std::move(value));
            }
            if (parsed.empty() || maximumCount == 0) return;
            if (!(high > low)) { low -= 0.5; high += 0.5; }
            appendTitle(title);
            constexpr double width = 720.0, height = 165.0;
            constexpr double left = 42.0, right = 145.0, top = 12.0, bottom = 28.0;
            const double plotWidth = width - left - right;
            const double plotHeight = height - top - bottom;
            auto canvas = Controls::Canvas(); canvas.Width(width); canvas.Height(height);
            canvas.Background(Brush(255,255,255));
            canvas.Children().Append(ReportLine(left, top, left, top + plotHeight,
                Brush(102,106,112), 1.0));
            canvas.Children().Append(ReportLine(left, top + plotHeight,
                left + plotWidth, top + plotHeight, Brush(102,106,112), 1.0));
            const std::array<Media::SolidColorBrush, 6> colors{
                Brush(32,113,180), Brush(225,87,89), Brush(42,157,143),
                Brush(244,162,97), Brush(111,78,153), Brush(120,120,120) };
            for (std::size_t seriesIndex = 0; seriesIndex < parsed.size(); ++seriesIndex) {
                auto const& current = parsed[seriesIndex];
                const auto stroke = colors[seriesIndex % colors.size()];
                double previousX = 0.0, previousY = 0.0;
                for (std::size_t index = 0; index < current.values.size(); ++index) {
                    const double fraction = maximumCount > 1
                        ? static_cast<double>(index) / static_cast<double>(maximumCount - 1) : 0.5;
                    const double x = left + fraction * plotWidth;
                    const double y = top + (high - current.values[index]) / (high - low) * plotHeight;
                    if (index > 0)
                        canvas.Children().Append(ReportLine(previousX, previousY, x, y, stroke, 1.5));
                    auto point = Shapes::Ellipse(); point.Width(5.0); point.Height(5.0);
                    point.Fill(stroke); Controls::Canvas::SetLeft(point, x - 2.5);
                    Controls::Canvas::SetTop(point, y - 2.5); canvas.Children().Append(point);
                    previousX = x; previousY = y;
                }
                if (seriesIndex < 7) {
                    auto key = ReportLine(left + plotWidth + 12.0, top + 13.0 * seriesIndex + 5.0,
                        left + plotWidth + 27.0, top + 13.0 * seriesIndex + 5.0, stroke, 2.0);
                    canvas.Children().Append(key);
                    auto label = ReportLabel(to_hstring(current.name).c_str(),
                        left + plotWidth + 32.0, top + 13.0 * seriesIndex - 2.0,
                        9.5, Brush(72,72,72)); label.Width(right - 38.0);
                    label.TextTrimming(TextTrimming::CharacterEllipsis);
                    canvas.Children().Append(label);
                }
            }
            report_.Children().Append(canvas);
        };

        if (state_.specification.showOverview && !state_.result.scaleSummary.numberOfItems.empty())
        {
            appendTitle(L"Scale summary");
            auto grid = Controls::Grid(); grid.ColumnSpacing(16); grid.RowSpacing(10);
            for (int i = 0; i < 3; ++i) {
                grid.ColumnDefinitions().Append(Controls::ColumnDefinition());
                grid.RowDefinitions().Append(Controls::RowDefinition());
            }
            const auto fields = ::rlispstat::core::ScaleAnalysisSummaryFields(state_.result);
            for (std::size_t i = 0; i < fields.size(); ++i) {
                auto cell = Controls::StackPanel(); cell.Spacing(5);
                auto label = Controls::TextBlock(); label.Text(to_hstring(fields[i].first));
                label.FontWeight(Windows::UI::Text::FontWeights::SemiBold()); cell.Children().Append(label);
                auto value = Controls::TextBlock(); value.Text(to_hstring(fields[i].second.empty() ? "—" : fields[i].second));
                cell.Children().Append(value);
                auto menu = CreateStatisticCellMenu(to_hstring(fields[i].first).c_str(), value,
                    AnalysisStatisticContext(fields[i].first, "scale", state_.specification.scoreMethod, "", state_.result.imputationCount > 1));
                AttachContextOnlyMenu(cell, menu);
                Controls::Grid::SetRow(cell, (int)i / 3); Controls::Grid::SetColumn(cell, (int)i % 3);
                grid.Children().Append(cell);
            }
            report_.Children().Append(grid);
        }

        appendTitle(state_.specification.showItemAnalysis ? L"Item analysis" : L"Scale items");
        const std::vector<double> itemWidths = state_.specification.showItemAnalysis
            ? std::vector<double>{ 170, 85, 95, 105, 75, 75, 85, 100, 115 }
            : std::vector<double>{ 250, 110, 120, 130 };
        const std::vector<std::wstring> itemLabels = state_.specification.showItemAnalysis
            ? std::vector<std::wstring>{ L"Item", L"Type", L"Direction", L"Range", L"Mean",
                L"SD", L"Missing %", L"Item-rest r", L"Alpha if deleted" }
            : std::vector<std::wstring>{ L"Item", L"Type", L"Direction", L"Range" };
        std::vector<std::vector<std::string>> itemRows;
        std::vector<Controls::MenuFlyout> itemMenus;

        for (std::size_t index = 0; index < state_.specification.items.size(); ++index)
        {
            const auto& specification = state_.specification.items[index];
            auto found = std::find_if(state_.result.items.begin(), state_.result.items.end(),
                [&](auto const& row) { return row.variable == specification.variable; });
            const std::string range = specification.hasScoringRange
                ? std::to_string(specification.scoringMinimum) + "–" +
                    std::to_string(specification.scoringMaximum) : "—";
            std::vector<std::string> values{
                specification.variable,
                ::rlispstat::core::ScaleItemTypeDisplayName(specification.type),
                specification.reversed ? "Reversed" : "Forward", range
            };
            if (state_.specification.showItemAnalysis) {
                values.push_back(found == state_.result.items.end() ? "—" : found->mean);
                values.push_back(found == state_.result.items.end() ? "—" : found->sd);
                values.push_back(found == state_.result.items.end() ? "—" : found->missingPercent);
                values.push_back(found == state_.result.items.end() ? "—" : found->itemRestCorrelation);
                values.push_back(found == state_.result.items.end() ? "—" : found->alphaIfDeleted);
            }
            itemRows.push_back(std::move(values)); itemMenus.push_back(CreateItemMenu(index));
        }
        appendTable(itemWidths, itemLabels, itemRows, itemMenus);
        if (auto parent = addItem_.Parent().try_as<Controls::Panel>()) {
            uint32_t index = 0;
            if (parent.Children().IndexOf(addItem_, index)) parent.Children().RemoveAt(index);
        }
        addItem_.Content(box_value(L"+ Add item"));
        addItem_.Background(TransparentBrush()); addItem_.BorderBrush(TransparentBrush());
        addItem_.HorizontalAlignment(HorizontalAlignment::Left);
        report_.Children().Append(addItem_);
        if (state_.specification.items.empty())
        {
            auto empty = Controls::TextBlock();
            empty.Text(L"Add numeric or ordinal items. A new scale is deliberately empty.");
            empty.Foreground(Brush(92,92,92)); empty.Margin(Thickness{ 3,10,3,10 });
            report_.Children().Append(empty);
        }

        // Reliability, correlations, dimensionality, scores and plots are
        // revision-bound derived analyses displayed in independent windows.
        // The primary scale window intentionally remains compact.
        status_.Text(to_hstring(state_.status));
        return;

        if (state_.specification.showReliability && !state_.result.scaleSummary.alpha.empty())
        {
            appendTitle(L"Reliability");
            appendTable({ 190, 150, 160, 130 },
                { L"Estimate", L"Alpha", L"Standardized alpha", L"Omega total" },
                {{ "Scale", state_.result.scaleSummary.alpha,
                   state_.result.scaleSummary.standardizedAlpha,
                   state_.result.scaleSummary.omegaTotal.empty() ? "—" : state_.result.scaleSummary.omegaTotal }});
            if (!state_.result.reliabilityByImputation.empty()) {
                std::vector<std::vector<std::string>> rows;
                for (auto const& value : state_.result.reliabilityByImputation)
                    rows.push_back({ value.imputation, value.alpha,
                        value.standardizedAlpha, value.omegaTotal.empty() ? "—" : value.omegaTotal });
                appendTable({ 190, 150, 160, 130 },
                    { L"Imputation", L"Alpha", L"Standardized alpha", L"Omega total" }, rows);
            }
            appendNote(state_.result.scaleSummary.reliabilityStatus);
        }

        if (state_.specification.showCorrelations)
        {
            appendTitle(L"Inter-item correlations");
            appendNote("Correlation basis: " + state_.result.correlations.basis +
                " · " + state_.result.correlations.status);
            const auto& correlations = state_.result.correlations;
            const std::size_t count = correlations.variables.size();
            if (count && correlations.values.size() == count * count) {
                std::vector<double> widths(count + 1, 92.0); widths[0] = 150.0;
                std::vector<std::wstring> labels{ L"Item" };
                for (auto const& variable : correlations.variables)
                    labels.push_back(to_hstring(variable).c_str());
                std::vector<std::vector<std::string>> rows;
                for (std::size_t row = 0; row < count; ++row) {
                    std::vector<std::string> values{ correlations.variables[row] };
                    for (std::size_t column = 0; column < count; ++column)
                        values.push_back(correlations.values[row * count + column]);
                    rows.push_back(std::move(values));
                }
                appendTable(widths, labels, rows);
            }
        }

        if (state_.specification.showDimensionality)
        {
            appendTitle(L"Dimensionality / exploratory factor analysis");
            appendNote("Status: " + state_.result.dimensionality.status +
                (state_.result.dimensionality.suggestedFactors.empty() ? "" :
                    " · Parallel analysis: " + state_.result.dimensionality.suggestedFactors));
            if (!state_.result.dimensionality.loadings.empty()) {
                std::vector<double> widths{ 190.0 };
                std::vector<std::wstring> labels{ L"Item" };
                for (auto const& factor : state_.result.dimensionality.factorNames) {
                    widths.push_back(95.0); labels.push_back(to_hstring(factor).c_str());
                }
                widths.push_back(110.0); widths.push_back(110.0);
                labels.push_back(L"Communality"); labels.push_back(L"Uniqueness");
                std::vector<std::vector<std::string>> rows;
                for (auto const& loading : state_.result.dimensionality.loadings) {
                    std::vector<std::string> values{ loading.item };
                    values.insert(values.end(), loading.loadings.begin(), loading.loadings.end());
                    values.push_back(loading.communality); values.push_back(loading.uniqueness);
                    rows.push_back(std::move(values));
                }
                appendTable(widths, labels, rows);
            }
        }

        if (state_.specification.showScoreDistribution)
        {
            appendTitle(L"Scale-score distribution");
            auto const& score = state_.result.scores;
            appendTable({ 140, 120, 120, 120, 120, 120 },
                { L"Method", state_.result.imputationCount > 1
                    ? L"N per imputation" : L"Valid N",
                  L"Mean", L"SD", L"Minimum", L"Maximum" },
                {{ score.method, score.validN, score.mean, score.sd, score.minimum, score.maximum }});
            appendNote(score.status);
            appendLineSeries(L"Score distributions by imputation",
                seriesForKind("score_distribution"));
        }
        if (state_.specification.showItemPlots)
        {
            std::vector<std::string> labels;
            std::vector<std::string> itemRest;
            std::vector<std::string> alphaDeleted;
            for (auto const& item : state_.result.items) {
                labels.push_back(item.variable);
                itemRest.push_back(item.itemRestCorrelation);
                alphaDeleted.push_back(item.alphaIfDeleted);
            }
            appendBars(L"Item-rest correlations", labels, itemRest);
            appendBars(L"Alpha if item deleted", labels, alphaDeleted);
        }
        if (state_.specification.showItemDistributions)
        {
            for (auto const* series : seriesForKind("item_distribution"))
                appendBars(std::wstring(L"Item distribution — ") +
                        to_hstring(series->name).c_str(),
                    series->labels, series->values);
        }
        if (state_.specification.showCorrelationHeatmap)
        {
            auto const& correlations = state_.result.correlations;
            const std::size_t count = correlations.variables.size();
            if (count > 0 && correlations.values.size() == count * count) {
                appendTitle(L"Correlation heatmap");
                const double cell = std::clamp(440.0 / static_cast<double>(count), 24.0, 54.0);
                const double label = 145.0;
                auto canvas = Controls::Canvas();
                canvas.Width(label + cell * count + 8.0);
                canvas.Height(label + cell * count + 8.0);
                canvas.Background(Brush(255,255,255));
                for (std::size_t index = 0; index < count; ++index) {
                    auto topLabel = ReportLabel(to_hstring(correlations.variables[index]).c_str(),
                        label + index * cell, label - 20.0, 9.0, Brush(72,72,72));
                    topLabel.Width(cell); topLabel.TextAlignment(TextAlignment::Center);
                    topLabel.TextTrimming(TextTrimming::CharacterEllipsis);
                    canvas.Children().Append(topLabel);
                    auto rowLabel = ReportLabel(to_hstring(correlations.variables[index]).c_str(),
                        0.0, label + index * cell + 5.0, 9.0, Brush(72,72,72));
                    rowLabel.Width(label - 7.0); rowLabel.TextAlignment(TextAlignment::Right);
                    rowLabel.TextTrimming(TextTrimming::CharacterEllipsis);
                    canvas.Children().Append(rowLabel);
                }
                for (std::size_t row = 0; row < count; ++row) {
                    for (std::size_t column = 0; column < count; ++column) {
                        double value = 0.0;
                        if (!parseNumber(correlations.values[row * count + column], value)) continue;
                        const double strength = std::clamp(std::abs(value), 0.0, 1.0);
                        const uint8_t pale = static_cast<uint8_t>(245.0 - 125.0 * strength);
                        auto cellShape = Shapes::Rectangle();
                        cellShape.Width(cell - 1.0); cellShape.Height(cell - 1.0);
                        cellShape.Fill(value < 0.0 ? Brush(225, pale, pale) : Brush(pale, pale, 225));
                        Controls::Canvas::SetLeft(cellShape, label + column * cell);
                        Controls::Canvas::SetTop(cellShape, label + row * cell);
                        canvas.Children().Append(cellShape);
                    }
                }
                report_.Children().Append(canvas);
            }
        }
        if (state_.specification.showReliabilityStability)
        {
            std::vector<std::string> labels;
            std::vector<std::string> values;
            for (auto const& row : state_.result.reliabilityByImputation) {
                labels.push_back("Imputation " + row.imputation);
                values.push_back(row.alpha);
            }
            appendBars(L"Reliability stability (alpha)", labels, values);
        }
        if (state_.specification.showLoadingPlot &&
            !state_.result.dimensionality.loadings.empty())
        {
            std::vector<std::string> labels;
            std::vector<std::string> values;
            for (auto const& row : state_.result.dimensionality.loadings) {
                labels.push_back(row.item);
                values.push_back(row.loadings.empty() ? "—" : row.loadings.front());
            }
            appendBars(L"First-factor loadings", labels, values);
        }
        if (state_.specification.showScreePlot)
        {
            auto scree = seriesForKind("scree_observed");
            auto reference = seriesForKind("scree_reference");
            scree.insert(scree.end(), reference.begin(), reference.end());
            appendLineSeries(L"Scree and parallel analysis", scree);
            for (auto const* series : seriesForKind("factor_stability"))
                appendBars(L"Suggested-factor stability", series->labels, series->values);
        }
        if (!state_.result.provenance.empty())
        {
            appendTitle(L"Statistical provenance");
            auto provenance = Controls::TextBlock(); provenance.Text(to_hstring(state_.result.provenance));
            provenance.FontSize(11); provenance.Foreground(Brush(92,92,92));
            provenance.TextWrapping(TextWrapping::Wrap); report_.Children().Append(provenance);
        }
        status_.Text(to_hstring(state_.status));
    }

    void ScaleAnalysisView::Activate() { if (!closed_) window_.Activate(); }
    void ScaleAnalysisView::Close()
    {
        if (closed_) return;
        auto children = std::move(derivedWindows_); derivedWindows_.clear();
        for (auto const& entry : children) if (entry.second->window) entry.second->window.Close();
        window_.Close();
    }

    std::shared_ptr<DendrogramView> DendrogramView::Create()
    {
        auto view = std::shared_ptr<DendrogramView>(new DendrogramView()); view->AttachLifetime(); return view;
    }
    DendrogramView::DendrogramView() { Initialize(); }
    void DendrogramView::Initialize()
    {
        using namespace ::rlispstat::core;
        window_ = CreateLinkEDAWindow(); window_.Title(to_hstring(DendrogramWindowTitle()));
        root_ = Controls::Grid(); root_.Padding(Thickness{16,14,16,10}); root_.RowSpacing(7);
        root_.Background(Brush(255,255,255)); root_.RequestedTheme(ElementTheme::Light);
        auto plotColumn=Controls::ColumnDefinition();plotColumn.Width(GridLengthHelper::FromValueAndType(1,GridUnitType::Star));root_.ColumnDefinitions().Append(plotColumn);auto notesColumn=Controls::ColumnDefinition();notesColumn.Width(GridLengthHelper::FromValueAndType(1,GridUnitType::Auto));root_.ColumnDefinitions().Append(notesColumn);
        auto autoRow=[](){auto row=Controls::RowDefinition();row.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Auto));return row;};
        root_.RowDefinitions().Append(autoRow()); root_.RowDefinitions().Append(autoRow());
        auto body=Controls::RowDefinition();body.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Star));root_.RowDefinitions().Append(body);
        root_.RowDefinitions().Append(autoRow());
        auto heading=Controls::Grid();auto grow=Controls::ColumnDefinition();grow.Width(GridLengthHelper::FromValueAndType(1,GridUnitType::Star));
        heading.ColumnDefinitions().Append(grow);heading.ColumnDefinitions().Append(Controls::ColumnDefinition());
        title_=Controls::TextBlock();title_.Text(to_hstring(DendrogramWindowTitle()));title_.FontSize(17);title_.FontWeight(Windows::UI::Text::FontWeights::SemiBold());heading.Children().Append(title_);
        badge_=Controls::TextBlock();badge_.FontSize(12);badge_.Foreground(Brush(72,72,72));badge_.VerticalAlignment(VerticalAlignment::Center);Controls::Grid::SetColumn(badge_,1);heading.Children().Append(badge_);root_.Children().Append(heading);
        auto controls=Controls::Grid();controls.ColumnSpacing(10);
        for(double width:{180.0,160.0,170.0,130.0}){auto column=Controls::ColumnDefinition();column.Width(GridLengthHelper::FromPixels(width));controls.ColumnDefinitions().Append(column);}
        linkage_=Controls::ComboBox();linkage_.Items().Append(box_value(to_hstring(DendrogramAverageLinkageTitle())));linkage_.Items().Append(box_value(to_hstring(DendrogramCompleteLinkageTitle())));linkage_.Items().Append(box_value(to_hstring(DendrogramSingleLinkageTitle())));linkage_.MinHeight(30);controls.Children().Append(linkage_);
        missing_=Controls::ComboBox();missing_.Items().Append(box_value(L"Pairwise"));missing_.Items().Append(box_value(L"Listwise"));missing_.MinHeight(30);Controls::Grid::SetColumn(missing_,1);controls.Children().Append(missing_);
        distance_=Controls::TextBlock();distance_.FontSize(11.5);distance_.Foreground(Brush(80,84,90));distance_.VerticalAlignment(VerticalAlignment::Center);Controls::Grid::SetColumn(distance_,2);controls.Children().Append(distance_);
        addVariable_=Controls::Button();addVariable_.Content(box_value(to_hstring(DendrogramVariablesButtonTitle(0))));
        addVariable_.FontSize(11);addVariable_.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        addVariable_.Foreground(Brush(122,122,122));addVariable_.Background(nullptr);addVariable_.BorderBrush(nullptr);
        addVariable_.Padding(Thickness{0,0,0,0});addVariable_.HorizontalContentAlignment(HorizontalAlignment::Left);
        Controls::Grid::SetColumn(addVariable_,3);controls.Children().Append(addVariable_);
        Controls::Grid::SetRow(controls,1);root_.Children().Append(controls);
        scroll_=Controls::ScrollViewer();scroll_.HorizontalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);scroll_.VerticalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);
        canvas_=Controls::Canvas();canvas_.Name(L"LinkEDA.SnapshotTable");canvas_.Background(Brush(255,255,255));scroll_.Content(canvas_);Controls::Grid::SetRow(scroll_,2);root_.Children().Append(scroll_);
        scroll_.SizeChanged([this](auto const&,SizeChangedEventArgs const& args){FitCanvasToViewport(static_cast<double>(args.NewSize().Width),static_cast<double>(args.NewSize().Height));});
        exportWebView_=Controls::WebView2();exportWebView_.Opacity(0.001);exportWebView_.Visibility(Visibility::Collapsed);exportWebView_.IsHitTestVisible(false);Controls::Grid::SetRow(exportWebView_,2);root_.Children().InsertAt(0,exportWebView_);
        status_=Controls::TextBlock();status_.FontSize(11.5);status_.Foreground(Brush(92,92,92));Controls::Grid::SetRow(status_,3);root_.Children().Append(status_);
        auto notesRoot=Controls::Grid();notesRoot.Width(270);notesRoot.Padding(Thickness{12,12,12,10});
        auto notesTitleRow=autoRow();auto notesBodyRow=Controls::RowDefinition();notesBodyRow.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Star));auto notesFooterRow=autoRow();notesRoot.RowDefinitions().Append(notesTitleRow);notesRoot.RowDefinitions().Append(notesBodyRow);notesRoot.RowDefinitions().Append(notesFooterRow);
        auto notesTitle=Controls::TextBlock();notesTitle.Text(L"Notes");notesTitle.FontSize(15);notesTitle.FontWeight(Windows::UI::Text::FontWeights::SemiBold());notesTitle.Margin(Thickness{0,0,0,8});notesRoot.Children().Append(notesTitle);
        notesText_=Controls::TextBox();notesText_.AcceptsReturn(true);notesText_.TextWrapping(TextWrapping::Wrap);notesText_.VerticalContentAlignment(VerticalAlignment::Top);Controls::Grid::SetRow(notesText_,1);notesRoot.Children().Append(notesText_);
        includeNotes_=Controls::CheckBox();includeNotes_.Content(box_value(L"Include notes in export"));includeNotes_.Margin(Thickness{0,8,0,0});Controls::Grid::SetRow(includeNotes_,2);notesRoot.Children().Append(includeNotes_);
        notesPanel_=Controls::Border();notesPanel_.BorderBrush(Brush(222,224,228));notesPanel_.BorderThickness(Thickness{1,0,0,0});notesPanel_.Background(Brush(249,249,249));notesPanel_.Child(notesRoot);notesPanel_.Visibility(Visibility::Collapsed);Controls::Grid::SetColumn(notesPanel_,1);Controls::Grid::SetRowSpan(notesPanel_,4);root_.Children().Append(notesPanel_);
        notesText_.TextChanged([this](auto const&,auto const&){if(updatingNotes_)return;::rlispstat::core::SetWindowNoteText(note_,to_string(notesText_.Text()));ConfigureContextMenu();});
        includeNotes_.Click([this](auto const&,auto const&){const auto checked=includeNotes_.IsChecked();::rlispstat::core::SetWindowNoteIncludedInExport(note_,checked&&checked.Value());});
        linkage_.SelectionChanged([this](auto const&,auto const&){if(!updating_&&changedCallback_)changedCallback_(linkage_.SelectedIndex()==1?"complete":linkage_.SelectedIndex()==2?"single":"average",missing_.SelectedIndex()==1?"listwise":"pairwise");});
        missing_.SelectionChanged([this](auto const&,auto const&){if(!updating_&&changedCallback_)changedCallback_(linkage_.SelectedIndex()==1?"complete":linkage_.SelectedIndex()==2?"single":"average",missing_.SelectedIndex()==1?"listwise":"pairwise");});
        canvas_.PointerPressed([this](auto const&,Input::PointerRoutedEventArgs const& event)
        {
            auto point=event.GetCurrentPoint(canvas_);
            if(!point.Properties().IsLeftButtonPressed())return;
            auto p=point.Position();dragStart_={p.X,p.Y};dragCurrent_=dragStart_;dragging_=true;
            canvas_.CapturePointer(event.Pointer());event.Handled(true);Render();
        });
        canvas_.PointerMoved([this](auto const&,Input::PointerRoutedEventArgs const& event)
        {
            if(!dragging_)return;auto p=event.GetCurrentPoint(canvas_).Position();dragCurrent_={p.X,p.Y};event.Handled(true);Render();
        });
        canvas_.PointerReleased([this](auto const&,Input::PointerRoutedEventArgs const& event){CompletePointerGesture(event);});
        canvas_.PointerCanceled([this](auto const&,Input::PointerRoutedEventArgs const& event){CompletePointerGesture(event);});
        window_.Content(root_);ResizeLogical(window_,780,520);
    }
    void DendrogramView::AttachLifetime(){std::weak_ptr<DendrogramView> weak=shared_from_this();window_.Closed([weak](auto const&,auto const&){if(auto view=weak.lock()){view->closed_=true;if(view->closedCallback_)view->closedCallback_();}});}
    void DendrogramView::SetClosedCallback(Closed callback){closedCallback_=std::move(callback);}
    void DendrogramView::SetChangedCallback(Changed callback){changedCallback_=std::move(callback);}
    void DendrogramView::SetSelectRowCallback(SelectRow callback){selectRowCallback_=std::move(callback);}
    void DendrogramView::SetSelectRowsCallback(SelectRows callback){selectRowsCallback_=std::move(callback);ConfigureContextMenu();}
    void DendrogramView::SetCommandCallback(Command callback){commandCallback_=std::move(callback);ConfigureContextMenu();}
    void DendrogramView::FitCanvasToViewport(double viewportWidth,double viewportHeight)
    {
        if(state_.id.empty())return;
        if(viewportWidth<=0.0)viewportWidth=scroll_.ActualWidth();
        if(viewportHeight<=0.0)viewportHeight=scroll_.ActualHeight();
        if(viewportWidth<=0.0||viewportHeight<=0.0)return;
        auto size=::rlispstat::core::DendrogramViewportContentSize(
            state_.caseRows.size(),viewportWidth,viewportHeight,fitTreeToWindow_,state_.rotate270);
        size.height=std::max(size.height,260.0+16.0*::rlispstat::core::DendrogramVariableCaptionLines(state_.variables,size.width).size());
        scroll_.VerticalScrollBarVisibility(size.height>viewportHeight+.5?Controls::ScrollBarVisibility::Auto:Controls::ScrollBarVisibility::Disabled);
        if(std::abs(canvas_.Width()-size.width)>.5||std::abs(canvas_.Height()-size.height)>.5){
            canvas_.Width(size.width);canvas_.Height(size.height);Render();
        }
    }
    void DendrogramView::CompletePointerGesture(Input::PointerRoutedEventArgs const& event)
    {
        if(!dragging_)return;
        auto p=event.GetCurrentPoint(canvas_).Position();dragCurrent_={p.X,p.Y};dragging_=false;
        canvas_.ReleasePointerCapture(event.Pointer());event.Handled(true);
        const auto brush=::rlispstat::core::RectBetweenPoints(dragStart_,dragCurrent_);
        const auto hit=::rlispstat::core::DendrogramSelectionRowsForGesture(geometry_,brush,dragCurrent_);
        if(hit.usedBrush){if(selectRowsCallback_)selectRowsCallback_(hit.rows,"replace");}
        else
        {
            const auto joinRows=::rlispstat::core::DendrogramJoinRowsAtPoint(geometry_,dragCurrent_,9.0);
            if(!joinRows.empty()){if(selectRowsCallback_)selectRowsCallback_(joinRows,"replace");}
            else if(!hit.rows.empty()){if(selectRowCallback_)selectRowCallback_(hit.rows.front());}
            else if(selectRowsCallback_)selectRowsCallback_({},"replace");
        }
        Render();
    }
    void DendrogramView::ConfigureContextMenu()
    {
        auto menu=Controls::MenuFlyout();
        if(state_.hasSeed&&commandCallback_){const auto numeric=::rlispstat::core::NumericVariableNames(&state_.seed);auto variables=Controls::MenuFlyoutSubItem();variables.Text(L"Variables");auto add=Controls::MenuFlyoutSubItem();add.Text(L"Add variable");for(auto const& name:numeric)if(std::find(state_.variables.begin(),state_.variables.end(),name)==state_.variables.end()){auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(name));item.Click([this,name](auto const&,auto const&){auto updated=state_.variables;updated.push_back(name);std::vector<std::string> command{"DENDRO_SET_VARIABLES",state_.id,std::to_string(updated.size())};command.insert(command.end(),updated.begin(),updated.end());commandCallback_(command);});add.Items().Append(item);}if(add.Items().Size()>0)variables.Items().Append(add);if(state_.variables.size()>1){auto remove=Controls::MenuFlyoutSubItem();remove.Text(L"Remove variable");for(auto const& name:state_.variables){auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(name));item.Click([this,name](auto const&,auto const&){auto updated=state_.variables;updated.erase(std::remove(updated.begin(),updated.end(),name),updated.end());std::vector<std::string> command{"DENDRO_SET_VARIABLES",state_.id,std::to_string(updated.size())};command.insert(command.end(),updated.begin(),updated.end());commandCallback_(command);});remove.Items().Append(item);}variables.Items().Append(remove);}menu.Items().Append(variables);}

        auto display=Controls::MenuFlyoutSubItem();display.Text(L"Display");
        auto distance=Controls::MenuFlyoutSubItem();distance.Text(L"Distance");
        for(auto const& entry:std::array<std::pair<std::string,std::wstring>,4>{{{"euclidean",L"Euclidean"},{"manhattan",L"Manhattan"},{"maximum",L"Maximum"},{"canberra",L"Canberra"}}}){auto item=Controls::ToggleMenuFlyoutItem();item.Text(entry.second);item.IsChecked(state_.distance==entry.first);item.Click([this,value=entry.first](auto const&,auto const&){if(commandCallback_)commandCallback_({"DENDRO_SET_DISTANCE",state_.id,value});});distance.Items().Append(item);}display.Items().Append(distance);
        auto fitTree=Controls::ToggleMenuFlyoutItem();fitTree.Text(L"Fit tree to window");
        fitTree.IsChecked(fitTreeToWindow_);
        fitTree.Click([this](auto const&,auto const&){fitTreeToWindow_=!fitTreeToWindow_;FitCanvasToViewport();ConfigureContextMenu();});
        display.Items().Append(fitTree);
        auto linkage=Controls::MenuFlyoutSubItem();linkage.Text(L"Linkage method");
        for(auto const& entry:std::array<std::pair<std::string,std::wstring>,3>{{{"average",L"Average"},{"complete",L"Complete"},{"single",L"Single"}}}){auto item=Controls::ToggleMenuFlyoutItem();item.Text(entry.second);item.IsChecked(state_.linkage==entry.first);item.Click([this,value=entry.first](auto const&,auto const&){if(changedCallback_)changedCallback_(value,state_.missingMode);});linkage.Items().Append(item);}display.Items().Append(linkage);
        auto missing=Controls::MenuFlyoutSubItem();missing.Text(L"Missing values");
        for(auto const& entry:std::array<std::pair<std::string,std::wstring>,2>{{{"pairwise",L"Pairwise"},{"listwise",L"Listwise"}}}){auto item=Controls::ToggleMenuFlyoutItem();item.Text(entry.second);item.IsChecked(state_.missingMode==entry.first);item.Click([this,value=entry.first](auto const&,auto const&){if(changedCallback_)changedCallback_(state_.linkage,value);});missing.Items().Append(item);}display.Items().Append(missing);

        auto labels=Controls::MenuFlyoutSubItem();labels.Text(L"Leaf labels");
        auto noLabels=Controls::ToggleMenuFlyoutItem();noLabels.Text(L"Row numbers");noLabels.IsChecked(state_.labelVariable.empty());noLabels.Click([this](auto const&,auto const&){if(commandCallback_)commandCallback_({"DENDRO_SET_LABEL",state_.id,"."});});labels.Items().Append(noLabels);
        for(auto const& variable:availableVariables_){auto item=Controls::ToggleMenuFlyoutItem();item.Text(to_hstring(variable));item.IsChecked(state_.labelVariable==variable);item.Click([this,variable](auto const&,auto const&){if(commandCallback_)commandCallback_({"DENDRO_SET_LABEL",state_.id,variable});});labels.Items().Append(item);}display.Items().Append(labels);
        auto rotate=Controls::ToggleMenuFlyoutItem();rotate.Text(L"Rotate dendrogram 270°");rotate.IsChecked(state_.rotate270);rotate.Click([this](auto const&,auto const&){if(commandCallback_)commandCallback_({"DENDRO_ROTATE_270",state_.id});});display.Items().Append(rotate);auto rotateLabels=Controls::ToggleMenuFlyoutItem();rotateLabels.Text(L"Rotate case labels 90°");rotateLabels.IsChecked(state_.rotateCaseLabels90);rotateLabels.Click([this](auto const&,auto const&){if(commandCallback_)commandCallback_({"DENDRO_ROTATE_LABELS_90",state_.id});});display.Items().Append(rotateLabels);
        auto legendMenu=Controls::MenuFlyoutSubItem();legendMenu.Text(L"Color legend");
        auto legend=Controls::ToggleMenuFlyoutItem();legend.Text(state_.colorByLegendVisible?L"Hide":L"Show");legend.IsChecked(state_.colorByLegendVisible);legend.IsEnabled(!state_.colorByVariable.empty());legend.Click([this](auto const&,auto const&){if(commandCallback_)commandCallback_({"DENDRO_TOGGLE_COLOR_LEGEND",state_.id});});legendMenu.Items().Append(legend);legendMenu.Items().Append(Controls::MenuFlyoutSeparator());
        for(auto const& position:std::vector<std::pair<std::string,std::wstring>>{{"top_left",L"Top left"},{"top_right",L"Top right"},{"bottom_left",L"Bottom left"},{"bottom_right",L"Bottom right"}}){auto item=Controls::MenuFlyoutItem();item.Text(position.second);item.Click([this,value=position.first](auto const&,auto const&){if(commandCallback_)commandCallback_({"DENDRO_SET_LEGEND_POSITION",state_.id,value});});legendMenu.Items().Append(item);}display.Items().Append(legendMenu);
        auto themeMenu=Controls::MenuFlyoutSubItem();themeMenu.Text(L"Theme");for(auto const& themeName : ::rlispstat::core::PlotThemeNames()){auto item=Controls::ToggleMenuFlyoutItem();item.Text(to_hstring(::rlispstat::core::PlotThemeDisplayName(themeName)));item.IsChecked(themeName==themeName_);item.Click([this,themeName](auto const&,auto const&){SetVisualTheme(themeName);});themeMenu.Items().Append(item);}display.Items().Append(themeMenu);menu.Items().Append(display);

        auto colorBy=Controls::MenuFlyoutSubItem();colorBy.Text(L"Override colors by");
        auto noColor=Controls::ToggleMenuFlyoutItem();noColor.Text(L"Cancel color override");noColor.IsChecked(state_.colorByVariable.empty());noColor.Click([this](auto const&,auto const&){if(commandCallback_)commandCallback_({"DENDRO_SET_COLOR_BY",state_.id,"."});});colorBy.Items().Append(noColor);
        for(auto const& variable:availableVariables_){auto item=Controls::ToggleMenuFlyoutItem();item.Text(to_hstring(variable));item.IsChecked(state_.colorByVariable==variable);item.Click([this,variable](auto const&,auto const&){if(commandCallback_)commandCallback_({"DENDRO_SET_COLOR_BY",state_.id,variable});});colorBy.Items().Append(item);}

        if(selectRowsCallback_){auto selection=Controls::MenuFlyoutSubItem();selection.Text(L"Selection");auto selectAll=Controls::MenuFlyoutItem();selectAll.Text(L"Select all displayed cases");selectAll.Click([this](auto const&,auto const&){if(selectRowsCallback_)selectRowsCallback_(state_.caseRows,"replace");});selection.Items().Append(selectAll);auto invert=Controls::MenuFlyoutItem();invert.Text(L"Invert displayed selection");invert.Click([this](auto const&,auto const&){std::vector<int> rows;for(int row:state_.caseRows)if(!selectedRows_.count(row))rows.push_back(row);if(selectRowsCallback_)selectRowsCallback_(rows,"replace");});selection.Items().Append(invert);auto clear=Controls::MenuFlyoutItem();clear.Text(L"Clear selection");clear.Click([this](auto const&,auto const&){if(selectRowsCallback_)selectRowsCallback_({},"replace");});selection.Items().Append(clear);auto colorState=Controls::MenuFlyoutSubItem();colorState.Text(L"Colors");auto colors=Controls::MenuFlyoutSubItem();colors.Text(L"Color selected points");for(auto const& color : ::rlispstat::core::PaletteColors()){auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(color.name));item.Click([this,name=color.name](auto const&,auto const&){if(commandCallback_)commandCallback_({"SET_SELECTED_COLOR",state_.group,name});});colors.Items().Append(item);}colorState.Items().Append(colors);auto palette=Controls::MenuFlyoutItem();palette.Text(L"Open color palette…");palette.Click([this](auto const&,auto const&){if(commandCallback_)commandCallback_({"OPEN_DENDRO_COLOR_PALETTE",state_.group});});colorState.Items().Append(palette);auto reset=Controls::MenuFlyoutItem();reset.Text(L"Reset selected point colors");reset.IsEnabled(!selectedRows_.empty());reset.Click([this](auto const&,auto const&){if(!commandCallback_)return;std::vector<std::string> command{"CLEAR_ROW_COLORS",state_.group,std::to_string(selectedRows_.size())};for(int row:selectedRows_)command.push_back(std::to_string(row));commandCallback_(command);});colorState.Items().Append(reset);colorState.Items().Append(Controls::MenuFlyoutSeparator());colorState.Items().Append(colorBy);selection.Items().Append(colorState);menu.Items().Append(selection);}

        if(commandCallback_){auto analyze=Controls::MenuFlyoutSubItem();analyze.Text(L"Analyze");auto matrix=Controls::MenuFlyoutItem();matrix.Text(L"Distance matrix…");matrix.IsEnabled(!state_.rFitPending && state_.caseRows.size()>=2);matrix.Click([this](auto const&,auto const&){if(commandCallback_)commandCallback_({"DENDRO_SHOW_DISTANCE_MATRIX",state_.id});});analyze.Items().Append(matrix);auto scope=Controls::MenuFlyoutSubItem();scope.Text(L"Analysis scope");auto selected=Controls::MenuFlyoutItem();selected.Text(to_hstring("Selected ("+std::to_string(selectedRows_.size())+")"));selected.IsEnabled(!selectedRows_.empty());selected.Click([this](auto const&,auto const&){if(!commandCallback_)return;std::vector<std::string> command{"SET_ANALYSIS_SCOPE_FROM_ROWS",state_.group,std::to_string(selectedRows_.size())};for(int row:selectedRows_)command.push_back(std::to_string(row));commandCallback_(command);});scope.Items().Append(selected);auto displayed=Controls::MenuFlyoutItem();displayed.Text(to_hstring("Displayed ("+std::to_string(state_.caseRows.size())+")"));displayed.Click([this](auto const&,auto const&){if(!commandCallback_)return;std::vector<std::string> command{"SET_ANALYSIS_SCOPE_FROM_ROWS",state_.group,std::to_string(state_.caseRows.size())};for(int row:state_.caseRows)command.push_back(std::to_string(row));commandCallback_(command);});scope.Items().Append(displayed);auto all=Controls::MenuFlyoutItem();all.Text(L"All observations");all.Click([this](auto const&,auto const&){if(commandCallback_)commandCallback_({"SET_ANALYSIS_SCOPE_ALL",state_.group});});scope.Items().Append(all);analyze.Items().Append(scope);menu.Items().Append(analyze);}

        auto annotate=Controls::MenuFlyoutSubItem();annotate.Text(note_.has_content?L"Annotate •":L"Annotate");auto toggleNotes=Controls::ToggleMenuFlyoutItem();toggleNotes.Text(note_.visible?L"Hide Notes":(note_.has_content?L"Show Notes •":L"Show Notes"));toggleNotes.IsChecked(note_.visible);toggleNotes.Click([this](auto const&,auto const&){SetNotesVisible(!note_.visible);});annotate.Items().Append(toggleNotes);auto clearNotes=Controls::MenuFlyoutItem();clearNotes.Text(L"Clear Notes");clearNotes.IsEnabled(note_.has_content);clearNotes.Click([this](auto const&,auto const&){::rlispstat::core::ClearWindowNote(note_);updatingNotes_=true;notesText_.Text(L"");updatingNotes_=false;ConfigureContextMenu();});annotate.Items().Append(clearNotes);annotate.Items().Append(Controls::MenuFlyoutSeparator());AppendSnapshotAlbumContextItems(window_,annotate);menu.Items().Append(annotate);
        auto exportMenu=Controls::MenuFlyoutSubItem();exportMenu.Text(L"Export");
        auto copyMenu=Controls::MenuFlyoutSubItem();copyMenu.Text(L"Copy");
        auto saveMenu=Controls::MenuFlyoutSubItem();saveMenu.Text(L"Save");
        const auto exportActions=::rlispstat::core::BuildExportMenuActions(
            ::rlispstat::core::StandardVisualExportCapabilities(
                ::rlispstat::core::ExportPlatform::Windows),
            ::rlispstat::core::ExportPlatform::Windows);
        for(auto const& action:exportActions){auto destination=action.command.rfind("COPY_",0)==0?copyMenu:saveMenu;if(action.separatorBefore&&destination.Items().Size()>0)destination.Items().Append(Controls::MenuFlyoutSeparator());auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(action.title));item.Click([this,command=action.command](auto const&,auto const&){if(command=="COPY_RICH"||command=="COPY_SVG")CopyPlotAsync(true);else if(command=="COPY_PNG")CopyPlotAsync(false);else if(command=="SAVE_SVG")SavePlotAsync("SVG");else if(command=="SAVE_PDF")SavePlotAsync("PDF");else if(command=="SAVE_PNG")SavePlotAsync("PNG");});destination.Items().Append(item);}if(copyMenu.Items().Size()>0)exportMenu.Items().Append(copyMenu);if(saveMenu.Items().Size()>0)exportMenu.Items().Append(saveMenu);AppendRCodeExportItems(exportMenu,commandCallback_,state_.id,false);menu.Items().Append(exportMenu);
        AttachWindowContextFlyout(window_, menu, false);
    }

    std::string DendrogramView::ExportSvgDocument() const
    {
        const double width=std::max(320.0,canvas_.Width());
        const double height=std::max(240.0,canvas_.Height());
        ::rlispstat::core::SvgWriter writer({width,height,1.0});const auto theme=::rlispstat::core::PlotThemeStyleForName(themeName_);
        auto svgThemeColor=[](::rlispstat::core::PlotRGBA const& c){char buffer[16];std::snprintf(buffer,sizeof(buffer),"#%02X%02X%02X",(int)std::lround(c.r*255.0),(int)std::lround(c.g*255.0),(int)std::lround(c.b*255.0));return std::string(buffer);};
        auto svgColor=[](std::string const& name){auto c=::rlispstat::core::PlotColorForNameOrHex(name);char buffer[16];std::snprintf(buffer,sizeof(buffer),"#%02X%02X%02X",(int)std::lround(c.r*255.0),(int)std::lround(c.g*255.0),(int)std::lround(c.b*255.0));return std::string(buffer);};
        writer.rectangle(0,0,width,height,{svgThemeColor(theme.background),"none",0.0,1.0,{}});
        const auto caption=::rlispstat::core::DendrogramVariableCaptionLines(state_.variables,width);
        ::rlispstat::core::SvgTextStyle captionStyle;captionStyle.fontSize=12;captionStyle.fill=svgThemeColor(theme.mutedText);
        for(std::size_t i=0;i<caption.size();++i)writer.text(20,20+16*i,caption[i],captionStyle);
        ::rlispstat::core::SvgPaint branchPaint{"none",svgThemeColor(theme.axis),1.2,1.0,{}};
        for(auto const& branch:geometry_.branches){const auto color=::rlispstat::core::DendrogramUniformBranchColor(branch,rowColors_,state_.colorByRowColors);const bool selected=::rlispstat::core::DendrogramBranchIsFullySelected(branch,selectedRows_);auto paint=branchPaint;if(!color.empty())paint.stroke=svgColor(color);else if(selected)paint.stroke=svgThemeColor(theme.selectedMarkStroke);paint.strokeWidth=selected?2.8:(!color.empty()?2.2:1.2);writer.line(branch.start.x,branch.start.y,branch.end.x,branch.end.y,paint);}
        for(auto const& leaf:geometry_.leaves){const bool selected=selectedRows_.count(leaf.rowId)>0;std::string color;auto manual=rowColors_.find(leaf.rowId);if(manual!=rowColors_.end())color=manual->second;else{auto grouped=state_.colorByRowColors.find(leaf.rowId);if(grouped!=state_.colorByRowColors.end())color=grouped->second;}::rlispstat::core::SvgPaint pointPaint{color.empty()?(selected?"#1c1e21":"#aeb1b5"):svgColor(color),"none",0.0,1.0,{}};writer.ellipse(leaf.point.x,leaf.point.y,3.4,3.4,pointPaint);if(selected)writer.ellipse(leaf.point.x,leaf.point.y,5.0,5.0,{"none",color.empty()?"#1c1e21":svgColor(color),1.4,1.0,{}});if(leaf.showLabel){::rlispstat::core::SvgTextStyle text;text.fill=selected?svgThemeColor(theme.text):svgThemeColor(theme.mutedText);text.fontSize=10.5;text.anchor=state_.rotate270&&!leaf.labelRotated90?"end":"middle";const double tx=leaf.labelRotated90?leaf.labelRect.x+leaf.labelRect.width*.5:(state_.rotate270?leaf.labelRect.x+leaf.labelRect.width:leaf.point.x);const double ty=leaf.labelRotated90?leaf.labelRect.y+leaf.labelRect.height*.5+3.5:leaf.labelRect.y+10.5;writer.text(tx,ty,leaf.label,text,leaf.labelRotated90?90.0:0.0);}}
        if(state_.colorByLegendVisible&&!state_.colorByVariable.empty()&&::rlispstat::core::DendrogramColorLegendMatchesLinkedRowColors(state_,rowColors_)){std::size_t longest=state_.colorByVariable.size();for(auto const& item:state_.colorByLegendItems)longest=std::max(longest,item.first.size());const double legendWidth=std::max(108.0,std::min(220.0,48.0+7.0*longest));const double legendHeight=28.0+20.0*state_.colorByLegendItems.size();double x=std::clamp(state_.colorByLegendX*width,4.0,std::max(4.0,width-legendWidth-4.0)),y=std::clamp(state_.colorByLegendY*height,4.0,std::max(4.0,height-legendHeight-4.0));writer.rectangle(x,y,legendWidth,legendHeight,{svgThemeColor(theme.panel),svgThemeColor(theme.axis),1.0,1.0,{}});::rlispstat::core::SvgTextStyle title;title.fill=svgThemeColor(theme.text);title.fontSize=11.5;title.fontWeight=600;writer.text(x+7,y+18,state_.colorByVariable,title);y+=28;for(auto const& entry:state_.colorByLegendItems){writer.ellipse(x+11,y+10,4,4,{svgColor(entry.second),"none",0,1,{}});::rlispstat::core::SvgTextStyle item;item.fill=svgThemeColor(theme.text);item.fontSize=10.5;writer.text(x+21,y+14,entry.first,item);y+=20;}}
        if(!geometry_.hasCases){::rlispstat::core::SvgTextStyle text;text.fill="#646464";text.fontSize=13.0;writer.text(24,80,(state_.status.empty() ? ::rlispstat::core::DendrogramEmptyPlotStatus(state_.variables.size()) : state_.status),text);}
        auto svg=writer.finish();if(note_.include_in_export&&note_.has_content)svg=::rlispstat::core::ComposeSvgDocumentWithWindowNote(svg,{"Notes",note_.plain_text});return svg;
    }

    Windows::Foundation::IAsyncOperation<Windows::Storage::Streams::InMemoryRandomAccessStream>
        DendrogramView::RenderSvgToPngAsync(std::string svg,double scale)
    {
        const double width=std::max(320.0,canvas_.Width());
        const double height=std::max(240.0,canvas_.Height());
        exportWebView_.Width(width*scale);exportWebView_.Height(height*scale);
        exportWebView_.Visibility(Visibility::Visible);
        co_await exportWebView_.EnsureCoreWebView2Async();
        const std::string html="<!doctype html><html><head><meta charset=\"utf-8\"><style>html,body{margin:0;background:white;overflow:hidden}svg{width:100vw;height:100vh}</style></head><body>"+svg+"</body></html>";
        winrt::handle completed(CreateEventW(nullptr,TRUE,FALSE,nullptr));
        event_token token=exportWebView_.NavigationCompleted([event=completed.get()](auto const&,auto const&){SetEvent(event);});
        exportWebView_.NavigateToString(to_hstring(html));co_await winrt::resume_on_signal(completed.get());
        co_await wil::resume_foreground(exportWebView_.DispatcherQueue());exportWebView_.NavigationCompleted(token);
        Windows::Storage::Streams::InMemoryRandomAccessStream stream;
        co_await exportWebView_.CoreWebView2().CapturePreviewAsync(
            Microsoft::Web::WebView2::Core::CoreWebView2CapturePreviewImageFormat::Png,stream);
        exportWebView_.Visibility(Visibility::Collapsed);stream.Seek(0);co_return stream;
    }

    Windows::Foundation::IAsyncOperation<bool>
        DendrogramView::RenderSvgToPdfAsync(std::string svg,std::wstring path)
    {
        const double width=std::max(320.0,canvas_.Width());
        const double height=std::max(240.0,canvas_.Height());
        exportWebView_.Width(width);exportWebView_.Height(height);exportWebView_.Visibility(Visibility::Visible);
        co_await exportWebView_.EnsureCoreWebView2Async();
        const std::string html="<!doctype html><html><head><meta charset=\"utf-8\"><style>@page{margin:0;size:"+std::to_string(width*0.75)+"pt "+std::to_string(height*0.75)+"pt}html,body{margin:0;background:white;overflow:hidden}svg{display:block;width:100vw;height:100vh}</style></head><body>"+svg+"</body></html>";
        winrt::handle completed(CreateEventW(nullptr,TRUE,FALSE,nullptr));
        event_token token=exportWebView_.NavigationCompleted([event=completed.get()](auto const&,auto const&){SetEvent(event);});
        exportWebView_.NavigateToString(to_hstring(html));co_await winrt::resume_on_signal(completed.get());
        co_await wil::resume_foreground(exportWebView_.DispatcherQueue());exportWebView_.NavigationCompleted(token);
        auto settings=exportWebView_.CoreWebView2().Environment().CreatePrintSettings();settings.ShouldPrintBackgrounds(true);settings.ShouldPrintHeaderAndFooter(false);settings.MarginTop(0);settings.MarginRight(0);settings.MarginBottom(0);settings.MarginLeft(0);settings.PageWidth(width/96.0);settings.PageHeight(height/96.0);
        const bool ok=co_await exportWebView_.CoreWebView2().PrintToPdfAsync(path,settings);exportWebView_.Visibility(Visibility::Collapsed);co_return ok;
    }

    fire_and_forget DendrogramView::CopyPlotAsync(bool includeSvg)
    {
        auto lifetime=shared_from_this();
        try{const std::string svg=ExportSvgDocument();auto stream=co_await RenderSvgToPngAsync(svg,2.0);Windows::ApplicationModel::DataTransfer::DataPackage package;package.SetBitmap(Windows::Storage::Streams::RandomAccessStreamReference::CreateFromStream(stream));if(includeSvg)package.SetData(L"image/svg+xml",box_value(to_hstring(svg)));Windows::ApplicationModel::DataTransfer::Clipboard::SetContent(package);Windows::ApplicationModel::DataTransfer::Clipboard::Flush();}
        catch(...){exportWebView_.Visibility(Visibility::Collapsed);MessageBoxW(NativeWindowHandle(window_),L"The Quick Cluster plot could not be copied.",L"Copy Plot",MB_OK|MB_ICONERROR);}co_return;
    }

    fire_and_forget DendrogramView::SavePlotAsync(std::string format)
    {
        auto lifetime=shared_from_this();const auto path=ChooseDendrogramExportPath(window_,state_.id,format);if(!path){Activate();co_return;}bool ok=false;
        try{const std::string svg=ExportSvgDocument();if(format=="SVG")ok=WriteDendrogramBytes(*path,std::vector<uint8_t>(svg.begin(),svg.end()));else if(format=="PDF")ok=co_await RenderSvgToPdfAsync(svg,*path);else{auto stream=co_await RenderSvgToPngAsync(svg,2.0);const uint32_t length=static_cast<uint32_t>(stream.Size());Windows::Storage::Streams::DataReader reader(stream.GetInputStreamAt(0));co_await reader.LoadAsync(length);std::vector<uint8_t> bytes(length);reader.ReadBytes(bytes);ok=WriteDendrogramBytes(*path,bytes);}}
        catch(...){exportWebView_.Visibility(Visibility::Collapsed);ok=false;}if(!ok)MessageBoxW(NativeWindowHandle(window_),L"The Quick Cluster plot could not be exported.",L"Export Plot",MB_OK|MB_ICONERROR);Activate();co_return;
    }

    void DendrogramView::Show(::rlispstat::core::DendrogramState const& state,std::set<int> const& selectedRows,std::vector<std::pair<int,std::string>> const& rowColors,std::vector<std::string> const& availableVariables)
    {
        ::rlispstat::windows::performance::Scope timing("Output.Dendrogram.Show");
        SetWindowDataSheetGroup(window_, state.group);
        SetWindowSnapshotSource(window_, "output", state.id, state.group);
        if(state_.id!=state.id)note_=::rlispstat::core::MakeWindowNote(state.id);state_=state;selectedRows_=selectedRows;rowColors_.clear();for(auto const& entry:rowColors)rowColors_[entry.first]=entry.second;availableVariables_=availableVariables;updating_=true;badge_.Text(to_hstring(state.group));
        linkage_.SelectedIndex(state.linkage=="complete"?1:state.linkage=="single"?2:0);missing_.SelectedIndex(state.missingMode=="listwise"?1:0);
        distance_.Text(to_hstring(::rlispstat::core::DendrogramDistanceControlLabel(state.distance)));updating_=false;
        auto variableMenu=Controls::MenuFlyout();auto remove=Controls::MenuFlyoutSubItem();remove.Text(L"Remove variable");
        const auto entries=::rlispstat::core::BuildDendrogramVariableMenuState(
            state.variables,::rlispstat::core::NumericVariableNames(&state.seed));
        for(const auto& entry:entries.items) {
            auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(entry.variable));item.IsEnabled(entry.enabled);
            item.Click([this,variable=entry.variable](auto const&,auto const&) {
                const auto result=::rlispstat::core::DendrogramVariablesAfterToggle(
                    state_.variables,variable,::rlispstat::core::NumericVariableNames(&state_.seed));
                if(!result.ok || !commandCallback_)return;
                std::vector<std::string> command{"DENDRO_SET_VARIABLES",state_.id,std::to_string(result.variables.size())};
                command.insert(command.end(),result.variables.begin(),result.variables.end());commandCallback_(command);
            });
            if(entry.included)remove.Items().Append(item);else variableMenu.Items().Append(item);
        }
        if(remove.Items().Size()>0) {
            if(variableMenu.Items().Size()>0)variableMenu.Items().Append(Controls::MenuFlyoutSeparator());
            variableMenu.Items().Append(remove);
        }
        addVariable_.IsEnabled(variableMenu.Items().Size()>0);addVariable_.Flyout(variableMenu);
        if(!presented_){canvas_.Width(796.0);canvas_.Height(408.0);}
        FitCanvasToViewport();Render();ConfigureContextMenu();
        status_.Text(to_hstring(::rlispstat::core::DendrogramWindowSummaryStatus(state.group,state.caseRows.size(),state.variables.size(),state.linkage,state.missingMode,selectedRows.size()) + (state.status.empty() ? "" : " · " + state.status)));
        PresentWindowOnce(window_,presented_,820.0,560.0);
        std::weak_ptr<DendrogramView> weak=shared_from_this();root_.DispatcherQueue().TryEnqueue([weak](){if(auto view=weak.lock())view->FitCanvasToViewport();});
    }
    void DendrogramView::RefreshSelection(std::set<int> const& selectedRows){selectedRows_=selectedRows;Render();ConfigureContextMenu();}
    void DendrogramView::RefreshRowColors(std::vector<std::pair<int,std::string>> const& rowColors){rowColors_.clear();for(auto const& entry:rowColors)rowColors_[entry.first]=entry.second;Render();ConfigureContextMenu();}
    void DendrogramView::SetVisualTheme(std::string const& themeName){themeName_=themeName;if(!state_.id.empty()){Render();ConfigureContextMenu();}}
    void DendrogramView::SetNotesVisible(bool visible)
    {
        if(note_.visible==visible)return;const double width=root_.ActualWidth()>0?root_.ActualWidth():780.0;const double height=root_.ActualHeight()>0?root_.ActualHeight():520.0;
        ::rlispstat::core::SetWindowNoteVisible(note_,visible);notesPanel_.Visibility(visible?Visibility::Visible:Visibility::Collapsed);ResizeLinkEDAWindowClient(window_,std::max(360.0,width+(visible?270.0:-270.0)),height);
        if(visible){updatingNotes_=true;notesText_.Text(to_hstring(note_.plain_text));updatingNotes_=false;includeNotes_.IsChecked(note_.include_in_export);notesText_.Focus(FocusState::Programmatic);}root_.UpdateLayout();ConfigureContextMenu();
    }
    void DendrogramView::Render()
    {
        const auto theme=::rlispstat::core::PlotThemeStyleForName(themeName_);const double width=canvas_.Width(),height=canvas_.Height();canvas_.Children().Clear();canvas_.Background(PlotBrush(theme.background));legendRect_={};geometry_=::rlispstat::core::BuildDendrogramPlotGeometry(state_.caseRows,state_.merges,state_.leafOrder,width,height,state_.verticalFlip,state_.rowLabels,state_.rotate270,state_.rotateCaseLabels90,16.0*::rlispstat::core::DendrogramVariableCaptionLines(state_.variables,width).size()+12.0);
        const auto caption=::rlispstat::core::DendrogramVariableCaptionLines(state_.variables,width);
        for(std::size_t i=0;i<caption.size();++i)canvas_.Children().Append(ReportLabel(to_hstring(caption[i]).c_str(),20,8+16*i,12,PlotBrush(theme.mutedText)));
        for(auto const& branch:geometry_.branches){const auto color=::rlispstat::core::DendrogramUniformBranchColor(branch,rowColors_,state_.colorByRowColors);const bool selected=::rlispstat::core::DendrogramBranchIsFullySelected(branch,selectedRows_);const auto stroke=!color.empty()?PlotBrush(::rlispstat::core::PlotColorForNameOrHex(color)):(selected?PlotBrush(theme.selectedMarkStroke):PlotBrush(theme.axis));canvas_.Children().Append(ReportLine(branch.start.x,branch.start.y,branch.end.x,branch.end.y,stroke,selected?2.8:(!color.empty()?2.2:1.2)));}
        for(auto const& leaf:geometry_.leaves){const bool selected=selectedRows_.count(leaf.rowId)>0;std::string color;auto manual=rowColors_.find(leaf.rowId);if(manual!=rowColors_.end())color=manual->second;else{auto grouped=state_.colorByRowColors.find(leaf.rowId);if(grouped!=state_.colorByRowColors.end())color=grouped->second;}auto fill=color.empty()?PlotBrush(selected?theme.selectedMarkFill:theme.markFill):PlotBrush(::rlispstat::core::PlotColorForNameOrHex(color));const double r=3.4;auto point=Shapes::Ellipse();point.Width(r*2);point.Height(r*2);point.Fill(fill);Controls::Canvas::SetLeft(point,leaf.point.x-r);Controls::Canvas::SetTop(point,leaf.point.y-r);canvas_.Children().Append(point);if(selected){auto ring=Shapes::Ellipse();const double rr=5.0;ring.Width(rr*2);ring.Height(rr*2);ring.Fill(TransparentBrush());ring.Stroke(color.empty()?PlotBrush(theme.selectedMarkStroke):PlotBrush(::rlispstat::core::PlotSelectedColorForNameOrHex(color)));ring.StrokeThickness(1.4);Controls::Canvas::SetLeft(ring,leaf.point.x-rr);Controls::Canvas::SetTop(ring,leaf.point.y-rr);canvas_.Children().Append(ring);}if(leaf.showLabel){auto label=ReportLabel(to_hstring(leaf.label).c_str(),leaf.labelRect.x,leaf.labelRect.y,10.5,PlotBrush(selected?theme.text:theme.mutedText));label.Width(leaf.labelRect.width);label.Height(leaf.labelRect.height);label.TextAlignment(state_.rotate270&&!leaf.labelRotated90?TextAlignment::Right:TextAlignment::Center);label.TextTrimming(TextTrimming::CharacterEllipsis);if(leaf.labelRotated90){auto rotation=Media::RotateTransform();rotation.Angle(90);label.RenderTransform(rotation);label.RenderTransformOrigin(Windows::Foundation::Point{.5f,.5f});}canvas_.Children().Append(label);}}
        if(state_.colorByLegendVisible&&!state_.colorByVariable.empty()&&!state_.colorByLegendItems.empty()&&::rlispstat::core::DendrogramColorLegendMatchesLinkedRowColors(state_,rowColors_)){
            std::size_t longest=state_.colorByVariable.size();for(auto const& item:state_.colorByLegendItems)longest=std::max(longest,item.first.size());const double legendWidth=std::max(108.0,std::min(220.0,48.0+7.0*static_cast<double>(longest)));const double legendHeight=28.0+20.0*state_.colorByLegendItems.size();const double left=std::clamp(state_.colorByLegendX*width,4.0,std::max(4.0,width-legendWidth-4.0));const double top=std::clamp(state_.colorByLegendY*height,4.0,std::max(4.0,height-legendHeight-4.0));legendRect_={left,top,legendWidth,legendHeight};
            auto border=Controls::Border();border.Width(legendWidth);border.Height(legendHeight);border.Background(PlotBrush(theme.panel));border.BorderBrush(Brush(90,98,106));border.BorderThickness(Thickness{1.2});auto content=Controls::StackPanel();auto header=Controls::Grid();header.Height(28);header.Padding(Thickness{7,3,5,2});header.Background(PlotBrush(theme.majorGrid,0.28));auto title=Controls::TextBlock();title.Text(to_hstring(state_.colorByVariable));title.FontSize(11.5);title.FontWeight(Windows::UI::Text::FontWeights::SemiBold());title.Foreground(PlotBrush(theme.text));header.Children().Append(title);content.Children().Append(header);
            for(auto const& [level,colorName]:state_.colorByLegendItems){auto row=Controls::StackPanel();row.Orientation(Controls::Orientation::Horizontal);row.Height(20);row.Padding(Thickness{7,2,4,2});row.Background(PlotBrush(theme.panel,0.01));auto swatch=Shapes::Ellipse();swatch.Width(9);swatch.Height(9);swatch.Fill(PlotBrush(::rlispstat::core::PlotColorForNameOrHex(colorName)));swatch.Margin(Thickness{0,2,6,0});auto label=Controls::TextBlock();label.Text(to_hstring(level));label.FontSize(10.5);label.Foreground(PlotBrush(theme.text));row.Children().Append(swatch);row.Children().Append(label);row.PointerPressed([this,row,level](auto const&,auto const& event){if(!event.GetCurrentPoint(row).Properties().IsLeftButtonPressed())return;const auto semanticRows=::rlispstat::core::DendrogramColorLegendRows(state_,level);std::vector<int> rows(semanticRows.begin(),semanticRows.end());const auto modifiers=event.KeyModifiers();using winrt::Windows::System::VirtualKeyModifiers;const std::string mode=::rlispstat::core::EffectiveSelectionModeName((modifiers&VirtualKeyModifiers::Menu)==VirtualKeyModifiers::Menu,false,(modifiers&VirtualKeyModifiers::Control)==VirtualKeyModifiers::Control,(modifiers&VirtualKeyModifiers::Shift)==VirtualKeyModifiers::Shift,"replace");if(selectRowsCallback_&&!rows.empty())selectRowsCallback_(rows,mode);event.Handled(true);});content.Children().Append(row);}border.Child(content);Controls::Canvas::SetLeft(border,left);Controls::Canvas::SetTop(border,top);Controls::Canvas::SetZIndex(border,50);
            auto legendMenu=Controls::MenuFlyout();auto hide=Controls::MenuFlyoutItem();hide.Text(L"Hide legend");hide.Click([this](auto const&,auto const&){if(commandCallback_)commandCallback_({"DENDRO_TOGGLE_COLOR_LEGEND",state_.id});});legendMenu.Items().Append(hide);legendMenu.Items().Append(Controls::MenuFlyoutSeparator());for(auto const& position:std::vector<std::pair<std::string,std::wstring>>{{"top_left",L"Top left"},{"top_right",L"Top right"},{"bottom_left",L"Bottom left"},{"bottom_right",L"Bottom right"}}){auto item=Controls::MenuFlyoutItem();item.Text(position.second);item.Click([this,value=position.first](auto const&,auto const&){if(commandCallback_)commandCallback_({"DENDRO_SET_LEGEND_POSITION",state_.id,value});});legendMenu.Items().Append(item);}
            header.PointerPressed([this,header,left,top](auto const&,auto const& event){if(!event.GetCurrentPoint(header).Properties().IsLeftButtonPressed())return;const auto p=event.GetCurrentPoint(canvas_).Position();colorLegendDragging_=true;colorLegendMoved_=false;colorLegendDragStartX_=p.X;colorLegendDragStartY_=p.Y;colorLegendOriginalX_=left;colorLegendOriginalY_=top;header.CapturePointer(event.Pointer());event.Handled(true);});
            header.PointerMoved([this,border,width,height,legendWidth,legendHeight](auto const&,auto const& event){if(!colorLegendDragging_)return;const auto p=event.GetCurrentPoint(canvas_).Position();if(std::hypot(p.X-colorLegendDragStartX_,p.Y-colorLegendDragStartY_)>3)colorLegendMoved_=true;if(!colorLegendMoved_)return;const double nextLeft=std::clamp(colorLegendOriginalX_+p.X-colorLegendDragStartX_,4.0,std::max(4.0,width-legendWidth-4.0));const double nextTop=std::clamp(colorLegendOriginalY_+p.Y-colorLegendDragStartY_,4.0,std::max(4.0,height-legendHeight-4.0));state_.colorByLegendX=nextLeft/width;state_.colorByLegendY=nextTop/height;Controls::Canvas::SetLeft(border,nextLeft);Controls::Canvas::SetTop(border,nextTop);event.Handled(true);});
            header.PointerReleased([this](auto const&,auto const& event){if(!colorLegendDragging_)return;colorLegendDragging_=false;if(colorLegendMoved_&&commandCallback_)commandCallback_({"DENDRO_SET_LEGEND_COORDS",state_.id,std::to_string(state_.colorByLegendX),std::to_string(state_.colorByLegendY)});event.Handled(true);});border.ContextFlyout(legendMenu);border.RightTapped([border,legendMenu](auto const&,auto const& event){legendMenu.ShowAt(border);event.Handled(true);});canvas_.Children().Append(border);
        }
        if(dragging_){auto rect=::rlispstat::core::RectBetweenPoints(dragStart_,dragCurrent_);auto brush=Shapes::Rectangle();brush.Width(rect.width);brush.Height(rect.height);brush.Fill(Media::SolidColorBrush(Windows::UI::ColorHelper::FromArgb(38,32,113,180)));brush.Stroke(Brush(13,79,133));brush.StrokeThickness(1.2);Controls::Canvas::SetLeft(brush,rect.x);Controls::Canvas::SetTop(brush,rect.y);canvas_.Children().Append(brush);}
        if(!geometry_.hasCases){auto empty=ReportLabel(to_hstring((state_.status.empty() ? ::rlispstat::core::DendrogramEmptyPlotStatus(state_.variables.size()) : state_.status)).c_str(),24,80,13,PlotBrush(theme.mutedText));canvas_.Children().Append(empty);}
    }
    void DendrogramView::Activate(){if(!closed_)window_.Activate();}
    void DendrogramView::Close(){if(!closed_)window_.Close();}

    std::shared_ptr<GeneralizedModelView> GeneralizedModelView::Create(){auto view=std::shared_ptr<GeneralizedModelView>(new GeneralizedModelView());view->AttachLifetime();return view;}
    GeneralizedModelView::GeneralizedModelView(){Initialize();}
    void GeneralizedModelView::Initialize()
    {
        window_=CreateLinkEDAWindow();root_=Controls::Grid();root_.Padding(Thickness{18,14,18,12});root_.RowSpacing(5);root_.Background(Brush(255,255,255));root_.RequestedTheme(ElementTheme::Light);
        auto autoRow=[](){auto row=Controls::RowDefinition();row.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Auto));return row;};root_.RowDefinitions().Append(autoRow());root_.RowDefinitions().Append(autoRow());auto body=Controls::RowDefinition();body.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Star));root_.RowDefinitions().Append(body);
        auto heading=Controls::Grid();auto grow=Controls::ColumnDefinition();grow.Width(GridLengthHelper::FromValueAndType(1,GridUnitType::Star));heading.ColumnDefinitions().Append(grow);heading.ColumnDefinitions().Append(Controls::ColumnDefinition());title_=Controls::TextBlock();title_.FontSize(17);title_.FontWeight(Windows::UI::Text::FontWeights::SemiBold());heading.Children().Append(title_);badge_=Controls::TextBlock();badge_.FontSize(12);badge_.Foreground(Brush(72,72,72));Controls::Grid::SetColumn(badge_,1);heading.Children().Append(badge_);root_.Children().Append(heading);
        subtitle_=Controls::TextBlock();subtitle_.FontSize(11.5);subtitle_.Foreground(Brush(78,82,88));
        controls_=Controls::StackPanel();controls_.Spacing(5);controls_.Margin(Thickness{0,5,0,7});Controls::Grid::SetRow(controls_,1);root_.Children().Append(controls_);
        auto scroll=Controls::ScrollViewer();scroll.HorizontalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);scroll.VerticalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);scroll.HorizontalContentAlignment(HorizontalAlignment::Stretch);content_=Controls::StackPanel();content_.Name(L"LinkEDA.SnapshotTable");content_.Spacing(8);content_.HorizontalAlignment(HorizontalAlignment::Stretch);scroll.Content(content_);Controls::Grid::SetRow(scroll,2);root_.Children().Append(scroll);window_.Content(root_);exporter_=TableExportService::Create(window_,root_);ResizeLogical(window_,kModelResultWindowWidth,560);
    }
    void GeneralizedModelView::AttachLifetime(){std::weak_ptr<GeneralizedModelView> weak=shared_from_this();window_.Closed([weak](auto const&,auto const&){if(auto view=weak.lock()){view->closed_=true;if(view->closedCallback_)view->closedCallback_();}});}
    void GeneralizedModelView::SetClosedCallback(Closed callback){closedCallback_=std::move(callback);}
    void GeneralizedModelView::SetDiagnosticCallback(Diagnostic callback) {
      diagnosticCallback_ = std::move(callback);
    }
    void GeneralizedModelView::SetCommandCallback(
        Command callback, std::vector<std::string> responseCandidates,
        std::vector<std::string> predictorCandidates,
        std::vector<std::string> exposureCandidates) {
      commandCallback_ = std::move(callback);
      commandChoicesChanged_ = responseCandidates_ != responseCandidates ||
          predictorCandidates_ != predictorCandidates ||
          exposureCandidates_ != exposureCandidates;
      responseCandidates_ = std::move(responseCandidates);
      predictorCandidates_ = std::move(predictorCandidates);
      exposureCandidates_ = std::move(exposureCandidates);
    }

    void GeneralizedModelView::SetScopeChoices(
        std::vector<::rlispstat::core::AnalysisScopeChoice> choices)
    {
        scopeChoices_ = std::move(choices);
        commandChoicesChanged_ = true;
        RebuildControls();
    }
    void GeneralizedModelView::RebuildControls()
    {
      controls_.Children().Clear();
      if ((state_.precomputed && !state_.multipleImputation) || !state_.hasSeed) {
        const std::string modelDetails = state_.countRegression
          ? "     Distribution: " + ::rlispstat::core::CountDistributionLabel(state_.countDistribution) +
            "     Link: log" + (state_.exposure.empty() ? std::string{} :
              "     Exposure: " + state_.exposure)
          : "     Distribution: " +
            (::rlispstat::core::FindDistributionSpecification(state_.family)
                ? ::rlispstat::core::FindDistributionSpecification(state_.family)->visibleName : state_.family) +
            "     Link: " + state_.link;
        auto summary = DialogFieldLabel(to_hstring("Response: " + state_.response +
          modelDetails + "     Scope: " + state_.scope +
          (state_.offsetVariable.empty() ? std::string{} :
            "     Link-scale offset: " + state_.offsetVariable + " (used as stored)")).c_str());
        summary.Foreground(Brush(78,82,88)); controls_.Children().Append(summary); return;
      }
      auto row = Controls::StackPanel(); row.Orientation(Controls::Orientation::Horizontal); row.Spacing(8);
      auto label=[](std::wstring const& text){auto result=FieldLabel(text);result.FontSize(11.5);return result;};
      auto send=[this](std::string const& name,std::string const& value=std::string{})
      {if(commandCallback_)commandCallback_({name,state_.id,value});};
      row.Children().Append(label(L"Response:"));
      auto response=Controls::ComboBox();response.MinWidth(110);response.FontSize(11.5);
      for(auto const& value:responseCandidates_)response.Items().Append(box_value(to_hstring(value)));
      auto responseAt=std::find(responseCandidates_.begin(),responseCandidates_.end(),state_.response);
      if(responseAt!=responseCandidates_.end())response.SelectedIndex((int)(responseAt-responseCandidates_.begin()));
      response.SelectionChanged([send,response,current=state_.response](auto const&,auto const&)mutable{if(response.SelectedIndex()<0)return;auto value=to_string(response.SelectedItem().as<hstring>());if(value!=current)send("GGLM_SET_RESPONSE",value);});row.Children().Append(response);
      if (state_.countRegression) {
        const bool boundedCount =
          ::rlispstat::core::CountDistributionUsesTrials(state_.countDistribution);
        row.Children().Append(label(L"Distribution:"));
        auto distribution = Controls::ComboBox(); distribution.MinWidth(132); distribution.FontSize(11.5);
        const auto distributionSpecs =
          ::rlispstat::core::DistributionSpecificationsForModel(
            ::rlispstat::core::StatisticalModelType::Count);
        std::vector<std::string> distributionIds;
        for (auto const& value : distributionSpecs) {
          distributionIds.push_back(value.id);
          distribution.Items().Append(box_value(to_hstring(value.visibleName)));
        }
        const std::string displayedDistribution = state_.countDistribution ==
            ::rlispstat::core::CountDistribution::QuasiPoisson
          ? "poisson" : ::rlispstat::core::CountDistributionId(state_.countDistribution);
        auto distributionAt = std::find(distributionIds.begin(), distributionIds.end(), displayedDistribution);
        if (distributionAt != distributionIds.end())
          distribution.SelectedIndex(static_cast<int>(distributionAt - distributionIds.begin()));
        distribution.SelectionChanged([send, distribution, distributionIds, current=displayedDistribution](auto const&, auto const&) mutable {
          if (distribution.SelectedIndex() < 0) return;
          const auto& value = distributionIds[static_cast<std::size_t>(distribution.SelectedIndex())];
          if (value != current) send("GGLM_SET_COUNT_DISTRIBUTION", value);
        });
        row.Children().Append(distribution);
        if (!boundedCount) {
          row.Children().Append(label(L"Inference:"));
          auto inference = Controls::ComboBox(); inference.MinWidth(142); inference.FontSize(11.5);
          inference.Items().Append(box_value(L"Standard likelihood"));
          inference.Items().Append(box_value(L"Quasi-Poisson"));
          inference.SelectedIndex(state_.countDistribution ==
              ::rlispstat::core::CountDistribution::QuasiPoisson ? 1 : 0);
          inference.SelectionChanged([send, inference, current=state_.countDistribution](auto const&, auto const&) mutable {
            if (inference.SelectedIndex() == 1 && current != ::rlispstat::core::CountDistribution::QuasiPoisson)
              send("GGLM_SET_COUNT_DISTRIBUTION", "quasipoisson");
            else if (inference.SelectedIndex() == 0 && current == ::rlispstat::core::CountDistribution::QuasiPoisson)
              send("GGLM_SET_COUNT_DISTRIBUTION", "poisson");
          });
          row.Children().Append(inference);
        }
        row.Children().Append(label(L"Link:"));
        auto fixedLink = DialogFieldLabel(boundedCount ? L"logit" : L"log"); fixedLink.FontSize(11.5); fixedLink.VerticalAlignment(VerticalAlignment::Center);
        row.Children().Append(fixedLink);
        if (boundedCount) {
          row.Children().Append(label(L"Trials:"));
          auto trials = Controls::TextBox(); trials.Width(92); trials.FontSize(11.5);
          if (std::isfinite(state_.trialsConstant))
            trials.Text(to_hstring(std::to_string(static_cast<long long>(state_.trialsConstant))));
          trials.PlaceholderText(L"positive integer");
          auto applyTrials = Controls::Button(); applyTrials.Content(box_value(L"Apply")); applyTrials.FontSize(11.5);
          applyTrials.Click([send, trials](auto const&, auto const&) mutable {
            send("GGLM_SET_TRIALS", to_string(trials.Text()));
          });
          row.Children().Append(trials); row.Children().Append(applyTrials);
        } else {
          row.Children().Append(label(L"Exposure:"));
          auto exposure = Controls::ComboBox(); exposure.MinWidth(110); exposure.FontSize(11.5);
          exposure.Items().Append(box_value(L"None"));
          for (auto const& value : exposureCandidates_) exposure.Items().Append(box_value(to_hstring(value)));
          exposure.SelectedIndex(0);
          for (std::size_t index = 0; index < exposureCandidates_.size(); ++index)
            if (exposureCandidates_[index] == state_.exposure) { exposure.SelectedIndex(static_cast<int>(index + 1)); break; }
          exposure.SelectionChanged([send, exposure, candidates=exposureCandidates_, current=state_.exposure](auto const&, auto const&) mutable {
            if (exposure.SelectedIndex() < 0) return;
            const std::string value = exposure.SelectedIndex() == 0 ? std::string{} :
              candidates[static_cast<std::size_t>(exposure.SelectedIndex() - 1)];
            if (value != current) send("GGLM_SET_EXPOSURE", value.empty() ? "__none__" : value);
          });
          row.Children().Append(exposure);
        }
      } else {
        row.Children().Append(label(L"Distribution:"));
        auto family=Controls::ComboBox();family.MinWidth(92);family.FontSize(11.5);
        if(state_.binaryRegression){family.Items().Append(box_value(L"Binomial"));family.SelectedIndex(0);family.IsEnabled(false);}
        else{auto distributions=::rlispstat::core::DistributionSpecificationsForModel(state_.modelType);std::vector<std::string> ids;for(auto const& value:distributions){ids.push_back(value.id);family.Items().Append(box_value(to_hstring(value.visibleName)));}auto at=std::find(ids.begin(),ids.end(),state_.family);if(at!=ids.end())family.SelectedIndex((int)(at-ids.begin()));family.SelectionChanged([send,family,ids,current=state_.family](auto const&,auto const&)mutable{if(family.SelectedIndex()<0)return;auto value=ids[static_cast<std::size_t>(family.SelectedIndex())];if(value!=current)send("GGLM_SET_FAMILY",value);});}row.Children().Append(family);
        if(state_.binaryRegression&&state_.responseCoding.ok){row.Children().Append(label(L"Event category:"));auto event=Controls::ComboBox();event.MinWidth(92);event.FontSize(11.5);event.Items().Append(box_value(to_hstring(state_.responseCoding.referenceLabel)));event.Items().Append(box_value(to_hstring(state_.responseCoding.eventLabel)));event.SelectedIndex(1);event.SelectionChanged([send,event,reference=state_.responseCoding.referenceValue,eventValue=state_.responseCoding.eventValue](auto const&,auto const&)mutable{if(event.SelectedIndex()==0)send("GGLM_SET_EVENT",reference);else if(event.SelectedIndex()==1)send("GGLM_SET_EVENT",eventValue);});row.Children().Append(event);}
        row.Children().Append(label(L"Link:"));auto link=Controls::ComboBox();link.MinWidth(86);link.FontSize(11.5);std::vector<std::string> links;if(state_.binaryRegression){for(auto supported : ::rlispstat::core::SupportedBinaryLinks())links.push_back(::rlispstat::core::BinaryLinkId(supported));}else links=::rlispstat::core::LinksForModelDistribution(state_.modelType,state_.family);for(auto const& value:links)link.Items().Append(box_value(to_hstring(value)));for(uint32_t i=0;i<link.Items().Size();++i)if(to_string(link.Items().GetAt(i).as<hstring>())==state_.link){link.SelectedIndex((int)i);break;}link.SelectionChanged([send,link,current=state_.link](auto const&,auto const&)mutable{if(link.SelectedIndex()<0)return;auto value=to_string(link.SelectedItem().as<hstring>());if(value!=current)send("GGLM_SET_LINK",value);});row.Children().Append(link);
      }
      row.Children().Append(label(L"Scope:"));auto scope=Controls::ComboBox();scope.FontSize(11.5);auto choices=scopeChoices_;if(choices.empty())choices=::rlispstat::core::BuildAnalysisScopeChoices(0,{},true);const auto currentScope=::rlispstat::core::AnalysisScopeChoiceValue(state_.scope,state_.dataScope,state_.dataScopeCaptured);PopulateAnalysisScopeCombo(scope,choices,currentScope);scope.SelectionChanged([send,scope,current=currentScope](auto const&,auto const&)mutable{if(scope.SelectedIndex()<0)return;auto value=SelectedAnalysisScopeChoice(scope);if(value!=current)send("GGLM_SET_SCOPE",value);});row.Children().Append(scope);
      auto autoRefit=Controls::CheckBox();autoRefit.Content(box_value(L"Auto-refit"));autoRefit.IsChecked(state_.autoRefit);autoRefit.FontSize(11.5);autoRefit.Click([send](auto const&,auto const&)mutable{send("GGLM_TOGGLE_AUTO");});row.Children().Append(autoRefit);controls_.Children().Append(row);
      auto second=Controls::StackPanel();second.Orientation(Controls::Orientation::Horizontal);second.Spacing(8);
      if (state_.modelType == ::rlispstat::core::StatisticalModelType::LegacyGeneralized &&
          ::rlispstat::core::GeneralizedFamilyRequiresResponseBounds(state_.family)) {
        second.Children().Append(label(L"Bounds:"));
        auto lower = Controls::TextBox(); lower.Width(72); lower.FontSize(11.5);
        if (std::isfinite(state_.responseLower)) lower.Text(to_hstring(std::to_string(state_.responseLower)));
        lower.PlaceholderText(L"lower");
        auto upper = Controls::TextBox(); upper.Width(72); upper.FontSize(11.5);
        if (std::isfinite(state_.responseUpper)) upper.Text(to_hstring(std::to_string(state_.responseUpper)));
        upper.PlaceholderText(L"upper");
        auto applyBounds = Controls::Button(); applyBounds.Content(box_value(L"Apply bounds")); applyBounds.FontSize(11.5);
        applyBounds.Click([send, lower, upper](auto const&, auto const&) mutable {
          send("GGLM_SET_RESPONSE_BOUNDS", to_string(lower.Text()) + ";" + to_string(upper.Text()));
        });
        second.Children().Append(lower); second.Children().Append(label(L"to"));
        second.Children().Append(upper); second.Children().Append(applyBounds);
      }
	  const bool supportsOffset = state_.family != "beta_one_inflated" &&
	      (!state_.countRegression ||
	       (state_.countDistribution != ::rlispstat::core::CountDistribution::HurdleBetaBinomialCeiling &&
	        state_.countDistribution != ::rlispstat::core::CountDistribution::PerfectScore));
	  if (supportsOffset) {
	    second.Children().Append(label(L"Link offset:"));
	    auto offset = Controls::ComboBox(); offset.MinWidth(150); offset.FontSize(11.5);
	    Controls::ToolTipService::SetToolTip(offset, box_value(
	      L"Advanced: values are used as stored on the link scale with coefficient fixed at 1. For positive exposure or person-time, use Exposure; LinkEDA applies log(exposure) automatically."));
	    std::vector<std::string> offsetVariables;
	    offset.Items().Append(box_value(L"None"));
	    for (auto const& variable : ::rlispstat::core::NumericVariableNames(&state_.seed)) {
	      if (variable == state_.response || variable == state_.exposure) continue;
	      offsetVariables.push_back(variable);
	      offset.Items().Append(box_value(to_hstring(variable)));
	    }
	    offset.SelectedIndex(0);
	    for (std::size_t index = 0; index < offsetVariables.size(); ++index)
	      if (offsetVariables[index] == state_.offsetVariable) {
	        offset.SelectedIndex(static_cast<int>(index + 1));
	        break;
	      }
	    offset.SelectionChanged([send, offset, offsetVariables,
	                             current=state_.offsetVariable](auto const&, auto const&) mutable {
	      if (offset.SelectedIndex() < 0) return;
	      const std::string value = offset.SelectedIndex() == 0 ? std::string{} :
	        offsetVariables[static_cast<std::size_t>(offset.SelectedIndex() - 1)];
	      if (value != current) send("GGLM_SET_OFFSET", value.empty() ? "__none__" : value);
	    });
	    second.Children().Append(offset);
	  }
      second.Children().Append(label(L"Residuals:"));auto residual=Controls::ComboBox();residual.FontSize(11.5);auto residualTypes=::rlispstat::core::AvailableGeneralizedResidualTypes(state_);for(auto const& value:residualTypes)residual.Items().Append(box_value(to_hstring(::rlispstat::core::GeneralizedResidualTypeLabel(value))));auto residualAt=std::find(residualTypes.begin(),residualTypes.end(),state_.diagnosticOptions.residualType);residual.SelectedIndex(residualAt==residualTypes.end()?0:static_cast<int>(residualAt-residualTypes.begin()));residual.SelectionChanged([send,residual,residualTypes,current=state_.diagnosticOptions.residualType](auto const&,auto const&)mutable{auto index=residual.SelectedIndex();if(index<0||static_cast<size_t>(index)>=residualTypes.size())return;auto value=residualTypes[static_cast<size_t>(index)];if(value!=current)send("GGLM_SET_RESIDUAL",value);});second.Children().Append(residual);auto refit=Controls::Button();refit.Content(box_value(L"Refit"));refit.FontSize(11.5);refit.IsEnabled(!state_.rFitPending);refit.Click([send](auto const&,auto const&)mutable{send("GGLM_REFIT");});second.Children().Append(refit);
      // Diagnostics remain available from the contextual menu, alongside the
      // other model-specific operations; they do not occupy the header.
      controls_.Children().Append(second);
    }
    void GeneralizedModelView::ConfigureContextMenu() {
      auto menu = Controls::MenuFlyout();
      AppendMissingInformationMenu(menu,commandCallback_,state_.id,!state_.provenance.missingInformationRows.empty());
      auto send = [this](std::string command, std::string value = {}) {
        if (commandCallback_)
          commandCallback_({std::move(command), state_.id, std::move(value)});
      };
      if ((!state_.precomputed || state_.multipleImputation) && state_.hasSeed) {
        const auto &available = predictorCandidates_;
        auto response = Controls::MenuFlyoutSubItem();
        response.Text(L"Response variable");
        for (auto const &variable : responseCandidates_) {
          auto item = Controls::ToggleMenuFlyoutItem();
          item.Text(to_hstring(variable));
          item.IsChecked(variable == state_.response);
          item.Click([send, variable](auto const &, auto const &) mutable {
            send("GGLM_SET_RESPONSE", variable);
          });
          response.Items().Append(item);
        }
        menu.Items().Append(response);
        const bool supportsOffset = state_.family != "beta_one_inflated" &&
            (!state_.countRegression ||
             (state_.countDistribution != ::rlispstat::core::CountDistribution::HurdleBetaBinomialCeiling &&
              state_.countDistribution != ::rlispstat::core::CountDistribution::PerfectScore));
        if (supportsOffset) {
          auto offset = Controls::MenuFlyoutSubItem();
          offset.Text(L"Link-scale offset (advanced)");
          auto none = Controls::RadioMenuFlyoutItem();
          none.GroupName(to_hstring("generalized-offset:" + state_.id));
          none.Text(L"None");
          none.IsChecked(state_.offsetVariable.empty());
          none.Click([send](auto const&, auto const&) mutable {
            send("GGLM_SET_OFFSET", "__none__");
          });
          offset.Items().Append(none);
          for (auto const& variable : ::rlispstat::core::NumericVariableNames(&state_.seed)) {
            if (variable == state_.response || variable == state_.exposure) continue;
            auto item = Controls::RadioMenuFlyoutItem();
            item.GroupName(to_hstring("generalized-offset:" + state_.id));
            item.Text(to_hstring(variable));
            item.IsChecked(variable == state_.offsetVariable);
            item.Click([send, variable](auto const&, auto const&) mutable {
              send("GGLM_SET_OFFSET", variable);
            });
            offset.Items().Append(item);
          }
          menu.Items().Append(offset);
        }
        auto add = Controls::MenuFlyoutSubItem();
        add.Text(to_hstring(::rlispstat::core::GLMAddTermTitle()));
        for (auto const &variable : available)
          if (variable != state_.response &&
              std::find(state_.terms.begin(), state_.terms.end(), variable) ==
                  state_.terms.end()) {
            auto item = Controls::MenuFlyoutItem();
            item.Text(to_hstring(variable));
            item.Click([send, variable](auto const &, auto const &) mutable {
              send("GGLM_ADD_TERM", variable);
            });
            add.Items().Append(item);
          }
        AppendPolynomialMenu(add, PolynomialNumericVariables(available, state_, &state_.seed), state_.response, state_.terms,
            [send](std::string const& value) mutable { send("GGLM_ADD_TERM", value); });
        const auto interactionCandidates = ::rlispstat::core::InteractionCandidateTermsForModel(
            available, state_.response, state_.terms, state_.terms);
        if (auto interactions = BuildInteractionMenuSubItem(
                interactionCandidates,
                [send](std::string const& term) mutable { send("GGLM_ADD_TERM", term); }))
          add.Items().Append(interactions);
        if (add.Items().Size() > 0)
          menu.Items().Append(add);
        if (!state_.terms.empty()) {
          auto remove = Controls::MenuFlyoutSubItem();
          remove.Text(L"Remove term");
          for (auto const &term : state_.terms) {
            auto item = Controls::MenuFlyoutItem();
            item.Text(to_hstring(term));
            item.Click([send, term](auto const &, auto const &) mutable {
              send("GGLM_REMOVE_TERM", term);
            });
            remove.Items().Append(item);
          }
          menu.Items().Append(remove);
          auto types = Controls::MenuFlyoutSubItem();
          types.Text(L"Predictor type");
          for (auto const &term : state_.terms) {
            if (term.find(':') != std::string::npos)
              continue;
            auto termMenu = Controls::MenuFlyoutSubItem();
            termMenu.Text(to_hstring(term));
            Controls::RadioMenuFlyoutItem::SetAreCheckStatesEnabled(termMenu, true);
            const std::string current =
                ::rlispstat::core::ModelSpecificationTermType(state_, term);
            for (auto const &type :
                 {std::string("numeric"), std::string("factor")}) {
              auto item = Controls::RadioMenuFlyoutItem();
              item.GroupName(to_hstring("generalized-type:" + state_.group + ":" + term));
              item.Text(to_hstring(type == "numeric" ? "Treat predictor as continuous"
                                                        : "Treat predictor as categorical"));
              item.IsChecked(current == type);
              item.Click(
                  [send, term, type](auto const &, auto const &) mutable {
                    send("GGLM_SET_TYPE", term + "\x1f" + type);
                  });
              termMenu.Items().Append(item);
            }
            types.Items().Append(termMenu);
          }
          if (types.Items().Size() > 0)
            menu.Items().Append(types);
          auto transformations = Controls::MenuFlyoutSubItem();
          transformations.Text(L"Predictor transformations");
          for (auto const &term : state_.terms) {
            if (term.find(':') != std::string::npos ||
                ::rlispstat::core::ModelSpecificationTermType(state_, term) ==
                    "factor")
              continue;
            auto item = Controls::ToggleMenuFlyoutItem();
            item.Text(to_hstring(term));
            item.IsChecked(state_.centeredPredictors.count(term) != 0);
            item.Click([send, term](auto const &, auto const &) mutable {
              send("GGLM_TOGGLE_CENTER", term);
            });
            transformations.Items().Append(item);
          }
          if (transformations.Items().Size() > 0) {
            transformations.Items().Append(Controls::MenuFlyoutSeparator());
            auto centerAll = Controls::MenuFlyoutItem();
            centerAll.Text(L"Center all numeric predictors");
            centerAll.Click([send](auto const &, auto const &) mutable {
              send("GGLM_TOGGLE_CENTER", "__all__");
            });
            transformations.Items().Append(centerAll);
            auto removeAll = Controls::MenuFlyoutItem();
            removeAll.Text(L"Remove all centering");
            removeAll.IsEnabled(!state_.centeredPredictors.empty());
            removeAll.Click([send](auto const &, auto const &) mutable {
              send("GGLM_TOGGLE_CENTER", "__none__");
            });
            transformations.Items().Append(removeAll);
            menu.Items().Append(transformations);
          }
          auto references = Controls::MenuFlyoutSubItem();
          references.Text(L"Reference category");
          for (auto const &term : state_.terms) {
            if (term.find(':') != std::string::npos ||
                ::rlispstat::core::ModelSpecificationTermType(state_, term) !=
                    "factor")
              continue;
            std::vector<std::string> levels;
            for (auto const &row : state_.rows) {
              const std::string source =
                  row.sourceTerm.empty() ? row.term : row.sourceTerm;
              if (source == term && !row.factorLevel.empty() &&
                  std::find(levels.begin(), levels.end(), row.factorLevel) ==
                      levels.end())
                levels.push_back(row.factorLevel);
            }
            if (levels.empty()) continue;
            auto termMenu = Controls::MenuFlyoutSubItem();
            termMenu.Text(to_hstring(term));
            const auto selected = state_.factorReferenceLevels.find(term);
            for (auto const &level : levels) {
              auto item = Controls::RadioMenuFlyoutItem();
              item.GroupName(to_hstring("generalized-reference:" + state_.id +
                                        ":" + term));
              item.Text(to_hstring(level));
              item.IsChecked((selected != state_.factorReferenceLevels.end() &&
                              selected->second == level) ||
                             (selected == state_.factorReferenceLevels.end() &&
                              level == levels.front()));
              item.Click([send, term, level](auto const &, auto const &) mutable {
                send("GGLM_SET_REFERENCE", term + "\x1f" + level);
              });
              termMenu.Items().Append(item);
            }
            references.Items().Append(termMenu);
          }
          if (references.Items().Size() > 0) menu.Items().Append(references);
        }
        if (state_.countRegression) {
          auto distribution = Controls::MenuFlyoutSubItem();
          distribution.Text(L"Distribution");
          const std::string selectedDistribution = state_.countDistribution ==
              ::rlispstat::core::CountDistribution::QuasiPoisson
            ? "poisson"
            : ::rlispstat::core::CountDistributionId(state_.countDistribution);
          for (auto const& value :
               ::rlispstat::core::DistributionSpecificationsForModel(
                 ::rlispstat::core::StatisticalModelType::Count)) {
            auto item = Controls::RadioMenuFlyoutItem();
            item.GroupName(to_hstring("count-distribution:" + state_.id));
            item.Text(to_hstring(value.visibleName));
            item.IsChecked(value.id == selectedDistribution);
            item.Click([send, id=value.id](auto const &, auto const &) mutable {
              send("GGLM_SET_COUNT_DISTRIBUTION", id);
            });
            distribution.Items().Append(item);
          }
          menu.Items().Append(distribution);

          auto inference = Controls::MenuFlyoutSubItem();
          inference.Text(L"Dispersion / inference");
          for (auto const& option : std::vector<std::pair<std::string, std::wstring>>{
                 {"poisson", L"Standard likelihood model"},
                 {"quasipoisson", L"Quasi-Poisson"}}) {
            auto item = Controls::RadioMenuFlyoutItem();
            item.GroupName(to_hstring("count-inference:" + state_.id));
            item.Text(option.second);
            item.IsChecked(option.first == "quasipoisson"
                ? state_.countDistribution == ::rlispstat::core::CountDistribution::QuasiPoisson
                : state_.countDistribution != ::rlispstat::core::CountDistribution::QuasiPoisson);
            item.Click([send, value=option.first](auto const&, auto const&) mutable {
              send("GGLM_SET_COUNT_DISTRIBUTION", value);
            });
            inference.Items().Append(item);
          }
          menu.Items().Append(inference);

          auto exposure = Controls::MenuFlyoutSubItem();
          exposure.Text(L"Exposure");
          auto none = Controls::RadioMenuFlyoutItem();
          none.GroupName(to_hstring("count-exposure:" + state_.id));
          none.Text(L"None"); none.IsChecked(state_.exposure.empty());
          none.Click([send](auto const &, auto const &) mutable {
            send("GGLM_SET_EXPOSURE", "__none__");
          });
          exposure.Items().Append(none);
          for (auto const& variable : exposureCandidates_) {
            auto item = Controls::RadioMenuFlyoutItem();
            item.GroupName(to_hstring("count-exposure:" + state_.id));
            item.Text(to_hstring(variable));
            item.IsChecked(variable == state_.exposure);
            item.Click([send, variable](auto const &, auto const &) mutable {
              send("GGLM_SET_EXPOSURE", variable);
            });
            exposure.Items().Append(item);
          }
          menu.Items().Append(exposure);
        } else {
          auto family = Controls::MenuFlyoutSubItem();
          family.Text(L"Distribution");
          family.IsEnabled(!state_.binaryRegression);
          for (auto const &value :
               ::rlispstat::core::DistributionSpecificationsForModel(state_.modelType)) {
            auto item = Controls::ToggleMenuFlyoutItem();
            item.Text(to_hstring(value.visibleName));
            item.IsChecked(value.id == state_.family);
            item.Click([send, id=value.id](auto const &, auto const &) mutable {
              send("GGLM_SET_FAMILY", id);
            });
            family.Items().Append(item);
          }
          menu.Items().Append(family);
          auto links = Controls::MenuFlyoutSubItem();
          links.Text(L"Link");
          std::vector<std::string> supportedLinks;
          if (state_.binaryRegression) {
            for (auto supported : ::rlispstat::core::SupportedBinaryLinks())
              supportedLinks.push_back(::rlispstat::core::BinaryLinkId(supported));
          } else
            supportedLinks = ::rlispstat::core::LinksForModelDistribution(
                state_.modelType, state_.family);
          for (auto const &value : supportedLinks) {
              auto item = Controls::ToggleMenuFlyoutItem();
              item.Text(to_hstring(value));
              item.IsChecked(value == state_.link);
              item.Click([send, value](auto const &, auto const &) mutable {
                send("GGLM_SET_LINK", value);
              });
              links.Items().Append(item);
          }
          menu.Items().Append(links);
        }

        auto refit = Controls::MenuFlyoutItem();
        refit.Text(L"Refit model");
        refit.Click([send](auto const &, auto const &) mutable {
          send("GGLM_REFIT", "");
        });
        menu.Items().Append(refit);
        menu.Items().Append(Controls::MenuFlyoutSeparator());
      }
      if ((!state_.precomputed || state_.multipleImputation) && state_.binaryRegression &&
          state_.responseCoding.ok) {
        auto event = Controls::MenuFlyoutSubItem();
        event.Text(L"Modelled event");
        for (auto const &entry : {std::pair<std::string, std::string>{
                                      state_.responseCoding.eventValue,
                                      state_.responseCoding.eventLabel},
                                  {state_.responseCoding.referenceValue,
                                   state_.responseCoding.referenceLabel}}) {
          auto item = Controls::ToggleMenuFlyoutItem();
          item.Text(to_hstring(entry.second));
          item.IsChecked(entry.first == state_.responseCoding.eventValue);
          item.Click(
              [send, value = entry.first](auto const &, auto const &) mutable {
                send("GGLM_SET_EVENT", value);
              });
          event.Items().Append(item);
        }
        menu.Items().Append(event);
      }
      if (!state_.precomputed || state_.multipleImputation) {
        auto residuals = Controls::MenuFlyoutSubItem();
        residuals.Text(L"Residuals");
        for (auto const &entry :
             {std::pair<std::string, std::wstring>{"deviance", L"Deviance"},
              {"pearson", L"Pearson"},
              {"working", L"Working"}}) {
          auto item = Controls::ToggleMenuFlyoutItem();
          item.Text(entry.second);
          item.IsChecked(entry.first == state_.diagnosticOptions.residualType);
          item.Click(
              [send, value = entry.first](auto const &, auto const &) mutable {
                send("GGLM_SET_RESIDUAL", value);
              });
          residuals.Items().Append(item);
        }
        menu.Items().Append(residuals);
        auto autoRefit = Controls::ToggleMenuFlyoutItem();
        autoRefit.Text(L"Auto-refit");
        autoRefit.IsChecked(state_.autoRefit);
        autoRefit.Click([send](auto const &, auto const &) mutable {
          send("GGLM_TOGGLE_AUTO", "");
        });
        menu.Items().Append(autoRefit);
      }
      if (!state_.response.empty() && !state_.countRegression) {
        auto compare = Controls::MenuFlyoutItem();
        compare.Text(to_hstring(
            ::rlispstat::core::DefaultModelContextMenuTitles().compareModels));
        compare.Click([send](auto const &, auto const &) mutable {
          send("GGLM_COMPARE_MODELS", "");
        });
        menu.Items().Append(compare);
      }
      auto diagnostics = Controls::MenuFlyoutSubItem();
      diagnostics.Text(to_hstring(::rlispstat::core::DefaultModelContextMenuTitles().openDiagnostics));
      std::weak_ptr<GeneralizedModelView> weak = shared_from_this();
      auto addDiagnostic = [&](std::string const &title,
                               std::string const &kind) {
        auto item = Controls::MenuFlyoutItem();
        item.Text(to_hstring(title));
        item.Click([weak, kind](auto const &, auto const &) {
          if (auto view = weak.lock())
            if (view->diagnosticCallback_)
              view->diagnosticCallback_(kind);
        });
        diagnostics.Items().Append(item);
      };
      for (auto const &option :
           ::rlispstat::core::ModelDiagnosticPlotOptions(true)) {
        if ((state_.binaryRegression || state_.countRegression) &&
            (option.value == "scale_location" ||
             option.value == "residuals_leverage" ||
             option.value == "cooks_distance")) continue;
        addDiagnostic(option.title, option.value);
      }
      if (state_.binaryRegression) {
        addDiagnostic("ROC curve", "roc_curve");
        addDiagnostic("Calibration plot", "calibration_plot");
      }
      if (state_.binaryRegression || (state_.countRegression &&
          ::rlispstat::core::CountDistributionSupportsScoreDistribution(
              state_.countDistribution))) {
        addDiagnostic("Observed vs predicted distribution",
                      "observed_predicted_score_distribution");
        addDiagnostic("Boundary / zero fit", "boundary_zero_fit");
      }
      menu.Items().Append(diagnostics);
      if (exporter_) {
        menu.Items().Append(Controls::MenuFlyoutSeparator());
        auto exportMenu = exporter_->CreateMenu([this]() {
          return GeneralizedModelExportPayload(state_);
        });
        AppendRCodeExportItems(exportMenu, commandCallback_, state_.id);
        menu.Items().Append(exportMenu);
      }
      AttachWindowContextFlyout(window_, menu);
    }
    void GeneralizedModelView::Show(
        ::rlispstat::core::GeneralizedGLMState const &state) {
      ::rlispstat::windows::performance::Scope timing("Output.GeneralizedModel.Show");
      SetWindowDataSheetGroup(window_, state.group);
      SetWindowSnapshotSource(window_, "output", state.id, state.group);
      const std::string title = ::rlispstat::core::StatisticalModelWindowTitle(
          state.modelType, state.multipleImputation);
      window_.Title(to_hstring(title));
      title_.Text(to_hstring(title));
      badge_.Text(to_hstring(state.group));
      const std::string details =
          state.binaryRegression
              ? ("Event:  " + state.responseCoding.eventLabel +
                 "     Reference:  " + state.responseCoding.referenceLabel)
              : state.countRegression
                  ? ("Distribution:  " +
                     ::rlispstat::core::CountDistributionLabel(state.countDistribution) +
                     (state.exposure.empty() ? std::string{} :
                       "     Exposure:  " + state.exposure))
                  : ("Distribution:  " +
                    (::rlispstat::core::FindDistributionSpecification(state.family)
                      ? ::rlispstat::core::FindDistributionSpecification(state.family)->visibleName
                      : state.family));
      subtitle_.Text(to_hstring("Response:  " + state.response + "     " +
                                details + "     Link:  " + state.link +
                                "     Scope:  " + state.scope));
      std::ostringstream signature;
      signature << state.id << '\x1f' << state.response << '\x1f' << state.family
                << '\x1f' << state.link << '\x1f' << state.scope << '\x1f'
                << state.binaryRegression << '\x1f' << state.precomputed
                << '\x1f' << state.countRegression << '\x1f'
                << ::rlispstat::core::CountDistributionId(state.countDistribution)
                << '\x1f' << state.exposure << '\x1f' << state.diagnosticOptions.residualType
                << '\x1f' << state.autoRefit << '\x1f' << state.responseBoundsConfigured
                << '\x1f' << state.responseLower << '\x1f' << state.responseUpper;
      for (auto const& term : state.terms) signature << '\x1e' << term;
      for (auto const& [term, type] : state.termTypes)
        signature << '\x1d' << term << '=' << type;
      for (auto const& [term, type] : state.termTypeOverrides)
        signature << '\x1c' << term << '=' << type;
      for (auto const& term : state.centeredPredictors)
        signature << '\x1b' << term;
      for (auto const& [term, level] : state.factorReferenceLevels)
        signature << '\x1a' << term << '=' << level;
      const std::string nextControlsSignature = signature.str();
      const bool scopeNoticeChanged = state.frozenScopeNotice != state_.frozenScopeNotice;
      const bool controlsChanged = !reportInitialized_ || commandChoicesChanged_ ||
          controlsSignature_ != nextControlsSignature;
      state_ = state;
      auto presentationState = state;
      if (::rlispstat::core::GeneralizedGLMHasPendingManualFit(state) && !state.ok) {
        presentationState.rows =
            ::rlispstat::core::GeneralizedGLMRowsForPresentation(state);
        presentationState.termTests.clear();
        presentationState.warnings.clear();
        presentationState.ok = false;
        presentationState.status =
            ::rlispstat::core::GeneralizedGLMPendingManualFitStatus();
      }
      if (controlsChanged) {
        ::rlispstat::windows::performance::Scope controlsTiming(
            "Output.GeneralizedModel.Controls.Rebuild");
        RebuildControls(); ConfigureContextMenu();
        controlsSignature_ = nextControlsSignature; commandChoicesChanged_ = false;
      }
      // As with the ordinary GLM, keep the previous completed report visible
      // while the main R session computes its replacement.
      if (controlsChanged || scopeNoticeChanged || !reportInitialized_ || (!state.rFitPending &&
          (state.fitVersion != renderedFitVersion_ || state.ok != renderedOk_ ||
           state.status != renderedStatus_))) Render(presentationState);
      const std::size_t visibleRows = presentationState.rows.size() +
          ((!state.precomputed || state.multipleImputation) ? 1u : 0u);
      const double desiredHeight = std::max(
          500.0, 345.0 + visibleRows * 27.0);
      PresentOrResizeAnalysisWindow(window_, presented_,
                                    kModelResultWindowWidth, desiredHeight);
      windowCoefficientRows_ = visibleRows;
    }
    void GeneralizedModelView::InitializeReport()
    {
        if (reportInitialized_) return;
        content_.Children().Clear();
        auto section=[this](std::wstring const& value){auto label=FieldLabel(value);label.FontSize(13);label.FontWeight(Windows::UI::Text::FontWeights::SemiBold());content_.Children().Append(label);};
        section(L"Model fit");
        fitSummary_=FieldLabel(L"");fitSummary_.FontSize(11.5);content_.Children().Append(fitSummary_);
        fitDetails_=FieldLabel(L"");fitDetails_.FontSize(11);fitDetails_.Foreground(Brush(92,92,92));content_.Children().Append(fitDetails_);
        countFitGrid_=Controls::Grid();content_.Children().Append(countFitGrid_);
        section(L"Terms and coefficients");
        coefficientGrid_=Controls::Grid();content_.Children().Append(coefficientGrid_);
        status_=FieldLabel(L"");status_.FontSize(11);status_.Foreground(Brush(92,92,92));content_.Children().Append(status_);
        warnings_=Controls::StackPanel();warnings_.Spacing(2);content_.Children().Append(warnings_);
        reportInitialized_=true;
    }

    void GeneralizedModelView::RebuildCoefficientRows(::rlispstat::core::GeneralizedGLMState const& state)
    {
        ::rlispstat::windows::performance::Scope timing("Output.GeneralizedModel.CoefficientStructure.Rebuild");
        using namespace ::rlispstat::core;
        coefficientGrid_.Children().Clear();coefficientGrid_.ColumnDefinitions().Clear();
        coefficientGrid_.RowDefinitions().Clear();coefficientCells_.clear();coefficientStructure_.clear();
        std::vector<std::string> headers;
        std::vector<double> widths;
        if(state.binaryRegression)
        {
            headers={"Variable / category","B","SE",state.statisticName,"p",
                GeneralizedModelHasExponentiatedEffect(state)
                    ? GeneralizedExponentiatedEffectLabel(state)+" / 95% CI"
                    : "95% CI for B"};
            widths={260,78,78,64,64,200};
        }
        else if(GeneralizedModelHasExponentiatedEffect(state))
        {
            headers={"Variable / category","Type","b","SE",state.statisticName,"p",
                GeneralizedExponentiatedEffectLabel(state)+" / 95% CI"};
            widths={260,72,72,72,56,60,230};
        }
        else
        {
            headers={"Variable / category","Type","b","SE",state.statisticName,"p","95% CI"};
            widths={320,82,88,82,72,76,150};
        }
        coefficientStructure_.push_back(state.statisticName+"\x1f"+
            std::to_string(headers.size()));
        for(double width:widths){auto column=Controls::ColumnDefinition();column.Width(GridLengthHelper::FromPixels(width));coefficientGrid_.ColumnDefinitions().Append(column);}
        auto addRow=[this](){auto row=Controls::RowDefinition();row.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Auto));coefficientGrid_.RowDefinitions().Append(row);};
        auto addCell=[this](std::wstring const& text,int row,int column,bool header,bool label,bool bold=false,bool muted=false,bool indented=false,bool separator=false){auto border=TableCell(text,header,label,bold,muted,indented,separator);Controls::Grid::SetRow(border,row);Controls::Grid::SetColumn(border,column);coefficientGrid_.Children().Append(border);return border.Child().as<Controls::TextBlock>();};
        auto targetFor=[](Controls::TextBlock const& text){auto parent=text.Parent().try_as<FrameworkElement>();return parent?parent:text.as<FrameworkElement>();};
        auto send=[this](std::string const& command,std::string const& value){if(commandCallback_)commandCallback_({command,state_.id,value});};
        auto typeMenu=[this,send](std::string const& term)
        {
            auto menu=Controls::MenuFlyout();auto heading=Controls::MenuFlyoutItem();heading.Text(to_hstring(term));heading.IsEnabled(false);menu.Items().Append(heading);menu.Items().Append(Controls::MenuFlyoutSeparator());
            const std::string current=::rlispstat::core::ModelSpecificationTermType(state_,term);
            for(auto const& entry:std::vector<std::pair<std::string,std::wstring>>{{"numeric",L"Treat predictor as continuous"},{"factor",L"Treat predictor as categorical"}}){auto item=Controls::RadioMenuFlyoutItem();item.GroupName(to_hstring("generalized-cell-type:"+state_.id+":"+term));item.Text(entry.second);item.IsChecked(current==entry.first);item.Click([send,term,type=entry.first](auto const&,auto const&)mutable{send("GGLM_SET_TYPE",term+'\x1f'+type);});menu.Items().Append(item);}return menu;
        };
        auto replacementMenu=[this,send](std::string const& term)
        {
            auto menu=Controls::MenuFlyout();
            std::set<std::string> occupied;
            for(auto const& modelTerm:state_.terms)
            {
                if(modelTerm==term)continue;
                for(auto const& variable : ::rlispstat::core::UniqueBaseVariablesForTerm(modelTerm))
                    occupied.insert(variable);
            }
            const std::string current=::rlispstat::core::BaseVariableForTermComponent(term);
            for(auto const& candidate:predictorCandidates_)
            {
                if(candidate==state_.response||candidate==current||occupied.count(candidate)>0)
                    continue;
                auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(candidate));
                item.Click([send,term,candidate](auto const&,auto const&)mutable
                {send("GGLM_REPLACE_TERM",term+'\x1f'+candidate);});
                menu.Items().Append(item);
            }
            if(menu.Items().Size()==0)
            {
                auto empty=Controls::MenuFlyoutItem();empty.Text(L"No replacement variables available");
                empty.IsEnabled(false);menu.Items().Append(empty);
            }
            return menu;
        };
        auto variableMenu=[this,send](::rlispstat::core::GeneralizedGLMRow const& row)
        {
            const std::string term=!row.sourceTerm.empty()?row.sourceTerm:row.term;const bool interaction=term.find(':')!=std::string::npos;const bool editable=!term.empty()&&term!="(Intercept)"&&row.rowType!="intercept";
            auto menu=Controls::MenuFlyout();if(editable){auto remove=Controls::MenuFlyoutItem();remove.Text(interaction?L"Remove interaction":L"Remove predictor");remove.Click([send,term](auto const&,auto const&)mutable{send("GGLM_REMOVE_TERM",term);});menu.Items().Append(remove);menu.Items().Append(Controls::MenuFlyoutSeparator());}auto heading=Controls::MenuFlyoutItem();heading.Text(to_hstring(!row.displayLabel.empty()?row.displayLabel:term));heading.IsEnabled(false);menu.Items().Append(heading);menu.Items().Append(Controls::MenuFlyoutSeparator());
            auto analyze=Controls::MenuFlyoutSubItem();analyze.Text(L"Analyze predictor");
            if(editable&&!interaction)
            {
                auto effect=Controls::MenuFlyoutItem();effect.Text(L"Effect plot…");
                effect.Click([send,term](auto const&,auto const&)mutable{send("GGLM_INTERACTION_PLOT",term);});
                analyze.Items().Append(effect);
                auto partial=Controls::MenuFlyoutItem();partial.Text(L"Partial regression plot…");
                partial.Click([send,term](auto const&,auto const&)mutable{send("GGLM_PARTIAL_PLOT",term);});
                analyze.Items().Append(partial);
                const std::string type=::rlispstat::core::ModelSpecificationTermType(state_,term);
                if(type=="factor"||type=="ordered")
                {
                    auto pairwise=Controls::MenuFlyoutItem();pairwise.Text(L"Pairwise comparisons…");
                    pairwise.Click([send,term](auto const&,auto const&)mutable{send("GGLM_PAIRWISE",term);});
                    analyze.Items().Append(pairwise);
                }
            }
            if(editable&&interaction)
            {
                auto report=Controls::MenuFlyoutItem();report.Text(L"Interpret interaction…");
                report.Click([send,term](auto const&,auto const&)mutable{send("GGLM_INTERACTION_REPORT",term);});
                analyze.Items().Append(report);
                auto plot=Controls::MenuFlyoutItem();plot.Text(L"Effect plot…");
                plot.Click([send,term](auto const&,auto const&)mutable{send("GGLM_INTERACTION_PLOT",term);});
                analyze.Items().Append(plot);
            }
            if(analyze.Items().Size()>0)menu.Items().Append(analyze);
            if(editable&&!interaction){auto replace=Controls::MenuFlyoutSubItem();replace.Text(L"Change variable");for(auto const& candidate:predictorCandidates_){if(candidate==state_.response||candidate==term||std::find(state_.terms.begin(),state_.terms.end(),candidate)!=state_.terms.end())continue;auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(candidate));item.Click([send,term,candidate](auto const&,auto const&)mutable{send("GGLM_REPLACE_TERM",term+'\x1f'+candidate);});replace.Items().Append(item);}if(replace.Items().Size()>0)menu.Items().Append(replace);
                auto types=Controls::MenuFlyoutSubItem();types.Text(L"Change type");const std::string current=::rlispstat::core::ModelSpecificationTermType(state_,term);for(auto const& entry:std::vector<std::pair<std::string,std::wstring>>{{"numeric",L"Treat predictor as continuous"},{"factor",L"Treat predictor as categorical"}}){auto item=Controls::RadioMenuFlyoutItem();item.GroupName(to_hstring("generalized-variable-type:"+state_.id+":"+term));item.Text(entry.second);item.IsChecked(current==entry.first);item.Click([send,term,type=entry.first](auto const&,auto const&)mutable{send("GGLM_SET_TYPE",term+'\x1f'+type);});types.Items().Append(item);}menu.Items().Append(types);
                if(current!="factor"&&current!="ordered"){auto center=Controls::ToggleMenuFlyoutItem();center.Text(state_.centeredPredictors.count(term)?L"Remove centering":L"Center predictor");center.IsChecked(state_.centeredPredictors.count(term)!=0);center.Click([send,term](auto const&,auto const&)mutable{send("GGLM_TOGGLE_CENTER",term);});menu.Items().Append(center);}
                else{std::vector<std::string> levels;for(auto const& candidate:state_.rows){const std::string source=candidate.sourceTerm.empty()?candidate.term:candidate.sourceTerm;if(source==term&&!candidate.factorLevel.empty()&&std::find(levels.begin(),levels.end(),candidate.factorLevel)==levels.end())levels.push_back(candidate.factorLevel);}if(!levels.empty()){auto reference=Controls::MenuFlyoutSubItem();reference.Text(L"Reference category");const auto selected=state_.factorReferenceLevels.find(term);for(auto const& level:levels){auto item=Controls::RadioMenuFlyoutItem();item.GroupName(to_hstring("generalized-cell-reference:"+state_.id+":"+term));item.Text(to_hstring(level));item.IsChecked((selected!=state_.factorReferenceLevels.end()&&selected->second==level)||(selected==state_.factorReferenceLevels.end()&&level==levels.front()));item.Click([send,term,level](auto const&,auto const&)mutable{send("GGLM_SET_REFERENCE",term+'\x1f'+level);});reference.Items().Append(item);}menu.Items().Append(reference);}}
            }
            if(editable) AppendPolynomialMenu(menu, PolynomialNumericVariables(predictorCandidates_, state_, &state_.seed), state_.response, state_.terms,
                [send](std::string const& value) mutable { send("GGLM_ADD_TERM", value); }, term);
            if(editable){const auto interactionCandidates=::rlispstat::core::InteractionCandidateTermsForModel(predictorCandidates_,state_.response,state_.terms,state_.terms,term);if(auto interactions=BuildInteractionMenuSubItem(interactionCandidates,[send](std::string const& combined)mutable{send("GGLM_ADD_TERM",combined);},true,term)){menu.Items().Append(interactions);}}
            return menu;
        };
        auto withExplanation=[this](Controls::MenuFlyout menu,Controls::TextBlock text,
                                    std::string statistic,int row) {
            auto item=Controls::MenuFlyoutItem();item.Text(L"Explain statistic");
            item.Click([this,text,statistic,row](auto const&,auto const&) {
                ShowStatisticExplanation(text,GeneralizedModelStatisticContext(state_,statistic,row));
            });
            menu.Items().Append(item);return menu;
        };
        addRow();
        for(std::size_t column=0;column<headers.size();++column) {
            const bool labelColumn=state.binaryRegression?column==0:column<2;
            auto text=addCell(to_hstring(headers[column]).c_str(),0,(int)column,true,labelColumn);
            AttachDirectMenu(targetFor(text),CreateStatisticCellMenu(to_hstring(headers[column]).c_str(),text,
                GeneralizedStatisticContext(state,headers[column])));
        }
        int rowIndex=1;
        for(auto const& row:state.rows) {
            std::string label=row.displayLabel.empty()?row.term:row.displayLabel;
            const std::string source=row.sourceTerm.empty()?row.term:row.sourceTerm;
            const bool sectionRow=row.rowType=="section_header";
            if(source.find(':')==std::string::npos&&state.centeredPredictors.count(source)&&
                row.rowType!="factor_level"&&row.rowType!="reference"&&!sectionRow)label+=" (centered)";
            coefficientStructure_.push_back(label+"\x1f"+row.termType+"\x1f"+row.rowType);addRow();
            for(std::size_t column=0;column<headers.size();++column) {
                auto text=addCell(L"",rowIndex,(int)column,false,state.binaryRegression?column==0:column<2,
                    sectionRow||row.rowType=="factor_parent"||row.rowType=="term_parent",sectionRow,false,
                    row.rowType=="factor_level"||row.rowType=="reference",rowIndex>1);
                coefficientCells_.push_back(text);
                if(sectionRow) {
                    AttachDirectMenu(targetFor(text),CreateStatisticCellMenu(L"Model component",text,
                        GeneralizedModelStatisticContext(state,"Model component",rowIndex-1)));
                } else if(column==0) {
                    const bool replaceOnPrimary=(!state.precomputed||state.multipleImputation)&&!source.empty()&&
                        source!="(Intercept)"&&source.find(':')==std::string::npos&&
                        (row.rowType=="factor_parent"||row.sourceTerm.empty()||row.term==row.sourceTerm);
                    auto menu=withExplanation(variableMenu(row),text,"Variable / category",rowIndex-1);
                    if(replaceOnPrimary)AttachPrimaryAndContextMenus(targetFor(text),replacementMenu(source),menu);
                    else AttachDirectMenu(targetFor(text),menu);
                } else if(column==1&&!state.binaryRegression) {
                    auto menu=!source.empty()&&source!="(Intercept)"&&source.find(':')==std::string::npos
                        ?typeMenu(source):CreateStructuralCellMenu(L"Term type",text);
                    AttachDirectMenu(targetFor(text),withExplanation(menu,text,"Type",rowIndex-1));
                } else AttachDirectMenu(targetFor(text),CreateStatisticCellMenu(to_hstring(headers[column]).c_str(),text,
                    GeneralizedModelStatisticContext(state,headers[column],rowIndex-1)));
            }
            ++rowIndex;
        }
        if(!state.precomputed || state.multipleImputation)
        {
            addRow();auto addTerm=Controls::Button();addTerm.Content(box_value(L"+ Add term"));addTerm.FontSize(11.5);addTerm.HorizontalAlignment(HorizontalAlignment::Left);addTerm.Background(TransparentBrush());addTerm.BorderBrush(TransparentBrush());addTerm.BorderThickness(Thickness{0,0,0,0});addTerm.Padding(Thickness{8,4,8,4});auto addMenu=Controls::MenuFlyout();
            for(auto const& variable:predictorCandidates_){if(variable==state.response||std::find(state.terms.begin(),state.terms.end(),variable)!=state.terms.end())continue;auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(variable));item.Click([this,variable](auto const&,auto const&){if(commandCallback_)commandCallback_({"GGLM_ADD_TERM",state_.id,variable});});addMenu.Items().Append(item);}
            AppendPolynomialMenu(addMenu, PolynomialNumericVariables(predictorCandidates_, state, &state.seed), state.response, state.terms,
                [this](std::string const& term) { if(commandCallback_) commandCallback_({"GGLM_ADD_TERM",state_.id,term}); });
            const auto interactions=InteractionCandidateTermsForModel(predictorCandidates_,state.response,state.terms,state.terms);if(!interactions.empty()){if(addMenu.Items().Size()>0)addMenu.Items().Append(Controls::MenuFlyoutSeparator());auto interactionMenu=BuildInteractionMenuSubItem(interactions,[this](std::string const& term){if(commandCallback_)commandCallback_({"GGLM_ADD_TERM",state_.id,term});});if(interactionMenu)addMenu.Items().Append(interactionMenu);}
            if(addMenu.Items().Size()==0){auto empty=Controls::MenuFlyoutItem();empty.Text(L"No terms available");empty.IsEnabled(false);addMenu.Items().Append(empty);}AttachDirectMenu(addTerm,addMenu);Controls::Grid::SetRow(addTerm,rowIndex);Controls::Grid::SetColumn(addTerm,0);Controls::Grid::SetColumnSpan(addTerm,static_cast<int>(headers.size()));coefficientGrid_.Children().Append(addTerm);
        }
        coefficientRowsInitialized_=true;
    }

    void GeneralizedModelView::RebuildTermTests(
        ::rlispstat::core::GeneralizedGLMState const& state)
    {
        (void)state;
        // Global tests are part of the parent rows in coefficientGrid_.
        // The legacy hook remains for ABI/source compatibility only.
    }

    void GeneralizedModelView::Render(::rlispstat::core::GeneralizedGLMState const& state)
    {
        ::rlispstat::windows::performance::Scope timing("Output.GeneralizedModel.Values.Update");
        using namespace ::rlispstat::core;InitializeReport();
        std::string fit;
        if(!state.ok)fit=state.status;
        else {
            fit="N = "+std::to_string(state.n)+"     ";
            if(state.binaryRegression)fit+="LR chi-square = "+FormatDoubleOrDash(state.globalLR,3)+"     df = "+std::to_string(state.dfModel)+"     p = "+FormatPValue(state.globalP)+"     Nagelkerke R\u00B2 = "+FormatDoubleOrDash(state.nagelkerkeR2,3);
            else if(state.countRegression&&CountDistributionUsesTrials(state.countDistribution)){
                fit+="Trials = "+(!state.trialsVariable.empty()?state.trialsVariable:FormatDoubleOrDash(state.trialsConstant,0))+
                    "     "+std::string(state.multipleImputation&&!state.observedCeilingCountConstant?
                        "Mean observed at ceiling = ":"Observed at ceiling = ")+FormatDoubleOrDash(state.perfectScoreCount,1)+
                    " ("+FormatPercentOrDash(state.observedCeilingProportion,1)+")"+
                    "     "+std::string(state.multipleImputation?"Mean predicted at ceiling = ":"Predicted at ceiling = ")+
                    FormatDoubleOrDash(std::isfinite(state.predictedCeilingCount)?
                        state.predictedCeilingCount:state.n*state.predictedCeilingProportion,1)+
                    " ("+FormatPercentOrDash(state.predictedCeilingProportion,1)+")";
            }else fit+="Null deviance = "+FormatDoubleOrDash(state.nullDeviance,3)+"     Residual deviance = "+FormatDoubleOrDash(state.residualDeviance,3)+"     df = "+std::to_string(state.dfResidual);
        }
        fitSummary_.Text(to_hstring(fit));
        std::string details;
        if(!state.ok) details.clear();
        else if(state.likelihoodAvailable) details="AIC = "+FormatDoubleOrDash(state.aic,1)+"     BIC = "+FormatDoubleOrDash(state.bic,1)+"     Log likelihood = "+FormatDoubleOrDash(state.logLik,3);
        else if(state.countRegression&&state.countDistribution==CountDistribution::QuasiPoisson) details="Likelihood statistics unavailable (quasi-Poisson)";
        else if(state.multipleImputation) details="Likelihood statistics are not pooled across imputations";
        else details="Likelihood statistics unavailable";
        if(state.ok&&state.countRegression){
            if(state.countDistribution==CountDistribution::HurdleBetaBinomialCeiling)
                details="Perfect-score logistic component: fitted     Below-ceiling truncated beta-binomial: fitted";
            details += "\n" + CountModelParameterSummary(state);

        }
        else if(state.ok&&state.family=="lognormal") details+="     Residual variance (log scale) = "+FormatDoubleOrDash(state.dispersion,3);
        else if(state.ok&&state.family=="gaussian_log") details+="     Dispersion = "+FormatDoubleOrDash(state.dispersion,3);
        if(state.ok) details+="     Converged: "+std::string(state.converged?"yes":"no");
        fitDetails_.Text(to_hstring(details));
        countFitGrid_.Children().Clear();
        countFitGrid_.RowDefinitions().Clear();
        countFitGrid_.ColumnDefinitions().Clear();
        const bool showCountFit = state.ok && state.countRegression;
        countFitGrid_.Visibility(showCountFit ? Visibility::Visible : Visibility::Collapsed);
        fitSummary_.Visibility(showCountFit ? Visibility::Collapsed : Visibility::Visible);
        fitDetails_.Visibility(showCountFit ? Visibility::Collapsed : Visibility::Visible);
        if (showCountFit) {
            for (int column=0; column<4; ++column) {
                auto definition=Controls::ColumnDefinition();
                definition.Width(GridLengthHelper::FromValueAndType(1,GridUnitType::Star));
                countFitGrid_.ColumnDefinitions().Append(definition);
            }
            for (int row=0; row<4; ++row) {
                auto definition=Controls::RowDefinition();
                definition.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Auto));
                countFitGrid_.RowDefinitions().Append(definition);
            }
            auto field=[&](std::string label,std::string value,int row,int column,int span=1) {
                auto panel=Controls::StackPanel();panel.Margin(Thickness{0,0,12,8});
                auto heading=FieldLabel(to_hstring(label).c_str());
                heading.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
                auto text=FieldLabel(to_hstring(value).c_str());
                panel.Children().Append(heading);panel.Children().Append(text);
                AttachDirectMenu(panel,CreateStatisticCellMenu(to_hstring(label).c_str(),text,GeneralizedStatisticContext(state,label)));
                Controls::Grid::SetRow(panel,row);Controls::Grid::SetColumn(panel,column);
                Controls::Grid::SetColumnSpan(panel,span);countFitGrid_.Children().Append(panel);
            };
            field("N",std::to_string(state.n),0,0);
            if (CountDistributionUsesTrials(state.countDistribution)) {
                field("Trials",state.trialsVariable.empty()?FormatDoubleOrDash(state.trialsConstant,0):state.trialsVariable,0,1);
                field(state.multipleImputation&&!state.observedCeilingCountConstant?"Mean observed at ceiling":"Observed at ceiling",
                    FormatDoubleOrDash(state.perfectScoreCount,1)+" ("+FormatPercentOrDash(state.observedCeilingProportion,1)+")",0,2);
                field(state.multipleImputation?"Mean predicted at ceiling":"Predicted at ceiling",
                    FormatDoubleOrDash(state.predictedCeilingCount,1)+" ("+FormatPercentOrDash(state.predictedCeilingProportion,1)+")",0,3);
            } else {
                field("Null deviance",FormatDoubleOrDash(state.nullDeviance,3),0,1);
                field("Residual deviance",FormatDoubleOrDash(state.residualDeviance,3),0,2);
                field("df residual",std::to_string(state.dfResidual),0,3);
            }
            if (state.countDistribution==CountDistribution::HurdleBetaBinomialCeiling) {
                field("Perfect-score logistic component","Fitted",1,0,2);
                field("Below-ceiling truncated beta-binomial","Fitted",1,2,2);
            } else if (state.likelihoodAvailable) {
                field("Log likelihood",FormatDoubleOrDash(state.logLik,3),1,0);
                field("AIC",FormatDoubleOrDash(state.aic,3),1,1);
                field("BIC",FormatDoubleOrDash(state.bic,3),1,2);
            } else field("Likelihood statistics",state.multipleImputation
                ?"Not pooled across imputations":"Unavailable for this model",1,0,4);
            const auto parameter=CountModelParameterSummary(state);
            const auto colon=parameter.find(':');
            field(parameter.substr(0,colon),parameter.substr(colon+2),2,0,4);
            field("Converged",state.converged?"Yes":"No",3,0);
        }
        const std::size_t baseColumns=state.binaryRegression?6u:7u;
        const std::size_t columns=baseColumns;
        std::vector<std::string> structure{state.statisticName+"\x1f"+
            std::to_string(columns)};
        for(auto const& row:state.rows){std::string label=row.displayLabel.empty()?row.term:row.displayLabel;const std::string source=row.sourceTerm.empty()?row.term:row.sourceTerm;if(source.find(':')==std::string::npos&&state.centeredPredictors.count(source)&&row.rowType!="factor_level"&&row.rowType!="reference")label+=" (centered)";structure.push_back(label+"\x1f"+row.termType+"\x1f"+row.rowType);}
        if(!coefficientRowsInitialized_||structure!=coefficientStructure_)RebuildCoefficientRows(state);
        std::size_t cell=0;
        for(auto const& row:state.rows){const bool sectionRow=row.rowType=="section_header";const bool nested=row.rowType=="factor_level"||row.rowType=="reference";const bool parent=row.rowType=="factor_parent"||row.rowType=="term_parent";const bool reference=row.rowType=="reference";const auto structuralText=[&](std::string value){if(sectionRow||parent)return std::string{};if(reference)return std::string("\u2014");return value;};std::string label=row.displayLabel.empty()?row.term:row.displayLabel;const std::string source=row.sourceTerm.empty()?row.term:row.sourceTerm;if(source.find(':')==std::string::npos&&state.centeredPredictors.count(source)&&!nested&&!sectionRow)label+=" (centered)";
            const GlobalTermTestRow* termTest=
                GeneralizedGlobalTermTestForPresentationRow(state,row);
            std::vector<std::string> values;
            const std::string dash="\u2014";
            if(state.binaryRegression)
            {
                std::string effect=structuralText(GeneralizedModelHasExponentiatedEffect(state)
                    ?FormatDoubleOrDash(row.exponentiatedEstimate,3)+" ["+FormatDoubleOrDash(row.exponentiatedLower,3)+", "+FormatDoubleOrDash(row.exponentiatedUpper,3)+"]"
                    :"["+FormatDoubleOrDash(row.ciLower,3)+", "+FormatDoubleOrDash(row.ciUpper,3)+"]");
                values={label,structuralText(FormatDoubleOrDash(row.estimate,4)),structuralText(FormatDoubleOrDash(row.stdError,4)),sectionRow?std::string{}:(parent?(termTest?FormatDoubleOrDash(termTest->statistic,3):dash):(reference?dash:FormatDoubleOrDash(row.statistic,3))),sectionRow?std::string{}:(parent?(termTest?FormatPValue(termTest->pValue):dash):(reference?dash:FormatPValue(row.pValue)))};
                values.push_back(effect);
            }
            else
            {
                values={label,(nested||sectionRow)?std::string{}:row.termType,structuralText(FormatDoubleOrDash(row.estimate,4)),structuralText(FormatDoubleOrDash(row.stdError,4)),sectionRow?std::string{}:(parent?(termTest?FormatDoubleOrDash(termTest->statistic,3):dash):(reference?dash:FormatDoubleOrDash(row.statistic,3))),sectionRow?std::string{}:(parent?(termTest?FormatPValue(termTest->pValue):dash):(reference?dash:FormatPValue(row.pValue)))};
                if(GeneralizedModelHasExponentiatedEffect(state))values.push_back(structuralText(
                    FormatDoubleOrDash(row.exponentiatedEstimate,3)+" ["+FormatDoubleOrDash(row.exponentiatedLower,3)+", "+FormatDoubleOrDash(row.exponentiatedUpper,3)+"]"));
                else values.push_back(structuralText("["+FormatDoubleOrDash(row.ciLower,3)+", "+FormatDoubleOrDash(row.ciUpper,3)+"]"));
            }
            for(auto const& value:values)coefficientCells_[cell++].Text(to_hstring(value));}
        std::string statusText=state.rFitPending?std::string{}:state.status;if(!statusText.empty()&&statusText.find("excluded")==std::string::npos)statusText+=" \u00B7 "+std::to_string(state.excluded)+" rows excluded";if(!state.frozenScopeNotice.empty())statusText+=" \u00B7 "+state.frozenScopeNotice;status_.Text(to_hstring(statusText));status_.Visibility(statusText.empty()?Visibility::Collapsed:Visibility::Visible);
        warnings_.Children().Clear();for(auto const& warning:state.warnings){auto label=FieldLabel(to_hstring("Warning: "+warning).c_str());label.FontSize(11);label.Foreground(Brush(145,54,45));label.TextWrapping(TextWrapping::Wrap);warnings_.Children().Append(label);}const std::string methodNote=GeneralizedGlobalTermTestMethodNote(state);if(!methodNote.empty()){auto label=FieldLabel(to_hstring("Note: "+methodNote).c_str());label.FontSize(11);label.Foreground(Brush(92,92,92));label.TextWrapping(TextWrapping::Wrap);warnings_.Children().Append(label);}const std::string hierarchyNote=GeneralizedGlobalTermTestHierarchyNote(state);if(!hierarchyNote.empty()){auto label=FieldLabel(to_hstring("Note: "+hierarchyNote).c_str());label.FontSize(11);label.Foreground(Brush(92,92,92));label.TextWrapping(TextWrapping::Wrap);warnings_.Children().Append(label);}
        renderedFitVersion_=state.fitVersion;renderedOk_=state.ok;renderedStatus_=state.status;
    }
    void GeneralizedModelView::Activate(){if(!closed_)window_.Activate();}void GeneralizedModelView::Close(){if(!closed_)window_.Close();}

    std::shared_ptr<MixedModelView> MixedModelView::Create(){auto view=std::shared_ptr<MixedModelView>(new MixedModelView());view->AttachLifetime();return view;}
    MixedModelView::MixedModelView(){Initialize();}
    void MixedModelView::Initialize(){window_=CreateLinkEDAWindow();auto root=Controls::Grid();root.Padding(Thickness{18,14,18,12});root.RowSpacing(5);root.Background(Brush(255,255,255));root.RequestedTheme(ElementTheme::Light);auto autoRow=[](){auto row=Controls::RowDefinition();row.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Auto));return row;};root.RowDefinitions().Append(autoRow());root.RowDefinitions().Append(autoRow());auto body=Controls::RowDefinition();body.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Star));root.RowDefinitions().Append(body);auto heading=Controls::Grid();auto grow=Controls::ColumnDefinition();grow.Width(GridLengthHelper::FromValueAndType(1,GridUnitType::Star));heading.ColumnDefinitions().Append(grow);heading.ColumnDefinitions().Append(Controls::ColumnDefinition());title_=Controls::TextBlock();title_.FontSize(17);title_.FontWeight(Windows::UI::Text::FontWeights::SemiBold());heading.Children().Append(title_);badge_=Controls::TextBlock();badge_.FontSize(12);badge_.Foreground(Brush(72,72,72));Controls::Grid::SetColumn(badge_,1);heading.Children().Append(badge_);root.Children().Append(heading);subtitle_=Controls::TextBlock();subtitle_.FontSize(11.5);subtitle_.Foreground(Brush(78,82,88));subtitle_.TextWrapping(TextWrapping::Wrap);subtitle_.Margin(Thickness{0,5,0,7});Controls::Grid::SetRow(subtitle_,1);root.Children().Append(subtitle_);auto scroll=Controls::ScrollViewer();scroll.HorizontalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);scroll.VerticalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);content_=Controls::StackPanel();content_.Name(L"LinkEDA.SnapshotTable");content_.Spacing(10);scroll.Content(content_);Controls::Grid::SetRow(scroll,2);root.Children().Append(scroll);window_.Content(root);ResizeLogical(window_,940,700);}
    void MixedModelView::AttachLifetime(){std::weak_ptr<MixedModelView> weak=shared_from_this();window_.Closed([weak](auto const&,auto const&){if(auto view=weak.lock()){view->closed_=true;if(view->closedCallback_)view->closedCallback_();}});}void MixedModelView::SetClosedCallback(Closed callback){closedCallback_=std::move(callback);}
    void MixedModelView::SetCommandCallback(
        Command callback, ::rlispstat::core::NativeMixedModelState const& nativeState,
        std::vector<std::string> responseCandidates,
        std::vector<std::string> fixedEffectCandidates,
        std::vector<std::string> groupingCandidates, std::vector<std::string> numericCandidates)
    {
        polynomialCandidates_ = std::move(numericCandidates);
        commandCallback_ = std::move(callback); nativeState_ = nativeState;
        responseCandidates_ = std::move(responseCandidates);
        fixedEffectCandidates_ = std::move(fixedEffectCandidates);
        groupingCandidates_ = std::move(groupingCandidates);
        hasNativeState_ = true; ConfigureContextMenu();
    }
    void MixedModelView::CopyReport()
    {
        Windows::ApplicationModel::DataTransfer::DataPackage package;
        package.SetText(to_hstring(state_.rawText));
        Windows::ApplicationModel::DataTransfer::Clipboard::SetContent(package);
        Windows::ApplicationModel::DataTransfer::Clipboard::Flush();
    }
    void MixedModelView::ConfigureContextMenu()
    {
        auto menu = Controls::MenuFlyout();
        auto appendExport = [this, &menu]() {
            auto exportMenu = Controls::MenuFlyoutSubItem(); exportMenu.Text(L"Export");
            auto copyMenu = Controls::MenuFlyoutSubItem(); copyMenu.Text(L"Copy");
            auto copy = Controls::MenuFlyoutItem(); copy.Text(to_hstring(
                ::rlispstat::core::MixedModelCopyTableTitle()));
            copy.Click([this](auto const&, auto const&) { CopyReport(); });
            copyMenu.Items().Append(copy); exportMenu.Items().Append(copyMenu);
            AppendRCodeExportItems(exportMenu, commandCallback_, state_.id);
            if (menu.Items().Size() > 0)
                menu.Items().Append(Controls::MenuFlyoutSeparator());
            menu.Items().Append(exportMenu);
        };
        if (!hasNativeState_ || !commandCallback_) {
            appendExport();
            AttachWindowContextFlyout(window_, menu); return;
        }
        auto send = [this](std::string command, std::string value = {})
        { commandCallback_({ std::move(command), nativeState_.id, std::move(value) }); };

        auto response = Controls::MenuFlyoutSubItem();
        response.Text(to_hstring(::rlispstat::core::MixedModelResponseMenuTitle()));
        for (auto const& name : responseCandidates_)
        {
            auto item = Controls::ToggleMenuFlyoutItem();
            item.Text(to_hstring(name));
            item.IsChecked(name == nativeState_.response);
            item.Click([send, name](auto const&, auto const&) mutable { send("MIXED_SET_RESPONSE", name); });
            response.Items().Append(item);
        }
        menu.Items().Append(response);

        auto fixed = Controls::MenuFlyoutSubItem(); fixed.Text(L"Fixed effects");
        auto addFixed = Controls::MenuFlyoutSubItem();
        addFixed.Text(to_hstring(::rlispstat::core::MixedModelAddFixedEffectTitle()));
        for (auto const& name : fixedEffectCandidates_)
        {
            if (name == nativeState_.response ||
                std::find(nativeState_.fixedEffects.begin(), nativeState_.fixedEffects.end(), name) != nativeState_.fixedEffects.end()) continue;
            auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(name));
            item.Click([send, name](auto const&, auto const&) mutable { send("MIXED_ADD_FIXED", name); });
            addFixed.Items().Append(item);
        }
        AppendPolynomialMenu(addFixed, polynomialCandidates_, nativeState_.response, nativeState_.fixedEffects,
            [send](std::string const& value) mutable { send("MIXED_ADD_FIXED", value); });
        if (addFixed.Items().Size() > 0) fixed.Items().Append(addFixed);
        auto removeFixed = Controls::MenuFlyoutSubItem(); removeFixed.Text(L"Remove fixed effect");
        for (auto const& name : nativeState_.fixedEffects)
        {
            auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(name));
            item.Click([send, name](auto const&, auto const&) mutable { send("MIXED_REMOVE_FIXED", name); });
            removeFixed.Items().Append(item);
        }
        if (removeFixed.Items().Size() > 0) fixed.Items().Append(removeFixed);
        menu.Items().Append(fixed);

        auto random = Controls::MenuFlyoutSubItem(); random.Text(L"Random effects");
        auto addRandom = Controls::MenuFlyoutSubItem();
        addRandom.Text(to_hstring(::rlispstat::core::MixedModelAddRandomEffectTitle()));
        for (auto const& name : groupingCandidates_)
        {
            const bool used = std::any_of(nativeState_.randomEffects.begin(), nativeState_.randomEffects.end(),
                [&](auto const& spec) { return spec.group == name; });
            if (used || name == nativeState_.response) continue;
            auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(name));
            item.Click([send, name](auto const&, auto const&) mutable { send("MIXED_ADD_RANDOM", name); });
            addRandom.Items().Append(item);
        }
        if (addRandom.Items().Size() > 0) random.Items().Append(addRandom);
        for (auto const& spec : nativeState_.randomEffects)
        {
            auto group = Controls::MenuFlyoutSubItem(); group.Text(to_hstring(spec.group));
            auto addSlope = Controls::MenuFlyoutSubItem();
            addSlope.Text(to_hstring(::rlispstat::core::MixedModelAddRandomSlopeTitle()));
            for (auto const& name : fixedEffectCandidates_)
            {
                if (name == nativeState_.response || name == spec.group ||
                    std::find(spec.terms.begin(), spec.terms.end(), name) != spec.terms.end()) continue;
                auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(name));
                item.Click([send, groupName = spec.group, name](auto const&, auto const&) mutable
                    { send("MIXED_ADD_SLOPE", groupName + "\x1f" + name); });
                addSlope.Items().Append(item);
            }
            if (addSlope.Items().Size() > 0) group.Items().Append(addSlope);
            auto removeSlope = Controls::MenuFlyoutSubItem();
            removeSlope.Text(to_hstring(::rlispstat::core::MixedModelRemoveRandomSlopeTitle()));
            for (auto const& term : spec.terms)
            {
                if (term == "1") continue;
                auto item = Controls::MenuFlyoutItem(); item.Text(to_hstring(term));
                item.Click([send, groupName = spec.group, term](auto const&, auto const&) mutable
                    { send("MIXED_REMOVE_SLOPE", groupName + "\x1f" + term); });
                removeSlope.Items().Append(item);
            }
            if (removeSlope.Items().Size() > 0) group.Items().Append(removeSlope);
            group.Items().Append(Controls::MenuFlyoutSeparator());
            auto remove = Controls::MenuFlyoutItem(); remove.Text(L"Remove random effect");
            remove.IsEnabled(nativeState_.randomEffects.size() > 1);
            remove.Click([send, name = spec.group](auto const&, auto const&) mutable
                { send("MIXED_REMOVE_RANDOM", name); }); group.Items().Append(remove);
            random.Items().Append(group);
        }
        menu.Items().Append(random);

        if (nativeState_.modelType == "linear_mixed_model")
        {
            auto estimation = Controls::MenuFlyoutSubItem(); estimation.Text(L"Estimation");
            for (auto const& method : { std::string("REML"), std::string("ML") })
            {
                auto item = Controls::ToggleMenuFlyoutItem();
                item.Text(to_hstring(method));
                item.IsChecked(method == nativeState_.method);
                item.Click([send, method](auto const&, auto const&) mutable { send("MIXED_SET_METHOD", method); });
                estimation.Items().Append(item);
            }
            menu.Items().Append(estimation);
        }
        else
        {
            auto family = Controls::MenuFlyoutSubItem(); family.Text(L"Distribution");
            for (auto const& value : { std::string("binomial"), std::string("poisson") })
            {
                auto item = Controls::ToggleMenuFlyoutItem();
                item.Text(to_hstring(value));
                item.IsChecked(value == nativeState_.family);
                item.Click([send, value](auto const&, auto const&) mutable { send("MIXED_SET_FAMILY", value); });
                family.Items().Append(item);
            }
            menu.Items().Append(family);
            auto link = Controls::MenuFlyoutSubItem(); link.Text(L"Link");
            for (auto const& value : { std::string("logit"), std::string("probit"),
                                      std::string("cloglog"), std::string("log") })
            {
                if (!::rlispstat::core::IsValidGeneralizedLink(nativeState_.family, value)) continue;
                auto item = Controls::ToggleMenuFlyoutItem();
                item.Text(to_hstring(value));
                item.IsChecked(value == nativeState_.link);
                item.Click([send, value](auto const&, auto const&) mutable { send("MIXED_SET_LINK", value); });
                link.Items().Append(item);
            }
            menu.Items().Append(link);
        }
        auto refit = Controls::MenuFlyoutItem(); refit.Text(L"Refit model");
        refit.Click([send](auto const&, auto const&) mutable { send("MIXED_REFIT", ""); });
        menu.Items().Append(refit);
        appendExport();
        AttachWindowContextFlyout(window_, menu);
    }
    void MixedModelView::Show(::rlispstat::core::MixedModelReportState const& state){state_=state;SetWindowDataSheetGroup(window_,state.group);SetWindowSnapshotSource(window_,"output",state.id,state.group);window_.Title(to_hstring(state.title));title_.Text(to_hstring(state.title));badge_.Text(to_hstring(state.group));subtitle_.Text(to_hstring("Formula: "+state.formula+"     Scope: "+state.scope));Render(state);ConfigureContextMenu();PresentWindowOnce(window_,presented_,kModelResultWindowWidth,720);}
    void MixedModelView::Render(::rlispstat::core::MixedModelReportState const& state)
    {
        content_.Children().Clear();
        auto send=[this](std::string const& name,std::string const& value)
        {if(commandCallback_)commandCallback_({name,state_.id,value});};
        auto trim=[](std::string value){const auto first=value.find_first_not_of(" \t");const auto last=value.find_last_not_of(" \t");return first==std::string::npos?std::string{}:value.substr(first,last-first+1);};
        for(auto const& section:state.sections)
        {
            auto heading=FieldLabel(to_hstring(section.title).c_str());heading.FontSize(13);heading.FontWeight(Windows::UI::Text::FontWeights::SemiBold());content_.Children().Append(heading);
            if(section.title=="Warnings")
            {
                for(auto const& row:section.rows){if(row.empty())continue;auto warning=FieldLabel(to_hstring(row[0]).c_str());warning.FontSize(11);warning.Foreground(Brush(145,54,45));warning.TextWrapping(TextWrapping::Wrap);content_.Children().Append(warning);}continue;
            }
            std::size_t columns=0;for(auto const& row:section.rows)columns=std::max(columns,row.size());if(columns==0)continue;
            auto grid=Controls::Grid();for(std::size_t i=0;i<columns;++i){auto column=Controls::ColumnDefinition();column.Width(GridLengthHelper::FromPixels(i==0?190:125));grid.ColumnDefinitions().Append(column);}
            for(std::size_t r=0;r<section.rows.size();++r)
            {
                auto definition=Controls::RowDefinition();definition.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Auto));grid.RowDefinitions().Append(definition);
                for(std::size_t c=0;c<section.rows[r].size();++c)
                {
                    auto cell=TableCell(to_hstring(section.rows[r][c]).c_str(),r==0,c==0,false,false,r>0&&c==0&&section.rows[r][c].rfind("  ",0)==0,r>1);
                    Controls::Grid::SetRow(cell,(int)r);Controls::Grid::SetColumn(cell,(int)c);grid.Children().Append(cell);
                    auto text=cell.Child().as<Controls::TextBlock>();
                    if(r==0||c>0)
                    {
                        const std::string statistic=r==0?section.rows[r][c]:
                            (c<section.rows[0].size()?section.rows[0][c]:section.title);
                        AttachDirectMenu(cell,r==0&&c==0?
                            CreateStructuralCellMenu(to_hstring(section.title).c_str(),text):
                            CreateStatisticCellMenu(to_hstring(statistic).c_str(),text));
                    }
                    else
                    {
                        const std::string term=trim(section.rows[r][c]);auto menu=Controls::MenuFlyout();auto title=Controls::MenuFlyoutItem();title.Text(to_hstring(term));title.IsEnabled(false);menu.Items().Append(title);menu.Items().Append(Controls::MenuFlyoutSeparator());bool actionable=false;
                        if(std::find(nativeState_.fixedEffects.begin(),nativeState_.fixedEffects.end(),term)!=nativeState_.fixedEffects.end())
                        {auto remove=Controls::MenuFlyoutItem();remove.Text(L"Remove fixed effect");remove.Click([send,term](auto const&,auto const&)mutable{send("MIXED_REMOVE_FIXED",term);});menu.Items().Append(remove);actionable=true;}
                        auto random=std::find_if(nativeState_.randomEffects.begin(),nativeState_.randomEffects.end(),[&](auto const& spec){return spec.group==term;});
                        if(random!=nativeState_.randomEffects.end())
                        {auto remove=Controls::MenuFlyoutItem();remove.Text(L"Remove random effect");remove.IsEnabled(nativeState_.randomEffects.size()>1);remove.Click([send,term](auto const&,auto const&)mutable{send("MIXED_REMOVE_RANDOM",term);});menu.Items().Append(remove);actionable=true;}
                        if(!actionable){auto copy=Controls::MenuFlyoutItem();copy.Text(L"Copy");copy.Click([text](auto const&,auto const&){CopyTextToClipboard(text.Text());});menu.Items().Append(copy);}AttachDirectMenu(cell,menu);
                    }
                }
            }
            content_.Children().Append(grid);
        }
        auto status=FieldLabel(to_hstring(::rlispstat::core::MixedModelReportStatusLine(state)).c_str());status.FontSize(11);status.Foreground(Brush(92,92,92));content_.Children().Append(status);
    }
    void MixedModelView::Activate(){if(!closed_)window_.Activate();}void MixedModelView::Close(){if(!closed_)window_.Close();}

    std::shared_ptr<RegressionComparisonView> RegressionComparisonView::Create(){auto view=std::shared_ptr<RegressionComparisonView>(new RegressionComparisonView());view->AttachLifetime();return view;}
    RegressionComparisonView::RegressionComparisonView(){Initialize();}
    void RegressionComparisonView::Initialize(){window_=CreateLinkEDAWindow();auto root=Controls::Grid();root.Padding(Thickness{18,14,18,12});root.RowSpacing(5);root.Background(Brush(255,255,255));root.RequestedTheme(ElementTheme::Light);auto autoRow=[](){auto row=Controls::RowDefinition();row.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Auto));return row;};root.RowDefinitions().Append(autoRow());root.RowDefinitions().Append(autoRow());root.RowDefinitions().Append(autoRow());auto body=Controls::RowDefinition();body.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Star));root.RowDefinitions().Append(body);auto heading=Controls::Grid();auto grow=Controls::ColumnDefinition();grow.Width(GridLengthHelper::FromValueAndType(1,GridUnitType::Star));heading.ColumnDefinitions().Append(grow);heading.ColumnDefinitions().Append(Controls::ColumnDefinition());title_=Controls::TextBlock();title_.FontSize(17);title_.FontWeight(Windows::UI::Text::FontWeights::SemiBold());heading.Children().Append(title_);badge_=Controls::TextBlock();badge_.FontSize(12);badge_.Foreground(Brush(72,72,72));Controls::Grid::SetColumn(badge_,1);heading.Children().Append(badge_);root.Children().Append(heading);subtitle_=Controls::TextBlock();subtitle_.FontSize(11.5);subtitle_.Foreground(Brush(78,82,88));subtitle_.TextWrapping(TextWrapping::Wrap);subtitle_.Margin(Thickness{0,4,0,2});Controls::Grid::SetRow(subtitle_,1);root.Children().Append(subtitle_);controls_=Controls::StackPanel();controls_.Orientation(Controls::Orientation::Horizontal);controls_.Spacing(8);controls_.Margin(Thickness{0,2,0,7});Controls::Grid::SetRow(controls_,2);root.Children().Append(controls_);auto scroll=Controls::ScrollViewer();scroll.HorizontalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);scroll.VerticalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);content_=Controls::StackPanel();content_.Name(L"LinkEDA.SnapshotTable");content_.Spacing(10);scroll.Content(content_);Controls::Grid::SetRow(scroll,3);root.Children().Append(scroll);exporter_=TableExportService::Create(window_,root);window_.Content(root);ResizeLogical(window_,940,650);}
    void RegressionComparisonView::AttachLifetime(){std::weak_ptr<RegressionComparisonView> weak=shared_from_this();window_.Closed([weak](auto const&,auto const&){if(auto view=weak.lock()){view->closed_=true;if(view->closedCallback_)view->closedCallback_();}});}void RegressionComparisonView::SetClosedCallback(Closed callback){closedCallback_=std::move(callback);}
    void RegressionComparisonView::ShowMessage(std::string const& message){if(!closed_)ShowWorkflowMessage(window_,L"Interaction unavailable",message);}
    void RegressionComparisonView::SetCommandCallback(Command callback, Diagnostic diagnostic,
        std::vector<std::string> numericVariables, std::vector<std::string> availableVariables)
    {
        commandCallback_=std::move(callback);diagnosticCallback_=std::move(diagnostic);
        commandChoicesChanged_=numericVariables_!=numericVariables||availableVariables_!=availableVariables;
        numericVariables_=std::move(numericVariables);availableVariables_=std::move(availableVariables);
    }
    void RegressionComparisonView::SetScopeChoices(std::vector<::rlispstat::core::AnalysisScopeChoice> choices){scopeChoices_=std::move(choices);commandChoicesChanged_=true;RebuildControls();}
    void RegressionComparisonView::ConfigureContextMenu()
    {
        auto menu=Controls::MenuFlyout();
        AppendMissingInformationMenu(menu,commandCallback_,state_.id,!state_.provenance.missingInformationRows.empty());
        const bool canEdit=!state_.precomputed;
        auto send=[this](std::string const& name,std::string const& value=std::string{})
        {if(commandCallback_)commandCallback_({name,state_.id,value});};
        auto response=Controls::MenuFlyoutSubItem();response.Text(L"Response variable");
        for(auto const& variable:numericVariables_){auto item=Controls::ToggleMenuFlyoutItem();item.Text(to_hstring(variable));item.IsChecked(variable==state_.response);item.Click([send,variable](auto const&,auto const&)mutable{send("REGCMP_SET_RESPONSE",variable);});response.Items().Append(item);}response.IsEnabled(canEdit);menu.Items().Append(response);
        // Predictor and model operations belong to the cell or model-header
        // that identifies their target.  Keeping them out of the background
        // menu avoids the hidden "active model" routing used by the old UI.

        auto autoRefit=Controls::ToggleMenuFlyoutItem();autoRefit.Text(L"Auto-refit");autoRefit.IsChecked(state_.autoRefit);autoRefit.IsEnabled(canEdit);autoRefit.Click([send](auto const&,auto const&)mutable{send("REGCMP_TOGGLE_AUTO","");});menu.Items().Append(autoRefit);
        auto criteria=Controls::ToggleMenuFlyoutItem();criteria.Text(L"Information criteria");criteria.IsChecked(state_.showInformationCriteria);criteria.Click([send](auto const&,auto const&)mutable{send("REGCMP_TOGGLE_INFO","");});menu.Items().Append(criteria);
        auto refit=Controls::MenuFlyoutItem();refit.Text(L"Refit all models");refit.IsEnabled(canEdit);refit.Click([send](auto const&,auto const&)mutable{send("REGCMP_REFIT","");});menu.Items().Append(refit);
        if(exporter_){menu.Items().Append(Controls::MenuFlyoutSeparator());auto exportMenu=exporter_->CreateMenu([this](){return RegressionComparisonExportPayload(state_);},true);AppendRCodeExportItems(exportMenu,commandCallback_,state_.id);menu.Items().Append(exportMenu);}
        AttachWindowContextFlyout(window_, menu);
    }
    void RegressionComparisonView::RebuildControls()
    {
        controls_.Children().Clear();
        const bool canEdit=!state_.precomputed;
        auto smallLabel=[](std::wstring const& text){auto label=FieldLabel(text);label.FontSize(11.5);return label;};
        controls_.Children().Append(smallLabel(L"Response:"));
        auto response=Controls::ComboBox();response.MinWidth(105);response.FontSize(11.5);response.IsEnabled(canEdit);for(auto const& variable:numericVariables_)response.Items().Append(box_value(to_hstring(variable)));auto responseIndex=std::find(numericVariables_.begin(),numericVariables_.end(),state_.response);if(responseIndex!=numericVariables_.end())response.SelectedIndex(static_cast<int>(responseIndex-numericVariables_.begin()));response.SelectionChanged([this,response](auto const&,auto const&){if(commandCallback_&&response.SelectedIndex()>=0){const std::string value=to_string(response.SelectedItem().as<hstring>());if(value!=state_.response)commandCallback_({"REGCMP_SET_RESPONSE",state_.id,value});}});controls_.Children().Append(response);
        controls_.Children().Append(smallLabel(L"Scope:"));auto scope=Controls::ComboBox();scope.FontSize(11.5);scope.IsEnabled(canEdit);auto choices=scopeChoices_;if(choices.empty())choices=::rlispstat::core::BuildAnalysisScopeChoices(0,{},true);const auto currentScope=::rlispstat::core::AnalysisScopeChoiceValue(state_.scope,state_.dataScope,state_.dataScopeCaptured);PopulateAnalysisScopeCombo(scope,choices,currentScope);scope.SelectionChanged([this,scope,current=currentScope](auto const&,auto const&){if(commandCallback_&&scope.SelectedIndex()>=0){const auto value=SelectedAnalysisScopeChoice(scope);if(value!=current)commandCallback_({"REGCMP_SET_SCOPE",state_.id,value});}});controls_.Children().Append(scope);
        auto autoRefit=Controls::CheckBox();autoRefit.Content(box_value(L"Auto-refit"));autoRefit.IsChecked(state_.autoRefit);autoRefit.IsEnabled(canEdit);autoRefit.FontSize(11.5);autoRefit.Click([this](auto const&,auto const&){if(commandCallback_)commandCallback_({"REGCMP_TOGGLE_AUTO",state_.id,""});});controls_.Children().Append(autoRefit);
    }
    void RegressionComparisonView::Show(::rlispstat::core::RegressionComparisonState const& state)
    {
        ::rlispstat::windows::performance::Scope timing("Output.RegressionComparison.Show");
        std::ostringstream signature;
        signature<<state.id<<'\x1f'<<state.response<<'\x1f'<<state.scope<<'\x1f'
                 <<state.autoRefit<<'\x1f'<<state.showInformationCriteria<<'\x1f'<<state.activeModel;
        for(auto const& term:state.termRows)signature<<'\x1e'<<term;
        for(auto const& model:state.models){signature<<'\x1d'<<model.id<<'='<<model.label;for(auto const& term:model.terms)signature<<'\x1c'<<term;signature<<'\x1b';for(auto const& [term,type]:model.termTypeOverrides)signature<<term<<'='<<type<<'\x19';for(auto const& term:model.centeredPredictors)signature<<term<<'\x1a';}
        const std::string nextSignature=signature.str();
        const bool controlsChanged=!reportInitialized_||commandChoicesChanged_||controlsSignature_!=nextSignature;
        state_=state;SetWindowDataSheetGroup(window_,state.group);SetWindowSnapshotSource(window_,"output",state.id,state.group);
        const std::string title=state.title.empty()?"Compare Linear Models":state.title;
        window_.Title(to_hstring(title));title_.Text(to_hstring(title));badge_.Text(to_hstring(state.group));
        const bool hasTerms=std::any_of(state.models.begin(),state.models.end(),[](auto const& model){return !model.terms.empty();});
        std::string subtitle;
        if(state.response.empty()) subtitle="Choose a response variable.";
        else {
            subtitle="Response:  "+state.response+"     Scope:  "+state.scope+"     "+std::to_string(state.models.size())+" models";
            if(!hasTerms) subtitle+="     Intercept-only model";
            if(state.rFitPending) subtitle+="     Updating…";
            else {
                auto failed=std::find_if(state.models.begin(),state.models.end(),[&](auto const& model){return ::rlispstat::core::RegressionComparisonResolvedFitState(state,model)==::rlispstat::core::RegressionComparisonFitState::Error;});
                if(failed!=state.models.end()) subtitle+="     Fit error: "+(failed->fit.warning.empty()?std::string("model could not be fitted"):failed->fit.warning);
            }
        }
        if (!state.frozenScopeNotice.empty()) subtitle += "     " + state.frozenScopeNotice;
        subtitle_.Text(to_hstring(subtitle));
        if(controlsChanged){RebuildControls();ConfigureContextMenu();controlsSignature_=nextSignature;commandChoicesChanged_=false;}
        Render(state);reportInitialized_=true;
        const auto fitRows=::rlispstat::core::RegressionComparisonVisibleFitRows(state.showInformationCriteria).size();
        const double width=std::clamp(420.0+state.models.size()*180.0,680.0,kWideResultWindowMaximumWidth);
        const double height=std::max(430.0,
            170.0+(state.termRows.size()+fitRows+3.0)*28.0);
        PresentOrResizeAnalysisWindow(window_,presented_,width,height);
    }
    void RegressionComparisonView::Render(::rlispstat::core::RegressionComparisonState const& state)
    {
        content_.Children().Clear();if(state.response.empty()){auto prompt=FieldLabel(L"Choose a response variable to begin.");prompt.FontSize(14);prompt.Foreground(Brush(78,82,88));content_.Children().Append(prompt);return;}
        using namespace ::rlispstat::core;
        auto send=[this](std::string const& name,std::string const& value){if(commandCallback_)commandCallback_({name,state_.id,value});};
        auto build=[&](std::vector<std::pair<std::string,std::vector<std::string>>> const& rows,
                       std::size_t termRowCount)
        {
            const int activeModel=state.models.empty()?0:std::clamp(state.activeModel,0,(int)state.models.size()-1);
            auto grid=Controls::Grid();auto stub=Controls::ColumnDefinition();stub.Width(GridLengthHelper::FromPixels(300));grid.ColumnDefinitions().Append(stub);for(std::size_t i=0;i<state.models.size();++i){auto column=Controls::ColumnDefinition();column.Width(GridLengthHelper::FromPixels(180));grid.ColumnDefinitions().Append(column);}auto addModelColumn=Controls::ColumnDefinition();addModelColumn.Width(GridLengthHelper::FromPixels(120));grid.ColumnDefinitions().Append(addModelColumn);
            auto addRow=[&](){auto row=Controls::RowDefinition();row.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Auto));grid.RowDefinitions().Append(row);};
            auto add=[&](FrameworkElement const& cell,int row,int column){Controls::Grid::SetRow(cell,row);Controls::Grid::SetColumn(cell,column);grid.Children().Append(cell);};
            auto finishMenu=[&](Controls::MenuFlyout const& menu)
            {
                if (exporter_) {
                    menu.Items().Append(Controls::MenuFlyoutSeparator());
                    auto exportMenu = exporter_->CreateMenu(
                        [snapshot=state](){return RegressionComparisonExportPayload(snapshot);},
                        true);
                    AppendRCodeExportItems(exportMenu, commandCallback_, state.id);
                    menu.Items().Append(exportMenu);
                }
                AppendTableAnnotationContextItems(window_, menu);
                return menu;
            };
            auto modelMenu=[&](std::size_t modelIndex)
            {
                auto menu=Controls::MenuFlyout();
                auto title=Controls::MenuFlyoutItem();title.Text(to_hstring(state.models[modelIndex].label));title.IsEnabled(false);menu.Items().Append(title);menu.Items().Append(Controls::MenuFlyoutSeparator());
                auto rename=Controls::MenuFlyoutItem();rename.Text(L"Rename model…");rename.IsEnabled(!state.precomputed);rename.Click([this,send,modelIndex](auto const&,auto const&)mutable{auto input=Controls::TextBox();input.Text(to_hstring(state_.models[modelIndex].label));input.SelectAll();auto dialog=Controls::ContentDialog();ConfigureNativeDialog(dialog,window_,L"Rename model",input,L"Rename",360);auto operation=dialog.ShowAsync();operation.Completed([dialog,input,send,modelIndex](auto const& completed,Windows::Foundation::AsyncStatus status)mutable{if(status==Windows::Foundation::AsyncStatus::Completed&&completed.GetResults()==Controls::ContentDialogResult::Primary){const auto label=to_string(input.Text());if(!label.empty())send("REGCMP_RENAME_MODEL",std::to_string(modelIndex)+'\x1f'+label);}});});menu.Items().Append(rename);
                auto terms=Controls::MenuFlyoutSubItem();terms.Text(L"Model terms");terms.IsEnabled(!state.precomputed);
                std::set<std::string> listedTerms;for(auto const& row:state.termRows){const std::string term=RegressionComparisonSourceTerm(state,row);if(term=="(Intercept)"||!listedTerms.insert(term).second)continue;auto item=Controls::ToggleMenuFlyoutItem();item.Text(to_hstring(term));item.IsChecked(ModelIncludesTerm(state.models[modelIndex],term));const std::string encoded=std::to_string(modelIndex)+'\x1f'+term;item.Click([send,encoded](auto const&,auto const&)mutable{send("REGCMP_TOGGLE_TERM",encoded);});terms.Items().Append(item);}menu.Items().Append(terms);
                auto diagnostics=Controls::MenuFlyoutSubItem();diagnostics.Text(to_hstring(DefaultModelContextMenuTitles().openDiagnostics));
                for(auto const& option:ModelDiagnosticPlotOptions(true)){auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(option.title));item.Click([this,modelIndex,kind=option.value](auto const&,auto const&){if(diagnosticCallback_)diagnosticCallback_((int)modelIndex,kind);});diagnostics.Items().Append(item);}menu.Items().Append(diagnostics);
                auto open=Controls::MenuFlyoutItem();open.Text(L"Open as single GLM");open.Click([send,modelIndex](auto const&,auto const&)mutable{send("REGCMP_OPEN_SINGLE",std::to_string(modelIndex));});menu.Items().Append(open);
                auto refit=Controls::MenuFlyoutItem();refit.Text(L"Refit this model");refit.IsEnabled(!state.precomputed);refit.Click([send,modelIndex](auto const&,auto const&)mutable{send("REGCMP_REFIT_MODEL",std::to_string(modelIndex));});menu.Items().Append(refit);
                auto duplicate=Controls::MenuFlyoutItem();duplicate.Text(L"Duplicate model");duplicate.IsEnabled(!state.precomputed);duplicate.Click([send,modelIndex](auto const&,auto const&)mutable{send("REGCMP_DUPLICATE_MODEL",std::to_string(modelIndex));});menu.Items().Append(duplicate);
                auto remove=Controls::MenuFlyoutItem();remove.Text(L"Delete model");remove.IsEnabled(!state.precomputed&&state.models.size()>1);remove.Click([send,modelIndex](auto const&,auto const&)mutable{send("REGCMP_DELETE_MODEL",std::to_string(modelIndex));});menu.Items().Append(remove);
                return finishMenu(menu);
            };
            auto termMenu=[&](std::string const& term,std::size_t modelIndex,Controls::TextBlock const& valueText)
            {
                auto menu=Controls::MenuFlyout();auto remove=Controls::MenuFlyoutItem();remove.Text(term.find(':')==std::string::npos?L"Remove predictor from comparison":L"Remove interaction from comparison");remove.IsEnabled(!state.precomputed);remove.Click([send,term](auto const&,auto const&)mutable{send("REGCMP_REMOVE_TERM",term);});menu.Items().Append(remove);menu.Items().Append(Controls::MenuFlyoutSeparator());auto title=Controls::MenuFlyoutItem();title.Text(to_hstring(term+" · "+state.models[modelIndex].label));title.IsEnabled(false);menu.Items().Append(title);menu.Items().Append(Controls::MenuFlyoutSeparator());
                auto analyze=Controls::MenuFlyoutSubItem();analyze.Text(L"Analyze predictor");
                auto details=Controls::MenuFlyoutItem();details.Text(L"Show coefficient details");details.Click([send,modelIndex,term](auto const&,auto const&)mutable{send("REGCMP_SHOW_DETAILS",std::to_string(modelIndex)+'\x1f'+term);});analyze.Items().Append(details);
                auto explain=Controls::MenuFlyoutItem();explain.Text(to_hstring(DefaultModelContextMenuTitles().explainStatistic));explain.Click([valueText,context=RegressionComparisonStatisticContext(state,modelIndex,"b")](auto const&,auto const&){ShowStatisticExplanation(valueText,context);});analyze.Items().Append(explain);
                const std::string type=RegressionComparisonModelTermType(state,(int)modelIndex,term);
                if(term.find(':')==std::string::npos){auto effect=Controls::MenuFlyoutItem();effect.Text(L"Effect plot…");effect.Click([send,modelIndex,term](auto const&,auto const&)mutable{send("REGCMP_INTERACTION_PLOT",std::to_string(modelIndex)+'\x1f'+term);});analyze.Items().Append(effect);auto partial=Controls::MenuFlyoutItem();partial.Text(L"Partial regression plot…");partial.Click([send,modelIndex,term](auto const&,auto const&)mutable{send("REGCMP_PARTIAL_PLOT",std::to_string(modelIndex)+'\x1f'+term);});analyze.Items().Append(partial);}
                if(term.find(':')==std::string::npos&&(type=="factor"||type=="ordered")){auto pairwise=Controls::MenuFlyoutItem();pairwise.Text(L"Pairwise comparisons…");pairwise.Click([send,modelIndex,term](auto const&,auto const&)mutable{send("REGCMP_PAIRWISE",std::to_string(modelIndex)+'\x1f'+term);});analyze.Items().Append(pairwise);}
                if(term.find(':')!=std::string::npos){auto interpretation=Controls::MenuFlyoutItem();interpretation.Text(L"Interpret interaction…");interpretation.Click([send,modelIndex,term](auto const&,auto const&)mutable{send("REGCMP_INTERACTION_REPORT",std::to_string(modelIndex)+'\x1f'+term);});analyze.Items().Append(interpretation);auto plot=Controls::MenuFlyoutItem();plot.Text(L"Effect plot…");plot.Click([send,modelIndex,term](auto const&,auto const&)mutable{send("REGCMP_INTERACTION_PLOT",std::to_string(modelIndex)+'\x1f'+term);});analyze.Items().Append(plot);}
                menu.Items().Append(analyze);
                auto modelTerms=Controls::MenuFlyoutSubItem();modelTerms.Text(L"Model terms");modelTerms.IsEnabled(!state.precomputed);
                auto inModel=Controls::ToggleMenuFlyoutItem();inModel.Text(L"Include in this model");inModel.IsChecked(ModelIncludesTerm(state.models[modelIndex],term));inModel.Click([send,modelIndex,term](auto const&,auto const&)mutable{send("REGCMP_TOGGLE_TERM",std::to_string(modelIndex)+'\x1f'+term);});modelTerms.Items().Append(inModel);
                auto addAll=Controls::MenuFlyoutItem();addAll.Text(L"Add to all models");addAll.Click([send,term](auto const&,auto const&)mutable{send("REGCMP_ADD_TERM_ALL",term);});modelTerms.Items().Append(addAll);
                {std::vector<std::vector<std::string>> comparisonTerms;for(auto const& model:state.models)comparisonTerms.push_back(model.terms);const auto candidates=InteractionCandidateTermsForComparison(availableVariables_,state.response,comparisonTerms,state.models[modelIndex].terms,term);if(auto interactions=BuildInteractionMenuSubItem(candidates,[send,modelIndex](std::string const& interaction)mutable{send("REGCMP_ADD_INTERACTION",std::to_string(modelIndex)+'\x1f'+interaction);},!state.precomputed,term)){modelTerms.Items().Append(interactions);}}
                menu.Items().Append(modelTerms);
                auto edit=Controls::MenuFlyoutSubItem();edit.Text(L"Edit predictor");edit.IsEnabled(!state.precomputed);
                if(term.find(':')==std::string::npos&&term.find('=')==std::string::npos){auto types=Controls::MenuFlyoutSubItem();types.Text(L"Change type");for(auto const& entry:std::vector<std::pair<std::string,std::wstring>>{{"numeric",L"Treat predictor as continuous"},{"factor",L"Treat predictor as categorical"}}){auto item=Controls::RadioMenuFlyoutItem();item.GroupName(to_hstring("comparison-cell-type:"+state.id+":"+std::to_string(modelIndex)+":"+term));item.Text(entry.second);item.IsChecked(type==entry.first);item.Click([send,modelIndex,term,nextType=entry.first](auto const&,auto const&)mutable{send("REGCMP_SET_TYPE",std::to_string(modelIndex)+'\x1f'+term+'\x1f'+nextType);});types.Items().Append(item);}edit.Items().Append(types);if(type!="factor"&&type!="ordered"){const bool centered=state.models[modelIndex].centeredPredictors.count(term)>0;auto center=Controls::MenuFlyoutItem();center.Text(centered?L"Remove centering":L"Center predictor");center.IsEnabled(ModelIncludesTerm(state.models[modelIndex],term));center.Click([send,modelIndex,term](auto const&,auto const&)mutable{send("REGCMP_TOGGLE_CENTER",std::to_string(modelIndex)+'\x1f'+term);});edit.Items().Append(center);}}
                if(edit.Items().Size()>0)menu.Items().Append(edit);
                auto diagnostics=Controls::MenuFlyoutSubItem();diagnostics.Text(to_hstring(DefaultModelContextMenuTitles().openDiagnostics));diagnostics.IsEnabled(state.models[modelIndex].fit.ok);
                for(auto const& option:ModelDiagnosticPlotOptions(true)){auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(option.title));item.Click([this,modelIndex,kind=option.value](auto const&,auto const&){if(diagnosticCallback_)diagnosticCallback_((int)modelIndex,kind);});diagnostics.Items().Append(item);}menu.Items().Append(diagnostics);
                auto copy=Controls::MenuFlyoutSubItem();copy.Text(L"Copy");auto copyValue=Controls::MenuFlyoutItem();copyValue.Text(L"Value");copyValue.Click([valueText](auto const&,auto const&){CopyTextToClipboard(valueText.Text());});copy.Items().Append(copyValue);auto copyNamed=Controls::MenuFlyoutItem();copyNamed.Text(L"Predictor and value");copyNamed.Click([valueText,term](auto const&,auto const&){CopyTextToClipboard(to_hstring(term)+L"\t"+valueText.Text());});copy.Items().Append(copyNamed);menu.Items().Append(copy);
                return finishMenu(menu);
            };
            auto replacementMenu=[&](std::string const& source,std::size_t modelIndex)
            {
                auto menu=Controls::MenuFlyout();
                for(auto const& candidate:availableVariables_)
                {
                    if(candidate==state.response||candidate==source||
                       ModelIncludesTerm(state.models[modelIndex],candidate))continue;
                    auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(candidate));
                    item.IsEnabled(!state.precomputed);
                    item.Click([send,modelIndex,source,candidate](auto const&,auto const&)mutable{
                        send("REGCMP_REPLACE_TERM",std::to_string(modelIndex)+'\x1f'+source+'\x1f'+candidate);
                    });
                    menu.Items().Append(item);
                }
                if(menu.Items().Size()==0)
                {
                    auto empty=Controls::MenuFlyoutItem();empty.Text(L"No replacement variables available");empty.IsEnabled(false);menu.Items().Append(empty);
                }
                return menu;
            };
            addRow();auto corner=TableCell(L"",true,true);add(corner,0,0);
            for(std::size_t i=0;i<state.models.size();++i)
            {
                auto cell=TableCell(to_hstring(state.models[i].label).c_str(),true,false);
                if((int)i==activeModel)cell.Background(Brush(242,247,252));
                cell.Tapped([send,i](auto const&,Input::TappedRoutedEventArgs const& args)mutable{send("REGCMP_SET_ACTIVE",std::to_string(i));args.Handled(true);});
                add(cell,0,(int)i+1);AttachContextOnlyMenu(cell,modelMenu(i));
            }
            auto addModelCell=TableCell(L"+ Add model",true,false);add(addModelCell,0,(int)state.models.size()+1);auto addModelMenu=Controls::MenuFlyout();auto emptyModel=Controls::MenuFlyoutItem();emptyModel.Text(L"New empty model");emptyModel.IsEnabled(!state.precomputed);emptyModel.Click([send](auto const&,auto const&)mutable{send("REGCMP_ADD_EMPTY_MODEL","");});addModelMenu.Items().Append(emptyModel);if(!state.models.empty())addModelMenu.Items().Append(Controls::MenuFlyoutSeparator());for(std::size_t i=0;i<state.models.size();++i){auto item=Controls::MenuFlyoutItem();item.Text(to_hstring("Duplicate "+state.models[i].label));item.IsEnabled(!state.precomputed);item.Click([send,i](auto const&,auto const&)mutable{send("REGCMP_DUPLICATE_MODEL",std::to_string(i));});addModelMenu.Items().Append(item);}addModelCell.Tapped([send,canAdd=!state.precomputed](auto const&,Input::TappedRoutedEventArgs const& args)mutable{if(canAdd)send("REGCMP_ADD_MODEL","");args.Handled(true);});AttachContextOnlyMenu(addModelCell,finishMenu(addModelMenu));
            int r=1;
            for(auto const& row:rows)
            {
                const bool termRow=(std::size_t)r<=termRowCount;
                const bool addTermRow=(std::size_t)r==termRowCount+1;
                const std::string rawTerm=termRow&&r>0&&((std::size_t)r-1)<state.termRows.size()?state.termRows[(std::size_t)r-1]:row.first;
                addRow();auto labelCell=TableCell(to_hstring(row.first).c_str(),false,true,false,false,false,r>1);add(labelCell,r,0);auto labelText=labelCell.Child().as<Controls::TextBlock>();
                if(addTermRow)
                {
                    auto menu=Controls::MenuFlyout();
                    if(!state.models.empty())
                    {
                        for(auto const& variable:availableVariables_){if(variable==state.response||ModelIncludesTerm(state.models[(std::size_t)activeModel],variable))continue;auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(variable));item.IsEnabled(!state.precomputed);item.Click([send,activeModel,variable](auto const&,auto const&)mutable{send("REGCMP_ADD_TERM_MODEL",std::to_string(activeModel)+'\x1f'+variable);});menu.Items().Append(item);}
                        std::vector<std::vector<std::string>> comparisonTerms;for(auto const& model:state.models)comparisonTerms.push_back(model.terms);
                        AppendPolynomialMenu(menu, PolynomialNumericVariables(availableVariables_, state.models[(std::size_t)activeModel], &state.seed), state.response, state.models[(std::size_t)activeModel].terms,
                            [send,activeModel](std::string const& value) mutable { send("REGCMP_ADD_INTERACTION", std::to_string(activeModel)+'\x1f'+value); });
                        const auto interactions=InteractionCandidateTermsForComparison(availableVariables_,state.response,comparisonTerms,state.models[(std::size_t)activeModel].terms);
                        if(!interactions.empty())
                        {
                            if(menu.Items().Size()>0)menu.Items().Append(Controls::MenuFlyoutSeparator());
                            auto submenu=BuildInteractionMenuSubItem(interactions,[send,activeModel](std::string const& interaction)mutable{send("REGCMP_ADD_INTERACTION",std::to_string(activeModel)+'\x1f'+interaction);},!state.precomputed);if(submenu)menu.Items().Append(submenu);
                        }
                    }
                    AttachDirectMenu(labelCell,finishMenu(menu));
                }
                else if(termRow&&rawTerm!="(Intercept)")
                {
                    const std::string term=RegressionComparisonSourceTerm(state,rawTerm);auto menu=Controls::MenuFlyout();auto remove=Controls::MenuFlyoutItem();remove.Text(term.find(':')==std::string::npos?L"Remove predictor from comparison":L"Remove interaction from comparison");remove.IsEnabled(!state.precomputed);remove.Click([send,term](auto const&,auto const&)mutable{send("REGCMP_REMOVE_TERM",term);});menu.Items().Append(remove);menu.Items().Append(Controls::MenuFlyoutSeparator());auto title=Controls::MenuFlyoutItem();title.Text(to_hstring(term));title.IsEnabled(false);menu.Items().Append(title);menu.Items().Append(Controls::MenuFlyoutSeparator());auto models=Controls::MenuFlyoutSubItem();models.Text(L"Models");models.IsEnabled(!state.precomputed);for(std::size_t i=0;i<state.models.size();++i){auto item=Controls::ToggleMenuFlyoutItem();item.Text(to_hstring(state.models[i].label));item.IsChecked(ModelIncludesTerm(state.models[i],term));item.Click([send,i,term](auto const&,auto const&)mutable{send("REGCMP_TOGGLE_TERM",std::to_string(i)+'\x1f'+term);});models.Items().Append(item);}menu.Items().Append(models);
                    const bool replaceOnPrimary=rawTerm==term&&term.find(':')==std::string::npos&&term.find('=')==std::string::npos&&!state.models.empty();
                    if(replaceOnPrimary)AttachPrimaryAndContextMenus(labelCell,replacementMenu(term,(std::size_t)activeModel),finishMenu(menu));
                    else AttachDirectMenu(labelCell,finishMenu(menu));
                }
                else AttachDirectMenu(labelCell,CreateStructuralCellMenu(to_hstring(row.first).c_str(),labelText));
                for(std::size_t i=0;i<state.models.size();++i)
                {
                    const std::string value=i<row.second.size()?row.second[i]:std::string{};
                    auto valueCell=TableCell(to_hstring(value).c_str(),false,false,false,false,false,r>1);
                    auto valueText=valueCell.Child().as<Controls::TextBlock>();
                    const std::string sourceTerm=termRow?RegressionComparisonSourceTerm(state,rawTerm):rawTerm;
                    const auto termCell=termRow
                        ? RegressionComparisonTermCell(state,static_cast<int>(i),rawTerm)
                        : RegressionComparisonTermCellState{};
                    const bool directTerm=termRow&&termCell.inclusionControlAvailable;
                    if(directTerm)
                    {
                        const bool included=termCell.termIncluded;
                        auto panel=Controls::Grid();
                        auto glyphColumn=Controls::ColumnDefinition();glyphColumn.Width(GridLengthHelper::FromPixels(30));panel.ColumnDefinitions().Append(glyphColumn);
                        auto valueColumn=Controls::ColumnDefinition();valueColumn.Width(GridLengthHelper::FromValueAndType(1,GridUnitType::Star));panel.ColumnDefinitions().Append(valueColumn);
                        auto toggle=Controls::Border();toggle.Width(26);toggle.Height(22);toggle.CornerRadius(CornerRadius{3});toggle.Background(Brush(248,249,251));toggle.BorderBrush(Brush(221,225,230));toggle.BorderThickness(Thickness{1});toggle.IsHitTestVisible(!state.precomputed);
                        auto glyph=Controls::TextBlock();glyph.Text(included?L"\u2212":L"+");glyph.FontSize(14);glyph.HorizontalAlignment(HorizontalAlignment::Center);glyph.VerticalAlignment(VerticalAlignment::Center);toggle.Child(glyph);
                        toggle.PointerEntered([toggle](auto const&,auto const&){toggle.Background(Brush(235,239,244));});
                        toggle.PointerExited([toggle](auto const&,auto const&){toggle.Background(Brush(248,249,251));});
                        toggle.Tapped([send,i,sourceTerm](auto const&,Input::TappedRoutedEventArgs const& args)mutable{send("REGCMP_TOGGLE_TERM",std::to_string(i)+'\x1f'+sourceTerm);args.Handled(true);});
                        panel.Children().Append(toggle);
                        valueCell.Child(nullptr);valueText.Text(to_hstring(value));valueText.HorizontalAlignment(HorizontalAlignment::Right);Controls::Grid::SetColumn(valueText,1);panel.Children().Append(valueText);
                        valueCell.Child(panel);
                    }
                    add(valueCell,r,(int)i+1);
                    if(termRow&&rawTerm!="(Intercept)")
                    {
                        const bool included=termCell.termIncluded;
                        valueCell.Tapped([send,i,sourceTerm,included](auto const&,Input::TappedRoutedEventArgs const& args)mutable{send("REGCMP_SET_ACTIVE",std::to_string(i));send(included?"REGCMP_SHOW_DETAILS":"REGCMP_TOGGLE_TERM",std::to_string(i)+'\x1f'+sourceTerm);args.Handled(true);});
                        AttachContextOnlyMenu(valueCell,termMenu(sourceTerm,i,valueText));
                    }
                    else
                    {
                        valueCell.Tapped([send,i](auto const&,Input::TappedRoutedEventArgs const& args)mutable{send("REGCMP_SET_ACTIVE",std::to_string(i));args.Handled(true);});
                        AttachContextOnlyMenu(valueCell,CreateStatisticCellMenu(
                            to_hstring(row.first).c_str(),valueText,
                            RegressionComparisonStatisticContext(state,i,row.first)));
                    }
                }
                ++r;
            }
            content_.Children().Append(grid);
        };
        std::vector<std::pair<std::string,std::vector<std::string>>> matrixRows;
        for(auto const& rawTerm:state.termRows){std::vector<std::string> values;for(std::size_t i=0;i<state.models.size();++i)values.push_back(RegressionComparisonTermCell(state,static_cast<int>(i),rawTerm).displayText);matrixRows.push_back({RegressionComparisonDisplayLabel(state,rawTerm),values});}
        const std::size_t termRowCount=matrixRows.size();matrixRows.push_back({RegressionComparisonAddTermLabel(),std::vector<std::string>(state.models.size(),"")});
        for(int fitRow:RegressionComparisonVisibleFitRows(state.showInformationCriteria)){std::vector<std::string> values;for(std::size_t i=0;i<state.models.size();++i)values.push_back(RegressionComparisonFitCell(state,static_cast<int>(i),fitRow).displayText);matrixRows.push_back({RegressionComparisonFitLabel(fitRow,state.multipleImputation),values});}
        build(matrixRows,termRowCount);const std::string noteText=state.note.empty()?RegressionComparisonFootnote():state.note+"  "+RegressionComparisonFootnote();auto note=FieldLabel(to_hstring(noteText).c_str());note.FontSize(11);note.Foreground(Brush(92,92,92));note.TextWrapping(TextWrapping::Wrap);content_.Children().Append(note);
    }
    void RegressionComparisonView::Activate(){if(!closed_)window_.Activate();}void RegressionComparisonView::Close(){if(!closed_)window_.Close();}

    std::shared_ptr<ModelTrellisView> ModelTrellisView::Create(){auto view=std::shared_ptr<ModelTrellisView>(new ModelTrellisView());view->AttachLifetime();return view;}
    ModelTrellisView::ModelTrellisView(){Initialize();}
    void ModelTrellisView::Initialize()
    {
        window_=CreateLinkEDAWindow();auto root=Controls::Grid();root.Padding(Thickness{16,14,16,11});root.RowSpacing(7);root.Background(Brush(255,255,255));root.RequestedTheme(ElementTheme::Light);
        auto autoRow=[](){auto row=Controls::RowDefinition();row.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Auto));return row;};
        root.RowDefinitions().Append(autoRow());root.RowDefinitions().Append(autoRow());auto body=Controls::RowDefinition();body.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Star));root.RowDefinitions().Append(body);root.RowDefinitions().Append(autoRow());
        auto heading=Controls::Grid();auto grow=Controls::ColumnDefinition();grow.Width(GridLengthHelper::FromValueAndType(1,GridUnitType::Star));heading.ColumnDefinitions().Append(grow);heading.ColumnDefinitions().Append(Controls::ColumnDefinition());
        title_=Controls::TextBlock();title_.Text(L"Linear Model Trellis");title_.FontSize(17);title_.FontWeight(Windows::UI::Text::FontWeights::SemiBold());heading.Children().Append(title_);
        badge_=Controls::TextBlock();badge_.FontSize(12);badge_.Foreground(Brush(72,72,72));Controls::Grid::SetColumn(badge_,1);heading.Children().Append(badge_);root.Children().Append(heading);
        subtitle_=Controls::TextBlock();subtitle_.FontSize(11.5);subtitle_.Foreground(Brush(78,82,88));subtitle_.TextWrapping(TextWrapping::Wrap);Controls::Grid::SetRow(subtitle_,1);root.Children().Append(subtitle_);
        auto scroll=Controls::ScrollViewer();scroll.HorizontalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);scroll.VerticalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);panels_=Controls::Grid();panels_.Name(L"LinkEDA.SnapshotTable");panels_.ColumnSpacing(12);panels_.RowSpacing(12);scroll.Content(panels_);Controls::Grid::SetRow(scroll,2);root.Children().Append(scroll);
        status_=Controls::TextBlock();status_.FontSize(11);status_.Foreground(Brush(92,92,92));status_.TextWrapping(TextWrapping::Wrap);Controls::Grid::SetRow(status_,3);root.Children().Append(status_);exporter_=TableExportService::Create(window_,root);window_.Content(root);ResizeLogical(window_,980,650);
    }
    void ModelTrellisView::AttachLifetime(){std::weak_ptr<ModelTrellisView> weak=shared_from_this();window_.Closed([weak](auto const&,auto const&){if(auto view=weak.lock()){view->closed_=true;if(view->closedCallback_)view->closedCallback_();}});}void ModelTrellisView::SetClosedCallback(Closed callback){closedCallback_=std::move(callback);}
    void ModelTrellisView::SetCommandCallback(Command callback,std::vector<std::string> predictors,std::vector<std::string> conditioningVariables)
    {commandCallback_=std::move(callback);predictors_=std::move(predictors);conditioningVariables_=std::move(conditioningVariables);ConfigureContextMenu();}
    void ModelTrellisView::ConfigureContextMenu()
    {
        using namespace ::rlispstat::core;auto menu=Controls::MenuFlyout();auto send=[this](std::string const& name,std::string const& value=std::string{}){if(commandCallback_)commandCallback_({name,state_.id,value});};
        auto content=Controls::MenuFlyoutSubItem();content.Text(L"Panel content");
        for(auto const& entry:{std::tuple<std::wstring,std::string,ModelTrellisPanelContent>{L"Compact summary","compact",ModelTrellisPanelContent::CompactSummary},{L"Selected result","selected",ModelTrellisPanelContent::SelectedResult},{L"Coefficients","coefficients",ModelTrellisPanelContent::Coefficients},{L"Term tests","term_tests",ModelTrellisPanelContent::TermTests},{L"Model fit","model_fit",ModelTrellisPanelContent::ModelFit}}){auto item=Controls::ToggleMenuFlyoutItem();const auto selected=std::get<2>(entry)==state_.specification.panelContent;item.Text(std::get<0>(entry));item.IsChecked(selected);const auto value=std::get<1>(entry);item.Click([send,value](auto const&,auto const&)mutable{send("MODEL_TRELLIS_SET_CONTENT",value);});content.Items().Append(item);}menu.Items().Append(content);
        auto terms=Controls::MenuFlyoutSubItem();terms.Text(L"Model terms");auto add=Controls::MenuFlyoutSubItem();add.Text(L"Add predictor");for(auto const& value:predictors_){auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(value));item.Click([send,value](auto const&,auto const&)mutable{send("MODEL_TRELLIS_ADD_TERM",value);});add.Items().Append(item);}terms.Items().Append(add);auto remove=Controls::MenuFlyoutSubItem();remove.Text(L"Remove predictor");for(auto const& value:state_.specification.baseModel.terms){auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(value));item.Click([send,value](auto const&,auto const&)mutable{send("MODEL_TRELLIS_REMOVE_TERM",value);});remove.Items().Append(item);}terms.Items().Append(remove);menu.Items().Append(terms);
        auto displayed=Controls::MenuFlyoutSubItem();displayed.Text(L"Displayed result");auto fit=Controls::ToggleMenuFlyoutItem();fit.Text(L"Model fit");fit.IsChecked(!state_.specification.displayedTermId&&!state_.specification.displayedCoefficientId);fit.Click([send](auto const&,auto const&)mutable{send("MODEL_TRELLIS_SET_RESULT","fit|");});displayed.Items().Append(fit);for(auto const& value:state_.specification.baseModel.terms){auto item=Controls::ToggleMenuFlyoutItem();item.Text(to_hstring(value));item.IsChecked(state_.specification.displayedTermId&&*state_.specification.displayedTermId==value);item.Click([send,value](auto const&,auto const&)mutable{send("MODEL_TRELLIS_SET_RESULT","term|"+value);});displayed.Items().Append(item);}std::set<std::string> seen;auto coefficients=Controls::MenuFlyoutSubItem();coefficients.Text(L"Coefficients");for(auto const& panel:state_.panels)for(auto const& row:panel.coefficients)if(seen.insert(row.coefficientId).second){auto item=Controls::ToggleMenuFlyoutItem();item.Text(to_hstring(row.label));item.IsChecked(state_.specification.displayedCoefficientId&&*state_.specification.displayedCoefficientId==row.coefficientId);const auto value=row.coefficientId;item.Click([send,value](auto const&,auto const&)mutable{send("MODEL_TRELLIS_SET_RESULT","coef|"+value);});coefficients.Items().Append(item);}displayed.Items().Append(coefficients);menu.Items().Append(displayed);
        auto conditioning=Controls::MenuFlyoutSubItem();conditioning.Text(L"Conditioning variables");auto addCondition=[&](std::wstring const& title,std::string const& dimension,std::optional<ModelTrellisConditioningVariable> const& current,std::optional<ModelTrellisConditioningVariable> const& other){auto submenu=Controls::MenuFlyoutSubItem();submenu.Text(title);auto none=Controls::ToggleMenuFlyoutItem();none.Text(L"None");none.IsChecked(!current);none.Click([send,dimension](auto const&,auto const&)mutable{send("MODEL_TRELLIS_SET_CONDITION",dimension+"|");});submenu.Items().Append(none);for(auto const& value:conditioningVariables_){auto item=Controls::ToggleMenuFlyoutItem();item.Text(to_hstring(value));item.IsChecked(current&&current->variableId==value);item.IsEnabled(!(other&&other->variableId==value));item.Click([send,dimension,value](auto const&,auto const&)mutable{send("MODEL_TRELLIS_SET_CONDITION",dimension+"|"+value);});submenu.Items().Append(item);}conditioning.Items().Append(submenu);};addCondition(L"Rows","rows",state_.specification.rowConditioningVariable,state_.specification.columnConditioningVariable);addCondition(L"Columns","columns",state_.specification.columnConditioningVariable,state_.specification.rowConditioningVariable);auto swap=Controls::MenuFlyoutItem();swap.Text(L"Swap rows and columns");swap.Click([send](auto const&,auto const&)mutable{send("MODEL_TRELLIS_SWAP","");});conditioning.Items().Append(swap);menu.Items().Append(conditioning);
        if((bool)state_.specification.rowConditioningVariable!=(bool)state_.specification.columnConditioningVariable){auto arrangement=Controls::MenuFlyoutSubItem();arrangement.Text(L"Panel arrangement");for(auto const& entry:{std::tuple<std::wstring,std::string,ModelTrellisPanelArrangement>{L"Automatic","automatic",ModelTrellisPanelArrangement::Automatic},{L"One row","one_row",ModelTrellisPanelArrangement::OneRow},{L"One column","one_column",ModelTrellisPanelArrangement::OneColumn}}){auto item=Controls::ToggleMenuFlyoutItem();item.Text(std::get<0>(entry));item.IsChecked(state_.specification.arrangement==std::get<2>(entry));const auto value=std::get<1>(entry);item.Click([send,value](auto const&,auto const&)mutable{send("MODEL_TRELLIS_SET_ARRANGEMENT",value);});arrangement.Items().Append(item);}menu.Items().Append(arrangement);}
        auto adjustment=Controls::MenuFlyoutSubItem();adjustment.Text(L"P-value adjustment");for(auto const& entry:{std::tuple<std::wstring,std::string,ModelTrellisPAdjustment>{L"None","none",ModelTrellisPAdjustment::None},{L"Holm","holm",ModelTrellisPAdjustment::Holm},{L"Bonferroni","bonferroni",ModelTrellisPAdjustment::Bonferroni}}){auto item=Controls::ToggleMenuFlyoutItem();item.Text(std::get<0>(entry));item.IsChecked(state_.specification.pAdjustment==std::get<2>(entry));const auto value=std::get<1>(entry);item.Click([send,value](auto const&,auto const&)mutable{send("MODEL_TRELLIS_SET_ADJUSTMENT",value);});adjustment.Items().Append(item);}menu.Items().Append(adjustment);auto refit=Controls::MenuFlyoutItem();refit.Text(L"Refit panel models");refit.Click([send](auto const&,auto const&)mutable{send("MODEL_TRELLIS_REFIT","");});menu.Items().Append(refit);
        if(exporter_){menu.Items().Append(Controls::MenuFlyoutSeparator());auto exportMenu=exporter_->CreateMenu([this](){return ModelTrellisExportPayload(state_);},true);AppendRCodeExportItems(exportMenu,commandCallback_,state_.id);menu.Items().Append(exportMenu);}AttachWindowContextFlyout(window_, menu);
    }
    void ModelTrellisView::Show(::rlispstat::core::ModelTrellisState const& state)
    {
        state_=state;SetWindowDataSheetGroup(window_,state.specification.baseModel.group);SetWindowSnapshotSource(window_,"output",state.id,state.specification.baseModel.group);window_.Title(L"Linear Model Trellis");badge_.Text(to_hstring(state.specification.baseModel.group));
        subtitle_.Text(to_hstring(::rlispstat::core::ModelTrellisFormulaText(state.specification.baseModel)+"     Scope: "+state.specification.baseModel.scope+"     Adjustment: "+::rlispstat::core::ModelTrellisPAdjustmentLabel(state.specification.pAdjustment)));
        status_.Text(to_hstring(state.status.empty()?::rlispstat::core::ModelTrellisMethodologicalNote():state.status));Render(state);ConfigureContextMenu();const auto layout=::rlispstat::core::BuildModelTrellisLayout(state);PresentWindowOnce(window_,presented_,std::clamp(layout.columns*312.0+48.0,720.0,1320.0),std::clamp(layout.rows*247.0+142.0,390.0,900.0));
    }
    void ModelTrellisView::Render(::rlispstat::core::ModelTrellisState const& state)
    {
        using namespace ::rlispstat::core;
        panels_.Children().Clear();panels_.RowDefinitions().Clear();panels_.ColumnDefinitions().Clear();const auto layout=::rlispstat::core::BuildModelTrellisLayout(state);
        for(std::size_t c=0;c<layout.columns;++c){auto column=Controls::ColumnDefinition();column.Width(GridLengthHelper::FromPixels(300));panels_.ColumnDefinitions().Append(column);}for(std::size_t r=0;r<layout.rows;++r){auto row=Controls::RowDefinition();row.Height(GridLengthHelper::FromPixels(235));panels_.RowDefinitions().Append(row);}
        for(auto const& placement:layout.panels){auto found=std::find_if(state.panels.begin(),state.panels.end(),[&](auto const& panel){return panel.panelId==placement.panelId;});if(found==state.panels.end())continue;auto const& panel=*found;
            auto border=Controls::Border();border.BorderBrush(Brush(218,221,225));border.BorderThickness(Thickness{1});border.Padding(Thickness{12,10,12,9});auto stack=Controls::StackPanel();stack.Spacing(5);
            std::string label;if(!panel.rowLevelLabel.empty())label+=panel.rowLevelLabel;if(!panel.columnLevelLabel.empty()){if(!label.empty())label+=" · ";label+=panel.columnLevelLabel;}if(label.empty())label="All observations";
            auto heading=FieldLabel(to_hstring(label).c_str());heading.FontSize(13);heading.FontWeight(Windows::UI::Text::FontWeights::SemiBold());stack.Children().Append(heading);
            if(!panel.estimable){auto warning=FieldLabel(to_hstring(panel.warnings.empty()?"Model not estimable":panel.warnings.front()).c_str());warning.FontSize(11);warning.Foreground(Brush(145,54,45));warning.TextWrapping(TextWrapping::Wrap);stack.Children().Append(warning);}else{
                auto fit=Controls::Grid();for(double width:{112.0,70.0,70.0}){auto column=Controls::ColumnDefinition();column.Width(GridLengthHelper::FromPixels(width));fit.ColumnDefinitions().Append(column);}std::vector<std::vector<std::string>> rows={{"","Value","Adjusted"},{"N",std::to_string(panel.modelResult.n),"—"},{"R²",FormatDouble(panel.modelResult.r2,3),FormatDouble(panel.modelResult.adjR2,3)},{"F",FormatDouble(panel.modelResult.globalF,3),FormatPValue(panel.adjustedGlobalPValue)}};for(std::size_t r=0;r<rows.size();++r){auto def=Controls::RowDefinition();def.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Auto));fit.RowDefinitions().Append(def);for(std::size_t c=0;c<rows[r].size();++c){auto cell=TableCell(to_hstring(rows[r][c]).c_str(),r==0,c==0,false,false,false,r>1);Controls::Grid::SetRow(cell,(int)r);Controls::Grid::SetColumn(cell,(int)c);fit.Children().Append(cell);auto text=cell.Child().as<Controls::TextBlock>();if(c==0||r==0)AttachDirectMenu(cell,CreateStructuralCellMenu(to_hstring(rows[r][c].empty()?(c<rows[0].size()?rows[0][c]:std::string{}):rows[r][c]).c_str(),text));else AttachDirectMenu(cell,CreateStatisticCellMenu(to_hstring(rows[r][0]+" "+rows[0][c]).c_str(),text));}}stack.Children().Append(fit);
                if(!panel.coefficients.empty()){auto section=FieldLabel(L"Coefficients");section.FontSize(11.5);section.FontWeight(Windows::UI::Text::FontWeights::SemiBold());stack.Children().Append(section);std::size_t shown=0;for(auto const& coefficient:panel.coefficients){if(!coefficient.estimable)continue;auto line=FieldLabel(to_hstring(coefficient.label+"   b = "+FormatDouble(coefficient.estimate,3)+"   p "+FormatPValue(coefficient.adjustedPValue)).c_str());line.FontSize(10.5);stack.Children().Append(line);if(++shown==3)break;}}
            }border.Child(stack);Controls::Grid::SetRow(border,(int)placement.row);Controls::Grid::SetColumn(border,(int)placement.column);panels_.Children().Append(border);}
        panels_.Width(std::max<std::size_t>(1,layout.columns)*312.0);panels_.Height(std::max<std::size_t>(1,layout.rows)*247.0);
    }
    void ModelTrellisView::Activate(){if(!closed_)window_.Activate();}void ModelTrellisView::Close(){if(!closed_)window_.Close();}

    std::shared_ptr<GeneralizedComparisonView> GeneralizedComparisonView::Create(){auto view=std::shared_ptr<GeneralizedComparisonView>(new GeneralizedComparisonView());view->AttachLifetime();return view;}
    GeneralizedComparisonView::GeneralizedComparisonView(){Initialize();}
    void GeneralizedComparisonView::Initialize(){window_=CreateLinkEDAWindow();auto root=Controls::Grid();root.Padding(Thickness{18,14,18,12});root.RowSpacing(6);root.Background(Brush(255,255,255));root.RequestedTheme(ElementTheme::Light);auto autoRow=[](){auto row=Controls::RowDefinition();row.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Auto));return row;};root.RowDefinitions().Append(autoRow());root.RowDefinitions().Append(autoRow());auto body=Controls::RowDefinition();body.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Star));root.RowDefinitions().Append(body);auto heading=Controls::Grid();auto grow=Controls::ColumnDefinition();grow.Width(GridLengthHelper::FromValueAndType(1,GridUnitType::Star));heading.ColumnDefinitions().Append(grow);heading.ColumnDefinitions().Append(Controls::ColumnDefinition());title_=Controls::TextBlock();title_.FontSize(17);title_.FontWeight(Windows::UI::Text::FontWeights::SemiBold());heading.Children().Append(title_);badge_=Controls::TextBlock();badge_.FontSize(12);badge_.Foreground(Brush(72,72,72));Controls::Grid::SetColumn(badge_,1);heading.Children().Append(badge_);root.Children().Append(heading);subtitle_=Controls::TextBlock();subtitle_.FontSize(11.5);subtitle_.Foreground(Brush(78,82,88));controls_=Controls::StackPanel();controls_.Orientation(Controls::Orientation::Horizontal);controls_.Spacing(8);controls_.Margin(Thickness{0,4,0,7});Controls::Grid::SetRow(controls_,1);root.Children().Append(controls_);auto scroll=Controls::ScrollViewer();scroll.HorizontalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);scroll.VerticalScrollBarVisibility(Controls::ScrollBarVisibility::Auto);content_=Controls::StackPanel();content_.Name(L"LinkEDA.SnapshotTable");content_.Spacing(9);scroll.Content(content_);Controls::Grid::SetRow(scroll,2);root.Children().Append(scroll);exporter_=TableExportService::Create(window_,root);window_.Content(root);ResizeLogical(window_,980,560);}
    void GeneralizedComparisonView::AttachLifetime(){std::weak_ptr<GeneralizedComparisonView> weak=shared_from_this();window_.Closed([weak](auto const&,auto const&){if(auto view=weak.lock()){view->closed_=true;if(view->closedCallback_)view->closedCallback_();}});}void GeneralizedComparisonView::SetClosedCallback(Closed callback){closedCallback_=std::move(callback);}
    void GeneralizedComparisonView::ShowMessage(std::string const& message){if(!closed_)ShowWorkflowMessage(window_,L"Interaction unavailable",message);}
    void GeneralizedComparisonView::SetCommandCallback(Command callback, Diagnostic diagnostic,std::vector<std::string> responseVariables,std::vector<std::string> availableVariables,std::vector<std::string> exposureVariables){commandCallback_=std::move(callback);diagnosticCallback_=std::move(diagnostic);commandChoicesChanged_=responseVariables_!=responseVariables||availableVariables_!=availableVariables||exposureVariables_!=exposureVariables;responseVariables_=std::move(responseVariables);availableVariables_=std::move(availableVariables);exposureVariables_=std::move(exposureVariables);}
    void GeneralizedComparisonView::SetScopeChoices(std::vector<::rlispstat::core::AnalysisScopeChoice> choices){scopeChoices_=std::move(choices);commandChoicesChanged_=true;RebuildControls();}
#if 0
    void GeneralizedComparisonView::ConfigureContextMenu()
    {
        auto menu=Controls::MenuFlyout();auto send=[this](std::string const& name,std::string const& value=std::string{}){if(commandCallback_)commandCallback_({name,state_.id,value});};
        auto terms=Controls::MenuFlyoutSubItem();terms.Text(L"Independent variables");const int active=state_.models.empty()?0:std::clamp(state_.activeModel,0,(int)state_.models.size()-1);for(auto const& variable:availableVariables_){if(variable==state_.response)continue;auto item=Controls::ToggleMenuFlyoutItem();const bool included=!state_.models.empty()&&::rlispstat::core::GeneralizedModelIncludesTerm(state_.models[(size_t)active],variable);item.Text(to_hstring(variable));item.IsChecked(included);item.Click([send,variable,included,active](auto const&,auto const&)mutable{if(included)send("GCOMP_TOGGLE_TERM",std::to_string(active)+'\x1f'+variable);else send("GCOMP_ADD_TERM",variable);});terms.Items().Append(item);}menu.Items().Append(terms);
        auto models=Controls::MenuFlyoutSubItem();models.Text(L"Models");auto add=Controls::MenuFlyoutItem();add.Text(L"Add model");add.Click([send](auto const&,auto const&)mutable{send("GCOMP_ADD_MODEL");});models.Items().Append(add);for(std::size_t i=0;i<state_.models.size();++i){auto model=Controls::MenuFlyoutItem();model.Text((i==(size_t)active?L"✓ ":L"")+to_hstring(state_.models[i].label));model.Click([send,i](auto const&,auto const&)mutable{send("GCOMP_SET_ACTIVE",std::to_string(i));});models.Items().Append(model);}auto remove=Controls::MenuFlyoutItem();remove.Text(L"Delete active model");remove.IsEnabled(state_.models.size()>2);remove.Click([send,active](auto const&,auto const&)mutable{send("GCOMP_DELETE_MODEL",std::to_string(active));});models.Items().Append(remove);menu.Items().Append(models);
        auto info=Controls::ToggleMenuFlyoutItem();info.Text(L"Information criteria");info.IsChecked(state_.showInformationCriteria);info.Click([send](auto const&,auto const&)mutable{send("GCOMP_TOGGLE_INFO");});menu.Items().Append(info);auto refit=Controls::MenuFlyoutItem();refit.Text(L"Refit all models");refit.Click([send](auto const&,auto const&)mutable{send("GCOMP_REFIT");});menu.Items().Append(refit);AttachWindowContextFlyout(window_,menu);
    }
#endif
    void GeneralizedComparisonView::ConfigureContextMenu()
    {
        auto menu=Controls::MenuFlyout();
        AppendMissingInformationMenu(menu,commandCallback_,state_.id,!state_.provenance.missingInformationRows.empty());
        auto send=[this](std::string const& name,std::string const& value=std::string{})
        {if(commandCallback_)commandCallback_({name,state_.id,value});};
        auto finishWindowMenu=[this,&menu]()
        {
            if(exporter_)
            {
                menu.Items().Append(Controls::MenuFlyoutSeparator());
                auto exportMenu=exporter_->CreateMenu(
                    [this](){return GeneralizedComparisonExportPayload(state_);},true);
                AppendRCodeExportItems(exportMenu,commandCallback_,state_.id);
                menu.Items().Append(exportMenu);
            }
            AppendTableAnnotationContextItems(window_,menu);
            AttachWindowContextFlyout(window_,menu);
        };
        auto response=Controls::MenuFlyoutSubItem();response.Text(L"Response variable");
        for(auto const& variable:responseVariables_){auto item=Controls::ToggleMenuFlyoutItem();item.Text(to_hstring(variable));item.IsChecked(variable==state_.response);item.Click([send,variable](auto const&,auto const&)mutable{send("GCOMP_SET_RESPONSE",variable);});response.Items().Append(item);}menu.Items().Append(response);
        if(!state_.binaryComparison&&!state_.countComparison)
        {
            auto family=Controls::MenuFlyoutSubItem();family.Text(L"Distribution");
            for(auto const& value : ::rlispstat::core::DistributionSpecificationsForModel(state_.modelType)){auto item=Controls::ToggleMenuFlyoutItem();item.Text(to_hstring(value.visibleName));item.IsChecked(value.id==state_.family);item.Click([send,id=value.id](auto const&,auto const&)mutable{send("GCOMP_SET_FAMILY",id);});family.Items().Append(item);}menu.Items().Append(family);
            auto links=Controls::MenuFlyoutSubItem();links.Text(L"Link");
            for(auto const& value : ::rlispstat::core::LinksForModelDistribution(state_.modelType,state_.family)){auto item=Controls::ToggleMenuFlyoutItem();item.Text(to_hstring(value));item.IsChecked(value==state_.link);item.Click([send,value](auto const&,auto const&)mutable{send("GCOMP_SET_LINK",value);});links.Items().Append(item);}menu.Items().Append(links);

            auto automatic=Controls::ToggleMenuFlyoutItem();automatic.Text(L"Auto-refit");automatic.IsChecked(state_.autoRefit);automatic.Click([send](auto const&,auto const&)mutable{send("GCOMP_TOGGLE_AUTO");});menu.Items().Append(automatic);
            auto info=Controls::ToggleMenuFlyoutItem();info.Text(L"Information criteria");info.IsChecked(state_.showInformationCriteria);info.Click([send](auto const&,auto const&)mutable{send("GCOMP_TOGGLE_INFO");});menu.Items().Append(info);
            auto refit=Controls::MenuFlyoutItem();refit.Text(L"Refit all models");refit.Click([send](auto const&,auto const&)mutable{send("GCOMP_REFIT");});menu.Items().Append(refit);
            finishWindowMenu();return;
        }
        if(state_.binaryComparison&&state_.responseCoding.ok){auto event=Controls::MenuFlyoutSubItem();event.Text(L"Modelled event");for(auto const& entry:{std::pair<std::string,std::string>{state_.responseCoding.referenceValue,state_.responseCoding.referenceLabel},{state_.responseCoding.eventValue,state_.responseCoding.eventLabel}}){auto item=Controls::ToggleMenuFlyoutItem();item.Text(to_hstring(entry.second));item.IsChecked(entry.first==state_.responseCoding.eventValue);item.Click([send,value=entry.first](auto const&,auto const&)mutable{send("GCOMP_SET_EVENT",value);});event.Items().Append(item);}menu.Items().Append(event);auto links=Controls::MenuFlyoutSubItem();links.Text(L"Link");for(auto link : ::rlispstat::core::SupportedBinaryLinks()){const auto value=::rlispstat::core::BinaryLinkId(link);auto item=Controls::ToggleMenuFlyoutItem();item.Text(to_hstring(value));item.IsChecked(value==state_.link);item.Click([send,value](auto const&,auto const&)mutable{send("GCOMP_SET_LINK",value);});links.Items().Append(item);}menu.Items().Append(links);}

        auto automatic=Controls::ToggleMenuFlyoutItem();automatic.Text(L"Auto-refit");automatic.IsChecked(state_.autoRefit);automatic.Click([send](auto const&,auto const&)mutable{send("GCOMP_TOGGLE_AUTO");});menu.Items().Append(automatic);
        auto info=Controls::ToggleMenuFlyoutItem();info.Text(L"Information criteria");info.IsChecked(state_.showInformationCriteria);info.Click([send](auto const&,auto const&)mutable{send("GCOMP_TOGGLE_INFO");});menu.Items().Append(info);auto refit=Controls::MenuFlyoutItem();refit.Text(L"Refit all models");refit.Click([send](auto const&,auto const&)mutable{send("GCOMP_REFIT");});menu.Items().Append(refit);finishWindowMenu();
    }
    void GeneralizedComparisonView::RebuildControls()
    {
        controls_.Children().Clear();auto fieldLabel=[](std::wstring const& text){auto result=FieldLabel(text);result.FontSize(11.5);return result;};auto send=[this](std::string const& name,std::string const& value=std::string{}){if(commandCallback_)commandCallback_({name,state_.id,value});};
        controls_.Children().Append(fieldLabel(L"Response:"));auto response=Controls::ComboBox();response.MinWidth(105);response.FontSize(11.5);for(auto const& variable:responseVariables_)response.Items().Append(box_value(to_hstring(variable)));auto responseAt=std::find(responseVariables_.begin(),responseVariables_.end(),state_.response);if(responseAt!=responseVariables_.end())response.SelectedIndex((int)(responseAt-responseVariables_.begin()));response.SelectionChanged([send,response,current=state_.response](auto const&,auto const&)mutable{if(response.SelectedIndex()<0)return;auto value=to_string(response.SelectedItem().as<hstring>());if(value!=current)send("GCOMP_SET_RESPONSE",value);});controls_.Children().Append(response);
        if(!state_.binaryComparison&&!state_.countComparison)
        {
            controls_.Children().Append(fieldLabel(L"Distribution:"));auto family=Controls::ComboBox();family.MinWidth(92);family.FontSize(11.5);std::vector<std::string> families;for(auto const& value : ::rlispstat::core::DistributionSpecificationsForModel(state_.modelType)){families.push_back(value.id);family.Items().Append(box_value(to_hstring(value.visibleName)));}auto familyAt=std::find(families.begin(),families.end(),state_.family);if(familyAt!=families.end())family.SelectedIndex((int)(familyAt-families.begin()));family.SelectionChanged([send,family,families,current=state_.family](auto const&,auto const&)mutable{if(family.SelectedIndex()<0||static_cast<std::size_t>(family.SelectedIndex())>=families.size())return;auto value=families[static_cast<std::size_t>(family.SelectedIndex())];if(value!=current)send("GCOMP_SET_FAMILY",value);});controls_.Children().Append(family);
            controls_.Children().Append(fieldLabel(L"Link:"));auto link=Controls::ComboBox();link.MinWidth(82);link.FontSize(11.5);auto links=::rlispstat::core::LinksForModelDistribution(state_.modelType,state_.family);for(auto const& value:links)link.Items().Append(box_value(to_hstring(value)));auto linkAt=std::find(links.begin(),links.end(),state_.link);if(linkAt!=links.end())link.SelectedIndex((int)(linkAt-links.begin()));link.SelectionChanged([send,link,current=state_.link](auto const&,auto const&)mutable{if(link.SelectedIndex()<0)return;auto value=to_string(link.SelectedItem().as<hstring>());if(value!=current)send("GCOMP_SET_LINK",value);});controls_.Children().Append(link);
            controls_.Children().Append(fieldLabel(L"Scope:"));auto scope=Controls::ComboBox();scope.FontSize(11.5);auto choices=scopeChoices_;if(choices.empty())choices=::rlispstat::core::BuildAnalysisScopeChoices(0,{},true);const auto currentScope=::rlispstat::core::AnalysisScopeChoiceValue(state_.scope,state_.dataScope,state_.dataScopeCaptured);PopulateAnalysisScopeCombo(scope,choices,currentScope);scope.SelectionChanged([send,scope,current=currentScope](auto const&,auto const&)mutable{if(scope.SelectedIndex()<0)return;auto value=SelectedAnalysisScopeChoice(scope);if(value!=current)send("GCOMP_SET_SCOPE",value);});controls_.Children().Append(scope);
            auto autoRefit=Controls::CheckBox();autoRefit.Content(box_value(L"Auto-refit"));autoRefit.IsChecked(state_.autoRefit);autoRefit.FontSize(11.5);autoRefit.Click([send](auto const&,auto const&)mutable{send("GCOMP_TOGGLE_AUTO");});controls_.Children().Append(autoRefit);
            if(!state_.autoRefit){auto refit=Controls::Button();refit.Content(box_value(L"Refit"));refit.FontSize(11.5);refit.Click([send](auto const&,auto const&)mutable{send("GCOMP_REFIT");});controls_.Children().Append(refit);}return;
        }
        if(state_.binaryComparison){controls_.Children().Append(fieldLabel(L"Event category:"));auto event=Controls::ComboBox();event.MinWidth(76);event.FontSize(11.5);event.Items().Append(box_value(to_hstring(state_.responseCoding.referenceLabel)));event.Items().Append(box_value(to_hstring(state_.responseCoding.eventLabel)));event.SelectedIndex(1);event.SelectionChanged([send,event,reference=state_.responseCoding.referenceValue,eventValue=state_.responseCoding.eventValue](auto const&,auto const&)mutable{if(event.SelectedIndex()==0)send("GCOMP_SET_EVENT",reference);else if(event.SelectedIndex()==1)send("GCOMP_SET_EVENT",eventValue);});controls_.Children().Append(event);controls_.Children().Append(fieldLabel(L"Link:"));auto link=Controls::ComboBox();link.FontSize(11.5);std::vector<std::string> links;for(auto supported : ::rlispstat::core::SupportedBinaryLinks())links.push_back(::rlispstat::core::BinaryLinkId(supported));for(auto const& value:links)link.Items().Append(box_value(to_hstring(value)));auto linkAt=std::find(links.begin(),links.end(),state_.link);if(linkAt!=links.end())link.SelectedIndex((int)(linkAt-links.begin()));link.SelectionChanged([send,link,current=state_.link](auto const&,auto const&)mutable{if(link.SelectedIndex()<0)return;auto value=to_string(link.SelectedItem().as<hstring>());if(value!=current)send("GCOMP_SET_LINK",value);});controls_.Children().Append(link);}
        controls_.Children().Append(fieldLabel(L"Scope:"));auto scope=Controls::ComboBox();scope.FontSize(11.5);auto choices=scopeChoices_;if(choices.empty())choices=::rlispstat::core::BuildAnalysisScopeChoices(0,{},true);const auto currentScope=::rlispstat::core::AnalysisScopeChoiceValue(state_.scope,state_.dataScope,state_.dataScopeCaptured);PopulateAnalysisScopeCombo(scope,choices,currentScope);scope.SelectionChanged([send,scope,current=currentScope](auto const&,auto const&)mutable{if(scope.SelectedIndex()<0)return;auto value=SelectedAnalysisScopeChoice(scope);if(value!=current)send("GCOMP_SET_SCOPE",value);});controls_.Children().Append(scope);auto autoRefit=Controls::CheckBox();autoRefit.Content(box_value(L"Auto-refit"));autoRefit.IsChecked(state_.autoRefit);autoRefit.FontSize(11.5);autoRefit.Click([send](auto const&,auto const&)mutable{send("GCOMP_TOGGLE_AUTO");});controls_.Children().Append(autoRefit);
        if(!state_.autoRefit){auto refit=Controls::Button();refit.Content(box_value(L"Refit"));refit.FontSize(11.5);refit.Click([send](auto const&,auto const&)mutable{send("GCOMP_REFIT");});controls_.Children().Append(refit);}
    }
    void GeneralizedComparisonView::Show(::rlispstat::core::GeneralizedComparisonState const& state)
    {
        ::rlispstat::windows::performance::Scope timing("Output.GeneralizedComparison.Show");
        std::ostringstream signature;
        signature<<state.id<<'\x1f'<<state.response<<'\x1f'<<state.family<<'\x1f'
                 <<state.link<<'\x1f'<<state.scope<<'\x1f'<<state.autoRefit<<'\x1f'
                 <<state.showInformationCriteria<<'\x1f'<<state.activeModel<<'\x1f'
                 <<state.countComparison<<'\x1f'
                 <<::rlispstat::core::StatisticalModelTypeId(state.modelType)<<'\x1f'
                 <<state.multipleImputation;
        for(auto const& term:state.termRows)signature<<'\x1e'<<term;
        for(auto const& model:state.models){signature<<'\x1d'<<model.id<<'='<<model.label<<'\x1c'<<model.modelVersion<<'\x1c'<<model.fitVersion<<'\x1c'<<model.isStale<<'\x1c'<<model.family<<'\x1c'<<model.link<<'\x1c'<<model.scope<<'\x1c'<<(int)model.countDistribution<<'\x1c'<<model.exposure;for(auto const& term:model.terms)signature<<'\x1c'<<term;for(auto const& entry:model.termTypes)signature<<'\x1b'<<entry.first<<'='<<entry.second;for(auto const& centered:model.centeredPredictors)signature<<'\x1a'<<centered;for(auto const& reference:model.factorReferenceLevels)signature<<'\x19'<<reference.first<<'='<<reference.second;}
        const std::string nextSignature=signature.str();
        const bool controlsChanged=!reportInitialized_||commandChoicesChanged_||controlsSignature_!=nextSignature;
        state_=state;SetWindowDataSheetGroup(window_,state.group);SetWindowSnapshotSource(window_,"output",state.id,state.group);
        const std::string title=::rlispstat::core::StatisticalModelComparisonTitle(state.modelType,state.multipleImputation);
        window_.Title(to_hstring(title));title_.Text(to_hstring(title));badge_.Text(to_hstring(state.group));
        const bool hasTerms=std::any_of(state.models.begin(),state.models.end(),[](auto const& model){return !model.terms.empty();});
        const bool pending=std::any_of(state.models.begin(),state.models.end(),[](auto const& model){return model.isStale;});
        std::string coding;
        if(state.binaryComparison&&!state.response.empty())coding="     Event: "+state.responseCoding.eventLabel+"     Reference: "+state.responseCoding.referenceLabel;
        std::string subtitle;
        if(state.response.empty())subtitle="Choose a response variable.";
        else
        {
            subtitle="Response: "+state.response+coding;
            if(state.countComparison)subtitle+="     Link: log";
            subtitle+="     "+std::to_string(state.models.size())+(state.models.size()==1?" model":" models");
            if(!hasTerms)subtitle+="     Intercept-only";
            if(pending)subtitle+="     Updating\xE2\x80\xA6";
        }
        if (!state.frozenScopeNotice.empty()) subtitle += "     " + state.frozenScopeNotice;
        subtitle_.Text(to_hstring(subtitle));
        if(controlsChanged){RebuildControls();ConfigureContextMenu();controlsSignature_=nextSignature;commandChoicesChanged_=false;}
        Render(state);reportInitialized_=true;
        auto visibleFitRows=::rlispstat::core::GeneralizedComparisonVisibleFitRows(state);
        if(state.countComparison)
            visibleFitRows.erase(std::remove(visibleFitRows.begin(),visibleFitRows.end(),1),visibleFitRows.end());
        const auto fitRows=visibleFitRows.size();
        const double width=std::clamp(420.0+state.models.size()*180.0,680.0,kWideResultWindowMaximumWidth);
        const double height=std::max(430.0,
            170.0+(state.termRows.size()+fitRows+3.0)*28.0);
        PresentOrResizeAnalysisWindow(window_,presented_,width,height);
    }
    void GeneralizedComparisonView::Render(::rlispstat::core::GeneralizedComparisonState const& state)
    {
        using namespace ::rlispstat::core;
        content_.Children().Clear();
        const bool hasTerms=std::any_of(state.models.begin(),state.models.end(),[](auto const& model){return !model.terms.empty();});
        if(state.response.empty()){auto prompt=FieldLabel(L"Choose a response variable to begin.");prompt.FontSize(14);prompt.Foreground(Brush(78,82,88));content_.Children().Append(prompt);return;}
        auto send=[this](std::string const& name,std::string const& value=std::string{})
        {if(commandCallback_)commandCallback_({name,state_.id,value});};
        {
            const int activeModel=state.models.empty()?0:std::clamp(state.activeModel,0,(int)state.models.size()-1);
            auto finishMenu=[&](Controls::MenuFlyout const& menu)
            {
                if(exporter_)
                {
                    menu.Items().Append(Controls::MenuFlyoutSeparator());
                    auto exportMenu=exporter_->CreateMenu(
                        [snapshot=state](){return GeneralizedComparisonExportPayload(snapshot);},true);
                    AppendRCodeExportItems(exportMenu,commandCallback_,state.id);
                    menu.Items().Append(exportMenu);
                }
                AppendTableAnnotationContextItems(window_,menu);
                return menu;
            };
            auto modelMenu=[&](std::size_t modelIndex)
            {
                auto menu=Controls::MenuFlyout();auto title=Controls::MenuFlyoutItem();title.Text(to_hstring(state.models[modelIndex].label));title.IsEnabled(false);menu.Items().Append(title);menu.Items().Append(Controls::MenuFlyoutSeparator());
                auto rename=Controls::MenuFlyoutItem();rename.Text(L"Rename model\u2026");rename.Click([this,send,modelIndex](auto const&,auto const&)mutable{auto input=Controls::TextBox();input.Text(to_hstring(state_.models[modelIndex].label));input.SelectAll();auto dialog=Controls::ContentDialog();ConfigureNativeDialog(dialog,window_,L"Rename model",input,L"Rename",360);auto operation=dialog.ShowAsync();operation.Completed([input,send,modelIndex](auto const& completed,Windows::Foundation::AsyncStatus status)mutable{if(status==Windows::Foundation::AsyncStatus::Completed&&completed.GetResults()==Controls::ContentDialogResult::Primary){const auto label=to_string(input.Text());if(!label.empty())send("GCOMP_RENAME_MODEL",std::to_string(modelIndex)+'\x1f'+label);}});});menu.Items().Append(rename);
                auto terms=Controls::MenuFlyoutSubItem();terms.Text(L"Model terms");std::set<std::string> listed;for(auto const& row:state.termRows){const std::string term=GeneralizedComparisonSourceTerm(state,row);if(term=="(Intercept)"||!listed.insert(term).second)continue;auto item=Controls::ToggleMenuFlyoutItem();item.Text(to_hstring(term));item.IsChecked(GeneralizedModelIncludesTerm(state.models[modelIndex],term));item.Click([send,modelIndex,term](auto const&,auto const&)mutable{send("GCOMP_TOGGLE_TERM",std::to_string(modelIndex)+'\x1f'+term);});terms.Items().Append(item);}menu.Items().Append(terms);
                if(state.countComparison)
                {
                    auto distribution=Controls::MenuFlyoutSubItem();distribution.Text(L"Distribution");
                    for(auto const& specification:DistributionSpecificationsForModel(StatisticalModelType::Count))
                    {
                        CountDistribution parsed{};if(!ParseCountDistribution(specification.id,parsed)||parsed==CountDistribution::QuasiPoisson)continue;
                        auto item=Controls::RadioMenuFlyoutItem();item.GroupName(to_hstring("gcomp-count-distribution:"+state.id+":"+state.models[modelIndex].id));item.Text(to_hstring(specification.visibleName));item.IsChecked(state.models[modelIndex].countDistribution!=CountDistribution::QuasiPoisson&&state.models[modelIndex].countDistribution==parsed);item.Click([send,modelIndex,distributionId=specification.id](auto const&,auto const&)mutable{send("GCOMP_SET_COUNT_DISTRIBUTION",std::to_string(modelIndex)+'\x1f'+distributionId);});distribution.Items().Append(item);
                    }
                    menu.Items().Append(distribution);
                    const auto currentDistribution=state.models[modelIndex].countDistribution;
                    if(CountDistributionUsesTrials(currentDistribution))
                    {
                        auto trials=Controls::MenuFlyoutItem();trials.Text(L"Set trials\u2026");trials.Click([this,send,modelIndex](auto const&,auto const&)mutable{auto input=Controls::TextBox();const auto value=state_.models[modelIndex].trialsConstant;if(std::isfinite(value))input.Text(to_hstring(std::to_string(static_cast<long long>(value))));input.PlaceholderText(L"Positive integer");input.SelectAll();auto dialog=Controls::ContentDialog();ConfigureNativeDialog(dialog,window_,L"Trials",input,L"Apply",360);auto operation=dialog.ShowAsync();operation.Completed([input,send,modelIndex](auto const& completed,Windows::Foundation::AsyncStatus status)mutable{if(status==Windows::Foundation::AsyncStatus::Completed&&completed.GetResults()==Controls::ContentDialogResult::Primary)send("GCOMP_SET_TRIALS",std::to_string(modelIndex)+'\x1f'+to_string(input.Text()));});});menu.Items().Append(trials);
                    }
                    else
                    {
                        auto inference=Controls::MenuFlyoutSubItem();inference.Text(L"Dispersion / inference");
                        for(auto const& entry:std::vector<std::pair<std::string,std::wstring>>{{"standard",L"Standard likelihood model"},{"quasipoisson",L"Quasi-Poisson"}})
                        {
                            auto item=Controls::RadioMenuFlyoutItem();item.GroupName(to_hstring("gcomp-count-inference:"+state.id+":"+state.models[modelIndex].id));item.Text(entry.second);const bool quasi=currentDistribution==CountDistribution::QuasiPoisson;item.IsChecked((entry.first=="quasipoisson")==quasi);item.Click([send,modelIndex,inferenceId=entry.first,currentDistribution](auto const&,auto const&)mutable{const auto target=inferenceId=="quasipoisson"?std::string("quasipoisson"):(currentDistribution==CountDistribution::QuasiPoisson?std::string("poisson"):CountDistributionId(currentDistribution));send("GCOMP_SET_COUNT_DISTRIBUTION",std::to_string(modelIndex)+'\x1f'+target);});inference.Items().Append(item);
                        }
                        menu.Items().Append(inference);
                        auto exposure=Controls::MenuFlyoutSubItem();exposure.Text(L"Exposure");
                        auto none=Controls::RadioMenuFlyoutItem();none.GroupName(to_hstring("gcomp-count-exposure:"+state.id+":"+state.models[modelIndex].id));none.Text(L"None");none.IsChecked(state.models[modelIndex].exposure.empty());none.Click([send,modelIndex](auto const&,auto const&)mutable{send("GCOMP_SET_EXPOSURE",std::to_string(modelIndex)+'\x1f');});exposure.Items().Append(none);
                        for(auto const& variable:exposureVariables_)
                        {
                            if(variable==state.response)continue;auto item=Controls::RadioMenuFlyoutItem();item.GroupName(to_hstring("gcomp-count-exposure:"+state.id+":"+state.models[modelIndex].id));item.Text(to_hstring(variable));item.IsChecked(state.models[modelIndex].exposure==variable);item.Click([send,modelIndex,variable](auto const&,auto const&)mutable{send("GCOMP_SET_EXPOSURE",std::to_string(modelIndex)+'\x1f'+variable);});exposure.Items().Append(item);
                        }
                        menu.Items().Append(exposure);
                    }

                }
                const auto& selectedModel = state.models[modelIndex];
                const bool supportsOffset = selectedModel.family != "beta_one_inflated" &&
                    (!state.countComparison ||
                     (selectedModel.countDistribution != CountDistribution::HurdleBetaBinomialCeiling &&
                      selectedModel.countDistribution != CountDistribution::PerfectScore));
                if (supportsOffset && state.hasSeed)
                {
                    auto offset=Controls::MenuFlyoutSubItem();offset.Text(L"Link-scale offset (advanced)");
                    const auto group=to_hstring("gcomp-offset:"+state.id+":"+selectedModel.id);
                    auto none=Controls::RadioMenuFlyoutItem();none.GroupName(group);none.Text(L"None");none.IsChecked(selectedModel.offsetVariable.empty());
                    none.Click([send,modelIndex](auto const&,auto const&)mutable{send("GCOMP_SET_OFFSET",std::to_string(modelIndex)+'\x1f');});offset.Items().Append(none);
                    for (auto const& variable : ::rlispstat::core::NumericVariableNames(&state.seed))
                    {
                        if(variable==selectedModel.response||variable==selectedModel.exposure)continue;
                        auto item=Controls::RadioMenuFlyoutItem();item.GroupName(group);item.Text(to_hstring(variable));item.IsChecked(variable==selectedModel.offsetVariable);
                        item.Click([send,modelIndex,variable](auto const&,auto const&)mutable{send("GCOMP_SET_OFFSET",std::to_string(modelIndex)+'\x1f'+variable);});offset.Items().Append(item);
                    }
                    menu.Items().Append(offset);
                }
                menu.Items().Append(Controls::MenuFlyoutSeparator());
                auto residuals=Controls::MenuFlyoutSubItem();residuals.Text(L"Diagnostic residuals");residuals.IsEnabled(state.models[modelIndex].fit.ok&&!state.models[modelIndex].fit.diagnostics.empty());
                for(auto const& residualType:AvailableGeneralizedResidualTypes(state.models[modelIndex].fit))
                {
                    auto item=Controls::RadioMenuFlyoutItem();
                    item.GroupName(to_hstring("gcomp-residual:"+state.id+":"+state.models[modelIndex].id));
                    item.Text(to_hstring(GeneralizedResidualTypeLabel(residualType)));
                    item.IsChecked(state.models[modelIndex].diagnosticOptions.residualType==residualType);
                    item.Click([send,modelId=state.models[modelIndex].id,residualType](auto const&,auto const&)mutable{send("GCOMP_SET_RESIDUAL",modelId+'\x1f'+residualType);});
                    residuals.Items().Append(item);
                }
                menu.Items().Append(residuals);
                auto diagnostics=Controls::MenuFlyoutSubItem();diagnostics.Text(to_hstring(DefaultModelContextMenuTitles().openDiagnostics));diagnostics.IsEnabled(state.models[modelIndex].fit.ok&&!state.models[modelIndex].fit.diagnostics.empty());
                for(auto const& option:ModelDiagnosticPlotOptions(true)){if((state.binaryComparison||state.countComparison)&&(option.value=="scale_location"||option.value=="residuals_leverage"||option.value=="cooks_distance"))continue;auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(option.title));item.Click([this,modelIndex,kind=option.value](auto const&,auto const&){if(diagnosticCallback_)diagnosticCallback_((int)modelIndex,kind);});diagnostics.Items().Append(item);}
                if(state.binaryComparison){for(auto const& option:std::vector<std::pair<std::string,std::string>>{{"ROC curve","roc_curve"},{"Calibration plot","calibration_plot"}}){auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(option.first));item.Click([this,modelIndex,kind=option.second](auto const&,auto const&){if(diagnosticCallback_)diagnosticCallback_((int)modelIndex,kind);});diagnostics.Items().Append(item);}}
                if(state.binaryComparison||(state.countComparison&&CountDistributionSupportsScoreDistribution(state.models[modelIndex].fit.countDistribution))){auto item=Controls::MenuFlyoutItem();item.Text(L"Observed vs predicted distribution");item.Click([this,modelIndex](auto const&,auto const&){if(diagnosticCallback_)diagnosticCallback_((int)modelIndex,"observed_predicted_score_distribution");});diagnostics.Items().Append(item);auto boundary=Controls::MenuFlyoutItem();boundary.Text(L"Boundary / zero fit");boundary.Click([this,modelIndex](auto const&,auto const&){if(diagnosticCallback_)diagnosticCallback_((int)modelIndex,"boundary_zero_fit");});diagnostics.Items().Append(boundary);}
                menu.Items().Append(diagnostics);
                menu.Items().Append(Controls::MenuFlyoutSeparator());
                auto open=Controls::MenuFlyoutItem();open.Text(state.countComparison?L"Open this model as single Count Model":(state.binaryComparison?L"Open this model as single Binary Model":L"Open this model as single Generalized Linear Model"));open.Click([send,modelIndex](auto const&,auto const&)mutable{send("GCOMP_OPEN_SINGLE",std::to_string(modelIndex));});menu.Items().Append(open);
                auto refit=Controls::MenuFlyoutItem();refit.Text(L"Refit this model");refit.Click([send,modelIndex](auto const&,auto const&)mutable{send("GCOMP_REFIT_MODEL",std::to_string(modelIndex));});menu.Items().Append(refit);
                auto duplicate=Controls::MenuFlyoutItem();duplicate.Text(L"Duplicate model");duplicate.Click([send,modelIndex](auto const&,auto const&)mutable{send("GCOMP_DUPLICATE_MODEL",std::to_string(modelIndex));});menu.Items().Append(duplicate);
                auto remove=Controls::MenuFlyoutItem();remove.Text(L"Delete model");remove.IsEnabled(state.models.size()>1);remove.Click([send,modelIndex](auto const&,auto const&)mutable{send("GCOMP_DELETE_MODEL",std::to_string(modelIndex));});menu.Items().Append(remove);return finishMenu(menu);
            };
            auto addTermMenu=[&](std::size_t modelIndex)
            {
                auto menu=Controls::MenuFlyout();for(auto const& variable:availableVariables_){if(variable==state.response||GeneralizedModelIncludesTerm(state.models[modelIndex],variable))continue;auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(variable));item.Click([send,modelIndex,variable](auto const&,auto const&)mutable{send("GCOMP_ADD_TERM_MODEL",std::to_string(modelIndex)+'\x1f'+variable);});menu.Items().Append(item);}
                std::vector<std::vector<std::string>> comparisonTerms;for(auto const& model:state.models)comparisonTerms.push_back(model.terms);
                AppendPolynomialMenu(menu, PolynomialNumericVariables(availableVariables_, state.models[modelIndex], &state.seed), state.response, state.models[modelIndex].terms,
                    [send,modelIndex](std::string const& value) mutable { send("GCOMP_ADD_INTERACTION", std::to_string(modelIndex)+'\x1f'+value); });
                const auto interactions=InteractionCandidateTermsForComparison(availableVariables_,state.response,comparisonTerms,state.models[modelIndex].terms);
                if(!interactions.empty())
                {
                    if(menu.Items().Size()>0)menu.Items().Append(Controls::MenuFlyoutSeparator());
                    auto submenu=BuildInteractionMenuSubItem(interactions,[send,modelIndex](std::string const& interaction)mutable{send("GCOMP_ADD_INTERACTION",std::to_string(modelIndex)+'\x1f'+interaction);});if(submenu)menu.Items().Append(submenu);
                }
                return finishMenu(menu);
            };
            auto replacementMenu=[&](std::string const& source,std::size_t modelIndex)
            {
                auto menu=Controls::MenuFlyout();
                for(auto const& candidate:availableVariables_)
                {
                    if(candidate==state.response||candidate==source||GeneralizedModelIncludesTerm(state.models[modelIndex],candidate))continue;
                    auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(candidate));
                    item.Click([send,modelIndex,source,candidate](auto const&,auto const&)mutable{send("GCOMP_REPLACE_TERM",std::to_string(modelIndex)+'\x1f'+source+'\x1f'+candidate);});
                    menu.Items().Append(item);
                }
                if(menu.Items().Size()==0)
                {
                    auto empty=Controls::MenuFlyoutItem();empty.Text(L"No replacement variables available");empty.IsEnabled(false);menu.Items().Append(empty);
                }
                return menu;
            };
            auto termMenu=[&](std::string const& term,std::size_t modelIndex,Controls::TextBlock const& valueText)
            {
                auto menu=Controls::MenuFlyout();auto remove=Controls::MenuFlyoutItem();remove.Text(term.find(':')==std::string::npos?L"Remove predictor from comparison":L"Remove interaction from comparison");remove.Click([send,term](auto const&,auto const&)mutable{send("GCOMP_REMOVE_TERM",term);});menu.Items().Append(remove);menu.Items().Append(Controls::MenuFlyoutSeparator());auto title=Controls::MenuFlyoutItem();title.Text(to_hstring(term+" \xC2\xB7 "+state.models[modelIndex].label));title.IsEnabled(false);menu.Items().Append(title);menu.Items().Append(Controls::MenuFlyoutSeparator());
                auto analyze=Controls::MenuFlyoutSubItem();analyze.Text(L"Analyze predictor");
                auto details=Controls::MenuFlyoutItem();details.Text(to_hstring(DefaultModelContextMenuTitles().showCoefficientDetails));details.Click([this,modelIndex,term](auto const&,auto const&){if(modelIndex>=state_.models.size())return;auto row=GeneralizedComparisonRowForPresentationTerm(state_.models[modelIndex].fit.rows,term);if(!row)return;auto body=Controls::TextBlock();body.Text(to_hstring(GeneralizedComparisonCoefficientDetailsStatus(*row,state_.binaryComparison&&state_.models[modelIndex].link=="logit")));body.TextWrapping(TextWrapping::Wrap);auto dialog=Controls::ContentDialog();ConfigureNativeDialog(dialog,window_,L"Coefficient details",body,L"Close",440);dialog.ShowAsync();});analyze.Items().Append(details);
                auto explain=Controls::MenuFlyoutItem();explain.Text(to_hstring(DefaultModelContextMenuTitles().explainStatistic));explain.Click([valueText,context=GeneralizedComparisonStatisticContext(state,modelIndex,"b")](auto const&,auto const&){ShowStatisticExplanation(valueText,context);});analyze.Items().Append(explain);
                const std::string analysisType=GeneralizedComparisonModelTermType(state,(int)modelIndex,term);
                if(term.find(':')==std::string::npos)
                {
                    auto effect=Controls::MenuFlyoutItem();effect.Text(L"Effect plot…");
                    effect.Click([send,modelIndex,term](auto const&,auto const&)mutable{send("GCOMP_INTERACTION_PLOT",std::to_string(modelIndex)+'\x1f'+term);});
                    analyze.Items().Append(effect);
                    auto partial=Controls::MenuFlyoutItem();partial.Text(L"Partial regression plot…");
                    partial.Click([send,modelIndex,term](auto const&,auto const&)mutable{send("GCOMP_PARTIAL_PLOT",std::to_string(modelIndex)+'\x1f'+term);});
                    analyze.Items().Append(partial);
                }
                if(term.find(':')==std::string::npos&&(analysisType=="factor"||analysisType=="ordered"))
                {
                    auto pairwise=Controls::MenuFlyoutItem();pairwise.Text(L"Pairwise comparisons…");
                    pairwise.Click([send,modelIndex,term](auto const&,auto const&)mutable{send("GCOMP_PAIRWISE",std::to_string(modelIndex)+'\x1f'+term);});
                    analyze.Items().Append(pairwise);
                }
                if(term.find(':')!=std::string::npos)
                {
                    auto report=Controls::MenuFlyoutItem();report.Text(L"Interpret interaction…");
                    report.Click([send,modelIndex,term](auto const&,auto const&)mutable{send("GCOMP_INTERACTION_REPORT",std::to_string(modelIndex)+'\x1f'+term);});
                    analyze.Items().Append(report);
                    auto plot=Controls::MenuFlyoutItem();plot.Text(L"Effect plot…");
                    plot.Click([send,modelIndex,term](auto const&,auto const&)mutable{send("GCOMP_INTERACTION_PLOT",std::to_string(modelIndex)+'\x1f'+term);});
                    analyze.Items().Append(plot);
                }
                if(analyze.Items().Size()>0)menu.Items().Append(analyze);
                auto modelTerms=Controls::MenuFlyoutSubItem();modelTerms.Text(L"Model terms");auto inModel=Controls::ToggleMenuFlyoutItem();inModel.Text(L"Include in this model");inModel.IsChecked(GeneralizedModelIncludesTerm(state.models[modelIndex],term));inModel.Click([send,modelIndex,term](auto const&,auto const&)mutable{send("GCOMP_TOGGLE_TERM",std::to_string(modelIndex)+'\x1f'+term);});modelTerms.Items().Append(inModel);auto addAll=Controls::MenuFlyoutItem();addAll.Text(L"Add to all models");addAll.Click([send,term](auto const&,auto const&)mutable{send("GCOMP_ADD_TERM_ALL",term);});modelTerms.Items().Append(addAll);{std::vector<std::vector<std::string>> comparisonTerms;for(auto const& model:state.models)comparisonTerms.push_back(model.terms);const auto candidates=InteractionCandidateTermsForComparison(availableVariables_,state.response,comparisonTerms,state.models[modelIndex].terms,term);if(auto interactions=BuildInteractionMenuSubItem(candidates,[send,modelIndex](std::string const& interaction)mutable{send("GCOMP_ADD_INTERACTION",std::to_string(modelIndex)+'\x1f'+interaction);},true,term)){modelTerms.Items().Append(interactions);}}menu.Items().Append(modelTerms);
                auto edit=Controls::MenuFlyoutSubItem();edit.Text(L"Edit predictor");
                if(term.find(':')==std::string::npos&&term.find('=')==std::string::npos)
                {
                    const std::string variable=::rlispstat::core::BaseVariableForTermComponent(term);
                    const std::string type=GeneralizedComparisonModelTermType(state,(int)modelIndex,variable);
                    auto types=Controls::MenuFlyoutSubItem();types.Text(L"Change type");
                    for(auto const& entry:std::vector<std::pair<std::string,std::wstring>>{{"numeric",L"Treat predictor as continuous"},{"factor",L"Treat predictor as categorical"}})
                    {
                        auto item=Controls::RadioMenuFlyoutItem();item.GroupName(to_hstring("gcomp-cell-type:"+state.id+":"+std::to_string(modelIndex)+":"+variable));item.Text(entry.second);item.IsChecked(type==entry.first);item.Click([send,modelIndex,variable,nextType=entry.first](auto const&,auto const&)mutable{send("GCOMP_SET_TYPE",std::to_string(modelIndex)+'\x1f'+variable+'\x1f'+nextType);});types.Items().Append(item);
                    }
                    edit.Items().Append(types);
                    if(type!="factor"&&type!="ordered")
                    {
                        const bool centered=state.models[modelIndex].centeredPredictors.count(variable)>0;auto center=Controls::MenuFlyoutItem();center.Text(centered?L"Remove centering":L"Center predictor");center.IsEnabled(GeneralizedModelIncludesTerm(state.models[modelIndex],variable));center.Click([send,modelIndex,variable](auto const&,auto const&)mutable{send("GCOMP_TOGGLE_CENTER",std::to_string(modelIndex)+'\x1f'+variable);});edit.Items().Append(center);
                    }
                    else
                    {
                        std::vector<std::string> levels;std::string fittedReference;
                        for(auto const& row:state.models[modelIndex].fit.rows)
                        {
                            const std::string source=row.sourceTerm.empty()?row.term:row.sourceTerm;
                            if(::rlispstat::core::BaseVariableForTermComponent(source)!=variable)continue;
                            if(!row.referenceLevel.empty())fittedReference=row.referenceLevel;
                            if(!row.factorLevel.empty()&&std::find(levels.begin(),levels.end(),row.factorLevel)==levels.end())levels.push_back(row.factorLevel);
                        }
                        if(!fittedReference.empty()&&std::find(levels.begin(),levels.end(),fittedReference)==levels.end())levels.insert(levels.begin(),fittedReference);
                        const auto configured=state.models[modelIndex].factorReferenceLevels.find(variable);
                        const std::string selected=configured!=state.models[modelIndex].factorReferenceLevels.end()?configured->second:fittedReference;
                        if(levels.size()>1)
                        {
                            auto reference=Controls::MenuFlyoutSubItem();reference.Text(L"Reference category");
                            for(auto const& level:levels)
                            {
                                auto item=Controls::RadioMenuFlyoutItem();item.GroupName(to_hstring("gcomp-cell-reference:"+state.id+":"+std::to_string(modelIndex)+":"+variable));item.Text(to_hstring(level));item.IsChecked(level==(selected.empty()?levels.front():selected));item.Click([send,modelIndex,variable,level](auto const&,auto const&)mutable{send("GCOMP_SET_REFERENCE",std::to_string(modelIndex)+'\x1f'+variable+'\x1f'+level);});reference.Items().Append(item);
                            }
                            edit.Items().Append(reference);
                        }
                    }
                }
                if(edit.Items().Size()>0)menu.Items().Append(edit);
                auto diagnostics=Controls::MenuFlyoutSubItem();diagnostics.Text(to_hstring(DefaultModelContextMenuTitles().openDiagnostics));diagnostics.IsEnabled(state.models[modelIndex].fit.ok&&!state.models[modelIndex].fit.diagnostics.empty());
                for(auto const& option:ModelDiagnosticPlotOptions(true)){if((state.binaryComparison||state.countComparison)&&(option.value=="scale_location"||option.value=="residuals_leverage"||option.value=="cooks_distance"))continue;auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(option.title));item.Click([this,modelIndex,kind=option.value](auto const&,auto const&){if(diagnosticCallback_)diagnosticCallback_((int)modelIndex,kind);});diagnostics.Items().Append(item);}
                if(state.binaryComparison){for(auto const& option:std::vector<std::pair<std::string,std::string>>{{"ROC curve","roc_curve"},{"Calibration plot","calibration_plot"}}){auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(option.first));item.Click([this,modelIndex,kind=option.second](auto const&,auto const&){if(diagnosticCallback_)diagnosticCallback_((int)modelIndex,kind);});diagnostics.Items().Append(item);}}menu.Items().Append(diagnostics);
                auto copy=Controls::MenuFlyoutSubItem();copy.Text(L"Copy");auto copyValue=Controls::MenuFlyoutItem();copyValue.Text(L"Value");copyValue.Click([valueText](auto const&,auto const&){CopyTextToClipboard(valueText.Text());});copy.Items().Append(copyValue);auto copyNamed=Controls::MenuFlyoutItem();copyNamed.Text(L"Predictor and value");copyNamed.Click([valueText,term](auto const&,auto const&){CopyTextToClipboard(to_hstring(term)+L"\t"+valueText.Text());});copy.Items().Append(copyNamed);menu.Items().Append(copy);return finishMenu(menu);
            };
            auto grid=Controls::Grid();auto stub=Controls::ColumnDefinition();stub.Width(GridLengthHelper::FromPixels(300));grid.ColumnDefinitions().Append(stub);for(std::size_t i=0;i<state.models.size();++i){auto column=Controls::ColumnDefinition();column.Width(GridLengthHelper::FromPixels(180));grid.ColumnDefinitions().Append(column);}auto addModelColumn=Controls::ColumnDefinition();addModelColumn.Width(GridLengthHelper::FromPixels(120));grid.ColumnDefinitions().Append(addModelColumn);
            auto addRow=[&](){auto row=Controls::RowDefinition();row.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Auto));grid.RowDefinitions().Append(row);};auto add=[&](FrameworkElement const& cell,int row,int column){Controls::Grid::SetRow(cell,row);Controls::Grid::SetColumn(cell,column);grid.Children().Append(cell);};
            addRow();add(TableCell(L"",true,true),0,0);for(std::size_t i=0;i<state.models.size();++i){auto cell=TableCell(to_hstring(state.models[i].label).c_str(),true,false);if((int)i==activeModel)cell.Background(Brush(242,247,252));cell.Tapped([send,i](auto const&,Input::TappedRoutedEventArgs const& args)mutable{send("GCOMP_SET_ACTIVE",std::to_string(i));args.Handled(true);});add(cell,0,(int)i+1);AttachContextOnlyMenu(cell,modelMenu(i));}auto addModel=TableCell(L"+ Add model",true,false);addModel.Tapped([send](auto const&,Input::TappedRoutedEventArgs const& args)mutable{send("GCOMP_ADD_EMPTY_MODEL","");args.Handled(true);});auto addModelMenu=Controls::MenuFlyout();auto emptyModel=Controls::MenuFlyoutItem();emptyModel.Text(L"New empty model");emptyModel.Click([send](auto const&,auto const&)mutable{send("GCOMP_ADD_EMPTY_MODEL","");});addModelMenu.Items().Append(emptyModel);for(std::size_t i=0;i<state.models.size();++i){auto item=Controls::MenuFlyoutItem();item.Text(to_hstring("Duplicate "+state.models[i].label));item.Click([send,i](auto const&,auto const&)mutable{send("GCOMP_DUPLICATE_MODEL",std::to_string(i));});addModelMenu.Items().Append(item);}add(addModel,0,(int)state.models.size()+1);AttachContextOnlyMenu(addModel,finishMenu(addModelMenu));
            int rowIndex=1;
            for(auto const& rawTerm:state.termRows)
            {
                addRow();
                const std::string label=GeneralizedComparisonDisplayLabel(state,rawTerm);
                const std::string source=GeneralizedComparisonSourceTerm(state,rawTerm);
                auto labelCell=TableCell(to_hstring(label).c_str(),false,true,false,false,false,rowIndex>1);
                add(labelCell,rowIndex,0);
                if(rawTerm!="(Intercept)")
                {
                    auto rowMenu=Controls::MenuFlyout();
                    auto remove=Controls::MenuFlyoutItem();remove.Text(source.find(':')==std::string::npos?L"Remove predictor from comparison":L"Remove interaction from comparison");
                    remove.Click([send,source](auto const&,auto const&)mutable{send("GCOMP_REMOVE_TERM",source);});rowMenu.Items().Append(remove);rowMenu.Items().Append(Controls::MenuFlyoutSeparator());
                    auto title=Controls::MenuFlyoutItem();title.Text(to_hstring(source));title.IsEnabled(false);rowMenu.Items().Append(title);
                    if(source.find(':')==std::string::npos&&source.find('=')==std::string::npos&&!state.models.empty())
                    {
                        auto replace=Controls::MenuFlyoutSubItem();replace.Text(to_hstring("Replace in "+state.models[(std::size_t)activeModel].label));
                        for(auto const& candidate:availableVariables_)
                        {
                            if(candidate==state.response||candidate==source||GeneralizedModelIncludesTerm(state.models[(std::size_t)activeModel],candidate))continue;
                            auto item=Controls::MenuFlyoutItem();item.Text(to_hstring(candidate));
                            item.Click([send,activeModel,source,candidate](auto const&,auto const&)mutable{send("GCOMP_REPLACE_TERM",std::to_string(activeModel)+'\x1f'+source+'\x1f'+candidate);});
                            replace.Items().Append(item);
                        }
                        if(replace.Items().Size()>0)rowMenu.Items().Append(replace);
                    }
                    auto models=Controls::MenuFlyoutSubItem();models.Text(L"Models");
                    for(std::size_t i=0;i<state.models.size();++i)
                    {
                        auto item=Controls::ToggleMenuFlyoutItem();item.Text(to_hstring(state.models[i].label));
                        item.IsChecked(GeneralizedModelIncludesTerm(state.models[i],source));
                        item.Click([send,i,source](auto const&,auto const&)mutable{send("GCOMP_TOGGLE_TERM",std::to_string(i)+'\x1f'+source);});
                        models.Items().Append(item);
                    }
                    rowMenu.Items().Append(models);
                    const bool replaceOnPrimary=rawTerm==source&&source.find(':')==std::string::npos&&source.find('=')==std::string::npos&&!state.models.empty();
                    if(replaceOnPrimary)
                        AttachPrimaryAndContextMenus(labelCell,replacementMenu(source,(std::size_t)activeModel),finishMenu(rowMenu));
                    else
                        AttachDirectMenu(labelCell,finishMenu(rowMenu));
                }
                else AttachDirectMenu(labelCell,CreateStructuralCellMenu(L"(Intercept)",labelCell.Child().as<Controls::TextBlock>()));
                for(std::size_t i=0;i<state.models.size();++i)
                {
                    const auto cellState=GeneralizedComparisonTermCell(state,(int)i,rawTerm);
                    auto valueCell=TableCell(to_hstring(cellState.displayText).c_str(),false,false,false,false,false,rowIndex>1);
                    auto valueText=valueCell.Child().as<Controls::TextBlock>();
                    if(cellState.inclusionControlAvailable)
                    {
                        auto panel=Controls::Grid();auto glyphColumn=Controls::ColumnDefinition();glyphColumn.Width(GridLengthHelper::FromPixels(30));panel.ColumnDefinitions().Append(glyphColumn);auto valueColumn=Controls::ColumnDefinition();valueColumn.Width(GridLengthHelper::FromValueAndType(1,GridUnitType::Star));panel.ColumnDefinitions().Append(valueColumn);auto toggle=Controls::Border();toggle.Width(26);toggle.Height(22);toggle.CornerRadius(CornerRadius{3});toggle.Background(Brush(248,249,251));toggle.BorderBrush(Brush(221,225,230));toggle.BorderThickness(Thickness{1});auto glyph=Controls::TextBlock();glyph.Text(cellState.termIncluded?L"\u2212":L"+");glyph.FontSize(14);glyph.HorizontalAlignment(HorizontalAlignment::Center);glyph.VerticalAlignment(VerticalAlignment::Center);toggle.Child(glyph);toggle.Tapped([send,i,source](auto const&,Input::TappedRoutedEventArgs const& args)mutable{send("GCOMP_TOGGLE_TERM",std::to_string(i)+'\x1f'+source);args.Handled(true);});panel.Children().Append(toggle);valueCell.Child(nullptr);valueText.Text(to_hstring(cellState.displayText));valueText.HorizontalAlignment(HorizontalAlignment::Right);Controls::Grid::SetColumn(valueText,1);panel.Children().Append(valueText);valueCell.Child(panel);
                    }
                    valueCell.Tapped([send,i](auto const&,Input::TappedRoutedEventArgs const& args)mutable{send("GCOMP_SET_ACTIVE",std::to_string(i));args.Handled(true);});
                    add(valueCell,rowIndex,(int)i+1);
                    if(rawTerm!="(Intercept)")AttachContextOnlyMenu(valueCell,termMenu(source,i,valueText));
                    else AttachContextOnlyMenu(valueCell,CreateStatisticCellMenu(
                        L"(Intercept)",valueText,
                        GeneralizedComparisonStatisticContext(state,i,"b")));
                }
                ++rowIndex;
            }
            addRow();auto addTermLabel=TableCell(L"+ Add term",false,true,false,false,false,true);add(addTermLabel,rowIndex,0);if(!state.models.empty())AttachDirectMenu(addTermLabel,addTermMenu((std::size_t)activeModel));for(std::size_t i=0;i<state.models.size();++i){auto cell=TableCell(L"",false,false,false,false,false,true);add(cell,rowIndex,(int)i+1);}++rowIndex;
            auto visibleFitRows=GeneralizedComparisonVisibleFitRows(state);
            if(state.countComparison)
                visibleFitRows.erase(std::remove(visibleFitRows.begin(),visibleFitRows.end(),1),visibleFitRows.end());
            for(int fitRow:visibleFitRows){addRow();const std::string label=GeneralizedComparisonFitLabel(fitRow);auto labelCell=TableCell(to_hstring(label).c_str(),false,true,false,false,false,true);add(labelCell,rowIndex,0);AttachDirectMenu(labelCell,CreateStructuralCellMenu(to_hstring(label).c_str(),labelCell.Child().as<Controls::TextBlock>()));for(std::size_t i=0;i<state.models.size();++i){const auto cellState=GeneralizedComparisonFitCell(state,(int)i,fitRow);auto valueCell=TableCell(to_hstring(cellState.displayText).c_str(),false,false,false,false,false,true);auto valueText=valueCell.Child().as<Controls::TextBlock>();valueCell.Tapped([send,i](auto const&,Input::TappedRoutedEventArgs const& args)mutable{send("GCOMP_SET_ACTIVE",std::to_string(i));args.Handled(true);});add(valueCell,rowIndex,(int)i+1);if(state.countComparison&&(fitRow==16||fitRow==17))AttachContextOnlyMenu(valueCell,modelMenu(i));else AttachContextOnlyMenu(valueCell,CreateStatisticCellMenu(to_hstring(label).c_str(),valueText,GeneralizedComparisonStatisticContext(state,i,label)));}++rowIndex;}
            content_.Children().Append(grid);auto note=FieldLabel(to_hstring(state.countComparison?CountRegressionComparisonFootnote():GeneralizedComparisonFootnote()).c_str());note.FontSize(11);note.Foreground(Brush(92,92,92));note.TextWrapping(TextWrapping::Wrap);content_.Children().Append(note);return;
        }
        if(!hasTerms){auto prompt=FieldLabel(L"Add at least one independent variable to begin.");prompt.FontSize(14);prompt.Foreground(Brush(78,82,88));content_.Children().Append(prompt);return;}
#if 0
        auto controls=Controls::StackPanel();controls.Orientation(Controls::Orientation::Horizontal);controls.Spacing(8);auto fieldLabel=[](std::wstring const& text){auto result=FieldLabel(text);result.FontSize(11.5);return result;};auto send=[this](std::string const& name,std::string const& value=std::string{}){if(commandCallback_)commandCallback_({name,state_.id,value});};
        controls.Children().Append(fieldLabel(L"Response:"));auto response=Controls::ComboBox();response.MinWidth(105);response.FontSize(11.5);for(auto const& variable:responseVariables_)response.Items().Append(box_value(to_hstring(variable)));auto responseAt=std::find(responseVariables_.begin(),responseVariables_.end(),state.response);if(responseAt!=responseVariables_.end())response.SelectedIndex((int)(responseAt-responseVariables_.begin()));response.SelectionChanged([send,response](auto const&,auto const&)mutable{if(response.SelectedIndex()>=0)send("GCOMP_SET_RESPONSE",to_string(response.SelectedItem().as<hstring>()));});controls.Children().Append(response);
        if(state.binaryComparison){controls.Children().Append(fieldLabel(L"Event category:"));auto event=Controls::ComboBox();event.MinWidth(76);event.FontSize(11.5);event.Items().Append(box_value(to_hstring(state.responseCoding.referenceLabel)));event.Items().Append(box_value(to_hstring(state.responseCoding.eventLabel)));event.SelectedIndex(1);event.SelectionChanged([send,event](auto const&,auto const&)mutable{if(event.SelectedIndex()>=0)send("GCOMP_SET_EVENT",to_string(event.SelectedItem().as<hstring>()));});controls.Children().Append(event);controls.Children().Append(fieldLabel(L"Link:"));auto link=Controls::ComboBox();link.FontSize(11.5);link.Items().Append(box_value(L"logit"));link.Items().Append(box_value(L"probit"));link.SelectedIndex(state.link=="probit"?1:0);link.SelectionChanged([send,link](auto const&,auto const&)mutable{if(link.SelectedIndex()>=0)send("GCOMP_SET_LINK",to_string(link.SelectedItem().as<hstring>()));});controls.Children().Append(link);}
        controls.Children().Append(fieldLabel(L"Scope:"));auto scope=Controls::ComboBox();scope.FontSize(11.5);for(auto const& value:{L"all",L"selected",L"unselected"})scope.Items().Append(box_value(value));scope.SelectedIndex(state.scope=="selected"?1:(state.scope=="unselected"?2:0));scope.SelectionChanged([send,scope](auto const&,auto const&)mutable{if(scope.SelectedIndex()>=0)send("GCOMP_SET_SCOPE",to_string(scope.SelectedItem().as<hstring>()));});controls.Children().Append(scope);auto autoRefit=Controls::CheckBox();autoRefit.Content(box_value(L"Auto-refit"));autoRefit.IsChecked(state.autoRefit);autoRefit.FontSize(11.5);autoRefit.Click([send](auto const&,auto const&)mutable{send("GCOMP_TOGGLE_AUTO");});controls.Children().Append(autoRefit);
        auto active=Controls::ComboBox();active.MinWidth(115);active.FontSize(11.5);for(auto const& model:state.models)active.Items().Append(box_value(to_hstring(model.label)));if(!state.models.empty())active.SelectedIndex(std::clamp(state.activeModel,0,(int)state.models.size()-1));active.SelectionChanged([send,active](auto const&,auto const&)mutable{if(active.SelectedIndex()>=0)send("GCOMP_SET_ACTIVE",std::to_string(active.SelectedIndex()));});controls.Children().Append(active);
        auto variables=Controls::Button();variables.Content(box_value(L"Independent variables…"));variables.FontSize(11.5);auto variableMenu=Controls::MenuFlyout();const int activeIndex=state.models.empty()?0:std::clamp(state.activeModel,0,(int)state.models.size()-1);for(auto const& variable:availableVariables_){if(variable==state.response)continue;const bool included=!state.models.empty()&&GeneralizedModelIncludesTerm(state.models[(size_t)activeIndex],variable);auto item=Controls::ToggleMenuFlyoutItem();item.Text(to_hstring(variable));item.IsChecked(included);item.Click([send,variable,included,activeIndex](auto const&,auto const&)mutable{if(included)send("GCOMP_TOGGLE_TERM",std::to_string(activeIndex)+'\x1f'+variable);else send("GCOMP_ADD_TERM",variable);});variableMenu.Items().Append(item);}variables.Flyout(variableMenu);controls.Children().Append(variables);auto addModel=Controls::Button();addModel.Content(box_value(L"Add model"));addModel.FontSize(11.5);addModel.Click([send](auto const&,auto const&)mutable{send("GCOMP_ADD_MODEL");});controls.Children().Append(addModel);auto deleteModel=Controls::Button();deleteModel.Content(box_value(L"Delete model"));deleteModel.FontSize(11.5);deleteModel.IsEnabled(state.models.size()>2);deleteModel.Click([send,activeIndex](auto const&,auto const&)mutable{send("GCOMP_DELETE_MODEL",std::to_string(activeIndex));});controls.Children().Append(deleteModel);auto refit=Controls::Button();refit.Content(box_value(L"Refit all"));refit.FontSize(11.5);refit.Click([send](auto const&,auto const&)mutable{send("GCOMP_REFIT");});controls.Children().Append(refit);

#endif
        if(!state.termRows.empty())
        {
            auto termHeading=FieldLabel(L"Terms in each model");termHeading.FontSize(13);termHeading.FontWeight(Windows::UI::Text::FontWeights::SemiBold());content_.Children().Append(termHeading);
            auto termGrid=Controls::Grid();auto stub=Controls::ColumnDefinition();stub.Width(GridLengthHelper::FromPixels(175));termGrid.ColumnDefinitions().Append(stub);for(std::size_t i=0;i<state.models.size();++i){auto column=Controls::ColumnDefinition();column.Width(GridLengthHelper::FromPixels(180));termGrid.ColumnDefinitions().Append(column);}for(std::size_t r=0;r<=state.termRows.size();++r){auto row=Controls::RowDefinition();row.Height(GridLengthHelper::FromValueAndType(1,GridUnitType::Auto));termGrid.RowDefinitions().Append(row);}
            for(std::size_t i=0;i<state.models.size();++i){auto header=TableCell(to_hstring(state.models[i].label).c_str(),true,false);Controls::Grid::SetColumn(header,(int)i+1);termGrid.Children().Append(header);auto menu=Controls::MenuFlyout();auto title=Controls::MenuFlyoutItem();title.Text(to_hstring(state.models[i].label));title.IsEnabled(false);menu.Items().Append(title);menu.Items().Append(Controls::MenuFlyoutSeparator());auto active=Controls::ToggleMenuFlyoutItem();active.Text(L"Active model");active.IsChecked((int)i==state.activeModel);active.Click([send,i](auto const&,auto const&)mutable{send("GCOMP_SET_ACTIVE",std::to_string(i));});menu.Items().Append(active);AttachDirectMenu(header,menu);}
            for(std::size_t r=0;r<state.termRows.size();++r)
            {
                auto term=state.termRows[r];auto name=TableCell(to_hstring(term).c_str(),false,true);Controls::Grid::SetRow(name,(int)r+1);termGrid.Children().Append(name);
                auto menu=Controls::MenuFlyout();auto title=Controls::MenuFlyoutItem();title.Text(to_hstring(term));title.IsEnabled(false);menu.Items().Append(title);menu.Items().Append(Controls::MenuFlyoutSeparator());auto models=Controls::MenuFlyoutSubItem();models.Text(L"Models");for(std::size_t m=0;m<state.models.size();++m){const bool included=GeneralizedModelIncludesTerm(state.models[m],term);auto item=Controls::ToggleMenuFlyoutItem();item.Text(to_hstring(state.models[m].label));item.IsChecked(included);item.Click([send,m,term](auto const&,auto const&)mutable{send("GCOMP_TOGGLE_TERM",std::to_string(m)+'\x1f'+term);});models.Items().Append(item);}menu.Items().Append(models);AttachDirectMenu(name,menu);
                for(std::size_t m=0;m<state.models.size();++m){const bool included=GeneralizedModelIncludesTerm(state.models[m],term);auto toggle=Controls::CheckBox();toggle.Content(box_value(included?L"Included":L"Add"));toggle.IsChecked(included);toggle.FontSize(11);toggle.HorizontalAlignment(HorizontalAlignment::Stretch);toggle.Click([send,m,term](auto const&,auto const&)mutable{send("GCOMP_TOGGLE_TERM",std::to_string(m)+'\x1f'+term);});Controls::Grid::SetRow(toggle,(int)r+1);Controls::Grid::SetColumn(toggle,(int)m+1);termGrid.Children().Append(toggle);}
            }
            content_.Children().Append(termGrid);
        }
        auto heading = FieldLabel(L"Model fit and adjacent-model tests");
        heading.FontSize(13);
        heading.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        content_.Children().Append(heading);

        auto grid = Controls::Grid();
        auto stub = Controls::ColumnDefinition();
        stub.Width(GridLengthHelper::FromPixels(175));
        grid.ColumnDefinitions().Append(stub);
        for (std::size_t i = 0; i < state.models.size(); ++i)
        {
            auto column = Controls::ColumnDefinition();
            column.Width(GridLengthHelper::FromPixels(180));
            grid.ColumnDefinitions().Append(column);
        }
        auto addRow = [&]()
        {
            auto row = Controls::RowDefinition();
            row.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Auto));
            grid.RowDefinitions().Append(row);
        };
        auto add = [&](FrameworkElement const& cell, int row, int column)
        {
            Controls::Grid::SetRow(cell, row);
            Controls::Grid::SetColumn(cell, column);
            grid.Children().Append(cell);
        };
        addRow();
        auto corner = TableCell(L"", true, true);
        add(corner, 0, 0);
        for (std::size_t i = 0; i < state.models.size(); ++i)
        {
            auto header = TableCell(to_hstring(state.models[i].label).c_str(), true, false);
            add(header, 0, static_cast<int>(i) + 1);
            auto menu=Controls::MenuFlyout();auto title=Controls::MenuFlyoutItem();title.Text(to_hstring(state.models[i].label));title.IsEnabled(false);menu.Items().Append(title);menu.Items().Append(Controls::MenuFlyoutSeparator());auto active=Controls::ToggleMenuFlyoutItem();active.Text(L"Active model");active.IsChecked((int)i==state.activeModel);active.Click([send,i](auto const&,auto const&)mutable{send("GCOMP_SET_ACTIVE",std::to_string(i));});menu.Items().Append(active);AttachDirectMenu(header,menu);
        }

        std::vector<std::string> labels;
        if (state.binaryComparison)
            labels = {"N", "log likelihood", "AIC", "BIC", "AUC", "\xCE\x94 deviance", "df", "p"};
        else
            labels = {"N", "log likelihood", "AIC", "BIC", "\xCE\x94 deviance", "df", "p"};
        const std::string dash = "\xE2\x80\x94";
        for (std::size_t rowIndex = 0; rowIndex < labels.size(); ++rowIndex)
        {
            addRow();
            const auto& label = labels[rowIndex];
            auto labelCell=TableCell(to_hstring(label).c_str(), false, true, false, false, false, rowIndex > 0);
            add(labelCell, static_cast<int>(rowIndex) + 1, 0);
            AttachDirectMenu(labelCell,CreateStructuralCellMenu(to_hstring(label).c_str(),labelCell.Child().as<Controls::TextBlock>()));
            for (std::size_t modelIndex = 0; modelIndex < state.models.size(); ++modelIndex)
            {
                auto const& model = state.models[modelIndex];
                std::string value;
                if (label == "N") value = std::to_string(model.fit.n);
                else if (label == "log likelihood") value = FormatDoubleOrDash(model.fit.logLik, 2);
                else if (label == "AIC") value = FormatDoubleOrDash(model.fit.aic, 2);
                else if (label == "BIC") value = FormatDoubleOrDash(model.fit.bic, 2);
                else if (label == "AUC") value = FormatDoubleOrDash(model.fit.auc, 3);
                else if (label == "\xCE\x94 deviance") value = model.comparisonOk ? FormatDoubleOrDash(model.comparisonStatistic, 3) : dash;
                else if (label == "df") value = model.comparisonOk ? std::to_string(model.comparisonDf) : dash;
                else value = model.comparisonOk ? FormatPValue(model.comparisonP) : dash;
                auto valueCell=TableCell(to_hstring(value).c_str(), false, false, false, false, false, rowIndex > 0);
                add(valueCell, static_cast<int>(rowIndex) + 1, static_cast<int>(modelIndex) + 1);
                AttachDirectMenu(valueCell,CreateStatisticCellMenu(to_hstring(label).c_str(),valueCell.Child().as<Controls::TextBlock>()));
            }
        }
        content_.Children().Append(grid);
        for (std::size_t i = 1; i < state.models.size(); ++i)
        {
            if (state.models[i].fit.status.empty()) continue;
            auto note = FieldLabel(to_hstring(state.models[i].label + ": " + state.models[i].fit.status).c_str());
            note.FontSize(10.5); note.Foreground(Brush(92,92,92)); note.TextWrapping(TextWrapping::Wrap);
            content_.Children().Append(note);
        }
        auto footer = FieldLabel(L"Likelihood-ratio tests are shown only for adjacent nested models fitted to identical rows with the same distribution, link, and response bounds.");
        footer.FontSize(10.5); footer.Foreground(Brush(92,92,92)); footer.TextWrapping(TextWrapping::Wrap);
        content_.Children().Append(footer);
    }
    void GeneralizedComparisonView::Activate(){if(!closed_)window_.Activate();}void GeneralizedComparisonView::Close(){if(!closed_)window_.Close();}
}
