#pragma once
#include "../../../../core/missing_data_model.h"

#include "../../../../core/analysis_scope.h"
#include <winrt/Microsoft.UI.Xaml.h>
#include <winrt/Microsoft.UI.Xaml.Controls.h>

#include <functional>
#include <cstdint>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace winrt::rlispstatWinUI::implementation
{
    enum class ApplicationCommandId
    {
        CloseContextWindow,
        CloseAllWindows,
        CloseDataFileAndAnalyses,
        OpenData,
        SaveData,
        SaveDataAs,
        ExportData,
        ImportData,
        OpenDataFromR,
        ReturnDataToR,
        ReturnSelectedRowsToR,
        ShowWelcome,
        ShowDataSheet,
        ShowVariableView,
        ChooseLabelColumn,
        SetSelectedColor,
        ResetSelectedColor,
        AddSelectionColumn,
        AddPointColorColumn,
        MakeSubsetFromSelection,
        ClearSelection,
        SelectAll,
        InvertSelection,
        OpenColorPalette,
        RunMultipleImputation,
        ShowMissingDataPatterns,
        ShowMissingnessModels,
        ShowImputationDiagnostics,
        ShowAnalysisScopePanel,
        UseSelectionScope,
        UseUnselectedScope,
        SaveSelectionScope,
        ApplySavedScope,
        UseAllScope,
        NewScatterplot,
        NewTrellisPlot,
        NewTrellisTimeSeries,
        NewTrellisBoxplot,
        NewTrellisBarChart,
        NewTrellisHistogram,
        NewTimeSeries,
        NewHistogram,
        NewScatterMatrix,
        NewParallelCoordinates,
        NewBoxplot,
        NewBarChart,
        AnalyzeTable1,
        AnalyzeContingencyTable,
        AnalyzeCorrelationMatrix,
        AnalyzeDimensionality,
        AnalyzeFactorAnalysis,
        AnalyzeScaleAnalysis,
        AnalyzeQuickCluster,
        AnalyzeOneSampleT,
        AnalyzeIndependentT,
        AnalyzePairedT,
        AnalyzeOneWayAnova,
        AnalyzeLinearModel,
        AnalyzeLinearModelTrellis,
        AnalyzeRegressionComparison,
        AnalyzeBinaryRegression,
        AnalyzeBinaryRegressionComparison,
        AnalyzeCountRegression,
        AnalyzeCountRegressionComparison,
        AnalyzePositiveContinuousModel,
        AnalyzePositiveContinuousComparison,
        AnalyzeProportionModel,
        AnalyzeProportionComparison,
        AnalyzeGeneralizedLinearModel,
        AnalyzeGeneralizedComparison,
        AnalyzeLinearMixedModel,
        AnalyzeGeneralizedMixedModel,
        PlotThemeClassic,
        PlotThemeMinimal,
        PlotThemeBlackWhite,
        PlotThemeGray,
        PlotThemeCowplot,
        PlotThemeIpsum,
        PlotThemeTq,
        PlotThemeModern,
        PlotThemeTufte,
        PlotThemeEconomist,
        PlotThemeFiveThirtyEight,
        PlotThemeManet,
        PlotThemeVista,
        PlotThemeBeige,
        PlotThemeDataDesk,
        PlotThemeGarish,
        PlotThemePublication,
        InterfaceFontInter,
        InterfaceFontSourceSans3,
        InterfaceFontIBMPlexSans,
        InterfaceFontAptos,
        InterfaceFontCalibri,
        InterfaceFontSegoeUI,
        InterfaceFontTahoma,
        InterfaceSizeCompact,
        InterfaceSizeStandard,
        InterfaceSizeLarge,
        ShowSnapshotAlbum,
        AddSnapshot,
        Documentation,
        About,
        QuitApplication
    };

    struct ApplicationCommandItem
    {
        ApplicationCommandId id{};
        std::wstring label;
        bool enabled = false;
        std::wstring disabledReason;
        std::wstring groupLabel;
        bool checkable = false;
        bool checked = false;
        std::string value;
        std::wstring subgroupLabel;
    };

    struct ApplicationMenuSection
    {
        std::wstring title;
        std::vector<ApplicationCommandItem> commands;
    };

    struct ApplicationWindowItem
    {
        std::string token;
        std::wstring title;
        bool active = false;
    };

    struct ApplicationMenuSnapshot
    {
        std::vector<ApplicationMenuSection> sections;
        std::vector<ApplicationWindowItem> windows;
    };

    class ApplicationCommandRegistry
    {
    public:
        using Dispatch = std::function<std::string(std::vector<std::string> const&)>;
        using DatasetExists = std::function<bool(std::string const&)>;
        using SelectionExists = std::function<bool(std::string const&)>;
        using CurrentInterfaceFont = std::function<std::wstring()>;
        using CurrentInterfaceSize = std::function<std::wstring()>;
        using CurrentPlotTheme = std::function<std::string()>;
        using CurrentAnalysisScope = std::function<::rlispstat::core::AnalysisScope(
            std::string const&)>;
        using CurrentSavedSelections = std::function<std::vector<::rlispstat::core::SavedSelection>(
            std::string const&)>;
        using CurrentSelectedColor = std::function<std::string(std::string const&)>;
        using Observer = std::function<bool()>;

        struct WindowContext
        {
            std::string token;
            std::wstring title;
            std::string group;
            std::string kind;
            std::string plotId;
            std::function<void()> activate;
            std::function<void()> close;
            std::function<void()> newScatterplot;
            std::function<void()> newScatterMatrix;
            std::function<void()> newParallelCoordinates;
            std::function<void()> descriptiveStatistics;
            std::function<void()> linearModelTrellis;
            std::function<void()> contingencyTable;
        };

        ApplicationCommandRegistry(Dispatch dispatch, DatasetExists datasetExists,
                                   SelectionExists selectionExists,
                                   CurrentInterfaceFont currentInterfaceFont,
                                   CurrentInterfaceSize currentInterfaceSize,
                                   CurrentPlotTheme currentPlotTheme,
                                   CurrentAnalysisScope currentAnalysisScope,
                                   CurrentSavedSelections currentSavedSelections,
                                   CurrentSelectedColor currentSelectedColor);
        void SetMissingDataCapabilities(std::function<::rlispstat::core::MissingDataCapabilities(std::string const&)> provider) { missingDataCapabilities_=std::move(provider); }
        void RegisterWindow(WindowContext context);
        void UnregisterWindow(std::string const& token);
        void ClearWindows();
        void CloseDatasetWindows(std::string const& group);
        void ActivateContext(std::string const& token);
        ApplicationMenuSnapshot Snapshot(std::string const& contextToken) const;
        void Execute(ApplicationCommandId command, std::string const& contextToken,
                     std::string const& value = {});
        void ActivateWindow(std::string const& token);
        void AddObserver(Observer observer);
        void RefreshCommandState();
        std::vector<::rlispstat::core::SavedSelection> SavedSelections(
            std::string const& group) const;

    private:
        WindowContext const* Context(std::string const& token) const;
        void NotifyObservers();

        std::function<::rlispstat::core::MissingDataCapabilities(std::string const&)> missingDataCapabilities_;
        Dispatch dispatch_;
        DatasetExists datasetExists_;
        SelectionExists selectionExists_;
        CurrentInterfaceFont currentInterfaceFont_;
        CurrentInterfaceSize currentInterfaceSize_;
        CurrentPlotTheme currentPlotTheme_;
        CurrentAnalysisScope currentAnalysisScope_;
        CurrentSavedSelections currentSavedSelections_;
        CurrentSelectedColor currentSelectedColor_;
        std::map<std::string, WindowContext> windows_;
        std::vector<Observer> observers_;
        std::string activeToken_;
    };

    class DataSheetMenuHost : public std::enable_shared_from_this<DataSheetMenuHost>
    {
    public:
        static std::shared_ptr<DataSheetMenuHost> Create();
        winrt::Microsoft::UI::Xaml::FrameworkElement Element() const;
        void SetRegistry(std::shared_ptr<ApplicationCommandRegistry> registry,
                         std::string contextToken);
        void RebuildMenu();

    private:
        DataSheetMenuHost();

        void RequestRebuild();
        void OnMenuOpened();
        void OnMenuClosed();
        winrt::Microsoft::UI::Xaml::Controls::MenuBar menuBar_{ nullptr };
        std::shared_ptr<ApplicationCommandRegistry> registry_;
        std::string contextToken_;
        unsigned int openMenuCount_ = 0;
        bool rebuildPending_ = false;
        bool rebuildQueued_ = false;
        bool hasLastSnapshot_ = false;
        ApplicationMenuSnapshot lastSnapshot_;
        std::uint64_t menuGeneration_ = 0;
    };
}
