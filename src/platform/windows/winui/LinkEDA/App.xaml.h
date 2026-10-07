#pragma once

#include "App.xaml.g.h"
#include "ApplicationMenu.h"
#include "DataSheetView.h"
#include "ScatterPlotView.h"
#include "WelcomeWindow.h"
#include "WorkflowWindows.h"
#include "../../../../core/command_dispatcher.h"
#include "../../../../core/plot_coordinator.h"
#include "../../../../core/snapshot_album_model.h"
#include "../../../../core/linkeda_document.h"

#include <atomic>
#include <cstdint>
#include <filesystem>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace winrt::LinkEDA::implementation
{
    struct App : AppT<App>
    {
        App();

        void OnLaunched(Microsoft::UI::Xaml::LaunchActivatedEventArgs const&);
        winrt::fire_and_forget StartTcpListener();
        std::string DispatchRequest(std::vector<std::string> const& lines);
        bool HasPendingMainRTasks() const noexcept;
        void ShowScatterPlot(::rlispstat::core::SessionPlot const& plot);
        void ShowNativePlot(::rlispstat::core::PlotModel& plot);
        void ShowDataSheet(std::string const& group);

    private:
        void InitializeDispatcher();
        void ShowWelcome(std::string const& version);
        void HideWelcome();
        void ChooseWelcomeFile();
        void ChooseNativeDocumentFile();
        void ChooseSaveDocumentFile(std::string const& group);
        std::optional<std::string> ChooseSaveDocumentPath(std::string const& group);
        void ChooseExportDataFile(std::string const& group);
        bool BuildNativeDocumentPayload(std::string const& group,
                                        std::vector<unsigned char>& payload,
                                        std::string& error,
                                        std::vector<unsigned char>* dataChangePayload = nullptr) const;
        void RecordInitialDataBaseline(std::string const& group);
        bool SaveNativeDocumentBlocking(std::string const& group,
                                        std::string const& path);
        void ShowAbout();
        void QuitApplication();
        winrt::fire_and_forget ImportDataFileNative(std::string path,
                                                    std::string sourcePath);
        void ShowImportColumnChooser(std::string stagedDataset,
                                     std::string information,
                                     std::vector<std::string> variables);
        void ShowNativeImportColumnChooser(::rlispstat::core::DataFrameModel dataframe,
                                           std::string sourcePath);
        winrt::fire_and_forget OpenNativeDocument(std::string path);
        winrt::fire_and_forget SaveNativeDocument(std::string group,
                                                  std::string path);
        void NoteWelcomeRecentPath(std::string const& path);
        void QueueWelcomeExample(std::string const& example);
        std::string ShowRDataFrameChooser(std::vector<std::string> const& command);
        bool ApplySelectedRows(std::string const& group, std::set<int> const& rows,
                               ::rlispstat::core::SelectionMode mode);
        void SetImputationDisplayMode(std::string const& group,
                                      std::string const& mode);
        void HandleViewClosed(std::string const& plotId);
        void HandleDataSheetClosed(std::string const& group);
        void ShowVariablesWindow(std::string const& group, bool activate = true);
        void HandleVariablesWindowClosed(std::string const& group);
        void HandleVariableTypeChanged(
            std::string const& group, std::string const& variable,
            std::string const& type,
            ::rlispstat::core::VariableTypeChangeEffects const& effects);
        void HandleDatasetMutation(
            ::rlispstat::core::DatasetMutationEvent const& event);
        bool QueueDatasetSyncToR(::rlispstat::core::DataFrameModel const& dataframe);
        ::rlispstat::core::PlotCoordinationResult AttachPlotModel(
            ::rlispstat::core::PlotModel& model);
        void HandleOutputClosed(std::string const& resultId);
        void OpenScatterplotDialog(std::string const& group);
        void OpenPlotWorkflowDialog(std::string const& group, PlotWorkflowKind kind);
        void OpenTrellisPlotDialog(std::string const& group, std::string const& plotType);
        void CreatePlotWorkflow(PlotWorkflowSpecification const& specification);
        void OpenAnalysisWorkflowDialog(std::string const& group, AnalysisWorkflowKind kind);
        bool StartAnalysis(std::string const& group, AnalysisWorkflowKind kind);
        void QueueAnalysisWorkflow(AnalysisWorkflowSpecification const& specification);
        void HandleAnalysisViewCommand(std::vector<std::string> const& command);
        void ShowColorPalette(std::string const& group);
        void ShowAnalysisScopePanel(std::string const& group);
        void RefreshAnalysisScopePanel();
        winrt::fire_and_forget SaveCurrentSelectionAsScopeAsync(
            std::string group,
            winrt::Microsoft::UI::Xaml::FrameworkElement owner);
        void ShowSnapshotAlbum();
        void RefreshSnapshotAlbum();
        winrt::fire_and_forget RenameSnapshotAsync(
            std::string snapshotId,
            winrt::Microsoft::UI::Xaml::FrameworkElement owner);
        void UpdateSnapshotAlbumNotesPanelWidth();
        void ExportSnapshotAlbum();
        winrt::fire_and_forget ExportSnapshotAlbumSvgAsync(std::filesystem::path folder);
        winrt::fire_and_forget ExportSnapshotAlbumPngAsync(std::filesystem::path folder);
        winrt::fire_and_forget ExportSnapshotAlbumPdfAsync(std::wstring path);
        winrt::Windows::Foundation::IAsyncOperation<
            winrt::Windows::Storage::Streams::InMemoryRandomAccessStream>
            RenderSnapshotNativePlotToPngAsync(std::string snapshotId,
                                               double width,
                                               double height,
                                               double scale = 3.125);
        winrt::Windows::Foundation::IAsyncOperation<
            winrt::Windows::Storage::Streams::InMemoryRandomAccessStream>
            RenderSnapshotSvgToPngAsync(std::string svg, double width, double height);
        winrt::fire_and_forget AddActiveSnapshot(
            std::string kind, std::string sourceId, std::string group);
        void QueueLinearModelFit(std::string const& group, bool force = false);
        void RefitLinearModel(std::string const& group);
        void RefitMeanComparison(std::string const& id);
        void OpenDescriptiveStatisticsDialog(std::string const& group);
        void OpenContingencyTableDialog(
            std::string const& group,
            ::rlispstat::core::AnalysisSpecification const* existingSpecification = nullptr);
        void OpenMissingDataImputationDialog(std::string const& group);
        void OpenChooseLabelColumnDialog(std::string const& group);
        winrt::fire_and_forget OpenMissingDataPatternsDialog(std::string group, bool models=false);
        winrt::fire_and_forget QueueMultipleImputation(
            MissingDataImputationSpecification specification);
        void OpenRDataAssignmentDialog(std::string const& group, bool selectedRowsOnly);
        void QueueRDataAssignment(RDataAssignmentSpecification const& specification);
        void CreateContingencyTable(ContingencyTableSpecification const& specification);
        void CreateScatterplot(ScatterplotSpecification const& specification);
        std::string CreateScatterMatrix(std::string const& group,
                                        std::vector<std::string> const& variables = {});
        std::string CreateParallelCoordinates(std::string const& group,
                                              std::vector<std::string> const& variables = {},
                                              bool standardize = true, bool connect = true);
        void RefreshAnalysisScopeIndicators(std::string const& group);
        bool RebuildExploratoryPlotForScope(
            ::rlispstat::core::PlotModel& plot,
            ::rlispstat::core::DataFrameModel const& dataframe,
            ::rlispstat::core::AnalysisScope const& scope);
        void RefreshOpenPlotsForScope(std::string const& group,
                                      bool selectionChanged = false);
        void RefitOpenAnalysesForScope(std::string const& group);
        void QueueDescriptiveStatistics(DescriptiveStatisticsSpecification const& specification);
        void ShowTable1(::rlispstat::core::Table1DisplayState const& state);
        void ShowGLMInteractionReport(
            std::string const& id,
            std::string const& datasetId,
            std::string const& title,
            std::string const& subtitle,
            ::rlispstat::core::GLMInteractionReport const& report);
        bool OpenPooledRegressionInteractionPlot(
            std::string const& group,
            std::string const& analysisId,
            std::string const& response,
            std::string const& term,
            std::vector<std::string> const& payload,
            ::rlispstat::core::AnalysisScope const& computedScope,
            bool scopeCaptured,
            std::string& message);
        bool OpenPooledRegressionPartialPlot(
            std::string const& group,
            std::string const& analysisId,
            std::string const& term,
            std::vector<std::string> const& payload,
            ::rlispstat::core::AnalysisScope const& computedScope,
            bool scopeCaptured,
            std::string& message);
        void HandleGLMInteractionReportClosed(std::string const& id);
        void ShowCorrelationMatrix(std::string const& id);
        void HandleCorrelationClosed(std::string const& id);
        void ShowMeanComparison(::rlispstat::core::MeanComparisonState const& state);
        void HandleMeanComparisonClosed(std::string const& id);
        void ShowLinearModel(std::string const& group);
        void HandleLinearModelClosed(std::string const& group);
        void ShowDimensionality(std::string const& id);
        void HandleDimensionalityClosed(std::string const& id);
        bool RefitDimensionality(std::string const& id);
        void OpenDimensionalityPlot(std::string const& id, std::string const& kind);
        void RefreshDimensionalityPlots(std::string const& id);
        void ShowScaleAnalysis(std::string const& id);
        void HandleScaleAnalysisClosed(std::string const& id);
        bool RefitScaleAnalysis(std::string const& id);
        void QueueCorrelationFit(::rlispstat::core::CorrelationMatrixState& state);
        bool RefitDendrogram(std::string const& id);
        void ShowDendrogram(std::string const& id);
        void HandleDendrogramClosed(std::string const& id);
        void ShowGeneralizedModel(std::string const& id);
        void HandleGeneralizedModelClosed(std::string const& id);
        bool RequestGeneralizedModelFit(std::string const& id);
        void ShowMixedModel(::rlispstat::core::NativeMixedModelState const& state);
        void ShowMixedModelText(std::string const& id, std::string const& group,
                                std::string const& modelType, std::string const& text);
        void HandleMixedModelClosed(std::string const& id);
        void ShowRegressionComparison(std::string const& id);
        void QueueRegressionComparison(::rlispstat::core::RegressionComparisonState& state);
        void HandleRegressionComparisonClosed(std::string const& id);
        std::string CreateModelTrellis(std::string const& group,
                                       std::string const& response = {},
                                       std::vector<std::string> const& terms = {},
                                       std::string const& rowCondition = {},
                                       std::string const& columnCondition = {},
                                       std::string const& pAdjustment = "holm",
                                       std::string const& requestedScope = "all");
        void QueueModelTrellisFit(::rlispstat::core::ModelTrellisState& state);
        void ShowModelTrellis(std::string const& id);
        void HandleModelTrellisClosed(std::string const& id);
        void ShowGeneralizedComparison(std::string const& id);
        void QueueGeneralizedComparison(::rlispstat::core::GeneralizedComparisonState& state);
        void HandleGeneralizedComparisonClosed(std::string const& id);
        std::string OpenLinearModelDiagnostic(std::string const& group, std::string const& kind);
        std::string OpenGeneralizedModelDiagnostic(std::string const& id, std::string const& kind);
        std::string OpenRegressionComparisonDiagnostic(std::string const& id, int modelIndex,
                                                        std::string const& kind);
        std::string OpenGeneralizedComparisonDiagnostic(std::string const& id, int modelIndex,
                                                         std::string const& kind);
        std::string AddDiagnosticPlot(std::unique_ptr<::rlispstat::core::PlotModel> model);
        void RefreshDiagnosticPlots();
        struct RegressionDerivedOutputDependency
        {
            std::string dependencyId;
            std::string outputId;
            std::string outputKind;
            std::string sourceKind;
            std::string sourceModelId;
            std::vector<std::string> refreshCommand;
            int sourceRevision = 0;
            int sourceFitVersion = 0;
            std::uint64_t requestGeneration = 0;
        };
        bool RegressionDerivedSourceIdentity(
            RegressionDerivedOutputDependency const& dependency,
            int& revision, int& fitVersion, bool& accepted) const;
        RegressionDerivedOutputDependency BeginRegressionDerivedOutputRequest(
            std::string outputId, std::string outputKind,
            std::string sourceKind, std::string sourceModelId,
            std::vector<std::string> refreshCommand);
        bool RegressionDerivedOutputRequestIsCurrent(
            RegressionDerivedOutputDependency const& dependency) const;
        bool RegressionDerivedOutputHasConsumer(
            RegressionDerivedOutputDependency const& dependency) const;
        void ClearRegressionDerivedOutput(
            RegressionDerivedOutputDependency const& dependency,
            std::string const& message);
        void RefreshRegressionDerivedOutputs(std::string const& sourceModelId);
        void ForgetRegressionDerivedOutput(
            std::string const& outputId, std::string const& outputKind);
        void RefreshSelectionVisuals(
            ::rlispstat::core::PlotCoordinationEvent const& event);
        void QueueSmoothRecompute(::rlispstat::core::PlotModel& model,
                                  ::rlispstat::core::SmoothCurveScope scope,
                                  std::string const& fitMethod = "");
        void QueueExistingSmoothRecompute(::rlispstat::core::PlotModel& model);
        void QueueCommandStateRefresh();
        void MarkMainRTaskPending() noexcept;
        void ResetAllViews();
        void LoadInterfaceFontPreference();
        bool ApplyInterfaceFont(std::string const& fontName, bool persist);
        void LoadInterfaceSizePreference();
        bool ApplyInterfaceSize(std::string const& sizeName, bool persist);
        bool PreparePersistentColorsForManualEdit(std::string const& group);
        std::string DispatchApplicationCommand(std::vector<std::string> const& command);
        winrt::Windows::Networking::Sockets::StreamSocketListener listener_{ nullptr };
        winrt::Microsoft::UI::Dispatching::DispatcherQueue dispatcherQueue_{ nullptr };
        winrt::Microsoft::UI::Xaml::Window technicalOwner_{ nullptr };
        winrt::Microsoft::UI::Xaml::Window paletteWindow_{ nullptr };
        winrt::Microsoft::UI::Xaml::Window analysisScopeWindow_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::TextBlock analysisScopeSummary_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::ScrollViewer analysisScopeScroll_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::StackPanel analysisScopeOptions_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::Button analysisScopeSave_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::Button analysisScopeIncludeAll_{ nullptr };
        winrt::Microsoft::UI::Xaml::Window snapshotAlbumWindow_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::TabView snapshotAlbumTabs_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::TextBlock snapshotAlbumEmpty_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::WebView2 snapshotAlbumExportWebView_{ nullptr };
        std::map<std::string, std::shared_ptr<ScatterPlotView>> snapshotAlbumPlotViews_;
        std::string snapshotAlbumSelectionId_;
        bool snapshotAlbumNotesExpanded_ = false;
        std::string paletteGroup_;
        std::string analysisScopeGroup_;
        ::rlispstat::core::SnapshotAlbum snapshotAlbum_;
        std::unique_ptr<::rlispstat::core::CommandDispatcher> commandDispatcher_;
        std::shared_ptr<ApplicationCommandRegistry> applicationCommands_;
        std::shared_ptr<WelcomeWindow> welcomeWindow_;
        std::shared_ptr<RDataFrameChooserWindow> rDataChooser_;
        std::shared_ptr<RDataFrameChooserWindow> exampleChooser_;
        std::string welcomeVersion_ = "unknown";
        std::vector<std::string> welcomeRecentPaths_;
        std::map<std::string, std::string> documentPaths_;
        std::map<std::string, std::vector<unsigned char>> documentBaselines_;
        std::map<std::string, std::unique_ptr<::rlispstat::core::PlotModel>> plots_;
        std::map<std::string, std::shared_ptr<ScatterPlotView>> views_;
        std::map<std::string, std::shared_ptr<DataSheetView>> dataSheets_;
        std::shared_ptr<DatasetVariableDialog> importColumnsDialog_;
        std::map<std::string, std::shared_ptr<VariablesWindow>> variableViews_;
        std::map<std::string, std::shared_ptr<ScatterplotDialog>> scatterDialogs_;
        std::map<std::string, std::shared_ptr<PlotWorkflowDialog>> plotWorkflowDialogs_;
        std::map<std::string, std::shared_ptr<AnalysisWorkflowDialog>> analysisWorkflowDialogs_;
        std::map<std::string, std::shared_ptr<DescriptiveStatisticsDialog>> descriptiveDialogs_;
        std::map<std::string, std::shared_ptr<ContingencyTableDialog>> contingencyDialogs_;
        std::map<std::string, std::shared_ptr<MissingDataImputationDialog>> imputationDialogs_;
        std::map<std::string, std::shared_ptr<DatasetVariableDialog>> datasetVariableDialogs_;
        std::map<std::string, std::shared_ptr<RDataAssignmentDialog>> rDataAssignmentDialogs_;
        std::map<std::string, std::shared_ptr<AnalysisOutputView>> outputViews_;
        std::map<std::string, ::rlispstat::core::Table1DisplayState> outputStates_;
        std::map<std::string, std::shared_ptr<GLMInteractionReportView>>
            interactionReportViews_;
        std::map<std::string, std::shared_ptr<CorrelationMatrixView>> correlationViews_;
        std::map<std::string, std::shared_ptr<MeanComparisonView>> meanComparisonViews_;
        std::map<std::string, std::shared_ptr<MeanComparisonView>> meanComparisonDescriptiveViews_;
        std::map<std::string, ::rlispstat::core::MeanComparisonState> meanComparisonStates_;
        std::map<std::string, std::shared_ptr<LinearModelView>> linearModelViews_;
        std::map<std::string, std::shared_ptr<DimensionalityView>> dimensionalityViews_;
        std::map<std::string, std::shared_ptr<ScaleAnalysisView>> scaleAnalysisViews_;
        std::map<std::string, std::shared_ptr<DendrogramView>> dendrogramViews_;
        std::map<std::string, std::shared_ptr<GeneralizedModelView>> generalizedModelViews_;
        std::map<std::string, std::shared_ptr<MixedModelView>> mixedModelViews_;
        std::map<std::string, std::shared_ptr<RegressionComparisonView>> regressionComparisonViews_;
        std::map<std::string, std::shared_ptr<ModelTrellisView>> modelTrellisViews_;
        std::map<std::string, std::shared_ptr<GeneralizedComparisonView>> generalizedComparisonViews_;
        std::map<std::string, RegressionDerivedOutputDependency>
            regressionDerivedOutputs_;
        std::uint64_t nextRegressionDerivedRequestGeneration_ = 0;
        std::set<std::string> datasetRecoveryPending_;
        ::rlispstat::core::MainRTaskBatch pendingMainRTasks_;
        std::atomic_bool pendingMainRTasksAvailable_{ false };
        std::atomic_bool mainRSessionAvailable_{ false };
        bool resettingViews_ = false;
        bool commandStateRefreshQueued_ = false;
        bool quitting_ = false;
        std::string interfaceFont_ = "Inter";
        std::string interfaceSize_ = "compact";
    };
}
