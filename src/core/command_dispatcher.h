#ifndef RLISPSTAT_CORE_COMMAND_DISPATCHER_H
#define RLISPSTAT_CORE_COMMAND_DISPATCHER_H

#include "application_state.h"
#include "command_model.h"
#include "format_model.h"
#include "main_r_task_model.h"
#include "mean_comparison_model.h"
#include "plot_coordinator.h"
#include "plot_geometry.h"
#include "selection_model.h"
#include "session_controller.h"
#include "table1_model.h"

#include <functional>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

// Semantic presentation of a frozen linear fit, shared by native and R exports.
std::vector<PublicationTableSpec> LinearModelReportTables(const GroupModelState &state,
                                                        const GLMFitSummary &fit);
OutputCodeReference LinearModelCodeReference(const GroupModelState &state,
                                            const GLMFitSummary &fit);

PublicationTableSpec GeneralizedModelPublicationTable(const GeneralizedGLMState &state);
std::vector<PublicationTableSpec> GeneralizedModelReportTables(const GeneralizedGLMState &state);
PublicationTableSpec RegressionComparisonPublicationTable(const RegressionComparisonState &state);
PublicationTableSpec GeneralizedComparisonPublicationTable(const GeneralizedComparisonState &state);

using CommandBoxplotMutation = std::function<bool(PlotModel &, std::string &)>;

struct CommandVariableInfo {
    std::string name;
    std::string type;
    std::string role;
};

enum class DatasetMutationKind {
    CellValue,
    VariableRename,
    VariableMetadata,
    ColumnStructure,
    LabelColumn
};

struct DatasetMutationEvent {
    DatasetMutationKind kind = DatasetMutationKind::VariableMetadata;
    std::string group;
    std::string variable;
    std::string previousVariable;
    int row = 0;
    DatasetValueChangeEffects valueChangeEffects;
};

struct CommandQueryServices {
    std::function<std::vector<PlotModel>()> listPlots;
    std::function<bool(const std::string &, PlotModel &, std::size_t &)> plotWithSelection;
    std::function<bool(const std::string &, PlotModel &)> plot;
    std::function<std::vector<GroupSelectionSummary>()> groups;
    std::function<bool(const std::string &, std::size_t &, std::vector<std::string> &)> groupInfo;
    std::function<bool(const std::string &, std::vector<std::pair<int, std::string>> &)> pointColors;
    std::function<bool(const std::string &, std::vector<CommandVariableInfo> &)> variableInfo;
    std::function<bool(const std::string &, PlotModel &)> groupPlot;
    std::function<bool(const std::string &, PlotModel &)> groupSeed;
    std::function<bool(const std::string &)> groupIsMultipleImputation;
    std::function<std::string(const std::string &)> createGeneralizedGLMId;
    std::function<bool(PlotModel &)> activePlot;
};

struct CommandUiServices {
    std::function<bool(const std::string &, const std::string &, const std::string &)> exportPlot;
    std::function<bool(const std::string &, const std::string &)> copyPlot;
    std::function<void()> refreshPlotTheme;
    std::function<void(const std::string &)> redrawPlot;
    std::function<bool(const std::string &)> closePlot;
    std::function<void(bool)> setPanelVisible;
    std::function<void(bool)> setPaletteVisible;
    std::function<void(const std::string &)> activeDatasetChanged;
    std::function<void(const std::string &)> openDataSheet;
    std::function<void(const std::string &)> showVariablesWindow;
    std::function<void()> resetWindowLayout;
    std::function<void()> showActiveDataset;
    std::function<bool(const std::string &, std::string &)> importDataFile;
    std::function<void(const std::string &, const std::string &, const std::string &,
                       const VariableTypeChangeEffects &)> datasetVariableTypeChanged;
    std::function<void(const std::string &, bool)> datasetRegistered;
    std::function<void()> closeAll;
    std::function<void(const std::string &)> refreshPlotTitle;
    std::function<void(const std::string &)> refreshModelGroup;
    std::function<void(const std::string &)> refreshModelTrellis;
    std::function<void(const std::string &)> showGeneralizedComparison;
    std::function<void(const std::string &)> showCorrelationMatrix;
    std::function<void(const std::string &)> showDimensionality;
    std::function<void(const std::string &)> refreshDimensionalityPlots;
    std::function<void(const MainRCompareMeansTask &)> queueCompareMeansTask;
    std::function<void(const std::string &)> refreshGeneralizedGLM;
    std::function<void(const std::string &)> showGeneralizedGLM;
    std::function<bool(const std::string &)> requestGeneralizedGLMFit;
    std::function<void(const NativeMixedModelState &)> showMixedModel;
    std::function<void(const std::string &, const std::string &, const std::string &, const std::string &)> showMixedModelText;
    std::function<std::string(const std::string &, const std::string &)> openGeneralizedGLMDiagnostic;
    std::function<void(const std::string &, const std::vector<int> &)> modelUpdated;
    std::function<void(const std::string &)> refreshRegressionComparison;
    std::function<void(const RegressionComparisonState &)> regressionComparisonUpdated;
    std::function<void(RegressionComparisonState &)> requestRegressionComparisonFit;
    std::function<void(const std::string &)> showRegressionComparison;
    std::function<bool(const std::string &, bool &)> regressionComparisonVisible;
    std::function<std::string(const std::string &, int, const std::string &)> openRegressionComparisonDiagnostic;
    std::function<void(const std::string &)> showLinearModel;
    std::function<std::string(const std::string &, const std::string &)> openLinearModelDiagnostic;
    std::function<std::string(const std::string &, const std::string &)> openLinearInteractionPlot;
    std::function<void(const std::string &)> dispatchRecordingCommand;
    std::function<void(PlotModel *)> addPlot;
    std::function<void(const std::string &, const std::string &, const std::string &)> showCompareMeansReport;
    std::function<void(const Table1DisplayState &)> showCompareMeansTable;
    std::function<void(const Table1DisplayState &)> showTable1;
    std::function<void(const MeanComparisonState &)> showMeanComparison;
    std::function<void(const std::string &, const std::string &)> showMeanComparisonError;
    // Appended to preserve the positional aggregate layout used by older
    // native adapters and focused dispatcher tests.
    std::function<void(const DatasetMutationEvent &)> datasetMutated;
    std::function<void(const AnalysisScopeChangeEvent &)> analysisScopeChanged;
    // Scale Analysis is appended so existing positional platform/test
    // initializers retain their layout while the shared semantic session owns
    // all statistical state.
    std::function<void(const std::string &)> showScaleAnalysis;
    std::function<void(const std::string &)> refreshScaleAnalysisPlots;
    std::function<void(const std::string &, const std::string &,
                       const std::optional<DataFrameModel> &)> showRCode;
    std::function<void(const std::string &)> showDendrogram;
};

struct CommandSelectionServices {
    std::function<bool(const std::string &, std::set<int> &)> visibleRows;
    std::function<void(const std::string &, const std::set<int> &, int, bool)> selectionChanged;
    std::function<void(const std::string &, const std::vector<int> &, bool, bool)> rowColorsChanged;
    std::function<bool(const std::string &, std::string &, std::string &)> plotModes;
    std::function<bool(const std::string &, const std::string &)> setInteractionMode;
    std::function<bool(const std::string &, const std::string &)> setSelectionMode;
    std::function<bool(const std::string &, const std::string &)> addLinearModelOverlay;
    std::function<bool(const std::string &)> clearOverlays;
    std::function<bool(const std::string &, SmoothCurveScope,
                       const std::vector<SmoothCurveData> &)> replaceSmoothCurves;
    std::function<bool(const std::string &, bool, const std::string &)> setAxisVariable;
    std::function<bool(const std::string &, const std::function<void(PlotModel &)> &)> mutateHistogram;
    std::function<bool(const std::string &, const std::function<void(PlotModel &)> &)> mutateBarplot;
    std::function<bool(const std::string &, const std::function<bool(PlotModel &)> &)> mutateScatterMatrix;
    std::function<bool(const std::string &, const CommandBoxplotMutation &, std::string &)> mutateBoxplot;
    std::function<bool(const std::string &)> refitCorrelation;
    std::function<bool(const std::string &)> refitDimensionality;
    std::function<bool(const std::string &)> refitDendrogram;
    std::function<bool(const std::string &, SmoothCurveScope)> toggleSmoothCurves;
    std::function<bool(const std::string &, const std::string &, SmoothCurveScope,
                       const std::vector<SmoothCurveData> &)> replaceTrellisSmoothCurves;
    // Appended so older aggregate initializers remain source-compatible.
    std::function<bool(const std::string &, const std::string &)> removeLinearModelOverlay;
    std::function<bool(const std::string &)> refitScaleAnalysis;
};

struct CommandDispatcherServices {
    SessionControllerServices session;
    CommandQueryServices queries;
    CommandUiServices ui;
    std::function<std::string(const CommandRequest &, const std::vector<std::string> &)> handle;
    CommandSelectionServices selection;
};

// Rebuilds the portable ggplot2 verification/publication reference after a
// native plot option changes. The statistical data stay in the captured data
// version; only the graph recipe and retained presentation specification are
// refreshed.
void RefreshBasicPlotCodeReference(ApplicationState &state, PlotModel &plot);

// Native descriptive windows also participate in the shared R verification
// workflow.  These helpers rebuild a portable public-R recipe from the exact
// captured specification and register it under the visible output id.
void RefreshCorrelationCodeReference(ApplicationState &state,
                                     CorrelationMatrixState &correlation);
std::optional<MainRDimensionalityTask> PrepareDimensionalityRTask(ApplicationState &application, DimensionalityState &state);
MainRCorrelationTask PrepareCorrelationRTask(ApplicationState &application, CorrelationMatrixState &state);
MainRDendrogramTask PrepareDendrogramRTask(ApplicationState &state, DendrogramState &dendrogram);
void RefreshDendrogramCodeReference(ApplicationState &state,
                                    DendrogramState &dendrogram);
void RefreshNativeTableCodeReference(ApplicationState &state,
                                     Table1DisplayState &table);

class CommandDispatcher {
public:
    explicit CommandDispatcher(CommandDispatcherServices services);
    void setMissingInformationRefresh(std::function<void(const Table1DisplayState &)> refresh) {
        missingInformationRefresh_ = std::move(refresh);
    }

    void prepareTable1Task(MainRTable1Task &task);
    void cancelTable1Task(const std::string &id);
    std::string dispatch(const std::vector<std::string> &lines);
    std::string dispatchSession(SessionReadLine readLine);
    bool sessionClosed() const;
    const std::string &plotTheme() const;
    ApplicationState &applicationState();
    const ApplicationState &applicationState() const;
    PlotCoordinator &plotCoordinator();
    const PlotCoordinator &plotCoordinator() const;

private:
    std::optional<std::string> dispatchPortable(const CommandRequest &request);

    struct PendingTable1 { MainRTable1Task task; bool pending = true; bool followsActiveScope = false; };
    std::map<std::string, PendingTable1> table1Requests_;
    std::uint64_t table1Revision_ = 0;
    bool currentTable1Reply(const std::string &id, const std::string &group,
                           std::uint64_t revision, std::uint64_t version) const;
    CommandDispatcherServices services_;
    SessionController session_;
    ApplicationState applicationState_;
    PlotCoordinator plotCoordinator_;
    std::string plotTheme_ = "publication";
    std::set<std::string> linkedMissingInformationSources_;
    std::function<void(const Table1DisplayState &)> missingInformationRefresh_;
};

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_COMMAND_DISPATCHER_H
