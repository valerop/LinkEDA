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

using CommandBoxplotMutation = std::function<bool(PlotModel &, std::string &)>;

struct CommandVariableInfo {
    std::string name;
    std::string type;
    std::string role;
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
    std::function<void(const MeanComparisonState &)> showMeanComparison;
    std::function<void(const std::string &, const std::string &)> showMeanComparisonError;
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
    std::function<bool(const std::string &, const CommandBoxplotMutation &, std::string &)> mutateBoxplot;
    std::function<bool(const std::string &)> refitCorrelation;
    std::function<bool(const std::string &)> refitDimensionality;
    std::function<bool(const std::string &)> refitDendrogram;
    std::function<bool(const std::string &, SmoothCurveScope)> toggleSmoothCurves;
    std::function<bool(const std::string &, const std::string &, SmoothCurveScope,
                       const std::vector<SmoothCurveData> &)> replaceTrellisSmoothCurves;
};

struct CommandDispatcherServices {
    SessionControllerServices session;
    CommandQueryServices queries;
    CommandUiServices ui;
    std::function<std::string(const CommandRequest &, const std::vector<std::string> &)> handle;
    CommandSelectionServices selection;
};

class CommandDispatcher {
public:
    explicit CommandDispatcher(CommandDispatcherServices services);

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

    CommandDispatcherServices services_;
    SessionController session_;
    ApplicationState applicationState_;
    PlotCoordinator plotCoordinator_;
    std::string plotTheme_ = "classic";
};

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_COMMAND_DISPATCHER_H
