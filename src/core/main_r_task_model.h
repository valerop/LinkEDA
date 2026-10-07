#ifndef RLISPSTAT_CORE_MAIN_R_TASK_MODEL_H
#define RLISPSTAT_CORE_MAIN_R_TASK_MODEL_H

#include "mixed_model.h"
#include "model_trellis_model.h"
#include "scale_analysis_model.h"

#include <cstdint>
#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace rlispstat {
namespace core {

enum class SmoothCurveScope;
struct ScatterplotImputationPointSet;

struct MainRSmoothPoint {
    int row = 0;
    double x = 0.0;
    double y = 0.0;
};

struct MainRSmoothImputationPointSet {
    int imputationIndex = 0;
    std::vector<MainRSmoothPoint> points;
};

struct MainRSmoothTask {
    std::string plotId;
    std::string group;
    SmoothCurveScope scope;
    std::string xVariable;
    std::string yVariable;
    double span = 0.75;
    // Interactive brushing state is owned by the native ApplicationState.
    // Carry the relevant snapshot with the task so fitting selected/coloured
    // curves never needs a nested transport round trip back into the UI.
    std::vector<int> selectedRows;
    std::vector<std::pair<int, std::string>> rowColors;
    // A native plot can show only a subset of the dataset. Preserve that
    // exact row snapshot when R refits its curves.
    bool useVisibleRows = false;
    std::vector<int> visibleRows;
    // When present, R fits one LOESS curve per completed imputation.  This is
    // also used by regression diagnostics, whose X/Y values are derived from
    // separately fitted models rather than worksheet columns.
    std::vector<MainRSmoothImputationPointSet> imputationPointSets;
    std::string fitMethod = "loess";
    double confidenceLevel = 0.95;
};

void PopulateMainRSmoothTaskImputationPointSets(
    MainRSmoothTask &task,
    const std::vector<ScatterplotImputationPointSet> &pointSets);

struct MainRTrellisSmoothTask {
    std::string plotId;
    std::string group;
    std::string panelId;
    SmoothCurveScope scope;
    std::string xVariable;
    std::string yVariable;
    double span = 0.75;
    std::vector<int> rows;
    std::vector<std::string> colorKeys;
    std::vector<int> selectedRows;
    std::vector<std::pair<int, std::string>> rowColors;
    std::string fitMethod = "loess";
    double confidenceLevel = 0.95;
    // Optional exact plotted values. Matrix cells use this snapshot so an R
    // fit always matches the currently displayed imputation and visible rows.
    bool useExplicitPoints = false;
    std::vector<MainRSmoothPoint> explicitPoints;
};

struct MainRTrellisPanelAnalysisTask {
    std::string requestId;
    std::string plotId;
    std::string group;
    std::string panelId;
    std::string panelLabel;
    std::string plotType;
    std::string xVariable;
    std::string yVariable;
    std::string groupingVariable;
    std::vector<int> rows;
    std::vector<std::string> modelTerms;
    std::map<std::string, std::string> termTypes;
};

struct MainRImportDataTask {
    std::string path;
    std::string sourcePath;
    bool removeAfterImport = false;
    std::string stagedDataset;
    std::vector<std::string> selectedVariables;
    bool cancelStagedImport = false;
};

struct MainRDataReturnTask {
    std::string requestId;
    std::string group;
    std::string payloadPath;
    std::string mode = "all";
    std::vector<int> rows;
};

struct MainRDataChoiceTask {
    std::string requestId;
    std::string objectName;
    bool cancelled = false;
};

struct MainRDataBrowseTask {
    std::string requestId;
};

struct MainRWelcomeActionTask {
    std::string requestId;
    std::string action;
    std::string value;
};

struct MainRDataAssignTask {
    std::string requestId;
    std::string group;
    std::string payloadPath;
    std::string objectName;
    std::string mode = "all";
    bool replaceExisting = false;
    std::vector<int> rows;
};

struct MainRPlotExportTask {
    std::string requestId;
    std::string plotId;
    std::string path;
    std::string format;
    std::string operation;
    double widthInches = 7.0;
    double heightInches = 5.0;
    std::string group;
    std::string plotKind;
    std::string xVariable;
    std::string yVariable;
    std::string title;
    std::vector<std::pair<std::string, std::string>> options;
};

struct MainRCompareMeansTask {
    struct OneSampleTest {
        std::string response;
        double testValue = 0.0;
        std::string method = "student";
    };
    std::string group;
    std::string testType;
    std::string var1;
    std::string var2;
    std::string groupVar;
    std::string id;
    std::vector<std::string> responses;
    std::vector<std::pair<std::string, std::string>> pairs;
    double testValue = 0.0;
    std::string alternative = "two.sided";
    double confidenceLevel = 0.95;
    std::string method = "welch";
    std::string pAdjustment = "holm";
    std::vector<std::string> groupOrder;
    std::string scope = "all";
    std::vector<int> rows;
    // Empty for the portable/macOS V2 route. Windows supplies these per-response
    // entries for mixed reports (continuous, rank and proportion tests in one batch).
    std::vector<OneSampleTest> oneSampleTests;
};

struct MainRLinearGLMTask {
    std::string group;
    // Empty for the legacy one-model-per-dataset protocol.
    std::string modelId;
    std::string dependent;
    std::string scope;
    std::vector<std::string> terms;
    std::map<std::string, std::string> termTypes;
    std::vector<std::string> centeredPredictors;
    std::map<std::string, std::string> factorReferenceLevels;
    std::vector<int> rows;
    // Opaque identity of the exact immutable request.  In particular this
    // includes the live selected/unselected row snapshot, which cannot be
    // reconstructed safely when an asynchronous R result arrives later.
    std::string requestIdentity;
};

struct MainRModelTrellisPanelTask {
    std::string panelId;
    std::string rowLevelId;
    std::string rowLevelLabel;
    std::string columnLevelId;
    std::string columnLevelLabel;
    std::vector<int> rows;
};

struct MainRModelTrellisTask {
    std::string id;
    int generation = 0;
    std::string group;
    std::string response;
    std::string scope;
    double confidenceLevel = 0.95;
    std::string pAdjustment = "holm";
    std::vector<std::string> baseTerms;
    std::vector<std::string> effectiveTerms;
    std::vector<std::string> omittedTerms;
    std::map<std::string, std::string> termTypes;
    std::vector<MainRModelTrellisPanelTask> panels;
};

struct MainRRegressionComparisonModelTask {
    std::string id;
    std::string label;
    std::string response;
    std::vector<std::string> terms;
    std::map<std::string, std::string> termTypes;
    std::vector<std::string> centeredPredictors;
    std::map<std::string, std::string> factorReferenceLevels;
};

struct MainRRegressionComparisonTask {
    std::string id;
    int generation = 0;
    std::string group;
    std::string response;
    std::string scope;
    bool autoRefit = true;
    std::string datasetType = "data_frame";
    std::string imputationSetId;
    std::string sourceDatasetId;
    int imputationCount = 0;
    std::vector<std::string> termRows;
    std::map<std::string, std::string> termTypes;
    std::vector<MainRRegressionComparisonModelTask> models;
    std::vector<int> rows;
};

struct MainRGeneralizedGLMTask {
    std::string id;
    std::uint64_t generation = 0;
    std::string group;
    std::string response;
    std::string family;
    std::string link;
    std::string modelType = "legacy_generalized";
    bool responseBoundsConfigured = false;
    double responseLower = 0.0;
    double responseUpper = 0.0;
    std::string scope;
    std::vector<std::string> terms;
    std::map<std::string, std::string> termTypes;
    std::vector<std::string> centeredPredictors;
    std::map<std::string, std::string> factorReferenceLevels;
    bool binaryRegression = false;
    bool countRegression = false;
    std::string countDistribution = "poisson";
    std::string exposure;
    std::string offsetVariable;
    std::string trialsVariable;
    double trialsConstant = NAN;
    std::string eventValue;
    std::string referenceValue;
    std::vector<int> rows;
};

struct MainRMixedModelTask {
    NativeMixedModelState state;
};

struct MainRCorrelationTask {
    std::string id, group;
    std::uint64_t revision = 0, dataVersion = 0;
    std::string scope = "all", method = "pearson", missingMode = "pairwise";
    std::vector<std::string> variables;
    std::vector<int> rows;
};

struct MainRDendrogramTask {
    std::string id, group;
    bool distanceMatrix = false;
    std::uint64_t revision = 0, dataVersion = 0;
    int imputation = 1;
    std::string scope = "all", distance = "euclidean", linkage = "average", missingMode = "pairwise";
    std::vector<std::string> variables;
    std::vector<int> rows;
};

struct MainRDimensionalityTask {
    std::uint64_t revision = 0;
    std::uint64_t dataVersion = 0;
    int displayedImputation = 0;
    std::string id;
    std::string group;
    std::string method;
    std::string missingMode;
    std::string rotation;
    std::string extraction = "minres";
    std::string scope;
    bool scale = true;
    int componentCount = 2;
    std::vector<std::string> variables;
    std::vector<int> rows;
};

struct MainRScaleAnalysisTask {
    std::string id;
    std::string group;
    ScaleAnalysisSpecification specification;
    std::string scope = "all";
    std::vector<int> rows;
};

struct MainRDimensionalityScoreSaveTask {
    std::string requestId;
    std::string sourceKind;
    std::string analysisId;
    std::string group;
    int componentCount = 1;
};

// Internal synchronization after an editable native data-sheet mutation.
// Unlike "Return Data to R", this updates LinkEDA's registered dataset only;
// it never creates or overwrites an object in .GlobalEnv.
struct MainRDatasetSyncTask {
    std::string requestId;
    std::string group;
    std::string payloadPath;
};

struct MainRGeneralizedComparisonModelTask {
    std::string id;
    std::string label;
    std::string response;
    std::string family;
    std::string link;
    std::string scope;
    bool responseBoundsConfigured = false;
    double responseLower = NAN;
    double responseUpper = NAN;
    bool countRegression = false;
    std::string countDistribution = "poisson";
    std::string exposure;
    std::string offsetVariable;
    std::string trialsVariable;
    double trialsConstant = NAN;
    std::vector<std::string> terms;
    std::map<std::string, std::string> termTypes;
    std::vector<std::string> centeredPredictors;
    std::map<std::string, std::string> factorReferenceLevels;
    int specificationRevision = 0;
    std::string specificationFingerprint;
};

struct MainRGeneralizedComparisonTask {
    std::string id;
    std::string group;
    std::string response;
    std::string family;
    std::string link;
    std::string scope;
    bool binaryComparison = false;
    bool countComparison = false;
    std::string modelType = "legacy_generalized";
    std::string eventValue;
    std::string referenceValue;
    std::string datasetType = "data_frame";
    std::string imputationSetId;
    std::string sourceDatasetId;
    int imputationCount = 0;
    std::map<std::string, std::string> termTypes;
    std::vector<MainRGeneralizedComparisonModelTask> models;
    std::vector<int> rows;
    std::uint64_t generation = 0;
    bool autoRefit = true;
};

struct MainRTable1Task {
    std::string analysisKind = "table1";
    std::string contingencyDisplayMode = "count_percent";
    std::string sourcePlotId;
    bool linkEnabled = false;
    std::uint64_t revision = 0;
    std::uint64_t dataVersion = 0;
    std::string title;
    std::string id;
    std::string group;
    std::vector<std::string> variables;
    std::string groupVariable;
    // Explicit Table 1 interpretation derived from the dataset's canonical
    // variable metadata.  R must not re-infer low-cardinality numeric columns
    // (for example mtcars$cyl) after the user has chosen Numeric in the UI.
    std::map<std::string, std::string> variableTypes;
    bool includeMissing = true;
    bool showP = true;
    bool showTest = true;
    bool showN = true;
    std::string ordinalAs = "ordinal";
    std::string scope = "all";
    std::string scopeDescription = "All observations";
    std::vector<int> rows;
};

struct MainRAnalysisWorkflowTask {
    std::string id;
    std::string kind;
    std::string group;
    std::string response;
    std::string secondary;
    std::string groupVariable;
    std::vector<std::string> variables;
    std::string family = "gaussian";
    std::string link = "identity";
    std::string method;
    std::string rowCondition;
    std::string columnCondition;
    std::vector<int> rows;
};

MainRAnalysisWorkflowTask ScaleTotalScoreSaveTask(const std::string &id, const std::string &group,
    const std::string &method, int revision, const std::string &fingerprint);

struct MainRMultipleImputationTask {
    std::string group;
    std::vector<std::string> imputeVariables;
    std::vector<std::string> predictorVariables;
    std::map<std::string, std::string> methods;
    int imputations = 5;
    int iterations = 5;
    std::string seed;
    bool openDataSheet = true;
};

struct MainRMIDiagnosticsTask {
    std::string group;
    std::string section = "summary";
    std::string variable;
    int imputationStart = 1;
};
std::string EncodeMainRTask(const MainRMIDiagnosticsTask &task);

struct MainRTaskBatch {
    std::vector<MainRMIDiagnosticsTask> miDiagnosticsTasks;
    std::vector<MainRSmoothTask> smoothTasks;
    std::vector<MainRTrellisSmoothTask> trellisSmoothTasks;
    std::vector<MainRTrellisPanelAnalysisTask> trellisPanelAnalysisTasks;
    std::vector<MainRImportDataTask> importDataTasks;
    std::vector<MainRDataReturnTask> dataReturnTasks;
    std::vector<MainRDataChoiceTask> dataChoiceTasks;
    std::vector<MainRDataBrowseTask> dataBrowseTasks;
    std::vector<MainRWelcomeActionTask> welcomeActionTasks;
    std::vector<MainRDataAssignTask> dataAssignTasks;
    std::vector<MainRDatasetSyncTask> datasetSyncTasks;
    std::vector<MainRPlotExportTask> plotExportTasks;
    std::vector<MainRCompareMeansTask> compareMeansTasks;
    std::vector<MainRLinearGLMTask> linearGLMTasks;
    std::vector<MainRModelTrellisTask> modelTrellisTasks;
    std::vector<MainRRegressionComparisonTask> regressionComparisonTasks;
    std::vector<MainRGeneralizedGLMTask> generalizedGLMTasks;
    std::vector<MainRGeneralizedComparisonTask> generalizedComparisonTasks;
    std::vector<MainRMixedModelTask> mixedModelTasks;
    std::vector<MainRDimensionalityTask> dimensionalityTasks;
    std::vector<MainRDendrogramTask> dendrogramTasks;
    std::vector<MainRCorrelationTask> correlationTasks;
    std::vector<MainRScaleAnalysisTask> scaleAnalysisTasks;
    std::vector<MainRDimensionalityScoreSaveTask> dimensionalityScoreSaveTasks;
    std::vector<MainRTable1Task> table1Tasks;
    std::vector<MainRAnalysisWorkflowTask> analysisWorkflowTasks;
    std::vector<MainRMultipleImputationTask> multipleImputationTasks;
};

// Scale Analysis is fitted asynchronously in the user's main R session.  Its
// task therefore remains leased until a result carrying the exact immutable
// specification identity has been accepted.  These helpers keep the lease
// semantics identical in the macOS and Windows frontends.
void QueueLatestMainRScaleAnalysisTask(
    std::vector<MainRScaleAnalysisTask> &tasks,
    MainRScaleAnalysisTask task);
bool AcknowledgeMainRScaleAnalysisTask(
    std::vector<MainRScaleAnalysisTask> &tasks,
    const std::string &id,
    int revision,
    const std::string &fingerprint);

bool MainRReplyAcceptsTaskAppend(const std::string &reply);
std::string MainRSmoothScopeToken(SmoothCurveScope scope);

std::string EncodeMainRTask(const MainRSmoothTask &task);
std::string EncodeMainRTask(const MainRTrellisSmoothTask &task);
std::string EncodeMainRTask(const MainRTrellisPanelAnalysisTask &task);
std::string EncodeMainRTask(const MainRImportDataTask &task);
std::string EncodeMainRTask(const MainRDataReturnTask &task);
std::string EncodeMainRTask(const MainRDataChoiceTask &task);
std::string EncodeMainRTask(const MainRDataBrowseTask &task);
std::string EncodeMainRTask(const MainRWelcomeActionTask &task);
std::string EncodeMainRTask(const MainRDataAssignTask &task);
std::string EncodeMainRTask(const MainRDatasetSyncTask &task);
std::string EncodeMainRTask(const MainRPlotExportTask &task);
std::string EncodeMainRTask(const MainRCompareMeansTask &task);
std::string EncodeMainRTask(const MainRLinearGLMTask &task);
std::string EncodeMainRTask(const MainRModelTrellisTask &task);
std::string EncodeMainRTask(const MainRRegressionComparisonTask &task);
// The row payload is the selection for a live selected/unselected request,
// but already-included rows for an explicitly captured scope.
void SetGeneralizedTaskAnalysisScope(MainRGeneralizedGLMTask &task,
    const AnalysisScope &scope, bool captured, const std::set<int> &selection);
std::string EncodeMainRTask(const MainRGeneralizedGLMTask &task);
// Encodes the same immutable generalized-model specification for the
// standalone macOS Rscript bridge.  Unlike the legacy family|link payload,
// this includes the analysis kind and every model-local option that changes
// the fitted design.
std::string EncodeStandaloneGeneralizedMISpec(const MainRGeneralizedGLMTask &task);
std::string EncodeStandaloneLinearMISpec(
    const std::map<std::string, std::string> &termTypes,
    const std::set<std::string> &centeredPredictors,
    const std::map<std::string, std::string> &factorReferenceLevels,
    const std::vector<int> &rows);
std::string EncodeMainRTask(const MainRGeneralizedComparisonTask &task);
std::string EncodeMainRTask(const MainRMixedModelTask &task);
std::string EncodeMainRTask(const MainRDimensionalityTask &task);
std::string EncodeMainRTask(const MainRDendrogramTask &task);
std::string EncodeMainRTask(const MainRCorrelationTask &task);
std::string EncodeMainRTask(const MainRScaleAnalysisTask &task);
std::string EncodeMainRTask(const MainRDimensionalityScoreSaveTask &task);
std::string EncodeMainRTask(const MainRTable1Task &task);
std::string EncodeMainRTask(const MainRAnalysisWorkflowTask &task);
std::string EncodeMainRTask(const MainRMultipleImputationTask &task);

std::string AppendMainRTaskMessages(const std::string &reply,
                                    const MainRTaskBatch &batch);

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_MAIN_R_TASK_MODEL_H
