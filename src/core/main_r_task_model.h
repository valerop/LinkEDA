#ifndef RLISPSTAT_CORE_MAIN_R_TASK_MODEL_H
#define RLISPSTAT_CORE_MAIN_R_TASK_MODEL_H

#include "mixed_model.h"
#include "model_trellis_model.h"

#include <map>
#include <string>
#include <utility>
#include <vector>

namespace rlispstat {
namespace core {

enum class SmoothCurveScope;

struct MainRSmoothTask {
    std::string plotId;
    std::string group;
    SmoothCurveScope scope;
    std::string xVariable;
    std::string yVariable;
    double span = 0.75;
};

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
};

struct MainRLinearGLMTask {
    std::string group;
    std::string dependent;
    std::string scope;
    std::vector<std::string> terms;
    std::map<std::string, std::string> termTypes;
    std::vector<int> rows;
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
};

struct MainRRegressionComparisonTask {
    std::string id;
    std::string group;
    std::string response;
    std::string scope;
    bool autoRefit = true;
    std::vector<std::string> termRows;
    std::map<std::string, std::string> termTypes;
    std::vector<MainRRegressionComparisonModelTask> models;
    std::vector<int> rows;
};

struct MainRGeneralizedGLMTask {
    std::string id;
    std::string group;
    std::string response;
    std::string family;
    std::string link;
    std::string scope;
    std::vector<std::string> terms;
    std::map<std::string, std::string> termTypes;
    bool binaryRegression = false;
    std::string eventValue;
    std::string referenceValue;
    std::vector<int> rows;
};

struct MainRMixedModelTask {
    NativeMixedModelState state;
};

struct MainRDimensionalityTask {
    std::string id;
    std::string group;
    std::string method;
    std::string missingMode;
    std::string rotation;
    std::string scope;
    bool scale = true;
    int componentCount = 2;
    std::vector<std::string> variables;
    std::vector<int> rows;
};

struct MainRTaskBatch {
    std::vector<MainRSmoothTask> smoothTasks;
    std::vector<MainRTrellisSmoothTask> trellisSmoothTasks;
    std::vector<MainRTrellisPanelAnalysisTask> trellisPanelAnalysisTasks;
    std::vector<MainRImportDataTask> importDataTasks;
    std::vector<MainRDataReturnTask> dataReturnTasks;
    std::vector<MainRDataChoiceTask> dataChoiceTasks;
    std::vector<MainRDataBrowseTask> dataBrowseTasks;
    std::vector<MainRWelcomeActionTask> welcomeActionTasks;
    std::vector<MainRDataAssignTask> dataAssignTasks;
    std::vector<MainRPlotExportTask> plotExportTasks;
    std::vector<MainRCompareMeansTask> compareMeansTasks;
    std::vector<MainRLinearGLMTask> linearGLMTasks;
    std::vector<MainRModelTrellisTask> modelTrellisTasks;
    std::vector<MainRRegressionComparisonTask> regressionComparisonTasks;
    std::vector<MainRGeneralizedGLMTask> generalizedGLMTasks;
    std::vector<MainRMixedModelTask> mixedModelTasks;
    std::vector<MainRDimensionalityTask> dimensionalityTasks;
};

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
std::string EncodeMainRTask(const MainRPlotExportTask &task);
std::string EncodeMainRTask(const MainRCompareMeansTask &task);
std::string EncodeMainRTask(const MainRLinearGLMTask &task);
std::string EncodeMainRTask(const MainRModelTrellisTask &task);
std::string EncodeMainRTask(const MainRRegressionComparisonTask &task);
std::string EncodeMainRTask(const MainRGeneralizedGLMTask &task);
std::string EncodeMainRTask(const MainRMixedModelTask &task);
std::string EncodeMainRTask(const MainRDimensionalityTask &task);

std::string AppendMainRTaskMessages(const std::string &reply,
                                    const MainRTaskBatch &batch);

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_MAIN_R_TASK_MODEL_H
