#include "main_r_task_model.h"

#include "plot_geometry.h"

#include <iomanip>
#include <sstream>
#include <string>

namespace rlispstat {
namespace core {

namespace {

void AppendCountedStrings(std::string &out, const std::vector<std::string> &values)
{
    out += "\t" + std::to_string((long long)values.size());
    for (const std::string &value : values) {
        out += "\t" + value;
    }
}

void AppendTermTypes(std::string &out, const std::map<std::string, std::string> &termTypes)
{
    out += "\t" + std::to_string((long long)termTypes.size());
    for (const auto &entry : termTypes) {
        out += "\t" + entry.first + "\t" + entry.second;
    }
}

void AppendRows(std::string &out, const std::vector<int> &rows)
{
    out += "\t" + std::to_string((long long)rows.size());
    for (int row : rows) out += "\t" + std::to_string(row);
}

void AppendTaskLine(std::string &out, const std::string &line)
{
    out += "\n";
    out += line;
}

} // namespace

bool MainRReplyAcceptsTaskAppend(const std::string &reply)
{
    return reply.size() >= 2 && reply[0] == 'O' && reply[1] == 'K';
}

std::string MainRSmoothScopeToken(SmoothCurveScope scope)
{
    switch (scope) {
        case SmoothCurveScope::Overall:
            return "overall";
        case SmoothCurveScope::Selection:
            return "selected";
        case SmoothCurveScope::ColorGroup:
            return "color";
    }
    return "overall";
}

std::string EncodeMainRTask(const MainRSmoothTask &task)
{
    std::ostringstream span;
    span << std::setprecision(17) << ClampSmoothSpan(task.span);
    return "SMOOTH_NEEDED|" + task.plotId + "|" + task.group + "|" +
           MainRSmoothScopeToken(task.scope) + "|" +
           task.xVariable + "|" + task.yVariable + "|" + span.str();
}

std::string EncodeMainRTask(const MainRTrellisSmoothTask &task)
{
    std::ostringstream span;
    span << std::setprecision(17) << ClampSmoothSpan(task.span);
    std::string out = "TRELLIS_SMOOTH_NEEDED\t" + task.plotId + "\t" + task.group + "\t" +
        task.panelId + "\t" +
        MainRSmoothScopeToken(task.scope) + "\t" + task.xVariable + "\t" + task.yVariable +
        "\t" + span.str() + "\t" + std::to_string((long long)task.rows.size());
    for (int row : task.rows) out += "\t" + std::to_string(row);
    out += "\t" + std::to_string((long long)task.colorKeys.size());
    for (const std::string &key : task.colorKeys) out += "\t" + key;
    return out;
}

std::string EncodeMainRTask(const MainRTrellisPanelAnalysisTask &task)
{
    std::string out = "TRELLIS_PANEL_ANALYSIS_NEEDED\t" + task.requestId + "\t" +
        task.plotId + "\t" + task.group + "\t" + task.panelId + "\t" +
        task.panelLabel + "\t" + task.plotType + "\t" + task.xVariable + "\t" +
        task.yVariable + "\t" + task.groupingVariable + "\t" +
        std::to_string((long long)task.rows.size());
    for (int row : task.rows) out += "\t" + std::to_string(row);
    if (!task.modelTerms.empty() || !task.termTypes.empty()) {
        AppendCountedStrings(out, task.modelTerms);
        AppendTermTypes(out, task.termTypes);
    }
    return out;
}

std::string EncodeMainRTask(const MainRImportDataTask &task)
{
    std::string out = "IMPORT_DATA_NEEDED\t" + task.path;
    if (!task.sourcePath.empty() || task.removeAfterImport) {
        out += "\t" + task.sourcePath + "\t" + (task.removeAfterImport ? "1" : "0");
    }
    return out;
}

std::string EncodeMainRTask(const MainRDataReturnTask &task)
{
    std::string out = "R_DATA_RETURN_NEEDED\t" + task.requestId + "\t" + task.group +
        "\t" + task.payloadPath + "\t" + task.mode + "\t" +
        std::to_string((long long)task.rows.size());
    for (int row : task.rows) out += "\t" + std::to_string(row);
    return out;
}

std::string EncodeMainRTask(const MainRDataChoiceTask &task)
{
    return "R_DATA_CHOICE_NEEDED\t" + task.requestId + "\t" +
        (task.cancelled ? "cancel" : "choose") + "\t" + task.objectName;
}

std::string EncodeMainRTask(const MainRDataBrowseTask &task)
{
    return "R_DATA_BROWSE_NEEDED\t" + task.requestId;
}

std::string EncodeMainRTask(const MainRWelcomeActionTask &task)
{
    return "WELCOME_ACTION_NEEDED\t" + task.requestId + "\t" +
           task.action + "\t" + task.value;
}

std::string EncodeMainRTask(const MainRDataAssignTask &task)
{
    std::string out = "R_DATA_ASSIGN_NEEDED\t" + task.requestId + "\t" + task.group +
        "\t" + task.payloadPath + "\t" + task.objectName + "\t" + task.mode + "\t" +
        (task.replaceExisting ? "1" : "0") + "\t" +
        std::to_string((long long)task.rows.size());
    for (int row : task.rows) out += "\t" + std::to_string(row);
    return out;
}

std::string EncodeMainRTask(const MainRPlotExportTask &task)
{
    std::ostringstream width;
    std::ostringstream height;
    width << std::setprecision(17) << task.widthInches;
    height << std::setprecision(17) << task.heightInches;
    std::string out = "PLOT_EXPORT_NEEDED\t" + task.requestId + "\t" + task.plotId + "\t" +
        task.path + "\t" + task.format + "\t" + task.operation + "\t" +
        width.str() + "\t" + height.str() + "\t" + task.group + "\t" + task.plotKind +
        "\t" + task.xVariable + "\t" + task.yVariable + "\t" + task.title + "\t" +
        std::to_string((long long)task.options.size());
    for (const auto &option : task.options) out += "\t" + option.first + "\t" + option.second;
    return out;
}

std::string EncodeMainRTask(const MainRCompareMeansTask &task)
{
    if (!task.id.empty() || !task.responses.empty() || !task.pairs.empty()) {
        auto number = [](double value) {
            std::ostringstream stream;
            stream << std::setprecision(17) << value;
            return stream.str();
        };
        std::string out = "COMPARE_MEANS_BATCH_NEEDED\t" + task.id + "\t" + task.group + "\t" +
            task.testType + "\t" + task.alternative + "\t" + number(task.confidenceLevel) + "\t" +
            task.method + "\t" + task.pAdjustment + "\t" + number(task.testValue) + "\t" + task.groupVar;
        AppendCountedStrings(out, task.responses);
        out += "\t" + std::to_string((long long)task.pairs.size());
        for (const auto &pair : task.pairs) out += "\t" + pair.first + "\t" + pair.second;
        AppendCountedStrings(out, task.groupOrder);
        out += "\t" + task.scope;
        AppendRows(out, task.rows);
        return out;
    }
    return "COMPARE_MEANS_NEEDED|" + task.group + "|" + task.testType + "|" +
           task.var1 + "|" + task.var2 + "|" + task.groupVar;
}

std::string EncodeMainRTask(const MainRLinearGLMTask &task)
{
    std::string out = "GLM_NEEDED\t" + task.group + "\t" + task.dependent + "\t" +
                      task.scope;
    AppendCountedStrings(out, task.terms);
    AppendTermTypes(out, task.termTypes);
    AppendRows(out, task.rows);
    return out;
}

std::string EncodeMainRTask(const MainRModelTrellisTask &task)
{
    std::ostringstream level;
    level << std::setprecision(17) << task.confidenceLevel;
    std::string out = "MODEL_TRELLIS_NEEDED\t" + task.id + "\t" +
        std::to_string(task.generation) + "\t" + task.group + "\t" +
        task.response + "\t" + task.scope + "\t" + level.str() + "\t" + task.pAdjustment;
    AppendCountedStrings(out, task.baseTerms);
    AppendCountedStrings(out, task.effectiveTerms);
    AppendCountedStrings(out, task.omittedTerms);
    AppendTermTypes(out, task.termTypes);
    out += "\t" + std::to_string((long long)task.panels.size());
    for (const MainRModelTrellisPanelTask &panel : task.panels) {
        out += "\t" + panel.panelId + "\t" + panel.rowLevelId + "\t" + panel.rowLevelLabel +
               "\t" + panel.columnLevelId + "\t" + panel.columnLevelLabel;
        out += "\t" + std::to_string((long long)panel.rows.size());
        for (int row : panel.rows) out += "\t" + std::to_string(row);
    }
    return out;
}

std::string EncodeMainRTask(const MainRRegressionComparisonTask &task)
{
    std::string out = "REGCMP_NEEDED\t" + task.id + "\t" + task.group + "\t" +
                      task.response + "\t" + task.scope + "\t" +
                      (task.autoRefit ? "TRUE" : "FALSE");
    AppendCountedStrings(out, task.termRows);
    AppendTermTypes(out, task.termTypes);
    out += "\t" + std::to_string((long long)task.models.size());
    for (const MainRRegressionComparisonModelTask &model : task.models) {
        out += "\t" + model.id + "\t" + model.label + "\t" + model.response;
        AppendCountedStrings(out, model.terms);
    }
    AppendRows(out, task.rows);
    return out;
}

std::string EncodeMainRTask(const MainRGeneralizedGLMTask &task)
{
    std::string out = "GGLM_NEEDED\t" + task.id + "\t" + task.group + "\t" +
                      task.response + "\t" + task.family + "\t" + task.link + "\t" +
                      task.scope;
    AppendCountedStrings(out, task.terms);
    AppendTermTypes(out, task.termTypes);
    out += "\t" + std::string(task.binaryRegression ? "BINARY" : "GENERALIZED") +
           "\t" + task.eventValue + "\t" + task.referenceValue;
    AppendRows(out, task.rows);
    return out;
}

std::string EncodeMainRTask(const MainRMixedModelTask &task)
{
    const NativeMixedModelState &state = task.state;
    std::string out = "MIXED_MODEL_NEEDED\t" + state.id + "\t" + state.group + "\t" +
                      state.modelType + "\t" + state.response + "\t" + state.method + "\t" +
                      state.family + "\t" + state.link;
    AppendCountedStrings(out, state.fixedEffects);
    out += "\t" + std::to_string((long long)state.randomEffects.size());
    for (const NativeMixedRandomSpec &spec : state.randomEffects) {
        out += "\t" + spec.group + "\t" + spec.covariance;
        AppendCountedStrings(out, spec.terms);
    }
    out += "\t" + std::string(state.dataScopeCaptured &&
        state.dataScope.kind == AnalysisScopeKind::ExplicitRowIds ? "selected" : "all");
    AppendRows(out, state.dataScopeCaptured &&
        state.dataScope.kind == AnalysisScopeKind::ExplicitRowIds
        ? ResolveAnalysisScopeRowIds(state.dataScope, state.dataScope.totalDatasetRows)
        : std::vector<int>{});
    return out;
}

std::string EncodeMainRTask(const MainRDimensionalityTask &task)
{
    std::string out = "PCAFA_FACTOR_NEEDED\t" + task.id + "\t" + task.group + "\t" +
                      task.method + "\t" + task.missingMode + "\t" + task.rotation + "\t" +
                      task.scope + "\t" + (task.scale ? "TRUE" : "FALSE") + "\t" +
                      std::to_string((long long)task.componentCount);
    AppendCountedStrings(out, task.variables);
    AppendRows(out, task.rows);
    return out;
}

std::string AppendMainRTaskMessages(const std::string &reply,
                                    const MainRTaskBatch &batch)
{
    if (!MainRReplyAcceptsTaskAppend(reply)) {
        return reply;
    }
    std::string out = reply;
    for (const MainRSmoothTask &task : batch.smoothTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRTrellisSmoothTask &task : batch.trellisSmoothTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRTrellisPanelAnalysisTask &task : batch.trellisPanelAnalysisTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRImportDataTask &task : batch.importDataTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRDataReturnTask &task : batch.dataReturnTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRDataChoiceTask &task : batch.dataChoiceTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRDataBrowseTask &task : batch.dataBrowseTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRWelcomeActionTask &task : batch.welcomeActionTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRDataAssignTask &task : batch.dataAssignTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRPlotExportTask &task : batch.plotExportTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRCompareMeansTask &task : batch.compareMeansTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRLinearGLMTask &task : batch.linearGLMTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRModelTrellisTask &task : batch.modelTrellisTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRRegressionComparisonTask &task : batch.regressionComparisonTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRGeneralizedGLMTask &task : batch.generalizedGLMTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRMixedModelTask &task : batch.mixedModelTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRDimensionalityTask &task : batch.dimensionalityTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    return out;
}

} // namespace core
} // namespace rlispstat
