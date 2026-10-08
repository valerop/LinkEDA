#include "main_r_task_model.h"

#include "command_model.h"
#include "plot_geometry.h"
#include "scatterplot_model.h"

#include <algorithm>
#include <iomanip>
#include <sstream>
#include <string>

namespace rlispstat {
namespace core {

void QueueLatestMainRScaleAnalysisTask(
    std::vector<MainRScaleAnalysisTask> &tasks,
    MainRScaleAnalysisTask task)
{
    tasks.erase(
        std::remove_if(tasks.begin(), tasks.end(),
                       [&](const MainRScaleAnalysisTask &pending) {
                           return pending.id == task.id;
                       }),
        tasks.end());
    tasks.push_back(std::move(task));
}

bool AcknowledgeMainRScaleAnalysisTask(
    std::vector<MainRScaleAnalysisTask> &tasks,
    const std::string &id,
    int revision,
    const std::string &fingerprint)
{
    const auto previousSize = tasks.size();
    tasks.erase(
        std::remove_if(tasks.begin(), tasks.end(),
                       [&](const MainRScaleAnalysisTask &pending) {
                           return pending.id == id &&
                               pending.specification.revision == revision &&
                               pending.specification.fingerprint == fingerprint;
                       }),
        tasks.end());
    return tasks.size() != previousSize;
}

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

std::string FormatProtocolDouble(double value)
{
    std::ostringstream out;
    out << std::setprecision(17) << value;
    return out.str();
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

void PopulateMainRSmoothTaskImputationPointSets(
    MainRSmoothTask &task,
    const std::vector<ScatterplotImputationPointSet> &pointSets)
{
    task.imputationPointSets.clear();
    task.imputationPointSets.reserve(pointSets.size());
    for (const ScatterplotImputationPointSet &source : pointSets) {
        MainRSmoothImputationPointSet target;
        target.imputationIndex = source.imputationIndex;
        target.points.reserve(source.points.size());
        for (const ScatterplotPointValue &point : source.points) {
            target.points.push_back(MainRSmoothPoint{point.caseId, point.x, point.y});
        }
        task.imputationPointSets.push_back(std::move(target));
    }
}

std::string EncodeMainRTask(const MainRSmoothTask &task)
{
    std::ostringstream span;
    span << std::setprecision(17) << ClampSmoothSpan(task.span);
    std::string out = "SMOOTH_NEEDED|" + task.plotId + "|" + task.group + "|" +
           MainRSmoothScopeToken(task.scope) + "|" +
           task.xVariable + "|" + task.yVariable + "|" + span.str();
    out += "|" + std::to_string((long long)task.selectedRows.size());
    for (int row : task.selectedRows) out += "|" + std::to_string(row);
    out += "|" + std::to_string((long long)task.rowColors.size());
    for (const auto &entry : task.rowColors) {
        out += "|" + std::to_string(entry.first) + "|" + entry.second;
    }
    if (!task.imputationPointSets.empty()) {
        out += "|MI_POINT_SETS_V1|" +
            std::to_string((long long)task.imputationPointSets.size());
        for (const MainRSmoothImputationPointSet &set : task.imputationPointSets) {
            out += "|" + std::to_string(set.imputationIndex) + "|" +
                std::to_string((long long)set.points.size());
            for (const MainRSmoothPoint &point : set.points) {
                std::ostringstream x;
                std::ostringstream y;
                x << std::setprecision(17) << point.x;
                y << std::setprecision(17) << point.y;
                out += "|" + std::to_string(point.row) + "|" + x.str() + "|" + y.str();
            }
        }
    }
    if (task.useVisibleRows) {
        out += "|VISIBLE_ROWS_V1|" + std::to_string((long long)task.visibleRows.size());
        for (int row : task.visibleRows) out += "|" + std::to_string(row);
    }
    out += "|FIT_CURVE_V2|" + task.fitMethod + "|";
    std::ostringstream confidence;
    confidence << std::setprecision(17) << task.confidenceLevel;
    out += confidence.str();
    return out;
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
    out += "\t" + std::to_string((long long)task.selectedRows.size());
    for (int row : task.selectedRows) out += "\t" + std::to_string(row);
    out += "\t" + std::to_string((long long)task.rowColors.size());
    for (const auto &entry : task.rowColors) {
        out += "\t" + std::to_string(entry.first) + "\t" + entry.second;
    }
    std::ostringstream confidence;
    confidence << std::setprecision(17) << task.confidenceLevel;
    out += "\tFIT_CURVE_V2\t" + task.fitMethod + "\t" + confidence.str();
    if (task.useExplicitPoints) {
        out += "\tEXPLICIT_POINTS_V1\t" +
            std::to_string((long long)task.explicitPoints.size());
        for (const MainRSmoothPoint &point : task.explicitPoints) {
            std::ostringstream x;
            std::ostringstream y;
            x << std::setprecision(17) << point.x;
            y << std::setprecision(17) << point.y;
            out += "\t" + std::to_string(point.row) + "\t" + x.str() + "\t" + y.str();
        }
    }
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
    if (!task.stagedDataset.empty()) {
        std::string out = "IMPORT_DATA_COMMIT_NEEDED\t" + task.stagedDataset + "\t" +
            (task.cancelStagedImport ? "1" : "0") + "\t" +
            std::to_string((long long)task.selectedVariables.size());
        for (const std::string &variable : task.selectedVariables) out += "\t" + variable;
        return out;
    }
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
    const bool mixedTask = task.testType == "one_sample_mixed" ||
        task.testType == "independent_mixed" || task.testType == "paired_mixed";
    // Mixed analyses carry their per-variable/per-pair method choices only in
    // the batch protocol. Never silently downgrade one to the legacy message:
    // that dispatcher cannot represent those choices or the *_mixed names.
    if (mixedTask || !task.id.empty() || !task.responses.empty() ||
        !task.pairs.empty() || !task.oneSampleTests.empty()) {
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
        out += "\t" + std::to_string((long long)task.oneSampleTests.size());
        for (const auto &test : task.oneSampleTests) {
            out += "\t" + test.response + "\t" + number(test.testValue) + "\t" + test.method;
        }
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
    AppendCountedStrings(out, task.centeredPredictors);
    AppendTermTypes(out, task.factorReferenceLevels);
    AppendRows(out, task.rows);
    if (!task.requestIdentity.empty()) {
        out += "\tFIT_ID_V1\t" + task.requestIdentity;
    }
    if (!task.modelId.empty() && task.modelId != task.group) {
        out += "\tLINEAR_MODEL_ID_V1\t" + task.modelId;
    }
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
    std::string out = "REGCMP_NEEDED\t" + task.id + "\t" +
                      std::to_string(task.generation) + "\t" + task.group + "\t" +
                      task.response + "\t" + task.scope + "\t" +
                      (task.autoRefit ? "TRUE" : "FALSE");
    AppendCountedStrings(out, task.termRows);
    AppendTermTypes(out, task.termTypes);
    out += "\t" + std::to_string((long long)task.models.size());
    for (const MainRRegressionComparisonModelTask &model : task.models) {
        out += "\t" + model.id + "\t" + model.label + "\t" + model.response;
        AppendCountedStrings(out, model.terms);
        AppendTermTypes(out, model.termTypes);
        AppendCountedStrings(out, model.centeredPredictors);
        AppendTermTypes(out, model.factorReferenceLevels);
    }
    AppendRows(out, task.rows);
    out += "\tREGCMP_DATASET_V1\t" + task.datasetType +
           "\t" + task.imputationSetId +
           "\t" + task.sourceDatasetId +
           "\t" + std::to_string(std::max(0, task.imputationCount));
    return out;
}

void SetGeneralizedTaskAnalysisScope(MainRGeneralizedGLMTask &task,
    const AnalysisScope &scope, bool captured, const std::set<int> &selection)
{
    task.rows.clear();
    if (captured) {
        task.scope = scope.kind == AnalysisScopeKind::ExplicitRowIds ? "selected" : "all";
        if (task.scope == "selected")
            task.rows = ResolveAnalysisScopeRowIds(scope, scope.totalDatasetRows);
    } else if (task.scope == "selected" || task.scope == "unselected") {
        task.rows.assign(selection.begin(), selection.end());
    }
}

std::string EncodeMainRTask(const MainRGeneralizedGLMTask &task)
{
    std::string out = "GGLM_NEEDED\t" + task.id + "\t" + task.group + "\t" +
                      task.response + "\t" + task.family + "\t" + task.link + "\t" +
                      task.scope;
    AppendCountedStrings(out, task.terms);
    AppendTermTypes(out, task.termTypes);
    out += "\t" + std::string(task.countRegression ? "COUNT" :
                               task.binaryRegression ? "BINARY" : "GENERALIZED") +
           "\t" + task.eventValue + "\t" + task.referenceValue;
    AppendRows(out, task.rows);
    // Keep the original GGLM wire prefix intact.  RStudio can retain an
    // already-loaded LinkEDA namespace after the native application has been
    // updated; an older request reader must therefore still be able to read
    // the mode and row selection.  Shared model-specification fields live in
    // a tagged trailing extension that old readers safely ignore.
    out += "\tMODEL_SPEC_V1";
    AppendCountedStrings(out, task.centeredPredictors);
    AppendTermTypes(out, task.factorReferenceLevels);
    if (task.countRegression) {
        out += "\tCOUNT_SPEC_V2\t" + task.countDistribution + "\t" + task.exposure +
               "\t" + task.trialsVariable + "\t" +
               FormatProtocolDouble(task.trialsConstant);
    }
    out += "\tOFFSET_SPEC_V1\t" + task.offsetVariable;
    if (task.responseBoundsConfigured) {
        out += "\tBOUNDED_RESPONSE_V1\t" + FormatProtocolDouble(task.responseLower) +
               "\t" + FormatProtocolDouble(task.responseUpper);
    }
    out += "\tMODEL_TYPE_V1\t" + task.modelType;
    out += "\tGGLM_REQUEST_V1\t" +
           std::to_string((unsigned long long)task.generation);
    return out;
}

std::string EncodeStandaloneGeneralizedMISpec(const MainRGeneralizedGLMTask &task)
{
    std::string out = "MI_GGLM_SPEC_V5";
    const auto append = [&](const std::string &value) {
        out += "|" + EncodeCommandField(value);
    };
    append(task.family);
    append(task.link);
    append(std::to_string((unsigned long long)task.generation));
    append(task.countRegression ? "COUNT" : task.binaryRegression ? "BINARY" : "GENERALIZED");
    append(task.countDistribution);
    append(task.exposure);
    append(task.offsetVariable);
    append(task.trialsVariable);
    append(FormatProtocolDouble(task.trialsConstant));
    append(task.eventValue);
    append(task.referenceValue);
    append(task.responseBoundsConfigured ? "TRUE" : "FALSE");
    append(FormatProtocolDouble(task.responseLower));
    append(FormatProtocolDouble(task.responseUpper));
    append(std::to_string((long long)task.termTypes.size()));
    for (const auto &entry : task.termTypes) {
        append(entry.first);
        append(entry.second);
    }
    append(std::to_string((long long)task.centeredPredictors.size()));
    for (const std::string &predictor : task.centeredPredictors) append(predictor);
    append(std::to_string((long long)task.factorReferenceLevels.size()));
    for (const auto &entry : task.factorReferenceLevels) {
        append(entry.first);
        append(entry.second);
    }
    append(std::to_string((long long)task.rows.size()));
    for (int row : task.rows) append(std::to_string(row));
    append("MODEL_TYPE_V1");
    append(task.modelType);
    return out;
}

std::string EncodeStandaloneLinearMISpec(
    const std::map<std::string, std::string> &termTypes,
    const std::set<std::string> &centeredPredictors,
    const std::map<std::string, std::string> &factorReferenceLevels,
    const std::vector<int> &rows)
{
    std::string out = "MI_LINEAR_SPEC_V1";
    const auto append = [&](const std::string &value) {
        out += "|" + EncodeCommandField(value);
    };
    append(std::to_string((long long)termTypes.size()));
    for (const auto &entry : termTypes) {
        append(entry.first);
        append(entry.second);
    }
    append(std::to_string((long long)centeredPredictors.size()));
    for (const std::string &predictor : centeredPredictors) append(predictor);
    append(std::to_string((long long)factorReferenceLevels.size()));
    for (const auto &entry : factorReferenceLevels) {
        append(entry.first);
        append(entry.second);
    }
    append(std::to_string((long long)rows.size()));
    for (int row : rows) append(std::to_string(row));
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

std::string EncodeMainRTask(const MainRCorrelationTask &task)
{
    std::string out = "CORRELATION_NEEDED\t" + task.id + "\t" + task.group + "\t" +
        std::to_string(task.revision) + "\t" + std::to_string(task.dataVersion) + "\t" +
        task.scope + "\t" + task.method + "\t" + task.missingMode;
    AppendCountedStrings(out, task.variables);
    AppendRows(out, task.rows);
    return out;
}

std::string EncodeMainRTask(const MainRDendrogramTask &task)
{
    if (task.distanceMatrix) {
        return "DENDRO_DISTANCE_MATRIX_NEEDED\t" + task.id + "\t" + task.group + "\t" +
            std::to_string(task.dataVersion) + "\t" + std::to_string(task.revision);
    }
    std::string out = "DENDROGRAM_NEEDED\t" + task.id + "\t" + task.group + "\t" +
        std::to_string(task.revision) + "\t" + std::to_string(task.dataVersion) + "\t" +
        std::to_string(task.imputation) + "\t" + task.scope + "\t" + task.distance + "\t" +
        task.linkage + "\t" + task.missingMode;
    AppendCountedStrings(out, task.variables);
    AppendRows(out, task.rows);
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
    out += "\tIMPUTATION_V1\t" + std::to_string(task.displayedImputation);
    out += "\tEXTRACTION_V1\t" + task.extraction;
    if (task.revision > 0) out += "\tREQUEST_V1\t" + std::to_string(task.revision) +
        "\t" + std::to_string(task.dataVersion);
    return out;
}

std::string EncodeMainRTask(const MainRScaleAnalysisTask &task)
{
    const ScaleAnalysisSpecification &specification = task.specification;
    std::string out = "SCALE_ANALYSIS_NEEDED\t" + task.id + "\t" + task.group + "\t" +
        std::to_string(specification.revision) + "\t" + specification.fingerprint + "\t" +
        specification.correlationBasis + "\t" + specification.scoreMethod + "\t" +
        std::to_string(specification.minimumValidItems) + "\t" +
        (specification.computeOmega ? "TRUE" : "FALSE") + "\t" +
        (specification.showDimensionality ? "TRUE" : "FALSE") + "\t" +
        specification.dimensionalityMethod + "\t" +
        specification.dimensionalityMissingMode + "\t" +
        (specification.dimensionalityScale ? "TRUE" : "FALSE") + "\t" +
        std::to_string(specification.factorCount) + "\t" + specification.extraction + "\t" +
        specification.rotation + "\t" + std::to_string(specification.parallelIterations) + "\t" +
        task.scope;
    AppendRows(out, task.rows);
    out += "\t" + std::to_string(static_cast<long long>(specification.items.size()));
    for (const ScaleItemSpecification &item : specification.items) {
        out += "\t" + item.variable + "\t" + ScaleItemTypeToken(item.type) + "\t" +
            (item.reversed ? "TRUE" : "FALSE") + "\t" +
            (item.hasScoringRange ? "TRUE" : "FALSE") + "\t";
        if (item.hasScoringRange) {
            out += FormatProtocolDouble(item.scoringMinimum) + "\t" +
                FormatProtocolDouble(item.scoringMaximum);
        } else {
            // R's strsplit() drops trailing empty fields.  A scale request
            // whose final item has no scoring range would therefore arrive
            // two fields short and be rejected before fitting.  The values
            // are ignored when hasScoringRange is FALSE, so keep both wire
            // fields explicit with harmless numeric sentinels.
            out += "0\t0";
        }
    }
    return out;
}

std::string EncodeMainRTask(const MainRDimensionalityScoreSaveTask &task)
{
    return "DIMENSIONALITY_SAVE_SCORES_NEEDED\t" + task.requestId + "\t" +
        task.sourceKind + "\t" + task.analysisId + "\t" + task.group + "\t" +
        std::to_string(std::max(1, task.componentCount));
}

void AppendNamedStrings(std::string &out,
                        const std::map<std::string, std::string> &values)
{
    out += "\tTYPES\t" + std::to_string((long long)values.size());
    for (const auto &entry : values) {
        out += "\t" + entry.first + "\t" + entry.second;
    }
}

std::string EncodeMainRTask(const MainRDatasetSyncTask &task)
{
    return "R_DATASET_SYNC_NEEDED\t" + task.requestId + "\t" + task.group +
        "\t" + task.payloadPath;
}

std::string EncodeMainRTask(const MainRGeneralizedComparisonTask &task)
{
    const char *comparisonKind = task.binaryComparison ? "BINARY" :
        (task.countComparison ? "COUNT" : "GENERALIZED");
    std::string out = "GCOMP_NEEDED\t" + task.id + "\t" + task.group + "\t" +
                      task.response + "\t" + task.family + "\t" + task.link + "\t" +
                      task.scope + "\t" + comparisonKind +
                      "\t" + task.eventValue + "\t" + task.referenceValue;
    AppendTermTypes(out, task.termTypes);
    out += "\t" + std::to_string((long long)task.models.size());
    for (const MainRGeneralizedComparisonModelTask &model : task.models) {
        out += "\t" + model.id + "\t" + model.label;
        AppendCountedStrings(out, model.terms);
    }
    AppendRows(out, task.rows);
    // Versioned per-model semantic extension.  The stable prefix above is
    // intentionally retained so an already-loaded older R namespace can
    // still consume the request.  Newer R code reads this canonical model
    // specification and no longer reconstructs every model from global UI
    // fields.
    out += "\tGCOMP_SPEC_V8\t" + std::to_string((unsigned long long)task.generation) +
           "\t" + (task.autoRefit ? "TRUE" : "FALSE") +
           "\t" + (task.countComparison ? "TRUE" : "FALSE") +
           "\t" + task.modelType +
           "\t" + task.datasetType +
           "\t" + task.imputationSetId +
           "\t" + task.sourceDatasetId +
           "\t" + std::to_string(std::max(0, task.imputationCount)) +
           "\t" + std::to_string((long long)task.models.size());
    for (const MainRGeneralizedComparisonModelTask &model : task.models) {
        out += "\t" + model.id + "\t" + model.response + "\t" + model.family +
               "\t" + model.link + "\t" + model.scope +
               "\t" + (model.countRegression ? "TRUE" : "FALSE") +
               "\t" + model.countDistribution + "\t" + model.exposure +
               "\t" + model.offsetVariable +
               "\t" + model.trialsVariable +
               "\t" + FormatProtocolDouble(model.trialsConstant) +
               "\t" + (model.responseBoundsConfigured ? "TRUE" : "FALSE") +
               "\t" + FormatProtocolDouble(model.responseLower) +
               "\t" + FormatProtocolDouble(model.responseUpper) +
               "\t" + std::to_string(model.specificationRevision) +
               "\t" + model.specificationFingerprint;
        AppendCountedStrings(out, model.terms);
        AppendTermTypes(out, model.termTypes);
        AppendCountedStrings(out, model.centeredPredictors);
        AppendTermTypes(out, model.factorReferenceLevels);
    }
    return out;
}

std::string EncodeMainRTask(const MainRTable1Task &task)
{
    std::string out = "TABLE1_NEEDED\t" + task.id + "\t" + task.group + "\t" +
        task.groupVariable + "\t" + (task.includeMissing ? "TRUE" : "FALSE") + "\t" +
        (task.showP ? "TRUE" : "FALSE") + "\t" +
        (task.showTest ? "TRUE" : "FALSE") + "\t" +
        (task.showN ? "TRUE" : "FALSE") + "\t" + task.ordinalAs + "\t" +
        task.scope + "\t" + task.scopeDescription;
    AppendCountedStrings(out, task.variables);
    AppendNamedStrings(out, task.variableTypes);
    AppendRows(out, task.rows);
    if (task.analysisKind != "table1") out += "\tANALYSIS_KIND_V1\t" + task.analysisKind;
    if (task.analysisKind == "nested_contingency") out += "\tCONTINGENCY_OPTIONS_V1\t" + task.contingencyDisplayMode;
    if (task.revision > 0) out += "\tREQUEST_V1\t" + std::to_string(task.revision) + "\t" + std::to_string(task.dataVersion);
    return out;
}

MainRAnalysisWorkflowTask ScaleTotalScoreSaveTask(const std::string &id, const std::string &group,
    const std::string &method, int revision, const std::string &fingerprint)
{
    MainRAnalysisWorkflowTask task;
    task.kind = "scale_save_total"; task.group = group; task.response = id;
    task.method = method; task.rowCondition = std::to_string(revision);
    task.columnCondition = fingerprint;
    return task;
}

std::string EncodeMainRTask(const MainRAnalysisWorkflowTask &task)
{
    std::string out = "ANALYSIS_WORKFLOW_NEEDED\t" + task.kind + "\t" + task.group + "\t" +
        task.response + "\t" + task.secondary + "\t" + task.groupVariable + "\t" +
        task.family + "\t" + task.link + "\t" + task.method + "\t" +
        task.rowCondition + "\t" + task.columnCondition;
    AppendCountedStrings(out, task.variables);
    AppendRows(out, task.rows);
    if (!task.id.empty()) out += "\t" + task.id;
    return out;
}

std::string EncodeMainRTask(const MainRMultipleImputationTask &task)
{
    std::string out = "MULTIPLE_IMPUTATION_NEEDED\t" + task.group + "\t" +
        std::to_string(std::max(1, task.imputations)) + "\t" +
        std::to_string(std::max(0, task.iterations)) + "\t" + task.seed + "\t" +
        (task.openDataSheet ? "TRUE" : "FALSE");
    AppendCountedStrings(out, task.imputeVariables);
    AppendCountedStrings(out, task.predictorVariables);
    AppendTermTypes(out, task.methods);
    return out;
}

std::string EncodeMainRTask(const MainRMIDiagnosticsTask &task)
{
    return "MI_DIAGNOSTICS_NEEDED\t" + task.group + "\t" + task.section + "\t" + task.variable +
        (task.imputationStart>1 ? "\t"+std::to_string(task.imputationStart) : "");
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
    for (const MainRDatasetSyncTask &task : batch.datasetSyncTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRDimensionalityScoreSaveTask &task : batch.dimensionalityScoreSaveTasks) {
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
    for (const MainRGeneralizedComparisonTask &task : batch.generalizedComparisonTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRMixedModelTask &task : batch.mixedModelTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRCorrelationTask &task : batch.correlationTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRDendrogramTask &task : batch.dendrogramTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRDimensionalityTask &task : batch.dimensionalityTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRScaleAnalysisTask &task : batch.scaleAnalysisTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRTable1Task &task : batch.table1Tasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRAnalysisWorkflowTask &task : batch.analysisWorkflowTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    for (const MainRMIDiagnosticsTask &task : batch.miDiagnosticsTasks) AppendTaskLine(out, EncodeMainRTask(task));
    for (const MainRMultipleImputationTask &task : batch.multipleImputationTasks) {
        AppendTaskLine(out, EncodeMainRTask(task));
    }
    return out;
}

} // namespace core
} // namespace rlispstat
