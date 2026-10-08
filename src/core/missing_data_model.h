#ifndef LINKEDA_MISSING_DATA_MODEL_H
#define LINKEDA_MISSING_DATA_MODEL_H
#include "dataset_model.h"
#include "main_r_task_model.h"
#include <algorithm>
namespace rlispstat { namespace core {
struct MissingDataCapabilities {
    bool overview=false, models=false, imputation=false, diagnostics=false, original=false;
    std::vector<std::string> targets;
    std::string modelsUnavailableReason;
};
inline MissingDataCapabilities MissingDataCapabilitiesFor(const DataFrameModel& data) {
    MissingDataCapabilities out;
    out.overview=data.rows>0 && !data.columns.empty();
    out.original=data.datasetType=="multiple_imputation" && data.imputationCount>0;
    out.diagnostics=out.original && !data.imputationProcess.empty();
    bool activeMissing=false;
    for(const auto& col:data.columns) {
        bool missing=false, observed=false;
        for(int row=0;row<data.rows;++row) {
            const auto value=out.original && DataFrameCellIsImputed(data,col,row) ? OriginalImputationValueForCell(col,row) :
                (static_cast<std::size_t>(row)<col.values.size()?col.values[row]:"");
            if(DataCellIsMissing(value)) missing=true; else observed=true;
            if(static_cast<std::size_t>(row)<col.values.size() && DataCellIsMissing(col.values[row])) activeMissing=true;
        }
        if(missing && observed) out.targets.push_back(col.name);
    }
    out.models=data.datasetType!="multiple_imputation" && !out.targets.empty();
    if(data.datasetType=="multiple_imputation") out.modelsUnavailableReason="Missingness models are disabled for imputed datasets. Open the original incomplete dataset to model missingness.";
    else if(!out.models) out.modelsUnavailableReason="Missingness models require a variable with both missing and observed values.";
    out.imputation=activeMissing && !out.original;
    return out;
}
inline MainRAnalysisWorkflowTask MissingDataWorkflowTask(const DataFrameModel& data,
    bool models,const std::string& source,const std::string& target,
    const std::vector<std::string>& variables,const std::vector<std::string>& descriptions,
    const std::vector<std::string>& save,const AnalysisScope& scope) {
    MainRAnalysisWorkflowTask task;
    task.kind=models?"missingness_model":"missing_data_overview";
    task.group=data.group; task.response=target; task.secondary=source; task.variables=variables;
    auto join=[](const std::vector<std::string>& values){std::string out;for(const auto& v:values){if(!out.empty())out+='\x1f';out+=v;}return out;};
    task.rowCondition=join(descriptions); task.method=join(save);
    task.columnCondition=std::to_string(data.dataVersion);
    task.family=scope.kind==AnalysisScopeKind::AllObservations?"all":"explicit";
    if(task.family!="all") task.rows=ResolveAnalysisScopeRowIds(scope,data.rows);
    return task;
}
inline MainRAnalysisWorkflowTask MissingPatternActionTask(const std::string& group,
    const std::string& id,const std::string& action,const std::string& value) {
    MainRAnalysisWorkflowTask task;
    task.kind="missing_data_action";task.group=group;task.response=id;
    task.secondary=action;task.rowCondition=value;task.family="all";
    return task;
}
}}
#endif
