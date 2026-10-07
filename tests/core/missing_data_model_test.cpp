#include "../../src/core/missing_data_model.h"
#include <cassert>
using namespace rlispstat::core;
int main(){
 DataFrameModel d;d.group="missing";d.rows=3;DataColumn c;c.name="score";c.type="numeric";c.values={"1","NA","3"};d.columns={c};
 auto caps=MissingDataCapabilitiesFor(d);assert(caps.overview&&caps.models&&caps.imputation&&!caps.diagnostics);
 d.columns[0].values={"1","2","3"};caps=MissingDataCapabilitiesFor(d);assert(!caps.models&&!caps.imputation&&!caps.diagnostics);
 d.datasetType="multiple_imputation";d.imputationCount=3;d.imputationProcess="retained metadata";
 d.columns[0].imputedMissing={false,true,false};d.columns[0].imputationOriginalSparse[1]="NA";
 caps=MissingDataCapabilitiesFor(d);assert(caps.original&&!caps.models&&!caps.imputation&&caps.diagnostics);assert(caps.overview);assert(caps.modelsUnavailableReason.find("original incomplete dataset")!=std::string::npos);assert(caps.targets==std::vector<std::string>{"score"});
 d.imputationProcess.clear();assert(!MissingDataCapabilitiesFor(d).diagnostics);
 auto scope=ExplicitAnalysisScope(d.group,{},AnalysisScopeSourceKind::CurrentSelection,"Empty selection",3);
 auto task=MissingDataWorkflowTask(d,false,"original","",{"score"},{"age","group"},{"pattern","indicators"},scope);
 assert(task.family=="explicit"&&task.rows.empty());assert(task.rowCondition==std::string("age")+char(31)+"group");assert(task.method==std::string("pattern")+char(31)+"indicators");
 assert(EncodeMainRTask(task).find("missing_data_overview")!=std::string::npos);
}
