#include "application_state.h"
#include "provenance_model.h"
#include "missing_data_model.h"
#include "linkeda_document.h"
#include <cassert>
#include <numeric>
using namespace rlispstat::core;
int main() {
 ApplicationState app; DataFrameModel data; data.group="scope";data.rows=100;
 DataColumn x; x.name="x"; x.type="numeric"; for(int i=1;i<=100;++i)x.values.push_back(std::to_string(i));data.columns={x};
 app.registerDataset(data);
 AnalysisScope result;bool captured=false;std::string legacy="selected";
 app.captureAnalysisScope("scope",result,captured,&legacy);
 assert(AnalysisScopeRowCount(result,100)==100 && legacy=="all");
 std::set<int> selected;for(int i=1;i<=20;++i)selected.insert(i);app.setSelectedRows("scope",selected);
 assert(app.resolveActiveAnalysisRowIds("scope").size()==100); // brushing is not scope
 assert(app.setActiveAnalysisScopeFromSelection("scope",AnalysisScopeSourceKind::CurrentSelection,"Selected observations"));
 app.captureAnalysisScope("scope",result,captured,&legacy);
 assert(result.originalRowIds.size()==20 && legacy=="selected");
 const auto old=result;
 selected.clear();for(int i=31;i<=100;++i)selected.insert(i);app.setSelectedRows("scope",selected);
 assert(app.resolveActiveAnalysisRowIds("scope").size()==70);
 assert(result.originalRowIds==old.originalRowIds); // result never mutates
 app.captureAnalysisScope("scope",result,captured,&legacy);assert(result.originalRowIds.size()==70);
 app.clearSelectedRows("scope");assert(app.resolveActiveAnalysisRowIds("scope").empty());
 app.captureAnalysisScope("scope",result,captured,&legacy);assert(legacy=="selected"&&result.originalRowIds.empty());
 app.setActiveAnalysisScopeFromUnselected("scope");assert(app.resolveActiveAnalysisRowIds("scope").size()==100);
 app.setSelectedRows("scope",{1,2});assert(app.resolveActiveAnalysisRowIds("scope").size()==98);
 app.saveCurrentSelectionAsAnalysisScope("scope","saved");app.setSelectedRows("scope",{3,4,5});assert(app.resolveActiveAnalysisRowIds("scope")==std::vector<int>({1,2}));
 std::vector<int> rows(80),used(70),excluded(10);std::iota(rows.begin(),rows.end(),1);std::iota(used.begin(),used.end(),11);std::iota(excluded.begin(),excluded.end(),1);
 const auto scope=ExplicitAnalysisScope("scope",rows,AnalysisScopeSourceKind::OtherExplicitSubset,"80 cases",100);
 DataVersionReference version;version.datasetId="scope";version.version=1;
 const auto snapshot=CaptureImmutableAnalysisScope(scope,version,StableRowIdsForCount("scope",100),used,excluded);
 assert(snapshot.scopeN==80&&snapshot.effectiveN==70&&snapshot.excludedStableRowIds.size()==10);
 app.setActiveAnalysisScopeFromUnselected("scope");
 LinkEDADataDocument document,restored;std::string error;
 assert(CreateLinkEDADataDocument(app,"scope",document,&error));
 assert(DecodeLinkEDADataDocument(EncodeLinkEDADataDocument(document,&error),restored,&error));
 assert(restored.analysis_scope.sourceKind==AnalysisScopeSourceKind::CurrentUnselection);
 assert(restored.analysis_scope.originalRowIds.size()==97);
 ApplicationState reopened;assert(ApplyLinkEDADataDocument(restored,reopened,&error));
 assert(reopened.resolveActiveAnalysisRowIds("scope").size()==97);
 // Older documents with supported source ids remain readable.
 document.version=7;document.analysis_scope=AllObservationsAnalysisScope("scope",100);
 assert(DecodeLinkEDADataDocument(EncodeLinkEDADataDocument(document,&error),restored,&error));
 auto task=MissingDataWorkflowTask(data,false,"original","",{"x"},{"x"},{},scope);
 assert(task.rows==rows&&task.family=="explicit");
}
