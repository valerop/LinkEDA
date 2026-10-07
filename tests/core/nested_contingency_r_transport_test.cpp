
#include "command_dispatcher.h"
#include <cassert>
#include <fstream>
#include <iostream>
using namespace rlispstat::core;
static std::vector<std::string> read(const std::string& folder,const std::string& name) {
 std::ifstream in(folder+"/"+name+".payload");assert(in);std::vector<std::string> out;std::string line;
 while(std::getline(in,line))out.push_back(line);return out;
}
int main(int argc,char** argv) {
 assert(argc==2);std::string folder=argv[1];int shown=0;Table1DisplayState state;
 CommandDispatcherServices svc;svc.ui.showTable1=[&](const auto& x){state=x;++shown;};CommandDispatcher d(svc);
 assert(d.dispatch(read(folder,"dataset")).rfind("OK",0)==0);auto& app=d.applicationState();
 auto scope=ExplicitAnalysisScope("nested-transport",{1,2,3,4,5},AnalysisScopeSourceKind::OtherExplicitSubset,"Chosen cases",6);
 assert(app.setActiveAnalysisScope(scope));
 MainRTable1Task task;task.id="nested-result";task.group="nested-transport";task.analysisKind="nested_contingency";
 task.variables={"a","b"};task.groupVariable="g";task.scope="selected";task.rows={1,2,3,4,5};task.scopeDescription="Chosen cases";
 task.sourcePlotId="bar-source";task.linkEnabled=true;
 for(const auto mode:{"count","percent","count_percent"}) {
  task.contingencyDisplayMode=mode;d.prepareTable1Task(task);
  assert(EncodeMainRTask(task).find(std::string("CONTINGENCY_OPTIONS_V1\t")+mode+"\tREQUEST_V1")!=std::string::npos);
  auto payload=read(folder,mode);payload[4]=std::to_string(task.revision);
  auto wrong=payload;auto tag=std::find(wrong.begin(),wrong.end(),"MI_CONTINGENCY_LAYOUT_V1");assert(tag!=wrong.end());*(tag+1)=mode==std::string("count")?"percent":"count";
  assert(d.dispatch(wrong).rfind("ERR",0)==0);
  auto reply=d.dispatch(payload);if(reply.rfind("OK",0)!=0)std::cerr<<reply<<'\n';assert(reply=="OK\tnested-result");
  assert(state.tableType=="nested_contingency" && state.nestedDisplayMode==mode && state.sourcePlotId=="bar-source" && state.linkEnabled);
  assert(state.rows.back().rowRows==std::vector<int>({1,2,3,4}));
  assert(ContingencyRowsForRowPrefix(state,{"A"})==std::set<int>({1,2}));
  assert(ContingencyRowsForSplitColumn(state,0)==std::set<int>({1,3}));
  assert(!state.codeReference.provenance.executedRCode.empty());
  const auto& pub=*state.codeReference.publication.table;
  assert(pub.stubColumns.size()==2 && pub.rows.size()==state.rows.size());
  assert(pub.columns.size()==(mode==std::string("count_percent")?8:5));
  assert(d.dispatch(payload).find("ignored stale")!=std::string::npos);
  d.prepareTable1Task(task);assert(!app.outputCodeReference(task.id));
  assert(d.dispatch(payload).find("ignored stale")!=std::string::npos);
  payload[4]=std::to_string(task.revision);d.cancelTable1Task(task.id);assert(d.dispatch(payload).find("ignored stale")!=std::string::npos);
 }
 d.prepareTable1Task(task);auto error=std::vector<std::string>{"TABLE1_OPEN_ERROR",task.id,task.group,"REQUEST_V1",std::to_string(task.revision),std::to_string(task.dataVersion),"No complete observations"};
 assert(d.dispatch(error)=="OK");assert(state.tableType=="nested_contingency" && !state.needsRFit && state.rows.empty());
 assert(state.stubHeaders==task.variables && state.sourcePlotId==task.sourcePlotId);
 std::cout<<"Nested contingency transport, modes, raw publication, scope, linking and stale requests passed.\n";
}
