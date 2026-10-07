#include "command_dispatcher.h"
#include <cassert>
#include <fstream>
#include <iostream>
using namespace rlispstat::core;
static std::vector<std::string> read(const std::string &folder,const std::string &name) {
    std::ifstream in(folder+"/"+name+".payload");assert(in);std::string line;std::vector<std::string> p;
    while(std::getline(in,line))p.push_back(line);return p;
}
int main(int argc,char **argv) {
    assert(argc==2);const std::string folder=argv[1];int shown=0;Table1DisplayState last;
    CommandDispatcherServices svc;svc.ui.showTable1=[&](const auto&s){last=s;++shown;};
    CommandDispatcher d(svc);assert(d.dispatch(read(folder,"dataset")).rfind("OK",0)==0);
    auto &app=d.applicationState();auto *df=app.datasets().find("table1-r");assert(df);
    MainRTable1Task task;task.id="native-table";task.group=df->group;task.variables={"mpg","vs","gear"};task.groupVariable="am";
    auto begin=[&](){d.prepareTable1Task(task);};
    begin();assert(task.revision==1 && task.variableTypes.at("gear")=="ordinal");
    assert(EncodeMainRTask(task).find("REQUEST_V1\t1\t1")!=std::string::npos);
    auto first=read(folder,"result-1");assert(d.dispatch(first)=="OK\tnative-table");assert(shown==1);
    assert(app.outputCodeReference(task.id));assert(d.dispatch(first).find("ignored stale")!=std::string::npos);
    task.scope="selected";task.scopeDescription="Chosen cases";
    for(int i=1;i<=12;++i)task.rows.push_back(i);for(int i=25;i<=32;++i)task.rows.push_back(i);
    auto scope=ExplicitAnalysisScope(task.group,task.rows,AnalysisScopeSourceKind::OtherExplicitSubset,task.scopeDescription,32);
    assert(app.setActiveAnalysisScope(scope));begin();assert(task.revision==2);
    assert(!app.outputCodeReference(task.id));assert(d.dispatch(first).find("ignored stale")!=std::string::npos);
    auto second=read(folder,"result-2");assert(d.dispatch(second)=="OK\tnative-table");assert(shown==2);
    assert(last.dataScope.originalRowIds==task.rows);
    // Data mutation invalidates a pending reply before a UI refit is queued.
    begin();auto pending=second;pending[4]=std::to_string(task.revision);
    auto changed=*df;++changed.dataVersion;changed.columns[0].type="ordered";assert(app.registerDataset(changed));
    assert(d.dispatch(pending).find("ignored stale")!=std::string::npos);assert(shown==2);
    begin();assert(task.dataVersion==changed.dataVersion && task.variableTypes.at("mpg")=="ordinal");
    auto error=std::vector<std::string>{"TABLE1_OPEN_ERROR",task.id,task.group,"REQUEST_V1",std::to_string(task.revision),std::to_string(task.dataVersion),"Fit failed"};
    assert(d.dispatch(error)=="OK");assert(last.statusText=="Fit failed" && !last.needsRFit);
    assert(!app.outputCodeReference(task.id));assert(d.dispatch(error).find("ignored stale")!=std::string::npos);
    begin();pending[4]=std::to_string(task.revision);pending[5]=std::to_string(task.dataVersion);
    d.cancelTable1Task(task.id);assert(d.dispatch(pending).find("ignored stale")!=std::string::npos);
    // Empty variable lists are valid editable requests, not implicit defaults.
    task.variables.clear();begin();assert(task.variables.empty());
    auto blank=Table1PendingStateForDataFrame(changed,task.id,{},"",{},&scope);
    assert(blank.needsRFit && blank.rows.empty());
    // Changes to a named active scope reject results even before the next request.
    task.variables={"mpg","vs","gear"};begin();pending=second;pending[4]=std::to_string(task.revision);pending[5]=std::to_string(task.dataVersion);
    scope.sourceDescription="Renamed scope";assert(app.setActiveAnalysisScope(scope));
    assert(d.dispatch(pending).find("ignored stale")!=std::string::npos);
    std::cout<<"Table 1 shared transport: R results, version, scope, types, duplicates and errors passed.\n";
}
