#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
#include <fstream>
#include <iostream>
static std::vector<std::string> Payload(const std::string &folder,const std::string &name) {
    std::ifstream input(folder+"/"+name+".payload"); assert(input);
    std::vector<std::string> lines;std::string line;while(std::getline(input,line))lines.push_back(line);return lines;
}
int main(int argc,char **argv){ @autoreleasepool {
    assert(argc==2);[NSApplication sharedApplication];const std::string folder=argv[1];
    assert(HandleCommandLines(Payload(folder,"dataset")).rfind("OK",0)==0);
    auto &dispatcher=MacCommandDispatcher();auto &app=dispatcher.applicationState();
    const auto *df=app.datasets().find("table1-r");assert(df);
    auto scope=app.activeAnalysisScope(df->group);
    auto initial=rlispstat::core::Table1PendingStateForDataFrame(*df,"native-table",{"mpg","vs","gear"},"am",{},&scope);
    ShowTable1StateOnMain(initial);
    assert(g_pendingTable1Tasks.size()==1 && g_pendingTable1Tasks.back().revision==1);
    auto *controller=g_table1Controllers.at("native-table");
    assert([controller state]->codeReference.outputId.empty());
    std::vector<int> rows;for(int i=1;i<=12;++i)rows.push_back(i);for(int i=25;i<=32;++i)rows.push_back(i);
    auto selected=rlispstat::core::ExplicitAnalysisScope(df->group,rows,
        rlispstat::core::AnalysisScopeSourceKind::OtherExplicitSubset,"Chosen cases",32);
    assert(app.setActiveAnalysisScope(selected));
    assert(HandleCommandLines(Payload(folder,"result-1")).find("ignored stale")!=std::string::npos);
    [controller rebuildFromBackendData];
    assert(g_pendingTable1Tasks.size()==1 && g_pendingTable1Tasks.back().revision==2);
    const auto reply=HandleCommandLines(Payload(folder,"result-2"));
    if(reply!="OK\tnative-table")std::cerr<<reply<<'\n';assert(reply=="OK\tnative-table");
    assert([controller state]->rows.size()>3);
    assert([controller state]->dataScope.originalRowIds==rows);
    const auto *reference=app.outputCodeReference("native-table");assert(reference);
    assert(!reference->provenance.executedRCode.empty());
    assert(reference->provenance.scope.description=="Chosen cases");
    assert(HandleCommandLines(Payload(folder,"result-2")).find("ignored stale")!=std::string::npos);
    auto legacy=Payload(folder,"result-2");legacy.erase(legacy.begin()+3,legacy.begin()+6);
    assert(HandleCommandLines(legacy).find("unversioned")!=std::string::npos);
    auto empty=rlispstat::core::ExplicitAnalysisScope(df->group,{},
        rlispstat::core::AnalysisScopeSourceKind::OtherExplicitSubset,"Chosen cases",32);
    assert(app.setActiveAnalysisScope(empty));[controller rebuildFromBackendData];
    assert(g_pendingTable1Tasks.back().revision==3);
    assert(!app.outputCodeReference("native-table"));
    assert(HandleCommandLines(Payload(folder,"error"))=="OK");
    assert([controller state]->statusText.find("No rows")!=std::string::npos);
    for(const auto &row:[controller state]->rows)for(const auto &value:row.values)assert(value.empty());
    assert(g_pendingTable1Tasks.back().revision==3); // No error/refit loop.
    // A subsequent edit uses the current canonical type and dataset version.
    auto changed=*df;++changed.dataVersion;changed.columns[0].type="ordered";
    assert(app.registerDataset(changed));
    assert(HandleCommandLines(Payload(folder,"result-2")).find("ignored stale")!=std::string::npos);
    [controller rebuildFromBackendData];auto closedReply=Payload(folder,"result-2");
    closedReply[4]=std::to_string(g_pendingTable1Tasks.back().revision);
    closedReply[5]=std::to_string(g_pendingTable1Tasks.back().dataVersion);
    [[controller getWindow] close];assert(g_table1Controllers.count("native-table")==0);
    assert(HandleCommandLines(closedReply).find("ignored stale")!=std::string::npos);
    assert(g_table1Controllers.count("native-table")==0);
    std::cout<<"Native Table 1: R task, scope/revision rejection, rendering, provenance and terminal error passed.\n";
    return 0;
}}
