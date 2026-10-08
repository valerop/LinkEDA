#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
#include <fstream>
#include <iostream>
static std::vector<std::string> Payload(const std::string &folder,const std::string &name) {
    std::ifstream in(folder+"/"+name+".payload");assert(in);std::string line;std::vector<std::string> p;
    while(std::getline(in,line))p.push_back(line);return p;
}
int main(int argc,char **argv){@autoreleasepool{
    assert(argc==2);[NSApplication sharedApplication];const std::string folder=argv[1];
    auto &app=MacCommandDispatcher().applicationState();
    for(const std::string mode:{"ordinary","mi"}) {
        auto dataset=Payload(folder,mode+"-dataset");const auto group=dataset[1];
        assert(HandleCommandLines(dataset).rfind("OK",0)==0);
        const auto *df=app.datasets().find(group);assert(df);
        assert(app.setActiveAnalysisScope(rlispstat::core::ExplicitAnalysisScope(group,{1,2,3,4},
            rlispstat::core::AnalysisScopeSourceKind::OtherExplicitSubset,"Chosen cases",6)));
        PlotModel seed;rlispstat::core::PopulateDatasetSeedPlot(seed,*df,group);
        ShowFrequencyTableForVariableOnMain(&seed,"x");assert(!g_pendingTable1Tasks.empty());
        auto task=g_pendingTable1Tasks.back();assert(task.analysisKind=="frequency");
        assert(task.variables==std::vector<std::string>({"x"}) && task.rows==std::vector<int>({1,2,3,4}));
        auto payload=Payload(folder,mode+"-result");payload[1]=task.id;payload[4]=std::to_string(task.revision);payload[5]=std::to_string(task.dataVersion);
        auto reply=HandleCommandLines(payload);if(reply!="OK\t"+task.id)std::cerr<<reply<<'\n';assert(reply=="OK\t"+task.id);
        auto *controller=g_table1Controllers.at(task.id);auto *state=[controller state];
        assert(state->tableType=="frequency" && state->columns==std::vector<std::string>({"N","Percent"}));
        assert(state->rows[0].values[0]==(mode=="mi"?"3.5":"3"));
        assert(state->codeReference.publication.table && state->codeReference.publication.table->rows.size()==state->rows.size());
        assert(!state->codeReference.provenance.executedRCode.empty());
        auto *menu=[controller contextMenuForRow:0];assert([menu itemWithTitle:@"Variable"] && [menu itemWithTitle:@"Export"]);
        auto *choice=[[NSMenuItem alloc] initWithTitle:@"g" action:nil keyEquivalent:@""];[choice setRepresentedObject:@"g"];
        [controller frequencyVariableSelected:choice];
        assert(g_pendingTable1Tasks.back().analysisKind=="frequency" && g_pendingTable1Tasks.back().variables==std::vector<std::string>({"g"}));
        assert(HandleCommandLines(payload).find("ignored stale")!=std::string::npos);
        [[controller getWindow] close];
    }
    std::cout<<"Native frequencies: R route, ordinary/MI counts, scope, variable menu, stale reply rejection and publication passed.\n";
}}
