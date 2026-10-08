#include "command_dispatcher.h"
#include <cassert>
#include <iostream>
using namespace rlispstat::core;
int main() {
    CommandDispatcherServices services;
    CommandDispatcher *active=nullptr;
    services.queries.groupSeed=[&](const std::string &,PlotModel &seed) {
        PopulateDatasetSeedPlot(seed,*active->applicationState().datasets().find("corr"),"corr");return true;
    };
    services.ui.showCorrelationMatrix=[](const std::string &) {};
    CommandDispatcher dispatcher(services); active=&dispatcher;
    auto &app=dispatcher.applicationState();
    DataFrameModel data;data.group="corr";data.rows=4;
    DataColumn x,y;x.name="x";y.name="y";x.type=y.type="numeric";
    x.values={"1","2","3","4"};y.values={"4","2","1","3"};data.columns={x,y};app.registerDataset(data);
    auto &state=app.correlationMatrices()["matrix"];state.id="matrix";state.group="corr";state.variables={"x","y"};
    const auto first=PrepareCorrelationRTask(app,state);
    auto response=[&](std::uint64_t revision) {
        std::vector<std::string> r={"CORR_UPDATE","matrix",std::to_string(revision),std::to_string(first.dataVersion),"ok",
            "matrix","corr","Pearson Correlation Matrix","pearson","pairwise","TRUE","FALSE","FALSE","0","","Calculated in R","2","x","y","4"};
        for(const std::string y:{"x","y"})for(const std::string x:{"x","y"}){
            bool diagonal=x==y;std::vector<std::string> c={x,y,diagonal?"NA":"-0.4",diagonal?"NA":"0.6","4",diagonal?"diagonal":"valid","R result","4","1","2","3","4"};r.insert(r.end(),c.begin(),c.end());
        }
        std::vector<std::string> tail={"CORR_SCOPE_V1","all","All observations","4","4","1","2","3","4",
            "ANALYSIS_PROVENANCE_V2","matrix","Correlation","corr",std::to_string(first.dataVersion),"data.frame","0","recorded","78","0","2","x","y","1","table","78","0"};
        r.insert(r.end(),tail.begin(),tail.end());return r;
    };
    assert(state.rFitPending && state.cells.empty());
    auto second=PrepareCorrelationRTask(app,state);
    assert(dispatcher.dispatch(response(first.revision)).find("ignored stale")!=std::string::npos);
    state.showP=false;state.showN=true;
    assert(dispatcher.dispatch({"CORR_SET_PART","matrix","lower"})=="OK");
    assert(state.requestRevision==second.revision && state.rFitPending);
    auto accepted=dispatcher.dispatch(response(second.revision));if(accepted.rfind("OK",0)!=0)std::cerr<<accepted<<"\n";
    assert(accepted.rfind("OK",0)==0);
    assert(!state.rFitPending && !state.precomputed && state.cells.size()==4 && !state.showP && state.showN);
    assert(state.cells[1].r==-.4 && state.cells[1].rowsUsed==std::vector<int>({1,2,3,4}));
    assert(app.outputCodeReference(state.id));
    assert(state.displayPart=="lower");
    assert(app.outputCodeReference(state.id)->publication.table->rows[0][2].text.empty());
    assert(!app.outputCodeReference(state.id)->publication.table->rows[1][1].text.empty());
    const auto revision=state.requestRevision;
    state.selectedRow=1;state.selectedCol=0;
    assert(dispatcher.dispatch({"CORR_SET_PART","matrix","upper"})=="OK");
    assert(state.selectedRow==-1 && state.requestRevision==revision && state.cells.size()==4);
    assert(dispatcher.dispatch({"CORR_TOGGLE_DISPLAY","matrix","show_p_value"})=="OK");
    assert(state.showPValue && state.requestRevision==revision);
    assert(dispatcher.dispatch({"CORR_SET_PART","matrix","invalid"}).rfind("ERR",0)==0);

    assert(dispatcher.dispatch(response(second.revision)).find("ignored stale")!=std::string::npos);
    app.setSelectedRows("corr",{2,4});app.setActiveAnalysisScopeFromSelection("corr",AnalysisScopeSourceKind::CurrentSelection,"Named scope");
    auto selected=PrepareCorrelationRTask(app,state);
    assert(selected.rows==std::vector<int>({2,4}) && selected.scope=="selected");
    assert(!app.outputCodeReference(state.id));
    assert(dispatcher.dispatch(response(selected.revision)).find("outside requested scope")!=std::string::npos);
    auto error=std::vector<std::string>{"CORR_UPDATE","matrix",std::to_string(selected.revision),std::to_string(selected.dataVersion),"error","Data unavailable"};
    assert(dispatcher.dispatch(error).rfind("OK",0)==0 && !state.rFitPending && state.cells.empty());
    app.clearSelectedRows("corr");auto empty=PrepareCorrelationRTask(app,state);
    assert(empty.scope=="selected" && empty.rows.empty());
    auto message=EncodeMainRTask(empty);assert(message.find("\tselected\tpearson\tpairwise\t2\tx\ty\t0")!=std::string::npos);
    MainRTaskBatch batch;batch.correlationTasks.push_back(empty);assert(AppendMainRTaskMessages("OK",batch).find(message)!=std::string::npos);
    // Data-version changes invalidate an in-flight result even without a new request.
    auto *changed=app.datasets().find("corr");++changed->dataVersion;
    assert(dispatcher.dispatch(response(empty.revision)).find("ignored stale")!=std::string::npos);
    std::cout<<"R correlation transport, immutable scopes, stale responses and display flags passed.\n";
}
