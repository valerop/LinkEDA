#include "command_dispatcher.h"
#include <cassert>
#include <iostream>
using namespace rlispstat::core;
int main() {
    CommandDispatcher dispatcher(CommandDispatcherServices{});
    auto &app=dispatcher.applicationState();
    DataFrameModel data; data.group="cluster"; data.rows=4;
    DataColumn x; x.name="x"; x.type="numeric"; x.values={"1","4","7","9"}; data.columns={x};
    app.registerDataset(data);
    DendrogramState model; model.id="tree";model.group="cluster";model.variables={"x"};
    auto &state=app.dendrograms()[model.id];state=model;
    const auto first=PrepareDendrogramRTask(app,state);
    assert(state.rFitPending && state.caseRows.empty());
    auto reply=[&](std::uint64_t revision) {
        std::vector<std::string> result={"DENDRO_UPDATE","tree",std::to_string(revision),
            std::to_string(first.dataVersion),"ok","R result","2","1","3","2","0","1","1","0","1","2.5",
            "ANALYSIS_PROVENANCE_V2","tree","Quick Cluster","cluster",std::to_string(first.dataVersion),
            "data.frame","0","recorded","78","0","1","x","1","plot","78","0"};
        return result;
    };
    const auto second=PrepareDendrogramRTask(app,state);
    assert(dispatcher.dispatch(reply(first.revision)).find("ignored stale")!=std::string::npos);
    assert(state.rFitPending && state.caseRows.empty());
    assert(dispatcher.dispatch(reply(second.revision)).rfind("OK",0)==0);
    assert(!state.rFitPending && state.caseRows==std::vector<int>({1,3}));
    assert(state.merges[0].height==2.5 && app.outputCodeReference("tree"));
    // A repeated terminal reply may not overwrite the accepted tree.
    auto duplicate=reply(second.revision); duplicate[15]="99";
    assert(dispatcher.dispatch(duplicate).find("ignored stale")!=std::string::npos);
    assert(state.merges[0].height==2.5);
    app.setSelectedRows("cluster",{2,4}); app.setActiveAnalysisScopeFromSelection("cluster",AnalysisScopeSourceKind::CurrentSelection,"Selected cases");
    const auto selected=PrepareDendrogramRTask(app,state);
    assert(selected.rows==std::vector<int>({2,4}) && selected.scope=="selected");
    assert(state.rFitPending && state.caseRows==std::vector<int>({1,3}));
    assert(state.status.find("previous tree shown")!=std::string::npos);
    assert(!app.outputCodeReference("tree"));
    assert(dispatcher.dispatch(reply(selected.revision)).find("outside requested scope")!=std::string::npos);
    auto failed=std::vector<std::string>{"DENDRO_UPDATE","tree",std::to_string(selected.revision),
        std::to_string(selected.dataVersion),"error","Undefined distances","0","0","0"};
    assert(dispatcher.dispatch(failed).rfind("OK",0)==0);
    assert(!state.rFitPending && state.caseRows.empty() && state.status=="Undefined distances");
    app.clearSelectedRows("cluster");
    const auto empty=PrepareDendrogramRTask(app,state);
    assert(empty.scope=="selected" && empty.rows.empty());
    auto message=EncodeMainRTask(empty);
    assert(message.find("DENDROGRAM_NEEDED\ttree\tcluster\t")==0);
    assert(message.find("\tselected\teuclidean\taverage\tpairwise\t1\tx\t0")!=std::string::npos);
    MainRTaskBatch batch;batch.dendrogramTasks.push_back(empty);
    assert(AppendMainRTaskMessages("OK",batch).find(message)!=std::string::npos);
    MainRDendrogramTask matrixTask=empty;
    matrixTask.distanceMatrix=true;
    assert(EncodeMainRTask(matrixTask)=="DENDRO_DISTANCE_MATRIX_NEEDED\ttree\tcluster\t"+
        std::to_string(empty.dataVersion)+"\t"+std::to_string(empty.revision));
    std::cout<<"R tree transport, stale-result rejection, scope and errors passed.\n";
}
