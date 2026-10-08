#include "command_dispatcher.h"
#include <cassert>
#include <iostream>
using namespace rlispstat::core;
int main() {
    CommandDispatcherServices services;
    services.ui.showDimensionality=[](const std::string&){};
    services.ui.refreshDimensionalityPlots=[](const std::string&){};
    CommandDispatcher dispatcher(services);auto &app=dispatcher.applicationState();
    DataFrameModel data;data.group="data";data.rows=4;
    DataColumn x,y;x.name="x";y.name="y";x.type=y.type="numeric";
    x.values={"1","2","3","4"};y.values={"4","2","1","3"};data.columns={x,y};
    assert(app.registerDataset(data));
    auto &s=app.dimensionalityModels()["dim"];s.id="dim";s.group="data";s.variables={"x","y"};
    auto scope=[&](std::vector<int> rows,const std::string &name){
        assert(app.setActiveAnalysisScope(ExplicitAnalysisScope("data",rows,
            AnalysisScopeSourceKind::OtherExplicitSubset,name,4)));
    };
    scope({1,2},"First scope");auto first=PrepareDimensionalityRTask(app,s);assert(first);
    assert(first->rows==std::vector<int>({1,2}) && first->scope=="selected");
    assert(EncodeMainRTask(*first).find("REQUEST_V1\t1\t1")!=std::string::npos);
    assert(!PrepareDimensionalityRTask(app,s)); // Same pending request is not duplicated.
    auto response=[&](const MainRDimensionalityTask &task,std::vector<int> rows){
        std::vector<std::string> p={"PCAFA_UPDATE","dim","REQUEST_V1",std::to_string(task.revision),
          std::to_string(task.dataVersion),"ok","data","pca","listwise","none",task.scope,"TRUE","2",
          "fitted","2","x","y",std::to_string(rows.size())};
        for(int row:rows)p.push_back(std::to_string(row));
        std::vector<std::string> rest={"0","1","1","1.5","NA",".75",".75","0","0",
          "IMPUTATION_V1",std::to_string(task.displayedImputation),"1",
          "ANALYSIS_PROVENANCE_V2","dim","PCA","data",std::to_string(task.dataVersion),"data.frame","0",
          "recorded","78","0","2","x","y","1","table","78","0"};
        p.insert(p.end(),rest.begin(),rest.end());return p;
    };
    auto old=response(*first,{1,2});
    scope({3,4},"Second scope"); // Reject immediately, even before the refit callback.
    assert(dispatcher.dispatch(old).find("ignored stale")!=std::string::npos);
    auto second=PrepareDimensionalityRTask(app,s);assert(second && second->revision>first->revision);
    assert(dispatcher.dispatch(old).find("ignored stale")!=std::string::npos);
    auto current=response(*second,{3,4});
    assert(dispatcher.dispatch(current)=="OK\tdim");
    assert(s.rowsUsed==std::vector<int>({3,4}) && !s.rFitPending);
    assert(app.outputCodeReference("dim")->provenance.scope.description=="Second scope");
    assert(!PrepareDimensionalityRTask(app,s)); // A current completed fit is reused.
    assert(dispatcher.dispatch(current).find("ignored stale")!=std::string::npos);
    scope({1,2},"First scope");auto third=PrepareDimensionalityRTask(app,s);assert(third);
    assert(dispatcher.dispatch(old).find("ignored stale")!=std::string::npos); // A -> B -> A.
    assert(!app.outputCodeReference("dim") && s.components.empty());
    auto badRows=response(*third,{3,4});
    assert(dispatcher.dispatch(badRows).find("outside its scope")!=std::string::npos);
    assert(s.rFitPending && s.components.empty());
    auto changed=*app.datasets().find("data");++changed.dataVersion;assert(app.registerDataset(changed));
    assert(dispatcher.dispatch(response(*third,{1,2})).find("ignored stale")!=std::string::npos);
    auto fourth=PrepareDimensionalityRTask(app,s);assert(fourth && fourth->dataVersion!=third->dataVersion);
    s.autoFit=false;PrepareDimensionalityRTask(app,s);
    assert(!s.rFitPending && !app.outputCodeReference("dim"));
    assert(dispatcher.dispatch(response(*fourth,{1,2})).find("ignored stale")!=std::string::npos);
    s.autoFit=true;scope({},"Empty scope");auto empty=PrepareDimensionalityRTask(app,s);
    assert(empty && empty->scope=="selected" && empty->rows.empty());
    assert(dispatcher.dispatch({"PCAFA_UPDATE","dim","REQUEST_V1",std::to_string(empty->revision),
      std::to_string(empty->dataVersion),"error","No complete cases"})=="OK\tdim");
    assert(!s.rFitPending && s.components.empty() && s.status=="No complete cases");
    assert(!PrepareDimensionalityRTask(app,s)); // A UI refresh must not loop on the error.
    auto legacy=current;legacy.erase(legacy.begin()+2,legacy.begin()+6);
    assert(dispatcher.dispatch(legacy).find("unversioned")!=std::string::npos);
    auto mi=*app.datasets().find("data");mi.datasetType="multiple_imputation";mi.imputationCount=2;
    mi.activeImputationVersion=1;assert(app.registerDataset(mi));
    scope({1,2},"MI scope");auto miFirst=PrepareDimensionalityRTask(app,s);assert(miFirst);
    s.requestedImputation=2;
    assert(dispatcher.dispatch(response(*miFirst,{1,2})).find("ignored stale")!=std::string::npos);
    auto miSecond=PrepareDimensionalityRTask(app,s);assert(miSecond && miSecond->displayedImputation==2);
    scope({1,2},"Renamed scope");
    auto renamed=PrepareDimensionalityRTask(app,s);assert(renamed && renamed->revision>miSecond->revision);
    assert(s.dataScope.sourceDescription=="Renamed scope");
    std::cout<<"Dimensionality: scope/data/revision rejection, cache identity, cancellation and errors passed.\n";
}
