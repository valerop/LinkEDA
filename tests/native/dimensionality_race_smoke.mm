#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
#include <fstream>
#include <iostream>
static std::vector<std::string> ReadPayload(const std::string &folder,const std::string &name) {
    std::ifstream input(folder+"/"+name+".payload");assert(input);
    std::vector<std::string> lines;std::string line;
    while(std::getline(input,line))lines.push_back(line);return lines;
}
int main(int argc,char **argv){@autoreleasepool{
    assert(argc==2);[NSApplication sharedApplication];
    const std::string folder=argv[1];
    assert(HandleCommandLines(ReadPayload(folder,"dataset")).rfind("OK",0)==0);
    auto &app=MacCommandDispatcher().applicationState();
    auto setScope=[&](int first,int last){std::vector<int> rows;for(int i=first;i<=last;++i)rows.push_back(i);
      assert(app.setActiveAnalysisScope(rlispstat::core::ExplicitAnalysisScope("dimension-race",rows,
        rlispstat::core::AnalysisScopeSourceKind::OtherExplicitSubset,"Cases "+std::to_string(first)+"–"+std::to_string(last),32)));};
    for(const std::string method:{"pca","factor"}){
      setScope(1,24);const std::string id="race-"+method;
      assert(HandleCommandLines({"PCAFA_OPEN",id,"dimension-race",method,"listwise","TRUE","1","none","selected",
        "4","mpg","disp","hp","wt"}).rfind("OK",0)==0);
      auto &state=g_dimensionalityModels.at(id);assert(state.rFitPending && state.requestRevision==1);
      setScope(9,32);RefitDimensionalityState(state);assert(state.requestRevision==2);
      assert(HandleCommandLines(ReadPayload(folder,method+"-1")).find("ignored stale")!=std::string::npos);
      auto reply=HandleCommandLines(ReadPayload(folder,method+"-2"));
      if(reply!="OK\t"+id)std::cerr<<reply<<'\n';assert(reply=="OK\t"+id);
      assert(!state.rFitPending && !state.components.empty() && state.rowsUsed.front()==9 && state.rowsUsed.back()==32);
      assert(app.outputCodeReference(id)->provenance.scope.description=="Cases 9–32");
      PlotModel plot;plot.kind="pca_scree";plot.dimensionalityModelId=id;
      assert(RefreshDimensionalityScreePlot(&plot) && !plot.points.empty());
      setScope(1,0);RefitDimensionalityState(state);assert(state.requestRevision==3 && state.rFitPending);
      assert(HandleCommandLines(ReadPayload(folder,method+"-error"))=="OK\t"+id);
      assert(!state.rFitPending && state.components.empty() && state.scores.empty() && !app.outputCodeReference(id));
      assert(RefreshDimensionalityScreePlot(&plot) && plot.points.empty());
      plot.kind="pca_biplot";plot.points.push_back({1,1,1});
      assert(RefreshDimensionalityBiplotPlot(&plot) && plot.points.empty());
      assert(HandleCommandLines(ReadPayload(folder,method+"-2")).find("ignored stale")!=std::string::npos);
      auto controller=g_dimensionalityControllers.at(id);
      NSButton *autoFit=[controller valueForKey:@"autoFitButton"];
      [autoFit setState:NSControlStateValueOff];[controller autoFitChanged:autoFit];
      [autoFit setState:NSControlStateValueOn];[controller autoFitChanged:autoFit];
      assert(state.rFitPending && state.requestRevision>3);
    }
    std::cout<<"Native PCA/factor: actual R results, stale-scope rejection, error completion and plot clearing passed.\n";
}}
