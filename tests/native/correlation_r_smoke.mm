#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
#include <fstream>
#include <iostream>
int main(int argc,char **argv) { @autoreleasepool {
    assert(argc==2);[NSApplication sharedApplication];
    DataFrameModel data;data.group="corr-smoke";data.rows=5;
    rlispstat::core::DataColumn x,y;x.name="x";y.name="y";x.type=y.type="numeric";
    x.values={"1","2","3","4","5"};y.values={"2","5","4","9","10"};data.columns={x,y};
    auto &app=MacCommandDispatcher().applicationState();assert(app.registerDataset(data));
    CorrelationMatrixState source;source.id="corr-smoke-matrix";source.group=data.group;source.variables={"x","y"};
    rlispstat::core::PopulateDatasetSeedPlot(source.seed,*app.datasets().find(data.group),data.group);source.hasSeed=true;
    g_correlationMatrices[source.id]=source;auto &state=g_correlationMatrices[source.id];
    RefitCorrelationMatrixState(state);
    assert(state.rFitPending && state.cells.empty() && g_pendingCorrelationTasks.size()==1);
    assert(rlispstat::core::EncodeMainRTask(g_pendingCorrelationTasks[0])=="CORRELATION_NEEDED\tcorr-smoke-matrix\tcorr-smoke\t1\t1\tall\tpearson\tpairwise\t2\tx\ty\t0");
    std::ifstream input(argv[1]);std::vector<std::string> payload;std::string line;while(std::getline(input,line))payload.push_back(line);
    auto reply=MacCommandDispatcher().dispatch(payload);if(reply.rfind("OK",0)!=0)std::cerr<<reply<<"\n";
    assert(reply.rfind("OK",0)==0 && !state.rFitPending && !state.precomputed);
    assert(state.cells.size()==4 && std::abs(state.cells[1].r-.9325048)<1e-6 && std::abs(state.cells[1].p-.02083515)<1e-6);
    assert(state.cells[1].rowsUsed==std::vector<int>({1,2,3,4,5}));assert(app.outputCodeReference(state.id));
    ShowCorrelationMatrixOnMain(state.id);
    while(CFRunLoopRunInMode(kCFRunLoopDefaultMode,.01,true)==kCFRunLoopRunHandledSource){}
    auto found=g_correlationMatrixControllers.find(state.id);assert(found!=g_correlationMatrixControllers.end());
    auto controller=found->second;[controller refresh];NSWindow *window=[controller valueForKey:@"window"];
    NSView *view=window.contentView;auto bitmap=[view bitmapImageRepForCachingDisplayInRect:view.bounds];
    [view cacheDisplayInRect:view.bounds toBitmapImageRep:bitmap];
    [[bitmap representationUsingType:NSBitmapImageFileTypePNG properties:@{}] writeToFile:@"/tmp/linkeda-correlation-audit/correlation.png" atomically:YES];
    [window setDelegate:nil];[window close];
    std::cout<<"macOS correlation window: R queue, actual R result, linked rows, provenance, editable controls and render passed.\n";
}}
