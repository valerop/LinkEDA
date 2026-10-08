#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
#include <fstream>
#include <iostream>
int main(int argc,char **argv) { @autoreleasepool {
    assert(argc==2);
    [NSApplication sharedApplication];
    DataFrameModel data;data.group="cluster-smoke";data.rows=4;
    rlispstat::core::DataColumn a,b;a.name="a";b.name="b";a.type=b.type="numeric";
    a.values={"1","4","7","9"};b.values={"7","2","4","1"};data.columns={a,b};
    auto &app=MacCommandDispatcher().applicationState(); assert(app.registerDataset(data));
    DendrogramState source; source.id="cluster-smoke-tree";source.group=data.group;source.variables={"a","b"};
    rlispstat::core::PopulateDatasetSeedPlot(source.seed,*app.datasets().find(data.group),data.group);
    source.hasSeed=true; g_dendrograms[source.id]=source;
    auto &state=g_dendrograms[source.id];
    // Native refit queues R work, with no local tree or substitute distances.
    RefitDendrogramState(state);
    assert(state.rFitPending && state.merges.empty() && g_pendingDendrogramTasks.size()==1);
    assert(rlispstat::core::EncodeMainRTask(g_pendingDendrogramTasks.back()).find(
        "DENDROGRAM_NEEDED\tcluster-smoke-tree\tcluster-smoke\t1\t1\t1\tall\teuclidean\taverage\tpairwise\t2\ta\tb\t0")==0);
    std::ifstream input(argv[1]);std::vector<std::string> lines;std::string line;
    while(std::getline(input,line)) lines.push_back(line);
    auto reply=MacCommandDispatcher().dispatch(lines);
    if(reply.rfind("OK",0)!=0)std::cerr<<reply<<"\n";
    assert(reply.rfind("OK",0)==0 && !state.rFitPending);
    assert(state.caseRows==std::vector<int>({1,2,3,4}) && state.merges.size()==3);
    assert(!state.provenance.executedRCode.empty());
    auto geometry=BuildDendrogramPlotGeometry(state.caseRows,state.merges,state.leafOrder,760,450);
    assert(geometry.hasCases && geometry.leaves.size()==4 && geometry.branches.size()==9);
    assert(app.outputCodeReference(state.id));
    ShowDendrogramOnMain(state.id);
    while(CFRunLoopRunInMode(kCFRunLoopDefaultMode,0.01,true)==kCFRunLoopRunHandledSource){}
    auto found=g_dendrogramControllers.find(state.id);assert(found!=g_dendrogramControllers.end());
    auto controller=found->second;[controller refresh];
    NSWindow *window=[controller valueForKey:@"window"];
    NSView *view=window.contentView;auto bitmap=[view bitmapImageRepForCachingDisplayInRect:view.bounds];
    [view cacheDisplayInRect:view.bounds toBitmapImageRep:bitmap];
    [[bitmap representationUsingType:NSBitmapImageFileTypePNG properties:@{}] writeToFile:@"/tmp/linkeda-correlation-audit/dendrogram.png" atomically:YES];
    DendrogramView *treeView=[controller valueForKey:@"dendrogramView"];
    auto svg=[treeView dendrogramSvgDocument];assert(svg.find("Variables: a, b")!=std::string::npos);
    NSData *pdf=[window.contentView dataWithPDFInsideRect:window.contentView.bounds];assert(pdf.length>1000);
    assert([pdf writeToFile:@"/tmp/linkeda-correlation-audit/dendrogram.pdf" atomically:YES]);
    [window setDelegate:nil];[window close];
    std::cout<<"Native dendrogram: queued R request, accepted public-R tree, linked row identities and rendering passed.\n";
}}
