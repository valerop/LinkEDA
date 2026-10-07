#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
static NSMenuItem* findItem(NSMenu* menu,NSString* title) {
    for(NSMenuItem* item in menu.itemArray) {
        if([item.title isEqualToString:title]) return item;
        if(auto child=findItem(item.submenu,title)) return child;
    }
    return nil;
}
static void drainDiagnosticEvents() {
    dispatch_after(dispatch_time(DISPATCH_TIME_NOW, 250 * NSEC_PER_MSEC), dispatch_get_main_queue(), ^{
        [NSApp stop:nil];
        [NSApp postEvent:[NSEvent otherEventWithType:NSEventTypeApplicationDefined
            location:NSZeroPoint modifierFlags:0 timestamp:0 windowNumber:0 context:nil
            subtype:0 data1:0 data2:0] atStart:NO];
    });
    [NSApp run];
}
int main(){@autoreleasepool{
    [NSApplication sharedApplication];
    [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
    using namespace rlispstat::core;
    int automaticallyOpenedSheets=0;
    PlotModel* plot=nullptr; CommandDispatcherServices services;
    services.ui.openDataSheet=[&](const std::string&){++automaticallyOpenedSheets;};
    services.ui.addPlot=[&](PlotModel* p){plot=p;}; CommandDispatcher dispatcher(services);
    assert(dispatcher.dispatch({"ADD_PLOT","series-diagnostic","series-diagnostic:unlinked","Value","Proportion",
        "Imputation diagnostics — Observed vs imputed","8",
        "4 0 1 0","4 0.5 2 0","10 0.5 3 0","10 1 4 0",
        "5 0 5 1","5 0.5 6 1","11 0.5 7 1","11 1 8 1",
        "TIME_SERIES","numeric","Series","2","Observed","Imputed 1",
        "TIME_SERIES_OPTIONS","legend","top_right","IMPUTATION_PROCESS_DIAGNOSTIC","distributions"})=="OK");
    assert(automaticallyOpenedSheets==0);
    assert(plot && PlotIsImputationDiagnostic(*plot) && !PlotLinksToDataRows(*plot));
    auto* view=[[ScatterView alloc] initWithModel:plot];
    auto* window=[[NSWindow alloc] initWithContentRect:NSMakeRect(50,100,720,520)
        styleMask:NSWindowStyleMaskTitled backing:NSBackingStoreBuffered defer:NO];
    [window setContentView:view];[window makeKeyAndOrderFront:nil];
    drainDiagnosticEvents();
    NSMenu* menu=[view timeSeriesContextMenu];
    assert(findItem(menu,@"Explain plot"));assert(findItem(menu,@"Legend position"));
    assert(!findItem(menu,@"Variables"));assert(!findItem(menu,@"Explore plot"));
    assert(!findItem(menu,@"Analysis scope"));assert(!findItem(menu,@"Plot"));
    assert(findItem(menu,@"Export"));
    DispatchCommand(plot,"TIME_SERIES_SET_LEGEND_POSITION|left");
    assert(plot->timeSeriesLegendPosition=="left");
    const NSRect r=[view plotRect];
    const auto items=BuildInteractionLegendLayout(*plot,{r.origin.x,r.origin.y,r.size.width,r.size.height});
    NSPoint down=[view convertPoint:NSMakePoint(items[0].sampleStart.x+8,items[0].sampleStart.y) toView:nil];
    NSPoint moved=NSMakePoint(down.x+130,down.y+60);
    auto event=[&](NSEventType type,NSPoint p){return [NSEvent mouseEventWithType:type location:p modifierFlags:0 timestamp:0 windowNumber:window.windowNumber context:nil eventNumber:1 clickCount:1 pressure:1];};
    [NSApp postEvent:event(NSEventTypeLeftMouseDragged,moved) atStart:NO];
    [NSApp postEvent:event(NSEventTypeLeftMouseUp,moved) atStart:NO];
    [view mouseDown:event(NSEventTypeLeftMouseDown,down)];
    assert(plot->interactionLegendUsesCustomPosition);
    assert(PlotSeriesLegendRows(*plot,0).empty());
    auto& state=MacCommandDispatcher().applicationState();
    const auto originalSelections=state.groupSelections();
    auto clickLegend=[&](std::size_t index, NSEventModifierFlags flags) {
        const auto legend=BuildInteractionLegendLayout(*plot,{r.origin.x,r.origin.y,r.size.width,r.size.height});
        const NSPoint at=[view convertPoint:NSMakePoint(legend[index].sampleStart.x+8,
            legend[index].sampleStart.y) toView:nil];
        [NSApp postEvent:event(NSEventTypeLeftMouseUp,at) atStart:NO];
        [view mouseDown:[NSEvent mouseEventWithType:NSEventTypeLeftMouseDown location:at
            modifierFlags:flags timestamp:0 windowNumber:window.windowNumber context:nil
            eventNumber:1 clickCount:1 pressure:1]];
    };
    clickLegend(0,0);
    assert(plot->selectedDiagnosticSeries==std::set<std::size_t>{0});
    assert(DiagnosticPlotSelectedPointIds(*plot)==(std::set<int>{1,2,3,4}));
    assert(PlotSeriesIsFullySelected(*plot,0,{}));
    assert(!PlotSeriesIsFullySelected(*plot,1,{}));
    clickLegend(1,NSEventModifierFlagShift);
    assert(plot->selectedDiagnosticSeries==(std::set<std::size_t>{0,1}));
    clickLegend(0,NSEventModifierFlagShift);
    assert(plot->selectedDiagnosticSeries==std::set<std::size_t>{1});
    // Hit the middle of a vertical segment away from the legend.
    plot->selectedDiagnosticSeries.clear();
    NSPoint lineAt=[view screenPointForPoint:DataPoint{11,0.75,0}];
    [view mouseDown:event(NSEventTypeLeftMouseDown,[view convertPoint:lineAt toView:nil])];
    assert(plot->selectedDiagnosticSeries==std::set<std::size_t>{1});
    assert([view savePNGToPath:@"/tmp/diagnostic-series-selected.png"]);
    NSPoint blank=[view screenPointForPoint:DataPoint{7,0.8,0}];
    [view mouseDown:event(NSEventTypeLeftMouseDown,[view convertPoint:blank toView:nil])];
    assert(plot->selectedDiagnosticSeries.empty());
    assert(state.groupSelections()==originalSelections);
    drainDiagnosticEvents();
    // The icon exists before provenance arrives. It must resolve the real dataset when clicked.
    state.plots()[plot->id]=plot;
    DataFrameModel source;source.group="diagnostic_source";source.rows=3;
    DataColumn sourceX;sourceX.name="hours";sourceX.type="numeric";sourceX.values={"4","5","11"};
    source.columns={sourceX};state.registerDataset(source);
    state.setSelectedRows(source.group,{2});
    SetWindowDataSheetGroupOnMac(window,plot->group);
    assert(state.dataSheetGroupForPlotGroup(plot->group)==plot->group);
    plot->codeReference.provenance.dataVersion.datasetId=source.group;
    assert(state.dataSheetGroupForPlotGroup(plot->group)==source.group);
    auto* target=(AssociatedDataSheetButtonTarget*)objc_getAssociatedObject(window,&kAssociatedDataSheetButtonTargetKey);
    [target showAssociatedDataSheet:nil];drainDiagnosticEvents();
    assert(ActiveDatasetGroupUnlocked()==source.group);
    assert(g_dataSheetControllers.size()==1);
    assert(g_dataSheetControllers.count(source.group));
    std::set<int> selectedSource;state.selectedRows(source.group,selectedSource);
    assert(selectedSource==std::set<int>{2});
    assert(!state.groupSelections().count(plot->group));
    NSWindow* sourceWindow=[g_dataSheetControllers.at(source.group) valueForKey:@"window"];
    assert(sourceWindow.visible);
    [sourceWindow close];state.plots().erase(plot->id);

    assert([view savePNGToPath:@"/tmp/series-legend-preview.png"]);
    auto svg=BuildSvgPlotSnapshotDocument(*plot,"light",plot->title);
    assert(svg.svg.find("Observed")!=std::string::npos);
    assert(svg.svg.find("Imputed 1")!=std::string::npos);
    std::ofstream("/tmp/series-legend-preview.svg")<<svg.svg;
    DataFrameModel data;data.group="real_series";data.rows=2;
    DataColumn t;t.name="time";t.type="numeric";t.values={"1","2"};data.columns={t};
    MacCommandDispatcher().applicationState().registerDataset(data);
    plot->group=data.group;plot->glmDiagnosticKind.clear();
    menu=[view timeSeriesContextMenu];
    assert(findItem(menu,@"Variables"));assert(findItem(menu,@"Explore plot"));
    assert(findItem(menu,@"Explain plot"));assert(!findItem(menu,@"Straight lines"));
    [window close];
    // Exercise the real macOS dispatcher and queued window creation as well.
    const auto sheetCount=g_dataSheetControllers.size();
    std::vector<std::string> page={"ADD_PLOT","native-mi-graph","native-mi-graph:unlinked",
        "Value","Proportion","Observed vs imputed — native window check","8",
        "4 0 1 0","4 0.5 2 0","10 0.5 3 0","10 1 4 0",
        "5 0 5 1","5 0.5 6 1","11 0.5 7 1","11 1 8 1",
        "TIME_SERIES","numeric","Series","2","Observed","Imputed 1",
        "TIME_SERIES_OPTIONS","legend","top_right","IMPUTATION_PROCESS_DIAGNOSTIC","distributions",
        "IMPUTATION_PLOT_VIEW","hours","20","1","5","512","1"};
    assert(HandleCommandLines(page)=="OK");
    drainDiagnosticEvents();
    ScatterView* actual=nil;
    for(ScatterView* candidate:g_views)
        if(candidate.model && candidate.model->id=="native-mi-graph") actual=candidate;
    assert(actual && actual.window.visible);
    assert(g_dataSheetControllers.size()==sheetCount);
    assert([actual savePNGToPath:@"/tmp/diagnostic-window-restored.png"]);
    actual.model->codeReference.provenance.dataVersion.datasetId="diagnostic_source";
    NSMenu* pages=[actual timeSeriesContextMenu];
    auto* secondPage=findItem(pages,@"Imputations 6–10");
    assert(secondPage);
    [NSApp sendAction:secondPage.action to:secondPage.target from:secondPage];
    assert(!g_pendingMIDiagnosticsTasks.empty());
    assert(g_pendingMIDiagnosticsTasks.back().imputationStart==6);
    assert(g_pendingMIDiagnosticsTasks.back().group=="diagnostic_source");
    const auto originalModel=actual.model;
    const auto viewsBefore=g_views.size();
    SetInteractionLegendCustomPosition(*actual.model,0.3,0.3);
    const auto metadata=std::find(page.begin(),page.end(),"IMPUTATION_PLOT_VIEW");
    *(metadata+3)="6";*(metadata+4)="10";
    assert(HandleCommandLines(page)=="OK");drainDiagnosticEvents();
    assert(actual.model==originalModel && g_views.size()==viewsBefore);
    assert(actual.model->imputationDiagnosticFirst==6);
    assert(actual.model->interactionLegendUsesCustomPosition);
    assert(ImputationDiagnosticDisplayStatus(*actual.model).find("6–10 of 20")!=std::string::npos);
    assert([actual savePNGToPath:@"/tmp/diagnostic-paged.png"]);
    [actual.window close];drainDiagnosticEvents();
    assert(!state.plots().count("native-mi-graph"));
    assert(HandleCommandLines(page)=="OK");drainDiagnosticEvents();
    bool reopened=false;
    for(ScatterView* candidate:g_views) if(candidate.model && candidate.model->id=="native-mi-graph") {
        reopened=candidate.window.visible;[candidate.window close];break;
    }
    assert(reopened);
    fprintf(stderr,"SERIES LEGEND AND MENUS VERIFIED\n");
}}
