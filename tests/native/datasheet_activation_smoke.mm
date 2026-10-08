#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
static void drainActivationEvents() {
    dispatch_async(dispatch_get_main_queue(), ^{
        [NSApp stop:nil];
        [NSApp postEvent:[NSEvent otherEventWithType:NSEventTypeApplicationDefined
            location:NSZeroPoint modifierFlags:0 timestamp:0 windowNumber:0 context:nil
            subtype:0 data1:0 data2:0] atStart:NO];
    });
    [NSApp run];
}
int main() { @autoreleasepool {
    [NSApplication sharedApplication];
    [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
    auto& state=MacCommandDispatcher().applicationState();
    for (const std::string group : {"activation_original", "activation_imputed"}) {
        DataFrameModel data; data.group=group; data.rows=3;
        DataColumn x; x.name="x"; x.type="numeric"; x.values={"1","2","3"};
        data.columns={x}; state.registerDataset(data);
    }
    state.setSelectedRows("activation_original",{2});
    state.saveCurrentSelectionAsAnalysisScope("activation_original","One case",
        rlispstat::core::AnalysisScopeSourceKind::DataTableRows);
    ShowDataSheetOnMain("activation_original"); drainActivationEvents();
    assert(ActiveDatasetGroupUnlocked()=="activation_original");
    ShowDataSheetOnMain("activation_imputed"); drainActivationEvents();
    assert(ActiveDatasetGroupUnlocked()=="activation_imputed");
    auto* original=g_dataSheetControllers.at("activation_original");
    auto* imputed=g_dataSheetControllers.at("activation_imputed");
    NSWindow* originalWindow=[original valueForKey:@"window"];
    NSWindow* imputedWindow=[imputed valueForKey:@"window"];
    [originalWindow makeKeyAndOrderFront:nil]; drainActivationEvents();
    assert(ActiveDatasetGroupUnlocked()=="activation_original");
    // A programmatic dataset change must not open/raise another sheet.
    assert(HandleCommandLines({"SET_ACTIVE_DATASET","activation_imputed"})=="OK");
    drainActivationEvents();
    assert([NSApp keyWindow]==originalWindow);
    assert(ActiveDatasetGroupUnlocked()=="activation_imputed");
    // Clicking inside an already-key sheet also restores its active dataset.
    [originalWindow sendEvent:[NSEvent mouseEventWithType:NSEventTypeLeftMouseDown
        location:NSMakePoint(20,18) modifierFlags:0 timestamp:0
        windowNumber:[originalWindow windowNumber] context:nil eventNumber:1 clickCount:1 pressure:1]];
    drainActivationEvents();
    assert(ActiveDatasetGroupUnlocked()=="activation_original");
    auto* target=[[AssociatedDataSheetButtonTarget alloc] init];
    target.group=@"activation_imputed";
    [target showAssociatedDataSheet:nil]; drainActivationEvents();
    assert(ActiveDatasetGroupUnlocked()=="activation_imputed");
    assert([NSApp keyWindow]==imputedWindow);
    assert(g_dataSheetControllers.size()==2);
    // Activating sheets preserves each dataset's selection and analysis scope.
    assert(state.resolveActiveAnalysisRowIds("activation_original")==std::vector<int>{2});
    assert(state.resolveActiveAnalysisRowIds("activation_imputed").size()==3);
    std::set<int> rows; state.selectedRows("activation_original",rows);
    assert(rows==std::set<int>{2});
    [target release];
    [originalWindow close]; [imputedWindow close];
    fprintf(stderr,"DATA SHEET ACTIVATION VERIFIED\n");
}}
