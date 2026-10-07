#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
#include <iostream>
using namespace rlispstat::core;
static std::vector<std::string> Payload(const std::string &name){
    std::ifstream input("/tmp/linkeda-dimension-"+name+".payload");assert(input);
    std::vector<std::string> lines;std::string line;while(std::getline(input,line))lines.push_back(line);return lines;
}
static std::vector<std::string> CurrentReply(std::vector<std::string> payload) {
    const auto &state=g_dimensionalityModels.at(payload[1]);
    payload.insert(payload.begin()+2,{"REQUEST_V1",std::to_string(state.requestRevision),
        std::to_string(state.sourceDataVersion),"ok"});
    return payload;
}
static void Drain(){
    dispatch_async(dispatch_get_main_queue(),^{[NSApp stop:nil];
        [NSApp postEvent:[NSEvent otherEventWithType:NSEventTypeApplicationDefined location:NSZeroPoint
         modifierFlags:0 timestamp:0 windowNumber:0 context:nil subtype:0 data1:0 data2:0] atStart:NO];});
    [NSApp run];
}
int main(){@autoreleasepool{
    [NSApplication sharedApplication];
    assert(HandleCommandLines(Payload("dataset")).rfind("OK",0)==0);
    assert(HandleCommandLines(Payload("typed-dataset")).rfind("OK",0)==0);
    const auto typedPayload=Payload("typed-result");
    const std::string typedId=typedPayload[1], typedGroup=typedPayload[2];
    assert(HandleCommandLines({"PCAFA_OPEN",typedId,typedGroup,"factor","pairwise","TRUE","1","none","all",
      "4","numeric_item","ordinal_item","binary_item","second_numeric"}).rfind("OK",0)==0);
    Drain();
    assert(HandleCommandLines(CurrentReply(typedPayload)).rfind("OK",0)==0);Drain();
    auto typedController=g_dimensionalityControllers.at(typedId);
    auto &typedState=g_dimensionalityModels.at(typedId);
    assert(typedState.eligibleVariables.size()==4);
    assert(typedState.loadings.size()==4 && typedState.scores.size()==480);
    assert(!typedState.multipleImputation);
    auto typedView=(DimensionalityView *)[typedController valueForKey:@"reportView"];
    assert([typedView reportState].calculationMethod.find("mixed; pairwise")!=std::string::npos);
    assert([typedView reportState].calculationImputation.empty());
    auto typedWindow=(NSWindow *)[typedController valueForKey:@"window"];
    [HighResolutionPNGDataForView(typedWindow.contentView) writeToFile:@"/tmp/dimensionality-mixed.png" atomically:YES];
    assert(HandleCommandLines({"PCAFA_SET_VARIABLES",typedId,"3","ordinal_item","binary_item","numeric_item"}).rfind("OK",0)==0);
    [typedWindow orderOut:nil];
    // The fixture uses an immutable scope of rows 1:24. Configure the same
    // global scope before opening its analyses.
    std::vector<int> scopeRows;for(int row=1;row<=24;++row)scopeRows.push_back(row);
    assert(MacCommandDispatcher().applicationState().setActiveAnalysisScope(
        ExplicitAnalysisScope("dimension-selector",scopeRows,AnalysisScopeSourceKind::OtherExplicitSubset,"Test cases",32)));
    for(const std::string method:{"pca","factor"}){
        const std::string id="dimension-selector-"+method;
        assert(HandleCommandLines({"PCAFA_OPEN",id,"dimension-selector",method,"listwise","TRUE","1","none","selected","4","mpg","disp","hp","wt"}).rfind("OK",0)==0);
        Drain();auto controller=g_dimensionalityControllers.at(id);
        auto &state=g_dimensionalityModels.at(id);
        assert(state.multipleImputation && state.imputationCount==2);
        NSPopUpButton *popup=[controller valueForKey:@"imputationPopup"];
        assert(!popup.hidden && popup.numberOfItems==2);
        for(int imp:{2,1}){
            [popup selectItemAtIndex:imp-1];[controller imputationChanged:popup];Drain();
            assert(state.displayedImputation==imp && state.requestedImputation==imp);
            assert(!g_pendingDimensionalityTasks.empty());
            assert(g_pendingDimensionalityTasks.back().displayedImputation==imp);
            const auto reply=HandleCommandLines(CurrentReply(Payload(method+"-"+std::to_string(imp))));
            if(reply.rfind("OK",0)!=0)std::cerr<<reply<<" method="<<method<<" imputation="<<imp<<"\n";
            assert(reply.rfind("OK",0)==0);Drain();
            assert(!state.rFitPending && !state.components.empty());
            const auto eigenvalue=state.components.front().eigenvalue;
            const auto version=state.modelVersion;
            // A late response for the other imputation must leave the current table intact.
            assert(HandleCommandLines(Payload(method+"-"+std::to_string(3-imp))).rfind("OK",0)==0);Drain();
            assert(state.displayedImputation==imp && state.modelVersion==version);
            assert(state.components.front().eigenvalue==eigenvalue);
            assert(popup.indexOfSelectedItem==imp-1);
        }
        assert(HandleCommandLines({"PCAFA_SET_IMPUTATION",id,"3"}).rfind("ERR",0)==0);
        const auto *dataset=MacCommandDispatcher().applicationState().datasets().find("dimension-selector");
        assert(dataset && dataset->activeImputationVersion==1);
        auto view=(DimensionalityView *)[controller valueForKey:@"reportView"];
        assert([view reportState].calculationImputation.find("imputation 1 of 2")!=std::string::npos);
        if(method=="pca"){
            auto window=(NSWindow *)[controller valueForKey:@"window"];
            [HighResolutionPNGDataForView(window.contentView) writeToFile:@"/tmp/linkeda-dimension-selector.png" atomically:YES];
        }
        [(NSWindow *)[controller valueForKey:@"window"] orderOut:nil];
    }
}}
