#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
static void drainScopeEvents(){dispatch_async(dispatch_get_main_queue(), ^{[NSApp stop:nil];[NSApp postEvent:[NSEvent otherEventWithType:NSEventTypeApplicationDefined location:NSZeroPoint modifierFlags:0 timestamp:0 windowNumber:0 context:nil subtype:0 data1:0 data2:0] atStart:NO];});[NSApp run];}
int main(){ @autoreleasepool {
    [NSApplication sharedApplication];
    using namespace rlispstat::core;
    auto& app=MacCommandDispatcher().applicationState();
    DataFrameModel data;data.group="global-scope-smoke";data.rows=100;
    DataColumn x;x.name="x";x.type="numeric";DataColumn y=x;y.name="y";
    for(int i=1;i<=100;++i){x.values.push_back(std::to_string(i));y.values.push_back(std::to_string(i%7));}
    data.columns={x,y};app.registerDataset(data);
    std::set<int> rows;for(int i=1;i<=20;++i)rows.insert(i);app.setSelectedRows(data.group,rows);
    auto* view=[[GlobalAnalysisScopeSummary alloc] initWithFrame:NSMakeRect(20,20,220,28)];
    PopulateAnalysisScopePopup(view,data.group,"selected",AnalysisScope{},false,true);
    assert(![view isEnabled] && [view numberOfItems]==1);
    assert(std::string([[[view itemAtIndex:0] title] UTF8String]).find("100")!=std::string::npos);
    app.setActiveAnalysisScopeFromSelection(data.group,AnalysisScopeSourceKind::CurrentSelection,"Selected observations");
    [view refreshGlobalScopeText];
    assert(std::string([[[view itemAtIndex:0] title] UTF8String]).find("20")!=std::string::npos);
    // A plot created after changing the global scope must capture and apply
    // that scope.  In particular, cloning the dataset seed must not retain an
    // older all-observations result merely because another plot is active.
    PlotModel seed;
    PopulateDatasetSeedPlot(seed,data,data.group);
    PlotModel scopedScatter(seed);
    scopedScatter.id="scope-scatter";
    scopedScatter.kind="scatter";
    scopedScatter.isDatasetSeed=false;
    scopedScatter.xLabel="x";
    scopedScatter.yLabel="y";
    RebuildPointsForCurrentVariables(&scopedScatter);
    assert(scopedScatter.points.size()==100);
    assert(AttachPlotModelUnlocked(&scopedScatter));
    assert(scopedScatter.dataScopeCaptured);
    assert(scopedScatter.dataScope.originalRowIds.size()==20);
    assert(scopedScatter.points.size()==20);
    assert(PlotWindowTitleWithAnalysisScope(scopedScatter).find("N = 20 of 100 observations")!=std::string::npos);
    // The case actions belong to exploration, as on Windows, not to Display.
    auto* scatterView = [[ScatterView alloc] initWithModel:&scopedScatter];
    NSEvent* menuEvent = [NSEvent mouseEventWithType:NSEventTypeRightMouseDown
        location:NSZeroPoint modifierFlags:0 timestamp:0 windowNumber:0
        context:nil eventNumber:0 clickCount:1 pressure:1];
    NSMenu* scatterMenu = [scatterView menuForEvent:menuEvent];
    NSMenu* exploreMenu = [[scatterMenu itemWithTitle:@"Explore plot"] submenu];
    NSMenu* scopeMenu = [[[[scatterMenu itemWithTitle:@"Analyze"] submenu]
        itemWithTitle:@"Global Analysis Scope…"] submenu];
    assert(scopeMenu);
    NSMenuItem* excludeCases = [scopeMenu itemWithTitle:@"Exclude selected cases from all analyses"];
    assert(excludeCases && [excludeCases isEnabled]);
    assert([scopeMenu itemWithTitle:@"Include selected cases again"]);
    assert([scopeMenu itemWithTitle:@"Include all cases again"]);
    assert([[exploreMenu itemWithTitle:@"Exclude selected cases from all analyses"] isEnabled]);
    assert([exploreMenu itemWithTitle:@"Include selected cases again"]);
    assert([exploreMenu itemWithTitle:@"Include all cases again"]);
    assert(![[[scatterMenu itemWithTitle:@"Display"] submenu]
        itemWithTitle:@"Global Analysis Scope…"]);
    [scatterView release];
    // An ANOVA pairwise result follows the accepted ANOVA rows, including
    // exclusions, and rejects a calculation started before the new fit.
    seed.id="anova-scope-seed";
    g_plots[seed.id]=&seed;
    MeanComparisonState anova;anova.id="anova-scope";anova.datasetId=data.group;
    anova.analysisType="one_way_anova";anova.groupVariable="x";
    anova.method="Welch one-way ANOVA";
    MeanComparisonTable omnibus;omnibus.tableId="omnibus";
    MeanComparisonRow anovaRow;anovaRow.variable="y";anovaRow.originalRowIndices={1,2};
    omnibus.rows.push_back(anovaRow);anova.tables.push_back(omnibus);
    g_meanComparisonAcceptedStates[anova.id]=anova;
    g_meanComparisonResultVersions[anova.id]=1;
    GLMPairwiseComparisonPlotLink pairwise;
    pairwise.sourceModelKind="mean_comparison";
    pairwise.sourceModelId=anova.id+"\x1f"+anovaRow.variable;
    pairwise.group=data.group;pairwise.factor="x";pairwise.response="y";
    PairwiseDerivedRefreshSnapshot oldPairwise;std::string pairwiseError;
    assert(BuildPairwiseDerivedRefreshSnapshot(pairwise,oldPairwise,pairwiseError));
    assert((oldPairwise.linearFit.rowsUsed==std::vector<int>{1,2}));
    assert(PairwiseDerivedSnapshotIsCurrent(oldPairwise));
    g_meanComparisonAcceptedStates[anova.id].tables[0].rows[0].originalRowIndices={30,31};
    ++g_meanComparisonResultVersions[anova.id];
    PairwiseDerivedRefreshSnapshot newPairwise;
    assert(BuildPairwiseDerivedRefreshSnapshot(pairwise,newPairwise,pairwiseError));
    assert((newPairwise.linearFit.rowsUsed==std::vector<int>{30,31}));
    assert(!PairwiseDerivedSnapshotIsCurrent(oldPairwise));
    g_meanComparisonAcceptedStates.erase(anova.id);
    g_meanComparisonResultVersions.erase(anova.id);
    g_plots.erase(seed.id);
    GeneralizedGLMState model;model.id="scope-model";model.group=data.group;model.response="y";model.terms={"x"};
    model.family="gaussian";model.link="identity";model.scope="all";
    RequestGeneralizedGLMFitInMainR(model);
    assert(model.dataScopeCaptured && model.dataScope.originalRowIds.size()==20);
    assert(!g_pendingGeneralizedGLMTasks.empty());
    const auto queued=g_pendingGeneralizedGLMTasks.back();
    assert(queued.scope=="selected" && queued.rows.size()==20);
    // The selection coordinator runs before the window scope notification.
    // It must leave a manually frozen linear fit and its computed rows intact.
    GroupModelState frozen;frozen.modelId="frozen-scope-model";frozen.group=data.group;
    frozen.response="y";frozen.terms={"x"};frozen.autoRefit=false;
    app.captureAnalysisScope(data.group,frozen.dataScope,frozen.dataScopeCaptured,&frozen.scope);
    frozen.isStale=false;frozen.fitVersion=1;frozen.lastRFitSignature="fitted-on-20";
    g_groupModels[frozen.modelId]=frozen;
    rows.clear();for(int i=31;i<=100;++i)rows.insert(i);app.setSelectedRows(data.group,rows);
    MarkSelectionScopedModelsStaleUnlocked(data.group);
    const auto& stillFrozen=g_groupModels.at(frozen.modelId);
    assert(stillFrozen.scope=="selected" && stillFrozen.dataScope.originalRowIds.size()==20);
    assert(!stillFrozen.isStale && stillFrozen.fitVersion==1 &&
           stillFrozen.lastRFitSignature=="fitted-on-20");
    assert(stillFrozen.frozenScopeNotice.find("current global scope")!=std::string::npos);
    g_groupModels.erase(frozen.modelId);
    // Existing live plots follow the same dynamic global scope. Freezing one
    // is the only way it may keep an older row set.
    RefreshOpenPlotsForGlobalScopeOnMain(data.group,true);
    assert(scopedScatter.points.size()==70);
    assert(scopedScatter.dataScope.originalRowIds.size()==70);
    scopedScatter.analysisScopeFrozen=true;
    app.setSelectedRows(data.group,{1,2,3});
    RefreshOpenPlotsForGlobalScopeOnMain(data.group,true);
    assert(scopedScatter.points.size()==70);
    assert(scopedScatter.frozenScopeNotice.find("current global scope")!=std::string::npos);
    scopedScatter.analysisScopeFrozen=false;
    RefreshOpenPlotsForGlobalScopeOnMain(data.group,true);
    assert(scopedScatter.points.size()==3);
    rows.clear();for(int i=31;i<=100;++i)rows.insert(i);
    app.setSelectedRows(data.group,rows);
    RefreshOpenPlotsForGlobalScopeOnMain(data.group,true);
    assert(scopedScatter.points.size()==70);
    [view refreshGlobalScopeText];
    assert(std::string([[[view itemAtIndex:0] title] UTF8String]).find("70")!=std::string::npos);
    assert(queued.rows.size()==20 && model.dataScope.originalRowIds.size()==20);
    // A visible model with a captured scope must request the new live rows;
    // the already queued fit remains an immutable 20-row request.
    g_generalizedGLMs[model.id] = model;
    g_generalizedGLMControllers[model.id] = nil;
    RefitOpenAnalysesForScopeOnMain(data.group, true);
    assert(g_generalizedGLMs[model.id].dataScope.originalRowIds.size()==70);
    assert(!g_pendingGeneralizedGLMTasks.empty() &&
           g_pendingGeneralizedGLMTasks.back().rows.size()==70);
    g_generalizedGLMControllers.erase(model.id);
    g_generalizedGLMs.erase(model.id);
    app.clearSelectedRows(data.group);RequestGeneralizedGLMFitInMainR(model);
    assert(!model.ok && model.status.find("no rows")!=std::string::npos);
    assert(model.dataScope.originalRowIds.empty());
    app.resetActiveAnalysisScopeToAllObservations(data.group);
    RequestGeneralizedGLMFitInMainR(model);
    assert(model.scope=="all" && AnalysisScopeRowCount(model.dataScope,100)==100);
    NSWindow* window=[[NSWindow alloc] initWithContentRect:NSMakeRect(0,0,400,100) styleMask:NSWindowStyleMaskTitled backing:NSBackingStoreBuffered defer:NO];
    [[window contentView] addSubview:view];
    PopulateAnalysisScopePopup(view,data.group,"all",model.dataScope,true,true);
    app.setSelectedRows(data.group,{1,3,5});
    assert(app.saveCurrentSelectionAsAnalysisScope(data.group,"Italia",AnalysisScopeSourceKind::DataTableRows));
    app.resetActiveAnalysisScopeToAllObservations(data.group);
    assert(HandleCommandLines({"USE_SAVED_ANALYSIS_SCOPE",data.group,"Italia"}).rfind("OK ",0)==0);
    drainScopeEvents();
    assert(std::string([[[view itemAtIndex:0] title] UTF8String])=="Italia · N = 3");
    assert(scopedScatter.points.size()==3);
    assert((scopedScatter.dataScope.originalRowIds==std::vector<int>{1,3,5}));
    const std::string tooltip=[[view toolTip] UTF8String];
    assert(tooltip.find("Global analysis scope: Selection: Italia")!=std::string::npos);
    assert(tooltip.find("Scope when computed: All observations")!=std::string::npos);
    assert(AnalysisScopeRowCount(model.dataScope,100)==100);
    // Native-menu route uses the spread event directly, and must refresh too.
    app.setSelectedRows(data.group,{2,4});
    app.saveCurrentSelectionAsAnalysisScope(data.group,"Control",AnalysisScopeSourceKind::DataTableRows);
    PublishSpreadPlotMessage("ANALYSIS_SCOPE_CHANGED",data.group);
    drainScopeEvents();
    assert(std::string([[[view itemAtIndex:0] title] UTF8String])=="Control · N = 2");
    assert(scopedScatter.points.size()==2);
    assert((scopedScatter.dataScope.originalRowIds==std::vector<int>{2,4}));
    app.resetActiveAnalysisScopeToAllObservations(data.group);
    PublishSpreadPlotMessage("ANALYSIS_SCOPE_CHANGED",data.group);drainScopeEvents();
    assert(std::string([[[view itemAtIndex:0] title] UTF8String])=="All · N = 100");
    assert(scopedScatter.points.size()==100);
    [view removeFromSuperview];[view release];

}}
