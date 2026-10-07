#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
#include <fstream>
int main(){@autoreleasepool{
    [NSApplication sharedApplication];
    using namespace rlispstat::core;
    const std::string body = "Interaction term: country:group\nType: factor_factor\nConfidence level: 95%\nMultiple comparisons: emmeans with tukey adjustment\nEMM reference test: None\n\nEstimated marginal means\ncountry\tgroup\tEstimate\tSE\tLower 95%\tUpper 95%\nItaly\tControl\t10.2\t0.5\t9.2\t11.2\nItaly\tTreatment\t12.3\t0.6\t11.1\t13.5\n";
    GLMInteractionReport report;
    assert(ParseGLMInteractionReportText(SplitLines(body),report));
    assert((GLMInteractionGroupingFactors(report)==std::vector<std::string>{"country","group"}));
    OutputCodeReference source;
    source.outputBlockId="model";source.analysisId="source-fixture";
    source.provenance.verificationRCode["model"]="reference_fits <- readRDS(Sys.getenv('LINKEDA_GROUPING_FITS'))";
    for(const std::string grouping:{"group","country"}) {
        report.sections[0].identityHeaders=grouping=="group"
            ?std::vector<std::string>{"country","group"}:std::vector<std::string>{"group","country"};
        const auto output=RegressionInteractionReportCodeReference("fixture",report,source);
        assert(output.analysisId==source.analysisId);
        std::ofstream("/tmp/linkeda-grouping-"+grouping+".R")<<output.provenance.verificationRCode.at(output.outputBlockId);
    }
    std::ofstream("/tmp/linkeda-grouping-native-dispatch.R")<<NativePooledAnalysisRScript();
    RegressionInteractionDerivedLink link;
    link.sourceModelId="grouping-fixture";link.sourceModelKind="linear_single";
    link.group="fixture";link.term="country:group";
    ShowGLMInteractionReportWindowOnMain("Interaction grouping",body,link);
    for(int i=0;i<20 && g_interactionReportWindows.empty();++i)
        [[NSRunLoop currentRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:.02]];
    assert(g_interactionReportWindows.size()==1);
    auto entry=g_interactionReportWindows.front();
    assert(entry.groupingSelector && entry.groupingSelector.numberOfItems==2);
    assert([entry.groupingSelector.titleOfSelectedItem isEqualToString:@"group"]);
    auto *reportView = (GLMInteractionReportView *)entry.scroll.documentView;
    NSMenu *context = [reportView menuForEvent:nil];
    NSMenu *explain = [[context itemWithTitle:@"Explain statistic"] submenu];
    assert(explain);
    NSMenu *meanStatistics = [[explain itemWithTitle:@"Estimated marginal means"] submenu];
    assert([meanStatistics itemWithTitle:@"Estimate"]);
    assert([meanStatistics itemWithTitle:@"SE"]);
    bool estimateCellHasHelp = false;
    for (NSView *subview in reportView.subviews) {
        NSTextField *cell = [subview isKindOfClass:[NSTextField class]]
            ? (NSTextField *)subview : nil;
        if (cell && [cell.stringValue isEqualToString:@"10.2"] &&
            [[cell menu] itemWithTitle:@"Estimate"])
            estimateCellHasHelp = true;
    }
    assert(estimateCellHasHelp);
    NSMenu *exportMenu = [[context itemWithTitle:@"Export"] submenu];
    assert(exportMenu.numberOfItems == 3);
    assert([[exportMenu itemAtIndex:0].title isEqualToString:@"Copy"]);
    assert([[exportMenu itemAtIndex:1].title isEqualToString:@"Save"]);
    assert([[exportMenu itemAtIndex:2].title isEqualToString:@"R Code"]);
    assert([[[exportMenu itemWithTitle:@"Copy"] submenu] itemWithTitle:@"PDF image (vector)"]);
    assert([[[exportMenu itemWithTitle:@"Save"] submenu] itemWithTitle:@"PDF (as shown)…"]);
    ReplaceGLMInteractionReportBody(entry.scroll, body +
        "\nANALYSIS_PROVENANCE_V2\ninternal-key\n0123456789abcdef\n");
    auto *visibleReport = (GLMInteractionReportView *)entry.scroll.documentView;
    NSString *visibleText = [visibleReport snapshotTabDelimitedText];
    assert([visibleText containsString:@"Italy\tControl"]);
    assert([visibleText rangeOfString:@"ANALYSIS_PROVENANCE_V2"].location == NSNotFound);
    assert([visibleText rangeOfString:@"0123456789abcdef"].location == NSNotFound);
    NSView *content=entry.window.contentView;
    [content layoutSubtreeIfNeeded];[content displayIfNeeded];
    NSBitmapImageRep *bitmap=[content bitmapImageRepForCachingDisplayInRect:content.bounds];
    [content cacheDisplayInRect:content.bounds toBitmapImageRep:bitmap];
    [[bitmap representationUsingType:NSBitmapImageFileTypePNG properties:@{}]
        writeToFile:@"/tmp/linkeda-interaction-grouping.png" atomically:YES];
    for(NSInteger selected:{0,1,0}) {
        [entry.groupingSelector selectItemAtIndex:selected];
        [NSApp sendAction:entry.groupingSelector.action to:entry.groupingSelector.target from:entry.groupingSelector];
        assert(g_interactionReportWindows.front().link.reportFocal==(selected==0?"group":"country"));
    }
    [entry.window close];
    const std::string threeWayBody =
        "Interaction term: country:group:period\nType: factor_factor_factor\n"
        "Confidence level: 95%\nMultiple comparisons: emmeans with tukey adjustment\n"
        "EMM reference test: None\n\nEstimated marginal means\n"
        "country\tgroup\tperiod\tEstimate\tSE\tLower 95%\tUpper 95%\n"
        "Italy\tControl\tBefore\t10.2\t0.5\t9.2\t11.2\n";
    GLMInteractionReport threeWay;
    assert(ParseGLMInteractionReportText(SplitLines(threeWayBody),threeWay));
    assert((GLMInteractionGroupingFactors(threeWay)==
        std::vector<std::string>{"country","group","period"}));
    RegressionInteractionDerivedLink threeWayLink=link;
    threeWayLink.term="country:group:period";
    ShowGLMInteractionReportWindowOnMain("Three-factor interaction",threeWayBody,threeWayLink);
    for(int i=0;i<20 && g_interactionReportWindows.size()!=1;++i)
        [[NSRunLoop currentRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:.02]];
    assert(g_interactionReportWindows.size()==1);
    auto triple=g_interactionReportWindows.front();
    assert(triple.comparisonSelector && triple.comparisonSelector.numberOfItems==3);
    assert(triple.groupingSelector && triple.groupingSelector.numberOfItems==3);
    assert([triple.stratumLabel.stringValue isEqualToString:@"Within: period"]);
    [triple.groupingSelector selectItemWithTitle:@"period"];
    [NSApp sendAction:triple.groupingSelector.action to:triple.groupingSelector.target
                from:triple.groupingSelector];
    assert(g_interactionReportWindows.front().link.reportFocal=="country");
    assert(g_interactionReportWindows.front().link.reportGroup=="period");
    assert([triple.stratumLabel.stringValue isEqualToString:@"Within: group"]);
    [triple.comparisonSelector selectItemWithTitle:@"period"];
    [NSApp sendAction:triple.comparisonSelector.action to:triple.comparisonSelector.target
                from:triple.comparisonSelector];
    assert(g_interactionReportWindows.front().link.reportFocal=="period");
    assert(g_interactionReportWindows.front().link.reportGroup=="country");
    assert([triple.stratumLabel.stringValue isEqualToString:@"Within: group"]);
    [triple.window close];

    DataFrameModel alien;alien.group="mixed-interaction-fixture";alien.rows=3;
    DataColumn happy;happy.name="happy";happy.type="factor";
    happy.values={"no","yes","no"};
    DataColumn planet;planet.name="planet";planet.type="factor";
    planet.values={"Aurelia","Borealis","Cygnus"};
    DataColumn eggs;eggs.name="eggs";eggs.type="numeric";
    eggs.values={"0","1","2"};
    alien.columns={happy,planet,eggs};
    assert(MacCommandDispatcher().applicationState().registerDataset(alien));
    const std::string mixedBody =
        "Interaction term: happy:planet:eggs\nType: three_way_with_numeric\n"
        "Confidence level: 95%\nMultiple comparisons: emmeans with tukey adjustment\n"
        "\nSimple slopes\nhappy\tplanet\tEstimate\tSE\tStatistic\tp\tLower 95%\tUpper 95%\n"
        "no\tAurelia\t0.70\t0.10\t7.0\t.001\t0.50\t0.90\n";
    RegressionInteractionDerivedLink mixedLink=link;
    mixedLink.group=alien.group;
    mixedLink.term="happy:planet:eggs";
    ShowGLMInteractionReportWindowOnMain("Mixed interaction",mixedBody,mixedLink);
    for(int i=0;i<20 && g_interactionReportWindows.size()!=1;++i)
        [[NSRunLoop currentRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:.02]];
    assert(g_interactionReportWindows.size()==1);
    auto mixed=g_interactionReportWindows.front();
    assert(mixed.comparisonSelector && mixed.comparisonSelector.numberOfItems==3);
    assert([mixed.comparisonSelector.titleOfSelectedItem isEqualToString:@"eggs"]);
    assert(mixed.groupingSelector && mixed.groupingSelector.numberOfItems==2);
    assert([mixed.groupingSelector.titleOfSelectedItem isEqualToString:@"happy"]);
    auto *mixedReport=(GLMInteractionReportView *)mixed.scroll.documentView;
    bool labelHasContext=false;
    for(NSView *view in mixedReport.subviews) {
        auto *label=[view isKindOfClass:[InteractionReportLineField class]]
            ? (InteractionReportLineField *)view : nil;
        if(label && [label.stringValue isEqualToString:@"Aurelia"] &&
           [[label menuForEvent:nil] itemWithTitle:@"Export"])
            labelHasContext=true;
    }
    assert(labelHasContext);
    [mixed.window close];

    // Model-local factor interpretations must take precedence over the raw
    // numeric storage of mtcars-style columns when building these controls.
    DataFrameModel cars;cars.group="numeric-storage-interaction-fixture";cars.rows=3;
    DataColumn am;am.name="am";am.type="numeric";am.values={"0","1","0"};
    DataColumn cyl;cyl.name="cyl";cyl.type="numeric";cyl.values={"4","6","8"};
    DataColumn wt;wt.name="wt";wt.type="numeric";wt.values={"2.1","3.2","4.3"};
    cars.columns={am,cyl,wt};
    assert(MacCommandDispatcher().applicationState().registerDataset(cars));
    GLMInteractionReport numericStorage;
    const std::string numericStorageBody =
        "Interaction term: am:cyl:wt\nType: three_way_with_numeric\n"
        "Confidence level: 95%\nMultiple comparisons: emmeans with tukey adjustment\n"
        "\nSimple slopes\nam\tcyl\tEstimate\tSE\tStatistic\tp\tLower 95%\tUpper 95%\n"
        "0\t4\t3.0\t1.0\t3.0\t.01\t1.0\t5.0\n";
    assert(ParseGLMInteractionReportText(SplitLines(numericStorageBody),numericStorage));
    assert((GLMInteractionGroupingFactors(numericStorage,
        {{"am","factor"},{"cyl","factor"},{"wt","numeric"}})==
        std::vector<std::string>{"am","cyl"}));
    assert(GLMInteractionGroupingFactors(numericStorage).empty());
    PlotModel numericSeed;numericSeed.id="numeric-storage-seed";
    numericSeed.group=cars.group;g_plots[numericSeed.id]=&numericSeed;
    const std::string numericModelId="numeric-storage-model";
    auto &numericModel=g_groupModels[numericModelId];
    numericModel.modelId=numericModelId;numericModel.group=cars.group;
    numericModel.response="wt";numericModel.terms={"am","cyl","wt","am:cyl:wt"};
    numericModel.termTypes={{"am","factor"},{"cyl","factor"},{"wt","numeric"}};
    numericModel.isStale=false;
    GLMFitSummary numericFit;numericFit.ok=true;
    g_precomputedLinearModelFits[numericModelId]=numericFit;
    RegressionInteractionDerivedLink numericLink=link;
    numericLink.sourceModelId=numericModelId;numericLink.group=cars.group;
    numericLink.term="am:cyl:wt";
    ShowGLMInteractionReportWindowOnMain(
        "Numeric-storage interaction",numericStorageBody,numericLink);
    for(int i=0;i<20 && g_interactionReportWindows.size()!=1;++i)
        [[NSRunLoop currentRunLoop] runUntilDate:
            [NSDate dateWithTimeIntervalSinceNow:.02]];
    assert(g_interactionReportWindows.size()==1);
    auto numericEntry=g_interactionReportWindows.front();
    assert(numericEntry.comparisonSelector &&
        numericEntry.comparisonSelector.numberOfItems==3);
    assert(numericEntry.groupingSelector &&
        numericEntry.groupingSelector.numberOfItems==2);
    assert([numericEntry.comparisonSelector.titleOfSelectedItem
        isEqualToString:@"wt"]);
    [numericEntry.window close];

    auto checkModelRoute = [&](const std::string &kind,
                               const std::string &modelId) {
        RegressionInteractionDerivedLink routeLink=numericLink;
        routeLink.sourceModelKind=kind;routeLink.sourceModelId=modelId;
        InteractionDerivedRefreshSnapshot snapshot;
        std::string message;
        assert(BuildInteractionDerivedRefreshSnapshot(
            routeLink,snapshot,message));
        assert(snapshot.termTypes.at("am")=="factor");
        assert(snapshot.termTypes.at("cyl")=="factor");
        ShowGLMInteractionReportWindowOnMain(
            "Model-local interaction",numericStorageBody,routeLink);
        for(int i=0;i<20 && g_interactionReportWindows.size()!=1;++i)
            [[NSRunLoop currentRunLoop] runUntilDate:
                [NSDate dateWithTimeIntervalSinceNow:.02]];
        assert(g_interactionReportWindows.size()==1);
        auto entry=g_interactionReportWindows.front();
        assert(entry.comparisonSelector &&
            entry.comparisonSelector.numberOfItems==3);
        assert(entry.groupingSelector && entry.groupingSelector.numberOfItems==2);
        [entry.window close];
    };

    GeneralizedGLMState generalized;
    generalized.id="generalized-interaction-model";
    generalized.group=cars.group;generalized.response="wt";
    generalized.terms=numericModel.terms;
    generalized.termTypes=numericModel.termTypes;
    generalized.ok=true;
    g_generalizedGLMs[generalized.id]=generalized;
    checkModelRoute("generalized_single",generalized.id);

    RegressionComparisonState linearComparison;
    linearComparison.id="linear-interaction-comparison";
    linearComparison.group=cars.group;linearComparison.response="wt";
    linearComparison.termTypes=numericModel.termTypes;
    RegressionComparisonModel linearColumn;
    linearColumn.id="linear-interaction-column";
    linearColumn.terms=numericModel.terms;
    linearColumn.termTypes=numericModel.termTypes;
    linearColumn.fit.ok=true;linearColumn.isStale=false;
    linearComparison.models.push_back(linearColumn);
    g_regressionComparisons[linearComparison.id]=linearComparison;
    checkModelRoute("linear_comparison",linearColumn.id);

    GeneralizedComparisonState generalizedComparison;
    generalizedComparison.id="generalized-interaction-comparison";
    generalizedComparison.group=cars.group;
    generalizedComparison.response="wt";
    generalizedComparison.termTypes=numericModel.termTypes;
    GeneralizedComparisonModel generalizedColumn;
    generalizedColumn.id="generalized-interaction-column";
    generalizedColumn.terms=numericModel.terms;
    generalizedColumn.termTypes=numericModel.termTypes;
    generalizedColumn.fit.ok=true;generalizedColumn.isStale=false;
    generalizedComparison.models.push_back(generalizedColumn);
    g_generalizedComparisons[generalizedComparison.id]=generalizedComparison;
    checkModelRoute("generalized_comparison",generalizedColumn.id);

    // MI models use the same stored specification while the result is pooled.
    g_groupModels[numericModelId].multipleImputation=true;
    g_generalizedGLMs[generalized.id].multipleImputation=true;
    g_regressionComparisons[linearComparison.id].multipleImputation=true;
    g_generalizedComparisons[generalizedComparison.id].multipleImputation=true;
    checkModelRoute("linear_single",numericModelId);
    checkModelRoute("generalized_single",generalized.id);
    checkModelRoute("linear_comparison",linearColumn.id);
    checkModelRoute("generalized_comparison",generalizedColumn.id);

    g_generalizedGLMs[generalized.id].binaryRegression=true;
    g_generalizedComparisons[generalizedComparison.id].binaryComparison=true;
    checkModelRoute("generalized_single",generalized.id);
    checkModelRoute("generalized_comparison",generalizedColumn.id);
    g_generalizedGLMs[generalized.id].binaryRegression=false;
    g_generalizedComparisons[generalizedComparison.id].binaryComparison=false;
    g_generalizedGLMs[generalized.id].countRegression=true;
    g_generalizedComparisons[generalizedComparison.id].countComparison=true;
    checkModelRoute("generalized_single",generalized.id);
    checkModelRoute("generalized_comparison",generalizedColumn.id);

    g_generalizedComparisons.erase(generalizedComparison.id);
    g_regressionComparisons.erase(linearComparison.id);
    g_generalizedGLMs.erase(generalized.id);
    g_precomputedLinearModelFits.erase(numericModelId);
    g_groupModels.erase(numericModelId);
    g_plots.erase(numericSeed.id);
}}
