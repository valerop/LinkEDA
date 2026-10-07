#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
using namespace rlispstat::core;
static NSMenuItem *Find(NSMenu *menu, NSString *title) {
    for(NSMenuItem *item in menu.itemArray) {
        if([item.title isEqualToString:title]) return item;
        if(auto found=Find(item.submenu,title)) return found;
    }
    return nil;
}
int main(){ @autoreleasepool {
    [NSApplication sharedApplication];
    DataFrameModel data; data.group="partial-menu-data";data.rows=5;
    DataColumn category;category.name="group";category.type="categorical";
    category.values={"A","B","A","B","A"};data.columns.push_back(category);
    MacCommandDispatcher().applicationState().datasets().registerDataset(data);
    PlotModel plot;plot.id="partial-menu";plot.group=data.group;plot.kind="scatter";
    plot.isGLMDiagnostic=true;plot.isRegressionDerivedPlot=true;
    plot.glmDiagnosticKind="partial_regression";plot.regressionDerivedKind="partial_regression_plot";
    plot.regressionDerivedTerm="x";plot.regressionPartialAvailableTerms={"x","group"};
    plot.diagnosticAvailableResidualTypes={"raw","standardized","studentized"};
    plot.displayedResidualType="studentized";plot.regressionPartialResidualType="studentized";
    plot.regressionPartialContributionScale="studentized";
    plot.regressionPartialPointsByImputation={{{.2,.4,2},{.7,1.2,5}},{{.3,.5,2},{.8,1.3,5}}};
    plot.diagnosticImputationIndex=2;plot.diagnosticImputationCount=2;
    assert(ApplyStoredPartialRegressionPlot(&plot));
    auto view=[[ScatterView alloc]initWithModel:&plot];
    [view setFrame:NSMakeRect(0,0,720,560)];
    NSMenu *axis=[view diagnosticAxisMenuForX:YES];
    assert(axis.numberOfItems==2);assert(Find(axis,@"x").state==NSControlStateValueOn);
    assert(Find(axis,@"group").action==@selector(selectPartialPlotTerm:));
    assert(Find([view diagnosticAxisMenuForX:NO],ToNSString(GLMRawResidualTitle())));
    NSMenu *menu=[view menuForEvent:nil];
    NSMenuItem *color=Find(menu,@"Color by");assert(color);assert(Find(color.submenu,@"group"));
    assert(!Find(menu,@"Change X Variable"));
    MacCommandDispatcher().applicationState().plots()[plot.id]=&plot;
    DispatchCommand(&plot,"PLOT_COLOR_BY|group");
    assert(plot.colorByVariable=="group");
    const auto colors=plot.colorByRowColors;
    assert(colors.at(2)!=colors.at(5));
    assert(plot.points[0].row==2 && plot.points[1].row==5);
    // Reproject another predictor/residual/imputation while preserving row colors.
    plot.regressionDerivedTerm="group";plot.regressionPartialResidualType="raw";
    plot.regressionPartialContributionScale="";plot.diagnosticImputationIndex=1;
    assert(ApplyStoredPartialRegressionPlot(&plot));
    assert(plot.colorByRowColors==colors && plot.colorByVariable=="group");
    assert(plot.xLabel=="group contribution");
    assert(Find([view menuForEvent:nil],@"Color legend"));
    [HighResolutionPNGDataForView(view) writeToFile:@"/tmp/linkeda-partial-menu.png" atomically:YES];
    DispatchCommand(&plot,"PLOT_COLOR_BY|.");assert(plot.colorByRowColors.empty());
    MacCommandDispatcher().applicationState().plots().erase(plot.id);[view release];
}}
