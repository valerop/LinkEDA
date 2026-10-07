#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
static GLMFitSummary fixture;
@interface LinearMenuTestController : GLMWindowController
@end
@implementation LinearMenuTestController
- (GLMFitSummary)currentFit { return fixture; }
@end
static NSMenuItem *Find(NSMenu *menu, NSString *title) {
 for(NSMenuItem *item in menu.itemArray) {
  if([item.title isEqualToString:title])return item;
  if(auto found=Find(item.submenu,title))return found;
 }
 return nil;
}
static void CheckDiagnostics(NSMenu *menu) {
 assert(!Find(menu,@"Open diagnostic plot"));
 assert(!Find(menu,@"Compare with null model"));
 assert(!Find(menu,@"Model details…"));
 NSMenuItem *diagnostics=Find(menu,@"Open Diagnostics");assert(diagnostics.submenu);
 const auto options=rlispstat::core::ModelDiagnosticPlotOptions(true);
 assert(diagnostics.submenu.numberOfItems==options.size());
 for(const auto &option:options) {
  NSMenuItem *item=Find(diagnostics.submenu,ToNSString(option.title));assert(item);
  assert([item.representedObject isEqualToString:ToNSString(option.value)]);
  assert(item.action==@selector(openDiagnosticFromMenu:));
 }
}
int main(){@autoreleasepool{
 [NSApplication sharedApplication];
 PlotModel model;model.group="linear-menu-test";model.kind="scatter";
 model.variables={{"x",{1,2,3}},{"y",{2,3,4}}};
 auto &state=g_groupModels[model.group];state.group=model.group;state.response="y";state.terms={"x"};state.termTypes["x"]="numeric";
 fixture.ok=true;fixture.n=20;fixture.dfResidual=18;fixture.r2=.2;
 for(const auto &name:{"(Intercept)","x"}){
  GLMCoefficientRow row;row.term=row.displayLabel=row.sourceTerm=name;row.rowType="coefficient";row.termType="numeric";row.estimate=1;row.stdError=.2;row.tValue=5;row.pValue=.001;fixture.coefficients.push_back(row);
 }
 auto controller=[[LinearMenuTestController alloc]initWithModel:&model];
 NSPopUpButton *button=[controller valueForKey:@"modelActionsPopup"];
 assert(button && !button.hidden && button.pullsDown);
 assert([button.title isEqualToString:@"Model"]);
 CheckDiagnostics(button.menu);
 NSWindow *window=[controller valueForKey:@"window"];
 CheckDiagnostics(window.contentView.menu);
 for(NSString *key in @[@"fitSummaryField",@"anovaModelField",@"anovaResidualField",@"anovaModelSSField",@"anovaModelFField",@"anovaModelPField"]){
  NSTextField *field=[controller valueForKey:key];CheckDiagnostics(field.menu);
 }
 CheckDiagnostics([controller contextMenuForCoefficientRow:1 column:2]);
 CheckDiagnostics([controller contextMenuForCoefficientRow:1 column:0]);
 CheckDiagnostics([controller contextMenuForCoefficientRow:-1 column:-1]);
 [controller refresh];
 NSTextField *title=[controller valueForKey:@"titleField"];
 NSTextField *badge=[controller valueForKey:@"badgeField"];
 assert(NSContainsRect(window.contentView.bounds,button.frame));
 assert(!NSIntersectsRect(button.frame,title.frame));assert(!NSIntersectsRect(button.frame,badge.frame));
 NSData *png=HighResolutionPNGDataForView(window.contentView);assert(png.length>1000);
 [png writeToFile:@"/tmp/linkeda-linear-model-menu.png" atomically:YES];
 [window close];[controller release];
}}
