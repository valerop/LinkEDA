#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
#import <PDFKit/PDFKit.h>
static GLMFitSummary testFit;
@interface PolynomialExportController : GLMWindowController
- (instancetype)initForTest:(PlotModel *)model;
@end
@implementation PolynomialExportController
- (instancetype)initForTest:(PlotModel *)model { self=[super init]; if(self) _model=model; return self; }
- (GLMFitSummary)currentFit { return testFit; }
@end
static NSMenuItem *ByTag(NSMenu *menu, NSInteger tag) {
 for(NSMenuItem *item in [menu itemArray]) if([item tag]==tag && [[item target] isKindOfClass:[MacVisualTableExportTarget class]])return item;
 return nil;
}
int main() {
 @autoreleasepool {
  [NSApplication sharedApplication];
  PlotModel model;model.group="polynomial-export-test";
  g_groupModels[model.group].group=model.group;g_groupModels[model.group].response="response";
  auto controller=[[PolynomialExportController alloc] initForTest:&model];
  auto root=[controller regressionExportMenuItem];NSMenu *menu=[root submenu];
  [controller menuNeedsUpdate:menu];assert(!ByTag(menu,0));
  testFit.ok=true;testFit.n=120;testFit.dfResidual=117;
  GLMCoefficientRow row;row.term=row.displayLabel="I(x^2)";row.rowType="coefficient";row.estimate=1.625;row.stdError=.21;row.tValue=7.738;row.pValue=.0001;
  testFit.coefficients.push_back(row);
  [controller menuNeedsUpdate:menu];auto target=(MacVisualTableExportTarget *)[ByTag(menu,0) target];assert(target);
  NSString *tsv=[target valueForKey:@"tabDelimited"];
  assert([tsv containsString:@"I(x^2)"] && [tsv containsString:@"1.625"]);
  [target retain];
  // Reopening the persistent menu must capture the changed result.
  testFit.coefficients[0].estimate=2.875;
  [controller menuNeedsUpdate:menu];auto changed=(MacVisualTableExportTarget *)[ByTag(menu,2) target];assert(changed);
  assert([[changed valueForKey:@"tabDelimited"] containsString:@"2.875"]);
  assert([[target valueForKey:@"tabDelimited"] containsString:@"1.625"]);
  NSData *svg=[changed svgData];assert([svg length]>100);
  NSString *svgText=[[[NSString alloc]initWithData:svg encoding:NSUTF8StringEncoding]autorelease];
  assert([svgText containsString:@"I(x^2)"] && ![svgText containsString:@"No results"]);
  NSData *png=[changed pngData];assert([png length]>1000);[png writeToFile:@"/tmp/linkeda-polynomial-copy.png" atomically:YES];
  NSData *pdf=[changed pdfData];assert([pdf length]>1000);[pdf writeToFile:@"/tmp/linkeda-polynomial-copy.pdf" atomically:YES];
  PDFDocument *document = [[PDFDocument alloc] initWithData:pdf];
  assert([[document string] containsString:@"2.875"] && [[document string] containsString:@"I(x^2)"]);
  assert(![[document string] containsString:@"No results"]); [document release];
  // A pending refit cannot export an empty or stale table.
  testFit.ok=false;[controller menuNeedsUpdate:menu];assert(!ByTag(menu,0));
  NSMenu *terms=[[NSMenu alloc]initWithTitle:@"terms"];
  assert(AddPolynomialChoices(terms,controller,@selector(addPredictorFromMenu:),{"x","y"},"y",{"x","I(x^2)"}));
  NSMenu *powers=[[[[terms itemAtIndex:0]submenu]itemAtIndex:0]submenu];assert([powers numberOfItems]==3);
  assert([[[powers itemAtIndex:0]representedObject]isEqualToString:@"I(x^3)"]);
  PlotModel cardinalityModel;
  cardinalityModel.variables={{"am",{0,1,0,1}},{"cyl",{4,6,8,4}},{"wt",{2.1,2.4,3.2,3.5,4.0,4.7}}};
  cardinalityModel.variableMeta={{"am","numeric"},{"cyl","numeric"},{"wt","numeric"}};
  NSMenu *limited=[[NSMenu alloc]initWithTitle:@"limited"];
  assert(AddPolynomialMenuItems(limited,controller,@selector(addPredictorFromMenu:),
                                &cardinalityModel,"drat",{},"",{}));
  NSMenu *limitedPowers=[[limited itemAtIndex:0]submenu];
  assert([limitedPowers numberOfItems]==2);
  assert([[[limitedPowers itemAtIndex:0]title]isEqualToString:@"cyl"]);
  assert([[[limitedPowers itemAtIndex:0]submenu]numberOfItems]==1);
  assert([[[limitedPowers itemAtIndex:1]title]isEqualToString:@"wt"]);
  [target release];[controller release];
 }
}
