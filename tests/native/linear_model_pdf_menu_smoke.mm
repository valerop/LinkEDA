#include "../../src/platform/macos/linkeda_macos_app.mm"
#import <objc/runtime.h>
#include <cassert>
#include <iostream>
static int prompts=0,saves=0;static bool cancelDetails=true;
static NSInteger Details(id object,SEL){
 auto a=(NSAlert*)object;assert([a.messageText isEqualToString:@"APA 7 table details"]);++prompts;
 auto views=[(NSStackView*)a.accessoryView arrangedSubviews];assert(views.count==4);
 assert([[(NSTextField*)views[0] stringValue] isEqualToString:@"Table number"]);
 assert([[(NSTextField*)views[2] stringValue] isEqualToString:@"Table title"]);
 [(NSTextField*)views[1] setStringValue:@"7"];[(NSTextField*)views[3] setStringValue:@"My adjusted model"];
 return cancelDetails?NSAlertSecondButtonReturn:NSAlertFirstButtonReturn;
}
static NSInteger Save(id object,SEL){assert(prompts>=2);++saves;assert([[(NSSavePanel*)object nameFieldStringValue] hasSuffix:@"_APA7.pdf"]);return NSModalResponseCancel;}
int main(){@autoreleasepool{
 [NSApplication sharedApplication];using namespace rlispstat::core;
 GroupModelState state;state.response="y";GLMFitSummary fit;fit.ok=true;fit.n=10;
 auto table=*LinearModelCodeReference(state,fit).publication.table;auto reports=LinearModelReportTables(state,fit);
 NSMenu *menu=[[NSMenu alloc]initWithTitle:@"Export"];
 AddStandardTableVisualExportItems(menu,nil,@"Linear Model",@"linear-model",@"Variable\tb\nx\t1",&table,&reports);
 auto copy=[[menu itemWithTitle:@"Copy"] submenu];auto save=[[menu itemWithTitle:@"Save"] submenu];
 assert(copy && save && [copy itemWithTitle:@"PDF image (vector)"]);
 auto normal=[save itemWithTitle:@"PDF (as shown)…"];auto apa=[save itemWithTitle:@"PDF (APA 7 table)…"];
 assert(normal&&apa);assert(normal.target==apa.target);assert(normal.action==@selector(saveVisualExport:));assert(apa.action==@selector(saveAPAPDF:));
 auto detailsMethod=class_getInstanceMethod([NSAlert class],@selector(runModal));auto saveMethod=class_getInstanceMethod([NSSavePanel class],@selector(runModal));
 auto originalDetails=method_setImplementation(detailsMethod,(IMP)Details);auto originalSave=method_setImplementation(saveMethod,(IMP)Save);
 [(MacVisualTableExportTarget*)apa.target saveAPAPDF:apa];assert(prompts==1&&saves==0);
 cancelDetails=false;[(MacVisualTableExportTarget*)apa.target saveAPAPDF:apa];assert(prompts==2&&saves==1);
 cancelDetails=true;
 for(auto type:{StatisticalModelType::Binary,StatisticalModelType::Count,
                StatisticalModelType::PositiveContinuous,StatisticalModelType::Proportion,
                StatisticalModelType::LegacyGeneralized}) for(bool mi:{false,true}) {
  GeneralizedGLMState s;s.id="apa-menu-model";s.group="apa-menu-data";s.response="y";
  s.ok=true;s.modelType=type;s.multipleImputation=mi;s.binaryRegression=type==StatisticalModelType::Binary;
  s.countRegression=type==StatisticalModelType::Count;
  g_generalizedGLMs[s.id]=s;
  auto controller=[[GeneralizedGLMWindowController alloc]initWithModelId:s.id];
  auto exports=[[controller modelExportMenuItem] submenu];
  auto modelSave=[[exports itemWithTitle:@"Save"] submenu];
  auto item=[modelSave itemWithTitle:@"PDF (APA 7 table)…"];
  assert(item && item.action==@selector(saveAPAPDF:));
  int before=prompts,beforeSaves=saves;
  [(MacVisualTableExportTarget*)item.target saveAPAPDF:item];
  assert(prompts==before+1 && saves==beforeSaves+1);
 }
 method_setImplementation(detailsMethod,originalDetails);method_setImplementation(saveMethod,originalSave);
 std::cout<<"Normal/APA menu separation, number/title prompt and both cancellation paths passed\n";
}}
