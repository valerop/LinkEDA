#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
static std::vector<std::string> read(const std::string& file){std::ifstream f(file);std::vector<std::string> out;for(std::string s;std::getline(f,s);)out.push_back(s);return out;}
static void drain(){dispatch_async(dispatch_get_main_queue(), ^{[NSApp stop:nil];[NSApp postEvent:[NSEvent otherEventWithType:NSEventTypeApplicationDefined location:NSZeroPoint modifierFlags:0 timestamp:0 windowNumber:0 context:nil subtype:0 data1:0 data2:0] atStart:NO];});[NSApp run];}
int main(){@autoreleasepool{
 [NSApplication sharedApplication];[NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
 for(auto name:{"source","register","descriptives","summary","little"}){
   const auto reply=HandleCommandLines(read(std::string("/tmp/missing-")+name+".txt"));
   fprintf(stderr,"%s: %s\n",name,reply.c_str());assert(reply.rfind("OK",0)==0);drain();
 }
 auto ids=read("/tmp/missing-report-ids.txt");auto a=g_table1Controllers.at(ids[0]),b=g_table1Controllers.at(ids[1]),c=g_table1Controllers.at(ids[2]);
 assert([a isVisible]);assert([b isVisible]);assert([c isVisible]);
 assert(g_dataSheetControllers.empty());
 NSView* first=[a valueForKey:@"reportView"];NSView* second=[b valueForKey:@"reportView"];
 assert([first window]!=[second window]);assert([c getWindow]!=[a getWindow]);
 assert([c state]->tableType=="missing_data_test");
 assert([c state]->showP);assert(![c state]->rows[0].p.empty());
 assert([c state]->codeReference.publication.table->rows.size()==1);
 assert([c state]->codeReference.publication.table->columns.back().label=="p");
 assert([a state]->codeReference.publication.table->rows.size()==4);
 for(const auto& row:[a state]->rows){fprintf(stderr,"ROW %s",row.label.c_str());for(auto& v:row.values)fprintf(stderr," [%s]",v.c_str());fprintf(stderr,"\n");}
 assert([a state]->rows[0].values[0]=="•");
 assert([a state]->stubHeaders[0]=="Variable");
 assert([a state]->missingnessSource=="missing_native");
 assert([a state]->addVariableOptions==std::vector<std::string>{"group"});
 [a selectMissingPattern:1];drain();
 std::set<int> selected;MacCommandDispatcher().applicationState().selectedRows("missing_native",selected);
 const auto expected=[a state]->missingnessPatternRows.at("P2");
 assert(selected==std::set<int>(expected.begin(),expected.end()));
 assert(![[a valueForKey:@"addVariableButton"] isHidden]);
 NSMenu* actions=[a missingnessMenu];
 assert([actions itemWithTitle:@"Descriptives for all patterns"]);
 assert([actions itemWithTitle:@"Save to data"]);
 [a missingPatternAction:[actions itemWithTitle:@"Little's MCAR test (continuous variables)"]];
 assert(g_pendingMissingDataTasks.back().kind=="missing_data_action");
 assert(g_pendingMissingDataTasks.back().secondary=="little");
 assert([a state]->tableType=="missing_data_overview");assert([a state]->rows.size()==4);
 assert(![a state]->codeReference.provenance.verificationRCode.empty());
 NSMenu* menu=[a contextMenuForRow:0];assert(menu.numberOfItems>0);
 NSView* content=[a valueForKey:@"reportView"];[content displayIfNeeded];
 auto rep=[content bitmapImageRepForCachingDisplayInRect:content.bounds];[content cacheDisplayInRect:content.bounds toBitmapImageRep:rep];
 [[rep representationUsingType:NSBitmapImageFileTypePNG properties:@{}] writeToFile:@"/tmp/missing-report-preview.png" atomically:YES];
 fprintf(stderr,"SEPARATE MISSINGNESS WINDOWS VERIFIED\n");
 [[a getWindow] close]; [[b getWindow] close]; [[c getWindow] close];
}}
