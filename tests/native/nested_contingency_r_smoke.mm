
#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
#include <fstream>
#include <iostream>
static std::vector<std::string> Payload(const std::string& folder,const std::string& name) {
 std::ifstream in(folder+"/"+name+".payload");assert(in);std::vector<std::string> out;std::string line;
 while(std::getline(in,line))out.push_back(line);return out;
}
int main(int argc,char** argv){@autoreleasepool{
 assert(argc==2);[NSApplication sharedApplication];const std::string folder=argv[1];
 assert(HandleCommandLines(Payload(folder,"dataset")).rfind("OK",0)==0);
 auto& app=MacCommandDispatcher().applicationState();auto* df=app.datasets().find("nested-transport");assert(df);
 auto scope=rlispstat::core::ExplicitAnalysisScope(df->group,{1,2,3,4,5},rlispstat::core::AnalysisScopeSourceKind::OtherExplicitSubset,"Chosen cases",6);
 assert(app.setActiveAnalysisScope(scope));
 auto initial=rlispstat::core::NestedContingencyTableStateForDataFrame(*df,"nested-result",{"a","b"},"g","count_percent",&scope);
 initial.sourcePlotId="bar-source";initial.linkEnabled=true;ShowTable1StateOnMain(initial);
 auto* controller=g_table1Controllers.at(initial.id);
 auto task=g_pendingTable1Tasks.back();assert(task.analysisKind=="nested_contingency" && task.rows==scope.originalRowIds);
 auto payload=Payload(folder,"count_percent");payload[4]=std::to_string(task.revision);payload[5]=std::to_string(task.dataVersion);
 auto reply=HandleCommandLines(payload);if(reply!="OK\tnested-result")std::cerr<<reply<<'\n';assert(reply=="OK\tnested-result");
 auto* state=[controller state];assert(state->tableType=="nested_contingency" && state->sourcePlotId=="bar-source" && state->linkEnabled);
 assert(state->rows.back().values==std::vector<std::string>({"2 (50.0%)","2 (50.0%)","4 (100.0%)"}));
 assert(rlispstat::core::ContingencyRowsForRowPrefix(*state,{"A"})==std::set<int>({1,2}));
 assert(!state->codeReference.provenance.executedRCode.empty());
 [controller rebuildNestedContingencyFromSelection];
 assert(g_pendingTable1Tasks.back().analysisKind=="nested_contingency");
 assert(HandleCommandLines(payload).find("ignored stale")!=std::string::npos);
 // Removing the final variable supersedes a pending calculation and leaves an editable empty window.
 auto blank=rlispstat::core::NestedContingencyTableStateForDataFrame(*df,initial.id,{},"g","count_percent",&scope);
 [controller updateState:blank];auto emptyTask=g_pendingTable1Tasks.back();assert(emptyTask.variables.empty());
 auto error=std::vector<std::string>{"TABLE1_OPEN_ERROR",initial.id,df->group,"REQUEST_V1",std::to_string(emptyTask.revision),std::to_string(emptyTask.dataVersion),"Add a row variable"};
 assert(HandleCommandLines(error)=="OK");assert([controller state]->rows.empty());
 [[controller getWindow] close];assert(g_table1Controllers.count(initial.id)==0);
 // All-numeric inputs require an explicit categorical type assignment.
 assert(HandleCommandLines(Payload(folder,"numeric-dataset")).rfind("OK",0)==0);
 auto* cars=app.datasets().find("contingency-numeric");assert(cars);
 auto definition=rlispstat::core::SharedAnalysisDefinition(rlispstat::core::SharedAnalysisKind::ContingencyTable);
 auto initialCars=rlispstat::core::ResolveInitialAnalysisSpecification(*cars,definition,{});
 auto* numericSetup=[[ContingencyTableSetupWindowController alloc] initWithId:"numeric-setup"
     dataframe:*cars initial:initialCars sourcePlotId:""];
 auto* numericRow=(NSPopUpButton*)[numericSetup valueForKey:@"rowPopup"];
 assert([numericRow numberOfItems]==1);
 assert(![(NSButton*)[numericSetup valueForKey:@"runButton"] isEnabled]);
 [[numericSetup valueForKey:@"window"] close];
 // Imported text categories and ordinal variables remain available.
 assert(HandleCommandLines(Payload(folder,"alien-dataset")).rfind("OK",0)==0);
 auto* alien=app.datasets().find("alien-import-categories");assert(alien);
 auto setupSpec=rlispstat::core::ResolveInitialAnalysisSpecification(*alien,definition,{});
 auto* setup=[[ContingencyTableSetupWindowController alloc] initWithId:"alien-setup"
     dataframe:*alien initial:setupSpec sourcePlotId:""];
 auto* row=(NSPopUpButton*)[setup valueForKey:@"rowPopup"];
 auto* col=(NSPopUpButton*)[setup valueForKey:@"columnPopup"];
 auto* run=(NSButton*)[setup valueForKey:@"runButton"];
 for(NSString* name in @[@"planet",@"happy",@"rating"])
   assert([row indexOfItemWithTitle:name]>0 && [col indexOfItemWithTitle:name]>0);
 for(NSString* name in @[@"alien_id",@"humans_eaten",@"size"])
   assert([row indexOfItemWithTitle:name]==-1 && [col indexOfItemWithTitle:name]==-1);
 assert(![run isEnabled]);
 [row selectItemWithTitle:@"planet"];[row sendAction:[row action] to:[row target]];
 assert(![run isEnabled]);
 [col selectItemWithTitle:@"planet"];[col sendAction:[col action] to:[col target]];
 assert(![run isEnabled]);
 [col selectItemWithTitle:@"happy"];[col sendAction:[col action] to:[col target]];
 assert([run isEnabled]);[run performClick:nil];
 const auto taskAlien=g_pendingTable1Tasks.back();
 assert(taskAlien.group==alien->group && taskAlien.analysisKind=="nested_contingency");
 assert(taskAlien.variables==std::vector<std::string>({"planet"}) && taskAlien.groupVariable=="happy");
 auto* alienController=g_table1Controllers.at(taskAlien.id);
 for(NSMenu* menu in @[[alienController contextMenuForNestedRowVariable:std::string("planet")],
                       [alienController contextMenuForNestedSplitVariable],
                       [alienController contextMenuForRow:-1]]) {
   std::function<bool(NSMenu*,NSString*)> contains = [&](NSMenu* current,NSString* title) {
     for(NSMenuItem* item in [current itemArray]) {
       if([[item title] isEqualToString:title]) return true;
       if([item submenu] && contains([item submenu],title)) return true;
     }return false;
   };
   assert(contains(menu,@"rating"));
   assert(!contains(menu,@"humans_eaten") && !contains(menu,@"alien_id") && !contains(menu,@"size"));
 }
 [[alienController getWindow] close];
 std::cout<<"Categorical contingency setup and edit menus: imported categories, ordinals, numeric/text exclusions, validation and dispatch passed.\n";
 std::cout<<"Native nested tables: R queue, display, linkage, scope, refit, empty specification and close passed.\n";
}}
