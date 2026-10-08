#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
using namespace rlispstat::core;
static std::vector<std::string> Payload(const std::string &name){std::ifstream f("/tmp/scale-"+name+".payload"); assert(f);std::vector<std::string> out;std::string line;while(std::getline(f,line))out.push_back(line);return out;}
// Feed native NSEvents directly to the tracking loop: posting synthesized drag
// events through AppKit replaces their location with the physical pointer.
@interface GestureProbeWindow : NSWindow
@property(nonatomic,strong) NSMutableArray *gestureEvents;
@end
@implementation GestureProbeWindow
- (NSEvent *)nextEventMatchingMask:(NSEventMask)mask untilDate:(NSDate *)date inMode:(NSRunLoopMode)mode dequeue:(BOOL)dequeue {
 if (self.gestureEvents.count) { NSEvent *event=[self.gestureEvents objectAtIndex:0];
  if (dequeue) [self.gestureEvents removeObjectAtIndex:0]; return event; }
 return [super nextEventMatchingMask:mask untilDate:date inMode:mode dequeue:dequeue];
}
@end
@interface BrushSheetProbe : DataSheetWindowController {
@public NSInteger refreshCount;
}
@end
@implementation BrushSheetProbe
- (void)refresh { ++refreshCount; [super refresh]; }
@end
static BrushSheetProbe *liveBrushSheet=nil;
static NSInteger liveBrushSheetRefreshCount=0;
@interface GestureProbeView : ScatterView {
@public
 std::vector<std::set<int>> duringDrag;
 std::vector<double> panMin;
}
@end
@implementation GestureProbeView
- (void)mouseDragged:(NSEvent *)event {
 [super mouseDragged:event];
 if (self.brushGestureActive && liveBrushSheet) {
  assert(liveBrushSheet->refreshCount==liveBrushSheetRefreshCount);
  NSTableView *table=[liveBrushSheet valueForKey:@"table"];
  std::set<int> rows;NSIndexSet *indexes=table.selectedRowIndexes;
  for(NSUInteger i=indexes.firstIndex;i!=NSNotFound;i=[indexes indexGreaterThanIndex:i]) rows.insert((int)i+1);
  assert(rows==g_groupSelections[self.model->group]);
 }
 duringDrag.push_back(g_groupSelections[self.model->group]);panMin.push_back(self.model->xmin);
}
@end
static void Gesture(GestureProbeView *v, NSPoint start, const std::vector<NSPoint> &moves, std::optional<NSPoint> release = {}) {
 auto event=[&](NSEventType type,NSPoint point){return [NSEvent mouseEventWithType:type
  location:[v convertPoint:point toView:nil] modifierFlags:0 timestamp:0
  windowNumber:v.window.windowNumber context:nil eventNumber:0 clickCount:1 pressure:1];};
 v->duringDrag.clear();v->panMin.clear();
 auto window=(GestureProbeWindow *)v.window;window.gestureEvents=[NSMutableArray array];
 for (auto point:moves) [window.gestureEvents addObject:event(NSEventTypeLeftMouseDragged,point)];
 [window.gestureEvents addObject:event(NSEventTypeLeftMouseUp,release.value_or(moves.back()))];
 [v mouseDown:event(NSEventTypeLeftMouseDown,start)];
 assert(!v.dragging && !v.brushGestureActive);
 assert(!g_liveBrushGroups.count(v.model->group));
}
static void CheckExploreGestures(const PlotModel &source) {
 // AppKit can repaint a hidden layer after this helper returns.
 static PlotModel model;model=source;model.id="gesture_probe";model.biplotLoadings.clear();
 model.points={{.2,.5,1},{.8,.5,2},{.5,.8,3}};
 model.xmin=model.ymin=model.dataXmin=model.dataYmin=0;
 model.xmax=model.ymax=model.dataXmax=model.dataYmax=1;
 model.brushWidth=model.brushHeight=30;
 auto v=[[GestureProbeView alloc]initWithModel:&model];
 auto w=[[GestureProbeWindow alloc]initWithContentRect:NSMakeRect(100,100,700,500)
  styleMask:NSWindowStyleMaskTitled backing:NSBackingStoreBuffered defer:NO];
 w.contentView=v;[w makeKeyAndOrderFront:nil];[v display];
 const auto a=[v screenPointForPoint:model.points[0]], b=[v screenPointForPoint:model.points[1]];
 liveBrushSheet=[[BrushSheetProbe alloc]initWithGroup:model.group];[liveBrushSheet show];
 g_detailDataSheetControllers.push_back(liveBrushSheet);
 liveBrushSheetRefreshCount=liveBrushSheet->refreshCount;
 model.interactionMode="brush";model.selectionMode="replace";
 MergeSelection(model.group,{},"replace");
 Gesture(v,a,{b,b});assert(v->duringDrag.size()==2);
 assert(v->duringDrag[0]==std::set<int>{1} && v->duringDrag[1]==std::set<int>{2});
 assert(g_groupSelections[model.group]==v->duringDrag.back());
 model.selectionMode="toggle";MergeSelection(model.group,{3},"replace");
 Gesture(v,a,{b,b});
 assert((v->duringDrag[0]==std::set<int>{1,3}));
 assert((v->duringDrag[1]==std::set<int>{2,3}));
 assert(g_groupSelections[model.group]==v->duringDrag.back());
 // A burst must paint the latest position, not replay thousands of old ones.
 model.selectionMode="replace";
 std::vector<NSPoint> burst(2000,b);
 const double started=CFAbsoluteTimeGetCurrent();Gesture(v,a,burst);
 assert(v->duringDrag.size()==2 && g_groupSelections[model.group]==std::set<int>{2});
 fprintf(stderr,"2000 queued brush moves coalesced to %zu live updates in %.3f s\n",
     v->duringDrag.size(),CFAbsoluteTimeGetCurrent()-started);
 // The release position is authoritative even if no drag event reached it.
 Gesture(v,a,{a},b);assert(g_groupSelections[model.group]==std::set<int>{2});
 model.interactionMode="pan";
 const auto old=g_groupSelections[model.group];
 Gesture(v,a,{NSMakePoint(a.x+30,a.y+20),NSMakePoint(a.x+60,a.y+40)});
 assert(v->panMin.size()==2 && v->panMin[0]<0 && v->panMin[1]<v->panMin[0]);
 assert(std::abs(model.xmax-model.xmin-1)<1e-10 && std::abs(model.ymax-model.ymin-1)<1e-10);
 assert(model.ymin>0 && g_groupSelections[model.group]==old);
 NSMenuItem *reset=[[NSMenuItem alloc]init];reset.representedObject=@"reset";[v explorePlot:reset];
 assert(model.xmin==0 && model.xmax==1 && model.ymin==0 && model.ymax==1);
 for (const auto &kind:{"scatter","time_series","pca_biplot"}) {
  model.kind=kind;NSMenu *menu=[v menuForEvent:nil];assert([menu itemWithTitle:@"Explore plot"]);
 }
 g_detailDataSheetControllers.erase(std::remove(g_detailDataSheetControllers.begin(),
     g_detailDataSheetControllers.end(),liveBrushSheet),g_detailDataSheetControllers.end());
 [[liveBrushSheet valueForKey:@"window"]orderOut:nil];liveBrushSheet=nil;
 MergeSelection(model.group,{},"replace");[w orderOut:nil];
 fprintf(stderr,"Live brush, stable toggle, pan, reset and shared Explore plot menus passed\n");
}
static void Drain(){dispatch_async(dispatch_get_main_queue(),^{[NSApp stop:nil];[NSApp postEvent:[NSEvent otherEventWithType:NSEventTypeApplicationDefined location:NSZeroPoint modifierFlags:0 timestamp:0 windowNumber:0 context:nil subtype:0 data1:0 data2:0] atStart:NO];});[NSApp run];}
int main(){@autoreleasepool{[NSApplication sharedApplication];[NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
 assert(HandleCommandLines(Payload("dataset")).rfind("OK",0)==0);
 assert(HandleCommandLines(Payload("result")).rfind("OK",0)==0);Drain();
 auto main=g_scaleAnalysisControllers.at("scale_visual");auto state=[main state];
 assert(state->result.imputationCount==3);
 NSWindow *window=[main valueForKey:@"window"];
 [HighResolutionPNGDataForView(window.contentView) writeToFile:@"/tmp/scale-main.png" atomically:YES];
 for(auto kind:{ScaleAnalysisChildKind::Dimensionality,ScaleAnalysisChildKind::ScreePlot,ScaleAnalysisChildKind::Biplot,ScaleAnalysisChildKind::ScaleScoreDistributionPlot,ScaleAnalysisChildKind::ItemRestPlot,ScaleAnalysisChildKind::ReliabilityIfDeletedPlot,ScaleAnalysisChildKind::ItemDistributionsPlot}){
   NSMenuItem *item=[[NSMenuItem alloc]init];item.tag=(int)kind;[main openDerivedResult:item];Drain();
   auto child=g_scaleAnalysisDerivedControllers.at("scale_visual").at((int)kind);
   NSWindow *w=[child valueForKey:@"window"];assert(w.visible);
   auto view=(ScaleAnalysisDerivedView *)[child valueForKey:@"reportView"];
   [view display];
   [HighResolutionPNGDataForView(w.contentView) writeToFile:[NSString stringWithFormat:@"/tmp/scale-child-%d.png",(int)kind] atomically:YES];
   if(kind==ScaleAnalysisChildKind::Dimensionality){for(int i=1;i<=3;i++)assert(!BuildScaleDimensionalityPresentation(state->result,i).scores.empty());

   }
   if(kind==ScaleAnalysisChildKind::Biplot || kind==ScaleAnalysisChildKind::ScreePlot || kind==ScaleAnalysisChildKind::ScaleScoreDistributionPlot){
     PlotModel model;assert(BuildScaleNativePlotModel(*state,kind,2,model));
     assert(PlotLinksToDataRows(model)==(kind==ScaleAnalysisChildKind::Biplot));
   }
   if (kind == ScaleAnalysisChildKind::Biplot) {
     ScaleNativePlotView *plot=[view valueForKey:@"nativePlotView"]; assert(plot && plot.model);
     NSMenu *menu=[child contextMenuForView:plot];
     assert([menu itemWithTitle:@"Display"] && [menu itemWithTitle:@"Explore plot"] && [menu itemWithTitle:@"Dimensionality"]);
     assert(![menu itemWithTitle:@"Extraction"]);
     NSMenu *display=[plot biplotDisplayMenu];
     NSMenuItem *overlap=[display itemWithTitle:@"Size points by overlap"];
     assert(overlap.target == plot); [plot changeBiplotDisplay:overlap]; assert(plot.model->scatterSizeByOverlap);
     // Exercise the real drawing path: distinct scores that occupy the same pixels.
     const PlotModel savedModel=*plot.model;
     plot.model->points.resize(3);
     for (int i=0;i<3;++i) {plot.model->points[i].row=i+1; plot.model->points[i].x=i==2?1.0:i*1e-8; plot.model->points[i].y=i==2?1.0:0.0;}
     plot.model->xmin=-1;plot.model->xmax=2;plot.model->ymin=-1;plot.model->ymax=2;
     plot.model->labelDisplayMode="none";plot.model->scatterSizeByOverlap=false;
     NSData *before=HighResolutionPNGDataForView(plot);assert(before);
     [plot changeBiplotDisplay:overlap];assert(plot.model->scatterSizeByOverlap);
     NSData *after=HighResolutionPNGDataForView(plot);assert(after && ![before isEqualToData:after]);
     [before writeToFile:@"/tmp/biplot-overlap-before.png" atomically:YES];
     [after writeToFile:@"/tmp/biplot-overlap-after.png" atomically:YES];
     // A case outside the zoomed panel must draw neither a mark nor a label
     // clamped onto the panel edge. Compare actual native renders.
     plot.model->scatterSizeByOverlap=false;plot.model->labelDisplayMode="all";
     plot.model->xmin=0;plot.model->xmax=1;plot.model->ymin=0;plot.model->ymax=1;
     plot.model->points={{.5,.5,1},{-.05,.5,2},{.5,-.05,3},{1.05,.5,4},{.5,1.05,5}};
     NSData *withOutside=HighResolutionPNGDataForView(plot);assert(withOutside);
     plot.model->points.resize(1);
     NSData *insideOnly=HighResolutionPNGDataForView(plot);
     assert(insideOnly && [withOutside isEqualToData:insideOnly]);
     [withOutside writeToFile:@"/tmp/biplot-zoom-clipped.png" atomically:YES];
     *plot.model=savedModel;
     CheckExploreGestures(savedModel);
     auto originalShade=plot.model->scatterShadeOverlap;
     [plot changeBiplotDisplay:[display itemWithTitle:@"Transparent points (shade overlaps)"]];
     assert(plot.model->scatterShadeOverlap != originalShade);
     NSMenuItem *labels=[[NSMenuItem alloc]init]; labels.representedObject=@"selected";
     [plot changeBiplotDisplay:labels]; assert(plot.model->labelDisplayMode=="selected");
     NSMenuItem *all=[[NSMenuItem alloc]init];all.representedObject=@"all";[plot explorePlot:all];
     const auto selected=g_groupSelections[plot.model->group];
     assert(!selected.empty());
     auto sourceLabels=RowLabelsForPlot(plot.model); assert(sourceLabels.count(plot.model->points.front().row));
     double xmin=plot.model->xmin+.01; plot.model->xmin=xmin;
     [view setNeedsDisplay:YES];[view display];
     assert(plot.model->scatterSizeByOverlap && plot.model->labelDisplayMode=="selected");
     assert(plot.model->xmin==xmin);
     NSMenuItem *clear=[[NSMenuItem alloc]init];clear.representedObject=@"clear";[plot explorePlot:clear];
     assert(g_groupSelections[plot.model->group].empty());
     [HighResolutionPNGDataForView(w.contentView) writeToFile:@"/tmp/scale-biplot-display.png" atomically:YES];
   }
   [w orderOut:nil];
 }
 auto tables=ScaleAnalysisPublicationTables(*state);assert(tables.size()==2 && tables[0].rows.size()==9);
 LatexPublicationOptions options; options.apa7=true; options.tableNumber="2"; options.title="Scale reliability and item analysis";
 NSData *pdf=PublicationTablesPDFData(tables,options);assert(pdf);[pdf writeToFile:@"/tmp/scale-apa.pdf" atomically:YES];
 assert(HandleCommandLines(Payload("plain-dataset")).rfind("OK",0)==0);
 assert(HandleCommandLines(Payload("plain-result")).rfind("OK",0)==0);Drain();
 auto plain=g_scaleAnalysisControllers.at("scale_plain");
 NSMenuItem *di=[[NSMenuItem alloc]init];di.tag=(int)ScaleAnalysisChildKind::Dimensionality;
 [plain openDerivedResult:di];Drain();
 auto child=g_scaleAnalysisDerivedControllers.at("scale_plain").at((int)ScaleAnalysisChildKind::Dimensionality);
 NSWindow *pw=[child valueForKey:@"window"];
 [HighResolutionPNGDataForView(pw.contentView) writeToFile:@"/tmp/scale-plain-dimension.png" atomically:YES];
 const auto output=MacCommandDispatcher().applicationState().outputCodeReference("scale_plain");assert(output);
 std::string code=BuildAnalysisVerificationRCode(output->provenance,"table");
 assert(code.find("smooth = FALSE")!=std::string::npos);
 assert(code.find("psych::factor.scores")!=std::string::npos);
 std::string bound;assert(BindAnalysisVerificationDataPath(code,"/tmp/scale-verification.rds",bound));
 std::ofstream("/tmp/scale-verification.R")<<bound;
 [pw orderOut:nil];[[plain valueForKey:@"window"]orderOut:nil];
 fprintf(stderr,"Scale UI, native plots, MI factor-score availability and semantic APA tables passed\n");[window orderOut:nil];
}}
