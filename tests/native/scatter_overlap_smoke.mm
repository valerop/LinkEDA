#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
using namespace rlispstat::core;
static double Difference(NSColor *a, NSColor *b) {
 a=[a colorUsingColorSpace:[NSColorSpace genericRGBColorSpace]];
 b=[b colorUsingColorSpace:[NSColorSpace genericRGBColorSpace]];
 return std::abs(a.redComponent-b.redComponent)+std::abs(a.greenComponent-b.greenComponent)+std::abs(a.blueComponent-b.blueComponent);
}
static NSMenuItem *FindMenuItem(NSMenu *menu, NSString *title) {
 for(NSMenuItem *item in menu.itemArray) {
  if([item.title isEqualToString:title]) return item;
  if(auto found=FindMenuItem(item.submenu,title)) return found;
 }
 return nil;
}
int main(){@autoreleasepool{
 [NSApplication sharedApplication];
 for (NSString *themeName in @[@"classic", @"theme_modern", @"manet"]) {
  PlotThemeOverrideScope override(themeName);
  PlotModel plot;plot.id="opaque-scatter";plot.group="opaque-scatter-data";plot.kind="scatter";
  plot.title="Uniform unselected points";plot.xLabel="Fitted";plot.yLabel="Observed";
  plot.xmin=0;plot.xmax=10;plot.ymin=0;plot.ymax=10;
  plot.points={{2,3,1},{5,3,2},{8,3,3}};
  for(int i=4;i<=24;++i)plot.points.push_back({5,3,i});
  for(int row=1;row<=24;++row)g_groupPointColors[plot.group][row]="yellow";
  auto view=[[ScatterView alloc]initWithModel:&plot];[view setFrame:NSMakeRect(0,0,720,560)];
  g_plots[plot.id]=&plot;
  NSMenuItem *pointSize=FindMenuItem([view menuForEvent:nil],@"Show point size slider");assert(pointSize);
  assert(plot.pointSizeSliderVisible==false && view.pointSizeControl.hidden);
  [view togglePointSizeControl:pointSize];
  assert(plot.pointSizeSliderVisible==true && !view.pointSizeControl.hidden);
  [view.pointSizeSlider setDoubleValue:1.8];
  [view pointSizeChanged:view.pointSizeSlider];
  assert(std::abs(plot.pointSizeScale-1.8)<1e-9);
  assert([view.pointSizeLabel.stringValue isEqualToString:@"Point size 180%"]);
  assert(FindMenuItem([view menuForEvent:nil],@"Hide point size slider"));
  NSColor *unselectedOverlapTone=nil;
  for(bool overlap:{false,true}) for(bool selected:{false,true}) {
   plot.scatterShadeOverlap=overlap;
   NSMenu *menu=[view menuForEvent:nil];
   NSMenuItem *display=FindMenuItem(menu,@"Display");assert(display);
   NSMenuItem *shade=FindMenuItem(display.submenu,@"Shade overlapping points");assert(shade);
   assert(shade.state==(overlap?NSControlStateValueOn:NSControlStateValueOff));
   assert(FindMenuItem(display.submenu,@"Size points by overlap"));
   g_groupSelections[plot.group]=selected?std::set<int>{3}:std::set<int>{};
   NSData *png=HighResolutionPNGDataForView(view);assert(png);
   NSBitmapImageRep *bitmap=[NSBitmapImageRep imageRepWithData:png];assert(bitmap);
   auto pixel=[&](double x){NSPoint p=[view screenPointForPoint:DataPoint{x,3,1}];
    double sx=bitmap.pixelsWide/[view bounds].size.width,sy=bitmap.pixelsHigh/[view bounds].size.height;
    return [bitmap colorAtX:std::lround(p.x*sx) y:std::lround(([view isFlipped]?p.y:[view bounds].size.height-p.y)*sy)];};
   NSColor *single=pixel(2),*stack=pixel(5),*last=pixel(8);
   fprintf(stderr,"%s selection=%d duplicate diff=%g selected diff=%g\n",[themeName UTF8String],selected,Difference(single,stack),Difference(single,last));
   [png writeToFile:ToNSString(std::string("/tmp/linkeda-overlap-")+[themeName UTF8String]+(overlap?"-on":"-off")+(selected?"-selected":"")+".png") atomically:YES];
   if(overlap) {
    assert(Difference(single,stack)>0.1);
    if(!selected)unselectedOverlapTone=[single retain];
    else assert(Difference(single,unselectedOverlapTone)<0.015);
   } else assert(Difference(single,stack)<0.015);
   if(selected)assert(Difference(single,last)>0.15);

   assert(plot.points.size()==24);
   if(selected&&[themeName isEqualToString:@"theme_modern"])[png writeToFile:@"/tmp/linkeda-uniform-points.png" atomically:YES];
  }
  assert(MacCommandDispatcher().dispatch({"SCATTER_TOGGLE_OVERLAP_SHADING",plot.id})=="OK");
  assert(!plot.scatterShadeOverlap);
  assert(MacCommandDispatcher().dispatch({"SCATTER_TOGGLE_OVERLAP_SIZE",plot.id})=="OK");
  assert(plot.scatterSizeByOverlap);
  NSMenuItem *size=FindMenuItem([view menuForEvent:nil],@"Size points by overlap");assert(size.state==NSControlStateValueOn);
  [HighResolutionPNGDataForView(view) writeToFile:ToNSString(std::string("/tmp/linkeda-overlap-")+[themeName UTF8String]+"-size.png") atomically:YES];
  assert(plot.points.size()==24 && g_groupSelections[plot.group]==std::set<int>{3});
  [view togglePointSizeControl:nil];
  assert(!plot.pointSizeSliderVisible && view.pointSizeControl.hidden);
  g_plots.erase(plot.id);[view release];
 }
}}
