#include "../../src/core/scatterplot_model.h"
#include "../../src/core/format_model.h"
#include <cassert>
#include <cmath>
using namespace rlispstat::core;
int main() {
 ScatterplotRenderInput input;
 input.viewport={0,10,0,10};input.plotRect={0,0,400,400};
 input.points={{1,2,3},{2,5,3},{3,5,3},{4,5,3},{5,5,3},{6,8,3}};
 for(bool shading:{false,true}) for(bool sizing:{false,true}) {
  input.shadeOverlap=shading;input.sizeByOverlap=sizing;
  input.selectedRows.clear();auto before=BuildScatterplotRenderPlan(input);
  input.selectedRows={3};auto after=BuildScatterplotRenderPlan(input);
  assert(before.points.size()==6 && after.points.size()==6);
  for(const auto &point:after.points) {
   assert(point.shadeOverlap==shading);
   if(sizing)assert(std::abs(point.radius-(point.caseId>=2&&point.caseId<=5?6.:3.))<1e-12);
   if(shading&&!point.selected) {
    assert(point.fillAlpha==0.35 && point.strokeAlpha==0.35);
    for(const auto &old:before.points)if(old.caseId==point.caseId)assert(old.fillAlpha==point.fillAlpha);
   }
  }
  assert(after.points.back().selected && after.points.back().caseId==3);
 }
 for(const auto &name:PlotThemeNames()) {
  auto theme=PlotThemeStyleForName(name);
  auto opaque=PlotMarkColor(PlotColorForName("yellow"),theme.panel,false,0.35);
  auto transparent=PlotMarkColor(PlotColorForName("yellow"),theme.panel,true,0.35);
  assert(opaque.a==1.0 && transparent.a==0.35);
 }
}
