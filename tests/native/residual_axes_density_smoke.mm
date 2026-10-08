#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
using namespace rlispstat::core;
static NSMenuItem *FindChoice(NSMenu *menu, const std::string &choice) {
 for(NSMenuItem *item in [menu itemArray]) {
  if([[item representedObject] isKindOfClass:[NSString class]] && [(NSString *)[item representedObject] isEqualToString:ToNSString(choice)])return item;
  if(auto found=FindChoice([item submenu],choice))return found;
 }
 return nil;
}
int main(){ @autoreleasepool {
 [NSApplication sharedApplication];
 // Only render and project values: fixtures stand in for vectors returned by R.
 GeneralizedGLMState base; base.id="residual-menu-fit";base.group="residual-menu-data";base.ok=true;base.modelVersion=1;base.fitVersion=1;base.diagnosticsVersion=1;
 for(int i=1;i<=24;++i){GeneralizedDiagnosticRow row;row.row=i;row.fitted=i/10.;row.observed=i%2;
 row.rawResidual=i/7.-1;row.pearsonResidual=i/6.-1;row.devianceResidual=i/5.-1;row.workingResidual=i/4.-1;row.standardizedResidual=i/3.-1;row.studentizedResidual=i/2.-1;row.dunnSmythResidual=i/8.-1;row.leverage=.01*i;row.cooksDistance=.01*i;base.diagnostics.push_back(row);}
 std::vector<GeneralizedGLMState> states;
 for(const auto &family:{"binomial","poisson","quasipoisson","Gamma","inverse.gaussian","gaussian","lognormal","beta","beta_one_inflated"}){auto state=base;state.family=family;states.push_back(state);}
 for(auto dist:{CountDistribution::Poisson,CountDistribution::QuasiPoisson,CountDistribution::NegativeBinomial,CountDistribution::BinomialTrials,CountDistribution::BetaBinomial,CountDistribution::HurdleBetaBinomialCeiling,CountDistribution::PerfectScore}){auto state=base;state.countRegression=true;state.countDistribution=dist;states.push_back(state);}
 for(auto state:states)for(bool mi:{false,true}){
  if(mi){state.diagnosticsByImputation={state.diagnostics,state.diagnostics};for(auto &row:state.diagnosticsByImputation[1])row.pearsonResidual+=.25;}
  g_generalizedGLMs[state.id]=state;
  for(const auto &kind:{"residuals_fitted","normal_qq","scale_location","residuals_leverage","residual_histogram","partial_regression"}){
   PlotModel plot;plot.id="residual-menu-plot";plot.group=state.group;plot.glmModelId=state.id;plot.isGLMDiagnostic=true;plot.glmDiagnosticKind=kind;plot.generalizedDiagnosticResiduals=true;plot.diagnosticAvailableResidualTypes=AvailableGeneralizedResidualTypes(state);plot.diagnosticImputationIndex=mi?2:1;
   plot.kind=std::string(kind)=="residual_histogram"?"histogram":"scatter";
   auto scatter=[[ScatterView alloc]initWithModel:&plot];auto hist=[[HistogramView alloc]initWithModel:&plot];
   const auto choices=DiagnosticPlotResidualChoices(plot);assert(!choices.empty());
   NSMenu *axis=plot.kind=="histogram"?[hist histogramAxisMenu]:[scatter diagnosticAxisMenuForX:NO];assert(axis);
   if(plot.kind!="histogram")assert(![scatter diagnosticAxisMenuForX:YES]);
   for(const auto &choice:choices){assert(FindChoice(axis,choice));
    // Partial residuals have a separate R computation path, tested by the R suite.
    if(std::string(kind)=="partial_regression")continue;
    plot.displayedResidualType="";
    assert(SelectDiagnosticResidualType(&plot,ToNSString(choice)));
    assert(plot.displayedResidualType==choice);
    const auto &rows=mi?state.diagnosticsByImputation[1]:state.diagnostics;
    const auto expected=BuildGeneralizedDiagnosticPlotData(kind,choice,rows,1,1);assert(expected.ok);
    if(plot.kind=="histogram")assert(std::abs(plot.histogramPoints.front().x-expected.points.front().x)<1e-10);
    else assert(std::abs(plot.points.front().y-expected.points.front().y)<1e-10);
    assert(plot.diagnosticImputationIndex==(mi?2:1));
   }
   [scatter release];[hist release];
  }
 }
 PlotModel linear;linear.isGLMDiagnostic=true;linear.glmDiagnosticKind="residuals_fitted";
 assert(DiagnosticPlotResidualChoices(linear)==AvailableLinearResidualTypes());
 linear.glmDiagnosticKind="observed_fitted";assert(DiagnosticPlotResidualChoices(linear).empty());
 // Smoothing changes the curve continuously while retaining bins and cases.
 PlotModel plot;plot.id="density-slider";plot.kind="histogram";
 for(int i=1;i<=40;++i)plot.histogramPoints.push_back({i<30?i*.1:10+i*.2,i,0});
 RebinHistogram(plot,12);plot.histogramShowDensity=true;
 MacCommandDispatcher().applicationState().plots()[plot.id]=&plot;
 auto hist=[[HistogramView alloc]initWithModel:&plot];g_histogramViews.push_back(hist);
 assert(![hist.densityAdjustControl isHidden]);assert([hist.densityAdjustSlider isContinuous]);
 const auto first=DensityCurvesForHistogram(plot,{},{});assert(!first.empty());
 [hist.densityAdjustSlider setDoubleValue:2.3];[hist densityAdjustChanged:hist.densityAdjustSlider];
 assert(std::abs(plot.histogramDensityAdjust-2.3)<1e-8);
 const auto second=DensityCurvesForHistogram(plot,{},{});assert(!second.empty());assert(first[0].y!=second[0].y);
 assert(plot.histogramBins.size()==12 && plot.histogramPoints.size()==40);
 [hist visualExportDataForFormat:@"PNG"];assert(![hist.densityAdjustControl isHidden]);
 // A slider edit must retain the frozen model recipe, not turn residuals into a data column.
 base.family="poisson";base.provenance.analysisId="frozen-fit";
 base.provenance.verificationRCode["model"]="reference_fit <- stats::glm(y ~ x, data = analysis_data, family = poisson())\n";
 g_generalizedGLMs[base.id]=base;
 plot.isGLMDiagnostic=true;plot.glmModelId=base.id;plot.glmDiagnosticKind="residual_histogram";
 plot.displayedResidualType="pearson";plot.diagnosticAvailableResidualTypes=AvailableGeneralizedResidualTypes(base);
 [hist.densityAdjustSlider setDoubleValue:1.7];[hist densityAdjustChanged:hist.densityAdjustSlider];
 const auto code=plot.codeReference.provenance.verificationRCode.at("diagnostic:residual_histogram");
 assert(code.find("reference_fit <- stats::glm")!=std::string::npos);
 assert(code.find("adjust = 1.7")!=std::string::npos);
 assert(code.find("bins = 12")!=std::string::npos);
 assert(plot.codeReference.analysisId=="frozen-fit");
 [hist.densityAdjustSlider setDoubleValue:2.3];[hist densityAdjustChanged:hist.densityAdjustSlider];
 // Changing residuals must preserve the visible curve and its controls.
 auto data=BuildGeneralizedDiagnosticPlotData("residual_histogram","pearson",base.diagnostics,1,1);
 assert(ApplyDiagnosticPlotData(&plot,data));assert(plot.histogramShowDensity);assert(plot.histogramBins.size()==12);assert(std::abs(plot.histogramDensityAdjust-2.3)<1e-8);
 g_histogramViews.clear();MacCommandDispatcher().applicationState().plots().erase(plot.id);[hist release];
}}
