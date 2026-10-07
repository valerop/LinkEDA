#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
#include <iostream>
using namespace rlispstat::core;
int main(){@autoreleasepool{
 [NSApplication sharedApplication];
 int checks=0;
 for(auto dist:{CountDistribution::Poisson,CountDistribution::QuasiPoisson,CountDistribution::NegativeBinomial,CountDistribution::BinomialTrials,CountDistribution::BetaBinomial,CountDistribution::HurdleBetaBinomialCeiling,CountDistribution::PerfectScore})for(bool mi:{false,true}) {
  GeneralizedGLMState s;s.id="count-help";s.group="count-help-data";s.response="y";s.terms={"x","group"};s.ok=true;s.autoRefit=true;s.countRegression=true;s.countDistribution=dist;s.multipleImputation=mi;
  s.family=CountDistributionUsesTrials(dist)?"binomial":"poisson";s.link=CountDistributionUsesTrials(dist)?"logit":"log";s.statisticName=mi?"t":"z";
  s.n=835;s.dfResidual=824;s.theta=4.342;s.thetaDescriptiveMean=4.342;s.thetaDescriptiveMin=3.279;s.thetaDescriptiveMax=5.202;s.dispersion=1;s.betaBinomialDispersion=.2;s.trialsConstant=24;
  s.likelihoodAvailable=!mi&&dist!=CountDistribution::QuasiPoisson;s.logLik=-100;s.aic=220;s.bic=230;s.nullDeviance=1118.820;s.residualDeviance=944.637;
  GeneralizedGLMRow a;a.term="(Intercept)";a.rowType="intercept";a.estimate=.61;a.stdError=.64;a.statistic=.94;a.pValue=.34;
  GeneralizedGLMRow b=a;b.term="x";b.rowType="coefficient";b.termType="numeric";
  GeneralizedGLMRow parent;parent.term="group";parent.sourceTerm="group";parent.rowType="factor_parent";parent.termType="factor";
  GeneralizedGLMRow ref;ref.term="groupControl";ref.sourceTerm="group";ref.displayLabel="Control (reference)";ref.rowType="reference";
  s.rows={a,b,parent,ref};GlobalTermTestRow gt;gt.term="group";gt.method=mi?"Rubin Wald chi-square":"LR chi-square";gt.statistic=1.586;gt.pValue=.663;gt.df=1;s.termTests={gt};
  if(dist==CountDistribution::BetaBinomial){s.familyDiagnostics["beta_binomial_rho"]=.053;s.familyDiagnostics["beta_binomial_variance_inflation"]=2.22;}
  if(dist==CountDistribution::NegativeBinomial){s.familyDiagnostics["negative_binomial_mean_fitted_count"]=4.1;s.familyDiagnostics["negative_binomial_variance_inflation"]=2.18;}
  if(dist==CountDistribution::Poisson)s.familyDiagnostics["pearson_dispersion_ratio"]=2.31;
  g_generalizedGLMs[s.id]=s;
  auto controller=[[GeneralizedGLMWindowController alloc]initWithModelId:s.id];
  auto view=[[GeneralizedGLMReportView alloc]initWithFrame:NSMakeRect(0,0,1050,380)];view.glmController=controller;
  auto window=[[NSWindow alloc]initWithContentRect:view.frame styleMask:NSWindowStyleMaskTitled backing:NSBackingStoreBuffered defer:NO];window.contentView=view;
  const auto layout=GeneralizedGLMReportLayout::Compute([controller coefficientRowCount],1050,[controller coefficientColumnCount],0,0,false,true,false);
  auto check=[&](NSPoint p,NSString *expected){
   auto e=[NSEvent mouseEventWithType:NSEventTypeRightMouseDown location:[view convertPoint:p toView:nil] modifierFlags:0 timestamp:0 windowNumber:window.windowNumber context:nil eventNumber:0 clickCount:1 pressure:1];
   auto menu=[view menuForEvent:e];auto item=[menu itemWithTitle:@"Explain statistic"];assert(item);assert([item.representedObject isEqualToString:expected]);++checks;
  };
  double cw=(layout.width-2*layout.margin)/4;
  for(int col=0;col<4;++col)for(double y:{40.,68.}) {
   NSPoint p=NSMakePoint(layout.margin+col*cw+8,y);auto key=[view fitStatisticAtPoint:p];assert(key);check(p,key);
   auto help=ExplainModelStatistic(GeneralizedModelStatisticContext(s,key.UTF8String));assert(help.find("This statistic is") == std::string::npos);
  }
  auto summary=CountModelParameterSummary(s);auto key=summary.substr(0,summary.find(':'));check(NSMakePoint(30,118),ToNSString(key));
  if(mi){assert([[controller coefficientValueForRow:2 column:4] isEqualToString:@"1.586†"]);assert([[controller coefficientValueForRow:2 column:5] isEqualToString:@".663"]);}
  if(!GeneralizedFamilyDiagnosticRows(s).empty())check(NSMakePoint(30,138),ToNSString(GeneralizedFamilyDiagnosticRows(s)[0].statistic));
  auto help=ExplainModelStatistic(GeneralizedModelStatisticContext(s,key));
  if(dist==CountDistribution::NegativeBinomial){assert(key=="Theta");assert(help.find("mu^2 / theta")!=std::string::npos);if(mi)assert(help.find("not a coefficient pooled")!=std::string::npos);}
  for(int col=0;col<[controller coefficientColumnCount];++col){
   auto key=[controller coefficientColumnIdentifier:col];double x=layout.margin+layout.coefX[col]+8;
   check(NSMakePoint(x,layout.headerY+7),key);
   for(int row=0;row<4;++row)check(NSMakePoint(x,layout.coefRowsY+row*layout.rowHeight+7),key);
  }
  assert(GeneralizedModelStatisticContext(s,"stat",2).statistic==
         (mi ? "Pooled Wald χ²" : "Global term test"));
  assert(GeneralizedModelStatisticContext(s,"p",2).statistic=="Global term p");
  assert(ExplainModelStatistic(GeneralizedModelStatisticContext(s,"term",3)).find("fixed to zero")!=std::string::npos);
  if(dist==CountDistribution::BetaBinomial)assert(GeneralizedModelStatisticContext(s,"effect_ci").statistic.find("Odds ratio")!=std::string::npos);
  if(dist==CountDistribution::NegativeBinomial&&mi){
   auto image=[view bitmapImageRepForCachingDisplayInRect:view.bounds];[view cacheDisplayInRect:view.bounds toBitmapImageRep:image];
   [[image representationUsingType:NSBitmapImageFileTypePNG properties:@{}] writeToFile:@"/tmp/linkeda-theta-help.png" atomically:YES];
  }
  [window orderOut:nil];
 }
 std::cout<<checks<<" native menu checks passed\n";
}}
