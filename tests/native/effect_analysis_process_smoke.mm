#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <iostream>
#include <cassert>
int main(){@autoreleasepool{
 [NSApplication sharedApplication];
 DataFrameModel df;df.group="effects";df.rows=40;
 DataColumn x,g,y; x.name="x";x.type="numeric";g.name="g";g.type="factor";y.name="y";y.type="numeric";
 for(int i=0;i<40;++i){x.values.push_back(std::to_string(i));g.values.push_back(i%4<2?"A":"B");y.values.push_back(std::to_string(5+i*.2+(i%2?1:-1)));}
 df.columns={x,g,y}; df.datasetType="multiple_imputation";df.imputationCount=3;df.imputationId="mi-effects";for(auto &col:df.columns){col.imputationOriginalValues=col.values; col.imputationValues={col.values,col.values,col.values};col.imputedMissing.assign(40,false);}df.columns[0].imputationOriginalValues[0]="NA";df.columns[0].imputedMissing[0]=true;std::string body,error;// A large opaque mids process must pass through without URLdecode's
 // quadratic byte concatenation before every effect request.
 df.imputationProcess.assign(4*1024*1024, 'A');
 bool ok=RunRBackedMIInteractionReport(df,"glm_interaction","effects","y",{"x","g"},"all","","g",&body,&error);
 assert(ok && !body.empty());
 std::cout<<"LINEAR REPORT "<<ok<<" ERROR "<<error<<std::endl;
 rlispstat::core::RegressionInteractionPlotResult result;
 ok=RunRBackedInteractionPlot(df,"glm_interaction_plot","effects","y",{"x","g"},"all","","g",&result,&error);
 assert(ok && !result.lines.empty());
 std::cout<<"LINEAR PLOT "<<ok<<" ERROR "<<error<<" LINES "<<result.lines.size()<<std::endl;
 for(auto type: {rlispstat::core::StatisticalModelType::PositiveContinuous,rlispstat::core::StatisticalModelType::Count}) {
  GeneralizedGLMState state; state.group=df.group; state.id="test"; state.response="y";state.terms={"x","g"};state.termTypes={{"x","numeric"},{"g","factor"}};state.modelType=type;
  state.family=type==rlispstat::core::StatisticalModelType::Count?"poisson":"Gamma";state.link="log";
  state.countRegression=type==rlispstat::core::StatisticalModelType::Count;state.countDistribution=rlispstat::core::CountDistribution::Poisson;
  const auto spec=EncodeStandaloneGeneralizedMISpec(GeneralizedGLMTaskForState(state,1));
  if(state.countRegression)for(auto &v:df.columns[2].values)v=std::to_string((int)std::stod(v));
  ok=RunRBackedMIInteractionReport(df,"gglm_interaction","effects","y",{"x","g"},"all",spec,"g",&body,&error);
  assert(ok && !body.empty());
  std::cout<<state.family<<" REPORT "<<ok<<" ERROR "<<error<<std::endl;
  ok=RunRBackedInteractionPlot(df,"gglm_interaction_plot","effects","y",{"x","g"},"all",spec,"g",&result,&error);
  assert(ok && !result.lines.empty());
  std::cout<<state.family<<" PLOT "<<ok<<" ERROR "<<error<<std::endl;
 }
 return ok?0:1;
}}
