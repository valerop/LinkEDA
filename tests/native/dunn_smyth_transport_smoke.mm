#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
#include <fstream>
#include <iostream>
using namespace rlispstat::core;
static std::string readAll(const std::string &p){std::ifstream f(p);return std::string(std::istreambuf_iterator<char>(f),{});}
int main(){@autoreleasepool{
 std::ifstream f("/tmp/linkeda-nb-mi-payload.txt");std::vector<std::string> lines;std::string l;while(std::getline(f,l))lines.push_back(l);
 GeneralizedGLMState s;std::size_t cursor=0;assert(ReadGeneralizedStatePayload(lines,cursor,s));assert(s.ok);assert(s.diagnosticsByImputation.size()==2);
 s.id="nb-native-validation";s.group="nb-validation";
 s.provenance.verificationRCode["model"]="completed <- readRDS('/tmp/dunn-completed.rds')\nreference_fits <- lapply(completed, function(d) MASS::glm.nb(y~x,data=d))\n";
 s.provenance.verificationRCode["diagnostic_randomization"]=readAll("/tmp/dunn-seed-recipe.R");
 assert(!s.provenance.verificationRCode["diagnostic_randomization"].empty());
 int checks=0;
 for(int imp:{1,2}){
  PlotModel p;p.id="dunn-native";p.group=s.group;p.glmModelId=s.id;p.isGLMDiagnostic=true;p.displayedResidualType="dunn_smyth";p.diagnosticImputationIndex=imp;
  const auto &d=s.diagnosticsByImputation[imp-1];
  for(auto kind:{"residuals_vs_fitted","normal_qq","residual_histogram"}){
   p.glmDiagnosticKind=kind;std::string error;assert(PopulateGeneralizedDiagnosticPlot(&p,s,&error));
   assert(p.diagnosticImputationIndex==imp);assert(p.title.find("imputation "+std::to_string(imp))!=std::string::npos);
   if(p.kind=="histogram")for(const auto &v:p.histogramPoints){auto it=std::find_if(d.begin(),d.end(),[&](const auto&r){return r.row==v.row;});assert(it!=d.end());assert(std::abs(it->dunnSmythResidual-v.x)<1e-12);++checks;}
   else for(const auto &v:p.points){auto it=std::find_if(d.begin(),d.end(),[&](const auto&r){return r.row==v.row;});assert(it!=d.end());assert(std::abs(it->dunnSmythResidual-v.y)<1e-12);++checks;}
  }
  p.glmDiagnosticKind="residuals_vs_fitted";assert(PopulateGeneralizedDiagnosticPlot(&p,s));
  auto output=GeneralizedDiagnosticPlotCodeReference(p,s);
  auto code=output.provenance.verificationRCode.at(output.outputBlockId);
  assert(code.find("statmod::qresiduals")!=std::string::npos);assert(code.find("48271")==std::string::npos);
  std::ofstream("/tmp/dunn-native-export-"+std::to_string(imp)+".R")<<code;
 }
 assert(s.diagnosticsByImputation[0][0].fitted!=s.diagnosticsByImputation[1][0].fitted);
 std::cout<<checks<<" native residual/row checks passed; two R exports created\n";
}}
