#include "command_dispatcher.h"
#include <cassert>
#include <fstream>
#include <iostream>
using namespace rlispstat::core;
static std::vector<std::string> read(const std::string &path) {
    std::ifstream in(path); assert(in); std::vector<std::string> lines; std::string line;
    while(std::getline(in,line)) lines.push_back(line); return lines;
}
int main(int argc,char **argv) {
    assert(argc==2);std::string folder=argv[1];
    auto fields=read(folder+"/provenance.payload");size_t cursor=0;std::string error;
    AnalysisProvenance p;
    assert(ReadAnalysisProvenancePayload(fields,cursor,p,&error));assert(cursor==fields.size());
    assert(p.scopeRecordedInR && p.dataVersion.version==1);
    assert(p.scope.scopeN==40 && p.scope.sourceN==80 && p.scope.effectiveN==40);
    assert(p.scope.requestedStableRowIds.front()=="gc_scope:row:11");
    auto bad=fields;bad.back()="gc_scope:row:80"; // final excluded count must be numeric
    cursor=0;AnalysisProvenance invalid;assert(!ReadAnalysisProvenancePayload(bad,cursor,invalid,&error));
    CommandDispatcher d(CommandDispatcherServices{});
    auto reply=d.dispatch(read(folder+"/dataset.payload"));
    if(reply.rfind("OK",0)!=0)std::cerr<<reply<<'\n';assert(reply.rfind("OK",0)==0);
    reply=d.dispatch(read(folder+"/comparison.payload"));
    if(reply.rfind("OK",0)!=0)std::cerr<<reply<<'\n';assert(reply.rfind("OK",0)==0);
    const auto *out=d.applicationState().outputCodeReference("gc-native");assert(out);
    assert(out->provenance.scopeRecordedInR && out->provenance.dataVersion.version==1);
    assert(out->provenance.scope.scopeN==40 && out->provenance.scope.sourceN==80);
    assert(out->provenance.preparedDataPath==folder+"/prepared.rds");
    std::string code=BuildAnalysisVerificationRCode(out->provenance,"comparison"),bound;
    assert(code.find("not yet available")==std::string::npos);
    assert(code.find("LinkEDA:::")==std::string::npos);
    assert(BindAnalysisVerificationDataPath(code,out->provenance.preparedDataPath,bound));
    std::ofstream(folder+"/native-verification.R")<<bound;
    std::cout<<"Comparison version, frozen scope, recipe and prepared-data transport passed.\n";
}
