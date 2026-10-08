#include "../../src/core/dataset_protocol.h"
#include "../../src/core/linkeda_document.h"
#include <algorithm>
#include <cassert>
#include <fstream>
using namespace rlispstat::core;
int main(int argc,char** argv) {
    if(argc==3 && std::string(argv[1])=="--scripts") {
        std::ofstream(std::string(argv[2])+"-impute.R") << NativeMiceImputationRScript();
        std::ofstream(std::string(argv[2])+"-import.R") << NativeImportRScript();
        return 0;
    }
    assert(argc==3);
    std::ifstream input(argv[1]); std::vector<std::string> lines;
    for(std::string line;std::getline(input,line);)lines.push_back(line);
    assert(lines.size()>2 && lines[0]=="REGISTER_DATASET");
    auto start=std::find(lines.begin(),lines.end(),"DATAFRAME");assert(start!=lines.end());
    std::size_t cursor=start-lines.begin();DataFrameModel data;bool parsed=false;std::string error;
    assert(ParseDataFramePayload(lines,cursor,lines[1],data,&parsed,error));
    assert(parsed && !data.imputationProcess.empty());
    ApplicationState state;assert(state.registerDataset(data));
    LinkEDADataDocument doc,restored;
    assert(CreateLinkEDADataDocument(state,data.group,doc,&error));
    auto bytes=EncodeLinkEDADataDocument(doc,&error);assert(!bytes.empty());
    assert(DecodeLinkEDADataDocument(bytes,restored,&error));
    assert(restored.dataset.imputationProcess==data.imputationProcess);
    std::ofstream output(argv[2]);WriteDataFramePayloadForR(output,restored.dataset,true);
    // Existing documents remain readable and explicitly lack process metadata.
    doc.version=7;
    assert(DecodeLinkEDADataDocument(EncodeLinkEDADataDocument(doc,&error),restored,&error));
    assert(restored.dataset.imputationProcess.empty());
}
