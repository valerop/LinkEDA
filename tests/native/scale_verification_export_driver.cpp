#include "command_dispatcher.h"
#include <cassert>
#include <fstream>
#include <iostream>
using namespace rlispstat::core;
// Feed the actual R result through the native parser and export its public recipe.
int main(int argc, char **argv) {
    assert(argc == 4);
    std::ifstream input(argv[1]);
    std::vector<std::string> payload; std::string line;
    while (std::getline(input, line)) payload.push_back(line);
    assert(!payload.empty() && payload[0] == "SCALE_ANALYSIS_OPEN");
    assert(payload.size() >= 9);
    const auto itemCount = std::stoul(payload[8]);
    assert(payload.size() >= 9 + 11 * itemCount);
    DataFrameModel data; data.group = payload[2];
    data.rows = std::stoi(argv[3]); data.datasetType = "multiple_imputation";
    data.imputationCount = std::stoi(payload[4]);
    for (std::size_t i=0; i<itemCount; ++i) {
        DataColumn column; column.name=payload[9+11*i]; column.type="numeric";
        column.values.resize(data.rows, "NA"); data.columns.push_back(column);
    }
    CommandDispatcherServices services;
    services.ui.showScaleAnalysis=[](const std::string &) {};
    CommandDispatcher dispatcher(services);
    assert(dispatcher.applicationState().registerDataset(data));
    auto reply=dispatcher.dispatch(payload);
    if (reply.rfind("OK",0)!=0) std::cerr<<reply<<'\n';
    assert(reply.rfind("OK",0)==0);
    const auto *output=dispatcher.applicationState().outputCodeReference(payload[1]);
    assert(output && output->provenance.scope.scopeN==static_cast<std::size_t>(data.rows));
    const auto &code=output->provenance.verificationRCode.at("table");
    assert(code.find("LinkEDA:::")==std::string::npos);
    assert(code.find(".999999")==std::string::npos);
    assert(code.find("mice::pool.scalar")!=std::string::npos);
    std::ofstream(argv[2]) << code;
}
