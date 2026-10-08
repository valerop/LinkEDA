#include "core/mean_comparison_model.h"
#include <cassert>
#include <cmath>
#include <fstream>
#include <iostream>
using namespace rlispstat::core;
int main(int argc, char **argv) {
    assert(argc == 2);
    std::ifstream input(argv[1]); assert(input);
    std::vector<std::string> payload; std::string line;
    while (std::getline(input,line)) payload.push_back(line);
    MeanComparisonState state; std::string error;
    assert(ParseMeanComparisonBatch(payload,state,&error));
    bool found = false;
    for (const auto &table : state.tables) {
        for (const auto &row : table.rows) {
            if (row.kind != MeanComparisonRowKind::Result) continue;
            for (std::size_t i=0; i<table.columns.size(); ++i) {
                if (table.columns[i].key != "effect") continue;
                const auto &cell = row.cells[i];
                assert(cell.numericValue && std::isinf(*cell.numericValue));
                assert(*cell.numericValue > 0 && !cell.notApplicable);
                found = true;
            }
        }
    }
    assert(found);
    assert(MeanComparisonAllTablesCSV(state).find("Inf") != std::string::npos);
    std::cout << "R matched odds infinity survives table parsing and CSV export.\n";
}
