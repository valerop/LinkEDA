#include "core/mean_comparison_model.h"
#include <cassert>
#include <cmath>
#include <fstream>
#include <iostream>
using namespace rlispstat::core;
int main(int argc, char **argv) {
    assert(argc == 2);
    for (const std::string name : {"hedges", "epsilon"}) {
        const std::string stem = std::string(argv[1]) + "/" + name;
        std::ifstream input(stem + ".payload"); assert(input);
        std::vector<std::string> payload; std::string line;
        while (std::getline(input,line)) payload.push_back(line);
        MeanComparisonState state; std::string error;
        assert(ParseMeanComparisonBatch(payload,state,&error));
        std::ifstream expected(stem + ".expected"); double p, effect;
        expected >> p >> effect; assert(expected);
        bool sawP=false, sawEffect=false;
        for (const auto &table : state.tables) {
            for (const auto &row : table.rows) {
                if (row.kind != MeanComparisonRowKind::Result) continue;
                for (std::size_t i=0; i<table.columns.size(); ++i) {
                    const auto &column=table.columns[i]; const auto &cell=row.cells[i];
                    if (column.key == "p") {
                        assert(cell.numericValue && std::fabs(*cell.numericValue-p)<1e-12); sawP=true;
                    }
                    if (column.key == "effect") {
                        assert(cell.numericValue && std::fabs(*cell.numericValue-effect)<1e-12);
                        assert(column.title == (name=="hedges" ? "g" : "\u03b5\u00b2")); sawEffect=true;
                    }
                }
            }
        }
        assert(sawP && sawEffect);
        assert(!state.provenance.verificationRCode.empty());
    }
    std::cout << "R effect estimates, p-values, labels and verification provenance survive native transport.\n";
}
