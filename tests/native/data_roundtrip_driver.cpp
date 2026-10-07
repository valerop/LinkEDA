// Exercise the real R/native dataset protocol without launching the GUI.
#include "dataset_protocol.h"
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

int main(int argc, char** argv) {
    if (argc != 3) return 2;
    std::ifstream input(argv[1]);
    if (!input) return 2;
    std::vector<std::string> lines;
    std::string line;
    while (std::getline(input, line)) lines.push_back(line);
    rlispstat::core::DataFrameModel data;
    std::size_t cursor = 0;
    bool parsed = false;
    std::string error;
    if (!rlispstat::core::ParseDataFramePayload(lines, cursor, "roundtrip", data,
                                               &parsed, error) || !parsed) {
        std::cerr << error;
        return 1;
    }
    std::ofstream output(argv[2]);
    rlispstat::core::WriteDataFramePayloadForR(output, data, true);
    return output.good() ? 0 : 1;
}
