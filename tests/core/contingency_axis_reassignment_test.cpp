#include "table1_model.h"
#include <cassert>
#include <string>
#include <vector>

using rlispstat::core::MoveContingencyRowToColumn;

int main()
{
    std::vector<std::string> rows{"country"};
    std::string column{"group"};
    assert(MoveContingencyRowToColumn(rows, column, "country"));
    assert(rows == std::vector<std::string>{"group"} && column == "country");
    assert(MoveContingencyRowToColumn(rows, column, "group"));
    assert(rows == std::vector<std::string>{"country"} && column == "group");

    rows = {"country", "gender"}; column.clear();
    assert(MoveContingencyRowToColumn(rows, column, "gender"));
    assert(rows == std::vector<std::string>{"country"} && column == "gender");
    assert(!MoveContingencyRowToColumn(rows, column, "missing"));
    rows = {"country"}; column.clear();
    assert(!MoveContingencyRowToColumn(rows, column, "country"));
    assert(rows == std::vector<std::string>{"country"} && column.empty());
}
