#include "../../src/core/selection_model.h"

#include <cassert>
#include <set>

using rlispstat::core::CaseId;
using rlispstat::core::CaseSetFromGroupedVectors;
using rlispstat::core::CaseSetFromVector;
using rlispstat::core::CaseSetText;
using rlispstat::core::EffectiveSelectionModeName;
using rlispstat::core::GroupInfoResponseText;
using rlispstat::core::GroupSelectionSummary;
using rlispstat::core::GroupSelectionsResponseText;
using rlispstat::core::InvertSelectionWithin;
using rlispstat::core::RowsForScope;
using rlispstat::core::SelectionMode;
using rlispstat::core::SelectionModeFromString;
using rlispstat::core::SelectionModel;

static std::set<CaseId> S(std::initializer_list<CaseId> values)
{
    return std::set<CaseId>(values.begin(), values.end());
}

int main()
{
    SelectionModel selection;
    assert(selection.empty());
    assert(selection.version() == 0);

    assert(selection.setSelectedCases(S({1, 2, 0, -1})));
    assert(selection.selectedCases() == S({1, 2}));
    assert(selection.version() == 1);

    assert(!selection.setSelectedCases(S({2, 1})));
    assert(selection.version() == 1);

    assert(selection.mergeSelection(S({3, 4}), SelectionMode::Add));
    assert(selection.selectedCases() == S({1, 2, 3, 4}));
    assert(selection.version() == 2);

    assert(selection.mergeSelection(S({2, 4}), SelectionMode::Subtract));
    assert(selection.selectedCases() == S({1, 3}));
    assert(selection.version() == 3);

    assert(selection.mergeSelection(S({3, 5}), SelectionMode::Toggle));
    assert(selection.selectedCases() == S({1, 5}));
    assert(selection.version() == 4);

    assert(selection.mergeSelection(S({7}), SelectionModeFromString("replace")));
    assert(selection.selectedCases() == S({7}));
    assert(selection.version() == 5);

    assert(selection.invertWithin(S({6, 7, 8})));
    assert(selection.selectedCases() == S({6, 8}));
    assert(selection.version() == 6);

    assert(!selection.mergeSelection(S({6, 8}), SelectionMode::Add));
    assert(selection.version() == 6);

    assert(selection.clear());
    assert(selection.empty());
    assert(selection.version() == 7);

    assert(InvertSelectionWithin(S({2}), S({1, 2, 3})) == S({1, 3}));
    assert(CaseSetFromVector({3, 1, 0, 3, -2}) == S({1, 3}));
    assert(CaseSetFromGroupedVectors({{1, 2}, {0, 2, 4}, {-1}}) == S({1, 2, 4}));
    assert(CaseSetText(S({3, 1, 0, -2})) == "1 3");
    assert(CaseSetText(S({3, 1}), ",") == "1,3");
    assert(CaseSetText(S({})) == "");
    assert(GroupSelectionsResponseText({
        GroupSelectionSummary{"cars", 2, 5},
        GroupSelectionSummary{"iris", 1, 0}
    }) == "OK\tcars|2|5\tiris|1|0");
    assert(GroupInfoResponseText("cars", 3, {"plot-1", "plot-2"}) ==
           "OK\tcars|3|plot-1|plot-2");
    assert(GroupInfoResponseText("empty", 0, {}) == "OK\tempty|0");
    assert(SelectionModeFromString("add") == SelectionMode::Add);
    assert(SelectionModeFromString("subtract") == SelectionMode::Subtract);
    assert(SelectionModeFromString("toggle") == SelectionMode::Toggle);
    assert(SelectionModeFromString("not-a-mode") == SelectionMode::Replace);
    assert(EffectiveSelectionModeName(true, true, true, true, "replace") == "subtract");
    assert(EffectiveSelectionModeName(false, true, false, true, "replace") == "toggle");
    assert(EffectiveSelectionModeName(false, false, true, true, "replace") == "toggle");
    assert(EffectiveSelectionModeName(false, false, false, true, "replace") == "add");
    assert(EffectiveSelectionModeName(false, false, false, false, "replace") == "replace");
    assert(EffectiveSelectionModeName(false, false, false, false, "brush") == "brush");
    assert(RowsForScope(5, S({2, 4}), "all").empty());
    assert((RowsForScope(5, S({2, 4}), "selected") == std::vector<int>{2, 4}));
    assert((RowsForScope(5, S({2, 4}), "unselected") == std::vector<int>{1, 3, 5}));
    assert((RowsForScope(3, S({0, -1, 2}), "selected") == std::vector<int>{2}));
    assert(RowsForScope(3, S({1}), "bad").empty());
    assert(RowsForScope(0, S({1}), "selected").empty());

    return 0;
}
