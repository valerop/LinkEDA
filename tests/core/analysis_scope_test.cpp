#include "../../src/core/analysis_scope.h"
#include "../../src/core/application_state.h"

#include <cassert>
#include <set>
#include <string>
#include <vector>

using namespace rlispstat::core;

int main()
{
    AnalysisScope all = AllObservationsAnalysisScope("cars", 5);
    assert(all.kind == AnalysisScopeKind::AllObservations);
    assert((ResolveAnalysisScopeRowIds(all, 5) == std::vector<int>{1, 2, 3, 4, 5}));
    assert(AnalysisScopeSummary(all, 5) == "Analysis scope: All observations · N = 5");

    AnalysisScope explicitRows = ExplicitAnalysisScope(
        "cars", {4, 2, 4, -1, 1}, AnalysisScopeSourceKind::CurrentSelection,
        "Selection snapshot", 5, std::string("plot-1"));
    assert((explicitRows.originalRowIds == std::vector<int>{4, 2, 1}));
    assert(AnalysisScopeContainsRow(explicitRows, 2, 5));
    assert(!AnalysisScopeContainsRow(explicitRows, 3, 5));
    assert(AnalysisScopeRowCount(explicitRows, 5) == 3);
    assert(AnalysisScopeSourceKindFromId("histogram_bin") == AnalysisScopeSourceKind::HistogramBin);
    assert(AnalysisScopeSourceRequiresSelectionName(AnalysisScopeSourceKind::CurrentSelection));
    assert(AnalysisScopeSourceRequiresSelectionName(AnalysisScopeSourceKind::DataTableRows));
    assert(!AnalysisScopeSourceRequiresSelectionName(AnalysisScopeSourceKind::TrellisPanel));
    assert(NormalizeAnalysisScopeSelectionName("  High mileage cars  ") ==
           "High mileage cars");
    assert(NamedSelectionAnalysisScopeDescription("  High mileage cars  ") ==
           "Selection: High mileage cars");
    std::string error;
    assert(ValidateAnalysisScopeSelectionName("High mileage cars", &error));
    assert(!ValidateAnalysisScopeSelectionName("   ", &error));
    assert(!ValidateAnalysisScopeSelectionName("bad|name", &error));

    AnalysisScope emptyExplicit = ExplicitAnalysisScope(
        "cars", {}, AnalysisScopeSourceKind::OtherExplicitSubset,
        "Empty explicit subset", 5);
    assert(emptyExplicit.kind == AnalysisScopeKind::ExplicitRowIds);
    assert(ResolveAnalysisScopeRowIds(emptyExplicit, 5).empty());
    assert(AnalysisScopeRowCount(emptyExplicit, 5) == 0);
    assert(ValidateAnalysisScope(emptyExplicit, "cars", 5, &error));

    AnalysisScope missingDataset = emptyExplicit;
    missingDataset.datasetId.clear();
    assert(!ValidateAnalysisScope(missingDataset, "cars", 5, &error));

    assert(ValidateAnalysisScope(explicitRows, "cars", 5, &error));
    assert(!ValidateAnalysisScope(explicitRows, "other", 5, &error));

    AnalysisScope reconciled = ReconcileAnalysisScope(explicitRows, 2);
    assert((reconciled.originalRowIds == std::vector<int>{2, 1}));
    assert(reconciled.invalidatedRowCount == 1);

    ApplicationState state;
    DataFrameModel cars;
    cars.group = "cars";
    cars.rows = 5;
    assert(state.registerDataset(cars));
    assert(state.activeAnalysisScope("cars").kind == AnalysisScopeKind::AllObservations);

    assert(state.setSelectedRows("cars", std::set<int>{2, 4}));
    AnalysisScopeChangeEvent change;
    assert(state.setActiveAnalysisScopeFromSelection(
        "cars", AnalysisScopeSourceKind::CurrentSelection,
        NamedSelectionAnalysisScopeDescription("High mileage cars"),
        std::string("data-sheet"), &change, &error));
    assert(change.version == 1);
    assert((state.resolveActiveAnalysisRowIds("cars") == std::vector<int>{2, 4}));
    assert(AnalysisScopeWindowSummary(state.activeAnalysisScope("cars"), 5) ==
           "Scope: Selection: High mileage cars · N = 2");
    assert(state.savedSelections("cars").size() == 1);
    assert(state.savedSelections("cars")[0].name == "High mileage cars");
    assert((state.savedSelections("cars")[0].originalRowIds == std::vector<int>{2, 4}));

    // Selection and scope are intentionally independent: changing the global
    // selection cannot mutate an already captured analysis snapshot.
    assert(state.setSelectedRows("cars", std::set<int>{1, 3, 5}));
    assert((state.resolveActiveAnalysisRowIds("cars") == std::vector<int>{2, 4}));
    assert(state.activeAnalysisScope("cars").sourceDescription ==
           "Selection: High mileage cars");

    assert(state.setActiveAnalysisScopeFromSelection(
        "cars", AnalysisScopeSourceKind::CurrentSelection,
        NamedSelectionAnalysisScopeDescription("Odd cars"),
        std::string("data-sheet"), &change, &error));
    assert(state.savedSelections("cars").size() == 2);
    assert(state.activateSavedSelection("cars", "High mileage cars", &change, &error));
    assert((state.resolveActiveAnalysisRowIds("cars") == std::vector<int>{2, 4}));
    assert(state.addSavedSelectionToActiveScope(
        "cars", "Odd cars", "High or odd cars", &change, &error));
    assert((state.resolveActiveAnalysisRowIds("cars") == std::vector<int>{2, 4, 1, 3, 5}));
    assert(state.savedSelections("cars").size() == 3);
    assert(state.savedSelection("cars", "High or odd cars").has_value());
    assert(state.activeAnalysisScope("cars").sourceDescription ==
           "Selection: High or odd cars");
    assert(state.setSelectedRows("cars", {}));
    assert((state.resolveActiveAnalysisRowIds("cars") == std::vector<int>{2, 4, 1, 3, 5}));

    // Re-registering the dataset does not expand an explicit snapshot with new rows.
    cars.rows = 7;
    assert(state.registerDataset(cars));
    assert((state.resolveActiveAnalysisRowIds("cars") == std::vector<int>{2, 4, 1, 3, 5}));
    assert(state.analysisScopeVersion("cars") == 5);

    assert(state.resetActiveAnalysisScopeToAllObservations("cars", &change, &error));
    assert((state.resolveActiveAnalysisRowIds("cars") ==
            std::vector<int>{1, 2, 3, 4, 5, 6, 7}));
    assert(state.analysisScopeVersion("cars") == 6);

    DataFrameModel trucks;
    trucks.group = "trucks";
    trucks.rows = 3;
    assert(state.registerDataset(trucks));
    assert(state.activeAnalysisScope("trucks").kind == AnalysisScopeKind::AllObservations);
    assert((state.resolveActiveAnalysisRowIds("cars") ==
            std::vector<int>{1, 2, 3, 4, 5, 6, 7}));

    AnalysisScope invalid = ExplicitAnalysisScope(
        "cars", {8}, AnalysisScopeSourceKind::OtherExplicitSubset,
        "Invalid", 7);
    assert(!state.setActiveAnalysisScope(invalid, nullptr, &error));
    assert(!error.empty());

    return 0;
}
