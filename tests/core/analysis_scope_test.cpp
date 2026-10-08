#include "../../src/core/analysis_scope.h"
#include "../../src/core/application_state.h"
#include "../../src/core/command_dispatcher.h"

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
    assert(AnalysisScopeCompactSummary(all,5)=="All · N = 5");
    assert(FrozenAnalysisScopeNotice(all, all).empty());
    const AnalysisScope plotWithoutTwo = AnalysisScopeExcludingRows(
        all, {2, 8}, 5, "plot-1");
    assert(plotWithoutTwo.sourceKind == AnalysisScopeSourceKind::PlotExclusion);
    assert(plotWithoutTwo.sourceViewId == std::optional<std::string>("plot-1"));
    assert((ResolveAnalysisScopeRowIds(plotWithoutTwo, 5) ==
            std::vector<int>{1, 3, 4, 5}));
    const AnalysisScope plotWithoutTwoAndFour = AnalysisScopeExcludingRows(
        plotWithoutTwo, {4}, 5, "plot-1");
    assert((ResolveAnalysisScopeRowIds(plotWithoutTwoAndFour, 5) ==
            std::vector<int>{1, 3, 5}));
    assert((ResolveAnalysisScopeRowIds(all, 5) ==
            std::vector<int>{1, 2, 3, 4, 5}));
    assert(AnalysisScopeSourceKindFromId(
        AnalysisScopeSourceKindId(plotWithoutTwo.sourceKind)) ==
        AnalysisScopeSourceKind::PlotExclusion);

    AnalysisScope explicitRows = ExplicitAnalysisScope(
        "cars", {4, 2, 4, -1, 1}, AnalysisScopeSourceKind::CurrentSelection,
        "Selection snapshot", 5, std::string("plot-1"));
    assert((explicitRows.originalRowIds == std::vector<int>{4, 2, 1}));
    assert(AnalysisScopeContainsRow(explicitRows, 2, 5));
    assert(!AnalysisScopeContainsRow(explicitRows, 3, 5));
    assert(AnalysisScopeRowCount(explicitRows, 5) == 3);
    assert(AnalysisScopeTracksCurrentSelection(explicitRows));
    const std::string frozenNotice = FrozenAnalysisScopeNotice(explicitRows, all);
    assert(frozenNotice.find("Results shown use") != std::string::npos);
    assert(frozenNotice.find("current global scope is All · N = 5") != std::string::npos);
    AnalysisScope sameSizeDifferentRows = explicitRows;
    sameSizeDifferentRows.originalRowIds = {5, 2, 1};
    assert(!FrozenAnalysisScopeNotice(explicitRows, sameSizeDifferentRows).empty());
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
           "Scope when computed: Selection: High mileage cars · N = 2 of 5 observations");
    assert(state.savedSelections("cars").size() == 1);
    assert(state.savedSelections("cars")[0].name == "High mileage cars");
    assert((state.savedSelections("cars")[0].originalRowIds == std::vector<int>{2, 4}));

    // Selection and scope are intentionally independent: changing the global
    // selection cannot mutate an already captured analysis snapshot.
    assert(state.setSelectedRows("cars", std::set<int>{1, 3, 5}));
    assert((state.resolveActiveAnalysisRowIds("cars") == std::vector<int>{2, 4}));
    assert(state.activeAnalysisScope("cars").sourceDescription ==
           "Selection: High mileage cars");
    assert(!AnalysisScopeTracksCurrentSelection(state.activeAnalysisScope("cars")));
    assert(AnalysisScopeMatchesRows(
        state.activeAnalysisScope("cars"), std::vector<int>{2, 4}));

    assert(state.setActiveAnalysisScopeFromSelection(
        "cars", AnalysisScopeSourceKind::CurrentSelection,
        NamedSelectionAnalysisScopeDescription("Odd cars"),
        std::string("data-sheet"), &change, &error));
    assert(state.savedSelections("cars").size() == 2);
    assert(state.activateSavedSelection("cars", "High mileage cars", &change, &error));
    assert(AnalysisScopeCompactSummary(state.activeAnalysisScope("cars"),5)=="High mileage cars · N = 2");
    assert((state.resolveActiveAnalysisRowIds("cars") == std::vector<int>{2, 4}));
    assert(state.setSelectedRows("cars", std::set<int>{5}));
    assert((state.resolveActiveAnalysisRowIds("cars") == std::vector<int>{2, 4}));
    assert(AnalysisScopeChoiceValue(
        "selected", state.activeAnalysisScope("cars"), true) ==
        SavedAnalysisScopeChoiceValue("High mileage cars"));
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

    ApplicationState separated;
    DataFrameModel people;
    people.group = "people";
    people.rows = 4;
    assert(separated.registerDataset(people));
    assert(separated.setSelectedRows("people", std::set<int>{1, 4}));
    assert(separated.saveCurrentSelectionAsAnalysisScope(
        "people", "Cases to revisit", AnalysisScopeSourceKind::DataTableRows,
        std::string("data-sheet"), &change, &error));
    assert(separated.activeAnalysisScope("people").kind ==
           AnalysisScopeKind::ExplicitRowIds);
    assert(AnalysisScopeSelectionName(separated.activeAnalysisScope("people")) ==
           std::optional<std::string>("Cases to revisit"));
    assert(!AnalysisScopeTracksCurrentSelection(
        separated.activeAnalysisScope("people")));
    assert(separated.savedSelections("people").size() == 1);
    assert(separated.setActiveAnalysisScopeFromSelection(
        "people", AnalysisScopeSourceKind::CurrentSelection,
        "Current selection", std::string("data-sheet"), &change, &error));
    assert(separated.savedSelections("people").size() == 1);
    assert((separated.resolveActiveAnalysisRowIds("people") ==
            std::vector<int>{1, 4}));

    const SavedSelection choiceSelection{
        "people", "Women aged 18–25", {2, 4},
        AnalysisScopeSourceKind::CurrentSelection, std::nullopt};
    const auto choices = BuildAnalysisScopeChoices(3, {choiceSelection}, true);
    assert(choices.size() == 4);
    assert(choices[1].label == "Current selection (live; 3)");
    assert(choices[3].value == "saved:Women aged 18–25");
    assert(choices[3].label == "Saved scope: Women aged 18–25 (2)");
    assert(SavedAnalysisScopeNameFromChoiceValue(choices[3].value) ==
           choiceSelection.name);
    AnalysisScope resolvedChoice;
    assert(separated.replaceSavedSelections("people", {choiceSelection}, &error));
    assert(separated.resolveSavedAnalysisScopeChoice(
        "people", choices[3].value, resolvedChoice, &error));
    assert(separated.activeAnalysisScope("people").sourceDescription ==
           "Current selection");
    assert((resolvedChoice.originalRowIds == std::vector<int>{2, 4}));
    assert(AnalysisScopeChoiceValue("all", resolvedChoice, true) ==
           choices[3].value);
    assert(AnalysisScopeChoiceValue("unselected", {}, false) == "unselected");
    assert(AnalysisScopeChoiceValue("all", ExplicitAnalysisScope(
        "people", {1}, AnalysisScopeSourceKind::CurrentUnselection,
        "Unselected observations", 4), true) == "unselected");
    assert(AnalysisScopeChoiceValue("all", ExplicitAnalysisScope(
        "people", {1}, AnalysisScopeSourceKind::OtherExplicitSubset,
        "Panel A", 4), true) == "explicit");

    // A replacement may reorder cases. Selections and saved scopes follow
    // stable case identity, not the old numeric position.
    ApplicationState reordered;
    DataFrameModel original;
    original.group = "reordered";
    original.rows = 3;
    original.stableRowIds = {"a", "b", "c"};
    assert(reordered.registerDataset(original));
    assert(reordered.setSelectedRows("reordered", {1, 3}));
    assert(reordered.saveCurrentSelectionAsAnalysisScope(
        "reordered", "Tracked", AnalysisScopeSourceKind::CurrentSelection,
        std::nullopt, &change, &error));
    DataFrameModel replacement = original;
    replacement.stableRowIds = {"c", "a", "d"};
    assert(reordered.registerDataset(replacement));
    std::set<int> selected;
    assert(reordered.selectedRows("reordered", selected));
    assert((selected == std::set<int>{1, 2}));
    assert((reordered.resolveActiveAnalysisRowIds("reordered") ==
            std::vector<int>{2, 1}));
    assert((reordered.savedSelection("reordered", "Tracked")->originalRowIds ==
            std::vector<int>{2, 1}));
    replacement.stableRowIds = {"c", "d", "e"};
    assert(reordered.registerDataset(replacement));
    assert((reordered.resolveActiveAnalysisRowIds("reordered") == std::vector<int>{1}));
    assert(reordered.activeAnalysisScope("reordered").invalidatedRowCount == 1);
    DataFrameModel unidentified = replacement;
    unidentified.stableRowIds.clear();
    assert(reordered.registerDataset(unidentified));
    assert(reordered.resolveActiveAnalysisRowIds("reordered").empty());
    assert(reordered.selectedRows("reordered", selected) && selected.empty());

    CommandDispatcher sheetOnly({});
    DataFrameModel sheet;
    sheet.group = "sheet-only";
    sheet.rows = 3;
    assert(sheetOnly.applicationState().registerDataset(sheet));
    assert(sheetOnly.dispatch({"SET_SELECTED", "sheet-only", "1", "4"}) ==
           "ERR selected row is outside the dataset");
    assert(sheetOnly.dispatch({"SELECT_ALL", "sheet-only"}) == "OK");
    assert(sheetOnly.dispatch({"SELECTED", "sheet-only"}) == "OK 1 2 3");

    // Brushing is live; exclusions are a separate dataset-wide mask. Saving
    // the selection fixes membership, while later exclusions still apply.
    ApplicationState inclusion;
    DataFrameModel subset;
    subset.group = "subset";
    subset.rows = 4;
    subset.stableRowIds = {"a", "b", "c", "d"};
    assert(inclusion.registerDataset(subset));
    assert(inclusion.setSelectedRows("subset", {1, 2}));
    assert(inclusion.setActiveAnalysisScopeFromSelection(
        "subset", AnalysisScopeSourceKind::CurrentSelection,
        "Current selection (live)", std::nullopt, &change, &error));
    assert((inclusion.resolveActiveAnalysisRowIds("subset") ==
        std::vector<int>{1, 2}));
    assert(inclusion.setSelectedRows("subset", {2, 3}));
    assert((inclusion.resolveActiveAnalysisRowIds("subset") ==
        std::vector<int>{2, 3}));
    assert(inclusion.saveCurrentSelectionAsAnalysisScope(
        "subset", "Group A", AnalysisScopeSourceKind::CurrentSelection,
        std::nullopt, &change, &error));
    assert(inclusion.setSelectedRows("subset", {1, 4}));
    assert((inclusion.resolveActiveAnalysisRowIds("subset") ==
        std::vector<int>{2, 3}));
    assert(inclusion.setExcludedRows("subset", {3}, &change, &error));
    assert((inclusion.excludedRows("subset") == std::set<int>{3}));
    assert((inclusion.resolveActiveAnalysisRowIds("subset") ==
        std::vector<int>{2}));
    assert((inclusion.baseAnalysisScope("subset").originalRowIds ==
        std::vector<int>{2, 3}));
    assert((inclusion.savedSelection("subset", "Group A")->originalRowIds ==
        std::vector<int>{2, 3}));
    assert(inclusion.resetActiveAnalysisScopeToAllObservations("subset", &change, &error));
    assert(inclusion.activeAnalysisScope("subset").sourceKind ==
        AnalysisScopeSourceKind::IncludedObservations);
    assert((inclusion.resolveActiveAnalysisRowIds("subset") ==
        std::vector<int>{1, 2, 4}));
    AnalysisScope computedScope;
    bool computedScopeCaptured = false;
    assert(inclusion.captureAnalysisScope("subset", computedScope,
        computedScopeCaptured));
    assert((computedScope.originalRowIds == std::vector<int>{1, 2, 4}));
    assert(inclusion.setExcludedRows("subset", {2, 3}, &change, &error));
    assert((computedScope.originalRowIds == std::vector<int>{1, 2, 4}));
    assert((inclusion.resolveActiveAnalysisRowIds("subset") ==
        std::vector<int>{1, 4}));
    subset.stableRowIds = {"d", "c", "b", "a"};
    assert(inclusion.registerDataset(subset));
    assert((inclusion.excludedRows("subset") == std::set<int>{2, 3}));
    assert((inclusion.resolveActiveAnalysisRowIds("subset") ==
        std::vector<int>{1, 4}));
    assert(inclusion.setExcludedRows("subset", {}, &change, &error));
    assert(inclusion.activeAnalysisScope("subset").kind ==
        AnalysisScopeKind::AllObservations);
    assert((inclusion.savedSelection("subset", "Group A")->originalRowIds ==
        std::vector<int>{3, 2}));

    ApplicationState nested;
    DataFrameModel nestedData;
    nestedData.group = "nested";
    nestedData.rows = 5;
    assert(nested.registerDataset(nestedData));
    assert(nested.setExcludedRows("nested", {5}, &change, &error));
    assert(nested.setSelectedRows("nested", {1, 2, 3}));
    assert(nested.setActiveAnalysisScopeFromSelection(
        "nested", AnalysisScopeSourceKind::CurrentSelection,
        "Current selection (live)", std::nullopt, &change, &error));
    assert(nested.setExcludedRows("nested", {2, 5}, &change, &error));
    std::set<int> remainingSelection;
    assert(nested.selectedRows("nested", remainingSelection));
    assert((remainingSelection == std::set<int>{1, 3}));
    assert(nested.activeAnalysisScope("nested").sourceKind ==
        AnalysisScopeSourceKind::CurrentSelection);
    assert(AnalysisScopeChoiceValue("all", change.currentScope, true) ==
        "selected");
    assert((nested.resolveActiveAnalysisRowIds("nested") ==
        std::vector<int>{1, 3}));
    assert(nested.saveCurrentSelectionAsAnalysisScope(
        "nested", "Saved", AnalysisScopeSourceKind::CurrentSelection,
        std::nullopt, &change, &error));
    assert(nested.setExcludedRows("nested", {1, 2, 5}, &change, &error));
    assert((nested.savedSelection("nested", "Saved")->originalRowIds ==
        std::vector<int>{1, 3}));
    assert(AnalysisScopeChoiceValue("all", change.currentScope, true) ==
        SavedAnalysisScopeChoiceValue("Saved"));
    assert((nested.resolveActiveAnalysisRowIds("nested") ==
        std::vector<int>{3}));
    assert(nested.setExcludedRows("nested", {2, 5}, &change, &error));
    assert((nested.savedSelection("nested", "Saved")->originalRowIds ==
        std::vector<int>{1, 3}));
    assert(AnalysisScopeChoiceValue("all", change.currentScope, true) ==
        SavedAnalysisScopeChoiceValue("Saved"));
    assert((nested.resolveActiveAnalysisRowIds("nested") ==
        std::vector<int>{1, 3}));
    assert(nested.setSelectedRows("nested", {3, 5}));
    assert(nested.saveCurrentSelectionAsAnalysisScope(
        "nested", "Created later", AnalysisScopeSourceKind::CurrentSelection,
        std::nullopt, &change, &error));
    assert((nested.savedSelection("nested", "Created later")->originalRowIds ==
        std::vector<int>{3}));

    CommandDispatcher caseToggle({});
    DataFrameModel toggleData;
    toggleData.group = "toggle";
    toggleData.rows = 4;
    assert(caseToggle.applicationState().registerDataset(toggleData));
    assert(caseToggle.applicationState().setSelectedRows("toggle", {1, 2}));
    assert(caseToggle.applicationState().saveCurrentSelectionAsAnalysisScope(
        "toggle", "Saved", AnalysisScopeSourceKind::CurrentSelection,
        std::nullopt, &change, &error));
    assert(caseToggle.dispatch({"TOGGLE_CASE_INCLUDED", "toggle", "2"}) ==
        "OK 3 of 4 cases included");
    assert((caseToggle.applicationState().excludedRows("toggle") == std::set<int>{2}));
    assert(AnalysisScopeChoiceValue("all",
        caseToggle.applicationState().activeAnalysisScope("toggle"), true) ==
        SavedAnalysisScopeChoiceValue("Saved"));
    assert((caseToggle.applicationState().resolveActiveAnalysisRowIds("toggle") ==
        std::vector<int>{1}));
    assert(caseToggle.dispatch({"TOGGLE_CASE_INCLUDED", "toggle", "2"}) ==
        "OK 4 of 4 cases included");
    assert((caseToggle.applicationState().resolveActiveAnalysisRowIds("toggle") ==
        std::vector<int>{1, 2}));
    assert(caseToggle.dispatch({"TOGGLE_CASE_INCLUDED", "toggle", "bad"}) ==
        "ERR case number is outside the dataset");

    assert(sheetOnly.dispatch({"SET_SELECTED", "sheet-only", "1", "2"}) == "OK");
    assert(sheetOnly.dispatch({"EXCLUDE_SELECTED_CASES", "sheet-only"}).find(
        "OK 2 of 3 cases included") == 0);
    assert(sheetOnly.dispatch({"GET_EXCLUDED_ROWS", "sheet-only"}) == "OK\t1|2");
    assert(sheetOnly.dispatch({"SELECTED", "sheet-only"}) == "OK");
    assert(sheetOnly.dispatch({"SET_SELECTED", "sheet-only", "1", "2"}) == "OK");
    assert(sheetOnly.dispatch({"INCLUDE_SELECTED_CASES", "sheet-only"}).find(
        "OK 3 of 3 cases included") == 0);
    assert(sheetOnly.dispatch({"SET_ANALYSIS_SCOPE_FROM_SELECTION", "sheet-only"}).rfind(
        "OK ", 0) == 0);
    assert(sheetOnly.dispatch({"EXCLUDE_SELECTED_CASES", "sheet-only"}).find(
        "OK 2 of 3 cases included") == 0);
    assert(sheetOnly.applicationState().activeAnalysisScope("sheet-only").sourceKind ==
        AnalysisScopeSourceKind::CurrentSelection);
    assert(sheetOnly.applicationState().resolveActiveAnalysisRowIds("sheet-only").empty());

    // A manually frozen window keeps its computed scope and fit while the
    // global scope changes. Re-enabling automatic fitting captures the new
    // scope and invalidates the old fit identity.
    auto &linear = separated.groupModels()["people"];
    linear.group = "people";
    assert(separated.resetActiveAnalysisScopeToAllObservations("people", &change, &error));
    assert(separated.captureAnalysisScope("people", linear.dataScope,
        linear.dataScopeCaptured, &linear.scope));
    linear.isStale = false;
    linear.lastRFitSignature = "fit-on-all-rows";
    assert(separated.setSelectedRows("people", std::set<int>{2, 4}));
    assert(separated.setActiveAnalysisScope(
        ExplicitAnalysisScope("people", {2, 4},
            AnalysisScopeSourceKind::CurrentSelection, "Current selection", 4),
        &change, &error));
    linear.autoRefit = false;
    linear.frozenScopeNotice = FrozenAnalysisScopeNotice(
        linear.dataScope, separated.activeAnalysisScope("people"));
    assert(!linear.frozenScopeNotice.empty());
    assert(linear.scope == "all" && linear.lastRFitSignature == "fit-on-all-rows");
    assert(!linear.isStale && linear.dataScope.originalRowIds.empty());
    linear.autoRefit = true;
    assert(separated.captureAnalysisScope("people", linear.dataScope,
        linear.dataScopeCaptured, &linear.scope));
    MarkGroupModelChanged(linear);
    assert(linear.scope == "selected");
    assert((linear.dataScope.originalRowIds == std::vector<int>{2, 4}));
    assert(linear.isStale && linear.lastRFitSignature.empty());
    assert(!separated.captureAnalysisScope("people", linear.dataScope,
        linear.dataScopeCaptured, &linear.scope));

    return 0;
}
