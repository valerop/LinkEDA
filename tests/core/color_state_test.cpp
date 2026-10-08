#include "../../src/core/application_state.h"

#include <cassert>
#include <set>

using namespace rlispstat::core;

static DataFrameModel ColorData()
{
    DataFrameModel data;
    data.group = "colors";
    data.rows = 4;
    data.stableRowIds = {"case-a", "case-b", "case-c", "case-d"};
    data.dataVersion = 7;
    DataColumn arm;
    arm.name = "arm";
    arm.type = "factor";
    arm.values = {"Control", "Treatment", "Control", "Treatment"};
    arm.displayValues = arm.values;
    arm.definedLevels = {"Control", "Treatment"};
    data.columns = {arm};
    return data;
}

int main()
{
    ApplicationState state;
    assert(state.registerDataset(ColorData()));
    assert(state.setPointColor("colors", 1, "red"));
    assert(state.setPointColor("colors", 2, "blue"));
    const auto persistent = state.pointColors("colors");

    std::string error;
    assert(state.setColorOverride("colors", "arm", &error));
    assert(state.colorOverride("colors")->variable == "arm");
    assert(state.pointColors("colors") == persistent);
    assert(state.displayPointColors("colors") != persistent);

    AnalysisScope subset = ExplicitAnalysisScope(
        "colors", {1, 3}, AnalysisScopeSourceKind::OtherExplicitSubset,
        "Subset", 4);
    assert(state.setActiveAnalysisScope(subset));
    assert(state.colorOverride("colors")->variable == "arm");

    assert(state.cancelColorOverride("colors"));
    assert(!state.colorOverride("colors"));
    assert(state.displayPointColors("colors") == persistent);

    ApplicationState promoted;
    assert(promoted.registerDataset(ColorData()));
    assert(promoted.setPointColor("colors", 1, "red"));
    assert(promoted.setColorOverride("colors", "arm", &error));
    const auto visibleOverride = promoted.displayPointColors("colors");
    assert(promoted.promoteColorOverrideToPersistent("colors"));
    assert(!promoted.colorOverride("colors"));
    assert(promoted.pointColors("colors") == visibleOverride);
    assert(promoted.displayPointColors("colors") == visibleOverride);

    assert(state.setColorOverride("colors", "arm", &error));
    assert(state.setPointColor("colors", 3, "green"));
    assert(!state.colorOverride("colors"));
    assert(state.pointColors("colors").size() == 3);

    assert(state.saveCurrentColorsAsScheme("colors", "Subset colours", &error));
    const auto scheme = state.colorScheme("colors", "Subset colours");
    assert(scheme);
    assert(scheme->scopeStableRowIds ==
           std::vector<std::string>({"case-a", "case-c"}));
    assert(scheme->colorsByStableRowId.at("case-a") == "red");
    assert(scheme->colorsByStableRowId.at("case-c") == "green");
    assert(state.colorSchemeCompatibility("colors", "Subset colours").exactScope);

    assert(state.setPointColor("colors", 1, "orange"));
    std::vector<int> changed;
    assert(state.applyColorSchemeToMatchingObservations(
        "colors", "Subset colours", &changed, &error));
    assert(std::set<int>(changed.begin(), changed.end()) == std::set<int>({1, 3}));
    assert(state.pointColors("colors").front().second == "red");

    assert(state.resetActiveAnalysisScopeToAllObservations("colors"));
    const ColorSchemeCompatibility mismatch =
        state.colorSchemeCompatibility("colors", "Subset colours");
    assert(!mismatch.exactScope);
    assert(mismatch.currentScopeCount == 4);
    assert(mismatch.schemeScopeCount == 2);
    AnalysisScopeChangeEvent event;
    assert(state.switchToColorSchemeScopeAndApply(
        "colors", "Subset colours", &event, &changed, &error));
    assert(state.resolveActiveAnalysisRowIds("colors") == std::vector<int>({1, 3}));
    return 0;
}
