#include "core/model_trellis_model.h"

#include <algorithm>
#include <cassert>
#include <set>

using namespace rlispstat::core;

int main()
{
    DataFrameModel data;
    data.group = "mtcars";
    data.rows = 5;
    DataColumn wt; wt.name = "wt"; wt.type = "numeric"; wt.values = {"2","3","4","5","6"};
    DataColumn mpg; mpg.name = "mpg"; mpg.type = "numeric"; mpg.values = {"20","21","22","23","24"};
    DataColumn am; am.name = "am"; am.type = "factor"; am.definedLevels = {"0","1"}; am.values = {"0","0","1","1","1"};
    DataColumn cyl; cyl.name = "cyl"; cyl.type = "factor"; cyl.definedLevels = {"4","6","8"}; cyl.values = {"4","6","4","6","6"};
    DataColumn hp; hp.name = "hp"; hp.type = "numeric"; hp.values = {"90","100","110","120","130"};
    data.columns = {wt, mpg, am, cyl, hp};

    ModelTrellisSpecification specification;
    specification.baseModel.group = "mtcars";
    specification.baseModel.response = "wt";
    specification.baseModel.terms = {"mpg", "am", "mpg:am"};
    specification.baseModel.termTypes = {{"mpg","numeric"},{"am","factor"}};
    std::string error;
    assert(SetModelTrellisConditioningVariable(specification, ModelTrellisDimension::Rows, "am", data, &error));
    assert(SetModelTrellisConditioningVariable(specification, ModelTrellisDimension::Columns, "cyl", data, &error));
    assert(!SetModelTrellisConditioningVariable(specification, ModelTrellisDimension::Columns, "am", data, &error));

    const auto candidates = ModelTrellisIndependentVariables(data, specification);
    assert(std::find(candidates.begin(), candidates.end(), "hp") != candidates.end());
    assert(std::find(candidates.begin(), candidates.end(), "am") == candidates.end());
    assert(std::find(candidates.begin(), candidates.end(), "cyl") == candidates.end());
    assert(AddModelTrellisIndependentVariable(specification, "hp", data, &error));
    assert(specification.baseModel.terms.back() == "hp");
    assert(specification.baseModel.termTypes["hp"] == "numeric");
    assert(!AddModelTrellisIndependentVariable(specification, "hp", data, &error));
    assert(RemoveModelTrellisTerm(specification, "hp"));
    assert(specification.baseModel.terms.size() == 3);

    std::vector<std::string> omitted;
    const auto effective = EffectiveModelTrellisTerms(specification, &omitted);
    assert(effective.size() == 1 && effective[0] == "mpg");
    assert(omitted.size() == 2);
    const auto panels = BuildModelTrellisPanels(specification, data);
    assert(panels.size() == 6);
    int empty = 0;
    std::set<std::string> ids;
    for (const auto &panel : panels) {
        ids.insert(panel.panelId);
        if (!panel.hasObservations) ++empty;
    }
    assert(ids.size() == 6);
    assert(empty == 2);

    ModelTrellisState state;
    state.specification = specification;
    state.panels = panels;
    const auto layout = BuildModelTrellisLayout(state);
    assert(layout.rows == 2 && layout.columns == 3);
    const auto firstIds = ids;
    SwapModelTrellisDimensions(state.specification);
    for (auto &panel : state.panels) {
        std::swap(panel.key.rowLevelId, panel.key.columnLevelId);
        std::swap(panel.rowLevelLabel, panel.columnLevelLabel);
    }
    const auto swapped = BuildModelTrellisLayout(state);
    assert(swapped.rows == 3 && swapped.columns == 2);
    for (const auto &panel : state.panels) assert(firstIds.count(panel.panelId));

    state.specification.displayedTermId = "mpg";
    const std::string csv = ModelTrellisSelectedResultCSV(state);
    assert(csv.find("Row_Condition,Column_Condition,N") == 0);
    assert(csv.find("No observations") != std::string::npos);

    state.specificationGeneration = 7;
    state.fitPending = true;
    std::vector<std::string> payload = {
        "1", state.panels[0].panelId, "", "", "", "", "FALSE", "FALSE", "1", "No observations",
        "FALSE", "0", "0", "0", "0", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
        "NA", "NA", "NA", "NA", "No observations", "0", "0", "0",
        "0", "0", "NA", "NA", "NA"
    };
    std::size_t cursor = 0;
    assert(ApplyModelTrellisUpdatePayload(state, 7, payload, cursor, &error));
    assert(!state.fitPending && state.fittedGeneration == 7);
    assert(!state.panels[0].hasObservations);
    return 0;
}
