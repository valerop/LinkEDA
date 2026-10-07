#include "../../src/core/application_state.h"
#include "../../src/core/command_dispatcher.h"

#include <algorithm>
#include <cassert>
#include <string>

int main()
{
    using namespace rlispstat::core;
    ApplicationState state;
    DataFrameModel dataset;
    dataset.group = "data";
    dataset.rows = 2;
    DataColumn auxiliary;
    auxiliary.name = "aux";
    auxiliary.type = "numeric";
    auxiliary.values = {"1", "2"};
    dataset.columns.push_back(auxiliary);
    assert(state.registerDataset(dataset));

    for (const std::string &role : {"exposure", "offset", "trials"}) {
        GeneralizedGLMState single;
        single.id = "single_" + role;
        single.group = "data";
        single.response = "y";
        GeneralizedComparisonState comparison;
        comparison.id = "comparison_" + role;
        comparison.group = "data";
        GeneralizedComparisonModel column;
        column.id = "column_" + role;
        column.response = "y";
        if (role == "exposure") single.exposure = column.exposure = "aux";
        if (role == "offset") single.offsetVariable = column.offsetVariable = "aux";
        if (role == "trials") single.trialsVariable = column.trialsVariable = "aux";
        comparison.models.push_back(column);
        state.generalizedGLMs()[single.id] = single;
        state.generalizedComparisons()[comparison.id] = comparison;
    }

    const auto changed = state.applyDatasetValueChange("data", "aux");
    assert(changed.generalizedGlmIds.size() == 3);
    assert(changed.generalizedGlmIdsToRefit.size() == 3);
    assert(changed.generalizedComparisonIds.size() == 3);

    GeneralizedGLMState frozenGeneralized;
    frozenGeneralized.id = "single_frozen";
    frozenGeneralized.group = "data";
    frozenGeneralized.response = "y";
    frozenGeneralized.terms = {"aux"};
    frozenGeneralized.autoRefit = false;
    frozenGeneralized.ok = true;
    frozenGeneralized.modelVersion = 2;
    frozenGeneralized.fitVersion = 2;
    GeneralizedGLMRow frozenRow;
    frozenRow.term = "aux";
    frozenRow.sourceTerm = "aux";
    frozenGeneralized.rows.push_back(frozenRow);
    state.generalizedGLMs()[frozenGeneralized.id] = frozenGeneralized;
    const auto frozenChange = state.applyDatasetValueChange("data", "aux");
    const auto &frozenAfterChange = state.generalizedGLMs().at("single_frozen");
    assert(frozenAfterChange.ok);
    assert(frozenAfterChange.rows.size() == 1);
    assert(frozenAfterChange.modelVersion == 3);
    assert(frozenAfterChange.fitVersion == 2);
    assert(frozenAfterChange.frozenScopeNotice.find("previous data") != std::string::npos);
    assert(std::find(frozenChange.generalizedGlmIdsToRefit.begin(),
                     frozenChange.generalizedGlmIdsToRefit.end(),
                     "single_frozen") == frozenChange.generalizedGlmIdsToRefit.end());
    GeneralizedGLMState polynomial;
    polynomial.id = "single_polynomial";
    polynomial.group = "data";
    polynomial.response = "y";
    polynomial.terms = {"I(aux^2)"};
    state.generalizedGLMs()[polynomial.id] = polynomial;
    GeneralizedComparisonState polynomialComparison;
    polynomialComparison.id = "comparison_polynomial";
    polynomialComparison.group = "data";
    GeneralizedComparisonModel polynomialColumn;
    polynomialColumn.id = "column_polynomial";
    polynomialColumn.response = "y";
    polynomialColumn.terms = {"I(aux^2)"};
    polynomialComparison.models.push_back(polynomialColumn);
    state.generalizedComparisons()[polynomialComparison.id] = polynomialComparison;
    NativeMixedModelState mixed;
    mixed.id = "mixed";
    mixed.group = "data";
    mixed.response = "y";
    mixed.fixedEffects = {"aux"};
    state.nativeMixedModels()[mixed.id] = mixed;
    const auto retyped = state.applyVariableTypeChange("data", "aux", "numeric");
    assert(retyped.generalizedGlmIds.size() == 5);
    assert(retyped.generalizedGlmIdsToRefit.size() == 4);
    assert(retyped.generalizedComparisonIds.size() == 4);
    assert(retyped.mixedModelIds == std::vector<std::string>({"mixed"}));

    ScaleAnalysisState scale;
    scale.id = "scale";
    scale.group = "data";
    scale.specification.items.push_back({"aux"});
    state.scaleAnalyses()[scale.id] = scale;
    ScaleAnalysisState frozenScale = scale;
    frozenScale.id = "frozen_scale";
    frozenScale.autoFit = false;
    frozenScale.result.backend = "R";
    frozenScale.result.summary = "Previous scale result";
    state.scaleAnalyses()[frozenScale.id] = frozenScale;
    const auto frozenScaleChange = state.applyDatasetValueChange("data", "aux");
    assert(std::find(frozenScaleChange.scaleAnalysisIds.begin(),
                     frozenScaleChange.scaleAnalysisIds.end(),
                     "frozen_scale") != frozenScaleChange.scaleAnalysisIds.end());
    const auto &frozenScaleAfterChange = state.scaleAnalyses().at("frozen_scale");
    assert(frozenScaleAfterChange.result.backend == "R");
    assert(frozenScaleAfterChange.result.summary == "Previous scale result");
    assert(frozenScaleAfterChange.status.find("Displaying the last completed result") !=
           std::string::npos);
    ModelTrellisState trellis;
    trellis.id = "trellis";
    trellis.specification.baseModel.group = "data";
    trellis.specification.baseModel.response = "aux";
    trellis.specification.baseModel.terms = {"aux"};
    trellis.specification.rowConditioningVariable =
        ModelTrellisConditioningVariable{"aux", ModelTrellisDimension::Rows};
    state.modelTrellises()[trellis.id] = trellis;

    const int scaleRevisionBeforeRename =
        state.scaleAnalyses().at("scale").specification.revision;
    const auto renamed = state.applyVariableRename("data", "aux", "renamed");
    assert(renamed.scaleAnalysisIds.size() == 2);
    assert(std::find(renamed.scaleAnalysisIds.begin(), renamed.scaleAnalysisIds.end(),
                     "scale") != renamed.scaleAnalysisIds.end());
    assert(std::find(renamed.scaleAnalysisIds.begin(), renamed.scaleAnalysisIds.end(),
                     "frozen_scale") != renamed.scaleAnalysisIds.end());
    assert(renamed.modelTrellisIds.size() == 1);
    assert(renamed.modelTrellisIds.front() == "trellis");
    assert(renamed.generalizedGlmIds.size() == 5);
    assert(renamed.generalizedGlmIdsToRefit.size() == 4);
    assert(renamed.generalizedComparisonIds.size() == 4);
    assert(renamed.mixedModelIds == std::vector<std::string>({"mixed"}));
    const auto &renamedScale = state.scaleAnalyses().at("scale");
    assert(renamedScale.specification.items.front().variable == "renamed");
    assert(renamedScale.specification.revision == scaleRevisionBeforeRename + 1);
    const auto &renamedTrellis = state.modelTrellises().at("trellis");
    assert(renamedTrellis.specification.baseModel.response == "renamed");
    assert(renamedTrellis.specification.baseModel.terms.front() == "renamed");
    assert(renamedTrellis.specification.rowConditioningVariable->variableId == "renamed");
    assert(state.nativeMixedModels().at("mixed").fixedEffects.front() == "renamed");
    for (const std::string &role : {"exposure", "offset", "trials"}) {
        const auto &single = state.generalizedGLMs().at("single_" + role);
        const auto &column = state.generalizedComparisons()
            .at("comparison_" + role).models.front();
        if (role == "exposure")
            assert(single.exposure == "renamed" && column.exposure == "renamed");
        else if (role == "offset")
            assert(single.offsetVariable == "renamed" && column.offsetVariable == "renamed");
        else
            assert(single.trialsVariable == "renamed" && column.trialsVariable == "renamed");
    }

    DatasetMutationEvent renameEvent;
    CommandDispatcherServices services;
    services.ui.datasetMutated = [&](const DatasetMutationEvent &event) {
        renameEvent = event;
    };
    CommandDispatcher dispatcher(services);
    assert(dispatcher.applicationState().registerDataset(dataset));
    ScaleAnalysisState linkedScale;
    linkedScale.id = "linked_scale";
    linkedScale.group = "data";
    linkedScale.specification.items.push_back({"aux"});
    dispatcher.applicationState().scaleAnalyses()[linkedScale.id] = linkedScale;
    dispatcher.applicationState().generalizedGLMs()["pending"].group = "data";
    dispatcher.applicationState().generalizedGLMs()["pending"].rFitPending = true;
    assert(dispatcher.dispatch({"SET_VARIABLE_TYPE", "data", "aux", "numeric"}).rfind(
        "ERR Wait for the current R calculation", 0) == 0);
    assert(dispatcher.dispatch({"RENAME_VARIABLE", "data", "aux", "renamed"}).rfind(
        "ERR Wait for the current R calculation", 0) == 0);
    dispatcher.applicationState().generalizedGLMs()["pending"].rFitPending = false;
    assert(dispatcher.dispatch({"RENAME_VARIABLE", "data", "aux", "renamed"}).rfind(
        "OK", 0) == 0);
    assert(renameEvent.kind == DatasetMutationKind::VariableRename);
    assert(renameEvent.valueChangeEffects.scaleAnalysisIds.size() == 1);
    assert(renameEvent.valueChangeEffects.scaleAnalysisIds.front() == "linked_scale");
    dispatcher.applicationState().generalizedGLMs()["pending"].rFitPending = true;
    assert(dispatcher.dispatch({"DATA_ADD_SELECTION_COLUMN", "data"}).rfind(
        "ERR Wait for the current R calculation", 0) == 0);
    dispatcher.applicationState().generalizedGLMs()["pending"].rFitPending = false;
    assert(dispatcher.dispatch({"DATA_ADD_SELECTION_COLUMN", "data"}).rfind(
        "OK", 0) == 0);
    assert(renameEvent.kind == DatasetMutationKind::ColumnStructure);
    return 0;
}
