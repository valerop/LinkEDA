#include "core/analysis_initialization.h"

#include <algorithm>
#include <cassert>
#include <map>
#include <string>
#include <vector>

using namespace rlispstat::core;

static DataColumn Column(std::string name, std::string type,
                         std::vector<std::string> values)
{
    DataColumn result;
    result.name = std::move(name);
    result.type = std::move(type);
    result.values = std::move(values);
    return result;
}

int main()
{
    DataFrameModel data;
    data.group = "study";
    data.rows = 4;
    data.columns = {
        Column("outcome", "numeric", {"1", "2", "3", "4"}),
        Column("outcome_2", "numeric", {"2", "3", "4", "5"}),
        Column("score", "numeric", {"4", "3", "2", "1"}),
        Column("age", "numeric", {"20", "30", "40", "50"}),
        Column("rank", "ordered", {"low", "medium", "high", "high"}),
        Column("arm_code", "numeric", {"0", "1", "0", "1"}),
        Column("arm", "factor", {"control", "treatment", "control", "treatment"}),
        Column("site", "factor", {"a", "b", "a", "b"}),
        Column("planet", "factor", {"Aurelia", "Borealis", "Cygnus", "Aurelia"}),
        Column("notes", "character", {"free one", "free two", "free three", "free four"})
    };

    const auto linear = SharedAnalysisDefinition(SharedAnalysisKind::LinearModel);

    // Availability is a backend fact shared by Windows and macOS.  Keep the
    // regression-family matrix centralized so a native adapter cannot leave
    // an obsolete local MI guard behind.
    assert(SharedAnalysisSupportsMultipleImputation(SharedAnalysisKind::LinearModel));
    assert(SharedAnalysisSupportsMultipleImputation(SharedAnalysisKind::RegressionComparison));
    assert(SharedAnalysisSupportsMultipleImputation(SharedAnalysisKind::GeneralizedLinearModel));
    assert(SharedAnalysisSupportsMultipleImputation(SharedAnalysisKind::GeneralizedComparison));
    assert(SharedAnalysisSupportsMultipleImputation(SharedAnalysisKind::CountRegression));
    assert(SharedAnalysisSupportsMultipleImputation(SharedAnalysisKind::BinaryRegression));
    assert(!SharedAnalysisSupportsMultipleImputation(SharedAnalysisKind::LinearMixedModel));

    // Exact valid roles.
    auto exact = ResolveInitialAnalysisSpecification(
        data, linear, {{"outcome", "dependent"}, {"score", "independent"}});
    assert(InitialAnalysisVariable(exact, "response") == "outcome");
    assert(InitialAnalysisVariables(exact, "predictors") ==
           std::vector<std::string>({"score"}));
    assert(exact.minimumValid);

    // No roles: compatible columns exist, but position is not intent.
    auto none = ResolveInitialAnalysisSpecification(data, linear, {});
    assert(InitialAnalysisVariable(none, "response").empty());
    assert(InitialAnalysisVariables(none, "predictors").empty());
    assert(!none.minimumValid);

    // Partial roles preserve the justified response and leave X unresolved.
    auto partial = ResolveInitialAnalysisSpecification(
        data, linear, {{"outcome", "dependent"}});
    assert(InitialAnalysisVariable(partial, "response") == "outcome");
    assert(InitialAnalysisVariables(partial, "predictors").empty());
    assert(partial.unresolvedSlots.count("predictors") == 1);

    // Ambiguous single slot remains blank.
    auto ambiguous = ResolveInitialAnalysisSpecification(
        data, linear, {{"outcome", "dependent"}, {"outcome_2", "dependent"},
                       {"score", "independent"}});
    assert(InitialAnalysisVariable(ambiguous, "response").empty());

    // Multiple predictor role is intentionally multi-valued.
    auto multiple = ResolveInitialAnalysisSpecification(
        data, linear, {{"outcome", "dependent"}, {"score", "independent"},
                       {"age", "independent"}});
    assert(InitialAnalysisVariables(multiple, "predictors") ==
           std::vector<std::string>({"age", "score"}));

    // Dataset defaults remain document-compatible, but the feature is
    // experimental and disabled: fresh analyses must not inherit them.
    assert(!DefaultVariableRolesExperimentalFeatureEnabled());
    const AnalysisVariableRoles storedDefaults{
        {"outcome", "dependent"}, {"score", "independent"},
        {"age", "independent"}, {"arm", "grouping"}};
    assert(DefaultVariableRolesForAnalysisInitialization(storedDefaults).empty());
    const auto freshRoles = FreshRegressionAnalysisVariableRoles(
        storedDefaults);
    assert(freshRoles.empty());
    const auto freshAliasRoles = FreshRegressionAnalysisVariableRoles(
        {{"outcome", "response"}, {"score", "predictor"}});
    assert(freshAliasRoles.empty());
    const auto freshGeneralized = ResolveInitialAnalysisSpecification(
        data, SharedAnalysisDefinition(SharedAnalysisKind::GeneralizedLinearModel),
        freshRoles);
    assert(InitialAnalysisVariable(freshGeneralized, "response").empty());
    assert(InitialAnalysisVariables(freshGeneralized, "predictors").empty());

    // An incompatible categorical response role cannot initialize a linear model.
    auto incompatible = ResolveInitialAnalysisSpecification(
        data, linear, {{"arm", "dependent"}, {"score", "independent"}});
    assert(InitialAnalysisVariable(incompatible, "response").empty());

    // An existing specification wins over current roles.
    AnalysisSpecification saved{{"response", {"outcome_2"}},
                                {"predictors", {"age"}}};
    auto restored = ResolveInitialAnalysisSpecification(
        data, linear, {{"outcome", "dependent"}, {"score", "independent"}}, &saved);
    assert(InitialAnalysisVariable(restored, "response") == "outcome_2");
    assert(InitialAnalysisVariables(restored, "predictors") ==
           std::vector<std::string>({"age"}));

    // A present but invalid saved slot is still authoritative.  It must not
    // silently fall back to a dataset role and thereby change a saved model.
    AnalysisSpecification invalidSaved{{"response", {"arm"}},
                                       {"predictors", {"age"}}};
    auto invalidRestored = ResolveInitialAnalysisSpecification(
        data, linear, {{"outcome", "dependent"}, {"score", "independent"}},
        &invalidSaved);
    assert(InitialAnalysisVariable(invalidRestored, "response").empty());
    assert(invalidRestored.unresolvedSlots.count("response") == 1);
    assert(!invalidRestored.minimumValid);

    // Comparison analyses keep the same type semantics as their editors:
    // ordinal responses are valid, and an explicitly declared numeric binary
    // group is valid for an independent-samples comparison.
    const auto independent = SharedAnalysisDefinition(
        SharedAnalysisKind::IndependentSamplesComparison);
    auto ordinalComparison = ResolveInitialAnalysisSpecification(
        data, independent, {{"rank", "dependent"}, {"arm_code", "grouping"}});
    assert(InitialAnalysisVariables(ordinalComparison, "responses") ==
           std::vector<std::string>({"rank"}));
    assert(InitialAnalysisVariable(ordinalComparison, "group") == "arm_code");
    assert(ordinalComparison.minimumValid);

    // Two-sample comparisons only offer variables with exactly two observed
    // groups.  Variables with three or more groups belong in the one-way
    // procedures and must not leak into the native grouping-variable popup.
    auto threeLevelComparison = ResolveInitialAnalysisSpecification(
        data, independent, {{"rank", "dependent"}, {"planet", "grouping"}});
    assert(InitialAnalysisVariable(threeLevelComparison, "group").empty());
    assert(!threeLevelComparison.minimumValid);
    const auto groupSlot = independent.slots[1];
    assert(!AnalysisVariableIsCompatible(data.columns[8], groupSlot.acceptedType));
    assert(!AnalysisVariableIsCompatible(data.columns[9], groupSlot.acceptedType));
    assert(AnalysisVariableIsCompatible(data.columns[6], groupSlot.acceptedType));
    assert(AnalysisVariableIsCompatible(data.columns[5], groupSlot.acceptedType));

    // Unused declared levels do not change the effective cardinality.
    DataColumn binaryWithUnusedLevel = data.columns[6];
    binaryWithUnusedLevel.definedLevels = {"control", "treatment", "unused"};
    assert(AnalysisVariableIsCompatible(binaryWithUnusedLevel,
                                        groupSlot.acceptedType));

    const auto categoricalType = AnalysisVariableType::Categorical;
    assert(AnalysisVariableIsCompatible(data.columns[8], categoricalType));
    assert(!AnalysisVariableIsCompatible(data.columns[9], categoricalType));
    const auto modelPredictorType = AnalysisVariableType::NumericOrCategorical;
    assert(AnalysisVariableIsCompatible(data.columns[8], modelPredictorType));
    assert(!AnalysisVariableIsCompatible(data.columns[9], modelPredictorType));

    // Semantically distinct slots cannot reuse the same variable.  Keep the
    // values visible so the UI can explain/correct the partial specification,
    // but never allow it to be computed.
    AnalysisSpecification repeated{{"response", {"outcome"}},
                                   {"predictors", {"outcome"}}};
    auto repeatedLinear = ResolveInitialAnalysisSpecification(
        data, linear, {}, &repeated);
    assert(InitialAnalysisVariable(repeatedLinear, "response") == "outcome");
    assert(InitialAnalysisVariables(repeatedLinear, "predictors") ==
           std::vector<std::string>({"outcome"}));
    assert(repeatedLinear.unresolvedSlots.count("response") == 1);
    assert(repeatedLinear.unresolvedSlots.count("predictors") == 1);
    assert(!repeatedLinear.minimumValid);

    // The same pure resolver is the semantic input consumed by both adapters.
    auto macSemantic = ResolveInitialAnalysisSpecification(
        data, linear, {{"outcome", "dependent"}, {"score", "independent"}});
    auto windowsSemantic = ResolveInitialAnalysisSpecification(
        data, linear, {{"outcome", "dependent"}, {"score", "independent"}});
    assert(macSemantic.variables == windowsSemantic.variables);
    assert(macSemantic.unresolvedSlots == windowsSemantic.unresolvedSlots);

    // Single categorical slots also detect ambiguity instead of using order.
    const auto contingency = SharedAnalysisDefinition(SharedAnalysisKind::ContingencyTable);
    for (const auto& names : std::vector<std::pair<std::string, std::string>>{
             {"rank", "arm"}, {"arm", "site"}}) {
        AnalysisSpecification selected{{"row", {names.first}}, {"column", {names.second}}};
        const auto resolved = ResolveInitialAnalysisSpecification(data, contingency, {}, &selected);
        assert(resolved.minimumValid);
        assert(InitialAnalysisVariable(resolved, "row") == names.first);
        assert(InitialAnalysisVariable(resolved, "column") == names.second);
    }
    for (const auto& name : {"arm_code", "score", "notes"}) {
        AnalysisSpecification selected{{"row", {name}}, {"column", {"arm"}}};
        const auto resolved = ResolveInitialAnalysisSpecification(data, contingency, {}, &selected);
        assert(!resolved.minimumValid);
        assert(InitialAnalysisVariable(resolved, "row").empty());
    }
    auto ambiguousColumn = ResolveInitialAnalysisSpecification(
        data, contingency, {{"arm", "dependent"}, {"site", "independent"}});
    assert(InitialAnalysisVariable(ambiguousColumn, "row") == "arm");
    assert(InitialAnalysisVariable(ambiguousColumn, "column") == "site");

    // A mixed model never promotes an ordinary predictor to a random grouping
    // factor.  That semantic role must be explicit, while fixed effects remain
    // a legitimate partial initialization.
    const auto mixed = SharedAnalysisDefinition(SharedAnalysisKind::LinearMixedModel);
    auto partialMixed = ResolveInitialAnalysisSpecification(
        data, mixed, {{"outcome", "dependent"}, {"score", "independent"},
                      {"arm", "independent"}});
    assert(InitialAnalysisVariable(partialMixed, "response") == "outcome");
    assert(InitialAnalysisVariables(partialMixed, "fixed_effects") ==
           std::vector<std::string>({"arm", "score"}));
    assert(InitialAnalysisVariable(partialMixed, "group").empty());
    assert(!partialMixed.minimumValid);

    auto completeMixed = ResolveInitialAnalysisSpecification(
        data, mixed, {{"outcome", "dependent"}, {"score", "independent"},
                      {"arm", "grouping"}});
    assert(InitialAnalysisVariable(completeMixed, "group") == "arm");
    assert(completeMixed.minimumValid);

    // Plot slots follow the same ambiguity rules as analysis tables.
    const auto scatter = SharedAnalysisDefinition(SharedAnalysisKind::Scatterplot);
    auto ambiguousScatter = ResolveInitialAnalysisSpecification(
        data, scatter, {{"outcome", "dependent"}, {"score", "independent"},
                        {"age", "independent"}});
    assert(InitialAnalysisVariable(ambiguousScatter, "y") == "outcome");
    assert(InitialAnalysisVariable(ambiguousScatter, "x").empty());
    assert(!ambiguousScatter.minimumValid);

    // A grouping role initializes the boxplot axis only; it is not copied to
    // the independent conditioning slot.
    const auto boxplot = SharedAnalysisDefinition(SharedAnalysisKind::Boxplot);
    auto boxplotRoles = ResolveInitialAnalysisSpecification(
        data, boxplot, {{"outcome", "dependent"}, {"arm", "grouping"}});
    assert(InitialAnalysisVariable(boxplotRoles, "y") == "outcome");
    assert(InitialAnalysisVariable(boxplotRoles, "x") == "arm");
    assert(InitialAnalysisVariables(boxplotRoles, "conditioning").empty());
    assert(boxplotRoles.minimumValid);

    // Multi-variable displays accept every compatible explicit role and no
    // implicit columns.
    const auto matrix = SharedAnalysisDefinition(SharedAnalysisKind::ScatterplotMatrix);
    auto matrixRoles = ResolveInitialAnalysisSpecification(
        data, matrix, {{"outcome", "dependent"}, {"score", "independent"},
                       {"age", "independent"}, {"arm", "independent"}});
    assert(InitialAnalysisVariables(matrixRoles, "variables") ==
           std::vector<std::string>({"age", "outcome", "score"}));
    assert(matrixRoles.minimumValid);

    // Model trellis initialization is allowed to remain partial.  A grouping
    // role is required before the shared specification becomes computable.
    const auto modelTrellis = SharedAnalysisDefinition(SharedAnalysisKind::LinearModelTrellis);
    auto partialTrellis = ResolveInitialAnalysisSpecification(
        data, modelTrellis, {{"outcome", "dependent"}, {"score", "independent"}});
    assert(InitialAnalysisVariable(partialTrellis, "y") == "outcome");
    assert(InitialAnalysisVariables(partialTrellis, "predictors") ==
           std::vector<std::string>({"score"}));
    assert(InitialAnalysisVariables(partialTrellis, "conditioning").empty());
    assert(!partialTrellis.minimumValid);

    auto completeTrellis = ResolveInitialAnalysisSpecification(
        data, modelTrellis, {{"outcome", "dependent"}, {"score", "independent"},
                             {"site", "grouping"}});
    assert(InitialAnalysisVariables(completeTrellis, "conditioning") ==
           std::vector<std::string>({"site"}));
    assert(completeTrellis.minimumValid);

    // Empty specifications are first-class UI states.  Compatible dataset
    // columns never become defaults merely because they occur first.
    const auto descriptive = SharedAnalysisDefinition(
        SharedAnalysisKind::DescriptiveTable);
    const auto correlation = SharedAnalysisDefinition(
        SharedAnalysisKind::CorrelationMatrix);
    const auto quickCluster = SharedAnalysisDefinition(
        SharedAnalysisKind::QuickCluster);
    auto emptyDescriptive = ResolveInitialAnalysisSpecification(data, descriptive, {});
    auto emptyCorrelation = ResolveInitialAnalysisSpecification(data, correlation, {});
    auto emptyCluster = ResolveInitialAnalysisSpecification(data, quickCluster, {});
    const auto dimensionality = SharedAnalysisDefinition(SharedAnalysisKind::Dimensionality);
    auto emptyDimensionality = ResolveInitialAnalysisSpecification(data, dimensionality, {});
    const auto scaleAnalysis = SharedAnalysisDefinition(SharedAnalysisKind::ScaleAnalysis);
    auto emptyScale = ResolveInitialAnalysisSpecification(data, scaleAnalysis, {});
    auto emptyContingency = ResolveInitialAnalysisSpecification(data, contingency, {});
    assert(InitialAnalysisVariables(emptyDescriptive, "variables").empty());
    assert(InitialAnalysisVariables(emptyCorrelation, "variables").empty());
    assert(InitialAnalysisVariables(emptyCluster, "variables").empty());
    assert(InitialAnalysisVariables(emptyDimensionality, "variables").empty());
    assert(InitialAnalysisVariables(emptyScale, "items").empty());
    assert(InitialAnalysisVariable(emptyContingency, "row").empty());
    assert(InitialAnalysisVariable(emptyContingency, "column").empty());
    assert(!emptyDescriptive.minimumValid);
    assert(!emptyCorrelation.minimumValid);
    assert(!emptyCluster.minimumValid);
    assert(!emptyDimensionality.minimumValid);
    assert(!emptyScale.minimumValid);
    assert(!emptyContingency.minimumValid);

    // Valid explicit roles initialize only the slots those roles justify.
    auto roleDescriptive = ResolveInitialAnalysisSpecification(
        data, descriptive, {{"outcome", "dependent"}, {"score", "independent"},
                            {"arm", "grouping"}});
    auto roleCorrelation = ResolveInitialAnalysisSpecification(
        data, correlation, {{"outcome", "dependent"}, {"score", "independent"}});
    auto roleCluster = ResolveInitialAnalysisSpecification(
        data, quickCluster, {{"score", "independent"}, {"age", "independent"}});
    assert(InitialAnalysisVariables(roleDescriptive, "variables") ==
           std::vector<std::string>({"outcome", "score"}));
    assert(InitialAnalysisVariable(roleDescriptive, "group") == "arm");
    assert(roleDescriptive.minimumValid);
    assert(InitialAnalysisVariables(roleCorrelation, "variables") ==
           std::vector<std::string>({"outcome", "score"}));
    assert(roleCorrelation.minimumValid);
    assert(InitialAnalysisVariables(roleCluster, "variables") ==
           std::vector<std::string>({"age", "score"}));
    assert(roleCluster.minimumValid);

    auto roleScale = ResolveInitialAnalysisSpecification(
        data, scaleAnalysis, {{"score", "scale_item"}, {"rank", "scale_item"}});
    assert(InitialAnalysisVariables(roleScale, "items") ==
           std::vector<std::string>({"rank", "score"}));
    assert(roleScale.minimumValid);
    const auto scaleSlot = scaleAnalysis.slots.front();
    assert(AnalysisVariableIsCompatible(data.columns[2], scaleSlot.acceptedType));
    assert(AnalysisVariableIsCompatible(data.columns[4], scaleSlot.acceptedType));
    assert(!AnalysisVariableIsCompatible(data.columns[6], scaleSlot.acceptedType));

    // Partial contingency roles remain partial and never borrow the next
    // categorical column from dataset order.
    auto partialContingency = ResolveInitialAnalysisSpecification(
        data, contingency, {{"arm", "row"}});
    assert(InitialAnalysisVariable(partialContingency, "row") == "arm");
    assert(InitialAnalysisVariable(partialContingency, "column").empty());
    assert(!partialContingency.minimumValid);

    // Roles are initialization hints, never a permission system. An unroled
    // numeric column remains a valid manual PCA choice after role-based
    // initialization has run once.
    auto roleDimensionality = ResolveInitialAnalysisSpecification(
        data, dimensionality, {{"outcome", "dependent"}, {"score", "independent"}});
    assert(InitialAnalysisVariables(roleDimensionality, "variables") ==
           std::vector<std::string>({"outcome", "score"}));
    const auto dimensionalitySlot = dimensionality.slots.front();
    for (const char *name : {"outcome", "outcome_2", "score", "age", "arm_code"}) {
        const auto column = std::find_if(data.columns.begin(), data.columns.end(),
            [&](const DataColumn &candidate) { return candidate.name == name; });
        assert(column != data.columns.end());
        assert(AnalysisVariableIsCompatible(*column, dimensionalitySlot.acceptedType));
    }

    // The same shared row model drives blank, short and scrolling variable
    // lists on both platforms. The Add row is always part of the content.
    auto blankRows = BuildAnalysisVariableListLayout(0);
    assert(blankRows.totalRows == 1 && blankRows.visibleRows == 1);
    assert(blankRows.contentHeight == 24.0 && !blankRows.scrolls);
    auto shortRows = BuildAnalysisVariableListLayout(5);
    assert(shortRows.totalRows == 6 && shortRows.viewportHeight == 144.0);
    assert(!shortRows.scrolls);
    auto longRows = BuildAnalysisVariableListLayout(25);
    assert(longRows.totalRows == 26 && longRows.visibleRows == 10);
    assert(longRows.contentHeight == 624.0 && longRows.viewportHeight == 240.0);
    assert(longRows.scrolls);

    // The semantic specification passed to both adapters is identical for
    // the four migrated workflows.
    for (const auto kind : {SharedAnalysisKind::DescriptiveTable,
                            SharedAnalysisKind::CorrelationMatrix,
                            SharedAnalysisKind::QuickCluster,
                            SharedAnalysisKind::ContingencyTable}) {
        auto mac = ResolveInitialAnalysisSpecification(
            data, SharedAnalysisDefinition(kind), {});
        auto windows = ResolveInitialAnalysisSpecification(
            data, SharedAnalysisDefinition(kind), {});
        assert(mac.variables == windows.variables);
        assert(mac.unresolvedSlots == windows.unresolvedSlots);
        assert(mac.minimumValid == windows.minimumValid);
    }

    const auto anova = ResolveInitialAnalysisSpecification(data,
        SharedAnalysisDefinition(SharedAnalysisKind::OneWayComparison),
        {{"rank","dependent"},{"arm","dependent"},{"outcome","dependent"},{"site","grouping"}});
    assert(InitialAnalysisVariables(anova,"responses")==std::vector<std::string>{"outcome"});
    assert(InitialAnalysisVariable(anova,"group")=="site");
    return 0;
}
