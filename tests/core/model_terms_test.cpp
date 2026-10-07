#include "../../src/core/model_terms.h"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <limits>
#include <map>
#include <set>
#include <string>
#include <vector>

using rlispstat::core::AddHierarchicalTermsToVector;
using rlispstat::core::BaseVariableForTermComponent;
using rlispstat::core::BaseVariablesFromModelTerms;
using rlispstat::core::BuildInteractionMenuGroups;
using rlispstat::core::EquivalentModelTerm;
using rlispstat::core::FactorLevelTermValue;
using rlispstat::core::FilterModelTermsForResponse;
using rlispstat::core::HierarchicalTermsForModelTerm;
using rlispstat::core::InteractionCandidateTermsForModel;
using rlispstat::core::InteractionCandidateTermsForComparison;
using rlispstat::core::IsInteractionTerm;
using rlispstat::core::IsPolynomialTerm;
using rlispstat::core::ParsePolynomialTerm;
using rlispstat::core::PolynomialTerm;
using rlispstat::core::ModelTermExistsForVariables;
using rlispstat::core::ModelTermBaseForDisplayRow;
using rlispstat::core::ModelTermShouldBeRemoved;
using rlispstat::core::ModelTermTypeIsInteraction;
using rlispstat::core::ModelTermTypeDisplayName;
using rlispstat::core::ModelTermWithoutLevelSuffixes;
using rlispstat::core::RenameVariableInTerm;
using rlispstat::core::RenameVariableInTermList;
using rlispstat::core::RemoveModelTermCascade;
using rlispstat::core::ResolvedModelTermDisplayType;
using rlispstat::core::SemanticInteractionTermType;
using rlispstat::core::SplitInteractionTerm;
using rlispstat::core::TermListContainsEquivalentModelTerm;
using rlispstat::core::UniqueBaseVariablesForTerm;
using rlispstat::core::VariableRoleForModelContext;
using rlispstat::core::AddModelSpecificationTerm;
using rlispstat::core::CapabilitiesForModelFamily;
using rlispstat::core::EquivalentModelSpecifications;
using rlispstat::core::EffectiveModelSpecificationTermTypes;
using rlispstat::core::ModelFamilyKind;
using rlispstat::core::ModelPredictorMetadata;
using rlispstat::core::ModelSpecification;
using rlispstat::core::ModelSpecificationFitScope;
using rlispstat::core::ModelSpecificationIncludesTerm;
using rlispstat::core::ModelSpecificationSelectionRows;
using rlispstat::core::ModelSpecificationTermType;
using rlispstat::core::RemoveModelSpecificationTerm;
using rlispstat::core::ReplaceModelSpecificationTerm;
using rlispstat::core::ResolveModelTermSpecification;
using rlispstat::core::SetModelSpecificationPredictorCentered;
using rlispstat::core::SetModelSpecificationReferenceLevel;
using rlispstat::core::SetModelSpecificationResponse;
using rlispstat::core::SetModelSpecificationTermType;

static bool equal(const std::vector<std::string> &a, const std::vector<std::string> &b)
{
    return a == b;
}

int main()
{
    std::vector<std::string> variables = {"mpg", "wt", "hp", "cyl", "am"};

    ModelSpecification scopedModel;
    scopedModel.scope = "selected";
    assert(ModelSpecificationFitScope(scopedModel) == "selected");
    assert(ModelSpecificationSelectionRows(scopedModel, {7, 2, 7, -1}) ==
           std::vector<int>({2, 7}));
    scopedModel.scope = "unselected";
    assert(ModelSpecificationSelectionRows(scopedModel, {2, 7}) ==
           std::vector<int>({2, 7}));
    scopedModel.scope = "compare_selected_all";
    assert(ModelSpecificationFitScope(scopedModel) == "all");
    assert(ModelSpecificationSelectionRows(scopedModel, {2, 7}).empty());
    scopedModel.scope = "unselected";
    scopedModel.dataScope = rlispstat::core::ExplicitAnalysisScope(
        "scope-test", {5, 3},
        rlispstat::core::AnalysisScopeSourceKind::CurrentSelection,
        "test", 8);
    scopedModel.dataScopeCaptured = true;
    assert(ModelSpecificationFitScope(scopedModel) == "selected");
    assert(ModelSpecificationSelectionRows(scopedModel, {2, 7}) ==
           std::vector<int>({5, 3}));
    scopedModel.dataScope =
        rlispstat::core::AllObservationsAnalysisScope("scope-test", 8);
    assert(ModelSpecificationFitScope(scopedModel) == "unselected");
    assert(ModelSpecificationSelectionRows(scopedModel, {2, 7}).empty());

    assert(equal(SplitInteractionTerm(" wt : factor(cyl) "), {"wt", "factor(cyl)"}));
    assert(IsInteractionTerm("wt:cyl"));
    assert(!IsInteractionTerm("wt"));
    assert(BaseVariableForTermComponent(" factor(cyl) ") == "cyl");
    std::string polynomialVariable;
    int polynomialDegree = 0;
    assert(ParsePolynomialTerm(" I(wt^3) ", polynomialVariable, polynomialDegree));
    assert(polynomialVariable == "wt" && polynomialDegree == 3);
    assert(IsPolynomialTerm("I(wt^2)"));
    assert(!IsPolynomialTerm("wt^2"));
    assert(equal(rlispstat::core::HierarchicalTermsForModelTerm({"x", "y"}, "y", "I(x^4)"),
                 {"x", "I(x^2)", "I(x^3)", "I(x^4)"}));
    const auto powers = rlispstat::core::PolynomialMenuTerms({"x", "y"}, "y", {"x", "I(x^2)"});
    assert(powers.size() == 1 && powers.count("x"));
    assert(equal(powers.at("x"), {"I(x^3)", "I(x^4)", "I(x^5)"}));
    assert(rlispstat::core::PolynomialMenuTerms({"x"}, "y", {}, "x:g").empty());
    const auto limitedPowers = rlispstat::core::PolynomialMenuTerms(
        {"am", "cyl", "wt"}, "drat", {}, "",
        {{"am", 1}, {"cyl", 2}, {"wt", 5}});
    assert(limitedPowers.count("am") == 0);
    assert(equal(limitedPowers.at("cyl"), {"I(cyl^2)"}));
    assert(equal(limitedPowers.at("wt"),
                 {"I(wt^2)", "I(wt^3)", "I(wt^4)", "I(wt^5)"}));
    assert(PolynomialTerm(" wt ", 4) == "I(wt^4)");
    assert(BaseVariableForTermComponent("I(wt^2)") == "wt");
    assert(equal(UniqueBaseVariablesForTerm("wt:factor(cyl):wt"), {"wt", "cyl"}));
    assert(RenameVariableInTerm("wt:factor(cyl)", "cyl", "cylinders") == "wt:factor(cylinders)");
    assert(RenameVariableInTerm(" wt : cyl ", "wt", "weight") == "weight:cyl");
    assert(RenameVariableInTerm("I(wt^3):cyl", "wt", "weight") ==
           "I(weight^3):cyl");

    assert(EquivalentModelTerm("wt:cyl", "factor(cyl):wt"));
    assert(EquivalentModelTerm("I(wt^2):cyl", "cyl:I(wt^2)"));
    assert(!EquivalentModelTerm("I(wt^2):cyl", "wt:cyl"));
    assert(!EquivalentModelTerm("I(wt^2):cyl", "I(wt^3):cyl"));
    assert(!EquivalentModelTerm("wt", "factor(wt)"));
    assert(ModelTermWithoutLevelSuffixes("cyl=6:am") == "cyl:am");
    assert(ModelTermWithoutLevelSuffixes("x:group=B") == "x:group");
    assert(ModelTermWithoutLevelSuffixes("group=B") == "group");
    assert(ModelTermWithoutLevelSuffixes("wt:hp") == "wt:hp");
    assert(ModelTermBaseForDisplayRow("group=B") == "group");
    assert(ModelTermBaseForDisplayRow("x:group=B") == "x:group");
    assert(ModelTermBaseForDisplayRow("factor(cyl)") == "factor(cyl)");
    assert(ModelTermBaseForDisplayRow("") == "");
    assert(FactorLevelTermValue(1.0) == "1");
    assert(FactorLevelTermValue(1.2345678901234) == "1.23456789012");
    assert(FactorLevelTermValue(std::numeric_limits<double>::quiet_NaN()).empty());
    assert(ModelTermExistsForVariables(variables, "factor(cyl):wt", "mpg"));
    assert(!ModelTermExistsForVariables(variables, "mpg:wt", "mpg"));
    assert(!ModelTermExistsForVariables(variables, "unknown:wt", "mpg"));

    std::vector<std::string> filtered = FilterModelTermsForResponse(
        variables,
        {"wt", "factor(cyl)", "cyl:wt", "wt:cyl", "mpg", "unknown"},
        "mpg");
    assert(equal(filtered, {"wt", "factor(cyl)", "cyl:wt"}));

    assert(ModelTermShouldBeRemoved("wt:cyl:hp", "wt:cyl"));
    assert(ModelTermShouldBeRemoved("factor(cyl):wt", "wt:cyl"));
    assert(!ModelTermShouldBeRemoved("hp:cyl", "wt"));
    assert(ModelTermShouldBeRemoved("am", "am"));
    assert(ModelTermShouldBeRemoved("mpg:am", "am"));
    assert(ModelTermShouldBeRemoved("am:gear", "am"));
    assert(!ModelTermShouldBeRemoved("mpg", "am"));
    assert(!ModelTermShouldBeRemoved("gear", "am"));
    assert(ModelTermShouldBeRemoved("I(wt^2):cyl", "I(wt^2)"));
    assert(!ModelTermShouldBeRemoved("wt", "I(wt^2)"));
    assert(!ModelTermShouldBeRemoved("I(wt^3)", "I(wt^2)"));
    assert(ModelTermShouldBeRemoved("I(wt^2)", "wt"));
    assert(ModelTermExistsForVariables(variables, "I(wt^2)", "mpg"));
    std::vector<std::string> removableTerms = {"wt", "cyl", "am", "wt:cyl", "cyl:am", "wt:cyl:am"};
    assert(RemoveModelTermCascade(removableTerms, "cyl:am"));
    assert(equal(removableTerms, {"wt", "cyl", "am", "wt:cyl"}));
    assert(RemoveModelTermCascade(removableTerms, "cyl"));
    assert(equal(removableTerms, {"wt", "am"}));

    std::vector<std::string> current = {"wt", "cyl", "hp"};
    std::vector<std::string> candidates = InteractionCandidateTermsForModel(
        variables, "mpg", current, current);
    assert(equal(candidates, {"wt:cyl", "wt:hp", "cyl:hp", "wt:cyl:hp"}));

    std::vector<std::string> afterBase = InteractionCandidateTermsForModel(
        variables, "mpg", current, current, "wt");
    assert(equal(afterBase, {"wt:cyl", "wt:hp", "wt:cyl:hp"}));

    std::vector<std::vector<std::string>> comparisonModels = {{"wt"}, {"cyl"}, {"hp"}};
    std::vector<std::string> comparisonCandidates = InteractionCandidateTermsForComparison(
        variables, "mpg", comparisonModels, {"wt"});
    assert(equal(comparisonCandidates, {"wt:cyl", "wt:hp", "cyl:hp", "wt:cyl:hp"}));
    std::vector<std::string> comparisonCandidatesForWt = InteractionCandidateTermsForComparison(
        variables, "mpg", comparisonModels, {"wt"}, "wt");
    assert(equal(comparisonCandidatesForWt, {"wt:cyl", "wt:hp", "wt:cyl:hp"}));
    std::vector<std::string> comparisonCandidatesAfterInteraction = InteractionCandidateTermsForComparison(
        variables, "mpg", comparisonModels, {"wt", "cyl", "wt:cyl"}, "wt");
    assert(equal(comparisonCandidatesAfterInteraction, {"wt:hp", "wt:cyl:hp"}));

    const auto interactionMenu = BuildInteractionMenuGroups(candidates);
    assert(interactionMenu.size() == 3);
    assert(interactionMenu[0].predictor == "wt");
    assert(interactionMenu[1].predictor == "cyl");
    assert(interactionMenu[2].predictor == "hp");
    assert(interactionMenu[0].orders.size() == 2);
    assert(interactionMenu[0].orders[0].order == 2);
    assert(interactionMenu[0].orders[0].choices.size() == 2);
    assert(interactionMenu[0].orders[0].choices[0].term == "wt:cyl");
    assert(interactionMenu[0].orders[0].choices[0].displayLabel == "wt × cyl");
    assert(interactionMenu[1].orders[0].choices[0].term == "cyl:wt");
    assert(interactionMenu[1].orders[0].choices[0].displayLabel == "cyl × wt");
    assert(interactionMenu[0].orders[1].order == 3);
    assert(interactionMenu[0].orders[1].choices[0].term == "wt:cyl:hp");

    const auto reversedAnchor = BuildInteractionMenuGroups(candidates, "cyl");
    assert(reversedAnchor[0].orders[0].choices[0].term == "cyl:wt");
    const auto reversedPrefix = BuildInteractionMenuGroups({"wt:cyl:hp"}, "hp:cyl");
    assert(reversedPrefix[0].orders[0].choices[0].term == "hp:cyl:wt");
    std::vector<std::string> chosenTerms{"wt", "cyl"};
    assert(AddHierarchicalTermsToVector({"wt", "cyl", "mpg"}, "mpg", chosenTerms,
        reversedAnchor[0].orders[0].choices[0].term));
    assert(chosenTerms.back() == "cyl:wt");
    assert(!AddHierarchicalTermsToVector({"wt", "cyl", "mpg"}, "mpg", chosenTerms, "wt:cyl"));

    const auto anchoredInteractionMenu = BuildInteractionMenuGroups(afterBase, "wt");
    assert(anchoredInteractionMenu.size() == 1);
    assert(anchoredInteractionMenu[0].predictor == "wt");
    assert(anchoredInteractionMenu[0].orders.size() == 2);
    assert(anchoredInteractionMenu[0].orders[0].choices.size() == 2);
    assert(anchoredInteractionMenu[0].orders[0].choices[0].displayLabel == "cyl");
    assert(anchoredInteractionMenu[0].orders[0].choices[1].displayLabel == "hp");
    assert(anchoredInteractionMenu[0].orders[1].choices[0].displayLabel == "wt × cyl × hp");

    const std::vector<std::string> fourMainEffects = {"wt", "cyl", "hp", "am"};
    const std::vector<std::string> afterInteraction = InteractionCandidateTermsForModel(
        variables, "mpg", fourMainEffects,
        {"wt", "cyl", "hp", "am", "wt:cyl"}, "wt:cyl");
    assert(equal(afterInteraction,
        {"wt:cyl:hp", "wt:cyl:am", "wt:cyl:hp:am"}));
    const auto interactionAnchoredMenu =
        BuildInteractionMenuGroups(afterInteraction, "wt:cyl");
    assert(interactionAnchoredMenu.size() == 1);
    assert(interactionAnchoredMenu[0].predictor == "wt × cyl");
    assert(interactionAnchoredMenu[0].orders.size() == 2);
    assert(interactionAnchoredMenu[0].orders[0].order == 3);
    assert(interactionAnchoredMenu[0].orders[0].choices[0].displayLabel == "hp");
    assert(interactionAnchoredMenu[0].orders[0].choices[1].displayLabel == "am");
    assert(interactionAnchoredMenu[0].orders[1].order == 4);
    assert(interactionAnchoredMenu[0].orders[1].choices[0].displayLabel ==
           "wt × cyl × hp × am");

    std::vector<std::string> hierarchy = HierarchicalTermsForModelTerm(variables, "mpg", "wt:cyl:hp");
    assert(equal(hierarchy, {"wt", "cyl", "hp", "wt:cyl", "wt:hp", "cyl:hp", "wt:cyl:hp"}));

    std::vector<std::string> terms = {"wt"};
    assert(AddHierarchicalTermsToVector(variables, "mpg", terms, "cyl:hp"));
    assert(equal(terms, {"wt", "cyl", "hp", "cyl:hp"}));
    assert(!AddHierarchicalTermsToVector(variables, "mpg", terms, "hp:cyl"));
    assert(TermListContainsEquivalentModelTerm(terms, "hp:cyl"));
    RenameVariableInTermList(terms, "cyl", "cylinders");
    assert(equal(terms, {"wt", "cylinders", "hp", "cylinders:hp"}));

    std::map<std::string, std::string> termTypes;
    termTypes["wt"] = "factor";
    termTypes["hp"] = "unsupported";
    assert(ResolvedModelTermDisplayType("wt:cyl", termTypes, "numeric") == "interaction");
    assert(ResolvedModelTermDisplayType("wt", termTypes, "numeric") == "factor");
    assert(ResolvedModelTermDisplayType("hp", termTypes, "numeric") == "numeric");
    assert(ResolvedModelTermDisplayType("am", termTypes, "factor") == "factor");
    assert(ModelTermTypeDisplayName("interaction") == "Interaction");
    assert(SemanticInteractionTermType({"factor", "factor"}) == "factor_factor_interaction");
    assert(SemanticInteractionTermType({"numeric", "factor"}) == "numeric_factor_interaction");
    assert(SemanticInteractionTermType({"numeric", "numeric"}) == "numeric_numeric_interaction");
    assert(SemanticInteractionTermType({"numeric", "factor", "factor"}) == "higher_order_interaction");
    assert(ModelTermTypeIsInteraction("factor_factor_interaction"));
    assert(ModelTermTypeIsInteraction("interaction"));
    assert(!ModelTermTypeIsInteraction("factor"));
    assert(ModelTermTypeDisplayName("factor_factor_interaction") == "Categorical x Categorical");
    assert(ModelTermTypeDisplayName("numeric_factor_interaction") == "Numeric x Categorical");
    assert(ModelTermTypeDisplayName("numeric_numeric_interaction") == "Numeric x Numeric");
    assert(ModelTermTypeDisplayName("factor") == "Categorical");
    assert(ModelTermTypeDisplayName("ordered") == "Ordinal");
    assert(ModelTermTypeDisplayName("numeric") == "Numeric");
    assert(ModelTermTypeDisplayName("unknown") == "Numeric");

    std::vector<std::string> bases = BaseVariablesFromModelTerms(
        variables, "mpg", {"wt", "factor(cyl)", "wt:cyl", "unknown", "mpg"});
    assert(equal(bases, {"wt", "cyl"}));

    assert(VariableRoleForModelContext("mpg", "mpg", {"wt"}, "wt", "mpg") == "Y");
    assert(VariableRoleForModelContext("wt", "mpg", {"wt", "wt:cyl"}, "", "") == "Predictor");
    assert(VariableRoleForModelContext("wt", "", {}, "wt", "mpg") == "X");
    assert(VariableRoleForModelContext("mpg", "", {}, "wt", "mpg") == "Y");
    assert(VariableRoleForModelContext("hp", "", {}, "wt", "mpg").empty());

    // A single semantic model specification now owns hierarchy, predictor
    // interpretation and transformations for both GLM entry points.
    const auto linearCapabilities = CapabilitiesForModelFamily(ModelFamilyKind::Linear);
    assert(linearCapabilities.supportsNumericPredictors);
    assert(linearCapabilities.supportsFactorPredictors);
    assert(linearCapabilities.supportsReferenceLevels);
    assert(linearCapabilities.supportsCentering);
    assert(linearCapabilities.supportsInteractions);
    const auto generalizedCapabilities =
        CapabilitiesForModelFamily(ModelFamilyKind::GeneralizedLinear);
    assert(generalizedCapabilities.supportsNumericPredictors);
    assert(generalizedCapabilities.supportsFactorPredictors);
    assert(generalizedCapabilities.supportsReferenceLevels);
    assert(generalizedCapabilities.supportsCentering);
    assert(generalizedCapabilities.supportsInteractions);

    ModelSpecification specification;
    assert(SetModelSpecificationResponse(specification, "mpg").changed);
    assert(AddModelSpecificationTerm(specification, variables, "wt:cyl").changed);
    assert(equal(specification.terms, {"wt", "cyl", "wt:cyl"}));
    assert(ModelSpecificationIncludesTerm(specification, "cyl:wt"));
    assert(ModelSpecificationTermType(specification, "wt:cyl") ==
           "numeric_numeric_interaction");

    ModelPredictorMetadata cylinderMetadata;
    cylinderMetadata.variable = "cyl";
    cylinderMetadata.storageType = "numeric";
    cylinderMetadata.observedCount = 32;
    cylinderMetadata.uniqueCount = 3;
    cylinderMetadata.levels = {"4", "6", "8"};
    auto factorEdit = SetModelSpecificationTermType(
        specification, "cyl", "factor", cylinderMetadata);
    assert(factorEdit.ok && factorEdit.changed);
    assert(ModelSpecificationTermType(specification, "cyl") == "factor");
    assert(ModelSpecificationTermType(specification, "wt:cyl") ==
           "numeric_factor_interaction");
    // Dataset metadata may be refreshed after a model-level override.  The
    // canonical effective map must keep the explicit interpretation instead
    // of silently reverting the predictor to its storage type.
    specification.termTypes["cyl"] = "numeric";
    const auto effectiveTypes = EffectiveModelSpecificationTermTypes(specification);
    assert(effectiveTypes.at("cyl") == "factor");
    assert(ModelSpecificationTermType(specification, "wt:cyl") ==
           "numeric_factor_interaction");
    assert(!SetModelSpecificationPredictorCentered(specification, "cyl", true).ok);
    auto referenceEdit = SetModelSpecificationReferenceLevel(
        specification, "cyl", "6", cylinderMetadata.levels);
    assert(referenceEdit.ok && referenceEdit.changed);
    assert(ResolveModelTermSpecification(specification, "cyl").referenceLevel == "6");

    ModelPredictorMetadata weightMetadata;
    weightMetadata.variable = "wt";
    weightMetadata.storageType = "numeric";
    weightMetadata.observedCount = 32;
    weightMetadata.uniqueCount = 29;
    auto centerEdit = SetModelSpecificationPredictorCentered(specification, "wt", true);
    assert(centerEdit.ok && centerEdit.changed);
    auto centered = ResolveModelTermSpecification(specification, "wt", {{"wt", 3.217}});
    assert(centered.centered);
    assert(std::fabs(centered.centeringConstant - 3.217) < 1e-12);
    auto highCardinalityFactor = SetModelSpecificationTermType(
        specification, "wt", "factor", weightMetadata);
    assert(!highCardinalityFactor.ok && !highCardinalityFactor.changed);
    assert(ModelSpecificationTermType(specification, "wt") == "numeric");

    ModelSpecification copied = specification;
    assert(EquivalentModelSpecifications(specification, copied));
    assert(RemoveModelSpecificationTerm(copied, "cyl").changed);
    assert(equal(copied.terms, {"wt"}));
    assert(copied.factorReferenceLevels.empty());
    assert(!EquivalentModelSpecifications(specification, copied));

    ModelSpecification replacement;
    replacement.response = "mpg";
    replacement.terms = {"wt", "cyl", "wt:cyl"};
    replacement.centeredPredictors.insert("wt");
    assert(ReplaceModelSpecificationTerm(replacement, variables, "wt", "hp").changed);
    assert(equal(replacement.terms, {"hp", "cyl", "hp:cyl"}));
    assert(replacement.centeredPredictors.empty());

    ModelSpecification responseChange = specification;
    assert(SetModelSpecificationResponse(responseChange, "wt").changed);
    assert(responseChange.response == "wt");
    assert(equal(responseChange.terms, {"cyl"}));
    assert(responseChange.centeredPredictors.empty());

    ModelSpecification factorResponseChange = specification;
    assert(SetModelSpecificationResponse(factorResponseChange, "cyl").changed);
    assert(factorResponseChange.response == "cyl");
    assert(equal(factorResponseChange.terms, {"wt"}));
    assert(factorResponseChange.factorReferenceLevels.count("cyl") == 0);
    assert(factorResponseChange.termTypeOverrides.count("cyl") == 0);
    assert(std::none_of(factorResponseChange.terms.begin(),
                       factorResponseChange.terms.end(),
                       [](const std::string &term) {
                           return ModelTermShouldBeRemoved(term, "cyl");
                       }));

    // Changing the dependent variable in the MI city example must remove it
    // from both a direct predictor and every interaction before the new fit is
    // queued.  This prevents the diagnostic from being fitted as y ~ y + ... .
    ModelSpecification cityResponseChange;
    cityResponseChange.response = "monthly_rent_eur";
    cityResponseChange.terms = {
        "apartment_price_eur_m2", "region",
        "apartment_price_eur_m2:region"};
    cityResponseChange.termTypes = {
        {"apartment_price_eur_m2", "numeric"}, {"region", "factor"}};
    cityResponseChange.termTypeOverrides = {{"region", "factor"}};
    cityResponseChange.centeredPredictors.insert("apartment_price_eur_m2");
    assert(SetModelSpecificationResponse(
        cityResponseChange, "apartment_price_eur_m2").changed);
    assert(cityResponseChange.response == "apartment_price_eur_m2");
    assert(equal(cityResponseChange.terms, {"region"}));
    assert(cityResponseChange.termTypes.count("apartment_price_eur_m2") == 0);
    assert(cityResponseChange.centeredPredictors.empty());

    // Binary Regression uses the same model-local specification edits as the
    // other GLM frontends. Numeric worksheet storage must remain only a
    // default while explicit factor interpretations survive together.
    ModelSpecification binarySpecification;
    binarySpecification.familyKind = ModelFamilyKind::GeneralizedLinear;
    assert(SetModelSpecificationResponse(binarySpecification, "mpg").changed);
    assert(AddModelSpecificationTerm(binarySpecification, variables, "hp").changed);
    assert(AddModelSpecificationTerm(binarySpecification, variables, "am:cyl").changed);
    assert(equal(binarySpecification.terms, {"hp", "am", "cyl", "am:cyl"}));

    ModelPredictorMetadata transmissionMetadata;
    transmissionMetadata.variable = "am";
    transmissionMetadata.storageType = "numeric";
    transmissionMetadata.observedCount = 32;
    transmissionMetadata.uniqueCount = 2;
    transmissionMetadata.levels = {"0", "1"};

    // Reproduce the former macOS split state after an initial numeric fit:
    // changing only the dataset-derived cache cannot supersede the explicit
    // model-local interpretation returned by R.
    assert(SetModelSpecificationTermType(
        binarySpecification, "am", "numeric", transmissionMetadata).changed);
    binarySpecification.termTypes["am"] = "factor";
    assert(binarySpecification.termTypeOverrides.at("am") == "numeric");
    assert(ModelSpecificationTermType(binarySpecification, "am") == "numeric");
    assert(EffectiveModelSpecificationTermTypes(binarySpecification).at("am") ==
           "numeric");

    assert(SetModelSpecificationTermType(
        binarySpecification, "am", "factor", transmissionMetadata).changed);
    assert(SetModelSpecificationTermType(
        binarySpecification, "cyl", "factor", cylinderMetadata).changed);
    assert(binarySpecification.termTypeOverrides.at("am") == "factor");
    assert(binarySpecification.termTypeOverrides.at("cyl") == "factor");
    const auto binaryFactorTypes =
        EffectiveModelSpecificationTermTypes(binarySpecification);
    assert(binaryFactorTypes.at("am") == "factor");
    assert(binaryFactorTypes.at("cyl") == "factor");
    assert(ModelSpecificationTermType(binarySpecification, "am:cyl") ==
           "factor_factor_interaction");
    assert(SetModelSpecificationReferenceLevel(
        binarySpecification, "am", "1", transmissionMetadata.levels).changed);
    assert(SetModelSpecificationReferenceLevel(
        binarySpecification, "cyl", "6", cylinderMetadata.levels).changed);

    // The semantic edit is reversible on the same model and removes stale
    // factor-only properties.
    assert(SetModelSpecificationTermType(
        binarySpecification, "am", "numeric", transmissionMetadata).changed);
    assert(binarySpecification.factorReferenceLevels.count("am") == 0);
    assert(ModelSpecificationTermType(binarySpecification, "am") == "numeric");
    assert(ModelSpecificationTermType(binarySpecification, "am:cyl") ==
           "numeric_factor_interaction");
    assert(SetModelSpecificationTermType(
        binarySpecification, "cyl", "numeric", cylinderMetadata).changed);
    assert(binarySpecification.factorReferenceLevels.empty());
    assert(ModelSpecificationTermType(binarySpecification, "am:cyl") ==
           "numeric_numeric_interaction");

    return 0;
}
