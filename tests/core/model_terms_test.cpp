#include "../../src/core/model_terms.h"

#include <cassert>
#include <limits>
#include <map>
#include <string>
#include <vector>

using rlispstat::core::AddHierarchicalTermsToVector;
using rlispstat::core::BaseVariableForTermComponent;
using rlispstat::core::BaseVariablesFromModelTerms;
using rlispstat::core::EquivalentModelTerm;
using rlispstat::core::FactorLevelTermValue;
using rlispstat::core::FilterModelTermsForResponse;
using rlispstat::core::HierarchicalTermsForModelTerm;
using rlispstat::core::InteractionCandidateTermsForModel;
using rlispstat::core::InteractionCandidateTermsForComparison;
using rlispstat::core::IsInteractionTerm;
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

static bool equal(const std::vector<std::string> &a, const std::vector<std::string> &b)
{
    return a == b;
}

int main()
{
    std::vector<std::string> variables = {"mpg", "wt", "hp", "cyl", "am"};

    assert(equal(SplitInteractionTerm(" wt : factor(cyl) "), {"wt", "factor(cyl)"}));
    assert(IsInteractionTerm("wt:cyl"));
    assert(!IsInteractionTerm("wt"));
    assert(BaseVariableForTermComponent(" factor(cyl) ") == "cyl");
    assert(equal(UniqueBaseVariablesForTerm("wt:factor(cyl):wt"), {"wt", "cyl"}));
    assert(RenameVariableInTerm("wt:factor(cyl)", "cyl", "cylinders") == "wt:factor(cylinders)");
    assert(RenameVariableInTerm(" wt : cyl ", "wt", "weight") == "weight:cyl");

    assert(EquivalentModelTerm("wt:cyl", "factor(cyl):wt"));
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
    assert(ModelTermTypeDisplayName("factor_factor_interaction") == "Factor x Factor");
    assert(ModelTermTypeDisplayName("numeric_factor_interaction") == "Numeric x Factor");
    assert(ModelTermTypeDisplayName("numeric_numeric_interaction") == "Numeric x Numeric");
    assert(ModelTermTypeDisplayName("factor") == "Factor");
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

    return 0;
}
