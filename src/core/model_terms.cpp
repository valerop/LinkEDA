#include "model_terms.h"

#include "string_utils.h"

#include <algorithm>
#include <cmath>
#include <iomanip>
#include <set>
#include <sstream>

namespace rlispstat {
namespace core {

namespace {

bool StringVectorContains(const std::vector<std::string> &values, const std::string &value)
{
    return std::find(values.begin(), values.end(), value) != values.end();
}

bool ContainsAllVariables(const std::vector<std::string> &candidate,
                          const std::vector<std::string> &required)
{
    for (const std::string &variable : required) {
        if (!StringVectorContains(candidate, variable)) {
            return false;
        }
    }
    return true;
}

std::set<std::string> BaseVariableSetForTerm(const std::string &term)
{
    std::set<std::string> bases;
    for (const std::string &part : SplitInteractionTerm(term)) {
        std::string base = BaseVariableForTermComponent(part);
        if (!base.empty()) {
            bases.insert(base);
        }
    }
    return bases;
}

void GenerateVariableCombinations(const std::vector<std::string> &variables,
                                  std::size_t wanted,
                                  std::size_t start,
                                  std::vector<std::string> &current,
                                  std::vector<std::vector<std::string>> &out)
{
    if (current.size() == wanted) {
        out.push_back(current);
        return;
    }
    for (std::size_t i = start; i < variables.size(); ++i) {
        current.push_back(variables[i]);
        GenerateVariableCombinations(variables, wanted, i + 1, current, out);
        current.pop_back();
    }
}

} // namespace

std::vector<std::string> SplitInteractionTerm(const std::string &term)
{
    std::vector<std::string> parts;
    std::size_t start = 0;
    while (start <= term.size()) {
        std::size_t pos = term.find(':', start);
        std::string part = TrimCopy(term.substr(start, pos == std::string::npos
            ? std::string::npos
            : pos - start));
        if (!part.empty()) {
            parts.push_back(part);
        }
        if (pos == std::string::npos) {
            break;
        }
        start = pos + 1;
    }
    return parts;
}

bool IsInteractionTerm(const std::string &term)
{
    return SplitInteractionTerm(term).size() >= 2;
}

std::string BaseVariableForTermComponent(const std::string &term)
{
    const std::string prefix = "factor(";
    std::string trimmed = TrimCopy(term);
    if (trimmed.rfind(prefix, 0) == 0 &&
        trimmed.size() > prefix.size() + 1 &&
        trimmed.back() == ')') {
        return TrimCopy(trimmed.substr(prefix.size(), trimmed.size() - prefix.size() - 1));
    }
    return trimmed;
}

std::vector<std::string> UniqueBaseVariablesForTerm(const std::string &term)
{
    std::vector<std::string> variables;
    for (const std::string &part : SplitInteractionTerm(term)) {
        std::string variable = BaseVariableForTermComponent(part);
        if (!variable.empty() && variable != "(Intercept)" &&
            !StringVectorContains(variables, variable)) {
            variables.push_back(variable);
        }
    }
    return variables;
}

std::string InteractionTermFromParts(const std::vector<std::string> &parts)
{
    std::ostringstream out;
    for (std::size_t i = 0; i < parts.size(); ++i) {
        if (i) {
            out << ":";
        }
        out << parts[i];
    }
    return out.str();
}

std::string RenameVariableInTerm(const std::string &term,
                                 const std::string &oldName,
                                 const std::string &newName)
{
    std::vector<std::string> renamed;
    for (const std::string &part : SplitInteractionTerm(term)) {
        std::string trimmed = TrimCopy(part);
        if (BaseVariableForTermComponent(trimmed) == oldName) {
            const std::string prefix = "factor(";
            if (trimmed.rfind(prefix, 0) == 0 &&
                trimmed.size() > prefix.size() + 1 &&
                trimmed.back() == ')') {
                renamed.push_back("factor(" + newName + ")");
            } else {
                renamed.push_back(newName);
            }
        } else {
            renamed.push_back(trimmed);
        }
    }
    return InteractionTermFromParts(renamed);
}

void RenameVariableInTermList(std::vector<std::string> &terms,
                              const std::string &oldName,
                              const std::string &newName)
{
    for (std::string &term : terms) {
        term = RenameVariableInTerm(term, oldName, newName);
    }
}

bool EquivalentInteractionTerm(const std::string &a, const std::string &b)
{
    std::vector<std::string> left = SplitInteractionTerm(a);
    std::vector<std::string> right = SplitInteractionTerm(b);
    if (left.size() < 2 || right.size() < 2 || left.size() != right.size()) {
        return false;
    }
    for (std::string &part : left) {
        part = BaseVariableForTermComponent(part);
    }
    for (std::string &part : right) {
        part = BaseVariableForTermComponent(part);
    }
    std::sort(left.begin(), left.end());
    std::sort(right.begin(), right.end());
    return left == right;
}

bool EquivalentModelTerm(const std::string &a, const std::string &b)
{
    const bool leftInteraction = IsInteractionTerm(a);
    const bool rightInteraction = IsInteractionTerm(b);
    if (leftInteraction || rightInteraction) {
        return leftInteraction && rightInteraction && EquivalentInteractionTerm(a, b);
    }
    return a == b;
}

std::string ModelTermWithoutLevelSuffixes(const std::string &term)
{
    std::vector<std::string> stripped;
    for (const std::string &part : SplitInteractionTerm(term)) {
        std::string trimmed = TrimCopy(part);
        std::size_t equals = trimmed.find('=');
        if (equals != std::string::npos) {
            trimmed = TrimCopy(trimmed.substr(0, equals));
        }
        if (!trimmed.empty()) {
            stripped.push_back(trimmed);
        }
    }
    return InteractionTermFromParts(stripped);
}

std::string FactorLevelTermValue(double value)
{
    if (!std::isfinite(value)) {
        return "";
    }
    std::ostringstream out;
    out << std::setprecision(12) << value;
    return out.str();
}

std::string ModelTermBaseForDisplayRow(const std::string &term)
{
    std::string stripped = ModelTermWithoutLevelSuffixes(term);
    return stripped.empty() ? BaseVariableForTermComponent(term) : stripped;
}

bool ModelTermShouldBeRemoved(const std::string &candidate, const std::string &removed)
{
    if (EquivalentModelTerm(candidate, removed)) {
        return true;
    }
    std::set<std::string> candidateBases = BaseVariableSetForTerm(candidate);
    std::set<std::string> removedBases = BaseVariableSetForTerm(removed);
    if (candidateBases.empty() || removedBases.empty() ||
        candidateBases.size() < removedBases.size()) {
        return false;
    }
    for (const std::string &base : removedBases) {
        if (candidateBases.find(base) == candidateBases.end()) {
            return false;
        }
    }
    return true;
}

bool RemoveModelTermCascade(std::vector<std::string> &terms,
                            const std::string &removed)
{
    std::size_t before = terms.size();
    terms.erase(std::remove_if(terms.begin(), terms.end(),
                               [&](const std::string &candidate) {
                                   return ModelTermShouldBeRemoved(candidate, removed);
                               }),
                terms.end());
    return terms.size() != before;
}

bool TermListContainsEquivalentModelTerm(const std::vector<std::string> &terms,
                                         const std::string &candidate)
{
    for (const std::string &term : terms) {
        if (EquivalentModelTerm(term, candidate)) {
            return true;
        }
    }
    return false;
}

bool ModelTermExistsForVariables(const std::vector<std::string> &availableVariables,
                                 const std::string &term,
                                 const std::string &response)
{
    std::vector<std::string> parts = SplitInteractionTerm(term);
    if (parts.empty()) {
        return false;
    }
    for (const std::string &part : parts) {
        std::string variable = BaseVariableForTermComponent(part);
        if (variable.empty() || variable == response ||
            !StringVectorContains(availableVariables, variable)) {
            return false;
        }
    }
    return true;
}

std::vector<std::string> FilterModelTermsForResponse(const std::vector<std::string> &availableVariables,
                                                     const std::vector<std::string> &terms,
                                                     const std::string &response)
{
    std::vector<std::string> kept;
    for (const std::string &term : terms) {
        if (!ModelTermExistsForVariables(availableVariables, term, response)) {
            continue;
        }
        if (!TermListContainsEquivalentModelTerm(kept, term)) {
            kept.push_back(term);
        }
    }
    return kept;
}

std::vector<std::string> BaseVariablesFromModelTerms(const std::vector<std::string> &availableVariables,
                                                     const std::string &response,
                                                     const std::vector<std::string> &modelTerms)
{
    std::vector<std::string> variables;
    for (const std::string &term : modelTerms) {
        if (term.empty() || term == "(Intercept)") {
            continue;
        }
        for (const std::string &variable : UniqueBaseVariablesForTerm(term)) {
            if (variable == response || !StringVectorContains(availableVariables, variable)) {
                continue;
            }
            if (!StringVectorContains(variables, variable)) {
                variables.push_back(variable);
            }
        }
    }
    return variables;
}

std::vector<std::string> InteractionCandidateTermsForModel(const std::vector<std::string> &availableVariables,
                                                           const std::string &response,
                                                           const std::vector<std::string> &modelTerms,
                                                           const std::vector<std::string> &existingTerms,
                                                           const std::string &baseTerm)
{
    std::vector<std::string> variables = BaseVariablesFromModelTerms(availableVariables, response, modelTerms);
    std::vector<std::string> required = UniqueBaseVariablesForTerm(baseTerm);
    if (!required.empty() && !ContainsAllVariables(variables, required)) {
        return {};
    }

    std::vector<std::string> candidates;
    auto maybeAdd = [&](const std::vector<std::string> &parts) {
        if (parts.size() < 2) {
            return;
        }
        if (!required.empty() && !ContainsAllVariables(parts, required)) {
            return;
        }
        std::string term = InteractionTermFromParts(parts);
        if (!ModelTermExistsForVariables(availableVariables, term, response)) {
            return;
        }
        if (TermListContainsEquivalentModelTerm(existingTerms, term) ||
            TermListContainsEquivalentModelTerm(candidates, term)) {
            return;
        }
        candidates.push_back(term);
    };

    for (std::size_t order = 2; order <= variables.size(); ++order) {
        std::vector<std::vector<std::string>> combinations;
        std::vector<std::string> current;
        GenerateVariableCombinations(variables, order, 0, current, combinations);
        for (const std::vector<std::string> &parts : combinations) {
            maybeAdd(parts);
        }
    }
    return candidates;
}

std::vector<std::string> InteractionCandidateTermsForComparison(
    const std::vector<std::string> &availableVariables,
    const std::string &response,
    const std::vector<std::vector<std::string>> &comparisonModelTerms,
    const std::vector<std::string> &activeModelTerms,
    const std::string &baseTerm)
{
    std::vector<std::string> comparisonTerms;
    for (const std::vector<std::string> &modelTerms : comparisonModelTerms) {
        for (const std::string &term : modelTerms) {
            if (term.empty() || term == "(Intercept)" ||
                TermListContainsEquivalentModelTerm(comparisonTerms, term)) {
                continue;
            }
            comparisonTerms.push_back(term);
        }
    }
    return InteractionCandidateTermsForModel(
        availableVariables, response, comparisonTerms, activeModelTerms, baseTerm);
}

std::vector<std::string> HierarchicalTermsForModelTerm(const std::vector<std::string> &availableVariables,
                                                       const std::string &response,
                                                       const std::string &term)
{
    std::vector<std::string> hierarchy;
    if (!IsInteractionTerm(term)) {
        if (ModelTermExistsForVariables(availableVariables, term, response)) {
            hierarchy.push_back(term);
        }
        return hierarchy;
    }

    std::vector<std::string> variables = UniqueBaseVariablesForTerm(term);
    for (std::size_t order = 1; order <= variables.size(); ++order) {
        std::vector<std::vector<std::string>> combinations;
        std::vector<std::string> current;
        GenerateVariableCombinations(variables, order, 0, current, combinations);
        for (const std::vector<std::string> &parts : combinations) {
            std::string candidate = parts.size() == 1
                ? parts.front()
                : InteractionTermFromParts(parts);
            if (ModelTermExistsForVariables(availableVariables, candidate, response) &&
                !TermListContainsEquivalentModelTerm(hierarchy, candidate)) {
                hierarchy.push_back(candidate);
            }
        }
    }
    return hierarchy;
}

bool AddHierarchicalTermsToVector(const std::vector<std::string> &availableVariables,
                                  const std::string &response,
                                  std::vector<std::string> &terms,
                                  const std::string &term)
{
    bool changed = false;
    for (const std::string &candidate : HierarchicalTermsForModelTerm(availableVariables, response, term)) {
        if (candidate == response) {
            continue;
        }
        if (!TermListContainsEquivalentModelTerm(terms, candidate)) {
            terms.push_back(candidate);
            changed = true;
        }
    }
    return changed;
}

std::string ResolvedModelTermDisplayType(const std::string &term,
                                         const std::map<std::string, std::string> &overrides,
                                         const std::string &fallbackType)
{
    if (IsInteractionTerm(term)) {
        return "interaction";
    }
    auto it = overrides.find(term);
    if (it != overrides.end() && (it->second == "numeric" || it->second == "factor")) {
        return it->second;
    }
    return fallbackType == "factor" ? "factor" : "numeric";
}

std::string SemanticInteractionTermType(const std::vector<std::string> &componentTypes)
{
    if (componentTypes.size() < 2) {
        return "interaction";
    }
    if (componentTypes.size() > 2) {
        return "higher_order_interaction";
    }
    int factorCount = 0;
    for (const std::string &type : componentTypes) {
        if (type == "factor") {
            factorCount += 1;
        }
    }
    if (factorCount == 2) {
        return "factor_factor_interaction";
    }
    if (factorCount == 1) {
        return "numeric_factor_interaction";
    }
    return "numeric_numeric_interaction";
}

bool ModelTermTypeIsInteraction(const std::string &type)
{
    return type == "interaction" ||
        type == "numeric_numeric_interaction" ||
        type == "numeric_factor_interaction" ||
        type == "factor_factor_interaction" ||
        type == "higher_order_interaction";
}

std::string ModelTermTypeDisplayName(const std::string &type)
{
    if (type == "numeric_numeric_interaction") {
        return "Numeric x Numeric";
    }
    if (type == "numeric_factor_interaction") {
        return "Numeric x Factor";
    }
    if (type == "factor_factor_interaction") {
        return "Factor x Factor";
    }
    if (type == "higher_order_interaction" || type == "interaction") {
        return "Interaction";
    }
    return type == "factor" ? "Factor" : "Numeric";
}

std::string VariableRoleForModelContext(const std::string &variable,
                                        const std::string &dependent,
                                        const std::vector<std::string> &terms,
                                        const std::string &xVariable,
                                        const std::string &yVariable)
{
    if (!dependent.empty() && dependent == variable) {
        return "Y";
    }
    if (TermListContainsEquivalentModelTerm(terms, variable)) {
        return "Predictor";
    }
    if (!xVariable.empty() && xVariable == variable) {
        return "X";
    }
    if (!yVariable.empty() && yVariable == variable) {
        return "Y";
    }
    return "";
}

} // namespace core
} // namespace rlispstat
