#include "model_terms.h"

#include "string_utils.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <iomanip>
#include <set>
#include <sstream>

namespace rlispstat {
namespace core {

std::vector<int> ModelSpecificationSelectionRows(
    const ModelSpecification &specification,
    const std::vector<int> &currentSelectedRows)
{
    if (specification.dataScopeCaptured &&
        specification.dataScope.kind == AnalysisScopeKind::ExplicitRowIds) {
        return ResolveAnalysisScopeRowIds(
            specification.dataScope,
            specification.dataScope.totalDatasetRows);
    }
    if (!specification.dataScopeCaptured &&
        (specification.scope == "selected" ||
         specification.scope == "unselected")) {
        std::vector<int> rows = currentSelectedRows;
        rows.erase(std::remove_if(rows.begin(), rows.end(),
            [](int row) { return row <= 0; }), rows.end());
        std::sort(rows.begin(), rows.end());
        rows.erase(std::unique(rows.begin(), rows.end()), rows.end());
        return rows;
    }
    return {};
}

std::string ModelSpecificationFitScope(
    const ModelSpecification &specification)
{
    if (specification.dataScopeCaptured &&
        specification.dataScope.kind == AnalysisScopeKind::ExplicitRowIds) {
        return "selected";
    }
    if (specification.scope.empty() || specification.scope == "compare_selected_all") {
        return "all";
    }
    return specification.scope;
}

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

ModelFamilyCapabilities CapabilitiesForModelFamily(ModelFamilyKind family)
{
    switch (family) {
    case ModelFamilyKind::Linear:
    case ModelFamilyKind::GeneralizedLinear:
        return {true, true, true, true, true};
    }
    return {};
}

std::string ModelSpecificationTermType(const ModelSpecification &specification,
                                       const std::string &term,
                                       const std::string &fallbackType)
{
    if (IsInteractionTerm(term)) {
        std::vector<std::string> componentTypes;
        for (const std::string &variable : UniqueBaseVariablesForTerm(term)) {
            componentTypes.push_back(ModelSpecificationTermType(
                specification, variable, fallbackType));
        }
        return SemanticInteractionTermType(componentTypes);
    }
    const std::string variable = BaseVariableForTermComponent(term);
    auto explicitType = specification.termTypeOverrides.find(variable);
    if (explicitType != specification.termTypeOverrides.end() &&
        (explicitType->second == "numeric" || explicitType->second == "factor")) {
        return explicitType->second;
    }
    auto effectiveType = specification.termTypes.find(variable);
    if (effectiveType != specification.termTypes.end() &&
        (effectiveType->second == "numeric" || effectiveType->second == "factor")) {
        return effectiveType->second;
    }
    return fallbackType == "factor" ? "factor" : "numeric";
}

std::map<std::string, std::string> EffectiveModelSpecificationTermTypes(
    const ModelSpecification &specification)
{
    std::map<std::string, std::string> effective = specification.termTypes;
    std::set<std::string> variables;
    for (const std::string &term : specification.terms) {
        const auto bases = UniqueBaseVariablesForTerm(term);
        variables.insert(bases.begin(), bases.end());
    }
    for (const auto &entry : specification.termTypeOverrides) {
        variables.insert(entry.first);
    }
    for (const std::string &variable : variables) {
        effective[variable] = ModelSpecificationTermType(specification, variable);
    }
    return effective;
}

ModelTermSpecification ResolveModelTermSpecification(
    const ModelSpecification &specification,
    const std::string &term,
    const std::map<std::string, double> &centeringConstants)
{
    ModelTermSpecification resolved;
    resolved.term = term;
    resolved.variables = UniqueBaseVariablesForTerm(term);
    resolved.type = ModelSpecificationTermType(specification, term);
    if (resolved.variables.size() == 1) {
        const std::string &variable = resolved.variables.front();
        auto reference = specification.factorReferenceLevels.find(variable);
        if (reference != specification.factorReferenceLevels.end()) {
            resolved.referenceLevel = reference->second;
        }
        resolved.centered = specification.centeredPredictors.count(variable) != 0;
        auto constant = centeringConstants.find(variable);
        if (constant != centeringConstants.end()) {
            resolved.centeringConstant = constant->second;
        }
    }
    return resolved;
}

bool ModelSpecificationIncludesTerm(const ModelSpecification &specification,
                                    const std::string &term)
{
    return TermListContainsEquivalentModelTerm(specification.terms, term);
}

namespace {

void RemoveModelSpecificationProperties(ModelSpecification &specification,
                                        const std::string &removed)
{
    for (auto it = specification.termTypes.begin(); it != specification.termTypes.end();) {
        if (ModelTermShouldBeRemoved(it->first, removed)) it = specification.termTypes.erase(it);
        else ++it;
    }
    for (auto it = specification.termTypeOverrides.begin();
         it != specification.termTypeOverrides.end();) {
        if (ModelTermShouldBeRemoved(it->first, removed)) {
            it = specification.termTypeOverrides.erase(it);
        } else {
            ++it;
        }
    }
    for (auto it = specification.centeredPredictors.begin();
         it != specification.centeredPredictors.end();) {
        if (ModelTermShouldBeRemoved(*it, removed)) it = specification.centeredPredictors.erase(it);
        else ++it;
    }
    for (auto it = specification.factorReferenceLevels.begin();
         it != specification.factorReferenceLevels.end();) {
        if (ModelTermShouldBeRemoved(it->first, removed)) {
            it = specification.factorReferenceLevels.erase(it);
        } else {
            ++it;
        }
    }
}

bool ValidPredictorType(const std::string &type)
{
    return type == "numeric" || type == "factor";
}

} // namespace

ModelSpecificationEditResult SetModelSpecificationResponse(
    ModelSpecification &specification,
    const std::string &response)
{
    if (response.empty()) return {false, false, "Response variable is required."};
    if (specification.response == response) return {true, false, {}};
    specification.response = response;
    RemoveModelTermCascade(specification.terms, response);
    RemoveModelSpecificationProperties(specification, response);
    PruneModelSpecificationState(specification);
    return {true, true, {}};
}

ModelSpecificationEditResult AddModelSpecificationTerm(
    ModelSpecification &specification,
    const std::vector<std::string> &availableVariables,
    const std::string &term)
{
    if (!CapabilitiesForModelFamily(specification.familyKind).supportsInteractions &&
        IsInteractionTerm(term)) {
        return {false, false, "This model family does not support interactions."};
    }
    if (!ModelTermExistsForVariables(availableVariables, term, specification.response)) {
        return {false, false, "predictor must be an available variable"};
    }
    const bool changed = AddHierarchicalTermsToVector(
        availableVariables, specification.response, specification.terms, term);
    PruneModelSpecificationState(specification);
    return {true, changed, {}};
}

ModelSpecificationEditResult RemoveModelSpecificationTerm(
    ModelSpecification &specification,
    const std::string &term)
{
    if (term.empty()) return {false, false, "Model term is required."};
    const bool changed = RemoveModelTermCascade(specification.terms, term);
    if (changed) RemoveModelSpecificationProperties(specification, term);
    PruneModelSpecificationState(specification);
    return {true, changed, {}};
}

ModelSpecificationEditResult ReplaceModelSpecificationTerm(
    ModelSpecification &specification,
    const std::vector<std::string> &availableVariables,
    const std::string &term,
    const std::string &replacement)
{
    if (term.empty() || replacement.empty()) {
        return {false, false, "Both the current and replacement terms are required."};
    }
    if (!ModelSpecificationIncludesTerm(specification, term)) {
        return {false, false, "Model term to replace was not found."};
    }
    if (!ModelTermExistsForVariables(availableVariables, replacement, specification.response)) {
        return {false, false, "Replacement predictor must be an available variable."};
    }
    if (!EquivalentModelTerm(term, replacement) &&
        ModelSpecificationIncludesTerm(specification, replacement)) {
        return {false, false, "Replacement predictor is already in the model."};
    }

    std::vector<std::string> renamed = specification.terms;
    RenameVariableInTermList(renamed, BaseVariableForTermComponent(term),
                             BaseVariableForTermComponent(replacement));
    std::vector<std::string> unique;
    for (const std::string &candidate : renamed) {
        if (!ModelTermExistsForVariables(availableVariables, candidate,
                                         specification.response)) {
            return {false, false, "Replacement produced an invalid model term."};
        }
        if (!TermListContainsEquivalentModelTerm(unique, candidate)) unique.push_back(candidate);
    }
    specification.terms = std::move(unique);
    RemoveModelSpecificationProperties(specification, term);
    PruneModelSpecificationState(specification);
    return {true, true, {}};
}

ModelSpecificationEditResult SetModelSpecificationTermType(
    ModelSpecification &specification,
    const std::string &term,
    const std::string &type,
    const ModelPredictorMetadata &metadata)
{
    const ModelFamilyCapabilities capabilities = CapabilitiesForModelFamily(specification.familyKind);
    if (!ValidPredictorType(type) ||
        (type == "numeric" && !capabilities.supportsNumericPredictors) ||
        (type == "factor" && !capabilities.supportsFactorPredictors)) {
        return {false, false, "Unsupported predictor type for this model family."};
    }
    const std::string variable = BaseVariableForTermComponent(term);
    if (variable.empty() || IsInteractionTerm(term) ||
        !ModelSpecificationIncludesTerm(specification, variable)) {
        return {false, false, "Predictor type can only be changed for a model main effect."};
    }
    if (!metadata.variable.empty() && metadata.variable != variable) {
        return {false, false, "Predictor metadata does not match the selected term."};
    }
    if (type == "factor" && metadata.storageType != "factor") {
        const std::size_t threshold = std::max<std::size_t>(
            20, (metadata.observedCount + 3) / 4);
        if (metadata.uniqueCount > threshold) {
            return {false, false,
                "This numeric predictor has too many distinct values to treat as categorical."};
        }
    }
    const std::string before = ModelSpecificationTermType(specification, variable,
                                                          metadata.storageType);
    const auto previousOverride = specification.termTypeOverrides.find(variable);
    if (before == type && previousOverride != specification.termTypeOverrides.end() &&
        previousOverride->second == type) {
        return {true, false, {}};
    }
    specification.termTypeOverrides[variable] = type;
    specification.termTypes[variable] = type;
    if (type == "factor") specification.centeredPredictors.erase(variable);
    else specification.factorReferenceLevels.erase(variable);
    PruneModelSpecificationState(specification);
    return {true, before != type || previousOverride == specification.termTypeOverrides.end(), {}};
}

ModelSpecificationEditResult SetModelSpecificationReferenceLevel(
    ModelSpecification &specification,
    const std::string &term,
    const std::string &referenceLevel,
    const std::vector<std::string> &availableLevels)
{
    const std::string variable = BaseVariableForTermComponent(term);
    if (!CapabilitiesForModelFamily(specification.familyKind).supportsReferenceLevels ||
        variable.empty() || IsInteractionTerm(term) ||
        ModelSpecificationTermType(specification, variable) != "factor") {
        return {false, false, "Reference categories are only available for categorical predictors."};
    }
    if (std::find(availableLevels.begin(), availableLevels.end(), referenceLevel) ==
        availableLevels.end()) {
        return {false, false, "Reference category is not available for this predictor."};
    }
    auto current = specification.factorReferenceLevels.find(variable);
    if (current != specification.factorReferenceLevels.end() &&
        current->second == referenceLevel) {
        return {true, false, {}};
    }
    specification.factorReferenceLevels[variable] = referenceLevel;
    return {true, true, {}};
}

ModelSpecificationEditResult SetModelSpecificationPredictorCentered(
    ModelSpecification &specification,
    const std::string &term,
    bool centered)
{
    const std::string variable = BaseVariableForTermComponent(term);
    if (!CapabilitiesForModelFamily(specification.familyKind).supportsCentering ||
        variable.empty() || IsInteractionTerm(term) ||
        !ModelSpecificationIncludesTerm(specification, variable) ||
        ModelSpecificationTermType(specification, variable) == "factor") {
        return {false, false, "Only numeric main-effect predictors can be centered."};
    }
    const bool wasCentered = specification.centeredPredictors.count(variable) != 0;
    if (centered) specification.centeredPredictors.insert(variable);
    else specification.centeredPredictors.erase(variable);
    return {true, wasCentered != centered, {}};
}

void PruneModelSpecificationState(ModelSpecification &specification)
{
    auto mainEffectPresent = [&](const std::string &variable) {
        return !variable.empty() && variable != specification.response &&
            TermListContainsEquivalentModelTerm(specification.terms, variable);
    };
    for (auto it = specification.centeredPredictors.begin();
         it != specification.centeredPredictors.end();) {
        if (!mainEffectPresent(*it) ||
            ModelSpecificationTermType(specification, *it) == "factor") {
            it = specification.centeredPredictors.erase(it);
        } else {
            ++it;
        }
    }
    for (auto it = specification.factorReferenceLevels.begin();
         it != specification.factorReferenceLevels.end();) {
        if (!mainEffectPresent(it->first) ||
            ModelSpecificationTermType(specification, it->first) != "factor") {
            it = specification.factorReferenceLevels.erase(it);
        } else {
            ++it;
        }
    }
    for (auto it = specification.termTypeOverrides.begin();
         it != specification.termTypeOverrides.end();) {
        if (!mainEffectPresent(it->first) || !ValidPredictorType(it->second)) {
            it = specification.termTypeOverrides.erase(it);
        } else {
            ++it;
        }
    }
}

bool EquivalentModelSpecifications(const ModelSpecification &left,
                                   const ModelSpecification &right)
{
    if (left.familyKind != right.familyKind || left.response != right.response ||
        left.termTypes != right.termTypes ||
        left.termTypeOverrides != right.termTypeOverrides ||
        left.centeredPredictors != right.centeredPredictors ||
        left.factorReferenceLevels != right.factorReferenceLevels ||
        left.scope != right.scope || left.dataScopeCaptured != right.dataScopeCaptured ||
        left.terms.size() != right.terms.size()) {
        return false;
    }
    if (left.dataScopeCaptured &&
        (left.dataScope.kind != right.dataScope.kind ||
         left.dataScope.datasetId != right.dataScope.datasetId ||
         left.dataScope.originalRowIds != right.dataScope.originalRowIds ||
         left.dataScope.sourceKind != right.dataScope.sourceKind ||
         left.dataScope.sourceDescription != right.dataScope.sourceDescription ||
         left.dataScope.sourceViewId != right.dataScope.sourceViewId ||
         left.dataScope.sourceElementId != right.dataScope.sourceElementId)) {
        return false;
    }
    for (const std::string &term : left.terms) {
        if (!TermListContainsEquivalentModelTerm(right.terms, term)) return false;
    }
    return true;
}

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

bool ParsePolynomialTerm(const std::string &term,
                         std::string &variable,
                         int &degree)
{
    variable.clear();
    degree = 0;
    const std::string trimmed = TrimCopy(term);
    if (trimmed.size() < 6 || trimmed.rfind("I(", 0) != 0 || trimmed.back() != ')') {
        return false;
    }
    const std::string inside = TrimCopy(trimmed.substr(2, trimmed.size() - 3));
    const std::size_t power = inside.rfind('^');
    if (power == std::string::npos) return false;
    variable = TrimCopy(inside.substr(0, power));
    const std::string degreeText = TrimCopy(inside.substr(power + 1));
    if (variable.empty() || degreeText.empty() ||
        !std::all_of(degreeText.begin(), degreeText.end(), [](unsigned char value) {
            return std::isdigit(value) != 0;
        })) {
        variable.clear();
        return false;
    }
    try {
        degree = std::stoi(degreeText);
    } catch (...) {
        variable.clear();
        degree = 0;
        return false;
    }
    if (degree < 2 || degree > 99) {
        variable.clear();
        degree = 0;
        return false;
    }
    return true;
}

bool IsPolynomialTerm(const std::string &term)
{
    std::string variable;
    int degree = 0;
    return ParsePolynomialTerm(term, variable, degree);
}

std::string PolynomialTerm(const std::string &variable, int degree)
{
    const std::string trimmed = TrimCopy(variable);
    if (trimmed.empty() || degree < 2 || degree > 99) return "";
    return "I(" + trimmed + "^" + std::to_string(degree) + ")";
}

std::map<std::string, std::vector<std::string>> PolynomialMenuTerms(
    const std::vector<std::string> &numericVariables, const std::string &response,
    const std::vector<std::string> &existingTerms, const std::string &baseTerm,
    const std::map<std::string, int> &maximumDegreeByVariable)
{
    std::map<std::string, std::vector<std::string>> groups;
    for (const auto &variable : numericVariables) {
        if (variable == response || (!baseTerm.empty() &&
            (IsInteractionTerm(baseTerm) || BaseVariableForTermComponent(baseTerm) != variable))) continue;
        const auto limit = maximumDegreeByVariable.find(variable);
        const int maximumDegree = limit == maximumDegreeByVariable.end() ? 5
            : std::min(5, limit->second);
        for (int degree = 2; degree <= maximumDegree; ++degree) {
            const auto term = PolynomialTerm(variable, degree);
            if (!TermListContainsEquivalentModelTerm(existingTerms, term)) groups[variable].push_back(term);
        }
    }
    return groups;
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
    std::string polynomialVariable;
    int polynomialDegree = 0;
    if (ParsePolynomialTerm(trimmed, polynomialVariable, polynomialDegree)) {
        return polynomialVariable;
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
                std::string polynomialVariable;
                int polynomialDegree = 0;
                if (ParsePolynomialTerm(trimmed, polynomialVariable, polynomialDegree)) {
                    renamed.push_back(PolynomialTerm(newName, polynomialDegree));
                } else {
                    renamed.push_back(newName);
                }
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
    auto canonicalComponent = [](const std::string &part) {
        const std::string trimmed = TrimCopy(part);
        std::string variable;
        int degree = 0;
        if (ParsePolynomialTerm(trimmed, variable, degree)) {
            return PolynomialTerm(variable, degree);
        }
        return BaseVariableForTermComponent(trimmed);
    };
    for (std::string &part : left) part = canonicalComponent(part);
    for (std::string &part : right) part = canonicalComponent(part);
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
    std::string removedPolynomialVariable;
    int removedPolynomialDegree = 0;
    if (ParsePolynomialTerm(removed, removedPolynomialVariable, removedPolynomialDegree)) {
        const std::string canonical = PolynomialTerm(
            removedPolynomialVariable, removedPolynomialDegree);
        for (const std::string &part : SplitInteractionTerm(candidate)) {
            std::string candidateVariable;
            int candidateDegree = 0;
            if (ParsePolynomialTerm(part, candidateVariable, candidateDegree) &&
                PolynomialTerm(candidateVariable, candidateDegree) == canonical) {
                return true;
            }
        }
        return false;
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

std::vector<InteractionMenuPredictorGroup> BuildInteractionMenuGroups(
    const std::vector<std::string> &candidateTerms,
    const std::string &baseTerm)
{
    std::vector<std::string> predictors;
    const std::vector<std::string> requestedVariables =
        UniqueBaseVariablesForTerm(baseTerm);
    const bool anchored = !requestedVariables.empty();
    if (anchored) {
        predictors.push_back(JoinStrings(requestedVariables, " × "));
    } else {
        // Candidate generation is already stable by model-term order. Keeping
        // first occurrence here preserves that order in every platform menu.
        for (const std::string &candidate : candidateTerms) {
            for (const std::string &part : SplitInteractionTerm(candidate)) {
                const std::string predictor = BaseVariableForTermComponent(part);
                if (!predictor.empty() && !StringVectorContains(predictors, predictor)) {
                    predictors.push_back(predictor);
                }
            }
        }
    }

    std::vector<InteractionMenuPredictorGroup> groups;
    for (const std::string &predictor : predictors) {
        InteractionMenuPredictorGroup predictorGroup;
        predictorGroup.predictor = predictor;
        for (const std::string &candidate : candidateTerms) {
            const std::vector<std::string> parts = SplitInteractionTerm(candidate);
            if (parts.size() < 2) {
                continue;
            }
            auto anchor = parts.end();
            if (anchored) {
                const std::vector<std::string> candidateVariables =
                    UniqueBaseVariablesForTerm(candidate);
                if (!ContainsAllVariables(candidateVariables, requestedVariables)) continue;
            } else {
                anchor = std::find_if(parts.begin(), parts.end(), [&](const std::string &part) {
                    return BaseVariableForTermComponent(part) == predictor;
                });
                if (anchor == parts.end()) continue;
            }

            auto order = std::find_if(predictorGroup.orders.begin(), predictorGroup.orders.end(),
                [&](const InteractionMenuOrderGroup &entry) { return entry.order == parts.size(); });
            if (order == predictorGroup.orders.end()) {
                predictorGroup.orders.push_back({parts.size(), {}});
                order = std::prev(predictorGroup.orders.end());
            }

            std::vector<std::string> displayParts;
            // When the interaction menu is opened from a particular main
            // effect, two-term choices are shown as the other available main
            // effect.  The selected term is already supplied by the context,
            // so repeating `selected × other` would add a redundant menu
            // level.  Higher-order choices retain their complete label.
            if (anchored && parts.size() == requestedVariables.size() + 1) {
                for (const std::string &part : parts) {
                    const std::string variable = BaseVariableForTermComponent(part);
                    if (!StringVectorContains(requestedVariables, variable)) {
                        displayParts.push_back(variable);
                    }
                }
            } else if (anchored) {
                displayParts = requestedVariables;
                for (const std::string &part : parts) {
                    const std::string variable = BaseVariableForTermComponent(part);
                    if (!StringVectorContains(requestedVariables, variable)) {
                        displayParts.push_back(variable);
                    }
                }
            } else {
                displayParts.push_back(BaseVariableForTermComponent(*anchor));
                for (auto part = parts.begin(); part != parts.end(); ++part) {
                    if (part != anchor) {
                        displayParts.push_back(BaseVariableForTermComponent(*part));
                    }
                }
            }
            // The command must use the same orientation the user selected.
            // Equivalence checks still prevent duplicate model terms.
            std::vector<std::string> orderedParts;
            const auto firstVariables = anchored ? requestedVariables
                : std::vector<std::string>{predictor};
            for (const auto &variable : firstVariables) {
                auto part = std::find_if(parts.begin(), parts.end(), [&](const std::string &value) {
                    return BaseVariableForTermComponent(value) == variable;
                });
                if (part != parts.end()) orderedParts.push_back(*part);
            }
            for (const auto &part : parts)
                if (!StringVectorContains(firstVariables, BaseVariableForTermComponent(part)))
                    orderedParts.push_back(part);
            order->choices.push_back({InteractionTermFromParts(orderedParts),
                                      JoinStrings(displayParts, " × ")});
        }
        if (!predictorGroup.orders.empty()) {
            groups.push_back(std::move(predictorGroup));
        }
    }
    return groups;
}

std::vector<std::string> HierarchicalTermsForModelTerm(const std::vector<std::string> &availableVariables,
                                                       const std::string &response,
                                                       const std::string &term)
{
    std::vector<std::string> hierarchy;
    std::string polynomialVariable; int degree = 0;
    if (ParsePolynomialTerm(term, polynomialVariable, degree)) {
        if (!ModelTermExistsForVariables(availableVariables, polynomialVariable, response)) return hierarchy;
        hierarchy.push_back(polynomialVariable);
        for (int power = 2; power <= degree; ++power) hierarchy.push_back(PolynomialTerm(polynomialVariable, power));
        return hierarchy;
    }
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
        return "Numeric x Categorical";
    }
    if (type == "factor_factor_interaction") {
        return "Categorical x Categorical";
    }
    if (type == "higher_order_interaction" || type == "interaction") {
        return "Interaction";
    }
    if (type == "factor") return "Categorical";
    if (type == "ordered") return "Ordinal";
    return "Numeric";
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
