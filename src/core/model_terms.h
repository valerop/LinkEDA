#ifndef RLISPSTAT_CORE_MODEL_TERMS_H
#define RLISPSTAT_CORE_MODEL_TERMS_H

#include "analysis_scope.h"
#include "provenance_model.h"

#include <cstddef>
#include <limits>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

// Model families share a semantic specification, but not their fit results or
// family-specific statistics.  Linear and generalized-linear fits cross this
// boundary without generalized models inheriting R-squared, F tests, or other
// linear-only concepts.
enum class ModelFamilyKind {
    Linear,
    GeneralizedLinear
};

struct ModelFamilyCapabilities {
    bool supportsNumericPredictors = false;
    bool supportsFactorPredictors = false;
    bool supportsReferenceLevels = false;
    bool supportsCentering = false;
    bool supportsInteractions = false;
};

ModelFamilyCapabilities CapabilitiesForModelFamily(ModelFamilyKind family);

struct ModelTermSpecification {
    std::string term;
    std::vector<std::string> variables;
    std::string type = "numeric";
    std::string referenceLevel;
    bool centered = false;
    // The fitted centering constant is optional semantic information.  A
    // pending/unfitted specification records the transformation with
    // `centered`; the fitting layer supplies the actual constant later.
    double centeringConstant = std::numeric_limits<double>::quiet_NaN();
};

// Canonical, platform-neutral specification shared by a single GLM and every
// column in General Linear Model Comparison. Fit results intentionally do not
// belong here.
struct ModelSpecification {
    ModelFamilyKind familyKind = ModelFamilyKind::Linear;
    std::string response;
    std::vector<std::string> terms;
    // Effective types are synchronized from dataset metadata.  Explicit
    // model-local interpretations override them without mutating the dataset.
    std::map<std::string, std::string> termTypes;
    std::map<std::string, std::string> termTypeOverrides;
    std::set<std::string> centeredPredictors;
    std::map<std::string, std::string> factorReferenceLevels;
    std::string scope = "all";
    AnalysisScope dataScope;
    bool dataScopeCaptured = false;
    std::string frozenScopeNotice;
    AnalysisProvenance provenance;
};

// Resolve the row-selection payload consumed by the R fitters. A captured
// explicit scope is immutable; otherwise selected/unselected model scopes
// follow the dataset's current linked selection. For an unselected fit the
// payload is still the selected IDs because R applies the complement.
std::vector<int> ModelSpecificationSelectionRows(
    const ModelSpecification &specification,
    const std::vector<int> &currentSelectedRows);

// A captured explicit scope is fitted as a selected subset. Without such a
// snapshot the model's editable scope remains authoritative.
std::string ModelSpecificationFitScope(
    const ModelSpecification &specification);

struct ModelPredictorMetadata {
    std::string variable;
    std::string storageType = "numeric";
    std::size_t observedCount = 0;
    std::size_t uniqueCount = 0;
    std::vector<std::string> levels;
};

struct ModelSpecificationEditResult {
    bool ok = false;
    bool changed = false;
    std::string message;
};

std::string ModelSpecificationTermType(const ModelSpecification &specification,
                                       const std::string &term,
                                       const std::string &fallbackType = "numeric");
// Materializes the effective per-variable type map consumed by fitting and
// display layers.  Explicit model-local interpretations always win over the
// dataset-derived defaults in `termTypes`.
std::map<std::string, std::string> EffectiveModelSpecificationTermTypes(
    const ModelSpecification &specification);
ModelTermSpecification ResolveModelTermSpecification(
    const ModelSpecification &specification,
    const std::string &term,
    const std::map<std::string, double> &centeringConstants = {});
bool ModelSpecificationIncludesTerm(const ModelSpecification &specification,
                                    const std::string &term);
ModelSpecificationEditResult SetModelSpecificationResponse(
    ModelSpecification &specification,
    const std::string &response);
ModelSpecificationEditResult AddModelSpecificationTerm(
    ModelSpecification &specification,
    const std::vector<std::string> &availableVariables,
    const std::string &term);
ModelSpecificationEditResult RemoveModelSpecificationTerm(
    ModelSpecification &specification,
    const std::string &term);
ModelSpecificationEditResult ReplaceModelSpecificationTerm(
    ModelSpecification &specification,
    const std::vector<std::string> &availableVariables,
    const std::string &term,
    const std::string &replacement);
ModelSpecificationEditResult SetModelSpecificationTermType(
    ModelSpecification &specification,
    const std::string &term,
    const std::string &type,
    const ModelPredictorMetadata &metadata);
ModelSpecificationEditResult SetModelSpecificationReferenceLevel(
    ModelSpecification &specification,
    const std::string &term,
    const std::string &referenceLevel,
    const std::vector<std::string> &availableLevels);
ModelSpecificationEditResult SetModelSpecificationPredictorCentered(
    ModelSpecification &specification,
    const std::string &term,
    bool centered);
void PruneModelSpecificationState(ModelSpecification &specification);
bool EquivalentModelSpecifications(const ModelSpecification &left,
                                   const ModelSpecification &right);

std::vector<std::string> SplitInteractionTerm(const std::string &term);
bool IsInteractionTerm(const std::string &term);
bool ParsePolynomialTerm(const std::string &term,
                         std::string &variable,
                         int &degree);
bool IsPolynomialTerm(const std::string &term);
std::map<std::string, std::vector<std::string>> PolynomialMenuTerms(
    const std::vector<std::string> &numericVariables, const std::string &response,
    const std::vector<std::string> &existingTerms, const std::string &baseTerm = "",
    const std::map<std::string, int> &maximumDegreeByVariable = {});
std::string PolynomialTerm(const std::string &variable, int degree);
std::string BaseVariableForTermComponent(const std::string &term);
std::vector<std::string> UniqueBaseVariablesForTerm(const std::string &term);
std::string InteractionTermFromParts(const std::vector<std::string> &parts);
std::string RenameVariableInTerm(const std::string &term,
                                 const std::string &oldName,
                                 const std::string &newName);
void RenameVariableInTermList(std::vector<std::string> &terms,
                              const std::string &oldName,
                              const std::string &newName);

bool EquivalentInteractionTerm(const std::string &a, const std::string &b);
bool EquivalentModelTerm(const std::string &a, const std::string &b);
std::string ModelTermWithoutLevelSuffixes(const std::string &term);
std::string FactorLevelTermValue(double value);
std::string ModelTermBaseForDisplayRow(const std::string &term);
bool ModelTermShouldBeRemoved(const std::string &candidate, const std::string &removed);
bool TermListContainsEquivalentModelTerm(const std::vector<std::string> &terms,
                                         const std::string &candidate);
bool RemoveModelTermCascade(std::vector<std::string> &terms,
                            const std::string &removed);

bool ModelTermExistsForVariables(const std::vector<std::string> &availableVariables,
                                 const std::string &term,
                                 const std::string &response = "");

std::vector<std::string> FilterModelTermsForResponse(const std::vector<std::string> &availableVariables,
                                                     const std::vector<std::string> &terms,
                                                     const std::string &response);

std::vector<std::string> BaseVariablesFromModelTerms(const std::vector<std::string> &availableVariables,
                                                     const std::string &response,
                                                     const std::vector<std::string> &modelTerms);

std::vector<std::string> InteractionCandidateTermsForModel(const std::vector<std::string> &availableVariables,
                                                           const std::string &response,
                                                           const std::vector<std::string> &modelTerms,
                                                           const std::vector<std::string> &existingTerms,
                                                           const std::string &baseTerm = "");

// A comparison exposes terms from several model columns. Interaction choices are
// therefore built from the union of their component variables, while terms that
// already belong to the active column remain excluded from the menu.
std::vector<std::string> InteractionCandidateTermsForComparison(
    const std::vector<std::string> &availableVariables,
    const std::string &response,
    const std::vector<std::vector<std::string>> &comparisonModelTerms,
    const std::vector<std::string> &activeModelTerms,
    const std::string &baseTerm = "");

// Platform-neutral navigation model for interaction menus. `term` is the
// ordered value sent to the model command; `displayLabel` preserves the same
// orientation, with the selected predictor or interaction prefix first.
struct InteractionMenuChoice {
    std::string term;
    std::string displayLabel;
};

struct InteractionMenuOrderGroup {
    std::size_t order = 0;
    std::vector<InteractionMenuChoice> choices;
};

struct InteractionMenuPredictorGroup {
    std::string predictor;
    std::vector<InteractionMenuOrderGroup> orders;
};

std::vector<InteractionMenuPredictorGroup> BuildInteractionMenuGroups(
    const std::vector<std::string> &candidateTerms,
    const std::string &baseTerm = "");

std::vector<std::string> HierarchicalTermsForModelTerm(const std::vector<std::string> &availableVariables,
                                                       const std::string &response,
                                                       const std::string &term);

bool AddHierarchicalTermsToVector(const std::vector<std::string> &availableVariables,
                                  const std::string &response,
                                  std::vector<std::string> &terms,
                                  const std::string &term);

std::string ResolvedModelTermDisplayType(const std::string &term,
                                         const std::map<std::string, std::string> &overrides,
                                         const std::string &fallbackType);
std::string SemanticInteractionTermType(const std::vector<std::string> &componentTypes);
bool ModelTermTypeIsInteraction(const std::string &type);
std::string ModelTermTypeDisplayName(const std::string &type);

std::string VariableRoleForModelContext(const std::string &variable,
                                        const std::string &dependent,
                                        const std::vector<std::string> &terms,
                                        const std::string &xVariable = "",
                                        const std::string &yVariable = "");

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_MODEL_TERMS_H
