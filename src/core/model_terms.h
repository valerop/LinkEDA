#ifndef RLISPSTAT_CORE_MODEL_TERMS_H
#define RLISPSTAT_CORE_MODEL_TERMS_H

#include <map>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

std::vector<std::string> SplitInteractionTerm(const std::string &term);
bool IsInteractionTerm(const std::string &term);
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
