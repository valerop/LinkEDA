#ifndef RLISPSTAT_CORE_ANALYSIS_SCOPE_H
#define RLISPSTAT_CORE_ANALYSIS_SCOPE_H

#include <cstddef>
#include <optional>
#include <set>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

enum class AnalysisScopeKind {
    AllObservations,
    ExplicitRowIds
};

enum class AnalysisScopeSourceKind {
    AllData,
    CurrentSelection,
    TrellisPanel,
    DataTableRows,
    ScatterplotSelection,
    BoxplotGroup,
    HistogramBin,
    BarSegment,
    VisibleObservations,
    OtherExplicitSubset,
    CurrentUnselection,
    PlotExclusion,
    IncludedObservations
};

struct AnalysisScope {
    AnalysisScopeKind kind = AnalysisScopeKind::AllObservations;
    std::string datasetId;
    std::vector<int> originalRowIds;
    AnalysisScopeSourceKind sourceKind = AnalysisScopeSourceKind::AllData;
    std::string sourceDescription = "All observations";
    std::optional<std::string> sourceViewId;
    std::optional<std::string> sourceElementId;
    std::size_t totalDatasetRows = 0;
    std::size_t invalidatedRowCount = 0;
};

struct AnalysisScopeChangeEvent {
    std::string datasetId;
    AnalysisScope previousScope;
    AnalysisScope currentScope;
    std::size_t version = 0;
};

struct SavedSelection {
    std::string datasetId;
    std::string name;
    std::vector<int> originalRowIds;
    AnalysisScopeSourceKind sourceKind = AnalysisScopeSourceKind::CurrentSelection;
    std::optional<std::string> sourceViewId;
};

// Platform-neutral labels for the central scope menu and read-only displays. Built-in
// choices retain their established ids; saved scopes use an opaque value so
// display labels never become part of an analysis specification.
struct AnalysisScopeChoice {
    std::string value;
    std::string label;
};

AnalysisScope AllObservationsAnalysisScope(const std::string &datasetId,
                                           std::size_t totalRows);
AnalysisScope ExplicitAnalysisScope(const std::string &datasetId,
                                    const std::vector<int> &originalRowIds,
                                    AnalysisScopeSourceKind sourceKind,
                                    const std::string &sourceDescription,
                                    std::size_t totalRows,
                                    const std::optional<std::string> &sourceViewId = std::nullopt,
                                    const std::optional<std::string> &sourceElementId = std::nullopt);
std::vector<int> NormalizeAnalysisScopeRowIds(const std::vector<int> &rowIds);
bool ValidateAnalysisScope(const AnalysisScope &scope,
                           const std::string &datasetId,
                           std::size_t totalRows,
                           std::string *error = nullptr);
AnalysisScope ReconcileAnalysisScope(const AnalysisScope &scope,
                                     std::size_t totalRows);
std::vector<int> ResolveAnalysisScopeRowIds(const AnalysisScope &scope,
                                            std::size_t totalRows);
AnalysisScope AnalysisScopeExcludingRows(const AnalysisScope &scope,
                                        const std::set<int> &excludedRows,
                                        std::size_t totalRows,
                                        const std::string &sourceViewId);
bool AnalysisScopeContainsRow(const AnalysisScope &scope,
                              int originalRowId,
                              std::size_t totalRows);
std::size_t AnalysisScopeRowCount(const AnalysisScope &scope,
                                  std::size_t totalRows);
bool AnalysisScopeMatchesRows(const AnalysisScope &scope,
                              const std::vector<int> &originalRowIds);
// True only for a live selection-backed scope. A named saved scope is an
// immutable snapshot even when it was originally created from the current
// selection.
bool AnalysisScopeTracksCurrentSelection(const AnalysisScope &scope);
std::string AnalysisScopeKindId(AnalysisScopeKind kind);
std::string AnalysisScopeSourceKindId(AnalysisScopeSourceKind kind);
AnalysisScopeSourceKind AnalysisScopeSourceKindFromId(const std::string &id);
bool AnalysisScopeSourceRequiresSelectionName(AnalysisScopeSourceKind kind);
std::string NormalizeAnalysisScopeSelectionName(const std::string &name);
bool ValidateAnalysisScopeSelectionName(const std::string &name,
                                        std::string *error = nullptr);
std::string NamedSelectionAnalysisScopeDescription(const std::string &name);
std::optional<std::string> AnalysisScopeSelectionName(const AnalysisScope &scope);
AnalysisScope AnalysisScopeForSavedSelection(const SavedSelection &selection,
                                             std::size_t totalRows);
std::string SavedAnalysisScopeChoiceValue(const std::string &name);
std::optional<std::string> SavedAnalysisScopeNameFromChoiceValue(
    const std::string &value);
std::vector<AnalysisScopeChoice> BuildAnalysisScopeChoices(
    std::size_t selectedRowCount,
    const std::vector<SavedSelection> &savedSelections,
    bool includeUnselected = true);
std::string AnalysisScopeChoiceValue(const std::string &fallbackScope,
                                     const AnalysisScope &capturedScope,
                                     bool captured);
std::string AnalysisScopeSummary(const AnalysisScope &scope,
                                 std::size_t totalRows,
                                 bool includePrefix = true);
std::string AnalysisScopeCompactSummary(const AnalysisScope &scope, std::size_t totalRows);
std::string AnalysisScopeWindowSummary(const AnalysisScope &scope,
                                       std::size_t totalRows);
// Explain why a manually frozen result differs from the active global scope.
// An empty string means the two scopes still identify the same analysis rows.
std::string FrozenAnalysisScopeNotice(const AnalysisScope &computed,
                                      const AnalysisScope &current);

} // namespace core
} // namespace rlispstat

#endif
