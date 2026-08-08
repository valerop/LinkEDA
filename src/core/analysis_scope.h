#ifndef RLISPSTAT_CORE_ANALYSIS_SCOPE_H
#define RLISPSTAT_CORE_ANALYSIS_SCOPE_H

#include <cstddef>
#include <optional>
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
    OtherExplicitSubset
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
bool AnalysisScopeContainsRow(const AnalysisScope &scope,
                              int originalRowId,
                              std::size_t totalRows);
std::size_t AnalysisScopeRowCount(const AnalysisScope &scope,
                                  std::size_t totalRows);
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
std::string AnalysisScopeSummary(const AnalysisScope &scope,
                                 std::size_t totalRows,
                                 bool includePrefix = true);
std::string AnalysisScopeWindowSummary(const AnalysisScope &scope,
                                       std::size_t totalRows);

} // namespace core
} // namespace rlispstat

#endif
