#include "analysis_scope.h"

#include <algorithm>
#include <cctype>
#include <numeric>
#include <set>

namespace rlispstat {
namespace core {

AnalysisScope AllObservationsAnalysisScope(const std::string &datasetId,
                                           std::size_t totalRows)
{
    AnalysisScope scope;
    scope.kind = AnalysisScopeKind::AllObservations;
    scope.datasetId = datasetId;
    scope.sourceKind = AnalysisScopeSourceKind::AllData;
    scope.sourceDescription = "All observations";
    scope.totalDatasetRows = totalRows;
    return scope;
}

std::vector<int> NormalizeAnalysisScopeRowIds(const std::vector<int> &rowIds)
{
    std::vector<int> normalized;
    normalized.reserve(rowIds.size());
    std::set<int> seen;
    for (int row : rowIds) {
        if (row > 0 && seen.insert(row).second) normalized.push_back(row);
    }
    return normalized;
}

AnalysisScope ExplicitAnalysisScope(const std::string &datasetId,
                                    const std::vector<int> &originalRowIds,
                                    AnalysisScopeSourceKind sourceKind,
                                    const std::string &sourceDescription,
                                    std::size_t totalRows,
                                    const std::optional<std::string> &sourceViewId,
                                    const std::optional<std::string> &sourceElementId)
{
    AnalysisScope scope;
    scope.kind = AnalysisScopeKind::ExplicitRowIds;
    scope.datasetId = datasetId;
    scope.originalRowIds = NormalizeAnalysisScopeRowIds(originalRowIds);
    scope.sourceKind = sourceKind;
    scope.sourceDescription = sourceDescription.empty() ? "Explicit subset" : sourceDescription;
    scope.sourceViewId = sourceViewId;
    scope.sourceElementId = sourceElementId;
    scope.totalDatasetRows = totalRows;
    return scope;
}

bool ValidateAnalysisScope(const AnalysisScope &scope,
                           const std::string &datasetId,
                           std::size_t totalRows,
                           std::string *error)
{
    if (scope.datasetId.empty()) {
        if (error) *error = "Analysis scope requires a dataset ID.";
        return false;
    }
    if (scope.datasetId != datasetId) {
        if (error) *error = "Analysis scope belongs to a different dataset.";
        return false;
    }
    if (scope.kind == AnalysisScopeKind::AllObservations) return true;
    std::set<int> seen;
    for (int row : scope.originalRowIds) {
        if (row <= 0 || static_cast<std::size_t>(row) > totalRows) {
            if (error) *error = "Analysis scope contains an invalid original row ID.";
            return false;
        }
        if (!seen.insert(row).second) {
            if (error) *error = "Analysis scope contains duplicate original row IDs.";
            return false;
        }
    }
    return true;
}

AnalysisScope ReconcileAnalysisScope(const AnalysisScope &scope,
                                     std::size_t totalRows)
{
    AnalysisScope reconciled = scope;
    reconciled.totalDatasetRows = totalRows;
    if (scope.kind == AnalysisScopeKind::AllObservations) {
        reconciled.originalRowIds.clear();
        reconciled.invalidatedRowCount = 0;
        return reconciled;
    }
    reconciled.originalRowIds.clear();
    std::set<int> seen;
    for (int row : scope.originalRowIds) {
        if (row > 0 && static_cast<std::size_t>(row) <= totalRows && seen.insert(row).second) {
            reconciled.originalRowIds.push_back(row);
        }
    }
    reconciled.invalidatedRowCount = scope.invalidatedRowCount +
        (NormalizeAnalysisScopeRowIds(scope.originalRowIds).size() - reconciled.originalRowIds.size());
    return reconciled;
}

std::vector<int> ResolveAnalysisScopeRowIds(const AnalysisScope &scope,
                                            std::size_t totalRows)
{
    if (scope.kind == AnalysisScopeKind::AllObservations) {
        std::vector<int> rows(totalRows);
        std::iota(rows.begin(), rows.end(), 1);
        return rows;
    }
    return ReconcileAnalysisScope(scope, totalRows).originalRowIds;
}

bool AnalysisScopeContainsRow(const AnalysisScope &scope,
                              int originalRowId,
                              std::size_t totalRows)
{
    if (originalRowId <= 0 || static_cast<std::size_t>(originalRowId) > totalRows) return false;
    if (scope.kind == AnalysisScopeKind::AllObservations) return true;
    return std::find(scope.originalRowIds.begin(), scope.originalRowIds.end(), originalRowId) !=
           scope.originalRowIds.end();
}

std::size_t AnalysisScopeRowCount(const AnalysisScope &scope,
                                  std::size_t totalRows)
{
    return scope.kind == AnalysisScopeKind::AllObservations
        ? totalRows : ResolveAnalysisScopeRowIds(scope, totalRows).size();
}

std::string AnalysisScopeKindId(AnalysisScopeKind kind)
{
    return kind == AnalysisScopeKind::AllObservations ? "all" : "explicit";
}

std::string AnalysisScopeSourceKindId(AnalysisScopeSourceKind kind)
{
    switch (kind) {
    case AnalysisScopeSourceKind::AllData: return "all_data";
    case AnalysisScopeSourceKind::CurrentSelection: return "current_selection";
    case AnalysisScopeSourceKind::TrellisPanel: return "trellis_panel";
    case AnalysisScopeSourceKind::DataTableRows: return "data_table_rows";
    case AnalysisScopeSourceKind::ScatterplotSelection: return "scatterplot_selection";
    case AnalysisScopeSourceKind::BoxplotGroup: return "boxplot_group";
    case AnalysisScopeSourceKind::HistogramBin: return "histogram_bin";
    case AnalysisScopeSourceKind::BarSegment: return "bar_segment";
    case AnalysisScopeSourceKind::VisibleObservations: return "visible_observations";
    case AnalysisScopeSourceKind::OtherExplicitSubset: return "other_explicit_subset";
    }
    return "other_explicit_subset";
}

AnalysisScopeSourceKind AnalysisScopeSourceKindFromId(const std::string &id)
{
    if (id == "all_data") return AnalysisScopeSourceKind::AllData;
    if (id == "current_selection") return AnalysisScopeSourceKind::CurrentSelection;
    if (id == "trellis_panel") return AnalysisScopeSourceKind::TrellisPanel;
    if (id == "data_table_rows") return AnalysisScopeSourceKind::DataTableRows;
    if (id == "scatterplot_selection") return AnalysisScopeSourceKind::ScatterplotSelection;
    if (id == "boxplot_group") return AnalysisScopeSourceKind::BoxplotGroup;
    if (id == "histogram_bin") return AnalysisScopeSourceKind::HistogramBin;
    if (id == "bar_segment") return AnalysisScopeSourceKind::BarSegment;
    if (id == "visible_observations") return AnalysisScopeSourceKind::VisibleObservations;
    return AnalysisScopeSourceKind::OtherExplicitSubset;
}

bool AnalysisScopeSourceRequiresSelectionName(AnalysisScopeSourceKind kind)
{
    return kind == AnalysisScopeSourceKind::CurrentSelection ||
        kind == AnalysisScopeSourceKind::DataTableRows ||
        kind == AnalysisScopeSourceKind::ScatterplotSelection;
}

std::string NormalizeAnalysisScopeSelectionName(const std::string &name)
{
    std::size_t first = 0;
    while (first < name.size() &&
           std::isspace(static_cast<unsigned char>(name[first]))) ++first;
    std::size_t last = name.size();
    while (last > first &&
           std::isspace(static_cast<unsigned char>(name[last - 1]))) --last;
    return name.substr(first, last - first);
}

bool ValidateAnalysisScopeSelectionName(const std::string &name,
                                        std::string *error)
{
    const std::string normalized = NormalizeAnalysisScopeSelectionName(name);
    if (normalized.empty()) {
        if (error) *error = "Enter a name for the selected observations.";
        return false;
    }
    for (unsigned char ch : normalized) {
        if (ch == '|' || ch == '\r' || ch == '\n' || ch == '\t' || ch < 0x20) {
            if (error) *error = "The selection name cannot contain control characters or |.";
            return false;
        }
    }
    return true;
}

std::string NamedSelectionAnalysisScopeDescription(const std::string &name)
{
    return "Selection: " + NormalizeAnalysisScopeSelectionName(name);
}

std::optional<std::string> AnalysisScopeSelectionName(const AnalysisScope &scope)
{
    static const std::string prefix = "Selection: ";
    if (scope.kind != AnalysisScopeKind::ExplicitRowIds ||
        scope.sourceDescription.rfind(prefix, 0) != 0) {
        return std::nullopt;
    }
    const std::string name = NormalizeAnalysisScopeSelectionName(
        scope.sourceDescription.substr(prefix.size()));
    return ValidateAnalysisScopeSelectionName(name) ?
        std::optional<std::string>(name) : std::nullopt;
}

AnalysisScope AnalysisScopeForSavedSelection(const SavedSelection &selection,
                                             std::size_t totalRows)
{
    return ExplicitAnalysisScope(
        selection.datasetId, selection.originalRowIds, selection.sourceKind,
        NamedSelectionAnalysisScopeDescription(selection.name), totalRows,
        selection.sourceViewId);
}

std::string AnalysisScopeSummary(const AnalysisScope &scope,
                                 std::size_t totalRows,
                                 bool includePrefix)
{
    const std::size_t n = AnalysisScopeRowCount(scope, totalRows);
    std::string text = includePrefix ? "Analysis scope: " : std::string();
    if (scope.kind == AnalysisScopeKind::AllObservations) {
        return text + "All observations · N = " + std::to_string(totalRows);
    }
    text += scope.sourceDescription.empty() ? "Explicit subset" : scope.sourceDescription;
    text += " · " + std::to_string(n) + " of " + std::to_string(totalRows) + " observations";
    if (n == 0) text += " · no valid observations";
    return text;
}

std::string AnalysisScopeWindowSummary(const AnalysisScope &scope,
                                       std::size_t totalRows)
{
    if (scope.kind == AnalysisScopeKind::AllObservations) {
        return "Scope: All observations · N = " + std::to_string(totalRows);
    }
    return "Scope: " + (scope.sourceDescription.empty() ? "Explicit subset" : scope.sourceDescription) +
        " · N = " + std::to_string(AnalysisScopeRowCount(scope, totalRows));
}

} // namespace core
} // namespace rlispstat
