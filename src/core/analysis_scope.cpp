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

AnalysisScope AnalysisScopeExcludingRows(const AnalysisScope &scope,
                                        const std::set<int> &excludedRows,
                                        std::size_t totalRows,
                                        const std::string &sourceViewId)
{
    std::vector<int> included = ResolveAnalysisScopeRowIds(scope, totalRows);
    included.erase(std::remove_if(included.begin(), included.end(),
        [&excludedRows](int row) { return excludedRows.count(row) != 0; }),
        included.end());
    return ExplicitAnalysisScope(scope.datasetId, included,
        AnalysisScopeSourceKind::PlotExclusion,
        "Cases retained in this plot", totalRows, sourceViewId);
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

bool AnalysisScopeMatchesRows(const AnalysisScope &scope,
                              const std::vector<int> &originalRowIds)
{
    if (scope.kind != AnalysisScopeKind::ExplicitRowIds) return false;
    return scope.originalRowIds == NormalizeAnalysisScopeRowIds(originalRowIds);
}

bool AnalysisScopeTracksCurrentSelection(const AnalysisScope &scope)
{
    if (scope.kind != AnalysisScopeKind::ExplicitRowIds ||
        AnalysisScopeSelectionName(scope).has_value()) {
        return false;
    }
    return scope.sourceKind == AnalysisScopeSourceKind::CurrentSelection ||
        scope.sourceKind == AnalysisScopeSourceKind::DataTableRows ||
        scope.sourceKind == AnalysisScopeSourceKind::ScatterplotSelection;
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
    case AnalysisScopeSourceKind::CurrentUnselection: return "current_unselection";
    case AnalysisScopeSourceKind::PlotExclusion: return "plot_exclusion";
    case AnalysisScopeSourceKind::IncludedObservations: return "included_observations";
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
    if (id == "current_unselection") return AnalysisScopeSourceKind::CurrentUnselection;
    if (id == "plot_exclusion") return AnalysisScopeSourceKind::PlotExclusion;
    if (id == "included_observations") return AnalysisScopeSourceKind::IncludedObservations;
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

std::string SavedAnalysisScopeChoiceValue(const std::string &name)
{
    return "saved:" + NormalizeAnalysisScopeSelectionName(name);
}

std::optional<std::string> SavedAnalysisScopeNameFromChoiceValue(
    const std::string &value)
{
    static const std::string prefix = "saved:";
    if (value.rfind(prefix, 0) != 0) return std::nullopt;
    const std::string name = NormalizeAnalysisScopeSelectionName(
        value.substr(prefix.size()));
    return ValidateAnalysisScopeSelectionName(name)
        ? std::optional<std::string>(name) : std::nullopt;
}

std::vector<AnalysisScopeChoice> BuildAnalysisScopeChoices(
    std::size_t selectedRowCount,
    const std::vector<SavedSelection> &savedSelections,
    bool includeUnselected)
{
    std::vector<AnalysisScopeChoice> choices{
        {"all", "Included cases"},
        {"selected", "Current selection (live; " +
            std::to_string(selectedRowCount) + ")"}
    };
    if (includeUnselected) choices.push_back({"unselected", "Unselected rows"});
    for (const SavedSelection &selection : savedSelections) {
        choices.push_back({
            SavedAnalysisScopeChoiceValue(selection.name),
            "Saved scope: " + selection.name + " (" +
                std::to_string(selection.originalRowIds.size()) + ")"
        });
    }
    return choices;
}

std::string AnalysisScopeChoiceValue(const std::string &fallbackScope,
                                     const AnalysisScope &capturedScope,
                                     bool captured)
{
    if (captured) {
        if (const auto savedName = AnalysisScopeSelectionName(capturedScope)) {
            return SavedAnalysisScopeChoiceValue(*savedName);
        }
        if (capturedScope.kind == AnalysisScopeKind::AllObservations ||
            capturedScope.sourceKind == AnalysisScopeSourceKind::IncludedObservations)
            return "all";
        if (capturedScope.sourceKind == AnalysisScopeSourceKind::CurrentUnselection)
            return "unselected";
        if (AnalysisScopeTracksCurrentSelection(capturedScope)) return "selected";
        return "explicit";
    }
    if (fallbackScope == "selected" || fallbackScope == "unselected") {
        return fallbackScope;
    }
    return "all";
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
    text += " · N = " + std::to_string(n) + " of " + std::to_string(totalRows) + " observations";
    if (n == 0) text += " · no valid observations";
    return text;
}

std::string AnalysisScopeCompactSummary(const AnalysisScope &scope, std::size_t totalRows)
{
    const auto name=AnalysisScopeSelectionName(scope);
    const std::string label=name.has_value()?*name:
        scope.kind==AnalysisScopeKind::AllObservations?"All":
        scope.sourceKind==AnalysisScopeSourceKind::CurrentSelection?"Selected":
        scope.sourceKind==AnalysisScopeSourceKind::CurrentUnselection?"Unselected":
        scope.sourceDescription.empty()?"Explicit subset":scope.sourceDescription;
    return label+" · N = "+std::to_string(AnalysisScopeRowCount(scope,totalRows));
}

std::string AnalysisScopeWindowSummary(const AnalysisScope &scope,
                                       std::size_t totalRows)
{
    return "Scope when computed: " + AnalysisScopeSummary(scope, totalRows, false);
}

std::string FrozenAnalysisScopeNotice(const AnalysisScope &computed,
                                      const AnalysisScope &current)
{
    if (computed.kind == current.kind &&
        computed.sourceKind == current.sourceKind &&
        computed.originalRowIds == current.originalRowIds &&
        computed.sourceDescription == current.sourceDescription &&
        computed.sourceViewId == current.sourceViewId &&
        computed.sourceElementId == current.sourceElementId &&
        computed.totalDatasetRows == current.totalDatasetRows &&
        computed.invalidatedRowCount == current.invalidatedRowCount)
        return {};
    return "Automatic refitting is off. Results shown use " +
        AnalysisScopeCompactSummary(computed, computed.totalDatasetRows) +
        "; current global scope is " +
        AnalysisScopeCompactSummary(current, current.totalDatasetRows) + ".";
}

} // namespace core
} // namespace rlispstat
