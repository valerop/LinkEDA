#include "application_state.h"
#include "trellis_scatterplot_model.h"

#include <algorithm>
#include <sstream>
#include <unordered_map>
#include <unordered_set>

namespace rlispstat {
namespace core {

namespace {

void RenameStateMapKey(std::map<std::string, std::string> &values,
                       const std::string &oldName,
                       const std::string &newName)
{
    auto value = values.find(oldName);
    if (value == values.end()) return;
    const std::string contents = value->second;
    values.erase(value);
    values[newName] = contents;
}

} // namespace

DatasetRegistry &ApplicationState::datasets()
{
    return datasets_;
}

const DatasetRegistry &ApplicationState::datasets() const
{
    return datasets_;
}

bool ApplicationState::registerDataset(const DataFrameModel &dataset)
{
    if (dataset.group.empty()) return false;
    const DataFrameModel *previousDataset = datasets_.find(dataset.group);
    const bool replacing = previousDataset != nullptr;
    const int previousRows = previousDataset ? std::max(0, previousDataset->rows) : 0;
    std::vector<std::string> previousIds = previousDataset
        ? previousDataset->stableRowIds : std::vector<std::string>{};
    // The registry assigns these same positional IDs when the producer has
    // no stronger case identity. Reconstruct them before replacing the entry.
    const std::vector<std::string> newIds = dataset.stableRowIds.empty()
        ? StableRowIdsForCount(dataset.group,
            static_cast<std::size_t>(std::max(0, dataset.rows)))
        : dataset.stableRowIds;
    const bool identityExpected = !previousIds.empty() || !newIds.empty();
    // Positional row numbers are safe only when stable identities are absent.
    // If both versions supply unique identities, remap every stored selection.
    std::unordered_map<std::string, int> newRowsById;
    bool remapByIdentity = replacing &&
        previousIds.size() == static_cast<std::size_t>(previousRows) &&
        newIds.size() == static_cast<std::size_t>(std::max(0, dataset.rows));
    if (remapByIdentity) {
        std::unordered_set<std::string> oldIds;
        for (const auto &id : previousIds)
            if (id.empty() || !oldIds.insert(id).second) remapByIdentity = false;
        for (std::size_t i = 0; i < newIds.size(); ++i)
            if (newIds[i].empty() ||
                !newRowsById.emplace(newIds[i], static_cast<int>(i + 1)).second)
                remapByIdentity = false;
    }
    auto remapRow = [&](int row) {
        if (remapByIdentity) {
            if (row < 1 || row > previousRows) return 0;
            auto found = newRowsById.find(previousIds[static_cast<std::size_t>(row - 1)]);
            return found == newRowsById.end() ? 0 : found->second;
        }
        // An incomplete or ambiguous identity vector cannot justify carrying
        // a previous case into a different row of the replacement dataset.
        if (identityExpected) return 0;
        return row >= 1 && row <= dataset.rows ? row : 0;
    };
    auto remapRows = [&](const std::vector<int> &rows, std::size_t *removed) {
        std::vector<int> result;
        result.reserve(rows.size());
        for (int row : rows) {
            const int mapped = remapRow(row);
            if (mapped) result.push_back(mapped);
            else if (removed) ++*removed;
        }
        return NormalizeAnalysisScopeRowIds(result);
    };
    if (replacing) preserveCurrentDatasetVersion(dataset.group);
    datasets_.registerDataset(dataset);
    if (replacing) {
        std::set<std::string> currentColumns;
        for (const DataColumn &column : dataset.columns)
            currentColumns.insert(column.name);
        auto defaults = groupDefaultVariableRoles_.find(dataset.group);
        if (defaults != groupDefaultVariableRoles_.end()) {
            for (auto role = defaults->second.begin(); role != defaults->second.end();) {
                if (!currentColumns.count(role->first)) role = defaults->second.erase(role);
                else ++role;
            }
            if (defaults->second.empty()) groupDefaultVariableRoles_.erase(defaults);
        }
    }
    ensureSelectionGroup(dataset.group);
    if (replacing) {
        auto selected = groupSelections_.find(dataset.group);
        if (selected != groupSelections_.end()) {
            std::set<int> remapped;
            for (int row : selected->second) {
                const int mapped = remapRow(row);
                if (mapped) remapped.insert(mapped);
            }
            if (remapped != selected->second) {
                selected->second = std::move(remapped);
                ++groupSelectionVersions_[dataset.group];
            }
        }
        auto excluded = excludedRows_.find(dataset.group);
        if (excluded != excludedRows_.end()) {
            std::set<int> remapped;
            for (int row : excluded->second) {
                const int mapped = remapRow(row);
                if (mapped) remapped.insert(mapped);
            }
            if (remapped != excluded->second) {
                excluded->second = std::move(remapped);
                ++analysisScopeVersions_[dataset.group];
            }
        }
    }
    auto existing = activeAnalysisScopes_.find(dataset.group);
    if (!replacing || existing == activeAnalysisScopes_.end()) {
        activeAnalysisScopes_[dataset.group] =
            AllObservationsAnalysisScope(dataset.group, static_cast<std::size_t>(std::max(0, dataset.rows)));
        analysisScopeVersions_.try_emplace(dataset.group, 0);
    } else {
        const AnalysisScope previous = existing->second;
        AnalysisScope mapped = previous;
        if (mapped.kind == AnalysisScopeKind::ExplicitRowIds) {
            std::size_t removed = 0;
            mapped.originalRowIds = remapRows(previous.originalRowIds, &removed);
            mapped.invalidatedRowCount += removed;
        }
        AnalysisScope reconciled = ReconcileAnalysisScope(
            mapped, static_cast<std::size_t>(std::max(0, dataset.rows)));
        const bool changed = reconciled.totalDatasetRows != previous.totalDatasetRows ||
            reconciled.originalRowIds != previous.originalRowIds ||
            reconciled.invalidatedRowCount != previous.invalidatedRowCount;
        existing->second = std::move(reconciled);
        if (changed) ++analysisScopeVersions_[dataset.group];
    }
    auto saved = savedSelections_.find(dataset.group);
    if (saved != savedSelections_.end()) {
        for (SavedSelection &selection : saved->second) {
            selection.originalRowIds = remapRows(selection.originalRowIds, nullptr);
        }
    }
    // A categorical colour override is a rule over the current dataset, so a
    // replacement dataset keeps the rule and recomputes its display mapping.
    if (groupColorOverrides_.count(dataset.group)) refreshColorOverride(dataset.group);
    return true;
}

std::string ApplicationState::dataSheetGroupForPlotGroup(const std::string &group) const
{
    if (datasets_.find(group)) return group;
    for (const auto &entry : plots_) {
        if (!entry.second || entry.second->group != group) continue;
        const auto &source = entry.second->codeReference.provenance.dataVersion.datasetId;
        if (!source.empty()) return source;
    }
    return group;
}

bool ApplicationState::eraseDataset(const std::string &group)
{
    // A data sheet may be closed while tables or plots that reference its
    // current immutable version remain open. Preserve that single shared
    // version before unregistering the live dataset.
    preserveCurrentDatasetVersion(group);
    if (!datasets_.erase(group)) return false;
    removeGroupDomainState(group);
    return true;
}

bool ApplicationState::closeDatasetAndAnalyses(const std::string &group)
{
    if (!datasets_.contains(group)) return false;
    for (auto it = plots_.begin(); it != plots_.end();) {
        const PlotModel *plot = it->second;
        const bool belongsToDataset = plot &&
            (plot->group == group ||
             plot->codeReference.provenance.dataVersion.datasetId == group);
        if (belongsToDataset) {
            if (activePlotId_ == it->first) activePlotId_.clear();
            it = plots_.erase(it);
        } else {
            ++it;
        }
    }
    if (activePlotId_.empty() && !plots_.empty()) activePlotId_ = plots_.begin()->first;
    for (auto it = outputCodeReferences_.begin(); it != outputCodeReferences_.end();) {
        if (it->second.provenance.dataVersion.datasetId == group)
            it = outputCodeReferences_.erase(it);
        else ++it;
    }
    datasets_.erase(group);
    removeGroupDomainState(group);
    // Linear models can have several independently named windows for one
    // dataset. removeGroupDomainState handles the legacy group-keyed model;
    // remove every instance as well.
    for (auto it = groupModels_.begin(); it != groupModels_.end();) {
        if (it->second.group == group) {
            linearModelFits_.erase(it->first);
            it = groupModels_.erase(it);
        } else ++it;
    }
    pruneUnreferencedHistoricalDatasetVersions();
    return true;
}

std::map<std::string, std::set<int>> &ApplicationState::groupSelections()
{
    return groupSelections_;
}

const std::map<std::string, std::set<int>> &ApplicationState::groupSelections() const
{
    return groupSelections_;
}

std::map<std::string, int> &ApplicationState::groupSelectionVersions()
{
    return groupSelectionVersions_;
}

const std::map<std::string, int> &ApplicationState::groupSelectionVersions() const
{
    return groupSelectionVersions_;
}

std::map<std::string, std::string> &ApplicationState::groupSelectedColors()
{
    return groupSelectedColors_;
}

const std::map<std::string, std::string> &ApplicationState::groupSelectedColors() const
{
    return groupSelectedColors_;
}

std::map<std::string, std::map<int, std::string>> &ApplicationState::groupPointColors()
{
    return groupPointColors_;
}

const std::map<std::string, std::map<int, std::string>> &ApplicationState::groupPointColors() const
{
    return groupPointColors_;
}

std::string &ApplicationState::activePlotId()
{
    return activePlotId_;
}

const std::string &ApplicationState::activePlotId() const
{
    return activePlotId_;
}

std::map<std::string, PlotModel *> &ApplicationState::plots()
{
    return plots_;
}

const std::map<std::string, PlotModel *> &ApplicationState::plots() const
{
    return plots_;
}

std::map<std::string, GroupModelState> &ApplicationState::groupModels()
{
    return groupModels_;
}

const std::map<std::string, GroupModelState> &ApplicationState::groupModels() const
{
    return groupModels_;
}

std::map<std::string, CorrelationMatrixState> &ApplicationState::correlationMatrices()
{
    return correlationMatrices_;
}

const std::map<std::string, CorrelationMatrixState> &ApplicationState::correlationMatrices() const
{
    return correlationMatrices_;
}

std::map<std::string, DimensionalityState> &ApplicationState::dimensionalityModels()
{
    return dimensionalityModels_;
}

const std::map<std::string, DimensionalityState> &ApplicationState::dimensionalityModels() const
{
    return dimensionalityModels_;
}

std::map<std::string, ScaleAnalysisState> &ApplicationState::scaleAnalyses()
{
    return scaleAnalyses_;
}

const std::map<std::string, ScaleAnalysisState> &ApplicationState::scaleAnalyses() const
{
    return scaleAnalyses_;
}

std::map<std::string, DendrogramState> &ApplicationState::dendrograms()
{
    return dendrograms_;
}

const std::map<std::string, DendrogramState> &ApplicationState::dendrograms() const
{
    return dendrograms_;
}

std::map<std::string, GeneralizedGLMState> &ApplicationState::generalizedGLMs()
{
    return generalizedGLMs_;
}

const std::map<std::string, GeneralizedGLMState> &ApplicationState::generalizedGLMs() const
{
    return generalizedGLMs_;
}

std::map<std::string, NativeMixedModelState> &ApplicationState::nativeMixedModels()
{
    return nativeMixedModels_;
}

const std::map<std::string, NativeMixedModelState> &ApplicationState::nativeMixedModels() const
{
    return nativeMixedModels_;
}

std::map<std::string, GLMFitSummary> &ApplicationState::linearModelFits()
{
    return linearModelFits_;
}

const std::map<std::string, GLMFitSummary> &ApplicationState::linearModelFits() const
{
    return linearModelFits_;
}

std::map<std::string, RegressionComparisonState> &ApplicationState::regressionComparisons()
{
    return regressionComparisons_;
}

std::map<std::string, GeneralizedComparisonState> &ApplicationState::generalizedComparisons()
{
    return generalizedComparisons_;
}

const std::map<std::string, GeneralizedComparisonState> &ApplicationState::generalizedComparisons() const
{
    return generalizedComparisons_;
}

const std::map<std::string, RegressionComparisonState> &ApplicationState::regressionComparisons() const
{
    return regressionComparisons_;
}

std::map<std::string, ModelTrellisState> &ApplicationState::modelTrellises()
{
    return modelTrellises_;
}

const std::map<std::string, ModelTrellisState> &ApplicationState::modelTrellises() const
{
    return modelTrellises_;
}

bool ApplicationState::selectedRows(const std::string &group, std::set<int> &rows) const
{
    auto it = groupSelections_.find(group);
    if (it == groupSelections_.end()) {
        return false;
    }
    rows = it->second;
    return true;
}

void ApplicationState::ensureSelectionGroup(const std::string &group)
{
    if (group.empty()) return;
    groupSelections_.try_emplace(group);
    groupSelectionVersions_.try_emplace(group, 0);
}

bool ApplicationState::hasSelectionGroup(const std::string &group) const
{
    return groupSelections_.find(group) != groupSelections_.end();
}

bool ApplicationState::setSelectedRows(const std::string &group,
                                       const std::set<int> &rows,
                                       int *selectionVersion)
{
    auto it = groupSelections_.find(group);
    if (it == groupSelections_.end()) {
        return false;
    }
    std::set<int> next = NormalizeCaseSet(rows);
    if (next == it->second) {
        if (selectionVersion) {
            *selectionVersion = groupSelectionVersions_[group];
        }
        return false;
    }
    auto scope = activeAnalysisScopes_.find(group);
    if (scope != activeAnalysisScopes_.end() && (AnalysisScopeTracksCurrentSelection(scope->second) || scope->second.sourceKind == AnalysisScopeSourceKind::CurrentUnselection))
        ++analysisScopeVersions_[group];
    it->second = std::move(next);
    const int version = ++groupSelectionVersions_[group];
    if (selectionVersion) {
        *selectionVersion = version;
    }
    return true;
}

bool ApplicationState::clearSelectedRows(const std::string &group, int *selectionVersion)
{
    return setSelectedRows(group, {}, selectionVersion);
}

std::set<int> ApplicationState::excludedRows(const std::string &datasetId) const
{
    auto found = excludedRows_.find(datasetId);
    return found == excludedRows_.end() ? std::set<int>{} : found->second;
}

bool ApplicationState::isRowExcluded(const std::string &datasetId,
                                     int originalRowId) const
{
    auto found = excludedRows_.find(datasetId);
    return found != excludedRows_.end() &&
        found->second.count(originalRowId) != 0;
}

bool ApplicationState::setExcludedRows(const std::string &datasetId,
                                       const std::set<int> &rows,
                                       AnalysisScopeChangeEvent *event,
                                       std::string *error)
{
    const DataFrameModel *dataset = datasets_.find(datasetId);
    if (!dataset) {
        if (error) *error = "Exclusion dataset is not registered.";
        return false;
    }
    for (int row : rows) {
        if (row < 1 || row > dataset->rows) {
            if (error) *error = "Excluded case is outside the dataset.";
            return false;
        }
    }
    const AnalysisScope previous = activeAnalysisScope(datasetId);
    // Excluding a case removes it from the live brush. Saved selections keep
    // their historical members; the inclusion mask filters their active scope
    // and including a case again can restore its saved membership.
    auto selected = groupSelections_.find(datasetId);
    if (selected != groupSelections_.end()) {
        std::set<int> remaining = selected->second;
        const auto previouslyExcluded = excludedRows_.find(datasetId);
        for (int row : rows) {
            if (previouslyExcluded == excludedRows_.end() ||
                previouslyExcluded->second.count(row) == 0)
                remaining.erase(row);
        }
        if (remaining != selected->second)
            setSelectedRows(datasetId, remaining);
    }
    if (rows.empty()) excludedRows_.erase(datasetId);
    else excludedRows_[datasetId] = rows;
    const std::size_t version = ++analysisScopeVersions_[datasetId];
    if (event) *event = {datasetId, previous, activeAnalysisScope(datasetId), version};
    return true;
}

AnalysisScope ApplicationState::baseAnalysisScope(const std::string &datasetId) const
{
    const DataFrameModel *dataset = datasets_.find(datasetId);
    const std::size_t totalRows = dataset
        ? static_cast<std::size_t>(std::max(0, dataset->rows)) : 0;
    auto found = activeAnalysisScopes_.find(datasetId);
    if (found == activeAnalysisScopes_.end())
        return AllObservationsAnalysisScope(datasetId, totalRows);
    AnalysisScope current = ReconcileAnalysisScope(found->second, totalRows);
    if (AnalysisScopeTracksCurrentSelection(current) || current.sourceKind == AnalysisScopeSourceKind::CurrentUnselection) {
        std::set<int> selected;
        selectedRows(datasetId, selected);
        current.originalRowIds.clear();
        if (current.sourceKind == AnalysisScopeSourceKind::CurrentUnselection) {
            for (std::size_t row=1; row<=totalRows; ++row)
                if (!selected.count(static_cast<int>(row))) current.originalRowIds.push_back(static_cast<int>(row));
        } else current.originalRowIds.assign(selected.begin(), selected.end());
        current = ReconcileAnalysisScope(current, totalRows);
    }
    return current;
}

AnalysisScope ApplicationState::activeAnalysisScope(const std::string &datasetId) const
{
    AnalysisScope scope = baseAnalysisScope(datasetId);
    const auto excluded = excludedRows_.find(datasetId);
    if (excluded == excludedRows_.end() || excluded->second.empty()) return scope;
    const std::size_t totalRows = scope.totalDatasetRows;
    std::vector<int> rows = ResolveAnalysisScopeRowIds(scope, totalRows);
    rows.erase(std::remove_if(rows.begin(), rows.end(),
        [&excluded](int row) { return excluded->second.count(row) != 0; }),
        rows.end());
    if (scope.kind == AnalysisScopeKind::AllObservations) {
        scope.sourceKind = AnalysisScopeSourceKind::IncludedObservations;
        scope.sourceDescription = "Included cases";
    }
    scope.kind = AnalysisScopeKind::ExplicitRowIds;
    scope.originalRowIds = std::move(rows);
    return scope;
}

bool ApplicationState::setActiveAnalysisScope(const AnalysisScope &scope,
                                              AnalysisScopeChangeEvent *event,
                                              std::string *error)
{
    const DataFrameModel *dataset = datasets_.find(scope.datasetId);
    if (!dataset) {
        if (error) *error = "Analysis scope dataset is not registered.";
        return false;
    }
    const std::size_t totalRows = static_cast<std::size_t>(std::max(0, dataset->rows));
    AnalysisScope normalized = scope;
    normalized.totalDatasetRows = totalRows;
    if (normalized.kind == AnalysisScopeKind::ExplicitRowIds) {
        normalized.originalRowIds = NormalizeAnalysisScopeRowIds(normalized.originalRowIds);
    } else {
        normalized.originalRowIds.clear();
        normalized.sourceKind = AnalysisScopeSourceKind::AllData;
        normalized.sourceDescription = "All observations";
    }
    if (!ValidateAnalysisScope(normalized, scope.datasetId, totalRows, error)) return false;
    const std::optional<std::string> selectionName = AnalysisScopeSelectionName(normalized);
    if (selectionName.has_value()) {
        std::vector<SavedSelection> &selections = savedSelections_[normalized.datasetId];
        auto existing = std::find_if(selections.begin(), selections.end(),
            [&](const SavedSelection &candidate) { return candidate.name == *selectionName; });
        if (existing == selections.end()) {
            const auto excluded = excludedRows_.find(normalized.datasetId);
            if (excluded != excludedRows_.end()) {
                normalized.originalRowIds.erase(std::remove_if(
                    normalized.originalRowIds.begin(), normalized.originalRowIds.end(),
                    [&excluded](int row) { return excluded->second.count(row) != 0; }),
                    normalized.originalRowIds.end());
            }
        }
        SavedSelection saved;
        saved.datasetId = normalized.datasetId;
        saved.name = *selectionName;
        saved.originalRowIds = normalized.originalRowIds;
        saved.sourceKind = normalized.sourceKind;
        saved.sourceViewId = normalized.sourceViewId;
        if (existing == selections.end()) selections.push_back(std::move(saved));
        else *existing = std::move(saved);
    }
    AnalysisScope previous = activeAnalysisScope(scope.datasetId);
    activeAnalysisScopes_[scope.datasetId] = normalized;
    const std::size_t version = ++analysisScopeVersions_[scope.datasetId];
    if (event) *event = {scope.datasetId, previous, activeAnalysisScope(scope.datasetId), version};
    return true;
}

bool ApplicationState::setActiveAnalysisScopeFromSelection(
    const std::string &datasetId,
    AnalysisScopeSourceKind sourceKind,
    const std::string &description,
    const std::optional<std::string> &sourceViewId,
    AnalysisScopeChangeEvent *event,
    std::string *error)
{
    const DataFrameModel *dataset = datasets_.find(datasetId);
    if (!dataset) {
        if (error) *error = "Analysis scope dataset is not registered.";
        return false;
    }
    std::set<int> selected;
    if (!selectedRows(datasetId, selected) || selected.empty()) {
        if (error) *error = "Select one or more observations first.";
        return false;
    }
    std::vector<int> rows(selected.begin(), selected.end());
    AnalysisScope scope = ExplicitAnalysisScope(
        datasetId, rows, sourceKind, description,
        static_cast<std::size_t>(std::max(0, dataset->rows)), sourceViewId);
    return setActiveAnalysisScope(scope, event, error);
}

bool ApplicationState::setActiveAnalysisScopeFromUnselected(const std::string &datasetId,
    AnalysisScopeChangeEvent *event, std::string *error)
{
    const auto *dataset = datasets_.find(datasetId);
    if (!dataset) { if(error) *error="Analysis scope dataset is not registered."; return false; }
    std::set<int> selected; selectedRows(datasetId, selected);
    std::vector<int> rows;
    for(int row=1; row<=dataset->rows; ++row) if(!selected.count(row)) rows.push_back(row);
    return setActiveAnalysisScope(ExplicitAnalysisScope(datasetId, rows,
        AnalysisScopeSourceKind::CurrentUnselection, "Unselected observations", dataset->rows), event, error);
}

bool ApplicationState::resetActiveAnalysisScopeToAllObservations(
    const std::string &datasetId,
    AnalysisScopeChangeEvent *event,
    std::string *error)
{
    const DataFrameModel *dataset = datasets_.find(datasetId);
    if (!dataset) {
        if (error) *error = "Analysis scope dataset is not registered.";
        return false;
    }
    return setActiveAnalysisScope(
        AllObservationsAnalysisScope(datasetId,
            static_cast<std::size_t>(std::max(0, dataset->rows))), event, error);
}

bool ApplicationState::saveCurrentSelectionAsAnalysisScope(
    const std::string &datasetId, const std::string &name,
    AnalysisScopeSourceKind sourceKind,
    const std::optional<std::string> &sourceViewId,
    AnalysisScopeChangeEvent *event,
    std::string *error)
{
    const DataFrameModel *dataset = datasets_.find(datasetId);
    if (!dataset) {
        if (error) *error = "Analysis scope dataset is not registered.";
        return false;
    }
    const std::string normalizedName = NormalizeAnalysisScopeSelectionName(name);
    if (!ValidateAnalysisScopeSelectionName(normalizedName, error)) return false;
    std::set<int> selected;
    if (!selectedRows(datasetId, selected) || selected.empty()) {
        if (error) *error = "Select one or more observations first.";
        return false;
    }
    const auto excluded = excludedRows_.find(datasetId);
    if (excluded != excludedRows_.end()) {
        for (int row : excluded->second) selected.erase(row);
    }
    if (selected.empty()) {
        if (error) *error = "Select one or more included observations first.";
        return false;
    }
    SavedSelection saved;
    saved.datasetId = datasetId;
    saved.name = normalizedName;
    saved.originalRowIds = NormalizeAnalysisScopeRowIds(
        std::vector<int>(selected.begin(), selected.end()));
    saved.sourceKind = sourceKind;
    saved.sourceViewId = sourceViewId;
    std::vector<SavedSelection> &selections = savedSelections_[datasetId];
    auto existing = std::find_if(selections.begin(), selections.end(),
        [&](const SavedSelection &candidate) { return candidate.name == saved.name; });
    if (existing == selections.end()) selections.push_back(std::move(saved));
    else *existing = std::move(saved);
    return setActiveAnalysisScope(ExplicitAnalysisScope(
        datasetId, std::vector<int>(selected.begin(), selected.end()), sourceKind,
        NamedSelectionAnalysisScopeDescription(normalizedName),
        static_cast<std::size_t>(std::max(0, dataset->rows)), sourceViewId),
        event, error);
}

std::vector<int> ApplicationState::resolveActiveAnalysisRowIds(
    const std::string &datasetId) const
{
    const DataFrameModel *dataset = datasets_.find(datasetId);
    if (!dataset) return {};
    return ResolveAnalysisScopeRowIds(activeAnalysisScope(datasetId),
        static_cast<std::size_t>(std::max(0, dataset->rows)));
}

bool ApplicationState::captureAnalysisScope(const std::string &datasetId,
                                             AnalysisScope &snapshot,
                                             bool &captured,
                                             std::string *legacyScope) const
{
    const AnalysisScope current = activeAnalysisScope(datasetId);
    const bool changed = !captured || snapshot.datasetId != current.datasetId ||
        snapshot.kind != current.kind || snapshot.originalRowIds != current.originalRowIds ||
        snapshot.totalDatasetRows != current.totalDatasetRows ||
        snapshot.sourceDescription != current.sourceDescription ||
        snapshot.sourceKind != current.sourceKind ||
        snapshot.sourceViewId != current.sourceViewId ||
        snapshot.sourceElementId != current.sourceElementId ||
        snapshot.invalidatedRowCount != current.invalidatedRowCount;
    snapshot = current;
    captured = true;
    // Compatibility wire fields are derived, never independent settings.
    if (legacyScope) *legacyScope = current.kind == AnalysisScopeKind::AllObservations
        ? "all" : "selected";
    return changed;
}

std::size_t ApplicationState::analysisScopeVersion(const std::string &datasetId) const
{
    auto found = analysisScopeVersions_.find(datasetId);
    return found == analysisScopeVersions_.end() ? 0 : found->second;
}

std::vector<SavedSelection> ApplicationState::savedSelections(
    const std::string &datasetId) const
{
    auto found = savedSelections_.find(datasetId);
    if (found == savedSelections_.end()) return {};
    const DataFrameModel *dataset = datasets_.find(datasetId);
    const int maximum = dataset ? std::max(0, dataset->rows) : 0;
    std::vector<SavedSelection> result = found->second;
    for (SavedSelection &selection : result) {
        selection.originalRowIds.erase(std::remove_if(
            selection.originalRowIds.begin(), selection.originalRowIds.end(),
            [maximum](int row) { return row <= 0 || row > maximum; }),
            selection.originalRowIds.end());
    }
    return result;
}

bool ApplicationState::replaceSavedSelections(
    const std::string &datasetId, const std::vector<SavedSelection> &selections,
    std::string *error)
{
    const DataFrameModel *dataset = datasets_.find(datasetId);
    if (!dataset) {
        if (error) *error = "Dataset `" + datasetId + "` is not registered.";
        return false;
    }
    std::vector<SavedSelection> normalized;
    normalized.reserve(selections.size());
    std::set<std::string> names;
    for (SavedSelection selection : selections) {
        selection.datasetId = datasetId;
        selection.name = NormalizeAnalysisScopeSelectionName(selection.name);
        if (!ValidateAnalysisScopeSelectionName(selection.name, error)) return false;
        if (!names.insert(selection.name).second) {
            if (error) *error = "Saved selection names must be unique.";
            return false;
        }
        selection.originalRowIds = NormalizeAnalysisScopeRowIds(selection.originalRowIds);
        selection.originalRowIds.erase(std::remove_if(
            selection.originalRowIds.begin(), selection.originalRowIds.end(),
            [dataset](int row) { return row <= 0 || row > dataset->rows; }),
            selection.originalRowIds.end());
        normalized.push_back(std::move(selection));
    }
    savedSelections_[datasetId] = std::move(normalized);
    return true;
}

std::optional<SavedSelection> ApplicationState::savedSelection(
    const std::string &datasetId, const std::string &name) const
{
    const std::vector<SavedSelection> selections = savedSelections(datasetId);
    auto found = std::find_if(selections.begin(), selections.end(),
        [&](const SavedSelection &candidate) { return candidate.name == name; });
    return found == selections.end() ? std::nullopt :
        std::optional<SavedSelection>(*found);
}

bool ApplicationState::resolveSavedAnalysisScopeChoice(
    const std::string &datasetId, const std::string &choiceValue,
    AnalysisScope &scope, std::string *error) const
{
    const DataFrameModel *dataset = datasets_.find(datasetId);
    const std::optional<std::string> name =
        SavedAnalysisScopeNameFromChoiceValue(choiceValue);
    const std::optional<SavedSelection> selection = name.has_value()
        ? savedSelection(datasetId, *name) : std::nullopt;
    if (!dataset || !selection.has_value()) {
        if (error) *error = "The saved scope is no longer available.";
        return false;
    }
    scope = AnalysisScopeForSavedSelection(
        *selection, static_cast<std::size_t>(std::max(0, dataset->rows)));
    return true;
}

bool ApplicationState::activateSavedSelection(
    const std::string &datasetId, const std::string &name,
    AnalysisScopeChangeEvent *event, std::string *error)
{
    const DataFrameModel *dataset = datasets_.find(datasetId);
    const std::optional<SavedSelection> selection = savedSelection(datasetId, name);
    if (!dataset || !selection.has_value()) {
        if (error) *error = "The saved selection is no longer available.";
        return false;
    }
    return setActiveAnalysisScope(AnalysisScopeForSavedSelection(
        *selection, static_cast<std::size_t>(std::max(0, dataset->rows))), event, error);
}

bool ApplicationState::addSavedSelectionToActiveScope(
    const std::string &datasetId, const std::string &savedName,
    const std::string &resultName, AnalysisScopeChangeEvent *event,
    std::string *error)
{
    if (!ValidateAnalysisScopeSelectionName(resultName, error)) return false;
    const DataFrameModel *dataset = datasets_.find(datasetId);
    const std::optional<SavedSelection> selection = savedSelection(datasetId, savedName);
    if (!dataset || !selection.has_value()) {
        if (error) *error = "The saved selection is no longer available.";
        return false;
    }
    const AnalysisScope active = activeAnalysisScope(datasetId);
    if (active.kind == AnalysisScopeKind::AllObservations) {
        if (error) *error = "Choose a selected or saved scope before adding another selection.";
        return false;
    }
    std::vector<int> combined = active.originalRowIds;
    combined.insert(combined.end(), selection->originalRowIds.begin(),
                    selection->originalRowIds.end());
    const auto excluded = excludedRows_.find(datasetId);
    if (excluded != excludedRows_.end()) {
        combined.erase(std::remove_if(combined.begin(), combined.end(),
            [&excluded](int row) { return excluded->second.count(row) != 0; }),
            combined.end());
    }
    AnalysisScope scope = ExplicitAnalysisScope(
        datasetId, combined, AnalysisScopeSourceKind::CurrentSelection,
        NamedSelectionAnalysisScopeDescription(resultName),
        static_cast<std::size_t>(std::max(0, dataset->rows)));
    return setActiveAnalysisScope(scope, event, error);
}

bool ApplicationState::setSelectedColor(const std::string &group, const std::string &color)
{
    if (!hasSelectionGroup(group)) return false;
    groupSelectedColors_[group] = color;
    return true;
}

std::string ApplicationState::selectedColor(const std::string &group) const
{
    auto it = groupSelectedColors_.find(group);
    return it == groupSelectedColors_.end() ? "black" : it->second;
}

bool ApplicationState::setPointColor(const std::string &group, int row, const std::string &color)
{
    if (!hasSelectionGroup(group) || row <= 0) return false;
    // Explicit persistent recolouring always reveals the persistent layer.
    cancelColorOverride(group);
    groupPointColors_[group][row] = color;
    return true;
}

std::vector<int> ApplicationState::clearPointColors(const std::string &group,
                                                     const std::vector<int> &rows)
{
    std::vector<int> changed;
    // Clearing colours is also an explicit persistent-colour edit.
    cancelColorOverride(group);
    auto colors = groupPointColors_.find(group);
    if (colors == groupPointColors_.end()) return changed;
    if (rows.empty()) {
        changed.reserve(colors->second.size());
        for (const auto &entry : colors->second) changed.push_back(entry.first);
        colors->second.clear();
        return changed;
    }
    for (int row : rows) {
        if (row > 0 && colors->second.erase(row) > 0) changed.push_back(row);
    }
    return changed;
}

std::vector<std::pair<int, std::string>> ApplicationState::pointColors(const std::string &group) const
{
    std::vector<std::pair<int, std::string>> result;
    auto colors = groupPointColors_.find(group);
    if (colors == groupPointColors_.end()) return result;
    result.reserve(colors->second.size());
    for (const auto &entry : colors->second) result.push_back(entry);
    return result;
}

std::vector<std::pair<int, std::string>> ApplicationState::displayPointColors(
    const std::string &group) const
{
    auto override = groupColorOverrides_.find(group);
    if (override == groupColorOverrides_.end()) return pointColors(group);
    std::vector<std::pair<int, std::string>> result;
    result.reserve(override->second.rowColors.size());
    for (const auto &entry : override->second.rowColors) result.push_back(entry);
    return result;
}

std::string ApplicationState::displayPointColor(const std::string &group, int row) const
{
    auto override = groupColorOverrides_.find(group);
    if (override != groupColorOverrides_.end()) {
        auto color = override->second.rowColors.find(row);
        return color == override->second.rowColors.end() ? std::string() : color->second;
    }
    auto persistent = groupPointColors_.find(group);
    if (persistent == groupPointColors_.end()) return {};
    auto color = persistent->second.find(row);
    return color == persistent->second.end() ? std::string() : color->second;
}

std::optional<ColorOverrideState> ApplicationState::colorOverride(
    const std::string &group) const
{
    auto found = groupColorOverrides_.find(group);
    if (found == groupColorOverrides_.end()) return std::nullopt;
    return found->second;
}

bool ApplicationState::setColorOverride(const std::string &group,
                                        const std::string &variable,
                                        std::string *error)
{
    if (variable.empty() || variable == ".") {
        cancelColorOverride(group);
        return true;
    }
    const DataFrameModel *dataset = datasets_.find(group);
    if (!dataset) {
        if (error) *error = "The dataset is no longer available.";
        return false;
    }
    PlotModel mapping;
    mapping.group = group;
    if (!SetPlotColorByVariable(mapping, *dataset, variable, error)) return false;
    ColorOverrideState state;
    state.datasetId = group;
    state.variable = variable;
    state.rowColors = std::move(mapping.colorByRowColors);
    state.legendItems = std::move(mapping.colorByLegendItems);
    state.legendRows = std::move(mapping.colorByLegendRows);
    groupColorOverrides_[group] = state;
    for (auto &[id, plot] : plots_) {
        (void)id;
        if (!plot || plot->group != group) continue;
        plot->colorByVariable = state.variable;
        plot->colorByRowColors = state.rowColors;
        plot->colorByLegendItems = state.legendItems;
        plot->colorByLegendRows = state.legendRows;
    }
    for (auto &[id, dendrogram] : dendrograms_) {
        (void)id;
        if (dendrogram.group != group) continue;
        dendrogram.colorByVariable = state.variable;
        dendrogram.colorByRowColors = state.rowColors;
        dendrogram.colorByLegendItems = state.legendItems;
        dendrogram.colorByLegendRows = state.legendRows;
    }
    return true;
}

bool ApplicationState::refreshColorOverride(const std::string &group,
                                            std::string *error)
{
    auto found = groupColorOverrides_.find(group);
    if (found == groupColorOverrides_.end()) return true;
    const std::string variable = found->second.variable;
    if (setColorOverride(group, variable, error)) return true;
    // A deleted or no-longer-categorical variable cannot leave a hidden stale
    // display rule behind.
    cancelColorOverride(group);
    return false;
}

bool ApplicationState::cancelColorOverride(const std::string &group)
{
    const bool changed = groupColorOverrides_.erase(group) > 0;
    for (auto &[id, plot] : plots_) {
        (void)id;
        if (!plot || plot->group != group) continue;
        plot->colorByVariable.clear();
        plot->colorByRowColors.clear();
        plot->colorByLegendItems.clear();
        plot->colorByLegendRows.clear();
    }
    for (auto &[id, dendrogram] : dendrograms_) {
        (void)id;
        if (dendrogram.group != group) continue;
        dendrogram.colorByVariable.clear();
        dendrogram.colorByRowColors.clear();
        dendrogram.colorByLegendItems.clear();
        dendrogram.colorByLegendRows.clear();
    }
    return changed;
}

bool ApplicationState::promoteColorOverrideToPersistent(const std::string &group)
{
    auto found = groupColorOverrides_.find(group);
    if (found == groupColorOverrides_.end()) return false;
    groupPointColors_[group] = found->second.rowColors;
    cancelColorOverride(group);
    return true;
}

bool ApplicationState::saveCurrentColorsAsScheme(const std::string &group,
                                                 const std::string &name,
                                                 std::string *error)
{
    const std::string normalized = NormalizeAnalysisScopeSelectionName(name);
    if (!ValidateAnalysisScopeSelectionName(normalized, error)) return false;
    const DataFrameModel *dataset = datasets_.find(group);
    if (!dataset || dataset->stableRowIds.size() !=
            static_cast<std::size_t>(std::max(0, dataset->rows))) {
        if (error) *error = "The dataset has no stable case identifiers.";
        return false;
    }
    NamedColorScheme scheme;
    scheme.datasetId = group;
    scheme.name = normalized;
    scheme.dataVersion = dataset->dataVersion;
    const AnalysisScope active = activeAnalysisScope(group);
    const std::vector<int> rows = ResolveAnalysisScopeRowIds(
        active, static_cast<std::size_t>(std::max(0, dataset->rows)));
    scheme.scope = ExplicitAnalysisScope(group, rows,
        AnalysisScopeSourceKind::OtherExplicitSubset,
        "Color scheme: " + normalized,
        static_cast<std::size_t>(std::max(0, dataset->rows)));
    const auto persistent = groupPointColors_.find(group);
    for (int row : rows) {
        const std::string &stableId = dataset->stableRowIds[static_cast<std::size_t>(row - 1)];
        scheme.scopeStableRowIds.push_back(stableId);
        if (persistent == groupPointColors_.end()) continue;
        auto color = persistent->second.find(row);
        if (color != persistent->second.end())
            scheme.colorsByStableRowId[stableId] = color->second;
    }
    auto &schemes = groupColorSchemes_[group];
    auto existing = std::find_if(schemes.begin(), schemes.end(),
        [&](const NamedColorScheme &candidate) { return candidate.name == normalized; });
    if (existing == schemes.end()) schemes.push_back(std::move(scheme));
    else *existing = std::move(scheme);
    return true;
}

std::vector<NamedColorScheme> ApplicationState::colorSchemes(
    const std::string &group) const
{
    auto found = groupColorSchemes_.find(group);
    return found == groupColorSchemes_.end()
        ? std::vector<NamedColorScheme>{} : found->second;
}

std::optional<NamedColorScheme> ApplicationState::colorScheme(
    const std::string &group, const std::string &name) const
{
    auto found = groupColorSchemes_.find(group);
    if (found == groupColorSchemes_.end()) return std::nullopt;
    auto scheme = std::find_if(found->second.begin(), found->second.end(),
        [&](const NamedColorScheme &candidate) { return candidate.name == name; });
    if (scheme == found->second.end()) return std::nullopt;
    return *scheme;
}

ColorSchemeCompatibility ApplicationState::colorSchemeCompatibility(
    const std::string &group, const std::string &name, std::string *error) const
{
    ColorSchemeCompatibility result;
    const DataFrameModel *dataset = datasets_.find(group);
    const auto scheme = colorScheme(group, name);
    if (!dataset || !scheme) {
        if (error) *error = !dataset ? "The dataset is no longer available."
                                    : "The named color scheme does not exist.";
        return result;
    }
    result.sameDataVersion = dataset->dataVersion == scheme->dataVersion;
    const std::vector<int> currentRows = resolveActiveAnalysisRowIds(group);
    std::set<std::string> currentIds;
    for (int row : currentRows)
        if (row > 0 && row <= dataset->rows)
            currentIds.insert(dataset->stableRowIds[static_cast<std::size_t>(row - 1)]);
    const std::set<std::string> schemeIds(scheme->scopeStableRowIds.begin(),
                                          scheme->scopeStableRowIds.end());
    result.currentScopeCount = currentIds.size();
    result.schemeScopeCount = schemeIds.size();
    for (const std::string &id : currentIds)
        if (schemeIds.count(id)) ++result.matchingObservationCount;
    for (const std::string &id : schemeIds)
        if (!currentIds.count(id)) ++result.missingObservationCount;
    result.exactScope = result.sameDataVersion && currentIds == schemeIds;
    return result;
}

bool ApplicationState::applyColorSchemeToMatchingObservations(
    const std::string &group, const std::string &name,
    std::vector<int> *changedRows, std::string *error)
{
    const DataFrameModel *dataset = datasets_.find(group);
    const auto scheme = colorScheme(group, name);
    if (!dataset || !scheme) {
        if (error) *error = !dataset ? "The dataset is no longer available."
                                    : "The named color scheme does not exist.";
        return false;
    }
    cancelColorOverride(group);
    const std::vector<int> resolvedRows = resolveActiveAnalysisRowIds(group);
    const std::set<int> currentRows(resolvedRows.begin(), resolvedRows.end());
    std::set<std::string> schemeIds(scheme->scopeStableRowIds.begin(),
                                    scheme->scopeStableRowIds.end());
    std::vector<int> changed;
    auto &persistent = groupPointColors_[group];
    for (int row : currentRows) {
        if (row <= 0 || row > dataset->rows) continue;
        const std::string &stableId = dataset->stableRowIds[static_cast<std::size_t>(row - 1)];
        if (!schemeIds.count(stableId)) continue;
        auto saved = scheme->colorsByStableRowId.find(stableId);
        if (saved == scheme->colorsByStableRowId.end()) persistent.erase(row);
        else persistent[row] = saved->second;
        changed.push_back(row);
    }
    if (changedRows) *changedRows = std::move(changed);
    return true;
}

bool ApplicationState::switchToColorSchemeScopeAndApply(
    const std::string &group, const std::string &name,
    AnalysisScopeChangeEvent *scopeEvent, std::vector<int> *changedRows,
    std::string *error)
{
    const DataFrameModel *dataset = datasets_.find(group);
    const auto scheme = colorScheme(group, name);
    if (!dataset || !scheme) {
        if (error) *error = !dataset ? "The dataset is no longer available."
                                    : "The named color scheme does not exist.";
        return false;
    }
    std::map<std::string, int> rowsById;
    for (int row = 1; row <= dataset->rows; ++row)
        rowsById[dataset->stableRowIds[static_cast<std::size_t>(row - 1)]] = row;
    std::vector<int> rows;
    for (const std::string &id : scheme->scopeStableRowIds) {
        auto found = rowsById.find(id);
        if (found == rowsById.end()) {
            if (error) *error = "The scheme scope cannot be restored because some saved observations are missing.";
            return false;
        }
        rows.push_back(found->second);
    }
    AnalysisScope scope = ExplicitAnalysisScope(group, rows,
        AnalysisScopeSourceKind::OtherExplicitSubset,
        "Color scheme: " + scheme->name,
        static_cast<std::size_t>(std::max(0, dataset->rows)));
    if (!setActiveAnalysisScope(scope, scopeEvent, error)) return false;
    return applyColorSchemeToMatchingObservations(group, name, changedRows, error);
}

bool ApplicationState::replaceColorSchemes(
    const std::string &group, const std::vector<NamedColorScheme> &schemes,
    std::string *error)
{
    std::set<std::string> names;
    for (const NamedColorScheme &scheme : schemes) {
        if (scheme.datasetId != group ||
            !ValidateAnalysisScopeSelectionName(scheme.name, error) ||
            !names.insert(scheme.name).second) {
            if (error && error->empty()) *error = "Saved color scheme names must be unique.";
            return false;
        }
    }
    groupColorSchemes_[group] = schemes;
    return true;
}

bool ApplicationState::setLabelColumn(const std::string &group, const std::string &column)
{
    if (!datasets_.contains(group)) return false;
    if (column.empty()) {
        groupLabelColumns_.erase(group);
    } else {
        const DataFrameModel *dataset = datasets_.find(group);
        const bool exists = dataset && std::any_of(
            dataset->columns.begin(), dataset->columns.end(),
            [&](const DataColumn &candidate) { return candidate.name == column; });
        if (!exists) return false;
        groupLabelColumns_[group] = column;
    }
    return true;
}

std::string ApplicationState::labelColumn(const std::string &group) const
{
    auto it = groupLabelColumns_.find(group);
    return it == groupLabelColumns_.end() ? std::string() : it->second;
}

std::optional<WindowNote> ApplicationState::documentNote(const std::string &group) const
{
    auto found = groupDocumentNotes_.find(group);
    return found == groupDocumentNotes_.end()
        ? std::optional<WindowNote>{} : std::optional<WindowNote>{found->second};
}

bool ApplicationState::setDocumentNote(
    const std::string &group, const std::optional<WindowNote> &note)
{
    if (!datasets_.contains(group)) return false;
    if (!note || !note->has_content) groupDocumentNotes_.erase(group);
    else groupDocumentNotes_[group] = *note;
    return true;
}

std::map<std::string, std::string> ApplicationState::variableRoles(
    const std::string &group) const
{
    std::map<std::string, std::string> roles;
    const DataFrameModel *dataset = datasets_.find(group);
    if (!dataset) return roles;
    for (const DataColumn &column : dataset->columns) roles[column.name] = "none";
    auto found = groupDefaultVariableRoles_.find(group);
    if (found == groupDefaultVariableRoles_.end()) return roles;
    for (const auto &[name, role] : found->second)
        if (roles.find(name) != roles.end()) roles[name] = role;
    return roles;
}

bool ApplicationState::setDefaultVariableRole(
    const std::string &group, const std::string &variable,
    const std::string &role, std::string *error)
{
    const DataFrameModel *dataset = datasets_.find(group);
    if (!dataset) {
        if (error) *error = "Dataset `" + group + "` is not registered.";
        return false;
    }
    const bool exists = std::any_of(dataset->columns.begin(), dataset->columns.end(),
        [&](const DataColumn &column) { return column.name == variable; });
    if (!exists) {
        if (error) *error = "Default role refers to unknown column `" + variable + "`.";
        return false;
    }
    std::string normalized = role;
    if (normalized == "response" || normalized == "y") normalized = "dependent";
    if (normalized == "predictor" || normalized == "x") normalized = "independent";
    if (normalized.empty()) normalized = "none";
    if (normalized != "none" && normalized != "dependent" &&
        normalized != "independent") {
        if (error) *error = "Unsupported default variable role `" + role + "`.";
        return false;
    }
    auto &defaults = groupDefaultVariableRoles_[group];
    if (normalized == "dependent") {
        for (auto entry = defaults.begin(); entry != defaults.end();) {
            if (entry->second == "dependent") entry = defaults.erase(entry);
            else ++entry;
        }
    }
    if (normalized == "none") defaults.erase(variable);
    else defaults[variable] = normalized;
    if (defaults.empty()) groupDefaultVariableRoles_.erase(group);
    return true;
}

bool ApplicationState::restoreVariableRoles(
    const std::string &group, const std::map<std::string, std::string> &roles,
    std::string *error)
{
    const DataFrameModel *dataset = datasets_.find(group);
    if (!dataset) {
        if (error) *error = "Dataset `" + group + "` is not registered.";
        return false;
    }
    std::set<std::string> columns;
    for (const DataColumn &column : dataset->columns) columns.insert(column.name);
    std::map<std::string, std::string> defaults;
    std::string dependent;
    for (const auto &[name, role] : roles) {
        if (columns.find(name) == columns.end()) {
            if (error) *error = "Default role refers to unknown column `" + name + "`.";
            return false;
        }
        if (role == "dependent") {
            if (!dependent.empty()) {
                if (error) *error = "Only one dependent default role can be restored.";
                return false;
            }
            dependent = name;
            defaults[name] = role;
        } else if (role == "independent") {
            defaults[name] = role;
        } else if (role != "none" && !role.empty()) {
            if (error) *error = "Unsupported default variable role `" + role + "`.";
            return false;
        }
    }
    if (defaults.empty()) groupDefaultVariableRoles_.erase(group);
    else groupDefaultVariableRoles_[group] = std::move(defaults);
    return true;
}

bool ApplicationState::setVariableType(const std::string &group,
                                       const std::string &variable,
                                       const std::string &type,
                                       VariableTypeChangeEffects &effects,
                                       std::string *message,
                                       const VariableTypeConversionSpecification *conversion)
{
    DataFrameModel *dataset = datasets_.find(group);
    if (!dataset) {
        if (message) *message = VariableNotFoundInDatasetStatus(variable, group);
        return false;
    }
    if (datasetCalculationPending(group)) {
        if (message) *message = "Wait for the current R calculation before editing this sheet.";
        return false;
    }
    auto column = std::find_if(dataset->columns.begin(), dataset->columns.end(),
        [&](const DataColumn &candidate) { return candidate.name == variable; });
    if (column == dataset->columns.end()) {
        if (message) *message = VariableNotFoundInDatasetStatus(variable, group);
        return false;
    }
    const std::string normalizedType = NormalizeVariableType(type);
    const std::string previousType = NormalizeVariableType(column->type);
    const DataColumn sourceColumn = *column;
    std::string conversionMessage;
    // Validate and perform the conversion on a copy first. A cancelled or
    // invalid mapping must not create a new dataset version or partially
    // mutate the canonical column.
    DataColumn convertedColumn = *column;
    if (!SetDataColumnType(convertedColumn, normalizedType, &conversionMessage, conversion)) {
        if (message) *message = conversionMessage;
        return false;
    }
    preserveCurrentDatasetVersion(group);
    *column = std::move(convertedColumn);
    TransformationStep typeChange;
    typeChange.label = "Change column `" + variable + "` statistical type from " +
        VariableTypeDisplayName(previousType) + " to " + VariableTypeDisplayName(normalizedType);
    typeChange.origin = RCodeOrigin::Recorded;
    typeChange.inputColumns = {variable};
    typeChange.outputColumns = {variable};
    typeChange.parameters["previous_type"] = previousType;
    typeChange.parameters["new_type"] = normalizedType;
    typeChange.parameters["previous_storage_type"] = DataColumnStorageType(sourceColumn);
    typeChange.parameters["storage_type"] = DataColumnStorageType(*column);
    typeChange.parameters["categories"] = [&] {
        std::string joined;
        for (const std::string &level : column->reversibleFactorLevels.empty()
                 ? column->definedLevels : column->reversibleFactorLevels) {
            if (!joined.empty()) joined += " | ";
            joined += level;
        }
        return joined;
    }();
    if (normalizedType == "numeric" && !column->numericMapping.empty()) {
        std::ostringstream code;
        code << "numeric_mapping <- c(";
        std::size_t index = 0;
        for (const auto &[label, value] : column->numericMapping) {
            if (index++) code << ", ";
            code << ProvenanceRStringLiteral(label) << " = " << value;
            typeChange.parameters["mapping:" + label] = value;
        }
        code << ")\ndata[[" << ProvenanceRStringLiteral(variable)
             << "]] <- unname(numeric_mapping[as.character(data[["
             << ProvenanceRStringLiteral(variable) << "]])])";
        typeChange.rCode = code.str();
    } else if ((normalizedType == "factor" || normalizedType == "ordered") &&
               previousType == "numeric" &&
               !sourceColumn.numericMapping.empty() &&
               !sourceColumn.reversibleFactorLevels.empty()) {
        std::ostringstream code;
        code << "category_labels <- c(";
        std::size_t index = 0;
        for (const std::string &label : sourceColumn.reversibleFactorLevels) {
            const auto mapped = sourceColumn.numericMapping.find(label);
            if (mapped == sourceColumn.numericMapping.end()) continue;
            if (index++) code << ", ";
            code << ProvenanceRStringLiteral(mapped->second) << " = "
                 << ProvenanceRStringLiteral(label);
        }
        code << ")\ndata[[" << ProvenanceRStringLiteral(variable) << "]] <- "
             << (normalizedType == "ordered" ? "ordered" : "factor")
             << "(unname(category_labels[as.character(data[["
             << ProvenanceRStringLiteral(variable) << "]])]), levels = c(";
        for (std::size_t level = 0; level < column->definedLevels.size(); ++level) {
            if (level) code << ", ";
            code << ProvenanceRStringLiteral(column->definedLevels[level]);
        }
        code << "))";
        typeChange.rCode = code.str();
    } else if (normalizedType == "factor" || normalizedType == "ordered") {
        std::ostringstream code;
        code << "data[[" << ProvenanceRStringLiteral(variable) << "]] <- "
             << (normalizedType == "ordered" ? "ordered" : "factor")
             << "(data[[" << ProvenanceRStringLiteral(variable) << "]], levels = c(";
        for (std::size_t index = 0; index < column->definedLevels.size(); ++index) {
            if (index) code << ", ";
            code << ProvenanceRStringLiteral(column->definedLevels[index]);
        }
        code << "))";
        typeChange.rCode = code.str();
    } else if (normalizedType == "character") {
        typeChange.rCode = "data[[" + ProvenanceRStringLiteral(variable) +
            "]] <- as.character(data[[" + ProvenanceRStringLiteral(variable) + "]])";
    }
    typeChange.parameters["missing_values"] = "preserved";
    if (dataset->datasetType == "multiple_imputation") {
        typeChange.parameters["multiple_imputation"] =
            "apply one stable conversion to original data and every completed imputation";
        typeChange.rCode =
            "# Apply this conversion identically to .imp = 0 and every completed imputation; preserve .id.\n" +
            typeChange.rCode +
            "\n# Rebuild the mids bookkeeping with mice::as.mids() and reattach variable metadata.";
    }
    RecordDataFrameTransformation(*dataset, std::move(typeChange));

    // DataColumn::type in DatasetRegistry is the source of truth.  Plot and
    // analysis seeds are derived caches used for fast UI and fitting; update
    // all of them here so no platform can accidentally keep an independent
    // variable type after the canonical metadata changes.
    std::vector<std::string> rebuiltTrellisPlots;
    for (auto &[id, plot] : plots_) {
        if (!plot || plot->group != group) continue;
        SyncPlotVariableFromColumn(*plot, *column);
        bool rebuilt = false;
        std::string ignored;
        if (SynchronizeTrellisConditioningVariableType(
                *plot, *dataset, variable, &rebuilt, &ignored) && rebuilt)
            rebuiltTrellisPlots.push_back(id);
    }
    for (auto &[id, state] : correlationMatrices_) {
        (void)id;
        if (state.group == group && state.hasSeed)
            SyncPlotVariableFromColumn(state.seed, *column);
    }
    for (auto &[id, state] : dendrograms_) {
        (void)id;
        if (state.group == group && state.hasSeed)
            SyncPlotVariableFromColumn(state.seed, *column);
    }
    for (auto &[id, state] : dimensionalityModels_) {
        (void)id;
        if (state.group == group && state.hasSeed)
            SyncPlotVariableFromColumn(state.seed, *column);
    }
    for (auto &[id, state] : regressionComparisons_) {
        (void)id;
        if (state.group == group && state.hasSeed)
            SyncPlotVariableFromColumn(state.seed, *column);
    }
    for (auto &[id, state] : generalizedGLMs_) {
        (void)id;
        if (state.group == group && state.hasSeed)
            SyncPlotVariableFromColumn(state.seed, *column);
    }
    for (auto &[id, state] : generalizedComparisons_) {
        (void)id;
        if (state.group == group && state.hasSeed)
            SyncPlotVariableFromColumn(state.seed, *column);
    }
    effects = applyVariableTypeChange(group, variable, normalizedType);
    effects.trellisPlotIds = std::move(rebuiltTrellisPlots);
    if (message) {
        *message = conversionMessage.empty()
            ? VariableTypeChangedStatus(variable, normalizedType)
            : conversionMessage;
    }
    return true;
}

VariableTypeChangeEffects ApplicationState::applyVariableTypeChange(
    const std::string &group,
    const std::string &variable,
    const std::string &normalizedType)
{
    VariableTypeChangeEffects effects;
    const bool factorLike = VariableTypeIsFactorLike(normalizedType);
    const bool numeric = normalizedType == "numeric";
    const bool nonNumeric = !numeric;
    const auto termUsesVariable = [&](const std::vector<std::string> &terms) {
        for (const std::string &term : terms) {
            const auto bases = UniqueBaseVariablesForTerm(term);
            if (std::find(bases.begin(), bases.end(), variable) != bases.end()) return true;
        }
        return false;
    };

    for (auto &entry : groupModels_) {
        GroupModelState &state = entry.second;
        if (state.group != group) continue;
        const bool usedByModel = state.response == variable || termUsesVariable(state.terms);
        if (usedByModel) {
            MarkGroupModelChanged(state);
            if (factorLike) state.termTypes[variable] = "factor";
            else state.termTypes.erase(variable);
        } else {
            // termTypes belongs to the model specification, not to the
            // dataset variable catalog. Keeping a metadata-only entry here
            // made some model editors treat a type change as an added term.
            state.termTypes.erase(variable);
        }
        // A real dataset metadata change supersedes an interpretation that
        // was captured earlier from a plot.
        state.termTypeOverrides.erase(variable);
    }

    for (auto &entry : correlationMatrices_) {
        CorrelationMatrixState &state = entry.second;
        if (state.group != group) continue;
        if (nonNumeric) {
            state.variables.erase(std::remove(state.variables.begin(), state.variables.end(), variable),
                                  state.variables.end());
            state.selectedRow = -1;
            state.selectedCol = -1;
        }
        effects.correlationIds.push_back(entry.first);
    }
    for (auto &entry : dendrograms_) {
        DendrogramState &state = entry.second;
        if (state.group != group) continue;
        if (nonNumeric) {
            state.variables.erase(std::remove(state.variables.begin(), state.variables.end(), variable),
                                  state.variables.end());
        }
        effects.dendrogramIds.push_back(entry.first);
    }
    for (auto &entry : dimensionalityModels_) {
        DimensionalityState &state = entry.second;
        if (state.group != group) continue;
        if (const auto *dataframe = datasets_.find(group))
            state.eligibleVariables = EligibleDimensionalityVariables(*dataframe);
        if (std::find(state.eligibleVariables.begin(), state.eligibleVariables.end(), variable) == state.eligibleVariables.end()) {
            state.variables.erase(std::remove(state.variables.begin(), state.variables.end(), variable),
                                  state.variables.end());
        }
        if (state.variables.size() < 2) {
            state.status = "At least two numeric, ordinal, or binary variables are required.";
            state.components.clear();
            state.loadings.clear();
            state.scores.clear();
            state.rowsUsed.clear();
            state.rowsExcluded.clear();
            ++state.modelVersion;
        } else {
            effects.dimensionalityIds.push_back(entry.first);
        }
    }
    for (auto &entry : scaleAnalyses_) {
        ScaleAnalysisState &state = entry.second;
        if (state.group != group) continue;
        auto item = std::find_if(state.specification.items.begin(),
            state.specification.items.end(), [&](const ScaleItemSpecification &candidate) {
                return candidate.variable == variable;
            });
        if (item == state.specification.items.end()) continue;
        if (normalizedType == "numeric") {
            item->type = ScaleItemType::Numeric;
        } else if (normalizedType == "ordered") {
            item->type = ScaleItemType::Ordinal;
            item->hasScoringRange = false;
        } else {
            state.specification.items.erase(item);
        }
        ++state.specification.revision;
        std::string scaleError;
        if (!ValidateScaleAnalysisSpecification(
                *datasets_.find(group), state.specification, scaleError)) {
            state.status = scaleError;
        } else {
            state.status = state.specification.items.size() < 2
                ? "Choose at least two numeric or ordinal items."
                : state.autoFit
                    ? "Variable type changed; refitting Scale Analysis..."
                    : "Auto-fit is off. Displaying the last completed result; "
                        "turn it on to analyse the changed variable type.";
        }
        if (state.autoFit) state.result = {};
        state.rFitPending = false;
        state.pendingRevision = -1;
        state.pendingFingerprint.clear();
        effects.scaleAnalysisIds.push_back(entry.first);
    }
    for (auto &entry : regressionComparisons_) {
        RegressionComparisonState &state = entry.second;
        if (state.group != group) continue;
        if (factorLike) {
            state.termTypes[variable] = "factor";
        } else {
            state.termTypes.erase(variable);
        }
        bool affects = state.response == variable;
        for (RegressionComparisonModel &model : state.models) {
            if (model.response == variable || termUsesVariable(model.terms)) {
                model.isStale = true;
                affects = true;
            }
        }
        if (affects) effects.regressionComparisonIds.push_back(entry.first);
    }
    for (auto &entry : generalizedGLMs_) {
        GeneralizedGLMState &state = entry.second;
        if (state.group != group) continue;
        if (factorLike) {
            state.termTypes[variable] = "factor";
        } else {
            state.termTypes.erase(variable);
        }
        const bool affects = state.response == variable || termUsesVariable(state.terms) ||
            state.exposure == variable || state.offsetVariable == variable ||
            state.trialsVariable == variable;
        if (!affects) continue;
        ++state.modelVersion;
        effects.generalizedGlmIds.push_back(entry.first);
        if (state.autoRefit) {
            state.ok = false;
            state.frozenScopeNotice.clear();
        } else {
            state.frozenScopeNotice =
                "Auto-refit is off. Displaying the last completed result for the previous variable type.";
        }
        if (state.autoRefit) effects.generalizedGlmIdsToRefit.push_back(entry.first);
    }
    for (auto &entry : generalizedComparisons_) {
        GeneralizedComparisonState &state = entry.second;
        if (state.group != group) continue;
        if (factorLike) {
            state.termTypes[variable] = "factor";
        } else {
            state.termTypes.erase(variable);
        }
        bool affects = state.response == variable;
        for (GeneralizedComparisonModel &model : state.models) {
            if (model.response == variable || termUsesVariable(model.terms) ||
                model.exposure == variable || model.offsetVariable == variable ||
                model.trialsVariable == variable) {
                model.isStale = true;
                affects = true;
            }
        }
        if (affects) effects.generalizedComparisonIds.push_back(entry.first);
    }
    for (auto &entry : modelTrellises_) {
        ModelTrellisState &state = entry.second;
        if (state.specification.baseModel.group != group) continue;
        if (factorLike) state.specification.baseModel.termTypes[variable] = "factor";
        else state.specification.baseModel.termTypes.erase(variable);
        const bool affects = state.specification.baseModel.response == variable ||
            termUsesVariable(state.specification.baseModel.terms);
        if (affects) {
            ++state.specificationGeneration;
            state.status = "Variable type changed; refit required.";
            effects.modelTrellisIds.push_back(entry.first);
        }
    }
    for (auto &[id, state] : nativeMixedModels_) {
        if (state.group != group) continue;
        bool affects = state.response == variable || termUsesVariable(state.fixedEffects);
        for (const NativeMixedRandomSpec &random : state.randomEffects) {
            if (random.group == variable || termUsesVariable(random.terms)) affects = true;
        }
        if (affects) effects.mixedModelIds.push_back(id);
    }
    if (groupColorOverrides_.count(group)) refreshColorOverride(group);
    return effects;
}

bool ApplicationState::datasetCalculationPending(const std::string &group) const
{
    for (const auto &[id, linear] : groupModels_)
        if (linear.group == group && linear.rFitPending) return true;
    for (const auto &[id, state] : generalizedGLMs_)
        if (state.group == group && state.rFitPending) return true;
    for (const auto &[id, state] : regressionComparisons_)
        if (state.group == group && state.rFitPending) return true;
    for (const auto &[id, state] : generalizedComparisons_)
        if (state.group == group && state.rFitPending) return true;
    for (const auto &[id, state] : scaleAnalyses_)
        if (state.group == group && state.rFitPending) return true;
    for (const auto &[id, state] : dimensionalityModels_)
        if (state.group == group && state.rFitPending) return true;
    for (const auto &[id, state] : correlationMatrices_)
        if (state.group == group && state.rFitPending) return true;
    for (const auto &[id, state] : dendrograms_)
        if (state.group == group && state.rFitPending) return true;
    for (const auto &[id, state] : modelTrellises_)
        if (state.specification.baseModel.group == group && state.fitPending) return true;
    return false;
}

DatasetValueChangeEffects ApplicationState::applyDatasetValueChange(
    const std::string &group,
    const std::string &variable)
{
    DatasetValueChangeEffects effects;
    const DataFrameModel *dataset = datasets_.find(group);
    if (!dataset) return effects;
    const DataColumn *column = FindDataColumnInDataFrame(*dataset, variable);
    if (!column) return effects;

    const auto termUsesVariable = [&](const std::vector<std::string> &terms) {
        for (const std::string &term : terms) {
            const auto bases = UniqueBaseVariablesForTerm(term);
            if (std::find(bases.begin(), bases.end(), variable) != bases.end()) return true;
        }
        return false;
    };
    const auto syncSeed = [&](PlotModel &seed, bool hasSeed) {
        if (hasSeed) SyncPlotVariableFromColumn(seed, *column);
    };

    for (auto &[id, linear] : groupModels_) {
        if (linear.group != group ||
            (linear.response != variable && !termUsesVariable(linear.terms))) continue;
        MarkGroupModelChanged(linear);
        effects.groupModel = true;
    }
    for (auto &[id, state] : correlationMatrices_) {
        if (state.group != group ||
            std::find(state.variables.begin(), state.variables.end(), variable) == state.variables.end()) continue;
        syncSeed(state.seed, state.hasSeed);
        effects.correlationIds.push_back(id);
    }
    for (auto &[id, state] : dimensionalityModels_) {
        if (state.group != group ||
            std::find(state.variables.begin(), state.variables.end(), variable) == state.variables.end()) continue;
        syncSeed(state.seed, state.hasSeed);
        state.rFitPending = false;
        state.lastRFitSignature.clear();
        effects.dimensionalityIds.push_back(id);
    }
    for (auto &[id, state] : scaleAnalyses_) {
        if (state.group != group) continue;
        const bool affected = std::any_of(state.specification.items.begin(),
            state.specification.items.end(), [&](const ScaleItemSpecification &item) {
                return item.variable == variable;
            });
        if (!affected) continue;
        syncSeed(state.seed, state.hasSeed);
        ++state.specification.revision;
        std::string scaleError;
        if (!ValidateScaleAnalysisSpecification(*dataset, state.specification, scaleError))
            state.status = scaleError;
        else if (state.autoFit)
            state.status = "Dataset value changed; refitting Scale Analysis...";
        else
            state.status = "Auto-fit is off. Displaying the last completed result; "
                "turn it on to analyse the changed data.";
        if (state.autoFit) state.result = {};
        state.rFitPending = false;
        state.pendingRevision = -1;
        state.pendingFingerprint.clear();
        effects.scaleAnalysisIds.push_back(id);
    }
    for (auto &[id, state] : dendrograms_) {
        if (state.group != group ||
            std::find(state.variables.begin(), state.variables.end(), variable) == state.variables.end()) continue;
        syncSeed(state.seed, state.hasSeed);
        effects.dendrogramIds.push_back(id);
    }
    for (auto &[id, state] : regressionComparisons_) {
        if (state.group != group) continue;
        bool affected = state.response == variable;
        syncSeed(state.seed, state.hasSeed);
        for (RegressionComparisonModel &model : state.models) {
            if (model.response == variable || termUsesVariable(model.terms)) {
                model.isStale = true;
                ++model.modelVersion;
                affected = true;
            }
        }
        if (!affected) continue;
        state.rFitPending = false;
        state.lastRFitSignature.clear();
        effects.regressionComparisonIds.push_back(id);
    }
    for (auto &[id, state] : generalizedGLMs_) {
        if (state.group != group ||
            (state.response != variable && !termUsesVariable(state.terms) &&
             state.exposure != variable && state.offsetVariable != variable &&
             state.trialsVariable != variable)) continue;
        syncSeed(state.seed, state.hasSeed);
        ++state.modelVersion;
        if (state.autoRefit) {
            state.ok = false;
            state.frozenScopeNotice.clear();
        } else {
            state.frozenScopeNotice =
                "Auto-refit is off. Displaying the last completed result for the previous data.";
        }
        state.rFitPending = false;
        state.lastRFitSignature.clear();
        effects.generalizedGlmIds.push_back(id);
        if (state.autoRefit) effects.generalizedGlmIdsToRefit.push_back(id);
    }
    for (auto &[id, state] : generalizedComparisons_) {
        if (state.group != group) continue;
        bool affected = state.response == variable;
        syncSeed(state.seed, state.hasSeed);
        for (GeneralizedComparisonModel &model : state.models) {
            if (model.response == variable || termUsesVariable(model.terms) ||
                model.exposure == variable || model.offsetVariable == variable ||
                model.trialsVariable == variable) {
                model.isStale = true;
                ++model.modelVersion;
                affected = true;
            }
        }
        if (affected) effects.generalizedComparisonIds.push_back(id);
    }
    for (auto &[id, state] : modelTrellises_) {
        if (state.specification.baseModel.group != group) continue;
        bool affected = state.specification.baseModel.response == variable ||
            termUsesVariable(state.specification.baseModel.terms);
        if (state.specification.rowConditioningVariable &&
            state.specification.rowConditioningVariable->variableId == variable) affected = true;
        if (state.specification.columnConditioningVariable &&
            state.specification.columnConditioningVariable->variableId == variable) affected = true;
        if (!affected) continue;
        state.fitPending = false;
        state.status = "Dataset value changed; refitting panels...";
        effects.modelTrellisIds.push_back(id);
    }
    for (auto &[id, state] : nativeMixedModels_) {
        if (state.group != group) continue;
        bool affected = state.response == variable || termUsesVariable(state.fixedEffects);
        for (const NativeMixedRandomSpec &random : state.randomEffects) {
            if (random.group == variable || termUsesVariable(random.terms)) affected = true;
        }
        if (affected) effects.mixedModelIds.push_back(id);
    }
    if (groupColorOverrides_.count(group) &&
        groupColorOverrides_[group].variable == variable)
        refreshColorOverride(group);
    return effects;
}

DatasetValueChangeEffects ApplicationState::applyVariableRename(const std::string &group,
                                           const std::string &oldName,
                                           const std::string &newName)
{
    DatasetValueChangeEffects effects;
    if (oldName.empty() || newName.empty() || oldName == newName) return effects;
    auto colorOverride = groupColorOverrides_.find(group);
    if (colorOverride != groupColorOverrides_.end() &&
        colorOverride->second.variable == oldName)
        colorOverride->second.variable = newName;
    auto defaultRoles = groupDefaultVariableRoles_.find(group);
    if (defaultRoles != groupDefaultVariableRoles_.end())
        RenameStateMapKey(defaultRoles->second, oldName, newName);
    for (auto &[id, plot] : plots_) {
        (void)id;
        if (!plot || plot->group != group) continue;
        if (plot->xLabel == oldName) plot->xLabel = newName;
        if (plot->yLabel == oldName) plot->yLabel = newName;
        if (plot->timeSeriesGroupVariable == oldName) plot->timeSeriesGroupVariable = newName;
        if (plot->trellisConditionVariable == oldName) plot->trellisConditionVariable = newName;
        if (plot->barplotSplitVariable == oldName) plot->barplotSplitVariable = newName;
        if (plot->colorByVariable == oldName) plot->colorByVariable = newName;
        if (plot->glmInteractionTerm == oldName) plot->glmInteractionTerm = newName;
        for (VariableMeta &meta : plot->variableMeta)
            if (meta.name == oldName) meta.name = newName;
        for (NumericVariable &numericVariable : plot->variables)
            if (numericVariable.name == oldName) numericVariable.name = newName;
        RenameVariableInTermList(plot->scatterMatrixVariables, oldName, newName);
        RenameVariableInTermList(plot->boxplotVariables, oldName, newName);
        RenameVariableInTermList(plot->boxplotGroupingVariables, oldName, newName);
        if (plot->kind == "boxplot" && !plot->boxplotGroupingVariables.empty())
            plot->xLabel = BoxplotGroupingLabel(plot->boxplotGroupingVariables);
        RenameVariableInTermList(plot->barplotXVariables, oldName, newName);
        if (plot->trellisSpecificationInitialized) {
            auto &specification = plot->trellisSpecification;
            if (specification.xVariableId == oldName) specification.xVariableId = newName;
            RenameVariableInTermList(
                specification.boxplotGroupingVariableIds, oldName, newName);
            if (specification.yVariableId == oldName) specification.yVariableId = newName;
            if (specification.groupingVariableId == oldName) specification.groupingVariableId = newName;
            if (specification.splitVariableId == oldName) specification.splitVariableId = newName;
            for (auto &condition : specification.conditioningVariables)
                if (condition.variableId == oldName) condition.variableId = newName;
            RenameVariableInTermList(
                specification.dataTable.displayedVariableIds, oldName, newName);
        }
    }
    for (auto &entry : groupModels_) {
        GroupModelState &state = entry.second;
        if (state.group != group) continue;
        bool affected = state.response == oldName;
        if (affected) state.response = newName;
        const auto previousTerms = state.terms;
        RenameVariableInTermList(state.terms, oldName, newName);
        affected = affected || previousTerms != state.terms;
        RenameStateMapKey(state.termTypes, oldName, newName);
        RenameStateMapKey(state.termTypeOverrides, oldName, newName);
        if (affected) {
            MarkGroupModelChanged(state);
            effects.groupModel = true;
        }
    }
    if (labelColumn(group) == oldName) {
        // The caller has already renamed the column in DatasetRegistry.
        setLabelColumn(group, newName);
    }
    for (auto &[id, state] : correlationMatrices_) {
        if (state.group != group) continue;
        const auto previous = state.variables;
        RenameVariableInTermList(state.variables, oldName, newName);
        if (previous != state.variables) effects.correlationIds.push_back(id);
    }
    for (auto &[id, state] : dendrograms_) {
        if (state.group != group) continue;
        const auto previous = state.variables;
        RenameVariableInTermList(state.variables, oldName, newName);
        if (previous != state.variables) effects.dendrogramIds.push_back(id);
    }
    for (auto &[id, state] : dimensionalityModels_) {
        if (state.group != group) continue;
        const auto previous = state.variables;
        RenameVariableInTermList(state.variables, oldName, newName);
        RenameVariableInTermList(state.eligibleVariables, oldName, newName);
        if (previous != state.variables) effects.dimensionalityIds.push_back(id);
    }
    for (auto &[id, state] : scaleAnalyses_) {
        if (state.group != group) continue;
        bool affected = false;
        for (ScaleItemSpecification &item : state.specification.items) {
            if (item.variable != oldName) continue;
            item.variable = newName;
            affected = true;
        }
        if (!affected) continue;
        if (state.hasSeed) {
            const DataFrameModel *dataset = datasets_.find(group);
            const DataColumn *column = dataset
                ? FindDataColumnInDataFrame(*dataset, newName) : nullptr;
            if (column) SyncPlotVariableFromColumn(state.seed, *column);
        }
        ++state.specification.revision;
        if (state.autoFit) state.result = {};
        state.rFitPending = false;
        state.pendingRevision = -1;
        state.pendingFingerprint.clear();
        state.status = state.autoFit
            ? "Variable renamed; refitting Scale Analysis..."
            : "Auto-fit is off. Displaying the last completed result; "
                "turn it on to analyse the renamed variable.";
        effects.scaleAnalysisIds.push_back(id);
    }
    for (auto &[id, state] : regressionComparisons_) {
        if (state.group != group) continue;
        bool affected = state.response == oldName;
        if (affected) state.response = newName;
        RenameVariableInTermList(state.termRows, oldName, newName);
        RenameStateMapKey(state.termTypes, oldName, newName);
        for (RegressionComparisonModel &model : state.models) {
            bool modelAffected = model.response == oldName;
            if (modelAffected) model.response = newName;
            const auto previousTerms = model.terms;
            RenameVariableInTermList(model.terms, oldName, newName);
            modelAffected = modelAffected || previousTerms != model.terms;
            if (modelAffected) {
                model.isStale = true;
                ++model.modelVersion;
                affected = true;
            }
        }
        if (affected) {
            state.rFitPending = false;
            state.lastRFitSignature.clear();
            effects.regressionComparisonIds.push_back(id);
        }
    }
    for (auto &[id, state] : generalizedGLMs_) {
        if (state.group != group) continue;
        bool affected = state.response == oldName;
        if (affected) state.response = newName;
        const auto previousTerms = state.terms;
        RenameVariableInTermList(state.terms, oldName, newName);
        affected = affected || previousTerms != state.terms;
        if (state.exposure == oldName) { state.exposure = newName; affected = true; }
        if (state.offsetVariable == oldName) { state.offsetVariable = newName; affected = true; }
        if (state.trialsVariable == oldName) { state.trialsVariable = newName; affected = true; }
        if (!affected) continue;
        ++state.modelVersion;
        state.rFitPending = false;
        state.lastRFitSignature.clear();
        effects.generalizedGlmIds.push_back(id);
        if (state.autoRefit) {
            state.ok = false;
            state.frozenScopeNotice.clear();
            effects.generalizedGlmIdsToRefit.push_back(id);
        } else {
            state.frozenScopeNotice =
                "Auto-refit is off. Displaying the last completed result for the previous variable name.";
        }
    }
    for (auto &[id, state] : generalizedComparisons_) {
        if (state.group != group) continue;
        bool affected = state.response == oldName;
        if (affected) state.response = newName;
        RenameStateMapKey(state.termTypes, oldName, newName);
        for (GeneralizedComparisonModel &model : state.models) {
            bool modelAffected = model.response == oldName;
            if (modelAffected) model.response = newName;
            const auto previousTerms = model.terms;
            RenameVariableInTermList(model.terms, oldName, newName);
            modelAffected = modelAffected || previousTerms != model.terms;
            if (model.exposure == oldName) { model.exposure = newName; modelAffected = true; }
            if (model.offsetVariable == oldName) { model.offsetVariable = newName; modelAffected = true; }
            if (model.trialsVariable == oldName) { model.trialsVariable = newName; modelAffected = true; }
            if (modelAffected) {
                model.isStale = true;
                ++model.modelVersion;
                affected = true;
            }
        }
        if (affected) {
            state.rFitPending = false;
            state.lastRFitSignature.clear();
            RefreshGeneralizedTermRowsFromFits(state);
            effects.generalizedComparisonIds.push_back(id);
        }
    }
    for (auto &[id, state] : modelTrellises_) {
        auto &spec = state.specification;
        if (spec.baseModel.group != group) continue;
        bool affected = spec.baseModel.response == oldName;
        if (affected) spec.baseModel.response = newName;
        const auto oldTerms = spec.baseModel.terms;
        RenameVariableInTermList(spec.baseModel.terms, oldName, newName);
        affected = affected || oldTerms != spec.baseModel.terms;
        RenameStateMapKey(spec.baseModel.termTypes, oldName, newName);
        if (spec.rowConditioningVariable &&
            spec.rowConditioningVariable->variableId == oldName) {
            spec.rowConditioningVariable->variableId = newName;
            affected = true;
        }
        if (spec.columnConditioningVariable &&
            spec.columnConditioningVariable->variableId == oldName) {
            spec.columnConditioningVariable->variableId = newName;
            affected = true;
        }
        if (!affected) continue;
        ++state.specificationGeneration;
        state.fitPending = false;
        state.status = "Variable renamed; refitting panels...";
        effects.modelTrellisIds.push_back(id);
    }
    for (auto &[id, state] : nativeMixedModels_) {
        if (state.group != group) continue;
        bool affected = state.response == oldName;
        if (affected) state.response = newName;
        const auto previousFixed = state.fixedEffects;
        RenameVariableInTermList(state.fixedEffects, oldName, newName);
        affected = affected || previousFixed != state.fixedEffects;
        for (NativeMixedRandomSpec &random : state.randomEffects) {
            if (random.group == oldName) {
                random.group = newName;
                affected = true;
            }
            const auto previousTerms = random.terms;
            RenameVariableInTermList(random.terms, oldName, newName);
            affected = affected || previousTerms != random.terms;
        }
        if (affected) effects.mixedModelIds.push_back(id);
    }
    return effects;
}

RemovedGroupDomainState ApplicationState::removeGroupDomainState(const std::string &group)
{
    RemovedGroupDomainState removed;
    groupSelections_.erase(group);
    excludedRows_.erase(group);
    groupSelectionVersions_.erase(group);
    activeAnalysisScopes_.erase(group);
    analysisScopeVersions_.erase(group);
    savedSelections_.erase(group);
    groupSelectedColors_.erase(group);
    groupPointColors_.erase(group);
    groupColorOverrides_.erase(group);
    groupColorSchemes_.erase(group);
    groupLabelColumns_.erase(group);
    groupDocumentNotes_.erase(group);
    groupDefaultVariableRoles_.erase(group);
    groupModels_.erase(group);
    linearModelFits_.erase(group);

    const auto removeForGroup = [&](auto &states, std::vector<std::string> &ids) {
        for (auto it = states.begin(); it != states.end();) {
            if (it->second.group == group) {
                ids.push_back(it->first);
                it = states.erase(it);
            } else {
                ++it;
            }
        }
    };
    removeForGroup(correlationMatrices_, removed.correlationIds);
    removeForGroup(dimensionalityModels_, removed.dimensionalityIds);
    removeForGroup(scaleAnalyses_, removed.scaleAnalysisIds);
    removeForGroup(dendrograms_, removed.dendrogramIds);
    removeForGroup(generalizedGLMs_, removed.generalizedGlmIds);
    removeForGroup(generalizedComparisons_, removed.generalizedComparisonIds);
    removeForGroup(nativeMixedModels_, removed.mixedModelIds);
    removeForGroup(regressionComparisons_, removed.regressionComparisonIds);
    for (auto it = modelTrellises_.begin(); it != modelTrellises_.end();) {
        if (it->second.specification.baseModel.group == group) {
            removed.modelTrellisIds.push_back(it->first);
            it = modelTrellises_.erase(it);
        } else {
            ++it;
        }
    }
    if (activePlotId_.rfind("dataset_seed_" + group, 0) == 0) {
        activePlotId_.clear();
    }
    return removed;
}

void ApplicationState::clearWorkbenchModels()
{
    groupSelections_.clear();
    excludedRows_.clear();
    groupSelectionVersions_.clear();
    activeAnalysisScopes_.clear();
    analysisScopeVersions_.clear();
    savedSelections_.clear();
    groupSelectedColors_.clear();
    groupPointColors_.clear();
    groupColorOverrides_.clear();
    groupColorSchemes_.clear();
    plots_.clear();
    groupModels_.clear();
    correlationMatrices_.clear();
    dimensionalityModels_.clear();
    scaleAnalyses_.clear();
    dendrograms_.clear();
    generalizedGLMs_.clear();
    nativeMixedModels_.clear();
    linearModelFits_.clear();
    regressionComparisons_.clear();
    modelTrellises_.clear();
    generalizedComparisons_.clear();
    outputCodeReferences_.clear();
    historicalDatasetVersions_.clear();
    groupLabelColumns_.clear();
    groupDocumentNotes_.clear();
    groupDefaultVariableRoles_.clear();
    activePlotId_.clear();
}

void ApplicationState::registerOutputCodeReference(const OutputCodeReference &reference)
{
    if (reference.outputId.empty()) return;
    outputCodeReferences_[reference.outputId] = reference;
    pruneUnreferencedHistoricalDatasetVersions();
    if (outputCodeReferenceChanged) outputCodeReferenceChanged(reference);
}

bool ApplicationState::eraseOutputCodeReference(const std::string &id)
{
    if (!outputCodeReferences_.erase(id)) return false;
    pruneUnreferencedHistoricalDatasetVersions();
    return true;
}

const OutputCodeReference *ApplicationState::outputCodeReference(const std::string &id) const
{
    auto found = outputCodeReferences_.find(id);
    return found == outputCodeReferences_.end() ? nullptr : &found->second;
}

const OutputCodeReference *ApplicationState::regressionSourceOutputCodeReference(
    const std::string &sourceKind,
    const std::string &sourceModelId) const
{
    if (sourceModelId.empty()) return nullptr;
    if (sourceKind == "linear_single")
        return outputCodeReference("glm:" + sourceModelId);
    if (sourceKind == "generalized_single")
        return outputCodeReference(sourceModelId);
    if (sourceKind == "linear_comparison") {
        for (const auto &[comparisonId, comparison] : regressionComparisons_) {
            const bool contains = std::any_of(
                comparison.models.begin(), comparison.models.end(),
                [&](const RegressionComparisonModel &model) {
                    return model.id == sourceModelId;
                });
            if (contains) return outputCodeReference(comparisonId);
        }
    } else if (sourceKind == "generalized_comparison") {
        for (const auto &[comparisonId, comparison] : generalizedComparisons_) {
            const bool contains = std::any_of(
                comparison.models.begin(), comparison.models.end(),
                [&](const GeneralizedComparisonModel &model) {
                    return model.id == sourceModelId;
                });
            if (contains) return outputCodeReference(comparisonId);
        }
    }
    // Backward compatibility for restored documents that predate explicit
    // source kinds or already stored their owning output id.
    if (const OutputCodeReference *direct = outputCodeReference(sourceModelId))
        return direct;
    return outputCodeReference("glm:" + sourceModelId);
}

std::vector<OutputCodeReference> ApplicationState::outputCodeReferencesForDataset(
    const std::string &datasetId) const
{
    std::vector<OutputCodeReference> result;
    for (const auto &[id, reference] : outputCodeReferences_) {
        (void)id;
        if (reference.provenance.dataVersion.datasetId == datasetId)
            result.push_back(reference);
    }
    return result;
}

void ApplicationState::restoreOutputCodeReferences(
    const std::vector<OutputCodeReference> &references)
{
    for (const OutputCodeReference &reference : references) {
        if (!reference.outputId.empty())
            outputCodeReferences_[reference.outputId] = reference;
    }
    pruneUnreferencedHistoricalDatasetVersions();
}

const DataFrameModel *ApplicationState::datasetVersion(
    const DataVersionReference &version) const
{
    const DataFrameModel *current = datasets_.find(version.datasetId);
    if (current && current->dataVersion == version.version) return current;
    const std::string key = version.storageKey.empty()
        ? version.datasetId + "@" + std::to_string(version.version)
        : version.storageKey;
    const auto found = historicalDatasetVersions_.find(key);
    return found == historicalDatasetVersions_.end() ? nullptr : &found->second;
}

void ApplicationState::preserveCurrentDatasetVersion(const std::string &datasetId)
{
    const DataFrameModel *current = datasets_.find(datasetId);
    if (!current) return;
    const std::string key = current->provenance.currentVersion.storageKey.empty()
        ? datasetId + "@" + std::to_string(current->dataVersion)
        : current->provenance.currentVersion.storageKey;
    const bool referenced = std::any_of(outputCodeReferences_.begin(),
        outputCodeReferences_.end(), [&](const auto &entry) {
            const DataVersionReference &version = entry.second.provenance.dataVersion;
            const std::string referenceKey = version.storageKey.empty()
                ? version.datasetId + "@" + std::to_string(version.version)
                : version.storageKey;
            return referenceKey == key;
        });
    if (referenced && !historicalDatasetVersions_.count(key))
        historicalDatasetVersions_.emplace(key, *current);
}

std::vector<DataFrameModel> ApplicationState::referencedHistoricalDatasetVersions(
    const std::string &datasetId) const
{
    std::vector<DataFrameModel> result;
    std::set<std::string> added;
    for (const auto &entry : outputCodeReferences_) {
        const DataVersionReference &version = entry.second.provenance.dataVersion;
        if (version.datasetId != datasetId) continue;
        const DataFrameModel *current = datasets_.find(datasetId);
        if (current && current->dataVersion == version.version) continue;
        const std::string key = version.storageKey.empty()
            ? datasetId + "@" + std::to_string(version.version)
            : version.storageKey;
        const auto found = historicalDatasetVersions_.find(key);
        if (found != historicalDatasetVersions_.end() && added.insert(key).second)
            result.push_back(found->second);
    }
    return result;
}

void ApplicationState::restoreHistoricalDatasetVersions(
    const std::vector<DataFrameModel> &versions)
{
    for (const DataFrameModel &version : versions) {
        if (version.group.empty() || version.dataVersion == 0) continue;
        const std::string key = version.provenance.currentVersion.storageKey.empty()
            ? version.group + "@" + std::to_string(version.dataVersion)
            : version.provenance.currentVersion.storageKey;
        historicalDatasetVersions_.try_emplace(key, version);
    }
}

void ApplicationState::pruneUnreferencedHistoricalDatasetVersions()
{
    std::set<std::string> referenced;
    for (const auto &entry : outputCodeReferences_) {
        const DataVersionReference &version = entry.second.provenance.dataVersion;
        if (version.datasetId.empty()) continue;
        referenced.insert(version.storageKey.empty()
            ? version.datasetId + "@" + std::to_string(version.version)
            : version.storageKey);
    }
    for (auto entry = historicalDatasetVersions_.begin();
         entry != historicalDatasetVersions_.end();) {
        if (!referenced.count(entry->first)) entry = historicalDatasetVersions_.erase(entry);
        else ++entry;
    }
}

} // namespace core
} // namespace rlispstat
