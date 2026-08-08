#include "application_state.h"

#include <algorithm>

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
    const bool replacing = datasets_.contains(dataset.group);
    datasets_.registerDataset(dataset);
    ensureSelectionGroup(dataset.group);
    auto existing = activeAnalysisScopes_.find(dataset.group);
    if (!replacing || existing == activeAnalysisScopes_.end()) {
        activeAnalysisScopes_[dataset.group] =
            AllObservationsAnalysisScope(dataset.group, static_cast<std::size_t>(std::max(0, dataset.rows)));
        analysisScopeVersions_.try_emplace(dataset.group, 0);
    } else {
        const AnalysisScope previous = existing->second;
        AnalysisScope reconciled = ReconcileAnalysisScope(
            previous, static_cast<std::size_t>(std::max(0, dataset.rows)));
        const bool changed = reconciled.totalDatasetRows != previous.totalDatasetRows ||
            reconciled.originalRowIds != previous.originalRowIds ||
            reconciled.invalidatedRowCount != previous.invalidatedRowCount;
        existing->second = std::move(reconciled);
        if (changed) ++analysisScopeVersions_[dataset.group];
    }
    auto saved = savedSelections_.find(dataset.group);
    if (saved != savedSelections_.end()) {
        const int maximum = std::max(0, dataset.rows);
        for (SavedSelection &selection : saved->second) {
            selection.originalRowIds.erase(std::remove_if(
                selection.originalRowIds.begin(), selection.originalRowIds.end(),
                [maximum](int row) { return row <= 0 || row > maximum; }),
                selection.originalRowIds.end());
        }
    }
    return true;
}

bool ApplicationState::eraseDataset(const std::string &group)
{
    if (!datasets_.erase(group)) return false;
    removeGroupDomainState(group);
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

AnalysisScope ApplicationState::activeAnalysisScope(const std::string &datasetId) const
{
    const DataFrameModel *dataset = datasets_.find(datasetId);
    const std::size_t totalRows = dataset
        ? static_cast<std::size_t>(std::max(0, dataset->rows)) : 0;
    auto found = activeAnalysisScopes_.find(datasetId);
    return found == activeAnalysisScopes_.end()
        ? AllObservationsAnalysisScope(datasetId, totalRows)
        : ReconcileAnalysisScope(found->second, totalRows);
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
        SavedSelection saved;
        saved.datasetId = normalized.datasetId;
        saved.name = *selectionName;
        saved.originalRowIds = normalized.originalRowIds;
        saved.sourceKind = normalized.sourceKind;
        saved.sourceViewId = normalized.sourceViewId;
        std::vector<SavedSelection> &selections = savedSelections_[normalized.datasetId];
        auto existing = std::find_if(selections.begin(), selections.end(),
            [&](const SavedSelection &candidate) { return candidate.name == saved.name; });
        if (existing == selections.end()) selections.push_back(std::move(saved));
        else *existing = std::move(saved);
    }
    AnalysisScope previous = activeAnalysisScope(scope.datasetId);
    activeAnalysisScopes_[scope.datasetId] = normalized;
    const std::size_t version = ++analysisScopeVersions_[scope.datasetId];
    if (event) *event = {scope.datasetId, previous, normalized, version};
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

std::vector<int> ApplicationState::resolveActiveAnalysisRowIds(
    const std::string &datasetId) const
{
    const DataFrameModel *dataset = datasets_.find(datasetId);
    if (!dataset) return {};
    return ResolveAnalysisScopeRowIds(activeAnalysisScope(datasetId),
        static_cast<std::size_t>(std::max(0, dataset->rows)));
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

std::optional<SavedSelection> ApplicationState::savedSelection(
    const std::string &datasetId, const std::string &name) const
{
    const std::vector<SavedSelection> selections = savedSelections(datasetId);
    auto found = std::find_if(selections.begin(), selections.end(),
        [&](const SavedSelection &candidate) { return candidate.name == name; });
    return found == selections.end() ? std::nullopt :
        std::optional<SavedSelection>(*found);
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
    groupPointColors_[group][row] = color;
    return true;
}

std::vector<int> ApplicationState::clearPointColors(const std::string &group,
                                                     const std::vector<int> &rows)
{
    std::vector<int> changed;
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

bool ApplicationState::setVariableType(const std::string &group,
                                       const std::string &variable,
                                       const std::string &type,
                                       VariableTypeChangeEffects &effects,
                                       std::string *message)
{
    DataFrameModel *dataset = datasets_.find(group);
    if (!dataset) {
        if (message) *message = VariableNotFoundInDatasetStatus(variable, group);
        return false;
    }
    auto column = std::find_if(dataset->columns.begin(), dataset->columns.end(),
        [&](const DataColumn &candidate) { return candidate.name == variable; });
    if (column == dataset->columns.end()) {
        if (message) *message = VariableNotFoundInDatasetStatus(variable, group);
        return false;
    }
    const std::string normalizedType = NormalizeVariableType(type);
    if (!SetDataColumnType(*column, normalizedType, message)) return false;
    effects = applyVariableTypeChange(group, variable, normalizedType);
    if (message) *message = VariableTypeChangedStatus(variable, normalizedType);
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

    auto groupModel = groupModels_.find(group);
    if (groupModel != groupModels_.end()) {
        GroupModelState &state = groupModel->second;
        if (state.dependent == variable || HasModelTerm(state, variable)) {
            MarkGroupModelChanged(state);
        }
        if (factorLike) {
            state.termTypes[variable] = "factor";
        } else if (numeric) {
            state.termTypes.erase(variable);
        }
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
        if (nonNumeric) {
            state.variables.erase(std::remove(state.variables.begin(), state.variables.end(), variable),
                                  state.variables.end());
        }
        if (state.variables.size() < 2) {
            state.status = "At least two numeric variables are required.";
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
    for (auto &entry : regressionComparisons_) {
        RegressionComparisonState &state = entry.second;
        if (state.group != group) continue;
        if (factorLike) {
            state.termTypes[variable] = "factor";
        } else if (numeric) {
            state.termTypes.erase(variable);
        }
        if (state.response == variable) state.autoRefit = false;
        for (RegressionComparisonModel &model : state.models) {
            if (model.response == variable || ModelIncludesTerm(model, variable)) {
                model.isStale = true;
            }
        }
    }
    for (auto &entry : generalizedGLMs_) {
        GeneralizedGLMState &state = entry.second;
        if (state.group != group) continue;
        const bool affects = state.response == variable ||
            std::find(state.terms.begin(), state.terms.end(), variable) != state.terms.end();
        if (!affects) continue;
        ++state.modelVersion;
        state.ok = false;
        if (state.autoRefit) effects.generalizedGlmIdsToRefit.push_back(entry.first);
    }
    for (auto &entry : generalizedComparisons_) {
        GeneralizedComparisonState &state = entry.second;
        if (state.group != group) continue;
        if (factorLike) {
            state.termTypes[variable] = "factor";
        } else if (numeric) {
            state.termTypes.erase(variable);
        }
        for (GeneralizedComparisonModel &model : state.models) {
            if (model.response == variable || GeneralizedModelIncludesTerm(model, variable)) {
                model.isStale = true;
            }
        }
    }
    return effects;
}

void ApplicationState::applyVariableRename(const std::string &group,
                                           const std::string &oldName,
                                           const std::string &newName)
{
    if (oldName.empty() || newName.empty() || oldName == newName) return;
    auto groupModel = groupModels_.find(group);
    if (groupModel != groupModels_.end()) {
        GroupModelState &state = groupModel->second;
        if (state.dependent == oldName) state.dependent = newName;
        RenameVariableInTermList(state.terms, oldName, newName);
        RenameStateMapKey(state.termTypes, oldName, newName);
        MarkGroupModelChanged(state);
    }
    if (labelColumn(group) == oldName) {
        // The caller has already renamed the column in DatasetRegistry.
        setLabelColumn(group, newName);
    }
    for (auto &entry : correlationMatrices_) {
        if (entry.second.group == group) {
            RenameVariableInTermList(entry.second.variables, oldName, newName);
        }
    }
    for (auto &entry : dendrograms_) {
        if (entry.second.group == group) {
            RenameVariableInTermList(entry.second.variables, oldName, newName);
        }
    }
    for (auto &entry : dimensionalityModels_) {
        if (entry.second.group == group) {
            RenameVariableInTermList(entry.second.variables, oldName, newName);
        }
    }
    for (auto &entry : regressionComparisons_) {
        RegressionComparisonState &state = entry.second;
        if (state.group != group) continue;
        if (state.response == oldName) state.response = newName;
        RenameVariableInTermList(state.termRows, oldName, newName);
        RenameStateMapKey(state.termTypes, oldName, newName);
        for (RegressionComparisonModel &model : state.models) {
            if (model.response == oldName) model.response = newName;
            RenameVariableInTermList(model.includedTerms, oldName, newName);
            model.isStale = true;
        }
    }
    for (auto &entry : generalizedGLMs_) {
        GeneralizedGLMState &state = entry.second;
        if (state.group != group) continue;
        if (state.response == oldName) state.response = newName;
        RenameVariableInTermList(state.terms, oldName, newName);
        ++state.modelVersion;
        state.ok = false;
    }
    for (auto &entry : generalizedComparisons_) {
        GeneralizedComparisonState &state = entry.second;
        if (state.group != group) continue;
        if (state.response == oldName) state.response = newName;
        RenameVariableInTermList(state.termRows, oldName, newName);
        RenameStateMapKey(state.termTypes, oldName, newName);
        for (GeneralizedComparisonModel &model : state.models) {
            if (model.response == oldName) model.response = newName;
            RenameVariableInTermList(model.includedTerms, oldName, newName);
            model.isStale = true;
        }
    }
}

RemovedGroupDomainState ApplicationState::removeGroupDomainState(const std::string &group)
{
    RemovedGroupDomainState removed;
    groupSelections_.erase(group);
    groupSelectionVersions_.erase(group);
    activeAnalysisScopes_.erase(group);
    analysisScopeVersions_.erase(group);
    savedSelections_.erase(group);
    groupSelectedColors_.erase(group);
    groupPointColors_.erase(group);
    groupLabelColumns_.erase(group);
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
    groupSelectionVersions_.clear();
    activeAnalysisScopes_.clear();
    analysisScopeVersions_.clear();
    savedSelections_.clear();
    groupSelectedColors_.clear();
    groupPointColors_.clear();
    plots_.clear();
    groupModels_.clear();
    correlationMatrices_.clear();
    dimensionalityModels_.clear();
    dendrograms_.clear();
    generalizedGLMs_.clear();
    nativeMixedModels_.clear();
    linearModelFits_.clear();
    regressionComparisons_.clear();
    modelTrellises_.clear();
    generalizedComparisons_.clear();
    groupLabelColumns_.clear();
    activePlotId_.clear();
}

} // namespace core
} // namespace rlispstat
