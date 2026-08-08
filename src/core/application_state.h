#ifndef RLISPSTAT_CORE_APPLICATION_STATE_H
#define RLISPSTAT_CORE_APPLICATION_STATE_H

#include "dataset_model.h"
#include "analysis_scope.h"
#include "correlation_model.h"
#include "glm_model.h"
#include "mixed_model.h"
#include "model_terms.h"
#include "model_trellis_model.h"
#include "dimensionality_model.h"
#include "dendrogram_model.h"
#include "selection_model.h"

#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace rlispstat {
namespace core {

struct RemovedGroupDomainState {
    std::vector<std::string> correlationIds;
    std::vector<std::string> dimensionalityIds;
    std::vector<std::string> dendrogramIds;
    std::vector<std::string> generalizedGlmIds;
    std::vector<std::string> generalizedComparisonIds;
    std::vector<std::string> mixedModelIds;
    std::vector<std::string> regressionComparisonIds;
    std::vector<std::string> modelTrellisIds;
};

struct VariableTypeChangeEffects {
    std::vector<std::string> correlationIds;
    std::vector<std::string> dimensionalityIds;
    std::vector<std::string> dendrogramIds;
    std::vector<std::string> generalizedGlmIdsToRefit;
};

class ApplicationState {
public:
    DatasetRegistry &datasets();
    const DatasetRegistry &datasets() const;
    // Registering a data frame also establishes the portable selection group
    // used by plots and analyses associated with that dataset.
    bool registerDataset(const DataFrameModel &dataset);
    bool eraseDataset(const std::string &group);

    // Selection is application state, independent from platform views. Callers
    // are responsible for synchronizing access when sharing the dispatcher.
    std::map<std::string, std::set<int>> &groupSelections();
    const std::map<std::string, std::set<int>> &groupSelections() const;
    std::map<std::string, int> &groupSelectionVersions();
    const std::map<std::string, int> &groupSelectionVersions() const;
    std::map<std::string, std::string> &groupSelectedColors();
    const std::map<std::string, std::string> &groupSelectedColors() const;
    std::map<std::string, std::map<int, std::string>> &groupPointColors();
    const std::map<std::string, std::map<int, std::string>> &groupPointColors() const;
    std::string &activePlotId();
    const std::string &activePlotId() const;
    // The registry indexes portable plot models. Platform code retains object
    // lifetime until view ownership can be migrated separately.
    std::map<std::string, PlotModel *> &plots();
    const std::map<std::string, PlotModel *> &plots() const;
    std::map<std::string, GroupModelState> &groupModels();
    const std::map<std::string, GroupModelState> &groupModels() const;
    std::map<std::string, CorrelationMatrixState> &correlationMatrices();
    const std::map<std::string, CorrelationMatrixState> &correlationMatrices() const;
    std::map<std::string, DimensionalityState> &dimensionalityModels();
    const std::map<std::string, DimensionalityState> &dimensionalityModels() const;
    std::map<std::string, DendrogramState> &dendrograms();
    const std::map<std::string, DendrogramState> &dendrograms() const;
    std::map<std::string, GeneralizedGLMState> &generalizedGLMs();
    const std::map<std::string, GeneralizedGLMState> &generalizedGLMs() const;
    std::map<std::string, NativeMixedModelState> &nativeMixedModels();
    const std::map<std::string, NativeMixedModelState> &nativeMixedModels() const;
    std::map<std::string, GLMFitSummary> &linearModelFits();
    const std::map<std::string, GLMFitSummary> &linearModelFits() const;
    std::map<std::string, RegressionComparisonState> &regressionComparisons();
    const std::map<std::string, RegressionComparisonState> &regressionComparisons() const;
    std::map<std::string, ModelTrellisState> &modelTrellises();
    const std::map<std::string, ModelTrellisState> &modelTrellises() const;

    bool selectedRows(const std::string &group, std::set<int> &rows) const;
    // A group is created by a portable plot or dataset registration. Platform
    // adapters may call this while attaching a native view, but callers do not
    // need access to the selection containers to establish that relationship.
    void ensureSelectionGroup(const std::string &group);
    bool hasSelectionGroup(const std::string &group) const;
    bool setSelectedRows(const std::string &group, const std::set<int> &rows,
                         int *selectionVersion = nullptr);
    bool clearSelectedRows(const std::string &group, int *selectionVersion = nullptr);

    AnalysisScope activeAnalysisScope(const std::string &datasetId) const;
    bool setActiveAnalysisScope(const AnalysisScope &scope,
                                AnalysisScopeChangeEvent *event = nullptr,
                                std::string *error = nullptr);
    bool setActiveAnalysisScopeFromSelection(const std::string &datasetId,
                                             AnalysisScopeSourceKind sourceKind,
                                             const std::string &description,
                                             const std::optional<std::string> &sourceViewId = std::nullopt,
                                             AnalysisScopeChangeEvent *event = nullptr,
                                             std::string *error = nullptr);
    bool resetActiveAnalysisScopeToAllObservations(
        const std::string &datasetId,
        AnalysisScopeChangeEvent *event = nullptr,
        std::string *error = nullptr);
    std::vector<int> resolveActiveAnalysisRowIds(const std::string &datasetId) const;
    std::size_t analysisScopeVersion(const std::string &datasetId) const;
    std::vector<SavedSelection> savedSelections(const std::string &datasetId) const;
    std::optional<SavedSelection> savedSelection(const std::string &datasetId,
                                                 const std::string &name) const;
    bool activateSavedSelection(const std::string &datasetId,
                                const std::string &name,
                                AnalysisScopeChangeEvent *event = nullptr,
                                std::string *error = nullptr);
    bool addSavedSelectionToActiveScope(const std::string &datasetId,
                                        const std::string &savedName,
                                        const std::string &resultName,
                                        AnalysisScopeChangeEvent *event = nullptr,
                                        std::string *error = nullptr);

    bool setSelectedColor(const std::string &group, const std::string &color);
    std::string selectedColor(const std::string &group) const;
    bool setPointColor(const std::string &group, int row, const std::string &color);
    std::vector<int> clearPointColors(const std::string &group,
                                      const std::vector<int> &rows = {});
    std::vector<std::pair<int, std::string>> pointColors(const std::string &group) const;
    bool setLabelColumn(const std::string &group, const std::string &column);
    std::string labelColumn(const std::string &group) const;
    // Applies the portable half of a dataset type transition. The platform
    // adapter receives the resulting effects only after the state is valid,
    // so it can synchronize views and schedule native work without owning the
    // validation or analysis rules.
    bool setVariableType(const std::string &group, const std::string &variable,
                         const std::string &type, VariableTypeChangeEffects &effects,
                         std::string *message = nullptr);
    VariableTypeChangeEffects applyVariableTypeChange(const std::string &group,
                                                       const std::string &variable,
                                                       const std::string &normalizedType);
    void applyVariableRename(const std::string &group,
                             const std::string &oldName,
                             const std::string &newName);

    // Removes domain state tied to a group after the platform has detached
    // every native representation of that group. The returned identifiers let
    // the adapter dispose of its corresponding views without exposing core
    // containers to the dispatcher.
    RemovedGroupDomainState removeGroupDomainState(const std::string &group);

    std::map<std::string, GeneralizedComparisonState> &generalizedComparisons();
    const std::map<std::string, GeneralizedComparisonState> &generalizedComparisons() const;
    void clearWorkbenchModels();

private:
    DatasetRegistry datasets_;
    std::map<std::string, std::set<int>> groupSelections_;
    std::map<std::string, int> groupSelectionVersions_;
    std::map<std::string, AnalysisScope> activeAnalysisScopes_;
    std::map<std::string, std::size_t> analysisScopeVersions_;
    std::map<std::string, std::vector<SavedSelection>> savedSelections_;
    std::map<std::string, std::string> groupSelectedColors_;
    std::map<std::string, std::map<int, std::string>> groupPointColors_;
    std::map<std::string, std::string> groupLabelColumns_;
    std::string activePlotId_;
    std::map<std::string, PlotModel *> plots_;
    std::map<std::string, GroupModelState> groupModels_;
    std::map<std::string, CorrelationMatrixState> correlationMatrices_;
    std::map<std::string, DimensionalityState> dimensionalityModels_;
    std::map<std::string, DendrogramState> dendrograms_;
    std::map<std::string, GeneralizedGLMState> generalizedGLMs_;
    std::map<std::string, NativeMixedModelState> nativeMixedModels_;
    std::map<std::string, GLMFitSummary> linearModelFits_;
    std::map<std::string, RegressionComparisonState> regressionComparisons_;
    std::map<std::string, ModelTrellisState> modelTrellises_;
    std::map<std::string, GeneralizedComparisonState> generalizedComparisons_;
};

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_APPLICATION_STATE_H
