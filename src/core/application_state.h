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
#include "scale_analysis_model.h"
#include "dendrogram_model.h"
#include "selection_model.h"
#include "window_note_model.h"

#include <map>
#include <cstdint>
#include <functional>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace rlispstat {
namespace core {

struct RemovedGroupDomainState {
    std::vector<std::string> correlationIds;
    std::vector<std::string> dimensionalityIds;
    std::vector<std::string> scaleAnalysisIds;
    std::vector<std::string> dendrogramIds;
    std::vector<std::string> generalizedGlmIds;
    std::vector<std::string> generalizedComparisonIds;
    std::vector<std::string> mixedModelIds;
    std::vector<std::string> regressionComparisonIds;
    std::vector<std::string> modelTrellisIds;
};

struct VariableTypeChangeEffects {
    std::vector<std::string> trellisPlotIds;
    std::vector<std::string> correlationIds;
    std::vector<std::string> dimensionalityIds;
    std::vector<std::string> scaleAnalysisIds;
    std::vector<std::string> dendrogramIds;
    std::vector<std::string> generalizedGlmIds;
    std::vector<std::string> generalizedGlmIdsToRefit;
    std::vector<std::string> mixedModelIds;
    std::vector<std::string> regressionComparisonIds;
    std::vector<std::string> generalizedComparisonIds;
    std::vector<std::string> modelTrellisIds;
};

struct DatasetValueChangeEffects {
    bool groupModel = false;
    std::vector<std::string> correlationIds;
    std::vector<std::string> dimensionalityIds;
    std::vector<std::string> scaleAnalysisIds;
    std::vector<std::string> dendrogramIds;
    std::vector<std::string> generalizedGlmIds;
    std::vector<std::string> generalizedGlmIdsToRefit;
    std::vector<std::string> mixedModelIds;
    std::vector<std::string> regressionComparisonIds;
    std::vector<std::string> generalizedComparisonIds;
    std::vector<std::string> modelTrellisIds;
};

// Persistent case colours, a temporary display override, and saved colour
// schemes are deliberately separate pieces of dataset state.  Row numbers are
// sufficient for the live override because it is recomputed from the current
// dataset.  Saved schemes use stable case identifiers so that a reordered or
// revised dataset cannot silently change their meaning.
struct ColorOverrideState {
    std::string datasetId;
    std::string variable;
    std::map<int, std::string> rowColors;
    std::vector<std::pair<std::string, std::string>> legendItems;
    std::map<std::string, std::vector<int>> legendRows;
};

struct NamedColorScheme {
    std::string datasetId;
    std::string name;
    std::uint64_t dataVersion = 0;
    AnalysisScope scope;
    std::vector<std::string> scopeStableRowIds;
    std::map<std::string, std::string> colorsByStableRowId;
};

struct ColorSchemeCompatibility {
    bool exactScope = false;
    bool sameDataVersion = false;
    std::size_t currentScopeCount = 0;
    std::size_t schemeScopeCount = 0;
    std::size_t matchingObservationCount = 0;
    std::size_t missingObservationCount = 0;
};

class ApplicationState {
public:
    DatasetRegistry &datasets();
    const DatasetRegistry &datasets() const;
    std::string dataSheetGroupForPlotGroup(const std::string &group) const;
    // Registering a data frame also establishes the portable selection group
    // used by plots and analyses associated with that dataset.
    bool registerDataset(const DataFrameModel &dataset);
    bool eraseDataset(const std::string &group);
    // Explicitly close a data file and every live analysis/plot derived from it.
    // Unlike eraseDataset(), this does not retain frozen result provenance.
    bool closeDatasetAndAnalyses(const std::string &group);

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
    std::map<std::string, ScaleAnalysisState> &scaleAnalyses();
    const std::map<std::string, ScaleAnalysisState> &scaleAnalyses() const;
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

    // Exclusion is shared by the dataset, independent of brushing and of the
    // selected analysis scope. Row IDs are one-based original dataset rows.
    std::set<int> excludedRows(const std::string &datasetId) const;
    bool isRowExcluded(const std::string &datasetId, int originalRowId) const;
    bool setExcludedRows(const std::string &datasetId, const std::set<int> &rows,
                         AnalysisScopeChangeEvent *event = nullptr,
                         std::string *error = nullptr);

    AnalysisScope activeAnalysisScope(const std::string &datasetId) const;
    AnalysisScope baseAnalysisScope(const std::string &datasetId) const;
    bool setActiveAnalysisScope(const AnalysisScope &scope,
                                AnalysisScopeChangeEvent *event = nullptr,
                                std::string *error = nullptr);
    bool setActiveAnalysisScopeFromSelection(const std::string &datasetId,
                                             AnalysisScopeSourceKind sourceKind,
                                             const std::string &description,
                                             const std::optional<std::string> &sourceViewId = std::nullopt,
                                             AnalysisScopeChangeEvent *event = nullptr,
                                             std::string *error = nullptr);
    bool setActiveAnalysisScopeFromUnselected(const std::string &datasetId,
        AnalysisScopeChangeEvent *event = nullptr, std::string *error = nullptr);
    bool resetActiveAnalysisScopeToAllObservations(
        const std::string &datasetId,
        AnalysisScopeChangeEvent *event = nullptr,
        std::string *error = nullptr);
    bool saveCurrentSelectionAsAnalysisScope(const std::string &datasetId,
                                             const std::string &name,
                                             AnalysisScopeSourceKind sourceKind =
                                                 AnalysisScopeSourceKind::CurrentSelection,
                                             const std::optional<std::string> &sourceViewId = std::nullopt,
                                             AnalysisScopeChangeEvent *event = nullptr,
                                             std::string *error = nullptr);
    std::vector<int> resolveActiveAnalysisRowIds(const std::string &datasetId) const;
    // Called at an analysis execution boundary. Existing result snapshots are
    // never rebound merely because the global state or brushing changes.
    bool captureAnalysisScope(const std::string &datasetId, AnalysisScope &snapshot,
                              bool &captured, std::string *legacyScope = nullptr) const;
    std::size_t analysisScopeVersion(const std::string &datasetId) const;
    std::vector<SavedSelection> savedSelections(const std::string &datasetId) const;
    bool replaceSavedSelections(const std::string &datasetId,
                                const std::vector<SavedSelection> &selections,
                                std::string *error = nullptr);
    std::optional<SavedSelection> savedSelection(const std::string &datasetId,
                                                 const std::string &name) const;
    bool resolveSavedAnalysisScopeChoice(const std::string &datasetId,
                                         const std::string &choiceValue,
                                         AnalysisScope &scope,
                                         std::string *error = nullptr) const;
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
    // Effective display colours use the temporary categorical override when
    // one is active and otherwise return persistent case colours.
    std::vector<std::pair<int, std::string>> displayPointColors(
        const std::string &group) const;
    std::string displayPointColor(const std::string &group, int row) const;
    std::optional<ColorOverrideState> colorOverride(const std::string &group) const;
    bool setColorOverride(const std::string &group, const std::string &variable,
                          std::string *error = nullptr);
    bool refreshColorOverride(const std::string &group,
                              std::string *error = nullptr);
    bool cancelColorOverride(const std::string &group);
    // Makes the currently displayed categorical override the persistent case
    // colour layer, then removes the override itself.
    bool promoteColorOverrideToPersistent(const std::string &group);
    bool saveCurrentColorsAsScheme(const std::string &group,
                                   const std::string &name,
                                   std::string *error = nullptr);
    std::vector<NamedColorScheme> colorSchemes(const std::string &group) const;
    std::optional<NamedColorScheme> colorScheme(const std::string &group,
                                                const std::string &name) const;
    ColorSchemeCompatibility colorSchemeCompatibility(
        const std::string &group, const std::string &name,
        std::string *error = nullptr) const;
    bool applyColorSchemeToMatchingObservations(const std::string &group,
                                                const std::string &name,
                                                std::vector<int> *changedRows = nullptr,
                                                std::string *error = nullptr);
    bool switchToColorSchemeScopeAndApply(const std::string &group,
                                          const std::string &name,
                                          AnalysisScopeChangeEvent *scopeEvent = nullptr,
                                          std::vector<int> *changedRows = nullptr,
                                          std::string *error = nullptr);
    bool replaceColorSchemes(const std::string &group,
                             const std::vector<NamedColorScheme> &schemes,
                             std::string *error = nullptr);
    bool setLabelColumn(const std::string &group, const std::string &column);
    std::string labelColumn(const std::string &group) const;
    std::optional<WindowNote> documentNote(const std::string &group) const;
    bool setDocumentNote(const std::string &group,
                         const std::optional<WindowNote> &note);
    // Dataset-level defaults used only to initialize a newly-created
    // analysis. They are deliberately independent from every live analysis
    // specification; editing a model must never write back into these roles.
    std::map<std::string, std::string> variableRoles(const std::string &group) const;
    bool setDefaultVariableRole(const std::string &group,
                                const std::string &variable,
                                const std::string &role,
                                std::string *error = nullptr);
    bool restoreVariableRoles(const std::string &group,
                              const std::map<std::string, std::string> &roles,
                              std::string *error = nullptr);
    // Applies the portable half of a dataset type transition. The platform
    // adapter receives the resulting effects only after the state is valid,
    // so it can synchronize views and schedule native work without owning the
    // validation or analysis rules.
    bool setVariableType(const std::string &group, const std::string &variable,
                         const std::string &type, VariableTypeChangeEffects &effects,
                         std::string *message = nullptr,
                         const VariableTypeConversionSpecification *conversion = nullptr);
    VariableTypeChangeEffects applyVariableTypeChange(const std::string &group,
                                                       const std::string &variable,
                                                       const std::string &normalizedType);
    DatasetValueChangeEffects applyDatasetValueChange(const std::string &group,
                                                      const std::string &variable);
    bool datasetCalculationPending(const std::string &group) const;
    DatasetValueChangeEffects applyVariableRename(const std::string &group,
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
    void registerOutputCodeReference(const OutputCodeReference &reference);
    std::function<void(const OutputCodeReference &)> outputCodeReferenceChanged;
    bool eraseOutputCodeReference(const std::string &id);
    const OutputCodeReference *outputCodeReference(const std::string &id) const;
    // Resolves the owning table for a derived regression result. Comparison
    // plots store the individual column id, whereas code references are owned
    // by the comparison itself; single linear models use the historical
    // "glm:" output id. Keep that translation out of platform code.
    const OutputCodeReference *regressionSourceOutputCodeReference(
        const std::string &sourceKind,
        const std::string &sourceModelId) const;
    std::vector<OutputCodeReference> outputCodeReferencesForDataset(
        const std::string &datasetId) const;
    void restoreOutputCodeReferences(const std::vector<OutputCodeReference> &references);
    const DataFrameModel *datasetVersion(const DataVersionReference &version) const;
    std::vector<DataFrameModel> referencedHistoricalDatasetVersions(
        const std::string &datasetId) const;
    void restoreHistoricalDatasetVersions(const std::vector<DataFrameModel> &versions);
    void preserveCurrentDatasetVersion(const std::string &datasetId);

private:
    DatasetRegistry datasets_;
    std::map<std::string, std::set<int>> groupSelections_;
    std::map<std::string, std::set<int>> excludedRows_;
    std::map<std::string, int> groupSelectionVersions_;
    std::map<std::string, AnalysisScope> activeAnalysisScopes_;
    std::map<std::string, std::size_t> analysisScopeVersions_;
    std::map<std::string, std::vector<SavedSelection>> savedSelections_;
    std::map<std::string, std::string> groupSelectedColors_;
    std::map<std::string, std::map<int, std::string>> groupPointColors_;
    std::map<std::string, ColorOverrideState> groupColorOverrides_;
    std::map<std::string, std::vector<NamedColorScheme>> groupColorSchemes_;
    std::map<std::string, std::string> groupLabelColumns_;
    std::map<std::string, WindowNote> groupDocumentNotes_;
    // Sparse storage: missing entries mean "none". At most one variable in a
    // dataset may have the dependent default role.
    std::map<std::string, std::map<std::string, std::string>> groupDefaultVariableRoles_;
    std::string activePlotId_;
    std::map<std::string, PlotModel *> plots_;
    std::map<std::string, GroupModelState> groupModels_;
    std::map<std::string, CorrelationMatrixState> correlationMatrices_;
    std::map<std::string, DimensionalityState> dimensionalityModels_;
    std::map<std::string, ScaleAnalysisState> scaleAnalyses_;
    std::map<std::string, DendrogramState> dendrograms_;
    std::map<std::string, GeneralizedGLMState> generalizedGLMs_;
    std::map<std::string, NativeMixedModelState> nativeMixedModels_;
    std::map<std::string, GLMFitSummary> linearModelFits_;
    std::map<std::string, RegressionComparisonState> regressionComparisons_;
    std::map<std::string, ModelTrellisState> modelTrellises_;
    std::map<std::string, GeneralizedComparisonState> generalizedComparisons_;
    std::map<std::string, OutputCodeReference> outputCodeReferences_;
    // One immutable shared copy per referenced historical version. Results
    // retain keys into this store; they never own complete dataset copies.
    std::map<std::string, DataFrameModel> historicalDatasetVersions_;
    void pruneUnreferencedHistoricalDatasetVersions();
};

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_APPLICATION_STATE_H
