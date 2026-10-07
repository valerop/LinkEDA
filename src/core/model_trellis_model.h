#ifndef RLISPSTAT_CORE_MODEL_TRELLIS_MODEL_H
#define RLISPSTAT_CORE_MODEL_TRELLIS_MODEL_H

#include "dataset_model.h"
#include "glm_model.h"
#include "analysis_scope.h"

#include <map>
#include <optional>
#include <set>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

enum class ModelTrellisFitMode { SeparatePanelFits };
enum class ModelTrellisPanelContent {
    CompactSummary,
    SelectedResult,
    Coefficients,
    TermTests,
    ModelFit
};
enum class ModelTrellisDimension { Rows, Columns };
enum class ModelTrellisPAdjustment { None, Holm, Bonferroni };
enum class ModelTrellisPanelArrangement { Automatic, OneRow, OneColumn };

struct GeneralLinearModelSpecification {
    std::string group;
    std::string response;
    std::vector<std::string> terms;
    std::map<std::string, std::string> termTypes;
    std::string scope = "all";
    bool includeIntercept = true;
    double confidenceLevel = 0.95;
    std::string missingPolicy = "complete_cases";
};

struct ModelTrellisConditioningVariable {
    std::string variableId;
    ModelTrellisDimension dimension = ModelTrellisDimension::Rows;
};

struct ModelTrellisSpecification {
    GeneralLinearModelSpecification baseModel;
    ModelTrellisFitMode fitMode = ModelTrellisFitMode::SeparatePanelFits;
    std::optional<ModelTrellisConditioningVariable> rowConditioningVariable;
    std::optional<ModelTrellisConditioningVariable> columnConditioningVariable;
    ModelTrellisPanelContent panelContent = ModelTrellisPanelContent::SelectedResult;
    std::optional<std::string> displayedTermId;
    std::optional<std::string> displayedCoefficientId;
    ModelTrellisPAdjustment pAdjustment = ModelTrellisPAdjustment::Holm;
    ModelTrellisPanelArrangement arrangement = ModelTrellisPanelArrangement::Automatic;
};

struct ModelTrellisPanelKey {
    std::optional<std::string> rowLevelId;
    std::optional<std::string> columnLevelId;
};

struct ModelTrellisCoefficientResult {
    std::string coefficientId;
    std::string sourceTermId;
    std::string label;
    std::string rowType = "coefficient";
    std::string termType = "numeric";
    std::string factorLevel;
    std::string referenceLevel;
    double estimate = NAN;
    double standardError = NAN;
    double statistic = NAN;
    double pValue = NAN;
    double adjustedPValue = NAN;
    double holmPValue = NAN;
    double bonferroniPValue = NAN;
    double confidenceLower = NAN;
    double confidenceUpper = NAN;
    bool estimable = false;
};

struct ModelTrellisTermTestResult {
    std::string termId;
    std::string label;
    double statistic = NAN;
    double df1 = NAN;
    double df2 = NAN;
    double pValue = NAN;
    double adjustedPValue = NAN;
    double holmPValue = NAN;
    double bonferroniPValue = NAN;
    bool estimable = false;
};

struct ModelTrellisPanelResult {
    std::string panelId;
    ModelTrellisPanelKey key;
    std::string rowLevelLabel;
    std::string columnLevelLabel;
    std::vector<int> candidateOriginalRows;
    std::vector<int> originalRowIndices;
    std::vector<std::string> effectiveTerms;
    std::vector<std::string> omittedTerms;
    GLMFitSummary modelResult;
    double adjustedGlobalPValue = NAN;
    double holmGlobalPValue = NAN;
    double bonferroniGlobalPValue = NAN;
    std::vector<ModelTrellisCoefficientResult> coefficients;
    std::vector<ModelTrellisTermTestResult> termTests;
    bool hasObservations = false;
    bool estimable = false;
    std::vector<std::string> warnings;
};

struct ModelTrellisState {
    std::string id;
    std::string sourceModelGroup;
    AnalysisScope dataScope;
    bool dataScopeCaptured = false;
    ModelTrellisSpecification specification;
    std::vector<ModelTrellisPanelResult> panels;
    int specificationGeneration = 1;
    int fittedGeneration = 0;
    bool fitPending = false;
    std::string status;
};

struct ModelTrellisLayoutPanel {
    std::string panelId;
    std::size_t row = 0;
    std::size_t column = 0;
};

struct ModelTrellisLayout {
    std::size_t rows = 1;
    std::size_t columns = 1;
    std::vector<ModelTrellisLayoutPanel> panels;
};

std::string ModelTrellisPanelContentId(ModelTrellisPanelContent value);
std::string ModelTrellisPAdjustmentId(ModelTrellisPAdjustment value);
std::string ModelTrellisPAdjustmentLabel(ModelTrellisPAdjustment value);
std::string ModelTrellisFormulaText(const GeneralLinearModelSpecification &specification,
                                    const std::vector<std::string> *terms = nullptr);
std::string ModelTrellisMethodologicalNote();
std::string ModelTrellisCopyNote();

std::vector<std::string> ModelTrellisCategoricalVariables(
    const DataFrameModel &data,
    const ModelTrellisSpecification &specification);
std::vector<std::string> ModelTrellisIndependentVariables(
    const DataFrameModel &data,
    const ModelTrellisSpecification &specification);
bool AddModelTrellisIndependentVariable(ModelTrellisSpecification &specification,
                                        const std::string &variable,
                                        const DataFrameModel &data,
                                        std::string *error = nullptr);
bool RemoveModelTrellisTerm(ModelTrellisSpecification &specification,
                            const std::string &term);
bool SetModelTrellisConditioningVariable(ModelTrellisSpecification &specification,
                                         ModelTrellisDimension dimension,
                                         const std::optional<std::string> &variable,
                                         const DataFrameModel &data,
                                         std::string *error = nullptr);
void SwapModelTrellisDimensions(ModelTrellisSpecification &specification);
void ApplyModelTrellisPAdjustment(ModelTrellisState &state);
std::vector<std::string> EffectiveModelTrellisTerms(
    const ModelTrellisSpecification &specification,
    std::vector<std::string> *omitted = nullptr);
std::vector<ModelTrellisPanelResult> BuildModelTrellisPanels(
    const ModelTrellisSpecification &specification,
    const DataFrameModel &data,
    const std::set<int> &selection = {});
ModelTrellisLayout BuildModelTrellisLayout(const ModelTrellisState &state);
std::string ModelTrellisSelectedResultCSV(const ModelTrellisState &state);
std::string ModelTrellisAllResultsCSV(const ModelTrellisState &state);
bool ApplyModelTrellisUpdatePayload(ModelTrellisState &state,
                                    int generation,
                                    const std::vector<std::string> &payload,
                                    std::size_t &cursor,
                                    std::string *error = nullptr);

} // namespace core
} // namespace rlispstat

#endif
