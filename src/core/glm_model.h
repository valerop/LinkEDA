#ifndef RLISPSTAT_CORE_GLM_MODEL_H
#define RLISPSTAT_CORE_GLM_MODEL_H

#include "plot_geometry.h"
#include "statistics_model.h"

#include <cstddef>
#include <limits>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

std::vector<std::string> GeneralizedLinksForFamily(const std::string &family);
std::string DefaultGeneralizedLink(const std::string &family);
bool IsValidGeneralizedFamily(const std::string &family);
bool IsValidGeneralizedLink(const std::string &family, const std::string &link);
void SortFactorLevelsLikeR(std::vector<std::string> &levels);
std::string GLMBaseVariableType(const PlotModel *model,
                                const std::string &variable,
                                const std::map<std::string, std::string> &termTypes);
NumericSummary GLMNumericSummaryForVariable(
    const DataColumn *col,
    const std::vector<NumericVariable> &numericVars,
    const std::string &variable,
    const std::set<int> &rows);
std::string DefaultGeneralizedFamilyForValues(const std::vector<double> &values);
std::string GeneralizedGLMRscriptLaunchFailedStatus();
std::string GeneralizedGLMRScript();

enum class BinaryLink {
    Logit,
    Probit
};

std::string BinaryLinkId(BinaryLink link);
std::string BinaryLinkLabel(BinaryLink link);
bool ParseBinaryLink(const std::string &value, BinaryLink &link);

struct BinaryResponseCoding {
    bool ok = false;
    bool numeric = false;
    std::string referenceValue;
    std::string referenceLabel;
    std::string eventValue;
    std::string eventLabel;
    int referenceCount = 0;
    int eventCount = 0;
    std::string status;
};

BinaryResponseCoding InspectBinaryResponse(const DataColumn &column,
                                           const std::set<int> &includedRows = {});

struct GeneralizedGLMState;

struct LinearOlsCoefficient {
    double estimate = std::numeric_limits<double>::quiet_NaN();
    double standardizedBeta = std::numeric_limits<double>::quiet_NaN();
    double stdError = std::numeric_limits<double>::quiet_NaN();
    double tValue = std::numeric_limits<double>::quiet_NaN();
    double pValue = std::numeric_limits<double>::quiet_NaN();
    double partialR2 = std::numeric_limits<double>::quiet_NaN();
};

struct LinearOlsDiagnosticRow {
    int row = 0;
    double observed = std::numeric_limits<double>::quiet_NaN();
    double fitted = std::numeric_limits<double>::quiet_NaN();
    double residual = std::numeric_limits<double>::quiet_NaN();
    double standardizedResidual = std::numeric_limits<double>::quiet_NaN();
    double studentizedResidual = std::numeric_limits<double>::quiet_NaN();
    double leverage = std::numeric_limits<double>::quiet_NaN();
    double cooksDistance = std::numeric_limits<double>::quiet_NaN();
    double sqrtAbsStandardizedResidual = std::numeric_limits<double>::quiet_NaN();
};

using GLMDiagnosticRow = LinearOlsDiagnosticRow;

struct GLMCoefficientRow {
    std::string term;
    std::string sourceTerm;
    std::string termType = "numeric";
    std::string rowType = "coefficient";
    std::string displayLabel;
    std::string factorLevel;
    std::string referenceLevel;
    double estimate = 0.0;
    double standardizedBeta = std::numeric_limits<double>::quiet_NaN();
    double stdError = 0.0;
    double tValue = 0.0;
    double pValue = std::numeric_limits<double>::quiet_NaN();
    double partialR2 = std::numeric_limits<double>::quiet_NaN();
};

struct GLMFitSummary {
    bool ok = false;
    int n = 0;
    int excluded = 0;
    int dfModel = 0;
    int dfResidual = 0;
    double r2 = 0.0;
    double adjR2 = 0.0;
    double globalF = std::numeric_limits<double>::quiet_NaN();
    double globalP = std::numeric_limits<double>::quiet_NaN();
    double ssRegression = std::numeric_limits<double>::quiet_NaN();
    double ssResidual = std::numeric_limits<double>::quiet_NaN();
    double msRegression = std::numeric_limits<double>::quiet_NaN();
    double msResidual = std::numeric_limits<double>::quiet_NaN();
    double rmse = 0.0;
    double sigma = 0.0;
    double aic = std::numeric_limits<double>::quiet_NaN();
    double bic = std::numeric_limits<double>::quiet_NaN();
    std::vector<GLMCoefficientRow> coefficients;
    std::vector<GLMDiagnosticRow> diagnostics;
    std::vector<double> beta;
    std::vector<std::vector<double>> covariance;
    std::vector<std::string> designLabels;
    std::vector<int> rowsUsed;
    std::vector<int> rowsExcluded;
    std::string warning;
};

struct LinearOlsInput {
    std::vector<std::vector<double>> designMatrix;
    std::vector<double> response;
    std::vector<int> rowIds;
};

struct LinearOlsResult {
    bool ok = false;
    int n = 0;
    int dfModel = 0;
    int dfResidual = 0;
    double r2 = 0.0;
    double adjR2 = 0.0;
    double globalF = std::numeric_limits<double>::quiet_NaN();
    double globalP = std::numeric_limits<double>::quiet_NaN();
    double ssRegression = std::numeric_limits<double>::quiet_NaN();
    double ssResidual = std::numeric_limits<double>::quiet_NaN();
    double msRegression = std::numeric_limits<double>::quiet_NaN();
    double msResidual = std::numeric_limits<double>::quiet_NaN();
    double rmse = 0.0;
    double sigma = 0.0;
    double aic = std::numeric_limits<double>::quiet_NaN();
    double bic = std::numeric_limits<double>::quiet_NaN();
    std::vector<double> beta;
    std::vector<std::vector<double>> covariance;
    std::vector<LinearOlsCoefficient> coefficients;
    std::vector<LinearOlsDiagnosticRow> diagnostics;
    std::string warning;
};

LinearOlsResult FitLinearOls(const LinearOlsInput &input);

struct NestedLinearModelSummary {
    bool ok = false;
    std::string response;
    std::vector<std::string> terms;
    std::vector<int> rowsUsed;
    int dfResidual = 0;
    double ssResidual = std::numeric_limits<double>::quiet_NaN();
};

struct NestedGeneralizedModelSummary {
    bool ok = false;
    std::string response;
    std::string family;
    std::string link;
    std::vector<std::string> terms;
    std::vector<int> rowsUsed;
    int dfResidual = 0;
    double residualDeviance = std::numeric_limits<double>::quiet_NaN();
    double dispersion = std::numeric_limits<double>::quiet_NaN();
};

struct NestedModelTestResult {
    bool ok = false;
    int df = 0;
    double df2 = std::numeric_limits<double>::quiet_NaN();
    double delta = std::numeric_limits<double>::quiet_NaN();
    double statistic = std::numeric_limits<double>::quiet_NaN();
    double p = std::numeric_limits<double>::quiet_NaN();
    std::string statisticName;
};

struct GeneralizedDiagnosticRow {
    int row = 0;
    std::string observedLabel;
    double observed = std::numeric_limits<double>::quiet_NaN();
    double observedBinary = std::numeric_limits<double>::quiet_NaN();
    double fitted = std::numeric_limits<double>::quiet_NaN();
    double linearPredictor = std::numeric_limits<double>::quiet_NaN();
    double devianceResidual = std::numeric_limits<double>::quiet_NaN();
    double pearsonResidual = std::numeric_limits<double>::quiet_NaN();
    double workingResidual = std::numeric_limits<double>::quiet_NaN();
    double leverage = std::numeric_limits<double>::quiet_NaN();
    double cooksDistance = std::numeric_limits<double>::quiet_NaN();
};

struct GeneralizedGLMRow {
    std::string rowType;
    std::string term;
    std::string displayLabel;
    std::string sourceTerm;
    std::string factorLevel;
    std::string referenceLevel;
    std::string termType;
    double estimate = std::numeric_limits<double>::quiet_NaN();
    double stdError = std::numeric_limits<double>::quiet_NaN();
    std::string statisticName = "z";
    double statistic = std::numeric_limits<double>::quiet_NaN();
    double pValue = std::numeric_limits<double>::quiet_NaN();
    double ciLower = std::numeric_limits<double>::quiet_NaN();
    double ciUpper = std::numeric_limits<double>::quiet_NaN();
    double oddsRatio = std::numeric_limits<double>::quiet_NaN();
    double oddsRatioLower = std::numeric_limits<double>::quiet_NaN();
    double oddsRatioUpper = std::numeric_limits<double>::quiet_NaN();
};

struct BinaryTermTestRow {
    std::string term;
    int df = 0;
    double statistic = std::numeric_limits<double>::quiet_NaN();
    double pValue = std::numeric_limits<double>::quiet_NaN();
};

struct BinaryPairwiseRow {
    std::string term;
    std::string contrast;
    std::string scale;
    double estimate = std::numeric_limits<double>::quiet_NaN();
    double stdError = std::numeric_limits<double>::quiet_NaN();
    double statistic = std::numeric_limits<double>::quiet_NaN();
    double pValue = std::numeric_limits<double>::quiet_NaN();
    double adjustedPValue = std::numeric_limits<double>::quiet_NaN();
    double ciLower = std::numeric_limits<double>::quiet_NaN();
    double ciUpper = std::numeric_limits<double>::quiet_NaN();
};

using GeneralizedGLMDiagnosticRow = GeneralizedDiagnosticRow;

struct GeneralizedGLMFitSummary {
    bool ok = false;
    std::string status = "Not fitted.";
    int n = 0;
    int excluded = 0;
    double nullDeviance = std::numeric_limits<double>::quiet_NaN();
    double residualDeviance = std::numeric_limits<double>::quiet_NaN();
    int dfResidual = 0;
    double aic = std::numeric_limits<double>::quiet_NaN();
    double bic = std::numeric_limits<double>::quiet_NaN();
    double dispersion = std::numeric_limits<double>::quiet_NaN();
    double logLik = std::numeric_limits<double>::quiet_NaN();
    std::string statisticName = "z";
    std::vector<GeneralizedGLMRow> rows;
    std::vector<int> rowsUsed;
    std::vector<int> rowsExcluded;
};

struct DiagnosticPlotPoint {
    double x = std::numeric_limits<double>::quiet_NaN();
    double y = std::numeric_limits<double>::quiet_NaN();
    int row = 0;
};

struct DiagnosticROCThreshold {
    double threshold = std::numeric_limits<double>::quiet_NaN();
    double falsePositiveRate = 0.0;
    double truePositiveRate = 0.0;
    int truePositive = 0;
    int falsePositive = 0;
    int trueNegative = 0;
    int falseNegative = 0;
    std::vector<int> crossingRows;
    std::vector<int> falsePositiveRows;
    std::vector<int> falseNegativeRows;
};

struct DiagnosticPlotData {
    bool ok = false;
    std::string kind = "scatter";
    std::string diagnosticKind;
    std::string title;
    std::string xLabel;
    std::string yLabel;
    std::string message;
    std::vector<DiagnosticPlotPoint> points;
    // ROC-only: visual step vertices are deliberately independent of the
    // unique empirical thresholds used for hit testing and row linking.
    std::vector<DiagnosticPlotPoint> visualPoints;
    std::vector<DiagnosticROCThreshold> rocThresholds;
    int fitVersion = 0;
    int diagnosticsVersion = 0;
};

struct LinearFunctionEstimate {
    std::string label;
    // Pairwise-comparison identity. These fields are empty for generic linear
    // functions and make pairwise views independent of formatted labels.
    std::string comparisonId;
    std::string factorName;
    std::string firstLevel;
    std::string secondLevel;
    double estimate = std::numeric_limits<double>::quiet_NaN();
    double stdError = std::numeric_limits<double>::quiet_NaN();
    double statistic = std::numeric_limits<double>::quiet_NaN();
    double pValue = std::numeric_limits<double>::quiet_NaN();
    double adjustedPValue = std::numeric_limits<double>::quiet_NaN();
    double degreesOfFreedom = std::numeric_limits<double>::quiet_NaN();
    double ciLower = std::numeric_limits<double>::quiet_NaN();
    double ciUpper = std::numeric_limits<double>::quiet_NaN();
};

struct ScenarioVariableInfo {
    std::string type = "numeric";
    double numericDefault = 0.0;
    std::vector<std::string> factorLevels;
};

struct ScenarioDesignInput {
    std::vector<std::string> terms;
    std::map<std::string, std::string> termTypes;
    std::map<std::string, ScenarioVariableInfo> variables;
    std::map<std::string, double> numericValues;
    std::map<std::string, std::string> factorValues;
    std::size_t expectedSize = 0;
};

bool SameIntegerSet(std::vector<int> left, std::vector<int> right);
bool TermVectorsAreNested(const std::vector<std::string> &left,
                          const std::vector<std::string> &right);
bool IsQuasiGeneralizedFamily(const std::string &family);
int ComparisonModelSerialFromId(const std::string &comparisonId,
                                const std::string &modelId);
int NextComparisonModelSerial(const std::string &comparisonId,
                              const std::vector<std::string> &modelIds);
std::string NextComparisonModelId(const std::string &comparisonId,
                                  const std::vector<std::string> &modelIds);
bool ComparisonLabelExists(const std::vector<std::string> &labels,
                           const std::string &label);
std::string NextUntitledComparisonLabel(const std::vector<std::string> &labels);
std::string UniqueRegressionComparisonCopyLabel(const std::vector<std::string> &labels,
                                                const std::string &label);
std::string UniqueGeneralizedComparisonCopyLabel(const std::vector<std::string> &labels,
                                                 const std::string &label);
std::vector<std::string> RegressionComparisonTermRowsFromFits(
    const std::vector<std::string> &existingRows,
    const std::vector<std::vector<std::string>> &includedTermsByModel,
    const std::vector<std::vector<GLMCoefficientRow>> &coefficientRowsByModel);
std::vector<std::string> GeneralizedComparisonTermRowsFromFits(
    const std::vector<std::vector<std::string>> &includedTermsByModel,
    const std::vector<std::vector<GeneralizedGLMRow>> &rowsByModel);
NestedLinearModelSummary BuildNestedLinearModelSummary(const GLMFitSummary &fit,
                                                       const std::string &response,
                                                       const std::vector<std::string> &terms);
NestedGeneralizedModelSummary BuildNestedGeneralizedModelSummary(bool ok,
                                                                 const std::string &response,
                                                                 const std::string &family,
                                                                 const std::string &link,
                                                                 const std::vector<std::string> &terms,
                                                                 const std::vector<int> &rowsUsed,
                                                                 int dfResidual,
                                                                 double residualDeviance,
                                                                 double dispersion);

bool ReadIntVectorPayload(const std::vector<std::string> &lines,
                          std::size_t &cursor,
                          std::vector<int> &out);
bool ReadLinearCoefficientRowsPayload(const std::vector<std::string> &lines,
                                      std::size_t &cursor,
                                      std::vector<GLMCoefficientRow> &rows);
bool ReadLinearFitPayload(const std::vector<std::string> &lines,
                          std::size_t &cursor,
                          GLMFitSummary &fit);
bool ReadLinearDiagnosticsPayload(const std::vector<std::string> &lines,
                                  std::size_t &cursor,
                                  std::vector<GLMDiagnosticRow> &diagnostics);
bool ReadLinearDesignPayload(const std::vector<std::string> &lines,
                             std::size_t &cursor,
                             GLMFitSummary &fit);
bool ReadGeneralizedRowsPayload(const std::vector<std::string> &lines,
                                std::size_t &cursor,
                                std::vector<GeneralizedGLMRow> &rows);
bool ReadGeneralizedFitPayload(const std::vector<std::string> &lines,
                               std::size_t &cursor,
                               GeneralizedGLMFitSummary &fit);
bool ReadGeneralizedStatePayload(const std::vector<std::string> &lines,
                                 std::size_t &cursor,
                                 GeneralizedGLMState &state);
const GLMCoefficientRow *CoefficientForTerm(const std::vector<GLMCoefficientRow> &rows,
                                            const std::string &term);
const GLMCoefficientRow *CoefficientForTerm(const GLMFitSummary &fit,
                                            const std::string &term);
std::vector<GLMCoefficientRow> CoefficientsForSourceTerm(const GLMFitSummary &fit,
                                                         const std::string &term);
std::string RegressionRowDisplayLabel(const GLMCoefficientRow *row,
                                      const std::string &fallback);
std::string RegressionRowSourceTerm(const GLMCoefficientRow *row,
                                    const std::string &fallback);
std::string RegressionComparisonFitLabel(int fitRow, bool multipleImputation);
bool RegressionComparisonFitRowIsModelTest(int fitRow);
std::string GeneralizedComparisonFitLabel(int fitRow);
std::string RegressionComparisonFootnote();
std::string GeneralizedComparisonFootnote();
struct GeneralizedComparisonFitDisplayData {
    bool ok = false;
    std::string family;
    std::string link;
    int n = 0;
    double nullDeviance = std::numeric_limits<double>::quiet_NaN();
    double residualDeviance = std::numeric_limits<double>::quiet_NaN();
    int dfResidual = 0;
    double aic = std::numeric_limits<double>::quiet_NaN();
    double bic = std::numeric_limits<double>::quiet_NaN();
    double dispersion = std::numeric_limits<double>::quiet_NaN();
    double logLik = std::numeric_limits<double>::quiet_NaN();
};
std::string RegressionComparisonFitDisplay(const GLMFitSummary &fit,
                                           const NestedModelTestResult &test,
                                           int fitRow);
std::vector<int> RegressionComparisonVisibleFitRows(bool showInformationCriteria);
std::vector<int> GeneralizedComparisonVisibleFitRows(bool binaryComparison,
                                                     bool showInformationCriteria);
std::string RegressionComparisonChangeDisplay(const GLMFitSummary &fit,
                                              const GLMFitSummary *previousFit,
                                              int fitRow);
std::string GeneralizedComparisonFitDisplay(const GeneralizedComparisonFitDisplayData &fit,
                                            const NestedModelTestResult &test,
                                            int fitRow);
struct ComparisonCopyTable {
    std::string title;
    std::string response;
    std::vector<std::string> modelLabels;
    std::vector<std::string> modelResponses;
    std::vector<std::string> termLabels;
    std::vector<std::vector<std::string>> termDisplaysByRow;
    std::vector<std::string> fitLabels;
    std::vector<std::vector<std::string>> fitDisplaysByRow;
    std::string footnote;
};
std::string ComparisonCopyTableText(const ComparisonCopyTable &table);
std::string RegressionComparisonModelTestStatus(const std::string &currentLabel,
                                                const std::string &previousLabel,
                                                const NestedModelTestResult &test,
                                                bool multipleImputation);
std::string RegressionComparisonNoPreviousModelStatus();
std::string RegressionComparisonUnavailableStatus();
std::string RegressionComparisonCopiedStatus();
std::string GeneralizedComparisonCopiedStatus();
std::string SelectedRowsStatus(std::size_t selected, std::size_t total);
std::string GLMPooledTableChangeInRStatus();
std::string GLMValueCopiedStatus();
std::string GLMFitStatisticCopiedStatus();
std::string GLMRegressionTableCopiedStatus();
std::string GLMCompareLinearModelsHintStatus();
std::string GLMSelectPredictorToRemoveStatus();
std::string GLMNoAvailablePredictorsStatus();
std::string GLMNoAvailableReplacementStatus();
std::string GLMTermActionHintStatus();
std::string GLMFittedStatus(int rows, int excluded, const std::string &scope);
std::string GLMPrecomputedTableRefitStatus(const std::string &action);
std::string GLMPooledMIScopeRefitStatus();
std::string GLMImputedDatasetUnavailableStatus();
std::string GLMRefittingPooledMIResponseStatus(const std::string &response);
std::string GLMRefittingPooledMITermStatus(const std::string &action,
                                           const std::string &term);
std::string GLMInteractionComponentTypeStatus(const std::string &term);
std::string GLMTermTypeChangedStatus(const std::string &term,
                                     const std::string &type);
std::string GLMModelTableCopiedStatus();
std::string GLMModelTableExportedStatus();
std::string GLMPDFWriteFailedStatus();
std::string GLMRegressionCopyTableText(const GLMFitSummary &fit,
                                       const std::string &dependent,
                                       bool usesPooledGlobalWald);
std::string GLMModelDetailsText(const GLMFitSummary &fit,
                               bool usesPooledGlobalWald);
std::string GeneralizedGLMModelDetailsText(const GeneralizedGLMState &state);
std::string GLMStatusMessage(const std::string &message);
std::string GLMFitNoteWarningStatus(const std::string &note,
                                    const std::string &warning);
std::string GLMInteractionReportOpenedStatus(const std::string &term);
std::string GLMInteractionPlotOpenedStatus(const std::string &term);
std::string GeneralizedCoefficientDetailsStatus(const GeneralizedGLMRow &row,
                                                const std::string &defaultStatisticName);
std::string ComparisonTermNotIncludedStatus();
std::string GeneralizedComparisonCoefficientDetailsStatus(const GeneralizedGLMRow &row);
std::string RegressionCoefficientGroupDetailsStatus(const std::vector<GLMCoefficientRow> &rows);
std::string RegressionCoefficientStructuralDetailsStatus(const GLMCoefficientRow &row);
std::string RegressionComparisonFitValueStatus(const std::string &modelLabel,
                                               const std::string &fitLabel,
                                               const std::string &value);
std::string RegressionComparisonModelRenamedStatus();
std::string RegressionComparisonOpenedSingleModelStatus(const std::string &modelLabel);
std::string GeneralizedGLMWindowStatus(const std::string &status,
                                       std::size_t selected,
                                       std::size_t total,
                                       const std::string &family,
                                       const std::string &link);
std::string GeneralizedGLMFormattedOutputText(const GeneralizedGLMState &state,
                                              bool showModelDetails = false);
std::string RegressionComparisonSummaryStatus(std::size_t modelCount,
                                              std::size_t termCount,
                                              bool preferNote,
                                              const std::string &note);
std::string GeneralizedComparisonSummaryStatus(std::size_t modelCount,
                                               std::size_t termCount);
std::string ComparisonNoAvailableTermsStatus();
std::string PrecomputedComparisonChangeInRStatus(const std::string &subject);
std::string PooledMIComparisonChangeInRStatus();
std::string PooledMITermTypesChangeInRStatus();
std::string PooledMIAutoRefitDisabledStatus();
std::string PooledOrPrecomputedRefitLayerStatus();
std::string PooledMIComparisonRefitStatus(const std::string &reason = "",
                                          bool viaMice = false);
std::string RegressionComparisonRefittedStatus();
std::string RowsUsedExcludedText(const std::vector<int> &rowsUsed,
                                 const std::vector<int> &rowsExcluded,
                                 const std::string &title = "");
std::string RegressionCoefficientDisplay(const GLMFitSummary &fit,
                                         const std::vector<std::string> &includedTerms,
                                         const std::string &term);
GeneralizedGLMRow *GeneralizedRowForTerm(std::vector<GeneralizedGLMRow> &rows,
                                         const std::string &term);
const GeneralizedGLMRow *GeneralizedRowForTerm(const std::vector<GeneralizedGLMRow> &rows,
                                               const std::string &term);
std::string GeneralizedRowDisplayLabel(const GeneralizedGLMRow *row,
                                       const std::string &fallback);
std::string GeneralizedRowSourceTerm(const GeneralizedGLMRow *row,
                                     const std::string &fallback);
std::string GeneralizedCoefficientDisplay(const std::vector<GeneralizedGLMRow> &rows,
                                          bool fitOk,
                                          const std::vector<std::string> &includedTerms,
                                          const std::string &term);

NestedModelTestResult LinearNestedModelTest(const NestedLinearModelSummary &left,
                                            const NestedLinearModelSummary &right);
NestedModelTestResult GeneralizedNestedModelTest(const NestedGeneralizedModelSummary &left,
                                                 const NestedGeneralizedModelSummary &right);

std::string NormalizeLinearDiagnosticKind(const std::string &kind);
bool LinearDiagnosticKindIsImplemented(const std::string &kind);
std::string NormalizeGeneralizedDiagnosticKind(const std::string &kind);
double GeneralizedResidualForDiagnostic(const GeneralizedDiagnosticRow &row,
                                        const std::string &residualType);
DiagnosticPlotData BuildLinearDiagnosticPlotData(const std::string &kind,
                                                 const std::vector<LinearOlsDiagnosticRow> &diagnostics,
                                                 int fitVersion,
                                                 int diagnosticsVersion);
DiagnosticPlotData BuildGeneralizedDiagnosticPlotData(const std::string &kind,
                                                      const std::string &residualType,
                                                      const std::vector<GeneralizedDiagnosticRow> &diagnostics,
                                                      int fitVersion,
                                                      int diagnosticsVersion);
std::vector<double> SubtractVectors(const std::vector<double> &left,
                                    const std::vector<double> &right);
double ContrastEstimate(const std::vector<double> &beta,
                        const std::vector<double> &contrast);
double ContrastVariance(const std::vector<std::vector<double>> &covariance,
                        const std::vector<double> &contrast);
LinearFunctionEstimate EstimateLinearFunction(const std::vector<double> &beta,
                                              const std::vector<std::vector<double>> &covariance,
                                              const std::vector<double> &contrast,
                                              const std::string &label,
                                              double confidenceLevel);
std::vector<double> HolmAdjustedPValues(const std::vector<double> &pValues);
void ApplyHolmAdjustedPValues(std::vector<LinearFunctionEstimate> &rows);
std::vector<double> MainEffectScenarioValues(const std::string &term,
                                             const ScenarioDesignInput &input);
std::vector<double> TermScenarioValues(const std::string &term,
                                       const ScenarioDesignInput &input);
bool BuildScenarioDesignVector(const ScenarioDesignInput &input,
                               std::vector<double> &out);
std::string GeneralizedGLMFitFailedStatus();
std::string GLMNoActiveInteractionDataStatus();
std::string GLMInteractionModelNotFittedStatus();
std::string GLMChooseResponseVariableStatus();
std::string GLMInvalidFamilyStatus();
std::string GLMInvalidLinkForFamilyStatus();
std::string GLMNoRowsForScopeStatus();
std::string GLMInteractionTwoWayOnlyStatus();
std::string GLMInteractionNoFittedValuesStatus();
std::string GLMInteractionFactorNoLevelsStatus();
std::string GLMInteractionTypeNotAvailableStatus();
std::string GLMInteractionNoFiniteValuesStatus();
std::string GLMChooseDependentVariableStatus();
std::string GLMDependentVariableNotAvailableStatus();
std::string GLMPredictorUnavailableStatus(const std::string &term);
std::string GLMFactorPredictorUnavailableStatus(const std::string &term);
std::string GLMNumericPredictorUnavailableStatus(const std::string &term);
std::string GLMNoDiagnosticRowsStatus();
std::string GLMNoActiveDiagnosticStatus();
std::string GLMCurrentLinearDiagnosticUnavailableStatus(const std::string &kind);
std::string GLMSelectedLinearDiagnosticUnavailableStatus(const std::string &kind);
std::string GLMDiagnosticStateSummaryText(const std::string &group,
                                          const std::string &dependent,
                                          const std::vector<std::string> &predictors,
                                          int validPredictors,
                                          int modelVersion,
                                          int fitVersion,
                                          bool isStale,
                                          bool hasDataSeed,
                                          std::size_t usedRows,
                                          std::size_t excludedRows);
std::string GLMCurrentDiagnosticFitFailedStatus(const std::string &summary);
std::string GLMSelectedDiagnosticFitFailedStatus(const std::string &warning);
std::string GLMCurrentGeneralizedDiagnosticFitFailedStatus(const std::string &status);
std::string GLMSelectedGeneralizedDiagnosticFitFailedStatus(const std::string &status);
std::string GLMReadyStatus();
std::string GLMNoInteractionTermStatus();
std::string GLMRegressionComparisonNotAvailableStatus();
std::string GLMChooseValidModelColumnStatus();
std::string GLMGeneralizedLinearModelNotAvailableStatus();
std::string GLMGeneralizedModelComparisonNotAvailableStatus();
std::string GLMNoNumericResponseVariablesStatus();
std::string GLMImputedDatasetNotAvailableStatus();
std::string GLMAddPredictorBeforeTableStatus();
std::string GLMComparisonRequiresPooledTestStatus();
std::string GLMNoInteractionReportTextStatus();
std::string GLMWindowTitle();
std::string GLMGeneralizedModelComparisonTitle();
std::string GLMGeneralLinearModelTitle();
std::string GLMAutoRefitButtonTitle();
std::string GLMDiagnosticResidualHistogramTitle();
std::string GLMDiagnosticResidualsFittedTitle();
std::string GLMDiagnosticObservedFittedTitle();
std::string GLMDiagnosticNormalQQTitle();
std::string GLMDevianceResidualTitle();
std::string GLMPearsonResidualTitle();
std::string GLMWorkingResidualTitle();
std::string GLMAddTermTitle();
std::string GLMChangeTermTitle();
std::string GLMResponseFieldLabel();
std::string GLMFamilyFieldLabel();
std::string GLMLinkFieldLabel();
std::string GLMScopeFieldLabel();
std::string GLMResidualFieldLabel();
std::string GLMResponseVariableIsFieldLabel();
std::string GLMSourceTableHeader();
std::string GLMRegressionDefaultModelName();
std::string GLMInteractionPlotWindowTitle();
std::string GLMDiagnosticPlotWindowTitle();
std::string GLMRenameModelTitle();
std::string GLMDuplicateModelTitle();
std::string GLMDeleteModelTitle();
std::string GLMReportInteractionTermPrefix();
std::string GLMReportTypePrefix();
std::string GLMReportConfidenceLevelPrefix();
std::string GLMReportMultipleComparisonsPrefix();
std::string GLMReportNotePrefix();
std::string GLMReportEffectHeader();
std::string GLMComparisonFamilyMenuTitle();
std::string GLMComparisonLinkMenuTitle();
std::string GLMComparisonFamilyMenuItemTitle();
std::string GLMComparisonLinkMenuItemTitle();
std::string GLMModelFitSectionTitle();
std::string GLMTermsSectionTitle();
std::string GLMFitNLabel();
std::string GLMFitNullDevianceLabel();
std::string GLMFitResidualDevianceLabel();
std::string GLMFitDfResidualLabel();
std::string GLMFitAICLabel();
std::string GLMFitBICLabel();
std::string GLMFitDispersionLabel();
std::string GLMFitLogLikLabel();
std::string GLMCoefVariableHeader();
std::string GLMCoefTypeHeader();
std::string GLMCoefBHeader();
std::string GLMCoefSEHeader();
std::string GLMCoefStatisticHeader();
std::string GLMCoefPHeader();
std::string GLMAnovaSumSquaresHeader();
std::string GLMAnovaDfHeader();
std::string GLMAnovaMeanSquareHeader();
std::string GLMAnovaFRatioHeader();
std::string GLMAnovaPHeader();
std::string GLMAnovaResidualLabel();
std::string GLMRowsUsedExcludedWindowTitle();
std::string GLMAddModelLabel();
std::string GLMAddTermLabel();
std::string GLMPlusSignLabel();
std::string GLMSelectedRowsPlaceholder();
std::string GLMD1WaldLabel();
std::string GLMAnovaFRatioLabel();
std::string GLMRegressionBetaLabel();
std::string GLMRegressionPartialRSquaredLabel();
std::string GLMInteractionsMenuTitle();
std::string GLMGeneralLinearTHeader();
std::string GLMGeneralLinearTermsHeader();
std::string GLMGeneralLinearVariableHeader();
std::string GLMGeneralLinearTypeHeader();
std::string GLMGeneralLinearBetaHeader();
std::string GLMInterceptTermName();
std::string GLMDashPlaceholder();
std::string GLMFitGlobalFitSectionTitle();
std::string GLMFitRSquaredLabel();
std::string GLMFitAdjustedRSquaredLabel();
std::string GLMFitSValueLabel();
std::string GLMFitDFLabel();

struct GroupModelState {
    std::string group;
    std::string dependent;
    std::vector<std::string> terms;
    std::map<std::string, std::string> termTypes;
    std::string scope = "all";
    AnalysisScope dataScope;
    bool dataScopeCaptured = false;
    bool precomputed = false;
    bool multipleImputation = false;
    int imputationCount = 0;
    std::string title;
    std::string note;
    int modelVersion = 0;
    int fitVersion = 0;
    int diagnosticsVersion = 0;
    bool isStale = true;
    bool rFitPending = false;
    std::string lastRFitSignature;
    std::vector<GLMDiagnosticRow> diagnostics;
    std::vector<int> rowsUsed;
    std::vector<int> rowsExcluded;
};

bool HasModelTerm(const GroupModelState &state, const std::string &term);
void MarkGroupModelChanged(GroupModelState &state);
GroupModelState &EnsureGroupModelState(std::map<std::string, GroupModelState> &states,
                                       const std::string &group,
                                       const PlotModel *seed = nullptr);

struct GLMDesignColumn {
    std::string label;
    std::string sourceTerm;
    std::string termType;
    std::string factorLevel;
    std::string referenceLevel;
    std::vector<double> values;
};

struct GLMFactorInfo {
    std::string term;
    std::vector<std::string> levels;
};

struct GLMInteractionReport {
    bool ok = false;
    std::string term;
    std::string kind;
    std::string message;
    std::vector<std::pair<std::string, std::vector<LinearFunctionEstimate>>> sections;
};

// A typed request for estimated-marginal-mean pairwise comparisons of one
// simple categorical main effect in a fitted Gaussian linear model.
struct GLMPairwiseComparisonRequest {
    std::string term;
    std::string adjustment = "tukey";
    double confidenceLevel = 0.95;
};

struct GLMPairwiseComparisonEligibility {
    bool eligible = false;
    std::string term;
    std::string message;
    std::vector<std::string> levels;
};

struct GLMPairwiseComparisonResult {
    bool ok = false;
    std::string term;
    std::string adjustment = "tukey";
    std::string method;
    std::string message;
    int degreesOfFreedom = 0;
    std::vector<std::string> levels;
    std::vector<LinearFunctionEstimate> comparisons;
};

struct RegressionComparisonModel {
    std::string id;
    std::string label;
    std::string response;
    std::vector<std::string> includedTerms;
    GLMFitSummary fit;
    bool comparisonOk = false;
    int comparisonDf = 0;
    double comparisonDf2 = NAN;
    double comparisonDelta = NAN;
    double comparisonStatistic = NAN;
    double comparisonP = NAN;
    int modelVersion = 0;
    int fitVersion = 0;
    bool isStale = true;
};

struct RegressionComparisonState {
    std::string id;
    std::string group;
    std::string response;
    std::string scope = "all";
    AnalysisScope dataScope;
    bool dataScopeCaptured = false;
    bool autoRefit = true;
    bool showInformationCriteria = false;
    bool precomputed = false;
    bool multipleImputation = false;
    int imputationCount = 0;
    std::string title;
    std::string note;
    std::vector<std::string> termRows;
    std::map<std::string, std::string> termTypes;
    std::vector<RegressionComparisonModel> models;
    int activeModel = 0;
    bool rFitPending = false;
    std::string lastRFitSignature;
    PlotModel seed;
    bool hasSeed = false;
};

struct GeneralizedGLMState {
    std::string id;
    std::string group;
    std::string response;
    std::vector<std::string> terms;
    std::map<std::string, std::string> termTypes;
    std::string family = "binomial";
    std::string link = "logit";
    bool binaryRegression = false;
    BinaryLink binaryLink = BinaryLink::Logit;
    BinaryResponseCoding responseCoding;
    bool responseCodingExplicit = false;
    std::string scope = "all";
    AnalysisScope dataScope;
    bool dataScopeCaptured = false;
    bool autoRefit = true;
    bool precomputed = false;
    bool multipleImputation = false;
    int imputationCount = 0;
    std::string title;
    std::string note;
    std::string residualType = "deviance";
    int modelVersion = 0;
    int fitVersion = 0;
    int diagnosticsVersion = 0;
    bool ok = false;
    bool rFitPending = false;
    std::string lastRFitSignature;
    std::string status = "Not fitted.";
    int n = 0;
    int excluded = 0;
    double nullDeviance = NAN;
    double residualDeviance = NAN;
    int dfResidual = 0;
    double aic = NAN;
    double bic = NAN;
    double dispersion = NAN;
    double logLik = NAN;
    int dfModel = 0;
    double globalLR = NAN;
    double globalP = NAN;
    double mcfaddenR2 = NAN;
    double coxSnellR2 = NAN;
    double nagelkerkeR2 = NAN;
    double auc = NAN;
    double calibrationIntercept = NAN;
    double calibrationSlope = NAN;
    bool converged = false;
    int iterations = 0;
    bool boundary = false;
    int rank = 0;
    int parameterCount = 0;
    bool rankDeficient = false;
    std::vector<std::string> warnings;
    std::string statisticName = "z";
    std::vector<GeneralizedGLMRow> rows;
    std::vector<BinaryTermTestRow> termTests;
    std::vector<BinaryPairwiseRow> pairwise;
    std::vector<GeneralizedGLMDiagnosticRow> diagnostics;
    std::vector<int> rowsUsed;
    std::vector<int> rowsExcluded;
    PlotModel seed;
    bool hasSeed = false;
};

struct GeneralizedComparisonModel {
    std::string id;
    std::string label;
    std::string response;
    std::string family = "binomial";
    std::string link = "logit";
    std::vector<std::string> includedTerms;
    GeneralizedGLMState fit;
    int modelVersion = 0;
    int fitVersion = 0;
    bool isStale = true;
    bool comparisonOk = false;
    bool comparisonCalculatedInR = false;
    int comparisonDf = 0;
    double comparisonDelta = NAN;
    double comparisonStatistic = NAN;
    double comparisonP = NAN;
};

struct GeneralizedComparisonState {
    std::string id;
    std::string group;
    std::string response;
    std::string family = "binomial";
    std::string link = "logit";
    std::string scope = "all";
    AnalysisScope dataScope;
    bool dataScopeCaptured = false;
    bool autoRefit = true;
    bool showInformationCriteria = false;
    bool binaryComparison = false;
    BinaryLink binaryLink = BinaryLink::Logit;
    BinaryResponseCoding responseCoding;
    std::vector<int> commonRows;
    std::vector<std::string> termRows;
    std::map<std::string, std::string> termTypes;
    std::vector<GeneralizedComparisonModel> models;
    int activeModel = 0;
    PlotModel seed;
    bool hasSeed = false;
};

std::string LinearGLMFitSignature(const std::string &dependent,
                                  const std::vector<std::string> &terms,
                                  const std::map<std::string, std::string> &termTypes,
                                  const std::string &scope);
std::string GeneralizedGLMFitSignature(const GeneralizedGLMState &state);
std::string RegressionComparisonFitSignature(const RegressionComparisonState &state);

std::string EncodePooledRegressionComparisonModelSpec(const RegressionComparisonState &state);

bool HasRegressionTerm(const RegressionComparisonState &state, const std::string &term);
bool ModelIncludesTerm(const RegressionComparisonModel &model, const std::string &term);
void AddRegressionTermRow(RegressionComparisonState &state, const std::string &term);
std::vector<std::string> RegressionComparisonModelIds(const RegressionComparisonState &state);
std::vector<std::string> RegressionComparisonModelLabels(const RegressionComparisonState &state);
std::string NextRegressionModelId(const RegressionComparisonState &state);
std::string NextUntitledRegressionLabel(const RegressionComparisonState &state);
std::string UniqueRegressionCopyLabel(const RegressionComparisonState &state, const std::string &label);

bool HasGeneralizedComparisonTerm(const GeneralizedComparisonState &state, const std::string &term);
bool GeneralizedModelIncludesTerm(const GeneralizedComparisonModel &model, const std::string &term);
void AddGeneralizedComparisonTermRow(GeneralizedComparisonState &state, const std::string &term);
std::vector<std::string> GeneralizedComparisonModelIds(const GeneralizedComparisonState &state);
std::vector<std::string> GeneralizedComparisonModelLabels(const GeneralizedComparisonState &state);
std::string NextGeneralizedComparisonModelId(const GeneralizedComparisonState &state);
std::string NextUntitledGeneralizedComparisonLabel(const GeneralizedComparisonState &state);
std::string UniqueGeneralizedComparisonCopyLabelForState(const GeneralizedComparisonState &state, const std::string &label);

std::string GLMInteractionReportText(const GLMInteractionReport &report);

GLMFitSummary FitMultipleLinearModel(const PlotModel &model,
                                     const DataFrameModel *dataframe,
                                     const std::set<int> &selectedRows,
                                     const std::string &dependent,
                                     const std::vector<std::string> &terms,
                                     const std::map<std::string, std::string> &termTypes,
                                     const std::string &scope);
GLMInteractionReport BuildGLMInteractionReport(const PlotModel &model,
                                               const DataFrameModel *dataframe,
                                               const std::string &term,
                                               const std::vector<std::string> &terms,
                                               const std::map<std::string, std::string> &termTypes,
                                               const GLMFitSummary &fit,
                                               double confidenceLevel = 0.95);
GLMPairwiseComparisonEligibility EvaluateGLMPairwiseComparisonEligibility(
    const PlotModel &model,
    const DataFrameModel *dataframe,
    const std::string &dependent,
    const std::vector<std::string> &terms,
    const std::map<std::string, std::string> &termTypes,
    const GLMFitSummary &fit,
    const GLMPairwiseComparisonRequest &request);
std::string GLMPairwiseComparisonsRScript();
std::string BinaryPairwiseComparisonsRScript();
bool ParseGLMPairwiseComparisonsRResult(const std::vector<std::string> &lines,
                                        GLMPairwiseComparisonResult &result);
std::string GLMPairwiseComparisonsText(const GLMPairwiseComparisonResult &result);

inline bool GLMRowIncluded(const std::set<int> &rows, int row)
{
    return rows.empty() || rows.find(row) != rows.end();
}

inline std::map<std::string, size_t> GLMLevelIndex(const std::vector<std::string> &levels)
{
    std::map<std::string, size_t> index;
    for (size_t i = 0; i < levels.size(); ++i) index[levels[i]] = i;
    return index;
}

NestedModelTestResult RegressionAdjacentModelTest(const RegressionComparisonState &state, int modelColumn);
NestedModelTestResult GeneralizedAdjacentModelTest(const GeneralizedComparisonState &state, int modelColumn);
bool BinaryModelsCompatibleForLikelihoodRatioTest(const GeneralizedGLMState &left,
                                                   const GeneralizedGLMState &right,
                                                   std::string *reason = nullptr);

void GLMAddInteractionLinePoint(PlotModel *plot, InteractionPlotLine &line, double x, double y);

void RefreshGeneralizedTermRowsFromFits(GeneralizedComparisonState &state);
const GeneralizedGLMRow *DisplayRowForGeneralizedComparisonTerm(const GeneralizedComparisonState &state,
                                                                const std::string &term);
std::string GeneralizedComparisonDisplayLabel(const GeneralizedComparisonState &state,
                                              const std::string &term);
std::string GeneralizedComparisonSourceTerm(const GeneralizedComparisonState &state,
                                            const std::string &displayTerm);

void RefreshRegressionTermRowsFromFits(RegressionComparisonState &state);
void PruneRegressionComparisonRowsForCurrentTypes(RegressionComparisonState &state);
void InferRegressionComparisonTermTypesFromFitRows(RegressionComparisonState &state);
void EnsureRegressionComparisonFactorLevelRows(RegressionComparisonState &state,
                                               const std::string &term);
bool ApplyRegressionComparisonTermType(RegressionComparisonState &state,
                                      const std::string &term,
                                      const std::string &type);
const GLMCoefficientRow *DisplayRowForComparisonTerm(const RegressionComparisonState &state,
                                                     const std::string &term);
std::string RegressionComparisonDisplayLabel(const RegressionComparisonState &state,
                                             const std::string &term);
std::string RegressionComparisonSourceTerm(const RegressionComparisonState &state,
                                            const std::string &term);

void PopulateMissingStandardizedBetas(GLMFitSummary &fit, const PlotModel &seed,
                                      const std::string &dependent);

std::string DefaultGeneralizedFamilyForResponse(const PlotModel &model, const std::string &response);

std::set<int> GLMRowsUsedSet(const GLMFitSummary &fit);

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_GLM_MODEL_H
