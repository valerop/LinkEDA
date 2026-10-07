#ifndef RLISPSTAT_CORE_GLM_MODEL_H
#define RLISPSTAT_CORE_GLM_MODEL_H

#include "analysis_initialization.h"
#include "model_terms.h"
#include "plot_geometry.h"
#include "statistics_model.h"

#include <cstddef>
#include <cstdint>
#include <limits>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

// Platform-neutral statistical model catalogue.  User-facing workflows must
// derive their distributions, links, response validation, fit backend, and
// output capabilities from this catalogue instead of maintaining UI-local
// family lists.  The legacy generalized workflow remains representable for
// old documents and programmatic R APIs, but is not a normal menu choice.
enum class StatisticalModelType {
    Linear,
    Binary,
    Count,
    PositiveContinuous,
    Proportion,
    LegacyGeneralized
};

enum class ResponseDomain {
    Continuous,
    Binary,
    NonNegativeInteger,
    StrictlyPositiveContinuous,
    OpenUnitInterval,
    OpenClosedUnitInterval,
    Any
};

enum class StatisticalFitBackend {
    StatsLm,
    StatsGlm,
    MassGlmNb,
    BetaReg,
    GamlssBetaOneInflated,
    GlmmTMBBetaBinomial,
    GamlssCeilingHurdleBetaBinomial
};

struct DistributionSpec {
    std::string id;
    std::string visibleName;
    ResponseDomain responseDomain = ResponseDomain::Any;
    std::vector<std::string> supportedLinks;
    std::string defaultLink;
    StatisticalFitBackend fitBackend = StatisticalFitBackend::StatsGlm;
    bool fullProbabilityDistribution = true;
    bool supportsLikelihood = true;
    bool supportsAIC = true;
    bool supportsBIC = true;
    bool supportsDeviance = true;
    bool supportsDispersionParameter = false;
    bool supportsMultipleImputation = true;
    bool supportsEstimatedMarginalMeans = true;
    bool supportsContrasts = true;
    bool supportsPredictions = true;
};

struct StatisticalModelTypeSpec {
    StatisticalModelType type = StatisticalModelType::LegacyGeneralized;
    std::string id;
    std::string visibleName;
    ResponseDomain responseDomain = ResponseDomain::Any;
    std::vector<std::string> distributionIds;
    // Quasi-likelihood choices live here rather than in distributionIds.
    std::vector<std::string> inferenceOptionIds;
    bool modelTypeSelectorAvailable = false;
    // Orthogonal structure dimension reserved for Standard / zero-inflated /
    // hurdle count models. Only implemented structures are returned to a UI.
    std::vector<std::string> modelStructureIds{"standard"};
    std::string defaultModelStructureId = "standard";
};

std::string StatisticalModelTypeId(StatisticalModelType type);
std::string StatisticalModelTypeLabel(StatisticalModelType type);
std::string StatisticalModelWindowTitle(StatisticalModelType type,
                                        bool multipleImputation = false);
std::string StatisticalModelComparisonTitle(StatisticalModelType type,
                                            bool multipleImputation = false);
bool ParseStatisticalModelType(const std::string &value, StatisticalModelType &type);
const StatisticalModelTypeSpec &ModelTypeSpecification(StatisticalModelType type);
const DistributionSpec *FindDistributionSpecification(const std::string &id);
std::vector<DistributionSpec> DistributionSpecificationsForModel(StatisticalModelType type);
std::vector<DistributionSpec> InferenceOptionSpecificationsForModel(StatisticalModelType type);
std::vector<std::string> ModelStructuresForModel(StatisticalModelType type);
bool DistributionIsAvailableForModel(StatisticalModelType type, const std::string &id);
std::vector<std::string> LinksForModelDistribution(StatisticalModelType type,
                                                   const std::string &distributionId);
std::string DefaultLinkForModelDistribution(StatisticalModelType type,
                                            const std::string &distributionId);
std::string StatisticalFitBackendId(StatisticalFitBackend backend);
StatisticalModelType StatisticalModelTypeForFamily(const std::string &family,
                                                   bool binaryRegression,
                                                   bool countRegression);

std::vector<std::string> GeneralizedLinksForFamily(const std::string &family);
std::string DefaultGeneralizedLink(const std::string &family);
bool IsValidGeneralizedFamily(const std::string &family);
bool IsValidGeneralizedLink(const std::string &family, const std::string &link);
bool GeneralizedFamilyRequiresResponseBounds(const std::string &family);
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
    Log,
    Probit,
    Cloglog
};

std::string BinaryLinkId(BinaryLink link);
std::string BinaryLinkLabel(BinaryLink link);
bool ParseBinaryLink(const std::string &value, BinaryLink &link);
std::vector<BinaryLink> SupportedBinaryLinks();

enum class CountDistribution {
    Poisson,
    QuasiPoisson,
    NegativeBinomial,
    BinomialTrials,
    BetaBinomial,
    HurdleBetaBinomialCeiling,
    PerfectScore
};

std::string CountDistributionId(CountDistribution distribution);
std::string CountDistributionLabel(CountDistribution distribution);
bool ParseCountDistribution(const std::string &value,
                            CountDistribution &distribution);
bool CountDistributionUsesTrials(CountDistribution distribution);
bool CountDistributionSupportsScoreDistribution(CountDistribution distribution);

struct CountResponseValidation {
    bool ok = false;
    int observed = 0;
    int missing = 0;
    std::string status;
};

struct CountExposureValidation {
    bool ok = false;
    int observed = 0;
    int missing = 0;
    std::string status;
};

struct ResponseDomainValidation {
    bool ok = false;
    int observed = 0;
    int missing = 0;
    std::string status;
};

CountResponseValidation InspectCountResponse(
    const DataColumn &column,
    const std::set<int> &includedRows = {});
CountExposureValidation InspectCountExposure(
    const DataColumn &column,
    const std::set<int> &includedRows = {});
ResponseDomainValidation InspectResponseDomain(
    const DataColumn &column,
    ResponseDomain domain,
    const std::set<int> &includedRows = {});

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
struct GeneralizedGLMRow;
struct GlobalTermTestRow;
bool GeneralizedModelHasExponentiatedEffect(const GeneralizedGLMState &state);
std::string GeneralizedExponentiatedEffectLabel(const GeneralizedGLMState &state);
bool GeneralizedModelShowsGlobalTermTests(const GeneralizedGLMState &state);
std::string GlobalTermTestMethodLabel(const std::string &method);
bool GeneralizedModelUsesMixedGlobalTermTestMethods(const GeneralizedGLMState &state);
std::string GeneralizedGlobalTermTestStatisticHeader(const GeneralizedGLMState &state);
std::string GeneralizedGlobalTermTestHierarchyNote(const GeneralizedGLMState &state);
const GlobalTermTestRow *GeneralizedGlobalTermTestForPresentationRow(
    const GeneralizedGLMState &state,
    const GeneralizedGLMRow &row);
std::string GeneralizedGlobalTermTestMethodNote(const GeneralizedGLMState &state);
std::string GeneralizedGlobalTermTestPooledWaldFootnote(const GeneralizedGLMState &state);

struct LinearOlsCoefficient {
    double estimate = std::numeric_limits<double>::quiet_NaN();
    double standardizedBeta = std::numeric_limits<double>::quiet_NaN();
    double stdError = std::numeric_limits<double>::quiet_NaN();
    double tValue = std::numeric_limits<double>::quiet_NaN();
    double pValue = std::numeric_limits<double>::quiet_NaN();
    double partialR = std::numeric_limits<double>::quiet_NaN();
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
    double qqRawQuantile = std::numeric_limits<double>::quiet_NaN();
    double qqStandardizedQuantile = std::numeric_limits<double>::quiet_NaN();
    double qqStudentizedQuantile = std::numeric_limits<double>::quiet_NaN();
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
    double partialR = std::numeric_limits<double>::quiet_NaN();
    double deltaR2 = std::numeric_limits<double>::quiet_NaN();
};

// Presentation semantics for the General Linear Model terms table.  Blank and
// not-applicable cells are intentionally different: a blank cell belongs to a
// hierarchical parent/child layout, whereas not-applicable is rendered as an
// em dash.  Keeping that distinction in the core prevents native front ends
// from independently interpreting NaN values.
enum class GLMTableDisplayState {
    Value,
    Blank,
    NotApplicable,
    Unavailable
};

enum class GLMTableColumn {
    Variable,
    Type,
    Estimate,
    StandardizedBeta,
    StandardError,
    Statistic,
    PValue,
    PartialR,
    DeltaRSquared
};

struct GLMTableCellDisplay {
    GLMTableDisplayState state = GLMTableDisplayState::Blank;
    std::string formattedValue;
};

GLMTableCellDisplay GLMRegressionTableCell(const GLMCoefficientRow &row,
                                           GLMTableColumn column);
std::string GLMRegressionCoefficientDisplayLabel(
    const GLMCoefficientRow &row,
    const std::set<std::string> &centeredPredictors);
std::vector<GLMTableCellDisplay> GLMRegressionTableRow(
    const GLMCoefficientRow &row);
std::vector<GLMTableCellDisplay> GLMRegressionTableRow(
    const GLMCoefficientRow &row,
    const std::set<std::string> &centeredPredictors);
std::string GLMTableDisplayStateId(GLMTableDisplayState state);
std::string GLMRegressionTableCellText(const GLMTableCellDisplay &cell);

// Platform-neutral sizing policy for native GLM result tables. Statistical
// columns keep their content-driven minimum widths; only explicitly marked
// textual columns may absorb spare window width.
struct GLMResultTableColumnLayout {
    double minimumWidth = 0.0;
    bool absorbsExtraWidth = false;
    // Zero means unbounded. Text-heavy result columns can use this to grow
    // with their content without consuming an arbitrarily wide window.
    double maximumWidth = 0.0;
};

struct GLMResultTableLayout {
    std::vector<GLMResultTableColumnLayout> columns;
    bool stretchesToAvailableWidth = false;
};

GLMResultTableLayout GLMAnovaResultTableLayout();
GLMResultTableLayout GLMTermsResultTableLayout();
std::vector<double> ResolveGLMResultTableColumnWidths(
    const GLMResultTableLayout &layout, double availableWidth);
bool GLMTermsTableNeedsRebuild(bool initialized,
                               const std::vector<std::string> &currentStructure,
                               const std::vector<std::string> &nextStructure);

// Exact categorical coding used by the fitted design matrix.  Keeping the
// level-to-contrast mapping with the fit prevents post-estimation views from
// silently rebuilding treatment coding in data-observation order.
struct GLMFactorCoding {
    std::string variable;
    std::vector<std::string> levels;
    std::string referenceLevel;
    std::vector<std::string> contrastLabels;
    std::vector<std::vector<double>> coding;
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
    // Row-level diagnostics are never pooled under multiple imputation.
    // Each entry is taken from the corresponding fitted model in the mira.
    std::vector<std::vector<GLMDiagnosticRow>> diagnosticsByImputation;
    std::vector<double> beta;
    std::vector<std::vector<double>> covariance;
    std::vector<std::string> designLabels;
    std::vector<GLMFactorCoding> factorCodings;
    // Exact constants subtracted before the fitted design matrix was built.
    // Keeping them with the fit makes prediction, interaction interpretation,
    // display, and export use the same parameterisation as estimation.
    std::map<std::string, double> predictorCenters;
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
    double dunnSmythResidual = std::numeric_limits<double>::quiet_NaN();
    double rawResidual = std::numeric_limits<double>::quiet_NaN();
    double devianceResidual = std::numeric_limits<double>::quiet_NaN();
    double pearsonResidual = std::numeric_limits<double>::quiet_NaN();
    double workingResidual = std::numeric_limits<double>::quiet_NaN();
    double standardizedResidual = std::numeric_limits<double>::quiet_NaN();
    double studentizedResidual = std::numeric_limits<double>::quiet_NaN();
    double leverage = std::numeric_limits<double>::quiet_NaN();
    double cooksDistance = std::numeric_limits<double>::quiet_NaN();
    std::map<std::string, double> qqTheoreticalQuantiles;
};

struct GeneralizedGLMRow {
    std::string component;
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
    double rateRatio = std::numeric_limits<double>::quiet_NaN();
    double rateRatioLower = std::numeric_limits<double>::quiet_NaN();
    double rateRatioUpper = std::numeric_limits<double>::quiet_NaN();
    double exponentiatedEstimate = std::numeric_limits<double>::quiet_NaN();
    double exponentiatedLower = std::numeric_limits<double>::quiet_NaN();
    double exponentiatedUpper = std::numeric_limits<double>::quiet_NaN();
};

struct GlobalTermTestRow {
    std::string component;
    std::string term;
    std::string coefficientNames;
    std::string method;
    double statistic = std::numeric_limits<double>::quiet_NaN();
    double df = std::numeric_limits<double>::quiet_NaN();
    double df2 = std::numeric_limits<double>::quiet_NaN();
    double pValue = std::numeric_limits<double>::quiet_NaN();
};

// Source compatibility for older platform code while the user-facing concept
// is generalized beyond Binary models.
using BinaryTermTestRow = GlobalTermTestRow;

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
    bool likelihoodAvailable = true;
    bool likelihoodRatioAvailable = true;
    double theta = std::numeric_limits<double>::quiet_NaN();
    std::string statisticName = "z";
    std::vector<GeneralizedGLMRow> rows;
    std::vector<int> rowsUsed;
    std::vector<int> rowsExcluded;
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

// Diagnostic choices are presentation/inspection state, not part of the
// fitted model specification.  They therefore have their own revision so an
// already-open diagnostic plot can invalidate its numerical data without
// refitting (or pretending that the fitted model changed).
struct DiagnosticOptions {
    // The neutral persisted default remains deviance for continuous GLMs.
    // Discrete models replace it with their capability-map default
    // (Dunn--Smyth when an exact PMF/CDF is available) when the model state
    // is synchronized.
    std::string residualType = "deviance";
    int version = 0;
};

struct GeneralizedGLMState;

struct BoundedCountDistributionPoint {
    double score = std::numeric_limits<double>::quiet_NaN();
    double observedFrequency = std::numeric_limits<double>::quiet_NaN();
    double predictedFrequency = std::numeric_limits<double>::quiet_NaN();
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
    // Non-row-wise diagnostic series, such as the observed and model-implied
    // bounded-score frequencies. Their numerical values are calculated in R;
    // native code only transports and renders them.
    std::vector<InteractionPlotLine> lines;
    bool showIdentityLine = false;
    // Row-wise coordinates produced by the independently fitted model in
    // every completed dataset.  The displayed point is their arithmetic
    // centre; these raw coordinates drive the uncertainty glyph and are not
    // pooled coefficients or a substitute fitted model.
    std::vector<ScatterplotPointImputationValues> imputationValues;
    // ROC-only: visual step vertices are deliberately independent of the
    // unique empirical thresholds used for hit testing and row linking.
    std::vector<DiagnosticPlotPoint> visualPoints;
    std::vector<DiagnosticROCThreshold> rocThresholds;
    int fitVersion = 0;
    int diagnosticsVersion = 0;
    int diagnosticOptionsVersion = 0;
    std::string residualType;
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
    std::string factorReferenceLevel;
    std::map<std::string, std::vector<double>> factorCoding;
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
bool ReadLinearMIDiagnosticsPayload(const std::vector<std::string> &lines,
                                    std::size_t &cursor,
                                    GLMFitSummary &fit);
bool ReadLinearDesignPayload(const std::vector<std::string> &lines,
                             std::size_t &cursor,
                             GLMFitSummary &fit);
const GLMFactorCoding *FactorCodingForVariable(const GLMFitSummary &fit,
                                              const std::string &variable);
bool ApplyFittedFactorCoding(const GLMFitSummary &fit,
                            const std::string &variable,
                            ScenarioVariableInfo &info);
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
std::string CountRegressionComparisonFootnote();
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
struct GeneralizedModelCapabilities {
    bool hasLikelihood = true;
    bool hasAIC = true;
    bool supportsNestedLRComparison = true;
};
struct GeneralizedComparisonState;
std::string RegressionComparisonFitDisplay(const GLMFitSummary &fit,
                                           const NestedModelTestResult &test,
                                           int fitRow);
std::vector<int> RegressionComparisonVisibleFitRows(bool showInformationCriteria);
std::vector<int> GeneralizedComparisonVisibleFitRows(bool binaryComparison,
                                                     bool showInformationCriteria);
std::vector<int> GeneralizedComparisonVisibleFitRows(const GeneralizedComparisonState &state);
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
std::string ComparisonInteractionNotIncludedMessage(const std::string &term,
                                                    const std::string &modelLabel);
std::string GeneralizedCoefficientDetailsStatus(const GeneralizedGLMRow &row,
                                                const std::string &defaultStatisticName);
std::string ComparisonTermNotIncludedStatus();
std::string GeneralizedComparisonCoefficientDetailsStatus(const GeneralizedGLMRow &row,
                                                          bool includeOddsRatio = false);
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
std::string CountModelParameterSummary(const GeneralizedGLMState &state);
struct GeneralizedFamilyDiagnosticRow {
    std::string statistic;
    std::string display;
};
std::vector<GeneralizedFamilyDiagnosticRow> GeneralizedFamilyDiagnosticRows(
    const GeneralizedGLMState &state);
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
// Comparison tables need a presentation identity in addition to the semantic
// coefficient name.  A numeric-by-numeric interaction legitimately has both
// a term-parent row and a coefficient row named, for example, `x:z`.
const GeneralizedGLMRow *GeneralizedComparisonRowForPresentationTerm(
    const std::vector<GeneralizedGLMRow> &rows,
    const std::string &presentationTerm);
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
DiagnosticPlotData BuildLinearDiagnosticPlotData(const std::string &kind,
                                                 const std::string &residualType,
                                                 const std::vector<LinearOlsDiagnosticRow> &diagnostics,
                                                 int fitVersion,
                                                 int diagnosticsVersion);
DiagnosticPlotData BuildGeneralizedDiagnosticPlotData(const std::string &kind,
                                                      const DiagnosticOptions &options,
                                                      const std::vector<GeneralizedDiagnosticRow> &diagnostics,
                                                      int fitVersion,
                                                      int diagnosticsVersion);
DiagnosticPlotData BuildBoundedCountDistributionDiagnosticPlotData(
    const std::vector<BoundedCountDistributionPoint> &distribution,
    int fitVersion,
    int diagnosticsVersion);
DiagnosticPlotData BuildDiscreteBoundaryDiagnosticPlotData(
    const std::vector<BoundedCountDistributionPoint> &distribution,
    bool includeCeiling,
    int fitVersion,
    int diagnosticsVersion);
OutputCodeReference GeneralizedDiagnosticPlotCodeReference(
    const PlotModel &plot,
    const GeneralizedGLMState &state);
void ApplyDiagnosticIdentityRange(PlotModel &plot);
DiagnosticPlotData BuildGeneralizedDiagnosticPlotData(const std::string &kind,
                                                      const std::string &residualType,
                                                      const std::vector<GeneralizedDiagnosticRow> &diagnostics,
                                                      int fitVersion,
                                                      int diagnosticsVersion);
DiagnosticPlotData BuildDiagnosticPlotDataAcrossImputations(
    const std::vector<DiagnosticPlotData> &perImputation);
DiagnosticPlotData BuildDiagnosticPlotDataAcrossImputations(
    const std::vector<DiagnosticPlotData> &perImputation,
    const std::set<int> &rowsWithImputedModelInputs);
// A diagnostic opened from a worksheet already displaying all completed
// datasets should inherit that presentation state. ROC, normal Q-Q, and
// histogram plots are excluded because the uncertainty renderer requires
// stable row-wise scatterplot coordinates across imputations.
bool DiagnosticImputationUncertaintyShouldBeDefault(
    const DataFrameModel &dataframe,
    const std::string &diagnosticKind);
// Multiple-imputation diagnostics must be built from the accepted collection
// of per-imputation fits.  A platform must not substitute a provisional fit
// of the currently visible completed dataset while the pooled fit is pending.
bool DiagnosticRequiresAcceptedMultipleImputationFit(
    const DataFrameModel *dataframe,
    bool modelUsesMultipleImputation);
// Original-row ids (one based) whose response or any predictor used by the
// exact model specification was imputed.  Diagnostic coordinates may vary
// between independently fitted imputations even for completely observed
// rows; only this mask is evidence of row-level imputation uncertainty.
std::set<int> RowsWithImputedModelInputs(
    const DataFrameModel &dataframe,
    const ModelSpecification &specification,
    const std::vector<std::string> &additionalVariables = {});
// Shared menu capabilities for every residual-bearing diagnostic projection.
bool DiagnosticPlotUsesResidualChoice(const PlotModel &plot);
bool DiagnosticPlotAxisUsesResidualChoice(const PlotModel &plot, bool xAxis);
std::vector<std::string> DiagnosticPlotResidualChoices(const PlotModel &plot);
std::vector<std::string> AvailableLinearResidualTypes();
std::vector<std::string> AvailableGeneralizedResidualTypes(const GeneralizedGLMState &state);
std::string DefaultGeneralizedResidualType(const GeneralizedGLMState &state);
std::string GeneralizedResidualTypeLabel(const std::string &residualType);
bool IsGeneralizedResidualTypeAvailable(const GeneralizedGLMState &state,
                                        const std::string &residualType);
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
std::string GLMDunnSmythResidualTitle();
std::string GLMRawResidualTitle();
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
std::string RegressionComparisonAddTermLabel();
std::string GLMPlusSignLabel();
std::string GLMSelectedRowsPlaceholder();
std::string GLMD1WaldLabel();
std::string GLMAnovaFRatioLabel();
std::string GLMRegressionBetaLabel();
std::string GLMRegressionPartialRLabel();
std::string GLMRegressionDeltaRSquaredLabel();
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

// Portable semantic help for native result tables.  Frontends pass the
// statistic identity rather than a formatted value so macOS and Windows show
// the same interpretation and the same multiple-imputation qualification.
struct ModelStatisticExplanationContext {
    std::string statistic;
    // Optional non-regression context.  These fields let the same semantic
    // help engine explain descriptive, contingency, correlation,
    // mean-comparison, dimensionality and pairwise-result tables without the
    // native front ends inventing their own wording.
    std::string analysisKind;
    std::string method;
    std::string adjustment;
    std::string detail;
    std::string modelFamily;
    std::string link;
    bool comparison = false;
    bool multipleImputation = false;
    bool rubinRulesApplied = false;
    // Empty for ordinary statistics; otherwise the actual method reported by
    // R (for example "mice::pool", "mice::D1" or "mice::D3").
    std::string miMethod;
    bool likelihoodAvailable = true;
};

std::string ExplainModelStatistic(
    const ModelStatisticExplanationContext &context);
ModelStatisticExplanationContext GeneralizedModelStatisticContext(
    const GeneralizedGLMState &state, const std::string &statistic, int row = -1);

struct GroupModelState : ModelSpecification {
    // Stable identity of this editor/result.  `group` remains the dataset ID;
    // several linear models may therefore refer to the same dataset.
    std::string modelId;
    std::string group;
    // Single general-linear-model windows use the same explicit refit policy
    // as the other regression editors.  Keep this in the canonical model
    // state (rather than in a native control) so toggling the checkbox has
    // real semantics and survives every refresh of the window.
    bool autoRefit = true;
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
// Keep the regression specification derived from the canonical dataset
// metadata, then apply any explicit plot-analysis interpretations. Numeric
// predictors normally use the model default, while an explicit numeric entry
// from a precomputed result is preserved.
bool SynchronizeGroupModelTermTypes(GroupModelState &state,
                                    const DataFrameModel &dataframe);
// Reconcile a family-neutral model specification with the canonical dataset
// storage types while preserving valid, explicit model-local interpretations.
// This is shared by ordinary and generalized linear models so their fit
// queues cannot serialize stale predictor types after analysis startup.
bool SynchronizeModelSpecificationTermTypes(ModelSpecification &specification,
                                             const DataFrameModel &dataframe);
ModelPredictorMetadata PredictorMetadataForLinearModel(
    const PlotModel &model,
    const DataFrameModel *dataframe,
    const std::string &variable);
// Dataset metadata used by the shared model-specification editor.  This is
// family-neutral: both ordinary and generalized linear models must apply the
// same validation when a predictor is reinterpreted as numeric or factor.
ModelPredictorMetadata PredictorMetadataForModelSpecification(
    const PlotModel &model,
    const DataFrameModel *dataframe,
    const std::string &variable);
bool LinearGLMFitMatchesTermTypes(const GLMFitSummary &fit,
                                  const std::vector<std::string> &terms,
                                  const std::map<std::string, std::string> &termTypes);
// A cached fit may be rendered only when it belongs to the exact current
// immutable specification.  In particular, a successful result for the
// previous predictor list must never remain visible while a replacement is
// pending; doing so mixes coefficient rows from one specification with the
// controls and term registry of another.
bool LinearGLMFitMayBePresented(const GroupModelState &state,
                                const std::string &currentSignature,
                                const GLMFitSummary *fit);
GroupModelState &EnsureGroupModelState(std::map<std::string, GroupModelState> &states,
                                       const std::string &group,
                                       const PlotModel *seed = nullptr);
std::string CreateLinearModelInstance(std::map<std::string, GroupModelState> &states,
                                      const std::string &datasetGroup);

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

enum class GLMInteractionSectionInference {
    EstimateOnly,
    ReferenceTest,
    Test,
    AdjustedComparison
};

struct GLMInteractionReportSection {
    // Keep pair-like member names for source compatibility with older native
    // renderers while attaching explicit inferential semantics.
    std::string first;
    std::vector<LinearFunctionEstimate> second;
    GLMInteractionSectionInference inference =
        GLMInteractionSectionInference::Test;
    // Identity columns remain separate from the numerical result columns so
    // factor-by-factor EMMs and conditioned comparisons are unambiguous in
    // native tables, copied text, and exports.  Empty vectors retain the
    // historical single `Effect` column backed by LinearFunctionEstimate::label.
    std::vector<std::string> identityHeaders;
    std::vector<std::vector<std::string>> identityRows;
    // Some R-backed post-estimation tables (notably Rubin-pooled interaction
    // contrasts) have row-specific degrees of freedom.  Keep this as an
    // explicit semantic column instead of flattening it into an identity
    // string or dropping it in one of the native renderers.
    bool showDegreesOfFreedom = false;
    std::string estimateHeader = "Estimate";
    std::string lowerHeader = "Lower 95%";
    std::string upperHeader = "Upper 95%";
};

struct GLMInteractionReport {
    bool ok = false;
    std::string term;
    std::string kind;
    std::string interpretation;
    std::string message;
    std::string multipleComparisons;
    std::string poolingMethod;
    int imputationCount = 1;
    bool emmReferenceTestEnabled = false;
    double emmTestReference = std::numeric_limits<double>::quiet_NaN();
    double confidenceLevel = 0.95;
    bool binaryProbability = false;
    bool boundedCount = false;
    bool ceilingHurdle = false;
    std::string trialsVariable;
    double trialsConstant = std::numeric_limits<double>::quiet_NaN();
    std::string eventLabel;
    std::string quantity = "predicted_probability";
    std::string adjustmentMode = "average_sample";
    std::string presentation = "percentage";
    std::vector<GLMInteractionReportSection> sections;
    // Preserve explanatory lines emitted by the R post-estimation layer. They
    // carry the exact contrast orientation, scale, and pooling semantics that
    // must remain visible after Windows parses the text protocol.
    std::vector<std::string> notes;
    AnalysisProvenance provenance;
};

ModelStatisticExplanationContext InteractionReportStatisticContext(
    const GLMInteractionReport &report,
    const GLMInteractionReportSection &section,
    const std::string &statistic);

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
    AnalysisProvenance provenance;
    // R-provided dual-scale presentation, optional for legacy payloads.
    GLMInteractionReport scaleReport;
    std::vector<std::string> scaleHeaders;
    std::vector<std::vector<std::string>> scaleRows;
};
PublicationTableSpec PairwiseScalePublicationTable(const GLMPairwiseComparisonResult &result);
std::vector<PublicationTableSpec> InteractionScalePublicationTables(const GLMInteractionReport &report);
OutputCodeReference PairwiseDiagnosticsCodeReference(
    const std::string &outputId, const GLMPairwiseComparisonResult &result);

// Regression-comparison state is deliberately typed. Inclusion controls,
// statistical values and fit lifecycle are separate concepts; native views
// must never infer one of them from a rendered dash.
enum class RegressionComparisonFitState {
    NotFitted,
    Pending,
    Valid,
    Error
};

enum class RegressionComparisonCellDisplayState {
    Value,
    Blank,
    NotApplicable
};

struct RegressionComparisonTermCellState {
    bool termIncluded = false;
    bool inclusionControlAvailable = false;
    bool factorParent = false;
    bool factorLevel = false;
    double coefficientValue = std::numeric_limits<double>::quiet_NaN();
    RegressionComparisonCellDisplayState displayState =
        RegressionComparisonCellDisplayState::Blank;
    std::string displayText;
};

struct RegressionComparisonFitCellState {
    RegressionComparisonCellDisplayState displayState =
        RegressionComparisonCellDisplayState::Blank;
    std::string displayText;
};

struct RegressionComparisonModel : ModelSpecification {
    std::string id;
    std::string label;
    // Presentation-only terms that were explicitly offered for this column.
    // They remain visible with an inclusion control after exclusion, but are
    // never part of the fitted ModelSpecification or its fingerprint.
    std::vector<std::string> candidateTerms;
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
    RegressionComparisonFitState fitState = RegressionComparisonFitState::NotFitted;
};

struct RegressionComparisonState {
    std::string id;
    std::string group;
    std::string response;
    std::string scope = "all";
    AnalysisScope dataScope;
    bool dataScopeCaptured = false;
    std::string frozenScopeNotice;
    bool autoRefit = true;
    bool showInformationCriteria = false;
    bool precomputed = false;
    bool multipleImputation = false;
    int imputationCount = 0;
    // Canonical dataset identity captured from DataFrameModel.  The group
    // name alone is not sufficient for MI because the registered completed
    // dataset set can be replaced while retaining the same group.
    std::string datasetType = "data_frame";
    std::string imputationSetId;
    std::string sourceDatasetId;
    std::string title;
    std::string note;
    std::vector<std::string> termRows;
    std::map<std::string, std::string> termTypes;
    std::vector<RegressionComparisonModel> models;
    int activeModel = 0;
    bool rFitPending = false;
    std::string lastRFitSignature;
    // Monotonic request token shared with the R backend.  The fitted result
    // must echo this value so a late response cannot replace a newer model.
    int rFitGeneration = 0;
    PlotModel seed;
    bool hasSeed = false;
    AnalysisProvenance provenance;
};

// Multiple-imputation is a property of the dataset, but Rubin pooling is a
// property of the exact analysis inputs.  A comparison whose response and all
// predictor values are observed does not acquire between-imputation
// uncertainty merely because unrelated columns were imputed.
bool RegressionComparisonUsesImputedModelInputs(
    const RegressionComparisonState &state,
    const DataFrameModel &dataframe);
std::string RegressionComparisonMultipleImputationNote(
    const RegressionComparisonState &state,
    const DataFrameModel *dataframe);

// Keep comparison predictors aligned with the canonical dataset metadata.
// Factor predictors remain valid model terms even though they are not stored
// in PlotModel::variables (which is intentionally numeric-only).
bool SynchronizeRegressionComparisonTermTypes(RegressionComparisonState &state,
                                              const DataFrameModel &dataframe);
bool SynchronizeRegressionComparisonDatasetIdentity(
    RegressionComparisonState &state,
    const DataFrameModel &dataframe);
std::map<std::string, std::string> RegressionComparisonModelTermTypes(
    const RegressionComparisonState &state,
    const RegressionComparisonModel &model);
std::string RegressionComparisonModelTermType(const RegressionComparisonState &state,
                                              int modelIndex,
                                              const std::string &term);
std::string RegressionComparisonEffectiveResponse(const RegressionComparisonState &state,
                                                  const RegressionComparisonModel &model);
// Materialize a self-contained semantic specification for one comparison
// column.  Platform views and fit queues use this instead of reconstructing a
// GLM from presentation state.
ModelSpecification EffectiveRegressionComparisonModelSpecification(
    const RegressionComparisonState &state,
    const RegressionComparisonModel &model);
RegressionComparisonFitState RegressionComparisonResolvedFitState(
    const RegressionComparisonState &state,
    const RegressionComparisonModel &model);

struct GeneralizedGLMState : ModelSpecification {
    GeneralizedGLMState()
    {
        familyKind = ModelFamilyKind::GeneralizedLinear;
    }
    std::string id;
    std::string group;
    StatisticalModelType modelType = StatisticalModelType::LegacyGeneralized;
    std::string family = "binomial";
    std::string link = "logit";
    bool responseBoundsConfigured = false;
    double responseLower = NAN;
    double responseUpper = NAN;
    bool binaryRegression = false;
    bool countRegression = false;
    CountDistribution countDistribution = CountDistribution::Poisson;
    std::string exposure;
    std::string offsetVariable;
    std::string trialsVariable;
    double trialsConstant = NAN;
    bool likelihoodAvailable = true;
    bool likelihoodRatioAvailable = true;
    double theta = NAN;
    double thetaDescriptiveMean = NAN;
    double thetaDescriptiveMin = NAN;
    double thetaDescriptiveMax = NAN;
    double perfectScoreCount = NAN;
    double nonPerfectScoreCount = NAN;
    double observedCeilingProportion = NAN;
    double predictedCeilingProportion = NAN;
    double predictedCeilingCount = NAN;
    bool observedCeilingCountConstant = true;
    std::string perfectScoreComponentStatus;
    std::string belowCeilingComponentStatus;
    int hurdleSuccessfulImputations = 0;
    int hurdleAttemptedImputations = 0;
    double betaBinomialDispersion = NAN;
    double betaBinomialDispersionMin = NAN;
    double betaBinomialDispersionMax = NAN;
    BinaryLink binaryLink = BinaryLink::Logit;
    BinaryResponseCoding responseCoding;
    bool responseCodingExplicit = false;
    bool autoRefit = true;
    bool precomputed = false;
    bool multipleImputation = false;
    int imputationCount = 0;
    std::string datasetType = "data_frame";
    std::string imputationSetId;
    std::string sourceDatasetId;
    std::string title;
    std::string note;
    DiagnosticOptions diagnosticOptions;
    int modelVersion = 0;
    int fitVersion = 0;
    int diagnosticsVersion = 0;
    bool ok = false;
    bool rFitPending = false;
    std::string lastRFitSignature;
    // Monotonic request identity echoed by R.  A result is current only when
    // this generation is still the pending generation for the model.
    std::uint64_t rFitGeneration = 0;
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
    std::map<std::string, double> familyDiagnostics;
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
    std::vector<GlobalTermTestRow> termTests;
    std::vector<BinaryPairwiseRow> pairwise;
    std::vector<GeneralizedGLMDiagnosticRow> diagnostics;
    // One diagnostic table per completed-data fit in the mice::mira object.
    // This is presentation data for choosing an imputation, not a pooled fit.
    std::vector<std::vector<GeneralizedGLMDiagnosticRow>> diagnosticsByImputation;
    // Descriptive summaries computed in R for each concrete fitted imputation.
    std::vector<std::string> diagnosticNotesByImputation;
    // Frequency distributions are computed in R from each model's actual PMF.
    // The aggregate is descriptive across imputations; it is not Rubin-pooled.
    std::vector<BoundedCountDistributionPoint> boundedCountDistribution;
    std::vector<std::vector<BoundedCountDistributionPoint>>
        boundedCountDistributionsByImputation;
    std::vector<int> rowsUsed;
    std::vector<int> rowsExcluded;
    PlotModel seed;
    bool hasSeed = false;
};

// Editing a generalized-model specification while Auto-refit is disabled
// must update the visible term registry immediately without presenting the
// coefficient rows from the previous fit as though they belonged to the new
// specification.
bool GeneralizedGLMHasPendingManualFit(const GeneralizedGLMState &state);
void ClearGeneralizedGLMFitResults(GeneralizedGLMState &state);
std::string GeneralizedGLMPendingManualFitStatus();
std::vector<GeneralizedGLMRow> GeneralizedGLMRowsForPresentation(
    const GeneralizedGLMState &state);

// Builds a new, independently-owned analysis session. Dataset roles may seed
// response/predictor choices, but model-local transformations are copied only
// when an explicit saved specification is supplied.
GeneralizedGLMState CreateIndependentGeneralizedGLMSession(
    const std::string &id,
    const std::string &group,
    const PlotModel &seed,
    const DataFrameModel &dataframe,
    SharedAnalysisKind analysisKind,
    const AnalysisVariableRoles &roles,
    const AnalysisScope &dataScope,
    const std::string &scope,
    const ModelSpecification *savedSpecification = nullptr);

// Resolve dataset-derived predictor types into the owned model specification
// immediately before a generalized-model request is fingerprinted and sent to
// R. Explicit model-local overrides remain authoritative. This request-boundary
// canonicalization prevents a UI from displaying a dataset-derived factor type
// while serializing an absent type as the numeric fallback.
bool SynchronizeGeneralizedGLMFitSpecification(
    GeneralizedGLMState &state,
    const DataFrameModel &dataframe);

struct GeneralizedComparisonModel : ModelSpecification {
    GeneralizedComparisonModel()
    {
        familyKind = ModelFamilyKind::GeneralizedLinear;
    }
    std::string id;
    std::string label;
    // Presentation-only terms that were explicitly offered for this column.
    // They remain visible with an inclusion control after exclusion, but are
    // never part of the fitted ModelSpecification or its fingerprint.
    std::vector<std::string> candidateTerms;
    std::string family = "binomial";
    std::string link = "logit";
    bool responseBoundsConfigured = false;
    double responseLower = NAN;
    double responseUpper = NAN;
    CountDistribution countDistribution = CountDistribution::Poisson;
    std::string exposure;
    std::string offsetVariable;
    std::string trialsVariable;
    double trialsConstant = NAN;
    // Adding a higher-order interaction materializes the lower-order terms
    // required by hierarchy.  Keep edit provenance outside the immutable
    // specification identity so removing that interaction can undo only the
    // terms introduced by that edit and preserve terms that already existed.
    std::map<std::string, std::vector<std::string>> interactionImpliedTerms;
    GeneralizedModelCapabilities capabilities;
    DiagnosticOptions diagnosticOptions;
    GeneralizedGLMState fit;
    int modelVersion = 0;
    int fitVersion = 0;
    // The request/result identity is deliberately stored per column.  A
    // comparison-wide generation rejects late batches; these fields also
    // prevent an internally mixed batch from attaching one column's fit to a
    // different current ModelSpecification.
    int requestedSpecificationRevision = 0;
    std::string requestedSpecificationFingerprint;
    int fitSpecificationRevision = 0;
    std::string fitSpecificationFingerprint;
    bool isStale = true;
    bool comparisonOk = false;
    bool comparisonCalculatedInR = false;
    int comparisonDf = 0;
    double comparisonDf2 = NAN;
    double comparisonDelta = NAN;
    double comparisonStatistic = NAN;
    double comparisonP = NAN;
    std::string comparisonMethod;
    RegressionComparisonFitState fitState = RegressionComparisonFitState::NotFitted;
};

struct GeneralizedComparisonState {
    std::string id;
    std::string group;
    StatisticalModelType modelType = StatisticalModelType::LegacyGeneralized;
    std::string response;
    std::string family = "binomial";
    std::string link = "logit";
    std::string scope = "all";
    AnalysisScope dataScope;
    bool dataScopeCaptured = false;
    std::string frozenScopeNotice;
    bool autoRefit = true;
    bool showInformationCriteria = false;
    bool binaryComparison = false;
    bool countComparison = false;
    bool multipleImputation = false;
    int imputationCount = 0;
    std::string datasetType = "data_frame";
    std::string imputationSetId;
    std::string sourceDatasetId;
    BinaryLink binaryLink = BinaryLink::Logit;
    BinaryResponseCoding responseCoding;
    std::vector<int> commonRows;
    // Derived presentation snapshot only.  ModelSpecification::terms is the
    // sole editable term registry.  This cache is rebuilt from the current
    // per-column specifications and identity-matched fits before presentation.
    std::vector<std::string> termRows;
    std::string presentationFingerprint;
    std::map<std::string, std::string> termTypes;
    std::vector<GeneralizedComparisonModel> models;
    int activeModel = 0;
    bool rFitPending = false;
    std::string lastRFitSignature;
    int rFitGeneration = 0;
    PlotModel seed;
    bool hasSeed = false;
    AnalysisProvenance provenance;
};

std::string LinearGLMFitSignature(const std::string &dependent,
                                  const std::vector<std::string> &terms,
                                  const std::map<std::string, std::string> &termTypes,
                                  const std::string &scope,
                                  const std::set<std::string> &centeredPredictors = {},
                                  const std::map<std::string, std::string> &factorReferenceLevels = {});
// Selected/unselected fits are identified by the exact immutable row ids that
// generated them.  Keep this encoding shared by both native front ends and by
// pooled-result adoption so a result can never cross selection revisions.
std::string LinearGLMFitIdentityWithSelection(
    const std::string &baseSignature,
    const std::string &scope,
    const std::vector<int> &selectionRows);
std::string GeneralizedGLMFitSignature(const GeneralizedGLMState &state);
std::string RegressionComparisonFitSignature(const RegressionComparisonState &state);
std::string GeneralizedComparisonModelSpecificationFingerprint(
    const GeneralizedComparisonState &state,
    const GeneralizedComparisonModel &model);
std::string GeneralizedComparisonFitSignature(const GeneralizedComparisonState &state);

// A specification edit invalidates the entire prior fit atomically.  These
// helpers also cancel the comparison-wide pending request so any late batch
// is rejected before it can be attached to the edited model.
void InvalidateRegressionComparisonModel(RegressionComparisonState &state,
                                         RegressionComparisonModel &model);
void InvalidateGeneralizedComparisonModel(GeneralizedComparisonState &state,
                                          GeneralizedComparisonModel &model);
bool SynchronizeGeneralizedComparisonDatasetIdentity(
    GeneralizedComparisonState &state,
    const DataFrameModel &dataframe);

std::string EncodePooledRegressionComparisonModelSpec(const RegressionComparisonState &state);

bool HasRegressionTerm(const RegressionComparisonState &state, const std::string &term);
bool ModelIncludesTerm(const RegressionComparisonModel &model, const std::string &term);
void AddRegressionTermRow(RegressionComparisonState &state, const std::string &term);
// Shared regression-comparison editing semantics. Platform views choose the
// target and render the result; they do not redefine what +/- means.
bool SetRegressionComparisonActiveModel(RegressionComparisonState &state, int modelIndex);
bool IncludeRegressionComparisonTerm(RegressionComparisonState &state,
                                     int modelIndex,
                                     const std::string &term,
                                     const std::vector<std::string> &availableVariables);
bool ExcludeRegressionComparisonTerm(RegressionComparisonState &state,
                                     int modelIndex,
                                     const std::string &term);
bool ToggleRegressionComparisonTerm(RegressionComparisonState &state,
                                    int modelIndex,
                                    const std::string &term,
                                    const std::vector<std::string> &availableVariables);
bool IncludeRegressionComparisonTermInAllModels(
    RegressionComparisonState &state,
    const std::string &term,
    const std::vector<std::string> &availableVariables);
bool ExcludeRegressionComparisonTermFromAllModels(RegressionComparisonState &state,
                                                  const std::string &term);
bool RemoveRegressionComparisonTermCompletely(RegressionComparisonState &state,
                                              const std::string &term);
bool ReplaceRegressionComparisonTerm(RegressionComparisonState &state,
                                     int modelIndex,
                                     const std::string &term,
                                     const std::string &replacement,
                                     const std::vector<std::string> &availableVariables);
int AddRegressionComparisonModel(RegressionComparisonState &state,
                                 int sourceModelIndex,
                                 bool emptyModel = false);
// Create a comparison from the exact semantic specification of a fitted
// single model.  This is the shared bridge used by macOS and Windows; views
// must not reconstruct the model from rendered rows or formula text.
RegressionComparisonState CreateRegressionComparisonFromSingleModel(
    const std::string &comparisonId,
    const GroupModelState &source,
    const GLMFitSummary *currentFit = nullptr,
    const PlotModel *seed = nullptr,
    const std::string &modelLabel = "Model 1");
bool DeleteRegressionComparisonModel(RegressionComparisonState &state, int modelIndex);
RegressionComparisonModel *RegressionComparisonModelById(RegressionComparisonState &state,
                                                         const std::string &modelId);
const RegressionComparisonModel *RegressionComparisonModelById(
    const RegressionComparisonState &state,
    const std::string &modelId);
std::vector<std::string> RegressionComparisonModelIds(const RegressionComparisonState &state);
std::vector<std::string> RegressionComparisonModelLabels(const RegressionComparisonState &state);
std::string NextRegressionModelId(const RegressionComparisonState &state);
std::string NextUntitledRegressionLabel(const RegressionComparisonState &state);
std::string UniqueRegressionCopyLabel(const RegressionComparisonState &state, const std::string &label);

bool HasGeneralizedComparisonTerm(const GeneralizedComparisonState &state, const std::string &term);
bool GeneralizedModelIncludesTerm(const GeneralizedComparisonModel &model, const std::string &term);
void AddGeneralizedComparisonTermRow(GeneralizedComparisonState &state, const std::string &term);
std::map<std::string, std::string> GeneralizedComparisonModelTermTypes(
    const GeneralizedComparisonState &state,
    const GeneralizedComparisonModel &model);
std::string GeneralizedComparisonModelTermType(const GeneralizedComparisonState &state,
                                               int modelIndex,
                                               const std::string &term);
ModelSpecification EffectiveGeneralizedComparisonModelSpecification(
    const GeneralizedComparisonState &state,
    const GeneralizedComparisonModel &model);
RegressionComparisonFitState GeneralizedComparisonResolvedFitState(
    const GeneralizedComparisonState &state,
    const GeneralizedComparisonModel &model);
bool SetGeneralizedComparisonActiveModel(GeneralizedComparisonState &state, int modelIndex);
bool IncludeGeneralizedComparisonTerm(GeneralizedComparisonState &state,
                                      int modelIndex,
                                      const std::string &term,
                                      const std::vector<std::string> &availableVariables);
bool ExcludeGeneralizedComparisonTerm(GeneralizedComparisonState &state,
                                      int modelIndex,
                                      const std::string &term);
bool ToggleGeneralizedComparisonTerm(GeneralizedComparisonState &state,
                                     int modelIndex,
                                     const std::string &term,
                                     const std::vector<std::string> &availableVariables);
bool ReplaceGeneralizedComparisonTerm(GeneralizedComparisonState &state,
                                      int modelIndex,
                                      const std::string &term,
                                      const std::string &replacement,
                                      const std::vector<std::string> &availableVariables);
bool IncludeGeneralizedComparisonTermInAllModels(
    GeneralizedComparisonState &state,
    const std::string &term,
    const std::vector<std::string> &availableVariables);
bool ExcludeGeneralizedComparisonTermFromAllModels(GeneralizedComparisonState &state,
                                                   const std::string &term);
bool RemoveGeneralizedComparisonTermCompletely(GeneralizedComparisonState &state,
                                               const std::string &term);
int AddGeneralizedComparisonModel(GeneralizedComparisonState &state,
                                  int sourceModelIndex,
                                  bool emptyModel = false);
GeneralizedComparisonState CreateGeneralizedComparisonFromSingleModel(
    const std::string &comparisonId,
    const GeneralizedGLMState &source,
    const std::string &modelLabel = "Model 1");
// Materialize one comparison column as an independently-owned single-model
// session. This inverse conversion preserves the exact semantic specification.
GeneralizedGLMState CreateSingleModelFromGeneralizedComparison(
    const GeneralizedComparisonState &state,
    int modelIndex,
    const std::string &singleId = "");
bool DeleteGeneralizedComparisonModel(GeneralizedComparisonState &state, int modelIndex);
bool ApplyGeneralizedComparisonTermType(GeneralizedComparisonState &state,
                                        int modelIndex,
                                        const std::string &term,
                                        const std::string &type,
                                        const ModelPredictorMetadata &metadata);
bool ApplyGeneralizedComparisonReferenceLevel(GeneralizedComparisonState &state,
                                              int modelIndex,
                                              const std::string &term,
                                              const std::string &referenceLevel,
                                              const std::vector<std::string> &availableLevels);
bool ToggleGeneralizedComparisonPredictorCentering(GeneralizedComparisonState &state,
                                                   int modelIndex,
                                                   const std::string &term);
// Family and link are comparison-wide parts of the immutable model
// specification.  Platform controls must use these setters so every column
// is invalidated atomically before a new R/MI fit is requested.
bool SetGeneralizedComparisonFamily(GeneralizedComparisonState &state,
                                    const std::string &family);
bool SetGeneralizedComparisonLink(GeneralizedComparisonState &state,
                                  const std::string &link);
bool SetGeneralizedComparisonCountDistribution(GeneralizedComparisonState &state,
                                               int modelIndex,
                                               CountDistribution distribution);
bool SetGeneralizedComparisonExposure(GeneralizedComparisonState &state,
                                      int modelIndex,
                                      const std::string &exposure);
bool SetGeneralizedComparisonOffset(GeneralizedComparisonState &state,
                                    int modelIndex,
                                    const std::string &offsetVariable);
bool SetGeneralizedComparisonTrials(GeneralizedComparisonState &state,
                                    int modelIndex,
                                    const std::string &trialsVariable,
                                    double trialsConstant);
bool SetGeneralizedComparisonModelScope(GeneralizedComparisonState &state,
                                        int modelIndex,
                                        const std::string &scope);
GeneralizedModelCapabilities GeneralizedComparisonModelCapabilities(
    const GeneralizedComparisonState &state,
    const GeneralizedComparisonModel &model);
bool GeneralizedComparisonModelsSupportLikelihoodRatio(
    const GeneralizedComparisonState &state,
    const GeneralizedComparisonModel &previous,
    const GeneralizedComparisonModel &current,
    std::string *reason = nullptr);
GeneralizedComparisonModel *GeneralizedComparisonModelById(GeneralizedComparisonState &state,
                                                           const std::string &modelId);
const GeneralizedComparisonModel *GeneralizedComparisonModelById(
    const GeneralizedComparisonState &state,
    const std::string &modelId);
std::vector<std::string> GeneralizedComparisonModelIds(const GeneralizedComparisonState &state);
std::vector<std::string> GeneralizedComparisonModelLabels(const GeneralizedComparisonState &state);
std::string NextGeneralizedComparisonModelId(const GeneralizedComparisonState &state);
std::string NextUntitledGeneralizedComparisonLabel(const GeneralizedComparisonState &state);
std::string UniqueGeneralizedComparisonCopyLabelForState(const GeneralizedComparisonState &state, const std::string &label);

OutputCodeReference RegressionInteractionReportCodeReference(
    const std::string &outputId, const GLMInteractionReport &report,
    const OutputCodeReference &sourceModel);
std::vector<std::string> GLMInteractionGroupingFactors(
    const GLMInteractionReport &report,
    const std::map<std::string, std::string> &fittedTermTypes = {});
std::string GLMInteractionReportText(const GLMInteractionReport &report);
bool ParseGLMInteractionReportText(const std::vector<std::string> &lines,
                                   GLMInteractionReport &report);

GLMFitSummary FitMultipleLinearModel(const PlotModel &model,
                                     const DataFrameModel *dataframe,
                                     const std::set<int> &selectedRows,
                                     const std::string &dependent,
                                     const std::vector<std::string> &terms,
                                     const std::map<std::string, std::string> &termTypes,
                                     const std::string &scope,
                                     const std::set<std::string> &centeredPredictors = {},
                                     const std::map<std::string, std::string> &factorReferenceLevels = {});
GLMFitSummary FitMultipleLinearModel(const PlotModel &model,
                                     const DataFrameModel *dataframe,
                                     const std::set<int> &selectedRows,
                                     const ModelSpecification &specification);
GLMInteractionReport BuildGLMInteractionReport(const PlotModel &model,
                                               const DataFrameModel *dataframe,
                                               const std::string &term,
                                               const std::vector<std::string> &terms,
                                               const std::map<std::string, std::string> &termTypes,
                                               const GLMFitSummary &fit,
                                               double confidenceLevel = 0.95,
                                               const std::set<std::string> &centeredPredictors = {},
                                               double emmTestReference =
                                                   std::numeric_limits<double>::quiet_NaN());
bool PopulateGLMInteractionPlotModel(PlotModel &plot,
                                     const PlotModel &seed,
                                     const DataFrameModel *dataframe,
                                     const std::string &dependent,
                                     const std::string &term,
                                     const std::vector<std::string> &terms,
                                     const std::map<std::string, std::string> &termTypes,
                                     const GLMFitSummary &fit,
                                     std::string *message = nullptr,
                                     const std::set<std::string> &centeredPredictors = {});
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

struct RegressionInteractionPlotResult {
    bool ok = false;
    std::string message;
    std::string term;
    std::string interactionType;
    std::string focal;
    std::string moderator;
    // Variables that define each plotted series. For a two-way interaction
    // this contains the moderator; for a three-way interaction it contains
    // both conditioning variables in specification order.
    std::vector<std::string> conditioning;
    int imputationCount = 1;
    double confidenceLevel = 0.95;
    // Statistical values are computed in R. These fields only preserve the
    // requested binary-effect semantics for native presentation and export.
    bool binaryProbability = false;
    bool boundedCount = false;
    bool ceilingHurdle = false;
    std::string trialsVariable;
    double trialsConstant = std::numeric_limits<double>::quiet_NaN();
    std::string eventLabel;
    std::string quantity = "predicted_probability";
    std::string adjustmentMode = "average_sample";
    std::string presentation = "percentage";
    std::vector<std::pair<double, std::string>> xTicks;
    std::vector<std::vector<int>> xTickCaseIds;
    std::vector<InteractionPlotLine> lines;
    AnalysisProvenance provenance;
};

struct RegressionPartialPlotResult {
    bool ok = false;
    std::string message;
    std::string term;
    std::string residualType;
    std::string contributionScale;
    std::vector<std::string> availableTerms;
    std::vector<std::string> availableResidualTypes;
    AnalysisProvenance provenance;
    int imputationCount = 1;
    std::vector<std::vector<DiagnosticPlotPoint>> pointsByImputation;
};

OutputCodeReference RegressionPartialPlotCodeReference(
    const PlotModel &plot, const RegressionPartialPlotResult &result,
    const OutputCodeReference &sourceModel);

bool ParseRegressionPartialPlotRResult(const std::vector<std::string> &lines,
                                       RegressionPartialPlotResult &result);
DiagnosticPlotData BuildRegressionPartialPlotData(const RegressionPartialPlotResult &result,
                                                   int imputationIndex = 1,
                                                   bool imputationUncertainty = false);

bool ParseRegressionInteractionPlotRResult(
    const std::vector<std::string> &lines,
    RegressionInteractionPlotResult &result);
void ApplyRegressionInteractionPresentation(
    PlotModel &plot,
    const RegressionInteractionPlotResult &result,
    const std::string &responseLabel);
void ApplyRegressionInteractionVariableLabels(
    PlotModel &plot,
    const RegressionInteractionPlotResult &result,
    const DataFrameModel &dataframe,
    const std::string &responseVariable);
std::string RegressionEffectPlotTitle(
    const RegressionInteractionPlotResult &result);
void ApplyRegressionInteractionAxisRange(PlotModel &plot);
OutputCodeReference RegressionInteractionPlotCodeReference(
    const PlotModel &plot,
    const RegressionInteractionPlotResult &result,
    const OutputCodeReference &sourceModel);

// Keep user-facing R graph exports synchronized with display-only choices made
// after the R-backed estimates and confidence limits were calculated.
void SynchronizeRegressionPlotCodeReferenceDisplay(PlotModel &plot);
void PopulateRegressionInteractionLegendRows(
    RegressionInteractionPlotResult &result,
    const DataFrameModel &dataframe);

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
bool GeneralizedComparisonPresentationIsConsistent(
    const GeneralizedComparisonState &state,
    std::string *reason = nullptr);
std::string GeneralizedComparisonConsistencyTrace(
    const GeneralizedGLMState &source,
    const GeneralizedComparisonState &state,
    int modelIndex = 0);
const GeneralizedGLMRow *DisplayRowForGeneralizedComparisonTerm(const GeneralizedComparisonState &state,
                                                                const std::string &term);
std::string GeneralizedComparisonDisplayLabel(const GeneralizedComparisonState &state,
                                              const std::string &term);
std::string GeneralizedComparisonSourceTerm(const GeneralizedComparisonState &state,
                                            const std::string &displayTerm);
RegressionComparisonTermCellState GeneralizedComparisonTermCell(
    const GeneralizedComparisonState &state,
    int modelIndex,
    const std::string &term);
RegressionComparisonFitCellState GeneralizedComparisonFitCell(
    const GeneralizedComparisonState &state,
    int modelIndex,
    int fitRow);

void RefreshRegressionTermRowsFromFits(RegressionComparisonState &state);
void PruneRegressionComparisonRowsForCurrentTypes(RegressionComparisonState &state);
void InferRegressionComparisonTermTypesFromFitRows(RegressionComparisonState &state);
void EnsureRegressionComparisonFactorLevelRows(RegressionComparisonState &state,
                                               const std::string &term);
bool ApplyRegressionComparisonTermType(RegressionComparisonState &state,
                                      int modelIndex,
                                      const std::string &term,
                                      const std::string &type,
                                      std::string *message = nullptr);
bool ToggleRegressionComparisonPredictorCentering(RegressionComparisonState &state,
                                                  int modelIndex,
                                                  const std::string &term);
const GLMCoefficientRow *DisplayRowForComparisonTerm(const RegressionComparisonState &state,
                                                     const std::string &term);
std::string RegressionComparisonDisplayLabel(const RegressionComparisonState &state,
                                             const std::string &term);
std::string RegressionComparisonSourceTerm(const RegressionComparisonState &state,
                                            const std::string &term);
std::string RegressionComparisonTermDisplay(const RegressionComparisonModel &model,
                                            const std::string &term);
RegressionComparisonTermCellState RegressionComparisonTermCell(
    const RegressionComparisonState &state,
    int modelIndex,
    const std::string &term);
RegressionComparisonFitCellState RegressionComparisonFitCell(
    const RegressionComparisonState &state,
    int modelIndex,
    int fitRow);
ComparisonCopyTable BuildRegressionComparisonCopyTable(
    const RegressionComparisonState &state);


std::string DefaultGeneralizedFamilyForResponse(const PlotModel &model, const std::string &response);

std::set<int> GLMRowsUsedSet(const GLMFitSummary &fit);

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_GLM_MODEL_H
