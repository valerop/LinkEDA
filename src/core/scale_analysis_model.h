#ifndef RLISPSTAT_CORE_SCALE_ANALYSIS_MODEL_H
#define RLISPSTAT_CORE_SCALE_ANALYSIS_MODEL_H

#include "dataset_model.h"
#include "correlation_model.h"
#include "dimensionality_model.h"
#include "plot_geometry.h"

#include <cstddef>
#include <limits>
#include <map>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

enum class ScaleItemType {
    Numeric,
    Ordinal
};

// Scale Analysis is one analysis with several presentations.  Child windows
// never own an independent specification or result: their identity is tied to
// the exact immutable parent revision/fingerprint below.
enum class ScaleAnalysisChildKind {
    Reliability,
    InterItemCorrelations,
    Dimensionality,
    ScaleScores,
    ItemRestPlot,
    ReliabilityIfDeletedPlot,
    ItemDistributionsPlot,
    ScaleScoreDistributionPlot,
    CorrelationHeatmap,
    ScreePlot,
    LoadingPlot,
    Biplot
};

struct ScaleAnalysisDerivedIdentity {
    std::string analysisId;
    ScaleAnalysisChildKind kind = ScaleAnalysisChildKind::Reliability;
    int sourceRevision = -1;
    std::string sourceFingerprint;
};

struct ScaleFactorCountResultRow {
    int factorCount = 0;
    int imputations = 0;
    double proportion = 0.0;
};

struct ScaleEigenvalueResultRow {
    std::string component;
    std::string observed;
    std::string reference;
};

struct ScaleItemSpecification {
    std::string variable;
    ScaleItemType type = ScaleItemType::Numeric;
    bool reversed = false;
    bool hasScoringRange = false;
    double scoringMinimum = std::numeric_limits<double>::quiet_NaN();
    double scoringMaximum = std::numeric_limits<double>::quiet_NaN();
};

struct ScaleAnalysisSpecification {
    std::string datasetId;
    std::vector<ScaleItemSpecification> items;
    std::string correlationBasis = "auto";
    std::string scoreMethod = "mean";
    int minimumValidItems = 0;
    bool showOverview = true;
    bool showReliability = false;
    bool showItemAnalysis = true;
    bool showCorrelations = false;
    bool showDimensionality = false;
    bool showScoreDistribution = false;
    bool showItemPlots = false;
    bool showItemDistributions = false;
    bool showCorrelationHeatmap = false;
    bool showReliabilityStability = false;
    bool showScreePlot = false;
    bool showLoadingPlot = false;
    // Native Scale Analysis presents omega in the main summary, so new
    // analyses request it automatically.  With fewer than three items the R
    // backend reports it as unavailable without invalidating the scale.
    bool computeOmega = true;
    std::string dimensionalityMethod = "factor";
    std::string dimensionalityMissingMode = "pairwise";
    bool dimensionalityScale = true;
    int factorCount = 0;
    std::string extraction = "minres";
    std::string rotation = "oblimin";
    int parallelIterations = 20;
    int revision = 0;
    std::string fingerprint;
};

struct ScaleItemResultRow {
    std::string variable;
    std::string type;
    std::string direction;
    std::string mean;
    std::string sd;
    std::string missingPercent;
    std::string itemRestCorrelation;
    std::string alphaIfDeleted;
};

struct ScaleSummaryResult {
    std::string numberOfItems;
    std::string nUsed;
    std::string missingPercent;
    std::string scaleMean;
    std::string scaleSd;
    std::string meanInterItemCorrelation;
    std::string alpha;
    std::string standardizedAlpha;
    std::string omegaTotal;
    std::string reliabilityStatus;
};

struct ScaleReliabilityResultRow {
    std::string imputation;
    std::string alpha;
    std::string standardizedAlpha;
    std::string omegaTotal;
};

struct ScaleCorrelationResult {
    std::string basis;
    std::string status;
    std::vector<std::string> variables;
    std::vector<std::string> values;
    std::vector<std::string> pValues;
    std::vector<std::string> sampleSizes;
};

struct ScaleLoadingResultRow {
    std::string item;
    std::vector<std::string> loadings;
    std::string communality;
    std::string uniqueness;
};

struct ScaleDimensionalityResult {
    std::string status;
    std::string suggestedFactors;
    std::string factorCount;
    std::vector<std::string> factorNames;
    std::vector<ScaleLoadingResultRow> loadings;
};

struct ScaleScoreSummaryResult {
    std::string method;
    std::string status;
    std::string validN;
    std::string mean;
    std::string sd;
    std::string minimum;
    std::string maximum;
};

struct ScalePlotSeriesResult {
    std::string kind;
    std::string name;
    std::vector<std::string> labels;
    std::vector<std::string> values;
};

struct ScaleAnalysisResult {
    std::string backend;
    int imputationCount = 1;
    std::string summary;
    std::string provenance;
    std::vector<ScaleItemResultRow> items;
    ScaleSummaryResult scaleSummary;
    std::vector<ScaleReliabilityResultRow> reliabilityByImputation;
    ScaleCorrelationResult correlations;
    ScaleDimensionalityResult dimensionality;
    ScaleScoreSummaryResult scores;
    std::vector<ScalePlotSeriesResult> plotSeries;
    int revision = 0;
    std::string fingerprint;
};

// Under multiple imputation, rotated factor solutions are intentionally not
// averaged element by element.  This presentation selects one explicitly
// identified completed dataset while factor-count stability remains an
// across-imputation summary.
struct ScaleDimensionalityPresentation {
    int imputation = 1;
    std::vector<std::string> variables;
    std::vector<DimensionalityFitComponent> components;
    std::vector<DimensionalityFitLoading> loadings;
    std::vector<DimensionalityFitScore> scores;
    std::string status;
};

struct ScaleAnalysisState {
    std::string id;
    std::string group;
    ScaleAnalysisSpecification specification;
    ScaleAnalysisResult result;
    PlotModel seed;
    bool hasSeed = false;
    // The dataset-wide Analysis Scope is frozen when the Scale Analysis
    // session is created.  Item edits/refits must keep using these same rows
    // instead of silently falling back to the complete dataset.
    AnalysisScope dataScope;
    bool dataScopeCaptured = false;
    bool autoFit = true;
    bool rFitPending = false;
    int pendingRevision = -1;
    std::string pendingFingerprint;
    int modelVersion = 0;
    std::string status = "Choose at least two numeric or ordinal items.";
};

struct ScaleAnalysisWindowLayout {
    double preferredWidth = 860.0;
    double preferredHeight = 480.0;
    double itemRowHeight = 24.0;
    std::size_t visibleItemRows = 0;
    bool itemListScrolls = false;
};

std::string ScaleAnalysisWindowTitle(bool multipleImputation = false);
std::string ScaleAnalysisChildWindowTitle(
    ScaleAnalysisChildKind kind,
    bool multipleImputation = false);
std::string ScaleItemTypeToken(ScaleItemType type);
std::string ScaleItemTypeDisplayName(ScaleItemType type);
std::string ScaleItemTypeMenuLabel(ScaleItemType type, const DataColumn *column);
bool ParseScaleItemType(const std::string &token, ScaleItemType &type);
bool ScaleCorrelationBasisIsValid(const std::string &basis);
std::vector<std::pair<std::string, std::string>> ScaleAnalysisSummaryFields(const ScaleAnalysisResult &result);

enum class ScaleAnalysisGlobalPresentation { CompactTable, ItemTableNote };
enum class ScaleAnalysisDimensionalityPresentation { CombinedTable, SeparateTables };
std::vector<PublicationTableSpec> ScaleAnalysisPublicationTables(
    const ScaleAnalysisState &state, int kind = -1, int imputation = 1,
    ScaleAnalysisGlobalPresentation globalPresentation = ScaleAnalysisGlobalPresentation::CompactTable,
    ScaleAnalysisDimensionalityPresentation dimensionalityPresentation =
        ScaleAnalysisDimensionalityPresentation::CombinedTable);

bool BuildScaleNativePlotModel(const ScaleAnalysisState &state, ScaleAnalysisChildKind kind, int imputation, PlotModel &model);
std::string ScaleAnalysisPlotExplanation(ScaleAnalysisChildKind kind);

bool ScaleScoreMethodIsValid(const std::string &method);
bool ScaleExtractionIsValid(const std::string &method);
bool ScaleRotationIsValid(const std::string &rotation);

bool ScaleColumnIsDichotomousCategorical(const DataColumn &column);
bool ScaleColumnIsEligible(const DataColumn &column);
ScaleItemType DefaultScaleItemType(const DataColumn &column);
std::vector<std::string> EligibleScaleVariables(
    const DataFrameModel &dataframe,
    const std::vector<ScaleItemSpecification> &items = {});
std::vector<ScaleItemSpecification> InitialScaleItems(
    const DataFrameModel &dataframe,
    const std::map<std::string, std::string> &roles,
    const ScaleAnalysisSpecification *saved = nullptr);

std::string ScaleAnalysisSpecificationFingerprint(
    const ScaleAnalysisSpecification &specification);
bool ValidateScaleAnalysisSpecification(
    const DataFrameModel &dataframe,
    ScaleAnalysisSpecification &specification,
    std::string &error);
bool ScaleAnalysisResultMatchesSpecification(
    const ScaleAnalysisSpecification &specification,
    const ScaleAnalysisResult &result);
bool ScaleAnalysisHasCompletedResult(const ScaleAnalysisResult &result);
bool ScaleAnalysisResultMayBePresented(const ScaleAnalysisState &state);
ScaleAnalysisDerivedIdentity BuildScaleAnalysisDerivedIdentity(
    const ScaleAnalysisState &state,
    ScaleAnalysisChildKind kind);
bool ScaleAnalysisDerivedIdentityMatchesState(
    const ScaleAnalysisDerivedIdentity &identity,
    const ScaleAnalysisState &state);
std::vector<ScaleFactorCountResultRow> ScaleFactorCountRows(
    const ScaleAnalysisResult &result);
std::vector<ScaleEigenvalueResultRow> ScaleMeanEigenvalueRows(
    const ScaleAnalysisResult &result);
ScaleDimensionalityPresentation BuildScaleDimensionalityPresentation(
    const ScaleAnalysisResult &result,
    int requestedImputation = 1);
DimensionalityScreePlotState BuildScaleDimensionalityScreePlotState(
    const ScaleAnalysisResult &result,
    int requestedImputation = 1,
    const std::string &method = "factor");
DimensionalityBiplotPlotState BuildScaleDimensionalityBiplotPlotState(
    const ScaleAnalysisResult &result,
    int requestedImputation = 1,
    int requestedXComponent = 1,
    int requestedYComponent = 2,
    const std::string &method = "factor");

// Scale Analysis owns item preparation, correlation-basis selection, and MI
// coordination.  These adapters deliberately hand the resulting statistics to
// the same presentation contracts used by Correlation Matrix and
// Principal Components / Factor Analysis, so derived scale windows do not
// maintain a second table/interaction implementation.
std::vector<CorrelationCellResult> ScaleCorrelationCells(
    const ScaleAnalysisResult &result);
CorrelationMatrixRenderPlan BuildScaleCorrelationRenderPlan(
    const ScaleAnalysisResult &result,
    int selectedRow = -1,
    int selectedColumn = -1,
    bool showP = false,
    bool showPValue = false,
    bool showN = false);
DimensionalityReportViewModel BuildScaleDimensionalityReportViewModel(
    const ScaleAnalysisResult &result,
    int focusedComponent = 0,
    const std::string &focusedVariable = "",
    int requestedImputation = 1,
    const std::string &method = "factor");

ScaleAnalysisWindowLayout BuildScaleAnalysisWindowLayout(
    std::size_t itemCount,
    std::size_t maximumVisibleItems = 10);

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_SCALE_ANALYSIS_MODEL_H
