#include "../../src/core/glm_model.h"
#include "../../src/core/binary_regression_export.h"
#include "../../src/core/dataset_model.h"
#include "../../src/core/scatterplot_model.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <cmath>
#include <limits>
#include <map>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

using rlispstat::core::DefaultGeneralizedLink;
using rlispstat::core::DefaultGeneralizedFamilyForValues;
using rlispstat::core::BuildScenarioDesignVector;
using rlispstat::core::BuildGeneralizedDiagnosticPlotData;
using rlispstat::core::BuildDiagnosticPlotDataAcrossImputations;
using rlispstat::core::DiagnosticImputationUncertaintyShouldBeDefault;
using rlispstat::core::DiagnosticRequiresAcceptedMultipleImputationFit;
using rlispstat::core::BuildLinearDiagnosticPlotData;
using rlispstat::core::RowsWithImputedModelInputs;
using rlispstat::core::BuildNestedGeneralizedModelSummary;
using rlispstat::core::BuildNestedLinearModelSummary;
using rlispstat::core::BinaryLink;
using rlispstat::core::BinaryLinkId;
using rlispstat::core::BinaryLinkLabel;
using rlispstat::core::BinaryTermTestRow;
using rlispstat::core::BinaryModelsCompatibleForLikelihoodRatioTest;
using rlispstat::core::BuildBinaryAPAReportModel;
using rlispstat::core::BinaryRegressionCoefficientCSV;
using rlispstat::core::BinaryRegressionTermTestsCSV;
using rlispstat::core::BinaryRegressionModelFitCSV;
using rlispstat::core::InspectBinaryResponse;
using rlispstat::core::ParseBinaryLink;
using rlispstat::core::DataColumn;
using rlispstat::core::DataFrameModel;
using rlispstat::core::ApplyHolmAdjustedPValues;
using rlispstat::core::ContrastEstimate;
using rlispstat::core::ContrastVariance;
using rlispstat::core::CountDistribution;
using rlispstat::core::CoefficientForTerm;
using rlispstat::core::CoefficientsForSourceTerm;
using rlispstat::core::DiagnosticPlotData;
using rlispstat::core::DiagnosticOptions;
using rlispstat::core::AvailableGeneralizedResidualTypes;
using rlispstat::core::IsGeneralizedResidualTypeAvailable;
using rlispstat::core::EstimateLinearFunction;
using rlispstat::core::FitLinearOls;
using rlispstat::core::FitMultipleLinearModel;
using rlispstat::core::EvaluateGLMPairwiseComparisonEligibility;
using rlispstat::core::GLMPairwiseComparisonsRScript;
using rlispstat::core::ParseGLMPairwiseComparisonsRResult;
using rlispstat::core::GLMPairwiseComparisonsText;
using rlispstat::core::GLMPairwiseComparisonRequest;
using rlispstat::core::ComparisonLabelExists;
using rlispstat::core::ComparisonCopyTable;
using rlispstat::core::ComparisonCopyTableText;
using rlispstat::core::ComparisonModelSerialFromId;
using rlispstat::core::ComparisonNoAvailableTermsStatus;
using rlispstat::core::ComparisonTermNotIncludedStatus;
using rlispstat::core::GeneralizedDiagnosticRow;
using rlispstat::core::GeneralizedGLMDiagnosticRow;
using rlispstat::core::GeneralizedGLMFitSummary;
using rlispstat::core::GeneralizedGLMRow;
using rlispstat::core::GeneralizedLinksForFamily;
using rlispstat::core::GeneralizedFamilyRequiresResponseBounds;
using rlispstat::core::GeneralizedGLMRScript;
using rlispstat::core::GeneralizedCoefficientDisplay;
using rlispstat::core::GeneralizedComparisonFitLabel;
using rlispstat::core::GeneralizedComparisonTermRowsFromFits;
using rlispstat::core::GeneralizedComparisonFitDisplay;
using rlispstat::core::GeneralizedComparisonVisibleFitRows;
using rlispstat::core::GeneralizedComparisonFitDisplayData;
using rlispstat::core::GeneralizedComparisonFootnote;
using rlispstat::core::GeneralizedComparisonCoefficientDetailsStatus;
using rlispstat::core::GeneralizedComparisonCopiedStatus;
using rlispstat::core::GeneralizedComparisonSummaryStatus;
using rlispstat::core::GeneralizedCoefficientDetailsStatus;
using rlispstat::core::GeneralizedGLMFitSignature;
using rlispstat::core::GeneralizedGLMWindowStatus;
using rlispstat::core::GeneralizedGLMFormattedOutputText;
using rlispstat::core::GeneralizedGLMHasPendingManualFit;
using rlispstat::core::GeneralizedGLMPendingManualFitStatus;
using rlispstat::core::GeneralizedGLMRowsForPresentation;
using rlispstat::core::ClearGeneralizedGLMFitResults;
using rlispstat::core::GeneralizedGLMState;
using rlispstat::core::GeneralizedExponentiatedEffectLabel;
using rlispstat::core::GeneralizedModelHasExponentiatedEffect;
using rlispstat::core::GeneralizedGlobalTermTestHierarchyNote;
using rlispstat::core::GeneralizedGlobalTermTestForPresentationRow;
using rlispstat::core::GeneralizedGlobalTermTestMethodNote;
using rlispstat::core::GeneralizedGlobalTermTestStatisticHeader;
using rlispstat::core::GeneralizedModelUsesMixedGlobalTermTestMethods;
using rlispstat::core::ModelFamilyKind;
using rlispstat::core::GeneralizedGLMFitFailedStatus;
using rlispstat::core::GeneralizedGLMRscriptLaunchFailedStatus;
using rlispstat::core::ExplainModelStatistic;
using rlispstat::core::ModelStatisticExplanationContext;
using rlispstat::core::GLMNoActiveInteractionDataStatus;
using rlispstat::core::GLMInteractionModelNotFittedStatus;
using rlispstat::core::GLMChooseResponseVariableStatus;
using rlispstat::core::GLMInvalidFamilyStatus;
using rlispstat::core::GLMInvalidLinkForFamilyStatus;
using rlispstat::core::GLMNoRowsForScopeStatus;
using rlispstat::core::GLMInteractionTwoWayOnlyStatus;
using rlispstat::core::GLMInteractionNoFittedValuesStatus;
using rlispstat::core::GLMInteractionFactorNoLevelsStatus;
using rlispstat::core::GLMInteractionTypeNotAvailableStatus;
using rlispstat::core::GLMInteractionNoFiniteValuesStatus;
using rlispstat::core::GLMChooseDependentVariableStatus;
using rlispstat::core::GLMDependentVariableNotAvailableStatus;
using rlispstat::core::GLMFactorPredictorUnavailableStatus;
using rlispstat::core::GLMNumericPredictorUnavailableStatus;
using rlispstat::core::GLMPredictorUnavailableStatus;
using rlispstat::core::GLMNoDiagnosticRowsStatus;
using rlispstat::core::GLMNoActiveDiagnosticStatus;
using rlispstat::core::GLMCurrentLinearDiagnosticUnavailableStatus;
using rlispstat::core::GLMDiagnosticStateSummaryText;
using rlispstat::core::GLMSelectedLinearDiagnosticUnavailableStatus;
using rlispstat::core::GLMCurrentDiagnosticFitFailedStatus;
using rlispstat::core::GLMSelectedDiagnosticFitFailedStatus;
using rlispstat::core::GLMCurrentGeneralizedDiagnosticFitFailedStatus;
using rlispstat::core::GLMSelectedGeneralizedDiagnosticFitFailedStatus;
using rlispstat::core::GLMReadyStatus;
using rlispstat::core::GLMNoInteractionTermStatus;
using rlispstat::core::GLMRegressionComparisonNotAvailableStatus;
using rlispstat::core::GLMChooseValidModelColumnStatus;
using rlispstat::core::GLMGeneralizedLinearModelNotAvailableStatus;
using rlispstat::core::GLMGeneralizedModelComparisonNotAvailableStatus;
using rlispstat::core::GLMNoNumericResponseVariablesStatus;
using rlispstat::core::GLMImputedDatasetNotAvailableStatus;
using rlispstat::core::GLMAddPredictorBeforeTableStatus;
using rlispstat::core::GLMBaseVariableType;
using rlispstat::core::GLMComparisonRequiresPooledTestStatus;
using rlispstat::core::GLMNoInteractionReportTextStatus;
using rlispstat::core::GLMWindowTitle;
using rlispstat::core::GLMGeneralizedModelComparisonTitle;
using rlispstat::core::GLMGeneralLinearModelTitle;
using rlispstat::core::GLMAutoRefitButtonTitle;
using rlispstat::core::GLMDiagnosticResidualHistogramTitle;
using rlispstat::core::GLMDiagnosticResidualsFittedTitle;
using rlispstat::core::GLMDiagnosticObservedFittedTitle;
using rlispstat::core::GLMDiagnosticNormalQQTitle;
using rlispstat::core::GLMDevianceResidualTitle;
using rlispstat::core::GLMPearsonResidualTitle;
using rlispstat::core::GLMWorkingResidualTitle;
using rlispstat::core::GLMAddTermTitle;
using rlispstat::core::GLMChangeTermTitle;
using rlispstat::core::GLMInteractionPlotWindowTitle;
using rlispstat::core::GLMDiagnosticPlotWindowTitle;
using rlispstat::core::GLMRenameModelTitle;
using rlispstat::core::GLMDuplicateModelTitle;
using rlispstat::core::GLMDeleteModelTitle;
using rlispstat::core::GLMComparisonFamilyMenuTitle;
using rlispstat::core::GLMComparisonLinkMenuTitle;
using rlispstat::core::GLMComparisonFamilyMenuItemTitle;
using rlispstat::core::GLMComparisonLinkMenuItemTitle;
using rlispstat::core::GLMModelFitSectionTitle;
using rlispstat::core::GLMTermsSectionTitle;
using rlispstat::core::GLMFitNLabel;
using rlispstat::core::GLMFitNullDevianceLabel;
using rlispstat::core::GLMFitResidualDevianceLabel;
using rlispstat::core::GLMFitDfResidualLabel;
using rlispstat::core::GLMFitAICLabel;
using rlispstat::core::GLMFitBICLabel;
using rlispstat::core::GLMFitDispersionLabel;
using rlispstat::core::GLMFitLogLikLabel;
using rlispstat::core::GLMCoefVariableHeader;
using rlispstat::core::GLMCoefTypeHeader;
using rlispstat::core::GLMCoefBHeader;
using rlispstat::core::GLMCoefSEHeader;
using rlispstat::core::GLMCoefStatisticHeader;
using rlispstat::core::GLMCoefPHeader;
using rlispstat::core::GLMAnovaSumSquaresHeader;
using rlispstat::core::GLMAnovaDfHeader;
using rlispstat::core::GLMAnovaMeanSquareHeader;
using rlispstat::core::GLMAnovaFRatioHeader;
using rlispstat::core::GLMAnovaPHeader;
using rlispstat::core::GLMAnovaResidualLabel;
using rlispstat::core::GLMRowsUsedExcludedWindowTitle;
using rlispstat::core::GLMAddModelLabel;
using rlispstat::core::GLMAddTermLabel;
using rlispstat::core::GLMPlusSignLabel;
using rlispstat::core::GLMSelectedRowsPlaceholder;
using rlispstat::core::GLMD1WaldLabel;
using rlispstat::core::GLMAnovaFRatioLabel;
using rlispstat::core::GLMRegressionBetaLabel;
using rlispstat::core::GLMRegressionPartialRLabel;
using rlispstat::core::GLMRegressionDeltaRSquaredLabel;
using rlispstat::core::GLMInteractionsMenuTitle;
using rlispstat::core::GLMGeneralLinearTHeader;
using rlispstat::core::GLMGeneralLinearTermsHeader;
using rlispstat::core::GLMGeneralLinearVariableHeader;
using rlispstat::core::GLMGeneralLinearTypeHeader;
using rlispstat::core::GLMGeneralLinearBetaHeader;
using rlispstat::core::GLMInterceptTermName;
using rlispstat::core::GLMDashPlaceholder;
using rlispstat::core::GLMFitGlobalFitSectionTitle;
using rlispstat::core::GLMFitRSquaredLabel;
using rlispstat::core::GLMFitAdjustedRSquaredLabel;
using rlispstat::core::GLMFitSValueLabel;
using rlispstat::core::GLMFitDFLabel;
using rlispstat::core::GeneralizedRowDisplayLabel;
using rlispstat::core::GeneralizedRowForTerm;
using rlispstat::core::GeneralizedComparisonRowForPresentationTerm;
using rlispstat::core::GeneralizedRowSourceTerm;
using rlispstat::core::GLMCoefficientRow;
using rlispstat::core::GLMRegressionTableCellText;
using rlispstat::core::GLMRegressionTableRow;
using rlispstat::core::GLMTableDisplayState;
using rlispstat::core::GLMTableDisplayStateId;
using rlispstat::core::GLMDiagnosticRow;
using rlispstat::core::GLMFitSummary;
using rlispstat::core::GLMCompareLinearModelsHintStatus;
using rlispstat::core::GLMFitStatisticCopiedStatus;
using rlispstat::core::GLMFittedStatus;
using rlispstat::core::GLMImputedDatasetUnavailableStatus;
using rlispstat::core::GLMInteractionComponentTypeStatus;
using rlispstat::core::GLMModelTableCopiedStatus;
using rlispstat::core::GLMModelTableExportedStatus;
using rlispstat::core::GLMNoAvailablePredictorsStatus;
using rlispstat::core::GLMNoAvailableReplacementStatus;
using rlispstat::core::GLMPDFWriteFailedStatus;
using rlispstat::core::GLMRegressionCopyTableText;
using rlispstat::core::GLMModelDetailsText;
using rlispstat::core::GeneralizedGLMModelDetailsText;
using rlispstat::core::GLMFitNoteWarningStatus;
using rlispstat::core::GLMPooledTableChangeInRStatus;
using rlispstat::core::GLMPooledMIScopeRefitStatus;
using rlispstat::core::GLMPrecomputedTableRefitStatus;
using rlispstat::core::GLMRegressionTableCopiedStatus;
using rlispstat::core::GLMInteractionPlotOpenedStatus;
using rlispstat::core::GLMInteractionReportOpenedStatus;
using rlispstat::core::GLMRefittingPooledMIResponseStatus;
using rlispstat::core::GLMRefittingPooledMITermStatus;
using rlispstat::core::GLMSelectPredictorToRemoveStatus;
using rlispstat::core::GLMStatusMessage;
using rlispstat::core::GLMTermActionHintStatus;
using rlispstat::core::GLMTermTypeChangedStatus;
using rlispstat::core::GLMValueCopiedStatus;
using rlispstat::core::GeneralizedNestedModelTest;
using rlispstat::core::GeneralizedResidualForDiagnostic;
using rlispstat::core::IsQuasiGeneralizedFamily;
using rlispstat::core::IsValidGeneralizedFamily;
using rlispstat::core::IsValidGeneralizedLink;
using rlispstat::core::LinearNestedModelTest;
using rlispstat::core::LinearDiagnosticKindIsImplemented;
using rlispstat::core::LinearGLMFitSignature;
using rlispstat::core::LinearFunctionEstimate;
using rlispstat::core::LinearOlsInput;
using rlispstat::core::LinearOlsResult;
using rlispstat::core::NestedGeneralizedModelSummary;
using rlispstat::core::NestedLinearModelSummary;
using rlispstat::core::NestedModelTestResult;
using rlispstat::core::NextComparisonModelId;
using rlispstat::core::NextComparisonModelSerial;
using rlispstat::core::NextUntitledComparisonLabel;
using rlispstat::core::NormalizeLinearDiagnosticKind;
using rlispstat::core::PooledMIAutoRefitDisabledStatus;
using rlispstat::core::PooledMIComparisonChangeInRStatus;
using rlispstat::core::PooledMIComparisonRefitStatus;
using rlispstat::core::PooledMITermTypesChangeInRStatus;
using rlispstat::core::PooledOrPrecomputedRefitLayerStatus;
using rlispstat::core::PlotModel;
using rlispstat::core::GroupModelState;
using rlispstat::core::EnsureGroupModelState;
using rlispstat::core::LinearGLMFitMatchesTermTypes;
using rlispstat::core::LinearGLMFitMayBePresented;
using rlispstat::core::SynchronizeGroupModelTermTypes;
using rlispstat::core::SynchronizeModelSpecificationTermTypes;
using rlispstat::core::PrecomputedComparisonChangeInRStatus;
using rlispstat::core::ReadGeneralizedRowsPayload;
using rlispstat::core::ReadGeneralizedFitPayload;
using rlispstat::core::ReadGeneralizedStatePayload;
using rlispstat::core::ReadIntVectorPayload;
using rlispstat::core::ReadLinearDesignPayload;
using rlispstat::core::ReadLinearDiagnosticsPayload;
using rlispstat::core::ReadLinearMIDiagnosticsPayload;
using rlispstat::core::ReadLinearFitPayload;
using rlispstat::core::RegressionCoefficientDisplay;
using rlispstat::core::RegressionComparisonFitDisplay;
using rlispstat::core::RegressionComparisonFitCell;
using rlispstat::core::RegressionComparisonFitState;
using rlispstat::core::RegressionComparisonVisibleFitRows;
using rlispstat::core::RegressionComparisonTermDisplay;
using rlispstat::core::RegressionComparisonTermCell;
using rlispstat::core::RegressionComparisonCellDisplayState;
using rlispstat::core::BuildRegressionComparisonCopyTable;
using rlispstat::core::RegressionComparisonChangeDisplay;
using rlispstat::core::RegressionComparisonFitSignature;
using rlispstat::core::RegressionComparisonModelTermType;
using rlispstat::core::RegressionComparisonModelTermTypes;
using rlispstat::core::RegressionComparisonEffectiveResponse;
using rlispstat::core::ApplyRegressionComparisonTermType;
using rlispstat::core::ToggleRegressionComparisonPredictorCentering;
using rlispstat::core::RegressionComparisonFitLabel;
using rlispstat::core::RegressionComparisonFitRowIsModelTest;
using rlispstat::core::RegressionComparisonFootnote;
using rlispstat::core::RegressionComparisonCopiedStatus;
using rlispstat::core::RegressionComparisonModel;
using rlispstat::core::RegressionComparisonModelById;
using rlispstat::core::RegressionComparisonModelTestStatus;
using rlispstat::core::RegressionComparisonNoPreviousModelStatus;
using rlispstat::core::RegressionComparisonRefittedStatus;
using rlispstat::core::RegressionComparisonFitValueStatus;
using rlispstat::core::RegressionComparisonModelRenamedStatus;
using rlispstat::core::RegressionComparisonOpenedSingleModelStatus;
using rlispstat::core::RegressionComparisonState;
using rlispstat::core::SynchronizeRegressionComparisonTermTypes;
using rlispstat::core::RegressionComparisonSummaryStatus;
using rlispstat::core::RegressionComparisonTermRowsFromFits;
using rlispstat::core::RegressionComparisonUnavailableStatus;
using rlispstat::core::RegressionCoefficientGroupDetailsStatus;
using rlispstat::core::RegressionCoefficientStructuralDetailsStatus;
using rlispstat::core::RegressionRowDisplayLabel;
using rlispstat::core::RegressionRowSourceTerm;
using rlispstat::core::RowsUsedExcludedText;
using rlispstat::core::SameIntegerSet;
using rlispstat::core::ScenarioDesignInput;
using rlispstat::core::ScenarioVariableInfo;
using rlispstat::core::SelectedRowsStatus;
using rlispstat::core::SortFactorLevelsLikeR;
using rlispstat::core::SubtractVectors;
using rlispstat::core::TermVectorsAreNested;
using rlispstat::core::HolmAdjustedPValues;
using rlispstat::core::UniqueGeneralizedComparisonCopyLabel;
using rlispstat::core::UniqueRegressionComparisonCopyLabel;

static bool closeEnough(double a, double b, double tolerance = 1.0e-6)
{
    return std::fabs(a - b) < tolerance;
}

int main()
{
    {
        using namespace rlispstat::core;
        RegressionPartialPlotResult result;
        assert(ParseRegressionPartialPlotRResult({
            "OK", "META\tx\tpearson\t2", "RESIDUALS\tdunn_smyth\tpearson\traw",
            "ROW\t1\t5\t0.5\t1.5", "ROW\t2\t5\t0.7\t1.2"
        }, result));
        assert(result.imputationCount == 2);
        assert(result.availableResidualTypes == std::vector<std::string>({"dunn_smyth", "pearson", "raw"}));
        assert(result.pointsByImputation[1][0].row == 5);
        auto data = BuildRegressionPartialPlotData(result, 2, false);
        assert(data.ok && data.points[0].x == 0.7 && data.points[0].y == 1.2);
        PlotModel plot;
        plot.id = "partial-test";
        plot.title = data.title;
        OutputCodeReference source;
        source.provenance.dataVersion.datasetId = "original-data";
        source.provenance.dataVersion.version = 7;
        source.provenance.scope.kind = "explicit";
        source.provenance.scope.requestedStableRowIds = {"case-5"};
        result.provenance.verificationRCode["partial"] = "print(partial_plot)";
        const auto output = RegressionPartialPlotCodeReference(plot, result, source);
        assert(output.kind == "plot" && output.outputBlockId == "partial");
        assert(output.provenance.dataVersion.version == 7);
        assert(output.provenance.scope.requestedStableRowIds == std::vector<std::string>{"case-5"});
        assert(output.provenance.verificationRCode.at("partial") == "print(partial_plot)");
        RegressionPartialPlotResult scaled;
        assert(ParseRegressionPartialPlotRResult({
            "OK", "META\tx\tstudentized\t1\tstudentized",
            "RESIDUALS\traw\tstandardized\tstudentized", "TERMS\tx\tg",
            "ROW\t1\t5\t0.2\t0.4"
        }, scaled));
        assert(scaled.availableTerms == std::vector<std::string>({"x", "g"}));
        const auto scaledPlot = BuildRegressionPartialPlotData(scaled, 1, false);
        assert(scaledPlot.xLabel == "x contribution (studentized scale)");
        assert(!ParseRegressionPartialPlotRResult({
            "OK", "META\tx\tpearson\t1", "ROW\t1\t1\t0\t1", "ANALYSIS_PROVENANCE_V2"
        }, result));
    }

    {
        using namespace rlispstat::core;
        const auto count = DistributionSpecificationsForModel(StatisticalModelType::Count);
        std::vector<std::string> ids;
        for (const auto &distribution : count) ids.push_back(distribution.id);
        assert((ids == std::vector<std::string>{
            "poisson", "negative_binomial", "binomial_trials", "beta_binomial",
            "hurdle_beta_binomial_ceiling", "perfect_score"
        }));
        assert(std::find(ids.begin(), ids.end(), "gaussian") == ids.end());
        assert(std::find(ids.begin(), ids.end(), "binomial") == ids.end());
        assert(std::find(ids.begin(), ids.end(), "Gamma") == ids.end());
        assert(std::find(ids.begin(), ids.end(), "beta") == ids.end());
        assert(!CountDistributionUsesTrials(CountDistribution::Poisson));
        assert(!CountDistributionUsesTrials(CountDistribution::NegativeBinomial));
        assert(CountDistributionUsesTrials(CountDistribution::BinomialTrials));
        assert(CountDistributionUsesTrials(CountDistribution::BetaBinomial));
        assert(CountDistributionUsesTrials(
            CountDistribution::HurdleBetaBinomialCeiling));
        assert(CountDistributionUsesTrials(CountDistribution::PerfectScore));
        assert(CountDistributionSupportsScoreDistribution(
            CountDistribution::BinomialTrials));
        assert(CountDistributionSupportsScoreDistribution(
            CountDistribution::BetaBinomial));
        assert(CountDistributionSupportsScoreDistribution(
            CountDistribution::HurdleBetaBinomialCeiling));
        assert(CountDistributionSupportsScoreDistribution(
            CountDistribution::PerfectScore));
        assert(CountDistributionSupportsScoreDistribution(
            CountDistribution::Poisson));
        assert(CountDistributionSupportsScoreDistribution(
            CountDistribution::NegativeBinomial));
        const auto countInference =
            InferenceOptionSpecificationsForModel(StatisticalModelType::Count);
        assert(countInference.size() == 1);
        assert(countInference.front().id == "quasipoisson");
        assert(!countInference.front().fullProbabilityDistribution);
        assert(!countInference.front().supportsLikelihood);
        assert(!countInference.front().supportsAIC);
        assert(!countInference.front().supportsBIC);
        assert((ModelStructuresForModel(StatisticalModelType::Count) ==
                std::vector<std::string>{"standard"}));
        assert(!ModelTypeSpecification(StatisticalModelType::Count)
                    .modelTypeSelectorAvailable);

        const auto binary = DistributionSpecificationsForModel(StatisticalModelType::Binary);
        assert(binary.size() == 1 && binary.front().id == "binomial");
        assert(InferenceOptionSpecificationsForModel(
                   StatisticalModelType::Binary).empty());
        assert((binary.front().supportedLinks ==
                std::vector<std::string>{"logit", "log", "probit", "cloglog"}));
        const auto positive =
            DistributionSpecificationsForModel(StatisticalModelType::PositiveContinuous);
        assert(positive.size() == 4 && positive[0].id == "gaussian_log" &&
               positive[1].id == "lognormal" && positive[2].id == "Gamma" &&
               positive[3].id == "inverse.gaussian");
        assert(positive[0].responseDomain == ResponseDomain::Continuous);
        assert(positive[0].fitBackend == StatisticalFitBackend::StatsGlm);
        assert(positive[1].responseDomain == ResponseDomain::StrictlyPositiveContinuous);
        assert(positive[1].fitBackend == StatisticalFitBackend::StatsLm);
        const auto proportion =
            DistributionSpecificationsForModel(StatisticalModelType::Proportion);
        assert(proportion.size() == 2 && proportion[0].id == "beta" &&
               proportion[1].id == "beta_one_inflated");
        assert(StatisticalModelWindowTitle(StatisticalModelType::Linear) ==
               "Linear Model");
        assert(StatisticalModelWindowTitle(StatisticalModelType::Count, true) ==
               "Count Model — Multiple Imputation");
        assert(StatisticalModelComparisonTitle(
                   StatisticalModelType::PositiveContinuous) ==
               "Compare Positive Continuous Models");
        assert(StatisticalModelComparisonTitle(
                   StatisticalModelType::Proportion, true) ==
               "Compare Proportion Models — Multiple Imputation");
        assert(StatisticalFitBackendId(
            FindDistributionSpecification("negative_binomial")->fitBackend) ==
            "MASS::glm.nb");
        assert((LinksForModelDistribution(StatisticalModelType::PositiveContinuous,
                                          "Gamma") ==
                std::vector<std::string>{"log", "inverse", "identity"}));
        assert((LinksForModelDistribution(StatisticalModelType::PositiveContinuous,
                                          "gaussian_log") ==
                std::vector<std::string>{"log"}));
        assert((LinksForModelDistribution(StatisticalModelType::PositiveContinuous,
                                          "lognormal") ==
                std::vector<std::string>{"identity"}));
        assert(LinksForModelDistribution(StatisticalModelType::Count, "Gamma").empty());

        DataColumn response;
        response.name = "y";
        response.type = "numeric";
        response.values = {"0", "2", "2.5", "-1", "NA", "0.4", "1"};
        assert(InspectResponseDomain(response, ResponseDomain::NonNegativeInteger,
                                     {1, 2, 5}).ok);
        assert(!InspectResponseDomain(response, ResponseDomain::NonNegativeInteger,
                                      {1, 3}).ok);
        assert(!InspectResponseDomain(response, ResponseDomain::NonNegativeInteger,
                                      {2, 4}).ok);
        assert(InspectResponseDomain(response, ResponseDomain::OpenUnitInterval,
                                     {6}).ok);
        assert(!InspectResponseDomain(response, ResponseDomain::OpenUnitInterval,
                                      {6, 7}).ok);
        assert(InspectResponseDomain(response, ResponseDomain::OpenClosedUnitInterval,
                                     {6, 7}).ok);

        DataColumn linkOffset;
        linkOffset.name = "known_offset";
        linkOffset.type = "numeric";
        linkOffset.values = {"-1.5", "0", "2.25", "NA", "Inf"};
        const auto validOffset = rlispstat::core::InspectLinkOffset(
            linkOffset, {1, 2, 3, 4});
        assert(validOffset.ok && validOffset.observed == 3 && validOffset.missing == 1);
        const auto invalidOffset = rlispstat::core::InspectLinkOffset(linkOffset);
        assert(!invalidOffset.ok);
        assert(invalidOffset.status.find("finite") != std::string::npos);
        DataColumn categoricalOffset = linkOffset;
        categoricalOffset.type = "factor";
        assert(!rlispstat::core::InspectLinkOffset(categoricalOffset).ok);
    }
    {
        rlispstat::core::RegressionInteractionPlotResult pooledPlot;
        assert(rlispstat::core::ParseRegressionInteractionPlotRResult({
            "OK",
            "META\tx:g\tnumeric_factor\tx\tg\t5",
            "CONFIDENCE\t0.9",
            "ROW\tg = A\t0\t1.2\t1.0\t1.4",
            "ROW\tg = A\t1\t1.8\t1.5\t2.1",
            "ROW\tg = B\t0\t0.8\t0.6\t1.0",
            "ROW\tg = B\t1\t1.1\t0.9\t1.3"
        }, pooledPlot));
        assert(pooledPlot.ok);
        assert(pooledPlot.term == "x:g");
        assert(pooledPlot.imputationCount == 5);
        assert(closeEnough(pooledPlot.confidenceLevel, 0.9));
        assert(pooledPlot.lines.size() == 2);
        assert(pooledPlot.lines[0].points.size() == 2);
        assert(pooledPlot.lines[0].points[1].x == 1.0);
        assert(pooledPlot.lines[0].points[1].y == 1.8);
        assert(pooledPlot.lines[0].confidenceLower.size() == 2);
        assert(pooledPlot.lines[0].confidenceUpper.size() == 2);
        assert(closeEnough(pooledPlot.lines[0].confidenceLower[1].y, 1.5));
        assert(closeEnough(pooledPlot.lines[0].confidenceUpper[1].y, 2.1));
        PlotModel effectPlot;
        effectPlot.id = "effect-plot";
        effectPlot.kind = "glm_interaction";
        effectPlot.title = "Effect plot";
        effectPlot.xLabel = "x";
        effectPlot.yLabel = "Estimated y";
        effectPlot.interactionLegendTitle = "Group";
        effectPlot.rExportTheme = "minimal";
        effectPlot.regressionConfidenceIntervalsVisible = true;
        rlispstat::core::OutputCodeReference sourceModel;
        sourceModel.analysisId = "model-1";
        sourceModel.outputBlockId = "model";
        sourceModel.provenance.analysisId = sourceModel.analysisId;
        sourceModel.provenance.verificationRCode["model"] =
            "reference_model <- stats::lm(y ~ x, data = analysis_data)\n";
        const auto continuousReference =
            rlispstat::core::RegressionInteractionPlotCodeReference(
                effectPlot, pooledPlot, sourceModel);
        const std::string continuousRecipe =
            continuousReference.provenance.verificationRCode.at("interaction_plot:x:g");
        assert(continuousRecipe.find("ggplot2::geom_ribbon") != std::string::npos);
        assert(continuousRecipe.find("ggplot2::geom_errorbar") == std::string::npos);
        assert(continuousRecipe.find("effect_legend_labels <- function(values)") !=
               std::string::npos);
        assert(continuousRecipe.find(
            "series_data <- lapply(reference_plot_data[series_variables], effect_legend_labels)") !=
            std::string::npos);
        assert(continuousRecipe.find(
            "linkeda_plot_theme <- ggplot2::theme_minimal()") !=
            std::string::npos);
        assert(continuousRecipe.find("legend_title <- \"Group\"") !=
               std::string::npos);
        assert(continuousReference.publication.plot &&
               continuousReference.publication.plot->theme == "minimal");
        assert(continuousReference.publication.plot->legendTitle == "Group");

        auto capturedPlot = pooledPlot;
        capturedPlot.provenance.analysisId = "captured-lognormal";
        capturedPlot.provenance.dataVersion.version = 42;
        capturedPlot.provenance.verificationRCode["plot"] =
            "# Captured exact lognormal mean recipe\nreference <- public_package_recipe()\n";
        const auto capturedReference =
            rlispstat::core::RegressionInteractionPlotCodeReference(
                effectPlot, capturedPlot, sourceModel);
        const auto capturedRecipe = capturedReference.provenance.verificationRCode.at(
            "interaction_plot:x:g");
        assert(capturedRecipe.find("Captured exact lognormal mean recipe") != std::string::npos);
        assert(capturedRecipe.find("reference_grid_for_fit") == std::string::npos);
        assert(capturedRecipe.find(".estimate = response") != std::string::npos);
        assert(capturedRecipe.find("ggplot2::geom_ribbon") != std::string::npos);
        assert(capturedReference.provenance.dataVersion.version == 42);
        assert(capturedReference.provenance.outputRCode.at("interaction_plot:x:g") == "reference_plot");
        rlispstat::core::RegressionInteractionPlotResult invalidCapturedPlot;
        assert(!rlispstat::core::ParseRegressionInteractionPlotRResult({
            "OK", "META\tg\tsimple_factor\tg\t\t1",
            "ROW\tg\t1\t2\t1\t3", "ANALYSIS_PROVENANCE_V2"
        }, invalidCapturedPlot));

        rlispstat::core::RegressionInteractionPlotResult factorPlot = pooledPlot;
        factorPlot.term = "coded_factor:g";
        factorPlot.focal = "coded_factor";
        factorPlot.interactionType = "factor_factor";
        factorPlot.xTicks = {{0.0, "1"}, {1.0, "2"}};
        effectPlot.xLabel = "coded_factor";
        const auto categoricalReference =
            rlispstat::core::RegressionInteractionPlotCodeReference(
                effectPlot, factorPlot, sourceModel);
        const std::string categoricalRecipe =
            categoricalReference.provenance.verificationRCode.at(
                "interaction_plot:coded_factor:g");
        assert(categoricalRecipe.find("ggplot2::geom_errorbar") != std::string::npos);
        assert(categoricalRecipe.find("ggplot2::position_dodge") != std::string::npos);
        assert(categoricalRecipe.find("ggplot2::geom_ribbon") == std::string::npos);
        const std::string categoricalPublication =
            rlispstat::core::BuildPublicationRCode(
                categoricalReference, rlispstat::core::PublicationBackend::Ggplot2);
        assert(categoricalPublication.find("ggplot2::geom_errorbar") != std::string::npos);
        assert(categoricalPublication.find("ggplot2::geom_ribbon") == std::string::npos);
        assert(categoricalPublication.find("ggplot2::geom_line") != std::string::npos);

        factorPlot.term = "coded_factor";
        factorPlot.conditioning.clear();
        const auto nominalMainEffect =
            rlispstat::core::RegressionInteractionPlotCodeReference(
                effectPlot, factorPlot, sourceModel);
        const std::string nominalRecipe =
            nominalMainEffect.provenance.verificationRCode.at(
                "interaction_plot:coded_factor");
        assert(nominalRecipe.find("ggplot2::geom_errorbar") != std::string::npos);
        assert(nominalRecipe.find("connect_estimates <- TRUE") != std::string::npos);
        assert(nominalRecipe.find("ggplot2::geom_line") != std::string::npos);
        const std::string nominalPublication =
            rlispstat::core::BuildPublicationRCode(
                nominalMainEffect,
                rlispstat::core::PublicationBackend::Ggplot2);
        assert(nominalPublication.find("ggplot2::geom_line") != std::string::npos);

        PlotModel toggledEffect = effectPlot;
        toggledEffect.codeReference = nominalMainEffect;
        toggledEffect.regressionConnectEstimates = false;
        toggledEffect.regressionConfidenceIntervalsVisible = false;
        toggledEffect.interactionLegendTitle = "Treatment group";
        rlispstat::core::SynchronizeRegressionPlotCodeReferenceDisplay(toggledEffect);
        assert(toggledEffect.codeReference.publication.plot.has_value());
        assert(!toggledEffect.codeReference.publication.plot->showLines);
        assert(!toggledEffect.codeReference.publication.plot->showConfidenceIntervals);
        const std::string toggledRecipe =
            toggledEffect.codeReference.provenance.verificationRCode.at(
                "interaction_plot:coded_factor");
        assert(toggledRecipe.find("connect_estimates <- FALSE") != std::string::npos);
        assert(toggledRecipe.find("show_confidence_intervals <- FALSE") != std::string::npos);
        assert(toggledRecipe.find("legend_title <- \"Treatment group\"") !=
               std::string::npos);
        assert(toggledEffect.codeReference.publication.plot->legendTitle ==
               "Treatment group");
        const std::string toggledPublication =
            rlispstat::core::BuildPublicationRCode(
                toggledEffect.codeReference,
                rlispstat::core::PublicationBackend::Ggplot2);
        assert(toggledPublication.find("ggplot2::geom_line") == std::string::npos);
        assert(toggledPublication.find("ggplot2::geom_errorbar") == std::string::npos);

        rlispstat::core::RegressionInteractionPlotResult binaryEffect;
        assert(rlispstat::core::ParseRegressionInteractionPlotRResult({
            "OK",
            "META\ttreatment:sex\tfactor_factor\ttreatment\tsex\t20\tsex",
            "CONFIDENCE\t0.9",
            "BINARY\tYes\tprobability_difference\taverage_sample\tpercentage",
            "TICK\t1\tTreatment − Control",
            "ROW\tsex = Female\t1\t8.2\t1.1\t15.3",
            "ROW\tsex = Male\t1\t-3.4\t-10.2\t3.4"
        }, binaryEffect));
        assert(binaryEffect.binaryProbability);
        assert(binaryEffect.eventLabel == "Yes");
        assert(binaryEffect.quantity == "probability_difference");
        assert(binaryEffect.adjustmentMode == "average_sample");
        assert(binaryEffect.presentation == "percentage");
        assert(closeEnough(binaryEffect.confidenceLevel, .9));
        PlotModel binaryPlot;
        binaryPlot.id = "binary-effect";
        binaryPlot.kind = "glm_interaction";
        binaryPlot.title = "Effect: treatment by sex";
        binaryPlot.xLabel = "treatment";
        binaryPlot.regressionConfidenceIntervalsVisible = false;
        binaryPlot.regressionConnectEstimates = true;
        binaryPlot.interactionPlotLines = binaryEffect.lines;
        binaryPlot.ymin = -10.2;
        binaryPlot.ymax = 15.3;
        rlispstat::core::ApplyRegressionInteractionPresentation(
            binaryPlot, binaryEffect, "event");
        rlispstat::core::ApplyRegressionInteractionAxisRange(binaryPlot);
        assert(binaryPlot.title ==
            "Treatment − Control difference in P(Yes), by sex");
        assert(binaryPlot.subtitle == "Pooled across 20 imputations");
        assert(binaryPlot.xLabel == "treatment contrast");
        assert(binaryPlot.interactionFocalVariable == "treatment");
        assert(binaryPlot.interactionLegendTitle == "sex");
        const double originalEstimate = binaryPlot.interactionPlotLines[0].points[0].y;
        DataFrameModel labelledData;
        DataColumn focalColumn; focalColumn.name = "treatment";
        focalColumn.displayName = "Treatment";
        focalColumn.description = "Treatment assignment and study protocol details";
        labelledData.columns.push_back(focalColumn);
        DataColumn sexColumn; sexColumn.name = "sex";
        sexColumn.displayName = "Gender";
        labelledData.columns.push_back(sexColumn);
        rlispstat::core::ApplyRegressionInteractionVariableLabels(
            binaryPlot, binaryEffect, labelledData, "event");
        assert(binaryPlot.xLabel == "treatment contrast");
        assert(binaryPlot.presentationXLabel == "Treatment contrast");
        assert(binaryPlot.title.find("study protocol") == std::string::npos);
        assert(binaryPlot.interactionLegendTitle == "Gender");
        assert(binaryPlot.title.find("Gender") != std::string::npos);
        assert(binaryPlot.interactionPlotLines[0].points[0].y == originalEstimate);
        binaryPlot.interactionLegendTitleOverride = "Study arm";
        rlispstat::core::ApplyRegressionInteractionPresentation(
            binaryPlot, binaryEffect, "event");
        rlispstat::core::ApplyRegressionInteractionVariableLabels(
            binaryPlot, binaryEffect, labelledData, "event");
        assert(binaryPlot.interactionLegendTitle == "Study arm");
        assert(binaryPlot.interactionLegendDefaultTitle == "Gender");
        assert(binaryPlot.interactionPlotLines[0].label == "Female");
        assert(binaryPlot.interactionPlotLines[1].label == "Male");
        assert(binaryPlot.yLabel.find("percentage points") != std::string::npos);
        assert(binaryPlot.ymin <= 0.0 && binaryPlot.ymax >= 0.0);
        const auto binaryReference =
            rlispstat::core::RegressionInteractionPlotCodeReference(
                binaryPlot, binaryEffect, sourceModel);
        const std::string binaryRecipe =
            binaryReference.provenance.verificationRCode.at(
                "interaction_plot:treatment:sex");
        assert(binaryRecipe.find("marginaleffects::avg_predictions") != std::string::npos);
        assert(binaryRecipe.find("marginaleffects::avg_comparisons") != std::string::npos);
        assert(binaryRecipe.find("mice::pool.scalar") != std::string::npos);
        assert(binaryRecipe.find("effect_legend_labels <- function(values)") !=
               std::string::npos);
        assert(binaryRecipe.find(
            "series_data <- lapply(reference_plot_data[conditioning_variables], effect_legend_labels)") !=
            std::string::npos);
        assert(binaryRecipe.find("ggplot2::geom_hline(yintercept = 0") != std::string::npos);
        assert(binaryRecipe.find("effect_quantity <- \"probability_difference\"") != std::string::npos);
        assert(binaryRecipe.find("x = \"treatment contrast\"") != std::string::npos);

        PlotModel boundedCountPlot;
        boundedCountPlot.kind = "glm_interaction";
        boundedCountPlot.regressionBoundedCount = true;
        boundedCountPlot.regressionEffectQuantity = "expected_count";
        boundedCountPlot.regressionTrialsConstant = 24.0;
        boundedCountPlot.ymin = 21.9;
        boundedCountPlot.ymax = 22.2;
        rlispstat::core::ApplyRegressionInteractionAxisRange(boundedCountPlot);
        assert(boundedCountPlot.ymin > 0.0);
        assert(boundedCountPlot.ymax < 24.0);
        assert(boundedCountPlot.ymin < 21.9);
        assert(boundedCountPlot.ymax > 22.2);
        assert(binaryRecipe.find("confidence_level <- 0.900000") != std::string::npos ||
               binaryRecipe.find("confidence_level <- 0.9") != std::string::npos);

        rlispstat::core::RegressionInteractionPlotResult hurdleEffect = factorPlot;
        hurdleEffect.term = "coded_factor:g";
        hurdleEffect.focal = "coded_factor";
        hurdleEffect.conditioning = {"g"};
        hurdleEffect.boundedCount = true;
        hurdleEffect.ceilingHurdle = true;
        hurdleEffect.quantity = "overall_expected_score";
        hurdleEffect.trialsVariable = "Trials";
        hurdleEffect.trialsConstant = NAN;
        rlispstat::core::OutputCodeReference hurdleSource = sourceModel;
        hurdleSource.provenance.verificationRCode["model"] =
            "data <- base::readRDS(verification_data_path)\n"
            "model_formula <- cbind(Trials - score, score) ~ coded_factor * g\n"
            "fit <- gamlss::gamlss(model_formula, data = data, "
            "family = gamlss.dist::ZABB(), trace = FALSE)\n";
        const auto hurdleReference =
            rlispstat::core::RegressionInteractionPlotCodeReference(
                effectPlot, hurdleEffect, hurdleSource);
        const std::string hurdleRecipe =
            hurdleReference.provenance.verificationRCode.at(
                "interaction_plot:coded_factor:g");
        assert(hurdleRecipe.find("gamlss.dist::dBB") != std::string::npos);
        assert(hurdleRecipe.find("mice::pool.scalar") != std::string::npos);
        assert(hurdleRecipe.find("overall_expected_score") != std::string::npos);
        assert(hurdleRecipe.find("emmeans::emmeans") == std::string::npos);
        DataFrameModel imputed;
        imputed.rows = 3;
        imputed.imputationCount = 2;
        DataColumn group;
        group.name = "g";
        group.type = "factor";
        group.values = {"A", "A", "B"};
        group.imputationValues = {{"A", "A", "B"}, {"A", "B", "B"}};
        imputed.columns.push_back(group);
        rlispstat::core::PopulateRegressionInteractionLegendRows(pooledPlot, imputed);
        assert((std::set<int>(pooledPlot.lines[0].caseIds.begin(),
                              pooledPlot.lines[0].caseIds.end()) ==
                std::set<int>{1, 2}));
        assert((std::set<int>(pooledPlot.lines[1].caseIds.begin(),
                              pooledPlot.lines[1].caseIds.end()) ==
                std::set<int>{3}));

        rlispstat::core::RegressionInteractionPlotResult categoricalRows;
        categoricalRows.focal = "g";
        categoricalRows.xTicks = {{1.0, "A"}, {2.0, "B"}};
        rlispstat::core::PopulateRegressionInteractionLegendRows(
            categoricalRows, imputed);
        assert((categoricalRows.xTickCaseIds[0] == std::vector<int>{1, 2}));
        assert((categoricalRows.xTickCaseIds[1] == std::vector<int>{3}));
        imputed.activeImputationVersion = 2;
        rlispstat::core::PopulateRegressionInteractionLegendRows(pooledPlot, imputed);
        assert((std::set<int>(pooledPlot.lines[0].caseIds.begin(),
                              pooledPlot.lines[0].caseIds.end()) ==
                std::set<int>{1}));
        assert((std::set<int>(pooledPlot.lines[1].caseIds.begin(),
                              pooledPlot.lines[1].caseIds.end()) ==
                std::set<int>{2, 3}));
        rlispstat::core::PopulateRegressionInteractionLegendRows(
            categoricalRows, imputed);
        assert((categoricalRows.xTickCaseIds[0] == std::vector<int>{1}));
        assert((categoricalRows.xTickCaseIds[1] == std::vector<int>{2, 3}));

        rlispstat::core::RegressionInteractionPlotResult threeWayPlot;
        assert(rlispstat::core::ParseRegressionInteractionPlotRResult({
            "OK",
            "META\tx:g:rating\tthree_way_with_numeric\tx\tg\t5\tg\trating",
            "ROW\tg = A · rating = low\t0\t1.2\t1.0\t1.4",
            "ROW\tg = A · rating = low\t1\t1.8\t1.5\t2.1",
            "ROW\tg = B · rating = high\t0\t0.8\t0.6\t1.0",
            "ROW\tg = B · rating = high\t1\t1.1\t0.9\t1.3"
        }, threeWayPlot));
        assert((threeWayPlot.conditioning ==
                std::vector<std::string>{"g", "rating"}));
        assert(threeWayPlot.lines.size() == 2);
        DataFrameModel threeWayData;
        threeWayData.rows = 3;
        DataColumn threeWayGroup;
        threeWayGroup.name = "g";
        threeWayGroup.type = "factor";
        threeWayGroup.values = {"A", "A", "B"};
        DataColumn rating;
        rating.name = "rating";
        rating.type = "ordered";
        rating.values = {"low", "high", "high"};
        threeWayData.columns = {threeWayGroup, rating};
        rlispstat::core::PopulateRegressionInteractionLegendRows(
            threeWayPlot, threeWayData);
        assert((threeWayPlot.lines[0].caseIds == std::vector<int>{1}));
        assert((threeWayPlot.lines[1].caseIds == std::vector<int>{3}));

        rlispstat::core::GLMInteractionReport parsedContrastReport;
        assert(rlispstat::core::ParseGLMInteractionReportText({
            "Interaction term: alpha:beta",
            "Type: factor_factor",
            "Pooling: emmeans support for mice::mira objects",
            "",
            "Interaction contrasts",
            "Contrast in A\tContrast in B\tEstimate\tSE\tdf\tStatistic\tp\tLower 95%\tUpper 95%",
            "a1 - a2\tb1 - b2\t2.7500\t0.1250\t41.50\t22.000\t< .001\t2.4975\t3.0025",
            "",
            "Exponentiated interaction contrasts",
            "Contrast in A\tContrast in B\tRatio of odds ratios\tSE\tLower 95%\tUpper 95%",
            "a1 - a2\tb1 - b2\t15.6426\t—\t12.1521\t20.1358",
            "",
            "Contrast factor A: alpha",
            "Comparison factor B: beta",
            "Scale: link",
            "Interaction contrast estimand: logit difference of differences (link scale)",
            "Interaction contrast pooling: mice::pool.scalar (Rubin's rules)",
            "Orientation: first minus second at both stages.",
            "Definition: (mu[a1,b1] - mu[a2,b1]) - (mu[a1,b2] - mu[a2,b2]).",
            "Note: estimates quantify statistical differences of differences."
        }, parsedContrastReport));
        assert(parsedContrastReport.poolingMethod ==
               "emmeans support for mice::mira objects");
        assert(parsedContrastReport.sections.size() == 2);
        assert(parsedContrastReport.sections[0].showDegreesOfFreedom);
        assert(closeEnough(
            parsedContrastReport.sections[0].second[0].degreesOfFreedom, 41.5));
        assert(closeEnough(
            parsedContrastReport.sections[1].second[0].estimate, 15.6426));
        assert(parsedContrastReport.notes.size() == 8);
        const std::string contrastReportText =
            rlispstat::core::GLMInteractionReportText(parsedContrastReport);
        assert(contrastReportText.find("Interaction contrast pooling: mice::pool.scalar") !=
               std::string::npos);
        assert(contrastReportText.find("Orientation: first minus second") !=
               std::string::npos);
        assert(contrastReportText.find("Interaction contrast estimand: logit") !=
               std::string::npos);
        assert(contrastReportText.find("Ratio of odds ratios\tSE") !=
               std::string::npos);
        const auto contrastTables =
            rlispstat::core::InteractionScalePublicationTables(parsedContrastReport);
        assert(contrastTables.size() == 2);
        assert(contrastTables[1].columns[2].label == "Ratio of odds ratios");
        assert(contrastTables[1].rows.size() == 1);
    }
    {
        std::map<std::string, GroupModelState> states;
        PlotModel oneNumericSeed;
        oneNumericSeed.xLabel = "Valor";
        oneNumericSeed.yLabel = "Valor";
        GroupModelState &state = EnsureGroupModelState(states, "datos", &oneNumericSeed);
        assert(state.autoRefit);
        // Plot axes are not model roles.  A newly opened analysis remains
        // unconfigured until the user declares roles in Variables/Data Sheet
        // or chooses them inside the analysis window.
        assert(state.response.empty());
        assert(state.terms.empty());
        state.response = "outcome";
        state.terms = {"predictor"};
        PlotModel unrelatedPlot;
        unrelatedPlot.xLabel = "plot_x";
        unrelatedPlot.yLabel = "plot_y";
        GroupModelState &preserved = EnsureGroupModelState(states, "datos", &unrelatedPlot);
        assert(preserved.response == "outcome");
        assert(preserved.terms == std::vector<std::string>({"predictor"}));
        preserved.autoRefit = false;
        rlispstat::core::MarkGroupModelChanged(preserved);
        // A specification mutation must not silently undo the user's refit
        // policy; re-enabling the checkbox is what explicitly retries it.
        assert(!preserved.autoRefit);

        GroupModelState miPreserved;
        miPreserved.precomputed = true;
        miPreserved.multipleImputation = true;
        miPreserved.imputationCount = 20;
        miPreserved.title = "General Linear Model - Multiple Imputation";
        miPreserved.note = "coefficients use mice::pool";
        miPreserved.isStale = false;
        miPreserved.rFitPending = true;
        miPreserved.lastRFitSignature = "old-selection";
        rlispstat::core::MarkGroupModelChanged(miPreserved);
        assert(miPreserved.precomputed);
        assert(miPreserved.multipleImputation);
        assert(miPreserved.imputationCount == 20);
        assert(miPreserved.title == "General Linear Model - Multiple Imputation");
        assert(miPreserved.note == "coefficients use mice::pool");
        assert(miPreserved.isStale);
        assert(!miPreserved.rFitPending);
        assert(miPreserved.lastRFitSignature.empty());

        PlotModel twoNumericSeed;
        twoNumericSeed.xLabel = "Tiempo";
        twoNumericSeed.yLabel = "Valor";
        GroupModelState &twoVariableState = EnsureGroupModelState(states, "datos2", &twoNumericSeed);
        assert(twoVariableState.response.empty());
        assert(twoVariableState.terms.empty());

        DataFrameModel dataframe;
        DataColumn numeric;
        numeric.name = "mpg";
        numeric.type = "numeric";
        DataColumn factor;
        factor.name = "cyl";
        factor.type = "factor";
        dataframe.columns = {numeric, factor};
        twoVariableState.terms = {"mpg", "cyl", "mpg:cyl"};
        twoVariableState.termTypes = {{"mpg", "factor"}, {"orphan", "factor"}};
        assert(SynchronizeGroupModelTermTypes(twoVariableState, dataframe));
        const std::map<std::string, std::string> expectedTypes = {
            {"cyl", "factor"}, {"mpg", "numeric"}};
        assert(twoVariableState.termTypes == expectedTypes);
        assert(!SynchronizeGroupModelTermTypes(twoVariableState, dataframe));

        twoVariableState.termTypeOverrides = {{"mpg", "factor"}};
        assert(SynchronizeGroupModelTermTypes(twoVariableState, dataframe));
        const std::map<std::string, std::string> plotInterpretedTypes = {
            {"cyl", "factor"}, {"mpg", "factor"}};
        assert(twoVariableState.termTypes == plotInterpretedTypes);
        assert(!SynchronizeGroupModelTermTypes(twoVariableState, dataframe));

        twoVariableState.termTypeOverrides["orphan"] = "factor";
        SynchronizeGroupModelTermTypes(twoVariableState, dataframe);
        assert(twoVariableState.termTypeOverrides.size() == 1);
        assert(twoVariableState.termTypeOverrides.at("mpg") == "factor");

        // Generalized-model startup must use the same canonical dataset
        // interpretation.  A stale numeric spelling for a character/factor
        // predictor would otherwise be serialized to R as as.numeric(),
        // turning every observation into NA and yielding zero complete cases.
        GeneralizedGLMState generalizedStartup;
        generalizedStartup.response = "mpg";
        generalizedStartup.terms = {"cyl"};
        generalizedStartup.termTypes = {{"cyl", "numeric"}};
        assert(SynchronizeModelSpecificationTermTypes(generalizedStartup, dataframe));
        assert(generalizedStartup.familyKind == ModelFamilyKind::GeneralizedLinear);
        assert(generalizedStartup.termTypes.size() == 1);
        assert(generalizedStartup.termTypes.at("cyl") == "factor");
        assert(!SynchronizeModelSpecificationTermTypes(generalizedStartup, dataframe));

        GLMFitSummary staleNumericFit;
        GLMCoefficientRow staleCyl;
        staleCyl.term = "cyl";
        staleCyl.sourceTerm = "cyl";
        staleCyl.termType = "numeric";
        staleNumericFit.coefficients.push_back(staleCyl);
        assert(!LinearGLMFitMatchesTermTypes(staleNumericFit,
                                             twoVariableState.terms,
                                             twoVariableState.termTypes));
        staleNumericFit.coefficients.front().termType = "factor";
        staleNumericFit.coefficients.front().rowType = "factor_parent";
        assert(LinearGLMFitMatchesTermTypes(staleNumericFit,
                                            twoVariableState.terms,
                                            twoVariableState.termTypes));

        // A cached fit is never presentation data for a newer specification.
        // This is the exact state reached while replacing a factor such as
        // `region` with `coastal`: the canonical term changes immediately,
        // while R is still calculating the new exact signature.
        GroupModelState presentationState;
        presentationState.response = "monthly_rent_eur";
        presentationState.terms = {"coastal"};
        presentationState.termTypes = {{"coastal", "factor"}};
        const std::string coastalSignature = LinearGLMFitSignature(
            presentationState.response, presentationState.terms,
            presentationState.termTypes, "all",
            presentationState.centeredPredictors,
            presentationState.factorReferenceLevels);
        GLMFitSummary oldRegionFit;
        oldRegionFit.ok = true;
        GLMCoefficientRow oldRegionRow;
        oldRegionRow.term = "region";
        oldRegionRow.sourceTerm = "region";
        oldRegionRow.termType = "factor";
        oldRegionFit.coefficients.push_back(oldRegionRow);
        presentationState.isStale = true;
        presentationState.rFitPending = true;
        presentationState.lastRFitSignature = coastalSignature;
        assert(!LinearGLMFitMayBePresented(
            presentationState, coastalSignature, &oldRegionFit));
        presentationState.isStale = false;
        presentationState.rFitPending = false;
        assert(LinearGLMFitMayBePresented(
            presentationState, coastalSignature, &oldRegionFit));
        assert(!LinearGLMFitMayBePresented(
            presentationState, coastalSignature + "|older", &oldRegionFit));

        // With Auto-refit disabled, preserve the accepted result so two
        // windows can continue to show fits calculated on different frozen
        // scopes. The status text makes clear that the controls/data have
        // changed since that fit.
        GeneralizedGLMState generalizedPresentation;
        generalizedPresentation.autoRefit = false;
        generalizedPresentation.ok = true;
        generalizedPresentation.modelVersion = 4;
        generalizedPresentation.fitVersion = 3;
        generalizedPresentation.terms = {"age", "group", "age:group"};
        generalizedPresentation.termTypes = {
            {"age", "numeric"}, {"group", "factor"}
        };
        GeneralizedGLMRow oldGeneralizedRow;
        oldGeneralizedRow.term = "country";
        oldGeneralizedRow.sourceTerm = "country";
        generalizedPresentation.rows = {oldGeneralizedRow};
        assert(GeneralizedGLMHasPendingManualFit(generalizedPresentation));
        const auto pendingRows =
            GeneralizedGLMRowsForPresentation(generalizedPresentation);
        assert(pendingRows.size() == 1);
        assert(pendingRows.front().term == "country");
        assert(GeneralizedGLMPendingManualFitStatus().find("Auto-refit is off") !=
               std::string::npos);
        const std::string pendingOutput = GeneralizedGLMFormattedOutputText(
            generalizedPresentation, false);
        assert(pendingOutput.find("last completed fit") != std::string::npos);
        assert(pendingOutput.find("country") != std::string::npos);

        generalizedPresentation.fitVersion = generalizedPresentation.modelVersion;
        assert(!GeneralizedGLMHasPendingManualFit(generalizedPresentation));
        const auto fittedRows = GeneralizedGLMRowsForPresentation(
            generalizedPresentation);
        assert(fittedRows.size() == 1);
        assert(fittedRows.front().term == "country");

        // Every newly opened generalized-model window owns a fresh semantic
        // specification. Dataset roles may seed variable choices, but no
        // model-local type, centering or reference state may leak between
        // Count Regression, ordinary GLM, Binary Regression, or two windows
        // of the same analysis.
        DataFrameModel sessionDataframe;
        sessionDataframe.group = "sessions";
        sessionDataframe.rows = 4;
        DataColumn countResponse;
        countResponse.name = "count_y";
        countResponse.type = "numeric";
        countResponse.values = {"0", "1", "2", "3"};
        DataColumn numericPredictor;
        numericPredictor.name = "x";
        numericPredictor.type = "numeric";
        numericPredictor.values = {"1", "2", "3", "4"};
        DataColumn factorPredictor;
        factorPredictor.name = "group";
        factorPredictor.type = "factor";
        factorPredictor.values = {"A", "B", "A", "B"};
        factorPredictor.definedLevels = {"A", "B"};
        DataColumn binaryResponse;
        binaryResponse.name = "binary_y";
        binaryResponse.type = "factor";
        binaryResponse.values = {"no", "yes", "no", "yes"};
        binaryResponse.definedLevels = {"no", "yes"};
        sessionDataframe.columns = {
            countResponse, numericPredictor, factorPredictor, binaryResponse};
        rlispstat::core::AnalysisVariableRoles sessionRoles = {
            {"count_y", "dependent"},
            {"x", "independent"},
            {"group", "independent"},
            {"binary_y", "none"}};
        rlispstat::core::AnalysisScope sessionScope;
        PlotModel sessionSeed;
        sessionSeed.group = "sessions";

        auto countSession = rlispstat::core::CreateIndependentGeneralizedGLMSession(
            "count-1", "sessions", sessionSeed, sessionDataframe,
            rlispstat::core::SharedAnalysisKind::CountRegression,
            sessionRoles, sessionScope, "all");
        auto glmSession = rlispstat::core::CreateIndependentGeneralizedGLMSession(
            "glm-1", "sessions", sessionSeed, sessionDataframe,
            rlispstat::core::SharedAnalysisKind::GeneralizedLinearModel,
            sessionRoles, sessionScope, "all");
        assert(countSession.response == "count_y");
        assert(glmSession.response == "count_y");
        assert(countSession.terms == std::vector<std::string>({"group", "x"}));
        assert(glmSession.terms == countSession.terms);
        auto interceptOnlyRoles = sessionRoles;
        interceptOnlyRoles["x"] = "none";
        interceptOnlyRoles["group"] = "none";
        auto interceptOnlyCount = rlispstat::core::CreateIndependentGeneralizedGLMSession(
            "count-intercept", "sessions", sessionSeed, sessionDataframe,
            rlispstat::core::SharedAnalysisKind::CountRegression,
            interceptOnlyRoles, sessionScope, "all");
        assert(interceptOnlyCount.terms.empty());
        assert(interceptOnlyCount.status == "Not fitted.");

        // macOS can add a predictor after startup. Its row label can derive
        // "factor" from the dataset even though the newly added variable has
        // not yet been materialized in ModelSpecification::termTypes. The fit
        // boundary must canonicalize the specification before its fingerprint
        // or task payload is produced.
        assert(rlispstat::core::AddModelSpecificationTerm(
            interceptOnlyCount,
            std::vector<std::string>{"count_y", "x", "group", "binary_y"},
            "group").changed);
        assert(interceptOnlyCount.termTypes.find("group") ==
               interceptOnlyCount.termTypes.end());
        assert(rlispstat::core::EffectiveModelSpecificationTermTypes(
            interceptOnlyCount).at("group") == "numeric");
        assert(rlispstat::core::SynchronizeGeneralizedGLMFitSpecification(
            interceptOnlyCount, sessionDataframe));
        assert(interceptOnlyCount.termTypes.at("group") == "factor");
        assert(rlispstat::core::EffectiveModelSpecificationTermTypes(
            interceptOnlyCount).at("group") == "factor");
        assert(rlispstat::core::GeneralizedGLMFitSignature(
            interceptOnlyCount).find("|group=factor") != std::string::npos);

        countSession.terms = {"x", "x:group"};
        countSession.termTypeOverrides["x"] = "factor";
        countSession.termTypes["x"] = "factor";
        countSession.centeredPredictors.insert("x");
        countSession.factorReferenceLevels["group"] = "B";
        countSession.family = "quasipoisson";
        countSession.link = "sqrt";
        countSession.exposure = "x";
        countSession.diagnosticOptions.residualType = "pearson";
        countSession.scope = "selected";
        countSession.dataScope.originalRowIds = {1, 3};
        assert(glmSession.terms == std::vector<std::string>({"group", "x"}));
        assert(glmSession.termTypeOverrides.empty());
        assert(rlispstat::core::ModelSpecificationTermType(
            glmSession, "x", "numeric") == "numeric");
        assert(glmSession.centeredPredictors.empty());
        assert(glmSession.factorReferenceLevels.empty());
        assert(glmSession.family == "gaussian");
        assert(glmSession.link == "identity");
        assert(glmSession.exposure.empty());
        assert(glmSession.diagnosticOptions.residualType == "deviance");
        assert(glmSession.scope == "all");
        assert(glmSession.dataScope.originalRowIds.empty());

        auto secondGlmSession = rlispstat::core::CreateIndependentGeneralizedGLMSession(
            "glm-2", "sessions", sessionSeed, sessionDataframe,
            rlispstat::core::SharedAnalysisKind::GeneralizedLinearModel,
            sessionRoles, sessionScope, "all");
        glmSession.terms.clear();
        glmSession.response = "x";
        glmSession.centeredPredictors.insert("group");
        assert(secondGlmSession.response == "count_y");
        assert(secondGlmSession.terms == std::vector<std::string>({"group", "x"}));
        assert(secondGlmSession.centeredPredictors.empty());

        rlispstat::core::AnalysisVariableRoles noRoles;
        auto blankGlmSession = rlispstat::core::CreateIndependentGeneralizedGLMSession(
            "glm-blank", "sessions", sessionSeed, sessionDataframe,
            rlispstat::core::SharedAnalysisKind::GeneralizedLinearModel,
            noRoles, sessionScope, "all");
        assert(blankGlmSession.response.empty());
        assert(blankGlmSession.terms.empty());

        rlispstat::core::ModelSpecification saved;
        saved.familyKind = ModelFamilyKind::GeneralizedLinear;
        saved.response = "count_y";
        saved.terms = {"x", "group", "x:group"};
        saved.termTypeOverrides["group"] = "factor";
        saved.centeredPredictors.insert("x");
        saved.factorReferenceLevels["group"] = "B";
        saved.scope = "selected";
        saved.dataScope.originalRowIds = {2, 4};
        saved.dataScopeCaptured = true;
        auto restored = rlispstat::core::CreateIndependentGeneralizedGLMSession(
            "glm-restored", "sessions", sessionSeed, sessionDataframe,
            rlispstat::core::SharedAnalysisKind::GeneralizedLinearModel,
            noRoles, sessionScope, "all", &saved);
        assert(restored.response == "count_y");
        assert(restored.terms == saved.terms);
        assert(restored.termTypeOverrides == saved.termTypeOverrides);
        assert(restored.centeredPredictors == saved.centeredPredictors);
        assert(restored.factorReferenceLevels == saved.factorReferenceLevels);
        restored.terms.clear();
        restored.centeredPredictors.clear();
        restored.factorReferenceLevels["group"] = "A";
        assert(saved.terms == std::vector<std::string>({"x", "group", "x:group"}));
        assert(saved.centeredPredictors.count("x") == 1);
        assert(saved.factorReferenceLevels.at("group") == "B");

        // A supplied but invalid saved response is not silently replaced by
        // a role-derived response from the dataset.
        rlispstat::core::ModelSpecification invalidSaved;
        invalidSaved.response = "missing_response";
        invalidSaved.terms = {"x"};
        auto invalidRestore = rlispstat::core::CreateIndependentGeneralizedGLMSession(
            "glm-invalid", "sessions", sessionSeed, sessionDataframe,
            rlispstat::core::SharedAnalysisKind::GeneralizedLinearModel,
            sessionRoles, sessionScope, "all", &invalidSaved);
        assert(invalidRestore.response.empty());
        assert(invalidRestore.terms == std::vector<std::string>({"x"}));
    }
    BinaryLink binaryLink = BinaryLink::Logit;
    assert(ParseBinaryLink("Probit", binaryLink));
    assert(binaryLink == BinaryLink::Probit);
    assert(BinaryLinkId(binaryLink) == "probit");
    assert(BinaryLinkLabel(binaryLink) == "Probit");
    assert(ParseBinaryLink("cloglog", binaryLink));
    assert(binaryLink == BinaryLink::Cloglog);
    assert(BinaryLinkId(binaryLink) == "cloglog");
    assert(BinaryLinkLabel(binaryLink) == "Complementary log-log");
    DataColumn binaryNumeric;
    binaryNumeric.name = "outcome";
    binaryNumeric.type = "numeric";
    binaryNumeric.values = {"5", "2", "5", "NA", "2"};
    auto coding = InspectBinaryResponse(binaryNumeric);
    assert(coding.ok);
    assert(coding.referenceValue == "2");
    assert(coding.eventValue == "5");
    assert(coding.referenceCount == 2);
    assert(coding.eventCount == 2);
    DataColumn invalidBinary = binaryNumeric;
    invalidBinary.values.push_back("8");
    assert(!InspectBinaryResponse(invalidBinary).ok);
    std::vector<std::string> numericLevels = {"10", "2", "1", "2.0"};
    SortFactorLevelsLikeR(numericLevels);
    assert((numericLevels == std::vector<std::string>{"1", "2", "2.0", "10"}));
    std::vector<std::string> textLevels = {"high", "low", "medium"};
    SortFactorLevelsLikeR(textLevels);
    assert((textLevels == std::vector<std::string>{"high", "low", "medium"}));
    std::vector<std::string> mixedLevels = {"2", "A", "1"};
    SortFactorLevelsLikeR(mixedLevels);
    assert((mixedLevels == std::vector<std::string>{"1", "2", "A"}));
    PlotModel baseTypePlot;
    baseTypePlot.variableMeta = {{"x", "numeric"}, {"group", "factor"}};
    std::map<std::string, std::string> baseTermTypes{{"x", "factor"}};
    assert(GLMBaseVariableType(&baseTypePlot, "x", baseTermTypes) == "factor");
    assert(GLMBaseVariableType(&baseTypePlot, "group", {}) == "factor");
    assert(GLMBaseVariableType(&baseTypePlot, "x:group", baseTermTypes) == "numeric");
    assert(GLMBaseVariableType(nullptr, "x", baseTermTypes) == "numeric");

    assert(IsValidGeneralizedFamily("gaussian"));
    assert(IsValidGeneralizedFamily("gaussian_log"));
    assert(IsValidGeneralizedFamily("lognormal"));
    assert(IsValidGeneralizedFamily("binomial"));
    assert(IsValidGeneralizedFamily("quasipoisson"));
    assert(IsValidGeneralizedFamily("beta"));
    assert(IsValidGeneralizedFamily("beta_one_inflated"));
    assert(IsValidGeneralizedFamily("gamma_distance"));
    assert(!IsValidGeneralizedFamily("cox"));

    assert(DefaultGeneralizedLink("gaussian") == "identity");
    assert(DefaultGeneralizedLink("gaussian_log") == "log");
    assert(DefaultGeneralizedLink("lognormal") == "identity");
    assert(DefaultGeneralizedLink("poisson") == "log");
    assert(DefaultGeneralizedLink("Gamma") == "inverse");
    assert(DefaultGeneralizedLink("beta") == "logit");
    assert(DefaultGeneralizedLink("gamma_distance") == "log");
    assert(DefaultGeneralizedLink("unknown") == "logit");
    assert(DefaultGeneralizedFamilyForValues({0.0, 1.0, 1.0, std::numeric_limits<double>::quiet_NaN()}) == "binomial");
    assert(DefaultGeneralizedFamilyForValues({0.0, 1.0, 2.0, 3.0}) == "poisson");
    assert(DefaultGeneralizedFamilyForValues({0.0, 1.5, 2.0}) == "gaussian");
    assert(DefaultGeneralizedFamilyForValues({-1.0, 0.0, 1.0}) == "gaussian");
    assert(DefaultGeneralizedFamilyForValues({std::numeric_limits<double>::quiet_NaN()}) == "gaussian");

    assert((GeneralizedLinksForFamily("binomial") ==
            std::vector<std::string>{"logit", "probit", "cloglog", "cauchit", "log"}));
    assert((GeneralizedLinksForFamily("gaussian_log") ==
            std::vector<std::string>{"log"}));
    assert((GeneralizedLinksForFamily("lognormal") ==
            std::vector<std::string>{"identity"}));
    assert(IsValidGeneralizedLink("binomial", "probit"));
    assert(!IsValidGeneralizedLink("poisson", "probit"));
    assert((GeneralizedLinksForFamily("beta_one_inflated") ==
            std::vector<std::string>{"logit"}));
    assert(GeneralizedFamilyRequiresResponseBounds("beta"));
    assert(GeneralizedFamilyRequiresResponseBounds("beta_one_inflated"));
    assert(GeneralizedFamilyRequiresResponseBounds("gamma_distance"));
    assert(!GeneralizedFamilyRequiresResponseBounds("gaussian"));
    assert(IsQuasiGeneralizedFamily("quasipoisson"));
    assert(!IsQuasiGeneralizedFamily("poisson"));

    GeneralizedGLMState exponentiatedState;
    exponentiatedState.family = "gaussian_log";
    assert(GeneralizedModelHasExponentiatedEffect(exponentiatedState));
    assert(!rlispstat::core::GeneralizedModelShowsGlobalTermTests(exponentiatedState));
    exponentiatedState.termTests.push_back({"x", "x", "Wald chi-square"});
    assert(rlispstat::core::GeneralizedModelShowsGlobalTermTests(exponentiatedState));
    exponentiatedState.termTests.clear();
    assert(GeneralizedExponentiatedEffectLabel(exponentiatedState) == "Mean ratio");
    exponentiatedState.family = "lognormal";
    assert(!rlispstat::core::GeneralizedModelShowsGlobalTermTests(exponentiatedState));
    assert(GeneralizedExponentiatedEffectLabel(exponentiatedState) ==
           "Multiplicative ratio");
    const std::string lognormalDetails =
        GeneralizedGLMModelDetailsText(exponentiatedState);
    assert(lognormalDetails.find("Response transformation: log(Y)") !=
           std::string::npos);
    assert(lognormalDetails.find("Residual variance (log scale)") !=
           std::string::npos);
    exponentiatedState.family = "binomial";
    exponentiatedState.binaryLink = BinaryLink::Log;
    assert(GeneralizedExponentiatedEffectLabel(exponentiatedState) == "Risk ratio");
    exponentiatedState.countRegression = true;
    assert(GeneralizedExponentiatedEffectLabel(exponentiatedState) == "Rate ratio");
    GeneralizedGLMState ordinaryLinearState;
    ordinaryLinearState.family = "gaussian";
    ordinaryLinearState.modelType = rlispstat::core::StatisticalModelType::Linear;
    assert(!rlispstat::core::GeneralizedModelShowsGlobalTermTests(ordinaryLinearState));

    std::map<std::string, std::string> signatureTypes{{"cyl", "factor"}, {"wt", "numeric"}};
    assert(LinearGLMFitSignature("mpg", {"wt", "cyl"}, signatureTypes, "selected") ==
           "mpg|selected|wt|cyl|types|cyl=factor|wt=numeric");
    assert(rlispstat::core::LinearGLMFitIdentityWithSelection(
               "mpg|selected|wt", "selected", {2, 5, 9}) ==
           "mpg|selected|wt|selection|2|5|9");
    assert(rlispstat::core::LinearGLMFitIdentityWithSelection(
               "mpg|all|wt", "all", {2, 5, 9}) == "mpg|all|wt");

    GeneralizedGLMState generalizedSignatureState;
    generalizedSignatureState.group = "cars";
    generalizedSignatureState.response = "am";
    generalizedSignatureState.family = "binomial";
    generalizedSignatureState.link = "logit";
    generalizedSignatureState.scope = "all";
    generalizedSignatureState.terms = {"wt", "cyl"};
    generalizedSignatureState.termTypes = signatureTypes;
    assert(generalizedSignatureState.familyKind == ModelFamilyKind::GeneralizedLinear);
    assert(GeneralizedGLMFitSignature(generalizedSignatureState) ==
           "cars|model-type=legacy_generalized|am|binomial|logit|all|wt|cyl|types|cyl=factor|wt=numeric|offset=");
    GeneralizedGLMState boundedSignatureState = generalizedSignatureState;
    boundedSignatureState.family = "beta";
    assert(GeneralizedGLMFitSignature(boundedSignatureState).find("|bounds=unset") !=
           std::string::npos);
    boundedSignatureState.responseBoundsConfigured = true;
    boundedSignatureState.responseLower = 0.0;
    boundedSignatureState.responseUpper = 100.0;
    assert(GeneralizedGLMFitSignature(boundedSignatureState).find("|bounds=0,100") !=
           std::string::npos);
    generalizedSignatureState.centeredPredictors.insert("wt");
    generalizedSignatureState.factorReferenceLevels["cyl"] = "6";
    assert(GeneralizedGLMFitSignature(generalizedSignatureState) ==
           "cars|model-type=legacy_generalized|am|binomial|logit|all|wt|cyl|types|cyl=factor|wt=numeric|offset=|centered|wt|references|cyl=6");
    generalizedSignatureState.centeredPredictors.clear();
    generalizedSignatureState.factorReferenceLevels.clear();
    generalizedSignatureState.binaryRegression = true;
    generalizedSignatureState.responseCoding.eventValue = "manual";
    generalizedSignatureState.responseCoding.referenceValue = "automatic";
    assert(GeneralizedGLMFitSignature(generalizedSignatureState) ==
           "cars|model-type=legacy_generalized|am|binomial|logit|all|binary|event=manual|reference=automatic|wt|cyl|types|cyl=factor|wt=numeric|offset=");
    generalizedSignatureState.dataScope = rlispstat::core::ExplicitAnalysisScope(
        "cars", {1, 3}, rlispstat::core::AnalysisScopeSourceKind::CurrentSelection,
        "Selected rows", 32);
    generalizedSignatureState.dataScopeCaptured = true;
    const std::string selectedRowsSignature =
        GeneralizedGLMFitSignature(generalizedSignatureState);
    generalizedSignatureState.dataScope.originalRowIds = {1, 4};
    assert(selectedRowsSignature != GeneralizedGLMFitSignature(generalizedSignatureState));
    generalizedSignatureState.dataScopeCaptured = false;

    GeneralizedGLMState binaryFull = generalizedSignatureState;
    GeneralizedGLMState binaryReduced = generalizedSignatureState;
    binaryFull.ok = binaryReduced.ok = true;
    binaryFull.rowsUsed = binaryReduced.rowsUsed = {1, 2, 3};
    binaryReduced.terms = {"wt"};
    std::string binaryReason;
    assert(BinaryModelsCompatibleForLikelihoodRatioTest(binaryReduced, binaryFull, &binaryReason));
    binaryFull.link = "probit";
    assert(!BinaryModelsCompatibleForLikelihoodRatioTest(binaryReduced, binaryFull, &binaryReason));
    assert(binaryReason.find("same binomial family and link") != std::string::npos);

    RegressionComparisonState comparisonSignatureState;
    comparisonSignatureState.id = "cmp1";
    comparisonSignatureState.group = "cars";
    comparisonSignatureState.response = "mpg";
    comparisonSignatureState.scope = "all";
    comparisonSignatureState.termTypes = signatureTypes;
    RegressionComparisonModel reducedModel;
    reducedModel.id = "m1";
    reducedModel.label = "Reduced";
    reducedModel.terms = {"wt"};
    reducedModel.centeredPredictors = {"wt"};
    RegressionComparisonModel fullModel;
    fullModel.id = "m2";
    fullModel.label = "Full";
    fullModel.response = "hp";
    fullModel.terms = {"wt", "cyl"};
    comparisonSignatureState.models = {reducedModel, fullModel};
    assert(RegressionComparisonFitSignature(comparisonSignatureState) ==
           "cmp1|cars|mpg|all|types|cyl=factor|wt=numeric|models|m1:Reduced:mpg+wt~centered+wt|m2:Full:hp+wt+cyl");

    DataFrameModel comparisonDataframe;
    comparisonDataframe.group = "cars";
    comparisonDataframe.rows = 3;
    DataColumn comparisonMpg;
    comparisonMpg.name = "mpg";
    comparisonMpg.type = "numeric";
    comparisonMpg.values = {"21", "22", "18"};
    DataColumn comparisonCyl;
    comparisonCyl.name = "cyl";
    comparisonCyl.type = "factor";
    comparisonCyl.values = {"4", "6", "8"};
    DataColumn comparisonWt;
    comparisonWt.name = "wt";
    comparisonWt.type = "numeric";
    comparisonWt.values = {"2.5", "3.0", "3.5"};
    DataColumn comparisonHp;
    comparisonHp.name = "hp";
    comparisonHp.type = "numeric";
    comparisonHp.values = {"110", "123", "175"};
    comparisonDataframe.columns = {
        comparisonMpg, comparisonCyl, comparisonWt, comparisonHp
    };
    comparisonSignatureState.models[0].centeredPredictors.insert("cyl");
    comparisonSignatureState.models[0].centeredPredictors.insert("missing");
    comparisonSignatureState.models[1].centeredPredictors.insert("cyl");
    comparisonSignatureState.termTypes.clear();
    assert(SynchronizeRegressionComparisonTermTypes(
        comparisonSignatureState, comparisonDataframe));
    assert(comparisonSignatureState.termTypes.size() == 1);
    assert(comparisonSignatureState.termTypes.at("cyl") == "factor");
    assert(comparisonSignatureState.models[0].centeredPredictors == std::set<std::string>{"wt"});
    assert(comparisonSignatureState.models[1].centeredPredictors.empty());
    comparisonSignatureState.models[0].centeredPredictors.insert("cyl");
    assert(SynchronizeRegressionComparisonTermTypes(
        comparisonSignatureState, comparisonDataframe));
    assert(comparisonSignatureState.models[0].centeredPredictors == std::set<std::string>{"wt"});
    assert(!SynchronizeRegressionComparisonTermTypes(
        comparisonSignatureState, comparisonDataframe));

    // Predictor interpretation and centering belong to the clicked model
    // column, not to the dataset or the comparison as a whole.  Both edits
    // must invalidate only that model and must survive interaction terms.
    RegressionComparisonState comparisonEditState;
    comparisonEditState.id = "comparison_edits";
    comparisonEditState.group = "comparison_edits";
    comparisonEditState.response = "y";
    comparisonEditState.termRows = {"(Intercept)", "x", "planet", "x:planet"};
    comparisonEditState.termTypes = {{"planet", "factor"}};
    comparisonEditState.seed.group = "comparison_edits";
    comparisonEditState.seed.variables = {
        {"y", {2.0, 4.0, 8.0, 11.0, 15.0, 20.0}},
        {"x", {1.0, 2.0, 1.0, 2.0, 1.0, 2.0}}
    };
    comparisonEditState.seed.variableMeta = {
        {"y", "numeric"}, {"x", "numeric"}, {"planet", "factor"}
    };
    RegressionComparisonModel editBase;
    editBase.id = "raw";
    editBase.label = "Raw";
    editBase.terms = {"x", "planet", "x:planet"};
    editBase.isStale = false;
    RegressionComparisonModel editAlternative = editBase;
    editAlternative.id = "edited";
    editAlternative.label = "Edited";
    comparisonEditState.models = {editBase, editAlternative};

    const std::string originalComparisonSignature =
        RegressionComparisonFitSignature(comparisonEditState);
    assert(ApplyRegressionComparisonTermType(comparisonEditState, 1, "x", "factor"));
    assert(RegressionComparisonModelTermType(comparisonEditState, 0, "x") == "numeric");
    assert(RegressionComparisonModelTermType(comparisonEditState, 1, "x") == "factor");
    assert(comparisonEditState.models[0].termTypeOverrides.empty());
    assert(comparisonEditState.models[1].termTypeOverrides.at("x") == "factor");
    assert(!comparisonEditState.models[0].isStale);
    assert(comparisonEditState.models[1].isStale);
    assert(comparisonEditState.models[0].modelVersion == 0);
    assert(comparisonEditState.models[1].modelVersion == 1);
    assert(rlispstat::core::ModelIncludesTerm(comparisonEditState.models[1], "x:planet"));
    assert(RegressionComparisonFitSignature(comparisonEditState) !=
           originalComparisonSignature);

    comparisonEditState.scope = "selected";
    comparisonEditState.models[1].factorReferenceLevels["x"] = "2";
    const auto standaloneSpecification =
        rlispstat::core::EffectiveRegressionComparisonModelSpecification(
            comparisonEditState, comparisonEditState.models[1]);
    assert(standaloneSpecification.response == "y");
    assert(standaloneSpecification.scope == "selected");
    assert(standaloneSpecification.terms == comparisonEditState.models[1].terms);
    assert(standaloneSpecification.termTypeOverrides.at("x") == "factor");
    assert(standaloneSpecification.factorReferenceLevels.at("x") == "2");
    assert(standaloneSpecification.termTypes.count("x") == 1);
    assert(standaloneSpecification.termTypes.count("planet") == 1);

    comparisonEditState.models[1].isStale = false;
    assert(ApplyRegressionComparisonTermType(comparisonEditState, 1, "x", "numeric"));
    assert(RegressionComparisonModelTermType(comparisonEditState, 1, "x") == "numeric");
    assert(comparisonEditState.models[1].termTypeOverrides.at("x") == "numeric");
    assert(ToggleRegressionComparisonPredictorCentering(comparisonEditState, 1, "x"));
    assert(comparisonEditState.models[0].centeredPredictors.empty());
    assert(comparisonEditState.models[1].centeredPredictors == std::set<std::string>{"x"});
    assert(rlispstat::core::ModelIncludesTerm(comparisonEditState.models[1], "x:planet"));
    assert(!ToggleRegressionComparisonPredictorCentering(comparisonEditState, 1, "planet"));
    assert(ToggleRegressionComparisonPredictorCentering(comparisonEditState, 1, "x"));
    assert(comparisonEditState.models[1].centeredPredictors.empty());

    RegressionComparisonState highCardinalityState = comparisonEditState;
    highCardinalityState.seed.variables.clear();
    rlispstat::core::NumericVariable almostContinuous;
    almostContinuous.name = "x";
    for (int value = 0; value < 60; ++value) almostContinuous.values.push_back(value);
    highCardinalityState.seed.variables.push_back(almostContinuous);
    highCardinalityState.models[1].termTypeOverrides["x"] = "numeric";
    std::string highCardinalityMessage;
    assert(!ApplyRegressionComparisonTermType(
        highCardinalityState, 1, "x", "factor", &highCardinalityMessage));
    assert(highCardinalityMessage.find("too many") != std::string::npos);
    assert(RegressionComparisonModelTermType(highCardinalityState, 1, "x") == "numeric");

    GLMCoefficientRow coefDefaults;
    assert(coefDefaults.termType == "numeric");
    assert(coefDefaults.rowType == "coefficient");
    assert(!std::isfinite(coefDefaults.pValue));
    GLMDiagnosticRow diagnosticDefaults;
    assert(diagnosticDefaults.row == 0);
    assert(!std::isfinite(diagnosticDefaults.residual));
    GLMFitSummary fitDefaults;
    assert(!fitDefaults.ok);
    assert(fitDefaults.coefficients.empty());
    assert(!std::isfinite(fitDefaults.globalF));
    GeneralizedGLMRow generalizedRowDefaults;
    assert(generalizedRowDefaults.statisticName == "z");
    assert(!std::isfinite(generalizedRowDefaults.estimate));
    GeneralizedGLMDiagnosticRow generalizedDiagnosticDefaults;
    assert(generalizedDiagnosticDefaults.row == 0);
    assert(!std::isfinite(generalizedDiagnosticDefaults.devianceResidual));
    GeneralizedGLMFitSummary generalizedFitDefaults;
    assert(!generalizedFitDefaults.ok);
    assert(generalizedFitDefaults.status == "Not fitted.");
    assert(generalizedFitDefaults.statisticName == "z");
    assert(!std::isfinite(generalizedFitDefaults.nullDeviance));
    std::vector<std::string> intPayload = {"3", "2", "4", "8"};
    std::size_t intCursor = 0;
    std::vector<int> intRows;
    assert(ReadIntVectorPayload(intPayload, intCursor, intRows));
    assert((intRows == std::vector<int>{2, 4, 8}));
    assert(intCursor == intPayload.size());

    std::vector<std::string> linearPayload = {
        "TRUE", "10", "2", "1", "8", "0.5", "0.4", "9.0", "0.01",
        "100", "50", "100", "6.25", "2.5", "2.5", "123.4", "130.1",
        "pooled warning",
        "2", "1", "3",
        "1", "4",
        "1",
        "x", "x", "numeric", "coefficient", "x", "", "",
        "1.5", "0.3", "0.2", "7.5", "0.002", "0.6", "0.125"
    };
    std::size_t linearCursor = 0;
    GLMFitSummary payloadFit;
    assert(ReadLinearFitPayload(linearPayload, linearCursor, payloadFit));
    assert(linearCursor == linearPayload.size());
    assert(payloadFit.ok);
    assert(payloadFit.n == 10);
    assert(payloadFit.rowsUsed == std::vector<int>({1, 3}));
    assert(payloadFit.rowsExcluded == std::vector<int>({4}));
    assert(payloadFit.coefficients.size() == 1);
    assert(payloadFit.coefficients[0].term == "x");
    assert(closeEnough(payloadFit.coefficients[0].estimate, 1.5));
    assert(closeEnough(payloadFit.coefficients[0].pValue, 0.002));
    assert(closeEnough(payloadFit.coefficients[0].partialR, 0.6));
    assert(closeEnough(payloadFit.coefficients[0].deltaR2, 0.125));
    const GLMCoefficientRow *payloadCoefficient = CoefficientForTerm(payloadFit, "x");
    assert(payloadCoefficient != nullptr);
    assert(closeEnough(payloadCoefficient->estimate, 1.5));
    assert(CoefficientForTerm(payloadFit.coefficients, "x") == payloadCoefficient);
    assert(CoefficientForTerm(payloadFit, "missing") == nullptr);
    std::vector<GLMCoefficientRow> sourceRows = CoefficientsForSourceTerm(payloadFit, "x");
    assert(sourceRows.size() == 1);
    assert(sourceRows[0].term == "x");
    assert(CoefficientsForSourceTerm(payloadFit, "missing").empty());

    std::vector<std::string> linearDiagnosticsPayload = {
        "2",
        "1", "10.0", "9.0", "1.0", "0.50", "0.60", "0.10", "0.01", "0.70",
        "3", "20.0", "18.0", "2.0", "1.50", "1.60", "0.20", "0.02", "1.20"
    };
    std::size_t linearDiagnosticsCursor = 0;
    std::vector<GLMDiagnosticRow> diagnosticsFromPayload;
    assert(ReadLinearDiagnosticsPayload(linearDiagnosticsPayload, linearDiagnosticsCursor, diagnosticsFromPayload));
    assert(linearDiagnosticsCursor == linearDiagnosticsPayload.size());
    assert(diagnosticsFromPayload.size() == 2);
    assert(diagnosticsFromPayload[0].row == 1);
    assert(closeEnough(diagnosticsFromPayload[0].fitted, 9.0));
    assert(closeEnough(diagnosticsFromPayload[1].cooksDistance, 0.02));
    linearDiagnosticsPayload.push_back("LINEAR_QQ_V1");
    linearDiagnosticsPayload.push_back("-0.6744897501960817\x1f-0.6744897501960817\x1f-0.6744897501960817");
    linearDiagnosticsPayload.push_back("0.6744897501960817\x1f""0.6744897501960817\x1f""0.6744897501960817");
    linearDiagnosticsCursor = 0;
    assert(ReadLinearDiagnosticsPayload(linearDiagnosticsPayload, linearDiagnosticsCursor, diagnosticsFromPayload));
    assert(linearDiagnosticsCursor == linearDiagnosticsPayload.size());
    assert(closeEnough(diagnosticsFromPayload[0].qqRawQuantile, -0.6744897501960817));
    assert(closeEnough(diagnosticsFromPayload[1].qqStudentizedQuantile, 0.6744897501960817));
    std::vector<std::string> truncatedLinearDiagnosticsPayload = linearDiagnosticsPayload;
    truncatedLinearDiagnosticsPayload.pop_back();
    linearDiagnosticsCursor = 0;
    assert(!ReadLinearDiagnosticsPayload(truncatedLinearDiagnosticsPayload, linearDiagnosticsCursor, diagnosticsFromPayload));

    std::vector<std::string> linearMIDiagnosticsPayload = {
        "LINEAR_MI_DIAGNOSTICS_V1", "2",
        "1", "1", "10.0", "9.0", "1.0", "0.50", "0.60", "0.10", "0.01", "0.70",
        "1", "1", "10.0", "8.5", "1.5", "0.75", "0.80", "0.12", "0.02", "0.87"
    };
    std::size_t linearMIDiagnosticsCursor = 0;
    GLMFitSummary linearMIFit;
    assert(ReadLinearMIDiagnosticsPayload(
        linearMIDiagnosticsPayload, linearMIDiagnosticsCursor, linearMIFit));
    assert(linearMIDiagnosticsCursor == linearMIDiagnosticsPayload.size());
    assert(linearMIFit.diagnosticsByImputation.size() == 2);
    assert(linearMIFit.diagnosticsByImputation[0].size() == 1);
    assert(closeEnough(linearMIFit.diagnosticsByImputation[0][0].fitted, 9.0));
    assert(closeEnough(linearMIFit.diagnosticsByImputation[1][0].fitted, 8.5));
    assert(linearMIFit.diagnostics.size() == 1);
    assert(closeEnough(linearMIFit.diagnostics[0].fitted, 9.0));
    std::vector<std::string> truncatedLinearMIDiagnostics = linearMIDiagnosticsPayload;
    truncatedLinearMIDiagnostics.pop_back();
    linearMIDiagnosticsCursor = 0;
    assert(!ReadLinearMIDiagnosticsPayload(
        truncatedLinearMIDiagnostics, linearMIDiagnosticsCursor, linearMIFit));

    std::vector<std::string> linearDesignPayload = {
        "2",
        "(Intercept)", "x",
        "1.0", "2.0",
        "0.25", "0.01", "0.01", "0.04"
    };
    std::size_t linearDesignCursor = 0;
    GLMFitSummary designFit;
    assert(ReadLinearDesignPayload(linearDesignPayload, linearDesignCursor, designFit));
    assert(linearDesignCursor == linearDesignPayload.size());
    assert(designFit.designLabels == std::vector<std::string>({"(Intercept)", "x"}));
    assert(designFit.beta.size() == 2);
    assert(closeEnough(designFit.beta[1], 2.0));
    assert(designFit.covariance.size() == 2);
    assert(closeEnough(designFit.covariance[1][1], 0.04));
    std::vector<std::string> codedLinearDesignPayload = linearDesignPayload;
    codedLinearDesignPayload.insert(codedLinearDesignPayload.end(), {
        "FACTOR_CODINGS", "1",
        "country", "Finland", "2", "1",
        "Finland", "Greece", "Greece",
        "0", "1",
        "PREDICTOR_CENTERS", "1", "x", "2.5"
    });
    linearDesignCursor = 0;
    assert(ReadLinearDesignPayload(codedLinearDesignPayload, linearDesignCursor, designFit));
    assert(linearDesignCursor == codedLinearDesignPayload.size());
    assert(designFit.factorCodings.size() == 1);
    assert(designFit.factorCodings[0].variable == "country");
    assert(designFit.factorCodings[0].referenceLevel == "Finland");
    assert(designFit.factorCodings[0].levels ==
           std::vector<std::string>({"Finland", "Greece"}));
    assert(designFit.predictorCenters.size() == 1);
    assert(closeEnough(designFit.predictorCenters.at("x"), 2.5));
    ScenarioVariableInfo countryScenario;
    assert(rlispstat::core::ApplyFittedFactorCoding(designFit, "country", countryScenario));
    assert(countryScenario.factorCoding["Finland"] == std::vector<double>({0.0}));
    assert(countryScenario.factorCoding["Greece"] == std::vector<double>({1.0}));
    std::vector<std::string> truncatedLinearDesignPayload = linearDesignPayload;
    truncatedLinearDesignPayload.pop_back();
    linearDesignCursor = 0;
    assert(!ReadLinearDesignPayload(truncatedLinearDesignPayload, linearDesignCursor, designFit));

    payloadFit.coefficients[0].displayLabel = "Predictor x";
    payloadFit.coefficients[0].sourceTerm = "source_x";
    assert(RegressionRowDisplayLabel(&payloadFit.coefficients[0], "fallback") == "Predictor x");
    assert(RegressionRowSourceTerm(&payloadFit.coefficients[0], "fallback") == "source_x");
    payloadFit.coefficients[0].displayLabel = "";
    payloadFit.coefficients[0].sourceTerm = "";
    assert(RegressionRowDisplayLabel(&payloadFit.coefficients[0], "fallback") == "x");
    assert(RegressionRowSourceTerm(&payloadFit.coefficients[0], "fallback") == "fallback");
    payloadFit.coefficients[0].term = "group=B";
    assert(RegressionRowSourceTerm(&payloadFit.coefficients[0], "fallback") == "group");
    payloadFit.coefficients[0].term = "x";
    payloadFit.coefficients[0].sourceTerm = "x";
    assert(RegressionCoefficientDisplay(payloadFit, {"x"}, "x") == "1.5000**");
    assert(RegressionCoefficientDisplay(payloadFit, {}, "x") == "\u2014");
    GLMFitSummary parentFit = payloadFit;
    parentFit.coefficients[0].rowType = "factor_parent";
    assert(RegressionCoefficientDisplay(parentFit, {"x"}, "x").empty());
    parentFit.coefficients[0].rowType = "term_parent";
    assert(RegressionCoefficientDisplay(parentFit, {"x"}, "x").empty());
    GLMFitSummary inferredSourceFit;
    inferredSourceFit.ok = true;
    GLMCoefficientRow inferredInteraction;
    inferredInteraction.term = "cyl=6:am";
    inferredInteraction.sourceTerm = "";
    inferredInteraction.estimate = 2.0;
    inferredInteraction.pValue = 0.2;
    inferredSourceFit.coefficients.push_back(inferredInteraction);
    assert(RegressionCoefficientDisplay(inferredSourceFit, {"cyl:am"}, "cyl=6:am") == "2.0000");
    assert(RegressionCoefficientDisplay(inferredSourceFit, {"cyl"}, "cyl=6:am") == "\u2014");

    std::vector<std::string> generalizedPayload = {
        "1",
        "x", "x", "numeric", "coefficient", "x", "", "",
        "2.5", "0.5", "z", "5.0", "0.001"
    };
    std::size_t generalizedCursor = 0;
    std::vector<GeneralizedGLMRow> generalizedPayloadRows;
    assert(ReadGeneralizedRowsPayload(generalizedPayload, generalizedCursor, generalizedPayloadRows));
    assert(generalizedCursor == generalizedPayload.size());
    assert(generalizedPayloadRows.size() == 1);
    assert(generalizedPayloadRows[0].term == "x");
    assert(generalizedPayloadRows[0].statisticName == "z");
    assert(closeEnough(generalizedPayloadRows[0].statistic, 5.0));
    GeneralizedGLMRow *mutableGeneralizedRow = GeneralizedRowForTerm(generalizedPayloadRows, "x");
    assert(mutableGeneralizedRow != nullptr);
    mutableGeneralizedRow->displayLabel = "Predictor x";
    mutableGeneralizedRow->sourceTerm = "source_x";
    const std::vector<GeneralizedGLMRow> constGeneralizedRows = generalizedPayloadRows;
    const GeneralizedGLMRow *constGeneralizedRow = GeneralizedRowForTerm(constGeneralizedRows, "x");
    assert(constGeneralizedRow != nullptr);
    assert(GeneralizedRowForTerm(constGeneralizedRows, "missing") == nullptr);
    assert(GeneralizedRowDisplayLabel(constGeneralizedRow, "fallback") == "Predictor x");
    assert(GeneralizedRowSourceTerm(constGeneralizedRow, "fallback") == "source_x");
    GeneralizedGLMRow naGeneralizedRow = *constGeneralizedRow;
    naGeneralizedRow.displayLabel = "NA";
    naGeneralizedRow.sourceTerm = "NA";
    assert(GeneralizedRowDisplayLabel(&naGeneralizedRow, "fallback") == "fallback");
    assert(GeneralizedRowSourceTerm(&naGeneralizedRow, "fallback") == "fallback");
    naGeneralizedRow.term = "group=B";
    assert(GeneralizedRowSourceTerm(&naGeneralizedRow, "fallback") == "group");
    mutableGeneralizedRow->displayLabel = "x";
    mutableGeneralizedRow->sourceTerm = "x";
    assert(GeneralizedCoefficientDisplay(generalizedPayloadRows, true, {"x"}, "x") == "2.500**");
    assert(GeneralizedCoefficientDisplay(generalizedPayloadRows, false, {"x"}, "x") == "\u2014");
    assert(GeneralizedCoefficientDisplay(generalizedPayloadRows, true, {}, "x") == "\u2014");
    GeneralizedGLMRow inferredGeneralizedInteraction;
    inferredGeneralizedInteraction.term = "cyl=6:am";
    inferredGeneralizedInteraction.sourceTerm = "";
    inferredGeneralizedInteraction.estimate = 2.0;
    inferredGeneralizedInteraction.pValue = 0.2;
    assert(GeneralizedCoefficientDisplay({inferredGeneralizedInteraction}, true, {"cyl:am"}, "cyl=6:am") == "2.000");

    std::vector<std::string> generalizedFitPayload = {
        "TRUE", "12", "1", "30.5", "18.25", "9", "42.0", "49.5",
        "1.1", "-17.2", "z", "Pooled generalized model",
        "3", "1", "2", "3",
        "1", "4",
        "1",
        "x", "x", "numeric", "coefficient", "x", "", "",
        "2.5", "0.5", "z", "5.0", "0.001"
    };
    std::size_t generalizedFitCursor = 0;
    GeneralizedGLMFitSummary generalizedFit;
    assert(ReadGeneralizedFitPayload(generalizedFitPayload, generalizedFitCursor, generalizedFit));
    assert(generalizedFitCursor == generalizedFitPayload.size());
    assert(generalizedFit.ok);
    assert(generalizedFit.n == 12);
    assert(generalizedFit.rowsUsed == std::vector<int>({1, 2, 3}));
    assert(generalizedFit.rowsExcluded == std::vector<int>({4}));
    assert(generalizedFit.rows.size() == 1);
    assert(generalizedFit.rows[0].term == "x");
    assert(closeEnough(generalizedFit.residualDeviance, 18.25));
    assert(generalizedFit.status == "Pooled generalized model");

    std::size_t generalizedStateCursor = 0;
    GeneralizedGLMState generalizedStateFromPayload;
    assert(ReadGeneralizedStatePayload(generalizedFitPayload, generalizedStateCursor, generalizedStateFromPayload));
    assert(generalizedStateCursor == generalizedFitPayload.size());
    assert(generalizedStateFromPayload.ok);
    std::vector<std::string> generalizedQuantilePayload = generalizedFitPayload;
    generalizedQuantilePayload.insert(generalizedQuantilePayload.end(), {
        "GENERALIZED_DIAGNOSTICS_V2", "2",
        "1", "0", "0.2", "-1", "-0.7", "-0.2", "-1.5", "-1.2", "-1", "-1.7", "-1.9", "0.1", "0.02",
        "2", "1", "0.8", "1", "0.6", "0.2", "0.8", "1.1", "0.9", "1.3", "1.5", "0.25", "0.08",
        "GENERALIZED_QQ_V1", "2",
        "-0.6744897501960817\x1f-0.6744897501960817\x1f-0.6744897501960817\x1f-0.6744897501960817\x1f-0.6744897501960817\x1f-0.6744897501960817\x1f-0.6744897501960817",
        "0.6744897501960817\x1f""0.6744897501960817\x1f""0.6744897501960817\x1f""0.6744897501960817\x1f""0.6744897501960817\x1f""0.6744897501960817\x1f""0.6744897501960817",
        "0"
    });
    generalizedStateCursor = 0;
    assert(ReadGeneralizedStatePayload(
        generalizedQuantilePayload, generalizedStateCursor, generalizedStateFromPayload));
    assert(generalizedStateCursor == generalizedQuantilePayload.size());
    assert(closeEnough(generalizedStateFromPayload.diagnostics[0]
        .qqTheoreticalQuantiles.at("deviance"), -0.6744897501960817));
    assert(generalizedStateFromPayload.n == 12);
    assert(generalizedStateFromPayload.excluded == 1);
    assert(closeEnough(generalizedStateFromPayload.nullDeviance, 30.5));
    assert(closeEnough(generalizedStateFromPayload.residualDeviance, 18.25));
    assert(generalizedStateFromPayload.rowsUsed == std::vector<int>({1, 2, 3}));
    assert(generalizedStateFromPayload.rowsExcluded == std::vector<int>({4}));
    assert(generalizedStateFromPayload.rows.size() == 1);
    assert(generalizedStateFromPayload.rows[0].term == "x");
    assert(closeEnough(generalizedStateFromPayload.rows[0].estimate, 2.5));
    assert(generalizedStateFromPayload.converged);
    assert(generalizedStateFromPayload.modelType ==
           rlispstat::core::StatisticalModelType::LegacyGeneralized);

    std::vector<std::string> typedGeneralizedPayload = generalizedFitPayload;
    typedGeneralizedPayload.insert(typedGeneralizedPayload.end(),
                                   {"MODEL_TYPE_V1", "proportion"});
    std::size_t typedGeneralizedCursor = 0;
    GeneralizedGLMState typedGeneralizedState;
    assert(ReadGeneralizedStatePayload(
        typedGeneralizedPayload, typedGeneralizedCursor, typedGeneralizedState));
    assert(typedGeneralizedCursor == typedGeneralizedPayload.size());
    assert(typedGeneralizedState.modelType ==
           rlispstat::core::StatisticalModelType::Proportion);

    std::vector<std::string> generalizedMIStatePayload = generalizedFitPayload;
    generalizedMIStatePayload.insert(generalizedMIStatePayload.end(), {
        "GENERALIZED_MI_DIAGNOSTICS_V2", "2",
        "1", "1", "event", "1", "1", "0.80", "1.386", "0.20", "0.25", "0.22", "0.23", "0.24", "0.10", "0.01",
        "1", "1", "event", "1", "1", "0.75", "1.099", "0.30", "0.33", "0.31", "0.32", "0.34", "0.11", "0.02"
    });
    std::size_t generalizedMICursor = 0;
    GeneralizedGLMState generalizedMIState;
    assert(ReadGeneralizedStatePayload(
        generalizedMIStatePayload, generalizedMICursor, generalizedMIState));
    assert(generalizedMICursor == generalizedMIStatePayload.size());
    assert(generalizedMIState.diagnosticsByImputation.size() == 2);
    assert(generalizedMIState.diagnosticsByImputation[0].size() == 1);
    assert(closeEnough(generalizedMIState.diagnosticsByImputation[0][0].fitted, 0.80));
    assert(closeEnough(generalizedMIState.diagnosticsByImputation[0][0].standardizedResidual, 0.23));
    assert(closeEnough(generalizedMIState.diagnosticsByImputation[0][0].studentizedResidual, 0.24));
    assert(closeEnough(generalizedMIState.diagnosticsByImputation[1][0].fitted, 0.75));
    assert(generalizedMIState.diagnostics.size() == 1);

    auto compactDiagnostic = [](const std::vector<std::string> &values) {
        std::string encoded;
        for (const auto &value : values) {
            if (!encoded.empty()) encoded.push_back('\x1f');
            encoded += value;
        }
        return encoded;
    };
    std::vector<std::string> compactMIPayload = generalizedFitPayload;
    compactMIPayload.insert(compactMIPayload.end(), {
        "GENERALIZED_MI_DIAGNOSTICS_V4", "2", "1",
        compactDiagnostic({"1", "event", "1", "1", "0.80", "1.386",
            "0.15", "0.20", "0.25", "0.22", "0.23", "0.24",
            "0.26", "0.10", "0.01"}),
        "1",
        compactDiagnostic({"1", "event", "1", "1", "0.75", "1.099",
            "0.16", "0.30", "0.33", "0.31", "0.32", "0.34",
            "0.36", "0.11", "0.02"})
    });
    std::size_t compactMICursor = 0;
    GeneralizedGLMState compactMIState;
    assert(ReadGeneralizedStatePayload(
        compactMIPayload, compactMICursor, compactMIState));
    assert(compactMICursor == compactMIPayload.size());
    assert(compactMIState.diagnosticsByImputation.size() == 2);
    assert(closeEnough(compactMIState.diagnosticsByImputation[0][0].fitted, 0.80));
    assert(closeEnough(compactMIState.diagnosticsByImputation[0][0].dunnSmythResidual, 0.15));
    assert(closeEnough(compactMIState.diagnosticsByImputation[1][0].fitted, 0.75));
    compactMIPayload.back().append(1, '\x1f');
    std::size_t malformedMICursor = 0;
    GeneralizedGLMState malformedMIState;
    assert(!ReadGeneralizedStatePayload(
        compactMIPayload, malformedMICursor, malformedMIState));

    std::vector<std::string> boundedDistributionPayload = generalizedFitPayload;
    boundedDistributionPayload.insert(boundedDistributionPayload.end(), {
        "BOUNDED_COUNT_DISTRIBUTION_V1",
        "3", "0", "2", "2.5", "1", "5", "5.5", "2", "3", "2",
        "2",
        "2", "0", "2", "2.4", "1", "5", "5.6",
        "2", "0", "3", "2.6", "1", "4", "5.4",
        "MODEL_TYPE_V1", "count"
    });
    std::size_t boundedDistributionCursor = 0;
    GeneralizedGLMState boundedDistributionState;
    assert(ReadGeneralizedStatePayload(
        boundedDistributionPayload, boundedDistributionCursor,
        boundedDistributionState));
    assert(boundedDistributionCursor == boundedDistributionPayload.size());
    assert(boundedDistributionState.boundedCountDistribution.size() == 3);
    assert(closeEnough(
        boundedDistributionState.boundedCountDistribution[1].predictedFrequency,
        5.5));
    assert(boundedDistributionState.boundedCountDistributionsByImputation.size() == 2);
    assert(closeEnough(
        boundedDistributionState.boundedCountDistributionsByImputation[1][0]
            .observedFrequency,
        3.0));
    assert(boundedDistributionState.modelType ==
           rlispstat::core::StatisticalModelType::Count);

    std::vector<std::string> generalizedDiagnosticStatePayload = generalizedFitPayload;
    generalizedDiagnosticStatePayload.insert(generalizedDiagnosticStatePayload.end(), {
        "GENERALIZED_DIAGNOSTICS_V1", "1",
        "1", "2", "1.5", "0.4", "0.2", "0.3", "0.4", "0.5", "0.6", "0.1", "0.01"
    });
    std::size_t generalizedDiagnosticCursor = 0;
    GeneralizedGLMState generalizedDiagnosticState;
    assert(ReadGeneralizedStatePayload(
        generalizedDiagnosticStatePayload, generalizedDiagnosticCursor,
        generalizedDiagnosticState));
    assert(generalizedDiagnosticCursor == generalizedDiagnosticStatePayload.size());
    assert(generalizedDiagnosticState.diagnostics.size() == 1);
    assert(closeEnough(generalizedDiagnosticState.diagnostics[0].standardizedResidual, 0.5));
    assert(closeEnough(generalizedDiagnosticState.diagnostics[0].studentizedResidual, 0.6));

    std::vector<std::string> generalizedCIStatePayload = generalizedFitPayload;
    generalizedCIStatePayload.insert(generalizedCIStatePayload.end(), {
        "GENERALIZED_CI_V1", "1", "1.520018", "3.479982"
    });
    std::size_t generalizedCICursor = 0;
    GeneralizedGLMState generalizedCIState;
    assert(ReadGeneralizedStatePayload(generalizedCIStatePayload, generalizedCICursor, generalizedCIState));
    assert(generalizedCICursor == generalizedCIStatePayload.size());
    assert(closeEnough(generalizedCIState.rows[0].ciLower, 1.520018));
    assert(closeEnough(generalizedCIState.rows[0].ciUpper, 3.479982));
    generalizedCIState.family = "gaussian";
    const std::string generalizedCIText = GeneralizedGLMFormattedOutputText(generalizedCIState);
    assert(generalizedCIText.find("95% CI") != std::string::npos);
    assert(generalizedCIText.find("[1.520, 3.480]") != std::string::npos);
    std::vector<std::string> generalizedFamilyPayload = generalizedCIStatePayload;
    generalizedFamilyPayload.insert(generalizedFamilyPayload.end(), {
        "GENERALIZED_FAMILY_DIAGNOSTICS_V1", "2",
        "gamma_cv", "0.6", "pearson_dispersion_ratio", "2.3"
    });
    std::size_t generalizedFamilyCursor = 0;
    GeneralizedGLMState generalizedFamilyState;
    assert(ReadGeneralizedStatePayload(generalizedFamilyPayload,
        generalizedFamilyCursor, generalizedFamilyState));
    assert(generalizedFamilyCursor == generalizedFamilyPayload.size());
    assert(closeEnough(generalizedFamilyState.familyDiagnostics.at("gamma_cv"), 0.6));
    assert(closeEnough(generalizedFamilyState.familyDiagnostics.at("pearson_dispersion_ratio"), 2.3));

    std::vector<std::string> generalizedMetaStatePayload = generalizedCIStatePayload;
    generalizedMetaStatePayload.insert(generalizedMetaStatePayload.end(), {
        "GENERALIZED_META_V1", "TRUE", "4", "FALSE", "2", "2", "FALSE",
        "1", "A captured GLM warning"
    });
    std::size_t generalizedMetaCursor = 0;
    GeneralizedGLMState generalizedMetaState;
    assert(ReadGeneralizedStatePayload(generalizedMetaStatePayload, generalizedMetaCursor, generalizedMetaState));
    assert(generalizedMetaCursor == generalizedMetaStatePayload.size());
    assert(generalizedMetaState.converged);
    assert(generalizedMetaState.iterations == 4);
    assert(!generalizedMetaState.boundary);
    assert(generalizedMetaState.rank == 2);
    assert(generalizedMetaState.parameterCount == 2);
    assert(!generalizedMetaState.rankDeficient);
    assert(generalizedMetaState.warnings == std::vector<std::string>({"A captured GLM warning"}));

    std::vector<std::string> countStatePayload = generalizedMetaStatePayload;
    countStatePayload.insert(countStatePayload.end(), {
        "COUNT_V4", "negative_binomial", "person_time", "", "nan", "TRUE", "TRUE", "2.75",
        "2.75", "2.75", "2.75",
        "1", "12.182494", "4.572219", "32.465201",
        "1", "7", "4", "5.25", "1.658228", "0.22", "0.24", "0.01", "0.025", "0.03", "0.08", "0.03"
    });
    std::size_t countStateCursor = 0;
    GeneralizedGLMState countState;
    assert(ReadGeneralizedStatePayload(countStatePayload, countStateCursor, countState));
    assert(countStateCursor == countStatePayload.size());
    assert(countState.countRegression);
    assert(countState.modelType == rlispstat::core::StatisticalModelType::Count);
    assert(countState.countDistribution == rlispstat::core::CountDistribution::NegativeBinomial);
    assert(countState.exposure == "person_time");
    assert(countState.trialsVariable.empty());
    assert(!std::isfinite(countState.trialsConstant));
    assert(countState.likelihoodAvailable);
    assert(countState.likelihoodRatioAvailable);
    assert(closeEnough(countState.theta, 2.75));
    assert(closeEnough(countState.thetaDescriptiveMean, 2.75));
    assert(closeEnough(countState.thetaDescriptiveMin, 2.75));
    assert(closeEnough(countState.thetaDescriptiveMax, 2.75));
    assert(closeEnough(countState.rows[0].rateRatio, std::exp(2.5)));
    assert(closeEnough(countState.rows[0].rateRatioLower, 4.572219, 1e-5));
    assert(closeEnough(countState.rows[0].rateRatioUpper, 32.465201, 1e-5));
    assert(countState.diagnostics.size() == 1);
    assert(countState.diagnostics[0].row == 7);
    assert(closeEnough(countState.diagnostics[0].observed, 4.0));
    assert(closeEnough(countState.diagnostics[0].fitted, 5.25));

    GeneralizedGLMState groupedBinomialState = countState;
    groupedBinomialState.countDistribution = CountDistribution::BinomialTrials;
    groupedBinomialState.exposure.clear();
    groupedBinomialState.trialsConstant = 24.0;
    groupedBinomialState.dispersion = 1.27;
    const std::string groupedBinomialText =
        GeneralizedGLMFormattedOutputText(groupedBinomialState);
    assert(groupedBinomialText.find("Binomial (successes/trials)") !=
           std::string::npos);
    assert(groupedBinomialText.find("Trials  24") != std::string::npos);
    assert(groupedBinomialText.find("Odds ratio / 95% CI") != std::string::npos);
    assert(groupedBinomialText.find("Pearson dispersion ratio: 1.270") !=
           std::string::npos);

    GeneralizedGLMState betaBinomialState = groupedBinomialState;
    betaBinomialState.countDistribution = CountDistribution::BetaBinomial;
    betaBinomialState.dispersion = 4.25;
    const std::string betaBinomialText =
        GeneralizedGLMFormattedOutputText(betaBinomialState);
    assert(betaBinomialText.find("Beta-Binomial") != std::string::npos);
    assert(betaBinomialText.find("Beta-binomial precision (phi): 4.250") !=
           std::string::npos);
    betaBinomialState.familyDiagnostics["beta_binomial_rho"] = 0.1904761905;
    betaBinomialState.familyDiagnostics["beta_binomial_variance_inflation"] = 5.380952381;
    const auto betaDiagnostics = GeneralizedFamilyDiagnosticRows(betaBinomialState);
    assert(betaDiagnostics.size() == 2);
    assert(betaDiagnostics[0].statistic == "Intra-trial correlation (rho)");
    assert(betaDiagnostics[1].display.find("5.38×") != std::string::npos);
    assert(ExplainModelStatistic(GeneralizedModelStatisticContext(
        betaBinomialState, betaDiagnostics[1].statistic)).find("actual trial count") != std::string::npos);
    assert(ExplainModelStatistic(GeneralizedModelStatisticContext(
        betaBinomialState, betaDiagnostics[0].statistic)).find("1 / (1 + phi)") != std::string::npos);

    GeneralizedGLMState poissonDiagnosticState = countState;
    poissonDiagnosticState.countDistribution = CountDistribution::Poisson;
    poissonDiagnosticState.familyDiagnostics["pearson_dispersion_ratio"] = 2.31;
    const auto poissonDiagnostics = GeneralizedFamilyDiagnosticRows(poissonDiagnosticState);
    assert(poissonDiagnostics.size() == 1);
    assert(ExplainModelStatistic(GeneralizedModelStatisticContext(
        poissonDiagnosticState, poissonDiagnostics[0].statistic)).find("Pearson residuals") != std::string::npos);

    GeneralizedGLMState negativeBinomialDiagnosticState = countState;
    negativeBinomialDiagnosticState.countDistribution = CountDistribution::NegativeBinomial;
    negativeBinomialDiagnosticState.familyDiagnostics["negative_binomial_mean_fitted_count"] = 4.1;
    negativeBinomialDiagnosticState.familyDiagnostics["negative_binomial_variance_inflation"] = 2.18;
    const auto negativeBinomialDiagnostics = GeneralizedFamilyDiagnosticRows(negativeBinomialDiagnosticState);
    assert(negativeBinomialDiagnostics.size() == 1);
    assert(ExplainModelStatistic(GeneralizedModelStatisticContext(
        negativeBinomialDiagnosticState, negativeBinomialDiagnostics[0].statistic)).find("1 + mu / theta") != std::string::npos);

    GeneralizedGLMState gammaDiagnosticState = countState;
    gammaDiagnosticState.countRegression = false;
    gammaDiagnosticState.family = "Gamma";
    gammaDiagnosticState.familyDiagnostics["gamma_cv"] = 0.6;
    const auto gammaDiagnostics = GeneralizedFamilyDiagnosticRows(gammaDiagnosticState);
    assert(gammaDiagnostics.size() == 1);
    assert(ExplainModelStatistic(GeneralizedModelStatisticContext(
        gammaDiagnosticState, gammaDiagnostics[0].statistic)).find("sqrt(phi)") != std::string::npos);

    GeneralizedGLMState waldState = betaBinomialState;
    waldState.multipleImputation = true;
    waldState.statisticName = "t";
    waldState.rows.clear();
    waldState.termTests.clear();
    const auto addWald = [&](const std::string &source, const std::string &testTerm,
                             double statistic, double p, int df) {
        GeneralizedGLMRow row;
        row.term = source;
        row.sourceTerm = source;
        row.rowType = source.find(':') == std::string::npos
            ? "factor_parent" : "term_parent";
        waldState.rows.push_back(row);
        rlispstat::core::GlobalTermTestRow test;
        test.term = testTerm;
        test.method = "Rubin Wald chi-square";
        test.statistic = statistic;
        test.pValue = p;
        test.df = df;
        waldState.termTests.push_back(test);
    };
    addWald("country", "country", 1.760, .624, 3);
    addWald("group", "group", .061, .805, 1);
    addWald("country:group", "group:country", 1.586, .663, 3);
    addWald("group:genero2", "genero2:group", .015, .904, 1);
    addWald("country:group:genero2", "genero2:country:group", 1.446, .695, 3);
    for (std::size_t index = 0; index < waldState.rows.size(); ++index) {
        const auto *test = GeneralizedGlobalTermTestForPresentationRow(
            waldState, waldState.rows[index]);
        assert(test && test->statistic == waldState.termTests[index].statistic);
        assert(test->pValue == waldState.termTests[index].pValue);
        const auto context = GeneralizedModelStatisticContext(waldState, "stat", (int)index);
        assert(context.statistic == "Pooled Wald χ²");
        assert(ExplainModelStatistic(context).find("jointly tests") != std::string::npos);
    }
    GeneralizedGLMRow child;
    child.term = "countryFinland:groupTreatment";
    child.sourceTerm = "country:group";
    child.rowType = "coefficient";
    child.statistic = 2.345;
    assert(GeneralizedGlobalTermTestForPresentationRow(waldState, child) == nullptr);
    assert(GeneralizedGlobalTermTestPooledWaldFootnote(waldState).find("† Parent-term statistics") != std::string::npos);

    GeneralizedGLMState pooledNegativeBinomial = countState;
    pooledNegativeBinomial.multipleImputation = true;
    pooledNegativeBinomial.likelihoodAvailable = false;
    pooledNegativeBinomial.theta = NAN;
    pooledNegativeBinomial.thetaDescriptiveMean = 3.42;
    pooledNegativeBinomial.thetaDescriptiveMin = 3.18;
    pooledNegativeBinomial.thetaDescriptiveMax = 3.71;
    pooledNegativeBinomial.dispersion = 1.0;
    const std::string pooledNegativeBinomialText =
        GeneralizedGLMFormattedOutputText(pooledNegativeBinomial);
    assert(pooledNegativeBinomialText.find("Theta: 3.420 — mean across imputations") !=
           std::string::npos);
    assert(pooledNegativeBinomialText.find("range 3.180–3.710") != std::string::npos);
    assert(pooledNegativeBinomialText.find("quasi-Poisson") == std::string::npos);
    assert(pooledNegativeBinomialText.find("Dispersion") == std::string::npos);
    assert(closeEnough(countState.diagnostics[0].standardizedResidual, 0.025));
    assert(closeEnough(countState.diagnostics[0].studentizedResidual, 0.03));
    const std::string countOutput = GeneralizedGLMFormattedOutputText(countState);
    assert(countOutput.find("Count Model") != std::string::npos);
    assert(countOutput.find("Negative binomial") != std::string::npos);
    assert(countOutput.find("Exposure  person_time (log offset)") != std::string::npos);
    assert(countOutput.find("Rate ratio / 95% CI") != std::string::npos);
    assert(countOutput.find("Rate ratio 95% CI") == std::string::npos);

    std::vector<std::string> binaryStatePayload = generalizedMetaStatePayload;
    binaryStatePayload.insert(binaryStatePayload.end(), {
        "BINARY_V3", "yes", "no", "7", "5", "1", "4.2", "0.04",
        "0.2", "0.3", "0.4", "0.8", "0", "1", "TRUE",
        "6", "FALSE", "1", "1", "FALSE",
        "1", "Very unstable coefficient estimate",
        "1", "1.5", "3.5", "12", "4", "Inf",
        "1", "x", "1", "4.2", "0.04",
        "0"
    });
    std::size_t binaryStateCursor = 0;
    GeneralizedGLMState binaryState;
    assert(ReadGeneralizedStatePayload(binaryStatePayload, binaryStateCursor, binaryState));
    assert(binaryStateCursor == binaryStatePayload.size());
    assert(binaryState.binaryRegression);
    assert(binaryState.modelType == rlispstat::core::StatisticalModelType::Binary);
    assert(binaryState.responseCoding.eventValue == "yes");
    assert(binaryState.responseCoding.referenceValue == "no");
    assert(binaryState.iterations == 6);
    assert(!binaryState.boundary);
    assert(binaryState.rank == 1);
    assert(binaryState.parameterCount == 1);
    assert(!binaryState.rankDeficient);
    assert(binaryState.warnings.size() == 1);
    assert(binaryState.termTests.size() == 1);
    assert(closeEnough(binaryState.rows[0].ciLower, 1.5));
    assert(closeEnough(binaryState.rows[0].ciUpper, 3.5));
    assert(std::isinf(binaryState.rows[0].oddsRatioUpper));

    std::vector<std::string> truncatedLinearPayload = linearPayload;
    truncatedLinearPayload.pop_back();
    linearCursor = 0;
    assert(!ReadLinearFitPayload(truncatedLinearPayload, linearCursor, payloadFit));
    std::vector<std::string> truncatedGeneralizedFitPayload = generalizedFitPayload;
    truncatedGeneralizedFitPayload.pop_back();
    generalizedFitCursor = 0;
    assert(!ReadGeneralizedFitPayload(truncatedGeneralizedFitPayload, generalizedFitCursor, generalizedFit));
    std::string generalizedScript = GeneralizedGLMRScript();
    assert(generalizedScript.find("stats::glm") != std::string::npos);
    assert(generalizedScript.find(".rls_model_data_for_term_types") != std::string::npos);
    assert(generalizedScript.find(".rls_model_coefficient_display_rows") != std::string::npos);
    assert(generalizedScript.find("META") != std::string::npos);
    assert(generalizedScript.find("DIAG") != std::string::npos);

    LinearOlsInput input;
    input.designMatrix = {
        {1.0, 0.0},
        {1.0, 1.0},
        {1.0, 2.0},
        {1.0, 3.0},
        {1.0, 4.0}
    };
    input.response = {1.0, 3.2, 4.8, 7.1, 8.9};
    input.rowIds = {1, 2, 3, 4, 5};
    LinearOlsResult fit = FitLinearOls(input);
    assert(fit.ok);
    assert(fit.n == 5);
    assert(fit.dfModel == 1);
    assert(fit.dfResidual == 3);
    assert(closeEnough(fit.beta[0], 1.06));
    assert(closeEnough(fit.beta[1], 1.97));
    assert(closeEnough(fit.coefficients[0].estimate, 1.06));
    assert(closeEnough(fit.coefficients[1].estimate, 1.97));
    assert(closeEnough(fit.coefficients[0].stdError, 0.13490738, 1.0e-6));
    assert(closeEnough(fit.coefficients[1].stdError, 0.05507571, 1.0e-6));
    assert(closeEnough(fit.coefficients[1].tValue, 35.768947, 1.0e-5));
    assert(!std::isfinite(fit.coefficients[0].partialR));
    assert(closeEnough(fit.coefficients[1].partialR,
                       fit.coefficients[1].tValue /
                           std::sqrt(fit.coefficients[1].tValue * fit.coefficients[1].tValue + fit.dfResidual),
                       1.0e-12));
    assert(closeEnough(fit.r2, 0.9976607, 1.0e-6));
    assert(closeEnough(fit.adjR2, 0.9968809, 1.0e-6));
    assert(closeEnough(fit.sigma, 0.1741647, 1.0e-6));
    assert(closeEnough(fit.globalF, 1279.4175824, 1.0e-4));
    assert(fit.diagnostics.size() == 5);
    assert(fit.diagnostics[0].row == 1);
    assert(closeEnough(fit.diagnostics[0].leverage, 0.6));
    assert(closeEnough(fit.diagnostics[1].leverage, 0.3));
    assert(std::isfinite(fit.diagnostics[0].cooksDistance));
    assert(fit.covariance.size() == 2);
    assert(fit.covariance[0].size() == 2);

    LinearOlsInput singular;
    singular.designMatrix = {{1.0, 1.0}, {1.0, 1.0}, {1.0, 1.0}};
    singular.response = {1.0, 2.0, 3.0};
    singular.rowIds = {1, 2, 3};
    LinearOlsResult singularFit = FitLinearOls(singular);
    assert(!singularFit.ok);
    assert(singularFit.warning.find("singular") != std::string::npos);

    LinearOlsInput insufficient;
    insufficient.designMatrix = {{1.0, 0.0}, {1.0, 1.0}};
    insufficient.response = {1.0, 2.0};
    insufficient.rowIds = {1, 2};
    LinearOlsResult insufficientFit = FitLinearOls(insufficient);
    assert(!insufficientFit.ok);
    assert(insufficientFit.warning.find("Not enough") != std::string::npos);

    assert(SameIntegerSet({3, 1, 2}, {2, 3, 1}));
    assert(!SameIntegerSet({1, 2}, {1, 2, 3}));
    assert(TermVectorsAreNested({"x", "z", "x:z"}, {"z", "x"}));
    assert(TermVectorsAreNested({"x:z"}, {"z:x"}));
    assert(!TermVectorsAreNested({"x", "z"}, {"x", "w"}));
    assert(ComparisonModelSerialFromId("cmp", "cmp:model:7") == 7);
    assert(ComparisonModelSerialFromId("cmp", "other:model:7") == 0);
    assert(NextComparisonModelSerial("cmp", {"cmp:model:1", "cmp:model:3", "bad"}) == 4);
    assert(NextComparisonModelId("cmp", {"cmp:model:1", "cmp:model:3"}) == "cmp:model:4");
    assert(ComparisonLabelExists({"Model 1", "Custom"}, "Custom"));
    assert(!ComparisonLabelExists({"Model 1", "Custom"}, "Missing"));
    assert(NextUntitledComparisonLabel({"Model 1", "Model 3"}) == "Model 4");
    assert(UniqueRegressionComparisonCopyLabel({"Model 1", "Model 2"}, "Model 1") == "Model 3");
    assert(UniqueRegressionComparisonCopyLabel({"Custom", "Model 1"}, "Custom") == "Model 2");
    assert(UniqueGeneralizedComparisonCopyLabel({"Model 1"}, "") == "Model 2");
    assert(UniqueGeneralizedComparisonCopyLabel({"A", "Model 1", "Model 2"}, "A") == "Model 3");
    assert(RegressionComparisonFitLabel(5, false) == "F");
    assert(RegressionComparisonFitLabel(5, true) == "D1/Wald");
    assert(RegressionComparisonFitLabel(10, false) == "\u0394SSE vs prev");
    assert(RegressionComparisonFitLabel(11, false) == "Partial F vs prev");
    assert(RegressionComparisonFitLabel(11, true) == "D1/Wald vs prev");
    assert(RegressionComparisonFitLabel(13, false) == "\u0394R\u00B2 vs prev");
    assert(RegressionComparisonFitLabel(16, false) == "\u0394AIC");
    assert(RegressionComparisonFitLabel(99, false).empty());
    assert(RegressionComparisonFitRowIsModelTest(9));
    assert(RegressionComparisonFitRowIsModelTest(12));
    assert(!RegressionComparisonFitRowIsModelTest(8));
    assert(GeneralizedComparisonFitLabel(0) == "Distribution");
    assert(GeneralizedComparisonFitLabel(12) == "\u03C7\u00B2/F vs prev");
    assert(GeneralizedComparisonFitLabel(14) == "\u0394AIC");
    assert(GeneralizedComparisonFitLabel(-1).empty());
    {
        const auto linearMainRows = RegressionComparisonVisibleFitRows(false);
        const auto linearInformationRows = RegressionComparisonVisibleFitRows(true);
        assert(std::find(linearMainRows.begin(), linearMainRows.end(), 7) == linearMainRows.end());
        assert(std::find(linearMainRows.begin(), linearMainRows.end(), 13) != linearMainRows.end());
        assert(std::find(linearInformationRows.begin(), linearInformationRows.end(), 7) != linearInformationRows.end());
        assert(std::find(linearInformationRows.begin(), linearInformationRows.end(), 16) != linearInformationRows.end());
        const auto binaryMainRows = GeneralizedComparisonVisibleFitRows(true, false);
        const auto binaryInformationRows = GeneralizedComparisonVisibleFitRows(true, true);
        const auto generalizedMainRows = GeneralizedComparisonVisibleFitRows(false, false);
        const auto generalizedInformationRows = GeneralizedComparisonVisibleFitRows(false, true);
        assert(std::find(binaryMainRows.begin(), binaryMainRows.end(), 0) == binaryMainRows.end());
        assert(std::find(binaryMainRows.begin(), binaryMainRows.end(), 1) == binaryMainRows.end());
        assert(std::find(binaryMainRows.begin(), binaryMainRows.end(), 4) == binaryMainRows.end());
        assert(std::find(binaryMainRows.begin(), binaryMainRows.end(), 12) != binaryMainRows.end());
        assert(std::find(binaryInformationRows.begin(), binaryInformationRows.end(), 0) == binaryInformationRows.end());
        assert(std::find(binaryInformationRows.begin(), binaryInformationRows.end(), 1) == binaryInformationRows.end());
        assert(std::find(binaryInformationRows.begin(), binaryInformationRows.end(), 4) != binaryInformationRows.end());
        assert(std::find(binaryInformationRows.begin(), binaryInformationRows.end(), 14) != binaryInformationRows.end());
        assert(std::find(generalizedMainRows.begin(), generalizedMainRows.end(), 0) == generalizedMainRows.end());
        assert(std::find(generalizedMainRows.begin(), generalizedMainRows.end(), 1) == generalizedMainRows.end());
        assert(std::find(generalizedInformationRows.begin(), generalizedInformationRows.end(), 0) == generalizedInformationRows.end());
        assert(std::find(generalizedInformationRows.begin(), generalizedInformationRows.end(), 1) == generalizedInformationRows.end());
    }
    {
        GroupModelState source;
        source.group = "conversion_data";
        source.response = "outcome";
        source.terms = {"score", "planet", "score:planet"};
        source.termTypes = {{"score", "numeric"}, {"planet", "factor"}};
        source.termTypeOverrides = {{"planet", "factor"}};
        source.centeredPredictors.insert("score");
        source.factorReferenceLevels["planet"] = "Borealis";
        source.scope = "selected";
        source.dataScopeCaptured = true;
        source.dataScope.datasetId = "conversion_data";
        source.modelVersion = 7;
        source.fitVersion = 7;
        source.isStale = false;

        GLMFitSummary fit;
        fit.ok = true;
        fit.n = 42;
        fit.predictorCenters["score"] = 12.5;
        rlispstat::core::GLMFactorCoding coding;
        coding.variable = "planet";
        coding.levels = {"Aurelia", "Borealis", "Cygnus"};
        coding.referenceLevel = "Borealis";
        fit.factorCodings.push_back(coding);
        source.lastRFitSignature = LinearGLMFitSignature(
            source.response, source.terms,
            rlispstat::core::EffectiveModelSpecificationTermTypes(source),
            source.scope, source.centeredPredictors,
            source.factorReferenceLevels);

        const RegressionComparisonState comparison =
            rlispstat::core::CreateRegressionComparisonFromSingleModel(
                "conversion:linear", source, &fit, nullptr, "Current model");
        assert(comparison.group == source.group);
        assert(comparison.response == source.response);
        assert(comparison.scope == source.scope);
        assert(comparison.dataScopeCaptured);
        assert(comparison.models.size() == 1);
        assert(comparison.models.front().label == "Current model");
        assert(rlispstat::core::EquivalentModelSpecifications(
            source, comparison.models.front()));
        assert(comparison.models.front().fitState ==
               RegressionComparisonFitState::Valid);
        assert(comparison.models.front().fit.predictorCenters.at("score") == 12.5);

        RegressionComparisonState independent = comparison;
        independent.models.front().centeredPredictors.clear();
        independent.models.front().factorReferenceLevels["planet"] = "Aurelia";
        independent.models.front().terms.push_back("extra");
        assert(source.centeredPredictors.count("score") == 1);
        assert(source.factorReferenceLevels.at("planet") == "Borealis");
        assert(source.terms.size() == 3);

        GroupModelState staleSource = source;
        staleSource.lastRFitSignature = "old specification";
        const RegressionComparisonState staleComparison =
            rlispstat::core::CreateRegressionComparisonFromSingleModel(
                "conversion:stale", staleSource, &fit);
        assert(staleComparison.models.front().isStale);
        assert(staleComparison.models.front().fitState ==
               RegressionComparisonFitState::NotFitted);

        GroupModelState unsignedSource = source;
        unsignedSource.lastRFitSignature.clear();
        const RegressionComparisonState unsignedComparison =
            rlispstat::core::CreateRegressionComparisonFromSingleModel(
                "conversion:unsigned", unsignedSource, &fit);
        assert(unsignedComparison.models.front().isStale);
        assert(unsignedComparison.models.front().fitState ==
               RegressionComparisonFitState::NotFitted);
    }
    {
        GeneralizedGLMState source;
        source.id = "conversion:generalized-source";
        source.group = "conversion_data";
        source.response = "count";
        source.family = "poisson";
        source.link = "log";
        source.terms = {"score", "planet", "score:planet"};
        source.termTypes = {{"score", "numeric"}, {"planet", "factor"}};
        source.termTypeOverrides = {{"planet", "factor"}};
        source.centeredPredictors.insert("score");
        source.factorReferenceLevels["planet"] = "Cygnus";
        source.scope = "selected";
        source.dataScopeCaptured = true;
        source.dataScope.datasetId = "conversion_data";
        source.modelVersion = 4;
        source.fitVersion = 4;
        source.ok = true;
        source.n = 39;
        source.residualDeviance = 16.1007302;
        source.logLik = -8.0503651;
        source.parameterCount = 6;
        source.rank = 6;
        source.dfResidual = 33;
        source.rowsUsed = {1, 2, 4};
        source.multipleImputation = true;
        source.imputationCount = 5;
        source.datasetType = "multiple_imputation";
        source.imputationSetId = "mi-conversion-5";
        source.sourceDatasetId = "conversion-source";
        auto semanticRow = [](const std::string &term, const std::string &sourceTerm,
                              const std::string &rowType, const std::string &termType) {
            GeneralizedGLMRow row;
            row.term = term;
            row.sourceTerm = sourceTerm;
            row.rowType = rowType;
            row.termType = termType;
            return row;
        };
        source.rows = {
            semanticRow("(Intercept)", "(Intercept)", "coefficient", "intercept"),
            semanticRow("score", "score", "coefficient", "numeric"),
            semanticRow("planet", "planet", "factor_parent", "factor"),
            semanticRow("planet=Cygnus", "planet", "reference", "factor"),
            semanticRow("planet=Orion", "planet", "factor_level", "factor"),
            semanticRow("planet=Vega", "planet", "factor_level", "factor"),
            semanticRow("score:planet", "score:planet", "term_parent", "numeric_factor_interaction"),
            semanticRow("score:planet=Orion", "score:planet", "coefficient", "numeric_factor_interaction"),
            semanticRow("score:planet=Vega", "score:planet", "coefficient", "numeric_factor_interaction")
        };
        source.rows[3].factorLevel = "Cygnus";
        source.rows[3].referenceLevel = "Cygnus";
        source.rows[4].factorLevel = "Orion";
        source.rows[4].referenceLevel = "Cygnus";
        source.rows[5].factorLevel = "Vega";
        source.rows[5].referenceLevel = "Cygnus";
        double semanticEstimate = 0.25;
        for (auto &row : source.rows) {
            if (row.rowType == "coefficient" || row.rowType == "factor_level") {
                row.estimate = semanticEstimate;
                row.stdError = semanticEstimate / 5.0;
                semanticEstimate += 0.25;
            }
        }
        GeneralizedGLMDiagnosticRow diagnostic;
        diagnostic.row = 1;
        diagnostic.observed = 1.0;
        diagnostic.observedBinary = 1.0;
        diagnostic.fitted = 0.731;
        source.diagnostics.push_back(diagnostic);
        source.lastRFitSignature = GeneralizedGLMFitSignature(source);

        const auto comparison =
            rlispstat::core::CreateGeneralizedComparisonFromSingleModel(
                "conversion:generalized", source, "Poisson model");
        assert(!comparison.binaryComparison);
        assert(comparison.multipleImputation);
        assert(comparison.imputationCount == 5);
        assert(comparison.family == "poisson");
        assert(comparison.link == "log");
        assert(comparison.commonRows == source.rowsUsed);
        assert(comparison.models.size() == 1);
        assert(comparison.models.front().family == source.family);
        assert(comparison.models.front().link == source.link);
        assert(rlispstat::core::EquivalentModelSpecifications(
            source, comparison.models.front()));
        assert(comparison.models.front().fitState ==
               RegressionComparisonFitState::Valid);
        const auto generalizedRoundTrip =
            rlispstat::core::CreateSingleModelFromGeneralizedComparison(
                comparison, 0, "conversion:generalized-roundtrip");
        assert(generalizedRoundTrip.ok);
        assert(generalizedRoundTrip.multipleImputation);
        assert(generalizedRoundTrip.imputationCount == 5);
        assert(generalizedRoundTrip.family == "poisson");
        assert(generalizedRoundTrip.link == "log");
        assert(rlispstat::core::EquivalentModelSpecifications(
            source, generalizedRoundTrip));

        // A version/signature pair alone cannot make an internally stale fit
        // reusable.  Every included source term must also be represented by
        // the semantic rows returned for that fit.
        GeneralizedGLMState incompleteFit = source;
        incompleteFit.rows.erase(
            std::remove_if(incompleteFit.rows.begin(), incompleteFit.rows.end(),
                [](const GeneralizedGLMRow &row) { return row.sourceTerm == "score"; }),
            incompleteFit.rows.end());
        const auto incompleteComparison =
            rlispstat::core::CreateGeneralizedComparisonFromSingleModel(
                "conversion:generalized-incomplete", incompleteFit);
        assert(incompleteComparison.models.front().isStale);
        assert(incompleteComparison.models.front().fitState ==
               RegressionComparisonFitState::NotFitted);

        GeneralizedGLMState countSource = source;
        countSource.countRegression = true;
        countSource.countDistribution = rlispstat::core::CountDistribution::NegativeBinomial;
        countSource.exposure = "person_time";
        countSource.theta = 2.75;
        countSource.lastRFitSignature = GeneralizedGLMFitSignature(countSource);
        const auto countComparison =
            rlispstat::core::CreateGeneralizedComparisonFromSingleModel(
                "conversion:count", countSource, "Rate model");
        assert(countComparison.countComparison);
        assert(!countComparison.binaryComparison);
        assert(countComparison.multipleImputation);
        assert(countComparison.imputationCount == 5);
        assert(countComparison.models.front().countDistribution ==
               rlispstat::core::CountDistribution::NegativeBinomial);
        assert(countComparison.models.front().exposure == "person_time");
        assert(countComparison.models.front().fit.theta == 2.75);
        const auto countRoundTrip =
            rlispstat::core::CreateSingleModelFromGeneralizedComparison(
                countComparison, 0, "conversion:count-roundtrip");
        assert(countRoundTrip.ok);
        assert(countRoundTrip.multipleImputation);
        assert(countRoundTrip.imputationCount == 5);
        assert(countRoundTrip.countRegression);
        assert(!countRoundTrip.binaryRegression);
        assert(countRoundTrip.countDistribution ==
               rlispstat::core::CountDistribution::NegativeBinomial);
        assert(countRoundTrip.exposure == "person_time");
        assert(countRoundTrip.theta == 2.75);
        assert(rlispstat::core::EquivalentModelSpecifications(
            countSource, countRoundTrip));
        auto independentCount = countComparison;
        independentCount.models.front().exposure = "other_time";
        independentCount.models.front().countDistribution =
            rlispstat::core::CountDistribution::Poisson;
        assert(countSource.exposure == "person_time");
        assert(countSource.countDistribution ==
               rlispstat::core::CountDistribution::NegativeBinomial);

        GeneralizedGLMState binarySource = source;
        binarySource.response = "accepted";
        binarySource.family = "binomial";
        binarySource.link = "probit";
        binarySource.binaryRegression = true;
        binarySource.binaryLink = BinaryLink::Probit;
        binarySource.responseCoding.ok = true;
        binarySource.responseCoding.referenceValue = "no";
        binarySource.responseCoding.referenceLabel = "No";
        binarySource.responseCoding.eventValue = "yes";
        binarySource.responseCoding.eventLabel = "Yes";
        binarySource.responseCoding.referenceCount = 21;
        binarySource.responseCoding.eventCount = 18;
        binarySource.responseCodingExplicit = true;
        binarySource.multipleImputation = true;
        binarySource.imputationCount = 5;
        binarySource.lastRFitSignature = GeneralizedGLMFitSignature(binarySource);

        const auto binaryComparison =
            rlispstat::core::CreateGeneralizedComparisonFromSingleModel(
                "conversion:binary", binarySource, "Binary model");
        assert(binaryComparison.binaryComparison);
        assert(binaryComparison.multipleImputation);
        assert(binaryComparison.imputationCount == 5);
        assert(binaryComparison.binaryLink == BinaryLink::Probit);
        assert(binaryComparison.responseCoding.referenceValue == "no");
        assert(binaryComparison.responseCoding.eventValue == "yes");
        assert(binaryComparison.models.front().fit.responseCodingExplicit);
        assert(binaryComparison.models.front().fit.responseCoding.eventLabel == "Yes");
        assert(binaryComparison.models.front().fitState ==
               RegressionComparisonFitState::Valid);
        assert(rlispstat::core::EquivalentModelSpecifications(
            binarySource, binaryComparison.models.front()));
        assert(binaryComparison.models.front().fit.residualDeviance ==
               binarySource.residualDeviance);
        assert(binaryComparison.models.front().fit.logLik == binarySource.logLik);
        assert(binaryComparison.models.front().fit.parameterCount ==
               binarySource.parameterCount);
        assert(binaryComparison.models.front().fit.dfResidual == binarySource.dfResidual);
        assert(binaryComparison.models.front().fit.rows.front().estimate ==
               binarySource.rows.front().estimate);
        assert(binaryComparison.models.front().fit.diagnostics.front().fitted ==
               binarySource.diagnostics.front().fitted);

        // Dataset/MI-set identity is part of the immutable comparison
        // fingerprint.  Opening a single MI model must preserve m=5 rather
        // than silently normalizing a missing count to m=1.
        const std::string binaryFingerprint =
            rlispstat::core::GeneralizedComparisonModelSpecificationFingerprint(
                binaryComparison, binaryComparison.models.front());
        assert(binaryFingerprint.find(
                   "|dataset-kind=multiple_imputation|imputation-set=mi-conversion-5"
                   "|source-dataset=conversion-source|imputations=5") !=
               std::string::npos);
        auto wrongImputationSet = binaryComparison;
        wrongImputationSet.imputationCount = 1;
        assert(rlispstat::core::GeneralizedComparisonModelSpecificationFingerprint(
                   wrongImputationSet, wrongImputationSet.models.front()) != binaryFingerprint);

        // Changing event/reference creates a new semantic revision.  The
        // preceding fit is quarantined immediately; a replacement request
        // can only attach to the new immutable fingerprint.
        auto changedEvent = binaryComparison;
        std::swap(changedEvent.responseCoding.eventValue,
                  changedEvent.responseCoding.referenceValue);
        std::swap(changedEvent.responseCoding.eventLabel,
                  changedEvent.responseCoding.referenceLabel);
        std::swap(changedEvent.responseCoding.eventCount,
                  changedEvent.responseCoding.referenceCount);
        rlispstat::core::InvalidateGeneralizedComparisonModel(
            changedEvent, changedEvent.models.front());
        changedEvent.models.front().fitState = RegressionComparisonFitState::Pending;
        changedEvent.rFitPending = true;
        assert(changedEvent.imputationCount == 5);
        assert(rlispstat::core::GeneralizedComparisonModelSpecificationFingerprint(
                   changedEvent, changedEvent.models.front()) != binaryFingerprint);
        assert(rlispstat::core::GeneralizedComparisonResolvedFitState(
                   changedEvent, changedEvent.models.front()) ==
               RegressionComparisonFitState::Pending);
        rlispstat::core::RefreshGeneralizedTermRowsFromFits(changedEvent);
        const auto pendingCoefficient =
            rlispstat::core::GeneralizedComparisonTermCell(
                changedEvent, 0, "(Intercept)");
        const auto pendingStatistic =
            rlispstat::core::GeneralizedComparisonFitCell(changedEvent, 0, 2);
        assert(pendingCoefficient.displayState ==
               rlispstat::core::RegressionComparisonCellDisplayState::Blank);
        assert(pendingCoefficient.displayText.empty());
        assert(pendingStatistic.displayState ==
               rlispstat::core::RegressionComparisonCellDisplayState::Blank);
        assert(pendingStatistic.displayText.empty());
        assert(changedEvent.models.front().fit.rows.empty());
        assert(changedEvent.models.front().fitVersion == 0);

        const auto binaryRoundTrip =
            rlispstat::core::CreateSingleModelFromGeneralizedComparison(
                binaryComparison, 0, "conversion:binary-roundtrip");
        assert(binaryRoundTrip.id == "conversion:binary-roundtrip");
        assert(binaryRoundTrip.multipleImputation);
        assert(binaryRoundTrip.imputationCount == 5);
        assert(binaryRoundTrip.binaryRegression);
        assert(!binaryRoundTrip.countRegression);
        assert(binaryRoundTrip.binaryLink == BinaryLink::Probit);
        assert(binaryRoundTrip.responseCodingExplicit);
        assert(binaryRoundTrip.responseCoding.referenceValue == "no");
        assert(binaryRoundTrip.responseCoding.eventValue == "yes");
        assert(binaryRoundTrip.factorReferenceLevels.at("planet") == "Cygnus");
        assert(binaryRoundTrip.centeredPredictors.count("score") == 1);
        assert(binaryRoundTrip.terms == binarySource.terms);
        assert(binaryRoundTrip.ok);
        assert(binaryRoundTrip.fitVersion == binaryRoundTrip.modelVersion);
        assert(binaryRoundTrip.lastRFitSignature ==
               GeneralizedGLMFitSignature(binaryRoundTrip));
        assert(binaryRoundTrip.residualDeviance == binarySource.residualDeviance);
        assert(binaryRoundTrip.logLik == binarySource.logLik);
        assert(binaryRoundTrip.parameterCount == binarySource.parameterCount);
        assert(binaryRoundTrip.dfResidual == binarySource.dfResidual);
        assert(binaryRoundTrip.rows.front().estimate == binarySource.rows.front().estimate);
        assert(binaryRoundTrip.diagnostics.front().fitted ==
               binarySource.diagnostics.front().fitted);
        assert(rlispstat::core::EquivalentModelSpecifications(
            binarySource, binaryRoundTrip));

        auto changedRoundTrip = binaryRoundTrip;
        changedRoundTrip.factorReferenceLevels["planet"] = "Aurelia";
        changedRoundTrip.centeredPredictors.clear();
        changedRoundTrip.terms.pop_back();
        assert(binaryComparison.models.front().factorReferenceLevels.at("planet") == "Cygnus");
        assert(binaryComparison.models.front().centeredPredictors.count("score") == 1);
        assert(binaryComparison.models.front().terms == binarySource.terms);

        auto staleBinaryComparison = binaryComparison;
        staleBinaryComparison.models.front().isStale = true;
        const auto staleRoundTrip =
            rlispstat::core::CreateSingleModelFromGeneralizedComparison(
                staleBinaryComparison, 0);
        assert(!staleRoundTrip.ok);
        assert(staleRoundTrip.fitVersion == 0);
        assert(staleRoundTrip.lastRFitSignature.empty());

        auto independentBinary = binaryComparison;
        independentBinary.models.front().factorReferenceLevels["planet"] = "Aurelia";
        independentBinary.models.front().centeredPredictors.clear();
        assert(binarySource.factorReferenceLevels.at("planet") == "Cygnus");
        assert(binarySource.centeredPredictors.count("score") == 1);

        GeneralizedGLMState unsignedSource = source;
        unsignedSource.lastRFitSignature.clear();
        const auto unsignedComparison =
            rlispstat::core::CreateGeneralizedComparisonFromSingleModel(
                "conversion:generalized-unsigned", unsignedSource);
        assert(unsignedComparison.models.front().isStale);
        assert(unsignedComparison.models.front().fitState ==
               RegressionComparisonFitState::NotFitted);
    }
    {
        // Exact macOS regression: a single Binary Regression
        // `vs ~ mpg + factor(cyl)` opened with Compare models must never mix
        // its fitted statistics with an unrelated presentation row such as hp.
        GeneralizedGLMState source;
        source.id = "binary:mtcars:source";
        source.group = "mtcars";
        source.response = "vs";
        source.family = "binomial";
        source.link = "logit";
        source.binaryRegression = true;
        source.binaryLink = BinaryLink::Logit;
        source.responseCoding.ok = true;
        source.responseCoding.eventValue = "0";
        source.responseCoding.eventLabel = "0";
        source.responseCoding.referenceValue = "1";
        source.responseCoding.referenceLabel = "1";
        source.terms = {"mpg", "cyl"};
        source.termTypes = {{"mpg", "numeric"}, {"cyl", "factor"}};
        source.termTypeOverrides = {{"cyl", "factor"}};
        source.factorReferenceLevels = {{"cyl", "8"}};
        source.modelVersion = 11;
        source.fitVersion = 11;
        source.ok = true;
        source.n = 32;
        source.parameterCount = 4;
        source.dfResidual = 28;
        source.residualDeviance = 16.101;

        auto row = [](const std::string &term, const std::string &sourceTerm,
                      const std::string &rowType, const std::string &termType,
                      double estimate = NAN) {
            GeneralizedGLMRow result;
            result.term = term;
            result.displayLabel = term;
            result.sourceTerm = sourceTerm;
            result.rowType = rowType;
            result.termType = termType;
            result.estimate = estimate;
            return result;
        };
        source.rows = {
            row("(Intercept)", "(Intercept)", "coefficient", "numeric", 18.231),
            row("mpg", "mpg", "coefficient", "numeric", 0.090),
            row("cyl", "cyl", "factor_parent", "factor"),
            row("cyl=8", "cyl", "reference", "factor"),
            row("cyl=4", "cyl", "factor_level", "factor", 2.015),
            row("cyl=6", "cyl", "factor_level", "factor", -19.854)
        };
        source.rows[3].factorLevel = "8";
        source.rows[3].referenceLevel = "8";
        source.rows[4].factorLevel = "4";
        source.rows[4].referenceLevel = "8";
        source.rows[5].factorLevel = "6";
        source.rows[5].referenceLevel = "8";
        source.lastRFitSignature = GeneralizedGLMFitSignature(source);

        auto comparison = rlispstat::core::CreateGeneralizedComparisonFromSingleModel(
            "binarycmp:mtcars:canonical", source, "Model 1");
        assert(comparison.models.size() == 1);
        const auto &copied = comparison.models.front();
        assert(copied.terms == source.terms);
        assert(rlispstat::core::GeneralizedComparisonModelTermType(
            comparison, 0, "mpg") == "numeric");
        assert(rlispstat::core::GeneralizedComparisonModelTermType(
            comparison, 0, "cyl") == "factor");
        assert(copied.fit.parameterCount == 4);
        assert(copied.fit.dfResidual == 28);
        assert(copied.fit.residualDeviance == 16.101);
        assert(std::find(comparison.termRows.begin(), comparison.termRows.end(), "hp") ==
               comparison.termRows.end());
        assert(std::find(comparison.termRows.begin(), comparison.termRows.end(), "cyl") !=
               comparison.termRows.end());
        assert(std::find(comparison.termRows.begin(), comparison.termRows.end(), "cyl=4") !=
               comparison.termRows.end());
        assert(std::find(comparison.termRows.begin(), comparison.termRows.end(), "cyl=6") !=
               comparison.termRows.end());
        std::string invariantReason;
        assert(rlispstat::core::GeneralizedComparisonPresentationIsConsistent(
            comparison, &invariantReason));
        auto corruptPresentation = comparison;
        corruptPresentation.termRows.push_back("hp");
        assert(!rlispstat::core::GeneralizedComparisonPresentationIsConsistent(
            corruptPresentation, &invariantReason));
        rlispstat::core::RefreshGeneralizedTermRowsFromFits(corruptPresentation);
        assert(std::find(corruptPresentation.termRows.begin(),
                         corruptPresentation.termRows.end(), "hp") ==
               corruptPresentation.termRows.end());
        assert(rlispstat::core::GeneralizedComparisonPresentationIsConsistent(
            corruptPresentation, &invariantReason));

        auto corruptFitRows = comparison;
        corruptFitRows.models.front().fit.rows.push_back(
            row("hp", "hp", "coefficient", "numeric", 0.5));
        rlispstat::core::RefreshGeneralizedTermRowsFromFits(corruptFitRows);
        assert(corruptFitRows.models.front().isStale);
        assert(rlispstat::core::GeneralizedComparisonResolvedFitState(
                   corruptFitRows, corruptFitRows.models.front()) !=
               RegressionComparisonFitState::Valid);
        assert(std::find(corruptFitRows.termRows.begin(), corruptFitRows.termRows.end(), "hp") ==
               corruptFitRows.termRows.end());
        std::cout << "\nCANONICAL_BINARY_COMPARISON_TRACE\n"
                  << rlispstat::core::GeneralizedComparisonConsistencyTrace(
                         source, comparison, 0)
                  << "\nEND_CANONICAL_BINARY_COMPARISON_TRACE\n";

        auto acceptCurrentRevision = [&](bool factor) {
            auto &model = comparison.models.front();
            const std::string fingerprint =
                rlispstat::core::GeneralizedComparisonModelSpecificationFingerprint(
                    comparison, model);
            model.requestedSpecificationRevision = model.modelVersion;
            model.requestedSpecificationFingerprint = fingerprint;
            model.fitSpecificationRevision = model.modelVersion;
            model.fitSpecificationFingerprint = fingerprint;
            model.fitVersion = model.modelVersion;
            model.isStale = false;
            model.fitState = RegressionComparisonFitState::Valid;
            static_cast<rlispstat::core::ModelSpecification &>(model.fit) =
                rlispstat::core::EffectiveGeneralizedComparisonModelSpecification(
                    comparison, model);
            model.fit.group = comparison.group;
            model.fit.family = "binomial";
            model.fit.link = "logit";
            model.fit.binaryRegression = true;
            model.fit.binaryLink = BinaryLink::Logit;
            model.fit.responseCoding = comparison.responseCoding;
            model.fit.ok = true;
            model.fit.n = 32;
            model.fit.parameterCount = factor ? 4 : 3;
            model.fit.dfResidual = factor ? 28 : 29;
            model.fit.residualDeviance = factor ? 16.101 : 25.533;
            model.fit.modelVersion = model.modelVersion;
            model.fit.fitVersion = model.modelVersion;
            model.fit.lastRFitSignature = fingerprint;
            model.fit.rows = {
                row("(Intercept)", "(Intercept)", "coefficient", "numeric", 1.0),
                row("mpg", "mpg", "coefficient", "numeric", 0.1)
            };
            if (factor) {
                const std::string reference = model.factorReferenceLevels.count("cyl")
                    ? model.factorReferenceLevels.at("cyl") : "4";
                model.fit.rows.push_back(row("cyl", "cyl", "factor_parent", "factor"));
                for (const std::string &level : std::vector<std::string>{"4", "6", "8"}) {
                    model.fit.rows.push_back(row(
                        "cyl=" + level, "cyl",
                        level == reference ? "reference" : "factor_level", "factor",
                        level == reference ? NAN : 0.25));
                    model.fit.rows.back().factorLevel = level;
                    model.fit.rows.back().referenceLevel = reference;
                }
            } else {
                model.fit.rows.push_back(row("cyl", "cyl", "coefficient", "numeric", 0.25));
            }
            comparison.rFitPending = false;
            rlispstat::core::RefreshGeneralizedTermRowsFromFits(comparison);
            assert(rlispstat::core::GeneralizedComparisonPresentationIsConsistent(
                comparison, &invariantReason));
            assert(model.fitSpecificationRevision == model.modelVersion);
            assert(model.fitSpecificationFingerprint ==
                   rlispstat::core::GeneralizedComparisonModelSpecificationFingerprint(
                       comparison, model));
        };

        const std::vector<std::string> variables = {"vs", "mpg", "cyl", "hp"};
        assert(rlispstat::core::IncludeGeneralizedComparisonTerm(
            comparison, 0, "hp", variables));
        assert(rlispstat::core::GeneralizedComparisonResolvedFitState(
                   comparison, comparison.models.front()) !=
               RegressionComparisonFitState::Valid);
        // Even if an asynchronous callback accidentally clears `isStale`, an
        // older per-column revision/fingerprint cannot become presentable.
        comparison.models.front().isStale = false;
        comparison.models.front().fitState = RegressionComparisonFitState::Valid;
        rlispstat::core::RefreshGeneralizedTermRowsFromFits(comparison);
        assert(comparison.models.front().isStale);
        assert(rlispstat::core::GeneralizedComparisonResolvedFitState(
                   comparison, comparison.models.front()) !=
               RegressionComparisonFitState::Valid);
        assert(std::find(comparison.termRows.begin(), comparison.termRows.end(), "hp") !=
               comparison.termRows.end());
        assert(rlispstat::core::ExcludeGeneralizedComparisonTerm(
            comparison, 0, "hp"));
        assert(std::find(comparison.termRows.begin(), comparison.termRows.end(), "hp") !=
               comparison.termRows.end());
        const auto excludedHp = rlispstat::core::GeneralizedComparisonTermCell(
            comparison, 0, "hp");
        assert(!excludedHp.termIncluded);
        assert(excludedHp.inclusionControlAvailable);
        acceptCurrentRevision(true);
        assert(rlispstat::core::RemoveGeneralizedComparisonTermCompletely(
            comparison, "hp"));
        assert(std::find(comparison.termRows.begin(), comparison.termRows.end(), "hp") ==
               comparison.termRows.end());

        assert(rlispstat::core::ApplyGeneralizedComparisonReferenceLevel(
            comparison, 0, "cyl", "4", {"4", "6", "8"}));
        acceptCurrentRevision(true);
        assert(rlispstat::core::GeneralizedComparisonSourceTerm(
            comparison, "cyl=4") == "cyl");

        rlispstat::core::ModelPredictorMetadata cylMetadata;
        cylMetadata.variable = "cyl";
        cylMetadata.storageType = "numeric";
        cylMetadata.observedCount = 32;
        cylMetadata.uniqueCount = 3;
        cylMetadata.levels = {"4", "6", "8"};
        assert(rlispstat::core::ApplyGeneralizedComparisonTermType(
            comparison, 0, "cyl", "numeric", cylMetadata));
        acceptCurrentRevision(false);
        assert(std::find(comparison.termRows.begin(), comparison.termRows.end(), "cyl=6") ==
               comparison.termRows.end());
        assert(rlispstat::core::ApplyGeneralizedComparisonTermType(
            comparison, 0, "cyl", "factor", cylMetadata));
        acceptCurrentRevision(true);
        assert(std::find(comparison.termRows.begin(), comparison.termRows.end(), "hp") ==
               comparison.termRows.end());
        assert(std::find(comparison.termRows.begin(), comparison.termRows.end(), "cyl=6") !=
               comparison.termRows.end());

        // Exact reported failure: am ~ factor(cyl) + mpg + factor(gear),
        // probit, event 1/reference 0.  A current six-column semantic design
        // must never be paired with the four-parameter statistics from the
        // earlier cyl + mpg revision.
        comparison.response = "am";
        comparison.link = "probit";
        comparison.binaryLink = BinaryLink::Probit;
        comparison.responseCoding.eventValue = "1";
        comparison.responseCoding.eventLabel = "1";
        comparison.responseCoding.referenceValue = "0";
        comparison.responseCoding.referenceLabel = "0";
        auto &exactModel = comparison.models.front();
        exactModel.response = "am";
        exactModel.link = "probit";
        exactModel.terms = {"cyl", "mpg"};
        exactModel.termTypeOverrides = {{"cyl", "factor"}, {"mpg", "numeric"}};
        exactModel.termTypes = exactModel.termTypeOverrides;
        exactModel.factorReferenceLevels = {{"cyl", "4"}};
        ++exactModel.modelVersion;
        exactModel.isStale = true;
        exactModel.fitState = RegressionComparisonFitState::NotFitted;
        comparison.termTypes = exactModel.termTypeOverrides;
        rlispstat::core::RefreshGeneralizedTermRowsFromFits(comparison);

        const std::vector<std::string> exactVariables = {"am", "cyl", "mpg", "gear", "hp"};
        assert(rlispstat::core::IncludeGeneralizedComparisonTerm(
            comparison, 0, "gear", exactVariables));
        rlispstat::core::ModelPredictorMetadata gearMetadata;
        gearMetadata.variable = "gear";
        gearMetadata.storageType = "numeric";
        gearMetadata.observedCount = 32;
        gearMetadata.uniqueCount = 3;
        gearMetadata.levels = {"3", "4", "5"};
        assert(rlispstat::core::ApplyGeneralizedComparisonTermType(
            comparison, 0, "gear", "factor", gearMetadata));

        auto acceptExactRevision = [&]() {
            auto &model = comparison.models.front();
            const std::string fingerprint =
                rlispstat::core::GeneralizedComparisonModelSpecificationFingerprint(
                    comparison, model);
            model.requestedSpecificationRevision = model.modelVersion;
            model.requestedSpecificationFingerprint = fingerprint;
            model.fitSpecificationRevision = model.modelVersion;
            model.fitSpecificationFingerprint = fingerprint;
            model.fitVersion = model.modelVersion;
            model.isStale = false;
            model.fitState = RegressionComparisonFitState::Valid;
            static_cast<rlispstat::core::ModelSpecification &>(model.fit) =
                rlispstat::core::EffectiveGeneralizedComparisonModelSpecification(
                    comparison, model);
            model.fit.group = "mtcars";
            model.fit.family = "binomial";
            model.fit.link = "probit";
            model.fit.binaryRegression = true;
            model.fit.binaryLink = BinaryLink::Probit;
            model.fit.responseCoding = comparison.responseCoding;
            model.fit.ok = true;
            model.fit.n = 32;
            model.fit.rows = {
                row("(Intercept)", "(Intercept)", "coefficient", "numeric", -14.109),
                row("cyl", "cyl", "factor_parent", "factor"),
                row("cyl=4", "cyl", "reference", "factor"),
                row("cyl=6", "cyl", "factor_level", "factor", 0.948),
                row("cyl=8", "cyl", "factor_level", "factor", 2.230),
                row("mpg", "mpg", "coefficient", "numeric", 0.326)
            };
            model.fit.rows[2].factorLevel = "4";
            model.fit.rows[2].referenceLevel = "4";
            model.fit.rows[3].factorLevel = "6";
            model.fit.rows[3].referenceLevel = "4";
            model.fit.rows[4].factorLevel = "8";
            model.fit.rows[4].referenceLevel = "4";
            int parameters = 4;
            if (rlispstat::core::GeneralizedModelIncludesTerm(model, "gear")) {
                const bool gearFactor =
                    rlispstat::core::GeneralizedComparisonModelTermType(
                        comparison, 0, "gear") == "factor";
                if (gearFactor) {
                    model.fit.rows.push_back(row("gear", "gear", "factor_parent", "factor"));
                    model.fit.rows.push_back(row("gear=3", "gear", "reference", "factor"));
                    model.fit.rows.back().factorLevel = "3";
                    model.fit.rows.back().referenceLevel = "3";
                    model.fit.rows.push_back(row("gear=4", "gear", "factor_level", "factor", 6.716));
                    model.fit.rows.back().factorLevel = "4";
                    model.fit.rows.back().referenceLevel = "3";
                    model.fit.rows.push_back(row("gear=5", "gear", "factor_level", "factor", 12.822));
                    model.fit.rows.back().factorLevel = "5";
                    model.fit.rows.back().referenceLevel = "3";
                    parameters += 2;
                } else {
                    model.fit.rows.push_back(row("gear", "gear", "coefficient", "numeric", 0.5));
                    parameters += 1;
                }
            }
            if (rlispstat::core::GeneralizedModelIncludesTerm(model, "hp")) {
                model.fit.rows.push_back(row("hp", "hp", "coefficient", "numeric", 0.01));
                parameters += 1;
            }
            const std::string cylReference = model.factorReferenceLevels.count("cyl")
                ? model.factorReferenceLevels.at("cyl") : "4";
            for (auto &fitRow : model.fit.rows) {
                if (fitRow.sourceTerm != "cyl" ||
                    (fitRow.rowType != "reference" && fitRow.rowType != "factor_level")) continue;
                fitRow.rowType = fitRow.factorLevel == cylReference
                    ? "reference" : "factor_level";
                fitRow.referenceLevel = cylReference;
            }
            model.fit.parameterCount = parameters;
            model.fit.rank = parameters;
            model.fit.rankDeficient = false;
            model.fit.dfResidual = 32 - parameters;
            model.fit.residualDeviance = parameters == 6 ? 10.98809 : 12.0;
            model.fit.modelVersion = model.modelVersion;
            model.fit.fitVersion = model.modelVersion;
            model.fit.lastRFitSignature = fingerprint;
            comparison.rFitPending = false;
            rlispstat::core::RefreshGeneralizedTermRowsFromFits(comparison);
            assert(rlispstat::core::GeneralizedComparisonResolvedFitState(
                       comparison, model) == RegressionComparisonFitState::Valid);
            assert(rlispstat::core::GeneralizedComparisonPresentationIsConsistent(
                comparison, &invariantReason));
        };

        acceptExactRevision();
        assert(exactModel.fit.parameterCount == 6);
        assert(exactModel.fit.dfResidual == 26);
        for (const std::string &term : std::vector<std::string>{
                 "cyl", "cyl=4", "cyl=6", "cyl=8", "mpg",
                 "gear", "gear=3", "gear=4", "gear=5"}) {
            assert(std::find(comparison.termRows.begin(), comparison.termRows.end(), term) !=
                   comparison.termRows.end());
        }
        assert(std::find(comparison.termRows.begin(), comparison.termRows.end(), "hp") ==
               comparison.termRows.end());
        GeneralizedGLMState exactSource = exactModel.fit;
        exactSource.id = "binary:mtcars:two-factor-source";
        exactSource.modelVersion = exactModel.modelVersion;
        exactSource.fitVersion = exactModel.modelVersion;
        exactSource.lastRFitSignature = GeneralizedGLMFitSignature(exactSource);
        std::cout << "\nEXACT_TWO_FACTOR_BINARY_COMPARISON_TRACE\n"
                  << rlispstat::core::GeneralizedComparisonConsistencyTrace(
                         exactSource, comparison, 0)
                  << "\nEND_EXACT_TWO_FACTOR_BINARY_COMPARISON_TRACE\n";

        auto mixedStatistics = comparison;
        mixedStatistics.models.front().fit.parameterCount = 4;
        mixedStatistics.models.front().fit.rank = 4;
        mixedStatistics.models.front().fit.dfResidual = 28;
        mixedStatistics.models.front().fit.residualDeviance = 29.404;
        rlispstat::core::RefreshGeneralizedTermRowsFromFits(mixedStatistics);
        assert(mixedStatistics.models.front().isStale);
        assert(rlispstat::core::GeneralizedComparisonResolvedFitState(
                   mixedStatistics, mixedStatistics.models.front()) !=
               RegressionComparisonFitState::Valid);

        assert(rlispstat::core::IncludeGeneralizedComparisonTerm(
            comparison, 0, "hp", exactVariables));
        assert(rlispstat::core::GeneralizedComparisonResolvedFitState(
                   comparison, exactModel) != RegressionComparisonFitState::Valid);
        acceptExactRevision();
        assert(exactModel.fit.parameterCount == 7 && exactModel.fit.dfResidual == 25);
        assert(rlispstat::core::ExcludeGeneralizedComparisonTerm(comparison, 0, "hp"));
        acceptExactRevision();
        assert(exactModel.fit.parameterCount == 6 && exactModel.fit.dfResidual == 26);
        assert(rlispstat::core::ApplyGeneralizedComparisonTermType(
            comparison, 0, "gear", "numeric", gearMetadata));
        acceptExactRevision();
        assert(exactModel.fit.parameterCount == 5 && exactModel.fit.dfResidual == 27);
        assert(rlispstat::core::ApplyGeneralizedComparisonTermType(
            comparison, 0, "gear", "factor", gearMetadata));
        acceptExactRevision();
        assert(exactModel.fit.parameterCount == 6 && exactModel.fit.dfResidual == 26);
        assert(rlispstat::core::ApplyGeneralizedComparisonReferenceLevel(
            comparison, 0, "cyl", "8", {"4", "6", "8"}));
        acceptExactRevision();
        assert(rlispstat::core::GeneralizedComparisonSourceTerm(
            comparison, "cyl=8") == "cyl");
    }
    {
        // Exact MI ownership regression: the source single-model session and
        // comparison Model 1 may share immutable dataset identity, but no
        // mutable specification/result container.  This is the native half of
        // y_bin ~ x + factor(g), m=5, A as reference.
        GeneralizedGLMState source;
        source.id = "binary:mi:independent-source";
        source.group = "mi_test_mids";
        source.response = "y_bin";
        source.family = "binomial";
        source.link = "logit";
        source.binaryRegression = true;
        source.binaryLink = BinaryLink::Logit;
        source.responseCoding.ok = true;
        source.responseCoding.eventValue = "1";
        source.responseCoding.eventLabel = "1";
        source.responseCoding.referenceValue = "0";
        source.responseCoding.referenceLabel = "0";
        source.responseCodingExplicit = true;
        source.terms = {"x", "g"};
        source.termTypes = {{"x", "numeric"}, {"g", "factor"}};
        source.termTypeOverrides = {{"x", "numeric"}, {"g", "factor"}};
        source.factorReferenceLevels = {{"g", "A"}};
        source.multipleImputation = true;
        source.imputationCount = 5;
        source.datasetType = "multiple_imputation";
        source.imputationSetId = "mi-independent-5";
        source.sourceDatasetId = "mi-independent-source-data";
        source.modelVersion = 17;
        source.fitVersion = 17;
        source.ok = true;
        source.n = 400;
        source.parameterCount = 4;
        source.rank = 4;
        source.dfResidual = 396;
        source.residualDeviance = 491.773;
        auto makeRow = [](const std::string &term, const std::string &sourceTerm,
                          const std::string &rowType, const std::string &termType,
                          double estimate = NAN) {
            GeneralizedGLMRow result;
            result.term = term;
            result.displayLabel = term;
            result.sourceTerm = sourceTerm;
            result.rowType = rowType;
            result.termType = termType;
            result.estimate = estimate;
            return result;
        };
        source.rows = {
            makeRow("(Intercept)", "(Intercept)", "coefficient", "numeric", 0.454),
            makeRow("x", "x", "coefficient", "numeric", 0.852),
            makeRow("g", "g", "factor_parent", "factor"),
            makeRow("g=A", "g", "reference", "factor"),
            makeRow("g=B", "g", "factor_level", "factor", 0.186),
            makeRow("g=C", "g", "factor_level", "factor", -0.798)
        };
        for (std::size_t i = 3; i < source.rows.size(); ++i) {
            source.rows[i].factorLevel = std::string(1, static_cast<char>('A' + i - 3));
            source.rows[i].referenceLevel = "A";
        }
        source.lastRFitSignature = GeneralizedGLMFitSignature(source);

        const GeneralizedGLMState original = source;
        const std::string originalSignature = GeneralizedGLMFitSignature(source);
        auto assertSourceInvariant = [&]() {
            assert(rlispstat::core::EquivalentModelSpecifications(source, original));
            assert(source.responseCoding.eventValue == original.responseCoding.eventValue);
            assert(source.responseCoding.referenceValue == original.responseCoding.referenceValue);
            assert(source.multipleImputation == original.multipleImputation);
            assert(source.imputationCount == original.imputationCount);
            assert(source.datasetType == original.datasetType);
            assert(source.imputationSetId == original.imputationSetId);
            assert(source.sourceDatasetId == original.sourceDatasetId);
            assert(source.ok == original.ok);
            assert(source.parameterCount == original.parameterCount);
            assert(source.dfResidual == original.dfResidual);
            assert(source.residualDeviance == original.residualDeviance);
            assert(source.rows.size() == original.rows.size());
            for (std::size_t i = 0; i < source.rows.size(); ++i) {
                assert(source.rows[i].term == original.rows[i].term);
                assert(source.rows[i].sourceTerm == original.rows[i].sourceTerm);
                assert(source.rows[i].rowType == original.rows[i].rowType);
                assert(source.rows[i].termType == original.rows[i].termType);
                assert(source.rows[i].factorLevel == original.rows[i].factorLevel);
                assert(source.rows[i].referenceLevel == original.rows[i].referenceLevel);
                if (std::isfinite(original.rows[i].estimate))
                    assert(source.rows[i].estimate == original.rows[i].estimate);
            }
            assert(GeneralizedGLMFitSignature(source) == originalSignature);
        };

        auto comparison = rlispstat::core::CreateGeneralizedComparisonFromSingleModel(
            "binarycmp:mi:independent", source, "Model 1");
        assert(comparison.models.size() == 1);
        assert(rlispstat::core::EquivalentModelSpecifications(
            source, comparison.models.front()));
        assert(comparison.models.front().fit.parameterCount == 4);
        assert(comparison.models.front().fit.dfResidual == 396);
        assert(&source.terms != &comparison.models.front().terms);
        assert(&source.termTypes != &comparison.models.front().termTypes);
        assert(&source.factorReferenceLevels !=
               &comparison.models.front().factorReferenceLevels);
        assert(&source.rows != &comparison.models.front().fit.rows);
        assertSourceInvariant();

        // The former comparison-wide fallback is deliberately non-canonical.
        // Even corrupting it cannot reinterpret a self-contained model column.
        comparison.termTypes["g"] = "numeric";
        assert(rlispstat::core::GeneralizedComparisonModelTermType(
            comparison, 0, "g") == "factor");
        assert(rlispstat::core::EffectiveGeneralizedComparisonModelSpecification(
            comparison, comparison.models.front()).termTypes.at("g") == "factor");
        assertSourceInvariant();

        const std::vector<std::string> variables = {"y_bin", "x", "g"};
        assert(rlispstat::core::ExcludeGeneralizedComparisonTerm(
            comparison, 0, "g"));
        assertSourceInvariant();
        assert(rlispstat::core::IncludeGeneralizedComparisonTerm(
            comparison, 0, "g", variables));
        assertSourceInvariant();

        rlispstat::core::ModelPredictorMetadata gMetadata;
        gMetadata.variable = "g";
        gMetadata.storageType = "factor";
        gMetadata.observedCount = 400;
        gMetadata.uniqueCount = 3;
        gMetadata.levels = {"A", "B", "C"};
        assert(rlispstat::core::ApplyGeneralizedComparisonTermType(
            comparison, 0, "g", "factor", gMetadata));
        assertSourceInvariant();
        assert(rlispstat::core::ApplyGeneralizedComparisonReferenceLevel(
            comparison, 0, "g", "B", gMetadata.levels));
        assertSourceInvariant();
        assert(rlispstat::core::ToggleGeneralizedComparisonPredictorCentering(
            comparison, 0, "x"));
        assertSourceInvariant();
        assert(rlispstat::core::IncludeGeneralizedComparisonTerm(
            comparison, 0, "x:g", variables));
        assertSourceInvariant();

        const int duplicate = rlispstat::core::AddGeneralizedComparisonModel(
            comparison, 0, false);
        assert(duplicate == 1);
        assert(rlispstat::core::ApplyGeneralizedComparisonTermType(
            comparison, duplicate, "g", "numeric", gMetadata));
        assert(rlispstat::core::GeneralizedComparisonModelTermType(
            comparison, 0, "g") == "factor");
        assertSourceInvariant();

        // Reverse conversion is independent as well: editing the new single
        // session cannot mutate the comparison column it was created from.
        const auto pristineComparison =
            rlispstat::core::CreateGeneralizedComparisonFromSingleModel(
                "binarycmp:mi:reverse", source, "Model 1");
        auto detachedSingle =
            rlispstat::core::CreateSingleModelFromGeneralizedComparison(
                pristineComparison, 0, "binary:mi:detached");
        const std::string comparisonFingerprint =
            rlispstat::core::GeneralizedComparisonModelSpecificationFingerprint(
                pristineComparison, pristineComparison.models.front());
        assert(rlispstat::core::SetModelSpecificationTermType(
            detachedSingle, "g", "numeric", gMetadata).changed);
        assert(pristineComparison.models.front().termTypes.at("g") == "factor");
        assert(pristineComparison.models.front().factorReferenceLevels.at("g") == "A");
        assert(rlispstat::core::GeneralizedComparisonModelSpecificationFingerprint(
            pristineComparison, pristineComparison.models.front()) ==
               comparisonFingerprint);
        assertSourceInvariant();

        // Closing/destroying a comparison and creating another one always
        // copies the still-correct source specification and result afresh.
        comparison = rlispstat::core::GeneralizedComparisonState();
        const auto reopened =
            rlispstat::core::CreateGeneralizedComparisonFromSingleModel(
                "binarycmp:mi:reopened", source, "Model 1");
        assert(rlispstat::core::GeneralizedComparisonModelTermType(
            reopened, 0, "g") == "factor");
        assert(reopened.models.front().fit.parameterCount == 4);
        assert(reopened.models.front().fit.dfResidual == 396);
        assertSourceInvariant();
    }
    assert(RegressionComparisonFootnote().find("Partial F uses \u0394SSE") != std::string::npos);
    assert(rlispstat::core::GLMAddTermLabel() == "+ Add term");
    assert(RegressionComparisonFootnote().find("global F") != std::string::npos);
    assert(GeneralizedComparisonFootnote().find("previous column") != std::string::npos);
    ComparisonCopyTable copyTable;
    copyTable.title = "General Linear Model Comparison";
    copyTable.response = "y";
    copyTable.modelLabels = {"Reduced", "Full"};
    copyTable.modelResponses = {"y", "other"};
    copyTable.termLabels = {"(Intercept)", "x"};
    copyTable.termDisplaysByRow = {{"1.0", "2.0"}, {"\u2014", "3.0*"}};
    copyTable.fitLabels = {"N", "Partial F vs prev"};
    copyTable.fitDisplaysByRow = {{"10", "10"}, {"\u2014", "4.500"}};
    copyTable.footnote = RegressionComparisonFootnote();
    std::string copyText = ComparisonCopyTableText(copyTable);
    assert(copyText.find("General Linear Model Comparison\nResponse:\ty\n\nTerm\tReduced\tFull\nResponse\ty\tother\n") == 0);
    assert(copyText.find("x\t\u2014\t3.0*\n\nN\t10\t10\nPartial F vs prev\t\u2014\t4.500") != std::string::npos);
    assert(copyText.find("Partial F uses \u0394SSE") != std::string::npos);

    RegressionComparisonState sharedComparisonTable;
    sharedComparisonTable.title = "General Linear Model Comparison";
    sharedComparisonTable.response = "outcome";
    sharedComparisonTable.termRows = {
        "(Intercept)", "planet", "planet=Aurelia", "planet=Borealis", "score"
    };
    RegressionComparisonModel sharedReduced;
    sharedReduced.label = "Reduced";
    sharedReduced.terms = {"planet"};
    sharedReduced.fit.ok = true;
    sharedReduced.fitState = RegressionComparisonFitState::Valid;
    sharedReduced.isStale = false;
    sharedReduced.fit.n = 60;
    GLMCoefficientRow sharedIntercept;
    sharedIntercept.term = "(Intercept)";
    sharedIntercept.sourceTerm = "(Intercept)";
    sharedIntercept.estimate = 9.95;
    sharedIntercept.pValue = 0.0001;
    GLMCoefficientRow sharedFactorParent;
    sharedFactorParent.term = "planet";
    sharedFactorParent.sourceTerm = "planet";
    sharedFactorParent.termType = "factor";
    sharedFactorParent.rowType = "factor_parent";
    GLMCoefficientRow sharedFactorReference;
    sharedFactorReference.term = "planet=Aurelia";
    sharedFactorReference.sourceTerm = "planet";
    sharedFactorReference.termType = "factor";
    sharedFactorReference.rowType = "reference";
    sharedFactorReference.displayLabel = "Aurelia";
    GLMCoefficientRow sharedFactorLevel;
    sharedFactorLevel.term = "planet=Borealis";
    sharedFactorLevel.sourceTerm = "planet";
    sharedFactorLevel.termType = "factor";
    sharedFactorLevel.rowType = "factor_level";
    sharedFactorLevel.displayLabel = "Borealis";
    sharedFactorLevel.estimate = 0.3;
    sharedFactorLevel.pValue = 0.866;
    sharedReduced.fit.coefficients = {
        sharedIntercept, sharedFactorParent, sharedFactorReference, sharedFactorLevel
    };
    RegressionComparisonModel sharedFull = sharedReduced;
    sharedFull.label = "Full";
    sharedFull.terms.push_back("score");
    GLMCoefficientRow sharedNumeric;
    sharedNumeric.term = "score";
    sharedNumeric.sourceTerm = "score";
    sharedNumeric.estimate = 1.25;
    sharedNumeric.pValue = 0.009;
    sharedFull.fit.coefficients.push_back(sharedNumeric);
    sharedComparisonTable.models = {sharedReduced, sharedFull};

    assert(RegressionComparisonTermDisplay(sharedReduced, "planet").empty());
    assert(RegressionComparisonTermDisplay(sharedReduced, "score") == "\u2014");
    assert(RegressionComparisonTermDisplay(sharedFull, "score") == "1.2500**");
    const ComparisonCopyTable sharedCopy =
        BuildRegressionComparisonCopyTable(sharedComparisonTable);
    assert(sharedCopy.modelLabels == std::vector<std::string>({"Reduced", "Full"}));
    assert(sharedCopy.termLabels == std::vector<std::string>({
        "(Intercept)", "planet", "Aurelia", "Borealis", "score"
    }));
    assert(sharedCopy.termDisplaysByRow[1] == std::vector<std::string>({"", ""}));
    assert(sharedCopy.termDisplaysByRow[4] ==
           std::vector<std::string>({"", "1.2500**"}));
    const std::string sharedCopyText = ComparisonCopyTableText(sharedCopy);
    assert(sharedCopyText.find("Term\tReduced\tFull") != std::string::npos);
    assert(sharedCopyText.find("score\t\t1.2500**") != std::string::npos);

    // A model column inherits the shared response when it does not override
    // it.  This is the native state used by the Windows comparison window.
    assert(RegressionComparisonEffectiveResponse(sharedComparisonTable, sharedReduced) ==
           "outcome");
    sharedFull.response = "other_outcome";
    assert(RegressionComparisonEffectiveResponse(sharedComparisonTable, sharedFull) ==
           "other_outcome");

    // Inclusion, coefficient availability and the fit lifecycle are distinct
    // states.  A missing/excluded value is blank; only a fitted reference
    // category has a statistical em dash.
    const auto excludedScore = RegressionComparisonTermCell(
        sharedComparisonTable, 0, "score");
    assert(!excludedScore.termIncluded);
    assert(excludedScore.inclusionControlAvailable);
    assert(excludedScore.displayState == RegressionComparisonCellDisplayState::Blank);
    assert(excludedScore.displayText.empty());
    const auto referencePlanet = RegressionComparisonTermCell(
        sharedComparisonTable, 0, "planet=Aurelia");
    const auto includedPlanet = RegressionComparisonTermCell(
        sharedComparisonTable, 0, "planet");
    assert(includedPlanet.termIncluded);
    assert(includedPlanet.inclusionControlAvailable);
    assert(referencePlanet.termIncluded);
    assert(referencePlanet.factorLevel);
    assert(referencePlanet.displayState ==
           RegressionComparisonCellDisplayState::NotApplicable);
    assert(referencePlanet.displayText == "\u2014");
    const auto validN = RegressionComparisonFitCell(sharedComparisonTable, 0, 0);
    assert(validN.displayState == RegressionComparisonCellDisplayState::Value);
    assert(validN.displayText == "60");

    RegressionComparisonState pendingComparison = sharedComparisonTable;
    pendingComparison.rFitPending = true;
    pendingComparison.models[0].isStale = true;
    pendingComparison.models[0].fitState = RegressionComparisonFitState::Pending;
    assert(RegressionComparisonTermCell(pendingComparison, 0, "planet=Aurelia")
               .displayText.empty());
    assert(RegressionComparisonFitCell(pendingComparison, 0, 0).displayText.empty());

    RegressionComparisonState failedComparison = sharedComparisonTable;
    failedComparison.models[0].fit.ok = false;
    failedComparison.models[0].fit.warning = "response not found";
    failedComparison.models[0].fitState = RegressionComparisonFitState::Error;
    assert(RegressionComparisonTermCell(failedComparison, 0, "planet=Aurelia")
               .displayText.empty());
    assert(RegressionComparisonFitCell(failedComparison, 0, 0).displayText.empty());

    // The two native comparison tables share one event/state model.  These
    // transitions cover the sequences that previously diverged between
    // Windows and macOS (active column, direct add, +/- and stale rows).
    RegressionComparisonState editedComparison;
    editedComparison.id = "shared-editing";
    editedComparison.group = "cars";
    editedComparison.response = "mpg";
    editedComparison.termRows = {"(Intercept)"};
    RegressionComparisonModel firstEditingModel;
    firstEditingModel.id = "shared-editing-model-1";
    firstEditingModel.label = "Untitled 1";
    firstEditingModel.response = "mpg";
    editedComparison.models = {firstEditingModel};
    const std::vector<std::string> comparisonVariables =
        rlispstat::core::AvailableVariableNames(
            editedComparison.seed, &comparisonDataframe);
    assert(std::find(comparisonVariables.begin(), comparisonVariables.end(), "cyl") !=
           comparisonVariables.end());
    assert(rlispstat::core::IncludeRegressionComparisonTerm(
        editedComparison, 0, "cyl", comparisonVariables));
    assert(editedComparison.activeModel == 0);
    assert(editedComparison.models[0].terms ==
           std::vector<std::string>({"cyl"}));
    assert(editedComparison.models[0].isStale);

    GLMCoefficientRow editedFactorParent = sharedFactorParent;
    editedFactorParent.term = editedFactorParent.sourceTerm = "cyl";
    GLMCoefficientRow editedFactorReference = sharedFactorReference;
    editedFactorReference.term = "cyl=4";
    editedFactorReference.sourceTerm = "cyl";
    GLMCoefficientRow editedFactorLevel = sharedFactorLevel;
    editedFactorLevel.term = "cyl=6";
    editedFactorLevel.sourceTerm = "cyl";
    editedComparison.models[0].fit.coefficients = {
        editedFactorParent, editedFactorReference, editedFactorLevel
    };
    editedComparison.models[0].isStale = false;
    rlispstat::core::RefreshRegressionTermRowsFromFits(editedComparison);
    assert(std::find(editedComparison.termRows.begin(), editedComparison.termRows.end(),
                     "cyl=6") != editedComparison.termRows.end());

    // Excluding a factor leaves its reusable base row, removes fitted child
    // rows and cannot resurrect those children from the now-stale old fit.
    assert(rlispstat::core::ExcludeRegressionComparisonTerm(
        editedComparison, 0, "cyl"));
    assert(editedComparison.models[0].terms.empty());
    assert(std::find(editedComparison.termRows.begin(), editedComparison.termRows.end(),
                     "cyl") != editedComparison.termRows.end());
    assert(std::find(editedComparison.termRows.begin(), editedComparison.termRows.end(),
                     "cyl=6") == editedComparison.termRows.end());
    rlispstat::core::RefreshRegressionTermRowsFromFits(editedComparison);
    assert(std::find(editedComparison.termRows.begin(), editedComparison.termRows.end(),
                     "cyl=6") == editedComparison.termRows.end());

    assert(rlispstat::core::IncludeRegressionComparisonTerm(
        editedComparison, 0, "wt", comparisonVariables));
    const int secondModel = rlispstat::core::AddRegressionComparisonModel(
        editedComparison, editedComparison.activeModel, false);
    assert(secondModel == 1);
    assert(editedComparison.activeModel == 1);
    assert(editedComparison.models[1].terms ==
           std::vector<std::string>({"wt"}));
    assert(rlispstat::core::IncludeRegressionComparisonTerm(
        editedComparison, 1, "wt:cyl", comparisonVariables));
    assert(editedComparison.models[1].terms ==
           std::vector<std::string>({"wt", "cyl", "wt:cyl"}));
    assert(rlispstat::core::ReplaceRegressionComparisonTerm(
        editedComparison, 1, "wt", "hp", comparisonVariables));
    assert(editedComparison.models[0].terms ==
           std::vector<std::string>({"wt"}));
    assert(editedComparison.models[1].terms ==
           std::vector<std::string>({"hp", "cyl", "hp:cyl"}));
    assert(editedComparison.models[1].isStale);
    assert(editedComparison.activeModel == 1);
    assert(std::find(editedComparison.termRows.begin(), editedComparison.termRows.end(),
                     "hp") != editedComparison.termRows.end());
    assert(std::find(editedComparison.termRows.begin(), editedComparison.termRows.end(),
                     "wt") != editedComparison.termRows.end());

    assert(rlispstat::core::SetRegressionComparisonActiveModel(
        editedComparison, 0));
    assert(rlispstat::core::ToggleRegressionComparisonTerm(
        editedComparison, 0, "wt", comparisonVariables));
    assert(!rlispstat::core::ModelIncludesTerm(editedComparison.models[0], "wt"));
    assert(editedComparison.activeModel == 0);
    assert(rlispstat::core::ToggleRegressionComparisonTerm(
        editedComparison, 0, "wt", comparisonVariables));
    assert(rlispstat::core::ModelIncludesTerm(editedComparison.models[0], "wt"));
    assert(rlispstat::core::RemoveRegressionComparisonTermCompletely(
        editedComparison, "cyl"));
    assert(!rlispstat::core::ModelIncludesTerm(editedComparison.models[1], "cyl"));
    assert(!rlispstat::core::ModelIncludesTerm(editedComparison.models[1], "wt:cyl"));
    assert(std::find(editedComparison.termRows.begin(), editedComparison.termRows.end(),
                     "cyl") == editedComparison.termRows.end());
    const int emptyModel = rlispstat::core::AddRegressionComparisonModel(
        editedComparison, editedComparison.activeModel, true);
    assert(emptyModel == 2);
    assert(editedComparison.models[2].terms.empty());
    assert(rlispstat::core::DeleteRegressionComparisonModel(
        editedComparison, 2));
    assert(editedComparison.models.size() == 2);

    // Deleting a column before the active one keeps the same logical model
    // active after its numeric index shifts to the left.
    const int thirdModel = rlispstat::core::AddRegressionComparisonModel(
        editedComparison, 1, false);
    assert(thirdModel == 2);
    const std::string activeModelId = editedComparison.models[2].id;
    assert(rlispstat::core::DeleteRegressionComparisonModel(
        editedComparison, 0));
    assert(editedComparison.activeModel == 1);
    assert(editedComparison.models[1].id == activeModelId);

    // Linear comparison uses the same column-local inclusion contract for
    // ordinary and MI data, including factors, ordered factors and
    // interactions. Auto-refit is deliberately irrelevant to the mutation.
    for (bool multipleImputation : {false, true}) {
        RegressionComparisonState linearControls;
        linearControls.id = multipleImputation
            ? "linear-controls-mi" : "linear-controls-ordinary";
        linearControls.group = multipleImputation ? "mi_data" : "ordinary_data";
        linearControls.response = "y";
        linearControls.multipleImputation = multipleImputation;
        linearControls.imputationCount = multipleImputation ? 5 : 0;
        linearControls.autoRefit = true;
        linearControls.termTypes = {{"x", "numeric"}, {"g", "factor"},
                                    {"rating", "factor"}};
        assert(rlispstat::core::AddRegressionComparisonModel(
            linearControls, -1, true) == 0);
        assert(rlispstat::core::AddRegressionComparisonModel(
            linearControls, 0, true) == 1);
        const std::vector<std::string> variables = {"y", "x", "g", "rating"};
        for (int modelIndex : {0, 1}) {
            assert(rlispstat::core::IncludeRegressionComparisonTerm(
                linearControls, modelIndex, "x", variables));
            assert(rlispstat::core::IncludeRegressionComparisonTerm(
                linearControls, modelIndex, "g", variables));
            assert(rlispstat::core::IncludeRegressionComparisonTerm(
                linearControls, modelIndex, "rating", variables));
            assert(rlispstat::core::IncludeRegressionComparisonTerm(
                linearControls, modelIndex, "x:g", variables));
        }
        rlispstat::core::RefreshRegressionTermRowsFromFits(linearControls);
        const auto secondBefore = linearControls.models[1].terms;
        assert(rlispstat::core::ExcludeRegressionComparisonTerm(
            linearControls, 0, "x:g"));
        rlispstat::core::RefreshRegressionTermRowsFromFits(linearControls);
        assert(!rlispstat::core::ModelIncludesTerm(linearControls.models[0], "x:g"));
        assert(linearControls.models[1].terms == secondBefore);
        assert(rlispstat::core::ModelIncludesTerm(linearControls.models[1], "x:g"));
        assert(std::find(linearControls.termRows.begin(), linearControls.termRows.end(),
                         "x:g") != linearControls.termRows.end());
        const auto interactionPlus = rlispstat::core::RegressionComparisonTermCell(
            linearControls, 0, "x:g");
        const auto interactionMinus = rlispstat::core::RegressionComparisonTermCell(
            linearControls, 1, "x:g");
        assert(!interactionPlus.termIncluded && interactionPlus.inclusionControlAvailable);
        assert(interactionMinus.termIncluded && interactionMinus.inclusionControlAvailable);

        linearControls.autoRefit = false;
        assert(rlispstat::core::ExcludeRegressionComparisonTerm(
            linearControls, 0, "g"));
        rlispstat::core::RefreshRegressionTermRowsFromFits(linearControls);
        assert(!rlispstat::core::ModelIncludesTerm(linearControls.models[0], "g"));
        assert(rlispstat::core::ModelIncludesTerm(linearControls.models[1], "g"));
        assert(std::find(linearControls.termRows.begin(), linearControls.termRows.end(),
                         "g") != linearControls.termRows.end());
        const auto factorPlus = rlispstat::core::RegressionComparisonTermCell(
            linearControls, 0, "g");
        assert(!factorPlus.termIncluded && factorPlus.inclusionControlAvailable);
        assert(!linearControls.autoRefit);

        assert(rlispstat::core::ExcludeRegressionComparisonTerm(
            linearControls, 0, "rating"));
        assert(rlispstat::core::ModelIncludesTerm(linearControls.models[1], "rating"));
        assert(rlispstat::core::IncludeRegressionComparisonTerm(
            linearControls, 0, "rating", variables));
        assert(rlispstat::core::ExcludeRegressionComparisonTerm(
            linearControls, 0, "rating"));
        assert(rlispstat::core::RemoveRegressionComparisonTermCompletely(
            linearControls, "g"));
        for (const auto &model : linearControls.models) {
            assert(!rlispstat::core::ModelIncludesTerm(model, "g"));
            assert(!rlispstat::core::ModelIncludesTerm(model, "x:g"));
            assert(std::find(model.candidateTerms.begin(), model.candidateTerms.end(),
                             "g") == model.candidateTerms.end());
            assert(std::find(model.candidateTerms.begin(), model.candidateTerms.end(),
                             "x:g") == model.candidateTerms.end());
        }
        assert(std::find(linearControls.termRows.begin(), linearControls.termRows.end(),
                         "g") == linearControls.termRows.end());
        assert(std::find(linearControls.termRows.begin(), linearControls.termRows.end(),
                         "x:g") == linearControls.termRows.end());
    }

    GLMCoefficientRow numericCoefficient;
    numericCoefficient.term = "x";
    numericCoefficient.sourceTerm = "x";
    GLMCoefficientRow factorCoefficient;
    factorCoefficient.term = "groupB";
    factorCoefficient.sourceTerm = "group";
    std::vector<std::string> regressionRows = RegressionComparisonTermRowsFromFits(
        {"(Intercept)", "groupB"}, {{"x", "group"}}, {{numericCoefficient, factorCoefficient}});
    assert(regressionRows == std::vector<std::string>({"(Intercept)", "x", "groupB"}));
    GLMCoefficientRow interactionParent;
    interactionParent.term = "x:group";
    interactionParent.sourceTerm = "x:group";
    interactionParent.termType = "numeric_factor_interaction";
    interactionParent.rowType = "term_parent";
    GLMCoefficientRow interactionChild;
    interactionChild.term = "x:group=B";
    interactionChild.sourceTerm = "x:group";
    interactionChild.termType = "numeric_factor_interaction";
    interactionChild.rowType = "coefficient";
    regressionRows = RegressionComparisonTermRowsFromFits(
        {"(Intercept)"}, {{"x", "group", "x:group"}}, {{interactionParent, interactionChild}});
    assert(regressionRows == std::vector<std::string>({"(Intercept)", "x", "group", "x:group", "x:group=B"}));
    regressionRows = RegressionComparisonTermRowsFromFits(
        {"(Intercept)", "cyl=6:am"}, {{}}, {{}});
    assert(regressionRows == std::vector<std::string>({"(Intercept)", "cyl:am"}));

    RegressionComparisonState stableRowOrder;
    stableRowOrder.termRows = {
        "(Intercept)", "group", "country", "group:country",
        "groupTreatment:countryGreece"
    };
    RegressionComparisonModel earlierModel;
    earlierModel.terms = {"group"};
    earlierModel.candidateTerms = {"group"};
    RegressionComparisonModel laterModel;
    laterModel.terms = {"country", "group", "group:country"};
    laterModel.candidateTerms = laterModel.terms;
    stableRowOrder.models = {earlierModel, laterModel};
    // Adding country to the earlier model must not reorder the already-visible
    // country/group interaction hierarchy by model traversal order.
    stableRowOrder.models[0].terms.push_back("country");
    stableRowOrder.models[0].candidateTerms.push_back("country");
    RefreshRegressionTermRowsFromFits(stableRowOrder);
    assert(stableRowOrder.termRows == std::vector<std::string>(
        {"(Intercept)", "group", "country", "group:country"}));

    rlispstat::core::GeneralizedComparisonState stableGeneralizedOrder;
    stableGeneralizedOrder.termRows = {
        "(Intercept)", "group", "country", "group:country"
    };
    rlispstat::core::GeneralizedComparisonModel earlierGeneralized;
    earlierGeneralized.terms = {"group", "country"};
    earlierGeneralized.candidateTerms = earlierGeneralized.terms;
    earlierGeneralized.isStale = true;
    rlispstat::core::GeneralizedComparisonModel laterGeneralized;
    laterGeneralized.terms = {"country", "group", "group:country"};
    laterGeneralized.candidateTerms = laterGeneralized.terms;
    laterGeneralized.isStale = true;
    stableGeneralizedOrder.models = {earlierGeneralized, laterGeneralized};
    RefreshGeneralizedTermRowsFromFits(stableGeneralizedOrder);
    assert(stableGeneralizedOrder.termRows == std::vector<std::string>(
        {"(Intercept)", "group", "country", "group:country"}));

    GeneralizedGLMRow generalizedCoefficient;
    generalizedCoefficient.term = "groupB";
    std::vector<std::string> generalizedTermRows = GeneralizedComparisonTermRowsFromFits(
        {{"x", "group"}}, {{generalizedCoefficient}});
    assert(generalizedTermRows == std::vector<std::string>({"(Intercept)", "x", "group"}));

    GeneralizedGLMRow generalizedFactorParent;
    generalizedFactorParent.term = "am";
    generalizedFactorParent.sourceTerm = "am";
    generalizedFactorParent.rowType = "factor_parent";
    GeneralizedGLMRow generalizedFactorReference;
    generalizedFactorReference.term = "0";
    generalizedFactorReference.sourceTerm = "am";
    generalizedFactorReference.rowType = "reference";
    GeneralizedGLMRow generalizedFactorLevel;
    generalizedFactorLevel.term = "1";
    generalizedFactorLevel.sourceTerm = "am";
    generalizedFactorLevel.rowType = "factor_level";
    GeneralizedGLMRow generalizedNumeric;
    generalizedNumeric.term = "wt";
    generalizedNumeric.sourceTerm = "wt";
    generalizedNumeric.rowType = "coefficient";
    generalizedTermRows = GeneralizedComparisonTermRowsFromFits(
        {{"mpg", "am", "wt"}},
        {{generalizedFactorParent, generalizedFactorReference, generalizedFactorLevel, generalizedNumeric}});
    assert(generalizedTermRows == std::vector<std::string>({"(Intercept)", "mpg", "am", "0", "1", "wt"}));

    // Numeric-by-numeric interactions have a semantic parent and a fitted
    // coefficient with the same R term name.  Both rows must survive the
    // comparison presentation registry.
    GeneralizedGLMRow generalizedInteractionParent;
    generalizedInteractionParent.term = "x:z";
    generalizedInteractionParent.displayLabel = "x:z";
    generalizedInteractionParent.sourceTerm = "x:z";
    generalizedInteractionParent.termType = "numeric_numeric_interaction";
    generalizedInteractionParent.rowType = "term_parent";
    GeneralizedGLMRow generalizedInteractionChild;
    generalizedInteractionChild.term = "x:z";
    generalizedInteractionChild.displayLabel = "  x:z";
    generalizedInteractionChild.sourceTerm = "x:z";
    generalizedInteractionChild.termType = "numeric_numeric_interaction";
    generalizedInteractionChild.rowType = "coefficient";
    generalizedInteractionChild.estimate = -0.0852;
    generalizedInteractionChild.pValue = 0.142;
    const std::vector<GeneralizedGLMRow> generalizedInteractionRows = {
        generalizedInteractionParent, generalizedInteractionChild};
    generalizedTermRows = GeneralizedComparisonTermRowsFromFits(
        {{"x", "z", "x:z"}}, {generalizedInteractionRows});
    assert(generalizedTermRows.size() == 5);
    assert(generalizedTermRows[3] == "x:z");
    assert(generalizedTermRows[4] != "x:z");
    const GeneralizedGLMRow *resolvedInteractionParent =
        GeneralizedComparisonRowForPresentationTerm(
            generalizedInteractionRows, generalizedTermRows[3]);
    const GeneralizedGLMRow *resolvedInteractionChild =
        GeneralizedComparisonRowForPresentationTerm(
            generalizedInteractionRows, generalizedTermRows[4]);
    assert(resolvedInteractionParent &&
           resolvedInteractionParent->rowType == "term_parent");
    assert(resolvedInteractionChild &&
           resolvedInteractionChild->rowType == "coefficient");
    assert(rlispstat::core::GeneralizedCoefficientDisplay(
               generalizedInteractionRows, true, {"x", "z", "x:z"},
               generalizedTermRows[3]).empty());
    assert(rlispstat::core::GeneralizedCoefficientDisplay(
               generalizedInteractionRows, true, {"x", "z", "x:z"},
               generalizedTermRows[4]) == "-0.085");

    // End-to-end comparison presentation for the reported MI Count model.
    // The immutable fit identity, fit statistics, factor hierarchy, interaction
    // parent and interaction coefficient must be accepted as one snapshot.
    {
        rlispstat::core::GeneralizedGLMState source;
        source.id = "mi-count-interaction-source";
        source.group = "mi_test_mids";
        source.response = "y_count";
        source.family = "poisson";
        source.link = "log";
        source.countRegression = true;
        source.multipleImputation = true;
        source.imputationCount = 5;
        source.datasetType = "multiple_imputation";
        source.imputationSetId = "mi-count-interaction-set";
        source.sourceDatasetId = "mi-count-interaction-data";
        source.terms = {"x", "z", "g", "x:z"};
        source.termTypes = {{"x", "numeric"}, {"z", "numeric"}, {"g", "factor"}};
        source.termTypeOverrides = {{"g", "factor"}};
        source.factorReferenceLevels = {{"g", "A"}};
        source.modelVersion = 23;
        source.fitVersion = 23;
        source.ok = true;
        source.n = 400;
        source.rank = 6;
        source.parameterCount = 6;
        source.dfResidual = 394;
        source.residualDeviance = 1380.790;

        auto semanticRow = [](const std::string &rowType,
                              const std::string &term,
                              const std::string &label,
                              const std::string &sourceTerm,
                              const std::string &termType) {
            GeneralizedGLMRow row;
            row.rowType = rowType;
            row.term = term;
            row.displayLabel = label;
            row.sourceTerm = sourceTerm;
            row.termType = termType;
            return row;
        };
        GeneralizedGLMRow intercept = semanticRow(
            "coefficient", "(Intercept)", "(Intercept)", "(Intercept)", "intercept");
        intercept.estimate = 0.720;
        GeneralizedGLMRow x = semanticRow("coefficient", "x", "x", "x", "numeric");
        x.estimate = 0.410;
        GeneralizedGLMRow z = semanticRow("coefficient", "z", "z", "z", "numeric");
        z.estimate = -0.261;
        GeneralizedGLMRow gParent = semanticRow(
            "factor_parent", "g", "g", "g", "factor");
        gParent.referenceLevel = "A";
        GeneralizedGLMRow gReference = semanticRow(
            "reference", "g=A", "  A (reference)", "g", "factor");
        gReference.factorLevel = "A";
        gReference.referenceLevel = "A";
        GeneralizedGLMRow gB = semanticRow(
            "factor_level", "g=B", "  B", "g", "factor");
        gB.factorLevel = "B";
        gB.referenceLevel = "A";
        gB.estimate = 0.519;
        GeneralizedGLMRow gC = semanticRow(
            "factor_level", "g=C", "  C", "g", "factor");
        gC.factorLevel = "C";
        gC.referenceLevel = "A";
        gC.estimate = 0.053;
        GeneralizedGLMRow interactionParent = semanticRow(
            "term_parent", "x:z", "x:z", "x:z", "numeric_numeric_interaction");
        GeneralizedGLMRow interactionCoefficient = semanticRow(
            "coefficient", "x:z", "  x:z", "x:z", "numeric_numeric_interaction");
        interactionCoefficient.estimate = -0.0852;
        interactionCoefficient.pValue = 0.142;
        source.rows = {intercept, x, z, gParent, gReference, gB, gC,
                       interactionParent, interactionCoefficient};
        source.lastRFitSignature = rlispstat::core::GeneralizedGLMFitSignature(source);

        auto comparison = rlispstat::core::CreateGeneralizedComparisonFromSingleModel(
            "mi-count-interaction-comparison", source, "Model 1");
        assert(comparison.models.size() == 1);
        assert(comparison.models[0].fitState ==
               rlispstat::core::RegressionComparisonFitState::Valid);
        assert(comparison.models[0].fit.rank == 6);
        assert(comparison.models[0].fit.parameterCount == 6);
        assert(comparison.models[0].fit.dfResidual == 394);
        assert(comparison.termRows.size() == 9);
        assert(rlispstat::core::GeneralizedComparisonDisplayLabel(
                   comparison, comparison.termRows[7]) == "x:z");
        assert(rlispstat::core::GeneralizedComparisonDisplayLabel(
                   comparison, comparison.termRows[8]) == "  x:z");
        const auto interactionParentCell =
            rlispstat::core::GeneralizedComparisonTermCell(
                comparison, 0, comparison.termRows[7]);
        const auto interactionCoefficientCell =
            rlispstat::core::GeneralizedComparisonTermCell(
                comparison, 0, comparison.termRows[8]);
        assert(interactionParentCell.displayState ==
               rlispstat::core::RegressionComparisonCellDisplayState::Blank);
        assert(interactionParentCell.termIncluded);
        assert(interactionParentCell.inclusionControlAvailable);
        assert(interactionCoefficientCell.displayState ==
               rlispstat::core::RegressionComparisonCellDisplayState::Value);
        assert(interactionCoefficientCell.displayText == "-0.085");
        std::string consistencyReason;
        assert(rlispstat::core::GeneralizedComparisonPresentationIsConsistent(
            comparison, &consistencyReason));
    }
    generalizedFactorReference.sourceTerm = "";
    generalizedFactorLevel.sourceTerm = "";
    generalizedTermRows = GeneralizedComparisonTermRowsFromFits(
        {{"mpg", "am", "wt"}},
        {{generalizedFactorParent, generalizedFactorReference, generalizedFactorLevel, generalizedNumeric}});
    assert(generalizedTermRows == std::vector<std::string>({"(Intercept)", "mpg", "am", "0", "1", "wt"}));

    // Generalized model comparison uses one semantic specification per model
    // column.  An intercept-only first column is a real editable model, not a
    // placeholder that requires opening a standalone GLM editor.
    {
        rlispstat::core::GeneralizedComparisonState comparison;
        comparison.id = "generalized-comparison-test";
        comparison.group = "data";
        comparison.response = "y";
        comparison.family = "gaussian";
        comparison.link = "identity";
        const int first = rlispstat::core::AddGeneralizedComparisonModel(
            comparison, -1, true);
        assert(first == 0);
        assert(comparison.models.size() == 1);
        assert(comparison.models[0].terms.empty());
        rlispstat::core::RefreshGeneralizedTermRowsFromFits(comparison);
        assert(comparison.termRows == std::vector<std::string>({"(Intercept)"}));
        const auto pendingIntercept = rlispstat::core::GeneralizedComparisonTermCell(
            comparison, 0, "(Intercept)");
        assert(pendingIntercept.termIncluded);
        assert(pendingIntercept.displayState ==
               rlispstat::core::RegressionComparisonCellDisplayState::Blank);

        // Adding a term to the clicked second model must not mutate Model 1.
        const int second = rlispstat::core::AddGeneralizedComparisonModel(
            comparison, 0, true);
        assert(second == 1);
        assert(rlispstat::core::IncludeGeneralizedComparisonTerm(
            comparison, 1, "x", {"x", "group"}));
        assert(comparison.models[0].terms.empty());
        assert(comparison.models[1].terms == std::vector<std::string>({"x"}));
        assert(comparison.activeModel == 1);

        // Predictor interpretation and centering also belong to one model
        // specification.  They cannot leak to another comparison column.
        rlispstat::core::ModelPredictorMetadata xMetadata;
        xMetadata.variable = "x";
        xMetadata.storageType = "numeric";
        xMetadata.observedCount = 12;
        xMetadata.uniqueCount = 3;
        xMetadata.levels = {"1", "2", "3"};
        assert(rlispstat::core::ApplyGeneralizedComparisonTermType(
            comparison, 1, "x", "factor", xMetadata));
        assert(rlispstat::core::GeneralizedComparisonModelTermType(
            comparison, 1, "x") == "factor");
        assert(comparison.models[0].termTypeOverrides.empty());
        assert(!rlispstat::core::ToggleGeneralizedComparisonPredictorCentering(
            comparison, 1, "x"));
        assert(rlispstat::core::ApplyGeneralizedComparisonTermType(
            comparison, 1, "x", "numeric", xMetadata));
        assert(rlispstat::core::ToggleGeneralizedComparisonPredictorCentering(
            comparison, 1, "x"));
        assert(comparison.models[1].centeredPredictors.count("x") == 1);
        assert(comparison.models[0].centeredPredictors.empty());

        // A returned fit replaces the pending cell state for exactly that
        // specification and supplies the matrix/fit rows directly.
        auto &model = comparison.models[0];
        model.isStale = false;
        model.fitState = rlispstat::core::RegressionComparisonFitState::Valid;
        model.fit.ok = true;
        model.fit.family = "gaussian";
        model.fit.link = "identity";
        model.fit.n = 12;
        model.fit.aic = 42.5;
        rlispstat::core::GeneralizedGLMRow intercept;
        intercept.rowType = "coefficient";
        intercept.term = "(Intercept)";
        intercept.sourceTerm = "(Intercept)";
        intercept.estimate = 4.25;
        intercept.pValue = 0.01;
        model.fit.rows = {intercept};
        const std::string fittedFingerprint =
            rlispstat::core::GeneralizedComparisonModelSpecificationFingerprint(
                comparison, model);
        model.requestedSpecificationRevision = model.modelVersion;
        model.requestedSpecificationFingerprint = fittedFingerprint;
        model.fitSpecificationRevision = model.modelVersion;
        model.fitSpecificationFingerprint = fittedFingerprint;
        model.fitVersion = model.modelVersion;
        model.fit.lastRFitSignature = fittedFingerprint;
        rlispstat::core::RefreshGeneralizedTermRowsFromFits(comparison);
        const auto fittedIntercept = rlispstat::core::GeneralizedComparisonTermCell(
            comparison, 0, "(Intercept)");
        assert(fittedIntercept.displayState ==
               rlispstat::core::RegressionComparisonCellDisplayState::Value);
        assert(fittedIntercept.displayText == "4.250*");
        const auto nCell = rlispstat::core::GeneralizedComparisonFitCell(
            comparison, 0, 2);
        assert(nCell.displayState ==
               rlispstat::core::RegressionComparisonCellDisplayState::Value);
        assert(nCell.displayText == "12");

        assert(rlispstat::core::RemoveGeneralizedComparisonTermCompletely(
            comparison, "x"));
        assert(comparison.models[1].terms.empty());
        assert(comparison.models[1].centeredPredictors.empty());
    }

    // A higher-order interaction edit is reversible.  Hierarchical lower-
    // order interactions introduced by that edit must disappear when the
    // higher-order term is removed, while a lower-order interaction that was
    // already in the model remains explicit.
    {
        rlispstat::core::GeneralizedComparisonState comparison;
        comparison.id = "generalized-interaction-undo";
        comparison.group = "mi_test_mids";
        comparison.response = "y_bin";
        comparison.family = "binomial";
        comparison.link = "logit";
        comparison.binaryComparison = true;
        assert(rlispstat::core::AddGeneralizedComparisonModel(
            comparison, -1, true) == 0);
        const std::vector<std::string> variables = {"x", "z", "g"};
        assert(rlispstat::core::IncludeGeneralizedComparisonTerm(
            comparison, 0, "x", variables));
        assert(rlispstat::core::IncludeGeneralizedComparisonTerm(
            comparison, 0, "z", variables));
        assert(rlispstat::core::IncludeGeneralizedComparisonTerm(
            comparison, 0, "g", variables));
        assert(rlispstat::core::IncludeGeneralizedComparisonTerm(
            comparison, 0, "x:g", variables));
        const std::vector<std::string> baseline = {"x", "z", "g", "x:g"};
        assert(comparison.models[0].terms == baseline);
        const std::string baselineFingerprint =
            rlispstat::core::GeneralizedComparisonModelSpecificationFingerprint(
                comparison, comparison.models[0]);

        for (int edit = 0; edit < 4; ++edit) {
            assert(rlispstat::core::IncludeGeneralizedComparisonTerm(
                comparison, 0, "x:z:g", variables));
            assert(rlispstat::core::GeneralizedModelIncludesTerm(
                comparison.models[0], "x:z"));
            assert(rlispstat::core::GeneralizedModelIncludesTerm(
                comparison.models[0], "z:g"));
            assert(rlispstat::core::GeneralizedModelIncludesTerm(
                comparison.models[0], "x:z:g"));
            assert(rlispstat::core::ExcludeGeneralizedComparisonTerm(
                comparison, 0, "x:z:g"));
            assert(comparison.models[0].terms == baseline);
            assert(rlispstat::core::GeneralizedComparisonModelSpecificationFingerprint(
                       comparison, comparison.models[0]) == baselineFingerprint);
            assert(std::find(comparison.termRows.begin(), comparison.termRows.end(),
                             "x:z") == comparison.termRows.end());
            assert(std::find(comparison.termRows.begin(), comparison.termRows.end(),
                             "z:g") == comparison.termRows.end());
            assert(std::find(comparison.termRows.begin(), comparison.termRows.end(),
                             "x:z:g") != comparison.termRows.end());
            const auto excludedInteraction =
                rlispstat::core::GeneralizedComparisonTermCell(
                    comparison, 0, "x:z:g");
            assert(!excludedInteraction.termIncluded);
            assert(excludedInteraction.inclusionControlAvailable);
            assert(std::find(comparison.termRows.begin(), comparison.termRows.end(),
                             "x:g") != comparison.termRows.end());
        }
    }

    // The +/- contract is shared by every generalized-comparison family and
    // is independent of ordinary versus multiple-imputation fitting. A minus
    // changes only the clicked column; the candidate row remains available as
    // plus until an explicit comparison-wide removal is requested.
    {
        struct ComparisonFamilyCase {
            const char *name;
            const char *family;
            const char *link;
            bool binary;
            bool count;
            rlispstat::core::CountDistribution distribution;
        };
        const std::vector<ComparisonFamilyCase> families = {
            {"generalized", "gaussian", "identity", false, false,
             rlispstat::core::CountDistribution::Poisson},
            {"binary-logit", "binomial", "logit", true, false,
             rlispstat::core::CountDistribution::Poisson},
            {"binary-probit", "binomial", "probit", true, false,
             rlispstat::core::CountDistribution::Poisson},
            {"binary-cloglog", "binomial", "cloglog", true, false,
             rlispstat::core::CountDistribution::Poisson},
            {"count-poisson", "poisson", "log", false, true,
             rlispstat::core::CountDistribution::Poisson},
            {"count-quasipoisson", "quasipoisson", "log", false, true,
             rlispstat::core::CountDistribution::QuasiPoisson},
            {"count-negative-binomial", "poisson", "log", false, true,
             rlispstat::core::CountDistribution::NegativeBinomial}
        };
        const std::vector<std::string> variables = {"y", "x", "g", "rating"};
        for (const ComparisonFamilyCase &familyCase : families) {
            for (bool multipleImputation : {false, true}) {
                rlispstat::core::GeneralizedComparisonState comparison;
                comparison.id = std::string(familyCase.name) +
                    (multipleImputation ? "-mi" : "-ordinary");
                comparison.group = multipleImputation ? "mi_data" : "ordinary_data";
                comparison.response = "y";
                comparison.family = familyCase.family;
                comparison.link = familyCase.link;
                comparison.binaryComparison = familyCase.binary;
                comparison.countComparison = familyCase.count;
                comparison.multipleImputation = multipleImputation;
                comparison.imputationCount = multipleImputation ? 5 : 0;
                comparison.autoRefit = true;
                comparison.termTypes = {{"x", "numeric"}, {"g", "factor"},
                                        {"rating", "factor"}};
                assert(rlispstat::core::AddGeneralizedComparisonModel(
                    comparison, -1, true) == 0);
                assert(rlispstat::core::AddGeneralizedComparisonModel(
                    comparison, 0, true) == 1);
                for (int modelIndex : {0, 1}) {
                    comparison.models[static_cast<std::size_t>(modelIndex)].family =
                        familyCase.family;
                    comparison.models[static_cast<std::size_t>(modelIndex)].link =
                        familyCase.link;
                    comparison.models[static_cast<std::size_t>(modelIndex)].countDistribution =
                        familyCase.distribution;
                    assert(rlispstat::core::IncludeGeneralizedComparisonTerm(
                        comparison, modelIndex, "x", variables));
                    assert(rlispstat::core::IncludeGeneralizedComparisonTerm(
                        comparison, modelIndex, "g", variables));
                    assert(rlispstat::core::IncludeGeneralizedComparisonTerm(
                        comparison, modelIndex, "rating", variables));
                    assert(rlispstat::core::IncludeGeneralizedComparisonTerm(
                        comparison, modelIndex, "x:g", variables));
                }

                const auto secondBefore = comparison.models[1].terms;
                assert(rlispstat::core::ExcludeGeneralizedComparisonTerm(
                    comparison, 0, "x:g"));
                assert(!rlispstat::core::GeneralizedModelIncludesTerm(
                    comparison.models[0], "x:g"));
                assert(comparison.models[1].terms == secondBefore);
                assert(rlispstat::core::GeneralizedModelIncludesTerm(
                    comparison.models[1], "x:g"));
                assert(std::find(comparison.termRows.begin(), comparison.termRows.end(),
                                 "x:g") != comparison.termRows.end());
                const auto interactionPlus =
                    rlispstat::core::GeneralizedComparisonTermCell(
                        comparison, 0, "x:g");
                const auto interactionMinus =
                    rlispstat::core::GeneralizedComparisonTermCell(
                        comparison, 1, "x:g");
                assert(!interactionPlus.termIncluded &&
                       interactionPlus.inclusionControlAvailable);
                assert(interactionMinus.termIncluded &&
                       interactionMinus.inclusionControlAvailable);

                comparison.autoRefit = false;
                assert(rlispstat::core::ExcludeGeneralizedComparisonTerm(
                    comparison, 0, "g"));
                assert(!rlispstat::core::GeneralizedModelIncludesTerm(
                    comparison.models[0], "g"));
                assert(rlispstat::core::GeneralizedModelIncludesTerm(
                    comparison.models[1], "g"));
                assert(std::find(comparison.termRows.begin(), comparison.termRows.end(),
                                 "g") != comparison.termRows.end());
                const auto factorPlus = rlispstat::core::GeneralizedComparisonTermCell(
                    comparison, 0, "g");
                assert(!factorPlus.termIncluded && factorPlus.inclusionControlAvailable);
                assert(!comparison.autoRefit);

                // Ordered variables use the same factor-valued model term
                // semantics while their level order is retained by R.
                assert(rlispstat::core::ExcludeGeneralizedComparisonTerm(
                    comparison, 0, "rating"));
                assert(!rlispstat::core::GeneralizedModelIncludesTerm(
                    comparison.models[0], "rating"));
                assert(rlispstat::core::GeneralizedModelIncludesTerm(
                    comparison.models[1], "rating"));
                assert(rlispstat::core::IncludeGeneralizedComparisonTerm(
                    comparison, 0, "rating", variables));
                assert(rlispstat::core::ExcludeGeneralizedComparisonTerm(
                    comparison, 0, "rating"));

                assert(rlispstat::core::RemoveGeneralizedComparisonTermCompletely(
                    comparison, "g"));
                for (const auto &model : comparison.models) {
                    assert(!rlispstat::core::GeneralizedModelIncludesTerm(model, "g"));
                    assert(!rlispstat::core::GeneralizedModelIncludesTerm(model, "x:g"));
                    assert(std::find(model.candidateTerms.begin(), model.candidateTerms.end(),
                                     "g") == model.candidateTerms.end());
                    assert(std::find(model.candidateTerms.begin(), model.candidateTerms.end(),
                                     "x:g") == model.candidateTerms.end());
                }
                assert(std::find(comparison.termRows.begin(), comparison.termRows.end(),
                                 "g") == comparison.termRows.end());
                assert(std::find(comparison.termRows.begin(), comparison.termRows.end(),
                                 "x:g") == comparison.termRows.end());
            }
        }
    }

    {
        // Ordinary Generalized Model Comparison exposes one shared family/link
        // specification.  Changing either field invalidates every model
        // column, so no old fit can be shown under a new header.
        rlispstat::core::GeneralizedComparisonState comparison;
        comparison.id = "generalized-family-link-controls";
        comparison.group = "continuous";
        comparison.response = "y";
        comparison.family = "gaussian";
        comparison.link = "identity";
        assert(rlispstat::core::AddGeneralizedComparisonModel(
            comparison, -1, true) == 0);
        assert(rlispstat::core::AddGeneralizedComparisonModel(
            comparison, 0, true) == 1);
        for (auto &model : comparison.models) {
            model.fit.ok = true;
            model.fitState = RegressionComparisonFitState::Valid;
            model.isStale = false;
        }
        assert(rlispstat::core::SetGeneralizedComparisonFamily(
            comparison, "Gamma"));
        assert(comparison.family == "Gamma");
        assert(comparison.link == "inverse");
        for (const auto &model : comparison.models) {
            assert(model.family == "Gamma");
            assert(model.link == "inverse");
            assert(!model.fit.ok);
            assert(model.fitState == RegressionComparisonFitState::NotFitted);
        }
        assert(rlispstat::core::SetGeneralizedComparisonLink(
            comparison, "log"));
        assert(comparison.link == "log");
        for (const auto &model : comparison.models) {
            assert(model.family == "Gamma");
            assert(model.link == "log");
        }
        assert(!rlispstat::core::SetGeneralizedComparisonLink(
            comparison, "logit"));
        assert(!rlispstat::core::SetGeneralizedComparisonFamily(
            comparison, "not-a-family"));

        rlispstat::core::GeneralizedComparisonState proportionComparison;
        proportionComparison.id = "proportion-distribution-controls";
        proportionComparison.group = "proportions";
        proportionComparison.response = "p";
        proportionComparison.modelType =
            rlispstat::core::StatisticalModelType::Proportion;
        proportionComparison.family = "beta";
        proportionComparison.link = "logit";
        assert(rlispstat::core::AddGeneralizedComparisonModel(
            proportionComparison, -1, true) == 0);
        assert(!rlispstat::core::SetGeneralizedComparisonFamily(
            proportionComparison, "Gamma"));
        assert(rlispstat::core::SetGeneralizedComparisonFamily(
            proportionComparison, "beta_one_inflated"));
        assert(proportionComparison.models.front().responseBoundsConfigured);
        assert(proportionComparison.models.front().responseLower == 0.0);
        assert(proportionComparison.models.front().responseUpper == 1.0);
        assert(!rlispstat::core::SetGeneralizedComparisonLink(
            proportionComparison, "identity"));
    }

    // Count Regression comparison reuses the generalized comparison model,
    // but distribution and exposure are independent per model column.
    {
        rlispstat::core::GeneralizedComparisonState comparison;
        comparison.id = "count-comparison-test";
        comparison.group = "counts";
        comparison.response = "events";
        comparison.family = "poisson";
        comparison.link = "log";
        comparison.countComparison = true;
        comparison.hasSeed = true;
        comparison.seed.group = "counts";
        comparison.seed.variables = {
            {"events", {}}, {"x", {}}, {"time", {}}, {"known", {}}};
        assert(rlispstat::core::AddGeneralizedComparisonModel(comparison, -1, true) == 0);
        assert(rlispstat::core::AddGeneralizedComparisonModel(comparison, 0, true) == 1);
        assert(rlispstat::core::IncludeGeneralizedComparisonTerm(
            comparison, 0, "x", {"x", "planet", "time"}));
        assert(rlispstat::core::IncludeGeneralizedComparisonTerm(
            comparison, 1, "x", {"x", "planet", "time"}));
        assert(rlispstat::core::IncludeGeneralizedComparisonTerm(
            comparison, 1, "planet", {"x", "planet", "time"}));

        assert(rlispstat::core::SetGeneralizedComparisonExposure(
            comparison, 1, "time"));
        assert(!rlispstat::core::SetGeneralizedComparisonOffset(
            comparison, 1, "time"));
        assert(rlispstat::core::SetGeneralizedComparisonOffset(
            comparison, 1, "known"));
        assert(!rlispstat::core::SetGeneralizedComparisonExposure(
            comparison, 1, "known"));
        assert(rlispstat::core::SetGeneralizedComparisonOffset(
            comparison, 1, ""));
        assert(rlispstat::core::SetGeneralizedComparisonCountDistribution(
            comparison, 1, rlispstat::core::CountDistribution::NegativeBinomial));
        assert(comparison.models[0].exposure.empty());
        assert(comparison.models[0].countDistribution ==
               rlispstat::core::CountDistribution::Poisson);
        assert(comparison.models[1].exposure == "time");
        assert(comparison.models[1].countDistribution ==
               rlispstat::core::CountDistribution::NegativeBinomial);

        const auto visibleRows = rlispstat::core::GeneralizedComparisonVisibleFitRows(comparison);
        assert(std::find(visibleRows.begin(), visibleRows.end(), 16) != visibleRows.end());
        assert(std::find(visibleRows.begin(), visibleRows.end(), 17) != visibleRows.end());
        assert(std::find(visibleRows.begin(), visibleRows.end(), 18) != visibleRows.end());
        // Specification rows remain visible before a refit, including with
        // Auto-refit disabled.
        comparison.autoRefit = false;
        assert(rlispstat::core::GeneralizedComparisonFitCell(comparison, 1, 16).displayText ==
               "Negative binomial");
        assert(rlispstat::core::GeneralizedComparisonFitCell(comparison, 1, 17).displayText ==
               "time (log offset)");

        auto &poisson = comparison.models[0];
        auto &negativeBinomial = comparison.models[1];
        poisson.fit.ok = true;
        poisson.fit.rowsUsed = {1, 2, 3, 4};
        poisson.fitState = rlispstat::core::RegressionComparisonFitState::Valid;
        poisson.isStale = false;
        negativeBinomial.fit.ok = true;
        negativeBinomial.fit.rowsUsed = {1, 2, 3, 4};
        negativeBinomial.fitState = rlispstat::core::RegressionComparisonFitState::Valid;
        negativeBinomial.isStale = false;
        std::string incompatibility;
        assert(!rlispstat::core::GeneralizedComparisonModelsSupportLikelihoodRatio(
            comparison, poisson, negativeBinomial, &incompatibility));
        assert(incompatibility.find("same count distribution") != std::string::npos);

        assert(rlispstat::core::SetGeneralizedComparisonCountDistribution(
            comparison, 1, rlispstat::core::CountDistribution::Poisson));
        negativeBinomial.fit.ok = true;
        negativeBinomial.fit.rowsUsed = {1, 2, 3, 4};
        negativeBinomial.fitState = rlispstat::core::RegressionComparisonFitState::Valid;
        negativeBinomial.isStale = false;
        assert(!rlispstat::core::GeneralizedComparisonModelsSupportLikelihoodRatio(
            comparison, poisson, negativeBinomial, &incompatibility));
        assert(incompatibility.find("same exposure") != std::string::npos);

        poisson.exposure = "time";
        assert(rlispstat::core::GeneralizedComparisonModelsSupportLikelihoodRatio(
            comparison, poisson, negativeBinomial, &incompatibility));

        assert(rlispstat::core::SetGeneralizedComparisonCountDistribution(
            comparison, 1, rlispstat::core::CountDistribution::QuasiPoisson));
        const auto quasiCapabilities = rlispstat::core::GeneralizedComparisonModelCapabilities(
            comparison, comparison.models[1]);
        assert(!quasiCapabilities.hasLikelihood);
        assert(!quasiCapabilities.hasAIC);
        assert(!quasiCapabilities.supportsNestedLRComparison);

        // Scope and term replacement are model-local semantic edits.  The
        // replacement also rebuilds interactions in the selected model only.
        assert(rlispstat::core::SetGeneralizedComparisonModelScope(
            comparison, 1, "selected"));
        assert(comparison.models[0].scope == "all");
        assert(comparison.models[1].scope == "selected");
        comparison.models[1].terms = {"x", "planet", "x:planet"};
        comparison.models[1].centeredPredictors.insert("x");
        assert(rlispstat::core::ReplaceGeneralizedComparisonTerm(
            comparison, 1, "x", "time", {"x", "planet", "time"}));
        assert(comparison.models[0].terms == std::vector<std::string>({"x"}));
        assert(comparison.models[1].terms ==
               std::vector<std::string>({"time", "planet", "time:planet"}));
        assert(comparison.models[1].centeredPredictors.empty());
        assert(comparison.activeModel == 1);

        // A copied model receives value-semantic Count settings; later edits
        // to the copy do not mutate its source column.
        const int copiedIndex = rlispstat::core::AddGeneralizedComparisonModel(
            comparison, 1, true);
        assert(copiedIndex == 2);
        assert(comparison.models[2].countDistribution ==
               comparison.models[1].countDistribution);
        assert(comparison.models[2].exposure == comparison.models[1].exposure);
        assert(comparison.models[2].scope == comparison.models[1].scope);
        comparison.models[2].exposure = "another_time";
        assert(comparison.models[1].exposure == "time");
        assert(rlispstat::core::CountRegressionComparisonFootnote().find(
            "analysis rows") != std::string::npos);
    }

    // Exact MI Count lifecycle from the reported regression:
    // y_count ~ x + z + factor(g) -> remove z -> y_count ~ x + factor(g).
    // A specification edit must quarantine the old df=393/deviance fit, keep
    // the five-imputation dataset identity, and accept only a result keyed to
    // the reduced revision (df=396, deviance about 1484.757).
    {
        rlispstat::core::GeneralizedComparisonState comparison;
        comparison.id = "count-mi-lifecycle";
        comparison.group = "mi_test_mids";
        comparison.response = "y_count";
        comparison.family = "poisson";
        comparison.link = "log";
        comparison.countComparison = true;
        comparison.multipleImputation = true;
        comparison.imputationCount = 5;
        comparison.datasetType = "multiple_imputation";
        comparison.imputationSetId = "mi_count_lifecycle_trace";
        comparison.sourceDatasetId = "mi_test_source";
        comparison.termTypes = {{"x", "numeric"}, {"z", "numeric"}, {"g", "factor"}};

        rlispstat::core::GeneralizedComparisonModel model;
        model.id = "count-mi-lifecycle:model:1";
        model.label = "Model 1";
        model.response = "y_count";
        model.family = "poisson";
        model.link = "log";
        model.countDistribution = rlispstat::core::CountDistribution::Poisson;
        model.terms = {"x", "z", "g"};
        model.termTypes = comparison.termTypes;
        model.termTypeOverrides = {{"g", "factor"}};
        model.factorReferenceLevels = {{"g", "A"}};
        model.modelVersion = 7;
        model.fitVersion = 7;
        model.fitSpecificationRevision = 7;
        model.fit.ok = true;
        model.fit.n = 400;
        model.fit.dfResidual = 393;
        model.fit.parameterCount = 7;
        model.fit.residualDeviance = 1386.737;
        model.fit.multipleImputation = true;
        model.fit.imputationCount = 5;
        model.fit.datasetType = comparison.datasetType;
        model.fit.imputationSetId = comparison.imputationSetId;
        model.fit.sourceDatasetId = comparison.sourceDatasetId;
        model.isStale = false;
        model.fitState = RegressionComparisonFitState::Valid;
        comparison.models.push_back(model);
        auto &current = comparison.models.front();
        const std::string oldFingerprint =
            rlispstat::core::GeneralizedComparisonModelSpecificationFingerprint(
                comparison, current);
        current.requestedSpecificationRevision = current.modelVersion;
        current.requestedSpecificationFingerprint = oldFingerprint;
        current.fitSpecificationFingerprint = oldFingerprint;
        current.fit.lastRFitSignature = oldFingerprint;

        assert(rlispstat::core::ExcludeGeneralizedComparisonTerm(
            comparison, 0, "z"));
        assert(current.terms == std::vector<std::string>({"x", "g"}));
        assert(current.modelVersion == 8);
        assert(current.fitVersion == 0);
        assert(!current.fit.ok);
        assert(current.fit.rows.empty());
        assert(!comparison.rFitPending);
        assert(comparison.multipleImputation && comparison.imputationCount == 5);
        assert(comparison.imputationSetId == "mi_count_lifecycle_trace");
        const std::string reducedFingerprint =
            rlispstat::core::GeneralizedComparisonModelSpecificationFingerprint(
                comparison, current);
        assert(reducedFingerprint != oldFingerprint);

        auto row = [](const std::string &term, const std::string &source,
                      const std::string &kind, double estimate = NAN,
                      double standardError = NAN) {
            rlispstat::core::GeneralizedGLMRow result;
            result.term = term;
            result.sourceTerm = source;
            result.rowType = kind;
            result.termType = source == "g" ? "factor" : "numeric";
            result.estimate = estimate;
            result.stdError = standardError;
            return result;
        };
        current.requestedSpecificationRevision = current.modelVersion;
        current.requestedSpecificationFingerprint = reducedFingerprint;
        current.fitSpecificationRevision = current.modelVersion;
        current.fitSpecificationFingerprint = reducedFingerprint;
        current.fitVersion = current.modelVersion;
        current.isStale = false;
        current.fitState = RegressionComparisonFitState::Valid;
        static_cast<rlispstat::core::ModelSpecification &>(current.fit) =
            rlispstat::core::EffectiveGeneralizedComparisonModelSpecification(
                comparison, current);
        current.fit.group = comparison.group;
        current.fit.family = "poisson";
        current.fit.link = "log";
        current.fit.countRegression = true;
        current.fit.ok = true;
        current.fit.n = 400;
        current.fit.dfResidual = 396;
        current.fit.parameterCount = 4;
        current.fit.residualDeviance = 1484.757;
        current.fit.multipleImputation = true;
        current.fit.imputationCount = 5;
        current.fit.datasetType = comparison.datasetType;
        current.fit.imputationSetId = comparison.imputationSetId;
        current.fit.sourceDatasetId = comparison.sourceDatasetId;
        current.fit.modelVersion = current.modelVersion;
        current.fit.fitVersion = current.modelVersion;
        current.fit.lastRFitSignature = reducedFingerprint;
        current.fit.rows = {
            row("(Intercept)", "(Intercept)", "coefficient", 0.72569611, 0.06246865),
            row("x", "x", "coefficient", 0.43210388, 0.04844945),
            row("g", "g", "factor_parent"),
            row("g=A", "g", "reference"),
            row("g=B", "g", "factor_level", 0.52526892, 0.07666258),
            row("g=C", "g", "factor_level", 0.09345032, 0.07950652)
        };
        for (std::size_t index = 3; index < current.fit.rows.size(); ++index) {
            current.fit.rows[index].factorLevel = index == 3 ? "A" : (index == 4 ? "B" : "C");
            current.fit.rows[index].referenceLevel = "A";
        }
        comparison.rFitPending = false;
        rlispstat::core::RefreshGeneralizedTermRowsFromFits(comparison);
        assert(rlispstat::core::GeneralizedComparisonResolvedFitState(
                   comparison, current) == RegressionComparisonFitState::Valid);
        assert(current.fit.dfResidual == 396);
        assert(closeEnough(current.fit.residualDeviance, 1484.757, 1.0e-6));
        assert(std::find(comparison.termRows.begin(), comparison.termRows.end(), "g=A") !=
               comparison.termRows.end());
        assert(std::find(comparison.termRows.begin(), comparison.termRows.end(), "g=B") !=
               comparison.termRows.end());
        assert(std::find(comparison.termRows.begin(), comparison.termRows.end(), "g=C") !=
               comparison.termRows.end());
        std::string reason;
        assert(rlispstat::core::GeneralizedComparisonPresentationIsConsistent(
            comparison, &reason));

        const std::vector<std::string> variables = {"y_count", "x", "z", "g"};
        assert(rlispstat::core::IncludeGeneralizedComparisonTerm(
            comparison, 0, "z", variables));
        assert(current.fitVersion == 0 && current.fit.rows.empty());
        assert(rlispstat::core::ExcludeGeneralizedComparisonTerm(
            comparison, 0, "x"));
        assert(rlispstat::core::IncludeGeneralizedComparisonTerm(
            comparison, 0, "x", variables));
        assert(rlispstat::core::ApplyGeneralizedComparisonReferenceLevel(
            comparison, 0, "g", "B", {"A", "B", "C"}));
        assert(rlispstat::core::ExcludeGeneralizedComparisonTerm(
            comparison, 0, "z"));
        assert(comparison.imputationCount == 5);
        assert(comparison.imputationSetId == "mi_count_lifecycle_trace");
        assert(current.factorReferenceLevels.at("g") == "B");
        assert(current.terms == std::vector<std::string>({"g", "x"}));
        assert(current.fitVersion == 0 && current.fit.rows.empty());
    }

    GLMFitSummary reducedLinearFit;
    reducedLinearFit.ok = true;
    reducedLinearFit.rowsUsed = {1, 2, 3, 4};
    reducedLinearFit.dfResidual = 18;
    reducedLinearFit.ssResidual = 100.0;
    NestedLinearModelSummary reducedLinear = BuildNestedLinearModelSummary(reducedLinearFit, "y", {"x"});
    assert(reducedLinear.ok);
    assert(reducedLinear.response == "y");
    assert(reducedLinear.terms == std::vector<std::string>({"x"}));
    assert(reducedLinear.rowsUsed == std::vector<int>({1, 2, 3, 4}));
    assert(reducedLinear.dfResidual == 18);
    assert(closeEnough(reducedLinear.ssResidual, 100.0));
    NestedLinearModelSummary fullLinear = reducedLinear;
    fullLinear.terms = {"x", "z", "x:z"};
    fullLinear.dfResidual = 16;
    fullLinear.ssResidual = 80.0;
    NestedModelTestResult linearTest = LinearNestedModelTest(reducedLinear, fullLinear);
    assert(linearTest.ok);
    assert(linearTest.df == 2);
    assert(closeEnough(linearTest.df2, 16.0));
    assert(closeEnough(linearTest.delta, 20.0));
    assert(closeEnough(linearTest.statistic, 2.0));
    assert(linearTest.statisticName == "F");
    assert(std::isfinite(linearTest.p));
    std::string linearStatus = RegressionComparisonModelTestStatus("Full", "Reduced", linearTest, false);
    assert(linearStatus.find("Status: Full vs Reduced") == 0);
    assert(linearStatus.find("\u0394SSE = 20.000") != std::string::npos);
    assert(linearStatus.find("partial F = 2.000") != std::string::npos);
    std::string miStatus = RegressionComparisonModelTestStatus("Full", "Reduced", linearTest, true);
    assert(miStatus.find("D1/Wald statistic = 2.000") != std::string::npos);
    assert(RegressionComparisonNoPreviousModelStatus().find("no previous model") != std::string::npos);
    assert(RegressionComparisonUnavailableStatus().find("nested models") != std::string::npos);
    assert(RegressionComparisonCopiedStatus().find("model comparison table copied") != std::string::npos);
    assert(GeneralizedComparisonCopiedStatus().find("generalized model comparison table copied") != std::string::npos);
    assert(SelectedRowsStatus(3, 12) == "Selected rows: 3 / 12");
    assert(GLMPooledTableChangeInRStatus() ==
           "Status: this is a pooled MI table; change the model in R and reopen the table.");
    assert(GLMValueCopiedStatus() == "Status: value copied.");
    assert(GLMFitStatisticCopiedStatus() == "Status: fit statistic copied.");
    assert(GLMRegressionTableCopiedStatus() == "Status: regression table copied.");
    assert(GLMCompareLinearModelsHintStatus().find("Compare Linear Models") != std::string::npos);
    assert(GLMSelectPredictorToRemoveStatus() == "Status: select a predictor to remove.");
    assert(GLMNoAvailablePredictorsStatus() == "Status: no available predictors.");
    assert(GLMNoAvailableReplacementStatus() == "Status: no available replacement.");
    assert(GLMTermActionHintStatus().find("contextual actions") != std::string::npos);
    assert(GLMFittedStatus(32, 2, "all") == "Status: fitted on 32 rows, 2 excluded, scope = all");
    assert(GLMPrecomputedTableRefitStatus("add predictors") ==
           "Status: precomputed tables must be refit from R to add predictors.");
    assert(GLMPooledMIScopeRefitStatus() ==
           "Status: pooled MI tables must be refit from R to change scope.");
    assert(GLMImputedDatasetUnavailableStatus() ==
           "Status: the imputed dataset is no longer available.");
    assert(GLMRefittingPooledMIResponseStatus("y") ==
           "Status: refitting pooled MI model via R/mice with Rubin's rules; response y...");
    assert(GLMRefittingPooledMITermStatus("adding", "x") ==
           "Status: refitting pooled MI model via R/mice with Rubin's rules after adding x...");
    assert(GLMInteractionComponentTypeStatus("x:z") ==
           "Status: x:z is an interaction; change the component variable types instead.");
    assert(GLMTermTypeChangedStatus("x", "factor") == "Status: x is Categorical.");
    assert(GLMTermTypeChangedStatus("x", "numeric") == "Status: x is Numeric.");
    assert(GLMModelTableCopiedStatus() == "Model table copied.");
    assert(GLMModelTableExportedStatus() == "Model table exported as PDF.");
    assert(GLMPDFWriteFailedStatus() == "Could not write the selected PDF file.");
    {
        GLMFitSummary fit;
        fit.ok = true;
        fit.n = 32;
        fit.r2 = 0.753;
        fit.adjR2 = 0.745;
        fit.sigma = 3.046;
        fit.dfResidual = 30;
        fit.globalF = 91.375;
        fit.globalP = 0.0002;
        fit.aic = 123.4;
        fit.bic = 130.5;
        GLMCoefficientRow coef;
        coef.term = "wt";
        coef.estimate = -5.344;
        coef.standardizedBeta = -0.868;
        coef.stdError = 0.559;
        coef.tValue = -9.559;
        coef.pValue = 0.0001;
        coef.partialR = -0.868;
        coef.deltaR2 = 0.753;
        fit.coefficients.push_back(coef);
        std::string table = GLMRegressionCopyTableText(fit, "mpg", true);
        assert(table.find("Response:\tmpg") != std::string::npos);
        assert(table.find("Variable\tb\tβ\tSE\tt\tp\tPartial r\tΔR²") != std::string::npos);
        assert(table.find("wt\t-5.3440\t-0.868\t0.5590\t-9.559\t< .001\t-0.868\t0.753") != std::string::npos);
        assert(table.find("D1/Wald\t91.375") != std::string::npos);
        assert(table.find("AIC") == std::string::npos);
        assert(GLMModelDetailsText(fit, true).find("AIC: 123.4") != std::string::npos);
    }
    {
        const auto assertDisplayRow = [](
            const GLMCoefficientRow &row,
            const std::vector<GLMTableDisplayState> &expectedStates,
            const std::vector<std::string> &expectedText) {
            const auto cells = GLMRegressionTableRow(row);
            assert(cells.size() == 9);
            assert(cells.size() == expectedStates.size());
            assert(cells.size() == expectedText.size());
            for (std::size_t column = 0; column < cells.size(); ++column) {
                assert(cells[column].state == expectedStates[column]);
                assert(GLMRegressionTableCellText(cells[column]) == expectedText[column]);
            }
        };
        const auto V = GLMTableDisplayState::Value;
        const auto B = GLMTableDisplayState::Blank;
        const auto N = GLMTableDisplayState::NotApplicable;
        const auto U = GLMTableDisplayState::Unavailable;
        const std::string dash = "\u2014";

        GLMCoefficientRow intercept;
        intercept.term = "(Intercept)";
        intercept.sourceTerm = "(Intercept)";
        intercept.termType = "intercept";
        intercept.displayLabel = "(Intercept)";
        intercept.estimate = 10.0;
        intercept.stdError = 0.5;
        intercept.tValue = 20.0;
        intercept.pValue = 0.01;
        assertDisplayRow(intercept,
            {V,V,V,N,V,V,V,N,N},
            {"(Intercept)","-","10.0000",dash,"0.5000","20.000",".010",dash,dash});

        GLMCoefficientRow numeric;
        numeric.term = "x";
        numeric.sourceTerm = "x";
        numeric.termType = "numeric";
        numeric.displayLabel = "x";
        numeric.estimate = 1.25;
        numeric.standardizedBeta = 0.4;
        numeric.stdError = 0.2;
        numeric.tValue = 6.25;
        numeric.pValue = 0.02;
        numeric.partialR = 0.35;
        numeric.deltaR2 = 0.12;
        assertDisplayRow(numeric,
            {V,V,V,V,V,V,V,V,V},
            {"x","Numeric","1.2500","0.400","0.2000","6.250",".020","0.350","0.120"});

        GLMCoefficientRow factorParent;
        factorParent.term = "group";
        factorParent.sourceTerm = "group";
        factorParent.termType = "factor";
        factorParent.rowType = "factor_parent";
        factorParent.displayLabel = "group";
        factorParent.deltaR2 = 0.25;
        assertDisplayRow(factorParent,
            {V,V,B,B,B,B,U,N,V},
            {"group","Categorical","","","","",dash,dash,"0.250"});

        GLMCoefficientRow reference;
        reference.term = "groupA";
        reference.sourceTerm = "group";
        reference.termType = "factor";
        reference.rowType = "reference";
        reference.displayLabel = "  A";
        assertDisplayRow(reference,
            {V,B,N,N,N,N,N,N,N},
            {"  A","",dash,dash,dash,dash,dash,dash,dash});

        GLMCoefficientRow factorLevel;
        factorLevel.term = "groupB";
        factorLevel.sourceTerm = "group";
        factorLevel.termType = "factor";
        factorLevel.rowType = "factor_level";
        factorLevel.displayLabel = "  B";
        factorLevel.estimate = 2.0;
        factorLevel.stdError = 0.4;
        factorLevel.tValue = 5.0;
        factorLevel.pValue = 0.03;
        factorLevel.partialR = 0.3;
        assertDisplayRow(factorLevel,
            {V,B,V,N,V,V,V,V,N},
            {"  B","","2.0000",dash,"0.4000","5.000",".030","0.300",dash});
        factorLevel.standardizedBeta = 0.2;
        factorLevel.deltaR2 = 0.05;
        assert(GLMRegressionTableCellText(GLMRegressionTableRow(factorLevel)[3]) == "0.200");
        assert(GLMRegressionTableCellText(GLMRegressionTableRow(factorLevel)[8]) == "0.050");

        GLMCoefficientRow interactionParent;
        interactionParent.term = "x:group";
        interactionParent.sourceTerm = "x:group";
        interactionParent.termType = "numeric_factor_interaction";
        interactionParent.rowType = "term_parent";
        interactionParent.displayLabel = "x:group";
        interactionParent.deltaR2 = 0.08;
        assertDisplayRow(interactionParent,
            {V,V,B,B,B,B,U,N,V},
            {"x:group","Numeric x Categorical","","","","",dash,dash,"0.080"});

        GLMCoefficientRow interactionCoefficient;
        interactionCoefficient.term = "x:groupB";
        interactionCoefficient.sourceTerm = "x:group";
        interactionCoefficient.termType = "numeric_factor_interaction";
        interactionCoefficient.rowType = "coefficient";
        interactionCoefficient.displayLabel = "  x by B";
        interactionCoefficient.estimate = -0.5;
        interactionCoefficient.standardizedBeta = -0.1;
        interactionCoefficient.stdError = 0.25;
        interactionCoefficient.tValue = -2.0;
        interactionCoefficient.pValue = 0.04;
        interactionCoefficient.partialR = -0.15;
        assertDisplayRow(interactionCoefficient,
            {V,B,V,V,V,V,V,V,N},
            {"  x by B","","-0.5000","-0.100","0.2500","-2.000",".040","-0.150",dash});

        numeric.stdError = std::numeric_limits<double>::quiet_NaN();
        const auto unavailable = GLMRegressionTableRow(numeric)[4];
        assert(GLMTableDisplayStateId(unavailable.state) == "unavailable");
        assert(GLMRegressionTableCellText(unavailable) == dash);
        assert(GLMTableDisplayStateId(B) == "blank");
        assert(GLMTableDisplayStateId(N) == "notApplicable");
        assert(GLMTableDisplayStateId(V) == "value");
    }
    assert(GLMStatusMessage("ready") == "Status: ready");
    assert(GLMFitNoteWarningStatus("pooled", "singular fit") == "Status: pooled Warning: singular fit");
    assert(GLMInteractionReportOpenedStatus("x:z") == "Status: interaction report opened for x:z.");
    assert(GLMInteractionPlotOpenedStatus("x:z") == "Status: effect plot opened for x:z.");
    assert(rlispstat::core::ComparisonInteractionNotIncludedMessage("group:country", "Model 2") ==
           "Interaction \u201Cgroup \u00D7 country\u201D is not included in the active model \u201CModel 2\u201D.");
    assert(GeneralizedGLMRscriptLaunchFailedStatus() == "Could not run Rscript for stats::glm().");
    GeneralizedGLMRow detailRow;
    detailRow.term = "x";
    detailRow.displayLabel = "x";
    detailRow.estimate = 1.23456;
    detailRow.stdError = 0.25;
    detailRow.statistic = 4.938;
    detailRow.pValue = 0.0004;
    detailRow.ciLower = 0.7446;
    detailRow.ciUpper = 1.7246;
    detailRow.oddsRatio = 3.4367;
    detailRow.oddsRatioLower = 2.1054;
    detailRow.oddsRatioUpper = 5.6102;
    assert(GeneralizedCoefficientDetailsStatus(detailRow, "z").find("x; b = 1.2346") != std::string::npos);
    GeneralizedGLMRow parentDetail;
    parentDetail.term = "group";
    parentDetail.rowType = "factor_parent";
    assert(GeneralizedCoefficientDetailsStatus(parentDetail, "z") ==
           "Status: group is a categorical term; omnibus p \u2014; use its category rows for individual coefficients.");
    GeneralizedGLMRow referenceDetail;
    referenceDetail.sourceTerm = "group";
    referenceDetail.factorLevel = "A";
    referenceDetail.rowType = "reference";
    assert(GeneralizedCoefficientDetailsStatus(referenceDetail, "z") ==
           "Status: group: A is the reference category; b = \u2014; SE = \u2014; z = \u2014; p = \u2014.");
    assert(ComparisonTermNotIncludedStatus() == "Status: term is not included in this model.");
    assert(GeneralizedComparisonCoefficientDetailsStatus(detailRow).find("Status: b = 1.2346") == 0);
    assert(GeneralizedComparisonCoefficientDetailsStatus(detailRow).find("95% CI [0.7446, 1.7246]") != std::string::npos);
    assert(GeneralizedComparisonCoefficientDetailsStatus(detailRow, true).find("OR = 3.4367") != std::string::npos);
    assert(GeneralizedComparisonCoefficientDetailsStatus(referenceDetail) ==
           "Status: group: A is the reference category; b = \u2014; SE = \u2014; p = \u2014.");
    GLMCoefficientRow linearLevelA;
    linearLevelA.term = "groupB";
    linearLevelA.estimate = 0.5;
    linearLevelA.pValue = 0.03;
    GLMCoefficientRow linearLevelB;
    linearLevelB.term = "groupC";
    linearLevelB.estimate = -0.25;
    linearLevelB.pValue = 0.2;
    assert(RegressionCoefficientGroupDetailsStatus({linearLevelA, linearLevelB}) ==
           "Status: groupB b = 0.5000, p .030; groupC b = -0.2500, p .200");
    GLMCoefficientRow linearParent;
    linearParent.sourceTerm = "group";
    linearParent.rowType = "factor_parent";
    assert(RegressionCoefficientStructuralDetailsStatus(linearParent) ==
           "Status: group is a categorical term; omnibus p \u2014; use its category rows for individual coefficients.");
    GLMCoefficientRow linearReference;
    linearReference.sourceTerm = "group";
    linearReference.factorLevel = "A";
    linearReference.rowType = "reference";
    assert(RegressionCoefficientStructuralDetailsStatus(linearReference) ==
           "Status: group: A is the reference category; b = \u2014; SE = \u2014; t = \u2014; p = \u2014.");
    assert(RegressionComparisonFitValueStatus("Model 2", "R\u00B2", "74.0%") ==
           "Status: Model 2 R\u00B2 = 74.0%");
    assert(RegressionComparisonModelRenamedStatus() == "Status: model renamed.");
    assert(RegressionComparisonOpenedSingleModelStatus("Full") ==
           "Status: opened Full as a single General Linear Model window.");
    assert(GeneralizedGLMWindowStatus("fitted", 4, 10, "poisson", "log") ==
           "Status: fitted  Selected rows: 4 / 10  Distribution = poisson, link = log");
    {
        GeneralizedGLMState state;
        state.response = "am";
        state.family = "binomial";
        state.link = "logit";
        state.diagnosticOptions.residualType = "deviance";
        state.n = 32;
        state.nullDeviance = 43.2;
        state.residualDeviance = 21.4;
        state.dfResidual = 29;
        state.aic = 31.5;
        state.bic = 35.0;
        state.dispersion = 1.0;
        state.logLik = -12.75;
        state.statisticName = "z";
        GeneralizedGLMRow row;
        row.term = "wt";
        row.termType = "numeric";
        row.estimate = -1.2345;
        row.stdError = 0.4567;
        row.statistic = -2.703;
        row.pValue = 0.0069;
        state.rows.push_back(row);
        std::string text = GeneralizedGLMFormattedOutputText(state);
        assert(text.find("Response  am     Distribution  Binomial     Link  logit") != std::string::npos);
        assert(text.find("Null deviance  43.200") != std::string::npos);
        assert(text.find("wt") != std::string::npos);
        assert(text.find(".007") != std::string::npos);
        state.centeredPredictors.insert("wt");
        text = GeneralizedGLMFormattedOutputText(state);
        assert(text.find("wt (centered)") != std::string::npos);
        assert(text.find("Numeric") != std::string::npos);
        assert(text.find("numeric") == std::string::npos);
        GeneralizedGLMState binary = state;
        binary.binaryRegression = true;
        binary.binaryLink = BinaryLink::Logit;
        binary.responseCoding.eventLabel = "1";
        binary.responseCoding.referenceLabel = "0";
        binary.rows[0].sourceTerm = "wt";
        binary.rows[0].oddsRatio = 0.291;
        binary.rows[0].oddsRatioLower = 0.119;
        binary.rows[0].oddsRatioUpper = 0.713;
        BinaryTermTestRow wtTest;
        wtTest.term = "wt";
        wtTest.method = "LR chi-square";
        wtTest.df = 1;
        wtTest.statistic = 8.25;
        wtTest.pValue = 0.0041;
        binary.termTests.push_back(wtTest);
        std::string binaryText = GeneralizedGLMFormattedOutputText(binary);
        assert(binaryText.find("Terms and coefficients") != std::string::npos);
        assert(binaryText.find("Variable / category") != std::string::npos);
        assert(binaryText.find("Type") == std::string::npos);
        assert(binaryText.find("Wald p") == std::string::npos);
        assert(binaryText.find("Term test") == std::string::npos);
        assert(binaryText.find("LR χ²") == std::string::npos);
        assert(binaryText.find("df2") == std::string::npos);
        assert(binaryText.find("\nTerms\n") == std::string::npos);
        assert(binaryText.find("8.25") == std::string::npos);
        assert(binaryText.find("AIC") == std::string::npos);
        assert(binaryText.find("Apparent AUC") == std::string::npos);
        assert(binaryText.find("Nagelkerke pseudo-R") != std::string::npos);
        assert(GeneralizedGLMModelDetailsText(binary).find("AIC:") != std::string::npos);
        GeneralizedGLMRow factorParent;
        factorParent.term = "cyl";
        factorParent.sourceTerm = "cyl";
        factorParent.displayLabel = "cyl";
        factorParent.rowType = "factor_parent";
        factorParent.termType = "factor";
        GeneralizedGLMRow factorReference;
        factorReference.term = "cyl4";
        factorReference.sourceTerm = "cyl";
        factorReference.displayLabel = "4";
        factorReference.rowType = "reference";
        factorReference.termType = "factor";
        factorReference.factorLevel = "4";
        factorReference.referenceLevel = "4";
        GeneralizedGLMRow factorLevel;
        factorLevel.term = "cyl6";
        factorLevel.sourceTerm = "cyl";
        factorLevel.displayLabel = "6";
        factorLevel.rowType = "factor_level";
        factorLevel.termType = "factor";
        factorLevel.factorLevel = "6";
        factorLevel.referenceLevel = "4";
        factorLevel.estimate = 1.446;
        factorLevel.stdError = 1.142;
        factorLevel.statistic = 1.266;
        factorLevel.pValue = 0.205;
        factorLevel.oddsRatio = 4.246;
        factorLevel.oddsRatioLower = 0.453;
        factorLevel.oddsRatioUpper = 39.800;
        binary.rows.push_back(factorParent);
        binary.rows.push_back(factorReference);
        binary.rows.push_back(factorLevel);
        BinaryTermTestRow cylTest;
        cylTest.term = "cyl";
        cylTest.method = "LR chi-square";
        cylTest.df = 2;
        cylTest.statistic = 12.47;
        cylTest.pValue = 0.002;
        binary.termTests.push_back(cylTest);
        assert(GeneralizedGlobalTermTestForPresentationRow(binary, factorParent) != nullptr);
        assert(GeneralizedGlobalTermTestForPresentationRow(binary, factorLevel) == nullptr);

        GeneralizedGLMState hurdle;
        hurdle.countRegression = true;
        hurdle.countDistribution = CountDistribution::HurdleBetaBinomialCeiling;
        GeneralizedGLMRow perfectParent = factorParent;
        perfectParent.component = "perfect_score";
        GeneralizedGLMRow belowParent = factorParent;
        belowParent.component = "below_ceiling";
        BinaryTermTestRow perfectTest = cylTest;
        perfectTest.component = "perfect_score";
        perfectTest.statistic = 3.1;
        BinaryTermTestRow belowTest = cylTest;
        belowTest.component = "below_ceiling";
        belowTest.statistic = 9.4;
        hurdle.termTests = {perfectTest, belowTest};
        assert(GeneralizedGlobalTermTestForPresentationRow(hurdle, perfectParent)->statistic == 3.1);
        assert(GeneralizedGlobalTermTestForPresentationRow(hurdle, belowParent)->statistic == 9.4);
        hurdle.ok = true;
        hurdle.rows = {perfectParent, belowParent};
        hurdle.diagnostics.resize(2);
        hurdle.betaBinomialDispersion = 0.25;
        ClearGeneralizedGLMFitResults(hurdle);
        assert(!hurdle.ok && hurdle.rows.empty() && hurdle.termTests.empty() &&
               hurdle.diagnostics.empty() && !std::isfinite(hurdle.betaBinomialDispersion));
        assert(GeneralizedGlobalTermTestMethodNote(binary) ==
               "Global tests shown on parent term rows use LR χ².");
        binaryText = GeneralizedGLMFormattedOutputText(binary);
        assert(binaryText.find("4 (reference)") != std::string::npos);
        const std::size_t factorParentStart = binaryText.find("\ncyl");
        assert(factorParentStart != std::string::npos);
        const std::size_t factorParentEnd = binaryText.find('\n', factorParentStart + 1);
        const std::string factorParentLine = binaryText.substr(
            factorParentStart + 1, factorParentEnd - factorParentStart - 1);
        assert(factorParentLine.find("LR χ²") == std::string::npos);
        assert(factorParentLine.find("12.47") != std::string::npos);
        assert(factorParentLine.find(".002") != std::string::npos);
        const std::size_t referenceStart = binaryText.find("\n4 (reference)");
        assert(referenceStart != std::string::npos);
        const std::size_t referenceEnd = binaryText.find('\n', referenceStart + 1);
        const std::string referenceLine = binaryText.substr(
            referenceStart + 1, referenceEnd - referenceStart - 1);
        assert(referenceLine.find("—") != std::string::npos);
        assert(binaryText.find("12.47") != std::string::npos);
        assert(binaryText.find("1.446") != std::string::npos);
        assert(binaryText.find("12.47") == binaryText.rfind("12.47"));
        GeneralizedGLMState hierarchical = binary;
        BinaryTermTestRow unavailableMain;
        unavailableMain.term = "cyl";
        unavailableMain.method = "LR chi-square";
        BinaryTermTestRow interactionTest;
        interactionTest.term = "cyl:wt";
        interactionTest.method = "LR chi-square";
        interactionTest.statistic = 5.2;
        interactionTest.df = 1;
        interactionTest.pValue = 0.023;
        hierarchical.termTests = {unavailableMain, interactionTest};
        assert(GeneralizedGlobalTermTestStatisticHeader(hierarchical) == "LR χ²");
        assert(!GeneralizedModelUsesMixedGlobalTermTestMethods(hierarchical));
        assert(GeneralizedGlobalTermTestHierarchyNote(hierarchical).find(
            "Lower-order terms contained in higher-order interactions") != std::string::npos);
        const std::string hierarchicalText = GeneralizedGLMFormattedOutputText(hierarchical);
        assert(hierarchicalText.find("simple effects or the effect plot") != std::string::npos);
        const std::string binaryDetailedText = GeneralizedGLMFormattedOutputText(binary, true);
        assert(binaryDetailedText.find("Model details") != std::string::npos);
        assert(binaryDetailedText.find("not out-of-sample validation") != std::string::npos);
        const auto apaLogit = BuildBinaryAPAReportModel(binary);
        assert(apaLogit.logit);
        assert(apaLogit.defaultTitle == "Binary Logistic Regression Predicting am");
        assert(apaLogit.coefficients.size() == 4);
        assert(apaLogit.coefficients[1].parent);
        assert(apaLogit.coefficients[2].reference);
        assert(apaLogit.coefficients[3].predictor == "  6 (vs 4)");
        assert(apaLogit.termTests.size() == 2);
        const std::string logitCSV = BinaryRegressionCoefficientCSV(binary);
        assert(logitCSV.find("Predictor,Type,Estimate,SE,z,p,Odds_Ratio,OR_CI_Lower,OR_CI_Upper,Reference") == 0);
        assert(logitCSV.find("\"cyl\",") != std::string::npos);
        assert(logitCSV.find(",,,12.47") != std::string::npos);
        assert(logitCSV.find("\"cyl: 4 (reference)\",\"\",,,,,,,,true") != std::string::npos);
        assert(logitCSV.find("\"cyl: 6 vs. 4\",\"\",1.446") != std::string::npos);
        assert(logitCSV.find("\"wt\",\"Numeric\",") != std::string::npos);
        const std::string termCSV = BinaryRegressionTermTestsCSV(binary);
        assert(termCSV.find("Term,Method,Statistic,df,df2,p") == 0);
        assert(termCSV.find("\"cyl\",") != std::string::npos);
        assert(termCSV.find(",2,,0.002") != std::string::npos);
        const std::string fitCSV = BinaryRegressionModelFitCSV(binary);
        assert(fitCSV.find("Statistic,Value") == 0);
        assert(fitCSV.find("\"AIC\",") != std::string::npos);
        assert(fitCSV.find("\"Apparent AUC\",") != std::string::npos);
        binary.binaryLink = BinaryLink::Probit;
        const auto apaProbit = BuildBinaryAPAReportModel(binary);
        assert(!apaProbit.logit);
        assert(apaProbit.defaultTitle == "Binary Probit Regression Predicting am");
        const std::string probitCSV = BinaryRegressionCoefficientCSV(binary);
        assert(probitCSV.find("Predictor,Type,Estimate,SE,z,p,CI_Lower,CI_Upper,Reference") == 0);
        assert(probitCSV.find("Odds_Ratio") == std::string::npos);
        state.rows.clear();
        assert(GeneralizedGLMFormattedOutputText(state).find("Add at least one predictor") != std::string::npos);
    }
    assert(RegressionComparisonSummaryStatus(2, 4, false, "") == "Status: 2 models, 4 terms");
    assert(RegressionComparisonSummaryStatus(2, 4, true, "pooled with Rubin's rules") ==
           "Status: pooled with Rubin's rules");
    assert(GeneralizedComparisonSummaryStatus(2, 4) == "Status: 2 generalized models, 3 terms");
    assert(GeneralizedComparisonSummaryStatus(1, 0) == "Status: 1 generalized models, 0 terms");
    assert(ComparisonNoAvailableTermsStatus() == "Status: no available terms.");
    assert(PrecomputedComparisonChangeInRStatus("comparison") ==
           "Status: this is a precomputed table; change the comparison in R and reopen the table.");
    assert(PooledMIComparisonChangeInRStatus() ==
           "Status: this is a pooled MI table; change the comparison in R and reopen the table.");
    assert(PooledMITermTypesChangeInRStatus() ==
           "Status: this is a pooled MI table; change term types in R and reopen the table.");
    assert(PooledMIAutoRefitDisabledStatus() ==
           "Status: this is a pooled MI table; auto-refit is disabled.");
    assert(PooledOrPrecomputedRefitLayerStatus() ==
           "Status: pooled/precomputed comparisons are refit from their analysis layer.");
    assert(PooledMIComparisonRefitStatus() == "Status: refitting pooled MI model comparison...");
    assert(PooledMIComparisonRefitStatus("", true) ==
           "Status: refitting pooled MI model comparison via R/mice...");
    assert(PooledMIComparisonRefitStatus("after adding x") ==
           "Status: refitting pooled MI comparison after adding x...");
    assert(RegressionComparisonRefittedStatus() == "Status: model refitted.");
    assert(RowsUsedExcludedText({1, 3}, {2}) == "Rows used (2): 1, 3\n\nRows excluded (1): 2");
    assert(RowsUsedExcludedText({1}, {}, "Model A") == "Model A\n\nRows used (1): 1\n\nRows excluded (0): ");
    GLMFitSummary displayFit;
    displayFit.ok = true;
    displayFit.n = 24;
    displayFit.r2 = 0.456;
    displayFit.adjR2 = 0.4;
    displayFit.sigma = 1.23456;
    displayFit.dfResidual = 16;
    displayFit.globalF = 5.5;
    displayFit.globalP = 0.002;
    displayFit.aic = 100.21;
    displayFit.bic = 110.78;
    assert(RegressionComparisonFitDisplay(displayFit, linearTest, 0) == "24");
    assert(RegressionComparisonFitDisplay(displayFit, linearTest, 1) == "45.6%");
    assert(RegressionComparisonFitDisplay(displayFit, linearTest, 9) == "2");
    assert(RegressionComparisonFitDisplay(GLMFitSummary(), linearTest, 0) == "\u2014");

    NestedLinearModelSummary rowMismatch = fullLinear;
    rowMismatch.rowsUsed = {1, 2, 5, 6};
    assert(!LinearNestedModelTest(reducedLinear, rowMismatch).ok);
    NestedLinearModelSummary termMismatch = fullLinear;
    termMismatch.terms = {"w"};
    assert(!LinearNestedModelTest(reducedLinear, termMismatch).ok);
    NestedLinearModelSummary responseMismatch = fullLinear;
    responseMismatch.response = "other";
    assert(!LinearNestedModelTest(reducedLinear, responseMismatch).ok);
    NestedLinearModelSummary impossibleLinear = fullLinear;
    impossibleLinear.ssResidual = 120.0;
    assert(!LinearNestedModelTest(reducedLinear, impossibleLinear).ok);

    NestedGeneralizedModelSummary reducedGlm = BuildNestedGeneralizedModelSummary(
        true, "y", "poisson", "log", {"x"}, {1, 2, 3, 4}, 18, 120.0, std::numeric_limits<double>::quiet_NaN());
    assert(reducedGlm.ok);
    assert(reducedGlm.response == "y");
    assert(reducedGlm.family == "poisson");
    assert(reducedGlm.link == "log");
    assert(reducedGlm.terms == std::vector<std::string>({"x"}));
    assert(reducedGlm.rowsUsed == std::vector<int>({1, 2, 3, 4}));
    assert(reducedGlm.dfResidual == 18);
    assert(closeEnough(reducedGlm.residualDeviance, 120.0));
    NestedGeneralizedModelSummary fullGlm = reducedGlm;
    fullGlm.terms = {"x", "z"};
    fullGlm.dfResidual = 16;
    fullGlm.residualDeviance = 100.0;
    NestedModelTestResult glmTest = GeneralizedNestedModelTest(reducedGlm, fullGlm);
    assert(glmTest.ok);
    assert(glmTest.df == 2);
    assert(closeEnough(glmTest.delta, 20.0));
    assert(closeEnough(glmTest.statistic, 20.0));
    assert(glmTest.statisticName == "Chi-square");
    assert(std::isfinite(glmTest.p));
    GeneralizedComparisonFitDisplayData generalizedDisplay;
    generalizedDisplay.ok = true;
    generalizedDisplay.family = "poisson";
    generalizedDisplay.link = "log";
    generalizedDisplay.n = 24;
    generalizedDisplay.nullDeviance = 130.5;
    generalizedDisplay.residualDeviance = 100.25;
    generalizedDisplay.dfResidual = 16;
    generalizedDisplay.aic = 80.2;
    generalizedDisplay.bic = 89.9;
    generalizedDisplay.dispersion = 1.0;
    generalizedDisplay.logLik = -40.1;
    assert(GeneralizedComparisonFitDisplay(generalizedDisplay, glmTest, 0) == "poisson");
    assert(GeneralizedComparisonFitDisplay(generalizedDisplay, glmTest, 4) == "100.250");
    assert(GeneralizedComparisonFitDisplay(generalizedDisplay, glmTest, 10) == "2");
    NestedModelTestResult miD3Test = glmTest;
    miD3Test.df2 = 14.25;
    miD3Test.statisticName = "mice::D3 (fallback because mice::D1 was undefined)";
    assert(GeneralizedComparisonFitDisplay(generalizedDisplay, miD3Test, 10) == "2 / 14.2");
    assert(GeneralizedComparisonFitDisplay(generalizedDisplay, miD3Test, 12) == "20.000 [D3]");
    GeneralizedComparisonFitDisplayData unfittedGeneralized;
    unfittedGeneralized.family = "binomial";
    unfittedGeneralized.link = "logit";
    assert(GeneralizedComparisonFitDisplay(unfittedGeneralized, glmTest, 0) == "binomial");
    assert(GeneralizedComparisonFitDisplay(unfittedGeneralized, glmTest, 2) == "\u2014");

    NestedGeneralizedModelSummary reducedQuasi = reducedGlm;
    reducedQuasi.family = "quasipoisson";
    NestedGeneralizedModelSummary fullQuasi = fullGlm;
    fullQuasi.family = "quasipoisson";
    fullQuasi.dispersion = 2.0;
    NestedModelTestResult quasiTest = GeneralizedNestedModelTest(reducedQuasi, fullQuasi);
    assert(quasiTest.ok);
    assert(quasiTest.df == 2);
    assert(closeEnough(quasiTest.df2, 16.0));
    assert(closeEnough(quasiTest.statistic, 5.0));
    assert(quasiTest.statisticName == "F");
    assert(std::isfinite(quasiTest.p));

    NestedGeneralizedModelSummary linkMismatch = fullGlm;
    linkMismatch.link = "identity";
    assert(!GeneralizedNestedModelTest(reducedGlm, linkMismatch).ok);
    NestedGeneralizedModelSummary badQuasi = fullQuasi;
    badQuasi.dispersion = 0.0;
    assert(!GeneralizedNestedModelTest(reducedQuasi, badQuasi).ok);

    assert(NormalizeLinearDiagnosticKind("observed_vs_fitted") == "observed_fitted");
    assert(NormalizeLinearDiagnosticKind("residuals_vs_fitted") == "residuals_fitted");
    assert(NormalizeLinearDiagnosticKind("unknown") == "unknown");
    assert(LinearDiagnosticKindIsImplemented("observed_vs_fitted"));
    assert(LinearDiagnosticKindIsImplemented("residuals_fitted"));
    assert(LinearDiagnosticKindIsImplemented("residual_histogram"));
    assert(LinearDiagnosticKindIsImplemented("normal_qq"));
    assert(LinearDiagnosticKindIsImplemented("scale_location"));
    assert(LinearDiagnosticKindIsImplemented("residuals_vs_leverage"));
    assert(LinearDiagnosticKindIsImplemented("cooks_distance"));
    assert(!LinearDiagnosticKindIsImplemented("roc_curve"));

    // These are R::stats::qnorm((i - .5) / 5) values transported with
    // diagnostic rows. Native rendering must never synthesize them.
    const std::array<double, 5> rQuantiles = {
        -1.2815515655446004, -0.5244005127080409, 0.0,
        0.5244005127080407, 1.2815515655446004};
    std::vector<std::size_t> residualOrder = {0, 1, 2, 3, 4};
    std::sort(residualOrder.begin(), residualOrder.end(),
              [&](std::size_t a, std::size_t b) {
                  return fit.diagnostics[a].residual < fit.diagnostics[b].residual;
              });
    for (std::size_t i = 0; i < residualOrder.size(); ++i) {
        fit.diagnostics[residualOrder[i]].qqRawQuantile = rQuantiles[i];
    }

    DiagnosticPlotData linearObserved = BuildLinearDiagnosticPlotData("observed_vs_fitted",
                                                                      fit.diagnostics,
                                                                      7,
                                                                      8);
    assert(linearObserved.ok);
    assert(linearObserved.kind == "scatter");
    assert(linearObserved.title == "Observed vs fitted");
    assert(linearObserved.xLabel == "Fitted");
    assert(linearObserved.yLabel == "Observed");
    assert(linearObserved.showIdentityLine);
    assert(linearObserved.fitVersion == 7);
    assert(linearObserved.diagnosticsVersion == 8);
    assert(linearObserved.points.size() == fit.diagnostics.size());
    assert(closeEnough(linearObserved.points[0].x, fit.diagnostics[0].fitted));
    assert(closeEnough(linearObserved.points[0].y, fit.diagnostics[0].observed));

    DiagnosticPlotData linearHistogram = BuildLinearDiagnosticPlotData(
        "residual_histogram", fit.diagnostics, 7, 8);
    assert(linearHistogram.ok);
    assert(linearHistogram.kind == "histogram");
    assert(linearHistogram.title == "Residual histogram");
    assert(linearHistogram.points.size() == fit.diagnostics.size());

    DiagnosticPlotData linearQQ = BuildLinearDiagnosticPlotData(
        "normal_qq", fit.diagnostics, 7, 8);
    assert(linearQQ.ok);
    assert(linearQQ.kind == "scatter");
    assert(linearQQ.title == "Normal Q-Q of residuals");
    assert(linearQQ.points.size() == fit.diagnostics.size());
    assert(closeEnough(linearQQ.points.front().x, rQuantiles.front()));

    DiagnosticPlotData linearScaleLocation = BuildLinearDiagnosticPlotData(
        "scale_location", fit.diagnostics, 7, 8);
    assert(linearScaleLocation.ok);
    assert(linearScaleLocation.title == "Scale-location");

    DiagnosticPlotData linearLeverage = BuildLinearDiagnosticPlotData(
        "residuals_leverage", fit.diagnostics, 7, 8);
    assert(linearLeverage.ok);
    assert(linearLeverage.title == "Residuals vs leverage");

    DiagnosticPlotData linearCooks = BuildLinearDiagnosticPlotData(
        "cooks_distance", fit.diagnostics, 7, 8);
    assert(linearCooks.ok);
    assert(linearCooks.title == "Cook's distance");

    // MI diagnostics retain the coordinates from every independently fitted
    // imputation. The displayed centre is descriptive only; the uncertainty
    // glyph uses the original per-imputation points.
    DiagnosticPlotData secondImputation = linearObserved;
    for (auto &point : secondImputation.points) {
        point.x += 0.4;
        point.y += 0.2;
    }
    DiagnosticPlotData acrossImputations =
        BuildDiagnosticPlotDataAcrossImputations(
            {linearObserved, secondImputation});
    assert(acrossImputations.ok);
    assert(acrossImputations.imputationValues.size() ==
           linearObserved.points.size());
    assert(acrossImputations.imputationValues.front().values.size() == 2);
    assert(closeEnough(acrossImputations.points.front().x,
        linearObserved.points.front().x + 0.2));
    assert(closeEnough(acrossImputations.points.front().y,
        linearObserved.points.front().y + 0.1));
    const int directlyImputedRow = linearObserved.points.front().row;
    DiagnosticPlotData directlyImputedOnly =
        BuildDiagnosticPlotDataAcrossImputations(
            {linearObserved, secondImputation}, {directlyImputedRow});
    assert(directlyImputedOnly.ok);
    assert(directlyImputedOnly.points.size() == linearObserved.points.size());
    assert(directlyImputedOnly.imputationValues.size() == 1);
    assert(directlyImputedOnly.imputationValues.front().row == directlyImputedRow);
    DiagnosticPlotData noDirectlyImputedRows =
        BuildDiagnosticPlotDataAcrossImputations(
            {linearObserved, secondImputation}, {});
    assert(noDirectlyImputedRows.ok);
    assert(noDirectlyImputedRows.points.size() == linearObserved.points.size());
    assert(noDirectlyImputedRows.imputationValues.empty());
    DiagnosticPlotData unsupportedAcross = linearObserved;
    unsupportedAcross.kind = "histogram";
    assert(!BuildDiagnosticPlotDataAcrossImputations(
        {unsupportedAcross, unsupportedAcross}).ok);

    DataFrameModel imputedModelData;
    imputedModelData.datasetType = "multiple_imputation";
    imputedModelData.imputationCount = 5;
    imputedModelData.imputationDisplayMode = "all";
    imputedModelData.rows = 4;
    DataColumn responseColumn;
    responseColumn.name = "outcome";
    responseColumn.imputedMissing = {false, true, false, false};
    DataColumn numericColumn;
    numericColumn.name = "x";
    numericColumn.imputedMissing = {false, false, true, false};
    DataColumn factorColumn;
    factorColumn.name = "g";
    factorColumn.imputedMissing = {false, false, false, true};
    DataColumn unusedColumn;
    unusedColumn.name = "unused";
    unusedColumn.imputedMissing = {true, false, false, false};
    DataColumn exposureColumn;
    exposureColumn.name = "exposure";
    exposureColumn.imputedMissing = {false, false, false, true};
    DataColumn fullyObservedColumn;
    fullyObservedColumn.name = "fully_observed";
    fullyObservedColumn.imputedMissing = {false, false, false, false};
    imputedModelData.columns = {
        responseColumn, numericColumn, factorColumn, unusedColumn, exposureColumn,
        fullyObservedColumn};
    assert(DiagnosticImputationUncertaintyShouldBeDefault(
        imputedModelData, "observed_fitted"));
    assert(!DiagnosticImputationUncertaintyShouldBeDefault(
        imputedModelData, "normal_qq"));
    assert(!DiagnosticImputationUncertaintyShouldBeDefault(
        imputedModelData, "roc_curve"));
    assert(!DiagnosticImputationUncertaintyShouldBeDefault(
        imputedModelData, "residual_histogram"));
    assert(!DiagnosticImputationUncertaintyShouldBeDefault(
        imputedModelData, "observed_predicted_score_distribution"));
    assert(DiagnosticRequiresAcceptedMultipleImputationFit(
        &imputedModelData, false));
    assert(DiagnosticRequiresAcceptedMultipleImputationFit(
        nullptr, true));
    DataFrameModel ordinaryDiagnosticData;
    ordinaryDiagnosticData.datasetType = "data_frame";
    assert(!DiagnosticRequiresAcceptedMultipleImputationFit(
        &ordinaryDiagnosticData, false));
    imputedModelData.imputationDisplayMode = "version";
    assert(!DiagnosticImputationUncertaintyShouldBeDefault(
        imputedModelData, "observed_fitted"));
    imputedModelData.imputationDisplayMode = "all";
    rlispstat::core::ModelSpecification imputedSpecification;
    imputedSpecification.response = "outcome";
    imputedSpecification.terms = {"x", "x:g"};
    const std::set<int> modelImputedRows =
        RowsWithImputedModelInputs(imputedModelData, imputedSpecification);
    assert(modelImputedRows == std::set<int>({2, 3, 4}));
    const std::set<int> modelImputedRowsWithExposure =
        RowsWithImputedModelInputs(
            imputedModelData, imputedSpecification, {"exposure"});
    assert(modelImputedRowsWithExposure == std::set<int>({2, 3, 4}));
    imputedSpecification.terms = {"x"};
    assert(RowsWithImputedModelInputs(imputedModelData, imputedSpecification) ==
           std::set<int>({2, 3}));
    assert(RowsWithImputedModelInputs(
               imputedModelData, imputedSpecification, {"exposure"}) ==
           std::set<int>({2, 3, 4}));

    rlispstat::core::RegressionComparisonState observedComparison;
    observedComparison.multipleImputation = true;
    observedComparison.imputationCount = 5;
    observedComparison.response = "fully_observed";
    rlispstat::core::RegressionComparisonModel observedComparisonModel;
    observedComparisonModel.response = "fully_observed";
    observedComparisonModel.terms = {};
    observedComparison.models.push_back(observedComparisonModel);
    assert(!rlispstat::core::RegressionComparisonUsesImputedModelInputs(
        observedComparison, imputedModelData));
    assert(rlispstat::core::RegressionComparisonMultipleImputationNote(
        observedComparison, &imputedModelData).find("Rubin's rules were not required or applied") !=
        std::string::npos);
    observedComparison.models.front().terms = {"unused"};
    assert(rlispstat::core::RegressionComparisonUsesImputedModelInputs(
        observedComparison, imputedModelData));
    observedComparison.note =
        "Pooled comparison; coefficients use mice::pool() and tests use mice::D3.";
    assert(rlispstat::core::RegressionComparisonMultipleImputationNote(
        observedComparison, &imputedModelData) == observedComparison.note);

    DiagnosticPlotData linearResidual = BuildLinearDiagnosticPlotData("residuals_fitted",
                                                                      fit.diagnostics,
                                                                      7,
                                                                      9);
    assert(linearResidual.ok);
    assert(linearResidual.title == "Residuals vs fitted");
    assert(linearResidual.yLabel == "raw residual");
    assert(closeEnough(linearResidual.points[0].y, fit.diagnostics[0].residual));

    GeneralizedDiagnosticRow gd1;
    gd1.row = 1;
    gd1.observed = 0.0;
    gd1.fitted = 0.20;
    gd1.dunnSmythResidual = -0.7;
    gd1.rawResidual = -0.2;
    gd1.devianceResidual = -1.5;
    gd1.pearsonResidual = -1.2;
    gd1.workingResidual = -1.0;
    gd1.standardizedResidual = -1.7;
    gd1.studentizedResidual = -1.9;
    gd1.leverage = 0.10;
    gd1.cooksDistance = 0.02;
    for (const char *type : {"dunn_smyth", "raw", "deviance", "pearson",
                             "working", "standardized", "studentized"}) {
        gd1.qqTheoreticalQuantiles[type] = -0.6744897501960817;
    }
    GeneralizedDiagnosticRow gd2;
    gd2.row = 2;
    gd2.observed = 1.0;
    gd2.fitted = 0.80;
    gd2.dunnSmythResidual = 0.6;
    gd2.rawResidual = 0.2;
    gd2.devianceResidual = 0.8;
    gd2.pearsonResidual = 1.1;
    gd2.workingResidual = 0.9;
    gd2.standardizedResidual = 1.3;
    gd2.studentizedResidual = 1.5;
    gd2.leverage = 0.25;
    gd2.cooksDistance = 0.08;
    for (const char *type : {"dunn_smyth", "raw", "deviance", "pearson",
                             "working", "standardized", "studentized"}) {
        gd2.qqTheoreticalQuantiles[type] = 0.6744897501960817;
    }
    std::vector<GeneralizedDiagnosticRow> generalizedRows = {gd1, gd2};
    assert(closeEnough(GeneralizedResidualForDiagnostic(gd1, "dunn_smyth"), -0.7));
    assert(closeEnough(GeneralizedResidualForDiagnostic(gd1, "raw"), -0.2));
    assert(closeEnough(GeneralizedResidualForDiagnostic(gd1, "pearson"), -1.2));
    assert(closeEnough(GeneralizedResidualForDiagnostic(gd1, "working"), -1.0));
    assert(closeEnough(GeneralizedResidualForDiagnostic(gd1, "deviance"), -1.5));
    assert(closeEnough(GeneralizedResidualForDiagnostic(gd1, "standardized"), -1.7));
    assert(closeEnough(GeneralizedResidualForDiagnostic(gd1, "studentized"), -1.9));

    DiagnosticPlotData generalizedObserved = BuildGeneralizedDiagnosticPlotData("observed_vs_fitted",
                                                                                "deviance",
                                                                                generalizedRows,
                                                                                2,
                                                                                3);
    assert(generalizedObserved.ok);
    assert(generalizedObserved.diagnosticKind == "observed_fitted");
    assert(generalizedObserved.title == "Observed vs fitted");
    assert(generalizedObserved.xLabel == "Fitted");
    assert(generalizedObserved.yLabel == "Observed");
    assert(generalizedObserved.showIdentityLine);
    assert(generalizedObserved.points.size() == 2);
    assert(closeEnough(generalizedObserved.points[1].y, 1.0));
    rlispstat::core::PlotModel identityRangePlot;
    identityRangePlot.diagnosticShowIdentityLine = true;
    identityRangePlot.xmin = 18.0;
    identityRangePlot.xmax = 23.0;
    identityRangePlot.ymin = 0.0;
    identityRangePlot.ymax = 24.0;
    rlispstat::core::ApplyDiagnosticIdentityRange(identityRangePlot);
    assert(closeEnough(identityRangePlot.xmin, 0.0));
    assert(closeEnough(identityRangePlot.ymin, 0.0));
    assert(closeEnough(identityRangePlot.xmax, 24.0));
    assert(closeEnough(identityRangePlot.ymax, 24.0));

    const std::vector<rlispstat::core::BoundedCountDistributionPoint>
        boundedDistribution = {
            {0.0, 3.0, 2.5}, {1.0, 5.0, 5.5}, {2.0, 2.0, 2.0}};
    DiagnosticPlotData boundedDistributionPlot =
        rlispstat::core::BuildBoundedCountDistributionDiagnosticPlotData(
            boundedDistribution, 12, 13);
    assert(boundedDistributionPlot.ok);
    assert(boundedDistributionPlot.kind == "glm_interaction");
    assert(boundedDistributionPlot.xLabel == "Response");
    assert(boundedDistributionPlot.yLabel == "Frequency");
    assert(boundedDistributionPlot.lines.size() == 2);
    assert(boundedDistributionPlot.lines[0].label == "Observed");
    assert(boundedDistributionPlot.lines[1].label == "Predicted");
    assert(closeEnough(boundedDistributionPlot.lines[0].points[1].y, 5.0));
    assert(closeEnough(boundedDistributionPlot.lines[1].points[1].y, 5.5));

    DiagnosticPlotData generalizedResidual = BuildGeneralizedDiagnosticPlotData("residuals_fitted",
                                                                                "pearson",
                                                                                generalizedRows,
                                                                                2,
                                                                                4);
    assert(generalizedResidual.ok);
    assert(generalizedResidual.title == "Residuals vs fitted");
    assert(generalizedResidual.yLabel == "Pearson residual");
    assert(closeEnough(generalizedResidual.points[0].y, -1.2));

    DiagnosticPlotData generalizedStandardized = BuildGeneralizedDiagnosticPlotData(
        "residuals_fitted", "standardized", generalizedRows, 2, 4);
    DiagnosticPlotData generalizedStudentized = BuildGeneralizedDiagnosticPlotData(
        "residuals_fitted", "studentized", generalizedRows, 2, 4);
    assert(generalizedStandardized.ok && generalizedStudentized.ok);
    assert(generalizedStandardized.yLabel == "standardized residual");
    assert(generalizedStudentized.yLabel == "studentized residual");
    assert(closeEnough(generalizedStandardized.points[0].y, -1.7));
    assert(closeEnough(generalizedStudentized.points[0].y, -1.9));

    DiagnosticPlotData histogram = BuildGeneralizedDiagnosticPlotData("histogram",
                                                                      "working",
                                                                      generalizedRows,
                                                                      2,
                                                                      5);
    assert(histogram.ok);
    assert(histogram.kind == "histogram");
    assert(histogram.title == "Residual histogram");
    assert(histogram.xLabel == "working residual");
    assert(closeEnough(histogram.points[1].x, 0.9));

    DiagnosticPlotData qq = BuildGeneralizedDiagnosticPlotData("qq",
                                                               "deviance",
                                                               generalizedRows,
                                                               2,
                                                               6);
    assert(qq.ok);
    assert(qq.diagnosticKind == "normal_qq");
    assert(qq.points.size() == 2);
    assert(qq.points[0].x < 0.0);
    assert(qq.points[1].x > 0.0);
    assert(qq.points[0].y < qq.points[1].y);

    // Residual selection is a diagnostic option, not part of the fitted-model
    // specification.  Rebuilding an already-open diagnostic with a different
    // option must therefore change the numerical points without changing the
    // fit version or requesting another fit.
    DiagnosticOptions devianceOptions;
    devianceOptions.residualType = "deviance";
    devianceOptions.version = 7;
    DiagnosticOptions pearsonOptions = devianceOptions;
    pearsonOptions.residualType = "pearson";
    pearsonOptions.version = 8;
    DiagnosticPlotData devianceDiagnostic = BuildGeneralizedDiagnosticPlotData(
        "residuals_fitted", devianceOptions, generalizedRows, 11, 12);
    DiagnosticPlotData pearsonDiagnostic = BuildGeneralizedDiagnosticPlotData(
        "residuals_fitted", pearsonOptions, generalizedRows, 11, 12);
    assert(devianceDiagnostic.ok && pearsonDiagnostic.ok);
    assert(devianceDiagnostic.fitVersion == pearsonDiagnostic.fitVersion);
    assert(devianceDiagnostic.diagnosticsVersion == pearsonDiagnostic.diagnosticsVersion);
    assert(devianceDiagnostic.diagnosticOptionsVersion == 7);
    assert(pearsonDiagnostic.diagnosticOptionsVersion == 8);
    assert(devianceDiagnostic.residualType == "deviance");
    assert(pearsonDiagnostic.residualType == "pearson");
    assert(!closeEnough(devianceDiagnostic.points[0].y,
                        pearsonDiagnostic.points[0].y));

    for (const std::string &family :
         {std::string("poisson"), std::string("negative_binomial")}) {
        GeneralizedGLMState countDiagnosticState;
        countDiagnosticState.family = family;
        countDiagnosticState.countRegression = true;
        countDiagnosticState.countDistribution = family == "negative_binomial"
            ? CountDistribution::NegativeBinomial : CountDistribution::Poisson;
        const auto residualTypes = AvailableGeneralizedResidualTypes(countDiagnosticState);
        assert((residualTypes == std::vector<std::string>{
            "dunn_smyth", "pearson", "deviance", "raw"}));
        assert(IsGeneralizedResidualTypeAvailable(countDiagnosticState, "dunn_smyth"));
        assert(IsGeneralizedResidualTypeAvailable(countDiagnosticState, "deviance"));
        assert(IsGeneralizedResidualTypeAvailable(countDiagnosticState, "pearson"));
        assert(IsGeneralizedResidualTypeAvailable(countDiagnosticState, "raw"));
        assert(!IsGeneralizedResidualTypeAvailable(countDiagnosticState, "invalid"));
        assert(rlispstat::core::DefaultGeneralizedResidualType(countDiagnosticState) ==
               "dunn_smyth");
    }
    GeneralizedGLMState quasiDiagnosticState;
    quasiDiagnosticState.family = "quasipoisson";
    quasiDiagnosticState.countRegression = true;
    quasiDiagnosticState.countDistribution = CountDistribution::QuasiPoisson;
    assert((AvailableGeneralizedResidualTypes(quasiDiagnosticState) ==
            std::vector<std::string>{"pearson", "deviance", "raw"}));
    GeneralizedGLMState betaBinomialDiagnosticState;
    betaBinomialDiagnosticState.countRegression = true;
    betaBinomialDiagnosticState.countDistribution = CountDistribution::BetaBinomial;
    assert((AvailableGeneralizedResidualTypes(betaBinomialDiagnosticState) ==
            std::vector<std::string>{"dunn_smyth", "pearson", "raw"}));
    assert(!IsGeneralizedResidualTypeAvailable(betaBinomialDiagnosticState, "deviance"));

    GeneralizedGLMState negativeBinomialExportState;
    negativeBinomialExportState.id = "negative-binomial-diagnostic";
    negativeBinomialExportState.response = "count";
    negativeBinomialExportState.terms = {"group", "time", "group:time"};
    negativeBinomialExportState.family = "poisson";
    negativeBinomialExportState.link = "log";
    negativeBinomialExportState.countRegression = true;
    negativeBinomialExportState.countDistribution = CountDistribution::NegativeBinomial;
    negativeBinomialExportState.multipleImputation = true;
    negativeBinomialExportState.imputationCount = 5;
    negativeBinomialExportState.modelVersion = 4;
    negativeBinomialExportState.provenance.analysisId =
        negativeBinomialExportState.id;
    negativeBinomialExportState.provenance.verificationRCode["model"] =
        "completed_sets <- split(mi_long[mi_long$.imp > 0, ], "
        "mi_long$.imp[mi_long$.imp > 0])\n"
        "fits <- lapply(completed_sets, function(data) "
        "MASS::glm.nb(count ~ group * time, data = data, link = \"log\"))\n";
    PlotModel diagnosticExportPlot;
    diagnosticExportPlot.id = "diagnostic-export";
    diagnosticExportPlot.title = "Residual histogram";
    diagnosticExportPlot.xLabel = "Dunn-Smyth residual";
    diagnosticExportPlot.yLabel = "Frequency";
    diagnosticExportPlot.glmDiagnosticKind = "residual_histogram";
    diagnosticExportPlot.displayedResidualType = "dunn_smyth";
    diagnosticExportPlot.diagnosticImputationIndex = 3;
    diagnosticExportPlot.diagnosticAvailableResidualTypes =
        AvailableGeneralizedResidualTypes(negativeBinomialExportState);
    diagnosticExportPlot.rExportTheme = "minimal";
    const auto negativeBinomialDiagnosticReference =
        rlispstat::core::GeneralizedDiagnosticPlotCodeReference(
            diagnosticExportPlot, negativeBinomialExportState);
    const std::string negativeBinomialDiagnosticRecipe =
        negativeBinomialDiagnosticReference.provenance.verificationRCode.at(
            "diagnostic:residual_histogram");
    assert(negativeBinomialDiagnosticRecipe.find("diagnostic_imputation <- 3L") !=
           std::string::npos);
    assert(negativeBinomialDiagnosticRecipe.find("stats::dnbinom") !=
           std::string::npos);
    assert(negativeBinomialDiagnosticRecipe.find("diagnostic_fit$theta") !=
           std::string::npos);
    assert(negativeBinomialDiagnosticRecipe.find("ggplot2::geom_histogram") !=
           std::string::npos);
    assert(negativeBinomialDiagnosticRecipe.find("ggplot2::theme_minimal()") !=
           std::string::npos);
    assert(rlispstat::core::BuildPublicationRCode(
               negativeBinomialDiagnosticReference,
               rlispstat::core::PublicationBackend::Ggplot2) ==
           negativeBinomialDiagnosticRecipe);

    PlotModel betaBinomialExportPlot = diagnosticExportPlot;
    betaBinomialExportPlot.diagnosticAvailableResidualTypes =
        AvailableGeneralizedResidualTypes(betaBinomialDiagnosticState);
    betaBinomialDiagnosticState.id = "beta-binomial-diagnostic";
    betaBinomialDiagnosticState.response = "successes";
    betaBinomialDiagnosticState.family = "binomial";
    betaBinomialDiagnosticState.link = "logit";
    betaBinomialDiagnosticState.trialsConstant = 24.0;
    betaBinomialDiagnosticState.provenance.analysisId =
        betaBinomialDiagnosticState.id;
    betaBinomialDiagnosticState.provenance.verificationRCode["model"] =
        "fit <- glmmTMB::glmmTMB(cbind(successes, 24 - successes) ~ group, "
        "data = analysis_data, family = glmmTMB::betabinomial(link = \"logit\"))\n";
    const auto betaBinomialDiagnosticReference =
        rlispstat::core::GeneralizedDiagnosticPlotCodeReference(
            betaBinomialExportPlot, betaBinomialDiagnosticState);
    const std::string betaBinomialDiagnosticRecipe =
        betaBinomialDiagnosticReference.provenance.verificationRCode.at(
            "diagnostic:residual_histogram");
    assert(betaBinomialDiagnosticRecipe.find("gamlss.dist::dBB") !=
           std::string::npos);
    assert(betaBinomialDiagnosticRecipe.find("beta_binomial_pmf") ==
           std::string::npos);
    assert(betaBinomialDiagnosticRecipe.find("type = \"deviance\"") ==
           std::string::npos);

    DiagnosticPlotData boundaryPlot =
        rlispstat::core::BuildDiscreteBoundaryDiagnosticPlotData(
            boundedDistribution, true, 12, 13);
    assert(boundaryPlot.ok);
    assert(boundaryPlot.title == "Floor / ceiling fit");
    assert(boundaryPlot.lines.size() == 2);
    assert(boundaryPlot.lines[0].points.size() == 2);
    assert(boundaryPlot.points.size() == 4);
    rlispstat::core::PlotModel boundaryViewport;
    boundaryViewport.kind = boundaryPlot.kind;
    boundaryViewport.glmDiagnosticKind = boundaryPlot.diagnosticKind;
    boundaryViewport.interactionPlotLines = boundaryPlot.lines;
    for (const auto &point : boundaryPlot.points) {
        assert(point.row == 0);
        boundaryViewport.points.push_back({point.x, point.y, point.row});
    }
    rlispstat::core::ComputeRanges(boundaryViewport);
    assert(boundaryViewport.xmin <= 0.0 && boundaryViewport.xmax >= 2.0);
    assert(boundaryViewport.ymin <= 2.0 && boundaryViewport.ymax >= 3.0);
    assert(!rlispstat::core::PlotLinksToDataRows(boundaryViewport));
    assert(!rlispstat::core::PlotHasConfidenceIntervals(boundaryViewport));
    const auto zeroPlot = rlispstat::core::BuildDiscreteBoundaryDiagnosticPlotData(
        boundedDistribution, false, 12, 13);
    assert(zeroPlot.ok && zeroPlot.points.size() == 2);
    assert(closeEnough(zeroPlot.points[0].x, 0.0));
    assert(closeEnough(zeroPlot.points[0].y, 3.0));
    assert(closeEnough(zeroPlot.points[1].y, 2.5));

    // Diagnostic windows are bound to the stable semantic model id, never to
    // the presentation column.  This keeps the binding intact when comparison
    // columns are reordered or an unrelated model is deleted.
    RegressionComparisonState stableLinearDiagnostics;
    stableLinearDiagnostics.id = "linear-diagnostic-comparison";
    RegressionComparisonModel linearModel1;
    linearModel1.id = stableLinearDiagnostics.id + ":model:1";
    linearModel1.response = "y";
    linearModel1.terms = {"planet"};
    RegressionComparisonModel linearModel2 = linearModel1;
    linearModel2.id = stableLinearDiagnostics.id + ":model:2";
    linearModel2.terms = {"planet", "x"};
    RegressionComparisonModel linearModel3 = linearModel1;
    linearModel3.id = stableLinearDiagnostics.id + ":model:3";
    stableLinearDiagnostics.models = {linearModel1, linearModel2, linearModel3};
    assert(RegressionComparisonModelById(stableLinearDiagnostics, linearModel2.id) ==
           &stableLinearDiagnostics.models[1]);
    std::swap(stableLinearDiagnostics.models[0], stableLinearDiagnostics.models[1]);
    assert(RegressionComparisonModelById(stableLinearDiagnostics, linearModel2.id) ==
           &stableLinearDiagnostics.models[0]);
    stableLinearDiagnostics.models.erase(stableLinearDiagnostics.models.begin() + 1);
    assert(RegressionComparisonModelById(stableLinearDiagnostics, linearModel2.id) ==
           &stableLinearDiagnostics.models[0]);
    assert(RegressionComparisonModelById(stableLinearDiagnostics, "missing") == nullptr);

    // Binary/generalized comparison acceptance case:
    //   Model 1: binary_y ~ planet
    //   Model 2: binary_y ~ planet + x
    // Once x is removed and Model 2 is refitted, its open Q-Q diagnostic must
    // become numerically identical to Model 1.  A later Model 1-only refit must
    // not alter Model 2's diagnostic version or data.
    rlispstat::core::GeneralizedComparisonState stableBinaryDiagnostics;
    stableBinaryDiagnostics.id = "binary-diagnostic-comparison";
    stableBinaryDiagnostics.response = "binary_y";
    stableBinaryDiagnostics.family = "binomial";
    stableBinaryDiagnostics.link = "logit";
    rlispstat::core::GeneralizedComparisonModel binaryModel1;
    binaryModel1.id = stableBinaryDiagnostics.id + ":model:1";
    binaryModel1.response = "binary_y";
    binaryModel1.family = "binomial";
    binaryModel1.link = "logit";
    binaryModel1.terms = {"planet"};
    binaryModel1.fit.ok = true;
    binaryModel1.diagnosticOptions.residualType = "deviance";
    binaryModel1.diagnosticOptions.version = 1;
    binaryModel1.fit.diagnosticOptions = binaryModel1.diagnosticOptions;
    binaryModel1.fit.diagnostics = generalizedRows;
    binaryModel1.fit.diagnosticsVersion = 1;
    binaryModel1.fitVersion = 1;
    rlispstat::core::GeneralizedComparisonModel binaryModel2 = binaryModel1;
    binaryModel2.id = stableBinaryDiagnostics.id + ":model:2";
    binaryModel2.terms = {"planet", "x"};
    binaryModel2.fit.diagnostics[0].devianceResidual = -0.4;
    binaryModel2.fit.diagnostics[1].devianceResidual = 0.3;
    stableBinaryDiagnostics.models = {binaryModel1, binaryModel2};

    // Each comparison model owns its diagnostic options.  Changing Model 2's
    // residual definition cannot mutate Model 1 or either model specification.
    const int model1VersionBeforeDiagnosticChange =
        stableBinaryDiagnostics.models[0].modelVersion;
    const int model2VersionBeforeDiagnosticChange =
        stableBinaryDiagnostics.models[1].modelVersion;
    stableBinaryDiagnostics.models[1].diagnosticOptions.residualType = "working";
    ++stableBinaryDiagnostics.models[1].diagnosticOptions.version;
    stableBinaryDiagnostics.models[1].fit.diagnosticOptions =
        stableBinaryDiagnostics.models[1].diagnosticOptions;
    assert(stableBinaryDiagnostics.models[0].diagnosticOptions.residualType == "deviance");
    assert(stableBinaryDiagnostics.models[0].diagnosticOptions.version == 1);
    assert(stableBinaryDiagnostics.models[1].diagnosticOptions.residualType == "working");
    assert(stableBinaryDiagnostics.models[1].diagnosticOptions.version == 2);
    assert(stableBinaryDiagnostics.models[0].modelVersion ==
           model1VersionBeforeDiagnosticChange);
    assert(stableBinaryDiagnostics.models[1].modelVersion ==
           model2VersionBeforeDiagnosticChange);
    stableBinaryDiagnostics.models[1].diagnosticOptions.residualType = "deviance";
    ++stableBinaryDiagnostics.models[1].diagnosticOptions.version;
    stableBinaryDiagnostics.models[1].fit.diagnosticOptions =
        stableBinaryDiagnostics.models[1].diagnosticOptions;

    auto const* stableModel1 = rlispstat::core::GeneralizedComparisonModelById(
        stableBinaryDiagnostics, binaryModel1.id);
    auto const* stableModel2 = rlispstat::core::GeneralizedComparisonModelById(
        stableBinaryDiagnostics, binaryModel2.id);
    assert(stableModel1 && stableModel2);
    DiagnosticPlotData model1Qq = BuildGeneralizedDiagnosticPlotData(
        "qq", stableModel1->diagnosticOptions, stableModel1->fit.diagnostics,
        stableModel1->fitVersion, stableModel1->fit.diagnosticsVersion);
    DiagnosticPlotData model2Qq = BuildGeneralizedDiagnosticPlotData(
        "qq", stableModel2->diagnosticOptions, stableModel2->fit.diagnostics,
        stableModel2->fitVersion, stableModel2->fit.diagnosticsVersion);
    assert(model1Qq.ok && model2Qq.ok);
    assert(!closeEnough(model1Qq.points[0].y, model2Qq.points[0].y));

    auto *updatedModel2 = rlispstat::core::GeneralizedComparisonModelById(
        stableBinaryDiagnostics, binaryModel2.id);
    assert(updatedModel2);
    updatedModel2->terms = {"planet"};
    updatedModel2->fit.diagnostics = generalizedRows;
    updatedModel2->fitVersion = 2;
    updatedModel2->fit.diagnosticsVersion = 2;
    model2Qq = BuildGeneralizedDiagnosticPlotData(
        "qq", updatedModel2->diagnosticOptions, updatedModel2->fit.diagnostics,
        updatedModel2->fitVersion, updatedModel2->fit.diagnosticsVersion);
    assert(model2Qq.fitVersion == 2);
    assert(model2Qq.diagnosticsVersion == 2);
    assert(model1Qq.points.size() == model2Qq.points.size());
    for (std::size_t index = 0; index < model1Qq.points.size(); ++index) {
        assert(closeEnough(model1Qq.points[index].x, model2Qq.points[index].x));
        assert(closeEnough(model1Qq.points[index].y, model2Qq.points[index].y));
    }

    const int model2FitVersion = updatedModel2->fitVersion;
    const int model2DiagnosticsVersion = updatedModel2->fit.diagnosticsVersion;
    auto *updatedModel1 = rlispstat::core::GeneralizedComparisonModelById(
        stableBinaryDiagnostics, binaryModel1.id);
    assert(updatedModel1);
    updatedModel1->fit.diagnostics[0].devianceResidual = -2.0;
    updatedModel1->fitVersion = 3;
    updatedModel1->fit.diagnosticsVersion = 3;
    stableModel2 = rlispstat::core::GeneralizedComparisonModelById(
        stableBinaryDiagnostics, binaryModel2.id);
    assert(stableModel2);
    assert(stableModel2->fitVersion == model2FitVersion);
    assert(stableModel2->fit.diagnosticsVersion == model2DiagnosticsVersion);

    std::swap(stableBinaryDiagnostics.models[0], stableBinaryDiagnostics.models[1]);
    assert(rlispstat::core::GeneralizedComparisonModelById(
               stableBinaryDiagnostics, binaryModel2.id) ==
           &stableBinaryDiagnostics.models[0]);
    stableBinaryDiagnostics.models.erase(stableBinaryDiagnostics.models.begin() + 1);
    assert(rlispstat::core::GeneralizedComparisonModelById(
               stableBinaryDiagnostics, binaryModel2.id) ==
           &stableBinaryDiagnostics.models[0]);
    assert(rlispstat::core::GeneralizedComparisonModelById(
               stableBinaryDiagnostics, "missing") == nullptr);

    DiagnosticPlotData leverage = BuildGeneralizedDiagnosticPlotData("residuals_vs_leverage",
                                                                     "deviance",
                                                                     generalizedRows,
                                                                     2,
                                                                     7);
    assert(leverage.ok);
    assert(leverage.title == "Residuals vs leverage");
    assert(closeEnough(leverage.points[1].x, 0.25));

    DiagnosticPlotData cooks = BuildGeneralizedDiagnosticPlotData("cooks",
                                                                  "deviance",
                                                                  generalizedRows,
                                                                  2,
                                                                  8);
    assert(cooks.ok);
    assert(cooks.title == "Cook's distance");
    assert(closeEnough(cooks.points[0].x, 1.0));
    assert(closeEnough(cooks.points[0].y, 0.02));

    GeneralizedDiagnosticRow roc1; roc1.row = 11; roc1.fitted = 0.8; roc1.observedBinary = 1.0;
    GeneralizedDiagnosticRow roc2; roc2.row = 12; roc2.fitted = 0.8; roc2.observedBinary = 0.0;
    GeneralizedDiagnosticRow roc3; roc3.row = 13; roc3.fitted = 0.3; roc3.observedBinary = 1.0;
    GeneralizedDiagnosticRow roc4; roc4.row = 14; roc4.fitted = 0.1; roc4.observedBinary = 0.0;
    DiagnosticPlotData roc = BuildGeneralizedDiagnosticPlotData(
        "roc_curve", "deviance", {roc1, roc2, roc3, roc4}, 3, 9);
    assert(roc.ok);
    assert(roc.points.size() == 3);             // unique fitted probabilities
    assert(roc.rocThresholds.size() == 3);      // empirical interactive thresholds
    assert(roc.visualPoints.size() > roc.points.size()); // independent step geometry
    assert((roc.rocThresholds[0].crossingRows == std::vector<int>{11, 12}));
    assert(roc.rocThresholds[0].truePositive == 1);
    assert(roc.rocThresholds[0].falsePositive == 1);
    assert((roc.rocThresholds[0].falsePositiveRows == std::vector<int>{12}));
    assert((roc.rocThresholds[0].falseNegativeRows == std::vector<int>{13}));
    assert(closeEnough(roc.rocThresholds[0].truePositiveRate, 0.5));
    assert(closeEnough(roc.rocThresholds[0].falsePositiveRate, 0.5));
    assert(closeEnough(roc.visualPoints.front().x, 0.0));
    assert(closeEnough(roc.visualPoints.front().y, 0.0));
    assert(closeEnough(roc.visualPoints.back().x, 1.0));
    assert(closeEnough(roc.visualPoints.back().y, 1.0));

    DiagnosticPlotData emptyDiagnostic = BuildGeneralizedDiagnosticPlotData("residuals_fitted",
                                                                           "deviance",
                                                                           {},
                                                                           0,
                                                                           0);
    assert(!emptyDiagnostic.ok);
    assert(emptyDiagnostic.message.find("no diagnostic rows") != std::string::npos);

    GeneralizedDiagnosticRow badDiagnostic;
    badDiagnostic.row = 1;
    badDiagnostic.devianceResidual = std::numeric_limits<double>::quiet_NaN();
    DiagnosticPlotData badHistogram = BuildGeneralizedDiagnosticPlotData("histogram",
                                                                         "deviance",
                                                                         {badDiagnostic},
                                                                         0,
                                                                         0);
    assert(!badHistogram.ok);
    assert(badHistogram.message.find("not finite") != std::string::npos);

    std::vector<double> difference = SubtractVectors({1.0, 5.0, 9.0}, {0.5, 2.0, 4.0});
    assert((difference == std::vector<double>{0.5, 3.0, 5.0}));
    assert(SubtractVectors({1.0}, {1.0, 2.0}).empty());

    std::vector<double> beta = {2.0, -1.0, 0.5};
    std::vector<double> contrast = {1.0, 2.0, -2.0};
    std::vector<std::vector<double>> covariance = {
        {4.0, 0.5, 0.0},
        {0.5, 1.0, 0.25},
        {0.0, 0.25, 0.5}
    };
    assert(closeEnough(ContrastEstimate(beta, contrast), -1.0));
    assert(closeEnough(ContrastVariance(covariance, contrast), 10.0));
    assert(!std::isfinite(ContrastEstimate(beta, {1.0, 2.0})));
    assert(!std::isfinite(ContrastVariance({{1.0}}, {1.0, 2.0})));

    LinearFunctionEstimate estimate = EstimateLinearFunction(beta, covariance, contrast, "contrast", 0.95);
    assert(estimate.label == "contrast");
    assert(closeEnough(estimate.estimate, -1.0));
    assert(closeEnough(estimate.stdError, std::sqrt(10.0)));
    assert(closeEnough(estimate.statistic, -1.0 / std::sqrt(10.0)));
    assert(std::isfinite(estimate.pValue));
    assert(closeEnough(estimate.ciLower, -1.0 - 1.95996398 * std::sqrt(10.0), 1.0e-5));
    assert(closeEnough(estimate.ciUpper, -1.0 + 1.95996398 * std::sqrt(10.0), 1.0e-5));

    std::vector<double> adjusted = HolmAdjustedPValues({0.04, 0.001, 0.20, std::numeric_limits<double>::quiet_NaN()});
    assert(closeEnough(adjusted[0], 0.08));
    assert(closeEnough(adjusted[1], 0.003));
    assert(closeEnough(adjusted[2], 0.20));
    assert(!std::isfinite(adjusted[3]));

    std::vector<LinearFunctionEstimate> estimates(3);
    estimates[0].pValue = 0.04;
    estimates[1].pValue = 0.001;
    estimates[2].pValue = 0.20;
    ApplyHolmAdjustedPValues(estimates);
    assert(closeEnough(estimates[0].adjustedPValue, 0.08));
    assert(closeEnough(estimates[1].adjustedPValue, 0.003));
    assert(closeEnough(estimates[2].adjustedPValue, 0.20));

    ScenarioDesignInput scenario;
    scenario.terms = {"x", "group", "x:group"};
    scenario.expectedSize = 6;
    ScenarioVariableInfo xInfo;
    xInfo.type = "numeric";
    xInfo.numericDefault = 10.0;
    ScenarioVariableInfo groupInfo;
    groupInfo.type = "factor";
    groupInfo.factorLevels = {"A", "B", "C"};
    scenario.variables["x"] = xInfo;
    scenario.variables["group"] = groupInfo;
    std::vector<double> design;
    assert(BuildScenarioDesignVector(scenario, design));
    assert((design == std::vector<double>{1.0, 10.0, 0.0, 0.0, 0.0, 0.0}));

    scenario.numericValues["x"] = 12.0;
    scenario.factorValues["group"] = "C";
    assert(BuildScenarioDesignVector(scenario, design));
    assert((design == std::vector<double>{1.0, 12.0, 0.0, 1.0, 0.0, 12.0}));

    scenario.terms = {"group:x"};
    scenario.expectedSize = 3;
    assert(BuildScenarioDesignVector(scenario, design));
    assert((design == std::vector<double>{1.0, 0.0, 12.0}));

    scenario.terms = {"factor(group)"};
    scenario.termTypes["factor(group)"] = "factor";
    scenario.expectedSize = 3;
    assert(BuildScenarioDesignVector(scenario, design));
    assert((design == std::vector<double>{1.0, 0.0, 1.0}));

    ScenarioDesignInput badFactor = scenario;
    badFactor.variables["group"].factorLevels.clear();
    assert(!BuildScenarioDesignVector(badFactor, design));
    assert(design.empty());

    ScenarioDesignInput mismatchedSize = scenario;
    mismatchedSize.expectedSize = 4;
    assert(!BuildScenarioDesignVector(mismatchedSize, design));
    assert(design.empty());

    // Greece occurs first in the data, but the fitted treatment coding uses
    // Finland as reference.  Every post-estimation surface must preserve that
    // fitted identity instead of rebuilding levels in observation order.
    PlotModel countryModel;
    countryModel.group = "country_baseline";
    const std::string responseName = "score_sum_situational_pressure_second_evaluation";
    const std::string baselineName = "score_sum_situational_pressure_first_evaluation";
    countryModel.yLabel = responseName;
    std::vector<double> baselineValues;
    std::vector<double> responseValues;
    std::vector<std::string> countries;
    std::vector<std::string> studyGroups;
    for (const std::string &country : {std::string("Greece"), std::string("Finland")}) {
        for (const std::string &studyGroup : {std::string("control"), std::string("treatment")}) {
            for (int value = 0; value < 4; ++value) {
                baselineValues.push_back(value);
                countries.push_back(country);
                studyGroups.push_back(studyGroup);
                responseValues.push_back(
                    2.0 + 0.3547 * value +
                    (country == "Greece" ? -0.3930 - 0.2011 * value : 0.0) +
                    (studyGroup == "treatment" ? 0.25 : 0.0) +
                    0.01 * std::array<double, 4>{1.0, -1.0, -1.0, 1.0}[value]);
            }
        }
    }
    countryModel.variables = {
        {responseName, responseValues}, {baselineName, baselineValues}
    };
    countryModel.variableMeta = {
        {responseName, "numeric"}, {baselineName, "numeric"},
        {"study_group", "factor"}, {"country", "factor"}
    };
    DataFrameModel countryData;
    countryData.group = countryModel.group;
    countryData.rows = static_cast<int>(baselineValues.size());
    countryData.columns = {
        {"study_group", "factor", "", "", -1, studyGroups},
        {"country", "factor", "", "", -1, countries}
    };
    const std::string countryInteraction = baselineName + ":country";
    const std::vector<std::string> countryTerms = {
        baselineName, "study_group", "country", countryInteraction
    };
    const std::map<std::string, std::string> countryTypes = {
        {"study_group", "factor"}, {"country", "factor"}
    };
    GLMFitSummary countryFit = FitMultipleLinearModel(
        countryModel, &countryData, {}, responseName, countryTerms, countryTypes, "all");
    assert(countryFit.ok);
    const std::string factorInteraction = "study_group:country";
    GLMFitSummary factorInteractionFit = FitMultipleLinearModel(
        countryModel, &countryData, {}, responseName,
        {"study_group", "country", factorInteraction}, countryTypes, "all");
    assert(factorInteractionFit.ok);
    const auto factorInteractionParent = std::find_if(
        factorInteractionFit.coefficients.begin(), factorInteractionFit.coefficients.end(),
        [&](const GLMCoefficientRow &row) {
            return row.sourceTerm == factorInteraction && row.rowType == "term_parent";
        });
    const auto factorInteractionChild = std::find_if(
        factorInteractionFit.coefficients.begin(), factorInteractionFit.coefficients.end(),
        [&](const GLMCoefficientRow &row) {
            return row.sourceTerm == factorInteraction && row.rowType == "coefficient";
        });
    assert(factorInteractionParent != factorInteractionFit.coefficients.end());
    assert(factorInteractionParent->displayLabel == "study_group × country");
    assert(factorInteractionChild != factorInteractionFit.coefficients.end());
    assert(factorInteractionChild->displayLabel ==
           "  treatment × Greece (vs control × Finland)");

    // Polynomial terms must be real design-matrix columns, not aliases of
    // the untransformed predictor.  Keep a small, non-degenerate residual so
    // the ordinary OLS inference path remains defined as well.
    PlotModel polynomialModel;
    polynomialModel.group = "polynomial_design";
    polynomialModel.variables = {
        {"y", {1.1, 3.9, 9.2, 15.8, 25.1, 35.9, 49.2}},
        {"x", {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0}}
    };
    polynomialModel.variableMeta = {{"y", "numeric"}, {"x", "numeric"}};
    const GLMFitSummary polynomialFit = FitMultipleLinearModel(
        polynomialModel, nullptr, {}, "y", {"I(x^2)"}, {}, "all");
    if (!polynomialFit.ok) {
        std::cerr << "Polynomial fit warning: " << polynomialFit.warning << "\n";
    }
    assert(polynomialFit.ok);
    assert(polynomialFit.designLabels.size() == 2);
    assert(polynomialFit.designLabels[1] == "I(x^2)");
    assert(polynomialFit.beta.size() == 2);
    assert(std::fabs(polynomialFit.beta[0]) < 0.15);
    assert(std::fabs(polynomialFit.beta[1] - 1.0) < 0.01);
    assert(polynomialFit.r2 > 0.9999);
    rlispstat::core::ModelSpecification countrySpecification;
    countrySpecification.response = responseName;
    countrySpecification.terms = countryTerms;
    countrySpecification.termTypes = {
        {baselineName, "numeric"}, {"study_group", "factor"}, {"country", "numeric"}
    };
    countrySpecification.termTypeOverrides = {{"country", "factor"}};
    countrySpecification.factorReferenceLevels = {{"country", "Greece"}};
    const auto countrySpecificationFit = FitMultipleLinearModel(
        countryModel, &countryData, {}, countrySpecification);
    assert(countrySpecificationFit.ok);
    const auto *countrySpecificationCoding =
        rlispstat::core::FactorCodingForVariable(countrySpecificationFit, "country");
    assert(countrySpecificationCoding != nullptr);
    assert(countrySpecificationCoding->referenceLevel == "Greece");
    const auto interceptEffect = std::find_if(countryFit.coefficients.begin(), countryFit.coefficients.end(),
        [](const GLMCoefficientRow &row) { return row.term == "(Intercept)"; });
    assert(interceptEffect != countryFit.coefficients.end());
    assert(!std::isfinite(interceptEffect->partialR));
    assert(!std::isfinite(interceptEffect->deltaR2));
    const auto countryParent = std::find_if(countryFit.coefficients.begin(), countryFit.coefficients.end(),
        [](const GLMCoefficientRow &row) { return row.sourceTerm == "country" && row.rowType == "factor_parent"; });
    assert(countryParent != countryFit.coefficients.end());
    assert(!std::isfinite(countryParent->partialR));
    assert(std::isfinite(countryParent->deltaR2));
    const auto countryReference = std::find_if(countryFit.coefficients.begin(), countryFit.coefficients.end(),
        [](const GLMCoefficientRow &row) { return row.sourceTerm == "country" && row.rowType == "reference"; });
    assert(countryReference != countryFit.coefficients.end());
    assert(!std::isfinite(countryReference->partialR));
    assert(!std::isfinite(countryReference->deltaR2));
    const auto countryContrast = std::find_if(countryFit.coefficients.begin(), countryFit.coefficients.end(),
        [](const GLMCoefficientRow &row) { return row.sourceTerm == "country" && row.rowType == "factor_level"; });
    assert(countryContrast != countryFit.coefficients.end());
    assert(std::isfinite(countryContrast->partialR));
    assert(!std::isfinite(countryContrast->deltaR2));
    const auto *countryCoding = rlispstat::core::FactorCodingForVariable(countryFit, "country");
    assert(countryCoding != nullptr);
    assert(countryCoding->referenceLevel == "Finland");
    assert((countryCoding->levels == std::vector<std::string>{"Finland", "Greece"}));
    const auto interactionRow = std::find_if(
        countryFit.coefficients.begin(), countryFit.coefficients.end(),
        [&](const GLMCoefficientRow &row) {
            return row.sourceTerm == countryInteraction && row.rowType == "coefficient";
        });
    assert(interactionRow != countryFit.coefficients.end());
    assert(interactionRow->factorLevel == "Greece");
    assert(interactionRow->referenceLevel == "Finland");
    assert(interactionRow->displayLabel.find(
        baselineName + " × country = Greece (vs Finland)") != std::string::npos);

    const auto baselineEffect = std::find_if(
        countryFit.coefficients.begin(), countryFit.coefficients.end(),
        [&](const GLMCoefficientRow &row) {
            return row.sourceTerm == baselineName && row.rowType == "coefficient";
        });
    assert(baselineEffect != countryFit.coefficients.end());
    const auto fittedInteractionParent = std::find_if(
        countryFit.coefficients.begin(), countryFit.coefficients.end(),
        [&](const GLMCoefficientRow &row) {
            return row.sourceTerm == countryInteraction && row.rowType == "term_parent";
        });
    assert(fittedInteractionParent != countryFit.coefficients.end());

    // The fitted Finland/Greece regression is also the cross-platform display
    // regression test: every cell retains the macOS distinction between a
    // structural blank and a statistic that is not applicable.
    const auto assertDisplayStates = [](
        const GLMCoefficientRow &row,
        const std::array<GLMTableDisplayState, 9> &expected) {
        const auto cells = GLMRegressionTableRow(row);
        assert(cells.size() == expected.size());
        for (std::size_t column = 0; column < cells.size(); ++column) {
            assert(cells[column].state == expected[column]);
            const std::string text = GLMRegressionTableCellText(cells[column]);
            if (expected[column] == GLMTableDisplayState::Blank) assert(text.empty());
            if (expected[column] == GLMTableDisplayState::NotApplicable) assert(text == "\u2014");
            if (expected[column] == GLMTableDisplayState::Value) assert(!text.empty());
        }
    };
    const auto V = GLMTableDisplayState::Value;
    const auto B = GLMTableDisplayState::Blank;
    const auto N = GLMTableDisplayState::NotApplicable;
    const auto U = GLMTableDisplayState::Unavailable;
    assertDisplayStates(*interceptEffect, {V,V,V,N,V,V,V,N,N});
    assertDisplayStates(*baselineEffect, {V,V,V,V,V,V,V,V,V});
    assertDisplayStates(*countryParent, {V,V,B,B,B,B,U,N,V});
    assertDisplayStates(*countryReference, {V,B,N,N,N,N,N,N,N});
    assertDisplayStates(*countryContrast, {V,B,V,V,V,V,V,V,N});
    assertDisplayStates(*fittedInteractionParent, {V,V,B,B,B,B,U,N,V});
    assertDisplayStates(*interactionRow, {V,B,V,V,V,V,V,V,N});

    auto countryReport = rlispstat::core::BuildGLMInteractionReport(
        countryModel, &countryData, countryInteraction, countryTerms, countryTypes, countryFit);
    assert(countryReport.ok && !countryReport.sections.empty());
    auto reportSlope = [&](const std::string &level) {
        const auto found = std::find_if(
            countryReport.sections.front().second.begin(),
            countryReport.sections.front().second.end(),
            [&](const LinearFunctionEstimate &row) {
                return row.label.find("country = " + level) != std::string::npos;
            });
        assert(found != countryReport.sections.front().second.end());
        return found->estimate;
    };
    assert(closeEnough(reportSlope("Finland"), 0.3547, 1e-9));
    assert(closeEnough(reportSlope("Greece"), 0.1536, 1e-9));

    PlotModel countryPlot;
    std::string countryPlotMessage;
    assert(rlispstat::core::PopulateGLMInteractionPlotModel(
        countryPlot, countryModel, &countryData, responseName, countryInteraction,
        countryTerms, countryTypes, countryFit, &countryPlotMessage));
    auto plottedSlope = [&](const std::string &level) {
        const auto found = std::find_if(
            countryPlot.interactionPlotLines.begin(), countryPlot.interactionPlotLines.end(),
            [&](const auto &line) { return line.label == "country = " + level; });
        assert(found != countryPlot.interactionPlotLines.end());
        assert(found->points.size() > 1);
        return (found->points.back().y - found->points.front().y) /
            (found->points.back().x - found->points.front().x);
    };
    assert(closeEnough(plottedSlope("Finland"), 0.3547, 1e-9));
    assert(closeEnough(plottedSlope("Greece"), 0.1536, 1e-9));

    DataFrameModel customReferenceData = countryData;
    customReferenceData.columns[1].definedLevels = {"Greece", "Finland"};
    const auto customReferenceFit = FitMultipleLinearModel(
        countryModel, &customReferenceData, {}, responseName,
        countryTerms, countryTypes, "all");
    const auto *customReferenceCoding =
        rlispstat::core::FactorCodingForVariable(customReferenceFit, "country");
    assert(customReferenceCoding != nullptr);
    assert(customReferenceCoding->referenceLevel == "Greece");
    const auto customReferenceReport = rlispstat::core::BuildGLMInteractionReport(
        countryModel, &customReferenceData, countryInteraction,
        countryTerms, countryTypes, customReferenceFit);
    assert(customReferenceReport.ok);
    for (const auto &expected : std::vector<std::pair<std::string, double>>{
             {"Finland", 0.3547}, {"Greece", 0.1536}}) {
        const auto found = std::find_if(
            customReferenceReport.sections.front().second.begin(),
            customReferenceReport.sections.front().second.end(),
            [&](const LinearFunctionEstimate &row) {
                return row.label.find("country = " + expected.first) != std::string::npos;
            });
        assert(found != customReferenceReport.sections.front().second.end());
        assert(closeEnough(found->estimate, expected.second, 1e-9));
    }

    // Changing the reference category is a real reparameterisation of the
    // fitted design, not a display-only relabel.  Coefficients and reference
    // rows change, whereas fitted values and interaction predictions do not.
    const auto explicitGreeceFit = FitMultipleLinearModel(
        countryModel, &countryData, {}, responseName, countryTerms,
        countryTypes, "all", {}, {{"country", "Greece"}});
    assert(explicitGreeceFit.ok);
    const auto *explicitGreeceCoding =
        rlispstat::core::FactorCodingForVariable(explicitGreeceFit, "country");
    assert(explicitGreeceCoding != nullptr);
    assert(explicitGreeceCoding->referenceLevel == "Greece");
    assert((explicitGreeceCoding->levels ==
            std::vector<std::string>{"Greece", "Finland"}));
    const auto explicitReferenceRow = std::find_if(
        explicitGreeceFit.coefficients.begin(), explicitGreeceFit.coefficients.end(),
        [](const GLMCoefficientRow &row) {
            return row.sourceTerm == "country" && row.rowType == "reference";
        });
    assert(explicitReferenceRow != explicitGreeceFit.coefficients.end());
    assert(explicitReferenceRow->factorLevel == "Greece");
    const auto explicitInteractionRow = std::find_if(
        explicitGreeceFit.coefficients.begin(), explicitGreeceFit.coefficients.end(),
        [&](const GLMCoefficientRow &row) {
            return row.sourceTerm == countryInteraction && row.rowType == "coefficient";
        });
    assert(explicitInteractionRow != explicitGreeceFit.coefficients.end());
    assert(explicitInteractionRow->factorLevel == "Finland");
    assert(explicitInteractionRow->referenceLevel == "Greece");
    assert(closeEnough(countryFit.r2, explicitGreeceFit.r2, 1e-12));
    assert(closeEnough(countryFit.adjR2, explicitGreeceFit.adjR2, 1e-12));
    assert(closeEnough(countryFit.sigma, explicitGreeceFit.sigma, 1e-12));
    assert(closeEnough(countryFit.globalF, explicitGreeceFit.globalF, 1e-9));
    assert(closeEnough(countryFit.globalP, explicitGreeceFit.globalP, 1e-9));
    assert(countryFit.diagnostics.size() == explicitGreeceFit.diagnostics.size());
    for (std::size_t index = 0; index < countryFit.diagnostics.size(); ++index) {
        assert(countryFit.diagnostics[index].row == explicitGreeceFit.diagnostics[index].row);
        assert(closeEnough(countryFit.diagnostics[index].fitted,
                           explicitGreeceFit.diagnostics[index].fitted, 1e-9));
        assert(closeEnough(countryFit.diagnostics[index].residual,
                           explicitGreeceFit.diagnostics[index].residual, 1e-9));
    }
    const auto explicitGreeceReport = rlispstat::core::BuildGLMInteractionReport(
        countryModel, &countryData, countryInteraction, countryTerms,
        countryTypes, explicitGreeceFit);
    assert(explicitGreeceReport.ok);
    for (const auto &expected : std::vector<std::pair<std::string, double>>{
             {"Finland", 0.3547}, {"Greece", 0.1536}}) {
        const auto found = std::find_if(
            explicitGreeceReport.sections.front().second.begin(),
            explicitGreeceReport.sections.front().second.end(),
            [&](const LinearFunctionEstimate &row) {
                return row.label.find("country = " + expected.first) != std::string::npos;
            });
        assert(found != explicitGreeceReport.sections.front().second.end());
        assert(closeEnough(found->estimate, expected.second, 1e-9));
    }
    assert(LinearGLMFitSignature("y", {"x", "country"}, countryTypes, "all") !=
           LinearGLMFitSignature("y", {"x", "country"}, countryTypes, "all", {},
                                 {{"country", "Greece"}}));

    // The same canonical mapping must scale beyond a binary factor.
    PlotModel multiModel;
    multiModel.group = "multi_level";
    multiModel.yLabel = "y";
    std::vector<double> multiX, multiY;
    std::vector<std::string> multiGroups;
    for (const auto &entry : std::vector<std::pair<std::string, double>>{
             {"C", 0.6}, {"A", 0.4}, {"B", 0.3}}) {
        for (int value = 0; value < 4; ++value) {
            multiX.push_back(value);
            multiY.push_back(1.0 + (entry.first == "B" ? 0.5 : 0.0) + entry.second * value);
            multiGroups.push_back(entry.first);
        }
    }
    multiModel.variables = {{"y", multiY}, {"x", multiX}};
    multiModel.variableMeta = {{"y", "numeric"}, {"x", "numeric"}, {"segment", "factor"}};
    DataFrameModel multiData;
    multiData.group = multiModel.group;
    multiData.rows = static_cast<int>(multiX.size());
    multiData.columns = {{"segment", "factor", "", "", -1, multiGroups}};
    const std::vector<std::string> multiTerms = {"x", "segment", "x:segment"};
    const std::map<std::string, std::string> multiTypes = {{"segment", "factor"}};
    const auto multiFit = FitMultipleLinearModel(
        multiModel, &multiData, {}, "y", multiTerms, multiTypes, "all");
    assert(multiFit.ok);
    const auto *multiCoding = rlispstat::core::FactorCodingForVariable(multiFit, "segment");
    assert(multiCoding != nullptr);
    assert((multiCoding->levels == std::vector<std::string>{"A", "B", "C"}));
    const auto multiReport = rlispstat::core::BuildGLMInteractionReport(
        multiModel, &multiData, "x:segment", multiTerms, multiTypes, multiFit);
    assert(multiReport.ok);
    for (const auto &expected : std::vector<std::pair<std::string, double>>{
             {"A", 0.4}, {"B", 0.3}, {"C", 0.6}}) {
        const auto found = std::find_if(
            multiReport.sections.front().second.begin(), multiReport.sections.front().second.end(),
            [&](const LinearFunctionEstimate &row) {
                return row.label.find("segment = " + expected.first) != std::string::npos;
            });
        assert(found != multiReport.sections.front().second.end());
        assert(closeEnough(found->estimate, expected.second, 1e-9));
    }

    // Regression case for the interaction report shown in the desktop UI.
    // The fitted-model representation must drive both the semantically
    // separated report and the plotted adjusted predictions for happy*planet.
    PlotModel happyPlanetModel;
    happyPlanetModel.group = "happy_planet";
    std::vector<double> happyPlanetOutcome;
    std::vector<std::string> happyValues;
    std::vector<std::string> planetValues;
    const std::map<std::string, double> planetOffsets = {
        {"Aurelia", 0.0}, {"Borealis", 1.5}, {"Cygnus", -0.75}
    };
    for (const auto &planet : planetOffsets) {
        for (const std::string &happy : {std::string("no"), std::string("yes")}) {
            for (int replicate = 0; replicate < 3; ++replicate) {
                const double happyEffect = happy == "yes" ? 5.0 : 0.0;
                const double interactionEffect = happy == "yes"
                    ? 0.4 * planet.second : 0.0;
                happyPlanetOutcome.push_back(
                    50.0 + planet.second + happyEffect + interactionEffect +
                    0.1 * static_cast<double>(replicate - 1));
                happyValues.push_back(happy);
                planetValues.push_back(planet.first);
            }
        }
    }
    happyPlanetModel.variables = {{"outcome", happyPlanetOutcome}};
    happyPlanetModel.variableMeta = {
        {"outcome", "numeric"}, {"happy", "factor"}, {"planet", "factor"}
    };
    DataFrameModel happyPlanetData;
    happyPlanetData.group = happyPlanetModel.group;
    happyPlanetData.rows = static_cast<int>(happyPlanetOutcome.size());
    happyPlanetData.columns = {
        {"happy", "factor", "", "", -1, happyValues},
        {"planet", "factor", "", "", -1, planetValues}
    };
    const std::vector<std::string> happyPlanetTerms = {
        "happy", "planet", "happy:planet"
    };
    const std::map<std::string, std::string> happyPlanetTypes = {
        {"happy", "factor"}, {"planet", "factor"}
    };
    const auto happyPlanetFit = FitMultipleLinearModel(
        happyPlanetModel, &happyPlanetData, {}, "outcome",
        happyPlanetTerms, happyPlanetTypes, "all");
    assert(happyPlanetFit.ok);
    const auto happyPlanetReport = rlispstat::core::BuildGLMInteractionReport(
        happyPlanetModel, &happyPlanetData, "happy:planet",
        happyPlanetTerms, happyPlanetTypes, happyPlanetFit);
    assert(happyPlanetReport.ok);
    assert(happyPlanetReport.kind == "categorical by categorical");
    assert(happyPlanetReport.sections.size() == 3);
    assert(happyPlanetReport.sections[0].first == "Estimated marginal means");
    assert(happyPlanetReport.sections[1].first ==
           "Pairwise comparisons of happy within planet (Holm)");
    assert(happyPlanetReport.sections[2].first ==
           "Pairwise comparisons of planet within happy (Holm)");
    assert(happyPlanetReport.sections[0].second.size() == 6);
    assert(happyPlanetReport.sections[1].second.size() == 3);
    assert(happyPlanetReport.sections[2].second.size() == 6);
    assert(happyPlanetReport.sections[0].inference ==
           rlispstat::core::GLMInteractionSectionInference::EstimateOnly);
    assert(happyPlanetReport.sections[1].inference ==
           rlispstat::core::GLMInteractionSectionInference::AdjustedComparison);
    assert(happyPlanetReport.sections[2].inference ==
           rlispstat::core::GLMInteractionSectionInference::AdjustedComparison);
    assert((happyPlanetReport.sections[0].identityHeaders ==
            std::vector<std::string>{"happy", "planet"}));
    assert((happyPlanetReport.sections[1].identityHeaders ==
            std::vector<std::string>{"planet", "Comparison"}));
    assert((happyPlanetReport.sections[2].identityHeaders ==
            std::vector<std::string>{"happy", "Comparison"}));
    assert(happyPlanetReport.sections[0].identityRows.size() == 6);
    assert((happyPlanetReport.sections[0].identityRows.front() ==
            std::vector<std::string>{"no", "Aurelia"}));
    assert(happyPlanetReport.sections[1].identityRows.size() == 3);
    assert((happyPlanetReport.sections[1].identityRows.front() ==
            std::vector<std::string>{"Aurelia", "no - yes"}));
    assert(happyPlanetReport.interpretation.find(
        "Estimated marginal means for happy at each category of planet") !=
        std::string::npos);
    std::set<std::string> reportLabels;
    for (const auto &section : happyPlanetReport.sections) {
        for (const auto &row : section.second) {
            assert(!row.label.empty());
            assert(row.label.find("...") == std::string::npos);
            assert(reportLabels.insert(section.first + "\n" + row.label).second);
        }
    }
    const std::string happyPlanetText =
        rlispstat::core::GLMInteractionReportText(happyPlanetReport);
    assert(happyPlanetText.find("Interaction term: happy:planet") != std::string::npos);
    assert(happyPlanetText.find("Estimated marginal means\n") != std::string::npos);
    assert(happyPlanetText.find(
        "Pairwise comparisons of happy within planet (Holm)\n") !=
        std::string::npos);
    assert(happyPlanetText.find(
        "Pairwise comparisons of planet within happy (Holm)\n") !=
        std::string::npos);
    assert(happyPlanetText.find("no\tAurelia") != std::string::npos);
    assert(happyPlanetText.find(
        "Estimated marginal means\nhappy\tplanet\tEstimate\tSE\tLower 95%\tUpper 95%") !=
        std::string::npos);
    assert(happyPlanetText.find(
        "Pairwise comparisons of happy within planet (Holm)\n"
        "planet\tComparison\tEstimate\tSE\tStatistic\tAdjusted p\tLower 95%\tUpper 95%") !=
        std::string::npos);
    assert(happyPlanetText.find("EMM reference test: None") != std::string::npos);
    assert(happyPlanetText.find(
        "the interaction itself is tested by its interaction term") !=
        std::string::npos);

    const auto happyPlanetReferenceReport =
        rlispstat::core::BuildGLMInteractionReport(
            happyPlanetModel, &happyPlanetData, "happy:planet",
            happyPlanetTerms, happyPlanetTypes, happyPlanetFit,
            0.95, {}, 10.0);
    assert(happyPlanetReferenceReport.ok);
    assert(happyPlanetReferenceReport.emmReferenceTestEnabled);
    assert(closeEnough(happyPlanetReferenceReport.emmTestReference, 10.0));
    assert(happyPlanetReferenceReport.sections[0].inference ==
           rlispstat::core::GLMInteractionSectionInference::ReferenceTest);
    assert(happyPlanetReferenceReport.sections[0].second.size() ==
           happyPlanetReport.sections[0].second.size());
    for (std::size_t row = 0;
         row < happyPlanetReferenceReport.sections[0].second.size(); ++row) {
        const auto &withoutTest = happyPlanetReport.sections[0].second[row];
        const auto &withTest = happyPlanetReferenceReport.sections[0].second[row];
        assert(closeEnough(withTest.estimate, withoutTest.estimate, 1e-12));
        assert(closeEnough(withTest.stdError, withoutTest.stdError, 1e-12));
        assert(closeEnough(withTest.ciLower, withoutTest.ciLower, 1e-12));
        assert(closeEnough(withTest.ciUpper, withoutTest.ciUpper, 1e-12));
        assert(closeEnough(withTest.statistic,
            (withTest.estimate - 10.0) / withTest.stdError, 1e-12));
        assert(std::isfinite(withTest.pValue));
    }
    const std::string happyPlanetReferenceText =
        rlispstat::core::GLMInteractionReportText(happyPlanetReferenceReport);
    assert(happyPlanetReferenceText.find("EMM reference test: 10.0000") !=
           std::string::npos);
    assert(happyPlanetReferenceText.find(
        "Estimated marginal means\nhappy\tplanet\tEstimate\tSE\tStatistic\tp\tLower 95%\tUpper 95%") !=
        std::string::npos);
    assert(happyPlanetReferenceText.find(
        "Estimated marginal means\nhappy\tplanet\tEstimate\tSE\tStatistic\tAdjusted p") ==
        std::string::npos);

    std::vector<std::string> happyPlanetReferenceLines;
    {
        std::istringstream input(happyPlanetReferenceText);
        for (std::string line; std::getline(input, line);)
            happyPlanetReferenceLines.push_back(std::move(line));
    }
    rlispstat::core::GLMInteractionReport parsedReferenceReport;
    assert(rlispstat::core::ParseGLMInteractionReportText(
        happyPlanetReferenceLines, parsedReferenceReport));
    assert(parsedReferenceReport.emmReferenceTestEnabled);
    assert(closeEnough(parsedReferenceReport.emmTestReference, 10.0));
    assert(parsedReferenceReport.multipleComparisons ==
           "Holm adjustment where applicable");
    assert(parsedReferenceReport.sections.size() == 3);
    assert(parsedReferenceReport.sections[0].inference ==
           rlispstat::core::GLMInteractionSectionInference::ReferenceTest);
    assert(parsedReferenceReport.sections[1].inference ==
           rlispstat::core::GLMInteractionSectionInference::AdjustedComparison);
    assert((parsedReferenceReport.sections[0].identityHeaders ==
            std::vector<std::string>{"happy", "planet"}));
    assert((parsedReferenceReport.sections[1].identityHeaders ==
            std::vector<std::string>{"planet", "Comparison"}));
    assert((parsedReferenceReport.sections[0].identityRows.front() ==
            std::vector<std::string>{"no", "Aurelia"}));
    assert(parsedReferenceReport.interpretation ==
           happyPlanetReferenceReport.interpretation);

    PlotModel happyPlanetPlot;
    std::string happyPlanetPlotMessage;
    assert(rlispstat::core::PopulateGLMInteractionPlotModel(
        happyPlanetPlot, happyPlanetModel, &happyPlanetData, "outcome",
        "happy:planet", happyPlanetTerms, happyPlanetTypes,
        happyPlanetFit, &happyPlanetPlotMessage));
    assert(happyPlanetPlot.kind == "glm_interaction");
    assert(happyPlanetPlot.xLabel == "happy");
    assert(happyPlanetPlot.interactionXTicks.size() == 2);
    assert(happyPlanetPlot.interactionPlotLines.size() == 3);
    for (const auto &line : happyPlanetPlot.interactionPlotLines) {
        assert(line.label.rfind("planet = ", 0) == 0);
        assert(!line.colorKey.empty());
        assert(line.points.size() == 2);
        assert(line.caseIds.size() == 6);
        for (const auto &point : line.points) assert(std::isfinite(point.y));
    }
    const auto aureliaLine = std::find_if(
        happyPlanetPlot.interactionPlotLines.begin(),
        happyPlanetPlot.interactionPlotLines.end(),
        [](const rlispstat::core::InteractionPlotLine &line) {
            return line.label == "planet = Aurelia";
        });
    assert(aureliaLine != happyPlanetPlot.interactionPlotLines.end());
    const auto aureliaPrediction = std::find_if(
        happyPlanetReport.sections[0].second.begin(),
        happyPlanetReport.sections[0].second.end(),
        [](const LinearFunctionEstimate &row) {
            return row.label == "happy = no, planet = Aurelia";
        });
    assert(aureliaPrediction != happyPlanetReport.sections[0].second.end());
    assert(closeEnough(aureliaLine->points.front().y,
                       aureliaPrediction->estimate, 1e-9));

    // Centering changes the intercept but not the fitted values or slope.  It is
    // part of the fit signature so cached ordinary and pooled fits cannot be
    // reused with the wrong model specification.
    PlotModel centeringModel;
    centeringModel.variables = {
        {"y", {3.1, 4.8, 7.1}},
        {"x", {1.0, 2.0, 3.0}}
    };
    centeringModel.variableMeta = {{"y", "numeric"}, {"x", "numeric"}};
    const auto uncenteredFit = FitMultipleLinearModel(
        centeringModel, nullptr, {}, "y", {"x"}, {}, "all");
    const auto centeredFit = FitMultipleLinearModel(
        centeringModel, nullptr, {}, "y", {"x"}, {}, "all", {"x"});
    rlispstat::core::ModelSpecification centeredSpecification;
    centeredSpecification.response = "y";
    centeredSpecification.terms = {"x"};
    centeredSpecification.termTypes = {{"x", "numeric"}};
    centeredSpecification.centeredPredictors = {"x"};
    const auto centeredSpecificationFit = FitMultipleLinearModel(
        centeringModel, nullptr, {}, centeredSpecification);
    assert(uncenteredFit.ok && centeredFit.ok);
    assert(centeredSpecificationFit.ok);
    const auto coefficientFor = [](const GLMFitSummary &fit, const std::string &term) {
        return std::find_if(fit.coefficients.begin(), fit.coefficients.end(),
            [&](const GLMCoefficientRow &row) { return row.term == term; });
    };
    const auto rawIntercept = coefficientFor(uncenteredFit, "(Intercept)");
    const auto centeredIntercept = coefficientFor(centeredFit, "(Intercept)");
    const auto rawSlope = coefficientFor(uncenteredFit, "x");
    const auto centeredSlope = coefficientFor(centeredFit, "x");
    assert(rawIntercept != uncenteredFit.coefficients.end());
    assert(centeredIntercept != centeredFit.coefficients.end());
    assert(rawSlope != uncenteredFit.coefficients.end());
    assert(centeredSlope != centeredFit.coefficients.end());
    assert(closeEnough(rawIntercept->estimate, 1.0, 1e-9));
    assert(closeEnough(centeredIntercept->estimate, 5.0, 1e-9));
    assert(closeEnough(rawSlope->estimate, 2.0, 1e-9));
    assert(closeEnough(centeredSlope->estimate, 2.0, 1e-9));
    assert(centeredFit.predictorCenters.size() == 1);
    assert(closeEnough(centeredFit.predictorCenters.at("x"), 2.0, 1e-12));
    assert(closeEnough(centeredSpecificationFit.predictorCenters.at("x"), 2.0, 1e-12));
    assert(rlispstat::core::GLMRegressionCoefficientDisplayLabel(
        *centeredSlope, {"x"}) == "x (centered)");
    assert(closeEnough(uncenteredFit.r2, centeredFit.r2, 1e-12));
    assert(closeEnough(uncenteredFit.adjR2, centeredFit.adjR2, 1e-12));
    assert(closeEnough(uncenteredFit.globalF, centeredFit.globalF, 1e-12));
    assert(closeEnough(uncenteredFit.globalP, centeredFit.globalP, 1e-12));
    assert(uncenteredFit.diagnostics.size() == centeredFit.diagnostics.size());
    for (std::size_t index = 0; index < uncenteredFit.diagnostics.size(); ++index) {
        assert(closeEnough(uncenteredFit.diagnostics[index].fitted,
                           centeredFit.diagnostics[index].fitted, 1e-9));
        assert(closeEnough(uncenteredFit.diagnostics[index].residual,
                           centeredFit.diagnostics[index].residual, 1e-9));
    }
    const auto restoredFit = FitMultipleLinearModel(
        centeringModel, nullptr, {}, "y", {"x"}, {}, "all");
    assert(restoredFit.ok);
    assert(restoredFit.predictorCenters.empty());
    assert(closeEnough(coefficientFor(restoredFit, "(Intercept)")->estimate,
                       rawIntercept->estimate, 1e-12));
    assert(closeEnough(coefficientFor(restoredFit, "x")->estimate,
                       rawSlope->estimate, 1e-12));
    assert(LinearGLMFitSignature("y", {"x"}, {}, "all") !=
           LinearGLMFitSignature("y", {"x"}, {}, "all", {"x"}));

    // The centering constant comes from the exact complete cases entering the
    // model, not from the whole source column.
    PlotModel completeCaseCenteringModel;
    completeCaseCenteringModel.variables = {
        {"y", {2.0, 5.0, 203.0, 10.0, 14.0}},
        {"x", {1.0, 2.0, 100.0, 4.0, 5.0}},
        {"z", {0.0, 1.0, NAN, 2.0, 4.0}}
    };
    completeCaseCenteringModel.variableMeta = {
        {"y", "numeric"}, {"x", "numeric"}, {"z", "numeric"}
    };
    const auto completeCaseCenteredFit = FitMultipleLinearModel(
        completeCaseCenteringModel, nullptr, {}, "y", {"x", "z"}, {},
        "all", {"x"});
    assert(completeCaseCenteredFit.ok);
    assert(closeEnough(completeCaseCenteredFit.predictorCenters.at("x"), 3.0, 1e-12));
    assert((completeCaseCenteredFit.rowsUsed == std::vector<int>{1, 2, 4, 5}));

    // A centered numeric-by-factor interaction consistently uses x - mean(x)
    // in both the lower-order term and every interaction design column.
    PlotModel centeredInteractionModel;
    centeredInteractionModel.group = "centered_interaction";
    centeredInteractionModel.variables = {
        {"y", {12.0, 14.0, 16.0, 18.0, 23.0, 26.0, 29.0, 32.0}},
        {"x", {1.0, 2.0, 3.0, 4.0, 1.0, 2.0, 3.0, 4.0}}
    };
    centeredInteractionModel.variableMeta = {
        {"y", "numeric"}, {"x", "numeric"}, {"group", "factor"}
    };
    DataFrameModel centeredInteractionData;
    centeredInteractionData.group = centeredInteractionModel.group;
    centeredInteractionData.rows = 8;
    centeredInteractionData.columns = {
        {"group", "factor", "", "", -1,
         {"A", "A", "A", "A", "B", "B", "B", "B"}}
    };
    const std::vector<std::string> centeredInteractionTerms = {
        "x", "group", "x:group"
    };
    const std::map<std::string, std::string> centeredInteractionTypes = {
        {"group", "factor"}
    };
    const auto rawInteractionFit = FitMultipleLinearModel(
        centeredInteractionModel, &centeredInteractionData, {}, "y",
        centeredInteractionTerms, centeredInteractionTypes, "all");
    const auto centeredInteractionFit = FitMultipleLinearModel(
        centeredInteractionModel, &centeredInteractionData, {}, "y",
        centeredInteractionTerms, centeredInteractionTypes, "all", {"x"});
    assert(rawInteractionFit.ok && centeredInteractionFit.ok);
    assert(closeEnough(centeredInteractionFit.predictorCenters.at("x"), 2.5, 1e-12));
    assert(rawInteractionFit.diagnostics.size() == centeredInteractionFit.diagnostics.size());
    for (std::size_t index = 0; index < rawInteractionFit.diagnostics.size(); ++index) {
        assert(closeEnough(rawInteractionFit.diagnostics[index].fitted,
                           centeredInteractionFit.diagnostics[index].fitted, 1e-9));
        assert(closeEnough(rawInteractionFit.diagnostics[index].residual,
                           centeredInteractionFit.diagnostics[index].residual, 1e-9));
    }
    const auto rawGroupB = std::find_if(
        rawInteractionFit.coefficients.begin(), rawInteractionFit.coefficients.end(),
        [](const GLMCoefficientRow &row) {
            return row.sourceTerm == "group" && row.factorLevel == "B";
        });
    const auto centeredGroupB = std::find_if(
        centeredInteractionFit.coefficients.begin(), centeredInteractionFit.coefficients.end(),
        [](const GLMCoefficientRow &row) {
            return row.sourceTerm == "group" && row.factorLevel == "B";
        });
    const auto rawInteractionB = std::find_if(
        rawInteractionFit.coefficients.begin(), rawInteractionFit.coefficients.end(),
        [](const GLMCoefficientRow &row) {
            return row.sourceTerm == "x:group" && row.factorLevel == "B" &&
                row.rowType == "coefficient";
        });
    const auto centeredInteractionB = std::find_if(
        centeredInteractionFit.coefficients.begin(), centeredInteractionFit.coefficients.end(),
        [](const GLMCoefficientRow &row) {
            return row.sourceTerm == "x:group" && row.factorLevel == "B" &&
                row.rowType == "coefficient";
        });
    assert(rawGroupB != rawInteractionFit.coefficients.end());
    assert(centeredGroupB != centeredInteractionFit.coefficients.end());
    assert(rawInteractionB != rawInteractionFit.coefficients.end());
    assert(centeredInteractionB != centeredInteractionFit.coefficients.end());
    assert(closeEnough(rawGroupB->estimate, 10.0, 1e-9));
    assert(closeEnough(centeredGroupB->estimate, 12.5, 1e-9));
    assert(closeEnough(rawInteractionB->estimate, 1.0, 1e-9));
    assert(closeEnough(centeredInteractionB->estimate, 1.0, 1e-9));
    assert(rlispstat::core::GLMRegressionCoefficientDisplayLabel(
        *centeredInteractionB, {"x"}).find("x (centered)") != std::string::npos);
    const auto centeredInteractionReport = rlispstat::core::BuildGLMInteractionReport(
        centeredInteractionModel, &centeredInteractionData, "x:group",
        centeredInteractionTerms, centeredInteractionTypes,
        centeredInteractionFit, 0.95, {"x"});
    assert(centeredInteractionReport.ok);

    // Pairwise requests are limited to included simple categorical main effects;
    // R performs Games-Howell for a one-factor model and heteroscedastic EMMs
    // for an adjusted model.
    PlotModel pairwiseModel;
    pairwiseModel.variables = {
        {"outcome", {10.0, 11.0, 9.0, 13.0, 12.0, 14.0, 16.0, 17.0, 15.0}},
        {"treatment", {1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 3.0, 3.0, 3.0}},
        {"age", {20.0, 21.0, 22.0, 20.0, 21.0, 22.0, 20.0, 21.0, 22.0}}
    };
    pairwiseModel.variableMeta = {{"outcome", "numeric"}, {"treatment", "factor"}, {"age", "numeric"}};
    const std::vector<std::string> pairwiseTerms = {"treatment", "age"};
    const std::map<std::string, std::string> pairwiseTypes = {{"treatment", "factor"}};
    GLMFitSummary pairwiseFit = FitMultipleLinearModel(
        pairwiseModel, nullptr, {}, "outcome", pairwiseTerms, pairwiseTypes, "all");
    assert(pairwiseFit.ok);
    GLMPairwiseComparisonRequest pairwiseRequest;
    pairwiseRequest.term = "treatment";
    auto pairwiseEligibility = EvaluateGLMPairwiseComparisonEligibility(
        pairwiseModel, nullptr, "outcome", pairwiseTerms, pairwiseTypes, pairwiseFit, pairwiseRequest);
    assert(pairwiseEligibility.eligible);
    assert((pairwiseEligibility.levels == std::vector<std::string>{"1", "2", "3"}));
    assert(pairwiseRequest.adjustment == "tukey");
    rlispstat::core::GLMPairwiseComparisonResult pairwiseResult;
    pairwiseResult.term = "treatment";
    assert(ParseGLMPairwiseComparisonsRResult({
        "OK",
        "METHOD\tR: nlme::gls with varIdent by selected factor; emmeans pairwise EMMs\tTukey (emmeans)",
        "ROW\t1 - 2\t1\t2\t-3\t0.8\t5.5\t-3.75\t0.02\t-5\t-1",
        "ROW\t1 - 3\t1\t3\t-6\t1.1\t6.2\t-5.45\t0.001\t-9\t-3",
        "ROW\t2 - 3\t2\t3\t-3\t0.9\t5.8\t-3.33\t0.03\t-5.5\t-0.5"
    }, pairwiseResult));
    assert(pairwiseResult.ok);
    assert(pairwiseResult.adjustment == "Tukey (emmeans)");
    assert(pairwiseResult.comparisons.size() == 3); // k(k - 1) / 2 for k = 3
    assert(pairwiseResult.comparisons[0].label == "1 - 2");
    assert(pairwiseResult.comparisons[1].label == "1 - 3");
    assert(pairwiseResult.comparisons[2].label == "2 - 3");
    assert(pairwiseResult.comparisons[0].firstLevel == "1");
    assert(pairwiseResult.comparisons[0].secondLevel == "2");
    assert(pairwiseResult.comparisons[0].comparisonId == "treatment\x1f" "1\x1f" "2");
    assert(std::isfinite(pairwiseResult.comparisons[0].adjustedPValue));
    assert(std::isfinite(pairwiseResult.comparisons[0].degreesOfFreedom));
    assert(GLMPairwiseComparisonsText(pairwiseResult).find("p (Tukey (emmeans))") != std::string::npos);
    assert(GLMPairwiseComparisonsRScript().find("rstatix::games_howell_test") != std::string::npos);
    assert(GLMPairwiseComparisonsRScript().find("nlme::gls") != std::string::npos);
    // The embedded R regular expression must contain real R escape sequences,
    // not doubled backslashes.  The latter are interpreted as a character
    // class containing t/r/n and corrupt labels (Europe -> Eu ope, etc.).
    assert(GLMPairwiseComparisonsRScript().find("gsub(\"[\\t\\r\\n]\"") != std::string::npos);
    assert(GLMPairwiseComparisonsRScript().find("gsub(\"[\\\\t\\\\r\\\\n]\"") == std::string::npos);

    GLMPairwiseComparisonRequest continuousRequest;
    continuousRequest.term = "age";
    assert(!EvaluateGLMPairwiseComparisonEligibility(
        pairwiseModel, nullptr, "outcome", pairwiseTerms, pairwiseTypes, pairwiseFit, continuousRequest).eligible);
    GLMPairwiseComparisonRequest interactionRequest;
    interactionRequest.term = "treatment:age";
    assert(!EvaluateGLMPairwiseComparisonEligibility(
        pairwiseModel, nullptr, "outcome", {"treatment:age"}, pairwiseTypes, pairwiseFit, interactionRequest).eligible);
    GLMPairwiseComparisonRequest interceptRequest;
    interceptRequest.term = "(Intercept)";
    assert(!EvaluateGLMPairwiseComparisonEligibility(
        pairwiseModel, nullptr, "outcome", pairwiseTerms, pairwiseTypes, pairwiseFit, interceptRequest).eligible);
    GLMPairwiseComparisonRequest transformedRequest;
    transformedRequest.term = "factor(treatment)";
    assert(!EvaluateGLMPairwiseComparisonEligibility(
        pairwiseModel, nullptr, "outcome", {"factor(treatment)"}, pairwiseTypes, pairwiseFit, transformedRequest).eligible);
    GLMPairwiseComparisonRequest missingRequest;
    missingRequest.term = "removed";
    assert(!EvaluateGLMPairwiseComparisonEligibility(
        pairwiseModel, nullptr, "outcome", {"removed"}, pairwiseTypes, pairwiseFit, missingRequest).eligible);
    GLMFitSummary failedPairwiseFit;
    failedPairwiseFit.warning = "singular fit";
    assert(!EvaluateGLMPairwiseComparisonEligibility(pairwiseModel, nullptr, "outcome", pairwiseTerms,
                                                      pairwiseTypes, failedPairwiseFit, pairwiseRequest).eligible);
    GLMFitSummary twoLevelRows = pairwiseFit;
    twoLevelRows.rowsUsed = {1, 2, 3, 4, 5, 6};
    assert(EvaluateGLMPairwiseComparisonEligibility(pairwiseModel, nullptr, "outcome", pairwiseTerms,
                                                     pairwiseTypes, twoLevelRows, pairwiseRequest).levels.size() == 2);
    assert(pairwiseTerms == std::vector<std::string>({"treatment", "age"}));
    assert(pairwiseModel.variables[0].name == "outcome");

    assert(GeneralizedGLMFitFailedStatus() == "Could not fit generalized linear model.");
    assert(GLMNoActiveInteractionDataStatus() == "No active model data are available.");
    assert(GLMInteractionModelNotFittedStatus() == "The model could not be fitted.");
    assert(GLMChooseResponseVariableStatus() == "Choose a response variable.");
    assert(GLMInvalidFamilyStatus() == "Invalid GLM family.");
    assert(GLMInvalidLinkForFamilyStatus() == "Invalid link for family.");
    assert(GLMNoRowsForScopeStatus() == "No rows are available for the selected scope.");
    assert(GLMInteractionTwoWayOnlyStatus() == "Effect plots for interactions currently support two-way interactions.");
    assert(GLMInteractionNoFittedValuesStatus() == "The effect plot could not be built because a component has no fitted values.");
    assert(GLMInteractionFactorNoLevelsStatus() == "The effect plot could not be built because a categorical predictor has no fitted categories.");
    assert(GLMInteractionTypeNotAvailableStatus() == "This effect plot type is not available.");
    assert(GLMInteractionNoFiniteValuesStatus() == "The effect plot has no finite fitted values.");
    assert(GLMChooseDependentVariableStatus() == "Choose a dependent variable.");
    assert(GLMDependentVariableNotAvailableStatus() == "Dependent variable is not available.");
    assert(GLMPredictorUnavailableStatus("x") == "Predictor `x` is not available.");
    assert(GLMFactorPredictorUnavailableStatus("group") == "Categorical predictor `group` is not available.");
    assert(GLMNumericPredictorUnavailableStatus("age") == "Numeric predictor `age` is not available.");
    assert(GLMNoDiagnosticRowsStatus() == "The current GLM has no diagnostic rows. Refit the model with at least one valid predictor.");
    assert(GLMNoActiveDiagnosticStatus() == "No active GLM model/plot is available.");
    assert(GLMCurrentLinearDiagnosticUnavailableStatus("qq") ==
           "Diagnostic `qq` is not available for the current linear model.");
    assert(GLMSelectedLinearDiagnosticUnavailableStatus("qq") ==
           "Diagnostic `qq` is not available for the selected comparison model.");
    assert(GLMDiagnosticStateSummaryText("cars", "mpg", {"wt", "hp"}, 2, 3, 4, true, true, 30, 2) ==
           "model_id=glm:cars\n"
           "dependent=mpg\n"
           "predictors=wt, hp\n"
           "valid_predictor_count=2\n"
           "model_version=3\n"
           "fit_version=4\n"
           "is_stale=TRUE\n"
           "has_data_seed=TRUE\n"
           "n_used_rows=30\n"
           "n_excluded_rows=2");
    assert(GLMDiagnosticStateSummaryText("cars", "", {}, 0, 0, 0, false, false, 0, 0).find("predictors=(none)") != std::string::npos);
    assert(GLMCurrentDiagnosticFitFailedStatus("valid_predictor_count=0") ==
           "The current GLM could not be fitted for diagnostics.\n\nvalid_predictor_count=0");
    assert(GLMSelectedDiagnosticFitFailedStatus("singular fit") ==
           "The selected model could not be fitted.\n\nsingular fit");
    assert(GLMCurrentGeneralizedDiagnosticFitFailedStatus("failed") ==
           "The current generalized linear model could not be fitted.\n\nfailed");
    assert(GLMSelectedGeneralizedDiagnosticFitFailedStatus("failed") ==
           "The selected generalized model could not be fitted.\n\nfailed");
    assert(GLMReadyStatus() == "Status: ready");
    assert(GLMNoInteractionTermStatus() == "No interaction term is selected.");
    assert(GLMRegressionComparisonNotAvailableStatus() == "The General Linear Model comparison is not available.");
    assert(GLMChooseValidModelColumnStatus() == "Choose a valid model column.");
    assert(GLMGeneralizedLinearModelNotAvailableStatus() == "The generalized linear model is not available.");
    assert(GLMGeneralizedModelComparisonNotAvailableStatus() == "The generalized model comparison is not available.");
    assert(GLMNoNumericResponseVariablesStatus() == "The active dataset has no numeric response variables.");
    assert(GLMImputedDatasetNotAvailableStatus() == "The active imputed dataset is no longer available.");
    assert(GLMAddPredictorBeforeTableStatus() == "Add at least one predictor before opening the pooled MI Generalized Linear Model table.");
    assert(GLMComparisonRequiresPooledTestStatus() == "Multiple-imputation generalized model comparisons require MI-aware pooled tests from the R analysis layer. Open a pooled generalized comparison from R so this native window receives pooled model-comparison results.");
    assert(GLMNoInteractionReportTextStatus() == "No interaction report text was produced for this term.");
    assert(GLMWindowTitle() == "Generalized Linear Model");
    assert(GLMGeneralizedModelComparisonTitle() == "Generalized Model Comparison");
    assert(GLMGeneralLinearModelTitle() == "Linear Model");
    assert(GLMAutoRefitButtonTitle() == "Auto-refit");
    assert(GLMDiagnosticResidualHistogramTitle() == "Residual histogram");
    assert(GLMDiagnosticResidualsFittedTitle() == "Residuals vs fitted");
    assert(GLMDiagnosticObservedFittedTitle() == "Observed vs fitted");
    assert(GLMDiagnosticNormalQQTitle() == "Normal Q-Q");
    assert(GLMDevianceResidualTitle() == "Deviance");
    assert(GLMPearsonResidualTitle() == "Pearson");
    assert(GLMWorkingResidualTitle() == "Working");
    assert(GLMAddTermTitle() == "Add independent variable");
    assert(GLMChangeTermTitle() == "Change term");
    assert(GLMInteractionPlotWindowTitle() == "Effect Plot");
    assert(GLMDiagnosticPlotWindowTitle() == "Diagnostic Plot");
    assert(GLMRenameModelTitle() == "Rename model");
    assert(GLMDuplicateModelTitle() == "Duplicate model");
    assert(GLMDeleteModelTitle() == "Delete model");
    assert(GLMComparisonFamilyMenuTitle() == "Distribution");
    assert(GLMComparisonLinkMenuTitle() == "Link");
    assert(GLMComparisonFamilyMenuItemTitle() == "Distribution...");
    assert(GLMComparisonLinkMenuItemTitle() == "Link...");
    assert(GLMModelFitSectionTitle() == "Model fit");
    assert(GLMTermsSectionTitle() == "Terms");
    assert(GLMCoefVariableHeader() == "Variable");
    assert(GLMCoefTypeHeader() == "Type");
    assert(GLMCoefBHeader() == "b");
    assert(GLMCoefSEHeader() == "SE");
    assert(GLMCoefStatisticHeader() == "z");
    assert(GLMCoefPHeader() == "p");
    assert(GLMAnovaSumSquaresHeader() == "Sum of Squares");
    assert(GLMAnovaDfHeader() == "df");
    assert(GLMAnovaMeanSquareHeader() == "Mean Square");
    assert(GLMAnovaFRatioHeader() == "F-ratio");
    assert(GLMAnovaPHeader() == "p");
    assert(GLMAnovaResidualLabel() == "Residual");
    assert(GLMFitNLabel() == "N");
    assert(GLMFitNullDevianceLabel() == "Null deviance");
    assert(GLMFitResidualDevianceLabel() == "Residual dev.");
    assert(GLMFitDfResidualLabel() == "df residual");
    assert(GLMFitAICLabel() == "AIC");
    assert(GLMFitBICLabel() == "BIC");
    assert(GLMFitDispersionLabel() == "Dispersion");
    assert(GLMFitLogLikLabel() == "logLik");
    assert(GLMRowsUsedExcludedWindowTitle() == "GLM Rows Used/Excluded");
    assert(GLMAddModelLabel() == "+ Add model");
    assert(GLMAddTermLabel() == "+ Add term");
    assert(rlispstat::core::RegressionComparisonAddTermLabel() == "+ Add term");
    assert(GLMPlusSignLabel() == "+");
    assert(GLMSelectedRowsPlaceholder() == "Selected rows: 0 / 0");
    assert(GLMD1WaldLabel() == "D1/Wald");
    assert(GLMAnovaFRatioLabel() == "F-ratio");
    assert(GLMRegressionBetaLabel() == "\u03B2");
    assert(GLMRegressionPartialRLabel() == "Partial r");
    assert(GLMRegressionDeltaRSquaredLabel() == "\u0394R\u00B2");
    assert(GLMInteractionsMenuTitle() == "Interaction");
    assert(GLMGeneralLinearTHeader() == "t");
    assert(GLMGeneralLinearTermsHeader() == "Terms");
    assert(GLMGeneralLinearVariableHeader() == "Variable");
    assert(GLMGeneralLinearTypeHeader() == "Type");
    assert(GLMGeneralLinearBetaHeader() == "\u03B2");
    assert(GLMInterceptTermName() == "(Intercept)");
    assert(GLMDashPlaceholder() == "-");
    assert(GLMCoefBHeader() == "b");
    assert(GLMCoefSEHeader() == "SE");
    assert(GLMCoefPHeader() == "p");
    assert(GLMGeneralLinearTHeader() == "t");
    assert(GLMFitGlobalFitSectionTitle() == "Global fit\n");
    assert(GLMFitRSquaredLabel() == "R\u00B2");
    assert(GLMFitAdjustedRSquaredLabel() == "Adjusted R\u00B2");
    assert(GLMFitSValueLabel() == "s");
    assert(GLMFitDFLabel() == "df");

    // "Explain statistic" is semantic and portable: it distinguishes the
    // actual mice method and does not describe quasi-likelihood results as D3.
    ModelStatisticExplanationContext d1Explanation;
    ModelStatisticExplanationContext miGuide;
    miGuide.analysisKind="mi_missing_information";
    miGuide.statistic="MCSE / SE (%)";
    assert(ExplainModelStatistic(miGuide).find("below 10%")!=std::string::npos);
    assert(ExplainModelStatistic(miGuide).find("not a pass/fail")!=std::string::npos);
    miGuide.statistic="B";
    assert(ExplainModelStatistic(miGuide).find("between imputations")!=std::string::npos);
    miGuide.statistic="df";
    assert(ExplainModelStatistic(miGuide).find("pooled estimate")!=std::string::npos);
    miGuide.statistic="FMI";
    assert(ExplainModelStatistic(miGuide).find("no universal")!=std::string::npos);
    d1Explanation.statistic = "D1/Wald vs prev";
    d1Explanation.modelFamily = "gaussian";
    d1Explanation.comparison = true;
    d1Explanation.multipleImputation = true;
    d1Explanation.rubinRulesApplied = true;
    d1Explanation.miMethod = "mice::D1";
    const std::string d1Text = ExplainModelStatistic(d1Explanation);
    assert(d1Text.find("mice's pooled multivariate Wald test") != std::string::npos);
    assert(d1Text.find("mice::D1/Wald") != std::string::npos);

    ModelStatisticExplanationContext d3Explanation = d1Explanation;
    d3Explanation.statistic = "D3";
    d3Explanation.miMethod = "mice::D3";
    const std::string d3Text = ExplainModelStatistic(d3Explanation);
    assert(d3Text.find("mice::D3") != std::string::npos);
    assert(d3Text.find("not being presented as a D1 result") != std::string::npos);

    ModelStatisticExplanationContext quasiExplanation;
    quasiExplanation.statistic = "logLik";
    quasiExplanation.modelFamily = "quasipoisson";
    quasiExplanation.likelihoodAvailable = false;
    const std::string quasiText = ExplainModelStatistic(quasiExplanation);
    assert(quasiText.find("quasi-likelihood") != std::string::npos);

    ModelStatisticExplanationContext noRubinExplanation;
    noRubinExplanation.statistic = "b";
    noRubinExplanation.multipleImputation = true;
    noRubinExplanation.rubinRulesApplied = false;
    const std::string noRubinText = ExplainModelStatistic(noRubinExplanation);
    assert(noRubinText.find("Rubin's rules are not required") != std::string::npos);

    ModelStatisticExplanationContext logitCoefficient;
    logitCoefficient.statistic = "b";
    logitCoefficient.modelFamily = "binomial";
    logitCoefficient.link = "logit";
    const std::string logitCoefficientText = ExplainModelStatistic(logitCoefficient);
    assert(logitCoefficientText.find("log odds") != std::string::npos);
    assert(logitCoefficientText.find("odds ratio") != std::string::npos);

    ModelStatisticExplanationContext probitCoefficient = logitCoefficient;
    probitCoefficient.link = "probit";
    const std::string probitCoefficientText = ExplainModelStatistic(probitCoefficient);
    assert(probitCoefficientText.find("latent standard-normal index") != std::string::npos);
    assert(probitCoefficientText.find("not a log-odds coefficient") != std::string::npos);

    ModelStatisticExplanationContext abbreviatedDeviance;
    abbreviatedDeviance.statistic = "Residual dev.";
    abbreviatedDeviance.modelFamily = "poisson";
    const std::string abbreviatedDevianceText = ExplainModelStatistic(abbreviatedDeviance);
    assert(abbreviatedDevianceText.find("measures lack of fit") != std::string::npos);

    ModelStatisticExplanationContext deltaAdjusted;
    deltaAdjusted.statistic = "Δ adjusted R² vs prev";
    deltaAdjusted.comparison = true;
    const std::string deltaAdjustedText = ExplainModelStatistic(deltaAdjusted);
    assert(deltaAdjustedText.find("can be negative") != std::string::npos);

    ModelStatisticExplanationContext pooledRSquared;
    pooledRSquared.statistic = "R²";
    pooledRSquared.modelFamily = "gaussian";
    pooledRSquared.multipleImputation = true;
    pooledRSquared.rubinRulesApplied = true;
    pooledRSquared.miMethod = "mice::pool";
    const std::string pooledRSquaredText = ExplainModelStatistic(pooledRSquared);
    assert(pooledRSquaredText.find("mice::pool.r.squared()") != std::string::npos);

    // The same help engine covers non-regression result tables and records
    // the exact method/adjustment instead of returning a model-only fallback.
    ModelStatisticExplanationContext descriptiveMean;
    descriptiveMean.statistic = "Mean";
    descriptiveMean.analysisKind = "descriptives";
    const std::string descriptiveMeanText = ExplainModelStatistic(descriptiveMean);
    assert(descriptiveMeanText.find("arithmetic average") != std::string::npos);

    ModelStatisticExplanationContext contingencyExpected;
    contingencyExpected.statistic = "Expected count";
    contingencyExpected.analysisKind = "contingency";
    const std::string contingencyExpectedText = ExplainModelStatistic(contingencyExpected);
    assert(contingencyExpectedText.find("null model") != std::string::npos);
    assert(contingencyExpectedText.find("Pearson chi-square approximation") != std::string::npos);

    ModelStatisticExplanationContext spearman;
    spearman.statistic = "Spearman rho";
    spearman.analysisKind = "correlation";
    spearman.method = "Spearman rank correlation";
    const std::string spearmanText = ExplainModelStatistic(spearman);
    assert(spearmanText.find("monotonic association") != std::string::npos);
    assert(spearmanText.find("Method note") != std::string::npos);

    ModelStatisticExplanationContext pairwiseAdjusted;
    pairwiseAdjusted.statistic = "Adjusted p";
    pairwiseAdjusted.analysisKind = "pairwise comparisons";
    pairwiseAdjusted.method = "estimated marginal means";
    pairwiseAdjusted.adjustment = "Tukey adjustment";
    const std::string pairwiseAdjustedText = ExplainModelStatistic(pairwiseAdjusted);
    assert(pairwiseAdjustedText.find("multiple-comparison") != std::string::npos);
    assert(pairwiseAdjustedText.find("Tukey adjustment") != std::string::npos);

    ModelStatisticExplanationContext loading;
    loading.statistic = "Loading";
    loading.analysisKind = "dimensionality";
    loading.method = "principal component analysis";
    const std::string loadingText = ExplainModelStatistic(loading);
    assert(loadingText.find("component or factor") != std::string::npos);
    assert(loadingText.find("principal component analysis") != std::string::npos);

    ModelStatisticExplanationContext combinedCount;
    combinedCount.statistic = "Observed count and Percent";
    combinedCount.analysisKind = "table1";
    const std::string combinedCountText = ExplainModelStatistic(combinedCount);
    assert(combinedCountText.find("observed frequency") != std::string::npos);
    assert(combinedCountText.find("denominator") != std::string::npos);

    ModelStatisticExplanationContext genericTest;
    genericTest.statistic = "Test";
    genericTest.analysisKind = "contingency";
    genericTest.method = "Pearson chi-square";
    const std::string genericTestText = ExplainModelStatistic(genericTest);
    assert(genericTestText.find("inferential statistic") != std::string::npos);
    assert(genericTestText.find("Pearson chi-square") != std::string::npos);

    ModelStatisticExplanationContext parallelEigenvalue;
    parallelEigenvalue.statistic = "Parallel eigenvalue";
    parallelEigenvalue.analysisKind = "dimensionality";
    const std::string parallelEigenvalueText = ExplainModelStatistic(parallelEigenvalue);
    assert(parallelEigenvalueText.find("random data") != std::string::npos);

    ModelStatisticExplanationContext pairedEffect;
    pairedEffect.statistic = "d_z";
    pairedEffect.analysisKind = "paired_samples_t_test";
    pairedEffect.method = "Paired t tests";
    const std::string pairedEffectText = ExplainModelStatistic(pairedEffect);
    assert(pairedEffectText.find("paired differences") != std::string::npos);

    ModelStatisticExplanationContext wilcoxonStatistic;
    wilcoxonStatistic.statistic = "V";
    wilcoxonStatistic.analysisKind = "one_sample_t_test";
    wilcoxonStatistic.method = "Wilcoxon signed-rank test";
    const std::string wilcoxonStatisticText = ExplainModelStatistic(wilcoxonStatistic);
    assert(wilcoxonStatisticText.find("computed from ranks") != std::string::npos);

    ModelStatisticExplanationContext pooledCorrelation;
    pooledCorrelation.statistic = "Pearson r";
    pooledCorrelation.analysisKind = "correlation";
    pooledCorrelation.method = "Pearson correlation";
    pooledCorrelation.multipleImputation = true;
    pooledCorrelation.rubinRulesApplied = true;
    pooledCorrelation.miMethod = "Fisher z + mice::pool.scalar";
    const std::string pooledCorrelationText = ExplainModelStatistic(pooledCorrelation);
    assert(pooledCorrelationText.find("Fisher z + mice::pool.scalar") != std::string::npos);
    assert(pooledCorrelationText.find("coefficient estimates") == std::string::npos);

    // GLM result tables keep every column compact at every window width.
    // Spare window width belongs after the completed table, not inside it.
    const auto anovaLayout = rlispstat::core::GLMAnovaResultTableLayout();
    const auto anovaNarrow = rlispstat::core::ResolveGLMResultTableColumnWidths(
        anovaLayout, 420.0);
    const auto anovaNormal = rlispstat::core::ResolveGLMResultTableColumnWidths(
        anovaLayout, 760.0);
    const auto anovaWide = rlispstat::core::ResolveGLMResultTableColumnWidths(
        anovaLayout, 1400.0);
    assert(anovaNarrow == anovaNormal);
    assert(anovaNormal == anovaWide);
    assert(anovaNormal.size() == 6);
    assert(anovaNormal.back() == 64.0);

    const auto termsLayout = rlispstat::core::GLMTermsResultTableLayout();
    const auto termsMinimum = rlispstat::core::ResolveGLMResultTableColumnWidths(
        termsLayout, 400.0);
    const auto termsNormal = rlispstat::core::ResolveGLMResultTableColumnWidths(
        termsLayout, 860.0);
    const auto termsWide = rlispstat::core::ResolveGLMResultTableColumnWidths(
        termsLayout, 1400.0);
    assert(termsMinimum.size() == 9);
    assert(!termsLayout.stretchesToAvailableWidth);
    for (std::size_t column = 0; column < termsMinimum.size(); ++column) {
        assert(!termsLayout.columns[column].absorbsExtraWidth);
        assert(termsNormal[column] == termsMinimum[column]);
        assert(termsWide[column] == termsMinimum[column]);
    }
    // Ordinary labels get a useful minimum. A long interaction can enlarge
    // Variable, but the native Auto column stops at this cap and ellipsizes
    // with a full-label tooltip instead of stretching the whole table.
    assert(termsLayout.columns[0].minimumWidth == 174.0);
    assert(termsLayout.columns[0].maximumWidth == 360.0);
    assert(termsNormal[7] == 76.0);
    assert(termsNormal[8] == 72.0);

    // An empty/intercept-only first render still constructs the header and
    // permanent + Add term action row.
    assert(rlispstat::core::GLMTermsTableNeedsRebuild(false, {}, {}));
    assert(!rlispstat::core::GLMTermsTableNeedsRebuild(true, {}, {}));
    assert(rlispstat::core::GLMTermsTableNeedsRebuild(
        true, {}, {"very_long_numeric_predictor\x1fnumeric\x1fterm"}));
    return 0;
}
