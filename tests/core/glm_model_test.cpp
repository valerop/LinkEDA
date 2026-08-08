#include "../../src/core/glm_model.h"
#include "../../src/core/binary_regression_export.h"
#include "../../src/core/dataset_model.h"

#include <cassert>
#include <cmath>
#include <limits>
#include <map>
#include <string>
#include <vector>

using rlispstat::core::DefaultGeneralizedLink;
using rlispstat::core::DefaultGeneralizedFamilyForValues;
using rlispstat::core::BuildScenarioDesignVector;
using rlispstat::core::BuildGeneralizedDiagnosticPlotData;
using rlispstat::core::BuildLinearDiagnosticPlotData;
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
using rlispstat::core::ApplyHolmAdjustedPValues;
using rlispstat::core::ContrastEstimate;
using rlispstat::core::ContrastVariance;
using rlispstat::core::CoefficientForTerm;
using rlispstat::core::CoefficientsForSourceTerm;
using rlispstat::core::DiagnosticPlotData;
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
using rlispstat::core::GeneralizedGLMState;
using rlispstat::core::GeneralizedGLMFitFailedStatus;
using rlispstat::core::GeneralizedGLMRscriptLaunchFailedStatus;
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
using rlispstat::core::GLMRegressionPartialRSquaredLabel;
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
using rlispstat::core::GeneralizedRowSourceTerm;
using rlispstat::core::GLMCoefficientRow;
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
using rlispstat::core::PrecomputedComparisonChangeInRStatus;
using rlispstat::core::ReadGeneralizedRowsPayload;
using rlispstat::core::ReadGeneralizedFitPayload;
using rlispstat::core::ReadGeneralizedStatePayload;
using rlispstat::core::ReadIntVectorPayload;
using rlispstat::core::ReadLinearDesignPayload;
using rlispstat::core::ReadLinearDiagnosticsPayload;
using rlispstat::core::ReadLinearFitPayload;
using rlispstat::core::RegressionCoefficientDisplay;
using rlispstat::core::RegressionComparisonFitDisplay;
using rlispstat::core::RegressionComparisonVisibleFitRows;
using rlispstat::core::RegressionComparisonChangeDisplay;
using rlispstat::core::RegressionComparisonFitSignature;
using rlispstat::core::RegressionComparisonFitLabel;
using rlispstat::core::RegressionComparisonFitRowIsModelTest;
using rlispstat::core::RegressionComparisonFootnote;
using rlispstat::core::RegressionComparisonCopiedStatus;
using rlispstat::core::RegressionComparisonModel;
using rlispstat::core::RegressionComparisonModelTestStatus;
using rlispstat::core::RegressionComparisonNoPreviousModelStatus;
using rlispstat::core::RegressionComparisonRefittedStatus;
using rlispstat::core::RegressionComparisonFitValueStatus;
using rlispstat::core::RegressionComparisonModelRenamedStatus;
using rlispstat::core::RegressionComparisonOpenedSingleModelStatus;
using rlispstat::core::RegressionComparisonState;
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
        std::map<std::string, GroupModelState> states;
        PlotModel oneNumericSeed;
        oneNumericSeed.xLabel = "Valor";
        oneNumericSeed.yLabel = "Valor";
        GroupModelState &state = EnsureGroupModelState(states, "datos", &oneNumericSeed);
        assert(state.dependent == "Valor");
        assert(state.terms.empty());

        PlotModel twoNumericSeed;
        twoNumericSeed.xLabel = "Tiempo";
        twoNumericSeed.yLabel = "Valor";
        GroupModelState &twoVariableState = EnsureGroupModelState(states, "datos2", &twoNumericSeed);
        assert(twoVariableState.dependent == "Valor");
        assert(twoVariableState.terms == std::vector<std::string>({"Tiempo"}));
    }
    BinaryLink binaryLink = BinaryLink::Logit;
    assert(ParseBinaryLink("Probit", binaryLink));
    assert(binaryLink == BinaryLink::Probit);
    assert(BinaryLinkId(binaryLink) == "probit");
    assert(BinaryLinkLabel(binaryLink) == "Probit");
    assert(!ParseBinaryLink("cloglog", binaryLink));
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
    assert(IsValidGeneralizedFamily("binomial"));
    assert(IsValidGeneralizedFamily("quasipoisson"));
    assert(!IsValidGeneralizedFamily("cox"));

    assert(DefaultGeneralizedLink("gaussian") == "identity");
    assert(DefaultGeneralizedLink("poisson") == "log");
    assert(DefaultGeneralizedLink("Gamma") == "inverse");
    assert(DefaultGeneralizedLink("unknown") == "logit");
    assert(DefaultGeneralizedFamilyForValues({0.0, 1.0, 1.0, std::numeric_limits<double>::quiet_NaN()}) == "binomial");
    assert(DefaultGeneralizedFamilyForValues({0.0, 1.0, 2.0, 3.0}) == "poisson");
    assert(DefaultGeneralizedFamilyForValues({0.0, 1.5, 2.0}) == "gaussian");
    assert(DefaultGeneralizedFamilyForValues({-1.0, 0.0, 1.0}) == "gaussian");
    assert(DefaultGeneralizedFamilyForValues({std::numeric_limits<double>::quiet_NaN()}) == "gaussian");

    assert((GeneralizedLinksForFamily("binomial") ==
            std::vector<std::string>{"logit", "probit", "cloglog", "cauchit", "log"}));
    assert(IsValidGeneralizedLink("binomial", "probit"));
    assert(!IsValidGeneralizedLink("poisson", "probit"));
    assert(IsQuasiGeneralizedFamily("quasipoisson"));
    assert(!IsQuasiGeneralizedFamily("poisson"));

    std::map<std::string, std::string> signatureTypes{{"cyl", "factor"}, {"wt", "numeric"}};
    assert(LinearGLMFitSignature("mpg", {"wt", "cyl"}, signatureTypes, "selected") ==
           "mpg|selected|wt|cyl|types|cyl=factor|wt=numeric");

    GeneralizedGLMState generalizedSignatureState;
    generalizedSignatureState.group = "cars";
    generalizedSignatureState.response = "am";
    generalizedSignatureState.family = "binomial";
    generalizedSignatureState.link = "logit";
    generalizedSignatureState.scope = "all";
    generalizedSignatureState.terms = {"wt", "cyl"};
    generalizedSignatureState.termTypes = signatureTypes;
    assert(GeneralizedGLMFitSignature(generalizedSignatureState) ==
           "cars|am|binomial|logit|all|wt|cyl|types|cyl=factor|wt=numeric");
    generalizedSignatureState.binaryRegression = true;
    generalizedSignatureState.responseCoding.eventValue = "manual";
    generalizedSignatureState.responseCoding.referenceValue = "automatic";
    assert(GeneralizedGLMFitSignature(generalizedSignatureState) ==
           "cars|am|binomial|logit|all|binary|event=manual|reference=automatic|wt|cyl|types|cyl=factor|wt=numeric");

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
    reducedModel.includedTerms = {"wt"};
    RegressionComparisonModel fullModel;
    fullModel.id = "m2";
    fullModel.label = "Full";
    fullModel.response = "hp";
    fullModel.includedTerms = {"wt", "cyl"};
    comparisonSignatureState.models = {reducedModel, fullModel};
    assert(RegressionComparisonFitSignature(comparisonSignatureState) ==
           "cmp1|cars|mpg|all|types|cyl=factor|wt=numeric|models|m1:Reduced:mpg+wt|m2:Full:hp+wt+cyl");

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
        "1.5", "0.3", "0.2", "7.5", "0.002", "0.6"
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
    std::vector<std::string> truncatedLinearDiagnosticsPayload = linearDiagnosticsPayload;
    truncatedLinearDiagnosticsPayload.pop_back();
    linearDiagnosticsCursor = 0;
    assert(!ReadLinearDiagnosticsPayload(truncatedLinearDiagnosticsPayload, linearDiagnosticsCursor, diagnosticsFromPayload));

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
    assert(RegressionCoefficientDisplay(payloadFit, {"x"}, "x") == "1.500**");
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
    assert(RegressionCoefficientDisplay(inferredSourceFit, {"cyl:am"}, "cyl=6:am") == "2.000");
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
    assert(generalizedStateFromPayload.n == 12);
    assert(generalizedStateFromPayload.excluded == 1);
    assert(closeEnough(generalizedStateFromPayload.nullDeviance, 30.5));
    assert(closeEnough(generalizedStateFromPayload.residualDeviance, 18.25));
    assert(generalizedStateFromPayload.rowsUsed == std::vector<int>({1, 2, 3}));
    assert(generalizedStateFromPayload.rowsExcluded == std::vector<int>({4}));
    assert(generalizedStateFromPayload.rows.size() == 1);
    assert(generalizedStateFromPayload.rows[0].term == "x");
    assert(closeEnough(generalizedStateFromPayload.rows[0].estimate, 2.5));

    std::vector<std::string> binaryStatePayload = generalizedFitPayload;
    binaryStatePayload.insert(binaryStatePayload.end(), {
        "BINARY_V2", "yes", "no", "7", "5", "1", "4.2", "0.04",
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
    assert(generalizedScript.find(".rls_model_coefficient_display_rows") != std::string::npos);
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
    assert(NextUntitledComparisonLabel({"Untitled 1", "Untitled 3"}) == "Untitled 4");
    assert(UniqueRegressionComparisonCopyLabel({"Model 1", "Untitled 1"}, "Model 1") == "Untitled 2");
    assert(UniqueRegressionComparisonCopyLabel({"Custom", "Custom copy"}, "Custom") == "Custom copy 2");
    assert(UniqueGeneralizedComparisonCopyLabel({"Untitled copy"}, "") == "Untitled copy 2");
    assert(UniqueGeneralizedComparisonCopyLabel({"A", "A copy", "A copy 2"}, "A") == "A copy 3");
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
    assert(GeneralizedComparisonFitLabel(0) == "Family");
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
        assert(std::find(binaryMainRows.begin(), binaryMainRows.end(), 4) == binaryMainRows.end());
        assert(std::find(binaryMainRows.begin(), binaryMainRows.end(), 12) != binaryMainRows.end());
        assert(std::find(binaryInformationRows.begin(), binaryInformationRows.end(), 4) != binaryInformationRows.end());
        assert(std::find(binaryInformationRows.begin(), binaryInformationRows.end(), 14) != binaryInformationRows.end());
    }
    assert(RegressionComparisonFootnote().find("Partial F uses \u0394SSE") != std::string::npos);
    assert(RegressionComparisonFootnote().find("global F") != std::string::npos);
    assert(GeneralizedComparisonFootnote().find("previous column") != std::string::npos);
    ComparisonCopyTable copyTable;
    copyTable.title = "Regression Model Comparison";
    copyTable.response = "y";
    copyTable.modelLabels = {"Reduced", "Full"};
    copyTable.modelResponses = {"y", "other"};
    copyTable.termLabels = {"(Intercept)", "x"};
    copyTable.termDisplaysByRow = {{"1.0", "2.0"}, {"\u2014", "3.0*"}};
    copyTable.fitLabels = {"N", "Partial F vs prev"};
    copyTable.fitDisplaysByRow = {{"10", "10"}, {"\u2014", "4.500"}};
    copyTable.footnote = RegressionComparisonFootnote();
    std::string copyText = ComparisonCopyTableText(copyTable);
    assert(copyText.find("Regression Model Comparison\nResponse:\ty\n\nTerm\tReduced\tFull\nResponse\ty\tother\n") == 0);
    assert(copyText.find("x\t\u2014\t3.0*\n\nN\t10\t10\nPartial F vs prev\t\u2014\t4.500") != std::string::npos);
    assert(copyText.find("Partial F uses \u0394SSE") != std::string::npos);

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

    GeneralizedGLMRow generalizedCoefficient;
    generalizedCoefficient.term = "groupB";
    std::vector<std::string> generalizedTermRows = GeneralizedComparisonTermRowsFromFits(
        {{"x", "group"}}, {{generalizedCoefficient}});
    assert(generalizedTermRows == std::vector<std::string>({"(Intercept)", "x", "group", "groupB"}));

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
    generalizedFactorReference.sourceTerm = "";
    generalizedFactorLevel.sourceTerm = "";
    generalizedTermRows = GeneralizedComparisonTermRowsFromFits(
        {{"mpg", "am", "wt"}},
        {{generalizedFactorParent, generalizedFactorReference, generalizedFactorLevel, generalizedNumeric}});
    assert(generalizedTermRows == std::vector<std::string>({"(Intercept)", "mpg", "am", "0", "1", "wt"}));

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
    assert(GLMTermTypeChangedStatus("x", "factor") == "Status: x is Factor.");
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
        coef.partialR2 = 0.753;
        fit.coefficients.push_back(coef);
        std::string table = GLMRegressionCopyTableText(fit, "mpg", true);
        assert(table.find("Response:\tmpg") != std::string::npos);
        assert(table.find("Variable\tb\tβ\tSE\tt\tp\tPartial R²") != std::string::npos);
        assert(table.find("wt\t-5.3440\t-0.868\t0.5590\t-9.559\t< .001\t75.3%") != std::string::npos);
        assert(table.find("D1/Wald\t91.375") != std::string::npos);
        assert(table.find("AIC") == std::string::npos);
        assert(GLMModelDetailsText(fit, true).find("AIC: 123.4") != std::string::npos);
    }
    assert(GLMStatusMessage("ready") == "Status: ready");
    assert(GLMFitNoteWarningStatus("pooled", "singular fit") == "Status: pooled Warning: singular fit");
    assert(GLMInteractionReportOpenedStatus("x:z") == "Status: interaction report opened for x:z.");
    assert(GLMInteractionPlotOpenedStatus("x:z") == "Status: interaction plot opened for x:z.");
    assert(GeneralizedGLMRscriptLaunchFailedStatus() == "Could not run Rscript for stats::glm().");
    GeneralizedGLMRow detailRow;
    detailRow.term = "x";
    detailRow.displayLabel = "x";
    detailRow.estimate = 1.23456;
    detailRow.stdError = 0.25;
    detailRow.statistic = 4.938;
    detailRow.pValue = 0.0004;
    assert(GeneralizedCoefficientDetailsStatus(detailRow, "z").find("x; b = 1.2346") != std::string::npos);
    GeneralizedGLMRow parentDetail;
    parentDetail.term = "group";
    parentDetail.rowType = "factor_parent";
    assert(GeneralizedCoefficientDetailsStatus(parentDetail, "z") ==
           "Status: group is a factor term; use its level rows for coefficients.");
    GeneralizedGLMRow referenceDetail;
    referenceDetail.sourceTerm = "group";
    referenceDetail.factorLevel = "A";
    referenceDetail.rowType = "reference";
    assert(GeneralizedCoefficientDetailsStatus(referenceDetail, "z") ==
           "Status: group: A is the reference category; b = \u2014; SE = \u2014; z = \u2014; p = \u2014.");
    assert(ComparisonTermNotIncludedStatus() == "Status: term is not included in this model.");
    assert(GeneralizedComparisonCoefficientDetailsStatus(detailRow).find("Status: b = 1.2346") == 0);
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
           "Status: group is a factor term; use its level rows for coefficients.");
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
           "Status: fitted  Selected rows: 4 / 10  Family = poisson, link = log");
    {
        GeneralizedGLMState state;
        state.response = "am";
        state.family = "binomial";
        state.link = "logit";
        state.residualType = "deviance";
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
        assert(text.find("Response  am     Family  binomial     Link  logit") != std::string::npos);
        assert(text.find("Null deviance  43.200") != std::string::npos);
        assert(text.find("wt") != std::string::npos);
        assert(text.find(".007") != std::string::npos);
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
        wtTest.df = 1;
        wtTest.statistic = 8.25;
        wtTest.pValue = 0.0041;
        binary.termTests.push_back(wtTest);
        std::string binaryText = GeneralizedGLMFormattedOutputText(binary);
        assert(binaryText.find("Terms and coefficients") != std::string::npos);
        assert(binaryText.find("Wald p") != std::string::npos);
        assert(binaryText.find("LR chi2") != std::string::npos);
        assert(binaryText.find("\nTerms\n") == std::string::npos);
        assert(binaryText.find("8.25") != std::string::npos);
        assert(binaryText.find("AIC") == std::string::npos);
        assert(binaryText.find("Apparent AUC") == std::string::npos);
        assert(binaryText.find("Nagelkerke pseudo-R") != std::string::npos);
        assert(GeneralizedGLMModelDetailsText(binary).find("AIC:") != std::string::npos);
        GeneralizedGLMRow factorParent;
        factorParent.term = "cyl";
        factorParent.sourceTerm = "cyl";
        factorParent.displayLabel = "cyl";
        factorParent.rowType = "factor_parent";
        GeneralizedGLMRow factorReference;
        factorReference.term = "cyl4";
        factorReference.sourceTerm = "cyl";
        factorReference.displayLabel = "4";
        factorReference.rowType = "reference";
        factorReference.factorLevel = "4";
        factorReference.referenceLevel = "4";
        GeneralizedGLMRow factorLevel;
        factorLevel.term = "cyl6";
        factorLevel.sourceTerm = "cyl";
        factorLevel.displayLabel = "6";
        factorLevel.rowType = "factor_level";
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
        cylTest.df = 2;
        cylTest.statistic = 12.47;
        cylTest.pValue = 0.002;
        binary.termTests.push_back(cylTest);
        binaryText = GeneralizedGLMFormattedOutputText(binary);
        assert(binaryText.find("4 (reference)") != std::string::npos);
        assert(binaryText.find("12.47") != std::string::npos);
        assert(binaryText.find("1.446") != std::string::npos);
        assert(binaryText.find("12.47") == binaryText.rfind("12.47"));
        const std::string binaryDetailedText = GeneralizedGLMFormattedOutputText(binary, true);
        assert(binaryDetailedText.find("Model details") != std::string::npos);
        assert(binaryDetailedText.find("not out-of-sample validation") != std::string::npos);
        const auto apaLogit = BuildBinaryAPAReportModel(binary);
        assert(apaLogit.logit);
        assert(apaLogit.defaultTitle == "Binary Logistic Regression Predicting am");
        assert(apaLogit.coefficients.size() == 2);
        assert(apaLogit.coefficients[1].predictor == "cyl: 6 vs. 4");
        assert(apaLogit.termTests.size() == 2);
        const std::string logitCSV = BinaryRegressionCoefficientCSV(binary);
        assert(logitCSV.find("Predictor,Estimate,SE,z,p,Odds_Ratio,OR_CI_Lower,OR_CI_Upper,Reference") == 0);
        assert(logitCSV.find("\"cyl: 4 (reference)\",,,,,,,,true") != std::string::npos);
        assert(logitCSV.find("\"cyl: 6 vs. 4\",1.446") != std::string::npos);
        const std::string termCSV = BinaryRegressionTermTestsCSV(binary);
        assert(termCSV.find("Term,LR_Chisq,df,p") == 0);
        assert(termCSV.find("\"cyl\",") != std::string::npos);
        assert(termCSV.find(",2,0.002") != std::string::npos);
        const std::string fitCSV = BinaryRegressionModelFitCSV(binary);
        assert(fitCSV.find("Statistic,Value") == 0);
        assert(fitCSV.find("\"AIC\",") != std::string::npos);
        assert(fitCSV.find("\"Apparent AUC\",") != std::string::npos);
        binary.binaryLink = BinaryLink::Probit;
        const auto apaProbit = BuildBinaryAPAReportModel(binary);
        assert(!apaProbit.logit);
        assert(apaProbit.defaultTitle == "Binary Probit Regression Predicting am");
        const std::string probitCSV = BinaryRegressionCoefficientCSV(binary);
        assert(probitCSV.find("Predictor,Estimate,SE,z,p,CI_Lower,CI_Upper,Reference") == 0);
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
    assert(!LinearDiagnosticKindIsImplemented("normal_qq"));

    DiagnosticPlotData linearObserved = BuildLinearDiagnosticPlotData("observed_vs_fitted",
                                                                      fit.diagnostics,
                                                                      7,
                                                                      8);
    assert(linearObserved.ok);
    assert(linearObserved.kind == "scatter");
    assert(linearObserved.title == "Observed vs fitted");
    assert(linearObserved.xLabel == "fitted");
    assert(linearObserved.yLabel == "observed");
    assert(linearObserved.fitVersion == 7);
    assert(linearObserved.diagnosticsVersion == 8);
    assert(linearObserved.points.size() == fit.diagnostics.size());
    assert(closeEnough(linearObserved.points[0].x, fit.diagnostics[0].fitted));
    assert(closeEnough(linearObserved.points[0].y, fit.diagnostics[0].observed));

    DiagnosticPlotData linearResidual = BuildLinearDiagnosticPlotData("residuals_fitted",
                                                                      fit.diagnostics,
                                                                      7,
                                                                      9);
    assert(linearResidual.ok);
    assert(linearResidual.title == "Residuals vs fitted");
    assert(linearResidual.yLabel == "residual");
    assert(closeEnough(linearResidual.points[0].y, fit.diagnostics[0].residual));

    GeneralizedDiagnosticRow gd1;
    gd1.row = 1;
    gd1.observed = 0.0;
    gd1.fitted = 0.20;
    gd1.devianceResidual = -1.5;
    gd1.pearsonResidual = -1.2;
    gd1.workingResidual = -1.0;
    gd1.leverage = 0.10;
    gd1.cooksDistance = 0.02;
    GeneralizedDiagnosticRow gd2;
    gd2.row = 2;
    gd2.observed = 1.0;
    gd2.fitted = 0.80;
    gd2.devianceResidual = 0.8;
    gd2.pearsonResidual = 1.1;
    gd2.workingResidual = 0.9;
    gd2.leverage = 0.25;
    gd2.cooksDistance = 0.08;
    std::vector<GeneralizedDiagnosticRow> generalizedRows = {gd1, gd2};
    assert(closeEnough(GeneralizedResidualForDiagnostic(gd1, "pearson"), -1.2));
    assert(closeEnough(GeneralizedResidualForDiagnostic(gd1, "working"), -1.0));
    assert(closeEnough(GeneralizedResidualForDiagnostic(gd1, "deviance"), -1.5));

    DiagnosticPlotData generalizedObserved = BuildGeneralizedDiagnosticPlotData("observed_vs_fitted",
                                                                                "deviance",
                                                                                generalizedRows,
                                                                                2,
                                                                                3);
    assert(generalizedObserved.ok);
    assert(generalizedObserved.diagnosticKind == "observed_fitted");
    assert(generalizedObserved.title == "Observed vs fitted");
    assert(generalizedObserved.points.size() == 2);
    assert(closeEnough(generalizedObserved.points[1].y, 1.0));

    DiagnosticPlotData generalizedResidual = BuildGeneralizedDiagnosticPlotData("residuals_fitted",
                                                                                "pearson",
                                                                                generalizedRows,
                                                                                2,
                                                                                4);
    assert(generalizedResidual.ok);
    assert(generalizedResidual.title == "Residuals vs fitted");
    assert(closeEnough(generalizedResidual.points[0].y, -1.2));

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
    assert(GLMInteractionTwoWayOnlyStatus() == "Interaction plots currently support two-way interactions.");
    assert(GLMInteractionNoFittedValuesStatus() == "The interaction plot could not be built because a component has no fitted values.");
    assert(GLMInteractionFactorNoLevelsStatus() == "The interaction plot could not be built because a factor has no fitted levels.");
    assert(GLMInteractionTypeNotAvailableStatus() == "This interaction plot type is not available.");
    assert(GLMInteractionNoFiniteValuesStatus() == "The interaction plot has no finite fitted values.");
    assert(GLMChooseDependentVariableStatus() == "Choose a dependent variable.");
    assert(GLMDependentVariableNotAvailableStatus() == "Dependent variable is not available.");
    assert(GLMPredictorUnavailableStatus("x") == "Predictor `x` is not available.");
    assert(GLMFactorPredictorUnavailableStatus("group") == "Factor predictor `group` is not available.");
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
    assert(GLMRegressionComparisonNotAvailableStatus() == "The regression comparison is not available.");
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
    assert(GLMGeneralLinearModelTitle() == "General Linear Model");
    assert(GLMAutoRefitButtonTitle() == "Auto-refit");
    assert(GLMDiagnosticResidualHistogramTitle() == "Residual histogram");
    assert(GLMDiagnosticResidualsFittedTitle() == "Residuals vs fitted");
    assert(GLMDiagnosticObservedFittedTitle() == "Observed vs fitted");
    assert(GLMDiagnosticNormalQQTitle() == "Normal Q-Q");
    assert(GLMDevianceResidualTitle() == "Deviance");
    assert(GLMPearsonResidualTitle() == "Pearson");
    assert(GLMWorkingResidualTitle() == "Working");
    assert(GLMAddTermTitle() == "Add term");
    assert(GLMChangeTermTitle() == "Change term");
    assert(GLMInteractionPlotWindowTitle() == "Interaction Plot");
    assert(GLMDiagnosticPlotWindowTitle() == "Diagnostic Plot");
    assert(GLMRenameModelTitle() == "Rename model");
    assert(GLMDuplicateModelTitle() == "Duplicate model");
    assert(GLMDeleteModelTitle() == "Delete model");
    assert(GLMComparisonFamilyMenuTitle() == "Family");
    assert(GLMComparisonLinkMenuTitle() == "Link");
    assert(GLMComparisonFamilyMenuItemTitle() == "Family...");
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
    assert(GLMPlusSignLabel() == "+");
    assert(GLMSelectedRowsPlaceholder() == "Selected rows: 0 / 0");
    assert(GLMD1WaldLabel() == "D1/Wald");
    assert(GLMAnovaFRatioLabel() == "F-ratio");
    assert(GLMRegressionBetaLabel() == "\u03B2");
    assert(GLMRegressionPartialRSquaredLabel() == "Partial R\u00B2");
    assert(GLMInteractionsMenuTitle() == "Interactions");
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
    return 0;
}
