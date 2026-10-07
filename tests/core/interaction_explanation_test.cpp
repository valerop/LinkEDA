#include "../../src/core/glm_model.h"

#include <cassert>
#include <string>

int main()
{
    using namespace rlispstat::core;
    GLMInteractionReport report;
    report.kind = "factor_factor";
    report.imputationCount = 50;
    report.poolingMethod = "Rubin's rules";
    report.multipleComparisons = "Tukey adjustment";
    report.notes = {
        "Estimated marginal means: response scale",
        "Statistical contrasts: logit scale",
        "Transformed contrast: ratio of odds ratios"
    };
    GLMInteractionReportSection means;
    means.first = "Estimated marginal means";
    means.inference = GLMInteractionSectionInference::EstimateOnly;
    const std::string meanExplanation = ExplainModelStatistic(
        InteractionReportStatisticContext(report, means, "Estimate"));
    assert(meanExplanation.find("not the raw group average") != std::string::npos);
    assert(meanExplanation.find("response scale") != std::string::npos);
    assert(meanExplanation.find("Rubin's rules") != std::string::npos);

    GLMInteractionReportSection comparisons;
    comparisons.first = "Pairwise comparisons";
    comparisons.inference = GLMInteractionSectionInference::AdjustedComparison;
    const std::string comparisonExplanation = ExplainModelStatistic(
        InteractionReportStatisticContext(report, comparisons, "Estimate"));
    assert(comparisonExplanation.find("row's stated order") != std::string::npos);
    assert(comparisonExplanation.find("logit scale") != std::string::npos);
    const std::string pExplanation = ExplainModelStatistic(
        InteractionReportStatisticContext(report, comparisons, "Adjusted p"));
    assert(pExplanation.find("Tukey adjustment") != std::string::npos);
    assert(pExplanation.find("not the interaction term as a whole") != std::string::npos);

    GLMInteractionReportSection ratios;
    ratios.first = "Exponentiated interaction contrasts";
    ratios.estimateHeader = "Ratio of odds ratios";
    const std::string ratioExplanation = ExplainModelStatistic(
        InteractionReportStatisticContext(report, ratios, ratios.estimateHeader));
    assert(ratioExplanation.find("not an ordinary single-predictor") != std::string::npos);
    assert(ratioExplanation.find("ratio of odds ratios") != std::string::npos);
}
