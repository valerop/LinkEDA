#include "../../src/core/statistics_model.h"

#include <cassert>
#include <cmath>
#include <limits>
#include <string>

using rlispstat::core::ChiSquareUpperTail;
using rlispstat::core::CompareMeansTitle;
using rlispstat::core::MissingDataPairwiseMethodLabel;
using rlispstat::core::MissingDataListwiseMethodLabel;
using rlispstat::core::ActiveDatasetNoNumericVariablesStatus;
using rlispstat::core::CorrelationMatrixNeedsTwoNumericStatus;
using rlispstat::core::PCANeedsAtLeastTwoNumericStatus;
using rlispstat::core::QuickClusterNeedsNumericStatus;
using rlispstat::core::OneSampleTNoNumericStatus;
using rlispstat::core::IndependentTNeedGroupingStatus;
using rlispstat::core::PairedTNeedTwoNumericStatus;
using rlispstat::core::OneWayAnovaNeedFactorStatus;
using rlispstat::core::FDistributionUpperTail;
using rlispstat::core::NormalQuantileApprox;
using rlispstat::core::NormalTwoSidedP;
using rlispstat::core::NumericSummary;
using rlispstat::core::PairedFiniteSampleSDs;
using rlispstat::core::PairedSampleSDsResult;
using rlispstat::core::SummarizeFiniteValues;
using rlispstat::core::DescriptiveStatisticsWindowTitle;
using rlispstat::core::FrequencyTableWindowTitle;
using rlispstat::core::CompareMeansWindowTitle;
using rlispstat::core::OneSampleTTestWindowTitle;
using rlispstat::core::IndependentSamplesTTestWindowTitle;
using rlispstat::core::PairedSamplesTTestWindowTitle;
using rlispstat::core::OneWayANOVAWindowTitle;
using rlispstat::core::QuickClusterWindowTitle;
using rlispstat::core::RowsUsedExcludedWindowTitle;

static bool closeEnough(double a, double b, double tolerance = 1.0e-6)
{
    return std::fabs(a - b) < tolerance;
}

int main()
{
    double nan = std::numeric_limits<double>::quiet_NaN();

    NumericSummary summary = SummarizeFiniteValues({1.0, 2.0, 3.0, nan, INFINITY});
    assert(summary.ok);
    assert(summary.n == 3);
    assert(closeEnough(summary.mean, 2.0));
    assert(closeEnough(summary.sd, 1.0));
    assert(closeEnough(summary.min, 1.0));
    assert(closeEnough(summary.max, 3.0));

    NumericSummary one = SummarizeFiniteValues({5.0});
    assert(one.ok);
    assert(one.n == 1);
    assert(closeEnough(one.mean, 5.0));
    assert(closeEnough(one.sd, 0.0));

    NumericSummary empty = SummarizeFiniteValues({nan, INFINITY});
    assert(!empty.ok);
    assert(empty.n == 0);
    assert(!std::isfinite(empty.mean));

    PairedSampleSDsResult paired = PairedFiniteSampleSDs({1.0, 3.0, 5.0, nan},
                                                         {2.0, 4.0, 8.0, 10.0},
                                                         {1, 3});
    assert(paired.ok);
    assert(paired.n == 2);
    assert(!paired.usedFallbackAllRows);
    assert(closeEnough(paired.xSd, std::sqrt(8.0)));
    assert(closeEnough(paired.ySd, std::sqrt(18.0)));

    PairedSampleSDsResult fallback = PairedFiniteSampleSDs({1.0, 3.0, 5.0},
                                                           {2.0, 4.0, 8.0},
                                                           {99});
    assert(fallback.ok);
    assert(fallback.n == 3);
    assert(fallback.usedFallbackAllRows);

    PairedSampleSDsResult invalidPaired = PairedFiniteSampleSDs({1.0, 1.0, 1.0},
                                                                {2.0, 3.0, 4.0});
    assert(!invalidPaired.ok);

    assert(closeEnough(NormalTwoSidedP(0.0), 1.0));
    assert(closeEnough(NormalTwoSidedP(1.95996398454005), 0.050000, 1.0e-5));
    assert(closeEnough(NormalTwoSidedP(-1.95996398454005), 0.050000, 1.0e-5));
    assert(closeEnough(NormalQuantileApprox(0.5), 0.0));
    assert(closeEnough(NormalQuantileApprox(0.975), 1.95996398, 1.0e-6));
    assert(closeEnough(NormalQuantileApprox(0.025), -1.95996398, 1.0e-6));
    assert(!std::isfinite(NormalQuantileApprox(0.0)));

    assert(closeEnough(FDistributionUpperTail(1.0, 1.0, 10.0), 0.340893, 1.0e-5));
    assert(closeEnough(FDistributionUpperTail(4.0, 2.0, 20.0), 0.03457161, 1.0e-5));
    assert(!std::isfinite(FDistributionUpperTail(nan, 1.0, 10.0)));

    assert(closeEnough(ChiSquareUpperTail(0.0, 1.0), 1.0));
    assert(closeEnough(ChiSquareUpperTail(3.84145882069413, 1.0), 0.050000, 1.0e-5));
    assert(closeEnough(ChiSquareUpperTail(5.99146454710798, 2.0), 0.050000, 1.0e-5));
    assert(!std::isfinite(ChiSquareUpperTail(1.0, nan)));

    assert(CompareMeansTitle("one_sample_t") == "One-Sample Tests");
    assert(CompareMeansTitle("independent_t") == "Two-Sample Tests");
    assert(CompareMeansTitle("paired_t") == "Paired-Samples Tests");
    assert(CompareMeansTitle("oneway_anova") == "One-Way ANOVA");
    assert(CompareMeansTitle("unknown") == "Compare Means");

    assert(MissingDataPairwiseMethodLabel() == "pairwise");
    assert(MissingDataListwiseMethodLabel() == "listwise");

    assert(ActiveDatasetNoNumericVariablesStatus() == "The active dataset has no numeric variables.");
    assert(CorrelationMatrixNeedsTwoNumericStatus() == "This plot does not have two numeric variables for a correlation matrix.");
    assert(PCANeedsAtLeastTwoNumericStatus() == "At least two numeric variables are required.");
    assert(QuickClusterNeedsNumericStatus() == "Quick Cluster requires at least one numeric variable.");

    assert(OneSampleTNoNumericStatus() == "No numeric variable available.");
    assert(IndependentTNeedGroupingStatus() == "Need one numeric and one grouping variable.");
    assert(PairedTNeedTwoNumericStatus() == "Need at least two numeric variables.");
    assert(OneWayAnovaNeedFactorStatus() == "Need one continuous and one categorical variable.");
    assert(DescriptiveStatisticsWindowTitle() == "Descriptive Statistics");
    assert(FrequencyTableWindowTitle() == "Frequency Table");
    assert(CompareMeansWindowTitle() == "Compare Means");
    assert(OneSampleTTestWindowTitle() == "One-Sample Tests");
    assert(IndependentSamplesTTestWindowTitle() == "Two-Sample Tests");
    assert(PairedSamplesTTestWindowTitle() == "Paired-Samples Tests");
    assert(OneWayANOVAWindowTitle() == "One-Way ANOVA");
    assert(QuickClusterWindowTitle() == "Quick Cluster");
    assert(RowsUsedExcludedWindowTitle() == "Rows Used/Excluded");

    return 0;
}
