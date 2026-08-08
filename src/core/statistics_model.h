#ifndef RLISPSTAT_CORE_STATISTICS_MODEL_H
#define RLISPSTAT_CORE_STATISTICS_MODEL_H

#include <limits>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

struct PooledCorrelationScalar {
    bool valid = false;
    double qbar = std::numeric_limits<double>::quiet_NaN();
    double se = std::numeric_limits<double>::quiet_NaN();
    double df = std::numeric_limits<double>::quiet_NaN();
    double p = std::numeric_limits<double>::quiet_NaN();
    double fmi = std::numeric_limits<double>::quiet_NaN();
};

struct NumericSummary {
    bool ok = false;
    int n = 0;
    double mean = std::numeric_limits<double>::quiet_NaN();
    double sd = std::numeric_limits<double>::quiet_NaN();
    double min = std::numeric_limits<double>::quiet_NaN();
    double max = std::numeric_limits<double>::quiet_NaN();
};

struct PairedSampleSDsResult {
    bool ok = false;
    int n = 0;
    bool usedFallbackAllRows = false;
    double xSd = std::numeric_limits<double>::quiet_NaN();
    double ySd = std::numeric_limits<double>::quiet_NaN();
};

NumericSummary SummarizeFiniteValues(const std::vector<double> &values);
PairedSampleSDsResult PairedFiniteSampleSDs(const std::vector<double> &x,
                                            const std::vector<double> &y,
                                            const std::vector<int> &oneBasedRows = {});
double NormalTwoSidedP(double z);
double NormalQuantileApprox(double p);
double FDistributionUpperTail(double f, double df1, double df2);
double ChiSquareUpperTail(double chisq, double df);
PooledCorrelationScalar PoolCorrelationOnFisherZ(const std::vector<double> &rByImputation,
                                                 const std::vector<int> &nByImputation);
std::string CompareMeansTitle(const std::string &testType);
std::string MissingDataPairwiseMethodLabel();
std::string MissingDataListwiseMethodLabel();
std::string MissingDataFieldLabel();
std::string DescriptiveStatisticsWindowTitle();
std::string FrequencyTableWindowTitle();
std::string CompareMeansWindowTitle();
std::string OneSampleTTestWindowTitle();
std::string IndependentSamplesTTestWindowTitle();
std::string PairedSamplesTTestWindowTitle();
std::string OneWayANOVAWindowTitle();
std::string QuickClusterWindowTitle();
std::string RowsUsedExcludedWindowTitle();
std::string ActiveDatasetNoNumericVariablesStatus();
std::string CorrelationMatrixNeedsTwoNumericStatus();
std::string PCANeedsAtLeastTwoNumericStatus();
std::string QuickClusterNeedsNumericStatus();
std::string OneSampleTNoNumericStatus();
std::string IndependentTNeedGroupingStatus();
std::string PairedTNeedTwoNumericStatus();
std::string OneWayAnovaNeedFactorStatus();
std::string RegressionModelComparisonTitle();

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_STATISTICS_MODEL_H
