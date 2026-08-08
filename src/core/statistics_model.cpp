#include "statistics_model.h"

#include <algorithm>
#include <cmath>

namespace rlispstat {
namespace core {

namespace {

double BetaContinuedFraction(double a, double b, double x)
{
    const int maxIterations = 200;
    const double epsilon = 3.0e-12;
    const double fpMin = 1.0e-300;
    double qab = a + b;
    double qap = a + 1.0;
    double qam = a - 1.0;
    double c = 1.0;
    double d = 1.0 - qab * x / qap;
    if (std::fabs(d) < fpMin) d = fpMin;
    d = 1.0 / d;
    double h = d;
    for (int m = 1; m <= maxIterations; ++m) {
        int m2 = 2 * m;
        double aa = m * (b - m) * x / ((qam + m2) * (a + m2));
        d = 1.0 + aa * d;
        if (std::fabs(d) < fpMin) d = fpMin;
        c = 1.0 + aa / c;
        if (std::fabs(c) < fpMin) c = fpMin;
        d = 1.0 / d;
        h *= d * c;
        aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2));
        d = 1.0 + aa * d;
        if (std::fabs(d) < fpMin) d = fpMin;
        c = 1.0 + aa / c;
        if (std::fabs(c) < fpMin) c = fpMin;
        d = 1.0 / d;
        double del = d * c;
        h *= del;
        if (std::fabs(del - 1.0) <= epsilon) break;
    }
    return h;
}

double RegularizedIncompleteBeta(double a, double b, double x)
{
    if (!(a > 0.0) || !(b > 0.0) || x < 0.0 || x > 1.0 || !std::isfinite(x)) {
        return NAN;
    }
    if (x == 0.0) return 0.0;
    if (x == 1.0) return 1.0;
    double bt = std::exp(std::lgamma(a + b) - std::lgamma(a) - std::lgamma(b) +
                         a * std::log(x) + b * std::log1p(-x));
    if (x < (a + 1.0) / (a + b + 2.0)) {
        return bt * BetaContinuedFraction(a, b, x) / a;
    }
    return 1.0 - bt * BetaContinuedFraction(b, a, 1.0 - x) / b;
}

double RegularizedGammaP(double a, double x)
{
    if (!(a > 0.0) || x < 0.0 || !std::isfinite(a) || !std::isfinite(x)) return NAN;
    if (x == 0.0) return 0.0;
    const int maxIterations = 200;
    const double epsilon = 3.0e-12;
    double ap = a;
    double del = 1.0 / a;
    double sum = del;
    for (int n = 1; n <= maxIterations; ++n) {
        ap += 1.0;
        del *= x / ap;
        sum += del;
        if (std::fabs(del) < std::fabs(sum) * epsilon) {
            return sum * std::exp(-x + a * std::log(x) - std::lgamma(a));
        }
    }
    return sum * std::exp(-x + a * std::log(x) - std::lgamma(a));
}

double RegularizedGammaQ(double a, double x)
{
    if (!(a > 0.0) || x < 0.0 || !std::isfinite(a) || !std::isfinite(x)) return NAN;
    if (x == 0.0) return 1.0;
    if (x < a + 1.0) {
        double p = RegularizedGammaP(a, x);
        return std::isfinite(p) ? std::max(0.0, 1.0 - p) : NAN;
    }
    const int maxIterations = 200;
    const double epsilon = 3.0e-12;
    const double fpMin = 1.0e-300;
    double b = x + 1.0 - a;
    double c = 1.0 / fpMin;
    double d = 1.0 / std::max(b, fpMin);
    double h = d;
    for (int i = 1; i <= maxIterations; ++i) {
        double an = -i * (i - a);
        b += 2.0;
        d = an * d + b;
        if (std::fabs(d) < fpMin) d = fpMin;
        c = b + an / c;
        if (std::fabs(c) < fpMin) c = fpMin;
        d = 1.0 / d;
        double del = d * c;
        h *= del;
        if (std::fabs(del - 1.0) <= epsilon) break;
    }
    return std::exp(-x + a * std::log(x) - std::lgamma(a)) * h;
}

} // namespace

NumericSummary SummarizeFiniteValues(const std::vector<double> &values)
{
    NumericSummary summary;
    std::vector<double> finite;
    finite.reserve(values.size());
    for (double value : values) {
        if (std::isfinite(value)) {
            finite.push_back(value);
        }
    }
    if (finite.empty()) return summary;

    summary.ok = true;
    summary.n = static_cast<int>(finite.size());
    summary.mean = 0.0;
    summary.min = finite.front();
    summary.max = finite.front();
    for (double value : finite) {
        summary.mean += value;
        summary.min = std::min(summary.min, value);
        summary.max = std::max(summary.max, value);
    }
    summary.mean /= static_cast<double>(finite.size());
    if (finite.size() > 1) {
        double ss = 0.0;
        for (double value : finite) {
            double d = value - summary.mean;
            ss += d * d;
        }
        summary.sd = std::sqrt(ss / static_cast<double>(finite.size() - 1));
    } else {
        summary.sd = 0.0;
    }
    return summary;
}

PairedSampleSDsResult PairedFiniteSampleSDs(const std::vector<double> &x,
                                            const std::vector<double> &y,
                                            const std::vector<int> &oneBasedRows)
{
    PairedSampleSDsResult out;
    std::vector<double> xs;
    std::vector<double> ys;
    auto appendAt = [&](std::size_t i) {
        if (i >= x.size() || i >= y.size()) {
            return;
        }
        double xValue = x[i];
        double yValue = y[i];
        if (!std::isfinite(xValue) || !std::isfinite(yValue)) {
            return;
        }
        xs.push_back(xValue);
        ys.push_back(yValue);
    };
    for (int row : oneBasedRows) {
        if (row > 0) {
            appendAt(static_cast<std::size_t>(row - 1));
        }
    }
    if (xs.size() < 2) {
        xs.clear();
        ys.clear();
        out.usedFallbackAllRows = !oneBasedRows.empty();
        std::size_t n = std::min(x.size(), y.size());
        for (std::size_t i = 0; i < n; ++i) {
            appendAt(i);
        }
    }
    if (xs.size() < 2) {
        return out;
    }
    NumericSummary xSummary = SummarizeFiniteValues(xs);
    NumericSummary ySummary = SummarizeFiniteValues(ys);
    out.n = static_cast<int>(xs.size());
    out.xSd = xSummary.sd;
    out.ySd = ySummary.sd;
    out.ok = std::isfinite(out.xSd) && out.xSd > 0.0 &&
             std::isfinite(out.ySd) && out.ySd > 0.0;
    return out;
}

double NormalTwoSidedP(double z)
{
    return std::erfc(std::fabs(z) / std::sqrt(2.0));
}

double NormalQuantileApprox(double p)
{
    if (!(p > 0.0 && p < 1.0)) return NAN;
    static const double a[] = {
        -3.969683028665376e+01, 2.209460984245205e+02,
        -2.759285104469687e+02, 1.383577518672690e+02,
        -3.066479806614716e+01, 2.506628277459239e+00
    };
    static const double b[] = {
        -5.447609879822406e+01, 1.615858368580409e+02,
        -1.556989798598866e+02, 6.680131188771972e+01,
        -1.328068155288572e+01
    };
    static const double c[] = {
        -7.784894002430293e-03, -3.223964580411365e-01,
        -2.400758277161838e+00, -2.549732539343734e+00,
         4.374664141464968e+00,  2.938163982698783e+00
    };
    static const double d[] = {
         7.784695709041462e-03, 3.224671290700398e-01,
         2.445134137142996e+00, 3.754408661907416e+00
    };
    const double plow = 0.02425;
    const double phigh = 1.0 - plow;
    if (p < plow) {
        double q = std::sqrt(-2.0 * std::log(p));
        return (((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5]) /
               ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1.0);
    }
    if (p > phigh) {
        double q = std::sqrt(-2.0 * std::log(1.0 - p));
        return -(((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5]) /
                ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1.0);
    }
    double q = p - 0.5;
    double r = q * q;
    return (((((a[0] * r + a[1]) * r + a[2]) * r + a[3]) * r + a[4]) * r + a[5]) * q /
           (((((b[0] * r + b[1]) * r + b[2]) * r + b[3]) * r + b[4]) * r + 1.0);
}

double FDistributionUpperTail(double f, double df1, double df2)
{
    if (!(f >= 0.0) || !(df1 > 0.0) || !(df2 > 0.0) ||
        !std::isfinite(f) || !std::isfinite(df1) || !std::isfinite(df2)) {
        return NAN;
    }
    double x = df2 / (df2 + df1 * f);
    return RegularizedIncompleteBeta(df2 / 2.0, df1 / 2.0, x);
}

double ChiSquareUpperTail(double chisq, double df)
{
    if (!(chisq >= 0.0) || !(df > 0.0) || !std::isfinite(chisq) || !std::isfinite(df)) {
        return NAN;
    }
    return RegularizedGammaQ(df / 2.0, chisq / 2.0);
}

PooledCorrelationScalar PoolCorrelationOnFisherZ(const std::vector<double> &rByImputation,
                                                 const std::vector<int> &nByImputation)
{
    PooledCorrelationScalar out;
    std::vector<double> z;
    std::vector<double> u;
    for (std::size_t i = 0; i < rByImputation.size() && i < nByImputation.size(); ++i) {
        double r = rByImputation[i];
        int n = nByImputation[i];
        if (!std::isfinite(r) || n <= 3) continue;
        r = std::max(-0.999999, std::min(0.999999, r));
        z.push_back(std::atanh(r));
        u.push_back(1.0 / (static_cast<double>(n) - 3.0));
    }
    std::size_t m = z.size();
    if (m == 0) return out;

    double qbar = 0.0, ubar = 0.0;
    for (std::size_t i = 0; i < m; ++i) {
        qbar += z[i];
        ubar += u[i];
    }
    qbar /= static_cast<double>(m);
    ubar /= static_cast<double>(m);

    double b = 0.0;
    if (m > 1) {
        for (double value : z) {
            double d = value - qbar;
            b += d * d;
        }
        b /= static_cast<double>(m - 1);
    }
    double total = ubar + (1.0 + 1.0 / static_cast<double>(m)) * b;
    if (!(total >= 0.0) || !std::isfinite(total)) return out;
    double se = std::sqrt(total);
    double riv = 0.0;
    if (std::isfinite(ubar) && ubar > 0.0) {
        riv = ((1.0 + 1.0 / static_cast<double>(m)) * b) / ubar;
    } else if (b > 0.0) {
        riv = INFINITY;
    }
    double df = INFINITY;
    if (m > 1 && std::isfinite(riv) && riv > 0.0) {
        df = (static_cast<double>(m) - 1.0) * std::pow(1.0 + 1.0 / riv, 2.0);
    }
    double statistic = (std::isfinite(se) && se > 0.0) ? qbar / se : NAN;
    double p = NAN;
    if (std::isfinite(statistic)) {
        p = std::isfinite(df) ? FDistributionUpperTail(statistic * statistic, 1.0, df)
                              : NormalTwoSidedP(statistic);
    }
    double lambda = (std::isfinite(total) && total > 0.0)
        ? ((1.0 + 1.0 / static_cast<double>(m)) * b) / total
        : NAN;
    double fmi = (std::isfinite(riv) && std::isfinite(df))
        ? (riv + 2.0 / (df + 3.0)) / (riv + 1.0)
        : lambda;
    out.valid = true;
    out.qbar = qbar;
    out.se = se;
    out.df = df;
    out.p = p;
    out.fmi = fmi;
    return out;
}

std::string CompareMeansTitle(const std::string &testType)
{
    if (testType == "one_sample_t") return "One-Sample t Test";
    if (testType == "independent_t") return "Independent-Samples t Test";
    if (testType == "paired_t") return "Paired-Samples t Test";
    if (testType == "oneway_anova") return "One-Way ANOVA";
    return "Compare Means";
}

std::string MissingDataPairwiseMethodLabel()
{
    return "pairwise";
}

std::string MissingDataListwiseMethodLabel()
{
    return "listwise";
}

std::string ActiveDatasetNoNumericVariablesStatus()
{
    return "The active dataset has no numeric variables.";
}

std::string CorrelationMatrixNeedsTwoNumericStatus()
{
    return "This plot does not have two numeric variables for a correlation matrix.";
}

std::string PCANeedsAtLeastTwoNumericStatus()
{
    return "At least two numeric variables are required.";
}

std::string QuickClusterNeedsNumericStatus()
{
    return "Quick Cluster requires at least one numeric variable.";
}

std::string OneSampleTNoNumericStatus()
{
    return "No numeric variable available.";
}

std::string IndependentTNeedGroupingStatus()
{
    return "Need one numeric and one grouping variable.";
}

std::string PairedTNeedTwoNumericStatus()
{
    return "Need at least two numeric variables.";
}

std::string OneWayAnovaNeedFactorStatus()
{
    return "Need one numeric and one factor variable.";
}

std::string RegressionModelComparisonTitle()
{
    return "Regression Model Comparison";
}

std::string MissingDataFieldLabel()
{
    return "Missing data:";
}

std::string DescriptiveStatisticsWindowTitle()
{
    return "Descriptive Statistics";
}

std::string FrequencyTableWindowTitle()
{
    return "Frequency Table";
}

std::string CompareMeansWindowTitle()
{
    return "Compare Means";
}

std::string OneSampleTTestWindowTitle()
{
    return "One-Sample t Test";
}

std::string IndependentSamplesTTestWindowTitle()
{
    return "Independent-Samples t Test";
}

std::string PairedSamplesTTestWindowTitle()
{
    return "Paired-Samples t Test";
}

std::string OneWayANOVAWindowTitle()
{
    return "One-Way ANOVA";
}

std::string QuickClusterWindowTitle()
{
    return "Quick Cluster";
}

std::string RowsUsedExcludedWindowTitle()
{
    return "Rows Used/Excluded";
}

} // namespace core
} // namespace rlispstat
