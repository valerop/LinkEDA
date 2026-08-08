#ifndef RLISPSTAT_CORE_BINARY_REGRESSION_EXPORT_H
#define RLISPSTAT_CORE_BINARY_REGRESSION_EXPORT_H

#include "glm_model.h"

#include <cmath>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

struct BinaryAPACoefficientRow {
    std::string predictor;
    std::string sourceTerm;
    std::string factorLevel;
    std::string referenceLevel;
    double estimate = NAN;
    double stdError = NAN;
    double statistic = NAN;
    double pValue = NAN;
    double ciLower = NAN;
    double ciUpper = NAN;
    double oddsRatio = NAN;
    double oddsRatioLower = NAN;
    double oddsRatioUpper = NAN;
};

struct BinaryAPAReportModel {
    bool logit = true;
    std::string defaultTitle;
    std::vector<BinaryAPACoefficientRow> coefficients;
    std::vector<BinaryTermTestRow> termTests;
    std::vector<std::string> warnings;
};

std::string BinaryAPAPredictorLabel(const GeneralizedGLMRow &row);
BinaryAPAReportModel BuildBinaryAPAReportModel(const GeneralizedGLMState &state);
std::string BinaryRegressionCoefficientCSV(const GeneralizedGLMState &state);
std::string BinaryRegressionTermTestsCSV(const GeneralizedGLMState &state);
std::string BinaryRegressionModelFitCSV(const GeneralizedGLMState &state);

} // namespace core
} // namespace rlispstat

#endif
