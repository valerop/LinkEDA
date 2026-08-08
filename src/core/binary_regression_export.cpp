#include "binary_regression_export.h"

#include <cmath>
#include <iomanip>
#include <sstream>
#include <utility>

namespace rlispstat {
namespace core {

std::string BinaryAPAPredictorLabel(const GeneralizedGLMRow &row)
{
    if (row.term == "(Intercept)" || row.sourceTerm == "(Intercept)") return "Intercept";
    if (!row.sourceTerm.empty() && !row.factorLevel.empty() && !row.referenceLevel.empty()) {
        return row.sourceTerm + ": " + row.factorLevel + " vs. " + row.referenceLevel;
    }
    if (!row.displayLabel.empty()) return row.displayLabel;
    return row.term;
}

BinaryAPAReportModel BuildBinaryAPAReportModel(const GeneralizedGLMState &state)
{
    BinaryAPAReportModel model;
    model.logit = state.binaryLink == BinaryLink::Logit;
    model.defaultTitle = std::string(model.logit ? "Binary Logistic Regression Predicting "
                                                : "Binary Probit Regression Predicting ") + state.response;
    model.warnings = state.warnings;
    for (const GeneralizedGLMRow &row : state.rows) {
        if (row.rowType == "factor_parent" || row.rowType == "term_parent" || row.rowType == "reference") continue;
        BinaryAPACoefficientRow output;
        output.predictor = BinaryAPAPredictorLabel(row);
        output.sourceTerm = row.sourceTerm.empty() ? row.term : row.sourceTerm;
        output.factorLevel = row.factorLevel;
        output.referenceLevel = row.referenceLevel;
        output.estimate = row.estimate;
        output.stdError = row.stdError;
        output.statistic = row.statistic;
        output.pValue = row.pValue;
        output.ciLower = row.ciLower;
        output.ciUpper = row.ciUpper;
        output.oddsRatio = row.oddsRatio;
        output.oddsRatioLower = row.oddsRatioLower;
        output.oddsRatioUpper = row.oddsRatioUpper;
        model.coefficients.push_back(std::move(output));
        if (!row.sourceTerm.empty() && !row.factorLevel.empty() && !row.referenceLevel.empty()) {
            const std::string rawName = row.sourceTerm + row.factorLevel;
            const std::string readable = BinaryAPAPredictorLabel(row);
            for (std::string &warning : model.warnings) {
                std::size_t position = 0;
                while ((position = warning.find(rawName, position)) != std::string::npos) {
                    warning.replace(position, rawName.size(), readable);
                    position += readable.size();
                }
            }
        }
    }
    for (const BinaryTermTestRow &test : state.termTests) {
        if (!test.term.empty() && test.term != "(Intercept)") model.termTests.push_back(test);
    }
    return model;
}

namespace {

std::string CSVQuoted(const std::string &value)
{
    std::string result = "\"";
    for (char ch : value) {
        if (ch == '"') result.push_back('"');
        result.push_back(ch);
    }
    return result + "\"";
}

std::string CSVNumber(double value)
{
    if (std::isnan(value)) return "";
    if (std::isinf(value)) return value > 0 ? "Inf" : "-Inf";
    std::ostringstream result;
    result << std::setprecision(17) << value;
    return result.str();
}

} // namespace

std::string BinaryRegressionCoefficientCSV(const GeneralizedGLMState &state)
{
    const bool logit = state.binaryLink == BinaryLink::Logit;
    std::ostringstream csv;
    csv << (logit
        ? "Predictor,Estimate,SE,z,p,Odds_Ratio,OR_CI_Lower,OR_CI_Upper,Reference\n"
        : "Predictor,Estimate,SE,z,p,CI_Lower,CI_Upper,Reference\n");
    for (const GeneralizedGLMRow &row : state.rows) {
        if (row.rowType == "factor_parent" || row.rowType == "term_parent") continue;
        const bool reference = row.rowType == "reference";
        std::string predictor;
        if (reference) {
            const std::string level = !row.factorLevel.empty() ? row.factorLevel
                : (!row.displayLabel.empty() ? row.displayLabel : row.term);
            predictor = (row.sourceTerm.empty() ? row.term : row.sourceTerm) + ": " + level + " (reference)";
        } else {
            predictor = BinaryAPAPredictorLabel(row);
        }
        csv << CSVQuoted(predictor) << ',';
        if (!reference) {
            csv << CSVNumber(row.estimate) << ',' << CSVNumber(row.stdError) << ','
                << CSVNumber(row.statistic) << ',' << CSVNumber(row.pValue) << ',';
            if (logit) {
                csv << CSVNumber(row.oddsRatio) << ',' << CSVNumber(row.oddsRatioLower) << ','
                    << CSVNumber(row.oddsRatioUpper) << ',';
            } else {
                csv << CSVNumber(row.ciLower) << ',' << CSVNumber(row.ciUpper) << ',';
            }
        } else {
            csv << (logit ? ",,,,,,," : ",,,,,,");
        }
        csv << (reference ? "true" : "false") << "\n";
    }
    return csv.str();
}

std::string BinaryRegressionTermTestsCSV(const GeneralizedGLMState &state)
{
    std::ostringstream csv;
    csv << "Term,LR_Chisq,df,p\n";
    for (const BinaryTermTestRow &row : state.termTests) {
        if (row.term.empty() || row.term == "(Intercept)") continue;
        csv << CSVQuoted(row.term) << ',' << CSVNumber(row.statistic) << ',' << row.df << ','
            << CSVNumber(row.pValue) << "\n";
    }
    return csv.str();
}

std::string BinaryRegressionModelFitCSV(const GeneralizedGLMState &state)
{
    std::ostringstream csv;
    csv << "Statistic,Value\n";
    const auto row = [&csv](const std::string &label, const std::string &value) {
        csv << CSVQuoted(label) << ',' << value << "\n";
    };
    row("N", std::to_string(state.n));
    row("Excluded", std::to_string(state.excluded));
    row("Events", std::to_string(state.responseCoding.eventCount));
    row("References", std::to_string(state.responseCoding.referenceCount));
    row("Log likelihood", CSVNumber(state.logLik));
    row("Null deviance", CSVNumber(state.nullDeviance));
    row("Residual deviance", CSVNumber(state.residualDeviance));
    row("Residual df", std::to_string(state.dfResidual));
    row("LR chi-square", CSVNumber(state.globalLR));
    row("Model df", std::to_string(state.dfModel));
    row("Global p", CSVNumber(state.globalP));
    row("AIC", CSVNumber(state.aic));
    row("BIC", CSVNumber(state.bic));
    row("McFadden pseudo-R2", CSVNumber(state.mcfaddenR2));
    row("Cox-Snell pseudo-R2", CSVNumber(state.coxSnellR2));
    row("Nagelkerke pseudo-R2", CSVNumber(state.nagelkerkeR2));
    row("Apparent AUC", CSVNumber(state.auc));
    row("Converged", state.converged ? "true" : "false");
    row("Iterations", std::to_string(state.iterations));
    row("Rank", std::to_string(state.rank));
    row("Parameters", std::to_string(state.parameterCount));
    return csv.str();
}

} // namespace core
} // namespace rlispstat
