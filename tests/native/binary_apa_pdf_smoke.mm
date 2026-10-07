#import <PDFKit/PDFKit.h>

#include "../../src/core/binary_regression_export.h"
#include "../../src/platform/macos/binary_regression_pdf_macos.h"

#include <cmath>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

namespace {

std::vector<std::string> Fields(const std::string &line)
{
    std::vector<std::string> fields;
    std::size_t start = 0;
    while (true) {
        const std::size_t tab = line.find('\t', start);
        fields.push_back(line.substr(start, tab == std::string::npos ? std::string::npos : tab - start));
        if (tab == std::string::npos) break;
        start = tab + 1;
    }
    return fields;
}

double Number(const std::string &value)
{
    return value == "NA" || value.empty() ? NAN : std::strtod(value.c_str(), nullptr);
}

} // namespace

int main(int argc, char **argv)
{
    if (argc < 3 || argc > 5) {
        std::cerr << "usage: binary_apa_pdf_smoke <fixture> <pdf> [csv] [all-options]\n";
        return 2;
    }
    rlispstat::core::GeneralizedGLMState state;
    state.binaryRegression = true;
    state.family = "binomial";
    state.ok = true;
    std::ifstream input(argv[1]);
    std::string line;
    while (std::getline(input, line)) {
        const std::vector<std::string> field = Fields(line);
        if (field.empty()) continue;
        if (field[0] == "META" && field.size() >= 17) {
            state.response = field[1]; state.link = field[2];
            state.binaryLink = field[2] == "probit" ? rlispstat::core::BinaryLink::Probit : rlispstat::core::BinaryLink::Logit;
            state.responseCoding.eventLabel = state.responseCoding.eventValue = field[3];
            state.responseCoding.referenceLabel = state.responseCoding.referenceValue = field[4];
            state.responseCoding.ok = true;
            state.n = std::atoi(field[5].c_str());
            state.responseCoding.eventCount = std::atoi(field[6].c_str());
            state.responseCoding.referenceCount = std::atoi(field[7].c_str());
            state.dfModel = std::atoi(field[8].c_str());
            state.globalLR = Number(field[9]); state.globalP = Number(field[10]);
            state.nagelkerkeR2 = Number(field[11]); state.logLik = Number(field[12]);
            state.residualDeviance = Number(field[13]); state.aic = Number(field[14]);
            state.bic = Number(field[15]); state.auc = Number(field[16]);
        } else if (field[0] == "ROW" && field.size() >= 17) {
            rlispstat::core::GeneralizedGLMRow row;
            row.rowType = field[1]; row.term = field[2]; row.displayLabel = field[3];
            row.sourceTerm = field[4]; row.termType = field[5]; row.factorLevel = field[6];
            row.referenceLevel = field[7]; row.estimate = Number(field[8]);
            row.stdError = Number(field[9]); row.statistic = Number(field[10]);
            row.pValue = Number(field[11]); row.ciLower = Number(field[12]);
            row.ciUpper = Number(field[13]); row.oddsRatio = Number(field[14]);
            row.oddsRatioLower = Number(field[15]); row.oddsRatioUpper = Number(field[16]);
            state.rows.push_back(row);
        } else if (field[0] == "TEST" && field.size() >= 5) {
            rlispstat::core::BinaryTermTestRow test;
            test.term = field[1]; test.df = std::atoi(field[2].c_str());
            test.statistic = Number(field[3]); test.pValue = Number(field[4]);
            state.termTests.push_back(test);
        } else if (field[0] == "WARN" && field.size() >= 2) {
            state.warnings.push_back(field[1]);
        }
    }
    if (!input.eof() || state.rows.empty()) return 3;
    rlispstat::platform::macos::BinaryAPAExportOptions options;
    options.title = rlispstat::core::BuildBinaryAPAReportModel(state).defaultTitle;
    if (argc == 5 && std::string(argv[4]) == "all-options") {
        options.includeDetailedFit = true;
    }
    std::string message;
    if (!rlispstat::platform::macos::RenderBinaryRegressionAPAPDF(state, argv[2], options, &message)) {
        std::cerr << message << "\n";
        return 4;
    }
    PDFDocument *document = [[PDFDocument alloc] initWithURL:[NSURL fileURLWithPath:[NSString stringWithUTF8String:argv[2]]]];
    NSString *pdfText = [document string];
    if (!document || [document pageCount] == 0 || !pdfText || [pdfText length] == 0) return 6;
    auto contains = [&](NSString *needle) { return [pdfText rangeOfString:needle].location != NSNotFound; };
    if (!contains(@"Predictor") || !contains(@"LR \u03c7\u00b2") || !contains(@"6 (vs 4)") ||
        contains(@"U00002014")) return 7;
    if (argc != 5) {
        if (contains(@"Global Tests of Model Terms") || contains(@"Detailed Binary Model Fit") ||
            contains(@"AIC") || contains(@"BIC") || contains(@"Apparent AUC")) return 8;
        if (state.binaryLink == rlispstat::core::BinaryLink::Logit) {
            if (!contains(@"95% CI for OR") || !contains(@"OR = odds ratio")) return 9;
        } else if (contains(@"OR = odds ratio") || !contains(@"probit scale")) return 10;
    } else if (contains(@"Global Tests of Model Terms") ||
               !contains(@"Detailed Binary Model Fit") || !contains(@"Apparent AUC")) return 11;
    if (argc >= 4) {
        std::ofstream csv(argv[3]);
        csv << rlispstat::core::BinaryRegressionCoefficientCSV(state);
        if (!csv) return 5;
    }
    return 0;
}
