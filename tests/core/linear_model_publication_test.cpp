#include "../../src/core/command_dispatcher.h"
#include <cassert>
#include <fstream>
#include <cmath>
using namespace rlispstat::core;
int main(int argc, char **argv) {
    GroupModelState state;
    state.group = "study"; state.response = "protective_response_after";
    state.multipleImputation = true; state.note = "Pooled across 20 imputations.";
    GLMFitSummary fit; fit.ok = true; fit.n = 286; fit.dfResidual = 282;
    fit.r2 = .144; fit.adjR2 = .135; fit.sigma = 4.318;
    fit.globalF = 14.362; fit.globalP = .0001;
    auto add = [&](const char *label, const char *type, double b, double se, double t, double p) {
        GLMCoefficientRow row; row.term = row.displayLabel = label;
        row.rowType = type; row.termType = row.rowType == "factor_parent" ? "factor" : "numeric"; row.estimate = b; row.stdError = se; row.tValue = t; row.pValue = p;
        fit.coefficients.push_back(row);
    };
    add("(Intercept)", "coefficient", 5.4326, .8669, 6.267, .0001);
    add("group", "factor_parent", NAN, NAN, NAN, .261);
    fit.coefficients.back().deltaR2 = .004;
    add("Control", "reference", NAN, NAN, NAN, NAN);
    add("Treatment", "factor_level", -.611, .5426, -1.126, .261);
    fit.coefficients.back().partialR = -.073;
    add("genero2_after", "factor_parent", NAN, NAN, NAN, .0001);
    fit.coefficients.back().deltaR2 = .045;
    add("Male", "reference", NAN, NAN, NAN, NAN);
    add("Female", "factor_level", 1.9744, .5313, 3.716, .0001);
    fit.coefficients.back().partialR = .228;
    add("protective_response_before", "coefficient", .3234, .0620, 5.217, .0001);
    fit.coefficients.back().standardizedBeta = .316;
    fit.coefficients.back().partialR = .342; fit.coefficients.back().deltaR2 = .095;
    const auto output = LinearModelCodeReference(state, fit);
    const auto &table = *output.publication.table;
    assert(table.columns.size() == 8);
    assert(table.subtitle.find(state.response) != std::string::npos);
    assert(table.rows[2][0].text == "  Control (reference)");
    assert(table.rows[3][1].number == -.611);
    assert(table.footnotes.front().find("D1/Wald = 14.362") != std::string::npos);
    assert(table.footnotes.front().find("N = 286") != std::string::npos);
    state.multipleImputation = false;
    assert(LinearModelCodeReference(state, fit).publication.table->footnotes.front().find("; F = ") != std::string::npos);
    fit.dfModel=3; fit.ssRegression=120;fit.msRegression=40;fit.ssResidual=5640;fit.msResidual=20;
    const auto displayed = LinearModelReportTables(state,fit);
    assert(displayed.size()==3);
    assert(displayed[0].title.find("Global fit")!=std::string::npos);
    assert(displayed[1].rows[0][0].text=="Regression");
    assert(displayed[1].rows[1][0].text=="Residual");
    assert(displayed[2].columns.size()==9);
    assert(displayed[2].rows[3][2].number==table.rows[3][1].number);
    LatexPublicationOptions normalOptions; normalOptions.reportHeadings=true;
    auto normalCode=BuildLatexPublicationRCode(displayed,true,normalOptions);
    LatexPublicationOptions apaOptions;apaOptions.apa7=true;apaOptions.tableNumber="7";apaOptions.title="Treatment effects after adjustment";
    auto apaTable=table;apaTable.title.clear();
    auto apaCode=BuildLatexPublicationRCode({apaTable},true,apaOptions);
    assert(normalCode.find("theme = \"striped\"")==std::string::npos);
    assert(normalCode.find("standalone")!=std::string::npos);
    assert(normalCode.find("pagecolor{white}")!=std::string::npos);
    assert(apaCode.find("pagecolor{white}")!=std::string::npos);
    assert(apaCode.find("standalone")==std::string::npos);
    assert(apaCode.find("theme = \"empty\"")!=std::string::npos);
    assert(apaCode.find("Table 7")!=std::string::npos);
    assert(apaCode.find(apaOptions.title)!=std::string::npos);
    for (auto type : {StatisticalModelType::Binary, StatisticalModelType::Count,
         StatisticalModelType::PositiveContinuous, StatisticalModelType::Proportion,
         StatisticalModelType::LegacyGeneralized}) {
        auto distributions = DistributionSpecificationsForModel(type);
        const auto inference = InferenceOptionSpecificationsForModel(type);
        distributions.insert(distributions.end(), inference.begin(), inference.end());
        for (const auto &dist : distributions) for (bool mi : {false, true}) {
            GeneralizedGLMState model; model.modelType=type; model.family=dist.id;
            model.link=dist.defaultLink; model.response="outcome"; model.n=120;
            model.binaryRegression=type==StatisticalModelType::Binary;
            model.countRegression=type==StatisticalModelType::Count;
            if(model.countRegression) ParseCountDistribution(dist.id,model.countDistribution);
            model.theta=4;model.betaBinomialDispersion=.2;model.dispersion=1;
            model.multipleImputation=mi; model.statisticName=mi ? "t" : "z";
            GeneralizedGLMRow row; row.term="treatment";row.estimate=.3;row.stdError=.1;
            row.statistic=3;row.pValue=.003;row.ciLower=.1;row.ciUpper=.5;
            row.exponentiatedEstimate=1.35;row.exponentiatedLower=1.10;row.exponentiatedUpper=1.65;
            model.rows={row};
            auto report=GeneralizedModelReportTables(model);
            assert(report.size()==2 && report.front().title=="Model fit");
            assert(report.back().columns[1].label=="Type");
            assert(report.back().rows.front()[2].number==.3);
            const auto reportCode=BuildLatexPublicationRCode(report,true,normalOptions);
            assert(reportCode.find("standalone")!=std::string::npos);
            assert(reportCode.find("striped")==std::string::npos);
            auto publication=GeneralizedModelPublicationTable(model);
            assert(publication.rows.front()[1].number==.3);
            assert(publication.subtitle.find("N = 120")!=std::string::npos);
            for(const auto &col:publication.columns) assert(col.key!="type");
            publication.title.clear();
            publication.footnotes.insert(publication.footnotes.begin(),publication.subtitle);
            const auto recipe=BuildLatexPublicationRCode({publication},true,apaOptions);
            assert(recipe.find("Table 7")!=std::string::npos);
            assert(recipe.find("shipout/background")!=std::string::npos);
            assert(recipe.find("theme = \"striped\"")==std::string::npos);
            if(argc>4 && !mi) std::ofstream(std::string(argv[4])+"/"+StatisticalModelTypeId(type)+"-"+dist.id+".R")<<recipe;
        }
    }
    GeneralizedGLMState omnibus;
    omnibus.multipleImputation = true;
    omnibus.statisticName = "t";
    omnibus.response = "event";
    omnibus.family = "binomial";
    omnibus.link = "logit";
    omnibus.binaryRegression = true;
    GeneralizedGLMRow parent; parent.term = parent.sourceTerm = "group";
    parent.rowType = "factor_parent";
    omnibus.rows.push_back(parent);
    GlobalTermTestRow global; global.term = "group";
    global.method = "Rubin Wald chi-square"; global.statistic = 3.373;
    global.pValue = .066; omnibus.termTests.push_back(global);
    auto omnibusAPA = GeneralizedModelPublicationTable(omnibus);
    auto omnibusReport = GeneralizedModelReportTables(omnibus);
    assert(omnibusAPA.rows.front()[3].text.find("†") != std::string::npos);
    assert(omnibusReport.back().rows.front()[4].text.find("†") != std::string::npos);
    bool hasMarkedFootnote = false;
    for (const auto &note : omnibusAPA.footnotes)
        hasMarkedFootnote = hasMarkedFootnote || note.find("†") != std::string::npos;
    assert(hasMarkedFootnote);
    for (const auto &note : omnibusAPA.footnotes)
        assert(note.find("Missing-information diagnostics") == std::string::npos);
    if(argc>3){std::ofstream(argv[2])<<normalCode;std::ofstream(argv[3])<<apaCode;}
    const auto code = BuildPublicationRCode(output, PublicationBackend::LatexPdf);
    assert(code.find("landscape") != std::string::npos);
    assert(code.find("Type") == std::string::npos);
    assert(code.find("align = \"r\"") != std::string::npos);
    assert(code.find("usepackage{xcolor}") != std::string::npos);
    assert(code.find("xelatex") != std::string::npos);
    if (argc > 1) std::ofstream(argv[1]) << code;
}
