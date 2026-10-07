#include "../../src/core/provenance_model.h"
#include "../../src/core/command_dispatcher.h"

#include <cassert>
#include <fstream>
#include <string>
#include <vector>

using namespace rlispstat::core;

int main(int argc, char **argv)
{
    const auto version = DefaultDataVersionReference("study", "data_frame", 0, 17);
    const auto ids = StableRowIdsForCount("study", 4);
    const AnalysisScope selected = ExplicitAnalysisScope(
        "study", {2, 4}, AnalysisScopeSourceKind::DataTableRows,
        "Selected observations", 4);
    const auto scope = CaptureImmutableAnalysisScope(selected, version, ids, {2}, {4});
    assert(scope.requestedStableRowIds == std::vector<std::string>({ids[1], ids[3]}));
    assert(scope.effectiveStableRowIds == std::vector<std::string>({ids[1]}));
    assert(scope.excludedStableRowIds == std::vector<std::string>({ids[3]}));

    DataProvenance data;
    data.currentVersion = version;
    data.origin = RCodeOrigin::Unavailable;
    data.originDescription = "Native import";
    const std::string dataCode = BuildDataProvenanceRCode(data);
    assert(dataCode.find("original R code") != std::string::npos);
    assert(dataCode.find("readRDS") == std::string::npos);

    DataProvenance midsData;
    midsData.currentVersion = DefaultDataVersionReference("imputed", "multiple_imputation", 20, 3);
    midsData.origin = RCodeOrigin::Reconstructed;
    midsData.originCode = "mids_metadata <- list(m = 20L)";
    const std::string midsCode = BuildDataProvenanceRCode(midsData);
    assert(midsCode.find("# Object type: mids") != std::string::npos);
    assert(midsCode.find("# Number of imputations: 20") != std::string::npos);
    assert(midsCode.find("mids_object") != std::string::npos);
    assert(midsCode.find("display_data") != std::string::npos);
    assert(midsCode.find("data <- completed_datasets") == std::string::npos);

    AnalysisProvenance analysis;
    analysis.analysisId = "model-1";
    analysis.title = "Linear model";
    analysis.dataVersion = version;
    analysis.scope = scope;
    analysis.codeOrigin = RCodeOrigin::Recorded;
    analysis.executedRCode =
        "model <- stats::lm(score ~ group, data = analysis_data)";
    analysis.outputRCode["coefficients"] =
        "coefficient_table <- stats::coef(summary(model))";
    analysis.verificationRCode["coefficients"] =
        "analysis_data <- base::readRDS(verification_data_path)\n"
        "reference_model <- stats::lm(score ~ group, data = analysis_data)\n"
        "print(summary(reference_model))";
    analysis.verificationVariables = {"score", "group"};
    analysis.verificationWarnings = {"Prepared derived columns are reused."};
    const std::string shown = BuildAnalysisProvenanceRCode(analysis, "coefficients");
    assert(shown.find("Dataset version: 17") != std::string::npos);
    assert(shown.find("ls_select_rows_by_id") != std::string::npos);
    assert(shown.find(analysis.executedRCode) != std::string::npos);
    assert(shown.find(analysis.outputRCode["coefficients"]) != std::string::npos);
    const std::string verification =
        BuildAnalysisVerificationRCode(analysis, "coefficients");
    assert(verification.find("Reference R code generated") != std::string::npos);
    assert(verification.find("not a dump") != std::string::npos);
    assert(verification.find("base::readRDS") != std::string::npos);
    assert(verification.find("verification_data_path <- NULL") != std::string::npos);
    assert(verification.find("Save Quarto + Data...") != std::string::npos);
    assert(verification.find("Run this script from") == std::string::npos);
    assert(verification.find("Prepared derived columns are reused") != std::string::npos);
    assert(verification.find("LinkEDA:::") == std::string::npos);

    const std::string firstPath =
        "/tmp/Carpeta con espacios y á/archivo \"uno\".rds";
    std::string boundVerification;
    assert(BindAnalysisVerificationDataPath(
        verification, firstPath, boundVerification));
    const std::string firstAssignment =
        "verification_data_path <- " + ProvenanceRStringLiteral(firstPath);
    assert(boundVerification.find(firstAssignment) != std::string::npos);
    assert(boundVerification.find("verification_data_path <- NULL") == std::string::npos);
    assert(boundVerification.find("Verification data exported by LinkEDA") !=
           std::string::npos);
    assert(boundVerification.find("readRDS(verification_data_path)") !=
           std::string::npos);

    const std::string windowsPath =
        "C:\\Users\\Pedro\\Datos con espacios\\archivo \"dos\".rds";
    std::string reboundVerification;
    assert(BindAnalysisVerificationDataPath(
        boundVerification, windowsPath, reboundVerification));
    assert(reboundVerification.find(
        "verification_data_path <- " + ProvenanceRStringLiteral(windowsPath)) !=
           std::string::npos);
    assert(reboundVerification.find(firstAssignment) == std::string::npos);

    std::string unchangedVerification;
    assert(!BindAnalysisVerificationDataPath(
        reboundVerification, "", unchangedVerification));
    assert(unchangedVerification == reboundVerification);
    std::string missingMarker;
    assert(!BindAnalysisVerificationDataPath(
        "analysis_data <- data.frame(x = 1)", firstPath, missingMarker));
    assert(missingMarker == "analysis_data <- data.frame(x = 1)");

    const std::string quarto = BuildQuartoVerificationDocument(
        "Pedro's model", verification);
    assert(quarto.find("title: 'Pedro''s model'") != std::string::npos);
    assert(quarto.find("format:\n  html: default\n  pdf:\n    keep-tex: true\n  typst: default") !=
           std::string::npos);
    assert(quarto.find("```{r}") != std::string::npos);
    assert(quarto.find("#| echo: false") != std::string::npos);
    assert(quarto.find("echo: true") == std::string::npos);
    assert(quarto.find(verification) != std::string::npos);
    assert(quarto.size() >= 4 && quarto.compare(quarto.size() - 4, 4, "```\n") == 0);
    std::string boundQuarto;
    assert(BindAnalysisVerificationDataPath(quarto, firstPath, boundQuarto));
    assert(boundQuarto.find(firstAssignment) != std::string::npos);

    AnalysisProvenance allAnalysis = analysis;
    allAnalysis.scope = CaptureImmutableAnalysisScope(
        AllObservationsAnalysisScope("study", 4), version, ids);
    const std::string allShown = BuildAnalysisProvenanceRCode(allAnalysis, "coefficients");
    assert(allShown.find("analysis_data <- source_data") != std::string::npos);
    assert(allShown.find("effective_row_ids <- c(") == std::string::npos);
    assert(allShown.find(ids[0]) == std::string::npos);

    const std::string missingOutput = BuildAnalysisProvenanceRCode(allAnalysis, "missing");
    assert(missingOutput.find("# 4. Result represented by this output") != std::string::npos);
    assert(missingOutput.find("output_result <- NULL") != std::string::npos);

    PublicationTableSpec table;
    table.title = "Results";
    table.stubColumns = {0};
    table.columns = {{"term", "Term", "", -1, false},
                     {"estimate", "Estimate", "", 3, false},
                     {"unused", "Internal", "", 3, false},
                     {"p.value", "p", "", 3, true}};
    table.rows = {
        {{PublicationValueKind::Text, 0, "(Intercept)", false},
         {PublicationValueKind::Number, 1.234567, {}, false},
         {},
         {PublicationValueKind::Number, 0.01234, {}, false}},
        {{PublicationValueKind::Text, 0, "  Treatment (reference)", false},
         {},
         {},
         {PublicationValueKind::Number, 0.01234, {}, false}}
    };
    const std::string presentation = BuildTinytablePublicationRCode(table);
    const std::string officeHtml = BuildPublicationTableHtml(
        table, true, "3", "Adjusted effects");
    const std::string officeTsv = BuildPublicationTableTsv(table);
    assert(officeHtml.find("<table ") != std::string::npos);
    assert(officeHtml.find("Table 3") != std::string::npos);
    assert(officeHtml.find("Adjusted effects") != std::string::npos);
    assert(officeHtml.find("Treatment (reference)") != std::string::npos);
    assert(officeHtml.find("Internal") == std::string::npos);
    assert(officeTsv.find("Term\tEstimate\tp\n") == 0);
    assert(officeTsv.find("1.235") != std::string::npos);
    const std::string latex = BuildLatexPublicationRCode(table, true);
    assert(presentation.find("tinytable::tt") != std::string::npos);
    assert(presentation.find("notes = NULL") != std::string::npos);
    assert(presentation.find("escape = TRUE") != std::string::npos);
    assert(presentation.find("`unused`") == std::string::npos);
    assert(presentation.find("Internal") == std::string::npos);
    assert(presentation.find("publication_indent_rows") != std::string::npos);
    assert(presentation.find("indent = 1") != std::string::npos);
    assert(presentation.find("replace = TRUE") != std::string::npos);
    assert(presentation.find("width = c(3, 1, 1)") != std::string::npos);
    assert(presentation.find("print(publication_table)") != std::string::npos);
    const std::string finalDisplay = "print(publication_table)\n";
    assert(presentation.size() >= finalDisplay.size());
    assert(presentation.compare(
        presentation.size() - finalDisplay.size(), finalDisplay.size(),
        finalDisplay) == 0);
    assert(presentation.find("gt::") == std::string::npos);
    assert(presentation.find("1.234567") != std::string::npos);
    assert(latex.find("tinytable::save_tt") != std::string::npos);
    assert(latex.find("gt::") == std::string::npos);
    assert(latex.find("system2") != std::string::npos);
    assert(latex.find("webshot") == std::string::npos);
    assert(latex.find("chrom") == std::string::npos);
    assert(PublicationTitleWithoutTableLabel("Table 1. Descriptive statistics") ==
           "Descriptive statistics");
    assert(PublicationTitleWithoutTableLabel("Table 12: Adjusted effects") ==
           "Adjusted effects");
    assert(PublicationTitleWithoutTableLabel("Estimated marginal means") ==
           "Estimated marginal means");
    LatexPublicationOptions apaTitleOptions;
    apaTitleOptions.apa7 = true;
    apaTitleOptions.tableNumber = "2";
    apaTitleOptions.title = "Table 1. Descriptive statistics";
    const std::string cleanApa = BuildLatexPublicationRCode({table}, true, apaTitleOptions);
    assert(cleanApa.find("Table 2") != std::string::npos);
    assert(cleanApa.find("Table 1. Descriptive statistics") == std::string::npos);
    assert(cleanApa.find("theme = \"striped\"") == std::string::npos);

    table.footnotes = {"First note", "Second note"};
    assert(BuildLatexPublicationRCode(table, false).find(
        "notes = list(\"First note. Second note\")") != std::string::npos);
    const std::string notedPresentation = BuildTinytablePublicationRCode(table);
    assert(notedPresentation.find("notes = list(\"First note\", \"Second note\")") !=
           std::string::npos);
    if (argc > 1) {
        std::ofstream script(argv[1]);
        script << presentation;
    }
    if (argc > 2) {
        std::ofstream script(argv[2]);
        script << latex;
    }
    if (argc > 3) {
        std::ofstream document(argv[3]);
        document << BuildQuartoVerificationDocument(
            "LinkEDA Quarto verification check", presentation);
    }

    PublicationPlotSpec plot;
    plot.title = "Estimated means";
    plot.xLabel = "Time";
    plot.yLabel = "Mean";
    plot.theme = "minimal";
    plot.xCategoryOrder = {"Before", "After"};
    plot.showConfidenceIntervals = true;
    plot.series = {{"control", "Control", {1, 2}, {3, 4}, {2.5, 3.5}, {3.5, 4.5}}};
    const std::string ggplot = BuildGgplot2PublicationRCode(plot);
    assert(ggplot.find("ggplot2::ggplot") != std::string::npos);
    assert(ggplot.find("requireNamespace(\"ggplot2\"") != std::string::npos);
    assert(ggplot.find("geom_errorbar") != std::string::npos);
    assert(ggplot.find("geom_ribbon") == std::string::npos);
    assert(ggplot.find("position_dodge") != std::string::npos);
    assert(ggplot.find("x_display") != std::string::npos);
    assert(ggplot.find("c(\"Before\", \"After\")") != std::string::npos);
    assert(ggplot.find("statistical values were computed") != std::string::npos);
    assert(ggplot.find("linkeda_plot_theme <- ggplot2::theme_minimal()") !=
           std::string::npos);
    plot.theme = "publication";
    plot.legendTitle = "Group";
    const std::string publicationGgplot = BuildGgplot2PublicationRCode(plot);
    assert(publicationGgplot.find("linkeda_plot_theme <- ggplot2::theme_classic") !=
           std::string::npos);
    assert(publicationGgplot.find("panel.grid.minor = ggplot2::element_blank()") !=
           std::string::npos);
    assert(publicationGgplot.find("colour = \"Group\", fill = \"Group\"") !=
           std::string::npos);
    assert(Ggplot2ThemeRExpression("publication") != Ggplot2ThemeRExpression("bw"));

    PublicationPlotSpec unavailableThemePlot = plot;
    unavailableThemePlot.theme = "manet";
    const std::string fallbackThemeGgplot =
        BuildGgplot2PublicationRCode(unavailableThemePlot);
    assert(fallbackThemeGgplot.find(
        "linkeda_plot_theme <- ggplot2::theme_bw()") != std::string::npos);
    assert(Ggplot2ThemeRExpression("cowplot").find(
        "else ggplot2::theme_bw()") != std::string::npos);

    PublicationPlotSpec continuousPlot = plot;
    continuousPlot.xCategoryOrder.clear();
    const std::string continuousGgplot =
        BuildGgplot2PublicationRCode(continuousPlot);
    assert(continuousGgplot.find("geom_ribbon") != std::string::npos);
    assert(continuousGgplot.find("geom_errorbar") == std::string::npos);

    PublicationPlotSpec scatterPlot;
    scatterPlot.kind = "scatter";
    scatterPlot.title = "Scatter";
    scatterPlot.xLabel = "x";
    scatterPlot.yLabel = "y";
    scatterPlot.series = {{"all", "All", {1, 2}, {3, 4}, {}, {}}};
    const std::string scatterGgplot = BuildGgplot2PublicationRCode(scatterPlot);
    assert(scatterGgplot.find("geom_point") != std::string::npos);
    assert(scatterGgplot.find("colour = series") == std::string::npos);

    PublicationPlotSpec barPlot;
    barPlot.kind = "barplot";
    barPlot.title = "Bar";
    barPlot.xLabel = "Group";
    barPlot.yLabel = "% within X";
    barPlot.xCategoryOrder = {"A", "B"};
    barPlot.series = {
        {"one", "One", {1, 2}, {60, 40}, {}, {}},
        {"two", "Two", {1, 2}, {40, 60}, {}, {}}
    };
    const std::string barGgplot = BuildGgplot2PublicationRCode(barPlot);
    assert(barGgplot.find("geom_col(position = \"stack\"") != std::string::npos);
    assert(barGgplot.find("fill = series") != std::string::npos);

    PublicationPlotSpec histogramPlot;
    histogramPlot.kind = "histogram_density";
    histogramPlot.title = "Histogram";
    histogramPlot.xLabel = "Score";
    histogramPlot.yLabel = "Count";
    histogramPlot.showAxisTickMarks = false;
    histogramPlot.showAxisTickLabels = false;
    histogramPlot.series = {
        {"histogram", "Score", {0.5, 1.5}, {2, 3}, {0, 1}, {1, 2}},
        {"density_source", "Density", {0.2, 0.7, 1.4}, {0, 0, 0}, {}, {}}
    };
    const std::string histogramGgplot =
        BuildGgplot2PublicationRCode(histogramPlot);
    assert(histogramGgplot.find("geom_rect") != std::string::npos);
    assert(histogramGgplot.find("geom_density") != std::string::npos);
    assert(histogramGgplot.find("after_stat(count * bin_width)") !=
           std::string::npos);
    assert(histogramGgplot.find("axis.ticks = ggplot2::element_blank()") !=
           std::string::npos);
    assert(histogramGgplot.find("axis.text = ggplot2::element_blank()") !=
           std::string::npos);

    PublicationPlotSpec boxPlot;
    boxPlot.kind = "boxplot_violin";
    boxPlot.title = "Distribution";
    boxPlot.xLabel = "Group";
    boxPlot.yLabel = "Score";
    boxPlot.xCategoryOrder = {"A", "B"};
    boxPlot.showPoints = true;
    boxPlot.series = {
        {"a", "A", {1, 1}, {2, 3}, {}, {}},
        {"b", "B", {2, 2}, {4, 5}, {}, {}}
    };
    const std::string boxGgplot = BuildGgplot2PublicationRCode(boxPlot);
    assert(boxGgplot.find("geom_violin") != std::string::npos);
    assert(boxGgplot.find("geom_boxplot") != std::string::npos);
    assert(boxGgplot.find("geom_jitter") != std::string::npos);

    if (argc > 4) {
        std::ofstream plotScripts(argv[4]);
        plotScripts << "local({\n" << ggplot << "})\n"
                    << "local({\n" << continuousGgplot << "})\n"
                    << "local({\n" << scatterGgplot << "})\n"
                    << "local({\n" << barGgplot << "})\n"
                    << "local({\n" << histogramGgplot << "})\n"
                    << "local({\n" << boxGgplot << "})\n";
    }

    std::vector<std::string> payload = {
        "ANALYSIS_PROVENANCE_V1", "model-1", "Título", "study", "17",
        "data.frame", "0", "recorded",
        "6d6f64656c203c2d2073746174733a3a6c6d2879207e203129", "1",
        "coefficients", "73756d6d617279286d6f64656c29"};
    std::size_t cursor = 0;
    AnalysisProvenance decoded;
    std::string error;
    assert(ReadAnalysisProvenancePayload(payload, cursor, decoded, &error));
    assert(decoded.executedRCode == "model <- stats::lm(y ~ 1)");
    assert(decoded.outputRCode.at("coefficients") == "summary(model)");
    assert(cursor == payload.size());

    std::vector<std::string> payloadV2 = {
        "ANALYSIS_PROVENANCE_V2", "model-2", "Reference model", "study", "17",
        "data.frame", "0", "recorded", "78", "0",
        "1", "y", "1", "model", "78", "1", "Prepared columns only"};
    cursor = 0;
    AnalysisProvenance decodedV2;
    assert(ReadAnalysisProvenancePayload(payloadV2, cursor, decodedV2, &error));
    assert(decodedV2.executedRCode == "x");
    assert(decodedV2.verificationVariables == std::vector<std::string>({"y"}));
    assert(decodedV2.verificationRCode.at("model") == "x");
    assert(decodedV2.verificationWarnings ==
           std::vector<std::string>({"Prepared columns only"}));
    assert(cursor == payloadV2.size());

    auto diagnosticsPayload=payloadV2;
    diagnosticsPayload.insert(diagnosticsPayload.end(),{
        "MI_DIAGNOSTICS_V1","/tmp/frozen-mi-input.rds","3","Variable","FMI","RIV","1","x","0.25","0.4"});
    cursor=0;
    AnalysisProvenance diagnostics;
    assert(ReadAnalysisProvenancePayload(diagnosticsPayload,cursor,diagnostics,&error));
    assert(cursor==diagnosticsPayload.size());
    assert(diagnostics.preparedDataPath=="/tmp/frozen-mi-input.rds");
    assert(diagnostics.missingInformationColumns==std::vector<std::string>({"Variable","FMI","RIV"}));
    assert(diagnostics.missingInformationRows[0][1]=="0.25");
    auto hierarchyPayload=diagnosticsPayload;
    hierarchyPayload.insert(hierarchyPayload.end(),{"MI_DIAGNOSTIC_ROWS_V1","3",
        "-1","factor_parent","sexo","sexo","-1","reference","Hombre (reference)","sexo",
        "0","factor_level","Mujer","sexo"});
    cursor=0;
    assert(ReadAnalysisProvenancePayload(hierarchyPayload,cursor,diagnostics,&error));
    assert(cursor==hierarchyPayload.size() && diagnostics.missingInformationDisplayRows.size()==3);
    assert(diagnostics.missingInformationDisplayRows[2].valueRow==0);
    assert(diagnostics.missingInformationDisplayRows[2].label=="Mujer");
    hierarchyPayload.pop_back();cursor=0;
    assert(!ReadAnalysisProvenancePayload(hierarchyPayload,cursor,diagnostics,&error));
    diagnosticsPayload.pop_back();cursor=0;
    assert(!ReadAnalysisProvenancePayload(diagnosticsPayload,cursor,diagnostics,&error));

    ApplicationState application;
    DataFrameModel descriptiveData;
    descriptiveData.group = "descriptive-source";
    descriptiveData.rows = 3;
    descriptiveData.stableRowIds = StableRowIdsForCount(descriptiveData.group, 3);
    descriptiveData.provenance.currentVersion = DefaultDataVersionReference(
        descriptiveData.group, "data_frame", 0, 1);
    descriptiveData.columns = {
        {"x", "numeric", "", "", -1, {"1", "2", "3"}},
        {"y", "numeric", "", "", -1, {"3", "2", "1"}},
        {"group", "factor", "", "", -1, {"A", "A", "B"}}
    };
    assert(application.registerDataset(descriptiveData));

    CorrelationMatrixState correlation;
    correlation.id = "correlation-output";
    correlation.group = descriptiveData.group;
    correlation.title = "Pearson Correlation Matrix";
    correlation.variables = {"x", "y"};
    correlation.cells = {
        {"x", "x", 1.0}, {"y", "x", -1.0},
        {"x", "y", -1.0}, {"y", "y", 1.0}
    };
    RefreshCorrelationCodeReference(application, correlation);
    assert(!application.outputCodeReference(correlation.id));
    correlation.provenance.analysisId=correlation.id;
    correlation.provenance.executedRCode="stats::cor.test(x,y)";
    correlation.provenance.verificationRCode["table"]="stats::cor.test(x,y)";
    RefreshCorrelationCodeReference(application, correlation);
    const OutputCodeReference *correlationCode =
        application.outputCodeReference(correlation.id);
    assert(correlationCode);
    assert(correlationCode->provenance.verificationRCode.at("table").find(
        "stats::cor.test") != std::string::npos);

    DendrogramState dendrogram;
    dendrogram.id = "cluster-output";
    dendrogram.group = descriptiveData.group;
    dendrogram.variables = {"x", "y"};
    // Provenance must come from the accepted R result, not mutable UI state.
    RefreshDendrogramCodeReference(application, dendrogram);
    assert(!application.outputCodeReference(dendrogram.id));
    dendrogram.provenance.executedRCode = "cluster <- stats::hclust(stats::dist(base::scale(x)))";
    dendrogram.provenance.verificationRCode["plot"] = dendrogram.provenance.executedRCode;
    RefreshDendrogramCodeReference(application, dendrogram);
    const OutputCodeReference *clusterCode =
        application.outputCodeReference(dendrogram.id);
    assert(clusterCode);
    assert(clusterCode->provenance.verificationRCode.at("plot").find(
        "stats::hclust") != std::string::npos);

    Table1DisplayState contingency;
    contingency.id = "contingency-output";
    contingency.datasetId = descriptiveData.group;
    contingency.title = "Contingency Table";
    contingency.tableType = "nested_contingency";
    contingency.variables = {"group"};
    contingency.groupVariable = "group";
    RefreshNativeTableCodeReference(application, contingency);
    const OutputCodeReference *contingencyCode =
        application.outputCodeReference(contingency.id);
    // A pending native specification must not manufacture an R calculation or
    // reconstruct a verification recipe from mutable table state.
    assert(!contingencyCode);
    contingency.codeReference.outputId=contingency.id;
    contingency.codeReference.provenance.executedRCode="counts <- base::table(x, y)";
    contingency.codeReference.provenance.verificationRCode["table"]="base::table(x, y)";
    RefreshNativeTableCodeReference(application,contingency);
    contingencyCode=application.outputCodeReference(contingency.id);
    assert(contingencyCode && contingencyCode->provenance.executedRCode=="counts <- base::table(x, y)");
    return 0;
}
