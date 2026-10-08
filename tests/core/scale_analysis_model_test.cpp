#include "core/scale_analysis_model.h"
#include "core/provenance_model.h"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <string>
#include <vector>

using namespace rlispstat::core;

static DataColumn Column(std::string name, std::string type,
                         std::vector<std::string> values)
{
    DataColumn result;
    result.name = std::move(name);
    result.type = std::move(type);
    result.values = std::move(values);
    return result;
}

int main()
{
    DataFrameModel data;
    data.group = "scale_data";
    data.rows = 4;
    data.columns = {
        Column("q1", "numeric", {"1", "2", "3", "4"}),
        Column("q2", "ordered", {"low", "middle", "high", "middle"}),
        Column("binary", "factor", {"No", "Yes", "No", "Yes"}),
        Column("unordered", "factor", {"a", "b", "c", "b"}),
        Column("label", "character", {"a", "b", "c", "d"})
    };

    assert(ScaleAnalysisWindowTitle(false) == "Scale Analysis");
    assert(ScaleAnalysisWindowTitle(true) == "Scale Analysis - Multiple Imputation");
    assert(ScaleColumnIsEligible(data.columns[0]));
    assert(ScaleColumnIsEligible(data.columns[1]));
    assert(ScaleColumnIsDichotomousCategorical(data.columns[2]));
    assert(ScaleColumnIsEligible(data.columns[2]));
    assert(!ScaleColumnIsEligible(data.columns[3]));
    assert(!ScaleColumnIsEligible(data.columns[4]));
    assert(DefaultScaleItemType(data.columns[0]) == ScaleItemType::Numeric);
    assert(DefaultScaleItemType(data.columns[1]) == ScaleItemType::Ordinal);
    assert(DefaultScaleItemType(data.columns[2]) == ScaleItemType::Ordinal);
    assert(ScaleItemTypeMenuLabel(ScaleItemType::Ordinal, &data.columns[2]) == "Binary (2 categories)");
    assert(ScaleItemTypeMenuLabel(ScaleItemType::Ordinal, &data.columns[1]) == "Ordinal");

    // A fresh analysis is blank. Generic numeric compatibility is never
    // interpreted as evidence that columns form one psychometric scale.
    assert(InitialScaleItems(data, {}).empty());
    auto roleItems = InitialScaleItems(data, {
        {"q1", "scale_item"}, {"q2", "item"}, {"binary", "scale_item"}});
    assert(roleItems.size() == 3);
    assert(roleItems[0].variable == "q1" && roleItems[0].type == ScaleItemType::Numeric);
    assert(roleItems[1].variable == "q2" && roleItems[1].type == ScaleItemType::Ordinal);
    assert(roleItems[2].variable == "binary" && roleItems[2].type == ScaleItemType::Ordinal);

    ScaleAnalysisSpecification specification;
    assert(specification.computeOmega);
    specification.datasetId = data.group;
    specification.items = roleItems;
    specification.revision = 7;
    std::string error;
    assert(ValidateScaleAnalysisSpecification(data, specification, error));
    assert(!specification.fingerprint.empty());

    ScaleAnalysisResult stale;
    stale.revision = 6;
    stale.fingerprint = specification.fingerprint;
    assert(!ScaleAnalysisResultMatchesSpecification(specification, stale));
    stale.revision = 7;
    assert(ScaleAnalysisResultMatchesSpecification(specification, stale));

    ScaleAnalysisState state;
    assert(state.autoFit);
    state.id = "scale-1";
    state.specification = specification;
    state.result = stale;
    assert(!ScaleAnalysisHasCompletedResult(state.result));
    state.result.summary = "completed";
    assert(ScaleAnalysisHasCompletedResult(state.result));
    assert(ScaleAnalysisResultMayBePresented(state));
    const auto identity = BuildScaleAnalysisDerivedIdentity(
        state, ScaleAnalysisChildKind::Dimensionality);
    assert(ScaleAnalysisDerivedIdentityMatchesState(identity, state));
    state.specification.revision++;
    assert(!ScaleAnalysisDerivedIdentityMatchesState(identity, state));
    assert(!ScaleAnalysisResultMayBePresented(state));
    state.autoFit = false;
    assert(ScaleAnalysisResultMayBePresented(state));
    state.autoFit = true;
    state.specification = specification;
    state.specification.correlationBasis = "polychoric";
    state.specification.fingerprint = ScaleAnalysisSpecificationFingerprint(
        state.specification);
    assert(!ScaleAnalysisDerivedIdentityMatchesState(identity, state));
    state.specification = specification;
    state.specification.rotation = "varimax";
    state.specification.fingerprint = ScaleAnalysisSpecificationFingerprint(
        state.specification);
    assert(!ScaleAnalysisDerivedIdentityMatchesState(identity, state));
    state.specification = specification;

    ScalePlotSeriesResult stability;
    stability.kind = "factor_stability";
    // Values arrive through the native wire formatter, which deliberately
    // preserves decimal places (for example "6.000000").  Factor-count
    // stability must not disappear merely because an integer-valued result
    // has that representation.
    stability.labels = {"2.000000", "3.000000", "4.000000"};
    stability.values = {"6.000000", "7.000000", "7.000000"};
    ScalePlotSeriesResult eigen;
    eigen.kind = "scree_observed";
    eigen.labels = {"1", "2"};
    eigen.values = {"2.4", "1.1"};
    ScalePlotSeriesResult random;
    random.kind = "scree_reference";
    random.labels = {"1", "2"};
    random.values = {"1.3", "1.1"};
    state.result.plotSeries = {stability, eigen, random};
    const auto factorRows = ScaleFactorCountRows(state.result);
    assert(factorRows.size() == 3);
    assert(factorRows[0].factorCount == 2 && factorRows[0].imputations == 6);
    assert(factorRows[2].proportion == 0.35);
    const auto eigenRows = ScaleMeanEigenvalueRows(state.result);
    assert(eigenRows.size() == 2 && eigenRows[0].reference == "1.3");

    // Scale correlations use the exact same portable cell and render-plan
    // contract as Correlation Matrix, irrespective of whether their basis was
    // Pearson, polychoric, or mixed.
    state.result.scaleSummary.nUsed = "287";
    state.result.correlations.basis = "pearson";
    state.result.correlations.status = "formal Fisher-z pooling";
    state.result.correlations.variables = {"q1", "q2"};
    state.result.correlations.values = {"1", "0.42", "0.42", "1"};
    state.result.correlations.pValues = {"", "0.012", "0.012", ""};
    state.result.correlations.sampleSizes = {"287", "280", "280", "287"};
    const auto correlationCells = ScaleCorrelationCells(state.result);
    assert(correlationCells.size() == 4);
    assert(correlationCells[0].status == "diagonal");
    assert(std::abs(correlationCells[1].r - 0.42) < 1e-12);
    assert(std::abs(correlationCells[1].p - 0.012) < 1e-12);
    assert(correlationCells[1].n == 280);
    assert(correlationCells[1].detail.find("pearson") != std::string::npos);
    const auto correlationPlan = BuildScaleCorrelationRenderPlan(
        state.result, 0, 1, true, true, true);
    assert(correlationPlan.columnHeaders.size() == 2);
    assert(correlationPlan.rowHeaders.size() == 2);
    assert(correlationPlan.cells.size() == 4);
    assert(correlationPlan.cells[1].textItems.size() == 3);
    assert(correlationPlan.cells[1].textItems[0].text.find(".420*") != std::string::npos);
    assert(correlationPlan.cells[1].textItems[1].text.find("p=.012") != std::string::npos);
    assert(correlationPlan.cells[1].textItems[2].text == "N = 280");
    assert(correlationPlan.addVariableLabel.empty());

    // Polychoric and mixed matrices may still expose their pairwise sample
    // sizes, but they must not manufacture p values or significance stars.
    state.result.correlations.basis = "polychoric";
    state.result.correlations.status = "descriptive across imputations";
    state.result.correlations.pValues = {"", "", "", ""};
    const auto descriptiveCells = ScaleCorrelationCells(state.result);
    assert(!std::isfinite(descriptiveCells[1].p));
    const auto descriptivePlan = BuildScaleCorrelationRenderPlan(
        state.result, 0, 1, true, true, true);
    assert(descriptivePlan.cells[1].textItems.size() == 2);
    assert(descriptivePlan.cells[1].textItems[0].text.find("*") == std::string::npos);
    assert(descriptivePlan.cells[1].textItems[1].text == "N = 280");
    const auto correlationPublicationTables = ScaleAnalysisPublicationTables(
        state, static_cast<int>(ScaleAnalysisChildKind::InterItemCorrelations));
    assert(correlationPublicationTables.size() == 1);
    assert(correlationPublicationTables[0].title.empty());
    assert(correlationPublicationTables[0].columns.size() == 3);
    assert(correlationPublicationTables[0].columns[1].label == "q1");
    assert(correlationPublicationTables[0].rows.size() == 2);
    assert(correlationPublicationTables[0].rows[0][1].text == "1");
    assert(correlationPublicationTables[0].rows[0][2].text == "0.42");
    assert(correlationPublicationTables[0].footnotes[0] == "Correlation method: polychoric.");

    // Scale EFA results likewise flow through the common dimensionality report
    // model, retaining scale-specific MI provenance outside the shared table.
    state.result.dimensionality.status = "value";
    state.result.dimensionality.factorCount = "2";
    state.result.dimensionality.factorNames = {"MR1", "MR2"};
    ScaleLoadingResultRow q1Loading;
    q1Loading.item = "q1";
    q1Loading.loadings = {"0.81", "0.11"};
    q1Loading.communality = "0.67";
    q1Loading.uniqueness = "0.33";
    ScaleLoadingResultRow q2Loading;
    q2Loading.item = "q2";
    q2Loading.loadings = {"0.18", "0.73"};
    q2Loading.communality = "0.57";
    q2Loading.uniqueness = "0.43";
    state.result.dimensionality.loadings = {q1Loading, q2Loading};
    const auto dimensionModel = BuildScaleDimensionalityReportViewModel(
        state.result, 2, "q2");
    assert(dimensionModel.report.componentPrefix == "F");
    assert(dimensionModel.report.componentCount == 2);
    assert(dimensionModel.report.loadingRows.size() == 2);
    assert(dimensionModel.report.loadingRows[1].variable == "q2");
    assert(dimensionModel.report.loadingRows[1].highlighted);
    assert(!dimensionModel.renderPlan.texts.empty());
    assert(ScaleAnalysisChildWindowTitle(
        ScaleAnalysisChildKind::Biplot, true) ==
        "Scale Factor Biplot - Multiple Imputation");

    // MI dimensionality presentation is deliberately keyed to one completed
    // dataset.  Rotated loadings are never averaged across imputations: the
    // report, scree/parallel plot, and biplot all consume the same selected
    // imputation series.
    state.result.imputationCount = 2;
    state.result.plotSeries = {stability};
    auto addSeries = [&state](std::string kind, std::string name,
                              std::vector<std::string> labels,
                              std::vector<std::string> values) {
        ScalePlotSeriesResult series;
        series.kind = std::move(kind);
        series.name = std::move(name);
        series.labels = std::move(labels);
        series.values = std::move(values);
        state.result.plotSeries.push_back(std::move(series));
    };
    addSeries("scree_observed_imputation", "Imputation 1",
              {"1", "2", "3"}, {"2.1", "1.0", "0.4"});
    addSeries("scree_reference_imputation", "Imputation 1",
              {"1", "2", "3"}, {"1.3", "1.1", "0.8"});
    addSeries("scree_observed_imputation", "Imputation 2",
              {"1", "2", "3"}, {"2.8", "1.4", "0.3"});
    addSeries("scree_reference_imputation", "Imputation 2",
              {"1", "2", "3"}, {"1.2", "1.0", "0.7"});
    addSeries("factor_loading_imputation", "1|F1", {"q1", "q2"},
              {"0.70", "0.20"});
    addSeries("factor_loading_imputation", "1|F2", {"q1", "q2"},
              {"0.10", "0.75"});
    addSeries("factor_loading_imputation", "2|F1", {"q1", "q2"},
              {"0.82", "0.16"});
    addSeries("factor_loading_imputation", "2|F2", {"q1", "q2"},
              {"0.08", "0.79"});
    addSeries("factor_communality_imputation", "2", {"q1", "q2"},
              {"0.68", "0.65"});
    addSeries("factor_uniqueness_imputation", "2", {"q1", "q2"},
              {"0.32", "0.35"});
    addSeries("factor_score_imputation", "2|F1", {"1", "2", "3"},
              {"-0.8", "0.1", "0.9"});
    addSeries("factor_score_imputation", "2|F2", {"1", "2", "3"},
              {"0.5", "-0.3", "0.2"});

    const auto imputationTwo = BuildScaleDimensionalityPresentation(
        state.result, 2);
    assert(imputationTwo.imputation == 2);
    assert(imputationTwo.components.size() == 3);
    assert(std::abs(imputationTwo.components[0].eigenvalue - 2.8) < 1e-12);
    assert(imputationTwo.loadings.size() == 2);
    assert(imputationTwo.loadings[0].values.size() == 2);
    assert(std::abs(imputationTwo.loadings[0].values[0] - 0.82) < 1e-12);
    assert(std::abs(imputationTwo.loadings[0].communality - 0.68) < 1e-12);
    assert(imputationTwo.scores.size() == 3);
    assert(imputationTwo.scores[1].row == 2);
    assert(std::abs(imputationTwo.scores[1].values[1] + 0.3) < 1e-12);
    assert(imputationTwo.status.find("Imputation 2") != std::string::npos);
    const auto imputationTwoReport = BuildScaleDimensionalityReportViewModel(
        state.result, 1, "q1", 2);
    assert(imputationTwoReport.report.loadingRows.size() == 2);
    assert(imputationTwoReport.report.loadingRows[0].loadings[0].text == "0.820");
    const auto dimensionTables = ScaleAnalysisPublicationTables(
        state, static_cast<int>(ScaleAnalysisChildKind::Dimensionality), 2);
    assert(dimensionTables.size() == 1);
    assert(dimensionTables[0].columns.size() == 6);
    assert(dimensionTables[0].columns[0].label == "Item / statistic");
    assert(dimensionTables[0].columns[1].label == "F1");
    assert(dimensionTables[0].columns[2].label == "F2");
    assert(dimensionTables[0].columns[3].label == "F3");
    assert(dimensionTables[0].rows.size() == 7);
    assert(dimensionTables[0].rows[0][1].text == "2.800");
    assert(dimensionTables[0].rows[1][2].text == "1.000");
    assert(dimensionTables[0].rows[0][3].text == "0.300");
    assert(dimensionTables[0].rows[1][3].text == "0.700");
    assert(dimensionTables[0].rows[2][3].text == "—");
    assert(dimensionTables[0].rows[5][0].text == "  q1");
    assert(dimensionTables[0].rows[5][1].text == "0.820");
    assert(dimensionTables[0].rows[5][3].text == "—");
    assert(dimensionTables[0].rows[5][4].text == "0.680");
    assert(dimensionTables[0].footnotes[0].find("3 of 3 candidates") != std::string::npos);
    assert(std::find(dimensionTables[0].footnotes.begin(), dimensionTables[0].footnotes.end(),
        "h² = communality; u² = uniqueness.") != dimensionTables[0].footnotes.end());
    for (const auto &note : dimensionTables[0].footnotes)
        assert(note.find("psych::") == std::string::npos);
    const auto separateDimensionTables = ScaleAnalysisPublicationTables(
        state, static_cast<int>(ScaleAnalysisChildKind::Dimensionality), 2,
        ScaleAnalysisGlobalPresentation::CompactTable,
        ScaleAnalysisDimensionalityPresentation::SeparateTables);
    assert(separateDimensionTables.size() == 2);
    assert(separateDimensionTables[0].title == "Parallel analysis and variance");
    assert(separateDimensionTables[0].columns.size() == 4);
    assert(separateDimensionTables[0].columns[0].label == "Statistic");
    assert(separateDimensionTables[0].columns[1].label == "F1");
    assert(separateDimensionTables[0].columns[2].label == "F2");
    assert(separateDimensionTables[0].columns[3].label == "F3");
    assert(separateDimensionTables[0].rows.size() == 4);
    assert(separateDimensionTables[0].rows[0][1].text == "2.800");
    assert(separateDimensionTables[0].rows[1][3].text == "0.700");
    assert(separateDimensionTables[0].rows[2][3].text == "—");
    assert(separateDimensionTables[1].title == "Factor loadings");
    assert(separateDimensionTables[1].columns.size() == 5);
    assert(separateDimensionTables[1].columns[0].label == "Item");
    assert(separateDimensionTables[1].columns[1].label == "F1");
    assert(separateDimensionTables[1].columns[2].label == "F2");
    assert(separateDimensionTables[1].columns[3].label == "h²");
    assert(separateDimensionTables[1].columns[4].label == "u²");
    assert(separateDimensionTables[1].rows.size() == 2);
    assert(separateDimensionTables[1].rows[0][0].text == "q1");
    assert(separateDimensionTables[1].rows[0][1].text == "0.820");
    assert(separateDimensionTables[1].rows[0][3].text == "0.680");
    auto pcaPublicationState = state;
    pcaPublicationState.specification.dimensionalityMethod = "pca";
    const auto pcaDimensionTables = ScaleAnalysisPublicationTables(
        pcaPublicationState, static_cast<int>(ScaleAnalysisChildKind::Dimensionality), 2);
    assert(pcaDimensionTables.size() == 1 && pcaDimensionTables[0].columns[1].label == "PC1");
    const auto separatePcaTables = ScaleAnalysisPublicationTables(
        pcaPublicationState, static_cast<int>(ScaleAnalysisChildKind::Dimensionality), 2,
        ScaleAnalysisGlobalPresentation::CompactTable,
        ScaleAnalysisDimensionalityPresentation::SeparateTables);
    assert(separatePcaTables.size() == 2);
    assert(separatePcaTables[0].columns[1].label == "PC1");
    assert(separatePcaTables[1].title == "Component loadings");
    assert(separatePcaTables[1].columns[2].label == "PC2");
    const auto dimensionApaCode = BuildLatexPublicationRCode(dimensionTables, true, {});
    assert(dimensionApaCode.find("Item loadings") != std::string::npos);
    assert(dimensionApaCode.find("0.820") != std::string::npos);
    const auto imputationTwoScree = BuildScaleDimensionalityScreePlotState(
        state.result, 2);
    assert(imputationTwoScree.ok);
    assert(imputationTwoScree.observed.size() == 3);
    assert(std::abs(imputationTwoScree.observed[0].y - 2.8) < 1e-12);
    const auto imputationTwoBiplot = BuildScaleDimensionalityBiplotPlotState(
        state.result, 2);
    assert(imputationTwoBiplot.ok);
    assert(imputationTwoBiplot.scores.size() == 3);
    assert(imputationTwoBiplot.loadings.size() == 2);
    assert(imputationTwoBiplot.title.find("Imputation 2") != std::string::npos);
    const auto pcaReport = BuildScaleDimensionalityReportViewModel(
        state.result, 1, "q1", 2, "pca");
    assert(pcaReport.report.componentPrefix == "PC");
    const auto pcaScree = BuildScaleDimensionalityScreePlotState(
        state.result, 2, "pca");
    assert(pcaScree.title.find("PCA") != std::string::npos);
    const auto pcaBiplot = BuildScaleDimensionalityBiplotPlotState(
        state.result, 2, 1, 2, "pca");
    assert(pcaBiplot.xLabel.find("PC1") != std::string::npos);
    assert(ScaleAnalysisChildWindowTitle(
        ScaleAnalysisChildKind::Reliability, true) ==
        "Scale Reliability - Multiple Imputation");

    ScaleAnalysisSpecification reversed = specification;
    reversed.items[0].reversed = true;
    assert(!ValidateScaleAnalysisSpecification(data, reversed, error));
    assert(error.find("theoretical scoring range") != std::string::npos);
    reversed.items[0].hasScoringRange = true;
    reversed.items[0].scoringMinimum = 1;
    reversed.items[0].scoringMaximum = 5;
    assert(ValidateScaleAnalysisSpecification(data, reversed, error));
    assert(reversed.fingerprint != specification.fingerprint);

    const auto eligible = EligibleScaleVariables(data, roleItems);
    assert(eligible.empty());
    const auto compact = BuildScaleAnalysisWindowLayout(3);
    assert(!compact.itemListScrolls && compact.visibleItemRows == 4);
    const auto scrolling = BuildScaleAnalysisWindowLayout(20, 10);
    assert(scrolling.itemListScrolls && scrolling.visibleItemRows == 10);

    // The native renderer consumes R plot values and keeps synthetic bins
    // separate from data-row selection. MI display is bounded to five series.
    state.specification.revision = state.result.revision;
    state.specification.fingerprint = state.result.fingerprint;
    state.result.imputationCount = 20;
    for (int i=1; i<=20; ++i) {
        ScalePlotSeriesResult s; s.kind="score_distribution";
        s.name="Imputation " + std::to_string(i); s.labels={"100", "110"};
        s.values={".25", ".75"}; state.result.plotSeries.push_back(s);
    }
    PlotModel plot;
    assert(BuildScaleNativePlotModel(state, ScaleAnalysisChildKind::ScaleScoreDistributionPlot, 6, plot));
    assert(plot.interactionPlotLines.size()==5 && plot.points.size()==10);
    assert(plot.points.front().x==100 && plot.points.back().x==110);
    assert(!PlotLinksToDataRows(plot));
    assert(ImputationDiagnosticDisplayStatus(plot).find("not individual cases")!=std::string::npos);
    state.result.scaleSummary.numberOfItems = "5";
    state.result.scaleSummary.nUsed = "2709";
    state.result.scaleSummary.missingPercent = "0.7";
    state.result.scaleSummary.scaleMean = "4.217";
    state.result.scaleSummary.scaleSd = "0.737";
    state.result.scaleSummary.meanInterItemCorrelation = "0.145";
    state.result.scaleSummary.alpha = "0.431";
    state.result.scaleSummary.standardizedAlpha = "0.459";
    state.result.scaleSummary.omegaTotal = "0.642";
    state.result.items.push_back({"a1", "numeric", "forward", "2.413", "1.408", "0.6", "-0.308", "0.719"});
    const auto tables=ScaleAnalysisPublicationTables(state);
    assert(tables.size()==2 && tables[0].rows.size()==3);
    assert(tables[0].title == "Scale-level results");
    assert(tables[1].title == "Item-level results");
    assert(tables[0].rows[0][1].text == "5");
    assert(tables[0].columns.size()==6 && tables[0].stubColumns.size()==3);
    assert(tables[0].rows[0][3].text == "4.217");
    assert(tables[0].rows[2][5].text == "0.642");
    assert(tables[0].rows[1][1].text == "2709");
    assert(tables[1].rows.size() == 1 && tables[1].rows[0][0].text == "a1");
    assert(tables[1].columns.size() == 6 && tables[1].rows[0][1].text == "2.413");
    assert(tables[1].columns[1].label == "M");
    assert(tables[0].footnotes.size() == 1);
    assert(tables[1].footnotes.size() == 1); // Method note, without a repeated MI note.
    const auto noteTables=ScaleAnalysisPublicationTables(state, -1, 1,
        ScaleAnalysisGlobalPresentation::ItemTableNote);
    assert(noteTables.size()==1 && noteTables[0].rows.size()==1);
    assert(noteTables[0].footnotes.size()==3); // MI, global results, method.
    assert(noteTables[0].footnotes[1].find("N = 2709")!=std::string::npos);
    assert(noteTables[0].footnotes[1].find("omega total = 0.642")!=std::string::npos);
    auto reversedState = state;
    reversedState.result.items[0].direction = "reversed";
    const auto reversedTables = ScaleAnalysisPublicationTables(reversedState);
    assert(reversedTables[1].columns.size() == 6);
    assert(reversedTables[1].footnotes.size() == 2);
    assert(reversedTables[1].footnotes[0].find("Reverse-scored items: a1") != std::string::npos);
    const auto reversedNoteTables = ScaleAnalysisPublicationTables(reversedState, -1, 1,
        ScaleAnalysisGlobalPresentation::ItemTableNote);
    assert(reversedNoteTables.size() == 1 && reversedNoteTables[0].footnotes.size() == 4);
    assert(reversedNoteTables[0].footnotes[2].find("Reverse-scored items: a1") != std::string::npos);
    LatexPublicationOptions apaOptions;
    apaOptions.apa7 = true;
    apaOptions.tableNumber = "1";
    apaOptions.title = "Scale Analysis";
    const auto apaCode = BuildLatexPublicationRCode(tables, true, apaOptions);
    assert(apaCode.find("Scale-level results") != std::string::npos);
    assert(apaCode.find("Item-level results") != std::string::npos);
    assert(apaCode.find("2709") != std::string::npos);
    assert(apaCode.find("a1") != std::string::npos);
    const auto noteCode=BuildLatexPublicationRCode(noteTables, true, apaOptions);
    assert(noteCode.find("Scale-level results: ")!=std::string::npos);
    assert(noteCode.find("omega total = 0.642")!=std::string::npos);
    assert(!ScaleAnalysisPlotExplanation(ScaleAnalysisChildKind::ReliabilityIfDeletedPlot).empty());
    assert(!ScaleAnalysisPlotExplanation(ScaleAnalysisChildKind::Biplot).empty());
    ScaleAnalysisResult ordinary;
    ordinary.imputationCount = 1;
    ordinary.plotSeries.push_back({"dimensionality_metadata", "1",
        {"596 complete rows; 4 incomplete", "psych::fa (minres); oblimin; pearson; pairwise",
         "Parallel analysis: psych::fa.parallel, 20 simulated datasets", "value"}, {"0", "0", "0", "0"}});
    auto ordinaryReport = BuildScaleDimensionalityReportViewModel(ordinary);
    assert(ordinaryReport.report.summary == "596 complete rows; 4 incomplete");
    assert(ordinaryReport.report.calculationMethod.find("minres") != std::string::npos);
    assert(ordinaryReport.report.calculationMethod.find("factanal") == std::string::npos);
    assert(ordinaryReport.report.status.empty());
    ordinary.imputationCount = 2;
    ordinary.plotSeries[0].labels[3] = "Correlation matrix is not positive definite.";
    ordinary.dimensionality.loadings.push_back({"q1", {".5", ".4"}, ".4", ".6"});
    auto invalid = BuildScaleDimensionalityPresentation(ordinary, 1);
    assert(invalid.loadings.empty()); // Do not borrow the first valid MI solution.
    assert(invalid.scores.empty());
    assert(invalid.status.find("not positive definite") != std::string::npos);
    // A failed first imputation must not borrow the second one's scree or loads.
    ordinary.plotSeries.push_back({"scree_observed_imputation", "Imputation 2", {"1", "2"}, {"1.8", "1.1"}});
    ordinary.plotSeries.push_back({"scree_reference_imputation", "Imputation 2", {"1", "2"}, {"1.3", "1.0"}});
    assert(BuildScaleDimensionalityPresentation(ordinary, 1).components.empty());
    assert(BuildScaleDimensionalityPresentation(ordinary, 2).components.size() == 2);
    // R has explicitly retained zero. Legacy top-level loads/counts must not leak in.
    ordinary.dimensionality.factorCount = "2";
    ordinary.dimensionality.factorNames = {"F1", "F2"};
    ordinary.plotSeries[0].labels[3] = "Parallel analysis recommends 0 factors. No dimensions were extracted.";
    auto zero = BuildScaleDimensionalityReportViewModel(ordinary, 0, "", 1);
    assert(zero.report.componentCount == 0);
    assert(zero.report.loadingHeaders.empty());
    assert(zero.report.loadingRows.empty());
    assert(!zero.report.hasLoadings);
    assert(zero.report.status.find("recommends 0") != std::string::npos);
    assert(BuildScaleDimensionalityScreePlotState(ordinary, 1).title.find("2 retained") == std::string::npos);

}
