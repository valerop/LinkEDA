#include "../../src/core/dimensionality_model.h"

#include <cassert>
#include <cmath>
#include <limits>
#include <string>
#include <vector>

using rlispstat::core::ApplyDimensionalityOrthomaxRotation;
using rlispstat::core::DimensionalityComponentPrefix;
using rlispstat::core::DimensionalityFitInput;
using rlispstat::core::DimensionalityFitResult;
using rlispstat::core::DimensionalityBiplotDimensionsMenuTitle;
using rlispstat::core::DimensionalityBiplotRequiresTwoComponentsStatus;
using rlispstat::core::DimensionalityAddVariableMenuTitle;
using rlispstat::core::DimensionalityComponentMenuLabel;
using rlispstat::core::DimensionalityNoMoreNumericVariablesTitle;
using rlispstat::core::DimensionalityNoScoresStatus;
using rlispstat::core::DimensionalityNoReplacementVariablesTitle;
using rlispstat::core::DimensionalityParallelEigenvalues;
using rlispstat::core::DimensionalityRotationIsValid;
using rlispstat::core::DimensionalityScoresSavedStatus;
using rlispstat::core::DimensionalityScopeIsValid;
using rlispstat::core::DimensionalityFitSignature;
using rlispstat::core::DimensionalityScreePlotTitle;
using rlispstat::core::DimensionalityScreePlotState;
using rlispstat::core::DimensionalitySourceSheetUnavailableStatus;
using rlispstat::core::DimensionalityReportState;
using rlispstat::core::DimensionalityVariableAddedStatus;
using rlispstat::core::DimensionalityVariableAlreadyIncludedStatus;
using rlispstat::core::DimensionalityVariableRemovedStatus;
using rlispstat::core::DimensionalityRemoveVariableTitle;
using rlispstat::core::DimensionalityVariableReplacedStatus;
using rlispstat::core::DimensionalityVariableUnavailableStatus;
using rlispstat::core::DimensionalityVariablesAfterAdd;
using rlispstat::core::DimensionalityVariablesAfterRemove;
using rlispstat::core::DimensionalityVariablesAfterReplace;
using rlispstat::core::DimensionalityVariablesAvailableToAdd;
using rlispstat::core::DimensionalityVariablesAvailableToReplace;
using rlispstat::core::BuildDimensionalityScreePlotState;
using rlispstat::core::BuildDimensionalityBiplotPlotState;
using rlispstat::core::BuildDimensionalityScreeRenderPlan;
using rlispstat::core::BuildDimensionalityBiplotRenderPlan;
using rlispstat::core::DimensionalityBiplotPlotState;
using rlispstat::core::DimensionalityBiplotRenderPlan;
using rlispstat::core::BuildDimensionalityScoreExportPlan;
using rlispstat::core::BuildDimensionalityReportState;
using rlispstat::core::BuildDimensionalityReportLayout;
using rlispstat::core::BuildDimensionalityReportRenderPlan;
using rlispstat::core::BuildDimensionalityReportViewModel;
using rlispstat::core::BuildDimensionalityControlState;
using rlispstat::core::BuildDimensionalityAddVariableMenuState;
using rlispstat::core::BuildDimensionalityVariableMenuState;
using rlispstat::core::BuildDimensionalityAnalysisMenuState;
using rlispstat::core::BuildDimensionalityScreeContextMenuState;
using rlispstat::core::BuildDimensionalityWindowLayout;
using rlispstat::core::DimensionalityControlState;
using rlispstat::core::DimensionalityAddVariableMenuState;
using rlispstat::core::DimensionalityAnalysisMenuState;
using rlispstat::core::DimensionalityWindowLayout;
using rlispstat::core::DimensionalityReportLayout;
using rlispstat::core::DimensionalityReportAction;
using rlispstat::core::DimensionalityReportActionKind;
using rlispstat::core::DimensionalityReportContextActionForHit;
using rlispstat::core::DimensionalityReportHit;
using rlispstat::core::DimensionalityReportHitKind;
using rlispstat::core::DimensionalityReportHighlightRole;
using rlispstat::core::DimensionalityReportPrimaryActionForHit;
using rlispstat::core::DimensionalityReportRectForComponent;
using rlispstat::core::DimensionalityReportFocusRect;
using rlispstat::core::DimensionalityReportRectForVariable;
using rlispstat::core::DimensionalityReportRectForVariableIndex;
using rlispstat::core::DimensionalityReportRenderPlan;
using rlispstat::core::DimensionalityReportViewModel;
using rlispstat::core::DimensionalityReportTextRole;
using rlispstat::core::DimensionalityReplaceVariableMenuTitle;
using rlispstat::core::DimensionalityMethodForPopupIndex;
using rlispstat::core::DimensionalityMethodPopupIndex;
using rlispstat::core::DimensionalityRotationForPopupIndex;
using rlispstat::core::DimensionalityRotationPopupIndex;
using rlispstat::core::DimensionalityScopeForPopupIndex;
using rlispstat::core::DimensionalityScopePopupIndex;
using rlispstat::core::DimensionalityScoreExportPlan;
using rlispstat::core::DimensionalityScreeContextMenuState;
using rlispstat::core::DimensionalityScreeRenderPlan;
using rlispstat::core::DimensionalityShowVariableInformationTitle;
using rlispstat::core::DimensionalityVariableMenuTitle;
using rlispstat::core::DimensionalityVariableMenuState;
using rlispstat::core::DimensionalityVariableUpdateResult;
using rlispstat::core::DataViewport;
using rlispstat::core::FitDimensionality;
using rlispstat::core::HitTestDimensionalityReport;
using rlispstat::core::JacobiEigenSymmetric;
using rlispstat::core::Point;
using rlispstat::core::Rect;

static bool closeEnough(double a, double b, double tolerance = 1.0e-6)
{
    return std::fabs(a - b) < tolerance;
}

static double rowNorm2(const std::vector<double> &values)
{
    double total = 0.0;
    for (double value : values) total += value * value;
    return total;
}

int main()
{
    assert(DimensionalityComponentPrefix("pca") == "PC");
    assert(DimensionalityComponentPrefix("factor") == "F");
    assert(DimensionalityComponentPrefix("other") == "PC");
    assert(DimensionalityVariableUnavailableStatus("x") ==
           "Status: `x` is not available as a numeric, ordinal, or binary variable.");
    assert(DimensionalityVariableAlreadyIncludedStatus("x") ==
           "Status: `x` is already in the analysis.");
    assert(DimensionalityVariableAddedStatus("x") == "Added variable `x`.");
    assert(DimensionalityVariableReplacedStatus("x", "y") == "Replaced `x` with `y`.");
    assert(DimensionalityVariableRemovedStatus("x") == "Removed variable `x`.");
    assert(DimensionalityNoMoreNumericVariablesTitle() == "No more eligible variables");
    assert(DimensionalityNoReplacementVariablesTitle() == "No replacement variables");
    assert(DimensionalityAddVariableMenuTitle() == "Add variable");
    assert(DimensionalityVariableMenuTitle() == "Variable");
    assert(DimensionalityReplaceVariableMenuTitle() == "Replace with");
    assert(DimensionalityShowVariableInformationTitle() == "Show Variable Information");
    assert(DimensionalityRemoveVariableTitle("x") == "Remove x");
    assert(DimensionalityBiplotDimensionsMenuTitle() == "Dimensions");
    assert(DimensionalityComponentMenuLabel("pca", 1, 0.4231) == "PC1 (42.3%)");
    assert(DimensionalityComponentMenuLabel("factor", 2, 0.157) == "F2 (15.7%)");
    assert(DimensionalityComponentMenuLabel("pca", 3, std::numeric_limits<double>::quiet_NaN()) == "PC3");
    assert(DimensionalityBiplotRequiresTwoComponentsStatus() ==
           "Status: at least two components are required for a biplot.");
    assert(DimensionalityNoScoresStatus() == "Status: no scores are available to save.");
    assert(DimensionalitySourceSheetUnavailableStatus() ==
           "Status: the source data sheet is not available.");
    assert(DimensionalityScoresSavedStatus(1) == "Status: saved 1 score column.");
    assert(DimensionalityScoresSavedStatus(2) == "Status: saved 2 score columns.");
    assert(DimensionalityScreePlotTitle("pca", 2, "none") ==
           "Scree plot - PCA (2 retained components)");
    assert(DimensionalityScreePlotTitle("factor", 1, "varimax") ==
           "Scree plot - Factor analysis (1 retained factor), varimax rotation");
    assert(DimensionalityFitSignature("factor", "listwise", "varimax", "pa", "selected", true, 3, 4, {"x", "y"}) ==
           "factor|listwise|varimax|pa|selected|1|3|imputation=4|x|y");
    assert(DimensionalityFitSignature("pca", "pairwise", "none", "minres", "all", false, 2, 1, {"a"}) ==
           "pca|pairwise|none|minres|all|0|2|imputation=1|a");
    DimensionalityScreeContextMenuState screeMenu = BuildDimensionalityScreeContextMenuState();
    assert(screeMenu.title == "Scree plot");
    assert(screeMenu.viewTitle == "View");
    assert(screeMenu.exportTitle == "Export");
    assert(screeMenu.closePlotTitle == "Close plot");
    assert(DimensionalityMethodForPopupIndex(1) == "factor");
    assert(DimensionalityMethodForPopupIndex(99) == "pca");
    assert(DimensionalityMethodPopupIndex("factor") == 1);
    assert(DimensionalityMethodPopupIndex("bad") == 0);
    assert(DimensionalityRotationForPopupIndex(1) == "varimax");
    assert(DimensionalityRotationForPopupIndex(2) == "quartimax");
    assert(DimensionalityRotationForPopupIndex(-1) == "none");
    assert(DimensionalityRotationPopupIndex("quartimax") == 2);
    assert(DimensionalityRotationPopupIndex("oblimin") == 3);
    assert(DimensionalityRotationPopupIndex("promax") == 4);
    assert(DimensionalityRotationPopupIndex("bad") == 0);
    assert(DimensionalityScopeForPopupIndex(1) == "selected");
    assert(DimensionalityScopeForPopupIndex(2) == "unselected");
    assert(DimensionalityScopeForPopupIndex(9) == "all");
    assert(DimensionalityScopePopupIndex("unselected") == 2);
    assert(DimensionalityScopePopupIndex("bad") == 0);
    DimensionalityControlState controls = BuildDimensionalityControlState(
        "factor", "pairwise", "quartimax", "selected", false, 5, 99);
    assert(controls.methodIndex == 1);
    assert(controls.rotationIndex == 2);
    assert(controls.scopeIndex == 1);
    assert(controls.missingMode == "pairwise");
    assert(!controls.scale);
    assert(controls.maxComponents == 4);
    assert(controls.componentCount == 4);
    assert(controls.componentIndex == 3);
    assert(controls.componentOptions == std::vector<std::string>({
        "1 component", "2 components", "3 components", "4 components"
    }));
    DimensionalityControlState defaultControls = BuildDimensionalityControlState(
        "bad", "bad", "bad", "bad", true, 0, -10);
    assert(defaultControls.methodIndex == 0);
    assert(defaultControls.rotationIndex == 0);
    assert(defaultControls.scopeIndex == 0);
    assert(defaultControls.missingMode == "listwise");
    assert(defaultControls.maxComponents == 1);
    assert(defaultControls.componentCount == 1);
    DimensionalityAnalysisMenuState emptyAnalysisMenu = BuildDimensionalityAnalysisMenuState(0, 1, 0);
    assert(emptyAnalysisMenu.title == "Principal Components / Factor Analysis");
    assert(emptyAnalysisMenu.screePlotTitle == "Scree Plot");
    assert(emptyAnalysisMenu.biplotTitle == "Biplot");
    assert(emptyAnalysisMenu.saveScoresTitle == "Save Scores to Data Sheet");
    assert(emptyAnalysisMenu.addVariableTitle == "Add Variable...");
    assert(!emptyAnalysisMenu.canOpenScreePlot);
    assert(!emptyAnalysisMenu.canOpenBiplot);
    assert(!emptyAnalysisMenu.canSaveScores);
    DimensionalityAnalysisMenuState screeOnlyMenu = BuildDimensionalityAnalysisMenuState(3, 1, 0);
    assert(screeOnlyMenu.canOpenScreePlot);
    assert(!screeOnlyMenu.canOpenBiplot);
    assert(!screeOnlyMenu.canSaveScores);
    DimensionalityAnalysisMenuState completeAnalysisMenu = BuildDimensionalityAnalysisMenuState(3, 2, 12);
    assert(completeAnalysisMenu.canOpenScreePlot);
    assert(completeAnalysisMenu.canOpenBiplot);
    assert(completeAnalysisMenu.canSaveScores);
    DimensionalityWindowLayout minimumWindow = BuildDimensionalityWindowLayout(500.0, 300.0, 1200.0, 800.0);
    assert(closeEnough(minimumWindow.maxWidth, 1104.0));
    assert(closeEnough(minimumWindow.maxHeight, 688.0));
    assert(closeEnough(minimumWindow.targetWidth, 760.0));
    assert(closeEnough(minimumWindow.targetHeight, 442.0));
    assert(closeEnough(minimumWindow.titleRect.y, 404.0));
    assert(closeEnough(minimumWindow.badgeRect.x, 590.0));
    assert(closeEnough(minimumWindow.autoFitButtonRect.x, 148.0));
    assert(closeEnough(minimumWindow.scrollViewRect.width, 736.0));
    assert(closeEnough(minimumWindow.scrollViewRect.height, 286.0));
    DimensionalityWindowLayout cappedWindow = BuildDimensionalityWindowLayout(1400.0, 900.0, 1000.0, 700.0);
    assert(closeEnough(cappedWindow.maxWidth, 920.0));
    assert(closeEnough(cappedWindow.maxHeight, 602.0));
    assert(closeEnough(cappedWindow.targetWidth, 920.0));
    assert(closeEnough(cappedWindow.targetHeight, 602.0));
    assert(closeEnough(cappedWindow.selectedRowsRect.x, 740.0));
    assert(closeEnough(cappedWindow.statusRect.width, 888.0));

    rlispstat::core::DimensionalityState defaultState;
    assert(defaultState.autoFit);

    std::vector<std::string> currentVariables = {"mpg", "wt"};
    std::vector<std::string> availableVariables = {"mpg", "wt", "hp", "drat", "hp"};
    assert((DimensionalityVariablesAvailableToAdd(currentVariables, availableVariables) ==
            std::vector<std::string>({"hp", "drat"})));
    assert((DimensionalityVariablesAvailableToReplace(currentVariables, availableVariables, "mpg") ==
            std::vector<std::string>({"hp", "drat"})));
    DimensionalityAddVariableMenuState addMenu = BuildDimensionalityAddVariableMenuState(
        currentVariables, availableVariables);
    assert((addMenu.variables == std::vector<std::string>({"hp", "drat"})));
    assert(addMenu.emptyTitle == "No more eligible variables");
    DimensionalityAddVariableMenuState emptyAddMenu = BuildDimensionalityAddVariableMenuState(
        currentVariables, {"mpg", "wt", "mpg"});
    assert(emptyAddMenu.variables.empty());
    assert(emptyAddMenu.emptyTitle == "No more eligible variables");
    DimensionalityVariableMenuState variableMenu = BuildDimensionalityVariableMenuState(
        currentVariables, availableVariables, 1);
    assert(variableMenu.ok);
    assert(variableMenu.index == 1);
    assert(variableMenu.variable == "wt");
    assert((variableMenu.replacementVariables == std::vector<std::string>({"hp", "drat"})));
    assert(variableMenu.replacementEmptyTitle == "No replacement variables");
    assert(variableMenu.removeTitle == "Remove wt");
    assert(variableMenu.canRemove);
    DimensionalityVariableMenuState removableMenu = BuildDimensionalityVariableMenuState(
        {"mpg", "wt", "hp"}, availableVariables, 1);
    assert(removableMenu.ok);
    assert(removableMenu.canRemove);
    assert(!BuildDimensionalityVariableMenuState(currentVariables, availableVariables, 99).ok);
    DimensionalityVariableUpdateResult added = DimensionalityVariablesAfterAdd(
        currentVariables, "hp", availableVariables);
    assert(added.ok);
    assert(added.changed);
    assert((added.variables == std::vector<std::string>({"mpg", "wt", "hp"})));
    assert(!DimensionalityVariablesAfterAdd(currentVariables, "wt", availableVariables).ok);
    assert(!DimensionalityVariablesAfterAdd(currentVariables, "qsec", availableVariables).ok);
    DimensionalityVariableUpdateResult replaced = DimensionalityVariablesAfterReplace(
        currentVariables, 1, "drat", availableVariables);
    assert(replaced.ok);
    assert(replaced.changed);
    assert((replaced.variables == std::vector<std::string>({"mpg", "drat"})));
    assert(!DimensionalityVariablesAfterReplace(currentVariables, 5, "hp", availableVariables).ok);
    assert(!DimensionalityVariablesAfterReplace(currentVariables, 0, "wt", availableVariables).ok);
    DimensionalityVariableUpdateResult removed = DimensionalityVariablesAfterRemove(
        currentVariables, 0);
    assert(removed.ok);
    assert(removed.changed);
    assert((removed.variables == std::vector<std::string>({"wt"})));
    DimensionalityVariableUpdateResult removedLast = DimensionalityVariablesAfterRemove(
        removed.variables, 0);
    assert(removedLast.ok);
    assert(removedLast.variables.empty());
    assert(!DimensionalityVariablesAfterRemove(currentVariables, 0, 2).ok);

    std::vector<rlispstat::core::DimensionalityFitComponent> screeComponents = {
        {1, 2.40, 1.20, 0.60, 0.60},
        {2, 1.10, 0.95, 0.28, 0.88},
        {3, 0.50, std::numeric_limits<double>::quiet_NaN(), 0.12, 1.00}
    };
    DimensionalityScreePlotState scree = BuildDimensionalityScreePlotState(
        screeComponents, "factor", 2, "quartimax");
    assert(scree.ok);
    assert(scree.title == "Scree plot - Factor analysis (2 retained factors), quartimax rotation");
    assert(scree.xLabel == "Component");
    assert(scree.yLabel == "Eigenvalue");
    assert(scree.observed.size() == 3);
    assert(scree.parallel.size() == 2);
    assert(scree.observed[0].component == 1);
    assert(closeEnough(scree.observed[0].x, 1.0));
    assert(closeEnough(scree.observed[0].y, 2.40));
    assert(scree.observed[0].rowId == 0);
    assert(scree.parallel[1].component == 2);
    assert(closeEnough(scree.parallel[1].y, 0.95));
    assert(scree.parallel[1].rowId == 0);
    assert(scree.xmin <= 0.5);
    assert(scree.xmax >= 3.5);
    assert(scree.ymin <= 0.0);
    assert(scree.ymax > 2.40);
    DimensionalityScreeRenderPlan screeRender = BuildDimensionalityScreeRenderPlan(
        scree.observed,
        scree.parallel,
        DataViewport{scree.xmin, scree.xmax, scree.ymin, scree.ymax},
        Rect{10.0, 20.0, 300.0, 200.0},
        2);
    assert(screeRender.observedLine.size() == 3);
    assert(screeRender.parallelLine.size() == 2);
    assert(screeRender.observedPoints.size() == 3);
    assert(!screeRender.observedPoints[0].focused);
    assert(screeRender.observedPoints[1].focused);
    assert(closeEnough(screeRender.observedPoints[0].point.x,
                       10.0 + (1.0 - scree.xmin) / (scree.xmax - scree.xmin) * 300.0));
    assert(closeEnough(screeRender.observedPoints[0].point.y,
                       20.0 + 200.0 - (2.40 - scree.ymin) / (scree.ymax - scree.ymin) * 200.0));
    DimensionalityScreePlotState emptyScree = BuildDimensionalityScreePlotState(
        {}, "pca", 2, "none");
    assert(!emptyScree.ok);
    assert(emptyScree.observed.empty());

    std::vector<rlispstat::core::DimensionalityFitLoading> biplotLoadings = {
        {"x", {0.80, 0.10, 0.05}, 0.65, 0.35},
        {"y", {0.15, 0.75, 0.10}, 0.60, 0.40}
    };
    std::vector<rlispstat::core::DimensionalityFitScore> biplotScores = {
        {3, -1.0, 0.5, {-1.0, 0.5, 0.2}},
        {7, 1.5, -0.4, {1.5, -0.4, 0.1}},
        {9, 0.2, 1.1, {0.2, 1.1, -0.3}}
    };
    DimensionalityBiplotPlotState biplot = BuildDimensionalityBiplotPlotState(
        screeComponents, biplotLoadings, biplotScores, "pca", 1, 1);
    assert(biplot.ok);
    assert(biplot.xComponent == 1);
    assert(biplot.yComponent == 2);
    assert(biplot.title == "PCA biplot");
    assert(biplot.xLabel == "PC1 (60.0%)");
    assert(biplot.yLabel == "PC2 (28.0%)");
    assert(biplot.scores.size() == 3);
    assert(biplot.scores[0].rowId == 3);
    assert(closeEnough(biplot.scores[1].x, 1.5));
    assert(biplot.loadings.size() == 2);
    assert(biplot.xmin < biplot.dataXmin);
    assert(biplot.xmax > biplot.dataXmax);
    assert(biplot.ymin < biplot.dataYmin);
    assert(biplot.ymax > biplot.dataYmax);
    DimensionalityBiplotRenderPlan biplotRender = BuildDimensionalityBiplotRenderPlan(
        biplot.loadings,
        DataViewport{biplot.xmin, biplot.xmax, biplot.ymin, biplot.ymax},
        Rect{10.0, 20.0, 300.0, 200.0},
        "y");
    assert(biplotRender.loadings.size() == 2);
    assert(biplotRender.loadings[0].variable == "x");
    assert(!biplotRender.loadings[0].focused);
    assert(biplotRender.loadings[1].variable == "y");
    assert(biplotRender.loadings[1].focused);
    assert(closeEnough(biplotRender.loadings[0].labelAnchor.x,
                       biplotRender.loadings[0].end.x + 4.0));
    assert(closeEnough(biplotRender.loadings[0].labelAnchor.y,
                       biplotRender.loadings[0].end.y - 6.0));
    assert(!closeEnough(biplotRender.loadings[1].arrowHeadA.x,
                        biplotRender.loadings[1].end.x));
    DimensionalityBiplotPlotState noBiplot = BuildDimensionalityBiplotPlotState(
        {screeComponents.front()}, biplotLoadings, biplotScores, "pca", 1, 2);
    assert(!noBiplot.ok);
    auto twoScoreColumns = biplotScores;
    for (auto &score : twoScoreColumns) score.values.resize(2);
    assert(rlispstat::core::DimensionalityBiplotAvailableComponentCount(
        screeComponents, twoScoreColumns) == 2);
    DimensionalityBiplotPlotState limitedBiplot =
        BuildDimensionalityBiplotPlotState(screeComponents, biplotLoadings,
            twoScoreColumns, "pca", 1, 3);
    assert(limitedBiplot.ok);
    assert(limitedBiplot.xComponent == 1);
    assert(limitedBiplot.yComponent == 2);

    std::vector<rlispstat::core::DimensionalityFitScore> exportScores = {
        {1, 0.0, 0.0, {1.25, 2.50}},
        {3, 0.0, 0.0, {std::numeric_limits<double>::quiet_NaN(), 4.75}},
        {5, 0.0, 0.0, {8.00, 9.00}}
    };
    DimensionalityScoreExportPlan exportPlan = BuildDimensionalityScoreExportPlan(
        exportScores, "pca", 2, 2, 4, {"PC1_score", "PC2_score"});
    assert(exportPlan.ok);
    assert(exportPlan.columns.size() == 2);
    assert(exportPlan.columns[0].name == "PC1_score_2");
    assert(exportPlan.columns[1].name == "PC2_score_2");
    assert(exportPlan.columns[0].values == std::vector<std::string>({
        "1.2500000000", "NA", "NA", "NA"
    }));
    assert(exportPlan.columns[1].values == std::vector<std::string>({
        "2.5000000000", "NA", "4.7500000000", "NA"
    }));
    DimensionalityScoreExportPlan factorExport = BuildDimensionalityScoreExportPlan(
        exportScores, "factor", 1, 1, 2, {});
    assert(factorExport.ok);
    assert(factorExport.columns.size() == 1);
    assert(factorExport.columns[0].name == "F1_score");
    assert(!BuildDimensionalityScoreExportPlan({}, "pca", 2, 2, 4, {}).ok);

    DimensionalityReportState report = BuildDimensionalityReportState(
        {"y", "x", "missing"},
        screeComponents,
        biplotLoadings,
        12,
        4,
        "factor",
        2,
        "Status: fitted.",
        2,
        "y",
        2);
    assert(report.summary == "12 complete rows, 4 excluded");
    assert(report.calculationMethod.find("Calculated in R with stats::factanal") != std::string::npos);
    assert(report.calculationMethod.find("listwise complete-row input") != std::string::npos);
    assert(report.calculationImputation.empty());
    assert(report.componentPrefix == "F");
    assert(report.componentCount == 2);
    assert(report.componentRows.size() == 2);
    assert(report.hasAdditionalComponents);
    assert(report.componentRows[0].component == 1);
    assert(report.componentRows[0].label == "F1");
    assert(!report.componentRows[0].highlighted);
    assert(report.componentRows[0].eigenvalue == "2.400");
    assert(report.componentRows[0].parallelEigenvalue == "1.200");
    assert(report.componentRows[0].variance == "60.0%");
    assert(report.componentRows[1].highlighted);
    assert(report.loadingHeaders == std::vector<std::string>({"F1", "F2"}));
    assert(report.loadingRows.size() == 3);
    assert(report.loadingRows[0].variable == "y");
    assert(report.loadingRows[0].highlighted);
    assert(report.loadingRows[0].loadings[0].component == 1);
    assert(report.loadingRows[0].loadings[0].text == "0.150");
    assert(!report.loadingRows[0].loadings[0].emphasized);
    assert(!report.loadingRows[0].loadings[0].highlighted);
    assert(report.loadingRows[0].loadings[1].text == "0.750");
    assert(report.loadingRows[0].loadings[1].emphasized);
    assert(report.loadingRows[0].loadings[1].highlighted);
    assert(report.loadingRows[2].variable == "missing");
    assert(!report.loadingRows[2].highlighted);
    assert(report.loadingRows[2].loadings[0].text == "\u2014");
    assert(report.loadingRows[2].communality == "\u2014");
    assert(report.status == "Status: fitted.");
    DimensionalityReportLayout layout = BuildDimensionalityReportLayout(report);
    assert(closeEnough(layout.preferredWidth, 740.0));
    assert(closeEnough(layout.preferredHeight, 384.0));
    assert(closeEnough(layout.summaryRect.x, 18.0));
    assert(closeEnough(layout.summaryRect.y, 14.0));
    assert(closeEnough(layout.calculationMethodRect.y, 36.0));
    assert(closeEnough(layout.componentsTitleRect.y, 56.0));
    assert(layout.componentHeaderRects.size() == 5);
    assert(closeEnough(layout.componentHeaderRects[1].x, 166.0));
    assert(closeEnough(layout.componentHeaderRects[4].x, 524.0));
    assert(closeEnough(layout.componentRuleEndX, 628.0));
    assert(closeEnough(layout.componentRowsY, 104.0));
    assert(closeEnough(layout.loadingsRowsY, 240.0));
    assert(layout.componentRects.size() == 2);
    assert(closeEnough(layout.componentRects[1].x, 18.0));
    assert(closeEnough(layout.componentRects[1].y, 128.0));
    assert(closeEnough(layout.componentRects[1].width, 610.0));
    assert(layout.componentCellRects.size() == 2);
    assert(layout.componentCellRects[0].size() == 5);
    assert(closeEnough(layout.componentCellRects[1][2].x, 276.0));
    assert(closeEnough(layout.componentCellRects[1][2].y, 128.0));
    assert(closeEnough(layout.additionalComponentsRect.y, 152.0));
    assert(closeEnough(layout.loadingsTitleRect.y, 192.0));
    assert(closeEnough(layout.loadingVariableHeaderRect.y, 216.0));
    assert(layout.loadingHeaderRects.size() == 2);
    assert(closeEnough(layout.loadingHeaderRects[1].x, 260.0));
    assert(closeEnough(layout.loadingHeaderRects[1].y, 216.0));
    assert(closeEnough(layout.communalityHeaderRect.x, 336.0));
    assert(closeEnough(layout.uniquenessHeaderRect.x, 412.0));
    assert(closeEnough(layout.loadingsRuleEndX, 486.0));
    assert(layout.loadingCellRects.size() == 3);
    assert(layout.loadingCellRects[0].size() == 2);
    assert(closeEnough(layout.loadingCellRects[0][1].x, 260.0));
    assert(closeEnough(layout.loadingCellRects[0][1].y, 240.0));
    assert(layout.variableRects.size() == 3);
    assert(closeEnough(layout.variableRects[2].y, 288.0));
    assert(layout.communalityRects.size() == 3);
    assert(closeEnough(layout.communalityRects[0].x, 336.0));
    assert(closeEnough(layout.uniquenessRects[0].x, 412.0));
    assert(closeEnough(layout.addVariableRect.y, 312.0));
    assert(closeEnough(layout.emptyStatusRect.x, 184.0));
    assert(closeEnough(layout.emptyStatusRect.y, 312.0));
    Rect componentRect = DimensionalityReportRectForComponent(report, layout, 2);
    assert(closeEnough(componentRect.y, 128.0));
    assert(closeEnough(DimensionalityReportRectForComponent(report, layout, 99).width, 0.0));
    Rect variableRectByIndex = DimensionalityReportRectForVariableIndex(layout, 1);
    assert(closeEnough(variableRectByIndex.y, 264.0));
    assert(closeEnough(DimensionalityReportRectForVariableIndex(layout, 99).width, 0.0));
    Rect variableRectByName = DimensionalityReportRectForVariable(report, layout, "missing");
    assert(closeEnough(variableRectByName.y, 288.0));
    assert(closeEnough(DimensionalityReportRectForVariable(report, layout, "absent").width, 0.0));
    Rect focusVariableRect = DimensionalityReportFocusRect(report, layout, 2, "missing");
    assert(closeEnough(focusVariableRect.y, 288.0));
    Rect focusFallbackRect = DimensionalityReportFocusRect(report, layout, 2, "absent");
    assert(closeEnough(focusFallbackRect.y, 128.0));
    assert(closeEnough(DimensionalityReportFocusRect(report, layout, 0, "").width, 0.0));
    DimensionalityReportHit componentHit = HitTestDimensionalityReport(
        report, layout, Point{20.0, 130.0});
    assert(componentHit.kind == DimensionalityReportHitKind::Component);
    assert(componentHit.component == 2);
    assert(componentHit.componentIndex == 1);
    DimensionalityReportHit headerHit = HitTestDimensionalityReport(
        report, layout, Point{262.0, 218.0});
    assert(headerHit.kind == DimensionalityReportHitKind::LoadingHeader);
    assert(headerHit.component == 2);
    DimensionalityReportHit cellHit = HitTestDimensionalityReport(
        report, layout, Point{262.0, 242.0});
    assert(cellHit.kind == DimensionalityReportHitKind::LoadingCell);
    assert(cellHit.component == 2);
    assert(cellHit.componentIndex == 1);
    assert(cellHit.variableIndex == 0);
    assert(cellHit.variable == "y");
    DimensionalityReportHit variableHit = HitTestDimensionalityReport(
        report, layout, Point{20.0, 290.0});
    assert(variableHit.kind == DimensionalityReportHitKind::Variable);
    assert(variableHit.variableIndex == 2);
    assert(variableHit.variable == "missing");
    DimensionalityReportHit addHit = HitTestDimensionalityReport(
        report, layout, Point{20.0, 314.0});
    assert(addHit.kind == DimensionalityReportHitKind::AddVariable);
    DimensionalityReportHit noneHit = HitTestDimensionalityReport(
        report, layout, Point{700.0, 20.0});
    assert(noneHit.kind == DimensionalityReportHitKind::None);
    DimensionalityReportAction componentAction = DimensionalityReportPrimaryActionForHit(componentHit);
    assert(componentAction.kind == DimensionalityReportActionKind::FocusComponent);
    assert(componentAction.component == 2);
    DimensionalityReportAction headerAction = DimensionalityReportPrimaryActionForHit(headerHit);
    assert(headerAction.kind == DimensionalityReportActionKind::FocusComponent);
    assert(headerAction.component == 2);
    DimensionalityReportAction cellAction = DimensionalityReportPrimaryActionForHit(cellHit);
    assert(cellAction.kind == DimensionalityReportActionKind::FocusLoading);
    assert(cellAction.component == 2);
    assert(cellAction.variable == "y");
    DimensionalityReportAction variableAction = DimensionalityReportPrimaryActionForHit(variableHit);
    assert(variableAction.kind == DimensionalityReportActionKind::OpenVariableMenu);
    assert(variableAction.variableIndex == 2);
    assert(variableAction.variable == "missing");
    assert(variableAction.focusVariable);
    DimensionalityReportAction addAction = DimensionalityReportPrimaryActionForHit(addHit);
    assert(addAction.kind == DimensionalityReportActionKind::OpenAddVariableMenu);
    DimensionalityReportAction clearAction = DimensionalityReportPrimaryActionForHit(noneHit);
    assert(clearAction.kind == DimensionalityReportActionKind::ClearFocus);
    DimensionalityReportAction contextVariableAction = DimensionalityReportContextActionForHit(variableHit);
    assert(contextVariableAction.kind == DimensionalityReportActionKind::OpenVariableMenu);
    assert(contextVariableAction.variableIndex == 2);
    assert(!contextVariableAction.focusVariable);
    DimensionalityReportAction contextAnalysisAction = DimensionalityReportContextActionForHit(noneHit);
    assert(contextAnalysisAction.kind == DimensionalityReportActionKind::OpenAnalysisMenu);
    DimensionalityReportRenderPlan renderPlan = BuildDimensionalityReportRenderPlan(report, layout);
    assert(renderPlan.rules.size() == 2);
    assert(renderPlan.highlights.size() == 6);
    assert(renderPlan.texts.size() == 41);
    assert(renderPlan.texts[0].text == "12 complete rows, 4 excluded");
    assert(renderPlan.texts[0].role == DimensionalityReportTextRole::Muted);
    assert(renderPlan.texts[1].text.find("stats::factanal") != std::string::npos);
    assert(renderPlan.texts[1].role == DimensionalityReportTextRole::Muted);
    assert(renderPlan.texts[2].text == "Components");
    assert(renderPlan.texts[2].role == DimensionalityReportTextRole::Section);
    assert(renderPlan.texts[4].text == "Eigenvalue");
    assert(renderPlan.texts[4].role == DimensionalityReportTextRole::RightHeader);
    assert(renderPlan.highlights[0].role == DimensionalityReportHighlightRole::Strong);
    assert(closeEnough(renderPlan.highlights[0].rect.y, 126.0));
    assert(renderPlan.highlights[2].role == DimensionalityReportHighlightRole::Soft);
    DimensionalityReportViewModel viewModel = BuildDimensionalityReportViewModel(
        {"y", "x", "missing"},
        screeComponents,
        biplotLoadings,
        12,
        4,
        "factor",
        2,
        "Status: fitted.",
        2,
        "y",
        2);
    assert(viewModel.report.summary == report.summary);
    assert(closeEnough(viewModel.layout.loadingsRowsY, layout.loadingsRowsY));
    assert(viewModel.renderPlan.texts.size() == renderPlan.texts.size());
    assert(viewModel.renderPlan.highlights.size() == renderPlan.highlights.size());

    DimensionalityReportState multipleImputationReport = BuildDimensionalityReportState(
        {"y", "x"},
        screeComponents,
        biplotLoadings,
        12,
        0,
        "pca",
        2,
        "Status: fitted.",
        0,
        "",
        2,
        "listwise",
        "none",
        true,
        true,
        3,
        20);
    assert(multipleImputationReport.calculationMethod.find("stats::prcomp") != std::string::npos);
    assert(multipleImputationReport.calculationImputation.find("imputation 3 of 20") != std::string::npos);
    assert(multipleImputationReport.calculationImputation.find("not Rubin-pooled") != std::string::npos);
    DimensionalityReportLayout multipleImputationLayout =
        BuildDimensionalityReportLayout(multipleImputationReport);
    assert(closeEnough(multipleImputationLayout.componentsTitleRect.y, 76.0));

    assert(DimensionalityRotationIsValid("none"));
    assert(DimensionalityRotationIsValid("varimax"));
    assert(DimensionalityRotationIsValid("quartimax"));
    assert(DimensionalityRotationIsValid("oblimin"));
    assert(DimensionalityRotationIsValid("promax"));

    assert(DimensionalityScopeIsValid("all"));
    assert(DimensionalityScopeIsValid("selected"));
    assert(DimensionalityScopeIsValid("unselected"));
    assert(!DimensionalityScopeIsValid("visible"));

    std::vector<double> values;
    std::vector<std::vector<double>> vectors;
    assert(JacobiEigenSymmetric({{2.0, 1.0}, {1.0, 2.0}}, values, vectors));
    assert(values.size() == 2);
    assert(closeEnough(values[0], 3.0));
    assert(closeEnough(values[1], 1.0));
    assert(vectors.size() == 2);
    assert(vectors[0].size() == 2);
    assert(closeEnough(std::fabs(vectors[0][0]), std::sqrt(0.5)));
    assert(closeEnough(std::fabs(vectors[1][0]), std::sqrt(0.5)));

    assert(!JacobiEigenSymmetric({{1.0, 2.0}, {3.0}}, values, vectors));
    assert(!JacobiEigenSymmetric({}, values, vectors));

    std::vector<std::vector<double>> loadings = {
        {0.80, 0.20},
        {0.70, 0.10},
        {0.10, 0.75},
        {0.15, 0.65}
    };
    std::vector<std::vector<double>> scores = {
        {1.0, 0.2},
        {0.7, -0.1},
        {-0.3, 0.8}
    };
    std::vector<double> loadingNorms;
    for (const std::vector<double> &row : loadings) loadingNorms.push_back(rowNorm2(row));
    std::vector<double> scoreNorms;
    for (const std::vector<double> &row : scores) scoreNorms.push_back(rowNorm2(row));
    ApplyDimensionalityOrthomaxRotation(loadings, scores, "varimax");
    for (std::size_t i = 0; i < loadings.size(); ++i) {
        assert(closeEnough(rowNorm2(loadings[i]), loadingNorms[i], 1.0e-5));
    }
    for (std::size_t i = 0; i < scores.size(); ++i) {
        assert(closeEnough(rowNorm2(scores[i]), scoreNorms[i], 1.0e-5));
    }

    std::vector<double> parallelA = DimensionalityParallelEigenvalues(20, 3, 5);
    std::vector<double> parallelB = DimensionalityParallelEigenvalues(20, 3, 5);
    assert(parallelA.size() == 3);
    assert(parallelB.size() == 3);
    for (std::size_t i = 0; i < parallelA.size(); ++i) {
        assert(std::isfinite(parallelA[i]));
        assert(parallelA[i] > 0.0);
        assert(closeEnough(parallelA[i], parallelB[i], 1.0e-12));
    }

    std::vector<double> missing = DimensionalityParallelEigenvalues(1, 3, 5);
    assert(missing.size() == 3);
    assert(std::isnan(missing[0]));

    DimensionalityFitInput fitInput;
    fitInput.variables = {"x", "y", "z"};
    fitInput.columns = {
        {1.0, 2.0, 3.0, 4.0, 5.0},
        {1.0, 4.0, 9.0, NAN, 25.0},
        {5.0, 3.0, 4.0, 2.0, 1.0}
    };
    fitInput.method = "pca";
    fitInput.componentCount = 2;
    fitInput.parallelIterations = 5;
    DimensionalityFitResult fit = FitDimensionality(fitInput);
    assert(fit.method == "pca");
    assert(fit.status.find("Principal components") != std::string::npos);
    assert(fit.rowsUsed == std::vector<int>({1, 2, 3, 5}));
    assert(fit.rowsExcluded == std::vector<int>({4}));
    assert(fit.componentCount == 2);
    assert(fit.components.size() == 3);
    assert(fit.loadings.size() == 3);
    assert(fit.scores.size() == 4);
    assert(std::isfinite(fit.components[0].eigenvalue));
    assert(std::isfinite(fit.loadings[0].communality));
    assert(std::isfinite(fit.scores[0].x));

    fitInput.method = "factor";
    fitInput.componentCount = 5;
    DimensionalityFitResult factorFit = FitDimensionality(fitInput);
    assert(factorFit.method == "factor");
    assert(factorFit.componentCount == 2);

    DimensionalityFitInput selectedInput;
    selectedInput.variables = {"a", "b"};
    selectedInput.columns = {{1.0, 2.0, 4.0, 8.0}, {2.0, 4.0, 8.0, 16.0}};
    selectedInput.scope = "selected";
    selectedInput.selectedRows = {2, 3};
    selectedInput.parallelIterations = 3;
    DimensionalityFitResult selectedFit = FitDimensionality(selectedInput);
    assert(selectedFit.scope == "selected");
    assert(selectedFit.rowsUsed == std::vector<int>({2, 3}));
    assert(selectedFit.rowsExcluded == std::vector<int>({1, 4}));

    DimensionalityFitInput badInput;
    badInput.variables = {"a", "flat"};
    badInput.columns = {{1.0, 2.0, 3.0}, {7.0, 7.0, 7.0}};
    DimensionalityFitResult badFit = FitDimensionality(badInput);
    assert(badFit.status.find("zero variance") != std::string::npos);

    DimensionalityFitInput normalizedInput;
    normalizedInput.variables = {"a"};
    normalizedInput.method = "nonsense";
    normalizedInput.rotation = "bad";
    normalizedInput.scope = "bad";
    DimensionalityFitResult normalizedFit = FitDimensionality(normalizedInput);
    assert(normalizedFit.method == "pca");
    assert(normalizedFit.rotation == "none");
    assert(normalizedFit.scope == "all");
    assert(normalizedFit.status == "At least two numeric variables are required.");

    assert(rlispstat::core::DimensionalityWindowTitle() == "Principal Components / Factor Analysis");
    assert(rlispstat::core::DimensionalityStandardizeButtonTitle() == "Standardize");
    assert(rlispstat::core::DimensionalityNoRotationTitle() == "No rotation");
    assert(rlispstat::core::DimensionalityVarimaxRotationTitle() == "Varimax");
    assert(rlispstat::core::DimensionalityQuartimaxRotationTitle() == "Quartimax");
    assert(rlispstat::core::DimensionalityPCAMethodTitle() == "Principal components");
    assert(rlispstat::core::DimensionalityFactorAnalysisMethodTitle() == "Factor analysis");

    return 0;
}
