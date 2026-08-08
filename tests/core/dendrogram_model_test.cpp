#include "../../src/core/dendrogram_model.h"

#include <cassert>
#include <cmath>
#include <limits>
#include <string>
#include <vector>

using rlispstat::core::DendrogramDistanceIsValid;
using rlispstat::core::DendrogramAtLeastOneVariableStatus;
using rlispstat::core::BuildDendrogramVariableMenuState;
using rlispstat::core::BuildDendrogramWindowLayout;
using rlispstat::core::BuildDendrogramContextMenuState;
using rlispstat::core::BuildDendrogramPlotGeometry;
using rlispstat::core::DendrogramColorSelectedCasesTitle;
using rlispstat::core::DendrogramContextMenuState;
using rlispstat::core::DendrogramCopyFailedMessage;
using rlispstat::core::DendrogramDefaultExportFilename;
using rlispstat::core::DendrogramDistanceControlLabel;
using rlispstat::core::DendrogramExportPanelTitle;
using rlispstat::core::DendrogramExportFailedMessage;
using rlispstat::core::DendrogramFitInput;
using rlispstat::core::DendrogramFitResult;
using rlispstat::core::DendrogramLeafRowsInRect;
using rlispstat::core::DendrogramLinkageIsValid;
using rlispstat::core::DendrogramLinkageControlLabel;
using rlispstat::core::DendrogramMissingDataControlLabel;
using rlispstat::core::DendrogramNearestLeafRowAtPoint;
using rlispstat::core::DendrogramNoNumericVariablesTitle;
using rlispstat::core::DendrogramPlotGeometry;
using rlispstat::core::DendrogramPreferredContentSize;
using rlispstat::core::DendrogramSelectionGestureResult;
using rlispstat::core::DendrogramSelectionRowsForGesture;
using rlispstat::core::DendrogramSize;
using rlispstat::core::DendrogramVariableMenuState;
using rlispstat::core::DendrogramVariableMenuTitle;
using rlispstat::core::DendrogramVariablesAfterToggle;
using rlispstat::core::DendrogramVariableUpdateResult;
using rlispstat::core::DendrogramVariablesButtonTitle;
using rlispstat::core::DendrogramWindowLayout;
using rlispstat::core::DendrogramWindowSummaryStatus;
using rlispstat::core::DendrogramWindowTitle;
using rlispstat::core::FitDendrogram;
using rlispstat::core::Point;
using rlispstat::core::Rect;

static bool closeEnough(double a, double b, double tolerance = 1.0e-6)
{
    return std::fabs(a - b) < tolerance;
}

int main()
{
    assert(DendrogramLinkageIsValid("average"));
    assert(DendrogramLinkageIsValid("complete"));
    assert(DendrogramLinkageIsValid("single"));
    assert(!DendrogramLinkageIsValid("ward"));

    assert(DendrogramDistanceIsValid("euclidean"));
    assert(DendrogramDistanceIsValid("correlation"));
    assert(!DendrogramDistanceIsValid("manhattan"));
    assert(DendrogramWindowSummaryStatus("cars", 32, 4, "average", "pairwise", 3) ==
           "cars: 32 cases, 4 variables, average linkage, pairwise missing mode, 3 selected");
    assert(DendrogramWindowTitle() == "Quick Cluster Dendrogram");
    assert(DendrogramVariableMenuTitle() == "Variables");
    assert(DendrogramLinkageControlLabel() == "Linkage:");
    assert(DendrogramMissingDataControlLabel() == "Missing data:");
    assert(DendrogramDistanceControlLabel("euclidean") == "Distance: euclidean (z)");
    assert(DendrogramDistanceControlLabel("correlation") == "Distance: correlation (z)");
    assert(DendrogramDistanceControlLabel("") == "Distance: euclidean (z)");
    assert(DendrogramAtLeastOneVariableStatus() == "At least one variable is required.");
    assert(DendrogramColorSelectedCasesTitle("blue") == "Color selected cases: blue");
    assert(DendrogramVariablesButtonTitle(3) == "Variables (3)...");
    assert(DendrogramNoNumericVariablesTitle() == "No numeric variables");
    assert(DendrogramExportPanelTitle("PDF") == "Export Dendrogram as PDF");
    assert(DendrogramExportPanelTitle("PNG") == "Export Dendrogram as PNG");
    assert(DendrogramDefaultExportFilename("PDF") == "LinkEDA-dendrogram.pdf");
    assert(DendrogramDefaultExportFilename("PNG") == "LinkEDA-dendrogram.png");
    assert(DendrogramExportFailedMessage() == "The dendrogram view could not be exported.");
    assert(DendrogramCopyFailedMessage("PNG") == "The dendrogram could not be copied as PNG.");
    assert(DendrogramCopyFailedMessage("PDF") == "The dendrogram could not be copied as PDF.");
    DendrogramSize smallSize = DendrogramPreferredContentSize(0);
    assert(closeEnough(smallSize.width, 760.0));
    assert(closeEnough(smallSize.height, 450.0));
    DendrogramSize largeSize = DendrogramPreferredContentSize(80);
    assert(closeEnough(largeSize.width, 1420.0));
    DendrogramWindowLayout minimumLayout = BuildDendrogramWindowLayout(500.0, 300.0, 1200.0, 800.0);
    assert(closeEnough(minimumLayout.maxWidth, 1128.0));
    assert(closeEnough(minimumLayout.maxHeight, 704.0));
    assert(closeEnough(minimumLayout.targetWidth, 700.0));
    assert(closeEnough(minimumLayout.targetHeight, 424.0));
    assert(closeEnough(minimumLayout.titleRect.y, 386.0));
    assert(closeEnough(minimumLayout.variablesButtonRect.x, 620.0));
    assert(closeEnough(minimumLayout.scrollViewRect.width, 676.0));
    DendrogramWindowLayout cappedLayout = BuildDendrogramWindowLayout(1400.0, 900.0, 1000.0, 700.0);
    assert(closeEnough(cappedLayout.maxWidth, 940.0));
    assert(closeEnough(cappedLayout.maxHeight, 616.0));
    assert(closeEnough(cappedLayout.targetWidth, 940.0));
    assert(closeEnough(cappedLayout.targetHeight, 616.0));
    assert(closeEnough(cappedLayout.variablesButtonRect.x, 750.0));
    assert(closeEnough(cappedLayout.statusRect.width, 908.0));
    DendrogramVariableMenuState variableMenu = BuildDendrogramVariableMenuState(
        {"x"}, {"x", "y", "y", "z"});
    assert(variableMenu.items.size() == 3);
    assert(variableMenu.items[0].variable == "x");
    assert(variableMenu.items[0].included);
    assert(!variableMenu.items[0].enabled);
    assert(variableMenu.items[1].variable == "y");
    assert(!variableMenu.items[1].included);
    assert(variableMenu.items[1].enabled);
    assert(variableMenu.emptyTitle == "No numeric variables");
    assert(BuildDendrogramVariableMenuState({"x"}, {}).items.empty());
    DendrogramVariableUpdateResult addVariable = DendrogramVariablesAfterToggle(
        {"x"}, "y", {"x", "y"});
    assert(addVariable.ok);
    assert(addVariable.changed);
    assert(addVariable.variables == std::vector<std::string>({"x", "y"}));
    DendrogramVariableUpdateResult removeVariable = DendrogramVariablesAfterToggle(
        {"x", "y"}, "x", {"x", "y"});
    assert(removeVariable.ok);
    assert(removeVariable.variables == std::vector<std::string>({"y"}));
    DendrogramVariableUpdateResult protectedRemove = DendrogramVariablesAfterToggle(
        {"x"}, "x", {"x", "y"});
    assert(!protectedRemove.ok);
    assert(protectedRemove.status == "At least one variable is required.");
    assert(!DendrogramVariablesAfterToggle({"x"}, "qsec", {"x", "y"}).ok);
    DendrogramContextMenuState contextMenu = BuildDendrogramContextMenuState("blue");
    assert(contextMenu.title == "Quick Cluster");
    assert(contextMenu.selectAllTitle == "Select all displayed cases");
    assert(contextMenu.clearSelectionTitle == "Clear selection");
    assert(contextMenu.invertSelectionTitle == "Invert displayed selection");
    assert(contextMenu.colorSelectedCasesTitle == "Color selected cases: blue");
    assert(contextMenu.dataPointsColorTitle == "Data Points Color...");
    assert(contextMenu.resetSelectedColorsTitle == "Reset selected case colors");
    assert(contextMenu.exportTitle == "Export");
    assert(contextMenu.savePngTitle == "Save as PNG...");
    assert(contextMenu.savePdfTitle == "Save as PDF...");
    assert(contextMenu.copyPngTitle == "Copy as PNG");
    assert(contextMenu.copyPdfTitle == "Copy as PDF");
    assert(BuildDendrogramContextMenuState("").colorSelectedCasesTitle ==
           "Color selected cases: black");

    DendrogramFitInput input;
    input.variables = {"x", "y"};
    input.columns = {
        {1.0, 2.0, 9.0, 10.0},
        {1.0, 2.0, 9.0, 10.0}
    };
    input.linkage = "average";
    DendrogramFitResult fit = FitDendrogram(input);
    assert(fit.linkage == "average");
    assert(fit.missingMode == "pairwise");
    assert(fit.caseRows == std::vector<int>({1, 2, 3, 4}));
    assert(fit.merges.size() == 3);
    assert(fit.leafOrder.size() == 4);
    assert(fit.merges[0].left == 0);
    assert(fit.merges[0].right == 1);
    assert(fit.merges[0].size == 2);
    assert(std::isfinite(fit.merges[0].height));
    assert(fit.merges[0].height < fit.merges.back().height);
    DendrogramPlotGeometry geometry = BuildDendrogramPlotGeometry(
        fit.caseRows, fit.merges, fit.leafOrder, 760.0, 450.0);
    assert(geometry.hasCases);
    assert(closeEnough(geometry.plotRect.x, 86.0));
    assert(closeEnough(geometry.plotRect.y, 18.0));
    assert(closeEnough(geometry.plotRect.width, 646.0));
    assert(closeEnough(geometry.plotRect.height, 326.0));
    assert(closeEnough(geometry.maxHeight, fit.merges.back().height));
    assert(geometry.leaves.size() == 4);
    assert(geometry.branches.size() == 9);
    assert(closeEnough(geometry.leaves[0].point.y, 362.0));
    assert(geometry.leaves[0].showLabel);
    assert(geometry.leaves.back().showLabel);
    assert(DendrogramNearestLeafRowAtPoint(geometry, geometry.leaves[0].point, 1.0) ==
           geometry.leaves[0].rowId);
    assert(DendrogramNearestLeafRowAtPoint(geometry, Point{0.0, 0.0}, 1.0) == 0);
    std::vector<int> oneLeaf = DendrogramLeafRowsInRect(
        geometry,
        Rect{geometry.leaves[0].point.x - 2.0, geometry.leaves[0].point.y - 2.0, 4.0, 4.0});
    assert(oneLeaf == std::vector<int>({geometry.leaves[0].rowId}));
    std::vector<int> allLeaves = DendrogramLeafRowsInRect(
        geometry,
        Rect{80.0, 350.0, 660.0, 30.0});
    assert(allLeaves.size() == 4);
    DendrogramSelectionGestureResult clickGesture = DendrogramSelectionRowsForGesture(
        geometry,
        Rect{geometry.leaves[0].point.x, geometry.leaves[0].point.y, 0.0, 0.0},
        geometry.leaves[0].point);
    assert(!clickGesture.usedBrush);
    assert(clickGesture.rows == std::vector<int>({geometry.leaves[0].rowId}));
    DendrogramSelectionGestureResult missGesture = DendrogramSelectionRowsForGesture(
        geometry,
        Rect{0.0, 0.0, 1.0, 1.0},
        Point{0.0, 0.0});
    assert(!missGesture.usedBrush);
    assert(missGesture.rows.empty());
    DendrogramSelectionGestureResult brushGesture = DendrogramSelectionRowsForGesture(
        geometry,
        Rect{80.0, 350.0, 660.0, 30.0},
        Point{0.0, 0.0});
    assert(brushGesture.usedBrush);
    assert(brushGesture.rows.size() == 4);
    DendrogramPlotGeometry fallbackGeometry = BuildDendrogramPlotGeometry(
        {10, 11}, {}, {1}, 300.0, 220.0);
    assert(fallbackGeometry.hasCases);
    assert(fallbackGeometry.leaves.size() == 2);
    assert(fallbackGeometry.leaves[0].rowId == 10);
    assert(fallbackGeometry.leaves[1].rowId == 11);
    assert(!BuildDendrogramPlotGeometry({}, {}, {}, 300.0, 220.0).hasCases);

    DendrogramFitInput missingInput;
    missingInput.variables = {"x", "y"};
    missingInput.columns = {
        {1.0, 2.0, std::numeric_limits<double>::quiet_NaN(), 4.0},
        {1.0, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), 4.0}
    };
    missingInput.missingMode = "pairwise";
    DendrogramFitResult pairwise = FitDendrogram(missingInput);
    assert(pairwise.caseRows == std::vector<int>({1, 2, 4}));

    missingInput.missingMode = "listwise";
    DendrogramFitResult listwise = FitDendrogram(missingInput);
    assert(listwise.caseRows == std::vector<int>({1, 4}));
    assert(listwise.merges.size() == 1);

    DendrogramFitInput oneCase;
    oneCase.variables = {"x"};
    oneCase.columns = {{42.0}};
    DendrogramFitResult one = FitDendrogram(oneCase);
    assert(one.caseRows == std::vector<int>({1}));
    assert(one.merges.empty());
    assert(one.leafOrder == std::vector<int>({0}));

    DendrogramFitInput normalized;
    normalized.variables = {"x"};
    normalized.columns = {{1.0, 2.0}};
    normalized.distance = "bad";
    normalized.linkage = "bad";
    normalized.missingMode = "bad";
    DendrogramFitResult normalizedFit = FitDendrogram(normalized);
    assert(normalizedFit.distance == "euclidean");
    assert(normalizedFit.linkage == "average");
    assert(normalizedFit.missingMode == "pairwise");

    DendrogramFitInput bad;
    bad.variables = {"x", "y"};
    bad.columns = {{1.0, 2.0}, {1.0}};
    DendrogramFitResult badFit = FitDendrogram(bad);
    assert(badFit.caseRows.empty());
    assert(badFit.merges.empty());
    assert(badFit.leafOrder.empty());

    assert(closeEnough(fit.merges[0].height, fit.merges[0].height));

    assert(rlispstat::core::DendrogramAverageLinkageTitle() == "average");
    assert(rlispstat::core::DendrogramCompleteLinkageTitle() == "complete");
    assert(rlispstat::core::DendrogramSingleLinkageTitle() == "single");
    assert(rlispstat::core::DendrogramExportWindowTitle() == "Export Dendrogram");
    assert(rlispstat::core::DendrogramPDFOptionIdentifier() == "PDF");
    assert(rlispstat::core::DendrogramPNGOptionIdentifier() == "PNG");
    assert(rlispstat::core::DendrogramCopyWindowTitle() == "Copy Dendrogram");

    return 0;
}
