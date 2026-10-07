#include "../../src/core/dendrogram_model.h"
#include "../../src/core/dataset_model.h"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <limits>
#include <string>
#include <vector>

using rlispstat::core::DendrogramDistanceIsValid;
using rlispstat::core::DendrogramAtLeastOneVariableStatus;
using rlispstat::core::DendrogramAddVariablesStatus;
using rlispstat::core::DendrogramEmptyPlotStatus;
using rlispstat::core::BuildDendrogramVariableMenuState;
using rlispstat::core::BuildDendrogramWindowLayout;
using rlispstat::core::BuildDendrogramContextMenuState;
using rlispstat::core::BuildDendrogramPlotGeometry;
using rlispstat::core::DendrogramColorSelectedCasesTitle;
using rlispstat::core::DendrogramColorLegendMatchesLinkedRowColors;
using rlispstat::core::DendrogramContextMenuState;
using rlispstat::core::DendrogramCopyFailedMessage;
using rlispstat::core::DendrogramDefaultExportFilename;
using rlispstat::core::DendrogramDistanceControlLabel;
using rlispstat::core::DendrogramExportPanelTitle;
using rlispstat::core::DendrogramExportFailedMessage;
using rlispstat::core::DendrogramFitResult;
using rlispstat::core::DendrogramLeafRowsInRect;
using rlispstat::core::DendrogramJoinRowsAtPoint;
using rlispstat::core::DendrogramBranchIsFullySelected;
using rlispstat::core::DendrogramUniformBranchColor;
using rlispstat::core::DendrogramLinkageIsValid;
using rlispstat::core::DendrogramLinkageControlLabel;
using rlispstat::core::DendrogramMissingDataControlLabel;
using rlispstat::core::DendrogramNearestLeafRowAtPoint;
using rlispstat::core::DendrogramNoNumericVariablesTitle;
using rlispstat::core::DendrogramPlotGeometry;
using rlispstat::core::DendrogramPreferredContentSize;
using rlispstat::core::DendrogramViewportContentSize;
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
using rlispstat::core::ReadDendrogramRTree;
using rlispstat::core::SetDendrogramColorByVariable;
using rlispstat::core::DendrogramColorLegendRows;
using rlispstat::core::SetDendrogramLabelVariable;
using rlispstat::core::Point;
using rlispstat::core::Rect;

static bool closeEnough(double a, double b, double tolerance = 1.0e-6)
{
    return std::fabs(a - b) < tolerance;
}

int main()
{
    const auto caption=rlispstat::core::DendrogramVariableCaptionLines({"age_before","age_after","perceived_risk_before"},220);
    assert(caption.size()>1);
    std::string joined;for(const auto &line:caption)joined+=line;
    assert(joined.find("age_before")!=std::string::npos && joined.find("age_after")!=std::string::npos);
    const auto utf8=rlispstat::core::DendrogramVariableCaptionLines({"año","puntuación"},500);
    assert(utf8.size()==1 && utf8[0]=="Variables: año, puntuación");

    assert(DendrogramLinkageIsValid("average"));
    assert(DendrogramLinkageIsValid("complete"));
    assert(DendrogramLinkageIsValid("single"));
    assert(!DendrogramLinkageIsValid("ward"));

    assert(DendrogramDistanceIsValid("euclidean"));
    assert(!DendrogramDistanceIsValid("correlation"));
    assert(DendrogramDistanceIsValid("manhattan"));
    assert(DendrogramDistanceIsValid("maximum"));
    assert(DendrogramDistanceIsValid("canberra"));
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
    assert(DendrogramAddVariablesStatus() == "Add one or more numeric variables to cluster the cases.");
    assert(DendrogramEmptyPlotStatus(0) == DendrogramAddVariablesStatus());
    assert(DendrogramEmptyPlotStatus(2) == rlispstat::core::NoCompleteCasesStatus());
    assert(DendrogramColorSelectedCasesTitle("blue") == "Color selected cases: blue");
    assert(DendrogramVariablesButtonTitle(3) == "+ Add variable");
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
    assert(closeEnough(minimumLayout.variablesButtonRect.x, 16.0));
    assert(closeEnough(minimumLayout.scrollViewRect.width, 676.0));
    DendrogramWindowLayout cappedLayout = BuildDendrogramWindowLayout(1400.0, 900.0, 1000.0, 700.0);
    assert(closeEnough(cappedLayout.maxWidth, 940.0));
    assert(closeEnough(cappedLayout.maxHeight, 616.0));
    assert(closeEnough(cappedLayout.targetWidth, 940.0));
    assert(closeEnough(cappedLayout.targetHeight, 616.0));
    assert(closeEnough(cappedLayout.variablesButtonRect.x, 16.0));
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

    // A rendering fixture transported from R, never fitted by the native UI.
    const std::vector<std::string> rTree = {
        "4", "1", "2", "3", "4", "4", "0", "1", "2", "3", "3",
        "0", "1", "0.303821810125100", "2", "3", "0.303821810125100",
        "4", "5", "2.430574481000800"};
    std::size_t cursor = 0;
    std::string error;
    DendrogramFitResult fit;
    assert(ReadDendrogramRTree(rTree, cursor, fit, error));
    assert(cursor == rTree.size());
    assert(fit.caseRows == std::vector<int>({1, 2, 3, 4}));
    assert(fit.merges.size() == 3 && fit.merges.back().size == 4);
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
    assert(geometry.branches.front().rows.size() == 1);
    assert(DendrogramBranchIsFullySelected(geometry.branches.front(), {geometry.branches.front().rows.front()}));
    assert(!DendrogramBranchIsFullySelected(geometry.branches.back(), {geometry.branches.front().rows.front()}));
    std::map<int, std::string> branchColors;
    for(int row : geometry.branches.back().rows) branchColors[row] = "blue";
    assert(DendrogramUniformBranchColor(geometry.branches.back(), branchColors, {}) == "blue");
    branchColors[geometry.branches.back().rows.front()] = "red";
    assert(DendrogramUniformBranchColor(geometry.branches.back(), branchColors, {}).empty());
    DendrogramPlotGeometry rotated = BuildDendrogramPlotGeometry(
        fit.caseRows, fit.merges, fit.leafOrder, 520.0, 760.0, false, {}, true, false);
    assert(rotated.leaves.size() == geometry.leaves.size());
    assert(std::abs(rotated.leaves.front().point.x - rotated.leaves.back().point.x) < 0.001);
    assert(rotated.leaves.front().point.y < rotated.leaves.back().point.y);
    assert(rotated.joins.back().point.x > rotated.leaves.front().point.x);
    assert(std::abs(rotated.joins.front().segmentStart.x - rotated.joins.front().segmentEnd.x) < 0.001);
    DendrogramPlotGeometry rotatedLabels = BuildDendrogramPlotGeometry(
        fit.caseRows, fit.merges, fit.leafOrder, 520.0, 760.0, false, {}, true, true);
    assert(rotatedLabels.leaves.front().labelRotated90);

    // Turning off Fit tree to window gives a rotated dendrogram a scrollable
    // vertical canvas with enough room for every horizontal case label.
    std::vector<int> manyRows;
    std::vector<int> manyOrder;
    for (int index = 0; index < 80; ++index) {
        manyRows.push_back(index + 1);
        manyOrder.push_back(index);
    }
    const DendrogramSize scrollable = DendrogramViewportContentSize(
        manyRows.size(), 520.0, 420.0, false, true);
    DendrogramPlotGeometry scrollableGeometry = BuildDendrogramPlotGeometry(
        manyRows, {}, manyOrder, scrollable.width, scrollable.height,
        false, {}, true, false);
    assert(std::all_of(scrollableGeometry.leaves.begin(),
                       scrollableGeometry.leaves.end(),
                       [](const auto &leaf) {
                           return leaf.showLabel;
                       }));
    const DendrogramSize fitted = DendrogramViewportContentSize(
        manyRows.size(), 520.0, 420.0, true, true);
    DendrogramPlotGeometry fittedGeometry = BuildDendrogramPlotGeometry(
        manyRows, {}, manyOrder, fitted.width, fitted.height,
        false, {}, true, false);
    assert(std::any_of(fittedGeometry.leaves.begin(), fittedGeometry.leaves.end(),
                       [](const auto &leaf) {
                           return !leaf.showLabel;
                       }));
    assert(geometry.joins.size() == 3);
    assert(closeEnough(geometry.leaves[0].point.y, 344.0));
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
        Rect{80.0, 336.0, 660.0, 16.0});
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
        Rect{80.0, 336.0, 660.0, 16.0},
        Point{0.0, 0.0});
    assert(brushGesture.usedBrush);
    assert(brushGesture.rows.size() == 4);
    DendrogramSelectionGestureResult horizontalBrushGesture =
        DendrogramSelectionRowsForGesture(
            geometry,
            Rect{geometry.leaves.front().point.x,
                 geometry.leaves.front().point.y,
                 geometry.leaves.back().point.x - geometry.leaves.front().point.x,
                 0.5},
            geometry.leaves.back().point);
    assert(horizontalBrushGesture.usedBrush);
    assert(horizontalBrushGesture.rows.size() == 4);
    const auto firstJoinRows = DendrogramJoinRowsAtPoint(
        geometry, geometry.joins.front().point, 1.0);
    assert(firstJoinRows.size() == 2);
    const Point firstJoinBarPoint{
        geometry.joins.front().segmentStart.x + 1.0,
        geometry.joins.front().segmentStart.y};
    assert(DendrogramJoinRowsAtPoint(geometry, firstJoinBarPoint, 1.0) == firstJoinRows);
    DendrogramSelectionGestureResult joinGesture = DendrogramSelectionRowsForGesture(
        geometry,
        Rect{geometry.joins.front().point.x, geometry.joins.front().point.y, 0.0, 0.0},
        geometry.joins.front().point);
    assert(!joinGesture.usedBrush);
    assert(joinGesture.rows == firstJoinRows);
    std::map<int, std::string> customLabels{{1, "Case A"}, {2, "Case B"}};
    DendrogramPlotGeometry flippedGeometry = BuildDendrogramPlotGeometry(
        fit.caseRows, fit.merges, fit.leafOrder, 760.0, 450.0, true, customLabels);
    assert(closeEnough(flippedGeometry.plotRect.y, 106.0));
    assert(closeEnough(flippedGeometry.leaves.front().point.y,
                       flippedGeometry.plotRect.y));
    bool foundCustomLabel = false;
    for (const auto &leaf : flippedGeometry.leaves)
        if (leaf.rowId == 1 && leaf.label == "Case A") foundCustomLabel = true;
    assert(foundCustomLabel);
    DendrogramPlotGeometry fallbackGeometry = BuildDendrogramPlotGeometry(
        {10, 11}, {}, {1}, 300.0, 220.0);
    assert(fallbackGeometry.hasCases);
    assert(fallbackGeometry.leaves.size() == 2);
    assert(fallbackGeometry.leaves[0].rowId == 10);
    assert(fallbackGeometry.leaves[1].rowId == 11);
    assert(!BuildDendrogramPlotGeometry({}, {}, {}, 300.0, 220.0).hasCases);

    rlispstat::core::DataFrameModel appearanceData;
    appearanceData.group = "cases";
    appearanceData.rows = 4;
    rlispstat::core::DataColumn labels;
    labels.name = "name"; labels.type = "character";
    labels.values = {"A", "B", "C", "D"};
    rlispstat::core::DataColumn groups;
    groups.name = "group"; groups.type = "factor";
    groups.values = {"1", "1", "2", "2"};
    groups.displayValues = {"Control", "Control", "Treatment", "Treatment"};
    groups.definedLevels = {"Control", "Treatment"};
    appearanceData.columns = {labels, groups};
    rlispstat::core::DendrogramState appearanceState;
    std::string appearanceError;
    assert(SetDendrogramLabelVariable(appearanceState, appearanceData, "name", &appearanceError));
    assert(appearanceState.rowLabels[3] == "C");
    assert(SetDendrogramColorByVariable(appearanceState, appearanceData, "group", &appearanceError));
    assert(appearanceState.colorByRowColors.size() == 4);
    assert(appearanceState.colorByLegendItems.size() == 2);
    assert(appearanceState.colorByLegendItems.front().first == "Control");
    assert(DendrogramColorLegendMatchesLinkedRowColors(
        appearanceState, appearanceState.colorByRowColors));
    auto conflictingColors = appearanceState.colorByRowColors;
    conflictingColors[2] = "purple";
    assert(!DendrogramColorLegendMatchesLinkedRowColors(
        appearanceState, conflictingColors));
    appearanceState.caseRows = {1, 3, 4};
    assert(DendrogramColorLegendRows(appearanceState, "Control") == std::set<int>({1}));
    assert(DendrogramColorLegendRows(appearanceState, "Treatment") == std::set<int>({3, 4}));

    for (const auto &bad : std::vector<std::vector<std::string>>{
        {"2", "1", "1", "2", "0", "1", "1", "0", "1", "2"}, // duplicated row
        {"2", "1", "2", "2", "0", "0", "1", "0", "1", "2"}, // duplicated leaf
        {"2", "1", "2", "2", "0", "1", "1", "0", "2", "2"}, // forward child
        {"2", "1", "2", "2", "0", "1", "1", "0", "1", "nan"},
        {"2", "1"}}) {
        cursor = 0; DendrogramFitResult invalid;
        assert(!ReadDendrogramRTree(bad, cursor, invalid, error));
        assert(invalid.caseRows.empty());
    }
    cursor = 0; DendrogramFitResult empty;
    assert(ReadDendrogramRTree({"0", "0", "0"}, cursor, empty, error));
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
