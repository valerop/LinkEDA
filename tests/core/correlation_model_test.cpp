#include "../../src/core/correlation_model.h"

#include <cassert>
#include <cmath>
#include <limits>
#include <string>

using rlispstat::core::ComputeCorrelationCellForDataFrameVersion;
using rlispstat::core::ComputePearsonCorrelationForRows;
using rlispstat::core::ComputePooledCorrelationCell;
using rlispstat::core::BuildCorrelationMatrixLayout;
using rlispstat::core::BuildCorrelationWindowLayout;
using rlispstat::core::BuildCorrelationWindowContentLayout;
using rlispstat::core::BuildCorrelationWindowControlState;
using rlispstat::core::BuildCorrelationAddVariableMenuState;
using rlispstat::core::BuildCorrelationCellMenuState;
using rlispstat::core::BuildCorrelationMatrixRenderPlan;
using rlispstat::core::BuildCorrelationVariableMenuState;
using rlispstat::core::CorrelationAddVariableMenuState;
using rlispstat::core::CorrelationCellAt;
using rlispstat::core::CorrelationCellBackground;
using rlispstat::core::CorrelationCellLabel;
using rlispstat::core::CorrelationCellCopyDetailText;
using rlispstat::core::CorrelationCellMenuState;
using rlispstat::core::CorrelationCellResult;
using rlispstat::core::CorrelationCellStatusText;
using rlispstat::core::CorrelationMatrixCellRect;
using rlispstat::core::CorrelationMatrixHit;
using rlispstat::core::CorrelationMatrixHitKind;
using rlispstat::core::CorrelationMatrixPreferredContentSize;
using rlispstat::core::CorrelationMatrixRenderPlan;
using rlispstat::core::CorrelationMatrixRowHeight;
using rlispstat::core::CorrelationAddVariableMenuTitle;
using rlispstat::core::CorrelationMissingModeLockedStatus;
using rlispstat::core::CorrelationNoMoreNumericVariablesTitle;
using rlispstat::core::CorrelationRemoveVariableTitle;
using rlispstat::core::CorrelationPooledAddVariableLockedTitle;
using rlispstat::core::CorrelationPooledAddVariableStatus;
using rlispstat::core::CorrelationPooledRemoveVariableStatus;
using rlispstat::core::CorrelationPooledVariableTypeStatus;
using rlispstat::core::CorrelationPooledVariableEditLockedTitle;
using rlispstat::core::CorrelationVariableMenuTitle;
using rlispstat::core::CorrelationVariableMenuState;
using rlispstat::core::CorrelationVariablesAfterAdd;
using rlispstat::core::CorrelationVariablesAfterRemove;
using rlispstat::core::CorrelationVariablesAvailableToAdd;
using rlispstat::core::CorrelationWindowLayout;
using rlispstat::core::CorrelationWindowControlState;
using rlispstat::core::DataColumn;
using rlispstat::core::DataFrameModel;
using rlispstat::core::HitTestCorrelationMatrix;
using rlispstat::core::Point;

static bool closeEnough(double a, double b, double tolerance = 1.0e-6)
{
    return std::fabs(a - b) < tolerance;
}

int main()
{
    CorrelationCellResult cell;
    cell.r = 0.51234;
    cell.p = 0.004;
    assert(CorrelationCellLabel(&cell, false, false) == ".512");
    assert(CorrelationCellLabel(&cell, true, false) == ".512**");
    assert(CorrelationCellLabel(&cell, true, true) == ".512**\np=.004");

    cell.r = -0.0234;
    cell.p = 0.50;
    assert(CorrelationCellLabel(&cell, true, false) == "-.023");

    cell.status = "diagonal";
    assert(CorrelationCellLabel(&cell, true, true) == "\u2014");

    cell.status = "valid";
    cell.r = std::numeric_limits<double>::quiet_NaN();
    assert(CorrelationCellLabel(&cell, true, true) == "\u2014");
    assert(CorrelationCellLabel(nullptr, true, true) == "\u2014");
    assert(CorrelationNoMoreNumericVariablesTitle() == "No more numeric variables");
    assert(CorrelationAddVariableMenuTitle() == "Add variable");
    assert(CorrelationVariableMenuTitle() == "Variable");
    assert(CorrelationRemoveVariableTitle("mpg") == "Remove mpg");
    assert(CorrelationPooledAddVariableLockedTitle() ==
           "Pooled MI matrices must be refit from R to add variables");
    assert(CorrelationPooledVariableEditLockedTitle() ==
           "Pooled MI matrix: refit from R to add/remove variables");
    assert(CorrelationPooledAddVariableStatus() ==
           "This pooled MI matrix must be refit from R to add variables.");
    assert(CorrelationPooledRemoveVariableStatus() ==
           "This pooled MI matrix must be refit from R to remove variables.");
    assert(CorrelationPooledVariableTypeStatus() ==
           "Variable type changes must be applied in R before refitting this pooled MI matrix.");
    assert(CorrelationMissingModeLockedStatus() ==
           "This matrix contains pooled precomputed results; missing-data mode cannot be changed here.");

    assert(closeEnough(CorrelationMatrixRowHeight(false, false), 32.0));
    assert(closeEnough(CorrelationMatrixRowHeight(true, false), 37.0));
    assert(closeEnough(CorrelationMatrixRowHeight(true, true), 57.0));
    auto layout = BuildCorrelationMatrixLayout(3, true, true);
    assert(closeEnough(layout.width, 720.0));
    assert(closeEnough(layout.height, 360.0));
    assert(closeEnough(layout.rowHeight, 57.0));
    assert(layout.columnHeaderRects.size() == 3);
    assert(layout.rowHeaderRects.size() == 3);
    assert(layout.cellRects.size() == 3);
    assert(closeEnough(layout.columnHeaderRects[0].x, 148.0));
    assert(closeEnough(layout.columnHeaderRects[2].x, 364.0));
    assert(closeEnough(layout.rowHeaderRects[1].y, 127.0));
    assert(closeEnough(CorrelationMatrixCellRect(layout, 2, 1).x, 256.0));
    assert(closeEnough(CorrelationMatrixCellRect(layout, 2, 1).y, 184.0));
    CorrelationMatrixHit addHit = HitTestCorrelationMatrix(
        layout, Point{layout.addVariableRect.x + 2.0, layout.addVariableRect.y + 2.0});
    assert(addHit.kind == CorrelationMatrixHitKind::AddVariable);
    CorrelationMatrixHit variableHit = HitTestCorrelationMatrix(
        layout, Point{layout.columnHeaderRects[1].x + 4.0, layout.columnHeaderRects[1].y + 4.0});
    assert(variableHit.kind == CorrelationMatrixHitKind::Variable);
    assert(variableHit.variableIndex == 1);
    CorrelationMatrixHit rowVariableHit = HitTestCorrelationMatrix(
        layout, Point{layout.rowHeaderRects[2].x + 4.0, layout.rowHeaderRects[2].y + 4.0});
    assert(rowVariableHit.kind == CorrelationMatrixHitKind::Variable);
    assert(rowVariableHit.variableIndex == 2);
    CorrelationMatrixHit cellHit = HitTestCorrelationMatrix(
        layout, Point{layout.cellRects[2][1].x + 4.0, layout.cellRects[2][1].y + 4.0});
    assert(cellHit.kind == CorrelationMatrixHitKind::Cell);
    assert(cellHit.row == 2);
    assert(cellHit.column == 1);
    assert(HitTestCorrelationMatrix(layout, Point{1.0, 1.0}).kind == CorrelationMatrixHitKind::None);
    auto emptySize = CorrelationMatrixPreferredContentSize(0, false, false);
    assert(closeEnough(emptySize.width, 720.0));
    assert(closeEnough(emptySize.height, 360.0));
    CorrelationWindowLayout windowLayout = BuildCorrelationWindowLayout(900.0, 500.0, 1200.0, 800.0);
    assert(closeEnough(windowLayout.maxWidth, 1104.0));
    assert(closeEnough(windowLayout.maxHeight, 688.0));
    assert(closeEnough(windowLayout.targetWidth, 924.0));
    assert(closeEnough(windowLayout.targetHeight, 624.0));
    assert(closeEnough(windowLayout.scrollViewRect.width, 900.0));
    assert(closeEnough(windowLayout.statusRect.y, 14.0));
    CorrelationWindowLayout fixedWindowLayout = BuildCorrelationWindowContentLayout(700.0, 500.0);
    assert(closeEnough(fixedWindowLayout.targetWidth, 700.0));
    assert(closeEnough(fixedWindowLayout.targetHeight, 500.0));
    assert(closeEnough(fixedWindowLayout.titleRect.y, 462.0));
    assert(closeEnough(fixedWindowLayout.scrollViewRect.height, 376.0));
    CorrelationWindowControlState controls = BuildCorrelationWindowControlState(
        "cars", "Pearson Correlation Matrix", {"mpg", "wt"}, "pairwise",
        true, false, true, false, false, 0, "");
    assert(controls.windowTitle == "Pearson Correlation Matrix");
    assert(controls.title == "Pearson Correlation Matrix");
    assert(controls.badge == "cars");
    assert(controls.missingMode == "pairwise");
    assert(controls.missingModeEnabled);
    assert(controls.showP);
    assert(!controls.showPValue);
    assert(controls.showN);
    assert(controls.status == "cars: 2 variables, Pearson, pairwise complete observations");
    CorrelationWindowControlState miControls = BuildCorrelationWindowControlState(
        "cars", "Pearson Correlation Matrix - Multiple Imputation", {"mpg"}, "pairwise",
        true, true, false, false, true, 5, "pooled note");
    assert(miControls.badge == "m = 5");
    assert(miControls.status == "pooled note");
    CorrelationWindowControlState lockedControls = BuildCorrelationWindowControlState(
        "cars", "Pearson Correlation Matrix", {"mpg"}, "listwise",
        false, false, false, true, false, 0, "precomputed note");
    assert(!lockedControls.missingModeEnabled);
    assert(lockedControls.status == "precomputed note");
    auto availableAdd = CorrelationVariablesAvailableToAdd(
        {"mpg", "wt"}, {"mpg", "hp", "wt", "hp", "qsec"});
    assert((availableAdd == std::vector<std::string>({"hp", "qsec"})));
    auto added = CorrelationVariablesAfterAdd({"mpg", "wt"}, "hp",
                                              {"mpg", "hp", "wt"});
    assert(added.ok);
    assert(added.changed);
    assert((added.variables == std::vector<std::string>({"mpg", "wt", "hp"})));
    assert(!CorrelationVariablesAfterAdd({"mpg", "wt"}, "wt", {"mpg", "wt"}).ok);
    assert(!CorrelationVariablesAfterAdd({"mpg", "wt"}, "disp", {"mpg", "wt"}).ok);
    auto removed = CorrelationVariablesAfterRemove({"mpg", "wt", "hp"}, 1);
    assert(removed.ok);
    assert(removed.changed);
    assert((removed.variables == std::vector<std::string>({"mpg", "hp"})));
    assert(!CorrelationVariablesAfterRemove({"mpg"}, 5).ok);
    CorrelationAddVariableMenuState addMenu = BuildCorrelationAddVariableMenuState(
        {"mpg"}, {"mpg", "wt", "hp"}, false);
    assert(!addMenu.locked);
    assert((addMenu.variables == std::vector<std::string>({"wt", "hp"})));
    assert(addMenu.emptyTitle == "No more numeric variables");
    CorrelationAddVariableMenuState lockedAddMenu = BuildCorrelationAddVariableMenuState(
        {"mpg"}, {"mpg", "wt"}, true);
    assert(lockedAddMenu.locked);
    assert(lockedAddMenu.variables.empty());
    assert(lockedAddMenu.lockedTitle == CorrelationPooledAddVariableLockedTitle());
    CorrelationVariableMenuState variableMenu = BuildCorrelationVariableMenuState(
        {"mpg", "wt"}, 1, false);
    assert(variableMenu.valid);
    assert(!variableMenu.locked);
    assert(variableMenu.variable == "wt");
    assert(variableMenu.removeVariableTitle == "Remove wt");
    assert(variableMenu.treatAsNumericTitle == "Treat as Numeric");
    assert(variableMenu.treatAsFactorTitle == "Treat as Factor");
    assert(variableMenu.informationTitle == "Show Variable Information");
    assert(!BuildCorrelationVariableMenuState({"mpg"}, 4, false).valid);
    CorrelationVariableMenuState lockedVariableMenu = BuildCorrelationVariableMenuState(
        {"mpg"}, 0, true);
    assert(lockedVariableMenu.valid);
    assert(lockedVariableMenu.locked);
    assert(lockedVariableMenu.lockedTitle == CorrelationPooledVariableEditLockedTitle());
    CorrelationCellMenuState offDiagonalMenu = BuildCorrelationCellMenuState(1, 0);
    assert(offDiagonalMenu.title == "Correlation cell");
    assert(offDiagonalMenu.openScatterplotTitle == "Open Scatterplot");
    assert(offDiagonalMenu.canOpenScatterplot);
    assert(offDiagonalMenu.copyRTitle == "Copy r");
    assert(offDiagonalMenu.copyDetailTitle == "Copy r, p, N");
    assert(!BuildCorrelationCellMenuState(2, 2).canOpenScatterplot);
    std::vector<CorrelationCellResult> renderCells(4);
    renderCells[1].xVariable = "wt";
    renderCells[1].yVariable = "mpg";
    renderCells[1].r = 0.50;
    renderCells[1].p = 0.04;
    renderCells[1].n = 32;
    renderCells[2].xVariable = "mpg";
    renderCells[2].yVariable = "wt";
    renderCells[2].r = 0.50;
    renderCells[2].p = 0.04;
    renderCells[2].n = 32;
    assert(CorrelationCellAt(renderCells, 2, 1, 0) == &renderCells[2]);
    assert(CorrelationCellAt(renderCells, 2, 4, 0) == nullptr);
    CorrelationMatrixRenderPlan renderPlan = BuildCorrelationMatrixRenderPlan(
        {"mpg", "wt"}, renderCells, 1, 0, true, true, true);
    assert(renderPlan.columnHeaders.size() == 2);
    assert(renderPlan.columnHeaders[0].text == "mpg");
    assert(renderPlan.rowHeaders[1].text == "wt");
    assert(renderPlan.cells.size() == 4);
    assert(renderPlan.cells[0].background == CorrelationCellBackground::Diagonal);
    assert(renderPlan.cells[0].textItems.size() == 1);
    assert(renderPlan.cells[0].textItems[0].text == "\u2014");
    assert(renderPlan.cells[2].background == CorrelationCellBackground::Selected);
    assert(renderPlan.cells[2].textItems.size() == 3);
    assert(renderPlan.cells[2].textItems[0].text.find(".500") != std::string::npos);
    assert(renderPlan.cells[2].textItems[1].muted);
    assert(renderPlan.cells[2].textItems[2].text == "N = 32");
    assert(renderPlan.addVariableLabel == "+ Add variable");
    assert(!renderPlan.showEmptyMessage);
    CorrelationMatrixRenderPlan emptyRenderPlan = BuildCorrelationMatrixRenderPlan(
        {}, {}, -1, -1, false, false, false);
    assert(emptyRenderPlan.showEmptyMessage);
    assert(emptyRenderPlan.emptyMessage == "Add numeric variables to build the matrix.");

    CorrelationCellResult pearson = ComputePearsonCorrelationForRows(
        "x", "y",
        {1, 2, 3, 4, 5},
        {2, 5, 4, 9, 10},
        {1, 2, 3, 4, 5});
    assert(pearson.status == "valid");
    assert(pearson.n == 5);
    assert(closeEnough(pearson.r, 0.9325048, 1.0e-6));
    assert(closeEnough(pearson.p, 0.02083515, 1.0e-6));
    assert(CorrelationCellStatusText(&pearson, 1, 0) ==
           "y x x: r = .933; p = .021; N = 5");
    assert(CorrelationCellStatusText(&pearson, 1, 1).empty());
    assert(CorrelationCellCopyDetailText(&pearson) ==
           "y x x\nr = .933; p = .021; N = 5");
    pearson.detail = "extra detail";
    assert(CorrelationCellStatusText(&pearson, 1, 0).find("extra detail") != std::string::npos);
    assert(CorrelationCellCopyDetailText(&pearson).find("\nextra detail") != std::string::npos);

    CorrelationCellResult zeroVariance = ComputePearsonCorrelationForRows(
        "x", "flat", {1, 2, 3}, {4, 4, 4}, {1, 2, 3});
    assert(zeroVariance.status == "zero_variance");

    DataFrameModel df;
    df.group = "demo";
    df.rows = 5;
    DataColumn x;
    x.name = "x";
    x.type = "numeric";
    x.values = {"1", "2", "3", "4", "5"};
    df.columns.push_back(x);
    DataColumn y;
    y.name = "y";
    y.type = "numeric";
    y.values = {"2", "5", "4", "9", "10"};
    df.columns.push_back(y);
    DataColumn z;
    z.name = "z";
    z.type = "numeric";
    z.values = {"1", "NA", "3", "4", "5"};
    df.columns.push_back(z);

    CorrelationCellResult xy = ComputeCorrelationCellForDataFrameVersion(
        df, {"x", "y"}, "x", "y", "pairwise", 0);
    assert(xy.status == "valid");
    assert(xy.n == 5);
    assert(closeEnough(xy.r, 0.9325048, 1.0e-6));

    CorrelationCellResult diagonal = ComputeCorrelationCellForDataFrameVersion(
        df, {"x", "z"}, "x", "x", "listwise", 0);
    assert(diagonal.status == "diagonal");
    assert(diagonal.n == 4);

    DataFrameModel imputed = df;
    imputed.datasetType = "multiple_imputation";
    imputed.imputationCount = 3;
    imputed.columns[1].imputedMissing = {false, true, false, false, false};
    imputed.columns[1].imputationValuesSparse.resize(3);
    imputed.columns[1].imputationValuesSparse[0][1] = "5";
    imputed.columns[1].imputationValuesSparse[1][1] = "6";
    imputed.columns[1].imputationValuesSparse[2][1] = "7";
    CorrelationCellResult pooled = ComputePooledCorrelationCell(
        imputed, {"x", "y"}, "x", "y", "pairwise");
    assert(pooled.status == "valid");
    assert(pooled.n == 5);
    assert(std::isfinite(pooled.r));
    assert(std::isfinite(pooled.p));
    assert(pooled.detail.find("Rubin") != std::string::npos);

    assert(rlispstat::core::CorrelationWindowTitle() == "Pearson Correlation Matrix");
    assert(rlispstat::core::CorrelationShowStarsButtonTitle() == "Stars");
    assert(rlispstat::core::CorrelationShowPValuesButtonTitle() == "p-values");
    assert(rlispstat::core::CorrelationShowNButtonTitle() == "N");

    return 0;
}
