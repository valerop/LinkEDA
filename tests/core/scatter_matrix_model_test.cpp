#include "../../src/core/scatter_matrix_model.h"

#include <cassert>
#include <cmath>
#include <map>
#include <set>
#include <vector>

using rlispstat::core::CaseId;
using rlispstat::core::Point;
using rlispstat::core::Rect;
using rlispstat::core::BuildScatterMatrixCaseGeometry;
using rlispstat::core::BuildScatterMatrixCreationDialogState;
using rlispstat::core::BuildScatterMatrixPointDrawPlan;
using rlispstat::core::BuildScatterMatrixRenderPlan;
using rlispstat::core::BuildScatterMatrixVariableMenuState;
using rlispstat::core::ScatterMatrixCaseGeometry;
using rlispstat::core::ScatterMatrixCellAtPoint;
using rlispstat::core::ScatterMatrixCellRect;
using rlispstat::core::ScatterMatrixCreationDialogState;
using rlispstat::core::ScatterMatrixLayout;
using rlispstat::core::ScatterMatrixNumericRange;
using rlispstat::core::ScatterMatrixPlotRectForBounds;
using rlispstat::core::ScatterMatrixPointDrawItem;
using rlispstat::core::ScatterMatrixPointRadius;
using rlispstat::core::ScatterMatrixRangeForValues;
using rlispstat::core::ScatterMatrixRenderPlan;
using rlispstat::core::ScatterMatrixVariableMenuState;
using rlispstat::core::ScatterMatrixVariablesAfterAdd;
using rlispstat::core::ScatterMatrixVariablesAfterRemove;
using rlispstat::core::ScatterMatrixVariablesAfterReplacement;
using rlispstat::core::ScatterMatrixVariablesAvailableToAdd;
using rlispstat::core::ScatterMatrixVariablesAvailableForReplacement;
using rlispstat::core::ScatterMatrixVariablesForInputs;
using rlispstat::core::ScatterMatrixVariableSeries;
using rlispstat::core::ScatterMatrixVisibleRows;
using rlispstat::core::SelectScatterMatrixCasesForGesture;

static std::set<CaseId> S(std::initializer_list<CaseId> values)
{
    return std::set<CaseId>(values.begin(), values.end());
}

int main()
{
    ScatterMatrixCreationDialogState creationDialog = BuildScatterMatrixCreationDialogState();
    assert(creationDialog.title == "Scatterplot Matrix");
    assert(creationDialog.informativeText ==
           "Choose numeric variables. The matrix shares row selection and point colors with the other plots in this dataset.");
    assert(creationDialog.createButtonTitle == "Create");
    assert(creationDialog.cancelButtonTitle == "Cancel");
    assert(creationDialog.optionalTitlePlaceholder == "Optional title");
    assert(creationDialog.hintText == "Tip: add or remove variables later from the plot context menu.");
    assert(creationDialog.noActiveDatasetStatus == "Open or import a dataset first.");
    assert(creationDialog.needsTwoNumericVariablesStatus ==
           "The active dataset needs at least two numeric variables.");
    assert(creationDialog.selectAtLeastTwoNumericVariablesStatus ==
           "Select at least two numeric variables.");
    assert(creationDialog.defaultTitle == "Scatterplot matrix");

    Rect plotRect = ScatterMatrixPlotRectForBounds(600.0, 500.0);
    assert(plotRect.x == 72.0);
    assert(plotRect.y == 46.0);
    assert(plotRect.width == 504.0);
    assert(plotRect.height == 414.0);
    Rect tinyPlotRect = ScatterMatrixPlotRectForBounds(80.0, 70.0);
    assert(tinyPlotRect.width == 10.0);
    assert(tinyPlotRect.height == 10.0);
    assert(ScatterMatrixPointRadius(5) == 2.7);
    assert(ScatterMatrixPointRadius(6) == 2.0);

    ScatterMatrixLayout layout{Rect{10.0, 20.0, 300.0, 300.0}, 3};
    ScatterMatrixRenderPlan renderPlan = BuildScatterMatrixRenderPlan(
        {"x", "y", "z"}, "Scatterplot matrix", layout);
    assert(renderPlan.showTitle);
    assert(renderPlan.title.text == "Scatterplot matrix");
    assert(renderPlan.title.rect.x == 10.0);
    assert(renderPlan.title.rect.y == 12.0);
    assert(!renderPlan.showEmptyMessage);
    assert(renderPlan.cells.size() == 9);
    assert(renderPlan.cells[0].diagonal);
    assert(!renderPlan.cells[1].diagonal);
    assert(renderPlan.diagonalLabels.size() == 3);
    assert(renderPlan.diagonalLabels[0].text == "x");
    assert(renderPlan.diagonalLabels[0].rect.x == 15.0);
    assert(renderPlan.diagonalLabels[0].rect.y == 60.0);
    assert(renderPlan.diagonalLabels[0].rect.height == 20.0);
    ScatterMatrixRenderPlan emptyPlan = BuildScatterMatrixRenderPlan(
        {"x"}, "", layout);
    assert(!emptyPlan.showTitle);
    assert(emptyPlan.showEmptyMessage);
    assert(emptyPlan.emptyMessage.text == "Select at least two numeric variables.");
    assert(emptyPlan.emptyMessage.rect.x == 28.0);

    Rect cell = ScatterMatrixCellRect(layout, 1, 2);
    assert(cell.x == 210.0);
    assert(cell.y == 120.0);
    assert(cell.width == 100.0);
    assert(cell.height == 100.0);

    auto hit = ScatterMatrixCellAtPoint(layout, Point{215.0, 125.0});
    assert(hit.has_value());
    assert(hit->row == 1);
    assert(hit->column == 2);

    auto edgeHit = ScatterMatrixCellAtPoint(layout, Point{310.0, 320.0});
    assert(edgeHit.has_value());
    assert(edgeHit->row == 2);
    assert(edgeHit->column == 2);

    std::vector<ScatterMatrixCaseGeometry> cases = {
        ScatterMatrixCaseGeometry{1, 0, 1, Point{50.0, 50.0}},
        ScatterMatrixCaseGeometry{2, 1, 0, Point{60.0, 60.0}},
        ScatterMatrixCaseGeometry{3, 1, 2, Point{230.0, 140.0}},
        ScatterMatrixCaseGeometry{4, 2, 2, Point{280.0, 280.0}},
        ScatterMatrixCaseGeometry{0, 1, 2, Point{232.0, 142.0}}
    };

    assert(SelectScatterMatrixCasesForGesture(
               cases, layout, Rect{40.0, 40.0, 40.0, 40.0}, Point{}, true) == S({1, 2}));

    assert(SelectScatterMatrixCasesForGesture(
               cases, layout, Rect{}, Point{232.0, 142.0}, false) == S({3}));

    assert(SelectScatterMatrixCasesForGesture(
               cases, layout, Rect{}, Point{275.0, 275.0}, false).empty());

    ScatterMatrixNumericRange range = ScatterMatrixRangeForValues({1.0, 3.0, NAN, 5.0});
    assert(range.minimum == 0.8);
    assert(range.maximum == 5.2);

    ScatterMatrixNumericRange constantRange = ScatterMatrixRangeForValues({2.0, 2.0});
    assert(constantRange.minimum == 1.45);
    assert(constantRange.maximum == 2.55);

    ScatterMatrixNumericRange emptyRange = ScatterMatrixRangeForValues({NAN});
    assert(emptyRange.minimum == 0.0);
    assert(emptyRange.maximum == 1.0);

    std::vector<ScatterMatrixVariableSeries> variables = {
        {"x", {1.0, NAN, 3.0, NAN}},
        {"y", {2.0, 2.5, NAN, NAN}},
        {"z", {NAN, 4.0, 5.0, NAN}}
    };
    std::vector<CaseId> visibleRows = ScatterMatrixVisibleRows(variables);
    assert((visibleRows == std::vector<CaseId>{1, 2, 3}));
    assert(ScatterMatrixVisibleRows({variables[0]}).empty());
    ScatterMatrixLayout geometryLayout{Rect{0.0, 0.0, 300.0, 300.0}, 3};
    std::vector<ScatterMatrixCaseGeometry> geometry = BuildScatterMatrixCaseGeometry(
        variables, visibleRows, geometryLayout, 5.0);
    assert(!geometry.empty());
    for (const ScatterMatrixCaseGeometry &entry : geometry) {
        assert(entry.caseId >= 1);
        assert(entry.row != entry.column);
        assert(entry.row < 3);
        assert(entry.column < 3);
        assert(entry.point.x >= 0.0);
        assert(entry.point.x <= 300.0);
        assert(entry.point.y >= 0.0);
        assert(entry.point.y <= 300.0);
    }
    assert(BuildScatterMatrixCaseGeometry({variables[0]}, visibleRows, geometryLayout).empty());

    std::vector<std::string> available = {"mpg", "wt", "hp", "qsec"};
    assert((ScatterMatrixVariablesForInputs({"wt", "bad", "wt", "hp"},
                                            "mpg",
                                            "qsec",
                                            available) ==
            std::vector<std::string>{"wt", "hp"}));
    assert((ScatterMatrixVariablesForInputs({},
                                            "mpg",
                                            "qsec",
                                            available) ==
            std::vector<std::string>{"mpg", "qsec", "wt", "hp"}));
    assert((ScatterMatrixVariablesForInputs({"bad"},
                                            "mpg",
                                            "qsec",
                                            available) ==
            std::vector<std::string>{"mpg", "qsec"}));
    assert((ScatterMatrixVariablesForInputs({"wt"},
                                            "wt",
                                            "hp",
                                            available) ==
            std::vector<std::string>{"wt", "hp"}));

    assert((ScatterMatrixVariablesAvailableToAdd({"mpg", "hp"},
                                                 {"mpg", "wt", "hp", "wt", "qsec"}) ==
            std::vector<std::string>{"wt", "qsec"}));
    assert((ScatterMatrixVariablesAfterAdd({"mpg", "hp"}, "wt", available) ==
            std::vector<std::string>{"mpg", "hp", "wt"}));
    assert((ScatterMatrixVariablesAfterAdd({"mpg", "hp"}, "bad", available) ==
            std::vector<std::string>{"mpg", "hp"}));
    assert((ScatterMatrixVariablesAfterRemove({"mpg", "hp"}, "hp") ==
            std::vector<std::string>{"mpg", "hp"}));
    assert((ScatterMatrixVariablesAfterRemove({"mpg", "hp", "wt"}, "hp") ==
            std::vector<std::string>{"mpg", "wt"}));
    assert((ScatterMatrixVariablesAvailableForReplacement(
                {"mpg", "hp"}, 0, {"mpg", "wt", "hp", "wt", "qsec"}) ==
            std::vector<std::string>{"wt", "qsec"}));
    assert(ScatterMatrixVariablesAvailableForReplacement(
               {"mpg", "hp"}, 2, available).empty());
    assert((ScatterMatrixVariablesAfterReplacement(
                {"mpg", "hp"}, 0, "wt", available) ==
            std::vector<std::string>{"wt", "hp"}));
    assert((ScatterMatrixVariablesAfterReplacement(
                {"mpg", "hp"}, 0, "hp", available) ==
            std::vector<std::string>{"mpg", "hp"}));
    assert((ScatterMatrixVariablesAfterReplacement(
                {"mpg", "hp"}, 4, "wt", available) ==
            std::vector<std::string>{"mpg", "hp"}));
    ScatterMatrixVariableMenuState menuState = BuildScatterMatrixVariableMenuState(
        {"mpg", "hp"}, available);
    assert(menuState.variablesTitle == "Variables");
    assert(menuState.addVariableTitle == "Add variable");
    assert(menuState.removeVariableTitle == "Remove variable");
    assert(menuState.noAvailableVariablesTitle == "No available variables");
    assert(menuState.keepAtLeastTwoVariablesTitle == "Keep at least two variables");
    assert(menuState.openVariablesWindowTitle == "Open variables window");
    assert((menuState.addVariables == std::vector<std::string>{"wt", "qsec"}));
    assert(!menuState.canRemoveVariables);
    assert(menuState.removeVariables.empty());
    ScatterMatrixVariableMenuState removableMenuState = BuildScatterMatrixVariableMenuState(
        {"mpg", "hp", "wt"}, available);
    assert(removableMenuState.canRemoveVariables);
    assert((removableMenuState.removeVariables == std::vector<std::string>{"mpg", "hp", "wt"}));

    std::vector<ScatterMatrixCaseGeometry> pointGeometry = {
        ScatterMatrixCaseGeometry{1, 0, 1, Point{20.0, 30.0}},
        ScatterMatrixCaseGeometry{2, 0, 1, Point{30.0, 40.0}},
        ScatterMatrixCaseGeometry{3, 0, 1, Point{40.0, 50.0}}
    };
    std::vector<ScatterMatrixPointDrawItem> selectedPointPlan = BuildScatterMatrixPointDrawPlan(
        pointGeometry,
        S({3}),
        std::map<CaseId, std::string>{{2, "orange"}},
        std::map<CaseId, std::string>{{1, "Case 1"}, {3, "Case 3"}},
        "selected",
        6);
    assert(selectedPointPlan.size() == 3);
    assert(selectedPointPlan[0].caseId == 1);
    assert(selectedPointPlan[1].caseId == 2);
    assert(selectedPointPlan[2].caseId == 3);
    assert(selectedPointPlan[0].colorName.empty());
    assert(selectedPointPlan[0].alpha == 0.22);
    assert(!selectedPointPlan[0].showLabel);
    assert(selectedPointPlan[1].colorName == "orange");
    assert(selectedPointPlan[1].hasExplicitColor);
    assert(selectedPointPlan[1].alpha == 0.35);
    assert(!selectedPointPlan[1].hasHalo);
    assert(selectedPointPlan[2].selected);
    assert(selectedPointPlan[2].colorName == "black");
    assert(!selectedPointPlan[2].hasExplicitColor);
    assert(selectedPointPlan[2].alpha == 1.0);
    assert(selectedPointPlan[2].hasHalo);
    assert(selectedPointPlan[2].haloRadius == 4.8);
    assert(selectedPointPlan[2].haloAlpha == 0.24);
    assert(selectedPointPlan[2].radius == 2.0);
    assert(selectedPointPlan[2].showLabel);
    assert(selectedPointPlan[2].label == "Case 3");

    std::vector<ScatterMatrixPointDrawItem> unselectedPointPlan = BuildScatterMatrixPointDrawPlan(
        pointGeometry,
        S({}),
        std::map<CaseId, std::string>{{2, "orange"}},
        std::map<CaseId, std::string>{{1, "Case 1"}},
        "all",
        3);
    assert(unselectedPointPlan.size() == 3);
    assert(unselectedPointPlan[0].alpha == 0.72);
    assert(unselectedPointPlan[0].radius == 2.7);
    assert(unselectedPointPlan[0].showLabel);
    assert(unselectedPointPlan[0].label == "Case 1");
    assert(unselectedPointPlan[1].colorName == "orange");
    assert(unselectedPointPlan[1].alpha == 0.78);

    return 0;
}
