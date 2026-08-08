#include "../../src/core/scatterplot_model.h"

#include <cassert>
#include <cmath>
#include <map>
#include <optional>
#include <set>
#include <vector>

using rlispstat::core::BuildScatterplotPointDrawPlan;
using rlispstat::core::BuildScatterplotOverlayLinePlan;
using rlispstat::core::BuildScatterplotCaseGeometry;
using rlispstat::core::BuildScatterplotCreationDialogState;
using rlispstat::core::BuildScatterplotMenuState;
using rlispstat::core::BuildScatterplotPointDrawInputs;
using rlispstat::core::BuildScatterplotRenderPlan;
using rlispstat::core::BuildSmoothCurveDrawItems;
using rlispstat::core::CaseId;
using rlispstat::core::DataColumn;
using rlispstat::core::DataFrameModel;
using rlispstat::core::DataViewport;
using rlispstat::core::FitSimpleLinearModel;
using rlispstat::core::FitScatterplotLineForViewport;
using rlispstat::core::NearestCaseToPoint;
using rlispstat::core::Point;
using rlispstat::core::Rect;
using rlispstat::core::ScatterplotCaseGeometry;
using rlispstat::core::ScatterplotBrushMenuOptions;
using rlispstat::core::ScatterplotChooseLabelColumnOption;
using rlispstat::core::ScatterplotClosePlotOption;
using rlispstat::core::ScatterplotCreationDialogState;
using rlispstat::core::ScatterplotDefaultTitle;
using rlispstat::core::ScatterplotNoValidXYStatus;
using rlispstat::core::ScatterplotAddIndependentVariableTitle;
using rlispstat::core::ScatterplotReplacePredictorTitle;
using rlispstat::core::ScatterplotSetActiveModelTitle;
using rlispstat::core::ScatterplotImputationGlyph;
using rlispstat::core::ScatterplotImputationGlyphForPoint;
using rlispstat::core::ScatterplotMouseModeMenuOptions;
using rlispstat::core::ScatterplotPlotMenuOptions;
using rlispstat::core::ScatterplotPointDrawInput;
using rlispstat::core::ScatterplotPointDrawItem;
using rlispstat::core::ScatterplotFittedLine;
using rlispstat::core::ScatterplotHasOverlaySource;
using rlispstat::core::ScatterplotLabelDisplayMenuOptions;
using rlispstat::core::ScatterplotOverlayLineItem;
using rlispstat::core::ScatterplotOverlaySpec;
using rlispstat::core::ScatterplotMenuState;
using rlispstat::core::ScatterplotPointValue;
using rlispstat::core::ScatterplotRenderInput;
using rlispstat::core::ScatterplotRenderPlan;
using rlispstat::core::ScatterplotSelectionActionMenuOptions;
using rlispstat::core::ScatterplotSelectionModeMenuOptions;
using rlispstat::core::ScatterplotViewportIncludingImputations;
using rlispstat::core::ScatterplotViewMenuOptions;
using rlispstat::core::SelectCasesForGesture;
using rlispstat::core::SelectCasesInBrush;
using rlispstat::core::SimpleLinearFitResult;
using rlispstat::core::SmoothCurveData;
using rlispstat::core::SmoothCurveScope;
using rlispstat::core::ScatterplotSmoothCurveDrawItem;

static bool closeEnough(double a, double b)
{
    return std::fabs(a - b) < 1.0e-9;
}

static std::set<CaseId> S(std::initializer_list<CaseId> values)
{
    return std::set<CaseId>(values.begin(), values.end());
}

int main()
{
    ScatterplotCreationDialogState creationDialog = BuildScatterplotCreationDialogState();
    assert(creationDialog.title == "Scatterplot");
    assert(creationDialog.informativeText ==
           "Choose variables from the active dataset. The new plot will share row selection with existing plots in this dataset.");
    assert(creationDialog.createButtonTitle == "Create");
    assert(creationDialog.cancelButtonTitle == "Cancel");
    assert(creationDialog.optionalTitlePlaceholder == "Optional title");
    assert(creationDialog.needsTwoNumericVariablesStatus ==
           "The active dataset needs at least two numeric variables.");
    assert(creationDialog.noActiveDatasetStatus ==
           "Open a LinkEDA plot first, or use ls_register_dataset() and ls_new_scatterplot() from R.");
    assert(ScatterplotDefaultTitle("wt", "mpg") == "mpg vs wt");

    std::vector<ScatterplotCaseGeometry> cases = {
        ScatterplotCaseGeometry{1, Point{10.0, 10.0}, false, Rect{}},
        ScatterplotCaseGeometry{2, Point{30.0, 30.0}, false, Rect{}},
        ScatterplotCaseGeometry{3, Point{60.0, 60.0}, true, Rect{55.0, 55.0, 12.0, 8.0}},
        ScatterplotCaseGeometry{0, Point{20.0, 20.0}, false, Rect{}}
    };

    assert(SelectCasesInBrush(cases, Rect{0.0, 0.0, 35.0, 35.0}) == S({1, 2}));
    assert(SelectCasesInBrush(cases, Rect{66.0, 60.0, 3.0, 3.0}, 2.0) == S({3}));
    assert(SelectCasesInBrush(cases, Rect{80.0, 80.0, 10.0, 10.0}).empty());

    auto nearest1 = NearestCaseToPoint(cases, Point{12.0, 12.0}, 8.0);
    assert(nearest1.has_value() && *nearest1 == 1);

    auto nearestGlyph = NearestCaseToPoint(cases, Point{68.0, 59.0}, 8.0, 2.0);
    assert(nearestGlyph.has_value() && *nearestGlyph == 3);

    auto none = NearestCaseToPoint(cases, Point{200.0, 200.0}, 8.0);
    assert(!none.has_value());

    assert(SelectCasesForGesture(cases, Rect{0.0, 0.0, 35.0, 35.0}, Point{0.0, 0.0}, true) == S({1, 2}));
    assert(SelectCasesForGesture(cases, Rect{0.0, 0.0, 0.0, 0.0}, Point{12.0, 12.0}, false) == S({1}));

    std::vector<ScatterplotPointDrawInput> drawInputs = {
        ScatterplotPointDrawInput{1, Point{10.0, 10.0}, false, ScatterplotImputationGlyph{}},
        ScatterplotPointDrawInput{2, Point{20.0, 20.0}, false, ScatterplotImputationGlyph{}},
        ScatterplotPointDrawInput{3, Point{30.0, 30.0}, true, ScatterplotImputationGlyph{true, 1, Rect{25.0, 26.0, 12.0, 8.0}}},
        ScatterplotPointDrawInput{0, Point{40.0, 40.0}, false, ScatterplotImputationGlyph{}}
    };
    std::vector<ScatterplotPointDrawItem> selectedDrawPlan = BuildScatterplotPointDrawPlan(
        drawInputs,
        S({1}),
        std::map<CaseId, std::string>{{2, "orange"}, {3, "blue"}},
        std::map<CaseId, std::string>{{1, "Case 1"}, {3, "Case 3"}},
        "selected");
    assert(selectedDrawPlan.size() == 4);
    assert(selectedDrawPlan[0].caseId == 1);
    assert(selectedDrawPlan[0].selected);
    assert(selectedDrawPlan[0].colorName == "black");
    assert(!selectedDrawPlan[0].hasExplicitColor);
    assert(selectedDrawPlan[0].fillAlpha == 1.0);
    assert(selectedDrawPlan[0].strokeAlpha == 0.90);
    assert(selectedDrawPlan[0].radius == 3.6);
    assert(selectedDrawPlan[0].hasHalo);
    assert(selectedDrawPlan[0].haloRadius == 6.0);
    assert(selectedDrawPlan[0].showLabel);
    assert(selectedDrawPlan[0].label == "Case 1");
    assert(selectedDrawPlan[1].colorName == "orange");
    assert(selectedDrawPlan[1].hasExplicitColor);
    assert(selectedDrawPlan[1].fillAlpha == 0.35);
    assert(selectedDrawPlan[1].strokeAlpha == 0.25);
    assert(!selectedDrawPlan[1].hasHalo);
    assert(selectedDrawPlan[2].hasImputationGlyph);
    assert(selectedDrawPlan[2].colorName == "blue");
    assert(closeEnough(selectedDrawPlan[2].labelAnchor.x, 31.0));
    assert(closeEnough(selectedDrawPlan[2].labelAnchor.y, 30.0));
    assert(!selectedDrawPlan[2].showLabel);
    assert(selectedDrawPlan[3].caseId == 0);
    assert(selectedDrawPlan[3].colorName.empty());
    assert(selectedDrawPlan[3].fillAlpha == 0.22);

    std::vector<ScatterplotPointDrawItem> allLabelDrawPlan = BuildScatterplotPointDrawPlan(
        drawInputs,
        S({}),
        std::map<CaseId, std::string>{{2, "orange"}},
        std::map<CaseId, std::string>{{1, "Case 1"}},
        "all",
        true);
    assert(allLabelDrawPlan.size() == 3);
    assert(allLabelDrawPlan[0].caseId == 1);
    assert(allLabelDrawPlan[0].fillAlpha == 0.72);
    assert(allLabelDrawPlan[0].strokeAlpha == 0.42);
    assert(allLabelDrawPlan[0].showLabel);
    assert(allLabelDrawPlan[1].caseId == 2);
    assert(allLabelDrawPlan[1].fillAlpha == 0.78);
    assert(allLabelDrawPlan[2].caseId == 3);

    DataFrameModel imputed;
    imputed.datasetType = "multiple_imputation";
    imputed.imputationCount = 2;
    imputed.imputationDisplayMode = "all";
    imputed.rows = 2;

    DataColumn x;
    x.name = "x";
    x.type = "numeric";
    x.values = {"18", "NA"};
    x.imputedMissing = {false, true};
    x.imputationValues = {{"18", "20"}, {"18", "21"}};
    imputed.columns.push_back(x);

    DataColumn y;
    y.name = "y";
    y.type = "numeric";
    y.values = {"1", "2"};
    y.imputedMissing = {false, false};
    y.imputationValues = {{"1", "2"}, {"1", "2"}};
    imputed.columns.push_back(y);

    std::vector<ScatterplotPointValue> pointValues = {
        ScatterplotPointValue{1, 18.0, 1.0},
        ScatterplotPointValue{2, 20.0, 2.0}
    };
    std::optional<DataViewport> viewport =
        ScatterplotViewportIncludingImputations(imputed, imputed.columns[0], imputed.columns[1], pointValues, "central80");
    assert(viewport.has_value());
    assert(closeEnough(viewport->xmin, 17.855));
    assert(closeEnough(viewport->xmax, 21.045));
    assert(closeEnough(viewport->ymin, 0.95));
    assert(closeEnough(viewport->ymax, 2.05));

    ScatterplotImputationGlyph glyph = ScatterplotImputationGlyphForPoint(
        imputed,
        imputed.columns[0],
        imputed.columns[1],
        pointValues[1],
        DataViewport{17.0, 22.0, 0.0, 3.0},
        Rect{0.0, 0.0, 500.0, 300.0},
        "central80");
    assert(glyph.hasImputation);
    assert(glyph.axisMask == 1);
    assert(closeEnough(glyph.rect.width, 80.0));
    assert(closeEnough(glyph.rect.height, 9.0));
    assert(closeEnough(glyph.rect.x, 310.0));
    assert(closeEnough(glyph.rect.y, 95.5));

    std::vector<ScatterplotPointDrawInput> builtDrawInputs =
        BuildScatterplotPointDrawInputs(pointValues,
                                        DataViewport{17.0, 22.0, 0.0, 3.0},
                                        Rect{0.0, 0.0, 500.0, 300.0},
                                        &imputed,
                                        &imputed.columns[0],
                                        &imputed.columns[1],
                                        "central80");
    assert(builtDrawInputs.size() == 2);
    assert(closeEnough(builtDrawInputs[0].point.x, 100.0));
    assert(closeEnough(builtDrawInputs[0].point.y, 200.0));
    assert(!builtDrawInputs[0].hasImputationGlyph);
    assert(builtDrawInputs[1].hasImputationGlyph);
    assert(closeEnough(builtDrawInputs[1].imputationGlyph.rect.x, glyph.rect.x));
    assert(closeEnough(builtDrawInputs[1].imputationGlyph.rect.y, glyph.rect.y));

    std::vector<ScatterplotCaseGeometry> builtHitGeometry =
        BuildScatterplotCaseGeometry(pointValues,
                                     DataViewport{17.0, 22.0, 0.0, 3.0},
                                     Rect{0.0, 0.0, 500.0, 300.0},
                                     &imputed,
                                     &imputed.columns[0],
                                     &imputed.columns[1],
                                     "central80");
    assert(builtHitGeometry.size() == 2);
    assert(!builtHitGeometry[0].hasGlyphRect);
    assert(builtHitGeometry[1].hasGlyphRect);
    assert(SelectCasesInBrush(builtHitGeometry, Rect{318.0, 97.0, 6.0, 4.0}, 0.0) == S({2}));

    std::vector<ScatterplotPointDrawInput> skippedDrawInputs =
        BuildScatterplotPointDrawInputs({ScatterplotPointValue{0, 1.0, 1.0}, pointValues[0]},
                                        DataViewport{0.0, 2.0, 0.0, 2.0},
                                        Rect{0.0, 0.0, 20.0, 20.0},
                                        nullptr,
                                        nullptr,
                                        nullptr,
                                        "central80",
                                        true);
    assert(skippedDrawInputs.size() == 1);
    assert(skippedDrawInputs[0].caseId == 1);

    ScatterplotRenderInput imputedRenderInput;
    imputedRenderInput.points = pointValues;
    imputedRenderInput.viewport = DataViewport{17.0, 22.0, 0.0, 3.0};
    imputedRenderInput.plotRect = Rect{0.0, 0.0, 500.0, 300.0};
    imputedRenderInput.selectedRows = S({2});
    imputedRenderInput.rowColors = std::map<CaseId, std::string>{{2, "orange"}};
    imputedRenderInput.rowLabels = std::map<CaseId, std::string>{{2, "Case 2"}};
    imputedRenderInput.labelDisplayMode = "selected";
    imputedRenderInput.imputationDataFrame = &imputed;
    imputedRenderInput.xColumn = &imputed.columns[0];
    imputedRenderInput.yColumn = &imputed.columns[1];
    imputedRenderInput.imputationUncertaintyMode = "central80";
    ScatterplotRenderPlan imputedRenderPlan = BuildScatterplotRenderPlan(imputedRenderInput);
    assert(imputedRenderPlan.points.size() == 2);
    assert(imputedRenderPlan.points[1].selected);
    assert(imputedRenderPlan.points[1].hasImputationGlyph);
    assert(imputedRenderPlan.points[1].colorName == "orange");
    assert(imputedRenderPlan.points[1].showLabel);
    assert(imputedRenderPlan.points[1].label == "Case 2");

    DataFrameModel active = imputed;
    active.imputationDisplayMode = "version";
    assert(!ScatterplotViewportIncludingImputations(active, active.columns[0], active.columns[1], pointValues, "central80").has_value());
    assert(!ScatterplotImputationGlyphForPoint(active, active.columns[0], active.columns[1], pointValues[1],
                                               DataViewport{17.0, 22.0, 0.0, 3.0},
                                               Rect{0.0, 0.0, 500.0, 300.0},
                                               "central80").hasImputation);

    std::vector<ScatterplotPointValue> fitPoints = {
        ScatterplotPointValue{1, 0.0, 1.0},
        ScatterplotPointValue{2, 1.0, 3.0},
        ScatterplotPointValue{3, 2.0, 5.0},
        ScatterplotPointValue{4, 3.0, 7.0}
    };
    SimpleLinearFitResult allFit = FitSimpleLinearModel(fitPoints, {}, "all", 5);
    assert(allFit.ok);
    assert(allFit.n == 4);
    assert(allFit.excluded == 1);
    assert(closeEnough(allFit.intercept, 1.0));
    assert(closeEnough(allFit.slope, 2.0));
    assert(closeEnough(allFit.r2, 1.0));
    assert(closeEnough(allFit.sse, 0.0));
    assert(closeEnough(allFit.sigma, 0.0));
    assert(closeEnough(allFit.xMean, 1.5));
    assert(closeEnough(allFit.yMean, 4.0));

    SimpleLinearFitResult selectedFit = FitSimpleLinearModel(fitPoints, S({2, 3}), "selected", 4);
    assert(selectedFit.ok);
    assert(selectedFit.n == 2);
    assert(selectedFit.excluded == 2);
    assert(closeEnough(selectedFit.intercept, 1.0));
    assert(closeEnough(selectedFit.slope, 2.0));

    SimpleLinearFitResult unselectedFit = FitSimpleLinearModel(fitPoints, S({2, 3}), "unselected", 4);
    assert(unselectedFit.ok);
    assert(unselectedFit.n == 2);
    assert(unselectedFit.excluded == 2);
    assert(closeEnough(unselectedFit.intercept, 1.0));
    assert(closeEnough(unselectedFit.slope, 2.0));

    ScatterplotFittedLine fittedLine = FitScatterplotLineForViewport(
        fitPoints,
        DataViewport{0.0, 3.0, 0.0, 8.0});
    assert(fittedLine.ok);
    assert(fittedLine.n == 4);
    assert(closeEnough(fittedLine.intercept, 1.0));
    assert(closeEnough(fittedLine.slope, 2.0));
    assert(closeEnough(fittedLine.start.x, 0.0));
    assert(closeEnough(fittedLine.start.y, 1.0));
    assert(closeEnough(fittedLine.end.x, 3.0));
    assert(closeEnough(fittedLine.end.y, 7.0));
    assert(!FitScatterplotLineForViewport(
        fitPoints,
        DataViewport{0.0, 0.0, 0.0, 1.0}).ok);

    std::vector<ScatterplotPointValue> overlayPoints = {
        ScatterplotPointValue{1, 0.0, 1.0},
        ScatterplotPointValue{2, 1.0, 3.0},
        ScatterplotPointValue{3, 2.0, 5.0},
        ScatterplotPointValue{4, 3.0, 7.0},
        ScatterplotPointValue{5, 4.0, 9.0},
        ScatterplotPointValue{6, 5.0, 11.0}
    };
    std::vector<ScatterplotOverlayLineItem> overlayPlan = BuildScatterplotOverlayLinePlan(
        overlayPoints,
        std::vector<ScatterplotOverlaySpec>{
            ScatterplotOverlaySpec{"lm", "all", true},
            ScatterplotOverlaySpec{"lm", "selected", true},
            ScatterplotOverlaySpec{"lm", "color", true},
            ScatterplotOverlaySpec{"loess", "all", true},
            ScatterplotOverlaySpec{"lm", "all", false}
        },
        S({2, 3, 4}),
        std::map<CaseId, std::string>{{3, "orange"}, {4, "orange"}, {5, "blue"}, {6, "blue"}},
        "purple",
        DataViewport{0.0, 5.0, 0.0, 12.0});
    assert(overlayPlan.size() == 5);
    assert(overlayPlan[0].useDefaultDarkColor);
    assert(overlayPlan[0].colorName.empty());
    assert(overlayPlan[0].lineWidth == 2.0);
    assert(!overlayPlan[0].dashed);
    assert(overlayPlan[1].colorName == "purple");
    assert(overlayPlan[1].dashed);
    assert(overlayPlan[1].lineWidth == 2.6);
    int blackLines = 0;
    int orangeLines = 0;
    int blueLines = 0;
    for (const ScatterplotOverlayLineItem &line : overlayPlan) {
        if (line.colorName == "black") ++blackLines;
        if (line.colorName == "orange") ++orangeLines;
        if (line.colorName == "blue") ++blueLines;
    }
    assert(blackLines == 1);
    assert(orangeLines == 1);
    assert(blueLines == 1);
    std::vector<ScatterplotOverlaySpec> visibleOverlays = {
        ScatterplotOverlaySpec{"lm", "all", true},
        ScatterplotOverlaySpec{"lm", "selected", true},
        ScatterplotOverlaySpec{"lm", "color", false}
    };
    assert(ScatterplotHasOverlaySource(visibleOverlays, "all"));
    assert(!ScatterplotHasOverlaySource(visibleOverlays, "color"));
    ScatterplotMenuState menuState = BuildScatterplotMenuState(
        {"mpg", "wt", "qsec"},
        "wt",
        "mpg",
        "cars",
        "case_label",
        "selected",
        visibleOverlays,
        true,
        "iqr");
    assert(menuState.variables.xOptions.size() == 3);
    assert(menuState.variables.xOptions[1].checked);
    assert(menuState.variables.xOptions[1].command == "CHANGE_X_VARIABLE|wt");
    assert(menuState.variables.yOptions[0].checked);
    assert(menuState.variables.openVariablesWindow.command == "OPEN_VARIABLES_WINDOW|cars");
    assert(menuState.mouseModeOptions.back().command == "SET_MODE_ZOOM");
    assert(menuState.selectionModeOptions[2].command == "SET_SELECTION_SUBTRACT");
    assert(menuState.selectionActionOptions[1].command == "INVERT_SELECTION");
    assert(menuState.brushOptions[0].command == "SET_MODE_BRUSH");
    assert(menuState.viewOptions[0].command == "RESET_ZOOM");
    assert(menuState.showImputationUncertaintyOptions);
    assert(menuState.imputationUncertaintyOptions.size() == 4);
    assert(menuState.imputationUncertaintyOptions[1].checked);
    assert(menuState.chooseLabelColumn.title == "Label column: case_label");
    assert(menuState.labelDisplayOptions[1].checked);
    assert(menuState.overlayOptions[0].checked);
    assert(menuState.overlayOptions[1].checked);
    assert(menuState.overlayOptions[2].checked);
    assert(!menuState.overlayOptions[3].checked);
    assert(menuState.clearOverlays.command == "CLEAR_OVERLAYS");
    assert(menuState.analysisOptions[0].title == "Correlation: mpg with wt");
    assert(menuState.analysisOptions[1].title == "Linear model: mpg ~ wt");
    assert(menuState.plotOptions[2].command == "PLOT_NEW_TIME_SERIES");
    assert(menuState.plotOptions[4].command == "PLOT_NEW_PARALLEL_COORDINATES");
    assert(menuState.plotOptions[7].command == "PLOT_NEW_LINKED_BAR_CHART");
    assert(menuState.closePlot.command == "CLOSE_PLOT");
    assert(ScatterplotMouseModeMenuOptions(false).size() == 5);
    assert(ScatterplotMouseModeMenuOptions(true).back().command == "SET_MODE_ZOOM");
    assert(ScatterplotSelectionModeMenuOptions().front().command == "SET_SELECTION_REPLACE");
    assert(ScatterplotSelectionActionMenuOptions().front().title == "Clear selection");
    assert(ScatterplotSelectionActionMenuOptions().back().command == "SELECT_ALL_VISIBLE");
    assert(ScatterplotBrushMenuOptions()[1].command == "BRUSH_LARGER");
    assert(ScatterplotViewMenuOptions()[1].command == "RESCALE");
    assert(ScatterplotChooseLabelColumnOption("").title == "Choose label column...");
    assert(ScatterplotChooseLabelColumnOption("id").title == "Label column: id");
    assert(ScatterplotLabelDisplayMenuOptions("all")[2].checked);
    assert(ScatterplotPlotMenuOptions()[3].command == "PLOT_NEW_LINKED_SCATTER_MATRIX");
    assert(ScatterplotClosePlotOption().command == "CLOSE_PLOT");

    ScatterplotRenderInput renderInput;
    renderInput.points = overlayPoints;
    renderInput.viewport = DataViewport{0.0, 5.0, 0.0, 12.0};
    renderInput.plotRect = Rect{0.0, 0.0, 500.0, 300.0};
    renderInput.selectedRows = S({2, 3, 4});
    renderInput.rowColors = std::map<CaseId, std::string>{{3, "orange"}, {4, "orange"}};
    renderInput.rowLabels = std::map<CaseId, std::string>{{2, "Case 2"}};
    renderInput.labelDisplayMode = "selected";
    renderInput.overlays = std::vector<ScatterplotOverlaySpec>{
        ScatterplotOverlaySpec{"lm", "all", true},
        ScatterplotOverlaySpec{"lm", "selected", true}
    };
    renderInput.selectedColorName = "purple";
    ScatterplotRenderPlan renderPlan = BuildScatterplotRenderPlan(renderInput);
    assert(renderPlan.points.size() == 6);
    assert(renderPlan.points[1].selected);
    assert(renderPlan.points[1].showLabel);
    assert(closeEnough(renderPlan.points[1].point.x, 100.0));
    assert(closeEnough(renderPlan.points[1].point.y, 225.0));
    assert(renderPlan.overlayLines.size() == 2);
    assert(closeEnough(renderPlan.overlayLines[0].start.x, 0.0));
    assert(closeEnough(renderPlan.overlayLines[0].start.y, 275.0));
    assert(closeEnough(renderPlan.overlayLines[0].end.x, 500.0));
    assert(closeEnough(renderPlan.overlayLines[0].end.y, 25.0));
    assert(renderPlan.overlayLines[1].colorName == "purple");
    assert(renderPlan.overlayLines[1].dashed);

    SimpleLinearFitResult tooFew = FitSimpleLinearModel(fitPoints, S({1}), "selected", 4);
    assert(!tooFew.ok);
    assert(tooFew.warning.find("Select at least two") != std::string::npos);

    SimpleLinearFitResult flatX = FitSimpleLinearModel({
        ScatterplotPointValue{1, 2.0, 1.0},
        ScatterplotPointValue{2, 2.0, 2.0},
        ScatterplotPointValue{3, 2.0, 3.0}
    });
    assert(!flatX.ok);
    assert(flatX.warning.find("no variation") != std::string::npos);

    assert(ScatterplotNoValidXYStatus() == "This scatterplot does not have a valid X/Y pair for a simple linear model.");
    assert(ScatterplotAddIndependentVariableTitle() == "Add independent variable");
    assert(ScatterplotReplacePredictorTitle() == "Replace predictor");
    assert(ScatterplotSetActiveModelTitle() == "Set as active model");

    // ---- Smooth curve tests ----
    {
    const DataViewport kViewport{0.0, 10.0, 0.0, 10.0};
    const Rect kPlotRect{50.0, 25.0, 500.0, 500.0};
    std::vector<SmoothCurveData> curves;

    // empty input
    {
        std::vector<ScatterplotSmoothCurveDrawItem> items = BuildSmoothCurveDrawItems(curves, kViewport, kPlotRect);
        assert(items.empty());
    }

    // valid curve
    {
        SmoothCurveData c;
        c.scope = SmoothCurveScope::Overall;
        c.groupId = ".";
        c.ok = true;
        c.x = {0.0, 5.0, 10.0};
        c.y = {0.0, 5.0, 10.0};
        curves.push_back(c);
    }
    {
        std::vector<ScatterplotSmoothCurveDrawItem> items = BuildSmoothCurveDrawItems(curves, kViewport, kPlotRect);
        assert(items.size() == 1);
        assert(items[0].points.size() == 3);
        assert(closeEnough(items[0].points[0].x, 50.0));
        assert(closeEnough(items[0].points[0].y, 525.0));
        assert(closeEnough(items[0].points[2].x, 550.0));
        assert(closeEnough(items[0].points[2].y, 25.0));
        assert(!items[0].dashed);
        assert(items[0].colorName.empty());
    }

    // selection curve is dashed
    {
        SmoothCurveData c;
        c.scope = SmoothCurveScope::Selection;
        c.groupId = ".";
        c.ok = true;
        c.x = {1.0, 2.0, 3.0};
        c.y = {1.0, 2.0, 3.0};
        curves.push_back(c);
    }
    {
        std::vector<ScatterplotSmoothCurveDrawItem> items = BuildSmoothCurveDrawItems(curves, kViewport, kPlotRect);
        assert(items.size() == 2);
        assert(items[1].dashed);
    }

    // color group curve gets colorName from groupId
    {
        SmoothCurveData c;
        c.scope = SmoothCurveScope::ColorGroup;
        c.groupId = "blue";
        c.ok = true;
        c.x = {2.0, 4.0, 6.0};
        c.y = {2.0, 4.0, 6.0};
        curves.push_back(c);
    }
    {
        std::vector<ScatterplotSmoothCurveDrawItem> items = BuildSmoothCurveDrawItems(curves, kViewport, kPlotRect);
        assert(items.size() == 3);
        assert(items[2].colorName == "blue");
    }

    // curve with ok=false is skipped
    {
        SmoothCurveData c;
        c.scope = SmoothCurveScope::Overall;
        c.ok = false;
        c.x = {1.0, 2.0};
        c.y = {1.0, 2.0};
        curves.push_back(c);
    }
    {
        std::vector<ScatterplotSmoothCurveDrawItem> items = BuildSmoothCurveDrawItems(curves, kViewport, kPlotRect);
        assert(items.size() == 3);
    }

    // curve with non-finite values is skipped
    {
        SmoothCurveData c;
        c.scope = SmoothCurveScope::Overall;
        c.ok = true;
        c.x = {1.0, NAN, 3.0};
        c.y = {1.0, 2.0, 3.0};
        curves.push_back(c);
    }
    {
        std::vector<ScatterplotSmoothCurveDrawItem> items = BuildSmoothCurveDrawItems(curves, kViewport, kPlotRect);
        assert(items.size() == 3);
    }

    // curve with only 1 point is skipped
    {
        SmoothCurveData c;
        c.scope = SmoothCurveScope::Overall;
        c.ok = true;
        c.x = {5.0};
        c.y = {5.0};
        curves.push_back(c);
    }
    {
        std::vector<ScatterplotSmoothCurveDrawItem> items = BuildSmoothCurveDrawItems(curves, kViewport, kPlotRect);
        assert(items.size() == 3);
    }

    // mismatched x/y sizes skipped
    {
        SmoothCurveData c;
        c.scope = SmoothCurveScope::Overall;
        c.ok = true;
        c.x = {1.0, 2.0};
        c.y = {1.0};
        curves.push_back(c);
    }
    {
        std::vector<ScatterplotSmoothCurveDrawItem> items = BuildSmoothCurveDrawItems(curves, kViewport, kPlotRect);
        assert(items.size() == 3);
    }

    // invalid viewport returns empty
    {
        std::vector<ScatterplotSmoothCurveDrawItem> items = BuildSmoothCurveDrawItems(curves, DataViewport{0, 0, 0, 10}, kPlotRect);
        assert(items.empty());
    }

    // SmoothCurveScope enum values
    assert(static_cast<int>(SmoothCurveScope::Overall) == 0);
    assert(static_cast<int>(SmoothCurveScope::Selection) == 1);
    assert(static_cast<int>(SmoothCurveScope::ColorGroup) == 2);

    // BuildScatterplotMenuState with smoothCurves
    {
        std::vector<SmoothCurveData> testCurves;
        testCurves.push_back(SmoothCurveData{SmoothCurveScope::Overall, "", {}, {}, true, ""});
        std::vector<ScatterplotOverlaySpec> emptyOverlays;
        ScatterplotMenuState menu = BuildScatterplotMenuState({}, "x", "y", "g", "", "none", emptyOverlays,
                                                              false, "central80", testCurves);
        assert(menu.smoothOptions.size() == 3);
        assert(menu.smoothOptions[0].value == "overall");
        assert(menu.smoothOptions[0].checked == true);
        assert(menu.smoothOptions[1].value == "selected");
        assert(menu.smoothOptions[1].checked == false);

        testCurves.push_back(SmoothCurveData{SmoothCurveScope::Selection, ".", {}, {}, false, "needed"});
        ScatterplotMenuState pendingMenu = BuildScatterplotMenuState({}, "x", "y", "g", "", "none", emptyOverlays,
                                                                     false, "central80", testCurves);
        assert(pendingMenu.smoothOptions[1].checked == true);
    }
    }

    std::fprintf(stderr, "All scatterplot model tests PASSED\n");
    return 0;
}
