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
using rlispstat::core::BuildScatterplotImputationPointSets;
using rlispstat::core::BuildRegressionDiagnosticSmoothPointSets;
using rlispstat::core::BuildSmoothCurveDrawItems;
using rlispstat::core::CaseId;
using rlispstat::core::DataColumn;
using rlispstat::core::DataFrameModel;
using rlispstat::core::DataViewport;
using rlispstat::core::FitSimpleLinearModel;
using rlispstat::core::FitScatterplotLineForViewport;
using rlispstat::core::NearestCaseToPoint;
using rlispstat::core::Point;
using rlispstat::core::PendingSmoothCurve;
using rlispstat::core::Rect;
using rlispstat::core::RegressionDiagnosticSupportsAddedLines;
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
using rlispstat::core::ScatterplotPointImputationValues;
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
using rlispstat::core::ScatterplotViewportIncludingPointImputations;
using rlispstat::core::ScatterplotViewMenuOptions;
using rlispstat::core::SelectCasesForGesture;
using rlispstat::core::SelectCasesInBrush;
using rlispstat::core::SimpleLinearFitResult;
using rlispstat::core::SmoothCurveData;
using rlispstat::core::SmoothCurveScope;
using rlispstat::core::ToggleSmoothCurveScopePending;
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
    assert(selectedDrawPlan[0].caseId == 2);
    assert(selectedDrawPlan[0].colorName == "orange");
    assert(selectedDrawPlan[0].hasExplicitColor);
    assert(selectedDrawPlan[0].fillAlpha == 0.35);
    assert(selectedDrawPlan[0].strokeAlpha == 0.25);
    assert(!selectedDrawPlan[0].hasHalo);
    assert(selectedDrawPlan[1].hasImputationGlyph);
    assert(selectedDrawPlan[1].colorName == "blue");
    assert(closeEnough(selectedDrawPlan[1].labelAnchor.x, 31.0));
    assert(closeEnough(selectedDrawPlan[1].labelAnchor.y, 30.0));
    assert(!selectedDrawPlan[1].showLabel);
    assert(selectedDrawPlan[2].caseId == 0);
    assert(selectedDrawPlan[2].colorName.empty());
    assert(selectedDrawPlan[2].fillAlpha == 0.22);
    assert(selectedDrawPlan[3].caseId == 1);
    assert(selectedDrawPlan[3].selected);
    assert(selectedDrawPlan[3].colorName == "black");
    assert(!selectedDrawPlan[3].hasExplicitColor);
    assert(selectedDrawPlan[3].fillAlpha == 1.0);
    assert(selectedDrawPlan[3].strokeAlpha == 0.90);
    assert(selectedDrawPlan[3].radius == 3.0);
    assert(selectedDrawPlan[3].hasHalo);
    assert(selectedDrawPlan[3].haloRadius == 6.0);
    assert(selectedDrawPlan[3].showLabel);
    assert(selectedDrawPlan[3].label == "Case 1");

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

    DataFrameModel manyImputations = imputed;
    manyImputations.imputationCount = 50;
    for (int version = 2; version < 50; ++version) {
        manyImputations.columns[0].imputationValues.push_back(
            version % 2 ? std::vector<std::string>{"18", "21"}
                        : std::vector<std::string>{"18", "20"});
        manyImputations.columns[1].imputationValues.push_back({"1", "2"});
    }
    ScatterplotRenderInput manyInput = imputedRenderInput;
    manyInput.imputationDataFrame = &manyImputations;
    manyInput.xColumn = &manyImputations.columns[0];
    manyInput.yColumn = &manyImputations.columns[1];
    manyInput.overlays = {ScatterplotOverlaySpec{"lm", "all", true}};
    ScatterplotRenderPlan manyPlan = BuildScatterplotRenderPlan(manyInput);
    assert(manyPlan.points.size() == 2 && manyPlan.points[1].selected);
    assert(manyPlan.points[1].hasImputationGlyph);
    assert(manyPlan.overlayLines.empty()); // Fitted lines come from R.
    const auto manyHits = BuildScatterplotCaseGeometry(
        manyInput.points, manyInput.viewport, manyInput.plotRect,
        manyInput.imputationDataFrame, manyInput.xColumn, manyInput.yColumn,
        manyInput.imputationUncertaintyMode);
    assert(manyHits.size() == 2 && manyHits[1].hasGlyphRect);
    assert(closeEnough(manyHits[1].glyphRect.x,
                       manyPlan.points[1].imputationGlyph.rect.x));
    const auto cachedDrawInputs = BuildScatterplotPointDrawInputs(
        manyInput.points, manyInput.viewport, manyInput.plotRect,
        manyInput.imputationDataFrame, manyInput.xColumn, manyInput.yColumn,
        manyInput.imputationUncertaintyMode);
    manyInput.precomputedPointDrawInputs = &cachedDrawInputs;
    manyInput.selectedRows.clear();
    const auto cachedPlan = BuildScatterplotRenderPlan(manyInput);
    assert(cachedPlan.points.size() == manyPlan.points.size());
    assert(!cachedPlan.points[1].selected && manyPlan.points[1].selected);
    assert(cachedPlan.points[1].hasImputationGlyph);
    assert(closeEnough(cachedPlan.points[1].imputationGlyph.rect.x,
                       manyPlan.points[1].imputationGlyph.rect.x));

    // Regression diagnostics use independently fitted per-imputation
    // coordinates rather than worksheet columns. They must drive the same
    // uncertainty glyph and viewport pipeline.
    std::vector<ScatterplotPointImputationValues> diagnosticValues = {
        {1, {{17.5, 0.8}, {18.5, 1.2}}},
        {2, {{19.0, 1.5}, {21.0, 2.5}}}
    };
    auto diagnosticViewport = ScatterplotViewportIncludingPointImputations(
        pointValues, diagnosticValues, "range");
    assert(diagnosticViewport.has_value());
    ScatterplotRenderInput diagnosticRenderInput = imputedRenderInput;
    diagnosticRenderInput.imputationDataFrame = nullptr;
    diagnosticRenderInput.xColumn = nullptr;
    diagnosticRenderInput.yColumn = nullptr;
    diagnosticRenderInput.pointImputationValues = &diagnosticValues;
    diagnosticRenderInput.imputationUncertaintyMode = "range";
    auto diagnosticRenderPlan = BuildScatterplotRenderPlan(diagnosticRenderInput);
    assert(diagnosticRenderPlan.points.size() == 2);
    assert(diagnosticRenderPlan.points[0].hasImputationGlyph);
    assert(diagnosticRenderPlan.points[1].hasImputationGlyph);
    assert(diagnosticRenderPlan.points[1].imputationGlyph.axisMask == 3);

    // Derived regression-diagnostic axes are not worksheet columns.  Their
    // per-imputation glyphs must nevertheless use the exact render geometry
    // for linked brushing.
    const auto diagnosticHitGeometry =
        BuildScatterplotCaseGeometry(diagnosticRenderInput);
    assert(diagnosticHitGeometry.size() == 2);
    assert(diagnosticHitGeometry[0].caseId == 1);
    assert(diagnosticHitGeometry[0].hasGlyphRect);
    assert(diagnosticHitGeometry[1].caseId == 2);
    assert(diagnosticHitGeometry[1].hasGlyphRect);
    assert(SelectCasesInBrush(
        diagnosticHitGeometry,
        diagnosticHitGeometry[0].glyphRect,
        0.0) == S({1}));

    diagnosticRenderInput.distinguishDiagnosticImputationRows = true;
    diagnosticRenderInput.directlyImputedModelRows = S({2});
    diagnosticRenderPlan = BuildScatterplotRenderPlan(diagnosticRenderInput);
    assert(diagnosticRenderPlan.points[0].hasExplicitColor);
    assert(diagnosticRenderPlan.points[0].colorName == "black");
    assert(diagnosticRenderPlan.points[1].hasExplicitColor);
    assert(diagnosticRenderPlan.points[1].colorName == "red");

    // A selected case remains the last painter layer even when an overlapping
    // unselected diagnostic case carries the red direct-imputation encoding.
    diagnosticRenderInput.directlyImputedModelRows = S({1});
    diagnosticRenderPlan = BuildScatterplotRenderPlan(diagnosticRenderInput);
    assert(diagnosticRenderPlan.points[0].caseId == 1);
    assert(diagnosticRenderPlan.points[0].colorName == "red");
    assert(!diagnosticRenderPlan.points[0].selected);
    assert(diagnosticRenderPlan.points[1].caseId == 2);
    assert(diagnosticRenderPlan.points[1].colorName == "black");
    assert(diagnosticRenderPlan.points[1].selected);

    const auto diagnosticPointSets =
        BuildScatterplotImputationPointSets(diagnosticRenderInput);
    assert(diagnosticPointSets.size() == 2);
    assert(diagnosticPointSets[0].imputationIndex == 1);
    assert(diagnosticPointSets[0].points.size() == 2);
    assert(closeEnough(diagnosticPointSets[1].points[0].x, 18.5));

    // Point filtering (for example, showing only direct-imputation glyphs)
    // must not filter the complete point clouds used by overlays/smoothers.
    std::vector<ScatterplotPointImputationValues> directDiagnosticValues = {
        diagnosticValues[1]
    };
    diagnosticRenderInput.pointImputationValues = &directDiagnosticValues;
    diagnosticRenderInput.completeImputationPointValues = &diagnosticValues;
    const auto completeDiagnosticPointSets =
        BuildScatterplotImputationPointSets(diagnosticRenderInput);
    assert(completeDiagnosticPointSets.size() == 2);
    assert(completeDiagnosticPointSets[0].points.size() == 2);
    diagnosticRenderInput.completeImputationPointValues = nullptr;
    diagnosticRenderInput.pointImputationValues = &diagnosticValues;

    // Diagnostic line requests must send the exact derived coordinates to R,
    // even when only one imputation is displayed. The axis captions are not
    // worksheet-column names and cannot be used to reconstruct these points.
    rlispstat::core::PlotModel diagnosticModel;
    diagnosticModel.kind = "scatter";
    diagnosticModel.isGLMDiagnostic = true;
    diagnosticModel.glmDiagnosticKind = "residuals_fitted";
    diagnosticModel.diagnosticImputationIndex = 7;
    diagnosticModel.points = {
        {4.0, -0.5, 1},
        {5.0, 0.75, 2}
    };
    assert(RegressionDiagnosticSupportsAddedLines(diagnosticModel));
    auto diagnosticSmoothSets =
        BuildRegressionDiagnosticSmoothPointSets(diagnosticModel);
    assert(diagnosticSmoothSets.size() == 1);
    assert(diagnosticSmoothSets[0].imputationIndex == 7);
    assert(diagnosticSmoothSets[0].points.size() == 2);
    assert(closeEnough(diagnosticSmoothSets[0].points[1].x, 5.0));
    assert(closeEnough(diagnosticSmoothSets[0].points[1].y, 0.75));

    diagnosticModel.diagnosticShowImputationUncertainty = true;
    diagnosticModel.diagnosticAllImputationValues = diagnosticValues;
    diagnosticSmoothSets =
        BuildRegressionDiagnosticSmoothPointSets(diagnosticModel);
    assert(diagnosticSmoothSets.size() == 2);
    assert(diagnosticSmoothSets[0].imputationIndex == 1);
    assert(diagnosticSmoothSets[1].imputationIndex == 2);
    assert(closeEnough(diagnosticSmoothSets[1].points[0].x, 18.5));

    diagnosticModel.glmDiagnosticKind = "roc_curve";
    assert(!RegressionDiagnosticSupportsAddedLines(diagnosticModel));
    assert(BuildRegressionDiagnosticSmoothPointSets(diagnosticModel).empty());
    diagnosticModel.glmDiagnosticKind = "residuals_fitted";
    diagnosticModel.kind = "histogram";
    assert(!RegressionDiagnosticSupportsAddedLines(diagnosticModel));
    diagnosticModel.kind = "scatter";
    diagnosticModel.isGLMDiagnostic = false;
    assert(!RegressionDiagnosticSupportsAddedLines(diagnosticModel));

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
        5,
        3,
        "all",
        "iqr");
    assert(menuState.variables.xOptions.size() == 3);
    assert(menuState.variables.xOptions[1].checked);
    assert(menuState.variables.xOptions[1].command == "CHANGE_X_VARIABLE|wt");
    assert(menuState.variables.yOptions[0].checked);
    assert(menuState.variables.openVariablesWindow.command == "OPEN_VARIABLES_WINDOW|cars");
    assert(menuState.mouseModeOptions.empty());
    assert(menuState.selectionModeOptions[2].command == "SET_SELECTION_SUBTRACT");
    assert(menuState.selectionActionOptions[1].command == "INVERT_SELECTION");
    assert(menuState.brushOptions.empty());
    assert(menuState.viewOptions.empty());
    assert(menuState.showImputationDisplayOptions);
    assert(menuState.imputationDisplayTitle == "Imputations: All (m = 5)");
    assert(menuState.imputationDisplayOptions.size() == 7);
    assert(menuState.imputationDisplayOptions[2].title == "Imputation 3 of 5");
    assert(!menuState.imputationDisplayOptions[2].checked);
    assert(menuState.imputationDisplayOptions[5].checked);
    assert(menuState.imputationDisplayOptions[5].command == "SET_IMPUTATION_DISPLAY|all");
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
    assert(menuState.clearOverlays.title == "Remove all regression lines");
    // Adaptive analysis options are now built from PlotAnalysisContext rather
    // than duplicated in the scatterplot display-menu state.
    assert(menuState.plotOptions[2].command == "PLOT_NEW_TIME_SERIES");
    assert(menuState.plotOptions[4].command == "PLOT_NEW_PARALLEL_COORDINATES");
    assert(menuState.plotOptions[7].command == "PLOT_NEW_LINKED_BAR_CHART");
    assert(menuState.closePlot.command == "CLOSE_PLOT");
    assert(ScatterplotMouseModeMenuOptions(false).empty());
    assert(ScatterplotMouseModeMenuOptions(true).empty());
    assert(ScatterplotSelectionModeMenuOptions().front().command == "SET_SELECTION_REPLACE");
    assert(ScatterplotSelectionActionMenuOptions().front().title == "Clear selection");
    assert(ScatterplotSelectionActionMenuOptions().back().command == "SELECT_ALL_VISIBLE");
    assert(ScatterplotBrushMenuOptions().empty());
    assert(ScatterplotViewMenuOptions().empty());
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
    SmoothCurveData rOverall;
    rOverall.scope = SmoothCurveScope::Overall;
    rOverall.fitMethod = "lm";
    rOverall.groupId = ".";
    rOverall.x = {0.0, 5.0};
    rOverall.y = {1.0, 11.0};
    rOverall.ok = true;
    SmoothCurveData rSelected = rOverall;
    rSelected.scope = SmoothCurveScope::Selection;
    rSelected.y = {2.0, 10.0};
    renderInput.smoothCurves = {rOverall, rSelected};
    ScatterplotRenderPlan renderPlan = BuildScatterplotRenderPlan(renderInput);
    assert(renderPlan.points.size() == 6);
    assert(!renderPlan.points[0].selected);
    assert(!renderPlan.points[1].selected);
    assert(!renderPlan.points[2].selected);
    assert(renderPlan.points[3].selected);
    assert(renderPlan.points[3].showLabel);
    assert(closeEnough(renderPlan.points[3].point.x, 100.0));
    assert(closeEnough(renderPlan.points[3].point.y, 225.0));
    assert(renderPlan.overlayLines.empty());
    assert(renderPlan.smoothCurves.size() == 2);
    assert(!renderPlan.smoothCurves[0].dashed);
    assert(renderPlan.smoothCurves[1].dashed);

    // In the all-imputations display the R task returns one fitted curve per
    // completed point cloud; native code does not refit those observations.
    ScatterplotRenderInput miOverlayInput = renderInput;
    miOverlayInput.overlays = {ScatterplotOverlaySpec{"lm", "all", true}};
    miOverlayInput.pointImputationValues = &diagnosticValues;
    miOverlayInput.points = pointValues;
    miOverlayInput.viewport = DataViewport{17.0, 22.0, 0.0, 3.0};
    SmoothCurveData rImputation1 = rOverall;
    rImputation1.groupId = std::string(".") + "\x1f" "mi:1";
    rImputation1.x = {18.0, 21.0};
    rImputation1.y = {1.0, 2.0};
    SmoothCurveData rImputation2 = rOverall;
    rImputation2.groupId = std::string(".") + "\x1f" "mi:2";
    rImputation2.x = {18.0, 21.0};
    rImputation2.y = {1.2, 2.2};
    miOverlayInput.smoothCurves = {rImputation1, rImputation2};
    ScatterplotRenderPlan miOverlayPlan = BuildScatterplotRenderPlan(miOverlayInput);
    assert(miOverlayPlan.overlayLines.empty());
    assert(miOverlayPlan.smoothCurves.size() == 2);
    assert(miOverlayPlan.smoothCurves[0].alpha <= 0.52);
    assert(miOverlayPlan.smoothCurves[1].alpha <= 0.52);

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

    // Per-imputation suffixes retain the intended visual group while making
    // the curve bundle lighter than a single fitted smooth.
    {
        SmoothCurveData c;
        c.scope = SmoothCurveScope::ColorGroup;
        c.groupId = std::string("red") + "\x1f" "mi:2";
        c.ok = true;
        c.x = {2.0, 4.0, 6.0};
        c.y = {3.0, 5.0, 7.0};
        curves.push_back(c);
    }
    {
        std::vector<ScatterplotSmoothCurveDrawItem> items =
            BuildSmoothCurveDrawItems(curves, kViewport, kPlotRect);
        assert(items.size() == 4);
        assert(items[3].colorName == "red");
        assert(items[3].alpha <= 0.52);
        assert(items[3].lineWidth <= 1.75);
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
        assert(items.size() == 4);
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
        assert(items.size() == 4);
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
        assert(items.size() == 4);
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
        assert(items.size() == 4);
    }

    // invalid viewport returns empty
    {
        std::vector<ScatterplotSmoothCurveDrawItem> items = BuildSmoothCurveDrawItems(curves, DataViewport{0, 0, 0, 10}, kPlotRect);
        assert(items.empty());
    }

    // curves are clipped to the plot rectangle instead of drawing through margins
    {
        SmoothCurveData c;
        c.scope = SmoothCurveScope::Overall;
        c.groupId = ".";
        c.ok = true;
        c.x = {-2.0, 5.0, 12.0};
        c.y = {12.0, 5.0, -2.0};
        auto items = BuildSmoothCurveDrawItems({c}, kViewport, kPlotRect);
        assert(!items.empty());
        for (auto const& item : items)
            for (auto const& point : item.points) {
                assert(point.x >= kPlotRect.x - 1e-6);
                assert(point.x <= kPlotRect.x + kPlotRect.width + 1e-6);
                assert(point.y >= kPlotRect.y - 1e-6);
                assert(point.y <= kPlotRect.y + kPlotRect.height + 1e-6);
            }
    }

    // Confidence bands are optional, use the R-returned limits, and remain
    // absent by default.
    {
        SmoothCurveData c;
        c.scope = SmoothCurveScope::Overall;
        c.groupId = ".";
        c.fitMethod = "lm";
        c.ok = true;
        c.x = {1.0, 5.0, 9.0};
        c.y = {2.0, 5.0, 8.0};
        c.confidenceLower = {1.0, 4.0, 7.0};
        c.confidenceUpper = {3.0, 6.0, 9.0};
        const auto hidden = BuildSmoothCurveDrawItems(
            {c}, kViewport, kPlotRect, false);
        assert(hidden.size() == 1);
        assert(hidden.front().confidencePolygon.empty());
        const auto shown = BuildSmoothCurveDrawItems(
            {c}, kViewport, kPlotRect, true);
        assert(shown.size() == 1);
        assert(shown.front().confidencePolygon.size() == 6);
        assert(shown.front().confidenceAlpha > 0.0);
    }

    // Linear and smoothed confidence intervals are independently visible.
    {
        SmoothCurveData linear;
        linear.scope = SmoothCurveScope::Overall;
        linear.groupId = ".";
        linear.fitMethod = "lm";
        linear.ok = true;
        linear.x = {1.0, 5.0, 9.0};
        linear.y = {2.0, 5.0, 8.0};
        linear.confidenceLower = {1.0, 4.0, 7.0};
        linear.confidenceUpper = {3.0, 6.0, 9.0};
        SmoothCurveData smooth = linear;
        smooth.fitMethod = "loess";
        const auto linearOnly = BuildSmoothCurveDrawItems(
            {linear, smooth}, kViewport, kPlotRect, true, false);
        assert(linearOnly.size() == 2);
        assert(!linearOnly[0].confidencePolygon.empty());
        assert(linearOnly[1].confidencePolygon.empty());
        const auto smoothOnly = BuildSmoothCurveDrawItems(
            {linear, smooth}, kViewport, kPlotRect, false, true);
        assert(smoothOnly.size() == 2);
        assert(smoothOnly[0].confidencePolygon.empty());
        assert(!smoothOnly[1].confidencePolygon.empty());
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
                                                              0, 1, "version", "central80", testCurves);
        assert(menu.smoothOptions.size() == 3);
        assert(menu.smoothOptions[0].value == "overall");
        assert(menu.smoothOptions[0].checked == true);
        assert(menu.smoothOptions[1].value == "selected");
        assert(menu.smoothOptions[1].checked == false);

        testCurves.push_back(SmoothCurveData{SmoothCurveScope::Selection, ".", {}, {}, false, "needed"});
        ScatterplotMenuState pendingMenu = BuildScatterplotMenuState({}, "x", "y", "g", "", "none", emptyOverlays,
                                                                     0, 1, "version", "central80", testCurves);
        assert(pendingMenu.smoothOptions[1].checked == true);
    }

    // A fitted straight line must not make the LOESS command look enabled.
    {
        const std::vector<ScatterplotOverlaySpec> straight = {
            {"lm", "all", true}
        };
        std::vector<SmoothCurveData> curves = {
            PendingSmoothCurve(SmoothCurveScope::Overall, "lm")
        };
        auto menu = BuildScatterplotMenuState({}, "x", "y", "g", "", "none",
            straight, 0, 1, "version", "", curves);
        assert(menu.overlayOptions[0].checked);
        assert(!menu.smoothOptions[0].checked);

        curves.push_back(PendingSmoothCurve(SmoothCurveScope::Overall));
        menu = BuildScatterplotMenuState({}, "x", "y", "g", "", "none",
            straight, 0, 1, "version", "", curves);
        assert(menu.overlayOptions[0].checked);
        assert(menu.smoothOptions[0].checked);

        ToggleSmoothCurveScopePending(curves, SmoothCurveScope::Overall);
        menu = BuildScatterplotMenuState({}, "x", "y", "g", "", "none",
            straight, 0, 1, "version", "", curves);
        assert(menu.overlayOptions[0].checked);
        assert(!menu.smoothOptions[0].checked);
    }

    // A selected MI version is explicit in the parent title and check mark;
    // joint uncertainty choices only make sense in the all-imputation view.
    {
        std::vector<ScatterplotOverlaySpec> emptyOverlays;
        ScatterplotMenuState versionMenu = BuildScatterplotMenuState(
            {}, "x", "y", "g", "", "none", emptyOverlays,
            5, 4, "version", "central80");
        assert(versionMenu.showImputationDisplayOptions);
        assert(versionMenu.imputationDisplayTitle == "Imputation: 4 of 5");
        assert(versionMenu.imputationDisplayOptions.size() == 7);
        assert(versionMenu.imputationDisplayOptions[3].checked);
        assert(versionMenu.imputationDisplayOptions[3].value == "version:4");
        assert(!versionMenu.showImputationUncertaintyOptions);
        assert(versionMenu.imputationUncertaintyOptions.empty());

        ScatterplotMenuState originalMenu = BuildScatterplotMenuState(
            {}, "x", "y", "g", "", "none", emptyOverlays,
            5, 4, "original", "sd");
        assert(originalMenu.imputationDisplayTitle == "Imputations: Original data");
        assert(originalMenu.imputationDisplayOptions.back().checked);
        assert(!originalMenu.showImputationUncertaintyOptions);
    }
    }

    {
        using namespace rlispstat::core;
        const auto counts = ScatterplotVisualOverlapCounts({{0,0},{1e-8,0},{5,0},{20,20},{NAN,0}});
        assert((counts == std::vector<std::size_t>{3,3,3,1,1}));
        // Dense stacks are processed once per occupied pixel cell.
        std::vector<Point> dense(100000, Point{50,50});
        const auto denseCounts = ScatterplotVisualOverlapCounts(dense);
        assert(denseCounts.front()==100000 && denseCounts.back()==100000);
        ScatterplotRenderInput input;
        input.points = {{1,0,0},{2,1e-8,0},{3,.04,0},{4,1,1}};
        input.viewport = {-1,2,-1,2}; input.plotRect = {0,0,300,300};
        input.sizeByOverlap = true; input.sizeByVisualOverlap = true;
        input.selectedRows = {2}; input.labelDisplayMode="selected"; input.rowLabels={{2,"Case two"}};
        auto plan=BuildScatterplotRenderPlan(input);
        for (const auto &point : plan.points) {
            assert(std::abs(point.radius - (point.caseId==4 ? 3.0 : 3.0*std::sqrt(3.0))) < 1e-8);
            if (point.caseId==2) assert(point.selected && point.showLabel && point.label=="Case two");
        }
        input.sizeByOverlap=false; plan=BuildScatterplotRenderPlan(input);
        for (const auto &point : plan.points) assert(point.radius==3.0);
        input.sizeByOverlap=true; input.viewport={-.01,.05,-.01,.05};
        plan=BuildScatterplotRenderPlan(input);
        for (const auto &point : plan.points)
            if(point.caseId==3) assert(point.radius==3.0); // Zoom separates the visible overlap.
    }

    std::fprintf(stderr, "All scatterplot model tests PASSED\n");
    return 0;
}
