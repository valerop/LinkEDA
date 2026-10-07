#include "../../src/core/barplot_model.h"

#include <cassert>
#include <cmath>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <vector>

using rlispstat::core::BarplotBarIndexAtPoint;
using rlispstat::core::BarplotBarRangeForGesture;
using rlispstat::core::BarplotBarRect;
using rlispstat::core::BarplotBuildInput;
using rlispstat::core::BarplotBuildResult;
using rlispstat::core::BuildBarplotCategoryLabelDrawPlan;
using rlispstat::core::BarplotCategoryComponentAtPoint;
using rlispstat::core::BarplotCategoryComponentHit;
using rlispstat::core::BarplotCategoryComponentRect;
using rlispstat::core::BarplotCategoryLabelRect;
using rlispstat::core::BarplotCategoryComponents;
using rlispstat::core::BarplotCategoryLabelForValues;
using rlispstat::core::BarplotCategoryLabelVerticalOffset;
using rlispstat::core::BarplotCategorySortOrder;
using rlispstat::core::BarplotCompositionSlice;
using rlispstat::core::BarplotCreationDialogState;
using rlispstat::core::BarplotDefaultTitle;
using rlispstat::core::BarplotNotAvailableStatus;
using rlispstat::core::BarplotChooseXVariableStatus;
using rlispstat::core::BarplotNoValuesToPlotStatus;
using rlispstat::core::BarplotWindowTitle;
using rlispstat::core::BarplotSegmentMenuTitle;
using rlispstat::core::BarplotSetSegmentColorMenuItemTitle;
using rlispstat::core::BarplotSetSegmentOpacityMenuItemTitle;
using rlispstat::core::BarplotSetPatternForThisSegmentMenuItemTitle;
using rlispstat::core::BarplotEncodingShowsColor;
using rlispstat::core::BarplotEncodingShowsPatterns;
using rlispstat::core::BarplotHeightScaleLabel;
using rlispstat::core::BarplotHorizontalRowColorCompositionSlices;
using rlispstat::core::BarplotInputRow;
using rlispstat::core::BarplotLayout;
using rlispstat::core::BarplotLevelIsMissing;
using rlispstat::core::BarplotLayoutInput;
using rlispstat::core::BarplotLevelIndex;
using rlispstat::core::BarplotMaxBarCount;
using rlispstat::core::BarplotPatternAtIndex;
using rlispstat::core::BarplotPatternDisplayName;
using rlispstat::core::BarplotPatternIsKnown;
using rlispstat::core::BarplotPlotRect;
using rlispstat::core::BarplotPreferredViewSize;
using rlispstat::core::BarplotSegmentGeometryAtPoint;
using rlispstat::core::BarplotSegmentGeometryForLevels;
using rlispstat::core::BarplotSegmentEncodingLabel;
using rlispstat::core::BarplotSegmentAtPoint;
using rlispstat::core::BarplotSegmentSummary;
using rlispstat::core::BuildBarplotSegmentDrawPlan;
using rlispstat::core::BuildBarplotCreationDialogState;
using rlispstat::core::BuildBarplotSegmentMenuState;
using rlispstat::core::BarplotSegmentTooltipText;
using rlispstat::core::BarplotSegmentRects;
using rlispstat::core::BarplotSegmentCommandPayload;
using rlispstat::core::BarplotSegmentMenuTitle;
using rlispstat::core::BarplotSegmentOverrideKey;
using rlispstat::core::BarplotSegmentOverrideKeys;
using rlispstat::core::BarplotSegmentOverrideKeysForBins;
using rlispstat::core::BarplotSegmentVisualStyle;
using rlispstat::core::BarplotSegmentOpacityMenuValues;
using rlispstat::core::ClearBarplotSplitVisualState;
using rlispstat::core::BarplotDragHighlightRect;
using rlispstat::core::BarplotRowsForBarRange;
using rlispstat::core::BarplotRowsAtPoint;
using rlispstat::core::BarplotRowsForSplitLevel;
using rlispstat::core::BarplotRowsForXVariableValue;
using rlispstat::core::BarplotRowsForGesture;
using rlispstat::core::BarplotSelectionDisplayShowsOutline;
using rlispstat::core::BarplotSelectionDisplayShowsOverlay;
using rlispstat::core::BarplotSelectionDisplayAfterSet;
using rlispstat::core::BarplotSelectionSlicePlan;
using rlispstat::core::BarplotSharedCategoryPrefixDepth;
using rlispstat::core::BarplotSideLabelDrawPlan;
using rlispstat::core::BarplotShowPatternsAfterCommand;
using rlispstat::core::BarplotShowPatternsMenuOption;
using rlispstat::core::BarplotPatternMenuValues;
using rlispstat::core::BarplotStateAfterSegmentEncoding;
using rlispstat::core::BarplotSplitLevelLabelIndexAtPoint;
using rlispstat::core::BarplotSplitLevelLabelRect;
using rlispstat::core::BarplotSplitLevelTooltipText;
using rlispstat::core::BarplotSplitLevelTotalsForBins;
using rlispstat::core::BarplotSplitLevelsForBins;
using rlispstat::core::BarplotSplitStrokeWidthDialogState;
using rlispstat::core::BarplotSplitStrokeWidthMenuOption;
using rlispstat::core::BarplotSplitStyleMenuOption;
using rlispstat::core::BarplotSubtitle;
using rlispstat::core::BarplotValueForBin;
using rlispstat::core::BarplotVerticalRowColorCompositionSlices;
using rlispstat::core::BarplotVisualState;
using rlispstat::core::BarplotWidthMode;
using rlispstat::core::BarplotWidthScaleLabel;
using rlispstat::core::BarplotWidthValueForBin;
using rlispstat::core::BarplotYAxisTicks;
using rlispstat::core::ZeroBaselineContentRect;
using rlispstat::core::ZeroBaselineY;
using rlispstat::core::BarplotCategoryTooltipText;
using rlispstat::core::BarplotModeIsValid;
using rlispstat::core::BarplotModeMenuOptions;
using rlispstat::core::BarplotModeAfterSet;
using rlispstat::core::BarplotRowColorDisplayAfterSet;
using rlispstat::core::BarplotRowColorDisplayAllowed;
using rlispstat::core::BarplotRowColorDisplayIsValid;
using rlispstat::core::BarplotRowColorDisplayMenuOptions;
using rlispstat::core::BarplotSelectionDisplayMenuOptions;
using rlispstat::core::BarplotSelectionDisplayModeIsValid;
using rlispstat::core::BarplotSegmentEncodingMenuOptions;
using rlispstat::core::BarplotSegmentReference;
using rlispstat::core::BarplotSegmentReferenceForIndices;
using rlispstat::core::BarplotBinSummary;
using rlispstat::core::BarplotRowsForSegmentIndices;
using rlispstat::core::BarplotSplitMenuState;
using rlispstat::core::BarplotWidthMenuOptions;
using rlispstat::core::BarplotWidthModeAfterSet;
using rlispstat::core::BarplotWidthModeIsValid;
using rlispstat::core::BarplotXMenuState;
using rlispstat::core::BarplotXVariablesForLabels;
using rlispstat::core::BarplotXVariablesAfterAdd;
using rlispstat::core::BarplotXVariablesAfterRemove;
using rlispstat::core::BarplotXVariablesAfterReplace;
using rlispstat::core::BarplotXVariablesAfterSet;
using rlispstat::core::BarplotXAxisLabelRect;
using rlispstat::core::BarplotYMaximum;
using rlispstat::core::BarplotAnalysisMenuTitle;
using rlispstat::core::BarplotAvailableVariables;
using rlispstat::core::BarplotConditionalPercentForMode;
using rlispstat::core::BarplotConditionalPercentMenuOption;
using rlispstat::core::ApplyBarplotBuildResultToModel;
using rlispstat::core::BuildBarplotBins;
using rlispstat::core::BuildBarplotBinsForColumns;
using rlispstat::core::BuildBarplotDisplayMenuState;
using rlispstat::core::BuildBarplotLayout;
using rlispstat::core::BuildBarplotSplitMenuState;
using rlispstat::core::BuildBarplotSplitStrokeWidthDialogState;
using rlispstat::core::BuildBarplotSelectionSlicePlan;
using rlispstat::core::BarplotSelectedSliceCoversY;
using rlispstat::core::BuildBarplotSideLabelDrawPlan;
using rlispstat::core::BuildBarplotXMenuState;
using rlispstat::core::ClampBarplotSplitStrokeWidth;
using rlispstat::core::ColorKeyIsDefaultNeutral;
using rlispstat::core::CountSelectedRowsForRows;
using rlispstat::core::DataColumn;
using rlispstat::core::DataFrameModel;
using rlispstat::core::DominantManualRowColorForRows;
using rlispstat::core::GeneratedBarplotTitle;
using rlispstat::core::OrderedRowColorComposition;
using rlispstat::core::OrderedUniqueBarplotLevels;
using rlispstat::core::ParseBarplotSegmentCommandPayload;
using rlispstat::core::PlotModel;
using rlispstat::core::ParseBarplotSegmentAlphaCommandValue;
using rlispstat::core::ParseBarplotSplitStrokeWidthCommandValue;
using rlispstat::core::Point;
using rlispstat::core::Rect;
using rlispstat::core::RefreshedBarplotTitle;
using rlispstat::core::RebuildBarplotFromDataFrame;
using rlispstat::core::RefreshBarplotFromDataFrame;
using rlispstat::core::NormalizeBarplotVisualState;
using rlispstat::core::NormalizeBarplotVisualStateForBins;
using rlispstat::core::NormalizedBarplotXVariables;
using rlispstat::core::NormalizedBarplotSegmentEncodingMode;
using rlispstat::core::ResetBarplotLevelColorOverride;
using rlispstat::core::ResetBarplotSegmentColorOverride;
using rlispstat::core::ResolveBarplotSegmentVisual;
using rlispstat::core::RowColorCompositionForRows;
using rlispstat::core::RowColorDisplayName;
using rlispstat::core::RowColorKeyForRow;
using rlispstat::core::SelectedFractionForRows;
using rlispstat::core::SetBarplotLevelColorOverride;
using rlispstat::core::SetBarplotLevelPatternOverride;
using rlispstat::core::SetBarplotSegmentAlphaOverride;
using rlispstat::core::SetBarplotSegmentColorOverride;
using rlispstat::core::SetBarplotSegmentPatternOverride;
using rlispstat::core::SplitBarplotConditionLabel;
using rlispstat::core::SplitBarplotXLabel;
using rlispstat::core::BarplotStateAfterClearSplit;
using rlispstat::core::BarplotStateAfterSplitBy;
using rlispstat::core::BarplotTitleIsAutoGenerated;

static bool closeEnough(double a, double b)
{
    return std::fabs(a - b) < 1.0e-9;
}

int main()
{
    BarplotCreationDialogState creationDialog = BuildBarplotCreationDialogState();
    assert(creationDialog.title == "Bar Chart");
    assert(creationDialog.informativeText ==
           "Choose a variable and optionally split each bar by another variable. The bar chart will share row selection with existing plots in this dataset.");
    assert(creationDialog.createButtonTitle == "Create");
    assert(creationDialog.cancelButtonTitle == "Cancel");
    assert(creationDialog.optionalTitlePlaceholder == "Optional title");
    assert(creationDialog.noActiveDatasetStatus == "Open or import a dataset first.");
    assert(creationDialog.needsBackendDataStatus == "The active dataset needs a backend data payload.");
    assert(creationDialog.selectedVariableMissingStatus == "Could not find the selected variable.");
    assert(creationDialog.selectedSplitVariableMissingStatus == "Could not find the selected split variable.");
    assert(BarplotDefaultTitle("cyl", "") == "Bar chart of cyl");
    assert(BarplotDefaultTitle("cyl + am", "gear") == "Bar chart of cyl + am by gear");

    BarplotLayout layout;
    layout.plotRect = Rect{20.0, 30.0, 400.0, 200.0};
    layout.values = {10.0, 20.0, 15.0, 5.0};
    layout.widthValues = {10.0, 20.0, 15.0, 5.0};
    layout.sharedPrefixDepthWithPrevious = {0, 1, 0, 1};
    layout.nestingDepth = 2;
    layout.yMaximum = 20.0;
    layout.widthMode = BarplotWidthMode::Equal;

    Rect first = BarplotBarRect(layout, 0);
    Rect second = BarplotBarRect(layout, 1);
    Rect third = BarplotBarRect(layout, 2);
    const Rect contentRect = ZeroBaselineContentRect(layout.plotRect);
    assert(first.width > 1.0);
    assert(contentRect.y > layout.plotRect.y);
    assert(contentRect.y + contentRect.height < layout.plotRect.y + layout.plotRect.height);
    assert(closeEnough(second.y, contentRect.y));
    assert(closeEnough(first.height, contentRect.height / 2.0));
    assert(closeEnough(first.y + first.height, ZeroBaselineY(layout.plotRect, 0.0)));
    assert(closeEnough(second.y, ZeroBaselineY(layout.plotRect, 1.0)));
    assert((third.x - (second.x + second.width)) > (second.x - (first.x + first.width)));
    const Rect fourth = BarplotBarRect(layout, 3);
    assert(first.x - layout.plotRect.x >= layout.plotRect.width * 0.025);
    assert(layout.plotRect.x + layout.plotRect.width -
           (fourth.x + fourth.width) >= layout.plotRect.width * 0.025);

    auto firstHit = BarplotBarIndexAtPoint(layout, Point{first.x + first.width / 2.0, 80.0});
    assert(firstHit.has_value() && *firstHit == 0);
    auto emptyHit = BarplotBarIndexAtPoint(layout, Point{0.0, 80.0});
    assert(!emptyHit.has_value());
    assert(!BarplotBarIndexAtPoint(layout,
        Point{layout.plotRect.x + 1.0, 80.0}).has_value());
    auto reversedRange = BarplotBarRangeForGesture(
        layout,
        Point{third.x + third.width / 2.0, 70.0},
        Point{first.x + first.width / 2.0, 80.0});
    assert(reversedRange.has_value());
    assert(reversedRange->first == 0);
    assert(reversedRange->second == 2);
    auto invalidRange = BarplotBarRangeForGesture(layout, Point{0.0, 80.0}, Point{first.x, 80.0});
    assert(!invalidRange.has_value());
    auto highlight = BarplotDragHighlightRect(
        layout,
        Point{first.x + first.width / 2.0, 60.0},
        Point{third.x + third.width / 2.0, 180.0});
    assert(highlight.has_value());
    assert(closeEnough(highlight->y, layout.plotRect.y));
    assert(closeEnough(highlight->height, layout.plotRect.height));
    assert(highlight->x <= first.x);
    assert(highlight->x + highlight->width >= third.x + third.width);
    auto selectedRows = BarplotRowsForBarRange({{1, 2}, {3}, {4, 5}}, 2, 0);
    assert(selectedRows.size() == 5);
    assert(selectedRows.find(1) != selectedRows.end());
    assert(selectedRows.find(5) != selectedRows.end());
    assert(BarplotRowsForBarRange({{1}}, 3, 4).empty());

    Rect label = BarplotCategoryLabelRect(layout.plotRect, first, 2, 7.0);
    Rect labelLine0 = BarplotCategoryComponentRect(label, 2, 0);
    Rect labelLine1 = BarplotCategoryComponentRect(label, 2, 1);
    assert(closeEnough(label.y, 237.0));
    assert(labelLine1.y > labelLine0.y);

    std::vector<Rect> segments = BarplotSegmentRects(second, {5, 15}, 20);
    assert(segments.size() == 2);
    assert(closeEnough(segments[0].height, second.height * 0.25));
    assert(closeEnough(segments[1].height, second.height * 0.75));
    assert(segments[1].y < segments[0].y);
    auto segmentGeometry = BarplotSegmentGeometryForLevels(
        second,
        {"missing", "low", "high"},
        {{"low", 5}, {"high", 15}},
        20);
    assert(segmentGeometry.size() == 2);
    assert(segmentGeometry[0].level == "low");
    assert(segmentGeometry[1].level == "high");
    auto lowSegment = BarplotSegmentGeometryAtPoint(
        second,
        {"low", "high"},
        {{"low", 5}, {"high", 15}},
        20,
        Point{segmentGeometry[0].rect.x + segmentGeometry[0].rect.width / 2.0,
              segmentGeometry[0].rect.y + segmentGeometry[0].rect.height / 2.0});
    assert(lowSegment.has_value());
    assert(lowSegment->level == "low");
    auto outsideSegment = BarplotSegmentGeometryAtPoint(
        second,
        {"low", "high"},
        {{"low", 5}, {"high", 15}},
        20,
        Point{second.x - 2.0, second.y});
    assert(!outsideSegment.has_value());

    Rect split0 = BarplotSplitLevelLabelRect(layout.plotRect, 0, {10, 30});
    Rect split1 = BarplotSplitLevelLabelRect(layout.plotRect, 1, {10, 30});
    assert(split0.y > split1.y);
    assert(closeEnough(split0.x, 8.0));

    layout.widthMode = BarplotWidthMode::Proportional;
    Rect proportional0 = BarplotBarRect(layout, 0);
    Rect proportional1 = BarplotBarRect(layout, 1);
    Rect proportionalLast = BarplotBarRect(layout, 3);
    assert(proportional1.width > proportional0.width);
    assert(proportional0.x - layout.plotRect.x >= layout.plotRect.width * 0.025);
    assert(layout.plotRect.x + layout.plotRect.width -
           (proportionalLast.x + proportionalLast.width) >=
           layout.plotRect.width * 0.025);

    std::map<int, std::string> rowColors{{1, "orange"}, {2, "orange"}, {3, ""}, {4, "blue"}};
    assert(RowColorKeyForRow(rowColors, 1) == "orange");
    assert(RowColorKeyForRow(rowColors, 3) == "__default__");
    assert(RowColorKeyForRow(rowColors, 9) == "__default__");

    std::map<std::string, int> composition = RowColorCompositionForRows({1, 2, 3, 4}, rowColors);
    assert(composition["orange"] == 2);
    assert(composition["blue"] == 1);
    assert(composition["__default__"] == 1);
    assert(DominantManualRowColorForRows({1, 2}, rowColors) == "orange");
    assert(DominantManualRowColorForRows({1, 3}, rowColors).empty());
    assert(DominantManualRowColorForRows({1, 4}, rowColors).empty());

    std::vector<std::pair<std::string, int>> ordered =
        OrderedRowColorComposition(composition, {"blue", "orange"});
    assert(ordered.size() == 3);
    assert(ordered[0].first == "__default__");
    assert(ordered[1].first == "blue");
    assert(ordered[2].first == "orange");
    assert(RowColorDisplayName("__default__") == "default");
    assert(RowColorDisplayName("orange") == "orange");
    assert(ColorKeyIsDefaultNeutral("Black"));
    assert(ColorKeyIsDefaultNeutral("#FFFFFF"));
    assert(!ColorKeyIsDefaultNeutral("blue"));

    std::vector<std::string> paletteOrder{"blue", "orange"};
    Rect compositionRect{10.0, 20.0, 100.0, 200.0};
    std::vector<BarplotCompositionSlice> verticalSlices =
        BarplotVerticalRowColorCompositionSlices(compositionRect,
                                                {1, 2, 3, 4},
                                                rowColors,
                                                paletteOrder);
    assert(verticalSlices.size() == 3);
    assert(verticalSlices[0].colorKey == "__default__");
    assert(closeEnough(verticalSlices[0].rect.y, 170.0));
    assert(closeEnough(verticalSlices[0].rect.height, 50.0));
    assert(verticalSlices[1].colorKey == "blue");
    assert(closeEnough(verticalSlices[1].rect.y, 120.0));
    assert(verticalSlices[2].colorKey == "orange");
    assert(closeEnough(verticalSlices[2].rect.y, 20.0));
    assert(closeEnough(verticalSlices[2].rect.height, 100.0));

    std::vector<BarplotCompositionSlice> manualOnlySlices =
        BarplotVerticalRowColorCompositionSlices(compositionRect,
                                                {1, 2, 3, 4},
                                                rowColors,
                                                paletteOrder,
                                                false,
                                                true);
    assert(manualOnlySlices.size() == 2);
    assert(manualOnlySlices[0].colorKey == "blue");
    assert(closeEnough(manualOnlySlices[0].rect.y, 120.0));
    assert(manualOnlySlices[1].colorKey == "orange");
    assert(BarplotVerticalRowColorCompositionSlices(compositionRect,
                                                   {3},
                                                   rowColors,
                                                   paletteOrder,
                                                   false,
                                                   true).empty());

    std::vector<BarplotCompositionSlice> horizontalSlices =
        BarplotHorizontalRowColorCompositionSlices(Rect{10.0, 20.0, 100.0, 4.0},
                                                  {1, 2, 3, 4},
                                                  rowColors,
                                                  paletteOrder);
    assert(horizontalSlices.size() == 3);
    assert(horizontalSlices[0].colorKey == "__default__");
    assert(closeEnough(horizontalSlices[0].rect.x, 10.0));
    assert(closeEnough(horizontalSlices[0].rect.width, 25.0));
    assert(horizontalSlices[1].colorKey == "blue");
    assert(closeEnough(horizontalSlices[1].rect.x, 35.0));
    assert(horizontalSlices[2].colorKey == "orange");
    assert(closeEnough(horizontalSlices[2].rect.x, 60.0));
    assert(closeEnough(horizontalSlices[2].rect.width, 50.0));

    BarplotSelectionSlicePlan selectionPlan =
        BuildBarplotSelectionSlicePlan(compositionRect,
                                       {1, 2, 3, 4},
                                       std::set<int>{2, 4},
                                       rowColors,
                                       paletteOrder,
                                       "purple");
    assert(selectionPlan.selected == 2);
    assert(selectionPlan.total == 4);
    assert(closeEnough(selectionPlan.selectedFraction, 0.5));
    assert(selectionPlan.dominantSelectedColorKey == "blue");
    assert(selectionPlan.backgroundSlices.size() == 3);
    assert(selectionPlan.backgroundSlices[0].colorKey == "__default__");
    assert(selectionPlan.backgroundSlices[1].colorKey == "blue");
    assert(selectionPlan.backgroundSlices[2].colorKey == "orange");
    assert(selectionPlan.slices.size() == 2);
    assert(selectionPlan.slices[0].colorKey == "blue");
    assert(closeEnough(selectionPlan.slices[0].rect.y, 120.0));
    assert(closeEnough(selectionPlan.slices[0].rect.height, 50.0));
    assert(selectionPlan.slices[1].colorKey == "orange");
    assert(closeEnough(selectionPlan.slices[1].rect.y, 70.0));
    assert(closeEnough(selectionPlan.slices[1].rect.height, 50.0));
    assert(BarplotSelectedSliceCoversY(selectionPlan, 145.0));
    assert(!BarplotSelectedSliceCoversY(selectionPlan, 45.0));
    assert(BuildBarplotSelectionSlicePlan(compositionRect,
                                         {1, 2, 3, 4},
                                         std::set<int>{9},
                                         rowColors,
                                         paletteOrder).slices.empty());
    BarplotSelectionSlicePlan unselectedPlan =
        BuildBarplotSelectionSlicePlan(compositionRect,
                                       {3},
                                       std::set<int>{},
                                       rowColors,
                                       paletteOrder,
                                       "yellow");
    assert(unselectedPlan.slices.empty());
    assert(!BarplotSelectedSliceCoversY(unselectedPlan, 145.0));
    assert(unselectedPlan.backgroundSlices.size() == 1);
    assert(unselectedPlan.backgroundSlices[0].colorKey == "__default__");

    assert(CountSelectedRowsForRows({1, 2, 3, 4}, std::set<int>{2, 4, 6}) == 2);
    assert(closeEnough(SelectedFractionForRows({1, 2, 3, 4}, 2), 0.5));
    assert(closeEnough(SelectedFractionForRows({}, 2), 0.0));

    assert(BarplotPatternAtIndex(0) == "diagonal_slash");
    assert(BarplotPatternAtIndex(6) == "diagonal_slash");
    assert(BarplotPatternIsKnown("dots"));
    assert(!BarplotPatternIsKnown("zigzag"));
    assert(BarplotPatternDisplayName("diagonal_slash") == "Diagonal Slash");
    assert(BarplotPatternDisplayName("custom") == "custom");
    assert(BarplotSegmentOverrideKey("am=0", "manual") == std::string("am=0\x1fmanual"));
    assert(closeEnough(ClampBarplotSplitStrokeWidth(NAN), 3.0));
    assert(closeEnough(ClampBarplotSplitStrokeWidth(0.2), 1.0));
    assert(closeEnough(ClampBarplotSplitStrokeWidth(20.0), 12.0));
    assert(closeEnough(ClampBarplotSplitStrokeWidth(4.5), 4.5));
    auto parsedStroke = ParseBarplotSplitStrokeWidthCommandValue(" 4.5 ");
    assert(parsedStroke.has_value() && closeEnough(*parsedStroke, 4.5));
    auto clampedStroke = ParseBarplotSplitStrokeWidthCommandValue("20");
    assert(clampedStroke.has_value() && closeEnough(*clampedStroke, 12.0));
    assert(!ParseBarplotSplitStrokeWidthCommandValue("4px").has_value());
    auto parsedAlpha = ParseBarplotSegmentAlphaCommandValue("0.85");
    assert(parsedAlpha.has_value() && closeEnough(*parsedAlpha, 0.85));
    auto clampedAlpha = ParseBarplotSegmentAlphaCommandValue("-0.5");
    assert(clampedAlpha.has_value() && closeEnough(*clampedAlpha, 0.0));
    assert(!ParseBarplotSegmentAlphaCommandValue("all").has_value());
    assert(BarplotEncodingShowsPatterns(true, "transparent_color_pattern"));
    assert(!BarplotEncodingShowsPatterns(false, "transparent_color_pattern"));
    assert(!BarplotEncodingShowsPatterns(true, "transparent_color_only"));
    assert(BarplotEncodingShowsColor("transparent_color_pattern"));
    assert(!BarplotEncodingShowsColor("pattern_only"));
    assert(BarplotSelectionDisplayShowsOverlay("overlay"));
    assert(BarplotSelectionDisplayShowsOverlay("overlay_outline"));
    assert(!BarplotSelectionDisplayShowsOverlay("outline"));
    assert(BarplotSelectionDisplayShowsOutline("outline"));
    assert(BarplotSelectionDisplayShowsOutline("overlay_outline"));
    assert(!BarplotSelectionDisplayShowsOutline("overlay"));
    assert(NormalizedBarplotSegmentEncodingMode("bad") == "transparent_color_pattern");
    assert(NormalizedBarplotSegmentEncodingMode("pattern_only") == "pattern_only");
    assert(BarplotLevelIsMissing(""));
    assert(BarplotLevelIsMissing("NA"));
    assert(BarplotLevelIsMissing("NaN"));
    assert(!BarplotLevelIsMissing("0"));
    int barIndex = -1;
    int segmentIndex = -1;
    std::string tail;
    assert(ParseBarplotSegmentCommandPayload("3|2|orange", &barIndex, &segmentIndex, &tail));
    assert(barIndex == 3);
    assert(segmentIndex == 2);
    assert(tail == "orange");
    assert(ParseBarplotSegmentCommandPayload("0|1", &barIndex, &segmentIndex, &tail));
    assert(barIndex == 0);
    assert(segmentIndex == 1);
    assert(tail.empty());
    assert(ParseBarplotSegmentCommandPayload("4|5|a|b", &barIndex, &segmentIndex, &tail));
    assert(tail == "a|b");
    assert(!ParseBarplotSegmentCommandPayload("-1|0", &barIndex, &segmentIndex, &tail));
    assert(!ParseBarplotSegmentCommandPayload("x|0", &barIndex, &segmentIndex, &tail));
    assert(!ParseBarplotSegmentCommandPayload("1", &barIndex, &segmentIndex, &tail));
    std::vector<std::string> uniqueLevels = OrderedUniqueBarplotLevels({"B", "A", "B", "NA", "A"});
    assert(uniqueLevels.size() == 3);
    assert(uniqueLevels[0] == "B");
    assert(uniqueLevels[1] == "A");
    assert(uniqueLevels[2] == "NA");
    std::set<std::string> segmentKeys = BarplotSegmentOverrideKeys({{"am=0", "A"}, {"am=0", "A"}, {"am=1", "B"}});
    assert(segmentKeys.size() == 2);
    assert(segmentKeys.find(BarplotSegmentOverrideKey("am=0", "A")) != segmentKeys.end());
    assert(segmentKeys.find(BarplotSegmentOverrideKey("am=1", "B")) != segmentKeys.end());

    std::vector<std::string> splitX = SplitBarplotXLabel(" am + cyl + am +  gear ");
    assert(splitX.size() == 3);
    assert(splitX[0] == "am");
    assert(splitX[1] == "cyl");
    assert(splitX[2] == "gear");
    std::vector<std::string> xVars = BarplotXVariablesForLabels({"", " cyl ", "am", "cyl"}, "fallback");
    assert(xVars.size() == 2);
    assert(xVars[0] == "cyl");
    assert(xVars[1] == "am");
    std::vector<std::string> fallbackX = NormalizedBarplotXVariables({}, "am + cyl");
    assert(fallbackX.size() == 2);
    assert(fallbackX[0] == "am");
    assert(fallbackX[1] == "cyl");

    std::vector<std::string> condition = SplitBarplotConditionLabel("am=0; cyl=4");
    assert(condition.size() == 2);
    assert(condition[0] == "am=0");
    assert(condition[1] == "cyl=4");
    std::vector<std::string> components = BarplotCategoryComponents({"am", "cyl"}, "am=0; cyl=4");
    assert(components.size() == 2);
    assert(components[0] == "0");
    assert(components[1] == "4");
    assert(BarplotCategoryComponents({"am"}, "0")[0] == "0");
    assert(BarplotSharedCategoryPrefixDepth({"am", "cyl"}, "am=0; cyl=4", "am=0; cyl=6") == 1);
    assert(BarplotSharedCategoryPrefixDepth({"am", "cyl"}, "am=0; cyl=4", "am=1; cyl=4") == 0);
    assert(BarplotCategoryLabelForValues({"am"}, {"0"}) == "0");
    assert(BarplotCategoryLabelForValues({"am", "cyl"}, {"0", "4"}) == "am=0; cyl=4");
    assert(BarplotCategoryLabelForValues({"am", "cyl"}, {"0"}) == "am=0; cyl=NA");
    assert(GeneratedBarplotTitle("", "") == "Bar chart of variable");
    assert(GeneratedBarplotTitle("am + cyl", "") == "Bar chart of am + cyl");
    assert(GeneratedBarplotTitle("am", "gear") == "Bar chart of am by gear");
    assert(BarplotTitleIsAutoGenerated(""));
    assert(BarplotTitleIsAutoGenerated("Bar chart of am"));
    assert(!BarplotTitleIsAutoGenerated("My custom title"));
    assert(RefreshedBarplotTitle("", "am", "") == "Bar chart of am");
    assert(RefreshedBarplotTitle("Bar chart of am", "cyl", "gear") == "Bar chart of cyl by gear");
    assert(RefreshedBarplotTitle("My custom title", "cyl", "gear") == "My custom title");
    assert(BarplotHeightScaleLabel("count") == "Counts");
    assert(BarplotHeightScaleLabel("conditional_percent") == "% within X");
    assert(BarplotHeightScaleLabel("overall_percent") == "% of total");
    assert(BarplotWidthScaleLabel("equal") == "equal");
    assert(BarplotWidthScaleLabel("proportional_n") == "proportional to N");
    assert(BarplotWidthScaleLabel("proportional_percent") == "proportional to % of total");
    assert(BarplotSegmentEncodingLabel(true, "pattern_only") == "solid color + border");
    assert(BarplotSegmentEncodingLabel(false, "pattern_only") == "transparent color");
    assert(BarplotSegmentEncodingLabel(false, "transparent_color_only") == "transparent color");
    assert(BarplotSegmentEncodingLabel(false, "transparent_color_pattern") == "transparent color");
    assert(BarplotSubtitle("overall_percent", "proportional_n", "gear", 20.0, "pattern_only") ==
           "Height: % of total | Width: proportional to N | Split: gear | Bar border: 12.0 px | Segment encoding: solid color + border");
    assert(BarplotSubtitle("count", "equal", "", 3.0, "transparent_color_only") ==
           "Height: Counts | Width: equal | Split: none | Bar border: 3.0 px | Segment encoding: transparent color");
    std::vector<rlispstat::core::BarplotAxisTick> countTicks = BarplotYAxisTicks(20.0, "count");
    assert(countTicks.size() == 5);
    assert(closeEnough(countTicks[2].value, 10.0));
    assert(countTicks[2].label == "10");
    std::vector<rlispstat::core::BarplotAxisTick> percentTicks = BarplotYAxisTicks(100.0, "overall_percent");
    assert(percentTicks.size() == 5);
    assert(percentTicks[2].label == "50%");
    assert(BarplotYAxisTicks(NAN, "count").empty());
    assert(BarplotModeMenuOptions().size() == 3);
    assert(BarplotModeIsValid("conditional_percent"));
    assert(!BarplotModeIsValid("frequency"));
    assert(BarplotWidthMenuOptions().size() == 3);
    assert(BarplotWidthModeIsValid("proportional_n"));
    assert(!BarplotWidthModeIsValid("wide"));
    assert(BarplotRowColorDisplayIsValid("composition_strip"));
    assert(BarplotRowColorDisplayAllowed("bar_fill", false));
    assert(!BarplotRowColorDisplayAllowed("bar_fill", true));
    assert(BarplotRowColorDisplayMenuOptions(true).back().value == "bar_fill");
    assert(!BarplotRowColorDisplayMenuOptions(true).back().enabled);
    assert(BarplotSelectionDisplayMenuOptions().back().value == "overlay_outline");
    assert(BarplotSelectionDisplayModeIsValid("overlay"));
    assert(!BarplotSelectionDisplayModeIsValid("glow"));
    assert(BarplotSegmentEncodingMenuOptions().front().command == "BARPLOT_SEGMENT_ENCODING|transparent_color_pattern");
    BarplotXMenuState xMenuState = BuildBarplotXMenuState(
        {"mpg", "cyl", "am", "gear", "mpg", ""},
        {"cyl", "am"},
        "gear");
    assert(xMenuState.title == "X Axis");
    assert(xMenuState.addXVariableTitle == "Add X Variable");
    assert(xMenuState.replaceOneXVariableTitle == "Replace One X Variable");
    assert(xMenuState.removeXVariableTitle == "Remove X Variable");
    assert(xMenuState.emptyTitle == "No available variables");
    assert(xMenuState.setOptions.size() == 3);
    assert(xMenuState.setOptions[0].command == "BARPLOT_SET_X|mpg");
    assert(xMenuState.addOptions.size() == 1);
    assert(xMenuState.addOptions[0].value == "mpg");
    assert(xMenuState.replaceSections.size() == 2);
    assert(xMenuState.replaceSections[0].variable == "cyl");
    assert(xMenuState.replaceSections[0].options.size() == 2);
    assert(xMenuState.replaceSections[0].options[0].value == "mpg");
    assert(xMenuState.replaceSections[0].options[1].value == "cyl");
    assert(xMenuState.replaceSections[0].options[1].checked);
    assert(xMenuState.removeOptions.size() == 2);
    assert(xMenuState.removeOptions[1].command == "BARPLOT_REMOVE_X|am");
    BarplotXMenuState singleXMenuState = BuildBarplotXMenuState({"mpg", "cyl"}, {"mpg"}, "");
    assert(singleXMenuState.setOptions[0].checked);
    assert(singleXMenuState.replaceSections.empty());
    assert(singleXMenuState.removeOptions.empty());
    BarplotSplitMenuState splitMenuState = BuildBarplotSplitMenuState(
        {"mpg", "cyl", "am", "gear"},
        {"cyl", "am"},
        "gear");
    assert(splitMenuState.title == "Split variable");
    assert(splitMenuState.emptyTitle == "No available variables");
    assert(!splitMenuState.noneOption.checked);
    assert(splitMenuState.splitOptions.size() == 2);
    assert(splitMenuState.splitOptions[0].value == "mpg");
    assert(splitMenuState.splitOptions[1].checked);
    assert(BarplotAnalysisMenuTitle({"cyl"}, "") == "Frequency table: cyl");
    assert(BarplotAnalysisMenuTitle({"cyl", "am"}, "") == "Contingency Table…");
    assert(BarplotAnalysisMenuTitle({}, "") == "Frequency table");
    assert(BarplotAnalysisMenuTitle({"cyl"}, "gear") == "Contingency Table…");
    assert(BarplotXVariablesAfterSet(" mpg ") == std::vector<std::string>({"mpg"}));
    assert(BarplotXVariablesAfterSet(" ").empty());
    assert(BarplotXVariablesAfterAdd({"cyl"}, "", "am") == std::vector<std::string>({"cyl", "am"}));
    assert(BarplotXVariablesAfterAdd({"cyl"}, "", "cyl") == std::vector<std::string>({"cyl"}));
    assert(BarplotXVariablesAfterAdd({}, "cyl + am", "gear") ==
           std::vector<std::string>({"cyl", "am", "gear"}));
    assert(BarplotXVariablesAfterReplace({"cyl", "am"}, "", "am", "gear") ==
           std::vector<std::string>({"cyl", "gear"}));
    assert(BarplotXVariablesAfterReplace({"cyl", "am"}, "", "missing", "gear") ==
           std::vector<std::string>({"cyl", "am"}));
    assert(BarplotXVariablesAfterRemove({"cyl", "am"}, "", "am") ==
           std::vector<std::string>({"cyl"}));
    assert(BarplotXVariablesAfterRemove({"cyl"}, "", "cyl") ==
           std::vector<std::string>({"cyl"}));
    auto splitState = BarplotStateAfterSplitBy({"cyl", "am"}, "", "", "bar_fill", false, "gear");
    assert(splitState.changed);
    assert(splitState.splitVariable == "gear");
    assert(splitState.rowColorDisplay == "tooltip");
    assert(splitState.showConditionalPercent);
    assert(splitState.resetSplitVisualState);
    auto rejectedSplitState = BarplotStateAfterSplitBy({"cyl", "am"}, "", "", "bar_fill", false, "am");
    assert(!rejectedSplitState.changed);
    assert(rejectedSplitState.splitVariable.empty());
    auto clearedSplitState = BarplotStateAfterClearSplit("gear", "composition_strip", true);
    assert(clearedSplitState.changed);
    assert(clearedSplitState.splitVariable.empty());
    assert(clearedSplitState.rowColorDisplay == "bar_fill");
    assert(!clearedSplitState.showConditionalPercent);
    assert(BarplotConditionalPercentForMode("conditional_percent", true));
    assert(!BarplotConditionalPercentForMode("conditional_percent", false));
    assert(!BarplotConditionalPercentForMode("count", true));
    assert(BarplotRowColorDisplayAfterSet("bar_fill", false, "hide") == "bar_fill");
    assert(BarplotRowColorDisplayAfterSet("bar_fill", true, "hide") == "hide");
    assert(BarplotSelectionDisplayAfterSet("outline", "hide") == "outline");
    assert(BarplotSelectionDisplayAfterSet("glow", "hide") == "hide");
    auto conditionalPercentOption = BarplotConditionalPercentMenuOption(true, false);
    assert(conditionalPercentOption.checked);
    assert(!conditionalPercentOption.enabled);
    assert(conditionalPercentOption.command == "BARPLOT_TOGGLE_CONDITIONAL_PERCENT");
    assert(BarplotSplitStrokeWidthMenuOption(20.0).title == "Bar Border Width... (12.0 px)");
    BarplotSplitStrokeWidthDialogState borderDialog =
        BuildBarplotSplitStrokeWidthDialogState(20.0);
    assert(borderDialog.title == "Bar Border Width");
    assert(borderDialog.informativeText == "Enter border width in pixels (range 1.0 to 12.0).");
    assert(borderDialog.applyButtonTitle == "Apply");
    assert(borderDialog.cancelButtonTitle == "Cancel");
    assert(borderDialog.invalidNumberStatus == "Bar border width must be a numeric value.");
    assert(borderDialog.currentValueText == "12.00");
    assert(!BarplotSplitStyleMenuOption().enabled);
    assert(BarplotSplitStyleMenuOption().title == "Split style: solid colors + thick borders");
    assert(BarplotShowPatternsMenuOption(true).checked);
    auto displayMenuState = BuildBarplotDisplayMenuState(
        "overall_percent",
        "proportional_n",
        "",
        "bar_fill",
        false,
        2.25,
        "pattern_only",
        true,
        "overlay_outline",
        {"cyl"});
    assert(displayMenuState.title == "Bar Chart");
    assert(displayMenuState.yAxisTitle == "Y Axis");
    assert(displayMenuState.barWidthTitle == "Bar Width");
    assert(displayMenuState.rowColorsTitle == "Row Colors");
    assert(displayMenuState.segmentEncodingTitle == "Segment Encoding");
    assert(displayMenuState.selectionDisplayTitle == "Selection Display");
    assert(!displayMenuState.showSegmentEncodingMenu);
    assert(displayMenuState.modeOptions[1].checked);
    assert(displayMenuState.widthOptions[1].checked);
    assert(displayMenuState.rowColorOptions[3].checked);
    assert(displayMenuState.segmentEncodingOptions[2].checked);
    assert(displayMenuState.showPatterns.checked);
    assert(displayMenuState.selectionDisplayOptions[3].checked);
    assert(displayMenuState.analysis.title == "Frequency table: cyl");
    assert(displayMenuState.analysis.command == "CONTEXT_BARCHART_TABLE");
    assert(displayMenuState.selectionOptions.size() == 2);
    assert(displayMenuState.selectionOptions[0].command == "CLEAR_SELECTION");
    assert(displayMenuState.selectionOptions[1].command == "INVERT_SELECTION");
    assert(displayMenuState.plotOptions.size() == 8);
    assert(displayMenuState.plotOptions[1].command == "PLOT_NEW_TRELLIS_SCATTERPLOT");
    assert(displayMenuState.plotOptions[2].command == "PLOT_NEW_TIME_SERIES");
    assert(displayMenuState.plotOptions[3].command == "PLOT_NEW_LINKED_SCATTER_MATRIX");
    assert(displayMenuState.plotOptions[4].value == "parallel_coordinates");
    assert(displayMenuState.plotOptions[7].value == "bar_chart");
    auto splitDisplayMenuState = BuildBarplotDisplayMenuState(
        "conditional_percent",
        "equal",
        "gear",
        "tooltip",
        true,
        4.0,
        "transparent_color_pattern",
        false,
        "outline",
        {"cyl", "am"});
    assert(!splitDisplayMenuState.showSegmentEncodingMenu);
    assert(splitDisplayMenuState.conditionalPercent.checked);
    assert(splitDisplayMenuState.conditionalPercent.enabled);
    assert(splitDisplayMenuState.rowColorOptions[3].enabled == false);
    assert(splitDisplayMenuState.splitStyle.enabled == false);
    assert(splitDisplayMenuState.selectionDisplayOptions[1].checked);
    assert(splitDisplayMenuState.analysis.title == "Contingency Table…");
    assert(BarplotModeAfterSet("overall_percent", "count") == "overall_percent");
    assert(BarplotModeAfterSet("frequency", "count") == "count");
    assert(BarplotWidthModeAfterSet("proportional_percent", "equal") == "proportional_percent");
    assert(BarplotWidthModeAfterSet("wide", "equal") == "equal");
    auto encodingState = BarplotStateAfterSegmentEncoding(
        "transparent_color_pattern",
        true,
        "transparent_color_only");
    assert(encodingState.changed);
    assert(encodingState.segmentEncodingMode == "transparent_color_only");
    assert(!encodingState.showPatterns);
    auto invalidEncodingState = BarplotStateAfterSegmentEncoding(
        "transparent_color_pattern",
        true,
        "sparkles");
    assert(!invalidEncodingState.changed);
    assert(invalidEncodingState.segmentEncodingMode == "transparent_color_pattern");
    assert(invalidEncodingState.showPatterns);
    assert(!BarplotShowPatternsAfterCommand(true, "toggle"));
    assert(BarplotShowPatternsAfterCommand(false, "toggle"));
    assert(BarplotShowPatternsAfterCommand(false, "on"));
    assert(!BarplotShowPatternsAfterCommand(true, "off"));

    std::vector<std::size_t> order = BarplotCategorySortOrder(
        {"am", "cyl"},
        {"am=0; cyl=4", "am=1; cyl=4", "am=0; cyl=6", "am=1; cyl=6"});
    assert(order.size() == 4);
    assert(order[0] == 0);
    assert(order[1] == 2);
    assert(order[2] == 1);
    assert(order[3] == 3);

    PlotModel singleVariableOrder;
    singleVariableOrder.kind = "barplot";
    singleVariableOrder.xLabel = "cyl";
    singleVariableOrder.barplotXVariables = {"cyl"};
    for (const std::string &category : {"6", "4", "8"}) {
        singleVariableOrder.barplotBins.emplace_back();
        singleVariableOrder.barplotBins.back().category = category;
    }
    rlispstat::core::SortBarplotBinsByXHierarchy(singleVariableOrder);
    assert(singleVariableOrder.barplotBins[0].category == "4");
    assert(singleVariableOrder.barplotBins[1].category == "6");
    assert(singleVariableOrder.barplotBins[2].category == "8");

    BarplotBuildInput build;
    build.xVariables = {"am", "cyl"};
    build.widthMode = "proportional_percent";
    build.rows = {
        BarplotInputRow{1, {"0", "4"}, "low"},
        BarplotInputRow{2, {"1", "4"}, "high"},
        BarplotInputRow{3, {"0", "6"}, "low"},
        BarplotInputRow{4, {"1", "6"}, "high"},
        BarplotInputRow{5, {"0", "4"}, "high"}
    };
    BarplotBuildResult built = BuildBarplotBins(build);
    assert(built.totalN == 5);
    assert(built.bins.size() == 4);
    assert(built.bins[0].category == "am=0; cyl=4");
    assert(built.bins[1].category == "am=0; cyl=6");
    assert(built.bins[2].category == "am=1; cyl=4");
    assert(built.bins[3].category == "am=1; cyl=6");
    assert(built.bins[0].n == 2);
    assert(closeEnough(built.bins[0].percent, 40.0));
    assert(closeEnough(built.bins[0].widthValue, 40.0));
    assert(built.bins[0].segments.size() == 2);
    assert(built.bins[0].segments[0].level == "high");
    assert(built.bins[0].segments[0].count == 1);
    assert(closeEnough(built.bins[0].segments[0].conditionalPercent, 50.0));
    assert(closeEnough(built.bins[0].segments[0].overallPercent, 20.0));
    assert(closeEnough(built.bins[0].segments[0].barPercent, 40.0));
    assert(closeEnough(built.bins[0].segments[0].barWidthValue, 40.0));
    assert(built.bins[1].segments.size() == 1);
    assert(built.bins[1].segments[0].barN == 1);
    assert(built.bins[1].segments[0].totalN == 5);
    DataColumn amColumn;
    amColumn.name = "am";
    amColumn.values = {"0", "1", "0", "1", "0"};
    DataColumn cylColumn;
    cylColumn.name = "cyl";
    cylColumn.values = {"4", "4", "6", "6", ""};
    DataColumn gearColumn;
    gearColumn.name = "gear";
    gearColumn.values = {"low", "high", "low", "", "high"};
    BarplotBuildResult builtFromColumns = BuildBarplotBinsForColumns(
        std::vector<const DataColumn *>{&amColumn, &cylColumn},
        &gearColumn,
        "proportional_percent");
    assert(builtFromColumns.totalN == 5);
    assert(builtFromColumns.bins.size() == 5);
    auto builtColumnBin = [&](const std::string &category) -> const BarplotBinSummary * {
        for (const BarplotBinSummary &bin : builtFromColumns.bins) {
            if (bin.category == category) {
                return &bin;
            }
        }
        return nullptr;
    };
    const BarplotBinSummary *missingCylBin = builtColumnBin("am=0; cyl=NA");
    assert(missingCylBin != nullptr);
    assert(missingCylBin->segments.size() == 1);
    assert(missingCylBin->segments[0].level == "high");
    const BarplotBinSummary *missingGearBin = builtColumnBin("am=1; cyl=6");
    assert(missingGearBin != nullptr);
    assert(missingGearBin->segments.size() == 1);
    assert(missingGearBin->segments[0].level == "NA");
    PlotModel barplotModel;
    barplotModel.kind = "barplot";
    barplotModel.barplotWidthMode = "proportional_percent";
    ApplyBarplotBuildResultToModel(barplotModel, builtFromColumns);
    assert(barplotModel.barplotTotalN == 5);
    assert(barplotModel.barplotBins.size() == builtFromColumns.bins.size());
    assert(barplotModel.barplotBins[0].segments[0].barWidthValue == builtFromColumns.bins[0].segments[0].barWidthValue);
    DataFrameModel barplotDf;
    barplotDf.group = "cars";
    barplotDf.rows = 5;
    barplotDf.columns = {amColumn, cylColumn, gearColumn};
    DataColumn idColumn;
    idColumn.name = "id";
    idColumn.type = "numeric";
    idColumn.values = {"1", "2", "3", "4", "5"};
    DataFrameModel barplotMenuDf = barplotDf;
    barplotMenuDf.columns.insert(barplotMenuDf.columns.begin(), idColumn);
    std::vector<std::string> availableBarplotVariables = BarplotAvailableVariables(barplotMenuDf);
    assert(availableBarplotVariables.size() == 3);
    assert(availableBarplotVariables[0] == "am");
    assert(availableBarplotVariables[1] == "cyl");
    assert(availableBarplotVariables[2] == "gear");
    PlotModel rebuiltBarplot;
    rebuiltBarplot.kind = "barplot";
    rebuiltBarplot.xLabel = "am";
    rebuiltBarplot.barplotXVariables = {"am", "cyl"};
    rebuiltBarplot.barplotSplitVariable = "gear";
    rebuiltBarplot.barplotShowConditionalPercent = true;
    rebuiltBarplot.barplotWidthMode = "proportional_percent";
    std::string rebuildMessage;
    assert(RebuildBarplotFromDataFrame(rebuiltBarplot, barplotDf, &rebuildMessage));
    assert(rebuildMessage.empty());
    assert(rebuiltBarplot.barplotBins.size() == 5);
    assert(rebuiltBarplot.barplotSplitVariable == "gear");
    PlotModel staleSplitBarplot = rebuiltBarplot;
    staleSplitBarplot.barplotSplitVariable = "missing";
    staleSplitBarplot.barplotShowConditionalPercent = true;
    assert(RebuildBarplotFromDataFrame(staleSplitBarplot, barplotDf, nullptr));
    assert(staleSplitBarplot.barplotSplitVariable.empty());
    assert(!staleSplitBarplot.barplotShowConditionalPercent);
    PlotModel xSplitBarplot = rebuiltBarplot;
    xSplitBarplot.barplotSplitVariable = "am";
    xSplitBarplot.barplotShowConditionalPercent = true;
    assert(RebuildBarplotFromDataFrame(xSplitBarplot, barplotDf, nullptr));
    assert(xSplitBarplot.barplotSplitVariable.empty());
    assert(!xSplitBarplot.barplotShowConditionalPercent);
    PlotModel staleXBarplot = rebuiltBarplot;
    staleXBarplot.barplotXVariables = {"missing"};
    staleXBarplot.xLabel = "missing";
    staleXBarplot.barplotBins = rebuiltBarplot.barplotBins;
    staleXBarplot.barplotTotalN = rebuiltBarplot.barplotTotalN;
    assert(RefreshBarplotFromDataFrame(staleXBarplot, barplotDf));
    assert(staleXBarplot.barplotBins.empty());
    assert(staleXBarplot.barplotTotalN == 0);
    PlotModel refreshStaleSplit = rebuiltBarplot;
    refreshStaleSplit.barplotSplitVariable = "missing";
    refreshStaleSplit.barplotShowConditionalPercent = true;
    assert(RefreshBarplotFromDataFrame(refreshStaleSplit, barplotDf));
    assert(refreshStaleSplit.barplotSplitVariable.empty());
    assert(!refreshStaleSplit.barplotShowConditionalPercent);
    auto segmentReference = BarplotSegmentReferenceForIndices(built.bins, 0, 1);
    assert(segmentReference.has_value());
    assert(segmentReference->category == "am=0; cyl=4");
    assert(segmentReference->segment.level == "low");
    assert(segmentReference->segment.rows == std::vector<int>({1}));
    assert(BarplotSegmentCommandPayload(0, 1) == "0|1");
    assert(BarplotSegmentMenuTitle("am + cyl", "gear", *segmentReference) ==
           "am + cyl = am=0; cyl=4 | gear = low");
    assert(BarplotSegmentOpacityMenuValues().size() == 6);
    assert(closeEnough(BarplotSegmentOpacityMenuValues().front(), 0.25));
    assert(BarplotPatternMenuValues().front() == "diagonal_slash");
    assert(BarplotPatternMenuValues().back() == "none");
    BarplotSegmentVisualStyle menuStyle;
    menuStyle.colorKey = "orange";
    menuStyle.alpha = 0.70;
    menuStyle.pattern = "dots";
    menuStyle.hasSegmentColorOverride = true;
    menuStyle.hasSegmentPatternOverride = true;
    auto segmentMenuState = BuildBarplotSegmentMenuState(
        "am + cyl",
        "gear",
        *segmentReference,
        menuStyle,
        {"blue", "orange"},
        "blue",
        "crosshatch");
    assert(segmentMenuState.title == "am + cyl = am=0; cyl=4 | gear = low");
    assert(segmentMenuState.payload == "0|1");
    assert(segmentMenuState.selectRows.command == "BARPLOT_SEGMENT_SELECT|0|1");
    assert(segmentMenuState.applyCurrentColorToRows.command == "BARPLOT_SEGMENT_APPLY_COLOR_TO_ROWS|0|1");
    assert(segmentMenuState.segmentColorOptions.size() == 2);
    assert(!segmentMenuState.segmentColorOptions[0].checked);
    assert(segmentMenuState.segmentColorOptions[1].checked);
    assert(segmentMenuState.levelColorOptions[0].checked);
    assert(segmentMenuState.segmentOpacityOptions[3].checked);
    assert(segmentMenuState.levelPatternOptions[2].checked);
    assert(segmentMenuState.segmentPatternOptions[1].checked);
    assert(segmentMenuState.resetLevelColor.command == "BARPLOT_LEVEL_RESET_COLOR|low");
    assert(segmentMenuState.details.command == "BARPLOT_SEGMENT_DETAILS|0|1");
    BarplotSegmentReference encodedReference = *segmentReference;
    encodedReference.segment.level = "hi|gh";
    auto encodedSegmentMenuState = BuildBarplotSegmentMenuState(
        "am + cyl",
        "gear",
        encodedReference,
        menuStyle,
        {"orange"},
        "",
        "");
    assert(encodedSegmentMenuState.resetLevelColor.command == "BARPLOT_LEVEL_RESET_COLOR|hi%7Cgh");
    assert(BarplotRowsForSegmentIndices(built.bins, 0, 0) == std::vector<int>({5}));
    assert(BarplotRowsForSegmentIndices(built.bins, 0, 1) == std::vector<int>({1}));
    assert(BarplotRowsForSegmentIndices(built.bins, 99, 0).empty());

    Rect plainPlotRect = BarplotPlotRect(Rect{0.0, 0.0, 720.0, 520.0}, false, false, 1);
    assert(closeEnough(plainPlotRect.x, 64.0));
    assert(closeEnough(plainPlotRect.y, 42.0));
    assert(closeEnough(plainPlotRect.width, 624.0));
    assert(closeEnough(plainPlotRect.height, 408.0));
    Rect splitNestedPlotRect = BarplotPlotRect(Rect{0.0, 0.0, 720.0, 520.0}, true, true, 4);
    assert(closeEnough(splitNestedPlotRect.x, 118.0));
    assert(closeEnough(splitNestedPlotRect.y, 66.0));
    assert(closeEnough(splitNestedPlotRect.height, 342.0));
    Rect xAxisRect = BarplotXAxisLabelRect(Rect{0.0, 0.0, 720.0, 520.0}, plainPlotRect);
    assert(closeEnough(xAxisRect.x, plainPlotRect.x));
    assert(closeEnough(xAxisRect.y, 473.0));
    assert(closeEnough(BarplotCategoryLabelVerticalOffset(false), 7.0));
    assert(closeEnough(BarplotCategoryLabelVerticalOffset(true), 9.0));
    assert(BarplotMaxBarCount(built.bins) == 2);
    assert(closeEnough(BarplotValueForBin(built.bins[0], "count"), 2.0));
    assert(closeEnough(BarplotValueForBin(built.bins[0], "overall_percent"), 40.0));
    assert(closeEnough(BarplotValueForBin(built.bins[0], "conditional_percent"), 100.0));
    assert(closeEnough(BarplotWidthValueForBin("proportional_n", 7, 25.0), 7.0));
    assert(closeEnough(BarplotWidthValueForBin("proportional_percent", 7, 25.0), 25.0));
    assert(closeEnough(BarplotYMaximum(built.bins, "count"), 2.0));
    assert(closeEnough(BarplotYMaximum(built.bins, "overall_percent"), 100.0));
    auto simplePreferred = BarplotPreferredViewSize(3, false, 1);
    assert(closeEnough(simplePreferred.width, 620.0));
    assert(closeEnough(simplePreferred.height, 460.0));
    auto splitPreferred = BarplotPreferredViewSize(12, true, 3);
    assert(splitPreferred.width > simplePreferred.width);
    assert(splitPreferred.height > simplePreferred.height);
    assert(splitPreferred.width <= 760.0);

    BarplotLayoutInput layoutInput;
    layoutInput.viewRect = Rect{0.0, 0.0, 720.0, 520.0};
    layoutInput.xVariables = {"am", "cyl"};
    layoutInput.bins = built.bins;
    layoutInput.mode = "count";
    layoutInput.widthMode = "equal";
    layoutInput.hasSplit = true;
    layoutInput.showCompositionStrip = false;
    BarplotLayout coreBuiltLayout = BuildBarplotLayout(layoutInput);
    assert(coreBuiltLayout.nestingDepth == 2);
    assert(closeEnough(coreBuiltLayout.yMaximum, 2.0));
    assert(BarplotBarRect(coreBuiltLayout, 0).y > coreBuiltLayout.plotRect.y);
    assert(coreBuiltLayout.widthMode == BarplotWidthMode::Equal);
    assert(coreBuiltLayout.values == std::vector<double>({2.0, 1.0, 1.0, 1.0}));
    assert(coreBuiltLayout.sharedPrefixDepthWithPrevious == std::vector<int>({0, 1, 0, 1}));

    BarplotLayout builtLayout;
    builtLayout.plotRect = Rect{20.0, 30.0, 400.0, 200.0};
    builtLayout.nestingDepth = 2;
    builtLayout.yMaximum = 2.0;
    builtLayout.widthMode = BarplotWidthMode::Equal;
    builtLayout.sharedPrefixDepthWithPrevious.resize(built.bins.size(), 0);
    for (std::size_t i = 0; i < built.bins.size(); ++i) {
        builtLayout.values.push_back(built.bins[i].n);
        builtLayout.widthValues.push_back(built.bins[i].widthValue);
        if (i > 0) {
            builtLayout.sharedPrefixDepthWithPrevious[i] =
                BarplotSharedCategoryPrefixDepth({"am", "cyl"}, built.bins[i - 1].category, built.bins[i].category);
        }
    }
    Rect builtFirstBar = BarplotBarRect(builtLayout, 0);
    Rect builtFirstLabel = BarplotCategoryLabelRect(builtLayout.plotRect, builtFirstBar, 2, 7.0);
    Rect builtFirstComponent0 = BarplotCategoryComponentRect(builtFirstLabel, 2, 0);
    std::vector<rlispstat::core::BarplotCategoryLabelDrawItem> nestedLabels =
        BuildBarplotCategoryLabelDrawPlan(builtLayout, built.bins, {"am", "cyl"}, 7.0);
    assert(nestedLabels.size() == built.bins.size() * 2);
    assert(nestedLabels[0].barIndex == 0);
    assert(nestedLabels[0].variableIndex == 0);
    assert(nestedLabels[0].label == "am=0");
    assert(nestedLabels[1].barIndex == 0);
    assert(nestedLabels[1].variableIndex == 1);
    assert(nestedLabels[1].label == "cyl=4");
    std::vector<rlispstat::core::BarplotCategoryLabelDrawItem> simpleLabels =
        BuildBarplotCategoryLabelDrawPlan(builtLayout, built.bins, {"am"}, 7.0);
    assert(simpleLabels.size() == built.bins.size());
    assert(simpleLabels[0].label == "am=0; cyl=4");
    auto componentHit = BarplotCategoryComponentAtPoint(
        builtLayout,
        built.bins,
        {"am", "cyl"},
        Point{builtFirstComponent0.x + 2.0, builtFirstComponent0.y + 2.0},
        7.0);
    assert(componentHit.has_value());
    assert(componentHit->barIndex == 0);
    assert(componentHit->variableIndex == 0);
    assert(componentHit->value == "0");

    std::vector<int> am0Rows = BarplotRowsForXVariableValue(built.bins, {"am", "cyl"}, 0, "0");
    assert(am0Rows == std::vector<int>({1, 3, 5}));
    std::vector<int> cyl4Rows = BarplotRowsForXVariableValue(built.bins, {"am", "cyl"}, 1, "4");
    assert(cyl4Rows == std::vector<int>({1, 2, 5}));
    assert(BarplotRowsForXVariableValue(built.bins, {"am", "cyl"}, 2, "x").empty());

    std::vector<std::string> splitLevels = BarplotSplitLevelsForBins(built.bins, true);
    assert(splitLevels.size() == 2);
    assert(splitLevels[0] == "high");
    assert(splitLevels[1] == "low");
    assert(BarplotLevelIndex(splitLevels, "high") == 0);
    assert(BarplotLevelIndex(splitLevels, "low") == 1);
    assert(BarplotLevelIndex(splitLevels, "missing", 3) == 3);
    std::map<std::string, int> splitTotals = BarplotSplitLevelTotalsForBins(built.bins, splitLevels);
    assert(splitTotals["low"] == 2);
    assert(splitTotals["high"] == 3);
    BarplotSideLabelDrawPlan sideLabels = BuildBarplotSideLabelDrawPlan(
        builtLayout.plotRect,
        "group",
        splitLevels,
        splitTotals,
        true);
    assert(sideLabels.splitTitle == "Split: group");
    assert(closeEnough(sideLabels.splitTitleRect.width,
                       std::max(42.0, builtLayout.plotRect.x - 16.0)));
    assert(sideLabels.splitLabels.size() == 2);
    assert(sideLabels.splitLabels[0].label == "high");
    assert(sideLabels.splitLabels[1].label == "low");
    assert(sideLabels.showRowStripLabel);
    assert(sideLabels.rowStripLabel == "Row strip");
    assert(closeEnough(sideLabels.rowStripLabelRect.y, builtLayout.plotRect.y + builtLayout.plotRect.height + 4.0));
    auto splitHit = BarplotSplitLevelLabelIndexAtPoint(
        builtLayout.plotRect,
        splitLevels,
        splitTotals,
        Point{10.0, BarplotSplitLevelLabelRect(builtLayout.plotRect, 0, {3, 2}).y + 4.0});
    assert(splitHit.has_value());
    assert(*splitHit == 0);
    std::vector<int> lowRows = BarplotRowsForSplitLevel(built.bins, "low");
    assert(lowRows == std::vector<int>({1, 3}));
    assert(BarplotRowsForSplitLevel(built.bins, "missing").empty());
    assert(BarplotSplitLevelsForBins(built.bins, false).empty());

    auto builtLowSegment = BarplotSegmentAtPoint(
        builtLayout,
        built.bins,
        splitLevels,
        true,
        Point{builtFirstBar.x + builtFirstBar.width / 2.0, builtFirstBar.y + builtFirstBar.height * 0.75});
    assert(builtLowSegment.has_value());
    assert(builtLowSegment->barIndex == 0);
    assert(builtLowSegment->segmentIndex == 0);
    assert(builtLowSegment->level == "high");
    assert(BarplotRowsAtPoint(
               builtLayout,
               built.bins,
               splitLevels,
               true,
               Point{builtFirstBar.x + builtFirstBar.width / 2.0, builtFirstBar.y + builtFirstBar.height * 0.75}) ==
           std::vector<int>({5}));
    assert(BarplotRowsAtPoint(
               builtLayout,
               built.bins,
               splitLevels,
               false,
               Point{builtFirstBar.x + builtFirstBar.width / 2.0, builtFirstBar.y + builtFirstBar.height * 0.75}) ==
           std::vector<int>({1, 5}));
    std::set<int> clickGestureRows = BarplotRowsForGesture(
        builtLayout,
        built.bins,
        splitLevels,
        true,
        Point{builtFirstBar.x + builtFirstBar.width / 2.0, builtFirstBar.y + builtFirstBar.height * 0.75},
        Point{builtFirstBar.x + builtFirstBar.width / 2.0 + 1.0, builtFirstBar.y + builtFirstBar.height * 0.75 + 1.0});
    assert(clickGestureRows == std::set<int>({5}));
    Rect builtThirdBar = BarplotBarRect(builtLayout, 2);
    std::set<int> dragGestureRows = BarplotRowsForGesture(
        builtLayout,
        built.bins,
        splitLevels,
        true,
        Point{builtFirstBar.x + builtFirstBar.width / 2.0, builtFirstBar.y + 2.0},
        Point{builtThirdBar.x + builtThirdBar.width / 2.0, builtThirdBar.y + 20.0});
    assert(dragGestureRows == std::set<int>({1, 2, 3, 5}));
    assert(BarplotCategoryTooltipText("am", "0", am0Rows, {1, 4}).find("selected = 1 of 3") != std::string::npos);
    assert(BarplotSplitLevelTooltipText("gear", "low", lowRows, {1, 3, 5}).find("selected = 2 of 2") != std::string::npos);

    BarplotSegmentVisualStyle tooltipStyle;
    tooltipStyle.colorKey = "orange";
    tooltipStyle.alpha = 0.65;
    tooltipStyle.pattern = "dots";
    tooltipStyle.hasSegmentColorOverride = true;
    std::string segmentTooltip = BarplotSegmentTooltipText(
        "am + cyl",
        "gear",
        "proportional_percent",
        built.bins[0],
        built.totalN,
        std::optional<BarplotSegmentSummary>(built.bins[0].segments[0]),
        std::optional<BarplotSegmentVisualStyle>(tooltipStyle),
        {{5, "orange"}},
        {5},
        {"blue", "orange"},
        true);
    assert(segmentTooltip.find("am + cyl = am=0; cyl=4") != std::string::npos);
    assert(segmentTooltip.find("gear = high") != std::string::npos);
    assert(segmentTooltip.find("conditional percent = 50.0%") != std::string::npos);
    assert(segmentTooltip.find("segment color = orange") != std::string::npos);
    assert(segmentTooltip.find("segment override = yes") != std::string::npos);
    assert(segmentTooltip.find("selected fraction within segment = 100.0%") != std::string::npos);

    std::string barTooltip = BarplotSegmentTooltipText(
        "am + cyl",
        "gear",
        "equal",
        built.bins[0],
        built.totalN,
        std::nullopt,
        std::nullopt,
        {{1, "orange"}, {5, "blue"}},
        {1},
        {"blue", "orange"},
        true);
    assert(barTooltip.find("gear = Bar total") != std::string::npos);
    assert(barTooltip.find("bar width = equal") != std::string::npos);
    assert(barTooltip.find("selected = 1 of 2 rows") != std::string::npos);
    assert(barTooltip.find("selected fraction within bar = 50.0%") != std::string::npos);

    BarplotVisualState drawState;
    std::vector<rlispstat::core::BarplotSegmentDrawItem> drawPlan =
        BuildBarplotSegmentDrawPlan(builtFirstBar, built.bins[0], splitLevels, drawState, {{5, "orange"}});
    assert(drawPlan.size() == 2);
    assert(drawPlan[0].segmentIndex == 0);
    assert(drawPlan[0].levelIndex == 0);
    assert(drawPlan[0].style.colorKey == "orange");
    assert(closeEnough(drawPlan[0].style.alpha, 0.82));
    assert(drawPlan[0].useRowColorIdentityLayer);
    assert(!drawPlan[0].drawBottomEdge);
    assert(drawPlan[1].drawBottomEdge);

    drawState.segmentColorOverrides[BarplotSegmentOverrideKey(built.bins[0].category, "high")] = "blue";
    std::vector<rlispstat::core::BarplotSegmentDrawItem> overridePlan =
        BuildBarplotSegmentDrawPlan(builtFirstBar, built.bins[0], splitLevels, drawState, {{5, "orange"}});
    assert(overridePlan.size() == 2);
    assert(overridePlan[0].style.colorKey == "blue");
    assert(overridePlan[0].style.hasSegmentColorOverride);
    assert(!overridePlan[0].useRowColorIdentityLayer);

    BarplotBuildInput noSplit;
    noSplit.xVariables = {"group"};
    noSplit.rows = {
        BarplotInputRow{1, {"A"}, "All"},
        BarplotInputRow{2, {"A"}, "All"},
        BarplotInputRow{3, {"B"}, "All"}
    };
    BarplotBuildResult simple = BuildBarplotBins(noSplit);
    assert(simple.totalN == 3);
    assert(simple.bins.size() == 2);
    assert(simple.bins[0].category == "A");
    assert(simple.bins[0].segments.size() == 1);
    assert(simple.bins[0].segments[0].level == "All");
    assert(closeEnough(simple.bins[0].widthValue, 2.0));

    std::string key = BarplotSegmentOverrideKey("am=1", "manual");
    BarplotSegmentVisualStyle style = ResolveBarplotSegmentVisual(
        0.70,
        {{"manual", "white"}},
        {{"manual", 1.25}},
        {{"manual", "dots"}},
        {{key, "orange"}},
        {{key, -0.20}},
        {{key, "crosshatch"}},
        "am=1",
        "manual",
        3);
    assert(style.colorKey == "orange");
    assert(closeEnough(style.alpha, 0.0));
    assert(style.pattern == "crosshatch");
    assert(style.hasSegmentColorOverride);
    assert(style.hasSegmentAlphaOverride);
    assert(style.hasSegmentPatternOverride);

    BarplotVisualState overrideState;
    BarplotSegmentReference visualReference;
    visualReference.category = "am=1";
    visualReference.segment.level = "manual";
    assert(SetBarplotSegmentColorOverride(overrideState, visualReference, "blue"));
    assert(overrideState.segmentColorOverrides[key] == "blue");
    assert(!SetBarplotSegmentColorOverride(overrideState, visualReference, ""));
    assert(SetBarplotLevelColorOverride(overrideState, "manual", "orange"));
    assert(overrideState.levelColors["manual"] == "orange");
    assert(!SetBarplotLevelColorOverride(overrideState, "", "orange"));
    assert(SetBarplotSegmentAlphaOverride(overrideState, visualReference, 1.5));
    assert(closeEnough(overrideState.segmentAlphaOverrides[key], 1.0));
    assert(SetBarplotSegmentAlphaOverride(overrideState, visualReference, -0.5));
    assert(closeEnough(overrideState.segmentAlphaOverrides[key], 0.0));
    assert(SetBarplotLevelPatternOverride(overrideState, "manual", "dots"));
    assert(overrideState.levelPatterns["manual"] == "dots");
    assert(!SetBarplotLevelPatternOverride(overrideState, "manual", "sparkles"));
    assert(SetBarplotSegmentPatternOverride(overrideState, visualReference, "crosshatch"));
    assert(overrideState.segmentPatternOverrides[key] == "crosshatch");
    assert(!SetBarplotSegmentPatternOverride(overrideState, visualReference, "sparkles"));
    assert(ResetBarplotSegmentColorOverride(overrideState, visualReference));
    assert(overrideState.segmentColorOverrides.find(key) == overrideState.segmentColorOverrides.end());
    assert(ResetBarplotLevelColorOverride(overrideState, "manual"));
    assert(overrideState.levelColors.find("manual") == overrideState.levelColors.end());
    assert(!ResetBarplotLevelColorOverride(overrideState, ""));

    BarplotSegmentVisualStyle fallback = ResolveBarplotSegmentVisual(
        0.60,
        {},
        {},
        {{"missing", "unknown"}},
        {},
        {},
        {},
        "x",
        "missing",
        1);
    assert(fallback.colorKey == "white");
    assert(closeEnough(fallback.alpha, 0.60));
    assert(fallback.pattern == "dots");

    BarplotVisualState visualState;
    visualState.defaultSegmentAlpha = 2.0;
    visualState.segmentEncodingMode = "bad";
    visualState.splitStrokeWidth = 20.0;
    visualState.levelColors = {{"manual", ""}, {"old", "orange"}};
    visualState.levelAlpha = {{"manual", -1.0}, {"old", 0.4}};
    visualState.levelPatterns = {{"manual", "unknown"}, {"auto", "dots"}, {"old", "vertical"}};
    visualState.segmentColorOverrides = {{key, "blue"}, {"old-key", "red"}};
    visualState.segmentAlphaOverrides = {{key, 0.2}, {"old-key", 0.3}};
    visualState.segmentPatternOverrides = {{key, "dots"}, {"old-key", "crosshatch"}};
    NormalizeBarplotVisualState(visualState, {"manual", "auto"}, {key});
    assert(closeEnough(visualState.defaultSegmentAlpha, 0.70));
    assert(visualState.segmentEncodingMode == "transparent_color_pattern");
    assert(closeEnough(visualState.splitStrokeWidth, 12.0));
    assert(visualState.levelColors.size() == 2);
    assert(visualState.levelColors["manual"] == "white");
    assert(visualState.levelColors["auto"] == "white");
    assert(closeEnough(visualState.levelAlpha["manual"], 0.0));
    assert(closeEnough(visualState.levelAlpha["auto"], 0.70));
    assert(visualState.levelPatterns["manual"] == "diagonal_slash");
    assert(visualState.levelPatterns["auto"] == "dots");
    assert(visualState.segmentColorOverrides.size() == 1);
    assert(visualState.segmentColorOverrides[key] == "blue");
    assert(visualState.segmentAlphaOverrides.size() == 1);
    assert(visualState.segmentPatternOverrides.size() == 1);

    std::string builtLowKey = BarplotSegmentOverrideKey(built.bins[0].category, "low");
    std::string builtHighKey = BarplotSegmentOverrideKey(built.bins[0].category, "high");
    std::set<std::string> builtOverrideKeys = BarplotSegmentOverrideKeysForBins(built.bins);
    assert(builtOverrideKeys.find(builtLowKey) != builtOverrideKeys.end());
    assert(builtOverrideKeys.find(builtHighKey) != builtOverrideKeys.end());
    BarplotVisualState binsVisualState;
    binsVisualState.levelColors = {{"low", "blue"}, {"stale", "orange"}};
    binsVisualState.segmentColorOverrides = {{builtLowKey, "orange"}, {"old-key", "blue"}};
    binsVisualState.segmentAlphaOverrides = {{builtLowKey, 0.25}, {"old-key", 0.50}};
    NormalizeBarplotVisualStateForBins(binsVisualState, built.bins);
    assert(binsVisualState.levelColors.size() == 2);
    assert(binsVisualState.levelColors["low"] == "blue");
    assert(binsVisualState.levelColors["high"] == "white");
    assert(binsVisualState.levelColors.find("stale") == binsVisualState.levelColors.end());
    assert(binsVisualState.segmentColorOverrides.size() == 1);
    assert(binsVisualState.segmentColorOverrides[builtLowKey] == "orange");
    assert(binsVisualState.segmentAlphaOverrides.size() == 1);
    assert(closeEnough(binsVisualState.segmentAlphaOverrides[builtLowKey], 0.25));

    ClearBarplotSplitVisualState(visualState, false);
    assert(visualState.segmentColorOverrides.empty());
    assert(visualState.segmentAlphaOverrides.empty());
    assert(visualState.segmentPatternOverrides.empty());
    assert(visualState.levelColors.size() == 2);
    assert(visualState.levelAlpha.size() == 2);
    assert(visualState.levelPatterns.size() == 2);

    visualState.segmentColorOverrides[key] = "blue";
    visualState.segmentAlphaOverrides[key] = 0.3;
    visualState.segmentPatternOverrides[key] = "dots";
    ClearBarplotSplitVisualState(visualState, true);
    assert(visualState.segmentColorOverrides.empty());
    assert(visualState.segmentAlphaOverrides.empty());
    assert(visualState.segmentPatternOverrides.empty());
    assert(visualState.levelColors.empty());
    assert(visualState.levelAlpha.empty());
    assert(visualState.levelPatterns.empty());

    assert(BarplotNotAvailableStatus() == "No active bar chart is available.");
    assert(BarplotChooseXVariableStatus() == "Choose at least one X variable.");
    assert(BarplotNoValuesToPlotStatus() == "The selected variable has no values to plot.");
    assert(BarplotWindowTitle() == "Bar Chart");
    assert(BarplotSegmentMenuTitle() == "Segment");
    assert(BarplotSetSegmentColorMenuItemTitle() == "Set segment color...");
    assert(BarplotSetSegmentOpacityMenuItemTitle() == "Set segment opacity...");
    assert(BarplotSetPatternForThisSegmentMenuItemTitle() == "Set pattern for this segment...");
    return 0;
}
