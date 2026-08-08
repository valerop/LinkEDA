#ifndef RLISPSTAT_CORE_BARPLOT_MODEL_H
#define RLISPSTAT_CORE_BARPLOT_MODEL_H

#include "dataset_model.h"
#include "plot_geometry.h"

#include <cstddef>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace rlispstat {
namespace core {

enum class BarplotWidthMode {
    Equal,
    Proportional
};

struct BarplotLayout {
    Rect plotRect;
    std::vector<double> values;
    std::vector<double> widthValues;
    std::vector<int> sharedPrefixDepthWithPrevious;
    std::size_t nestingDepth = 1;
    double yMaximum = 1.0;
    BarplotWidthMode widthMode = BarplotWidthMode::Equal;
};

struct BarplotSegmentVisualStyle {
    std::string colorKey;
    double alpha = 0.70;
    std::string pattern = "none";
    bool hasSegmentColorOverride = false;
    bool hasSegmentAlphaOverride = false;
    bool hasSegmentPatternOverride = false;
};

struct BarplotVisualState {
    double defaultSegmentAlpha = 0.70;
    std::string segmentEncodingMode = "transparent_color_pattern";
    double splitStrokeWidth = 3.0;
    std::map<std::string, std::string> levelColors;
    std::map<std::string, double> levelAlpha;
    std::map<std::string, std::string> levelPatterns;
    std::map<std::string, std::string> segmentColorOverrides;
    std::map<std::string, double> segmentAlphaOverrides;
    std::map<std::string, std::string> segmentPatternOverrides;
};

struct BarplotInputRow {
    int row = 0;
    std::vector<std::string> xValues;
    std::string splitLevel = "All";
};

struct BarplotSegmentSummary {
    std::string level;
    std::vector<int> rows;
    int count = 0;
    int barN = 0;
    int totalN = 0;
    double conditionalPercent = 0.0;
    double overallPercent = 0.0;
    double barPercent = 0.0;
    double barWidthValue = 0.0;
};

struct BarplotSegmentGeometry {
    std::string level;
    int count = 0;
    Rect rect;
};

struct BarplotSegmentHit {
    std::size_t barIndex = 0;
    std::size_t segmentIndex = 0;
    std::string level;
    Rect rect;
};

struct BarplotSegmentDrawItem {
    std::size_t segmentIndex = 0;
    std::size_t levelIndex = 0;
    Rect rect;
    BarplotSegmentVisualStyle style;
    bool useRowColorIdentityLayer = false;
    bool drawBottomEdge = false;
    double conditionalPercent = 0.0;
    std::vector<int> rows;
};

struct BarplotAxisTick {
    double value = 0.0;
    std::string label;
};

struct BarplotMenuOption {
    std::string title;
    std::string value;
    std::string command;
    bool enabled = true;
    bool checked = false;
};

struct BarplotViewSize {
    double width = 0.0;
    double height = 0.0;
};

struct BarplotCategoryLabelDrawItem {
    std::size_t barIndex = 0;
    std::size_t variableIndex = 0;
    Rect rect;
    std::string label;
};

struct BarplotSideLabelDrawPlan {
    std::string splitTitle;
    Rect splitTitleRect;
    std::vector<BarplotCategoryLabelDrawItem> splitLabels;
    bool showRowStripLabel = false;
    std::string rowStripLabel;
    Rect rowStripLabelRect;
};

struct BarplotReplaceXMenuSection {
    std::string variable;
    std::vector<BarplotMenuOption> options;
};

struct BarplotXMenuState {
    std::string title = "X Axis";
    std::string addXVariableTitle = "Add X Variable";
    std::string replaceOneXVariableTitle = "Replace One X Variable";
    std::string removeXVariableTitle = "Remove X Variable";
    std::vector<BarplotMenuOption> setOptions;
    std::vector<BarplotMenuOption> addOptions;
    std::vector<BarplotReplaceXMenuSection> replaceSections;
    std::vector<BarplotMenuOption> removeOptions;
    std::string emptyTitle = "No available variables";
};

struct BarplotSplitMenuState {
    std::string title = "Split Bars By";
    BarplotMenuOption noneOption;
    std::vector<BarplotMenuOption> splitOptions;
    std::string emptyTitle = "No available variables";
};

struct BarplotDisplayMenuState {
    std::string title = "Bar Chart";
    std::string yAxisTitle = "Y Axis";
    std::string barWidthTitle = "Bar Width";
    std::string rowColorsTitle = "Row Colors";
    std::string segmentEncodingTitle = "Segment Encoding";
    std::string selectionDisplayTitle = "Selection Display";
    std::vector<BarplotMenuOption> modeOptions;
    std::vector<BarplotMenuOption> widthOptions;
    BarplotMenuOption conditionalPercent;
    std::vector<BarplotMenuOption> rowColorOptions;
    BarplotMenuOption splitStrokeWidth;
    bool showSegmentEncodingMenu = true;
    std::vector<BarplotMenuOption> segmentEncodingOptions;
    BarplotMenuOption showPatterns;
    BarplotMenuOption splitStyle;
    std::vector<BarplotMenuOption> selectionDisplayOptions;
    BarplotMenuOption analysis;
    BarplotMenuOption nestedAnalysis;
    std::vector<BarplotMenuOption> selectionOptions;
    std::vector<BarplotMenuOption> plotOptions;
};

struct BarplotCreationDialogState {
    std::string title = "Bar Chart";
    std::string informativeText = "Choose a variable and optionally split each bar by another variable. The bar chart will share row selection with existing plots in this dataset.";
    std::string createButtonTitle = "Create";
    std::string cancelButtonTitle = "Cancel";
    std::string optionalTitlePlaceholder = "Optional title";
    std::string noActiveDatasetStatus = "Open or import a dataset first.";
    std::string needsBackendDataStatus = "The active dataset needs a backend data payload.";
    std::string selectedVariableMissingStatus = "Could not find the selected variable.";
    std::string selectedSplitVariableMissingStatus = "Could not find the selected split variable.";
};

struct BarplotSplitStrokeWidthDialogState {
    std::string title = "Bar Border Width";
    std::string informativeText = "Enter border width in pixels (range 1.0 to 12.0).";
    std::string applyButtonTitle = "Apply";
    std::string cancelButtonTitle = "Cancel";
    std::string invalidNumberStatus = "Bar border width must be a numeric value.";
    std::string currentValueText;
};

struct BarplotSplitState {
    std::string splitVariable;
    std::string rowColorDisplay;
    bool showConditionalPercent = false;
    bool resetSplitVisualState = false;
    bool changed = false;
};

struct BarplotSegmentEncodingState {
    std::string segmentEncodingMode;
    bool showPatterns = true;
    bool changed = false;
};

struct BarplotSegmentReference {
    std::size_t barIndex = 0;
    std::size_t segmentIndex = 0;
    std::string category;
    BarplotSegmentSummary segment;
};

struct BarplotSegmentMenuState {
    std::string title = "Segment";
    std::string payload;
    std::string level;
    std::string levelColorTitle;
    std::string levelPatternTitle;
    BarplotMenuOption selectRows;
    BarplotMenuOption applyCurrentColorToRows;
    std::vector<BarplotMenuOption> segmentColorOptions;
    std::vector<BarplotMenuOption> levelColorOptions;
    std::vector<BarplotMenuOption> segmentOpacityOptions;
    std::vector<BarplotMenuOption> levelPatternOptions;
    std::vector<BarplotMenuOption> segmentPatternOptions;
    BarplotMenuOption resetSegmentColor;
    BarplotMenuOption resetLevelColor;
    BarplotMenuOption details;
};

struct BarplotCompositionSlice {
    Rect rect;
    std::string colorKey;
    int count = 0;
};

struct BarplotSelectionSlicePlan {
    // Full-height light-tone bands for the rows represented by the bar.  These
    // remain visible when no cases are selected, matching the data-sheet
    // selected/unselected colour convention.
    std::vector<BarplotCompositionSlice> backgroundSlices;
    std::vector<BarplotCompositionSlice> slices;
    std::string dominantSelectedColorKey = "__default__";
    int selected = 0;
    int total = 0;
    double selectedFraction = 0.0;
};

struct BarplotCategoryComponentHit {
    std::size_t barIndex = 0;
    std::size_t variableIndex = 0;
    std::string value;
    Rect labelRect;
    Rect componentRect;
};

struct BarplotBinSummary {
    std::string category;
    std::vector<int> rows;
    int n = 0;
    double percent = 0.0;
    double widthValue = 0.0;
    std::vector<BarplotSegmentSummary> segments;
};

struct BarplotLayoutInput {
    Rect viewRect;
    std::vector<std::string> xVariables;
    std::vector<BarplotBinSummary> bins;
    std::string mode = "count";
    std::string widthMode = "equal";
    bool hasSplit = false;
    bool showCompositionStrip = false;
};

struct BarplotBuildInput {
    std::vector<std::string> xVariables;
    std::vector<BarplotInputRow> rows;
    std::string widthMode = "equal";
};

struct BarplotBuildResult {
    std::vector<BarplotBinSummary> bins;
    int totalN = 0;
};

bool IsValidBarplotLayout(const BarplotLayout &layout);
std::vector<Rect> BarplotBarRects(const BarplotLayout &layout);
Rect BarplotBarRect(const BarplotLayout &layout, std::size_t index);
std::optional<std::size_t> BarplotBarIndexAtPoint(const BarplotLayout &layout,
                                                  const Point &point);
std::optional<std::pair<std::size_t, std::size_t>> BarplotBarRangeForGesture(
    const BarplotLayout &layout,
    const Point &start,
    const Point &current);
std::optional<Rect> BarplotDragHighlightRect(const BarplotLayout &layout,
                                             const Point &start,
                                             const Point &current);
std::set<int> BarplotRowsForBarRange(const std::vector<std::vector<int>> &rowsByBar,
                                     std::size_t first,
                                     std::size_t last);

Rect BarplotSegmentRect(const Rect &barRect,
                        int segmentCount,
                        int barCount,
                        double *cursor);
std::vector<Rect> BarplotSegmentRects(const Rect &barRect,
                                      const std::vector<int> &segmentCounts,
                                      int barCount);
std::vector<BarplotSegmentGeometry> BarplotSegmentGeometryForLevels(
    const Rect &barRect,
    const std::vector<std::string> &orderedLevels,
    const std::map<std::string, int> &countsByLevel,
    int barCount);
std::optional<BarplotSegmentGeometry> BarplotSegmentGeometryAtPoint(
    const Rect &barRect,
    const std::vector<std::string> &orderedLevels,
    const std::map<std::string, int> &countsByLevel,
    int barCount,
    const Point &point);
std::optional<BarplotSegmentHit> BarplotSegmentAtPoint(
    const BarplotLayout &layout,
    const std::vector<BarplotBinSummary> &bins,
    const std::vector<std::string> &orderedLevels,
    bool hasSplit,
    const Point &point);
std::vector<BarplotSegmentDrawItem> BuildBarplotSegmentDrawPlan(
    const Rect &barRect,
    const BarplotBinSummary &bin,
    const std::vector<std::string> &orderedLevels,
    const BarplotVisualState &visualState,
    const std::map<int, std::string> &rowColors);
std::vector<BarplotAxisTick> BarplotYAxisTicks(
    double yMaximum,
    const std::string &mode,
    int divisions = 4);
std::vector<BarplotMenuOption> BarplotModeMenuOptions();
std::vector<BarplotMenuOption> BarplotWidthMenuOptions();
std::vector<BarplotMenuOption> BarplotRowColorDisplayMenuOptions(bool hasSplit);
std::vector<BarplotMenuOption> BarplotSelectionDisplayMenuOptions();
std::vector<BarplotMenuOption> BarplotSegmentEncodingMenuOptions();
BarplotMenuOption BarplotConditionalPercentMenuOption(bool showConditionalPercent,
                                                      bool hasSplit);
BarplotMenuOption BarplotSplitStrokeWidthMenuOption(double splitStrokeWidth);
BarplotMenuOption BarplotSplitStyleMenuOption();
BarplotMenuOption BarplotShowPatternsMenuOption(bool showPatterns);
BarplotDisplayMenuState BuildBarplotDisplayMenuState(
    const std::string &mode,
    const std::string &widthMode,
    const std::string &splitVariable,
    const std::string &rowColorDisplay,
    bool showConditionalPercent,
    double splitStrokeWidth,
    const std::string &segmentEncodingMode,
    bool showPatterns,
    const std::string &selectionDisplay,
    const std::vector<std::string> &xVariables);
BarplotXMenuState BuildBarplotXMenuState(
    const std::vector<std::string> &availableVariables,
    const std::vector<std::string> &currentXVariables,
    const std::string &splitVariable);
BarplotSplitMenuState BuildBarplotSplitMenuState(
    const std::vector<std::string> &availableVariables,
    const std::vector<std::string> &currentXVariables,
    const std::string &splitVariable);
BarplotCreationDialogState BuildBarplotCreationDialogState();
std::string BarplotDefaultTitle(const std::string &xLabel,
                                const std::string &splitVariable);
BarplotSplitStrokeWidthDialogState BuildBarplotSplitStrokeWidthDialogState(
    double currentWidth);
std::string BarplotAnalysisMenuTitle(const std::vector<std::string> &xVariables,
                                     const std::string &splitVariable);
std::vector<std::string> BarplotXVariablesAfterSet(const std::string &variable);
std::vector<std::string> BarplotXVariablesAfterAdd(
    const std::vector<std::string> &currentXVariables,
    const std::string &fallbackXLabel,
    const std::string &variable);
std::vector<std::string> BarplotXVariablesAfterReplace(
    const std::vector<std::string> &currentXVariables,
    const std::string &fallbackXLabel,
    const std::string &oldVariable,
    const std::string &newVariable);
std::vector<std::string> BarplotXVariablesAfterRemove(
    const std::vector<std::string> &currentXVariables,
    const std::string &fallbackXLabel,
    const std::string &variable);
BarplotSplitState BarplotStateAfterSplitBy(
    const std::vector<std::string> &currentXVariables,
    const std::string &fallbackXLabel,
    const std::string &currentSplitVariable,
    const std::string &rowColorDisplay,
    bool showConditionalPercent,
    const std::string &variable);
BarplotSplitState BarplotStateAfterClearSplit(
    const std::string &currentSplitVariable,
    const std::string &rowColorDisplay,
    bool showConditionalPercent);
bool BarplotConditionalPercentForMode(const std::string &mode,
                                      bool hasSplit);
std::string BarplotRowColorDisplayAfterSet(const std::string &requestedMode,
                                           bool hasSplit,
                                           const std::string &currentMode);
std::string BarplotSelectionDisplayAfterSet(const std::string &requestedMode,
                                            const std::string &currentMode);
std::string BarplotModeAfterSet(const std::string &requestedMode,
                                const std::string &currentMode);
std::string BarplotWidthModeAfterSet(const std::string &requestedMode,
                                     const std::string &currentMode);
BarplotSegmentEncodingState BarplotStateAfterSegmentEncoding(
    const std::string &currentMode,
    bool currentShowPatterns,
    const std::string &requestedMode);
bool BarplotShowPatternsAfterCommand(bool currentShowPatterns,
                                     const std::string &commandValue);
std::string BarplotSegmentCommandPayload(std::size_t barIndex,
                                         std::size_t segmentIndex);
std::string BarplotSegmentMenuTitle(const std::string &xLabel,
                                    const std::string &splitVariable,
                                    const BarplotSegmentReference &reference);
std::vector<double> BarplotSegmentOpacityMenuValues();
std::vector<std::string> BarplotPatternMenuValues();
BarplotSegmentMenuState BuildBarplotSegmentMenuState(
    const std::string &xLabel,
    const std::string &splitVariable,
    const BarplotSegmentReference &reference,
    const BarplotSegmentVisualStyle &style,
    const std::vector<std::string> &paletteNames,
    const std::string &levelColor,
    const std::string &levelPattern);
std::optional<BarplotSegmentReference> BarplotSegmentReferenceForIndices(
    const std::vector<BarplotBinSummary> &bins,
    std::size_t barIndex,
    std::size_t segmentIndex);
std::vector<int> BarplotRowsForSegmentIndices(
    const std::vector<BarplotBinSummary> &bins,
    std::size_t barIndex,
    std::size_t segmentIndex);
bool BarplotModeIsValid(const std::string &mode);
bool BarplotWidthModeIsValid(const std::string &widthMode);
bool BarplotRowColorDisplayIsValid(const std::string &mode);
bool BarplotRowColorDisplayAllowed(const std::string &mode,
                                   bool hasSplit);
bool BarplotSelectionDisplayModeIsValid(const std::string &mode);
BarplotViewSize BarplotPreferredViewSize(std::size_t categoryCount,
                                         bool hasSplit,
                                         std::size_t xVariableCount);
double BarplotCategoryLabelVerticalOffset(bool showCompositionStrip);
Rect BarplotPlotRect(const Rect &viewRect,
                     bool hasSplit,
                     bool showCompositionStrip,
                     std::size_t xVariableCount);
Rect BarplotXAxisLabelRect(const Rect &viewRect,
                           const Rect &plotRect);
int BarplotMaxBarCount(const std::vector<BarplotBinSummary> &bins);
double BarplotValueForBin(const BarplotBinSummary &bin,
                          const std::string &mode);
double BarplotWidthValueForBin(const std::string &widthMode,
                               int count,
                               double percent);
double BarplotYMaximum(const std::vector<BarplotBinSummary> &bins,
                       const std::string &mode);
BarplotLayout BuildBarplotLayout(const BarplotLayoutInput &input);
BarplotLayout BuildBarplotLayoutForPlotRect(const Rect &plotRect,
                                            const std::vector<std::string> &xVariables,
                                            const std::vector<BarplotBinSummary> &bins,
                                            const std::string &mode,
                                            const std::string &widthMode);
std::vector<int> BarplotRowsAtPoint(
    const BarplotLayout &layout,
    const std::vector<BarplotBinSummary> &bins,
    const std::vector<std::string> &orderedLevels,
    bool hasSplit,
    const Point &point);
std::set<int> BarplotRowsForGesture(
    const BarplotLayout &layout,
    const std::vector<BarplotBinSummary> &bins,
    const std::vector<std::string> &orderedLevels,
    bool hasSplit,
    const Point &start,
    const Point &current,
    double minimumWidth = 3.0,
    double minimumHeight = 3.0);

Rect BarplotCategoryLabelRect(const Rect &plotRect,
                              const Rect &barRect,
                              std::size_t componentCount,
                              double verticalOffset);
Rect BarplotCategoryComponentRect(const Rect &labelRect,
                                  std::size_t componentCount,
                                  std::size_t componentIndex);
std::optional<BarplotCategoryComponentHit> BarplotCategoryComponentAtPoint(
    const BarplotLayout &layout,
    const std::vector<BarplotBinSummary> &bins,
    const std::vector<std::string> &xVariables,
    const Point &point,
    double verticalOffset,
    double labelHitPaddingX = 2.0,
    double labelHitPaddingY = 2.0,
    double componentHitPaddingX = 2.0,
    double componentHitPaddingY = 1.0);
std::vector<int> BarplotRowsForXVariableValue(
    const std::vector<BarplotBinSummary> &bins,
    const std::vector<std::string> &xVariables,
    std::size_t variableIndex,
    const std::string &value);
std::vector<BarplotCategoryLabelDrawItem> BuildBarplotCategoryLabelDrawPlan(
    const BarplotLayout &layout,
    const std::vector<BarplotBinSummary> &bins,
    const std::vector<std::string> &xVariables,
    double verticalOffset);
BarplotSideLabelDrawPlan BuildBarplotSideLabelDrawPlan(
    const Rect &plotRect,
    const std::string &splitVariable,
    const std::vector<std::string> &levels,
    const std::map<std::string, int> &totals,
    bool showCompositionStrip);

Rect BarplotSplitLevelLabelRect(const Rect &plotRect,
                                std::size_t index,
                                const std::vector<int> &totals,
                                double left = 8.0,
                                double rightPadding = 38.0,
                                double height = 18.0);
std::vector<std::string> BarplotSplitLevelsForBins(
    const std::vector<BarplotBinSummary> &bins,
    bool hasSplit);
std::size_t BarplotLevelIndex(const std::vector<std::string> &levels,
                              const std::string &level,
                              std::size_t fallbackIndex = 0);
std::map<std::string, int> BarplotSplitLevelTotalsForBins(
    const std::vector<BarplotBinSummary> &bins,
    const std::vector<std::string> &levels);
std::optional<std::size_t> BarplotSplitLevelLabelIndexAtPoint(
    const Rect &plotRect,
    const std::vector<std::string> &levels,
    const std::map<std::string, int> &totalsByLevel,
    const Point &point,
    double labelHitPaddingX = 3.0,
    double labelHitPaddingY = 2.0);
std::vector<int> BarplotRowsForSplitLevel(
    const std::vector<BarplotBinSummary> &bins,
    const std::string &level);
std::string BarplotCategoryTooltipText(
    const std::string &variableName,
    const std::string &value,
    const std::vector<int> &rows,
    const std::set<int> &selection);
std::string BarplotSplitLevelTooltipText(
    const std::string &splitVariable,
    const std::string &level,
    const std::vector<int> &rows,
    const std::set<int> &selection);
std::string BarplotSegmentTooltipText(
    const std::string &xLabel,
    const std::string &splitVariable,
    const std::string &widthMode,
    const BarplotBinSummary &bin,
    int totalN,
    const std::optional<BarplotSegmentSummary> &segment,
    const std::optional<BarplotSegmentVisualStyle> &visualStyle,
    const std::map<int, std::string> &rowColors,
    const std::set<int> &selection,
    const std::vector<std::string> &paletteOrder,
    bool showRowColors);

std::string BarplotPatternAtIndex(std::size_t index);
bool BarplotPatternIsKnown(const std::string &pattern);
std::string BarplotPatternDisplayName(const std::string &pattern);
std::string BarplotSegmentOverrideKey(const std::string &xCondition,
                                      const std::string &level);
double ClampUnitInterval(double value);
double ClampBarplotSplitStrokeWidth(double value);
std::optional<double> ParseBarplotSplitStrokeWidthCommandValue(const std::string &text);
std::optional<double> ParseBarplotSegmentAlphaCommandValue(const std::string &text);
bool BarplotSegmentEncodingModeIsValid(const std::string &encodingMode);
std::string NormalizedBarplotSegmentEncodingMode(const std::string &encodingMode);
bool BarplotEncodingShowsPatterns(bool showPatterns,
                                  const std::string &encodingMode);
bool BarplotEncodingShowsColor(const std::string &encodingMode);
bool BarplotSelectionDisplayShowsOverlay(const std::string &selectionDisplay);
bool BarplotSelectionDisplayShowsOutline(const std::string &selectionDisplay);
bool BarplotLevelIsMissing(const std::string &level);
bool ParseBarplotSegmentCommandPayload(const std::string &payload,
                                       int *barIndex,
                                       int *segmentIndex,
                                       std::string *tail = nullptr);
std::vector<std::string> OrderedUniqueBarplotLevels(const std::vector<std::string> &levels);
std::vector<std::string> CanonicalLevelOrder(const std::vector<std::string> &values);
std::set<std::string> BarplotSegmentOverrideKeys(
    const std::vector<std::pair<std::string, std::string>> &segments);
std::vector<std::string> SplitBarplotXLabel(const std::string &label);
std::vector<std::string> BarplotXVariablesForLabels(
    const std::vector<std::string> &barplotXVariables,
    const std::string &fallbackXLabel);
std::vector<std::string> NormalizedBarplotXVariables(
    const std::vector<std::string> &barplotXVariables,
    const std::string &fallbackXLabel);
std::vector<std::string> SplitBarplotConditionLabel(const std::string &label);
std::vector<std::string> BarplotCategoryComponents(
    const std::vector<std::string> &barplotXVariables,
    const std::string &category);
std::string GeneratedBarplotTitle(const std::string &xLabel,
                                  const std::string &splitVariable);
bool BarplotTitleIsAutoGenerated(const std::string &title);
std::string RefreshedBarplotTitle(const std::string &currentTitle,
                                  const std::string &xLabel,
                                  const std::string &splitVariable);
std::string BarplotHeightScaleLabel(const std::string &mode);
std::string BarplotWidthScaleLabel(const std::string &widthMode);
std::string BarplotSegmentEncodingLabel(bool hasSplit,
                                        const std::string &segmentEncodingMode);
std::string BarplotSubtitle(const std::string &mode,
                            const std::string &widthMode,
                            const std::string &splitVariable,
                            double splitStrokeWidth,
                            const std::string &segmentEncodingMode);
int BarplotSharedCategoryPrefixDepth(
    const std::vector<std::string> &barplotXVariables,
    const std::string &left,
    const std::string &right);
std::vector<std::size_t> BarplotCategorySortOrder(
    const std::vector<std::string> &barplotXVariables,
    const std::vector<std::string> &categories);
std::string BarplotCategoryLabelForValues(
    const std::vector<std::string> &barplotXVariables,
    const std::vector<std::string> &values);
BarplotBuildResult BuildBarplotBins(const BarplotBuildInput &input);
BarplotBuildResult BuildBarplotBinsForColumns(
    const std::vector<const DataColumn *> &xColumns,
    const DataColumn *splitColumn,
    const std::string &widthMode);
std::vector<std::string> BarplotAvailableVariables(const DataFrameModel &df);
void ApplyBarplotBuildResultToModel(PlotModel &model,
                                    const BarplotBuildResult &result);
void RebuildBarplotBinsForColumns(PlotModel &model,
                                  const std::vector<const DataColumn *> &xColumns,
                                  const DataColumn *splitColumn);
bool RebuildBarplotFromDataFrame(PlotModel &model,
                                 const DataFrameModel &df,
                                 std::string *message = nullptr);
bool RefreshBarplotFromDataFrame(PlotModel &model,
                                 const DataFrameModel &df);

std::vector<std::string> BarplotPaletteOrder();
void NormalizeBarplotVisualState(BarplotVisualState &state,
                                 const std::vector<std::string> &levels,
                                 const std::set<std::string> &validSegmentKeys);
std::set<std::string> BarplotSegmentOverrideKeysForBins(
    const std::vector<BarplotBinSummary> &bins);
void NormalizeBarplotVisualStateForBins(BarplotVisualState &state,
                                        const std::vector<BarplotBinSummary> &bins);
void ClearBarplotSplitVisualState(BarplotVisualState &state,
                                  bool includeLevelState);
bool SetBarplotSegmentColorOverride(BarplotVisualState &state,
                                    const BarplotSegmentReference &reference,
                                    const std::string &colorName);
bool SetBarplotLevelColorOverride(BarplotVisualState &state,
                                  const std::string &level,
                                  const std::string &colorName);
bool SetBarplotSegmentAlphaOverride(BarplotVisualState &state,
                                    const BarplotSegmentReference &reference,
                                    double alpha);
bool SetBarplotLevelPatternOverride(BarplotVisualState &state,
                                    const std::string &level,
                                    const std::string &pattern);
bool SetBarplotSegmentPatternOverride(BarplotVisualState &state,
                                      const BarplotSegmentReference &reference,
                                      const std::string &pattern);
bool ResetBarplotSegmentColorOverride(BarplotVisualState &state,
                                      const BarplotSegmentReference &reference);
bool ResetBarplotLevelColorOverride(BarplotVisualState &state,
                                    const std::string &level);
BarplotSegmentVisualStyle ResolveBarplotSegmentVisual(
    double defaultSegmentAlpha,
    const std::map<std::string, std::string> &levelColors,
    const std::map<std::string, double> &levelAlpha,
    const std::map<std::string, std::string> &levelPatterns,
    const std::map<std::string, std::string> &segmentColorOverrides,
    const std::map<std::string, double> &segmentAlphaOverrides,
    const std::map<std::string, std::string> &segmentPatternOverrides,
    const std::string &xCondition,
    const std::string &level,
    std::size_t levelIndex);

std::string RowColorKeyForRow(const std::map<int, std::string> &rowColors,
                              int row,
                              const std::string &defaultKey = "__default__");
std::string RowColorDisplayName(const std::string &name,
                                const std::string &defaultKey = "__default__");
bool ColorKeyIsDefaultNeutral(const std::string &colorKey);
std::map<std::string, int> RowColorCompositionForRows(
    const std::vector<int> &rows,
    const std::map<int, std::string> &rowColors,
    const std::string &defaultKey = "__default__");
std::string DominantManualRowColorForRows(const std::vector<int> &rows,
                                          const std::map<int, std::string> &rowColors,
                                          const std::string &defaultKey = "__default__");
std::vector<std::pair<std::string, int>> OrderedRowColorComposition(
    const std::map<std::string, int> &composition,
    const std::vector<std::string> &paletteOrder,
    const std::string &defaultKey = "__default__");
std::vector<BarplotCompositionSlice> BarplotVerticalRowColorCompositionSlices(
    const Rect &geometry,
    const std::vector<int> &rows,
    const std::map<int, std::string> &rowColors,
    const std::vector<std::string> &paletteOrder,
    bool includeDefault = true,
    bool requireManualColor = false,
    const std::string &defaultKey = "__default__");
std::vector<BarplotCompositionSlice> BarplotHorizontalRowColorCompositionSlices(
    const Rect &geometry,
    const std::vector<int> &rows,
    const std::map<int, std::string> &rowColors,
    const std::vector<std::string> &paletteOrder,
    bool includeDefault = true,
    bool requireManualColor = false,
    const std::string &defaultKey = "__default__");
BarplotSelectionSlicePlan BuildBarplotSelectionSlicePlan(
    const Rect &geometry,
    const std::vector<int> &rows,
    const std::set<int> &selection,
    const std::map<int, std::string> &rowColors,
    const std::vector<std::string> &paletteOrder,
    const std::string &fallbackSelectedColorKey = "black",
    const std::string &defaultKey = "__default__");
int CountSelectedRowsForRows(const std::vector<int> &rows,
                             const std::set<int> &selection);
double SelectedFractionForRows(const std::vector<int> &rows,
                               int selected);
std::string BarplotNotAvailableStatus();
std::string BarplotChooseXVariableStatus();
std::string BarplotNoValuesToPlotStatus();
std::string BarplotWindowTitle();
std::string BarplotSegmentMenuTitle();
std::string BarplotSetSegmentColorMenuItemTitle();
std::string BarplotSetSegmentOpacityMenuItemTitle();
std::string BarplotSetPatternForThisSegmentMenuItemTitle();

BarplotSegmentSummary ToBarplotSegmentSummary(const BarplotSegment &segment);
BarplotBinSummary ToBarplotBinSummary(const BarplotBin &bin);
std::vector<BarplotBinSummary> BarplotBinsForModel(const PlotModel &model);
inline std::vector<BarplotBinSummary> BarplotBinsForModel(const PlotModel *model) {
    return model ? BarplotBinsForModel(*model) : std::vector<BarplotBinSummary>();
}

std::vector<std::string> BarplotXVariablesForModel(const PlotModel &model);
inline std::vector<std::string> BarplotXVariablesForModel(const PlotModel *model) {
    return model ? BarplotXVariablesForModel(*model) : std::vector<std::string>();
}
void NormalizeBarplotXVariables(PlotModel &model);
inline void NormalizeBarplotXVariables(PlotModel *model) {
    if (model) NormalizeBarplotXVariables(*model);
}
void SortBarplotBinsByXHierarchy(PlotModel &model);
inline void SortBarplotBinsByXHierarchy(PlotModel *model) {
    if (model) SortBarplotBinsByXHierarchy(*model);
}
void RefreshGeneratedBarplotTitle(PlotModel &model);
inline void RefreshGeneratedBarplotTitle(PlotModel *model) {
    if (model) RefreshGeneratedBarplotTitle(*model);
}

std::vector<std::string> BarplotSplitLevels(const PlotModel &model);
inline std::vector<std::string> BarplotSplitLevels(const PlotModel *model) {
    return model ? BarplotSplitLevels(*model) : std::vector<std::string>();
}
BarplotVisualState BarplotVisualStateForModel(const PlotModel &model);
inline BarplotVisualState BarplotVisualStateForModel(const PlotModel *model) {
    return model ? BarplotVisualStateForModel(*model) : BarplotVisualState();
}
void ApplyBarplotVisualStateToModel(PlotModel &model, const BarplotVisualState &state);
inline void ApplyBarplotVisualStateToModel(PlotModel *model, const BarplotVisualState &state) {
    if (model) ApplyBarplotVisualStateToModel(*model, state);
}
void NormalizeBarplotVisualState(PlotModel &model);
inline void NormalizeBarplotVisualState(PlotModel *model) {
    if (model) NormalizeBarplotVisualState(*model);
}
void ResetBarplotSplitVisualState(PlotModel &model, bool includeLevelState);
inline void ResetBarplotSplitVisualState(PlotModel *model, bool includeLevelState) {
    if (model) ResetBarplotSplitVisualState(*model, includeLevelState);
}
void ApplyNormalizedBarplotVisualState(PlotModel &model, BarplotVisualState state);
inline void ApplyNormalizedBarplotVisualState(PlotModel *model, BarplotVisualState state) {
    if (model) ApplyNormalizedBarplotVisualState(*model, state);
}
BarplotSegmentVisualStyle ResolveBarplotSegmentVisual(
    const PlotModel &model,
    const std::string &xCondition,
    const std::string &level,
    std::size_t levelIndex);
inline BarplotSegmentVisualStyle ResolveBarplotSegmentVisual(
    const PlotModel *model,
    const std::string &xCondition,
    const std::string &level,
    std::size_t levelIndex) {
    return model ? ResolveBarplotSegmentVisual(*model, xCondition, level, levelIndex)
                 : ResolveBarplotSegmentVisual(0.70, {}, {}, {}, {}, {}, {}, xCondition, level, levelIndex);
}

bool BarplotEncodingShowsPatterns(const PlotModel &model);
inline bool BarplotEncodingShowsPatterns(const PlotModel *model) {
    return !model || BarplotEncodingShowsPatterns(*model);
}
bool BarplotEncodingShowsColor(const PlotModel &model);
inline bool BarplotEncodingShowsColor(const PlotModel *model) {
    return !model || BarplotEncodingShowsColor(*model);
}
std::optional<BarplotSegmentReference> BarplotCoreSegmentReferenceForIndices(
    const PlotModel &model, int barIndex, int segmentIndex);
inline std::optional<BarplotSegmentReference> BarplotCoreSegmentReferenceForIndices(
    const PlotModel *model, int barIndex, int segmentIndex) {
    return model ? BarplotCoreSegmentReferenceForIndices(*model, barIndex, segmentIndex) : std::nullopt;
}

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_BARPLOT_MODEL_H
