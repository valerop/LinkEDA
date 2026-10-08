#ifndef RLISPSTAT_CORE_HISTOGRAM_MODEL_H
#define RLISPSTAT_CORE_HISTOGRAM_MODEL_H

#include "plot_geometry.h"
#include "selection_model.h"

#include <cstddef>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace rlispstat {
namespace core {

struct HistogramLayout {
    Rect plotRect;
    std::vector<int> counts;
    int maxCount = 1;
    double gap = 2.0;
    std::vector<double> values;
    double maxValue = 1.0;
};

struct HistogramCaseValue {
    double x = 0.0;
    CaseId row = 0;
};

struct HistogramBinData {
    double lower = 0.0;
    double upper = 0.0;
    std::vector<CaseId> rows;
};

struct HistogramBinningResult {
    std::vector<HistogramBinData> bins;
    std::vector<int> pointBins;
    double minimum = 0.0;
    double maximum = 0.0;
};

struct HistogramDensityCurve {
    std::string kind;
    std::string colorName;
    std::string label;
    std::vector<double> x;
    std::vector<double> y;
    int n = 0;
};

struct HistogramRugCase {
    CaseId caseId = 0;
    std::size_t binIndex = 0;
};

struct HistogramRugRenderItem {
    CaseId caseId = 0;
    Point start;
    Point end;
};

struct HistogramDensityRenderItem {
    std::string kind;
    std::string colorName;
    std::string label;
    int n = 0;
    std::vector<Point> linePoints;
    std::vector<Point> fillPolygon;
};

struct HistogramDensityOverlapRenderItem {
    std::string firstColorName;
    std::string secondColorName;
    std::vector<Point> fillPolygon;
};

struct HistogramDensityRenderPlan {
    double xMinimum = 0.0;
    double xMaximum = 1.0;
    double yMaximum = 1.0;
    std::vector<HistogramDensityRenderItem> curves;
    std::vector<HistogramDensityOverlapRenderItem> overlaps;
};

struct HistogramBarSegment {
    Rect rect;
    std::string colorName;
    double alpha = 0.0;
    bool defaultSelection = false;
};

struct HistogramBarRenderItem {
    Rect rect;
    int count = 0;
    bool showCount = false;
    std::string countLabel;
    std::vector<HistogramBarSegment> colorSegments;
    std::vector<HistogramBarSegment> selectedSegments;
};

struct HistogramRenderInput {
    HistogramLayout layout;
    std::vector<std::vector<CaseId>> binRows;
    std::set<CaseId> selection;
    std::map<CaseId, std::string> rowColors;
    bool showCounts = false;
    bool showColorSegments = true;
    bool showRug = false;
    std::vector<HistogramRugCase> rugCases;
    std::vector<HistogramDensityCurve> densityCurves;
    double densityXMinimum = 0.0;
    double densityXMaximum = 1.0;
    double densityMaximumHeightFraction = 0.45;
};

struct HistogramRenderPlan {
    std::vector<HistogramBarRenderItem> bars;
    std::vector<HistogramRugRenderItem> rugs;
    HistogramDensityRenderPlan density;
};

struct HistogramMenuOption {
    std::string title;
    std::string value;
    std::string command;
    bool enabled = true;
    bool checked = false;
};

std::vector<HistogramMenuOption> HistogramBinningRuleMenuOptions();
std::vector<HistogramMenuOption> HistogramBinCountMenuOptions(std::size_t currentCount);
// Existing rule engine, applied to the observations in the displayed scope.
std::optional<int> HistogramBinCountForChoice(const PlotModel &plot, const std::string &choice);

struct HistogramMenuState {
    std::string title = "Histogram";
    std::string densityCurvesTitle = "Density Curves";
    std::string densityModeTitle = "Density Mode";
    HistogramMenuOption counts;
    HistogramMenuOption tickMarks;
    HistogramMenuOption tickLabels;
    HistogramMenuOption rug;
    HistogramMenuOption densityToggle;
    std::vector<HistogramMenuOption> densityModes;
    HistogramMenuOption densityBandwidth;
    HistogramMenuOption densityAdjust;
    HistogramMenuOption frequencyTable;
    HistogramMenuOption descriptives;
    std::vector<HistogramMenuOption> selectionOptions;
    std::vector<HistogramMenuOption> plotOptions;
};

struct HistogramDensityState {
    bool showDensity = false;
    std::string densityMode = "all";
    bool changed = false;
};

struct HistogramCreationDialogState {
    std::string title = "Histogram";
    std::string informativeText = "Choose a numeric variable. The histogram will share row selection with existing plots in this dataset.";
    std::string createButtonTitle = "Create";
    std::string cancelButtonTitle = "Cancel";
    std::string binsPlaceholder = "auto";
    std::string optionalTitlePlaceholder = "Optional title";
    std::string noActiveDatasetStatus = "Open a LinkEDA plot first, or use ls_new_histogram() from R.";
    std::string needsNumericVariableStatus = "The active dataset needs at least one numeric variable.";
    std::string selectedXMissingStatus = "Could not find the selected X variable.";
};

struct HistogramDensityParameterDialogState {
    std::string title;
    std::string informativeText;
    std::string setButtonTitle = "Set";
    std::string cancelButtonTitle = "Cancel";
    std::string currentValueText;
};

bool IsValidHistogramLayout(const HistogramLayout &layout);
std::vector<Rect> HistogramBinRects(const HistogramLayout &layout);
Rect HistogramBinRect(const HistogramLayout &layout, std::size_t index);
std::optional<std::size_t> HistogramBinIndexAtPoint(const HistogramLayout &layout,
                                                    const Point &point);
std::optional<std::pair<std::size_t, std::size_t>> HistogramBinRangeForGesture(
    const HistogramLayout &layout,
    const Point &start,
    const Point &current);
std::optional<Rect> HistogramDragHighlightRect(
    const HistogramLayout &layout,
    const Point &start,
    const Point &current);
std::set<CaseId> SelectHistogramCasesForGesture(
    const std::vector<std::vector<CaseId>> &binRows,
    const HistogramLayout &layout,
    const Point &start,
    const Point &current);
int HistogramMaximumBinCount(const std::vector<std::vector<CaseId>> &binRows);

double QuantileSorted(const std::vector<double> &sortedValues, double probability);
int HistogramBinCountForRule(const std::vector<double> &values, const std::string &rule);
bool RebinHistogramCases(const std::vector<HistogramCaseValue> &cases,
                         int binCount,
                         HistogramBinningResult &result);
double HistogramAverageBinWidth(const std::vector<HistogramBinData> &bins);
std::vector<HistogramBarRenderItem> BuildHistogramBarRenderPlan(
    const HistogramLayout &layout,
    const std::vector<std::vector<CaseId>> &binRows,
    const std::set<CaseId> &selection,
    const std::map<CaseId, std::string> &rowColors,
    bool showCounts,
    bool showColorSegments);
std::vector<HistogramRugRenderItem> BuildHistogramRugRenderPlan(
    const HistogramLayout &layout,
    const std::vector<HistogramRugCase> &cases,
    double rugHeight = 7.0);
HistogramDensityRenderPlan BuildHistogramDensityRenderPlan(
    const Rect &plotRect,
    const std::vector<HistogramDensityCurve> &curves,
    double xMinimum,
    double xMaximum,
    double maximumHeightFraction = 0.45);
HistogramRenderPlan BuildHistogramRenderPlan(const HistogramRenderInput &input);
bool HistogramDensityModeIsValid(const std::string &mode);
bool HistogramDensityDisplayActive(bool showDensity,
                                   const std::string &densityMode);
bool HistogramColorSegmentsVisible(bool showDensity,
                                   const std::string &densityMode);
std::vector<HistogramMenuOption> HistogramDensityModeMenuOptions();
HistogramMenuState BuildHistogramMenuState(const std::string &xLabel,
                                           bool showCounts,
                                           bool showTickMarks,
                                           bool showTickLabels,
                                           bool showRug,
                                           bool showDensity,
                                           const std::string &densityMode);
HistogramCreationDialogState BuildHistogramCreationDialogState();
std::string HistogramDefaultTitle(const std::string &xVariable);
HistogramDensityParameterDialogState BuildHistogramDensityBandwidthDialogState(
    double currentBandwidth);
HistogramDensityParameterDialogState BuildHistogramDensityAdjustDialogState(
    double currentAdjust);
HistogramDensityState HistogramStateAfterToggleDensity(bool currentShowDensity,
                                                       const std::string &currentDensityMode);
HistogramDensityState HistogramStateAfterSetDensityMode(bool currentShowDensity,
                                                        const std::string &currentDensityMode,
                                                        const std::string &requestedDensityMode);
std::optional<double> ParseHistogramDensityBandwidthCommandValue(const std::string &text);
std::optional<double> ParseHistogramDensityAdjustCommandValue(const std::string &text);
HistogramDensityCurve DensityCurveForValues(const std::vector<double> &values,
                                            const std::string &kind,
                                            const std::string &colorName,
                                            const std::string &label,
                                            double xmin,
                                            double xmax,
                                            double bandwidth,
                                            double adjust);
std::vector<HistogramDensityCurve> DensityCurvesForHistogramCases(
    const std::vector<HistogramCaseValue> &cases,
    double xmin,
    double xmax,
    bool showDensity,
    const std::string &mode,
    double bandwidth,
    double adjust,
    const std::set<CaseId> &selection,
    const std::map<CaseId, std::string> &rowColors);

std::string HistogramNoFiniteValuesStatus();
std::string HistogramXFieldLabel();
std::string HistogramBinsFieldLabel();
std::string HistogramWindowTitle();

void RebinHistogram(PlotModel &model, int binCount);
inline void RebinHistogram(PlotModel *model, int binCount) {
    if (model) RebinHistogram(*model, binCount);
}
void RebinHistogramByRule(PlotModel &model, const std::string &rule);
inline void RebinHistogramByRule(PlotModel *model, const std::string &rule) {
    if (model) RebinHistogramByRule(*model, rule);
}
void RebuildHistogramPointsFromCurrentVariable(PlotModel &model);
inline void RebuildHistogramPointsFromCurrentVariable(PlotModel *model) {
    if (model) RebuildHistogramPointsFromCurrentVariable(*model);
}
double HistogramAverageBinWidth(const PlotModel &model);
inline double HistogramAverageBinWidth(const PlotModel *model) {
    return model ? HistogramAverageBinWidth(*model) : 0.0;
}
std::string HistogramBreaksResponseText(const PlotModel &model);
std::string HistogramDensityInfoResponseText(const PlotModel &model);
std::vector<HistogramDensityCurve> DensityCurvesForHistogram(
    PlotModel &model,
    const std::set<CaseId> &selection,
    const std::map<CaseId, std::string> &rowColors);
inline std::vector<HistogramDensityCurve> DensityCurvesForHistogram(
    PlotModel *model,
    const std::set<CaseId> &selection,
    const std::map<CaseId, std::string> &rowColors) {
    return model ? DensityCurvesForHistogram(*model, selection, rowColors) : std::vector<HistogramDensityCurve>();
}

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_HISTOGRAM_MODEL_H
