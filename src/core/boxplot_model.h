#ifndef RLISPSTAT_CORE_BOXPLOT_MODEL_H
#define RLISPSTAT_CORE_BOXPLOT_MODEL_H

#include "plot_geometry.h"
#include "selection_model.h"

#include <optional>
#include <map>
#include <limits>
#include <set>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

struct BoxplotCase {
    CaseId caseId = 0;
    double value = 0.0;
    std::string category;
};

struct BoxplotLayout {
    Rect plotRect;
    std::vector<std::string> categories;
    double yMinimum = 0.0;
    double yMaximum = 1.0;
    bool variableAxes = false;
    bool connectRows = false;
    double yPaddingFraction = 0.08;
    double jitterWidth = 34.0;
    // One outer-to-inner level path per category. When present, category
    // centres use progressively larger gaps at outer group boundaries.
    std::vector<std::vector<std::string>> categoryLevels;
};

struct BoxplotGroupSpan {
    std::size_t level = 0;
    std::string label;
    // Inclusive leaf-category range covered by this branch.  Keeping it in
    // the geometry model lets every UI make hierarchical labels selectable.
    std::size_t firstCategoryIndex = 0;
    std::size_t lastCategoryIndex = 0;
    double startX = 0.0;
    double endX = 0.0;
    double centerX = 0.0;
};

struct BoxplotDisplayRange {
    double minimum = 0.0;
    double maximum = 1.0;
    bool hasFiniteValue = false;
};

struct BoxplotAxisTick {
    double value = 0.0;
    std::string label;
};

struct BoxplotValueInterval {
    double lower = 0.0;
    double upper = 0.0;
};

struct BoxplotCaseGeometry {
    CaseId caseId = 0;
    Point point;
    std::string category;
    double value = 0.0;
};

struct BoxplotPointDrawItem {
    CaseId caseId = 0;
    Point point;
    bool selected = false;
    bool dimmed = false;
};

struct BoxplotConnectionLine {
    CaseId caseId = 0;
    std::vector<Point> points;
    bool selected = false;
    bool dimmed = false;
};

struct BoxplotStatsRenderItem {
    std::string category;
    double centerX = 0.0;
    double boxWidth = 0.0;
    Rect boxRect;
    Point medianStart;
    Point medianEnd;
    Point upperWhiskerStart;
    Point upperWhiskerEnd;
    Point lowerWhiskerStart;
    Point lowerWhiskerEnd;
    Point upperCapStart;
    Point upperCapEnd;
    Point lowerCapStart;
    Point lowerCapEnd;
};

struct BoxplotStats {
    std::string category;
    double q1 = 0.0;
    double median = 0.0;
    double q3 = 0.0;
    double lower = 0.0;
    double upper = 0.0;
    int n = 0;
};

struct BoxplotH0SimulationResult {
    int draws = 0;
    std::vector<double> simulatedValues;
    std::vector<double> referenceLines;
    double lower = std::numeric_limits<double>::quiet_NaN();
    double upper = std::numeric_limits<double>::quiet_NaN();
    double pValue = std::numeric_limits<double>::quiet_NaN();
    double effect = std::numeric_limits<double>::quiet_NaN();
    std::string label;
};

struct BoxplotVariableSeries {
    std::string name;
    std::vector<double> values;
};

struct ParallelBoxplotBuildInput {
    std::vector<BoxplotVariableSeries> variables;
    std::vector<std::string> selectedVariables;
    bool standardize = false;
};

struct ParallelBoxplotBuildResult {
    bool ok = false;
    std::string error;
    std::vector<BoxplotCase> cases;
    std::vector<std::string> categories;
};

struct BoxplotVariableUpdateResult {
    bool ok = false;
    bool changed = false;
    std::string error;
    std::vector<std::string> variables;
};

struct ParallelBoxplotLabelState {
    std::string yLabel;
    std::string title;
};

struct BoxplotMenuOption {
    std::string title;
    std::string value;
    std::string command;
    bool enabled = true;
    bool checked = false;
};

struct BoxplotMenuState {
    std::string title = "Boxplot";
    BoxplotMenuOption togglePoints;
    BoxplotMenuOption toggleBox;
    BoxplotMenuOption toggleWhiskers;
    BoxplotMenuOption toggleViolin;
    BoxplotMenuOption configureSplitViolin;
    bool showClearSplitViolin = false;
    BoxplotMenuOption clearSplitViolin;
    BoxplotMenuOption configureH0;
    bool showClearH0 = false;
    BoxplotMenuOption clearH0;
    bool showVariableAxisOptions = false;
    std::string addVariableTitle;
    std::vector<BoxplotMenuOption> addVariableOptions;
    std::string noMoreNumericVariablesTitle;
    bool showRemoveVariableMenu = false;
    std::string removeVariableTitle;
    std::vector<BoxplotMenuOption> removeVariableOptions;
    BoxplotMenuOption connectRows;
    BoxplotMenuOption standardizeVariables;
    BoxplotMenuOption descriptives;
    std::vector<BoxplotMenuOption> selectionOptions;
    std::vector<BoxplotMenuOption> plotOptions;
};

struct BoxplotCreationDialogState {
    std::string title = "Boxplot";
    std::string informativeText = "Choose a numeric Y variable and an optional grouping variable from the active dataset.";
    std::string createButtonTitle = "Create";
    std::string cancelButtonTitle = "Cancel";
    std::string optionalTitlePlaceholder = "Optional title";
    std::string noActiveDatasetStatus = "Open a LinkEDA plot first, or use ls_new_boxplot() from R.";
    std::string needsNumericDataStatus = "The active dataset needs numeric variables and a backend data payload.";
    std::string selectedYMissingStatus = "Could not find the selected Y variable.";
};

struct ParallelCoordinatesDialogState {
    std::string title = "Parallel Coordinates";
    std::string informativeText = "Choose two or more numeric variables. Values are standardized and matching rows are connected.";
    std::string createButtonTitle = "Create";
    std::string cancelButtonTitle = "Cancel";
    std::string optionalTitlePlaceholder = "Optional title";
    std::string defaultTitle = "Parallel Coordinates";
    std::string hintText = "Click a variable name later to add, replace, or remove variables.";
    std::string noActiveDatasetStatus = "Open or activate a dataset first.";
    std::string needsNumericDataStatus = "The active dataset needs at least two numeric variables.";
    std::string selectAtLeastTwoStatus = "Select at least two numeric variables.";
};

bool IsValidBoxplotLayout(const BoxplotLayout &layout);
DataViewport BoxplotViewport(const BoxplotLayout &layout);
BoxplotDisplayRange BoxplotDisplayRangeForCases(
    const std::vector<BoxplotCase> &cases,
    bool includeH0Simulation,
    const std::vector<double> &h0Values,
    const std::vector<double> &h0ReferenceLines,
    double h0Lower,
    double h0Upper);
std::vector<BoxplotAxisTick> BoxplotYAxisTicks(double minimum,
                                               double maximum,
                                               int targetCount = 5);
std::vector<std::string> OrderedBoxplotCategories(
    const std::vector<std::string> &definedCategories,
    const std::vector<BoxplotCase> &cases,
    const std::string &order);
std::vector<BoxplotMenuOption> BoxplotGroupOrderMenuOptions(const std::string &currentOrder);
double BoxplotCategoryCenter(const BoxplotLayout &layout, const std::string &category);
std::string BoxplotInnermostCategoryLabel(const BoxplotLayout &layout,
                                          const std::string &category);
std::vector<BoxplotGroupSpan> BoxplotGroupSpans(const BoxplotLayout &layout);
std::set<std::string> BoxplotCategoriesForLevelValue(
    const BoxplotLayout &layout,
    std::size_t level,
    const std::string &value);
double BoxplotH0SimulationCenterX(const BoxplotLayout &layout);
std::vector<BoxplotValueInterval> BoxplotTailIntervalsForAlternative(
    double displayMinimum,
    double displayMaximum,
    double lowerCut,
    double upperCut,
    const std::string &alternative);
Point BoxplotCasePoint(const BoxplotLayout &layout, const BoxplotCase &boxplotCase);
std::vector<BoxplotCaseGeometry> BoxplotCaseGeometryForLayout(
    const BoxplotLayout &layout,
    const std::vector<BoxplotCase> &cases);
std::vector<BoxplotPointDrawItem> BuildBoxplotPointDrawPlan(
    const std::vector<BoxplotCaseGeometry> &cases,
    const std::set<CaseId> &selection);
std::vector<BoxplotConnectionLine> BuildBoxplotConnectionLinePlan(
    const std::vector<BoxplotCaseGeometry> &cases,
    const std::vector<std::string> &categories,
    const std::set<CaseId> &selection,
    bool variableAxes,
    bool connectRows);
std::vector<BoxplotStatsRenderItem> BuildBoxplotStatsRenderPlan(
    const BoxplotLayout &layout,
    const std::vector<BoxplotStats> &stats);

std::set<CaseId> SelectBoxplotCasesInBrush(const std::vector<BoxplotCaseGeometry> &cases,
                                           const Rect &brush,
                                           double pointPadding = 0.0);

std::optional<CaseId> NearestBoxplotCaseToPoint(const std::vector<BoxplotCaseGeometry> &cases,
                                                const Point &point,
                                                double maxDistance = 8.0);

std::set<CaseId> SelectBoxplotCasesForGesture(const std::vector<BoxplotCaseGeometry> &cases,
                                              const Rect &brush,
                                              const Point &clickPoint,
                                              bool dragBrush,
                                              double maxClickDistance = 8.0);

double BoxplotQuantileSorted(const std::vector<double> &sortedValues, double probability);
double MeanOfValues(const std::vector<double> &values);
double SampleSDOfValues(const std::vector<double> &values);
std::map<std::string, std::vector<double>> BoxplotValuesByCategory(const std::vector<BoxplotCase> &cases);
std::vector<std::string> BoxplotCategoriesForCases(const std::vector<BoxplotCase> &cases);
bool BoxplotVariableListContains(const std::vector<std::string> &variables,
                                 const std::string &name);
bool BoxplotUsesVariableAxes(const std::string &kind,
                             const std::string &xLabel);
std::string BoxplotAddVariableTitle();
std::string BoxplotRemoveVariableTitle();
std::string BoxplotNoMoreNumericVariablesTitle();
BoxplotCreationDialogState BuildBoxplotCreationDialogState();
std::string BoxplotDefaultTitle(const std::string &yVariable,
                                const std::string &groupVariable);
std::vector<std::string> BoxplotVariablesAvailableToAdd(
    const std::vector<std::string> &currentVariables,
    const std::vector<std::string> &availableVariables);
BoxplotVariableUpdateResult BoxplotVariablesAfterAdd(
    const std::vector<std::string> &currentVariables,
    const std::string &name,
    const std::vector<std::string> &availableVariables);
BoxplotVariableUpdateResult BoxplotVariablesAfterRemove(
    const std::vector<std::string> &currentVariables,
    const std::string &name,
    std::size_t minimumVariables = 1);
BoxplotVariableUpdateResult BoxplotVariablesAfterReplacement(
    const std::vector<std::string> &currentVariables,
    const std::string &oldName,
    const std::string &newName,
    const std::vector<std::string> &availableVariables);
double BoxplotConnectionStrokeWidth(double requestedWidth, bool selected);
ParallelCoordinatesDialogState BuildParallelCoordinatesDialogState();
ParallelBoxplotLabelState ParallelBoxplotLabelsForVariables(
    const std::vector<std::string> &variables,
    bool standardize,
    const std::string &currentTitle);
BoxplotMenuState BuildBoxplotMenuState(
    bool showPoints,
    bool showBox,
    bool showWhiskers,
    bool showViolin,
    bool splitViolin,
    bool showH0Simulation,
    bool variableAxes,
    bool connectRows,
    bool standardizeVariables,
    const std::vector<std::string> &currentVariables,
    const std::vector<std::string> &availableVariables);
ParallelBoxplotBuildResult BuildParallelBoxplotCases(const ParallelBoxplotBuildInput &input);
std::vector<BoxplotStats> BoxplotStatsForCases(const std::vector<BoxplotCase> &cases,
                                               const std::vector<std::string> &categories);
bool BuildBoxplotH0Simulation(const std::vector<BoxplotCase> &cases,
                              const std::vector<std::string> &categories,
                              bool variableAxes,
                              double h0,
                              const std::string &alternative,
                              int requestedDraws,
                              BoxplotH0SimulationResult &result,
                              std::string *error = nullptr);
std::string BoxplotSplitViolinInfoText();
std::string BoxplotH0SimulationInfoText();
std::string BoxplotXVariableFieldLabel();
std::string BoxplotAddXFieldLabel();
std::string BoxplotSplitByFieldLabel();
std::string BoxplotWindowTitle();
std::string BoxplotSegmentDetailsWindowTitle();
std::string BoxplotH0SimulationWindowTitle();
std::string BoxplotObservedLabel();
std::string BoxplotParallelLabel();
std::string BoxplotAlternativeFieldLabel();
std::string BoxplotLessOptionLabel();
std::string BoxplotGreaterOptionLabel();
std::string BoxplotTwoSidedOptionLabel();
std::string BoxplotThresholdLowerFieldLabel();
std::string BoxplotUpperFieldLabel();
std::string BoxplotH0FieldLabel();
std::string BoxplotDrawsFieldLabel();
std::string BoxplotSplitViolinDialogTitle();
std::string BoxplotPBracketLabel();
std::string BoxplotLeafLabel();

bool BoxplotUsesVariableAxes(const PlotModel &model);
inline bool BoxplotUsesVariableAxes(const PlotModel *model) {
    return model && BoxplotUsesVariableAxes(*model);
}
bool BoxplotHasVariable(const PlotModel &model, const std::string &name);
inline bool BoxplotHasVariable(const PlotModel *model, const std::string &name) {
    return model && BoxplotHasVariable(*model, name);
}
void UpdateParallelBoxplotLabels(PlotModel &model);
inline void UpdateParallelBoxplotLabels(PlotModel *model) {
    if (model) UpdateParallelBoxplotLabels(*model);
}
bool RebuildParallelBoxplotPoints(PlotModel &model, std::string *error = nullptr);
inline bool RebuildParallelBoxplotPoints(PlotModel *model, std::string *error = nullptr) {
    return model ? RebuildParallelBoxplotPoints(*model, error) : false;
}
void RebuildGroupedBoxplotPointsFromDataFrame(PlotModel &model,
                                              const DataFrameModel &df);
std::string BoxplotGroupingLabel(const std::vector<std::string> &variables);
std::string BoxplotNestedCategoryLabel(const std::vector<std::string> &variables,
                                       const std::vector<std::string> &levels);
std::vector<std::vector<std::string>> BoxplotCategoryLevelsForCategories(
    const PlotModel &model,
    const std::vector<std::string> &categories);
inline void RebuildGroupedBoxplotPointsFromDataFrame(PlotModel *model,
                                                     const DataFrameModel &df) {
    if (model) RebuildGroupedBoxplotPointsFromDataFrame(*model, df);
}

std::vector<BoxplotCase> BoxplotCasesForModel(const PlotModel &model);
inline std::vector<BoxplotCase> BoxplotCasesForModel(const PlotModel *model) {
    return model ? BoxplotCasesForModel(*model) : std::vector<BoxplotCase>();
}
std::vector<std::string> CategoriesForPlot(const PlotModel &model);
inline std::vector<std::string> CategoriesForPlot(const PlotModel *model) {
    return model ? CategoriesForPlot(*model) : std::vector<std::string>();
}
void ClearBoxplotH0Simulation(PlotModel &model);
inline void ClearBoxplotH0Simulation(PlotModel *model) {
    if (model) ClearBoxplotH0Simulation(*model);
}
std::vector<BoxplotStats> BoxplotStatsForModel(const PlotModel &model);
inline std::vector<BoxplotStats> BoxplotStatsForModel(const PlotModel *model) {
    return model ? BoxplotStatsForModel(*model) : std::vector<BoxplotStats>();
}
std::string BoxplotOptionsResponseText(const PlotModel &model);
void EnsureParallelBoxplotState(PlotModel &model);
inline void EnsureParallelBoxplotState(PlotModel *model) {
    if (model) EnsureParallelBoxplotState(*model);
}
bool AddParallelBoxplotVariable(PlotModel &model,
                                const std::string &name,
                                std::string *error = nullptr);
inline bool AddParallelBoxplotVariable(PlotModel *model,
                                       const std::string &name,
                                       std::string *error = nullptr) {
    return model ? AddParallelBoxplotVariable(*model, name, error) : false;
}
bool RemoveParallelBoxplotVariable(PlotModel &model,
                                   const std::string &name,
                                   std::string *error = nullptr);
inline bool RemoveParallelBoxplotVariable(PlotModel *model,
                                          const std::string &name,
                                          std::string *error = nullptr) {
    return model ? RemoveParallelBoxplotVariable(*model, name, error) : false;
}
bool ReplaceParallelBoxplotVariable(PlotModel &model,
                                    const std::string &oldName,
                                    const std::string &newName,
                                    std::string *error = nullptr);
inline bool ReplaceParallelBoxplotVariable(PlotModel *model,
                                           const std::string &oldName,
                                           const std::string &newName,
                                           std::string *error = nullptr) {
    return model ? ReplaceParallelBoxplotVariable(*model, oldName, newName, error) : false;
}
bool RebuildBoxplotH0Simulation(PlotModel &model, std::string *error = nullptr);
inline bool RebuildBoxplotH0Simulation(PlotModel *model, std::string *error = nullptr) {
    return model ? RebuildBoxplotH0Simulation(*model, error) : false;
}

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_BOXPLOT_MODEL_H
