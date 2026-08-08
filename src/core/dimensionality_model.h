#ifndef RLISPSTAT_CORE_DIMENSIONALITY_MODEL_H
#define RLISPSTAT_CORE_DIMENSIONALITY_MODEL_H

#include <cstddef>
#include <limits>
#include <string>
#include <vector>

#include "plot_geometry.h"

namespace rlispstat {
namespace core {

std::string DimensionalityWindowTitle();
std::string DimensionalityStandardizeButtonTitle();
std::string DimensionalityNoRotationTitle();
std::string DimensionalityVarimaxRotationTitle();
std::string DimensionalityQuartimaxRotationTitle();
std::string DimensionalityPCAMethodTitle();
std::string DimensionalityFactorAnalysisMethodTitle();

std::string DimensionalityComponentPrefix(const std::string &method);
std::string DimensionalityVariableUnavailableStatus(const std::string &name);
std::string DimensionalityVariableAlreadyIncludedStatus(const std::string &name);
std::string DimensionalityVariableAddedStatus(const std::string &name);
std::string DimensionalityVariableReplacedStatus(const std::string &oldName,
                                                 const std::string &newName);
std::string DimensionalityVariableRemovedStatus(const std::string &name);
std::string DimensionalityNoMoreNumericVariablesTitle();
std::string DimensionalityNoReplacementVariablesTitle();
std::string DimensionalityAddVariableMenuTitle();
std::string DimensionalityVariableMenuTitle();
std::string DimensionalityReplaceVariableMenuTitle();
std::string DimensionalityShowVariableInformationTitle();
std::string DimensionalityRemoveVariableTitle(const std::string &name);
std::string DimensionalityBiplotDimensionsMenuTitle();
std::string DimensionalityComponentMenuLabel(const std::string &method,
                                             int component,
                                             double variance);
std::string DimensionalityBiplotRequiresTwoComponentsStatus();
std::string DimensionalityNoScoresStatus();
std::string DimensionalitySourceSheetUnavailableStatus();
std::string DimensionalityScoresSavedStatus(std::size_t columnCount);
std::string DimensionalityScreePlotTitle(const std::string &method,
                                         int retainedComponents,
                                         const std::string &rotation);
std::string DimensionalityFitSignature(const std::string &method,
                                       const std::string &missingMode,
                                       const std::string &rotation,
                                       const std::string &scope,
                                       bool scale,
                                       int componentCount,
                                       const std::vector<std::string> &variables);

bool DimensionalityRotationIsValid(const std::string &rotation);
bool DimensionalityScopeIsValid(const std::string &scope);
std::string DimensionalityMethodForPopupIndex(int index);
int DimensionalityMethodPopupIndex(const std::string &method);
std::string DimensionalityRotationForPopupIndex(int index);
int DimensionalityRotationPopupIndex(const std::string &rotation);
std::string DimensionalityScopeForPopupIndex(int index);
int DimensionalityScopePopupIndex(const std::string &scope);

bool JacobiEigenSymmetric(std::vector<std::vector<double>> a,
                          std::vector<double> &values,
                          std::vector<std::vector<double>> &vectors);

void ApplyDimensionalityOrthomaxRotation(std::vector<std::vector<double>> &loadings,
                                         std::vector<std::vector<double>> &scores,
                                         const std::string &rotation);

std::vector<double> DimensionalityParallelEigenvalues(std::size_t n,
                                                      std::size_t p,
                                                      int iterations = 100);

struct DimensionalityFitComponent {
    int index = 0;
    double eigenvalue = std::numeric_limits<double>::quiet_NaN();
    double parallelEigenvalue = std::numeric_limits<double>::quiet_NaN();
    double variance = std::numeric_limits<double>::quiet_NaN();
    double cumulative = std::numeric_limits<double>::quiet_NaN();
};

struct DimensionalityFitLoading {
    std::string variable;
    std::vector<double> values;
    double communality = std::numeric_limits<double>::quiet_NaN();
    double uniqueness = std::numeric_limits<double>::quiet_NaN();
};

struct DimensionalityFitScore {
    int row = 0;
    double x = std::numeric_limits<double>::quiet_NaN();
    double y = std::numeric_limits<double>::quiet_NaN();
    std::vector<double> values;
};

struct DimensionalityState {
    std::string id;
    std::string group;
    AnalysisScope dataScope;
    bool dataScopeCaptured = false;
    std::vector<std::string> variables;
    std::string method = "pca";
    std::string missingMode = "listwise";
    std::string rotation = "none";
    std::string scope = "all";
    bool scale = true;
    int componentCount = 2;
    std::vector<int> rowsUsed;
    std::vector<int> rowsExcluded;
    std::vector<DimensionalityFitComponent> components;
    std::vector<DimensionalityFitLoading> loadings;
    std::vector<DimensionalityFitScore> scores;
    std::string status = "Not fitted.";
    int focusedComponent = 0;
    std::string focusedVariable;
    int modelVersion = 0;
    bool rFitPending = false;
    std::string lastRFitSignature;
    PlotModel seed;
    bool hasSeed = false;
};

struct DimensionalityScreePoint {
    int component = 0;
    double x = std::numeric_limits<double>::quiet_NaN();
    double y = std::numeric_limits<double>::quiet_NaN();
    int rowId = 0;
};

struct DimensionalityScreePlotState {
    bool ok = false;
    std::string title;
    std::string xLabel = "Component";
    std::string yLabel = "Eigenvalue";
    std::vector<DimensionalityScreePoint> observed;
    std::vector<DimensionalityScreePoint> parallel;
    double xmin = 0.0;
    double xmax = 1.0;
    double ymin = 0.0;
    double ymax = 1.0;
};

struct DimensionalityBiplotScorePoint {
    int rowId = 0;
    double x = std::numeric_limits<double>::quiet_NaN();
    double y = std::numeric_limits<double>::quiet_NaN();
};

struct DimensionalityBiplotLoadingVector {
    std::string variable;
    double x = std::numeric_limits<double>::quiet_NaN();
    double y = std::numeric_limits<double>::quiet_NaN();
};

struct DimensionalityBiplotPlotState {
    bool ok = false;
    int xComponent = 1;
    int yComponent = 2;
    std::string title;
    std::string xLabel;
    std::string yLabel;
    std::vector<DimensionalityBiplotScorePoint> scores;
    std::vector<DimensionalityBiplotLoadingVector> loadings;
    double dataXmin = 0.0;
    double dataXmax = 1.0;
    double dataYmin = 0.0;
    double dataYmax = 1.0;
    double xmin = 0.0;
    double xmax = 1.0;
    double ymin = 0.0;
    double ymax = 1.0;
};

struct DimensionalityScreeContextMenuState {
    std::string title = "Scree plot";
    std::string viewTitle = "View";
    std::string exportTitle = "Export";
    std::string rescaleToDataTitle = "Rescale to data";
    std::string closePlotTitle = "Close plot";
};

struct DimensionalityScreeRenderPoint {
    int component = 0;
    Point point;
    bool focused = false;
};

struct DimensionalityScreeRenderPlan {
    std::vector<Point> observedLine;
    std::vector<Point> parallelLine;
    std::vector<DimensionalityScreeRenderPoint> observedPoints;
};

struct DimensionalityBiplotLoadingRenderItem {
    std::string variable;
    Point start;
    Point end;
    Point arrowHeadA;
    Point arrowHeadB;
    Point labelAnchor;
    bool focused = false;
};

struct DimensionalityBiplotRenderPlan {
    std::vector<DimensionalityBiplotLoadingRenderItem> loadings;
};

struct DimensionalityScoreExportColumn {
    std::string name;
    std::vector<std::string> values;
};

struct DimensionalityScoreExportPlan {
    bool ok = false;
    std::vector<DimensionalityScoreExportColumn> columns;
};

struct DimensionalityReportComponentRow {
    int component = 0;
    std::string label;
    std::string eigenvalue;
    std::string parallelEigenvalue;
    std::string variance;
    std::string cumulative;
    bool highlighted = false;
};

struct DimensionalityReportValueCell {
    int component = 0;
    std::string text;
    bool emphasized = false;
    bool highlighted = false;
};

struct DimensionalityReportLoadingRow {
    std::string variable;
    std::vector<DimensionalityReportValueCell> loadings;
    std::string communality;
    std::string uniqueness;
    bool highlighted = false;
};

struct DimensionalityReportState {
    std::string summary;
    std::string componentPrefix;
    int componentCount = 0;
    int focusedComponent = 0;
    std::string focusedVariable;
    std::vector<DimensionalityReportComponentRow> componentRows;
    bool hasAdditionalComponents = false;
    bool hasLoadings = false;
    std::vector<std::string> loadingHeaders;
    std::vector<DimensionalityReportLoadingRow> loadingRows;
    std::string status;
};

struct DimensionalityReportLayout {
    double preferredWidth = 740.0;
    double preferredHeight = 380.0;
    double componentRowsY = 90.0;
    double loadingsRowsY = 156.0;
    Rect summaryRect;
    Rect componentsTitleRect;
    std::vector<Rect> componentHeaderRects;
    std::vector<std::vector<Rect>> componentCellRects;
    Rect additionalComponentsRect;
    double componentRuleEndX = 628.0;
    Rect loadingsTitleRect;
    Rect loadingVariableHeaderRect;
    std::vector<Rect> componentRects;
    std::vector<Rect> loadingHeaderRects;
    std::vector<std::vector<Rect>> loadingCellRects;
    std::vector<Rect> variableRects;
    Rect communalityHeaderRect;
    Rect uniquenessHeaderRect;
    std::vector<Rect> communalityRects;
    std::vector<Rect> uniquenessRects;
    double loadingsRuleEndX = 628.0;
    Rect addVariableRect;
    Rect emptyStatusRect;
};

enum class DimensionalityReportHitKind {
    None,
    Component,
    LoadingHeader,
    LoadingCell,
    Variable,
    AddVariable
};

struct DimensionalityReportHit {
    DimensionalityReportHitKind kind = DimensionalityReportHitKind::None;
    int component = 0;
    std::size_t componentIndex = 0;
    std::size_t variableIndex = 0;
    std::string variable;
};

enum class DimensionalityReportActionKind {
    ClearFocus,
    FocusComponent,
    FocusLoading,
    OpenVariableMenu,
    OpenAddVariableMenu,
    OpenAnalysisMenu
};

struct DimensionalityReportAction {
    DimensionalityReportActionKind kind = DimensionalityReportActionKind::ClearFocus;
    int component = 0;
    std::size_t variableIndex = 0;
    std::string variable;
    bool focusVariable = false;
};

enum class DimensionalityReportTextRole {
    Section,
    Left,
    Muted,
    Right,
    RightHeader
};

enum class DimensionalityReportHighlightRole {
    Soft,
    Strong
};

struct DimensionalityReportTextItem {
    std::string text;
    Rect rect;
    DimensionalityReportTextRole role = DimensionalityReportTextRole::Left;
};

struct DimensionalityReportHighlightItem {
    Rect rect;
    DimensionalityReportHighlightRole role = DimensionalityReportHighlightRole::Soft;
};

struct DimensionalityReportRuleItem {
    Point start;
    Point end;
};

struct DimensionalityReportRenderPlan {
    std::vector<DimensionalityReportHighlightItem> highlights;
    std::vector<DimensionalityReportRuleItem> rules;
    std::vector<DimensionalityReportTextItem> texts;
};

struct DimensionalityReportViewModel {
    DimensionalityReportState report;
    DimensionalityReportLayout layout;
    DimensionalityReportRenderPlan renderPlan;
};

struct DimensionalityWindowLayout {
    double maxWidth = 720.0;
    double maxHeight = 430.0;
    double targetWidth = 760.0;
    double targetHeight = 520.0;
    Rect titleRect;
    Rect badgeRect;
    Rect methodPopupRect;
    Rect missingPopupRect;
    Rect componentsPopupRect;
    Rect rotationPopupRect;
    Rect scopeLabelRect;
    Rect scopePopupRect;
    Rect scaleButtonRect;
    Rect selectedRowsRect;
    Rect scrollViewRect;
    Rect statusRect;
};

struct DimensionalityControlState {
    int methodIndex = 0;
    int rotationIndex = 0;
    int scopeIndex = 0;
    std::string missingMode = "listwise";
    bool scale = true;
    int maxComponents = 1;
    int componentCount = 1;
    int componentIndex = 0;
    std::vector<std::string> componentOptions;
};

struct DimensionalityVariableUpdateResult {
    bool ok = false;
    bool changed = false;
    std::vector<std::string> variables;
    std::string error;
};

struct DimensionalityAddVariableMenuState {
    std::vector<std::string> variables;
    std::string emptyTitle;
};

struct DimensionalityVariableMenuState {
    bool ok = false;
    std::size_t index = 0;
    std::string variable;
    std::vector<std::string> replacementVariables;
    std::string replacementEmptyTitle;
    std::string removeTitle;
    bool canRemove = false;
};

struct DimensionalityAnalysisMenuState {
    std::string title = "Principal Components / Factor Analysis";
    std::string screePlotTitle = "Scree Plot";
    std::string biplotTitle = "Biplot";
    std::string saveScoresTitle = "Save Scores to Data Sheet";
    std::string addVariableTitle = "Add Variable...";
    bool canOpenScreePlot = false;
    bool canOpenBiplot = false;
    bool canSaveScores = false;
};

struct DimensionalityFitInput {
    std::vector<std::string> variables;
    std::vector<std::vector<double>> columns;
    std::vector<int> selectedRows;
    std::string method = "pca";
    std::string missingMode = "listwise";
    std::string rotation = "none";
    std::string scope = "all";
    bool scale = true;
    int componentCount = 2;
    int parallelIterations = 100;
};

struct DimensionalityFitResult {
    std::string method = "pca";
    std::string missingMode = "listwise";
    std::string rotation = "none";
    std::string scope = "all";
    bool scale = true;
    int componentCount = 2;
    std::vector<int> rowsUsed;
    std::vector<int> rowsExcluded;
    std::vector<DimensionalityFitComponent> components;
    std::vector<DimensionalityFitLoading> loadings;
    std::vector<DimensionalityFitScore> scores;
    std::string status = "Not fitted.";
};

DimensionalityFitResult FitDimensionality(const DimensionalityFitInput &input);
DimensionalityScreePlotState BuildDimensionalityScreePlotState(
    const std::vector<DimensionalityFitComponent> &components,
    const std::string &method,
    int retainedComponents,
    const std::string &rotation);
DimensionalityBiplotPlotState BuildDimensionalityBiplotPlotState(
    const std::vector<DimensionalityFitComponent> &components,
    const std::vector<DimensionalityFitLoading> &loadings,
    const std::vector<DimensionalityFitScore> &scores,
    const std::string &method,
    int requestedXComponent,
    int requestedYComponent);
DimensionalityScreeContextMenuState BuildDimensionalityScreeContextMenuState();
DimensionalityScreeRenderPlan BuildDimensionalityScreeRenderPlan(
    const std::vector<DimensionalityScreePoint> &observed,
    const std::vector<DimensionalityScreePoint> &parallel,
    const DataViewport &viewport,
    const Rect &plotRect,
    int focusedComponent = 0);
DimensionalityBiplotRenderPlan BuildDimensionalityBiplotRenderPlan(
    const std::vector<DimensionalityBiplotLoadingVector> &loadings,
    const DataViewport &viewport,
    const Rect &plotRect,
    const std::string &focusedVariable = "");
DimensionalityScoreExportPlan BuildDimensionalityScoreExportPlan(
    const std::vector<DimensionalityFitScore> &scores,
    const std::string &method,
    int requestedComponents,
    int availableComponents,
    int rowCount,
    const std::vector<std::string> &existingNames);
DimensionalityReportState BuildDimensionalityReportState(
    const std::vector<std::string> &variables,
    const std::vector<DimensionalityFitComponent> &components,
    const std::vector<DimensionalityFitLoading> &loadings,
    std::size_t rowsUsed,
    std::size_t rowsExcluded,
    const std::string &method,
    int componentCount,
    const std::string &status,
    int focusedComponent = 0,
    const std::string &focusedVariable = "",
    std::size_t maxShownComponents = 8);
DimensionalityReportLayout BuildDimensionalityReportLayout(
    const DimensionalityReportState &report);
Rect DimensionalityReportRectForComponent(
    const DimensionalityReportState &report,
    const DimensionalityReportLayout &layout,
    int component);
Rect DimensionalityReportRectForVariableIndex(
    const DimensionalityReportLayout &layout,
    std::size_t index);
Rect DimensionalityReportRectForVariable(
    const DimensionalityReportState &report,
    const DimensionalityReportLayout &layout,
    const std::string &variable);
Rect DimensionalityReportFocusRect(
    const DimensionalityReportState &report,
    const DimensionalityReportLayout &layout,
    int component,
    const std::string &variable);
DimensionalityReportRenderPlan BuildDimensionalityReportRenderPlan(
    const DimensionalityReportState &report,
    const DimensionalityReportLayout &layout);
DimensionalityReportViewModel BuildDimensionalityReportViewModel(
    const std::vector<std::string> &variables,
    const std::vector<DimensionalityFitComponent> &components,
    const std::vector<DimensionalityFitLoading> &loadings,
    std::size_t rowsUsed,
    std::size_t rowsExcluded,
    const std::string &method,
    int componentCount,
    const std::string &status,
    int focusedComponent = 0,
    const std::string &focusedVariable = "",
    std::size_t maxShownComponents = 8);
DimensionalityReportHit HitTestDimensionalityReport(
    const DimensionalityReportState &report,
    const DimensionalityReportLayout &layout,
    const Point &point,
    double xPadding = 3.0,
    double yPadding = 2.0);
DimensionalityReportAction DimensionalityReportPrimaryActionForHit(
    const DimensionalityReportHit &hit);
DimensionalityReportAction DimensionalityReportContextActionForHit(
    const DimensionalityReportHit &hit);
DimensionalityWindowLayout BuildDimensionalityWindowLayout(
    double reportWidth,
    double reportHeight,
    double visibleWidth,
    double visibleHeight);
DimensionalityControlState BuildDimensionalityControlState(
    const std::string &method,
    const std::string &missingMode,
    const std::string &rotation,
    const std::string &scope,
    bool scale,
    std::size_t variableCount,
    int requestedComponentCount);
std::vector<std::string> DimensionalityVariablesAvailableToAdd(
    const std::vector<std::string> &currentVariables,
    const std::vector<std::string> &availableVariables);
std::vector<std::string> DimensionalityVariablesAvailableToReplace(
    const std::vector<std::string> &currentVariables,
    const std::vector<std::string> &availableVariables,
    const std::string &oldName);
DimensionalityAddVariableMenuState BuildDimensionalityAddVariableMenuState(
    const std::vector<std::string> &currentVariables,
    const std::vector<std::string> &availableVariables);
DimensionalityVariableMenuState BuildDimensionalityVariableMenuState(
    const std::vector<std::string> &currentVariables,
    const std::vector<std::string> &availableVariables,
    std::size_t index,
    std::size_t minimumVariables = 2);
DimensionalityAnalysisMenuState BuildDimensionalityAnalysisMenuState(
    std::size_t availableComponentCount,
    int retainedComponentCount,
    std::size_t scoreCount);
DimensionalityVariableUpdateResult DimensionalityVariablesAfterAdd(
    const std::vector<std::string> &currentVariables,
    const std::string &name,
    const std::vector<std::string> &availableVariables);
DimensionalityVariableUpdateResult DimensionalityVariablesAfterReplace(
    const std::vector<std::string> &currentVariables,
    std::size_t index,
    const std::string &name,
    const std::vector<std::string> &availableVariables);
DimensionalityVariableUpdateResult DimensionalityVariablesAfterRemove(
    const std::vector<std::string> &currentVariables,
    std::size_t index,
    std::size_t minimumVariables = 2);

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_DIMENSIONALITY_MODEL_H
