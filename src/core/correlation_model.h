#ifndef RLISPSTAT_CORE_CORRELATION_MODEL_H
#define RLISPSTAT_CORE_CORRELATION_MODEL_H

#include "dataset_model.h"
#include "plot_geometry.h"

#include <limits>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

struct CorrelationCellResult {
    std::string xVariable;
    std::string yVariable;
    double r = std::numeric_limits<double>::quiet_NaN();
    double p = std::numeric_limits<double>::quiet_NaN();
    int n = 0;
    std::vector<int> rowsUsed;
    std::string status = "valid";
    std::string detail;
};

struct CorrelationMatrixState {
    std::string id;
    std::string group;
    AnalysisScope dataScope;
    bool dataScopeCaptured = false;
    std::string title = "Pearson Correlation Matrix";
    std::vector<std::string> variables;
    std::string method = "pearson";
    std::string missingMode = "pairwise";
    bool showP = true;
    bool showPValue = false;
    bool showN = false;
    std::string displayPart = "full";
    bool precomputed = false;
    bool multipleImputation = false;
    int imputationCount = 0;
    std::string poolingMethod;
    std::string note;
    std::vector<CorrelationCellResult> cells;
    int selectedRow = -1;
    int selectedCol = -1;
    int modelVersion = 0;
    std::uint64_t requestRevision = 0, sourceDataVersion = 0;
    bool rFitPending = false;
    PlotModel seed;
    bool hasSeed = false;
    AnalysisProvenance provenance;
};

struct CorrelationContextOption {
    std::string section, title, command, value;
    bool checked = false, enabled = true;
};
std::vector<CorrelationContextOption> CorrelationContextOptions(const CorrelationMatrixState &state);
bool CorrelationCellIsVisible(const std::string &displayPart, int row, int column);
std::string CorrelationDisplayedCellText(const CorrelationMatrixState &state, int row, int column);

struct CorrelationSize {
    double width = 720.0;
    double height = 360.0;
};

struct CorrelationWindowLayout {
    double maxWidth = 640.0;
    double maxHeight = 420.0;
    double targetWidth = 640.0;
    double targetHeight = 420.0;
    Rect titleRect;
    Rect badgeRect;
    Rect missingLabelRect;
    Rect missingPopupRect;
    Rect showPButtonRect;
    Rect showPValueButtonRect;
    Rect showNButtonRect;
    Rect scopeLabelRect;
    Rect scopePopupRect;
    Rect scrollViewRect;
    Rect statusRect;
};

struct CorrelationWindowControlState {
    std::string windowTitle;
    std::string title;
    std::string badge;
    std::string missingMode;
    bool missingModeEnabled = true;
    bool showP = true;
    bool showPValue = false;
    bool showN = false;
    std::string status;
};

struct CorrelationMatrixLayout {
    double top = 42.0;
    double left = 16.0;
    double rowHeaderWidth = 132.0;
    double cellWidth = 108.0;
    double headerHeight = 28.0;
    double rowHeight = 32.0;
    double width = 720.0;
    double height = 360.0;
    Rect addVariableRect;
    Rect emptyStatusRect;
    std::vector<Rect> columnHeaderRects;
    std::vector<Rect> rowHeaderRects;
    std::vector<std::vector<Rect>> cellRects;
};

enum class CorrelationMatrixHitKind {
    None,
    AddVariable,
    Variable,
    Cell
};

struct CorrelationMatrixHit {
    CorrelationMatrixHitKind kind = CorrelationMatrixHitKind::None;
    int variableIndex = -1;
    int row = -1;
    int column = -1;
};

struct CorrelationVariableUpdateResult {
    bool ok = false;
    bool changed = false;
    std::vector<std::string> variables;
    std::string status;
};

struct CorrelationAddVariableMenuState {
    bool locked = false;
    std::vector<std::string> variables;
    std::string emptyTitle;
    std::string lockedTitle;
};

struct CorrelationVariableMenuState {
    bool valid = false;
    bool locked = false;
    std::string variable;
    std::string addVariableTitle;
    std::string replaceVariableTitle;
    std::string removeVariableTitle;
    std::string treatAsNumericTitle;
    std::string treatAsFactorTitle;
    std::string informationTitle;
    std::string lockedTitle;
};

struct CorrelationCellMenuState {
    std::string title;
    std::string openScatterplotTitle;
    bool canOpenScatterplot = false;
    std::string copyRTitle;
    std::string copyDetailTitle;
};

enum class CorrelationCellBackground {
    None,
    Selected,
    Diagonal
};

struct CorrelationTextItem {
    std::string text;
    Rect rect;
    bool muted = false;
};

struct CorrelationHeaderRenderItem {
    int index = -1;
    std::string text;
    Rect rect;
};

struct CorrelationCellRenderItem {
    int row = -1;
    int column = -1;
    Rect rect;
    CorrelationCellBackground background = CorrelationCellBackground::None;
    std::vector<CorrelationTextItem> textItems;
};

struct CorrelationMatrixRenderPlan {
    CorrelationMatrixLayout layout;
    Rect headerRuleRect;
    std::vector<CorrelationHeaderRenderItem> columnHeaders;
    std::vector<CorrelationHeaderRenderItem> rowHeaders;
    std::vector<CorrelationCellRenderItem> cells;
    bool showEmptyMessage = false;
    std::string emptyMessage;
    Rect emptyMessageRect;
    std::string addVariableLabel;
    Rect addVariableRect;
};

std::string CorrelationCellLabel(const CorrelationCellResult *cell,
                                 bool showP,
                                 bool showPValue);
std::string CorrelationNoMoreNumericVariablesTitle();
std::string CorrelationAddVariableMenuTitle();
std::string CorrelationVariableMenuTitle();
std::string CorrelationReplaceVariableTitle(const std::string &name);
std::string CorrelationRemoveVariableTitle(const std::string &name);
std::string CorrelationPooledAddVariableLockedTitle();
std::string CorrelationPooledVariableEditLockedTitle();
std::string CorrelationPooledAddVariableStatus();
std::string CorrelationPooledRemoveVariableStatus();
std::string CorrelationPooledVariableTypeStatus();
std::string CorrelationMissingModeLockedStatus();
std::string CorrelationCellStatusText(const CorrelationCellResult *cell,
                                      int row,
                                      int column);
std::string CorrelationCellCopyDetailText(const CorrelationCellResult *cell);
std::string CorrelationWindowTitle();
std::string CorrelationShowStarsButtonTitle();
std::string CorrelationShowPValuesButtonTitle();
std::string CorrelationShowNButtonTitle();
double CorrelationMatrixRowHeight(bool showPValue, bool showN);
CorrelationMatrixLayout BuildCorrelationMatrixLayout(std::size_t variableCount,
                                                     bool showPValue,
                                                     bool showN);
CorrelationSize CorrelationMatrixPreferredContentSize(std::size_t variableCount,
                                                      bool showPValue,
                                                      bool showN);
Rect CorrelationMatrixCellRect(const CorrelationMatrixLayout &layout,
                               int row,
                               int column);
CorrelationMatrixHit HitTestCorrelationMatrix(const CorrelationMatrixLayout &layout,
                                              const Point &point,
                                              const std::string &displayPart = "full");
CorrelationWindowLayout BuildCorrelationWindowLayout(double matrixWidth,
                                                     double matrixHeight,
                                                     double visibleWidth,
                                                     double visibleHeight);
CorrelationWindowLayout BuildCorrelationWindowContentLayout(double contentWidth,
                                                            double contentHeight);
CorrelationWindowControlState BuildCorrelationWindowControlState(
    const std::string &group,
    const std::string &title,
    const std::vector<std::string> &variables,
    const std::string &missingMode,
    bool showP,
    bool showPValue,
    bool showN,
    bool precomputed,
    bool multipleImputation,
    int imputationCount,
    const std::string &note);
Rect CorrelationMatrixDocumentRect(double matrixWidth,
                                   double matrixHeight,
                                   double visibleWidth,
                                   double visibleHeight);
std::vector<std::string> CorrelationVariablesAvailableToAdd(
    const std::vector<std::string> &currentVariables,
    const std::vector<std::string> &availableVariables);
CorrelationVariableUpdateResult CorrelationVariablesAfterAdd(
    const std::vector<std::string> &currentVariables,
    const std::string &variable,
    const std::vector<std::string> &availableVariables);
CorrelationVariableUpdateResult CorrelationVariablesAfterReplace(
    const std::vector<std::string> &currentVariables,
    std::size_t index,
    const std::string &variable,
    const std::vector<std::string> &availableVariables);
CorrelationVariableUpdateResult CorrelationVariablesAfterRemove(
    const std::vector<std::string> &currentVariables,
    std::size_t index);
CorrelationAddVariableMenuState BuildCorrelationAddVariableMenuState(
    const std::vector<std::string> &currentVariables,
    const std::vector<std::string> &availableVariables,
    bool locked);
CorrelationVariableMenuState BuildCorrelationVariableMenuState(
    const std::vector<std::string> &variables,
    std::size_t index,
    bool locked);
CorrelationCellMenuState BuildCorrelationCellMenuState(int row,
                                                       int column);
const CorrelationCellResult *CorrelationCellAt(
    const std::vector<CorrelationCellResult> &cells,
    std::size_t variableCount,
    int row,
    int column);
CorrelationMatrixRenderPlan BuildCorrelationMatrixRenderPlan(
    const std::vector<std::string> &variables,
    const std::vector<CorrelationCellResult> &cells,
    int selectedRow,
    int selectedColumn,
    bool showP,
    bool showPValue,
    bool showN,
    const std::string &displayPart = "full");
std::vector<int> CompleteRowsForVariables(PlotModel *model, const std::vector<std::string> &variables);

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_CORRELATION_MODEL_H
