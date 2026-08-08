#ifndef RLISPSTAT_CORE_DENDROGRAM_MODEL_H
#define RLISPSTAT_CORE_DENDROGRAM_MODEL_H

#include <cstddef>
#include <limits>
#include <string>
#include <vector>

#include "plot_geometry.h"

namespace rlispstat {
namespace core {

struct DendrogramMergeModel {
    int left = -1;
    int right = -1;
    double height = std::numeric_limits<double>::quiet_NaN();
    int size = 0;
};

struct DendrogramState {
    std::string id;
    std::string group;
    AnalysisScope dataScope;
    bool dataScopeCaptured = false;
    std::vector<std::string> variables;
    std::string distance = "euclidean";
    std::string linkage = "average";
    std::string missingMode = "pairwise";
    std::vector<int> caseRows;
    std::vector<DendrogramMergeModel> merges;
    std::vector<int> leafOrder;
    int modelVersion = 0;
    PlotModel seed;
    bool hasSeed = false;
};

struct DendrogramFitInput {
    std::vector<std::string> variables;
    std::vector<std::vector<double>> columns;
    std::string distance = "euclidean";
    std::string linkage = "average";
    std::string missingMode = "pairwise";
    // Stable 1-based original row IDs. Empty means all rows unless
    // restrictRows is true, in which case the explicit scope is empty.
    std::vector<int> includedRows;
    bool restrictRows = false;
};

struct DendrogramFitResult {
    std::string distance = "euclidean";
    std::string linkage = "average";
    std::string missingMode = "pairwise";
    std::vector<int> caseRows;
    std::vector<DendrogramMergeModel> merges;
    std::vector<int> leafOrder;
};

struct DendrogramSize {
    double width = 760.0;
    double height = 450.0;
};

struct DendrogramWindowLayout {
    double maxWidth = 700.0;
    double maxHeight = 420.0;
    double targetWidth = 700.0;
    double targetHeight = 420.0;
    Rect titleRect;
    Rect badgeRect;
    Rect linkageLabelRect;
    Rect linkagePopupRect;
    Rect missingLabelRect;
    Rect missingPopupRect;
    Rect distanceLabelRect;
    Rect variablesButtonRect;
    Rect scrollViewRect;
    Rect statusRect;
};

struct DendrogramVariableMenuItem {
    std::string variable;
    bool included = false;
    bool enabled = true;
};

struct DendrogramVariableMenuState {
    std::vector<DendrogramVariableMenuItem> items;
    std::string emptyTitle;
};

struct DendrogramVariableUpdateResult {
    bool ok = false;
    bool changed = false;
    std::vector<std::string> variables;
    std::string status;
};

struct DendrogramContextMenuState {
    std::string title = "Quick Cluster";
    std::string selectAllTitle = "Select all displayed cases";
    std::string clearSelectionTitle = "Clear selection";
    std::string invertSelectionTitle = "Invert displayed selection";
    std::string colorSelectedCasesTitle;
    std::string dataPointsColorTitle = "Data Points Color...";
    std::string resetSelectedColorsTitle = "Reset selected case colors";
    std::string exportTitle = "Export";
    std::string savePngTitle;
    std::string savePdfTitle;
    std::string copyPngTitle;
    std::string copyPdfTitle;
};

struct DendrogramLeafGeometry {
    int rowId = 0;
    int leafIndex = 0;
    int orderIndex = 0;
    Point point;
    Rect labelRect;
    std::string label;
    bool showLabel = false;
};

struct DendrogramBranchSegment {
    Point start;
    Point end;
};

struct DendrogramPlotGeometry {
    bool hasCases = false;
    Rect plotRect;
    double maxHeight = 1.0;
    std::vector<DendrogramLeafGeometry> leaves;
    std::vector<DendrogramBranchSegment> branches;
};

struct DendrogramSelectionGestureResult {
    bool usedBrush = false;
    std::vector<int> rows;
};

bool DendrogramLinkageIsValid(const std::string &linkage);
bool DendrogramDistanceIsValid(const std::string &distance);
std::string DendrogramWindowSummaryStatus(const std::string &group,
                                          std::size_t caseCount,
                                          std::size_t variableCount,
                                          const std::string &linkage,
                                          const std::string &missingMode,
                                          std::size_t selectedCount);
std::string DendrogramWindowTitle();
std::string DendrogramVariableMenuTitle();
std::string DendrogramLinkageControlLabel();
std::string DendrogramMissingDataControlLabel();
std::string DendrogramDistanceControlLabel(const std::string &distance);
std::string DendrogramAtLeastOneVariableStatus();
std::string DendrogramColorSelectedCasesTitle(const std::string &color);
std::string DendrogramVariablesButtonTitle(std::size_t variableCount);
std::string DendrogramNoNumericVariablesTitle();
std::string DendrogramExportPanelTitle(const std::string &format);
std::string DendrogramDefaultExportFilename(const std::string &format);
std::string DendrogramExportFailedMessage();
std::string DendrogramExportWindowTitle();
std::string DendrogramPDFOptionIdentifier();
std::string DendrogramPNGOptionIdentifier();
std::string DendrogramCopyFailedMessage(const std::string &format);
std::string DendrogramAverageLinkageTitle();
std::string DendrogramCompleteLinkageTitle();
std::string DendrogramSingleLinkageTitle();
DendrogramSize DendrogramPreferredContentSize(std::size_t caseCount);
DendrogramWindowLayout BuildDendrogramWindowLayout(double dendrogramWidth,
                                                   double dendrogramHeight,
                                                   double visibleWidth,
                                                   double visibleHeight);
DendrogramVariableMenuState BuildDendrogramVariableMenuState(
    const std::vector<std::string> &currentVariables,
    const std::vector<std::string> &numericVariables,
    std::size_t minimumVariables = 1);
DendrogramVariableUpdateResult DendrogramVariablesAfterToggle(
    const std::vector<std::string> &currentVariables,
    const std::string &variable,
    const std::vector<std::string> &numericVariables,
    std::size_t minimumVariables = 1);
DendrogramContextMenuState BuildDendrogramContextMenuState(
    const std::string &selectedColor);
DendrogramPlotGeometry BuildDendrogramPlotGeometry(
    const std::vector<int> &caseRows,
    const std::vector<DendrogramMergeModel> &merges,
    const std::vector<int> &leafOrder,
    double boundsWidth,
    double boundsHeight);
int DendrogramNearestLeafRowAtPoint(const DendrogramPlotGeometry &geometry,
                                    const Point &point,
                                    double maxDistance);
std::vector<int> DendrogramLeafRowsInRect(const DendrogramPlotGeometry &geometry,
                                          const Rect &rect);
DendrogramSelectionGestureResult DendrogramSelectionRowsForGesture(
    const DendrogramPlotGeometry &geometry,
    const Rect &brush,
    const Point &clickPoint,
    double minimumBrushSize = 3.0,
    double maxClickDistance = 9.0);

DendrogramFitResult FitDendrogram(const DendrogramFitInput &input);
std::string DendrogramCopyWindowTitle();

size_t DendrogramRowCountForVariables(PlotModel *model,
                                      const std::vector<std::string> &variables,
                                      std::vector<NumericVariable *> &vars);

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_DENDROGRAM_MODEL_H
