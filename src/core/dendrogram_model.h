#ifndef RLISPSTAT_CORE_DENDROGRAM_MODEL_H
#define RLISPSTAT_CORE_DENDROGRAM_MODEL_H

#include <cstddef>
#include <limits>
#include <map>
#include <set>
#include <string>
#include <vector>

#include "plot_geometry.h"
#include "provenance_model.h"

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
    bool verticalFlip = false;
    bool rotate270 = false;
    bool rotateCaseLabels90 = false;
    std::string labelVariable;
    bool labelVariableConfigured = false;
    std::map<int, std::string> rowLabels;
    std::string colorByVariable;
    std::map<int, std::string> colorByRowColors;
    std::vector<std::pair<std::string, std::string>> colorByLegendItems;
    // Semantic legend level -> original case rows. Keep selection identity
    // independent from palette colors, which may be reused by multiple levels.
    std::map<std::string, std::vector<int>> colorByLegendRows;
    bool colorByLegendVisible = true;
    double colorByLegendX = 0.72;
    double colorByLegendY = 0.10;
    int modelVersion = 0;
    std::uint64_t requestRevision = 0;
    std::uint64_t sourceDataVersion = 0;
    int displayedImputation = 1;
    bool rFitPending = false;
    std::string status;
    AnalysisProvenance provenance;
    PlotModel seed;
    bool hasSeed = false;
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
    bool labelRotated90 = false;
};

struct DendrogramBranchSegment {
    Point start;
    Point end;
    std::vector<int> rows;
};

struct DendrogramJoinGeometry {
    int mergeIndex = -1;
    Point point;
    Point segmentStart;
    Point segmentEnd;
    std::vector<int> rows;
};

struct DendrogramPlotGeometry {
    bool hasCases = false;
    Rect plotRect;
    double maxHeight = 1.0;
    std::vector<DendrogramLeafGeometry> leaves;
    std::vector<DendrogramBranchSegment> branches;
    std::vector<DendrogramJoinGeometry> joins;
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
std::string DendrogramAddVariablesStatus();
std::string DendrogramEmptyPlotStatus(std::size_t variableCount);
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
DendrogramSize DendrogramViewportContentSize(std::size_t caseCount, double width,
                                            double height, bool fitTree, bool rotated);
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
std::vector<std::string> DendrogramVariableCaptionLines(const std::vector<std::string> &variables, double width);
DendrogramPlotGeometry BuildDendrogramPlotGeometry(
    const std::vector<int> &caseRows,
    const std::vector<DendrogramMergeModel> &merges,
    const std::vector<int> &leafOrder,
    double boundsWidth,
    double boundsHeight,
    bool verticalFlip = false,
    const std::map<int, std::string> &rowLabels = {},
    bool rotate270 = false,
    bool rotateCaseLabels90 = false,
    double variableHeaderHeight = 0.0);
int DendrogramNearestLeafRowAtPoint(const DendrogramPlotGeometry &geometry,
                                    const Point &point,
                                    double maxDistance);
std::vector<int> DendrogramLeafRowsInRect(const DendrogramPlotGeometry &geometry,
                                          const Rect &rect);
std::vector<int> DendrogramJoinRowsAtPoint(const DendrogramPlotGeometry &geometry,
                                           const Point &point,
                                           double maxDistance = 9.0);
bool DendrogramBranchIsFullySelected(const DendrogramBranchSegment &branch,
                                     const std::set<int> &selectedRows);
std::string DendrogramUniformBranchColor(
    const DendrogramBranchSegment &branch,
    const std::map<int, std::string> &manualRowColors,
    const std::map<int, std::string> &groupedRowColors);
DendrogramSelectionGestureResult DendrogramSelectionRowsForGesture(
    const DendrogramPlotGeometry &geometry,
    const Rect &brush,
    const Point &clickPoint,
    double minimumBrushSize = 3.0,
    double maxClickDistance = 9.0);

bool SetDendrogramLabelVariable(DendrogramState &state,
                                const DataFrameModel &dataframe,
                                const std::string &variable,
                                std::string *error = nullptr);
bool SetDendrogramColorByVariable(DendrogramState &state,
                                  const DataFrameModel &dataframe,
                                  const std::string &variable,
                                  std::string *error = nullptr);
std::set<int> DendrogramColorLegendRows(const DendrogramState &state,
                                        const std::string &level);
bool DendrogramColorLegendMatchesLinkedRowColors(
    const DendrogramState &state,
    const std::map<int, std::string> &linkedRowColors);

// Decode R's hclust tree; native code only validates topology and renders it.
bool ReadDendrogramRTree(const std::vector<std::string> &payload, std::size_t &cursor,
                        DendrogramFitResult &result, std::string &error);
std::string DendrogramCopyWindowTitle();

size_t DendrogramRowCountForVariables(PlotModel *model,
                                      const std::vector<std::string> &variables,
                                      std::vector<NumericVariable *> &vars);

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_DENDROGRAM_MODEL_H
