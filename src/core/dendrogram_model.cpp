#include "dendrogram_model.h"

#include "barplot_model.h"
#include "command_model.h"
#include "dataset_model.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdlib>
#include <sstream>

namespace rlispstat {
namespace core {

bool DendrogramLinkageIsValid(const std::string &linkage)
{
    return linkage == "average" || linkage == "complete" || linkage == "single";
}

bool DendrogramDistanceIsValid(const std::string &distance)
{
    return distance == "euclidean" || distance == "manhattan" ||
           distance == "maximum" || distance == "canberra";
}

std::string DendrogramWindowSummaryStatus(const std::string &group,
                                          std::size_t caseCount,
                                          std::size_t variableCount,
                                          const std::string &linkage,
                                          const std::string &missingMode,
                                          std::size_t selectedCount)
{
    std::ostringstream out;
    out << group << ": " << caseCount << " cases, " << variableCount
        << " variables, " << linkage << " linkage, " << missingMode
        << " missing mode, " << selectedCount << " selected";
    return out.str();
}

std::string DendrogramWindowTitle()
{
    return "Quick Cluster Dendrogram";
}

std::string DendrogramVariableMenuTitle()
{
    return "Variables";
}

std::string DendrogramLinkageControlLabel()
{
    return "Linkage:";
}

std::string DendrogramMissingDataControlLabel()
{
    return "Missing data:";
}

std::string DendrogramDistanceControlLabel(const std::string &distance)
{
    return "Distance: " + (distance.empty() ? std::string("euclidean") : distance) + " (z)";
}

std::string DendrogramAtLeastOneVariableStatus()
{
    return "At least one variable is required.";
}

std::string DendrogramAddVariablesStatus()
{
    return "Add one or more numeric variables to cluster the cases.";
}

std::string DendrogramEmptyPlotStatus(std::size_t variableCount)
{
    return variableCount == 0
        ? DendrogramAddVariablesStatus()
        : NoCompleteCasesStatus();
}

std::string DendrogramColorSelectedCasesTitle(const std::string &color)
{
    return "Color selected cases: " + color;
}

std::string DendrogramVariablesButtonTitle(std::size_t variableCount)
{
    (void)variableCount;
    return "+ Add variable";
}

std::string DendrogramNoNumericVariablesTitle()
{
    return "No numeric variables";
}

std::string DendrogramExportPanelTitle(const std::string &format)
{
    return format == "PDF" ? "Export Dendrogram as PDF" : "Export Dendrogram as PNG";
}

std::string DendrogramDefaultExportFilename(const std::string &format)
{
    std::string extension = format;
    for (char &ch : extension) {
        ch = static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
    }
    return "LinkEDA-dendrogram." + extension;
}

std::string DendrogramExportFailedMessage()
{
    return "The dendrogram view could not be exported.";
}

std::string DendrogramExportWindowTitle()
{
    return "Export Dendrogram";
}

std::string DendrogramCopyFailedMessage(const std::string &format)
{
    return "The dendrogram could not be copied as " + format + ".";
}

DendrogramSize DendrogramPreferredContentSize(std::size_t caseCount)
{
    DendrogramSize size;
    const double effectiveCases = static_cast<double>(std::max<std::size_t>(1, caseCount));
    size.width = std::max(760.0, 140.0 + effectiveCases * 16.0);
    size.height = 450.0;
    return size;
}

DendrogramSize DendrogramViewportContentSize(std::size_t caseCount, double width,
                                            double height, bool fitTree, bool rotated)
{
    auto size = DendrogramPreferredContentSize(caseCount);
    if (rotated) std::swap(size.width, size.height);
    size.width = fitTree ? width : std::max(size.width, width);
    size.height = fitTree ? height : std::max(size.height, height);
    return size;
}

DendrogramWindowLayout BuildDendrogramWindowLayout(double dendrogramWidth,
                                                   double dendrogramHeight,
                                                   double visibleWidth,
                                                   double visibleHeight)
{
    DendrogramWindowLayout layout;
    layout.maxWidth = std::max(700.0, visibleWidth * 0.94);
    layout.maxHeight = std::max(420.0, visibleHeight * 0.88);
    layout.targetWidth = std::min(layout.maxWidth, std::max(700.0, dendrogramWidth + 24.0));
    layout.targetHeight = std::min(layout.maxHeight, std::max(420.0, dendrogramHeight + 124.0));

    const double w = layout.targetWidth;
    const double h = layout.targetHeight;
    layout.titleRect = Rect{16.0, h - 38.0, 320.0, 24.0};
    layout.badgeRect = Rect{w - 170.0, h - 36.0, 150.0, 20.0};
    layout.linkageLabelRect = Rect{16.0, h - 70.0, 66.0, 22.0};
    layout.linkagePopupRect = Rect{82.0, h - 74.0, 110.0, 24.0};
    layout.missingLabelRect = Rect{212.0, h - 70.0, 86.0, 22.0};
    layout.missingPopupRect = Rect{300.0, h - 74.0, 126.0, 24.0};
    layout.distanceLabelRect = Rect{448.0, h - 70.0, 170.0, 22.0};
    // Keep the add control near the other controls, including in narrow windows.
    layout.variablesButtonRect = Rect{16.0, h - 102.0, 140.0, 24.0};
    layout.scrollViewRect = Rect{
        12.0,
        44.0,
        std::max(200.0, w - 24.0),
        std::max(140.0, h - 152.0)
    };
    layout.statusRect = Rect{16.0, 14.0, std::max(200.0, w - 32.0), 20.0};
    return layout;
}

static bool DendrogramVariableListContains(const std::vector<std::string> &variables,
                                           const std::string &name)
{
    return std::find(variables.begin(), variables.end(), name) != variables.end();
}

DendrogramVariableMenuState BuildDendrogramVariableMenuState(
    const std::vector<std::string> &currentVariables,
    const std::vector<std::string> &numericVariables,
    std::size_t minimumVariables)
{
    DendrogramVariableMenuState state;
    state.emptyTitle = DendrogramNoNumericVariablesTitle();
    for (const std::string &name : numericVariables) {
        bool duplicate = false;
        for (const DendrogramVariableMenuItem &item : state.items) {
            if (item.variable == name) {
                duplicate = true;
                break;
            }
        }
        if (duplicate) continue;
        const bool included = DendrogramVariableListContains(currentVariables, name);
        state.items.push_back(DendrogramVariableMenuItem{
            name,
            included,
            !(included && currentVariables.size() <= minimumVariables)
        });
    }
    return state;
}

DendrogramVariableUpdateResult DendrogramVariablesAfterToggle(
    const std::vector<std::string> &currentVariables,
    const std::string &variable,
    const std::vector<std::string> &numericVariables,
    std::size_t minimumVariables)
{
    DendrogramVariableUpdateResult result;
    result.variables = currentVariables;
    auto it = std::find(result.variables.begin(), result.variables.end(), variable);
    if (it == result.variables.end()) {
        if (!DendrogramVariableListContains(numericVariables, variable)) {
            return result;
        }
        result.variables.push_back(variable);
        result.ok = true;
        result.changed = true;
        return result;
    }
    if (result.variables.size() <= minimumVariables) {
        result.status = DendrogramAtLeastOneVariableStatus();
        return result;
    }
    result.variables.erase(it);
    result.ok = true;
    result.changed = true;
    return result;
}

DendrogramContextMenuState BuildDendrogramContextMenuState(
    const std::string &selectedColor)
{
    DendrogramContextMenuState state;
    state.colorSelectedCasesTitle = DendrogramColorSelectedCasesTitle(
        selectedColor.empty() ? "black" : selectedColor);
    std::vector<std::vector<LinkedPlotCommandOption>> exportGroups =
        PlotExportCommandOptionGroups(false);
    for (const auto &group : exportGroups) {
        for (const LinkedPlotCommandOption &option : group) {
            if (option.command == "SAVE_PNG") state.savePngTitle = option.title;
            if (option.command == "SAVE_PDF") state.savePdfTitle = option.title;
            if (option.command == "COPY_PNG") state.copyPngTitle = option.title;
            if (option.command == "COPY_PDF") state.copyPdfTitle = option.title;
        }
    }
    return state;
}

std::vector<std::string> DendrogramVariableCaptionLines(const std::vector<std::string> &variables, double width)
{
    if (variables.empty()) return {};
    std::string caption="Variables: ";
    for (std::size_t i=0;i<variables.size();++i) {
        if (i) caption += ", ";
        caption += variables[i];
    }
    // Conservative width at 12pt, preserving UTF-8 characters and all variable names.
    const std::size_t limit=static_cast<std::size_t>(std::max(12.0,(width-40.0)/8.0));
    std::vector<std::string> lines;
    while (!caption.empty()) {
        std::size_t end=0, characters=0, separator=0;
        while (end<caption.size() && characters<limit) {
            if (caption[end]==' ') separator=end;
            ++end;
            while (end<caption.size() && (static_cast<unsigned char>(caption[end])&0xc0)==0x80) ++end;
            ++characters;
        }
        if (end<caption.size() && separator>limit/3) end=separator;
        lines.push_back(caption.substr(0,end)); caption.erase(0,end);
        if (!caption.empty() && caption.front()==' ') caption.erase(0,1);
    }
    return lines;
}

DendrogramPlotGeometry BuildDendrogramPlotGeometry(
    const std::vector<int> &caseRows,
    const std::vector<DendrogramMergeModel> &merges,
    const std::vector<int> &leafOrder,
    double boundsWidth,
    double boundsHeight,
    bool verticalFlip,
    const std::map<int, std::string> &rowLabels,
    bool rotate270,
    bool rotateCaseLabels90,
    double variableHeaderHeight)
{
    DendrogramPlotGeometry geometry;
    const std::size_t n = caseRows.size();
    if (n == 0) {
        return geometry;
    }

    const double leftInset = rotate270 ? (rotateCaseLabels90 ? 48.0 : 132.0) : 86.0;
    const double rightInset = 28.0;
    const double topInset = variableHeaderHeight + (rotate270 ? 28.0 : (verticalFlip ? 106.0 : 18.0));
    const double bottomInset = rotate270 ? 28.0 : (verticalFlip ? 18.0 : 106.0);
    geometry.plotRect = Rect{
        leftInset,
        topInset,
        std::max(120.0, boundsWidth - leftInset - rightInset),
        std::max(120.0, boundsHeight - topInset - bottomInset)
    };

    geometry.maxHeight = 0.0;
    for (const DendrogramMergeModel &merge : merges) {
        if (std::isfinite(merge.height)) {
            geometry.maxHeight = std::max(geometry.maxHeight, merge.height);
        }
    }
    if (!(geometry.maxHeight > 0.0)) {
        geometry.maxHeight = 1.0;
    }

    const std::size_t totalNodes = n + merges.size();
    std::vector<double> nodeX(totalNodes, std::numeric_limits<double>::quiet_NaN());
    std::vector<double> nodeY(totalNodes, std::numeric_limits<double>::quiet_NaN());
    std::vector<std::vector<int>> nodeRows(totalNodes);
    std::vector<int> order = leafOrder;
    if (order.size() != n) {
        order.clear();
        order.reserve(n);
        for (std::size_t i = 0; i < n; ++i) {
            order.push_back(static_cast<int>(i));
        }
    }

    const double plotTop = geometry.plotRect.y;
    const double plotBottom = geometry.plotRect.y + geometry.plotRect.height;
    const double leafY = verticalFlip ? plotTop : plotBottom;
    const double step = n > 1 ? (rotate270 ? geometry.plotRect.height : geometry.plotRect.width) / static_cast<double>(n - 1) : 0.0;
    // Compare the label footprint with the axis on which leaves are spaced.
    // A 270-degree tree uses ordinary horizontal text in a vertical list; its
    // 11-point labels need about 14 px vertically.  In the ordinary tree,
    // labels rotated by 90 degrees have the same short, font-height footprint
    // along the horizontal leaf axis.  The scrollable content size provides
    // 16 px per case, so both arrangements can show every case label.  The
    // long side of the text controls thinning only when it lies along the leaf
    // axis (ordinary horizontal labels, or a rotated label in a 270-degree
    // tree).
    const double labelExtent = rotate270
        ? (rotateCaseLabels90 ? 88.0 : 14.0)
        : (rotateCaseLabels90 ? 14.0 : 56.0);
    const std::size_t labelStep = step > 0.0
        ? std::max<std::size_t>(1, static_cast<std::size_t>(std::ceil(labelExtent / step))) : 1;
    geometry.leaves.reserve(n);
    for (std::size_t idx = 0; idx < n; ++idx) {
        int leafIndex = order[idx];
        if (leafIndex < 0 || static_cast<std::size_t>(leafIndex) >= n) {
            continue;
        }
        const double x = rotate270 ? geometry.plotRect.x : (n > 1
            ? geometry.plotRect.x + static_cast<double>(idx) * step
            : geometry.plotRect.x + geometry.plotRect.width * 0.5);
        const double y = rotate270 ? (n > 1
            ? geometry.plotRect.y + static_cast<double>(idx) * step
            : geometry.plotRect.y + geometry.plotRect.height * 0.5) : leafY;
        nodeX[static_cast<std::size_t>(leafIndex)] = x;
        nodeY[static_cast<std::size_t>(leafIndex)] = y;
        int rowId = caseRows[static_cast<std::size_t>(leafIndex)];
        nodeRows[static_cast<std::size_t>(leafIndex)] = {rowId};
        bool showLabel = idx % labelStep == 0;
        auto label = rowLabels.find(rowId);
        const std::string text = label != rowLabels.end() && !label->second.empty()
            ? label->second : std::to_string(rowId);
        geometry.leaves.push_back(DendrogramLeafGeometry{
            rowId,
            leafIndex,
            static_cast<int>(idx),
            Point{x, y},
            rotate270
                ? (rotateCaseLabels90 ? Rect{x - 66.0, y - 10.0, 88.0, 20.0}
                                      : Rect{x - 126.0, y - 10.0, 116.0, 20.0})
                : (rotateCaseLabels90 ? Rect{x - 44.0, leafY + (verticalFlip ? -66.0 : 46.0), 88.0, 20.0}
                                      : Rect{x - 44.0, leafY + (verticalFlip ? -32.0 : 12.0), 88.0, 20.0}),
            text,
            showLabel,
            rotateCaseLabels90
        });
    }

    geometry.branches.reserve(merges.size() * 3);
    for (std::size_t mergeIndex = 0; mergeIndex < merges.size(); ++mergeIndex) {
        const DendrogramMergeModel &merge = merges[mergeIndex];
        if (merge.left < 0 || merge.right < 0) continue;
        if (static_cast<std::size_t>(merge.left) >= totalNodes ||
            static_cast<std::size_t>(merge.right) >= totalNodes) {
            continue;
        }
        double leftX = nodeX[static_cast<std::size_t>(merge.left)];
        double rightX = nodeX[static_cast<std::size_t>(merge.right)];
        double leftY = nodeY[static_cast<std::size_t>(merge.left)];
        double rightY = nodeY[static_cast<std::size_t>(merge.right)];
        if (!std::isfinite(leftX) || !std::isfinite(rightX) ||
            !std::isfinite(leftY) || !std::isfinite(rightY)) {
            continue;
        }
        const double ratio = (std::max(0.0, merge.height) / geometry.maxHeight);
        double x = rotate270 ? geometry.plotRect.x + ratio * geometry.plotRect.width : (leftX + rightX) * 0.5;
        double y = rotate270 ? (leftY + rightY) * 0.5 : (verticalFlip
            ? plotTop + ratio * geometry.plotRect.height
            : plotBottom - ratio * geometry.plotRect.height);
        std::size_t nodeIndex = n + mergeIndex;
        nodeX[nodeIndex] = x;
        nodeY[nodeIndex] = y;
        nodeRows[nodeIndex] = nodeRows[static_cast<std::size_t>(merge.left)];
        nodeRows[nodeIndex].insert(nodeRows[nodeIndex].end(),
            nodeRows[static_cast<std::size_t>(merge.right)].begin(),
            nodeRows[static_cast<std::size_t>(merge.right)].end());
        if (rotate270) {
            geometry.joins.push_back(DendrogramJoinGeometry{static_cast<int>(mergeIndex), Point{x, y}, Point{x, std::min(leftY, rightY)}, Point{x, std::max(leftY, rightY)}, nodeRows[nodeIndex]});
            geometry.branches.push_back(DendrogramBranchSegment{Point{leftX, leftY}, Point{x, leftY}, nodeRows[static_cast<std::size_t>(merge.left)]});
            geometry.branches.push_back(DendrogramBranchSegment{Point{rightX, rightY}, Point{x, rightY}, nodeRows[static_cast<std::size_t>(merge.right)]});
            geometry.branches.push_back(DendrogramBranchSegment{Point{x, std::min(leftY, rightY)}, Point{x, std::max(leftY, rightY)}, nodeRows[nodeIndex]});
        } else {
            geometry.joins.push_back(DendrogramJoinGeometry{static_cast<int>(mergeIndex), Point{nodeX[nodeIndex], y}, Point{std::min(leftX, rightX), y}, Point{std::max(leftX, rightX), y}, nodeRows[nodeIndex]});
            geometry.branches.push_back(DendrogramBranchSegment{Point{leftX, leftY}, Point{leftX, y}, nodeRows[static_cast<std::size_t>(merge.left)]});
            geometry.branches.push_back(DendrogramBranchSegment{Point{rightX, rightY}, Point{rightX, y}, nodeRows[static_cast<std::size_t>(merge.right)]});
            geometry.branches.push_back(DendrogramBranchSegment{Point{std::min(leftX, rightX), y}, Point{std::max(leftX, rightX), y}, nodeRows[nodeIndex]});
        }
    }

    geometry.hasCases = !geometry.leaves.empty();
    return geometry;
}

int DendrogramNearestLeafRowAtPoint(const DendrogramPlotGeometry &geometry,
                                    const Point &point,
                                    double maxDistance)
{
    std::vector<IndexedScreenPoint> candidates;
    candidates.reserve(geometry.leaves.size());
    for (const DendrogramLeafGeometry &leaf : geometry.leaves) {
        candidates.push_back(IndexedScreenPoint{leaf.rowId, leaf.point});
    }
    return HitNearestIndexedPoint(point, candidates, maxDistance);
}

std::vector<int> DendrogramLeafRowsInRect(const DendrogramPlotGeometry &geometry,
                                          const Rect &rect)
{
    std::vector<int> rows;
    for (const DendrogramLeafGeometry &leaf : geometry.leaves) {
        if (PointInRect(leaf.point, rect)) {
            rows.push_back(leaf.rowId);
        }
    }
    return rows;
}

std::vector<int> DendrogramJoinRowsAtPoint(const DendrogramPlotGeometry &geometry,
                                           const Point &point,
                                           double maxDistance)
{
    const DendrogramJoinGeometry *best = nullptr;
    double bestDistance = maxDistance * maxDistance;
    for (const DendrogramJoinGeometry &join : geometry.joins) {
        const double dx = join.point.x - point.x;
        const double dy = join.point.y - point.y;
        double distance = dx * dx + dy * dy;
        const double segmentDx = join.segmentEnd.x - join.segmentStart.x;
        const double segmentDy = join.segmentEnd.y - join.segmentStart.y;
        const double lengthSquared = segmentDx * segmentDx + segmentDy * segmentDy;
        if (lengthSquared > 0.0) {
            const double projection = std::clamp(
                ((point.x - join.segmentStart.x) * segmentDx +
                 (point.y - join.segmentStart.y) * segmentDy) / lengthSquared,
                0.0, 1.0);
            const double nearestX = join.segmentStart.x + projection * segmentDx;
            const double nearestY = join.segmentStart.y + projection * segmentDy;
            const double sx = nearestX - point.x;
            const double sy = nearestY - point.y;
            distance = std::min(distance, sx * sx + sy * sy);
        }
        if (distance <= bestDistance) {
            bestDistance = distance;
            best = &join;
        }
    }
    return best ? best->rows : std::vector<int>{};
}

bool DendrogramBranchIsFullySelected(const DendrogramBranchSegment &branch,
                                     const std::set<int> &selectedRows)
{
    return !branch.rows.empty() && std::all_of(branch.rows.begin(), branch.rows.end(),
        [&selectedRows](int row) { return selectedRows.count(row) > 0; });
}

std::string DendrogramUniformBranchColor(
    const DendrogramBranchSegment &branch,
    const std::map<int, std::string> &manualRowColors,
    const std::map<int, std::string> &groupedRowColors)
{
    std::string uniform;
    for (int row : branch.rows) {
        std::string color;
        auto manual = manualRowColors.find(row);
        if (manual != manualRowColors.end()) color = manual->second;
        else {
            auto grouped = groupedRowColors.find(row);
            if (grouped != groupedRowColors.end()) color = grouped->second;
        }
        if (color.empty()) return {};
        if (uniform.empty()) uniform = color;
        else if (uniform != color) return {};
    }
    return uniform;
}

DendrogramSelectionGestureResult DendrogramSelectionRowsForGesture(
    const DendrogramPlotGeometry &geometry,
    const Rect &brush,
    const Point &clickPoint,
    double minimumBrushSize,
    double maxClickDistance)
{
    DendrogramSelectionGestureResult result;
    if (brush.width >= minimumBrushSize || brush.height >= minimumBrushSize) {
        result.usedBrush = true;
        Rect hitRect = brush;
        if (hitRect.width < minimumBrushSize) {
            hitRect.x -= maxClickDistance;
            hitRect.width += maxClickDistance * 2.0;
        }
        if (hitRect.height < minimumBrushSize) {
            hitRect.y -= maxClickDistance;
            hitRect.height += maxClickDistance * 2.0;
        }
        result.rows = DendrogramLeafRowsInRect(geometry, hitRect);
        return result;
    }
    result.rows = DendrogramJoinRowsAtPoint(geometry, clickPoint, maxClickDistance);
    if (!result.rows.empty()) return result;
    const int row = DendrogramNearestLeafRowAtPoint(geometry, clickPoint, maxClickDistance);
    if (row > 0) {
        result.rows.push_back(row);
    }
    return result;
}

bool SetDendrogramLabelVariable(DendrogramState &state,
                                const DataFrameModel &dataframe,
                                const std::string &variable,
                                std::string *error)
{
    state.labelVariable = variable == "." ? std::string{} : variable;
    state.rowLabels.clear();
    if (state.labelVariable.empty()) return true;
    const DataColumn *column = FindDataColumnInDataFrame(dataframe, state.labelVariable);
    if (!column) {
        if (error) *error = "The selected label variable is unavailable.";
        state.labelVariable.clear();
        return false;
    }
    state.rowLabels = RowLabelMapForColumn(*column);
    return true;
}

bool SetDendrogramColorByVariable(DendrogramState &state,
                                  const DataFrameModel &dataframe,
                                  const std::string &variable,
                                  std::string *error)
{
    state.colorByVariable = variable == "." ? std::string{} : variable;
    state.colorByRowColors.clear();
    state.colorByLegendItems.clear();
    state.colorByLegendRows.clear();
    if (state.colorByVariable.empty()) return true;
    const DataColumn *column = FindDataColumnInDataFrame(dataframe, state.colorByVariable);
    if (!column) {
        if (error) *error = "The selected Color by variable is unavailable.";
        state.colorByVariable.clear();
        return false;
    }
    std::vector<std::string> levels;
    for (const std::string &level : column->definedLevels)
        if (!DataCellIsMissing(level) &&
            std::find(levels.begin(), levels.end(), level) == levels.end()) levels.push_back(level);
    for (std::size_t row = 0; row < column->values.size(); ++row) {
        const std::string label = DisplayValueForCell(*column, row);
        if (!DataCellIsMissing(label) &&
            std::find(levels.begin(), levels.end(), label) == levels.end()) levels.push_back(label);
    }
    const auto palette = BarplotPaletteOrder();
    if (levels.empty() || palette.empty()) {
        if (error) *error = "The selected Color by variable has no displayable categories.";
        return false;
    }
    std::map<std::string, std::string> colors;
    for (std::size_t index = 0; index < levels.size(); ++index) {
        colors[levels[index]] = palette[index % palette.size()];
        state.colorByLegendItems.push_back({levels[index], colors[levels[index]]});
    }
    for (std::size_t row = 0; row < column->values.size(); ++row) {
        const std::string label = DisplayValueForCell(*column, row);
        auto color = colors.find(label);
        if (color != colors.end()) {
            const int caseId = static_cast<int>(row + 1);
            state.colorByRowColors[caseId] = color->second;
            state.colorByLegendRows[label].push_back(caseId);
        }
    }
    return true;
}

std::set<int> DendrogramColorLegendRows(const DendrogramState &state,
                                        const std::string &level)
{
    std::set<int> rows;
    auto semantic = state.colorByLegendRows.find(level);
    if (semantic != state.colorByLegendRows.end()) {
        for (int row : semantic->second) if (row > 0) rows.insert(row);
    } else {
        // Backward compatibility for state snapshots created before semantic
        // legend membership was stored explicitly.
        std::string color;
        for (const auto &item : state.colorByLegendItems) {
            if (item.first == level) {
                color = item.second;
                break;
            }
        }
        if (!color.empty()) {
            for (const auto &[row, assignedColor] : state.colorByRowColors)
                if (row > 0 && assignedColor == color) rows.insert(row);
        }
    }

    // A dendrogram can omit cases through its analysis scope or missing-data
    // policy. Legend clicks must select the cases actually represented here.
    if (!state.caseRows.empty()) {
        const std::set<int> displayed(state.caseRows.begin(), state.caseRows.end());
        for (auto it = rows.begin(); it != rows.end();) {
            if (!displayed.count(*it)) it = rows.erase(it);
            else ++it;
        }
    }
    return rows;
}

bool DendrogramColorLegendMatchesLinkedRowColors(
    const DendrogramState &state,
    const std::map<int, std::string> &linkedRowColors)
{
    if (state.colorByVariable.empty() || state.colorByLegendItems.empty()) return true;
    for (const auto &[row, linkedColor] : linkedRowColors) {
        const auto semantic = state.colorByRowColors.find(row);
        if (semantic != state.colorByRowColors.end() && semantic->second != linkedColor) {
            return false;
        }
    }
    return true;
}

bool ReadDendrogramRTree(const std::vector<std::string> &payload, std::size_t &cursor,
                        DendrogramFitResult &result, std::string &error)
{
    auto integer = [&](int &value) {
        if (cursor >= payload.size()) return false;
        const auto &text = payload[cursor++];
        char *end = nullptr;
        const long parsed = std::strtol(text.c_str(), &end, 10);
        if (text.empty() || !end || *end || parsed < 0 || parsed > 2147483647L) return false;
        value = static_cast<int>(parsed); return true;
    };
    DendrogramFitResult tree;
    int n = 0, count = 0;
    if (!integer(n) || static_cast<std::size_t>(n) > payload.size() - cursor) {
        error = "Invalid clustering case count"; return false;
    }
    std::set<int> uniqueRows;
    for (int i = 0; i < n; ++i) {
        int row = 0;
        if (!integer(row) || row == 0 || !uniqueRows.insert(row).second) {
            error = "Invalid clustering case identity"; return false;
        }
        tree.caseRows.push_back(row);
    }
    if (!integer(count) || count != n) { error = "Invalid clustering leaf count"; return false; }
    std::set<int> uniqueLeaves;
    for (int i = 0; i < n; ++i) {
        int leaf = 0;
        if (!integer(leaf) || leaf >= n || !uniqueLeaves.insert(leaf).second) {
            error = "Invalid clustering leaf order"; return false;
        }
        tree.leafOrder.push_back(leaf);
    }
    if (!integer(count) || count != std::max(0, n - 1)) {
        error = "Invalid clustering merge count"; return false;
    }
    std::vector<int> sizes(static_cast<std::size_t>(n), 1);
    std::set<int> children;
    for (int i = 0; i < count; ++i) {
        DendrogramMergeModel merge;
        if (!integer(merge.left) || !integer(merge.right) ||
            merge.left >= n + i || merge.right >= n + i ||
            !children.insert(merge.left).second || !children.insert(merge.right).second ||
            cursor >= payload.size()) {
            error = "Invalid clustering tree topology"; return false;
        }
        const auto &height = payload[cursor++];
        char *end = nullptr;
        merge.height = std::strtod(height.c_str(), &end);
        if (height.empty() || !end || *end || !std::isfinite(merge.height) || merge.height < 0) {
            error = "Invalid R clustering height"; return false;
        }
        merge.size = sizes[merge.left] + sizes[merge.right];
        sizes.push_back(merge.size);
        tree.merges.push_back(merge);
    }
    result = std::move(tree);
    return true;
}

std::string DendrogramAverageLinkageTitle()
{
    return "average";
}

std::string DendrogramCompleteLinkageTitle()
{
    return "complete";
}

std::string DendrogramSingleLinkageTitle()
{
    return "single";
}

std::string DendrogramPDFOptionIdentifier()
{
    return "PDF";
}

std::string DendrogramPNGOptionIdentifier()
{
    return "PNG";
}

std::string DendrogramCopyWindowTitle()
{
    return "Copy Dendrogram";
}

size_t DendrogramRowCountForVariables(PlotModel *model,
                                      const std::vector<std::string> &variables,
                                      std::vector<NumericVariable *> &vars)
{
    vars.clear();
    if (!model || variables.empty()) return 0;
    size_t rowCount = (size_t)-1;
    for (const std::string &name : variables) {
        NumericVariable *var = FindNumericVariable(*model, name);
        if (!var) {
            vars.clear();
            return 0;
        }
        vars.push_back(var);
        rowCount = std::min(rowCount, var->values.size());
    }
    if (rowCount == (size_t)-1) return 0;
    return rowCount;
}

} // namespace core
} // namespace rlispstat
