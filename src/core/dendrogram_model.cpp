#include "dendrogram_model.h"

#include "command_model.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <sstream>

namespace rlispstat {
namespace core {

namespace {

double ClusterDistance(const std::vector<int> &leftLeaves,
                       const std::vector<int> &rightLeaves,
                       const std::vector<std::vector<double>> &leafDistances,
                       const std::string &linkage)
{
    if (leftLeaves.empty() || rightLeaves.empty()) return 1.0;
    double value = linkage == "complete" ? 0.0 : 1.0e300;
    double sum = 0.0;
    std::size_t count = 0;
    for (int li : leftLeaves) {
        for (int ri : rightLeaves) {
            if (li < 0 || ri < 0 ||
                static_cast<std::size_t>(li) >= leafDistances.size() ||
                static_cast<std::size_t>(ri) >= leafDistances.size()) {
                continue;
            }
            double d = leafDistances[static_cast<std::size_t>(li)][static_cast<std::size_t>(ri)];
            if (linkage == "single") {
                value = std::min(value, d);
            } else if (linkage == "complete") {
                value = std::max(value, d);
            } else {
                sum += d;
                count += 1;
            }
        }
    }
    if (linkage == "average") {
        return count > 0 ? sum / static_cast<double>(count) : 1.0;
    }
    if (value >= 1.0e299) return 1.0;
    return value;
}

void CollectLeafOrder(int nodeId,
                      int leafCount,
                      const std::vector<DendrogramMergeModel> &merges,
                      std::vector<int> &order)
{
    if (nodeId < 0) return;
    if (nodeId < leafCount) {
        order.push_back(nodeId);
        return;
    }
    int mergeIndex = nodeId - leafCount;
    if (mergeIndex < 0 || static_cast<std::size_t>(mergeIndex) >= merges.size()) return;
    const DendrogramMergeModel &merge = merges[static_cast<std::size_t>(mergeIndex)];
    CollectLeafOrder(merge.left, leafCount, merges, order);
    CollectLeafOrder(merge.right, leafCount, merges, order);
}

} // namespace

bool DendrogramLinkageIsValid(const std::string &linkage)
{
    return linkage == "average" || linkage == "complete" || linkage == "single";
}

bool DendrogramDistanceIsValid(const std::string &distance)
{
    return distance == "euclidean" || distance == "correlation";
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

std::string DendrogramColorSelectedCasesTitle(const std::string &color)
{
    return "Color selected cases: " + color;
}

std::string DendrogramVariablesButtonTitle(std::size_t variableCount)
{
    return "Variables (" + std::to_string(variableCount) + ")...";
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
    layout.variablesButtonRect = Rect{std::max(620.0, w - 190.0), h - 74.0, 170.0, 24.0};
    layout.scrollViewRect = Rect{
        12.0,
        44.0,
        std::max(200.0, w - 24.0),
        std::max(140.0, h - 124.0)
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

DendrogramPlotGeometry BuildDendrogramPlotGeometry(
    const std::vector<int> &caseRows,
    const std::vector<DendrogramMergeModel> &merges,
    const std::vector<int> &leafOrder,
    double boundsWidth,
    double boundsHeight)
{
    DendrogramPlotGeometry geometry;
    const std::size_t n = caseRows.size();
    if (n == 0) {
        return geometry;
    }

    constexpr double leftInset = 86.0;
    constexpr double rightInset = 28.0;
    constexpr double topInset = 18.0;
    constexpr double bottomInset = 106.0;
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
    std::vector<int> order = leafOrder;
    if (order.size() != n) {
        order.clear();
        order.reserve(n);
        for (std::size_t i = 0; i < n; ++i) {
            order.push_back(static_cast<int>(i));
        }
    }

    const double plotBottom = geometry.plotRect.y + geometry.plotRect.height;
    const double step = n > 1 ? geometry.plotRect.width / static_cast<double>(n - 1) : 0.0;
    const std::size_t labelStep = std::max<std::size_t>(1, n / 60);
    geometry.leaves.reserve(n);
    for (std::size_t idx = 0; idx < n; ++idx) {
        int leafIndex = order[idx];
        if (leafIndex < 0 || static_cast<std::size_t>(leafIndex) >= n) {
            continue;
        }
        const double x = n > 1
            ? geometry.plotRect.x + static_cast<double>(idx) * step
            : geometry.plotRect.x + geometry.plotRect.width * 0.5;
        nodeX[static_cast<std::size_t>(leafIndex)] = x;
        nodeY[static_cast<std::size_t>(leafIndex)] = plotBottom;
        int rowId = caseRows[static_cast<std::size_t>(leafIndex)];
        bool showLabel = idx % labelStep == 0 || idx == n - 1;
        geometry.leaves.push_back(DendrogramLeafGeometry{
            rowId,
            leafIndex,
            static_cast<int>(idx),
            Point{x, plotBottom + 18.0},
            Rect{x - 44.0, plotBottom + 30.0, 88.0, 20.0},
            std::to_string(rowId),
            showLabel
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
        double y = plotBottom -
            (std::max(0.0, merge.height) / geometry.maxHeight) * geometry.plotRect.height;
        std::size_t nodeIndex = n + mergeIndex;
        nodeX[nodeIndex] = (leftX + rightX) * 0.5;
        nodeY[nodeIndex] = y;

        geometry.branches.push_back(DendrogramBranchSegment{Point{leftX, leftY}, Point{leftX, y}});
        geometry.branches.push_back(DendrogramBranchSegment{Point{rightX, rightY}, Point{rightX, y}});
        geometry.branches.push_back(DendrogramBranchSegment{
            Point{std::min(leftX, rightX), y},
            Point{std::max(leftX, rightX), y}
        });
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

DendrogramSelectionGestureResult DendrogramSelectionRowsForGesture(
    const DendrogramPlotGeometry &geometry,
    const Rect &brush,
    const Point &clickPoint,
    double minimumBrushSize,
    double maxClickDistance)
{
    DendrogramSelectionGestureResult result;
    if (brush.width >= minimumBrushSize && brush.height >= minimumBrushSize) {
        result.usedBrush = true;
        result.rows = DendrogramLeafRowsInRect(geometry, brush);
        return result;
    }
    const int row = DendrogramNearestLeafRowAtPoint(geometry, clickPoint, maxClickDistance);
    if (row > 0) {
        result.rows.push_back(row);
    }
    return result;
}

DendrogramFitResult FitDendrogram(const DendrogramFitInput &input)
{
    DendrogramFitResult result;
    result.distance = DendrogramDistanceIsValid(input.distance) ? input.distance : "euclidean";
    result.linkage = DendrogramLinkageIsValid(input.linkage) ? input.linkage : "average";
    result.missingMode = input.missingMode == "listwise" ? "listwise" : "pairwise";

    if (input.variables.empty() || input.columns.empty()) return result;
    if (input.columns.size() != input.variables.size()) return result;
    std::size_t rowCount = input.columns[0].size();
    if (rowCount == 0) return result;
    for (const std::vector<double> &column : input.columns) {
        if (column.size() != rowCount) return result;
    }

    std::vector<double> means(input.columns.size(), 0.0);
    std::vector<double> sds(input.columns.size(), 0.0);
    for (std::size_t j = 0; j < input.columns.size(); ++j) {
        double sum = 0.0;
        std::size_t count = 0;
        for (std::size_t i = 0; i < rowCount; ++i) {
            double v = input.columns[j][i];
            if (!std::isfinite(v)) continue;
            sum += v;
            count += 1;
        }
        if (count == 0) {
            means[j] = 0.0;
            sds[j] = 0.0;
            continue;
        }
        means[j] = sum / static_cast<double>(count);
        if (count < 2) {
            sds[j] = 0.0;
            continue;
        }
        double ss = 0.0;
        for (std::size_t i = 0; i < rowCount; ++i) {
            double v = input.columns[j][i];
            if (!std::isfinite(v)) continue;
            double d = v - means[j];
            ss += d * d;
        }
        sds[j] = std::sqrt(ss / static_cast<double>(count - 1));
        if (!std::isfinite(sds[j]) || sds[j] <= 1.0e-12) {
            sds[j] = 0.0;
        }
    }

    std::vector<std::vector<double>> zValues(input.columns.size(), std::vector<double>(rowCount, std::numeric_limits<double>::quiet_NaN()));
    for (std::size_t j = 0; j < input.columns.size(); ++j) {
        for (std::size_t i = 0; i < rowCount; ++i) {
            double v = input.columns[j][i];
            if (!std::isfinite(v)) continue;
            if (sds[j] > 0.0) {
                zValues[j][i] = (v - means[j]) / sds[j];
            } else {
                zValues[j][i] = 0.0;
            }
        }
    }

    for (std::size_t i = 0; i < rowCount; ++i) {
        if (input.restrictRows &&
            std::find(input.includedRows.begin(), input.includedRows.end(),
                      static_cast<int>(i) + 1) == input.includedRows.end()) {
            continue;
        }
        bool anyFinite = false;
        bool allFinite = true;
        for (std::size_t j = 0; j < input.columns.size(); ++j) {
            bool finite = std::isfinite(zValues[j][i]);
            anyFinite = anyFinite || finite;
            allFinite = allFinite && finite;
        }
        if (result.missingMode == "listwise") {
            if (allFinite) result.caseRows.push_back(static_cast<int>(i) + 1);
        } else if (anyFinite) {
            result.caseRows.push_back(static_cast<int>(i) + 1);
        }
    }

    std::size_t n = result.caseRows.size();
    if (n == 0) return result;

    std::vector<std::vector<double>> leafDistances(n, std::vector<double>(n, 0.0));
    for (std::size_t i = 0; i < n; ++i) {
        for (std::size_t j = i + 1; j < n; ++j) {
            std::size_t leftRow = static_cast<std::size_t>(result.caseRows[i]) - 1;
            std::size_t rightRow = static_cast<std::size_t>(result.caseRows[j]) - 1;
            double sumSq = 0.0;
            std::size_t count = 0;
            for (std::size_t col = 0; col < zValues.size(); ++col) {
                double a = zValues[col][leftRow];
                double b = zValues[col][rightRow];
                if (!std::isfinite(a) || !std::isfinite(b)) continue;
                double diff = a - b;
                sumSq += diff * diff;
                count += 1;
            }
            double d = count > 0 ? std::sqrt(sumSq / static_cast<double>(count)) : 1.0;
            leafDistances[i][j] = d;
            leafDistances[j][i] = d;
        }
    }

    struct WorkingCluster {
        int nodeId = -1;
        int minLeaf = -1;
        bool active = true;
        std::vector<int> leaves;
    };

    std::vector<WorkingCluster> clusters;
    clusters.reserve(n * 2);
    for (std::size_t i = 0; i < n; ++i) {
        WorkingCluster cluster;
        cluster.nodeId = static_cast<int>(i);
        cluster.minLeaf = static_cast<int>(i);
        cluster.leaves.push_back(static_cast<int>(i));
        clusters.push_back(cluster);
    }

    int nextNodeId = static_cast<int>(n);
    int activeCount = static_cast<int>(n);
    while (activeCount > 1) {
        int bestLeft = -1;
        int bestRight = -1;
        double bestDistance = 1.0e300;
        for (std::size_t i = 0; i < clusters.size(); ++i) {
            if (!clusters[i].active) continue;
            for (std::size_t j = i + 1; j < clusters.size(); ++j) {
                if (!clusters[j].active) continue;
                double d = ClusterDistance(clusters[i].leaves, clusters[j].leaves,
                                           leafDistances, result.linkage);
                bool better = d < bestDistance - 1.0e-12;
                if (!better && std::fabs(d - bestDistance) <= 1.0e-12 && bestLeft >= 0 && bestRight >= 0) {
                    int currentMin = std::min(clusters[static_cast<std::size_t>(bestLeft)].minLeaf,
                                              clusters[static_cast<std::size_t>(bestRight)].minLeaf);
                    int candidateMin = std::min(clusters[i].minLeaf, clusters[j].minLeaf);
                    better = candidateMin < currentMin;
                }
                if (better) {
                    bestDistance = d;
                    bestLeft = static_cast<int>(i);
                    bestRight = static_cast<int>(j);
                }
            }
        }
        if (bestLeft < 0 || bestRight < 0) break;
        const WorkingCluster &leftCluster = clusters[static_cast<std::size_t>(bestLeft)];
        const WorkingCluster &rightCluster = clusters[static_cast<std::size_t>(bestRight)];

        int leftNodeId = leftCluster.nodeId;
        int rightNodeId = rightCluster.nodeId;
        int leftMinLeaf = leftCluster.minLeaf;
        int rightMinLeaf = rightCluster.minLeaf;
        if (leftMinLeaf > rightMinLeaf) {
            std::swap(leftNodeId, rightNodeId);
            std::swap(leftMinLeaf, rightMinLeaf);
        }

        DendrogramMergeModel merge;
        merge.left = leftNodeId;
        merge.right = rightNodeId;
        merge.height = std::isfinite(bestDistance) ? bestDistance : 1.0;
        merge.size = static_cast<int>(leftCluster.leaves.size() + rightCluster.leaves.size());
        result.merges.push_back(merge);

        WorkingCluster merged;
        merged.nodeId = nextNodeId++;
        merged.minLeaf = std::min(leftCluster.minLeaf, rightCluster.minLeaf);
        merged.leaves = leftCluster.leaves;
        merged.leaves.insert(merged.leaves.end(), rightCluster.leaves.begin(), rightCluster.leaves.end());

        clusters[static_cast<std::size_t>(bestLeft)].active = false;
        clusters[static_cast<std::size_t>(bestRight)].active = false;
        activeCount -= 2;
        clusters.push_back(merged);
        activeCount += 1;
    }

    if (n == 1) {
        result.leafOrder.push_back(0);
    } else if (!result.merges.empty()) {
        int rootNodeId = static_cast<int>(n) + static_cast<int>(result.merges.size()) - 1;
        CollectLeafOrder(rootNodeId, static_cast<int>(n), result.merges, result.leafOrder);
    }
    if (result.leafOrder.size() != n) {
        result.leafOrder.clear();
        for (std::size_t i = 0; i < n; ++i) result.leafOrder.push_back(static_cast<int>(i));
    }

    return result;
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
