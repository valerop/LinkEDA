#include "correlation_model.h"

#include "format_model.h"
#include "statistics_model.h"
#include "string_utils.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <sstream>

namespace rlispstat {
namespace core {

namespace {

const DataColumn *CorrelationDataColumn(const DataFrameModel &df, const std::string &name)
{
    return FindDataColumnInDataFrame(df, name);
}

double CorrelationDataValue(const DataFrameModel &df, const DataColumn &col,
                            std::size_t row, int versionIndex)
{
    return NumericValueForDataFrameCellVersion(df, col, row, versionIndex);
}

std::string JoinUniqueStatuses(const std::vector<std::string> &statuses)
{
    std::vector<std::string> unique;
    for (const std::string &status : statuses) {
        if (status.empty()) continue;
        if (std::find(unique.begin(), unique.end(), status) == unique.end()) {
            unique.push_back(status);
        }
    }
    if (unique.empty()) return "invalid";
    std::ostringstream out;
    for (std::size_t i = 0; i < unique.size(); ++i) {
        if (i) out << ",";
        out << unique[i];
    }
    return out.str();
}

} // namespace

std::string CorrelationCellLabel(const CorrelationCellResult *cell,
                                 bool showP,
                                 bool showPValue)
{
    if (!cell || cell->status == "diagonal" || !std::isfinite(cell->r)) return "\u2014";
    std::string label = FormatCorrelation(cell->r);
    if (showP) {
        std::string stars = SignificanceStars(cell->p);
        if (!stars.empty()) label += stars;
    }
    if (showPValue && std::isfinite(cell->p)) {
        label += "\n" + std::string("p=") + FormatPValue(cell->p);
    }
    return label;
}

std::string CorrelationNoMoreNumericVariablesTitle()
{
    return "No more numeric variables";
}

std::string CorrelationAddVariableMenuTitle()
{
    return "Add variable";
}

std::string CorrelationVariableMenuTitle()
{
    return "Variable";
}

std::string CorrelationRemoveVariableTitle(const std::string &name)
{
    return "Remove " + name;
}

std::string CorrelationPooledAddVariableLockedTitle()
{
    return "Pooled MI matrices must be refit from R to add variables";
}

std::string CorrelationPooledVariableEditLockedTitle()
{
    return "Pooled MI matrix: refit from R to add/remove variables";
}

std::string CorrelationPooledAddVariableStatus()
{
    return "This pooled MI matrix must be refit from R to add variables.";
}

std::string CorrelationPooledRemoveVariableStatus()
{
    return "This pooled MI matrix must be refit from R to remove variables.";
}

std::string CorrelationPooledVariableTypeStatus()
{
    return "Variable type changes must be applied in R before refitting this pooled MI matrix.";
}

std::string CorrelationMissingModeLockedStatus()
{
    return "This matrix contains pooled precomputed results; missing-data mode cannot be changed here.";
}

std::string CorrelationCellStatusText(const CorrelationCellResult *cell,
                                      int row,
                                      int column)
{
    if (!cell || row == column) return "";
    std::string text = cell->yVariable + " x " + cell->xVariable + ": r = " +
        FormatCorrelation(cell->r) + "; p = " + FormatPValue(cell->p) +
        "; N = " + std::to_string(cell->n);
    if (!cell->detail.empty()) text += "; " + cell->detail;
    return text;
}

std::string CorrelationCellCopyDetailText(const CorrelationCellResult *cell)
{
    if (!cell) return "";
    std::string text = cell->yVariable + " x " + cell->xVariable + "\n";
    text += "r = " + FormatCorrelation(cell->r) + "; p = " + FormatPValue(cell->p) +
        "; N = " + std::to_string(cell->n);
    if (!cell->detail.empty()) text += "\n" + cell->detail;
    return text;
}

double CorrelationMatrixRowHeight(bool showPValue, bool showN)
{
    const double topInset = 5.0;
    const double bottomInset = 3.0;
    const double mainHeight = 13.0;
    const double mutedHeight = 13.0;
    double height = topInset + mainHeight + bottomInset;
    if (showPValue) {
        const double mainToDetailGap = showN ? 4.0 : 3.0;
        height += mainToDetailGap + mutedHeight;
    }
    if (showN) {
        const double detailToNGap = showPValue ? 6.0 : 5.0;
        height += detailToNGap + mutedHeight;
    }
    return std::max(32.0, height);
}

CorrelationMatrixLayout BuildCorrelationMatrixLayout(std::size_t variableCount,
                                                     bool showPValue,
                                                     bool showN)
{
    CorrelationMatrixLayout layout;
    layout.rowHeight = CorrelationMatrixRowHeight(showPValue, showN);

    const std::size_t effectiveCount = std::max<std::size_t>(1, variableCount);
    layout.width = std::max(720.0,
                            layout.left + layout.rowHeaderWidth +
                                static_cast<double>(effectiveCount) * layout.cellWidth + 24.0);
    const double addVariableY = layout.top + layout.headerHeight +
        static_cast<double>(effectiveCount) * layout.rowHeight + 14.0;
    layout.height = std::max(360.0, addVariableY + 42.0);
    layout.addVariableRect = Rect{layout.left, addVariableY, 220.0, 22.0};
    layout.emptyStatusRect = Rect{layout.left, layout.top + 32.0, 360.0, 20.0};

    layout.columnHeaderRects.reserve(variableCount);
    layout.rowHeaderRects.reserve(variableCount);
    layout.cellRects.reserve(variableCount);
    for (std::size_t i = 0; i < variableCount; ++i) {
        layout.columnHeaderRects.push_back(Rect{
            layout.left + layout.rowHeaderWidth + static_cast<double>(i) * layout.cellWidth,
            layout.top,
            layout.cellWidth,
            layout.headerHeight
        });
        layout.rowHeaderRects.push_back(Rect{
            layout.left,
            layout.top + layout.headerHeight + static_cast<double>(i) * layout.rowHeight,
            layout.rowHeaderWidth,
            layout.rowHeight
        });
        std::vector<Rect> rowRects;
        rowRects.reserve(variableCount);
        for (std::size_t j = 0; j < variableCount; ++j) {
            rowRects.push_back(Rect{
                layout.left + layout.rowHeaderWidth + static_cast<double>(j) * layout.cellWidth,
                layout.top + layout.headerHeight + static_cast<double>(i) * layout.rowHeight,
                layout.cellWidth,
                layout.rowHeight
            });
        }
        layout.cellRects.push_back(rowRects);
    }
    return layout;
}

CorrelationSize CorrelationMatrixPreferredContentSize(std::size_t variableCount,
                                                      bool showPValue,
                                                      bool showN)
{
    CorrelationMatrixLayout layout = BuildCorrelationMatrixLayout(variableCount,
                                                                  showPValue,
                                                                  showN);
    return CorrelationSize{layout.width, layout.height};
}

Rect CorrelationMatrixCellRect(const CorrelationMatrixLayout &layout,
                               int row,
                               int column)
{
    if (row < 0 || column < 0) return Rect{};
    const std::size_t r = static_cast<std::size_t>(row);
    const std::size_t c = static_cast<std::size_t>(column);
    if (r >= layout.cellRects.size() || c >= layout.cellRects[r].size()) return Rect{};
    return layout.cellRects[r][c];
}

CorrelationMatrixHit HitTestCorrelationMatrix(const CorrelationMatrixLayout &layout,
                                              const Point &point)
{
    CorrelationMatrixHit hit;
    if (PointInRect(point, layout.addVariableRect)) {
        hit.kind = CorrelationMatrixHitKind::AddVariable;
        return hit;
    }
    for (std::size_t i = 0; i < layout.columnHeaderRects.size(); ++i) {
        if (PointInRect(point, layout.columnHeaderRects[i]) ||
            PointInRect(point, layout.rowHeaderRects[i])) {
            hit.kind = CorrelationMatrixHitKind::Variable;
            hit.variableIndex = static_cast<int>(i);
            return hit;
        }
    }
    for (std::size_t row = 0; row < layout.cellRects.size(); ++row) {
        for (std::size_t column = 0; column < layout.cellRects[row].size(); ++column) {
            if (PointInRect(point, layout.cellRects[row][column])) {
                hit.kind = CorrelationMatrixHitKind::Cell;
                hit.row = static_cast<int>(row);
                hit.column = static_cast<int>(column);
                return hit;
            }
        }
    }
    return hit;
}

static void ApplyCorrelationWindowContentLayout(CorrelationWindowLayout &layout,
                                                double width,
                                                double height)
{
    layout.targetWidth = width;
    layout.targetHeight = height;
    layout.titleRect = Rect{16.0, height - 38.0, std::max(280.0, width - 210.0), 24.0};
    layout.badgeRect = Rect{width - 170.0, height - 36.0, 150.0, 20.0};
    layout.missingLabelRect = Rect{16.0, height - 70.0, 92.0, 22.0};
    layout.missingPopupRect = Rect{108.0, height - 74.0, 126.0, 24.0};
    layout.showPButtonRect = Rect{252.0, height - 70.0, 96.0, 24.0};
    layout.showPValueButtonRect = Rect{360.0, height - 70.0, 118.0, 24.0};
    layout.showNButtonRect = Rect{490.0, height - 70.0, 70.0, 24.0};
    layout.scrollViewRect = Rect{12.0, 44.0, std::max(200.0, width - 24.0),
                                 std::max(140.0, height - 124.0)};
    layout.statusRect = Rect{16.0, 14.0, std::max(200.0, width - 32.0), 20.0};
}

CorrelationWindowLayout BuildCorrelationWindowLayout(double matrixWidth,
                                                     double matrixHeight,
                                                     double visibleWidth,
                                                     double visibleHeight)
{
    CorrelationWindowLayout layout;
    layout.maxWidth = std::max(640.0, visibleWidth * 0.92);
    layout.maxHeight = std::max(420.0, visibleHeight * 0.86);
    layout.targetWidth = std::min(layout.maxWidth, std::max(640.0, matrixWidth + 24.0));
    layout.targetHeight = std::min(layout.maxHeight, std::max(420.0, matrixHeight + 124.0));
    ApplyCorrelationWindowContentLayout(layout, layout.targetWidth, layout.targetHeight);
    return layout;
}

CorrelationWindowLayout BuildCorrelationWindowContentLayout(double contentWidth,
                                                            double contentHeight)
{
    CorrelationWindowLayout layout;
    layout.maxWidth = contentWidth;
    layout.maxHeight = contentHeight;
    ApplyCorrelationWindowContentLayout(layout, contentWidth, contentHeight);
    return layout;
}

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
    const std::string &note)
{
    CorrelationWindowControlState state;
    state.windowTitle = title;
    state.title = title;
    state.badge = group;
    if (multipleImputation && imputationCount > 0) {
        state.badge = "m = " + std::to_string(imputationCount);
    }
    state.missingMode = missingMode;
    state.missingModeEnabled = !precomputed;
    state.showP = showP;
    state.showPValue = showPValue;
    state.showN = showN;
    if ((precomputed || multipleImputation) && !note.empty()) {
        state.status = note;
    } else {
        state.status = group + ": " + std::to_string(variables.size()) +
            " variables, Pearson, " + missingMode + " complete observations";
    }
    return state;
}

Rect CorrelationMatrixDocumentRect(double matrixWidth,
                                   double matrixHeight,
                                   double visibleWidth,
                                   double visibleHeight)
{
    return Rect{0.0, 0.0, std::max(matrixWidth, visibleWidth),
                std::max(matrixHeight, visibleHeight)};
}

static bool CorrelationVariableListContains(const std::vector<std::string> &variables,
                                            const std::string &name)
{
    return std::find(variables.begin(), variables.end(), name) != variables.end();
}

std::vector<std::string> CorrelationVariablesAvailableToAdd(
    const std::vector<std::string> &currentVariables,
    const std::vector<std::string> &availableVariables)
{
    std::vector<std::string> out;
    for (const std::string &name : availableVariables) {
        if (!CorrelationVariableListContains(currentVariables, name) &&
            !CorrelationVariableListContains(out, name)) {
            out.push_back(name);
        }
    }
    return out;
}

CorrelationVariableUpdateResult CorrelationVariablesAfterAdd(
    const std::vector<std::string> &currentVariables,
    const std::string &variable,
    const std::vector<std::string> &availableVariables)
{
    CorrelationVariableUpdateResult result;
    result.variables = currentVariables;
    if (CorrelationVariableListContains(result.variables, variable)) {
        result.status = variable + " is already in the matrix.";
        return result;
    }
    if (!CorrelationVariableListContains(availableVariables, variable)) {
        result.status = variable + " is not available as a numeric variable.";
        return result;
    }
    result.variables.push_back(variable);
    result.ok = true;
    result.changed = true;
    return result;
}

CorrelationVariableUpdateResult CorrelationVariablesAfterRemove(
    const std::vector<std::string> &currentVariables,
    std::size_t index)
{
    CorrelationVariableUpdateResult result;
    result.variables = currentVariables;
    if (index >= result.variables.size()) return result;
    result.variables.erase(result.variables.begin() + static_cast<std::ptrdiff_t>(index));
    result.ok = true;
    result.changed = true;
    return result;
}

CorrelationAddVariableMenuState BuildCorrelationAddVariableMenuState(
    const std::vector<std::string> &currentVariables,
    const std::vector<std::string> &availableVariables,
    bool locked)
{
    CorrelationAddVariableMenuState state;
    state.locked = locked;
    state.emptyTitle = CorrelationNoMoreNumericVariablesTitle();
    state.lockedTitle = CorrelationPooledAddVariableLockedTitle();
    if (!locked) {
        state.variables = CorrelationVariablesAvailableToAdd(currentVariables, availableVariables);
    }
    return state;
}

CorrelationVariableMenuState BuildCorrelationVariableMenuState(
    const std::vector<std::string> &variables,
    std::size_t index,
    bool locked)
{
    CorrelationVariableMenuState state;
    if (index >= variables.size()) return state;
    state.valid = true;
    state.locked = locked;
    state.variable = variables[index];
    state.addVariableTitle = "Add Variable...";
    state.removeVariableTitle = CorrelationRemoveVariableTitle(state.variable);
    state.treatAsNumericTitle = "Treat as Numeric";
    state.treatAsFactorTitle = "Treat as Factor";
    state.informationTitle = "Show Variable Information";
    state.lockedTitle = CorrelationPooledVariableEditLockedTitle();
    return state;
}

CorrelationCellMenuState BuildCorrelationCellMenuState(int row,
                                                       int column)
{
    CorrelationCellMenuState state;
    state.title = "Correlation cell";
    state.openScatterplotTitle = "Open Scatterplot";
    state.canOpenScatterplot = row != column;
    state.copyRTitle = "Copy r";
    state.copyDetailTitle = "Copy r, p, N";
    return state;
}

const CorrelationCellResult *CorrelationCellAt(
    const std::vector<CorrelationCellResult> &cells,
    std::size_t variableCount,
    int row,
    int column)
{
    if (row < 0 || column < 0) return nullptr;
    const std::size_t r = static_cast<std::size_t>(row);
    const std::size_t c = static_cast<std::size_t>(column);
    if (r >= variableCount || c >= variableCount) return nullptr;
    const std::size_t index = r * variableCount + c;
    if (index >= cells.size()) return nullptr;
    return &cells[index];
}

CorrelationMatrixRenderPlan BuildCorrelationMatrixRenderPlan(
    const std::vector<std::string> &variables,
    const std::vector<CorrelationCellResult> &cells,
    int selectedRow,
    int selectedColumn,
    bool showP,
    bool showPValue,
    bool showN)
{
    CorrelationMatrixRenderPlan plan;
    plan.layout = BuildCorrelationMatrixLayout(variables.size(), showPValue, showN);
    plan.headerRuleRect = Rect{
        plan.layout.left,
        plan.layout.top + plan.layout.headerHeight,
        plan.layout.rowHeaderWidth +
            static_cast<double>(std::max<std::size_t>(1, variables.size())) * plan.layout.cellWidth,
        0.0
    };
    plan.addVariableLabel = "+ Add variable";
    plan.addVariableRect = Rect{
        plan.layout.addVariableRect.x + 6.0,
        plan.layout.addVariableRect.y + 3.0,
        plan.layout.addVariableRect.width - 12.0,
        plan.layout.addVariableRect.height - 6.0
    };
    plan.emptyMessage = "Add numeric variables to build the matrix.";
    plan.emptyMessageRect = plan.layout.emptyStatusRect;
    plan.showEmptyMessage = variables.empty();

    plan.columnHeaders.reserve(variables.size());
    plan.rowHeaders.reserve(variables.size());
    plan.cells.reserve(variables.size() * variables.size());

    for (std::size_t i = 0; i < variables.size(); ++i) {
        Rect columnHeader = plan.layout.columnHeaderRects[i];
        columnHeader.x += 4.0;
        columnHeader.y += 6.0;
        columnHeader.width -= 8.0;
        columnHeader.height -= 12.0;
        plan.columnHeaders.push_back(CorrelationHeaderRenderItem{
            static_cast<int>(i),
            variables[i],
            columnHeader
        });

        Rect rowHeader = plan.layout.rowHeaderRects[i];
        Rect rowText = Rect{
            rowHeader.x + 4.0,
            rowHeader.y + std::max(0.0, (plan.layout.rowHeight - 16.0) * 0.5),
            rowHeader.width - 8.0,
            16.0
        };
        plan.rowHeaders.push_back(CorrelationHeaderRenderItem{
            static_cast<int>(i),
            variables[i],
            rowText
        });
    }

    for (std::size_t row = 0; row < variables.size(); ++row) {
        for (std::size_t column = 0; column < variables.size(); ++column) {
            const CorrelationCellResult *cell = CorrelationCellAt(
                cells, variables.size(), static_cast<int>(row), static_cast<int>(column));
            Rect cellRect = plan.layout.cellRects[row][column];
            CorrelationCellRenderItem item;
            item.row = static_cast<int>(row);
            item.column = static_cast<int>(column);
            item.rect = cellRect;
            if (selectedRow == static_cast<int>(row) && selectedColumn == static_cast<int>(column)) {
                item.background = CorrelationCellBackground::Selected;
            } else if (row == column) {
                item.background = CorrelationCellBackground::Diagonal;
            }

            if (cell && row != column) {
                std::vector<std::string> labelLines = SplitLines(
                    CorrelationCellLabel(cell, showP, showPValue));
                const double topInset = 5.0;
                const double mainHeight = 13.0;
                const double mutedHeight = 13.0;
                const double mainToDetailGap = (showPValue && showN) ? 4.0 : 3.0;
                const double detailToNGap = showPValue ? 6.0 : 5.0;
                double cursorY = cellRect.y + topInset;

                if (!labelLines.empty()) {
                    item.textItems.push_back(CorrelationTextItem{
                        labelLines[0],
                        Rect{cellRect.x + 4.0, cursorY, cellRect.width - 8.0, mainHeight},
                        false
                    });
                }

                bool drewPValue = false;
                if (labelLines.size() > 1) {
                    cursorY += mainHeight + mainToDetailGap;
                    item.textItems.push_back(CorrelationTextItem{
                        labelLines[1],
                        Rect{cellRect.x + 4.0, cursorY, cellRect.width - 8.0, mutedHeight},
                        true
                    });
                    drewPValue = true;
                }

                if (showN) {
                    cursorY += (drewPValue ? mutedHeight : mainHeight) + detailToNGap;
                    item.textItems.push_back(CorrelationTextItem{
                        "N = " + std::to_string(cell->n),
                        Rect{cellRect.x + 4.0, cursorY, cellRect.width - 8.0, mutedHeight},
                        true
                    });
                }
            } else {
                const std::string label = CorrelationCellLabel(cell, showP, showPValue);
                item.textItems.push_back(CorrelationTextItem{
                    label,
                    Rect{
                        cellRect.x + 4.0,
                        cellRect.y + std::max(0.0, (plan.layout.rowHeight - 15.0) * 0.5),
                        cellRect.width - 8.0,
                        15.0
                    },
                    false
                });
            }
            plan.cells.push_back(item);
        }
    }

    return plan;
}

CorrelationCellResult ComputePearsonCorrelationForRows(
    const std::string &xName,
    const std::string &yName,
    const std::vector<double> &xValues,
    const std::vector<double> &yValues,
    const std::vector<int> &rowsUsed)
{
    CorrelationCellResult cell;
    cell.xVariable = xName;
    cell.yVariable = yName;
    cell.rowsUsed = rowsUsed;
    cell.n = static_cast<int>(rowsUsed.size());
    if (cell.n < 3) {
        cell.status = "insufficient_n";
        return cell;
    }

    double sumX = 0.0, sumY = 0.0;
    for (int row : rowsUsed) {
        if (row <= 0) {
            cell.status = "invalid_row";
            return cell;
        }
        std::size_t i = static_cast<std::size_t>(row) - 1;
        if (i >= xValues.size() || i >= yValues.size() ||
            !std::isfinite(xValues[i]) || !std::isfinite(yValues[i])) {
            cell.status = "invalid_row";
            return cell;
        }
        sumX += xValues[i];
        sumY += yValues[i];
    }
    double meanX = sumX / static_cast<double>(cell.n);
    double meanY = sumY / static_cast<double>(cell.n);
    double ssX = 0.0, ssY = 0.0, sp = 0.0;
    for (int row : rowsUsed) {
        std::size_t i = static_cast<std::size_t>(row) - 1;
        double dx = xValues[i] - meanX;
        double dy = yValues[i] - meanY;
        ssX += dx * dx;
        ssY += dy * dy;
        sp += dx * dy;
    }
    if (ssX <= 0.0 || ssY <= 0.0) {
        cell.status = "zero_variance";
        return cell;
    }
    cell.r = std::max(-1.0, std::min(1.0, sp / std::sqrt(ssX * ssY)));
    if (std::fabs(cell.r) >= 1.0) {
        cell.p = 0.0;
    } else {
        double t2 = (cell.r * cell.r) * (cell.n - 2) / (1.0 - cell.r * cell.r);
        cell.p = FDistributionUpperTail(t2, 1.0, static_cast<double>(cell.n) - 2.0);
    }
    cell.status = "valid";
    return cell;
}

CorrelationCellResult ComputeCorrelationCellForDataFrameVersion(
    const DataFrameModel &df,
    const std::vector<std::string> &variables,
    const std::string &xName,
    const std::string &yName,
    const std::string &missingMode,
    int versionIndex)
{
    CorrelationCellResult cell;
    cell.xVariable = xName;
    cell.yVariable = yName;
    const DataColumn *xCol = CorrelationDataColumn(df, xName);
    const DataColumn *yCol = CorrelationDataColumn(df, yName);
    if (!xCol || !yCol) {
        cell.status = "missing_variable";
        return cell;
    }
    if (xName == yName) {
        std::vector<const DataColumn *> columns;
        if (missingMode == "listwise") {
            for (const std::string &name : variables) {
                const DataColumn *col = CorrelationDataColumn(df, name);
                if (col) columns.push_back(col);
            }
        } else {
            columns.push_back(xCol);
        }
        cell.rowsUsed = CompleteRowsForDataColumns(df, columns, versionIndex);
        cell.n = static_cast<int>(cell.rowsUsed.size());
        cell.status = "diagonal";
        return cell;
    }

    std::vector<const DataColumn *> columns;
    if (missingMode == "listwise") {
        for (const std::string &name : variables) {
            const DataColumn *col = CorrelationDataColumn(df, name);
            if (col) columns.push_back(col);
        }
    } else {
        columns.push_back(xCol);
        columns.push_back(yCol);
    }
    cell.rowsUsed = CompleteRowsForDataColumns(df, columns, versionIndex);
    cell.n = static_cast<int>(cell.rowsUsed.size());
    if (cell.n < 4) {
        cell.status = "insufficient_n";
        return cell;
    }

    double sumX = 0.0, sumY = 0.0;
    for (int row : cell.rowsUsed) {
        std::size_t i = static_cast<std::size_t>(row) - 1;
        sumX += CorrelationDataValue(df, *xCol, i, versionIndex);
        sumY += CorrelationDataValue(df, *yCol, i, versionIndex);
    }
    double meanX = sumX / static_cast<double>(cell.n);
    double meanY = sumY / static_cast<double>(cell.n);
    double ssX = 0.0, ssY = 0.0, sp = 0.0;
    for (int row : cell.rowsUsed) {
        std::size_t i = static_cast<std::size_t>(row) - 1;
        double dx = CorrelationDataValue(df, *xCol, i, versionIndex) - meanX;
        double dy = CorrelationDataValue(df, *yCol, i, versionIndex) - meanY;
        ssX += dx * dx;
        ssY += dy * dy;
        sp += dx * dy;
    }
    if (ssX <= 0.0 || ssY <= 0.0) {
        cell.status = "zero_variance";
        return cell;
    }
    cell.r = std::max(-1.0, std::min(1.0, sp / std::sqrt(ssX * ssY)));
    cell.status = "valid";
    return cell;
}

CorrelationCellResult ComputePooledCorrelationCell(
    const DataFrameModel &df,
    const std::vector<std::string> &variables,
    const std::string &xName,
    const std::string &yName,
    const std::string &missingMode)
{
    CorrelationCellResult cell;
    cell.xVariable = xName;
    cell.yVariable = yName;
    int imputationCount = std::max(1, df.imputationCount);
    std::vector<double> rByImputation;
    std::vector<int> nByImputation;
    std::vector<std::string> statuses;
    std::vector<int> firstRows;

    for (int version = 0; version < imputationCount; ++version) {
        CorrelationCellResult current = ComputeCorrelationCellForDataFrameVersion(
            df, variables, xName, yName, missingMode, version);
        if (version == 0) firstRows = current.rowsUsed;
        nByImputation.push_back(current.n);
        statuses.push_back(current.status);
        rByImputation.push_back(current.r);
    }
    cell.rowsUsed = firstRows;
    if (!nByImputation.empty()) {
        double nSum = 0.0;
        for (int n : nByImputation) nSum += static_cast<double>(n);
        cell.n = static_cast<int>(std::llround(nSum / static_cast<double>(nByImputation.size())));
    }

    if (xName == yName) {
        cell.status = "diagonal";
        cell.detail = "Multiple imputation matrix; m = " + std::to_string(imputationCount) + "; diagonal cell.";
        return cell;
    }

    PooledCorrelationScalar pooled = PoolCorrelationOnFisherZ(rByImputation, nByImputation);
    if (!pooled.valid) {
        cell.status = JoinUniqueStatuses(statuses);
        return cell;
    }
    cell.r = std::tanh(pooled.qbar);
    cell.p = pooled.p;
    cell.status = "valid";
    std::ostringstream detail;
    detail << "m = " << imputationCount
           << "; pooling = Fisher z transformation plus Rubin's rules"
           << "; SE(z) = " << FormatDouble(pooled.se, 3)
           << "; df = " << (std::isfinite(pooled.df) ? FormatDouble(pooled.df, 1) : std::string("Inf"));
    if (std::isfinite(pooled.fmi)) {
        detail << "; FMI = " << FormatDouble(100.0 * pooled.fmi, 1) << "%";
    }
    detail << ". Scatterplots opened from this cell display the active imputation.";
    cell.detail = detail.str();
    return cell;
}

std::string CorrelationWindowTitle()
{
    return "Pearson Correlation Matrix";
}

std::string CorrelationShowStarsButtonTitle()
{
    return "Stars";
}

std::string CorrelationShowPValuesButtonTitle()
{
    return "p-values";
}

std::string CorrelationShowNButtonTitle()
{
    return "N";
}

std::vector<int> CompleteRowsForVariables(PlotModel *model, const std::vector<std::string> &variables)
{
    std::vector<int> rows;
    if (!model || variables.empty()) return rows;
    std::vector<std::vector<double>> columns;
    columns.reserve(variables.size());
    for (const std::string &name : variables) {
        NumericVariable *var = FindNumericVariable(*model, name);
        if (!var) return rows;
        columns.push_back(var->values);
    }
    return CompleteRowsForNumericVectors(columns);
}

} // namespace core
} // namespace rlispstat
