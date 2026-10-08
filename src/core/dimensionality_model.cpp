#include "dimensionality_model.h"
#include "analysis_initialization.h"
#include "scale_analysis_model.h"

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <iomanip>
#include <limits>
#include <random>
#include <set>
#include <sstream>

namespace rlispstat {
namespace core {

namespace {

std::string DimensionalityFormatPercent(double proportion, int digits)
{
    if (!std::isfinite(proportion)) {
        return "";
    }
    std::ostringstream out;
    out << std::fixed << std::setprecision(digits) << (100.0 * proportion) << "%";
    return out.str();
}

std::string DimensionalityFormatDouble(double value, int digits)
{
    if (!std::isfinite(value)) {
        return "NA";
    }
    std::ostringstream out;
    out << std::fixed << std::setprecision(digits) << value;
    return out.str();
}

std::string DimensionalityFormatDoubleOrDash(double value, int digits)
{
    if (!std::isfinite(value)) {
        return "\u2014";
    }
    std::ostringstream out;
    out << std::fixed << std::setprecision(digits) << value;
    return out.str();
}

std::string DimensionalityFormatPercentOrDash(double proportion, int digits)
{
    if (!std::isfinite(proportion)) {
        return "\u2014";
    }
    std::ostringstream out;
    out << std::fixed << std::setprecision(digits) << (100.0 * proportion) << "%";
    return out.str();
}

std::string DimensionalityComponentAxisLabel(const std::vector<DimensionalityFitComponent> &components,
                                             const std::string &method,
                                             int component)
{
    std::string label = DimensionalityComponentPrefix(method) + std::to_string(component);
    std::size_t index = component > 0 ? static_cast<std::size_t>(component - 1) : components.size();
    if (index < components.size()) {
        std::string percent = DimensionalityFormatPercent(components[index].variance, 1);
        if (!percent.empty()) {
            label += " (" + percent + ")";
        }
    }
    return label;
}

} // namespace

std::vector<std::string> EligibleDimensionalityVariables(const DataFrameModel &dataframe)
{
    return EligibleScaleVariables(dataframe, {});
}

std::string DimensionalityComponentPrefix(const std::string &method)
{
    return method == "factor" ? "F" : "PC";
}

std::string DimensionalityVariableUnavailableStatus(const std::string &name)
{
    return "Status: `" + name + "` is not available as a numeric, ordinal, or binary variable.";
}

std::string DimensionalityVariableAlreadyIncludedStatus(const std::string &name)
{
    return "Status: `" + name + "` is already in the analysis.";
}

std::string DimensionalityVariableAddedStatus(const std::string &name)
{
    return "Added variable `" + name + "`.";
}

std::string DimensionalityVariableReplacedStatus(const std::string &oldName,
                                                 const std::string &newName)
{
    return "Replaced `" + oldName + "` with `" + newName + "`.";
}

std::string DimensionalityVariableRemovedStatus(const std::string &name)
{
    return "Removed variable `" + name + "`.";
}

std::string DimensionalityNoMoreNumericVariablesTitle()
{
    return "No more eligible variables";
}

std::string DimensionalityNoReplacementVariablesTitle()
{
    return "No replacement variables";
}

std::string DimensionalityAddVariableMenuTitle()
{
    return "Add variable";
}

std::string DimensionalityVariableMenuTitle()
{
    return "Variable";
}

std::string DimensionalityReplaceVariableMenuTitle()
{
    return "Replace with";
}

std::string DimensionalityShowVariableInformationTitle()
{
    return "Show Variable Information";
}

std::string DimensionalityRemoveVariableTitle(const std::string &name)
{
    return "Remove " + name;
}

std::string DimensionalityBiplotDimensionsMenuTitle()
{
    return "Dimensions";
}

std::string DimensionalityComponentMenuLabel(const std::string &method,
                                             int component,
                                             double variance)
{
    std::string label = DimensionalityComponentPrefix(method) + std::to_string(component);
    std::string percent = DimensionalityFormatPercent(variance, 1);
    if (!percent.empty()) {
        label += " (" + percent + ")";
    }
    return label;
}

std::string DimensionalityBiplotRequiresTwoComponentsStatus()
{
    return "Status: at least two components are required for a biplot.";
}

std::string DimensionalityNoScoresStatus()
{
    return "Status: no scores are available to save.";
}

std::string DimensionalitySourceSheetUnavailableStatus()
{
    return "Status: the source data sheet is not available.";
}

std::string DimensionalityScoresSavedStatus(std::size_t columnCount)
{
    std::ostringstream status;
    status << "Status: saved " << columnCount << " score column";
    if (columnCount != 1) status << "s";
    status << ".";
    return status.str();
}

std::string DimensionalityScreePlotTitle(const std::string &method,
                                         int retainedComponents,
                                         const std::string &rotation)
{
    const bool factor = method == "factor";
    std::ostringstream title;
    title << "Scree plot - " << (factor ? "Factor analysis" : "PCA");
    if (retainedComponents > 0) {
        title << " (" << retainedComponents << " retained "
              << (factor ? "factor" : "component");
        if (retainedComponents != 1) title << "s";
        title << ")";
    }
    if (rotation != "none" && DimensionalityRotationIsValid(rotation)) {
        title << ", " << rotation << " rotation";
    }
    return title.str();
}

std::string DimensionalityFitSignature(const std::string &method,
                                       const std::string &missingMode,
                                       const std::string &rotation,
                                       const std::string &extraction,
                                       const std::string &scope,
                                       bool scale,
                                       int componentCount,
                                       int displayedImputation,
                                       const std::vector<std::string> &variables)
{
    std::ostringstream out;
    out << method << "|" << missingMode << "|" << rotation << "|" << extraction << "|"
        << scope << "|" << (scale ? "1" : "0") << "|"
        << componentCount << "|imputation=" << std::max(1, displayedImputation);
    for (const std::string &variable : variables) {
        out << "|" << variable;
    }
    return out.str();
}

DimensionalityScreeContextMenuState BuildDimensionalityScreeContextMenuState()
{
    return DimensionalityScreeContextMenuState{};
}

DimensionalityScreePlotState BuildDimensionalityScreePlotState(
    const std::vector<DimensionalityFitComponent> &components,
    const std::string &method,
    int retainedComponents,
    const std::string &rotation)
{
    DimensionalityScreePlotState state;
    state.title = DimensionalityScreePlotTitle(method, retainedComponents, rotation);
    if (components.empty()) {
        return state;
    }

    bool found = false;
    double xmin = std::numeric_limits<double>::infinity();
    double xmax = -std::numeric_limits<double>::infinity();
    double ymin = std::numeric_limits<double>::infinity();
    double ymax = -std::numeric_limits<double>::infinity();
    auto addRange = [&](double x, double y) {
        if (!std::isfinite(x) || !std::isfinite(y)) return;
        found = true;
        xmin = std::min(xmin, x);
        xmax = std::max(xmax, x);
        ymin = std::min(ymin, y);
        ymax = std::max(ymax, y);
    };

    for (const DimensionalityFitComponent &component : components) {
        const double x = static_cast<double>(component.index);
        if (component.index > 0 && std::isfinite(component.eigenvalue)) {
            state.observed.push_back(DimensionalityScreePoint{
                component.index,
                x,
                component.eigenvalue,
                0
            });
            addRange(x, component.eigenvalue);
        }
        if (component.index > 0 && std::isfinite(component.parallelEigenvalue)) {
            state.parallel.push_back(DimensionalityScreePoint{
                component.index,
                x,
                component.parallelEigenvalue,
                0
            });
            addRange(x, component.parallelEigenvalue);
        }
    }
    if (!found || state.observed.empty()) {
        state.observed.clear();
        state.parallel.clear();
        return state;
    }
    if (xmin == xmax) {
        xmin -= 0.5;
        xmax += 0.5;
    }
    if (ymin == ymax) {
        ymin -= 0.5;
        ymax += 0.5;
    }
    state.xmin = std::min(0.5, xmin);
    state.xmax = std::max(xmax + 0.5, static_cast<double>(components.size()) + 0.5);
    state.ymin = std::min(0.0, ymin);
    state.ymax = ymax + std::max(0.10, (ymax - ymin) * 0.08);
    if (state.ymax <= state.ymin) {
        state.ymax = state.ymin + 1.0;
    }
    state.ok = true;
    return state;
}

int DimensionalityBiplotAvailableComponentCount(
    const std::vector<DimensionalityFitComponent> &components,
    const std::vector<DimensionalityFitScore> &scores)
{
    std::size_t scoreColumns = 0;
    for (const auto &score : scores)
        scoreColumns = std::max(scoreColumns, score.values.size());
    return static_cast<int>(std::min(components.size(), scoreColumns));
}

DimensionalityBiplotPlotState BuildDimensionalityBiplotPlotState(
    const std::vector<DimensionalityFitComponent> &components,
    const std::vector<DimensionalityFitLoading> &loadings,
    const std::vector<DimensionalityFitScore> &scores,
    const std::string &method,
    int requestedXComponent,
    int requestedYComponent)
{
    DimensionalityBiplotPlotState state;
    const int maxDim = DimensionalityBiplotAvailableComponentCount(
        components, scores);
    if (maxDim < 2) {
        return state;
    }
    state.xComponent = std::max(1, std::min(requestedXComponent, maxDim));
    state.yComponent = std::max(1, std::min(requestedYComponent, maxDim));
    if (state.xComponent == state.yComponent) {
        state.yComponent = state.xComponent == maxDim ? std::max(1, maxDim - 1) : state.xComponent + 1;
    }
    const std::size_t xCol = static_cast<std::size_t>(state.xComponent - 1);
    const std::size_t yCol = static_cast<std::size_t>(state.yComponent - 1);

    bool foundScore = false;
    double xmin = std::numeric_limits<double>::infinity();
    double xmax = -std::numeric_limits<double>::infinity();
    double ymin = std::numeric_limits<double>::infinity();
    double ymax = -std::numeric_limits<double>::infinity();
    for (const DimensionalityFitScore &score : scores) {
        if (score.values.size() <= std::max(xCol, yCol)) continue;
        double x = score.values[xCol];
        double y = score.values[yCol];
        if (!std::isfinite(x) || !std::isfinite(y)) continue;
        state.scores.push_back(DimensionalityBiplotScorePoint{score.row, x, y});
        foundScore = true;
        xmin = std::min(xmin, x);
        xmax = std::max(xmax, x);
        ymin = std::min(ymin, y);
        ymax = std::max(ymax, y);
    }
    if (!foundScore) {
        state.scores.clear();
        return state;
    }
    if (xmin == xmax) {
        xmin -= 0.5;
        xmax += 0.5;
    }
    if (ymin == ymax) {
        ymin -= 0.5;
        ymax += 0.5;
    }
    const double scoreXPad = (xmax - xmin) * 0.05;
    const double scoreYPad = (ymax - ymin) * 0.05;
    xmin -= scoreXPad;
    xmax += scoreXPad;
    ymin -= scoreYPad;
    ymax += scoreYPad;

    const double xSpan = std::max(1.0e-6, xmax - xmin);
    const double ySpan = std::max(1.0e-6, ymax - ymin);
    double maxLoading = 0.0;
    for (const DimensionalityFitLoading &loading : loadings) {
        if (loading.values.size() <= std::max(xCol, yCol)) continue;
        double x = loading.values[xCol];
        double y = loading.values[yCol];
        if (std::isfinite(x) && std::isfinite(y)) {
            maxLoading = std::max(maxLoading, std::sqrt(x * x + y * y));
        }
    }
    const double scale = maxLoading > 0.0 ? 0.38 * std::min(xSpan, ySpan) / maxLoading : 1.0;
    for (const DimensionalityFitLoading &loading : loadings) {
        if (loading.values.size() <= std::max(xCol, yCol)) continue;
        double x = loading.values[xCol];
        double y = loading.values[yCol];
        if (!std::isfinite(x) || !std::isfinite(y)) continue;
        x *= scale;
        y *= scale;
        state.loadings.push_back(DimensionalityBiplotLoadingVector{loading.variable, x, y});
        xmin = std::min(xmin, x);
        xmax = std::max(xmax, x);
        ymin = std::min(ymin, y);
        ymax = std::max(ymax, y);
    }

    state.dataXmin = xmin;
    state.dataXmax = xmax;
    state.dataYmin = ymin;
    state.dataYmax = ymax;
    const double padX = std::max(0.10, (xmax - xmin) * 0.08);
    const double padY = std::max(0.10, (ymax - ymin) * 0.08);
    state.xmin = xmin - padX;
    state.xmax = xmax + padX;
    state.ymin = ymin - padY;
    state.ymax = ymax + padY;
    state.xLabel = DimensionalityComponentAxisLabel(components, method, state.xComponent);
    state.yLabel = DimensionalityComponentAxisLabel(components, method, state.yComponent);
    state.title = method == "factor" ? "Factor biplot" : "PCA biplot";
    state.ok = true;
    return state;
}

DimensionalityScreeRenderPlan BuildDimensionalityScreeRenderPlan(
    const std::vector<DimensionalityScreePoint> &observed,
    const std::vector<DimensionalityScreePoint> &parallel,
    const DataViewport &viewport,
    const Rect &plotRect,
    int focusedComponent)
{
    DimensionalityScreeRenderPlan plan;
    if (!IsValidViewport(viewport) || !IsValidRect(plotRect)) {
        return plan;
    }

    plan.observedLine.reserve(observed.size());
    plan.observedPoints.reserve(observed.size());
    for (const DimensionalityScreePoint &point : observed) {
        Point screen = DataToScreen(Point{point.x, point.y}, viewport, plotRect, true);
        if (!std::isfinite(screen.x) || !std::isfinite(screen.y)) {
            continue;
        }
        plan.observedLine.push_back(screen);
        plan.observedPoints.push_back(DimensionalityScreeRenderPoint{
            point.component,
            screen,
            focusedComponent > 0 && point.component == focusedComponent
        });
    }

    plan.parallelLine.reserve(parallel.size());
    for (const DimensionalityScreePoint &point : parallel) {
        Point screen = DataToScreen(Point{point.x, point.y}, viewport, plotRect, true);
        if (std::isfinite(screen.x) && std::isfinite(screen.y)) {
            plan.parallelLine.push_back(screen);
        }
    }

    return plan;
}

DimensionalityBiplotRenderPlan BuildDimensionalityBiplotRenderPlan(
    const std::vector<DimensionalityBiplotLoadingVector> &loadings,
    const DataViewport &viewport,
    const Rect &plotRect,
    const std::string &focusedVariable)
{
    DimensionalityBiplotRenderPlan plan;
    if (!IsValidViewport(viewport) || !IsValidRect(plotRect)) {
        return plan;
    }

    Point start = DataToScreen(Point{0.0, 0.0}, viewport, plotRect, true);
    if (!std::isfinite(start.x) || !std::isfinite(start.y)) {
        return plan;
    }

    plan.loadings.reserve(loadings.size());
    for (const DimensionalityBiplotLoadingVector &loading : loadings) {
        if (!std::isfinite(loading.x) || !std::isfinite(loading.y)) {
            continue;
        }
        Point end = DataToScreen(Point{loading.x, loading.y}, viewport, plotRect, true);
        if (!std::isfinite(end.x) || !std::isfinite(end.y)) {
            continue;
        }
        const bool focused = !focusedVariable.empty() && loading.variable == focusedVariable;
        const double head = focused ? 9.5 : 7.0;
        const double angle = std::atan2(end.y - start.y, end.x - start.x);
        plan.loadings.push_back(DimensionalityBiplotLoadingRenderItem{
            loading.variable,
            start,
            end,
            Point{end.x - head * std::cos(angle - 0.45),
                  end.y - head * std::sin(angle - 0.45)},
            Point{end.x - head * std::cos(angle + 0.45),
                  end.y - head * std::sin(angle + 0.45)},
            Point{end.x + 4.0, end.y - 6.0},
            focused
        });
    }

    return plan;
}

DimensionalityScoreExportPlan BuildDimensionalityScoreExportPlan(
    const std::vector<DimensionalityFitScore> &scores,
    const std::string &method,
    int requestedComponents,
    int availableComponents,
    int rowCount,
    const std::vector<std::string> &existingNames)
{
    DimensionalityScoreExportPlan plan;
    if (scores.empty() || rowCount < 0) {
        return plan;
    }
    int count = std::max(1, std::min(requestedComponents, availableComponents));
    const std::string prefix = DimensionalityComponentPrefix(method);
    std::set<std::string> used(existingNames.begin(), existingNames.end());
    for (int k = 0; k < count; ++k) {
        const std::string base = prefix + std::to_string(k + 1) + "_score";
        std::string name = base;
        int suffix = 2;
        while (used.find(name) != used.end()) {
            name = base + "_" + std::to_string(suffix++);
        }
        used.insert(name);

        DimensionalityScoreExportColumn column;
        column.name = name;
        column.values.assign(static_cast<std::size_t>(rowCount), "NA");
        for (const DimensionalityFitScore &score : scores) {
            if (score.row < 1 || score.row > rowCount || score.values.size() <= static_cast<std::size_t>(k)) {
                continue;
            }
            column.values[static_cast<std::size_t>(score.row - 1)] =
                DimensionalityFormatDouble(score.values[static_cast<std::size_t>(k)], 10);
        }
        plan.columns.push_back(column);
    }
    plan.ok = !plan.columns.empty();
    return plan;
}

DimensionalityReportState BuildDimensionalityReportState(
    const std::vector<std::string> &variables,
    const std::vector<DimensionalityFitComponent> &components,
    const std::vector<DimensionalityFitLoading> &loadings,
    std::size_t rowsUsed,
    std::size_t rowsExcluded,
    const std::string &method,
    int componentCount,
    const std::string &status,
    int focusedComponent,
    const std::string &focusedVariable,
    std::size_t maxShownComponents,
    const std::string &missingMode,
    const std::string &rotation,
    bool scale,
    bool multipleImputation,
    int displayedImputation,
    int imputationCount,
    const std::string &calculationMethod)
{
    DimensionalityReportState report;
    report.componentPrefix = DimensionalityComponentPrefix(method);
    report.componentCount = std::max(1, componentCount);
    report.focusedComponent = std::max(0, focusedComponent);
    report.focusedVariable = focusedVariable;
    report.status = status;
    report.hasLoadings = !loadings.empty();
    std::ostringstream summary;
    summary << rowsUsed << " complete rows, " << rowsExcluded << " excluded";
    report.summary = calculationMethod.empty() ? summary.str()
        : std::to_string(rowsUsed) + " cases in analysis; " + std::to_string(rowsExcluded) +
          " excluded. Scores use complete cases.";
    std::ostringstream calculation;
    calculation << "Calculated in R with "
                << (method == "factor" ? "stats::factanal (maximum likelihood)" : "stats::prcomp")
                << "; " << (scale ? "standardized variables" : "unstandardized variables");
    if (missingMode == "pairwise") {
        calculation << "; listwise complete-row decomposition"
                    << " (the pairwise option currently falls back to listwise for this table)";
    } else {
        calculation << "; listwise complete-row input";
    }
    if (!rotation.empty() && rotation != "none") calculation << "; " << rotation << " rotation";
    calculation << ". Parallel reference: 95th percentile from 100 simulated normal data sets.";
    report.calculationMethod = calculationMethod.empty() ? calculation.str() : calculationMethod;
    if (multipleImputation) {
        std::ostringstream imputation;
        imputation << "Multiple imputation: showing completed imputation "
                   << std::max(1, displayedImputation) << " of " << std::max(1, imputationCount)
                   << "; eigenvalues and loading matrices are not Rubin-pooled.";
        report.calculationImputation = imputation.str();
    }

    const std::size_t shownComponents = std::min(components.size(), maxShownComponents);
    report.hasAdditionalComponents = components.size() > shownComponents;
    report.componentRows.reserve(shownComponents);
    for (std::size_t i = 0; i < shownComponents; ++i) {
        const DimensionalityFitComponent &component = components[i];
        report.componentRows.push_back(DimensionalityReportComponentRow{
            component.index,
            report.componentPrefix + std::to_string(component.index),
            DimensionalityFormatDoubleOrDash(component.eigenvalue, 3),
            DimensionalityFormatDoubleOrDash(component.parallelEigenvalue, 3),
            DimensionalityFormatPercentOrDash(component.variance, 1),
            DimensionalityFormatPercentOrDash(component.cumulative, 1),
            focusedComponent > 0 && component.index == focusedComponent
        });
    }

    report.loadingHeaders.reserve(static_cast<std::size_t>(report.componentCount));
    for (int k = 0; k < report.componentCount; ++k) {
        report.loadingHeaders.push_back(report.componentPrefix + std::to_string(k + 1));
    }

    report.loadingRows.reserve(variables.size());
    for (const std::string &variable : variables) {
        const DimensionalityFitLoading *loading = nullptr;
        for (const DimensionalityFitLoading &candidate : loadings) {
            if (candidate.variable == variable) {
                loading = &candidate;
                break;
            }
        }

        DimensionalityReportLoadingRow row;
        row.variable = variable;
        row.highlighted = !focusedVariable.empty() && variable == focusedVariable;
        row.loadings.reserve(static_cast<std::size_t>(report.componentCount));
        for (int k = 0; k < report.componentCount; ++k) {
            const int component = k + 1;
            double value = (loading && loading->values.size() > static_cast<std::size_t>(k))
                ? loading->values[static_cast<std::size_t>(k)]
                : std::numeric_limits<double>::quiet_NaN();
            row.loadings.push_back(DimensionalityReportValueCell{
                component,
                DimensionalityFormatDoubleOrDash(value, 3),
                std::isfinite(value) && std::fabs(value) >= 0.40,
                focusedComponent > 0 && component == focusedComponent
            });
        }
        row.communality = DimensionalityFormatDoubleOrDash(
            loading ? loading->communality : std::numeric_limits<double>::quiet_NaN(), 3);
        row.uniqueness = DimensionalityFormatDoubleOrDash(
            loading ? loading->uniqueness : std::numeric_limits<double>::quiet_NaN(), 3);
        report.loadingRows.push_back(row);
    }
    return report;
}

DimensionalityReportLayout BuildDimensionalityReportLayout(
    const DimensionalityReportState &report)
{
    constexpr double left = 18.0;
    constexpr double summaryY = 14.0;
    constexpr double sectionStep = 24.0;
    constexpr double rowStep = 24.0;
    constexpr double rowHeight = 22.0;
    constexpr double componentWidth = 610.0;
    constexpr double variableWidth = 150.0;
    constexpr double loadingsLeft = left + 166.0;
    constexpr double loadingColumnWidth = 64.0;
    constexpr double loadingColumnStep = 76.0;

    DimensionalityReportLayout layout;
    layout.summaryRect = Rect{left, summaryY, 520.0, 18.0};
    const double extraLines = 18.0 * std::count(report.calculationMethod.begin(), report.calculationMethod.end(), '\n');
    layout.calculationMethodRect = Rect{left, summaryY + 22.0, 700.0, 18.0 + extraLines};
    layout.calculationImputationRect = Rect{left, summaryY + 42.0 + extraLines, 700.0, 18.0};
    const double calculationHeight = (report.calculationImputation.empty() ? 42.0 : 62.0) + extraLines;
    layout.componentsTitleRect = Rect{left, summaryY + calculationHeight, 160.0, 20.0};
    const double componentHeaderY = layout.componentsTitleRect.y + sectionStep;
    layout.componentHeaderRects = {
        Rect{left, componentHeaderY, 120.0, 18.0},
        Rect{left + 148.0, componentHeaderY, 90.0, 18.0},
        Rect{left + 258.0, componentHeaderY, 92.0, 18.0},
        Rect{left + 382.0, componentHeaderY, 92.0, 18.0},
        Rect{left + 506.0, componentHeaderY, 92.0, 18.0}
    };
    layout.componentRuleEndX = left + componentWidth;
    layout.componentRowsY = layout.componentsTitleRect.y + sectionStep + sectionStep;
    layout.componentRects.reserve(report.componentRows.size());
    layout.componentCellRects.reserve(report.componentRows.size());
    for (std::size_t i = 0; i < report.componentRows.size(); ++i) {
        const double y = layout.componentRowsY + rowStep * static_cast<double>(i);
        layout.componentRects.push_back(Rect{
            left,
            y,
            componentWidth,
            rowHeight
        });
        layout.componentCellRects.push_back({
            Rect{layout.componentHeaderRects[0].x, y, layout.componentHeaderRects[0].width, 18.0},
            Rect{layout.componentHeaderRects[1].x, y, layout.componentHeaderRects[1].width, 18.0},
            Rect{layout.componentHeaderRects[2].x, y, layout.componentHeaderRects[2].width, 18.0},
            Rect{layout.componentHeaderRects[3].x, y, layout.componentHeaderRects[3].width, 18.0},
            Rect{layout.componentHeaderRects[4].x, y, layout.componentHeaderRects[4].width, 18.0}
        });
    }
    layout.additionalComponentsRect = Rect{
        left,
        layout.componentRowsY + rowStep * static_cast<double>(report.componentRows.size()),
        420.0,
        18.0
    };

    layout.loadingsRowsY = layout.componentRowsY +
        rowStep * static_cast<double>(report.componentRows.size()) +
        (report.hasAdditionalComponents ? 22.0 : 0.0) +
        18.0 + sectionStep + sectionStep;
    layout.loadingsTitleRect = Rect{left, layout.loadingsRowsY - 48.0, 160.0, 20.0};
    const double loadingHeaderY = layout.loadingsRowsY - sectionStep;
    layout.loadingVariableHeaderRect = Rect{left, loadingHeaderY, variableWidth, 18.0};
    layout.loadingHeaderRects.reserve(report.loadingHeaders.size());
    for (std::size_t k = 0; k < report.loadingHeaders.size(); ++k) {
        layout.loadingHeaderRects.push_back(Rect{
            loadingsLeft + loadingColumnStep * static_cast<double>(k),
            loadingHeaderY,
            loadingColumnWidth,
            rowHeight
        });
    }
    const double metricX = loadingsLeft +
        loadingColumnStep * static_cast<double>(std::max<std::size_t>(1, report.loadingHeaders.size()));
    layout.communalityHeaderRect = Rect{metricX, loadingHeaderY, 64.0, 18.0};
    layout.uniquenessHeaderRect = Rect{metricX + loadingColumnStep, loadingHeaderY, 64.0, 18.0};
    layout.loadingsRuleEndX = metricX + 150.0;

    layout.loadingCellRects.reserve(report.loadingRows.size());
    layout.variableRects.reserve(report.loadingRows.size());
    layout.communalityRects.reserve(report.loadingRows.size());
    layout.uniquenessRects.reserve(report.loadingRows.size());
    for (std::size_t i = 0; i < report.loadingRows.size(); ++i) {
        const double y = layout.loadingsRowsY + rowStep * static_cast<double>(i);
        layout.variableRects.push_back(Rect{left, y, variableWidth, rowHeight});
        std::vector<Rect> cells;
        cells.reserve(report.loadingHeaders.size());
        for (std::size_t k = 0; k < report.loadingHeaders.size(); ++k) {
            cells.push_back(Rect{
                loadingsLeft + loadingColumnStep * static_cast<double>(k),
                y,
                loadingColumnWidth,
                rowHeight
            });
        }
        layout.loadingCellRects.push_back(cells);
        layout.communalityRects.push_back(Rect{metricX, y, 64.0, 18.0});
        layout.uniquenessRects.push_back(Rect{metricX + loadingColumnStep, y, 64.0, 18.0});
    }

    layout.addVariableRect = Rect{
        left,
        layout.loadingsRowsY + rowStep * static_cast<double>(report.loadingRows.size()),
        variableWidth,
        rowHeight
    };
    layout.emptyStatusRect = Rect{loadingsLeft, layout.addVariableRect.y, 520.0, 18.0};
    const double loadingWidth = 420.0 + loadingColumnStep * static_cast<double>(std::max(2, report.componentCount));
    layout.preferredWidth = std::max(740.0, loadingWidth);
    const AnalysisVariableListLayout variableList = BuildAnalysisVariableListLayout(
        report.loadingRows.size(), true, 10, rowStep, 0.0);
    layout.preferredHeight = std::max(
        250.0,
        layout.loadingsRowsY + std::max(rowStep, variableList.contentHeight) + 48.0);
    return layout;
}

Rect DimensionalityReportRectForComponent(
    const DimensionalityReportState &report,
    const DimensionalityReportLayout &layout,
    int component)
{
    for (std::size_t i = 0; i < report.componentRows.size() && i < layout.componentRects.size(); ++i) {
        if (report.componentRows[i].component == component) {
            return layout.componentRects[i];
        }
    }
    return {};
}

Rect DimensionalityReportRectForVariableIndex(
    const DimensionalityReportLayout &layout,
    std::size_t index)
{
    if (index >= layout.variableRects.size()) {
        return {};
    }
    return layout.variableRects[index];
}

Rect DimensionalityReportRectForVariable(
    const DimensionalityReportState &report,
    const DimensionalityReportLayout &layout,
    const std::string &variable)
{
    if (variable.empty()) {
        return {};
    }
    for (std::size_t i = 0; i < report.loadingRows.size() && i < layout.variableRects.size(); ++i) {
        if (report.loadingRows[i].variable == variable) {
            return layout.variableRects[i];
        }
    }
    return {};
}

Rect DimensionalityReportFocusRect(
    const DimensionalityReportState &report,
    const DimensionalityReportLayout &layout,
    int component,
    const std::string &variable)
{
    if (!variable.empty()) {
        Rect rect = DimensionalityReportRectForVariable(report, layout, variable);
        if (rect.width > 0.0 && rect.height > 0.0) {
            return rect;
        }
    }
    if (component > 0) {
        return DimensionalityReportRectForComponent(report, layout, component);
    }
    return {};
}

DimensionalityReportRenderPlan BuildDimensionalityReportRenderPlan(
    const DimensionalityReportState &report,
    const DimensionalityReportLayout &layout)
{
    auto textRect = [](Rect rect) {
        rect.height = 18.0;
        return rect;
    };
    auto addText = [](DimensionalityReportRenderPlan &plan,
                      const std::string &text,
                      Rect rect,
                      DimensionalityReportTextRole role) {
        plan.texts.push_back(DimensionalityReportTextItem{text, rect, role});
    };

    DimensionalityReportRenderPlan plan;
    addText(plan, report.summary, textRect(layout.summaryRect), DimensionalityReportTextRole::Muted);
    std::istringstream methodLines(report.calculationMethod);
    std::string methodLine; double methodY = layout.calculationMethodRect.y;
    while (std::getline(methodLines, methodLine)) {
        auto rect = layout.calculationMethodRect; rect.y = methodY; rect.height = 18;
        addText(plan, methodLine, textRect(rect), DimensionalityReportTextRole::Muted);
        methodY += 18;
    }
    if (!report.calculationImputation.empty()) {
        addText(plan, report.calculationImputation, textRect(layout.calculationImputationRect), DimensionalityReportTextRole::Muted);
    }
    addText(plan, "Components", layout.componentsTitleRect, DimensionalityReportTextRole::Section);
    const std::vector<std::string> componentHeaders = {
        "Component", "Eigenvalue", "Parallel", "Variance", "Cumulative"
    };
    for (std::size_t i = 0; i < componentHeaders.size() && i < layout.componentHeaderRects.size(); ++i) {
        addText(plan,
                componentHeaders[i],
                textRect(layout.componentHeaderRects[i]),
                i == 0 ? DimensionalityReportTextRole::Section : DimensionalityReportTextRole::RightHeader);
    }
    if (!layout.componentHeaderRects.empty()) {
        const double y = layout.componentHeaderRects[0].y + 20.0;
        plan.rules.push_back(DimensionalityReportRuleItem{
            Point{layout.summaryRect.x, y},
            Point{layout.componentRuleEndX, y}
        });
    }

    for (std::size_t i = 0; i < report.componentRows.size(); ++i) {
        if (i >= layout.componentRects.size() || i >= layout.componentCellRects.size()) {
            continue;
        }
        const DimensionalityReportComponentRow &row = report.componentRows[i];
        if (row.highlighted) {
            Rect rect = layout.componentRects[i];
            plan.highlights.push_back(DimensionalityReportHighlightItem{
                Rect{rect.x - 4.0, rect.y - 2.0, rect.width + 10.0, 22.0},
                DimensionalityReportHighlightRole::Strong
            });
        }
        const std::vector<Rect> &cells = layout.componentCellRects[i];
        if (cells.size() >= 5) {
            addText(plan, row.label, textRect(cells[0]), DimensionalityReportTextRole::Left);
            addText(plan, row.eigenvalue, textRect(cells[1]), DimensionalityReportTextRole::Right);
            addText(plan, row.parallelEigenvalue, textRect(cells[2]), DimensionalityReportTextRole::Right);
            addText(plan, row.variance, textRect(cells[3]), DimensionalityReportTextRole::Right);
            addText(plan, row.cumulative, textRect(cells[4]), DimensionalityReportTextRole::Right);
        }
    }
    if (report.hasAdditionalComponents) {
        addText(plan,
                "Additional components are available in the scree plot.",
                textRect(layout.additionalComponentsRect),
                DimensionalityReportTextRole::Muted);
    }

    addText(plan, "Loadings", layout.loadingsTitleRect, DimensionalityReportTextRole::Section);
    addText(plan, "Variable", textRect(layout.loadingVariableHeaderRect), DimensionalityReportTextRole::Section);
    for (std::size_t k = 0; k < report.loadingHeaders.size() && k < layout.loadingHeaderRects.size(); ++k) {
        if (report.focusedComponent > 0 && static_cast<int>(k) + 1 == report.focusedComponent) {
            const Rect &rect = layout.loadingHeaderRects[k];
            plan.highlights.push_back(DimensionalityReportHighlightItem{
                Rect{rect.x - 2.0, rect.y - 2.0, rect.width + 4.0, 22.0},
                DimensionalityReportHighlightRole::Strong
            });
        }
        addText(plan,
                report.loadingHeaders[k],
                textRect(layout.loadingHeaderRects[k]),
                DimensionalityReportTextRole::RightHeader);
    }
    addText(plan, "h2", textRect(layout.communalityHeaderRect), DimensionalityReportTextRole::RightHeader);
    addText(plan, "u2", textRect(layout.uniquenessHeaderRect), DimensionalityReportTextRole::RightHeader);
    {
        const double y = layout.loadingVariableHeaderRect.y + 20.0;
        plan.rules.push_back(DimensionalityReportRuleItem{
            Point{layout.summaryRect.x, y},
            Point{layout.loadingsRuleEndX, y}
        });
    }

    for (std::size_t i = 0; i < report.loadingRows.size(); ++i) {
        if (i >= layout.variableRects.size() || i >= layout.loadingCellRects.size()) {
            continue;
        }
        const DimensionalityReportLoadingRow &row = report.loadingRows[i];
        if (row.highlighted) {
            const Rect &rect = layout.variableRects[i];
            plan.highlights.push_back(DimensionalityReportHighlightItem{
                Rect{
                    layout.summaryRect.x - 4.0,
                    rect.y - 2.0,
                    std::max(610.0, layout.loadingsRuleEndX - layout.summaryRect.x),
                    22.0
                },
                DimensionalityReportHighlightRole::Soft
            });
        }
        addText(plan, row.variable, textRect(layout.variableRects[i]), DimensionalityReportTextRole::Left);
        for (std::size_t k = 0; k < row.loadings.size() && k < layout.loadingCellRects[i].size(); ++k) {
            const Rect &rect = layout.loadingCellRects[i][k];
            if (row.loadings[k].highlighted) {
                plan.highlights.push_back(DimensionalityReportHighlightItem{
                    Rect{rect.x - 2.0, rect.y - 2.0, rect.width + 4.0, 22.0},
                    DimensionalityReportHighlightRole::Strong
                });
            }
            addText(plan,
                    row.loadings[k].text,
                    textRect(rect),
                    row.loadings[k].emphasized
                        ? DimensionalityReportTextRole::RightHeader
                        : DimensionalityReportTextRole::Right);
        }
        if (i < layout.communalityRects.size()) {
            addText(plan, row.communality, textRect(layout.communalityRects[i]), DimensionalityReportTextRole::Right);
        }
        if (i < layout.uniquenessRects.size()) {
            addText(plan, row.uniqueness, textRect(layout.uniquenessRects[i]), DimensionalityReportTextRole::Right);
        }
    }
    addText(plan, "+ Add variable", textRect(layout.addVariableRect), DimensionalityReportTextRole::Section);
    if (!report.hasLoadings) {
        addText(plan, report.status, textRect(layout.emptyStatusRect), DimensionalityReportTextRole::Muted);
    }
    return plan;
}

DimensionalityReportViewModel BuildDimensionalityReportViewModel(
    const std::vector<std::string> &variables,
    const std::vector<DimensionalityFitComponent> &components,
    const std::vector<DimensionalityFitLoading> &loadings,
    std::size_t rowsUsed,
    std::size_t rowsExcluded,
    const std::string &method,
    int componentCount,
    const std::string &status,
    int focusedComponent,
    const std::string &focusedVariable,
    std::size_t maxShownComponents,
    const std::string &missingMode,
    const std::string &rotation,
    bool scale,
    bool multipleImputation,
    int displayedImputation,
    int imputationCount,
    const std::string &calculationMethod)
{
    DimensionalityReportViewModel model;
    model.report = BuildDimensionalityReportState(
        variables,
        components,
        loadings,
        rowsUsed,
        rowsExcluded,
        method,
        componentCount,
        status,
        focusedComponent,
        focusedVariable,
        maxShownComponents,
        missingMode,
        rotation,
        scale,
        multipleImputation,
        displayedImputation,
        imputationCount, calculationMethod);
    model.layout = BuildDimensionalityReportLayout(model.report);
    model.renderPlan = BuildDimensionalityReportRenderPlan(model.report, model.layout);
    return model;
}

DimensionalityReportHit HitTestDimensionalityReport(
    const DimensionalityReportState &report,
    const DimensionalityReportLayout &layout,
    const Point &point,
    double xPadding,
    double yPadding)
{
    auto hitRect = [&](const Rect &rect) {
        return PointInRect(point, ExpandRect(rect, xPadding, yPadding));
    };

    for (std::size_t i = 0; i < report.componentRows.size() && i < layout.componentRects.size(); ++i) {
        if (hitRect(layout.componentRects[i])) {
            DimensionalityReportHit hit;
            hit.kind = DimensionalityReportHitKind::Component;
            hit.component = report.componentRows[i].component;
            hit.componentIndex = i;
            return hit;
        }
    }

    for (std::size_t k = 0; k < report.loadingHeaders.size() && k < layout.loadingHeaderRects.size(); ++k) {
        if (hitRect(layout.loadingHeaderRects[k])) {
            DimensionalityReportHit hit;
            hit.kind = DimensionalityReportHitKind::LoadingHeader;
            hit.component = static_cast<int>(k) + 1;
            hit.componentIndex = k;
            return hit;
        }
    }

    for (std::size_t i = 0; i < report.loadingRows.size() && i < layout.loadingCellRects.size(); ++i) {
        const std::vector<Rect> &cells = layout.loadingCellRects[i];
        for (std::size_t k = 0; k < report.loadingHeaders.size() && k < cells.size(); ++k) {
            if (hitRect(cells[k])) {
                DimensionalityReportHit hit;
                hit.kind = DimensionalityReportHitKind::LoadingCell;
                hit.component = static_cast<int>(k) + 1;
                hit.componentIndex = k;
                hit.variableIndex = i;
                hit.variable = report.loadingRows[i].variable;
                return hit;
            }
        }
    }

    for (std::size_t i = 0; i < report.loadingRows.size() && i < layout.variableRects.size(); ++i) {
        if (hitRect(layout.variableRects[i])) {
            DimensionalityReportHit hit;
            hit.kind = DimensionalityReportHitKind::Variable;
            hit.variableIndex = i;
            hit.variable = report.loadingRows[i].variable;
            return hit;
        }
    }

    if (hitRect(layout.addVariableRect)) {
        DimensionalityReportHit hit;
        hit.kind = DimensionalityReportHitKind::AddVariable;
        return hit;
    }
    return {};
}

DimensionalityReportAction DimensionalityReportPrimaryActionForHit(
    const DimensionalityReportHit &hit)
{
    DimensionalityReportAction action;
    switch (hit.kind) {
    case DimensionalityReportHitKind::Component:
    case DimensionalityReportHitKind::LoadingHeader:
        action.kind = DimensionalityReportActionKind::FocusComponent;
        action.component = hit.component;
        return action;
    case DimensionalityReportHitKind::LoadingCell:
        action.kind = DimensionalityReportActionKind::FocusLoading;
        action.component = hit.component;
        action.variableIndex = hit.variableIndex;
        action.variable = hit.variable;
        return action;
    case DimensionalityReportHitKind::Variable:
        action.kind = DimensionalityReportActionKind::OpenVariableMenu;
        action.variableIndex = hit.variableIndex;
        action.variable = hit.variable;
        action.focusVariable = true;
        return action;
    case DimensionalityReportHitKind::AddVariable:
        action.kind = DimensionalityReportActionKind::OpenAddVariableMenu;
        return action;
    case DimensionalityReportHitKind::None:
    default:
        action.kind = DimensionalityReportActionKind::ClearFocus;
        return action;
    }
}

DimensionalityReportAction DimensionalityReportContextActionForHit(
    const DimensionalityReportHit &hit)
{
    DimensionalityReportAction action;
    if (hit.kind == DimensionalityReportHitKind::Variable) {
        action.kind = DimensionalityReportActionKind::OpenVariableMenu;
        action.variableIndex = hit.variableIndex;
        action.variable = hit.variable;
        action.focusVariable = false;
        return action;
    }
    if (hit.kind == DimensionalityReportHitKind::AddVariable) {
        action.kind = DimensionalityReportActionKind::OpenAddVariableMenu;
        return action;
    }
    action.kind = DimensionalityReportActionKind::OpenAnalysisMenu;
    return action;
}

DimensionalityWindowLayout BuildDimensionalityWindowLayout(
    double reportWidth,
    double reportHeight,
    double visibleWidth,
    double visibleHeight)
{
    DimensionalityWindowLayout layout;
    layout.maxWidth = std::max(720.0, visibleWidth * 0.92);
    layout.maxHeight = std::max(430.0, visibleHeight * 0.86);
    layout.targetWidth = std::min(layout.maxWidth, std::max(760.0, reportWidth + 24.0));
    layout.targetHeight = std::min(layout.maxHeight, std::max(390.0, reportHeight + 142.0));

    const double w = layout.targetWidth;
    const double h = layout.targetHeight;
    layout.titleRect = Rect{16.0, h - 38.0, 360.0, 24.0};
    layout.badgeRect = Rect{w - 170.0, h - 36.0, 150.0, 20.0};
    layout.methodPopupRect = Rect{16.0, h - 72.0, 156.0, 26.0};
    layout.missingPopupRect = Rect{184.0, h - 72.0, 112.0, 26.0};
    layout.componentsPopupRect = Rect{308.0, h - 72.0, 116.0, 26.0};
    layout.rotationPopupRect = Rect{438.0, h - 72.0, 118.0, 26.0};
    layout.scopeLabelRect = Rect{570.0, h - 70.0, 46.0, 22.0};
    layout.scopePopupRect = Rect{616.0, h - 74.0, 138.0, 28.0};
    layout.scaleButtonRect = Rect{16.0, h - 102.0, 120.0, 22.0};
    layout.autoFitButtonRect = Rect{148.0, h - 102.0, 100.0, 22.0};
    layout.selectedRowsRect = Rect{w - 180.0, h - 100.0, 164.0, 20.0};
    layout.scrollViewRect = Rect{
        12.0,
        44.0,
        std::max(200.0, w - 24.0),
        std::max(140.0, h - 156.0)
    };
    layout.statusRect = Rect{16.0, 14.0, std::max(200.0, w - 32.0), 20.0};
    return layout;
}

static bool DimensionalityVariableListContains(const std::vector<std::string> &variables,
                                               const std::string &name)
{
    return std::find(variables.begin(), variables.end(), name) != variables.end();
}

std::vector<std::string> DimensionalityVariablesAvailableToAdd(
    const std::vector<std::string> &currentVariables,
    const std::vector<std::string> &availableVariables)
{
    std::vector<std::string> out;
    for (const std::string &name : availableVariables) {
        if (!DimensionalityVariableListContains(currentVariables, name) &&
            !DimensionalityVariableListContains(out, name)) {
            out.push_back(name);
        }
    }
    return out;
}

std::vector<std::string> DimensionalityVariablesAvailableToReplace(
    const std::vector<std::string> &currentVariables,
    const std::vector<std::string> &availableVariables,
    const std::string &oldName)
{
    std::vector<std::string> out;
    for (const std::string &name : availableVariables) {
        if (name == oldName) continue;
        if (!DimensionalityVariableListContains(currentVariables, name) &&
            !DimensionalityVariableListContains(out, name)) {
            out.push_back(name);
        }
    }
    return out;
}

DimensionalityAddVariableMenuState BuildDimensionalityAddVariableMenuState(
    const std::vector<std::string> &currentVariables,
    const std::vector<std::string> &availableVariables)
{
    DimensionalityAddVariableMenuState state;
    state.variables = DimensionalityVariablesAvailableToAdd(currentVariables, availableVariables);
    state.emptyTitle = DimensionalityNoMoreNumericVariablesTitle();
    return state;
}

DimensionalityVariableMenuState BuildDimensionalityVariableMenuState(
    const std::vector<std::string> &currentVariables,
    const std::vector<std::string> &availableVariables,
    std::size_t index,
    std::size_t minimumVariables)
{
    DimensionalityVariableMenuState state;
    if (index >= currentVariables.size()) {
        return state;
    }
    state.ok = true;
    state.index = index;
    state.variable = currentVariables[index];
    state.replacementVariables = DimensionalityVariablesAvailableToReplace(
        currentVariables, availableVariables, state.variable);
    state.replacementEmptyTitle = DimensionalityNoReplacementVariablesTitle();
    state.removeTitle = DimensionalityRemoveVariableTitle(state.variable);
    state.canRemove = currentVariables.size() > minimumVariables;
    return state;
}

DimensionalityAnalysisMenuState BuildDimensionalityAnalysisMenuState(
    std::size_t availableComponentCount,
    int retainedComponentCount,
    std::size_t scoreCount)
{
    DimensionalityAnalysisMenuState state;
    state.canOpenScreePlot = availableComponentCount > 0;
    state.canOpenBiplot = retainedComponentCount >= 2 && scoreCount > 0;
    state.canSaveScores = scoreCount > 0;
    return state;
}

DimensionalityVariableUpdateResult DimensionalityVariablesAfterAdd(
    const std::vector<std::string> &currentVariables,
    const std::string &name,
    const std::vector<std::string> &availableVariables)
{
    DimensionalityVariableUpdateResult result;
    result.variables = currentVariables;
    if (!DimensionalityVariableListContains(availableVariables, name)) {
        result.error = "numeric variable not found: " + name;
        return result;
    }
    if (DimensionalityVariableListContains(currentVariables, name)) {
        result.error = "variable is already in the analysis: " + name;
        return result;
    }
    result.variables.push_back(name);
    result.ok = true;
    result.changed = true;
    return result;
}

DimensionalityVariableUpdateResult DimensionalityVariablesAfterReplace(
    const std::vector<std::string> &currentVariables,
    std::size_t index,
    const std::string &name,
    const std::vector<std::string> &availableVariables)
{
    DimensionalityVariableUpdateResult result;
    result.variables = currentVariables;
    if (index >= currentVariables.size()) {
        result.error = "variable index is out of range";
        return result;
    }
    if (!DimensionalityVariableListContains(availableVariables, name)) {
        result.error = "numeric variable not found: " + name;
        return result;
    }
    if (DimensionalityVariableListContains(currentVariables, name) &&
        currentVariables[index] != name) {
        result.error = "variable is already in the analysis: " + name;
        return result;
    }
    result.variables[index] = name;
    result.ok = true;
    result.changed = currentVariables[index] != name;
    return result;
}

DimensionalityVariableUpdateResult DimensionalityVariablesAfterRemove(
    const std::vector<std::string> &currentVariables,
    std::size_t index,
    std::size_t minimumVariables)
{
    DimensionalityVariableUpdateResult result;
    result.variables = currentVariables;
    if (index >= currentVariables.size()) {
        result.error = "variable index is out of range";
        return result;
    }
    if (currentVariables.size() <= minimumVariables) {
        result.error = "the analysis must keep at least " +
            std::to_string(minimumVariables) + " variables";
        return result;
    }
    result.variables.erase(result.variables.begin() + static_cast<std::ptrdiff_t>(index));
    result.ok = true;
    result.changed = true;
    return result;
}

bool DimensionalityRotationIsValid(const std::string &rotation)
{
    return rotation == "none" || rotation == "varimax" || rotation == "quartimax" ||
        rotation == "oblimin" || rotation == "promax";
}

bool DimensionalityScopeIsValid(const std::string &scope)
{
    return scope == "all" || scope == "selected" || scope == "unselected";
}

std::string DimensionalityMethodForPopupIndex(int index)
{
    return index == 1 ? "factor" : "pca";
}

int DimensionalityMethodPopupIndex(const std::string &method)
{
    return method == "factor" ? 1 : 0;
}

std::string DimensionalityRotationForPopupIndex(int index)
{
    if (index == 1) return "varimax";
    if (index == 2) return "quartimax";
    return "none";
}

int DimensionalityRotationPopupIndex(const std::string &rotation)
{
    if (rotation == "varimax") return 1;
    if (rotation == "quartimax") return 2;
    if (rotation == "oblimin") return 3;
    if (rotation == "promax") return 4;
    return 0;
}

std::string DimensionalityScopeForPopupIndex(int index)
{
    if (index == 1) return "selected";
    if (index == 2) return "unselected";
    return "all";
}

int DimensionalityScopePopupIndex(const std::string &scope)
{
    if (scope == "selected") return 1;
    if (scope == "unselected") return 2;
    return 0;
}

DimensionalityControlState BuildDimensionalityControlState(
    const std::string &method,
    const std::string &missingMode,
    const std::string &rotation,
    const std::string &scope,
    bool scale,
    std::size_t variableCount,
    int requestedComponentCount)
{
    DimensionalityControlState state;
    const std::string normalizedMethod = method == "factor" ? "factor" : "pca";
    state.methodIndex = DimensionalityMethodPopupIndex(normalizedMethod);
    state.rotationIndex = DimensionalityRotationPopupIndex(rotation);
    state.scopeIndex = DimensionalityScopePopupIndex(scope);
    state.missingMode = missingMode == "pairwise" ? "pairwise" : "listwise";
    state.scale = scale;
    const int factorAdjustment = normalizedMethod == "factor" ? 1 : 0;
    state.maxComponents = std::max(1, static_cast<int>(variableCount) - factorAdjustment);
    state.componentCount = std::max(1, std::min(requestedComponentCount, state.maxComponents));
    state.componentIndex = state.componentCount - 1;
    state.componentOptions.reserve(static_cast<std::size_t>(state.maxComponents));
    for (int i = 1; i <= state.maxComponents; ++i) {
        state.componentOptions.push_back(std::to_string(i) + (i == 1 ? " component" : " components"));
    }
    return state;
}

bool JacobiEigenSymmetric(std::vector<std::vector<double>> a,
                          std::vector<double> &values,
                          std::vector<std::vector<double>> &vectors)
{
    std::size_t n = a.size();
    values.clear();
    vectors.assign(n, std::vector<double>(n, 0.0));
    if (n == 0) return false;
    for (std::size_t i = 0; i < n; ++i) {
        if (a[i].size() != n) return false;
        vectors[i][i] = 1.0;
    }
    const int maxIterations = static_cast<int>(std::max<std::size_t>(80, n * n * 80));
    for (int iter = 0; iter < maxIterations; ++iter) {
        std::size_t p = 0, q = 1;
        double maxOff = 0.0;
        for (std::size_t i = 0; i < n; ++i) {
            for (std::size_t j = i + 1; j < n; ++j) {
                double off = std::fabs(a[i][j]);
                if (off > maxOff) {
                    maxOff = off;
                    p = i;
                    q = j;
                }
            }
        }
        if (maxOff < 1.0e-12) break;
        double app = a[p][p];
        double aqq = a[q][q];
        double apq = a[p][q];
        double tau = (aqq - app) / (2.0 * apq);
        double t = (tau >= 0.0 ? 1.0 : -1.0) / (std::fabs(tau) + std::sqrt(1.0 + tau * tau));
        double c = 1.0 / std::sqrt(1.0 + t * t);
        double s = t * c;
        for (std::size_t k = 0; k < n; ++k) {
            if (k == p || k == q) continue;
            double akp = a[k][p];
            double akq = a[k][q];
            a[k][p] = a[p][k] = c * akp - s * akq;
            a[k][q] = a[q][k] = s * akp + c * akq;
        }
        a[p][p] = c * c * app - 2.0 * s * c * apq + s * s * aqq;
        a[q][q] = s * s * app + 2.0 * s * c * apq + c * c * aqq;
        a[p][q] = a[q][p] = 0.0;
        for (std::size_t k = 0; k < n; ++k) {
            double vip = vectors[k][p];
            double viq = vectors[k][q];
            vectors[k][p] = c * vip - s * viq;
            vectors[k][q] = s * vip + c * viq;
        }
    }
    values.resize(n);
    for (std::size_t i = 0; i < n; ++i) values[i] = a[i][i];
    std::vector<std::size_t> order(n);
    for (std::size_t i = 0; i < n; ++i) order[i] = i;
    std::stable_sort(order.begin(), order.end(), [&](std::size_t left, std::size_t right) {
        return values[left] > values[right];
    });
    std::vector<double> sortedValues(n);
    std::vector<std::vector<double>> sortedVectors(n, std::vector<double>(n, 0.0));
    for (std::size_t newCol = 0; newCol < n; ++newCol) {
        std::size_t oldCol = order[newCol];
        sortedValues[newCol] = values[oldCol];
        for (std::size_t row = 0; row < n; ++row) {
            sortedVectors[row][newCol] = vectors[row][oldCol];
        }
        std::size_t maxIndex = 0;
        double maxAbs = 0.0;
        for (std::size_t row = 0; row < n; ++row) {
            double v = std::fabs(sortedVectors[row][newCol]);
            if (v > maxAbs) {
                maxAbs = v;
                maxIndex = row;
            }
        }
        if (sortedVectors[maxIndex][newCol] < 0.0) {
            for (std::size_t row = 0; row < n; ++row) sortedVectors[row][newCol] *= -1.0;
        }
    }
    values = sortedValues;
    vectors = sortedVectors;
    return true;
}

void ApplyDimensionalityOrthomaxRotation(std::vector<std::vector<double>> &loadings,
                                         std::vector<std::vector<double>> &scores,
                                         const std::string &rotation)
{
    if (rotation == "none" || loadings.empty() || loadings[0].size() < 2) return;
    std::size_t p = loadings.size();
    std::size_t q = loadings[0].size();
    if (p == 0 || q < 2) return;
    double gamma = rotation == "quartimax" ? 0.0 : 1.0;
    const int maxIterations = 60;
    const double tolerance = 1.0e-6;
    for (int iter = 0; iter < maxIterations; ++iter) {
        double maxChange = 0.0;
        for (std::size_t left = 0; left + 1 < q; ++left) {
            for (std::size_t right = left + 1; right < q; ++right) {
                double sumU = 0.0, sumV = 0.0, sumUSqMinusVSq = 0.0, sumUV = 0.0;
                for (std::size_t row = 0; row < p; ++row) {
                    double x = loadings[row][left];
                    double y = loadings[row][right];
                    double u = x * x - y * y;
                    double v = 2.0 * x * y;
                    sumU += u;
                    sumV += v;
                    sumUSqMinusVSq += u * u - v * v;
                    sumUV += u * v;
                }
                double cTerm = sumUSqMinusVSq - gamma / static_cast<double>(p) * (sumU * sumU - sumV * sumV);
                double dTerm = 2.0 * sumUV - 2.0 * gamma / static_cast<double>(p) * sumU * sumV;
                double phi = 0.25 * std::atan2(dTerm, cTerm);
                if (!std::isfinite(phi) || std::fabs(phi) < tolerance) continue;
                double co = std::cos(phi);
                double si = std::sin(phi);
                for (std::size_t row = 0; row < p; ++row) {
                    double x = loadings[row][left];
                    double y = loadings[row][right];
                    loadings[row][left] = co * x + si * y;
                    loadings[row][right] = -si * x + co * y;
                }
                for (std::size_t row = 0; row < scores.size(); ++row) {
                    if (scores[row].size() <= right) continue;
                    double x = scores[row][left];
                    double y = scores[row][right];
                    scores[row][left] = co * x + si * y;
                    scores[row][right] = -si * x + co * y;
                }
                maxChange = std::max(maxChange, std::fabs(phi));
            }
        }
        if (maxChange < tolerance) break;
    }
}

std::vector<double> DimensionalityParallelEigenvalues(std::size_t n,
                                                      std::size_t p,
                                                      int iterations)
{
    const double missing = std::numeric_limits<double>::quiet_NaN();
    std::vector<std::vector<double>> valuesByComponent(p);
    if (n < 2 || p == 0) return std::vector<double>(p, missing);
    iterations = std::max(1, iterations);
    std::mt19937 rng(static_cast<std::uint32_t>(271828 + n * 37 + p * 101));
    std::normal_distribution<double> normal(0.0, 1.0);
    for (int iter = 0; iter < iterations; ++iter) {
        std::vector<std::vector<double>> z(n, std::vector<double>(p, 0.0));
        for (std::size_t row = 0; row < n; ++row) {
            for (std::size_t col = 0; col < p; ++col) z[row][col] = normal(rng);
        }
        for (std::size_t col = 0; col < p; ++col) {
            double mean = 0.0;
            for (std::size_t row = 0; row < n; ++row) mean += z[row][col];
            mean /= static_cast<double>(n);
            double ss = 0.0;
            for (std::size_t row = 0; row < n; ++row) {
                double d = z[row][col] - mean;
                ss += d * d;
            }
            double sd = n > 1 ? std::sqrt(ss / static_cast<double>(n - 1)) : 0.0;
            if (!std::isfinite(sd) || sd <= 1.0e-12) sd = 1.0;
            for (std::size_t row = 0; row < n; ++row) z[row][col] = (z[row][col] - mean) / sd;
        }
        std::vector<std::vector<double>> matrix(p, std::vector<double>(p, 0.0));
        for (std::size_t j = 0; j < p; ++j) {
            for (std::size_t k = j; k < p; ++k) {
                double sum = 0.0;
                for (std::size_t row = 0; row < n; ++row) sum += z[row][j] * z[row][k];
                matrix[j][k] = matrix[k][j] = sum / static_cast<double>(n - 1);
            }
        }
        std::vector<double> eigenvalues;
        std::vector<std::vector<double>> eigenvectors;
        if (!JacobiEigenSymmetric(matrix, eigenvalues, eigenvectors)) continue;
        for (std::size_t k = 0; k < std::min(p, eigenvalues.size()); ++k) {
            valuesByComponent[k].push_back(std::max(0.0, eigenvalues[k]));
        }
    }
    std::vector<double> reference(p, missing);
    for (std::size_t k = 0; k < p; ++k) {
        std::vector<double> values = valuesByComponent[k];
        if (values.empty()) continue;
        std::sort(values.begin(), values.end());
        std::size_t index = static_cast<std::size_t>(std::ceil(0.95 * static_cast<double>(values.size()))) - 1;
        index = std::min(index, values.size() - 1);
        reference[k] = values[index];
    }
    return reference;
}

DimensionalityFitResult FitDimensionality(const DimensionalityFitInput &input)
{
    DimensionalityFitResult result;
    result.method = input.method == "factor" ? "factor" : "pca";
    result.missingMode = input.missingMode == "pairwise" ? "pairwise" : "listwise";
    result.rotation = DimensionalityRotationIsValid(input.rotation) ? input.rotation : "none";
    result.scope = DimensionalityScopeIsValid(input.scope) ? input.scope : "all";
    result.scale = input.scale;
    result.componentCount = input.componentCount;
    result.status = "Not fitted.";

    // Oblique rotations are intentionally an R-only calculation.  The native
    // routine is retained for legacy tests/offline fixtures, but must never
    // approximate oblimin or promax with an orthogonal transform.
    if (result.rotation == "oblimin" || result.rotation == "promax") {
        result.status = "Oblique rotations must be fitted in R.";
        return result;
    }

    if (input.variables.size() < 2) {
        result.status = "At least two numeric variables are required.";
        return result;
    }
    if (input.columns.size() != input.variables.size() || input.columns.empty()) {
        result.status = "One or more variables are not available.";
        return result;
    }
    std::size_t rowCount = input.columns[0].size();
    if (rowCount == 0) {
        result.status = "One or more variables are not available.";
        return result;
    }
    for (const std::vector<double> &column : input.columns) {
        if (column.size() != rowCount) {
            result.status = "One or more variables are not available.";
            return result;
        }
    }

    std::set<int> selectedRows(input.selectedRows.begin(), input.selectedRows.end());
    for (std::size_t i = 0; i < rowCount; ++i) {
        bool complete = true;
        for (const std::vector<double> &column : input.columns) {
            if (!std::isfinite(column[i])) {
                complete = false;
                break;
            }
        }
        int row = static_cast<int>(i) + 1;
        bool selected = selectedRows.find(row) != selectedRows.end();
        if (complete && result.scope == "selected" && !selected) complete = false;
        if (complete && result.scope == "unselected" && selected) complete = false;
        if (complete) {
            result.rowsUsed.push_back(row);
        } else {
            result.rowsExcluded.push_back(row);
        }
    }

    std::size_t n = result.rowsUsed.size();
    std::size_t p = input.columns.size();
    if (n < 2) {
        result.status = "At least two complete rows are required.";
        return result;
    }

    std::vector<double> means(p, 0.0);
    std::vector<double> sds(p, 1.0);
    for (std::size_t j = 0; j < p; ++j) {
        for (int row : result.rowsUsed) means[j] += input.columns[j][static_cast<std::size_t>(row) - 1];
        means[j] /= static_cast<double>(n);
        double ss = 0.0;
        for (int row : result.rowsUsed) {
            double d = input.columns[j][static_cast<std::size_t>(row) - 1] - means[j];
            ss += d * d;
        }
        sds[j] = n > 1 ? std::sqrt(ss / static_cast<double>(n - 1)) : 0.0;
        if (!std::isfinite(sds[j]) || sds[j] <= 1.0e-12) {
            result.status = "Variable `" + input.variables[j] + "` has zero variance.";
            return result;
        }
    }

    std::vector<std::vector<double>> z(n, std::vector<double>(p, 0.0));
    for (std::size_t i = 0; i < n; ++i) {
        std::size_t rowIndex = static_cast<std::size_t>(result.rowsUsed[i]) - 1;
        for (std::size_t j = 0; j < p; ++j) {
            double scale = result.scale ? sds[j] : 1.0;
            z[i][j] = (input.columns[j][rowIndex] - means[j]) / scale;
        }
    }

    std::vector<std::vector<double>> matrix(p, std::vector<double>(p, 0.0));
    for (std::size_t j = 0; j < p; ++j) {
        for (std::size_t k = j; k < p; ++k) {
            double sum = 0.0;
            for (std::size_t i = 0; i < n; ++i) sum += z[i][j] * z[i][k];
            matrix[j][k] = matrix[k][j] = sum / static_cast<double>(n - 1);
        }
    }

    std::vector<double> eigenvalues;
    std::vector<std::vector<double>> eigenvectors;
    if (!JacobiEigenSymmetric(matrix, eigenvalues, eigenvectors)) {
        result.status = "Could not decompose the covariance/correlation matrix.";
        return result;
    }

    int maxComponents = static_cast<int>(p);
    if (result.method == "factor") maxComponents = std::max(1, static_cast<int>(p) - 1);
    result.componentCount = std::max(1, std::min(result.componentCount, maxComponents));

    double total = 0.0;
    for (double value : eigenvalues) {
        if (std::isfinite(value) && value > 0.0) total += value;
    }
    double cumulative = 0.0;
    std::vector<double> parallel = DimensionalityParallelEigenvalues(
        n, p, std::max(1, input.parallelIterations));
    for (std::size_t k = 0; k < eigenvalues.size(); ++k) {
        double value = std::max(0.0, eigenvalues[k]);
        double variance = total > 0.0 ? value / total : std::numeric_limits<double>::quiet_NaN();
        if (std::isfinite(variance)) cumulative += variance;
        DimensionalityFitComponent component;
        component.index = static_cast<int>(k) + 1;
        component.eigenvalue = value;
        component.parallelEigenvalue = k < parallel.size() ? parallel[k] : std::numeric_limits<double>::quiet_NaN();
        component.variance = variance;
        component.cumulative = cumulative;
        result.components.push_back(component);
    }

    std::vector<std::vector<double>> loadingMatrix(p, std::vector<double>(eigenvalues.size(), std::numeric_limits<double>::quiet_NaN()));
    for (std::size_t j = 0; j < p; ++j) {
        for (std::size_t k = 0; k < eigenvalues.size(); ++k) {
            loadingMatrix[j][k] = eigenvectors[j][k] * std::sqrt(std::max(0.0, eigenvalues[k]));
        }
    }

    std::vector<std::vector<double>> scoreMatrix(n, std::vector<double>(eigenvalues.size(), 0.0));
    for (std::size_t i = 0; i < n; ++i) {
        for (std::size_t k = 0; k < eigenvalues.size(); ++k) {
            double score = 0.0;
            for (std::size_t j = 0; j < p; ++j) score += z[i][j] * eigenvectors[j][k];
            scoreMatrix[i][k] = score;
        }
    }

    if (result.rotation != "none" && result.componentCount >= 2) {
        std::vector<std::vector<double>> retainedLoadings(p, std::vector<double>(static_cast<std::size_t>(result.componentCount), std::numeric_limits<double>::quiet_NaN()));
        std::vector<std::vector<double>> retainedScores(n, std::vector<double>(static_cast<std::size_t>(result.componentCount), std::numeric_limits<double>::quiet_NaN()));
        for (std::size_t j = 0; j < p; ++j) {
            for (int k = 0; k < result.componentCount; ++k) retainedLoadings[j][static_cast<std::size_t>(k)] = loadingMatrix[j][static_cast<std::size_t>(k)];
        }
        for (std::size_t i = 0; i < n; ++i) {
            for (int k = 0; k < result.componentCount; ++k) retainedScores[i][static_cast<std::size_t>(k)] = scoreMatrix[i][static_cast<std::size_t>(k)];
        }
        ApplyDimensionalityOrthomaxRotation(retainedLoadings, retainedScores, result.rotation);
        for (std::size_t j = 0; j < p; ++j) {
            for (int k = 0; k < result.componentCount; ++k) loadingMatrix[j][static_cast<std::size_t>(k)] = retainedLoadings[j][static_cast<std::size_t>(k)];
        }
        for (std::size_t i = 0; i < n; ++i) {
            for (int k = 0; k < result.componentCount; ++k) scoreMatrix[i][static_cast<std::size_t>(k)] = retainedScores[i][static_cast<std::size_t>(k)];
        }
    }

    for (std::size_t j = 0; j < p; ++j) {
        DimensionalityFitLoading loading;
        loading.variable = input.variables[j];
        loading.values = loadingMatrix[j];
        double communality = 0.0;
        for (int k = 0; k < result.componentCount && static_cast<std::size_t>(k) < loading.values.size(); ++k) {
            double value = loading.values[static_cast<std::size_t>(k)];
            if (std::isfinite(value)) communality += value * value;
        }
        loading.communality = communality;
        loading.uniqueness = std::max(0.0, (result.scale ? 1.0 : matrix[j][j]) - communality);
        result.loadings.push_back(loading);
    }

    for (std::size_t i = 0; i < n; ++i) {
        DimensionalityFitScore score;
        score.row = result.rowsUsed[i];
        score.values = scoreMatrix[i];
        if (!score.values.empty()) score.x = score.values[0];
        if (score.values.size() >= 2) score.y = score.values[1];
        result.scores.push_back(score);
    }

    if (result.componentCount < 2) {
        for (DimensionalityFitScore &score : result.scores) {
            score.y = 0.0;
            if (score.values.size() < 2) {
                score.values.resize(2, 0.0);
            }
        }
    }

    std::ostringstream status;
    status << (result.method == "factor" ? "Factor analysis" : "Principal components")
           << ": " << input.variables.size() << " variables, " << result.rowsUsed.size()
           << " complete rows";
    if (result.scope == "selected") {
        status << ", selected rows";
    } else if (result.scope == "unselected") {
        status << ", unselected rows";
    }
    if (result.rotation != "none") {
        status << ", " << result.rotation << " rotation";
    }
    if (result.missingMode == "pairwise") {
        status << " (listwise decomposition)";
    }
    result.status = status.str();
    return result;
}

std::string DimensionalityWindowTitle()
{
    return "Principal Components / Factor Analysis";
}

std::string DimensionalityStandardizeButtonTitle()
{
    return "Standardize";
}

std::string DimensionalityNoRotationTitle()
{
    return "No rotation";
}

std::string DimensionalityVarimaxRotationTitle()
{
    return "Varimax";
}

std::string DimensionalityQuartimaxRotationTitle()
{
    return "Quartimax";
}

std::string DimensionalityPCAMethodTitle()
{
    return "Principal components";
}

std::string DimensionalityFactorAnalysisMethodTitle()
{
    return "Factor analysis";
}

} // namespace core
} // namespace rlispstat
